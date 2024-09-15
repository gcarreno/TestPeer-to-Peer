# Prompt

Are you sure that a class method can be used as a signal handler call back?
If so, can you please later the code to not error out.
If not, can you suggest an alternative approach?

# Answer

You're right! Class methods cannot be directly used as signal handler callbacks in the manner presented earlier. Signal handlers must be procedural and cannot be methods of a class because the signal handler callback has a strict signature requirement. Specifically, it must be a free-standing procedure (not tied to an object) in most operating systems.

To address this, we need to adjust the signal handler to be a global procedure and use shared global variables or some other mechanism to interact with the class instances.

### Revised Approach

We can define a global signal handler that interacts with the `TPeerApp` instance in a safe manner. Here's the corrected version of the code:

### Revised Code with a Global Signal Handler

```pascal
program PeerExampleApp;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, blcksock, synsock, TPeer, TConnection,
  {$IFDEF UNIX}
  baseunix, unix, 
  {$ENDIF}
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  { Add other necessary units here }
  ;

var
  AppInstance: TObject;  // Global reference to the application instance for signal handling

type
  TPeerApp = class(TCustomApplication)
  private
    Peer: TPeer;
    FHost: string;
    FPort: string;
  protected
    procedure DoRun; override;
    procedure ParseCommandLineArgs;
    procedure SetupSignalHandlers;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure HandleTerminationSignal;
  end;

{$IFDEF UNIX}
procedure UnixSignalHandler(Signal: cint); cdecl;
begin
  if Assigned(AppInstance) and (AppInstance is TPeerApp) then
    TPeerApp(AppInstance).HandleTerminationSignal;
end;

procedure TPeerApp.SetupSignalHandlers;
var
  OldAction: SigActionRec;
  NewAction: SigActionRec;
begin
  FillChar(NewAction, SizeOf(NewAction), 0);
  NewAction.sa_Handler := @UnixSignalHandler;
  fpsigemptyset(NewAction.sa_mask);

  // Handle SIGINT (Ctrl+C), SIGTERM (kill signal)
  if fpSigAction(SIGINT, @NewAction, @OldAction) <> 0 then
    WriteLn('Error setting up SIGINT handler');
  if fpSigAction(SIGTERM, @NewAction, @OldAction) <> 0 then
    WriteLn('Error setting up SIGTERM handler');
end;
{$ENDIF}

{$IFDEF WINDOWS}
function WindowsSignalHandler(Signal: DWORD): BOOL; stdcall;
begin
  if Assigned(AppInstance) and (AppInstance is TPeerApp) then
  begin
    TPeerApp(AppInstance).HandleTerminationSignal;
    Result := True;  // Signal handled
  end
  else
    Result := False;
end;

procedure TPeerApp.SetupSignalHandlers;
begin
  // Windows Ctrl+C, Ctrl+Break, and system shutdown events
  SetConsoleCtrlHandler(@WindowsSignalHandler, True);
end;
{$ENDIF}

constructor TPeerApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  SetupSignalHandlers;
  AppInstance := Self;  // Set the global instance reference for signal handling
end;

destructor TPeerApp.Destroy;
begin
  if Assigned(Peer) then
  begin
    Peer.Terminate;
    Peer.WaitFor;
    FreeAndNil(Peer);
  end;
  inherited Destroy;
end;

procedure TPeerApp.DoRun;
begin
  // Parse command line options
  ParseCommandLineArgs;

  // Create and start the peer-to-peer node
  Peer := TPeer.Create(FPort, 60);  // 60-second timeout for inactive connections
  try
    Peer.OnDataAvailable := @HandleDataAvailable;
    Peer.OnConnectionClosed := @HandleConnectionClosed;

    // Start the peer thread
    Peer.Start;

    // Optionally, initiate a connection to another peer (default connection)
    WriteLn('Initiating connection to ', FHost, ':54321...');
    Peer.InitiateConnection(FHost, '54321');

    // Broadcast a message to all connected peers (both accepted and initiated connections)
    Peer.BroadcastMessage('Hello, peers!');

    // List active connections
    Peer.ListConnections;

    // Block the main program until Peer is terminated by signal
    WriteLn('Peer running on ', FHost, ':', FPort, '... Press Ctrl+C to stop.');
    Peer.WaitFor;  // This will block until Peer.Terminate is called from the signal handler

    WriteLn('Peer terminated.');
  finally
    Terminate;  // Signal application termination
  end;
end;

procedure TPeerApp.HandleTerminationSignal;
begin
  WriteLn('Termination signal received. Terminating Peer.');
  if Assigned(Peer) then
    Peer.Terminate;
end;

procedure HandleDataAvailable(Sender: TObject; AConnection: TConnection);
var
  ReceivedData: string;
begin
  // Read data directly in the event handler
  ReceivedData := AConnection.ReadData;
  if ReceivedData <> '' then
  begin
    WriteLn('Received from ', AConnection.Socket.GetRemoteSinIP, ': ', ReceivedData);

    // Optionally, send a response back
    AConnection.SendMessage('Acknowledged: ' + ReceivedData);
  end;
end;

procedure HandleConnectionClosed(Sender: TObject; AConnection: TConnection);
begin
  WriteLn('Connection closed: ', AConnection.Socket.GetRemoteSinIP);
end;

var
  Application: TPeerApp;

begin
  Application := TPeerApp.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
```

### Explanation of the Changes:

1. **Global `AppInstance` Variable**:
   - We use a global `AppInstance` variable that stores a reference to the `TPeerApp` instance.
   - The global signal handler can now interact with this global reference, safely invoking the `HandleTerminationSignal` method on the instance.

2. **Global Signal Handlers**:
   - The signal handlers (`UnixSignalHandler` for Unix-like systems and `WindowsSignalHandler` for Windows) are now free-standing procedures.
   - These procedures call `HandleTerminationSignal` on the global `AppInstance`.

3. **HandleTerminationSignal**:
   - This is a method in `TPeerApp` that performs the necessary shutdown procedure when a termination signal is received (e.g., calling `Peer.Terminate`).

4. **Compiler Directives**:
   - **Unix**: We use the `SIGINT` and `SIGTERM` signals, and the handler is set up using `fpSigAction`.
   - **Windows**: `SetConsoleCtrlHandler` is used to catch events like `CTRL_C_EVENT` and `CTRL_CLOSE_EVENT`.

### How the Signal Handling Works:

- **For Unix (Linux, macOS)**:
   - The `UnixSignalHandler` is set up using `fpSigAction`. When a signal like `SIGINT` (Ctrl+C) or `SIGTERM` is received, the handler calls `HandleTerminationSignal` on the global `AppInstance`.
  
- **For Windows**:
   - The `WindowsSignalHandler` is set up using `SetConsoleCtrlHandler`. It catches various console control signals such as `CTRL_C_EVENT` (Ctrl+C), `CTRL_CLOSE_EVENT`, and others. When one of these signals is caught, the handler invokes `HandleTerminationSignal`.

This solution allows clean and platform-independent handling of termination signals while ensuring that class-specific methods like `HandleTerminationSignal` can be called safely.