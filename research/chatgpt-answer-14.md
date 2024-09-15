# Prompt

Can you please add code to the `TApplication` signal handling in order for it to be multi platform:
- Make sure it fully hooks into termination signals for unix
- Make sure it fully hooks into termination signals for Windows
- Make sure it fully hooks into termination signals for macOS/OSX
- Use enough compiler directives to make it work

# Answer

Sure! We can hook into the appropriate termination signals for Unix, Windows, and macOS by using compiler directives and platform-specific APIs.

Here's the revised version of the `TPeerApp` with signal handling that works on all three platforms:

### Full Cross-Platform Signal Handling with `TApplication`

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
    {$IFDEF UNIX}
    procedure UnixSignalHandler(Signal: cint); cdecl;
    {$ENDIF}
    {$IFDEF WINDOWS}
    procedure WindowsSignalHandler(Signal: DWORD); stdcall;
    {$ENDIF}
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TPeerApp.ParseCommandLineArgs;
begin
  if HasOption('h', 'host') then
    FHost := GetOptionValue('h', 'host')
  else
    FHost := '127.0.0.1';  // Default host

  if HasOption('p', 'port') then
    FPort := GetOptionValue('p', 'port')
  else
    FPort := '12345';  // Default port
end;

{$IFDEF UNIX}
procedure TPeerApp.UnixSignalHandler(Signal: cint); cdecl;
begin
  case Signal of
    SIGINT, SIGTERM:
      begin
        WriteLn('Unix signal received: ', Signal, '. Terminating Peer.');
        if Assigned(Peer) then
          Peer.Terminate;  // Signal the peer thread to stop
      end;
  end;
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
procedure TPeerApp.WindowsSignalHandler(Signal: DWORD); stdcall;
begin
  case Signal of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT, CTRL_SHUTDOWN_EVENT, CTRL_LOGOFF_EVENT:
      begin
        WriteLn('Windows signal received: ', Signal, '. Terminating Peer.');
        if Assigned(Peer) then
          Peer.Terminate;  // Signal the peer thread to stop
      end;
  end;
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

### Key Points:

1. **Platform-Specific Signal Handling**:
   - **UNIX**: For Unix-based systems (Linux, macOS), we use `fpSigAction` to handle `SIGINT` (Ctrl+C) and `SIGTERM` (termination signal). The `UnixSignalHandler` is invoked when these signals are caught.
   
   - **Windows**: For Windows, we use `SetConsoleCtrlHandler` to catch console control signals like `CTRL_C_EVENT`, `CTRL_CLOSE_EVENT`, and others. The `WindowsSignalHandler` handles these events.

2. **Compiler Directives**:
   - **`{$IFDEF UNIX}`**: This ensures that Unix-specific signal handling code is only included when compiling on a Unix system (Linux, macOS).
   - **`{$IFDEF WINDOWS}`**: This ensures that Windows-specific signal handling code is only included when compiling on Windows.

3. **Windows Signal Events**:
   - `CTRL_C_EVENT`, `CTRL_CLOSE_EVENT`, `CTRL_BREAK_EVENT`: These events are triggered by Ctrl+C, Ctrl+Break, and closing the console window.
   - `CTRL_SHUTDOWN_EVENT`, `CTRL_LOGOFF_EVENT`: These handle system shutdown or user logoff events, allowing graceful shutdown during such events.

4. **Unix Signal Handling**:
   - `SIGINT`: Triggered by Ctrl+C.
   - `SIGTERM`: Triggered by a termination signal (`kill` command).
   
   We use `fpsigemptyset` and `fpsigaction` to handle these signals in a cross-Unix compatible way.

### Explanation:

- **Cross-Platform Signal Handling**: With this setup, you have proper signal handling for both Unix and Windows platforms, ensuring that the application terminates gracefully when receiving system signals.
  
- **Clean Shutdown**: The `Terminate` method is invoked when the signal is received, allowing the `TPeer` thread to stop gracefully.

This solution provides the required platform compatibility for handling termination signals, with the appropriate hooks in place for Windows, Unix/Linux, and macOS.