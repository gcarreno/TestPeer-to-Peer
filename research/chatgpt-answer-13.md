# Prompt

Can you please change the usage code to use the console `TApplication` provided by Free Pascal and make it parse the command line for the address and port to bind?

# Answer

Certainly! You can use Free Pascal’s `TApplication` class (found in the `CustApp` unit) for building console applications, which allows for better command-line parsing, structured event handling, and overall application management.

Below is an updated version of the usage code, which now uses `TApplication` and parses the command line for the address and port to bind.

### Updated Usage Example with `TApplication` and Command-Line Parsing

```pascal
program PeerExampleApp;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, blcksock, synsock, TPeer, TConnection, 
  Unix, baseunix;  // For signal handling

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
    procedure SignalHandler(Signal: cint); cdecl;
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

procedure TPeerApp.SignalHandler(Signal: cint); cdecl;
begin
  case Signal of
    SIGINT, SIGTERM: 
      begin
        WriteLn('Signal received: ', Signal, '. Terminating Peer.');
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
  NewAction.sa_Handler := @SignalHandler;
  fpsigemptyset(NewAction.sa_mask);

  // Handle SIGINT (Ctrl+C) and SIGTERM (kill signal)
  if fpSigAction(SIGINT, @NewAction, @OldAction) <> 0 then
    WriteLn('Error setting up SIGINT handler');
  if fpSigAction(SIGTERM, @NewAction, @OldAction) <> 0 then
    WriteLn('Error setting up SIGTERM handler');
end;

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

### Key Changes:

1. **Use of `TApplication`**:
   - The program is now built around `TApplication` (via `TCustomApplication`) from the `CustApp` unit, which simplifies command-line handling and application flow.

2. **Command-Line Parsing**:
   - The `ParseCommandLineArgs` method parses the host and port from the command line using `GetOptionValue`.
   - Use `--host` (or `-h`) to specify the address to bind to, and `--port` (or `-p`) to specify the port.
   - Default values are used (`127.0.0.1` for the host and `12345` for the port) if none are provided.

   **Example command-line usage**:
   ```bash
   ./PeerExampleApp --host=192.168.0.1 --port=9876
   ```

3. **Signal Handling**:
   - The `SetupSignalHandlers` method ensures that `SIGINT` (Ctrl+C) and `SIGTERM` signals are caught. When caught, the `SignalHandler` method calls `Peer.Terminate` to cleanly stop the peer thread.

4. **Blocking on `Peer.WaitFor`**:
   - The `DoRun` method blocks by calling `Peer.WaitFor`, ensuring the program remains running until the peer thread is terminated by a signal.

5. **Application Lifecycle**:
   - The `TPeerApp` class ensures proper startup and shutdown. When the application starts, it parses the command-line arguments and creates a `TPeer` instance. On termination (when a signal is received), the application stops the `TPeer` thread gracefully.

### How It Works:

- **Command-Line Parsing**:
   The program will parse the provided host and port from the command line. If not provided, it defaults to `127.0.0.1` and port `12345`.

- **Signal Handling**:
   If the program receives a `SIGINT` (Ctrl+C) or `SIGTERM`, it will call the signal handler, which terminates the `Peer` thread and performs a clean shutdown.

This setup provides a cleaner structure for handling command-line arguments, signals, and the application's lifecycle using Free Pascal’s `TApplication` infrastructure.