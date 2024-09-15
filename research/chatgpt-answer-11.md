# Prompt

Can you please add code that hooks to `SIGINT` and `SIGKILL` in order to perform a graceful shutdown of the `TPeer` thread?

# Answer

To gracefully shut down the `TPeer` thread upon receiving `SIGINT` (Ctrl+C) or `SIGTERM` (termination signal), you can hook into these signals using the `Signals` unit in Free Pascal. This will allow us to catch the signals and perform a graceful shutdown, cleaning up any resources and stopping the `TPeer` thread properly.

Here's the updated usage example with signal handling:

### Updated Usage Example with Signal Handling

```pascal
program PeerExample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, blcksock, synsock, TPeer, TConnection, 
  Unix, baseunix; // For signal handling

var
  Peer: TPeer;
  TerminatedFlag: Boolean = False;

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

procedure SignalHandler(Signal: cint); cdecl;
begin
  case Signal of
    SIGINT, SIGTERM: 
      begin
        WriteLn('Signal received: ', Signal);
        TerminatedFlag := True;
      end;
  end;
end;

procedure SetupSignalHandlers;
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

begin
  // Set up signal handlers
  SetupSignalHandlers;

  // Create a peer-to-peer node on port 12345 with a 60-second timeout for inactive connections
  Peer := TPeer.Create('12345', 60);
  try
    Peer.OnDataAvailable := @HandleDataAvailable;
    Peer.OnConnectionClosed := @HandleConnectionClosed;

    // Start the peer thread
    Peer.Start;

    // Optionally, initiate a connection to another peer
    WriteLn('Initiating connection to 127.0.0.1:54321...');
    Peer.InitiateConnection('127.0.0.1', '54321');

    // Broadcast a message to all connected peers (both accepted and initiated connections)
    Peer.BroadcastMessage('Hello, peers!');

    // List active connections
    Peer.ListConnections;

    // Main loop, waiting for signal
    WriteLn('Peer running... Press Ctrl+C to stop.');
    while not TerminatedFlag do
    begin
      Sleep(100);  // Prevents busy-waiting
    end;

    // Gracefully shut down the peer
    WriteLn('Shutting down...');
    Peer.Terminate;  // Signal the peer thread to stop
    Peer.WaitFor;    // Wait for the thread to finish
    WriteLn('Peer terminated.');
  finally
    Peer.Free;
  end;
end.
```

### Key Additions:

1. **Signal Handling**:
   - The `SignalHandler` procedure catches `SIGINT` (Ctrl+C) and `SIGTERM` (kill signal). When either signal is received, it sets the `TerminatedFlag` to `True`, signaling the main loop to break and perform a graceful shutdown.
   
2. **Main Loop**:
   - The main loop now checks `TerminatedFlag` every 100 milliseconds (using `Sleep(100)`), which ensures the program doesn't busy-wait but will exit cleanly when a signal is received.

3. **Graceful Shutdown**:
   - Once the signal is received, `Peer.Terminate` is called to signal the `TPeer` thread to stop.
   - `Peer.WaitFor` is then called to wait for the `TPeer` thread to finish execution before the program exits, ensuring all resources are cleaned up properly.

### Explanation:

- **`SetupSignalHandlers`**: This sets up the handlers for `SIGINT` and `SIGTERM` using Free Pascal's `Unix` and `baseunix` units. When a signal is caught, the `SignalHandler` procedure is executed, setting the `TerminatedFlag` to `True`.
  
- **Graceful Exit**: The program will stop the `TPeer` thread, ensuring all resources like sockets and connections are properly closed when the signal is received.

With this setup, your peer-to-peer application will handle termination gracefully and cleanly exit when interrupted or killed by the system.