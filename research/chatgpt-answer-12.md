# Prompt

Instead of using a flag that is tested in that `while` loop, can you please have `TPeer` block on the main program and have the signal handling callback issue a `Terminate` to the `TPeer` class?

# Answer

Sure! Instead of using a flag and a loop in the main program, you can have the `TPeer` thread block the main program's execution and rely on the signal handler to call `Terminate` on the `TPeer` class. We can achieve this by simply calling `WaitFor` after starting the `TPeer` thread and letting the signal handler handle the termination process.

Here’s how you can modify the example to achieve that:

### Updated Usage Example with Blocking and Signal-Driven Termination

```pascal
program PeerExample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, blcksock, synsock, TPeer, TConnection, 
  Unix, baseunix;  // For signal handling

var
  Peer: TPeer;

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
        WriteLn('Signal received: ', Signal, '. Terminating Peer.');
        if Assigned(Peer) then
          Peer.Terminate;  // Signal the peer thread to stop
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

    // Wait for the thread to finish (blocks the main program)
    WriteLn('Peer running... Press Ctrl+C to stop.');
    Peer.WaitFor;  // This will block until Peer.Terminate is called from the signal handler

    WriteLn('Peer terminated.');
  finally
    Peer.Free;
  end;
end.
```

### Key Changes:

1. **Blocking the Main Program**:
   - The `Peer.WaitFor` call is now used to block the main program. This will block the program until the `Peer` thread terminates.

2. **Signal-Driven Termination**:
   - When a signal is received (`SIGINT` or `SIGTERM`), the `SignalHandler` is invoked, and it calls `Peer.Terminate`, which signals the `TPeer` thread to stop.

3. **No More Loop**:
   - The main program no longer runs in an infinite loop. It simply starts the `Peer` thread and blocks by calling `Peer.WaitFor`. This will wait until the signal handler calls `Peer.Terminate`.

### How It Works:

- **Main Execution**: 
  The main thread starts the `Peer` thread and immediately blocks with `Peer.WaitFor`. This keeps the program from exiting while allowing the `TPeer` thread to run.
  
- **Signal Handling**: 
  When a termination signal is received (`SIGINT` or `SIGTERM`), the signal handler executes and calls `Peer.Terminate`. This signals the `TPeer` thread to stop.

- **Graceful Shutdown**: 
  Once the `Peer.Terminate` is called, the `TPeer` thread finishes its work, and control is returned to the main program, which then exits cleanly.

This way, the program can now cleanly handle termination signals and perform a graceful shutdown without the need for an explicit flag and loop.