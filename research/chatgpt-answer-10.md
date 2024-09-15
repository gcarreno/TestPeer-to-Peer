# Prompt

You forgot to add an updated usage example. Can you please add it?

# Answer

Here’s an updated usage example that demonstrates how to use the new `TPeer` class with timeouts, message sending, and event handling.

### Updated Usage Example

```pascal
program PeerExample;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, blcksock, synsock, TPeer, TConnection;

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

begin
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

    // Keep the main program running
    WriteLn('Peer running... Press Ctrl+C to stop.');
    while True do;
  finally
    Peer.Free;
  end;
end.
```

### Explanation:

1. **Data Handling**:
   - The `HandleDataAvailable` procedure reads incoming data from a connection when available. It processes the data inside the event handler, ensuring that the connection’s read data can be handled directly by the user.
   - After receiving data, it optionally sends a response using `AConnection.SendMessage`.

2. **Connection Timeout**:
   - Connections will automatically timeout and close if they are inactive for more than 60 seconds. The `HandleConnectionClosed` procedure gets triggered when a connection is closed due to timeout or disconnection by the peer.

3. **Broadcast and Listing**:
   - The example also demonstrates broadcasting a message to all peers and listing the current active connections.

4. **Thread Safety**:
   - The program runs in an infinite loop, ensuring the `TPeer` class continues to manage connections in the background.

You can modify the peer-to-peer system as needed to add more logic or handle specific network events.