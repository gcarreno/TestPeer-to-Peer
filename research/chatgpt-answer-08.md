# Prompt

Can you please add the feature of sending messages?

# Answer

To add a feature for sending messages, we can implement a method in the `TConnection` class that allows the event handler or other parts of the code to send data through a connection.

This requires two key modifications:
1. **Add a `SendMessage` method** to the `TConnection` class to handle outgoing messages.
2. Allow the main thread to use this method to send messages to peers.

### Updated Code

### File 1: `TPeer.pas`

```pascal
unit TPeer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, SyncObjs, TConnection;

type
  // Event triggered when data is available on a connection
  TDataAvailableEvent = procedure(Sender: TObject; AConnection: TConnection) of object;
  TConnectionClosedEvent = procedure(Sender: TObject; AConnection: TConnection) of object;

  TPeer = class(TThread)
  private
    FConnections: TThreadList;
    FPort: string;
    FSocket: TTCPBlockSocket;
    FOnDataAvailable: TDataAvailableEvent;
    FOnConnectionClosed: TConnectionClosedEvent;
    procedure AddConnection(ASocket: TTCPBlockSocket; AInitiated: Boolean);
    procedure RemoveConnection(AConnection: TConnection);
  protected
    procedure Execute; override;
  public
    constructor Create(const APort: string);
    destructor Destroy; override;

    procedure InitiateConnection(const AHost: string; const APort: string);
    procedure ListConnections;

    // Send a message to all connected peers
    procedure BroadcastMessage(const AMessage: string);

    property OnDataAvailable: TDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
    property OnConnectionClosed: TConnectionClosedEvent read FOnConnectionClosed write FOnConnectionClosed;
  end;

implementation

{ TConnectionThread }

type
  TConnectionThread = class(TThread)
  private
    FOwner: TPeer;
    FConnection: TConnection;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TPeer; AConnection: TConnection);
  end;

constructor TConnectionThread.Create(AOwner: TPeer; AConnection: TConnection);
begin
  inherited Create(True);
  FOwner := AOwner;
  FConnection := AConnection;
  FreeOnTerminate := True;
  Start;
end;

procedure TConnectionThread.Execute;
begin
  while not Terminated do
  begin
    if FConnection.Socket.CanRead(1000) then
    begin
      // Notify that data is available but do not read it in this thread
      if Assigned(FOwner.OnDataAvailable) then
      begin
        FOwner.OnDataAvailable(FOwner, FConnection);
      end;
    end;

    // Check if the connection has been closed
    if FConnection.Socket.LastError <> 0 then
    begin
      // Connection closed, notify the owner and terminate the thread
      FOwner.RemoveConnection(FConnection);

      if Assigned(FOwner.OnConnectionClosed) then
      begin
        FOwner.OnConnectionClosed(FOwner, FConnection);
      end;

      Terminate;
    end;
  end;
end;

{ TPeer }

constructor TPeer.Create(const APort: string);
begin
  inherited Create(True);  // Create as a suspended thread
  FPort := APort;
  FConnections := TThreadList.Create;
  FSocket := TTCPBlockSocket.Create;
  FreeOnTerminate := True;
end;

destructor TPeer.Destroy;
begin
  FSocket.Free;
  FConnections.Free;
  inherited Destroy;
end;

procedure TPeer.Execute;
var
  ClientSocket: TTCPBlockSocket;
begin
  FSocket.CreateSocket;
  FSocket.SetLinger(True, 10);
  FSocket.Bind('0.0.0.0', FPort);
  FSocket.Listen;

  while not Terminated do
  begin
    if FSocket.CanRead(1000) then
    begin
      ClientSocket := TTCPBlockSocket.Create;
      ClientSocket.Socket := FSocket.Accept;

      if FSocket.LastError = 0 then
      begin
        AddConnection(ClientSocket, False);
      end
      else
      begin
        ClientSocket.Free;
      end;
    end;
  end;
end;

procedure TPeer.InitiateConnection(const AHost: string; const APort: string);
var
  Socket: TTCPBlockSocket;
begin
  Socket := TTCPBlockSocket.Create;
  try
    Socket.Connect(AHost, APort);
    if Socket.LastError = 0 then
    begin
      AddConnection(Socket, True);
      WriteLn('Connection initiated with ', AHost, ':', APort);
    end
    else
    begin
      WriteLn('Failed to connect to ', AHost, ':', APort);
    end;
  finally
    Socket.Free;
  end;
end;

procedure TPeer.AddConnection(ASocket: TTCPBlockSocket; AInitiated: Boolean);
var
  Connection: TConnection;
begin
  Connection := TConnection.Create(ASocket, AInitiated);
  FConnections.Add(Connection);
  WriteLn('Accepted connection from ', ASocket.GetRemoteSinIP);

  // Start a thread to monitor the connection for data and disconnection
  TConnectionThread.Create(Self, Connection);
end;

procedure TPeer.RemoveConnection(AConnection: TConnection);
var
  List: TList;
  I: Integer;
begin
  List := FConnections.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      if List[I] = AConnection then
      begin
        List.Delete(I);
        Break;
      end;
    end;
  finally
    FConnections.UnlockList;
  end;

  AConnection.Free;
  WriteLn('Connection removed: ', AConnection.Socket.GetRemoteSinIP);
end;

procedure TPeer.ListConnections;
var
  List: TList;
  Connection: TConnection;
  I: Integer;
begin
  List := FConnections.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Connection := TConnection(List[I]);
      WriteLn('Connection: ', Connection.Socket.GetRemoteSinIP, ' (Initiated: ', Connection.Initiated, ')');
    end;
  finally
    FConnections.UnlockList;
  end;
end;

procedure TPeer.BroadcastMessage(const AMessage: string);
var
  List: TList;
  Connection: TConnection;
  I: Integer;
begin
  List := FConnections.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Connection := TConnection(List[I]);
      Connection.SendMessage(AMessage);
    end;
  finally
    FConnections.UnlockList;
  end;
end;

end.
```

### File 2: `TConnection.pas`

```pascal
unit TConnection;

{$mode objfpc}{$H+}

interface

uses
  blcksock, SysUtils;

type
  TConnection = class
  private
    FSocket: TTCPBlockSocket;
    FInitiated: Boolean;
  public
    constructor Create(ASocket: TTCPBlockSocket; AInitiated: Boolean);
    destructor Destroy; override;

    // Method to read data from the connection
    function ReadData: string;

    // Method to send a message to the peer
    procedure SendMessage(const AMessage: string);

    property Socket: TTCPBlockSocket read FSocket;
    property Initiated: Boolean read FInitiated;
  end;

implementation

constructor TConnection.Create(ASocket: TTCPBlockSocket; AInitiated: Boolean);
begin
  FSocket := ASocket;
  FInitiated := AInitiated;
end;

destructor TConnection.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

function TConnection.ReadData: string;
begin
  Result := FSocket.RecvPacket(1024);  // Adjust size as needed
end;

procedure TConnection.SendMessage(const AMessage: string);
begin
  FSocket.SendString(AMessage);
end;

end.
```

### Key Changes:

1. **SendMessage Method in `TConnection`**:
   - The `TConnection` class now has a `SendMessage` method, which sends a string message to the connected peer using `FSocket.SendString`.
   
2. **Broadcast Message in `TPeer`**:
   - The `TPeer` class has a `BroadcastMessage` method that sends a message to all active connections.

### Example Usage:

```pascal
procedure HandleDataAvailable(Sender: TObject; AConnection: TConnection);
var
  Data: string;
begin
  Data := AConnection.ReadData;
  if Data <> '' then
  begin
    WriteLn('Data received from ', AConnection.Socket.GetRemoteSinIP, ': ', Data);
    
    // Optionally, send a response
    AConnection.SendMessage('Message received: ' + Data);
  end;
end;

procedure HandleConnectionClosed(Sender: TObject; AConnection: TConnection);
begin
  WriteLn('Connection closed: ', AConnection.Socket.GetRemoteSinIP);
end;

var
  Peer: TPeer;
begin
  Peer := TPeer.Create('8080');
  try
    Peer.OnDataAvailable := @HandleDataAvailable;
    Peer.OnConnectionClosed := @HandleConnectionClosed;
    Peer.Start;

    // Broadcast a message to all connected peers
    Peer.BroadcastMessage('Hello, peers!');

    // Optionally, initiate a connection to another peer
    Peer.InitiateConnection('127.0.0.1', '808

1');

    while True do;
  finally
    Peer.Free;
  end;
```

### How It Works:

- **Sending Messages**: 
   - You can send a message from any peer to all other peers by calling `Peer.BroadcastMessage('Your message here')`.
   - You can also send a message to a specific peer within the `OnDataAvailable` event, or anytime after a connection has been established, using `AConnection.SendMessage`.
  
- **Receiving Data**: 
   - When data is received, it is processed in the `HandleDataAvailable` event handler, and responses can be sent back using `AConnection.SendMessage`.

With this, the peer-to-peer network can now handle both receiving and sending messages dynamically!