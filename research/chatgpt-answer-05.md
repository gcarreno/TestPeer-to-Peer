# Prompt

Can you please add some changes in order to handle disconnections?

# Answer

Certainly! To handle disconnections in the `TPeer` class, we need to:

1. Detect when a connection is closed either by the remote peer or locally.
2. Remove the disconnected connection from the thread-safe list.
3. Properly clean up any resources associated with the disconnected connection.

### Key Changes:
- Add logic in the `TConnectionThread` to detect disconnection events.
- Modify the `TPeer` class to handle the removal of connections from the list safely.
- Introduce an `OnConnectionClosed` event to notify when a connection is closed.

### Updated Implementation

### File 1: `TPeer.pas`

```pascal
unit TPeer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, SyncObjs, TConnection;

type
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
var
  Data: string;
begin
  while not Terminated do
  begin
    if FConnection.Socket.CanRead(1000) then
    begin
      Data := FConnection.Socket.RecvPacket(1024); // Read available data (adjust size as needed)
      
      if FConnection.Socket.LastError = 0 then
      begin
        // Trigger the event if data is available
        if Assigned(FOwner.OnDataAvailable) then
        begin
          FOwner.OnDataAvailable(FOwner, FConnection);
        end;
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
  Connection: TConnection;
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

end.
```

### File 2: `TConnection.pas` (unchanged)
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

end.
```

### Changes Summary:

1. **`OnConnectionClosed` Event**:
   - This event is added to notify when a connection is closed.
   - Users of the `TPeer` class can assign a handler to this event to take action when a peer disconnects.

2. **Disconnection Handling**:
   - In `TConnectionThread`, the `RecvPacket` method checks for disconnection by examining the `LastError` property of the socket. If an error occurs, it assumes the connection is closed.
   - The connection is removed from the list in the `RemoveConnection` method of `TPeer`, and any resources related to the connection are cleaned up.

3. **Connection Cleanup**:
   - When a connection is closed, it is removed from the thread-safe list, and the `OnConnectionClosed` event is triggered if it has been assigned.

### Example Usage:

```pascal
procedure HandleDataAvailable(Sender: TObject; AConnection: TConnection);
begin
  WriteLn('Data received from ', AConnection.Socket.GetRemoteSinIP);
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
    Peer.Start;  // Start the peer thread to listen for connections
    
    // Optionally, initiate connections to other peers
    Peer.InitiateConnection('127.0.0.1', '8081');

    // Keep the program running to listen for connections and handle data
    while True do;
  finally
    Peer.Free;
  end;
```

### How It Works:

- The `TPeer` thread handles listening for new connections and initiating connections.
- Each connection runs in its own thread (`TConnectionThread`) that monitors for incoming data and disconnection events.
- If data is received, the `OnDataAvailable` event is fired.
- If a connection is closed, the `OnConnectionClosed`