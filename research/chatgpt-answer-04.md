# Prompt

Can you please make the class `TPeer` be a thread itself?

# Answer

Certainly! I can refactor the `TPeer` class to be a thread itself. This allows the peer node to operate as a threaded process, managing both listening for incoming connections and initiating connections in a threaded environment. 

In this new design:
- The `TPeer` class will inherit from `TThread` and handle both listening and managing connections within the thread.
- Each connection will still have its own thread for monitoring data, as before.
- The peer will start and run in the background, simplifying the flow.

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

  TPeer = class(TThread)
  private
    FConnections: TThreadList;
    FPort: string;
    FSocket: TTCPBlockSocket;
    FOnDataAvailable: TDataAvailableEvent;
    procedure AddConnection(ASocket: TTCPBlockSocket; AInitiated: Boolean);
  protected
    procedure Execute; override;
  public
    constructor Create(const APort: string);
    destructor Destroy; override;

    procedure InitiateConnection(const AHost: string; const APort: string);
    procedure ListConnections;

    property OnDataAvailable: TDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
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
      Data := FConnection.Socket.RecvPacket(1024);  // Read available data (adjust size as needed)
      if FConnection.Socket.LastError = 0 then
      begin
        // Trigger the event if data is available
        if Assigned(FOwner.OnDataAvailable) then
        begin
          FOwner.OnDataAvailable(FOwner, FConnection);
        end;
      end;
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

  // Start a thread to monitor the connection for data
  TConnectionThread.Create(Self, Connection);
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

### Key Changes:

1. **`TPeer` as a thread**:
   - The `TPeer` class now inherits from `TThread`. The `Execute` method of `TPeer` handles listening for incoming connections.
   - The thread listens on the provided port (`FPort`) and accepts incoming connections, adding them to the connection list.
   
2. **Connection threads**:
   - The `TConnectionThread` is used to monitor each connection for incoming data, just like before. Each connection runs in its own thread.

3. **`InitiateConnection`**:
   - You can still initiate connections manually, and those connections will be monitored by their own threads for incoming data.

### Example Usage:

```pascal
procedure HandleDataAvailable(Sender: TObject; AConnection: TConnection);
begin
  WriteLn('Data received from ', AConnection.Socket.GetRemoteSinIP);
  // You can add custom logic to handle the data here
end;

var
  Peer: TPeer;
begin
  Peer := TPeer.Create('8080');
  try
    Peer.OnDataAvailable := @HandleDataAvailable;
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

- The `TPeer` thread will automatically start listening on port `8080` when the `Start` method is called.
- If any data is received on any connection, the `OnDataAvailable` event is triggered.
- The program continues running indefinitely, handling both initiated and accepted connections in parallel.

This structure now runs everything in a threaded environment while still using the event-driven approach for data handling. Let me know if you need any further modifications!