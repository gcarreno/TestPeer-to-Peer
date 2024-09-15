# Prompt

Can you please add a property to the `TPeer` class that will server as an event for when any connection in the connection list has data to process and possibly use one thread per connection?

# Answer

Certainly! I’ll add an event to the `TPeer` class that triggers when a connection has data to process, and we’ll modify the implementation to use one thread per connection. Each connection will have its own dedicated thread to monitor incoming data. When data is received on a connection, it will fire an event for the `TPeer` class to handle.

### Changes:

- We’ll introduce a new property `OnDataAvailable` to the `TPeer` class.
- Each `TConnection` will be monitored in its own thread, which will listen for incoming data.
- When data is received on a connection, the event will trigger.

### Updated Implementation:

### File 1: `TPeer.pas`
```pascal
unit TPeer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, SyncObjs, TConnection;

type
  TDataAvailableEvent = procedure(Sender: TObject; AConnection: TConnection) of object;

  TPeer = class
  private
    FConnections: TThreadList;
    FListenThread: TThread;
    FOnDataAvailable: TDataAvailableEvent;
    procedure AddConnection(ASocket: TTCPBlockSocket; AInitiated: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitiateConnection(const AHost: string; const APort: string);
    procedure StartListening(const APort: string);
    procedure ListConnections;

    property OnDataAvailable: TDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
  end;

implementation

{ TListenThread }

type
  TListenThread = class(TThread)
  private
    FOwner: TPeer;
    FPort: string;
    FSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TPeer; const APort: string);
    destructor Destroy; override;
  end;

constructor TListenThread.Create(AOwner: TPeer; const APort: string);
begin
  inherited Create(True);
  FOwner := AOwner;
  FPort := APort;
  FSocket := TTCPBlockSocket.Create;
  FreeOnTerminate := True;
end;

destructor TListenThread.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

procedure TListenThread.Execute;
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
        FOwner.AddConnection(ClientSocket, False);
      end
      else
      begin
        ClientSocket.Free;
      end;
    end;
  end;
end;

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
  end;
end;

{ TPeer }

constructor TPeer.Create;
begin
  FConnections := TThreadList.Create;
end;

destructor TPeer.Destroy;
begin
  if Assigned(FListenThread) then
    FListenThread.Terminate;
  FConnections.Free;
  inherited Destroy;
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

procedure TPeer.StartListening(const APort: string);
begin
  if not Assigned(FListenThread) then
  begin
    FListenThread := TListenThread.Create(Self, APort);
    FListenThread.Start;
    WriteLn('Listening on port ', APort);
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

### Explanation of Changes:

1. **`OnDataAvailable` event in `TPeer`:**
   - This property is an event that gets triggered whenever data is received on any connection.
   - It is of type `TDataAvailableEvent`, a procedure that passes the `TPeer` instance and the `TConnection` that has data available.

2. **`TConnectionThread` class:**
   - Each connection gets its own thread, created with the `TConnectionThread` class, which monitors the connection.
   - This thread listens for incoming data using `RecvPacket`. If data is received, it triggers the `OnDataAvailable` event in the `TPeer` class.

3. **Handling Incoming Data:**
   - When data arrives on a connection, the connection’s dedicated thread detects it, and the event handler `OnDataAvailable` is called.

### Usage Example:
```pascal
procedure HandleDataAvailable(Sender: TObject; AConnection: TConnection);
begin
  WriteLn('Data received from ', AConnection.Socket.GetRemoteSinIP);
  // You can add custom logic to handle the data here
end;

var
  Peer: TPeer;
begin
  Peer := TPeer.Create;
  try
    Peer.OnDataAvailable := @HandleDataAvailable;
    Peer.StartListening('8080');
    
    // Optionally, initiate connections to other peers
    // Peer.InitiateConnection('127.0.0.1', '8081');

    // Keep the program running to listen for connections and handle data
    while True do;
  finally
    Peer.Free;
  end;
```

### How It Works:
- The program starts listening on port `8080`.
- When a connection is accepted, a `TConnectionThread` starts monitoring it for incoming data.
- When data is detected on a connection, the `HandleDataAvailable` procedure is called.

Let me know if you need any further improvements!