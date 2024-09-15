# Prompt

Can you please add some code to handle timeouts?

# Answer

To handle timeouts for inactive connections in a peer-to-peer network, we can add a timeout check mechanism to both the `TPeer` class and the `TConnection` class. This will help us detect and remove inactive peers after a certain period of inactivity.

We will:
1. **Add a `LastActivity` timestamp** to the `TConnection` class, which will be updated whenever the connection sends or receives data.
2. **Check for timeouts** inside the `TConnectionThread` and close connections that have been inactive for a specified amount of time.

### Modifications

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
    FTimeout: Integer;  // Timeout period in seconds
    procedure AddConnection(ASocket: TTCPBlockSocket; AInitiated: Boolean);
    procedure RemoveConnection(AConnection: TConnection);
  protected
    procedure Execute; override;
  public
    constructor Create(const APort: string; ATimeout: Integer = 60);  // Set a default timeout of 60 seconds
    destructor Destroy; override;

    procedure InitiateConnection(const AHost: string; const APort: string);
    procedure ListConnections;
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
    FTimeout: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TPeer; AConnection: TConnection; ATimeout: Integer);
  end;

constructor TConnectionThread.Create(AOwner: TPeer; AConnection: TConnection; ATimeout: Integer);
begin
  inherited Create(True);
  FOwner := AOwner;
  FConnection := AConnection;
  FTimeout := ATimeout;
  FreeOnTerminate := True;
  Start;
end;

procedure TConnectionThread.Execute;
var
  CurrentTime: TDateTime;
begin
  while not Terminated do
  begin
    if FConnection.Socket.CanRead(1000) then
    begin
      // Notify that data is available
      if Assigned(FOwner.OnDataAvailable) then
      begin
        FOwner.OnDataAvailable(FOwner, FConnection);
        FConnection.UpdateLastActivity;  // Update the last activity timestamp
      end;
    end;

    // Check for timeout
    CurrentTime := Now;
    if SecondsBetween(CurrentTime, FConnection.LastActivity) > FTimeout then
    begin
      // Connection timed out, remove it
      FOwner.RemoveConnection(FConnection);
      if Assigned(FOwner.OnConnectionClosed) then
      begin
        FOwner.OnConnectionClosed(FOwner, FConnection);
      end;
      Terminate;
    end;

    // Check if the connection has been closed by peer
    if FConnection.Socket.LastError <> 0 then
    begin
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

constructor TPeer.Create(const APort: string; ATimeout: Integer = 60);
begin
  inherited Create(True);  // Create as a suspended thread
  FPort := APort;
  FTimeout := ATimeout;
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

  // Start a thread to monitor the connection for data, timeout, and disconnection
  TConnectionThread.Create(Self, Connection, FTimeout);
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
    FLastActivity: TDateTime;  // To track the last activity
  public
    constructor Create(ASocket: TTCPBlockSocket; AInitiated: Boolean);
    destructor Destroy; override;

    // Method to read data from the connection
    function ReadData: string;

    // Method to send a message to the peer
    procedure SendMessage(const AMessage: string);

    // Update the last activity timestamp
    procedure UpdateLastActivity;

    property Socket: TTCPBlockSocket read FSocket;
    property Initiated: Boolean read FInitiated;
    property LastActivity: TDateTime read FLastActivity;
  end;

implementation

constructor TConnection.Create(ASocket: TTCPBlockSocket; AInitiated: Boolean);
begin
  FSocket := ASocket;
  FInitiated := AInitiated;
  FLastActivity := Now;  // Set initial activity time to now
end;

destructor TConnection.Destroy;
begin
  FSocket.Free;
  inherited Destroy;
end;

function TConnection.ReadData: string;
begin
  Result := FSocket.RecvPacket(1024);  // Adjust size as needed
  UpdateLastActivity;  // Update activity time when data is received
end;

procedure TConnection.SendMessage(const AMessage: string);
begin
  FSocket.SendString(AMessage);
  UpdateLastActivity;  // Update activity time when data is sent
end;

procedure TConnection.UpdateLastActivity;
begin
  FLastActivity := Now;
end;

end.
```

### Explanation of Changes:

1. **Timeout Mechanism**:
   - The `TConnection` class now has a `LastActivity`

 field that is updated whenever data is sent or received.
   - The `TConnectionThread` checks for timeouts based on the difference between the current time and the `LastActivity` timestamp. If the difference exceeds the specified timeout (e.g., 60 seconds), the connection is considered inactive and closed.
   
2. **Thread Safety**:
   - `TThreadList` ensures that access to the connection list is thread-safe.

3. **Disconnection Handling**:
   - The connection thread monitors the socket and removes connections when they are closed by the peer or when a timeout occurs.

4. **Timeout Customization**:
   - The `TPeer` class constructor now accepts a `Timeout` parameter that specifies the timeout period (in seconds). This timeout is passed down to each connection thread.

Now, the network will automatically detect and remove inactive connections based on the specified timeout value.