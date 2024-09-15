# Prompt

Instead of the `AcceptConnection` method, can you please alter the code to have the `TPeer` class manage the `Listen` and `Accept` internally and add those accepted connections to the internal thread safe list of connections?

# Answer

Sure! I can adjust the `TPeer` class so that it will internally manage the `Listen` and `Accept` processes, and automatically add any accepted connections to the internal thread-safe list.

In this updated version, `TPeer` will have a `StartListening` method to begin accepting connections on a given port. It will continuously listen for incoming connections in a separate thread, and each accepted connection will be added to the list of connections.

Here’s the updated implementation:

### File 1: `TPeer.pas`
```pascal
unit TPeer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, SyncObjs, TConnection;

type
  TListenThread = class(TThread)
  private
    FOwner: TObject;
    FPort: string;
    FSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TObject; const APort: string);
    destructor Destroy; override;
  end;

  TPeer = class
  private
    FConnections: TThreadList;
    FListenThread: TListenThread;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitiateConnection(const AHost: string; const APort: string);
    procedure StartListening(const APort: string);
    procedure ListConnections;

    procedure AddConnection(ASocket: TTCPBlockSocket; AInitiated: Boolean);
  end;

implementation

{ TListenThread }

constructor TListenThread.Create(AOwner: TObject; const APort: string);
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
    if FSocket.CanRead(1000) then  // Timeout every 1 second
    begin
      ClientSocket := TTCPBlockSocket.Create;
      ClientSocket.Socket := FSocket.Accept;

      if FSocket.LastError = 0 then
      begin
        TPeer(FOwner).AddConnection(ClientSocket, False);
      end
      else
      begin
        ClientSocket.Free;
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
      Connection := TConnection.Create(Socket, True);
      FConnections.Add(Connection);
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

### File 2: `TConnection.pas` (remains unchanged)
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

1. **`TListenThread`**:
   - A dedicated thread that handles the `Listen` and `Accept` loop for incoming connections.
   - It binds to a specified port and listens for incoming connections, accepting them in a non-blocking manner.

2. **`StartListening` method in `TPeer`**:
   - Starts the `TListenThread` to begin accepting connections on the given port.

3. **`AddConnection` method in `TPeer`**:
   - When an incoming connection is accepted by `TListenThread`, the thread calls `TPeer.AddConnection`, which creates a `TConnection` object for the accepted socket and adds it to the thread-safe connection list.

### How It Works:

- To **initiate connections** to other peers, you can call the `InitiateConnection` method as before.
- To **start listening** for incoming connections, you call the `StartListening` method, passing in the port number you want to listen on.
- Incoming connections are automatically accepted in the background thread (`TListenThread`) and added to the connection list.
  
Let me know if you'd like further adjustments or explanations!