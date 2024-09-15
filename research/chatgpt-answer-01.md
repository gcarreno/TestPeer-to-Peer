# Prompt

Can you please implement the following while targeting mode objfpc of the Free Pascal compiler:
- A class named `TPeer` that implements the rudiments of a node on a peer to peer network
- The `TPeer` class should implement methods to instruct it to initiate a new connection
- The `TPeer` class should implement a thread safe list for the accepted and initiated connects
- Each item on that list should be implemented as a class named `TConnection`, not a record
- Those connection items should contain a field to store if the connection is initiated or accepted in conjunction with the socket information
- Each class in it's own file
- The internet connection library used should be `synapse`

# Answer

Here’s a basic implementation of your request, using Free Pascal's `objfpc` mode and the `synapse` library for networking. We will create two classes: `TPeer` and `TConnection`, each in their own file. The `TPeer` class handles the peer-to-peer logic, while `TConnection` stores connection details.

### File 1: `TPeer.pas`
```pascal
unit TPeer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, SyncObjs, TConnection;

type
  TPeer = class
  private
    FConnections: TThreadList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitiateConnection(const AHost: string; const APort: string);
    procedure AcceptConnection(ASocket: TTCPBlockSocket);
    procedure ListConnections;
  end;

implementation

constructor TPeer.Create;
begin
  FConnections := TThreadList.Create;
end;

destructor TPeer.Destroy;
begin
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

procedure TPeer.AcceptConnection(ASocket: TTCPBlockSocket);
var
  Connection: TConnection;
begin
  Connection := TConnection.Create(ASocket, False);
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

### Explanation:

1. **TPeer class:**
   - The class is responsible for managing peer connections.
   - `FConnections` is a thread-safe list (`TThreadList`) holding all active connections.
   - `InitiateConnection` starts a new connection to a remote host and adds it to the list.
   - `AcceptConnection` adds an incoming connection to the list.
   - `ListConnections` lists all the connections, indicating whether they are initiated or accepted.

2. **TConnection class:**
   - Encapsulates the socket connection with fields to track whether the connection was initiated or accepted.

### Required Library:
You need to have the `synapse` library installed for `blcksock` and `TTCPBlockSocket`. You can get Synapse from [here](https://synapse.ararat.cz/doku.php/start).

Let me know if you need further refinements!