unit Peer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, blcksock
, synsock
, Peer.Connection
;

type
{ TPeer }
  TPeer = class(TThread)
  private
    FConnections: TThreadList;
    FListenThread: TThread;
    FAddress: String;
    FPort: string;
    FOnDataAvailable: TDataAvailableEvent;
    FOnConnectionClosed: TConnectionClosedEvent;
    FTimeout: Integer;  // Timeout period in seconds
    procedure AddConnection(ASocket: TTCPBlockSocket; AInitiated: Boolean);
    procedure RemoveConnection(AConnection: TConnection);
  protected
    procedure Execute; override;
  public
    constructor Create(const AAdress, APort: string; ATimeout: Integer = 60);  // Set a default timeout of 60 seconds
    destructor Destroy; override;

    procedure StartListening;
    procedure InitiateConnection(const AHost: string; const APort: string);
    { #todo 99 -ogcarreno : May need a method to disconnect a connection }
    procedure ListConnections;
    procedure BroadcastMessage(const AMessage: string);

    property OnDataAvailable: TDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
    property OnConnectionClosed: TConnectionClosedEvent read FOnConnectionClosed write FOnConnectionClosed;
  end;

implementation

uses
  Peer.Threads
;

{ TPeer }

constructor TPeer.Create(const AAdress, APort: string; ATimeout: Integer);
begin
  inherited Create(True);  // Create as a suspended thread
  FAddress:= AAdress;
  FPort:= APort;
  FTimeout:= ATimeout;
  FConnections:= TThreadList.Create;
  FreeOnTerminate:= False;
end;

destructor TPeer.Destroy;
begin
  if Assigned(FListenThread) then
  begin
    FListenThread.Terminate;
    FListenThread.WaitFor;
  end;
  { #todo 99 -ogcarreno : May need a way to close all open connections }
  FConnections.Free;
  inherited Destroy;
end;

procedure TPeer.StartListening;
begin
  if not Assigned(FListenThread) then
  begin
    FListenThread := TListenThread.Create({Self,} FAddress, FPort);
    FListenThread.Start;
    WriteLn('Listening on port ', FPort);
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
  TConnectionThread.Create({Self, }Connection, FTimeout);
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
      { #todo 1 -ogcarreno : Maybe needs to disconnect the connection? }
      if TConnection(List[I]) = AConnection then
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

procedure TPeer.Execute;
begin
  { #todo 99 -ogcarreno : Probably need to block on listning }
  while not Terminated do
  begin
    WriteLn('Dunno what to do here...');
    Sleep(3000);
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

