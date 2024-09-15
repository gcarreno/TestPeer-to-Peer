unit Peer.Threads;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, DateUtils
, blcksock
, Peer.Connection
;

type
{ TListenThread }
  TListenThread = class(TThread)
  private
    {FOwner: TPeer;}
    FAddress: String;
    FPort: string;
    FSocket: TTCPBlockSocket;
  protected
    procedure Execute; override;
  public
    constructor Create({AOwner: TPeer; }const AAdress, APort: string);
    destructor Destroy; override;
  end;


{ TConnectionThread }
  TConnectionThread = class(TThread)
  private
    {FOwner: TPeer;}
    FConnection: TConnection;
    FTimeout: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(
      {AOwner: TPeer; }
      AConnection: TConnection;
      ATimeout: Integer
    );
  end;


implementation

{ TListenThread }

constructor TListenThread.Create({AOwner: TPeer; }const AAdress, APort: string);
begin
  inherited Create(True); // Create suspended
  {FOwner := AOwner;}
  FAddress:= AAdress;
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
  FSocket.Bind(FAddress, FPort);
  FSocket.Listen;

  while not Terminated do
  begin
    if FSocket.CanRead(1000) then
    begin
      ClientSocket := TTCPBlockSocket.Create;
      ClientSocket.Socket := FSocket.Accept;

      if FSocket.LastError = 0 then
      begin
        //FOwner.AddConnection(ClientSocket, False);
      end
      else
      begin
        ClientSocket.Free;
      end;
    end;
  end;
end;

{ TConnectionThread }

constructor TConnectionThread.Create(
  {AOwner: TPeer; }
  AConnection: TConnection;
  ATimeout: Integer
);
begin
  inherited Create(True);
  {FOwner := AOwner;}
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
      //if Assigned(FOwner.OnDataAvailable) then
      //begin
      //  FOwner.OnDataAvailable(FOwner, FConnection);
      //  FConnection.UpdateLastActivity;  // Update the last activity timestamp
      //end;
    end;

    // Check for timeout
    CurrentTime := Now;
    if SecondsBetween(CurrentTime, FConnection.LastActivity) > FTimeout then
    begin
      // Connection timed out, remove it
      //FOwner.RemoveConnection(FConnection);
      //if Assigned(FOwner.OnConnectionClosed) then
      //begin
      //  FOwner.OnConnectionClosed(FOwner, FConnection);
      //end;
      Terminate;
    end;

    // Check if the connection has been closed by peer
    if FConnection.Socket.LastError <> 0 then
    begin
      //FOwner.RemoveConnection(FConnection);
      //if Assigned(FOwner.OnConnectionClosed) then
      //begin
      //  FOwner.OnConnectionClosed(FOwner, FConnection);
      //end;
      Terminate;
    end;
  end;
end;

end.

