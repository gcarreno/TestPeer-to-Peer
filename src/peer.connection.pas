unit Peer.Connection;

{$mode ObjFPC}{$H+}

interface

uses
  Classes
, SysUtils
, blcksock
;

type

{ TConnection }
  TConnection = class
  private
    FSocket: TTCPBlockSocket;
    { #todo 99 -ogcarreno : This must be a set, not a boolean }
    FInitiated: Boolean;
    FLastActivity: TDateTime;
  public
    constructor Create(ASocket: TTCPBlockSocket; AInitiated: Boolean);
    destructor Destroy; override;

    function ReadData: string;

    procedure SendMessage(const AMessage: string);

    procedure UpdateLastActivity;

    property Socket: TTCPBlockSocket read FSocket;
    property Initiated: Boolean read FInitiated;
    property LastActivity: TDateTime read FLastActivity;
  end;

  TDataAvailableEvent = procedure(Sender: TObject; AConnection: TConnection) of object;
  TConnectionClosedEvent = procedure(Sender: TObject; AConnection: TConnection) of object;


implementation

{ TConnection }

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

