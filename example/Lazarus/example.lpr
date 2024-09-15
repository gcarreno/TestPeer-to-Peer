program Example;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads, baseunix, unix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}
  Classes, SysUtils, CustApp
, Peer
, Peer.Connection
  { you can add units after this };

type

{ TPeerApplication }
  TPeerApplication = class(TCustomApplication)
  private
    FPeer: TPeer;
    FAddress: string;
    FPort: string;
  protected
    procedure DoRun; override;
    procedure ParseCommandLineArgs;
    procedure SetupSignalHandlers;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    procedure HandleTerminationSignal;
    procedure HandleDataAvailable(Sender: TObject; AConnection: TConnection);
    procedure HandleConnectionClosed(Sender: TObject; AConnection: TConnection);
  end;


var
  Application: TPeerApplication;

{$IFDEF UNIX}
procedure UnixSignalHandler(
  ASignal: longInt;
  AInfo: PSigInfo;
  AContext: PSigContext
); cdecl;
begin
  case ASignal of
    SIGINT, SIGTERM:
      begin
        WriteLn;
        WriteLn('Unix signal received: ', ASignal);
        if Assigned(Application) then
          Application.HandleTerminationSignal;
      end;
  end;
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure WindowsSignalHandler(ASignal: DWORD); stdcall;
begin
  case ASignal of
    CTRL_C_EVENT,
    CTRL_BREAK_EVENT,
    CTRL_CLOSE_EVENT,
    CTRL_SHUTDOWN_EVENT,
    CTRL_LOGOFF_EVENT:
      begin
        WriteLn('Windows signal received: ', ASignal, '. Terminating Peer.');
        if Assigned(Application) then
          Application.HandleTerminationSignal;
      end;
  end;
end;
{$ENDIF}

{ TPeerApplication }

constructor TPeerApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  SetupSignalHandlers;
end;

destructor TPeerApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TPeerApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hapc', ['help', 'address', 'port', 'connect-to']);
  if ErrorMsg<>'' then begin
    //ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  ParseCommandLineArgs;

  FPeer:= TPeer.Create(FAddress, FPort, 60);
  try
    FPeer.OnDataAvailable := @HandleDataAvailable;
    FPeer.OnConnectionClosed := @HandleConnectionClosed;

    // Start the FPeer thread
    FPeer.Start;

    // Optionally, initiate a connection to another FPeer
    //WriteLn('Initiating connection to 127.0.0.1:54321...');
    //FPeer.InitiateConnection('127.0.0.1', '54321');

    // Broadcast a message to all connected peers (both accepted and initiated connections)
    //FPeer.BroadcastMessage('Hello, peers!');

    // List active connections
    //FPeer.ListConnections;

    // Keep the main program running
    WriteLn('Peer running... Press Ctrl+C to stop.');
    FPeer.WaitFor;
  finally
    FPeer.Free;
  end;

  // stop program loop
  //Terminate;
end;

procedure TPeerApplication.ParseCommandLineArgs;
begin
  if HasOption('a', 'address') then
    FAddress := GetOptionValue('a', 'address')
  else
    FAddress := '127.0.0.1';  // Default host

  if HasOption('p', 'port') then
    FPort := GetOptionValue('p', 'port')
  else
    FPort := '12345';  // Default port
end;

{$IFDEF UNIX}
procedure TPeerApplication.SetupSignalHandlers;
var
  OldAction: SigActionRec;
  NewAction: SigActionRec;
begin
  FillChar(NewAction, SizeOf(NewAction), 0);
  NewAction.sa_Handler := @UnixSignalHandler;
  fpsigemptyset(NewAction.sa_mask);

  // Handle SIGINT (Ctrl+C), SIGTERM (kill signal)
  if fpSigAction(SIGINT, @NewAction, @OldAction) <> 0 then
    WriteLn('Error setting up SIGINT handler');
  if fpSigAction(SIGTERM, @NewAction, @OldAction) <> 0 then
    WriteLn('Error setting up SIGTERM handler');
end;
{$ENDIF}

{$IFDEF WINDOWS}
procedure TPeerApplication.SetupSignalHandlers;
begin
  // Windows Ctrl+C, Ctrl+Break, and system shutdown events
  SetConsoleCtrlHandler(@WindowsSignalHandler, True);
end;
{$ENDIF}

procedure TPeerApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

procedure TPeerApplication.HandleTerminationSignal;
begin
  WriteLn('Terminating Peer.');
  if Assigned(FPeer) then
  begin
    FPeer.Terminate;
    FPeer.WaitFor;
  end;
  WriteLn('Terminating Application.');
  Terminate;
end;

procedure TPeerApplication.HandleDataAvailable(Sender: TObject;
  AConnection: TConnection);
var
  ReceivedData: string;
begin
  // Read data directly in the event handler
  ReceivedData := AConnection.ReadData;
  if ReceivedData <> '' then
  begin
    WriteLn('Received from ', AConnection.Socket.GetRemoteSinIP, ': ', ReceivedData);

    // Optionally, send a response back
    AConnection.SendMessage('Acknowledged: ' + ReceivedData);
  end;
end;

procedure TPeerApplication.HandleConnectionClosed(Sender: TObject;
  AConnection: TConnection);
begin
  WriteLn('Connection closed: ', AConnection.Socket.GetRemoteSinIP);
end;

begin
  Application:=TPeerApplication.Create(nil);
  Application.Title:='Peer Application';
  Application.Run;
  Application.Free;
end.

