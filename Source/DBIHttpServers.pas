// _____________________________________________________________________________
{
  Copyright (C) 1996-2014, All rights reserved, John Vander Reest

  This source is free software; you may redistribute, use and/or modify it under
  the terms of the GNU Lesser General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your option)
  any later version.
  You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Version: 2.00
  Author: John Vander Reest

  Change History:
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 28/04/2014 09:12:33 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBIHttpServers;

interface

{$I DBICompilers.inc}

{$define UseIndy10 True}

uses
  Classes, SysUtils, DBIComponents, SyncObjs, IdGlobal, IdBaseComponent, IdComponent,
{$ifndef UseIndy10}
  IdThreadMgr, IdThreadMgrDefault, IdThreadMgrPool,
{$else}
  IdGlobalProtocols, IdContext,
{$endif}
  IdIntercept, {IdSSLIntercept, }
  IdSSLOpenSSL, IdIOHandlerSocket, IdTCPServer, IdHTTPServer, IdCustomHTTPServer;

const
  EmulateSlowdownDelayInSeconds = 0;   // 0 means disable Slowdown, 15 means (15 seconds)

type
  TDBIHttpServerInfo = class(TDBIPersistenceAdapter)
  private
    FActive: Boolean;
    FAuthentication: Boolean;
    FLogging: Boolean;
    FManageSessions: Boolean;
    FPort: Integer;
    FRoot: TFileName;

  protected
    function GetFileName: TFileName; override;
    procedure Load;

    class procedure ReleaseInstance;
    procedure SetRoot(const Value: TFileName);

  public
    class function Instance: TDBIHttpServerInfo;

  published
    property Active: Boolean read FActive write FActive;
    property Authentication: Boolean read FAuthentication write FAuthentication;
    property Logging: Boolean read FLogging write FLogging;
    property ManageSessions: Boolean read FManageSessions write FManageSessions;
    property Port: Integer read FPort write FPort;
    property Root: TFilename read FRoot write SetRoot;


  end;


type
  TDBIHttpMimeTable = class(TIdMIMETable)
  protected
    class procedure ReleaseInstance;

  public
    class function Instance: TDBIHttpMimeTable;

  end;


type
{$ifndef UseIndy10}
  TIdContext = TIdPeerThread;
{$endif}

  TDBIHttpServer = class(TPersistent)
  private
    FHttpServer: TIdHttpServer;
    FHttpServerInfo: TDBIHttpServerInfo;
    FMessages: TStrings;
    FUILock: TCriticalSection;

  protected
    function Authenticate(
      AThread: TIdContext;
      RequestInfo: TIdHTTPRequestInfo;
      ResponseInfo: TIdHTTPResponseInfo
      ): Boolean;

    procedure DisplaySessionChange(const Session: String);

    function GetActive: Boolean;
    function GetHttpServer: TIdHttpServer;
    function GetHttpServerInfo: TDBIHttpServerInfo;
    procedure GetKeyPassword(var APassword: String);
    function GetMessages: TStrings;
    function GetPassword: String; virtual;
    function GetUserName: String; virtual;

    procedure ManageUserSession(
      AThread: TIdContext;
      RequestInfo: TIdHTTPRequestInfo;
      ResponseInfo: TIdHTTPResponseInfo
      );

    procedure MyInfoCallback(Msg: String);

    class procedure ReleaseInstance;

    procedure ServeFile(
      AThread: TIdContext;
      RequestInfo: TIdHTTPRequestInfo;
      ResponseInfo: TIdHTTPResponseInfo
      );

    procedure ServeVirtualFolder(
      AThread: TIdContext;
      RequestInfo: TIdHTTPRequestInfo;
      ResponseInfo: TIdHTTPResponseInfo
      );

    procedure SetActive(const Value: Boolean);
    procedure SetMessages(Value: TStrings);

    property Password: String read GetPassword;
    property UILock: TCriticalSection read FUILock;
    property UserName: String read GetUserName;

  protected
    procedure CommandGet(
      AThread: TIdContext;
      RequestInfo: TIdHTTPRequestInfo;
      ResponseInfo: TIdHTTPResponseInfo
      );

{$ifdef UseIndy10}
    procedure CommandOther(
      AThread: TIdContext;
      RequestInfo: TIdHTTPRequestInfo;
      ResponseInfo: TIdHTTPResponseInfo
      );
{$else}
    procedure CommandOther(
      Thread: TIdContext;
      const asCommand:String;
      const asData: String;
      const asVersion: String
      );
{$endif}
    procedure Connect(AThread: TIdContext);

    procedure Disconnect(AThread: TIdContext);

    procedure Execute(AThread: TIdContext);

    procedure SessionEnd(Sender: TIdHTTPSession);

    procedure SessionStart(Sender: TIdHTTPSession);

    procedure Status(
      ASender: TObject;
      const AStatus: TIdStatus;
      const AStatusText: String
      );

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DisplayMessage(const Msg: string; const Args: array of const);
    
    class function Instance: TDBIHttpServer;

    property Active: Boolean read GetActive write SetActive;
    property HttpServer: TIdHttpServer read GetHttpServer;
    property Messages: TStrings read GetMessages;
    property ServerInfo: TDBIHttpServerInfo read GetHttpServerInfo;
  end;


implementation

uses
{$ifndef Delphi2009}
  Windows, FileCtrl,
{$endif}
  IdStack, IdSocketHandle, DBIUtils;


const
  DefaultTimeFormat = 'c';
  sauthenticationrealm = 'My Http Server';

type
  TDBIHttpServerMessages = class(TStringList)
  public
    function Add(const S: string): Integer; override;
  end;


{ TDBIHttpServerMessages }

function TDBIHttpServerMessages.Add(const S: string): Integer;
begin
  Result := inherited Add(FormatDateTime(DefaultTimeFormat, Now) + ' : ' + S);
end;





{ TDBIHttpServer }

var
  _HttpServer: TDBIHttpServer = nil;

function TDBIHttpServer.Authenticate(
  AThread: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo
  ): Boolean;
begin
  Result := ServerInfo.Authentication and
   ((RequestInfo.AuthUsername <> UserName) or (RequestInfo.AuthPassword <> Password));

  if Result then begin
    ResponseInfo.ContentText :=
      '<html><head><title>Error</title></head><body><h1>Authentication failed</h1>'#13 +
      'Check the demo source code to discover the password:<br><ul><li>Search for <b>AuthUsername</b> in <b>Main.pas</b>!</ul></body></html>';

    ResponseInfo.AuthRealm := sauthenticationrealm;
  end;

  Result := not Result;
end;


procedure TDBIHttpServer.CommandGet(
  AThread: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo
  );

begin
  // Log the request
  DisplayMessage('Command %s %s received from %s:%d', [
    RequestInfo.Command, RequestInfo.Document,
    TIdIOHandlerSocket(AThread.Connection.IOHandler).Binding.PeerIP,
    TIdIOHandlerSocket(AThread.Connection.IOHandler).Binding.PeerPort
    ]);

  if not Authenticate(AThread, RequestInfo, ResponseInfo) then begin
    Exit;
  end;

  if ServerInfo.ManageSessions then begin
    ManageUserSession(AThread, RequestInfo, ResponseInfo);
  end;

  if (Pos('/session', LowerCase(RequestInfo.Document)) = 1) then begin
    ServeVirtualFolder(AThread, RequestInfo, ResponseInfo);
  end
  else begin
    ServeFile(AThread, RequestInfo, ResponseInfo);
  end;
end;


{$ifdef UseIndy10}
procedure TDBIHttpServer.CommandOther(
  AThread: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo
  );
begin
  DisplayMessage('Command other: %s', [RequestInfo.Command]);
end;
{$else}
procedure TDBIHttpServer.CommandOther(
  Thread: TIdContext;
  const asCommand: String;
  const asData: String;
  const asVersion: String
  );
begin
  DisplayMessage('Command other: %s', [asCommand]);
end;
{$endif}


procedure TDBIHttpServer.Connect(AThread: TIdContext);
begin
  DisplayMessage('User logged in', []);
end;


constructor TDBIHttpServer.Create;
begin
  inherited Create;

  FUILock := TCriticalSection.Create;
end;


destructor TDBIHttpServer.Destroy;
begin
  ServerInfo.SaveToFile;
  if ServerInfo.Logging then begin
    Messages.SaveToFile(ChangeFileExt(ParamStr(0), '.log.txt'));
  end;

  FreeAndNil(FUILock);
  FreeAndNil(FHTTPServer);
  FreeAndNil(FHttpServerInfo);
  FreeAndNil(FMessages);

  inherited Destroy;
end;


procedure TDBIHttpServer.Disconnect(AThread: TIdContext);
begin
  DisplayMessage('User logged out', []);
end;


procedure TDBIHttpServer.DisplayMessage(const Msg: string; const Args: array of const);
begin
  if ServerInfo.Logging then begin
    UILock.Acquire;
    try
      Messages.Add(Format(Msg, Args));
    finally
      UILock.Release;
    end;
  end;
end;


procedure TDBIHttpServer.DisplaySessionChange(const Session: String);
var
  Index: Integer;

begin
  if ServerInfo.Logging then begin
    UILock.Acquire;
    try
      Index := Messages.IndexOf(Session);
      if (Index > -1) then begin
        Messages.Delete(Index);
      end
      else begin
        Messages.Append(Session);
      end;
    finally
      UILock.Release;
    end;
  end;
end;


procedure TDBIHttpServer.Execute(AThread: TIdContext);
begin
  DisplayMessage('Execute', []);
end;


function TDBIHttpServer.GetActive: Boolean;
begin
  Result := ServerInfo.Active;
end;


function TDBIHttpServer.GetHttpServer: TIdHttpServer;
begin
  if not Assigned(FHttpServer) then begin
    FHttpServer := TIdHttpServer.Create(nil);

{$ifndef UseIndy10}
    FHttpServer.Greeting.NumericCode := -1;
    FHttpServer.MaxConnectionReply.NumericCode := 0;
    FHttpServer.ReplyExceptionCode := 0;
    FHttpServer.ReplyUnknownCommand.NumericCode := 0;
{$endif}

    FHttpServer.AutoStartSession := True;
    FHttpServer.SessionTimeOut := 1200000;
    FHttpServer.UseCookiesForSessions := False;
    FHttpServer.OnCommandGet := CommandGet;
    FHttpServer.OnCommandOther := CommandOther;
    FHttpServer.OnConnect := Connect;
    FHttpServer.OnDisconnect := Disconnect;

{$ifndef UseIndy10}
    // This does nothing - only available for TIdTcpServer
    FHttpServer.OnExecute := Execute;
{$endif}

    FHttpServer.OnSessionEnd := SessionEnd;
    FHttpServer.OnSessionStart := SessionStart;
    FHttpServer.OnStatus := Status;
  end;
  Result := FHttpServer;
end;


function TDBIHttpServer.GetHttpServerInfo: TDBIHttpServerInfo;
begin
  if not Assigned(FHttpServerInfo) then begin
    FHttpServerInfo := TDBIHttpServerInfo.Create(nil);
    FHttpServerInfo.Load;
  end;
  Result := FHttpServerInfo;
end;


procedure TDBIHttpServer.GetKeyPassword(var APassword: String);
begin
  APassword := GetPassword;
end;


function TDBIHttpServer.GetMessages: TStrings;
begin
  if not Assigned(FMessages) then begin
    FMessages := TDBIHttpServerMessages.Create;
  end;
  Result := FMessages;
end;


function TDBIHttpServer.GetPassword: String;
begin
  Result := 'password';
end;


function TDBIHttpServer.GetUserName: String;
begin
  Result := 'guest';
end;


class function TDBIHttpServer.Instance: TDBIHttpServer;
begin
  if not Assigned(_HttpServer) then begin
    _HttpServer := Self.Create;

    if _HttpServer.ServerInfo.Active then begin
      _HttpServer.Active := True;
    end;
  end;
  Result := _HttpServer;
end;


procedure TDBIHttpServer.ManageUserSession(
  AThread: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo
  );
var
  NumberOfView: Integer;

begin
  // Manage session informations
  if Assigned(RequestInfo.Session) or (HTTPServer.CreateSession(AThread, ResponseInfo, RequestInfo) <> nil) then
  begin
    RequestInfo.Session.Lock;
    try
      NumberOfView := StrToIntDef(RequestInfo.Session.Content.Values['NumViews'], 0);
      inc(NumberOfView);
      RequestInfo.Session.Content.Values['NumViews'] := IntToStr(NumberOfView);
      RequestInfo.Session.Content.Values['UserName'] := RequestInfo.AuthUsername;
      RequestInfo.Session.Content.Values['Password'] := RequestInfo.AuthPassword;
    finally
      RequestInfo.Session.Unlock;
    end;
  end;
end;


procedure TDBIHttpServer.MyInfoCallback(Msg: String);
begin
  DisplayMessage(Msg, []);
end;


class procedure TDBIHttpServer.ReleaseInstance;
begin
  if Assigned(_HttpServer) then begin
    _HttpServer.Active := False;
    FreeAndNil(_HttpServer);
  end;
end;


procedure TDBIHttpServer.ServeFile(
  AThread: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo
  );
const
  C_COMMAND_HEAD = 'HEAD';
  DispositionType: array[Boolean] of String = ('attachment', 'inline');

var
  LocalDoc: TFileName;
  ByteSent: Cardinal;
  ResultFile: TStream;

begin
  // Interprete the command to it's final path (avoid sending files in parent folders)
  LocalDoc := ExpandFilename(ServerInfo.Root + RequestInfo.Document);

  // Default document (index.html) for folder
  if not FileExists(LocalDoc) and DirectoryExists(LocalDoc) and FileExists(ExpandFileName(LocalDoc + '/index.html')) then
  begin
    LocalDoc := ExpandFileName(LocalDoc + '/index.html');
  end;

   // If File exists
  if FileExists(LocalDoc) then begin
    // File down in folder structure
    if AnsiSameText(Copy(LocalDoc, 1, Length(ServerInfo.Root)), ServerInfo.Root) then begin
      ResponseInfo.ContentType := TDBIHttpMIMETable.Instance.GetFileMIMEType(LocalDoc);

      if AnsiSameText(RequestInfo.Command, 'HEAD') then begin
        // HEAD request, don't send the document but still send back it's size
        ResultFile := TFileStream.Create(LocalDoc, fmOpenRead	or fmShareDenyWrite);
        try
          ResponseInfo.ResponseNo := 200;
          ResponseInfo.ContentLength := ResultFile.Size;
        finally
          ResultFile.Free;
        end;
      end

      // Normal document request - Send the document back
      else begin
      
        // Debug slowdown on server
        if (EmulateSlowdownDelayInSeconds > 0) then begin
          Sleep(EmulateSlowdownDelayInSeconds * 1000);
        end;

{$ifdef UseIndy10}
        ResponseInfo.ContentDisposition := Format(
          '%s; filename=%s', [
            DispositionType[CompareText(ResponseInfo.ContentType, 'text/html') = 0],
            RequestInfo.Document
            ]);
        ByteSent := ResponseInfo.ServeFile(AThread, LocalDoc);
{$else}
        ByteSent := HTTPServer.ServeFile(AThread, ResponseInfo, LocalDoc);
{$endif}

        DisplayMessage('Serving file %s (%d bytes / %d bytes sent) to %s:%d', [
          LocalDoc, ByteSent, FileSizeByName(LocalDoc),
          TIdIOHandlerSocket(AThread.Connection.IOHandler).Binding.PeerIP,
          TIdIOHandlerSocket(AThread.Connection.IOHandler).Binding.PeerPort
          ]);
      end;
    end

    // Access Denied (403)
    else begin
      ResponseInfo.ContentText :=
        '<html><head><title>Error</title></head><body><h1>Access denied</h1>'#13 +
        'You do not have sufficient priviligies to access this document.</body></html>';
      ResponseInfo.ResponseNo := 403;
    end;
  end

  // File not found (404)
  else begin
    ResponseInfo.ResponseNo := 404;
    ResponseInfo.ContentText := '<html><head><title>Error</title></head><body><h1>' + ResponseInfo.ResponseText + '</h1></body></html>';
  end;
end;


procedure TDBIHttpServer.ServeVirtualFolder(
  AThread: TIdContext;
  RequestInfo: TIdHTTPRequestInfo;
  ResponseInfo: TIdHTTPResponseInfo
  );
begin
  ResponseInfo.ContentType := 'Text/HTML';
  ResponseInfo.ContentText := '<html><head><title>Virtual folder</title></head><body>';

  if AnsiSameText(RequestInfo.Params.Values['action'], 'close') then begin
    // Closing user session
    RequestInfo.Session.Free;
    ResponseInfo.ContentText :=  ResponseInfo.ContentText + '<h1>Session cleared</h1><p><a href="/sessions">Back</a></p>';
  end
  else begin
    if assigned(RequestInfo.Session) then begin
      if Length(RequestInfo.Params.Values['ParamName']) > 0 then begin
        // Add a new parameter to the session
        ResponseInfo.Session.Content.Values[RequestInfo.Params.Values['ParamName']] := RequestInfo.Params.Values['Param'];
      end;

      ResponseInfo.ContentText := ResponseInfo.ContentText + '<h1>Session informations</h1>';
      RequestInfo.Session.Lock;
      try
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<table border=1>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<tr><td>SessionID</td><td>' + RequestInfo.Session.SessionID + '</td></tr>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<tr><td>Number of page requested during this session</td><td>'+RequestInfo.Session.Content.Values['NumViews']+'</td></tr>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<tr><td>Session data (raw)</td><td><pre>' + RequestInfo.Session.Content.Text + '</pre></td></tr>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '</table>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<h1>Tools:</h1>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<h2>Add new parameter</h2>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<form method="POST">';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<p>Name: <input type="Text" Name="ParamName"></p>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<p>value: <input type="Text" Name="Param"></p>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<p><input type="Submit"><input type="reset"></p>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '</form>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<h2>Other:</h2>';
        ResponseInfo.ContentText := ResponseInfo.ContentText + '<p><a href="' + RequestInfo.Document + '?action=close">Close current session</a></p>';
      finally
        RequestInfo.Session.Unlock;
      end;
    end
    else
    begin
      ResponseInfo.ContentText := ResponseInfo.ContentText + '<p color=#FF000>No session</p>';
    end;
  end;
  ResponseInfo.ContentText := ResponseInfo.ContentText + '</body></html>';
end;


procedure TDBIHttpServer.SessionEnd(Sender: TIdHTTPSession);
var
  DateTime: TDateTime;
  Seconds: Integer;
  Hours, Mins, Secs, Msecs: word;

begin
  DisplayMessage('Ending session %s',[Sender.SessionID]);

  DateTime := (StrToDateTime(sender.Content.Values['StartTime'])-Now);
  DecodeTime(DateTime, Hours, Mins, Secs, Msecs);
  Seconds := ((Trunc(DateTime)*24 + Hours)*60 + Mins)*60 + Secs;

  DisplayMessage('Session duration was: %d seconds', [Seconds]);
  DisplaySessionChange(Sender.SessionID);
end;


procedure TDBIHttpServer.SessionStart(Sender: TIdHTTPSession);
begin
  Sender.Content.Values['StartTime'] := DateTimeToStr(Now);
  DisplayMessage('Starting session %s',[Sender.SessionID]);
  DisplaySessionChange(Sender.SessionID);
end;


procedure TDBIHttpServer.SetActive(const Value: Boolean);
{$ifdef UseIndy10}
const
  MsgActivate = 'Activating Server - Using Indy 10 - Unicode';
{$else}
const
  MsgActivate = 'Activating Server - Using Indy 9 - Ansi';
{$endif}

begin
  if (ServerInfo.Active = Value) then begin
    Exit;
  end;
  ServerInfo.Active := Value;

  // Activate
  if ServerInfo.Active then begin
    Messages.Clear;
    DisplayMessage(MsgActivate, []);
    if (EmulateSlowdownDelayInSeconds > 0) then begin
      DisplayMessage(
        'Emulate Http Server under load, Slowdown is set to %d seconds',
        [EmulateSlowdownDelayInSeconds]
        );
    end;

    if not HTTPServer.Active then begin
      HTTPServer.Bindings.Clear;
      HTTPServer.DefaultPort := ServerInfo.Port;
      HTTPServer.Bindings.Add;
    end;

    // If Folder is Invalid then Abort
    if not DirectoryExists(ServerInfo.Root) then begin
      DisplayMessage('Web root folder (%s) not found.', [ServerInfo.Root]);
      ServerInfo.Active := False;
    end

    // Otherwise proceed as normal
    else begin
      try
        HTTPServer.SessionState := ServerInfo.ManageSessions;
{
        // SSL stuff
        if CheckboxSSL.Checked then begin
          with IdServerInterceptOpenSSL.SSLOptions do begin
            Method := sslvSSLv23;
            AppDir := ExtractFilePath(Application.ExeName);
            RootCertFile := AppDir + 'cert\CAcert.pem';
            CertFile     := AppDir + 'cert\WSScert.pem';
            KeyFile      := AppDir + 'cert\WSSkey.pem';
          end;
          IdServerInterceptOpenSSL.OnStatusInfo := MyInfoCallback;
          IdServerInterceptOpenSSL.OnGetPassword := GetKeyPassword;
          HTTPServer.Intercept := IdServerInterceptOpenSSL;
        end;
        // END SSL stuff
}

        HTTPServer.Active := True;
        DisplayMessage(
          'Listening for HTTP connections on %s:%d.',
          [HTTPServer.Bindings[0].IP, HTTPServer.Bindings[0].Port]
          );

      except
        on E: Exception do begin
          ServerInfo.Active := False;
          DisplayMessage('Exception %s in Activate. Error is:"%s".', [E.ClassName, e.Message]);
        end;
      end;

    end;
  end

  // Deactivate
  else begin
    HTTPServer.Active := False;

    // SSL stuff
    HTTPServer.Intercept := nil;

    DisplayMessage('Stop listening.', []);
  end;
end;


procedure TDBIHttpServer.SetMessages(Value: TStrings);
begin
  if (FMessages is TDBIHttpServerMessages) then begin
    FreeAndNil(FMessages);
  end;
  FMessages := Value;
end;


procedure TDBIHttpServer.Status(
  ASender: TObject;
  const AStatus: TIdStatus;
  const AStatusText: String
  );
begin
  DisplayMessage('Status: %s', [AStatusText]);
end;





{ TDBIHttpMimeTable }

var
  _HttpMimeTable: TDBIHttpMimeTable = nil;

class function TDBIHttpMimeTable.Instance: TDBIHttpMimeTable;
begin
  if not Assigned(_HttpMimeTable) then begin
    _HttpMimeTable := Self.Create;
  end;
  Result := _HttpMimeTable;
end;


class procedure TDBIHttpMimeTable.ReleaseInstance;
begin
  FreeAndNil(_HttpMimeTable);
end;





{ TDBIHttpServerInfo }

var
  _HttpServerInfo: TDBIHttpServerInfo = nil;

function TDBIHttpServerInfo.GetFileName: TFileName;
begin
  Result := ChangeFileExt(ParamStr(0), '.info');
end;


class function TDBIHttpServerInfo.Instance: TDBIHttpServerInfo;
begin
  if not Assigned(_HttpServerInfo) then begin
    _HttpServerInfo := Self.Create(nil);
    _HttpServerInfo.Load;
  end;
  Result := _HttpServerInfo;
end;


procedure TDBIHttpServerInfo.Load;
begin
  if FileExists(GetFileName) then begin
    LoadFromFile;
  end;

  if (Root = '') then begin
    Root := ExtractFilePath(ParamStr(0)) + 'Web';
  end;

  if (Port <= 0) then begin
    Port := 9003;
  end;
end;


class procedure TDBIHttpServerInfo.ReleaseInstance;
begin
  FreeAndNil(_HttpServerInfo);
end;


procedure TDBIHttpServerInfo.SetRoot(const Value: TFileName);
begin
  FRoot := Value;

  ForceDirectories(FRoot);
end;




initialization
finalization
  TDBIHttpServer.ReleaseInstance;
  TDBIHttpMimeTable.ReleaseInstance;
  TDBIHttpServerInfo.ReleaseInstance;

end.
