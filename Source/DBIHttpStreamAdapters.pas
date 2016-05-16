// _____________________________________________________________________________
{
  Copyright (C) 1996-2015, All rights reserved, John Vander Reest

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
  1.0 | 12/06/2015 06:10:16 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIHttpStreamAdapters;

interface

uses
  Classes, SysUtils, Forms, Dialogs, IdGlobal, IdURI, IdHttp, DBIUtils, DBIStreamAdapters;

{. $define UseZLib}
{. $define UseOpenssl}


type
  TDBICustomHttpStreamAdapter = class(TDBICustomStreamAdapter)
  private
{$ifdef UseZLib}
    FCompressor: TIdCompressorZLib;
{$endif}
{$ifdef UseOpenssl}
    FEncryption: TIdSSLIOHandlerSocketOpenSSL;
{$endif}
    FHttp: TIdHTTP;
    FUri: TIdURI;

  protected
    function CanHandleURI: Boolean;

{$ifdef UseZLib}
    function GetCompressor: TIdCompressorZLib;
{$endif}
{$ifdef UseOpenssl}
    function GetEncryption: TIdSSLIOHandlerSocketOpenSSL;
{$endif}
    function GetHttp: TIdHTTP;
    function GetUri: TIdURI;
    function GetUrl: String;

    procedure SetUrl(const Value: String);

{$ifdef  UseZLib}
    property Compressor: TIdCompressorZLib read GetCompressor;
{$endif}
{$ifdef UseOpenssl}
    property Encryption: TIdSSLIOHandlerSocketOpenSSL read GetEncryption;
{$endif}
    property Http: TIdHTTP read GetHttp;
    property Uri: TIdURI read GetUri;

  public
    destructor Destroy; override;

    function GetDocumentName: String;
    function Load: Boolean;

    property DocumentName: String read GetDocumentName;
    property Url: String read GetUrl write SetUrl;
  end;
  TDBIHttpStreamAdapter = class(TDBICustomHttpStreamAdapter);


implementation



{ TDBICustomHttpStreamAdapter }

function TDBICustomHttpStreamAdapter.CanHandleURI: Boolean;
begin
  Result := UpperCase(Uri.Protocol) = 'HTTP';

{$ifdef UseOpenSSL}
  if not Result then begin
    Result := UpperCase(Uri.Protocol) = 'HTTPS';
  end;
{$endif}
end;


destructor TDBICustomHttpStreamAdapter.Destroy;
begin
{$ifdef UseZLib}
  FreeAndNil(FCompressor);
{$endif}
{$ifdef UseOpenSSL}
  FreeAndNil(FEncryption);
{$endif}
  FreeAndNil(FHttp);
  FreeAndNil(FUri);

  inherited Destroy;
end;


{$ifdef UseZLib}
function TDBICustomHttpStreamAdapter.GetCompressor: TIdCompressorZLib;
begin
  if not Assigned(FCompressor) then begin
    FCompressor := TIdCompressorZLib.Create;
  end;
  Result := FCompressor;
end;
{$endif}


function TDBICustomHttpStreamAdapter.GetDocumentName: String;
begin
  if (Http.Response.Location <> '') then begin
    Uri.Uri := Http.Response.Location;
  end;

  Result := Uri.Document;
  if (Result = '') then begin
    Result := 'index.html';
  end;
end;


{$ifdef UseOpenSSL}
function TDBICustomHttpStreamAdapter.GetEncryption: TIdSSLIOHandlerSocketOpenSSL;
begin
  if not Assigned(FEncryption) then begin
    FEncryption := TIdSSLIOHandlerSocketOpenSSL.Create;
  end;
  Result := FEncryption;
end;
{$endif}


function TDBICustomHttpStreamAdapter.GetHttp: TIdHTTP;
begin
  if not Assigned(FHttp) then begin
    FHttp := TIdHTTP.Create;
{$ifdef UseZLib}
    FHttp.Compressor := Compressor;
{$endif}

    // Set to false if you want this to simply raise an exception on redirects
    FHttp.HandleRedirects := True;

    {
      Note that you probably should set the UserAgent because some servers now screen out requests from
      our default string "Mozilla/3.0 (compatible; Indy Library)" to prevent address harvesters
      and Denial of Service attacks.  Some people have used Indy for these.

      Note that you do need a Mozilla string for the UserAgent property. The format is like this:

      Mozilla/4.0 (compatible; MyProgram)
    }
    FHttp.Request.UserAgent := 'Mozilla/4.0 (compatible; httpget)';

{$ifdef UseOpenSSL}
    FHttp.IOHandler := Encryption;
{$endif}
  end;
  Result := FHttp;
end;


function TDBICustomHttpStreamAdapter.GetUri: TIdURI;
begin
  if not Assigned(FUri) then begin
    FUri := TIdURI.Create;
  end;
  Result := FUri;
end;


function TDBICustomHttpStreamAdapter.GetUrl: String;
begin
  Result := Uri.Uri;
end;


function TDBICustomHttpStreamAdapter.Load: Boolean;
begin
  Result := CanHandleURI;
  if Result then begin
    Http.Get(Uri.Uri, Stream);
  end;
end;


procedure TDBICustomHttpStreamAdapter.SetUrl(const Value: String);
begin
  Uri.Uri := Value;
end;



end.



