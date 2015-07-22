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
  1.0 | 17/07/1998 10:29:00 | Jvr | Initial Release
  1.1 | 23/02/2005 16:36:46 | Jvr | Refactored and Simplified
  1.2 | 27/05/2013 19:17:33 | Jvr | Unicode support
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIStreamAdapters;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils;

type
  TDBICustomStreamAdapter = class(TPersistent)
  private
    FModified: Boolean;
    FOwnsData: Boolean;
    FReadOnly: Boolean;
    FStream: TStream;

  protected
    procedure CheckCanModify;

    function GetCanModify: Boolean;
    function GetEof: Boolean;
    function GetPosition: Integer;
    function GetStream: TStream; virtual;
    function GetText: String;

    procedure ReleaseStream;
    procedure SetModified(const Value: Boolean); virtual;
    procedure SetText(const Value: String);
    procedure SetReadOnly(const Value: Boolean); virtual;
    procedure SetStream(const Value: TStream);

    property CanModify: Boolean read GetCanModify;
    property Eof: Boolean read GetEof;
    property Modified: Boolean read FModified;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Stream: TStream read GetStream write SetStream;
    property Text: String read GetText write SetText;

  protected
    procedure Append;

  public
    constructor Create(AStream: TStream = nil); virtual;
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromFile(const AFileName: TFileName);
    procedure LoadFromStream(AStream: TStream);

    function ReadChar(var AChar: AnsiChar): Integer; virtual;
    procedure Reset; virtual;
    procedure SaveToFile(const AFileName: TFileName);
    procedure SaveToStream(AStream: TStream);

  end;


type
  TDBIEnabledCallback = procedure(Sender: TObject; var Enabled: Boolean) of object;
  
  TDBICustomStreamFormatter = class(TDBICustomStreamAdapter)
  private
    FEnabled: Boolean;
    FEnabledCallback: TDBIEnabledCallback;

  protected
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(const Value: Boolean);

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property EnabledCallback: TDBIEnabledCallback read FEnabledCallback write FEnabledCallback;
	
  public
    constructor Create(AStream: TStream = nil); override;
    destructor Destroy; override;

    procedure WriteStr(const Str: AnsiString); overload;
{$ifdef Delphi2009}
    procedure WriteStr(const Str: WideString); overload;
{$endif}

    procedure WriteFmt(const Str: AnsiString; const Args: array of const); overload;
{$ifdef Delphi2009}
    procedure WriteFmt(const Str: WideString; const Args: array of const); overload;
{$endif}

    procedure WriteLine(const Str: AnsiString = ''); overload;
{$ifdef Delphi2009}
    procedure WriteLine(const Str: WideString = ''); overload;
{$endif}

  end;


type
  TDBIStreamFormatter = class(TDBICustomStreamFormatter)
  public
    property Enabled;
    property ReadOnly;
    property Stream;
    property Text;

  end;


implementation

const
{$ifndef DELPHI6}
  SLineBreak = {$ifdef LINUX} #10 {$else} #13#10 {$endif};
{$endif}

  SNoFileName = 'No File name specified';
  SStreamUnAssigned = 'Stream is unassigned';
  SStreamReadOnly = 'Cannot modify a read-only stream';

  
{ TDBICustomStreamFormatter }

// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 16:03:46 - Updated code.<br>
}
constructor TDBICustomStreamFormatter.Create(AStream: TStream = nil);
begin
  inherited Create(AStream);

  FEnabled := True;
end;


destructor TDBICustomStreamFormatter.Destroy;
begin
  FEnabledCallback := nil;

  inherited Destroy;
end;


function TDBICustomStreamFormatter.GetEnabled: Boolean;
begin
  Result := FEnabled;

  if Assigned(FEnabledCallback) then begin
    FEnabledCallback(Self, Result);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 16:33:17 - Updated code.<br>
}
procedure TDBICustomStreamFormatter.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 16:06:58 - Updated code.<br>
}
procedure TDBICustomStreamFormatter.WriteFmt(const Str: AnsiString; const Args: array of const);
begin
  WriteStr(AnsiString(Format(String(Str), Args)));
end;

{$ifdef Delphi2009}
procedure TDBICustomStreamFormatter.WriteFmt(const Str: WideString; const Args: array of const);
begin
  WriteStr(AnsiString(Format(String(Str), Args)));
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 16:07:06 - Updated code.<br>
}
procedure TDBICustomStreamFormatter.WriteLine(const Str: AnsiString = '');
begin
  WriteStr(AnsiString(Str) + SLineBreak);
end;

{$ifdef Delphi2009}
procedure TDBICustomStreamFormatter.WriteLine(const Str: WideString = '');
begin
  WriteStr(AnsiString(Str) + SLineBreak);
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 16:04:16 - Updated code.<br>
}
procedure TDBICustomStreamFormatter.WriteStr(const Str: AnsiString);
begin
  if Enabled and (Str <> '') then begin
    Stream.Write(PAnsiChar(AnsiString(Str))^, Length(Str));
    SetModified(True);
  end;
end;

{$ifdef Delphi2009}
procedure TDBICustomStreamFormatter.WriteStr(const Str: WideString);
begin
  WriteStr(AnsiString(Str));
end;
{$endif}




{ TDBICustomStreamAdapter }

// _____________________________________________________________________________
{**
  Jvr - 18/03/2005 12:54:50 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.Append;
begin
  Stream.Seek(0, soFromEnd);
end;


// _____________________________________________________________________________
{**
  Jvr - 18/03/2005 13:24:20 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.CheckCanModify;
begin
  if not CanModify then begin
    raise Exception.Create(SStreamReadOnly);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 20/03/2008 08:04:53 - Updated code.<br>
}
procedure TDBICustomStreamAdapter.Clear;
begin
  Reset;

  if CanModify then begin
    Stream.Size := 0;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 17:58:31 - Initial code.<br>
}
constructor TDBICustomStreamAdapter.Create(AStream: TStream = nil);
begin
  inherited Create;

  SetStream(AStream);
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 16:20:12 - Initial code.<br>
}
destructor TDBICustomStreamAdapter.Destroy;
begin
  ReleaseStream;

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 18/03/2005 17:10:55 - Initial code.<br>
}
function TDBICustomStreamAdapter.GetCanModify: Boolean;
begin
  Result := not ReadOnly;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 16:22:48 - Initial code.<br>
}
function TDBICustomStreamAdapter.GetEof: Boolean;
begin
  Result := Stream.Position = Stream.Size;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/06/2007 15:21:23 - Initial code.<br>
}
function TDBICustomStreamAdapter.GetPosition: Integer;
begin
  Result := Stream.Position;
end;


// _____________________________________________________________________________
{**
  Jvr - 18/03/2005 14:43:39 - Initial code.<br>
}
function TDBICustomStreamAdapter.GetStream: TStream;
begin
  if not Assigned(FStream) then begin
    FStream := TMemoryStream.Create;
  end;
  Result := FStream;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 17:57:36 - Initial code.<br>
}
function TDBICustomStreamAdapter.GetText: String;
var
  StringData: TStringStream;

begin
  StringData := TStringStream.Create('');
  try
    SaveToStream(StringData);

    Result := String(StringData.DataString);
  finally
    StringData.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 18:33:13 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.LoadFromFile(const AFileName: TFileName);
var
  LocalStream: TMemoryStream;

begin
  Assert(AFileName <> '', SNoFileName);

  LocalStream := TMemoryStream.Create;
  try
    LocalStream.LoadFromFile(AFileName);
    LoadFromStream(LocalStream);
  finally
    LocalStream.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 18:35:06 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.LoadFromStream(AStream: TStream);
var
  Offset: Integer;

begin
  Assert(AStream <> nil, SStreamUnAssigned);

  Offset := AStream.Position;
  try
  Stream.Size := Stream.CopyFrom(AStream, 0);
  finally
    AStream.Seek(Offset, soFromBeginning);
  end;

  Stream.Seek(0, soFromBeginning);
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 18:02:53 - Initial code.<br>
}
function TDBICustomStreamAdapter.ReadChar(var AChar: AnsiChar): Integer;
begin
  if (Stream.Read(AChar, 1) > 0) then begin
    Result := Stream.Position;
  end
  else begin
    Result := -1;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 16:31:56 - Updated code.<br>
}
procedure TDBICustomStreamAdapter.ReleaseStream;
begin
  if FOwnsData then begin
    FStream.Free;
    FStream := nil;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 18:25:48 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.Reset;
begin
  Stream.Seek(0, soFromBeginning);
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 18:30:12 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.SaveToFile(const AFileName: TFileName);
var
  LocalStream: TMemoryStream;

begin
  Assert(AFileName <> '', SNoFileName);

  LocalStream := TMemoryStream.Create;
  try
    SaveToStream(LocalStream);
    LocalStream.SaveToFile(AFileName);
  finally
    LocalStream.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 18:38:55 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.SaveToStream(AStream: TStream);
var
  Offset: Integer;

begin
  Assert(AStream <> nil, SStreamUnAssigned);

  Offset := Stream.Position;
  try
    AStream.Size := AStream.CopyFrom(Stream, 0);
  finally
    Stream.Seek(Offset, soFromBeginning);
  end;

  AStream.Seek(0, soFromBeginning);
end;


// _____________________________________________________________________________
{**
  Jvr - 18/03/2005 14:44:41 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;


// _____________________________________________________________________________
{**
  Error method to wrap exception handling.<br>
}
procedure TDBICustomStreamAdapter.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 18/03/2005 12:43:40 - Initial code.<br>
}
procedure TDBICustomStreamAdapter.SetStream(const Value: TStream);
begin
  ReleaseStream;
  FStream := Value;
  FOwnsData := not Assigned(Value);
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2005 16:18:20 - Initial code.<br>
  Jvr - 12/04/2007 15:59:10 - Stream is now cleared, not just reset.<br>
}
procedure TDBICustomStreamAdapter.SetText(const Value: String);
var
  LocalStream: TStream;

begin
  CheckCanModify;
  Clear;

  LocalStream := TStringStream.Create(Value);
  try
    LoadFromStream(LocalStream);
  finally
    LocalStream.Free;
  end;
end;


end.
