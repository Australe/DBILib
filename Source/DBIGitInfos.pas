// _____________________________________________________________________________
{
  Copyright (C) 1996-2013, All rights reserved, John Vander Reest

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
  1.0 | 12/06/2013 14:33:12 | JVR | Initial Release
  1.1 | 23/01/2014 11:14:27 | JVR | Status improvements
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBIGitInfos;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Dialogs, DBIProcesses,
  DB, DBIDataset, DBIObjectListDatasets, DBIDataPacketWriters;

type
  EGitCustomError = class(Exception);
  EGitStatusError = class(EGitCustomError);

type
  TDBIGitFileStatus = (gfsStaged, gfsUnstaged, gfsUntracked);
  TDBIPathType = ( ptCmd, ptCmdWOW64, ptGit, ptGrep, ptXE3, ptD2006, ptSource, pt3rdParty );

const
  PathNames: array[TDBIPathType] of String = (
    { ptCmd       }  'C:\Windows\System32\cmd.exe /c',
    { ptCmdWOW64  }  'C:\Windows\SysWOW64\cmd.exe /c',
    { ptGit       }  'C:\Program Files\git\bin\git.exe',
    { ptGrep      }  'C:\Program Files\Git\bin\grep.exe',
    { ptXE3       }  'C:\Program Files\Embarcadero\RAD Studio\11.0\Source\',
    { pt2006      }  'C:\Program Files\Borland\BDS\4.0\Source\',
    { ptSource    }  'O:\Prj\omSource\',
    { pt3rdParty  }  'O:\Prj\3rdParty\'
    );


type
  TDBIGitLog = class(TPersistent)
  private
    FHash: String;
    FAuthorName: String;
    FAuthorEmail: String;
    FAuthorDate: String;
    FCommitterName: String;
    FCommitterEmail: String;
    FCommitterDate: String;
    FSubject: String;

  published
    property Hash: String read FHash write FHash;
    property AuthorName: String read FAuthorName write FAuthorName;
    property AuthorEmail: String read FAuthorEmail write FAuthorEmail;
    property AuthorDate: String read FAuthorDate write FAuthorDate;
    property CommitterName: String read FCommitterName write FCommitterName;
    property CommitterEmail: String read FCommitterEmail write FCommitterEmail;
    property CommitterDate: String read FCommitterDate write FCommitterDate;
    property Subject: String read FSubject write FSubject;

  public
    class procedure BuildFieldDefs(ADataset: TDataset);

  end;


type
  TDBIGitStatus = class(TPersistent)
  private
    FStatus: AnsiString;
    FFileName: AnsiString;
    FSourceName: AnsiString;

  published
    property Status: AnsiString read FStatus write FStatus;
    property FileName: AnsiString read FFileName write FFileName;
    property SourceName: AnsiString read FSourceName write FSourceName;

  public
    class procedure BuildFieldDefs(ADataset: TDataset);
    class procedure UpdateFields(ADataset: TDataset);

  end;


type
  TDBIGitLogDataPacketWriter = class(TDBIXmlDataPacketWriter)
  private
    FData: TStrings;

  protected
    function GetData: TStrings;

    procedure WriteData; override;

  public
    destructor Destroy; override;

    property Data: TStrings read GetData;

  end;


type
  TDBIGitCustomCommandProcessor = class(TDBICustomProcess)
  private
    FFileName: TFileName;
    FStream: TStream;
    FOnEmit: TDBIProcessEmit;

  protected
    procedure Emit(
      Sender: TObject;
      const Message: String;
      const EmitKind: TDBIProcessEmitKind
      ); override;

    class function ExtractRelativeUnixName(
      const FileName: String;
      const SourceName: String
      ): String;

    function GetRepositoryName: String; overload;
    function GetStream: TStream;
    function GetTargetName: String; override;
    procedure SetFileName(const Value: TFileName); virtual;

    property FileName: TFileName read FFileName write SetFileName;
    property Stream: TStream read GetStream;
    property RepositoryName: String read GetRepositoryName;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;

    class function GetRepositoryName(const AFileName: TFileName): String; overload;

    property OnEmit: TDBIProcessEmit read FOnEmit write FOnEmit;
  end;


  TDBIGitCommandProcessor = class(TDBIGitCustomCommandProcessor)
  protected
    function GetCommandLine: String; override;

  public
    property Output;
    property Parameters;
    property SourceName;

  end;


type
  TDBIBeyondCompare = class(TDBICustomProcess)
  private
    FFileName1: TFileName;
    FFileName2: TFileName;

  protected
    function GetParameters: String; override;
    function GetSourceName: String; override;
    function GetTargetName: String; override;

  public
    property FileName1: TFileName read FFileName1 write FFileName1;
    property FileName2: TFileName read FFileName2 write FFileName2;
    property Options;
    property Visible;

  end;


type
  TDBIGitLogInfo = class(TDBIGitCustomCommandProcessor)
  protected
    function GetParameters: String; override;

    procedure ProcessOutputLine(const Value: AnsiString); override;

    procedure SetFileName(const Value: TFileName); override;

  public
    function Execute: Boolean; override;

    property FileName;
    property SourceName;
    property Stream;

  end;


type
  TDBIGitStatusInfo = class(TDBIGitCustomCommandProcessor)
  protected
    function GetParameters: String; override;

    procedure ProcessOutputLine(const Value: AnsiString); override;

  public
    function Execute: Boolean; override;

    property FileName;
    property SourceName;
    property Stream;

  end;


type
  TDBIGitCommitInfo = class(TDBIGitCustomCommandProcessor)
  protected
    function GetParameters: String; override;

  public
    property SourceName;
    property Output;

  end;


type
  TDBIGitFileCommitInfo = class(TDBIGitCustomCommandProcessor)
  protected
    function GetParameters: String; override;

    procedure ProcessOutputLine(const Value: AnsiString); override;

  public
    function Execute: Boolean; override;

    property FileName;
    property SourceName;
    property Stream;

  end;


type
  TDBIGitFileCheckout = class(TDBIGitCustomCommandProcessor)
  protected
    function GetParameters: String; override;

  public
    property FileName;
    property Options;
    property Output;
    property SourceName;

  end;


type
  TDBIGitFileCommit = class(TDBIGitCustomCommandProcessor)
  private
    FComment: String;

  protected
    function GetParameters: String; override;

  public
    property Comment: String read FComment write FComment;
    property SourceName;
    property Output;

  end;


type
  TDBIGitFileCompare = class(TDBIGitCustomCommandProcessor)
  private
    FLeftHash: String;
    FRightHash: String;

  protected
    function GetParameters: String; override;

  public
    constructor Create(AOwner: TComponent); override;

    property FileName;
    property Options;
    property Output;
    property LeftHash: String read FLeftHash write FLeftHash;
    property RightHash: String read FRightHash write FRightHash;
    property SourceName;

  end;


type
  TDBIGitFileSearch = class(TDBIGitCustomCommandProcessor)
  protected
    function GetCommandLine: String; override;
    function GetParameters: String; override;

  public
    function FileFind(const AFileName: String; const APathName: String): Boolean;

    property FileName;
    property Output;
    property SourceName;

  end;


type
  TDBIGitFileStage = class(TDBIGitCustomCommandProcessor)
  private
    FFileStatus: AnsiChar;

  protected
    function GetParameters: String; override;

  public
    property FileName;
    property FileStatus: AnsiChar read FFileStatus write FFileStatus;
    property SourceName;
    property Output;

  end;


type
  TDBIGitFileUnstage = class(TDBIGitCustomCommandProcessor)
  protected
    function GetParameters: String; override;

  public
    property FileName;
    property SourceName;
    property Output;

  end;


type
  TDBIGitUncommit = class(TDBIGitCustomCommandProcessor)
  protected
    function GetParameters: String; override;

  public
    property SourceName;
    property Output;

  end;


implementation

uses
  SysConst, DBIUtils, DBIStrings, DBIDialogs;


function Is64Bit: Boolean;
begin
  Result := SizeOf(Pointer) = 8
end;


function IsLaterThanWindowsXP: Boolean;
var
  osVerInfo: TOSVersionInfo;
begin
  osVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo) ;
  Result :=  Windows.GetVersionEx(osVerInfo) and (osVerInfo.dwMajorVersion > 5);
end;

type
  // Type of IsWow64Process API fn
  TIsWow64Process = function(Handle: THandle; var Res: BOOL): BOOL; stdcall;

// Returns true if the current process is executing as a 32 bit process under
// WOW64 on 64 bit Windows. s gives status message}
function IsWow64: Boolean;
var
  IsWow64Result: BOOL;              // Result from IsWow64Process
  IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference

begin
  Result := False;

  // Try to load required function from kernel32
  IsWow64Process := Windows.GetProcAddress(Windows.GetModuleHandle('kernel32'), 'IsWow64Process');
  if Assigned(IsWow64Process) then begin
    if IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then begin
      Result := IsWow64Result;
    end;
  end;
end;





{ TDBIGitUnCommit }

function TDBIGitUncommit.GetParameters: String;
begin
  Result := 'reset --soft HEAD^';
end;





{ TDBIGitFileUnstage }

function TDBIGitFileUnstage.GetParameters: String;
begin
  Result := 'reset ' + FileName;
end;





{ TDBIGitFileStage }

function TDBIGitFileStage.GetParameters: String;
begin
  if (FileStatus = '?') or (FileStatus = 'M') then begin
    Result := 'add ' + FileName;
  end
  else if (FileStatus = 'D') then begin
    Result := 'rm ' + FileName;
  end;
end;





{ TDBIGitFileSearch }

function TDBIGitFileSearch.FileFind(const AFileName: String; const APathName: String): Boolean;
begin
  FileName := AFileName;
  SourceName := APathName;
  Execute;
  Result := Output.Count > 0;
end;


function TDBIGitFileSearch.GetCommandLine: String;
begin
  if IsWow64 then begin
    Result := PathNames[ptCmdWOW64] + ' ' + '"' + inherited GetCommandLine + '"';
  end
  else begin
    Result := PathNames[ptCmd] + ' ' + '"' + inherited GetCommandLine + '"';
  end;
end;


function TDBIGitFileSearch.GetParameters: String;
begin
  Result := 'ls-files | "' + PathNames[ptGrep] + '" -i ' + FileName;
end;





{ TDBIGitFileCompare }

constructor TDBIGitFileCompare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Options := [piDetachedProcess, piRedirectOutput];
end;

function TDBIGitFileCompare.GetParameters: String;
begin
  Result := 'difftool -y ' + LeftHash + ' ' + RightHash + ' ' + FileName;
end;




{ TDBIGitFileCommit }

function TDBIGitFileCommit.GetParameters: String;
begin
  Result := 'commit -m "' + Comment + '"';
end;





{ TDBIGitFileCheckout }

function TDBIGitFileCheckout.GetParameters: String;
begin
  Result := 'checkout ' + FileName;
end;





{ TDBIGitFileCommitInfo }

function TDBIGitFileCommitInfo.Execute: Boolean;
var
  ODS: TObjectListDataset;
  Writer: TDBIGitLogDataPacketWriter;

begin
  Result := inherited Execute;

  if Result then begin
    ODS := Local(TObjectListDataset.Create(nil)).Obj as TObjectListDataset;
    ODS.ClassTypeName := TDBIGitStatus.ClassName;

    TDBIGitStatus.BuildFieldDefs(ODS);

    Writer := Local(TDBIGitLogDataPacketWriter.Create).Obj as TDBIGitLogDataPacketWriter;
    Writer.Data.Assign(Output);
    Writer.Dataset := ODS;
    Writer.SaveToStream(Stream);
  end
  else begin
    raise EGitStatusError.Create('Fatal Git Error!'#13#13 + Output.Text);
  end;
end;


function TDBIGitFileCommitInfo.GetParameters: String;
begin
  Result := '--no-pager diff --name-only HEAD^ HEAD';
end;


procedure TDBIGitFileCommitInfo.ProcessOutputLine(const Value: AnsiString);
var
  Line: AnsiString;

begin
  Line := '<ROW ' + 'Status="" FileName="' + Value + '"/>';

  inherited ProcessOutputLine(Line);
end;





{ TDBIGitCommitInfo }

function TDBIGitCommitInfo.GetParameters: String;
begin
  Result := 'log -1 --pretty=format:%s';
end;





{ TDBIGitStatusInfo }

function TDBIGitStatusInfo.Execute: Boolean;
var
  ODS: TObjectListDataset;
  Writer: TDBIGitLogDataPacketWriter;

begin
  Result := inherited Execute;

  if Result then begin
    ODS := Local(TObjectListDataset.Create(nil)).Obj as TObjectListDataset;
    ODS.ClassTypeName := TDBIGitStatus.ClassName;

    TDBIGitStatus.BuildFieldDefs(ODS);

    Writer := Local(TDBIGitLogDataPacketWriter.Create).Obj as TDBIGitLogDataPacketWriter;
    Writer.Data.Assign(Output);
    Writer.Dataset := ODS;
    Writer.SaveToStream(Stream);
    Writer.SaveToFile('C:\Temp\Data.xml');
  end;
end;


function TDBIGitStatusInfo.GetParameters: String;
begin
  Result := 'status --porcelain';
end;


procedure TDBIGitStatusInfo.ProcessOutputLine(const Value: AnsiString);
var
  Offset: Integer;
  Status: AnsiString;
  FileData: AnsiString;
  TargetName: AnsiString;
  SourceName: AnsiString;
  Line: AnsiString;

begin
  Line := '';

  if Length(Value) > 2 then begin
    Status := Copy(Value, 1, 2);
    FileData := Copy(Value, 4, 126);

    Offset := TDBIAnsi.Pos('->', FileData);
    if (Offset > 0) then begin
      SourceName := Copy(FileData, 1, Offset-2);
      TargetName := Copy(FileData, Offset+3, 255);
    end
    else begin
      SourceName := '';
      TargetName := FileData;
    end;
    Line := '<ROW ' + 'Status="' + Status + '" FileName="' + TargetName + '" SourceName="' + SourceName + '"/>';

    inherited ProcessOutputLine(Line);
  end;
end;





{ TDBIGitLogInfo }

function TDBIGitLogInfo.Execute: Boolean;
var
  ODS: TObjectListDataset;
  Writer: TDBIGitLogDataPacketWriter;

begin
  Result := inherited Execute;

  if Result then begin
    ODS := Local(TObjectListDataset.Create(nil)).Obj as TObjectListDataset;
    ODS.ClassTypeName := TDBIGitLog.ClassName;

    TDBIGitLog.BuildFieldDefs(ODS);

    Writer := Local(TDBIGitLogDataPacketWriter.Create).Obj as TDBIGitLogDataPacketWriter;
    Writer.Data.Assign(Output);
    Writer.Dataset := ODS;
    Writer.SaveToStream(Stream);
  end;
end;


function TDBIGitLogInfo.GetParameters: String;
begin
  Result :=
//##JVR    'log --all --follow --pretty=format:"' +
    'log --all --pretty=format:"' +
    '<ROW ' +
    'Hash=%x0B%H%x0B ' +

    'AuthorName=%x0B%an%x0B ' +
    'AuthorEmail=%x0B%ae%x0B ' +
    'AuthorDate=%x0B%ai%x0B ' +

    'CommitterName=%x0B%cn%x0B ' +
    'CommitterEmail=%x0B%ce%x0B ' +
    'CommitterDate=%x0B%ci%x0B ' +

    'Subject=%x0B%s%x0B ' +
    '/>' + '" ' +
    FileName;
end;


procedure TDBIGitLogInfo.ProcessOutputLine(const Value: AnsiString);
const
  ChrSingleQuote = #39;
  ChrAmpersand = #38;
  ChrDoubleQuote = #34;
  ChrVerticalTab = #11;

var
  Index: Integer;
  Line: AnsiString;

begin
  Line := Value;
  for Index := 1 to Length(Line) do begin
    case Line[Index] of
      ChrDoubleQuote: Line[Index] := ChrSingleQuote;
      ChrVerticalTab: Line[Index] := ChrDoubleQuote;

      // Special case
      // This is a hack to keep things simple - "&" triggers the XML Entity processing
      // In most cases "&" is a substitute for "and", I therefore replace it with a "+"
      ChrAmpersand: Line[Index] := '+';
    end;
  end;

  inherited ProcessOutputLine(Line);
end;


procedure TDBIGitLogInfo.SetFileName(const Value: TFileName);
begin
  //##DEBUG

  inherited SetFileName(Value);
end;






{ TDBIGitCommandProcessor }

function TDBIGitCommandProcessor.GetCommandLine: String;
begin
  if IsWow64 then begin
    Result := PathNames[ptCmdWOW64] + ' ' + '"' + inherited GetCommandLine + '"';
  end
  else begin
    Result := PathNames[ptCmd] + ' ' + '"' + inherited GetCommandLine + '"';
  end;

  Output.Add('[' + SourceName + ']');
  Output.Add('>> ' + Result);
end;





{ TDBIBeyondCompare }

function TDBIBeyondCompare.GetParameters: String;
begin
  Result := FileName1 + ' ' + FileName2;
end;

function TDBIBeyondCompare.GetSourceName: String;
begin
  Result := 'C:\Temp\';
end;

function TDBIBeyondCompare.GetTargetName: String;
const
  CmdNotePad = 'C:\windows\system32\notepad.exe';
  CmdCompare = '"C:\Program Files\Beyond Compare 3\BComp.exe"';

begin
  Result := CmdCompare;
end;





{ TDBIGitCustomCommandProcessor }

constructor TDBIGitCustomCommandProcessor.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);

  Options := [piRedirectOutput];
  Visible := False;
end;


destructor TDBIGitCustomCommandProcessor.Destroy;
begin
  FreeAndNil(FStream);

  inherited Destroy;
end;


function TDBIGitCustomCommandProcessor.Execute: Boolean;
begin
  Emit(Self, '', ioTrace);
  Emit(Self, '[ ' + SourceName + ' ] ' + GetCommandLine, ioTrace);

  Result := inherited Execute;

  if not Result then begin
    raise EGitStatusError.Create('Fatal Git Error! Command Failed.');
  end;
end;


function TDBIGitCustomCommandProcessor.GetRepositoryName: String;
begin
  Result := GetRepositoryName(FileName);
end;


class function TDBIGitCustomCommandProcessor.GetRepositoryName(const AFileName: TFileName): String;
const
  GitConfig = '\.git\config';

begin
  if (AFileName = '') then begin
    Result := PathNames[ptSource];
    Exit;
  end;

  if not FileExists(AFileName) then begin
    raise EInOutError.CreateFmt(SFileNotFound + ' "%s"', [AFileName]);
  end;

  Result := AFileName;
  while (Result <> '') do begin
    Result := ExtractFileDir(Result);
    if FileExists(Result + GitConfig) then begin
      Result := Result + '\';
      Break;
    end;
  end;

  if (Result = '') then begin
    raise EInOutError.CreateFmt('No Git Repository found for "%s"', [AFileName]);
  end;
end;


function TDBIGitCustomCommandProcessor.GetStream: TStream;
begin
  if not Assigned(FStream) then begin
    FStream := TMemoryStream.Create;
  end;
  Result := FStream;
end;


function TDBIGitCustomCommandProcessor.GetTargetName: String;
begin
  Result := '"' + PathNames[ptGit] + '"';
end;


procedure TDBIGitCustomCommandProcessor.SetFileName(const Value: TFileName);
begin
  // If Windows Path (Has Drive specifier '$:')
  if Pos(':', Value) > 0 then begin
    FFileName := ExtractRelativeUnixName(Value, SourceName);
  end
  else begin
    FFileName := Value;
  end
end;


procedure TDBIGitCustomCommandProcessor.Emit(
  Sender: TObject;
  const Message: String;
  const EmitKind: TDBIProcessEmitKind
  );
begin
  if Assigned(FOnEmit) then FOnEmit(Sender, Message, EmitKind);
end;


class function TDBIGitCustomCommandProcessor.ExtractRelativeUnixName(
  const FileName: String;
  const SourceName: String
  ): String;
begin
  Result := Copy(FileName, Length(SourceName)+1, 128);

  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;





{ TDBIGitLogDataPacketWriter }

destructor TDBIGitLogDataPacketWriter.Destroy;
begin
  FreeAndNil(FData);

  inherited Destroy;
end;


function TDBIGitLogDataPacketWriter.GetData: TStrings;
begin
  if not Assigned(FData) then begin
    FData := TStringList.Create;
  end;
  Result := FData;
end;


procedure TDBIGitLogDataPacketWriter.WriteData;
begin
  WriteStr('<ROWDATA>');
  WriteStr(Data.Text);
  WriteStr('</ROWDATA>');
end;





{ TDBIGitStatus }

class procedure TDBIGitStatus.BuildFieldDefs(ADataset: TDataset);
var
  FieldDef: TFieldDef;

begin
  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'Status';
  FieldDef.DataType := ftString;
  FieldDef.Size := 2;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'FileName';
  FieldDef.DataType := ftString;
  FieldDef.Size := 128;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'SourceName';
  FieldDef.DataType := ftString;
  FieldDef.Size := 128;

end;


class procedure TDBIGitStatus.UpdateFields(ADataset: TDataset);
begin
  ADataset.FieldByName('Status').DisplayWidth := 4;
  ADataset.FieldByName('FileName').DisplayWidth := 80;
  ADataset.FieldByName('SourceName').DisplayWidth := 80;
end;





{ TDBIGitLog }

class procedure TDBIGitLog.BuildFieldDefs(ADataset: TDataset);
var
  FieldDef: TFieldDef;

begin
  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'Hash';
  FieldDef.DataType := ftString;
  FieldDef.Size := 40;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'AuthorName';
  FieldDef.DataType := ftString;
  FieldDef.Size := 40;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'AuthorEmail';
  FieldDef.DataType := ftString;
  FieldDef.Size := 40;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'AuthorDate';
  FieldDef.DataType := ftString;
  FieldDef.Size := 20;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'CommitterName';
  FieldDef.DataType := ftString;
  FieldDef.Size := 40;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'CommitterEmail';
  FieldDef.DataType := ftString;
  FieldDef.Size := 40;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'CommitterDate';
  FieldDef.DataType := ftString;
  FieldDef.Size := 20;

  FieldDef := ADataset.FieldDefs.AddFieldDef;
  FieldDef.Name := 'Subject';
  FieldDef.DataType := ftString;
  FieldDef.Size := 127;

end;



initialization
  Classes.RegisterClass(TDBIGitLog);
  Classes.RegisterClass(TDBIGitStatus);

end.
