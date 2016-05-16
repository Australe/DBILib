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
  1.0 | 12/06/2013 14:33:12 | JVR | Initial Release
  1.1 | 23/01/2014 11:14:27 | JVR | Status improvements
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIGitInfos;

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, Contnrs, DBIComponents, DBIProcesses,
  DBIUtils, DB, DBClient, DBIConst, DBIDataset, DBIObjectListDatasets, DBIDataPacketWriters;

type
  EGitCustomError = class(Exception);
  EGitStatusError = class(EGitCustomError);

type
  TDBIGitFileStatus = (gfsCommitted, gfsStaged, gfsUnstaged, gfsUntracked);

type
  TDBIGitPersistent = class(TPersistent)
  protected
    class procedure BuildFieldDefs(ADataset: TDataset); virtual; abstract;
    class procedure ConfigureFields(ADataset: TDataset); virtual; abstract;
    class procedure FilterDataset(ADataset: TDataset); virtual;

  public
    class procedure LoadDataset(ADataset: TDataset; Strings: TStrings); virtual;

  end;


type
  TDBIGitLog = class(TDBIGitPersistent)
  private
    FHash: String;
    FAuthorName: String;
    FAuthorEmail: String;
    FAuthorDate: String;
    FCommitterName: String;
    FCommitterEmail: String;
    FCommitterDate: String;
    FSubject: String;

  protected
    class procedure BuildFieldDefs(ADataset: TDataset); override;
    class procedure ConfigureFields(ADataset: TDataset); override;

  published
    property Hash: String read FHash write FHash;
    property AuthorName: String read FAuthorName write FAuthorName;
    property AuthorEmail: String read FAuthorEmail write FAuthorEmail;
    property AuthorDate: String read FAuthorDate write FAuthorDate;
    property CommitterName: String read FCommitterName write FCommitterName;
    property CommitterEmail: String read FCommitterEmail write FCommitterEmail;
    property CommitterDate: String read FCommitterDate write FCommitterDate;
    property Subject: String read FSubject write FSubject;

  end;


type
  // NOTES:
  //   The status Column consists of (2) two bytes
  //   Committed = (Status = '  ')
  //   Staged = ('A', 'D', 'M', 'R')   in Status[1] - gfsStatus
  //   Unstaged = ('A', 'D', 'M', 'R') in Status[2] - gfsUnstaged
  //   Untracked = (Status = '??')

  TDBIGitStatus = class(TDBIGitPersistent)
  private
    FStatus: AnsiString;
    FFileName: AnsiString;
    FSourceName: AnsiString;

  protected
    class procedure BuildFieldDefs(ADataset: TDataset); override;
    class procedure ConfigureFields(ADataset: TDataset); override;
    class procedure FilterDataset(ADataset: TDataset); override;
    class function Excluded(Item: TDBIGitStatus): Boolean; virtual;

  published
    property Status: AnsiString read FStatus write FStatus;
    property FileName: AnsiString read FFileName write FFileName;
    property SourceName: AnsiString read FSourceName write FSourceName;

  end;

  TDBIGitCommittedStatus = class(TDBIGitStatus)
  protected
    class function Excluded(Item: TDBIGitStatus): Boolean; override;
  end;

  TDBIGitStagedStatus = class(TDBIGitStatus)
  protected
    class function Excluded(Item: TDBIGitStatus): Boolean; override;
  end;

  TDBIGitUnstagedStatus = class(TDBIGitStatus)
  protected
    class function Excluded(Item: TDBIGitStatus): Boolean; override;
  end;

  TDBIGitUntrackedStatus = class(TDBIGitStatus)
  protected
    class function Excluded(Item: TDBIGitStatus): Boolean; override;
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
    property Stream;
  end;


type
  TDBIVclFiles = class(TDBICachedFiles)
  public
    constructor Create(AOwner: TComponent); override;

  published
    property Path;

  end;


type
  TDBIShellExecute = class(TComponent)
  private
    FFileName: String;
    FParameters: String;
    FTargetName: String;

  public
    constructor Create(AOwner: TComponent); override;

    function Execute: Boolean; overload; virtual;

    class function Execute(const AFileName: String): Boolean; overload; virtual;

  published
    property FileName: String read FFileName write FFileName stored False;
    property Parameters: String read FParameters write FParameters;
    property TargetName: String read FTargetName write FTargetName;

  end;

  TDBIFileOpenFolder = class(TDBIShellExecute);
  TDBIFileOpenWith = class(TDBIShellExecute);
  TDBIFileOpenWithDefault = class(TDBIShellExecute);


type
  TDBICustomCommandProcessor = class(TDBICustomProcess)
  private
    FFileName: TFileName;
    FOnEmit: TDBIProcessEmit;
    FPaths: TStringList;
    FShell: String;

  protected
    procedure Emit(
      Sender: TObject;
      const Message: String;
      const EmitKind: TDBIProcessEmitKind
      ); override;

    function GetCommandLine: String; override;
    function GetFileName: TFileName; virtual;
    function GetPath: String; virtual;
    function GetPaths: TStrings;
    function GetRepositoryName: String; overload;

    procedure SetFileName(const Value: TFileName); virtual;
    procedure SetPath(const Value: String);

    function ProcessExecute: Boolean; override;

    property FileName: TFileName read GetFileName write SetFileName;
    property Paths: TStrings read GetPaths;
    property RepositoryName: String read GetRepositoryName;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean; overload; override;

    class function ExtractRelativeUnixName(
      const FileName: String;
      const SourceName: String
      ): String;

    class function GetRepositoryName(const AFileName: TFileName): String; overload;

    property CommandLine;
    property Output;
    property SourceName;

    property OnEmit: TDBIProcessEmit read FOnEmit write FOnEmit;

  published
    property Path: String read GetPath write SetPath;
    property Shell: String read FShell write FShell;

    property Options;
    property Parameters;
    property TargetName;
    property Tag stored False;
  end;


  TDBIFileOpenCompare = class(TDBICustomCommandProcessor)
  private
    FFileName1: TFileName;
    FFileName2: TFileName;

  protected
    function GetSourceName: String; override;

  public
    class function Execute(const AFileName1, AFileName2: String): Boolean; overload;

  published
    property FileName1: TFileName read FFileName1 write FFileName1 stored False;
    property FileName2: TFileName read FFileName2 write FFileName2 stored False;

  end;


  TDBIFileOpenEdit = class(TDBICustomCommandProcessor)
  public
    class function Execute(const AFileName: String): Boolean; overload;

  published
    property FileName stored False;

  end;


  TDBIGitCustomCommand = class(TDBICustomCommandProcessor)
  protected
    function GetSourceName: String; override;

    procedure SetOptions(const Value: TDBIProcessInfooptions); override;
    procedure SetFileName(const Value: TFileName); override;

  public
    constructor Create(AOwner: TComponent); override;

  end;


  TDBIGitCommandProcessor = class(TDBIGitCustomCommand)
  protected
    function GetCommandLine: String; override;
  end;


  TDBIGitBranchCheckout = class(TDBIGitCustomCommand)
  private
    FBranch: String;
  published
    property Branch: String read FBranch write FBranch stored False;
  end;


  TDBIGitBranchInfo = class(TDBIGitCustomCommand)
  protected
    function GetItemIndex: Integer;
  public
    property ItemIndex: Integer read GetItemIndex;
  end;


  TDBIGitCommitInfo = class(TDBIGitCustomCommand);
  TDBIGitStash = class(TDBIGitCustomCommand);
  TDBIGitUncommit = class(TDBIGitCustomCommand);


  TDBIGitFileCommit = class(TDBIGitCustomCommand)
  private
    FComment: String;
  published
    property Comment: String read FComment write FComment stored False;
  end;


  TDBIGitCustomFileCommand = class(TDBIGitCustomCommand)
  published
    property FileName stored False;
  end;


  TDBIGitLogInfo = class(TDBIGitCustomFileCommand)
  protected
    procedure ProcessOutputLine(const Value: String); override;
  end;


  TDBIGitShowInfo = class(TDBIGitCustomFileCommand)
  private
    FSha1: String;
  published
    property Sha1: String read FSha1 write FSha1 stored False;
  end;


  TDBIGitCustomFileStatus = class(TDBIGitCustomFileCommand)
  private
    FFileName1: String;
    FFileName2: String;
    FLineFormat: String;
    FStatus: String;
    FStatusSize: Integer;
  protected
    procedure ProcessOutputLine(const Value: String); override;
  published
    property FileName1: String read FFileName1 write FFileName2 stored False;
    property FileName2: String read FFileName2 write FFileName2 stored False;
    property LineFormat: String read FLineFormat write FLineFormat;
    property Status: String read FStatus write FStatus stored False;
    property StatusSize: Integer read FStatusSize write FStatusSize stored False;
  end;


  TDBIGitStatusInfo = class(TDBIGitCustomFileStatus);
  TDBIGitFileCommitInfo = class(TDBIGitCustomFileStatus);


  TDBIGitFileCompare = class(TDBIGitCustomFileCommand)
  private
    FLeftHash: String;
    FRightHash: String;
  published
    property LeftHash: String read FLeftHash write FLeftHash stored False;
    property RightHash: String read FRightHash write FRightHash stored False;
  end;


  TDBIGitFileSearch = class(TDBIGitCustomFileCommand)
  private
    FCaption: String;

  public
    class function FindFile(FileName: String): Boolean;

    function Execute: Boolean; override;
    function GetWindowsFileName: String;
    procedure MessageFileNotFound;

    property Caption: String read FCaption write FCaption;

  end;


  TDBIGitFileStage = class(TDBIGitCustomFileCommand)
  private
    FFileStatus: AnsiChar;

  protected
    function GetParameters: String; override;

  published
    property FileStatus: AnsiChar read FFileStatus write FFileStatus stored False;

  end;


  TDBIGitFileCheckout = class(TDBIGitCustomFileCommand);
  TDBIGitFileUnstage = class(TDBIGitCustomFileCommand);


implementation

uses
  SysConst, Types, ShellAPI, UITypes, DBIStrings, DBITypInfo, DBIDialogs, DBITokenizers;


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

function TDBIGitFileSearch.Execute: Boolean;
const
  Title = 'Select applicable file name';

var
  TempName: TFileName;
  FileNames: TStringList;
  Index: Integer;

begin
  TempName := '';
  Output.Clear;

  if (Caption = '') then begin
    Caption := Title;
  end;

  for Index := 0 to Paths.Count-1 do begin
    SourceName := Paths[Index];

    Result := ProcessExecute and (Output.Count > 0);
    if Result then begin
      FileNames := Local(TStringList.Create).Obj as TStringList;
      FileNames.Duplicates := dupIgnore;
      FileNames.Sorted := True;
      FileNames.Assign(Output);
      if (FileNames.Count > 1) then begin
        TempName := TDBIDialogSelect.Execute(FileNames, Caption);
      end
      else begin
        TempName := Output[0];
      end;

      Break;
    end;
  end;

  Result := TempName <> '';
  if Result then begin
    FileName := TempName;
  end;
end;


class function TDBIGitFileSearch.FindFile(FileName: String): Boolean;
var
  Git: TDBIGitFileSearch;
  Index: Integer;

begin
  FileName := ExtractFileName(FileName);

  Result := (FileName <> '');
  if Result then begin
    Git := Local(Self.Create(nil)).Obj as Self;
    Git.FileName := FileName;

    for Index := 0 to Git.Paths.Count-1 do begin
      Git.SourceName := Git.Paths[Index];
      Git.ProcessExecute;

      Result := Git.Output.Count > 0;
      if Result then begin
        Break;
      end;
    end;
  end;
end;


function TDBIGitFileSearch.GetWindowsFileName: String;
begin
  Result := SourceName + StringReplace(FileName, '/', '\', [rfReplaceAll]);
end;


procedure TDBIGitFileSearch.MessageFileNotFound;
const
  Prompt = 'File "%s" not found in git!';
begin
  MessageDlg(Format(Prompt, [FileName]), mtWarning, [mbOK], 0);
end;


{ TDBIGitCustomFileStatus }

procedure TDBIGitCustomFileStatus.ProcessOutputLine(const Value: String);
var
  Offset: Integer;
  FileData: String;
  Line: String;

begin
  Line := '';

  if Length(Value) > 2 then begin
    Status := Copy(Value, 1, StatusSize);        // Copy Two Characters!
    FileData := Copy(Value, StatusSize+2, 126);  // Skip Status+Separator and Copy the rest

    Offset := Pos('->', FileData);
    if (Offset > 0) then begin
      FileName1 := Copy(FileData, 1, Offset-2);
      FileName2 := Copy(FileData, Offset+3, 255);
    end
    else begin
      FileName1 := '';
      FileName2 := FileData;
    end;
    Line := TDBIReflectionProcessor.Format(LineFormat, Self);

    inherited ProcessOutputLine(Line);
  end;
end;





{ TDBIGitLogInfo }

procedure TDBIGitLogInfo.ProcessOutputLine(const Value: String);
const
  ChrSingleQuote = #39;
  ChrAmpersand = #38;
  ChrDoubleQuote = #34;
  ChrVerticalTab = #11;

var
  Index: Integer;
  Line: String;

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






{ TDBIGitBranchInfo }

function TDBIGitBranchInfo.GetItemIndex: Integer;
begin
  for Result := Output.Count-1 downto 0 do begin
    if Pos('*', Output.Strings[Result]) > 0 then begin
      Break;
    end;
  end;
end;





{ TDBIGitCommandProcessor }

function TDBIGitCommandProcessor.GetCommandLine: String;
begin
  Result := inherited GetCommandLine;

  Output.Add('[' + SourceName + ']');
  Output.Add('>> ' + Result);
end;





{ TDBIGitCustomCommand }

constructor TDBIGitCustomCommand.Create(AOwner: TComponent);
begin
  Options := [piRedirectOutput];

  inherited Create(AOwner);

  Visible := False;
end;


function TDBIGitCustomCommand.GetSourceName: String;
begin
  Result := inherited GetSourceName;
  if (Result = '') then begin
    Result := TDBIGitStatusInfo.GetRepositoryName(FileName);
  end;
end;


procedure TDBIGitCustomCommand.SetFileName(const Value: TFileName);
var
  WindowsFileName: String;

begin
  // If Windows Path (Has Drive specifier '$:')
  if Pos(':', Value) > 0 then begin
    WindowsFileName := TDBIWindowsPath.CheckPathName(Value);

    inherited SetFileName(ExtractRelativeUnixName(WindowsFileName, SourceName));
  end
  else begin
    inherited SetFileName(Value);
  end;
end;


procedure TDBIGitCustomCommand.SetOptions(const Value: TDBIProcessInfooptions);
begin
  inherited SetOptions([piRedirectOutput, piVerifyTargetName] + Value);
end;




{ TDBIFileOpenEdit }

class function TDBIFileOpenEdit.Execute(const AFileName: String): Boolean;
var
  FileEdit: TDBIFileOpenEdit;

begin
  Result := AFileName <> '';
  if Result then begin
    FileEdit := Local(Self.Create(nil)).Obj as self;
    FileEdit.SourceName := ExtractFileDir(AFileName);
    FileEdit.FileName := AFileName;
    FileEdit.Execute;
  end;
end;





{ TDBIFileOpenCompare }

class function TDBIFileOpenCompare.Execute(const AFileName1, AFileName2: String): Boolean;
var
  Compare: TDBIFileOpenCompare;

begin
  Result := AFileName1 <> '';
  if Result then begin
    Compare := Local(Self.Create(nil)).Obj as Self;
    Compare.FileName1 := AFileName1;
    Compare.FileName2 := AFileName2;
    Compare.Execute;
  end;
end;


function TDBIFileOpenCompare.GetSourceName: String;
begin
  Result := TDBIHostInfo.GetCacheUserFolder;
end;





{ TDBIGitPersistenceAdapter }

type
  TDBIGitPersistenceAdapter = class(TDBICustomIniFilePersistenceAdapter)
  public
    class procedure Load(Value: TComponent);

  end;

class procedure TDBIGitPersistenceAdapter.Load(Value: TComponent);
begin
  (Local(Self.Create(nil)).Obj as Self).LoadFromFile(Value, ChangeFileExt(ParamStr(0), '.git'));
end;





{ TDBIGitCustomCommandProcessor }

constructor TDBICustomCommandProcessor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TDBIGitPersistenceAdapter.Load(Self);
end;


destructor TDBICustomCommandProcessor.Destroy;
begin
  FreeAndNil(FPaths);

  inherited Destroy;
end;


procedure TDBICustomCommandProcessor.Emit(
  Sender: TObject;
  const Message: String;
  const EmitKind: TDBIProcessEmitKind
  );
begin
  if Assigned(FOnEmit) then FOnEmit(Sender, Message, EmitKind);
end;



function TDBICustomCommandProcessor.Execute: Boolean;
var
  Errors: TStrings;

begin
  Result := ProcessExecute;

  if not Result then begin
    Errors := Local(TStringList.Create).Obj as TStrings;
    Errors.Text := Format(
      'Source: %1:s %0:sTarget: %2:s %0:s%3:s',
      [SLineBreak, SourceName, GetCommandLine, Output.Text]
      );
    TDBIDialogMessage.Execute(Errors, 'ERROR! COMMAND FAILED');
  end;
end;


class function TDBICustomCommandProcessor.ExtractRelativeUnixName(
  const FileName: String;
  const SourceName: String
  ): String;
begin
  Result := Copy(FileName, Length(SourceName)+1, 128);

  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;


function TDBICustomCommandProcessor.GetCommandLine: String;
begin
  CheckTargetName;

  if (FShell <> '') then begin
    Result := TDBIReflectionProcessor.Format(FShell, Self);
  end
  else begin
    Result := TDBIReflectionProcessor.Format(TargetName + ' ' + Parameters, Self);
  end;
end;


function TDBICustomCommandProcessor.GetFileName: TFileName;
begin
  Result := FFileName;
end;


function TDBICustomCommandProcessor.GetPath: String;
begin
  Result := Paths.CommaText;
end;


function TDBICustomCommandProcessor.GetPaths: TStrings;
begin
  if not Assigned(FPaths) then begin
    FPaths := TStringList.Create;
  end;
  Result := FPaths;
end;


function TDBICustomCommandProcessor.GetRepositoryName: String;
begin
  Result := GetRepositoryName(FileName);
end;


class function TDBICustomCommandProcessor.GetRepositoryName(const AFileName: TFileName): String;
const
  GitConfig = '\.git\config';

var
  Git: TDBICustomCommandProcessor;

begin
  Result := AFileName;

  if (Result = '') then begin
    Git := Local(Self.Create(nil)).Obj as Self;
    if (Git.Paths.Count > 0) then begin
      Result := Git.Paths[0];
    end;
    Exit;
  end;

  if not FileExists(Result) then begin
    raise EInOutError.CreateFmt(SFileNotFound + ' "%s"', [Result]);
  end;

  while (Result <> '') do begin
    Result := ExtractFileDir(Result);
    if FileExists(Result + GitConfig) then begin
      Result := Result + '\';
      Break;
    end;
  end;

  if (Result = '') then begin
    raise EInOutError.CreateFmt('No Git Repository found for "%s"', [Result]);
  end;
end;


function TDBICustomCommandProcessor.ProcessExecute: Boolean;
begin
  Emit(Self, '', ioTrace);
  Emit(Self, '( ' + Self.ClassName + ' ) [ ' + SourceName + ' ] ' + GetCommandLine, ioTrace);

  Result := inherited ProcessExecute;
end;


procedure TDBICustomCommandProcessor.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;


procedure TDBICustomCommandProcessor.SetPath(const Value: String);
begin
  Paths.CommaText := Value;
end;





{ TDBIShellExecute }

constructor TDBIShellExecute.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TDBIGitPersistenceAdapter.Load(Self);
end;

function TDBIShellExecute.Execute: Boolean;
const
  Operation = 'open';

begin
  Parameters := TDBIReflectionProcessor.Format(Parameters, Self);
  TargetName := TDBIReflectionProcessor.Format(TargetName, Self);

  Result := ShellExecute(Application.Handle, PChar(Operation), PChar(TargetName), PChar(Parameters), '', SW_SHOWNORMAL) > 32;
end;


class function TDBIShellExecute.Execute(const AFileName: String): Boolean;
var
  Shell: TDBIShellExecute;

begin
  Shell := Local(Self.Create(nil)).Obj as Self;
  Shell.FileName := AFileName;

  Result := Shell.Execute;
end;





{ TDBIVclFiles }

constructor TDBIVclFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  TDBIGitPersistenceAdapter.Load(Self);
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
  if ADataset is TObjectListDataset then begin
    TObjectListDataset(ADataset).ClassTypeName := Self.ClassName;
  end;

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


class procedure TDBIGitStatus.ConfigureFields(ADataset: TDataset);
begin
  ADataset.FieldByName('Status').Alignment := taRightJustify;
  ADataset.FieldByName('Status').DisplayWidth := 4;
  ADataset.FieldByName('FileName').DisplayWidth := 80;
  ADataset.FieldByName('SourceName').DisplayWidth := 80;
end;


class procedure TDBIGitStatus.FilterDataset(ADataset: TDataset);
var
  List: TObjectList;
  Index: Integer;

begin
  if (ADataset is TObjectListDataset) then begin
    ADataset.Close;

    List := TObjectListDataset(ADataset).List;
    for Index := List.Count-1 downto 0 do begin
      if Excluded(List[Index] as TDBIGitStatus) then begin
        List.Delete(Index);
      end;
    end;

    ADataset.Open;
  end;
end;


class function TDBIGitStatus.Excluded(Item: TDBIGitStatus): Boolean;
begin
  Result := False;
end;


class function TDBIGitCommittedStatus.Excluded(Item: TDBIGitStatus): Boolean;
begin
  Result := inherited Excluded(Item);
end;


class function TDBIGitStagedStatus.Excluded(Item: TDBIGitStatus): Boolean;
begin
  Result :=
    (Item.Status[1] <> 'A') and  // Added
    (Item.Status[1] <> 'D') and  // Deleted
    (Item.Status[1] <> 'M') and  // Modified
    (Item.Status[1] <> 'R');     // Renamed
end;


class function TDBIGitUnstagedStatus.Excluded(Item: TDBIGitStatus): Boolean;
begin
  Result :=
    (Item.Status[2] <> 'A') and  // Added
    (Item.Status[2] <> 'D') and  // Deleted
    (Item.Status[2] <> 'M') and  // Modified
    (Item.Status[2] <> 'R');     // Renamed
end;


class function TDBIGitUntrackedStatus.Excluded(Item: TDBIGitStatus): Boolean;
begin
  Result := Item.Status <> '??';
end;





{ TDBIGitLog }

class procedure TDBIGitLog.BuildFieldDefs(ADataset: TDataset);
var
  FieldDef: TFieldDef;

begin
  if ADataset is TObjectListDataset then begin
    TObjectListDataset(ADataset).ClassTypeName := Self.ClassName;
  end;

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


class procedure TDBIGitLog.ConfigureFields(ADataset: TDataset);
begin
  ADataset.FieldByName('Subject').Index := 0;
  ADataset.FieldByName('Subject').DisplayWidth := 64;

  ADataset.FieldByName('AuthorName').Index := 1;
  ADataset.FieldByName('AuthorName').DisplayWidth := 30;

  ADataset.FieldByName('AuthorEmail').Index := 2;
  ADataset.FieldByName('AuthorEmail').DisplayWidth := 30;

  ADataset.FieldByName('AuthorDate').Index := 3;

  ADataset.FieldByName('CommitterName').Index := 4;
  ADataset.FieldByName('CommitterName').DisplayWidth := 30;

  ADataset.FieldByName('CommitterEmail').Index := 5;
  ADataset.FieldByName('CommitterEmail').DisplayWidth := 30;

  ADataset.FieldByName('CommitterDate').Index := 6;
end;



{ TDBIGitPersistent }

class procedure TDBIGitPersistent.FilterDataset(ADataset: TDataset);
begin
  // NOP
end;


class procedure TDBIGitPersistent.LoadDataset(ADataset: TDataset; Strings: TStrings);
var
  Writer: TDBIGitLogDataPacketWriter;

begin
  ADataset.DisableControls;
  try
    ADataset.Close;
    ADataset.FieldDefs.Clear;
    ADataset.Fields.Clear;

    BuildFieldDefs(ADataset);

    Writer := Local(TDBIGitLogDataPacketWriter.Create).Obj as TDBIGitLogDataPacketWriter;
    Writer.Data.Assign(Strings);
    Writer.Dataset := ADataset;

    ADataset.Close;
    ADataset.FieldDefs.Clear;
    ADataset.Fields.Clear;

    if (ADataset is TObjectListDataset) then begin
      TObjectListDataset(ADataset).List.Clear;
      TObjectListDataset(ADataset).LoadFromStream(Writer.Stream, dfXML);
    end
    else if (ADataset is TClientDataset) then begin
      TClientDataset(ADataset).LoadFromStream(Writer.Stream);
    end
    else begin
      raise Exception.CreateFmt('Dataset of type "%s" not supported', [ADataset.ClassName]);
    end;

    FilterDataset(TObjectListDataset(ADataset));
    ConfigureFields(ADataset);

  finally
    ADataset.EnableControls;
  end;
end;



initialization
  Classes.RegisterClass(TDBIGitLog);
  Classes.RegisterClass(TDBIGitStatus);
  Classes.RegisterClass(TDBIGitCommittedStatus);
  Classes.RegisterClass(TDBIGitStagedStatus);
  Classes.RegisterClass(TDBIGitUnstagedStatus);
  Classes.RegisterClass(TDBIGitUntrackedStatus);

end.
