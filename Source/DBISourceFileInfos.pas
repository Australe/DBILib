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
  1.0 | 18/06/2015 06:01:05 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBISourceFileInfos;

interface

uses
  Classes, SysUtils, Windows, Forms, Dialogs, Menus, Actions, ActnList,
  DBIUtils, DBIGitInfos;

type
  TDBISourceFileNameType = (snWindowsFileName, snUnixFileName);
  TDBIShellOpenFileAction = (soFileOpenFolder, soFileOpenWith, soFileOpenWithDefault);

  TDBICustomSourceFileInfo = class(TComponent)
  private
    FFileData: String;
    FFileName: String;
    FFileLineNo: Integer;
    FGitFileSearch: TDBIGitFileSearch;
    FGitFileShow: TDBIGitShowInfo;
    FSha1: String;
    FVclFiles: TDBIFileList;

  protected
    function FileFind: Boolean;

    class function ExtractValidFileName(FileName: String): String; static;
    class function FileOpenBDS(FileName: string; LineNo: Integer): Boolean;
    class function FileOpenCompare(
      const FileName1: String;
      const FileName2: String = ''
      ): Boolean;

    function GetFileExtension: String;

    function GetGitFileName(var FileName: String): Boolean;
    function GetGitFileSearch: TDBIGitFileSearch;
    function GetGitFileShow: TDBIGitShowInfo;
    function GetShaFileName(var FileName: String): Boolean;

    function GetVclFileName(var FileName: String): Boolean;
    function GetVclFiles: TDBIFileList;
    function GetFileShortName: String;


    property GitSearch: TDBIGitFileSearch read GetGitFileSearch;
    property GitSha: TDBIGitShowInfo read GetGitFileShow;
    property VclFiles: TDBIFileList read GetVclFiles;

  public
    destructor Destroy; override;

    function CopyToClipboard(const FileNameType: TDBISourceFileNameType): Boolean;
    class function ExtractLineNumber(TextLine: String): Integer;

    function FileOpenCFM(const FileName: String): Boolean;
    function FileOpenDefault(const FileName: String): Boolean;
    function FileOpenJS(const FileName: String): Boolean;
    function FileOpenPAS(const FileName: String): Boolean;
    function FileOpenRailo: Boolean;
    function FileOpenRegistered: Boolean;
    function FileOpenShell(const Action: TDBIShellOpenFileAction): Boolean;

    function IsRegisteredFileExtension: Boolean;
    function IsValidFileExtension(Extensions: array of String): Boolean;
    function IsValidFileName: Boolean;

    procedure SetFileName(const Value: String);

    property FileExtension: String read GetFileExtension;
    property FileData: String read FFileData;
    property FileShortName: String read GetFileShortName;
    property LineNo: Integer read FFileLineNo write FFileLineNo;
    property Sha1: String read FSha1 write FSha1;

  end;
  TDBISourceFileInfo = class(TDBICustomSourceFileInfo);


type
  TDBIActionsSourceFile = class(TDBICustomSourceFileInfo)
  private
    FCopyUnixFileName: TAction;
    FCopyWindowsFileName: TAction;
    FFileFind: TAction;
    FFileOpen: TAction;
    FFileOpenWith: TAction;
    FFileOpenWithDefault: TAction;
    FFolderOpen: TAction;
    FGitLog: TAction;

  protected
    function GetActionCopyUnixFileName: TAction;
    function GetActionCopyWindowsFileName: TAction;
    function GetActionFileFind: TAction;
    function GetActionFileOpen: TAction;
    function GetActionFileOpenWith: TAction;
    function GetActionFileOpenWithDefault: TAction;
    function GetActionFolderOpen: TAction;
    function GetActionGitLog: TAction;

  protected
    procedure CopyWindowsFileNameExecute(Sender: TObject);
    procedure CopyUnixFileNameExecute(Sender: TObject);
    procedure FileFindExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileOpenWithDefaultExecute(Sender: TObject);
    procedure FileOpenWithExecute(Sender: TObject);
    procedure FolderOpenExecute(Sender: TObject);

  public
    property ActionCopyUnixFileName: TAction read GetActionCopyUnixFileName;
    property ActionCopyWindowsFileName: TAction read GetActionCopyWindowsFileName;
    property ActionFileFind: TAction read GetActionFileFind;
    property ActionFileOpen: TAction read GetActionFileOpen;
    property ActionFileOpenWith: TAction read GetActionFileOpenWith;
    property ActionFileOpenWithDefault: TAction read GetActionFileOpenWithDefault;
    property ActionFolderOpen: TAction read GetActionFolderOpen;
    property ActionGitLog: TAction read GetActionGitLog;

  end;


type
  TDBIPopupMenuSourceFile = class(TPopupMenu)
  private
    FActions: TDBIActionsSourceFile;

    FMenuItemCopyUnixFileName: TMenuItem;
    FMenuItemCopyWindowsFileName: TMenuItem;
    FMenuItemFileOpen: TMenuItem;
    FMenuItemFileOpenWith: TMenuItem;
    FMenuItemFileOpenWithDefault: TMenuItem;
    FMenuItemFolderOpen: TMenuItem;
    FMenuItemGitLog: TMenuItem;
    FMenuItemN1: TMenuItem;
    FMenuItemN2: TMenuItem;
    FMenuItemN3: TMenuItem;
    FMenuItemTitle: TMenuItem;

  protected
    function GetActions: TDBIActionsSourceFile;

  protected
    function GetMenuItemCopyUnixFileName: TMenuItem;
    function GetMenuItemCopyWindowsFileName: TMenuItem;
    function GetMenuItemFileOpen: TMenuItem;
    function GetMenuItemFileOpenWith: TMenuItem;
    function GetMenuItemFileOpenWithDefault: TMenuItem;
    function GetMenuItemFolderOpen: TMenuItem;
    function GetMenuItemGitLog: TMenuItem;
    function GetMenuItemN1: TMenuItem;
    function GetMenuItemN2: TMenuItem;
    function GetMenuItemN3: TMenuItem;
    function GetMenuItemTitle: TMenuItem;

  public
    procedure Popup(X:Integer=-1; Y: Integer=-1); override;
    function FileOpen: Boolean;

    function SetFileData(const FileName: String; const Sha1: String = ''): Boolean;
    function SetFileName(const FileName: String; const LineNo: Integer = 1): Boolean;

    property MenuItemCopyUnixFileName: TMenuItem read GetMenuItemCopyUnixFileName;
    property MenuItemCopyWindowsFileName: TMenuItem read GetMenuItemCopyWindowsFileName;
    property MenuItemFileOpen: TMenuItem read GetMenuItemFileOpen;
    property MenuItemFileOpenWith: TMenuItem read GetMenuItemFileOpenWith;
    property MenuItemFileOpenWithDefault: TMenuItem read GetMenuItemFileOpenWithDefault;
    property MenuItemFolderOpen: TMenuItem read GetMenuItemFolderOpen;
    property MenuItemGitLog: TMenuItem read GetMenuItemGitLog;
    property MenuItemN1: TMenuItem read GetMenuItemN1;
    property MenuItemN2: TMenuItem read GetMenuItemN2;
    property MenuItemN3: TMenuItem read GetMenuItemN3;
    property MenuItemTitle: TMenuItem read GetMenuItemTitle;

    property Actions: TDBIActionsSourceFile read GetActions;

  end;


implementation

uses
  Controls, ShellAPI, ClipBrd, DBIHttpGet, DBIProcesses;



{ TDBIPopupMenuSourceFile }

function TDBIPopupMenuSourceFile.FileOpen: Boolean;
begin
  Result := Actions.FileOpenRegistered;
end;

function TDBIPopupMenuSourceFile.GetActions: TDBIActionsSourceFile;
begin
  if not Assigned(FActions) then begin
    FActions := TDBIActionsSourceFile.Create(Self);
  end;
  Result := FActions;
end;

function TDBIPopupMenuSourceFile.GetMenuItemTitle: TMenuItem;
begin
  if not Assigned(FMenuItemTitle) then begin
    FMenuItemTitle := TMenuItem.Create(Self);
    FMenuItemTitle.Caption := 'Source File';
    FMenuItemTitle.Enabled := False;
  end;
  Result := FMenuItemTitle;
end;

function TDBIPopupMenuSourceFile.GetMenuItemN1: TMenuItem;
begin
  if not Assigned(FMenuItemN1) then begin
    FMenuItemN1 := TMenuItem.Create(Self);
    FMenuItemN1.Caption := '-';
  end;
  Result := FMenuItemN1;
end;

function TDBIPopupMenuSourceFile.GetMenuItemFileOpen: TMenuItem;
begin
  if not Assigned(FMenuItemFileOpen) then begin
    FMenuItemFileOpen := TMenuItem.Create(Self);
    FMenuItemFileOpen.Action := Actions.ActionFileOpen;
    FMenuItemFileOpen.Default := True;
  end;
  Result := FMenuItemFileOpen;
end;

function TDBIPopupMenuSourceFile.GetMenuItemFileOpenWith: TMenuItem;
begin
  if not Assigned(FMenuItemFileOpenWith) then begin
    FMenuItemFileOpenWith := TMenuItem.Create(Self);
    FMenuItemFileOpenWith.Action := Actions.ActionFileOpenWith;
  end;
  Result := FMenuItemFileOpenWith;
end;

function TDBIPopupMenuSourceFile.GetMenuItemFileOpenWithDefault: TMenuItem;
begin
  if not Assigned(FMenuItemFileOpenWithDefault) then begin
    FMenuItemFileOpenWithDefault := TMenuItem.Create(Self);
    FMenuItemFileOpenWithDefault.Action := Actions.ActionFileOpenWithDefault;
  end;
  Result := FMenuItemFileOpenWithDefault;
end;

function TDBIPopupMenuSourceFile.GetMenuItemFolderOpen: TMenuItem;
begin
  if not Assigned(FMenuItemFolderOpen) then begin
    FMenuItemFolderOpen := TMenuItem.Create(Self);
    FMenuItemFolderOpen.Action := Actions.ActionFolderOpen;
  end;
  Result := FMenuItemFolderOpen;
end;

function TDBIPopupMenuSourceFile.GetMenuItemN2: TMenuItem;
begin
  if not Assigned(FMenuItemN2) then begin
    FMenuItemN2 := TMenuItem.Create(Self);
    FMenuItemN2.Caption := '-';
  end;
  Result := FMenuItemN2;
end;

function TDBIPopupMenuSourceFile.GetMenuItemCopyWindowsFileName: TMenuItem;
begin
  if not Assigned(FMenuItemCopyWindowsFileName) then begin
    FMenuItemCopyWindowsFileName := TMenuItem.Create(Self);
    FMenuItemCopyWindowsFileName.Action := Actions.ActionCopyWindowsFileName;
  end;
  Result := FMenuItemCopyWindowsFileName;
end;

function TDBIPopupMenuSourceFile.GetMenuItemCopyUnixFileName: TMenuItem;
begin
  if not Assigned(FMenuItemCopyUnixFileName) then begin
    FMenuItemCopyUnixFileName := TMenuItem.Create(Self);
    FMenuItemCopyUnixFileName.Action := Actions.ActionCopyUnixFileName;
  end;
  Result := FMenuItemCopyUnixFileName;
end;

function TDBIPopupMenuSourceFile.GetMenuItemN3: TMenuItem;
begin
  if not Assigned(FMenuItemN3) then begin
    FMenuItemN3 := TMenuItem.Create(Self);
    FMenuItemN3.Caption := '-';
  end;
  Result := FMenuItemN3;
end;

function TDBIPopupMenuSourceFile.GetMenuItemGitLog: TMenuItem;
begin
  if not Assigned(FMenuItemGitLog) then begin
    FMenuItemGitLog := TMenuItem.Create(Self);
    FMenuItemGitLog.Action := Actions.ActionGitLog;
  end;
  Result := FMenuItemGitLog;
end;

procedure TDBIPopupMenuSourceFile.Popup(X, Y: Integer);
begin
  if (Items.Count <= 0) then begin
    AutoHotkeys := maManual;

    Items.Add(MenuItemTitle);
    Items.Add(MenuItemN1);
    Items.Add(MenuItemCopyUnixFileName);
    Items.Add(MenuItemCopyWindowsFileName);
    Items.Add(MenuItemN2);
    Items.Add(MenuItemFileOpen);
    Items.Add(MenuItemFileOpenWith);
    Items.Add(MenuItemFileOpenWithDefault);
    Items.Add(MenuItemFolderOpen);
    Items.Add(MenuItemN3);
    Items.Add(MenuItemGitLog);
  end;

  if (X = -1) then begin
     X := Mouse.CursorPos.X;
  end;

  if (Y = -1) then begin
    Y := Mouse.CursorPos.Y;
  end;

  inherited Popup(X, Y);
end;


function TDBIPopupMenuSourceFile.SetFileData(const FileName: String; const Sha1: String): Boolean;
begin
  Actions.SetFileName(FileName);
  Actions.Sha1 := Sha1;

  Result := Actions.IsValidFileName;
  if Result then begin
    MenuItemTitle.Caption := Actions.FileShortName;
  end;
end;


function TDBIPopupMenuSourceFile.SetFileName(const FileName: String; const LineNo: Integer): Boolean;
begin
  Actions.SetFileName(FileName);
  Actions.LineNo := LineNo;

  Result := Actions.IsValidFileName;
  if Result then begin
    MenuItemTitle.Caption := Actions.FileShortName;
  end;
end;





{ TDBISourceFileActions }

procedure TDBIActionsSourceFile.CopyUnixFileNameExecute(Sender: TObject);
begin
  CopyToClipboard(snUnixFileName);
end;

procedure TDBIActionsSourceFile.CopyWindowsFileNameExecute(Sender: TObject);
begin
  CopyToClipboard(snWindowsFileName);
end;

function TDBIActionsSourceFile.GetActionCopyUnixFileName: TAction;
begin
  if not Assigned(FCopyUnixFileName) then begin
    FCopyUnixFileName := TAction.Create(Self);
    FCopyUnixFileName.Category := 'Tools';
    FCopyUnixFileName.Caption := 'Copy Unix file name';
    FCopyUnixFileName.OnExecute := CopyUnixFileNameExecute;
  end;
  Result := FCopyUnixFileName;
end;

function TDBIActionsSourceFile.GetActionCopyWindowsFileName: TAction;
begin
  if not Assigned(FCopyWindowsFileName) then begin
    FCopyWindowsFileName := TAction.Create(Self);
    FCopyWindowsFileName.Category := 'Tools';
    FCopyWindowsFileName.Caption := 'Copy Windows file name';
    FCopyWindowsFileName.OnExecute := CopyWindowsFileNameExecute;
  end;
  Result := FCopyWindowsFileName;
end;

function TDBIActionsSourceFile.GetActionFileFind: TAction;
begin
  if not Assigned(FFileFind) then begin
    FFileFind := TAction.Create(Self);
    FFileFind.Category := 'Tools';
    FFileFind.Caption := 'Find';
    FFileFind.OnExecute := FileFindExecute;
  end;
  Result := FFileFind;
end;

function TDBIActionsSourceFile.GetActionFileOpen: TAction;
begin
  if not Assigned(FFileOpen) then begin
    FFileOpen := TAction.Create(Self);
    FFileOpen.Category := 'Tools';
    FFileOpen.Caption := 'Open';
    FFileOpen.OnExecute := FileOpenExecute;
  end;
  Result := FFileOpen;
end;

function TDBIActionsSourceFile.GetActionFileOpenWith: TAction;
begin
  if not Assigned(FFileOpenWith) then begin
    FFileOpenWith := TAction.Create(Self);
    FFileOpenWith.Category := 'Tools';
    FFileOpenWith.Caption := 'Open with...';
    FFileOpenWith.OnExecute := FileOpenWithExecute;
  end;
  Result := FFileOpenWith;
end;

function TDBIActionsSourceFile.GetActionFileOpenWithDefault: TAction;
begin
  if not Assigned(FFileOpenWithDefault) then begin
    FFileOpenWithDefault := TAction.Create(Self);
    FFileOpenWithDefault.Category := 'Tools';
    FFileOpenWithDefault.Caption := 'Open with default application';
    FFileOpenWithDefault.OnExecute := FileOpenWithDefaultExecute;
  end;
  Result := FFileOpenWithDefault;
end;

function TDBIActionsSourceFile.GetActionFolderOpen: TAction;
begin
  if not Assigned(FFolderOpen) then begin
    FFolderOpen := TAction.Create(Self);
    FFolderOpen.Category := 'Tools';
    FFolderOpen.Caption := 'Open Containing Folder';
    FFolderOpen.OnExecute := FolderOpenExecute;
  end;
  Result := FFolderOpen;
end;

function TDBIActionsSourceFile.GetActionGitLog: TAction;
begin
  if not Assigned(FGitLog) then begin
    FGitLog := TAction.Create(Self);
    FGitLog.Category := 'Tools';
    FGitLog.Caption := 'Git Log';
    FGitLog.OnExecute := nil;
  end;
  Result := FGitLog;
end;


procedure TDBIActionsSourceFile.FileFindExecute(Sender: TObject);
begin
  FileFind;
end;


procedure TDBIActionsSourceFile.FileOpenExecute(Sender: TObject);
begin
  FileOpenRegistered;
end;

procedure TDBIActionsSourceFile.FolderOpenExecute(Sender: TObject);
begin
  FileOpenShell(soFileOpenFolder);
end;

procedure TDBIActionsSourceFile.FileOpenWithExecute(Sender: TObject);
begin
  FileOpenShell(soFileOpenWith);
end;

procedure TDBIActionsSourceFile.FileOpenWithDefaultExecute(Sender: TObject);
begin
  FileOpenShell(soFileOpenWithDefault);
end;





{ TDBICustomSourceFileInfo }

function TDBICustomSourceFileInfo.CopyToClipboard(const FileNameType: TDBISourceFileNameType): Boolean;
var
  FileName: String;

begin
  ClipBoard.AsText := FileShortName;

  Result := GetVclFileName(FileName);
  if Result then begin
    case FileNameType of
      snWindowsFileName: ClipBoard.AsText := FileName;
      snUnixFileName: ClipBoard.AsText := GitSearch.ExtractRelativeUnixName(FileName, 'C:\');
    end;
  end

  else begin
    GitSearch.Caption := 'Copy file name to clipboard';
    Result := GitSearch.Execute;
    if Result then begin
      case FileNameType of
        snWindowsFileName: ClipBoard.AsText := GitSearch.GetWindowsFileName;
        snUnixFileName: ClipBoard.AsText := GitSearch.FileName;
      end;
    end;
  end;
end;


destructor TDBICustomSourceFileInfo.Destroy;
begin
  FreeAndNil(FGitFileShow);
  FreeAndNil(FGitFileSearch);
  FreeAndNil(FVclFiles);

  inherited Destroy;
end;


class function TDBICustomSourceFileInfo.ExtractLineNumber(TextLine: String): Integer;
begin
  Result := Pos('line', TextLine);
  if (Result > 0) then begin
    TextLine := Copy(TextLine, Result + 4, $f);

    Result := StrToIntDef(TextLine, 1);
    Exit;
  end;

  Result := Pos('.java:', TextLine);
  if (Result > 0) then begin
    Result := Pos('):', TextLine, Result);
    TextLine := Copy(TextLine, Result + 2, $f);

    Result := StrToIntDef(TextLine, 1);
    Exit;
  end;

  Result := 1;
end;


class function TDBICustomSourceFileInfo.ExtractValidFileName(FileName: String): String;
var
  Index: Integer;

begin
  // Strip Url parameters if they exist
  Index := Pos('?', FileName);
  if Index > 0 then begin
    FileName := Copy(FileName, 0, Index-1);
  end;

  Result := StringReplace(FileName, '/', '\', [rfReplaceAll]);
end;


function TDBICustomSourceFileInfo.FileFind: Boolean;
begin
  Result := True;

//##JVR  ClipBoard.AsText := FileShortName;
  ShellExecute(Application.Handle, 'find', PChar(PathNames[ptXE3]), PChar(FileShortName), nil, SW_SHOWNORMAL);
end;


class function TDBICustomSourceFileInfo.FileOpenBDS(FileName: string; LineNo: Integer): Boolean;
var
  WmX: Word;
  Buffer: array[0..255] of char;
  Atom: TAtom;
  Str: string;

begin
  Result := FileExists(FileName) and TDBIProcess.ProcessExists('bds.exe');
  if Result then begin
    Wmx:= RegisterWindowMessage('omIDEFileOpener');

    Str:= FileName + '@' + IntToStr(LineNo);

    Atom:= GlobalAddAtom(StrPCopy(Buffer, Str));
    try
      SendMessage(HWND_Broadcast, WmX, 0, Atom);
    finally
      GlobalDeleteAtom(Atom);
    end;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenCFM(const FileName: String): Boolean;
begin
  Result := IsValidFileExtension(['.cfm']) and FileOpenCompare(FileName);
end;


class function TDBICustomSourceFileInfo.FileOpenCompare(const FileName1, FileName2: String): Boolean;
var
  Compare: TDBIBeyondCompare;

begin
  Result := FileName1 <> '';
  if Result then begin
    Compare := Local(TDBIBeyondCompare.Create(nil)).Obj as TDBIBeyondCompare;
    Compare.Options := [piDetachedProcess, piVerifyTargetName];
    Compare.FileName1 := FileName1;
    Compare.FileName2 := FileName2;
    Compare.Execute;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenDefault(const FileName: String): Boolean;
begin
  Result := FileOpenCompare(FileName);
end;


function TDBICustomSourceFileInfo.FileOpenJS(const FileName: String): Boolean;
var
  Http: TDBIHttpStreamAdapter;

begin
  Result := IsValidFileExtension(['.js']);
  if Result then begin
    Http := Local(TDBIHttpStreamAdapter.Create).Obj as TDBIHttpStreamAdapter;
    Http.Url := 'http:' + FileData;

    Result := Http.Load;
    if Result then begin
      ForceDirectories(TDBIHostInfo.GetCacheUserFolder);
      Http.SaveToFile(TDBIHostInfo.GetCacheUserFolder + Http.DocumentName);

      FileOpenCompare(TDBIHostInfo.GetCacheUserFolder + Http.DocumentName, FileName);
    end;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenPAS(const FileName: String): Boolean;
begin
  Result := IsValidFileExtension(['.pas', '.dpr']) and FileOpenBDS(FileName, LineNo);
end;


function TDBICustomSourceFileInfo.FileOpenRailo: Boolean;
var
  FileName: String;

  function GetRailoURL: String;
  const
    RailoRuntime = 'https://github.com/getrailo/railo/blob/master/railo-java/railo-core/src/';
    RailoLoader = 'https://github.com/getrailo/railo/blob/master/railo-java/railo-loader/src/';

  var
    Offset: Integer;
    RailoSource: String;

  begin
    Result := '';
    if (Pos('railo.runtime', FileData) = 1) then begin
      RailoSource := RailoRuntime;
    end
    else if (Pos('railo.loader', FileData) = 1) then begin
      RailoSource := RailoLoader;
    end
    else begin
      RailoSource := RailoRuntime;
    end;

    Offset := Pos('(', FileData);
    if Offset > 1 then begin
      Result := Copy(FileData, 1, Offset - 1);
      Result := ChangeFileExt(Result, '');
      Result := StringReplace(Result, '.', '/', [rfReplaceAll]);
      Result := ChangeFileExt(Result, FileExtension);
      Result := RailoSource + Result + '#L' + IntToStr(LineNo);
    end;
  end;

begin
  Result := (FileExtension = '.java') and (Pos('railo.', FileData) = 1);
  if Result then begin
    FileName := GetRailoURL;
    Result := ShellExecute(Application.Handle, 'open', PChar(FileName), '', '', SW_SHOWNORMAL) > 32;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenRegistered: Boolean;
var
  FileName: String;

begin
  Result := FileOpenRailo;
  if not Result then begin
    Result := GetVclFileName(FileName) or GetShaFileName(FileName);
    if Result then begin
      Result := FileOpenPAS(FileName) or FileOpenJS(FileName) or FileOpenCFM(FileName) or FileOpenDefault(FileName);
    end;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenShell(const Action: TDBIShellOpenFileAction): Boolean;
var
  FileName: String;
  Parameters: String;

begin
  Result := GetVclFileName(FileName) or GetShaFileName(FileName);
  if Result then begin
    case Action of
      soFileOpenFolder: begin
        Parameters := '/select,"' + FileName + '"';
        FileName := PathNames[ptCmdExplorer];
      end;

      soFileOpenWith: begin
        Parameters := 'shell32.dll, OpenAs_RunDLL ' + FileName;
        FileName := PathNames[ptCmdRunDll];
      end;

      soFileOpenWithDefault: Parameters := '';
    end;

    Result := ShellExecute(Application.Handle, 'open', PChar(FileName), PChar(Parameters), '', SW_SHOWNORMAL) > 32;
  end;
end;


function TDBICustomSourceFileInfo.GetFileExtension: String;
begin
  Result := ExtractFileExt(FileShortName);
end;


function TDBICustomSourceFileInfo.GetFileShortName: String;
begin
  Result := ExtractFileName(FFileName);
end;


function TDBICustomSourceFileInfo.GetGitFileName(var FileName: String): Boolean;
begin
  Result := GitSearch.Execute;
  if Result then begin
    FileName := GitSearch.GetWindowsFileName;
  end;
end;


function TDBICustomSourceFileInfo.GetGitFileSearch: TDBIGitFileSearch;
begin
  if not Assigned(FGitFileSearch) then begin
    FGitFileSearch := TDBIGitFileSearch.Create(nil);
    FGitFileSearch.Options := FGitFileSearch.Options + [piVerifyTargetName];
  end;
  Result := FGitFileSearch;
end;


function TDBICustomSourceFileInfo.GetGitFileShow: TDBIGitShowInfo;
begin
  if not Assigned(FGitFileShow) then begin
    FGitFileShow := TDBIGitShowInfo.Create(nil);
  end;
  Result := FGitFileShow;
end;


function TDBICustomSourceFileInfo.GetShaFileName(var FileName: String): Boolean;
begin
  Result := Sha1 = '';

  if Result then begin
    Result := GitSearch.Execute;
    if Result then begin
      FileName := GitSearch.GetWindowsFileName;
    end;
  end

  // Sha specified
  else begin
    Result := GitSearch.Execute;
    if Result then begin
      GitSha.Output.Clear;

      GitSha.Sha1 := Sha1;
      GitSha.FileName := GitSearch.FileName;
      GitSha.SourceName := GitSearch.SourceName;
      Result := GitSha.Execute;
      if Result then begin
        FileName := TDBIHostInfo.GetCacheUserFolder + GitSha.Sha1 + '\' + FileShortName;
        ForceDirectories(ExtractFileDir(FileName));

        GitSha.Output.SaveToFile(FileName);
      end;
    end;
  end;
end;


function TDBICustomSourceFileInfo.GetVclFileName(var FileName: String): Boolean;
var
  Index: Integer;

begin
  Index := VclFiles.IndexOfName(ChangeFileExt(FileShortName, ''));
  Result := Index >= 0;
  if Result then begin
    FileName := VclFiles.FileName[Index];
  end;
end;


function TDBICustomSourceFileInfo.GetVclFiles: TDBIFileList;
begin
  if not Assigned(FVclFiles) then begin
    FVclFiles := TDBIFileList.Create;
    FVclFiles.Duplicates := dupIgnore;
    FVclFiles.Sorted := True;
    FVclFiles.GetFiles(PathNames[ptXe3], '*.pas');
  end;
  Result := FVclFiles;
end;


function TDBICustomSourceFileInfo.IsRegisteredFileExtension: Boolean;
begin
  Result := DBIVerifyExtensionRegistered(FileExtension);
end;


function TDBICustomSourceFileInfo.IsValidFileExtension(Extensions: array of String): Boolean;
var
  Index: Integer;

begin
  Result := False;

  for Index := Low(Extensions) to High(Extensions) do begin
    Result := CompareText(FileExtension, Extensions[Index]) = 0;
    if Result then begin
      Break;
    end;
  end;
end;


function TDBICustomSourceFileInfo.IsValidFileName: Boolean;
begin
  Result := DBIVerifyExtensionRegistered(FileExtension) or TDBIGitFileSearch.FindFile(FileShortName);
end;


procedure TDBICustomSourceFileInfo.SetFileName(const Value: String);
begin
  FFileData := Value;
  FFileName := ExtractValidFileName(Value);

  GitSearch.FileName := FileShortName;
  GitSearch.Caption := 'Select file to open';
end;



end.

