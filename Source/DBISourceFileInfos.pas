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

  TDBICustomSourceFileInfo = class(TComponent)
  private
    FFileData: String;
    FFileName: String;
    FFileLineNo: Integer;
    FGitFileSearch: TDBIGitFileSearch;
    FSha1: String;
    FVclFiles: TDBIFileList;


    class function ExtractValidFileName(FileName: String): String; static;

  protected
    function FileFind: Boolean;
    class function FileOpenBDS(FileName: string; LineNo: Integer): Boolean;

    function GetFileExtension: String;
    function GetGitFileSearch: TDBIGitFileSearch;

    function GetVclFileName(var TargetName: String): Boolean;
    function GetVclFiles: TDBIFileList;
    function GetFileShortName: String;

    property Git: TDBIGitFileSearch read GetGitFileSearch;
    property VclFiles: TDBIFileList read GetVclFiles;

  public
    destructor Destroy; override;

    function CopyToClipboard(const FileNameType: TDBISourceFileNameType): Boolean;
    class function ExtractLineNumber(TextLine: String): Integer;

    function FileOpenWithDefault: Boolean;

    function FileOpenCFM: Boolean;
    function FileOpenDefault: Boolean;
    function FileOpenJS: Boolean;
    function FileOpenPAS: Boolean;
    function FileOpenRegistered: Boolean;
    function FileOpenWith: Boolean;
    function FolderOpen: Boolean;

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

(*##JVR
//    object ToolsPopupMenu: TAction
//      Category := 'Tools';
//      Caption := 'ToolsPopupMenu';
//      OnExecute := ToolsPopupMenuExecute;
//    end
//*)


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
  FolderOpen;
end;

procedure TDBIActionsSourceFile.FileOpenWithExecute(Sender: TObject);
begin
  FileOpenWith;
end;

procedure TDBIActionsSourceFile.FileOpenWithDefaultExecute(Sender: TObject);
begin
  FileOpenWithDefault;
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
      snUnixFileName: ClipBoard.AsText := Git.ExtractRelativeUnixName(FileName, 'C:\');
    end;
  end

  else begin
    Git.Caption := 'Copy file name to clipboard';
    Result := Git.Execute;
    if Result then begin
      case FileNameType of
        snWindowsFileName: ClipBoard.AsText := Git.GetWindowsFileName;
        snUnixFileName: ClipBoard.AsText := Git.FileName;
      end;
    end;
  end;
end;


destructor TDBICustomSourceFileInfo.Destroy;
begin
  FreeAndNil(FGitFileSearch);
  FreeAndNil(FVclFiles);

  inherited Destroy;
end;


class function TDBICustomSourceFileInfo.ExtractLineNumber(TextLine: String): Integer;
begin
  Result := Pos('line', TextLine);
  if (Result <= 0) then begin
    Result := 1;
  end
  else begin
    TextLine := Copy(TextLine, Result + 4, $f);

    Result := StrToIntDef(TextLine, 1);
  end;
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
  Result := TDBIProcess.ProcessExists('bds.exe');
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


function TDBICustomSourceFileInfo.FileOpenCFM: Boolean;
var
  Compare: TDBIBeyondCompare;

begin
  Result := IsValidFileExtension(['.cfm']);
  if Result then begin
    Result := Git.Execute;
    if Result then begin
      Compare := Local(TDBIBeyondCompare.Create(nil)).Obj as TDBIBeyondCompare;
      Compare.Options := [piDetachedProcess];
      Compare.FileName1 := Git.GetWindowsFileName;
      Compare.Execute;
    end;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenDefault: Boolean;
var
  Compare: TDBIBeyondCompare;

begin
  Result := Git.Execute;
  if Result then begin
    Compare := Local(TDBIBeyondCompare.Create(nil)).Obj as TDBIBeyondCompare;
    Compare.Options := [piDetachedProcess];
    Compare.FileName1 := Git.GetWindowsFileName;
    Compare.Execute;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenJS: Boolean;
var
  Compare: TDBIBeyondCompare;
  Http: TDBIHttpStreamAdapter;

begin
  Result := IsValidFileExtension(['.js']);
  if Result then begin
    Http := Local(TDBIHttpStreamAdapter.Create).Obj as TDBIHttpStreamAdapter;
    Http.Url := 'http:' + FileData;

    Result := Http.Load;
    if Result then begin
      Result := Git.Execute;
      if Result then begin
        Http.SaveToFile(TDBIHostInfo.GetCacheUserFolder + Http.DocumentName);

        Compare := Local(TDBIBeyondCompare.Create(nil)).Obj as TDBIBeyondCompare;
        Compare.Options := [piDetachedProcess];
        Compare.FileName1 := TDBIHostInfo.GetCacheUserFolder + Http.DocumentName;
        Compare.FileName2 := Git.GetWindowsFileName;

        Compare.Execute;
      end;
    end;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenPAS: Boolean;
var
  FileName: String;

begin
  Result := IsValidFileExtension(['.pas', '.dpr']);
  if Result then begin
    Result := GetVclFileName(FileName);
    if Result then begin
      Result := FileOpenBDS(FileName, LineNo);
    end
    else begin
      Result := Git.Execute and FileOpenBDS(Git.GetWindowsFileName, LineNo);
    end;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenRegistered: Boolean;
begin
  Result := FileOpenPAS or FileOpenJS or FileOpenCFM or FileOpenDefault;
end;


function TDBICustomSourceFileInfo.FileOpenWith: Boolean;
const
  Run = 'rundll32.exe';

var
  FileName: String;

begin
  Result := GetVclFileName(FileName);
  if Result then begin
    ShellExecute(Application.Handle, 'open', Run, PChar('shell32.dll, OpenAs_RunDLL ' + FileName), nil, SW_SHOWNORMAL);
  end
  else begin
    Result := Git.Execute;
    if Result then begin
      ShellExecute(Application.Handle, 'open', Run, PChar('shell32.dll, OpenAs_RunDLL ' + Git.GetWindowsFileName), nil, SW_SHOWNORMAL);
    end;
  end;
end;


function TDBICustomSourceFileInfo.FileOpenWithDefault: Boolean;
var
  FileName: String;

begin
  Result := GetVclFileName(FileName);
  if Result then begin
    ShellExecute(Application.Handle, 'open', PChar(FileName), '', '', SW_SHOWNORMAL);
  end
  else begin
    Result := Git.Execute;
    if Result then begin
      ShellExecute(Application.Handle, 'open', PChar(Git.GetWindowsFileName), '', '', SW_SHOWNORMAL);
    end;
  end;
end;


function TDBICustomSourceFileInfo.FolderOpen: Boolean;
var
  FileName: String;

begin
  Result := GetVclFileName(FileName);
  if Result then begin
    ShellExecute(Application.Handle, 'open', PChar(PathNames[ptCmdExplorer]), PChar('/select,"' + FileName + '"'), nil, SW_SHOWNORMAL);
  end
  else begin
    Git.Caption := 'Open folder for file';

    if Git.Execute then begin
      ShellExecute(Application.Handle, 'open', PChar(PathNames[ptCmdExplorer]), PChar('/select,"' + Git.GetWindowsFileName + '"'), nil, SW_SHOWNORMAL);
    end;
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


function TDBICustomSourceFileInfo.GetGitFileSearch: TDBIGitFileSearch;
begin
  if not Assigned(FGitFileSearch) then begin
    FGitFileSearch := TDBIGitFileSearch.Create(nil);
  end;
  Result := FGitFileSearch;
end;


function TDBICustomSourceFileInfo.GetVclFileName(var TargetName: String): Boolean;
var
  Index: Integer;

begin
  Index := VclFiles.IndexOfName(ChangeFileExt(FileShortName, ''));
  Result := Index >= 0;
  if Result then begin
    TargetName := VclFiles.FileName[Index];
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

  Git.FileName := FileShortName;
  Git.Caption := 'Select file to open';
end;



end.

