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
  1.0 | 09/02/2001 15:40:38 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIUtils;

interface

{$I DBICompilers.inc}

uses
{$ifndef fpc}
  Forms,
{$endif}  
  Windows, Messages, Classes, SysUtils, DB, DBIConst, DBIIntfConsts;

type
  TDBIDebugKind = (
    dkAudit,
    dkBasic,
    dkDebug,
    dkDetailed,
    dkError,
    dkLogging,
    dkMedium,
    dkIntermediate,
    dkAdvanced
    );
  TDBIDebugKinds = set of TDBIDebugKind;

const
  DBIDebugKinds = [];

type

  { TDBIDebugInfo }

  TDBIDebugInfo = class(TPersistent)
  public
    class procedure Display(const Msg: String; Args: array of const);

    class function GetAttributes(const Value: TFieldAttributes): String;
    class function GetDataType(const Value: TFieldType): String;

    class procedure LogMsg(const Kind: TDBIDebugKind; const Msg: String; Args: array of const);
  end;


const
  BoolName: array[Boolean] of String = ('[_]', '[X]');

  OpenModes: array[Boolean] of Word = (fmOpenReadWrite, fmOpenRead);
  ShareModes: array[Boolean] of Word = (fmShareDenyNone, fmShareExclusive);
  CreateModes: array[Boolean] of Word = ($0000, fmCreate);

type
  TDBIFieldMap = class(TPersistent)
  private
    FSourceFields: TList;
    FTargetFields: TList;

  protected
    function GetFieldCount: Integer;
    function GetSourceField(Index: Integer): TField;
    function GetTargetField(Index: Integer): TField;

  public
    constructor Create(Source: TDataset; Target: TDataset);
    destructor Destroy; override;

    property FieldCount: Integer read GetFieldCount;
    property SourceFields[Index: Integer]: TField read GetSourceField;
    property TargetFields[Index: Integer]: TField read GetTargetField;
  end;  { TDBIFieldMap }


type
  TDBINullFlags = class(TPersistent)
  private
    FSize: Integer;
    FBits: Pointer;
    FIsNullable: Boolean;
    FNullFlagsIndex: array[0..255] of Integer;

  protected
    procedure Error;

    function GetBit(Index: Integer): Boolean;
    function GetIndex(Index: Integer): Integer;
    function GetIsNull(Index: Integer): Boolean;

    procedure SetBit(Index: Integer; Value: Boolean);
    procedure SetIndex(Index: Integer; Value: Integer);
    procedure SetIsNull(Index: Integer; Value: Boolean);

  public
    procedure Log(const Index: Integer);

    procedure SetBuffer(const PBuffer: Pointer; const ASize: Integer);
    function SetNullIndex(
      Index: Integer;
      IsNullableField: Boolean;
      Value: Integer
      ): Boolean;

//    property Bits[Index: Integer]: Boolean read GetBit write SetBit;
    property NullIndex[Index: Integer]: Integer read GetIndex write SetIndex;
    property Size: Integer read FSize;
    property IsNull[Index: Integer]: Boolean read GetIsNull write SetIsNull; default;
    property IsNullable: Boolean read FIsNullable write FIsNullable;

  end;  { TDBINullFlags }


{$ifndef fpc}
type
  TDBICommandCallBack = procedure (const Parameters: String) of object;

  TDBIApplication = class(TComponent)
  private
    FOnCommand: TDBICommandCallBack;

  protected
    procedure DoCommand(const Parameters: String);

    class function ForceForeGroundWindow(Handle: THandle): Boolean;

    function HookMessage(var Message: TMessage): Boolean;

    class procedure HookApplication;
    class function FindApplication(Handle: THandle): Boolean;
    class function CallCommand(var Message: TMessage; CallBack: TDBICommandCallBack): Boolean; overload;
    class function CallCommand: Boolean; overload;

  public
    class function ApplicationExists: Boolean;
    class function ForceToFront(Form: TCustomForm): Boolean;
    class function Instance: TDBIApplication;
    class procedure UnHookApplication;

    property OnCommand: TDBICommandCallBack read FOnCommand write FOnCommand;
  end;
{$endif}


type
  TDBIFileListAction = (flaInclude, flaExclude);

  TDBIFileList = class(TStringList)
  protected
    function GetFileName(const Index: Integer): TFileName;

{$ifndef DELPHI2006}
    function GetValueFromIndex(Index: Integer): String;

    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex;
{$endif}

  public
    function GetFile(
      const FileName: TFileName;
      const Action: TDBIFileListAction = flaInclude
      ): Boolean;

    function GetFiles(
      const PathName: TFileName;
      const FileMask: String;
      const Action: TDBIFileListAction = flaInclude
      ): Boolean;

    property FileName[const Index: Integer]: TFileName read GetFileName;
  end;


type
  TDBIHostInfo = class(TPersistent)
  public
    class function GetCacheUserFolder: String;
    class function GetComputerName: String;
    class function GetLocalHostName: String;
    class function GetUserName: WideString;

  public
    property CacheUserFolder: String read GetCacheUserFolder;
    property ComputerName: String read GetComputerName;
    property HostName: String read GetLocalHostName;
    property UserName: WideString read GetUserName;

  end;


type
  TDBIMouseInfo = class(TPersistent)
  public
    class function GetShiftState: TShiftState;

  end;


type
  ILocalInstance = interface(IUnknown)
    function Add(Instance: TObject): TObject;
  end;

  TInstanceRecord = record
    Guard: ILocalInstance;
    Obj: TObject;
  end;

function Local(Instance: TObject): TInstanceRecord;


// General Helper routines
procedure Check(Status: DBIResult);

function DBIForceDirectories(Dir: string): Boolean;
function DBIModuleName: String;
function DBIModuleDateTime(AModuleName: String = ''): TDateTime;
function DBIStrDateStampToDateTime(PDateStamp: PAnsiChar): TDateTime;
function DBIStrTimeStampToDateTime(PTimeStamp: PAnsiChar; const MilliSeconds: Boolean = True): TDateTime;
function DBITempFolder: String;
function DBIVerifyExtensionRegistered(const Extension: String): Boolean;


{$ifndef DELPHI6}
function FileIsReadOnly(const FileName: string): Boolean;
{$endif}

{$ifdef fpc}
function GetEnvironmentVariable(lpName: PChar; lpBuffer: PChar; nSize: Cardinal): LongWord;
{$endif}

type
  TDBIHtmlToTextOption = (
    htRemoveQuote,
    htRemoveApostrophe,
    htRemoveLineBreak
    );
  TDBIHtmlToTextOptions = set of TDBIHtmlToTextOption;

function HtmlToText(Html: String; const Options: TDBIHtmlToTextOptions): String;

function SwapDWord(Value: LongWord): LongWord;
function SwapDouble(Value: Double): Double;

function SystemErrorMessageParam(Param: String = ''): String;


{$ifdef fpc}
var                           { Taken from Delphi System.pas }
  HeapAllocFlags: Word = 2;   { Heap allocation flags, gmem_Moveable }
{$endif}


implementation

uses
  WinSock, TypInfo, Dialogs, Contnrs, Registry, DBIXbaseConsts;


{ ILocalInstance }

type
  TLocalInstance = class(TInterfacedObject, ILocalInstance)
  private
    FList: TObjectList;

  public
    constructor Create;
    destructor Destroy; override;
    function Add(Instance: TObject): TObject;
  end;


function TLocalInstance.Add(Instance: TObject): TObject;
begin
  FList.Add(Instance);

  Result := Instance;
end;

constructor TLocalInstance.Create;
begin
  FList := TObjectList.Create;
end;

destructor TLocalInstance.Destroy;
begin
  FList.Free;
  FList := nil;

  inherited Destroy;
end;

function Local(Instance: TObject): TInstanceRecord;
begin
  if not Assigned(Result{%H-}.Guard) then begin
    Result.Guard := TLocalInstance.Create;
  end;
  Result.Obj := Result.Guard.Add(Instance);
end;





{ TDBIMouseInfo }

class function TDBIMouseInfo.GetShiftState: TShiftState;
var
  KeyState: TKeyboardState;

begin
  GetKeyboardState(KeyState);
  Result := KeyboardStateToShiftState(KeyState);
end;





class function TDBIHostInfo.GetCacheUserFolder: String;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'cache\' + TDBIHostInfo.GetUserName + '\';
end;


// _____________________________________________________________________________
{**
  Jvr - 26/07/2005 17:20:20 - Initial code.<br>
}
class function TDBIHostInfo.GetComputerName: String;
var
  Buffer: array[0..255] of Char;
  Size: LongWord;

begin
  Size := SizeOf(Buffer);
{$Warnings Off}
  Win32Check(Windows.GetComputerName(@Buffer[0], Size));
{$Warnings On}
  Result := string(Buffer);
end;


// _____________________________________________________________________________
{**
  Jvr - 04/09/2002 13:52:42.<P>
}
class function TDBIHostInfo.GetUserName: WideString;
var
  Buffer: array[0..255] of WideChar;
  Size: LongWord;
begin
  Size := SizeOf(Buffer);
{$ifdef Delphi6}
  if not Windows.GetUserNameW(@Buffer[0], Size) then RaiseLastOSError;
{$else}
  Win32Check(Windows.GetUserNameW(@Buffer[0], Size));
{$endif}
  Result := WideString(Buffer);
end;


// _____________________________________________________________________________
{**
  Jvr - 23/01/2003 15:22:26.<P>
}
class function TDBIHostInfo.GetLocalHostName: String;
{$ifdef fpc}
begin
  Result := 'localpc';
end;
{$else}
var
  HostName: AnsiString;

begin
  SetLength(HostName, 255);
  if WinSock.GetHostName(PAnsiChar(HostName), Length(HostName)) <> 0 then begin
{$ifdef DELPHI6}
    RaiseLastOSError;
{$else}
    RaiseLastWin32Error;
{$endif}
  end;
  SetLength(HostName, StrLen(PAnsiChar(HostName)));

  Result := String(HostName);
end;
{$endif}





{ TDBIFileList }

function TDBIFileList.GetFileName(const Index: Integer): TFileName;
begin
  Result := ValueFromIndex[Index];
end;


function TDBIFileList.GetFile(
  const FileName: TFileName;
  const Action: TDBIFileListAction = flaInclude
  ): Boolean;
var
  FileIndex: Integer;
  ItemName: String;

begin
  ItemName := ChangeFileExt(ExtractFileName(FileName), '=') + FileName;
  FileIndex :=  IndexOf(ItemName);

  Result :=  (Action = flaInclude) and (FileIndex < 0);
  if Result then begin
    Add(ItemName);
  end
  else begin
    Result :=  (Action = flaExclude) and (FileIndex > -1);
    if Result then begin
      Delete(FileIndex);
    end;
  end;
end;


function TDBIFileList.GetFiles(
  const PathName: TFileName;
  const FileMask: String;
  const Action: TDBIFileListAction = flaInclude
  ): Boolean;
var
  FileInfo: TSearchRec;

begin
  if SysUtils.FindFirst(PathName + '*.*', faDirectory, FileInfo) = 0 then begin
    repeat
      if (FileInfo.Name <> '.') and (FileInfo.Name <> '..') then begin
        if FileInfo.Attr = faDirectory then begin
          GetFiles(PathName + FileInfo.Name + '\', FileMask, Action);
        end;
      end;
    until SysUtils.FindNext(FileInfo) <> 0;

    SysUtils.FindClose(FileInfo);
  end;

  if SysUtils.FindFirst(PathName + FileMask, faAnyFile, FileInfo) = 0 then begin
    repeat
      if FileInfo.Attr <> faDirectory then begin
        GetFile(PathName + FileInfo.Name, Action);
      end;
    until SysUtils.FindNext(FileInfo) <> 0;

    SysUtils.FindClose(FileInfo);
  end;

  Result := Count > 0;
end;


{$ifndef DELPHI2006}
function TDBIFileList.GetValueFromIndex(Index: Integer): string;
const
  NameValueSeparator = '=';

var
  SepPos: Integer;

begin
  Result := '';

  if Index >= 0 then begin
    Result := Get(Index);
    SepPos := AnsiPos(NameValueSeparator, Result);
    if (SepPos > 0) then begin
      System.Delete(Result, 1, SepPos);
    end;
  end;
end;
{$endif}





{$ifndef fpc}
{ TDBIApplication }

const
  WM_Call_Command = WM_USER + 1;

var
  _ApplicationHandle: THandle = 0;

function EnumWindowsCallback(Handle: THandle; Param: LParam): Boolean; Stdcall;
begin
  Result := (Handle <> Application.Handle) and not TDBIApplication.FindApplication(Handle);
  if not Result then begin
    _ApplicationHandle := Handle;
  end;
end;


class function TDBIApplication.ApplicationExists: Boolean;
begin
  Result :=
    (CreateSemaphore(nil, 0, 1, PChar(Application.ExeName)) = 0) or
    (GetLastError = Error_Already_Exists);

  if Result then begin
    SetWindowText(Application.Handle, PChar(Application.Title));

    _ApplicationHandle := 0;

    EnumWindows(@EnumWindowsCallback, 0);
    Result := (_ApplicationHandle <> 0) and (_ApplicationHandle <> Application.Handle);

    if Result then begin
      CallCommand;
    end
    else begin
      HookApplication;
    end;
  end;
end;

(*
In several Delphi PC Magazine utilities, Neil Rubenking uses a different approach.  Here's how Neil
prevented two instances of his recent KeyClick utility from running:

type
  THandles = Record
    AppHandle: HWnd;
    WinHandle: HWnd;
  end;

var
  hMap: THandle;
  PH: ^THandles;

begin
  hMap := OpenFileMapping(FILE_MAP_ALL_ACCESS, False, MainMapName);

  // first instance
  if hMap = 0 then begin
    hMap := CreateFileMapping($FFFFFFFF, NIL, PAGE_READWRITE, 0, SizeOf(THandles), MainMapName);
    PH := MapViewOfFile(hMap, FILE_MAP_ALL_ACCESS, 0, 0, 0);

    Application.Initialize;
    Application.Title := 'KeyTick';
    Application.CreateForm(TMainForm, MainForm);
    Application.MainForm.HandleNeeded;

    PH^.AppHandle := Application.Handle;
    PH^.WinHandle := Application.Mainform.Handle;

    Application.Run;
  end

  // Any instance after the first
  else begin
    PH := MapViewOfFile(hMap, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    SendMessage(PH^.WinHandle, regMsg, -1, -1);
  end;

  UnMapViewOfFile(PH);
  CloseHandle(hMap);
end.

//*)

class function TDBIApplication.CallCommand: Boolean;
var
  Atom: TAtom;

begin
  Result := _ApplicationHandle <> 0;
  if Result then begin
    Atom := Windows.GlobalAddAtom(PChar(CMDLine));
    Windows.SendMessage(_ApplicationHandle, WM_Call_Command, Atom, ParamCount);
    GlobalDeleteAtom(Atom);
  end;
end;


class function TDBIApplication.CallCommand(var Message: TMessage; CallBack: TDBICommandCallBack): Boolean;
var
  Parameters: String;

begin
  Result := Message.Msg = WM_Call_Command;
  if Result then begin
    Parameters := '';

    if (Message.LParam > 0) then begin
      SetLength(Parameters, MAX_PATH);
      Windows.GlobalGetAtomName(TMessage(Message).WParam, PChar(Parameters), MAX_PATH);
      SetLength(Parameters, StrLen(PChar(Parameters)));
    end;

    CallBack(Parameters);
  end;
end;


procedure TDBIApplication.DoCommand(const Parameters: String);
begin
  if Assigned(FOnCommand) then begin
    FOnCommand(Parameters);
  end
  else begin
    ShowMessage(Parameters);
  end;
end;


class function TDBIApplication.FindApplication(Handle: THandle): Boolean;
var
  WindowName: array[0..127] of Char;

begin
  Windows.GetClassName(Handle, WindowName, SizeOf(WindowName));
  Result := StrIComp(WindowName, PChar(String(Application.ClassName))) = 0;

  if Result then begin
    Windows.GetWindowText(Handle, WindowName, SizeOf(WindowName));
    Result := StrIComp(WindowName, PChar(Application.Title)) = 0;
  end;
end;


class function TDBIApplication.ForceForeGroundWindow(Handle: THandle): Boolean;
var
  ForeGround: THandle;
  ForeGroundThreadID, ThisThreadID: LongWord;

begin
  Foreground := Windows.GetForeGroundWindow;

  Result := ForeGround = Handle;
  if not Result then begin
    ForeGroundThreadID := Windows.GetWindowThreadProcessId(ForeGround, nil);
    ThisThreadID := Windows.GetWindowThreadProcessId(Handle, nil);
    if Windows.AttachThreadInput(ThisThreadID, ForeGroundThreadID, True) then begin
      Windows.BringWindowToTop(Handle);
      Windows.SetForegroundWindow(Handle);
      Windows.AttachThreadInput(ThisThreadID, ForeGroundThreadID, False);
      Result := Windows.GetForeGroundWindow = Handle;
    end;
  end;
end;


class function TDBIApplication.ForceToFront(Form: TCustomForm): Boolean;
begin
  Result := ForceForeGroundWindow(Form.Handle);
end;


class procedure TDBIApplication.HookApplication;
begin
  Application.HookMainWindow(Instance.HookMessage);
end;


function TDBIApplication.HookMessage(var Message: TMessage): Boolean;
begin
  Result := CallCommand(Message, DoCommand);
end;


var
  _ApplicationInstance: TDBIApplication = nil;

class function TDBIApplication.Instance: TDBIApplication;
begin
  if not Assigned(_ApplicationInstance) then begin
    _ApplicationInstance := Self.Create(Application);
  end;
  Result := _ApplicationInstance;
end;


class procedure TDBIApplication.UnhookApplication;
begin
  if (_ApplicationHandle <> 0) then begin
    Application.UnhookMainWindow(Instance.HookMessage);
  end;
end;
{$endif}





{ TDBIFieldMap }

// _____________________________________________________________________________
{**
  Jvr - 13/06/2001 12:07:13.<P>
}
constructor TDBIFieldMap.Create(Source: TDataset; Target: TDataset);
var
  FieldNo: Integer;
  TargetField: TField;

begin
  inherited Create;

  FSourceFields := TList.Create;
  FTargetFields := TList.Create;

  for FieldNo := 0 to Source.FieldCount - 1 do begin
    TargetField := Target.FindField(Source.Fields[FieldNo].FieldName);
    if (TargetField <> nil) then begin
      FSourceFields.Add(Source.Fields[FieldNo]);
      FTargetFields.Add(TargetField);
    end;
  end;  { for }
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 13/06/2001 12:34:13.<P>
}
destructor TDBIFieldMap.Destroy;
begin
  FTargetFields.Free;
  FSourceFields.Free;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 13/06/2001 12:07:46.<P>
}
function TDBIFieldMap.GetFieldCount: Integer;
begin
  Result := FSourceFields.Count;
end;  { GetFieldCount }


// _____________________________________________________________________________
{**
  Jvr - 13/06/2001 11:45:39.<P>
}
function TDBIFieldMap.GetSourceField(Index: Integer): TField;
begin
  Result := TField(FSourceFields[Index]);
end;  { GetSourceField }


// _____________________________________________________________________________
{**
  Jvr - 13/06/2001 12:04:34.<P>
}
function TDBIFieldMap.GetTargetField(Index: Integer): TField;
begin
  Result := TField(FTargetFields[Index]);
end;  { GetTargetField }





{ TDBINullFlags }

// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 16:36:17.<P>
}
procedure TDBINullFlags.Error;
begin
  raise EBitsError.Create('TDBINullFlags: Bits index out of range');
end;  { Error }


procedure TDBINullFlags.Log(const Index: Integer);
begin
  TDBIDebugInfo.LogMsg(dkLogging,
    ' --> TDBINullFlags::Log( IsNullable: %s  -  Size: %d  -  NullIndex[%d]: %d  -  IsNull[%d]: %s )',
    [BoolName[IsNullable], Size, Index, NullIndex[Index], Index, BoolName[IsNull[Index]] ]
    );
end;


// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 16:35:14.<P>
}
function TDBINullFlags.GetBit(Index: Integer): Boolean; assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     TDBINullFlags.Error
        MOV     EAX,[EAX].FBits
        BT      [EAX],Index
        SBB     EAX,EAX
        AND     EAX,1
end;  { GetBit }


// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 18:11:38.<P>
}
function TDBINullFlags.GetIndex(Index: Integer): Integer;
begin
  Result := FNullFlagsIndex[Index];
end;  { GetIndex }


// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 18:14:03.<P>
}
function TDBINullFlags.GetIsNull(Index: Integer): Boolean;
begin
  Result :=
    FIsNullable and
    (FSize > 0) and
    (FNullFlagsIndex[Index] <> -1) and
    GetBit(FNullFlagsIndex[Index]);
end;  { GetIndexValue }


// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 16:35:32.<P>
}
procedure TDBINullFlags.SetBit(Index: Integer; Value: Boolean); assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     TDBINullFlags.Error

@@1:    MOV     EAX,[EAX].FBits
        OR      Value,Value
        JZ      @@2
        BTS     [EAX],Index
        RET

@@2:    BTR     [EAX],Index
        RET
end;  { SetBit }


// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 16:41:27.<P>
}
procedure TDBINullFlags.SetBuffer(const PBuffer: Pointer; const ASize: Integer);
begin
  FBits := PBuffer;
  FSize := ASize * 8 {Bits};
end;  { SetBuffer }


// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 18:07:04.<P>
}
procedure TDBINullFlags.SetIndex(Index: Integer; Value: Integer);
begin
  if FIsNullable then begin
    FNullFlagsIndex[Index] := Value;
  end
  else begin
    FNullFlagsIndex[Index] := -1;
  end;
end;  { SetIndex }


// _____________________________________________________________________________
{**
  Jvr - 10/05/2001 13:26:04.<P>
}
procedure TDBINullFlags.SetIsNull(Index: Integer; Value: Boolean);
begin
  if FIsNullable and (FNullFlagsIndex[Index] <> -1) then begin
    SetBit(FNullFlagsIndex[Index], Value);
  end;
end;  { SetIsNull }


// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 18:43:01.<P>
}
function TDBINullFlags.SetNullIndex(
  Index: Integer;
  IsNullableField: Boolean;
  Value: Integer
  ): Boolean;
begin
  Result := FIsNullable and IsNullableField;

  if Result then begin
    FNullFlagsIndex[Index] := Value;
  end
  else begin
    FNullFlagsIndex[Index] := -1;
  end;
end;  { SetNullIndex }





{ General helper routines }

// _____________________________________________________________________________
{**
  Jvr - 04/10/2002 10:14:14 - Moved from DBIConst<P>
}
procedure Check(Status: DBIResult);
var
  ErrorMessage: String;

begin
  if (Status = DBIERR_NONE) then Exit;

  ErrorMessage := LoadStr(Status);
  if (ErrorMessage = '') then begin
    ErrorMessage := 'An unexpected error occurred ' +
      'in the DBI engine while performing a dataset operation';
  end;

  raise EDBIException.Create(ErrorMessage);
end;  { Check }



// _____________________________________________________________________________
{**
  Jvr - 21/02/2013 14:03:23 - Moved from DBIConst<P>
}
function DBIForceDirectories(Dir: string): Boolean;
{$ifdef DELPHI6}
begin
  Result := ForceDirectories(Dir);
end;
{$else}
var
  E: EInOutError;

  function DBIDirectoryExists(const Directory: String): Boolean;
  var
    Code: longWord;
  begin
    Code := GetFileAttributes(PChar(Directory));
    Result := (longInt(Code) <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
  end;

begin
  Result := True;
  if (Dir = '') then begin
    E := EInOutError.Create(SCannotCreateDir);
    E.ErrorCode := 3;
    raise E;
  end;
  Dir := ExcludeTrailingBackSlash(Dir);

  if (Length(Dir) < 3) or DBIDirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.

  Result := DBIForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 06/02/2003 13:48:53.<P>
}
function DBIModuleName: String;
begin
  if IsLibrary then begin
    SetLength(Result, MAX_PATH + 1);
    GetModuleFileName(HInstance, PChar(Result), Length(Result) - 1);
    SetLength(Result, StrLen(PChar(Result)));
  end
  else begin
    Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  end;
end;  { DBIModuleName }


// _____________________________________________________________________________
{**
  Jvr - 07/02/2003 12:40:17.<P>
}
function DBIModuleDateTime(AModuleName: String = ''): TDateTime;
var
  FHandle: Integer;

begin
  if (AModuleName = '') then begin
    AModuleName := DBIModuleName;
  end;

  FHandle := FileOpen(AModuleName, fmOpenRead or fmShareDenyNone);
  try
    Result := FileDateToDateTime(FileGetDate(FHandle));
  finally
    FileClose(FHandle);
  end;
end;  { DBIModuleDateTime }


// _____________________________________________________________________________
{**
  Jvr - 18/06/2014 14:15:01.<P>
}
function DBIVerifyExtensionRegistered(const Extension: String): Boolean;
var
  Key: HKey;

begin
  Result := (Extension <> '') and
    (ERROR_SUCCESS = RegOpenKeyEx(HKEY_CLASSES_ROOT, PChar(Extension), 0, KEY_QUERY_VALUE, Key));

  if Result then begin
    RegCloseKey(Key);
  end;
end;


type
  TDBITimeStampBytes = array[0..20] of Byte;
  PDBITimeStampBytes = ^TDBITimeStampBytes;

function DBIStrDateStampToDateTime(PDateStamp: PAnsiChar): TDateTime;
var
  Year, Month, Day: Word;
  PDateStampBytes: PDBITimeStampBytes;

begin
  PDateStampBytes := PDBITimeStampBytes(PDateStamp);

  Year :=
    ((PDateStampBytes[0] - $30) * 1000) +
    ((PDateStampBytes[1] - $30) * 100) +
    ((PDateStampBytes[2] - $30) * 10) +
     (PDateStampBytes[3] - $30);

  Month :=
    ((PDateStampBytes[4] - $30) * 10) +
     (PDateStampBytes[5] - $30);

  Day :=
    ((PDateStampBytes[6] - $30) * 10) +
     (PDateStampBytes[7] - $30);

  Result := EncodeDate(Year, Month, Day);
end;


function DBIStrTimeStampToDateTime(PTimeStamp: PAnsiChar; const MilliSeconds: Boolean = True): TDateTime;
var
  Year, Month, Day, Hour, Minute, Sec, MSec: Word;
  PTimeStampBytes: PDBITimeStampBytes;

begin
  PTimeStampBytes := PDBITimeStampBytes(PTimeStamp);

  Year :=
    ((PTimeStampBytes[0] - $30) * 1000) +
    ((PTimeStampBytes[1] - $30) * 100) +
    ((PTimeStampBytes[2] - $30) * 10) +
     (PTimeStampBytes[3] - $30);

  Month :=
    ((PTimeStampBytes[4] - $30) * 10) +
     (PTimeStampBytes[5] - $30);

  Day :=
    ((PTimeStampBytes[6] - $30) * 10) +
     (PTimeStampBytes[7] - $30);

  Hour :=
    ((PTimeStampBytes[9] - $30) * 10) +
    (PTimeStampBytes[10] - $30);

  Minute :=
    ((PTimeStampBytes[12] - $30) * 10) +
     (PTimeStampBytes[13] - $30);

  Sec :=
    ((PTimeStampBytes[15] - $30) * 10) +
     (PTimeStampBytes[16] - $30);


  if Milliseconds then begin
    MSec :=
      ((PTimeStampBytes[17] - $30) * 100) +
      ((PTimeStampBytes[18] - $30) * 10) +
       (PTimeStampBytes[19] - $30);
  end
  else begin
    MSec := 0;
  end;

  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Sec, MSec);
end;


// _____________________________________________________________________________
{**
  Jvr - 09/11/2004 13:18:53 - Initial code.<p>
}
var
  _TempFolder: String = '';

function DBITempFolder: String;
const
  TempDefault = 'C:\Temp\';
  TempFormat = '_yyyymmdd_hhnnss_zzz_';

begin
  if (_TempFolder = '') then begin
    _TempFolder :=
      TempDefault +
      DBIModuleName +
      FormatDateTime(TempFormat, Now) +
      IntToStr(Windows.GetCurrentProcessId) +
      {$ifdef DELPHI6} '_' + TDBIHostInfo.GetLocalHostName + PathDelim {$else} '\' {$endif};

    Assert(DBIForceDirectories(_TempFolder), 'Failed to create TempFolder');
  end;
  Result := _TempFolder;
end;  { DBITempFolder }


{$ifndef DELPHI6}
function FileIsReadOnly(const FileName: string): Boolean;
begin
  Result := (GetFileAttributes(PAnsiChar(FileName)) and FILE_ATTRIBUTE_READONLY) <> 0;
end;
{$endif}


{$ifdef fpc}
// _____________________________________________________________________________
{**
  Jvr - 21/02/2013 14:23:17 - Moved from DBIConst<P>
}
function GetEnvironmentVariable(lpName: PChar; lpBuffer: PChar; nSize: Cardinal): LongWord;
begin
  Result := 0;

  raise Exception.CreateFmt('DBIUtils::GetEnvironmentVariable() Not implemented for FPC', []);
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 22/03/2013 15:34:28 - Origin: Scalabium Software - http://www.scalabium.com <br>
}
function HtmlToText(Html: String; const Options: TDBIHtmlToTextOptions): String;
const
  Quotes: array[Boolean] of String = (#34, '');
  Apostrophes: array[Boolean] of String = (#39, '');

var
  PHtml: PAnsiChar;
  InTag: Boolean;
  Text: AnsiString;

begin
  Html := StringReplace(Html, '<br>', sLineBreak,  [rfReplaceAll, rfIgnoreCase]);
  Html := StringReplace(Html, '</p>', sLineBreak + sLineBreak,  [rfReplaceAll, rfIgnoreCase]);
  Html := StringReplace(Html, '<li>', '* ',  [rfReplaceAll, rfIgnoreCase]);
  Html := StringReplace(Html, '</li>', sLineBreak,  [rfReplaceAll, rfIgnoreCase]);
  Html := StringReplace(Html, '&nbsp;', ' ',  [rfReplaceAll, rfIgnoreCase]);

  PHtml := PAnsiChar(AnsiString(Html));
  Text := '';

  InTag := False;
  repeat
    case PHtml^ of
      '<': InTag := True;
      '>': InTag := False;

      #34:
        if not (htRemoveQuote in Options) then begin
          Text := Text + AnsiChar(PHtml^);
        end;

      #39:
        if not (htRemoveApostrophe in Options) then begin
          Text := Text + AnsiChar(PHtml^);
        end;

      #13, #10:
        if not (htRemoveLineBreak in Options) then begin
          Text := Text + AnsiChar(PHtml^);
        end;

      else
        if not InTag then begin
          if (PHtml^ in [#9, #32]) and ((PHtml+1)^ in [#10, #13, #32, #9, '<']) then
          else
            Text := Text + AnsiChar(PHtml^);
        end;
    end;
    Inc(PHtml);
  until (PHtml^ = #0);

  Result := String(Text);

  { convert system characters }
  Result := StringReplace(Result, '&quot;', Quotes[htRemoveQuote in Options],  [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&apos;', Apostrophes[htRemoveApostrophe in Options], [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&gt;', '>',  [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&lt;', '<',  [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&amp;',  '&',  [rfReplaceAll, rfIgnoreCase]);
end;


// _____________________________________________________________________________
{**
  Jvr - 04/10/2002 10:14:40 - Moved from DBIConst<P>
}
function SwapDWord(Value: LongWord): LongWord;
type
  TSwapRec = packed record
    case Integer of
      0: (Value: LongWord); { 4 Bytes, 32 Bits }
      1: (Low, High: Word); { also: 4 Bytes, 32 Bits }
  end;  { TSwapRec }

var
  SwapBytes: TSwapRec;
  SwapWords: TSwapRec;

begin
  SwapBytes.Value := Value;
  SwapBytes.Low := Swap(SwapBytes.Low);
  SwapBytes.High := Swap(SwapBytes.High);
  SwapWords.High := SwapBytes.Low;
  SwapWords.Low := SwapBytes.High;
  Result := SwapWords.Value;
end;  { SwapDWord }


// _____________________________________________________________________________
{**
  Jvr - 04/10/2002 10:14:52 - Moved from DBIConst<P>
}
type
  PByte = ^Byte;

function SwapDouble(Value: Double): Double;
var
  PByte1: PByte;
  PByte2: PByte;
  SaveByte: Byte;
  Index: Integer;

begin
  PByte1 := @Value;
  PByte2 := @Value;
  Inc(PByte2, 8);

  for Index := 0 to 3 do begin
    SaveByte := PByte1^;
    PByte1^ := PByte2^;
    PByte2^ := SaveByte;
    Inc(PByte1);
    Dec(PByte2);
  end;

  Result := Value;
end;  { SwapDouble }


// _____________________________________________________________________________
{**
  Displays text of the last system error formated (if necessary) with Param.<P>

  @param Param string that represents array for formating
}
function SystemErrorMessageParam(Param: String = ''): String;
var
  Len: Integer;
  Buffer: array[0..255] of Char;
  ArgArray: array[1..1] of PChar;
  ErrorCode: Integer;

begin
  ArgArray[1] := PChar(Param);
  ErrorCode := GetLastError;
  
  Len := Windows.FormatMessage(
    Format_Message_From_System or Format_Message_Argument_Array,
    nil, ErrorCode, 0, Buffer, SizeOf(Buffer), @ArgArray);

{$ifdef UNICODE}
  while (Len > 0) and CharInSet(Buffer[Len - 1], [#0..#32, '.']) do begin
{$else}
  while (Len > 0) and (Buffer[Len - 1] in[#0..#32, '.']) do begin
{$endif}
    Dec(Len);
  end;  { while }

  SetString(Result, Buffer, Len);
end;  { SystemErrorMessageParam }





{ TDBIDebugInfo }

class procedure TDBIDebugInfo.Display(const Msg: String; Args: array of const);
begin
{$ifdef DebugMode}
  ShowMessage(Format(Msg, Args));
{$else}
  Windows.OutputDebugString(PChar(Format(Msg, Args)));
{$endif}
end;


class function TDBIDebugInfo.GetAttributes(const Value: TFieldAttributes): String;
var
  Index: TFieldAttribute;

begin
  Result := '';

  for Index := Low(TFieldAttribute) to High(TFieldAttribute) do begin
    if Index in Value then begin
      Result := Result + ' | ' + TypInfo.GetEnumName(TypeInfo(TFieldAttribute), Ord(Index));
    end;
  end;
end;


class function TDBIDebugInfo.GetDataType(const Value: TFieldType): String;
begin
  Result := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(Value));
end;


class procedure TDBIDebugInfo.LogMsg(const Kind: TDBIDebugKind; const Msg: String; Args: array of const);
begin
  if {%H-}(Kind in DBIDebugKinds) then begin
    Windows.OutputDebugString(PChar(Format(Msg, Args)));
  end;
end;





{$ifndef fpc}
initialization

finalization
  TDBIApplication.UnhookApplication;
{$endif}

end.
