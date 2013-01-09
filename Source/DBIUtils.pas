// _____________________________________________________________________________
{**
  <H5>Copyright</H5> 
  Better Innovative Technical Services Pty Ltd<BR>
  Copyright (C) 2001, All rights reserved.<!--

  Project:       DBILib
  Files(s):      DBIUtils.pas
  Classes:       N/A
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 09/02/2001 15:40:38 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

{#omcodecop off : jvr : native api code}

unit DBIUtils;

interface

{$I DBICompilers.inc}

{$ifdef fpc}
  {$asmmode intel}
  {$mode delphi}
{$endif}

uses
  Classes, SysUtils, TypInfo, DB, DBIConst, DBIIntfConsts, DBIInterfaces;

const
  OpenModes: array[Boolean] of Word = (fmOpenReadWrite, fmOpenRead);
  ShareModes: array[Boolean] of Word = (fmShareDenyNone, fmShareExclusive);
  CreateModes: array[Boolean] of Word = ($0000, fmCreate);

type
  TDBIFieldMap = class(TObject) //##JVR TList)
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
  TDBINullFlags = class(TObject)
  private
    FSize: Integer;
    FBits: Pointer;
    FIsNullable: Boolean;
    FNullFlagsIndex: array[0..255] of Integer;

  protected
    procedure Error;

    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;

    procedure SetIndex(Index: Integer; Value: Integer);
    function GetIndex(Index: Integer): Integer;

    procedure SetIsNull(Index: Integer; Value: Boolean);
    function GetIsNull(Index: Integer): Boolean;

  public
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


// Helper routines for Delphi6/Kylix conversion
// These routines will be linked in for Delphi 5 and lower
{$ifndef DELPHI6}
  function FileIsReadOnly(const FileName: string): Boolean;
{$endif}


function SwapDWord(Value: LongWord): LongWord;
function SwapDouble(Value: Double): Double;

procedure Check(Status: DBIResult);
{##JVR
procedure CheckActive(ADatasource: TDatasource); overload;
procedure CheckActive(ADataset: TDataset); overload;
procedure CheckEditMode(ADataset: TDataset);
procedure CheckBrowseMode(ADataset: TDataset);

function InputQuery(
  const ACaption: String;
  const APrompt: String;
  var Value: string;
  const AWidth: Integer = -1;
  const AHeight: Integer = -1
  ): Boolean;
//}
function SystemErrorMessageParam(Param: String = ''): String;
procedure SaveAsPsvStream(Stream: TStream; DataSet: TDataSet);
procedure SaveAsPsvFile(const AFileName: String; ADataSet: TDataSet);

(*##JVR
type
{$ifdef UNICODE}
  PDBIModuleName = PWideChar;
{$else}
  PDBIModuleName = PAnsiChar;
{$endif}

function DBICheckAttributes(
  const Filter: DSAttr;
  const Value: DSAttr;
  const Mask: DSAttr;
  const Condition: Boolean = True
  ): Boolean;
//*)

procedure DBIGetPropertyList(ClassInfo: PTypeInfo; List: TList);
function DBIGetUserName: WideString;
function DBIForceDirectories(Dir: string): Boolean;
function DBILocalHostName: String;
function DBIModuleName: String;
function DBITempFolder: String;

{##JVR
function DBIIsUpdateableProp(Instance: TObject; const PropName: String): Boolean;
function DBIModuleDateTime(AModuleName: String = ''): TDateTime;
function DBINetUserName: String;
//}

implementation

uses
  SysConst, Windows, WinSock, DBIStrings, DBIFileStreams;

const
  UnitName = 'DBIUtils';



// =============================================================================
// Helper routines for Delphi6/Kylix conversion
// These routines will be linked in for Delphi 5 and lower
// =============================================================================

{$ifndef DELPHI6}
function FileIsReadOnly(const FileName: string): Boolean;
begin
  Result := (GetFileAttributes(PAnsiChar(FileName)) and FILE_ATTRIBUTE_READONLY) <> 0;
end;
{$endif}





// =============================================================================
// 'TDBIFieldMap' methods
// =============================================================================

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





// =============================================================================
// 'TDBINullFlags' public methods
// =============================================================================
(*
const
  BitsPerInt = SizeOf(Integer) * 8;

type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;
  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;
*)

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



// =============================================================================
// 'TDBINullFlags' protected methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 09/05/2001 16:36:17.<P>
}
procedure TDBINullFlags.Error;
begin
  raise EBitsError.Create('TDBINullFlags: Bits index out of range');
//##JVR  raise EBitsError.CreateRes(@SBitsIndexError);
end;  { Error }


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
  Jvr - 09/05/2001 18:11:38.<P>
}
function TDBINullFlags.GetIndex(Index: Integer): Integer;
begin
  Result := FNullFlagsIndex[Index];
end;  { GetIndex }


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
  Jvr - 09/05/2001 18:14:03.<P>
}
function TDBINullFlags.GetIsNull(Index: Integer): Boolean;
begin
  Result :=
    FIsNullable and
    (FNullFlagsIndex[Index] <> -1) and
    GetBit(FNullFlagsIndex[Index]);
end;  { GetIndexValue }



// =============================================================================
// General helper routines
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 15:46:30.<P>
}
procedure Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
begin
{$IFDEF DebugExceptions}
  raise EDBIException.CreateFmt(
    UnitName + '::' + Caller + '::' + Reference + #13 + ErrMsg, Args);
{$ELSE}
  raise EDBIException.CreateFmt(ErrMsg, Args);
{$ENDIF DebugExceptions}
end;  { Error }


(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 16/03/2001 13:45:27 - The Value passed in, is Masked against the
                              attributes specified in the Mask parameter and
                              then checked to see if the Masked Value is
                              included in the StatusFilter.<P>
                              In the case of the condition parameter being
                              false we check the Masked value is
                              NOT in the StatusFilter.<P>
}
function DBICheckAttributes(
  const Filter: DSAttr;
  const Value: DSAttr;
  const Mask: DSAttr;
  const Condition: Boolean = True
  ): Boolean;
begin
  if Condition then begin
    Result := ((Value and Mask) = Mask) and ((Filter and Mask) = Mask);
  end
  else begin
    Result := ((Value and Mask) = Mask) and not ((Filter and Mask) = Mask);
  end;
end;
//*)

// _____________________________________________________________________________
{**
  Jvr - 08/05/2002 13:16:58.<P>
}
procedure DBIGetPropertyList(ClassInfo: PTypeInfo; List: TList);
var
  Index, PropCount: Integer;
  PropList: PPropList;

begin
  if not Assigned(ClassInfo) then begin
    raise Exception.Create(
      'No Runtime class information available for this class "' +
      '!'#13 +
      'Make sure your class is derived from TPersistent or that {$M+} is used.'#13 +
      'Watch out for forward declarations!'
    );
  end;


  PropCount := GetTypeData(ClassInfo)^.PropCount;
  if PropCount > 0 then begin
    GetMem(PropList, PropCount * SizeOf(Pointer));
    try
      GetPropInfos(ClassInfo, PropList);
      List.Count := PropCount;
      for Index := 0 to Pred(PropCount) do begin
        List.Items[Index] := PropList[Index];
      end;
    finally
      FreeMem(PropList);
    end;
  end;
end;  { DBIGetPropertyList }


function DBIGetUserName: WideString;
var
  Buffer: array[0..255] of WideChar;
  Size: LongWord;
begin
  Size := SizeOf(Buffer);
  Win32Check(Windows.GetUserNameW(@Buffer[0], Size));
  Result := WideString(Buffer);
end;


(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 04/09/2002 13:52:42.<P>
}
function DBIIsUpdateableProp(Instance: TObject; const PropName: String): Boolean;
var
  PropInfo: PPropInfo;

begin
  PropInfo := GetPropInfo(Instance, PropName);
  Result := (PropInfo <> nil) and (PropInfo^.SetProc <> nil);
end;  { IsUpdateable }



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
  end;  { try..finally }
end;  { ModuleDateTime }


// _____________________________________________________________________________
{**
  Jvr - 01/05/2002 16:53:40.<P>
}
function DBINetUserName: String;
var
  Size: DWORD;
  LogonName: array[0..255] of Char;

begin
    Size := SizeOf(LogonName);
  if Windows.GetUserName(LogonName, Size) then begin
    Result := LogonName;
  end
  else begin
    Result := SUnknownNameResult;
  end;
end;  { DBINetUserName }
//*)


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

(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 09/11/2004 18:01:36 - Initial code.<p>
}
procedure CheckActive(ADataset: TDataset);
begin
  if not Assigned(ADataset) then begin
    raise EDBIException.Create('Dataset is not Assigned');
  end
  else if (ADataset.State = dsInactive) then begin
    raise EDBIException.Create('Dataset is inactive');
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/11/2004 17:51:35 - Initial code.<p>
}
procedure CheckEditMode(ADataset: TDataset);
begin
  CheckActive(ADataset);

  if not (ADataset.State in dsEditModes) then begin
    ADataset.Edit;
  end;
end;  { CheckEditMode }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2004 18:17:35 - Initial code.<p>
}
procedure CheckBrowseMode(ADataset: TDataset);
begin
  CheckActive(ADataset);
 ADataset.CheckBrowseMode;
end;  { CheckEditMode }
//*)

(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 09/11/2004 18:53:28 - Initial code.<p>
}
function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of AnsiChar;
begin
  for I := 0 to 25 do Buffer[I] := AnsiChar(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := AnsiChar(I + Ord('a'));
  windows.GetTextExtentPointA(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;


function InputQuery(
  const ACaption: String;
  const APrompt: String;
  var Value: string;
  const AWidth: Integer;
  const AHeight: Integer
  ): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  PromptTop: Integer;
  Edit: TEdit;
  EditTop: Integer;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      if (AWidth > 0) then begin
        ClientWidth := AWidth;
      end
      else begin
        ClientWidth := MulDiv(180, DialogUnits.X, 4);
      end;

      if (AHeight > 0) then begin
        ClientHeight := AHeight;
        PromptTop := AHeight - (41 + 8);
        EditTop := AHeight - (41 + 8 + 19);
      end
      else begin
        ClientHeight := MulDiv(163, DialogUnits.Y, 8);
        PromptTop := MulDiv(8, DialogUnits.Y, 8);
        EditTop := MulDiv(19, DialogUnits.Y, 8);
      end;

      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
//##JVR        Top := MulDiv(8, DialogUnits.Y, 8);
        Top := PromptTop;
        Caption := APrompt;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
//##JVR        Top := MulDiv(19, DialogUnits.Y, 8);
        Top := EditTop;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;

      if (AHeight > 0) then begin
        ButtonTop := AHeight - 41;
      end
      else begin
        ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      end;

      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;
//*)


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
  Jvr - 23/01/2003 15:22:26.<P>
}
function DBILocalHostName: String;
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
end;  { LocalHostName }




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
end;  { ModuleName }


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
      {$ifdef DELPHI6} '_' + DBILocalHostName + PathDelim {$else} '\' {$endif};

    Assert(DBIForceDirectories(_TempFolder), 'Failed to create TempFolder');
  end;
  Result := _TempFolder;
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


// _____________________________________________________________________________
{**
  Convert a dataset to a PSV style CSV stream.

  Jvr - 20/05/2002 16:45:09.<P>
}
procedure SaveAsPsvStream(Stream: TStream; DataSet: TDataSet);
var
  Index: Integer;

  procedure Write(Str: String);
  begin
    if Str <> '' then begin
      Stream.Write(Str[1], length(Str));
    end;
  end;

begin
  if not Dataset.Active then begin
    Dataset.Open;
  end
  else begin
    Dataset.First;
  end;

  Dataset.DisableControls;
  try
    DataSet.First;
    Write('#');
    for Index := 0 to DataSet.Fields.Count - 1 do begin
      if DataSet.Fields[Index].FieldName <> '_NullFlags' then begin
        if Index <> 0 then begin
          Write('|');
        end;
        Write(DataSet.Fields[Index].FieldName);
      end;
    end;
    Write(sLineBreak);

    while not DataSet.EOF do begin
      for Index := 0 to DataSet.Fields.Count - 1 do begin
        if DataSet.Fields[Index].FieldName <> '_NullFlags' then begin
          if Index <> 0 then begin
            Write('|');
          end;
          Write(DataSet.Fields[Index].AsString);
        end;
      end;
      Write(sLineBreak);
      DataSet.Next;
    end;
  finally
    Dataset.EnableControls;
  end;
end;  { SaveAsPsvStream }


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
procedure SaveAsPsvFile(const AFileName: String; ADataSet: TDataSet);
const
  Caller = 'SaveAsPsvFile';

var
  LocalStream: TStream;

begin
  try
    LocalStream := TDBIFileStream.Create(
      AFileName,
      fmCreate,
      DBIPageBufferSize,
      [{No Options}]
      );
    try
      SaveAsPsvStream(LocalStream, ADataSet);
    finally
      LocalStream.Free;
    end;
  except
    Error(nil, Caller, '685', 'Failed to save dataset as Psv', []);
  end;
end;  { SaveAsPsvFile }


end.
