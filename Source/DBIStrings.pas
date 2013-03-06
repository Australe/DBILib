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
  1.0 | 17/01/2013 10:11:01 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : native api code}

unit DBIStrings;

interface

{$I DBICompilers.inc}

{$ifdef fpc}
  {$asmmode intel}
  {$mode delphi}
{$endif}

uses
  Classes, SysUtils, TypInfo;

const
  DBIMaxListSize = MaxInt div 16;

type
  TDBIText = String;
  TDBIString = AnsiString;
  PDBIString = PAnsiString;

  TDBIChar = AnsiChar;
  PDBIChar = PAnsiChar;

  TDBIStrings = class;

{ TGetModuleProc }
{ Used in the TFormDesigner class to allow component/property editors access
  to project specific information }

  TGetModuleProc = procedure(const FileName, UnitName, FormName,
    DesignClass: TDBIString; CoClasses: TDBIStrings) of object;

{ IStringsAdapter interface }
{ Maintains link between TDBIStrings and IStrings implementations }

  IStringsAdapter = interface
    ['{739C2F34-52EC-11D0-9EA6-0020AF3D82DA}']
    procedure ReferenceStrings(S: TDBIStrings);
    procedure ReleaseStrings;
  end;

{ TDBIStrings class }

  TDBIStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator,
    sdLineBreak, sdStrictDelimiter);

  TDBIStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TDBIStrings;
  public
    constructor Create(AStrings: TDBIStrings);
    function GetCurrent: TDBIString;
    function MoveNext: Boolean;
    property Current: TDBIString read GetCurrent;
  end;

  TDBIStrings = class(TPersistent)
  private
    FDefined: TDBIStringsDefined;
    FDelimiter: TDBIChar;
    FLineBreak: TDBIString;
    FQuoteChar: TDBIChar;
    FNameValueSeparator: TDBIChar;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    FAdapter: IStringsAdapter;
    function GetCommaText: TDBIString;
    function GetDelimitedText: TDBIString;
    function GetName(Index: Integer): TDBIString;
    function GetValue(const Name: TDBIString): TDBIString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: TDBIString);
    procedure SetDelimitedText(const Value: TDBIString);
    procedure SeTDBIStringsAdapter(const Value: IStringsAdapter);
    procedure SetValue(const Name, Value: TDBIString);
    procedure WriteData(Writer: TWriter);
    function GetDelimiter: TDBIChar;
    procedure SetDelimiter(const Value: TDBIChar);
    function GetLineBreak: TDBIString;
    procedure SetLineBreak(const Value: TDBIString);
    function GetQuoteChar: TDBIChar;
    procedure SetQuoteChar(const Value: TDBIChar);
    function GetNameValueSeparator: TDBIChar;
    procedure SetNameValueSeparator(const Value: TDBIChar);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    function GetValueFromIndex(Index: Integer): TDBIString;
    procedure SetValueFromIndex(Index: Integer; const Value: TDBIString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: String; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: TDBIString): TDBIString;
    function Get(Index: Integer): TDBIString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: TDBIString; virtual;
    procedure Put(Index: Integer; const S: TDBIString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: TDBIString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: TDBIString): Integer; virtual;
  public
    destructor Destroy; override;
    function Add(const S: TDBIString): Integer; virtual;
    function AddObject(const S: TDBIString; AObject: TObject): Integer; virtual;
    procedure Append(const S: TDBIString);
    procedure AddStrings(Strings: TDBIStrings); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TDBIStrings): Boolean; {$ifdef Delphi2009} reintroduce; {$endif} {$ifdef fpc} reintroduce; {$endif}
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TDBIStringsEnumerator;
    function GetText: PDBIChar; virtual;
    function IndexOf(const S: TDBIString): Integer; virtual;
    function IndexOfName(const Name: TDBIString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: TDBIString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: TDBIString;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: String); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PDBIChar); virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: TDBIString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: TDBIChar read GetDelimiter write SetDelimiter;
    property DelimitedText: TDBIString read GetDelimitedText write SetDelimitedText;
    property LineBreak: TDBIString read GetLineBreak write SetLineBreak;
    property Names[Index: Integer]: TDBIString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: TDBIChar read GetQuoteChar write SetQuoteChar;
    property Values[const Name: TDBIString]: TDBIString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: TDBIString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: TDBIChar read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: TDBIString read Get write Put; default;
    property Text: TDBIString read GetTextStr write SetTextStr;
    property StringsAdapter: IStringsAdapter read FAdapter write SeTDBIStringsAdapter;
  end;

{ TDBIStringList class }

  TDBIStringList = class;

  PStringItem = ^TDBIStringItem;
  TDBIStringItem = record
    FString: TDBIString;
    FObject: TObject;
  end;

  PStringItemList = ^TDBIStringItemList;
  TDBIStringItemList = array[0..DBIMaxListSize] of TDBIStringItem;
  TDBIStringListSortCompare = function(List: TDBIStringList; Index1, Index2: Integer): Integer;

  TDBIStringList = class(TDBIStrings)
  private
    FList: PStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TDBIStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): TDBIString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: TDBIString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: TDBIString): Integer; override;
    procedure InsertItem(Index: Integer; const S: TDBIString; AObject: TObject); virtual;
  public
    destructor Destroy; override;
    function Add(const S: TDBIString): Integer; override;
    function AddObject(const S: TDBIString; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: TDBIString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: TDBIString): Integer; override;
    procedure Insert(Index: Integer; const S: TDBIString); override;
    procedure InsertObject(Index: Integer; const S: TDBIString;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TDBIStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;


{ Ansi String Helpers }
type
{$ifdef UNICODE}
  PDBIDataElement = PByte;
{$else}
  PDBIDataElement = PAnsiChar;
{$endif}

  function DBIAnsiCompareStr(const S1, S2: TDBIString): Integer;
  function DBIAnsiCompareText(const S1, S2: TDBIString): Integer;
  function DBIAnsiPos(const Substr, S: TDBIString): Integer;
  function DBIAnsiQuotedStr(const S: TDBIString; Quote: TDBIChar): TDBIString;

  function DBICharNext(var P: PDBIChar): PDBIChar;
  function DBICompareStr(const S1, S2: TDBIString): Integer; assembler;
  function DBICompareText(const S1, S2: TDBIString): Integer; assembler;
  function DBIStrLen(const Str: PDBIDataElement): Cardinal; assembler;
  function DBITrimRight(const S: TDBIString): TDBIString;

  function DBITextToFloat(Buffer: PDBIDataElement; var Value; ValueType: TFloatValue): Boolean; assembler;
  function DBIFloatToStr(Value: Extended): TDBIString;

  
{ TypInfo Helpers }

{$ifndef DELPHI6}
  function GetWideStrProp(Instance: TObject; const PropName: string): WideString; overload;
  procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString); overload;
{$endif}


{$ifndef DELPHI7}
const
  sLineBreak = {$ifdef LINUX} #10 {$else} #13#10 {$endif};
{$endif}


implementation

uses
{$ifdef DELPHI6}
  RtlConsts,
{$endif}
{$ifndef fpc}
  Consts,
{$endif}
  Windows;


{ TypInfo Helpers }

{$ifndef DELPHI6}

procedure DBIAssignWideStr(var Dest: WideString; const Source: WideString);
begin
  Dest := Source;
end;

function DBIFindPropInfo(Instance: TObject; const PropName: string): PPropInfo;
begin
  Result := GetPropInfo(Instance, PropName);
  if Result = nil then begin
    raise EPropertyError.CreateResFmt(@SUnknownProperty, [PropName]);
  end;
end;


procedure DBIGetWideStrProp(Instance: TObject; PropInfo: PPropInfo; var Value: WideString); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to result string    }

        PUSH    ESI
        PUSH    EDI
        MOV     EDI,EDX

        MOV     EDX,[EDI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     ESI,[EDI].TPropInfo.GetProc
        CMP     [EDI].TPropInfo.GetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

@@isVirtualMethod:
        MOVSX   ESI,SI                          { sign extend slot offset }
        ADD     ESI,[EAX]                       { vmt + slot offset }
        CALL    DWORD PTR [ESI]
        JMP     @@exit

@@isStaticMethod:
        CALL    ESI
        JMP     @@exit

@@isField:
  AND  ESI,$00FFFFFF
  MOV  EDX,[EAX+ESI]
  MOV  EAX,ECX
  CALL  DBIAssignWideStr

@@exit:
        POP     EDI
        POP     ESI
end;


function GetWideStrProp(Instance: TObject; const PropName: string): WideString; overload;
begin
  DBIGetWideStrProp(Instance, DBIFindPropInfo(Instance, PropName), Result);
end;


procedure DBISetWideStrProp(Instance: TObject; PropInfo: PPropInfo; const Value: WideString); assembler;
asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to string value     }

        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EDX

        MOV     EDX,[ESI].TPropInfo.Index       { pass index in EDX }
        CMP     EDX,$80000000
        JNE     @@hasIndex
        MOV     EDX,ECX                         { pass value in EDX }
@@hasIndex:
        MOV     EDI,[ESI].TPropInfo.SetProc
        CMP     [ESI].TPropInfo.SetProc.Byte[3],$FE
        JA      @@isField
        JB      @@isStaticMethod

@@isVirtualMethod:
        MOVSX   EDI,DI
        ADD     EDI,[EAX]
        CALL    DWORD PTR [EDI]
        JMP     @@exit

@@isStaticMethod:
        CALL    EDI
        JMP     @@exit

@@isField:
  AND  EDI,$00FFFFFF
  ADD  EAX,EDI
  MOV  EDX,ECX
  CALL  DBIAssignWideStr

@@exit:
        POP     EDI
        POP     ESI
end;


procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString); overload;
begin
  DBISetWideStrProp(Instance, DBIFindPropInfo(Instance, PropName), Value);
end;

{$endif}





{ Ansi String Helpers }

function DBIAnsiCompareStr(const S1, S2: TDBIString): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, 0, PDBIChar(S1), Length(S1),
    PDBIChar(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF LINUX}
  // glibc 2.1.2 / 2.1.3 implementations of strcoll() and strxfrm()
  // have severe capacity limits.  Comparing two 100k strings may
  // exhaust the stack and kill the process.
  // Fixed in glibc 2.1.91 and later.
  Result := strcoll(PChar(S1), PChar(S2));
{$ENDIF}
end;


function DBIAnsiCompareText(const S1, S2: TDBIString): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PDBIChar(S1),
    Length(S1), PDBIChar(S2), Length(S2)) - 2;
{$ENDIF}
{$IFDEF LINUX}
  Result := WideCompareText(S1, S2);
{$ENDIF}
end;


function DBIAnsiPos(const Substr, S: TDBIString): Integer;
var
  P: PDBIChar;
begin
  Result := 0;
  P := AnsiStrPos(PDBIChar(S), PDBIChar(SubStr));
  if P <> nil then
    Result := Integer(P - PDBIChar(S)) + 1;
end;


function DBIAnsiQuotedStr(const S: TDBIString; Quote: TDBIChar): TDBIString;
var
  P, Src, Dest: PDBIChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := AnsiStrScan(PDBIChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := AnsiStrScan(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := Quote + S + Quote;
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := PDBIChar(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := PDBIChar(S);
  P := AnsiStrScan(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, (P - Src) * SizeOf(Char));
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := AnsiStrScan(Src, Quote);
  until P = nil;
  P := StrEnd(Src);
  Move(Src^, Dest^, (P - Src) * SizeOf(Char));
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;


function DBICharNext(var P: PDBIChar): PDBIChar;
begin
  Result := windows.CharNextA(P);
end;


function DBICompareStr(const S1, S2: TDBIString): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        OR      EAX,EAX
        JE      @@1
        MOV     EAX,[EAX-4]
@@1:    OR      EDX,EDX
        JE      @@2
        MOV     EDX,[EDX-4]
@@2:    MOV     ECX,EAX
        CMP     ECX,EDX
        JBE     @@3
        MOV     ECX,EDX
@@3:    CMP     ECX,ECX
        REPE    CMPSB
        JE      @@4
        MOVZX   EAX,BYTE PTR [ESI-1]
        MOVZX   EDX,BYTE PTR [EDI-1]
@@4:    SUB     EAX,EDX
        POP     EDI
        POP     ESI
end;


(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The implementation of function CompareText is subject to the
 * Mozilla Public License Version 1.1 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fastcode
 *
 * The Initial Developer of the Original Code is
 * Fastcode
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
function DBICompareText(const S1, S2: TDBIString): Integer; assembler;
asm
        TEST   EAX, EAX
        JNZ    @@CheckS2
        TEST   EDX, EDX
        JZ     @@Ret
        MOV    EAX, [EDX-4]
        NEG    EAX
@@Ret:
        RET
@@CheckS2:
        TEST   EDX, EDX
        JNZ    @@Compare
        MOV    EAX, [EAX-4]
        RET
@@Compare:
        PUSH   EBX
        PUSH   EBP
        PUSH   ESI
        MOV    EBP, [EAX-4]     // length(S1)
        MOV    EBX, [EDX-4]     // length(S2)
        SUB    EBP, EBX         // Result if All Compared Characters Match
        SBB    ECX, ECX
        AND    ECX, EBP
        ADD    ECX, EBX         // min(length(S1),length(S2)) = Compare Length
        LEA    ESI, [EAX+ECX]   // Last Compare Position in S1
        ADD    EDX, ECX         // Last Compare Position in S2
        NEG    ECX
        JZ     @@SetResult      // Exit if Smallest Length = 0
@@Loop:                         // Load Next 2 Chars from S1 and S2
                                // May Include Null Terminator}
        MOVZX  EAX, WORD PTR [ESI+ECX]
        MOVZX  EBX, WORD PTR [EDX+ECX]
        CMP    EAX, EBX
        JE     @@Next           // Next 2 Chars Match
        CMP    AL, BL
        JE     @@SecondPair     // First Char Matches
        MOV    AH, 0
        MOV    BH, 0
        CMP    AL, 'a'
        JL     @@UC1
        CMP    AL, 'z'
        JG     @@UC1
        SUB    EAX, 'a'-'A'
@@UC1:
        CMP    BL, 'a'
        JL     @@UC2
        CMP    BL, 'z'
        JG     @@UC2
        SUB    EBX, 'a'-'A'
@@UC2:
        SUB    EAX, EBX         // Compare Both Uppercase Chars
        JNE    @@Done           // Exit with Result in EAX if Not Equal
        MOVZX  EAX, WORD PTR [ESI+ECX] // Reload Same 2 Chars from S1
        MOVZX  EBX, WORD PTR [EDX+ECX] // Reload Same 2 Chars from S2
        CMP    AH, BH
        JE     @@Next           // Second Char Matches
@@SecondPair:
        SHR    EAX, 8
        SHR    EBX, 8
        CMP    AL, 'a'
        JL     @@UC3
        CMP    AL, 'z'
        JG     @@UC3
        SUB    EAX, 'a'-'A'
@@UC3:
        CMP    BL, 'a'
        JL     @@UC4
        CMP    BL, 'z'
        JG     @@UC4
        SUB    EBX, 'a'-'A'
@@UC4:
        SUB    EAX, EBX         // Compare Both Uppercase Chars
        JNE    @@Done           // Exit with Result in EAX if Not Equal
@@Next:
        ADD    ECX, 2
        JL     @@Loop           // Loop until All required Chars Compared
@@SetResult:
        MOV    EAX, EBP         // All Matched, Set Result from Lengths
@@Done:
        POP    ESI
        POP    EBP
        POP    EBX
end;


function DBIStrLen(const Str: PDBIDataElement): Cardinal; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;


function DBITrimRight(const S: TDBIString): TDBIString;
var
  I: Integer;
begin
  Result := S;
  I := Length(Result);
  while (I > 0) and (Result[I] <= ' ') do Dec(I);
  SetLength(Result, I); //##JVR := Copy(S, 1, I);
end;


{$ifdef DBIPackage}
function DBITextToFloat(Buffer: PDBIDataElement; var Value; ValueType: TFloatValue): Boolean;
begin
  Result := SysUtils.TextToFloat(PAnsiChar(Buffer), Value, ValueType);
end;

{$else}

const
// 8087 status word masks
  mIE = $0001;
{$ifdef _UNUSED}
  mDE = $0002;
  mZE = $0004;
{$endif}
  mOE = $0008;
{$ifdef _UNUSED}
  mUE = $0010;
  mPE = $0020;
  mC0 = $0100;
  mC1 = $0200;
  mC2 = $0400;
  mC3 = $4000;
{$endif}

  DCon10: Integer = 10;


{$warnings Off}
function DBITextToFloat(Buffer: PDBIDataElement; var Value; ValueType: TFloatValue): Boolean; assembler;
const
// 8087 control word
// Infinity control  = 1 Affine
// Rounding Control  = 0 Round to nearest or even
// Precision Control = 3 64 bits
// All interrupts masked
  CWNear: Word = $133F;

var
  Temp: Integer;
  CtrlWord: Word;
  DecimalSep: TDBIChar;
  SaveGOT: Integer;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
{$IFDEF PIC}
        PUSH    ECX
        CALL    GetGOT
        POP     EBX
        MOV     SaveGOT,EAX
  {$ifdef UNICODE}
        MOV     ECX,[EAX].OFFSET AnsiChar(FormatSettings.DecimalSeparator)
  {$else}
        MOV     ECX,[EAX].OFFSET DecimalSeparator
  {$endif}
        MOV     CL,[ECX].Byte
        MOV     DecimalSep,CL
{$ELSE}
        MOV     SaveGOT,0
  {$ifdef UNICODE}
        MOV     AL,AnsiChar(FormatSettings.DecimalSeparator)
  {$else}
        MOV     AL,DecimalSeparator
  {$endif}
        MOV     DecimalSep,AL
        MOV     EBX,ECX
{$ENDIF}
        FSTCW   CtrlWord
        FCLEX
{$IFDEF PIC}
        FLDCW   [EAX].CWNear
{$ELSE}
        FLDCW   CWNear
{$ENDIF}
        FLDZ
        CALL    @@SkipBlanks
        MOV     BH, byte ptr [ESI]
        CMP     BH,'+'
        JE      @@1
        CMP     BH,'-'
        JNE     @@2
@@1:    INC     ESI
@@2:    MOV     ECX,ESI
        CALL    @@GetDigitStr
        XOR     EDX,EDX
        MOV     AL,[ESI]
        CMP     AL,DecimalSep
        JNE     @@3
        INC     ESI
        CALL    @@GetDigitStr
        NEG     EDX
@@3:    CMP     ECX,ESI
        JE      @@9
        MOV     AL, byte ptr [ESI]
        AND     AL,0DFH
        CMP     AL,'E'
        JNE     @@4
        INC     ESI
        PUSH    EDX
        CALL    @@GetExponent
        POP     EAX
        ADD     EDX,EAX
@@4:    CALL    @@SkipBlanks
        CMP     BYTE PTR [ESI],0
        JNE     @@9
        MOV     EAX,EDX
        CMP     BL,fvCurrency
        JNE     @@5
        ADD     EAX,4
@@5:    PUSH    EBX
        MOV     EBX,SaveGOT
        CALL    FPower10
        POP     EBX
        CMP     BH,'-'
        JNE     @@6
        FCHS
@@6:    CMP     BL,fvExtended
        JE      @@7
        FISTP   QWORD PTR [EDI]
        JMP     @@8
@@7:    FSTP    TBYTE PTR [EDI]
@@8:    FSTSW   AX
        TEST    AX,mIE+mOE
        JNE     @@10
        MOV     AL,1
        JMP     @@11
@@9:    FSTP    ST(0)
@@10:   XOR     EAX,EAX
@@11:   FCLEX
        FLDCW   CtrlWord
        FWAIT
        JMP     @@Exit

@@SkipBlanks:

@@21:   LODSB
        OR      AL,AL
        JE      @@22
        CMP     AL,' '
        JE      @@21
@@22:   DEC     ESI
        RET

// Process string of digits
// Out EDX = Digit count

@@GetDigitStr:

        XOR     EAX,EAX
        XOR     EDX,EDX
@@31:   LODSB
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@32
{$IFDEF PIC}
        XCHG    SaveGOT,EBX
        FIMUL   [EBX].DCon10
        XCHG    SaveGOT,EBX
{$ELSE}
        FIMUL   DCon10
{$ENDIF}
        MOV     Temp,EAX
        FIADD   Temp
        INC     EDX
        JMP     @@31
@@32:   DEC     ESI
        RET

// Get exponent
// Out EDX = Exponent (-4999..4999)

@@GetExponent:

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     CL, byte ptr [ESI]
        CMP     CL,'+'
        JE      @@41
        CMP     CL,'-'
        JNE     @@42
@@41:   INC     ESI
@@42:   MOV     AL, byte ptr [ESI]
        SUB     AL,'0'+10
        ADD     AL,10
        JNC     @@43
        INC     ESI
        IMUL    EDX,10
        ADD     EDX,EAX
        CMP     EDX,500
        JB      @@42
@@43:   CMP     CL,'-'
        JNE     @@44
        NEG     EDX
@@44:   RET

@@Exit:
        POP     EBX
        POP     ESI
        POP     EDI
end;
{$warnings on}

{$endif}


function DBIFloatToStr(Value: Extended): TDBIString;
var
  Buffer: array[0..63] of TDBIChar;
begin
  SetString(Result, Buffer, FloatToText(Buffer, Value, {$ifndef fpc} fvExtended, {$endif} ffGeneral, 15, 0));
end;





{ TDBIStringsEnumerator }

constructor TDBIStringsEnumerator.Create(AStrings: TDBIStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

function TDBIStringsEnumerator.GetCurrent: TDBIString;
begin
  Result := FStrings[FIndex];
end;

function TDBIStringsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TDBIStrings }

destructor TDBIStrings.Destroy;
begin
  StringsAdapter := nil;
  inherited Destroy;
end;

function TDBIStrings.Add(const S: TDBIString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TDBIStrings.AddObject(const S: TDBIString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TDBIStrings.Append(const S: TDBIString);
begin
  Add(S);
end;

procedure TDBIStrings.AddStrings(Strings: TDBIStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TDBIStrings.Assign(Source: TPersistent);
begin
  if Source is TDBIStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDefined := TDBIStrings(Source).FDefined;
      FNameValueSeparator := TDBIStrings(Source).FNameValueSeparator;
      FQuoteChar := TDBIStrings(Source).FQuoteChar;
      FDelimiter := TDBIStrings(Source).FDelimiter;
      FLineBreak := TDBIStrings(Source).FLineBreak;
      FStrictDelimiter := TDBIStrings(Source).FStrictDelimiter;
      AddStrings(TDBIStrings(Source));
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TDBIStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TDBIStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TDBIStrings then
        Result := not Equals(TDBIStrings(Filer.Ancestor))
    end
    else Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TDBIStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TDBIStrings.Equals(Strings: TDBIStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TDBIStrings.Error(const Msg: String; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TDBIStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

procedure TDBIStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: TDBIString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TDBIStrings.ExtractName(const S: TDBIString): TDBIString;
var
  P: Integer;
begin
  Result := S;
  P := DBIAnsiPos(NameValueSeparator, Result);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TDBIStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

function TDBIStrings.GetCommaText: TDBIString;
var
  LOldDefined: TDBIStringsDefined;
  LOldDelimiter: TDBIChar;
  LOldQuoteChar: TDBIChar;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

function TDBIStrings.GetDelimitedText: TDBIString;
var
  S: TDBIString;
  P: PDBIChar;
  I, Count: Integer;
  LDelimiters: TSysCharSet;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    LDelimiters := [#0, QuoteChar, Delimiter];
    if not StrictDelimiter then
      LDelimiters := LDelimiters + [#1..' '];
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PDBIChar(S);
      while not (P^ in LDelimiters) do
      {$IFDEF MSWINDOWS}
        P := DBICharNext(P); //##JVR CharNext(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      if (P^ <> #0) then S := DBIAnsiQuotedStr(S, QuoteChar);
      Result := Result + S + Delimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TDBIStrings.GetEnumerator: TDBIStringsEnumerator;
begin
  Result := TDBIStringsEnumerator.Create(Self);
end;

function TDBIStrings.GetName(Index: Integer): TDBIString;
begin
  Result := ExtractName(Get(Index));
end;

function TDBIStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TDBIStrings.GetText: PDBIChar;
begin
  Result := StrNew(PDBIChar(GetTextStr));
end;

function TDBIStrings.GetTextStr: TDBIString;
var
  I, L, Size, Count: Integer;
  P: PDBIChar;
  S, LB: TDBIString;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L);
      Inc(P, L);
    end;
  end;
end;

function TDBIStrings.GetValue(const Name: TDBIString): TDBIString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TDBIStrings.IndexOf(const S: TDBIString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TDBIStrings.IndexOfName(const Name: TDBIString): Integer;
var
  P: Integer;
  S: TDBIString;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := DBIAnsiPos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TDBIStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TDBIStrings.InsertObject(Index: Integer; const S: TDBIString;
  AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TDBIStrings.LoadFromFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDBIStrings.LoadFromStream(Stream: TStream);
var
  Size: Integer;
  S: TDBIString;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    SetTextStr(S);
  finally
    EndUpdate;
  end;
end;

procedure TDBIStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: TDBIString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TDBIStrings.Put(Index: Integer; const S: TDBIString);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TDBIStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TDBIStrings.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do Add(TDBIString(Reader.ReadString));
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;

procedure TDBIStrings.SaveToFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDBIStrings.SaveToStream(Stream: TStream);
var
  S: TDBIString;
begin
  S := GetTextStr;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TDBIStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

procedure TDBIStrings.SetCommaText(const Value: TDBIString);
begin
  Delimiter := ',';
  QuoteChar := '"';
  SetDelimitedText(Value);
end;

procedure TDBIStrings.SeTDBIStringsAdapter(const Value: IStringsAdapter);
begin
  if FAdapter <> nil then FAdapter.ReleaseStrings;
  FAdapter := Value;
  if FAdapter <> nil then FAdapter.ReferenceStrings(Self);
end;

procedure TDBIStrings.SetText(Text: PDBIChar);
begin
  SetTextStr(Text);
end;

procedure TDBIStrings.SetTextStr(const Value: TDBIString);
var
  P, Start, LB: PDBIChar;
  S: TDBIString;
  LineBreakLen: Integer;
  StrPosProc: function(S1, S2: PDBIChar): PDBIChar;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if DBICompareStr(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (P^ in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        // Use StrPos to find line breaks. If running on a FarEast
        // locale use AnsiStrPos, which handles MBCS characters
        if not SysLocale.FarEast then
          StrPosProc := @StrPos
        else
          StrPosProc := @AnsiStrPos;
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := StrPosProc(P, PDBIChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;

procedure TDBIStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TDBIStrings.SetValue(const Name, Value: TDBIString);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TDBIStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do Writer.WriteString(String(Get(I)));
  Writer.WriteListEnd;
end;

procedure TDBIStrings.SetDelimitedText(const Value: TDBIString);
var
  P, P1: PDBIChar;
  S: TDBIString;
begin
  BeginUpdate;
  try
    Clear;
    P := PDBIChar(Value);
    if not StrictDelimiter then
      while P^ in [#1..' '] do
      {$IFDEF MSWINDOWS}
        P := DBICharNext(P); //##JVR CharNext(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
    while P^ <> #0 do
    begin
      if P^ = QuoteChar then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not FStrictDelimiter and (P^ > ' ')) or
              (FStrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
        {$IFDEF MSWINDOWS}
          P := DBICharNext(P); //##JVR CharNext(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not FStrictDelimiter then
        while P^ in [#1..' '] do
        {$IFDEF MSWINDOWS}
          P := DBICharNext(P); //##JVR CharNext(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}

      if P^ = Delimiter then
      begin
        P1 := P;
        {$IFDEF MSWINDOWS}
        if DBICharNext(P1)^ = #0 then //##JVR CharNext(P1)^ = #0 then
        {$ELSE}
        Inc(P1);
        if P1^ = #0 then
        {$ENDIF}
          Add('');
        repeat
          {$IFDEF MSWINDOWS}
          P := DBICharNext(P); //##JVR CharNext(P);
          {$ELSE}
          Inc(P);
          {$ENDIF}
        until not (not FStrictDelimiter and (P^ in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TDBIStrings.GetDelimiter: TDBIChar;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TDBIStrings.GetLineBreak: TDBIString;
begin
  if not (sdLineBreak in FDefined) then
    LineBreak := sLineBreak;
  Result := FLineBreak;
end;

function TDBIStrings.GetQuoteChar: TDBIChar;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

function TDBIStrings.GetStrictDelimiter: Boolean;
begin
  if not (sdStrictDelimiter in FDefined) then
    StrictDelimiter := False;
  Result := FStrictDelimiter;
end;

procedure TDBIStrings.SetDelimiter(const Value: TDBIChar);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TDBIStrings.SetLineBreak(const Value: TDBIString);
begin
  if (FLineBreak <> Value) or not (sdLineBreak in FDefined) then
  begin
    Include(FDefined, sdLineBreak);
    FLineBreak := Value;
  end
end;

procedure TDBIStrings.SetQuoteChar(const Value: TDBIChar);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

procedure TDBIStrings.SetStrictDelimiter(const Value: Boolean);
begin
  if (FStrictDelimiter <> Value) or not (sdStrictDelimiter in FDefined) then
  begin
    Include(FDefined, sdStrictDelimiter);
    FStrictDelimiter := Value;
  end
end;

function TDBIStrings.CompareStrings(const S1, S2: TDBIString): Integer;
begin
  Result := DBIAnsiCompareText(S1, S2);
end;

function TDBIStrings.GetNameValueSeparator: TDBIChar;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

procedure TDBIStrings.SetNameValueSeparator(const Value: TDBIChar);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

function TDBIStrings.GetValueFromIndex(Index: Integer): TDBIString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, MaxInt) else
    Result := '';
end;

procedure TDBIStrings.SetValueFromIndex(Index: Integer; const Value: TDBIString);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

{ TDBIStringList }

destructor TDBIStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

function TDBIStringList.Add(const S: TDBIString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TDBIStringList.AddObject(const S: TDBIString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error(@SDuplicateString, 0);
      end;
  InsertItem(Result, S, AObject);
end;

procedure TDBIStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDBIStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TDBIStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TDBIStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TDBIStringItem));
  Changed;
end;

procedure TDBIStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TDBIStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TDBIStringList.Find(const S: TDBIString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList^[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TDBIStringList.Get(Index: Integer): TDBIString;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TDBIStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TDBIStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TDBIStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TDBIStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TDBIStringList.IndexOf(const S: TDBIString): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TDBIStringList.Insert(Index: Integer; const S: TDBIString);
begin
  InsertObject(Index, S, nil);
end;

procedure TDBIStringList.InsertObject(Index: Integer; const S: TDBIString;
  AObject: TObject);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then Error(@SListIndexError, Index);
  InsertItem(Index, S, AObject);
end;

procedure TDBIStringList.InsertItem(Index: Integer; const S: TDBIString; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TDBIStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := AObject;
    FString := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TDBIStringList.Put(Index: Integer; const S: TDBIString);
begin
  if Sorted then Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TDBIStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then Error(@SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;
end;

procedure TDBIStringList.QuickSort(L, R: Integer; SCompare: TDBIStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TDBIStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TDBIStringItem));
  FCapacity := NewCapacity;
end;

procedure TDBIStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TDBIStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompareStrings(List: TDBIStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList^[Index1].FString,
                                List.FList^[Index2].FString);
end;

procedure TDBIStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TDBIStringList.CustomSort(Compare: TDBIStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TDBIStringList.CompareStrings(const S1, S2: TDBIString): Integer;
begin
  if CaseSensitive then
    Result := DBIAnsiCompareStr(S1, S2)
  else
    Result := DBIAnsiCompareText(S1, S2);
end;

procedure TDBIStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;


end.
