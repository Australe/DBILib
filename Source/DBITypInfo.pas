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
  1.0 | 17/01/2014 12:30:12 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBITypInfo;

{$I DBICompilers.inc}

interface

uses
  Classes, Sysutils, Contnrs, TypInfo, DB, DBIObjectListDatasets;

type
  TDBIParamsAdapter = class(TPersistent)
  private
    FParams: TParams;

  protected
    class procedure AssignDefaultValue(AParam: TParam; ADataType: TFieldType);

  public
    constructor Create(AParams: TParams = nil); virtual;
    destructor Destroy; override;

    procedure AssignFromFields(Fields: TFields; const ItemName: String = '');
    procedure AssignFromProperties(Instance: TObject; const ItemName: String = '');

    function FindParam(const Value: String): TParam;

    function GetByName(
      const AParamName: String;
      const ADataType: TFieldType = ftUnknown;
      const AParamType: TParamType = ptUnknown
      ): TParam; virtual;

    property Params: TParams read FParams;
  end;


type
  TDBIEnumName = class(TPersistent)
  private
    FID: Integer;
    FName: String;

  public
    class function BuildLookupDataset(
      TypeInfo: PTypeInfo;
      const Low: Integer;
      const High: Integer
      ): TObjectListDataset;

    class function New(const ID: Integer; const Name: String): TDBIEnumName;
    class function Add(List: TObjectList; const Name: String): TDBIEnumName;
    class procedure Populate(List: TObjectList; TypeInfo: PTypeInfo; const Low, High: Integer);

  published
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;

  end;



type
  TDBICustomFieldData = class(TPersistent)
  private
    FKind: TFieldKind;
    FTitle: String;
    FName: String;
    FSize: Integer;
    FPrecision: Integer;
    FRequired: Boolean;
    FViewName: String;

  protected
    property Kind: TFieldKind read FKind write FKind;
    property Title: String read FTitle write FTitle;
    property Name: String read FName write FName;
    property Size: Integer read FSize write FSize;
    property Precision: Integer read FPrecision write FPrecision;
    property Required: Boolean read FRequired write FRequired;
    property ViewName: String read FViewName write FViewName;

  public
    class function BuildDataset: TObjectListDataset;

  end;

  TDBIFieldData = class(TDBICustomFieldData)
  protected
    property Kind;
    property Title;
    property Name;
    property Size;
    property Precision;
    property Required;
    property ViewName;

  end;


type
  TDBITypeKinds = set of TTypeKind;

// TTypeKind Aliases
const
{$ifdef fpc}
  tkBoolean = tkBool;
{$else}
  tkBoolean = tkEnumeration;
{$endif}


const
  tkStringTypes = [
      tkString
    , tkLString
    , tkWString
{$ifdef DELPHI2009}
    , tkUString
{$endif}
{$ifdef fpc}
    , tkAString
{$endif}
    ];


type
  TDBIPropType = class
  public
    class function Check(
      Instance: TObject;
      const PropName: String;
      const ValidKinds: TDBITypeKinds
      ): TTypeKind;

    class function GetDataType(AClass: TClass; const PropName: String): TFieldType;

    class function GetTypeData(PropInfo: PPropInfo): PTypeData;

    class function IsPublishedProp(AClass: TClass; const PropName: String): Boolean;

    class function IsTypeKind(
      Instance: TObject;
      const PropName: String;
      ValidKinds: TDBITypeKinds
      ): Boolean;

  end;


type
  TDBIPropertyOption = (
    poPropertyAssigned,
    poPropertyDecapitalise,
    poPropertyReadRequired,
    poPropertyWriteRequired,
    poPropertyStoredRequired,
    poPropertyUnsupportedError
    );
  TDBIPropertyOptions = set of TDBIPropertyOption;

  EPropertyUnsupportedError = class(Exception);

  TDBIProperties = class(TStringList)
  protected
    class function Decapitalise(const Value: String): String;

  public
    class procedure GetEnumNames(PInfo: PTypeInfo; Strings: TStrings);
    class procedure GetMethodParameterInfo(PInfo: PTypeInfo; Strings: TStrings);
    class function GetSetAsString(Value: Cardinal; PInfo: PTypeInfo): String;

    class function GetTypeInfo(PInfo: PTypeInfo; Strings: TStrings): PTypeData;

    class procedure GetProperties(
      AInstance: TObject;
      Strings: TStrings;
      Options: TDBIPropertyOptions
      ); overload; virtual;

    procedure GetProperties(
      AInstance: TObject;
      Options: TDBIPropertyOptions
      ); overload; virtual;

    class function GetPropValue(
      AInstance: TObject;
      const PropName: String
      ): String; overload;

    class function GetPropValue(
      AInstance: TObject;
      PropInfo: PPropInfo
      ): String; overload;

  end;


  procedure DBIGetPropertyList(ClassInfo: PTypeInfo; List: TList);

  function DBIAnsiGetStrProp(Instance: TObject; const PropName: String): AnsiString;
  procedure DBIAnsiSetStrProp(Instance: TObject; const PropName: String; const AData: AnsiString);

  function DBIUnicodeGetStrProp(Instance: TObject; const PropName: String): WideString;
  procedure DBIUnicodeSetStrProp(Instance: TObject; const PropName: String; const WData: WideString);

  function GetFieldTypeName(const DataType: TFieldType): String;


{$ifndef DELPHI6}
  function GetWideStrProp(Instance: TObject; const PropName: string): WideString; overload;
  procedure SetWideStrProp(Instance: TObject; const PropName: string; const Value: WideString); overload;
{$endif}


implementation

uses
  Graphics,
  Controls,
  Forms,
{$ifndef fpc}
  {$ifdef DELPHI6}
  RtlConsts,
  {$endif}
  Consts,
  DBConsts,
{$endif}
  DBIConst,
  DBIUtils,
  DBIIntfConsts,
  DBIInterfaces;


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
    raise EPropertyError.CreateFmt('Property %s does not exist', [PropName]);
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






{ Rtti }

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


function DBIAnsiGetStrProp(Instance: TObject; const PropName: String): AnsiString;
const
  Caller = 'DBIAnsiGetStrProp';

var
  PropKind: TTypeKind;
  PropInstance: TObject;

begin
{$WARNINGS OFF}
  Result := '';

  PropKind := PropType(Instance, PropName);
  case PropKind of
    tkString, tkLString {$ifdef fpc} , tkAString {$endif} : begin
      Result := AnsiString(GetStrProp(Instance, PropName));
    end;

    tkWString {$ifdef DELPHI2009}, tkUString {$endif} : begin
      Result := AnsiString(GetWideStrProp(Instance, PropName));
    end;

    tkChar, tkWChar: begin
      SetLength(Result, 1);
      Result := AnsiChar(GetOrdProp(Instance, PropName));
    end;

    // Property is of type TStrings
    tkClass: begin
      PropInstance := GetObjectProp(Instance, PropName, TStrings);
      if Assigned(PropInstance) then begin
        Result := AnsiString((PropInstance as TStrings).Text);
      end
      else begin
        raise EPropertyError.CreateFmt('Illegal Object Datatype: %s',
          [GetEnumName(TypeInfo(TTypeKind), Ord(PropKind))]
          );
      end;
    end;

  else
    raise EPropertyError.CreateFmt(
      '%s(): "%s" is NOT a supported String Property', [Caller, PropName]
      );
  end;
{$WARNINGS ON}
end;


procedure DBIAnsiSetStrProp(Instance: TObject; const PropName: String; const AData: AnsiString);
const
  Caller = 'DBIAnsiSetStrProp';

var
  PropKind: TTypeKind;
  PropInstance: TObject;

begin
{$WARNINGS OFF}
  PropKind := PropType(Instance, PropName);
  case PropKind of
    tkString, tkLString {$ifdef fpc} , tkAString {$endif} : begin
    {$ifdef Delphi2009}
      SetAnsiStrProp(Instance, PropName, AData);
    {$else}
      SetStrProp(Instance, PropName, AData);
    {$endif}
    end;

    tkWString {$ifdef DELPHI2009} , tkUString {$endif} : begin
      SetWideStrProp(Instance, PropName, WideString(AData));
    end;

    tkChar, tkWChar: begin
      SetOrdProp(Instance, PropName, Ord(AData[1]));
    end;

    // Property is of type TStrings
    tkClass: begin
      PropInstance := GetObjectProp(Instance, PropName, TStrings);
      if Assigned(PropInstance) then begin
        (PropInstance as TStrings).Text := String(AData);
      end
      else begin
        raise EPropertyError.CreateFmt('Illegal Object Datatype: %s',
          [GetEnumName(TypeInfo(TTypeKind), Ord(PropKind))]
          );
      end;
    end;

{$WARNINGS ON}
  else
    raise EPropertyError.CreateFmt(
      '%s(): "%s" is NOT a supported String Property', [Caller, PropName]
      );
  end;
end;


function DBIUnicodeGetStrProp(Instance: TObject; const PropName: String): WideString;
const
  Caller = 'DBIUnicodeGetStrProp';

var
  PropKind: TTypeKind;

begin
{$WARNINGS OFF}
  Result := '';

  PropKind := PropType(Instance, PropName);
  case PropKind of
    tkString, tkLString {$ifdef fpc} , tkAString {$endif} : begin
      Result := WideString(GetStrProp(Instance, PropName));
    end;

    tkWString {$ifdef DELPHI2009}, tkUString {$endif} : begin
      Result := WideString(GetWideStrProp(Instance, PropName));
    end;

    tkChar, tkWChar: begin
      SetLength(Result, 1);
      Result := WideChar(GetOrdProp(Instance, PropName));
    end;

  else
    raise EPropertyError.CreateFmt(
      '%s(): "%s" is NOT a supported String Property', [Caller, PropName]
      );
  end;
{$WARNINGS ON}
end;


procedure DBIUnicodeSetStrProp(Instance: TObject; const PropName: String; const WData: WideString);
const
  Caller = 'DBIUnicodeSetStrProp';

var
  PropKind: TTypeKind;

begin
{$WARNINGS OFF}
  PropKind := PropType(Instance, PropName);
  case PropKind of
    tkString, tkLString {$ifdef fpc} , tkAString {$endif} : begin
    {$ifdef Delphi2009}
      SetAnsiStrProp(Instance, PropName, AnsiString(WData));
    {$else}
      SetStrProp(Instance, PropName, AnsiString(WData));
    {$endif}
    end;

    tkWString {$ifdef DELPHI2009} , tkUString {$endif} : begin
      SetWideStrProp(Instance, PropName, WData);
    end;

    tkChar, tkWChar: begin
      SetOrdProp(Instance, PropName, Ord(WData[1]));
    end;

  else
    raise EPropertyError.CreateFmt(
      '%s(): "%s" is NOT a supported String Property', [Caller, PropName]
      );
  end;
{$WARNINGS ON}
end;




// _____________________________________________________________________________
{**
  Jvr - 21/02/2013 14:30:11 - Moved from DBIConst<P>
}
function GetFieldTypeName(const DataType: TFieldType): String;
begin
  Result := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(DataType));
end;





{ TDBIProperties }

class procedure TDBIProperties.GetEnumNames(PInfo: PTypeInfo; Strings: TStrings);
var
  Index: Integer;
  PType: PTypeData;

begin
  PType := GetTypeData(PInfo);
  for Index := PType^.MinValue to PType^.MaxValue do begin
    Strings.Add(GetEnumName(PInfo, Index));
  end;
end;

{
  ParamList: array[1..ParamCount] of
    record
      Flags: TParamFlags;
      ParamName: ShortString;
      TypeName: ShortString;
    end;
  ResultType: ShortString;   // only if MethodKind = mkFunction
  ResultTypeRef: PPTypeInfo; // only if MethodKind = mkFunction
  CC: TCallConv;
  ParamTypeRefs: array[1..ParamCount] of PPTypeInfo;
  MethSig: PProcedureSignature;
  MethAttrData: TAttrData
}

type
  PParamData = ^TParamData;
  TParamData = record
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
    // beware: string length varies!!!
  end;

type
{$ifdef Delphi2009}
  TMethodParamType = pfVar..pfResult;
{$else}
  TMethodParamType = pfVar..pfOut;
{$endif}

class procedure TDBIProperties.GetMethodParameterInfo(PInfo: PTypeInfo; Strings: TStrings);
const
  ParamTypeNames: array[TMethodParamType, Boolean] of String = (
      ('', 'var ')
    , ('', 'const ')
    , ('', 'array ')
    , ('', ' address')
    , ('', ' reference')
    , ('', ' out')
{$ifdef Delphi2009}
    , ('', ' result')
{$endif}
    );

var
  TypeData: PTypeData;
  PParam: PParamData;
  Param: String;
  ParamIndex: Integer;
  PTypeName: ^ShortString;
  PResultType: ^ShortString;

begin
  if (PInfo^.Kind = tkMethod) then begin
    TypeData := GetTypeData(PInfo);

    PParam := PParamData(@(TypeData^.ParamList));
    for ParamIndex := 1 to TypeData^.ParamCount do begin
      Param := '';
      Param := Param + ParamTypeNames[pfVar, pfVar in PParam^.Flags];
      Param := Param + ParamTypeNames[pfConst, pfConst in PParam^.Flags];
      Param := Param + ParamTypeNames[pfOut, pfOut in PParam^.Flags];
      Param := Param + String(PParam^.ParamName) + ': ';
      Param := Param + ParamTypeNames[pfArray, pfArray in PParam^.Flags];

      // The type name string must be located...
      // moving a pointer past the params and
      // the string (including its size byte)
      PTypeName := Pointer(Integer(PParam) + SizeOf(TParamFlags) + Length(PParam^.ParamName) + 1);
      Param := Param + String(PTypeName^);

      // Add Parameter
      TDBIDebugInfo.LogMsg(dkAudit, Param, []);
      Strings.Add(Param);

      // Move the pointer to the next structure,
      // past the two strings (including size byte)
      PParam := PParamData(Integer(PParam) +
        SizeOf(TParamFlags) +
        Length(PParam^.ParamName) + 1 +
        Length(PTypeName^) + 1);
    end;

    // Show the return type if a function
    if (TypeData^.MethodKind = mkFunction) then begin
      // at the end, instead of a param data,
      // there is the return string
      PResultType := Pointer(PParam);
      Strings.Add('Result=' + String(PResultType));
    end;
  end;
end;


procedure TDBIProperties.GetProperties(
  AInstance: TObject;
  Options: TDBIPropertyOptions
  );
begin
  GetProperties(AInstance, Self, Options);
end;


// _____________________________________________________________________________
{**
  Jvr - 13/09/2011 19:16:00 - Initial code.<br />
}
class procedure TDBIProperties.GetProperties(
  AInstance: TObject;
  Strings: TStrings;
  Options: TDBIPropertyOptions
  );

  function ReadProperty(PropInfo: PPropInfo): String;
  var
    PropRequired: Boolean;

  begin
    Result := '';

    PropRequired :=
      Assigned(PropInfo) and
      (PropInfo^.Name <> 'Name') and
      (Assigned(PropInfo.GetProc) or not (poPropertyReadRequired in Options)) and
      (Assigned(PropInfo.SetProc) or not (poPropertyWriteRequired in Options)) and
      (Assigned(PropInfo.StoredProc) or not (poPropertyStoredRequired in Options));

    if PropRequired then begin
      case PropInfo^.PropType^.Kind of
        tkString, tkWstring, tkLString{$ifdef DELPHIxe2}, tkUString{$endif}: Result := GetStrProp(AInstance, PropInfo);
        tkFloat: Result := FloatToStr(GetFloatProp(AInstance, PropInfo));
        tkInteger: Result := IntToStr(GetOrdProp(AInstance, PropInfo));
        tkInt64: Result := IntToStr(GetInt64Prop(AInstance, PropInfo));
        tkEnumeration: Result := GetEnumProp(AInstance, PropInfo);
        tkChar, tkWChar: Result := Chr(GetOrdProp(AInstance, PropInfo));
        tkSet: Result := GetSetProp(AInstance, PropInfo, False);
      else
        if (poPropertyUnsupportedError in Options) then begin
          raise EPropertyUnsupportedError.CreateFmt(
            'Property "%s" of type "%s" not supported',
            [PropInfo^.Name, GetEnumName(TypeInfo(TTypeKind), Ord(PropInfo^.PropType^.Kind))]
            );
        end;
      end;
      if (Result <> '') or not (poPropertyAssigned in Options) then begin
        if (poPropertyDecapitalise in Options) then begin
          Strings.Add(Decapitalise(String(PropInfo^.Name)) + '=' + Result);
        end
        else begin
          Strings.Add(String(PropInfo^.Name) + '=' + Result);
        end;
      end;
    end;
  end;

var
  PropList: TList;
  PropIndex: Integer;

begin
  PropList := Local(TList.Create).Obj as TList;
  PropList.Count := GetTypeData(AInstance.ClassInfo)^.PropCount;
  if PropList.Count > 0 then begin
    GetPropInfos(AInstance.ClassInfo, PPropList(PropList.List));
    for PropIndex := 0 to PropList.Count - 1 do begin
      ReadProperty(PropList[PropIndex]);
    end;
  end;
end;


class function TDBIProperties.GetPropValue(AInstance: TObject; const PropName: String): String;

  procedure PropertyNotFound(const Name: string);
  begin
    raise EPropertyError.CreateFmt('Property %s does not exist', [Name]);
  end;

  function FindPropInfo(Instance: TObject; const PropName: string): PPropInfo; overload;
  begin
    Result := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
    if Result = nil then begin
      PropertyNotFound(PropName);
    end;
  end;

begin
  Result := GetPropValue(AInstance, FindPropInfo(AInstance, PropName));
end;


class function TDBIProperties.GetPropValue(AInstance: TObject; PropInfo: PPropInfo): String;

  function GetOwnerForm (Comp: TComponent): TComponent;
  begin
    while not (Comp is TForm) and not (Comp is TDataModule) do begin
      Comp := Comp.Owner;
    end;
    Result := Comp;
  end;

var
  PAttribute: Pointer;
  Word: Cardinal;
  PropType: PTypeInfo;

begin
  Result := 'Undefined type';
  PropType := {$ifdef fpc} PropInfo^.PropType; {$else} PropInfo^.PropType^; {$endif}

  case PropType^.Kind of
    tkUnknown:
      Result := 'Unknown type';

    tkChar:
    begin
      Word := GetOrdProp(AInstance, PropInfo);
      if (Word > 32) then begin
        Result := Char(Word);
      end
      else begin
        Result := '#' + IntToStr(Word);
      end;
    end;

    tkWChar:
    begin
      Word := GetOrdProp(AInstance, PropInfo);
      if (Word > 32) then begin
        Result := WideChar(Word);
      end
      else begin
        Result := '#' + IntToStr(Word);
      end;
    end;

    tkInteger:
      if (PropInfo.PropType^.Name = 'TColor') then begin
        Result := ColorToString(GetOrdProp(AInstance, PropInfo));
      end
      else if (PropInfo.PropType^.Name = 'TCursor') then begin
        Result := CursorToString(GetOrdProp(AInstance, PropInfo));
      end
      else begin
        Result := Format('%d', [GetOrdProp(AInstance, PropInfo)]);
      end;

    tkEnumeration:
      Result := GetEnumName(PropType, GetOrdProp(AInstance, PropInfo));

    tkFloat:
      Result := FloatToStr(GetFloatProp(AInstance, PropInfo));

    tkString, tkLString, tkWString {$ifdef DELPHIXE2}, tkUString {$endif} {$ifdef fpc} , tkAString {$endif} :
      Result := GetStrProp(AInstance, PropInfo);

    tkSet:
      Result := GetSetAsString(GetOrdProp(AInstance, PropInfo), PropType);

    tkClass:
    begin
      PAttribute := Pointer(GetOrdProp(AInstance, PropInfo));
      if Assigned(PAttribute) then begin
        Result := Format ('(Object %p)', [PAttribute]);
      end
      else begin
        Result := '(None)';
      end;
    end;

    tkMethod:
    begin
      PAttribute := GetMethodProp(AInstance, PropInfo).Code;
      if Assigned(PAttribute) then begin
        Result := GetOwnerForm(AInstance as TComponent).MethodName(PAttribute);
      end
      else begin
        Result := '';
      end;
    end;

    tkVariant:
      Result := GetVariantProp(AInstance, PropInfo);

    tkArray, tkRecord, tkInterface:
      Result := 'Unsupported type';

  end;
end;


class function TDBIProperties.GetTypeInfo(PInfo: PTypeInfo; Strings: TStrings): PTypeData;
var
  TypeData: PTypeData;
  Values: TStrings;
  Params: TStrings;

begin
  TypeData := GetTypeData(PInfo);

  Strings.Add('Kind=' + GetEnumName(TypeInfo(TTypeKind), Ord(PInfo^.Kind)));
  Strings.Add('Name=' + String(PInfo^.Name));

  if not (PInfo^.Kind in [tkSet, tkClass, tkMethod]) then begin
    Strings.Add('Minimum=' + IntToStr(TypeData^.MinValue));
    Strings.Add('Maximum=' + IntToStr(TypeData^.MaxValue));
  end;

  if (PInfo^.Kind = tkClass) then begin
    Strings.Add('Size=' + IntToStr(TypeData.ClassType.InstanceSize));
    Strings.Add('Unit=' + String(TypeData.UnitName));
  end
  else begin
    Strings.Add('Native=' + GetEnumName(TypeInfo(TOrdType), Ord(TypeData^.OrdType)));
  end;

  if (PInfo^.Kind = tkEnumeration) then begin
    Values := Local(TStringList.Create).Obj as TStrings;
    TDBIProperties.GetEnumNames(PInfo, Values);
    Strings.Add('Values=' + Values.CommaText);
  end;

  // show RTTI info about set base type
  if (PInfo^.Kind = tkSet) then begin
    Strings.Add('');
    Strings.Add('Set base type information...');
    GetTypeInfo({$ifdef fpc} TypeData^.CompType {$else} TypeData^.CompType^ {$endif}, Strings);
  end;

  if (PInfo^.Kind = tkMethod) then begin
    Strings.Add('Method.Type=' + GetEnumName(TypeInfo(TMethodKind), Ord(TypeData^.MethodKind)));
    Strings.Add('Params.Count=' + IntToStr(TypeData^.ParamCount));

    Params := Local(TStringList.Create).Obj as TStrings;
    GetMethodParameterInfo(PInfo, Params);
    Strings.Add('Params.Values=' + Params.CommaText);
  end;

  Result := TypeData;
end;


class function TDBIProperties.Decapitalise(const Value: String): String;
var
  Size: Integer;
  PResult, PValue: PChar;

begin
  Result := Value;
  Size := Length(Value);

  if (Size > 0) then begin
    PResult := PChar(Pointer(Result));
    PValue := PChar(Pointer(Value));

    case PValue^ of
      'A'..'Z': PResult^ := Char(Word(PValue^) or $0020);
    end;
  end;
end;


class function TDBIProperties.GetSetAsString(Value: Cardinal; PInfo: PTypeInfo): String;

  function IsBitOn (Value: Integer; Bit: Byte): Boolean;
  begin
    Result := (Value and (1 shl Bit)) <> 0;
  end;

var
  BaseType: PTypeInfo;
  Index: Integer;
  Found: Boolean;

begin
  Result := '[';
  BaseType := {$ifdef fpc} GetTypeData(PInfo).CompType; {$else} GetTypeData(PInfo).CompType^; {$endif}

  Found := False;
  for Index := GetTypeData(BaseType).MinValue to GetTypeData(BaseType).MaxValue do begin
    // If the bit Index (computed as 1 shl Index) is set,
    // then the corresponding element is in the set
    // (the and is a bitwise and, not a boolean operation)
    if IsBitOn(Value, Index) then begin
      // add the name of the element
      Result := Result + GetEnumName (BaseType, Index) + ', ';
      Found := True;
    end;
  end;

  if Found then begin
    // Remove the final comma and space (2 chars)
    SetLength(Result, Length(Result) - 2);
  end;
  Result := Result + ']';
end;





{ TDBIPropType }

// _____________________________________________________________________________
{**
  Jvr - 09/11/2013 13:07:24<P> - Check the property validity for the specified PropName
}
class function TDBIPropType.Check(
  Instance: TObject;
  const PropName: String;
  const ValidKinds: TDBITypeKinds
  ): TTypeKind;
const
  PropertyUnexpected = 'Unexpected Property TypeKind "%s" for %s::%s';

var
  TypeKindName: String;

begin
  Result := TypInfo.PropType(Instance, PropName);
  if not (Result in ValidKinds) then begin
    TypeKindName := GetEnumName(TypeInfo(TTypeKind), Ord(Result));

    raise EPropertyError.CreateFmt(PropertyUnexpected, [TypeKindName, Instance.ClassName, PropName]);
  end;
end;


class function TDBIPropType.GetDataType(AClass: TClass; const PropName: String): TFieldType;
type
  TDBIFieldOrdTypeMap = array[TOrdType] of TFieldType;

const
  FieldOrdTypeMap: TDBIFieldOrdTypeMap = (
    ftSigned8,                         // otSByte,
    ftUnsigned8,                       // otUByte,
    ftSigned16,                        // otSWord,
    ftUnsigned16,                      // otUWord,
    ftSigned32,                        // otSLong,
    ftUnsigned32                       // otULong
    );

var
  PropInfo: PPropInfo;

  procedure CheckFieldType;
  begin
    if Result = ftUnknown then begin
      raise Exception.CreateFmt('Invalid type for property "%s"', [PropName]);
    end;

    if (Result = ftInteger) then begin

    end;
  end;

begin
  Result := ftUnknown;
  if (AClass = nil) then begin
    CheckFieldType;
    Exit;
  end;

  PropInfo := TypInfo.GetPropInfo(PTypeInfo(AClass.ClassInfo), PropName);
  if (PropInfo = nil) then begin
    CheckFieldType;
    Exit;
  end;

  if (PropInfo^.PropType^.Kind = tkEnumeration) then begin
    if CompareText(String(PropInfo^.PropType^.Name), 'Boolean') = 0 then begin
      Result := ftBoolean;
    end
    else begin
      Result := FieldOrdTypeMap[GetTypeData(PropInfo)^.OrdType];
    end;
  end
  else begin
    Result := FieldKindTypeMap[PropInfo^.PropType^.Kind];
  end;

  CheckFieldType;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:26:48<P>
}
class function TDBIPropType.GetTypeData(PropInfo: PPropInfo): PTypeData;
begin
  Result := TypInfo.GetTypeData(PropInfo^.PropType{$ifndef fpc}^{$endif});
end;


class function TDBIPropType.IsPublishedProp(AClass: TClass; const PropName: String): Boolean;
begin
  Result := (AClass <> nil) and (TypInfo.GetPropInfo(PTypeInfo(AClass.ClassInfo), PropName) <> nil);
end;


class function TDBIPropType.IsTypeKind(
  Instance: TObject;
  const PropName: String;
  ValidKinds: TDBITypeKinds
  ): Boolean;
var
  FieldKind: TTypeKind;
begin
  FieldKind := TypInfo.PropType(Instance, PropName);
  Result :=  FieldKind in ValidKinds;
end;




{ TDBICustomFieldData }

class function TDBICustomFieldData.BuildDataset: TObjectListDataset;
begin
  Result := TObjectListDataset.Create(nil);
  Result.ClassTypeName := Self.ClassName;
  Result.Open;
end;





{ TDBIEnumName }

class function TDBIEnumName.Add(List: TObjectList; const Name: String): TDBIEnumName;
begin
  Result := New(List.Count, Name);

  List.Add(Result);
end;


class function TDBIEnumName.BuildLookupDataset(
  TypeInfo: PTypeInfo;
  const Low: Integer;
  const High: Integer
  ): TObjectListDataset;
begin
  Result := TObjectListDataset.Create(nil);
  Result.ClassTypeName := TDBIEnumName.ClassName;
  TDBIEnumName.Populate(Result.List, TypeInfo, Low, High);
  Result.Open;
end;


class function TDBIEnumName.New(const ID: Integer; const Name: String): TDBIEnumName;
begin
  Result := Self.Create;
  Result.ID := ID;
  Result.Name := Name;
end;


class procedure TDBIEnumName.Populate(
  List: TObjectList;
  TypeInfo: PTypeInfo;
  const Low, High: Integer
  );
var
  ID: Integer;

  function GetName: String;
  var
    Data: AnsiString;
    Index: Integer;
  begin
    Result := '';
    Data := AnsiString(Copy(GetEnumName(TypeInfo, ID), 3, 128));
    for Index := 1 to Length(Data) do begin
      if (Data[Index] in ['A'..'Z']) then begin
        Result := Result + ' ';
      end;
      Result := Result + String(Data[Index]);
    end;
  end;

begin
  List.Clear;
  for ID := Low to High do begin
    List.Add(New(ID, GetName));
  end;
end;





{ TDBIParamsAdapter }

// _____________________________________________________________________________
{**
  Jvr - 19/01/2009 12:27:19 - Initial code.<br>
}
class procedure TDBIParamsAdapter.AssignDefaultValue(AParam: TParam; ADataType: TFieldType);
const
  DefaultStringValue = '';

begin
  AParam.DataType := ADataType;
  case ADataType of
    ftString,
    ftFixedChar,
    ftMemo: AParam.Value := DefaultStringValue;

    ftAutoInc,
    ftBoolean,
    ftSmallint,
    ftInteger,
    ftWord,
    ftFloat,
    ftCurrency,
    ftLargeint: AParam.Value := 0;

    ftDateTime: AParam.Value := DBISignifyNullDateTime;
  else
    DatabaseError(Format(SUnknownFieldType, [AParam.Name]));
  end;
end;


procedure TDBIParamsAdapter.AssignFromFields(Fields: TFields; const ItemName: String = '');
var
  FieldIndex: Integer;
  FieldName: String;
  Param: TParam;

begin
  for FieldIndex := 0 to Fields.Count-1 do begin
    FieldName := ItemName + '.' + Fields[FieldIndex].FieldName;
    Param := Params.FindParam(FieldName);

    if not Assigned(Param) then begin
      Param := Params.Add as TParam;
    end;

    Param.AssignField(Fields[FieldIndex]);
    Param.Name := FieldName;
  end;
end;


procedure TDBIParamsAdapter.AssignFromProperties(Instance: TObject; const ItemName: String);
var
{$ifdef AllowClassTypes}
  PropInstance: TObject;
{$endif}
  PropIndex: integer;
  PropInfo: PPropInfo;
  PropList: TList;
  PropName: String;

begin
  PropList := Local(TList.Create).Obj as TList;

  DBIGetPropertyList(Instance.ClassInfo, PropList);
  for PropIndex := 0 to PropList.Count-1 do begin
    PropInfo := PropList[PropIndex];
    PropName := ItemName + '.' + String(PropInfo^.Name);

    case PropInfo^.PropType^.Kind of
      tkString, tkLString, tkWString {$ifdef DELPHI2009}, tkUString {$endif} {$ifdef fpc}, tkAString {$endif}:
        GetByName(PropName).AsString := TypInfo.GetStrProp(Instance, PropInfo);

      tkFloat:
        GetByName(PropName).AsFloat := TypInfo.GetFloatProp(Instance, PropInfo);

      tkInteger:
        GetByName(PropName).AsInteger := TypInfo.GetOrdProp(Instance, PropInfo);

      tkInt64:
        GetByName(PropName).AsInteger := TypInfo.GetInt64Prop(Instance, PropInfo);

      tkEnumeration:
        GetByName(PropName).AsString := TypInfo.GetEnumProp(Instance, PropInfo);
{$ifdef AllowClassTypes}
      tkClass: begin
        PropInstance := GetObjectProp(Instance, PropInfo, TClass(nil));
      end;
{$endif}
    else
      raise Exception.CreateFmt('Property type for "%s" not supported', [PropName]);
    end;
  end;
end;


type
  TDBIDefaultParams = class(TParams);

constructor TDBIParamsAdapter.Create(AParams: TParams = nil);
begin
  inherited Create;

  if Assigned(AParams) then begin
    FParams := AParams;
  end
  else begin
    FParams := TDBIDefaultParams.Create;
  end;
end;


destructor TDBIParamsAdapter.Destroy;
begin
  if (FParams is TDBIDefaultParams) then begin
  //##JVR
    FParams.Clear;

    FreeAndNil(FParams);
  end;

  inherited Destroy;
end;


function TDBIParamsAdapter.FindParam(const Value: String): TParam;
begin
  Result := Params.FindParam(Value);
end;


function TDBIParamsAdapter.GetByName(
  const AParamName: String;
  const ADataType: TFieldType = ftUnknown;
  const AParamType: TParamType = ptUnknown
  ): TParam;
begin
  Result := Params.FindParam(AParamName);
  if not Assigned(Result) then begin
    Result := Params.Add as TParam;
    Result.Name := AParamName;
  end;

  if (AParamType <> ptUnknown) then begin
    Result.ParamType := AParamType;
  end;

  if (ADataType <> ftUnknown) then begin
    AssignDefaultValue(Result, ADataType);
  end;
end;



initialization
  Classes.RegisterClass(TDBIEnumName);
  Classes.RegisterClass(TDBIFieldData);

end.
