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
  1.0 | 06/02/2001 16:30:37 | Jvr | Initial Release
  ______________________________________________________________________________
}

unit DBIUnitTests;

{#omcodecop off : jvr : native api code}

interface

{$I DBICompilers.inc}

uses
  Classes, Contnrs, DB, DBIStrings, DBIIntfConsts, DBIObjectListDatasets,
  DBIXbaseDatasets, DBIXbaseConsts,
{$ifndef fpc}
  DBClient, DSIntf,
  {$ifdef omTesting}
  omTestSuites, omTestMastery;
  {$else}
  TestFrameWork;
  {$endif}
{$else}
  fpcunit, testregistry;
{$endif}

{$ifndef omTesting}
type
  TomTestSuite = TTestCase;
  {$ifdef fpc}
  ETestFailed = EAssertionFailedError;
  {$else}
  ETestFailed = ETestFailure;
  {$endif}
{$endif}


type
{$ifndef fpc}
  TDBIClientDataset = class(TClientDataset)
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;

  public
    property DSBase;
    property DSCursor;
  end;
{$endif}

  TDBIObjectListDataset = class(TObjectListDataset)
  public
    property DSBase;
    property DSCursor;
  end;


  TDBIXbaseDataset = class(TXbaseDataset)
  public
    property DSBase;
    property DSCursor;
  end;


type
  TDBIUnitTests = class(TomTestSuite)
  private
    function GetParent: TomTestSuite;

  protected
{$ifndef omTesting}
    function GetTestTempDir: String;

    property TestTempDir: String read GetTestTempDir;
{$endif}

  public
    function DataPath(const FileName: String = ''): String;

    property Parent: TomTestSuite read GetParent;

  end;  { TDBIUnitTests }


const
  // Dataset TFieldType Aliases
  ftSigned16 = ftSmallInt;
  ftSigned32 = ftInteger;
  ftUnsigned16 = ftWord;
  ftAnsiString = ftString;
  ftUnicodeString = ftWideString;

{$ifdef Delphi2009}
  ftSigned8 = ftShortInt;
  ftUnsigned8 = ftByte;
  ftUnsigned32 = ftLongWord;
  ftFloat4 = ftSingle;
  ftFloat10 = ftExtended;
  ftDefaultString = ftWideString;
{$else}
  ftSigned8 = ftSmallInt;
  ftUnsigned8 = ftWord;
  ftUnsigned32 = ftInteger;
  ftFloat4 = ftFloat;
  ftFloat10 = ftFloat;
  ftDefaultString = ftString;
{$endif}


type
  PFieldRecord = ^TFieldRecord;
  TFieldRecord = record
    FieldName: String;
    FieldType: TFieldType;
    FieldSize: Integer;
    Precision: Integer;
    Required: Boolean;
    ReadOnly: Boolean;
  end;

  TFieldRecords = array[0..127] of TFieldRecord;
  PFieldRecords = ^TFieldRecords;
  
type
  TDBIUnitTest = class(TPersistent)
  protected
    class procedure AddFieldDefs(FieldDefs: TFieldDefs; PFieldData: PFieldRecords; const Count: Word);
    class procedure BuildFields(ADataset: TDataset; PFieldData: PFieldRecords; const Count: Word);
    class procedure BuildFieldDefs(ADataset: TDataset; PFieldData: PFieldRecords; const Count: Word);

    class function CreateXbaseDataset: TDBIXbaseDataset;
    class function GetRecordCount: Integer; virtual;
{$ifndef fpc}
    class procedure FieldProps(ADataset: TDBIClientDataset); overload;
{$endif}
    class procedure FieldProps(ADataset: TDBIObjectListDataset); overload;
    class procedure FieldProps(ADataset: TDBIXbaseDataset); overload;

  public
    class procedure ApplyValues(ADataset: TDataset; Index: Integer); virtual; abstract;
    class procedure AssertBlanks(ADataset: TDataset);
    class procedure AssertValues(ADataset: TDataset); virtual;
    class procedure CheckValues(ADataset: TDataset; Index: Integer); virtual; abstract;
    class procedure ClearValues(ADataset: TDataset);
    class procedure CreateFields(ADataset: TDataset); virtual; abstract;
    class procedure CreateFieldDefs(ADataset: TDataset); virtual; abstract;
    class function GetDateTime(Index: Integer): TDateTime;
    class function GetDateTimeFormat: String;
    class procedure OccupyValues(ADataset: TDataset); virtual;
    class procedure RefillValues(ADataset: TDataset); virtual;
//##JVR    class procedure UpdateValues(ADataset: TDataset); virtual; abstract;
    class procedure VerifyFields(ADataset: TDataset); virtual;

    class procedure DeleteTables(const AFileName: String); virtual;
    class procedure FieldValues(ADataset: TDataset);

{$ifndef fpc}
    class procedure CDSCreateTable(AFileName: String); virtual;
{$endif}    
    class procedure ODSCreateTable(AFileName: String); virtual;
    class procedure XDSCreateTable(AFileName: String); virtual;
  end;


type
  TBookRecord = record
    Sequence: Integer;
    Name: TDBIString;
    Author: TDBIString;
    Purchased: TDBIString;
    Price: Double;
    Currency: TDBIString;
    Rating: Integer;
    Approved: Boolean;
    Comments: TDBIString;
    Notes: TDBIString;
    Details: TDBIString;
  end;

  PBookRecords = ^TBookRecords;
  TBookRecords = array[0..4] of TBookRecord;

  TBookFields = array[0..10] of TFieldRecord;
  TBookIndex = array[Low(TBookRecords)..High(TBookRecords)] of Integer;
  TBookFilter = array[0..1] of Integer;

type
  TBookData = class(TDBIUnitTest)
  private
    FSequence: Integer;
    FName: TDBIString;
    FAuthor: TDBIString;
    FPurchased: TDateTime;
    FPrice: Double;
    FCurrency: TDBIString;
    FRating: Integer;
    FApproved: Boolean;
    FComments: TDBIString;
    FNotes: TDBIString;
    FDetails: TDBIString;

  protected
    class function GetRecordCount: Integer; override;

  public
    class procedure AddIndexes(ADataset: TDataset);
    class procedure AddIndexDefs(ADataset: TDataset);

    class procedure ApplyValues(ADataset: TDataset; Index: Integer); override;
    class procedure CheckAscending(ADataset: TDataset; const AKeyName: String);
    class procedure CheckDescending(ADataset: TDataset; const AKeyName: String);
    class procedure CheckValues(ADataset: TDataset; Index: Integer); override;
    class procedure CreateFields(ADataset: TDataset); override;
    class procedure CreateFieldDefs(ADataset: TDataset); override;

    class function GetFields: TBookFields;
    class function GetNameAsc: TBookIndex;
    class function GetNameDesc: TBookIndex;
    class function GetRatingAsc: TBookIndex;
    class function GetRatingDesc: TBookIndex;
    class function GetRatingFilter: TBookFilter;
    class function GetRecords: TBookRecords;
    class function GetUpdateRecords: TBookRecords;

    class procedure ReviseFields(ADataset: TDataset);
    class procedure UpdateValues(ADataset: TDataset);

    class procedure ODSUpdateTable(const AFileName: String);
    class procedure XDSUpdateTable(const AFileName: String);

  published
    property Sequence: Integer read FSequence write FSequence;
    property Name: TDBIString read FName write FName;
    property Author: TDBIString read FAuthor write FAuthor;
    property Purchased: TDateTime read FPurchased write FPurchased;
    property Price: Double read FPrice write FPrice;
    property Currency: TDBIString read FCurrency write FCurrency;
    property Rating: Integer read FRating write FRating;
    property Approved: Boolean read FApproved write FApproved;
    property Comments: TDBIString read FComments write FComments;
    property Notes: TDBIString read FNotes write FNotes;
    property Details: TDBIString read FDetails write FDetails;

  end;  { TBookData }


type
  TBookCategory = class(TPersistent)
  private
    FSequence: Integer;
    FCategoryName: TDBIString;
    FBooks: TObjectList;

  published
    property Sequence: Integer read FSequence write FSequence;
    property CategoryName: TDBIString read FCategoryName write FCategoryName;
    property Books: TObjectList read FBooks write FBooks;

  end;  { TBookCategory }


type
  TGadRecord = record
    Age: Integer;
    Gender: TDBIString;
    YieldRate: Double;
    Value: Integer;
    Status: AnsiString;
  end;

  TGadRecords = array[0..99] of TGadRecord;
  TGadFields = array[0..9] of TFieldRecord;

type
  TGadData = class(TDBIUnitTest)
  private
    FID: Word;
    FGender: TDBIString;
    FYieldRate: Double;
    FAge: Integer;
    FValue: Double;
    FComment: WideString;
    FStatus: AnsiString;
    FDate: TDateTime;
    FTime: TDBIString;
    FCreated: TDateTime;

  protected
    class function GetComment: String;
    class function GetFields: TGadFields;
    class function GetRecordCount: Integer; override;

    procedure SetGender(const Value: TDBIString);

  public
    class procedure ApplyValues(ADataset: TDataset; Index: Integer); override;
    class procedure CheckValues(ADataset: TDataset; Index: Integer); override;
    class procedure CreateFields(ADataset: TDataset); override;
    class procedure CreateFieldDefs(ADataset: TDataset); override;
    class function GetRecords: TGadRecords;
    class procedure UpdateValues(ADataset: TDataset);

    class procedure ODSCreateTable(AFileName: String); override;

  published
    procedure DoValidation;

    property ID: Word read FID write FID;
    property Age: Integer read FAge write FAge;
    property Gender: TDBIString read FGender write SetGender;
    property YieldRate: Double read FYieldRate write FYieldRate;
    property Value: Double read FValue write FValue;
    property Comment: WideString read FComment write FComment;
    property Status: AnsiString read FStatus write FStatus;
    property Date: TDateTime read FDate write FDate;
    property Time: TDBIString read FTime write FTime;
    property Created: TDateTime read FCreated write Fcreated;

  end;  { TGadData }


type
  TNumericRecord = record
    ID: Integer;
    _2Point0: Double;
    _4Point0: Double;
    _4Point1: Double;
    _8Point0: Double;
    _8Point2: Double;
    _8Point3: Double;
  end;

  TNumericRecords = array[0..3] of TNumericRecord;
  TNumericFields = array[0..6] of TFieldRecord;

type
  TNumericData = class(TDBIUnitTest)
  private
    FID: Integer;
    F2Point0: Double;

    F4Point0: Double;
    F4Point1: Double;

    F8Point0: Double;
    F8Point2: Double;
    F8Point3: Double;

  protected
    class function GetFields: TNumericFields;
    class function GetRecordCount: Integer; override;
    class function GetRecords: TNumericRecords;

  public
    class procedure ApplyValues(ADataset: TDataset; Index: Integer); override;
    class procedure CheckValues(ADataset: TDataset; Index: Integer); override;
    class procedure CreateFields(ADataset: TDataset); override;
    class procedure CreateFieldDefs(ADataset: TDataset); override;

  published
    property ID: Integer read FID write FID;

    property _2Point0: Double read F2Point0 write F2Point0;

    property _4Point0: Double read F4Point0 write F4Point0;
    property _4Point1: Double read F4Point1 write F4Point1;

    property _8Point0: Double read F8Point0 write F8Point0;
    property _8Point2: Double read F8Point2 write F8Point2;
    property _8Point3: Double read F8Point3 write F8Point3;

  end;  { TNumericData }


type
  TOrdinalRecord = record
    _Byte: Byte;
    _Word: Word;
    _LongWord: LongWord;
    _Cardinal: Cardinal;
    _ShortInt: ShortInt;
    _SmallInt: SmallInt;
    _LongInt: LongInt;
    _Int64: Int64;
  end;

  TOrdinalRecords = array[0..7] of TOrdinalRecord;
  TOrdinalFields = array[0..7] of TFieldRecord;

type
  TOrdinalData = class(TDBIUnitTest)
  private
    FByte: Byte;
    FWord: Word;
    FLongWord: LongWord;
    FCardinal: Cardinal;
    FShortInt: ShortInt;
    FSmallInt: SmallInt;
    FLongInt: LongInt;
    FInt64: Int64;

  protected
    class function GetFields: TOrdinalFields;
    class function GetRecordCount: Integer; override;
    class function GetRecords: TOrdinalRecords;

  public
    class procedure ApplyValues(ADataset: TDataset; Index: Integer); override;
    class procedure CheckValues(ADataset: TDataset; Index: Integer); override;
    class procedure CreateFields(ADataset: TDataset); override;
    class procedure CreateFieldDefs(ADataset: TDataset); override;

  published
    property _Byte: Byte read FByte write FByte;
    property _Word: Word read FWord write FWord;
    property _LongWord: LongWord read FLongWord write FLongWord;
    property _Cardinal: Cardinal read FCardinal write FCardinal;
    property _ShortInt: ShortInt read FShortInt write FshortInt;
    property _SmallInt: SmallInt read FSmallInt write FSmallInt;
    property _LongInt: LongInt read FLongInt write FLongInt;
    property _Int64: Int64 read FInt64 write FInt64;

  end;  { TOrdinalData }


type
  TFloatRecord = record
    _Single: Single;
    _Double: Double;
    _Extended: Extended;
    _Currency: Currency;
  end;

  TFloatRecords = array[0..3] of TFloatRecord;
  TFloatFields = array[0..3] of TFieldRecord;

type
  TFloatData = class(TDBIUnitTest)
  private
    FSingle: Single;
    FDouble: Double;
    FExtended: Extended;
    FCurrency: Currency;

  protected
    class function GetFields: TFloatFields;
    class function GetRecordCount: Integer; override;
    class function GetRecords: TFloatRecords;

  public
    class procedure ApplyValues(ADataset: TDataset; Index: Integer); override;
    class procedure CheckValues(ADataset: TDataset; Index: Integer); override;
    class procedure CreateFields(ADataset: TDataset); override;
    class procedure CreateFieldDefs(ADataset: TDataset); override;

  published
    property _Single: Single read FSingle write FSingle;
    property _Double: Double read FDouble write FDouble;
    property _Extended: Extended read FExtended write FExtended;
    property _Currency: Currency read FCurrency write FCurrency;

  end;  { TFloatData }


type
  TStringRecord = record
    ID: Integer;
    Environment: WideString;
    Fullname: AnsiString;
    Value: String;
  end;

  TStringRecords = array[0..3] of TStringRecord;
  TStringFields = array[0..6] of TFieldRecord ;

type
  TStringData = class(TDBIUnitTest)
  private
    FID: Integer;
    FEnvironment: WideString;
    FFullName: AnsiString;
    FValue: String;

  protected
    class procedure BuildDataset(ADataset: TDataset);
    class function GetFields: TStringFields;
    class function GetRecordCount: Integer; override;
    class function GetRecords: TStringRecords;

    function GetApplication: String;
    function GetPath: String;
    function GetUserName: WideString;

  public
    class procedure CheckValues(ADataset: TDataset; Index: Integer); override;
    class procedure CreateFields(ADataset: TDataset); override;
    class procedure CreateFieldDefs(ADataset: TDataset); override;
    class procedure ApplyValues(ADataset: TDataset; Index: Integer); override;

    class procedure SetupDataset(ADataset: TDataset);
    class procedure StringFields(ADataset: TDataset);

  published
    property ID: Integer read FID write FID;
    property Environment: WideString read FEnvironment write FEnvironment;
    property FullName: AnsiString read FFullName write FFullName;
    property Value: String read FValue write FValue;

    property Application: String read GetApplication;
    property Path: String read GetPath;
    property UserName: WideString read GetUserName;
  end;  { TStringData }


type
  TAddressRecord = record
    ID: Integer;
    First: String;
    Last: String;
    Address: String;
    City: String;
    Code: Word;
  end;

  TAddressRecords = array[0..3] of TAddressRecord;
  TAddressFields = array[0..5] of TFieldRecord;

type
  TAddressData = class(TPersistent)
  private
    FID: Integer;
    FFirst: String;
    FLast: String;
    FAddress: String;
    FCity: String;
    FCode: word;

  protected
    procedure AssignTo(Dest: TPersistent); override;

    class function GetFields: TAddressFields;
    class function GetRecords: TAddressRecords;

  published
    property ID: Integer read FID write FID default 0;
    property First: String read FFirst write FFirst;
    property Last: String read FLast write FLast;
    property Address: String read FAddress write FAddress;
    property City: String read FCity write FCity;
    property Code: Word read FCode write FCode;

  end;


type
  TOtherData = class(TPersistent)
    // This is to test ADT's with
    // NO published properties
  end;


type
  TEntityRecord = record
    ID: Integer;
    Name: String;
    Business: Boolean;
  end;

  TEntityRecords = array[0..3] of TEntityRecord;
  TEntityFields = array[0..4] of TFieldRecord;

type
  TEntityData = class(TDBIUnitTest)
  private
    FID: Integer;
    FName: String;
    FBusiness: Boolean;
    FAddress: TAddressData;
    FOther: TOtherData;
    FUntyped: TObject;
    FCreated: TDateTime;

  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetAddress: TAddressData;
    function GetOther: TOtherData;
    function GetUntyped: TObject;

    class function GetFields: TEntityFields;
    class function GetRecordCount: Integer; override;
    class function GetRecords: TEntityRecords;

    procedure SetAddress(Value: TAddressData);
    procedure SetOther(Value: TOtherData);
    procedure SetUntyped(Value: TObject);

  public
    class procedure ApplyValues(ADataset: TDataset; Index: Integer); override;
    class procedure CheckValues(ADataset: TDataset; Index: Integer); override;

    class procedure ODSCreateEntity;
    class procedure CreateFields(ADataset: TDataset); override;
    class procedure CreateFieldDefs(ADataset: TDataset); override;

  public
    destructor Destroy; override;

  published
    property ID: Integer read FID write FID default 0;
    property Name: String read FName write FName;
    property Business: Boolean read FBusiness write FBusiness default False;
    property Address: TAddressData read GetAddress write SetAddress;
    property Other: TOtherData read GetOther write setOther;
    property Untyped: TObject read GetUntyped write SetUntyped;
    property Created: TDateTime read FCreated write FCreated;

  end;

{$ifndef fpc}
procedure CompareFieldProps(CDS: TDBIClientDataset; ODS: TDBIObjectListDataset);
{$endif}
procedure Equalz(const Str1: String; const Str2: AnsiString);


implementation

uses
  Windows, SysUtils, Dialogs, Forms, DBIConst, DBIUtils, DBIDataset;


{ Helpers }

function AddField(const FieldName: String; const Datatype: TFieldType; Adataset: TDataset): TField;
begin
  Result := nil;
  case DataType of
    ftBCD: Result := TBCDField.Create(ADataset);
    ftBoolean: Result := TBooleanField.Create(ADataset);
    ftCurrency: Result := TCurrencyField.Create(ADataset);
    ftDate: Result := TDateField.Create(ADataset);
    ftDateTime: Result := TDateTimeField.Create(ADataset);
    ftFloat: Result := TFloatField.Create(ADataset);
    ftInteger: Result := TIntegerField.Create(ADataset);
    ftMemo: Result := TMemoField.Create(ADataset);
    ftString: Result := TStringField.Create(ADataset);
    ftWideString: Result := TWideStringField.Create(ADataset);
    ftWord: Result := TWordField.Create(ADataset);
{$ifdef DELPHI2009}
    ftSingle: Result := TSingleField.Create(ADataset);

    ftExtended:
      if (ADataset is TDBIDataset) then begin
        Result := TDBIExtendedField.Create(ADataset);
      end
      else begin
        Result := TFloatField.Create(ADataset);
      end;
{$endif}
  end;

  if not Assigned(Result) then begin
    raise Exception.CreateFmt(
      'Create Field: "%s", Unsupported DataType "%s"',
      [FieldName, DBIUtils.GetFieldTypeName(DataType)]
      );
  end;

  Result.FieldName := FieldName;
  Result.Dataset := ADataset;
end;


function AddFieldDef(const FieldName: String; const Datatype: TFieldType; ADataset: TDataset): TFieldDef;
begin
  Result := ADataset.FieldDefs.AddFieldDef;
  Result.Name := FieldName;
  Result.DataType := Datatype;
end;



function CheckField(
  PFieldData: PFieldRecord;
  PFieldProps: DBIIntfConsts.pDSFLDDesc
  ): Boolean;
var
  iFldType: Word;

begin
  if (PFieldProps^.iFldType = fldUniCode) then begin
    iFldType := fldWideString;
  end
  else begin
    iFldType := PFieldProps^.iFldType;
  end;

  Result := CompareText(String(PFieldProps^.szName), PFieldData^.FieldName) = 0;

  // Float types
  if (PFieldProps^.iFldType = fldFLOAT) then begin
    Result := Result and (FldSubTypeMap[ftCurrency] = PFieldProps^.iFldSubType);
  end

  // Blob types
  else if (PFieldProps^.iFldType = fldBLOB) then begin
    Result := Result and (FldSubTypeMap[ftMemo] = PFieldProps^.iFldSubType);
  end

  // Otherwise
  else begin
    Result := Result and (DBIIntfConsts.DataTypeMap[iFldType] = PFieldData^.FieldType);
  end;

  if (PFieldData^.FieldType = ftWideString) then begin
    Result := Result and (PFieldProps^.iUnits1 = 2 * PFieldData^.FieldSize);
  end
  else if (PFieldData^.FieldType = ftString) then begin
    Result := Result and (PFieldProps^.iUnits1 = PFieldData^.FieldSize);
  end;

  Assert(Result, PFieldData^.FieldName + ' is not equal to the predefined Field');
end;


{$ifndef fpc}
procedure CompareFieldProps(CDS: TDBIClientDataset; ODS: TDBIObjectListDataset);
var
  CDSProps: DSIntf.DSProps;
  CDSFieldProps: DBClient.TFieldDescList;

  ODSProps: DBIIntfConsts.DSProps;
  ODSFieldProps: DBIIntfConsts.TFieldDescList;

  Index: Integer;
  Fieldname: String;

  procedure Verify(const Condition: Boolean; const AFieldProp, AFieldName: String);
  begin
    Assert(Condition, Format('%s is wrong for field "%s"', [AFieldProp, AFieldName]));
  end;

begin
  Assert(CDS.DSBase.GetProps(CDSProps) = 0);
  Assert(CDSProps.iFields = CDS.Fields.Count);
  SetLength(CDSFieldProps, CDSProps.iFields);
  Assert(CDS.DSBase.GetFieldDescs(DSIntf.PDSFldDesc(CDSFieldProps)) = 0);

  Assert(ODS.DSBase.GetProps(ODSProps) = 0);
  Assert(ODSProps.iFields = ODS.Fields.Count);
  SetLength(ODSFieldProps, ODSProps.iFields);
  Assert(ODS.DSBase.GetFieldDescs(DBIIntfConsts.PDSFldDesc(ODSFieldProps)) = 0);


  for Index := 0 to ODS.FieldCount-1 do begin
    FieldName := ODS.Fields[Index].FieldName;

    Verify(CDSFieldProps[Index].szName = ODSFieldProps[Index].szName, 'szName', FieldName);
    Verify(CDSFieldProps[Index].iFldType = ODSFieldProps[Index].iFldType, 'iFldType', FieldName);
    Verify(CDSFieldProps[Index].iFldSubType = ODSFieldProps[Index].iFldSubType, 'iFldSubType', FieldName);
    Verify(CDSFieldProps[Index].iUnits1 = ODSFieldProps[Index].iUnits1, 'iUnits1', FieldName);
    Verify(CDSFieldProps[Index].iUnits2 = ODSFieldProps[Index].iUnits2, 'iUnits2', FieldName);
    Verify(CDSFieldProps[Index].iFldLen = ODSFieldProps[Index].iFldLen, 'iFldLen', FieldName);
    Verify(CDSFieldProps[Index].iFldOffsInRec = ODSFieldProps[Index].iFldOffsInRec, 'iFldOffsInRec', FieldName);
    Verify(CDSFieldProps[Index].iNullOffsInRec = ODSFieldProps[Index].iNullOffsInRec, 'iNullOffsInRec', FieldName);
    Verify(CDSFieldProps[Index].iFieldID = ODSFieldProps[Index].iFieldID, 'iFieldID', FieldName);
    Verify(CDSFieldProps[Index].iFieldIDParent = ODSFieldProps[Index].iFieldIDParent, 'iFieldIDParent', FieldName);
    Verify(CDSFieldProps[Index].bCalculated = ODSFieldProps[Index].bCalculated, 'bCalculated', FieldName);
//##JVR    Verify(CDSFieldProps[Index].iFldAttr = ODSFieldProps[Index].iFldAttr, 'iFldAttr', FieldName);
    Verify(CDSFieldProps[Index].iOptParameters = ODSFieldProps[Index].iOptParameters, 'iOptParameters', FieldName);
  end;
end;
{$endif}

procedure Equalz(const Str1: String; const Str2: AnsiString);
begin
  Assert(Str1 = String(Str2));
end;





{ TEntityData }

class procedure TEntityData.ApplyValues(ADataset: TDataset; Index: Integer);
var
  Fields: TFields;
  EntityData: TEntityRecords;
{$ifndef fpc}
  AddressData: TAddressRecords;
{$endif}
begin
  EntityData := TEntityData.GetRecords;
  Fields := ADataset.Fields;
  Fields.FieldByName('ID').AsInteger := EntityData[Index].ID;
  Fields.FieldByName('Name').AsString := EntityData[Index].Name;
  Fields.FieldByName('Business').AsBoolean := EntityData[Index].Business;
  Fields.FieldByName('Created').AsDateTime := GetDateTime(Index);

{$ifndef fpc}
  AddressData := TAddressData.GetRecords;
  Fields := (Fields.FieldByName('Address') as TADTField).Fields;
  Fields.FieldByName('ID').AsInteger := AddressData[Index].ID;
  Fields.FieldByName('First').AsString := AddressData[Index].First;
  Fields.FieldByName('Last').AsString := AddressData[Index].Last;
  Fields.FieldByName('Address').AsString := AddressData[Index].Address;
  Fields.FieldByName('City').AsString := AddressData[Index].City;
  Fields.FieldByName('Code').AsInteger := AddressData[Index].Code;
{$endif}  
end;


procedure TEntityData.AssignTo(Dest: TPersistent);
begin
  if Dest is TEntityData then begin
    TEntityData(Dest).FID := FID;
    TEntityData(Dest).FName := FName;
    TEntityData(Dest).FBusiness := FBusiness;
    TEntityData(Dest).FCreated := FCreated;
    TEntityData(Dest).FAddress.Assign(FAddress);
    TEntityData(Dest).FOther.Assign(FOther);
  end
  else begin
    inherited AssignTo(Dest);
  end;
end;


class procedure TEntityData.CheckValues(ADataset: TDataset; Index: Integer);
var
  EntityData: TEntityRecords;
{$ifndef fpc}
  AddressData: TAddressRecords;
  Field: TField;
{$else}
  Today: TDateTime;
{$endif}
  Fields: TFields;

begin
  EntityData := TEntityData.GetRecords;

{$ifndef fpc}
  Field := ADataset.FindField('Address');
  Assert(Field is TADTField);

  Field := ADataset.FindField('Other');
  Assert(Field = nil);

  Field := ADataset.FindField('Untyped');
  Assert(Field = nil);
{$endif}

  Fields := ADataset.Fields;
  Assert(Fields.FieldByName('ID').AsInteger = EntityData[Index].ID);
  Assert(Fields.FieldByName('Name').AsString = EntityData[Index].Name);
  Assert(Fields.FieldByName('Business').AsBoolean = EntityData[Index].Business);

//##DEBUG - Start
{$ifdef fpc}
  Today := Fields.FieldByName('Created').AsDateTime;
  ShowMessageFmt('"%s" = "%s"', [DateTimeToStr(Today), DateTimeToStr(GetDateTime(Index))]);
{$endif}
//##DEBUG - End
Assert(Fields.FieldByName('Created').AsDateTime = GetDateTime(Index));

{$ifndef fpc}
  AddressData := TAddressData.GetRecords;
  Fields := (Fields.FieldByName('Address') as TADTField).Fields;
  Assert(Fields.FieldByName('ID').AsInteger = AddressData[Index].ID);
  Assert(Fields.FieldByName('First').AsString = AddressData[Index].First);
  Assert(Fields.FieldByName('Last').AsString = AddressData[Index].Last);
  Assert(Fields.FieldByName('Address').AsString = AddressData[Index].Address);
  Assert(Fields.FieldByName('City').AsString = AddressData[Index].City);
  Assert(Fields.FieldByName('Code').AsInteger = AddressData[Index].Code);
{$endif}  
end;


class procedure TEntityData.ODSCreateEntity;
var
  ODS: TDBIObjectListDataset;

begin
  // Create a new ObjectlistDataset, add data, and verify
  ODS := TDBIObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := Self.ClassName;
    CreateFieldDefs(ODS);
    ODS.CreateDataset;

    VerifyFields(ODS);
    OccupyValues(ODS);
    AssertValues(ODS);
  finally
    ODS.Free;
  end;


  // Open a new ObjectlistDataset, add data, and verify
  ODS := TDBIObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := Self.ClassName;
    ODS.Open;

    VerifyFields(ODS);
    OccupyValues(ODS);
    AssertValues(ODS);
  finally
    ODS.Free;
  end;
end;


class procedure TEntityData.CreateFields(ADataset: TDataset);
var
  EntityFields: TEntityFields;
begin
  EntityFields := TEntityData.GetFields;
  BuildFields(ADataset, @EntityFields, Length(EntityFields));
end;


class procedure TEntityData.CreateFieldDefs(ADataset: TDataset);
var
{$ifndef fpc}
  AddressFields: TAddressFields;
  ChildDefs: TFieldDefs;
{$endif}
  EntityFields: TEntityFields;

begin
  EntityFields := TEntityData.GetFields;
  BuildFieldDefs(ADataset, @EntityFields, Length(EntityFields));

{$ifndef fpc}
  AddressFields := TAddressData.GetFields;
  ChildDefs := ADataset.FieldDefs.Find('Address').ChildDefs;
  AddFieldDefs(ChildDefs, @AddressFields, Length(AddressFields));
{$endif}
end;


destructor TEntityData.Destroy;
begin
  FUntyped.Free;
  FUntyped := nil;

  FOther.Free;
  FOther := nil;

  FAddress.Free;
  FAddress := nil;

  inherited Destroy;
end;


function TEntityData.GetAddress: TAddressData;
begin
  if not Assigned(FAddress) then begin
    FAddress := TAddressData.Create;
  end;
  Result := FAddress;
end;


class function TEntityData.GetFields: TEntityFields;
const
  CData: TEntityFields = (
    (FieldName: 'ID';       FieldType: ftInteger;  FieldSize:  0; Precision: 0; Required: True;  ReadOnly: False; ),
    (FieldName: 'Name';     FieldType: ftString;   FieldSize: 20; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Business'; FieldType: ftBoolean;  FieldSize:  0; Precision: 0; Required: True;  ReadOnly: False; ),
    (FieldName: 'Address';  FieldType: ftADT;      FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Created';  FieldType: ftDateTime; FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; )
    );
begin
  Result := CData;
end;


class function TEntityData.GetRecordCount: Integer;
begin
  Result := Length(TEntityData.GetRecords);
end;


class function TEntityData.GetRecords: TEntityRecords;
const
  CData: TEntityRecords = (
    ( ID: 1; Name: 'John';    Business: False; ),
    ( ID: 2; Name: 'Yolanda'; Business: False; ),
    ( ID: 3; Name: 'Andrew';  Business: False; ),
    ( ID: 4; Name: 'Sophie';  Business: True; )
    );
begin
  Result := CData;
end;

function TEntityData.GetOther: TOtherData;
begin
  if not Assigned(FOther) then begin
    FOther := TOtherData.Create;
  end;
  Result := FOther;
end;


function TEntityData.GetUntyped: TObject;
begin
  if not Assigned(FUntyped) then begin
    FUntyped := TObject.Create;
  end;
  Result := FUntyped;
end;


procedure TEntityData.SetAddress(Value: TAddressData);
begin
  if Assigned(Value) then begin
    Value.AssignTo(GetAddress);
  end;
end;


procedure TEntityData.SetOther(Value: TOtherData);
begin
  if Assigned(Value) then begin
    Value.AssignTo(GetOther);
  end;
end;


procedure TEntityData.SetUntyped(Value: TObject);
begin
  raise Exception.Create('Unable to set the Untyped field to a TObject');
end;





{ TAddressData }

procedure TAddressData.AssignTo(Dest: TPersistent);
begin
  if Dest is TAddressData then begin
    TAddressData(Dest).FID := FID;
    TAddressData(Dest).FFirst := FFirst;
    TAddressData(Dest).FLast := FLast;
    TAddressData(Dest).FAddress := FAddress;
    TAddressData(Dest).FCity := FCity;
    TAddressData(Dest).FCode := FCode;
  end
  else begin
    inherited AssignTo(Dest)
  end;
end;


class function TAddressData.GetFields: TAddressFields;
const
  CData: TAddressFields = (
    (FieldName: 'ID';      FieldType: ftInteger; FieldSize:  0; Precision: 0; Required: True;  ReadOnly: False; ),
    (FieldName: 'First';   FieldType: ftString;  FieldSize: 20; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Last';    FieldType: ftString;  FieldSize: 25; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Address'; FieldType: ftString;  FieldSize: 30; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'City';    FieldType: ftString;  FieldSize: 25; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Code';    FieldType: ftWord;    FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; )
    );
begin
  Result := CData;
end;


class function TAddressData.GetRecords: TAddressRecords;
const
  CData: TAddressRecords = (
    ( ID: 1; First: 'John'; Last: 'Smith'; Address: '2 Green Hill Way'; City: 'Mango Orchard'; Code: 1357; ),
    ( ID: 2; First: 'Yolanda'; Last: 'Smart'; Address: '8 Wiilow Lane'; City: 'Cherry Hill'; Code: 8642; ),
    ( ID: 3; First: 'Andrew'; Last: 'Jones'; Address: '4 Alley Street '; City: 'Orange Trees'; Code: 1919; ),
    ( ID: 4; First: 'Sophie'; Last: 'Brown'; Address: '3 Terry Court '; City: 'Strawberry Fields'; Code: 5678; )
    );
begin
  Result := CData;
end;





{ TStringDataList }

type
  TStringDataList = class(TStringList)
  public
{$ifndef fpc}
    procedure AddProps(F: DSIntf.DSFLDDesc); overload;
{$endif}
    procedure AddProps(F: DBIIntfConsts.DSFLDDesc); overload;
    procedure AddProps(F: TField); overload;
  end;

{$ifndef fpc}
procedure TStringDataList.AddProps(F: DSIntf.DSFLDDesc);
const
  Fmt = 'Name: %-20.20s, Type: %2.2d, SubType: %2.2d, Units1: %4.4d, Units2: %4.4d, FldLen: 6.6, iFldAttr: %2.2d';
begin
  Add(
    Format(Fmt, [F.szName, F.iFldType, F.ifldSubType, F.iUnits1, F.iUnits2, F.iFldLen, F.iFldAttr])
    );
end;
{$endif}

procedure TStringDataList.AddProps(F: DBIIntfConsts.DSFLDDesc);
const
  Fmt = 'Name: %-20.20s, Type: %2.2d, SubType: %2.2d, Units1: %4.4d, Units2: %4.4d, FldLen: %6.6d, iFldAttr: %2.2d';
begin
  Add(
    Format(Fmt, [F.szName, F.iFldType, F.ifldSubType, F.iUnits1, F.iUnits2, F.iFldLen, F.iFldAttr])
    );
end;

procedure TStringDataList.AddProps(F: TField);
const
  Fmt = 'Name: %-20.20s, Type: %-20.20s, Size: %4.4d';
begin
  Add(
    Format(Fmt, [F.FieldName, GetFieldTypeName(F.DataType), F.Size])
    );
end;





{ TStringValuesList }

type
  TFieldValuesList = class(TStringList)
  public
    procedure AddField(F: TField);
  end;

procedure TFieldValuesList.AddField(F: TField);
const
  Fmt = '%s[%d]: %s';
begin
  Add(
    Format(Fmt, [F.FieldName, F.Size, F.AsString])
    );
end;





{ TStringData }

class procedure TStringData.ApplyValues(ADataset: TDataset; Index: Integer);
var
  StringData: TStringRecords;
begin
  StringData := TStringData.GetRecords;
  ADataset.FieldByName('ID').AsInteger := StringData[Index].ID;
  ADataset.FieldByName('Environment').AsString := String(StringData[Index].Environment);
  ADataset.FieldByName('Fullname').AsString := String(StringData[Index].Fullname);
  ADataset.FieldByName('Value').AsString := String(StringData[Index].Value);
end;


class procedure TStringData.CheckValues(ADataset: TDataset; Index: Integer);
var
  StringData: TStringRecords;
begin
  StringData := TStringData.GetRecords;
  Assert(ADataset.FieldByName('ID').AsInteger = StringData[Index].ID);
  Assert(ADataset.FieldByName('Environment').AsString = StringData[Index].Environment);
  Equalz(ADataset.FieldByName('Fullname').AsString , StringData[Index].Fullname);
  Assert(ADataset.FieldByName('Value').AsString = StringData[Index].Value);
end;


class procedure TStringData.BuildDataset(ADataset: TDataset);
begin
  if (ADataset is TDBIDataset) then begin
    (ADataset as TDBIDataset).CreateDataset;
{$ifndef fpc}
  end
  else if (ADataset is TClientDataset) then begin
    (ADataset as TClientDataset).CreateDataset;
{$endif}
  end;
end;


class procedure TStringData.CreateFields(ADataset: TDataset);
var
  StringFields: TStringFields;
begin
  StringFields := TStringData.GetFields;
  BuildFields(ADataset, @StringFields, Length(StringFields));
end;

class procedure TStringData.CreateFieldDefs(ADataset: TDataset);
var
  ODS: TObjectListDataset;
  Index: Integer;
  Delta: Integer;
  Field: TFieldDef;
  StringFields: TStringFields;
begin
  StringFields := TStringData.GetFields;
  Delta := 1;

  if (ADataset is TObjectListDataset) then begin
    ODS := ADataset as TObjectListDataset;
    ODS.ClassTypeName := Self.ClassName;
    ODS.StringFieldSize := 32;
    ODS.Options := ODS.Options - [osErrorOnReadOnlyProperty];
{$ifndef Delphi6}
  end
  else begin
    Delta := 2;
{$endif}
  end;

  ADataset.FieldDefs.Clear;
  ADataset.Fields.Clear;

  for Index := Low(StringFields) to High(StringFields) do begin
    Field := AddFieldDef(
      StringFields[Index].FieldName,
      StringFields[Index].FieldType,
      ADataset
      );
    Field.Size := StringFields[Index].FieldSize * Delta;

    if StringFields[Index].ReadOnly then begin
      Field.Attributes := Field.Attributes + [DB.faReadOnly];
    end
    else begin
      Field.Attributes := Field.Attributes - [DB.faReadOnly];
    end;
  end;
end;


function TStringData.GetApplication: String;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;


function TStringData.GetUserName: WideString;
begin
  Result := DBIUtils.DBIGetUserName;
end;


function TStringData.GetPath: String;
begin
  Result := ExtractFileDir(ParamStr(0));
end;


class function TStringData.GetFields: TStringFields;
const
  CData: TStringFields = (
    (FieldName: 'ID';          FieldType: ftInteger;       FieldSize:  0; Precision: 0; Required: True;  ReadOnly: False; ),
    (FieldName: 'Environment'; FieldType: ftUnicodeString; FieldSize: 20; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'FullName';    FieldType: ftUnicodeString; FieldSize: 40; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Value';       FieldType: ftAnsiString;    FieldSize: 60; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Application'; FieldType: ftDefaultString; FieldSize: 32; Precision: 0; Required: False; ReadOnly: True;  ),
    (FieldName: 'Path';        FieldType: ftDefaultString; FieldSize: 64; Precision: 0; Required: False; ReadOnly: True;  ),
    (FieldName: 'UserName';    FieldType: ftUnicodeString; FieldSize: 32; Precision: 0; Required: False; ReadOnly: True;  )
    );
begin
  Result := CData;
end;


class function TStringData.GetRecords: TStringRecords;
const
  CData: TStringRecords = (
    ( ID: 101; Environment: 'Android';    Fullname: 'Samuel Vamder Reest';    Value: 'Star Wars Trilogy'; ),
    ( ID: 102; Environment: 'Linux';      Fullname: 'John Vander Reest';      Value: '2001, A Space Odyssey'; ),
    ( ID: 103; Environment: 'Windows';    Fullname: 'Charlotte Vander Reest'; Value: 'The Adventures of Tin Tin'; ),
    ( ID: 104; Environment: 'Firefox OS'; Fullname: 'Melinda Vander Reest';   Value: 'Lord of the Rings'; )
    );
begin
  Result := CData;
end;


class function TStringData.GetRecordCount: Integer;
begin
  Result := Length(TStringData.GetRecords);
end;


class procedure TStringData.SetupDataset(ADataset: TDataset);
var
{$ifndef fpc}
  CDS: TClientDataset;
{$endif}
  ODS: TObjectListDataset;

begin
  if (ADataset is TObjectListDataset) then begin
    ODS := ADataset as TObjectListDataset;
    ODS.ClassTypeName := Self.ClassName;
    ODS.StringFieldSize := 32;
    ODS.Options := ODS.Options - [osErrorOnReadOnlyProperty];
  end;

{$ifndef fpc}
  if (ADataset is TClientDataset) then begin
    CDS := ADataset as TClientDataset;
    CDS.LogChanges := False;
  end;
{$endif}  
end;


class procedure TStringData.StringFields(ADataset: TDataset);
var
  Index: Integer;

begin
  ADataset.Close;

  ADataset.FieldDefs.Clear;
  ADataset.Fields.Clear;

  CreateFields(ADataset);
  BuildDataset(ADataset);
  ADataset.Close;

  for Index := 0 to ADataset.Fields.Count-1 do begin
    if ADataset.Fields[Index] is TStringField then begin
      ADataset.FieldDefs[Index].Size := 0;
//      ADataset.Fields[Index].Size := 0;
    end;
  end;

  BuildDataset(ADataset);
  VerifyFields(ADataset);
  AssertValues(ADataset);
end;





{ TFloatData }

class procedure TFloatData.ApplyValues(ADataset: TDataset; Index: Integer);
var
  FloatData: TFloatRecords;
begin
  FloatData := TFloatData.GetRecords;
  ADataset.FieldByName('_Single').AsFloat := FloatData[Index]._Single;
  ADataset.FieldByName('_Double').AsFloat := FloatData[Index]._Double;
  ADataset.FieldByName('_Extended').AsFloat := FloatData[Index]._Extended;
  ADataset.FieldByName('_Currency').AsCurrency := FloatData[Index]._Currency;
end;


class procedure TFloatData.CheckValues(ADataset: TDataset; Index: Integer);
  function FloatText(F: Extended): String;
  begin
    Result := Format('"%30.16s"', [Format('%18.12f', [F])]);
  end;

  procedure AssertExtended(F1, F2: Extended);
  var
    Str1, Str2: String;
  begin
    Str1 := FloatText(F1);
    Str2 := FloatText(F2);

    if (Str1 <> Str2) then begin
      Assert(Str1 = Str2, Format('Record[%d]: "%s" <> "%s"', [ADataset.RecNo, Str1, Str2]));
    end;
  end;

var
  FloatData: TFloatRecords;
begin
  FloatData := TFloatData.GetRecords;
  Assert(ADataset.FieldByName('_Single').AsFloat = FloatData[Index]._Single);
  Assert(ADataset.FieldByName('_Double').AsFloat = FloatData[Index]._Double);
  AssertExtended(ADataset.FieldByName('_Extended').AsFloat , FloatData[Index]._Extended);
  Assert(ADataset.FieldByName('_Currency').AsCurrency = FloatData[Index]._Currency);
end;


class procedure TFloatData.CreateFields(ADataset: TDataset);
var
  FloatFields: TFloatFields;
begin
  FloatFields := TFloatData.GetFields;
  BuildFields(ADataset, @FloatFields, Length(FloatFields));
end;


class procedure TFloatData.CreateFieldDefs(ADataset: TDataset);
var
  FloatFields: TFloatFields;
begin
  FloatFields := TFloatData.GetFields;
  BuildFieldDefs(ADataset, @FloatFields, Length(FloatFields));
end;


class function TFloatData.GetFields: TFloatFields;
const
  CData: TFloatFields = (
    (FieldName: '_Single';   FieldType: ftFloat4;   FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_Double';   FieldType: ftFloat;    FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_Extended'; FieldType: ftFloat10;  FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_Currency'; FieldType: ftCurrency; FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; )
  );
begin
  Result := CData;
end;


class function TFloatData.GetRecordCount: Integer;
begin
  Result := Length(TFloatData.GetRecords);
end;


class function TFloatData.GetRecords: TFloatRecords;
const
  CData: TFloatRecords = (
    ( _Single: 1234.56789; _Double: 12345.6789; _Extended: 0;          _Currency: 0.0001; ),
    ( _Single: 1234.56789; _Double: 12345.6789; _Extended: 1.33333;    _Currency: 987.654; ),
    ( _Single: 1234.56789; _Double: 12345.6789; _Extended: -1.3333;    _Currency: 12345.6789; ),
    ( _Single: 1234.56789; _Double: 12345.6789; _Extended: 12345.6789; _Currency: 9999999999.9999; )
    );
begin
  Result := CData;
end;





{ TOrdinalData }

class procedure TOrdinalData.ApplyValues(ADataset: TDataset; Index: Integer);
var
  Int54Field: TLargeIntField;
  OrdinalData: TOrdinalRecords;
begin
  OrdinalData := TOrdinalData.GetRecords;
  ADataset.FieldByName('_Byte').AsInteger := OrdinalData[Index]._Byte;
  ADataset.FieldByName('_Word').AsInteger := OrdinalData[Index]._Word;
  ADataset.FieldByName('_LongWord').AsInteger := LongInt(OrdinalData[Index]._LongWord);
  ADataset.FieldByName('_Cardinal').AsInteger := LongInt(OrdinalData[Index]._Cardinal);

  ADataset.FieldByName('_ShortInt').AsInteger := OrdinalData[Index]._ShortInt;
  ADataset.FieldByName('_SmallInt').AsInteger := OrdinalData[Index]._SmallInt;
  ADataset.FieldByName('_LongInt').AsInteger := OrdinalData[Index]._LongInt;

  Int54Field := ADataset.FieldByName('_Int64') as TLargeIntField;
  Int54Field.AsLargeInt := OrdinalData[Index]._Int64;
end;


class procedure TOrdinalData.CheckValues(ADataset: TDataset; Index: Integer);
var
  Int54Field: TLargeIntField;
  _Integer: LongInt;
  OrdinalData: TOrdinalRecords;
begin
  OrdinalData := TOrdinalData.GetRecords;
  Assert(not ADataset.FieldByName('_Word').IsNull);
  _Integer := ADataset.FieldByName('_Word').AsInteger;
  Assert(Word(_Integer) = OrdinalData[Index]._Word);

  Assert(not ADataset.FieldByName('_LongWord').IsNull);
  _Integer := ADataset.FieldByName('_LongWord').AsInteger;
  Assert(LongWord(_Integer) = OrdinalData[Index]._LongWord);

  Assert(not ADataset.FieldByName('_Cardinal').IsNull);
  _Integer := ADataset.FieldByName('_Cardinal').AsInteger;
  Assert(Cardinal(_Integer) = OrdinalData[Index]._Cardinal);


  Assert(not ADataset.FieldByName('_SmallInt').IsNull);
  Assert(ADataset.FieldByName('_SmallInt').AsInteger = OrdinalData[Index]._SmallInt);

  Assert(not ADataset.FieldByName('_LongInt').IsNull);
  Assert(ADataset.FieldByName('_LongInt').AsInteger = OrdinalData[Index]._LongInt);

  Assert(not ADataset.FieldByName('_Int64').IsNull);
  Int54Field := ADataset.FieldByName('_Int64') as TLargeIntField;
  Assert(Int54Field.AsLargeInt = OrdinalData[Index]._Int64);
end;


class procedure TOrdinalData.CreateFields(ADataset: TDataset);
var
  OrdinalFields: TOrdinalFields;
begin
  OrdinalFields := TOrdinalData.GetFields;
  BuildFields(ADataset, @OrdinalFields, Length(OrdinalFields));
end;


class procedure TOrdinalData.CreateFieldDefs(ADataset: TDataset);
var
  OrdinalFields: TOrdinalFields;
begin
  OrdinalFields := TOrdinalData.GetFields;
  BuildFieldDefs(ADataset, @OrdinalFields, Length(OrdinalFields));
end;


class function TOrdinalData.GetFields: TOrdinalFields;
const
  CData: TOrdinalFields = (
    (FieldName: '_Byte';      FieldType: ftUnsigned8;  FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_Word';      FieldType: ftUnsigned16; FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_LongWord';  FieldType: ftUnsigned32; FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_Cardinal';  FieldType: ftUnsigned32; FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_ShortInt';  FieldType: ftSigned8;    FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_SmallInt';  FieldType: ftSigned16;   FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_LongInt';   FieldType: ftSigned32;   FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: '_Int64';     FieldType: ftLargeInt;   FieldSize: 0; Precision: 0; Required: False; ReadOnly: False; )
    );
begin
  Result := CData;
end;


class function TOrdinalData.GetRecords: TOrdinalRecords;
const
  CData: TOrdinalRecords = (
    ( _Byte:   0; _Word: $0000; _LongWord: $00000000; _Cardinal: $FFFFFFFF; _ShortInt:  127; _SmallInt:  32767; _LongInt:  2147483647; _Int64: $7FFFFFFFFFFFFFFF; ),
    ( _Byte:  15; _Word: $000F; _LongWord: $000000FF; _Cardinal: $01000000; _ShortInt:   63; _SmallInt:  16383; _LongInt:  1073741823; _Int64:  $888888888888888; ),
    ( _Byte:  31; _Word: $0010; _LongWord: $00000100; _Cardinal: $00FFFFFF; _ShortInt:   31; _SmallInt:   8191; _LongInt:   536870911; _Int64:   $91A2B3C4D5E6F8; ),
    ( _Byte:  63; _Word: $00FF; _LongWord: $0000FFFF; _Cardinal: $00010000; _ShortInt:    0; _SmallInt:      0; _LongInt:           0; _Int64:    $9B5837385BA10; ),
    ( _Byte: 127; _Word: $0100; _LongWord: $00010000; _Cardinal: $0000FFFF; _ShortInt:   -1; _SmallInt:     -1; _LongInt:          -1; _Int64:     $A5B36E19FB56; ),
    ( _Byte: 160; _Word: $0FFF; _LongWord: $00FFFFFF; _Cardinal: $00000100; _ShortInt:  -32; _SmallInt:  -8192; _LongInt:  -536870911; _Int64:      $B0BF645FFB0; ),
    ( _Byte: 223; _Word: $1000; _LongWord: $01000000; _Cardinal: $000000FF; _ShortInt:  -64; _SmallInt: -16384; _LongInt: -1073741823; _Int64:       $BC87E28883; ),
    ( _Byte: 255; _Word: $FFFF; _LongWord: $FFFFFFFF; _Cardinal: $00000000; _ShortInt: -128; _SmallInt: -32768; _LongInt: -2147483647; _Int64:                 0; )
    );
begin
  Result := CData;
end;


class function TOrdinalData.GetRecordCount: Integer;
begin
  Result := Length(TOrdinalData.GetRecords);
end;





{ TNumericData}

class procedure TNumericData.ApplyValues(ADataset: TDataset; Index: Integer);
var
  NumericData: TNumericRecords;
begin
  NumericData := TNumericData.GetRecords;
  ADataset.FieldByName('ID').AsInteger := NumericData[Index].ID;

  ADataset.FieldByName('_2Point0').AsFloat := NumericData[Index]._2Point0;

  ADataset.FieldByName('_4Point0').AsFloat := NumericData[Index]._4Point0;
  ADataset.FieldByName('_4Point1').AsFloat := NumericData[Index]._4Point1;

  ADataset.FieldByName('_8Point0').AsFloat := NumericData[Index]._8Point0;
  ADataset.FieldByName('_8Point2').AsFloat := NumericData[Index]._8Point2;
  ADataset.FieldByName('_8Point3').AsFloat := NumericData[Index]._8Point3;
end;


class procedure TNumericData.CheckValues(ADataset: TDataset; Index: Integer);
var
  NumericData: TNumericRecords;
begin
  NumericData := TNumericData.GetRecords;
  Assert(ADataset.FieldByName('ID').AsInteger = NumericData[Index].ID);

  Assert(ADataset.FieldByName('_2Point0').AsFloat = NumericData[Index]._2Point0);

  Assert(ADataset.FieldByName('_4Point0').AsFloat = NumericData[Index]._4Point0);
  Assert(ADataset.FieldByName('_4Point1').AsFloat = NumericData[Index]._4Point1);

  Assert(ADataset.FieldByName('_8Point0').AsFloat = NumericData[Index]._8Point0);
  Assert(ADataset.FieldByName('_8Point2').AsFloat = NumericData[Index]._8Point2);
  Assert(ADataset.FieldByName('_8Point3').AsFloat = NumericData[Index]._8Point3);
end;


class procedure TNumericData.CreateFields(ADataset: TDataset);
var
  NumericFields: TNumericFields;
begin
  NumericFields := TNumericData.GetFields;
  BuildFields(ADataset, @NumericFields, Length(NumericFields));
end;


class procedure TNumericData.CreateFieldDefs(ADataset: TDataset);
var
  NumericFields: TNumericFields;
begin
  NumericFields := TNumericData.GetFields;
  BuildFieldDefs(ADataset, @NumericFields, Length(NumericFields));
end;


class function TNumericData.GetFields: TNumericFields;
const
  CData: TNumericFields = (
    (FieldName: 'ID';       FieldType: ftWord; FieldSize: 0; Precision: 2; Required: True;  ReadOnly: False; ),
    (FieldName: '_2Point0'; FieldType: ftBCD;  FieldSize: 0; Precision: 2; Required: False; ReadOnly: False; ),
    (FieldName: '_4Point0'; FieldType: ftBCD;  FieldSize: 0; Precision: 4; Required: False; ReadOnly: False; ),
    (FieldName: '_4Point1'; FieldType: ftBCD;  FieldSize: 1; Precision: 4; Required: False; ReadOnly: False; ),
    (FieldName: '_8Point0'; FieldType: ftBCD;  FieldSize: 0; Precision: 8; Required: False; ReadOnly: False; ),
    (FieldName: '_8Point2'; FieldType: ftBCD;  FieldSize: 2; Precision: 8; Required: False; ReadOnly: False; ),
    (FieldName: '_8Point3'; FieldType: ftBCD;  FieldSize: 3; Precision: 8; Required: False; ReadOnly: False; )
  );
begin
  Result := CData;
end;


class function TNumericData.GetRecordCount: Integer;
begin
  Result := Length(TNumericData.GetRecords);
end;


class function TNumericData.GetRecords: TNumericRecords;
const
  CData: TNumericRecords = (
    ( ID: 42; _2Point0: 24; _4Point0: 2424; _4Point1: 24.2; _8Point0: 1024; _8Point2: 1024.24; _8Point3: 1024.102; ),
    ( ID:  1; _2Point0: 48; _4Point0: 4848; _4Point1: 48.4; _8Point0: 2048; _8Point2: 2048.48; _8Point3: 2048.204; ),
    ( ID:  2; _2Point0: 72; _4Point0: 7272; _4Point1: 72.7; _8Point0: 4096; _8Point2: 4096.96; _8Point3: 4096.409; ),
    ( ID:  3; _2Point0: 96; _4Point0: 9666; _4Point1: 96.9; _8Point0: 8192; _8Point2: 8192.92; _8Point3: 8192.819; )
    );
begin
  Result := CData;
end;





{ TGadData }

class procedure TGadData.ApplyValues(ADataset: TDataset; Index: Integer);
var
  Today: TDateTime;
  GadData: TGadRecords;

begin
  Sleep(5);
  Today := Now;
  GadData := TGadData.GetRecords;
  
  ADataset.FieldByName('ID').AsInteger := Index;
  ADataset.FieldByName('Age').AsInteger := GadData[Index].Age;
  ADataset.FieldByName('Gender').AsString := String(GadData[Index].Gender);
  ADataset.FieldByName('YieldRate').AsFloat := GadData[Index].YieldRate;
  ADataset.FieldByName('Value').AsInteger := GadData[Index].Value;
{$ifdef DELPHI6}
  ADataset.FieldByName('Comment').AsString := TGadData.GetComment;
{$endif}
  ADataset.FieldByName('Status').AsString := String(GadData[Index].Status);
  ADataset.FieldByName('Created').AsDateTime := Today;
  ADataset.FieldByName('Date').AsDateTime := Trunc(Today);

  // We post the record here to force the Created datetime value
  // to be stored prior to proceeding.
  ADataset.Post;
  ADataset.Edit;

  Today := ADataset.FieldByName('Created').AsDateTime;
  ADataset.FieldByName('Time').AsString := FormatDateTime('hh:nn:ss.zzz', Today);
end;


class procedure TGadData.CheckValues(ADataset: TDataset; Index: Integer);
var
  GadData: TGadRecords;

begin
  GadData := TGadData.GetRecords;
  Assert(ADataset.FieldByName('Age').AsInteger = GadData[Index].Age);
  Equalz(ADataset.FieldByName('Gender').ASString , GadData[Index].Gender);
  Assert(ADataset.FieldByName('YieldRate').AsFloat = GadData[Index].YieldRate);
  Assert(ADataset.FieldByName('Value').AsInteger = GadData[Index].Value);
{$ifdef DELPHI6}
  Assert(ADataset.FieldByName('Comment').AsString = TGadData.GetComment);
{$endif}
  Equalz(ADataset.FieldByName('Status').AsString , GadData[Index].Status);
end;


class procedure TGadData.CreateFieldDefs(ADataset: TDataset);
var
  GadFields: TGadFields;
begin
  GadFields := TGadData.GetFields;
  BuildFieldDefs(ADataset, @GadFields, Length(GadFields));
end;


class procedure TGadData.CreateFields(ADataset: TDataset);
var
  GadFields: TGadFields;
begin
  GadFields := TGadData.GetFields;
  BuildFields(ADataset, @GadFields, Length(GadFields));
end;


procedure TGadData.DoValidation;
begin
  DatabaseError('Validation failed - no reason!');
end;


class function TGadData.GetComment: String;
begin
  Result := 'John_Vander_Reest';
end;


class function TGadData.GetFields: TGadFields;
const
  CData: TGadFields = (
    (FieldName: 'ID';         FieldType: ftWord;       FieldSize:  0; Precision: 0; Required: True;  ReadOnly: False; ),
    (FieldName: 'Age';        FieldType: ftInteger;    FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Gender';     FieldType: ftString;     FieldSize:  1; Precision: 0; Required: True;  ReadOnly: False; ),
    (FieldName: 'YieldRate';  FieldType: ftFloat;      FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Value';      FieldType: ftFloat;      FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Comment';    FieldType: ftWideString; FieldSize: 20; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Status';     FieldType: ftString;     FieldSize:  6; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Date';       FieldType: ftDate;       FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Time';       FieldType: ftString;     FieldSize: 12; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Created';    FieldType: ftDateTime;   FieldSize:  0; Precision: 0; Required: False; ReadOnly: False; )
    );
begin
  Result := CData;
end;


class function TGadData.GetRecordCount: Integer;
begin
  Result := Length(TGadData.GetRecords);
end;


class function TGadData.GetRecords: TGadRecords;
const
  CData: TGadRecords = (
    ( Age: 26; Gender: 'M'; YieldRate: 3; Value: 37; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3; Value: 36; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 3.25; Value: 39; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3.25; Value: 38; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 3.5; Value: 41; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3.5; Value: 40; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 3.75; Value: 43; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3.75; Value: 42; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4; Value: 45; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4; Value: 44; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4.25; Value: 47; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4.25; Value: 46; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4.5; Value: 49; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4.5; Value: 48; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4.75; Value: 51; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4.75; Value: 50; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5; Value: 53; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5; Value: 52; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5.25; Value: 55; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5.25; Value: 54; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5.5; Value: 57; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5.5; Value: 56; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5.75; Value: 59; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5.75; Value: 58; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6; Value: 62; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6; Value: 61; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6.25; Value: 64; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6.25; Value: 63; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6.5; Value: 66; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6.5; Value: 65; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6.75; Value: 68; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6.75; Value: 67; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7; Value: 70; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7; Value: 69; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7.25; Value: 72; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7.25; Value: 72; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7.5; Value: 75; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7.5; Value: 74; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7.75; Value: 77; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7.75; Value: 76; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8; Value: 79; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8; Value: 79; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8.25; Value: 81; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8.25; Value: 81; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8.5; Value: 84; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8.5; Value: 83; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8.75; Value: 86; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8.75; Value: 85; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 9; Value: 88; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 9; Value: 88; Status: 'abcdef'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 6.5; Value: 68; Status: 'abcd'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 6.5; Value: 67; Status: 'abcd'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 6.75; Value: 70; Status: 'abc'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 6.75; Value: 69; Status: 'abc'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 7; Value: 72; Status: 'abcdef'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 7; Value: 71; Status: 'abcdef'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 7.25; Value: 75; Status: 'abc'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 7.25; Value: 73; Status: 'abc'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 7.5; Value: 77; Status: 'abcd'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 7.5; Value: 75; Status: 'abcd'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 4.5; Value: 59; Status: 'abcd'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 4.5; Value: 56; Status: 'abcd'; ),

{}  ( Age: 47; Gender: 'M'; YieldRate: 4.75; Value: 61; Status: 'ABC'; ),

    ( Age: 47; Gender: 'F'; YieldRate: 4.75; Value: 57; Status: 'ABC'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 5; Value: 63; Status: 'ABCDEF'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 5; Value: 59; Status: 'ABCDEF'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 5.25; Value: 65; Status: 'ABC'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 5.25; Value: 61; Status: 'ABC'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 5.5; Value: 67; Status: 'ABCD'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 5.5; Value: 63; Status: 'ABCD'; ),
    ( Age: 52; Gender: 'M'; YieldRate: 9.5; Value: 104; Status: 'ABC'; ),
    ( Age: 52; Gender: 'F'; YieldRate: 9.5; Value: 100; Status: 'ABC'; ),
    ( Age: 52; Gender: 'M'; YieldRate: 9.75; Value: 106; Status: 'AB'; ),
    ( Age: 52; Gender: 'F'; YieldRate: 9.75; Value: 102; Status: 'AB'; ),
    ( Age: 52; Gender: 'M'; YieldRate: 10; Value: 108; Status: 'ABCD'; ),
    ( Age: 52; Gender: 'F'; YieldRate: 10; Value: 104; Status: 'ABCD'; ),
    ( Age: 53; Gender: 'M'; YieldRate: 3; Value: 55; Status: 'ABCDEF'; ),
    ( Age: 53; Gender: 'F'; YieldRate: 3; Value: 49; Status: 'ABCDEF'; ),
    ( Age: 53; Gender: 'M'; YieldRate: 3.25; Value: 56; Status: 'ABC'; ),
    ( Age: 53; Gender: 'F'; YieldRate: 3.25; Value: 51; Status: 'ABC'; ),
    ( Age: 63; Gender: 'M'; YieldRate: 9.5; Value: 121; Status: 'ABC'; ),
    ( Age: 63; Gender: 'F'; YieldRate: 9.5; Value: 112; Status: 'ABC'; ),
    ( Age: 63; Gender: 'M'; YieldRate: 9.75; Value: 123; Status: 'AB'; ),
    ( Age: 63; Gender: 'F'; YieldRate: 9.75; Value: 114; Status: 'AB'; ),
    ( Age: 63; Gender: 'M'; YieldRate: 10; Value: 125; Status: 'ABCD'; ),
    ( Age: 63; Gender: 'F'; YieldRate: 10; Value: 116; Status: 'ABCD'; ),
    ( Age: 64; Gender: 'M'; YieldRate: 3; Value: 74; Status: 'ABCDEF'; ),
    ( Age: 64; Gender: 'F'; YieldRate: 3; Value: 65; Status: 'ABCDEF'; ),
    ( Age: 64; Gender: 'M'; YieldRate: 3.25; Value: 76; Status: 'ABC'; ),
    ( Age: 64; Gender: 'F'; YieldRate: 3.25; Value: 66; Status: 'ABC'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 5.5; Value: 127; Status: 'ABC'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 5.5; Value: 111; Status: 'ABC'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 5.75; Value: 129; Status: 'AB'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 5.75; Value: 113; Status: 'AB'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 6; Value: 131; Status: 'ABCDE'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 6; Value: 115; Status: 'ABCDE'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 6.25; Value: 133; Status: 'AB'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 6.25; Value: 116; Status: 'AB'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 6.5; Value: 134; Status: 'ABC'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 6.5; Value: 118;Status: 'ABCD'; )
  );
begin
  Result := CData;
end;


// _____________________________________________________________________________
{**
  Jvr - 06/12/2012 07:40:21<P>
}
class procedure TGadData.ODSCreateTable(AFileName: String);
var
  ODS: TDBIObjectListDataset;
  XDS: TDBIXbaseDataset;
  Gad: TGadData;
  Index: Integer;
  Today: TDateTime;
  GadData: TGadRecords;

begin
  // Create Gad Table
  ODS := TDBIObjectListDataset.Create(nil);
  try
    GadData := TGadData.GetRecords;
    for Index := Low(GadData) to High(GadData) do begin
      Today := Now;

      Gad := TGadData.Create;
      Gad.ID := Index;
      Gad.Age := GadData[Index].Age;
      Gad.Gender := GadData[Index].Gender;
      Gad.YieldRate := GadData[Index].YieldRate;
      Gad.Value := GadData[Index].Value;
      Gad.Comment := TGadData.GetComment;
      Gad.Status := GadData[Index].Status;
      Gad.Date := Trunc(Today);
      Gad.Time := TDBIString(Copy(FormatDateTime('hh:nn:ss.zzz', ToDay), 1, 8));
      Gad.Created := Today;

      ODS.List.Add(Gad);
      Sleep(5);
    end;

    CreateFields(ODS);
    ODS.CreateDataset;

    VerifyFields(ODS);
    AssertValues(ODS);

    XDS := TDBIXbaseDataset.Create(nil);
    try
      CreateFieldDefs(XDS);
      XDS.CreateDataset;
      VerifyFields(XDS);

      XDS.LoadFromDataset(ODS);

      VerifyFields(XDS);
      AssertValues(XDS);
{##JVR
      XDS.SaveToFile(ChangeFileExt(AFileName, '.cds'), dfCDS);
      XDS.SaveToFile(ChangeFileExt(AFileName, '.dbf'));
//}
    finally
      XDS.Free;
    end;

    ODS.SaveToFile(ChangeFileExt(AFileName, '.dbf'));
    ODS.SaveToFile(ChangeFileExt(AFileName, '.cds'), dfCDS);

    ODS.Close;
  finally
    ODS.Free;
  end;

  Assert(SysUtils.FileExists(ChangeFileExt(AFileName, '.dbf')), 'Failed to create Gad Table dbf');
  Assert(SysUtils.FileExists(ChangeFileExt(AFileName, '.cds')), 'Failed to create Gad Table cds');
end;


// _____________________________________________________________________________
{**
  Jvr - 03/12/2002 11:22:46.<P>
}
procedure TGadData.SetGender(const Value: TDBIString);
begin
  if (Value <> 'M') and (Value <> 'F') then begin
    raise Exception.Create('Illegal gender value, values [F, M] permited');
  end;

  FGender := Value;
end;


class procedure TGadData.UpdateValues(ADataset: TDataset);
var
  Index: Integer;
  GadData: TGadRecords;

begin
  GadData := TGadData.GetRecords;
  Index := Low(GadData);
  ADataset.First;

  while not ADataset.Eof do begin
    ADataset.Edit;
    ADataset.FieldByName('Age').AsInteger := GadData[Index].Age;
    ADataset.FieldByName('Gender').ASString := String(GadData[Index].Gender);
    ADataset.FieldByName('YieldRate').AsFloat := GadData[Index].YieldRate;
    ADataset.FieldByName('Value').AsInteger := GadData[Index].Value;
    ADataset.FieldByName('Comment').AsString := TGadData.getComment;
    ADataset.FieldByName('Status').AsString := String(GadData[Index].Status);
    ADataset.CheckBrowseMode;

    Inc(Index);
    ADataset.Next;
  end;
end;





{ TBookData }

class procedure TBookData.AddIndexes(ADataset: TDataset);

  procedure DefineIndex(const AName, AFields:  String; AOptions: TIndexOptions);
  begin
    if ADataset is TDBIDataset then begin
      (ADataset as TDBIDataset).AddIndex(AName, AFields, AOptions);;
{$ifndef fpc}
    end
    else if ADataset is TClientDataset then begin
      (ADataset as TClientDataset).AddIndex(AName, AFields, AOptions);;
{$endif}
    end;
  end;

begin
  // String indices
  DefineIndex('NameOrder', 'Name', [ixCaseInsensitive]);
  DefineIndex('NameReverse', 'Name', [ixDescending, ixCaseInsensitive]);

  // Integer indices
  DefineIndex('RatingOrder', 'Rating', []);
  DefineIndex('RatingReverse', 'Rating', [ixDescending]);
end;


class procedure TBookData.AddIndexDefs(ADataset: TDataset);

  function DefineIndexDef(const AName, AFields:  String; AOptions: TIndexOptions): TIndexDef;
  begin
    Result := nil;

    if ADataset is TDBIDataset then begin
      Result := (ADataset as TDBIDataset).IndexDefs.AddIndexDef;
{$ifndef fpc}
    end
    else if ADataset is TClientDataset then begin
      Result := (ADataset as TClientDataset).IndexDefs.AddIndexDef;
{$endif}
    end;

    if Assigned(Result) then begin
      Result.Name := AName;
      Result.Fields := AFields;
      Result.Options := AOptions;
    end;
  end;

begin
  // String indices
  DefineIndexDef('NameOrder', 'Name', [ixCaseInsensitive]);
  DefineIndexDef('NameReverse', 'Name', [ixDescending, ixCaseInsensitive]);

  // Integer indices
  DefineIndexDef('RatingOrder', 'Rating', []);
  DefineIndexDef('RatingReverse', 'Rating', [ixDescending]);
end;


class procedure TBookData.ApplyValues(ADataset: TDataset; Index: Integer);
var
  NewBooks: TBookRecords;
begin
  NewBooks := TBookData.GetRecords;
  ADataset.SetFields([
    NewBooks[Index].Sequence,
    NewBooks[Index].Name,
    NewBooks[Index].Author,
    StrToDateTime(String(NewBooks[Index].Purchased)),
    NewBooks[Index].Price,
    NewBooks[Index].Currency,
    NewBooks[Index].Rating,
    NewBooks[Index].Approved,
    NewBooks[Index].Comments,
    NewBooks[Index].Notes,
    NewBooks[Index].Details
    ]);
end;


class procedure TBookData.CheckAscending(ADataset: TDataset; const AKeyName: String);
var
  Index: Integer;
  UpdatedBooks: TBookRecords;
begin
  UpdatedBooks := TBookData.GetUpdateRecords;

  if ADataset is TDBIDataset then begin
    (ADataset as TDBIDataset).IndexName := AKeyName;
{$ifndef fpc}
  end
  else if (ADataset is TClientDataset) then begin
    (ADataset as TClientDataset).IndexName := AKeyName;
{$endif}    
  end;

  ADataset.First;
  for Index := 0 to ADataset.RecordCount-1 do begin
    if CompareText(AKeyName, 'NameOrder') = 0 then begin
      Equalz(ADataset.FieldByName('Name').AsString , UpdatedBooks[GetNameAsc[Index]].Name);
    end
    else if CompareText(AKeyName, 'NameReverse') = 0 then begin
      Equalz(ADataset.FieldByName('Name').AsString , UpdatedBooks[GetNameDesc[Index]].Name);
    end
    else if CompareText(AKeyName, 'RatingOrder') = 0 then begin
      Assert(ADataset.FieldByName('Sequence').AsInteger = UpdatedBooks[GetRatingAsc[Index]].Sequence);
      Assert(ADataset.FieldByName('Rating').AsInteger = UpdatedBooks[GetRatingAsc[Index]].Rating);
    end
    else if CompareText(AKeyName, 'RatingReverse') = 0 then begin
      Assert(ADataset.FieldByName('Sequence').AsInteger = UpdatedBooks[GetRatingDesc[Index]].Sequence);
      Assert(ADataset.FieldByName('Rating').AsInteger = UpdatedBooks[GetRatingDesc[Index]].Rating);
    end
    else begin
      Assert(False, AKeyName + ' is an invalid index');
    end;

    ADataset.Next;
  end;
end;


class procedure TBookData.CheckDescending(ADataset: TDataset; const AKeyName: String);
var
  Index: Integer;
  UpdatedBooks: TBookRecords;
begin
  UpdatedBooks := TBookData.GetUpdateRecords;

  if ADataset is TDBIDataset then begin
    (ADataset as TDBIDataset).IndexName := AKeyName + ';Descending';
{$ifndef fpc}
  end
  else if ADataset is TClientDataset then begin
    (ADataset as TClientDataset).IndexName := AKeyName + ';Descending';
{$endif}
  end;

  ADataset.First;
  for Index := 0 to ADataset.RecordCount-1 do begin
    if CompareText(AKeyName, 'NameOrder') = 0 then begin
      Equalz(ADataset.FieldByName('Name').AsString , UpdatedBooks[GetNameDesc[Index]].Name);
    end
    else if CompareText(AKeyName, 'NameReverse') = 0 then begin
      Equalz(ADataset.FieldByName('Name').AsString , UpdatedBooks[GetNameAsc[Index]].Name);
    end
    else if CompareText(AKeyName, 'RatingOrder') = 0 then begin
      Assert(ADataset.FieldByName('Sequence').AsInteger = UpdatedBooks[GetRatingDesc[Index]].Sequence);
      Assert(ADataset.FieldByName('Rating').AsInteger = UpdatedBooks[GetRatingDesc[Index]].Rating);
    end
    else if CompareText(AKeyName, 'RatingReverse') = 0 then begin
      Assert(ADataset.FieldByName('Sequence').AsInteger = UpdatedBooks[GetRatingAsc[Index]].Sequence);
      Assert(ADataset.FieldByName('Rating').AsInteger = UpdatedBooks[GetRatingAsc[Index]].Rating);
    end;

    ADataset.Next;
  end;
end;


class procedure TBookData.CheckValues(ADataset: TDataset; Index: Integer);
var
  NewBooks: TBookRecords;
begin
  NewBooks := TBookData.GetRecords;
  Assert(ADataset.RecNo = (Index + 1));

  Assert(ADataset.FieldByName('Sequence').AsInteger = NewBooks[Index].Sequence);
  Equalz(ADataset.FieldByName('Name').AsString , NewBooks[Index].Name);
  Equalz(ADataset.FieldByName('Author').AsString , NewBooks[Index].Author);
  Equalz(
    FormatDateTime(GetDateTimeFormat, ADataset.FieldByName('Purchased').AsDateTime),
    NewBooks[Index].Purchased
    );
  Assert(ADataset.FieldByName('Price').AsFloat = NewBooks[Index].Price);
  Equalz(ADataset.FieldByName('Currency').AsString , NewBooks[Index].Currency);
  Assert(ADataset.FieldByName('Rating').AsInteger = NewBooks[Index].Rating);
  Assert(ADataset.FieldByName('Approved').AsBoolean = NewBooks[Index].Approved);
  Equalz(ADataset.FieldByName('Comments').AsString , NewBooks[Index].Comments);
  Equalz(ADataset.FieldByName('Notes').AsString , NewBooks[Index].Notes);
  Equalz(ADataset.FieldByName('Details').AsString , NewBooks[Index].Details);
end;


class procedure TBookData.CreateFields(ADataset: TDataset);
var
  BookFields: TBookFields;
begin
  BookFields := TBookData.GetFields;
  BuildFields(ADataset, @BookFields, Length(BookFields));
end;


class procedure TBookData.CreateFieldDefs(ADataset: TDataset);
var
  BookFields: TBookFields;
begin
  BookFields := TBookData.GetFields;
  BuildFieldDefs(ADataset, @BookFields, Length(BookFields));
end;


class function TBookData.GetFields: TBookFields;
const
  CData: TBookFields = (
    (FieldName: 'Sequence';  FieldType: ftInteger;  FieldSize:   0; Precision: 0; Required: True;  ReadOnly: False; ),
    (FieldName: 'Name';      FieldType: ftString;   FieldSize:  50; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Author';    FieldType: ftString;   FieldSize:  50; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Purchased'; FieldType: ftDate;     FieldSize:   0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Price';     FieldType: ftCurrency; FieldSize:   0; Precision: 2; Required: False; ReadOnly: False; ),
    (FieldName: 'Currency';  FieldType: ftString;   FieldSize:   3; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Rating';    FieldType: ftInteger;  FieldSize:   0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Approved';  FieldType: ftBoolean;  FieldSize:   0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Comments';  FieldType: ftString;   FieldSize: 250; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Notes';     FieldType: ftMemo;     FieldSize:   0; Precision: 0; Required: False; ReadOnly: False; ),
    (FieldName: 'Details';   FieldType: ftMemo;     FieldSize:   0; Precision: 0; Required: False; ReadOnly: False; )
  );
begin
  Result := CData;
end;


class function TBookData.GetNameAsc: TBookIndex;
const
  CData: TBookIndex = (4, 3, 0, 1, 2);
begin
  Result := CData;
end;


class function TBookData.GetNameDesc: TBookIndex;
const
  CData: TBookIndex = (2, 1, 0, 3, 4);
begin
  Result := CData;
end;


class function TBookData.GetRatingAsc: TBookIndex;
const
  CData: TBookIndex = (1, 4, 3, 0, 2);
begin
  Result := CData;
end;


class function TBookData.GetRatingDesc: TBookIndex;
const
  CData: TBookIndex = (2, 0, 3, 4, 1);
begin
  Result := CData;
end;


class function TBookData.GetRatingFilter: TBookFilter;
const
  CData: TBookFilter = (1, 4);
begin
  Result := CData;
end;


class function TBookData.GetRecordCount: Integer;
begin
  Result := Length(TBookData.GetRecords);
end;


class function TBookData.GetRecords: TBookRecords;
const
  CData: TBookRecords = (
    (
      Sequence: 0;
      Name: 'Learn Delphi 2 Database Programming Today!';
      Author: 'Jeff Cogswell';
      Purchased: '21/11/1996 00:00:00';
      Price: 69.95;
      Currency: 'AUD';
      Rating: 9;
      Approved: False;
      Comments: 'Unable to form a valid opinion on this one';
      Notes:    'SQL, OOP, Client/Server and 32-bit Delphi';
      Details:  'Jeff Cogswell, a freelance writer and professional ' +
                'programmer, is a regular contributer to Windows Tech Journal ' +
                'and VB Tech Journal.';
    ), (
      Sequence: 1;
      Name: 'Secrets of Delphi 2';
      Author: 'Ray Lischner';
      Purchased: '02/02/1997 00:00:00';
      Price: 49.95;
      Currency: 'USA';
      Rating: 2;
      Approved: True;
      Comments: 'A terrific book on the advanced subjects of Delphi';
      Notes:    '';
      Details:  '';
    ), (
      Sequence: 2;
      Name: 'Teach Yourself Delphi in 21 Days';
      Author: 'Wozniewics & Shammas';
      Purchased: '04/03/1996 00:00:00';
      Price: 59.95;
      Currency: 'AUD';
      Rating: 10;
      Approved: False;
      Comments: 'Book for beginners';
      Notes:    'In just 21 days, you can become a proficient Delphi ' +
                'programmer! Using easy-to-understand examples and full code ' +
                'listings, you''ll learn how to implement the latest Delphi ' +
                'enhancements and Teach Yourself Delphi in 21 Days.';
      Details:  '';
    ), (
      Sequence: 3;
      Name: 'Hidden Paths of Delphi 3';
      Author: 'Ray Lischner';
      Purchased: '15/08/1997 00:00:00';
      Price: 39.95;
      Currency: 'USA';
      Rating: 5;
      Approved: True;
      Comments: 'A Great book to learn about the internals of the Delphi IDE';
      Notes:    'Hidden Paths of Delphi 3 cuts through the clutter of ' +
                'all-in-one Delphi tomes to present a focused treatment of ' +
                'a vital, yet undocumented, ' +
                'Delphi feature: The open Tools API.';
      Details:  'LEARN TO BUILD DELPHI 3 Wizards.'#13 +
                'A collection of classes for interfacing with Delphi''s IDE, ' +
                'the Open Tools API allows developers to program in ' +
                'a familiar language, namely Object Pascal. The Open Tools API ' +
                'enables Delphi developers to create wizards to automate the ' +
                'creation of new forms and projects, create new items in ' +
                'Delphi''s menu, intercat with Delphi''s code and form editors, ' +
                'and define new file systems for storing and ' +
                'retrieving source, form, and resource files.'#13#13 +
                'ABOUT THE AUTHOR:'#13 +
                'Ray Lischner has written articles for Delphi Informat and ' +
                'Delphi Developer''s Journal. His first book, ' +
                'Secrets of Delphi 2, is widely recognized as the most useful ' +
                'book for advanced Delphi programming.'#13 +
                'Ray teaches computer science at Oregon State University';
    ), (
      Sequence: 4;
      Name: 'Developing Custom Delphi Components';
      Author: 'Ray Konopka';
      Purchased: '21/07/1998 00:00:00';
      Price: 80.00;
      Currency: 'AUD';
      Rating: 3;
      Approved: True;
      Comments: 'A good beginners book to component development';
      Notes:    '';
      Details:  'Ray Konopka is the founder of Raize Software Solutions, ' +
                'Inc., a supplier of Delphi custom components, training, ' +
                'and consulting services.';
    )
  );
begin
  Result := CData;
end;


class function TBookData.GetUpdateRecords: TBookRecords;
const
  CData: TBookRecords = (
    (
      Sequence: 0;
      Name: 'Learn Delphi 2 Database Programming Today!';
      Author: 'Jeff Cogswell';
      Purchased: '21/11/1996 00:00:00';
      Price: 69.95;
      Currency: 'AUD';
      Rating: 9;
      Approved: False;
      Comments: 'Unable to form a valid opinion on this one';
      Notes:    'SQL, OOP, Client/Server and 32-bit Delphi';
      Details:  'Jeff Cogswell, a freelance writer and professional ' +
                'programmer, is a regular contributer to Windows Tech Journal ' +
                'and VB Tech Journal.';
    ), (
      Sequence: 1;
      Name: 'Secrets of Delphi 2';
      Author: 'Ray Lischner';
      Purchased: '02/02/1997 00:00:00';
      Price: 99.95;
      Currency: 'AUD';
      Rating: 2;
      Approved: True;
      Comments: 'A terrific book on the advanced subjects of Delphi';
      Notes:    'Secrets of Delphi 2 exposes the mysteries of the ' +
                'undocumented features Delphi programmers must know to create ' +
                'high-quality, state-of-the art components and applications. ' +
                'From writing experts to mastering the internal messages in ' +
                'the Delphi base classes to controlling palette realization.';
      Details:  '';
    ), (
      Sequence: 10;
      Name: 'Teach Yourself Delphi in 21 Days';
      Author: 'Wozniewics & Shammas';
      Purchased: '04/03/1996 00:00:00';
      Price: 59.95;
      Currency: 'AUD';
      Rating: 10;
      Approved: False;
      Comments: 'Book for beginners';
      Notes:    'In just 21 days, you can become a proficient Delphi ' +
                'programmer! Using easy-to-understand examples and full code ' +
                'listings, you''ll learn how to implement the latest Delphi ' +
                'enhancements and Teach Yourself Delphi in 21 Days.';
      Details:  '';
    ), (
      Sequence: 5;
      Name: 'Hidden Paths of Delphi 3';
      Author: 'Ray Lischner';
      Purchased: '15/08/1997 00:00:00';
      Price: 39.95;
      Currency: 'USA';
      Rating: 7;
      Approved: True;
      Comments: 'A Great book to learn about the internals of the Delphi IDE';
      Notes:    'Hidden Paths of Delphi 3 cuts through the clutter of ' +
                'all-in-one Delphi tomes to present a focused treatment of ' +
                'a vital, yet undocumented, ' +
                'Delphi feature: The open Tools API.'#13#13 +
                'LEARN TO BUILD DELPHI 3 Wizards.'#13 +
                'A collection of classes for interfacing with Delphi''s IDE, ' +
                'the Open Tools API allows developers to program in ' +
                'a familiar language, namely Object Pascal. The Open Tools API ' +
                'enables Delphi developers to create wizards to automate the ' +
                'creation of new forms and projects, create new items in ' +
                'Delphi''s menu, intercat with Delphi''s code and form editors, ' +
                'and define new file systems for storing and ' +
                'retrieving source, form, and resource files.';
      Details:  'ABOUT THE AUTHOR:'#13 +
                'Ray Lischner has written articles for Delphi Informat and ' +
                'Delphi Developer''s Journal. His first book, ' +
                'Secrets of Delphi 2, is widely recognized as the most useful ' +
                'book for advanced Delphi programming.'#13 +
                'Ray teaches computer science at Oregon State University';
    ), (
      Sequence: 4;
      Name: 'Developing Custom Delphi Components';
      Author: 'Ray Konopka';
      Purchased: '21/07/1998 00:00:00';
      Price: 80.00;
      Currency: 'AUD';
      Rating: 3;
      Approved: True;
      Comments: 'A good beginners book for component development. Further-more';{ +
                'if possible it would be a good idea to purchase a more ' +
                'up-to-date version of the book. I believe that the latest ' +
                'version is for Delphi 3, but it may be difficult to get as ' +
                'it is already out of print.';}
      Notes:    'Developing Custom Delpi';
      Details:  '';
    )
  );
begin
  Result := CData;
end;


class procedure TBookData.ODSUpdateTable(const AFileName: String);
var
  ODS: TObjectListDataset;

begin
  ODSCreateTable(AFileName);

  // Open Dataset
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.LoadFromFile(AFileName);
    TBookData.AssertValues(ODS);

    // Update Data
    TBookData.UpdateValues(ODS);
    TBookData.ReviseFields(ODS);

    ODS.SaveToFile(AFileName);
    ODS.SaveToFile(ChangeFileExt(AFileName, '.cds'), dfCDS);
    ODS.Close;
  finally
    ODS.Free;
  end;
end;


class procedure TBookData.ReviseFields(ADataset: TDataset);
var
  Index: Integer;
  UpdatedBooks: TBookRecords;
begin
  UpdatedBooks := TBookData.GetUpdateRecords;

  ADataset.First;
  for Index := Low(UpdatedBooks) to High(UpdatedBooks) do begin
    Assert(ADataset.RecNo = (Index + 1));

    Assert(ADataset.FieldByName('Sequence').AsInteger = UpdatedBooks[Index].Sequence);
    Equalz(ADataset.FieldByName('Name').AsString , UpdatedBooks[Index].Name);
    Equalz(ADataset.FieldByName('Author').AsString , UpdatedBooks[Index].Author);
    Equalz(
      FormatDateTime(GetDateTimeFormat, ADataset.FieldByName('Purchased').AsDateTime),
      UpdatedBooks[Index].Purchased
      );
    Assert(ADataset.FieldByName('Price').AsFloat = UpdatedBooks[Index].Price);
    Equalz(ADataset.FieldByName('Currency').AsString , UpdatedBooks[Index].Currency);
    Assert(ADataset.FieldByName('Rating').AsInteger = UpdatedBooks[Index].Rating);
    Assert(ADataset.FieldByName('Approved').AsBoolean = UpdatedBooks[Index].Approved);
    Equalz(ADataset.FieldByName('Comments').AsString , UpdatedBooks[Index].Comments);
    Equalz(ADataset.FieldByName('Notes').AsString , UpdatedBooks[Index].Notes);
    Equalz(ADataset.FieldByName('Details').AsString , UpdatedBooks[Index].Details);

    ADataset.Next;
  end;

  Assert(ADataset.RecordCount = Length(UpdatedBooks));
end;


class procedure TBookData.UpdateValues(ADataset: TDataset);
var
  Index: Integer;
  UpdatedBooks: TBookRecords;
begin
  UpdatedBooks := TBookData.GetUpdateRecords;

  ADataset.First;
  for Index := Low(UpdatedBooks) to High(UpdatedBooks) do begin
    ADataset.Edit;
    ADataset.SetFields([
      UpdatedBooks[Index].Sequence,
      UpdatedBooks[Index].Name,
      UpdatedBooks[Index].Author,
      StrToDateTime(String(UpdatedBooks[Index].Purchased)),
      UpdatedBooks[Index].Price,
      UpdatedBooks[Index].Currency,
      UpdatedBooks[Index].Rating,
      UpdatedBooks[Index].Approved,
      UpdatedBooks[Index].Comments,
      UpdatedBooks[Index].Notes,
      UpdatedBooks[Index].Details
    ]);
    ADataset.Post;
    ADataset.Next;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2001 15:07:48<P>
}
class procedure TBookData.XDSUpdateTable(const AFileName: String);
var
{$ifndef fpc}
  CDS: TClientDataset;
{$endif}
  XDS: TXbaseDataset;

begin
  DeleteTables(AFileName);

  XDSCreateTable(AFileName);

  // Update Data
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FileName := AFileName;
    XDS.Open;

    TBookData.UpdateValues(XDS);
    TBookData.ReviseFields(XDS);

    XDS.SaveToFile(ChangeFileExt(AFileName, '.cds'), dfCDS);
    XDS.Close;
  finally
    XDS.Free;
  end;

  // Verify the data that was saved to the ".cds' is valid
  // Open Dataset
  Assert(SysUtils.FileExists(ChangeFileExt(AFileName, '.dbf')));
  Assert(SysUtils.FileExists(ChangeFileExt(AFileName, '.fpt')));
  Assert(SysUtils.FileExists(ChangeFileExt(AFileName, '.cds')));

{$ifndef fpc}
  CDS := TClientDataset.Create(nil);
  try
    CDS.LoadFromFile(ChangeFileExt(AFileName, '.cds'));

    TBookData.ReviseFields(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
{$endif}
end;





{ TDBIUnitTest}

class procedure TDBIUnitTest.AssertBlanks(ADataset: TDataset);
const
  ErrMsg = 'Field "%s" of Type "%s", is NOT NULL';

var
  Index: Integer;
  Field: TField;
  Success: Boolean;

begin
//##NULLS
{$ifdef fpc}
  Exit;
{$endif}
  Success := False;

  ADataset.First;
  while not ADataset.Eof do begin
    for Index := 0 to ADataset.FieldCount-1 do begin
      Field := ADataset.Fields[Index];

      // Skip fields that are "Required" (NULLs not permitted)
      if Field.Required then begin
        Success := True;
      end

      // Particular types in an object can NOT be represented as NULL
      else if (ADataset is TObjectListDataset) then begin
        case Field.Datatype of
{$ifdef DELPHI2009}
          ftByte,
{$endif}
          ftWord,
          ftInteger: Success := Field.AsInteger = 0;

{$ifdef DELPHI2009}
          ftSingle,
          ftExtended,
{$endif}
          ftFloat:   Success := Field.AsFloat = 0;
        end;
      end

      // Otherwise check if the field is NULL
      else begin
        Success := Field.IsNull;
      end;

      Assert(Success, Format(ErrMsg, [Field.FieldName, GetFieldTypeName(Field.Datatype)]));
    end;
    ADataset.Next;
  end;
end;


class procedure TDBIUnitTest.AssertValues(ADataset: TDataset);
var
  Index: Integer;

begin
  Index := 0;
  ADataset.First;

  Assert(ADataset.RecordCount > 0);
  while not ADataset.Eof do begin
    CheckValues(ADataset, Index);
    Inc(Index);
    ADataset.Next;
  end;
end;


class procedure TDBIUnitTest.AddFieldDefs(FieldDefs: TFieldDefs; PFieldData: PFieldRecords; const Count: Word);
var
  Index: Integer;
  FieldDef: TFieldDef;

begin
  for Index := 0 to Count-1 do begin
    FieldDef := FieldDefs.AddFieldDef;
    FieldDef.Name := UpperCase(PFieldData^[Index].FieldName);
    FieldDef.DataType := PFieldData^[Index].FieldType;

    if (PFieldData^[Index].FieldSize > 0) then begin
      FieldDef.Size := PFieldData^[Index].FieldSize;
    end;

    if (PFieldData^[Index].Precision > 0) then begin
      FieldDef.Precision := PFieldData^[Index].Precision;
    end;

    if PFieldData^[Index].Required then begin
      FieldDef.Required := PFieldData^[Index].Required;
    end;
  end;
end;


class procedure TDBIUnitTest.BuildFields(ADataset: TDataset; PFieldData: PFieldRecords; const Count: Word);
var
  Index: Integer;
  Field: TField;
  WCharFactor: Word;

begin
  WCharFactor := 1;
  if (ADataset is TObjectListDataset) then begin
    TObjectListDataset(ADataset).ClassTypeName := Self.ClassName;
{$ifndef DELPHI6}
  end
  else begin
    WCharFactor := 2;
{$endif}
  end;

  ADataset.FieldDefs.Clear;
  ADataset.Fields.Clear;

  for Index := 0 to Count-1 do begin
    Field := AddField(
      PFieldData^[Index].FieldName,
      PFieldData^[Index].FieldType,
      ADataset
      );

    Field.ReadOnly := PFieldData^[Index].ReadOnly;

    if PFieldData^[Index].Required then begin
      Field.Required := PFieldData^[Index].Required;
    end;

    if (PFieldData^[Index].FieldType = ftWideString) then begin
      Field.Size := PFieldData^[Index].FieldSize * WCharFactor;
    end
    else begin
      Field.Size := PFieldData^[Index].FieldSize;
    end;
  end;
end;


class procedure TDBIUnitTest.BuildFieldDefs(ADataset: TDataset; PFieldData: PFieldRecords; const Count: Word);

  function GetNullFieldSize: Word;
  var
    Index: Word;
    
  begin
    Result := 0;

    for Index := 0 to ADataset.FieldDefs.Count-1 do begin
      if not ADataset.FieldDefs[Index].Required then begin
        Inc(Result);
      end;
    end;

    if Result > 0 then begin
      if (Result Mod 8) = 0 then begin
        Result := Result div 8;
      end
      else begin
        Result := 1 + (Result div 8);
      end;
    end;
  end;

var
  Index: Integer;
  FieldDef: TFieldDef;
  
begin
  if (ADataset is TObjectListDataset) then begin
    (ADataset as TObjectListDataset).ClassTypeName := Self.ClassName;
  end;

  ADataset.FieldDefs.Clear;
  ADataset.Fields.Clear;

  AddFieldDefs(ADataset.FieldDefs, PFieldData, Count);
//##NULLS
{$ifndef fpc}
  Index := GetNullFieldSize;
  if (Index > 0) and (ADataset is TXbaseDataset) then begin
    FieldDef := ADataset.FieldDefs.AddFieldDef;
    FieldDef.Name := Xbase_NullFlags;
    FieldDef.DataType := ftBytes;
    FieldDef.Attributes := [db.faHiddenCol, db.faReadOnly];
    FieldDef.Size := Index;

{$ifdef UseDebugInfo}
    ShowMessageFmt('Added Fielddef "%s"', [FieldDef.Name]);
{$endif}
  end;
{$endif}
end;


class procedure TDBIUnitTest.ClearValues(ADataset: TDataset);
var
  Index: Integer;
  Field: TField;

begin
  ADataset.First;
  while not ADataset.Eof do begin
    ADataset.Edit;

    for Index := 0 to ADataset.FieldCount-1 do begin
      Field := ADataset.Fields[Index];
      if not Field.Required then begin
        Field.Clear;
      end;
    end;

    ADataset.Post;
    ADataset.Next;
  end;
end;


class function TDBIUnitTest.CreateXBaseDataset: TDBIXbaseDataset;
begin
  Result := TDBIXbaseDataset.Create(nil);

  CreateFields(Result);
  Result.CreateDataset;
end;


class function TDBIUnitTest.GetDateTime(Index: Integer): TDateTime;
var
  TimeValue: TDateTime;

begin
  TimeValue := EncodeTime(Index mod 12, Index mod 60, Index mod 60, 0);
  Result := Trunc(Now) + TimeValue;
end;


class function TDBIUnitTest.GetDateTimeFormat: String;
begin
  Result := 'DD/MM/YYYY HH:NN:SS';
end;


class function TDBIUnitTest.GetRecordCount: Integer;
begin
  Result := 0;
end;


class procedure TDBIUnitTest.DeleteTables(const AFileName: String);
begin
  // Delete the Datafile if it exists
  SysUtils.DeleteFile(ChangeFileExt(AFileName, '.dbf'));
  Assert(not SysUtils.FileExists(ChangeFileExt(AFileName, '.dbf')));

  // Delete the Blobfile if it exists
  SysUtils.DeleteFile(ChangeFileExt(AFileName, '.fpt'));
  Assert(not SysUtils.FileExists(ChangeFileExt(AFileName, '.fpt')));

  // Delete the Cdsfile if it exists
  SysUtils.DeleteFile(ChangeFileExt(AFileName, '.cds'));
  Assert(not SysUtils.FileExists(ChangeFileExt(AFileName, '.cds')));
end;


{$ifndef fpc}
class procedure TDBIUnitTest.FieldProps(ADataset: TDBIClientDataset);
var
  Data: TStringDataList;
  CursorProps: DSIntf.DSProps;
  FieldProps: DBClient.TFieldDescList;
  Index: Integer;

begin
  Assert(ADataset.DSBase.GetProps(CursorProps) = 0);
  Assert(CursorProps.iFields = ADataset.Fields.Count);

  SetLength(FieldProps, CursorProps.iFields);
  Assert(ADataset.DSBase.GetFieldDescs(DSIntf.PDSFldDesc(FieldProps)) = 0);
{##JVR
  for Index := Low(TBookFields) to High(TBookFields) do begin
    CheckField(@TBookFields[Index], @FieldProps[Index]);
  end;
//}
  Data := TStringDataList.Create;
  try
    Data.Add(Format('--- TClientDataset field definitions ---', []));
    for Index := 0 to CursorProps.iFields-1 do begin
      Data.AddProps(FieldProps[Index]);
    end;

    ShowMessage(Data.Text);
  finally
    Data.Free;
  end;
end;
{$endif}

class procedure TDBIUnitTest.FieldProps(ADataset: TDBIObjectListDataset);
var
  Data: TStringDataList;
  CursorProps: DBIIntfConsts.DSProps;
  FieldProps: DBIIntfConsts.TFieldDescList;
  Index: Integer;

begin
  Assert(ADataset.DSBase.GetProps(CursorProps) = 0);
  Assert(CursorProps.iFields = ADataset.Fields.Count);

  SetLength(FieldProps, CursorProps.iFields);
  Assert(ADataset.DSBase.GetFieldDescs(DBIIntfConsts.PDSFldDesc(FieldProps)) = 0);
{##JVR
  for Index := Low(TOrdinalDataFields) to High(TOrdinalDataFields) do begin
    CheckField(@TStringDataFields[Index], @FieldProps[Index]);
  end;
//}
  Data := TStringDataList.Create;
  try
    Data.Add(Format('--- TObjectListDataset field definitions ---', []));

    for Index := 0 to CursorProps.iFields-1 do begin
      Data.AddProps(FieldProps[Index]);
    end;

    ShowMessage(Data.Text);
  finally
    Data.Free;
  end;
end;


class procedure TDBIUnitTest.FieldProps(ADataset: TDBIXbaseDataset);
var
  Data: TStringDataList;
  CursorProps: DBIIntfConsts.DSProps;
  FieldProps: DBIIntfConsts.TFieldDescList;
  Index: Integer;

begin
  Assert(ADataset.DSBase.GetProps(CursorProps) = 0);
//##JVR  Assert(CursorProps.iFields = ADataset.Fields.Count);

  SetLength(FieldProps, CursorProps.iFields);
  Assert(ADataset.DSBase.GetFieldDescs(DBIIntfConsts.PDSFldDesc(FieldProps)) = 0);
{##JVR
  for Index := Low(TBookFields) to High(TBookFields) do begin
    CheckField(@TBookFields[Index], @FieldProps[Index]);
  end;
//}
  Data := TStringDataList.Create;
  try
    Data.Add(Format('--- TXBaseDataset field definitions ---', []));
    for Index := 0 to CursorProps.iFields-1 do begin
      Data.AddProps(FieldProps[Index]);
    end;

    ShowMessage(Data.Text);
  finally
    Data.Free;
  end;
end;


class procedure TDBIUnitTest.FieldValues(ADataset: TDataset);
{$ifdef UseDebugInfo}
var
  Data: TFieldValuesList;
  Index: Integer;

begin
  Data := TFieldValuesList.Create;
  try
    ADataset.First;
    while not ADataset.Eof do begin
      Data.Add(Format('  >> %d', [ADataset.RecNo]));
      for Index := 0 to ADataset.FieldCount-1 do begin
        Data.AddField(ADataset.Fields[Index]);
      end;

      ADataset.Next;
    end;
    ShowMessage(Data.Text);
  finally
    Data.Free;
  end;
{$else}
begin
{$endif}
end;


class procedure TDBIUnitTest.OccupyValues(ADataset: TDataset);
var
  Index: Integer;

begin
  for Index := 0 to GetRecordCount-1 do begin
    ADataset.Append;
    ApplyValues(ADataset, Index);
    ADataset.Post;

    CheckValues(ADataset, Index);
  end;
end;


class procedure TDBIUnitTest.RefillValues(ADataset: TDataset);
var
  Index: Integer;

begin
  Index := 0;
  ADataset.First;

  while not ADataset.Eof do begin
    ADataset.Edit;
    ApplyValues(ADataset, Index);
    ADataset.Post;
    
    Inc(Index);
    ADataset.Next;
  end;
end;


class procedure TDBIUnitTest.VerifyFields(ADataset: TDataset);
{$ifdef UseDebugInfo}
var
  Data: TStringDataList;
  Index: Integer;

begin
  if ADataset is TDBIObjectListDataset then begin
    FieldProps(ADataset as TDBIObjectListDataset);
  end
{$ifndef fpc}
  else if ADataset is TDBIClientDataset then begin
    FieldProps(ADataset as TDBIClientDataset);
  end
{$endif}
  else if ADataset is TDBIXBaseDataset then begin
    FieldProps(ADataset as TDBIXBaseDataset);
  end;


  Data := TStringDataList.Create;
  try
    Data.Add(Format('--- TObjectListDataset fields ---', []));

    for Index := 0 to ADataset.Fields.Count-1 do begin
      Data.AddProps(ADataset.Fields[Index]);
    end;

    ShowMessage(Data.Text);
  finally
    Data.Free;
  end;
{$else}
begin
{$endif}
end;


{$ifndef fpc}
class procedure TDBIUnitTest.CDSCreateTable(AFileName: String);
var
  CDS: TDBIClientDataset;

begin
  CDS := TDBIClientDataset.Create(nil);
  try
    CreateFieldDefs(CDS);
    CDS.CreateDataset;

    VerifyFields(CDS);
    OccupyValues(CDS);
    AssertValues(CDS);

    CDS.SaveToFile(ChangeFileExt(AFileName, '.xml'), dfXML);
    CDS.Close;
  finally
    CDS.Free;
  end;
end;
{$endif}


class procedure TDBIUnitTest.ODSCreateTable(AFileName: String);
var
{$ifndef fpc}
  CDS: TDBIClientDataset;
{$endif}
  ODS: TDBIObjectListDataset;
  XDS: TDBIXbaseDataset;

begin
  // Create a new ObjectlistDataset, add data, and verify
  ODS := TDBIObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := Self.ClassName;
    CreateFieldDefs(ODS);
    ODS.CreateDataset;

    VerifyFields(ODS);
    OccupyValues(ODS);

    ODS.SaveToFile(ChangeFileExt(AFileName, '.dbf'));
    ODS.SaveToFile(ChangeFileExt(AFileName, '.cds'), dfCDS);
    AssertValues(ODS);

    // Load Memory XBaseDataset from ObjectListDataset and verify data
    XDS := TDBIXBaseDataset.Create(nil);
    try
      XDS.FileName := ChangeFileExt(ChangeFileExt(AFileName, '') + 'a', '.dbf');
      CreateFieldDefs(XDS);
      XDS.CreateDataset;
      VerifyFields(XDS);
      XDS.LoadFromDataset(ODS);
      AssertValues(XDS);

      ClearValues(XDS);
      AssertBlanks(XDS);

      RefillValues(XDS);
      AssertValues(XDS);

      XDS.Close;
    finally
      XDS.Free;
    end;

    AFileName := ChangeFileExt(ChangeFileExt(AFileName, '') + 'b', '.dbf');
    ODS.SaveToFile(ChangeFileExt(AFileName, '.dbf'));
    ODS.SaveToFile(ChangeFileExt(AFileName, '.cds'), dfCDS);
    ODS.Close;
  finally
    ODS.Free;
  end;


  // Load ObjectListDataset from file, test nulls  and verify data
  ODS := TDBIObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := Self.ClassName;
    CreateFieldDefs(ODS);
    ODS.CreateDataset;

    ODS.LoadFromFile(AFileName);
    VerifyFields(ODS);
    AssertValues(ODS);

    ClearValues(ODS);
    AssertBlanks(ODS);

    ODS.SaveToFile(ChangeFileExt(AFileName, '.nulls.dbf'));

    RefillValues(ODS);
    AssertValues(ODS);

    ODS.Close;
  finally
    ODS.Free;
  end;


  // Load Memory XBaseDataset from file and verify data
  XDS := TDBIXBaseDataset.Create(nil);
  try
    XDS.LoadFromFile(AFileName);
    VerifyFields(XDS);
    AssertValues(XDS);
    XDS.Close;
  finally
    XDS.Free;
  end;


  // Load the table into a ClientDataset and verify data
{$ifndef fpc}
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.LoadFromFile(ChangeFileExt(AFileName, '.cds'));
    AssertValues(CDS);

    // Load Memory XBaseDataset from ClientDataset and verify data
    XDS := TDBIXBaseDataset.Create(nil);
    try
      CreateFieldDefs(XDS);
      XDS.CreateDataset;
      XDS.LoadFromDataset(CDS);
      AssertValues(XDS);
      XDS.Close;
    finally
      XDS.Free;
    end;

    CDS.Close;
  finally
    CDS.Free;
  end;
{$endif}
end;


class procedure TDBIUnitTest.XDSCreateTable(AFileName: String);
var
{$ifndef fpc}
  CDS: TDBIClientDataset;
{$endif}
  XDS: TDBIXBaseDataset;

begin
  // Create New Table
  XDS := TDBIXBaseDataset.Create(nil);
  try
    CreateFieldDefs(XDS);

    XDS.FileName := ChangeFileExt(AFileName, '.dbf');
    XDS.CreateDataset;

    VerifyFields(XDS);
    OccupyValues(XDS);
    AssertValues(XDS);

    XDS.SaveToFile(ChangeFileExt(AFileName, '.cds'), dfCDS);

    XDS.Close;
  finally
    XDS.Free;
  end;


  // Reload XBase Table from file, test nulls and verify data
  XDS := TDBIXBaseDataset.Create(nil);
  try
    XDS.LoadFromFile(AFileName);
    AssertValues(XDS);

    ClearValues(XDS);
    AssertBlanks(XDS);

    XDS.SaveToFile(ChangeFileExt(AFileName, '.nulls.dbf'));

    RefillValues(XDS);
    AssertValues(Xds);

    XDS.Close;
  finally
    XDS.Free;
  end;


  // Load the table into a ClientDataset and verify data
{$ifndef fpc}
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.LoadFromFile(ChangeFileExt(AFileName, '.cds'));

    AssertValues(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
{$endif}
end;





{ TDBIUnitTests }

// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 12:49:01 - Initial code.<br>
  Jvr - 13/09/2004 14:55:58 - Modified way path is determined.<p>
}
function TDBIUnitTests.DataPath(const FileName: String = ''): String;
begin
  Result := TestTempDir + FileName;
end;


function TDBIUnitTests.GetParent: TomTestSuite;
begin
  Result := Self;
end;


{$ifndef omTesting}
function TDBIUnitTests.GetTestTempDir: String;
begin
  Result := DBIUtils.DBITempFolder;
end;
{$endif}





{ TDBIClientDataset }

{$ifndef fpc}
function TDBIClientDataset.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := inherited GetFieldClass(FieldType);
{$ifdef DELPHI2009}
  if (FieldType = ftExtended) then begin
    Result := TDBIExtendedField;
  end;
{$endif}
end;
{$endif}


initialization
  Classes.RegisterClass(TGadData);
  Classes.RegisterClass(TBookData);
  Classes.RegisterClass(TBookCategory);
  Classes.RegisterClass(TOrdinalData);
  Classes.RegisterClass(TFloatData);
  Classes.RegisterClass(TStringData);
  Classes.RegisterClass(TEntityData);

{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBIUnitTests);
{$else}
  RegisterTest('', TDBIUnitTests.Suite);
{$endif}

end.

