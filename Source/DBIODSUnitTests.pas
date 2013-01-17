{ ______________________________________________________________________________

  <H5>Copyright</H5>
  ObjectMastery Pty Ltd<BR>
  Copyright (C) 2001, All rights reserved.<!--

  Project:       Common
  Files(s):      DBIODSUnitTests.pas
  Classes:       ...
  Author:        John Vander Reest
  Purpose:       Test Suites for the Objectlist Dataset

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 30/01/2001 10:44:42 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

{#omcodecop off : jvr : native api code}

unit DBIODSUnitTests;

interface

{$I DBICompilers.inc}

uses
  Classes,
  Contnrs,
{$ifdef omTesting}
  omTestSuites,
  omTestMastery,
{$else}
  TestFrameWork,
{$endif}
  DBIStrings,
  DBIObjectListDatasets,
  DBIUnitTests
  ;

type
  TDBIODSUnitTests = class(TDBIUnitTests)
  private
    FObjectListDataset: TObjectListDataset;

  protected
    procedure CreateGadTable;

    procedure Setup; override;
    procedure Teardown; override;

{$ifdef omTesting}
    procedure Fail(msg: string; errorAddr: Pointer = nil);
{$endif}

    // Still in progress
    procedure NestedDataset;
    procedure ReferenceFields;

  published
    procedure AutoFieldDefs;
    procedure CreateBooks;
    procedure CreateGadCDS;
    procedure CreateGadODS;
    procedure DateTimeConsts;
    procedure DefaultStringAttributes;
    procedure FieldPropsCDS;
    procedure FieldPropsODS;
    procedure Indices;
    procedure LoadFromDataset;
    procedure LoadFromFile;
    procedure Locate;
    procedure ObjectDataEvents;
    procedure MemoryStreams;
    procedure NullFlags;
    procedure ReadOnlyProperty;
    procedure SaveAsCDS;
    procedure UpdateBooks;
    procedure Validation;

  end;  { TDBIObjectListUnitTests }


  TObjectListBaseData = class(TPersistent)
  private
    FSequence: Integer;
    FName: TDBIString;

  published
    property Sequence: Integer read FSequence write FSequence;
    property Name: TDBIString read FName write FName;

  end;  { TObjectListBaseData }


  TObjectListData = class(TObjectListBaseData)
  private
    FDescription: TDBIString;
    FComments: TDBIString;

  published
    property Description: TDBIString read FDescription write FDescription;
    property Comments: TDBIString read FComments write FComments;

  end;  { TObjectListData }


  TReadOnlyData = class(TObjectListBaseData)
  private
    FDoNotSet: TDBIString;

  published
    property DoNotSet: TDBIString read FDoNotSet;

  end;  { TReadOnlyData }


  TReference = class(TPersistent)
  private
    FDescription: TDBIString;

  published
    property Description: TDBIString read FDescription write FDescription;

  end;  { TReference }


  TParentObject = class(TPersistent)
  private
    FName: TDBIString;
    FReference: TReference;

  published
    property Name: TDBIString read FName write FName;
    property Reference: TReference read FReference write FReference;

  end;  { TParentObject }








implementation

uses
{$ifdef DELPHI6}
  Variants,
{$endif}
  Windows,
  SysUtils,
  Dialogs,
  DB,
  DBClient,
  DSIntf,
  DBIConst,
  DBIDataset,
  DBIXbaseDatasets;



// =============================================================================
// 'TDBIObjectListUnitTests' methods
// =============================================================================


// _____________________________________________________________________________
{**
  Test the auto FieldDefs generation from the object's published properties,
  when none have been previously defined.

  Jvr - 28/05/2001 16:15:10.<P>
}
procedure TDBIODSUnitTests.AutoFieldDefs;
var
  ODS: TObjectListDataset;

begin
  // Create a new Dataset
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.StringFieldSize := 1024;
    
    // No fielddefs are defined because we are relying on the dataset to create
    // them automatically from the object's published properties
    ODS.Open;

    TBookData.OccupyFields(ODS);
    TBookData.AssertFields(ODS);

    ODS.Close;
  finally
    ODS.Free;
  end;
end;  { AutoFieldDefs }


// _____________________________________________________________________________
{**
  Jvr - 12/02/2001 14:25:47.<P>
}
procedure TDBIODSUnitTests.CreateBooks;
const
  TableName = 'oCreateBooks.dbf';

var
  ODS: TObjectListDataset;

begin
  // Create a new Dataset
  TBookData.ODSCreateTable(DataPath(TableName));

  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;

    TBookData.CreateFieldDefs(ODS);

//    ODS.CreateDataset;

    // Verify Data was written correctly to file
    ODS.List.Clear;
    ODS.LoadFromFile(DataPath(TableName));

    TBookData.AssertFields(ODS);

    ODS.Close;

  finally
    ODS.Free;
  end;
end;  { CreateBooks }


procedure TDBIODSUnitTests.CreateGadCDS;
var
  CDS: TClientDataset;
  Index: Integer;
  Today: TDateTime;

begin
  // Create Gad CDS
  CDS := TClientDataset.Create(nil);
  try
    TGad.CreateFields(CDS);
    CDS.CreateDataset;

    for Index := Low(GadData) to High(GadData) do begin
      CDS.Append;
      Today := Now;

      CDS.FieldByName('ID').AsInteger := Index;
      CDS.FieldByName('Age').AsInteger := GadData[Index].Age;
      CDS.FieldByName('Gender').AsString := String(GadData[Index].Gender);
      CDS.FieldByName('YieldRate').AsFloat := GadData[Index].YieldRate;
      CDS.FieldByName('Value').AsFloat := GadData[Index].Value;
      CDS.FieldByName('Comment').AsString := GadData_Comment;
      CDS.FieldByName('Status').AsString := String(GadData[Index].Status);
      CDS.FieldByName('Date').AsDateTime := Trunc(Today);
      CDS.FieldByName('Time').AsString := Copy(FormatDateTime('hh:nn:ss.zzz', Today), 1, 8);
      CDS.FieldByName('Created').AsDateTime := Today;

      CDS.CheckBrowseMode;
      Sleep(5);
    end;

    TGad.AssertFields(CDS);

    CDS.SaveToFile(DataPath(cdsOGad));
    Assert(SysUtils.FileExists(DataPath(cdsOGad)), 'Failed to create Gad XML file');

    CDS.Close;
  finally
    CDS.Free;
  end;


  // Reload Gad Table and verify data
  CDS := TClientDataset.Create(nil);
  try
    TGad.CreateFields(CDS);
    CDS.LoadFromFile(DataPath(cdsOGad));

    TGad.AssertFields(CDS);
    TGad.ClearFields(CDS);
    TGad.AssertBlank(CDS);
    TGAD.UpdateFields(CDS);
    TGad.AssertFields(CDS);

    CDS.Close;

  finally
    CDS.Free;
  end;
end;  { CreateGadCDS }


procedure TDBIODSUnitTests.CreateGadODS;
var
  ODS: TObjectListDataset;

begin
  ZapOGadTable;
  CreateGadTable;

  // Reload Gad Table and verify data
  ODS := TObjectListDataset.Create(nil);
  try
    TGad.CreateFields(ODS);
    ODS.LoadFromFile(DataPath(dbfOGad), dfDefault, omCreateDataset);
    ODS.SaveToFile(DataPath('oGadVerify.dbf'));

    TGad.AssertFields(ODS);
    ODS.Close;
  finally
    ODS.Free;
  end;

  // Reload Gad Table and clear/edit/update data
  ODS := TObjectListDataset.Create(nil);
  try
    TGad.CreateFields(ODS);
    ODS.LoadFromFile(DataPath(dbfOGad), dfDefault, omCreateDataset);

    TGad.AssertFields(ODS);
    TGad.ClearFields(ODS);
    TGad.AssertBlank(ODS);
    TGad.UpdateFields(ODS);
    TGad.AssertFields(ODS);
    
    ODS.Close;
  finally
    ODS.Free;
  end;

end;  { CreateGadODS }


// _____________________________________________________________________________
{**
  Jvr - 06/12/2012 07:40:21<P>
}
procedure TDBIODSUnitTests.CreateGadTable;
var
  ODS: TObjectListDataset;
  Gad: TGad;
  Index: Integer;
  Today: TDateTime;

begin
  // Only create Gad table if it doesn't exist!
  if SysUtils.FileExists(DataPath(dbfOGad)) then begin
    Exit;
  end;

  // Create Gad Table
  ODS := TObjectListDataset.Create(nil);
  try
    TGad.CreateFields(ODS);

    for Index := Low(GadData) to High(GadData) do begin
      Today := Now;

      Gad := TGad.Create;
      Gad.ID := Index;
      Gad.Age := GadData[Index].Age;
      Gad.Gender := GadData[Index].Gender;
      Gad.YieldRate := GadData[Index].YieldRate;
      Gad.Value := GadData[Index].Value;
      Gad.Comment := GadData_Comment;
      Gad.Status := GadData[Index].Status;
      Gad.Date := Trunc(Today);
      Gad.Time := TDBIString(Copy(FormatDateTime('hh:nn:ss.zzz', ToDay), 1, 8));
      Gad.Created := Today;

      ODS.List.Add(Gad);
      Sleep(5);
    end;

    ODS.CreateDataset;
    ODS.SaveToFile(DataPath(dbfOGad));
    Assert(SysUtils.FileExists(DataPath(dbfOGad)), 'Failed to create Gad Table');

    TGad.AssertFields(ODS);
    ODS.Close;
  finally
    ODS.Free;
  end;
end;  { CreateGadTable }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2002 16:19:32.<P>
}
procedure TDBIODSUnitTests.DateTimeConsts;
var
//##JVR  DateTimeString: String;
  DateTimeValue: TDateTime;
  DateTimeVariant: Variant;

begin
  // Validate the DBIMinDateTime constant
  StrToDateTime(DateTimeToStr(DBIMinDateTime));
//(*##JVR
  try
    StrToDateTime(DateTimeToStr(DBIZeroDateTime));
    Fail('DBIZeroDateTime: Should never reach this point');

  except
    on ETestFailed do raise;
  else
    // Do Nothing
  end;
//*)

  // Validate the DBIMinDateTime constant
  StrToDateTime(DateTimeToStr(DBIMaxDateTime));
(*##JVR
  try
    DateTimeString := DateTimeToStr(DBIMaxDateTime+2);
    StrToDateTime(DateTimeString);
    Fail('DBIMaxDateTime + 2 = ' + DateTimeString + ': Should never reach this point');

  except
    on ETestFailed do raise;
  else
    // Do Nothing
  end;  { try..except }
//*)
  DateTimeValue := DBISignifyNullDateTime;
  DateTimeVariant := DateTimeValue;

  if not (DateTimeVariant = DateTimeValue) then begin
    raise Exception.Create('Comparison Failed');
  end;
end;  { DateTimeConsts }


procedure TDBIODSUnitTests.DefaultStringAttributes;
var
  ODS: TObjectListDataset;

begin
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.Options := [{Clear Options}];
    ODS.ClassTypename := TStringData.ClassName;
    ODS.StringFieldSize := 32;
{##JVR
    TDefaultStringAttributes.CreateFields(ODS);
    ODS.CreateDataset;
(*
//}
    ODS.Open;
//*)
    TStringData.OccupyFields(ODS);
    TStringData.AssertFields(ODS);

//##JVR    ShowMessage(ODS.FieldbyName('UserName').asString);
  finally
    ODS.Free;
  end;
end;  { DefaultStringAttributes }


// _____________________________________________________________________________
{**
  Jvr - 30/01/2001 13:27:28.<P>
}
{$ifdef omTesting}
procedure TDBIODSUnitTests.Fail(msg: string; errorAddr: Pointer = nil);
begin
  if errorAddr = nil then
    raise ETestFailed.Create(msg) //##JVR at CallerAddr
  else
    raise ETestFailed.Create(msg) at errorAddr;
end;
{$endif}


procedure TDBIODSUnitTests.FieldPropsCDS;
var
  CDS: TDBIClientDataset;

begin
  // Create String Data CDS
  CDS := TDBIClientDataset.Create(nil);
  try
    TStringData.CreateFields(CDS);
    CDS.CreateDataset;
    TStringData.SetupDataset(CDS);
    TStringData.OccupyFields(CDS);

    TStringData.VerifyFields(CDS);
    TStringData.AssertFields(CDS);

    CDS.SaveToFile(DataPath('FieldPropsCDS.cds'), dfXML);
    CDS.Close;
  finally
    CDS.Free;
  end;

  // Create String Data CDS
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.loadFromFile(DataPath('FieldPropsCDS.cds'));
    TStringData.VerifyFields(CDS);
    TStringData.AssertFields(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
end;


procedure TDBIODSUnitTests.FieldPropsODS;
var
  ODS: TDBIObjectListDataset;
  CDS: TDBIClientDataset;
  
begin
  // Create String Data ODS
  ODS := TDBIObjectListDataset.Create(nil);
  try
    TStringData.SetupDataset(ODS);
    TStringData.CreateFields(ODS);
    ODS.CreateDataset;

    TStringData.OccupyFields(ODS);
    TStringData.VerifyFields(ODS);
    TStringData.AssertFields(ODS);

    ODS.SaveToFile(DataPath('FieldPropsODS.cds'), dfCDS);
    ODS.SaveToFile(DataPath('FieldPropsODS.dbf'));
    ODS.Close;
  finally
    ODS.Free;
  end;

  // Create String Data CDS
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.loadFromFile(DataPath('FieldPropsODS.cds'));
    TStringData.VerifyFields(CDS);
    TStringData.AssertFields(CDS);

    // Create String Data ODS
    ODS := TDBIObjectListDataset.Create(nil);
    try
      TStringData.SetupDataset(ODS);
      TStringData.CreateFields(ODS);
//      ODS.CreateDataset;
      ODS.loadFromFile(DataPath('FieldPropsODS.dbf'), dfDefault, omCreateDataset);
      TStringData.VerifyFields(ODS);
      TStringData.AssertFields(ODS);

      CompareFieldProps(CDS, ODS);

      ODS.Close;
    finally
      ODS.Free;
    end;

    CDS.Close;
  finally
    CDS.Free;
  end;

end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 14:55:05.<P>
}
procedure TDBIODSUnitTests.Indices;
const
  TableName = 'oIndice.dbf';

var
  ODS: TObjectListDataset;
  Index: Integer;

begin
  DeleteTables(DataPath(TableName));
  TBookData.ODSUpdateTable(DataPath(TableName));

  ODS := TObjectListDataset.Create(nil);
  try
    // Verify Data
    with ODS do begin
      Close;
      ClassTypeName := TBookData.ClassName;
      LoadFromFile(DataPath(TableName));

      AddIndex('NameOrder', 'Name', [ixCaseInsensitive]);
      AddIndex('NameReverse', 'Name', [ixDescending, ixCaseInsensitive]);

      // Ascending Order
      IndexName := 'NameOrder';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdatedBooks[NameOrderIdx[Index]].Name);
        Next;
      end;

      // Descending Order
      IndexName := 'NameOrder' + ';Descending';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdatedBooks[NameReverseIdx[Index]].Name);
        Next;
      end;

      // Descending Order
      IndexName := 'NameReverse';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdatedBooks[NameReverseIdx[Index]].Name);
        Next;
      end;

      // Ascending Order
      IndexName := 'NameReverse' + ';Descending';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdatedBooks[NameOrderIdx[Index]].Name);
        Next;
      end;

      Close;
    end;  { with }
  finally
    ODS.Free;
  end;
end;  { Indices }


// _____________________________________________________________________________
{**
  Test the LoadFromDataset functionality.

  Jvr - 28/05/2001 16:32:04.<P>
}
procedure TDBIODSUnitTests.LoadFromDataset;
const
  TableName = 'oLoadFromDataset.dbf';

var
  ODS: TObjectListDataset;
  XDS: TXbaseDataset;

begin
  // Create a new books Table
  TBookData.ODSCreateTable(DataPath(TableName));

  // Open Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(DataPath(TableName));

    // dbfODSTest should have the same number of records as NewBooks
    Assert(XDS.RecordCount = Length(NewBooks));

    // Verify Data
    TBookData.AssertFields(XDS);

    // Load Data
    ODS := TObjectListDataset.Create(nil);
    try
      ODS.ClassTypeName := TBookData.ClassName;
      ODS.LoadFromDataset(XDS, [lmCreateDataset]);

      // Verify Data
      TBookData.AssertFields(ODS);

      ODS.Close;
    finally
      ODS.Free;
    end;

    XDS.Close;
  finally
    XDS.Free;
  end;
end;  { LoadFromDataset }


procedure TDBIODSUnitTests.LoadFromFile;
const
  TableName = 'oLoadFromFile.dbf';

var
  ODS: TObjectListDataset;

begin
  // Delete the Table if it exists
  TDBIUnitTests.DeleteTables(DataPath(TableName));

  // Create new table
  TBookData.ODSCreateTable(DataPath(TableName));

  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    TBookData.CreateFieldDefs(ODS);

    // Verify Data was written correctly to file
    ODS.List.Clear;
    ODS.LoadFromFile(DataPath(TableName));
    TBookData.AssertFields(ODS);

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 06/02/2001 16:42:38.<P>
}
procedure TDBIODSUnitTests.Locate;
var
  ODS: TObjectListDataset;
  Gad: TGad;
  Value: Integer;

begin
  ZapOGadTable;
  CreateGadTable;

  ODS := TObjectListDataset.Create(nil);
  try
    ODS.Close;
    ODS.ClassTypeName := TGad.Classname;
    ODS.LoadFromFile(DataPath(dbfOGad));

    Assert(ODS.RecordCount = Length(GadData));
    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([47, 'M', 4.75]), []));
    Value := ODS.FieldByName('Value').AsInteger;
    Assert(Value = 61);
    Assert(ODS.FieldByName('Value').AsInteger = 61);
    Gad := ODS.List[ODS.RecNo-1] as TGad;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([26, 'F', 3.50]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 40);
    Gad := ODS.List[ODS.RecNo-1] as TGad;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([35, 'M', 7.00]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 72);
    Gad := ODS.List[ODS.RecNo-1] as TGad;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([63, 'F', 10.00]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 116);
    Gad := ODS.List[ODS.RecNo-1] as TGad;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([52, 'M', 9.75]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 106);
    Gad := ODS.List[ODS.RecNo-1] as TGad;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([74, 'F', 6.25]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 116);
    Gad := ODS.List[ODS.RecNo-1] as TGad;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 26/07/2002 11:21:45.<P>
}
procedure TDBIODSUnitTests.MemoryStreams;

var
  DataStream: TMemoryStream;
  Gad: TObjectListDataset;
  Clone: TObjectlistDataset;

begin
  Gad := TObjectListDataset.Create(nil);
  try
    Clone := TObjectListDataset.Create(nil);
    try
      Gad.ClassTypeName := TGad.ClassName;
      Gad.LoadFromFile(DataPath(dbfOGad));

      DataStream := TMemoryStream.Create;
      try
        Gad.SaveToStream(DataStream);

        Clone.ClassTypeName := TGad.ClassName;
        Clone.LoadFromStream(DataStream, dfXbase, omOpen);

        { TODO 1 -oJvr -cTDBIODSUnitTests.MemoryStreams() :
          03/10/2002 12:09:05
          TCustomListDataset.LoadFromStream doesn't activate the dataset.
          It only gets the data, this may NOT be the desired behaviour,
          but it overcomes the problem of deciding if the dataset should be
          Opened, CreateDataset, or left InActive.
          This could possibly be resolved by adding an options parameter
          that defaults to open (e.g. ldOpen, ldCreateDataset, ldClose)
        }
        Clone.Open;

      finally
        DataStream.Free;
      end;

      // Test to see that the cloning procedure via a TMemoryStream is ok
      Assert(Gad.RecordCount = Clone.RecordCount);

      Gad.First;
      Clone.First;
      Assert(Gad.FieldByName('Value').AsInteger = Clone.FieldByName('Value').AsInteger);

      Gad.Next;
      Clone.Next;
      Assert(Gad.FieldByName('Value').AsInteger = Clone.FieldByName('Value').AsInteger);

      Gad.Last;
      Clone.Last;
      Assert(Gad.FieldByName('Value').AsInteger = Clone.FieldByName('Value').AsInteger);

      Gad.Prior;
      Clone.Prior;
      Assert(Gad.FieldByName('Value').AsInteger = Clone.FieldByName('Value').AsInteger);

      Clone.Close;
      Gad.Close;

    finally
      Clone.Free;
    end;  { with }
  finally
    Gad.Free;
  end;
end;  { MemoryStreams }


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 18:31:48.<P>
}
procedure TDBIODSUnitTests.NestedDataset;
var
  ODS: TObjectListDataset;
//##JVR  Index: Integer;
  
begin
  // Create a new Dataset
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookCategory.ClassName;
    // No fielddefs are defined because we are relying on the dataset to create
    // them automatically from the object's published properties
    ODS.CreateDataset;

(*##JVR
    Index := 0;
    ODS.AppendRecord([
      NewBooks[Index].Sequence,
      NewBooks[Index].Name,
      NewBooks[Index].Author,
      StrToDateTime(NewBooks[Index].Purchased),
      NewBooks[Index].Price,
      NewBooks[Index].Currency,
      NewBooks[Index].Rating,
      NewBooks[Index].Approved,
      NewBooks[Index].Comments,
      NewBooks[Index].Notes,
      NewBooks[Index].Details
    ]);

    Assert(ODS.RecNo = (Index + 1));
    Assert(ODS.FieldByName('Sequence').AsInteger = NewBooks[Index].Sequence);
    Assert(ODS.FieldByName('Name').AsString , NewBooks[Index].Name);
    Assert(ODS.FieldByName('Author').AsString , NewBooks[Index].Author);
    Assert(FormatDateTime(DateTimeFormat, ODS.FieldByName('Purchased').AsDateTime)
      = NewBooks[Index].Purchased);
    Assert(ODS.FieldByName('Price').AsFloat = NewBooks[Index].Price);
    Assert(ODS.FieldByName('Currency').AsString , NewBooks[Index].Currency);
    Assert(ODS.FieldByName('Rating').AsInteger = NewBooks[Index].Rating);
    Assert(ODS.FieldByName('Approved').AsBoolean = NewBooks[Index].Approved);
    Assert(ODS.FieldByName('Comments').AsString , NewBooks[Index].Comments);
    Assert(ODS.FieldByName('Notes').AsString , NewBooks[Index].Notes);

    // If the Data is too long then this will fail because the auto fielddefs
    // generation creates string fields with a default length of 255
    Assert(ODS.FieldByName('Details').AsString , NewBooks[Index].Details);

    Assert(ODS.RecordCount = 1);
*)
    ODS.Close;

  finally
    ODS.Free;
  end;  { try..finally }
end;  { NestedDataset }


// _____________________________________________________________________________
{**
  Jvr - 30/01/2001 10:53:38.<P>
}
procedure TDBIODSUnitTests.NullFlags;
const
  TestString = 'F1974';

begin
  with FObjectListDataset do begin
    Close;
    Open;  //##Jvr - 22/05/2002 14:15:38 CreateDataSet;
    AppendRecord([1, null, 'TestData']);

    // Assign some data
    Edit;
    FieldByName('Name').AsString := TestString;
    Assert(FieldByName('Name').AsString = TestString);

    Post;
    Assert(FieldByName('Name').AsString = TestString);

    // Now set it to null
    Edit;
    FieldByName('Name').Clear;
    Post;
    Assert(FieldByName('Name').IsNull);

    // Ok now reassign the data
    Edit;
    FieldByName('Name').AsString := TestString;
    Assert(FieldByName('Name').AsString = TestString);

    Post;
    Assert(FieldByName('Name').AsString = TestString);

  end;  { with }
end;  { NullFlags }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 16:10:38.<P>
}
procedure TDBIODSUnitTests.ObjectDataEvents;
type
  PNumRec = ^TNumRec;
  TNumRec = record
    Sequence: Integer;
    CategoryName: String; //##JVR [40];
  end;

const
  OrderedNumbers: array[0..9] of TNumRec = (
    (Sequence: 8; CategoryName: 'Eight'),
    (Sequence: 5; CategoryName: 'Five'),
    (Sequence: 4; CategoryName: 'Four'),
    (Sequence: 9; CategoryName: 'Nine'),
    (Sequence: 1; CategoryName: 'One'),
    (Sequence: 7; CategoryName: 'Seven'),
    (Sequence: 6; CategoryName: 'Six'),
    (Sequence: 3; CategoryName: 'Three'),
    (Sequence: 2; CategoryName: 'Two'),
    (Sequence: 0; CategoryName: 'Zero')
  );

var
  ODS: TObjectListDataset;
  DataObject: TBookCategory;
  Index: Integer;


  function NewCategory(const ID: Integer; const Name: String): TBookCategory;
  begin
    Result := TBookCategory.Create;
    Result.Sequence := ID;
    Result.CategoryName := TDBIString(Name);
  end;


  function CheckObject(const ID: Integer; const Name: String): Integer;
  begin
    Result := ODS.AppendObject(NewCategory(ID, Name));
    DataObject := ODS.List[Result] as TBookCategory;
    Assert(DataObject.Sequence = ID);
    Equalz(Name, DataObject.CategoryName);
  end;

  function CompareOrderedRecordWithObject(PItem: PNumRec): TBookCategory;
  begin
    Result := ODS.List[ODS.RecordNumber-1] as TBookCategory;

    Assert(ODS.FieldByName('Sequence').AsInteger = Result.Sequence);
    Equalz(ODS.FieldByName('CategoryName').AsString , Result.CategoryName);

    Assert(ODS.FieldByName('Sequence').AsInteger = PItem^.Sequence);
    Assert(
      CompareText(ODS.FieldByName('CategoryName').AsString , PItem^.CategoryName) = 0
      );
  end;

begin
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookCategory.ClassName;

    ODS.Open;
    ODS.AddIndex('KeyCategory', 'CategoryName', []);
    ODS.IndexName := 'KeyCategory' ;

    Assert(CheckObject(1, 'One') = 0);
    Assert(CheckObject(2, 'Two') = 1);
    Assert(CheckObject(3, 'Three') = 2);
    Assert(CheckObject(4, 'Four') = 3);
    Assert(CheckObject(5, 'Five') = 4);
    Assert(CheckObject(6, 'Six') = 5);
    Assert(CheckObject(7, 'Seven') = 6);
    Assert(CheckObject(8, 'Eight') = 7);
    Assert(CheckObject(9, 'Nine') = 8);
    Assert(CheckObject(10, 'Ten') = 9);

    DataObject := ODS.List[ODS.List.Count-1] as TBookCategory;
    DataObject.Sequence := 0;
    DataObject.CategoryName := 'Zero';
    ODS.UpdateObject(DataObject);

    ODS.First;
    for Index := Low(OrderedNumbers) to High(OrderedNumbers) do begin
      Assert(
        CompareOrderedRecordWithObject(@(OrderedNumbers[Index])).Sequence =
        (ODS.Data as TBookCategory).Sequence
        );
      ODS.Next;
    end;

    for Index := ODS.RecordCount-1 downto 0 do begin
      DataObject := ODS.List[Index] as TBookCategory;
      ODS.RemoveObject(DataObject);
      Assert(ODS.RecordCount = Index);
    end;

    Assert(CheckObject(11, 'Eleven') = 0);
    Assert(ODS.RecordCount = 1);

  finally
    ODS.Free;
  end;
end;  { ObjectDataEvents }


// _____________________________________________________________________________
{**
  Jvr - 30/01/2001 12:54:28.<P>
}
procedure TDBIODSUnitTests.ReadOnlyProperty;
var
  ObjectListDataset: TObjectListDataset;

begin
  ObjectListDataset := TObjectListDataset.Create(nil);
  try
    with ObjectListDataset do begin
      Close;
      Options := [osErrorOnReadonlyProperty];
      ClassTypeName := 'TReadOnlyData';
      FieldDefs.Clear;

      with FieldDefs.AddFieldDef do begin
        Name := 'Sequence';
        DataType := ftInteger;
        Required := True;
      end;

      with FieldDefs.AddFieldDef do begin
        Name := 'Name';
        DataType := ftString;
        Size := 20;
      end;

      with FieldDefs.AddFieldDef do begin
        Name := 'DoNotSet';
        DataType := ftString;
        Size := 10;
      end;

      CreateDataSet;
      try
        AppendRecord([1, null, 'TestData']);
        Fail('Should never reach this point');

      except
        on ETestFailed do raise;
      else
        // Do Nothing
      end;  { try..except }
    end;  { with }
  finally
    ObjectListDataset.Free;
  end;
end;  { ReadOnlyProperty }


// _____________________________________________________________________________
{**
  Jvr - 15/10/2001 13:58:12.<P>
}
procedure TDBIODSUnitTests.ReferenceFields;
var
  ODS: TObjectListDataset;
  Index: Integer;
  ParentObject: TParentObject;
//##JVR  PReference: ^TReference;
//##JVR  rf: TReferenceField;
  fld: TField;
  DataString: String;

begin
  // Create a new Dataset
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TParentObject.ClassName;

    ParentObject := TParentObject.Create;
    ParentObject.Name := 'John';
    ParentObject.Reference := TReference.Create;
    ParentObject.Reference.Description := 'One';
    ODS.List.Add(ParentObject);

    ParentObject := TParentObject.Create;
    ParentObject.Name := 'Melinda';
    ParentObject.Reference := TReference.Create;
    ParentObject.Reference.Description := 'Two';
    ODS.List.Add(ParentObject);

    ParentObject := TParentObject.Create;
    ParentObject.Name := 'Sam';
    ParentObject.Reference := TReference.Create;
    ParentObject.Reference.Description := 'Three';
    ODS.List.Add(ParentObject);

    ParentObject := TParentObject.Create;
    ParentObject.Name := 'Charlotte';
    ParentObject.Reference := TReference.Create;
    ParentObject.Reference.Description := 'Four';
    ODS.List.Add(ParentObject);

    ODS.Open; //##Jvr - 22/05/2002 13:16:59 - CreateDataset;

    // Verify Data
    Assert(ODS.RecordCount = ODS.List.Count);

{0} ODS.First;
    Equalz(ODS.FieldByName('Name').AsString , (ODS.List[0] as TParentObject).Name);
    fld := ODS.FieldByName('Reference');
    DataString := fld.ClassName;
    Windows.MessageBox(0, PChar(DataString), 'Debug', MB_OK);
//    PReference := Pointer(fld.AsVariant);
//    PReference := Pointer(ODS.FieldByName('Reference').AsVariant);
//    Assert(PReference^.Description = 'One');

{1} ODS.Next;
    Equalz(ODS.FieldByName('Name').AsString , (ODS.List[1] as TParentObject).Name);
//    PReference := Pointer(ODS.FieldByName('Reference').AsVariant);
//    Assert(PReference^.Description = 'Two');

{2} ODS.Next;
    Equalz(ODS.FieldByName('Name').AsString , (ODS.List[2] as TParentObject).Name);
//    PReference := Pointer(ODS.FieldByName('Reference').AsVariant);
//    Assert(PReference^.Description = 'Three');

{3} ODS.Next;
    Equalz(ODS.FieldByName('Name').AsString , (ODS.List[3] as TParentObject).Name);
//    PReference := Pointer(ODS.FieldByName('Reference').AsVariant);
//    Assert(PReference^.Description = 'Four');

    ODS.Close;

  finally
    // Free referenced objects
    for Index := 0 to ODS.List.Count - 1 do begin
      (ODS.List[Index] as TParentObject).Reference.Free;
      (ODS.List[Index] as TParentObject).Reference := nil;
    end;

    ODS.Free;
  end;
end;  { ReferenceFields }


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 12:40:34.<P>
}
procedure TDBIODSUnitTests.SaveAsCDS;
const
  TableName = 'oSaveAsCDS.dbf';

var
  ODS: TObjectListDataset;
  CDS: TClientDataSet;

begin
  TBookData.ODSUpdateTable(DataPath(TableName));

  // Open Dataset
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.LoadFromFile(DataPath(TableName));

    TBookData.ReviseFields(ODS);

    ODS.SaveToFile(DataPath(ChangeFileExt(TableName, '.cds')), dfCDS);
    ODS.Close;

    Assert(SysUtils.FileExists(DataPath(ChangeFileExt(TableName, '.cds'))));
  finally
    ODS.Free;
  end;  { try..finally }


  // Verify the data that was saved to the ".cds' is valid
  // Open Dataset
  CDS := TClientDataset.Create(nil);
  try
    CDS.LoadFromFile(DataPath(ChangeFileExt(TableName, '.cds')));

    // Verify Data
    TBookData.ReviseFields(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
end;  { SaveAsCDS }


procedure TDBIODSUnitTests.Setup;
begin
  inherited Setup;

{$ifdef Delphi2009}
  FormatSettings.ShortDateFormat := DateTimeFormat;
{$else}
  ShortDateFormat := DateTimeFormat;
{$endif}

  FObjectListDataset := TObjectListDataset.Create(nil);
  with FObjectListDataset do begin
    Close;
    ClassTypeName := 'TObjectListData';

    { DONE 5 -oJvr -cTDBIODSUnitTests.Setup :
      An object list dataset should read it's fielddefs
      from the class published properties information (RTTI)
      if the user hasn't already defined ther own fielddefs.
    }
(*
    FieldDefs.Clear;
    with FieldDefs.AddFieldDef do begin
      Name := 'Sequence';
      DataType := ftInteger;
      Required := True;
    end;

    with FieldDefs.AddFieldDef do begin
      Name := 'Name';
      DataType := ftString;
      Size := 20;
    end;

    with FieldDefs.AddFieldDef do begin
      Name := 'Description';
      DataType := ftString;
      Size := 40;
    end;

    with FieldDefs.AddFieldDef do begin
      Name := 'Comments';
      DataType := ftMemo;
    end;
//*)
  end;  { with }
end;


procedure TDBIODSUnitTests.Teardown;
begin
  FObjectListDataset.Free;
  FObjectListDataset := nil;

  inherited TearDown;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2001 16:06:32<P>
}
procedure TDBIODSUnitTests.UpdateBooks;
const
  TableName = 'oUpdateBooks.dbf';

begin
  TBookData.ODSUpdateTable(DataPath(TableName));
end;

(*##JVR
procedure TDBIODSUnitTests.UpdateBooksTable(const AFileName: String);
var
  ODS: TObjectListDataset;
  Index: Integer;

begin
  TBookData.ODSCreateTable(AFileName);

  // Open Dataset
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.LoadFromFile(AFileName);
    TBookData.AssertFields(ODS);

    // Update Data
    TBookData.UpdateFields(ODS);
    TBookData.ReviseFields(ODS);

    ODS.SaveToFile(AFileName);
    ODS.Close;
  finally
    ODS.Free;
  end;
end;
//*)

// _____________________________________________________________________________
{**
  Jvr - 28/11/2002 10:34:37.<P>
}
procedure TDBIODSUnitTests.Validation;
const
  GenderMap: array[Boolean] of TDBIString = ('M', 'F');

var
  ODS: TObjectListDataset;
  Gad: TGad;
  Index: Integer;

begin
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TGad.ClassName;
    ODS.StringFieldSize := 10;

    // Enable Validation
    ODS.Options := [osObjectValidation];

    // Disable validation procedure
    ODS.ObjectValidationProc := '';

    for Index := 1 to 100 do begin
      Gad := TGad.Create;
      Gad.Age := Index;
      Gad.Gender := GenderMap[(Index mod 2) = 0];
      ODS.List.Add(Gad);
    end;

    ODS.Open;

    ODS.First;
    ODS.Next;
    ODS.Next;

    ODS.Edit;
    try
      ODS.FieldByName('Gender').AsString := 'male';
      Fail('Should never reach this point');

    except
      on ETestFailed do begin
        ODS.Cancel;

        raise;
      end;
    else
      // Do Nothing
    end;  { try..except }


    ODS.Post;
  finally
    ODS.Free;
  end;
end;  { Validation }





initialization
  Classes.RegisterClass(TBookData);
  Classes.RegisterClass(TBookCategory);
  Classes.RegisterClass(TObjectListData);
  Classes.RegisterClass(TReadOnlyData);
  Classes.RegisterClass(TGad);
  Classes.RegisterClass(TReference);
  Classes.RegisterClass(TParentObject);

{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBIODSUnitTests);
{$else}
  RegisterTest('', TDBIODSUnitTests.Suite);
{$endif}

end.
