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
  1.0 | 30/01/2001 10:44:42 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIODSUnitTests;

interface

{$I DBICompilers.inc}

uses
  Classes, Contnrs,
{$ifndef fpc}
  DBClient, DSIntf,
  {$ifdef omTesting}
  omTestSuites, omTestMastery,
  {$else}
  TestFrameWork,
  {$endif}
{$else}
  testregistry,
{$endif}
  DBIStrings, DBIObjectListDatasets, DBIUnitTests;

type
  TDBIODSUnitTests = class(TDBIUnitTests)
  protected
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
    procedure BinaryTypes;
    procedure CreateBooks;
    procedure CreateGad;
{$ifndef fpc}
    procedure CreateGadCDS;
{$endif}
    procedure DataPacket;
    procedure DateTimeConsts;
    procedure DefaultStringAttributes;
{$ifndef fpc}
    procedure FieldPropsCDS;
{$endif}
    procedure FieldDefs;
    procedure FieldPropsODS;
    procedure Filters;
    procedure FloatTypes;
    procedure Indices;
    procedure LoadFromDataset;
    procedure LoadFromFile;
    procedure Locate;
    procedure ObjectDataEvents;
    procedure ObjectTypes;
    procedure OrdinalTypes;
    procedure MemoryStreams;
    procedure NullFlags;
    procedure ReadOnlyProperty;
    procedure SaveAsCDS;
    procedure Streaming;
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

{$ifdef omTesting}
  // Avoid the deprecation warning from TClientDataset.LoadFromFile in OM's patched D2006 DBClient.pas
  {$WARN SYMBOL_DEPRECATED OFF}
{$endif}

uses
{$ifdef DELPHI6}
  Variants,
{$endif}
  Windows,
  SysUtils,
  Dialogs,
  DB,
  DBIConst,
  DBIUtils,
  DBIComponents,
  DBIDataset,
  DBIIntfConsts,
  DBIXbaseDatasets,
  DBIXmlUtils;



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

    TBookData.OccupyValues(ODS);
    TBookData.AssertValues(ODS);

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


procedure TDBIODSUnitTests.BinaryTypes;
const
  Caller = 'BinaryTypes';
  TableName = 'oBinaryTypes.cds';

var
{$ifndef fpc}
  CDS: TDBIClientDataset;
{$endif}
  ODS: TObjectListDataset;
  ODSClone: TObjectListDataset;
  Index: Integer;
  Delta, Usage: Int64;

begin
{$ifndef fpc}
  // Create a new ObjectlistDataset, add data, and verify
  CDS := TDBIClientDataset.Create(nil);
  try
    TBinaryData.CreateFieldDefs(CDS);
    CDS.CreateDataset;

    TBinaryData.OccupyValues(CDS);
    TBinaryData.AssertValues(CDS);

    CDS.SaveToFile(DataPath('cBinaryTypes.xml'));

    CDS.Close;
  finally
    CDS.Free;
  end;


  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBinaryData.ClassName;

    // Verify Data was loaded correctly from CDS Xml file
    ODS.LoadFromFile(DataPath('cBinaryTypes.xml'));
    TBinaryData.AssertValues(ODS);

    TBinaryData.ClearValues(ODS);
    TBinaryData.AssertBlanks(ODS);

    TBinaryData.RefillValues(ODS);
    TBinaryData.AssertValues(ODS);

    TBinaryData.BlankValuesOfType(ODS, [ftInteger, ftDateTime]);
    TBinaryData.BlankValuesOfType(ODS, [ftString, ftMemo]);
    TBinaryData.AssertBlanks(ODS);

    ODS.Close;
  finally
    ODS.Free;
  end;
{$endif}


  // Create a new Dataset
  TBinaryData.ODSCreateTable(DataPath(TableName));

  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBinaryData.ClassName;

    // Verify Data was written correctly to file
    ODS.List.Clear;
    ODS.LoadFromFile(DataPath(TableName));
    TBinaryData.AssertValues(ODS);


    ODSClone := TObjectListDataset.Create(nil);
    try
      ODSClone.ClassTypeName := TBinaryData.ClassName;
      ODSClone.FieldDefs.Assign(ODS.FieldDefs);
      ODSClone.CloneCursor(ODS, True);

      Delta := 0;
      for Index := 0 to 1000 do begin
        ODSClone.Active := False;
        ODSClone.Active := True;
        TBinaryData.AssertValues(ODSClone);

        Usage := GetProcessMemoryUsage;
        if (Index > 1) then begin
          Delta := Usage - Delta;
          if (Delta <> 0) then begin
            Assert(Delta <= 0, Format('Increased memory usage of *%d bytes, on iteration %d', [Delta, Index]));
          end;
        end;
        Delta := Usage;

        TDBIDebugInfo.LogMsg(dkDebug, '%s::%s Memory Usage = [%f KB]', [Self.ClassName, Caller, Usage / 1024]);
      end;


      ODSClone.Close;
    finally
      ODSClone.Free;
    end;


    ODS.Close;
  finally
    ODS.Free;
  end;
end;


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

    // Verify Data was written correctly to file
    ODS.List.Clear;
    ODS.LoadFromFile(DataPath(TableName));

    TBookData.AssertValues(ODS);

    ODS.Close;

  finally
    ODS.Free;
  end;
end;


procedure TDBIODSUnitTests.CreateGad;
const
  TableName = 'oCreateGad.dbf';
var
  ODS: TObjectListDataset;

begin
  TGadData.DeleteTables(DataPath(TableName));
  TGadData.ODSCreateTable(DataPath(TableName));

  // Reload Gad Table and verify data
  ODS := TObjectListDataset.Create(nil);
  try
    TGadData.CreateFieldDefs(ODS);
    ODS.LoadFromFile(DataPath(TableName), dfDefault, omCreateDataset);
    ODS.SaveToFile(DataPath(TableName));

    TGadData.AssertValues(ODS);
    ODS.Close;

    ODS.Open;
    TGadData.AssertValues(ODS);
    ODS.Close;
  finally
    ODS.Free;
  end;

  // Reload Gad Table and clear/edit/update data
  ODS := TObjectListDataset.Create(nil);
  try
    TGadData.CreateFields(ODS);
    ODS.LoadFromFile(DataPath(TableName), dfDefault, omCreateDataset);
    TGadData.AssertValues(ODS);

    TGadData.ClearValues(ODS);
    TGadData.AssertBlanks(ODS);

    TGadData.UpdateValues(ODS);
    TGadData.AssertValues(ODS);

    ODS.Close;
  finally
    ODS.Free;
  end;
end;

{$ifndef fpc}
procedure TDBIODSUnitTests.CreateGadCDS;
const
  TableName = 'oCreateGadCDS.dbf';

var
  CDS: TDBIClientDataset;

begin
  // Create Gad CDS
  CDS := TDBIClientDataset.Create(nil);
  try
    TGadData.CreateFields(CDS);
    CDS.CreateDataset;

    TGadData.OccupyValues(CDS);
    TGadData.AssertValues(CDS);

    CDS.SaveToFile(DataPath(TableName));
    Assert(SysUtils.FileExists(DataPath(TableName)), 'Failed to create Gad XML file');

    CDS.Close;
  finally
    CDS.Free;
  end;


  // Reload Gad Table and verify data
  CDS := TDBIClientDataset.Create(nil);
  try
    TGadData.CreateFields(CDS);
    CDS.LoadFromFile(DataPath(TableName));
    TGadData.AssertValues(CDS);

    TGadData.ClearValues(CDS);
    TGadData.AssertBlanks(CDS);

    TGadData.UpdateValues(CDS);
    TGadData.AssertValues(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
end;
{$endif}


procedure TDBIODSUnitTests.DataPacket;
const
  TableName = 'oDataPacket.dbf';

var
  DataPacket: TDBIDataPacket;
  FieldData: TBookFields;
  ODS: TDBIObjectListDataset;

begin
  FieldData := TBookData.GetFields;

  // Create a new Dataset
  TBookData.ODSCreateTable(DataPath(TableName));

  DataPacket := nil;
  try
    ODS := TDBIObjectListDataset.Create(nil);
    try
      ODS.Name := ChangeFileExt(TableName, '');
      ODS.ClassTypeName := TBookData.ClassName;

      TBookData.CreateFieldDefs(ODS);

      // Verify Data was written correctly to file
      ODS.List.Clear;
      ODS.LoadFromFile(DataPath(TableName));

      TBookData.AssertValues(ODS);

      // Save the Dataset as a Data Packet
      DataPacket := TDBIXmlData.CreateDataPacket(ODS.Data);

      ODS.Close;
     finally
      ODS.Free;
    end;

    ODS := TDBIObjectListDataset.Create(nil);
    try
      ODS.Name := ChangeFileExt(TableName, '');
      ODS.ClassTypeName := TBookData.ClassName;

      // Assign DataPacket
      ODS.Data := DataPacket;

      Assert(ODS.DataSize > 0, 'Datasize indicates no data');
      Assert(Length(ODS.XmlData) = ODS.DataSize, 'Datasize is incorrect');

      TBookData.VerifyFields(ODS, @FieldData, Length(FieldData));
      TBookData.AssertValues(ODS);

      ODS.Close;
     finally
      ODS.Free;
    end;
  finally
    TDBIXmlData.FreeDataPacket(DataPacket);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/11/2002 16:19:32.<P>
}
procedure TDBIODSUnitTests.DateTimeConsts;
var
  DateTimeString: String;
  DateTimeValue: TDateTime;
  DateTimeVariant: Variant;

begin
  // Validate the DBIMinDateTime constant
  DateTimeString := FormatDateTime(TDBIUnitTest.GetDateTimeFormat, DBIMinDateTime);
  StrToDateTime(DateTimeString);

  // Validate the DBIMaxDateTime constant
  DateTimeString := FormatDateTime(TDBIUnitTest.GetDateTimeFormat, DBIMaxDateTime);
  StrToDateTime(DateTimeString);

  // Validate the DBIZeroDateTime constant - should fail!
  try
    DateTimeString := FormatDateTime(TDBIUnitTest.GetDateTimeFormat, DBIZeroDateTime);
    StrToDateTime(DateTimeString);

    Fail('DBIZeroDateTime: Should never reach this point');
  except
    on ETestFailed do raise;
  else
    // Do Nothing
  end;


  DateTimeValue := DBISignifyNullDateTime;
  DateTimeVariant := DateTimeValue;

  if not (DateTimeVariant = DateTimeValue) then begin
    raise Exception.Create('Comparison Failed');
  end;
end;


procedure TDBIODSUnitTests.DefaultStringAttributes;
const
  TableName = 'DefaultStringAttributes.xml';
  
var
  ODS: TObjectListDataset;

begin
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.Options := [{Clear Options}];
    ODS.ClassTypename := TStringData.ClassName;
    ODS.StringFieldSize := 32;

    TStringData.CreateFieldDefs(ODS);
    ODS.Open;

    TStringData.OccupyValues(ODS);
    TStringData.AssertValues(ODS);

    ODS.Close;

    ODS.Open;
    TStringData.AssertValues(ODS);
    ODS.SaveToFile(DataPath(TableName));
    ODS.Close;

  finally
    ODS.Free;
  end;


  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypename := TStringData.ClassName;
    ODS.LoadFromFile(DataPath(TableName));
    TStringData.AssertValues(ODS);
    ODS.Close;

  finally
    ODS.Free;
  end;

end;


// _____________________________________________________________________________
{**
  Jvr - 30/01/2001 13:27:28.<P>
}
{$ifdef omTesting}
procedure TDBIODSUnitTests.Fail(msg: string; errorAddr: Pointer = nil);
begin
  if errorAddr = nil then
    raise ETestFailed.Create(msg)
  else
    raise ETestFailed.Create(msg) at errorAddr;
end;
{$endif}


procedure TDBIODSUnitTests.FieldDefs;
var
  ODS: TDBIObjectListDataset;
  FieldDef: TfieldDef;
  
begin
  ODS := TDBIObjectListDataset.Create(nil);
  try
    FieldDef := ODS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'FloatField';
    FieldDef.DataType := ftFloat;
    FieldDef.Size := 0;
    FieldDef.Precision := 0;

    ODS.CreateDataset;

    // After the CreateDataset Get the FielDef - These are recreated!
    FieldDef := ODS.FieldDefs[0];
    Assert(FieldDef.Size = 0, Format('FieldDef.Size = %d, Should be Zero', [FieldDef.Size]));
    Assert(FieldDef.Precision = 0, Format('FieldDef.Precision = %d, Should be Zero', [FieldDef.Precision]));

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


{$ifndef fpc}
procedure TDBIODSUnitTests.FieldPropsCDS;
const
  TableName = 'FieldPropsCDS.dbf';

var
  CDS: TDBIClientDataset;
  FieldData: TStringFields;

begin
  FieldData := TStringData.GetFields;

  // Create String Data CDS
  CDS := TDBIClientDataset.Create(nil);
  try
    TStringData.CreateFields(CDS);
    CDS.CreateDataset;
    TStringData.SetupDataset(CDS);
    TStringData.OccupyValues(CDS);

    TStringData.VerifyFields(CDS, @FieldData, Length(FieldData));
    TStringData.AssertValues(CDS);

    CDS.SaveToFile(DataPath(ChangeFileExt(TableName, '.cds')), dbclient.dfXML);
    CDS.Close;
  finally
    CDS.Free;
  end;

  // Create String Data CDS
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.loadFromFile(DataPath(ChangeFileExt(TableName, '.cds')));
    TStringData.VerifyFields(CDS, @FieldData, Length(FieldData));
    TStringData.AssertValues(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
end;
{$endif}

procedure TDBIODSUnitTests.FieldPropsODS;
const
  TableName = 'FieldPropsODS.dbf';

var
{$ifndef fpc}
  CDS: TDBIClientDataset;
{$endif}
  ODS: TDBIObjectListDataset;
  FieldData: TStringFields;

begin
  FieldData := TStringData.GetFields;

  // Create new Objectlist Dattaset
  ODS := TDBIObjectListDataset.Create(nil);
  try
    TStringData.SetupDataset(ODS);

    // Build the FieldDefs from the defined TFields
    TStringData.CreateFields(ODS);
    ODS.CreateDataset;

    TStringData.OccupyValues(ODS);
    TStringData.VerifyFields(ODS, @FieldData, Length(FieldData));
    TStringData.AssertValues(ODS);

    ODS.SaveToFile(DataPath(ChangeFileExt(TableName, '.cds')), dfXML);
    ODS.SaveToFile(DataPath(TableName));
    ODS.Close;

    // Use the previously created Fieldefs and fields when opening the dataset
    ODS.Open;
    TStringData.VerifyFields(ODS, @FieldData, Length(FieldData));
    TStringData.AssertValues(ODS);
    ODS.Close;
  finally
    ODS.Free;
  end;


  // Create new Objectlist Dattaset
  ODS := TDBIObjectListDataset.Create(nil);
  try
    TStringData.SetupDataset(ODS);

    // Build the FieldDefs from the defined TFields
    TStringData.CreateFieldDefs(ODS);

    // Load data from saved file
    ODS.LoadFromFile(DataPath(TableName));

    TStringData.VerifyFields(ODS, @FieldData, Length(FieldData));
    TStringData.AssertValues(ODS);
    ODS.Close;

    // Test Zero length String Fields
    try
      TStringData.StringFields(ODS);
      Fail('Zero length fields: Should never reach this point');

    except
      on ETestFailed do raise;
    else
      // Do Nothing
    end;

    ODS.Close;

    // We clear both Fieldefs and Fields to allow them to be rebuilt from
    // the published properties of the class specifoied in
    // the TObjectListDataset ClassTypeName property.
    // The Size attribute for string fields will be set to the value specified in
    // the TObjectListDataset StringFieldSize property.
    ODS.FieldDefs.Clear;
    ODS.Fields.Clear;
    ODS.Open;
//##JVR    TStringData.VerifyFields(ODS, @FieldData, Length(FieldData));
    TStringData.CheckFields(ODS, [fcFieldName, fcFieldKind, fcFieldSize, fcPrecision, fcReadOnly, fcSkipFieldSizeString]);
    TStringData.AssertValues(ODS);
    ODS.Close;

  finally
    ODS.Free;
  end;

  // Create new Client Dattaset
{$ifndef fpc}
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.loadFromFile(DataPath(ChangeFileExt(TableName, '.cds')));
    TStringData.VerifyFields(CDS, @FieldData, Length(FieldData));
    TStringData.AssertValues(CDS);

    // Create new Objectlist Dattaset
    ODS := TDBIObjectListDataset.Create(nil);
    try
      TStringData.SetupDataset(ODS);
      TStringData.CreateFields(ODS);

      // Load data from ClientDataset
      ODS.loadFromDataset(CDS, [lmCreateDataset]);
      TStringData.VerifyFields(ODS, @FieldData, Length(FieldData));
      TStringData.AssertValues(ODS);

      CompareFieldProps(CDS, ODS);

      ODS.Close;
    finally
      ODS.Free;
    end;

    CDS.Close;
  finally
    CDS.Free;
  end;
{$endif}
end;


type
  TFilteredListDataset = class(TObjectListDataset)
  protected
    procedure DoFilterRecord(DataSet: TDataSet; var Accept: Boolean);
  end;

procedure TFilteredListDataset.DoFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := Dataset.FieldByName('Rating').AsInteger <= 5;
end;


procedure TDBIODSUnitTests.Filters;
const
  TableName = 'oFilters.dbf';

var
  ODS: TFilteredListDataset;
  Index: Integer;
  UpdatedBooks: TBookRecords;

begin
  UpdatedBooks := TBookData.GetUpdateRecords;

  TBookData.DeleteTables(DataPath(TableName));
  TBookData.ODSUpdateTable(DataPath(TableName));

  ODS := TFilteredListDataset.Create(nil);
  try
    ODS.Close;
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.OnFilterRecord := ODS.DoFilterRecord;
    ODS.LoadFromFile(DataPath(TableName));

    Assert(ODS.RecordCount = Length(TBookData.GetRecords));
    ODS.Filtered := True;

    Assert(ODS.RecordCount = Length(TBookData.GetRatingFilter));
    for Index := 0 to ODS.RecordCount-1 do begin
      Assert(ODS.FieldByName('Sequence').AsInteger = UpdatedBooks[TBookData.GetRatingFilter[Index]].Sequence);
      Assert(ODS.FieldByName('Rating').AsInteger = UpdatedBooks[TBookData.GetRatingFilter[Index]].Rating);
      ODS.Next;
    end;

    ODS.Close;
  finally
    ODS.Free;
  end;
end;



procedure TDBIODSUnitTests.FloatTypes;
const
  TableName = 'oFloatTypes.dbf';
begin
  TFloatData.DeleteTables(DataPath(TableName));
  TFloatData.ODSCreateTable(DataPath(TableName));
end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 14:55:05.<P>
}
procedure TDBIODSUnitTests.Indices;
const
  TableName = 'oIndice.dbf';

var
{$ifndef fpc}
  CDS: TDBIClientDataset;
{$endif}
  ODS: TDBIObjectListDataset;

begin
  TBookData.DeleteTables(DataPath(TableName));
  TBookData.ODSUpdateTable(DataPath(TableName));

{$ifndef fpc}
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.LoadFromFile(DataPath(ChangeFileExt(TableName, '.cds')));
    TBookData.AddIndexDefs(CDS);

    TBookData.CheckAscending(CDS, 'NameOrder');
    TBookData.CheckAscending(CDS, 'NameReverse');
    TBookData.CheckAscending(CDS, 'RatingOrder');
    TBookData.CheckAscending(CDS, 'RatingReverse');

    CDS.Close;

    CDS.Fields.Clear;
    CDS.FieldDefs.Clear;
    CDS.LoadFromFile(DataPath(ChangeFileExt(TableName, '.cds')));

    TBookData.CheckAscending(CDS, 'NameOrder');
    TBookData.CheckAscending(CDS, 'NameReverse');
    TBookData.CheckAscending(CDS, 'RatingOrder');
    TBookData.CheckAscending(CDS, 'RatingReverse');

    CDS.Close;
  finally
    CDS.Free;
  end;
{$endif}


  ODS := TDBIObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.LoadFromFile(DataPath(TableName));
    TBookData.AddIndexDefs(ODS);

    TBookData.CheckAscending(ODS, 'NameOrder');
    TBookData.CheckAscending(ODS, 'NameReverse');
    TBookData.CheckAscending(ODS, 'RatingOrder');
    TBookData.CheckAscending(ODS, 'RatingReverse');

    ODS.Close;
    ODS.CreateDataset;

    TBookData.CheckAscending(ODS, 'NameOrder');
    TBookData.CheckAscending(ODS, 'NameReverse');
    TBookData.CheckAscending(ODS, 'RatingOrder');
    TBookData.CheckAscending(ODS, 'RatingReverse');

    ODS.Close;

    ODS.List.Clear;
    ODS.Fields.Clear;
    ODS.FieldDefs.Clear;
    ODS.LoadFromFile(DataPath(TableName));

    TBookData.CheckAscending(ODS, 'NameOrder');
    TBookData.CheckAscending(ODS, 'NameReverse');
    TBookData.CheckAscending(ODS, 'RatingOrder');
    TBookData.CheckAscending(ODS, 'RatingReverse');

    ODS.Close;
  finally
    ODS.Free;
  end;


  ODS := TDBIObjectListDataset.Create(nil);
  try
    ODS.Close;
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.LoadFromFile(DataPath(TableName));

    TBookData.AddIndexes(ODS);

    // Ascending Order String
    TBookData.CheckAscending(ODS, 'NameOrder');

    // Descending Order String
    TBookData.CheckDescending(ODS, 'NameOrder');

    // Descending Order String
    TBookData.CheckAscending(ODS, 'NameReverse');

    // Ascending Order String
    TBookData.CheckDescending(ODS, 'NameReverse');

    // Ascending Order Integer
    TBookData.CheckAscending(ODS, 'RatingOrder');

    // Descending Order Integer
    TBookData.CheckDescending(ODS, 'RatingOrder');

    // Descending Order Integer
    TBookData.CheckAscending(ODS, 'RatingReverse');

    // Ascending Order Integer
    TBookData.CheckDescending(ODS, 'RatingReverse');

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


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
    Assert(XDS.RecordCount = Length(TBookData.GetRecords));

    // Verify Data
    TBookData.AssertValues(XDS);

    // Load Data
    ODS := TObjectListDataset.Create(nil);
    try
      ODS.ClassTypeName := TBookData.ClassName;
      ODS.LoadFromDataset(XDS, [lmCreateDataset]);

      // Verify Data
      TBookData.AssertValues(ODS);

      ODS.Close;
    finally
      ODS.Free;
    end;

    XDS.Close;
  finally
    XDS.Free;
  end;
end;


procedure TDBIODSUnitTests.LoadFromFile;
const
  TableName = 'oLoadFromFile.dbf';

var
  ODS: TObjectListDataset;

begin
  // Delete the Table if it exists
  TBookData.DeleteTables(DataPath(TableName));

  // Create new table
  TBookData.ODSCreateTable(DataPath(TableName));

  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    TBookData.CreateFieldDefs(ODS);

    // Verify Data was written correctly to file
    ODS.List.Clear;
    ODS.LoadFromFile(DataPath(TableName));
    TBookData.AssertValues(ODS);

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
const
  Tablename = 'oLocate.dbf';

var
  ODS: TObjectListDataset;
  Gad: TGadData;
  Value: Integer;

begin
  TGadData.DeleteTables(DataPath(TableName));
  TGadData.ODSCreateTable(DataPath(TableName));

  ODS := TObjectListDataset.Create(nil);
  try
    ODS.Close;
    ODS.ClassTypeName := TGadData.Classname;
    ODS.LoadFromFile(DataPath(TableName));

    Assert(ODS.RecordCount = Length(TGadData.GetRecords));
    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([47, 'M', 4.75]), []));
    Value := ODS.FieldByName('Value').AsInteger;
    Assert(Value = 61);
    Assert(ODS.FieldByName('Value').AsInteger = 61);
    Gad := ODS.List[ODS.RecNo-1] as TGadData;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([26, 'F', 3.50]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 40);
    Gad := ODS.List[ODS.RecNo-1] as TGadData;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([35, 'M', 7.00]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 72);
    Gad := ODS.List[ODS.RecNo-1] as TGadData;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([63, 'F', 10.00]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 116);
    Gad := ODS.List[ODS.RecNo-1] as TGadData;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([52, 'M', 9.75]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 106);
    Gad := ODS.List[ODS.RecNo-1] as TGadData;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([74, 'F', 6.25]), []));
    Assert(ODS.FieldByName('Value').AsInteger = 116);
    Gad := ODS.List[ODS.RecNo-1] as TGadData;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);

    Assert(ODS.Locate('Status', '', []));
    Assert(ODS.FieldByName('Status').AsString = '');
    Assert(ODS.FieldByName('Age').AsInteger = 52);
    Assert(ODS.FieldByName('Value').AsInteger = 102);
    Assert(ODS.FieldByName('Gender').AsString = 'F');

    Gad := ODS.List[ODS.RecNo-1] as TGadData;
    Assert(ODS.FieldByName('Value').AsInteger = Gad.Value);


    // Indexed Locate
    ODS.AddIndex('KeyStatus', 'Status', []);
    ODS.IndexName := 'KeyStatus';
    ODS.Last;

  { Age: 47; Gender: 'M'; YieldRate: 4.75; Value: 61; Status: 'ABC'; Created: '01/01/1900 01:01:01'; }

    Assert(ODS.Locate('Age;Gender;YieldRate', VarArrayOf([47, 'M', 4.75]), []));
    Assert(ODS.FieldByName('Age').AsInteger = 47);
    Assert(ODS.FieldByName('Gender').AsString = 'M');
    Assert(ODS.FieldByName('Status').AsString = 'ABC');

    Value := ODS.FieldByName('Value').AsInteger;
    Assert(Value = 61);
    Assert(ODS.FieldByName('Value').AsInteger = 61);


    Assert(ODS.Locate('Status', '', []));
    Assert(ODS.FieldByName('Status').AsString = '');
    Assert(ODS.FieldByName('Age').AsInteger = 52);
    Assert(ODS.FieldByName('Value').AsInteger = 102);
    Assert(ODS.FieldByName('Gender').AsString = 'F');


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
const
  TableName = 'oMemoryStreams.dbf';

var
  DataStream: TMemoryStream;
  Gad: TObjectListDataset;
  Clone: TObjectlistDataset;

begin
  TGadData.DeleteTables(DataPath(TableName));
  TGadData.ODSCreateTable(DataPath(TableName));

  Gad := TObjectListDataset.Create(nil);
  try
    Clone := TObjectListDataset.Create(nil);
    try
      Gad.ClassTypeName := TGadData.ClassName;
      Gad.LoadFromFile(DataPath(TableName));

      DataStream := TMemoryStream.Create;
      try
        Gad.SaveToStream(DataStream);

        Clone.ClassTypeName := TGadData.ClassName;
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
    end;
  finally
    Gad.Free;
  end;
end;


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
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 30/01/2001 10:53:38.<P>
}
procedure TDBIODSUnitTests.NullFlags;
const
  TestString = 'F1974';

var
  ODS: TObjectListDataset;

begin
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.Close;
    ODS.ClassTypeName := 'TObjectListData';

    ODS.Open;  //##Jvr - 22/05/2002 14:15:38 CreateDataSet;
    ODS.AppendRecord([1, null, 'TestData']);

    // Assign some data
    ODS.Edit;
    ODS.FieldByName('Name').AsString := TestString;
    Assert(ODS.FieldByName('Name').AsString = TestString);

    ODS.Post;
    Assert(ODS.FieldByName('Name').AsString = TestString);

    // Now set it to null
    ODS.Edit;
    ODS.FieldByName('Name').Clear;
    ODS.Post;
    Assert(ODS.FieldByName('Name').IsNull);

    // Ok now reassign the data
    ODS.Edit;
    ODS.FieldByName('Name').AsString := TestString;
    Assert(ODS.FieldByName('Name').AsString = TestString);

    ODS.Post;
    Assert(ODS.FieldByName('Name').AsString = TestString);

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 16:10:38.<P>
}
procedure TDBIODSUnitTests.ObjectDataEvents;
type
  PNumRec = ^TNumRec;
  TNumRec = record
    Sequence: Integer;
    CategoryName: TDBIString;
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
      CompareText(ODS.FieldByName('CategoryName').AsString , String(PItem^.CategoryName)) = 0
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
        (ODS.DataObject as TBookCategory).Sequence
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
end;


procedure TDBIODSUnitTests.ObjectTypes;
const
  TableName = 'oCreateEntity.xml';

begin
  // Run Object Tests
  TEntityData.ODSCreateEntity(DataPath(TableName));
end;


procedure TDBIODSUnitTests.OrdinalTypes;
const
  TableName = 'oOrdinalTypes.dbf';
begin
  TOrdinalData.DeleteTables(DataPath(TableName));
  TOrdinalData.ODSCreateTable(DataPath(TableName));
end;


// _____________________________________________________________________________
{**
  Jvr - 30/01/2001 12:54:28.<P>
}
procedure TDBIODSUnitTests.ReadOnlyProperty;
const
  TableName = 'oReadOnlyProperty.dbf';

var
  ODS: TObjectListDataset;
  FieldDef: TFieldDef;

begin
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.Close;
    ODS.Options := [];
    ODS.ClassTypeName := 'TReadOnlyData';
    ODS.FieldDefs.Clear;

    FieldDef := ODS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'Sequence';
    FieldDef.DataType := ftInteger;
    FieldDef.Required := True;

    FieldDef := ODS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'Name';
    FieldDef.DataType := ftString;
    FieldDef.Size := 20;

    FieldDef := ODS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'DoNotSet';
    FieldDef.DataType := ftString;
    FieldDef.Size := 10;

    ODS.CreateDataSet;
    ODS.AppendRecord([1, null, 'TestData1']);
    ODS.AppendRecord([2, null, 'TestData2']);
    ODS.AppendRecord([3, null, 'TestData3']);

    ODS.SaveToFile(DataPath(TableName));

    ODS.Close;
  finally
    ODS.Free;
  end;


  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := 'TReadOnlyData';
    ODS.Options := [osErrorOnReadonlyProperty];

    try
      ODS.LoadFromFile(DataPath(TableName));

    except
      on ETestFailed do raise;
    else
      // Do Nothing
    end;

    ODS.Close;
  finally
    ODS.Free;
  end;


  ODS := TObjectListDataset.Create(nil);
  try
    ODS.Close;
    ODS.Options := [osErrorOnReadonlyProperty];
    ODS.ClassTypeName := 'TReadOnlyData';
    ODS.FieldDefs.Clear;

    FieldDef := ODS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'Sequence';
    FieldDef.DataType := ftInteger;
    FieldDef.Required := True;

    FieldDef := ODS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'Name';
    FieldDef.DataType := ftString;
    FieldDef.Size := 20;

    FieldDef := ODS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'DoNotSet';
    FieldDef.DataType := ftString;
    FieldDef.Size := 10;

    ODS.CreateDataSet;
    try
      ODS.AppendRecord([1, null, 'TestData']);
      Fail('Should never reach this point');

    except
      on ETestFailed do raise;
    else
      // Do Nothing
    end;

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


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
end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 12:40:34.<P>
}
procedure TDBIODSUnitTests.SaveAsCDS;
const
  TableName = 'oSaveAsCDS.dbf';

var
{$ifndef fpc}
  CDS: TDBIClientDataSet;
{$endif}
  ODS: TObjectListDataset;

begin
  TBookData.ODSUpdateTable(DataPath(TableName));

  // Open Dataset
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    ODS.LoadFromFile(DataPath(TableName));

    TBookData.ReviseFields(ODS);

    ODS.SaveToFile(DataPath(ChangeFileExt(TableName, '.cds')), dfXML);
    ODS.Close;

    Assert(SysUtils.FileExists(DataPath(ChangeFileExt(TableName, '.cds'))));
  finally
    ODS.Free;
  end;


  // Verify the data that was saved to the ".cds' is valid
  // Open Dataset
{$ifndef fpc}
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.LoadFromFile(DataPath(ChangeFileExt(TableName, '.cds')));

    // Verify Data
    TBookData.ReviseFields(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
{$endif}
end;


procedure TDBIODSUnitTests.Setup;
begin
  inherited Setup;

{$ifndef Delphi2007}
  ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$else}
  FormatSettings.ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$endif}
end;


// _____________________________________________________________________________
{**
  Jvr - 19/11/2014 10:24:01.<P>
}
type
  TODSStreamingComponent = class(TDBIPersistenceAdapter)
  private
    FODS: TDBIObjectListDataset;
  public
    function GetRootComponent: TComponent; override;
  end;

  function TODSStreamingComponent.GetRootComponent: TComponent;
  begin
    if not Assigned(FODS) then begin
      FODS := TDBIObjectListDataset.Create(Self);
    end;
    Result := FODS;
  end;

procedure TDBIODSUnitTests.Streaming;
const
  TableName = 'oStreamBooks.dbf';

var
  Adapter: TODSStreamingComponent;
  ODS: TDBIObjectListDataset;

begin
  Adapter := TODSStreamingComponent.Create(nil);
  try
    // Create a new Dataset
    TBookData.ODSCreateTable(DataPath(TableName));

    ODS := Adapter.RootComponent as TDBIObjectListDataset;
    ODS.Name := ChangeFileExt(TableName, '');
    ODS.ClassTypeName := TBookData.ClassName;

    TBookData.CreateFieldDefs(ODS);

    // Verify Data was written correctly to file
    ODS.List.Clear;
    ODS.LoadFromFile(DataPath(TableName));
    ODS.SaveToFile(DataPath(ChangeFileExt(TableName, '.xml')));

    TBookData.CheckFields(ODS, [fcFieldName, fcFieldKind, fcFieldType, fcFieldSize, fcPrecision, fcRequired, fcReadOnly, fcXmlMapFieldTypes]);
    TBookData.AssertValues(ODS);

    // Stream the Dataset out to File
    Adapter.SaveToFile(DataPath(ChangeFileExt(TableName, '.dfm')));
    ODS.Close;

  finally
    FreeAndNil(Adapter);
  end;

  ODS := TDBIObjectListDataset.Create(nil);
  try
    ODS.Name := ChangeFileExt(TableName, '');
    ODS.ClassTypeName := TBookData.ClassName;

    // Verify Data was written correctly to file
    ODS.List.Clear;
    ODS.LoadFromFile(DataPath(ChangeFileExt(TableName, '.xml')));

//##JVR    TBookData.VerifyFields(ODS, @FieldData, Length(FieldData));
    TBookData.CheckFields(ODS, [fcFieldName, fcFieldKind, fcFieldType, fcFieldSize, fcPrecision, fcRequired, fcReadOnly, fcXmlMapFieldTypes]);
    TBookData.AssertValues(ODS);
  finally
    FreeAndNil(ODS);
  end;

  Adapter := TODSStreamingComponent.Create(nil);
  try
    // Stream the Dataset Back in
    Adapter.LoadFromFile(DataPath(ChangeFileExt(TableName, '.dfm')));
    ODS := Adapter.RootComponent as TDBIObjectListDataset;
    ODS.SaveToFile(DataPath(ChangeFileExt(TableName, '.2.xml')));

//##JVR    TBookData.VerifyFields(ODS, @FieldData, Length(FieldData));
    TBookData.CheckFields(ODS, [fcFieldName, fcFieldKind, fcFieldType, fcFieldSize, fcPrecision, fcRequired, fcReadOnly, fcXmlMapFieldTypes]);
    TBookData.AssertValues(ODS);
  finally
    FreeAndNil(Adapter);
  end;
end;


procedure TDBIODSUnitTests.Teardown;
begin

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


// _____________________________________________________________________________
{**
  Jvr - 28/11/2002 10:34:37.<P>
}
procedure TDBIODSUnitTests.Validation;
const
  GenderMap: array[Boolean] of TDBIString = ('M', 'F');

var
  ODS: TObjectListDataset;
  Gad: TGadData;
  Index: Integer;

begin
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TGadData.ClassName;
    ODS.StringFieldSize := 10;

    // Enable Validation
    ODS.Options := [osObjectValidation];

    // Disable validation procedure
    ODS.ObjectValidationProc := '';

    for Index := 1 to 100 do begin
      Gad := TGadData.Create;
      Gad.Age := Index;
      Gad.Gender := GenderMap[(Index mod 2) = 0];
      ODS.List.Add(Gad);
    end;

    ODS.Open;

    ODS.First;
    ODS.Next;
    ODS.Next;

    // First Test valid data
    ODS.Edit;
    ODS.FieldByName('Gender').AsString := 'F';
    ODS.Post;

    // Now test invalid data
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

  
  // Test Validation proc
  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TGadData.ClassName;
    ODS.StringFieldSize := 10;

    // Enable Validation procedure - is called prior to post
    ODS.ObjectValidationProc := 'DoValidation';

    for Index := 1 to 100 do begin
      Gad := TGadData.Create;
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
      ODS.FieldByName('Age').AsInteger := 101;
      ODS.Post;

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
end;





initialization
  Classes.RegisterClass(TObjectListData);
  Classes.RegisterClass(TReadOnlyData);
  Classes.RegisterClass(TReference);
  Classes.RegisterClass(TParentObject);

{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBIODSUnitTests);
{$else}
  RegisterTest('', TDBIODSUnitTests.Suite);
{$endif}

end.
