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
  1.0 | 25/01/2001 12:08:30 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : native api code}

unit DBIXDSUnitTests;

interface

{$I DBICompilers.inc}

uses
  DBIXbaseDatasets,
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
  DBIXbaseDataConnections, DBIDataset, DBIUnitTests;

type
  TDBIXDSUnitTests = class(TDBIUnitTests)
  protected
    procedure LoadFromXbaseFile;

    procedure Setup; override;
    procedure Teardown; override;

  published
    procedure CreateBooks;
{$ifndef fpc}
    procedure CreateBooksCDS;
    procedure CreateDataSetCDS;
{$endif}
    procedure CreateGad;
    procedure CreateMemoryDataset;
    procedure DateTimes;
    procedure FloatTypes;
    procedure Indices;
    procedure LoadAndSave;
    procedure LoadFromDataset;
    procedure Locate;
    procedure NullValues;
    procedure Numeric;
    procedure OrdinalTypes;
    procedure SaveAsCDS;
    procedure TestCloseAndDelete;
    procedure UpdateBooks;
  end;


implementation

{$ifdef omTesting}
  // Avoid the deprecation warning from TClientDataset.LoadFromFile in OM's patched D2006 DBClient.pas
  {$WARN SYMBOL_DEPRECATED OFF}
{$endif}

uses
{$ifdef DELPHI6}
  Variants,
{$endif}
  SysUtils, Windows, Forms, Dialogs, DB, DBIConst, DBIUtils, DBIObjectListDatasets;



// =============================================================================
// Testing starts here
// =============================================================================


// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 17:07:47.<P>
}
procedure TDBIXDSUnitTests.CreateBooks;
const
  TableName = 'xCreateBooks.dbf';

var
  XDS: TXbaseDataset;

begin
  TBookData.DeleteTables(DataPath(TableName));

  // Create a new Dataset
  TBookData.XDSCreateTable(DataPath(TableName));

  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(DataPath(TableName));

    TBookData.AssertValues(XDS);
    TBookData.ClearValues(XDS);
    TBookData.AssertBlanks(XDS);

    TBookData.FieldValues(XDS);

    XDS.SaveToFile(DataPath('xBlankBooks.dbf'));
    XDS.Close;
  finally
    XDS.Free;
  end;
end;


{$ifndef fpc}
procedure TDBIXDSUnitTests.CreateBooksCDS;
const
  TableName = 'cCreateBooks.xml';

begin
  TBookData.DeleteTables(DataPath(TableName));

  // Create a new Dataset
  TBookData.CDSCreateTable(DataPath(TableName));
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 12/05/2005 19:08:30 - This is here to debug a CDS.<br>
}
{$ifndef fpc}
procedure TDBIXDSUnitTests.CreateDataSetCDS;
const
  cdsXDSDbug = 'XDSDbug.cds';

var
  CDS: TDBIClientDataset;
  Index: Integer;
  FieldDef: TFieldDef;
  BookFields: TBookFields;
  NewBooks: TBookRecords;

begin
  NewBooks := TBookData.GetRecords;
  BookFields := TBookData.GetFields;

  // Delete the Datafile if it exists
  SysUtils.DeleteFile(DataPath(cdsXDSDbug));
  Assert(not SysUtils.FileExists(DataPath(cdsXDSDbug)));

  // Create a new Dataset
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.FileName := DataPath(cdsXDSDbug);
    CDS.FieldDefs.Clear;

    for Index := Low(BookFields) to High(BookFields) do begin
      FieldDef := CDS.FieldDefs.AddFieldDef;
      FieldDef.Name := UpperCase(BookFields[Index].FieldName);
      FieldDef.DataType := BookFields[Index].FieldType;

      if (BookFields[Index].FieldSize > 0) then begin
        FieldDef.Size := BookFields[Index].FieldSize;
      end;

      if (BookFields[Index].Precision > 0) then begin
        FieldDef.Precision := BookFields[Index].Precision;
      end;
    end;  { for }

    CDS.CreateDataset;

    // Add Data
    for Index := Low(NewBooks) to High(NewBooks) do begin
      CDS.AppendRecord([
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

      Assert(CDS.RecNo = (Index + 1));
      Assert(CDS.FieldByName('Sequence').AsInteger = NewBooks[Index].Sequence);
      Equalz(CDS.FieldByName('Name').AsString , NewBooks[Index].Name);
      Equalz(CDS.FieldByName('Author').AsString , NewBooks[Index].Author);
      Equalz(
        FormatDateTime(TBookData.GetDateTimeFormat, CDS.FieldByName('Purchased').AsDateTime),
        NewBooks[Index].Purchased
        );
      Assert(CDS.FieldByName('Price').AsFloat = NewBooks[Index].Price);
      Equalz(CDS.FieldByName('Currency').AsString , NewBooks[Index].Currency);
      Assert(CDS.FieldByName('Rating').AsInteger = NewBooks[Index].Rating);
      Assert(CDS.FieldByName('Approved').AsBoolean = NewBooks[Index].Approved);
      Equalz(CDS.FieldByName('Comments').AsString , NewBooks[Index].Comments);
      Equalz(CDS.FieldByName('Notes').AsString , NewBooks[Index].Notes);
      Equalz(CDS.FieldByName('Details').AsString , NewBooks[Index].Details);
    end;

    Assert(CDS.RecordCount = Length(NewBooks));

    CDS.Close;
  finally
    CDS.Free;
  end;
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 06/02/2001 18:15:25.<P>
}
procedure TDBIXDSUnitTests.CreateGad;
const
  TableName = 'xCreateGad.dbf';

begin
  TGadData.DeleteTables(DataPath(TableName));

  TGadData.XDSCreateTable(DataPath(TableName));
end;


// _____________________________________________________________________________
{**
  Jvr - 01/07/2005 11:12:26 - Initial code.<br>
}
procedure TDBIXDSUnitTests.CreateMemoryDataSet;
const
  dbfXDSMem = 'XDSMem.dbf';
  fptXDSMem = 'XDSMem.fpt';

var
  XDS: TXbaseDataset;
  Index: Integer;
  FieldDef: TFieldDef;
  BookFields: TBookFields;
  NewBooks: TBookRecords;
begin
  NewBooks := TBookData.GetRecords;
  BookFields := TBookData.GetFields;
  
  // Delete the Datafile if it exists
  SysUtils.DeleteFile(DataPath(dbfXDSMem));
  Assert(not SysUtils.FileExists(DataPath(dbfXDSMem)));

  // Delete the Blobfile if it exists
  SysUtils.DeleteFile(DataPath(fptXDSMem));
  Assert(not SysUtils.FileExists(DataPath(fptXDSMem)));

  // Create a new Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FieldDefs.Clear;

    for Index := Low(TBookFields) to High(TBookFields) do begin
      FieldDef := XDS.FieldDefs.AddFieldDef;
      FieldDef.Name := UpperCase(BookFields[Index].FieldName);
      FieldDef.DataType := BookFields[Index].FieldType;

      if (BookFields[Index].FieldSize > 0) then begin
        FieldDef.Size := BookFields[Index].FieldSize;
      end;

      if (BookFields[Index].Precision > 0) then begin
        FieldDef.Precision := BookFields[Index].Precision;
      end;
    end;

    XDS.CreateDataset;

    // Add Data
    for Index := Low(NewBooks) to High(NewBooks) do begin
      XDS.AppendRecord([
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

      Assert(XDS.RecNo = (Index + 1));
      Assert(XDS.FieldByName('Sequence').AsInteger = NewBooks[Index].Sequence);
      Equalz(XDS.FieldByName('Name').AsString , NewBooks[Index].Name);
      Equalz(XDS.FieldByName('Author').AsString , NewBooks[Index].Author);
      Equalz(
        FormatDateTime(TBookData.GetDateTimeFormat, XDS.FieldByName('Purchased').AsDateTime),
        NewBooks[Index].Purchased
        );
      Assert(XDS.FieldByName('Price').AsFloat = NewBooks[Index].Price);
      Equalz(XDS.FieldByName('Currency').AsString , NewBooks[Index].Currency);
      Assert(XDS.FieldByName('Rating').AsInteger = NewBooks[Index].Rating);
      Assert(XDS.FieldByName('Approved').AsBoolean = NewBooks[Index].Approved);
      Equalz(XDS.FieldByName('Comments').AsString , NewBooks[Index].Comments);
      Equalz(XDS.FieldByName('Notes').AsString , NewBooks[Index].Notes);
      Equalz(XDS.FieldByName('Details').AsString , NewBooks[Index].Details);
    end;

    Assert(XDS.RecordCount = Length(NewBooks));

    XDS.Close;


    // AMemory Dataset should not lose it's data after closing;
    XDS.Open;
    Assert(XDS.RecordCount = Length(NewBooks));

    XDS.SaveToFile(DataPath(dbfXDSMem));
    Assert(SysUtils.FileExists(DataPath(dbfXDSMem)));
    Assert(SysUtils.FileExists(DataPath(fptXDSMem)));
  finally
    XDS.Free;
  end;
end;


procedure TDBIXDSUnitTests.FloatTypes;
const
  TableName = 'xFloatTypes.dbf';

begin
  TFloatData.DeleteTables(DataPath(TableName));
  TFloatData.XDSCreateTable(DataPath(TableName));
end;


// _____________________________________________________________________________
{**
  Jvr - 08/05/2001 18:22:32.<P>
}
procedure TDBIXDSUnitTests.DateTimes;
const
  TableName = 'xDateTimes.dbf';

var
  XDS: TXbaseDataset;
  DateTimeStamp: TTimeStamp;
  TimeTimeStamp: TTimeStamp;
  TimeStamp: TTimeStamp;
  CombinedTimeStamp: TTimeStamp;
  ResultTimeStamp: TTimeStamp;
  DateTimeValue: TDateTime;
  DateValue: TDateTime;
  Today: TDateTime;
  ID: Word;

begin
  TGadData.DeleteTables(DataPath(TableName));
  TGadData.XDSCreateTable(DataPath(TableName));

  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FileName := DataPath(TableName);
    XDS.Open;

    XDS.First;
    while not XDS.Eof do begin
      ID := XDS.FieldByName('ID').AsInteger;
      Today := XDS.FieldByName('Created').AsDateTime;
      TimeStamp := DateTimeToTimeStamp(Today);

      DateTimeValue := XDS.FieldByName('Date').AsDateTime;
      DateValue := Trunc(Today);
      Assert(DateValue = DateTimeValue, 'Dates do NOT match');

      DateTimeStamp := DateTimeToTimeStamp(XDS.FieldByName('Date').AsDateTime);
      DateTimeStamp.Time := 0;
      TimeTimeStamp :=  DateTimeToTimeStamp(XDS.FieldByName('Time').AsDateTime);
      TimeTimeStamp.Date := 0;

      // Combined Date and Time Fields
      CombinedTimeStamp.Date := DateTimeStamp.Date;
      CombinedTimeStamp.Time := TimeTimeStamp.Time;

      // Verify that they match
      Assert(
        TimeStamp.Date = DateTimeStamp.Date,
        Format('[%d] Date (%d) does NOT match (%d)', [ID, TimeStamp.Date, DateTimeStamp.Date])
        );
      Assert(
        TimeStamp.Time = TimeTimeStamp.Time,
        Format('[%d] Time (%d) does NOT match (%d)', [ID, TimeStamp.Time, TimeTimeStamp.Time])
        );

      Assert(
        CombinedTimeStamp.Date = TimeStamp.Date,
        Format('[%d] Combined Date (%d) does NOT match (%d)', [ID, TimeStamp.Date, TimeStamp.Date])
        );
      Assert(
        CombinedTimeStamp.Time = TimeStamp.Time,
        Format('[%d] Combined Time (%d) does NOT match (%d)', [ID, CombinedTimeStamp.Time, TimeStamp.Time])
        );

      // Now change it so that it doesn't match
      XDS.Edit;
      DateTimeValue := TimeStampToDateTime(DateTimeToTimeStamp(Now));
      XDS.FieldByName('Created').AsDateTime := DateTimeValue;
      XDS.Post;

      // Are they the same?
      TimeStamp := DateTimeToTimeStamp(XDS.FieldByName('Created').AsDateTime);
      ResultTimeStamp := DateTimeToTimeStamp(DateTimeValue);
      Assert(ResultTimeStamp.Date = TimeStamp.Date);
      Assert(Round(ResultTimeStamp.Time/1000) * 1000 = Round(TimeStamp.Time/1000) * 1000);

      // Ok if that works then change it back again
      XDS.Edit;
      XDS.FieldByName('Created').AsDateTime := Today;
      XDS.Post;

      // And test it again
      TimeStamp := DateTimeToTimeStamp(XDS.FieldByName('Created').AsDateTime);
      Assert(
        CombinedTimeStamp.Date = TimeStamp.Date,
        Format('[%d] Date (%d) does NOT match (%d)', [ID, CombinedTimeStamp.Date, TimeStamp.Date])
        );
      Assert(
        CombinedTimeStamp.Time = TimeStamp.Time,
        Format('[%d] Time (%d) does NOT match (%d)', [ID, CombinedTimeStamp.Time, TimeStamp.Time])
        );

      XDS.Next;
    end;
  finally
    XDS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 12:44:19 - Test created to find memory leaks.<P>
}
procedure TDBIXDSUnitTests.Indices;
const
  TableName = 'xIndices.dbf';

var
  XDS: TXbaseDataset;
  Index: Integer;
  UpdatedBooks: TBookRecords;
begin
  UpdatedBooks := TBookData.GetUpdateRecords;

  TBookData.DeleteTables(DataPath(TableName));
  TBookData.XDSCreateTable(DataPath(TableName));

  XDS := TXbaseDataset.Create(nil);
  try
    XDS.Close;
    XDS.FileName := DataPath(TableName);
    XDS.LoadFromFile;

    XDS.AddIndex('NameOrder', 'Name', [ixCaseInsensitive]);
    XDS.AddIndex('NameReverse', 'Name', [ixDescending, ixCaseInsensitive]);

    // Ascending Order
    XDS.IndexName := 'NameOrder';
    XDS.First;
    for Index := 0 to XDS.RecordCount-1 do begin
      Equalz(XDS.FieldByName('Name').AsString , UpdatedBooks[TBookData.GetNameAsc[Index]].Name);
      XDS.Next;
    end;

    // Descending Order
    XDS.IndexName := 'NameOrder' + ';Descending';
    XDS.First;
    for Index := 0 to XDS.RecordCount-1 do begin
      Equalz(XDS.FieldByName('Name').AsString , UpdatedBooks[TBookData.GetNameDesc[Index]].Name);
      XDS.Next;
    end;

    // Descending Order
    XDS.IndexName := 'NameReverse';
    XDS.First;
    for Index := 0 to XDS.RecordCount-1 do begin
      Equalz(XDS.FieldByName('Name').AsString , UpdatedBooks[TBookData.GetNameDesc[Index]].Name);
      XDS.Next;
    end;

    // Ascending Order
    XDS.IndexName := 'NameReverse' + ';Descending';
    XDS.First;
    for Index := 0 to XDS.RecordCount-1 do begin
      Equalz(XDS.FieldByName('Name').AsString , UpdatedBooks[TBookData.GetNameAsc[Index]].Name);
      XDS.Next;
    end;

    XDS.Close;
  finally
    XDS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 26/02/2001 16:32:35.<P>
}
procedure TDBIXDSUnitTests.LoadAndSave;
const
  TableName = 'xLoadAndSave.dbf';

var
  XDS: TXbaseDataset;

begin
  TBookData.DeleteTables(DataPath(TableName));

  TBookData.XDSCreateTable(DataPath(TableName));

  // Open Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FileName := DataPath(TableName);
    XDS.LoadFromFile;

    TBookData.UpdateValues(XDS);
    TBookData.ReviseFields(XDS);

    // Save the results to file
    XDS.SaveToFile(DataPath(TableName));
    XDS.Close;
  finally
    XDS.Free;
  end;


  // Now reload the updated results to test that the file was updated properly
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(DataPath(TableName));
    TBookData.ReviseFields(XDS);

    XDS.Close;
  finally
    XDS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Test the LoadFromDataset functionality.

  Jvr - 05/06/2001 12:13:11.<P>
}
procedure TDBIXDSUnitTests.LoadFromDataset;
const
  TableName = 'xLoadFromDataset.dbf';

var
  ODS: TObjectListDataset;
  XDS: TXbaseDataset;

begin
  TBookData.DeleteTables(DataPath(TableName));
  TBookData.XDSUpdateTable(DataPath(TableName));

  ODS := TObjectListDataset.Create(nil);
  try
    ODS.ClassTypeName := TBookData.ClassName;
    TBookData.CreateFieldDefs(ODS);
    ODS.LoadFromFile(DataPath(TableName));
    TBookData.ReviseFields(ODS);

    // Update Data to XDS using a Memory Stream
    XDS := TXbaseDataset.Create(nil);
    try
      XDS.LoadFromDataset(ODS, [lmCreateDataset]);
      TBookData.ReviseFields(XDS);

      XDS.SaveToFile(DataPath(TableName));
      XDS.Close;
    finally
      XDS.Free;
    end;


    // Update Data to XDS using a File Stream from previously saved xbase file
    XDS := TXbaseDataset.Create(nil);
    try
      XDS.LoadFromFile(DataPath(TableName));

      TBookData.ReviseFields(XDS);
      XDS.Close;
    finally
      XDS.Free;
    end;

    ODS.Close;
  finally
    ODS.Free;
  end;
end;


procedure TDBIXDSUnitTests.LoadFromXbaseFile;
const
  dbfJobQueue = 'JobQueue.dbf';

var
  TestTable: TXbaseDataset;

begin
  TestTable := TXbaseDataset.Create(nil);
  try
    TestTable.FileName := DataPath(dbfJobQueue);
    TestTable.Active := True;
    Assert(TestTable.RecordCount > 0);

    TestTable.Active := False;
    TestTable.LoadFromFile(DataPath(dbfJobQueue));
    TestTable.Active := True;
    Assert(TestTable.RecordCount > 0);

  finally
    TestTable.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 06/02/2001 17:11:40.<P>
}
procedure TDBIXDSUnitTests.Locate;
const
  TableName = 'xLocate.dbf';

var
  Gad: TXbaseDataset;

begin
  TGadData.DeleteTables(DataPath(TableName));
  TGadData.XDSCreateTable(DataPath(TableName));

  Gad := TXbaseDataset.Create(nil);
  try
    Gad.Close;
    Gad.FileName := DataPath(TableName);
    Gad.Open;

    Assert(Gad.RecordCount = Length(TGadData.GetRecords));
    Assert(Gad.Locate('Age;Gender;YieldRate', VarArrayOf([47, 'M', 4.75]), []));
    Assert(Gad.FieldByName('Value').AsInteger = 61);
    Assert(Gad.Locate('Age;Gender;YieldRate', VarArrayOf([26, 'F', 3.50]), []));
    Assert(Gad.FieldByName('Value').AsInteger = 40);
    Assert(Gad.Locate('Age;Gender;YieldRate', VarArrayOf([35, 'M', 7.00]), []));
    Assert(Gad.FieldByName('Value').AsInteger = 72);
    Assert(Gad.Locate('Age;Gender;YieldRate', VarArrayOf([63, 'F', 10.00]), []));
    Assert(Gad.FieldByName('Value').AsInteger = 116);
    Assert(Gad.Locate('Age;Gender;YieldRate', VarArrayOf([52, 'M', 9.75]), []));
    Assert(Gad.FieldByName('Value').AsInteger = 106);
    Assert(Gad.Locate('Age;Gender;YieldRate', VarArrayOf([74, 'F', 6.25]), []));
    Assert(Gad.FieldByName('Value').AsInteger = 116);

    Gad.Close;
  finally
    Gad.Free;
  end;
end;


procedure TDBIXDSUnitTests.NullValues;
const
  TableName = 'xNullValues.dbf';

var
  XDS: TXBaseDataset;
  FieldData: TOrdinalFields;

begin
  FieldData := TOrdinalData.GetFields;

  TOrdinalData.XDSCreateTable(DataPath(TableName));

  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FileName := DataPath(TableName);
    XDS.Open;

    TOrdinalData.VerifyFields(XDS, @FieldData, Length(FieldData));
    TOrdinalData.AssertValues(XDS);

    TOrdinalData.ClearValues(XDS);
    TOrdinalData.AssertBlanks(XDS);

    XDS.Close;
  finally
    XDS.Free;
  end;

  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FileName := DataPath(TableName);
    XDS.Open;

    TOrdinalData.AssertBlanks(XDS);

    TOrdinalData.RefillValues(XDS);
    TOrdinalData.AssertValues(XDS);

    XDS.Close;
  finally
    XDS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2001 15:07:48<P>
}
procedure TDBIXDSUnitTests.Numeric;
const
  TableName = 'xNumeric1.dbf';

var
  XDS: TDBIXbaseDataset;
  FieldData: TNumericFields;

begin
  FieldData := TNumericData.GetFields;

  XDS := TDBIXbaseDataset.Create(nil);
  try
    TNumericData.CreateFields(XDS);
    XDS.CreateDataset;
    TNumericData.VerifyFields(XDS, @FieldData, Length(FieldData));

    TNumericData.OccupyValues(XDS);
    TNumericData.AssertValues(XDS);

    XDS.SaveToFile(DataPath(TableName));
    XDS.Close;

    XDS.FileName := DataPath(TableName);
    XDS.Fields.Clear;
    XDS.FieldDefs.Clear;
    XDS.Open;

    TNumericData.VerifyFields(XDS, @FieldData, Length(FieldData));
    TNumericData.AssertValues(XDS);

    XDS.Close;
  finally
    XDS.Free;
  end;


  XDS := TDBIXbaseDataset.Create(nil);
  try
    TNumericData.CreateFields(XDS);
    XDS.FileName := DataPath(TableName);
    XDS.CreateDataset;
    XDS.Close;

    XDS.Open;

    TNumericData.VerifyFields(XDS, @FieldData, Length(FieldData));
    TNumericData.OccupyValues(XDS);
    TNumericData.AssertValues(XDS);

    XDS.Close;
  finally
    XDS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 14:25:53.<P>
}
procedure TDBIXDSUnitTests.OrdinalTypes;
const
  TableName = 'xOrdinalTypes.dbf';

begin
  TOrdinalData.DeleteTables(DataPath(TableName));
  TOrdinalData.XDSCreateTable(DataPath(TableName));
end;


// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 17:07:47.<P>
}
procedure TDBIXDSUnitTests.SaveAsCDS;
const
  TableName = 'xSaveAsCDS.dbf';

var
{$ifndef fpc}
  CDS: TDBIClientDataSet;
{$endif}
  XDS: TXbaseDataset;

begin
  TBookData.DeleteTables(DataPath(TableName));

  TBookData.XDSUpdateTable(DataPath(TableName));

  // Open Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(DataPath(TableName));

    TBookData.ReviseFields(XDS);

    XDS.SaveToFile(DataPath(ChangeFileExt(TableName, '.cds')), dfCDS);
    XDS.Close;

  finally
    XDS.Free;
  end;


  // Verify the data that was saved to the ".cds' is valid
  // Open Dataset
  Assert(SysUtils.FileExists(DataPath(ChangeFileExt(TableName, '.cds'))));

{$ifndef fpc}
  CDS := TDBIClientDataSet.Create(nil);
  try
    CDS.LoadFromFile(DataPath(ChangeFileExt(TableName, '.cds')));

    TBookData.ReviseFields(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
{$endif}
end;


procedure TDBIXDSUnitTests.Setup;
begin
  inherited;

{$ifndef Delphi2007}
  ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$else}
  FormatSettings.ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$endif}
end;


procedure TDBIXDSUnitTests.Teardown;
begin
  inherited;

end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 14:25:53.<P>
}
procedure TDBIXDSUnitTests.TestCloseAndDelete;
const
  TableName = 'xTestCloseAndDelete.dbf';

var
  XDS: TXbaseDataset;
  FieldDef: TFieldDef;

begin
  TDBIUnitTest.DeleteTables(DataPath(TableName));

  XDS := TXbaseDataset.Create(nil);
  try
    // Build the table from scratch
    XDS.FieldDefs.Clear;
    XDS.FileName := DataPath(TableName);

    FieldDef := XDS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'LRef';
    FieldDef.DataType := ftString;
    FieldDef.Size := 10;

    FieldDef := XDS.FieldDefs.AddFieldDef;
    FieldDef.Name := 'LTimeStamp';
    FieldDef.DataType := ftString;
    FieldDef.Size := 64;

    XDS.CreateDataset;
    XDS.Close;

    Assert(SysUtils.FileExists(XDS.FileName));
    Assert(SysUtils.DeleteFile(XDS.FileName));
  finally
    XDS.Free;
  end;
end;


procedure TDBIXDSUnitTests.UpdateBooks;
const
  TableName = 'xUpdateBooks.dbf';

var
  XDS: TXbaseDataset;

begin
  TBookData.DeleteTables(DataPath(TableName));

  TBookData.XDSUpdateTable(DataPath(TableName));

  // Open Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(DataPath(TableName));

    TBookData.UpdateValues(XDS);
    TBookData.ReviseFields(XDS);

    XDS.SaveToFile(DataPath(TableName));
    XDS.Close;
  finally
    XDS.Free;
  end;
end;



initialization
{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBIXDSUnitTests);
{$else}
  RegisterTest('', TDBIXDSUnitTests.Suite);
{$endif}

end.
