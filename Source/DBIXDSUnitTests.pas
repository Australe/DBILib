{ ______________________________________________________________________________

  <H5>Copyright</H5>
  ObjectMastery Pty Ltd<BR>
  Copyright (C) 2001, All rights reserved.<!--

  Project:       Common
  Files(s):      DBIXDSUnitTests.pas
  Classes:       ...
  Author:        John Vander Reest
  Purpose:       Test suites for the Xbase datasets

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 25/01/2001 12:08:30 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

{#omcodecop off : jvr : native api code}

unit DBIXDSUnitTests;

interface

{$I DBICompilers.inc}

uses
  DBIXbaseDatasets,
  DBIDataset,
{$ifdef omTesting}
  omTestSuites,
  omTestMastery,
{$else}
  TestFrameWork,
{$endif}
  DBIUnitTests
  ;

type
  TDBIXDSUnitTests = class(TDBIUnitTests)
  protected
    procedure CreateBooksTable(const AFileName: String);
    procedure CreateGadTable;
    procedure CreateBooksCDS;
    procedure UpdateBooksTable;

    procedure LoadFromXbaseFile;
    
    procedure Setup; override;
    procedure Teardown; override;

  published
    procedure CreateDataset;
    procedure CreateDataSetCDS;
    procedure CreateMemoryDataset;
    procedure DateTimes;
    procedure Indices;
    procedure LoadAndSave;
    procedure LoadFromDataset;
    procedure Locate;
    procedure SaveAsCDS;
    procedure TestCloseAndDelete;
    procedure UpdateDataset;
  end;


implementation


uses
{$ifdef DELPHI6}
  Variants,
{$endif}
  SysUtils,
  Windows,
  Forms,
  Dialogs,
  DB,
  DBClient,
  DBIConst,
  DBIUtils;


  
// =============================================================================
// Testing starts here
// =============================================================================


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 14:25:53.<P>
}
procedure TDBIXDSUnitTests.CreateBooksCDS;
var
  XDS: TXbaseDataset;
  CDS: TClientDataSet;
  Index: Integer;
  MemoData: String;

begin
  // Only create Books CDS if it doesn't exist!
  if SysUtils.FileExists(DataPath(cdsXBooksUpD8)) then begin
    Exit;
  end;

  UpdateBooksTable;

  // Open Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(DataPath(dbfXBooksUpD8));

    Assert(XDS.RecordCount = Length(NewBooks));

    XDS.SaveToFile(DataPath(cdsXBooksUpD8), dfCDS);
    XDS.Close;

  finally
    XDS.Free;
  end;  { try..finally }

  Assert(SysUtils.FileExists(DataPath(cdsXBooksUpD8)));


  // Verify the data that was saved to the ".cds' is valid
  // Open Dataset
  CDS := TClientDataset.Create(nil);
  try
    CDS.LoadFromFile(DataPath(cdsXBooksUpD8));
    Assert(CDS.RecordCount = Length(NewBooks));

    // Verify Data
    with CDS do begin
      First;
      for Index := Low(UpdateBooks) to High(UpdateBooks) do begin
        Assert(RecNo = (Index + 1));
        Assert(FieldByName('Sequence').AsInteger = UpdateBooks[Index].Sequence);
        Equalz(FieldByName('Name').AsString , UpdateBooks[Index].Name);
        Equalz(FieldByName('Author').AsString , UpdateBooks[Index].Author);
        Equalz(
          FormatDateTime(DateTimeFormat, FieldByName('Purchased').AsDateTime),
          UpdateBooks[Index].Purchased
          );
        Assert(FieldByName('Price').AsFloat = UpdateBooks[Index].Price);
        Equalz(FieldByName('Currency').AsString , UpdateBooks[Index].Currency);
        Assert(FieldByName('Rating').AsInteger = UpdateBooks[Index].Rating);
        Assert(FieldByName('Approved').AsBoolean = UpdateBooks[Index].Approved);
        Equalz(FieldByName('Comments').AsString , UpdateBooks[Index].Comments);

        // I need to do this because I believe ther is a bug in CDS
        MemoData := FieldByName('Notes').AsString;
        SetLength(MemoData, StrLen(PChar(MemoData)));
        Equalz(MemoData , UpdateBooks[Index].Notes);

        MemoData := FieldByName('Details').AsString;
        SetLength(MemoData, StrLen(PChar(MemoData)));
        Equalz(MemoData , UpdateBooks[Index].Details);

        Next;
      end;  { for }
    end;  { with }

    CDS.Close;
  finally
    CDS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 17:07:47.<P>
}
procedure TDBIXDSUnitTests.CreateBooksTable(const AFileName: String);
begin
  // Only create Books table if it doesn't exist!
  if not SysUtils.FileExists(AFileName) then begin
    TBookData.XbaseCreateTable(AFileName);
  end;
end;  { CreateBooksTable }


// _____________________________________________________________________________
{**
  Jvr - 06/02/2001 18:15:25.<P>
}
procedure TDBIXDSUnitTests.CreateGadTable;
var
  XDS: TXBaseDataset;
  Index: Integer;
  Today: TdateTime;

begin
  // Only create Gad table if it doesn't exist!
  if SysUtils.FileExists(DataPath(dbfXGad)) then begin
    Exit;
  end;

  // Create Gad Table
  XDS := TXBaseDataset.Create(nil);
  try
    TGad.CreateFields(XDS);
    XDS.FileName := DataPath(dbfXGad);
    XDS.CreateDataset;

    for Index := Low(GadData) to High(GadData) do begin
      Today := Now;

      XDS.Append;
      XDS.FieldByName('ID').AsInteger := Index;
      XDS.FieldByName('Age').AsInteger := GadData[Index].Age;
      XDS.FieldByName('Gender').AsString := String(GadData[Index].Gender);
      XDS.FieldByName('YieldRate').AsFloat := GadData[Index].YieldRate;
      XDS.FieldByName('Value').AsInteger := GadData[Index].Value;
      XDS.FieldByName('Created').AsDateTime := Today;
      XDS.FieldByName('Date').AsDateTime := Trunc(Today);
//{
      // We post the record here to force the Created datetime value
      // to be stored prior to proceeding.
      XDS.Post;
      XDS.Edit;
//}
      Today := XDS.FieldByName('Created').AsDateTime;
      XDS.FieldByName('Time').AsString := FormatDateTime('hh:nn:ss.zzz', Today);

      XDS.CheckBrowseMode;
      Sleep(5);
    end;


(*##JVR
    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'Age';
    Field.DataType := ftSmallInt;

    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'Gender';
    Field.DataType := ftString;
    Field.Size := 1;

    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'YieldRate';
    Field.DataType := ftFloat;
{##JVR
    Field.DataType := ftBCD;
    Field.Size := 14;
    Field.Precision := 4;
//}
    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'Value';
    Field.DataType := ftSmallInt;
{##JVR
    ODS.FieldDefs.Add('Gender', ftString, 1, False);
    ODS.FieldDefs.Add('YieldRate', ftFloat, 2, False);
    ODS.FieldDefs.Add('Value', ftSmallInt, 0, False);
//}
*)

    XDS.First;
    Index := 0;
    while not XDS.Eof do begin
      Assert(XDS.FieldByName('Age').AsInteger = GadData[Index].Age);
      Equalz(XDS.FieldByName('Gender').ASString , GadData[Index].Gender);
      Assert(XDS.FieldByName('YieldRate').AsFloat = GadData[Index].YieldRate);
      Assert(XDS.FieldByName('Value').AsInteger = GadData[Index].Value);

      Inc(Index);
      XDS.Next;
    end;
    XDS.Close;

  finally
    XDS.Free;
  end;


  Assert(SysUtils.FileExists(DataPath(dbfXGad)), 'Failed to create Gad Table');

  // Reload Gad Table and verify data
  XDS := TXBaseDataset.Create(nil);
  try
//##JVR    XDSGad_Init;
    XDS.LoadFromFile(DataPath(dbfXGad));

    XDS.First;
    Index := 0;
    while not XDS.Eof do begin
      Assert(XDS.FieldByName('Age').AsInteger = GadData[Index].Age);
      Equalz(XDS.FieldByName('Gender').ASString , GadData[Index].Gender);
      Assert(XDS.FieldByName('YieldRate').AsFloat = GadData[Index].YieldRate);
      Assert(XDS.FieldByName('Value').AsInteger = GadData[Index].Value);

      Inc(Index);
      XDS.Next;
    end;

    XDS.Close;

  finally
    XDS.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 12/05/2005 19:08:30 - This is here to debug a CDS.<br>
}
procedure TDBIXDSUnitTests.CreateDataSetCDS;
var
  CDS: TClientDataset;
  Index: Integer;
  FieldDef: TFieldDef;

begin
  // Delete the Datafile if it exists
  SysUtils.DeleteFile(DataPath(cdsXDSDbug));
  Assert(not SysUtils.FileExists(DataPath(cdsXDSDbug)));

  // Create a new Dataset
  CDS := TClientDataset.Create(nil);
  try
    CDS.FileName := DataPath(cdsXDSDbug);
    CDS.FieldDefs.Clear;

    for Index := Low(TBookFields) to High(TBookFields) do begin
      FieldDef := CDS.FieldDefs.AddFieldDef;
      FieldDef.Name := UpperCase(TBookFields[Index].FieldName);
      FieldDef.DataType := TBookFields[Index].FieldType;

      if (TBookFields[Index].FieldSize > 0) then begin
        FieldDef.Size := TBookFields[Index].FieldSize;
      end;

      if (TBookFields[Index].Precision > 0) then begin
        FieldDef.Precision := TBookFields[Index].Precision;
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
        FormatDateTime(DateTimeFormat, CDS.FieldByName('Purchased').AsDateTime),
        NewBooks[Index].Purchased
        );
      Assert(CDS.FieldByName('Price').AsFloat = NewBooks[Index].Price);
      Equalz(CDS.FieldByName('Currency').AsString , NewBooks[Index].Currency);
      Assert(CDS.FieldByName('Rating').AsInteger = NewBooks[Index].Rating);
      Assert(CDS.FieldByName('Approved').AsBoolean = NewBooks[Index].Approved);
      Equalz(CDS.FieldByName('Comments').AsString , NewBooks[Index].Comments);
      Equalz(CDS.FieldByName('Notes').AsString , NewBooks[Index].Notes);
      Equalz(CDS.FieldByName('Details').AsString , NewBooks[Index].Details);
    end;  { for }

    Assert(CDS.RecordCount = Length(NewBooks));

    CDS.Close;
  finally
    CDS.Free;
  end;
end;  { CDSCreateDataSet }


// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 17:07:47.<P>
}
procedure TDBIXDSUnitTests.CreateDataSet;
begin
  // Delete the Datafile if it exists
  SysUtils.DeleteFile(DataPath(dbfXDSTest));
  Assert(not SysUtils.FileExists(DataPath(dbfXDSTest)));

  // Delete the Blobfile if it exists
  SysUtils.DeleteFile(DataPath(fptXDSTest));
  Assert(not SysUtils.FileExists(DataPath(fptXDSTest)));

  // Create new table
  CreateBooksTable(DataPath(dbfXBooks));
end;  { CreateDataSet }


// _____________________________________________________________________________
{**
  Jvr - 01/07/2005 11:12:26 - Initial code.<br>
}
procedure TDBIXDSUnitTests.CreateMemoryDataSet;
var
  XDS: TXbaseDataset;
  Index: Integer;

begin
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
      with XDS.FieldDefs.AddFieldDef do begin
        Name := UpperCase(TBookFields[Index].FieldName);
        DataType := TBookFields[Index].FieldType;

        if (TBookFields[Index].FieldSize > 0) then begin
          Size := TBookFields[Index].FieldSize;
        end;

        if (TBookFields[Index].Precision > 0) then begin
          Precision := TBookFields[Index].Precision;
        end;
      end;  { with }
    end;  { for }

    XDS.CreateDataset;

    // Add Data
    with XDS do begin
      for Index := Low(NewBooks) to High(NewBooks) do begin
        AppendRecord([
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

        Assert(RecNo = (Index + 1));
        Assert(FieldByName('Sequence').AsInteger = NewBooks[Index].Sequence);
        Equalz(FieldByName('Name').AsString , NewBooks[Index].Name);
        Equalz(FieldByName('Author').AsString , NewBooks[Index].Author);
        Equalz(
          FormatDateTime(DateTimeFormat, FieldByName('Purchased').AsDateTime),
          NewBooks[Index].Purchased
          );
        Assert(FieldByName('Price').AsFloat = NewBooks[Index].Price);
        Equalz(FieldByName('Currency').AsString , NewBooks[Index].Currency);
        Assert(FieldByName('Rating').AsInteger = NewBooks[Index].Rating);
        Assert(FieldByName('Approved').AsBoolean = NewBooks[Index].Approved);
        Equalz(FieldByName('Comments').AsString , NewBooks[Index].Comments);
        Equalz(FieldByName('Notes').AsString , NewBooks[Index].Notes);
        Equalz(FieldByName('Details').AsString , NewBooks[Index].Details);
      end;  { for }
    end;  { with }

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
end;  { CreateMemoryDataSet }


// _____________________________________________________________________________
{**
  Jvr - 26/02/2001 16:32:35.<P>
}
procedure TDBIXDSUnitTests.LoadAndSave;
var
  XDS: TXbaseDataset;
  Index: Integer;
  
begin
  // Delete the Datafile if it exists
  SysUtils.DeleteFile(DataPath(dbfXBooks));
  Assert(not SysUtils.FileExists(DataPath(dbfXBooks)));

  // Delete the Blobfile if it exists
  SysUtils.DeleteFile(DataPath(fptXBooks));
  Assert(not SysUtils.FileExists(DataPath(fptXBooks)));

  CreateBooksTable(DataPath(dbfXBooks));

  // Open Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    Assert(SysUtils.FileExists(DataPath(dbfXBooks)));
    Assert(SysUtils.FileExists(DataPath(fptXBooks)));

    XDS.FileName := DataPath(dbfXBooks);
    XDS.LoadFromFile;

    // Update Data
    with XDS do begin
      First;
      for Index := Low(UpdateBooks) to High(UpdateBooks) do begin
        Edit;
        SetFields([
          UpdateBooks[Index].Sequence,
          UpdateBooks[Index].Name,
          UpdateBooks[Index].Author,
          StrToDateTime(String(UpdateBooks[Index].Purchased)),
          UpdateBooks[Index].Price,
          UpdateBooks[Index].Currency,
          UpdateBooks[Index].Rating,
          UpdateBooks[Index].Approved,
          UpdateBooks[Index].Comments,
          UpdateBooks[Index].Notes,
          UpdateBooks[Index].Details
        ]);
        Post;

        Assert(RecNo = (Index + 1));
        Assert(FieldByName('Sequence').AsInteger = UpdateBooks[Index].Sequence);
        Equalz(FieldByName('Name').AsString , UpdateBooks[Index].Name);
        Equalz(FieldByName('Author').AsString , UpdateBooks[Index].Author);
        Equalz(
          FormatDateTime(DateTimeFormat, FieldByName('Purchased').AsDateTime),
          UpdateBooks[Index].Purchased
          );
        Assert(FieldByName('Price').AsFloat = UpdateBooks[Index].Price);
        Equalz(FieldByName('Currency').AsString , UpdateBooks[Index].Currency);
        Assert(FieldByName('Rating').AsInteger = UpdateBooks[Index].Rating);
        Assert(FieldByName('Approved').AsBoolean = UpdateBooks[Index].Approved);
        Equalz(FieldByName('Comments').AsString , UpdateBooks[Index].Comments);
        Equalz(FieldByName('Notes').AsString , UpdateBooks[Index].Notes);
        Equalz(FieldByName('Details').AsString , UpdateBooks[Index].Details);

        Next;
      end;  { for }
    end;  { with }

    Assert(XDS.RecordCount = Length(UpdateBooks));

    // Save the results to file
    XDS.SaveToFile(DataPath(dbfXBooksUpD8));
    Assert(SysUtils.FileExists(DataPath(dbfXBooksUpD8)));
    Assert(SysUtils.FileExists(DataPath(fptXBooksUpD8)));

    XDS.Close;
  finally
    XDS.Free;
  end;


  // Now reload the updated results to test that the file was updated properly
  XDS := TXbaseDataset.Create(nil);
  try
    Assert(SysUtils.FileExists(DataPath(dbfXBooksUpD8)));
    Assert(SysUtils.FileExists(DataPath(fptXBooksUpD8)));
    XDS.LoadFromFile(DataPath(dbfXBooksUpD8));
    Assert(XDS.RecordCount = Length(UpdateBooks));

    // Verify Data
    with XDS do begin
      First;
      for Index := Low(UpdateBooks) to High(UpdateBooks) do begin
        Assert(RecNo = (Index + 1));
        Assert(FieldByName('Sequence').AsInteger = UpdateBooks[Index].Sequence);
        Equalz(FieldByName('Name').AsString , UpdateBooks[Index].Name);
        Equalz(FieldByName('Author').AsString , UpdateBooks[Index].Author);
        Equalz(
          FormatDateTime(DateTimeFormat, FieldByName('Purchased').AsDateTime),
          UpdateBooks[Index].Purchased
          );
        Assert(FieldByName('Price').AsFloat = UpdateBooks[Index].Price);
        Equalz(FieldByName('Currency').AsString , UpdateBooks[Index].Currency);
        Assert(FieldByName('Rating').AsInteger = UpdateBooks[Index].Rating);
        Assert(FieldByName('Approved').AsBoolean = UpdateBooks[Index].Approved);
        Equalz(FieldByName('Comments').AsString , UpdateBooks[Index].Comments);
        Equalz(FieldByName('Notes').AsString , UpdateBooks[Index].Notes);
        Equalz(FieldByName('Details').AsString , UpdateBooks[Index].Details);

        Next;
      end;  { for }
    end;  { with }

    XDS.Close;
  finally
    XDS.Free;
  end;
end;  { LoadAndSave }


// _____________________________________________________________________________
{**
  Jvr - 23/02/2001 15:07:48<P>
}
procedure TDBIXDSUnitTests.UpdateBooksTable;
var
  XDS: TXbaseDataset;
  Index: Integer;

begin
  // Only create BooksUpD8 table if it doesn't exist!
  if SysUtils.FileExists(DataPath(dbfXBooksUpD8)) then begin
    Exit;
  end;

  CreateBooksTable(DataPath(dbfXBooks));

  // Open Dataset
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(dbfXBooks);
    XDS.SaveToFile(dbfXBooksUpD8);
    XDS.Close;

  finally
    XDS.Free;
  end;

  
  XDS := TXbaseDataset.Create(nil);
  try
    Assert(SysUtils.FileExists(DataPath(dbfXBooksUpD8)));
    Assert(SysUtils.FileExists(DataPath(fptXBooksUpD8)));

    XDS.FileName := DataPath(dbfXBooksUpD8);
    XDS.Open;

    // Update Data
    with XDS do begin
      for Index := Low(UpdateBooks) to High(UpdateBooks) do begin
        Edit;
        SetFields([
          UpdateBooks[Index].Sequence,
          UpdateBooks[Index].Name,
          UpdateBooks[Index].Author,
          StrToDateTime(String(UpdateBooks[Index].Purchased)),
          UpdateBooks[Index].Price,
          UpdateBooks[Index].Currency,
          UpdateBooks[Index].Rating,
          UpdateBooks[Index].Approved,
          UpdateBooks[Index].Comments,
          UpdateBooks[Index].Notes,
          UpdateBooks[Index].Details
        ]);
        Post;

        Assert(RecNo = (Index + 1));
        Assert(FieldByName('Sequence').AsInteger = UpdateBooks[Index].Sequence);
        Equalz(FieldByName('Name').AsString , UpdateBooks[Index].Name);
        Equalz(FieldByName('Author').AsString , UpdateBooks[Index].Author);
        Equalz(
          FormatDateTime(DateTimeFormat, FieldByName('Purchased').AsDateTime),
          UpdateBooks[Index].Purchased
          );
        Assert(FieldByName('Price').AsFloat = UpdateBooks[Index].Price);
        Equalz(FieldByName('Currency').AsString , UpdateBooks[Index].Currency);
        Assert(FieldByName('Rating').AsInteger = UpdateBooks[Index].Rating);
        Assert(FieldByName('Approved').AsBoolean = UpdateBooks[Index].Approved);
        Equalz(FieldByName('Comments').AsString , UpdateBooks[Index].Comments);
        Equalz(FieldByName('Notes').AsString , UpdateBooks[Index].Notes);
        Equalz(FieldByName('Details').AsString , UpdateBooks[Index].Details);

        Next;
      end;  { for }
    end;  { with }

    Assert(XDS.RecordCount = Length(NewBooks));

    XDS.Close;
  finally
    XDS.Free;
  end;
end;  { UpdateDataSet }


// _____________________________________________________________________________
{**
  Test the LoadFromDataset functionality.

  Jvr - 05/06/2001 12:13:11.<P>
}
procedure TDBIXDSUnitTests.LoadFromDataset;
var
  CDS: TClientDataset;
  XDS: TXbaseDataset;
  Index: Integer;
  MemoData: String;

begin
  CreateBooksCDS;

  CDS := TClientDataset.Create(nil);
  try
    CDS.LoadFromFile(DataPath(cdsXBooksUpD8));
    Assert(CDS.RecordCount = Length(NewBooks));

    // Verify Data
    with CDS do begin
      First;
      for Index := Low(UpdateBooks) to High(UpdateBooks) do begin
        Assert(RecNo = (Index + 1));
        Assert(FieldByName('Sequence').AsInteger = UpdateBooks[Index].Sequence);
        Equalz(FieldByName('Name').AsString , UpdateBooks[Index].Name);
        Equalz(FieldByName('Author').AsString , UpdateBooks[Index].Author);
        Equalz
          (FormatDateTime(DateTimeFormat, FieldByName('Purchased').AsDateTime),
          UpdateBooks[Index].Purchased
          );
        Assert(FieldByName('Price').AsFloat = UpdateBooks[Index].Price);
        Equalz(FieldByName('Currency').AsString , UpdateBooks[Index].Currency);
        Assert(FieldByName('Rating').AsInteger = UpdateBooks[Index].Rating);
        Assert(FieldByName('Approved').AsBoolean = UpdateBooks[Index].Approved);
        Equalz(FieldByName('Comments').AsString , UpdateBooks[Index].Comments);

        // I need to do this because I believe ther is a bug in CDS
        MemoData := FieldByName('Notes').AsString;
        SetLength(MemoData, StrLen(PChar(MemoData)));
        Equalz(MemoData , UpdateBooks[Index].Notes);

        MemoData := FieldByName('Details').AsString;
        SetLength(MemoData, StrLen(PChar(MemoData)));
        Equalz(MemoData , UpdateBooks[Index].Details);

        Next;
      end;  { for }
    end;  { with }



    // Delete the Datafile if it exists
    SysUtils.DeleteFile(DataPath(dbfXDSLoad));
    Assert(not SysUtils.FileExists(DataPath(dbfXDSLoad)));

    // Delete the Blobfile if it exists
    SysUtils.DeleteFile(DataPath(fptXDSLoad));
    Assert(not SysUtils.FileExists(DataPath(fptXDSLoad)));

    // Update Data to XDS using a Memory Stream
    XDS := TXbaseDataset.Create(nil);
    try
      XDS.LoadFromDataset(CDS, [lmCreateDataset]);

      XDS.First;
      for Index := Low(UpdateBooks) to High(UpdateBooks) do begin
        Assert(XDS.RecNo = (Index + 1));
        Assert(XDS.FieldByName('Sequence').AsInteger = UpdateBooks[Index].Sequence);
        Equalz(XDS.FieldByName('Name').AsString , UpdateBooks[Index].Name);
        Equalz(XDS.FieldByName('Author').AsString , UpdateBooks[Index].Author);
        Equalz(
          FormatDateTime(DateTimeFormat, XDS.FieldByName('Purchased').AsDateTime),
          UpdateBooks[Index].Purchased
          );
        Assert(XDS.FieldByName('Price').AsFloat = UpdateBooks[Index].Price);
        Equalz(XDS.FieldByName('Currency').AsString , UpdateBooks[Index].Currency);
        Assert(XDS.FieldByName('Rating').AsInteger = UpdateBooks[Index].Rating);
        Assert(XDS.FieldByName('Approved').AsBoolean = UpdateBooks[Index].Approved);
        Equalz(XDS.FieldByName('Comments').AsString , UpdateBooks[Index].Comments);
        Equalz(XDS.FieldByName('Notes').AsString , UpdateBooks[Index].Notes);
//##JVR - Clientdatasets don't support Character fields this long
//##JVR          Assert(FieldByName('Details').AsString = NewBooks[Index].Details);

        XDS.Next;
      end;  { for }

      XDS.SaveToFile(DataPath(dbfXDSLoad));
      Assert(XDS.RecordCount = Length(NewBooks));
      Assert(SysUtils.FileExists(DataPath(dbfXDSLoad)));
      Assert(SysUtils.FileExists(DataPath(fptXDSLoad)));

      XDS.Close;
    finally
      XDS.Free;
    end;  { try..finally }



    // Delete the Datafile if it exists
    SysUtils.DeleteFile(DataPath(dbfXDSLoad));
    Assert(not SysUtils.FileExists(DataPath(dbfXDSLoad)));

    // Delete the Blobfile if it exists
    SysUtils.DeleteFile(DataPath(fptXDSLoad));
    Assert(not SysUtils.FileExists(DataPath(fptXDSLoad)));

    // Update Data to XDS using a File Stream
    XDS := TXbaseDataset.Create(nil);
    try
      XDS.FileName := DataPath(dbfXDSLoad);
      XDS.LoadFromDataset(CDS, [lmCreateDataset]);

      with XDS do begin
        First;
        for Index := Low(UpdateBooks) to High(UpdateBooks) do begin
          Assert(RecNo = (Index + 1));
          Assert(FieldByName('Sequence').AsInteger = UpdateBooks[Index].Sequence);
          Equalz(FieldByName('Name').AsString , UpdateBooks[Index].Name);
          Equalz(FieldByName('Author').AsString , UpdateBooks[Index].Author);
          Equalz(
            FormatDateTime(DateTimeFormat, FieldByName('Purchased').AsDateTime),
            UpdateBooks[Index].Purchased
            );
          Assert(FieldByName('Price').AsFloat = UpdateBooks[Index].Price);
          Equalz(FieldByName('Currency').AsString , UpdateBooks[Index].Currency);
          Assert(FieldByName('Rating').AsInteger = UpdateBooks[Index].Rating);
          Assert(FieldByName('Approved').AsBoolean = UpdateBooks[Index].Approved);
          Equalz(FieldByName('Comments').AsString , UpdateBooks[Index].Comments);
          Equalz(FieldByName('Notes').AsString , UpdateBooks[Index].Notes);
//##JVR - Clientdatasets don't support Character fields this long
//##JVR          Assert(FieldByName('Details').AsString = NewBooks[Index].Details);

          Next;
        end;  { for }
      end;  { with }

      Assert(XDS.RecordCount = Length(NewBooks));
      Assert(SysUtils.FileExists(DataPath(dbfXDSLoad)));
      Assert(SysUtils.FileExists(DataPath(fptXDSLoad)));

      XDS.Close;
    finally
      XDS.Free;
    end;  { try..finally }

    CDS.Close;
  finally
    CDS.Free;
  end;  { try..finally }
end;  { LoadFromDataset }


procedure TDBIXDSUnitTests.LoadFromXbaseFile;
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
end;  { LoadFromDBF }


// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 12:44:19 - Test created to find memory leaks.<P>
}
procedure TDBIXDSUnitTests.Indices;
var
  XDS: TXbaseDataset;
  Index: Integer;

begin
  CreateBooksTable(DataPath(dbfXBooks));

  XDS := TXbaseDataset.Create(nil);
  try
    with XDS do begin
      Close;
      FileName := DataPath(dbfXBooks); //##JVR dbfXDSTest);
      LoadFromFile;

      AddIndex('NameOrder', 'Name', [ixCaseInsensitive]);
      AddIndex('NameReverse', 'Name', [ixDescending, ixCaseInsensitive]);

      // Ascending Order
      IndexName := 'NameOrder';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdateBooks[NameOrderIdx[Index]].Name);
        Next;
      end;

      // Descending Order
      IndexName := 'NameOrder' + ';Descending';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdateBooks[NameReverseIdx[Index]].Name);
        Next;
      end;

      // Descending Order
      IndexName := 'NameReverse';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdateBooks[NameReverseIdx[Index]].Name);
        Next;
      end;

      // Ascending Order
      IndexName := 'NameReverse' + ';Descending';
      First;
      for Index := 0 to RecordCount-1 do begin
        Equalz(FieldByName('Name').AsString , UpdateBooks[NameOrderIdx[Index]].Name);
        Next;
      end;

      Close;
    end;  { with }
  finally
    XDS.Free;
  end;
end;  { Indices }


// _____________________________________________________________________________
{**
  Jvr - 06/02/2001 17:11:40.<P>
}
procedure TDBIXDSUnitTests.Locate;
var
  Gad: TXbaseDataset;

begin
  CreateGadTable;
  
  Gad := TXbaseDataset.Create(nil);
  try
    Gad.Close;
    Gad.FileName := DataPath(dbfXGad);
    Gad.Open;

    Assert(Gad.RecordCount = Length(GadData));
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
end;  { Locate }


// _____________________________________________________________________________
{**
  Jvr - 08/05/2001 18:22:32.<P>
}
procedure TDBIXDSUnitTests.DateTimes;
var
  XDS: TXbaseDataset;
  DateTimeStamp: TTimeStamp;
  TimeTimeStamp: TTimeStamp;
  TimeStamp: TTimeStamp;
  CombinedTimeStamp: TTimeStamp;
  ResultTimeStamp: TTimeStamp;
  SetValue: TDateTime;
  Today: TDateTime;
  ID: Word;

begin
  CreateGadTable;

  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FileName := DataPath(dbfXGad);
    XDS.Open;

    XDS.First;
    while not XDS.Eof do begin
      ID := XDS.FieldByName('ID').AsInteger;
      Today := XDS.FieldByName('Created').AsDateTime;
      TimeStamp := DateTimeToTimeStamp(Today);

      Assert(Trunc(Today) = XDS.FieldByName('Date').AsDateTime, 'Dates do NOT match');

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
      SetValue := TimeStampToDateTime(DateTimeToTimeStamp(Now));
      XDS.FieldByName('Created').AsDateTime := SetValue;
      XDS.Post;

      // Are they the same?
      TimeStamp := DateTimeToTimeStamp(XDS.FieldByName('Created').AsDateTime);
      ResultTimeStamp := DateTimeToTimeStamp(SetValue);
      Assert(ResultTimeStamp.Date = TimeStamp.Date);
      Assert(Round(ResultTimeStamp.Time/1000) * 1000 = Round(TimeStamp.Time/1000) * 1000);

      // Ok if that works then change it back again
      XDS.Edit;
      XDS.FieldByName('Created').AsDateTime := Today; //##JVR TimeStampToDateTime(TimeStamp);
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
end;  { DateTimes }


procedure TDBIXDSUnitTests.SaveAsCDS;
begin
  CreateBooksCDS;
end;  { SaveAsCDS }


procedure TDBIXDSUnitTests.TestCloseAndDelete;
const
  Filename = 'C:\Temp\TestFile.DBF';

var
  XDS: TXbaseDataset;
  FieldDef: TFieldDef;

begin
  XDS := TXbaseDataset.Create(nil);
  try
    // Build the table from scratch
    XDS.FieldDefs.Clear;
    XDS.FileName := Filename;

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

    Assert(SysUtils.DeleteFile(XDS.Filename));
  finally
    XDS.Free;
  end;
end;  { TestCloseAndDelete }


procedure TDBIXDSUnitTests.UpdateDataset;
begin
  UpdateBooksTable;
end;


procedure TDBIXDSUnitTests.Setup;
begin
  inherited;

{$ifdef Delphi2009}
  FormatSettings.ShortDateFormat := DateTimeFormat;
{$else}
  ShortDateFormat := DateTimeFormat;
{$endif}
end;  { Setup }


procedure TDBIXDSUnitTests.Teardown;
begin
  inherited;

end;  { TearDown }


initialization
{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBIXDSUnitTests);
{$else}
  RegisterTest('', TDBIXDSUnitTests.Suite);
{$endif}

end.
