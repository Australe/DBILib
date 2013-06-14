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
  1.0 | 01/05/2013 11:03:07 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : native api code}

unit DBICDSUnitTests;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, Contnrs, DBIStrings, DBIObjectListDatasets, DBIUnitTests,
{$ifndef fpc}
  DBClient, DSIntf,
  {$ifdef omTesting}
  omTestSuites, omTestMastery;
  {$else}
  TestFrameWork;
  {$endif}
{$else}
  testregistry;
{$endif}

type
  TDBICDSUnitTests = class(TDBIUnitTests)
  public
    class function FilePath(const AFilename: TFileName): TFileName;

  protected
    procedure Setup; override;
    procedure Teardown; override;

{$ifdef omTesting}
    procedure Fail(msg: string; errorAddr: Pointer = nil);
{$endif}

  published
    procedure FilterGad;
    procedure TranName;

  end;


implementation

uses
{$ifdef DELPHI6}
  Variants,
{$endif}
  Windows,
  Dialogs,
  DB,
  DBIConst,
  DBIDataset,
  DBIXbaseDatasets,
  DBIUnitTestsData;



procedure TDBICDSUnitTests.FilterGad;
const
  Quote = #39;
  TableName = 'cFilterGad.xml';

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

    CDS.Filter := Format('Gender = %sM%s', [Quote, Quote]);
    CDS.Filtered := True;

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


{$ifdef omTesting}
procedure TDBICDSUnitTests.Fail(msg: string; errorAddr: Pointer = nil);
begin
  if errorAddr = nil then
    raise ETestFailed.Create(msg)
  else
    raise ETestFailed.Create(msg) at errorAddr;
end;
{$endif}


procedure TDBICDSUnitTests.Setup;
begin
  inherited Setup;

{$ifndef Delphi2007}
  ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$else}
  FormatSettings.ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$endif}
end;


procedure TDBICDSUnitTests.Teardown;
begin

  inherited TearDown;
end;


procedure TDBICDSUnitTests.TranName;
const
  TableName = 'TranName.dbf';

var
  XDS: TXbaseDataset;
  ODS: TObjectListDataset;
  List: TObjectList;
  Count: Integer;
  Index: Integer;
  EditValue: Variant;

begin
//  EditValue := 'CATI';

  XDS := TXbaseDataset.Create(nil);
  try
    XDS.LoadFromFile(FilePath(TableName));
    XDS.SaveToFile(ChangeFileExt(FilePath(TableName), '.xml'));

    XDS.Close;
  finally
    XDS.Free;
  end;

  List := TObjectList.Create;
  try
    ODS := TObjectlistDataset.Create(nil);
    try
      ODS.ClassTypeName := TomTranName.ClassName;
      ODS.StringFieldSize := 255;
      ODS.List := List;
      ODS.LoadFromFile(FilePath(TableName));
      Count := ODS.RecordCount;
      ODS.Close;

    finally
      ODS.Free;
    end;


    for Index := 500 downto 1 do begin
      ODS := TObjectlistDataset.Create(nil);
      try
        Assert(List.Count = Count);
//List.Free;
//List.Add(nil);
        ODS.ClassTypeName := TomTranName.ClassName;
        ODS.StringFieldSize := 255;
        ODS.List := List;
        ODS.Active := True;

        Assert(ODS.RecordCount = Count);
(*##JVR
//        EditValue := '123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345';
        EditValue := String(PChar('This is a test')); //42.25;
        ODS.Locate('TranType', EditValue, [loCaseInsensitive]);
{##JVR
//*)
        EditValue := ODS.FieldByName('TranType').Value;
        ODS.First;
        Assert(ODS.Locate('TranType', EditValue, [loCaseInsensitive]));
        ODS.Last;
        Assert(ODS.Locate('TranType', EditValue, [loCaseInsensitive]));
        ODS.First;
        Assert(ODS.Locate('TranType', EditValue, [loCaseInsensitive]));
        ODS.Next;
        Assert(ODS.Locate('TranType', EditValue, [loCaseInsensitive]));
        ODS.Prior;
        Assert(ODS.Locate('TranType', EditValue, [loCaseInsensitive]));
//}
        ODS.Close;

      finally
        ODS.Free;
      end;
    end;

  finally
    List.Free;
  end;
end;


class function TDBICDSUnitTests.FilePath(const AFilename: TFileName): TFileName;
begin
  // cd ../../../Data
  Result := ExtractFileDir(ParamStr(0));
  Result := ExtractFileDir(Result);
{$ifndef omTesting}
  Result := ExtractFileDir(Result);
{$endif}  
  Result := ExtractFileDir(Result) + '\Data\' + AFilename;
end;


initialization
{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBICDSUnitTests);
{$else}
  RegisterTest('', TDBICDSUnitTests.Suite);
{$endif}

end.
