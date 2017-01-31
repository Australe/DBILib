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
  1.0 | 01/05/2013 11:03:07 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBICDSUnitTests;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, Contnrs,
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
    procedure Calculated;
    procedure FilterGad;
    procedure Streaming;
    procedure UpdateBooks;

  public
    procedure ObjectTypes;
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
  DBIComponents,
  DBIDataset,
  DBIXbaseDatasets;



procedure TDBICDSUnitTests.Calculated;
const
  Quote = #39;
  TableName = 'cCalculated' + xmlExtension;

var
  CDS: TDBIClientDataset;

begin
  // Create Gad CDS
  CDS := TDBIClientDataset.Create(nil);
  try
    TCalculatedData.CreateFields(CDS);
    CDS.CreateDataset;

    TCalculatedData.OccupyValues(CDS);
    TCalculatedData.AssertValues(CDS);

    CDS.SaveToFile(DataPath(TableName));
    Assert(SysUtils.FileExists(DataPath(TableName)), 'Failed to create Calc XML file');

    CDS.Close;
  finally
    CDS.Free;
  end;


  // Reload Table Data and verify data
  CDS := TDBIClientDataset.Create(nil);
  try
    TCalculatedData.CreateFields(CDS);
    CDS.LoadFromFile(DataPath(TableName));
    TCalculatedData.AssertValues(CDS);

    TCalculatedData.ClearValues(CDS);
    TCalculatedData.AssertBlanks(CDS);

    TCalculatedData.UpdateValues(CDS);
    TCalculatedData.AssertValues(CDS);

    CDS.Close;
  finally
    CDS.Free;
  end;
end;


procedure TDBICDSUnitTests.FilterGad;
const
  Quote = #39;
  TableName = 'cFilterGad' + xmlExtension;

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


procedure TDBICDSUnitTests.ObjectTypes;
const
  TableName = 'cCreateEntity.xml';

begin
  // Run Object Tests
  TEntityData.CDSCreateEntity(DataPath(TableName));
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


procedure TDBICDSUnitTests.Setup;
begin
  inherited Setup;

{$ifndef Delphi2007}
  ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$else}
  FormatSettings.ShortDateFormat := TDBIUnitTest.GetDateTimeFormat;
{$endif}
end;


type
  TCDSStreamingComponent = class(TDBIPersistenceAdapter)
  private
    FCDS: TDBIClientDataset;
  protected
    function GetRootComponent: TComponent; override;
  end;

  function TCDSStreamingComponent.GetRootComponent: TComponent;
  begin
    if not Assigned(FCDS) then begin
      FCDS := TDBIClientDataset.Create(Self);
    end;
    Result := FCDS;
  end;

procedure TDBICDSUnitTests.Streaming;
const
  TableName = 'cStreamBooks' + xmlExtension;

var
  Adapter: TCDSStreamingComponent;
  FieldData: TBookFields;
  CDS: TDBIClientDataset;

begin
  FieldData := TBookData.GetFields;

  Adapter := TCDSStreamingComponent.Create(nil);
  try
    // Create a new Dataset
    TBookData.CDSCreateTable(DataPath(TableName));

    CDS := Adapter.RootComponent as TDBIClientDataset;
    try
      CDS.Name := ChangeFileExt(TableName, '');
      TBookData.CreateFieldDefs(CDS);

      // Verify Data was written correctly to file
      CDS.LoadFromFile(DataPath(TableName));
      TBookData.AssertValues(CDS);

      // Stream the Dataset out to File
      Adapter.SaveToFile(DataPath(ChangeFileExt(TableName, dfmExtension)));
      CDS.Close;
    finally
      CDS.Free;
    end;
  finally
    FreeAndNil(Adapter);
  end;

  Adapter := TCDSStreamingComponent.Create(nil);
  try
    // Stream the Dataset Back in
    Adapter.LoadFromFile(DataPath(ChangeFileExt(TableName, dfmExtension)));
    CDS := Adapter.RootComponent as TDBIClientDataset;

    TBookData.VerifyFields(CDS, @FieldData, Length(FieldData));
    TBookData.AssertValues(CDS);
  finally
    FreeAndNil(Adapter);
  end;
end;


procedure TDBICDSUnitTests.Teardown;
begin

  inherited TearDown;
end;


procedure TDBICDSUnitTests.UpdateBooks;
const
  TableName = 'cUpdateBooks' + xmlExtension;

var
  CDS: TDBIClientDataset;

begin
  TBookData.DeleteTables(DataPath(TableName));

  TBookData.CDSUpdateTable(DataPath(TableName));

  // Open Dataset
  CDS := TDBIClientDataset.Create(nil);
  try
    CDS.LoadFromFile(DataPath(TableName));

    TBookData.CheckFields(CDS, DBIFieldCheckAll);
    TBookData.UpdateValues(CDS);
    TBookData.ReviseFields(CDS);

    CDS.SaveToFile(DataPath(TableName));
    CDS.Close;
  finally
    CDS.Free;
  end;
end;


initialization
{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBICDSUnitTests);
{$else}
  RegisterTest('', TDBICDSUnitTests.Suite);
{$endif}

end.
