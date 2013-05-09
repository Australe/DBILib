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
  Classes, Contnrs, DBIStrings, DBIObjectListDatasets, DBIUnitTests,
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
  protected
    procedure Setup; override;
    procedure Teardown; override;

{$ifdef omTesting}
    procedure Fail(msg: string; errorAddr: Pointer = nil);
{$endif}

  published
    procedure FilterGad;

  end;


implementation

uses
{$ifdef DELPHI6}
  Variants,
{$endif}
  Windows,
  SysUtils,
  Dialogs,
  DB,
  DBIConst,
  DBIDataset,
  DBIXbaseDatasets;



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


initialization
{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBICDSUnitTests);
{$else}
  RegisterTest('', TDBICDSUnitTests.Suite);
{$endif}

end.
