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
  1.0 | 18/06/2013 07:36:11 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : native api code}

unit DBIAUXUnitTests;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DB, DBIStrings, DBIObjectListDatasets, DBIUnitTests,
  DBITokenizers, DBIDataPacketReaders,
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
  TDBICustomReader = class(TDBICustomParser)
  protected
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

  public
    function Process: Boolean;

  end;

type
  TDBIAUXUnitTests = class(TDBIUnitTests)
  protected
    procedure Setup; override;
    procedure Teardown; override;

  published
    procedure Encode;

  end;

implementation

uses
{$ifdef DELPHI6}
  Variants,
{$endif}
{$ifdef omTesting}
  omLogMastery,
{$endif}
  DBIConst,
  DBIDataset,
  DBIXbaseDatasets,
  DBIUtils;


procedure TDBIAUXUnitTests.Setup;
begin
  inherited Setup;

end;


procedure TDBIAUXUnitTests.Teardown;
begin

  inherited TearDown;
end;


procedure TDBIAUXUnitTests.Encode;
const
  InputData =  'Some text &amp; a &apos;test&apos;.';
  OutputData = 'Some text & a ''test''.';

var
  Processor: TDBICustomReader;

begin
  Processor := TDBICustomReader.Create;
  try
    Processor.Input.Text := InputData;
    Processor.Process;

    Assert(Processor.Output.Text = OutputData);
  finally
    Processor.Free;
  end;
end;





{ TDBICustomReader }

function TDBICustomReader.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBIXMLDataPacketLexer;
end;


function TDBICustomReader.Process: Boolean;
begin
  Result := True;

  // BootStrap the lexer;
  Input.Reset;

  repeat
    Input.NextToken;
    Output.WriteStr(Input.Token.AsString);
  until Input.Eof;
end;





initialization

{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBIAUXUnitTests);
{$else}
  RegisterTest('', TDBIAUXUnitTests.Suite);
{$endif}

end.
