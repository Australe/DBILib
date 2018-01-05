// _____________________________________________________________________________
{
  Copyright (C) 1996-2018, All rights reserved, John Vander Reest

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
  1.0 | 22/12/2017 07:36:11 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBITokenizeUnitTests;

interface

{$I DBICompilers.inc}

uses
  Classes,
  SysUtils,
  DB,
{$ifndef fpc}
  DBClient,
  DSIntf,
  {$ifdef omTesting}
  omTestSuites,
  omTestMastery,
  {$else}
  TestFrameWork,
  {$endif}
{$else}
  testregistry,
{$endif}
  DBIStrings,
  DBIObjectListDatasets,
  DBIUnitTests,
  DBITokenizers,
  DBIDataPacketReaders,
  DBIMacroProcessors;

type
  TDBICustomReader = class(TDBICustomParser)
  protected
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

  public
    function Process: Boolean;

  end;


type
  TDBITokenizeUnitTests = class(TDBIUnitTests)
  protected
    procedure Setup; override;
    procedure Teardown; override;

  published
    procedure BindToODS;
    procedure BindToStrings;
    procedure ConvertCase;
    procedure Encode;
    procedure Expand;
    procedure GlobalAttributes;
    procedure IfThenElseObject;
    procedure IfThenElseParam;

  protected
    procedure IfThenElseLiteral; // Processor needs fixing

  end;


implementation

uses
{$ifdef DELPHI6}
  Variants,
{$endif}
{$ifdef omTesting}
  omLogMastery,
{$endif}
  IniFiles,
  DBIConst,
  DBIUtils,
  DBIDataset,
  DBIXbaseDatasets;


{ TDBITokenizeUnitTests }

procedure TDBITokenizeUnitTests.BindToODS;
const
  C_TableName = 'oCreateBooks.dbf';
  C_Template = 'Count = ${BookData.Count} ,{#foreach BookData as Book} Sequ = $Book.Sequence ,{#endfor}';
  C_OutputData = 'Count = 5 , Sequ = 0 , Sequ = 1 , Sequ = 2 , Sequ = 3 , Sequ = 4 ,';

var
  ODS: TObjectListDataset;
  Processor: TDBIGenericMacroProcessor;

begin
  // Create a new Dataset
  TBookData.ODSCreateTable(DataPath(C_TableName));

  ODS := Local(TObjectListDataset.Create(nil)).Obj as TObjectListDataset;
  ODS.ClassTypeName := TBookData.ClassName;

  TBookData.CreateFieldDefs(ODS);

  // Verify Data was written correctly to file
  ODS.List.Clear;
  ODS.LoadFromFile(DataPath(C_TableName));

  TBookData.AssertValues(ODS);

  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;
  Processor.Scope.AddBinding('BookData', TDBIObjectListDatasetEnumeratorScope, ODS);
  Processor.Input.Text := C_Template;
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData);

  ODS.Close;
end;


procedure TDBITokenizeUnitTests.BindToStrings;
const
  C_Template = '{#define EnumerableName FirstNames}{#foreach ${EnumerableName} as FirstName}Name = ${FirstName.Value},{#endfor}';
  C_InputData = 'John,Melinda,Samuel,Charlotte';
  C_OutputData = 'Name = John,Name = Melinda,Name = Samuel,Name = Charlotte,';
var
  Data: TStrings;
  Processor: TDBIGenericMacroProcessor;

begin
  Data := Local(TStringList.Create).Obj as TStrings;
  Data.CommaText := C_InputData;

  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;
  Processor.Scope.AddBinding('FirstNames', TDBIStringsEnumeratorScope, Data);
  Processor.Input.Text := C_Template;
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData);
end;


procedure TDBITokenizeUnitTests.ConvertCase;
const
  C_Template =
    '{#boolean Shout True\}'#13#10 +
    '{#define Caption\}'#13#10 +
    '{#define Name.Surname Vander Reest\}'#13#10 +
    '{#if Shout\}'#13#10 +
    '{#define Caption x{#UpperCase y{#Slice 8, 5, ${Name.Surname}y}}x\}'#13#10 +
    '{#else\}'#13#10 +
    '{#define Caption {#LowerCase ${Name.Surname}}\}'#13#10 +
    '{#endif\}'#13#10 +
    'My Surname is {#CamelCase ${Name.Surname}}'#13#10 +
    'More Text ${Caption}'#13#10 +
    'Last Line';

  C_OutputData =
    'My Surname is vander Reest'#13#10 +
    'More Text xYREESTx'#13#10 +
    'Last Line';

var
  Processor: TDBIGenericMacroProcessor;

begin
  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;
  Processor.Scope.AddBinding('', TDBIGlobalScope, nil);
  Processor.Output.Clear;
  Processor.Input.Clear;
  Processor.Input.Text := C_Template;
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData);
end;


procedure TDBITokenizeUnitTests.Encode;
const
  C_InputData =  'Some text &amp; a &apos;test&apos;.';
  C_OutputData = 'Some text & a ''test''.';

var
  Processor: TDBICustomReader;

begin
  Processor := Local(TDBICustomReader.Create).Obj as TDBICustomReader;
  Processor.Input.Text := C_InputData;
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData);
end;


procedure TDBITokenizeUnitTests.Expand;
const
  C_TableName = 'oCreateBooks.dbf';
  C_Template = '{#define Once Twice}{#define Again ${Once} again}{#define Final ${Again}, repeated}${Final}';
  C_OutputData = 'Twice again, repeated';

var
  Processor: TDBIGenericMacroProcessor;

begin
  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;
  Processor.Scope.AddBinding('this', TDBIGlobalScope, nil);
  Processor.Input.Text := C_Template;
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData);
end;


procedure TDBITokenizeUnitTests.GlobalAttributes;
const
  C_Template = '${ComputerName}';

var
  Processor: TDBIGenericMacroProcessor;

begin
  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;

  // Instantiate global object
  Processor.Scope.Global;

  Processor.Input.Text := C_Template;
  Processor.Process;

  Assert(Processor.Output.Text = TDBIHostInfo.GetComputerName);
end;


procedure TDBITokenizeUnitTests.IfThenElseLiteral;
const
  C_Template = '{#if ''%s'' == ${this.UserName} } Name = John {#else} Name = Melinda {#endif}';
  C_OutputData: array[Boolean] of String = (' Name = Melinda ', ' Name = John ');

var
  Processor: TDBIGenericMacroProcessor;

begin
  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;
  Processor.Scope.AddBinding('this', TDBIGlobalScope, nil);
  Processor.Input.Text := Format(C_Template, [TDBIHostInfo.GetUserName]);
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData[True]);

  Processor.Output.Clear;
  Processor.Input.Clear;
  Processor.Input.Text := Format(C_Template, ['unknown']);
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData[False]);
end;


procedure TDBITokenizeUnitTests.IfThenElseObject;
const
  C_Template = '{#if ${this.UserName} == ''%s'' } Name = John {#else} Name = Melinda {#endif}';
  C_OutputData: array[Boolean] of String = (' Name = Melinda ', ' Name = John ');

var
  Processor: TDBIGenericMacroProcessor;

begin
  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;
  Processor.Scope.AddBinding('this', TDBIGlobalScope, nil);
  Processor.Input.Text := Format(C_Template, [TDBIHostInfo.GetUserName]);
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData[True]);

  Processor.Output.Clear;
  Processor.Input.Clear;
  Processor.Input.Text := Format(C_Template, ['unknown']);
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData[False]);
end;


procedure TDBITokenizeUnitTests.IfThenElseParam;
const
  C_Template = '{#if ${ItemIndex} == 5 } Name = John {#else} Name = Melinda {#endif}';
  C_OutputData: array[Boolean] of String = (' Name = Melinda ', ' Name = John ');

var
  Processor: TDBIGenericMacroProcessor;

begin
  Processor := Local(TDBIGenericMacroProcessor.Create).Obj as TDBIGenericMacroProcessor;
  Processor.Scope.AddBinding('this', TDBIGlobalScope, nil);
  Processor.Scope.Top.Params.GetByName('ItemIndex').AsInteger := 5;
  Processor.Input.Text := C_Template;
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData[True]);

  Processor.Output.Clear;
  Processor.Scope.Top.Params.GetByName('ItemIndex').AsInteger := 0;
  Processor.Process;

  Assert(Processor.Output.Text = C_OutputData[False]);
end;


procedure TDBITokenizeUnitTests.Setup;
begin
  inherited Setup;

end;


procedure TDBITokenizeUnitTests.Teardown;
begin

  inherited TearDown;
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
  TomTestMastery.RegisterTestSuite(TDBITokenizeUnitTests);
{$else}
  RegisterTest('', TDBITokenizeUnitTests.Suite);
{$endif}

end.
