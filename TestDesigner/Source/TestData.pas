unit TestData;

interface

uses
  SysUtils, Windows, Forms, Dialogs, DB, Grids, DBGrids, DBCtrls, Controls, Buttons, Classes, ExtCtrls,
  Contnrs, DBIDataset, DBIObjectListDatasets;

type
  TDataString = AnsiString;

  TSimple = class(TPersistent)
  private
    FID: Integer;
    FDescription: TDataString;
    FComment: WideString;

  published
    property ID: Integer read FID write FID;
    property Description: TDataString read FDescription write FDescription;
    property Comment: WideString read FComment write FComment;

  end;

type
  TomString = WideString;

  TTestData = class(TPersistent)
  private
    FEnvironment: TomString;
    FFullName: TomString;
    FValue: TomString;

  protected
    function GetApplication: TomString;
    function GetPath: TomString;
    function GetName: TomString;

  public
    class procedure AddData(ODS: TObjectListDataset);
    class procedure CreateFields(ADataset: TDataset);
    class function Build(ODS: TObjectListDataset; const Env, Full, Val: String): TTestData;

  published
    property Environment: TomString read FEnvironment write FEnvironment;
    property FullName: TomString read FFullName write FFullName;
    property Value: TomString read FValue write FValue;

    property Application: TomString read GetApplication;
    property Path: TomString read GetPath;
    property Name: TomString read GetName;
  end;



implementation

function AddField(const FieldName: String; const Datatype: TFieldType; Adataset: TDataset): TField;
begin
  Result := nil;
  case DataType of
    ftDate: Result := TDateField.Create(ADataset);
    ftDateTime: Result := TDateTimeField.Create(ADataset);
    ftInteger: Result := TIntegerField.Create(ADataset);
    ftFloat: Result := TFloatField.Create(ADataset);
    ftString: Result := TStringField.Create(ADataset);
    ftWideString: Result := TWideStringField.Create(ADataset);
    ftWord: Result := TWordField.Create(ADataset);
  end;
  Result.FieldName := FieldName;
  Result.Dataset := ADataset;
end;


class function TTestData.Build(ODS: TObjectListDataset; const Env, Full, Val: String): TTestData;
begin
  Result := Self.Create;
  Result.Environment := Env;
  Result.FullName := Full;
  Result.Value := Val;
  
  ODS.List.Add(Result);
end;

class procedure TTestData.CreateFields(ADataset: TDataset);
const
  ftDefaultString = ftString;

begin
  if (ADataset is TObjectListDataset) then begin
    (ADataset as TObjectListDataset).ClassTypeName := Self.ClassName;
  end;

  ADataset.FieldDefs.Clear;
  ADataset.Fields.Clear;

  AddField('Environment', ftDefaultString, ADataset).Size := 20;
  AddField('FullName', ftDefaultString, ADataset).Size := 20;
  AddField('Value', ftDefaultString, ADataset).Size := 20;
  AddField('Application', ftDefaultString, ADataset).Size := 20;
  AddField('Path', ftDefaultString, ADataset).Size := 20;
  AddField('Name', ftDefaultString, ADataset).Size := 20;
end;

function TTestData.GetApplication: TomString;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;

function TTestData.GetName: TomString;
begin
  Result := ChangeFileExt(ExtractFileName(Path + '.jvr'), '');
end;

function TTestData.GetPath: TomString;
begin
  Result := ExtractFileDir(ParamStr(0));
end;


class procedure TTestData.AddData(ODS: TObjectListDataset);
begin
{##JVR
   ODS := TObjectListDataset.Create(Self);
    ODS.ClassTypeName := TSimple.ClassName;
    ODS.StringFieldSize := 20;

    Instance := TSimple.Create;
    Instance.ID := 42;
    Instance.Description := 'John Vander Reest';
    Instance.Comment := 'This is a WideString';

    ODS.List.Add(Instance);
//}
//    TTestData.CreateFields(ODS);

//{##JVR
    TTestData.Build(ODS, 'Windows', 'John Vander Reest', 'Maybe');
    TTestData.Build(ODS, 'Linux', 'Samuel Vander Reest', 'Yes');
    TTestData.Build(ODS, 'MacOS', 'Melinda Vander Reest', 'No');
    TTestData.Build(ODS, 'Android', 'Charlotte Vander Reest', 'Yes');
//}

{##JVR
    ODS.CreateDataset;
(*##JVR
//}
    ODS.Open;
//*)
    ODS.SaveToFile('C:\Temp\jvr.dbf');

//    ODS.First;
//    ShowMessageFmt('FullName := %s', [ODS.FieldByName('FullName').AsString]);
end;

initialization
  Classes.RegisterClass(TTestData);
  Classes.RegisterClass(TSimple);
end.
