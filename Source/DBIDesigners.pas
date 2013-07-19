unit DBIDesigners;

{$I DBICompilers.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, DB, DSDesign,
  {$ifdef Delphi6} DesignIntf {$else} DsgnIntf {$endif} ;

type
{$ifdef Delphi6}
  IDBIDesigner = IDesigner;
{$else}
  IDBIDesigner = IFormDesigner;
{$endif}


type
  TFormODSDesigner = class(TForm)
    PageControl: TPageControl;
    TabSheetCode: TTabSheet;
    MemoBusinessObject: TMemo;
    TabSheetPreferences: TTabSheet;
    PanelBusinessObject: TPanel;
    SpeedButton1: TSpeedButton;
    PanelSettings: TPanel;
    CheckBoxCustomClass: TCheckBox;
    EditName: TEdit;
    CheckBoxAccessors: TCheckBox;
    procedure GenerateExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  protected
    function GetDataset: TDataset;

    property Dataset: TDataset read GetDataset;

  public
    { Public declarations }
  end;


type
  TDBIFieldsContainer = class(TComponent)
  private
    FEditorControls: TList;

  protected
    function GetEditorControls: TList;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);

    property EditorControls: TList read GetEditorControls;
    property Visible: Boolean read GetVisible write SetVisible;

  public
    destructor Destroy; override;

  end;

type
  TDBIFieldsEditor = class(TFieldsEditor)
  private
    FTabControl: TTabControl;
    FFieldsContainer: TDBIFieldsContainer;
    FODSEditor: TFormODSDesigner;

  protected
    procedure TabChanged(Sender: TObject);

    function GetFieldsContainer: TDBIFieldsContainer;
    function GetODSEditor: TFormODSDesigner;
    function GetTabControl: TTabControl;

    procedure Loaded; override;

    property FieldsContainer: TDBIFieldsContainer read GetFieldsContainer;
    property ODSEditor: TFormODSDesigner read GetODSEditor;
    property TabControl: TTabControl read GetTabControl;

  public
    destructor Destroy; override;
    procedure Load;

    class function CreateFieldsEditor(
      Designer: IDBIDesigner;
      ADataset: TDataset;
      DesignerClass: TDSDesignerClass;
      var Shared: Boolean
      ): TDBIFieldsEditor;

    class procedure ShowFieldsEditor(
      Designer: IDBIDesigner;
      ADataset: TDataset;
      DesignerClass: TDSDesignerClass
      );

  end;


type
  TDBIBusinessObjectPublisher = class(TPersistent)
  private
    FAccessors: Boolean;
    FCustomClass: Boolean;
    FDataset: TDataset;
    FName: String;

  protected
    function GetCount: Integer;
    function GetFieldName(const Index: Integer): String;
    function GetFieldSize(const Index: Integer): Word;
    function GetFieldType(const Index: Integer): TFieldType;
    function GetFieldTypeName(const Index: Integer): String;
    function GetTypeName(const Index: Integer): String;

  public
    procedure Publish(Output: TStrings);

    property Accessors: Boolean read FAccessors write FAccessors;
    property Count: Integer read GetCount;
    property CustomClass: Boolean read FCustomClass write FCustomClass;
    property Dataset: TDataset read FDataset write FDataset;
    property FieldNames[const Index: Integer]: String read GetFieldName;
    property FieldSizes[const Index: Integer]: Word read GetFieldSize;
    property FieldTypes[const Index: Integer]: TFieldType read GetFieldType;
    property FieldTypeNames[const Index: Integer]: String read GetFieldTypeName;
    property Name: String read FName write FName;
    property TypeNames[const Index: Integer]: String read GetTypeName;

  end;


var
  FormODSDesigner: TFormODSDesigner;

implementation

{$R *.DFM}

uses
  TypInfo;

  
{ TDBIBusinessObjectTemplate }

const
  TDBITypeName: array[TFieldType] of String = (
    'Unknown', 'String', 'Smallint', 'Integer', 'Word',
    'Boolean', 'Double', 'Currency', 'TBCD', 'TDate', 'TTime', 'TDateTime',
    'Bytes', 'VarBytes', 'AutoInc', 'Blob', 'String', 'Graphic', 'FmtMemo',
    'ParadoxOle', 'DBaseOle', 'TypedBinary', 'Cursor', 'FixedChar', 'WideString',
    'Largeint', 'ADT', 'Array', 'Reference', 'TDataSet', 'OraBlob', 'OraClob',
    'Variant', 'Interface', 'IDispatch', 'Guid'
{$ifdef Delphi2006}
    ,
    'TimeStamp', 'FMTBcd',
    'FixedWideChar', 'WideMemo', 'OraTimeStamp', 'OraInterval'
{$endif}

{$ifdef DelphiXE3}
    ,
    'LongWord', 'Shortint', 'Byte', 'Extended', 'Connection', 'Params', 'Stream',
    'TimeStampOffset', 'Object', 'Single'
{$endif}
    );

    
function TDBIBusinessObjectPublisher.GetCount: Integer;
begin
  if (Dataset.FieldCount > 0) then begin
    Result := Dataset.FieldCount;
  end
  else begin
    Result := Dataset.FieldDefs.Count;
  end;
end;


function TDBIBusinessObjectPublisher.GetFieldName(const Index: Integer): String;
begin
  if (Dataset.FieldCount > 0) then begin
    Result := Dataset.Fields[Index].FieldName;
  end
  else begin
    Result := Dataset.FieldDefs[Index].Name;
  end;
end;


function TDBIBusinessObjectPublisher.GetFieldSize(const Index: Integer): Word;
begin
  if (Dataset.FieldCount > 0) then begin
    Result := Dataset.Fields[Index].Size;
  end
  else begin
    Result := Dataset.FieldDefs[Index].Size;
  end;
end;


function TDBIBusinessObjectPublisher.GetFieldType(const Index: Integer): TFieldType;
begin
  if (Dataset.FieldCount > 0) then begin
    Result := Dataset.Fields[Index].DataType;
  end
  else begin
    Result := Dataset.FieldDefs[Index].DataType;
  end;
end;


function TDBIBusinessObjectPublisher.GetFieldTypeName(const Index: Integer): String;
begin
  Result := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(GetFieldType(Index)));
end;


function TDBIBusinessObjectPublisher.GetTypeName(const Index: Integer): String;
begin
  Result := TDBITypeName[getFieldType(Index)];
end;


procedure TDBIBusinessObjectPublisher.Publish(Output: TStrings);
const
  Prefix: array[Boolean] of String = ('Tom', 'TomCustom');
  PropertyScope: array[Boolean] of String = ('published', 'protected');
  UseFieldDefs = True;

var
  Index: Integer;

  function UseAccessors: Boolean;
  begin
    Result := Accessors;
  end;

  function UseCustom: Boolean;
  begin
    Result := CustomClass;
  end;

  procedure WriteFmt(const Msg: String; Args: array of const);
  begin
    Output.Add(Format(Msg, Args));
  end;

begin
  if (Dataset = nil) then begin
    Exit;
  end;
  Output.Clear;

  WriteFmt('unit %s;', [Name]);
  WriteFmt('', []);
  WriteFmt('interface', []);
  WriteFmt('', []);
  WriteFmt('uses', []);
  WriteFmt('  Classes, Sysutils, DB;', []);
  WriteFmt('', []);
  WriteFmt('type', []);
  WriteFmt('  %s%s = class(TPersistent)', [Prefix[UseCustom], Name]);

  WriteFmt('  private', []);
  for Index := 0 to Count-1 do begin
    WriteFmt('    F%s: %s;', [FieldNames[Index], TypeNames[Index]]);
  end;
  WriteFmt('', []);

  if UseAccessors then begin
    WriteFmt('  protected', []);
    for Index := 0 to Count-1 do begin
      WriteFmt('    function Get%s: %s;', [FieldNames[Index], TypeNames[Index]]);
      WriteFmt('    procedure Set%s(const Value: %s);',[FieldNames[Index], TypeNames[Index]]);
      WriteFmt('', []);
    end;
  end;

  WriteFmt('  %s', [PropertyScope[UseCustom]]);
  for Index := 0 to Count-1 do begin
    if UseAccessors then begin
      WriteFmt('    property %0:s: %1:s read Get%0:s write Set%0:s;', [FieldNames[Index], TypeNames[Index]]);
    end
    else begin
      WriteFmt('    property %0:s: %1:s read F%0:s write F%0:s;', [FieldNames[Index], TypeNames[Index]]);
    end;
  end;
  WriteFmt('', []);

  if UseFieldDefs then begin
    WriteFmt('  public', []);
    WriteFmt('    class procedure BuildFieldDefs(ADataset: TDataset);', []);
    WriteFmt('', []);
  end;

  WriteFmt('  end;', []);
  WriteFmt('', []);

  if UseCustom then begin
    WriteFmt('', []);
    WriteFmt('type', []);
    WriteFmt('  Tom%0:s = class(TomCustom%0:s)', [Name]);

    WriteFmt('  published', []);
    for Index := 0 to Count-1 do begin
      WriteFmt('    property %s;', [FieldNames[Index]]);
    end;
    WriteFmt('', []);

    WriteFmt('  end;', []);
    WriteFmt('', []);
  end;

  WriteFmt('implementation', []);
  WriteFmt('', []);

  // Publish Build FieldDefs
  if UseFieldDefs then begin
    WriteFmt('class procedure %s%s.BuildFieldDefs(ADataset: TDataset);', [Prefix[UseCustom], Name]);
    WriteFmt('var', []);
    WriteFmt('  FieldDef: TFieldDef;', []);
    WriteFmt('', []);
    WriteFmt('begin', []);

    for Index := 0 to Count-1 do begin
      WriteFmt('  FieldDef := ADataset.FieldDefs.AddFieldDef;', []);
      WriteFmt('  FieldDef.Name := ''%s'';', [FieldNames[Index]]);
      WriteFmt('  FieldDef.DataType := %s;', [FieldTypeNames[Index]]);

      if (FieldSizes[Index] > 0) then begin
        WriteFmt('  FieldDef.Size := %d;', [FieldSizes[Index]]);
      end;
      WriteFmt('', []);
   end;

    WriteFmt('end;', []);
    WriteFmt('', []);
  end;

  // Publish Accessors
  if UseAccessors then begin
    for Index := 0 to Count-1 do begin
      WriteFmt(
        'function %s%s.Get%s: %s;',
        [Prefix[UseCustom], Name, FieldNames[Index], TypeNames[Index]]
        );
      WriteFmt('begin', []);
      WriteFmt('  Result := F%s;', [FieldNames[Index]]);
      WriteFmt('end;', []);
      WriteFmt('', []);

      WriteFmt(
        'procedure %s%s.Set%s(const Value: %s);',
        [Prefix[UseCustom], Name, FieldNames[Index], TypeNames[Index]]
        );
      WriteFmt('begin', []);
      WriteFmt('  F%s := Value;', [FieldNames[Index]]);
      WriteFmt('end;', []);
      WriteFmt('', []);
    end;
  end;

  WriteFmt('initialization', []);
  WriteFmt('  Classes.RegeisterClass(%s%s)', [Prefix[UseCustom], Name]);
  WriteFmt('', []);
  WriteFmt('end.', []);
end;


{ TDBIFieldsEditorEditor }

class function TDBIFieldsEditor.CreateFieldsEditor(
  Designer: IDBIDesigner;
  ADataset: TDataset;
  DesignerClass: TDSDesignerClass;
  var Shared: Boolean
  ): TDBIFieldsEditor;
begin
  Shared := True;

  if Assigned(ADataset.Designer) then begin
    Result := (ADataset.Designer as TDSDesigner).FieldsEditor as TDBIFieldsEditor;
  end
  else begin
    Result := TDBIFieldsEditor.Create(Application);
    Result.Name := 'FieldsEditor';
    Result.DSDesignerClass := DesignerClass;
    Result.Designer := Designer;
    Result.Dataset := ADataset;

    Shared := False;
  end;

  Result.Load;
end;  { CreateFieldsEditor }


destructor TDBIFieldsEditor.Destroy;
begin
  FFieldsContainer.Free;
  FFieldsContainer := nil;
  
  inherited Destroy;
end;  { Destroy }


function TDBIFieldsEditor.GetFieldsContainer: TDBIFieldsContainer;
begin
  if not Assigned(FFieldsContainer) then begin
    FFieldsContainer := TDBIFieldsContainer.Create(Self);
  end;
  Result := FFieldsContainer;
end;


function TDBIFieldsEditor.GetODSEditor: TFormODSDesigner;
begin
  if not Assigned(FODSEditor) then begin
    FODSEditor := TFormODSDesigner.Create(Self);
    FODSEditor.Visible := False;
    FODSEditor.Parent := Self;
    FODSEditor.Align := alClient;
    FODSEditor.BorderStyle := bsNone;
    FODSEditor.Top := Height;
  end;
  Result := FODSEditor;
end;


function TDBIFieldsEditor.GetTabControl: TTabControl;
begin
  if not Assigned(FTabControl) then begin
    FTabControl := TTabControl.Create(Self);
    FTabControl.Parent := Self;
    FTabControl.Align := alTop;
    FTabControl.Top := 0;
    FTabControl.Height := 30;

    FTabControl.Tabs.Add('Field Editor');
    FTabControl.Tabs.Add('Business Object');
    FTabControl.TabIndex := 0;
    FTabControl.Style := tsFlatButtons;
    FTabControl.OnChange := TabChanged;
  end;
  Result := FTabControl
end;  { GetTabControl }


procedure TDBIFieldsEditor.Load;
begin
  ODSEditor.PageControl.ActivePage := ODSEditor.TabSheetPreferences;
end;


procedure TDBIFieldsEditor.Loaded;
begin
  inherited Loaded;

  FieldsContainer.Visible := True;
  TabControl.Visible := True;
end;  { Loaded }


class procedure TDBIFieldsEditor.ShowFieldsEditor(
  Designer: IDBIDesigner;
  ADataset: TDataset;
  DesignerClass: TDSDesignerClass
  );
var
  FieldsEditor: TFieldsEditor;
  vShared: Boolean;

begin
  FieldsEditor := TDBIFieldsEditor.CreateFieldsEditor(Designer, ADataSet, DesignerClass, vShared);
  if Assigned(FieldsEditor) then begin
    FieldsEditor.Show;
  end;
end;  { ShowFieldsEditor }


procedure TDBIFieldsEditor.TabChanged(Sender: TObject);
begin
  case TabControl.TabIndex of
    0: begin
      ODSEditor.Visible := False;
      FieldsContainer.Visible := True;
    end;

    1: begin
      FieldsContainer.Visible := False;
      ODSEditor.Visible := True;
    end;
  end;
end;





{ TFormODSDesigner }

function TFormODSDesigner.GetDataset: TDataset;
var
  FieldsEditor: TFieldsEditor;

begin
  FieldsEditor := Owner as TDBIFieldsEditor;
  Assert(Assigned(FieldsEditor));

  Result := FieldsEditor.Dataset;
end;

procedure TFormODSDesigner.PageControlChange(Sender: TObject);
begin
  if (PageControl.ActivePage = TabSheetCode) then begin
    GenerateExecute(Sender);
  end;
end;

procedure TFormODSDesigner.GenerateExecute(Sender: TObject);
var
  Publisher: TDBIBusinessObjectPublisher;

begin
  Publisher := TDBIBusinessObjectPublisher.Create;
  try
    Publisher.Dataset := GetDataset;
    Publisher.Name := EditName.Text;
    Publisher.CustomClass := CheckBoxCustomClass.Checked;
    Publisher.Accessors := CheckBoxAccessors.Checked;
    Publisher.Publish(MemoBusinessObject.Lines);
  finally
    Publisher.Free;
  end;
end;





{ TDBIFieldsContainer }

destructor TDBIFieldsContainer.Destroy;
begin
  FEditorControls.Free;
  FEditorControls := nil;

  inherited Destroy;
end;

function TDBIFieldsContainer.GetEditorControls: TList;
var
  FieldsEditor: TDBIFieldsEditor;
  Index: Integer;

begin
  FieldsEditor := Owner as TDBIFieldsEditor;

  if not Assigned(FEditorControls) then begin
    FieldsEditor.Panel1.Visible := False;
    FEditorControls := TList.Create;

    for Index := 0 to FieldsEditor.ControlCount-1 do begin
      if FieldsEditor.Controls[Index].Visible then begin
        FEditorControls.Add(FieldsEditor.Controls[Index]);
      end;
    end;

    FieldsEditor.BorderWidth := 5;
    FieldsEditor.Caption := 'ObjectListDataset Designer';
    FieldsEditor.Height := 400;
    FieldsEditor.Width := 400;

    FieldsEditor.DBNavigator.Flat := True;
    FieldsEditor.DBNavigator.Align := alNone;
    FieldsEditor.DBNavigator.Width := 50;

    FieldsEditor.FieldListbox.Font.Size := 10;
    FieldsEditor.FieldListbox.BorderStyle := bsNone;;
  end;
  Result := FEditorControls;
end;  { GetEditorControls }



function TDBIFieldsContainer.GetVisible: Boolean;
var
  Item: TControl;

begin
  Item := EditorControls.Items[0];
  Result := Item.Visible;
end;


procedure TDBIFieldsContainer.SetVisible(const Value: Boolean);
var
  Index: Integer;
  Item: TControl;

begin
  for Index := 0 to EditorControls.Count-1 do begin
    Item := EditorControls.Items[Index];
    Item.Visible := Value;
  end;
end;


end.
