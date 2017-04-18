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
  1.0 | 09/10/2002 08:18:01 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIDesigners;

{$I DBICompilers.inc}

interface

uses
{$ifdef fpc}
  ComponentEditors, FieldsEditor, ToolWin,
{$else}
  {$ifdef Delphi6} DesignIntf, {$else} DsgnIntf, {$endif} DSDesign,
{$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, DB, DBIConst, Menus;

type
{$ifdef fpc}
  TFieldsEditor = TDSFieldsEditorFrm;
{$else}
  {$ifdef Delphi6} IDBIDesigner = IDesigner; {$else} IDBIDesigner = IFormDesigner; {$endif}
{$endif}

  EDBIPropertyEditorException = class(EDBICustomException);


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
    BevelIntro: TBevel;
    BevelName: TBevel;
    BevelClass: TBevel;
    BevelAccessors: TBevel;
    LabelIntro1: TLabel;
    LabelIntro2: TLabel;
    LabelName: TLabel;
    LabelClass: TLabel;
    LabelAccessors: TLabel;
    PopupMenuCode: TPopupMenu;
    PopupMenuPreferences: TPopupMenu;
    EditCutItem: TMenuItem;
    EditCopyItem: TMenuItem;
    EditPasteItem: TMenuItem;
    EditDeleteItem: TMenuItem;
    EditSelectAllItem: TMenuItem;
    N1: TMenuItem;
    EditSaveItem: TMenuItem;
    SaveDialog: TSaveDialog;

    procedure EditCopyItemExecute(Sender: TObject);
    procedure EditSelectAllExecute(Sender: TObject);
    procedure GenerateExecute(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure EditCutItemExecute(Sender: TObject);
    procedure EditPasteItemExecute(Sender: TObject);
    procedure EditDeleteItemExecute(Sender: TObject);
    procedure EditSaveItemExecute(Sender: TObject);
  protected
    function GetDataset: TDataset;

  public

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
{$ifdef fpc}
    FDataset: TDataset;
{$endif}
    FTabControl: TTabControl;
    FFieldsContainer: TDBIFieldsContainer;
    FODSEditor: TFormODSDesigner;

  protected
    procedure TabChanged(Sender: TObject);

    function GetFieldsContainer: TDBIFieldsContainer;
    function GetODSEditor: TFormODSDesigner;
    function GetTabControl: TTabControl;

    procedure Loaded; override;
{$ifdef fpc}
    property FieldListBox: TListbox read FieldsListBox;
{$endif}
    property FieldsContainer: TDBIFieldsContainer read GetFieldsContainer;
    property ODSEditor: TFormODSDesigner read GetODSEditor;
    property TabControl: TTabControl read GetTabControl;

  public
    destructor Destroy; override;
    procedure Display;

{$ifndef fpc}
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
{$else}
    class function CreateFieldsEditor(Editor: TFieldsComponentEditor): TDBIFieldsEditor;
    class procedure ShowFieldsEditor(Editor: TFieldsComponentEditor);

    property Dataset: TDataset read FDataset write FDataset;
{$endif}
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
    'Unknown', 'String', 'Smallint', 'Integer', 'Word', 'Boolean', // 0..5
    'Double', 'Currency', 'TBCD', 'TDate', 'TTime', 'TDateTime', 'Bytes', // 6..12
    'VarBytes', 'AutoInc', 'Blob', 'String', 'Graphic', 'FmtMemo', 'ParadoxOle', // 13..19
    'DBaseOle', 'TypedBinary', 'Cursor', 'FixedChar', // 20..23
    'WideString', 'Largeint', 'ADT', // 24..26
    'Array', 'Reference', 'TDataSet', 'OraBlob', 'OraClob', 'Variant', 'Interface', // 27..33
    'IDispatch', 'Guid' {$ifdef Delphi6} , 'TimeStamp', 'FMTBcd' {$endif} //36..37
    {$ifdef DELPHI2006} , 'FixedWideChar', 'WideMemo' {$ifndef fpc}, 'OraTimeStamp', 'OraInterval' {$endif} {$endif} // 38..41
    {$ifdef DelphiXE2} ,
    'LongWord', 'Shortint', 'Byte', 'Extended', 'Connection', 'Params', 'Stream', //42..48
    'TimeStampOffset', 'Object', 'Single' // 49..51
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
  Prefix: array[Boolean] of String = ('T', 'TCustom');
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
      if Dataset.Fields[Index].FieldKind = fkData then begin
        WriteFmt('  FieldDef := ADataset.FieldDefs.AddFieldDef;', []);
        WriteFmt('  FieldDef.Name := ''%s'';', [FieldNames[Index]]);
        WriteFmt('  FieldDef.DataType := %s;', [FieldTypeNames[Index]]);

        if (FieldSizes[Index] > 0) then begin
          WriteFmt('  FieldDef.Size := %d;', [FieldSizes[Index]]);
        end;
        WriteFmt('', []);
      end;
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
  WriteFmt('  Classes.RegisterClass(%s%s)', [Prefix[UseCustom], Name]);
  WriteFmt('', []);
  WriteFmt('end.', []);
end;


{ TDBIFieldsEditor }
{$ifndef fpc}

class function TDBIFieldsEditor.CreateFieldsEditor(
  Designer: IDBIDesigner;
  ADataset: TDataset;
  DesignerClass: TDSDesignerClass;
  var Shared: Boolean
  ): TDBIFieldsEditor;
begin
  Shared := Assigned(ADataset.Designer);

  if Shared then begin
    Result := (ADataset.Designer as TDSDesigner).FieldsEditor as TDBIFieldsEditor;
  end
  else begin
    Result := TDBIFieldsEditor.Create(Application);
    Result.DSDesignerClass := DesignerClass;
    Result.Designer := Designer;
    Result.Dataset := ADataset;
  end;
end;  { CreateFieldsEditor }

{$else}

class function TDBIFieldsEditor.CreateFieldsEditor(
  Editor: TFieldsComponentEditor
  ): TDBIFieldsEditor;
var
  Instance: TObject;
  ADataset: TDataset;

begin
  ADataset := Editor.GetComponent as TDataset;
  if (ADataset = nil) then begin
    raise Exception.Create('TFieldsComponentEditor.Edit LinkDataset=nil');
  end;

  Instance := FindEditorForm(ADataset);
  if Assigned(Instance) then begin
    Result := Instance as Self;
  end
  else begin
    Result := Self.Create(Application, ADataset, Editor.Designer);
    RegisterEditorForm(Result, ADataset);
  end;

  if Assigned(Result) then begin
    Result.ComponentEditor := Editor;
    Result.Dataset := ADataset;
  end;
end;  { CreateFieldsEditor }

{$endif}


destructor TDBIFieldsEditor.Destroy;
begin
  FFieldsContainer.Free;
  FFieldsContainer := nil;

  inherited Destroy;
end;  { Destroy }


procedure TDBIFieldsEditor.Display;
begin
  ODSEditor.PageControl.ActivePage := ODSEditor.TabSheetPreferences;

  Show;
  BringToFront;
end;


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


procedure TDBIFieldsEditor.Loaded;
begin
  inherited Loaded;

  FieldsContainer.Visible := True;
  TabControl.Visible := True;
end;  { Loaded }


{$ifdef fpc}
class procedure TDBIFieldsEditor.ShowFieldsEditor(
  Editor: TFieldsComponentEditor
  );
var
  FieldsEditor: TDBIFieldsEditor;

{$else}
class procedure TDBIFieldsEditor.ShowFieldsEditor(
  Designer: IDBIDesigner;
  ADataset: TDataset;
  DesignerClass: TDSDesignerClass
  );
var
  FieldsEditor: TDBIFieldsEditor;
  vShared: Boolean;

{$endif}
begin
  try
{$ifdef fpc}
    FieldsEditor := TDBIFieldsEditor.CreateFieldsEditor(Editor);
{$else}
    FieldsEditor := TDBIFieldsEditor.CreateFieldsEditor(Designer, ADataSet, DesignerClass, vShared);
{$endif}
    if Assigned(FieldsEditor) then begin
      FieldsEditor.Display;
    end;
  except
    on E: Exception do
      raise EDBIPropertyEditorException.Create(
        Self, E, 'ShowFieldsEditor::585', 'Fields Editor Failed!', []
        );
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

function TFormODSDesigner.{%H-}GetDataset: TDataset;
var
  FieldsEditor: TDBIFieldsEditor;

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


procedure TFormODSDesigner.EditCutItemExecute(Sender: TObject);
begin
  MemoBusinessObject.CutToClipBoard;
end;


procedure TFormODSDesigner.EditCopyItemExecute(Sender: TObject);
begin
  MemoBusinessObject.CopyToClipBoard;
end;


procedure TFormODSDesigner.EditPasteItemExecute(Sender: TObject);
begin
  MemoBusinessObject.PasteFromClipBoard;
end;


procedure TFormODSDesigner.EditDeleteItemExecute(Sender: TObject);
begin
  MemoBusinessObject.Clear;
end;


procedure TFormODSDesigner.EditSelectAllExecute(Sender: TObject);
begin
  MemoBusinessObject.SelectAll;
end;


procedure TFormODSDesigner.EditSaveItemExecute(Sender: TObject);
begin
  SaveDialog.Filename := ChangeFileExt(EditName.Text, '.pas');
  if SaveDialog.Execute then begin
    MemoBusinessObject.Lines.SaveToFile(saveDialog.Filename);
  end;
end;


function TDBIFieldsContainer.{%H-}GetEditorControls: TList;
var
  FieldsEditor: TDBIFieldsEditor;
  Index: Integer;

begin
  FieldsEditor := Owner as TDBIFieldsEditor;

  if not Assigned(FEditorControls) then begin
{$ifndef fpc}
    FieldsEditor.Panel1.Visible := False;
{$endif}
    FEditorControls := TList.Create;

    for Index := 0 to FieldsEditor.ControlCount-1 do begin
      if FieldsEditor.Controls[Index].Visible then begin
        FEditorControls.Add(FieldsEditor.Controls[Index]);
      end;
    end;

    FieldsEditor.BorderWidth := 5;
    FieldsEditor.Caption := 'ObjectListDataset Designer';
    FieldsEditor.Height := 400;
    FieldsEditor.Width := 420;
{$ifdef fpc}
    FieldsEditor.TBCommands.Color := clWindow;
    FieldsEditor.TBCommands.EdgeBorders := [ebBottom, ebLeft, ebRight, ebTop];
{$else}
    FieldsEditor.DBNavigator.Flat := True;
    FieldsEditor.DBNavigator.Align := alNone;
    FieldsEditor.DBNavigator.Width := 50;
    FieldsEditor.FieldListbox.BorderStyle := bsNone;
{$endif}
    FieldsEditor.FieldListbox.Font.Size := 10;
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


initialization
  Classes.RegisterClass(TBevel);
  Classes.RegisterClass(TCheckbox);
  Classes.RegisterClass(TEdit);
  Classes.RegisterClass(TForm);
  Classes.RegisterClass(TLabel);
  Classes.RegisterClass(TPageControl);
  Classes.RegisterClass(TSpeedButton);
  Classes.RegisterClass(TTabSheet);

end.
