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


{$ifdef UseCustomMemo}
type
  TMemo = class(StdCtrls.TMemo)
  private
    FStartChar, FEndChar: Integer;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  public
    procedure Underline(StartLine, StartChar, EndLine, EndChar: Integer);
  end;
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
    function GetFieldType(const Index: Integer): TFieldType;
    function GetTypeName(const Index: Integer): String;

  public
    procedure Publish(Output: TStrings);

    property Accessors: Boolean read FAccessors write FAccessors;
    property Count: Integer read GetCount;
    property CustomClass: Boolean read FCustomClass write FCustomClass;
    property Dataset: TDataset read FDataset write FDataset;
    property FieldNames[const Index: Integer]: String read GetFieldName;
    property FieldTypes[const Index: Integer]: TFieldType read GetFieldType;
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


function TDBIBusinessObjectPublisher.GetFieldType(const Index: Integer): TFieldType;
begin
  if (Dataset.FieldCount > 0) then begin
    Result := Dataset.Fields[Index].DataType;
  end
  else begin
    Result := Dataset.FieldDefs[Index].DataType;
  end;
end;


function TDBIBusinessObjectPublisher.GetTypeName(const Index: Integer): String;
begin
  Result := TDBITypeName[getFieldType(Index)];
end;


procedure TDBIBusinessObjectPublisher.Publish(Output: TStrings);
const
  Prefix: array[Boolean] of String = ('Tom', 'TomCustom');
  PropertyScope: array[Boolean] of String = ('published', 'protected');

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

//##JVR    Item.Visible := Item.Visible and not (Item is TPanel);
  end;
end;





{ TMemo }

{$ifdef UseCustomMemo}
procedure TMemo.Underline(StartLine, StartChar, EndLine, EndChar: Integer);
begin
  FStartChar := SendMessage(Handle, EM_LINEINDEX, StartLine, 0) + StartChar;
  FEndChar := SendMessage(Handle, EM_LINEINDEX, EndLine, 0) + EndChar;
  Invalidate;
end;

procedure TMemo.WMPaint(var Msg: TWMPaint);

  function GetLine(CharPos: Integer): Integer;
  begin
    Result := SendMessage(Handle, EM_LINEFROMCHAR, CharPos, 0);
  end;

  procedure DrawLine(First, Last: Integer);
  var
    LineHeight: Integer;
    Pt1, Pt2: TSmallPoint;
    DC: HDC;
    Rect: TRect;
    ClipRgn: HRGN;
  begin
    // font height approximation (compensate 1px for internal leading)
    LineHeight := Abs(Font.Height) - Abs(Font.Height) div Font.Height;

    // get logical top-left coordinates for line bound characters
    Integer(Pt1) := SendMessage(Handle, EM_POSFROMCHAR, First, 0);
    Integer(Pt2) := SendMessage(Handle, EM_POSFROMCHAR, Last, 0);

    DC := GetDC(Handle);

    // clip to not to draw to non-text area (internal margins)
    SendMessage(Handle, EM_GETRECT, 0, Integer(@Rect));
    ClipRgn := CreateRectRgn(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    SelectClipRgn(DC, ClipRgn);
    DeleteObject(ClipRgn); // done with region

    // set pen color to red and draw line
    SelectObject(DC, GetStockObject(DC_PEN));
    SetDCPenColor(DC, RGB(255, 0 ,0));
    MoveToEx(DC, Pt1.x, Pt1.y + LineHeight, nil);
    LineTo(DC, Pt2.x, Pt2.y + LineHeight);

    ReleaseDC(Handle, DC); // done with dc
  end;

var
  StartChar, CharPos, LinePos: Integer;

begin
  inherited;

  if FEndChar > FStartChar then begin
    // Find out where to draw.
    // Can probably optimized a bit by using EM_LINELENGTH
    StartChar := FStartChar;
    CharPos := StartChar;
    LinePos := GetLine(CharPos);
    while True do begin
      Inc(CharPos);
      if GetLine(CharPos) > LinePos then begin
        DrawLine(StartChar, CharPos - 1);
        StartChar := CharPos;
        Dec(CharPos);
        Inc(LinePos);
        Continue;
      end else
        if CharPos >= FEndChar then begin
          DrawLine(StartChar, FEndChar);
          Break;
        end;
    end;
  end;
end;
{$endif}

(*##JVR
procedure TMemo.WMPaint(var Message: TWMPaint);
var
  Buffer: array[0..4096] of Char;
  PS: TPaintStruct;
  DC: HDC;
  i: Integer;
  X, Y, Z: Integer;
  OldColor: TColor;

  LineNo: Integer;

begin
LineNo := 230;
try
if Lines.Count > 0 then begin
LineNo := 231;
  DC := Message.DC;
LineNo := 235;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);
LineNo := 238;
  try
LineNo := 240;
    X := 1;
LineNo := 242;
    Y := 1;
LineNo := 244;
    SetBkColor(DC, Color);
LineNo := 246;
    SetBkMode(DC, Transparent);
LineNo := 248;
    OldColor := Font.Color;
LineNo := 250;
    for i:=0 to Pred(Lines.Count) do begin
LineNo := 253;
      { this is your place to set/reset the font... }
      if odd(i) then
        SetTextColor(DC, clRed)
      else
        SetTextColor(DC, OldColor);
LineNo := 259;
      Z := Length(Lines[i]);
LineNo := 261;
      StrPCopy(Buffer, Lines[i]);
LineNo := 2663;
      Buffer[Z] := #0;  { not really needed }
LineNo := 265;
      TextOut(DC, X, Y, Buffer, Z);
LineNo := 267;
      Inc(Y, abs(Font.Height));
    end;
  finally
LineNo := 270;
    if Message.DC = 0 then begin
LineNo := 273;
      EndPaint(Handle, PS);
    end;
  end;
end;
except
  on E: Exception do
    ShowMessageFmt('%d, %s', [LineNo, E.Message]);
end;
end;
//*)


end.
