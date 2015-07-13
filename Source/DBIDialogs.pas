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
  1.0 | 08/07/2013 07:53:42 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIDialogs;

interface

{$I DBICompilers.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DB, DBGrids;

type
  TDBICancelButton = class(TButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;


type
  TDBIOKButton = class(TButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;


type
  TDBIMemoPosition = (mpColumn, mpPosition, mpRow);

  TDBIMemo = class(TMemo)
  private
    FStartIndex: Integer;
    FEndIndex: Integer;
    FHighLightColor: TColor;
    FHighLightedText: String;

    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var Message: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;

  protected
    procedure DrawLine(First, Last: Integer);
    function GetPosition(const Index: TDBIMemoPosition): Integer;
    function GetRow(const CharIndex: Integer): Integer;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function SelectWordAtCaret: string;
    function ValidateSelection(const Value: String): Boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;

    procedure HighLight(const StartIndex, EndIndex: Word);

    property HighLightColor: TColor read FHighLightColor write FHighLightColor;
    property HighLightedText: String read FHighLightedText;

    property Column: Integer index mpColumn read GetPosition;
    property Position: Integer index mpPosition read GetPosition;
    property Row: Integer index mpRow read GetPosition;

  end;


type
  TDBICustomDialog = class(TForm)
  private
    FButtonOK: TButton;
    FButtonCancel: TButton;
    FClient: TPanel;
    FComment: TLabel;
    FEdit: TEdit;
    FFooter: TPanel;
    FImage: TImage;
    FListBox: TListBox;
    FShape: TShape;

  protected
    function GetButtonCancel: TButton; virtual;
    function GetButtonOK: TButton; virtual;
    function GetClient: TPanel; virtual;
    function GetComment: TLabel; virtual;
    function GetEdit: TEdit; virtual;
    function GetFooter: TPanel; virtual;
    function GetImage: TImage; virtual;
    function GetListBox: TListBox; virtual;
    function GetShape: TShape;

    property ButtonOK: TButton read GetButtonOK;
    property ButtonCancel: TButton read GetButtonCancel;
    property Client: TPanel read GetClient;
    property Comment: TLabel read GetComment;
    property Edit: TEdit read GetEdit;
    property Footer: TPanel read GetFooter;
    property Image: TImage read GetImage;
    property ListBox: TListBox read GetListBox;
    property Shape: TShape read GetShape;

  public
    class function BuildDialog(AOwner: TComponent = nil): TDBICustomDialog; virtual; abstract;

  end;


type
  TDBIDialogDataset = class(TDBICustomDialog)
  private
    FDBGrid: TDBGrid;
    FDataSource: TDataSource;

  protected
    function GetDBGrid: TDBGrid;
    function GetDataSource: TDataSource;

    property DataSource: TDataSource read GetDataSource;
    property DBGrid: TDBGrid read GetDBGrid;

  public
    class function BuildDialog(AOwner: TComponent = nil): TDBICustomDialog; override;
    class function Execute(ADataset: TDataset; const ATitle: String = ''): Boolean;

 end;


type
  TDBIDialogListbox = class(TDBICustomDialog)
  public
    class function BuildDialog(AOwner: TComponent = nil): TDBICustomDialog; override;

  end;


type
  TDBIDialogSelect = class(TDBIDialogListBox)
  public
    class function Execute(Strings: TStrings; const ATitle: String = 'Select'): String; overload;
  end;


type
  TDBIDialogAsk = class(TDBIDialogListBox)
  public
    class function Execute(Strings: TStrings; const ATitle: String = 'Select'): Boolean; overload;
  end;


type
  TDBIDialogProgress = class(TDBICustomDialog)
  private
    FBar: TProgressBar;
    FCount: Integer;
    FIndex: Integer;
    FMessage: String;
    FDelay: Integer;

  protected
    function GetCount: Integer;
    function GetProgressBar: TProgressBar;
    function GetMessage: String;

    procedure Idle;

    procedure SetCount(const Value: Integer);
    procedure SetMessage(const Value: String);

  public
    class function BuildDialog(AOwner: TComponent = nil): TDBICustomDialog; override;
    class procedure Wait(Delay: Integer = 1000; Message: String = ''; Caption: String = '');

    procedure First;
    procedure Last;
    procedure Next;

    property Bar: TProgressBar read GetProgressBar;
    property Count: Integer read GetCount write SetCount;
    property Delay: Integer read FDelay write FDelay default 0;
    property Index: Integer read FIndex;
    property Message: String read GetMessage write SetMessage;

  end;


implementation

{. $R *.DFM}

uses
  DBIUtils;

const
  clHotLight = $00CC6600;

{ TDBIDialogProgress }

class function TDBIDialogProgress.BuildDialog(AOwner: TComponent = nil): TDBICustomDialog;
begin
  Result := Self.CreateNew(AOwner);
  Result.BorderStyle := bsDialog;
  Result.BorderIcons := [];
  Result.Color := clWindow;
  Result.FormStyle := fsStayOnTop;
  Result.Position := poScreenCenter;
  Result.Height := 130;
  Result.Width := 450;

  Result.Client.BorderWidth := 20;
  Result.Image;
  Result.Shape;
end;


procedure TDBIDialogProgress.First;
begin
  FIndex := 0;

  Bar.Min := 0;
  Bar.Max := 999;
  Bar.Position := 0;

  Idle;
end;


function TDBIDialogProgress.GetCount: Integer;
begin
  Result := FCount;
end;


function TDBIDialogProgress.GetProgressBar: TProgressBar;
begin
  if not Assigned(FBar) then begin
    FBar := TProgressBar.Create(Self);
    FBar.Align := alTop;
    FBar.Parent := Client;
  end;
  Result := FBar;
end;


function TDBIDialogProgress.GetMessage: String;
begin
  Result := FMessage;
end;


procedure TDBIDialogProgress.Idle;
begin
  if Delay > 0 then begin
    Sleep(Delay);
  end;

  Application.ProcessMessages;
end;


procedure TDBIDialogProgress.Last;
begin
  FIndex := FCount-1;

  Bar.Max := 99;
  Bar.Position := Bar.Max;

  Idle;
end;


procedure TDBIDialogProgress.Next;
begin
  Inc(FIndex);
  Bar.Position := (FIndex * Bar.Max) div FCount;

  if (BorderStyle <> bsNone) then begin
    Comment.Caption := Format('%s  #%d of %d', [Message, (FIndex * 100) div FCount, 100]);
  end;

  Idle;
end;


procedure TDBIDialogProgress.SetCount(const Value: Integer);
begin
  FCount := Value;

  First;
end;


procedure TDBIDialogProgress.SetMessage(const Value: String);
begin
  FMessage := Value;
end;


class procedure TDBIDialogProgress.Wait(Delay: Integer = 1000; Message: String = ''; Caption: String = '');
var
  Dialog: TDBIDialogProgress;

begin
  if (Caption = '') then begin
    Caption := Application.Title;
  end;

  Dialog := Local(TDBIDialogProgress.BuildDialog).Obj as TDBIDialogProgress;
  Dialog.Show;
  Dialog.Count := Delay div 10;
  Dialog.Caption := Caption;
  Dialog.Message := Message;
  Dialog.Delay := 10;

  Dialog.First;
  while (Dialog.Index < Dialog.Count) do begin
    Dialog.Next;
  end;
  Dialog.Last;
end;





{ TDBIDialogProceed }

class function TDBIDialogAsk.Execute(Strings: TStrings; const ATitle: String = 'Select'): Boolean;
const
  BorderStyles: array[Boolean] of TFormBorderStyle = (bsDialog, bsSizeable);
  ItemIndices: array[Boolean] of Integer = (-1, 0);

var
  Dialog: TDBIDialogask;

begin
  Result := Assigned(Strings) and (Strings.Count > 0);
  if Result then begin
    Dialog := BuildDialog as Self;
    try
      Dialog.Caption := Application.Title + ' - ' + ATitle;
      Dialog.Listbox.Items.Assign(Strings);
      Dialog.ListBox.ItemIndex := -1;
      Dialog.Height := 100 + Abs(Strings.Count * Dialog.ListBox.Font.Height);

      Result := Dialog.ShowModal = mrOK;
    finally
      Dialog.Free;
    end;
  end;
end;





{ TDBIDialogSelect }

class function TDBIDialogSelect.Execute(Strings: TStrings; const ATitle: String = 'Select'): String;
const
  BorderStyles: array[Boolean] of TFormBorderStyle = (bsDialog, bsSizeable);
  ItemIndices: array[Boolean] of Integer = (-1, 0);

var
  Dialog: TDBIDialogSelect;

begin
  Result := '';

  if Assigned(Strings) and (Strings.Count > 0) then begin
    Dialog := BuildDialog as Self;
    try
      Dialog.Caption := Application.Title + ' - ' + ATitle;
      Dialog.Listbox.Items.Assign(Strings);
      Dialog.ListBox.Enabled := Strings.Count > 1;
      Dialog.ListBox.ItemIndex := ItemIndices[Dialog.ListBox.Enabled];
      Dialog.BorderStyle := BorderStyles[Dialog.ListBox.Enabled];

      if (Dialog.ShowModal = mrOK) then begin
        if (Dialog.ListBox.ItemIndex < 0) then begin
          Result := Strings[0];
        end
        else begin
          Result := Strings[Dialog.ListBox.ItemIndex];
        end;
      end
      else begin
        abort;
      end;

    finally
      Dialog.Free;
    end;
  end;
end;





{ TDBIDialogListbox }

class function TDBIDialogListbox.BuildDialog(AOwner: TComponent = nil): TDBICustomDialog;
begin
  Result := Self.CreateNew(AOwner);
  Result.Color := clWindow;
  Result.Width := 800;
  Result.Height := 180;
  Result.Constraints.MinHeight := Result.Height - 24;
  Result.Constraints.MinWidth := 400;
  Result.Position := poScreenCenter;

  Result.Client.BorderWidth := 10;
  Result.Shape;
  Result.ButtonOK;
  Result.ButtonCancel;
  Result.ListBox;
end;





{ TDBIDialogDataset }

class function TDBIDialogDataset.BuildDialog(AOwner: TComponent = nil): TDBICustomDialog;
begin
  Result := Self.CreateNew(AOwner);
  Result.Position := poScreenCenter;
  Result.Height := 400;
  Result.Width := 600;

  Result.Client.BorderWidth := 5;
  Result.Shape;
  Result.ButtonOK;
  Result.ButtonCancel;
end;


class function TDBIDialogDataset.Execute(ADataset: TDataset; const ATitle: String = ''): Boolean;
var
  Dialog: TDBIDialogDataset;

begin
  Dialog := Local(BuildDialog).Obj as Self;
  Dialog.Caption := Application.Title + ' - ' + ATitle;
  Dialog.DataSource.Dataset := ADataset;

  Result := Dialog.ShowModal = mrOK;
end;


function TDBIDialogDataset.GetDataSource: TDataSource;
begin
  if not Assigned(FDataSource) then begin
    FDataSource := TDataSource.Create(Self);

    DBGrid.DataSource := FDataSource;
  end;
  Result := FDataSource;
end;


function TDBIDialogDataset.GetDBGrid: TDBGrid;
begin
  if not Assigned(FDBGrid) then begin
    FDBGrid := TDBGrid.Create(Self);
    FDBGrid.Parent := Self;
    FDBGrid.Align := alClient;
    FDBGrid.BorderStyle := bsNone;
    FDBGrid.DataSource := DataSource;
    FDBGrid.Options := [dgEditing, dgTitles, dgColumnResize, dgTabs, dgConfirmDelete, dgCancelOnExit];
    FDBGrid.ReadOnly := True;
    FDBGrid.TabStop := False;
  end;
  Result := FDBGrid;
end;





{ TDBICustomDialog }

function TDBICustomDialog.GetButtonCancel: TButton;
begin
  if not Assigned(FButtonCancel) then begin
    FButtonCancel := TDBICancelButton.Create(Self);
    FButtonCancel.Parent := Footer;
  end;
  Result := FButtonCancel;
end;


function TDBICustomDialog.GetButtonOK: TButton;
begin
  if not Assigned(FButtonOK) then begin
    FButtonOK := TDBIOKButton.Create(Self);
    FButtonOK.Parent := Footer;
  end;
  Result := FButtonOK;
end;


function TDBICustomDialog.GetClient: TPanel;
begin
  if not Assigned(FClient) then begin
    FClient := TPanel.Create(Self);
{$ifdef DelphiXE}
    FClient.ParentBackground := False;
{$endif}
    FClient.Align := alClient;
    FClient.BevelOuter := bvNone;
    FClient.Caption := '';
    FClient.Color := clWindow;
    FClient.Parent := Self;
  end;
  Result := FClient;
end;


function TDBICustomDialog.GetComment: TLabel;
begin
  if not Assigned(FComment) then begin
    FComment := TLabel.Create(Self);
    FComment.Parent := Footer;
    FComment.Font.Color := clHotLight;
    FComment.Top := 12;
    FComment.Left := 15;
  end;
  Result := FComment;
end;


function TDBICustomDialog.GetEdit: TEdit;
begin
  if not Assigned(FEdit) then begin
    FEdit := TEdit.Create(Self);
    FEdit.Align := alNone;
    FEdit.Parent := Footer;
    FEdit.Font.Charset := ANSI_CHARSET;
    FEdit.Font.Color := clWindowText;
    FEdit.Font.Height := -13;
    FEdit.Font.Name := 'Lucida Console';
    FEdit.Left := 150;
    FEdit.Top := 10;
  end;
  Result := FEdit;
end;


function TDBICustomDialog.GetListBox: TListBox;
begin
  if not Assigned(FListBox) then begin
    FListBox := TListBox.Create(Self);
    FListBox.Parent := Client;
    FListBox.Align := alClient;
    FListBox.BorderStyle := bsNone;
    FListBox.Font.Charset := ANSI_CHARSET;
    FListBox.Font.Color := clWindowText;
    FListBox.Font.Height := -13;
    FListBox.Font.Name := 'Lucida Console';
  end;
  Result := FListBox;
end;


function TDBICustomDialog.GetFooter: TPanel;
begin
  if not Assigned(FFooter) then begin
    FFooter := TPanel.Create(Self);
{$ifdef DelphiXE}
    FFooter.ParentBackground := False;
{$endif}
    FFooter.Align := alBottom;
    FFooter.BevelOuter := bvNone;
    FFooter.Caption := '';
    FFooter.Color := clBtnFace;
    FFooter.Height := 41;
    FFooter.Parent := Self;
  end;
  Result := FFooter;
end;


function TDBICustomDialog.GetImage: TImage;
begin
  if not Assigned(FImage) then begin
    FImage := TImage.Create(Self);
    FImage.Align := alLeft;
    FImage.Parent := Self;
    FImage.Center := True;
    FImage.Picture.Assign(Application.Icon);
    FImage.Width := FImage.Picture.Width * 2;
  end;
  Result := FImage;
end;


function TDBICustomDialog.GetShape: TShape;
begin
  if not Assigned(FShape) then begin
    FShape := TShape.Create(Self);
    FShape.Align := alTop;
    FShape.Parent := Footer;
    FShape.Pen.Color := cl3DLight;
    FShape.Brush.Color := cl3DLight;
    FShape.Height := 1;
    FShape.Top := 0;
  end;
  Result := FShape;
end;





{ TDBIMemo }

constructor TDBIMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHighLightColor := clBlue;
end;


procedure TDBIMemo.DrawLine(First, Last: Integer);
const
  Padding = 0;

var
  LineHeight: Integer;
  Pt1, Pt2: TSmallPoint;
  Context: HDC;
  Rect: TRect;
  ClipRgn: HRGN;

begin
  // font height approximation (compensate 1px for internal leading)
  LineHeight := Padding + Abs(Font.Height) - Abs(Font.Height) div Font.Height;

  // get logical top-left coordinates for line bound characters
  Integer(Pt1) := SendMessage(Handle, EM_POSFROMCHAR, First, 0);
  Integer(Pt2) := SendMessage(Handle, EM_POSFROMCHAR, Last, 0);

  Context := GetDC(Handle);

  // clip to not to draw to non-text area (internal margins)
  SendMessage(Handle, EM_GETRECT, 0, Integer(@Rect));
  ClipRgn := CreateRectRgn(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  SelectClipRgn(Context, ClipRgn);
  DeleteObject(ClipRgn);

  // set pen color to red and draw line
  SelectObject(Context, GetStockObject(DC_PEN));
  SetDCPenColor(Context, ColorToRGB(FhighLightColor));
  MoveToEx(Context, Pt1.X, Pt1.Y + LineHeight, nil);
  LineTo(Context, Pt2.X, Pt2.Y + LineHeight);

  ReleaseDC(Handle, Context);
end;


function TDBIMemo.GetPosition(const Index: TDBIMemoPosition): Integer;
var
  CharIndex: Integer;
  Column: Integer;
  Row: Integer;

begin
  CharIndex := SelStart;
  Row := Perform(EM_LINEFROMCHAR, CharIndex, 0);
  Column := CharIndex - Perform(EM_LINEINDEX, Row, 0) ;

  case Index of
    mpColumn: Result := Column;
    mpRow: Result := Row;
  else
    Result := CharIndex;
  end;
end;


function TDBIMemo.GetRow(const CharIndex: Integer): Integer;
begin
  Result := SendMessage(Handle, EM_LINEFROMCHAR, CharIndex, 0);
end;


procedure TDBIMemo.HighLight(const StartIndex, EndIndex: Word);
begin
  FStartIndex := StartIndex;
  FEndIndex := EndIndex;
  Invalidate;
end;


procedure TDBIMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (ssCtrl in Shift) then begin
    Cursor := crHandPoint;
  end
  else begin
    Cursor := crDefault;
  end;
end;


function TDBIMemo.SelectWordAtCaret: string;
const
  InValidChars = [' ', ',', ':', ';'];

var
  Line: string;
  InitPos: Integer;
  EndPos: Integer;

begin
  FHighLightedText := '';
  FStartIndex := 0;
  FEndIndex := 0;

  // Validate the line number
  if Lines.Count-1 < Row then begin
    Exit;
  end;

  Line := Lines[Row];

  // Find the initial offset using "InValidChars" as separators
  InitPos := Column+1;
  while (InitPos > 0) and not (AnsiChar(Line[InitPos]) in InValidChars) do begin
    Dec(InitPos);
  end;

  // Find the final offset using "InValidChars" as separators
  Inc(InitPos);
  EndPos := InitPos;
  while (EndPos <= Length(Line)) and not (AnsiChar(Line[EndPos]) in InValidChars) do begin
    Inc(EndPos);
  end;

  // Assign the text
  FHighLightedText := Copy(Line, InitPos, EndPos - InitPos);

  // Highlight the text in the Memo
  Dec(InitPos);
  if ValidateSelection(FHighLightedText) then begin
    FStartIndex := Perform(EM_LINEINDEX, Row, 0) + InitPos;
    FEndIndex := FStartIndex + Length(FHighLightedText);
  end;

  Invalidate;
  Result := FHighLightedText;
end;


function TDBIMemo.ValidateSelection(const Value: String): Boolean;
var
  Extension: String;
  Size: Word;

begin
  Extension := ExtractFileExt(Value);
  Size := Length(Extension);
  Result := (Size > 1) and (Size < 5) and (Extension[1] = '.');
end;


procedure TDBIMemo.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;

  if (ssCtrl in KeysToShiftState(Message.Keys)) and not (csNoStdEvents in ControlStyle) then begin
    SelectWordAtCaret;
  end;
end;


procedure TDBIMemo.WMRButtonDown(var Message: TWMRButtonDown);
begin
  inherited;

  if (ssCtrl in KeysToShiftState(Message.Keys)) and not (csNoStdEvents in ControlStyle) then begin
    Mouse_Event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    Mouse_Event(MOUSEEVENTF_LEFTUP, 0, 0, 0, 0);
  end;
end;


procedure TDBIMemo.WMPaint(var Msg: TWMPaint);
var
  StartIndex, CharIndex, Row: Integer;

begin
  inherited;

  if (FEndIndex > FStartIndex) then begin
    // May possibly improve this by using EM_LINELENGTH
    StartIndex := FStartIndex;
    CharIndex := StartIndex;
    Row := GetRow(CharIndex);
    while True do begin
      Inc(CharIndex);
      if GetRow(CharIndex) > Row then begin
        DrawLine(StartIndex, CharIndex - 1);
        StartIndex := CharIndex;
        Dec(CharIndex);
        Inc(Row);
      end
      else if (CharIndex >= FEndIndex) then begin
        DrawLine(StartIndex, FEndIndex);
        Break;
      end;
    end;
  end;
end;





{ TDBICancelButton }

constructor TDBICancelButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Anchors := [akTop, akRight];
  Cancel := True;
  Caption := 'Cancel';
  Height := 23;
  Left := 100;
  Top := 9;
  Width := 75;
  ModalResult := mrCancel;
end;





{ TDBIOKButton }

constructor TDBIOKButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Anchors := [akTop, akRight];
  Caption := 'OK';
  Default := True;
  Height := 23;
  Left := 15;
  Top := 9;
  Width := 75;
  ModalResult := mrOk;
end;



initialization
  Classes.RegisterClasses([TButton, TEdit, TLabel, TListBox, TPanel, TProgressBar, TImage, TShape]);

end.
