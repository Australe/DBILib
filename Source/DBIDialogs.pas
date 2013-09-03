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
  1.0 | 08/07/2013 07:53:42 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBIDialogs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, DB, DBGrids;

type
  TDBIMemo = class(TMemo)
  private
    FStartIndex: Integer;
    FEndIndex: Integer;
    FHighLightColor: TColor;
    FHighLightedText: String;
    
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;

  protected
    procedure DrawLine(First, Last: Integer);
    function GetRow(const CharIndex: Integer): Integer;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function SelectWordAtCaret: string;
    function ValidateSelection(const Value: String): Boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;

    procedure HighLight(const StartIndex, EndIndex: Word);

    property HighLightColor: TColor read FHighLightColor write FHighLightColor;
    property HighLightedText: String read FHighLightedText;

  end;


type
  TDBICustomDialog = class(TForm)
  private
    FButtonOK: TButton;
    FButtonCancel: TButton;
    FPanel: TPanel;

  protected
    function GetButtonCancel: TButton;
    function GetButtonOK: TButton;
    function GetPanel: TPanel;

    property ButtonOK: TButton read GetButtonOK;
    property ButtonCancel: TButton read GetButtonCancel;
    property Panel: TPanel read GetPanel;

  public
    class function Build(AOwner: TComponent): TDBICustomDialog; virtual;

    function Execute: Boolean; virtual;
  end;


type
  TDBIDialogDataset = class(TDBICustomDialog)
  private
    FDBGrid: TDBGrid;
    FDataSource: TDataSource;

  protected
    function GetDBGrid: TDBGrid;
    function GetDataSource: TDataSource;

  public
    class function Dialog(AOwner: TComponent = nil): TDBICustomDialog;

    property ButtonOK;
    property ButtonCancel;
    property DBGrid: TDBGrid read GetDBGrid;
    property DataSource: TDataSource read GetDataSource;
    property Panel;
 end;


type
  TDBIDialogListbox = class(TForm)
  private
    FListBox: TListBox;

  protected
    class function BuildDialog(const ATitle: String): TDBIDialogListbox; virtual;

    property ListBox: TListBox read FListBox;

  end;


type
  TDBIDialogSelect = class(TDBIDialogListBox)
  public
    class function Execute(Strings: TStrings; const ATitle: String = 'Select'): String;
  end;


type
  TDBIDialogProgress = class(TCustomForm)
  private
    FProgress: TProgressBar;
    FText: String;

  protected
    function GetCount: Integer;
    function GetProgressBar: TProgressBar;
    function GetText: String;

    procedure SetCount(const Value: Integer);
    procedure SetText(const Value: String);

  public
    class function Dialog(AOwner: TComponent = nil): TDBIDialogProgress;

    procedure Next;

    property Count: Integer read GetCount write SetCount;
    property Progress: TProgressBar read GetProgressBar;
    property Caption: String read GetText write SetText;

  end;


implementation

{. $R *.DFM}


{ TDBIDialogProgress }

class function TDBIDialogProgress.Dialog(AOwner: TComponent = nil): TDBIDialogProgress;
begin
  Result := Self.CreateNew(AOwner);
  Result.BorderStyle := bsNone;
  Result.BorderIcons := [];
  Result.BorderWidth := 2;
  Result.Color := clDkGray;
  Result.FormStyle := fsStayOnTop;
  Result.Position := poScreenCenter;
  Result.Progress.Style := pbstMarquee;
  Result.Height := 45;
  Result.Width := 400;
end;

function TDBIDialogProgress.GetCount: Integer;
begin
  Result := Progress.Max+1;
end;

function TDBIDialogProgress.GetProgressBar: TProgressBar;
var
  Panel: TPanel;

begin
  if not Assigned(FProgress) then begin
    Panel := TPanel.Create(Self);
    Panel.Align := alClient;
    Panel.BorderWidth := 10;
    Panel.Color := clWindow;
    Panel.Parent := Self;

    FProgress := TProgressBar.Create(Self);
    FProgress.Align := alClient;
    FProgress.Parent := Panel;
  end;
  Result := FProgress;
end;

function TDBIDialogProgress.GetText: String;
begin
  Result := FText;
end;

procedure TDBIDialogProgress.Next;
begin
  Progress.Position := Progress.Position + 1;

  if (BorderStyle <> bsNone) then begin
    inherited Caption := Format('%s  #%d of %d', [FText, Progress.Position, Progress.Max]);
  end;
end;

procedure TDBIDialogProgress.SetCount(const Value: Integer);
begin
  Progress.Position := 0;
  Progress.Max := Value-1;
end;

procedure TDBIDialogProgress.SetText(const Value: String);
begin
  FText := Value;

  inherited Caption := FText;

  if (BorderStyle = bsNone) then begin
    BorderStyle := {bsToolWindow; //}bsDialog;
    Height := Height + 28;
  end;

  Application.ProcessMessages;
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
    Dialog := BuildDialog(ATitle) as Self;
    try
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
      end;

    finally
      Dialog.Free;
    end;
  end;
end;





{ TDBIDialogListbox }

class function TDBIDialogListbox.BuildDialog(const ATitle: String): TDBIDialogListbox;
  function BuildButton(AParent: TWinControl): TButton;
  begin
    Result := TButton.Create(AParent);
    Result.Parent := AParent;
    Result.Top := AParent.Constraints.MinHeight;
    Result.Width := 75;
    Result.Height := 25;
    Result.Anchors := [akRight, akBottom];
  end;

var
  Bevel: TBevel;
  ButtonOK: TButton;
  ButtonCancel: TButton;

begin
  Result := Self.CreateNew(nil);
  Result.Width := 800;
  Result.Height := 180;
  Result.BorderWidth := 10;
  Result.Caption := ATitle;
  Result.Constraints.MinHeight := Result.Height - 24;
  Result.Constraints.MinWidth := 400;
  Result.Position := poScreenCenter;

  Bevel := TBevel.Create(Result);
  Bevel.Parent := Result;
  Bevel.Height := 35;
  Bevel.Align := alBottom;
  Bevel.Shape := bsSpacer;

  Result.FListBox := TListBox.Create(Result);
  Result.FListBox.Parent := Result;
  Result.FListBox.Align := alClient;
  Result.FListBox.BorderStyle := bsNone;
  Result.FListBox.Font.Charset := ANSI_CHARSET;
  Result.FListBox.Font.Color := clWindowText;
  Result.FListBox.Font.Height := -13;
  Result.FListBox.Font.Name := 'Lucida Console';

  ButtonOK := BuildButton(Result);
  ButtonOK.Left := 535;
  ButtonOK.Caption := 'OK';
  ButtonOK.Default := True;
  ButtonOK.ModalResult := mrOK;

  ButtonCancel := BuildButton(Result);
  ButtonCancel.Left := 620;
  ButtonCancel.Cancel := True;
  ButtonCancel.Caption := 'Cancel';
  ButtonCancel.ModalResult := mrCancel;
end;





{ TDBIDialogDataset }

class function TDBIDialogDataset.Dialog(AOwner: TComponent = nil): TDBICustomDialog;
begin
  Result := Self.Build(AOwner) as Self;
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





{ TDBIDialogDataset }

class function TDBICustomDialog.Build(AOwner: TComponent): TDBICustomDialog;
begin
  Result := Self.CreateNew(AOwner);
  Result.BorderWidth := 5;
  Result.Position := poScreenCenter;
  Result.Height := 250;
  Result.Width := 250;
  Result.ButtonOK;
  Result.ButtonCancel;
end;


function TDBICustomDialog.Execute: Boolean;
begin
  Result := ShowModal = mrOK;

  Application.ProcessMessages;
end;


function TDBICustomDialog.GetButtonCancel: TButton;
begin
  if not Assigned(FButtonCancel) then begin
    FButtonCancel := TButton.Create(Self);
    FButtonCancel.Anchors := [akTop, akRight];
    FButtonCancel.Cancel := True;
    FButtonCancel.Caption := 'Cancel';
    FButtonCancel.ModalResult := mrCancel;
    FButtonCancel.Left := 105;
    FButtonCancel.Parent := Panel;
    FButtonCancel.Top := 5;
  end;
  Result := FButtonCancel;
end;


function TDBICustomDialog.GetButtonOK: TButton;
begin
  if not Assigned(FButtonOK) then begin
    FButtonOK := TButton.Create(Self);
    FButtonOK.Anchors := [akTop, akRight];
    FButtonOK.Caption := 'OK';
    FButtonOK.Default := True;
    FButtonOK.ModalResult := mrOK;
    FButtonOK.Left := 20;
    FButtonOK.Parent := Panel;
    FButtonOK.Top := 5;
  end;
  Result := FButtonOK;
end;


function TDBICustomDialog.GetPanel: TPanel;
begin
  if not Assigned(FPanel) then begin
    FPanel := TPanel.Create(Self);
    FPanel.Align := alBottom;
    FPanel.BevelOuter := bvNone;
    FPanel.Caption := '';
    FPanel.Height := 31;
    FPanel.Parent := Self;
  end;
  Result := FPanel;
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
  CharIndex: Integer;
  Row: Integer;
  Column: Integer;
  Line: string;
  InitPos: Integer;
  EndPos: Integer;

begin
  FHighLightedText := '';
  FStartIndex := 0;
  FEndIndex := 0;

  // Get the caret position
  CharIndex := SelStart;
  Row := Perform(EM_LINEFROMCHAR, CharIndex, 0) ;
  Column := CharIndex - Perform(EM_LINEINDEX, Row, 0) ;

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
  EndPos := InitPos+1;
  while (EndPos <= Length(Line)) and not (AnsiChar(Line[EndPos]) in InValidChars) do begin
    Inc(EndPos);
  end;

  // Assign the text
  FHighLightedText := Trim(Copy(Line, InitPos, EndPos - InitPos));

  // Highlight the text in the Memo
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



end.
