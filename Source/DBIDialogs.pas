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
  StdCtrls, ExtCtrls;

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


implementation

{$R *.DFM}


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
  Result.Width := 700;
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
