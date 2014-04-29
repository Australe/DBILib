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
  1.0 | 22/04/2014 08:23:00 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBICssUtils;

interface

uses
  Graphics, Classes, SysUtils;

type
  TDBICssDisplay = (
    cssDisplayInitial,
    cssDisplayInherit,
    cssDisplayNone
    );

  TDBICssFontStyle = (
    cssFontStyleInitial,
    cssFontStyleInherit,
    cssFontStyleNormal,
    cssFontStyleItalic,
    cssFontStyleOblique
    );

  TDBICssFontWeight = (
    cssFontWeightInitial,
    csFontWeightInherit,
    cssFontWeightNormal,
    cssFontWeightBold
    );


type
  TDBICssColors = class(TStringList)
  protected
    function GetColor(const Index: String): TColor;
    procedure GetColors;

    class procedure ReleaseInstance;
  public
    class function Instance: TDBICssColors;

    property Color[const Index: String]: TColor read GetColor; default;
  end;


type
  TDBICustomControlStyle = class(TPersistent)
  private
    FBackgroundColor: TColor;
    FColor: TColor;
    FContent: String;
    FFontSize: Word;
    FFontStyle: TDBICssFontStyle;
    FFontWeight: TDBICssFontWeight;
    FDisplay: TDBICssDisplay;
    FName: String;
    FTextShadowColor: TColor;
    FTextStatic: String;

  protected
    function GetFontStyles: TFontStyles;

    class procedure SetColorProperty(
      Instance: TObject;
      const PropName: String;
      const Value: String
      );

    class procedure SetEnumProperty(
      Instance: TObject;
      const PropName: String;
      const Value: String
      );

    procedure SetFontSizeName(Value: String);
    procedure SetFontWeightName(const Value: String);
    procedure SetContent(const Value: String);
    procedure SetTextStatic(const Value: String);

    procedure SetElement(const Index: String; Value: String);

  protected
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor default clWindow;
    property Color: TColor read FColor write FColor default clWindowText;
    property Content: String read FContent write SetContent;
    property FontSize: Word read FFontSize write FFontSize;
    property FontStyle: TDBICssFontStyle read FFontStyle write FFontStyle;
    property FontStyles: TFontStyles read GetFontStyles;
    property FontWeight: TDBICssFontWeight read FFontWeight write FFontWeight;
    property Display: TDBICssDisplay read FDisplay write FDisplay;
    property Name: String read FName write FName;
    property TextShadowColor: TColor read FTextShadowColor write FTextShadowColor default clWindow;
    property TextStatic: String read FTextStatic write SetTextStatic;

    property Element[const Index: String]: String write SetElement;

  public
    procedure Assign(Source: TPersistent); override;

  end;


  TDBIControlStyle = class(TDBICustomControlStyle)
  public
    property Element;
    property FontStyles;

  published
    property BackgroundColor;
    property Color;
    property Content;
    property FontSize;
    property FontStyle;
    property FontWeight;
    property Display;
    property Name;
    property TextShadowColor;
    property TextStatic;

  end;


  TDBICustomControlStyleList = class(TStringList)
  protected
    function FindStyle(const Index: String): TDBIControlStyle;
    function GetStyle(const Index: String): TDBIControlStyle;

  public
    destructor Destroy; override;

    procedure LoadFromFile(AFileName: TFileName = '');

    property Style[const Index: String]: TDBIControlStyle read GetStyle; default;
  end;
  TDBIControlStylelist = class(TDBICustomControlStyleList);


implementation

uses
  TypInfo, UIConsts, DBIUtils, DBICssTokenizers;


{ TDBICssCustomStyleList }

destructor TDBICustomControlStyleList.Destroy;
begin
  //##DEBUG

  inherited Destroy;
end;


function TDBICustomControlStyleList.FindStyle(const Index: String): TDBIControlStyle;
begin
  Result := GetStyle(Index);
  if (Result = nil) then begin
    Result := TDBIControlStyle.Create;
    Result.Name := Index;
    Result.BackgroundColor := clWindow;
    Result.Color := clBlack;

    AddObject(Index, Result);
  end;
end;


function TDBICustomControlStyleList.GetStyle(const Index: String): TDBIControlStyle;
var
  StyleIndex: Integer;

begin
  Result := nil;

  StyleIndex := IndexOf(Index);
  if (StyleIndex > -1) then begin
    Result := Objects[StyleIndex] as TDBIControlStyle;
  end;
end;


procedure TDBICustomControlStyleList.LoadFromFile(AFileName: TFileName = '');
var
  Processor: TDBICssAnalyser;
  Style: TDBICssGenericStyle;
  StyleIndex: Integer;
  ChildStyle: TDBICssGenericStyle;
  ChildIndex: Integer;
  ControlStyle: TDBIControlStyle;

begin
  if (AFileName = '') then begin
    AFileName := ChangeFileExt(ParamStr(0), '.css');
  end;

  Processor := Local(TDBICssAnalyser.Create).Obj as TDBICssAnalyser;
  Processor.Input.LoadFromFile(AFileName);
  Processor.Process;

  for StyleIndex := 0 to Processor.StyleSheet.ChildCount-1 do begin
    Style := Processor.StyleSheet.Children[StyleIndex] as TDBICssGenericStyle;
    for ChildIndex := 0 to Style.ChildCount-1 do begin
      if (Style.Children[ChildIndex] is TDBICssGenericStyle) then begin
        ChildStyle := Style.Children[ChildIndex] as TDBICssGenericStyle;
        ControlStyle := FindStyle(Copy(Style.Text, 2, 255) + ChildStyle.Text);
        ControlStyle.Assign(ChildStyle);
      end
      else begin
        ControlStyle := FindStyle(Copy(Style.Text, 2, 255));
        ControlStyle.Assign(Style);
      end;
    end;
  end;
end;





{ TDBICustomControlStyle }

procedure TDBICustomControlStyle.Assign(Source: TPersistent);

  procedure AssignFromCss(CssStyle: TDBICssGenericStyle);
  var
    Index: Integer;
    CssProperty: TDBICssStyleItem;

  begin
    for Index := 0 to CssStyle.ChildCount-1 do begin
      if (CssStyle.Children[Index] is TDBICssStyleItem) then begin
        CssProperty := CssStyle.Children[Index] as TDBICssStyleItem;
        Element[CssProperty.Text] := CssProperty.Data;
      end
      else if (CssStyle.Children[Index] is TDBICssPseudoElementStyle) then begin
        // Skip - dealt with by caller

{##JVR
      end
      else begin
        MessageDlg(Format('Invalid Class "%s"', [CssStyle.Children[Index].Classname]), mtError, [mbOK], 0);
//}
      end;
    end;
  end;


begin
  if (Source is TDBICssGenericStyle) then begin
    AssignFromCss(TDBICssGenericStyle(Source));
  end
  else begin
    inherited Assign(Source);
  end;
end;


function TDBICustomControlStyle.GetFontStyles: TFontStyles;
const
  BoldStyle: array[Boolean] of TFontStyles = ([], [fsBold]);
  ItalicStyle: array[Boolean] of TFontStyles = ([], [fsItalic]);

begin
  Result := BoldStyle[FontWeight = cssFontWeightBold] + ItalicStyle[FontStyle = cssFontStyleItalic];
end;


class procedure TDBICustomControlStyle.SetColorProperty(
  Instance: TObject;
  const PropName: String;
  const Value: String
  );
var
  PropInfo: PPropInfo;

begin
  PropInfo := TypInfo.GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
  if Assigned(PropInfo) then begin
    TypInfo.SetOrdProp(Instance, PropInfo, TDBICssColors.Instance[Value]);
  end;
end;


procedure TDBICustomControlStyle.SetContent(const Value: String);
begin
  FContent := StringReplace(Value, '@', '$', [rfReplaceAll]);
end;


class procedure TDBICustomControlStyle.SetEnumProperty(
  Instance: TObject;
  const PropName: String;
  const Value: String
  );
var
  PropInfo: PPropInfo;

begin
  PropInfo := TypInfo.GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);
  if Assigned(PropInfo) then begin
    TypInfo.SetEnumProp(Instance, PropInfo, Value);
  end;
end;


procedure TDBICustomControlStyle.SetElement(const Index: String; Value: String);
begin
  if CompareText(Index, 'background-color') = 0 then begin
    SetColorProperty(Self, 'backgroundcolor', Value);
  end

  else if CompareText(Index, 'text-shadow-color') = 0 then begin
    SetColorProperty(Self, 'textshadowcolor', Value);
  end

  else if CompareText(Index, 'color') = 0 then begin
    SetColorProperty(Self, 'color', Value);
  end

  else if CompareText(Index, 'display') = 0 then begin
    SetEnumProperty(Self, 'display', 'cssDisplay' + Value);
  end

  else if CompareText(Index, 'font-size') = 0 then begin
    SetFontSizeName(Value);
  end

  else if CompareText(Index, 'font-weight') = 0 then begin
    SetEnumProperty(Self, 'fontweight', 'cssFontWeight' + Value);
  end

  else if CompareText(Index, 'content') = 0 then begin
    SetContent(Value);
  end

  else if CompareText(Index, 'text-static') = 0 then begin
    SetTextStatic(Value);
  end;
end;


procedure TDBICustomControlStyle.SetFontSizeName(Value: String);
begin
  if Pos('px', Value) > 0 then begin
    Value := Copy(Value, 1, Length(Value)-2)
  end;
  FFontSize := StrToIntDef(Value, 0);
end;


procedure TDBICustomControlStyle.SetFontWeightName(const Value: String);
begin
  FFontWeight := TDBICssFontWeight(
    TypInfo.GetEnumValue(TypeInfo(TDBICssFontWeight), 'cssFontWeight' + Value)
    );
end;


procedure TDBICustomControlStyle.SetTextStatic(const Value: String);
begin
  FTextStatic := StringReplace(Value, '@', '$', [rfReplaceAll]);
end;





{ TDBICssColors }

function TDBICssColors.GetColor(const Index: String): TColor;
var
  ItemIndex: Integer;

begin
  ItemIndex := IndexOf(Index);
  if (ItemIndex >= 0) then begin
    Result := TColor(Objects[ItemIndex]);
  end
  else begin
    Result := StrToInt(StringReplace(Index, '#', '$', []));
  end;
end;


procedure TDBICssColors.GetColors;
begin
  if (Count <= 0) then begin
    Duplicates := dupIgnore;
    Sorted := True;

    // Actual colors
    AddObject('Aliceblue', TObject($FFF8F0));
    AddObject('Antiquewhite', TObject($D7EBFA));
    AddObject('Aqua', TObject($FFFF00));
    AddObject('Aquamarine', TObject($D4FF7F));
    AddObject('Azure', TObject($FFFFF0));
    AddObject('Beige', TObject($DCF5F5));
    AddObject('Bisque', TObject($C4E4FF));
    AddObject('Black', TObject($000000));
    AddObject('Blanchedalmond', TObject($CDEBFF));
    AddObject('Blue', TObject($FF0000));
    AddObject('Blueviolet', TObject($E22B8A));
    AddObject('Brown', TObject($2A2AA5));
    AddObject('Burlywood', TObject($87B8DE));
    AddObject('Cadetblue', TObject($A09E5F));
    AddObject('Chartreuse', TObject($00FF7F));
    AddObject('Chocolate', TObject($1E69D2));
    AddObject('Coral', TObject($507FFF));
    AddObject('Cornflowerblue', TObject($ED9564));
    AddObject('Cornsilk', TObject($DCF8FF));
    AddObject('Crimson', TObject($3C14DC));
    AddObject('Cyan', TObject($FFFF00));
    AddObject('Darkblue', TObject($8B0000));
    AddObject('Darkcyan', TObject($8B8B00));
    AddObject('Darkgoldenrod', TObject($0B86B8));
    AddObject('Darkgray', TObject($A9A9A9));
    AddObject('Darkgreen', TObject($006400));
    AddObject('Darkgrey', TObject($A9A9A9));
    AddObject('Darkkhaki', TObject($6BB7BD));
    AddObject('Darkmagenta', TObject($8B008B));
    AddObject('Darkolivegreen', TObject($2F6B55));
    AddObject('Darkorange', TObject($008CFF));
    AddObject('Darkorchid', TObject($CC3299));
    AddObject('Darkred', TObject($00008B));
    AddObject('Darksalmon', TObject($7A96E9));
    AddObject('Darkseagreen', TObject($8FBC8F));
    AddObject('Darkslateblue', TObject($8B3D48));
    AddObject('Darkslategray', TObject($4F4F2F));
    AddObject('Darkslategrey', TObject($4F4F2F));
    AddObject('Darkturquoise', TObject($D1CE00));
    AddObject('Darkviolet', TObject($D30094));
    AddObject('Deeppink', TObject($9314FF));
    AddObject('Deepskyblue', TObject($FFBF00));
    AddObject('Dimgray', TObject($696969));
    AddObject('Dimgrey', TObject($696969));
    AddObject('Dodgerblue', TObject($FF901E));
    AddObject('Firebrick', TObject($2222B2));
    AddObject('Floralwhite', TObject($F0FAFF));
    AddObject('Forestgreen', TObject($228B22));
    AddObject('Fuchsia', TObject($FF00FF));
    AddObject('Gainsboro', TObject($DCDCDC));
    AddObject('Ghostwhite', TObject($FFF8F8));
    AddObject('Gold', TObject($00D7FF));
    AddObject('Goldenrod', TObject($20A5DA));
    AddObject('Gray', TObject($808080));
    AddObject('Green', TObject($008000));
    AddObject('Greenyellow', TObject($2FFFAD));
    AddObject('Grey', TObject($808080));
    AddObject('Honeydew', TObject($F0FFF0));
    AddObject('Hotpink', TObject($B469FF));
    AddObject('Indianred', TObject($5C5CCD));
    AddObject('Indigo', TObject($82004B));
    AddObject('Ivory', TObject($F0FFFF));
    AddObject('Khaki', TObject($8CE6F0));
    AddObject('Lavender', TObject($FAE6E6));
    AddObject('Lavenderblush', TObject($F5F0FF));
    AddObject('Lawngreen', TObject($00FC7C));
    AddObject('Lemonchiffon', TObject($CDFAFF));
    AddObject('Lightblue', TObject($E6D8AD));
    AddObject('Lightcoral', TObject($8080F0));
    AddObject('Lightcyan', TObject($FFFFE0));
    AddObject('Lightgoldenrodyellow', TObject($D2FAFA));
    AddObject('Lightgray', TObject($D3D3D3));
    AddObject('Lightgreen', TObject($90EE90));
    AddObject('Lightgrey', TObject($D3D3D3));
    AddObject('Lightpink', TObject($C1B6FF));
    AddObject('Lightsalmon', TObject($7AA0FF));
    AddObject('Lightseagreen', TObject($AAB220));
    AddObject('Lightskyblue', TObject($FACE87));
    AddObject('Lightslategray', TObject($998877));
    AddObject('Lightslategrey', TObject($998877));
    AddObject('Lightsteelblue', TObject($DEC4B0));
    AddObject('Lightyellow', TObject($E0FFFF));
    AddObject('LtGray', TObject($C0C0C0));
    AddObject('MedGray', TObject($A4A0A0));
    AddObject('DkGray', TObject($808080));
    AddObject('MoneyGreen', TObject($C0DCC0));
    AddObject('LegacySkyBlue', TObject($F0CAA6));
    AddObject('Cream', TObject($F0FBFF));
    AddObject('Lime', TObject($00FF00));
    AddObject('Limegreen', TObject($32CD32));
    AddObject('Linen', TObject($E6F0FA));
    AddObject('Magenta', TObject($FF00FF));
    AddObject('Maroon', TObject($000080));
    AddObject('Mediumaquamarine', TObject($AACD66));
    AddObject('Mediumblue', TObject($CD0000));
    AddObject('Mediumorchid', TObject($D355BA));
    AddObject('Mediumpurple', TObject($DB7093));
    AddObject('Mediumseagreen', TObject($71B33C));
    AddObject('Mediumslateblue', TObject($EE687B));
    AddObject('Mediumspringgreen', TObject($9AFA00));
    AddObject('Mediumturquoise', TObject($CCD148));
    AddObject('Mediumvioletred', TObject($8515C7));
    AddObject('Midnightblue', TObject($701919));
    AddObject('Mintcream', TObject($FAFFF5));
    AddObject('Mistyrose', TObject($E1E4FF));
    AddObject('Moccasin', TObject($B5E4FF));
    AddObject('Navajowhite', TObject($ADDEFF));
    AddObject('Navy', TObject($800000));
    AddObject('Oldlace', TObject($E6F5FD));
    AddObject('Olive', TObject($008080));
    AddObject('Olivedrab', TObject($238E6B));
    AddObject('Orange', TObject($00A5FF));
    AddObject('Orangered', TObject($0045FF));
    AddObject('Orchid', TObject($D670DA));
    AddObject('Palegoldenrod', TObject($AAE8EE));
    AddObject('Palegreen', TObject($98FB98));
    AddObject('Paleturquoise', TObject($EEEEAF));
    AddObject('Palevioletred', TObject($9370DB));
    AddObject('Papayawhip', TObject($D5EFFF));
    AddObject('Peachpuff', TObject($B9DAFF));
    AddObject('Peru', TObject($3F85CD));
    AddObject('Pink', TObject($CBC0FF));
    AddObject('Plum', TObject($DDA0DD));
    AddObject('Powderblue', TObject($E6E0B0));
    AddObject('Purple', TObject($800080));
    AddObject('Red', TObject($0000FF));
    AddObject('Rosybrown', TObject($8F8FBC));
    AddObject('Royalblue', TObject($E16941));
    AddObject('Saddlebrown', TObject($13458B));
    AddObject('Salmon', TObject($7280FA));
    AddObject('Sandybrown', TObject($60A4F4));
    AddObject('Seagreen', TObject($578B2E));
    AddObject('Seashell', TObject($EEF5FF));
    AddObject('Sienna', TObject($2D52A0));
    AddObject('Silver', TObject($C0C0C0));
    AddObject('Skyblue', TObject($EBCE87));
    AddObject('Slateblue', TObject($CD5A6A));
    AddObject('Slategray', TObject($908070));
    AddObject('Slategrey', TObject($908070));
    AddObject('Snow', TObject($FAFAFF));
    AddObject('Springgreen', TObject($7FFF00));
    AddObject('Steelblue', TObject($B48246));
    AddObject('Tan', TObject($8CB4D2));
    AddObject('Teal', TObject($808000));
    AddObject('Thistle', TObject($D8BFD8));
    AddObject('Tomato', TObject($4763FF));
    AddObject('Turquoise', TObject($D0E040));
    AddObject('Violet', TObject($EE82EE));
    AddObject('Wheat', TObject($B3DEF5));
    AddObject('White', TObject($FFFFFF));
    AddObject('Whitesmoke', TObject($F5F5F5));
    AddObject('Yellow', TObject($00FFFF));
    AddObject('Yellowgreen', TObject($32CD9A));
  end;
end;


var
  _CssColors: TDBICssColors = nil;

class function TDBICssColors.Instance: TDBICssColors;
begin
  if not Assigned(_CssColors) then begin
    _CssColors := Self.Create;
    _CssColors.GetColors;
  end;
  Result := _CssColors;
end;


class procedure TDBICssColors.ReleaseInstance;
begin
  FreeAndNil(_CssColors);
end;


initialization
finalization
  TDBICssColors.ReleaseInstance;

end.
