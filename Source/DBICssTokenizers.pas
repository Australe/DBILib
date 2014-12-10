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
  1.0 | 02/04/2014 09:15:00 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBICssTokenizers;

{$I DBICompilers.inc}

interface

uses
  Classes, SysUtils, DBIComponents, DBIDialogs, DBIStreamAdapters, DBITokenizers,
  DBITokenizerConsts;

type
  TDBICssNode = class(TDBIPersistentComponent)
  private
    FData: String;
    FKindName: String;
    FPosition: TDBITokenPosition;
    FDummy: String;

  protected
    function ChildOwner: TComponent; virtual;
    function GetClassKindName: String; virtual;
    function GetNode(const ACompoundName: String): TDBICssNode;
    function GetNodeName: String; virtual;

    procedure SetClassKindName(const Value: String);

  public
    property Position: TDBITokenPosition read FPosition write FPosition;

  published
    property Data: String read FData write FData;
    property DisplayName;
    property KindName: String read FKindName write FKindName;
    property Text;
    property ClassKindName: String read GetClassKindName write SetClassKindName;
    property NodeName: String read GetNodeName write FDummy;
    property Row: Integer read FPosition.Row write FPosition.Row;
    property Column: Integer read FPosition.Column write FPosition.Column;

  end;


  TDBICssGenericStyle = class(TDBICssNode)
  private
    FOperator: String;

  published
    property Operator: String read FOperator write FOperator;

  end;

  TDBICssClassStyle = class(TDBICssGenericStyle);
  TDBICssPseudoClassStyle = class(TDBICssGenericStyle);
  TDBICssPseudoElementStyle = class(TDBICssGenericStyle);
  TDBICssElementStyle = class(TDBICssGenericStyle);
  TDBICssIDStyle = class(TDBICssGenericStyle);

  TDBICssStyleOperator = class(TDBICssGenericStyle);

  TDBICssStyleSheet = class(TDBICssNode);
  TDBICssStyleItem = class(TDBICssNode);


type
  TDBICustomCssLexer = class(TDBICustomXMLLexer)
  protected
    procedure LexIdentifier; override;
    procedure LexInitialise; override;

    function IsXML: Boolean;

    function Upto(TokenTypes: TDBITokenTypes): String; overload;

  public
    procedure NextToken; override;

  end;
  TDBICssLexer = class(TDBICustomCssLexer);


type
  TDBICssAnalyser = class(TDBICustomParser)
  private
    FStyleSheet: TDBICssStyleSheet;

  protected
    function GetSelector(
      ANode: TDBICssNode;
      const SelectorType: TDBITokenType;
      const SelectorName: String
      ): TDBICssGenericStyle;

    function GetInput: TDBICssLexer;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;
    function GetStyleSheet: TDBICssStyleSheet;

    function ProcessHtml: Boolean;
    function ProcessKeywords: Boolean;
    function ProcessStyle(ANode: TDBICssNode): Boolean;
    function ProcessStyleElements(Selectors: TStrings): Boolean;

  public
    destructor Destroy; override;

    procedure Process; virtual;

    property Input: TDBICssLexer read GetInput;
    property StyleSheet: TDBICssStyleSheet read GetStyleSheet;
  end;


implementation

uses
  Windows, TypInfo, DBIUtils;

const
  Tokens_Comment3 = [Tok_Slash_Star, Tok_Star_Slash];
  Tokens_Macro = [Tok_OpenCurlyBracket_Dollar, Tok_CloseCurlyBracket];


{ TDBICssAnalyser }

destructor TDBICssAnalyser.Destroy;
begin
  FStyleSheet.Free;
  FStyleSheet := nil;

  inherited Destroy;
end;


function TDBICssAnalyser.GetInput: TDBICssLexer;
begin
  Result := inherited Input as TDBICssLexer;
end;


function TDBICssAnalyser.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBICssLexer;
end;


function TDBICssAnalyser.GetSelector(
  ANode: TDBICssNode;
  const SelectorType: TDBITokenType;
  const SelectorName: String
  ): TDBICssGenericStyle;
var
  CompoundName: String;

begin
  CompoundName := ANode.GetNodeName + '.' + SelectorName;

  Result := (ANode.RootComponent as TDBICssNode).GetNode(CompoundName) as TDBICssGenericStyle;
  if not Assigned(Result) then begin
    case SelectorType of
      Tok_ColonColon: Result := TDBICssPseudoElementStyle.Create(ANode.ChildOwner, ANode);
      Tok_Colon: Result := TDBICssPseudoClassStyle.Create(ANode.ChildOwner, ANode);
      Tok_Dot: Result := TDBICssClassStyle.Create(ANode.ChildOwner, ANode);
      Tok_Hash: Result := TDBICssIDStyle.Create(ANode.ChildOwner, ANode);

      Tok_Greater, Tok_Plus, Tok_Tilde: Result := TDBICssStyleOperator.Create(ANode.ChildOwner, ANode);
    else
      Result := TDBICssElementStyle.Create(ANode.ChildOwner, ANode);
    end;

    Result.Text := SelectorName;
  end;
end;


function TDBICssAnalyser.GetStyleSheet: TDBICssStyleSheet;
begin
  if not Assigned(FStyleSheet) then begin
    FStyleSheet := TDBICssStyleSheet.Create(nil, nil);
  end;
  Result := FStyleSheet;
end;


procedure TDBICssAnalyser.Process;
begin
  // BootStrap the lexer;
  Input.Reset;

  while ProcessKeywords and not Input.Eof do;
end;


function TDBICssAnalyser.ProcessHtml: Boolean;
begin
  Result := True;

  Input.Fetch([Tok_Smaller]);
  if (Input.Token.TokenType = Tok_Identifier) and (CompareText(Input.Token.TokenString, 'html') = 0)then begin
    while (Input.Token.TokenType <> Tok_Eof) do begin
      if Input.Have([Tok_Smaller]) then begin
        if (Input.Token.TokenType = Tok_Identifier) and (CompareText(Input.Token.TokenString, 'style') = 0) then begin
          Input.Upto([Tok_Greater]);

          Break;
        end;
      end
      else begin
        Input.GetToken;
      end;
    end;
  end;
end;


function TDBICssAnalyser.ProcessKeywords: Boolean;
begin
  Result := True;

  if Input.IsXML then begin
    case Input.Token.TokenType of
      Tok_Smaller: Result := ProcessHtml;
    else
      Input.GetToken;
    end;
  end

  else begin
    case Input.Token.TokenType of
      Tok_Minus: Input.Check([Tok_Hash, Tok_Dot, Tok_Identifier]);
      Tok_Hash, Tok_Dot, Tok_Identifier: Result := ProcessStyle(StyleSheet);
    else
      Input.NextToken;
    end;
  end;
end;


function TDBICssAnalyser.ProcessStyle(ANode: TDBICssNode): Boolean;
var
  Scope: TDBICssNode;
  SelectorName: String;
  SelectorType: TDBITokenType;
  Selectors: TStringList;
  Position: TDBITokenPosition;
  Operation: String;

begin
  Result := True;
  Scope := ANode;

  Selectors := Local(TStringList.Create).Obj as TStringlist;
  Selectors.Duplicates := dupIgnore;
  Selectors.Sorted := True;

  repeat
    SelectorType := Input.Token.TokenType;

    // Selectors: Class,, ID, Element
    if (Input.Token.TokenKind = tkIdentifier) then begin
      Position := Input.Token.TokenPosition;
      SelectorName := Input.Fetch([tkIdentifier]);

      Scope := GetSelector(Scope, SelectorType, SelectorName);
      Scope.Position := Position;
      TDBICssGenericStyle(Scope).Operator := Operation;

      Operation := '';
    end

    // Pseudo-Classes and -Elements
    else if (Input.Token.TokenType in [Tok_Colon, Tok_ColonColon]) then begin
      Position := Input.Token.TokenPosition;
      SelectorName := Input.Fetch([Tok_Colon, Tok_ColonColon]);
      SelectorName := SelectorName + Input.Fetch([Tok_Identifier]);

      Scope := GetSelector(Scope, SelectorType, SelectorName);
      Scope.Position := Position;
      TDBICssGenericStyle(Scope).Operator := Operation;

      Operation := '';
    end

    // If it's a comma then reset the scope and create a new branch
    // The existing scope should ne added to a list to process later
    else if (Input.Token.TokenType = Tok_Comma) then begin
      Selectors.AddObject(Scope.GetNodeName, Scope);
      Scope := ANode;

      Operation := Input.Fetch([Tok_Comma]);
    end

    // Process other types of selector operators
    else if (Input.Token.TokenType in [Tok_Greater, Tok_Plus, Tok_Tilde]) then begin
      Operation := Input.Fetch([Tok_Greater, Tok_Plus, Tok_Tilde]);
    end

    // Why am I here?
    else begin
      Input.Check([tkIdentifier]);
      Exit;
    end;
  until (Input.Token.TokenType = Tok_OpenCurlyBracket) or Input.IsXML;

  // Reached the end of the selector(s) - OK add the last selector to the list
  Selectors.AddObject(Scope.GetNodeName, Scope);

  // Process Items for this style
  Result := ProcessStyleElements(Selectors);
end;


function TDBICssAnalyser.ProcessStyleElements(Selectors: TStrings): Boolean;
var
  Element: TDBICssStyleItem;
  ElementText: String;
  ElementData: String;
  Selector: TDBICssNode;
  Index: Integer;

begin
  Result := True;

  if Input.Have([Tok_OpenCurlyBracket]) then begin
    repeat
      ElementText := Input.Fetch([Tok_Identifier, Tok_Minus]);
      Input.Fetch([Tok_Colon]);

      ElementData := '';
      repeat
        ElementData :=  ElementData + Input.Token.TokenString;
        Input.GetToken;

      until Input.Have([Tok_SemiColon]);

      for Index := 0 to Selectors.Count-1 do begin
        Selector := Selectors.Objects[Index] as TDBICssNode;

        Element := TDBICssStyleItem.Create(Selector.ChildOwner, Selector);
        Element.Text := ElementText;
        Element.Data := ElementData;
      end;
    until (Input.Token.TokenType = Tok_CloseCurlyBracket);

    Input.Fetch([Tok_CloseCurlyBracket]);
  end;
end;





{ TDBICustomCssLexer }

function TDBICustomCssLexer.IsXML: Boolean;
const
  Tok_XMLTokens = [Tok_Smaller, Tok_Greater, Tok_Smaller_Slash];

begin
  Result := (tsXmlElement in Token.TokenStatus) or (Token.TokenType in Tok_XMLTokens);
end;


procedure TDBICustomCssLexer.LexIdentifier;
var
  TokenType: TDBITokenType;

begin
  TokenType := Token.TokenType;

  inherited LexIdentifier;

  if (LexerChar = Chr_Minus) then begin
    while (LexerChar = Chr_Minus) do begin
      Token.AddChar(LexerChar);

      while GetChar do begin
        if (LexerChar in soAlphaNumeric) then begin
          Token.AddChar(LexerChar);
        end
        else begin
          Break;
        end;
      end;

    end;
  end;

  Token.TokenType := TokenType;
end;


procedure TDBICustomCssLexer.LexInitialise;
var
  Character: AnsiChar;

begin
  inherited LexInitialise;

  // String Literals toggles State only!
  MapSymbol(LexSymbol, Chr_Apostrophe, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);

  // Identifiers
  for Character := 'a' to 'z' do begin
    MapSymbol(LexIdentifier, Character, Tok_Identifier, tkIdentifier);
  end;

  // Identifiers
  for Character := 'A' to 'Z' do begin
    MapSymbol(LexIdentifier, Character, Tok_Identifier, tkIdentifier);
  end;

  // Underscore Identifier
  MapSymbol(LexIdentifier, Chr_UnderScore, Tok_Identifier, tkIdentifier);

  // Css Selectors
  MapSymbol(LexIdentifier, Chr_Minus, Tok_Default, tkIdentifier);
  MapSymbol(LexIdentifier, Chr_Hash, Tok_Default, tkIdentifier);
  MapSymbol(LexIdentifier, Chr_Dot, Tok_Default, tkIdentifier);

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace, [tsStringLiteral, tsComment2, tsMask]);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace, [tsStringLiteral, tsComment2, tsMask]);

  // State: tsComment3 - /*...*/ style comments
  MapDualSymbol(LexNone, Chr_Slash_Star, Tok_Slash_Star, tkSymbol, [tsComment3]);
  MapDualSymbol(LexNone, Chr_Star_Slash, Tok_Star_Slash, tkSymbol, [tsComment3, tsMask]);

  MapDualSymbol(LexNone, Chr_ColonColon, Tok_ColonColon, tkSymbol);
end;


procedure TDBICustomCssLexer.NextToken;
begin
  inherited NextToken;

  while not Eof do begin
    // Whitespace
    if (Token.TokenKind = tkWhitespace) and not (tsStringLiteral in Token.TokenStatus) then begin
      inherited NextToken;
    end

    // tsComment3 or leadin
    else if (tsComment3 in Token.TokenStatus) or (Token.TokenType in Tokens_Comment3) then begin
      inherited NextToken;
    end
    else begin
      Break;
    end;
  end;

  if (Token.TokenType = Tok_Smaller_Slash) then begin
    inherited NextToken;

    if (Token.TokenType = Tok_Identifier) and (CompareText(Token.TokenString, 'Style') = 0 ) then begin
      Token.TokenType := Tok_Eof;
      Eof := True;
    end;
  end

  else if (Token.TokenType = Tok_Identifier) and (CompareText(Token.TokenString, 'unit') = 0) then begin
    Token.TokenType := Tok_Eof;
    Eof := True;
  end;

end;


function TDBICustomCssLexer.Upto(TokenTypes: TDBITokenTypes): String;
begin
  Result := '';

  while not Eof do begin
    if (not (tsStringLiteral in Token.TokenStatus)) and (Token.TokenType in TokenTypes) then begin
      Break;
    end;

    Result := Result + Token.TokenString;
    inherited NextToken;
  end;
end;





{ TDBICssNode }

function TDBICssNode.ChildOwner: TComponent;
begin
  if Assigned(Owner) then begin
    Result := Owner;
  end
  else begin
    Result := Self;
  end;

  Assert(Assigned(Result));
end;


function TDBICssNode.GetClassKindName: String;
const
  KindOffset = 8; // length('TDBICss') + 1;
begin
  Result := LowerCase(Copy(Self.ClassName, KindOffset, 128));
end;


function TDBICssNode.GetNode(const ACompoundName: String): TDBICssNode;

  function GetOwner: TComponent;
  begin
    Result := Owner;
    if not Assigned(Result) then begin
      Result := Self;
    end;
  end;

var
  Item: TComponent;
  ItemName: String;
  ItemIndex: Integer;

begin
  Result := nil;
  for ItemIndex := 0 to GetOwner.ComponentCount-1 do begin
    Item := GetOwner.Components[ItemIndex];
    ItemName := (Item as TDBICssNode).GetNodeName;
    if CompareText(ItemName, ACompoundName) = 0 then begin
      Result := GetOwner.Components[ItemIndex] as TDBICssNode;
      Break;
    end;
  end;
end;


function TDBICssNode.GetNodeName: String;
var
  ObjectPath: String;

begin
  Result := GetText;

  if (Parent is TDBICssNode) then begin
    ObjectPath := TDBICssNode(Parent).GetNodeName;
     if (ObjectPath <> '') then begin
      Result := ObjectPath + '.' + Result;
    end;
  end;
end;


procedure TDBICssNode.SetClassKindName(const Value: String);
begin
  raise EPropertyError.Create('Property "ClassKindName" is read only');
end;


end.
