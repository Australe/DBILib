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
  1.0 | 29/05/2002 16:54:01 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBITokenizers;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DBITypInfo, DBIStreamAdapters, DBITokenizerConsts;

type
  EAnalyserError = class(Exception);

type
  TDBIString = AnsiString;
  TDBIChar = AnsiChar;
  PDBIChar = PAnsiChar;

type
{$ifdef Delphi7}
  TDBILexerData = record
{$else}
  TDBILexerData = object
{$endif}
    Column: Integer;
    Row: Integer;
    Position: Integer;
    Eof: Boolean;
    LexerChar: TDBIChar;
    PriorChar: TDBIChar;
    Status: TDBITokenStatus;

    procedure Clear;
    procedure UpdateAsciiPosition;
  end;


type
  TDBILexerToken = class(TPersistent)
  private
    FTokenKind: TDBITokenKind;
    FTokenPosition: TDBITokenPosition;
    FTokenStatus: TDBITokenStatus;
    FTokenString: TDBIString;
    FTokenType: TDBITokenType;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetTokenChar: TDBIChar;
    function GetTokenInteger: Integer;
    function GetTokenName: String;
    function GetTokenString: String;

    procedure SetTokenChar(const Value: TDBIChar);
    procedure SetTokenString(const Value: String);

  public
    constructor Create;

    procedure AddChar(const Value: TDBIChar);
    procedure Clear;
    procedure ClearString;

    procedure Info(const AMessage: String);

    property AsChar: TDBIChar read GetTokenChar write SetTokenChar;
    property AsInteger: Integer read GetTokenInteger;
    property AsString: TDBIString read FTokenString write FTokenString;

    property Column: Integer read FTokenPosition.Column write FTokenPosition.Column;
    property Row: Integer read FTokenPosition.Row write FTokenPosition.Row;
    property Position: Integer read FTokenPosition.Position write FTokenPosition.Position;

    property TokenKind: TDBITokenKind read FTokenKind write FTokenKind;
    property TokenName: String read GetTokenName;
    property TokenPosition: TDBITokenPosition read FTokenPosition;
    property TokenStatus: TDBITokenStatus read FTokenStatus write FTokenStatus;
    property TokenString: String read GetTokenString write SetTokenString;
    property TokenType: TDBITokenType read FTokenType write FTokenType;

  end;


type
  TDBICustomScope = class;
  TDBICustomScopeClass = class of TDBICustomScope;
  TDBICustomScopeCallback = procedure(AScope: TDBICustomScope) of object;

  TDBICustomScope = class(TDBILexerToken)
  private
    FCallBack: TDBICustomScopeCallback;
    FData: TObject;
    FParent: TDBICustomScope;

  protected
    function GetEnabled: Boolean; virtual;

  public
    function CheckType(AClassType: TDBICustomScopeClass): Boolean;

    property CallBack: TDBICustomScopeCallback read FCallback write FCallback;
    property Data: TObject read FData write FData;
    property Enabled: Boolean read GetEnabled;
    property Parent: TDBICustomScope read FParent write FParent;

  end;


type
  TDBIScopeStack = class(TPersistent)
  private
    FItems: TList;

  protected
    function GetItems: TList;
    function GetTop: TDBICustomScope;

    property Items: TList read GetItems;

  public
    destructor Destroy; override;

    function Push(Item: TDBICustomScope): TDBICustomScope;
    function Pop: TDBICustomScope;

    property Top: TDBICustomScope read GetTop;
  end;


type
  TDBIAbstractLexer = class(TDBICustomStreamAdapter)
  private
    FPutbackBuffer: array[0..Pred(PUTCHARBUFFERSIZE)] of TDBIChar;
    FBufferIndex: Integer;
    FLexerCharMap: TDBILexerCharacterMap;
    FLexerData: TDBILexerData;
    FLexerToken: TDBILexerToken;

  protected
    function GetBufferIndex: Integer;
    function GetLexerToken: TDBILexerToken;
    function GetTokenType: TDBITokenType;

    procedure LexInitialise; virtual;
    procedure LexSymbol; overload; virtual;
    procedure LexSymbol(AChar: TDBIChar); overload;
    procedure LexEof; virtual;
    procedure LexNop; virtual;

    procedure MapSymbol(
      const ALexerProc: TDBILexerProcedure;
      const ASymbol: TDBIChar;
      const ATokenType: TDBITokenType = Tok_Default;
      const ATokenKind: TDBITokenKind = tkSymbol;
      const ATokenStatus: TDBITokenStatus = tsNoChange
      );

    procedure SetBufferIndex(const Value: Integer);
    procedure SetEof(const Value: Boolean);
    procedure SetStatus(Value: TDBITokenStatus);  virtual;
    procedure UpdateStatus(Value: TDBITokenStatus);

    property BufferIndex: Integer read GetBufferIndex write SetBufferIndex;
    property Eof: Boolean read FLexerData.Eof write SetEof default False;
    property PriorChar: TDBIChar read FLexerData.PriorChar write FLexerData.PriorChar;
    property LexerChar: TDBIChar read FLexerData.LexerChar write FLexerData.LexerChar;
    property LexerCharMap: TDBILexerCharacterMap read FLexerCharMap;
    property LexerStatus: TDBITokenStatus read FLexerData.Status;
    property LexerTokenType: TDBITokenType read GetTokenType;
    property Token: TDBILexerToken read FLexerToken;

  public
    constructor Create(AStream: TStream = nil); override;
    destructor Destroy; override;

    function GetChar: Boolean; virtual;
    function GetToken: TDBILexerToken;

    procedure PutChar(const Value: TDBIChar); virtual;
    procedure PutStr(const Value: AnsiString);

    procedure NextToken; virtual;

    procedure PutBack(const Value: AnsiString); overload;
{$ifdef Delphi2009}
    procedure PutBack(const Value: WideString); overload;
{$endif}

    procedure Reset; override;
    procedure Restore(AToken: TDBILexerToken);

  end;


type
  TDBICustomAsciiLexer = class(TDBIAbstractLexer)
  private
    FLexerSymbolMap: TDBILexerSymbolMap;

  protected
    procedure LexDualSymbol; virtual;
    procedure LexInitialise; override;
    procedure LexIdentifier; virtual;
    procedure LexNumber; virtual;
    procedure LexWhiteSpace; virtual;

    procedure MapDualSymbol(
      const ALexerProc: TDBILexerProcedure;
      const Symbols: TDBILexerSymbolsParameter;
      const ATokenType: TDBITokenType;
      const ATokenKind: TDBITokenKind = tkSymbol;
      const ATokenStatus: TDBITokenStatus = tsNoChange
      );

    procedure MapDualSymbolType(
      const ALexerProc: TDBILexerProcedure;
      const Symbol1: TDBITokenType;
      const Symbol2: TDBITokenType;
      const ATokenType: TDBITokenType;
      const ATokenKind: TDBITokenKind = tkSymbol;
      const ATokenStatus: TDBITokenStatus = tsNoChange
      );

  public
    function Check(TokenTypes: TDBITokenTypes): String; overload;
    function Check(TokenKinds: TDBITokenKinds): String; overload;

    function Fetch(TokenTypes: TDBITokenTypes): String; overload;
    function Fetch(TokenKinds: TDBITokenKinds): String; overload;

    function Have(TokenTypes: TDBITokenTypes): Boolean; overload;
    function Have(TokenKinds: TDBITokenKinds): Boolean; overload;

    function Skip(TokenTypes: TDBITokenTypes): String; overload;
    function Skip(TokenKinds: TDBITokenKinds): String; overload;

  public
    function GetChar: Boolean; override;
    procedure PutChar(const Value: TDBIChar); override;

    property Eof;
    property Text;
    property Token;

  end;
  TDBICustomAsciiLexerClass = class of TDBICustomAsciiLexer;


type
  TDBICustomXMLLexer = class(TDBICustomAsciiLexer)
  protected
    procedure LexXMLEntity;
    procedure LexEncodedSymbol;
    procedure LexInitialise; override;

  end;


type
  TDBICustomMacroLexer = class(TDBICustomAsciiLexer)
  protected
    procedure LexInitialise; override;
    procedure LexMacro;

  end;


type
  TDBICustomParser = class(TPersistent)
  private
    FInput: TDBIAbstractLexer;
    FOutput: TDBIStreamFormatter;
    FScope: TDBIScopeStack;

  protected
    procedure EnabledHandler(Sender: TObject; var Enabled: Boolean);

    function GetInput: TDBICustomAsciiLexer;
    function GetLexerClassType: TDBICustomAsciiLexerClass; virtual; abstract;
    function GetOutput: TDBIStreamFormatter;
    function GetScope: TDBIScopeStack; virtual;

    procedure SyntaxError(
      const Caller: String;
      const ErrMsg: String;
      Args: array of const
      ); virtual;

    property Scope: TDBIScopeStack read GetScope;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Input: TDBICustomAsciiLexer read GetInput;
    property Output: TDBIStreamFormatter read GetOutput;

  end;


type
  TDBIGetParam = function(
      Sender: TObject; const ParamName: String; var ParamValue: Variant
      ): Boolean of object;

  TDBICustomMacroProcessor = class(TDBICustomParser)
  private
    FOnGetParam: TDBIGetParam;

  protected
    function GetParam(
      Sender: TObject; const ParamName: String; var ParamValue: Variant
      ): Boolean; virtual;

    function GetLexerClassType: TDBICustomAsciiLexerClass; override;
    function MacroExpand: Boolean;

    property OnGetParam: TDBIGetParam read FOnGetParam write FOnGetParam;

  public
    procedure Process; virtual;

  end;


type
  TDBIReflectionMacroProcessor = class(TDBICustomMacroProcessor)
  private
    FProperties: TDBIProperties;
    function GetProperties: TStrings;

  protected
    function GetParam(
      Sender: TObject; const ParamName: String; var ParamValue: Variant
      ): Boolean; override;

    property Properties: TStrings read GetProperties;

  public
    destructor Destroy; override;

  end;


function Macro(const Format: String; const Args: array of const): String;


implementation

uses
{$ifdef Delphi6}
  Types,
{$endif}
  Windows,
  TypInfo,
  DBIUtils;


function Macro(const Format: String; const Args: array of const): String;
var
  Processor: TDBIReflectionMacroProcessor;
  Index: Integer;

begin
  Result := '';

  if (Format <> '') then begin
    Processor := Local(TDBIReflectionMacroProcessor.Create).Obj as TDBIReflectionMacroProcessor;
    Processor.Input.Text := Format;

    for Index := Low(Args) to High(Args) do begin
      if (Args[Index].VType = vtObject) and (Args[Index].VObject is TPersistent) then begin
        TDBIProperties.GetProperties(TPersistent(Args[Index].VObject), Processor.Properties);
      end;
    end;

    Processor.Process;
    Result := Processor.Output.Text;
  end;
end;



{ TDBIReflectionMacroProcessor }

destructor TDBIReflectionMacroProcessor.Destroy;
begin
  FProperties.Free;
  FProperties := nil;

  inherited Destroy;
end;


function TDBIReflectionMacroProcessor.GetParam(
  Sender: TObject;
  const ParamName: String;
  var ParamValue: Variant
  ): Boolean;
begin
  Result := Properties.IndexOfName(ParamName) > -1;
  if Result then begin
    ParamValue := Properties.Values[ParamName];
  end
  else begin
    Paramvalue := '';
  end;

  Result := True;
end;


function TDBIReflectionMacroProcessor.GetProperties: TStrings;
begin
  if not Assigned(FProperties) then begin
    FProperties := TDBIProperties.Create;
    FProperties.Duplicates := dupIgnore;
    FProperties.Sorted := True;
  end;
  Result := FProperties;
end;



{ TDBICustomMacroProcessor }

function TDBICustomMacroProcessor.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBICustomMacroLexer;
end;


function TDBICustomMacroProcessor.GetParam(
  Sender: TObject;
  const ParamName: String;
  var ParamValue: Variant
  ): Boolean;
begin
  Result := Assigned(FOnGetParam) and FOnGetParam(Sender, ParamName, ParamValue);
end;


function TDBICustomMacroProcessor.MacroExpand: Boolean;
const
  ErrMsg = 'Unable to subsitute macro value for parameter "%s", '#13#13'%s';

var
  ParamValue: Variant;
  ParamString: String;

begin
  // Deal with the $Macros, and ${Macros}
  Result := Input.Token.TokenType = Tok_Macro;
  if Result then begin
    try
      ParamValue := varNull;
      if GetParam(Self, Input.Token.TokenString, ParamValue) then begin
        ParamString := ParamValue;

        Input.PutBack(ParamString);
      end
      else begin
        Output.WriteStr('$' + Input.Token.AsString);
      end;

    except
      on E: Exception do
        raise Exception.CreateFmt(ErrMsg, [Input.Token.TokenString, E.Message]);
    end;

    // Get the first token from the expanded input
    Input.NextToken;
  end;
end;


procedure TDBICustomMacroProcessor.Process;
begin
  // BootStrap the lexer;
  Input.Reset;

  while (Input.Token.TokenType <> Tok_Eof) do begin
    if not MacroExpand then begin
      Output.WriteStr(Input.Token.AsString);
      Input.NextToken;
    end;
  end;
end;





{ TDBICustomParser }

constructor TDBICustomParser.Create;
begin
  inherited Create;

  // Place holder (to allow constructor to be virtual)
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:14:08 - Initial code.<br>
}
destructor TDBICustomParser.Destroy;
begin
  FreeAndNil(FScope);
  FreeAndNil(FOutput);
  FreeAndNil(FInput);

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/01/2011 12:28:05 - Initial code.<br />
}
procedure TDBICustomParser.EnabledHandler(Sender: TObject; var Enabled: Boolean);
begin
  if Enabled and Assigned(Scope.Top) then begin
    Enabled := Scope.Top.Enabled;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:04:04 - Initial code.<br>
}
function TDBICustomParser.GetInput: TDBICustomAsciiLexer;
begin
  if not Assigned(FInput) then begin
    FInput := GetLexerClassType.Create;
  end;
  Result := FInput as TDBICustomAsciiLexer;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:14:12 - Initial code.<br />
}
function TDBICustomParser.GetOutput: TDBIStreamFormatter;
begin
  if not Assigned(FOutput) then begin
    FOutput := TDBIStreamFormatter.Create;
  end;
  Result := FOutput;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:14:12 - Initial code.<br />
}
type
  TDBIProtectedStreamFormatter = class(TDBIStreamFormatter);

function TDBICustomParser.GetScope: TDBIScopeStack;
begin
  if not Assigned(FScope) then begin
    FScope := TDBIScopeStack.Create;

    // Register Callback to ensure that the output.enabled property
    // correctly reflects the scope.enabled;
    TDBIProtectedStreamFormatter(Output).EnabledCallback := EnabledHandler;
  end;
  Result := FScope;
end;


// _____________________________________________________________________________
{**
  Jvr - 06/06/2007 15:57:20 - Initial code.<br>
}
procedure TDBICustomParser.SyntaxError(
  const Caller: String;
  const ErrMsg: String;
  Args: array of const
  );
{$ifndef fpc}
var
  Address: Pointer;
{$endif}

const
  MsgFormat =
    '%s Syntax Error at [Line: %d, Offset: %d]'#13 +
    '  %s'#13 +
    '  Token = "%s"'#13 +
    '  Type = "%s"'#13 +
    '  String = "%s"';

begin
{$ifndef fpc}
  // Get return address
  asm
    mov eax, [ebp + 4]
    mov Address, eax
  end;
{$endif}

  raise Exception.CreateFmt(MsgFormat, [
    Caller,
    Input.Token.Row,
    Input.Token.Column,
    Format(ErrMsg, Args),
    Input.Token.TokenName,
    GetEnumName(TypeInfo(TDBITokenKind), Ord(Input.Token.TokenKind)),
    Input.Token.AsString
    ]) {$ifndef fpc} at Address {$endif} ;
end;





{ TDBICustomMacroLexer }

procedure TDBICustomMacroLexer.LexInitialise;
begin
  inherited LexInitialise;

  // ${Macros}
  MapSymbol(LexMacro, Chr_Dollar);

  // String Literals toggles State only!
  MapSymbol(LexSymbol, Chr_Quotes, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);
  MapSymbol(LexSymbol, Chr_Apostrophe, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace);
end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2005 14:31:17 - Initial code.<br>
  Jvr - 07/06/2007 14:56:14 - Added allowance for dot notation (objects).<br>
}
procedure TDBICustomMacroLexer.LexMacro;
var
  Leadin: TDBIChar;
  HasDelimiter: Boolean;

  function IsValidMacroChar(const AChar: TDBIChar): Boolean;
  begin
    if HasDelimiter then begin
      Result := (AChar <> Chr_CloseCurlyBracket);
    end
    else begin
      Result := AChar in soAlphaNumericObject;
    end;
  end;

begin
  Leadin := LexerChar;
  GetChar;

  HasDelimiter := LexerChar = Chr_OpenCurlyBracket;
  if HasDelimiter then begin
    GetChar;
  end;

  // If Ch is alphanumeric then we have an identifier
  if (LexerChar in soAlphaNumeric) then begin
    Token.TokenKind := tkMacro;
    Token.AsChar := LexerChar;
    while GetChar do begin
      if IsValidMacroChar(LexerChar) then begin
        Token.AsString := Token.AsString + LexerChar;
      end
      else begin
        Break;
      end;
    end;

    // Swallow terminating CloseCurlyBracket
    if HasDelimiter then begin
      GetChar;
    end;

    Token.TokenType := Tok_Macro;
    Token.TokenKind := tkMacro;
  end

  // Otherwise it is an orphan Dollar symbol
  else begin
    // Push it all back
    PutChar(LexerChar);
    if HasDelimiter then begin
      PutChar(Chr_OpenCurlyBracket);
    end;
    PutChar(Leadin);

    // Now get the next character
    GetChar;

    inherited LexSymbol;
  end;
end;





{ TDBICustomXMLLexerr }

procedure TDBICustomXMLLexer.LexXMLEntity;
const
  XMLEntityFilter = [
    'A', 'a',  // &amp;, &apos;
    'G', 'g',  // &gt;
    'L', 'l',  // &lt;
    'Q', 'q'   // &quot;
    ];

var
  PSymbolData: PLexerSymbolData;

begin
  // Swallow Leadin Ampersand and check next character
  if (LexerChar in XMLEntityFilter) then begin
    PSymbolData := @(LexerCharMap[Chr_Ampersand]);
    Token.TokenType := PSymbolData^.TokenType;
    SetStatus(PSymbolData^.TokenStatus);

    if (LexerChar in ['A', 'a']) then begin
      GetChar;

      // &amp;
      if (LexerChar in ['M', 'm']) then begin
        Assert(GetChar and (LexerChar in ['P', 'p']));
        Token.AsString := Chr_Ampersand;
      end

      // &apos;
      else if (LexerChar in ['P', 'p']) then begin
        Assert(GetChar and (LexerChar in ['O', 'o']));
        Assert(GetChar and (LexerChar in ['S', 's']));
        Token.AsString := Chr_Apostrophe;
      end;
    end

    // &gt;
    else if (LexerChar in ['G', 'g']) and GetChar and (LexerChar in ['T', 't']) then begin
      Token.AsString := Chr_Greater;
    end

    // &lt;
    else if (LexerChar in ['L', 'l']) and GetChar and (LexerChar in ['T', 't']) then begin
      Token.AsString := Chr_Smaller;
    end

    // &quot;
    else if (LexerChar in ['Q', 'q']) and GetChar and (LexerChar in ['U', 'u']) then begin
      Assert(GetChar and (LexerChar in ['O', 'o']));
      Assert(GetChar and (LexerChar in ['T', 't']));
      Token.AsString := Chr_Quotes;
    end

    else begin
      raise Exception.Create('Unsupported XML Entity');
    end;

    Assert(GetChar and (LexerChar = ';'));
    // Swallow terminating SemiColon
    GetChar;
  end

  // POtherwise Undo
  else begin
    PutChar(Chr_Ampersand);

    inherited LexSymbol;
  end;
end;


procedure TDBICustomXMLLexer.LexEncodedSymbol;
var
  PSymbolData: PLexerSymbolData;

begin
  // If Ch is numeric then we have a valid encoded symbol
  if (LexerChar in soDigits) then begin
    PSymbolData := @(LexerCharMap[LexerChar]);
    Token.TokenType := PSymbolData^.TokenType;
    Token.AsString := LexerChar;

    SetStatus(PSymbolData^.TokenStatus);

    while GetChar and (LexerChar in soDigits) do begin
      Token.AddChar(LexerChar);
    end;

    // Swallow terminating SemiColon
    Assert(LexerChar = ';');
    GetChar;

    Token.AsString := TDBIString(Chr(Token.AsInteger));
  end
  else begin
    inherited LexSymbol;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 26/05/2013 00:56:38 - Updated code.<br>

  Remember to Map Single Symbols before Dual Symbols
}
procedure TDBICustomXMLLexer.LexInitialise;
begin
  inherited LexInitialise;

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace);

{$ifdef DebugInfo}
  // String Literals - toggles State only!
  MapSymbol(LexSymbol, Chr_Quotes, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);
  MapSymbol(LexSymbol, Chr_Apostrophe, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);
{$endif}

  // Xml Elements - toggles State only!
  MapSymbol(LexSymbol, Chr_Smaller, Tok_Default, tkSymbol, [tsXmlElement]);
  MapSymbol(LexSymbol, Chr_Greater, Tok_Default, tkSymbol, [tsXmlelement, tsMask]);

{$ifdef DebugInfo}
  // State: tsComment3 - /*...*/ style comments
  MapDualSymbol(LexNone, Chr_Slash_Star, Tok_Slash_Star, tkSymbol, [tsComment3]);
  MapDualSymbol(LexNone, Chr_Star_Slash, Tok_Star_Slash, tkSymbol, [tsComment3, tsMask]);
{$endif}

  // Xml Elements - toggles State only!
  MapDualSymbol(LexNone, Chr_Smaller_Slash, Tok_Smaller_Slash, tkSymbol, [tsXmlElement]);
  MapDualSymbol(LexNone, Chr_Slash_Greater, Tok_Slash_Greater, tkSymbol, [tsXmlElement, tsMask]);

  // Always Map the Single symbol before the Dual symbol with the same first char
  MapSymbol(LexXMLEntity, Chr_Ampersand, Tok_Default, tkSymbol);
  MapDualSymbol(LexEncodedSymbol, Chr_Ampersand_Hash, Tok_Macro, tkSymbol);
end;





{ TDBICustomAsciiLexer }

// _____________________________________________________________________________
{**
  Jvr - 16/12/2010 13:43:43 - Initial code.<br />
}
function TDBICustomAsciiLexer.Check(TokenTypes: TDBITokenTypes): String;
const
  ErrMsg = 'Unexpected Token "%s" at [ %d, %d ]';

begin
  Result := Token.TokenString;
  if not (Token.TokenType in TokenTypes) then begin
    raise EAnalyserError.CreateFmt(ErrMsg, [Token.TokenString, Token.Row, Token.Column]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/12/2010 18:56:47 - Initial code.<br />
}
function TDBICustomAsciiLexer.Check(TokenKinds: TDBITokenKinds): String;
const
  ErrMsg = 'Unexpected Token "%s" at [ %d, %d ]';

begin
  Result := Token.TokenString;
  if not (Token.TokenKind in TokenKinds) then begin
    raise EAnalyserError.CreateFmt(ErrMsg, [Token.TokenString, Token.Row, Token.Column]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2008 21:08:51 - Initial code.<br />
}
function TDBICustomAsciiLexer.Fetch(TokenTypes: TDBITokenTypes): String;
begin
  Result := Token.TokenString;
  Check(TokenTypes);
  NextToken;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2008 21:09:07 - Initial code.<br />
}
function TDBICustomAsciiLexer.Fetch(TokenKinds: TDBITokenKinds): String;
begin
  Result := Token.TokenString;
  Check(TokenKinds);
  NextToken;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/02/2005 11:38:57 - Updated code.<br>
}
function TDBICustomAsciiLexer.GetChar: Boolean;
begin
  Result := inherited GetChar;
  if Eof then begin
    LexerChar := Chr_Eof;
    Exit;
  end;

  if (LexerChar = Chr_CarriageReturn) then begin
    ; // Skip Carriage-return white space for row and column count
      // DOS uses CRLF, UNIX uses LF only, thus the CR is of no consequence
  end
  else if (LexerChar = Chr_LineFeed) then begin
    FLexerData.Row := FLexerData.Row + 1;
    FLexerData.Column := 0;
  end
  else begin
    FLexerData.Column := FLexerData.Column + 1;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/12/2010 10:07:44 - Initial code.<br />
}
function TDBICustomAsciiLexer.Have(TokenTypes: TDBITokenTypes): Boolean;
begin
  Result := (Token.TokenType in TokenTypes);
  if Result then begin;
    NextToken;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/02/2005 18:10:24 - Initial code.<br>
}
function TDBICustomAsciiLexer.Have(TokenKinds: TDBITokenKinds): Boolean;
begin
  Result := (Token.TokenKind in TokenKinds);
  if Result then begin
    NextToken;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/02/2005 20:17:02 - Initial code.<br>
}
procedure TDBICustomAsciiLexer.LexDualSymbol;
var
  PSymbolData: PLexerSymbolData;
  PSymbol1Data: PLexerSymbolData;
  PSymbol2Data: PLexerSymbolData;

begin
{$ifdef DebugInfo}
  // If the first character is of type ctrl or binary then Kaboom
  Assert(not ((LexerChar > Chr_Tilde) or (LexerChar < Chr_Bang)) );
{$endif}

  Token.AsChar := LexerChar;

  PSymbol1Data := @(FLexerCharMap[LexerChar]);
  GetChar;
  PSymbol2Data := @(FLexerCharMap[LexerChar]);

  // If the second character type is of type ctrl, alpha, numeric or binary
  // then the token is a single character symbol
  if (PSymbol2Data.TokenType > Tok_Tilde) or (PSymbol2Data.TokenType < Tok_Bang) then begin

    // Get the Single character mapped symbol data
    PSymbolData := @(FLexerSymbolMap[PSymbol1Data.TokenType, Tok_Unassigned]);
  end

  // Otherwise we need to check the Dual Symbol Map befor proceeding
  else begin

    // Get the Dual character mapped symbol data
    PSymbolData := @(FLexerSymbolMap[PSymbol1Data.TokenType, PSymbol2Data^.TokenType]);

    // Are these two symbols mapped to a TokenType?
    if not  (PSymbolData^.TokenType in [Tok_Unassigned, Tok_None]) then begin
      Token.AddChar(LexerChar);

      GetChar;
    end

    // Otherwise the token is a single character symbol
    else begin
      PSymbolData := @(FLexerSymbolMap[PSymbol1Data.TokenType, Tok_Unassigned]);
    end;
  end;

  // Assign the type
  Token.TokenType := PSymbolData^.TokenType;

  // Change state according to the state array
  UpdateStatus(PSymbolData^.TokenStatus);

  // Call the mapped lexer function if it is assigned and not Eof
  if Assigned(PSymbolData^.LexerProc) and not Eof then begin
    PSymbolData^.LexerProc;
  end;
end;


// _____________________________________________________________________________
{**
  An Identifier may only start with a character or underscore.

  Jvr - 07/02/2005 17:25:56 - Updated code.<br>
}
procedure TDBICustomAsciiLexer.LexIdentifier;
begin
  Token.TokenType := Tok_Identifier;
  Token.AsChar := LexerChar;

  while GetChar do begin
    if (LexerChar in soAlphaNumeric) then begin
      Token.AddChar(LexerChar);
    end
    else begin
      Break;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/02/2005 21:03:58 - Initial code.<br>
}
procedure TDBICustomAsciiLexer.LexInitialise;
var
  Character: TDBIChar;

begin
  inherited LexInitialise;

  // Identifiers
  for Character := 'a' to 'z' do begin
    MapSymbol(LexIdentifier, Character, Tok_NoChange, tkIdentifier);
  end;

  // Identifiers
  for Character := 'A' to 'Z' do begin
    MapSymbol(LexIdentifier, Character, Tok_NoChange, tkIdentifier);
  end;

  // Underscore Identifier
  MapSymbol(LexIdentifier, Chr_UnderScore, Tok_NoChange, tkIdentifier);

  // Numbers
  MapSymbol(LexNumber, Chr_Plus, Tok_NoChange, tkNumber);
  MapSymbol(LexNumber, Chr_Minus, Tok_NoChange, tkNumber);

  for Character := '0' to '9' do begin
    MapSymbol(LexNumber, Character, Tok_NoChange, tkNumber);
  end;

end;


// _____________________________________________________________________________
{**
  Jvr - 07/02/2005 18:40:38 - Updated code.<br>
}
procedure TDBICustomAsciiLexer.LexNumber;
begin
  Token.TokenType := Tok_IntegerLiteral;
  Token.AsChar := LexerChar;

  while GetChar do begin
    if (LexerChar = Chr_Dot) then begin
      GetChar;

      if (LexerChar = Chr_Dot) then begin
        PutChar(LexerChar);
        Break;
       end
       else begin
        PutChar(LexerChar);
        LexerChar := Chr_Dot;
        Token.TokenType := Tok_FloatLiteral;
      end;

    end;

    if (LexerChar in soDecimalDigits) then begin
      Token.AddChar(LexerChar);
    end
    else if (PriorChar in ['+', '-']) then begin
      LexSymbol(PriorChar);
      Break;
    end
    else begin
      Break;
    end;

  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/02/2005 14:40:03 - Initial code.<br>
}
procedure TDBICustomAsciiLexer.LexWhiteSpace;
var
  PSymbolData: PLexerSymbolData;

begin
  PSymbolData := @(FLexerCharMap[LexerChar]);

  Token.ClearString;
  Token.TokenType := FLexerCharMap[LexerChar].TokenType;

  while (Token.TokenType = FLexerCharMap[LexerChar].TokenType) and not Eof do begin
    Token.AddChar(LexerChar);
    GetChar;
  end;

  // Change state according to the state array
  UpdateStatus(PSymbolData^.TokenStatus);
end;


// _____________________________________________________________________________
{**
  Jvr - 03/03/2005 18:19:23 - Initial code.<br>
}
procedure TDBICustomAsciiLexer.MapDualSymbol(
  const ALexerProc: TDBILexerProcedure;
  const Symbols: TDBILexerSymbolsParameter;
  const ATokenType: TDBITokenType;
  const ATokenKind: TDBITokenKind = tkSymbol;
  const ATokenStatus: TDBITokenStatus = tsNoChange
  );
var
  LexerProc: TDBILexerProcedure;
  PSymbolData: PLexerSymbolData;

begin
  // This Dual symbol data is for Single Char Symbols
  // Check carefully - It should only be initialised once!
  PSymbolData := @(FLexerSymbolMap[TokenCharacterMap[Symbols[0]], Tok_Unassigned]);
  if (PSymbolData^.TokenType = Tok_Unassigned) and (PSymbolData^.TokenKind = tkUnassigned) then begin
    // Assign FLexerCharMap[Char].LexerProc to [Token1, Tok_UnAssigned]
    // If the LexecProc is LexSymbol, then assign nil,
    // because the LexDualSymbol has allready performed the LexSymbol task
    //
    // The LexerProc in this case is the Lexer procedure to be called from LexDualSymbol,
    // to post process the datastream after the detected Lead-in symbol(s)
    LexerProc := LexSymbol;
    if TMethod(LexerProc).Code <> TMethod(FLexerCharMap[Symbols[0]].LexerProc).Code then begin
      LexerProc := FLexerCharMap[Symbols[0]].LexerProc;
    end
    else begin
      LexerProc := nil;
    end;

    MapDualSymbolType(
      LexerProc,
      TokenCharacterMap[Symbols[0]],
      Tok_UnAssigned,
      FLexerCharMap[Symbols[0]].TokenType,
      FLexerCharMap[Symbols[0]].TokenKind,
      FLexerCharMap[Symbols[0]].TokenStatus
      );

    // Change FLexerCharMap[Char].LexerProc from LexSymbol to LexDualSymbol
    FLexerCharMap[Symbols[0]].LexerProc := LexDualSymbol;
  end;

  // Dual symbol data for Char pairs
  MapDualSymbolType(
    ALexerProc,
    TokenCharacterMap[Symbols[0]],
    TokenCharacterMap[Symbols[1]],
    ATokenType,
    ATokenKind,
    ATokenStatus
    );
end;


// _____________________________________________________________________________
{**
  Jvr - 03/03/2005 19:14:18 - Initial code.<br>
}
procedure TDBICustomAsciiLexer.MapDualSymbolType(
  const ALexerProc: TDBILexerProcedure;
  const Symbol1: TDBITokenType;
  const Symbol2: TDBITokenType;
  const ATokenType: TDBITokenType;
  const ATokenKind: TDBITokenKind = tkSymbol;
  const ATokenStatus: TDBITokenStatus = tsNoChange
  );
var
  PSymbolData: PLexerSymbolData;

begin
  PSymbolData := @(FLexerSymbolMap[Symbol1, Symbol2]);
  PSymbolData^.TokenType := ATokenType;
  PSymbolData^.TokenKind := ATokenKind;
  PSymbolData^.TokenStatus := ATokenStatus;
  PSymbolData^.LexerProc := ALexerProc;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/02/2005 20:55:14 - Initial code.<br>
}
procedure TDBICustomAsciiLexer.PutChar(const Value: TDBIChar);
begin
  inherited PutChar(Value);

  FLexerData.Column := FLexerData.Column - 0;
  if (Value = Chr_CarriageReturn) then begin
    FLexerData.Row := FLexerData.Row - 1;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/10/2009 12:09:08 - Initial code.<br />
}
function TDBICustomAsciiLexer.Skip(TokenTypes: TDBITokenTypes): String;
begin
  Result := '';
  while (Token.TokenType in TokenTypes) and not Eof do begin
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2008 20:50:03 - Initial code.<br />
}
function TDBICustomAsciiLexer.Skip(TokenKinds: TDBITokenKinds): String;
begin
  Result := '';
  while (Token.TokenKind in TokenKinds) and not Eof do begin
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;










{ TDBIAbstractLexer }

// _____________________________________________________________________________
{**
  Jvr - 07/02/2005 17:18:36 - Updated code.<br>
}
constructor TDBIAbstractLexer.Create(AStream: TStream = nil);
begin
  inherited Create(AStream);

  GetLexerToken;

  Reset;
  LexInitialise;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/02/2005 18:21:56 - Updated code.<br>
}
destructor TDBIAbstractLexer.Destroy;
begin
  Clear;

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/04/2008 15:47:42 - Initial code. - DEBUGGING PURPOSES <br />
}
function TDBIAbstractLexer.GetBufferIndex: Integer;
begin
  Result := FBufferIndex;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/02/2005 11:38:57 - Updated code.<br>
}
function TDBIAbstractLexer.GetChar: Boolean;
begin
  FLexerData.PriorChar := FLexerData.LexerChar;

  if (BufferIndex > 0) then begin
    FLexerData.LexerChar := FPutbackBuffer[BufferIndex];
    BufferIndex := BufferIndex-1;
    Result := True;
  end
  else begin
    FLexerData.Position := ReadChar(FLexerData.LexerChar);
    Result := FLexerData.Position > 0;
  end;

  Eof := not Result;
end;


// _____________________________________________________________________________
{**
  Jvr - 11/03/2005 14:55:26 - Initial code.<br>
}
function TDBIAbstractLexer.GetLexerToken: TDBILexerToken;
begin
  if not Assigned(FLexerToken) then begin
    FLexerToken := TDBILexerToken.Create;
  end;
  Result := FLexerToken;
end;


function TDBIAbstractLexer.GetToken: TDBILexerToken;
var
  PSymbolData: PLexerSymbolData;

begin
  // Update Token Position
  Token.Position := FLexerData.Position;
  Token.Column := FLexerData.Column;
  Token.Row := FLexerData.Row;

  PSymbolData := @(FLexerCharMap[LexerChar]);
  Token.TokenKind := PSymbolData^.TokenKind;
  Token.TokenStatus := LexerStatus;
  Token.TokenType := PSymbolData^.TokenType;

  if Eof then begin
    LexEof;
  end
  else begin
    PSymbolData^.LexerProc;
  end;

  Result := Token;
end;


// _____________________________________________________________________________
{**
  Jvr - 03/11/2008 09:02:21 - Initial code.<br>
}
function TDBIAbstractLexer.GetTokenType: TDBITokenType;
begin
  Result := FLexerCharMap[LexerChar].TokenType;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/02/2005 18:41:12 - Updated code.<br>
}
procedure TDBIAbstractLexer.LexEof;
begin
  Token.AsString := '';
  Token.TokenKind := tkSymbol;
  Token.TokenType := Tok_Eof;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/02/2005 19:34:37 - Initial code.<br>
}
procedure TDBIAbstractLexer.LexInitialise;
var
  Character: TDBIChar;

begin
  for Character := Low(FLexerCharMap) to High(FLexerCharMap) do begin
    MapSymbol(LexSymbol, Character);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/08/2006 16:44:35 - Initial code.<br>
}
procedure TDBIAbstractLexer.LexNop;
begin
  // NOP
end;


// _____________________________________________________________________________
{**
  Jvr - 24/02/2005 20:17:02 - Initial code.<br>
}
procedure TDBIAbstractLexer.LexSymbol;
var
  PSymbolData: PLexerSymbolData;

begin
  Token.AsChar := LexerChar;
  PSymbolData := @(FLexerCharMap[LexerChar]);
  Token.TokenType := PSymbolData^.TokenType;

  UpdateStatus(PSymbolData^.TokenStatus);
  GetChar;
end;


procedure TDBIAbstractLexer.LexSymbol(AChar: TDBIChar);
var
  PSymbolData: PLexerSymbolData;

begin
  Token.AsChar := AChar;
  PSymbolData := @(FLexerCharMap[AChar]);
  Token.TokenType := PSymbolData^.TokenType;

  UpdateStatus(PSymbolData^.TokenStatus);
end;


// _____________________________________________________________________________
{**
  Jvr - 03/03/2005 18:36:13 - Initial code.<br>
}
procedure TDBIAbstractLexer.MapSymbol(
  const ALexerProc: TDBILexerProcedure;
  const ASymbol: TDBIChar;
  const ATokenType: TDBITokenType = Tok_Default;
  const ATokenKind: TDBITokenKind = tkSymbol;
  const ATokenStatus: TDBITokenStatus = tsNoChange
  );
var
  PSymbolData: PLexerSymbolData;

begin
  Assert(Assigned(AlexerProc), 'Lexer procedure is required!');
  PSymbolData := @(FLexerCharMap[ASymbol]);

  // if Tok_Default then set to predefined mapped type
  if (ATokenType = Tok_Default) then begin
    if (ASymbol > Chr_Tilde) then begin
      PSymbolData^.TokenType := Tok_Binary;
    end
    else begin
      PSymbolData^.TokenType := TokenCharacterMap[ASymbol];
    end;
  end

  // Otherwise if not Tok_NoChange then set to specified TokenType;
  else if (ATokenType <> Tok_NoChange) then begin
    PSymbolData^.TokenType := ATokenType;
  end;

  PSymbolData^.TokenKind := ATokenKind;
  PSymbolData^.TokenStatus := ATokenStatus;
  PSymbolData^.LexerProc := ALexerProc;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/02/2005 12:35:41 - Updated code.<br>
}
procedure TDBIAbstractLexer.NextToken;
begin
  GetToken;
end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2005 16:52:20 - Initial code.<br>
  Jvr - 12/10/2006 15:14:17 - Now Checks for empty values.<br>
}
procedure TDBIAbstractLexer.PutBack(const Value: AnsiString);
var
  Index: Integer;

begin
  if (Value <> '') then begin
    if not Eof then begin
      PutChar(LexerChar);
    end;

    for Index := Length(Value) downto 1 do begin
      PutChar(Value[Index]);
    end;

    GetChar;
  end;
end;

{$ifdef Delphi2009}
procedure TDBIAbstractLexer.PutBack(const Value: WideString);
begin
  PutBack(AnsiString(Value));
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 08/02/2005 11:38:25 - Updated code.<br>
}
procedure TDBIAbstractLexer.PutChar(const Value: TDBIChar);
begin
  BufferIndex := BufferIndex + 1;
  Assert(BufferIndex < SizeOf(FPutbackBuffer));
  FPutbackBuffer[BufferIndex] := Value;
end;


procedure TDBIAbstractLexer.PutStr(const Value: AnsiString);
var
  Index: Integer;

begin
  if (Value <> '') then begin
    for Index := Length(Value) downto 1 do begin
      PutChar(Value[Index]);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/08/2006 18:52:22 - Initial code.<br>
}
procedure TDBIAbstractLexer.Reset;
begin
  inherited Reset;

  FLexerToken.Clear;
  FLexerData.Clear;
  FBufferIndex := 0;

  GetChar;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/10/2009 09:04:29 - Initial code.<br />
}
procedure TDBIAbstractLexer.Restore(AToken: TDBILexerToken);
begin
  // Restore Lexer Position
  Stream.Position := AToken.Position;

  // Restore Lexer State
  Token.Assign(AToken);

  FLexerData.Eof := False;
  FLexerData.Status := AToken.TokenStatus;

  FLexerData.Column := AToken.Column;
  FLexerData.Position := AToken.Position;
  FLexerData.Row := AToken.Row;

  GetChar;
  GetToken;
end;



// _____________________________________________________________________________
{**
  Jvr - 24/04/2008 15:48:36 - Initial code. - DEBUGGING PURPOSES <br />
}
procedure TDBIAbstractLexer.SetBufferIndex(const Value: Integer);
begin
  FBufferIndex := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/06/2007 12:52:58 - Initial code.<br>
}
procedure TDBIAbstractLexer.SetEof(const Value: Boolean);
begin
  FLexerData.Eof := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/03/2005 15:37:45 - Initial code.<br/>
}
procedure TDBIAbstractLexer.SetStatus(Value: TDBITokenStatus);
var
  LexerState: TDBILexerState;

begin
  // Exclude Comments when in a String Literal
  if (tsStringLiteral in FLexerData.Status) then begin
    Value := Value - [tsComment1, tsComment2, tsComment3];
  end

  // Exclude String Literal from Comments
  else if (tsComment1 in FLexerData.Status) or (tsComment2 in FLexerData.Status) or  (tsComment3 in FLexerData.Status) then begin
    Value := Value - [tsStringLiteral];
  end;

  // Mask - may need to change the order of this to first
  if (tsMask in Value) then begin
    FLexerData.Status := FLexerData.Status - Value;
  end

  // Toggle
  else if (tsToggle in Value) then begin
    Value := Value - [tsToggle];
    for LexerState := Low(TDBILexerState) to High(TDBILexerState) do begin
      if (LexerState in Value) then begin
        if (LexerState in FLexerData.Status) then begin
          FLexerData.Status := FLexerData.Status - [LexerState];
        end
        else begin
          FLexerData.Status := FLexerData.Status + [LexerState];
        end;
      end;
    end;
  end

  // Include
  else begin
    FLexerData.Status := FLexerData.Status + Value;
  end;
end;


procedure TDBIAbstractLexer.UpdateStatus(Value: TDBITokenStatus);
begin
  // Change state according to the state array
  if (Value <> tsNoChange) then begin
    SetStatus(Value);

    // if we are clearing a state then assign state "Before"
    if (tsMask in Value) then begin
      Token.TokenStatus := LexerStatus;
    end;
  end;
end;





{ TDBIScopeStack }

// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:21:51 - Initial code.<br />
}
destructor TDBIScopeStack.Destroy;
begin
  { TODO -ovip -cTDBIScopeStack.Destroy() : ##TPL - Check Stack is empty }
  if Assigned(FItems) then begin
    Assert(FItems.Count = 0);
    FItems.Free;
    FItems := nil;
  end;

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:22:04 - Initial code.<br />
}
function TDBIScopeStack.GetItems: TList;
begin
  if not Assigned(FItems) then begin
    FItems := TList.Create;
  end;
  Result := FItems;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:23:41 - Initial code.<br />
}
function TDBIScopeStack.GetTop: TDBICustomScope;
begin
  Result := nil;
  if Items.Count > 0 then begin
    Result := Items[Items.Count-1];
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:24:32 - Initial code.<br />
}
function TDBIScopeStack.Pop: TDBICustomScope;
begin
  { TODO -ovip -cTDBIScopeStack.Pop() : ##TPL - Pop Scope from the stack }
  Result := Items[Items.Count-1];
  Items.Remove(Result);
end;


// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:25:57 - Initial code.<br />
}
function TDBIScopeStack.Push(Item: TDBICustomScope): TDBICustomScope;
begin
  { TODO -ovip -cTDBIScopeStack.Push() : ##TPL - Push Scope on to the stack }
  Result := Item;
  if Assigned(Result) then begin
    Result.Parent := Top;
    Items.Add(Result);
  end;
end;





{ TDBICustomScope }

function TDBICustomScope.CheckType(AClassType: TDBICustomScopeClass): Boolean;
var
  ScopeName: String;

begin
  Result := Self is AClassType;

  if not Result then begin
    if Assigned(Self) then begin
      ScopeName := Self.ClassName;
    end
    else begin
      ScopeName := '<nil>, probably a syntax error!';
    end;

//##JVR    TDBICustomScope.Error(nil, Caller, '1995',
    raise Exception.CreateFmt(
      'Expected Scope of type "%s", actual scope is %s',
      [AClassType.ClassName, ScopeName]
      );
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 31/03/2011 12:44:04 - Initial code.<br />
}
function TDBICustomScope.GetEnabled: Boolean;
begin
  Result := Assigned(FData) and Assigned(FCallBack);
end;





{ TDBILexerToken }

// _____________________________________________________________________________
{**
  Jvr - 04/10/2008 13:18:38 - Initial code.<br />
}
procedure TDBILexerToken.AddChar(const Value: TDBIChar);
var
  Len: Integer;

begin
  // Optimuized code - Please do NOT change! - Yes it does make a difference
  Len := Length(FTokenString) + 1;
  SetLength(FTokenString, Len);
  FTokenString[Len] := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/02/2005 11:26:50 - Initial code.<br />
}
procedure TDBILexerToken.AssignTo(Dest: TPersistent);
var
  TokenData: TDBILexerToken;

begin
  if (Dest is TDBILexerToken) then begin
    TokenData := TDBILexerToken(Dest);
    TokenData.FTokenType := FTokenType;
    TokenData.FTokenKind := FTokenKind;
    TokenData.FTokenStatus := FTokenStatus;
    TokenData.FTokenString := FTokenString;

    TokenData.Row := Row;
    TokenData.Column := Column;
    TokenData.Position := Position;
  end
  else begin
    inherited AssignTo(Dest);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 16/06/2010 10:04:31 - Clear Re-initializes the whole object.<br />
}
procedure TDBILexerToken.Clear;
begin
  Row := 1;
  Column := 0;
  Position := 0;
  TokenType := Tok_UnAssigned;
  TokenKind := tkUnAssigned;
  TokenStatus := tsNone;

  SetLength(FTokenString, 0);
end;


// _____________________________________________________________________________
{**
  Jvr - 04/10/2008 13:14:53 - Initial code.<br />
}
procedure TDBILexerToken.ClearString;
begin
  SetLength(FTokenString, 0);
end;


// _____________________________________________________________________________
{**
  Jvr - 25/05/2007 14:51:03 - Initial code.<br>
}
constructor TDBILexerToken.Create;
begin
  inherited Create;

  Clear;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/10/2008 13:08:36 - Initial code.<br />
}
function TDBILexerToken.GetTokenChar: TDBIChar;
begin
  if Length(FTokenString) = 0 then begin
    Result := Chr_Null;
  end
  else begin
    Result := FTokenString[1];
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 03/05/2007 18:25:42 - Initial code.<br>
}
function TDBILexerToken.GetTokenInteger: Integer;
begin
  Result := StrToInt(String(FTokenString));
end;


// _____________________________________________________________________________
{**
  Jvr - 06/06/2007 16:26:35 - Initial code.<br>
}

function TDBILexerToken.GetTokenName: String;
var
  Len: Integer;

begin
  Len := Length('Tok_') + 1;

  Result := Copy(GetEnumName(TypeInfo(TDBITokenType), Ord(FTokenType)), Len, 32);
end;


// _____________________________________________________________________________
{**
  Jvr - 25/05/2007 14:35:52 - Initial code - For debugging purposes.<br>
}
function TDBILexerToken.GetTokenString: String;
begin
  Result := String(FTokenString);
end;


procedure TDBILexerToken.Info(const AMessage: String);
begin
  Windows.OutputDebugString(
    PChar(
      Format(
        'Class: %s, Position(%d = %d , %d) [%s] - %s',
        [ClassName, Position, Row, Column, TokenString, AMessage]
        )
      )
    );
end;


// _____________________________________________________________________________
{**
  Jvr - 04/10/2008 13:09:50 - Initial code.<br />
}
procedure TDBILexerToken.SetTokenChar(const Value: TDBIChar);
begin
  SetLength(FTokenString, 1);

  FTokenString[1] := Value;
end;


procedure TDBILexerToken.SetTokenString(const Value: String);
begin
  FTokenString := AnsiString(Value);
end;





{ TDBILexerData }

procedure TDBILexerData.Clear;
begin
  PriorChar := #0;
  LexerChar := #0;
  Eof := False;
  Status := tsNone;

  Column := 0;
  Position := -1;
  Row := 1;
end;


procedure TDBILexerData.UpdateAsciiPosition;
begin
  if (LexerChar = Chr_CarriageReturn) then begin
    ; // Skip Carriage-return white space for row and column count
      // DOS uses CRLF, UNIX uses LF only, thus the CR is of no consequence
  end
  else if (LexerChar = Chr_LineFeed) then begin
    Inc(Row, 1);
    Column := 0;
  end
  else begin
    Inc(Column, 1);
  end;
end;

end.
