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
  1.0 | 29/05/2002 16:54:01 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : DBILib}

unit DBITokenizers;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DBIStreamAdapters, DBITokenizerConsts;

type
  TDBIString = AnsiString;
  TDBIChar = AnsiChar;
  PDBIChar = PAnsiChar;

type
  TDBILexerToken = class(TPersistent)
  private
    FColumn: Integer;
    FPosition: Integer;
    FRow: Integer;
    FTokenKind: TDBITokenKind;
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
    procedure SetTokenRow(const Value: Integer);

  public
    constructor Create;

    procedure AddChar(const Value: TDBIChar);
    procedure Clear;
    procedure ClearString;
    
    property AsChar: TDBIChar read GetTokenChar write SetTokenChar;
    property AsInteger: Integer read GetTokenInteger;
    property AsString: TDBIString read FTokenString write FTokenString;

    property Column: Integer read FColumn write FColumn;
    property Row: Integer read FRow write SetTokenRow;
    property Position: Integer read FPosition write FPosition;

    property TokenKind: TDBITokenKind read FTokenKind write FTokenKind;
    property TokenName: String read GetTokenName;
    property TokenStatus: TDBITokenStatus read FTokenStatus write FTokenStatus;
    property TokenString: String read GetTokenString;
    property TokenType: TDBITokenType read FTokenType write FTokenType;

  end;

type
  TDBIAbstractLexer = class(TDBICustomStreamAdapter)
  private
    FPutbackBuffer: array[0..Pred(PUTCHARBUFFERSIZE)] of TDBIChar;
    FBufferIndex: Integer;

    FInternalColumn: Integer;
    FInternalPosition: Integer;
    FInternalRow: Integer;

    FLexerChar: TDBIChar;
    FLexerCharMap: TDBILexerCharacterMap;
    FLexerEof: Boolean;
    FLexerStatus: TDBITokenStatus;
    FLexerToken: TDBILexerToken;

  protected
    function GetBufferIndex: Integer;
    function GetToken: TDBILexerToken;
    function GetTokenType: TDBITokenType;

    procedure LexInitialise; virtual;
    procedure LexSymbol; virtual;
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
    procedure SetStatus(Value: TDBITokenStatus);

    property BufferIndex: Integer read GetBufferIndex write SetBufferIndex;
    property Eof: Boolean read FLexerEof write SetEof default False;
    property LexerChar: TDBIChar read FLexerChar write FLexerChar;
    property LexerCharMap: TDBILexerCharacterMap read FLexerCharMap;
    property LexerStatus: TDBITokenStatus read FLexerStatus;
    property LexerTokenType: TDBITokenType read GetTokenType;
    property Token: TDBILexerToken read GetToken;

  public
    constructor Create(AStream: TStream = nil); override;
    destructor Destroy; override;

    function GetChar: Boolean; virtual;
    procedure PutChar(const Value: TDBIChar); virtual;

    procedure NextToken; virtual;

    procedure PutBack(const Value: AnsiString); overload;
{$ifdef Delphi2009}
    procedure PutBack(const Value: WideString); overload;
{$endif}

    procedure Reset; override;

  end;

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
    function GetChar: Boolean; override;
    procedure PutChar(const Value: TDBIChar); override;

    property Eof;
    property Text;
    property Token;

  end;

  TDBICustomAsciiLexerClass = class of TDBICustomAsciiLexer;

type
  TDBICustomParser = class(TPersistent)
  private
    FInput: TDBIAbstractLexer;
    FOutput: TDBIStreamFormatter;

  protected
    function GetInput: TDBICustomAsciiLexer;
    function GetLexerClassType: TDBICustomAsciiLexerClass; virtual; abstract;
    function GetOutput: TDBIStreamFormatter;

    procedure SyntaxError(
      const Caller: String;
      const ErrMsg: String;
      Args: array of const
      ); virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Input: TDBICustomAsciiLexer read GetInput;
    property Output: TDBIStreamFormatter read GetOutput;

  end;


type
  TDBICustomMacroLexer = class(TDBICustomAsciiLexer)
  protected
    procedure LexInitialise; override;
    procedure LexMacro;

  end;


type
  TDBIGetParam = function(
      Sender: TObject; const ParamName: TDBIString; var ParamValue: Variant
      ): Boolean of object;

  TDBICustomMacroProcessor = class(TDBICustomParser)
  private
    FOnGetParam: TDBIGetParam;

  protected
    function GetParam(
      Sender: TObject; const ParamName: TDBIString; var ParamValue: Variant
      ): Boolean; virtual;

    function GetLexerClassType: TDBICustomAsciiLexerClass; override;
    function MacroExpand: Boolean;

    property OnGetParam: TDBIGetParam read FOnGetParam write FOnGetParam;

  public
    procedure Process; virtual;

  end;


implementation

uses
  TypInfo;


{ TDBICustomMacroProcessor }

function TDBICustomMacroProcessor.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBICustomMacroLexer;
end;


function TDBICustomMacroProcessor.GetParam(
  Sender: TObject;
  const ParamName: TDBIString;
  var ParamValue: Variant
  ): Boolean;
begin
  Result := Assigned(FOnGetParam) and FOnGetParam(SEnder, ParamName, ParamValue);
end;


function TDBICustomMacroProcessor.MacroExpand: Boolean;
const
  ErrMsg = 'Unable to subsitute macro value, SQL Parameter "%s" not found!';

var
  ParamValue: Variant;
  ParamString: String;

begin
  // Deal with the $Macros, and ${Macros}
  Result := Input.Token.TokenType = Tok_Macro;
  if Result then begin
    ParamValue := varNull;
    if GetParam(Self, Input.Token.AsString, ParamValue) then begin
      ParamString := ParamValue;

      Input.PutBack(ParamString);
    end
    else begin
      Output.WriteStr('$' + Input.Token.AsString);
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
  Token.TokenType := LexerTokenType;
  Token.AsString := LexerChar;
  Leadin := LexerChar;
  GetChar;

  HasDelimiter := LexerChar = Chr_OpenCurlyBracket;
  if HasDelimiter then begin
    GetChar;
  end;

  // If Ch is alphanumeric then we have an identifier
  if (LexerChar in soAlphaNumeric) then begin
    Token.TokenKind := tkMacro;
    Token.AsString := LexerChar;
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
  FreeAndNil(FOutput);
  FreeAndNil(FInput);

  inherited Destroy;
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
  Jvr - 06/06/2007 15:57:20 - Initial code.<br>
}
procedure TDBICustomParser.SyntaxError(
  const Caller: String;
  const ErrMsg: String;
  Args: array of const
  );
const
  MsgFormat =
    '%s Syntax Error at [Line: %d, Offset: %d]'#13 +
    '  %s'#13 +
    '  Token = "%s"'#13 +
    '  Type = "%s"'#13 +
    '  TDBIString = "%s"';

var
  Address: Pointer;

  function GetUnitName: TDBIString;
  begin
    Result := TDBIString(TypInfo.GetTypeData(Self.ClassInfo)^.UnitName);
  end;

begin
  // Get return address
  asm
    mov eax, [ebp + 4]
    mov Address, eax
  end;

  raise Exception.CreateFmt(MsgFormat, [
    Caller,
    Input.Token.Row,
    Input.Token.Column,
    Format(ErrMsg, Args),
    Input.Token.TokenName,
    GetEnumName(TypeInfo(TDBITokenKind), Ord(Input.Token.TokenKind)),
    Input.Token.AsString
    ]) at Address;
end;





{ TDBIAbstractLexer }

// _____________________________________________________________________________
{**
  Jvr - 07/02/2005 17:18:36 - Updated code.<br>
}
constructor TDBIAbstractLexer.Create(AStream: TStream = nil);
begin
  inherited Create(AStream);

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
  if (BufferIndex > 0) then begin
    FLexerChar := FPutbackBuffer[BufferIndex];
    BufferIndex := BufferIndex-1;
    Result := True;
  end
  else begin
    FInternalPosition := ReadChar(FLexerChar);
    Result := FInternalPosition > 0;
  end;

  Eof := not Result;
end;


// _____________________________________________________________________________
{**
  Jvr - 11/03/2005 14:55:26 - Initial code.<br>
}
function TDBIAbstractLexer.GetToken: TDBILexerToken;
begin
  if not Assigned(FLexerToken) then begin
    FLexerToken := TDBILexerToken.Create;
  end;
  Result := FLexerToken;
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

  SetStatus(PSymbolData^.TokenStatus);
  GetChar;
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
var
  PSymbolData: PLexerSymbolData;

begin
  // Update Token Position
  Token.Position := FInternalPosition;
  Token.Column := FInternalColumn;
  Token.Row := FInternalRow;

  PSymbolData := @(FLexerCharMap[LexerChar]);
  Token.TokenKind := PSymbolData.TokenKind;
  Token.TokenStatus := LexerStatus;

  if Eof then begin
    LexEof;
  end
  else begin
    PSymbolData^.LexerProc;
  end;
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


// _____________________________________________________________________________
{**
  Jvr - 07/08/2006 18:52:22 - Initial code.<br>
}
procedure TDBIAbstractLexer.Reset;
begin
  FBufferIndex := 0;

  FLexerChar := #0;
  FLexerEof := False;
  FLexerStatus := tsNone;

  FreeAndNil(FLexerToken);

  FInternalColumn := 0;
  FInternalPosition := -1;
  FInternalRow := 1;

  GetChar;
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
  FLexerEof := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/03/2005 15:37:45 - Initial code.<br/>

  Example<br/>
  <pre>
   tsComment2           = $00000200;   // comment style-2 /* ... */
   tsQuotedString       = $0F010000;   // Quoted String state flag toggle
   tsToggleMask (AND)   = $F0FFFFFF;

   Intermediate Result  = $00010200;
   tsQuotedString       = $0F010000;   // Quoted String state flag toggle

   Final Result         = $00000200;   // still in comment style-2 state
  </pre>
}
procedure TDBIAbstractLexer.SetStatus(Value: TDBITokenStatus);
var
  LexerState: TDBILexerState;

begin
  // Exclude
  if (tsMask in Value) then begin
    FLexerStatus := FLexerStatus - Value;
  end

  // Toggle
  else if (tsToggle in Value) then begin
    Value := Value - [tsToggle];
    for LexerState := Low(TDBILexerState) to High(TDBILexerState) do begin
      if (LexerState in Value) then begin
        if (LexerState in FLexerStatus) then begin
          FLexerStatus := FLexerStatus - [LexerState];
        end
        else begin
          FLexerStatus := FLexerStatus + [LexerState];
        end;
      end;
    end;
  end

  // Include
  else begin
    FLexerStatus := FLexerStatus + Value;
  end;
end;





{ TDBICustomAsciiLexer }

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
    FInternalRow := FInternalRow + 1;
    FInternalColumn := 0;
  end
  else begin
    FInternalColumn := FInternalColumn + 1;
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
    if not  (PSymbolData^.TokenType in [Tok_Unassigned, Tok_None]) then begin //##JVR(PSymbolData^.TokenType <> Tok_None) then begin
      Token.AddChar(LexerChar);

      GetChar;
    end

    // Otherwise the token is a single character symbol
    else begin
//##JVR      PSymbolData := PSymbol1Data;
      PSymbolData := @(FLexerSymbolMap[PSymbol1Data.TokenType, Tok_Unassigned]);
    end;
  end;

  // Assign the type
  Token.TokenType := PSymbolData^.TokenType;

  // Change state according to the state array
  if (PSymbolData^.TokenStatus <> tsNoChange) then begin
    SetStatus(PSymbolData^.TokenStatus);

    // if we are setting a state then assign state "Before"
    if {not} (tsMask in PSymbolData^.TokenStatus) then begin
      Token.TokenStatus := LexerStatus;
    end;

  end;

  // Call the mapped lexer function if it is assigned and not Eof
  if Assigned(PSymbolData^.LexerProc) and not Eof then begin
    PSymbolData.LexerProc;
  end;
end;


// _____________________________________________________________________________
{**
  An Identifier may only start with a character or underscore.

  Jvr - 07/02/2005 17:25:56 - Updated code.<br>
}
procedure TDBICustomAsciiLexer.LexIdentifier;
begin
  Token.AsChar := LexerChar;
  Token.TokenType := Tok_Identifier;

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

  // Numbers
  for Character := '0' to '9' do begin
    MapSymbol(LexNumber, Character, Tok_NoChange, tkNumber);
  end;

  // Underscore Identifier
  Character := '_';
  MapSymbol(LexIdentifier, Character, Tok_NoChange, tkIdentifier);

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
  if (PSymbolData^.TokenStatus <> tsNoChange) then begin
    SetStatus(PSymbolData^.TokenStatus);

    // if we are setting a state then assign state "Before"
    if {not} (tsMask in PSymbolData^.TokenStatus) then begin
      Token.TokenStatus := LexerStatus;
    end;
  end;
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

begin
  // This Dual symbol data is for Single Char Symbols
  // It only needs to be initialised once!
  if not Assigned (FLexerSymbolMap[TokenCharacterMap[Symbols[0]], Tok_Unassigned].LexerProc) then begin
    // Assign FLexerCharMap[Char].LexerProc to [Token1, Tok_UnAssigned]
    // If the LexecProc is LexSymbol, then assign nil,
    // because the LexDualSymbol has allready performed the LexSymbol task
    //
    // The LexecProc in this case is the Lexer procedure to be called from LexDualSymbol,
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
  PSymbolData.TokenType := ATokenType;
  PSymbolData.TokenKind := ATokenKind;
  PSymbolData.TokenStatus := ATokenStatus;
  PSymbolData.LexerProc := ALexerProc;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/02/2005 20:55:14 - Initial code.<br>
}
procedure TDBICustomAsciiLexer.PutChar(const Value: TDBIChar);
begin
  inherited PutChar(Value);

  FInternalColumn := FInternalColumn - 0;
  if (Value = Chr_CarriageReturn) then begin
    FInternalRow := FInternalRow - 1;
  end;
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

    TokenData.FRow := FRow;
    TokenData.FColumn := FColumn;
    TokenData.FPosition := FPosition;
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
  FRow := 1;
  FColumn := 0;
  FPosition := 0;
  FTokenType := Tok_UnAssigned;
  FTokenKind := tkUnAssigned;
  FTokenStatus := tsNone;

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


function TDBILexerToken.GetTokenString: String;
begin
  Result := String(FTokenString);
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


// _____________________________________________________________________________
{**
  Jvr - 25/05/2007 14:35:52 - Initial code - For debugging purposes.<br>
}
procedure TDBILexerToken.SetTokenRow(const Value: Integer);
begin
  FRow := Value;
end;


end.
