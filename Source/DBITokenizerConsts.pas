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
  1.0 | 29/05/2002 10:11:02 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBITokenizerConsts;

interface

const
  Chr_Null                   = #0;     // NUL
  Chr_Backspace              = #8;     // BS
  Chr_HorizontalTab          = #9;     // TAB
  Chr_LineFeed               = #10;    // LF
  Chr_CarriageReturn         = #13;    // CR
  Chr_Eof                    = #26;    // EOF
  Chr_Space                  = #32;    // SP

  Chr_Bang                   = #33;    // !
  Chr_Quotes                 = #34;    // "
  Chr_Hash                   = #35;    // #
  Chr_Dollar                 = #36;    // $
  Chr_Percent                = #37;    // %
  Chr_Ampersand              = #38;    // &
  Chr_Apostrophe             = #39;    // '
  Chr_OpenBracket            = #40;    // (
  Chr_CloseBracket           = #41;    // )
  Chr_Multiply               = #42;    // *
  Chr_Plus                   = #43;    // +
  Chr_Comma                  = #44;    // ,
  Chr_Minus                  = #45;    // -
  Chr_Dot                    = #46;    // .
  Chr_Divide                 = #47;    // /
  Chr_Colon                  = #58;    // :
  Chr_SemiColon              = #59;    // ;
  Chr_Smaller                = #60;    // <
  Chr_Equals                 = #61;    // =
  Chr_Greater                = #62;    // >
  Chr_QuestionMark           = #63;    // ?
  Chr_AtSign                 = #64;    // @
  Chr_OpenSquareBracket      = #91;    // [
  Chr_BackSlash              = #92;    // \
  Chr_CloseSquareBracket     = #93;    // ]
  Chr_Caret                  = #94;    // ^
  Chr_UnderScore             = #95;    // _
  Chr_Accent                 = #96;    // `
  Chr_OpenCurlyBracket       = #123;   // {
  Chr_Pipe                   = #124;   // |
  Chr_CloseCurlyBracket      = #125;   // }
  Chr_Tilde                  = #126;   // ~

  // Aliases
  Chr_Asterisk               = Chr_Multiply;
  Chr_SingleQuote            = Chr_Apostrophe;

  // Dual-character Symbols
  Chr_Assign                 = ':=';
  Chr_AssignAdd              = '+=';
  Chr_AssignAnd              = '&=';
  Chr_AssignDivide           = '/=';
  Chr_AssignModulus          = '%=';
  Chr_AssignMultiply         = '*=';
  Chr_AssignOr               = '|=';
  Chr_AssignSubtract         = '-=';
  Chr_AssignXor              = '^=';
  Chr_Decrement              = '--';
  Chr_DotDot                 = '..';
  Chr_ColonColon             = '::';
  Chr_EqualGreater           = '=>';
  Chr_GreaterEqual           = '>=';
  Chr_Equality               = '==';
  Chr_Increment              = '++';
  Chr_InEquality             = '!=';
  Chr_LogicalAnd             = '&&';
  Chr_LogicalOr              = '||';
  Chr_NotEqual               = '<>';
  Chr_OpenBracket_Star       = '(*';
  Chr_CloseBracket_Star      = '*)';
  Chr_OpenCurlyBracket_Star  = '{*';
  Chr_CloseCurlyBracket_Star = '*}';
  Chr_OpenCurlyBracket_Dollar= '{$';
  Chr_OpenCurlyBracket_Hash  = '{#';
  Chr_CloseCurlyBracket_Hash = '#}';
  Chr_Pointer                = '->';
  Chr_ShiftLeft              = '<<';
  Chr_ShiftRight             = '>>';
  Chr_SlashSlash             = '//';
  Chr_Slash_Greater          = '/>';
  Chr_Slash_Star             = '/*';
  Chr_Star_Slash             = '*/';
  Chr_SmallerEqual           = '<=';
  Chr_Smaller_Slash          = '</';

  Chr_Dollar_OpenCurlyBracket= '${';
  Chr_Ampersand_Hash         = '&#';

  // Special Escaped Symbols
  Chr_EscapeAtSign           = '\@';
  Chr_EscapeDollar           = '\$';
  Chr_EscapeTab              = '\~';

  // Nonstandard Symbols
  Chr_Asm_Segment            = 'ASM';
  Chr_Asm_Segment_End        = 'END';

const
  soWhiteSpace = [Chr_Space, Chr_CarriageReturn, Chr_LineFeed, Chr_HorizontalTab];
  soUpperCase = ['A'..'Z'];
  soLowerCase = ['a'..'z'];
  soLetters = soUpperCase + soLowerCase + [Chr_Underscore];
  soDigits = ['0'..'9'];
  soDecimalDigits = soDigits + [Chr_Dot];
  soHexDigits = soDigits + ['A'..'F'] + ['a'..'f'];
  soAlphaNumeric = soLetters + soDigits;
  soAlphaNumericObject = soAlphaNumeric + [Chr_Dot];

type
  TDBITokenKind = (
    tkUnassigned,
    tkWhitespace,
    tkSymbol,
    tkNumber,
    tkIdentifier,
    tkMacro
    );
  TDBITokenKinds = set of TDBITokenKind;

type
  TDBILexerState = (
    tsToggle,            // Mask to detect if TokenState is Toggle
    tsMask,              // Mask to detect if TokenState is Mask
    tsStringLiteral,
    tsXmlElement,
    tsComment1,          // State: tsComment1 - {.....} style comments
    tsComment2,          // State: tsComment2 - //..... style comments
    tsComment3,          // State: tsComment3 - (*...*) style comments - /*...*/ as well
    tsMacro,
    tsUnknown
    );
  TDBITokenStatus = set of TDBILexerState;

const
  tsNone = [];
  tsNoChange = [];

type
 TDBITokenType = (
    Tok_UnAssigned,                    // Used for Single Token Values

    { Control Characters }
    Tok_Null,                          //  #0 = ^@
    Tok_SOH,                           //  #1 = ^A
    Tok_STX,                           //  #2 = ^B
    Tok_ETX,                           //  #3 = ^C
    Tok_EOT,                           //  #4 = ^D
    Tok_ENQ,                           //  #5 = ^E
    Tok_ACK,                           //  #6 = ^F
    Tok_Bell,                          //  #7 = ^G
    Tok_Backspace,                     //  #8 = ^H
    Tok_HorizontalTab,                 //  #9 = ^I
    Tok_LineFeed,                      // #10 = ^J
    Tok_VerticalTab,                   // #11 = ^K
    Tok_FormFeed,                      // #12 = ^L
    Tok_CarriageReturn,                // #13 = ^M
    Tok_SO,                            // #14 = ^N
    Tok_SI,                            // #15 = ^O
    Tok_DLE,                           // #16 = ^P
    Tok_DC1,                           // #17 = ^Q
    Tok_DC2,                           // #18 = ^R
    Tok_DC3,                           // #19 = ^S
    Tok_DC4,                           // #20 = ^T
    Tok_NAK,                           // #21 = ^U
    Tok_SYN,                           // #22 = ^V
    Tok_ETB,                           // #23 = ^W
    Tok_CAN,                           // #24 = ^X
    Tok_EM,                            // #25 = ^Y
    Tok_SUB,                           // #26 = ^Z
    Tok_Escape,                        // #27 = Escape
    Tok_FS,                            // #28
    Tok_GS,                            // #29
    Tok_RS,                            // #30
    Tok_US,                            // #31

    { Simple Symbols }
    Tok_Space,                         // #32 = Space
    Tok_Bang,                          // #33 = !
    Tok_Quotes,                        // #34 = "
    Tok_Hash,                          // #35 = #
    Tok_Dollar,                        // #36 = $
    Tok_Percent,                       // #37 = %
    Tok_Ampersand,                     // #38 = &
    Tok_Apostrophe,                    // #39 = '
    Tok_OpenBracket,                   // #40 = (
    Tok_CloseBracket,                  // #41 = )
    Tok_Multiply,                      // #42 = *
    Tok_Plus,                          // #43 = +
    Tok_Comma,                         // #44 = ,
    Tok_Minus,                         // #45 = -
    Tok_Dot,                           // #46 = .
    Tok_Divide,                        // #47 = /
    Tok_Colon,                         // #58 = :
    Tok_SemiColon,                     // #59 = ;
    Tok_Smaller,                       // #60 = <
    Tok_Equals,                        // #61 = =
    Tok_Greater,                       // #62 = >
    Tok_QuestionMark,                  // #63 = ?
    Tok_AtSign,                        // #64 = @
    Tok_OpenSquareBracket,             // #91 = [
    Tok_BackSlash,                     // #92 = \
    Tok_CloseSquareBracket,            // #93 = ]
    Tok_Caret,                         // #94 = ^
    Tok_UnderScore,                    // #95 = _
    Tok_Accent,                        // #96 = `
    Tok_OpenCurlyBracket,              //#123 = {
    Tok_Pipe,                          //#124 = |
    Tok_CloseCurlyBracket,             //#125 = }
    Tok_Tilde,                         //#126 = ~

    Tok_Digit,                         // #48 = 0 ..  #57 = 9
    Tok_UppercaseLetter,               // #65 = A ..  #90 = Z
    Tok_LowerCaseLetter,               // #97 = a .. #122 = z
    Tok_Binary,                        //#127     .. #255

    { Complex Symbols }
    Tok_Assign,                        // ':='
    Tok_AssignAdd,                     // '+='
    Tok_AssignAnd,                     // '&='
    Tok_AssignDivide,                  // '/='
    Tok_AssignModulus,                 // '%='
    Tok_AssignMultiply,                // '*='
    Tok_AssignOr,                      // '|='
    Tok_AssignSubtract,                // '-='
    Tok_AssignXor,                     // '^='
    Tok_Decrement,                     // '--'
    Tok_DotDot,                        // '..'
    Tok_ColonColon,                    // '::'
    Tok_EqualGreater,                  // '=>'
    Tok_GreaterEqual,                  // '>='
    Tok_Equality,                      // '=='
    Tok_Increment,                     // '++'
    Tok_InEquality,                    // '!='
    Tok_LogicalAnd,                    // '&&'
    Tok_LogicalOr,                     // '||'
    Tok_NotEqual,                      // '<>'
    Tok_OpenBracket_Star,              // '(*'
    Tok_CloseBracket_Star,             // '*)'
    Tok_OpenCurlyBracket_Star,         // '{*'
    Tok_CloseCurlyBracket_Star,        // '*}'
    Tok_OpenCurlyBracket_Dollar,       // '{$'
    Tok_OpenCurlyBracket_Hash,         // '{#'
    Tok_CloseCurlyBracket_Hash,        // '#}'
    Tok_Pointer,                       // '->'
    Tok_ShiftLeft,                     // '<<'
    Tok_ShiftRight,                    // '>>'
    Tok_SlashSlash,                    // '//'
    Tok_Slash_Greater,                 // '/>'
    Tok_Slash_Star,                    // '/*'
    Tok_Star_Slash,                    // '*/'
    Tok_SmallerEqual,                  // '<='
    Tok_Smaller_Slash,                 // '</'

    Tok_EscapeAtSign,                  // '\@'
    Tok_EscapeDollar,                  // '\$'
    Tok_EscapeQuotes,                  // '\"'
    Tok_EscapeBackSpace,               // '\b'
    Tok_EscapeFormFeed,                // '\f'
    Tok_EscapeLineFeed,                // '\n'
    Tok_EscapeReturn,                  // '\r'
    Tok_EscapeTab,                     // '\t'

    { Special Tokens }
    Tok_None,                          // Nothing / Unassigned
    Tok_NoChange,                      // Used as parameter, indicates no change
    Tok_Default,                       // Used as default parameter, means lookup
    Tok_LineBreak,                     // CR/LF
    Tok_Identifier,                    // Any Word inc. Letters,Digits,Underscores
    Tok_NameSpace,                     // Identifiers joined with dot notation
    Tok_ControlCharacter,
    Tok_QuotedString,
    Tok_IntegerLiteral,
    Tok_FloatLiteral,
    Tok_HexLiteral,
    Tok_Eof,
    Tok_Macro,
    Tok_UTF8
    );

  TDBITokenTypes = set of TDBITokenType;


const
  // Aliases
  Tok_Asterisk = Tok_Multiply;
  Tok_SingleQuote = Tok_Apostrophe;
  Tok_DoubleQuote = Tok_Quotes;
  Tok_Slash = Tok_Divide;

  // Special sets
  Tok_Comparison = [
    Tok_Smaller,
    Tok_Greater,
    Tok_GreaterEqual,
    Tok_Equality,
    Tok_InEquality,
    Tok_NotEqual,
    Tok_SmallerEqual
    ];

    
const
  // Map characters to tokens
  TokenCharacterMap: array[#0..#127] of TDBITokenType = (

    { Control Characters }
    Tok_Null,                          //  #0 = ^@
    Tok_SOH,                           //  #1 = ^A
    Tok_STX,                           //  #2 = ^B
    Tok_ETX,                           //  #3 = ^C
    Tok_EOT,                           //  #4 = ^D
    Tok_ENQ,                           //  #5 = ^E
    Tok_ACK,                           //  #6 = ^F
    Tok_Bell,                          //  #7 = ^G
    Tok_BackSpace,                     //  #8 = ^H
    Tok_HorizontalTab,                 //  #9 = ^I
    Tok_LineFeed,                      // #10 = ^J
    Tok_VerticalTab,                   // #11 = ^K
    Tok_FormFeed,                      // #12 = ^L
    Tok_CarriageReturn,                // #13 = ^M
    Tok_SO,                            // #14 = ^N
    Tok_SI,                            // #15 = ^O
    Tok_DLE,                           // #16 = ^P
    Tok_DC1,                           // #17 = ^Q
    Tok_DC2,                           // #18 = ^R
    Tok_DC3,                           // #19 = ^S
    Tok_DC4,                           // #20 = ^T
    Tok_NAK,                           // #21 = ^U
    Tok_SYN,                           // #22 = ^V
    Tok_ETB,                           // #23 = ^W
    Tok_CAN,                           // #24 = ^X
    Tok_EM,                            // #25 = ^Y
    Tok_SUB,                           // #26 = ^Z
    Tok_Escape,                        // #27 = Escape
    Tok_FS,                            // #28
    Tok_GS,                            // #29
    Tok_RS,                            // #30
    Tok_US,                            // #31
    Tok_Space,                         // #32 = Space

    { Standard Characters }
    Tok_Bang,                          // #33 = !
    Tok_Quotes,                        // #34 = "
    Tok_Hash,                          // #35 = #
    Tok_Dollar,                        // #36 = $
    Tok_Percent,                       // #37 = %
    Tok_Ampersand,                     // #38 = &
    Tok_Apostrophe,                    // #39 = '
    Tok_OpenBracket,                   // #40 = (
    Tok_CloseBracket,                  // #41 = )
    Tok_Multiply,                      // #42 = *
    Tok_Plus,                          // #43 = +
    Tok_Comma,                         // #44 = ,
    Tok_Minus,                         // #45 = -
    Tok_Dot,                           // #46 = .
    Tok_Divide,                        // #47 = /
    Tok_Digit,                         // #48 = 0
    Tok_Digit,                         // #48 = 1
    Tok_Digit,                         // #48 = 2
    Tok_Digit,                         // #48 = 3
    Tok_Digit,                         // #48 = 4
    Tok_Digit,                         // #48 = 5
    Tok_Digit,                         // #48 = 6
    Tok_Digit,                         // #48 = 7
    Tok_Digit,                         // #48 = 8
    Tok_Digit,                         // #48 = 9
    Tok_Colon,                         // #58 = :
    Tok_SemiColon,                     // #59 = ;
    Tok_Smaller,                       // #60 = <
    Tok_Equals,                        // #61 = =
    Tok_Greater,                       // #62 = >
    Tok_QuestionMark,                  // #63 = ?
    Tok_AtSign,                        // #64 = @
    Tok_UppercaseLetter,               // #65 = A
    Tok_UppercaseLetter,               // #65 = B
    Tok_UppercaseLetter,               // #65 = C
    Tok_UppercaseLetter,               // #65 = D
    Tok_UppercaseLetter,               // #65 = E
    Tok_UppercaseLetter,               // #65 = F
    Tok_UppercaseLetter,               // #65 = G
    Tok_UppercaseLetter,               // #65 = H
    Tok_UppercaseLetter,               // #65 = I
    Tok_UppercaseLetter,               // #65 = J
    Tok_UppercaseLetter,               // #65 = K
    Tok_UppercaseLetter,               // #65 = L
    Tok_UppercaseLetter,               // #65 = M
    Tok_UppercaseLetter,               // #65 = N
    Tok_UppercaseLetter,               // #65 = O
    Tok_UppercaseLetter,               // #65 = P
    Tok_UppercaseLetter,               // #65 = Q
    Tok_UppercaseLetter,               // #65 = R
    Tok_UppercaseLetter,               // #65 = S
    Tok_UppercaseLetter,               // #65 = T
    Tok_UppercaseLetter,               // #65 = U
    Tok_UppercaseLetter,               // #65 = V
    Tok_UppercaseLetter,               // #65 = W
    Tok_UppercaseLetter,               // #65 = X
    Tok_UppercaseLetter,               // #65 = Y
    Tok_UppercaseLetter,               // #65 = Z
    Tok_OpenSquareBracket,             // #91 = [
    Tok_BackSlash,                     // #92 = \
    Tok_CloseSquareBracket,            // #93 = ]
    Tok_Caret,                         // #94 = ^
    Tok_UnderScore,                    // #95 = _
    Tok_Accent,                        // #96 = `
    Tok_LowerCaseLetter,               // #97 = a
    Tok_LowerCaseLetter,               // #97 = b
    Tok_LowerCaseLetter,               // #97 = c
    Tok_LowerCaseLetter,               // #97 = d
    Tok_LowerCaseLetter,               // #97 = e
    Tok_LowerCaseLetter,               // #97 = f
    Tok_LowerCaseLetter,               // #97 = g
    Tok_LowerCaseLetter,               // #97 = h
    Tok_LowerCaseLetter,               // #97 = i
    Tok_LowerCaseLetter,               // #97 = j
    Tok_LowerCaseLetter,               // #97 = k
    Tok_LowerCaseLetter,               // #97 = l
    Tok_LowerCaseLetter,               // #97 = m
    Tok_LowerCaseLetter,               // #97 = n
    Tok_LowerCaseLetter,               // #97 = o
    Tok_LowerCaseLetter,               // #97 = p
    Tok_LowerCaseLetter,               // #97 = q
    Tok_LowerCaseLetter,               // #97 = r
    Tok_LowerCaseLetter,               // #97 = s
    Tok_LowerCaseLetter,               // #97 = t
    Tok_LowerCaseLetter,               // #97 = u
    Tok_LowerCaseLetter,               // #97 = v
    Tok_LowerCaseLetter,               // #97 = w
    Tok_LowerCaseLetter,               // #97 = x
    Tok_LowerCaseLetter,               // #97 = y
    Tok_LowerCaseLetter,               // #97 = z
    Tok_OpenCurlyBracket,              //#123 = {
    Tok_Pipe,                          //#124 = |
    Tok_CloseCurlyBracket,             //#125 = }
    Tok_Tilde,                         //#126 = ~
    Tok_Binary                         //#127 = DEL
    );

type
  TDBIChar = Char;
  TDBILexerProcedure = procedure of object;
  TDBILexerSymbolsParameter = array[0..1] of Char;
  TDBILexerSymbolTypesParameter = array[0..1] of TDBITokenType;

  PLexerSymbolData = ^TDBILexerSymbolData;
  TDBILexerSymbolData = record
    TokenType: TDBITokenType;
    TokenKind: TDBITokenKind;
    TokenStatus: TDBITokenStatus;
    LexerProc: TDBILexerProcedure;
  end;


  // ___________________________________________________________________________
  {**
    Jvr - 09/06/2010 14:58:50 - Updated code.<br />

    NOTES:<br>

    I have expanded the Symbol Map to include the control characters
    This may cause some unexpected issues, but I hope NOT!
  }
  TDBILexerSymbolArray = array[Tok_UnAssigned..Tok_Tilde] of TDBILexerSymbolData;
  TDBILexerSymbolMap = array[Tok_Null..Tok_Macro] of TDBILexerSymbolArray;
  TDBILexerCharacterMap = array[#0..#255] of TDBILexerSymbolData;

const
  PUTCHARBUFFERSIZE = $2000;

  // This constant is used as a parameter to the methods MapSymbol() and MapDualSymbol()
  // it means to only map the symbols without calling a Lexer Procedure
  LexNone = nil;

implementation

end.