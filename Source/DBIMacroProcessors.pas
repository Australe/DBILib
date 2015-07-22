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
  1.0 | 17/03/2005 11:04:40 | Jvr | Initial Release
  1.1 | 04/06/2014 06:38:24 | Jvr | Fully refactored
  ______________________________________________________________________________
}

{#omcodecop off : jvr : DBILib}

unit DBIMacroProcessors;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DBITokenizerConsts, DBITokenizers;

type
  TDBIMacroLexer = class(TDBICustomMacroLexer)
  protected
    procedure Info(const AMessage: String);

    procedure LexInitialise; override;
    procedure LexEscape;

  end;


type
  TDBIMacroProcessor = class(TDBICustomMacroProcessor)
  protected
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

  end;



implementation

uses
  Windows, TypInfo, Controls, Dialogs;



{ TDBIMacroProcessor }

// _____________________________________________________________________________
{**
  Jvr - 19/01/2006 13:50:34 - Initial code.<br>
}
function TDBIMacroProcessor.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBIMacroLexer;
end;  { GetLexerClassType }





{ TDBIMacroLexer }

procedure TDBIMacroLexer.Info(const AMessage: String);
begin
  OutputDebugString(
    PChar(
      Format(
        'Position(%d = %d , %d) [%s]'#13'%s',
        [Token.Position, Token.Row, Token.Column, Token.AsString, AMessage]
        )
      )
    );
end;


// _____________________________________________________________________________
{**
  Jvr - 11/12/2008 09:41:44 - Initial code.<br>
}
procedure TDBIMacroLexer.LexEscape;
begin
  if (Length(Token.AsString) > 1) then begin
    if (Token.AsString[2] = Chr_EscapeTab[2]) then
      Token.AsString := #9
    else
      Token.AsString := Token.AsString[2];
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/02/2005 17:58:33 - Initial code.<br>
}
procedure TDBIMacroLexer.LexInitialise;
begin
  inherited LexInitialise;

  // Standard operators
  MapDualSymbol(LexNone, Chr_GreaterEqual, Tok_GreaterEqual, tkSymbol);
  MapDualSymbol(LexNone, Chr_SmallerEqual, Tok_SmallerEqual, tkSymbol);
  MapDualSymbol(LexNone, Chr_Equality, Tok_Equality, tkSymbol);
  MapDualSymbol(LexNone, Chr_InEquality, Tok_InEquality, tkSymbol);
  MapDualSymbol(LexNone, Chr_NotEqual, Tok_NotEqual, tkSymbol);

  // # Hash Macros
{$ifdef Use_AtSign_Macro}
  MapSymbol(LexMacro, Chr_AtSign);   // Keep this here for backward compatiblity
{$endIf}

  MapSymbol(LexSymbol, Chr_CloseCurlyBracket, Tok_CloseCurlyBracket, tkSymbol, [tsMacro, tsMask]);
  MapDualSymbol(LexNone, Chr_OpenCurlyBracket_Hash, Tok_OpenCurlyBracket_Hash, tkSymbol, [tsMacro]);

  // State: tsComment2 - /*...*/ style comments
  MapDualSymbol(LexNone, Chr_Slash_Star, Tok_Slash_Star, tkSymbol, [tsComment3]);
  MapDualSymbol(LexNone, Chr_Star_Slash, Tok_Star_Slash, tkSymbol, [tsComment3, tsMask]);

  // Provide a way to specify an Escape for the '$' and TAB characters
  MapDualSymbol(LexEscape, Chr_EscapeDollar, Tok_EscapeDollar);
  MapDualSymbol(LexEscape, Chr_EscapeTab, Tok_EscapeTab);

end;






end.
