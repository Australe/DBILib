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

  Description
  Boyer-Moore-Horspool search algorithm

  Change History:
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 06/04/2005 20:50:32 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : native api code}

unit DBITextSearchers;

interface

uses
  SysUtils;

type
  TDBIIndexOfFunc = function: Integer of object;

  TDBITextSearch = class(TObject)
  private
    FCaseSensitive: Boolean;
    FJumpTable: array[AnsiChar] of Integer;
    FPatternLength: Integer;
    FPattern: AnsiString;
    FPatternLC: AnsiString;
    FText: AnsiString;
    PTextBuf: PAnsiChar;
    PTextEnd: PAnsiChar;
    PPatternBuf: PAnsiChar;
    FIndexOfFunc: TDBIIndexOfFunc;

    class procedure InitialiseLowerCaseMap;

  protected
    function GetPattern: String;
    function GetText: String;

    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetJumpTable;
    procedure SetPattern(const Value: String);
    procedure SetText(const Value: String);

    function IndexOfCaseSensitive: Integer;
    function IndexOfCaseInsensitive: Integer;

  public
    constructor Create(const AText: String = '');

    function Find(
      const ASearchPattern: String = '';
      const ASearchText: String = ''
      ): Boolean;

    property IndexOf: TDBIIndexOfFunc read FIndexOfFunc;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property Text: String read GetText write SetText;
    property Pattern: String read GetPattern write SetPattern;
  end;



function Horspool(
  PPattern: PAnsiChar;
  PatternLength: Integer;
  PText: PAnsiChar;
  TextLength: Integer
  ): Integer;



implementation

uses
  DBIStrings;

const
  LowerChrMask = $20;

var
  LowerCaseMap: array[AnsiChar] of AnsiChar;


// _____________________________________________________________________________
{**
  Boyer-Moor-Horspool Search algorithm - Case Sensitive.

  Jvr - 27/09/2005 17:07:08 - Initial code.<br>
}
function Horspool(
  PPattern: PAnsiChar;
  PatternLength: Integer;
  PText: PAnsiChar;
  TextLength: Integer
  ): Integer;
var
   TextIndex: Integer;
   JumpTable: array[AnsiChar] of Integer;
   CharCode: AnsiChar;

  procedure PrepareJumpTable(PPattern: PAnsiChar; PatternLength: Integer);
  var
    CharIndex: AnsiChar;
    PatternIndex: Integer;

  begin
     for CharIndex := Low(JumpTable) to High(JumpTable) do
       JumpTable[CharIndex] := PatternLength;

     for PatternIndex := 0 to PatternLength-2 do
       JumpTable[PPattern[PatternIndex]] := PatternLength - PatternIndex - 1;
  end;


begin
  Result := 0;

  // Preprocessing
  PrepareJumpTable(PPattern, PatternLength);

  // Searching
  TextIndex := 0;
  while TextIndex <= (TextLength - PatternLength) do begin
    CharCode := PText[TextIndex + PatternLength - 1];
    if (PPattern[PatternLength - 1] = CharCode) and CompareMem(PPattern, PText + TextIndex, PatternLength - 1) then begin
       Result := TextIndex;
       Break;
    end;

    Inc(TextIndex, JumpTable[CharCode]);
  end;
end;  { Horspool }





// =============================================================================
// 'TDBITextSearch' implementation
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 06/04/2005 13:58:21 - Updated code.<br>
}
constructor TDBITextSearch.Create(const AText: String = '');
begin
  inherited Create;

  FPatternLength := 0;
  FCaseSensitive := False;
  FIndexOfFunc := IndexOfCaseInsensitive;

  if (AText <> '') then begin
    Text := AText;
  end;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 06/04/2005 21:14:14 - Update code.<br>
}
procedure TDBITextSearch.SetPattern(const Value: String);
begin
  FPattern := AnsiString(Value);
  FPatternLength := Length(FPattern);

  SetJumpTable;
end;  { SetPattern }


// _____________________________________________________________________________
{**
  Jvr - 11/04/2005 18:37:50 - Initial code.<br>
}
procedure TDBITextSearch.SetJumpTable;
var
  CharIndex: AnsiChar;
  PatternIndex: integer;

begin
  if (FPatternLength = 0) then Exit;

  if FCaseSensitive then begin
    PPatternBuf := PAnsiChar(FPattern);
  end
  else begin
    FPatternLC := DBILowerCase(FPattern);
    PPatternBuf := PAnsiChar(FPatternLC);
  end;

  for CharIndex := Low(FJumpTable) to High(FJumpTable) do begin
    FJumpTable[CharIndex] := FPatternLength;
  end;

  for PatternIndex := 0 to FPatternLength-2 do begin
    FJumpTable[PPatternBuf[PatternIndex]] := FPatternLength-PatternIndex-1;
  end;
end;  { SetJumpTable }


// _____________________________________________________________________________
{**
  Jvr - 11/04/2005 18:00:20 - Initial code.<br>
}
function TDBITextSearch.IndexOfCaseSensitive: Integer;
var
  PatternIndex: Integer;
  PTextPtr: PAnsiChar;
  PPatternPtr: PAnsiChar;

begin
  Result:= 0;

  // Starts at the end of the pattern
  PPatternPtr := PPatternBuf;
  Inc(PPatternPtr, FPatternLength-1);
  PTextPtr := PTextBuf;
  Inc(PTextPtr, FPatternLength-1);

  while PTextPtr < PTextEnd do begin
    // Test last character first
    // if Last AnsiChar does not match
    if (PTextPtr^ <> PPatternPtr^) then begin //##JVR PPatternBuf[FPatternLength-1]) then begin
      Inc(PTextPtr, FJumpTable[PTextPtr^]);
    end

    // Last AnsiChar matches at least
    // So check whether the previous characters match as well
    else begin
      PatternIndex := FPatternLength - 1;

      while (PatternIndex > 0)
      and ((PPatternPtr-PatternIndex)^ = (PTextPtr-PatternIndex)^) do
        Dec(PatternIndex);

      // Found a difference in one of the characters
      if (PatternIndex) <> 0 then begin
        Inc(PTextPtr, FJumpTable[PTextPtr^]); //##JVR FPatternLength);
      end

      // All characters match so return the index of the first character
      else begin
        Result := PTextPtr - PTextBuf - FPatternLength + 2;
        Exit;
      end;
    end;  { if }
  end;  { while }
end;  { IndexOfCaseSensitive }


// _____________________________________________________________________________
{**
  Returns the index in the found pattern, 0 means Pattern not found
  Can repeat the search: searches from the previous location (not implemented).

  Jvr - 06/04/2005 21:08:42 - Updated code.<br>
  Jvr - 15/11/2005 11:04:25 - Fixed last AnsiChar case insensitive compare.<br>
}
function TDBITextSearch.IndexOfCaseInsensitive: Integer;
var
  PatternIndex: Integer;
  PTextPtr: PAnsiChar;
  PPatternPtr: PAnsiChar;

begin
  Result:= 0;

  // Starts at the end of the pattern
  PPatternPtr := PPatternBuf;
  Inc(PPatternPtr, FPatternLength-1);
  PTextPtr := PTextBuf;
  Inc(PTextPtr, FPatternLength-1);

  while PTextPtr < PTextEnd do begin
    // Test last character first
    // if Last AnsiChar does not match
    // 15/11/2005 11:04:25 - This needs to do case insensitive compare
    if (LowerCaseMap[PTextPtr^] <> PPatternPtr^) then begin
//##JVR    if (PTextPtr^ <> PPatternPtr^) then begin //##JVR PPatternBuf[FPatternLength-1]) then begin
      Inc(PTextPtr, FJumpTable[LowerCaseMap[PTextPtr^]]);
    end

    // Last AnsiChar matches at least
    // So check whether the previous characters match as well
    else begin
      PatternIndex := FPatternLength - 1;

      while (PatternIndex > 0)
      and ((PPatternPtr-PatternIndex)^ = LowerCaseMap[(PTextPtr-PatternIndex)^]) do
        Dec(PatternIndex);

      // Found a difference in one of the characters
      if (PatternIndex) <> 0 then begin
        Inc(PTextPtr, FJumpTable[LowerCaseMap[PTextPtr^]]); //##JVRFPatternLength);
      end

      // All characters match so return the index of the first character
      else begin
        Result := PTextPtr - PTextBuf - FPatternLength + 2;
        Exit;
      end;
    end;  { if }
  end;  { while }
end;  { IndexOfCaseInsensitive }


// _____________________________________________________________________________
{**
  Jvr - 06/04/2005 13:24:58 - Updated code.<br>
}
function TDBITextSearch.Find(
  const ASearchPattern: String = '';
  const ASearchText: String = ''
  ): Boolean;
begin
  if (ASearchText <> '') then
    Text := ASearchText;

  if (ASearchPattern <> '') then
    Pattern := ASearchPattern;

  Result := IndexOf > 0;
end;  { Find }


function TDBITextSearch.GetPattern: String;
begin
  Result := String(FPattern);
end;


function TDBITextSearch.GetText: String;
begin
  Result := String(FText);
end;


// _____________________________________________________________________________
{**
  Jvr - 11/04/2005 10:57:44 - Initial code.<br>
}
procedure TDBITextSearch.SetText(const Value: String);
begin
  FText := AnsiString(Trim(Value));

  PTextBuf := PAnsiChar(FText);
  PTextEnd := PTextBuf;
  Inc(PTextEnd, Length(FText));
end;


// _____________________________________________________________________________
{**
  Jvr - 11/04/2005 18:25:10 - Initial code.<br>
}
procedure TDBITextSearch.SetCaseSensitive(const Value: Boolean);
begin
  if (FCaseSensitive <> Value) then begin
    FCaseSensitive := Value;

    if FCaseSensitive then begin
      FIndexOfFunc := IndexOfCaseSensitive;
    end
    else begin
      FIndexOfFunc := IndexOfCaseInsensitive;
    end;

    SetJumpTable;
  end;
end;  { SetCaseSensitive }


// _____________________________________________________________________________
{**
  Jvr - 11/04/2005 18:09:19 - Initial code.<br>
}
class procedure TDBITextSearch.InitialiseLowerCaseMap;
var
  Index: AnsiChar;

begin
  // Create CaseInsensitive Map
  for Index := Low(LowerCaseMap) to High(LowerCaseMap) do begin
    if (Index < 'A') or (Index > 'Z') then begin
      LowerCaseMap[Index] := Index;
    end
    else begin
      LowerCaseMap[Index] := AnsiChar(Byte(Index) or LowerChrMask);
    end;
  end;
end;  { InitialiseLowerCaseMap }


initialization
  TDBITextSearch.InitialiseLowerCaseMap;

end.
