// _____________________________________________________________________________
{
  Copyright (C) 1996-2015, All rights reserved, John Vander Reest

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
  1.0 | 20/06/1999 12:00:00 | Jmb | Julian M Bucknall Delphi Magazine #46 + #60
  1.1 | 24/10/2007 15:18:01 | Jvr | Added User Functions
  1.2 | 04/06/2014 09:23:11 | Jvr | Code Cleanup
  ______________________________________________________________________________
}

{#omcodecop off : jvr : DBILib}

unit DBIExpressionEvaluators;

interface

uses
  Classes, SysUtils;

type
  PDBIShortString = ^TDBIShortString;
  TDBIShortString = String[255];

type
  TExambleList = class(TList);

type
  // Expression token types
  TDBIExprTokenType = (
    ttOperator,              // An Operator}
    ttNumOperand,            // A numeric operand}
    ttVarOperand,            // An Operand that's a variable
    ttEndOfExpr              // End of the Expression
    );

type
  // Possible parser states
  TDBIExprParserState = (
    psCannotBeOperand,       // Next token cannot be an operand
    psCouldBeOperand,        // Next token could be an operand
    psMustBeOperand          // Next token must an operand or '('
    );


type
  TDBICustomStack = class(TObject)
  private
    FStack: PAnsiChar;
    FStackPointer: Integer;
    FSize: Integer;

  protected
    procedure CheckEmpty;
    function GetCount: Integer; virtual;

  public
    procedure Clear; virtual;

    destructor Destroy; override;

    property Count: Integer read GetCount;

  end;


  TDBICharacterStack = class(TDBICustomStack)
  protected
    procedure SetCapacity(const NewCapacity: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    function Peek: AnsiChar;
    function IsEmpty: Boolean;
    function Pop: AnsiChar;
    procedure Push(Value: AnsiChar);

  end;


type
  TDBIFloatStack = class(TDBICustomStack)
  protected
    procedure SetCapacity(const NewCapacity: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    function IsEmpty: Boolean;
    function Peek: Double;
    function Pop: Double;
    procedure Push(Value: Double);

  end;


type
  TDBIStringStack = class
  private
    FPageSize: Integer;
    FPage: PAnsiChar;
    FCount: Integer;
    FCurString: PAnsiChar;

  protected
    procedure CheckEmpty;
    procedure Grow;

    function GetCount: Integer;
    function GetFreeSpace: Integer;
    function GetPageCount: Integer;

    property FreeSpace: Integer read GetFreeSpace;
    property PageCount: Integer read GetPageCount;
    property PageSize: Integer read FPageSize;

  public
    constructor Create(APageSize: Integer);
    destructor Destroy; override;

    procedure Clear;
    function Peek: AnsiString;
    function IsEmpty: Boolean;
    function Pop: AnsiString;
    function Push(const Value: AnsiString): PDBIShortString;

    property Count: Integer read GetCount;

  end;



type
  PDBIVariableNode = ^TDBIVariableNode;
  TDBIVariableNode = packed record
    PName: PDBIShortString;
    Value: Double;
  end;

const
  DBIMaxVarItems = MaxInt div SizeOf(TDBIVariableNode);

type
  PDBIVariableArray = ^TDBIVariableArray;
  TDBIVariableArray = array [0..Pred(DBIMaxVarItems)] of TDBIVariableNode;

type
  TDBIVariableList = class
  private
    FArray: PDBIVariableArray;
    FCapacity: Integer;
    FCount: Integer;
    FVariableStack: TDBIStringStack;

  protected
    function GetName(const Index: Integer): AnsiString;
    function GetVariable(const Index: AnsiString): Double;
    function FindName(const AName: AnsiString; var Index: Integer): Boolean;
    procedure SetCapacity(const NewCapacity: Integer);
    procedure SetVariable(const Index: AnsiString; const Value: Double);

    procedure Grow;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function GetValue(const AName: AnsiString; var Value: Double): Boolean;
    function IsEmpty : Boolean;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
    property Name[const Index: Integer]: AnsiString read GetName;

    // Associative array of variable names and their values,
    // on read, if the variable does not exist, 0.0 is returned
    property Value[const AName: AnsiString]: Double read GetVariable write SetVariable;

  end;



type
  TDBIExpressionParser = class
  private
    FExpr: PAnsiChar;
    FOrigExpression: AnsiString;
    FParsed: Boolean;
    FOperandStack: TDBIStringStack;
    FOperatorStack: TDBICharacterStack;
    FVarList: TDBIVariableList;

  protected
    procedure CheckBadParserState(
      aState: TDBIExprParserState;
      aBadState: TDBIExprParserState;
      aCharPos: PAnsiChar
      );

    procedure FindEndOfNumber;
    procedure FindEndOfIdentifier;
    procedure FormRPNSubExpr(aOp: AnsiChar; aCharPos: PAnsiChar);

    function GetExpression: String;
    function GetFunctionOperator(const AFunction: String): AnsiChar;
    function GetPrecedence(AOperator: AnsiChar): Integer;
    function GetRPNExpression: String;
    function GetValue: Double;
    function GetVariable(const AName: AnsiString): Double;

    function NextToken(var aStartToken: PAnsiChar): TDBIExprTokenType;
    procedure ParseToRPN;
    procedure PushNewFunction(aStartPos: PAnsiChar);
    procedure PushNewOperand(aStartPos: PAnsiChar);

    procedure RaiseBadExpressionError(aPosn: PAnsiChar);
    procedure SetExpression(const Value: String); virtual;
    procedure SetVariable(const AName: AnsiString; Value: Double);
    procedure SkipBlanks;

  public
    constructor Create(const AExpression: String); virtual;
    destructor Destroy; override;

    {$IFOPT D+}
    procedure TokenPrint;
    {$ENDIF}

    property Expression: String read GetExpression write SetExpression;
    property RPNExpression: String read GetRPNExpression;
    property Variable[const AName: AnsiString]: Double read GetVariable write SetVariable;

    property Result: Double read GetValue;
  end;

type
  TDBIExpressionEvaluator = class(TDBIExpressionParser);


implementation

uses
  TypInfo;

type
  TDBIFunctionOperator = (
    fnFrac,
    fnInt,
    fnRound,
    fnTrunc
    );
  TDBIFunctionOperators = set of TDBIFunctionOperator;

const
  FunctionOperators = [Low(TDBIFunctionOperator)..High(TDBIFunctionOperator)];

const
  UnaryMinus = AnsiChar(Ord('-') or $80);

const
  OperatorSet = ['(', ')', '^', '*', '/', '+', '-', '%'];
  NumberSet = ['0'..'9', '.'];
  IdentifierSet = ['A'..'Z', 'a'..'z', '0'..'9', '_'];


function Power(X, Y: Double): Double;
begin
  if (Y = 0.0) then begin
    Result := 1.0;
  end
  else if (Y = 1.0) then begin
    Result := X;
  end
  else begin
    Result := Exp(ln(X) * Y);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/10/2007 13:26:03 - Initial code.<br>
}
function IsWhiteSpace(Ch: AnsiChar): Boolean;
begin
  Result := (Ch > #0) and (Ch <= ' ');
end;




{ TDBIExpressionParser }

procedure TDBIExpressionParser.CheckBadParserState(
  aState: TDBIExprParserState; aBadState: TDBIExprParserState; aCharPos: PAnsiChar
  );
begin
  if (aState = aBadState) then begin
    RaiseBadExpressionError(aCharPos);
  end;
end;


constructor TDBIExpressionParser.Create(const AExpression: String);
begin
  inherited Create;

  // Create a String stack for the operands
  FOperandStack := TDBIStringStack.Create(4096);

  // Create an operator stack
  FOperatorStack := TDBICharacterStack.Create;

  // Create a variable list
  FVarList := TDBIVariableList.Create;

  // Set the expression String
  Expression := AExpression;
end;


destructor TDBIExpressionParser.Destroy;
begin
  FreeAndNil(FOperandStack);
  FreeAndNil(FOperatorStack);
  FreeAndNil(FVarList);

  inherited Destroy;
end;


procedure TDBIExpressionParser.FindEndOfIdentifier;
var
  PTempExpr: PAnsiChar;

begin
  // Assume that FExpr is an alphanum AnsiChar, find the end of the stream
  // of alphanum Chars
  PTempExpr := FExpr;
  while (PTempExpr^ in IdentifierSet) do begin
    Inc(PTempExpr);
  end;

  FExpr := PTempExpr;
end;


procedure TDBIExpressionParser.FindEndOfNumber;
var
  PTempExpr: PAnsiChar;

begin
  // Assume that FExpr is a digit, find the end of the stream of digits
  PTempExpr := FExpr;
  while (PTempExpr^ in NumberSet) do
    Inc(PTempExpr);
  FExpr := PTempExpr;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 18:21:16 - Initial code.<br>

  This routine is called when the operator about to be pushed, aOp,
  has a precedence lower than or equal to the operator on top of the
  operator stack. We need to pop off some operators and operands and
  form some RPN expressions to push onto the operand stack, until the
  operator stack is exhausted or the top operator has a precedence
  value less than the given operator's precedence value.
}
procedure TDBIExpressionParser.FormRPNSubExpr(aOp: AnsiChar; aCharPos: PAnsiChar);
var
  PrecOp: Integer;
  PrecTop: Integer;
  TempOp: AnsiChar;
  Operand1: TDBIShortString;
  Operand2: TDBIShortString;

begin
  PrecOp := GetPrecedence(aOp);
  PrecTop := GetPrecedence(FOperatorStack.Peek);
  while (PrecOp <= PrecTop) and (PrecTop > 1) do begin
    TempOp := FOperatorStack.Pop;
    if (TempOp = UnaryMinus) then begin
      if (FOperandStack.Count = 0) then begin
        RaiseBadExpressionError(aCharPos);
      end;
      
      Operand1 := FOperandStack.Pop + UnaryMinus;
      FOperandStack.Push(Operand1);
    end

    else if (TDBIFunctionOperator(Ord(TempOp) and $7F) in FunctionOperators) then begin
      if (FOperandStack.Count = 0) then begin
        RaiseBadExpressionError(aCharPos);
      end;

      Operand1 := FOperandStack.Pop + TempOp;
      FOperandStack.Push(Operand1);
    end

    else begin
      if (FOperandStack.Count < 2) then begin
        RaiseBadExpressionError(aCharPos);
      end;
      
      Operand2 := FOperandStack.Pop;
      Operand1 := FOperandStack.Pop + Operand2 + TempOp;
      FOperandStack.Push(Operand1);
    end;

    if FOperatorStack.IsEmpty then begin
      PrecOp := 0;
    end
    else begin
      PrecTop := GetPrecedence(FOperatorStack.Peek);
    end;
  end;

  // If the given operator was a right parenthesis the top of the
  // operator stack *must* be a left parenthesis and we should remove it
  if (aOp = ')') then begin
    if FOperatorStack.IsEmpty or (FOperatorStack.Peek <> '(') then begin
      RaiseBadExpressionError(aCharPos);
    end;
    
    FOperatorStack.Pop;
  end;
end;


function TDBIExpressionParser.GetExpression: String;
begin
  Result := String(FOrigExpression);
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 17:08:10 - Initial code.<br>
}
function TDBIExpressionParser.GetFunctionOperator(const AFunction: String): AnsiChar;
var
  Index: TDBIFunctionOperator;

begin
  Result := AnsiChar(0);

  for Index := Low(TDBIFunctionOperator) to High(TDBIFunctionOperator) do begin
    if CompareText(Trim(AFunction), Copy(TypInfo.GetEnumName(TypeInfo(TDBIFunctionOperator), Ord(Index)), 3, 255)) = 0 then begin
      Result := AnsiChar(Byte(Index) or $80);
      Break;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 17:28:31 - Updated code.<br>
}
function TDBIExpressionParser.GetPrecedence(AOperator: AnsiChar): Integer;
const
  Operators : String[8] = '()^*/+-' + UnaryMinus;
  Precedences : array [1..8] of byte = (1,1,7,5,5,3,3,9);
  UnaryPrecedence = 9;

var
  Offset: Integer;

begin
  if (TDBIFunctionOperator(Ord(AOperator) and $7F) in FunctionOperators) then begin
    Result := UnaryPrecedence;
  end
  else begin
    Offset := Pos(AOperator, Operators);
    Result := Precedences[Offset];
  end;
end;


function TDBIExpressionParser.GetRPNExpression: String;
begin
  if not FParsed then begin
    ParseToRPN;
  end;
  Result := String(FOperandStack.Peek);
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 15:40:05 - Updated code.<br>
}
function TDBIExpressionParser.GetValue: Double;
var
  DblStack: TDBIFloatStack;
  Index: Integer;
  Operand1: Double;
  Operand2: Double;
  Expr: AnsiString;
  OperandSt: AnsiString;

  function UserFunction(FunctionOperator: TDBIFunctionOperator): Boolean;
  begin
    Result := FunctionOperator in FunctionOperators;
    if Result then begin
      case FunctionOperator of
        fnFrac: DblStack.Push(Frac(DblStack.Pop));
        fnInt: DblStack.Push(Int(DblStack.Pop));
        fnRound: DblStack.Push(Round(DblStack.Pop));
        fnTrunc: DblStack.Push(Trunc(DblStack.Pop));
      end;
    end;
  end;


  procedure PerformCalculation;
  begin
    // Unary Minus operator !
    if Expr[Index] = UnaryMinus then begin
      DblStack.Push(-DblStack.Pop);
    end

    // Standard two value operators
    else if not UserFunction(TDBIFunctionOperator(Ord(Expr[Index]) and $7F)) then begin
      Operand2 := DblStack.Pop;
      Operand1 := DblStack.Pop;

      case Expr[Index] of
        '+' : DblStack.Push(Operand1 + Operand2);
        '-' : DblStack.Push(Operand1 - Operand2);
        '*' : DblStack.Push(Operand1 * Operand2);
        '/' : DblStack.Push(Operand1 / Operand2);
        '^' : DblStack.Push(Power(Operand1, Operand2));

        //##JVR Added - 24/10/2007 15:31:00
        '%' : DblStack.Push(Round(Operand1) mod Round(Operand2));
      end;
    end;
  end;


begin
  if not FParsed then begin
    ParseToRPN;
  end;

  // Prepare a stack for doubles
  DblStack := TDBIFloatStack.Create;
  try
    // Read through the RPN expression and evaluate it
    Expr := FOperandStack.Peek;
    Index := 0;
    while (Index < Length(Expr)) do begin
      Inc(Index);

      if IsWhiteSpace(Expr[Index]) then begin
        if Expr[Index+1] in NumberSet then begin
          OperandSt := '';
          while (Index < Length(Expr)) and (Expr[Index+1] in NumberSet) do begin
            OperandSt := OperandSt + Expr[Index+1];
            Inc(Index);
          end;
          DblStack.Push(StrToFloat(String(OperandSt)));
        end
        else begin
          OperandSt := '';
          while (Index < Length(Expr)) and (Expr[Index+1] in IdentifierSet) do begin
            OperandSt := OperandSt + Expr[Index+1];
            Inc(Index);
          end;
          DblStack.Push(FVarList.Value[OperandSt]);
        end
      end
      else begin
        PerformCalculation;
      end;
    end;
    Result := DblStack.Pop;
  finally
    DblStack.Free;
  end;
end;


function TDBIExpressionParser.GetVariable(const AName: AnsiString): Double;
begin
  Result := FVarList.Value[AName];
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 15:46:32 - Updated code.<br>
}
function TDBIExpressionParser.NextToken(var aStartToken: PAnsiChar): TDBIExprTokenType;
var
  CurChar: AnsiChar;

begin
  SkipBlanks;
  aStartToken := FExpr;
  CurChar := aStartToken^;

  if (CurChar = #0) then begin
    Result := ttEndOfExpr;
  end
  else if (CurChar in OperatorSet) then begin
    Inc(FExpr); {operators are always one Character in size}
    Result := ttOperator;
  end
  else if (CurChar in NumberSet) then begin
    FindEndOfNumber;
    Result := ttNumOperand;
  end
  else if (CurChar in IdentifierSet) then begin
    FindEndOfIdentifier;
    Result := ttVarOperand;
  end
  else begin
    Result := ttEndOfExpr;
    RaiseBadExpressionError(aStartToken);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 16:12:43 - Initial code.<br>
}
procedure TDBIExpressionParser.ParseToRPN;
var
  ParserState: TDBIExprParserState;
  TokenType: TDBIExprTokenType;
  Op: AnsiChar;
  StartPos: PAnsiChar;
  PrecOp: Integer;
  PrecTop: Integer;
  
begin
  // If we've done this already, get out
  if FParsed then begin
    Exit;
  end;

  // Initialise the operator stack to have a left parenthesis; when we
  // reach the end of the expression we'll be pretending it has a right
  // parenthesis
  FOperatorStack.Clear;
  FOperatorStack.Push('(');

  // Initialise the operand stack
  FOperandStack.Clear;

  // Initialise the parser
  FExpr := PAnsiChar(FOrigExpression);
  ParserState := psCouldBeOperand;

  // Get the next token from the expression
  TokenType := NextToken(StartPos);

  // Process all the tokens
  while (TokenType <> ttEndOfExpr) do begin

    // What type of token are we trying to parse?
    case TokenType of
      ttOperator :
        begin
          // It's an operator
          Op := StartPos^;

          // If the operator is a left parenthesis, just push it onto
          // the operator stack}
          if (Op = '(') then begin
            FOperatorStack.Push(Op);
            ParserState := psCouldBeOperand;
          end
          else begin
            CheckBadParserState(ParserState, psMustBeOperand, StartPos);

            // If the operator is a right parenthesis, start popping off
            // operators and operands and forming RPN subexpressions,
            // until we reach a left parenthesis
            if (Op = ')') then begin
              if FOperatorStack.IsEmpty then
                RaiseBadExpressionError(StartPos);
              FormRPNSubExpr(')', StartPos);
              ParserState := psCannotBeOperand;
            end

            // If the operator is a unary operator, then ignore a unary
            // plus (it has no effect) and push a unary minus
            else if (ParserState = psCouldBeOperand) then begin
              if (Op <> '+') and (Op <> '-') then begin
                RaiseBadExpressionError(StartPos);
              end;

              if (Op = '-') then begin
                FOperatorStack.Push(UnaryMinus);
              end;

              ParserState := psMustBeOperand;
            end

            // If we reach this point, the operator must be pushed onto
            // the stack, however, we first need to check that we are
            // not pushing it onto an operator of greater or equal
            // precedence
            else begin
              PrecOp := GetPrecedence(Op);
              if FOperatorStack.IsEmpty then begin
                PrecTop := 0;
              end
              else begin
                PrecTop := GetPrecedence(FOperatorStack.Peek);
              end;

              if (PrecOp <= PrecTop) then begin
                FormRPNSubExpr(Op, StartPos);
              end;
              FOperatorStack.Push(Op);
              ParserState := psCouldBeOperand;
            end;
          end;
        end;

      ttNumOperand:
        begin
          // It's an operand
          CheckBadParserState(ParserState, psCannotBeOperand, StartPos);
          PushNewOperand(StartPos);
          ParserState := psCannotBeOperand;
        end;

      ttVarOperand :
        begin
          // If the operator is a unary operator, then ignore a unary
          // plus (it has no effect) and push a unary minus
          if (ParserState = psCouldBeOperand) then begin
            PushNewFunction(StartPos);
            ParserState := psMustBeOperand;
          end

          // It's an operand
          else begin
            CheckBadParserState(ParserState, psCannotBeOperand, StartPos);
            PushNewOperand(StartPos);
            ParserState := psCannotBeOperand;
          end;
        end;

    end;

    // Get the next token from the expression
    TokenType := NextToken(StartPos);
  end;

  // At the end we pretend that the expression was terminated with a
  // right parenthesis and we can't be expecting an operand
  CheckBadParserState(ParserState, psMustBeOperand, StartPos);
  FormRPNSubExpr(')', StartPos);

  // At this point, the operator stack should be empty and the operand
  // stack should have one item: the RPN of the original expression
  if (not FOperatorStack.IsEmpty) or (FOperandStack.Count <> 1) then begin
    RaiseBadExpressionError(StartPos);
  end;
  
  FParsed := true;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 17:06:31 - Initial code.<br>
}
procedure TDBIExpressionParser.PushNewFunction(aStartPos: PAnsiChar);
var
  TempStr: TDBIShortString;
  Operator: AnsiChar;

begin
  TempStr[0] := AnsiChar(Succ(FExpr - aStartPos));
  TempStr[1] := ' ';
  Move(aStartPos^, TempStr[2], FExpr - aStartPos);

  Operator := GetFunctionOperator(String(TempStr));
  if (Operator > #0) then begin
    FOperatorStack.Push(Operator);
  end
  else begin
    raise Exception.CreateFmt('Invalid function: %s', [TempStr]);
  end;
end;


procedure TDBIExpressionParser.PushNewOperand(aStartPos: PAnsiChar);
var
  TempStr: TDBIShortString;

begin
  TempStr[0] := AnsiChar(Succ(FExpr - aStartPos));
  TempStr[1] := ' ';
  Move(aStartPos^, TempStr[2], FExpr - aStartPos);
  FOperandStack.Push(TempStr);
end;


procedure TDBIExpressionParser.RaiseBadExpressionError(aPosn: PAnsiChar);
begin
  if (aPosn = StrEnd(PAnsiChar(FOrigExpression))) then begin
    raise Exception.Create(
       'Badly formed expression detected at end of String');
  end
  else begin
    raise Exception.Create(
       Format('Badly formed expression with Character [%s], at position %d',
              [aPosn^, succ(aPosn - PAnsiChar(FOrigExpression))]));
  end;
end;


procedure TDBIExpressionParser.SetExpression(const Value: String);
begin
  if (Length(Value) > 255) then begin
    raise Exception.Create('TDBIExpressionParser: the expression is too long');
  end;

  FOrigExpression := AnsiString(Value);

  // The expression is not yet parsed
  FParsed := Value = '';
end;


procedure TDBIExpressionParser.SetVariable(const AName: AnsiString; Value: Double);
begin
  FVarList.Value[AName] := Value
end;




// _____________________________________________________________________________
{**
  Jvr - 25/10/2007 11:10:13 - Updated code.<br>

  Skip all Characters with an ascii value equal to or less than a space (Ctrl-Chars)
}
procedure TDBIExpressionParser.SkipBlanks;
var
  PTempExpr: PAnsiChar;

begin
  // Jump past all the blanks
  PTempExpr := FExpr;

  while IsWhiteSpace(PTempExpr^) do begin
    Inc(PTempExpr);
  end;

  FExpr := PTempExpr;
end;



{$IFOPT D+}
procedure TDBIExpressionParser.TokenPrint;
var
  Index: Integer;
  PStartPos: PAnsiChar;
  TokenType: TDBIExprTokenType;

begin
  FExpr := PAnsiChar(FOrigExpression);
  TokenType := NextToken(PStartPos);

  while TokenType <> ttEndOfExpr do begin
    case TokenType of
      ttOperator:   write('  operator:         ');
      ttNumOperand: write('  number operand:   ');
      ttVarOperand: write('  variable operand: ');
    end;

    for Index := 0 to Pred(FExpr - PStartPos) do begin
      Write((PStartPos + Index)^);
    end;

    WriteLn;
    TokenType := NextToken(PStartPos);
  end;
  
  WriteLn('  end of expression');
end;
{$ENDIF}





{ TDBIVariableList }

constructor TDBIVariableList.Create;
begin
  inherited Create;

  FVariableStack := TDBIStringStack.Create(1024);
end;


destructor TDBIVariableList.Destroy;
begin
  FreeAndNil(FVariableStack);

  Capacity := 0;

  inherited Destroy;
end;


procedure TDBIVariableList.Clear;
begin
  FCount := 0;

  FVariableStack.Clear;
end;


function TDBIVariableList.GetValue(const AName: AnsiString; var Value: Double): Boolean;
var
  ItemIndex: LongInt;

begin
  Result := FindName(AName, ItemIndex);
  if Result then begin
    Value := FArray^[ItemIndex].Value;
  end;
end;


function TDBIVariableList.IsEmpty : Boolean;
begin
  Result := (FCount = 0);
end;


function TDBIVariableList.FindName(const AName: AnsiString; var Index: Integer): Boolean;
var
  L, R, M: LongInt;
  PMidNode: PDBIVariableNode;
  ShortName: TDBIShortString;

begin
  Shortname := AName;

  // Binary search
  L := 0;
  R := Pred(Count);

  while (L <= R) do begin
    M := (L + R) div 2;
    PMidNode := @FArray^[M];
    if (PMidNode^.PName^ < ShortName) then begin
      L := Succ(M);
    end
    else if (PMidNode^.PName^ > ShortName) then begin
      R := Pred(M);
    end
    else {found it} begin
      Index := M;
      Result := True;
      Exit;
    end;
  end;

  Index := L;
  Result := False;
end;


function TDBIVariableList.GetName(const Index: Integer): AnsiString;
begin
  if (Index >= 0) and (Index < Count) then begin
    Result := AnsiString(FArray^[Index].PName^);
  end
end;


function TDBIVariableList.GetVariable(const Index: AnsiString): Double;
begin
  if not GetValue(TDBIShortString(Index), Result) then begin
    Result := 0.0;
  end;
end;


procedure TDBIVariableList.Grow;
begin
  if (Capacity = 0) then begin
    Capacity := 4;
  end
  else if (Capacity < 64) then begin
    Capacity := Capacity * 2;
  end
  else begin
    Capacity := Capacity + (Capacity div 4);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 24/10/2007 19:07:23 - Updated code.<br>
}
procedure TDBIVariableList.SetCapacity(const NewCapacity: Integer);
var
  PNewArray: PDBIVariableArray;
  CopyCount: LongInt;

begin
  PNewArray := nil;

  if (NewCapacity <> FCapacity) then begin
    if (NewCapacity < FCapacity) then begin
      CopyCount := NewCapacity;

      if (NewCapacity < FCount) then begin
        FCount := NewCapacity;
      end;
    end
    else begin
      CopyCount := FCapacity;
    end;

    if (NewCapacity > 0) then begin
      GetMem(PNewArray, LongInt(NewCapacity) * SizeOf(TDBIVariableNode));
    end;

    if (CopyCount > 0) then begin
      Move(FArray^, PNewArray^, CopyCount * SizeOf(TDBIVariableNode));
    end;

    if (FCapacity > 0) then begin
      FreeMem(FArray, LongInt(FCapacity) * SizeOf(TDBIVariableNode));
    end;

    FArray := PNewArray;
    FCapacity := NewCapacity;
  end;
end;  { SetCapacity }


procedure TDBIVariableList.SetVariable(const Index: AnsiString; const Value: Double);
var
  ItemIndex: Integer;
  ShortName: TDBIShortString;

begin
  ShortName := Index;

  // Make sure there's enough room
  if (Count = Capacity) then begin
    Grow;
  end;

  // First the simple case
  if (Count = 0) then begin
    FArray^[0].PName := FVariableStack.Push(ShortName);
    FArray^[0].Value := Value;
    Inc(FCount);
  end

  // Next the case where the name is already present
  else if FindName(ShortName, ItemIndex) then begin
    FArray^[ItemIndex].Value := Value;
  end

  // Finally the case where the name is not present
  else begin
    if (ItemIndex <> Count) then begin
      Move(FArray^[ItemIndex], FArray^[ItemIndex+1], (Count - ItemIndex) * SizeOf(TDBIVariableNode));
    end;

    FArray^[ItemIndex].PName := FVariableStack.Push(ShortName);
    FArray^[ItemIndex].Value := Value;
    Inc(FCount);
  end;
end;






{ TDBIStringStack }

// _____________________________________________________________________________
{**
  Notes:
  
  FCurString acts as the stack Pointer.
  To push a String, FCurString is advanced past the current String,
  and aligned to the nearest 4-byte boundary.
  The routine then checks to see if the String being pushed can be
  added to the remaining space in the Page. If so, it is.
  If not, a new Page is allocated and the new String is added to
  the beginning of that. The position of the new String on the
  stack is returned.
  To pop a String, the current String is returned and FCurString
  is moved back to the previous String. If that is in another
  Page, the Page just vacated is freed.
}

type
  PDBIStackPageHeader = ^TDBIStackPageHeader;
  TDBIStackPageHeader = packed record
    PPrev: PDBIStackPageHeader;
    PLimit: PAnsiChar;
  end;

type
  PDBIStringNode = ^TDBIStringNode;
  TDBIStringNode = packed record
    PPrev: PDBIStringNode;
    Data: TDBIShortString;
  end;

const
  // The minimum for the chunk size will hold a full 256 byte short
  // String, as well as the String node and chunk overhead}
  MinChunkSize = 256 + SizeOf(TDBIStackPageHeader) + SizeOf(Pointer);


procedure TDBIStringStack.CheckEmpty;
begin
  if (FCurString = nil) then begin
    raise Exception.CreateFmt('%s: The stack is empty', [Classname]);
  end;
end;


constructor TDBIStringStack.Create(APageSize: Integer);
begin
  inherited Create;

  if (APageSize < MinChunkSize) then begin
    APageSize := MinChunkSize;
  end;
  
  FPageSize := APageSize;
end;


destructor TDBIStringStack.Destroy;
begin
  Clear;
  
  inherited Destroy;
end;


procedure TDBIStringStack.Clear;
var
  PPage: PDBIStackPageHeader;
  PTemp: PDBIStackPageHeader;
  
begin
  PTemp := PDBIStackPageHeader(FPage);

  while (PTemp <> nil) do begin
    PPage := PTemp^.PPrev;
    FreeMem(PTemp, (PTemp^.PLimit - PAnsiChar(PTemp)));
    PTemp := PPage;
  end;

  FPage := nil;
  FCurString := nil;
  FCount := 0;
end;


function TDBIStringStack.GetCount: Integer;
begin
  Result := FCount;
end;


function TDBIStringStack.GetFreeSpace: Integer;
var
  PTemp: PDBIStackPageHeader;
  PNext: PDBIStackPageHeader;
  PNode: PDBIStringNode;
  PStartFree: PAnsiChar;

begin
  Result := 0;

  if (FPage <> nil) then begin
    PNext := PDBIStackPageHeader(FPage);
    PTemp := PNext^.PPrev;

    while (PTemp <> nil) do begin
      PNode := PDBIStringNode(PAnsiChar(PNext) + SizeOf(TDBIStackPageHeader))^.PPrev;
      PStartFree := PAnsiChar(PNode) + SizeOf(Pointer) + Length(PNode^.Data) + 2;
      Inc(Result, PTemp^.PLimit - PStartFree);
      PNext := PTemp;
      PTemp := PTemp^.PPrev;
    end;
  end;
end;


function TDBIStringStack.GetPageCount: Integer;
var
  PTemp: PDBIStackPageHeader;

begin
  Result := 0;

  PTemp := PDBIStackPageHeader(FPage);
  while (PTemp <> nil) do begin
    Inc(Result);
    PTemp := PTemp^.PPrev;
  end;
end;


procedure TDBIStringStack.Grow;
var
  PNewPage: PAnsiChar;

begin
  GetMem(PNewPage, FPageSize);
  PDBIStackPageHeader(PNewPage)^.PLimit := PNewPage + FPageSize;
  PDBIStackPageHeader(PNewPage)^.PPrev := PDBIStackPageHeader(FPage);
  FPage := PNewPage;
  FCurString := PNewPage + SizeOf(TDBIStackPageHeader);
end;


function TDBIStringStack.IsEmpty : Boolean;
begin
  Result := (FCurString = nil);
end;


function TDBIStringStack.Peek: AnsiString;
begin
  // Check for the obvious mistake
  CheckEmpty;

  // Return the current String
  Result := PDBIStringNode(FCurString)^.Data;
end;


function TDBIStringStack.Pop: AnsiString;
var
  PTemp: PAnsiChar;

begin
  // Check for the obvious mistake
  CheckEmpty;

  // Return the current String
  Result := PDBIStringNode(FCurString)^.Data;

  // Move the current String Pointer back, checking for switching
  // chunks where we need to free the chunk just left
  if (FPage + SizeOf(TDBIStackPageHeader) = FCurString) then begin
    // We're leaving this chunk; set the current String Pointer
    FCurString := PAnsiChar(PDBIStringNode(FCurString)^.PPrev);

    // Reset the chunk address and dispose of the one just left
    PTemp := FPage;
    FPage := PAnsiChar(PDBIStackPageHeader(FPage)^.PPrev);
    FreeMem(PTemp, FPageSize);
  end

  // Otherwise, Just move the current String Pointer back
  else begin
    FCurString := PAnsiChar(PDBIStringNode(FCurString)^.PPrev);
  end;

  Dec(FCount);
end;


function TDBIStringStack.Push(const Value: AnsiString): PDBIShortString;
var
  PPrevNode: PDBIStringNode;
  PNewString: PAnsiChar;
  
begin
  // Save the current String node address
  PPrevNode := PDBIStringNode(FCurString);
  
  // Check for an empty stack
  if (FCurString = nil) then begin
    if (FPage = nil) then begin
      Grow;
    end;
  end
  else begin
    // Advance the current String Pointer
    PNewString := PAnsiChar(PPrevNode) +
      SizeOf(Pointer) +
      Length(PPrevNode^.Data) +
      2;  // The Length byte and the hidden end null

    // Align the new Pointer
    PNewString := Pointer((LongWord(PNewString) + 3) and $FFFFFFFC);

    // If there's not enough room for the new String, get a new chunk
    if (PDBIStackPageHeader(FPage)^.PLimit - PNewString) < (SizeOf(Pointer) + Length(Value) + 2) then
    begin
      Grow;
    end

    // Otherwise, position the current String Pointer
    else begin
      FCurString := PNewString;
    end;
  end;

  // Set up the new node
  with PDBIStringNode(FCurString)^ do begin
    PPrev := PPrevNode;
    Data := Value;
    Data[Length(Value)+1] := #0;
  end;
  
  // Return address of the pushed String
  Result := PDBIShortString(FCurString + SizeOf(Pointer));
  Inc(FCount);
end;





{ TDBIFloatStack }

type
  PDouble = ^Double;

constructor TDBIFloatStack.Create;
begin
  inherited Create;

  FSize := 1024;
  GetMem(FStack, FSize);
  FStackPointer := -1;
end;


destructor TDBIFloatStack.Destroy;
begin
  if (FStack <> nil) then begin
    FreeMem(FStack, FSize);
  end;

  inherited Destroy;
end;


function TDBIFloatStack.IsEmpty : Boolean;
begin
  Result := (FStackPointer = -1);
end;


function TDBIFloatStack.Peek: Double;
begin
  // Check for the obvious mistake
  CheckEmpty;

  // Return the current Character
  Result := PDouble(@FStack[FStackPointer])^;
end;


function TDBIFloatStack.Pop: Double;
begin
  // Check for the obvious mistake
  CheckEmpty;

  // Return the current Character
  Result := PDouble(@FStack[FStackPointer])^;

  // Decrement the stack Pointer
  Dec(FStackPointer, SizeOf(Double));
end;


procedure TDBIFloatStack.Push(Value: Double);
begin
  // Increment the stack Pointer
  Inc(FStackPointer, SizeOf(Double));

  // If we've run out of space, reallocate the stack buffer
  if (FStackPointer >= FSize) then begin
    SetCapacity(FSize + 256);
  end;

  // Store the pushed Character
  PDouble(@FStack[FStackPointer])^ := Value;
end;


procedure TDBIFloatStack.SetCapacity(const NewCapacity: Integer);
var
  PNewStack: PAnsiChar;

begin
  GetMem(PNewStack, NewCapacity);
  Move(FStack^, PNewStack^, FSize);
  FreeMem(FStack, FSize);
  FStack := PNewStack;
  FSize := NewCapacity;
end;





{ TDBICharacterStack }

constructor TDBICharacterStack.Create;
begin
  inherited Create;
  
  FSize := 1024;
  GetMem(FStack, FSize);
  FStackPointer := -1;
end;


destructor TDBICharacterStack.Destroy;
begin
  if (FStack <> nil) then begin
    FreeMem(FStack, FSize);
  end;

  inherited Destroy;
end;


function TDBICharacterStack.Peek: AnsiChar;
begin
  // Check for the obvious mistake
  CheckEmpty;

  // Return the current Character
  Result := FStack[FStackPointer];
end;


function TDBICharacterStack.IsEmpty : Boolean;
begin
  Result := (FStackPointer = -1);
end;


function TDBICharacterStack.Pop: AnsiChar;
begin
  // Check for the obvious mistake
  CheckEmpty;

  // Return the current Character
  Result := FStack[FStackPointer];

  // Decrement the stack Pointer
  Dec(FStackPointer);
end;


procedure TDBICharacterStack.Push(Value: AnsiChar);
begin
  // Increment the stack Pointer
  Inc(FStackPointer);

  // If we've run out of space, reallocate the stack buffer
  if (FStackPointer = FSize) then begin
    SetCapacity(FSize + 256);
  end;

  // Store the pushed Character
  FStack[FStackPointer] := Value;
end;


procedure TDBICharacterStack.SetCapacity(const NewCapacity: Integer);
var
  PNewStack: PAnsiChar;

begin
  GetMem(PNewStack, NewCapacity);
  Move(FStack^, PNewStack^, FSize);

  if (FStack <> nil) then begin
    FreeMem(FStack, FSize);
  end;

  FStack := PNewStack;
  FSize := NewCapacity;
end;





{ TDBICustomStack }

procedure TDBICustomStack.CheckEmpty;
begin
  if (FStackPointer = -1) then begin
    raise Exception.CreateFmt('%s: The stack is empty', [ClassName]);
  end;
end;


procedure TDBICustomStack.Clear;
begin
  FStackPointer := -1;
end;


destructor TDBICustomStack.Destroy;
begin
  inherited Destroy;

  Clear;
end;


function TDBICustomStack.GetCount: Integer;
begin
  Result := Succ(FStackPointer);
end;




end.

