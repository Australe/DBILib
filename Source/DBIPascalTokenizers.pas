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
  1.0 | 26/02/2014 08:58:08 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBIPascalTokenizers;

interface

uses
  Classes, SysUtils, DBIComponents, DBIDialogs, DBIStreamAdapters, DBITokenizers,
  DBITokenizerConsts, omSendMail;

type
 TDBIPascalMacro = (
    macroUnknown,

    // Macro Directives
    macroIfdef,
    macroElse,
    macroEndif
    );

type
  TDBIPascalKeyword = (
    pasUnknown,

    // General
    pasAs,
    pasAt,
    pasDiv,
    pasFor,
    pasHelper,
    pasIndex,
    pasNot,
    pasOf,
    pasObject,
    pasPacked,
    pasReference,
    pasTo,
    pasUnit,
    pasUses,

    // Types
    pasAnsiString,
    pasArray,
    pasBoolean,
    pasDouble,
    pasTDateTime,
    pasInteger,
    pasSet,
    pasString,
    pasUnicodeString,
    pasWideString,

    // Scope Interruptors
    pasCase,
    pasClass,
    pasConst,
    pasEnd,
    pasDispinterface,
    pasFinalization,
    pasProperty,
    pasPrivate,
    pasProtected,
    pasPublic,
    pasPublished,
    pasInterface,
    pasInitialization,
    pasImplementation,
    pasRecord,
    pasResourceString,
    pasStrict,
    pasThreadVar,
    pasType,
    pasVar,

    // Method Kinds / Scope Interruptors Cont.
    pasConstructor,
    pasDestructor,
    pasFunction,
    pasProcedure,
    pasOperator,

    // Method Directives
    pasAbstract,
    pasAssembler,
    pasCdecl,
    pasDeprecated,
    pasDispid,
    pasDynamic,
    pasExternal,
    pasForward,
    pasInline,
    pasMessage,
    pasOverload,
    pasOverride,
    pasReintroduce,
    pasSafecall,
    pasStatic,
    pasStdcall,
    pasVarargs,
    pasVirtual,

    // Property Directives
    pasDefault,
    pasRead,
    pasStored,
    pasWrite,

    // Argument Directives
    pasIn,
    pasOut,

    // Fake
    pasUTF8,

   _pasEnd
  );
  TDBIPascalKeywords = set of TDBIPascalKeyword;

  TDBIPascalMethodKind = pasConstructor..PasOperator;
  TDBIPascalMethodDirective = pasAbstract..pasVirtual;

const
  TDBIPascalMethodKinds = [Low(TDBIPascalMethodKind)..High(TDBIPascalMethodKind)];
  TDBIPascalMethodDirectives = [Low(TDBIPascalMethodDirective)..High(TDBIPascalMethodDirective)];
  TDBIPascalScopeInterruptors = [pasCase..pasOperator];

  TDBIPascalMethodKindName: array[TDBIPascalMethodKind] of String = (
    'constructor',
    'destructor',
    'function',
    'procedure',
    'operator'
    );

const
  Tokens_Comment1 = [Tok_OpenCurlyBracket, Tok_CloseCurlyBracket];
  Tokens_Comment3 = [Tok_OpenBracket_Star, Tok_CloseBracket_Star];
  Tokens_Macro = [Tok_OpenCurlyBracket_Dollar, Tok_CloseCurlyBracket];


type
  // Forward declarations
  TDBIPascalClass = class;
  TDBIPascalComplex = class;
  TDBIPascalInterface = class;
  TDBIPascalRecord = class;


  TDBIPascalNode = class(TDBIPersistentComponent)
  private
    FData: String;
    FDirectives: TDBIPascalKeywords;
    FKindName: String;
    FPosition: TDBITokenPosition;

  protected
    function ChildOwner: TComponent; virtual;
    function GetClassKindName: String; virtual;
    procedure SetClassKindName(const Value: String);

  public
    procedure DirectiveInclude(const ADirective: TDBIPascalKeyword);
    procedure DirectivesInclude(const ADirectives: TDBIPascalKeywords);

    property Caption: String read GetDisplayName;
    property Directives: TDBIPascalKeywords read FDirectives write FDirectives;
    property Position: TDBITokenPosition read FPosition write FPosition;

  published
    property Data: String read FData write FData;
    property DisplayName;
    property KindName: String read FKindName write FKindName;
    property Text;
    property ClassKindName: String read GetClassKindName write SetClassKindName;

  end;


  TDBIPascalScopedItem = class(TDBIPascalNode);

  TDBIPascalArgument = class(TDBIPascalScopedItem);
  TDBIPascalArguments = class(TDBIPascalScopedItem)
    procedure Clone(AScope: TDBIPascalNode);
  end;

  TDBIPascalConst = class(TDBIPascalScopedItem)
  protected
    function GetDisplayName: String; override;
  end;

  TDBIPascalEnum = class(TDBIPascalScopedItem);
  TDBIPascalField = class(TDBIPascalScopedItem);
  TDBIPascalGuid = class(TDBIPascalScopedItem);

  TDBIPascalProcedure = class(TDBIPascalScopedItem)
  protected
    function GetDisplayName: String; override;
  end;

  TDBIPascalMethod = class(TDBIPascalProcedure);
  TDBIPascalProperty = class(TDBIPascalScopedItem);
  TDBIPascalResourceString = class(TDBIPascalScopedItem);

  TDBIPascalType = class(TDBIPascalScopedItem)
  protected
    function GetDisplayName: String; override;
  end;

  TDBIPascalUse = class(TDBIPascalScopedItem);
  TDBIPascalVar = class(TDBIPascalScopedItem);


  TDBIPascalScopeClass = class of TDBIPascalScope;
  TDBIPascalScope = class(TDBIPascalNode)
  private
    FComplex: TDBIPascalComplex;
    FScopeClass: TDBIPascalScopeClass;

  protected
    function GetScope: TDBIPascalScope; overload; virtual;
    function GetScope(var ScopeField: TDBIPascalScope): TDBIPascalScope; overload;

    function GetScopeForClass: TDBIPascalScope; virtual;
    function GetScopeForConst: TDBIPascalScope; virtual;
    function GetScopeForInterface: TDBIPascalScope; virtual;
    function GetScopeForProcedure: TDBIPascalScope; virtual;
    function GetScopeForRecord: TDBIPascalScope; virtual;
    function GetScopeForResourceString: TDBIPascalScope; virtual;
    function GetScopeForType: TDBIPascalScope; virtual;
    function GetScopeForUse: TDBIPascalScope; virtual;
    function GetScopeForVar: TDBIPascalScope; virtual;

    procedure SetScope(const Value: TDBIPascalScopeClass);

  public
    function NewClass(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalClass;
    function NewConst(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalConst;
    function NewField(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalField;
    function NewGuid(const AName: String; APosition: TDBITokenPosition): TDBIPascalGuid;
    function NewInterface(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalInterface;
    function NewMethod(const AName: String; APosition: TDBITokenPosition): TDBIPascalMethod;
    function NewProcedure(const AName: String; APosition: TDBITokenPosition): TDBIPascalProcedure;
    function NewProperty(const AName: String; APosition: TDBITokenPosition): TDBIPascalProperty;
    function NewRecord(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalRecord;
    function NewResourceString(const AName: String; APosition: TDBITokenPosition): TDBIPascalResourceString;
    function NewType(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalType;
    function NewUnit(const AName: String; APosition: TDBITokenPosition): TDBIPascalUse;
    function NewVar(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalVar;

  end;


  // Scope
  TDBIPascalComplexes = class(TDBIPascalScope)
  protected
    procedure Done;
    function GetComplex: TDBIPascalComplex;
    function GetScope: TDBIPascalScope; override;

  end;

  TDBIPascalClasses = class(TDBIPascalComplexes);
  TDBIPascalInterfaces = class(TDBIPascalComplexes);
  TDBIPascalRecords = class(TDBIPascalComplexes);

  TDBIPascalConsts = class(TDBIPascalScope);
  TDBIPascalProcedures = class(TDBIPascalScope);
  TDBIPascalResourceStrings = class(TDBIPascalConsts);
  TDBIPascalTypes = class(TDBIPascalScope);
  TDBIPascalUses = class(TDBIPascalScope);
  TDBIPascalVars = class(TDBIPascalScope);

  TDBIPascalPrivate = class(TDBIPascalScope);
  TDBIPascalProtected = class(TDBIPascalScope);
  TDBIPascalPublic = class(TDBIPascalScope);
  TDBIPascalPublished = class(TDBIPascalScope);


  TDBIPascalUnitGroupingOption = (
    ugoConsts,
    ugoClasses,
    ugoInterfaces,
    ugoProcedures,
    ugoRecords,
    ugoResourceStrings,
    ugoTypes,
    ugoVars
    );
  TDBIPascalUnitGroupingOptions = set of TDBIPascalUnitGroupingOption;

  TDBIPascalUnit = class(TDBIPascalScope)
  private
    FClasses: TDBIPascalClasses;
    FConsts: TDBIPascalConsts;
    FInterfaces: TDBIPascalInterfaces;
    FProcedures: TDBIPascalProcedures;
    FRecords: TDBIPascalRecords;
    FResourceStrings: TDBIPascalResourceStrings;
    FTypes: TDBIPascalTypes;
    FUses: TDBIPascalUses;
    FVars: TDBIPascalVars;

    FOptions: TDBIPascalUnitGroupingOptions;

  protected
    function GetScopeForClass: TDBIPascalScope; override;
    function GetScopeForConst: TDBIPascalScope; override;
    function GetScopeForInterface: TDBIPascalScope; override;
    function GetScopeForProcedure: TDBIPascalScope; override;
    function GetScopeForRecord: TDBIPascalScope; override;
    function GetScopeForResourceString: TDBIPascalScope; override;
    function GetScopeForType: TDBIPascalScope; override;
    function GetScopeForUse: TDBIPascalScope; override;
    function GetScopeForVar: TDBIPascalScope; override;

  public
    property GroupingOptions: TDBIPascalUnitGroupingOptions read FOptions write FOptions;

  end;


  TDBIPascalComplex = class(TDBIPascalScope)
  protected
    function GetDisplayName: String; override;

  end;

  TDBIPascalClass = class(TDBIPascalComplex)
  private
    FPrivate: TDBIPascalScope;
    FProtected: TDBIPascalScope;
    FPublic: TDBIPascalScope;
    FPublished: TDBIPascalScope;

  protected
    function GetScope: TDBIPascalScope; override;

  end;
  TDBIPascalInterface = class(TDBIPascalComplex);
  TDBIPascalRecord = class(TDBIPascalClass);


type
  TDBICustomPascalLexer = class(TDBICustomAsciiLexer)
  private
    FPreviousToken: TDBILexerToken;

  protected
    function GetKeyword: TDBIPascalKeyword;
    function GetMacro: TDBIPascalMacro;
    function GetPreviousToken: TDBILexerToken;
    procedure LexControlCharacter;
    procedure LexHexadecimal;
    procedure LexIdentifier; override;
    procedure LexInitialise; override;
    procedure LexSymbol; overload; override;

    procedure SetStatus(Value: TDBITokenStatus); override;
    procedure UndoToken(const Value: AnsiString);
    procedure UptoToken;

    property PreviousToken: TDBILexerToken read GetPreviousToken;

  public
    function GetConstantValue: String;
    function GetArguments(AScope: TDBIPascalScopedItem): Boolean;
    function GetDimensions: String;
    function GetQuotedString: String;
    function GetTypeSpecifier(AScope: TDBIPascalScopedItem; var ADirectives: TDBIPascalKeywords): String;

    procedure IncludeDirective(var ADirectives: TDBIPascalKeywords; const Value: TDBIPascalKeyword);
    function IsKeywordScopeInterrupter: Boolean;

  public
    procedure Check(const Value: TDBIPascalKeyword); overload;
    function Collate(ATokenKind: TDBITokenKind): String; overload;
    function Fetch(const Value: TDBIPascalKeyword): String; overload;
    function Have(const Value: TDBIPascalKeyword): Boolean; overload;
    function Skip(const Value: TDBIPascalKeyword): String; overload;

    function Upto(const Value: TDBIPascalKeyword): String; overload;
    function Upto(TokenTypes: TDBITokenTypes; const Inclusive: Boolean = False): String; overload;
    function Upto(TokenKinds: TDBITokenKinds; const Inclusive: Boolean = False): String; overload;

    property Keyword: TDBIPascalKeyword read GetKeyword;

    // HACK
    property GetOpenArray: String read GetDimensions;
  end;


type
  TDBIPascalInterfaceLexer = class(TDBICustomPascalLexer)
  public
    procedure NextToken; override;
  end;


type
  TDBIPascalInterfaceAnalyser = class(TDBICustomParser)
  private
    FScope: TDBILexerToken;
    FUnit: TDBIPascalUnit;

  protected
    function GetInput: TDBIPascalInterfaceLexer;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;
    function GetUnit: TDBIPascalUnit; virtual;

  protected
    function ProcessCaseType(AScope: TDBIPascalScope): Boolean;
    function ProcessComplexType(AScope: TDBIPascalScope; const ATypeName: String; AComplexType: TDBIPascalKeyword): Boolean;
    function ProcessComplexTypeKeywords(AScope: TDBIPascalScope): Boolean;
    function ProcessDirectives(const ADirectives: TDBIPascalKeywords; var Value: TDBIPascalKeywords): Boolean;
    function ProcessGuid(AScope: TDBIPascalScope): Boolean;
    function ProcessProperty(AScope: TDBIPascalScope): Boolean;

    function ProcessProcedure(
      AScope: TDBIPascalScope;
      const AProcedureType: TDBIPascalMethodKind;
      const ADirectives: TDBIPascalKeywords
      ): Boolean;

    function ProcessScope(
      AScope: TDBIPascalScope;
      AScopeClass: TDBIPascalScopeClass;
      ScopeDirectives: TDBIPascalKeywords
      ): Boolean;

    function ProcessSimpleType(AScope: TDBIPascalScope; ATypeName: String): Boolean; overload;
    function ProcessSimpleType(AScope: TDBIPascalScope; ATypeNames: TStrings): Boolean; overload;

    function ProcessType(AScope: TDBIPascalScope): Boolean;
    function ProcessUnit(AScope: TDBIPascalScope): Boolean;
    function ProcessUses(AScope: TDBIPascalScope): Boolean;
    function ProcessVariable(AScope: TDBIPascalScope; const AVariableType: TDBIPascalKeyword): Boolean;

    function ProcessKeywords: Boolean;

  public
    destructor Destroy; override;

    procedure Process; virtual;

    property Ast: TDBIPascalUnit read GetUnit;
    property Input: TDBIPascalInterfaceLexer read GetInput;

  end;


type
  ECodeAnalyserError = class(Exception);


implementation

uses
  Windows, Dialogs, Forms, TypInfo, Clipbrd, DBIStrings, DBIUtils;


{ TDBIPascalInterfaceAnalyser }

function TDBIPascalInterfaceAnalyser.GetUnit: TDBIPascalUnit;
begin
  if not Assigned(FUnit) then begin
    FUnit := TDBIPascalUnit.Create(nil, nil);
    FUnit.Text := 'unit';
    FUnit.GroupingOptions := [ugoConsts{, ugoResourceStrings}];
  end;

  Result := FUnit;
end;


procedure TDBIPascalInterfaceAnalyser.Process;
begin
  // BootStrap the lexer;
  Input.Reset;

  while ProcessKeywords and not Input.Eof do;
end;


function TDBIPascalInterfaceAnalyser.ProcessCaseType(AScope: TDBIPascalScope): Boolean;
var
  TypeName: String;
  TypeNames: TStrings;

begin
  Assert(Assigned(AScope));
  Result := True;

  Input.Fetch(pasCase);
  Input.Fetch([Tok_Identifier]);
  Input.Fetch(pasOf);

  while not Input.Eof do begin
    if (Input.Token.TokenType = Tok_CloseBracket) or (Input.keyword = pasEnd) then begin
      Break;
    end;

    repeat
      Input.Fetch([Tok_Identifier, Tok_IntegerLiteral]);
    until not Input.Have([Tok_Comma]);
    Input.Fetch([Tok_Colon]);

    // Is it a multi-part union definition
    if Input.Have([Tok_OpenBracket]) then begin
      while not Input.Eof do begin
        if Input.Have([Tok_CloseBracket]) then begin
          Break;
        end;

        case Input.Keyword of
          pasCase: ProcessCaseType(AScope);

        else
          TypeNames := Local(TStringList.Create).Obj as TStrings;
          repeat
            TypeNames.Add(Input.Fetch([Tok_Identifier]));
          until not Input.Have([Tok_Comma]);
          Input.Fetch([Tok_Colon]);

          Result := ProcessSimpleType(AScope, TypeNames);
        end;

      end;
    end
    else begin
      TypeName := Input.Fetch([Tok_Identifier]);
      ProcessSimpleType(AScope, TypeName);
    end;

    Input.Skip([Tok_SemiColon]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/12/2010 14:26:55 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessComplexType(AScope: TDBIPascalScope; const ATypeName: String; AComplexType: TDBIPascalKeyword): Boolean;
var
  Position: TDBITokenPosition;
  LocalDirectives: TDBIPascalKeywords;
  KindName: String;
  Complex: TDBIPascalComplex;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  LocalDirectives := [];
  Complex := nil;
  Result := True;
  Input.Fetch(AComplexType);

  // Is this an abstract class
  Input.IncludeDirective(LocalDirectives, pasAbstract);

  // Is this a class of type definition?
  if Input.Have(pasOf) then begin
    // What is the class / interface type name
    KindName := ' of ' + Input.Fetch([Tok_Identifier]);
    AComplexType := pasOf;
  end

  // Is this a class helper for type definition
  else if Input.Have(pasHelper) then begin
    Input.Fetch(pasFor);
    KindName := KindName + ' helper for ' + Input.Fetch([Tok_Identifier]);
  end

  // Do we have a parent type?
  else if Input.Have([Tok_OpenBracket]) then begin
    // What is the class / interface type name
    KindName := Input.Upto([Tok_CloseBracket]);
    Input.Fetch([Tok_CloseBracket]);
  end;

  // Create Class representation
  case AComplexType of
    pasClass: Complex := AScope.NewClass(ATypeName, KindName, Position);
    pasDispinterface: Complex := AScope.NewInterface(ATypeName, KindName, Position);
    pasOf: AScope.NewType(ATypeName, KindName, Position);
    pasInterface: Complex := AScope.NewInterface(ATypeName, KindName, Position);
    pasRecord: Complex := AScope.NewRecord(ATypeName, KindName, Position);
  end;

  if not Input.Have([Tok_SemiColon]) then begin
    Result := ProcessComplexTypeKeywords(Complex);
  end;

  // Directives
  if Assigned(Complex) then begin
    Complex.Directives := Complex.Directives + LocalDirectives;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessComplexTypeKeywords(AScope: TDBIPascalScope): Boolean;
var
  LocalDirectives: TDBIPascalKeywords;

begin
  Assert(Assigned(AScope));
  LocalDirectives := [];
  Result := True;

  while not Input.Eof do begin
    case Input.Keyword of
      pasCase: Result := ProcessCaseType(AScope);
      pasConst: Result := ProcessVariable(AScope, pasConst);
      pasConstructor: Result := ProcessProcedure(AScope, pasConstructor, LocalDirectives);
      pasDestructor: Result := ProcessProcedure(AScope, pasDestructor, LocalDirectives);
      pasFunction: Result := ProcessProcedure(AScope, pasFunction, LocalDirectives);
      pasOperator: Result := ProcessProcedure(AScope, pasOperator, LocalDirectives);
      pasProcedure: Result := ProcessProcedure(AScope, pasProcedure, LocalDirectives);
      pasProperty: Result := ProcessProperty(AScope);
      pasPrivate: Result := ProcessScope(AScope, TDBIPascalPrivate, LocalDirectives);
      pasProtected: Result := ProcessScope(AScope, TDBIPascalProtected, LocalDirectives);
      pasPublic: Result := ProcessScope(AScope, TDBIPascalPublic, LocalDirectives);
      pasPublished: Result := ProcessScope(AScope, TDBIPascalPublished, LocalDirectives);
      pasType: Result := ProcessType(AScope);
      pasVar: Result := ProcessVariable(AScope, pasVar);

      pasClass: begin
        Input.Fetch(pasClass);
        Include(LocalDirectives, pasClass);
      end;

      pasStrict: begin
        Input.Fetch(pasStrict);
        Include(LocalDirectives, pasStrict);
      end;

      pasEnd: begin
        Input.Fetch(pasEnd);

        ProcessDirectives([pasDeprecated], LocalDirectives);
        Input.Skip([Tok_SemiColon]);

        if AScope.Parent is TDBIPascalComplexes then begin
         (AScope.Parent as TDBIPascalComplexes).Done;
        end;

        Break;
      end;

    else
      case Input.Token.TokenType of
        Tok_Identifier: Result := ProcessVariable(AScope, pasDefault);
        Tok_OpenSquareBracket: Result := ProcessGuid(AScope);
      else
        Break;
      end;
    end;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessDirectives(
  const ADirectives: TDBIPascalKeywords;
  var Value: TDBIPascalKeywords
  ): Boolean;
var
  Keyword: TDBIPascalKeyword;

begin
  Result := True;

  Keyword := Input.Keyword;
  while (Keyword in ADirectives) do begin
    Include(Value, Keyword);

    Input.NextToken;
    case Keyword of
      pasDeprecated: Input.GetQuotedString;
      pasDispid: Input.Fetch([Tok_IntegerLiteral]);
      pasExternal: Input.GetQuotedString;
      pasMessage: Input.Fetch([Tok_Identifier]);
    end;
    Input.Skip([Tok_SemiColon]);

    Keyword := Input.Keyword;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessGuid(AScope: TDBIPascalScope): Boolean;
begin
  Result := True;
  Input.Fetch([Tok_OpenSquareBracket]);
  AScope.NewGuid(Input.Upto([Tok_CloseSquareBracket]), Input.Token.TokenPosition);
  Input.Fetch([Tok_CloseSquareBracket]);
end;


// _____________________________________________________________________________
{**
  Jvr - 13/12/2010 11:48:32 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessKeywords: Boolean;
var
  UnitScope: TDBIPascalScope;

begin
  Result := True;
  UnitScope := GetUnit;

  case Input.KeyWord of
    pasConst: Result := ProcessVariable(UnitScope, pasConst);
    pasImplementation: Result := False;
    pasInterface: Input.NextToken;
    pasResourceString: Result := ProcessVariable(UnitScope, pasResourceString);
    pasThreadVar: Result := ProcessVariable(UnitScope, pasThreadVar);
    pasType: Result := ProcessType(UnitScope);
    pasUnit: Result := ProcessUnit(UnitScope);
    pasUses: Result := ProcessUses(UnitScope);
    pasVar: Result := ProcessVariable(UnitScope, pasVar);

    pasFunction: Result := ProcessProcedure(UnitScope, pasFunction, []);
    pasProcedure: Result := ProcessProcedure(UnitScope, pasProcedure, []);
  else
    case Input.Token.TokenType of
      Tok_Unassigned: Input.NextToken;

      Tok_Apostrophe: begin
        Input.GetQuotedString;  //##JVR - NOT nice - Used to force compilation errors
      end;

      Tok_UTF8: begin
        Input.Fetch([Tok_UTF8]);
        UnitScope.DirectiveInclude(pasUTF8);
      end;

    else
      Input.Check([Tok_Apostrophe, Tok_Unassigned, Tok_UTF8]);
//##JVR      Input.NextToken;
    end;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessProcedure(
  AScope: TDBIPascalScope;
  const AProcedureType: TDBIPascalMethodKind;
  const ADirectives: TDBIPascalKeywords
  ): Boolean;
var
  Position: TDBITokenPosition;
  LocalDirectives: TDBIPascalKeywords;
  ItemName: String;
  Item: TDBIPascalProcedure;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  LocalDirectives := [];
  Result := True;

  Input.Fetch(AProcedureType);
  Include(LocalDirectives, AProcedureType);
  ItemName := Input.Fetch([Tok_Identifier, Tok_NameSpace]);

  if AScope is TDBIPascalComplex then begin
    Item := AScope.NewMethod(ItemName, Position);
  end
  else begin
    Item := AScope.NewProcedure(ItemName, Position);
  end;

  // Do we have parameters?
  if (Input.Token.TokenType = Tok_OpenBracket) then begin
    Input.GetArguments(Item);
  end;

  // Do we have a return type? or is this interface method assigned to some other method
  if (Input.Token.TokenType in [Tok_Colon, Tok_Equals]) then begin
    Item.KindName := Input.GetTypeSpecifier(nil, LocalDirectives);
  end;
  Input.Fetch([Tok_SemiColon]);

  ProcessDirectives(TDBIPascalMethodDirectives, LocalDirectives);
  Item.Directives := LocalDirectives;
end;


function TDBIPascalInterfaceAnalyser.ProcessProperty(AScope: TDBIPascalScope): Boolean;
var
  Position: TDBITokenPosition;
  LocalDirectives: TDBIPascalKeywords;
  Prop: TDBIPascalProperty;

begin
  LocalDirectives := [];
  Assert(Assigned(AScope));
  Result := True;

  Position := Input.Token.TokenPosition;
  Input.Fetch(pasProperty);
  Prop := AScope.NewProperty(Input.Fetch([Tok_Identifier]), Position);

  // Is this property indexed
  Input.GetDimensions;

  if (Input.Token.TokenType = Tok_Colon) then begin
    Prop.KindName := Input.GetTypeSpecifier(nil, LocalDirectives);
  end;
  Prop.Data := Input.UpTo([Tok_SemiColon]);
  Input.Fetch([Tok_SemiColon]);

  ProcessDirectives([pasDefault, pasStored], LocalDirectives);
  Prop.Directives := LocalDirectives;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/12/2010 13:03:28 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessScope(
  AScope: TDBIPascalScope;
  AScopeClass: TDBIPascalScopeClass;
  ScopeDirectives: TDBIPascalKeywords
  ): Boolean;
begin
  Result := True;
  Input.Fetch([Tok_Identifier]);
  AScope.SetScope(AScopeClass);

  AScope.GetScope.Directives := ScopeDirectives;
  ScopeDirectives := [];
end;


function TDBIPascalInterfaceAnalyser.ProcessSimpleType(AScope: TDBIPascalScope; ATypeName: String): Boolean;
var
  TypeNames: TStrings;

begin
  TypeNames := Local(TStringList.Create).Obj as TStrings;
  TypeNames.Add(ATypeName);

  Result := ProcessSimpleType(AScope, TypeNames);
end;


function TDBIPascalInterfaceAnalyser.ProcessSimpleType(AScope: TDBIPascalScope; ATypeNames: TStrings): Boolean;
var
  Position: TDBITokenPosition;
  LocalDirectives: TDBIPascalKeywords;
  KindName: String;
  Index: Integer;
  SimpleType: TDBIPascalScopedItem;
  Args: TDBIPascalArguments;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  LocalDirectives := [];
  Result := True;

  Args := Local(TDBIPascalArguments.Create(AScope.ChildOwner, AScope)).Obj as TDBIPascalArguments;
  KindName := Input.GetTypeSpecifier(Args, LocalDirectives);
  Input.Skip([Tok_SemiColon]);

  for Index := 0 to ATypeNames.Count-1 do begin
    SimpleType := AScope.NewType(ATypeNames[Index], KindName, Position);
    SimpleType.Directives := LocalDirectives;
    Args.Clone(SimpleType);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/12/2010 10:53:35 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessType(AScope: TDBIPascalScope): Boolean;
var
  TypeName: String;

begin
  Result := True;
  Input.Fetch(pasType);

  while not Input.Eof do begin
    // Check next keyword
    if Input.IsKeywordScopeInterrupter then begin
      Break;
    end;

    TypeName := Input.Fetch([Tok_Identifier]);
    Input.Fetch([Tok_Equals]);

    // Process Packed Directive - Skip it for now
    Input.Skip(pasPacked);

    case Input.Keyword of
      pasClass: Result := ProcessComplexType(AScope, TypeName, pasClass);
      pasConst: Result := ProcessVariable(AScope, pasConst);
      pasDispinterface: Result := ProcessComplexType(AScope, TypeName, pasDispinterface);
      pasFunction: Result := ProcessSimpleType(AScope, TypeName);
      pasInterface: Result := ProcessComplexType(AScope, TypeName, pasInterface);
      pasProcedure: Result := ProcessSimpleType(AScope, TypeName);
      pasRecord: Result := ProcessComplexType(AScope, TypeName, pasRecord);
    else
      Result := ProcessSimpleType(AScope, TypeName);
    end;

  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessUnit(AScope: TDBIPascalScope): Boolean;
begin
  Assert(Assigned(AScope));
  Result := True;
  Input.Fetch(pasUnit);

  GetUnit.Position := Input.Token.TokenPosition;
  GetUnit.Text := Input.Fetch([Tok_NameSpace, Tok_Identifier]);
  Input.Fetch([Tok_SemiColon]);
end;


function TDBIPascalInterfaceAnalyser.ProcessUses(AScope: TDBIPascalScope): Boolean;
var
  Position: TDBITokenPosition;
  UnitName: String;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;
  Input.Fetch(pasUses);

  while not Input.Eof do begin
    UnitName := Input.Fetch([Tok_NameSpace, Tok_Identifier]);
    AScope.NewUnit(UnitName, Position);

    if Input.Have([Tok_SemiColon]) then begin
      Break;
    end
    else begin
      Input.Fetch([Tok_Comma]);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/12/2010 15:47:20 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessVariable(AScope: TDBIPascalScope; const AVariableType: TDBIPascalKeyword): Boolean;
var
  Position: TDBITokenPosition;
  LocalDirectives: TDBIPascalKeywords;
  KindName: String;
  Data: String;
  Item: TDBIPascalScopedItem;
  Items: TStrings;
  Index: Integer;

begin
  Assert(Assigned(AScope));
  Result := True;

  Input.Skip(AVariableType);

  Items := Local(TStringList.Create).Obj as TStrings;
  while not Input.Eof do begin
    Position := Input.Token.TokenPosition;
    LocalDirectives := [];
    KindName := '';
    Data := '';

    // Multiple Names
    Items.Clear;
    repeat
      Items.Add(Input.Fetch([Tok_Identifier]));
    until not Input.Have([Tok_Comma]);

    // Kind
    if (Input.Token.TokenType = Tok_Colon) then begin
      KindName := Input.GetTypeSpecifier(nil, LocalDirectives);
    end;

    // Value
    Data := Input.GetConstantValue;
    Input.Skip([Tok_SemiColon]);

    // Create the Variables
    for Index := 0 to Items.Count-1 do begin
      case AVariableType of
        pasConst: Item := AScope.NewConst(Items[Index], KindName, Position);
        pasDefault: Item := AScope.NewField(Items[Index], KindName, Position);
        pasResourceString: Item := AScope.NewResourceString(Items[Index], Position);
        pasThreadVar: Item := AScope.NewVar(Items[Index], KindName, Position);
        pasVar: Item := AScope.NewVar(Items[Index], KindName, Position);
      else
        Item := nil;
        Input.Check(pasUnknown);
      end;

      Item.Data := Data;
      Item.DirectivesInclude(LocalDirectives);
    end;

    // Check next keyword
    if Input.IsKeywordScopeInterrupter then begin
      Break;
    end;
  end;
end;





{ TPascalInterfaceAnalyser }

destructor TDBIPascalInterfaceAnalyser.Destroy;
begin
  FScope.Free;
  FScope := nil;

  FUnit.Free;
  FUnit := nil;

  inherited Destroy;
end;


function TDBIPascalInterfaceAnalyser.GetInput: TDBIPascalInterfaceLexer;
begin
  Result := inherited Input as TDBIPascalInterfaceLexer;
end;


function TDBIPascalInterfaceAnalyser.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBIPascalInterfaceLexer;
end;





{ TPascalInterfaceLexer }

procedure TDBIPascalInterfaceLexer.NextToken;
begin
  inherited NextToken;

  while not Eof do begin
    // Whitespace
    if (Token.TokenKind = tkWhitespace) and not (tsStringLiteral in Token.TokenStatus) then begin
      inherited NextToken;
    end

    // tsComment1 or leadin
    else if (tsComment1 in Token.TokenStatus) or (Token.TokenType in Tokens_Comment1) then begin
      inherited NextToken;
    end

    // tsComment2 or leadin
    else if (tsComment2 in Token.TokenStatus) or (Token.TokenType = Tok_SlashSlash) then begin
      inherited NextToken;
    end

    // tsComment3 or leadin
    else if (tsComment3 in Token.TokenStatus) or (Token.TokenType in Tokens_Comment3) then begin
      inherited NextToken;
    end

    // Macros
    else if (tsMacro in Token.TokenStatus) or (Token.TokenType in Tokens_Macro) then begin
      inherited NextToken;
    end

    else begin
      Break;
    end;
  end;
end;





{ TCustomPascalLexer }

procedure TDBICustomPascalLexer.Check(const Value: TDBIPascalKeyword);
const
  ErrMsg = 'Unexpected Keyword "%s" at [ %d, %d ], expected "%s"';

begin
  if not (Keyword = Value) then begin
    raise ECodeAnalyserError.CreateFmt(
      ErrMsg,
      [Token.TokenString, Token.Row, Token.Column, GetEnumName(TypeInfo(TDBIPascalKeyword), Ord(Value))]
      );
  end;
end;


function TDBICustomPascalLexer.Collate(ATokenKind: TDBITokenKind): String;
begin
  Result := '';

  while (Token.TokenKind = ATokenKind) do begin
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;


function TDBICustomPascalLexer.Fetch(const Value: TDBIPascalKeyword): String;
begin
  Result := Token.TokenString;
  Check(Value);
  NextToken;
end;


function TDBICustomPascalLexer.GetArguments(AScope: TDBIPascalScopedItem): Boolean;
var
  Position: TDBITokenPosition;
  LocalDirectives: TDBIPascalKeywords;
  KindName: String;
  Data: String;
  Argument: TDBIPascalArgument;
  Arguments: TStringList;
  Index: Integer;

begin
  LocalDirectives := [];

  Result := Have([Tok_OpenBracket]);
  while Result and (Token.TokenType <> Tok_CloseBracket) do begin
    // Directives
    LocalDirectives := [];
    while (Keyword in [pasConst, pasIn, pasOut, pasVar]) do begin
      Include(LocalDirectives, Keyword);

      Fetch(Keyword);
    end;

    // Argument Name(s)
    Arguments := Local(TStringList.Create).Obj as TStringList;
    while not Eof do begin
      Position := Token.TokenPosition;
      Arguments.Add(Fetch([Tok_Identifier, Tok_NameSpace]));
      if not Have([Tok_Comma]) then begin
        Break;
      end;
    end;

    // Does this Argument have a type
    if (Token.TokenType = Tok_Colon) then begin
      KindName := GetTypeSpecifier(nil, LocalDirectives);
    end;
    Data := GetConstantValue;

    for Index := 0 to Arguments.Count-1 do begin
      Argument := TDBIPascalArgument.Create(AScope.ChildOwner, AScope);
      Argument.Text := Arguments[Index];
      Argument.KindName := KindName;
      Argument.Data := Data;
      Argument.Directives := LocalDirectives;
      Argument.Position := Position;
    end;

    if not Have([Tok_SemiColon]) then begin
      Break;
    end;
  end;

  Fetch([Tok_CloseBracket]);
end;


function TDBICustomPascalLexer.GetConstantValue: String;
const
  Tok_ArithmeticOperators = [Tok_Multiply, Tok_Plus, Tok_Minus, Tok_Divide, Tok_Identifier];
  Tok_SimpleValues = [
    Tok_Identifier,
    Tok_NameSpace,
    Tok_HexLiteral,
    Tok_IntegerLiteral,
    Tok_FloatLiteral
    ];

var
  Success: Boolean;
  NestingCount: Integer;

begin
  Result := '';

  Success := Have([Tok_Equals]);
  while Success do begin
    // Arithmetic
    if (Token.TokenType in Tok_ArithmeticOperators) or (keyword in [pasDiv, pasNot]) then begin
      Result := Result + Chr_Space + Fetch(Tok_ArithmeticOperators);
    end

    // Open Array
    else if (Token.TokenType = Tok_OpenSquareBracket) then begin
      Result := Result + Chr_Space + GetOpenArray;
    end

    // Constant Array of Records or Values
    else if (Token.TokenType = Tok_OpenBracket) then begin
      Result := Result + Fetch([Tok_OpenBracket]);

      NestingCount := 1;
      while (NestingCount > 0) and not Eof do begin
        case Token.TokenType of
          Tok_OpenBracket: Inc(NestingCount);
          Tok_CloseBracket: Dec(NestingCount);
        end;

        Result := Result + Token.TokenString;
        NextToken;
      end;
    end

    // String Literal + Control Characters
    else if (Token.TokenType in [Tok_ControlCharacter, Tok_Apostrophe]) then begin
      while (Token.TokenType in [Tok_ControlCharacter, Tok_Apostrophe]) do begin
        if (Token.TokenType = Tok_Apostrophe) then begin
          Result := Result + GetQuotedString;
        end;

        while (Token.TokenType = Tok_ControlCharacter) do begin
          Result := Result + Fetch([Tok_ControlCharacter]);
        end;
      end;
    end

    // Otherwise simple value
    else if (Token.TokenType in Tok_SimpleValues) then begin
      Result := Result + Chr_Space + Fetch(Tok_SimpleValues);
    end

    else begin
      Success := False;
    end;
  end;
end;


function TDBICustomPascalLexer.GetDimensions: String;
var
  Success: Boolean;

begin
  Result := '';

  Success := Token.TokenType = Tok_OpenSquareBracket;
  while Success and not Eof do begin
    Success := Token.TokenType <> Tok_CloseSquareBracket;
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/01/2011 11:17:26 - Initial code.<br />
}
function TDBICustomPascalLexer.GetKeyword: TDBIPascalKeyword;
var
  Index: Integer;

begin
  Result := pasUnknown;

  if (Token.TokenType = Tok_UTF8) then begin
    Result := pasUTF8;
  end

  else if (Token.TokenType = Tok_Identifier) and (Token.TokenStatus = []) then begin
    Index := GetEnumValue(TypeInfo(TDBIPascalKeyword), 'pas' + Token.TokenString);
    if (Index <> -1) then begin
      Result := TDBIPascalKeyword(Index);
    end;
  end;
end;


function TDBICustomPascalLexer.GetMacro: TDBIPascalMacro;
var
  Index: Integer;

begin
  Result := macroUnknown;

  if (tsMacro in Token.TokenStatus) then begin
    Index := GetEnumValue(TypeInfo(TDBIPascalMacro), 'macro' + Token.TokenString);
    if (Index <> -1) then begin
      Result := TDBIPascalMacro(Index);
    end;
  end;
end;


function TDBICustomPascalLexer.GetPreviousToken: TDBILexerToken;
begin
  if not Assigned(FPreviousToken) then begin
    FPreviousToken := TDBILexerToken.Create;
  end;

  Result := FPreviousToken;
end;


function TDBICustomPascalLexer.GetQuotedString: String;
var
  Success: Boolean;

begin
  Result := '';

  repeat
    Success := Token.TokenType = Tok_Apostrophe;
    while Success do begin
      Result := Result + Token.TokenString;
      NextToken;
      Success := tsStringLiteral in Token.TokenStatus;
    end;
  until not (Token.TokenType = Tok_Apostrophe);
end;


function TDBICustomPascalLexer.GetTypeSpecifier(AScope: TDBIPascalScopedItem; var ADirectives: TDBIPascalKeywords): String;
var
  Success: Boolean;

begin
  Result := '';

  // leadin
  if (Token.TokenType = Tok_Colon) then begin
    Fetch([Tok_Colon]);
  end
  else begin
    Result := Result + Skip([Tok_Equals]);
  end;

  // Pointer
  if (Token.TokenType = Tok_Caret) then begin
    Result := Result + Fetch([Tok_Caret]);
  end;

  // Packed directive
  if Have(pasPacked) then begin
    Include(ADirectives, pasPacked);
  end;


  // Procedural Type
  if (Keyword in [pasFunction, pasProcedure]) then begin
    Include(ADirectives, Keyword);
    Result := Result + Fetch(Keyword);

    // Do we have parameters?
    if (Token.TokenType = Tok_OpenBracket) then begin
      if Assigned(AScope) then begin
        GetArguments(AScope);
      end
      else begin
        Result := Result + Upto([Tok_CloseBracket]);
        Result := Result + Fetch([Tok_CloseBracket]);
      end;
    end;

    // Is it a Function
    if Have([Tok_Colon]) then begin
      Result := Result + Chr_Colon + Chr_Space + Fetch([Tok_Identifier]);
    end;

    // Is it a Method Type
    if (Keyword = pasOf) then begin
      Result := Result + Chr_Space + Fetch(pasOf) + Chr_Space + Fetch(pasObject);
    end;

    // SemiColon
    Success := Skip([Tok_SemiColon]) <> '';

    // Directives
    case Keyword of
      pasCdecl: begin
        Include(ADirectives, pasCdecl);
        Fetch(pasCdecl);
      end;

      pasStdCall: begin
        Include(ADirectives, pasStdCall);
        Fetch(pasStdCall);
      end;
    else
      if Success then begin
        UndoToken(Chr_SemiColon);
      end;
    end;

    // Premature Exit;
//##JVR}    Exit;
  end

  // Array
  else if (Keyword = pasArray) then begin
    Result := Result + Fetch(pasArray);
    Result := Result + GetDimensions;
    Result := Result + Chr_Space + Fetch(pasOf);
    Result := Result + Skip([Tok_Caret]);
    Result := Result + Chr_Space + Fetch([Tok_Identifier, Tok_NameSpace]);
    Result := Result + GetDimensions;
  end

  // Reference to
  else if (Keyword = pasReference) then begin
    Result := Result + Fetch(pasReference);
    Result := Result + Chr_Space + Fetch(pasTo);
    Result := Result + Chr_Space + Fetch([Tok_Identifier, Tok_NameSpace]);
    Result := Result + Chr_Space + Upto([Tok_SemiColon]);
  end

  // Record Pointer
  else if (Keyword = pasRecord) then begin
    Result := Result + Fetch(pasRecord);
    Result := Result + Upto(pasEnd);
    Result := Result + Chr_Space + Fetch(pasEnd);
  end

  // Set Type
  else if (Keyword = pasSet) then begin
    Fetch(pasSet);
    Fetch(pasOf);

    if Have([Tok_OpenBracket]) then begin
      Result := Result + Upto([Tok_CloseBracket]) + Fetch([Tok_CloseBracket]);
    end
    else begin
      Result := Result + Upto([Tok_SemiColon]);
    end;
  end

  // Char or String
  else if (Token.TokenType = Tok_Apostrophe) then begin
    Result := Result + Fetch([Tok_Apostrophe]);
    Result := Result + Fetch([Tok_Identifier, Tok_IntegerLiteral]);
    Result := Result + Fetch([Tok_Apostrophe]);
  end

  // Enumerated Type
  else if (Token.TokenType = Tok_OpenBracket) then begin
    repeat
      Result := Result + Fetch([Token.TokenType]);
      Result := Result + Chr_Space + Fetch([Tok_Identifier]);

      // User defined ordinal value
      if (Token.TokenType = Tok_Equals) then begin
        Result := Result + Chr_Space + Fetch([Tok_Equals]);
        Result := Result + Chr_Space + Fetch([Tok_IntegerLiteral]);
      end;
    until (Token.TokenType <> Tok_Comma);
    Result := Result + Fetch([Tok_CloseBracket]);
  end

  // Otherwise Simple Type
  else begin
    Result := Result + Fetch([Tok_Identifier, Tok_IntegerLiteral, Tok_NameSpace]);
  end;


  // Range
  if Have([Tok_DotDot]) then begin
    if Have([Tok_Apostrophe]) then begin
      Result := Result + Chr_DotDot + Chr_Apostrophe;
      Result := Result + Fetch([Tok_Identifier, Tok_IntegerLiteral]);
      Result := Result + Fetch([Tok_Apostrophe]);
    end
    else begin
      Result := Result + Chr_DotDot + Fetch([Tok_Identifier, Tok_IntegerLiteral]);
    end
  end;

  // To Help avoid {$ifdef} - {$else} - {$endif} problens, skip the {$else} bit
  if (Keyword <> pasEnd) then begin
    Skip([Tok_Identifier, Tok_NameSpace]);
  end;

  Result := Result + GetDimensions;


  // Generics
  Success := Have([Tok_Smaller]);
  while Success do begin
    Success := not (Token.TokenType in [Tok_Greater, Tok_Eof]);
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;


function TDBICustomPascalLexer.Have(const Value: TDBIPascalKeyword): Boolean;
begin
  Result := Keyword = Value;
  if Result then begin
    NextToken;
  end;
end;


procedure TDBICustomPascalLexer.IncludeDirective(
  var ADirectives: TDBIPascalKeywords;
  const Value: TDBIPascalKeyword
  );
begin
  if (Keyword = Value) then begin
    NextToken;

    Include(ADirectives, Value);
  end;
end;


function TDBICustomPascalLexer.IsKeywordScopeInterrupter: Boolean;
begin
  Result := Keyword in TDBIPascalScopeInterruptors;
  if not (Result or (Keyword in [pasUnknown, pasIndex]) ) then begin
    Check(pasUnknown);
  end;
end;


procedure TDBICustomPascalLexer.LexControlCharacter;
begin
  Token.AsChar := LexerChar;
  GetChar;

  if (LexerChar in soDigits) then begin
    Token.TokenType := Tok_ControlCharacter;
    Token.AddChar(LexerChar);

    while GetChar do begin
      if (LexerChar in soDigits) then begin
        Token.AddChar(LexerChar);
      end
      else begin
        Break;
      end;

    end;
  end
  else begin
    PutChar(LexerChar);
    PutChar(PriorChar);
    GetChar;

    inherited LexSymbol;
  end;
end;


procedure TDBICustomPascalLexer.LexHexadecimal;
begin
  Token.AsChar := LexerChar;
  GetChar;

  if (LexerChar in soHexDigits) then begin
    Token.TokenType := Tok_HexLiteral;
    Token.AddChar(LexerChar);

    while GetChar do begin
      if (LexerChar in soHexDigits) then begin
        Token.AddChar(LexerChar);
      end
      else begin
        Break;
      end;

    end;
  end
  else begin
    PutChar(LexerChar);
    PutChar(PriorChar);
    GetChar;

    inherited LexSymbol;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/12/2010 15:10:47 - Initial code.<br />
}
procedure TDBICustomPascalLexer.LexIdentifier;
begin
  inherited LexIdentifier;

  while (LexerChar = Chr_Dot) do begin
    GetChar;

    // Is it a DotDot
    if (LexerChar = Chr_Dot) then begin
      PutChar(LexerChar);
      PutChar(PriorChar);
      GetChar;
      Break;
    end

    // Otherwise it is a namespace
    else begin
      Token.TokenType := Tok_NameSpace;
      Token.AddChar(Chr_Dot);

      while (LexerChar in soAlphaNumeric) do begin
        Token.AddChar(LexerChar);
        GetChar;
      end;
    end;

  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/12/2010 15:29:40 - Initial code.<br />
}
procedure TDBICustomPascalLexer.LexInitialise;
begin
  inherited LexInitialise;

  // String Literals toggles State only!
  MapSymbol(LexSymbol, Chr_Apostrophe, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);

  // ControlString
  MapSymbol(LexControlCharacter, Chr_Hash);

  // Hexadecimal
  MapSymbol(LexHexadecimal, Chr_Dollar);

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace, [tsStringLiteral, tsComment2, tsMask]);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace, [tsStringLiteral, tsComment2, tsMask]);

  // State: tsComment1 - {.....} style comments
  MapSymbol(LexSymbol, Chr_OpenCurlyBracket, Tok_OpenCurlyBracket, tkSymbol, [tsComment1]);
  MapSymbol(LexSymbol, Chr_CloseCurlyBracket, Tok_CloseCurlyBracket, tkSymbol, [tsComment1, tsMacro, tsMask]);

  // State: tsComment2 - //..... style comments
  MapDualSymbol(LexNone, Chr_SlashSlash, Tok_SlashSlash, tkSymbol, [tsComment2]);

  // State: tsComment3 - (*...*) style comments
  MapDualSymbol(LexNone, Chr_OpenBracket_Star, Tok_OpenBracket_Star, tkSymbol, [tsComment3]);
  MapDualSymbol(LexNone, Chr_CloseBracket_Star, Tok_CloseBracket_Star, tkSymbol, [tsComment3, tsMask]);

  // $ Dollar Macros
  MapDualSymbol(LexNone, Chr_OpenCurlyBracket_Dollar, Tok_OpenCurlyBracket_Dollar, tkSymbol, [tsMacro]);

  // Common Pascal Symbols
  MapDualSymbol(LexNone, Chr_DotDot, Tok_DotDot);
end;


procedure TDBICustomPascalLexer.LexSymbol;
const
  Chr_EF = #239;
  Chr_BF = #191;
  Chr_BB = #187;

begin
  inherited LexSymbol;

  // Bom - UTF8
  if (PriorChar = Chr_EF) and (Token.Row = 1) and (Token.Column = 1) then begin
    Token.TokenType := Tok_UTF8;

    Assert(LexerChar = Chr_BB);
    Token.AddChar(LexerChar);
    GetChar;

    Assert(LexerChar = Chr_BF);
    Token.AddChar(LexerChar);
    GetChar;
  end;
end;


procedure TDBICustomPascalLexer.SetStatus(Value: TDBITokenStatus);
begin
  // Exclude // and (* *) embedded style comments in a { } comment
  if (tsComment1 in LexerStatus) then begin
    Value := Value - [tsComment2, tsComment3];
  end

  // Exclude { } and (* *) embedded style comments when in a // comment
  else if (tsComment2 in LexerStatus) then begin
    Value := Value - [tsComment1, tsComment3];
  end

  // Exclude { } and // embedded style comments in a (* *) comment
  else if (tsComment3 in LexerStatus) then begin
    Value := Value - [tsComment1, tsComment2];
  end;

  inherited SetStatus(Value);
end;


function TDBICustomPascalLexer.Skip(const Value: TDBIPascalKeyword): String;
begin
  Result := '';
  while (Keyword = Value) and not Eof do begin
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;


procedure TDBICustomPascalLexer.UndoToken(const Value: AnsiString);
var
  Data: AnsiString;

begin
  if not Eof then begin
    PutChar(LexerChar);
  end;

  PutChar(Chr_Space);
  Data := Token.AsString;
  PutStr(Data);
  PutChar(Chr_Space);
  PutStr(Value);

  GetChar;
  NextToken;
end;


function TDBICustomPascalLexer.Upto(const Value: TDBIPascalKeyword): String;
begin
  Result := '';

  while not Eof do begin
    if (not (tsStringLiteral in Token.TokenStatus)) and (Keyword = Value) then begin
      Break;
    end;

    Result := Result + Token.TokenString;
    UptoToken;
  end;
end;


function TDBICustomPascalLexer.Upto(TokenTypes: TDBITokenTypes; const Inclusive: Boolean = False): String;
begin
  Result := '';

  while not Eof do begin
    if (not (tsStringLiteral in Token.TokenStatus)) and (Token.TokenType in TokenTypes) then begin
      if Inclusive then begin
        Result := Result + Token.TokenString;
        inherited NextToken;
      end;
      Break;
    end;

    Result := Result + Token.TokenString;
    UptoToken;
  end;
end;


function TDBICustomPascalLexer.Upto(TokenKinds: TDBITokenKinds; const Inclusive: Boolean = False): String;
begin
  Result := '';

  while not Eof do begin
    if (not (tsStringLiteral in Token.TokenStatus)) and (Token.TokenKind in TokenKinds) then begin
      if Inclusive then begin
        Result := Result + Token.TokenString;
        inherited NextToken;
      end;
      Break;
    end;

    Result := Result + Token.TokenString;
    UptoToken;
  end;
end;


procedure TDBICustomPascalLexer.UptoToken;
begin
  inherited NextToken;

  while not Eof do begin
    // tsComment1 or leadin
    if (tsComment1 in Token.TokenStatus) or (Token.TokenType in Tokens_Comment1) then begin
      inherited NextToken;
    end

    // tsComment2 or leadin
    else if (tsComment2 in Token.TokenStatus) or (Token.TokenType = Tok_SlashSlash) then begin
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
end;





{ XScope }

// _____________________________________________________________________________
{**
  Jvr - 21/12/2010 15:47:28 - Initial code.<br />
}
function TDBIPascalScope.GetScope(var ScopeField: TDBIPascalScope): TDBIPascalScope;
begin
  if not Assigned(ScopeField) then begin
    ScopeField := FScopeClass.Create(ChildOwner, Self);
  end;
  Result := ScopeField;
end;


function TDBIPascalScope.GetScopeForClass: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForConst: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForInterface: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForProcedure: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForRecord: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForResourceString: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForType: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForUse: TDBIPascalScope;
begin
  Result := GetScope;
end;

function TDBIPascalScope.GetScopeForVar: TDBIPascalScope;
begin
  Result := GetScope;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/12/2010 13:23:12 - Initial code.<br />
}
function TDBIPascalScope.GetScope: TDBIPascalScope;
begin
  Result := Self;
end;


function TDBIPascalScope.NewClass(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalClass;
begin
  Result := TDBIPascalClass.Create(ChildOwner, GetScopeForClass);
  Result.Text := AName;
  Result.KindName := AKindName;
  Result.SetScope(TDBIPascalPublished);
  Result.Position := APosition;

  GetScopeForClass.FComplex := Result;
end;


function TDBIPascalScope.NewConst(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalConst;
begin
  Result := TDBIPascalConst.Create(ChildOwner, GetScopeForConst);
  Result.Text := AName;
  Result.KindName := AKindName;
  Result.Position := APosition;
end;


function TDBIPascalScope.NewField(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalField;
begin
  Result := TDBIPascalField.Create(ChildOwner, GetScope);
  Result.Text := AName;
  Result.KindName := AKindName;
  Result.Position := APosition;
end;


function TDBIPascalScope.NewGuid(const AName: String; APosition: TDBITokenPosition): TDBIPascalGuid;
begin
  Result := TDBIPascalGuid.Create(ChildOwner, GetScope);
  Result.Text := AName;
  Result.Position := APosition;
end;


function TDBIPascalScope.NewProcedure(const AName: String; APosition: TDBITokenPosition): TDBIPascalProcedure;
begin
  Result := TDBIPascalProcedure.Create(ChildOwner, GetScopeForProcedure);
  Result.Text := AName;
  Result.Position := APosition
end;


function TDBIPascalScope.NewInterface(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalInterface;
begin
  Result := TDBIPascalInterface.Create(ChildOwner, GetScopeForInterface);
  Result.Text := AName;
  Result.KindName := AKindName;
  Result.SetScope(TDBIPascalPublic);
  Result.Position := APosition;

  GetScopeForInterface.FComplex := Result;
end;


function TDBIPascalScope.NewMethod(const AName: String; APosition: TDBITokenPosition): TDBIPascalMethod;
begin
  Result := TDBIPascalMethod.Create(ChildOwner, GetScope);
  Result.Text := AName;
  Result.Position := APosition;
end;


function TDBIPascalScope.NewProperty(const AName: String; APosition: TDBITokenPosition): TDBIPascalProperty;
begin
  Result := TDBIPascalProperty.Create(ChildOwner, GetScope);
  Result.Text := AName;
  Result.Position := APosition;
end;


function TDBIPascalScope.NewRecord(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalRecord;
begin
  Result := TDBIPascalRecord.Create(ChildOwner, GetScopeForRecord);
  Result.Text := AName;
  Result.KindName := AKindName;
  Result.SetScope(TDBIPascalPublished);
  Result.Position := APosition;

  GetScopeForRecord.FComplex := Result;
end;


function TDBIPascalScope.NewResourceString(const AName: String; APosition: TDBITokenPosition): TDBIPascalResourceString;
begin
  Result := TDBIPascalResourceString.Create(ChildOwner, GetScopeForResourceString);
  Result.Text := AName;
  Result.Position := APosition;
end;


function TDBIPascalScope.NewType(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalType;
begin
  Result := TDBIPascalType.Create(ChildOwner, GetScopeForType);
  Result.Text := AName;
  Result.KindName := AKindName;
  Result.Position := APosition
end;


function TDBIPascalScope.NewUnit(const AName: String; APosition: TDBITokenPosition): TDBIPascalUse;
begin
  Result := TDBIPascalUse.Create(ChildOwner, GetScopeForUse);
  Result.Text := AName;
  Result.Position := APosition;
end;


function TDBIPascalScope.NewVar(const AName, AKindName: String; APosition: TDBITokenPosition): TDBIPascalVar;
begin
  Result := TDBIPascalVar.Create(ChildOwner, GetScopeForVar);
  Result.Text := AName;
  Result.KindName := AKindName;
  Result.Position := APosition;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/12/2010 12:07:18 - Initial code.<br />
}
procedure TDBIPascalScope.SetScope(const Value: TDBIPascalScopeClass);
begin
  FScopeClass := Value;
end;





{ XUnit }

function TDBIPascalUnit.GetScopeForClass: TDBIPascalScope;
begin
  if (ugoClasses in GroupingOptions) then begin
    if not Assigned(FClasses) then begin
      FClasses := TDBIPascalClasses.Create(ChildOwner, Self);
      FClasses.Text := 'Classes';
    end;
    Result := FClasses;
  end
  else begin
    Result := inherited GetScopeForClass;
  end;
end;


function TDBIPascalUnit.GetScopeForConst: TDBIPascalScope;
begin
  if (ugoConsts in GroupingOptions) then begin
    if not Assigned(FConsts) then begin
      FConsts := TDBIPascalConsts.Create(ChildOwner, Self);
      FConsts.Text := 'Consts';
    end;
    Result := FConsts;
  end
  else begin
    Result := inherited GetScopeForConst;
  end;
end;


function TDBIPascalUnit.GetScopeForInterface: TDBIPascalScope;
begin
  if (ugoInterfaces in GroupingOptions) then begin
    if not Assigned(FInterfaces) then begin
      FInterfaces := TDBIPascalInterfaces.Create(ChildOwner, Self);
      FInterfaces.Text := 'Interfaces';
    end;
    Result := FInterfaces;
  end
  else begin
    Result := inherited GetScopeForInterface;
  end;
end;


function TDBIPascalUnit.GetScopeForProcedure: TDBIPascalScope;
begin
  if (ugoProcedures in GroupingOptions) then begin
    if not Assigned(FProcedures) then begin
      FProcedures := TDBIPascalProcedures.Create(ChildOwner, Self);
      FProcedures.Text := 'Procedures';
    end;
    Result := FProcedures;
  end
  else begin
    Result := inherited GetScopeForProcedure;
  end;
end;


function TDBIPascalUnit.GetScopeForRecord: TDBIPascalScope;
begin
  if (ugoRecords in GroupingOptions) then begin
    if not Assigned(FRecords) then begin
      FRecords := TDBIPascalRecords.Create(ChildOwner, Self);
      FRecords.Text := 'Records';
    end;
    Result := FRecords;
  end
  else begin
    Result := inherited GetScopeForRecord;
  end;
end;


function TDBIPascalUnit.GetScopeForResourceString: TDBIPascalScope;
begin
  if (ugoResourceStrings in GroupingOptions) then begin
    if not Assigned(FResourceStrings) then begin
      FResourceStrings := TDBIPascalResourceStrings.Create(ChildOwner, Self);
      FResourceStrings.Text := 'ResourceStrings';
    end;
    Result := FResourceStrings;
  end
  else begin
    Result := inherited GetScopeForResourceString;
  end;
end;

{##JVR
function XUnit.GetScope: XScope;
begin
  if Assigned(FComplex) then begin
    Result := FComplex.Scope;
  end
  else begin
    Result := Self;
  end;
end;
//}

function TDBIPascalUnit.GetScopeForType: TDBIPascalScope;
begin
  if (ugoTypes in GroupingOptions) then begin
    if not Assigned(FTypes) then begin
      FTypes := TDBIPascalTypes.Create(ChildOwner, Self);
      FTypes.Text := 'Types';
    end;
    Result := FTypes;
  end
  else begin
    Result := inherited GetScopeForType;
  end;
end;


function TDBIPascalUnit.GetScopeForUse: TDBIPascalScope;
begin
  if not Assigned(FUses) then begin
    FUses := TDBIPascalUses.Create(ChildOwner, Self);
    FUses.Text := 'Uses';
  end;
  Result := FUses;
end;


function TDBIPascalUnit.GetScopeForVar: TDBIPascalScope;
begin
  if (ugoVars in GroupingOptions) then begin
    if not Assigned(FVars) then begin
      FVars := TDBIPascalVars.Create(ChildOwner, Self);
      FVars.Text := 'Vars';
    end;
    Result := FVars;
  end
  else begin
    Result := inherited GetScopeForVar;
  end;
end;





{ XComplexes }

procedure TDBIPascalComplexes.Done;
begin
if not Assigned(FComplex) then begin
  Assert(Assigned(FComplex));
end;
  FComplex := nil;
end;


function TDBIPascalComplexes.GetComplex: TDBIPascalComplex;
begin
if not Assigned(FComplex) then begin
  Assert(Assigned(FComplex));
end;
  Result := FComplex;
end;


function TDBIPascalComplexes.GetScope: TDBIPascalScope;
begin
  Result := GetComplex.GetScope;
end;





{ XClass }

// _____________________________________________________________________________
{**
  Jvr - 13/12/2010 11:58:39 - Initial code.<br />
}
function TDBIPascalClass.GetScope: TDBIPascalScope;
begin
  if FScopeClass = TDBIPascalPublished then begin
    Result := GetScope(FPublished);
  end
  else if FScopeClass = TDBIPascalPublic then begin
    Result := GetScope(FPublic);
  end
  else if FScopeClass = TDBIPascalProtected then begin
    Result := GetScope(FProtected);
  end
  else if FScopeClass = TDBIPascalPrivate then begin
    Result := GetScope(FPrivate);
  end
  else begin
    raise Exception.CreateFmt('"%s": Invalid scope', [Self.Classname]);
  end;
end;





{ XComplex }

function TDBIPascalComplex.GetDisplayName: String;
begin
  Result := Text + ' = ' + ClassKindName;

  if  (KindName <> '') then begin
    Result := Result + ' (' + KindName + ')';
  end;

  if (pasAbstract in Directives) then begin
    Result := Result + '  abstract';
  end;
end;





{ TDBIPascalType }

function TDBIPascalType.GetDisplayName: String;
begin
  if (KindName = '') then begin
    Result := Text + ': <unknown>';
  end
  else begin
    Result := Text + ': ' + KindName;
  end;

  if (pasCdecl in Directives) then begin
    Result := Result + ': cdecl';
  end;

  if (pasStdCall in Directives) then begin
    Result := Result + ': stdcall';
  end;
end;





{ TDBIPascalProcedure }

function TDBIPascalProcedure.GetDisplayName: String;
begin
  if (pasConstructor in Directives) then begin
    Result := TDBIPascalMethodKindName[pasConstructor] + '  ' + Text;
  end
  else if (pasDestructor in Directives) then begin
    Result := TDBIPascalMethodKindName[pasDestructor] + '  ' + Text;
  end
  else if (pasFunction in Directives) then begin
    Result := TDBIPascalMethodKindName[pasFunction] + '  ' + Text + ': ' + KindName;
  end
  else if (pasProcedure in Directives) then begin
    Result := TDBIPascalMethodKindName[pasProcedure] + '  ' + Text;
  end
  else begin
    Result := TDBIPascalMethodKindName[pasOperator] + '  ' + Text;
  end;
end;





{ TDBIPascalConst }

function TDBIPascalConst.GetDisplayName: String;
begin
  if (KindName = '') then begin
    Result := {ClassKindName + '  ' +} Text + ' = ' + Data;
  end
  else begin
    Result := {ClassKindName + '  ' +} Text + ': ' + KindName + ' = ' + Data;
  end;
end;





{ TDBIPascalArguments }

procedure TDBIPascalArguments.Clone(AScope: TDBIPascalNode);
var
  Index: Integer;
  Item: TDBIPascalArgument;
  Argument: TDBIPascalArgument;

begin
  for Index := 0 to ChildCount-1 do begin
    Item := Children[Index] as TDBIPascalArgument;

    Argument := TDBIPascalArgument.Create(AScope.ChildOwner, AScope);
    Argument.Text := Item.Text;
    Argument.KindName := Item.KindName;
    Argument.Data := Item.Data;
    Argument.Directives := Item.Directives;
    Argument.Position := Item.Position;
  end;
end;





{ TDBIPascalNode }

// _____________________________________________________________________________
{**
  Jvr - 09/12/2010 14:21:37 - Initial code.<br />
}
function TDBIPascalNode.ChildOwner: TComponent;
begin
  if Assigned(Owner) then begin
    Result := Owner;
  end
  else begin
    Result := Self;
  end;

  Assert(Assigned(Result));
end;


procedure TDBIPascalNode.DirectiveInclude(const ADirective: TDBIPascalKeyword);
begin
  Include(FDirectives, ADirective);
end;


procedure TDBIPascalNode.DirectivesInclude(const ADirectives: TDBIPascalKeywords);
begin
  FDirectives := FDirectives + ADirectives;
end;


function TDBIPascalNode.GetClassKindName: String;
const
  KindOffset = 11; // length('TDBIPascal') + 1;
begin
  Result := LowerCase(Copy(Self.ClassName, KindOffset, 128));
end;


procedure TDBIPascalNode.SetClassKindName(const Value: String);
begin
  raise EPropertyError.Create('Property "ClassKindName" is read only');
end;


end.

