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
  DBITokenizerConsts;

type
  TDBIPascalKeyword = (
    pasUnknown,

    pasAs,
    pasAt,
    pasCase,
    pasFor,
    pasIndex,
    pasMessage,
    pasOf,
    pasObject,
    pasPacked,
    pasReference,
    pasSet,
    pasTo,
    pasType,

    // Types
    pasAnsiString,
    pasArray,
    pasBoolean,
    pasDouble,
    pasTDateTime,
    pasInteger,
    pasString,
    pasUnicodeString,
    pasWideString,

    // Scope
    pasClass,
    pasConst,
    pasEnd,
    pasDispinterface,
    pasFinalization,
    pasPrivate,
    pasProtected,
    pasPublic,
    pasPublished,
    pasIn,
    pasInterface,
    pasInitialization,
    pasImplementation,
    pasOut,
    pasRecord,
    pasResourceString,
    pasThreadVar,
    pasUnit,
    pasUses,
    pasVar,

    // Class Directives
    pasHelper,
    pasStrict,

    // Method Kinds
    pasConstructor,
    pasDestructor,
    pasFunction,
    pasProcedure,
    pasOperator,

    // Method Directives
    pasAbstract,
    pasCdecl,
    pasDeprecated,
    pasDispid,
    pasDynamic,
    pasInline,
    pasOverload,
    pasOverride,
    pasReintroduce,
    pasSafecall,
    pasStatic,
    pasStdcall,
    pasVirtual,

    // Property Directives
    pasDefault,
    pasProperty,
    pasRead,
    pasStored,
    pasWrite,

   _pasEnd
  );
  TDBIPascalKeywords = set of TDBIPascalKeyword;

  TDBIPascalMethodKind = pasConstructor..PasOperator;
  TDBIPascalMethodDirective = pasAbstract..pasVirtual;

const
  TDBIPascalMethodKinds = [Low(TDBIPascalMethodKind)..High(TDBIPascalMethodKind)];
  TDBIPascalMethodDirectives = [Low(TDBIPascalMethodDirective)..High(TDBIPascalMethodDirective)];

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

  TDBIPascalConst = class(TDBIPascalScopedItem)
  protected
    function GetDisplayName: String; override;
  end;

  TDBIPascalEnum = class(TDBIPascalScopedItem);
  TDBIPascalField = class(TDBIPascalScopedItem);
  TDBIPascalGuid = class(TDBIPascalScopedItem);

  TDBIPascalMethod = class(TDBIPascalScopedItem)
  protected
    function GetDisplayName: String; override;
  end;

  TDBIPascalProcedure = class(TDBIPascalScopedItem)
  protected
    function GetDisplayName: String; override;
  end;

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
    function GetPreviousToken: TDBILexerToken;
    procedure LexControlCharacter;
    procedure LexHexadecimal;
    procedure LexIdentifier; override;
    procedure LexInitialise; override;
    procedure SetStatus(Value: TDBITokenStatus); override;
    procedure UptoToken;

    property PreviousToken: TDBILexerToken read GetPreviousToken;

  public
    function GetConstantValue: String;
    function GetDimensions: String;
    function GetParameterList: String;
    function GetQuotedString: String;
    function GetTypeSpecifier: String;

    procedure IncludeDirective(var ADirectives: TDBIPascalKeywords; const Value: TDBIPascalKeyword);

  public
    function Check(TokenTypes: TDBITokenTypes): String; overload;
    function Check(TokenKinds: TDBITokenKinds): String; overload;
    procedure Check(const Value: TDBIPascalKeyword); overload;

    function Collate(ATokenKind: TDBITokenKind): String; overload;

    function Fetch(TokenTypes: TDBITokenTypes): String; overload;
    function Fetch(TokenKinds: TDBITokenKinds): String; overload;
    function Fetch(const Value: TDBIPascalKeyword): String; overload;

    function Have(TokenTypes: TDBITokenTypes): Boolean; overload;
    function Have(TokenKinds: TDBITokenKinds): Boolean; overload;
    function Have(const Value: TDBIPascalKeyword): Boolean; overload;

    function Upto(TokenTypes: TDBITokenTypes; const Inclusive: Boolean = False): String; overload;
    function Upto(TokenKinds: TDBITokenKinds; const Inclusive: Boolean = False): String; overload;
    function Upto(const Value: TDBIPascalKeyword): String; overload;

    procedure Skip(TokenTypes: TDBITokenTypes); overload;
    procedure Skip(TokenKinds: TDBITokenKinds); overload;
    procedure Skip(const Value: TDBIPascalKeyword); overload;

    property Keyword: TDBIPascalKeyword read GetKeyword;

    // HACK
    property GetOpenArray: String read GetDimensions;
  end;


type
  TDBIPascalInterfaceLexer = class(TDBICustomPascalLexer)
  public
    procedure NextToken; override;

    function UptoTokenTypeOrKeyword(TokenTypes: TDBITokenTypes; const AKeyword: TDBIPascalKeyword): String;
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
    function ProcessArguments(AScope: TDBIPascalScopedItem): Boolean;

    function ProcessArrayType(AScope: TDBIPascalScope; const ATypeName: String): Boolean; overload;
    function ProcessArrayType(AScope: TDBIPascalScope; const ATypeNames: TStrings): Boolean; overload;
    function ProcessCaseType(AScope: TDBIPascalScope): Boolean;
    function ProcessComplexType(AScope: TDBIPascalScope; const ATypeName: String; AComplexType: TDBIPascalKeyword): Boolean;
    function ProcessComplexTypeKeywords(AScope: TDBIPascalScope): Boolean;
    function ProcessComplexEnd(AScope: TDBIPascalScope): Boolean;
    function ProcessConst(AScope: TDBIPascalScope; AConstType: TDBIPascalKeyword): Boolean;
    function ProcessDirective(const ADirective: TDBIPascalKeyword): Boolean;
    function ProcessEnumeratedType(AScope: TDBIPascalScope; const ATypeName: String): Boolean;
    function ProcessField(AScope: TDBIPascalScope): Boolean;
    function ProcessGuid(AScope: TDBIPascalScope): Boolean;
    function ProcessProcedure(AScope: TDBIPascalScope; AProcedureType: TDBIPascalKeyword): Boolean;
    function ProcessProperty(AScope: TDBIPascalScope): Boolean;
    function ProcessMethod(
      AScope: TDBIPascalScope;
      const AMethodKind: TDBIPascalMethodKind;
      var MethodDirectives: TDBIPascalKeywords
      ): Boolean;

    function ProcessProceduralType(AScope: TDBIPascalScope; const ATypeName: String): Boolean;
    function ProcessScope(
      AScope: TDBIPascalScope;
      AScopeClass: TDBIPascalScopeClass;
      ScopeDirectives: TDBIPascalKeywords
      ): Boolean;

    function ProcessSetType(AScope: TDBIPascalScope; const ATypeName: String): Boolean;
    function ProcessSimpleType(AScope: TDBIPascalScope; ATypeName: String): Boolean; overload;
    function ProcessSimpleType(AScope: TDBIPascalScope; ATypeNames: TStrings): Boolean; overload;
    function ProcessType(AScope: TDBIPascalScope): Boolean;
    function ProcessUnit(AScope: TDBIPascalScope): Boolean;
    function ProcessUses(AScope: TDBIPascalScope): Boolean;
    function ProcessVar(AScope: TDBIPascalScope; const AVarType: TDBIPascalKeyword): Boolean;

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

// _____________________________________________________________________________
{**
  Jvr - 09/12/2010 10:07:44 - Initial code.<br />
}
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


function TDBIPascalInterfaceAnalyser.ProcessArrayType(AScope: TDBIPascalScope; const ATypeName: String): Boolean;
var
  TypeNames: TStrings;

begin
  TypeNames := Local(TStringList.Create).Obj as TStrings;
  TypeNames.Add(ATypeName);

  Result := ProcessArraytype(AScope, TypeNames);
end;


function TDBIPascalInterfaceAnalyser.ProcessArguments(AScope: TDBIPascalScopedItem): Boolean;
var
  Argument: TDBIPascalArgument;
  Arguments: TStringList;
  KindName: String;
  Data: String;
  Index: Integer;
  LocalDirectives: TDBIPascalKeywords;
  Position: TDBITokenPosition;

begin
  LocalDirectives := [];

  Result := Input.Have([Tok_OpenBracket]);
  while Result and (Input.Token.TokenType <> Tok_CloseBracket) do begin
    // Directives
    LocalDirectives := [];
    while (Input.Keyword in [pasConst, pasIn, pasOut, pasVar]) do begin
      Include(LocalDirectives, Input.Keyword);

      Input.Fetch(Input.Keyword);
    end;

    // Argument Name(s)
    Arguments := Local(TStringList.Create).Obj as TStringList;
    while not Input.Eof do begin
      Position := Input.Token.TokenPosition;
      Arguments.Add(Input.Fetch([Tok_Identifier, Tok_NameSpace]));
      if not Input.Have([Tok_Comma]) then begin
        Break;
      end;
    end;

    KindName := Input.GetTypeSpecifier;
    Data := Input.GetConstantValue;

    for Index := 0 to Arguments.Count-1 do begin
      Argument := TDBIPascalArgument.Create(AScope.ChildOwner, AScope);
      Argument.Text := Arguments[Index];
      Argument.KindName := KindName;
      Argument.Data := Data;
      Argument.Directives := LocalDirectives;
      Argument.Position := Position;
    end;

    if not Input.Have([Tok_SemiColon]) then begin
      Break;
    end;
  end;

  Input.Fetch([Tok_CloseBracket]);
end;


function TDBIPascalInterfaceAnalyser.ProcessArrayType(AScope: TDBIPascalScope; const ATypeNames: TStrings): Boolean;
var
  KindName: String;
  Index: Integer;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;

  KindName := Input.Fetch(pasArray);
  KindName := KindName + Input.GetDimensions;
  KindName := KindName + ' ' + Input.Fetch(pasOf);
  KindName := KindName + ' ' + Input.Fetch([Tok_Identifier]);

  Input.Skip([Tok_SemiColon]);

  for Index := 0 to ATypeNames.Count-1 do begin
    AScope.NewType(ATypeNames[Index], KindName, Position);
  end;
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
    if Input.Keyword <> pasUnknown then begin
      Break;
    end;

    Input.Fetch([Tok_Identifier, Tok_IntegerLiteral]);
    Input.Fetch([Tok_Colon]);

    // Is it a multi-part union definition
    if Input.Have([Tok_OpenBracket]) then begin
      while not Input.Eof do begin
        if Input.Have([Tok_CloseBracket]) then begin
          Break;
        end;

        TypeNames := Local(TStringList.Create).Obj as TStrings;
        while not Input.Eof do begin
          TypeNames.Add(Input.Fetch([Tok_Identifier]));
          if Input.Have([Tok_Colon]) then begin
            Break;
          end
          else begin
            Input.Fetch([Tok_Comma]);
          end;
        end;

        case Input.Keyword of
          pasArray: Result := ProcessArrayType(AScope, TypeNames);
        else
          Result := ProcessSimpleType(AScope, TypeNames);
        end;
      end;
    end
    else begin
      TypeName := Input.Fetch([Tok_Identifier]);
      Input.Fetch([Tok_Colon]);
      ProcessSimpleType(AScope, TypeName);
    end;

    Input.Fetch([Tok_SemiColon]);
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessComplexEnd(AScope: TDBIPascalScope): Boolean;
begin
  Result := True;
  Input.Fetch(pasEnd);

  if Input.Keyword = pasDeprecated then begin
    ProcessDirective(pasDeprecated);
  end
  else begin
    Input.Fetch([Tok_SemiColon]);
  end;

  if AScope.Parent is TDBIPascalComplexes then begin
   (AScope.Parent as TDBIPascalComplexes).Done;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/12/2010 14:26:55 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessComplexType(AScope: TDBIPascalScope; const ATypeName: String; AComplexType: TDBIPascalKeyword): Boolean;
var
  KindName: String;
  Complex: TDBIPascalComplex;
  LocalDirectives: TDBIPascalKeywords;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  LocalDirectives := [];
  Position := Input.Token.TokenPosition;
  Complex := nil;
  Result := True;
  Input.Fetch(AComplexType);

  // Is this an abstract class
  Input.IncludeDirective(LocalDirectives, pasAbstract);

  // Is this a class of type definition?
  if Input.Have(pasOf) then begin
    // What is the class / interface type name
    KindName := ' of ' + Input.fetch([Tok_Identifier]);
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
      pasConst: Result := ProcessConst(AScope, pasConst);
      pasConstructor: Result := ProcessMethod(AScope, pasConstructor, LocalDirectives);
      pasDestructor: Result := ProcessMethod(AScope, pasDestructor, LocalDirectives);
      pasFunction: Result := ProcessMethod(AScope, pasFunction, LocalDirectives);
      pasOperator: Result := ProcessMethod(AScope, pasOperator, LocalDirectives);
      pasProcedure: Result := ProcessMethod(AScope, pasProcedure, LocalDirectives);
      pasProperty: Result := ProcessProperty(AScope);
      pasPrivate: Result := ProcessScope(AScope, TDBIPascalPrivate, LocalDirectives);
      pasProtected: Result := ProcessScope(AScope, TDBIPascalProtected, LocalDirectives);
      pasPublic: Result := ProcessScope(AScope, TDBIPascalPublic, LocalDirectives);
      pasPublished: Result := ProcessScope(AScope, TDBIPascalPublished, LocalDirectives);
//##JVR      pasRecord: Result := ProcessField(Complex);
      PasType: Result := ProcessType(AScope); //##JVRProcessClassType(Complex);
      pasVar: Result := ProcessVar(AScope, pasVar);

      { TODO -oJVR -cTPascalCodeProcessor.ProcessComplexType() :
        21/02/2014 11:03:22
        Skip the Class directive for methods.
        Will revisit this later.
      }
      pasClass: begin
        Input.Fetch(pasClass);
        Include(LocalDirectives, pasClass);
      end;

      pasStrict: begin
        Input.Fetch(pasStrict);
        Include(LocalDirectives, pasStrict);
      end;

      pasEnd: begin
        Result := ProcessComplexEnd(AScope);
        Break;
      end;

    else
      if (Input.Keyword <> pasUnknown) then begin
        Break;
      end
      else if (Input.Token.TokenType = Tok_Identifier) then begin
        Result := ProcessField(AScope);
      end
      else if (Input.Token.TokenType = Tok_OpenSquareBracket) then begin
        Result := ProcessGuid(AScope);
      end
      else begin
        Break;
      end;
    end;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessConst(AScope: TDBIPascalScope; AConstType: TDBIPascalKeyword): Boolean;
var
  Index: Integer;
  ConstKind: String;
  ConstValue: String;
  Constants: TStrings;
  NestingCount: Integer;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;
  Input.Fetch(AConstType);

  Constants := Local(TStringList.Create).Obj as TStrings;
  while not Input.Eof do begin

    // Const Name
    Constants.Add(Input.Fetch([Tok_Identifier]));

    // Multiple variables
    if Input.Have([Tok_Comma]) then begin
      Continue;
    end;

    // Colon (:)
    if Input.Have([Tok_Colon]) then begin
      if Input.Have(pasArray) then begin
        ConstKind := 'array';
        ConstKind := ConstKind + Input.GetDimensions;
        ConstKind := ConstKind + ' ' + Input.Fetch(pasOf);
        ConstKind := ConstKind + ' ' + Input.Fetch([Tok_Identifier]);
        ConstKind := ConstKind + Input.GetDimensions;
      end
      else begin
        ConstKind := Input.Upto([Tok_Equals]);
      end;
    end;

    // Equals (=)
    Input.Fetch([Tok_Equals]);

    // Const Value
    if Input.Have([Tok_OpenBracket]) then begin
      ConstValue := Chr_OpenBracket;

      NestingCount := 1;
      while (NestingCount > 0) and not Input.Eof do begin
        if Input.Token.TokenType = Tok_OpenBracket then begin
          Inc(NestingCount);
        end;

        if Input.Token.TokenType = Tok_CloseBracket then begin
          Dec(NestingCount);
        end;

        ConstValue := ConstValue + Input.Token.TokenString;
        Input.NextToken;
      end;
    end
    else begin
      ConstValue := Input.Upto([Tok_SemiColon]);
    end;
    Input.Fetch([Tok_SemiColon]);

    // Check next keyword
    if (Input.Keyword <> pasUnknown) then begin
      Break;
    end;
  end;

  for Index := 0 to Constants.Count-1 do begin
    case AConstType of
//##JVR        pasArray: Result := ProcessArrayType(Ast.GetConsts, Constants);
      pasConst: AScope.NewConst(Constants[Index], ConstKind, Position).Data := ConstValue;
      pasResourceString: AScope.NewResourceString(Constants[Index], Position).Data := ConstValue;
    end;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessDirective(const ADirective: TDBIPascalKeyword): Boolean;
begin
  Result := True;
  Input.Fetch(ADirective);

  if ADirective = pasDeprecated then begin
    if Input.Have([Tok_Apostrophe]) then begin
      while (tsStringLiteral in Input.Token.TokenStatus) do begin
        Input.Nexttoken;
      end;
    end;
  end;

  Input.Fetch([Tok_SemiColon]);
end;


function TDBIPascalInterfaceAnalyser.ProcessEnumeratedType(AScope: TDBIPascalScope; const ATypeName: String): Boolean;
var
  Data: TStrings;
  Enum: TDBIPascalScopedItem;
  EnumName: String;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;
  Input.Fetch([Tok_OpenBracket]);

  Enum := AScope.NewType(ATypeName, 'Enumerated', Position);

  Data := Local(TStringList.Create).Obj as TStrings;
  while not Input.Eof do begin
    EnumName := Input.Fetch([Tok_Identifier]);
    if Input.Have([Tok_Equals]) then begin
      Data.Add(EnumName + '=' + Input.Fetch([Tok_IntegerLiteral]));
    end
    else begin
      Data.Add(EnumName);
    end;

    if Input.Have([Tok_CloseBracket]) then begin
      Break;
    end
    else begin
      Input.Fetch([Tok_Comma]);
    end;
  end;

  Enum.Data := Data.Text;
  Input.Fetch([Tok_SemiColon]);
end;


// _____________________________________________________________________________
{**
  Jvr - 08/12/2010 15:47:20 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessField(AScope: TDBIPascalScope): Boolean;
var
  FieldNames: TStrings;
  Position: TDBITokenPosition;
  TypeName: String;
  Index: Integer;

begin
  Assert(Assigned(AScope));
  Result := Input.Token.TokenType <> Tok_Eof;

  if Result then begin
    Position := Input.Token.TokenPosition;

    FieldNames := Local(TStringList.Create).Obj as TStrings;
    while (Input.Token.TokenType <> Tok_Eof) do begin
      FieldNames.Add(Input.Fetch([Tok_Identifier]));

      if not Input.Have([Tok_Comma]) then begin
        Break;
      end;
    end;

    TypeName := Input.GetTypeSpecifier;
(*##JVR
    Input.Fetch([Tok_Colon]);

    if (Input.Keyword = pasRecord) then begin
      Input.Fetch(pasRecord);
      TypeName := Input.UptoTokenTypeOrKeyword([Tok_SemiColon], pasEnd);
      Input.Fetch(pasEnd);
    end
    else begin
      TypeName := {Input.GetTypeSpecifier; //##JVR} Input.UptoTokenTypeOrKeyword([Tok_SemiColon], pasEnd);
    end;
//*)
    for Index := 0 to FieldNames.Count-1 do begin
      AScope.NewField(FieldNames[Index], TypeName, Position);
    end;

    Input.Skip([Tok_SemiColon]);
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
  UnitScope := GetUnit;

  case Input.KeyWord of
    pasConst: Result := ProcessConst(UnitScope, pasConst);
    pasImplementation: Result := False;
    pasResourceString: Result := ProcessConst(UnitScope, pasResourceString);
    pasThreadVar: Result := ProcessVar(UnitScope, pasThreadVar);
    pasType: Result := ProcessType(UnitScope);
    pasUnit: Result := ProcessUnit(UnitScope);
    pasUses: Result := ProcessUses(UnitScope);
    pasVar: Result := ProcessVar(UnitScope, pasVar);

    pasFunction: Result := ProcessProcedure(UnitScope, pasFunction);
    pasProcedure: Result := ProcessProcedure(UnitScope, pasProcedure);
  else
    Input.NextToken;
    Result := True;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessMethod(
  AScope: TDBIPascalScope;
  const AMethodKind: TDBIPascalMethodKind;
  var MethodDirectives: TDBIPascalKeywords
  ): Boolean;
var
  Method: TDBIPascalMethod;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;
  Input.Fetch(AMethodKind);
  Method := AScope.NewMethod(Input.Fetch([Tok_NameSpace, Tok_Identifier]), Position);

  // Do we have parameters?
  if Input.Token.TokenType = Tok_OpenBracket then begin
    ProcessArguments(Method);
  end;

{##ARGS
  if Input.Have([Tok_OpenBracket]) then begin
    Method.Data := Input.Upto([Tok_CloseBracket]);
    Input.Fetch([Tok_CloseBracket]);
  end;
//}
  // Do we have a return type? or is this interface method assigned to some other method
  Method.KindName := Input.GetTypeSpecifier;
  Input.Fetch([Tok_SemiColon]);

  Include(MethodDirectives, AMethodKind);
  while not Input.Eof do begin
    if (Input.Keyword = pasDispid) then begin
      Input.Fetch(pasDispid);
      Input.Fetch([Tok_IntegerLiteral]);
      Input.Fetch([Tok_SemiColon]);
    end
    else if (Input.Keyword in TDBIPascalMethodDirectives) then begin
      Result := ProcessDirective(Input.Keyword);
    end
    else begin
      Break;
    end;

    Include(MethodDirectives, Input.Keyword);
  end;

  Method.Directives := MethodDirectives;
  MethodDirectives := [];
end;


function TDBIPascalInterfaceAnalyser.ProcessProceduralType(AScope: TDBIPascalScope; const ATypeName: String): Boolean;
var
  Proc: TDBIPascalScopedItem;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;

  Proc := AScope.NewType(ATypeName, Input.Fetch([Tok_Identifier]), Position);

  if (Input.Token.TokenType = Tok_OpenBracket) then begin
    Proc.Data := Input.Upto([Tok_CloseBracket]);
    Input.Fetch([Tok_CloseBracket]);
  end;

  // Is it a function
  if Input.Have([Tok_Colon]) then begin
    Input.Fetch([Tok_Identifier]);
  end;

  Input.Skip(pasOf);
  Input.Skip(pasObject);
  Input.Skip([Tok_SemiColon]);

  // Directives
  if Input.Have(pasCdecl) then begin
    Proc.Directives := Proc.Directives + [pasCdecl];
    Input.Fetch([Tok_SemiColon]);
  end;

  if Input.Have(pasStdCall) then begin
    Proc.Directives := Proc.Directives + [pasStdCall];
    Input.Fetch([Tok_SemiColon]);
  end;

end;


function TDBIPascalInterfaceAnalyser.ProcessProcedure(AScope: TDBIPascalScope; AProcedureType: TDBIPascalKeyword): Boolean;
var
  Position: TDBITokenPosition;
  ProcName: String;
  ProcItem: TDBIPascalProcedure;
  LocalDirectives: TDBIPascalKeywords;

begin
  Assert(Assigned(AScope));
  LocalDirectives := [];
  Result := True;

  Input.Fetch(AProcedureType);
  Include(LocalDirectives, AProcedureType);
  Position := Input.Token.TokenPosition;
  ProcName := Input.Fetch([Tok_Identifier]);

  ProcItem := AScope.NewProcedure(ProcName, Position);
  ProcItem.Data := Input.GetParameterList;
  ProcItem.KindName := Input.GetTypeSpecifier;
  ProcItem.Directives := LocalDirectives;

  Input.Fetch([Tok_SemiColon]);
end;


function TDBIPascalInterfaceAnalyser.ProcessProperty(AScope: TDBIPascalScope): Boolean;
var
  Prop: TDBIPascalProperty;
  LocalDirectives: TDBIPascalKeywords;
  Position: TDBITokenPosition;

begin
  LocalDirectives := [];
  Assert(Assigned(AScope));
  Result := True;

  Position := Input.Token.TokenPosition;
  Input.Fetch(pasProperty);
  Prop := AScope.NewProperty(Input.Fetch([Tok_Identifier]), Position);
  Prop.KindName := Input.GetTypeSpecifier;
  Prop.Data := Input.UpTo([Tok_SemiColon]);
  Input.Fetch([Tok_SemiColon]);

  while not Input.Eof do begin
    case Input.Keyword of
      pasDefault: Result := ProcessDirective(pasDefault);
      pasStored: Result := ProcessDirective(pasStored);
    else
      Break;
    end;

    Include(LocalDirectives, Input.Keyword);
  end;

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


function TDBIPascalInterfaceAnalyser.ProcessSetType(AScope: TDBIPascalScope; const ATypeName: String): Boolean;
var
  KindName: String;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;

  Input.Fetch(pasSet);
  Input.Fetch(pasOf);

  if Input.Have([Tok_OpenBracket]) then begin
    KindName := Input.Upto([Tok_CloseBracket]);
    Input.Fetch([Tok_CloseBracket]);
  end
  else begin
    KindName := Input.Upto([Tok_SemiColon]);
  end;
  Input.Fetch([Tok_SemiColon]);

  AScope.NewType(ATypeName, KindName, Position);
end;


function TDBIPascalInterfaceAnalyser.ProcessSimpleType(AScope: TDBIPascalScope; ATypeName: String): Boolean;
var
  TypeNames: TStrings;

begin
  TypeNames := Local(TStringList.Create).Obj as TStrings;
  Typenames.Add(ATypeName);

  Result := ProcessSimpleType(AScope, TypeNames);
end;


function TDBIPascalInterfaceAnalyser.ProcessSimpleType(AScope: TDBIPascalScope; ATypeNames: TStrings): Boolean;
var
  KindName: String;
  Index: Integer;
  LocalDirectives: TDBIPascalKeywords;
  SimpleType: TDBIPascalScopedItem;
  Position: TDBITokenPosition;

begin
  Assert(Assigned(AScope));
  LocalDirectives := [];
  Position := Input.Token.TokenPosition;
  Result := True;

  if Input.Have([Tok_Caret]) then begin
    KindName := Chr_Caret;
  end;

  // Type directive
  if Input.Have(pasType) then begin
    Include(LocalDirectives, pasType);
  end;

  // Reference to
  if Input.Have(pasReference) then begin
    KindName := KindName + 'reference ' + Input.Fetch(pasTo);
    KindName := KindName + Input.Upto([Tok_SemiColon]);
  end

  // Char or String
  else if Input.Have([Tok_Apostrophe]) then begin
    KindName := KindName + Chr_Apostrophe;
    KindName := KindName + Input.Fetch([Tok_Identifier, Tok_IntegerLiteral]);
    KindName := KindName + Input.Fetch([Tok_Apostrophe]);
  end

  // Simple
  else begin
    KindName := KindName + Input.Fetch([Tok_NameSpace, Tok_Identifier, Tok_IntegerLiteral]);
  end;


  // Range
  if Input.Have([Tok_DotDot]) then begin
    if Input.Have([Tok_Apostrophe]) then begin
      KindName := KindName + Chr_DotDot + Chr_Apostrophe;
      KindName := KindName + Input.Fetch([Tok_Identifier, Tok_IntegerLiteral]);
      KindName := KindName + Input.Fetch([Tok_Apostrophe]);
    end
    else begin
      KindName := KindName + '..' + Input.Fetch([Tok_Identifier, Tok_IntegerLiteral]);
    end
  end

  // Generics
  else if Input.Have([Tok_Smaller]) then begin
    KindName := KindName + '<' + Input.Upto([Tok_Greater]) +'>';
    Input.Fetch([Tok_Greater]);
  end;

  // Dimensions
  KindName := Kindname + Input.GetDimensions;
  Input.Skip([Tok_SemiColon]);

  for Index := 0 to ATypeNames.Count-1 do begin
    SimpleType := AScope.NewType(ATypeNames[Index], KindName, Position); //##JVR .Data := Data;
    SimpleType.directives := localDirectives;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/12/2010 10:53:35 - Initial code.<br />
}
function TDBIPascalInterfaceAnalyser.ProcessType(AScope: TDBIPascalScope): Boolean;
const
  SUnexpectedKeyword = 'Unexpected keyword "%s" at [%d, %d], following packed, valid are array, record and set';

var
  TypeName: String;

begin
  Result := True;
  Input.Fetch(pasType);

  while not Input.Eof do begin
    if (Input.Keyword <> pasUnknown) then begin
      Break;
    end;

    TypeName := Input.Fetch([Tok_Identifier]);
    Input.Fetch([Tok_Equals]);

    case Input.Token.TokenType of
      Tok_OpenBracket: Result := ProcessEnumeratedType(AScope, TypeName);

    else
      case Input.Keyword of
        pasArray: Result := ProcessArrayType(AScope, TypeName);
        pasClass: Result := ProcessComplexType(AScope, TypeName, pasClass);
        pasConst: Result := ProcessConst(AScope, pasConst);
        pasDispinterface: Result := ProcessComplexType(AScope, TypeName, pasDispinterface);
        pasFunction: Result := ProcessProceduralType(AScope, TypeName);
        pasInterface: Result := ProcessComplexType(AScope, TypeName, pasInterface);
        pasProcedure: Result := ProcessProceduralType(AScope, TypeName);
        pasRecord: Result := ProcessComplexType(AScope, TypeName, pasRecord);
        pasSet: Result := ProcessSetType(AScope, TypeName);

        { TODO -oJVR -cTPascalCodeProcessor.ProcessComplexType() :
          21/02/2014 14:33:02
          Skip the packed directive for records.
          Will revisit this later.
        }
        pasPacked: begin
          Input.Fetch(pasPacked);
          case Input.Keyword of
            pasArray: Result := ProcessArrayType(AScope, TypeName);
            pasRecord: Result := ProcessComplexType(AScope, TypeName, pasRecord);
            pasSet: Result := ProcessSetType(AScope, TypeName);
          else
            raise ECodeAnalyserError.CreateFmt(
              SUnexpectedKeyword, [Input.Token.TokenString, Input.Token.Row, Input.Token.Column]);
          end;
        end;

      // Otherwise it is some other type
      else
        ProcessSimpleType(AScope, TypeName);
      end;
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
begin
  Assert(Assigned(AScope));
  Result := True;
  Input.Fetch(pasUses);

  while not Input.Eof do begin
    AScope.NewUnit(Input.Fetch([Tok_NameSpace, Tok_Identifier]), Input.Token.TokenPosition);

    if Input.Have([Tok_SemiColon]) then begin
      Break;
    end
    else begin
      Input.Fetch([Tok_Comma]);
    end;
  end;
end;


function TDBIPascalInterfaceAnalyser.ProcessVar(AScope: TDBIPascalScope; const AVarType: TDBIPascalKeyword): Boolean;
var
  Index: Integer;
  Position: TDBITokenPosition;
  VarKind: String;
  VarValue: String;
  Variables: TStrings;

begin
  Assert(Assigned(AScope));
  Position := Input.Token.TokenPosition;
  Result := True;
  Input.Fetch(AVarType);

  Variables := Local(TStringList.Create).Obj as TStrings;
  while not Input.Eof do begin

    // Var Name
    Variables.Add(Input.Fetch([Tok_Identifier]));

    // Multiple variables
    if Input.Have([Tok_Comma]) then begin
      Continue;
    end;

    // Colon (:)
    Input.Fetch([Tok_Colon]);

    // Var Kind
    VarKind := Input.Upto([Tok_Assign, Tok_SemiColon]);

    // Var Value;
    if Input.Have([Tok_Assign]) then begin
      VarValue := Input.Upto([Tok_SemiColon]);
    end
    else begin
      VarValue := '';
    end;
    Input.Fetch([Tok_SemiColon]);

    // Check next keyword
    if (Input.Keyword <> pasUnknown) then begin
      Break;
    end;
  end;

  for Index := 0 to Variables.count-1 do begin
    case AVarType of
      pasThreadVar: AScope.NewVar(Variables[Index], VarKind, position).Data := VarValue;
      pasVar: AScope.NewVar(Variables[Index], VarKind, Position).Data := VarValue;
    end;
  end;
end;





{ TPascalInterfaceAnalyser }

destructor TDBIPascalInterfaceAnalyser.Destroy;
begin
  FScope.Free;
  FScope := nil;

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
//##JVR  PreviousToken.Assign(Token);

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

    else begin
      Break;
    end;
  end;
end;


function TDBIPascalInterfaceLexer.UptoTokenTypeOrKeyword(TokenTypes: TDBITokenTypes; const AKeyword: TDBIPascalKeyword): String;
begin
  Result := '';

  while not Eof do begin
    if ((not (tsStringLiteral in Token.TokenStatus)) and
       ((Token.TokenType in TokenTypes) or (Keyword = AKeyword))) then
    begin
      Break;
    end;

    Result := Result + Token.TokenString;
    UptoToken;
  end;
end;





{ TCustomPascalLexer }

// _____________________________________________________________________________
{**
  Jvr - 16/12/2010 13:43:43 - Initial code.<br />
}
function TDBICustomPascalLexer.Check(TokenTypes: TDBITokenTypes): String;
const
  ErrMsg = 'Unexpected Token "%s" at [ %d, %d ]';

begin
  if not (Token.TokenType in TokenTypes) then begin
    raise ECodeAnalyserError.CreateFmt(ErrMsg, [Token.TokenString, Token.Row, Token.Column]);
  end;
end;


function TDBICustomPascalLexer.Check(TokenKinds: TDBITokenKinds): String;
const
  ErrMsg = 'Unexpected Token "%s" at [ %d, %d ]';

begin
  if not (Token.TokenKind in TokenKinds) then begin
    raise ECodeAnalyserError.CreateFmt(ErrMsg, [Token.TokenString, Token.Row, Token.Column]);
  end;
end;


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


// _____________________________________________________________________________
{**
  Jvr - 08/02/2005 18:10:24 - Initial code.<br>
}
function TDBICustomPascalLexer.Collate(ATokenKind: TDBITokenKind): String;
begin
  Result := '';

  while (Token.TokenKind = ATokenKind) do begin
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2008 21:08:51 - Initial code.<br />
}
function TDBICustomPascalLexer.Fetch(TokenTypes: TDBITokenTypes): String;
begin
  Result := Token.TokenString;
  Check(TokenTypes);
  NextToken;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2008 21:09:07 - Initial code.<br />
}
function TDBICustomPascalLexer.Fetch(TokenKinds: TDBITokenKinds): String;
begin
  Result := Token.TokenString;
  Check(TokenKinds);
  NextToken;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/12/2010 18:56:47 - Initial code.<br />
}
function TDBICustomPascalLexer.Fetch(const Value: TDBIPascalKeyword): String;
begin
  Result := Token.TokenString;
  Check(Value);
  NextToken;
end;


function TDBICustomPascalLexer.GetConstantValue: String;
const
  Tok_ArithmeticOperators = [Tok_Multiply, Tok_Plus, Tok_Minus, Tok_Divide];
  Tok_Values = [
    Tok_Identifier,
    Tok_NameSpace,
    Tok_ControlCharacter,
    Tok_HexLiteral,
    Tok_IntegerLiteral,
    Tok_FloatLiteral
    ];

var
  Success: Boolean;

begin
  Result := '';

  Success := Token.TokenType = Tok_Equals;
  if Success then begin
    Result := Result + Fetch([Tok_Equals]);

    // String Literal
    if (Token.TokenType = Tok_Apostrophe) then begin
      Result := Result + GetQuotedString;
    end

    // Open Array
    else if (Token.TokenType = Tok_OpenSquareBracket) then begin
      Result := Result + GetOpenArray;
    end

    // Otherwise simple value
    else begin
      Result := Result + ' ' + Fetch(Tok_Values);
    end;

    // Arithmetic
    if (Token.TokenType in Tok_ArithmeticOperators) then begin
      Result := Result + Fetch(Tok_ArithmeticOperators);
      Result := Result + ' ' + Fetch(Tok_Values);
    end

    else if (Token.TokenType = Tok_OpenBracket) then begin
      Result := Result + Upto([Tok_CloseBracket]);
      Result := Result + Fetch([Tok_CloseBracket]);
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


function TDBICustomPascalLexer.GetKeyword: TDBIPascalKeyword;
var
  Index: Integer;

begin
  Result := pasUnknown;

  if (Token.TokenType = Tok_Identifier) and (Token.TokenStatus = []) then begin
    Index := GetEnumValue(TypeInfo(TDBIPascalKeyword), 'pas' + Token.TokenString);
    if (Index <> -1) then begin
      Result := TDBIPascalKeyword(Index);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/01/2011 11:17:26 - Initial code.<br />
}
function TDBICustomPascalLexer.GetParameterList: String;
var
  Success: Boolean;

begin
  Result := '';

  Success := Token.TokenType = Tok_OpenBracket;
  while Success do begin
    Success := not (Token.TokenType in [Tok_CloseBracket, Tok_Eof]);
    Result := Result + Token.TokenString;
    NextToken;
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

  Success := Token.TokenType = Tok_Apostrophe;
  while Success and not Eof do begin
    Result := Result + Token.TokenString;
    NextToken;
    Success := tsStringLiteral in Token.TokenStatus;
  end;
end;


function TDBICustomPascalLexer.GetTypeSpecifier: String;
var
  Success: Boolean;

begin
  Result := GetDimensions;

  // Get Type (Kind) name
  Success := Token.TokenType in [Tok_Colon, Tok_Equals];
  if Success then begin
    // Fetch leadin
    if (Token.TokenType = Tok_Colon) then begin
      Fetch([Tok_Colon]);
    end
    else begin
      Result := Result + Fetch([Tok_Equals]);
    end;

    // Pointer
    if (Token.TokenType = Tok_Caret) then begin
      Result := Result + Fetch([Tok_Caret]);
    end;

{##JVR
    // Type directive
    if Input.Have(pasType) then begin
      Include(LocalDirectives, pasType);
    end;
//}
    // Reference to
    if (Keyword = pasReference) then begin
      Result := Result + Fetch(pasReference);
      Result := Result + ' ' + Fetch(pasTo);
      Result := Result + ' ' + Fetch([Tok_Identifier, Tok_NameSpace]);
//##JVR      Result := Result + ' ' + Upto([Tok_SemiColon]);
    end
{##JVR
    // Char or String
    else if Have([Tok_Apostrophe]) then begin
      Result := Result + Chr_Apostrophe;
      Result := Result + Fetch([Tok_Identifier, Tok_IntegerLiteral]);
      Result := Result + Fetch([Tok_Apostrophe]);
    end
//}
    // Record Pointer
    else if (Keyword = pasRecord) then begin
      Result := Result + Fetch(pasRecord);
      Result := Result + Upto(pasEnd);
      Result := Result + ' ' + Fetch(pasEnd);
    end

    // Array
    else if (Keyword = pasArray) then begin
      Result := Result + Fetch(pasArray);
      Result := Result + GetDimensions;
      Result := Result + ' ' + Fetch(pasOf);
      Result := Result + ' ' + Fetch([Tok_Identifier, Tok_NameSpace]);
      Result := Result + GetDimensions;
    end

    // Otherwise default
    else begin
      Result := Result + Fetch([Tok_Identifier, Tok_NameSpace]);

      // To Help avoid {$ifdef} - {$else} - {$endif} problens, skip the {$else} bit
      if (Keyword <> pasEnd) then begin
        Skip([Tok_Identifier, Tok_NameSpace]);
      end;

      Result := Result + GetDimensions;
    end;

  end;


  // Generics
  Success := Success and (Token.TokenType = Tok_Smaller);
  if Success then begin
    Result := Result + Fetch([Tok_Smaller]);
    while Success do begin
      Success := not (Token.TokenType in [Tok_Greater, Tok_Eof]);
      Result := Result + Token.tokenString;
      NextToken;
    end;
  end;
end;


function TDBICustomPascalLexer.Have(TokenTypes: TDBITokenTypes): Boolean;
begin
  Result := (Token.TokenType in TokenTypes);
  if Result then begin;
    NextToken;
  end;
end;


function TDBICustomPascalLexer.Have(TokenKinds: TDBITokenKinds): Boolean;
begin
  Result := (Token.TokenKind in TokenKinds);
  if Result then begin
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


procedure TDBICustomPascalLexer.LexIdentifier;
begin
  inherited LexIdentifier;

  if (LexerChar = Chr_Dot) then begin
    Token.TokenType := Tok_NameSpace;

    while (LexerChar = Chr_Dot) do begin
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
  MapSymbol(LexSymbol, Chr_CloseCurlyBracket, Tok_CloseCurlyBracket, tkSymbol, [tsComment1, tsMask]);

  // State: tsComment2 - //..... style comments
  MapDualSymbol(LexNone, Chr_SlashSlash, Tok_SlashSlash, tkSymbol, [tsComment2]);

  // State: tsComment3 - (*...*) style comments
  MapDualSymbol(LexNone, Chr_OpenBracket_Star, Tok_OpenBracket_Star, tkSymbol, [tsComment3]);
  MapDualSymbol(LexNone, Chr_CloseBracket_Star, Tok_CloseBracket_Star, tkSymbol, [tsComment3, tsMask]);

  // Common Pascal Symbols
  MapDualSymbol(LexNone, Chr_DotDot, Tok_DotDot);
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


// _____________________________________________________________________________
{**
  Jvr - 14/10/2009 12:09:08 - Initial code.<br />
}
procedure TDBICustomPascalLexer.Skip(TokenTypes: TDBITokenTypes);
begin
  while (Token.TokenType in TokenTypes) and not Eof do begin
    NextToken;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/03/2008 20:50:03 - Initial code.<br />
}
procedure TDBICustomPascalLexer.Skip(TokenKinds: TDBITokenKinds);
begin
  while (Token.TokenKind in TokenKinds) and not Eof do begin
    NextToken;
  end;
end;


procedure TDBICustomPascalLexer.Skip(const Value: TDBIPascalKeyword);
begin
  while (Keyword = Value) and not Eof do begin
    NextToken;
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


// _____________________________________________________________________________
{**
  Jvr - 21/12/2010 15:10:47 - Initial code.<br />
}
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
  if (ugoresourceStrings in GroupingOptions) then begin
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

  if  (kindName <> '') then begin
    Result := Result + ' (' + KindName + ')';
  end;

  if (pasAbstract in Directives) then begin
    Result := Result + '  abstract';
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

  Assert(Result is TDBIPascalUnit);
end;


procedure TDBIPascalNode.DirectiveInclude(const ADirective: TDBIPascalKeyword);
begin
  Include(FDirectives, ADirective);
end;


function TDBIPascalNode.GetClassKindName: String;
const
  KindOffset = 11; // 'TDBIPascal'
begin
  Result := LowerCase(Copy(Self.ClassName, KindOffset, 128));
end;


procedure TDBIPascalNode.SetClassKindName(const Value: String);
begin
  raise EPropertyError.Create('Property "ClassKindName" is read only');
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
  if (pasFunction in Directives) then begin
    Result := 'function  ' + Text + ': ' + KindName;
  end
  else begin
    Result := 'procedure   ' + Text;
  end;
end;





{ TDBIPascalMethod }

function TDBIPascalMethod.GetDisplayName: String;
begin
  if (pasConstructor in Directives) then begin
    Result := TDBIPascalMethodKindName[pasConstructor] + '  ' + Text;
  end
  else if (pasDestructor in Directives) then begin
    Result := TDBIPascalMethodKindName[pasDestructor] + '  ' + Text;
  end
  else if (pasFunction in Directives) then begin
    Result := TDBIPascalMethodKindName[pasFunction] + '  ' + Text + ': ' + kindName;
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





end.

