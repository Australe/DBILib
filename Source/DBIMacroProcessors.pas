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
  1.0 | 17/03/2005 11:04:40 | Jvr | Initial Release
  1.1 | 04/06/2014 06:38:24 | Jvr | Fully refactored
  1.2 | 16/07/2015 09:12:00 | Jvr | Merged Enumeration
  ______________________________________________________________________________
}

{#omcodecop off : jvr : DBILib}

unit DBIMacroProcessors;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DBITypInfo, DB, DBIConst, DBIUtils, DBIComponents,
  DBIStreamAdapters, DBITokenizerConsts, DBITokenizers, DBIExpressionEvaluators;

type
  TDBICustomScope = class;
  TDBICustomScopeClass = class of TDBICustomScope;
  TDBICustomScopeCallback = procedure(AScope: TDBICustomScope) of object;

  TDBICustomScope = class(TDBILexerToken)
  private
    FCallBack: TDBICustomScopeCallback;
    FData: TObject;
    FItemName: String;
    FName: String;
    FParams: TDBIParamsAdapter;
    FParent: TDBICustomScope;

  protected
    function GetEnabled: Boolean; virtual;
    function GetItemName: String; virtual;
    function GetName: String; virtual;
    function GetParams: TDBIParamsAdapter; virtual;

    procedure SetItemName(const value: String); virtual;
    procedure SetName(const Value: String); virtual;

    procedure UpdateParams; virtual;

    property CallBack: TDBICustomScopeCallback read FCallback write FCallback;
    property Data: TObject read FData write FData;
    property Parent: TDBICustomScope read FParent write FParent;

  public
    constructor Create(AData: TObject = nil); virtual;
    destructor Destroy; override;

    function CheckType(AClassType: TDBICustomScopeClass): Boolean;

    property Enabled: Boolean read GetEnabled;
    property ItemName: String read GetItemName write SetItemName;
    property Name: String read GetName write SetName;
    property Params: TDBIParamsAdapter read GetParams;

  end;


type
  TDBICustomGlobalScope = class(TDBICustomScope)
  protected
    function GetItemName: String; override;
    function GetEnabled: Boolean; override;

  end;


type
  TDBIGlobalScope = class(TDBICustomGlobalScope)
  protected
    function GetComputerName: String;
    function GetDate: String;
    function GetDateStamp: String;
    function GetDay: String;
    function GetLongDay: String;
    function GetLongMonth: String;
    function GetMonth: String;
    function GetNow: TDateTime;
    function GetShortDay: String;
    function GetShortMonth: String;
    function GetTime: String;
    function GetTimeStamp: String;
    function GetToday: String;
    function GetUserName: String;
    function GetYear: String;

  published
    property ComputerName: String read GetComputerName;
    property Date: String read GetDate;
    property DateStamp: String read GetDateStamp;
    property Day: String read GetDay;
    property LongDay: String read GetLongDay;
    property LongMonth: String read GetLongMonth;
    property Month: String read GetMonth;
    property Now: TDateTime read GetNow;
    property ShortDay: String read GetShortDay;
    property ShortMonth: String read GetShortMonth;
    property Time: String read GetTime;
    property TimeStamp: String read GetTimeStamp;
    property Today: String read GetToday;
    property UserName: String read GetUserName;
    property Year: String read GetYear;

  end;


type
  TDBIScopeStack = class(TPersistent)
  private
    FItems: TList;

  protected
    procedure Clear;

    function GetGlobal: TDBICustomScope; virtual;
    function GetItems: TList;
    function GetTop: TDBICustomScope;

    property Global: TDBICustomScope read GetGlobal;
    property Items: TList read GetItems;

  public
    destructor Destroy; override;

    function FindParam(const ParamName: String): TParam;
    function Push(Item: TDBICustomScope): TDBICustomScope;
    function Pop: TDBICustomScope;

    property Top: TDBICustomScope read GetTop;
  end;


type
  TDBICustomBindingsList = class(TStringList);
  TDBIScopeClass = class of TDBICustomScope;

  TDBIGlobalScopeStack = class(TDBIScopeStack)
  private
    FBindings: TDBICustomBindingsList;

  protected
    function GetBindings: TDBICustomBindingsList;
    function GetGlobal: TDBICustomScope; override;

  public
    destructor Destroy; override;

    procedure AddBinding(
      const ScopeName: String;
      const ScopeClass: TDBIScopeClass;
      const ScopeData: TObject
      );

    function Bind(
      const DataBindingName: String;
      const EnumeratorName: String;
      const KeyName: String;
      const Recursive: Boolean = False
      ): TDBICustomScope;

    procedure ReleaseBindings;

    property Bindings: TDBICustomBindingsList read GetBindings;
  end;


type
  TDBICustomMacroProcessorState = (msIf, msEnd);

  TDBIIfThenElseScope = class(TDBICustomScope)
  private
    FState: TDBICustomMacroProcessorState;
    FCondition: Boolean;

  protected
    function GetEnabled: Boolean; override;

  public
    property State: TDBICustomMacroProcessorState read FState write FState;
    property Condition: Boolean read FCondition write FCondition;

  end;


type
  TDBICustomEnumeratorScope = class(TDBICustomScope)
  private
    FItemIndex: Integer;
    FKeyName: String;

  protected
    function GetBof: Boolean; virtual;
    function GetCount: Integer; virtual;
    function GetCurrent: TObject; virtual;
    function GetEnabled: Boolean; override;
    function GetEof: Boolean; virtual;
    function GetItemIndex: Integer; virtual;
    function GetKeyName: String; virtual;

    procedure SetKeyName(const Value: String); virtual;
    procedure UpdateParams; override;

    procedure First; virtual;
    function Next: Boolean; virtual;
    procedure Last; virtual;

    property Bof: Boolean read GetBof;
    property Count: Integer read GetCount;
    property Eof: Boolean read GetEof;
    property ItemIndex: Integer read GetItemIndex;
    property KeyName: String read GetKeyName write SetKeyName;

  public
    constructor Create(AData: TObject); override;
    destructor Destroy; override;

    procedure UpdateParentParams(AParams: TDBIParamsAdapter); virtual;

  end;


  TDBINullEnumeratorScope = class(TDBICustomEnumeratorScope)
  end;


type
  TDBIStringsEnumeratorScope = class(TDBICustomEnumeratorScope)
  protected
    function GetCount: Integer; override;
    function GetDataType: String;
    function GetStrings: TStrings;
    function GetName: String; override;
    function GetValue: String;

  published
    property DataType: String read GetDataType;
    property Name;
    property Value: String read GetValue;

  end;


type
  TDBICollectionEnumeratorScope = class(TDBICustomEnumeratorScope)
  protected
    function GetCount: Integer; override;
    function GetCurrent: TObject; override;
    procedure UpdateParams; override;

  end;


  TDBIParamsEnumeratorScope = class(TDBICollectionEnumeratorScope)
  protected
    procedure UpdateParams; override;

  end;


type
  TDBIComponentEnumeratorScope = class(TDBICustomEnumeratorScope)
  protected
    function GetCount: Integer; override;
    function GetCurrent: TObject; override;
    procedure UpdateParams; override;

  end;


  TDBIComponentTraversalScope = class(TDBIComponentEnumeratorScope)
  private
    FList: TList;

  protected
    function GetCount: Integer; override;
    function GetList: TList;
    function GetCurrent: TObject; override;
    procedure UpdateParams; override;

  public
    destructor Destroy; override;

  end;


type
  TDBICustomDatasetEnumeratorScope = class(TDBICustomEnumeratorScope)
  private
    FCursor: TDataset;

  protected
    procedure First; override;

    function GetCount: Integer; override;
    function GetCursor: TDataset; virtual;

    procedure Last; override;
    function Next: Boolean; override;

    procedure UpdateParams; override;

  public
    destructor Destroy; override;

  end;

{$ifndef fpc}
  TDBIClientDatasetEnumeratorScope = class(TDBICustomDatasetEnumeratorScope)
  protected
    function GetCursor: TDataset; override;

  end;
{$endif}

  TDBIObjectListDatasetEnumeratorScope = class(TDBICustomDatasetEnumeratorScope)
  protected
    function GetCursor: TDataset; override;

  end;


type
  TDBICustomScopedMacroProcessor = class(TDBICustomMacroProcessor)
  private
    FScope: TDBIScopeStack;

  protected
    function CreateScope: TDBIScopeStack; virtual;
    procedure EnabledHandler(Sender: TObject; var Enabled: Boolean);

    function GetScope: TDBIScopeStack; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

  end;


type
  TDBIStandardKeyword = (
    Keyword_Unknown,
    Keyword_As,
    Keyword_Boolean,
    Keyword_DateTime,
    Keyword_Double,
    Keyword_Define,
    Keyword_ForEach,
    Keyword_Else,
    Keyword_EndFor,
    Keyword_EndIf,
    Keyword_If,
    Keyword_Integer,
    Keyword_Short,
    Keyword_String
    );
  TDBIStandardKeywords = set of TDBIStandardKeyword;

  TDBIGenericMacroLexer = class(TDBICustomMacroLexer)
  protected
    function GetDotString(Scope: TDBIScopeStack): String;
    function GetKeyword: TDBIStandardKeyword;

    procedure LexInitialise; override;
    procedure LexEscape;

    function Upto(TokenTypes: TDBITokenTypes; const Inclusive: Boolean = False): String; overload;
    function Upto(TokenKinds: TDBITokenKinds; const Inclusive: Boolean = False): String; overload;

    procedure UptoToken;

  public
    function Check(Keywords: TDBIStandardKeywords): String; overload;
    function Fetch(Keywords: TDBIStandardKeywords): String; overload;
    function Have(Keywords: TDBIStandardKeywords): Boolean; overload;
    function Skip(Keywords: TDBIStandardKeywords): String; overload;

    property Keyword: TDBIStandardKeyword read GetKeyword;
  end;


type
  TDBIMacroProcessorIncludeEvent = function(const IncludeName: string): string of object;

  TDBIGenericMacroProcessor = class(TDBICustomScopedMacroProcessor)
  private
    FEvaluator: TDBIExpressionEvaluator;
    FLexer: TDBIGenericMacroLexer;
    FMacroName: String;
    FMacroValue: String;
    FNullParam: TParam;
    FOnInclude: TDBIMacroProcessorIncludeEvent;
    FOrigin: TDBICompoundComponent;
    FScope: TDBIGlobalScopeStack;

  protected
    function CreateScope: TDBIScopeStack; override;
    function FindParam(const ParamName: String): TParam;

    function GetEvaluator: TDBIExpressionEvaluator;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function GetMemberComponent(const AName: String): TComponent;
    function GetMemberComponentParams(const AName: String): TParams;

    function GetParam(
      Sender: TObject; const ParamName: String; var ParamValue: Variant
      ): Boolean; override;

    function ProcessComments: Boolean; virtual;
    function ProcessDefine(const ParamType: TDBIStandardKeyword; const ExpectedTypes: TDBITokenTypes): Boolean;
    function ProcessElse: Boolean;
    function ProcessEndFor: Boolean;
    function ProcessEndIf: Boolean;
    function ProcessForEach: Boolean;

    function ProcessGetMacroName: String;
    function ProcessGetMacroValue(
      const ExpectedTypes: TDBITokenTypes;
      const InvalidTokens: TDBITokenTypes = [Tok_BackSlash, Tok_OpenCurlyBracket, Tok_CloseCurlyBracket, Tok_Eof]
      ): String;

    function ProcessIf: Boolean;
    function ProcessMacros: Boolean; virtual;

    property Evaluator: TDBIExpressionEvaluator read GetEvaluator;
    property Input: TDBIGenericMacroLexer read FLexer;
    property NullParam: TParam read FNullParam;
    property OnInclude: TDBIMacroProcessorIncludeEvent read FOnInclude write FOnInclude;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Process; override;

    property Scope: TDBIGlobalScopeStack read FScope;
  end;


implementation

uses
{$ifdef Delphi6}
  Types,
{$endif}
  Windows,
  Controls,
  TypInfo,
{$ifndef fpc}
  DBConsts,
  DBClient,
{$endif}
  DBIDataset,
  DBIObjectListDatasets;


const
  paramMin = '.min';
  paramMax = '.max';
  paramCount = '.count';
  paramHasNext = '.hasnext';
  paramLevel = '.level';
  paramItemName = '.name';
  paramClassName = '.classname';
  paramCaption = '.caption';
  paramDataType = '.datatype';
  paramValue = '.value';
  paramPathName = '.pathname';
  paramParentClassName = '.parent.classname';
  paramParentCaption = 'parent.caption';
  paramParentID = '.parent.ID';


{ Some additional String Utils}

function DataTypeName(const DataType: TFieldType): String;
begin
  Result := Copy(TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(DataType)), 3, MAXBYTE);
end;


function ExtractObjectPath(const CompoundName: String): String;
const
  Separator = '.';
begin
  Result := Copy(
    CompoundName, 1,
    Length(CompoundName) - Length(String(StrRScan(PChar(CompoundName), Separator)))
    );
end;


function ExtractPropertyName(const CompoundName: String): String;
const
  Separator = '.';
begin
  Result := String(StrRScan(PChar(CompoundName), Separator));
  if Length(Result) > 0 then begin
    Result := Copy(Result, 2, MAXBYTE);
  end;
end;





{ TDBICustomEnumeratorProcessor }

constructor TDBIGenericMacroProcessor.Create;
begin
  inherited Create;

  FNullParam := TParam.Create(nil);
  FNullParam.AsString := '0';

  FLexer := GetInput as TDBIGenericMacroLexer;
end;


// _____________________________________________________________________________
{**
  Jvr - 19/01/2009 11:15:15 - Initial code.<br>
}
function TDBIGenericMacroProcessor.CreateScope: TDBIScopeStack;
begin
  FScope := TDBIGlobalScopeStack.Create;

  Result := FScope;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/08/2006 17:57:54 - Initial code.<br>
}
destructor TDBIGenericMacroProcessor.Destroy;
begin
  FreeAndNil(FNullParam);
  FreeAndNil(FEvaluator);

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 18:05:09 - Initial code.<br>
}
function TDBIGenericMacroProcessor.FindParam(const ParamName: String): TParam;
begin
  Result := Scope.FindParam(ParamName);

  if Scope.Top.Enabled then begin
    if not Assigned(Result) and (Pos('.', ParamName) > 1) then begin
      Input.SyntaxError('Undeclared identifier "%s"', [ParamName]);
    end;
  end
  else begin
    Result := NullParam;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 19/12/2006 13:09:14 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetEvaluator: TDBIExpressionEvaluator;
begin
  if not Assigned(FEvaluator) then begin
    FEvaluator := TDBIExpressionEvaluator.Create('');
  end;
  Result := FEvaluator;
end;


// _____________________________________________________________________________
{**
  Jvr - 19/01/2006 13:58:37 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBIGenericMacroLexer;
end;


// _____________________________________________________________________________
{**
  Jvr - 06/06/2014 11:35:22 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetMemberComponent(const AName: String): TComponent;
begin
  Result := FOrigin.GetMemberComponent(AName);
end;


// _____________________________________________________________________________
{**
  Jvr - 06/06/2014 11:38:13 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetMemberComponentParams(const AName: String): TParams;
var
  Member: TObject;

begin
  Result := nil;
  Member := GetMemberComponent(ExtractObjectPath(AName));

  // Ok we have the Object, now for the property.
  // We assume it is an object property.
  if Assigned(Member) then begin
    Member := TypInfo.GetObjectProp(Member, ExtractPropertyName(AName));

    if Member is TParams then begin
      Result := Member as TParams;
    end;
  end
end;


// _____________________________________________________________________________
{**
  Jvr - 25/10/2007 21:45:08 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetParam(
  Sender: TObject;
  const ParamName: String;
  var ParamValue: Variant
  ): Boolean;
var
  Param: TParam;

begin
  Result := True;

  if Scope.Top.Enabled then begin
    Param := FindParam(ParamName);

    Result := Assigned(Param);
    if Result then begin
      ParamValue := Param.Value;
    end
  end
  else begin
    ParamValue := '';
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 18:02:23 - Initial code.<br>
}
procedure TDBIGenericMacroProcessor.Process;
begin
  Output.Text := '';

  // BootStrap the lexer;
  Input.Reset;

  while (Input.Token.TokenType <> Tok_Eof) do begin
    if not ({##JVR ProcessComments or} ProcessMacros or MacroExpand) then begin
      if Output.Enabled then begin
        Output.WriteStr(Input.Token.AsString);
      end;

      Input.NextToken;
    end;
  end;

end;


// _____________________________________________________________________________
{**
  Jvr - 18/01/2006 15:06:54 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessComments: Boolean;
begin
  Result := False;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/09/2006 15:33:05 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessDefine(
  const ParamType: TDBIStandardKeyword;
  const ExpectedTypes: TDBITokenTypes
  ): Boolean;
var
  Params: TDBIParamsAdapter;
  EOL: Boolean;

begin
  // Swallow Type ( #boolean, #define, #double, #integer, #short, #string )
  Input.Fetch([Tok_Identifier]);
  Input.Skip([tkWhiteSpace]);

  // Get Macro Name
  FMacroName := ProcessGetMacroName;
  Input.Skip([tkWhiteSpace]);

  // Now create the appropriate typed parameter
  FMacroValue := ProcessGetMacroValue(ExpectedTypes);

  // Do we remove whitespace at EOL
  EOL := Input.Have([Tok_BackSlash]);

  // Get Closing Bracket
  Input.Fetch([Tok_CloseCurlyBracket]);

  // Eat whitespace til EOL
  if EOL then begin
    Input.Skip([Tok_Space, Tok_HorizontalTab]);
    Input.Have([Tok_LineBreak]);
  end;

  if Scope.Top.Enabled then begin
    Params := Scope.Top.Params;

    case ParamType of
      Keyword_Boolean: begin
        Params.GetByName(FMacroName).AsBoolean := CompareText('True', FMacroValue) = 0;
      end;

      Keyword_DateTime: begin
        if (FMacroValue = '') then begin
          Params.GetByName(FMacroName).AsDateTime := Now;
        end
        else begin
          Params.GetByName(FMacroName).AsString := FMacroValue;
          Params.GetByName(FMacroName).AsDateTime := Params.GetByName(FMacroName).AsDateTime;
        end;
      end;

      Keyword_Double: begin
        if (FMacroValue = '') then begin
          Params.GetByName(FMacroName).AsFloat := 0;
        end
        else begin
          Evaluator.Expression := FMacroValue;
          Params.GetByName(FMacroName).AsFloat := Evaluator.Result;
        end;
      end;

      Keyword_Integer: begin
        if (FMacroValue = '') then begin
          Params.GetByName(FMacroName).AsInteger := 0;
        end
        else begin
          Evaluator.Expression := FMacroValue;
          Params.GetByName(FMacroName).AsInteger := Round(Evaluator.Result);
        end;
      end;

      Keyword_Short: begin
        if (FMacroValue = '') then begin
          Params.GetByName(FMacroName).AsSmallint := 0;
        end
        else begin
          Evaluator.Expression := FMacroValue;
          Params.GetByName(FMacroName).AsSmallint := Round(Evaluator.Result);
        end;
      end;

      Keyword_String: begin
        Params.GetByName(FMacroName).AsString := FMacroValue;
      end;

      else { Keyword_Define } begin
        Params.GetByName(FMacroName).AsString := FMacroValue;
      end;
    end;
  end;

  Result := True;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:53:26 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessElse: Boolean;
var
  EOL: Boolean;

begin
  // Swallow keyword 'Else', Whitespace and Closing Bracket
  Input.Fetch([Keyword_Else]);
  Input.Skip([tkWhiteSpace]);

  // Do we remove whitespace at EOL
  EOL := Input.Have([Tok_BackSlash]);

  // Get Closing Bracket
  Input.Fetch([Tok_CloseCurlyBracket]);

  // Eat whitespace til EOL
  if EOL then begin
    Input.Skip([Tok_Space, Tok_HorizontalTab]);
    Input.Have([Tok_LineBreak]);
  end;

  Result := Scope.Top.CheckType(TDBIIfThenElseScope);
  if Result then begin
    (Scope.Top as TDBIIfThenElseScope).State := msEnd;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 29/05/2007 17:51:48 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessEndFor: Boolean;
const
  Caller = 'ProcessEndFor';

var
  EnumeratorScope: TDBICustomEnumeratorScope;
  EOL: Boolean;

begin
  // Swallow keyword 'endfor', Whitespace and Closing Bracket
  Result := Input.Keyword = Keyword_EndFor;
  if Result then begin
    Input.Fetch([Tok_Identifier]);
    Input.Skip([tkWhiteSpace]);

    // Do we remove whitespace at EOL
    EOL := Input.Have([Tok_BackSlash]);

    // Get Closing Bracket
    Input.Fetch([Tok_CloseCurlyBracket]);

    // Eat whitespace til EOL
    if EOL then begin
      Input.Skip([Tok_Space, Tok_HorizontalTab]);
      Input.Have([Tok_LineBreak]);
    end;
  end;

  Result := Scope.Top is TDBICustomEnumeratorScope;
  if Result then begin
    EnumeratorScope := Scope.Top as TDBICustomEnumeratorScope;

    // If Items remaining then Restore Lexer position and state
    if EnumeratorScope.Enabled and not EnumeratorScope.Eof then begin
      EnumeratorScope.Next;

      Input.Restore(EnumeratorScope);
    end

    // Otherwise release scope
    else begin
      Scope.Pop;
    end;
  end

  // Otherwise, Wrong Scope.Top
  else begin
    raise Exception.CreateFmt(Caller + '::815::' + 'Expected Scope on Stack of type "%s", found "%s"',
      [TDBICustomEnumeratorScope.ClassName, Scope.Top.ClassName]
      );
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:53:55 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessEndIf: Boolean;
var
  EOL: Boolean;

begin
  // Swallow keyword 'endif', Whitespace and Closing Bracket
  Input.Fetch([Keyword_Endif]);
  Input.Skip([tkWhiteSpace]);

  // Do we remove whitespace at EOL
  EOL := Input.Have([Tok_BackSlash]);

  // Get Closing Bracket
  Input.Fetch([Tok_CloseCurlyBracket]);

  // Eat whitespace til EOL
  if EOL then begin
    Input.Skip([Tok_Space, Tok_HorizontalTab]);
    Input.Have([Tok_LineBreak]);
  end;

  Result := Scope.Top.CheckType(TDBIIfThenElseScope);
  if Result then begin
    Scope.Pop.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 11/08/2006 00:47:29 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessForEach: Boolean;
var
  EnumerableName: String;
  EnumeratorName: String;
  EnumeratorKeyName: String;
  EnumeratorScope: TDBICustomEnumeratorScope;
  Recursive: Boolean;
  EOL: Boolean;

begin
  EnumerableName := '';
  EnumeratorName := '';
  EnumeratorKeyName := '';

  // Swallow Keyword "foreach"
  Input.Fetch([Keyword_Foreach]);
  Input.Skip([tkWhiteSpace]);

  // Is it Recursive
  Recursive :=
    (Input.Token.TokenType = Tok_Identifier) and
    (CompareText(Input.Token.TokenString, 'recursive') = 0);
  if Recursive then begin
    Input.NextToken;
    Input.Skip([tkWhiteSpace]);
  end;

  // Get name of Enumerable
  EnumerableName := (Input as TDBIGenericMacroLexer).GetDotString(Scope);
  Input.Skip([tkWhiteSpace]);

  // Get "as" keyword
  Input.Fetch([Keyword_As]);
  Input.Skip([tkWhiteSpace]);

  // Get name of Enumerator  ##HACK
  EnumeratorName := Input.Fetch([Tok_Identifier]);
  if Input.Have([Tok_Dot]) then begin
    EnumeratorName := EnumeratorName + '.' + Input.Fetch([Tok_Identifier]);
  end;
  Input.Skip([tkWhiteSpace]);

  // Do we have a Key name? then reassign Key name and get Enumerator name
  if Input.Have([Tok_EqualGreater]) then begin
    EnumeratorKeyName := EnumeratorName;
    Input.Skip([tkWhiteSpace]);

    EnumeratorName := Input.Fetch([Tok_Identifier]);
    Input.Skip([tkWhiteSpace]);
  end;

  // Do we remove whitespace at EOL
  EOL := Input.Have([Tok_BackSlash]);

  // Get Closing Bracket
  Input.Fetch([Tok_CloseCurlyBracket]);

  // Eat whitespace til EOL
  if EOL then begin
    Input.Skip([Tok_Space, Tok_HorizontalTab]);
    Input.Have([Tok_LineBreak]);
  end;

  // Create and Push the scope Object onto the stack
  EnumeratorScope := Scope.Bind(EnumerableName, EnumeratorName, EnumeratorKeyName, Recursive) as TDBICustomEnumeratorScope;

  // Initialise the Enumerator Scope Object and start the enumeration
  Result := Assigned(EnumeratorScope);
  if Result then begin
    EnumeratorScope.Assign(Input.Token);
    EnumeratorScope.CheckState(Input.LexerData);

    if (EnumeratorScope.Count > 0) then begin
      EnumeratorScope.First;

      // Restore Lexer position and state
      Input.Restore(EnumeratorScope);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/10/2007 19:08:30 - Initial code.<br>
  Jvr - 01/11/2007 14:02:45 - Allow the macro/variable to be specified.<br>
}
function TDBIGenericMacroProcessor.ProcessGetMacroName: String;
const
  Caller = 'ProcessGetMacroName';

begin
  // Allow for macro variables, in the future we may insist on it!
  Result := Input.Fetch([Tok_Identifier, Tok_Macro]);
  try
    while (Input.Token.TokenType = Tok_Dot) do begin
      Input.NextToken;

      Result := Result + '.' + Input.Fetch([Tok_Identifier]);
      Input.NextToken;
    end;

  except
    on E: Exception do
      raise Exception.CreateFmt(Caller + '::950::' + 'Failed on ProcessGetMacroName, "%s"', [Result]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 12/10/2006 15:41:23 - Initial code.<br>

  Returns the 'typed' value for a defined macro upto the terminating curlybracket
}
function TDBIGenericMacroProcessor.ProcessGetMacroValue(
  const ExpectedTypes: TDBITokenTypes;
  const InvalidTokens: TDBITokenTypes
  ): String;
const
  Caller = 'ProcessGetMacroValue';

var
  InterimName: String;
  Param: TParam;

begin
  Result := '';
  InterimName := 'Undefined';

  try
    while not (Input.Token.TokenType in InvalidTokens) do begin
      // Deal with previously define macros
      if (Input.Token.TokenKind = tkMacro) then begin
        InterimName := Input.Token.TokenString;
        Param := FindParam(InterimName);
        if not Assigned(Param) then begin
          Result := Result + '${' + Input.Token.TokenString + '}';
          { TODO 4 -oJvr -cProcessGetMacroValue(() :
            When a macro value is null or empty,
            then we need to replace the empty value
            with the original text.

            What we have here is a temporary hack
            This currently does NOT work properly!

            Result := Result +
              Input.Tokens[Input.TokenIndex-1].AsString +
              Input.Token.AsString;

            03/03/2011 15:13:42
          }
        end
        else if not (Param.IsNull or (Param.AsString = '')) then begin
          Input.PutBack(Param.AsString);
        end;
      end

      // Otherwise get the literal value(s)
      else begin
        if (ExpectedTypes <> []) then begin
          Input.Check(ExpectedTypes);
        end;

        Result := Result + Input.Token.TokenString;
      end;
      Input.NextToken;
    end;

   except
    on E: Exception do
      raise Exception.CreateFmt(Caller + '::1100::' + 'Failed to get macro "%s"'#13'%s', [InterimName, E.Message]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:54:32 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessIf: Boolean;
const
  Caller = 'ProcessIf';

var
  ComparisonType: TDBITokenType;
  FloatValue: Double;
  StringValue: String;
  IfThenElse: TDBIIfThenElseScope;
  EOL: Boolean;
  Param: TParam;

begin
  Result := False;
  try
    // Swallow keyword 'If' / 'ElseIf'
    Input.Fetch([Keyword_If]);
    Input.Skip([tkWhiteSpace]);
    FMacroName := ProcessGetMacroName;

    Input.Skip([tkWhiteSpace]);
    ComparisonType := Input.Token.TokenType;
    if not Input.Have(Tok_Comparison) then begin
      ComparisonType := Tok_None;
    end;

    Input.Skip([tkWhiteSpace]);

    Param := FindParam(FMacroName);
    if Assigned(Param) then begin
      if (ComparisonType = Tok_None) then begin //process as if it's a boolean
        Result := CompareText(Param.AsString, 'True') = 0;
      end
      else if (Input.Token.TokenType = Tok_SingleQuote) then begin //process as if it's a string
        Input.Fetch([Tok_SingleQuote]);
        StringValue := ProcessGetMacroValue(
          [],
          [Tok_OpenCurlyBracket, Tok_CloseCurlyBracket, Tok_SingleQuote, Tok_Eof]
          );
        Input.Fetch([Tok_SingleQuote]);

        case ComparisonType of
          Tok_Smaller:      Result := CompareText(Param.AsString, StringValue) < 0;
          Tok_Greater:      Result := CompareText(Param.AsString, StringValue) > 0;
          Tok_GreaterEqual: Result := CompareText(Param.AsString, StringValue) >= 0;
          Tok_Equality:     Result := CompareText(Param.AsString, StringValue) = 0;
          Tok_InEquality:   Result := CompareText(Param.AsString, StringValue) <> 0;
          Tok_NotEqual:     Result := CompareText(Param.AsString, StringValue) <> 0;
          Tok_SmallerEqual: Result := CompareText(Param.AsString, StringValue) <= 0;
        end;
      end
      else begin //process as if it's a numeric
        Evaluator.Expression := ProcessGetMacroValue([]);
        FloatValue := Evaluator.Result;

        case ComparisonType of
          Tok_Smaller:      Result := Param.AsFloat < FloatValue;
          Tok_Greater:      Result := Param.AsFloat > FloatValue;
          Tok_GreaterEqual: Result := Param.AsFloat >= FloatValue;
          Tok_Equality:     Result := Param.AsFloat = FloatValue;
          Tok_InEquality:   Result := Param.AsFloat <> FloatValue;
          Tok_NotEqual:     Result := Param.AsFloat <> FloatValue;
          Tok_SmallerEqual: Result := Param.AsFloat <= FloatValue;
        end;
      end;
    end;

    // Do we remove whitespace at EOL
    EOL := Input.Have([Tok_BackSlash]);

    // Get Closing Bracket
    Input.Fetch([Tok_CloseCurlyBracket]);

    // Eat whitespace til EOL
    if EOL then begin
      Input.Skip([Tok_Space, Tok_HorizontalTab]);
      Input.Have([Tok_LineBreak]);
    end;

  except
    on E: Exception do
      raise Exception.CreateFmt(Caller + '::1105::' + 'Failed on ProcessIf, "%s"'#13'%s', [FMacroName, E.Message]);
  end;

  // Check the Parent Scope to make sure it is enabled
  IfThenElse := Scope.Push(TDBIIfThenElseScope.Create) as TDBIIfThenElseScope;
  IfThenElse.Condition := Result;
  IfThenElse.State := msIf;
  IfThenElse.TokenString := FMacroName;
end;


// _____________________________________________________________________________
{**
  Jvr - 19/03/2008 08:07:54 - Initial code.<br />
}
function TDBIGenericMacroProcessor.ProcessMacros: Boolean;
const
  Caller = 'ProcessMacros';

begin
  // Swallow the Leadin {#
  if Input.Have([Tok_OpenCurlyBracket_Hash]) then begin
    Input.Skip([tkWhiteSpace]);
  end;

  Result := (tsMacro in Input.Token.TokenStatus);
  if Result then begin
    case Input.Keyword of
      Keyword_Define: Result := ProcessDefine(Input.Keyword, []);
      Keyword_ForEach: Result := ProcessForEach;
      Keyword_Else: Result := ProcessElse;
      Keyword_EndFor: Result := ProcessEndFor;
      Keyword_EndIf: Result := ProcessEndIf;
      Keyword_If: Result := ProcessIf;

    else
      Result := False;
      Input.SyntaxError('Unexpected token type "%s"', [Input.Token.TokenName], Self, Caller);
    end;
  end;
end;





{ TDBICustomScopedMacroProcessor }

type
  TDBIProtectedStreamFormatter = class(TDBIStreamFormatter);

constructor TDBICustomScopedMacroProcessor.Create;
begin
  inherited Create;

  FScope := CreateScope;

  // Register Callback to ensure that the output.enabled property
  // correctly reflects the scope.enabled;
  TDBIProtectedStreamFormatter(Output).EnabledCallback := EnabledHandler;
end;


// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:14:12 - Initial code.<br />
}
function TDBICustomScopedMacroProcessor.CreateScope: TDBIScopeStack;
begin
  Result := TDBIScopeStack.Create;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:14:08 - Initial code.<br>
}
destructor TDBICustomScopedMacroProcessor.Destroy;
begin
  FreeAndNil(FScope);

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/01/2011 12:28:05 - Initial code.<br />
}
procedure TDBICustomScopedMacroProcessor.EnabledHandler(Sender: TObject; var Enabled: Boolean);
begin
  if Enabled and Assigned(GetScope.Top) then begin
    Enabled := GetScope.Top.Enabled;
  end;
end;


function TDBICustomScopedMacroProcessor.GetScope: TDBIScopeStack;
begin
  Result := FScope;
end;





{ TDBIObjectListDatasetEnumeratorScope }

function TDBIObjectListDatasetEnumeratorScope.GetCursor: TDataset;
begin
  if not Assigned(FCursor) then begin
    Assert(Data is TObjectListDataset);

    FCursor := TObjectListDataset.Create(nil);
    TObjectListDataset(FCursor).CloneCursor(Data as TObjectListDataset, False, False);
  end;
  Result := FCursor;
end;





{ TDBIClientDatasetEnumeratorScope }
{$ifndef fpc}
function TDBIClientDatasetEnumeratorScope.GetCursor: TDataset;
begin
  if not Assigned(FCursor) then begin
    Assert(Data is TClientDataset);

    FCursor := TClientDataset.Create(nil);
    TClientDataset(FCursor).CloneCursor(Data as TClientDataset, False, False);
  end;
  Result := FCursor;
end;
{$endif}




{ TDBICustomDatasetEnumeratorScope }

destructor TDBICustomDatasetEnumeratorScope.Destroy;
begin
  FreeAndNil(FCursor);

  inherited Destroy;
end;


procedure TDBICustomDatasetEnumeratorScope.First;
begin
  GetCursor.First;

  inherited First;
end;


function TDBICustomDatasetEnumeratorScope.GetCount: Integer;
begin
  Result := GetCursor.RecordCount;
end;


function TDBICustomDatasetEnumeratorScope.GetCursor: TDataset;
begin
  Result := Data as TDataset;
end;


procedure TDBICustomDatasetEnumeratorScope.Last;
begin
  GetCursor.Last;

  inherited Last;
end;


function TDBICustomDatasetEnumeratorScope.Next: Boolean;
begin
  Result := ItemIndex < Count;
  if Result then begin
    GetCursor.Next;

    inherited Next;
  end;
end;


procedure TDBICustomDatasetEnumeratorScope.UpdateParams;
begin
  Params.GetByName(ItemName + paramHasNext).AsBoolean := ItemIndex < (Count - 1);

  if (KeyName <> '') then begin
    Params.GetByName(KeyName).AsInteger := ItemIndex;
  end;

  Params.AssignFromFields(GetCursor.Fields, ItemName);
end;





{ TDBIComponentTraversalScope }

destructor TDBIComponentTraversalScope.Destroy;
begin
  FreeAndNil(FList);

  inherited Destroy;
end;


function TDBIComponentTraversalScope.GetCount: Integer;
begin
  Result := GetList.Count;
end;


function TDBIComponentTraversalScope.GetCurrent: TObject;
begin
  Result := GetList.Items[ItemIndex];
end;


function TDBIComponentTraversalScope.GetList: TList;

  // Head Recursion
  procedure Iterate(Parent: TDBICompoundComponent; const Level: Word);
  var
    Index: Integer;
    Child: TDBICompoundComponent;

  begin
    for Index := 0 to Parent.ChildCount-1 do begin
      Child := Parent.Children[Index];
      Child.Tag := Level;

      FList.Add(Child);
      Iterate(Child, Level + 1);
    end;
  end;

begin
  if not Assigned(FList) then begin
    FList := TList.Create;

    Iterate(Data as TDBICompoundComponent, 0);
  end;
  Result := FList;
end;


procedure TDBIComponentTraversalScope.UpdateParams;
var
  Item: TDBICompoundComponent;

begin
  inherited UpdateParams;

  if ItemIndex < Count then begin
    Item := GetCurrent as TDBICompoundComponent;

    Params.GetByName(ItemName + paramLevel).AsInteger := Item.Tag;
  end;
end;






{ TDBIComponentEnumeratorScope}

// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 12:48:54 - Initial code.<br />
}
function TDBIComponentEnumeratorScope.GetCount: Integer;
begin
  Result := (Data as TDBICompoundComponent).ChildCount;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 12:53:36 - Initial code.<br />
}
function TDBIComponentEnumeratorScope.GetCurrent: TObject;
begin
  Result := (Data as TDBICompoundComponent).Children[ItemIndex];
end;


// _____________________________________________________________________________
{**
  Jvr - 07/06/2007 14:08:36 - Initial code.<br>
}
procedure TDBIComponentEnumeratorScope.UpdateParams;
var
  Item: TDBICompoundComponent;

  function GetCaption: String;
  const
    PropName = 'Caption';

  var
    PropInfo: PPropInfo;

  begin
    PropInfo := GetPropInfo(Item, PropName);
    if Assigned(PropInfo) then begin
      Result := TypInfo.GetStrProp(Item, PropInfo);
    end
    else begin
      Result := Item.Name;
    end;
  end;

begin
  inherited UpdateParams;

  // If Items remaining then Update the Scoped Parameters for the current item
  if ItemIndex < Count then begin
    Item := GetCurrent as TDBICompoundComponent;

    Params.GetByName(ItemName + paramItemName).AsString := Item.Name;
    Params.GetByName(ItemName + paramClassName).AsString := Item.ClassName;
    Params.GetByName(ItemName + paramCaption).AsString := GetCaption;
    Params.GetByName(ItemName + paramPathName).AsString := Item.CompoundName;

    if (Item.Parent is TDBICompoundComponent) then begin
      Item := Item.Parent as TDBICompoundComponent;

      Params.GetByName(ItemName + paramParentClassName).AsString := Item.ClassName;
      Params.GetByName(ItemName + paramParentID).AsInteger := Item.ComponentIndex;
      Params.GetByName(ItemName + paramParentCaption).AsString := GetCaption;
//##PARAM      Params.GetByName(ItemName + paramParentName).AsString := Item.Name;
//##PARAM      Params.GetByName(ItemName + paramParentPathName).AsString := Item.CompoundName;
    end;
  end;
end;





{ TDBIParamsEnumeratorScope }

procedure TDBIParamsEnumeratorScope.UpdateParams;
var
  Param: TParam;

begin
  inherited UpdateParams;

  if ItemIndex < Count then begin
    Param := GetCurrent as TParam;

    Params.GetByName(ItemName + paramValue).Value := Param.Value;
    Params.GetByName(ItemName + paramDataType).Value := DataTypeName(Param.DataType);
  end;
end;





{ TDBICollectionEnumeratorScope }

function TDBICollectionEnumeratorScope.GetCount: Integer;
begin
  Result := (Data as TCollection).Count
end;


function TDBICollectionEnumeratorScope.GetCurrent: TObject;
begin
  Result := (Data as TCollection).Items[ItemIndex];
end;


procedure TDBICollectionEnumeratorScope.UpdateParams;
var
  Item: TCollectionItem;

begin
  inherited UpdateParams;

  // If Items remaining then Update the Scoped Parameters for the current item
  if ItemIndex < Count then begin
    Item := GetCurrent as TCollectionItem;

    Params.GetByName(ItemName + paramItemName).AsString := Item.DisplayName;
    Params.GetByName(ItemName + paramPathName).AsString := Item.GetNamePath;
  end;
end;





{ TDBIStringsEnumeratorScope }

function TDBIStringsEnumeratorScope.GetCount: Integer;
begin
  Result := GetStrings.Count;
end;

function TDBIStringsEnumeratorScope.GetStrings: TStrings;
begin
  Result := (Data as TStrings);
end;

function TDBIStringsEnumeratorScope.GetDataType: String;
begin
  Result := DataTypeName(ftString);
end;

function TDBIStringsEnumeratorScope.GetName: String;
begin
  Result := GetStrings.Names[ItemIndex];
end;

function TDBIStringsEnumeratorScope.GetValue: String;
var
  Offset: Integer;

begin
  Result := GetStrings.Strings[ItemIndex];
  Offset := AnsiPos('=', Result);
  if (Offset > 0) then begin
    System.Delete(Result, 1, Offset);
  end;
end;





{ TDBICustomEnumeratorScope }

constructor TDBICustomEnumeratorScope.Create(AData: TObject);
begin
  inherited Create(AData);

  //##DEBUG
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2009 07:30:09 - Initial code.<br />
}
destructor TDBICustomEnumeratorScope.Destroy;
begin
  //##DEBUG

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2009 07:56:05 - Initial code.<br />
}
procedure TDBICustomEnumeratorScope.First;
begin
  FItemIndex := 0;
  UpdateParams;
end;


function TDBICustomEnumeratorScope.GetBof: Boolean;
begin
  Result := ItemIndex <= 0;
end;


function TDBICustomEnumeratorScope.GetCount: Integer;
begin
  Result := 0;
end;


function TDBICustomEnumeratorScope.GetCurrent: TObject;
begin
  Result := nil;
end;


// _____________________________________________________________________________
{**
  Jvr - 05/04/2011 07:19:41 - Initial code.<br />
}
function TDBICustomEnumeratorScope.GetEnabled: Boolean;
begin
  Result := (Count > 0) and (ItemIndex < Count);
end;


// _____________________________________________________________________________
{**
  Jvr - 08/08/2006 17:57:54 - Initial code.<br>
}
function TDBICustomEnumeratorScope.GetEof: Boolean;
begin
  Result := ItemIndex >= (Count - 1);
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2009 08:32:56 - Initial code.<br />
}
function TDBICustomEnumeratorScope.GetItemIndex: Integer;
begin
  if Count > 0 then begin
    Result := FItemIndex;
  end
  else begin
    Result := -1;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 13:53:11 - Initial code.<br />
}
function TDBICustomEnumeratorScope.GetKeyName: String;
begin
  Result := FKeyName;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 12:57:12 - Initial code.<br />
}
procedure TDBICustomEnumeratorScope.Last;
begin
  FItemIndex := Count-1;
  UpdateParams;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 12:46:28 - Initial code.<br />
}
function TDBICustomEnumeratorScope.Next: Boolean;
begin
  Result := ItemIndex < (Count-1);
  if Result then begin
    Inc(FItemIndex);
    UpdateParams;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 29/05/2007 18:11:51 - Initial code.<br>
}
procedure TDBICustomEnumeratorScope.SetKeyName(const Value: String);
begin
  FKeyName := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 03/02/2005 17:12:45 - Initial code.<br>
}
procedure TDBICustomEnumeratorScope.UpdateParams;
begin
  inherited UpdateParams;

  Params.GetByName(ItemName + paramHasNext).AsBoolean := ItemIndex < (Count - 1);

  if (KeyName <> '') then begin
    Params.GetByName(KeyName).AsInteger := ItemIndex;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/10/2009 08:58:43 - Initial code.<br />
}
procedure TDBICustomEnumeratorScope.UpdateParentParams(AParams: TDBIParamsAdapter);
begin
  if Assigned(AParams) then begin
    AParams.GetByName(Name + paramMin).AsInteger := 0;
    AParams.GetByName(Name + paramMax).AsInteger := Count-1;
    AParams.GetByName(Name + paramCount).AsInteger := Count;
  end;
end;





{ TDBIIfThenElseScope }

function TDBIIfThenElseScope.GetEnabled: Boolean;
begin
  Result := True;

  if Assigned(Parent) then begin
    Result := Parent.Enabled;
  end;

  if Result then begin
    if Condition then begin
      Result := State = msIf;
    end
    else begin
      Result := State <> msIf;
    end;
  end;
end;





{ TDBIGlobalScopeStack }

procedure TDBIGlobalScopeStack.AddBinding(
  const ScopeName: String;
  const ScopeClass: TDBIScopeClass;
  const ScopeData: TObject
  );
var
  GlobalParams: TDBIParamsAdapter;
  Scope: TDBICustomScope;

begin
  Scope := ScopeClass.Create(ScopeData);
  Scope.Name := ScopeName;

  Bindings.AddObject(ScopeName, Scope);

  GlobalParams := Global.Params;  // Instantiate
  if (Scope is TDBICustomEnumeratorScope) then begin
    (Scope as TDBICustomEnumeratorScope).UpdateParentParams(GlobalParams);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 18:02:23 - Initial code.<br>
}
function TDBIGlobalScopeStack.Bind(
  const DataBindingName: String;
  const EnumeratorName: String;
  const KeyName: String;
  const Recursive: Boolean = False
  ): TDBICustomScope;
var
  ItemIndex: Integer;
  Enumerator: TDBICustomEnumeratorScope;

begin
  ItemIndex := Bindings.IndexOf(DataBindingName);
  if (ItemIndex >= 0) then begin
    Enumerator := Push(Bindings.Objects[ItemIndex] as TDBICustomScope) as TDBICustomEnumeratorScope;
    Enumerator.Name := DataBindingName;
    Enumerator.ItemName := EnumeratorName;
    Enumerator.KeyName := KeyName;

    Result := Enumerator;
  end
  else begin
    raise Exception.CreateFmt(
      'Undeclared databinding "%s" for Enumerator "%s"',
      [DataBindingName, EnumeratorName]
      );
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 19/01/2006 13:50:34 - Initial code.<br>
}
destructor TDBIGlobalScopeStack.Destroy;
begin
  ReleaseBindings;
  FreeAndNil(FBindings);

  inherited Destroy;
end;


function TDBIGlobalScopeStack.GetBindings: TDBICustomBindingsList;
begin
  if not Assigned(FBindings) then begin
    FBindings := TDBICustomBindingsList.Create;
    FBindings.Duplicates := dupError;
    FBindings.Sorted := True;
  end;
  Result := FBindings;
end;


type
  TDBIProtectedScope = class(TDBICustomGlobalScope);

function TDBIGlobalScopeStack.GetGlobal: TDBICustomScope;
var
  Index: Integer;
  Scope: TDBIProtectedScope;

begin
  if Items.Count <= 0 then begin
    for Index := 0 to Bindings.Count-1 do begin
      if Bindings.Objects[Index] is TDBICustomGlobalScope then begin
        Scope := TDBIProtectedScope(Bindings.Objects[Index]);
        Push(Scope);
        Scope.UpdateParams;
      end;
    end;
  end;

  Result := inherited GetGlobal;
end;


procedure TDBIGlobalScopeStack.ReleaseBindings;
var
  Index: Integer;
  Scope: TDBICustomScope;

begin
  // Remove Global Binding Objects from Scope
  for Index := Items.Count-1 downto 0 do begin
    if (TObject(Items[Index]) is TDBICustomScope) then begin
      Scope := TDBICustomScope(Items[Index]);
      if Bindings.IndexOfObject(Scope) >= 0 then begin
        Items.Remove(Scope);
      end;
    end;
  end;

  // Remove all Binding objects from Bindings and Free them
  if Assigned(FBindings) and (FBindings.Count > 0) then begin
    FBindings.Changing;
    try
      for Index := FBindings.Count-1 downto 0 do begin
        FBindings.Objects[Index].Free;
      end;
      FBindings.Clear;
    finally
      FBindings.Changed;
    end;
  end;
end;





{ TDBIMacroLexer }

function TDBIGenericMacroLexer.Check(Keywords: TDBIStandardKeywords): String;
begin
  Result := Token.TokenString;
  if not (Keyword in Keywords) then begin
    SyntaxError('Unexpected Keyword "%s"', [Token.TokenString]);
  end;
end;


function TDBIGenericMacroLexer.Fetch(Keywords: TDBIStandardKeywords): String;
begin
  Result := Token.TokenString;
  Check(Keywords);
  NextToken;
end;


// _____________________________________________________________________________
{**
  Jvr - 11/08/2006 00:47:29 - Initial code.<br>
}
function TDBIGenericMacroLexer.GetDotString(Scope: TDBIScopeStack): String;
var
  InterimName: String;
  Param: TParam;
  ValidTokens: TDBITokenTypes;

begin
  Result := '';
  InterimName := 'Undefined';
  ValidTokens := [Tok_Identifier, Tok_Dot, Tok_Macro];

  try
    while (Token.TokenType in ValidTokens) do begin
      // Deal with previously define macros
      if (Token.TokenKind = tkMacro) then begin
        InterimName := Token.TokenString;

        Param := Scope.FindParam(InterimName);
        if not Assigned(Param) then begin
          Result := Result + '${' + Token.TokenString + '}';
          { TODO 4 -oJvr -cProcessGetMacroValue(() :
            03/03/2011 15:13:42

            When a macro value is null or empty,
            then we need to replace the empty value
            with the original text.

            The current implementation is a temporary hack
          }
        end
        else if not (Param.IsNull or (Param.AsString = '')) then begin
          PutBack(Param.AsString);
        end;
      end

      // Otherwise get the literal value(s)
      else begin
        Result := Result + Token.TokenString;
      end;

      NextToken;

      // Swallow the close-curly bracket if there is one
      if Token.TokenType = Tok_CloseCurlyBracket then begin
        NextToken;
      end;
    end;

   except
    on E: Exception do
      SyntaxError('Failed to get macro "%s"', [InterimName]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/08/2006 17:57:54 - Initial code.<br>
}
function TDBIGenericMacroLexer.GetKeyword: TDBIStandardKeyword;
var
  Index: Integer;

begin
  Index := GetEnumValue(TypeInfo(TDBIStandardKeyword), 'Keyword_' + Token.TokenString);
  if (Index <> -1) then begin
    Result := TDBIStandardKeyword(Index);
  end
  else begin
    Result := Keyword_Unknown;
  end;
end;


function TDBIGenericMacroLexer.Have(Keywords: TDBIStandardKeywords): Boolean;
begin
  Result := (Keyword in Keywords);
  if Result then begin;
    NextToken;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 11/12/2008 09:41:44 - Initial code.<br>
}
procedure TDBIGenericMacroLexer.LexEscape;
const
  EscapeTab = '~';

begin
  if (Length(Token.AsString) > 1) then begin
    if (Token.AsString[2] = EscapeTab) then
      Token.AsString := #9
    else
      Token.AsString := Token.AsString[2];
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/02/2005 17:58:33 - Initial code.<br>
}
procedure TDBIGenericMacroLexer.LexInitialise;
begin
  inherited LexInitialise;

  // Standard operators
  MapDualSymbol(LexNone, Chr_GreaterEqual, Tok_GreaterEqual, tkSymbol);
  MapDualSymbol(LexNone, Chr_SmallerEqual, Tok_SmallerEqual, tkSymbol);
  MapDualSymbol(LexNone, Chr_Equality, Tok_Equality, tkSymbol);
  MapDualSymbol(LexNone, Chr_InEquality, Tok_InEquality, tkSymbol);
  MapDualSymbol(LexNone, Chr_NotEqual, Tok_NotEqual, tkSymbol);

  // foreach
  MapDualSymbol(LexNone, Chr_EqualGreater, Tok_EqualGreater, tkSymbol);

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


function TDBIGenericMacroLexer.Skip(Keywords: TDBIStandardKeywords): String;
begin
  Result := '';
  while (Keyword in Keywords) and not Eof do begin
    Result := Result + Token.TokenString;
    NextToken;
  end;
end;


function TDBIGenericMacroLexer.Upto(TokenTypes: TDBITokenTypes; const Inclusive: Boolean = False): String;
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

    inherited NextToken;
//##JVR    UptoToken;
  end;
end;


function TDBIGenericMacroLexer.Upto(TokenKinds: TDBITokenKinds; const Inclusive: Boolean = False): String;
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

    inherited NextToken;
//##JVR    UptoToken;
  end;
end;


procedure TDBIGenericMacroLexer.UptoToken;
const
  Tokens_Comment1 = [Tok_OpenCurlyBracket, Tok_CloseCurlyBracket];
  Tokens_Comment3 = [Tok_OpenBracket_Star, Tok_CloseBracket_Star];

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





{ TDBIScopeStack }

// _____________________________________________________________________________
{**
  Jvr - 04/04/2011 09:21:51 - Initial code.<br />
}
procedure TDBIScopeStack.Clear;
var
  Scope: TDBICustomScope;

begin
  if Assigned(FItems) then begin
    while FItems.Count > 0 do begin
      Scope := Items[FItems.Count-1];
      FItems.Remove(Scope);
      FreeAndNil(Scope);
    end;
  end;
end;


destructor TDBIScopeStack.Destroy;
begin
  Clear;
  FreeAndNil(FItems);

  inherited Destroy;
end;


function TDBIScopeStack.FindParam(const ParamName: String): TParam;
var
  Index: Integer;
  Scope: TDBICustomScope;

begin
  Result := nil;

  for Index := Items.Count-1 downto 0 do begin
    Scope := Items[Index];

    if Assigned(Scope) and Assigned(Scope.Params) then begin
      Result := Scope.GetParams.FindParam(ParamName);
    end;

    if Assigned(Result) then begin
      Break;
    end;
  end;
end;


function TDBIScopeStack.GetGlobal: TDBICustomScope;
begin
  if Items.Count <= 0 then begin
    Push(TDBIGlobalScope.Create);
  end;
  Result := nil;
  if Items.Count > 0 then begin
    Result := Items[0];
  end;
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





{ TDBIGlobalScope }

function TDBIGlobalScope.GetComputerName: String;
begin
  Result := TDBIHostInfo.GetComputerName;
end;

function TDBIGlobalScope.GetDate: String;
begin
  Result := FormatDateTime('dd/mm/yyyy', SysUtils.Now);
end;

function TDBIGlobalScope.GetDateStamp: String;
begin
  Result := FormatDateTime('mm/dd/yyyy', SysUtils.Now);
end;

function TDBIGlobalScope.GetDay: String;
begin
  Result := FormatDateTime('dd', SysUtils.Now);
end;

function TDBIGlobalScope.GetLongDay: String;
begin
  Result := FormatDateTime('dddd', SysUtils.Now);
end;

function TDBIGlobalScope.GetLongMonth: String;
begin
  Result := FormatDateTime('mmmm', SysUtils.Now);
end;

function TDBIGlobalScope.GetMonth: String;
begin
  Result := FormatDateTime('mm', SysUtils.Now);
end;

function TDBIGlobalScope.GetNow: TDateTime;
begin
  Result := SysUtils.Now;
end;

function TDBIGlobalScope.GetShortDay: String;
begin
  Result := FormatDateTime('ddd', SysUtils.Now);
end;

function TDBIGlobalScope.GetShortMonth: String;
begin
  Result := FormatDateTime('mmm', SysUtils.Now);
end;

function TDBIGlobalScope.GetTime: String;
begin
  Result := FormatDateTime('hh:nn:ss', SysUtils.Now);
end;

function TDBIGlobalScope.GetTimeStamp: String;
begin
  Result := FormatDateTime('mm/dd/yyyy hh:nn:ss:zzz', SysUtils.Now);
end;

function TDBIGlobalScope.GetToday: String;
begin
  Result := FormatDateTime('dd/mm/yyyy', SysUtils.Now);
end;

function TDBIGlobalScope.GetUserName: String;
begin
  Result := TDBIHostInfo.GetUserName;
end;

function TDBIGlobalScope.GetYear: String;
begin
  Result := FormatDateTime('yyyy', SysUtils.Now);
end;





{ TDBICustomGlobalScope }

function TDBICustomGlobalScope.GetEnabled: Boolean;
begin
  Result := True;
end;

function TDBICustomGlobalScope.GetItemName: String;
begin
  Result := GetName;
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

    raise Exception.CreateFmt(
      'Expected Scope of type "%s", actual scope is %s',
      [AClassType.ClassName, ScopeName]
      );
  end;
end;


constructor TDBICustomScope.Create(AData: TObject);
begin
  inherited Create;

  FData := AData;
  FParams := TDBIParamsAdapter.Create;
end;


destructor TDBICustomScope.Destroy;
begin
  FreeAndNil(FParams);

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 31/03/2011 12:44:04 - Initial code.<br />
}
function TDBICustomScope.GetEnabled: Boolean;
begin
  Result := Assigned(FData) and Assigned(FCallBack);
end;


function TDBICustomScope.GetItemName: String;
begin
  Result := FItemName;
end;


function TDBICustomScope.GetName: String;
begin
  Result := FName;
end;


function TDBICustomScope.GetParams: TDBIParamsAdapter;
begin
  Result := FParams;
end;


procedure TDBICustomScope.SetItemName(const value: String);
begin
  FItemName := Value;
end;


procedure TDBICustomScope.SetName(const Value: String);
begin
  FName := Value;
end;


procedure TDBICustomScope.UpdateParams;
begin
  Params.AssignFromProperties(Self, ItemName);
end;



end.

