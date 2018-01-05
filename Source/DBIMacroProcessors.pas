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
  Classes, SysUtils, DBITypInfo, DB, DBIConst, DBIUtils, DBIComponents, DBIStrings,
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
    FScopeName: String;
    FParams: TDBIParamsAdapter;
    FParent: TDBICustomScope;

  protected
    function GetEnabled: Boolean; virtual;
    function GetItemName: String; virtual;
    function GetScopeName: String; virtual;
    function GetParams: TDBIParamsAdapter; virtual;

    procedure SetItemName(const value: String); virtual;
    procedure SetScopeName(const Value: String); virtual;

    property CallBack: TDBICustomScopeCallback read FCallback write FCallback;
    property Data: TObject read FData write FData;
    property Parent: TDBICustomScope read FParent write FParent;

  public
    constructor Create(AData: TObject = nil); virtual;
    destructor Destroy; override;

    function CheckType(AClassType: TDBICustomScopeClass): Boolean;

    property Enabled: Boolean read GetEnabled;
    property ItemName: String read GetItemName write SetItemName;
    property Params: TDBIParamsAdapter read GetParams;
    property ScopeName: String read GetScopeName write SetScopeName;

  end;
  TDBIScopeClass = class of TDBICustomScope;


type
  TDBICompoundScope = class;

  TDBIScopeItems = class(TList)
  protected
    function ExtractNamedScope(const CompoundName: String; out BaseName, ScopeName: String): Boolean;
    class function IndexOfChar(const Character: Char; const Text: String): Word;

  public
    function GetScope(CompoundName: String): TDBICompoundScope; virtual;
  end;

  TDBICompoundScope = class(TDBICustomScope)
  private
    FScopeItems: TDBIScopeItems;

  protected
    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean; virtual;

    function GetItemCount: Integer;
    function GetItems: TDBIScopeItems;

    property ItemCount: Integer read GetItemCount;
    property Items: TDBIScopeItems read GetItems;

  public
    destructor Destroy; override;

  end;


type
  TDBICustomGlobalScope = class(TDBICompoundScope)
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
    FItems: TDBIScopeItems;

  protected
    procedure Clear;

    function GetGlobal: TDBICompoundScope; virtual;
    function GetItems: TDBIScopeItems;
    function GetTop: TDBICustomScope;

    property Global: TDBICompoundScope read GetGlobal;

  public
    destructor Destroy; override;

    function FindParam(const ParamName: String): TParam;
    function Push(Item: TDBICustomScope): TDBICustomScope;
    function Pop: TDBICustomScope;

    property Items: TDBIScopeItems read GetItems;
    property Top: TDBICustomScope read GetTop;
  end;

type
  TDBICustomBindingsList = class(TDBICustomStringList);

  TDBIGlobalScopeStack = class(TDBIScopeStack)
  private
    FBindings: TDBICustomBindingsList;

  protected
    function GetBindings: TDBICustomBindingsList;
    function GetGlobal: TDBICompoundScope; override;

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

    //##JVR Temporary Hack
    property Global;
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
  TDBICustomEnumeratorScope = class(TDBICompoundScope)
  private
    FItemIndex: Integer;
    FKeyName: String;

  protected
    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean; override;
    function GetBof: Boolean; virtual;
    function GetCount: Integer; virtual;
    function GetCurrent: TObject; virtual;
    function GetEnabled: Boolean; override;
    function GetEof: Boolean; virtual;
    function GetItemIndex: Integer; virtual;
    function GetKeyName: String; virtual;
    function GetMinimum: Integer; virtual;
    function GetMaximum: Integer; virtual;

    procedure SetKeyName(const Value: String); virtual;

    procedure First; virtual;
    function Next: Boolean; virtual;
    procedure Last; virtual;

    property Bof: Boolean read GetBof;
    property Eof: Boolean read GetEof;
    property ItemIndex: Integer read GetItemIndex;
    property KeyName: String read GetKeyName write SetKeyName;

  public
    constructor Create(AData: TObject); override;
    destructor Destroy; override;

  published
    property Count: Integer read GetCount;
    property Min: Integer read GetMinimum;
    property Max: Integer read GetMaximum;

  end;


  TDBINullEnumeratorScope = class(TDBICustomEnumeratorScope)
  end;


type
  TDBIStringsEnumeratorScope = class(TDBICustomEnumeratorScope)
  protected
    function GetCount: Integer; override;
    function GetDataType: String;
    function GetName: String;
    function GetStrings: TStrings; virtual;
    function GetValue: String;

  published
    property DataType: String read GetDataType;
    property Name: String read GetName;
    property Value: String read GetValue;

  end;


type
  TDBICollectionEnumeratorScope = class(TDBICustomEnumeratorScope)
  protected
    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean; override;
    function GetCount: Integer; override;
    function GetCurrent: TObject; override;

  end;


  TDBIParamsEnumeratorScope = class(TDBICollectionEnumeratorScope)
  protected
    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean; override;

  end;


type
  TDBIComponentEnumeratorScope = class(TDBICustomEnumeratorScope)
  protected
    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean; override;
    function GetCount: Integer; override;
    function GetCurrent: TObject; override;

  end;


  TDBIComponentTraversalScope = class(TDBIComponentEnumeratorScope)
  private
    FList: TList;

  protected
    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean; override;
    function GetCount: Integer; override;
    function GetList: TList;
    function GetCurrent: TObject; override;

  public
    destructor Destroy; override;

  end;


type
  TDBICustomDatasetEnumeratorScope = class(TDBICustomEnumeratorScope)
  private
    FCursor: TDataset;

  protected
    procedure First; override;

    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean; override;
    function GetCount: Integer; override;
    function GetCursor: TDataset; virtual;

    procedure Last; override;
    function Next: Boolean; override;

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
    Keyword_IfNot,
    Keyword_Integer,
    Keyword_Short,
    Keyword_Slice,
    Keyword_String,
    Keyword_LowerCase,
    Keyword_UpperCase,
    Keyword_CamelCase,
    Keyword_EOL
    );
  TDBIStandardKeywords = set of TDBIStandardKeyword;

  TDBIGenericMacroLexer = class(TDBICustomMacroLexer)
  protected
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
    FNullParam: TParam;
    FOnInclude: TDBIMacroProcessorIncludeEvent;
    FOrigin: TDBICompoundComponent;
    FScope: TDBIGlobalScopeStack;

  protected
    function CreateScope: TDBIScopeStack; override;

    function GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
    function GetParamValue(const ParamName: String; var ParamValue: Variant): Boolean;

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
    function ProcessEOL: Boolean;
    procedure ProcessMacroEnd;

    function ProcessGetQualifiedIdentifier: String;
    function ProcessGetMacroName: String;
    function ProcessGetMacroValue(
      const ExpectedTypes: TDBITokenTypes;
      const InvalidTokens: TDBITokenTypes = [Tok_BackSlash, Tok_OpenCurlyBracket, Tok_CloseCurlyBracket, Tok_Eof]
      ): String;

    function ProcessIf(const ParamType: TDBIStandardKeyword): Boolean;
    function ProcessSlice(const Keyword: TDBIStandardKeyword): Boolean;
    function ProcessStringConversion(const Keyword: TDBIStandardKeyword): Boolean;

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
  attributeHasNext = 'hasnext';
  attributeLevel = 'level';
  attributeItemName = 'name';
  attributeClassName = 'classname';
  attributeCaption = 'caption';
  attributeDataType = 'datatype';
  attributeValue = 'value';
  attributePathName = 'pathname';
  attributeParentClassName = 'parent.classname';
  attributeParentCaption = 'parent.caption';
  attributeParentID = 'parent.ID';


{ Some additional String Utils}

function DataTypeName(const DataType: TFieldType): String;
begin
  Result := Copy(TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(DataType)), 3, MAXBYTE);
end;


function ExtractObjectPath(const CompoundName: String): String;
const
  Separator = '.';
begin
  Result := String(StrRScan(PChar(CompoundName), Separator));
  if Length(Result) > 0 then begin
    Result := Copy(CompoundName, 1, Length(CompoundName) - Length(Result));
  end;
end;


function ExtractPropertyName(const CompoundName: String): String;
const
  Separator = '.';
begin
  Result := String(StrRScan(PChar(CompoundName), Separator));
  if Length(Result) > 0 then begin
    Result := Copy(Result, 2, MAXBYTE);
  end
  else begin
    Result := CompoundName;
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


function TDBIGenericMacroProcessor.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
var
  Data: TDBICompoundScope;

begin
  Result := True;

  if Scope.Top.Enabled then begin
    Data := Scope.Items.GetScope(ExtractObjectPath(AttributeName));

    Result := Assigned(Data) and Data.GetAttributeValue(ExtractPropertyName(AttributeName), AttributeValue);
  end
  else begin
    AttributeValue := '0';
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 18:05:09 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetParamValue(const ParamName: String; var ParamValue: Variant): Boolean;
const
  Caller = 'GetParamValue';

var
  Param: TParam;

begin
  Result := True;
  Param := Scope.FindParam(ParamName);

  if Scope.Top.Enabled then begin
    Result := Assigned(Param);
    if not Result and (Pos('.', ParamName) > 1) then begin
      Input.SyntaxError('Undeclared identifier "%s"', [ParamName], Self, Caller);
    end;
  end
  else begin
    Param :=  NullParam;
  end;

  if Result then begin
    ParamValue := Param.Value;
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
begin
  Result := True;

  if Scope.Top.Enabled then begin
    Result := GetAttributeValue(ParamName, ParamValue) or GetParamValue(ParamName, ParamValue);
  end
  else begin
    ParamValue := '0';
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


// _______________________________________________________________________________________

{**
  Jvr - 25/09/2006 15:33:05 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessDefine(
  const ParamType: TDBIStandardKeyword;
  const ExpectedTypes: TDBITokenTypes
  ): Boolean;
var
  Param: TParam;
  MacroName: String;
  MacroValue: String;

begin
  // Swallow Type ( #boolean, #define, #double, #integer, #short, #string )
  Input.Fetch([ParamType]);
  Input.Skip([tkWhiteSpace]);

  // Get Macro Name
  MacroName := ProcessGetMacroName;
  Input.Skip([tkWhiteSpace]);

  // Now create the appropriate typed parameter
  MacroValue := ProcessGetMacroValue(ExpectedTypes);

  ProcessMacroEnd;

  if Scope.Top.Enabled then begin
    Param := Scope.FindParam(MacroName);

    if not Assigned(Param) then begin
      Param := Scope.Top.Params.GetByName(MacroName);
    end;

    case ParamType of
      Keyword_Boolean: begin
        Param.AsBoolean := CompareText('True', MacroValue) = 0;
      end;

      Keyword_DateTime: begin
        if (MacroValue = '') then begin
          Param.AsDateTime := Now;
        end
        else begin
          Param.AsString := MacroValue;
          Param.AsDateTime := Param.AsDateTime;
        end;
      end;

      Keyword_Double: begin
        if (MacroValue = '') then begin
          Param.AsFloat := 0;
        end
        else begin
          Evaluator.Expression := MacroValue;
          Param.AsFloat := Evaluator.Result;
        end;
      end;

      Keyword_Integer: begin
        if (MacroValue = '') then begin
          Param.AsInteger := 0;
        end
        else begin
          Evaluator.Expression := MacroValue;
          Param.AsInteger := Round(Evaluator.Result);
        end;
      end;

      Keyword_Short: begin
        if (MacroValue = '') then begin
          Param.AsSmallint := 0;
        end
        else begin
          Evaluator.Expression := MacroValue;
          Param.AsSmallint := Round(Evaluator.Result);
        end;
      end;

      Keyword_String: begin
        Param.AsString := MacroValue;
      end;

      else { Keyword_Define } begin
        Param.AsString := MacroValue;
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
begin
  // Swallow keyword 'Else', Whitespace and Closing Bracket
  Input.Fetch([Keyword_Else]);

  ProcessMacroEnd;

  Result := Scope.Top.CheckType(TDBIIfThenElseScope);
  if Result then begin
    (Scope.Top as TDBIIfThenElseScope).State := msEnd;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:53:55 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessEndIf: Boolean;
begin
  // Swallow keyword 'endif', Whitespace and Closing Bracket
  Input.Fetch([Keyword_Endif]);

  ProcessMacroEnd;

  Result := Scope.Top.CheckType(TDBIIfThenElseScope);
  if Result then begin
    Scope.Pop.Free;
  end;
end;



function TDBIGenericMacroProcessor.ProcessEOL: Boolean;
var
  EOL: Boolean;
begin
  Result := True;
  // Swallow keyword 'eol', Whitespace and Closing Bracket
  Input.Fetch([Keyword_EOL]);

  Input.Skip([tkWhiteSpace]);

  // Do we remove whitespace at EOL
  EOL := Input.Have([Tok_BackSlash]);

  // Get Closing Bracket
  Input.Fetch([Tok_CloseCurlyBracket]);

  // Eat whitespace til EOL
  if EOL then begin
    Input.Skip([Tok_Space, Tok_HorizontalTab]);
    Input.Have([Tok_LineBreak]);
  end
  else begin
    Input.Skip([tkWhitespace]);
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

begin
  // Swallow keyword 'endfor', Whitespace and Closing Bracket
  Input.Fetch([Keyword_EndFor]);

  ProcessMacroEnd;

  Result := Scope.Top.CheckType(TDBICustomEnumeratorScope);
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
//##JVR      Scope.Pop.Free;
    end;
  end;
end;


procedure TDBIGenericMacroProcessor.ProcessMacroEnd;
var
  EOL: Boolean;
begin
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
  EnumerableName := ProcessGetQualifiedIdentifier;
  Input.Skip([tkWhiteSpace]);

  // Get "as" keyword
  Input.Fetch([Keyword_As]);
  Input.Skip([tkWhiteSpace]);

  // Get name of Enumerator
  EnumeratorName := Input.Fetch([Tok_Identifier]);
  if Input.Have([Tok_Dot]) then begin
    EnumeratorName := Input.Fetch([Tok_Identifier]);
  end;

  Input.Skip([tkWhiteSpace]);

  // Do we have a Key name? then reassign Key name and get Enumerator name
  if Input.Have([Tok_EqualGreater]) then begin
    EnumeratorKeyName := EnumeratorName;
    Input.Skip([tkWhiteSpace]);

    EnumeratorName := Input.Fetch([Tok_Identifier]);
  end;

  ProcessMacroEnd;

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
  if (Input.Token.TokenType = Tok_OpenCurlyBracket_Hash) then begin
    ProcessMacros;
  end;

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
  ParamName: String;
  ParamValue: Variant;

begin
  Result := '';
  ParamName := 'Undefined';
  ParamValue := '';

  try
    while not (Input.Token.TokenType in InvalidTokens) do begin
      // Deal with previously define macros
      if (Input.Token.TokenKind = tkMacro) then begin
        ParamName := Input.Token.TokenString;
        if not GetParam(Self, ParamName, ParamValue) then begin
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
        else begin
          Input.PutBack(String(ParamValue));
        end;
      end

      // Is it an inline function
      else if (Input.Token.TokenType = Tok_OpenCurlyBracket_Hash) then begin
        ProcessMacros;

        Continue;
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
    on EAnalyserError do begin
      raise;
    end;
    on E: Exception do begin
      Input.SyntaxError('Failed to get macro "%s". %s', [ParamName, E.Message], Self, Caller);
    end;
  end;

  Result := Trim(Result);
end;


// _____________________________________________________________________________
{**
  Jvr - 11/08/2006 00:47:29 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessGetQualifiedIdentifier: String;
var
  ParamName: String;
  ParamValue: Variant;
  ValidTokens: TDBITokenTypes;

begin
  Result := '';
  ParamName := 'Undefined';
  ValidTokens := [Tok_Identifier, Tok_Dot, Tok_Macro];

  try
    while (Input.Token.TokenType in ValidTokens) do begin
      // Deal with previously define macros
      if (Input.Token.TokenKind = tkMacro) then begin
        ParamName := Input.Token.TokenString;
        if not GetParam(Self, ParamName, ParamValue) then begin
          Result := Result + '${' + Input.Token.TokenString + '}';
          { TODO 4 -oJvr -cProcessGetQualifiedIdentifier(() :
            03/03/2011 15:13:42

            When a macro value is null or empty,
            then we need to replace the empty value
            with the original text.

            The current implementation is a temporary hack
          }
        end
        else begin
          Input.PutBack(String(ParamValue));
        end;
      end

      // Otherwise get the literal value(s)
      else begin
        Result := Result + Input.Token.TokenString;
      end;

      Input.NextToken;

      // Swallow the close-curly bracket if there is one
      if Input.Token.TokenType = Tok_CloseCurlyBracket then begin
        Input.NextToken;
      end;
    end;

   except
    on E: Exception do
      Input.SyntaxError('Failed to get qualified identifier "%s"', [ParamName]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:54:32 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessIf(const ParamType: TDBIStandardKeyword): Boolean;
const
  Caller = 'ProcessIf';

var
  ComparisonResult: Boolean;
  ComparisonType: TDBITokenType;
  FloatValue: Double;
  StringValue: String;
  IfThenElse: TDBIIfThenElseScope;
  Param: TParam;
  ParamName: String;
  ParamValue: Variant;
  MacroName: String;

begin
  Result := True;
  ComparisonResult := False;
  try
    // Swallow keyword 'If' / 'IfNot'
    Input.Fetch([ParamType]);
    Input.Skip([tkWhiteSpace]);
    MacroName := ProcessGetMacroName;

    Input.Skip([tkWhiteSpace]);
    ComparisonType := Input.Token.TokenType;
    if not Input.Have(Tok_Comparison) then begin
      ComparisonType := Tok_None;
    end;

    Input.Skip([tkWhitespace]);

    if GetParam(Self, MacroName, ParamValue) then begin
      Param := Local(TParam.Create(nil)).Obj as TParam;
      Param.Value := ParamValue;

      // Process as a Booleans

      if (ComparisonType = Tok_None) then begin
        If (ParamType = Keyword_If) then begin
          ComparisonResult := CompareText(Param.AsString, 'True') = 0;
        end
        else begin
          ComparisonResult := CompareText(Param.AsString, 'False') = 0;
        end;
      end

      // Process as a Numeric
      else if (Input.Token.TokenType in [Tok_IntegerLiteral..Tok_HexLiteral]) then begin
        Evaluator.Expression := ProcessGetMacroValue([]);
        FloatValue := Evaluator.Result;

        case ComparisonType of
          Tok_Smaller:      ComparisonResult := Param.AsFloat < FloatValue;
          Tok_Greater:      ComparisonResult := Param.AsFloat > FloatValue;
          Tok_GreaterEqual: ComparisonResult := Param.AsFloat >= FloatValue;
          Tok_Equality:     ComparisonResult := Param.AsFloat = FloatValue;
          Tok_InEquality:   ComparisonResult := Param.AsFloat <> FloatValue;
          Tok_NotEqual:     ComparisonResult := Param.AsFloat <> FloatValue;
          Tok_SmallerEqual: ComparisonResult := Param.AsFloat <= FloatValue;
        end;

        if ParamType = Keyword_IfNot then begin
          ComparisonResult := not ComparisonResult;
        end;
      end

      // Process as a String
      else begin
        Input.Skip([Tok_SingleQuote]);
        StringValue := ProcessGetMacroValue(
          [],
          [Tok_OpenCurlyBracket, Tok_CloseCurlyBracket, Tok_SingleQuote, Tok_Eof]
          );
        Input.Skip([Tok_SingleQuote]);

        case ComparisonType of
          Tok_Smaller:      ComparisonResult := CompareText(Param.AsString, StringValue) < 0;
          Tok_Greater:      ComparisonResult := CompareText(Param.AsString, StringValue) > 0;
          Tok_GreaterEqual: ComparisonResult := CompareText(Param.AsString, StringValue) >= 0;
          Tok_Equality:     ComparisonResult := CompareText(Param.AsString, StringValue) = 0;
          Tok_InEquality:   ComparisonResult := CompareText(Param.AsString, StringValue) <> 0;
          Tok_NotEqual:     ComparisonResult := CompareText(Param.AsString, StringValue) <> 0;
          Tok_SmallerEqual: ComparisonResult := CompareText(Param.AsString, StringValue) <= 0;
        end;

        if ParamType = Keyword_IfNot then begin
          ComparisonResult := not ComparisonResult;
        end;
      end;
    end;

    ProcessMacroEnd;

  except
    on EAnalyserError do begin
      raise;
    end;
    on E: Exception do begin
      Input.SyntaxError('%s', [E.Message], Self, Caller);
    end;
  end;

  // Check the Parent Scope to make sure it is enabled
  IfThenElse := Scope.Push(TDBIIfThenElseScope.Create) as TDBIIfThenElseScope;
  IfThenElse.Condition := ComparisonResult;
  IfThenElse.State := msIf;
//##JVR  IfThenElse.TokenString := MacroName;
  IfThenElse.TokenString := ParamValue;
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
    try
      case Input.Keyword of
        Keyword_Define,
        Keyword_Boolean,
        Keyword_DateTime,
        Keyword_Double,
        Keyword_Integer,
        Keyword_Short:
          Result := ProcessDefine(Input.Keyword, []);

        Keyword_ForEach:
          Result := ProcessForEach;

        Keyword_Else:
          Result := ProcessElse;

        Keyword_EndFor:
          Result := ProcessEndFor;

        Keyword_EndIf:
          Result := ProcessEndIf;

        Keyword_If,
        Keyword_IfNot:
          Result := ProcessIf(Input.Keyword);

        Keyword_CamelCase,
        Keyword_Lowercase,
        Keyword_Uppercase:
          Result := ProcessStringConversion(Input.Keyword);

        Keyword_Slice:
          Result := ProcessSlice(Input.Keyword);

        Keyword_EOL: Result := ProcessEOL;

      else
        raise Exception.CreateFmt('Unexpected token type "%s"', [Input.Token.TokenName]);
      end;
    except
      on EAnalyserError do begin
        raise;
      end;
      on E: Exception do begin
        Input.SyntaxError('%s', [E.Message], Self, Caller);
      end;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 05/01/2018 10:11:12 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessSlice(const Keyword: TDBIStandardKeyword): Boolean;
var
  MacroValue: String;
  Offset: Integer;
  Count: Integer;

begin
  Input.Fetch([Keyword]);
  Input.Skip([tkWhitespace]);

  Offset := StrToInt(Input.Fetch([tkNumber]));
  Input.Skip([Tok_Space, Tok_Comma]);

  Count := StrToInt(Input.Fetch([tkNumber]));
  Input.Skip([Tok_Space, Tok_Comma]);

  MacroValue := ProcessGetMacroValue([]);
  ProcessMacroEnd;

  Input.Rewind(Input.Token);
  Input.PutBack(Copy(MacroValue, Offset, Count));
  Input.NextToken;

  Result := True;
end;


// _____________________________________________________________________________
{**
  Jxg - 19/10/2015 03:00:00 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessStringConversion(const Keyword: TDBIStandardKeyword): Boolean;
var
  MacroValue: String;

begin
  Input.Fetch([Keyword]);
  Input.Skip([tkWhitespace]);

  MacroValue := ProcessGetMacroValue([]);

  case Keyword of
    Keyword_LowerCase: MacroValue := AnsiLowerCase(MacroValue);
    Keyword_UpperCase: MacroValue := AnsiUpperCase(MacroValue);
    Keyword_CamelCase: MacroValue[1] := AnsiLowerCase(MacroValue[1])[1];
  end;

  ProcessMacroEnd;

  Input.Rewind(Input.Token);
  Input.PutBack(MacroValue);
  Input.NextToken;

  Result := True;
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


function TDBICustomDatasetEnumeratorScope.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
var
  Field: TField;

begin
  Result := CompareText(AttributeName, KeyName) = 0;
  if Result then begin
    AttributeValue := ItemIndex;
    Exit;
  end;

  Result := CompareText(AttributeName, attributeHasNext) = 0;
  if Result then begin
    AttributeValue := ItemIndex < (Count - 1);
    Exit;
  end;

  Field := GetCursor.Fields.FindField(AttributeName);
  Result := Assigned(Field);
  if Result then begin
    AttributeValue := Field.Value;
    Exit;
  end;

  Result := inherited GetAttributeValue(AttributeName, AttributeValue);
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





{ TDBIComponentTraversalScope }

destructor TDBIComponentTraversalScope.Destroy;
begin
  FreeAndNil(FList);

  inherited Destroy;
end;


function TDBIComponentTraversalScope.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
var
  Item: TDBICompoundComponent;

begin
  Result := (ItemIndex < Count) and (CompareText(AttributeName, attributeLevel) = 0);
  if Result then begin
    Item := GetCurrent as TDBICompoundComponent;

    AttributeValue := Item.Tag;
  end
  else begin
    Result := inherited GetAttributeValue(AttributeName, AttributeValue);
  end;
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





{ TDBIComponentEnumeratorScope}

// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 12:48:54 - Initial code.<br />
}
function TDBIComponentEnumeratorScope.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
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
  Result := ItemIndex < Count;
  Item := GetCurrent as TDBICompoundComponent;

  if Result and (CompareText(AttributeName, attributeItemName) = 0) then begin
    AttributeValue := Item.Name;
  end
  else if Result and (CompareText(AttributeName, attributeClassName) = 0) then begin
    AttributeValue := Item.ClassName;
  end
  else if Result and (CompareText(AttributeName, attributeCaption) = 0) then begin
    AttributeValue := GetCaption;
  end
  else if Result and (CompareText(AttributeName, attributePathName) = 0) then begin
    AttributeValue := Item.CompoundName;
  end

  else if Result and (Item.Parent is TDBICompoundComponent) then begin
    Item := Item.Parent as TDBICompoundComponent;

    if (CompareText(AttributeName, attributeParentClassName) = 0) then begin
      AttributeValue := Item.ClassName;
    end
    else if (CompareText(AttributeName, attributeParentID) = 0) then begin
      AttributeValue := Item.ComponentIndex;
    end
    else if (CompareText(AttributeName, attributeParentCaption) = 0) then begin
      AttributeValue := GetCaption;
    end
    else begin
      Result := inherited GetAttributeValue(AttributeName, AttributeValue);
    end;
  end

  else begin
    Result := inherited GetAttributeValue(AttributeName, AttributeValue);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/06/2007 14:08:36 - Initial code.<br>
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





{ TDBIParamsEnumeratorScope }

function TDBIParamsEnumeratorScope.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
begin
  Result := ItemIndex < Count;

  if Result and (CompareText(AttributeName, attributeValue) = 0) then begin
    AttributeValue := (GetCurrent as TParam).Value;
  end

  else if Result and (CompareText(AttributeName, attributeDataType) = 0) then begin
    AttributeValue := DataTypeName((GetCurrent as TParam).DataType);
  end

  else begin
    Result := inherited GetAttributeValue(AttributeName, AttributeValue);
  end;
end;





{ TDBICollectionEnumeratorScope }

function TDBICollectionEnumeratorScope.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
begin
  Result := ItemIndex < Count;

  if Result and (CompareText(AttributeName, attributeItemName) = 0) then begin
    AttributeValue := (GetCurrent as TCollectionItem).DisplayName;
  end

  else if Result and (CompareText(AttributeName, attributePathName) = 0) then begin
    AttributeValue := (GetCurrent as TCollectionItem).GetNamePath;
  end

  else begin
    Result := inherited GetAttributeValue(AttributeName,  AttributeValue);
  end;
end;


function TDBICollectionEnumeratorScope.GetCount: Integer;
begin
  Result := (Data as TCollection).Count
end;


function TDBICollectionEnumeratorScope.GetCurrent: TObject;
begin
  Result := (Data as TCollection).Items[ItemIndex];
end;





{ TDBIStringsEnumeratorScope }

function TDBIStringsEnumeratorScope.GetCount: Integer;
begin
  Result := GetStrings.Count;
end;

function TDBIStringsEnumeratorScope.GetDataType: String;
begin
  Result := DataTypeName(ftString);
end;

function TDBIStringsEnumeratorScope.GetName: String;
begin
  Result := GetStrings.Names[ItemIndex];
  if (Result = '') then begin
    Result := GetStrings.Strings[ItemIndex];
  end;
end;

function TDBIStringsEnumeratorScope.GetStrings: TStrings;
begin
  Result := (Data as TStrings);
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
end;


function TDBICustomEnumeratorScope.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
begin
  Result := True;

  if CompareText(AttributeName, KeyName) = 0 then begin
    AttributeValue := ItemIndex;
  end

  else if CompareText(AttributeName, attributeHasNext) = 0 then begin
    AttributeValue := ItemIndex < (Count - 1);
  end
  else begin
    Result := inherited GetAttributeValue(AttributeName, AttributeValue);
  end;
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
  Result := True;

  if Assigned(Parent) then begin
    Result := Parent.Enabled;
  end;

  if Result then begin
    Result := (Count > 0) and (ItemIndex < Count);
  end;
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
  Jvr - 03/02/2005 17:12:45 - Initial code.<br>
}
function TDBICustomEnumeratorScope.GetMaximum: Integer;
begin
  Result := Count - 1;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/10/2009 08:58:43 - Initial code.<br />
}
function TDBICustomEnumeratorScope.GetMinimum: Integer;
begin
  Result := 0;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 12:57:12 - Initial code.<br />
}
procedure TDBICustomEnumeratorScope.Last;
begin
  FItemIndex := Count-1;
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
  Scope: TDBICustomScope;

begin
  Classes.RegisterClass(ScopeClass);

  Scope := ScopeClass.Create(ScopeData);
  Scope.ScopeName := ScopeName;

  { TODO -ojvr -cTDBIGlobalScopeStack.AddBinding() :
    08/09/2016 09:31:22
    We need to create an adapter to distinguish between the
    container and the enumerator. The Container adapter would
    then be added to the global scope and the enumerator is
    pushed on the stack as required.
  }
  Bindings.AddObject(ScopeName + '=' + ScopeClass.ClassName, Scope);

  // Add to Global scope - Don't add the Global object to the Global items :-/
  if Global <> Scope then begin
    Global.Items.Add(Scope);
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

  function GetScopeClass(const DataBindingName, EnumeratorName: String): TDBICustomScope;
  var
    ItemIndex: Integer;
    ScopeClass: TDBIScopeClass;

  begin
    Result := nil;

    ItemIndex := Bindings.IndexOfName(DataBindingName);
    if (ItemIndex >=0) then begin
      ScopeClass := TDBIScopeClass(Classes.GetClass(Bindings.ValueFromIndex[ItemIndex]));
      if Assigned(ScopeClass) then begin
        Result := Bindings.Objects[ItemIndex] as TDBICustomScope;
        Result.ScopeName := DataBindingName;
        Result.ItemName := EnumeratorName;
      end;
    end;
  end;


begin
  Result := GetScopeClass(DataBindingName, EnumeratorName);
  if Assigned(Result) then begin
    (Result as TDBICustomEnumeratorScope).KeyName := KeyName;

    Push(Result);
  end;

  if not Assigned(Result) then begin
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

function TDBIGlobalScopeStack.GetGlobal: TDBICompoundScope;
var
  Index: Integer;
  Scope: TDBIProtectedScope;

begin
  if Items.Count <= 0 then begin
    for Index := 0 to Bindings.Count-1 do begin
      if Bindings.Objects[Index] is TDBICustomGlobalScope then begin
        Scope := TDBIProtectedScope(Bindings.Objects[Index]);
        Push(Scope);
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


function TDBIScopeStack.GetGlobal: TDBICompoundScope;
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
function TDBIScopeStack.GetItems: TDBIScopeItems;
begin
  if not Assigned(FItems) then begin
    FItems := TDBIScopeItems.Create;
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
  Result := GetScopeName;
end;





{ TDBICompoundScope }

destructor TDBICompoundScope.Destroy;
begin
  FreeAndNil(FScopeItems);

  inherited Destroy;
end;


function TDBICompoundScope.GetAttributeValue(const AttributeName: String; var AttributeValue: Variant): Boolean;
var
  PropInfo: PPropInfo;
  PropData: TObject;

begin
  PropData := Self;
  PropInfo := GetPropInfo(PTypeInfo(PropData.ClassInfo), AttributeName);
  Result := Assigned(PropInfo);

  if Assigned(Data) and not Result then begin
    PropData := Data;
    PropInfo := GetPropInfo(PTypeInfo(PropData.ClassInfo), AttributeName);
    Result := Assigned(PropInfo);
  end;

  if Result then begin
    AttributeValue := TDBIProperties.GetPropValue(PropData, PropInfo);
  end;
end;


function TDBICompoundScope.GetItemCount: Integer;
begin
  Result := 0;
  if Assigned(FScopeItems) then begin
    Result := FScopeItems.Count;
  end;
end;


function TDBICompoundScope.GetItems: TDBIScopeItems;
begin
  if not Assigned(FScopeItems) then begin
    FScopeItems := TDBIScopeItems.Create;
  end;
  Result := FScopeItems;
end;





{ TDBIScopeItems }

function TDBIScopeItems.ExtractNamedScope(const CompoundName: String; out BaseName, ScopeName: String): Boolean;
const
  Separator = '.';

var
  Offset: Word;

begin
  Scopename := '';
  BaseName := CompoundName;

  Offset := IndexOfChar(Separator, CompoundName);
  Result := Offset > 0;
  if Result then begin
    BaseName := Copy(CompoundName, 1, Offset-1);
    ScopeName := Copy(CompoundName, Offset+1, 255);
  end;
end;


function TDBIScopeItems.GetScope(CompoundName: String): TDBICompoundScope;
var
  BaseName: String;
  Index: Integer;
  Scope: TDBICompoundScope;
  ScopeName: String;

begin
  Result := nil;

  if not ExtractNamedScope(CompoundName, BaseName, ScopeName) then begin
    BaseName := CompoundName;
  end;

  for Index := Count-1 downto 0 do begin
    Scope := Items[Index];
    Assert(Assigned(Scope), 'Why is "Scope" unassigned');

    if not Scope.InheritsFrom(TDBICompoundScope) then begin
      Continue;
    end;

    // Global
    if (Scope is TDBICustomGlobalScope) then begin
      if (BaseName = '') or (CompareText(CompoundName, Scope.ScopeName) = 0) then begin
        Result := Scope;
      end
      else if (Scope.ItemCount > 0) then begin
        Result := Scope.Items.GetScope(CompoundName);
      end
    end

    // Enumerator (Databinding Name / ScopeName)
    else if CompareText(BaseName, Scope.ScopeName) = 0 then begin
      if (ScopeName = '') then begin
        Result := Scope;
      end
      else if (Scope.ItemCount > 0) then begin
        Result := Scope.Items.GetScope(ScopeName);
      end;
    end

    // Enumerator Item (KeyName / ItemName)
    else if (Scope.ItemName <> '') and (CompareText(BaseName, Scope.ItemName) = 0) then begin
      if (ScopeName = '') then begin
        Result := Scope;
      end
      else if (Scope.ItemCount > 0) then begin
        Result := Scope.Items.GetScope(ScopeName);
      end;
    end;

    if Assigned(Result) then begin
      Break;
    end;

  end;
end;


class function TDBIScopeItems.IndexOfChar(const Character: Char; const Text: String): Word;
begin
  for Result := Length(Text) downto 0 do begin
    if (Result = 0) or (Text[Result] = Character) then begin
      Break;
    end;
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


function TDBICustomScope.GetScopeName: String;
begin
  Result := FScopeName;
end;


function TDBICustomScope.GetParams: TDBIParamsAdapter;
begin
  Result := FParams;
end;


procedure TDBICustomScope.SetItemName(const value: String);
begin
  FItemName := Value;
end;


procedure TDBICustomScope.SetScopeName(const Value: String);
begin
  FScopeName := Value;
end;


end.

