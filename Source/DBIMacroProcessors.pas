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
  Classes, SysUtils, DB, DBConsts, DBIConst, DBIComponents, DBIStreamAdapters, DBIUtils,
  DBITokenizerConsts, DBITokenizers, DBIExpressionEvaluators;

type
  TDBIParams = class(TParams)
  protected
    class procedure AssignDefaultValue(AParam: TParam; ADataType: TFieldType);

  public
    class procedure AssignDefaultParams(AParams: TDBIParams);

    function GetByName(
      const AParamName: String;
      const ADataType: TFieldType = ftUnknown;
      const AParamType: TParamType = ptUnknown
      ): TParam;

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
    FItemName: String;
    FKeyName: String;
    FParams: TDBIParams;

  protected
    procedure ClearParams;

    function GetBof: Boolean; virtual;
    function GetEnumerableName: String;
    function GetCount: Integer; virtual;
    function GetCurrent: TObject; virtual;
    function GetEnabled: Boolean; override;
    function GetEof: Boolean; virtual;
    function GetItemIndex: Integer; virtual;
    function GetItemName: String; virtual;
    function GetKeyName: String; virtual;
    function GetParams: TDBIParams; virtual;

    procedure SetEnumerableName(const Value: String);
    procedure SetItemName(const Value: String); virtual;
    procedure SetKeyName(const Value: String); virtual;
    procedure SetParams(const Value: TDBIParams); virtual;
    procedure UpdateParams; virtual;

    procedure First; virtual;
    function Next: Boolean; virtual;
    procedure Last; virtual;

    property Bof: Boolean read GetBof;
    property Count: Integer read GetCount;
    property EnumerableName: String read GetEnumerableName write SetEnumerableName;
    property Eof: Boolean read GetEof;
    property ItemIndex: Integer read GetItemIndex;
    property KeyName: String read GetKeyName write SetKeyName;
    property Name: String read GetItemName write SetItemName;
    property Params: TDBIParams read GetParams write SetParams;

  public
    constructor Create(ASource: TObject); virtual;
    destructor Destroy; override;

  end;
  TDBICustomEnumeratorScopeClass = class of TDBICustomEnumeratorScope;


  TDBINullEnumeratorScope = class(TDBICustomEnumeratorScope)
  end;


type
  TDBIStringsEnumeratorScope = class(TDBICustomEnumeratorScope)
  protected
    function GetCount: Integer; override;
    function GetCurrent: TObject; override;
    procedure UpdateParams; override;

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


  TDBIClientDatasetEnumeratorScope = class(TDBICustomDatasetEnumeratorScope)
  protected
    function GetCursor: TDataset; override;

  end;


  TDBIObjectListDatasetEnumeratorScope = class(TDBICustomDatasetEnumeratorScope)
  protected
    function GetCursor: TDataset; override;

  end;


type
  TDBIEnumerationKeyword = (Enum_Unknown, Enum_As, Enum_ForEach, Enum_Else, Enum_EndFor, Enum_EndIf, Enum_If );
  TDBIEnumerationKeywords = set of TDBIEnumerationKeyword;

  TDBIGenericMacroLexer = class(TDBICustomMacroLexer)
  protected
    function GetDotString(Params: TParams): String;
    function GetKeyword: TDBIEnumerationKeyword;

    procedure LexInitialise; override;
    procedure LexEscape;

    function Upto(TokenTypes: TDBITokenTypes; const Inclusive: Boolean = False): String; overload;
    function Upto(TokenKinds: TDBITokenKinds; const Inclusive: Boolean = False): String; overload;

    procedure UptoToken;

  public
    function Check(Keywords: TDBIEnumerationKeywords): String; overload;
    function Fetch(Keywords: TDBIEnumerationKeywords): String; overload;
    function Have(Keywords: TDBIEnumerationKeywords): Boolean; overload;
    function Skip(Keywords: TDBIEnumerationKeywords): String; overload;

    property Keyword: TDBIEnumerationKeyword read GetKeyword;
  end;


type
  TDBIMacroProcessorIncludeEvent = function(const IncludeName: string): string of object;

  TDBICustomBindingsList = class(TStringList);

  TDBIGenericMacroProcessor = class(TDBICustomMacroProcessor)
  private
    FBindings: TDBICustomBindingsList;
    FEvaluator: TDBIExpressionEvaluator;
    FLexer: TDBIGenericMacroLexer;
    FOnInclude: TDBIMacroProcessorIncludeEvent;
    FOrigin: TDBICompoundComponent;
    FMacroName: String;
    FParams: TDBIParams;

  protected
    procedure AddBinding(
      const ResourceName: String;
      const ResourceData: TObject;
      const ResourceClass: TDBICustomEnumeratorScopeClass
      );

    function GetBindings: TDBICustomBindingsList;
    function GetEnumeratorScope(
      const BindingName: String;
      const Recursive: Boolean
      ): TDBICustomEnumeratorScope;

    function GetEvaluator: TDBIExpressionEvaluator;
//##JVR    function GetKeyword: TDBIEnumerationKeyword;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function GetMemberComponent(const AName: String): TComponent;
    function GetMemberComponentParams(const AName: String): TParams;

    function GetParam(
      Sender: TObject; const ParamName: String; var ParamValue: Variant
      ): Boolean; override;

    function GetParams: TDBIParams; virtual;

    function ProcessComments: Boolean; virtual;
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

    procedure ReleaseBindings;

    property Evaluator: TDBIExpressionEvaluator read GetEvaluator;
    property Input: TDBIGenericMacroLexer read FLexer;
    property OnInclude: TDBIMacroProcessorIncludeEvent read FOnInclude write FOnInclude;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Process; override;

    property Params: TDBIParams read GetParams;
    property Bindings: TDBICustomBindingsList read GetBindings;
  end;


(*##JVR
  TDBIIteratorProcessor = class(TDBICustomIteratorProcessor)
  private
    FConduit: TDBIStreamFormatter;

  protected
    function DoIncludeEvent(const Value: String): String;

    function GetConduit: TDBIStreamFormatter;
    function GetRootComponent: TDBICompoundComponent;
    function GetTemplate(const Index: String): String;

    property Template[const Index: String]: String read GetTemplate;

  public
    destructor Destroy; override;
    procedure Publish(
      AComponent: TDBICompoundComponent;
      const ATemplateName: String;
      const AFileName: TFileName
      );

    property Conduit: TDBIStreamFormatter read GetConduit;

  end;
//*)


implementation

uses
  Windows, TypInfo, Controls, DBClient, DBIDataset, DBIObjectListDatasets, Dialogs;


const
  paramMin = '.min';
  paramMax = '.max';
  paramCount = '.count';
  paramLevel = '.level';
  paramName = '.name';
  paramClassName = '.classname';
  paramCaption = '.caption';
  paramDataType = '.datatype';
  paramValue = '.value';
  paramID = '.ID';
  paramPathName = '.pathname';
  paramParentName = '.parent.name';
  paramParentClassName = '.parent.classname';
  paramParentCaption = 'parent.caption';
  paramParentID = '.parent.ID';
  paramParentPathname = '.parent.pathname';


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





{ TDBIComponentProcessor }
(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 19/03/2008 08:04:16 - Initial code.<br />
}
destructor TDBIIteratorProcessor.Destroy;
begin
  FConduit.Free;
  FConduit := nil;

  FOrigin.Free;
  FOrigin := nil;

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/08/2006 17:51:36 - Initial code.<br>
}

function TDBIIteratorProcessor.GetConduit: TDBIStreamFormatter;
begin
  if not Assigned(FConduit) then begin
    FConduit := TDBIStreamFormatter.Create;
  end;
  Result := FConduit;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/03/2011 15:44:02 - Initial code.<br />
}
function TDBIIteratorProcessor.GetRootComponent: TDBICompoundComponent;
begin
  Result := FOrigin.GetRootComponent as TDBICompoundComponent;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/03/2011 15:34:52 - Initial code.<br />
}
function TDBIIteratorProcessor.GetTemplate(const Index: String): String;
const
  Caller = 'GetTemplate';

var
  TemplateComponent: TComponent;
  TemplateInfo: PPropInfo;

begin
  Result := '';

  if (Index <> '') then begin
    TemplateComponent := GetRootComponent.GetMemberComponent(Index, TDBICompoundComponent);
    if Assigned(TemplateComponent) then begin
      TemplateInfo := TypInfo.GetPropInfo(TemplateComponent, 'Text');
      if Assigned(TemplateInfo) then begin
        Input.Text := GetStrProp(TemplateComponent, TemplateInfo);
        Process;
        Result := Output.Text;
      end;
    end;

    if (Result = '') then begin
      raise Exception.CreateFmt(Caller + '::450::' + 'Template "%s" not found!', [Index]);
//##JVR      Error(nil, Caller, '180', 'Template "%s" not found!', [Index]);
    end
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 31/03/2011 09:35:57 - Initial code.<br />
}
procedure TDBIIteratorProcessor.Publish(
  AComponent: TDBICompoundComponent;
  const ATemplateName: String;
  const AFileName: TFileName
  );
begin
  FOrigin := AComponent;
  OnInclude := DoIncludeEvent;
  try
    if Assigned(AComponent) and (AComponent.ChildCount > 0) then begin
      Conduit.WriteLine(Template[ATemplateName]);
      Conduit.SaveToFile(AFileName);
    end;
  finally
    OnInclude := nil;
    FOrigin := nil;
  end;
end;


function TDBIIteratorProcessor.DoIncludeEvent(const Value: String): String;
begin
  //##NOP
end;

{##JVR
const
  Prompt = 'Resource'#13'  "%s"'#13#13'Requires'#13'  "%s"'#13#13'Required resource not found!';

var
  Item: TComponent;

begin
  if Assigned(FOrigin) then begin
    Item := FOrigin.GetMemberComponent(Trim(Value), XLeaf);

    if not Assigned(Item) then begin
      Result := '';
      MessageDlg(Format(Prompt, ['(FItemComponent as XCompoundComponent).CompoundName', Value]), mtWarning, [mbOK], 0);
    end
    else begin
      Result := (Item as XLeaf).DisplayText;
    end;
  end;
end;
//}
//*)





{ TDBICustomEnumeratorProcessor }

procedure TDBIGenericMacroProcessor.AddBinding(
  const ResourceName: String;
  const ResourceData: TObject;
  const ResourceClass: TDBICustomEnumeratorScopeClass
  );
begin
  Bindings.AddObject(ResourceName, ResourceClass.Create(ResourceData))
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 18:02:23 - Initial code.<br>
}
constructor TDBIGenericMacroProcessor.Create;
begin
  inherited Create;

  FLexer := GetInput as TDBIGenericMacroLexer;
end;


destructor TDBIGenericMacroProcessor.Destroy;
begin
  ReleaseBindings;
  FreeAndNil(FEvaluator);
  FreeAndNil(FParams);

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 19/01/2006 13:50:34 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetBindings: TDBICustomBindingsList;
begin
  if not Assigned(FBindings) then begin
    FBindings := TDBICustomBindingsList.Create;
    FBindings.Duplicates := dupError;
    FBindings.Sorted := True;
  end;
  Result := FBindings;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/02/2005 18:05:09 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetEnumeratorScope(
  const BindingName: String;
  const Recursive: Boolean
  ): TDBICustomEnumeratorScope;
var
  ItemIndex: Integer;

begin
  ItemIndex := Bindings.IndexOf(BindingName);
  if (ItemIndex >= 0) then begin
    Result := Scope.Push(Bindings.Objects[ItemIndex] as TDBICustomScope) as TDBICustomEnumeratorScope;
  end
  else begin
    raise Exception.CreateFmt('Enumeration failed, Data "%s" not available', [BindingName]);
  end;

{##JVR
  // Try to Create a Component Traversal Scope
  if Recursive then begin
    Result := Scope.Push(
      TDBIComponentTraversalScope.Create(GetMemberComponent(EnumerableName))
      ) as TDBICustomEnumeratorScope;
  end;

  // If there is NO Component Scope, then try a Component Enumerator Scope
  if not Assigned(Result) then begin
    Result := Scope.Push(
      TDBIComponentEnumeratorScope.Create(GetMemberComponent(EnumerableName))
      ) as TDBICustomEnumeratorScope;
  end;

  // If there is NO Component Scope, then try a Params Scope
  if not Assigned(Result) then begin
    Result := Scope.Push(
      TDBIParamsEnumeratorScope.Create(GetMemberComponentParams(EnumerableName))
      ) as TDBICustomEnumeratorScope;
  end;

  // Otherwise create a null scope as a place holder to provide proper program flow
  if not Assigned(Result) then begin
    Result := Scope.Push(
      TDBINullEnumeratorScope.Create(nil)
      ) as TDBICustomEnumeratorScope;
  end;
//}
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

(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 08/08/2006 17:57:54 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetKeyword: TDBIEnumerationKeyword;
var
  Index: Integer;

begin
  Index := GetEnumValue(TypeInfo(TDBIEnumerationKeyword), 'Enum_' + Input.Token.TokenString);
  if (Index <> -1) then begin
    Result := TDBIEnumerationKeyword(Index);
  end
  else begin
    Result := Enum_Unknown;
  end;
end;
//*)

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
  Param := Params.FindParam(ParamName);
  Result := Assigned(Param);
  if Result then begin
    ParamValue := Param.Value;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 19/01/2009 11:15:15 - Initial code.<br>
}
function TDBIGenericMacroProcessor.GetParams: TDBIParams;
begin
  if not Assigned(FParams) then begin
    FParams := TDBIParams.Create;
    TDBIParams.AssignDefaultParams(FParams);
  end;
  Result := FParams;
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

//##JVR  Result := Output.Text;
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
  Jvr - 10/08/2006 22:53:26 - Initial code.<br>
}
function TDBIGenericMacroProcessor.ProcessElse: Boolean;
var
  EOL: Boolean;

begin
  // Swallow keyword 'Else', Whitespace and Closing Bracket
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
  Result := Input.Keyword = Enum_EndFor;
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

  Result := Assigned(Scope.Top);
  if not Result then begin
    ShowMessageFmt('Position(%d = %d ,  %d) [%s]'#13'%s', [
      Input.Token.Position,
      Input.Token.Row,
      Input.Token.Column,
      Input.Token.AsString,
      Input.Text
      ]);
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
const
  Caller = 'ProcessForEach';

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
  Input.Fetch([Tok_Identifier]);
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
  EnumerableName := (Input as TDBIGenericMacroLexer).GetDotString(Params);
  Input.Skip([tkWhiteSpace]);

  // Get "as" keyword
  Input.Fetch([Enum_As]);
  Input.Skip([tkWhiteSpace]);

  // Get name of Enumerator
  EnumeratorName := Input.Fetch([Tok_Identifier]);
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
  EnumeratorScope := GetEnumeratorScope(EnumerableName, Recursive);

  // Initialise the Enumerator Scope Object and start the enumoration
  Result := Assigned(EnumeratorScope);
  if Result then begin
    EnumeratorScope.Params := Params;
    EnumeratorScope.Name := EnumeratorName;
    EnumeratorScope.KeyName := EnumeratorKeyName;
    EnumeratorScope.EnumerableName := EnumerableName;
    EnumeratorScope.Assign(Input.Token);

    // Ok go!
    if EnumeratorScope.Enabled and (EnumeratorScope.Count > 0) then begin
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
  ErrMsg = 'Syntax Error at [Line: %d, Offset: %d]'#13'Failed to get macro, "%s"';

var
  InterimName: String;
  InterimValue: String;
  Param: TParam;

begin
  Result := '';
  InterimName := 'Undefined';
  InterimValue := '';

  try
    while not (Input.Token.TokenType in InvalidTokens) do begin
      // Deal with previously define macros
      if (Input.Token.TokenKind = tkMacro) then begin
        InterimName := Input.Token.TokenString;
        Param := Params.FindParam(InterimName);
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
      raise Exception.CreateFmt(Caller + '::1020::' + ErrMsg, [Input.Token.Row, Input.Token.Column, InterimName]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/08/2006 22:54:32 - Initial code.<br>
}
{$ifndef UseExpressions}
function TDBIGenericMacroProcessor.ProcessIf: Boolean;
const
  Caller = 'ProcessIf';

var
  ComparisonType: TDBITokenType;
  Value: Double;
  StringValue: String;
  Retval: Integer;
  IfThenElse: TDBIIfThenElseScope;
  EOL: Boolean;

begin
  Result := False;
  try
    // Swallow keyword 'If' / 'ElseIf'
    Input.Fetch([Tok_Identifier]);
    Input.Skip([tkWhiteSpace]);
    FMacroName := ProcessGetMacroName;

    Input.Skip([tkWhiteSpace]);
    Input.Check(Tok_Comparision);
    ComparisonType := Input.Token.TokenType;
    Input.NextToken;

    Input.Skip([tkWhiteSpace]);

    // Is it a string literal
    if (Params.GetByName(FMacroName).DataType = ftString) or (Input.Token.TokenType = Tok_SingleQuote) then begin
      Input.Fetch([Tok_SingleQuote]);
      StringValue := ProcessGetMacroValue(
        [],
        [Tok_OpenCurlyBracket, Tok_CloseCurlyBracket, Tok_SingleQuote, Tok_Eof]
        );
      Input.Fetch([Tok_SingleQuote]);

      Retval := CompareText(Params.GetByName(FMacroName).AsString, StringValue);
      case ComparisonType of
        Tok_Smaller:      Result := Retval < 0;
        Tok_Greater:      Result := Retval > 0;
        Tok_GreaterEqual: Result := Retval >= 0;
        Tok_Equality:     Result := Retval = 0;
        Tok_InEquality:   Result := Retval <> 0;
        Tok_NotEqual:     Result := Retval <> 0;
        Tok_SmallerEqual: Result := Retval <= 0;
      end;
    end
    else begin
      Evaluator.Expression := ProcessGetMacroValue([]);
      Value := Evaluator.Result;

      case ComparisonType of
        Tok_Smaller:      Result := Params.GetByName(FMacroName).AsFloat < Value;
        Tok_Greater:      Result := Params.GetByName(FMacroName).AsFloat > Value;
        Tok_GreaterEqual: Result := Params.GetByName(FMacroName).AsFloat >= Value;
        Tok_Equality:     Result := Params.GetByName(FMacroName).AsFloat = Value;
        Tok_InEquality:   Result := Params.GetByName(FMacroName).AsFloat <> Value;
        Tok_NotEqual:     Result := Params.GetByName(FMacroName).AsFloat <> Value;
        Tok_SmallerEqual: Result := Params.GetByName(FMacroName).AsFloat <= Value;
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
      raise Exception.CreateFmt(Caller + '::1105::' + 'Failed on ProcessIf, "%s"', [FMacroName]);
  end;

  // Check the Parent Scope to make sure it is enabled
  IfThenElse := Scope.Push(TDBIIfThenElseScope.Create) as TDBIIfThenElseScope;
  IfThenElse.Condition := Result;
  IfThenElse.State := msIf;
  IfThenElse.TokenString := FMacroName;
end;


{$else}


function TDBIGenericMacroProcessor.ProcessIf: Boolean;
const
  Caller = 'ProcessIf';

var
//  ComparisonType: TDBITokenType;
//  Value: Double;
//  StringValue: String;
//  Retval: Integer;
  IfThenElse: TDBIIfThenElseScope;
  Expression: String;
  EOL: Boolean;

begin
  Result := False;
  try
    // Swallow keyword 'If' / 'ElseIf'
    Input.Fetch([Tok_Identifier]);
//##JVR    Input.Skip([tkWhiteSpace]);

    // Fetch the whole expression upto Closing Bracket '}'
    Expression := Lexer.Upto([Tok_CloseCurlyBracket]);

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
      raise Exception.CreateFmt(Caller + '::1105::' + 'Failed on ProcessIf, "%s"', [FMacroName]);
  end;

  // Check the Parent Scope to make sure it is enabled
  IfThenElse := Scope.Push(TDBIIfThenElseScope.Create) as TDBIIfThenElseScope;
  IfThenElse.Condition := Result;
  IfThenElse.State := msIf;
  IfThenElse.TokenString := FMacroName;
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 19/03/2008 08:07:54 - Initial code.<br />
}
function TDBIGenericMacroProcessor.ProcessMacros: Boolean;
const
  Caller = 'ProcessMacros';

begin
  // Swallow the Leadin {#
  Input.Skip([Tok_OpenCurlyBracket_Hash]);

  Result := (tsMacro in Input.Token.TokenStatus);
  if Result then begin
    case Input.Keyword of
      Enum_ForEach: Result := ProcessForEach;
      Enum_Else: Result := ProcessElse;
      Enum_EndFor: Result := ProcessEndFor;
      Enum_EndIf: Result := ProcessEndIf;
      Enum_If: Result := ProcessIf;

    else
      Result := False;
      SyntaxError(Caller, 'Unexpected token type "%s"', [Input.Token.TokenName]);
    end;
  end;

end;


procedure TDBIGenericMacroProcessor.ReleaseBindings;
var
  Index: Integer;

begin
  if Assigned(FBindings) then begin
    for Index := FBindings.Count-1 downto 0 do begin
      FBindings.Objects[Index].Free;
    end;

    FreeAndNil(FBindings);
  end;
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

function TDBIClientDatasetEnumeratorScope.GetCursor: TDataset;
begin
  if not Assigned(FCursor) then begin
    Assert(Data is TClientDataset);

    FCursor := TClientDataset.Create(nil);
    TClientDataset(FCursor).CloneCursor(Data as TClientDataset, False, False);
  end;
  Result := FCursor;
end;





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
var
  Fields: TFields;
  Param: TParam;
  ParamIndex: Integer;
  ParamName: String;

begin
  Fields := GetCursor.Fields;
//##JVR  Assert(Assigned(Dataset));

//##JVR  Params.GetByName(Name + paramCount).AsInteger := Dataset.RecordCount;
//##JVR  Params.GetByName(Name + paramID).AsInteger := Dataset.RecNo;

  for ParamIndex := 0 to Count-1 do begin
    ParamName := Name + '.' + Fields[ParamIndex].FieldName;
    Param := Params.FindParam(ParamName);

    if not Assigned(Param) then begin
      Param := Params.Add as TParam;
    end;

    Param.AssignField(Fields[ParamIndex]);
    Param.Name := ParamName;

    //TODO: paramDataType is missing
  end;
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

    Params.GetByName(Name + paramLevel).AsInteger := Item.Tag;
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

//##JVR    Params.GetByName(Name + paramCount).AsInteger := Count;
    Params.GetByName(Name + paramName).AsString := Item.Name;
    Params.GetByName(Name + paramClassName).AsString := Item.ClassName;
    Params.GetByName(Name + paramCaption).AsString := GetCaption;
//##JVR    Params.GetByName(Name + paramID).AsInteger := Item.ComponentIndex;
    Params.GetByName(Name + paramPathName).AsString := Item.CompoundName;

    if (Item.Parent is TDBICompoundComponent) then begin
      Item := Item.Parent as TDBICompoundComponent;

      Params.GetByName(Name + paramParentClassName).AsString := Item.ClassName;
      Params.GetByName(Name + paramParentID).AsInteger := Item.ComponentIndex;
      Params.GetByName(Name + paramParentCaption).AsString := GetCaption;
//##PARAM      Params.GetByName(Name + paramParentName).AsString := Item.Name;
//##PARAM      Params.GetByName(Name + paramParentPathName).AsString := Item.CompoundName;
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

    Params.GetByName(Name + paramValue).Value := Param.Value;
    Params.GetByName(Name + paramDataType).Value := DataTypeName(Param.DataType);
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

//##JVR    Params.GetByName(Name + paramCount).AsInteger := Count;
    Params.GetByName(Name + paramName).AsString := Item.DisplayName;
//##JVR    Params.GetByName(Name + paramID).AsInteger := Item.Index;
    Params.GetByName(Name + paramPathName).AsString := Item.GetNamePath;
    Params.GetByName(Name + paramParentID).AsInteger := -1;
  end;
end;





{ TDBIStringsEnumeratorScope }

function TDBIStringsEnumeratorScope.GetCount: Integer;
begin
  Result := (Data as TStrings).Count;
end;


function TDBIStringsEnumeratorScope.GetCurrent: TObject;
begin
  Result := (Data as TStrings).Objects[ItemIndex];
end;


procedure TDBIStringsEnumeratorScope.UpdateParams;
var
  ItemValue: String;
  Strings: TStrings;

  // Provides Backward compatibility
  function GetValueFromIndex(Index: Integer): string;
  var
    Offset: Integer;

  begin
    Result := Strings[Index];
    Offset := AnsiPos('=', Result);
    if (Offset > 0) then begin
      System.Delete(Result, 1, Offset);
    end
    else begin
      Result := '';
    end;
  end;

begin
  inherited UpdateParams;

  // If Items remaining then Update the Scoped Parameters for the current item
  if (ItemIndex < Count) then begin
    Strings := (Data as TStrings);
    ItemValue := GetValueFromIndex(ItemIndex);
    if (ItemValue = '') then begin
      ItemValue := Strings[ItemIndex];
    end;

    Params.GetByName(Name + paramValue).AsString := ItemValue;
//##JVR    Params.GetByName(Name + paramCount).AsInteger := Count;
//##JVR    Params.GetByName(Name + paramID).AsInteger := ItemIndex;
    Params.GetByName(Name + paramName).AsString := Strings.Names[ItemIndex];
    Params.GetByName(Name + paramDataType).Value := DataTypeName(ftString);
  end;
end;





{ TDBICustomEnumeratorScope }

constructor TDBICustomEnumeratorScope.Create(ASource: TObject);
begin
  inherited Create;

  Data := ASource;
end;


destructor TDBICustomEnumeratorScope.Destroy;
begin
  ClearParams;

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


function TDBICustomEnumeratorScope.GetEnumerableName: String;
begin
  Result := Params.GetByName(Name + paramPathName).AsString;
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
  Result := FItemIndex;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/04/2011 13:53:11 - Initial code.<br />
}
function TDBICustomEnumeratorScope.GetItemName: String;
begin
  Result := FItemName;
end;


function TDBICustomEnumeratorScope.GetKeyName: String;
begin
  Result := FKeyName;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2009 07:30:09 - Initial code.<br />
}
function TDBICustomEnumeratorScope.GetParams: TDBIParams;
begin
  Assert(Assigned(FParams));

  Result := FParams;
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
  Result := ItemIndex < Count;
  if Result then begin
    Inc(FItemIndex);
    UpdateParams;
  end;
end;


procedure TDBICustomEnumeratorScope.SetEnumerableName(const Value: String);
begin
  Params.GetByName(Name + paramPathName).AsString := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 29/05/2007 18:11:51 - Initial code.<br>
}
procedure TDBICustomEnumeratorScope.SetItemName(const Value: String);
begin
  FItemName := Value;
end;


procedure TDBICustomEnumeratorScope.SetKeyName(const Value: String);
begin
  FKeyName := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/10/2009 08:58:43 - Initial code.<br />
}
procedure TDBICustomEnumeratorScope.SetParams(const Value: TDBIParams);
begin
  FParams := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 03/02/2005 17:12:45 - Initial code.<br>
}
procedure TDBICustomEnumeratorScope.UpdateParams;
begin
  Params.GetByName(EnumerableName + paramMin).AsInteger := 0;
  Params.GetByName(EnumerableName + paramMax).AsInteger := Count-1;
  Params.GetByName(EnumerableName + paramCount).AsInteger := Count;

  if (KeyName <> '') then begin
    Params.GetByName(Name + '.' + KeyName).AsInteger := ItemIndex;
  end;
end;


procedure TDBICustomEnumeratorScope.ClearParams;
begin
  Params.FindParam(EnumerableName + paramMin).Free;
  Params.FindParam(EnumerableName + paramMax).Free;
  Params.FindParam(EnumerableName + paramCount).Free;

  Params.FindParam(Name + paramName).Free;
  Params.FindParam(Name + paramID).Free;
  Params.FindParam(Name + paramPathName).Free;
  Params.FindParam(Name + paramParentID).Free;
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





{ TDBIMacroLexer }

function TDBIGenericMacroLexer.Check(Keywords: TDBIEnumerationKeywords): String;
const
  ErrMsg = 'Unexpected Keyword "%s" at [ %d, %d ]';

begin
  Result := Token.TokenString;
  if not (Keyword in Keywords) then begin
    raise EAnalyserError.CreateFmt(ErrMsg, [Token.TokenString, Token.Row, Token.Column]);
  end;
end;


function TDBIGenericMacroLexer.Fetch(Keywords: TDBIEnumerationKeywords): String;
begin
  Result := Token.TokenString;
  Check(Keywords);
  NextToken;
end;


// _____________________________________________________________________________
{**
  Jvr - 11/08/2006 00:47:29 - Initial code.<br>
}
function TDBIGenericMacroLexer.GetDotString(Params: TParams): String;
const
  Caller = 'GetDotString';
  ErrMsg = 'Syntax Error at [Line: %d, Offset: %d]'#13'Failed to get macro, "%s"';
  ExpectedTypes = [];

var
  InterimName: String;
  InterimValue: String;
  Param: TParam;
  ValidTokens: TDBITokenTypes;

begin
  Result := '';
  InterimName := 'Undefined';
  InterimValue := '';
  ValidTokens := [Tok_Identifier, Tok_Dot, Tok_Macro];

  try
    while (Token.TokenType in ValidTokens) do begin
      // Deal with previously define macros
      if (Token.TokenKind = tkMacro) then begin
        InterimName := Token.TokenString;

        Param := Params.FindParam(InterimName);
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
      raise Exception.CreateFmt(Caller + '::1785::' + ErrMsg, [Token.Row, Token.Column, InterimName]);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/08/2006 17:57:54 - Initial code.<br>
}
function TDBIGenericMacroLexer.GetKeyword: TDBIEnumerationKeyword;
var
  Index: Integer;

begin
  Index := GetEnumValue(TypeInfo(TDBIEnumerationKeyword), 'Enum_' + Token.TokenString);
  if (Index <> -1) then begin
    Result := TDBIEnumerationKeyword(Index);
  end
  else begin
    Result := Enum_Unknown;
  end;
end;


function TDBIGenericMacroLexer.Have(Keywords: TDBIEnumerationKeywords): Boolean;
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


function TDBIGenericMacroLexer.Skip(Keywords: TDBIEnumerationKeywords): String;
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





{ TDBIParams }

// _____________________________________________________________________________
{**
  Jvr - 19/01/2009 12:27:19 - Initial code.<br>
}
class procedure TDBIParams.AssignDefaultParams(AParams: TDBIParams);
const
  Caller = 'AssignDefaultParams';

begin
  try
    { Add pre-defined parameters }
    AParams.GetByName('TimeStamp').AsString := FormatDateTime('mm/dd/yyyy hh:nn:ss:zzz', now);
    AParams.GetByName('DateStamp').AsString := FormatDateTime('mm/dd/yyyy', now);
    AParams.GetByName('Today').AsString := FormatDateTime('dd/mm/yyyy', now);
    AParams.GetByName('Time').AsString := FormatDateTime('hh:nn:ss', now);
    AParams.GetByName('Year').AsString := FormatDateTime('yyyy', now);
    AParams.GetByName('Date').AsString := FormatDateTime('dd/mm/yyyy', now);
    AParams.GetByName('Month').AsString := FormatDateTime('mm', now);
    AParams.GetByName('Day').AsString := FormatDateTime('dd', now);
    AParams.GetByName('ShortMonth').AsString := FormatDateTime('mmm', now);
    AParams.GetByName('ShortDay').AsString := FormatDateTime('ddd', now);
    AParams.GetByName('LongMonth').AsString := FormatDateTime('mmmm', now);
    AParams.GetByName('LongDay').AsString := FormatDateTime('dddd', now);
    AParams.GetByName('Now').AsDateTime := Now;

    { TODO 3 -oJvr -cTXiCustomMacroProcessor.AssignDefaultParams() :
      01/10/2008 10:02:51
    }
    AParams.GetByName('Who').AsString := TDBIHostInfo.GetUserName;
    AParams.GetByName('UserName').AsString := TDBIHostInfo.GetUserName;
    AParams.GetByName('Machine.UserName').AsString := TDBIHostInfo.GetUserName;
    AParams.GetByName('Machine.ComputerName').AsString := TDBIHostInfo.GetComputerName;

  except
    on E: Exception do
      raise Exception.CreateFmt('%s::%s::1895#13%s', [
        Self.ClassName, Caller, 'Failed to assign default params'
        ]);

  end;
end;


class procedure TDBIParams.AssignDefaultValue(AParam: TParam; ADataType: TFieldType);
const
  DefaultStringValue = #39#39;

begin
  AParam.DataType := ADataType;
  case ADataType of
    ftString,
    ftFixedChar,
    ftMemo: AParam.Value := DefaultStringValue;

    ftAutoInc,
    ftBoolean,
    ftSmallint,
    ftInteger,
    ftWord,
    ftFloat,
    ftCurrency,
    ftLargeint: AParam.Value := 0;

    ftDateTime: AParam.Value := DBISignifyNullDateTime;
  else
    DatabaseError(Format(SUnknownFieldType, [AParam.Name]));
  end;
end;


function TDBIParams.GetByName(
  const AParamName: String;
  const ADataType: TFieldType = ftUnknown;
  const AParamType: TParamType = ptUnknown
  ): TParam;
begin
  Result := FindParam(AParamName);
  if not Assigned(Result) then begin
    Result := Add as TParam;
    Result.Name := AParamName;
  end;

  if (AParamType <> ptUnknown) then begin
    Result.ParamType := AParamType;
  end;

  if (ADataType <> ftUnknown) then begin
    AssignDefaultValue(Result, ADataType);
  end;
end;



end.

