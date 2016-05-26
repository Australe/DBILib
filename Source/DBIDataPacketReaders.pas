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
  1.0 | 24/05/2013 06:15:22 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIDataPacketReaders;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DB, DBIStrings, DBITokenizers, DBITokenizerConsts, DBIDataset;

type
  TDBIColumnAttribute = (
    colAttribute_None,
    colAttribute_AttrName,
    colAttribute_Change_Log,
    colAttribute_FieldType,
    colAttribute_Hidden,
    colAttribute_Link,
    colAttribute_ReadOnly,
    colAttribute_Required,
    colAttribute_Width,
    colAttribute_RowState,
    colAttribute_SubType,
    colAttribute_UnNamed
    );


type
  TDBIPacketContext = (
    packetContext_None,
    packetContext_DataPacket,
    packetContext_Field,
    packetContext_Fields,
    packetContext_MetaData,
    packetContext_Params,
    packetContext_Row,
    packetContext_RowData,
    packetContext_Version
    );


type
  TDBIDatasetDataPacketReader = class(TDBICustomParser)
  private
    FContext: TDBIPacketContext;
    FDataset: TDataset;
    FRowData: TStrings;
    FVersion: Double;

  protected
    function CreateColumn(
      const FieldName: String;
      const DataType: TFieldType;
      const FieldSize: Word;
      const Required: Boolean;
      const Attributes: TFieldAttributes
      ): Boolean; virtual;

    function CreateRow(RowData: TStrings): Boolean; virtual;

    function GetColumnAttribute: TDBIColumnAttribute; virtual;
    function GetContext: TDBIPacketContext; virtual;
    function GetData: Boolean; virtual; abstract;
    function GetFieldDefs: TFieldDefs;
    function GetMetaData: Boolean; virtual; abstract;
    function GetRowData: TStrings;
    function GetVersionData: Boolean; virtual;

    property Context: TDBIPacketContext read FContext write FContext;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property RowData: TStrings read GetRowData;

  public
    destructor Destroy; override;

    class function GetFieldSize(const FieldSize: String): Word;
    class function GetFieldSubType(const DataType: TFieldType; const FieldSubType: String): TFieldType;
    class function GetFieldType(const FieldType: String): TFieldType;

    property Dataset: TDataset read FDataset write FDataset;

  end;


type
  TDBIXMLDataPacketLexer = class(TDBICustomXMLLexer)
  protected
    function GetAttributeValue: String;
  end;


type
  TDBIXMLDataPacketReader = class(TDBIDatasetDataPacketReader)
  private
    FLexer: TDBIXMLDataPacketLexer;

  protected
    function GetColumnAttribute: TDBIColumnAttribute; override;
    function GetContext: TDBIPacketContext; override;
    function GetLexer: TDBIXMLDataPacketLexer;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function ProcessColumn: Boolean;
    function ProcessRow: Boolean;

    property Input: TDBIXMLDataPacketLexer read GetLexer;

  public
    function DropMetaData: Boolean;
    function GetData: Boolean; override;
    function GetMetaData: Boolean; override;

  public
    class procedure LoadFromFile(const AFileName: TFileName; ADataset: TDBIDataset);
    class procedure LoadFromStream(Stream: TStream; ADataset: TDBIDataset);

  end;


type
  TDBIJsonKeyword = (
    json_Unknown,
    json_False,
    json_Null,
    json_True
    );
  TDBIJsonKeywords = set of TDBIJsonKeyword;

type
  TDBIJsonDataPacketLexer = class(TDBICustomAsciiLexer)
  protected
    function GetAttributeFloat: Double;
    function GetAttributeName: String;
    function GetAttributeValue: String;
    function GetAttributeInteger: Integer;

    procedure LexEscape;
    procedure LexInitialise; override;

  end;


type
  TDBIJsonDataPacketReader = class(TDBIDatasetDataPacketReader)
  private
    FLexer: TDBIJsonDataPacketLexer;

  protected
    function GetColumnAttribute: TDBIColumnAttribute; override;
    function GetContext: TDBIPacketContext; override;
    function GetLexer: TDBIJsonDataPacketLexer;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function ProcessColumn: Boolean;
    function ProcessRow: Boolean;

    property Context: TDBIPacketContext read GetContext;
    property Input: TDBIJsonDataPacketLexer read GetLexer;

  public
    function GetData: Boolean; override;
    function GetMetaData: Boolean; override;
    function GetVersionData: Boolean; override;

  public
    class procedure LoadFromFile(const AFileName: TFileName; ADataset: TDBIDataset);
    class procedure LoadFromStream(Stream: TStream; ADataset: TDBIDataset);

  end;


type
  TDBICSVDataPacketLexer = class(TDBICustomAsciiLexer)
  protected
    function GetAttributeValue: String;

    procedure LexInitialise; override;

  end;


  TDBICSVDataPacketReader = class(TDBIDatasetDataPacketReader)
  private
    FLexer: TDBICSVDataPacketLexer;

  protected
    function GetLexer: TDBICSVDataPacketLexer;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function ProcessRow: Boolean;

    property Input: TDBICSVDataPacketLexer read GetLexer;

  public
    function GetData: Boolean; override;
    function GetMetaData: Boolean; override;

  public
    class procedure LoadFromFile(const AFileName: TFileName; ADataset: TDBIDataset);
    class procedure LoadFromStream(Stream: TStream; ADataset: TDBIDataset);

  end;


implementation

uses
  TypInfo, Dialogs, DBIConst, DBIIntfConsts, DBIUtils, DBIDataPacketWriters;


{ TDBICSVDataPacketReader }

function TDBICSVDataPacketReader.GetData: Boolean;
begin
  Result := True;
  while Result and not Input.Eof do begin
    Result := ProcessRow;
  end;
end;


function TDBICSVDataPacketReader.GetLexer: TDBICSVDataPacketLexer;
begin
  if not Assigned(FLexer) then begin
    FLexer := GetInput as TDBICSVDataPacketLexer;
  end;
  Result := FLexer;
end;


function TDBICSVDataPacketReader.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBICSVDataPacketLexer;
end;


function TDBICSVDataPacketReader.GetMetaData: Boolean;
begin
  Result := True;

  // BootStrap the lexer;
  Input.Reset;
  Input.NextToken;

  while Result and not Input.Eof do begin
    Input.NextToken;
    Result := Input.Token.TokenType <> Tok_LineBreak;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 02/06/2013 10:58:11 - Initial code.<br>
}
class procedure TDBICSVDataPacketReader.LoadFromFile(const AFileName: TFileName; ADataSet: TDBIDataset);
var
  Connection: TDBICSVDataPacketReader;

begin
  if not ADataset.Active then begin
    raise EDBIException.Create(Self, 'LoadFromFile::275',
      'Create and open the dataset before attempting to load a CSV file', []
      );
  end;

  Connection := Local(TDBICSVDataPacketReader.Create).Obj as TDBICSVDataPacketReader;
  Connection.Input.LoadFromFile(AFileName);
  Connection.Dataset := ADataSet;
  Connection.GetMetaData;
  Connection.GetData;

  ADataSet.First;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/06/2013 13:09:43 - Initial code.<br>
}
class procedure TDBICSVDataPacketReader.LoadFromStream(Stream: TStream; ADataSet: TDBIDataset);
var
  Connection: TDBICSVDataPacketReader;

begin
  if not ADataset.Active then begin
    raise EDBIException.Create(Self, 'LoadFromStream::305',
      'Create and open the Dataset before attempting to load a CSV stream', []
      );
  end;

  Connection := Local(TDBICSVDataPacketReader.Create).Obj as TDBICSVDataPacketReader;
  Connection.Input.LoadFromStream(Stream);
  Connection.Dataset := ADataSet;
  Connection.GetMetaData;
  Connection.GetData;

  ADataSet.First;
end;


function TDBICSVDataPacketReader.ProcessRow: Boolean;
const
  Separator: array[Boolean] of TDBITokenType = (Tok_LineBreak, Tok_Comma);

var
  FieldIndex: Integer;
  FieldName: String;
  FieldValue: String;

begin
  Result := Input.Token.TokenType = Tok_LineBreak;
  if Result then begin
    RowData.Clear;

    FieldIndex := 0;
    while Result and not Input.Eof do begin
      Input.Fetch([Separator[FieldIndex > 0]]);
      Input.Skip([Tok_LineBreak]);

      FieldValue := String(Input.GetAttributeValue);
      if (FieldValue <> '') then begin
        FieldName := FieldDefs[FieldIndex].Name;
        RowData.Add(FieldName + '=' + FieldValue);
      end;

      Inc(FieldIndex);
      Result := Input.Token.TokenType <> Tok_LineBreak;
    end;

    Result := CreateRow(RowData);
  end;
end;





{ TDBICSVDataPacketLexer }

function TDBICSVDataPacketLexer.GetAttributeValue: String;
var
  Data: TStringStream;

  procedure PushChar(const Character: AnsiChar);
  begin
    PutBack(Token.AsString);
    PutBack(Character);
    NextToken;
  end;

begin
  Result := '';

  // If the value is "Delimited by Quotes", then we have a value
  // Otherwise it is assumed that we have NO Value
  if Token.TokenType = Tok_Quotes then begin
    Fetch([Tok_Quotes]);

    if Token.TokenType <> Tok_Quotes then begin
      Data := TStringStream.Create('');
      try
        while not Eof do begin
          Data.WriteString(Token.TokenString);

          NextToken;
          if Token.TokenType = Tok_Quotes then begin
            NextToken;
            if Token.TokenType <> Tok_Quotes then begin
              PushChar(Chr_Quotes);
              Break;
            end;
          end;
        end;

        Result := Data.DataString;
      finally
        Data.Free;
      end;
    end;

    Fetch([Tok_Quotes]);
  end;
end;


procedure TDBICSVDataPacketLexer.LexInitialise;
begin
  inherited LexInitialise;

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace);
end;





{ TDBIJsonDataPacketReader }

function TDBIJsonDataPacketReader.GetContext: TDBIPacketContext;
var
  Index: Integer;
  Keyword: String;

begin
  if (Input.Token.TokenType = Tok_Quotes) then begin
    Keyword := 'packetContext_' + Input.GetAttributeName;

    Index := GetEnumValue(TypeInfo(TDBIPacketContext), Keyword);
    if (Index <> -1) then begin
      FContext := TDBIPacketContext(Index);
    end
    else begin
      FContext := packetContext_None;
    end;
  end;

  Result := FContext;
end;


function TDBIJsonDataPacketReader.GetColumnAttribute: TDBIColumnAttribute;
var
  Index: Integer;
  Keyword: String;

begin
  Result := colAttribute_None;
  Keyword := 'colAttribute_' + Input.GetAttributeName;

  Index := GetEnumValue(TypeInfo(TDBIColumnAttribute), Keyword);
  if (Index <> -1) then begin
    Result := TDBIColumnAttribute(Index);
  end;
end;


function TDBIJsonDataPacketReader.GetData: Boolean;
begin
  Result := (Context = packetContext_RowData) and Input.Have([Tok_OpenSquareBracket]);
  if Result then begin
    while Result and not Input.Eof do begin
      Result := ProcessRow and Input.Have([Tok_Comma]);

      Input.Skip([tkWhiteSpace]);
    end;
  end;

  Input.Fetch([Tok_CloseSquareBracket]);
  Input.Skip([tkWhiteSpace]);
  Input.Fetch([Tok_CloseCurlyBracket]);
end;


function TDBIJsonDataPacketReader.GetLexer: TDBIJsonDataPacketLexer;
begin
  if not Assigned(FLexer) then begin
    FLexer := GetInput as TDBIJsonDataPacketLexer;
  end;
  Result := FLexer;
end;


function TDBIJsonDataPacketReader.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBIJsonDataPacketLexer;
end;


function TDBIJsonDataPacketReader.GetMetaData: Boolean;

  function ProcessColumns: Boolean;
  begin
    Result := (Context = packetContext_Fields) and Input.Have([Tok_OpenSquareBracket]);
    while Result and not Input.Eof do begin
      Input.Skip([tkWhiteSpace]);

      ProcessColumn;

      Result := not Input.Have([Tok_CloseSquareBracket]);
    end;

    Input.Skip([tok_Comma]);
    Input.Skip([tkWhiteSpace]);
  end;

  function ProcessParams: Boolean;
  begin
    Result := (Context = packetContext_Params);

    // Eat null
    Input.NextToken;

    Input.Skip([tok_Comma]);
    Input.Skip([tkWhiteSpace]);
  end;

begin
  Result := (Context = packetContext_MetaData) and Input.Have([Tok_OpenSquareBracket]);
  if Result then begin
    Input.Skip([tkWhiteSpace]);

    while (ProcessColumns or ProcessParams) and not Input.Have([Tok_CloseSquareBracket]) do;

    Input.Skip([Tok_Comma]);
    Input.Skip([tkWhiteSpace]);
  end;
end;


function TDBIJsonDataPacketReader.GetVersionData: Boolean;
begin
  // BootStrap the lexer;
  Input.Reset;
  Input.NextToken;

  Input.Skip([tkWhiteSpace]);
  Input.Fetch([Tok_OpenCurlyBracket]);
  Input.Skip([tkWhiteSpace]);

  Result := Context = packetContext_Version;
  if Result then begin
    FVersion := Input.GetAttributeFloat;
  end;
  Result := Input.Have([Tok_Comma]);

  Input.Skip([tkWhiteSpace]);
end;


class procedure TDBIJsonDataPacketReader.LoadFromFile(const AFileName: TFileName; ADataSet: TDBIDataset);
var
  Connection: TDBIJsonDataPacketReader;
  ReadOnly: Boolean;

begin
  Connection := Local(TDBIJsonDataPacketReader.Create).Obj as TDBIJsonDataPacketReader;
  ADataSet.Close;

  Connection.Input.LoadFromFile(AFileName);
  Connection.Dataset := ADataSet;
  Connection.GetVersionData;
  Connection.GetMetaData;

  ReadOnly := ADataset.ReadOnly;
  try
    ADataset.ReadOnly := False;

    ADataSet.CreateDataset;
    Connection.GetData;
  finally
    ADataset.ReadOnly := ReadOnly;
  end;

  ADataSet.First;
end;


class procedure TDBIJsonDataPacketReader.LoadFromStream(Stream: TStream; ADataSet: TDBIDataset);
var
  Connection: TDBIJsonDataPacketReader;

begin
  Connection := Local(TDBIJsonDataPacketReader.Create).Obj as TDBIJsonDataPacketReader;
  ADataSet.Close;

  Connection.Input.LoadFromStream(Stream);
  Connection.Dataset := ADataSet;
  Connection.GetVersionData;
  Connection.GetMetaData;

  ADataSet.CreateDataset;
  Connection.GetData;

  ADataSet.First;
end;


function TDBIJsonDataPacketReader.ProcessColumn: Boolean;
var
  FieldName: String;
  DataType: TFieldType;
  FieldSize: Word;
  Required: Boolean;
  Attributes: TFieldAttributes;

  function CheckAttribute(const Value: TFieldAttribute): Boolean;
  begin
    Result := CompareText(Input.GetAttributeValue, 'true') = 0;
    if Result then begin
      Include(Attributes, Value);
    end;
  end;

begin
  Result := (Context = packetContext_Fields) and Input.Have([Tok_OpenCurlyBracket]);
  if Result then begin
    Input.Skip([tkWhiteSpace]);

    Attributes := [];
    DataType := ftUnknown;
    FieldName := '';
    FieldSize := 0;
    Required := False;
    while Result and not Input.Eof do begin
      Input.Skip([tkWhiteSpace]);

      case GetColumnAttribute of
        colAttribute_AttrName: FieldName := Input.GetAttributeValue;
        colAttribute_FieldType: DataType := GetFieldType(Input.GetAttributeValue);
        colAttribute_Hidden: CheckAttribute(faHiddenCol);
        colAttribute_Link: CheckAttribute(faLink);
        colAttribute_ReadOnly: CheckAttribute(faReadOnly);
        colAttribute_Required: Required := CompareText(Input.GetAttributeValue, 'true') = 0;
        colAttribute_Width: FieldSize := Input.GetAttributeInteger;
        colAttribute_SubType: DataType := GetFieldSubType(DataType, Input.GetAttributeValue);
        colAttribute_UnNamed: CheckAttribute(faUnNamed);
      end;

      Result := Input.Have([Tok_Comma]);
    end;

    Result :=
      Input.Have([Tok_CloseCurlyBracket]) and
      CreateColumn(FieldName, DataType, FieldSize, Required, Attributes);
  end;

  //##JVR Resolve bug for Now
  Input.Skip([Tok_Comma]);

  // Skip white space
  Input.Skip([tkWhiteSpace]);
end;


function TDBIJsonDataPacketReader.ProcessRow: Boolean;
const
  Separator: array[Boolean] of TDBITokenType = (Tok_OpenCurlyBracket, Tok_Comma);

var
  FieldIndex: Integer;
  FieldName: String;
  FieldValue: String;

begin
  Result := Context = packetContext_Rowdata;
  if Result then begin
    RowData.Clear;

    FieldIndex := 0;
    while Result and not Input.Eof do begin
      Input.Skip([tkWhiteSpace]);
      Input.Fetch([Separator[FieldIndex > 0]]);
      Input.Skip([tkWhiteSpace]);

      FieldName := Input.GetAttributeName;
      FieldValue := Input.GetAttributeValue;
      if (FieldValue <> '') then begin
        RowData.Add(FieldName + '=' + FieldValue);
      end;

      Inc(FieldIndex);
      Result := not Input.Have([Tok_CloseCurlyBracket]);
    end;

    Result := CreateRow(RowData);
  end;

  Input.Skip([tkWhiteSpace]);
end;





{ TDBIJsonDataPacketLexer }

function TDBIJsonDataPacketLexer.GetAttributeFloat: Double;
begin
  Check([Tok_FloatLiteral]);
  Result := Token.AsFloat;

  NextToken;
  Skip([tkWhiteSpace]);
end;


function TDBIJsonDataPacketLexer.GetAttributeName: String;
begin
  Result := '';
  if Have([Tok_Quotes]) then begin
    Result := Fetch([Tok_Identifier]);
    Fetch([Tok_Quotes]);
    Skip([tkWhiteSpace]);
    Fetch([Tok_Colon]);
    Skip([tkWhiteSpace]);
  end;
end;


function TDBIJsonDataPacketLexer.GetAttributeValue: String;
var
  Data: TStringStream;

begin
  Result := '';
  if Have([Tok_Quotes]) then begin
    if not Have([Tok_Quotes]) then begin
      Data := Local(TStringStream.Create('')).Obj as TStringStream;

      while not Eof do begin
        if Have([Tok_Quotes]) then begin
          Break;
        end
        else begin
          Data.WriteString(Token.TokenString);
          NextToken;
        end;
      end;

      Result := Data.DataString;
    end;
  end;

  Skip([tkWhiteSpace]);
end;


function TDBIJsonDataPacketLexer.GetAttributeInteger: Integer;
begin
  Check([Tok_IntegerLiteral]);
  Result := Token.AsInteger;

  NextToken;
  Skip([tkWhiteSpace]);
end;


procedure TDBIJsonDataPacketLexer.LexEscape;
var
  PSymbolData: PLexerSymbolData;

begin
  if not (tsStringLiteral in LexerStatus) then begin
    SyntaxError('Unexpected Escape character "\", not in String literal', [], Self, 'LexEscape');
  end;

  GetChar;
  case LexerChar of
    'b': Token.AsChar := #8;
    't': Token.AsChar := #9;
    'n': Token.AsChar := #10;
    'f': Token.AsChar := #12;
    'r': Token.AsChar := #13;

    'u': SyntaxError('Unexpected Hex Code "\u"', [], Self, 'LexEscape');
  else
    Token.AsChar := LexerChar;
  end;

  PSymbolData := @(LexerCharMap[LexerChar]);

  if (LexerChar = #34) then begin
    Token.TokenType := Tok_EscapeQuotes;
  end
  else begin
    Token.TokenType := PSymbolData^.TokenType;
  end;

//##JVR  UpdateStatus(PSymbolData^.TokenStatus);
  GetChar;
end;


procedure TDBIJsonDataPacketLexer.LexInitialise;
begin
  inherited LexInitialise;

  MapSymbol(LexEscape, Chr_BackSlash, Tok_Default, tkSymbol);

  // String Literals toggles State only!
  MapSymbol(LexSymbol, Chr_Quotes, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);
//##JVR  MapSymbol(LexSymbol, Chr_Apostrophe, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace);
end;





{ TDBIXMLDataPacketReader }

function TDBIXMLDataPacketReader.DropMetaData: Boolean;
begin
  Result := True;

  // BootStrap the lexer;
  Input.Reset;
  Input.NextToken;

  while Result and not Input.Eof do begin
    Result := (Context <> packetContext_RowData);
    if Result then begin
      Context := GetContext;

      while Result and not Input.Eof do begin
        Input.NextToken;
        Context := GetContext;
        Result := (Context <> packetContext_RowData);
      end;
    end;
  end;
end;


function TDBIXMLDataPacketReader.GetData: Boolean;
begin
  Result := True;
  while not Input.Eof do begin
    if Result then begin
      Context := GetContext;

      while Result and not Input.Eof do begin
        ProcessRow;

        Input.NextToken;
        Context := GetContext;
        Result := (Context <> packetContext_RowData);
      end;
    end;

    if not Result then begin
{$ifdef DebugInfo}
      Output.WriteLine('GetData is Done, LastToken = ' + Input.Token.AsString);
{$endif}
      Break;
    end;
  end;
end;


function TDBIXMLDataPacketReader.GetColumnAttribute: TDBIColumnAttribute;
var
  Index: Integer;

begin
  Result := colAttribute_None;
  if Input.Token.TokenKind = tkIdentifier then begin
    Index := GetEnumValue(TypeInfo(TDBIColumnAttribute), 'colAttribute_' + String(Input.Token.AsString));
    if (Index <> -1) then begin
      Result := TDBIColumnAttribute(Index);
    end;
  end;
end;


function TDBIXMLDataPacketReader.GetContext: TDBIPacketContext;
var
  Index: Integer;

begin
  Result := packetContext_None;
  if Input.Token.TokenKind = tkIdentifier then begin
    Index := GetEnumValue(TypeInfo(TDBIPacketContext), 'packetContext_' + Input.Token.TokenString);
    if (Index <> -1) then begin
      Result := TDBIPacketContext(Index);

      Input.NextToken;
    end;
  end;

  Input.Skip([Tok_Space]);
end;


function TDBIXMLDataPacketReader.GetLexer: TDBIXMLDataPacketLexer;
begin
  if not Assigned(FLexer) then begin
    FLexer := GetInput as TDBIXMLDataPacketLexer;
  end;
  Result := FLexer;
end;


function TDBIXMLDataPacketReader.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBIXMLDataPacketLexer;
end;


function TDBIXMLDataPacketReader.GetMetaData: Boolean;
begin
  Result := True;

  // BootStrap the lexer;
  Input.Reset;
  Input.NextToken;

  while Result and not Input.Eof do begin
    Result := (Context <> packetContext_RowData);
    if Result then begin
      Context := GetContext;

      while Result and not Input.Eof do begin
        ProcessColumn;

        Input.NextToken;
        Context := GetContext;
        Result := (Context <> packetContext_RowData);
      end;
    end;
  end;
end;


function TDBIXMLDataPacketReader.ProcessColumn: Boolean;
var
  FieldName: String;
  DataType: TFieldType;
  FieldSize: Word;
  Required: Boolean;
  Attributes: TFieldAttributes;

begin
  Result := Context = packetContext_Field;
  if Result then begin
    Attributes := [];
    DataType := ftUnknown;
    FieldName := '';
    FieldSize := 0;
    Required := False;
    while Result and not Input.Eof do begin
      case GetColumnAttribute of
        colAttribute_AttrName: FieldName := String(Input.GetAttributeValue);
        colAttribute_FieldType: DataType := GetFieldType(Input.GetAttributeValue);
        colAttribute_Hidden: Include(Attributes, faHiddenCol);
        colAttribute_Link: Include(Attributes, faLink);
        colAttribute_ReadOnly: Include(Attributes, faReadOnly);
        colAttribute_Required: Required := CompareText(String(Input.GetAttributeValue), 'true') = 0;
        colAttribute_Width: FieldSize := GetFieldSize(Input.GetAttributeValue);
        colAttribute_SubType: DataType := GetFieldSubType(DataType, Input.GetAttributeValue);
        colAttribute_UnNamed: Include(Attributes, faUnNamed);
      end;

      Input.NextToken;
      Result := (tsXmlElement in Input.Token.TokenStatus);
    end;

    Result := CreateColumn(FieldName, DataType, FieldSize, Required, Attributes);
  end;
end;


function TDBIXMLDataPacketReader.ProcessRow: Boolean;
var
  FieldName: String;
  FieldValue: String;

begin
  Result := Context = packetContext_Row;
  if Result then begin
    RowData.Clear;

    while Result and not Input.Eof do begin
      case GetColumnAttribute of
        colAttribute_RowState: Input.GetAttributeValue;
      else
        if (Input.Token.Tokenkind = tkIdentifier) then begin
          FieldName := String(Input.Token.TokenString);
          FieldValue := String(Input.GetAttributeValue);
          RowData.Add(FieldName + '=' + FieldValue);
        end;
      end;

      Input.NextToken;
      Result := (tsXmlElement in Input.Token.TokenStatus);
    end;

    Result := CreateRow(RowData);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 02/06/2013 10:58:11 - Initial code.<br><p>

  If a Dataset is readonly then this method of loading from an XML file will
  FAIL.  Therefore we preserve the "ReadOnly" status, force it to FALSE,
  and load the data.

  The ReadOnly property is then restored to the initial value.</p>

  <b>Please Note</b></br><p>
  This is only a hack and is not likely to be fail-safe.
  In the future a more reliable method of loading will need to be made.</p>
}
class procedure TDBIXMLDataPacketReader.LoadFromFile(const AFileName: TFileName; ADataSet: TDBIDataset);
var
  Connection: TDBIXmlDataPacketReader;
  ReadOnly: Boolean;

begin
  Connection := Local(TDBIXmlDataPacketReader.Create).Obj as TDBIXmlDataPacketReader;
  ADataSet.Close;

  Connection.Input.LoadFromFile(AFileName);
  Connection.Dataset := ADataSet;
  Connection.GetMetaData;

  ReadOnly := ADataset.ReadOnly;
  try
    ADataset.ReadOnly := False;

    ADataSet.CreateDataset;
    Connection.GetData;
  finally
    ADataset.ReadOnly := ReadOnly;
  end;

  ADataSet.First;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/06/2013 13:09:43 - Initial code.<br>
}
class procedure TDBIXMLDataPacketReader.LoadFromStream(Stream: TStream; ADataSet: TDBIDataset);
var
  Connection: TDBIXmlDataPacketReader;

begin
  Connection := Local(TDBIXmlDataPacketReader.Create).Obj as TDBIXmlDataPacketReader;
  ADataSet.Close;

  Connection.Input.LoadFromStream(Stream);
  Connection.Dataset := ADataSet;
  Connection.GetMetaData;

  ADataSet.CreateDataset;
  Connection.GetData;

  ADataSet.First;
end;





{ TDBIXMLDataPacketLexer }

function TDBIXMLDataPacketLexer.GetAttributeValue: String;
var
  Data: TStringStream;

begin
  Result := '';

  Fetch([tkIdentifier]); // Attribute Name
  Fetch([Tok_Equals]);
  Fetch([Tok_Quotes]);

  if Token.TokenType <> Tok_Quotes then begin
    Data := TStringStream.Create('');
    try
      while not Eof do begin
        Data.WriteString(Token.TokenString);

        NextToken;
        if Token.TokenType = Tok_Quotes then begin
          Break;
        end;
      end;

      Result := Data.DataString;
    finally
      Data.Free;
    end;

    NextToken;
  end;
end;








{ TDBIDatasetDataPacketReader }

function TDBIDatasetDataPacketReader.CreateColumn(
  const FieldName: String;
  const DataType: TFieldType;
  const FieldSize: Word;
  const Required: Boolean;
  const Attributes: TFieldAttributes
  ): Boolean;
var
  FieldDef: TFieldDef;

begin
  FieldDef := FieldDefs.AddFieldDef;
  FieldDef.Name := FieldName;
  FieldDef.Attributes := Attributes;
  FieldDef.DataType := DataType;

  // WideString vs AnsiString
  if Datatype in [ftWideString] then begin
    FieldDef.Size := FieldSize div 2;
  end
  else begin
    FieldDef.Size := FieldSize;
  end;

  // Don't change this order - fpc needs this!
  if (Required) then begin
    FieldDef.Attributes := FieldDef.Attributes + [faRequired];
  end;

  FieldDef.Required := Required;

  Result := FieldDefs.Count > 0;
end;


function TDBIDatasetDataPacketReader.CreateRow(RowData: TStrings): Boolean;
var
  Index: Integer;
  Field: TField;
  FieldName: String;
  FieldData: String;

  // TClientDataset stores byte and word as signed values - we therefore need to correct
  function DBIStrToUnsigned(const Str: String; const MaxValue: Word): Integer;
  begin
    Result := StrToIntDef(Str, 0);
    if Result < 0 then begin
      Result := Result + MaxValue + 1;
    end;
  end;

  function DBIStrToDateTime(const Str: String): TDateTime;
  begin

    if Length(Str) = 20 then begin
      Result := DBIStrTimeStampToDateTime(PAnsiChar(AnsiString(Str)), True);
    end
    else if Length(Str) = 17 then begin
      Result := DBIStrTimeStampToDateTime(PAnsiChar(AnsiString(Str)), False);
    end
    else if Length(Str) = 8 then begin
      Result := DBIStrDateStampToDateTime(PAnsiChar(AnsiString(Str)));
    end
    else begin
      Result := 0.0;

      DatabaseError('Invalid TDateTime format: ' + Str);
    end;
  end;

begin
  Result := RowData.Count > 0;
  if Result then begin
    Dataset.Append;

    for Index := 0 to RowData.Count-1 do begin
      FieldName := RowData.Names[Index];
      Field := Dataset.FindField(String(FieldName));
      if Assigned(Field) then begin
        FieldData := RowData.Values[FieldName];
        if (FieldData = '') then begin
          Field.Clear;
        end
        else if not Field.ReadOnly then begin
          case Field.DataType of
            ftBoolean: Field.AsBoolean := CompareText('True', FieldData) = 0;
            ftCurrency: Field.AsFloat := StrToFloat(String(FieldData));
            ftDate: Field.AsDateTime := DBIStrDateStampToDateTime(PAnsiChar(AnsiString(FieldData)));
            ftDateTime: Field.AsDateTime := DBIStrToDateTime(FieldData);
            ftFloat: Field.AsFloat := StrToFloat(String(FieldData));
            ftMemo: Field.AsString := FieldData;
            ftString: Field.AsString := FieldData;
            ftWideString: Field.AsString := FieldData;
            ftInteger: Field.AsInteger := StrToIntDef(FieldData, 0);
            ftLargeint: TLargeIntField(Field).AsLargeint := StrToInt64(FieldData);
            ftSmallint: Field.AsInteger := StrToIntDef(FieldData, 0);
            ftWord: Field.AsInteger := DBIStrToUnsigned(FieldData, $FFFF);
{$ifdef Delphi2009}
            db.ftByte: Field.AsInteger := DBIStrToUnsigned(FieldData, $FF);
            db.ftShortInt: Field.AsInteger := StrToIntDef(FieldData, 0);
            db.ftLongWord: Field.AsInteger := StrToIntDef(FieldData, 0);
            db.ftSingle: Field.AsFloat := StrToFloat(String(FieldData));
            db.ftExtended: Field.AsFloat := StrToFloat(String(FieldData));
{$endif}
          else
            raise Exception.CreateFmt('DataType %s not supported', [GetEnumName(TypeInfo(TFieldType), Ord(Field.DataType))]);
          end;
        end;

      end;
    end;

    Dataset.Post;
  end;
end;


destructor TDBIDatasetDataPacketReader.Destroy;
begin
  FRowData.Free;
  FRowData := nil;

  inherited Destroy;
end;


function TDBIDatasetDataPacketReader.GetColumnAttribute: TDBIColumnAttribute;
begin
  Result := colAttribute_None;
end;


function TDBIDatasetDataPacketReader.GetContext: TDBIPacketContext;
begin
  Result := packetContext_None;
end;


function TDBIDatasetDataPacketReader.GetFieldDefs: TFieldDefs;
begin
  Assert(Assigned(FDataset));

  Result :=  FDataset.FieldDefs;
end;


class function TDBIDatasetDataPacketReader.GetFieldSize(const FieldSize: String): Word;
begin
  Result := StrToIntDef(FieldSize, 0);
end;


class function TDBIDatasetDataPacketReader.GetFieldSubType(
  const DataType: TFieldType;
  const FieldSubType: String
  ): TFieldType;
const
  UnsupportedSubtype = 'Unsupported %s subtype "%s"';

begin
  // Binary subtypes
  if DataType = db.ftBytes then begin
    if CompareText(FieldSubType, 'Text') = 0 then begin
      Result := db.ftMemo;
    end
    else begin
      raise Exception.CreateFmt(UnsupportedSubtype, ['binary', FieldSubType]);
    end;
  end

  // Float subtypes
  else if DataType = db.ftFloat then begin
    if CompareText(FieldSubType, 'Money') = 0 then begin
      Result := db.ftCurrency;
    end
    else begin
      raise Exception.CreateFmt(UnsupportedSubtype, ['float', FieldSubType]);
    end;
  end

  // WideString subtypes
  else if DataType = db.ftWideString then begin
    if CompareText(FieldSubType, 'WideText') = 0 then begin
      Result := db.ftWideString;
    end
    else begin
      raise Exception.CreateFmt(UnsupportedSubtype, ['widestring', FieldSubType]);
    end;
  end

  // Unsupported subtypes
  else begin
    raise Exception.CreateFmt(UnsupportedSubtype, ['', FieldSubType]);
  end;
end;


class function TDBIDatasetDataPacketReader.GetFieldType(const FieldType: String): TFieldType;
begin
  if CompareText(FieldType, cdsFieldShortInt) = 0 then begin
    Result := ftSigned8;
  end
  else if CompareText(FieldType, cdsFieldByte) = 0 then begin
    Result := ftUnsigned8;
  end
  else if CompareText(FieldType, cdsFieldSmallInt) = 0 then begin
    Result := ftSigned16;
  end
  else if CompareText(FieldType, cdsFieldWord) = 0 then begin
    Result := ftUnsigned16;
  end
  else if CompareText(FieldType, cdsFieldInteger) = 0 then begin
    Result := ftSigned32;
  end
  else if CompareText(FieldType, cdsFieldLongWord) = 0 then begin
    Result := ftUnsigned32;
  end
  else if CompareText(FieldType, cdsFieldLargeInt) = 0 then begin
    Result := ftLargeint;
  end
  else if CompareText(FieldType, cdsFieldSingle) = 0 then begin
    Result := ftFloat4;
  end
  else if CompareText(FieldType, cdsFieldFloat) = 0 then begin
    Result := db.ftFloat;
  end
  else if CompareText(FieldType, cdsFieldExtended) = 0 then begin
    Result := ftFloatIEEE;
  end
  else if CompareText(FieldType, cdsFieldDate) = 0 then begin
    Result := db.ftDate;
  end
  else if CompareText(FieldType, cdsFieldTime) = 0 then begin
    Result := db.ftTime;
  end
  else if CompareText(FieldType, cdsFieldDateTime) = 0 then begin
    Result := db.ftDateTime;
  end
  else if CompareText(FieldType, cdsFieldAnsiString) = 0 then begin
    Result := db.ftString;
  end
  else if CompareText(FieldType, cdsFieldWideString) = 0 then begin
    Result := db.ftWideString;
  end
  else if CompareText(FieldType, cdsFieldBoolean) = 0 then begin
    Result := db.ftBoolean;
  end
  else if CompareText(FieldType, cdsFieldBytes) = 0 then begin
    Result := db.ftBytes;
  end
  else if CompareText(FieldType, cdsFieldADT) = 0 then begin
    Result := db.ftADT;
  end
  else begin
    raise Exception.CreateFmt('Unknown FieldType "%s"', [FieldType]);
  end;
end;


function TDBIDatasetDataPacketReader.GetRowData: TStrings;
begin
  if not Assigned(FRowData) then begin
    FRowData := TStringList.Create;
  end;
  Result := FRowData;
end;


function TDBIDatasetDataPacketReader.GetVersionData: Boolean;
begin
  FVersion := 0.0;

  Result := False;
end;


end.
