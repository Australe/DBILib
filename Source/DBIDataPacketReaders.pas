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
  Classes, SysUtils, DB, DBIStrings,  DBITokenizers, DBITokenizerConsts;

type
  TDBICustomDataPacketReader = class(TDBICustomParser)
  protected
    function CreateColumn(
      const FieldName: String;
      const DataType: TFieldType;
      const FieldSize: Word;
      const Required: Boolean;
      const Attributes: TFieldAttributes
      ): Boolean; virtual;

    function CreateRow(RowData: TStrings): Boolean; virtual;

    procedure PushChar(const Character: AnsiChar);

  public
    function GetData: Boolean; virtual; abstract;
    function GetMetaData: Boolean; virtual; abstract;

  end;


type
  TDBIDatasetDataPacketReader = class(TDBICustomDataPacketReader)
  private
    FDataset: TDataset;
    FRowData: TStrings;

  protected
    function CreateColumn(
      const FieldName: String;
      const DataType: TFieldType;
      const FieldSize: Word;
      const Required: Boolean;
      const Attributes: TFieldAttributes
      ): Boolean; override;

    function CreateRow(RowData: TStrings): Boolean; override;

    function GetFieldDefs: TFieldDefs;
    function GetRowData: TStrings;

    property FieldDefs: TFieldDefs read GetFieldDefs;

    property RowData: TStrings read GetRowData;

  public
    destructor Destroy; override;

    property Dataset: TDataset read FDataset write FDataset;

  end;


type
  TDBIXmlAttribute = (
    xmlAttribute_None,
    xmlAttribute_AttrName,
    xmlAttribute_Change_Log,
    xmlAttribute_FieldType,
    xmlAttribute_Hidden,
    xmlAttribute_Link,
    xmlAttribute_ReadOnly,
    xmlAttribute_Required,
    xmlAttribute_Width,
    xmlAttribute_RowState,
    xmlAttribute_SubType,
    xmlAttribute_UnNamed
    );

  TDBIXmlElement = (
    xmlElement_None,
    xmlElement_DataPacket,
    xmlElement_Field,
    xmlElement_Fields,
    xmlElement_MetaData,
    xmlElement_Params,
    xmlElement_Row,
    xmlElement_RowData
    );


type
  TDBIXMLDataPacketLexer = class(TDBICustomXMLLexer);

  TDBICustomXMLDataPacketReader = class(TDBIDatasetDataPacketReader)
  private
    FContext: TDBIXmlElement;

  protected
    function GetAttribute: TDBIXmlAttribute;
    function GetAttributeValue: AnsiString;
    function GetDataType: TFieldType;
    function GetElement: TDBIXmlElement;
    function GetFieldSize: Word;
    function GetFieldSubType(const DataType: TFieldType): TFieldType;

    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function ProcessColumn: Boolean;
    function ProcessRow: Boolean;

  public
    function DropMetaData: Boolean;
    function GetData: Boolean; override;
    function GetMetaData: Boolean; override;

    property Context: TDBIXmlElement read FContext write FContext;

  end;
  TDBIXMLDataPacketReader = class(TDBICustomXMLDataPacketReader);


type
  TDBICSVDataPacketLexer = class(TDBICustomAsciiLexer)
  protected
    procedure LexInitialise; override;

  end;


  TDBICustomCSVDataPacketReader = class(TDBIDatasetDataPacketReader)
  protected
    function GetAttributeValue: AnsiString;
    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function ProcessRow: Boolean;

  public
    function GetData: Boolean; override;
    function GetMetaData: Boolean; override;

  end;
  TDBICSVDataPacketReader = class(TDBICustomCSVDataPacketReader);


implementation

uses
  TypInfo, Dialogs, DBIIntfConsts, DBIUtils, DBIDataPacketWriters;


{ TDBICustomCSVDataPacketReader }

function TDBICustomCSVDataPacketReader.GetAttributeValue: AnsiString;
var
  Data: TStringStream;

begin
  Result := '';

  // If the value is "Delimited by Quotes", then we have a value
  // Otherwise it is assumed that we have NO Value
  if Input.Token.TokenType = Tok_Quotes then begin
    Input.Fetch([Tok_Quotes]);

    if Input.Token.TokenType <> Tok_Quotes then begin
      Data := TStringStream.Create('');
      try
        while not Input.Eof do begin
          Data.WriteString(String(Input.Token.AsString));

          Input.NextToken;
          if Input.Token.TokenType = Tok_Quotes then begin
            Input.NextToken;
            if Input.Token.TokenType <> Tok_Quotes then begin
              PushChar(Chr_Quotes);
              Break;
            end;
          end;
        end;

        Result := AnsiString(Data.DataString);
      finally
        Data.Free;
      end;
    end;

    Input.Fetch([Tok_Quotes]);
  end;
end;


function TDBICustomCSVDataPacketReader.GetData: Boolean;
begin
  Result := True;
  while Result and not Input.Eof do begin
    Result := ProcessRow;
  end;
end;


function TDBICustomCSVDataPacketReader.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBICSVDataPacketLexer;
end;


function TDBICustomCSVDataPacketReader.GetMetaData: Boolean;
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


function TDBICustomCSVDataPacketReader.ProcessRow: Boolean;
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

      FieldValue := String(GetAttributeValue);
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

procedure TDBICSVDataPacketLexer.LexInitialise;
begin
  inherited LexInitialise;

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace);
end;





{ TDBICustomXMLDataPacketReader }

function TDBICustomXMLDataPacketReader.DropMetaData: Boolean;
begin
  Result := True;

  // BootStrap the lexer;
  Input.Reset;
  Input.NextToken;

  while Result and not Input.Eof do begin
    Result := (Context <> xmlElement_RowData);
    if Result then begin
      Context := GetElement;

      while Result and not Input.Eof do begin
        Input.NextToken;
        Context := GetElement;
        Result := (Context <> xmlElement_RowData);
      end;
    end;
  end;
end;


function TDBICustomXMLDataPacketReader.GetAttribute: TDBIXmlAttribute;
var
  Index: Integer;

begin
  Result := xmlAttribute_None;
  if Input.Token.TokenKind = tkIdentifier then begin
    Index := GetEnumValue(TypeInfo(TDBIXmlAttribute), 'xmlAttribute_' + String(Input.Token.AsString));
    if (Index <> -1) then begin
      Result := TDBIXmlAttribute(Index);
    end;
  end;
end;


function TDBICustomXMLDataPacketReader.GetAttributeValue: AnsiString;
var
  Data: TStringStream;

begin
  Result := '';

  Input.Fetch([tkIdentifier]); // Attribute Name
  Input.Fetch([Tok_Equals]);
  Input.Fetch([Tok_Quotes]);

  if Input.Token.TokenType <> Tok_Quotes then begin
    Data := TStringStream.Create('');
    try
      while not Input.Eof do begin
        Data.WriteString(String(Input.Token.AsString));

        Input.NextToken;
        if Input.Token.TokenType = Tok_Quotes then begin
          Break;
        end;
      end;

      Result := AnsiString(Data.DataString);
    finally
      Data.Free;
    end;

    Input.NextToken;
  end;
end;


function TDBICustomXMLDataPacketReader.GetData: Boolean;
begin
  Result := True;
  while not Input.Eof do begin
    if Result then begin
      Context := GetElement;

      while Result and not Input.Eof do begin
        ProcessRow;

        Input.NextToken;
        Context := GetElement;
        Result := (Context <> xmlElement_RowData);
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


// _______________________________________________________________________________________
{**
  NOTE: Please use a sorted string list here!
}
function TDBICustomXMLDataPacketReader.GetDataType: TFieldType;
var
  FieldType: AnsiString;

begin
  FieldType := GetAttributeValue;

  if DBICompareText(FieldType, cdsFieldShortInt) = 0 then begin
    Result := ftSigned8;
  end
  else if DBICompareText(FieldType, cdsFieldByte) = 0 then begin
    Result := ftUnsigned8;
  end
  else if DBICompareText(FieldType, cdsFieldSmallInt) = 0 then begin
    Result := ftSigned16;
  end
  else if DBICompareText(FieldType, cdsFieldWord) = 0 then begin
    Result := ftUnsigned16;
  end
  else if DBICompareText(FieldType, cdsFieldInteger) = 0 then begin
    Result := ftSigned32;
  end
  else if DBICompareText(FieldType, cdsFieldLongWord) = 0 then begin
    Result := ftUnsigned32;
  end
  else if DBICompareText(FieldType, cdsFieldLargeInt) = 0 then begin
    Result := ftLargeint;
  end
  else if DBICompareText(FieldType, cdsFieldSingle) = 0 then begin
    Result := ftFloat4;
  end
  else if DBICompareText(FieldType, cdsFieldFloat) = 0 then begin
    Result := db.ftFloat;
  end
  else if DBICompareText(FieldType, cdsFieldExtended) = 0 then begin
    Result := ftFloatIEEE;
  end
  else if DBICompareText(FieldType, cdsFieldDate) = 0 then begin
    Result := db.ftDate;
  end
  else if DBICompareText(FieldType, cdsFieldTime) = 0 then begin
    Result := db.ftTime;
  end
  else if DBICompareText(FieldType, cdsFieldDateTime) = 0 then begin
    Result := db.ftDateTime;
  end
  else if DBICompareText(FieldType, cdsFieldAnsiString) = 0 then begin
    Result := db.ftString;
  end
  else if DBICompareText(FieldType, cdsFieldWideString) = 0 then begin
    Result := db.ftWideString;
  end
  else if DBICompareText(FieldType, cdsFieldBoolean) = 0 then begin
    Result := db.ftBoolean;
  end
  else if DBICompareText(FieldType, cdsFieldBytes) = 0 then begin
    Result := db.ftBytes;
  end
  else if DBICompareText(FieldType, cdsFieldADT) = 0 then begin
    Result := db.ftADT;
  end
  else begin
    raise Exception.CreateFmt('Unknown FieldType "%s"', [FieldType]);
  end;
end;


function TDBICustomXMLDataPacketReader.GetElement: TDBIXmlElement;
var
  Index: Integer;

begin
  Result := xmlElement_None;
  if Input.Token.TokenKind = tkIdentifier then begin
    Index := GetEnumValue(TypeInfo(TDBIXmlElement), 'xmlElement_' + String(Input.Token.AsString));
    if (Index <> -1) then begin
      Result := TDBIXmlElement(Index);

      Input.NextToken;
    end;
  end;

  Input.Skip([Tok_Space]);
end;


function TDBICustomXMLDataPacketReader.GetFieldSize: Word;
begin
  Result := StrToIntDef(String(GetAttributeValue), 0);
end;


function TDBICustomXMLDataPacketReader.GetFieldSubType(const DataType: TFieldType): TFieldType;
const
  UnsupportedSubtype = 'Unsupported %s subtype "%s"';

var
  FieldSubType: AnsiString;

begin
  FieldSubType := GetAttributeValue;

  // Binary subtypes
  if DataType = db.ftBytes then begin
    if DBICompareText(FieldSubType, 'Text') = 0 then begin
      Result := db.ftMemo;
    end
    else begin
      raise Exception.CreateFmt(UnsupportedSubtype, ['binary', FieldSubType]);
    end;
  end

  // Float subtypes
  else if DataType = db.ftFloat then begin
    if DBICompareText(FieldSubType, 'Money') = 0 then begin
      Result := db.ftCurrency;
    end
    else begin
      raise Exception.CreateFmt(UnsupportedSubtype, ['float', FieldSubType]);
    end;
  end

  // WideString subtypes
  else if DataType = db.ftWideString then begin
    if DBICompareText(FieldSubType, 'WideText') = 0 then begin
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


function TDBICustomXMLDataPacketReader.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBIXMLDataPacketLexer;
end;


function TDBICustomXMLDataPacketReader.GetMetaData: Boolean;
begin
  Result := True;

  // BootStrap the lexer;
  Input.Reset;
  Input.NextToken;

  while Result and not Input.Eof do begin
    Result := (Context <> xmlElement_RowData);
    if Result then begin
      Context := GetElement;

      while Result and not Input.Eof do begin
        ProcessColumn;

        Input.NextToken;
        Context := GetElement;
        Result := (Context <> xmlElement_RowData);
      end;
    end;
  end;
end;


function TDBICustomXMLDataPacketReader.ProcessColumn: Boolean;
var
  FieldName: String;
  DataType: TFieldType;
  FieldSize: Word;
  Required: Boolean;
  Attributes: TFieldAttributes;

begin
  Result := Context = xmlElement_Field;
  if Result then begin
    Attributes := [];
    DataType := ftUnknown;
    FieldName := '';
    FieldSize := 0;
    Required := False;
    while Result and not Input.Eof do begin
      case GetAttribute of
        xmlAttribute_AttrName: FieldName := String(GetAttributeValue);
        xmlAttribute_FieldType: DataType := GetDataType;
        xmlAttribute_Hidden: Include(Attributes, faHiddenCol);
        xmlAttribute_Link: Include(Attributes, faLink);
        xmlAttribute_ReadOnly: Include(Attributes, faReadOnly);
        xmlAttribute_Required: Required := CompareText(String(GetAttributeValue), 'true') = 0;
        xmlAttribute_Width: FieldSize := GetFieldSize;
        xmlAttribute_SubType: DataType := GetFieldSubType(DataType);
        xmlAttribute_UnNamed: Include(Attributes, faUnNamed);
      end;

      Input.NextToken;
      Result := (tsXmlElement in Input.Token.TokenStatus);
    end;

    Result := CreateColumn(FieldName, DataType, FieldSize, Required, Attributes);
  end;
end;


function TDBICustomXMLDataPacketReader.ProcessRow: Boolean;
var
  FieldName: String;
  FieldValue: String;

begin
  Result := Context = xmlElement_Row;
  if Result then begin
    RowData.Clear;

    while Result and not Input.Eof do begin
      case GetAttribute of
        xmlAttribute_RowState: GetAttributeValue;
      else
        if (Input.Token.Tokenkind = tkIdentifier) then begin
          FieldName := String(Input.Token.AsString);
          FieldValue := String(GetAttributeValue);
          RowData.Add(FieldName + '=' + FieldValue);
        end;
      end;

      Input.NextToken;
      Result := (tsXmlElement in Input.Token.TokenStatus);
    end;

    Result := CreateRow(RowData);
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
{$ifdef DebugInfo}
  inherited CreateColumn(FieldName, DataType, FieldSize, Required, Attributes);
{$endif}
  FieldDef := FieldDefs.AddFieldDef;
  FieldDef.Name := FieldName;
  FieldDef.DataType := DataType;
  FieldDef.Size := FieldSize;
  FieldDef.Attributes := Attributes;

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
{$ifdef DebugInfo}
  inherited CreateRow(RowData);
{$endif}
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


function TDBIDatasetDataPacketReader.GetFieldDefs: TFieldDefs;
begin
  Assert(Assigned(FDataset));

  Result :=  FDataset.FieldDefs;
end;


function TDBIDatasetDataPacketReader.GetRowData: TStrings;
begin
  if not Assigned(FRowData) then begin
    FRowData := TStringList.Create;
  end;
  Result := FRowData;
end;





{ TDBICustomXmlDataPacketReader }

function TDBICustomDataPacketReader.CreateColumn(
  const FieldName: String;
  const DataType: TFieldType;
  const FieldSize: Word;
  const Required: Boolean;
  const Attributes: TFieldAttributes
  ): Boolean;
const
  BoolName: array[Boolean] of String = ('False', 'True');
  FieldInfo = 'FieldName = %s, FieldType = %d, FieldSize = %d, Required = %s';
begin
  Result := True;

  Output.WriteFmt(FieldInfo, [FieldName, Ord(DataType), FieldSize, BoolName[Required] ]);
  Output.WriteLine('');
end;


function TDBICustomDataPacketReader.CreateRow(RowData: TStrings): Boolean;
begin
  Result := True;

  Output.WriteLine(RowData.Text);
end;


procedure TDBICustomDataPacketReader.PushChar(const Character: AnsiChar);
begin
  Input.PutBack(Input.Token.AsString);
  Input.PutBack(Character);
  Input.NextToken;
end;



end.
