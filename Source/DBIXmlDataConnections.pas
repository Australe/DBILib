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
  1.0 | 24/05/2013 06:15:22 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : DBILib}

unit DBIXmlDataConnections;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DB, DBIStrings,  DBITokenizers, DBITokenizerConsts;

type
  TDBIXmlAttribute = (
    xmlAttribute_None,
    xmlAttribute_AttrName,
    xmlAttribute_Change_Log,
    xmlAttribute_FieldType,
    xmlAttribute_Required,
    xmlAttribute_Width,
    xmlAttribute_RowState
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
  TDBICustomDataPacketLexer = class(TDBICustomAsciiLexer)
  protected
    procedure LexInitialise; override;

  end;

  
type
  TDBICustomDataPacketReader = class(TDBICustomParser)
  private
    FAttribute: TDBIXmlAttribute;
    FContext: TDBIXmlElement;
    FRowData: TStrings;

  protected
    function CreateColumn(
      const FieldName: String;
      const DataType: TFieldType;
      const FieldSize: Word;
      const Required: String
      ): Boolean; virtual;
      
    function CreateRow(RowData: TStrings): Boolean; virtual;

    function GetAttribute: TDBIXmlAttribute;
    function GetAttributeValue: AnsiString;
    function GetRowData: TStrings;
    function GetDataType: TFieldType;
    function GetElement: TDBIXmlElement;
    function GetFieldSize: Word;

    function GetLexerClassType: TDBICustomAsciiLexerClass; override;

    function ProcessColumn: Boolean;
    function ProcessRow: Boolean;

    procedure Skip(TokenTypes: TDBITokenTypes); overload;
    procedure Skip(TokenKinds: TDBITokenKinds); overload;

    procedure Take(ATokenType: TDBITokenType); overload;
    procedure Take(ATokenKind: TDBITokenKind); overload;

    property RowData: TStrings read GetRowData;

  public
    destructor Destroy; override;

    function GetData: Boolean;
    function GetMetaData: Boolean;

    class function XmlTimeStampToDateTime(PTimeStamp: PAnsiChar): TDateTime;

    property Attribute: TDBIXmlAttribute read GetAttribute write FAttribute;
    property AttributeValue: AnsiString read GetAttributeValue;
    property Context: TDBIXmlElement read FContext write FContext;

  end;


type
  TDBIXmlDataPacketReader = class(TDBICustomDataPacketReader)
  private
    FDataset: TDataset;

  protected
    function CreateColumn(
      const FieldName: String;
      const DataType: TFieldType;
      const FieldSize: Word;
      const Required: String
      ): Boolean; override;

    function CreateRow(RowData: TStrings): Boolean; override;

    function GetFieldDefs: TFieldDefs;

    function GetFieldDateTime(Field: TField; FieldData: String): Boolean;
    function GetFieldFloat(Field: TField; FieldData: String): Boolean;
    function GetFieldInteger(Field: TField; FieldData: String): Boolean;
    function GetFieldString(Field: TField; FieldData: String): Boolean;

    property FieldDefs: TFieldDefs read GetFieldDefs;

  public
    property Dataset: TDataset read FDataset write FDataset;
    
  end;


implementation

uses
  TypInfo, Dialogs;
  

{ TDBIXmlDataPacketReader }

function TDBIXmlDataPacketReader.CreateColumn(
  const FieldName: String;
  const DataType: TFieldType;
  const FieldSize: Word;
  const Required: String
  ): Boolean;
var
  FieldDef: TFieldDef;

begin
{$ifdef DebugReader}
  inherited CreateColumn(FieldName, DataType, FieldSize, Required);
{$endif}
  FieldDef := FieldDefs.AddFieldDef;
  FieldDef.Name := FieldName;
  FieldDef.DataType := DataType;
  FieldDef.Size := FieldSize;
  FieldDef.Required := False;

  Result := FieldDefs.Count > 0;
end;


function TDBIXmlDataPacketReader.CreateRow(RowData: TStrings): Boolean;
var
  Index: Integer;
  Field: TField;
  FieldName: String;

begin
{$ifdef DebugReader}
  inherited CreateRow(RowData);
{$endif}
  Result := RowData.Count > 0;
  if Result then begin
    Dataset.Append;

    for Index := 0 to RowData.Count-1 do begin
      FieldName := RowData.Names[Index];
      Field := Dataset.FindField(String(FieldName));
      if Assigned(Field) then begin
        case Field.DataType of
          ftDateTime: GetFieldDateTime(Field, RowData.Values[FieldName]);
          ftFloat: GetFieldFloat(Field, RowData.Values[FieldName]);
          ftInteger: GetFieldInteger(Field, RowData.Values[FieldName]);
          ftString: GetFieldString(Field, RowData.Values[FieldName]);
        else
          raise Exception.CreateFmt('DataType %s not supported', [GetEnumName(TypeInfo(TFieldType), Ord(Field.DataType))]);
        end;
      end;
    end;

    Dataset.Post;
  end;
end;


function TDBIXmlDataPacketReader.GetFieldDateTime(Field: TField; FieldData: String): Boolean;
begin
  Result := FieldData <> '';

  if Result then begin
    Field.AsDateTime := XmlTimeStampToDateTime(PAnsiChar(AnsiString(FieldData)));
  end
  else begin
    Field.Clear;
  end;
end;


function TDBIXmlDataPacketReader.GetFieldDefs: TFieldDefs;
begin
  Assert(Assigned(FDataset));

  Result :=  FDataset.FieldDefs;
end;


function TDBIXmlDataPacketReader.GetFieldFloat(Field: TField; FieldData: String): Boolean;
begin
  Result := FieldData <> '';

  if Result then begin
    Field.AsFloat := StrToFloat(String(FieldData));
  end
  else begin
    Field.Clear;
  end;
end;


function TDBIXmlDataPacketReader.GetFieldInteger(Field: TField; FieldData: String): Boolean;
begin
  Result := FieldData <> '';

  if Result then begin
    Field.AsInteger := StrToIntDef(FieldData, 0);
  end
  else begin
    Field.Clear;
  end;
end;


function TDBIXmlDataPacketReader.GetFieldString(Field: TField; FieldData: String): Boolean;
begin
  Result := FieldData <> '';

  if Result then begin
    Field.AsString := FieldData;
  end
  else begin
    Field.Clear;
  end;
end;





{ TDBICustomDataPacketProcessor }

function TDBICustomDataPacketReader.CreateColumn(
  const FieldName: String;
  const DataType: TFieldType;
  const FieldSize: Word;
  const Required: String
  ): Boolean;
const
  FieldInfo = 'FieldName = %s, FieldType = %d, FieldSize = %d, Required = %s';
begin
  Result := True;

  Output.WriteFmt(FieldInfo, [FieldName, Ord(DataType), FieldSize, Required]);
  Output.WriteLine('');
end;


function TDBICustomDataPacketReader.CreateRow(RowData: TStrings): Boolean;
begin
  Result := true;
  
  Output.WriteLine(RowData.Text);
end;


destructor TDBICustomDataPacketReader.Destroy;
begin
  FRowData.Free;
  FRowData := nil;

  inherited Destroy;
end;


function TDBICustomDataPacketReader.GetAttribute: TDBIXmlAttribute;
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


function TDBICustomDataPacketReader.GetAttributeValue: AnsiString;
var
  Data: TStringStream;

begin
  Result := '';

  Take(tkIdentifier); // Attribute Name
  Take(Tok_Equals);
  Take(Tok_Quotes);

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


function TDBICustomDataPacketReader.GetRowData: TStrings;
begin
  if not Assigned(FRowData) then begin
    FRowData := TStringList.Create;
  end;
  Result := FRowData;
end;


function TDBICustomDataPacketReader.GetData: Boolean;
begin
{##JVR
  // BootStrap the lexer;
  Input.Reset;
  Input.NextToken;
//}

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
{$ifdef DebugReader}
      Output.WriteLine('GetData is Done, LastToken = ' + Input.Token.AsString);
{$endif}
      Break;
    end;
  end;
end;


function TDBICustomDataPacketReader.GetDataType: TFieldType;
var
  FieldType: AnsiString;

begin
  FieldType := GetAttributeValue;
  if DBICompareText(FieldType, 'i4') = 0 then begin
    Result := db.ftInteger;
  end
  else if DBICompareText(FieldType, 'i2') = 0 then begin
    Result := db.ftSmallInt;
  end
{$ifdef DELPHI2009}
  else if DBICompareText(FieldType, 'i1') = 0 then begin
    Result := db.ftShortInt;
  end
  else if DBICompareText(FieldType, 'u1') = 0 then begin
    Result := db.ftByte;
  end
  else if DBICompareText(FieldType, 'u4') = 0 then begin
    Result := db.ftLongWord;
  end
  else if DBICompareText(FieldType, 'r4') = 0 then begin
    Result := db.ftSingle;
  end
  else if DBICompareText(FieldType, 'r10') = 0 then begin
    Result := db.ftExtended;
  end
{$endif}
  else if DBICompareText(FieldType, 'u2') = 0 then begin
    Result := db.ftWord;
  end
  else if DBICompareText(FieldType, 'r8') = 0 then begin
    Result := db.ftFloat;
  end
  else if DBICompareText(FieldType, 'date') = 0 then begin
    Result := db.ftDate;
  end
  else if DBICompareText(FieldType, 'time') = 0 then begin
    Result := db.ftTime;
  end
  else if DBICompareText(FieldType, 'datetime') = 0 then begin
    Result := db.ftDateTime;
  end
  else if DBICompareText(FieldType, 'string') = 0 then begin
    Result := db.ftString;
  end
  else begin
    Result := db.ftUnknown;
  end;
end;


function TDBICustomDataPacketReader.GetElement: TDBIXmlElement;
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

  Skip([Tok_Space]);
end;


function TDBICustomDataPacketReader.GetFieldSize: Word;
begin
  Result := StrToIntDef(String(GetAttributeValue), 0);
end;


function TDBICustomDataPacketReader.GetLexerClassType: TDBICustomAsciiLexerClass;
begin
  Result := TDBICustomDataPacketLexer;
end;


function TDBICustomDataPacketReader.GetMetaData: Boolean;
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


function TDBICustomDataPacketReader.ProcessColumn: Boolean;
var
  FieldName: String;
  DataType: TFieldType;
  FieldSize: Word;
  Required: String;

begin
  Result := Context = xmlElement_Field;
  if Result then begin
    FieldName := '';
    FieldSize := 0;
    DataType := ftUnknown;

    while Result and not Input.Eof do begin
      case Attribute of
        xmlAttribute_AttrName: FieldName := String(GetAttributeValue);
        xmlAttribute_FieldType: DataType := GetDataType;
        xmlAttribute_Required: Required := String(GetAttributeValue);
        xmlAttribute_Width: FieldSize := GetFieldSize;
      end;

      Input.NextToken;
      Result := (tsXmlElement in Input.Token.TokenStatus);
    end;
    
    Result := CreateColumn(FieldName, DataType, FieldSize, Required);
  end;
end;


function TDBICustomDataPacketReader.ProcessRow: Boolean;
var
  FieldName: String;
  FieldValue: String;

begin
  Result := Context = xmlElement_Row;
  if Result then begin
    RowData.Clear;

    while Result and not Input.Eof do begin
      case Attribute of
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


procedure TDBICustomDataPacketReader.Skip(TokenTypes: TDBITokenTypes);
begin
  while (Input.Token.TokenType in TokenTypes) and not Input.Eof do begin
    Input.NextToken;
  end;
end;


procedure TDBICustomDataPacketReader.Skip(TokenKinds: TDBITokenKinds);
begin
  while (Input.Token.TokenKind in TokenKinds) and not Input.Eof do begin
    Input.NextToken;
  end;
end;


procedure TDBICustomDataPacketReader.Take(ATokenType: TDBITokenType);
begin
  Assert(Input.Token.TokenType = ATokenType, Input.Token.AsString);
  Input.NextToken;
end;


procedure TDBICustomDataPacketReader.Take(ATokenKind: TDBITokenKind);
begin
  Assert(Input.Token.TokenKind = ATokenKind);
  Input.NextToken;
end;


class function TDBICustomDataPacketReader.XmlTimeStampToDateTime(PTimeStamp: PAnsiChar): TDateTime;
var
  Year, Month, Day, Hour, Minute, Sec, MSec: Word;

begin
  Year :=
    ((Ord(PTimeStamp[0])  - $30) * 1000) +
    ((Ord(PTimeStamp[1])  - $30) * 100) +
    ((Ord(PTimeStamp[2])  - $30) * 10) +
     (Ord(PTimeStamp[3])  - $30);

  Month :=
    ((Ord(PTimeStamp[4])  - $30) * 10) +
     (Ord(PTimeStamp[5])  - $30);

  Day :=
    ((Ord(PTimeStamp[6])  - $30) * 10) +
     (Ord(PTimeStamp[7])  - $30);

  Hour :=
    ((Ord(PTimeStamp[9])  - $30) * 10) +
     (Ord(PTimeStamp[10]) - $30);

  Minute :=
    ((Ord(PTimeStamp[12]) - $30) * 10) +
     (Ord(PTimeStamp[13]) - $30);

  Sec :=
    ((Ord(PTimeStamp[15]) - $30) * 10) +
     (Ord(PTimeStamp[16]) - $30);

  MSec :=
    ((Ord(PTimeStamp[17]) - $30) * 100) +
    ((Ord(PTimeStamp[18]) - $30) * 10) +
     (Ord(PTimeStamp[19]) - $30);

  Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute, Sec, MSec);
end;





{ TDBICustomDatasetLexer }

// _____________________________________________________________________________
{**
  Jvr - 26/05/2013 00:56:38 - Updated code.<br>

  Remember to Map Single Symbols before Dual Symbols
}
procedure TDBICustomDataPacketLexer.LexInitialise;
begin
  inherited LexInitialise;
{$ifdef DebugReader}
  // String Literals - toggles State only!
  MapSymbol(LexSymbol, Chr_Quotes, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);
  MapSymbol(LexSymbol, Chr_Apostrophe, Tok_Default, tkSymbol, [tsStringLiteral, tsToggle]);
{$endif}
  // Xml Elements - toggles State only!
  MapSymbol(LexSymbol, Chr_Smaller, Tok_Default, tkSymbol, [tsXmlElement]);
  MapSymbol(LexSymbol, Chr_Greater, Tok_Default, tkSymbol, [tsXmlelement, tsMask]);

  // Kind: tkWhiteSpace
  MapSymbol(LexWhiteSpace, Chr_Space, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_HorizontalTab, Tok_NoChange, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_CarriageReturn, Tok_LineBreak, tkWhiteSpace);
  MapSymbol(LexWhiteSpace, Chr_LineFeed, Tok_LineBreak, tkWhiteSpace);
{$ifdef DebugReader}
  // State: tsComment2 - /*...*/ style comments
  MapDualSymbol(LexNone, Chr_Slash_Star, Tok_Slash_Star, tkSymbol, [tsComment2]);
  MapDualSymbol(LexNone, Chr_Star_Slash, Tok_Star_Slash, tkSymbol, [tsComment2, tsMask]);
{$endif}
  // Xml Elements - toggles State only!
  MapDualSymbol(LexNone, Chr_Smaller_Slash, Tok_Smaller_Slash, tkSymbol, [tsXmlElement]);
  MapDualSymbol(LexNone, Chr_Slash_Greater, Tok_Slash_Greater, tkSymbol, [tsXmlElement, tsMask]);
end;


end.
