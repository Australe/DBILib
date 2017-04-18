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
  1.0 | 27/02/2001 17:47:37 | Jvr | Initial Release
  1.1 | 12/07/2013 08:04:02 | Jvr | Refactored
  1.2 | 17/12/2013 08:04:02 | Jvr | Added CSV support
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIDataPacketWriters;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DB, DBIStreamAdapters, DBIStrings, DBIDataset;

type
  TDBIMetadataAttribute = (
    maAttrName,
    maFieldType,
    maRequired,
    maSubType,
    maWidth
    );

const
  // FieldTypes
  cdsFieldADT =              'adt';
  cdsFieldAnsiString =       'string';
  cdsFieldWideString =       'string.uni';
  cdsFieldBoolean =          'boolean';
  cdsFieldShortInt =         'i1';
  cdsFieldSmallInt =         'i2';
  cdsFieldInteger =          'i4';
  cdsFieldLargeInt =         'i8';
  cdsFieldByte =             'ui1';
  cdsFieldWord =             'ui2';
  cdsFieldLongWord =         'ui4';
  cdsFieldSingle =           'r4';
  cdsFieldFloat =            'r8';
  cdsFieldExtended =         'r10';
  cdsFieldBCD =              'fixed';
  cdsFieldDate =             'date';
  cdsFieldTime =             'time';
  cdsFieldDateTime =         'datetime';
  cdsFieldBytes =            'bin.hex';

  cdsFieldSubTypeAutoInc =   'Autoinc';
  cdsFieldSubTypeCurrency =  'Money';
  cdsFieldSubTypeVarBytes =  'Binary';
  cdsFieldSubTypeMemo =      'Text';
  cdsFieldSubTypeGraphic =   'Graphics';
  cdsFieldSubTypeFmtMemo =   'Formatted';
  cdsFieldSubTypeOle =       'Ole';
  cdsFieldSubTypeWideText =  'WideText';


type
  TDBICustomDataPacketWriter = class(TDBICustomStreamFormatter)
  private
    FDataset: TDataset;

  protected
    procedure SetDataset(Value: TDataset); virtual;

    procedure WriteAttribute(const Attribute: TDBIMetadataAttribute; const AttributeValue: String); virtual;
    procedure WriteData; virtual; abstract;
    procedure WriteFieldAttribute(Attribute: TFieldAttribute); virtual;
    procedure WriteFieldAttributes(FieldDef: TFieldDef);
    procedure WriteFieldWidth(FieldDef: TFieldDef); virtual;
    procedure WriteFieldType(FieldDef: TFieldDef); overload;
    procedure WriteFieldType(const AFieldType: String; const ASubType: String = ''; const ReadOnly: Boolean = False); overload; virtual;
    procedure WriteFooter; virtual; abstract;
    procedure WriteHeader; virtual; abstract;
    procedure WriteMetaData; virtual; abstract;

    property Dataset: TDataset read FDataset write SetDataset;

  public
    procedure WriteDataset; virtual;

  end;


type
  TDBIXMLDataPacketWriter = class(TDBICustomDataPacketWriter)
  protected
    function EncodeFieldData(FieldData: String): String;
    function EncodeFieldName(FieldName: String): String;

    procedure SetDataset(Value: TDataset); override;

    procedure WriteAttribute(const Attribute: TDBIMetadataAttribute; const AttributeValue: String); override;
    procedure WriteAttributeName(FieldDef: TFieldDef);
    procedure WriteData; override;
    procedure WriteFieldAttribute(Attribute: TFieldAttribute); override;
    procedure WriteFieldData(Field: TField);
    procedure WriteFieldName(FieldDef: TFieldDef);
    procedure WriteFieldType(const AFieldType: String; const ASubType: String = ''; const ReadOnly: Boolean = False); overload; override;

    procedure WriteFieldWidth(FieldDef: TFieldDef); override;
    procedure WriteHeader; override;
    procedure WriteMetaData; override;
    procedure WriteFooter; override;

  public
    property Dataset;

  public
    class procedure SaveToFile(const AFileName: TFilename; ADataSet: TDataSet); overload;
    class procedure SaveToStream(Stream: TStream; DataSet: TDataSet); overload;

  end;


type
  TDBIJsonDataPacketWriter = class(TDBICustomDataPacketWriter)
  protected
    function EncodeFieldData(FieldData: String): String;
    function EncodeFieldName(FieldName: String): String;

    procedure SetDataset(Value: TDataset); override;

    procedure WriteAttributeName(FieldDef: TFieldDef);
    procedure WriteData; override;
    procedure WriteFieldAttribute(Attribute: TFieldAttribute); override;
    procedure WriteFieldData(Field: TField);
    procedure WriteFieldName(FieldDef: TFieldDef);
    procedure WriteFieldType(const AFieldType: String; const ASubType: String = ''; const ReadOnly: Boolean = False); overload; override;
    procedure WriteFieldWidth(FieldDef: TFieldDef); override;
    procedure WriteHeader; override;
    procedure WriteMetaData; override;
    procedure WriteFooter; override;

  public
    property Dataset;

  public
    class procedure SaveToStream(Stream: TStream; DataSet: TDataSet); overload;
    class procedure SaveToFile(const AFileName: TFileName; ADataSet: TDataSet); overload;
  end;


type
  TDBICSVDataPacketWriter = class(TDBICustomDataPacketWriter)
  protected
    function EncodeFieldData(FieldData: String): String;

    procedure SetDataset(Value: TDataset); override;

    procedure WriteData; override;
    procedure WriteFieldData(Field: TField);
    procedure WriteHeader; override;
    procedure WriteMetaData; override;
    procedure WriteFooter; override;

  public
    property Dataset;

  public
    class procedure SaveToFile(const AFileName: TFilename; ADataSet: TDataSet); overload;
    class procedure SaveToStream(Stream: TStream; DataSet: TDataSet); overload;
  end;


const
  // faHiddenCol, faReadonly, faRequired, faLink, faUnNamed, faFixed
  TDBIFieldAttributeName: array[TFieldAttribute] of String = (
    'hidden', 'readonly', 'required', 'link', 'unnamed', 'fixed'
    );



implementation

uses
  TypInfo, DBIConst, DBIUtils, DBIFileStreams;

const
  // Date Conversion format
  cdsDateFormat = 'YYYYMMDD';
  cdsDateTimeFormat = 'YYYYMMDD HH:NN:SSZZZ';


{ TDBICSVDataPacketWriter }

function TDBICSVDataPacketWriter.EncodeFieldData(FieldData: String): String;
const
  Quotes = #34;

begin
  // Delimit field and esacpe double quotes
  Result := Quotes + StringReplace(FieldData, Quotes, Quotes+Quotes, [rfReplaceAll]) + Quotes;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
class procedure TDBICsvDataPacketWriter.SaveToFile(const AFileName: TFileName; ADataSet: TDataSet);
var
  LocalStream: TStream;

begin
  try
    LocalStream := Local(
      TDBIFileStream.Create(AFileName, fmCreate, DBIPageBufferSize, [])
      ).Obj as TStream;

    SaveToStream(LocalStream, ADataSet);
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'SaveToFile::245', 'Failed to save Dataset as CSV', []);
  end;
end;


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style CSV stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
class procedure TDBICsvDataPacketWriter.SaveToStream(Stream: TStream; DataSet: TDataSet);
var
  Writer: TDBICSVDataPacketWriter;

begin
  Writer := Local(TDBICSVDataPacketWriter.Create).Obj as TDBICSVDataPacketWriter;
  Writer.Dataset := Dataset;
  Writer.SaveToStream(Stream);
end;


procedure TDBICSVDataPacketWriter.SetDataset(Value: TDataset);
begin
  inherited SetDataset(Value);

  if Assigned(Value) then begin
    WriteDataset;
  end
  else begin
    Stream.Size := 0;
  end;
end;


procedure TDBICSVDataPacketWriter.WriteData;
const
  FieldSeparator: array[Boolean] of String = (#44, #13);

var
  Index: Integer;

begin

  // Write CSV Data to stream
  while not Dataset.Eof do begin
    for Index := 0 to Dataset.Fields.Count-1 do begin
      // if the field is unnamed then I can't save it, so skip it
      // Yay, now it's a feature - prevent specific fields from saving
      if (db.faUnNamed in Dataset.FieldDefs[Index].Attributes) then begin
        Continue;
      end;

      WriteFieldData(Dataset.Fields[Index]);
      WriteStr(FieldSeparator[Index = Dataset.FieldCount-1]);
    end;

    Dataset.Next;
  end;
end;


procedure TDBICSVDataPacketWriter.WriteFieldData(Field: TField);
var
  FieldData: String;

begin
  case Field.DataType of
    ftDate: begin
      FieldData := FormatDateTime(cdsDateFormat, Field.AsDateTime);
    end;

    ftDateTime: begin
      FieldData := FormatDateTime(cdsDateTimeFormat, Field.AsDateTime);
    end;

  // All other fieldtypes
  else
    FieldData := Field.AsString;
  end;

  WriteStr(EncodeFieldData(FieldData));
end;


procedure TDBICSVDataPacketWriter.WriteFooter;
begin
  //##NOP
end;


procedure TDBICSVDataPacketWriter.WriteHeader;
begin
  //##NOP
end;


procedure TDBICSVDataPacketWriter.WriteMetaData;
const
  FieldDelimiter = #34;
  FieldSeparator: array[Boolean] of String = (#44, #13);

var
  FieldIndex: Integer;

begin
  for FieldIndex := 0 to Dataset.FieldDefs.Count-1 do begin
    WriteStr(FieldDelimiter + Dataset.FieldDefs[FieldIndex].Name + FieldDelimiter);
    WriteStr(FieldSeparator[FieldIndex = Dataset.FieldDefs.Count-1]);
  end;
end;





{ TDBIJsonDataPacketWriter }

function TDBIJsonDataPacketWriter.EncodeFieldName(FieldName: String): String;
var
  Index: Integer;

begin
  for Index := Length(FieldName) downto 1 do begin
    if not (UpCase(TDBIString(FieldName)[Index]) in ['A'..'Z','1'..'9']) then begin
      FieldName[Index] := '_';
    end;
  end;
  Result := FieldName;
end;


function TDBIJsonDataPacketWriter.EncodeFieldData(FieldData: String): String;
const
  EscapedCharacters: array[8..13] of String = ('\b', '\t', '\n', '\u000B', '\f', '\r');
  Quotation = 34;
  ForwardSlash = 47;
  BackSlash = 92;
  Quotes = #34;

var
  Index: Integer;

begin
  // Escape special characters
  for Index := Length(FieldData) downto 1 do begin
    if Ord(FieldData[Index]) in [8..13] then begin
      Insert(EscapedCharacters[Ord(FieldData[Index])], FieldData, Index+1);
      Delete(FieldData, Index, 1)
    end
    else if Ord(FieldData[Index]) = Quotation then begin
      Insert('\', FieldData, Index);
    end
    else if Ord(FieldData[Index]) = ForwardSlash then begin
      Insert('\', FieldData, Index);
    end
    else if Ord(FieldData[Index]) = BackSlash then begin
      Insert('\', FieldData, Index);
    end
    else if Ord(FieldData[Index]) in [1..31] then begin
      Insert('\u' + IntToHex(Ord(FieldData[Index]), 4), FieldData, Index+1);
      Delete(FieldData, Index, 1)
    end
    else if Ord(FieldData[Index]) = 0 then begin
      Delete(FieldData, Index, 1);
    end;
  end;

  Result := Quotes + FieldData + Quotes;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
class procedure TDBIJsonDataPacketWriter.SaveToFile(const AFileName: TFileName; ADataSet: TDataSet);
var
  LocalStream: TStream;

begin
  try
    LocalStream := Local(
      TDBIFileStream.Create(AFileName, fmCreate, DBIPageBufferSize, [])
      ).Obj as TStream;

    SaveToStream(LocalStream, ADataSet);
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'SaveToFile::435', 'Failed to save Dataset as JSON', []);
  end;
end;


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style JSON stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
class procedure TDBIJsonDataPacketWriter.SaveToStream(Stream: TStream; DataSet: TDataSet);
var
  Writer: TDBIJsonDataPacketWriter;

begin
  Writer := Local(Self.Create).Obj as Self;
  Writer.Dataset := Dataset;
  Writer.SaveToStream(Stream);
end;


procedure TDBIJsonDataPacketWriter.SetDataset(Value: TDataset);
begin
  inherited SetDataset(Value);

  if Assigned(Value) then begin
    WriteDataset;
  end
  else begin
    Stream.Size := 0;
  end;
end;


procedure TDBIJsonDataPacketWriter.WriteAttributeName(FieldDef: TFieldDef);
begin
  WriteFmt('"attrname": "%s"', [EncodeFieldName(FieldDef.Name)]);
end;


procedure TDBIJsonDataPacketWriter.WriteData;
const
  jsonFieldSeparator: array[Boolean] of String = (', ', '');
  jsonRowDataHeader = '"rowdata": [';
  jsonRowDataBegin  = '  {';
  jsonRowDataEnd: array[Boolean] of String = ('},', '}');
  jsonRowDataFooter = ']';

var
  Index: Integer;

begin
  // Write JSON Data to stream
  WriteLine(jsonRowDataHeader);

  while not Dataset.Eof do begin
    WriteStr(jsonRowDataBegin);

    for Index := 0 to Dataset.Fields.Count-1 do begin
      // if the field is unnamed then I can't save it, so skip it
      // Yay, now it's a feature - prevent specific fields from saving
      if (db.faUnNamed in Dataset.FieldDefs[Index].Attributes) then begin
        Continue;
      end;

      WriteFieldData(Dataset.Fields[Index]);
      WriteStr(jsonFieldSeparator[Index = Dataset.FieldCount-1]);
    end;

    Dataset.Next;
    WriteLine(jsonRowDataEnd[Dataset.Eof]);
  end;

  WriteLine(jsonRowDataFooter);
end;


procedure TDBIJsonDataPacketWriter.WriteFieldAttribute(Attribute: TFieldAttribute);
begin
  WriteFmt(', "%s": "%s"', [ TDBIFieldAttributeName[Attribute], 'true' ]);
end;


procedure TDBIJsonDataPacketWriter.WriteFieldData(Field: TField);
const
  BoolName: array[Boolean] of String = ('false', 'true');
  Quotes = #34;
  Colon = ':';
  Space = ' ';

var
  FieldData: String;

begin
  case Field.DataType of
    ftBoolean: begin
      FieldData := BoolName[Field.AsBoolean];
    end;

    ftDate: begin
      FieldData := FormatDateTime(cdsDateFormat, Field.AsDateTime);
    end;

    ftDateTime: begin
      FieldData := FormatDateTime(cdsDateTimeFormat, Field.AsDateTime);
    end;

  // All other fieldtypes
  else
    FieldData := Field.AsString;
  end;

  WriteStr(Quotes + EncodeFieldName(Field.FieldName) + Quotes + Colon + Space);
  case Field.DataType of
{$ifdef DELPHI2009}
    db.ftByte,
    db.ftLongWord,
    db.ftShortint,
    db.ftSingle,
    db.ftExtended,
{$endif}
    db.ftAutoInc,
    db.ftBoolean,
    db.ftFloat,
    db.ftCurrency,
    db.ftWord,
    db.ftSmallint,
    db.ftInteger,
    db.ftLargeint: WriteStr(FieldData);

  else
    WriteStr(EncodeFieldData(FieldData));
  end;
end;


procedure TDBIJsonDataPacketWriter.WriteFieldName(FieldDef: TFieldDef);
begin
  if EncodeFieldName(FieldDef.Name) <> FieldDef.Name then begin
    WriteFmt('"fieldname": "%s", ', [FieldDef.Name]);
  end;
end;


procedure TDBIJsonDataPacketWriter.WriteFieldType(const AFieldType: String; const ASubType: String = ''; const ReadOnly: Boolean = False);
begin
  WriteStr(', "fieldtype": "' + AFieldType + '"');

  if ReadOnly then begin
    WriteStr(', "readonly": "true"');
  end;

  if (ASubType <> '') then begin
    WriteStr(', "subtype": "' + ASubType + '"');
  end;
end;


procedure TDBIJsonDataPacketWriter.WriteFieldWidth(FieldDef: TFieldDef);
const
  FieldSizeOf: array[Boolean] of Integer = (1, 2);
  FieldWidth = ', "width": %d';

begin
  if (FieldDef.Size > 0) then begin
    WriteFmt(FieldWidth, [FieldSizeOf[FieldDef.DataType = ftWideString] * FieldDef.Size]);
  end;
end;


procedure TDBIJsonDataPacketWriter.WriteFooter;
const
  jsonDataPacketFooter =     '}';

begin
  WriteLine(jsonDataPacketFooter);
end;


procedure TDBIJsonDataPacketWriter.WriteHeader;
const
  jsonDataPacketHeader = '"version": 1.00,';

begin
  WriteLine('{');
  WriteLine(jsonDataPacketHeader);
end;

{$define UseMetaData True}

procedure TDBIJsonDataPacketWriter.WriteMetaData;
const
  jsonMetaDataHeader = '"metadata": {';
  jsonFieldsHeader   = '  "fields": [';
  jsonFieldBegin     = '    {';
  jsonFieldEnd: array[Boolean] of String = ('}', '},');
  jsonFieldsFooter   = '  ],';
  jsonParams         = '  "params": null';
  jsonMetaDataFooter = '},';

var
  Index: Integer;
  FieldDefs: TFieldDefs;

begin
{$ifdef UseMetaData}
  // Write cds Metadata to stream
  WriteLine(jsonMetaDataHeader);
  WriteLine(jsonFieldsHeader);

  // Get info without opening the database
  FieldDefs := Dataset.FieldDefs;
  if not Dataset.Active then begin
    FieldDefs.Update;
  end;

  for Index := 0 to FieldDefs.Count-1 do begin
    WriteStr(jsonFieldBegin);

    WriteFieldName(FieldDefs[Index]);
    WriteAttributeName(FieldDefs[Index]);
    WriteFieldType(FieldDefs[Index]);
    WriteFieldAttributes(FieldDefs[Index]);
    WriteFieldWidth(FieldDefs[Index]);

    WriteLine(jsonFieldEnd[Index < Pred(FieldDefs.Count)]);
  end;

  WriteLine(jsonFieldsFooter);
  WriteLine(jsonParams);
  WriteLine(jsonMetaDataFooter);
{$endif}
end;





{ TDBIXmlDataPacketWriter }

// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 13:16:54 - Converts a fieldname with illegal fieldname chars
                              to a valid fieldname.<P>
}
function TDBIXMLDataPacketWriter.EncodeFieldName(FieldName: String): String;
var
  Index: Integer;

begin
  for Index := Length(FieldName) downto 1 do begin
    if not (UpCase(TDBIString(FieldName)[Index]) in ['A'..'Z','1'..'9']) then begin
      FieldName[Index] := '_';
    end;
  end;

  Result := FieldName;
end;


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 13:14:59 - Converts field contents to single line of XML.<P>
}
function TDBIXMLDataPacketWriter.EncodeFieldData(FieldData: String): String;
var
  Index: Integer;

begin
  for Index := Length(FieldData) downto 1 do begin
    // escape all characters less than 32 (space) and greater than 126
    if (Ord(FieldData[Index]) in [1..31, 127..255]) then begin
      Insert('&#' + IntToStr(Ord(FieldData[Index])) + ';', FieldData, Index+1);
      Delete(FieldData, Index ,1)
    end

    // escape double quotes
    else if (FieldData[Index] = #34) then begin
      Insert('&quot;', FieldData, Index+1);
      Delete(FieldData, Index ,1)
    end

    // escape apostrophe
    else if (FieldData[Index] = #39) then begin
      Insert('&apos;', FieldData, Index+1);
      Delete(FieldData, Index ,1)
    end

    // escape lesser than
    else if (FieldData[Index] = #60) then begin
      Insert('&lt;', FieldData, Index+1);
      Delete(FieldData, Index ,1)
    end

    // escape greater than
    else if (FieldData[Index] = #62) then begin
      Insert('&gt;', FieldData, Index+1);
      Delete(FieldData, Index ,1)
    end

    // escape ampersand
    else if (FieldData[Index] = #38) then begin
      Insert('&amp;', FieldData, Index+1);
      Delete(FieldData, Index ,1)
    end

    else if (FieldData[Index] = #0) then begin
      Delete(FieldData, Index, 1);
    end;
  end;
  Result := FieldData;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
class procedure TDBIXMLDataPacketWriter.SaveToFile(const AFileName: TFileName; ADataSet: TDataSet);
var
  LocalStream: TStream;

begin
  try
    LocalStream := Local(
      TDBIFileStream.Create(AFileName, fmCreate, DBIPageBufferSize, [])
      ).Obj as TStream;

    SaveToStream(LocalStream, ADataSet);
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'SaveToFile::145', 'Failed to save Dataset as Xml', []);
  end;
end;


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style Xml stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
class procedure TDBIXMLDataPacketWriter.SaveToStream(Stream: TStream; DataSet: TDataSet);
var
  Writer: TDBIXmlDataPacketWriter;

begin
  Writer := Local(TDBIXmlDataPacketWriter.Create).Obj as TDBIXmlDataPacketWriter;
  Writer.Dataset := Dataset;
  Writer.SaveToStream(Stream);
end;


procedure TDBIXMLDataPacketWriter.SetDataset(Value: TDataset);
begin
  inherited SetDataset(Value);

  if Assigned(Value) then begin
    WriteDataset;
  end
  else begin
    Stream.Size := 0;
  end;
end;


procedure TDBIXMLDataPacketWriter.WriteAttribute(
  const Attribute: TDBIMetadataAttribute;
  const AttributeValue: String
  );
var
  AttributeTypeName: String;

begin
  AttributeTypeName := Copy(GetEnumName(TypeInfo(TDBIMetadataAttribute), Ord(Attribute)), 3, 128);
  WriteFmt('%s="%s"', [AttributeTypeName, AttributeValue]);
end;


procedure TDBIXMLDataPacketWriter.WriteAttributeName(FieldDef: TFieldDef);
begin
  WriteFmt(' attrname="%s"', [EncodeFieldName(FieldDef.Name)]);
end;


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 15:46:30.<P>
}
procedure TDBIXMLDataPacketWriter.WriteData;
const
  cdsRowDataBegin  = '<ROW ';
  cdsRowDataEnd    = '/>';
  cdsRowDataFooter = '</ROWDATA>';
  cdsRowDataHeader = '<ROWDATA>';

var
  Index: Integer;

begin
  // Write cds Data to stream
  WriteLine(cdsRowDataHeader);

  while not Dataset.Eof do begin
    WriteStr(cdsRowDataBegin);

    for Index := 0 to Dataset.Fields.Count-1 do begin
      // if the field is unnamed then I can't save it, so skip it
      // Yay, now it's a feaure - prevent specific fields from saving
      if (db.faUnNamed in Dataset.FieldDefs[Index].Attributes) then begin
        Continue;
      end;

      WriteFieldData(Dataset.Fields[Index]);
    end;

    WriteLine(cdsRowDataEnd);
    Dataset.Next;
  end;

  WriteLine(cdsRowDataFooter);
end;


procedure TDBIXMLDataPacketWriter.WriteFieldAttribute(Attribute: TFieldAttribute);
begin
  WriteFmt(' %s="%s"', [ TDBIFieldAttributeName[Attribute], 'true' ]);
end;


procedure TDBIXMLDataPacketWriter.WriteFieldData(Field: TField);
var
  FieldData: String;
  FieldText: String;
  DisplayText: String;
  HasData: Boolean;

begin
  case Field.DataType of
    ftDate: begin
      FieldData := FormatDateTime(cdsDateFormat, Field.AsDateTime);
    end;

    ftDateTime: begin
      FieldData := FormatDateTime(cdsDateTimeFormat, Field.AsDateTime);
    end;

  // All other fieldtypes
  else
    FieldData := Field.AsString;
  end;


  // DisplayText in Currency fields are preceded by the currency symbol
  DisplayText := Field.DisplayText;

  if (Field.DataType = ftCurrency) then begin
    FieldText := CurrToStr(Field.AsCurrency);
  end
  else begin
    FieldText := FieldData;
  end;

  HasData := (FieldData <> '') and ((FieldText = FieldData) or (DisplayText = '(MEMO)'));
  if HasData then begin
    WriteStr(EncodeFieldName(Field.FieldName) + '="');
    WriteStr(EncodeFieldData(FieldData) + '" ');
  end;
end;


procedure TDBIXMLDataPacketWriter.WriteFieldName(FieldDef: TFieldDef);
begin
  if EncodeFieldName(FieldDef.Name) <> FieldDef.Name then begin
    WriteFmt(' fieldname="%s"', [FieldDef.Name]);
  end;
end;


procedure TDBIXMLDataPacketWriter.WriteFieldType(const AFieldType: String; const ASubType: String = ''; const ReadOnly: Boolean = False);
begin
  WriteStr(' fieldtype="' + AFieldType + '"');

  if ReadOnly then begin
    WriteStr(' readonly="true"');
  end;

  if (ASubType <> '') then begin
    WriteStr(' SUBTYPE="' + ASubType + '"');
  end;
end;


procedure TDBIXMLDataPacketWriter.WriteFieldWidth(FieldDef: TFieldDef);
const
  FieldSizeOf: array[Boolean] of Integer = (1, 2);
  FieldWidth = ' width="%d"';

begin
  if (FieldDef.Size > 0) then begin
    WriteFmt(FieldWidth, [FieldSizeOf[FieldDef.DataType = ftWideString] * FieldDef.Size]);
  end;
end;


procedure TDBIXMLDataPacketWriter.WriteFooter;
const
  cdsDataPacketFooter = '</DATAPACKET>';

begin
  WriteLine(cdsDataPacketFooter);
end;


procedure TDBIXMLDataPacketWriter.WriteHeader;
const
  cdsVersion = '<?xml version="1.0" standalone="yes"?>';
  cdsDataPacketHeader = '<DATAPACKET Version="2.0">';

begin
  WriteLine(cdsVersion);
  WriteLine(cdsDataPacketHeader);
end;


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style Xml stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
procedure TDBIXMLDataPacketWriter.WriteMetaData;
const
  cdsMetaDataHeader = '<METADATA>';
  cdsFieldsHeader = '<FIELDS>';
  cdsFieldsFooter = '</FIELDS>';
  cdsMetaDataFooter = '</METADATA>';
  cdsParams = '<PARAMS/>';

var
  Index: Integer;
  FieldDefs: TFieldDefs;

begin
  // Write cds Metadata to stream
  WriteLine(cdsMetaDataHeader);
  WriteLine(cdsFieldsHeader);

  // Get info without opening the database
  FieldDefs := Dataset.FieldDefs;
  if not Dataset.Active then begin
    FieldDefs.Update;
  end;

  for Index := 0 to FieldDefs.Count-1 do begin
    WriteStr('<FIELD');

    WriteFieldName(FieldDefs[Index]);
    WriteAttributeName(FieldDefs[Index]);
    WriteFieldType(FieldDefs[Index]);
    WriteFieldAttributes(FieldDefs[Index]);
    WriteFieldWidth(FieldDefs[Index]);

    WriteLine('/>');
  end;

  WriteLine(cdsFieldsFooter);
  WriteLine(cdsParams);
  WriteLine(cdsMetaDataFooter);
end;





{ TDBICustomDataPacketWriter }

procedure TDBICustomDataPacketWriter.WriteAttribute(
  const Attribute: TDBIMetadataAttribute;
  const AttributeValue: String
  );
begin
  //##NOP
end;


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
procedure TDBICustomDataPacketWriter.WriteDataset;
begin
  WriteHeader;

  if not Dataset.Active then begin
    Dataset.Open;
  end;

  Dataset.DisableControls;
  try
    Dataset.First;

    WriteMetaData;
    WriteData;
  finally
    Dataset.EnableControls;
  end;

  WriteFooter;
end;


procedure TDBICustomDataPacketWriter.WriteFieldAttribute(Attribute: TFieldAttribute);
begin
  //##NOP
end;


procedure TDBICustomDataPacketWriter.WriteFieldAttributes(FieldDef: TFieldDef);
var
  FieldAttribute: TFieldAttribute;

begin
  for FieldAttribute := Low(TFieldAttribute) to High(FieldAttribute) do begin
    if (FieldAttribute in FieldDef.Attributes) then begin
      WriteFieldAttribute(FieldAttribute);
    end;
  end;
end;


procedure TDBICustomDataPacketWriter.WriteFieldType(FieldDef: TFieldDef);
begin
  case FieldDef.DataType of
    db.ftADT:         WriteFieldType(cdsFieldADT);
    db.ftString,
    db.ftFixedChar:   WriteFieldType(cdsFieldAnsiString);
    db.ftWideString:  WriteFieldType(cdsFieldWideString {$ifndef Delphi2006}, cdsFieldSubTypeWideText{$endif});
    db.ftBoolean:     WriteFieldType(cdsFieldBoolean);
{$ifdef DELPHI2009}
    db.ftShortInt:    WriteFieldType(cdsFieldShortInt);
{$endif}
    db.ftSmallInt:    WriteFieldType(cdsFieldSmallInt);
    db.ftInteger:     WriteFieldType(cdsFieldInteger);
    db.ftLargeInt:    WriteFieldType(cdsFieldLargeInt);
    db.ftAutoInc:     WriteFieldType(cdsFieldInteger, cdsFieldSubTypeAutoInc, True);
    db.ftWord:        WriteFieldType(cdsFieldWord);
{$ifdef DELPHI2009}
    db.ftByte:        WriteFieldType(cdsFieldByte);
    db.ftLongWord:    WriteFieldType(cdsFieldLongWord);

    // There is a bug in TClientDataset for TExtendedField,
    // so this field type has been mapped to double instead
    db.ftExtended:    WriteFieldType(cdsFieldFloat);
    //##JVR ftExtended:    WriteFldType(cdsFieldExtended);

    // Guess what now they have broken TSingleField as well,
    // so this field type has been mapped to double instead
    db.ftSingle:      WriteFieldType(cdsFieldFloat);
    //##JVR ftSingle:      WriteFldType(cdsFieldSingle);

{$endif}
    db.ftFloat:       WriteFieldType(cdsFieldFloat);
    db.ftCurrency:    WriteFieldType(cdsFieldFloat, cdsFieldSubTypeCurrency);
    db.ftBCD:         WriteFieldType(cdsFieldBCD);
    db.ftDate:        WriteFieldType(cdsFieldDate);
    db.ftTime:        WriteFieldType(cdsFieldTime);
    db.ftDateTime:    WriteFieldType(cdsFieldDateTime);
    db.ftBytes:       WriteFieldType(cdsFieldBytes);
    db.ftVarBytes,
    db.ftBlob:        WriteFieldType(cdsFieldBytes, cdsFieldSubTypeVarBytes);
    db.ftMemo:        WriteFieldType(cdsFieldBytes, cdsFieldSubTypeMemo);
    db.ftGraphic,
    db.ftTypedBinary: WriteFieldType(cdsFieldBytes, cdsFieldSubTypeGraphic);
    db.ftFmtMemo:     WriteFieldType(cdsFieldBytes, cdsFieldSubTypeFmtMemo);
    db.ftParadoxOle,
    db.ftDBaseOle:    WriteFieldType(cdsFieldBytes, cdsFieldSubTypeOle);
  end;
end;


procedure TDBICustomDataPacketWriter.WriteFieldType(
  const AFieldType: String;
  const ASubType: String = '';
  const ReadOnly: Boolean = False
  );
begin
  //##NOP
end;


procedure TDBICustomDataPacketWriter.WriteFieldWidth;
begin
  //##NOP
end;


procedure TDBICustomDataPacketWriter.SetDataset(Value: TDataset);
begin
  FDataset := Value;
end;

end.
