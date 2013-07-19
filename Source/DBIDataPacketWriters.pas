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
  1.0 | 27/02/2001 17:47:37 | Jvr | Initial Release
  1.0 | 12/07/2013 08:04:02 | Jvr | Refactored
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBIDataPacketWriters;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DB, DBIStreamAdapters, DBIStrings, DBIDataset;

type
  TDBICustomDataPacketWriter = class(TDBICustomStreamFormatter)
  private
    FDataset: TDataset;

  protected
    procedure SetDataset(Value: TDataset); virtual;

    procedure WriteData; virtual; abstract;
    procedure WriteFooter; virtual; abstract;
    procedure WriteHeader; virtual; abstract;
    procedure WriteMetaData; virtual; abstract;

    property Dataset: TDataset read FDataset write SetDataset;

  public
    procedure WriteDataset; virtual;

  end;


  TDBIXmlDataPacketWriter = class(TDBICustomDataPacketWriter)
  protected
    function EncodeFieldData(FieldData: String): String;
    function EncodeFieldName(FieldName: String): String;

    procedure SetDataset(Value: TDataset); override;

    procedure WriteData; override;
    procedure WriteField(Field: TField);
    procedure WriteHeader; override;
    procedure WriteMetaData; override;
    procedure WriteFooter; override;

  public
    property Dataset;
        
  end;


const
  // FieldTypes
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
  cdsFieldAutoInc =          'i4" readonly="true" SUBTYPE="Autoinc';
  cdsFieldSingle =           'r4';
  cdsFieldFloat =            'r8';
  cdsFieldExtended =         'r10';
  cdsFieldCurrency =         'r8" SUBTYPE="Money';
  cdsFieldBCD =              'fixed';
  cdsFieldDate =             'date';
  cdsFieldTime =             'time';
  cdsFieldDateTime =         'datetime';
  cdsFieldBytes =            'bin.hex';
  cdsFieldVarBytes =         'bin.hex" SUBTYPE="Binary';
  cdsFieldMemo =             'bin.hex" SUBTYPE="Text';
  cdsFieldGraphic =          'bin.hex" SUBTYPE="Graphics';
  cdsFieldFmtMemo =          'bin.hex" SUBTYPE="Formatted';
  cdsFieldOle =              'bin.hex" SUBTYPE="Ole';
  cdsFieldReadOnly =         '" readonly="true';
  cdsFieldRequired =         '" required="true';
  cdsFieldSubTypeWideText =  '" SUBTYPE="WideText';


implementation

uses
  TypInfo, DBIConst, DBIIntfConsts, DBIFileStreams;



{ TDBIXmlDataPacketWriter }

const
  // Date Conversion format
  cdsDateFormat = 'YYYYMMDD';
  cdsDateTimeFormat = 'YYYYMMDD HH:NN:SSZZZ';

  // Tags
  cdsVersion =               '<?xml version="1.0" standalone="yes"?>  ';
  cdsDataPacketHeader =      '<DATAPACKET Version="2.0">';
  cdsMetaDataHeader =        '<METADATA>';
  cdsFieldsHeader =          '<FIELDS>';
  cdsFieldsFooter =          '</FIELDS>';
  cdsMetaDataFooter =        '</METADATA>';
  cdsParams =                '<PARAMS/>';
  cdsRowDataHeader =         '<ROWDATA>';
  cdsRowDataFooter =         '</ROWDATA>';
  cdsDataPacketFooter =      '</DATAPACKET>';


// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 13:16:54 - Converts a fieldname with illegal fieldname chars
                              to a valid fieldname.<P>
}
function TDBIXmlDataPacketWriter.EncodeFieldName(FieldName: String): String;
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
function TDBIXmlDataPacketWriter.EncodeFieldData(FieldData: String): String;
var
  Index: Integer;

begin
  for Index := Length(FieldData) downto 1 do begin
    if (Ord(FieldData[Index]) in [1..31, 34, 38..39]) then begin
      Insert('&#' + IntToStr(Ord(FieldData[Index])) + ';', FieldData, Index+1);
      Delete(FieldData, Index ,1)
    end
    else if (FieldData[Index] = #0) then begin
      Delete(FieldData, Index, 1);
    end;
  end;

  Result := FieldData
end;


procedure TDBIXmlDataPacketWriter.SetDataset(Value: TDataset);
begin
  inherited SetDataset(Value);

  if Assigned(Value) then begin
    WriteDataset;
  end
  else begin
    Stream.Size := 0;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 15:46:30.<P>
}
procedure TDBIXmlDataPacketWriter.WriteData;
var
  Index: Integer;

begin
  // Write cds Data to stream
  WriteStr(cdsRowDataHeader);

  while not Dataset.Eof do begin
    WriteStr('<ROW ');

    for Index := 0 to Dataset.Fields.Count-1 do begin
{##JVR
      // if the field is readonly then skip
      if (db.faReadOnly in Dataset.FieldDefs[Index].Attributes) then begin
        Continue;
      end;
//}
      WriteField(Dataset.Fields[Index]);
    end;

    WriteStr('/>');
    Dataset.Next;
  end;

  WriteStr(cdsRowDataFooter);
end;


procedure TDBIXmlDataPacketWriter.WriteField(Field: TField);
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


procedure TDBIXmlDataPacketWriter.WriteFooter;
begin
  WriteStr(cdsDataPacketFooter);
end;


procedure TDBIXmlDataPacketWriter.WriteHeader;
begin
  WriteStr(cdsVersion);
  WriteStr(cdsDataPacketHeader);
end;


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style Xml stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
procedure TDBIXmlDataPacketWriter.WriteMetaData;
var
  Index: Integer;
  FieldDefs: TFieldDefs;

begin
  // Write cds Metadata to stream
  WriteStr(cdsMetaDataHeader);
  WriteStr(cdsFieldsHeader);

  // Get info without opening the database
  FieldDefs := Dataset.FieldDefs;
  if not Dataset.Active then begin
    FieldDefs.Update;
  end;

  for Index := 0 to FieldDefs.Count-1 do begin
//##WIP if (DB.faReadOnly in FieldDefs[Index].Attributes) then Continue;

    WriteStr('<FIELD ');
    if EncodeFieldName(FieldDefs[Index].Name) <> FieldDefs[Index].Name then begin
      WriteFmt('fieldname="%s"', [FieldDefs[Index].Name]);
    end;

    WriteFmt(
      'attrname="%s" fieldtype="',
      [EncodeFieldName(FieldDefs[Index].Name)]
      );

    case FieldDefs[Index].DataType of
      db.ftString,
      db.ftFixedChar:   WriteStr(cdsFieldAnsiString);
      db.ftWideString:  WriteStr(cdsFieldWideString);
      db.ftBoolean:     WriteStr(cdsFieldBoolean);
{$ifdef DELPHI2009}
      db.ftShortInt:    WriteStr(cdsFieldShortInt);
{$endif}
      db.ftSmallInt:    WriteStr(cdsFieldSmallInt);
      db.ftInteger:     WriteStr(cdsFieldInteger);
      db.ftLargeInt:    WriteStr(cdsFieldLargeInt);
      db.ftAutoInc:     WriteStr(cdsFieldAutoInc);
      db.ftWord:        WriteStr(cdsFieldWord);
{$ifdef DELPHI2009}
      db.ftByte:        WriteStr(cdsFieldByte);
      db.ftLongWord:    WriteStr(cdsFieldLongWord);

      // There is a bug in TClientDataset for TExtendedField,
      // so this field type has been mapped to double instead
      db.ftExtended:    WriteStr(cdsFieldFloat);
      //##JVR ftExtended:    WriteStr(cdsFieldExtended);

      // Guess what now they have broken TSingleField as well,
      // so this field WriteStr has been mapped to double instead
      db.ftSingle:      WriteStr(cdsFieldFloat);
      //##JVR ftSingle:      WriteStr(cdsFieldSingle);

{$endif}
      db.ftFloat:       WriteStr(cdsFieldFloat);
      db.ftCurrency:    WriteStr(cdsFieldCurrency);
      db.ftBCD:         WriteStr(cdsFieldBCD);
      db.ftDate:        WriteStr(cdsFieldDate);
      db.ftTime:        WriteStr(cdsFieldTime);
      db.ftDateTime:    WriteStr(cdsFieldDateTime);
      db.ftBytes:       WriteStr(cdsFieldBytes);
      db.ftVarBytes,
      db.ftBlob:        WriteStr(cdsFieldVarBytes);
      db.ftMemo:        WriteStr(cdsFieldMemo);
      db.ftGraphic,
      db.ftTypedBinary: WriteStr(cdsFieldGraphic);
      db.ftFmtMemo:     WriteStr(cdsFieldFmtMemo);
      db.ftParadoxOle,
      db.ftDBaseOle:    WriteStr(cdsFieldOle);
    end;  { case }

    if (DB.faReadOnly in FieldDefs[Index].Attributes) then begin
      WriteStr(cdsFieldReadOnly);
    end;

    if FieldDefs[Index].Required then begin
      WriteStr(cdsFieldRequired);
    end;

{$ifndef DELPHI2006}
    case FieldDefs[Index].DataType of
      ftWideString: WriteStr(cdsFieldSubTypeWideText);
    end;
{$endif}

    if (FieldDefs[Index].Size > 0) then begin
      case FieldDefs[Index].DataType of
        ftWideString:  WriteFmt('" WIDTH="%d', [2 * FieldDefs[Index].Size]);
      else
        WriteFmt('" WIDTH="%d', [FieldDefs[Index].Size]);
      end;
    end;

    WriteStr('"/>');
  end;  { for }

  WriteStr(cdsFieldsFooter);
  WriteStr(cdsParams);
  WriteStr(cdsMetaDataFooter);
end;




{ TDBICustomDataPacketWriter }

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


procedure TDBICustomDataPacketWriter.SetDataset(Value: TDataset);
begin
  FDataset := Value;
end;


end.
