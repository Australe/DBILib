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
  ______________________________________________________________________________
}

{#omcodecop off : jvr : native api code}

unit DBIXmlUtils;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DB, DBIStrings, DBIDataset;

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

procedure LoadFromXmlFile(const AFileName: TFileName; ADataset: TDBIDataset);
procedure LoadFromXmlStream(Stream: TStream; ADataset: TDBIDataset);

procedure SaveToXmlFile(const AFileName: TFilename; ADataSet: TDataSet);
procedure SaveToXmlStream(Stream: TStream; DataSet: TDataSet);


implementation

uses
  DBIConst, DBIIntfConsts, DBIFileStreams, DBIXmlDataConnections;

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
  Jvr - 09/02/2001 15:46:30.<P>
}
procedure Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
begin
{$IFDEF DebugExceptions}
  raise EDBIException.CreateFmt(
    'DBIXmlUtils::' + Caller + '::' + Reference + #13 + ErrMsg, Args);
{$ELSE}
  raise EDBIException.CreateFmt(ErrMsg, Args);
{$ENDIF DebugExceptions}
end;  { Error }




// _____________________________________________________________________________
{**
  Jvr - 28/02/2001 13:16:54 - Converts a fieldname with illegal fieldname chars
                              to a valid fieldname.<P>
}
function EncodeFieldName(FieldName: String): String;
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
function EncodeFieldData(FieldData: String): String;
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


// _____________________________________________________________________________
{**
  Jvr - 02/06/2013 10:58:11 - Initial code.<br>
}
procedure LoadFromXmlFile(const AFileName: TFileName; ADataSet: TDBIDataset);
var
  Connection: TDBIXmlDataPacketReader;

begin
  Connection := TDBIXmlDataPacketReader.Create;
  try
    ADataSet.Close;

    Connection.Input.LoadFromFile(AFileName);
    Connection.Dataset := ADataSet;
    Connection.GetMetaData;

    ADataSet.CreateDataset;
    Connection.GetData;

    ADataSet.First;
  finally
    Connection.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 14/06/2013 13:09:43 - Initial code.<br>
}
procedure LoadFromXmlStream(Stream: TStream; ADataSet: TDBIDataset);
var
  Connection: TDBIXmlDataPacketReader;

begin
  Connection := TDBIXmlDataPacketReader.Create;
  try
    ADataSet.Close;

    Connection.Input.LoadFromStream(Stream);
    Connection.Dataset := ADataSet;
    Connection.GetMetaData;

    ADataSet.CreateDataset;
    Connection.GetData;

    ADataSet.First;
  finally
    Connection.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
procedure SaveToXmlFile(const AFileName: TFileName; ADataSet: TDataSet);
const
  Caller = 'SaveToXmlFile';

var
  LocalStream: TStream;

begin
  try
    LocalStream := TDBIFileStream.Create(
      AFileName, fmCreate, DBIPageBufferSize, [{No Options}]);
    try
      SaveToXmlStream(LocalStream, ADataSet);
    finally
      LocalStream.Free;
    end;
  except
    Error(nil, Caller, '245', 'Failed to save dataset as Xml', []);
  end;
end;  { SaveAsXmlFile }


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style Xml stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
procedure SaveToXmlStream(Stream: TStream; DataSet: TDataSet);
var
  Index: Integer;
  FieldData: String;
  FieldText: String;
  DisplayText: String;
  HasData: Boolean;

  procedure Write(const Data: String);
  begin
    Stream.Write(TDBIStringBuffer(TDBIString(Data))^, Length(Data));
  end;

  procedure WriteFmt(const Data: String; Args: array of const);
  begin
    Write(Format(Data, Args));
  end;

begin
  Write(cdsVersion);
  Write(cdsDataPacketHeader);

  with DataSet do begin
    if not Active then begin
      Open;
    end
    else begin
      First;
    end;

    Dataset.DisableControls;
    try
      // Write cds Metadata to stream
      Write(cdsMetaDataHeader);
      Write(cdsFieldsHeader);

      // Get info without opening the database
      if not Active then begin
        FieldDefs.Update;
      end;

      for Index := 0 to FieldDefs.Count-1 do begin
//##WIP if (DB.faReadOnly in FieldDefs[Index].Attributes) then Continue;

        Write('<FIELD ');
        if EncodeFieldName(FieldDefs[Index].Name) <> FieldDefs[Index].Name then begin
          WriteFmt('fieldname="%s"', [FieldDefs[Index].Name]);
        end;

        WriteFmt(
          'attrname="%s" fieldtype="',
          [EncodeFieldName(FieldDefs[Index].Name)]
          );

        case FieldDefs[Index].DataType of
          ftString,
          ftFixedChar:   Write(cdsFieldAnsiString);
          ftWideString:  Write(cdsFieldWideString);
          ftBoolean:     Write(cdsFieldBoolean);
{$ifdef DELPHI2009}
          ftShortInt:    Write(cdsFieldShortInt);
{$endif}
          ftSmallInt:    Write(cdsFieldSmallInt);
          ftInteger:     Write(cdsFieldInteger);
          ftLargeInt:    Write(cdsFieldLargeInt);
          ftAutoInc:     Write(cdsFieldAutoInc);
          ftWord:        Write(cdsFieldWord);
{$ifdef DELPHI2009}
          ftByte:        Write(cdsFieldByte);
          ftLongWord:    Write(cdsFieldLongWord);

          // There is a bug in TClientDataset for TExtendedField,
          // so this field type has been mapped to double instead
          ftExtended:    Write(cdsFieldFloat);
          //##JVR ftExtended:    Write(cdsFieldExtended);

          // Guess what now they have broken TSingleField as well,
          // so this field type has been mapped to double instead
          ftSingle:      Write(cdsFieldFloat);
          //##JVR ftSingle:      Write(cdsFieldSingle);

{$endif}
          ftFloat:       Write(cdsFieldFloat);
          ftCurrency:    Write(cdsFieldCurrency);
          ftBCD:         Write(cdsFieldBCD);
          ftDate:        Write(cdsFieldDate);
          ftTime:        Write(cdsFieldTime);
          ftDateTime:    Write(cdsFieldDateTime);
          ftBytes:       Write(cdsFieldBytes);
          ftVarBytes,
          ftBlob:        Write(cdsFieldVarBytes);
          ftMemo:        Write(cdsFieldMemo);
          ftGraphic,
          ftTypedBinary: Write(cdsFieldGraphic);
          ftFmtMemo:     Write(cdsFieldFmtMemo);
          ftParadoxOle,
          ftDBaseOle:    Write(cdsFieldOle);
        end;  { case }

        if (DB.faReadOnly in FieldDefs[Index].Attributes) then begin
          Write(cdsFieldReadOnly);
        end;

        if FieldDefs[Index].Required then begin
          Write(cdsFieldRequired);
        end;

{$ifndef DELPHI2006}
        case FieldDefs[Index].DataType of
          ftWideString: Write(cdsFieldSubTypeWideText);
        end;
{$endif}

        if FieldDefs[Index].Size > 0 then begin
          case FieldDefs[Index].DataType of
            ftWideString:  WriteFmt('" WIDTH="%d', [2 * FieldDefs[Index].Size]);
          else
            WriteFmt('" WIDTH="%d', [FieldDefs[Index].Size]);
          end;
        end;

        Write('"/>');
      end;  { for }

      Write(cdsFieldsFooter);
      Write(cdsParams);
      Write(cdsMetaDataFooter);


      // Write cds Data to stream
      Write(cdsRowDataHeader);

      while not Eof do begin
        Write('<ROW ');

        for Index := 0 to Fields.Count-1 do begin
          // if the field is readonly then skip
          if (db.faReadOnly in FieldDefs[Index].Attributes) then Continue;

          case Fields[Index].DataType of
            ftDate: begin
              FieldData := FormatDateTime(cdsDateFormat, Fields[Index].AsDateTime);
            end;

            ftDateTime: begin
              FieldData := FormatDateTime(cdsDateTimeFormat, Fields[Index].AsDateTime);
            end;

          // All other fieldtypes
          else
            FieldData := Fields[Index].AsString;
          end;


          // Displaytext in Currency fields are preceded by the currency symbol
          DisplayText := Fields[Index].DisplayText;

          if (Fields[Index].DataType = ftCurrency) then begin
            FieldText := CurrToStr(Fields[Index].AsCurrency);
          end
          else begin
            FieldText := FieldData;
          end;

          HasData := (FieldData <> '') and ((FieldText = FieldData) or (DisplayText = '(MEMO)'));
          if HasData then begin
            Write(EncodeFieldName(Fields[Index].FieldName) + '="');
            Write(EncodeFieldData(FieldData) + '" ');
          end;
        end;  { for }

        Write('/>');
        Next;
      end;  { while }

      Write(cdsRowDataFooter);
      Write(cdsDataPacketFooter);
    finally
      Dataset.EnableControls;
    end;  { try..finally }
  end;  { with }
end;  { SaveToXmlStream }


end.
