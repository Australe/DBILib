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
  Classes, DB, DBIStrings;

procedure SaveAsXmlStream(Stream: TStream; DataSet: TDataSet);
procedure SaveAsXmlFile(const AFileName: String; ADataSet: TDataSet);



implementation

uses
  SysUtils, DBIConst, DBIIntfConsts, DBIFileStreams;

const
  // Date Conversion format
  CdsDateFormat = 'YYYYMMDD';

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
{ cdsEndTag =                '/>';}

  // FieldTypes
  cdsFieldAnsiString =       'string';
  cdsFieldWideString =       'string.uni';
  cdsFieldBoolean =          'boolean';
{$ifdef DELPHI2009}
  cdsFieldShortInt =         'i1';
{$endif}
  cdsFieldSmallInt =         'i2';
  cdsFieldInteger =          'i4';
  cdsFieldLargeInt =         'i8';
{$ifdef DELPHI2009}
  cdsFieldByte =             'ui1';
{$endif}
  cdsFieldWord =             'ui2';
{$ifdef DELPHI2009}
  cdsFieldLongWord =         'ui4';
{$endif}
  cdsFieldAutoInc =          'i4" readonly="true" SUBTYPE="Autoinc';
{$ifdef DELPHI2009}
  cdsFieldSingle =           'r4';
{$endif}
  cdsFieldFloat =            'r8';
{$ifdef DELPHI2009}
  cdsFieldExtended =         'r10';
{$endif}
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
{$ifndef DELPHI2006}
  cdsFieldSubTypeWideText =  '" SUBTYPE="WideText';
{$endif}

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
  end;  { for }

  Result := FieldName;
end;  { Print }


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
  end;  { for }

  Result := FieldData
end;  { EncodeFieldData }


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style Xml stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
procedure SaveAsXmlStream(Stream: TStream; DataSet: TDataSet);
const
  xlateAmp = '&amp;';
  xlateQuote = '&quot;';
  xlateApost = '&apos;';
  xlateCR = '&#013;';
  xlateLF = '&#010;';
  xlateTAB = '&#09;';

var
  SaveDateFormat: String;
  Index: Integer;
  FieldData: String;
  FieldText: String;
  DisplayText: String;
  HasData: Boolean;

  function XmlSafe(const Source: String): String;
  var
    SourceIndex: Integer;
    ResultIndex: Integer;
    XlateIndex: Integer;

  begin
    ResultIndex := 1;
    SetLength(Result, 2 * Length(Source));

    for SourceIndex :=1 to Length(Source) do begin
      case Source[SourceIndex] of
        '&' : begin
          for XlateIndex := 1 to Length(xlateAmp) do begin
            Result[ResultIndex] := XlateAmp[XlateIndex];
            Inc(ResultIndex);
          end;
        end;

        '"' : begin
          for XlateIndex := 1 to Length(xlateQuote) do begin
            Result[ResultIndex] := xlateQuote[XlateIndex];
            Inc(ResultIndex);
          end;
        end;

        '''' : begin
          for XlateIndex := 1 to Length(xlateApost) do begin
            Result[ResultIndex] := xlateApost[XlateIndex];
            Inc(ResultIndex);
          end;
        end;

        #13 : begin
          for XlateIndex := 1 to Length(xlateCR) do begin
            Result[ResultIndex] := xlateCR[XlateIndex];
            Inc(ResultIndex);
          end;
        end;

        #10 : begin
          for XlateIndex := 1 to Length(xlateLF) do begin
            Result[ResultIndex] := xlateLF[XlateIndex];
            Inc(ResultIndex);
          end;
        end;

        #9 : begin
          for XlateIndex := 1 to Length(xlateTAB) do begin
            Result[ResultIndex] := xlateTAB[XlateIndex];
            Inc(ResultIndex);
          end;
        end;

        else begin
          Result[ResultIndex] := Source[SourceIndex];
          Inc(ResultIndex)
        end;
      end;  { case}
    end;  { for }
    
    SetLength(Result, ResultIndex-1);
  end;


  procedure Write(const Data: String);
  begin
    Stream.Write(TDBIStringBuffer(TDBIString(Data))^, Length(Data));
  end;

  procedure WriteFmt(const Data: String; Args: array of const);
  begin
    Write(Format(Data, Args));
  end;

begin
{$ifndef Delphi2007}
  SaveDateFormat := ShortDateFormat;
{$else}
  SaveDateFormat := FormatSettings.ShortDateFormat;
{$endif}
  try
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
            ftSingle:      Write(cdsFieldSingle);

           // There is a bug in clientdataset for TExtendedField,
           // so this field type has been mapped to double instead
            ftExtended:    Write(cdsFieldFloat);
           //##JVR ftExtended:    Write(cdsFieldExtended);
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
if (DB.faReadOnly in FieldDefs[Index].Attributes) then Continue;

            try
              // Set the Date format for conversion in cds format
{$ifndef Delphi2007}
              ShortDateFormat := cdsDateFormat;
{$else}
              FormatSettings.ShortDateFormat := cdsDateFormat;
{$endif}
              FieldData := Fields[Index].AsString;
              DisplayText := Fields[Index].DisplayText;

            finally
              // Reset the Dateformat, to avoid upsetting other code
              // Jee the VCL has some really bad stuff in it

              { TODO 3 -oJvr -cDBIXMLUtils.SaveAsXmlStream():
                Imagine what sort of problems this causes in threaded code
              }
{$ifndef Delphi2007}
              ShortDateFormat := SaveDateFormat;
{$else}
              FormatSettings.ShortDateFormat := SaveDateFormat;
{$endif}
            end;  { try..finally }

            // Displaytext in Currency fields are preceded by the currency symbol
            if (Fields[Index].DataType = ftCurrency) then begin
              FieldText := CurrToStr(Fields[Index].AsCurrency);
            end
            else begin
              FieldText := FieldData;
            end;

            HasData := (FieldData <> '') and
                       ((FieldText = FieldData) or (DisplayText = '(MEMO)'));
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

  finally
{$ifndef Delphi2007}
    ShortDateFormat := SaveDateFormat;
{$else}
    FormatSettings.ShortDateFormat := SaveDateFormat;
{$endif}
  end;  { try..finally }
end;  { SaveAsXmlStream }


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
procedure SaveAsXmlFile(const AFileName: String; ADataSet: TDataSet);
const
  Caller = 'SaveAsXmlFile';

var
  LocalStream: TStream;

begin
  try
    LocalStream := TDBIFileStream.Create(
      AFileName, fmCreate, DBIPageBufferSize, [{No Options}]);
    try
      SaveAsXmlStream(LocalStream, ADataSet);
    finally
      LocalStream.Free;
    end;
  except
    Error(nil, Caller, '470', 'Failed to save dataset as Xml', []);
  end;
end;  { SaveAsXmlFile }


end.
