// _____________________________________________________________________________
{**
  <H5>Copyright</H5> 
  Better Innovative Technical Services Pty Ltd<BR>
  Copyright (C) 2001, All rights reserved.<!--

  Project:       DBILib
  Files(s):      DBIXmlUtils.pas
  Classes:       ...
  Author:        John Vander Reest
  Purpose:       Utils to save data to a ClientDataset XML file

  Notes:         ...
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 27/02/2001 17:47:37 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
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
  SysUtils, TypInfo, Math, DBIConst, DBIIntfConsts, DBIFileStreams;

const
  UnitName = 'DBIXmlUtils';

  // Date Conversion format
  CdsDateFormat = 'YYYYMMDD';

  // Tags
  cdsVersion =               '<?xml version="1.0" standalone="yes"?>  ';
  cdsDataPacketHeader =      '<DATAPACKET Version="2.0">';
  cdsMetaDataHeader =        '<METADATA>';
  cdsFieldsHeader =          '<FIELDS>';
  cdsFieldsFooter =          '</FIELDS>';
  cdsMetaDataFooter =        '</METADATA>';
  cdsRowDataHeader =         '<ROWDATA>';
  cdsRowDataFooter =         '</ROWDATA>';
  cdsDataPacketFooter =      '</DATAPACKET>';
  cdsEndTag =                '/>';

  // FieldTypes
  cdsFieldAnsiString =       'string';
  cdsFieldWideString =       'string.uni';
  cdsFieldBoolean =          'boolean';
  cdsFieldSmallInt =         'i2';
  cdsFieldInteger =          'i4';
  cdsFieldAutoInc =          'i4" readonly="true" SUBTYPE="Autoinc';
  cdsFieldFloat =            'r8';
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
    UnitName + '::' + Caller + '::' + Reference + #13 + ErrMsg, Args);
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
  Caller = 'SaveAsXmlStream';

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
//  DisplayData: String;
  DisplayText: String;
  HasData: Boolean;

  function XmlSafe(const Source: String): String;
  var
    SourceIndex: Integer;
    ResultIndex: Integer;
    XlateIndex: Integer;
//##JVR    Tolerance: Integer;

  begin
    ResultIndex := 1;
//##JVR    Tolerance := Min(10, (Length(Source) div 10)); // Add ten percent
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
    
//##JVR    Result[ResultIndex] := #0;
    SetLength(Result, ResultIndex-1); //##JVRStrLen(PChar(Result)));
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
{$ifdef Delphi2009}
  SaveDateFormat := FormatSettings.ShortDateFormat;
{$else}
  SaveDateFormat := ShortDateFormat;
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
            ftSmallint:    Write(cdsFieldSmallint);
            ftInteger:     Write(cdsFieldInteger);
            ftAutoInc:     Write(cdsFieldAutoInc);
            ftWord,
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

          if FieldDefs[Index].Required then begin
            Write('" required="true');
          end;

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
        Write(cdsMetaDataFooter);


        // Write cds Data to stream
        Write(cdsRowDataHeader);

        while not Eof do begin
          Write('<ROW ');

          for Index := 0 to Fields.Count-1 do begin
            try
              // Set the Date format for conversion in cds format
{$ifdef Delphi2009}
              FormatSettings.ShortDateFormat := cdsDateFormat;
{$else}
              ShortDateFormat := cdsDateFormat;
{$endif}
              FieldData := Fields[Index].AsString;
              DisplayText := Fields[Index].DisplayText;

            finally
              // Reset the Dateformat, to avoid upsetting other code
              // Jee the VCL has some really bad stuff in it

              { TODO 3 -oJvr -cDBIXMLUtils.SaveAsXmlStream():
                Imagine what sort of problems this causes in threaded code
              }
{$ifdef Delphi2009}
              FormatSettings.ShortDateFormat := SaveDateFormat;
{$else}
              ShortDateFormat := SaveDateFormat;
{$endif}
            end;  { try..finally }

            // Displaytext in Currency fields are preceded by the currency symbol
            if (Fields[Index].DataType = ftCurrency) then begin
              FieldText := CurrToStr(Fields[Index].AsCurrency);
  //            DisplayData := Copy(DisplayText, 2, Length(DisplayText));
            end
            else begin
              try
                FieldText := FieldData; //##JVR XmlSafe(FieldData);
  //            DisplayData := DisplayText;
              except
                on E: Exception do
                  Error(E, Caller, '370', 'XmlSafe failed on "%s"', [FieldData]);
              end;
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
{$ifdef Delphi2009}
    FormatSettings.ShortDateFormat := SaveDateFormat;
{$else}
    ShortDateFormat := SaveDateFormat;
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
      AFileName,
      fmCreate,
      DBIPageBufferSize,
      [{No Options}]
      );
    try
      SaveAsXmlStream(LocalStream, ADataSet);
    finally
      LocalStream.Free;
    end;
  except
    Error(nil, Caller, '390', 'Failed to save dataset as Xml', []);
  end;
end;  { SaveAsXmlFile }


end.
