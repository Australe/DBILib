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
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIXmlUtils;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, DB, DBIConst, DBIIntfConsts, DBIDataset;

type
  TDBICsvData = class(TPersistent)
  public
    class procedure LoadFromFile(const AFileName: TFileName; ADataset: TDBIDataset);
    class procedure LoadFromStream(Stream: TStream; ADataset: TDBIDataset);

    class procedure SaveToFile(const AFileName: TFilename; ADataSet: TDataSet);
    class procedure SaveToStream(Stream: TStream; DataSet: TDataSet);
  end;


type
  TDBIPsvData = class(TPersistent)
  public
    class procedure SaveToStream(Stream: TStream; DataSet: TDataSet);
    class procedure SaveToFile(const AFileName: String; ADataSet: TDataSet);
  end;


type
  TDBIXmlData = class(TPersistent)
  public
    class procedure LoadFromFile(const AFileName: TFileName; ADataset: TDBIDataset);
    class procedure LoadFromStream(Stream: TStream; ADataset: TDBIDataset);

    class procedure SaveToFile(const AFileName: TFilename; ADataSet: TDataSet);
    class procedure SaveToStream(Stream: TStream; DataSet: TDataSet);

  public
    // Data Packet Helpers
    class function CreateDataPacket(const Value: String = ''): TDBIDataPacket; overload;
    class function CreateDataPacket(ADataset: TDataset): TDBIDataPacket; overload;
    class function CreateDataPacket(ADataPacket: TDBIDataPacket; const ASize: LongInt = 0): TDBIDataPacket; overload;
    class function DataPacketSize(ADataPacket: TDBIDataPacket): Integer;
    class function DatasetToXmlString(ADataset: TDataset): String;
    class procedure FreeDataPacket(var ADataPacket: TDBIDataPacket);
    class procedure LoadData(ADataPacket: TDBIDataPacket; ADataSet: TDBIDataset);
    class procedure LoadFromDataPacket(ADataPacket: TDBIDataPacket; ADataset: TDBIDataset);

  end;


implementation

uses
  DBIUtils, DBIFileStreams, DBIDataPacketReaders, DBIDataPacketWriters;


{ TDBIXmlData }

// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 12:36:54 - Initial code.<br>
}
class function TDBIXmlData.CreateDataPacket(const Value: String = ''): TDBIDataPacket;
begin
  Result := TStringStream.Create(Value);
end;


// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 12:54:36 - Initial code.<br>
}
class function TDBIXmlData.CreateDataPacket(ADataset: TDataset): TDBIDataPacket;
begin
  Result := CreateDataPacket;
  SaveToStream(Result, ADataset);
end;


// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 13:23:43 - Initial code.<br>
}
class function TDBIXmlData.CreateDataPacket(ADataPacket: TDBIDataPacket; const ASize: LongInt = 0): TDBIDataPacket;
begin
  Result := CreateDataPacket;
  Result.CopyFrom(ADataPacket, ASize);
end;


// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 12:57:03 - Initial code.<br>
}
class function TDBIXmlData.DataPacketSize(ADataPacket: TDBIDataPacket): Integer;
begin
  if Assigned(ADataPacket) then begin
    Result := ADataPacket.Size;
  end
  else begin
    Result := -1;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 12:44:21 - Initial code.<br>
}
class function TDBIXmlData.DatasetToXmlString(ADataset: TDataset): String;
var
  DataPacket: TDBIDataPacket;

begin
  DataPacket := CreateDataPacket(ADataset);
  try
    Result := TStringStream(DataPacket).DataString;
  finally
    FreeDataPacket(DataPacket);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 12:23:34 - Initial code.<br>
}
class procedure TDBIXmlData.FreeDataPacket(var ADataPacket: TDBIDataPacket);
begin
  if Assigned(ADataPacket) then
  begin
    ADataPacket.Free;
    ADataPacket := nil;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/11/2014 14:54:23 - Initial code.<br>
}
class procedure TDBIXmlData.LoadData(ADataPacket: TDBIDataPacket; ADataSet: TDBIDataset);
var
  Connection: TDBIXmlDataPacketReader;

begin
  Connection := Local(TDBIXmlDataPacketReader.Create).Obj as TDBIXmlDataPacketReader;
  Connection.Input.LoadFromStream(ADataPacket);
  Connection.Dataset := ADataSet;
  Connection.DropMetaData;
  Connection.GetData;

  ADataSet.First;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/11/2014 13:35:57 - Initial code.<br>
}
class procedure TDBIXmlData.LoadFromDataPacket(ADataPacket: TDBIDataPacket; ADataset: TDBIDataset);
begin
  if (DataPacketSize(ADataPacket) > 0) then begin
    ADataset.Close;
    ADataset.Fields.Clear;
    ADataset.FieldDefs.Clear;

    LoadFromStream(ADataPacket, ADataset);
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
class procedure TDBIXmlData.LoadFromFile(const AFileName: TFileName; ADataSet: TDBIDataset);
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
class procedure TDBIXmlData.LoadFromStream(Stream: TStream; ADataSet: TDBIDataset);
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


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
class procedure TDBIXmlData.SaveToFile(const AFileName: TFileName; ADataSet: TDataSet);
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
end;  { SaveToXmlFile }


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style Xml stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
class procedure TDBIXmlData.SaveToStream(Stream: TStream; DataSet: TDataSet);
var
  Writer: TDBIXmlDataPacketWriter;

begin
  Writer := Local(TDBIXmlDataPacketWriter.Create).Obj as TDBIXmlDataPacketWriter;
  Writer.Dataset := Dataset;
  Writer.SaveToStream(Stream);
end;  { SaveToXmlStream }





{ TDBIPsv }

// _____________________________________________________________________________
{**
  Convert a dataset to a PSV style CSV stream.

  Jvr - 20/05/2002 16:45:09.<P>
}
class procedure TDBIPsvData.SaveToStream(Stream: TStream; DataSet: TDataSet);
var
  Index: Integer;

  procedure Write(Str: String);
  begin
    if Str <> '' then begin
      Stream.Write(Str[1], Length(Str));
    end;
  end;

begin
  if not Dataset.Active then begin
    Dataset.Open;
  end
  else begin
    Dataset.First;
  end;

  Dataset.DisableControls;
  try
    DataSet.First;
    Write('#');
    for Index := 0 to DataSet.Fields.Count - 1 do begin
      if (DataSet.Fields[Index].FieldName <> FieldName_NullFlags) then begin
        if Index <> 0 then begin
          Write('|');
        end;
        Write(DataSet.Fields[Index].FieldName);
      end;
    end;
    Write(sLineBreak);

    while not DataSet.EOF do begin
      for Index := 0 to DataSet.Fields.Count - 1 do begin
        if (DataSet.Fields[Index].FieldName <> FieldName_NullFlags) then begin
          if Index <> 0 then begin
            Write('|');
          end;
          Write(DataSet.Fields[Index].AsString);
        end;
      end;
      Write(sLineBreak);
      DataSet.Next;
    end;
  finally
    Dataset.EnableControls;
  end;
end;  { SaveAsPsvStream }


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
class procedure TDBIPsvData.SaveToFile(const AFileName: String; ADataSet: TDataSet);
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
      raise EDBIException.Create(Self, E, 'SavetoFile::260', 'Failed to save Dataset as Psv', []);
  end;
end;  { SaveAsPsvFile }





// _____________________________________________________________________________
{**
  Jvr - 02/06/2013 10:58:11 - Initial code.<br>
}
class procedure TDBICsvData.LoadFromFile(const AFileName: TFileName; ADataSet: TDBIDataset);
var
  Connection: TDBICSVDataPacketReader;

begin
  if not ADataset.Active then begin
    raise EDBIException.Create(Self, 'LoadFromFile::275',
      'Create and open the dataset before atempting to import a CSV file', []
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
class procedure TDBICsvData.LoadFromStream(Stream: TStream; ADataSet: TDBIDataset);
var
  Connection: TDBICSVDataPacketReader;

begin
  if not ADataset.Active then begin
    raise EDBIException.Create(Self, 'LoadFromStream::305',
      'Create and open the Dataset before atempting to import a CSV stream', []
      );
  end;

  Connection := Local(TDBICSVDataPacketReader.Create).Obj as TDBICSVDataPacketReader;
  Connection.Input.LoadFromStream(Stream);
  Connection.Dataset := ADataSet;
  Connection.GetMetaData;
  Connection.GetData;

  ADataSet.First;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 18:12:53.<P>
}
class procedure TDBICsvData.SaveToFile(const AFileName: TFileName; ADataSet: TDataSet);
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
      raise EDBIException.Create(Self, E, 'SaveToFile::345', 'Failed to save Dataset as CSV', []);
  end;
end;  { SaveToCSVFile }


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style CSV stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
class procedure TDBICsvData.SaveToStream(Stream: TStream; DataSet: TDataSet);
var
  Writer: TDBICSVDataPacketWriter;

begin
  Writer := Local(TDBICSVDataPacketWriter.Create).Obj as TDBICSVDataPacketWriter;
  Writer.Dataset := Dataset;
  Writer.SaveToStream(Stream);
end;  { SaveToCSVStream }


end.
