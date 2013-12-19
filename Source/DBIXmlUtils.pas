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
  Classes, SysUtils, DB, DBIDataset;

procedure LoadFromXmlFile(const AFileName: TFileName; ADataset: TDBIDataset);
procedure LoadFromXmlStream(Stream: TStream; ADataset: TDBIDataset);

procedure SaveToXmlFile(const AFileName: TFilename; ADataSet: TDataSet);
procedure SaveToXmlStream(Stream: TStream; DataSet: TDataSet);

procedure LoadFromCSVFile(const AFileName: TFileName; ADataset: TDBIDataset);
procedure LoadFromCSVStream(Stream: TStream; ADataset: TDBIDataset);

procedure SaveToCSVFile(const AFileName: TFilename; ADataSet: TDataSet);
procedure SaveToCSVStream(Stream: TStream; DataSet: TDataSet);


implementation

uses
  DBIConst, DBIFileStreams, DBIDataPacketReaders, DBIDataPacketWriters;


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
end;  { SaveToXmlFile }


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style Xml stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
procedure SaveToXmlStream(Stream: TStream; DataSet: TDataSet);
var
  Writer: TDBIXmlDataPacketWriter;

begin
  Writer := TDBIXmlDataPacketWriter.Create;
  try
    Writer.Dataset := Dataset;
    Writer.SaveToStream(Stream);
  finally
    Writer.Free;
  end;
end;  { SaveToXmlStream }





// _____________________________________________________________________________
{**
  Jvr - 02/06/2013 10:58:11 - Initial code.<br>
}
procedure LoadFromCSVFile(const AFileName: TFileName; ADataSet: TDBIDataset);
var
  Connection: TDBICSVDataPacketReader;

begin
  Assert(ADataset.Active, 'Create and open the dataset before atempting to import a CSV file');

  Connection := TDBICSVDataPacketReader.Create;
  try
//##JVR    ADataSet.Close;

    Connection.Input.LoadFromFile(AFileName);
    Connection.Dataset := ADataSet;
    Connection.GetMetaData;

//##JVR    ADataSet.CreateDataset;
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
procedure LoadFromCSVStream(Stream: TStream; ADataSet: TDBIDataset);
var
  Connection: TDBICSVDataPacketReader;

begin
  Assert(ADataset.Active, 'Create and open the dataset before atempting to import a CSV stream');

  Connection := TDBICSVDataPacketReader.Create;
  try
//##JVR    ADataSet.Close;

    Connection.Input.LoadFromStream(Stream);
    Connection.Dataset := ADataSet;
    Connection.GetMetaData;

//##JVR    ADataSet.CreateDataset;
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
procedure SaveToCSVFile(const AFileName: TFileName; ADataSet: TDataSet);
const
  Caller = 'SaveToCSVFile';

var
  LocalStream: TStream;

begin
  try
    LocalStream := TDBIFileStream.Create(
      AFileName, fmCreate, DBIPageBufferSize, [{No Options}]);
    try
      SaveToCSVStream(LocalStream, ADataSet);
    finally
      LocalStream.Free;
    end;
  except
    Error(nil, Caller, '245', 'Failed to save dataset as CSV', []);
  end;
end;  { SaveToCSVFile }


// _____________________________________________________________________________
{**
  Convert a dataset to a CDS style CSV stream.

  Jvr - 28/02/2001 12:33:01 - Specified a fixed date format for streaming.<BR>
  Jvr - 15/05/2001 13:04:12 - Fixed the currency symbol problem with
                              Currency fields.<P>
}
procedure SaveToCSVStream(Stream: TStream; DataSet: TDataSet);
var
  Writer: TDBICSVDataPacketWriter;

begin
  Writer := TDBICSVDataPacketWriter.Create;
  try
    Writer.Dataset := Dataset;
    Writer.SaveToStream(Stream);
  finally
    Writer.Free;
  end;
end;  { SaveToCSVStream }


end.
