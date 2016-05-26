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
  TDBIXmlData = class(TPersistent)
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
  TDBIXMLDataPacketWriter.SaveToStream(Result, ADataset);
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

    TDBIXMLDataPacketReader.LoadFromStream(ADataPacket, ADataset);
  end;
end;


end.
