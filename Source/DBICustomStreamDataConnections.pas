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
  1.0 | 11/05/2005 11:05:30 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBICustomStreamDataConnections;

{$I DBICompilers.inc}

interface

uses
  Classes, SysUtils, DBIConst, DBIUtils, DBIInterfaces,
{$ifndef fpc}
  DBCommon,
{$endif}
  DBIIntfConsts, DBIFileStreams, DBIXbaseBlobConnections;


  // ___________________________________________________________________________
  {**
    <CODE>

         +---------+
         | TObject |
         +---------+
              |
              |
      +----------------------+
      |  TDBIDataConnection  |
      | <DBIInterfaces.pas>  |
      +----------------------+
              |                               +---------------+
              |                               |  FDataStream  |
    +-----------------------------------+     |   (TStream)   |
    |  TDBICustomStreamDataConnection   |<>---| <Classes.pas> |
    | <DBICustomStreamConnections.pas>  |     +---------------+
    |                                   |     +------------------------------+
    |                                   |<>---|       FBlobConnection        |
    +-----------------------------------+     |  (TDBIFoxProBlobConnection)  |
                                              | <DBIbaseBlobConnections.pas> |
                                              +------------------------------+
    </CODE>
  }
const
// ReadData & Writedata position constants
  posBof = -1;
  posEof = -2;

type
  TDBICustomStreamDataConnection = class(TDBIDataConnection)
  private
    FDirty: Boolean;
    FDataStream: TStream;
    FBlobConnection: TDBIXbaseCustomBlobConnection;

  protected
    function AllocPhysicalBuffer: TDBIRecordBuffer; virtual; abstract;
    procedure FreePhysicalBuffer(var Buffer: TDBIRecordBuffer); virtual; abstract;

    procedure CheckFileName(const Context: String);
    function GetBlobFileName: TFileName; virtual; abstract;
    function GetRecordSize: Word; virtual; abstract;
    function HasBlobs: Boolean;

    procedure SetActive(const Value: Boolean); override;
    procedure SetDataStream(AStream: TStream);

  protected
    // Non standard methods - added for this implementation
    procedure CloseDataStream;
    procedure OpenDataStream(const ANew: Boolean);

    procedure WriteMetaData(
      const AMode: TDBIMetaDataMode = mdAll;
      const ForceWrite: Boolean = False
      ); virtual; abstract;

    property BlobConnection: TDBIXbaseCustomBlobConnection read FBlobConnection;
    property Dirty: Boolean read FDirty write FDirty;
    property RecordSize: Word read GetRecordSize;

  public
    // Conversion Routines from Record-Buffer to Field-Buffer
    function GetData(const PhysicalBuffer; var Buffer): DSAttr; virtual; abstract;
    procedure PutData(var PhysicalBuffer; const Buffer); virtual; abstract;
    function WriteData(
      const APosition: Integer;
      var APhysicalBuffer;
      ASize: Integer = -1
      ): Longint; virtual; abstract;

  public
    constructor Create(AOwner: TObject); override;
    procedure Release; override;
    procedure Reset; override;

    function Append(const Buffer): Integer; override;
    procedure Cancel; override;
    procedure New; override;
    procedure Update(const Position: Integer; const Buffer); override;

    function IsCursorOpen: Boolean; virtual;
    function IsModified: Boolean; virtual;

    procedure LoadFromStream(
      AStream: TStream;
      const Format: TDBIDataFormat = dfXbasePlus
      ); override;
    procedure SaveToFile(
      AFileName: String = '';
      const Format: TDBIDataFormat = dfXbasePlus
      ); override;
    procedure SaveToStream(
     Stream: TStream;
     const Format: TDBIDataFormat = dfXbasePlus
     ); override;


    // Blob methods
    function CreateBlobConnection: TDBIXbaseCustomBlobConnection; virtual; abstract;
    procedure CloseBlob;
    procedure OpenBlob(const CreateNew: Boolean);

    procedure GetBlob(
      const Position: LongWord;
      const FieldNo: LongWord;
      const OffSet: LongWord;
      PData: Pointer;
      out Size: LongWord
      ); override;

    function PutBlob(
      const Position: LongWord;
      const FieldNo: LongWord;
      const OffSet: LongWord;
      PData: Pointer;
      const Size: LongWord
      ): Integer; override;

    // Properties specific to this implementation
    property DataStream: TStream read FDataStream write SetDataStream;
    property FileName: TFileName read GetFileName write SetFileName;
    property BlobFileName: TFileName read GetBlobFileName;

  end;  { TDBIBaseDataConnection }


implementation


{ TDBICustomStreamDataConnection }

// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 17:17:30<br>
}
function TDBICustomStreamDataConnection.Append(const Buffer): Integer;
var
  PhysicalBuffer: TDBIRecordBuffer;

begin
  // If buffer is nil then we only want the status
  PhysicalBuffer := AllocPhysicalBuffer;
  try
    if (@Buffer <> nil) then begin
      PutData(PhysicalBuffer^, Buffer);
    end;

    Result := WriteData(posEof, PhysicalBuffer^);
  finally
    FreePhysicalBuffer(PhysicalBuffer);
  end;

  NotifyDataEventCallBacks(Result, dbiRecordInserted, @Buffer);
  WriteMetaData(mdHeaderOnly);
end;  { Append }


// _____________________________________________________________________________
{**
  Nothing to do on a cancel.

  Jvr - 20/09/2002 13:49:53.<br>
}
procedure TDBICustomStreamDataConnection.Cancel;
begin
  //NOP - nothing to do on a cancel
end;  { Cancel }


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 18:26:28 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.CheckFileName(const Context: String);
begin
  if (FileName = '') then begin
    raise EDBIException.Create(Self, Context, 'Invalid FileName', []);
  end;
end;  { CheckFileName }


// _____________________________________________________________________________
{**
  Jvr - 31/12/2004 14:18:50 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.CloseBlob;
begin
  // Close BlobFile if previously opened
  if Assigned(FBlobConnection) then begin
    FBlobConnection.CloseBlobStream;
  end;
end;  { CloseBlob }


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 12:37:47 - Initial code.<br>
  Jvr - 24/06/2005 10:55:48 - Don't free memory streams on closure
                              Otherwise all the data disappears.<br>
}
procedure TDBICustomStreamDataConnection.CloseDataStream;
begin
  if (StreamMode = smFileStream) then begin
    FDataStream.Free;
    FDataStream := nil;
  end;
end;  { CloseDataStream }


// _____________________________________________________________________________
{**
  Jvr - 11/05/2005 15:49:48 - Initial code.<br>
}
constructor TDBICustomStreamDataConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FDirty := False;
  FDataStream := nil;
  FBlobConnection := nil;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 13:52:16.<br>
}
procedure TDBICustomStreamDataConnection.GetBlob(
  const Position: LongWord;
  const FieldNo: LongWord;
  const OffSet: LongWord;
  PData: Pointer;
  out Size: LongWord
  );
begin
  FBlobConnection.GetBlob(Position, Offset, PData, Size);
end;  { GetBlob }


// _____________________________________________________________________________
{**
  Jvr - 15/02/2001 13:17:45 - Returns true if dataset
                              has any blob- or memo-fields.<P>
  Jvr - 11/05/2005 14:52:35 - Updated to use logical fieldtypes.<br>
}
function TDBICustomStreamDataConnection.HasBlobs: Boolean;
var
  Index: Integer;

begin
  Result := False;

  for Index := Low(FieldProps) to High(FieldProps) do begin
    if (FieldProps[Index].iFldType = fldBLOB) then begin
      Result := True;
      Break;
    end;
  end;  { for }
end;  { HasBlobs }


// _____________________________________________________________________________
{**
  Jvr - 11/05/2005 16:01:41 - Initial code.<br>
}
function TDBICustomStreamDataConnection.IsCursorOpen: Boolean;
begin
  Result := Assigned(FDataStream);
end;  { IsCursorOpen }


// _____________________________________________________________________________
{**
  Jvr - 11/05/2005 15:58:59 - Initial code.<br>
}
function TDBICustomStreamDataConnection.IsModified: Boolean;
begin
  Result := Modified;
end;  { IsModified }


// _____________________________________________________________________________
{**
  Jvr - 25/07/2002 18:06:49 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.LoadFromStream(
  AStream: TStream;
  const Format: TDBIDataFormat
  );
begin
  inherited LoadFromStream(AStream, Format);

  FDataStream := TMemoryStream.Create;
  FDataStream.CopyFrom(AStream, 0);
end;  { LoadFromStream }


// _____________________________________________________________________________
{**
  If no datastream present then create one.
  Normally there is none at this stage, except if one has been assigned to
  the DataStream property (e.g. a TMemoryStream created externally)<br>

  Jvr - 01/11/2000 11:48:41<br>
  Jvr - 26/07/2002 15:04:48 - Merged the OpenDatastream method into new.<br>
  Jvr - 31/12/2004 14:08:32 - Blobs are now created/opened in OpenBlob().<br>
}
procedure TDBICustomStreamDataConnection.New;
begin
  if Active then Exit;

  // Create the appropriate stream
  OpenDataStream(True);

  // Write the metadata
  WriteMetaData(mdNew, True);

  // Take care of blobs
  OpenBlob(True);

  Active := True;
end;  { New }


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 12:40:02 - Initial code.<br>
  Jvr - 29/10/2002 14:07:08 - Readonly file support added.<br>
}
procedure TDBICustomStreamDataConnection.OpenDataStream(const ANew: Boolean);
var
  FileMode: Word;

begin
  if not Assigned(FDataStream) then begin
    FileMode := OpenModes[ReadOnly] or ShareModes[Exclusive] or CreateModes[ANew];

    FDataStream := TDBIFileStream.CreateStream(FileName, FileMode, StreamMode);
    CanModify := (FileMode and fmOpenReadWrite) = fmOpenReadWrite;

    Modified := False;
    Assert(Assigned(FDataStream));
  end;
end;  { OpenDataStream }


// _____________________________________________________________________________
{**
  Jvr - 31/12/2004 13:54:34 - Initial code.<br>
  Jvr - 09/05/2005 15:47:42 - Added xbVisualFoxpro support.<br>
}
procedure TDBICustomStreamDataConnection.OpenBlob(const CreateNew: Boolean);
begin
  if HasBlobs then begin
    if not Assigned(FBlobConnection) then begin
      FBlobConnection := CreateBlobConnection;
      FBlobConnection.FileName := BlobFileName;
      FBlobConnection.StreamMode := StreamMode;
      FBlobConnection.Exclusive := Exclusive;
      FBlobConnection.ReadOnly := ReadOnly;
//##JVR      FBlobConnection.Version := Version;
    end;

    if CreateNew then begin
      FBlobConnection.NewBlobStream;
    end
    else begin
      FBlobConnection.OpenBlobStream;
    end;
  end;
end;  { OpenBlob }


// _____________________________________________________________________________
{**
  Jvr - 08/02/2001 15:35:32.<br>
}
function TDBICustomStreamDataConnection.PutBlob(
  const Position: LongWord;
  const FieldNo: LongWord;
  const OffSet: LongWord;
  PData: Pointer;
  const Size: LongWord
  ): Integer;
begin
  Result := FBlobConnection.PutBlob(Position, Offset, PData, Size);
end;  { PutBlob }


// _____________________________________________________________________________
{**
  Jvr - 30/06/2005 14:29:12 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.Release;
begin
  if (StreamMode <> smExternalStream) then begin
    FDataStream.Free;
  end;

  FDataStream := nil;

  // Close BlobFile if previously opened
  if Assigned(FBlobConnection) then begin
    FBlobConnection.Free;
    FBlobConnection := nil;
  end;
end;  { Release }


// _____________________________________________________________________________
{**
  Clear all buffered data and refresh.

  Jvr - 12/02/2001 18:18:52.<br>
  Jvr - 19/06/2002 11:38:02.<br>
}
procedure TDBICustomStreamDataConnection.Reset;
begin
  GetMetaData;
end;  { Reset }


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 11:42:00 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.SaveToFile(
  AFileName: String;
  const Format: TDBIDataFormat
  );
begin
  if (AFileName = '') then begin
    AFileName := FileName;
  end;

  if (AFileName = '') then begin
    raise EDBIException.Create(Self, 'SaveToFile::475', 'Invalid FileName', []);
  end;

  if not Active then begin
    raise EDBIException.Create(Self, 'SaveToFile::480', 'DataConnection not active', []);
  end;

  if (Format = dfXML) then begin
    raise EDBIException.Create(Self, 'SaveToFile::485', 'Saving to XML not Supported Yet!', []);
  end;

  // Save to File
  TDBIFileStream.SaveStreamToFile(FDataStream, AFileName);
  if Assigned(FBlobConnection) then begin
    FBlobConnection.SaveToFile(ChangeFileExt(AFileName, '.fpt'));
  end;

  Modified := False;
end;  { SaveToFile }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 12:49:58 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.SaveToStream(
  Stream: TStream;
  const Format: TDBIDataFormat
  );
begin
  if (Stream = nil) then begin
    raise EDBIException.Create(Self, 'SaveToStream::505', 'Invalid Stream', []);
  end;

  if not Active then begin
    raise EDBIException.Create(Self, 'SaveToStream::510', 'DataConnection not active', []);
  end;

  if (Format = dfXML) then begin
    raise EDBIException.Create(Self, 'SaveToStream::515', 'Saving to XML not Supported Yet!', []);
  end;

  if Assigned(FBlobConnection) then begin
    raise EDBIException.Create(Self, 'SaveToStream::520', 'Blobs not supported when saving to stream', []);
  end;

  // Save to Stream
  if (FDataStream is TMemoryStream) then begin
    (FDataStream as TMemoryStream).SaveToStream(Stream);
  end
  else begin
    raise EDBIException.Create(Self, 'SaveToStream::530',
      'Saving to stream for a "%s" not implemented yet!',
      [FDataStream.ClassName]
      );
  end;

  Modified := False;
end;  { SaveToStream }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 15:08:34 - Initial code.<br>
  Jvr - 31/12/2004 14:08:32 - Blobs are now created/opened in OpenBlob().<br>
}
procedure TDBICustomStreamDataConnection.SetActive(const Value: Boolean);
begin
  if (Value = Active) then Exit;

  // Open DataConnection
  if Value then begin
    OpenDataStream(False);

    GetMetaData;

    // Now that the Metadata has been read open blob connection if required
    OpenBlob(False);
  end

  // Close DataConnection
  else begin
    // Update header with current date and other details
    WriteMetaData(mdHeaderOnly);

    CloseDataStream;

    // Close BlobFile if previously opened
    CloseBlob;
  end;  { if }

  inherited SetActive(Value);
end;  { SetActive }


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 15:47:54 - Updated code.<br>
}
procedure TDBICustomStreamDataConnection.SetDataStream(AStream: TStream);
begin
  FileName := '';
  FDataStream := AStream;

  SetStreamMode(smExternalStream, AStream <> nil);
end;  { SetDataStream }


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 17:29:39<br>
}
procedure TDBICustomStreamDataConnection.Update(const Position: Integer; const Buffer);
var
  PhysicalBuffer: TDBIRecordBuffer;

begin
  if (Position < 0) or (Position >= Integer(GetRecordCount(dsRecAll))) then begin
    raise EDBIException.Create(Self, 'Update::600', 'Record "%d" out of legal range', [Position]);
  end;

  // If buffer is nil then we only want the status
  PhysicalBuffer := AllocPhysicalBuffer;
  try
    if (@Buffer <> nil) then begin
      PutData(PhysicalBuffer^, Buffer);
    end;
    WriteData(Position, PhysicalBuffer^);
  finally
    FreePhysicalBuffer(PhysicalBuffer);
  end;

  NotifyDataEventCallBacks(Position+1, dbiRecordModified, @Buffer);

  { TODO -ojvr -cTDBIBaseDataConnection.Update() :
    I'm not sure that we should update the header when doing an update.
    No records have been added/deleted so the RecordCount doesn't change,
    and the datestamp is updated when we close the file.
  }
  WriteMetaData(mdHeaderOnly);
end;  { Update }


end.

