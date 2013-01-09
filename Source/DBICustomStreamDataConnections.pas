// _____________________________________________________________________________
{**
  <H5>Copyright</H5>
  Better Innovative Technical Services Pty Ltd<BR>
  Copyright (C) 1996-2003, All rights reserved.

  Project:       DBIcl50
  Files(s):      DBICustomStreamDataConnections.pas
  Classes:       ...
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 11/05/2005 11:05:30 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

{#omcodecop off : jvr : native api code}

unit DBICustomStreamDataConnections;

interface

uses
  Classes, SysUtils, DBIConst, DBIUtils, DB, DBCommon, DBIStrings, DBIInterfaces,
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
{##JVR
    procedure Error(
      E: Exception;
      const Caller: String;
      const Reference: String;
      const ErrMsg: String;
      Args: array of const
      );
//}
    procedure CheckFileName(const Caller, Line: String);
    function GetBlobFileName: TFileName; virtual; abstract;
    function GetRecordSize: Word; virtual; abstract;
    function HasBlobs: Boolean;

    procedure SetActive(const Value: Boolean); override;
    procedure SetDataStream(AStream: TStream);

  protected
    // Non standard methods - added for this implementation
    procedure CloseDataStream;
    procedure OpenDataStream(const New: Boolean);

    procedure WriteMetaData(
      const Mode: TDBIMetaDataMode = mdAll;
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

    procedure New; override;
    function Append(const Buffer): Integer; override;
    procedure Cancel; override;
//##JVR    procedure Delete(const Position: Integer); override;
//##JVR    procedure Get(const Position: Integer; var Buffer; DataInfo: PDataInfo); override;
    procedure Update(const Position: Integer; const Buffer); override;

    function IsCursorOpen: Boolean; virtual;
    function IsModified: Boolean; virtual;
{##JVR
    procedure LoadFromFile(
      AFileName: TDBIString = '';
      const Format: TDBIDataFormat = dfXbasePlus
      ); override;
}      
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
      var Size: LongWord
      ); override;

    function PutBlob(
      const Position: LongWord;
      const FieldNo: LongWord;
      const OffSet: LongWord;
      PData: Pointer;
      const Size: LongWord
      ): Integer; override;
{//##JVR
    function GetBlobLen(
      const Position: LongWord;
      const FieldNo: LongWord
      ): Integer; override;
}

    // Properties specific to this implementation
    property DataStream: TStream read FDataStream write SetDataStream;
    property FileName: TFileName read GetFileName write SetFileName;
    property BlobFileName: TFileName read GetBlobFileName;

  end;  { TDBIBaseDataConnection }


implementation

const
  UnitName = 'DBICustomStreamDataConnections';

(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 14:41:51<P>
  Jvr - 09/02/2001 15:41:29 - Moved from DBIXBaseDataConnections to DBIUtils<P>
}
function TDBICustomStreamDataConnection.CreateStream(
  const AFileName: TDBIString;
  var AMode: Word;
  AStreamMode: TDBIStreamMode
  ): TStream;
const
  Caller = 'CreateStream';
  fmOpenReadMask   = $FFF0;

var
  LocalStream: TStream;

  function CreateFileStream: TStream;
  begin
    {$ifdef Use_BufferedStreams}
      Result := TDBIFileStream.Create(
        AFileName,
        AMode,
        DBIPageBufferSize,
        [soWriteThrough]
        );
    {$else}
      Result := TFileStream.Create(AFileName, AMode);
    {$endif Use_BufferedStreams}
  end;

begin
  Result := nil;

  if ((AMode and fmCreate) <> fmCreate) and not FileExists(AFileName) then begin
    Error(nil, Caller, '230', 'File "%s" not found', [AFileName]);
  end;

  // Check file for readonly attribute
  // if it is a readonly file then open in readonly mode
  if FileExists(AFileName) and FileIsReadOnly(AFileName) then begin
    AMode := AMode and fmOpenReadMask;
  end;

  case AStreamMode of
    smFileOpen: begin
      Result := CreateFileStream;
    end;  { smFileOpen }

    smLoadFromFile: begin
      LocalStream := CreateFileStream;
      try
        Result := TMemoryStream.Create;
        (Result as TMemoryStream).LoadFromStream(LocalStream);
      finally
        LocalStream.Free;
      end;
    end;

  else
    Result := TMemoryStream.Create;
//##JVR    Error(nil, Caller, '370', 'Unable to access this stream type', []);
  end;  { case }
end;  { CreateStream }
*)


// =============================================================================
// 'TDBICustomStreamDataConnection' Public Methods
// =============================================================================

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
//##JVR    FBlobConnection.CloseBlobStream;
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
  If no datastream present then create one.
  Normally there is none at this stage, except if one has been assigned to
  the DataStream property (e.g. a TMemoryStream created externally)<br>

  Jvr - 01/11/2000 11:48:41<br>
  Jvr - 26/07/2002 15:04:48 - Merged the OpenDatastream method into new.<br>
  Jvr - 31/12/2004 14:08:32 - Blobs are now created/opened in OpenBlob().<br>
}
procedure TDBICustomStreamDataConnection.New;
const
  Caller = 'New';

begin
  if Active then Exit;

(*##JVR
  Modified := False;

  { TODO 2 -ojvr -cTDBIBaseDataConnection.New() :
    26/07/2002 15:01:46
    This is possibly where the decision should be made what type of datastream
    to create. (e.g. FileStream, MemoryStream or other)
  }
  if not Assigned(FDataStream) then begin
    Mode := OpenModes[False] or ShareModes[Exclusive] or fmCreate;
    FDataStream := DBIFileStreams.CreateFileStream(FileName, Mode); //##JVRCreateStream(FFileName, Mode, FStreamMode);
    Assert(Assigned(FDataStream));
  end;
*)
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
//##Left Off Here!    
      PutData(PhysicalBuffer^, Buffer);
    end;

    Result := WriteData(posEof, PhysicalBuffer^);
{##JVR
    FDataStream.Seek(
      (FHeader.RecordSize * Integer(FHeader.RecordCount)) + DataOffset,
      soFromBeginning
      );

    FDataStream.Write(PhysicalBuffer^, FHeader.RecordSize);
}
//##JVR    Modified := True;
//##JVR    FDirty := True;
  finally
    FreePhysicalBuffer(PhysicalBuffer);
  end;
{##JVR
  FHeader.RecordCount := FHeader.RecordCount + 1;
  Result := FHeader.RecordCount;
}
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

(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 13/03/2001 13:44:03 - Deletion in an Xbase file only marks the record
                              deleted. The RecordCount in the metadata (header)
                              is not altered as this is the number of physical
                              records in the file.
                              (thus all records, current & deleted)<br>
}
procedure TDBICustomStreamDataConnection.Delete(const Position: Integer);
var
  DeleteStatus: AnsiChar;

begin
  if (Position < 0) or (Position >= Integer(FHeader.RecordCount)) then begin
    Error(nil, 'Delete', '635', 'Record "%d" out of legal range', [Position]);
  end;

  FDataStream.Seek((FHeader.RecordSize * Position) + DataOffset, soFromBeginning);
  FDataStream.Read(DeleteStatus, 1);

  // Toggle the Deletion Status
  if (DeleteStatus = DT_DELFLAG) then begin
    DeleteStatus := ' ';
  end
  else begin
    DeleteStatus := DT_DELFLAG;
  end;

  FDataStream.Seek((FHeader.RecordSize * Position) + DataOffset, soFromBeginning);
  FDataStream.Write(DeleteStatus, 1);
  Modified := True;
  FDirty := True;
  { TODO 5 -oJvr -cTDBIBaseDataConnection.Delete() :
    This still needs to be done otherwise the indices won't work!
    Or does it? Xbase records don't get really deleted so like in foxpro the
    record entries should probably stay in the index.
    This is a philosophical argument and probably needs some more thought
  }
//##JVR  NotifyDataEventCallBacks(Self, dbiRecordDeleted, Buffer);

  { TODO -oJvr -cTDBIBaseDataConnection.Delete() :
    I'm not sure that we should update the header when doing a delete.
    No records have been added/deleted (the record count only reflects the total
    record count [active & inactive]) so the RecordCount doesn't change,
    and the datestamp is updated when we close the file.
  }
  WriteMetaData(mdHeaderOnly);
end;  { Delete }
//*)
(*##JVR
// _____________________________________________________________________________
{**
  GetRecord data for the specified record position.

  Jvr - 01/11/2000 17:39:47.<br>
  Jvr - 19/03/2001 12:22:50 - Record now returns dsRecActive or dsRecDeleted<br>
}
procedure TDBICustomStreamDataConnection.Get(const Position: Integer; var Buffer; DataInfo: PDataInfo);
var
  PhysicalBuffer: TDBIRecordBuffer;
  LocalPhysicalBuffer: array[0..Default_TemporaryRecordBufferSize] of Ansichar;

begin
  if (Position < 0) or (Position >= Integer(FHeader.RecordCount)) then begin
    Error(nil, 'Get', '685', 'Record "%d" out of legal range', [Position]);
  end;

  // Create a buffer on the heap if required buffer is larger than the LocalBuffer
  if (FHeader.RecordSize > SizeOf(LocalPhysicalBuffer)) then begin
    PhysicalBuffer := AllocPhysicalBuffer;
  end

  // Otherwise just use the LocalBuffer
  else begin
    PhysicalBuffer := LocalPhysicalBuffer;
  end;


  try
    FDataStream.Seek((FHeader.RecordSize * Position) + DataOffset, soFromBeginning);
    FDataStream.Read(PhysicalBuffer^, FHeader.RecordSize);

    // If buffer is nil then we only want the status
    if (@Buffer <> nil) then begin
      GetData(PhysicalBuffer^, Buffer);
    end;

    // If deleted flag is set, then record is deleted
    if Assigned(DataInfo) then begin
      if (PhysicalBuffer[0] = DT_DELFLAG) then begin
        DataInfo.Attribute := dsRecDeleted;
      end

      // Otherwise record is active
      else begin
        DataInfo.Attribute := dsRecActive;
      end;  { if }

      DataInfo.Data := @Buffer;
    end;

  finally
    // Only free buffer if allocated on the heap
    if (FHeader.RecordSize > SizeOf(LocalPhysicalBuffer)) then begin
      FreePhysicalBuffer(PhysicalBuffer);
    end;
  end;
end;  { Get }
//*)
//(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 17:29:39<br>
}
procedure TDBICustomStreamDataConnection.Update(const Position: Integer; const Buffer);
var
  PhysicalBuffer: TDBIRecordBuffer;

begin
  if (Position < 0) or (Position >= Integer(GetRecordCount(dsRecAll))) then begin
    Error(nil, 'Update', '740', 'Record "%d" out of legal range', [Position]);
  end;

  // If buffer is nil then we only want the status
  PhysicalBuffer := AllocPhysicalBuffer;
  try
    if (@Buffer <> nil) then begin
      PutData(PhysicalBuffer^, Buffer);
    end;
    WriteData(Position, PhysicalBuffer^);
{##JVR
    FDataStream.Seek((RecordSize * Position) + DataOffset, soFromBeginning);
    FDataStream.Write(PhysicalBuffer^, RecordSize);
}
//##JVR    Modified := True;
//##JVR    FDirty := True;
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
//*)



// _____________________________________________________________________________
{**
  Jvr - 31/12/2004 14:18:50 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.CloseBlob;
begin
  // Close BlobFile if previously opened
  if Assigned(FBlobConnection) then begin
    FBlobConnection.CloseBlobStream;
//##JVR    FBlobConnection.Free;
//##JVR    FBlobConnection := nil;
  end;
end;  { CloseBlob }


// _____________________________________________________________________________
{**
  Jvr - 31/12/2004 13:54:34 - Initial code.<br>
  Jvr - 09/05/2005 15:47:42 - Added xbVisualFoxpro support.<br>
}
procedure TDBICustomStreamDataConnection.OpenBlob(const CreateNew: Boolean);
const
  Caller = 'OpenBlob';

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
  Jvr - 09/02/2001 13:52:16.<br>
}
procedure TDBICustomStreamDataConnection.GetBlob(
  const Position: LongWord;
  const FieldNo: LongWord;
  const OffSet: LongWord;
  PData: Pointer;
  var Size: LongWord
  );
begin
  FBlobConnection.GetBlob(Position, Offset, PData, Size);
end;  { GetBlob }


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

(*
// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 13:00:59.<br>
  Jvr - 07/03/2001 15:30:18 - If the data file is not allready opened then we
                              attempt to open it, read the header,
                              and then close it again.<BR>
  Jvr - 08/03/2001 13:43:21 - Validate the Header if specified in options.<BR>
  Jvr - 05/06/2001 14:34:17 - Now allows Lower AnsiChars in FieldNames and Digits.<BR>
  Jvr - 14/02/2003 14:07:28 - Now using Setlength to resize the FFields array.<BR>
  Jvr - 14/02/2003 14:58:27 - Added limited support for extended fieldnames.<P>
}
procedure TDBICustomStreamDataConnection.GetMetaData;
var
  FieldNo: Integer;
  BytesRead: Integer;
  PhysicalFieldOffset: LongWord;
  LogicalFieldOffset: Word;
  RecordBufferSize: Integer;
  WasAllReadyOpen: Boolean;
  NullFlagsIndex: Integer;
  IsNullable: Boolean;

begin
  // This allows us to just read the datafile header
  WasAllReadyOpen := Assigned(FDataStream);
  if not WasAllReadyOpen then begin
    OpenDataStream;
{$ifdef Use_BufferedStreams}
  end
  else if (FDataStream is TDBIFileStream) then begin
    (FDataStream as TDBIFileStream).Reset;
{$endif Use_BufferedStreams}
  end;

  try
    // Read General Header Information
    FDataStream.Seek(0, soFromBeginning);
    FDataStream.Read(FHeader, SizeOf(FHeader));

    // Only verify that this is a valid Xbase file if specified in options
    if (xsStrictHeader in FOptions) then begin
      GetVersion;
    end;

    // Read Fields Data
    // Setup Maximum number of allowed field properties in array
    SetLength(FFields, XbaseFoxProMaxFieldCount);
    BytesRead := FDataStream.Read(
      TDBIRecordBuffer(@(FFields[0]))^,
      SizeOf(TDBIBaseField) * XbaseFoxProMaxFieldCount
      );
    if (BytesRead < SizeOf(TDBIBaseField)) then begin
      raise Exception.Create(SReadFieldsFailed);
    end;


    // -------------------------------------------------------------------------
    // Setup the Physical Dataset properies (metadata)

    // First Field Starts at 1 (DelFlag starts at 0)
    PhysicalFieldOffset := 1;

    for FieldNo := 0 to 255 do begin
      with FFields[FieldNo] do begin
        // If not a valid field name character then set FFields array dimension
        // to current number of valid fields read.
        if (FieldName[0] < #48) or
           (FieldName[0] > #122) or
           ((FieldName[0] > #57) and (FieldName[0] < #65)) then
        begin
          // Set the FieldCount and allocate space for logical fieldprops
          FieldCount := FieldNo;
//##JVR          FFields := Copy(FFields, 0, FieldCount);
          SetLength(FFields, FieldCount);
          Break;
        end

        // Update the field offset if not specified in the Header (e.g. Value = 0)
        // Field Offsets need to be calculated if they are non existent
        else if (LongWord(FieldOffset) = 0) then begin
          LongWord(FieldOffset) := PhysicalFieldOffset;
        end; { if }

        // Calculate the Physical Field Offsets
        if (FFields[FieldNo].FieldType = DT_CHARACTER) then begin
          Inc(PhysicalFieldOffset, Word(FieldSize));
        end
        else begin
          Inc(PhysicalFieldOffset, FieldSize[0]);
        end;  { if }

      end;  { with }
    end;  { for }


    // -------------------------------------------------------------------------
    // Setup the Logical Dataset properies (metadata)
    // This is used primarily by IDBIBase & IDBICursor

    // Offset to first field is 0, The logical buffer need not worry about the DelFlag
    LogicalFieldOffset := Default_LogicalBufferFirstFieldOffset;
    RecordBufferSize := 0;
{##JVR//
    // Create the virtual deleted field
    with FieldProps[FieldNo] do begin
      iFieldID := 0;
      StrLCopy(szName, 'Deleted', XBASEMAXNAMELEN);
      iFldType := fldBOOL;
      iFldSubType := fldUNKNOWN;
      iUnits1 := 1;
      iUnits2 := 0;
      iFldLen := 1;
      iFldAttr := 0;
      iFldOffsInRec := LogicalFieldOffset;

      // Calculate the Logical Field Offsets
      iFldOffsInRec := LogicalFieldOffset;
      Inc(LogicalFieldOffset, iFldLen);

      // Calculate Logical RecordSize
      Inc(RecordBufferSize, iFldLen);
    end;
//}

    // Now the real fields
    for FieldNo := 0 to FieldCount - 1 do begin
      with FieldProps[FieldNo] do begin
        iFieldID := FieldNo+1;
        StrLCopy(
          szName,
          FFields[FieldNo].FieldName,
          SizeOf(FFields[FieldNo].FieldName)-1
//##JVR          XBASEMAXNAMELEN
          );

        // If we have extended fields then append the rest of the name
        if (xfExtendedFields in Flags) then begin
          StrLCat(
            szName,
            FFields[FieldNo].FieldData,
            (SizeOf(FFields[FieldNo].FieldName)-1) + SizeOf(FFields[FieldNo].FieldData)
            );
        end;

        GetLogicalProperties(
          FFields[FieldNo],
          iFldType,
          iFldSubType,
          iUnits1,
          iUnits2,
          iFldLen,
          iFldAttr
          );

        // Calculate the Logical Field Offsets
        iFldOffsInRec := LogicalFieldOffset;
        Inc(LogicalFieldOffset, iFldLen);

        // Calculate Logical RecordSize
        Inc(RecordBufferSize, iFldLen);
      end;  { with }
    end;  { for }


    // -------------------------------------------------------------------------
    // Null field flag information
    FNullFlags.IsNullable :=
      SameText(FFields[FieldCount - 1].FieldName, '_Nullflags');

    NullFlagsIndex := 0;
    for FieldNo := 0 to FieldCount - 1 do begin
      // Create Index values for the physical fields Null-Flags
      IsNullable := ffNullableField in FFields[FieldNo].FieldFlags;
      if FNullFlags.SetNullIndex(FieldNo, IsNullable, NullFlagsIndex) then begin
        Inc(NullFlagsIndex);
      end;

      // Calculate Field Null-Flag Offsets
      with FieldProps[FieldNo] do begin
        iNullOffsInRec := RecordBufferSize + FieldNo;
      end;
    end;  { for }

    // Allow for FieldCount * Null-Flags at the end of the Buffer
    Inc(RecordBufferSize, FieldCount);
    LogicalBufferSize := RecordBufferSize;

  finally
    if not WasAllReadyOpen then begin
      CloseDataStream;
    end;
  end;  { try..Finally }
end;  { GetMetaData }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 13:01:03.<br>
  Jvr - 02/02/2001 12:54:49 - WriteMetaData now checks the
                              FDirty Flag and ForceWrite flags
                              writing the MetaData<P>
}
procedure  TDBICustomStreamDataConnection.WriteMetaData(
  const Mode: TDBIBaseHeaderMode = hmAll;
  const ForceWrite: Boolean = False
  );
var
  CurrentPosition: Integer;
  Index: Integer;

begin
  // If the buffers aren't dirty (current data has already been saved)
  // then don't bother writing the MetaData back to disk
  // unless the Forcewrite flags insists we do.
  if not FDirty and not ForceWrite then Exit;

  Assert(Assigned(FDataStream));
  CurrentPosition := FDataStream.Position;
  try
    try
      // Write General Header Information back to stream
      FDataStream.Seek(0, soFromBeginning);
      FDataStream.Write(FHeader, SizeOf(FHeader));
      Modified := True;
    except
      on E: Exception do
        Error(E, 'WriteMetaData', '1310', 'Unable to Write Header Structure', []);
    end;  { try..except }

    if (Mode = hmAll) then begin
      // Write Fields Data
      try
        for Index := 0 to FieldCount-1 do begin
          FDataStream.Write(TDBIRecordBuffer(@(FFields[Index]))^, SizeOf(TDBIBaseField));
        end;
      except
        on E: Exception do
          Error(E, 'WriteMetaData', '1320', 'Unable to Write Fields Structure', []);
      end;  { try..except }
    end;  { if }

  finally
    // Reset Position to original position
    FDataStream.Position := CurrentPosition;
    FDirty := False;
  end;  { try..finally }

end;  { WriteMetaData }
//*)

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
  Jvr - 11/05/2005 16:01:41 - Initial code.<br>
}
function TDBICustomStreamDataConnection.IsCursorOpen: Boolean;
begin
  Result := Assigned(FDataStream); //##JVR or }FCursorOpen;
end;  { IsCursorOpen }


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 11:42:00 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.SaveToFile(
  AFileName: String;
  const Format: TDBIDataFormat
  );
const
  Caller = 'SaveToFile';

begin
  if (AFileName = '') then begin
    AFileName := FileName;
  end;

  if (AFileName = '') then begin
    Error(nil, Caller, '1360', 'Invalid FileName', []);
  end;

  if not Active then begin
    Error(nil, Caller, '1365', 'DataConnection not active', []);
  end;

  if (Format = dfCDS) then begin
    Error(nil, Caller, '1370', 'Saving to XML not Supported Yet!', []);
  end;

  // Save to File
  DBIFileStreams.SaveStreamToFile(FDataStream, AFileName);
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
const
  Caller = 'SaveToStream';

begin
  if (Stream = nil) then begin
    Error(nil, Caller, '1395', 'Invalid Stream', []);
  end;

  if not Active then begin
    Error(nil, Caller, '1400', 'DataConnection not active', []);
  end;

  if (Format = dfCDS) then begin
    Error(nil, Caller, '1405', 'Saving to XML not Supported Yet!', []);
  end;

  if Assigned(FBlobConnection) then begin
    Error(nil, Caller, '1410', 'Blobs not supported when saving to stream', []);
  end;

  // Save to Stream
  if (FDataStream is TMemoryStream) then begin
    (FDataStream as TMemoryStream).SaveToStream(Stream);
  end
  else begin
    Error(nil, Caller, '1415',
      'Saving to stream for a "%s" not implemented yet!',
      [FDataStream.ClassName]
      );
  end;

  Modified := False;
end;  { SaveToStream }



// =============================================================================
// 'TDBIBaseDataConnection' protected methods
// =============================================================================

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
  Jvr - 01/11/2000 12:40:02 - Initial code.<br>
  Jvr - 29/10/2002 14:07:08 - Readonly file support added.<br>
}
procedure TDBICustomStreamDataConnection.OpenDataStream(const New: Boolean);
var
  Mode: Word;

begin
  if not Assigned(FDataStream) then begin
    Mode := OpenModes[ReadOnly] or ShareModes[Exclusive] or CreateModes[New];

    FDataStream := DBIFileStreams.CreateStream(FileName, Mode, StreamMode);
    CanModify := (Mode and fmOpenReadWrite) = fmOpenReadWrite;

    Modified := False;
    Assert(Assigned(FDataStream));
  end;
end;  { OpenDataStream }


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
  Close;
  Release;

  FileName := '';
  FDataStream := AStream;

  SetStreamMode(smExternalStream, AStream <> nil);
end;  { SetDataStream }



// =============================================================================
// 'TDBIBaseDataConnection' Private methods
// =============================================================================


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 14:22:32 - Initial code.<br>
}
(*##JVR
procedure TDBICustomStreamDataConnection.Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
begin
{$ifdef DebugExceptions}
  raise EDBIException.CreateFmt(
    UnitName + '::' + Self.ClassName + '::' + Caller + '::' + Reference + #13 + ErrMsg, Args);
{$else}
  raise EDBIException.CreateFmt(ErrMsg, Args);
{$endif DebugExceptions}
end;  { Error }
//*)

// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 18:26:28 - Initial code.<br>
}
procedure TDBICustomStreamDataConnection.CheckFileName(const Caller, Line: String);
begin
  if (FileName = '') then begin
    Error(nil, Caller, Line, 'Invalid FileName', []);
  end;
end;  { CheckFileName }



(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 29/10/2002 15:50:52 - Initial code.<br>
}
function TDBICustomStreamDataConnection.GetBlobFileName: TDBIString;
const
  ExtensionMap: array[TDBIbaseVersion] of TDBIString = (
    '.err',        // xbUnknownXbaseVersion,
    '.fpt',        // xbFoxBase,
    '.fpt',        // xbFoxPro,
    '.fpt',        // xbVisualFoxPro,
    '.dbt',        // xbDbase3,
    '.dbt',        // xbDbase4,
    '.dbt'         // xbDbase5
    );
begin
  Result := ChangeFileExt(FFileName, ExtensionMap[Version]);
end;  { GetBlobFileName }
*)

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



end.

