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
  1.0 | 02/05/2002 14:57:55 | Jvr | Initial Release
  ______________________________________________________________________________
}

unit DBIFileStreams;

{$I DBICompilers.inc}

{$ifdef fpc}
  {$asmmode intel}
  {$mode delphi}
{$endif}

{$define Use_BufferedStreams 1}

interface

uses
{$ifdef DELPHI6}
  RtlConsts,
{$endif}
{$ifndef fpc}
  Consts,
{$endif}
  Classes, SysUtils, Windows, TypInfo, DBIConst, DBIUtils;

const
  DBIPageBlockSize = 1024;             // Block size for buffered stream I/O
  DBIPageBlockMask = $FFFFFC00;        // Block mask for buffered stream I/O
  DBIPageMaximumSize = 32 * DBIPageBlockSize;
  DBIPageBufferSize = 16 * DBIPageBlockSize;


type
  TDBIHandle = THandle;                // File handle type

  TDBIHandleStreamOption = (soWriteThrough, soForceFlush);
  TDBIHandleStreamOptions = set of TDBIHandleStreamOption;

  PDBIByteArray = ^TDBIByteArray;
  TDBIByteArray = array[0..32767] of Byte;
  TDBIBufferArray = array[0..$7FFFFFFE] of Byte;

type
  TDBIHandleStream = class(TStream)
  private
    FHandle: TDBIHandle;               // Handle of file
    FPage: PDBIByteArray;              // Page buffer
    FPageSize: Longint;                // Size of Page buffer (multiple of 1K)
    FPageStart: Longint;               // Start of Page buffer as offset in stream
    FPagePosition: Longint;            // Current position in page buffer
    FPageByteCount: Longint;           // Count of valid bytes in Page buffer
    FSize: Longint;                    // Count of bytes in stream
    FDirty: Boolean;                   // Status the Page buffer
    FOptions: TDBIHandleStreamOptions; // Options to control Streaming behaviour

  protected
    class procedure Error(
      E: Exception;
      const Caller: String;
      const Reference: String;
      const ErrMsg: String;
      Args: array of const
      );

    function Commit(grfCommitFlags: Longint = 0): Longint; virtual;

    procedure PageRead;
    procedure PageWrite;

    property Options: TDBIHandleStreamOptions read FOptions write FOptions;

  public
    constructor Create(
      AHandle: TDBIHandle;
      APageSize: Longint;
      AOptions: TDBIHandleStreamOptions
      );
    destructor Destroy; override;

    procedure Reset;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure SetSize(NewSize: Longint); override;

    property Handle: TDBIHandle read FHandle;
  end;  { TDBIBufferedHandleStream }


type
  TDBIFileStream = class(TDBIHandleStream)
  private
    FFileName: String;

  public
    class function CreateFileStream(const AFileName: String; AMode: Word): TStream;

    class function CreateStream(
      const AFileName: String;
      var AMode: Word;
      AStreamMode: TDBIStreamMode
      ): TStream;

    class procedure SaveStreamToFile(Stream: TStream; const AFileName: String);


  public
    constructor Create(
      const AFileName: String;
      Mode: Word;
      APageSize: Longint;
      AOptions: TDBIHandleStreamOptions
      );

    destructor Destroy; override;

    property FileName: String read FFileName;
  end;


implementation

  
{ TDBIHandleStream }

// _____________________________________________________________________________
{**
  Jvr - 02/05/2002 16:28:04.<P>
}
constructor TDBIHandleStream.Create(
  AHandle: TDBIHandle;
  APageSize: Integer;
  AOptions: TDBIHandleStreamOptions
  );
const
  Caller = 'Create';

begin
  inherited Create;

  // Save the handle
  FHandle := AHandle;
  FOptions := AOptions;

  // Round up the buffer size to a multiple of 1K and a maximum of 32K
  FPageSize := (Longint(APageSize) + (DBIPageBlockSize-1)) and DBIPageBlockMask;
  if (FPageSize > DBIPageMaximumSize) then begin
    FPageSize := DBIPageMaximumSize;
  end;

  // Create the buffer
  Reset;
  GetMem(FPage, FPageSize);

  if (FSize = -1) then begin
    Error(nil, Caller, '190', 'Getting file size, Failed to seek to EOF', []);
  end;
end;  { Create }


// _____________________________________________________________________________
{**
  Destroy the buffer, if need be after writing it to disk.

  Jvr - 02/05/2002 16:32:01.<P>
}
destructor TDBIHandleStream.Destroy;
begin
  // If we have any outstanding data to be written then now is the time.
  Reset;

  // Free the Page buffer
  if (FPage <> nil) then begin
    FreeMem(FPage, FPageSize);
  end;  { if }

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  The parameter and result are ignored, they have no relevance for this class.

  Jvr - 27/11/2002 16:57:58.<P>
}
function TDBIHandleStream.Commit(grfCommitFlags: Longint = 0): Longint;
begin
  Result := 0; { = Windows.S_OK }

  if (soForceFlush in FOptions) then begin
{$ifdef LINUX}
    raise Exception.Create('Windows.FlushFileBuffers() is a windows call!');
{$else}
  {$ifdef DELPHI6} {$WARN SYMBOL_PLATFORM OFF} {$endif}
    SysUtils.Win32Check(Windows.FlushFileBuffers(FHandle));
  {$ifdef DELPHI6} {$WARN SYMBOL_PLATFORM ON} {$endif}
{$endif}
  end;  { if }
end;  { FlushFileBuffers }


// _____________________________________________________________________________
{**
  Jvr - 02/05/2002 16:37:01.<P>
}
procedure TDBIHandleStream.PageRead;
const
  Caller = 'PageRead';

var
  Offset: Longint;

begin
  Offset := FileSeek(FHandle, FPageStart, soFromBeginning);
  if (Offset = -1) then begin
    Error(nil, Caller, '237', 'Failed to seek to position %d', [FPageStart]);
  end;

  FPageByteCount := FileRead(FHandle, FPage^, FPageSize);
  if (FPageByteCount <= 0) then begin
    Error(nil, Caller, '240', 'Failed to read from position %d', [FPageStart]);
  end;
end;  { PageRead }


// _____________________________________________________________________________
{**
  Jvr - 02/05/2002 16:38:34.<P>
}
procedure TDBIHandleStream.PageWrite;
const
  Caller = 'PageWrite';

var
  Offset: Longint;
  BytesWritten: Longint;

begin
  // If write through mode has been set then skip all this
  if not (soWriteThrough in FOptions) then begin
    Offset := FileSeek(FHandle, FPageStart, soFromBeginning);
    if (Offset = -1) then begin
      Error(nil, Caller, '265', 'Failed to seek to position %d', [FPageStart]);
    end;

    BytesWritten := FileWrite(FHandle, FPage^, FPageByteCount);
    if (BytesWritten <> FPageByteCount) then begin
      Error(nil, Caller, '270', 'Failed to write from position %d', [FPageStart]);
    end;

    Commit;
  end;  { if }
end;  { PageWrite }


// _____________________________________________________________________________
{**
  Set the page/buffer variables to the start of the stream

  Jvr - 19/06/2002 11:29:58.<P>
}
procedure TDBIHandleStream.Reset;
begin
  if (FPage <> nil) then begin
    // If we have any outstanding data to be written then now is the time.
    if FDirty then begin
      PageWrite;
      FDirty := False;
    end;

    Commit;
  end;  { if }

  FPagePosition := 0;
  FPageByteCount := 0;
  FPageStart := 0;
  FDirty := False;
  FSize := FileSeek(FHandle, 0, soFromEnd);
end;  { Reset }


// _____________________________________________________________________________
{**
  Reads up to Count bytes of data from the resource associated with the handle
  stream into Buffer.

  Use Read to read data from the resource associated with the handle stream
  when the number of bytes in the file is not known. Read transfers up to
  Count bytes from the resource, starting at the current position, and then
  advances the current position in the resource by the number of bytes
  actually transferred. Read returns the number of bytes actually transferred,
  which may be less than Count if the end of file marker is encountered.<P>

  Reading is complicated by the fact we can only read in chunks of
  FPageSize: we need to partition out the overall read into a
  read from part of the buffer, zero or more reads from complete
  buffers and then a possible read from part of a buffer.<P>

  Jvr - 02/05/2002 16:48:40.<P>
}
function TDBIHandleStream.Read(var Buffer; Count: Longint): Longint;
var
  PBuffer: {TDBIByteArray} TDBIBufferArray absolute Buffer;
  BufferIndex: Longint;
  BytesRemaining: Longint;
  BytesToRead: Longint;

begin
  // Calculate the actual number of bytes we can read - this depends on
  // the current position and size of the stream as well as the number
  // of bytes requested
  BytesRemaining := Count;
  if (FSize < (FPageStart + FPagePosition + Count)) then begin
    BytesRemaining := FSize - (FPageStart + FPagePosition);
  end;

  if (BytesRemaining <= 0) then begin
    Result := 0;
    Exit;
  end;

  // Remember to return the result of our calculation
  Result := BytesRemaining;

  // Initialise the buffer index for the caller's passed in buffer
  BufferIndex := 0;

  // Is there anything in the buffer? if not, go read something from
  // the file on disk
  if (FPageByteCount = 0) then begin
    PageRead;
  end;

  // Calculate the number of bytes we can read prior to the loop
  BytesToRead := FPageByteCount - FPagePosition;
  if (BytesToRead > BytesRemaining) then begin
    BytesToRead := BytesRemaining;
  end;

  // Copy from the stream buffer to the caller's buffer
  Move(FPage^[FPagePosition], PBuffer[BufferIndex], BytesToRead);

  // Calculate the number of bytes still to read
  Dec(BytesRemaining, BytesToRead);

  // While we have bytes to read, read them
  while (BytesRemaining > 0) do begin
    // Advance the buffer index for the caller's buffer
    Inc(BufferIndex, BytesToRead);

    // As we've exhausted this buffer-full, advance to the next, check
    // to see whether we need to write the buffer out first
    if FDirty then begin
      PageWrite;
      FDirty := False;
    end;

    Inc(FPageStart, FPageSize);
    FPagePosition := 0;
    PageRead;

    // Calculate the number of bytes we can read in this cycle
    BytesToRead := FPageByteCount;
    if (BytesToRead > BytesRemaining) then begin
      BytesToRead := BytesRemaining;
    end;

    // Copy from the stream buffer to the caller's buffer
    Move(FPage^, PBuffer[BufferIndex], BytesToRead);

    // Calculate the number of bytes still to read
    Dec(BytesRemaining, BytesToRead);
  end;  { while }

  // Remember our new position
  Inc(FPagePosition, BytesToRead);

  if (FPagePosition = FPageSize) then begin
    Inc(FPageStart, FPageSize);
    FPagePosition := 0;
    FPageByteCount := 0;
  end;
end;  { Read }


// _____________________________________________________________________________
{**
  Resets the current position of the handle stream.

  Use Seek to move the current position within the resource associated with
  the handle stream by the indicated offset. Seek allows an application to
  read from or write to a particular location within the resource.<P>

  The Origin parameter indicates how to interpret the Offset parameter.<P>

  Seek returns the new value of the Position property, the new current
  position in the resource.<P>

  Jvr - 02/05/2002 16:52:50.<P>
}
function TDBIHandleStream.Seek(Offset: Longint; Origin: Word): Longint;
const
  Caller = 'Seek';

var
  NewPageStart: Longint;
  NewPos: Longint;

begin
  // Calculate the new position
  case Origin of
    soFromBeginning: NewPos := Offset;
    soFromCurrent:   NewPos := FPageStart + FPagePosition + Offset;
    soFromEnd:       NewPos := FSize + Offset;
  else
    NewPos := 0;
    Error(nil, Caller, '415', 'Invalid origin of "%d" specified', [Origin]);
  end;  { case }

  if (NewPos < 0) or (NewPos > FSize) then begin
    Error(nil, Caller, '420', 'Invalid offset of "%d" specified', [Offset]);
  end;

  // Calculate which page of the file we need to be at
  NewPageStart := NewPos and not(Pred(Longint(FPageSize)));

  // If the new page is different than the old, mark the buffer as being
  // ready to be replenished, and if need be write out any dirty data
  if (NewPageStart <> FPageStart) then begin
    if FDirty then begin
      PageWrite;
      FDirty := False;
    end;

    FPageStart := NewPageStart;
    FPageByteCount := 0;
  end;

  // Save the new position
  FPagePosition := NewPos - NewPageStart;
  Result := NewPos;
end;  { Seek }


// _____________________________________________________________________________
{**
  Sets the end of file marker to truncate the resource at the indicated position.

  Call SetSize to set the size of the resource. Size overrides the inherited
  method to allow the size of the resource identified by the handle to be
  changed. SetSize calls Seek to go to the indicated position, and then writes
  an end of file marker. If the size of the resource can not be changed,
  an exception is raised. For example, calling SetSize for a file handle that
  was opened in fmOpenRead mode will raise an exception.<P>

  Jvr - 02/05/2002 16:54:03.<P>
}
procedure TDBIHandleStream.SetSize(NewSize : Longint);
const
  Caller = 'SetSize';

begin
  // Save the new size and alter the position if required
  FSize := NewSize;

  if ((FPageStart + FPagePosition) > NewSize) then begin
    Seek(0, soFromEnd);
  end;

  // Now truncate/extend the file handle
  if FileSeek(FHandle, NewSize, soFromBeginning) = -1 then begin
    Error(nil, Caller, '470', 'Failed to seek to position %d', [NewSize]);
  end;

{$ifdef LINUX}
    if ftruncate(FHandle, Position) = -1 then begin
      raise EStreamError(sStreamSetSize);
    end;
{$else}
  {$ifdef DELPHI6} {$WARN SYMBOL_PLATFORM OFF} {$endif}
    Win32Check(SetEndOfFile(FHandle));
  {$ifdef DELPHI6} {$WARN SYMBOL_PLATFORM ON} {$endif}
{$endif}
end;  { SetSize }


// _____________________________________________________________________________
{**
  Writes Count bytes from the Buffer to the current position in the resource.

  Use Write to write Count bytes to the resource associated with the
  handle stream, starting at the current position. After writing to the
  resource, Write advances the current position by the number bytes written,
  and returns the number of bytes written.<P>

  Writing is complicated by the fact we write in chunks of
  FPageSize: we need to partition out the overall write into a
  write from part of the buffer, zero or more writes from complete
  buffers and then a possible write from part of a buffer.<P>

  Jvr - 02/05/2002 16:57:49.<P>
}
function TDBIHandleStream.Write(const Buffer; Count: Longint): Longint;
const
  Caller = 'Write';

var
  PBuffer: {TDBIByteArray} TDBIBufferArray absolute Buffer;
  BufferIndex: Longint;
  BytesRemaining: Longint;
  BytesToWrite: Longint;
  Offset: Longint;

begin
  // Return the number of bytes written
  // This value is not nescessarily the real amount of bytes written
  Result := Count;

  // When we write to the stream we always assume that we can write the
  // requested number of bytes: if we can't (eg, the disk is full) we'll
  // get an exception somewhere eventually
  BytesRemaining := Count;

  // Initialise the buffer index for the caller's passed in buffer
  BufferIndex := 0;

  // Is there anything in the buffer? if not, go try read a block from
  // the file on disk - we might be overwriting existing data rather
  // than appending data to the end of the stream
  if (FPageByteCount = 0) and (FSize > FPageStart) then begin
    PageRead;
  end;


  // If write through mode is set then write straight to the HandleStream
  // PageWrite() takes care of it self,
  // in write through mode writing back the page is skipped.
  if (soWriteThrough in FOptions) then begin
    Offset := FileSeek(FHandle, FPageStart + FPagePosition, soFromBeginning);
    if (Offset = -1) then begin
      Error(nil, Caller, '530', 'Failed to seek to position %d', [FPageStart]);
    end;

    Result := FileWrite(FHandle, Buffer, Count);

    if (Result = -1) then begin
      Result := 0;
    end;
  end;  { if }


  // Calculate the number of bytes we can write prior to the loop
  BytesToWrite := FPageSize - FPagePosition;

  if (BytesToWrite > BytesRemaining) then begin
    BytesToWrite := BytesRemaining;
  end;

  // Copy from the caller's buffer to the stream buffer
  Move(PBuffer[BufferIndex], FPage^[FPagePosition], BytesToWrite);

  // Mark the stream buffer as requiring a save to disk, note that this
  // will suffice for the rest of the routine as well: no inner routine
  // will turn off the dirty flag
  FDirty := True;

  // Calculate the number of bytes still to write
  // BytesToWrite are the number of bytes allready written and need to be
  // subtracted from the number of Bytes remaining
  Dec(BytesRemaining, BytesToWrite);

  // While we have bytes to write, write them
  while (BytesRemaining > 0) do begin
    // Advance the buffer index for the caller's buffer
    Inc(BufferIndex, BytesToWrite);

    // As we've filled this buffer, write it out to disk and advance to
    // the next buffer, reading it if required
    FPageByteCount := FPageSize;
    PageWrite;

    Inc(FPageStart, FPageSize);
    FPagePosition := 0;
    FPageByteCount := 0;
    if (FSize > FPageStart) then begin
      PageRead;
    end;

    // Calculate the number of bytes we can write in this cycle
    BytesToWrite := FPageSize;
    if (BytesToWrite > BytesRemaining) then begin
      BytesToWrite := BytesRemaining;
    end;

    // Copy from the caller's buffer to the stream buffer
    Move(PBuffer[BufferIndex], FPage^, BytesToWrite);

    // Calculate the number of bytes still to write
    Dec(BytesRemaining, BytesToWrite);
  end;  { while }

  // Remember our new position
  Inc(FPagePosition, BytesToWrite);

  // Make sure the count of valid bytes is correct
  if (FPageByteCount < FPagePosition) then begin
    FPageByteCount := FPagePosition;
  end;

  // Make sure the stream size is correct
  if (FSize < (FPageStart + FPageByteCount)) then begin
    FSize := FPageStart + FPageByteCount;
  end;

  // If we're at the end of the buffer, write it out and advance to the
  // start of the next page
  if (FPagePosition = FPageSize) then begin
    PageWrite;
    FDirty := False;

    Inc(FPageStart, FPageSize);
    FPagePosition := 0;
    FPageByteCount := 0;
  end;
end;  { Write }


// _____________________________________________________________________________
{**
  Jvr - 02/05/2002 18:46:12.<P>
}
class procedure TDBIHandleStream.Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
const
  SDebugException = #13#13'raised exception %s with message'#13'%s';

var
  Address: Pointer;

{$ifdef DebugExceptions}
  function GetUnitName: String;
  begin
    Result := String(TypInfo.GetTypeData(Self.ClassInfo)^.UnitName);
  end;
{$endif}

var
  DebugInfo: String;
  ErrorInfo: String;

begin
  asm
    mov eax, [ebp + 4] // get return address
    mov Address, eax
  end;

  ErrorInfo := '';
{$ifdef DebugExceptions}
  if Assigned(E) then begin
    ErrorInfo := Format(SDebugException, [E.ClassName, E.Message]);
  end;

  DebugInfo := GetUnitName + '::' + Self.ClassName + '::' + Caller + '::' + Reference + #13;
{$else}
  DebugInfo := '';
{$endif}

  raise EDBIException.CreateFmt(DebugInfo + ErrMsg + ErrorInfo, Args) at Address;
end;  { Error }





{ TDBIFileStream }

// _____________________________________________________________________________
{**
  Jvr - 02/05/2002 16:04:15.<P>
}
constructor TDBIFileStream.Create(
  const AFileName: String;
  Mode: Word;
  APageSize: Longint;
  AOptions: TDBIHandleStreamOptions
  );
var
  Handle: TDBIHandle;

begin
  if (Mode and fmCreate = fmCreate) then begin
    Handle := FileCreate(AFileName);
    if (Handle = INVALID_HANDLE_VALUE) then begin
      raise EFCreateError.CreateResFmt(@SFCreateError, [AFileName]);
    end;
  end

  else begin
    Handle := FileOpen(AFileName, Mode);
    if (Handle = INVALID_HANDLE_VALUE) then begin
      raise EFOpenError.CreateResFmt(@SFOpenError, [AFileName]);
    end;
  end;  { if }

  inherited Create(Handle, APageSize, AOptions);

  FFileName := ExpandFileName(AFileName);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 02/05/2002 16:21:02.<P>
}
destructor TDBIFileStream.Destroy;
begin
  // Inherited is called first on purpose to perform all the inherited
  // functionality related to the filehandle before closing it.
  inherited Destroy;

  if (FHandle > 0) then begin
    FileClose(FHandle);
  end;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 28/06/2005 13:20:52 - Updated code.<br>
}
class function TDBIFileStream.CreateFileStream(const AFileName: String; AMode: Word): TStream;
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


// _____________________________________________________________________________
{**
  Jvr - 31/10/2000 18:53:02.<P>
  Jvr - 09/02/2001 15:44:30 - Moved from DBIXBaseDataConnections to DBIUtils.<P>
  Jvr - 08/05/2001 13:20:17 - Added exception handling to detect why a file
                              could not be opened.<P>
}
class function TDBIFileStream.CreateStream(
  const AFileName: String;
  var AMode: Word;
  AStreamMode: TDBIStreamMode
  ): TStream;
const
  Caller = 'CreateStream';
  fmOpenReadMask   = $FFF0;

var
  LocalStream: TStream;

begin
  Result := nil;

  if ((AMode and fmCreate) <> fmCreate) and (AFileName <> '') and not FileExists(AFileName) then begin
    Error(nil, Caller, '230', 'File "%s" not found', [AFileName]);
  end;

  // Check file for readonly attribute
  // if it is a readonly file then open in readonly mode
  if FileExists(AFileName) and FileIsReadOnly(AFileName) then begin
    AMode := AMode and fmOpenReadMask;
  end;

  case AStreamMode of
    smMemoryStream: begin
      Result := TMemoryStream.Create;
    end;  { smMemoryStream }

    smFileStream: begin
      Result := CreateFileStream(AFileName, AMode);
    end;  { smFileStream }

    smLoadFromFile: begin
      LocalStream := CreateFileStream(AFileName, AMode);
      try
        Result := TMemoryStream.Create;
        (Result as TMemoryStream).LoadFromStream(LocalStream);
      finally
        LocalStream.Free;
      end;
    end;  { smLoadFromFile }

  else
    Error(nil, Caller, '370', 'Unable to access this stream type', []);
  end;  { case }
end;  { CreateStream }



// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 11:24:55.<P>
}
class procedure TDBIFileStream.SaveStreamToFile(Stream: TStream; const AFileName: String);
const
  Caller = 'SaveToFile';

var
  LocalStream: TStream;

  function CreateFileStream: TStream;
  begin
    Result := nil;

    if (AFileName = '') then begin
      Error(nil, Caller, '860', 'Invalid FileName', []);
    end;

    try
{$ifdef Use_BufferedStreams}
      Result := TDBIFileStream.Create(
        AFileName,
        fmCreate or fmShareDenyNone,
        DBIPageBufferSize,
        [{ No Options}]
        );
{$else}
      Result := TFileStream.Create(
        AFileName,
        fmCreate or fmShareDenyNone
        );
{$endif Use_BufferedStreams}

    except
      on E: Exception do
        Error(E, Caller, '915',
          'Unable to open file "%s" for writing'#13#10'%s',
          [AFileName, DBIUtils.SystemErrorMessageParam(ExtractFileName(ParamStr(0)))]
          );
    end;

    Assert(Result <> nil);
  end;  { CreateFileStream }


begin
  Assert(Assigned(Stream));

  LocalStream := CreateFileStream;
  try
    if (Stream is TMemoryStream) then begin
      (Stream as TMemoryStream).SaveToStream(LocalStream);
    end
    else begin
      Error(nil, Caller, '890',
        'SaveStreamToFile for this type of stream not implemented Yet', []
        );
    end;
  finally
    LocalStream.Free;
  end;
end;  { SaveStreamToFile }


end.
