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
  1.0 | 11/11/2002 13:25:26 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIMemoryStreams;

{$I DBICompilers.inc}

interface

uses
  Windows, Classes;

type
  TVirtualMemoryStreamMode = (vsUnknown, vsStandard, vsVirtual);

type
  TDBIMemoryMappedFile = class(TPersistent)
  private
    FFileName: String;
    FMode: Word;
    FHandle: Integer;
    FFileMapping: THandle;
    FMemory: Pointer;

  protected
    class procedure CheckFreeSpace(const APath: String; const Size: Int64);
    class function CurrentDirName: String;
    class function TempDirName(const RequiredBytes: Int64): String;
    class function TempFileName(const RequiredBytes: Int64): String;

  public
    class function VirtualAllocPtr(
      out MemoryMappedFile: TDBIMemoryMappedFile;
      const Bytes: Longint
      ): Pointer;
    class function VirtualReallocPtr(
      var MemoryMappedFile: TDBIMemoryMappedFile;
      const Bytes: Longint
      ): Pointer;

    constructor Create(const AFileName: String; AMode: Word);
    destructor Destroy; override;

    function Map(const Size: Integer = 0): Pointer;
    procedure UnMap;

    property Filename: String read FFileName;
    property Memory: Pointer read FMemory;
  end;  { TMappedFile }


  TDBIVirtualMemoryStream = class(TCustomMemoryStream)
  private
    FVirtualMemory: TDBIMemoryMappedFile;
    FCapacity: Longint;
    FMode: TVirtualMemoryStreamMode;

  protected
    procedure InternalSetPosition(const NewPosition: Longint);
    procedure InternalSetSize(const NewSize: Longint);

    function GetFileName: String;
    function GetMode(const Bytes: LongInt): TVirtualMemoryStreamMode;
    function GetVirtualMemoryThreshhold: LongInt;

    function VirtualRealloc(var NewCapacity: Longint): Pointer; virtual;
    function Realloc(var NewCapacity: Longint): Pointer; virtual;
    procedure SetCapacity(NewCapacity: Longint); virtual;

    property Capacity: Longint read FCapacity write SetCapacity;

  public
    destructor Destroy; override;

    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property FileName: String read GetFileName;
    property Mode: TVirtualMemoryStreamMode read FMode;
  end;



implementation

uses
{$ifdef DELPHI6}
  RtlConsts,
{$else}
  FileCtrl,  
{$endif}
{$ifndef fpc}
  Consts,
{$endif}
  SysUtils,
  DBIUtils;



// =============================================================================
// 'TMemoryMappedFile' public methods
// =============================================================================

class function TDBIMemoryMappedFile.VirtualAllocPtr(
  out MemoryMappedFile: TDBIMemoryMappedFile;
  const Bytes: Integer
  ): Pointer;
var
  TempTarget: String;

begin
  TempTarget := TempFileName(Bytes);
  CheckFreeSpace(TempTarget, Bytes);

  MemoryMappedFile := TDBIMemoryMappedFile.Create(TempTarget, fmCreate);
  Result := MemoryMappedFile.Map(Bytes);
end;  { VirtualAllocPtr }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 14:36:17.<P>
}
class function TDBIMemoryMappedFile.VirtualReallocPtr(
  var MemoryMappedFile: TDBIMemoryMappedFile;
  const Bytes: Longint
  ): Pointer;
var
  TempTarget: String;

begin
  TempTarget := TempFileName(Bytes);
  CheckFreeSpace(TempTarget, Bytes);
  Assert(MemoryMappedFile <> nil);
  
  MemoryMappedFile.UnMap;
  Result := MemoryMappedFile.Map(Bytes);
end;  { VirtualReallocPtr }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 12:48:13.<P>
}
constructor TDBIMemoryMappedFile.Create(const AFileName: String; AMode: Word);
begin
  inherited Create;

  Assert(AFileName <> '');
  FFileName := AFileName;
  FMode := AMode;

  if (FMode = fmCreate) then begin
//##JVR!!
Sleep(1);
    FHandle := FileCreate(FFileName);
    if (FHandle < 0) then begin
      raise EFCreateError.CreateResFmt(@SFCreateError, [FFileName]);
    end;
  end
  else begin
    FHandle := FileOpen(FFileName, FMode);
    if (FHandle < 0) then begin
      raise EFOpenError.CreateResFmt(@SFOpenError, [FFileName]);
    end;
  end;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 12:49:58.<P>
}
destructor TDBIMemoryMappedFile.Destroy;
begin
  UnMap;

  if (FHandle >= 0) then begin
    FileClose(FHandle);
  end;

  DeleteFile(FFileName);

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 12:45:21.<P>
}
function TDBIMemoryMappedFile.Map(const Size: Integer = 0): Pointer;
var
  Access: Integer;

begin
  if (FMode and fmOpenReadWrite) <> 0 then begin
    Access := PAGE_READWRITE;
  end
  else begin
    Access := PAGE_READONLY;
  end;

  FFileMapping := CreateFileMapping(FHandle, nil, Access, 0, Size, nil);
  if (FFileMapping = 0) then begin
    raise Exception.CreateFmt('CreateFileMapping failed'#13'%s', [
      DBIUtils.SystemErrorMessageParam(FFileName)
      ]);
  end;

  if (FMode and fmOpenReadWrite) <> 0 then begin
    Access := FILE_MAP_WRITE;
  end
  else begin
    Access := FILE_MAP_READ;
  end;

  FMemory := MapViewOfFile(FFileMapping, Access, 0, 0, 0);
  if (FMemory = nil) then begin
    raise Exception.CreateFmt('CreateFileMapping failed'#13'%s', [
      DBIUtils.SystemErrorMessageParam(FFileName)
      ]);
  end;

  Result := FMemory;
end;  { MapFile }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 12:44:14.<P>
}
procedure TDBIMemoryMappedFile.UnMap;
begin
 if (FMemory <> nil) then begin
   UnmapViewOfFile(FMemory);
 end;

 if (FFileMapping <> 0) then begin
   CloseHandle(FFileMapping);
 end;
end;  { UnMap }



// =============================================================================
// 'TMemoryMappedFile' protected methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 13/11/2002 11:44:24.<P>
}
class procedure TDBIMemoryMappedFile.CheckFreeSpace(
  const APath: String;
  const Size: Int64
  );
var
  FreeSpace: Int64;

begin
  FreeSpace := DiskFree((Byte(APath[1]) and $9F));

  if (FreeSpace < Size) then begin
    raise EStreamError.CreateFmt(
      'Insufficient Diskspace to allocate %d bytes for %s, only %d bytes available',
      [Size, APath, FreeSpace]
      );
  end;
end;  { CheckFreeSpace }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 15:28:58.<P>
}
class function TDBIMemoryMappedFile.CurrentDirName: String;
var
  Size: DWORD;

begin
  Size := MAX_PATH;
  SetLength(Result, Size);
  Size := GetCurrentDirectory(Size, PChar(Result));
  SetLength(Result, Size);

{$ifdef DELPHI6}
  Result := IncludeTrailingPathDelimiter(Result);
{$else}
  Result := IncludeTrailingBackSlash(Result);
{$endif}
end;  { CurrentDirName }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 14:27:40.<P>
}
class function TDBIMemoryMappedFile.TempFileName(const RequiredBytes: Int64): String;
begin
  Result :=
    TempDirName(RequiredBytes) +
    ChangeFileExt(ExtractFileName(ParamStr(0)), '') +
    FormatDateTime('YYYYMMDDHHNNSSZZZ', Now) +
    '.vmf';
end;  { TempFileName }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 14:43:28.<P>
}
class function TDBIMemoryMappedFile.TempDirName(const RequiredBytes: Int64): String;
label
  TrailingPathDelimiter;

var
  Size: DWORD;

  function ExtractPathName(const Paths: String; var Pos: Integer): string;
  var
    Offset: Integer;

  begin
    Offset := Pos;
    while (Offset <= Length(Paths)) and (Paths[Offset] <> ';') do Inc(Offset);
    Result := Trim(Copy(Paths, Pos, Offset - Pos));
    if (Offset <= Length(Paths)) and (Paths[Offset] = ';') then Inc(Offset);
    Pos := Offset;
  end;

  function GetPath(var PathNames: String): Integer;
  var
    Offset: Integer;
    TempPath: String;

  begin
    Result := 0;
    Offset := 1;

    while Offset <= Length(PathNames) do begin
      TempPath := ExtractPathName(PathNames, Offset);
      if DiskFree((Byte(TempPath[1]) and $9F)) > RequiredBytes then begin
        PathNames := TempPath;
        Result := Length(PathNames);
        Break;
      end;
    end;
  end;  { GetPath }

begin
  Size := MAX_PATH;
  SetLength(Result, Size);

  Size := GetEnvironmentVariable('TempPath', PChar(Result), Size);
  if (Size > 0) then begin
    SetLength(Result, Size);
    Size := GetPath(Result);
  end;

  if (Size = 0) then begin
    Size := MAX_PATH;
    Size := GetEnvironmentVariable('Temp', PChar(Result), Size);
  end;

  if (Size = 0) then begin
    Size := MAX_PATH;
    Size := GetEnvironmentVariable('Tmp', PChar(Result), Size);
  end;

  if (Size = 0) then begin
    Result := 'C:\Temp';
    if DirectoryExists(Result) then
      goto TrailingPathDelimiter;

    Result := 'C:\Tmp';
    if DirectoryExists(Result) then
      goto TrailingPathDelimiter;

    Result := 'C:\WinNT\Temp';
    if DirectoryExists(Result) then
      goto TrailingPathDelimiter;

    Result := 'C:\Windows\Temp';
    if DirectoryExists(Result) then
      goto TrailingPathDelimiter;

    Result := ExtractFilePath(ParamStr(0));
  end
  else begin
    SetLength(Result, Size);
  end;

TrailingPathDelimiter:
{$ifdef DELPHI6}
  Result := IncludeTrailingPathDelimiter(Result);
{$else}
  Result := IncludeTrailingBackSlash(Result);
{$endif}
end;  { TempDirName }





// =============================================================================
// 'TVirtualMemoryStream' public methods
// =============================================================================

const
  MemoryDelta = $2000; { Must be a power of 2 }

// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 14:15:47.<P>
}
destructor TDBIVirtualMemoryStream.Destroy;
begin
  Clear;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 11:33:15.<P>
}
procedure TDBIVirtualMemoryStream.Clear;
begin
  SetCapacity(0);
  InternalSetPosition(0);
  InternalSetSize(0);

  FMode := vsUnknown;
end;  { Clear }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 11:47:07.<P>
}
procedure TDBIVirtualMemoryStream.LoadFromStream(Stream: TStream);
var
  Count: Longint;

begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);

  if (Count <> 0) then begin
    Stream.ReadBuffer(Memory^, Count);
  end;
end;  { LoadFromStream }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 11:48:00.<P>
}
procedure TDBIVirtualMemoryStream.LoadFromFile(const FileName: String);
var
  Stream: TStream;

begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;  { LoadFromFile }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 11:49:03.<P>
}
procedure TDBIVirtualMemoryStream.SetSize(NewSize: Longint);
var
  OldPosition: Longint;

begin
  OldPosition := Position;
  SetCapacity(NewSize);
  InternalSetSize(NewSize);

  if (OldPosition > NewSize) then begin
    Seek(0, soFromEnd);
  end;
end;  { SetSize }





// =============================================================================
// 'TVirtualMemoryStream' procedure methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 09/12/2002 22:48:59.<P>
}
function TDBIVirtualMemoryStream.GetFileName: String;
begin
  Result := FVirtualMemory.FileName;
end;  { GetFileName }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 15:01:04.<P>
}
function TDBIVirtualMemoryStream.GetMode(const Bytes: LongInt): TVirtualMemoryStreamMode;
begin
  if (FMode = vsUnknown)then begin
    if (Bytes > GetVirtualMemoryThreshhold) then begin
      FMode := vsVirtual;
    end
    else begin
      FMode := vsStandard;
    end;
  end;

  Result := FMode;
end;  { GetMode }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 15:04:58.<P>
}
function TDBIVirtualMemoryStream.GetVirtualMemoryThreshhold: LongInt;
begin
  Result := $1000000; { = 16,777,216 }
end;  { GetVirtualMemoryThreshhold }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 16:42:26.<P>
}
procedure TDBIVirtualMemoryStream.InternalSetPosition(const NewPosition: Longint);
begin
  Seek(NewPosition, soFromBeginning);
end;  { InternalSetPosition }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 16:36:40.<P>
}
procedure TDBIVirtualMemoryStream.InternalSetSize(const NewSize: Integer);
begin
  SetPointer(Memory, NewSize);
end;  { InternalSetSize }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 11:48:36.<P>
}
procedure TDBIVirtualMemoryStream.SetCapacity(NewCapacity: Longint);
begin
  if (GetMode(NewCapacity) = vsStandard) then begin
    SetPointer(Realloc(NewCapacity), Size);
  end
  else begin
    SetPointer(VirtualRealloc(NewCapacity), Size);
  end;

  if (NewCapacity = 0) then begin
    FMode := vsUnknown;
  end;

  FCapacity := NewCapacity;
end;  { SetCapacity }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 12:40:27.<P>
}
function TDBIVirtualMemoryStream.VirtualRealloc(var NewCapacity: Longint): Pointer;
begin
  if (NewCapacity > 0) then begin
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  end;

  Result := Memory;
  if (NewCapacity <> FCapacity) then begin
    if (NewCapacity = 0) then begin
      FVirtualMemory.Free;
      FVirtualMemory := nil;

      Result := nil;
    end
    else begin
      if (Capacity = 0) then begin
        Result := TDBIMemoryMappedFile.VirtualAllocPtr(FVirtualMemory, NewCapacity);
      end
      else begin
        Result := TDBIMemoryMappedFile.VirtualReallocPtr(FVirtualMemory, NewCapacity);
      end;

      if (Result = nil) then begin
        raise EStreamError.CreateRes(@SMemoryStreamError);
      end;
    end;
  end;
end;  { Realloc }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 11:49:52.<P>
}
function TDBIVirtualMemoryStream.Realloc(var NewCapacity: Longint): Pointer;
begin
  if (NewCapacity > 0) then begin
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  end;

  Result := Memory;
  if (NewCapacity <> FCapacity) then begin
    if (NewCapacity = 0) then begin
      GlobalFreePtr(Memory);
      Result := nil;
    end
    else begin
      if (Capacity = 0) then begin
        Result := GlobalAllocPtr(HeapAllocFlags, NewCapacity);
      end
      else begin
        Result := GlobalReallocPtr(Memory, NewCapacity, HeapAllocFlags);
      end;

      if (Result = nil) then begin
        raise EStreamError.CreateRes(@SMemoryStreamError);
      end;
    end;
  end;
end;  { Realloc }


// _____________________________________________________________________________
{**
  Jvr - 12/11/2002 11:56:48.<P>
}
type
{$ifdef Delphi2009}
  PRawBuffer = PByte;
{$else}
  PRawBuffer = PAnsiChar;
{$endif}

function TDBIVirtualMemoryStream.Write(const Buffer; Count: Longint): Longint;
var
  Pos: Longint;

begin
  if (Position >= 0) and (Count >= 0) then begin
    Pos := Position + Count;
    if (Pos > 0) then begin
      if (Pos > Size) then begin
        if (Pos > FCapacity) then begin
          SetCapacity(Pos);
        end;

        InternalSetSize(Pos);
      end;

      System.Move(Buffer, (PRawBuffer(Memory) + Position)^, Count);

      InternalSetPosition(Pos);
      Result := Count;
      Exit;
    end;
  end;

  Result := 0;
end;  { Write }





end.
