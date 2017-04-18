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
  1.0 | 08/02/2001 17:56:57 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIXbaseBlobConnections;

{$I DBICompilers.inc}

interface

uses
  Classes, DB, DBIConst, DBIUtils, DBIXBaseConsts, DBIFileStreams;

const
  DefaultFoxproBlobFileFirstBlock = 8;   //* Blocks
  DefaultFoxproBlobFileBlockSize = 64;   //* Bytes
  DefaultFoxproBlobFileHeaderSize = 512; //* Bytes

  DefaultDBaseBlobFileFirstBlock = 1;    //* Blocks
  DefaultDBaseBlobFileBlockSize = 512;   //* Bytes
  DefaultDBaseBlobFileHeaderSize = 512;  //* Bytes

const
  // dBase
  dBaseFieldTerminator = #26;            //* Char = SUB ($1A / 26)


type
  //*  Pointer to dBase-III Header Record
  PDBIDBase3BlobHeader = ^TDBIDBase3BlobHeader;

  //*  dBase-III Header Record              <pre>
  TDBIDBase3BlobHeader = packed record
                                       //*  Bytes  Size  | Xbase-Version
    NextFreeBlock: LongWord;           //*  00- 03 [04]  | D3
    Reserved1: array[4..15] of Byte;   //*  04- 15 [12]  | D3
    Version: Byte;                     //*  16     [01]  | D3 ==> 03h
    Reserved2: array[17..511] of Byte; //*  17-511 [495] | D3
  end;  { TDBIDBase3BlobHeader }       //*  </pre>


  {**  DBase IV Header Record  *}
  PDBIDBase4BlobHeader = ^TDBIDBase4BlobHeader;
  TDBIDBase4BlobHeader = packed record
    NextFreeBlock: LongWord;           //*   0-  3 [04]  | D4
    BlockSize: LongWord;               //*   4-  7 [04]  | D4
    szDBFName: array[8..15] of Char;   //*   8- 15 [12]  | D4
    Reserved1: Byte;                   //*  16     [01]  | D4 ==> 00h
    Reserved2: array[17..19] of Byte;  //*  17- 19 [03]  | D4
    BlockLength: Word;                 //*  20- 21 [02]  | D4
    Reserved3: array[22..511] of Byte; //*  22-511 [490] | D4
  end;  { TDBIDBase4BlobHeader }


  {**  DBase IV BlobField Record  *}
  PDBIDBase4BlobField = ^TDBIDBase4BlobField;
  TDBIDBase4BlobField = packed record  //*                       Used block Sign
    NextFreeBlock: LongWord;           //*   0-  3 [04] | D4 ==> FFh FFh 08h 00h
    Size: LongWord;                    //*   4-  7 [02] | D4
    Data: array[0..MaxInt-9] of Byte;  //*   8-  n [02] | D4 ==> 2*1Ah EndOfField
  end;  { TDBIDBase4BlobField }



  {**  FoxPro Header Record  **}
  PDBIFoxProBlobHeader = ^TDBIFoxProBlobHeader;
  TDBIFoxProBlobHeader = packed record
    NextFreeBlock: LongWord;           //*   0-  3 [04]  | FB  FP
    Reserved1: Word;                   //*   4-  5 [02]  | FB  FP
    BlockSize: Word;                   //*   6-  7 [02]  | FB  FP
    Reserved2: array[8..511] of Byte   //*   8-511 [504] | FB  FP
  end;  { TFoxProDBIBlobHeader }

  {**  FoxPro Infomation Record - used  **}
  PDBIFoxProBlobInfo = ^TDBIFoxProBlobInfo;
  TDBIFoxProBlobInfo = packed record
    Kind: LongWord;                    //*   0-  3 [04] | FB  FP
    Size: LongWord;                    //*   4-  7 [02] | FB  FP
  end;  { TFoxProDBIBlobInfo }

  {**  FoxPro BlobField Record  **}
  PDBIFoxProBlobField = ^TDBIFoxProBlobField;
  TDBIFoxProBlobField = packed record
    Kind: LongWord;                    //*   0-  3 [04] | FB  FP
    Size: LongWord;                    //*   4-  7 [02] | FB  FP
    Data: array[0..MaxInt-9] of Byte;  //*   8-  n [02] | FB  FP (n = Size)
  end;  { TFoxProDBIBlobField }


type
  TDBIXbaseCustomBlobConnection = class(TPersistent)
  private
    FReadonly: Boolean;
    FExclusive: Boolean;
    FFileName: String;
    FStreamMode: TDBIStreamMode;
    FModified: Boolean;
    FDirty: Boolean;
    FVersion: TDBIXbaseVersion;

    FBlobStream: TStream;         //* The Stream to access the Memo Stream/File
    PHeader: Pointer;             //* Header Data for Blob file

  protected
    procedure CreateBlobStream(const New: Boolean);

    function GetAttributes: TFieldAttributes;

    function GetHeaderBuf: Pointer; virtual;
    function GetHeaderSize: Integer; virtual; abstract;

    function GetNextFreeBlock: LongWord; virtual; abstract;
    procedure SetNextFreeBlock(const Value: LongWord); virtual; abstract;

    function GetBlockSize: Word; virtual; abstract;
    procedure SetBlockSize(const Value: Word); virtual; abstract;

    function GetFileMode: Word;
    procedure DefaultMetaData; virtual;
    procedure ReadMetaData; virtual;
    procedure WriteMetaData; virtual;

    property NextFreeBlock: LongWord read GetNextFreeBlock write SetNextFreeBlock;
    property BlockSize: Word read GetBlockSize write SetBlockSize;
    property FileMode: Word read GetFileMode;
    property Stream: TStream read FBlobStream;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CloseBlobStream;
    procedure OpenBlobStream;
    procedure NewBlobStream;
    procedure SaveToFile(AFileName: String);

    // Blob methods
    procedure GetBlob(
      const Position: Int64;
      const OffSet: LongWord;
      PData: Pointer;
      out Size: LongWord
      ); virtual; abstract;

    function PutBlob(
      const Position: Int64;
      const OffSet: LongWord;
      PData: Pointer;
      const Size: LongWord
      ): Int64; virtual; abstract;

  protected
    function WriteBlob(
      const Position: Int64;
      const BlockCount: LongWord;
      const Size: LongWord;
      PData: Pointer
      ): Int64; virtual; abstract;

    procedure SaveToStream(AStream: TStream);

  public
    property Attributes: TFieldAttributes read GetAttributes;
    property Exclusive: Boolean read FExclusive write FExclusive;
    property StreamMode: TDBIStreamMode read FStreamMode write FStreamMode;
    property FileName: String read FFileName write FFileName;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Version: TDBIXbaseVersion read FVersion write FVersion;
  end;


  TDBIDbaseBlobConnection = class(TDBIXbaseCustomBlobConnection)
  protected
    procedure DefaultMetaData; override;

    function GetHeaderSize: Integer; override;
    function GetBlobSize(const Position: Int64): Int64;

    function GetNextFreeBlock: LongWord; override;
    procedure SetNextFreeBlock(const Value: LongWord); override;

    function GetBlockSize: Word; override;
    procedure SetBlockSize(const Value: Word); override;

  public
    // Blob methods
    procedure GetBlob(
      const Position: Int64;
      const OffSet: LongWord;
      PData: Pointer;
      out Size: LongWord
      ); override;

    function PutBlob(
      const Position: Int64;
      const OffSet: LongWord;
      PData: Pointer;
      const Size: LongWord
      ): Int64; override;

  protected
    function WriteBlob(
      const Position: Int64;
      const BlockCount: LongWord;
      const Size: LongWord;
      PData: Pointer
      ): Int64; override;

  end;  { TDBIDbaseBlobConnection }



  TDBIFoxProBlobConnection = class(TDBIXbaseCustomBlobConnection)
  protected
    procedure DefaultMetaData; override;
    function GetHeaderSize: Integer; override;

    function GetNextFreeBlock: LongWord; override;
    procedure SetNextFreeBlock(const Value: LongWord); override;

    function GetBlockSize: Word; override;
    procedure SetBlockSize(const Value: Word); override;

  public
    // Blob methods
    procedure GetBlob(
      const Position: Int64;
      const OffSet: LongWord;
      PData: Pointer;
      out Size: LongWord
      ); override;

    function PutBlob(
      const Position: Int64;
      const OffSet: LongWord;
      PData: Pointer;
      const Size: LongWord
      ): Int64; override;

  protected
    function WriteBlob(
      const Position: Int64;
      const BlockCount: LongWord;
      const Size: LongWord;
      PData: Pointer
      ): Int64; override;

  end;  { TDBIFoxProBlobConnection }



implementation

uses
  SysUtils, Math;


{ TDBIDbaseBlobConnection }

// _____________________________________________________________________________
{**
  Jvr - 10/05/2005 14:11:52 - Initial code.<br>
}
procedure TDBIDbaseBlobConnection.DefaultMetaData;
begin
  inherited DefaultMetaData;

  NextFreeBlock := DefaultDBaseBlobFileFirstBlock;
  BlockSize := DefaultDBaseBlobFileBlockSize;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 17:23:21.<P>
}
procedure TDBIDbaseBlobConnection.GetBlob(
  const Position: Int64;
  const OffSet: LongWord;
  PData: Pointer;
  out Size: LongWord
  );
var
  BlobInfo: TDBIFoxProBlobInfo;

begin
  Assert(Assigned(FBlobStream));
  try
    // Get the BlobSize
    BlobInfo.Size := GetBlobSize(Position);

    // If PData parameter is nil, then only return the Size value
    // indicating the size of the data
    if (PData = nil) {##JVR or (Size = 0)} or (BlobInfo.Size = 0) then begin
      Size := BlobInfo.Size;
      Exit;
    end;

    // Return the appropriate Size value
    Size := Min(BlobInfo.Size, Size);

    // No need to reposition, Getting the Blob Size already has done that.
    FBlobStream.Read(PData^, Size);

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'GetBlob::325', 'Unable to Get Blob Data', []);
  end;
end;  { GetBlob }


// _____________________________________________________________________________
{**
  Jvr - 30/12/2004 14:40:56 - Initial code.<br>

  <b>NOTE:</b><br>
  Getting the Blob size should also position the BlobStream at the
  first blob data byte.
}
function TDBIDbaseBlobConnection.GetBlobSize(const Position: Int64): Int64;
var
  PBlobInfo: PChar;
  PBlobIterator: PChar;
  BytesRead: Integer;
  BlockCount: Int64;
  BlockIndex: Integer;

begin
  BlockCount := 0;
  Result := 0;

  GetMem(PBlobInfo, BlockSize);
  try
    FBlobStream.Position := BlockSize * Position;

    repeat
      BytesRead := FBlobStream.Read(PBlobInfo^, BlockSize);
      PBlobIterator := PBlobInfo;

      for BlockIndex := 0 to BytesRead-1 do begin
         // if char = SUB ($1A / 26) then we have reached the end of the memo
        if (PBlobIterator^ = dBaseFieldTerminator) then begin
          Result := (BlockCount * BlockSize) + BlockIndex;
          Break;
        end;

        // Next Character
        Inc(PBlobIterator);
      end;  { for }

      // Another Block
      Inc(BlockCount);
    until (Result > 0) or (BytesRead <> BlockSize);

    // Reposition BlobStream ready for reading the Blob
    FBlobStream.Position := BlockSize * Position;
  finally
    FreeMem(PBlobInfo);
  end;
end;  { GetBlobSize }


// _____________________________________________________________________________
{**
  This is fixed at 512 Bytes for DBase3+

  Jvr - 29/12/2004 18:48:13 - Initial code.<br>
}
function TDBIDbaseBlobConnection.GetBlockSize: Word;
begin
  Result := DefaultDBaseBlobFileBlockSize;
end;  { GetNextFreeBlock }


// _____________________________________________________________________________
{**
  Jvr - 29/12/2004 18:26:37 - Initial code.<br>
}
function TDBIDbaseBlobConnection.GetHeaderSize: Integer;
begin
  Result := SizeOf(TDBIDbase3BlobHeader);
end;  { GetHeaderSize }


// _____________________________________________________________________________
{**
  Jvr - 29/12/2004 18:42:28 - Initial code.<br>
}
function TDBIDbaseBlobConnection.GetNextFreeBlock: LongWord;
begin
  Result := PDBIDbase3BlobHeader(PHeader)^.NextFreeBlock;
end;  { GetNextFreeBlock }


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 17:24:23.<P>

  If PData is nil then the existing blob will be cleared.<P>
  If Position is Zero then we are adding a new Blob.<P>

  @Return    The new offset value for the just written Blob
}
function TDBIDbaseBlobConnection.PutBlob(
  const Position: Int64;
  const OffSet: LongWord;
  PData: Pointer;
  const Size: LongWord
  ): Int64;
var
  BlobInfo: TDBIFoxProBlobInfo;
  OldBlockCount: LongWord;
  NewBlockCount: LongWord;

begin
  OldBlockCount := 0; // No Data;

  Assert(Assigned(FBlobStream));
  try
    // If its not a New Blob
    if (Position <> 0) then begin
      BlobInfo.Size := GetBlobSize(Position);

      // Calculate the Old block count
      OldBlockCount := ((BlobInfo.Size + SizeOf(BlobInfo)) div BlockSize) + 1;
    end;

    // Calculate the New Block Count
    NewBlockCount := ((Size + SizeOf(BlobInfo)) div BlockSize) + 1;


    // New Block Count is Larger, or its a new Blob -> Append Memo to the end
    if (NewBlockCount > OldBlockCount) or (Position = 0) then begin
      { TODO 3 -oJvr -cTDBIDbaseBlobConnection.PutBlob() :
        We need to check if there are any free blocks immediately following the
        existing data blocks (creating a contiguous block)
        that have been marked as free (cleared)
        before deciding to append the data to the end of the file
      }
      Assert(NewBlockCount > 0);

      // Clear the old data first, only if it's NOT a new Blob - Returns 0 (Clear only)
      if (Position <> 0) then begin
        Result := WriteBlob(Position, OldBlockCount, 0, nil);
        if (Result <> 0) then begin
          raise EDBIException.Create(Self, 'PutBlob::465', 'Unable to clear Blob Data', []);
        end;
      end;

      // Now write the new data - Return the position of the blob written
      Result := WriteBlob(NextFreeBlock, NewBlockCount, Size, PData);
      if (Result > 0) then begin
        // Now update the dbt file header
        NextFreeBlock := NextFreeBlock + NewBlockCount;
        FDirty := True;
        WriteMetaData;
      end
      else begin
        raise EDBIException.Create(Self, 'PutBlob::475', 'Unable to write Blob Data', []);
      end;
    end

    // Data can fit into existing space, but needs the remaining blocks cleared
    else if (NewBlockCount < OldBlockCount) then begin
      Result := WriteBlob(Position, NewBlockCount, Size, PData);

      { TODO 3 -oJvr -cTDBIDbaseBlobConnection.PutBlob() :
        Clear the remaining blocks not used
      }
    end

    // Data can fit into existing space
    else begin
      // If PData is nil then the Blob will be Cleared
      Result := WriteBlob(Position, NewBlockCount, Size, PData);
    end;

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'PutBlob::500', 'Unable to write Blob Data', []);
  end;
end;  { PutBlob }


// _____________________________________________________________________________
{**
  Jvr - 29/12/2004 18:44:13 - Initial code.<br>
}
procedure TDBIDbaseBlobConnection.SetNextFreeBlock(const Value: LongWord);
begin
  PDBIDbase3BlobHeader(PHeader)^.NextFreeBlock := Value;
end;  { SetNextFreeBlock }


// _____________________________________________________________________________
{**
  Jvr - 29/12/2004 18:44:13 - Initial code.<br>
}
procedure TDBIDbaseBlobConnection.SetBlockSize(const Value: Word);
begin
  // NOP - Do nothing the header no blocksize information
end;  { SetBlockSize }


// _____________________________________________________________________________
{**
  Jvr - 15/02/2001 17:38:48 - Write Blob data to the BlobStream.<P>

  If PData is nil then clear the Block(s) specified at 'Position' specified
}
function TDBIDbaseBlobConnection.WriteBlob(
  const Position: Int64;
  const BlockCount: LongWord;
  const Size: LongWord;
  PData: Pointer
  ): Int64;
var
  PBlob: PDBIFoxProBlobField;

begin
  Result := 0;                         // Memo is Blank

  try
    GetMem(PBlob, BlockCount * BlockSize);
    try
      // Clear Allocated Memory
      FillChar(PBlob^, BlockCount * BlockSize, #0);

      // Write Data if the Data Pointer is referencing an address (not nil)
      if Assigned(PData) then begin
        Move(PData^, PBlob^.Data, Size);

        // Add Field Terminating Markers 2x
        PBlob^.Data[Size] := Byte(dbaseFieldTerminator);
        PBlob^.Data[Size+1] := Byte(dbaseFieldTerminator);

        Result := Position;            // Memo has data, is NOT blank
      end;

      { TODO 5 -ojvr -cDBIXBaseBlobConnections.WriteBlob() :
        This needs attention fairly urgently!!!

        This needs fixing I need to use a different structure for dbase Memos
        At the moment the last eight bytes are garbage because I am writing the Data field
        as if it were the full 512 bytes, thus minus the kind and size fields!!!!
      }
      FBlobStream.Position := BlockSize * Position;
      FBlobStream.Write(PBlob^.Data,  BlockCount * BlockSize);
      FModified := True;

    finally
      FreeMem(PBlob);
    end;
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'WriteBlob::575',
        'Unable to write "%d" bytes ("%d blocks) at position "%d" to file "%s"',
        [Size, BlockCount, Position, FFileName]
        );
  end;
end;  { WriteBlob }





{ TDBIFoxProBlobConnection }

// _____________________________________________________________________________
{**
  Jvr - 10/05/2005 14:20:55 - Initial code.<br>
}
procedure TDBIFoxProBlobConnection.DefaultMetaData;
begin
  inherited DefaultMetaData;

  NextFreeBlock := DefaultFoxproBlobFileFirstBlock;
  BlockSize := DefaultFoxproBlobFileBlockSize;
end;  { DefaultMetaData }


// _____________________________________________________________________________
{**
  Jvr - 10/05/2005 14:04:22 - Initial code.<br>
}
function TDBIFoxProBlobConnection.GetBlockSize: Word;
begin
  Result := Swap(PDBIFoxProBlobHeader(GetHeaderBuf)^.BlockSize);
end;

function TDBIFoxProBlobConnection.GetHeaderSize: Integer;
begin
  Result := SizeOf(TDBIFoxProBlobHeader);
end;

function TDBIFoxProBlobConnection.GetNextFreeBlock: LongWord;
begin
  Result := SwapDWord(PDBIFoxProBlobHeader(GetHeaderBuf)^.NextFreeBlock);
end;

procedure TDBIFoxProBlobConnection.SetBlockSize(const Value: Word);
begin
  PDBIFoxProBlobHeader(GetHeaderBuf)^.BlockSize := Swap(Value);
end;

procedure TDBIFoxProBlobConnection.SetNextFreeBlock(const Value: LongWord);
begin
  PDBIFoxProBlobHeader(GetHeaderBuf)^.NextFreeBlock := SwapDWord(Value);
end;


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 17:23:21.<P>
}
procedure TDBIFoxProBlobConnection.GetBlob(
  const Position: Int64;
  const OffSet: LongWord;
  PData: Pointer;
  out Size: LongWord
  );
var
  BlobInfo: TDBIFoxProBlobInfo;

begin
  Assert(Assigned(FBlobStream));
  try
    // Get the BlobSize
    FBlobStream.Position := BlockSize * Position;
    FBlobStream.Read(BlobInfo{%H-}, SizeOf(BlobInfo));
    BlobInfo.Size := SwapDWord(BlobInfo.Size);

    // If PData parameter is nil, then only return the Size value
    // indicating the size of the data
    if (PData = nil) or (BlobInfo.Size = 0) then begin
      Size := BlobInfo.Size;
      Exit;
    end;

    // Return the appropriate Size value
    Size := Min(BlobInfo.Size, Size);

    // No need to reposition, Getting the Blob Size already has done that.
    FBlobStream.Read(PData^, Size);

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'GetBlob::665', 'Unable to Get Blob Data', []);
  end;
end;  { GetBlob }


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 17:24:23.<P>

  If PData is nil then the existing blob will be cleared.<P>
  If Position is Zero then we are adding a new Blob.<P>

  @Return    The new offset value for the just written Blob
}
function TDBIFoxProBlobConnection.PutBlob(
  const Position: Int64;
  const OffSet: LongWord;
  PData: Pointer;
  const Size: LongWord
  ): Int64;
var
  BlobInfo: TDBIFoxProBlobInfo;
  OldBlockCount: LongWord;
  NewBlockCount: LongWord;

begin
  OldBlockCount := 0; // No Data

  Assert(Assigned(FBlobStream));
  try
    // If its not a New Blob
    if (Position <> 0) then begin
      // Get Current Blob Metadata
      FBlobStream.Position := BlockSize * Position;
      FBlobStream.Read(BlobInfo{%H-}, SizeOf(BlobInfo));
      BlobInfo.Kind := SwapDWord(BlobInfo.Kind);
      BlobInfo.Size := SwapDWord(BlobInfo.Size);

      // Calculate the Old block count
      OldBlockCount := ((BlobInfo.Size + SizeOf(BlobInfo)) div BlockSize) + 1;
    end;

    // Calculate the New Block Count
    NewBlockCount := ((Size + SizeOf(BlobInfo)) div BlockSize) + 1;


    // New Block Count is Larger, or its a new Blob -> Append Memo to the end
    if (NewBlockCount > OldBlockCount) or (Position = 0) then begin
      { TODO 3 -oJvr -cTDBIFoxProBlobConnection.PutBlob() :
        We need to check if there are any free blocks immediately following the
        existing data blocks (creating a contiguous block)
        that have been marked as free (cleared)
        before deciding to append the data to the end of the file
      }
      Assert(NewBlockCount > 0);

      // Clear the old data first, only if it's NOT a new Blob - Returns 0 (Clear only)
      if (Position <> 0) then begin
        Result := WriteBlob(Position, OldBlockCount, 0, nil);
        if (Result <> 0) then begin
          raise EDBIException.Create(Self, 'PutBlob::730', 'Unable to Clear Blob Data', []);
        end;
      end;

      // Now write the new data - Return the position of the blob written
      Result := WriteBlob(NextFreeBlock, NewBlockCount, Size, PData);
      if (Result > 0) then begin
        // Now update the fpt file header
        NextFreeBlock := NextFreeBlock + NewBlockCount;
        FDirty := True;
        WriteMetaData;
      end
      else if (Size > 0) then begin
        raise EDBIException.Create(Self, 'PutBlob::740', 'Unable to write Blob Data', []);
      end;
    end

    // Data can fit into existing space, but needs the remaining blocks cleared
    else if (NewBlockCount < OldBlockCount) then begin
      Result := WriteBlob(Position, NewBlockCount, Size, PData);

      { TODO 3 -oJvr -cTDBIFoxProBlobConnection.PutBlob() :
        Clear the remaining blocks not used
      }
    end

    // Data can fit into existing space
    else begin
      // If PData is nil then the Blob will be Cleared
      Result := WriteBlob(Position, NewBlockCount, Size, PData);
    end;

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'PutBlob::765', 'Unable to write Blob Data', []);
  end;
end;  { PutBlob }


// _____________________________________________________________________________
{**
  Jvr - 15/02/2001 17:38:48 - Write Blob data to the BlobStream.<P>

  If PData is nil then clear the Block(s) specified at 'Position' specified
}
function TDBIFoxProBlobConnection.WriteBlob(
  const Position: Int64;
  const BlockCount: LongWord;
  const Size: LongWord;
  PData: Pointer
  ): Int64;
const
  // TFoxProDBIBlobKind = (FoxProBlobImage, FoxProBlobMemo, FoxProBlobObject);
  FoxProBlobMemo = 1;

var
  PBlob: PDBIFoxProBlobField;

begin
  Result := 0;                         // Memo is Blank

  try
    GetMem(PBlob, BlockCount * BlockSize);
    try
      FillChar(PBlob^, BlockCount * BlockSize, #0);

      if Assigned(PData) then begin
        { TODO 3 -oJvr -cTDBIFoxProBlobConnection.WriteBlob() :
          FoxProBlobMemo is now a constant, but will in the future need to be
          determined by the information coming from the datafile metadata
        }
        PBlob^.Kind := SwapDWord(FoxProBlobMemo);
        PBlob^.Size := SwapDWord(Size);

        Move(PData^, PBlob^.Data, Size);
        Result := Position;            // Memo has data, is NOT blank
      end;

      FBlobStream.Position := BlockSize * Position;
      FBlobStream.Write(PBlob^,  BlockCount * BlockSize);
      FModified := True;

    finally
      FreeMem(PBlob);
    end;
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'WriteBlob::815',
        'Unable to write "%d" bytes ("%d blocks) at position "%d" to file "%s"',
        [Size, BlockCount, Position, FFileName]
        );
  end;
end;  { WriteBlob }





{ TDBIXbaseCustomBlobConnection }

// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 12:40:52 - Initial code.<br>
  Jvr - 29/12/2004 13:32:48 - Refactored to allow multiple data formats.<br>
}
procedure TDBIXbaseCustomBlobConnection.CloseBlobStream;
begin
  WriteMetaData;

  if (StreamMode = smFileStream) then begin
    FBlobStream.Free;
    FBlobStream := nil;
  end;
end;  { CloseBlobStream }


// _____________________________________________________________________________
{**
  <p>Constructor for the Blob connection XBase class.</p>

  Jvr - 09/02/2001 18:16:29 - Initial code.<br>
  Jvr - 29/12/2004 13:32:18 - Refactored to allow multiple data formats.<br>
}
constructor TDBIXbaseCustomBlobConnection.Create;
begin
  inherited Create;

  FExclusive := False;
  FReadOnly := False;
  FModified := False;
  FDirty := False;
  FStreamMode := smMemoryStream;
  FBlobStream := nil;

  // Blob Header Data
  GetMem(PHeader, GetHeaderSize);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 18:52:59 - Initial code.<br>
}
procedure TDBIXbaseCustomBlobConnection.CreateBlobStream(const New: Boolean);
var
  Mode: Word;

begin
  // Don't free the stream if it is an externally assigned stream
  if (StreamMode <> smExternalStream) then begin
    FBlobStream.Free;
    FBlobStream := nil;
  end;

  Mode := OpenModes[ReadOnly] or ShareModes[Exclusive] or CreateModes[New];

  FBlobStream := TDBIFileStream.CreateStream(FFileName, Mode, FStreamMode);
  ReadOnly := (Mode and fmOpenReadWrite) = fmOpenReadWrite;

  FModified := False;
  Assert(Assigned(FBlobStream));
end;


// _____________________________________________________________________________
{**
  Jvr - 10/05/2005 14:09:30 - Initial code.<br>
}
procedure TDBIXbaseCustomBlobConnection.DefaultMetaData;
begin
  // Clear Header Data;
  FillChar(GetHeaderBuf^, GetHeaderSize, #0);
end;  { DefaultMetaData }


// _____________________________________________________________________________
{**
  Jvr - 29/12/2004 13:31:23 - Refactored to allow multiple data formats.<br>
}
destructor TDBIXbaseCustomBlobConnection.Destroy;
begin
  CloseBlobStream;

  if (StreamMode <> smExternalStream) then begin
    FBlobStream.Free;
    FBlobStream := nil;
  end;

  FreeMem(PHeader);
  PHeader := nil;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  This works but I'm not sure if this is the way I want to do it
  It probably isn't an issue.

  Jvr - 06/10/2005 13:27:38 - Initial code.<br>
}
function TDBIXbaseCustomBlobConnection.GetAttributes: TFieldAttributes;
const
  ReadOnlyAttrMap: array[Boolean] of TFieldAttributes = ([], [db.faReadOnly]);

begin
  Assert(Assigned(Self));

  Result := ReadOnlyAttrMap[(FBlobStream is THandleStream) and FileIsReadOnly(FFilename)];
end;  { GetFieldAttributes }


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 17:53:34 - Initial code.<br>
}
function TDBIXbaseCustomBlobConnection.GetFileMode: Word;
begin
  Result := OpenModes[ReadOnly] or ShareModes[Exclusive];
end;  { GetFileMode }


// _____________________________________________________________________________
{**
  Jvr - 30/12/2004 13:05:09 - Initial code.<br>
}
function TDBIXbaseCustomBlobConnection.GetHeaderBuf: Pointer;
begin
  Result := PHeader;
end;  { GetHeaderBuf }


// _____________________________________________________________________________
{**
  Jvr - 16/02/2001 12:15:22 - Initial code.<br>
  Jvr - 29/10/2002 14:06:41 - Readonly file support added.<br>
  Jvr - 29/12/2004 11:35:00 - Refactored to allow multiple data formats.<br>
}
procedure TDBIXbaseCustomBlobConnection.NewBlobStream;
begin
  CreateBlobStream(True);

  // Write the initial Metadata
  DefaultMetaData;
  FDirty := True;
  FModified := False;
  WriteMetaData;
end;  { NewBlobStream }


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 12:35:38 - Currently this implementation only supports
                              file based memos<BR>
  Jvr - 29/10/2002 14:06:06 - Readonly file support added<P>
  Jvr - 29/12/2004 13:33:04 - Refactored to allow multiple data formats.<br>
}
procedure TDBIXbaseCustomBlobConnection.OpenBlobStream;
begin
  if FileExists(FFileName) then begin
    CreateBlobStream(False);
    ReadMetaData;
  end;
end;  { OpenBlobStream }


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 16:37:49 - Initial code.<br>
}
procedure TDBIXbaseCustomBlobConnection.ReadMetaData;
begin
  Assert(Assigned(FBlobStream));

  try
    FBlobStream.Seek(0, soFromBeginning);
    FBlobStream.Read(GetHeaderBuf^, GetHeaderSize);
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'ReadMetaData::1010', 'Unable to Read Blob MetaData', []);
  end;
end;  { ReadMetaData }


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 12:40:46 - Initial code.<br>
  Jvr - 29/12/2004 11:34:16 - Refactored to allow multiple data formats.<p>
}
procedure TDBIXbaseCustomBlobConnection.SaveToFile(AFileName: String);
begin
  if (AFileName = '') then begin
    AFileName := FFileName;
  end;

  if (AFileName = '') then begin
    raise EDBIException.Create(Self, 'SaveToFile::1025', 'Invalid FileName', []);
  end;

  TDBIFileStream.SaveStreamToFile(FBlobStream, AFileName);
  FModified := False;
end;  { SaveToFile }


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 17:45:48 - Initial code.<br>
}
procedure TDBIXbaseCustomBlobConnection.SaveToStream(AStream: TStream);
begin
  raise EDBINotImplementedException.Create(Self, 'SaveToStream::1040');
end;  { SaveToStream }


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 16:39:14.<P>
}
procedure TDBIXbaseCustomBlobConnection.WriteMetaData;
begin
  if not FDirty then begin
    Exit;
  end;

  Assert(Assigned(FBlobStream));
  try
    // Write the header to the stream
    FBlobStream.Seek(0, soFromBeginning);
    FBlobStream.Write(GetHeaderBuf^, GetHeaderSize);
    FDirty := False;
    FModified := True;

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'WriteMetaData::1065',
        'Unable to write "%d" bytes of MetaData to file "%s"',
        [GetHeaderSize, FFileName]
        );
  end;
end;  { WriteMetaData }

end.
