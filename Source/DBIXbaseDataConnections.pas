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

  Notes:         SaveToFile
                   [bForceSave = True] - Write changes even if not modified

  Change History:
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 27/01/1999 11:55:57 | Jvr | Initial Release
  1.1 | 23/03/1999 11:05:00 | Jvr | TDBIStream now has a Modified property
  1.2 | 23/03/1999 12:53:00 | Jvr | TStreamDBInterface now has a IsModified method
  1.3 | 02/11/2000 14:24:39 | Jvr | Refactored (Major)
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIXbaseDataConnections;

interface

{$I DBICompilers.inc}

{$define Use_BufferedStreams 1}

{$ifdef fpc}
  {$packrecords 1}
  {$packenum 1}
  {$packset 1}
{$endif}

uses
{$ifdef Delphi2009}
  AnsiStrings,
{$endif}
  Classes, SysUtils, Windows, DB, DBIStrings, DBIConst, DBIXbaseConsts,
{$ifndef fpc}
  DBCommon,
{$endif}
  DBIIntfConsts, DBIInterfaces, DBIUtils, DBIFileStreams, DBIXbaseBlobConnections,
  DBICustomStreamDataConnections;


type
  TDBIXbaseGetFieldProc = function (const RecordBuffer; var FieldBuffer: TDBIFieldBuffer; FieldNo: Word): Boolean of object;
  TDBIXbasePutFieldProc = procedure (var RecordBuffer; const FieldBuffer: TDBIFieldBuffer; FieldNo: Word) of object;


type
  // ___________________________________________________________________________
  {**
    Record to decode the Modification date in the Xbase Header
  }
  TDBIXbaseDateRec = packed record
    Year: Byte;
    Month: Byte;
    Day: Byte;
  end;


  // ___________________________________________________________________________
  {**
    <CODE>
    * (dBASE IV) Production index / Multiple index file
      01h MDX file present
      00h no MDX file (index upon demand).

    * (FoxBase)
      01h CDX compound index file present,
      00h no CDX file.

    * (Visual FoxPro)
      02h With memo
      04h Database Container (DBC)
      07h DBC (incl. memo & indexes)

    * (Extended Xbase - Jvr File format)
      08h Extended Field array available in header
    </CODE>
  }
  TDBIXbaseFlag = (
    xfCompoundIndex,
    xfMemo,
    xfDatabaseContainer,
    xfReserved3,
    xfReserved4,
    xfReserved5,
    xfReserved6,
    xfExtendedFields
  );
  TDBIXbaseFlags = set of TDBIXbaseFlag;

const
  TDBIXbaseInvalidFlags = [xfReserved3, xfReserved4, xfReserved5, xfReserved6];


  // ___________________________________________________________________________
  {**
    These following classes and its implementation will be moved to another
    file soon.!
    <P>
    <B>LEGEND:</B><BR>
    <CODE>
    FB = Foxbase for DOS
    FP = Foxpro for windows
    D3 = Dbase-III plus for DOS
    D4 = Dbase-IV 2.0 for DOS
    D5 = Dbase 5.0 for DOS
    W5 = Dbase 5.0 for Windows
    </CODE>
    <P>
    <B>NOTES:</B><BR>
    <CODE>
    DataOffset = [32]          | SizeOf(FHeader)
               + [32 * nFlds]  | SizeOf(TDBIXbaseField) * FieldCount
               + [264]         | I have no idea what the significance of 264 is

    RecordCount = number of physical records in the data file
                  marking records deleted, has no effect on this value
  </CODE>
  }
type
  TDBIXbaseHeader = packed record
    Version: Byte;                       // |  0    [01] | FB  FP  D3  D4  D5  W5
    Modified: TDBIXbaseDateRec;          // |  1- 3 [03] | FB  FP  D3  D4  D5  W5
    RecordCount: LongWord;               // |  4- 7 [04] | FB  FP  D3  D4  D5  W5
    DataOffset: Word;                    // |  8- 9 [02] | FB  FP  D3  D4  D5  W5
    RecordSize: Word;                    // | 10-11 [02] | FB  FP  D3  D4  D5  W5
    Reserved1: array[0..1] of Byte;      // | 12-13 [02] |
    Transaction: Byte;                   // | 14    [01] |             D4  D5
    Encrypted: Byte;                     // | 15    [01] |             D4  D5
    FreeRecThread: array[0..3] of Byte;  // | 16-19 [04] |             D4  D5
    Reserved2: array[0..7] of Byte;      // | 20-27 [08] |             D4  D5
    Flags: TDBIXbaseFlags;      {Byte}   // | 28    [01] |     FP      D4  D5  W5
    Language: TDBIXbaseCodePage;{Byte}   // | 29    [01] |     FP      D4  D5  W5
    Reserved3: array[0..1] of Byte;      // | 30-31 [02] |
  end;


  // ___________________________________________________________________________
  {**
    Byte 18 of a field record in the field array indicates the following:<br />

    <ul>
      <li>0x01 = Bit1: System Column (column is hidden to the user)</li>
      <li>0x02 = Bit2: Nullable, (column can store null values</li>
      <li>0x04 = Bit3: NOCPTRANS, (Binary column (for AnsiChar and Memo only)</li>
      <li>0x06 = Bit2+Bit3: (When a field is NULL and binary (Integer, Currency, and AnsiChar/Memo fields)</li>
      <li>0x0C = Bit4+Bit3: Column is Auto-Incrementing
    </ul><P>

    <b>Notes:</b><br />
    <ul>
      <li>
        The above attributes may be ORed together
      </li>

      <li>
        NOCPTRANS - Prevents translation to a different code page for
        fields with this attribute set  (MS-Visual foxpro only)
      </li>

      <li>
        This flag is only applicable to Character & Memo fields and is set in
        MS-Visual Foxpro by selecting the Binary fieldtypes of Character & Memo
      </li>

      <li>
        <B>Syntax: </B>
        SET NOTRANS TO [<I>FieldName1</I> [, <I>FieldName2</I> ...]]
      </li>
    </ul>
  }
type
  TDBIFieldFlag = (ffHiddenField, ffNullableField, ffNOCPTRANS);
  TDBIFieldFlags = set of TDBIFieldFlag;


  // ___________________________________________________________________________
  {**
    <B>NOTES:</B><BR>
    FieldOffset[0] is used by FoxPro<BR>
    FieldOffset[0..1] is used by Dbase-III Plus<BR>
  }
type
  PDBIXBaseField = ^TDBIXbaseField;
  TDBIXbaseField = packed record
    FieldName: array[0..10] of AnsiChar;   // |  0-10 [11] | FB  FP  D3  D4  D5  W5
    FieldType: AnsiChar;                   // | 11    [01] | FB  FP  D3  D4  D5  W5
    FieldOffset: LongWord;                 // | 12-15 [02] |     FP  D3
    FieldSize: array[0..1] of Byte;        // | 16-17 [02] | FB  FP  D3  D4  D5  W5
    FieldFlags: TDBIFieldFlags; {Byte}     // | 18    [01] |     FP
    Reserved1: Byte;                       // | 19    [01] |
    WorkAreaID: Byte;                      // | 20    [01] |         D3  D4  D5  W5
    Reserved2: array[0..1] of Byte;        // | 21-22 [02] |
    SetFields: Byte;                       // | 23    [01] |         D3
    FieldData: array[0..6] of AnsiChar;    // | 24-30 [07] | JVR !!!
    FieldInMdx: Byte;                      // | 31    [01] |         D3  D4  D5  W5
  end;
  TDBIXbaseFields = array of TDBIXbaseField;



  // ___________________________________________________________________________
  {**
    FoxPro physical Timestamp record (as stored in the data file)
  }
type
  TDBIXbaseFoxProTimeStamp = record
    Date: Integer;      { One plus number of days since 1/1/0001 }
    Time: Integer;      { Number of milliseconds since midnight }
  end;


  // ___________________________________________________________________________
  {**
    FoxPro general constants
  }
const
  XbaseFoxProDataOffset    = 264;
  XbaseFoxProDateOffset    = 1721425;
  XbaseFoxProMaxFieldCount = 256;


  // ___________________________________________________________________________
  {**
    FoxPro Y2K constants
  }
const
  XBaseFoxProStdYearOffset = 1900;
  XBaseFoxProY2KYearOffset = 2000;


  // ___________________________________________________________________________
  {**
    Xbase Options
  }
type
  TDBIXbaseDataConnectionOption = (
    xsShowDeleted,      { If set then all records are displayed (inc Deleted) }
    xsShowNullFlags,    { Display NullFlags in the Fielddefs if set.          }
    xsStrictHeader,     { Verify that this is a valid Xbase file              }
    xsStrictFieldNames, { Fieldnames will always be in uppercase              }
    xsStrictFieldValues,{ Don't trim fields, preserve the data as in foxpro   }
    xsStrictDateTimes,  { If turned off then Datetime fields will preserve    }
                        { full compatibility with Foxpro,                     }
                        { If set then Datetime fields will be accurate to     }
                        { the millisecond.                                    }
    xsMapNumericAsBCD   { If set then Numeric fields are mapped to BCD, else  }
    );                  { Numeric fields are mapped to standard field types.  }

  TDBIXbaseDataConnectionOptions = set of TDBIXbaseDataConnectionOption;


  // ___________________________________________________________________________
  {**
    <CODE>

         +---------+
         | TObject |
         +---------+
              |
              |
      +---------------------+
      | TDBIDataConnection  |
      | <DBIInterfaces.pas> |
      +---------------------+
              |
              |
      +--------------------------------------+
      |    TDBICustomStreamDataConnection    |
      | <DBICustomStreamDataConnections.pas> |
      +--------------------------------------+
              |                       +---------------+
              |                       |  FDataStream  |
    +---------------------------+     |   (TStream)   |
    | TDBIXbaseDataConnection   |<>---| <Classes.pas> |
    | <DBIXbaseConnections.pas> |     +---------------+
    |                           |     +------------------------------+
    |                           |<>---|       FBlobConnection        |
    +---------------------------+     |  (TDBIFoxProBlobConnection)  |
                                      |<DBIXbaseBlobConnections.pas> |
                                      +------------------------------+
    </CODE>
  }
type
  TDBIXbaseDataConnection = class(TDBICustomStreamDataConnection)
  private
    FOptions: TDBIXbaseDataConnectionOptions;

    // Physical Dataset metadata
    FHeader: TDBIXbaseHeader;
    FFields: TDBIXbaseFields;

    procedure EncodeFieldDesc(
      const PhysicalFieldProps: TDBIXbaseField;
      PFieldDesc: pDSFLDDesc
      );
    procedure EncodePhysicalFieldDefs;
    procedure InitializeNullFlags;
    class function IsFieldCompatible(
      PFieldProps: PDBIXbaseField;
      PFieldDesc: pDSFLDDesc
      ): Boolean;
    class function IsValidField(PFieldDesc: pDSFLDDesc): Boolean;

  protected
    function AllocPhysicalBuffer: TDBIRecordBuffer; override;
    procedure FreePhysicalBuffer(var Buffer: TDBIRecordBuffer); override;
    function GetBlobFileName: TFileName; override;

    function GetModificationDate: TDateTime;
    function GetPhysicalFieldNameByIndex(const Index: Integer): TDBIString;
    function GetRecordSize: Word; override;
    function GetVersion: TDBIXbaseVersion;
    procedure SetModificationDate(const Value: TDateTime);

  protected
    // Data methods
    function GetDataOffset: Integer;
    function GetRecordCount(const StatusFilter: DSAttr): Integer; override;

    procedure SetFlags(const Value: TDBIXbaseFlags);
    procedure SetDataOffset(const Value: Integer);
{$ifdef _UNUSED}
    procedure SetRecordCount(const Value: Integer); override;
{$endif}
    procedure SetLanguage(const Value: Byte);
{$ifdef _UNUSED}
    procedure SetVersion(const Value: Byte);
{$endif}

    procedure GetPhysicalProperties(PFieldProps: PDSFLDDesc; var PhysicalFieldProps: TDBIXbaseField);
    procedure GetMetaData; override;
    procedure ReadMetaData;
    procedure WriteMetaData(
      const AMode: TDBIMetadataMode = mdAll;
      const ForceWrite: Boolean = False
      ); override;

    property DataOffset: Integer read GetDataOffset write SetDataOffset;
    property PhysicalFieldNames[const Index: Integer]: TDBIString read GetPhysicalFieldNameByIndex;
    property ModificationDate: TDateTime read GetModificationDate write SetModificationDate;

  public
    // Conversion Routines from Record-Buffer to Field-Buffer
    function GetData(const PhysicalBuffer; var Buffer): DSAttr; override;
    procedure PutData(var PhysicalBuffer; const Buffer); override;
    function WriteData(
      const APosition: Integer;
      var APhysicalBuffer;
      ASize: Integer = -1
      ): Longint; override;

    function GetFieldAsString(
      const RecordBuffer;
      FieldNo: Word;
      var HasValidData: Boolean
      ): TDBIString;

    function GetFieldAsPChar(RecordBuffer: TDBIRecordBuffer; FieldNo: Word): Boolean;

    function GetFieldUnknown(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldUnknown(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldValue(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldValue(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldBoolean(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldBoolean(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldString(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldString(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldFloat(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldFloat(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldFloatFP(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldFloatFP(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldCurrency(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldCurrency(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldBCD(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldBCD(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldInteger(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldInteger(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldIntegerFP(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldIntegerFP(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldNullFlags(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldNullFlags(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldDateTime(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldDateTime(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldDate(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldDate(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldMemo(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldMemo(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

    function GetFieldDeleted(
      const RecordBuffer;
      var FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      ): Boolean;
    procedure PutFieldDeleted(
      var RecordBuffer;
      const FieldBuffer: TDBIFieldBuffer;
      FieldNo: Word
      );

  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;
    procedure Reset; override;

    procedure CreateDS(iFields: LongWord; pFldDes: pDSFLDDesc; pszName: TDBINameBuffer); override;
    procedure Delete(const Position: Integer); override;
    procedure Get(const Position: Integer; out Buffer; DataInfo: PDataInfo); override;

    // Locking methods
    // _________________________________________________________________________
    {**
      <B>Issues:</B><BR>
      <UL>
        <LI>property RetryCount: Integer;
        <LI>property RetryPeriod: Integer;
        <LI>Implicit versus Explicit record locking
        <LI>File Locking
        <LI>Locking for different versions of Xbase data files
      </UL>

      <B>Conditions:</B><BR>
      <UL>
        <LI>If Xbase File has been opened Exclusively or ReadOnly then all
            locking is to be ignored.
        <LI>If Xbase File has a FileLock then all locking is to be ignored.
        <LI>If Implicit locking has been turned on (default) then
      </UL>

      <B>Issuses not yet looked into:</B><BR>
      <UL>
        <LI>Index locking, do I have to lock the '.cdx' file
        <LI>Blob/memo field locking, do I have to lock the '.fpt' file
      </UL>

      <B>Record Locking Information:</B><BR>
      <UL>
        <LI><U>Vfp version $30</U>
        <UL>
            <LI>LockOffset = $7FFFFFFE
            <LI>Locks 1 byte per record
            <LI>Algorithm to lock record
              >> Byte to lock = Lockoffset - RecordNumber
        </UL>

        <LI><U>Foxpro version $03 (DbaseIIIplus)</U><BR>
        <UL>
            <LI>LockOffset = $40000000
            <LI>Locks 1 byte per record
            <LI>Algorithm to lock record
              >> Byte to lock = Lockoffset + (RecordNumber * RecordSize)
        </UL>

      </UL>

      <B>File Locking Information:</B><BR>
      <UL>
        <LI><U>Vfp version $30</U>

        <LI><U>Foxpro version $03 (DbaseIIIplus)</U><BR>
      </UL>
    }
    function Lock: Boolean; override;
    function Unlock: Boolean; override;
    function LockRecord(const RecNo: Integer): Boolean; override;
    function UnlockRecord(const RecNo: Integer): Boolean; override;

    // Blob methods
    function CreateBlobConnection: TDBIXbaseCustomBlobConnection; override;

    // Properties specific to this implementation
    property Date: TDateTime read GetModificationDate write SetModificationDate;
    property Options: TDBIXbaseDataConnectionOptions read FOptions write FOptions;
    property Version: TDBIXbaseVersion read GetVersion;
    property Language: Byte read FHeader.Language write SetLanguage;
    property Flags: TDBIXbaseFlags read FHeader.Flags write SetFlags;

  end;  { TDBIXbaseDataConnection }


implementation

uses
{$ifdef DELPHI6}
  FmtBcd,
{$endif}
  TypInfo,
  Dialogs;


{ TDBIXbaseDataConnection }

// _____________________________________________________________________________
{**
  Jvr - 01/07/2005 10:42:40 - Updated code.<br>
}
constructor TDBIXbaseDataConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FOptions := [xsStrictHeader];
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 01/07/2005 10:42:48 - Updated code.<br>
}
destructor TDBIXbaseDataConnection.Destroy;
begin

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Clear all buffered data and refresh.

  Jvr - 12/02/2001 18:18:52.<P>
  Jvr - 19/06/2002 11:38:02.<P>
}
procedure TDBIXbaseDataConnection.Reset;
begin
  GetMetaData;
end;  { Reset }


// _____________________________________________________________________________
{**
  Jvr - 13/03/2001 13:44:03 - Deletion in an Xbase file only marks the record
                              deleted. The RecordCount in the metadata (header)
                              is not altered as this is the number of physical
                              records in the file.
                              (thus all records, current & deleted)<P>
}
procedure TDBIXbaseDataConnection.Delete(const Position: Integer);
var
  DeleteStatus: AnsiChar;

begin
  if (Position < 0) or (Position >= Integer(FHeader.RecordCount)) then begin
    raise EDBIException.Create(Self, 'Delete::670', 'Record "%d" out of legal range', [Position]);
  end;

  DataStream.Seek((FHeader.RecordSize * Position) + DataOffset, soFromBeginning);
  DataStream.Read(DeleteStatus{%H-}, 1);

  // Toggle the Deletion Status
  if (DeleteStatus = DT_DELFLAG) then begin
    DeleteStatus := ' ';
  end
  else begin
    DeleteStatus := DT_DELFLAG;
  end;

  DataStream.Seek((FHeader.RecordSize * Position) + DataOffset, soFromBeginning);
  DataStream.Write(DeleteStatus, 1);
  Modified := True;
  Dirty := True;
  { TODO 5 -oJvr -cTDBIXbaseDataConnection.Delete() :
    This still needs to be done otherwise the indices won't work!
    Or does it? Xbase records don't get really deleted so like in foxpro the
    record entries should probably stay in the index.
    This is a philosophical argument and probably needs some more thought
  }
//##JVR  NotifyDataEventCallBacks(Self, dbiRecordDeleted, Buffer);

  { TODO -oJvr -cTDBIXbaseDataConnection.Delete() :
    I'm not sure that we should update the header when doing a delete.
    No records have been added/deleted (the record count only reflects the total
    record count [active & inactive]) so the RecordCount doesn't change,
    and the datestamp is updated when we close the file.
  }
  WriteMetaData(mdHeaderOnly);
end;  { Delete }


// _____________________________________________________________________________
{**
  GetRecord data for the specified record position.

  Jvr - 01/11/2000 17:39:47.<BR>
  Jvr - 19/03/2001 12:22:50 - Record now returns dsRecActive or dsRecDeleted<P>
}
procedure TDBIXbaseDataConnection.Get(const Position: Integer; out Buffer; DataInfo: PDataInfo);
var
  PhysicalBuffer: TDBIRecordBuffer;
  LocalPhysicalBuffer: array[0..Default_TemporaryRecordBufferSize] of TDBIRecordElement;

begin
  if (Position < 0) or (Position >= Integer(FHeader.RecordCount)) then begin
  raise EDBIException.Create(Self, 'Get::720', 'Record "%d" out of legal range', [Position]);
  end;

  // Create a buffer on the heap if required buffer is larger than the LocalBuffer
  if (FHeader.RecordSize > SizeOf(LocalPhysicalBuffer)) then begin
    PhysicalBuffer := AllocPhysicalBuffer;
  end

  // Otherwise just use the LocalBuffer
  else begin
    PhysicalBuffer := @(LocalPhysicalBuffer[0]);
  end;


  try
    DataStream.Seek((FHeader.RecordSize * Position) + DataOffset, soFromBeginning);
    DataStream.Read(PhysicalBuffer^, FHeader.RecordSize);

    // If buffer is nil then we only want the status
    if (@Buffer <> nil) then begin
      GetData(PhysicalBuffer^, Buffer);
    end;

    // If deleted flag is set, then record is deleted
    if Assigned(DataInfo) then begin
      if (PhysicalBuffer[0] = TDBIRecordElement(DT_DELFLAG)) then begin
        DataInfo^.Attribute := dsRecDeleted;
      end

      // Otherwise record is active
      else begin
        DataInfo^.Attribute := dsRecActive;
      end;  { if }

      DataInfo^.Data := @Buffer;
    end;

  finally
    // Only free buffer if allocated on the heap
    if (FHeader.RecordSize > SizeOf(LocalPhysicalBuffer)) then begin
      FreePhysicalBuffer(PhysicalBuffer);
    end;
  end;
end;  { Get }


// _____________________________________________________________________________
{**
  Jvr - 15/04/2002 12:53:16.<BR>
  Jvr - 19/06/2002 18:47:16 - Lock now resets the Stream before Locking<P>
}
function TDBIXbaseDataConnection.Lock: Boolean;
var
  OffsetLow: LongWord;
  SizeLow: LongWord;

begin
  // Memory Streams can't be locked,
  // this dataset was probably created using one of the Load methods
  if (DataStream is TMemoryStream) then begin
    Result := True;
    Exit;
  end;

  OffsetLow := 0;

  // Calculate the File lock Offset Low
  case Version of
    xbDbase3: begin
      OffsetLow := XbaseLockOffsets[Version];
    end;  { xbDbase3 }

    xbVisualFoxPro: begin
      OffsetLow :=
        XbaseLockOffsets[Version] -
        Trunc((XbaseLockOffsets[Version] - FHeader.DataOffset) / (FHeader.RecordSize + 1));
    end;  { xbVisualFoxPro }
  end;  { case }


  // Calculate the File lock Size Low
  case Version of
    xbDbase3: begin
      // Lock/Unlock starting at the LockOffset UPTO the MaxFileSize
      SizeLow := $7FFFFFFF - XbaseLockOffsets[Version];
    end;

    xbVisualFoxPro: begin
      // Lock/Unlock starting at the LockOffset DOWNTO the MaxRecordCount
      SizeLow := {%H-} 1 + XbaseLockOffsets[Version] - OffsetLow;
    end;

  else
    raise EDBINotImplementedException.Create(Self, 'Lock::820',
      '"%s"'#13'is an Xbase file of version $%x'#13 +
      'Locking is not Yet! supported for this file version',
      [FileName, FHeader.Version]
      );
  end;  { case }


  // This makes sure that when we attempt to Append/Update/Delete
  // we get the latest version.
{$ifdef Use_BufferedStreams}
  if (DataStream is TDBIFileStream) then begin
    (DataStream as TDBIFileStream).Reset;
  end;
{$endif Use_BufferedStreams}

  Result := Windows.LockFile(
{$ifdef Use_BufferedStreams}
    (DataStream as TDBIFileStream).Handle,// handle of file to lock
{$else}
    (DataStream as TFileStream).Handle,   // handle of file to lock
{$endif Use_BufferedStreams}
     OffsetLow,                           // low-order word of lock region offset
     0,                                   // Win-95/98/Me: dwFileOffsetHigh must be 0
     SizeLow,                             // low-order word of length to lock
     0                                    // high-order word of length to lock
    );
end;  { Lock }


// _____________________________________________________________________________
{**
  Jvr - 15/04/2002 12:53:39.<BR>
  Jvr - 19/06/2002 18:47:49 - Unlock now resets the Stream before Locking<P>
}
function TDBIXbaseDataConnection.Unlock: Boolean;
var
  OffsetLow: LongWord;
  SizeLow: LongWord;

begin
  // Memory Streams can't be locked,
  // this dataset was probably created using one of the Load methods
  if (DataStream is TMemoryStream) then begin
    Result := True;
    Exit;
  end;

  Result := False;
  OffsetLow := 0;

  if (DataStream is TMemoryStream) then begin
    Exit;
  end;

  // Calculate the File lock Offset Low
  case Version of
    xbDbase3: begin
      OffsetLow := XbaseLockOffsets[Version];
    end;  { xbDbase3 }

    xbVisualFoxPro: begin
      OffsetLow :=
        XbaseLockOffsets[Version] -
        Trunc((XbaseLockOffsets[Version] - FHeader.DataOffset) / (FHeader.RecordSize + 1));
    end;  { xbVisualFoxPro }
  end;  { case }


  // Calculate the File lock Size Low
  case Version of
    xbDbase3: begin
      // Lock/Unlock starting at the LockOffset UPTO the MaxFileSize
      SizeLow := $7FFFFFFF - XbaseLockOffsets[Version];
    end;  { xbDbase3 }

    xbVisualFoxPro: begin
      // Lock/Unlock starting at the LockOffset DOWNTO the MaxRecordCount
      SizeLow := XbaseLockOffsets[Version] - OffsetLow;
    end;  { xbVisualFoxPro }

  else
    raise EDBINotImplementedException.Create(Self, 'Unlock::895',
      '"%s"'#13'is an Xbase file of version $%x'#13 +
      'Locking is not Yet! supported for this file version',
      [FileName, FHeader.Version]
      );
  end;  { case }


  // This makes sure that when we attempt to Append/Update/Delete
  // we get the latest version.
{$ifdef Use_BufferedStreams}
  if (DataStream is TDBIFileStream) then begin
    (DataStream as TDBIFileStream).Reset;
  end;
{$endif Use_BufferedStreams}

  Result := Windows.UnLockFile(
{$ifdef Use_BufferedStreams}
    (DataStream as TDBIFileStream).Handle,// handle of file to lock
{$else}
    (DataStream as TFileStream).Handle,   // handle of file to lock
{$endif Use_BufferedStreams}
    OffsetLow,                            // low-order word of lock region offset
    0,                                    // Win-95/98/Me: dwFileOffsetHigh must be 0
    SizeLow,                              // low-order word of length to lock
    0                                     // high-order word of length to lock
    );
end;  { Unlock }


// _____________________________________________________________________________
{**
  Jvr - 15/04/2002 12:57:16.<P>
  Jvr - 19/06/2002 18:55:23 - LockRecord now resets the Stream before Locking<P>
}
function TDBIXbaseDataConnection.LockRecord(const RecNo: Integer): Boolean;
var
  OffsetLow: LongWord;
  OffsetHigh: LongWord;
  SizeLow: LongWord;
  SizeHigh: LongWord;

begin
  // Return ok unlessthe locking operation fails
  Result := True;

  // Memory Streams can't be locked,
  // this dataset was probably created using one of the Load methods
  if (DataStream is TMemoryStream) then begin
    Exit;
  end;

  OffsetHigh := 0;
  SizeLow := 1;
  SizeHigh := 0;

  // Calculate Locking parameters
  case Version of
    xbDbase3: begin
      OffsetLow :=
        XbaseLockOffsets[Version] +
        FHeader.DataOffset +
        LongWord((RecNo-1) * FHeader.RecordSize);
    end;

    xbVisualFoxpro: begin
      OffsetLow := XbaseLockOffsets[Version] - LongWord(RecNo);
    end;
  else
    raise EDBINotImplementedException.Create(Self, 'LockRecord::965',
      '"%s"'#13'is an Xbase file of version $%x'#13 +
      'Locking is not Yet! supported for this file version',
      [FileName, FHeader.Version]
      );
  end;  { case }

  // This makes sure that when we attempt to Append/Update/Delete
  // we get the latest version.
{$ifdef Use_BufferedStreams}
  if (DataStream is TDBIFileStream) then begin
    (DataStream as TDBIFileStream).Reset;
  end;
{$endif Use_BufferedStreams}

  // if the offset const is valid (not 0) then proceed
  if (XbaseLockOffsets[Version] <> $0) then begin
    Result := Windows.LockFile(
{$ifdef Use_BufferedStreams}
      (DataStream as TDBIFileStream).Handle,// handle of file to lock
{$else}
      (DataStream as TFileStream).Handle,  // handle of file to lock
{$endif Use_BufferedStreams}
      OffsetLow,                    // low-order word of lock region offset
      OffsetHigh,                   // Win-95/98/Me: dwFileOffsetHigh must be 0
      SizeLow,                      // low-order word of length to lock
      SizeHigh                      // high-order word of length to lock
      );
  end;
end;  { LockRecord }


// _____________________________________________________________________________
{**
  Jvr - 15/04/2002 12:58:19.<P>
  Jvr - 19/06/2002 18:55:49 - UnlockRecord now resets the Stream before Locking<P>
}
function TDBIXbaseDataConnection.UnLockRecord(const RecNo: Integer): Boolean;
var
  OffsetLow: LongWord;
  OffsetHigh: LongWord;
  SizeLow: LongWord;
  SizeHigh: LongWord;

begin
  // Return ok unlessthe locking operation fails
  Result := True;

  // Memory Streams can't be locked,
  // this dataset was probably created using one of the Load methods
  if (DataStream is TMemoryStream) then begin
    Exit;
  end;

  OffsetHigh := 0;
  SizeLow := 1;
  SizeHigh := 0;

  // Calculate Locking parameters
  case Version of
    xbDbase3: begin
      OffsetLow :=
        XbaseLockOffsets[Version] +
        FHeader.DataOffset +
        LongWord((RecNo-1) * FHeader.RecordSize);
    end;

    xbVisualFoxpro: begin
      OffsetLow := XbaseLockOffsets[Version] - LongWord(RecNo);
    end;
  else
    raise EDBINotImplementedException.Create(Self, 'UnlockRecord::1035',
      '"%s"'#13'is an Xbase file of version $%x'#13 +
      'Locking is not Yet! supported for this file version',
      [FileName, FHeader.Version]
      );
  end;  { case }


  // This makes sure that when we attempt to Append/Update/Delete
  // we get the latest version.
{$ifdef Use_BufferedStreams}
  if (DataStream is TDBIFileStream) then begin
    (DataStream as TDBIFileStream).Reset;
  end;
{$endif Use_BufferedStreams}

  // if the offset const is valid (not 0) then proceed
  if (XbaseLockOffsets[Version] <> $0) then begin
    Result := Windows.UnLockFile(
{$ifdef Use_BufferedStreams}
      (DataStream as TDBIFileStream).Handle,// handle of file to lock
{$else}
      (DataStream as TFileStream).Handle,  // handle of file to lock
{$endif Use_BufferedStreams}
      OffsetLow,                    // low-order word of lock region offset
      OffsetHigh,                   // Win-95/98/Me: dwFileOffsetHigh must be 0
      SizeLow,                     // low-order word of length to lock
      SizeHigh                      // high-order word of length to lock
      );
  end;
end;  { UnlockRecord }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 13:00:59.<BR>
  Jvr - 07/03/2001 15:30:18 - If the data file is not allready opened then we
                              attempt to open it, read the header,
                              and then close it again.<BR>
  Jvr - 08/03/2001 13:43:21 - Validate the Header if specified in options.<BR>
  Jvr - 05/06/2001 14:34:17 - Now allows Lower Chars in FieldNames and Digits.<BR>
  Jvr - 14/02/2003 14:07:28 - Now using Setlength to resize the FFields array.<BR>
  Jvr - 14/02/2003 14:58:27 - Added limited support for extended fieldnames.<P>
  Jvr - 24/06/2005 10:17:41 - Added more error checking when reading header.<br>
}
class function TDBIXbaseDataConnection.IsFieldCompatible(PFieldProps: PDBIXbaseField; PFieldDesc: pDSFLDDesc): Boolean;
var
  FldType: Word;

begin
  FldType := XbaseFieldBaseTypeMap[PFieldProps^.FieldType];
  Result :=
    ((FldType = fldZSTRING) or (FldType = fldWIDESTRING) or (FldType = fldUNICODE));

  FldType := PFieldDesc^.iFldType;
  Result := Result and
    ((FldType = fldZSTRING) or (FldType = fldWIDESTRING) or (FldType = fldUNICODE));
end;


class function TDBIXbaseDataConnection.IsValidField(PFieldDesc: pDSFLDDesc): Boolean;
begin
  Result :=
    (PFieldDesc^.iFldType = fldUNICODE) or
    (PFieldDesc^.iFldType in [fldZSTRING..fldWIDESTRING, fldSINGLE..fldUINT8, fldDATETIMEOFFSET]);

  if Result then begin
    case PFieldDesc^.iFldType of
      fldBLOB: begin
        Assert(PFieldDesc^.iFldSubType > 0);

        Result := (PFieldDesc^.iFldSubType = fldstMEMO) and
          ((PFieldDesc^.iFldLen = SizeOfMemo) or (PFieldDesc^.iFldLen = 10)) { DBase-3 }
      end;

      fldSINGLE, fldFLOAT, fldFLOATIEEE: Result := True;
    else
      Assert(PFieldDesc^.iUnits1 > 0);
    end;
  end;
end;


procedure TDBIXbaseDataConnection.EncodeFieldDesc(
  const PhysicalFieldProps: TDBIXbaseField;
  PFieldDesc: pDSFLDDesc
  );
var
  Attributes: TFieldAttributes;

begin
  // Set Field Attributes
  if (ffNullableField in PhysicalFieldProps.FieldFlags) then begin
    Attributes := [];
  end
  else begin
    Attributes := [faRequired];
  end;

  // Now the complicated stuff - field types, subtypes, and other.
  // This takes care of
  // DT_CURRENCY, DT_DATE, DT_DATETIME, DT_DOUBLE, DT_GENERAL, DT_LOGICAL
  if (PhysicalFieldProps.FieldType in ['A'..'Z']) then begin
    // Check to see if we allready have a compatible type
    // If so then DO NOT change the type
    if not IsFieldCompatible(@PhysicalFieldProps, PFieldDesc) then begin
      PFieldDesc^.iFldType := XbaseFieldBaseTypeMap[PhysicalFieldProps.FieldType];
      PFieldDesc^.iFldSubType := XbaseFieldSubTypeMap[PhysicalFieldProps.FieldType];

      if (PFieldDesc^.iFldType = fldBlob) then begin
        PFieldDesc^.iFldLen := PhysicalFieldProps.FieldSize[0];   // Size
      end
      else begin
        PFieldDesc^.iUnits1 := PhysicalFieldProps.FieldSize[0];   // Size
      end;

      PFieldDesc^.iUnits2 := PhysicalFieldProps.FieldSize[1];     // Decimals
    end;
  end

  // DT_NULLFLAGS
  else if (PhysicalFieldProps.FieldType = DT_NULLFLAGS) then begin
    PFieldDesc^.iFldType := fldBYTES;
    PFieldDesc^.iFldSubType := fldstNONE;
    PFieldDesc^.iUnits1 := PhysicalFieldProps.FieldSize[0];   // Size
    PFieldDesc^.iUnits2 := PhysicalFieldProps.FieldSize[1];   // Decimals
  end

  // Otherwise it's an invalid field type
  else begin
    PFieldDesc^.iFldType := fldUNKNOWN;
    PFieldDesc^.iFldSubType := fldstNONE;

    raise EDBIException.Create(Self, 'GetMetaData::EncodeField::1185',
      'Unknown Xbase Field Type "%s"', [PhysicalFieldProps.FieldType]
      );
  end;


  // Now set the type dependant stuff
  case PhysicalFieldProps.FieldType of
    DT_INTEGER: begin
      if (PFieldDesc^.iUnits2 = DT_AUTOINCFLAG) then begin
        PFieldDesc^.iFldSubType := fldstAUTOINC;
      end;
    end;  { DT_INTEGER }

    DT_MEMO: begin
      Attributes := []; //##JVR - Blob NOT yet Initialised, can't do this ??? // BlobConnection.Attributes;
    end;  { DT_MEMO }

    DT_NUMERIC, DT_FLOAT: begin
      // Map to BCD
      if xsMapNumericAsBCD in Options then begin
        if (PFieldDesc^.iUnits2 < 5) then begin
          PFieldDesc^.iFldType := fldBCD;
        end
        else begin
{$ifdef DELPHI6}
          PFieldDesc^.iFldType := fldFMTBCD;
{$else}
          PFieldDesc^.iFldType := fldBCD;
{$endif}
        end;
      end

      // Otherwise map to standard types
      else begin
        if (PFieldDesc^.iUnits2 = 0) then begin
          if (PFieldDesc^.iUnits1 < 5) then begin
            PFieldDesc^.iFldType := fldINT16;
          end
          else if (PFieldDesc^.iUnits1 >= 5) and (PFieldDesc^.iUnits1 < 10) then begin
            PFieldDesc^.iFldType := fldINT32;
          end
          else begin
            PFieldDesc^.iFldType := fldINT64;
          end;  { if }
        end
        else begin
          PFieldDesc^.iFldType := fldFLOAT;
        end;  { if }
      end;  { if }
    end;  { DT_NUMERIC, DT_FLOAT }

    DT_NULLFLAGS: begin
      Include(Attributes, faReadOnly);

      if not (xsShowNullFlags in FOptions) then begin
        Include(Attributes, faHiddenCol);
      end;
    end;  { DT_NULLFLAGS }
  end;  { case }

  PFieldDesc^.iFldAttr := PByte(@Attributes)^;
end;  { EncodeFieldDesc }


procedure TDBIXbaseDataConnection.InitializeNullFlags;
var
  FieldNo: Integer;
  NullFlagsIndex: Integer;
  IsNullable: Boolean;

begin
  // Physical Null field flag information
  NullFlags.IsNullable :=
    DBICompareText(FFields[FieldCount - 1].FieldName, FieldName_NullFlags) = 0;

  NullFlagsIndex := 0;
  for FieldNo := 0 to FieldCount - 1 do begin
    // Create Index values for the physical fields Null-Flags
    IsNullable := ffNullableField in FFields[FieldNo].FieldFlags;
    if NullFlags.SetNullIndex(FieldNo, IsNullable, NullFlagsIndex) then begin
      Inc(NullFlagsIndex);
    end;
  end;
end;  { InitializeNullFlags }


procedure TDBIXbaseDataConnection.EncodePhysicalFieldDefs;
const
  ValidFieldChars = ['0'..'9', 'A'..'Z', '_', 'a'..'z'];

var
  FieldNo: Integer;
  PhysicalFieldOffset: LongWord;

begin
  // First Field Starts at 1 (DelFlag starts at 0)
  PhysicalFieldOffset := 1;

  for FieldNo := Low(FFields) to High(FFields) do begin
    // If not a valid field name character then set FFields array dimension
    // to current number of valid fields read.
    if not (FFields[FieldNo].FieldName[0] in ValidFieldChars) then begin
      // Set the FieldCount and allocate space for logical fieldprops
      FieldCount := FieldNo;
      SetLength(FFields, FieldCount);
      Break;
    end

    // Update the field offset if not specified in the Header (e.g. Value = 0)
    // Field Offsets need to be calculated if they are non existent
    else if (LongWord(FFields[FieldNo].FieldOffset) = 0) then begin
      LongWord(FFields[FieldNo].FieldOffset) := PhysicalFieldOffset;
    end;

    // Calculate the Physical Field Offsets
    if (FFields[FieldNo].FieldType = DT_CHARACTER) then begin
      Inc(PhysicalFieldOffset, Word(FFields[FieldNo].FieldSize));
    end
    else begin
      Inc(PhysicalFieldOffset, FFields[FieldNo].FieldSize[0]);
    end;
  end;
end;  { EncodePhysicalFieldDefs }


procedure TDBIXbaseDataConnection.GetMetaData;
var
  FieldNo: Integer;

begin
  // Read the Metadata from the table
  ReadMetaData;

  EncodePhysicalFieldDefs;
  InitializeNullFlags;

  for FieldNo := 0 to FieldCount - 1 do begin
    if not IsValidField(@(FieldProps[FieldNo])) then begin
      FieldProps[FieldNo].szName := PhysicalFieldNames[FieldNo];

      EncodeFieldDesc(FFields[FieldNo], @(FieldProps[FieldNo]));
    end;
  end;

  SetFieldProps(FieldProps);
end;  { GetMetaData }


// _____________________________________________________________________________
{**
  Jvr - 01/07/2005 17:23:13 - Initial code.<br>
}
procedure TDBIXbaseDataConnection.ReadMetaData;
var
  BytesRead: Integer;
  WasAllReadyOpen: Boolean;

begin
  // This allows us to just read the datafile header
  WasAllReadyOpen := IsCursorOpen;
  if not WasAllReadyOpen then begin
    OpenDataStream(False);
{$ifdef Use_BufferedStreams}
  end
  else if (DataStream is TDBIFileStream) then begin
    (DataStream as TDBIFileStream).Reset;
{$endif Use_BufferedStreams}
  end;

  try
    // Read General Header Information
    DataStream.Seek(0, soFromBeginning);
    BytesRead := DataStream.Read(FHeader, SizeOf(FHeader));

    if (BytesRead < SizeOf(FHeader)) then begin
      raise EDBIException.Create(Self, 'ReadMetaData::1365', SReadHeaderFailed, []);
    end;

    // Only verify that this is a valid Xbase file if specified in options
    if (xsStrictHeader in FOptions) then begin
      GetVersion;
    end;

    // Read Fields Data
    // Setup Maximum number of allowed field properties in array
    SetLength(FFields, XbaseFoxProMaxFieldCount);
    BytesRead := DataStream.Read(
      TDBIFieldData(@(FFields[0]))^,
      SizeOf(TDBIXbaseField) * XbaseFoxProMaxFieldCount
      );
    if (BytesRead < SizeOf(TDBIXbaseField)) then begin
      raise EDBIException.Create(Self, 'ReadMetaData::1375', SReadFieldsFailed, []);
    end;
  finally
    if not WasAllReadyOpen then begin
      CloseDataStream;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 13:01:03.<BR>
  Jvr - 02/02/2001 12:54:49 - WriteMetaData now checks the
                              FDirty Flag and ForceWrite flags
                              writing the MetaData<P>
}
procedure  TDBIXbaseDataConnection.WriteMetaData(
  const AMode: TDBIMetadataMode = mdAll;
  const ForceWrite: Boolean = False
  );
var
  CurrentPosition: Integer;
  Index: Integer;

  procedure ClearHeader;
  var
    PCleanHeader: TDBIRecordBuffer;

  begin
    // Clear the Header of the stream
    // Set the length of the File to the size of the header (FHeader.Dataoffset)
    GetMem(PCleanHeader, FHeader.DataOffset);
    try
      // Initialise the buffer to nulls
      FillChar(PCleanHeader^, FHeader.DataOffset, #0);

      // Write the prepared buffer to the stream
      DataStream.Seek(0, soFromBeginning);
      DataStream.Write(PCleanHeader^, FHeader.DataOffset);
    finally
      FreeMem(PCleanHeader);
    end;
  end;  { ClearHeader }


begin
  // If the buffers aren't dirty (current data has already been saved)
  // then don't bother writing the MetaData back to disk
  // unless the Forcewrite flags insists we do.
  if not Dirty and not ForceWrite then Exit;

  Assert(Assigned(DataStream));
  CurrentPosition := DataStream.Position;
  try
    try
      if (AMode = mdNew) then begin
        ClearHeader;
      end;

      // Write General Header Information back to stream
      DataStream.Seek(0, soFromBeginning);
      DataStream.Write(FHeader, SizeOf(FHeader));
      Modified := True;
    except
      on E: Exception do
        raise EDBIException.Create(
          Self, E, 'WriteMetaData::1445', 'Unable to Write Header Structure', []);
    end;  { try..except }

    if (AMode = mdAll) or (AMode = mdNew) then begin
      // Write Fields Data
      try
        for Index := 0 to FieldCount-1 do begin
          DataStream.Write(TDBIFieldData(@(FFields[Index]))^, SizeOf(TDBIXbaseField));
        end;
      except
        on E: Exception do
          raise EDBIException.Create(
            Self, E, 'WriteMetaData::1455', 'Unable to Write Fields Structure', []);
      end;  { try..except }
    end;  { if }

  finally
    // Reset Position to original position
    DataStream.Position := CurrentPosition;
    Dirty := False;
  end;  { try..finally }

end;  { WriteMetaData }


// _____________________________________________________________________________
{**
  Jvr - 01/07/2005 10:47:39 - Updated code.<br>
}
function TDBIXbaseDataConnection.GetDataOffset: Integer;
begin
  Result := FHeader.DataOffset;
end;  { GetDataOffset }


// _____________________________________________________________________________
{**
  Jvr - 01/07/2005 10:47:48 - Updated code.<br>
}
function TDBIXbaseDataConnection.GetModificationDate: TDateTime;
  { TODO 1 -ojvr -cTDBIXbaseDataConnection.GetModificationDate() :
    Guess what, Visual Foxpro has a Y2k problem,
    Oh well this will keep things going for the next 50 years
  }
  function Y2K: Integer;
  begin
    if (FHeader.Modified.Year < 50) then begin
      Result := FHeader.Modified.Year + XBaseFoxProY2KYearOffset;
    end
    else begin
      Result := FHeader.Modified.Year + XBaseFoxProStdYearOffset;
    end;
  end;  { Y2K }

begin
  Result := EncodeDate(Y2K, FHeader.Modified.Month, FHeader.Modified.Day);
end;  { GetModified }


function TDBIXbaseDataConnection.GetPhysicalFieldNameByIndex(const Index: Integer): TDBIString;
var
  PResult: PAnsiChar;

begin
  SetLength(Result, 255);
  PResult := PAnsiChar(Result);
  {$ifdef DelphiXE4}AnsiStrings.{$endif}StrLCopy(PResult, @(FFields[Index].FieldName), SizeOf(FFields[Index].FieldName)-1);

  // If we have extended fields then append the rest of the name
  if (xfExtendedFields in Flags) then begin
    {$ifdef DelphiXE4}AnsiStrings.{$endif}StrLCat(PResult, @(FFields[Index].FieldData), (SizeOf(FFields[Index].FieldName)-1) + SizeOf(FFields[Index].FieldData));
  end;

  SetLength(Result, {$ifdef DelphiXE4}AnsiStrings.{$endif}StrLen(PResult));
end;


// _____________________________________________________________________________
{**
  Jvr - 11/05/2005 17:11:49 - Initial code.<br>
}
function TDBIXbaseDataConnection.GetRecordSize: Word;
begin
  Result := FHeader.RecordSize;
end;  { GetRecordSize }


// _____________________________________________________________________________
{**
  Jvr - 07/03/2001 14:22:10.<P>
}
function TDBIXbaseDataConnection.GetVersion: TDBIXbaseVersion;
begin
  with FHeader do begin
    case Version of
      DSVer_FoxBase: Result := xbFoxBase;           // $02 = FoxBase
      DSVer_FoxBase_Memo: Result := xbFoxBase;      // $FB = FoxBase with Memo
      DSVer_FoxPro: Result := xbFoxPro;             // $F5 = FoxPro with Memo
      DSVer_VisualFoxPro: Result := xbVisualFoxPro; // $30 = Visual Foxpro
      DSVer_DBase3: Result := xbDbase3;             // $03 = dBase3, no memo
      DSVer_DBase3_Memo: Result := xbDbase3;        // $83 = dBase3, with memo
      DSVer_DBase4: Result := xbDbase4;             // $04 = dBase4, no memo
      DSVer_DBase5: Result := xbDbase5;             // $05 = dBase5, no memo
    else
      inherited Active := False;
      
      raise EDBIException.Create(Self, 'GetVersion::1530',
        'Not a known Xbase file, Signature = "%x"',
        [FHeader.Version]
        );
    end;  { case }
  end;
end;  { GetVersionInfo }


// _____________________________________________________________________________
{**
  Jvr - 14/03/2001 15:16:07 - Get RecordCount returns the number of records for
                              a particular FilterStatus.<br>
  Jvr - 02/05/2001 12:41:33 - Now first checks the Physical RecordCount.<br>
  Jvr - 09/07/2002 12:55:02 - Changed it again. Greg need to be able to get the
                              active/deleted (dsRecActive/dsRecDeleted)
                              record count, so I re-instated the functionality.<br>
  Jvr - 09/12/2004 17:57:01 - Replaced Attributes with DataInfo<p>

<B>Notes:</B>
<TABLE>
  <TR>
    <TD>dsRecAll</TD>
    <TD>Returns a record count of all records in the physical file</TD>
  </TR>
  <TR>
    <TD>dsRecActive</TD>
    <TD>Returns a record count of all active records</TD>
  </TR>
  <TR>
    <TD>dsRecDeleted</TD>
    <TD>Returns a record count of all records marked deleted</TD>
  </TR>
</TABLE>
}
function TDBIXbaseDataConnection.GetRecordCount(
  const StatusFilter: DSAttr
  ): Integer;
var
  Position: Integer;
//##JVR  Attributes: DSAttr;
  DataInfo: TDBIDataInfo;
  PRecBuf: TDBIRecordBuffer;

begin
{##JVR
  Result := -1;
  PRecBuf := nil;

  // If Physical Recordcount is Zero then return 0 no matter what
  if FHeader.RecordCount = 0 then begin
    Result := 0;
  end
}
  // If dsRecAll then Get the RecordCount from the metadata (Header)
  if (StatusFilter and dsRecAll) = dsRecAll then begin
    Result := FHeader.RecordCount;
  end

  // If dsRecRefresh then re-read the RecordCount from the DataStream
  // NOTE:
  //   dsRecRefresh has to be before all other cases because the value
  //   of dsRecRefresh includes all other values (255)
  else if (StatusFilter = dsRecRefresh) then begin
    raise EDBIException.Create(Self, 'GetRecordCount::1595',
      'Refreshed RecordCount currently disabled',
      [StatusFilter]
      );

    GetMetaData;
    Result := FHeader.RecordCount;
  end

  // If dsRecActive then Get the RecordCount from the metadata (Header)
  //##JVR -  NO, count the active records.
  else if (StatusFilter and dsRecActive) = dsRecActive then begin
    raise EDBIException.Create(Self, 'GetRecordCount::1605',
      'Get RecordCount for active records currently disabled',
      [StatusFilter]
      );

//##JVR    Result := FHeader.RecordCount;
    GetMetaData;
    Result := 0;
    for Position := 0 to FHeader.RecordCount-1 do begin
//##JVR      Attributes := GetCurrentRecord(Position, PRecBuf^);
      GetCurrentRecord(Position, {%H-}PRecBuf^, @DataInfo);
//##JVR      if (Attributes and StatusFilter) = StatusFilter then begin
      if (DataInfo.Attribute and StatusFilter) = StatusFilter then begin
        Inc(Result);
      end;  { if }
    end;  { for }
  end

  // Count the deleted records
  else if (StatusFilter and dsRecDeleted) = dsRecDeleted then begin
    raise EDBIException.Create(Self, 'GetRecordCount::1625',
      'Get RecordCount for deleted records currently disabled',
      [StatusFilter]
      );

    GetMetaData;
    Result := 0;
    for Position := 0 to FHeader.RecordCount-1 do begin
      {Attributes := }GetCurrentRecord(Position, PRecBuf^, @DataInfo);
//##JVR      if (Attributes and StatusFilter) = StatusFilter then begin
      if (DataInfo.Attribute and StatusFilter) = StatusFilter then begin
        Inc(Result);
      end;  { if }
    end;  { for }
  end

  // Oops how did we get here?
  else begin
    raise EDBIException.Create(Self, 'GetRecordCount::1645',
      'Illegal status filter value "%d" for RecordCount',
      [StatusFilter]
      );
    Result := 0;
  end;  { if }
end;   { GetRecordCount }


// _____________________________________________________________________________
{**
  Jvr - 14/02/2003 16:55:20 - Initial code.<br>
}
procedure TDBIXbaseDataConnection.SetFlags(const Value: TDBIXbaseFlags);
begin
  FHeader.Flags := Value - TDBIXbaseInvalidFlags;
end;  { SetFlags }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 14:11:28 - Initial code.<br>
}
procedure TDBIXbaseDataConnection.SetDataOffset(const Value: Integer);
begin
  if (FHeader.DataOffset <> Value) then begin
    FHeader.DataOffset := Value;
    Modified := True;
    Dirty := True;
  end;
end;  { SetDataOffset }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 14:11:15 - Initial code.<br>
}
procedure TDBIXbaseDataConnection.SetModificationDate(const Value: TDateTime);
var
  YearValue: Word;
  MonthValue: Word;
  DayValue: Word;

begin
  DecodeDate(Value, YearValue, MonthValue, DayValue);

  FHeader.Modified.Year := YearValue - XBaseFoxProStdYearOffset;
  FHeader.Modified.Month := MonthValue;
  FHeader.Modified.Day := DayValue;
  Modified := True;
end;  { SetModificationDate }


// _____________________________________________________________________________
{**
  Jvr - 21/03/2000 14:12:51.<P>
}
{$ifdef _UNUSED}
procedure TDBIXbaseDataConnection.SetRecordCount(const Value: Integer);
begin
  if (Integer(FHeader.RecordCount) <> Value) then begin
    FHeader.RecordCount := Value;
    Modified := True;
    FDirty := True;
  end;
end;  { SetRecordCount }
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 02/02/2001 17:10:42.<P>
}
procedure TDBIXbaseDataConnection.SetLanguage(const Value: Byte);
begin
  if (FHeader.Language <> Value) then begin
    FHeader.Language := Value;
    Modified := True;
    Dirty := True;
  end;
end;  { SetLanguage }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 14:11:06.<P>
}
{$ifdef _UNUSED}
procedure TDBIXbaseDataConnection.SetVersion(const Value: Byte);
begin
  if (FHeader.Version <> Value) then begin
    FHeader.Version := Value;
    Modified := True;
    FDirty := True;
  end;
end;  { SetVersion }
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 11/05/2005 16:26:23 - Initial code.<br>
}
function TDBIXbaseDataConnection.CreateBlobConnection: TDBIXbaseCustomBlobConnection;
begin
  case Version of
    xbDbase3: Result := TDBIDbaseBlobConnection.Create;
    xbFoxPro: Result := TDBIFoxProBlobConnection.Create;
    xbVisualFoxpro: Result := TDBIFoxProBlobConnection.Create;
  else
    raise EDBIException.Create(Self, 'CreateBlobConnection::1755',
      'This Xbase file data format has no support for Blobs', []
      );
  end;

  Result.Version := Version;
end;  { CreateBlobConnection }


// _____________________________________________________________________________
{**
  Jvr - 29/10/2002 15:50:52.<P>
}
function TDBIXbaseDataConnection.GetBlobFileName: TFileName;
const
  ExtensionMap: array[TDBIXbaseVersion] of String = (
    '.err',        // xbUnknownXbaseVersion,
    '.fpt',        // xbFoxBase,
    '.fpt',        // xbFoxPro,
    '.fpt',        // xbVisualFoxPro,
    '.dbt',        // xbDbase3,
    '.dbt',        // xbDbase4,
    '.dbt'         // xbDbase5
    );
begin
  if (FileName <> '') then begin
    Result := ChangeFileExt(FileName, ExtensionMap[Version]);
  end
  else begin
    Result := FileName;
  end;
end;  { GetBlobFileName }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 12:18:45.<BR>
  Jvr - 05/02/2001 14:29:13 - Added support for FP Integer & Double datatypes<BR>
  Jvr - 16/02/2001 12:39:36 - Added support for Subtypes<BR>
  Jvr - 17/09/2002 12:33:38 - Added BCD field support<BR>
  Jvr - 07/10/2002 11:49:16 - Added FMTBCD field support for geater precision<P>
}
type
  PFieldAttributes = ^TFieldAttributes;

procedure TDBIXbaseDataConnection.GetPhysicalProperties(
  PFieldProps: PDSFLDDesc;
  var PhysicalFieldProps: TDBIXbaseField
  );
const
  dbfPhysicalSizeOfBoolean = 1;
  dbfPhysicalSizeOfBlob = 4;
  dbfPhysicalSizeOfDate = 8;
  dbfPhysicalSizeOfDouble = 8;
  dbfPhysicalSizeOfTimeStamp = 8;
  dbfPhysicalSizeOfInteger = 4;
  dbfPhysicalSizeOfNumericInt08 = 4;   // Length('-127)
  dbfPhysicalSizeOfNumericInt16 = 6;   // Length('-23767')
  dbfPhysicalSizeOfNumericInt32 = 11;  // Length('-2147483647')
  dbfPhysicalSizeOfNumericInt64 = 20;  // Length('-9223372036854775807')

  dbfDefaultFloatSize = 14;
  dbfDefaultFloatDecimals = 4;

  dbfExtendedFloatSize = 18;
  dbfExtendedFloatDecimals = 8;

var
  Attributes: TFieldAttributes;

begin
//##JVR  Size := PFieldProps^.iUnits1;

  // Set a default fieldsize to trap errors
//##JVR  Word(PhysicalFieldProps.FieldSize) := 0;
  PhysicalFieldProps.FieldSize[0] := 0;
  PhysicalFieldProps.FieldSize[1] := 0;

  Attributes := PFieldAttributes(@(PFieldProps^.iFldAttr))^;
  if not (faRequired in Attributes) then begin
    PhysicalFieldProps.FieldFlags := [ffNullableField];
  end;

  case PFieldProps^.iFldType of
    fldTIMESTAMP: begin
      PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfTimeStamp;
      PhysicalFieldProps.FieldType := DT_DATETIME;
    end;  { fldTIMESTAMP }

    fldBLOB: begin
      PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfBlob;
      case PFieldProps^.iFldSubType of
        fldstMEMO: PhysicalFieldProps.FieldType := DT_MEMO;
      else
        PhysicalFieldProps.FieldType := DT_GENERAL;
      end;

      // Don't forget to update the header to indicate we have a memo file
      Include(FHeader.Flags, xfMemo);
    end;  { fldBLOB }

    fldZSTRING: begin
      Word(PhysicalFieldProps.FieldSize) := PFieldProps^.iUnits1;
      PhysicalFieldProps.FieldType := DT_CHARACTER;
    end;  { fldZSTRING }

    fldWIDESTRING, fldUNICODE: begin
      Word(PhysicalFieldProps.FieldSize) := PFieldProps^.iUnits1 div 2;
      PhysicalFieldProps.FieldType := DT_CHARACTER;
    end;  { fldWIDESTRING }

    fldDATE: begin
      PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfDate;
      PhysicalFieldProps.FieldType := DT_DATE;
    end;  { fldDATE }

    fldBOOL: begin
      PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfBoolean;
      PhysicalFieldProps.FieldType := DT_LOGICAL;
    end;  { fldBOOL }

{$ifdef DELPHI6}
    fldFMTBCD,
{$endif}
    fldBCD: begin
      PhysicalFieldProps.FieldSize[0] := PFieldProps^.iUnits1;
      PhysicalFieldProps.FieldSize[1] := PFieldProps^.iUnits2;
      PhysicalFieldProps.FieldType := DT_NUMERIC;
    end;  { fldBCD }

    fldSINGLE, fldFLOAT, fldFLOATIEEE: begin
      // If version indicates a visual foxpro file
      if (Version = xbVisualFoxpro) then begin
        // If the subtype indicates a fieldtype of Currency
        if (PFieldProps^.iFldSubType = fldstMONEY) then begin
          PhysicalFieldProps.FieldSize[0] := sizeofCurrency;
          if (PFieldProps^.iUnits2 > 0) then begin
            PhysicalFieldProps.FieldSize[1] := PFieldProps^.iUnits2;
          end
          else begin
            PhysicalFieldProps.FieldSize[1] := dbfDefaultFloatDecimals;
          end;
          PhysicalFieldProps.FieldType := DT_CURRENCY;
        end

        // If the DataType is of type Extended
        else if (PFieldProps^.iFldType = fldFLOATIEEE) then begin
          PhysicalFieldProps.FieldSize[0] := dbfExtendedFloatSize;
          PhysicalFieldProps.FieldSize[1] := dbfExtendedFloatDecimals;
          PhysicalFieldProps.FieldType := DT_FLOAT;
        end

        // Otherwise it is a standard floating point type
        else begin
          PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfDouble;
{$ifdef fpc}
        if PFieldProps^.iUnits2 > 0 then
{$endif}
          PhysicalFieldProps.FieldSize[1] := PFieldProps^.iUnits2;
          PhysicalFieldProps.FieldType := DT_DOUBLE;
        end;
      end

      // Otherwise use standard XBase (DbaseIII-plus/Foxbase) types
      else begin
        if (PFieldProps^.iUnits1 > 0) then begin
          PhysicalFieldProps.FieldSize[0] := PFieldProps^.iUnits1;
        end
        else begin
          PhysicalFieldProps.FieldSize[0] := dbfDefaultFloatSize;
        end;

        if (PFieldProps^.iUnits2 > 0) then begin
          PhysicalFieldProps.FieldSize[1] := PFieldProps^.iUnits2;
        end
        else begin
          PhysicalFieldProps.FieldSize[1] := dbfDefaultFloatDecimals;
        end;

        PhysicalFieldProps.FieldType := DT_FLOAT;
      end;  { if }
    end;  { fldFLOAT }

    fldINT8, fldUINT8: begin
      if (PFieldProps^.iUnits1 > 1) then begin
        PhysicalFieldProps.FieldSize[0] := PFieldProps^.iUnits1;
      end
      else begin
        PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfNumericInt08;
      end;

      PhysicalFieldProps.FieldType := DT_NUMERIC;
    end;  { fldINT8}

    fldINT16, fldUINT16: begin
      if (PFieldProps^.iUnits1 > 1) then begin
        PhysicalFieldProps.FieldSize[0] := PFieldProps^.iUnits1;
      end
      else begin
        PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfNumericInt16;
      end;

      PhysicalFieldProps.FieldType := DT_NUMERIC;
    end;  { fldINT16 }

    fldINT32, fldUINT32: begin
      // If version indicates a visual foxpro file
      if (Version = xbVisualFoxpro) then begin
        PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfInteger;
        PhysicalFieldProps.FieldType := DT_INTEGER;
      end

      // Otherwise use standard XBase (DbaseIII-plus/Foxbase) types
      else begin
        if (PFieldProps^.iUnits1 > 1) then begin
          PhysicalFieldProps.FieldSize[0] := PFieldProps^.iUnits1;
        end
        else begin
          PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfNumericInt32;
        end;

        PhysicalFieldProps.FieldType := DT_NUMERIC;
      end;  { if }
    end;  { fldINT32 }

    fldINT64, fldUINT64: begin
      if (PFieldProps^.iUnits1 > 1) then begin
        PhysicalFieldProps.FieldSize[0] := PFieldProps^.iUnits1;
      end
      else begin
        PhysicalFieldProps.FieldSize[0] := dbfPhysicalSizeOfNumericInt64;
      end;

      PhysicalFieldProps.FieldType := DT_NUMERIC;
    end;  { fldINT64 }

    fldBYTES: begin
      //##NULLFLAGS -  Is (xsShowNullFlags in FOptions) important here or not?
     if DBICompareText(PFieldProps^.szName, FieldName_NullFlags) = 0 then begin
        PhysicalFieldProps.FieldSize[0] := PFieldProps^.iUnits1;
        PhysicalFieldProps.FieldType := DT_NULLFLAGS;
        PhysicalFieldProps.FieldFlags := [ffHiddenField];
      end;
    end;  { fldBYTES }

  else
    // Do nothing
  end;  { case }

  if (Word(PhysicalFieldProps.FieldSize) <= 0) then begin
    raise EDBIException.Create(Self, 'GetPhysicalProperties::2005',
      'Unable to map Data Type "%d" correctly',
      [PFieldProps^.iFldType]
      );
  end;
end;  { GetPhysicalProperties }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 12:07:12 - Initial code<BR>
  Jvr - 14/02/2003 15:07:15 - Added limited support for extended fieldnames<P>
}
procedure TDBIXbaseDataConnection.CreateDS(
  iFields: LongWord;
  pFldDes: pDSFLDDesc;
  pszName: TDBINameBuffer
  );

var
  FieldNo: Integer;
  PhysicalFieldOffset: LongWord;
  FieldData: TDBIFieldData;
  FieldName: TDBIString;
  FieldSize: Word;

begin
  // Call inherited to set up logical field properties
  inherited CreateDS(iFields, pFldDes, pszName);

  // Setup Physical properties
  try
    { TODO 2 -oJvr -cTDBIXbaseDataConnection.CreateDS() :
      08/11/2000 12:07:12
      File format version should NOT be hard coded
    }
    FHeader.Version := DSVer_VisualFoxpro;  // Header Byte: $30 = Visual Foxpro

//   Version := DSVer_Foxpro2;              // FoxPro 2.x (or earlier with memo)
//   Version := DSVer_FoxBasePlus_dBaseIII; // FoxBase+ & DBASE-III, no memo
//   Version := DSVer_Foxpro2;              // FoxPro 2.x (or earlier with memo)
//   Version := DSVer_dbaseIV_Memo;         // dBase-IV SQL System files, with memo
//   Version := DSVer_FoxBase_Memo;         // FoxBase

    { TODO 2 -oJvr -cTDBIXbaseDataConnection.CreateDS() :
      14/02/2003 12:25:09
      File format language should NOT be hard coded
    }
    Language := cpWindowsANSI;              // Windows Ansi codepage

    DataOffset := ((FieldCount + 1) * SizeOf(TDBIXbaseField)) + XbaseFoxProDataOffset;
    ModificationDate := Now;                // New file --> Todays Date
    FHeader.RecordCount := 0;               // New file --> No records

    // Reset Header Flags property
    Exclude(FHeader.Flags, xfCompoundIndex);
    Exclude(FHeader.Flags, xfMemo);
    Exclude(FHeader.Flags, xfDatabaseContainer);

    // Delete Flags starts at offset 0, the first field starts at offset 1
    PhysicalFieldOffset := 1;

    SetLength(FFields, FieldCount);
    for FieldNo := 0 to FieldCount - 1 do begin
      // Initialize the FFields[FieldNo] record
      FillChar(FFields[FieldNo], SizeOf(TDBIXbaseField), #0);

      FieldName := TDBIString(FieldProps[FieldNo].szName);
      FieldSize := SizeOf(FFields[FieldNo].FieldName)-1;

      // If use Strict Foxpro/Dbase Conventions then
      // make sure the fieldnames are upper-case
      if (xsStrictFieldNames in FOptions) then begin
        FieldName := {$ifdef Delphi2009}AnsiStrings.{$endif}UpperCase(FieldName);
      end;

      FieldData := PDBIChar(FieldName);

      {$ifdef DelphiXE4}AnsiStrings.{$endif}StrLCopy(
        FFields[FieldNo].FieldName,             // Target
        FieldData,                              // Source
        FieldSize                               // MaxLen
        );

      if (xfExtendedFields in Flags) and (Length(FieldName) > FieldSize) then begin
        Inc(FieldData, FieldSize);

        {$ifdef DelphiXE4}AnsiStrings.{$endif}StrLCopy(
          FFields[FieldNo].FieldData,           // Target
          FieldData,                            // Source
          SizeOf(FFields[FieldNo].FieldData)    // MaxLen
          );
      end;

      GetPhysicalProperties(@FieldProps[FieldNo], FFields[FieldNo]);

      // Calculate the Field Offsets & RecordSize
      FFields[FieldNo].FieldOffset := PhysicalFieldOffset;
      if (FFields[FieldNo].FieldType = DT_CHARACTER) then begin
        PhysicalFieldOffset := PhysicalFieldOffset + Word(FFields[FieldNo].FieldSize);
      end
      else begin
        PhysicalFieldOffset := PhysicalFieldOffset + FFields[FieldNo].FieldSize[0];
      end;
    end;  { for }

    { TODO 5 -oJvr -cTDBIXbaseDataConnection.CreateDS() :
      I'm not sure but maybe we are not setting all we need to in the Physical Header
    }
    FHeader.RecordSize := PhysicalFieldOffset;    // Sum(F1.Size...Fn.Size)

{##NULLFLAGS
    for FieldNo := 0 to FieldCount-1 do begin
      if FieldNo >= (FieldCount-1) then begin
        FFields[FieldNo].FieldFlags := [ffHiddenField];
      end
      else begin
        FFields[FieldNo].FieldFlags := [ffNullableField];
      end;
    end;
//}
    New;

  finally
    // Now Close it
    Close;
  end;  { try..finally }
end;  { CreateDS }


// _____________________________________________________________________________
{**
  Translate from the Physical data buffer to the Logical data buffer

  Jvr - 06/11/2000 12:49:48.<BR>
  Jvr - 10/05/2001 12:13:55 - Added null value support.<P>
}
function TDBIXbaseDataConnection.GetData(const PhysicalBuffer; var Buffer): DSAttr;
var
  FieldNo: Integer;
  LogicalOffset: Integer;
  PhysicalOffset: LongWord;
  GetFieldProc: TDBIXbaseGetFieldProc;
  IsBlank: Boolean;
  pRecBuf: TDBIRecordBuffer;
  pFldBuf: TDBIFieldBuffer;

begin
  Result := dsRecUnmodified;

  // Initialise Buffer (Logical Size)
  FillChar(Buffer, LogicalBufferSize, #0);

  // Setup the NullFlags buffer if this dataset supports it
  if NullFlags.IsNullable then begin
    FieldNo := FieldCount - 1;
    LogicalOffset := FieldProps[FieldNo].iFldOffsInRec;
    pFldBuf := @(TDBIRecordBuffer(@Buffer)[LogicalOffset]);
    NullFlags.SetBuffer(pFldBuf, FieldProps[FieldNo].iFldLen);
  end;

  // Iterate through the fields in REVERSE ORDER and get the data
  for FieldNo := FieldCount - 1 downto 0 do begin
    PhysicalOffset := LongWord(FFields[FieldNo].FieldOffset);
    LogicalOffset := FieldProps[FieldNo].iFldOffsInRec;
    GetFieldProc := GetFieldValue;

    // If type is a specific foxpro type
    case FFields[FieldNo].FieldType of
      DT_INTEGER:    GetFieldProc := GetFieldIntegerFP;
      DT_DOUBLE:     GetFieldProc := GetFieldFloatFP;
      DT_CURRENCY:   GetFieldProc := GetFieldCurrency;
      DT_MEMO:       GetFieldProc := GetFieldMemo;
      DT_DELETED:    GetFieldProc := GetFieldDeleted;
      DT_DATETIME:   GetFieldProc := GetFieldDateTime;
      DT_NULLFLAGS:  GetFieldProc := GetFieldNullFlags;

    // Otherwise use the Delphi types
    else
      case FieldProps[FieldNo].iFldType of
        fldUNKNOWN:          GetFieldProc := GetFieldUnknown;
        fldZSTRING:          GetFieldProc := GetFieldString;
        fldDATE:             GetFieldProc := GetFieldDate;
        fldBLOB:             GetFieldProc := GetFieldUnknown;
        fldBOOL:             GetFieldProc := GetFieldBoolean;
        fldINT16:            GetFieldProc := GetFieldInteger;
        fldINT32:            GetFieldProc := GetFieldInteger;
        fldFLOAT:            GetFieldProc := GetFieldFloat;
        fldBCD:              GetFieldProc := GetFieldBCD;
        fldBYTES:            GetFieldProc := GetFieldUnknown;
        fldTIME:             GetFieldProc := GetFieldUnknown;
        fldTIMESTAMP:        GetFieldProc := GetFieldDateTime;
        fldUINT16:           GetFieldProc := GetFieldInteger;
        fldUINT32:           GetFieldProc := GetFieldInteger;
        fldFLOATIEEE:        GetFieldProc := GetFieldFloat;
        fldVARBYTES:         GetFieldProc := GetFieldUnknown;
        fldLOCKINFO:         GetFieldProc := GetFieldUnknown;
        fldCURSOR:           GetFieldProc := GetFieldUnknown;
        fldINT64:            GetFieldProc := GetFieldInteger;
        fldUINT64:           GetFieldProc := GetFieldInteger;
        fldADT:              GetFieldProc := GetFieldUnknown;
        fldARRAY:            GetFieldProc := GetFieldUnknown;
        fldREF:              GetFieldProc := GetFieldUnknown;
        fldTABLE:            GetFieldProc := GetFieldUnknown;
        fldDATETIME:         GetFieldProc := GetFieldUnknown;
        fldFMTBCD:           GetFieldProc := GetFieldBCD;
        fldWIDESTRING:       GetFieldProc := GetFieldString;
        fldUNICODE:          GetFieldProc := GetFieldString;
        fldINT8:             GetFieldProc := GetFieldInteger;
        fldUINT8:            GetFieldProc := GetFieldInteger;
        fldSINGLE:           GetFieldProc := GetFieldFloat;
        fldDATETIMEOFFSET:   GetFieldProc := GetFieldUnknown;
      else
        GetFieldProc := GetFieldUnknown;
      end;  { case }
    end;  { case }

    { TODO 5 -oJvr -cTDBIXbaseDataConnection.GetData() :
      NullFlags, Check for null value(s)
    }
    IsBlank := (FFields[FieldNo].FieldType <> DT_NULLFLAGS) and NullFlags.IsNull[FieldNo];
    if not IsBlank then begin
      // Setup Record Buffer
      pRecBuf := @(TDBIRecordBuffer(@PhysicalBuffer)[PhysicalOffset]);

      // Setup FieldBuffer
      pFldBuf := @(TDBIRecordBuffer(@Buffer)[LogicalOffset]);

      IsBlank := not GetFieldProc(pRecBuf^, pFldBuf, FieldNo);
    end;  { if }

    { TODO 5 -oJvr -cTXBaseDBIDataConnection.GetData() :
      Somewhere in here we need to deal with the deleted field
      and act accordingly. The "StatusFilter: TUpdateStatus" property in the
      TDBIDataset should determine if the deleted records should be displayed
      or not.  Another property should determine if the actual Deleted Field
      should be hidden or shown.
    }

    // Set the Null-Flag for this field, in the logical buffer, to the 'IsBlank' value
    if (FFields[FieldNo].FieldType <> DT_NULLFLAGS) then begin
      Byte(TDBIRecordBuffer(@Buffer)[FieldProps[FieldNo].iNullOffsInRec]) := Ord(IsBlank);
    end;
  end;  { for }
end;  { GetData }


// _____________________________________________________________________________
{**
  Translate from the Logical data buffer to the Physical data buffer.

  Jvr - 05/02/2001 12:48:45.<P>
}
procedure TDBIXbaseDataConnection.PutData(var PhysicalBuffer; const Buffer);
var
  FieldNo: Integer;
  LogicalOffset: Integer;
  PhysicalOffset: LongWord;
  PutFieldProc: TDBIXbasePutFieldProc;
  pRecBuf: TDBIRecordBuffer;
  pFldBuf: TDBIFieldBuffer;
  IsBlank: Boolean;

begin
  // Clear Physical buffer
  FillChar(PhysicalBuffer, FHeader.RecordSize, #0);

  // Setup the NullFlags buffer if this dataset supports it
  if NullFlags.IsNullable then begin
    FieldNo := FieldCount - 1;
    LogicalOffset := FieldProps[FieldNo].iFldOffsInRec;
    pFldBuf := @(TDBIRecordBuffer(@Buffer)[LogicalOffset]);
    NullFlags.SetBuffer(pFldBuf, FieldProps[FieldNo].iFldLen);
  end;

  // Update Xbase field data from logical field buffers
  for FieldNo := 0 to FieldCount - 1 do begin
    PhysicalOffset := LongWord(FFields[FieldNo].FieldOffset);
    LogicalOffset := FieldProps[FieldNo].iFldOffsInRec;
    PutFieldProc := PutFieldValue;

    // If type is a specific foxpro type
    case FFields[FieldNo].FieldType of
      DT_INTEGER:    PutFieldProc := PutFieldIntegerFP;
      DT_DOUBLE:     PutFieldProc := PutFieldFloatFP;
      DT_CURRENCY:   PutFieldProc := PutFieldCurrency;
      DT_MEMO:       PutFieldProc := PutFieldMemo;
      DT_DELETED:    PutFieldProc := PutFieldDeleted;
      DT_NULLFLAGS:  PutFieldProc := PutFieldNullFlags;

    // Otherwise use the Xbase types
    else
      case FieldProps[FieldNo].iFldType of
        fldUNKNOWN:          PutFieldProc := PutFieldUnknown;
        fldZSTRING:          PutFieldProc := PutFieldString;
        fldDATE:             PutFieldProc := PutFieldDate;
        fldBLOB:             PutFieldProc := PutFieldUnknown;
        fldBOOL:             PutFieldProc := PutFieldBoolean;
        fldINT16:            PutFieldProc := PutFieldInteger;
        fldINT32:            PutFieldProc := PutFieldInteger;
        fldFLOAT:            PutFieldProc := PutFieldFloat;
        fldBCD:              PutFieldProc := PutFieldBCD;
        fldBYTES:            PutFieldProc := PutFieldUnknown;
        fldTIME:             PutFieldProc := PutFieldUnknown;
        fldTIMESTAMP:        PutFieldProc := PutFieldDateTime;
        fldUINT16:           PutFieldProc := PutFieldInteger;
        fldUINT32:           PutFieldProc := PutFieldInteger;
        fldFLOATIEEE:        PutFieldProc := PutFieldFloat;
        fldVARBYTES:         PutFieldProc := PutFieldUnknown;
        fldLOCKINFO:         PutFieldProc := PutFieldUnknown;
        fldCURSOR:           PutFieldProc := PutFieldUnknown;
        fldINT64:            PutFieldProc := PutFieldInteger;
        fldUINT64:           PutFieldProc := PutFieldInteger;
        fldADT:              PutFieldProc := PutFieldUnknown;
        fldARRAY:            PutFieldProc := PutFieldUnknown;
        fldREF:              PutFieldProc := PutFieldUnknown;
        fldTABLE:            PutFieldProc := PutFieldUnknown;
        fldDATETIME:         PutFieldProc := PutFieldUnknown;
        fldFMTBCD:           PutFieldProc := PutFieldBCD;
        fldWIDESTRING:       PutFieldProc := PutFieldString;
        fldINT8:             PutFieldProc := PutFieldInteger;
        fldUINT8:            PutFieldProc := PutFieldInteger;
        fldSINGLE:           PutFieldProc := PutFieldFloat;
        fldDATETIMEOFFSET:   PutFieldProc := PutFieldUnknown;
        fldUNICODE:          PutFieldProc := PutFieldString;
      else
        PutFieldProc := PutFieldUnknown;
      end;  { case }
    end;  { case }


    // Is this field blank?
    IsBlank :=
      (FFields[FieldNo].FieldType <> DT_NULLFLAGS) and
      Boolean(Byte(TDBIRecordBuffer(@Buffer)[FieldProps[FieldNo].iNullOffsInRec]));

    // Setup Record Buffer
    pRecBuf := @(TDBIRecordBuffer(@PhysicalBuffer)[PhysicalOffset]);

    // Setup FieldBuffer
    if IsBlank then begin
      pFldBuf := nil;
    end
    else begin
      pFldBuf := @(TDBIRecordBuffer(@Buffer)[LogicalOffset]);
    end;

    // Set the '_NullFlags' bit for this field to the 'IsBlank' value
    if (FFields[FieldNo].FieldType <> DT_NULLFLAGS) then begin
      NullFlags.IsNull[FieldNo] := IsBlank;
    end;

    // Call field conversion proceedure
    PutFieldProc(pRecBuf^, pFldBuf, FieldNo);
  end;  { for }
end;  { PutData }


// _____________________________________________________________________________
{**
  Jvr - 24/06/2005 14:36:48 - Initial code.<br>
}
function TDBIXbaseDataConnection.WriteData(
  const APosition: Integer;
  var APhysicalBuffer;
  ASize: Integer
  ): LongInt;
var
  RecordOffset: Longint;
  BytesRead: Longint;

begin
  case APosition of
    posBof: RecordOffset := 0;
    posEof: RecordOffset := FHeader.RecordSize * Longint(FHeader.RecordCount);
  else
    RecordOffset := FHeader.RecordSize * APosition;
  end;
  DataStream.Seek(FHeader.DataOffset + RecordOffset, soFromBeginning);

  if (ASize = -1) then begin
    ASize := FHeader.RecordSize;
  end;

  BytesRead := DataStream.Write(APhysicalBuffer, ASize);
  Modified := True;
  Dirty := True;

  if (BytesRead <> ASize) then begin
    raise EDBIException.Create(Self, 'WriteData::2405', SWriteDataFailed, []);
  end;

  if (APosition = posEof) then begin
    FHeader.RecordCount := FHeader.RecordCount + 1;
    Result := FHeader.RecordCount;
  end
  else begin
    Result := APosition;
  end;
end;  { WriteData }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:26:48<P>
}
function TDBIXbaseDataConnection.GetFieldAsString(
  const RecordBuffer;
  FieldNo: Word;
  var HasValidData: Boolean
  ): TDBIString;
begin
  // Field has value - not blank
  { TODO 5 -oJvr -cTDBIXbaseDataConnection.GetFieldAsString() :
    HasValidData := (@RecordBuffer <> nil) and (TDBIRecordBuffer(@RecordBuffer)^ <> #0);
  }
  { TODO 5 -oJvr -cTDBIXbaseDataConnection.GetFieldAsString() :
    Probably should check to see if the data is valid before proceeding
  }
  SetLength(Result, FFields[FieldNo].FieldSize[0]);
  Move(RecordBuffer, TDBIRecordBuffer(Result)^, FFields[FieldNo].FieldSize[0]);

  // Check to see if the field is blank
  Result := TDBIAnsi.TrimRight(Result);
  HasValidData := Result <> '';
end;  { GetFieldAsString }


// _____________________________________________________________________________
{**
  Jvr - 29/11/2000 18:28:11<P>
}
function TDBIXbaseDataConnection.GetFieldAsPChar(
  RecordBuffer: TDBIRecordBuffer;
  FieldNo: Word
  ): Boolean;

  procedure RightTrimSpaces(PDataString: TDBIRecordBuffer; Count: Integer);
  var
    Buffer: TDBIRecordBuffer;

  begin
    Buffer := PDataString;
    Inc(Buffer, Count - 1);

    while (Buffer^ = TDBIRecordElement(' ')) do begin
      Buffer^ := TDBIRecordElement(0);
      Dec(Buffer, 1);
    end;
  end;  { StrRightTrim }

begin
  RightTrimSpaces(RecordBuffer, FFields[FieldNo].FieldSize[0]);
  Result := RecordBuffer^ <> TDBIRecordElement(0);
end;  { GetFieldAsPChar }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:27:11<P>
}
function TDBIXbaseDataConnection.{%H-}GetFieldUnknown(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  DataTypeName: String;

begin
  try
    try
      DataTypeName := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(DataTypeMap[FieldProps[FieldNo].iFldType]));
    except
      DataTypeName := Format('%d', [FieldProps[FieldNo].iFldType]);
    end;
  finally
    raise EDBIException.Create(Self, 'GetFieldUnknown::2490',
      'Field: %s, Unknown data type - "%s" Not supported',
      [FieldProps[FieldNo].szName, DataTypeName]
      );
  end;
end;  { GetFieldUnKnown }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:29:17<P>
}
procedure TDBIXbaseDataConnection.PutFieldUnknown(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
var
  DataTypeName: String;

begin
  try
    try
      DataTypeName := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(DataTypeMap[FieldProps[FieldNo].iFldType]));
    except
      DataTypeName := Format('%d', [FieldProps[FieldNo].iFldType]);
    end;
  finally
    raise EDBIException.Create(Self, 'PutFieldUnknown::2520',
      'Field: %s, Unknown data type - "%s" Not supported',
      [FieldProps[FieldNo].szName, DataTypeName]
      );
  end;
end;  { PutFieldUnKnown }


// _____________________________________________________________________________
{**
  Jvr - 08/05/2001 11:59:05 - Generic binary Get Field Value routine.
                              <B>Note:</B><BR>
                              Physical field length needs to be the same as
                              the Logical field length.<P>
}
function TDBIXbaseDataConnection.GetFieldValue(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
begin
  // Field has value - not blank
  Result := True;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  Move(RecordBuffer, FieldBuffer^, FieldProps[FieldNo].iFldLen);
end;  { GetFieldValue }


// _____________________________________________________________________________
{**
  Jvr - 08/05/2001 12:01:46 - Generic binary Put Field Value routine.
                              <B>Note:</B><BR>
                              Physical field length needs to be the same as
                              the Logical field length.<P>
}
procedure TDBIXbaseDataConnection.PutFieldValue(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
begin
  if (FieldBuffer = nil) then Exit;

  Move(FieldBuffer^, RecordBuffer, FieldProps[FieldNo].iFldLen);
end;  { PutFieldValue }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 14:00:02 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<BR>
  Jvr - 29/10/2002 12:37:51 - Optimised the trim trailing blanks code.<P>
}
function TDBIXbaseDataConnection.GetFieldString(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  PRecBuf: Pointer;
  PData: TDBIRecordBuffer;
  DataString: AnsiString;
  Size: Integer;

begin
  // We assume that the data in the xbase file is Ansi (not Unicode)
  PData := @RecordBuffer;
  Size := FFields[FieldNo].FieldSize[0];

  // Trim the buffer of trailing blanks
  // This section of code as been optimised, please don't touch it (Jvr)
  if not (xsStrictFieldValues in FOptions) then begin
    // Move pointer to the end of the field data
    Inc(PData, Size - 1);

    if PData^ = TDBIRecordElement(' ') then begin
      // Right trim the data in the buffer if 'xsStrictFieldValues' is not set.
      while (PData^ = TDBIRecordElement(' ')) and (PData >= TDBIRecordBuffer(@RecordBuffer)) do begin
        Dec(PData, SizeOf(TDBIRecordElement));
        Dec(Size);
      end;  { while }
    end;
  end;  { if }


  // Do we have valid data?
  Result := (Size <> 0);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;


  if (FieldProps[FieldNo].iFldType = fldWideString) or (FieldProps[FieldNo].iFldType = fldUNICODE) then begin
    DataString := AnsiString(PAnsiChar(@RecordBuffer));
    SetLength(DataString, Size);

    PRecBuf := PWideChar(WideString(DataString));
    Size := Size * 2;
  end
  else {IsAnsiString} begin
    PRecBuf := @RecordBuffer;
  end;

  // Assign data to field buffer
  Move(PRecBuf^, FieldBuffer^, Size);
end;  { GetFieldString }


// _____________________________________________________________________________
{**
  Jvr - 02/10/2002 19:02:59 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
procedure TDBIXbaseDataConnection.PutFieldString(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
var
  DataString: AnsiString;

begin
  // We assume that the data in the xbase file is Ansi (not Unicode)
  // Pad the buffer out
  FillChar(RecordBuffer, FFields[FieldNo].FieldSize[0], Ord(' '));

  // Only copy the text (excluding any null terminator)
  if (FieldBuffer <> nil) then begin
    case FieldProps[FieldNo].iFldType of
      fldWideString, fldUNICODE: DataString := AnsiString(WideString(PWideChar(FieldBuffer)));
    else
      DataString := AnsiString(PAnsiChar(FieldBuffer));
    end;
  end;
  Move(PAnsiChar(DataString)^, RecordBuffer, Length(DataString));
end;  { PutFieldString }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 14:01:41 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
function TDBIXbaseDataConnection.GetFieldBoolean(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
const
  T1 = TDBIRecordElement('T');
  T2 = TDBIRecordElement('t');
  T3 = TDBIRecordElement('Y');
  T4 = TDBIRecordElement('y');

var
  PData: TDBIRecordBuffer;

begin
  PData := @RecordBuffer;
  Result := (PData^ <> TDBIRecordElement(' ')) and (PData^ <> TDBIRecordElement('?'));

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  // Assign data to field buffer
  if TDBIRecordElement(PData^) in [T1, T2, T3, T4] then begin
    Boolean(FieldBuffer^) := True;
  end
  else begin
    Boolean(FieldBuffer^) := False;
  end;
end;  { GetFieldBoolean }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 14:02:21 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
procedure TDBIXbaseDataConnection.PutFieldBoolean(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
var
  PData: TDBIRecordBuffer;

begin
  PData := @RecordBuffer;

  if (FieldBuffer = nil) then begin
    PData^ := TDBIRecordElement('?');
  end
  else if (Boolean(FieldBuffer^) = True) then begin
    PData^ := TDBIRecordElement('T');
  end
  else begin
    PData^ := TDBIRecordElement('F');
  end;
end;  { PutFieldBoolean }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 14:03:08 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
function TDBIXbaseDataConnection.GetFieldFloat(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  PData: TDBIRecordBuffer;
  Size: Integer;
  ConvBuff: array[0..32] of TDBIRecordElement;  // Numeric fields can be no larger than 20 chars
  FloatValue: Extended;

begin
  PData := @RecordBuffer;
  Size := FFields[FieldNo].FieldSize[0];

  // Move pointer to first valid Ansichar of data
  while (PData^ = TDBIRecordElement(' ')) and (Size > 0) do begin
    Inc(PData, 1);
    Dec(Size);
  end;

  // Do we have valid data?
  Result := (Size <> 0) and (PData^ <> TDBIRecordElement('*'));

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  // Assign data to conversion buffer
  Move(PData^, ConvBuff{%H-}, Size);
  // Make sure the data is null terminated for the conversion routine
  ConvBuff[Size] := TDBIRecordElement(0);
  // Convert the dbase numeric type to an extended floating point value
{$ifdef DELPHIXE2}
  Result := DBITextToFloat(@ConvBuff[0], FloatValue, fvExtended);
{$else}
  Result := TextToFloat(@ConvBuff[0], FloatValue, fvExtended);
{$endif}

  // Assign Floating point value to the Field Buffer
  case FieldProps[FieldNo].iFldType of
    fldSingle:    PSingle(FieldBuffer)^ := FloatValue;
    fldFloat:     PDouble(FieldBuffer)^ := FloatValue;
    fldFLOATIEEE: PExtended(FieldBuffer)^ := FloatValue;
  end;
end;  { GetFieldFloat }


// _____________________________________________________________________________
{**
  Jvr - 03/10/2002 11:32:40 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
procedure TDBIXbaseDataConnection.PutFieldFloat(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
var
  FloatValue: Double;

begin
  if (FieldBuffer = nil) then begin
    FillChar(RecordBuffer, FFields[FieldNo].FieldSize[0], Ord(' '));
  end
  else begin
    case FieldProps[FieldNo].iFldType of
      fldSingle:    FloatValue := PSingle(FieldBuffer)^;
      fldFloat:     FloatValue := PDouble(FieldBuffer)^;
      fldFLOATIEEE: FloatValue := PExtended(FieldBuffer)^;
    else
      FloatValue := 0.00;
    end;

    TDBIAnsi.FormatBuf(RecordBuffer, FFields[FieldNo].FieldSize[0], '%*.*f',
      [
        FFields[FieldNo].FieldSize[0],
        FFields[FieldNo].FieldSize[1],
        FloatValue
        ]
      );
  end;  { if }
end;  { PutFieldFloat }


// _____________________________________________________________________________
{**
  Jvr - 05/02/2001 12:31:27.<P>
}
function TDBIXbaseDataConnection.GetFieldFloatFP(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
begin
  // Do we have valid data?
  Result := True;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  // Assign Floating point value to the Field Buffer
  case FieldProps[FieldNo].iFldType of
    fldSingle:    PSingle(FieldBuffer)^ := Double(RecordBuffer);
    fldFloat:     PDouble(FieldBuffer)^ := Double(RecordBuffer);
    fldFLOATIEEE: PExtended(FieldBuffer)^ := Double(RecordBuffer);
  end;
end;  { GetFieldFloatFP }


// _____________________________________________________________________________
{**
  Jvr - 05/02/2001 12:54:32.<P>
}
procedure TDBIXbaseDataConnection.PutFieldFloatFP(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
begin
  // FieldType = DT_DOUBLE, Size = 8, Decimals = 0
  if (FieldBuffer = nil) then begin
    Double(RecordBuffer) := 0;
  end
  else begin
    case FieldProps[FieldNo].iFldType of
      fldSingle:    Double(RecordBuffer) := PSingle(FieldBuffer)^;
      fldFloat:     Double(RecordBuffer) := PDouble(FieldBuffer)^;
      fldFLOATIEEE: Double(RecordBuffer) := PExtended(FieldBuffer)^;
    else
      Double(RecordBuffer) := 0.00;
    end;
  end;
end;  { PutFieldFloatFP }


// _____________________________________________________________________________
{**
  Jvr - 10/05/2001 15:44:45.<P>
}
function TDBIXbaseDataConnection.GetFieldCurrency(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
begin
  // Field has value - not blank
  Result := True;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  PDouble(FieldBuffer)^ := Int64(RecordBuffer) / 10000;
end;  { GetFieldCurrency }


// _____________________________________________________________________________
{**
  Jvr - 10/05/2001 15:44:51.<P>
}
procedure TDBIXbaseDataConnection.PutFieldCurrency(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
begin
  if (FieldBuffer <> nil) then begin
    // FieldType = DT_CURRENCY, Size = 8, Decimals = 0
    Int64(RecordBuffer) := Round(PDouble(FieldBuffer)^ * 10000);
  end;
end;  { PutFieldCurrency }


// _____________________________________________________________________________
{**
  Jvr - 16/09/2002 11:39:54.<P>
}
function TDBIXbaseDataConnection.GetFieldBCD(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  PData: TDBIRecordBuffer;
  Size: Integer;
  ConvBuff: array[0..32] of TDBIRecordElement;  // Numeric fields can be no larger than 20 Chars
  CurrencyValue: Currency;

begin
  PData := @RecordBuffer;
  Size := FieldProps[FieldNo].iUnits1;

  // Move pointer to first valid Ansichar of data
  while (PData^ = TDBIRecordElement(' ')) and (Size > 0) do begin
    Inc(PData, 1);
    Dec(Size);
  end;

  // Do we have valid data?
  Result := (Size <> 0) and (PData^ <> TDBIRecordElement('*'));

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  // Assign data to conversion buffer
  Move(PData^, ConvBuff{%H-}, Size);
  // Make sure the data is null terminated for the conversion routine
  ConvBuff[Size] := TDBIRecordElement(0);

  // Convert the dbase numeric type to an extended floating point value
{$ifdef DELPHIXE2}
  Result := DBITextToFloat(@ConvBuff[0], CurrencyValue, fvCurrency);
{$else}
  Result := TextToFloat(@ConvBuff[0], CurrencyValue, fvCurrency);
{$endif}

  // Assign Floating point value to the Field Buffer
{$ifdef fpc}
  PCurrency(FieldBuffer)^ := CurrencyValue;
{$else}
  CurrToBCD(CurrencyValue, PBcd(FieldBuffer)^);
{$endif}
end;  { GetFieldBCD }


// _____________________________________________________________________________
{**
  Jvr - 16/09/2002 11:47:19 - Initial code
  Jvr - 03/10/2002 11:45:59 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
procedure TDBIXbaseDataConnection.PutFieldBCD(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
var
  CurrencyValue: Currency;

begin
  if (FieldBuffer = nil) then begin
    FillChar(RecordBuffer, FFields[FieldNo].FieldSize[0], Ord(' '));
  end
  else begin
{$ifdef fpc}
    CurrencyValue := PCurrency(FieldBuffer)^;
{$else}
    BcdToCurr(PBcd(FieldBuffer)^, CurrencyValue);
{$endif}

    TDBIAnsi.FormatBuf(RecordBuffer, FieldProps[FieldNo].iUnits1, '%*.*f',
      [
        FieldProps[FieldNo].iUnits1,
        FieldProps[FieldNo].iUnits2,
        CurrencyValue
        ]
      );

{$ifdef DBDebug}
    ShowMessageFmt(
      'StrValue = %s, DoubleValue = %10.10f, BCDPrecision = %d, BCDScale = %d, Units1 = %d, Units2 = %d', [
       String(PAnsiChar(@RecordBuffer)),
       FloatValue,
       BCDPrecision(TBcd(FieldBuffer^)),
       BcdScale(TBcd(FieldBuffer^)),
       FieldProps[FieldNo].iUnits1,
       FieldProps[FieldNo].iUnits2
      ]);
{$endif DBDebug}
  end;  { if }
end;  { PutFieldBCD }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 14:04:01 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
function TDBIXbaseDataConnection.GetFieldInteger(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  PData: TDBIRecordBuffer;
  Size: Integer;
  ConvBuff: array[0..32] of AnsiChar;  // Numeric fields can be no larger than 20 chars
  Code: Integer;

begin
  PData := @RecordBuffer;
  Size := FFields[FieldNo].FieldSize[0];

  // Move pointer to first valid char of data
  while (PData^ = TDBIRecordElement(' ')) and (Size > 0) do begin
    Inc(PData, 1);
    Dec(Size);
  end;

  // Do we have valid data?
  Result := (Size <> 0) and (PData^ <> TDBIRecordElement('*'));

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  // Assign data to conversion buffer
  Move(PData^, ConvBuff{%H-}, Size);
  // Make sure the data is null terminated for the conversion routine
  ConvBuff[Size] := #0;

  // Convert the dbase numeric type to an integer value and assign to Field Buffer
  case FieldProps[FieldNo].iFldType of
    fldINT8:   Val(TDBINumberString(ConvBuff), PShortInt(FieldBuffer)^, Code);
    fldUINT8:  Val(TDBINumberString(ConvBuff), PByte(FieldBuffer)^, Code);

    fldINT16:  Val(TDBINumberString(ConvBuff), PSmallInt(FieldBuffer)^, Code);
    fldUINT16: Val(TDBINumberString(ConvBuff), PWord(FieldBuffer)^, Code);

    fldINT32:  Val(TDBINumberString(ConvBuff), PLongInt(FieldBuffer)^, Code);
    fldUINT32: Val(TDBINumberString(ConvBuff), PLongWord(FieldBuffer)^, Code);

    fldINT64:  Val(TDBINumberString(ConvBuff), PInt64(FieldBuffer)^, Code);
    fldUINT64: Val(TDBINumberString(ConvBuff), PInt64(FieldBuffer)^, Code);
  end;

  Result := Code = 0;
end;  { GetFieldInteger }


// _____________________________________________________________________________
{**
  Jvr - 03/10/2002 13:27:00 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
procedure TDBIXbaseDataConnection.PutFieldInteger(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
const
  Mask = '%*d';

var
  Size: Word;

begin
  Size := FFields[FieldNo].FieldSize[0];

  if (FieldBuffer = nil) then begin
    FillChar(RecordBuffer, FFields[FieldNo].FieldSize[0], Ord(' '));
  end
  else begin
    // FieldType = DT_NUMERIC, Decimals = 0
    case FieldProps[FieldNo].iFldType of
      fldINT8: begin
        TDBIAnsi.FormatBuf(RecordBuffer, Size, Mask, [Size, PShortInt(FieldBuffer)^]);
      end;  { ShortInt }

      fldUINT8: begin
        TDBIAnsi.FormatBuf(RecordBuffer, Size, Mask, [Size, PByte(FieldBuffer)^]);
      end;  { Byte }

      fldINT16: begin
        TDBIAnsi.FormatBuf(RecordBuffer, Size, Mask, [Size, PSmallInt(FieldBuffer)^]);
      end;  { SmallInt }

      fldUINT16: begin
        TDBIAnsi.FormatBuf(RecordBuffer, Size, Mask, [Size, PWord(FieldBuffer)^]);
      end;  { Word }

      fldINT32: begin
        TDBIAnsi.FormatBuf(RecordBuffer, Size, Mask, [Size, PLongInt(FieldBuffer)^]);
      end;  { LongInt }

      fldUINT32: begin
        TDBIAnsi.FormatBuf(RecordBuffer, Size, Mask, [Size, PLongWord(FieldBuffer)^]);
      end;  { LongWord }

      fldINT64, fldUINT64: begin
        TDBIAnsi.FormatBuf(RecordBuffer, Size, Mask, [Size, PInt64(FieldBuffer)^]);
      end;  { Int64 }
    end;
  end;  { if }
end;  { PutFieldInteger }


// _____________________________________________________________________________
{**
  Jvr - 05/02/2001 12:17:43.<P>
}
function TDBIXbaseDataConnection.GetFieldIntegerFP(const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer; FieldNo: Word): Boolean;
begin
  // Do we have valid data?
  Result := True;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  case FieldProps[FieldNo].iFldType of
    fldINT8, fldUINT8:   PShortInt(FieldBuffer)^ := LongInt(RecordBuffer);
    fldINT16, fldUINT16: PSmallInt(FieldBuffer)^ := LongInt(RecordBuffer);
    fldINT32, fldUINT32: PLongInt(FieldBuffer)^ := LongInt(RecordBuffer);
    fldINT64, fldUINT64: PInt64(FieldBuffer)^ := LongInt(RecordBuffer);
  end;
end;  { GetFieldIntegerFP }


// _____________________________________________________________________________
{**
  Jvr - 05/02/2001 12:51:32.<BR>
  Jvr - 29/04/2002 17:14:33 - Added support for AutoInc fields<P>

  <B>NOTE:</B><BR>
  If an integer field has the AutoInc flag set then the previous value for
  the autoinc data is incremented the field is set to this value ONLY if
  an existing value is not present in the fieldbuffer.<P>
}
procedure TDBIXbaseDataConnection.PutFieldIntegerFP(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );

var
  PIntegerValue: PLongInt;

begin
  if (FieldBuffer <> nil) then begin
    // FieldType = DT_INTEGER, Size = 4, Decimals = 0
    case FieldProps[FieldNo].iFldType of
      fldINT8, fldUINT8:   LongInt(RecordBuffer) := PShortInt(FieldBuffer)^;
      fldINT16, fldUINT16: LongInt(RecordBuffer) := PSmallInt(FieldBuffer)^;
      fldINT32, fldUINT32: LongInt(RecordBuffer) := PLongInt(FieldBuffer)^;
      fldINT64, fldUINT64: LongInt(RecordBuffer) := PInt64(FieldBuffer)^;
    end;
  end
  else if (FFields[FieldNo].FieldSize[1] = DT_AUTOINCFLAG) then begin
    PIntegerValue := @(FFields[FieldNo].FieldData);
    Inc(PIntegerValue^);
    LongInt(RecordBuffer) := PIntegerValue^;
  end;
end;  { PutFieldIntegerFP }


// _____________________________________________________________________________
{**
  Jvr - 08/05/2001 11:59:05.<P>
}
function TDBIXbaseDataConnection.GetFieldNullFlags(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
begin
  // Field has value - not blank
  Result := True;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  Move(RecordBuffer, FieldBuffer^, FieldProps[FieldNo].iFldLen);
end;  { GetFieldNullFlags }


// _____________________________________________________________________________
{**
  Jvr - 08/05/2001 12:01:46.<P>
}
procedure TDBIXbaseDataConnection.PutFieldNullFlags(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
begin
  if (FieldBuffer = nil) then Exit;

  Move(FieldBuffer^, RecordBuffer, FieldProps[FieldNo].iFldLen);
end;  { PutFieldNullFlags }


// _____________________________________________________________________________
{**
  Jvr - 08/05/2001 14:53:34 - Because of the way conversion is done between
                              TDateTime, TTimeStame & MSecs - I have to round
                              the time value to 1/100-th of a second (yuk).<P>
                              <B>Notes:</B><BR>
                              The problem is that milliseconds when NOT
                              displayed get truncated (it seems) not rounded to
                              the nearest second. At this stage I'm not going to
                              worry about it and just round milliseconds to
                              1/100-ths of a second.<BR>
  Jvr - 11/10/2002 13:49:27 - Now checking for 0 (blank values)<P>
}
function TDBIXbaseDataConnection.GetFieldDateTime(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  TimeStamp: TTimeStamp;
  FPTimeStamp: TDBIXbaseFoxProTimeStamp;

begin
  Result := Int64(RecordBuffer) <> 0;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  FPTimeStamp := TDBIXbaseFoxProTimeStamp(Pointer(@RecordBuffer)^);
  TimeStamp.Date := FPTimeStamp.Date - XbaseFoxProDateOffset;

  // This is here to allow datetime fields full compatibility with Foxpro,
  // or provide to the millisecond accuracy if xsStrictDateTimes is set.
  if xsStrictDateTimes in FOptions then begin
    TimeStamp.Time := FPTimeStamp.Time;
  end
  else begin
    TimeStamp.Time := Round(FPTimeStamp.Time/1000) * 1000;
  end;

  PDateTime(FieldBuffer)^ := TimeStampToMSecs(TimeStamp);
end;  { GetFieldDateTime }


// _____________________________________________________________________________
{**
  Jvr - 07/05/2001 14:12:58 - Because of the way conversion is done between
                              TDateTime, TTimeStame & MSecs - I have to round
                              the time value to 1/100-th of a second (yuk).<P>
                              <B>Notes:</B><BR>
                              The problem is that milliseconds when NOT
                              displayed get truncated (it seems) not rounded to
                              the nearest second. At this stage I'm not going to
                              worry about it and just round milliseconds to
                              1/100-ths of a second.
  Jvr - 09/12/2002 17:00:00 - Foxpro Datetime storage 8 * null bytes represents
                              an empty datetime value
}
procedure TDBIXbaseDataConnection.PutFieldDateTime(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
var
  TimeStamp: TTimeStamp;

begin
  if (FieldBuffer = nil) then begin
    FillChar(RecordBuffer, 8, #0);
  end
  else begin
    TimeStamp := MSecsToTimeStamp(PDateTime(FieldBuffer)^);

    TDBIXbaseFoxProTimeStamp(RecordBuffer).Date := TimeStamp.Date + XbaseFoxProDateOffset;
    TDBIXbaseFoxProTimeStamp(RecordBuffer).Time := TimeStamp.Time;
  end;
end;  { PutFieldDateTime }


// _____________________________________________________________________________
{**
  Jvr - 10/13/2003 3:08:31 PM - Updated code.<p>
}
function TDBIXbaseDataConnection.GetFieldDate(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  TimeStamp: TTimeStamp;
  DateTime: TDateTime;
  Data: array[0..10] of AnsiChar;

begin
  // Convert Buffer to String;
  FillChar(Data, SizeOf(Data), #0);
  Move(RecordBuffer, Data, FFields[FieldNo].FieldSize[0]);
  Result := (Data[0] <> ' ') and (Data[0] <> #0);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  DateTime := DBIStrDateStampToDateTime(PAnsiChar(@Data));
  TimeStamp := DateTimeToTimeStamp(DateTime);
  PDateTimeRec(FieldBuffer)^.Date := TimeStamp.Date;
end;  { GetFieldDate }


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 14:22:32<P>
}
procedure TDBIXbaseDataConnection.PutFieldDate(
  var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  );
var
  TimeStamp: TTimeStamp;               { TTimeStamp declared in SysUtils }
  DateTime: TDateTime;
  DateString: String;                  { Use Default string type for conversion }
  DataString: TDBIString;

begin
  if (FieldBuffer = nil) or (PDateTimeRec(FieldBuffer)^.Date = 0) then begin
    FillChar(RecordBuffer, FFields[FieldNo].FieldSize[0], Ord(' '));
  end
  else begin
    TimeStamp.Date := PDateTimeRec(FieldBuffer)^.Date;
    DateTime := TimeStampToDateTime(TimeStamp);
    DateTimeToString(DateString, STOREDDATEFORMAT, DateTime);

    // Convert to AnsiString!
    DataString := TDBIString(DateString);
    Move(TDBIStringBuffer(DataString)^, RecordBuffer, FFields[FieldNo].FieldSize[0]);
  end;
end;  { PutFieldDate }


// _____________________________________________________________________________
{**
  Jvr - 02/03/2001 13:56:42 - Now also supports the older Dbase-3 style memos<P>
}
function TDBIXbaseDataConnection.GetFieldMemo(
  const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer;
  FieldNo: Word
  ): Boolean;
var
  Position: LongWord;
  PData: TDBIRecordBuffer;
  Size: Integer;
  ConvBuff: array[0..15] of AnsiChar;
  Code: Integer;

begin
  // if the physical length of the data = 4 then the value is binary
  if (FieldProps[FieldNo].iFldLen = SizeOfMemo) then begin
    Move(RecordBuffer, Pointer(@Position)^, SizeOfMemo);
    Result := Position <> 0;

    // If FieldBuffer parameter is nil, then only return a True or False value
    // indicating if there is data present or not (eg. not blank)
    if (FieldBuffer = nil) or not Result then Exit;

    Move((@Position)^, FieldBuffer^, SizeOfMemo);
  end


  // else if the physical field length = 10 then the value is ascii (DBase-3)
  else if (FieldProps[FieldNo].iFldLen = 10) then begin
    PData := @RecordBuffer;
    Size := FieldProps[FieldNo].iFldLen;

    // Move pointer to first valid char of data
    while (PData^ = TDBIRecordElement(' ')) and (Size > 0) do begin
      Inc(PData, 1);
      Dec(Size);
    end;

    // Do we have valid data?
    Result := (Size <> 0) and (PData^ <> TDBIRecordElement('*'));

    // If FieldBuffer parameter is nil, then only return a True or False value
    // indicating if there is data present or not (eg. not blank)
    if (FieldBuffer = nil) or not Result then Exit;

    // Assign data to conversion buffer
    Move(PData^, ConvBuff, Size);
    // Make sure the data is null terminated for the conversion routine
    ConvBuff[Size] := #0;

    // Convert the dbase numeric type to an integer value and assign to Field Buffer
    Val(TDBINumberString(ConvBuff), PInteger(FieldBuffer)^, Code);
    Result := Code = 0;
  end


  // Otherwise we don't know how the memo position is stored
  else begin
    raise EDBIException.Create(Self, 'GetFieldMemo::3435',
      'Unknown memo field (position), size = %d',
      [FieldProps[FieldNo].iFldLen]
      );
  end;
end;  { GetFieldMemo }


// _____________________________________________________________________________
{**
  Jvr - 02/03/2001 14:09:30 - Now also supports the older Dbase-3 style memos<P>
  Jvr - 03/10/2002 14:18:35 - Now using PAnsiChars instead of Strings.
                              Strings are slower and have a higher overhead.<P>
}
procedure TDBIXbaseDataConnection.PutFieldMemo(var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer; FieldNo: Word);
begin
  // if the physical length of the data = 4 then the value is binary
  if (FieldProps[FieldNo].iFldLen = SizeOfMemo) then begin
    if (FieldBuffer <> nil) then begin
      Move(FieldBuffer^, RecordBuffer, FieldProps[FieldNo].iFldLen);
    end;
  end


  // else if the physical field size = 10 then the value is ascii (DBase-3)
  else if (FieldProps[FieldNo].iFldLen = 10) then begin
    if (FieldBuffer = nil) then begin
      FillChar(RecordBuffer, FieldProps[FieldNo].iFldLen, Ord(' '));
    end
    else begin
      TDBIAnsi.FormatBuf(RecordBuffer, FieldProps[FieldNo].iFldLen, '%*d',
        [FieldProps[FieldNo].iFldLen, Integer(FieldBuffer^)]);
    end;
  end


  // Otherwise we don't know how the memo position is stored
  else begin
    raise EDBIException.Create(Self, 'PutFieldMemo::3475',
      'Unknown memo field (position), size = %d',
      [FieldProps[FieldNo].iFldLen]
      );
  end;
end;  { PutFieldMemo }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 13:57:27.<P>
}
function TDBIXbaseDataConnection.GetFieldDeleted(const RecordBuffer;
  var FieldBuffer: TDBIFieldBuffer; FieldNo: Word): Boolean;
var
  PData: TDBIRecordBuffer;

begin
  PData := @RecordBuffer;
  // The deleted field always contains data
  Result := True;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  // Assign data to field buffer
  if (PData^ = TDBIRecordElement('*')) then begin
    Boolean(FieldBuffer^) := True;
  end
  else begin
    Boolean(FieldBuffer^) := False;
  end;
end;  { GetFieldDeleted }


// _____________________________________________________________________________
{**
  Jvr - 09/03/2001 13:50:39.<P>
}
procedure TDBIXbaseDataConnection.PutFieldDeleted(var RecordBuffer;
  const FieldBuffer: TDBIFieldBuffer; FieldNo: Word);
var
  PData: TDBIRecordBuffer;

begin
  PData := @RecordBuffer;

  if (FieldBuffer <> nil) and (Boolean(FieldBuffer^) = True) then begin
    PData^ := TDBIRecordElement('*');
  end
  else begin
    PData^ := TDBIRecordElement(' ');
  end;
end;  { PutFieldDeleted }


// _____________________________________________________________________________
{**
  Jvr - 06/11/2000 12:49:45<P>
}
function TDBIXbaseDataConnection.AllocPhysicalBuffer: TDBIRecordBuffer;
begin
  Result := AllocMem(RecordSize);
end;  { AllocPhysicalBuffer }


// _____________________________________________________________________________
{**
  Jvr - 06/11/2000 12:49:42<P>
}
procedure TDBIXbaseDataConnection.FreePhysicalBuffer(var Buffer: TDBIRecordBuffer);
begin
  FreeMem(Buffer);
end;  { FreePhysicalBuffer }



end.

