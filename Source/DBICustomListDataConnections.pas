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
  1.0 | 08/11/2000 11:08:22 | Jvr | Initial Release
  1.1 | 05/03/2002 11:23:13 | Jvr | Added ADT support (Object properties)
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBICustomListDataConnections;

interface

{$I DBICompilers.inc}

uses
  Classes, SysUtils, Windows, Contnrs, TypInfo, DB, DBIConst, DBIInterfaces,
  DBIIntfConsts, DBIStrings;

type
  TGetObjectFieldProc = function (
    DataObject: TObject;
    var FieldBuffer: TDBIFieldBuffer;
    const FieldName: TDBIFieldName;
    const FieldNo: Word
    ): Boolean of object;
  TPutObjectFieldProc = procedure (
    DataObject: TObject;
    const FieldBuffer: TDBIFieldBuffer;
    const FieldName: TDBIFieldName;
    const FieldNo: Word
    ) of object;

type
  TDBICustomListDataConnection = class(TDBIDataConnection)
  private
    FClassTypeName: String;
    FOptions: TDBIListDataConnectionOptions;
    FFieldObjectStack: TList;
    FOnCreateObject: TDBIOnCreateObjectEvent;
    FStringFieldSize: Integer;
    FObjectValidationProc: TDBIMethodName;
    FObjectDataEventProc: TDBIMethodName;
//##JVR    FOnCreateValidationObject: TDBIOnCreateObjectEvent;

    // Jvr - 20/09/2002 13:17:08
    FValidationObject: TObject;

  protected
    // Get & Put Field Accessor methods
    function GetFieldUnknown(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldUnknown(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldString(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldString(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldInteger(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldInteger(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldBoolean(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldBoolean(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldBCD(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldBCD(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldFloat(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldFloat(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldDateTime(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldDateTime(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldMemo(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldMemo(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldNullFlags(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldNullFlags(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word );

    function GetFieldDataset(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldDataset(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

    function GetFieldADT(DataObject: TObject; var FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word): Boolean;
    procedure PutFieldADT(DataObject: TObject; const FieldBuffer: TDBIFieldBuffer;
      const FieldName: TDBIFieldName; const FieldNo: Word);

  protected
    function GetClassTypeName: String; virtual;
    function GetData(DataObject: TObject; var Buffer): DSAttr; virtual;
    procedure PutData(DataObject: TObject; const Buffer); virtual;

    function GetClassKind: TPersistentClass; virtual;
    procedure SetClassTypeName(const Value: String); virtual;
    procedure SetObjectValidationProc(const Value: TDBIMethodName);
    procedure SetOptions(const Value: TDBIListDataConnectionOptions);

    function GetFieldName(const FieldNo: Integer): TDBIFieldName;
    procedure GetMetaData; override;

  protected
    function CreateObject(EventType: TDBICreateObjectEventType): TObject; virtual;
    function GetItem(Index: Integer): TObject; virtual;
    procedure SetItem(Index: Integer; const Value: TObject); virtual;
    function GetRecordCount(const StatusFilter: DSAttr): Integer; override;
    procedure SetActive(const Value: Boolean); override;
    procedure DataEvent(DataObject: TObject; Event: TDBIObjectDataEvent); virtual;

    procedure LoadFromXBaseFile(const AFileName: TFileName);
    procedure LoadFromXBaseStream(AStream: TStream);

    procedure ValidateObject(DataObject: TObject); virtual;

  public
    constructor Create(AOwner: TObject); override;
    constructor CreateEmbedded(
      AOwner: TDBIDataConnection;
      const iFieldID: Integer
      ); override;
    destructor Destroy; override;

    function AddItem(Item: TObject): Integer; virtual;
    function CreateItem: TObject; virtual;
    procedure DeleteItem(const Index: Integer); virtual;
    function IndexOfItem(Item: TObject): Integer; virtual;
    function RemoveItem(Item: TObject): Integer; virtual;

    procedure New; override;
    function Append(const Buffer): Integer; override;
    procedure Cancel; override;
    procedure Delete(const Position: Integer); override;
    procedure Get(const Position: Integer; out Buffer; DataInfo: PDataInfo); override;
    procedure Update(const Position: Integer; const Buffer); override;
    procedure Reset; override;
    procedure SyncRecordBuffer(var Buffer; const Initialise: Boolean); override;
    procedure ValidateMetaData; override;
    procedure ValidateField(
      const Position: Integer;
      const FieldNo: Word;
      pFldBuf: Pointer
      ); override;

    procedure NotifyDataEvent(
      DataObject: TObject;
      Event: TDBIDataChangeEventType;
      ItemIndex: Integer
      );

    // Locking methods
    function Lock: Boolean; override;
    function Unlock: Boolean; override;
    function LockRecord(const Position: Integer): Boolean; override;
    function UnlockRecord(const Position: Integer): Boolean; override;


    // Blob methods
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

    // Load methods
    procedure LoadFromStream(
      AStream: TStream;
      const Format: TDBIDataFormat
      ); override;

    procedure LoadFromFile(
      AFileName: String;
      const Format: TDBIDataFormat
      ); override;

    procedure SaveToStream(
      AStream: TStream;
      const Format: TDBIDataFormat
      ); override;

    procedure SaveToFile(
      AFileName: String;
      const Format: TDBIDataFormat
      ); override;

    // Properties
    property ClassKind: TPersistentClass read GetClassKind;
    property ClassTypeName: String read GetClassTypeName write SetClassTypeName;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property Options: TDBIListDataConnectionOptions read FOptions write SetOptions;
    property StringFieldSize: Integer read FStringFieldSize write FStringFieldSize;
    property ObjectValidationProc: TDBIMethodName
      read FObjectValidationProc
      write SetObjectValidationProc;

    // Events
    property OnCreateObject: TDBIOnCreateObjectEvent
      read FOnCreateObject
      write FOnCreateObject;

  end;  { TDBICustomListDataConnection }



implementation

uses
{$ifdef DELPHI6}
  FmtBcd,
  RtlConsts,
{$endif}
  Math,
{$ifndef fpc}
  Consts,
{$endif}
  Dialogs,
  DBITypInfo,
  DBIXbaseDataConnections,
  DBIUtils;


type
  TDBIObjectValidationProc = procedure of object;

  TDBIObjectDataEventProc = procedure(
    Sender: TObject;
    Event: TDBIObjectDataEvent
    ) of object;

  PDBIObject = ^TObject;


{ TDBIBlob }

type
  TDBIBlob = class(TPersistent)
  private
    FDataString: TDBIString;

  public
    constructor Create(PData: PDBIString; const Size: Integer);

    property AsString: TDBIString read FDataString;
  end;


constructor TDBIBlob.Create(PData: PDBIString; const Size: Integer);
begin
  FDataString := '';

  if (Size > 0) then begin
    SetLength(FDataString, Size);
    Move(PData^, PDBIString(FDataString)^, Size);
  end;
end;





// =============================================================================
// 'TDBICustomListDataConnection' public methods
// =============================================================================


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 16:17:41<P>
}
constructor TDBICustomListDataConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  CanModify := True;
  FOptions := osDefaultListDataConnectionOptions;
  FStringFieldSize := Default_StringFieldLength;
  FOnCreateObject := nil;

  FFieldObjectStack := TList.Create;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 23/03/2005 10:53:19 - Initial code.<br>
}
constructor TDBICustomListDataConnection.CreateEmbedded(
  AOwner: TDBIDataConnection;
  const iFieldID: Integer
  );
var
  ParentConnection: TDBICustomListDataConnection;

begin
  inherited CreateEmbedded(AOwner, iFieldID);

  ParentConnection := AOwner as TDBICustomListDataConnection;
  ClassTypeName := GetClass(ParentConnection.GetFieldName(iFieldID)).ClassName;
end;  { CreateEmbedded }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 16:19:27<P>
}
destructor TDBICustomListDataConnection.Destroy;
var
  Index: Integer;

begin
  // This should never happen??? - If it does then at least we cleaned up
  for Index := FFieldObjectStack.Count-1 downto 0 do begin
    TObject(FFieldObjectStack.Items[Index]).Free;
    FFieldObjectStack.Delete(Index);
  end;

  FFieldObjectStack.Free;
  FFieldObjectStack := nil;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 14:10:39.<P>
}
function TDBICustomListDataConnection.{%H-}AddItem(Item: TObject): Integer;
begin
  raise EDBINotImplementedException.Create(Self, 'AddItem::375',
    'AddItem has NOT been implemented in the derived class!', []);
end;  { AddItem }


// _____________________________________________________________________________
{**
  Jvr - 06/10/2009 12:40:32 - Initial code.<br />
}
function TDBICustomListDataConnection.CreateItem: TObject;
var
  ClassRef: TClass;

begin
  ClassRef := FindClass(ClassTypeName);
  Result := ClassRef.Create;
end;  { CreateItem }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 14:18:53.<P>
}
procedure TDBICustomListDataConnection.DeleteItem(const Index: Integer);
begin
  raise EDBINotImplementedException.Create(Self, 'DeleteItem::400',
    'DeleteItem has NOT been implemented in the derived class!', []);
end;  { DeleteItem }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 14:30:05.<P>
}
function TDBICustomListDataConnection.{%H-}IndexOfItem(Item: TObject): Integer;
begin
  raise EDBINotImplementedException.Create(Self, 'IndexOfItem::415',
    'IndexOfItem has NOT been implemented in the derived class!', []);
end;  { IndexOfItem }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 15:27:24.<P>
}
function TDBICustomListDataConnection.{%H-}RemoveItem(Item: TObject): Integer;
begin
  raise EDBINotImplementedException.Create(Self, 'RemoveItem::425',
    'RemoveItem has NOT been implemented in the derived class!', []);
end;  { RemoveItem }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 16:55:57 - Initial code<BR>
  Jvr - 20/09/2002 13:20:38 - Added field validation via a validation Object<BR>
  Jvr - 28/11/2002 14:21:33 - Added WriteThrough mode<P>
}
function TDBICustomListDataConnection.Append(const Buffer): Integer;
var
  NewObject: TObject;

begin
  ValidateObject(FValidationObject);

  Result := -1;

  try
    // Create the new object
    NewObject := CreateObject(coNormal);
    try
      // If not a blank record
      if (@Buffer <> nil) then begin
        // Attempt to set the property values.
        // If this fails then the new object should be destroyed
        PutData(NewObject, Buffer);
      end;  { if }

    except
      NewObject.Free;

      // This will ensure that the rest of the method will fail
      // and the generated exception is surfaced.
      raise;
    end;  { try..except }

    // If creation and updating the properties succeeds then add the new object.
    Result := AddItem(NewObject);
    Inc(Result);
    Assert(Result > 0);

    // Pass in the record number (not the record position)
    NotifyDataEventCallBacks(Result, dbiRecordInserted, @Buffer);

  finally
    // Cleanup validation object if it exists
    FValidationObject.Free;
    FValidationObject := nil;
  end;  { try..finally }
end;  { Append }


// _____________________________________________________________________________
{**
  Jvr - 20/09/2002 13:46:55 - Added field validation via a validation Object<P>
}
procedure TDBICustomListDataConnection.Cancel;
begin
  // Cleanup validation object
  FValidationObject.Free;
  FValidationObject := nil;
end;  { Cancel }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 16:18:56<P>
}
procedure TDBICustomListDataConnection.Delete(const Position: Integer);
begin
  DeleteItem(Position);

  // Pass in the the record number (not the record position)
  NotifyDataEventCallBacks(Position+1, dbiRecordDeleted, nil);
end;  { Delete }


// _____________________________________________________________________________
{**
  Jvr - 10/11/2000 11:53:01 - Initial code.<br>
  Jvr - 09/12/2004 20:14:43 - Replaced Result with DataInfo Parameter<p>
}
procedure TDBICustomListDataConnection.Get(
  const Position: Integer;
  out Buffer;
  DataInfo: PDataInfo
  );
begin
  if Assigned(DataInfo) then begin
    DataInfo.Attribute := dsRecUnmodified;
    DataInfo.Data := Items[Position];
  end;

  if (Position < 0) or (Position >= GetCount(dsRecAll)) then begin
    raise EDBIException.Create(Self, 'Get::525', 'Record "%d" out of legal range', [Position]);
  end;

  if (Items[Position] = nil) then begin
    if Assigned(DataInfo) then begin
      DataInfo.Attribute := DataInfo.Attribute or dsRecDeleted;
    end;
  end
  else if (@Buffer <> nil) then begin
    GetData(Items[Position], Buffer);
  end;
end;  { Get }


// _____________________________________________________________________________
{**
  Jvr - 06/10/2009 11:59:53 - Initial code.<br />
}
function TDBICustomListDataConnection.GetClassTypeName: String;
begin
  Result := FClassTypeName;
end;  { GetClassTypeName }


// _____________________________________________________________________________
{**
  Translate from the Physical data buffer to the Logical data buffer

  Jvr - 10/11/2000 12:50:17.<BR>
  Jvr - 04/06/2001 14:23:19 - Added BCD support<P>
}
function TDBICustomListDataConnection.GetData(DataObject: TObject; var Buffer): DSAttr;
var
  FieldName: TDBIFieldName;
  FieldNo: Integer;
  FieldObject: TObject;
  LogicalOffset: Integer;
  GetFieldProc: TGetObjectFieldProc;
  pFldBuf: TDBIFieldBuffer;
  IsBlank: Boolean;

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

  for FieldNo := 0 to FieldCount - 1 do begin
    LogicalOffset := FieldProps[FieldNo].iFldOffsInRec;
    GetFieldProc := GetFieldUnknown;
    FieldName := GetFieldName(FieldNo);
    IsBlank := True;

    // If an embedded object has been placed on the stack for this field then get it!
    if (FFieldObjectStack.Count > 0) then begin
      FieldObject := FFieldObjectStack[FFieldObjectStack.Count - 1];
      FFieldObjectStack.Delete(FFieldObjectStack.Count - 1);
    end
    else begin
      FieldObject := DataObject;
    end;  { if }

    if IsPublishedProp(FieldObject, FieldName) then begin
      // Simple types don't support null (or nil) values therefore most of the
      // time IsBlank will be false.  The GetObjectFieldProc returns False
      // (e.g. for a blank string or a nil object) if the value is blank.
      IsBlank := False;

      case FieldProps[FieldNo].iFldType of
        fldUNKNOWN:    GetFieldProc := GetFieldUnknown;
        fldZSTRING:    GetFieldProc := GetFieldString;
        fldDATE:       GetFieldProc := GetFieldDateTime;
        fldBLOB:       GetFieldProc := GetFieldMemo;
        fldBOOL:       GetFieldProc := GetFieldBoolean;
        fldINT16:      GetFieldProc := GetFieldInteger;
        fldINT32:      GetFieldProc := GetFieldInteger;
        fldFLOAT:      GetFieldProc := GetFieldFloat;
        fldBCD:        GetFieldProc := GetFieldBCD;
        fldBYTES:      GetFieldProc := GetFieldUnknown;
        fldTIME:       GetFieldProc := GetFieldDateTime;
        fldTIMESTAMP:  GetFieldProc := GetFieldDateTime;
        fldUINT16:     GetFieldProc := GetFieldInteger;
        fldUINT32:     GetFieldProc := GetFieldInteger;
        fldFLOATIEEE:  GetFieldProc := GetFieldFloat;
        fldVARBYTES:   GetFieldProc := GetFieldUnknown;
        fldLOCKINFO:   GetFieldProc := GetFieldUnknown;
        fldCURSOR:     GetFieldProc := GetFieldUnknown;
        fldINT64:      GetFieldProc := GetFieldInteger;
        fldUINT64:     GetFieldProc := GetFieldInteger;
        fldADT:        GetFieldProc := GetFieldADT;
        fldARRAY:      GetFieldProc := GetFieldUnknown;
        fldREF:        GetFieldProc := GetFieldUnknown;
        fldTABLE:      GetFieldProc := GetFieldDataset;
        fldDATETIME:   GetFieldProc := GetFieldUnknown;
        fldFMTBCD:     GetFieldProc := GetFieldBCD;
        fldWIDESTRING: GetFieldProc := GetFieldString;
        fldINT8:       GetFieldProc := GetFieldInteger;
        fldUINT8:      GetFieldProc := GetFieldInteger;
        fldSINGLE:     GetFieldProc := GetFieldFloat;
        fldUNICODE:    GetFieldProc := GetFieldString;
      else
        GetFieldProc := GetFieldUnKnown;
      end;  { case }
    end;  { if IsPublishedProp }

    if not IsBlank then begin
      // Setup FieldBuffer
      pFldBuf := @(TDBIRecordBuffer(@Buffer)[LogicalOffset]);

      IsBlank := not GetFieldProc(FieldObject, pFldBuf, FieldName, FieldNo);
    end;

    { TODO 5 -oJvr -cTDBIObjectListDataConnection.GetData() : NullFlags }
    // When rebuilding the record buffer the Nullflags need to be set as well!
    // Set the Null-Flag for this field to the 'IsBlank' value
    // If iNullOffsInRec is Zero or less then Nulls are not supported
    if (FieldProps[FieldNo].iNullOffsInRec > 0) then begin
      Byte(TDBIRecordBuffer(@Buffer)[FieldProps[FieldNo].iNullOffsInRec]) := Ord(IsBlank);
    end;
  end;  { for }
end;  { GetData }


// _____________________________________________________________________________
{**
  Translate from the Logical data buffer to the Physical data buffer.

  Jvr - 27/11/2000 11:11:39<BR>
  Jvr - 04/06/2001 14:21:13 - Added BCD support<P>
}
procedure TDBICustomListDataConnection.PutData(DataObject: TObject; const Buffer);
var
  FieldName: TDBIFieldName;
  FieldNo: Integer;
  FieldObject: TObject;
  PropInfo: PPropInfo;
  LogicalOffset: Integer;
  PutFieldProc: TPutObjectFieldProc;
  pFldBuf: Pointer;
  IsBlank: Boolean;

begin
  // Setup the NullFlags buffer if this dataset supports it
  if NullFlags.IsNullable then begin
    FieldNo := FieldCount - 1;
    LogicalOffset := FieldProps[FieldNo].iFldOffsInRec;
    pFldBuf := @(TDBIRecordBuffer(@Buffer)[LogicalOffset]);
    NullFlags.SetBuffer(pFldBuf, FieldProps[FieldNo].iFldLen);
  end;

  for FieldNo := 0 to FieldCount - 1 do begin
    FieldName := GetFieldName(FieldNo);
    LogicalOffset := FieldProps[FieldNo].iFldOffsInRec;

    // If an embedded object has been placed on the stack for this field then get it!
    if (FFieldObjectStack.Count > 0) then begin
      FieldObject := FFieldObjectStack[FFieldObjectStack.Count - 1];
      FFieldObjectStack.Delete(FFieldObjectStack.Count - 1);
    end
    else begin
      FieldObject := DataObject;
    end;

    PropInfo := GetPropInfo(FieldObject, FieldName);
    if (PropInfo <> nil) then begin
      // This check sometimes causes a failure
      // on legal constructs. It would be removed
      // if it wasn't so handy.
      if not Assigned(PropInfo.SetProc) then begin
        if (osErrorOnReadonlyProperty in Options) then begin
          raise EDBIException.Create(Self, 'PutData::705',
            SErrorReadOnlyProperty,
            [DataObject.ClassName + '.' + TDBIPropName(PropInfo.Name)]
            );
        end
        else begin
          Continue;
        end;
      end;  { if }

      case FieldProps[FieldNo].iFldType of
        fldUNKNOWN:    PutFieldProc := PutFieldUnknown;
        fldZSTRING:    PutFieldProc := PutFieldString;
        fldDATE:       PutFieldProc := PutFieldDateTime;
        fldBLOB:       PutFieldProc := PutFieldMemo;
        fldBOOL:       PutFieldProc := PutFieldBoolean;
        fldINT16:      PutFieldProc := PutFieldInteger;
        fldINT32:      PutFieldProc := PutFieldInteger;
        fldFLOAT:      PutFieldProc := PutFieldFloat;
        fldBCD:        PutFieldProc := PutFieldBCD;
        fldBYTES:      PutFieldProc := PutFieldUnknown;
        fldTIME:       PutFieldProc := PutFieldDateTime;
        fldTIMESTAMP:  PutFieldProc := PutFieldDateTime;
        fldUINT16:     PutFieldProc := PutFieldInteger;
        fldUINT32:     PutFieldProc := PutFieldInteger;
        fldFLOATIEEE:  PutFieldProc := PutFieldFloat;
        fldVARBYTES:   PutFieldProc := PutFieldUnknown;
        fldLOCKINFO:   PutFieldProc := PutFieldUnknown;
        fldCURSOR:     PutFieldProc := PutFieldUnknown;
        fldINT64:      PutFieldProc := PutFieldInteger;
        fldUINT64:     PutFieldProc := PutFieldInteger;
        fldADT:        PutFieldProc := PutFieldADT;
        fldARRAY:      PutFieldProc := PutFieldUnknown;
        fldREF:        PutFieldProc := PutFieldUnknown;
        fldTABLE:      PutFieldProc := PutFieldDataset;
        fldDATETIME:   PutFieldProc := PutFieldUnknown;
        fldFMTBCD:     PutFieldProc := PutFieldBCD;
        fldWIDESTRING: PutFieldProc := PutFieldString;
        fldINT8:       PutFieldProc := PutFieldInteger;
        fldUINT8:      PutFieldProc := PutFieldInteger;
        fldSINGLE:     PutFieldProc := PutFieldFloat;
        fldUNICODE:    PutFieldProc := PutFieldString;
      else
        PutFieldProc := PutFieldUnknown;
      end;  { case }

      // Is this field blank?
      IsBlank := Boolean(Byte(TDBIRecordBuffer(@Buffer)[FieldProps[FieldNo].iNullOffsInRec]));

      // Setup FieldBuffer
      if IsBlank then begin
        pFldBuf := nil;
      end
      else begin
        pFldBuf := @(TDBIRecordBuffer(@Buffer)[LogicalOffset]);
      end;

      PutFieldProc(FieldObject, pFldBuf, FieldName, FieldNo);

      // Set the Null-Flag for this field to the 'IsBlank' value
      { TODO 2 -oJvr -cTObjectListDataConnection.PutData() :
        NullFlags
        I probably shouldn't be touching the Nullflags in this case!!!
      }
//##JVR      Byte(TDBIRecordBuffer(@Buffer)[FieldProps[FieldNo].iNullOffsInRec]) := Ord(IsBlank);
    end;  { if }
  end;  { for }
end;  { PutData }


// _____________________________________________________________________________
{**
  Jvr - 13/11/2000 12:38:22 - Result always returns False indicating Blank.<P>
}
function TDBICustomListDataConnection.{%H-}GetFieldUnknown(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
var
  DataTypeName: String;

begin
  DataTypeName := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(DataTypeMap[FieldProps[FieldNo].iFldType]));

  raise EDBIException.Create(Self, 'GetFieldUnknown::790',
    'Field: %s, Unknown data type - "%s" Not supported',
    [FieldName, DataTypeName]
    );
end;  { GetFieldUnknown }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 11:39:39<P>
}
procedure TDBICustomListDataConnection.PutFieldUnknown(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
var
  DataTypeName: String;

begin
  DataTypeName := TypInfo.GetEnumName(TypeInfo(TFieldType), Ord(DataTypeMap[FieldProps[FieldNo].iFldType]));

  raise EDBIException.Create(Self, 'PutFieldUnknown::815',
    'Field: %s, Unknown data type - "%s" Not supported',
    [FieldName, DataTypeName]
    );
end;  { PutFieldUnknown }


// _____________________________________________________________________________
{**
  Jvr - 13/11/2000 12:25:07 - Result returns True if NOT Blank.<P>
}
function TDBICustomListDataConnection.GetFieldString(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
var
  AData: AnsiString;
  WData: WideString;
  DataSize: Integer;

begin
  case FieldProps[FieldNo].iFldType of
    fldWideString, fldUNICODE: begin
      WData := DBIUnicodeGetStrProp(DataObject, FieldName);
      Result := Length(WData) > 0;
    end
    
  else
    AData := DBIAnsiGetStrProp(DataObject, FieldName);
    Result := Length(AData) > 0;
  end;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  // Assign data to field buffer
  case FieldProps[FieldNo].iFldType of
    fldWideString, fldUNICODE: begin
      DataSize := Min(2 * Length(WData), FieldProps[FieldNo].iUnits1);
      Move(PByte(PWideChar(WData))^, FieldBuffer^, DataSize);
    end;

  else
    DataSize := Min(Length(AData), FieldProps[FieldNo].iUnits1);
    Move(PByte(PAnsiChar(AData))^, FieldBuffer^, DataSize);
  end;
end;  { GetFieldString }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 12:08:08<P>
}
procedure TDBICustomListDataConnection.PutFieldString(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
var
  AData: AnsiString;
  WData: WideString;

begin
  // Assign data to object property
  if (FieldBuffer = nil) then begin
    case FieldProps[FieldNo].iFldType of
      fldWIDESTRING, fldUNICODE:
        DBIUnicodeSetStrProp(DataObject, FieldName, '');
      else
        DBIAnsiSetStrProp(DataObject, FieldName, '');
    end;
  end
  else begin
    case FieldProps[FieldNo].iFldType of
      fldWIDESTRING, fldUNICODE: begin
        WData := WideString(PWideChar(FieldBuffer));
        DBIUnicodeSetStrProp(DataObject, FieldName, WData);
      end;

    else
      AData := AnsiString(PAnsiChar(FieldBuffer));
      DBIAnsiSetStrProp(DataObject, FieldName, AData);
    end;
  end;
end;  { PutFieldString }


// _____________________________________________________________________________
{**
  Jvr - 13/11/2000 12:36:44 - Result returns True if NOT Blank.<BR>
  Jvr - 28/08/2001 12:37:06 - Added support for sets.<P>
}
function TDBICustomListDataConnection.GetFieldInteger(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
var
  IntegerValue: LongInt;

begin
  // Field has no value - blank
  Result := TDBIPropType.IsTypeKind(DataObject, FieldName, [tkInteger, tkInt64, tkEnumeration, tkSet]);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  if (FieldProps[FieldNo].iFldType in [fldINT8, fldUINT8, fldINT16, fldUINT16, fldINT32, fldUINT32]) then begin
    IntegerValue := GetOrdProp(DataObject, FieldName);
  end
  else begin
    IntegerValue := 0;
  end;

  // Assign data to field buffer
  case FieldProps[FieldNo].iFldType of
    fldINT8, fldUINT8:   PShortInt(FieldBuffer)^ := ShortInt(IntegerValue);
    fldINT16, fldUINT16:  PSmallInt(FieldBuffer)^ := SmallInt(IntegerValue);
    fldINT32, fldUINT32:  PLongInt(FieldBuffer)^ := LongInt(IntegerValue);
    fldINT64, fldUINT64:  PInt64(FieldBuffer)^ := GetInt64Prop(DataObject, FieldName);
  end;
end;  { GetFieldInteger }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 11:42:20 - Initial Version<BR>
  Jvr - 28/08/2001 12:38:39 - Added support for sets<P>
}
procedure TDBICustomListDataConnection.PutFieldInteger(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
begin
  if not TDBIPropType.IsTypeKind(DataObject, FieldName, [tkInteger, tkInt64, tkEnumeration, tkSet]) then
  begin
    Exit;
  end;
  
  // Assign data to object property
  if (FieldBuffer = nil) then begin
    case FieldProps[FieldNo].iFldType of
      fldINT8, fldUINT8:  SetOrdProp(DataObject, FieldName, ShortInt(0));
      fldINT16, fldUINT16:  SetOrdProp(DataObject, FieldName, SmallInt(0));
      fldINT32, fldUINT32:  SetOrdProp(DataObject, FieldName, LongInt(0));
      fldINT64, fldUINT64:  SetInt64Prop(DataObject, FieldName, Int64(0));
    end;
  end
  else begin
    case FieldProps[FieldNo].iFldType of
      fldINT8, fldUINT8:    SetOrdProp(DataObject, FieldName, PShortInt(FieldBuffer)^);
      fldINT16, fldUINT16:  SetOrdProp(DataObject, FieldName, PSmallInt(FieldBuffer)^);
      fldINT32, fldUINT32:  SetOrdProp(DataObject, FieldName, PLongInt(FieldBuffer)^);
      fldINT64, fldUINT64:  SetInt64Prop(DataObject, FieldName, PInt64(FieldBuffer)^);
    end;
  end;
end;  { PutFieldInteger }


// _____________________________________________________________________________
{**
  Jvr - 13/11/2000 12:54:12 - Result returns True if NOT Blank.<P>
}
function TDBICustomListDataConnection.GetFieldBoolean(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
begin
  // Field has no value - blank
  Result := TDBIPropType.IsTypeKind(DataObject, FieldName, [tkBoolean]);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  Boolean(FieldBuffer^) := Boolean(GetOrdProp(DataObject, FieldName));
end;  { GetFieldBoolean }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 11:54:00<P>
}
procedure TDBICustomListDataConnection.PutFieldBoolean(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
begin
  if not TDBIPropType.IsTypeKind(DataObject, FieldName, [tkBoolean]) then
  begin
    Exit;
  end;

  // Assign data to object property
  if (FieldBuffer = nil) then begin
    SetOrdProp(DataObject, FieldName, Integer(Boolean(False)));
  end
  else begin
    SetOrdProp(DataObject, FieldName, Integer(Boolean(FieldBuffer^)));
  end;
end;  { PutFieldBoolean }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2001 14:26:28.<P>

  PBcd = ^TBcd;
  TBcd  = packed record
    Precision: Byte;                        // 1..64
    SignSpecialPlaces: Byte;                // Sign:1, Special:1, Places:6
    Fraction: packed array [0..31] of Byte; // BCD Nibbles, 00..99 per Byte, high Nibble 1st
  end;
}
function TDBICustomListDataConnection.GetFieldBCD(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
var
  Money: Currency;

begin
  // Field has no value - blank
  Result := TDBIPropType.IsTypeKind(DataObject, FieldName, [tkFloat]);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  Money := GetFloatProp(DataObject, FieldName);
  CurrToBCD(Money, PBcd(FieldBuffer)^);
end;  { GetFieldBCD }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2001 14:25:58.<P>
}
procedure TDBICustomListDataConnection.PutFieldBCD(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
var
  Money: Currency;

begin
  if not TDBIPropType.IsTypeKind(DataObject, FieldName, [tkFloat]) then
  begin
    Exit;
  end;

  // Assign data to object property
  if (FieldBuffer = nil) then begin
    Money := 0.00;
  end
  else begin
    BcdToCurr(PBcd(FieldBuffer)^, Money);
  end;
  SetFloatProp(DataObject, FieldName, Money);
end;  { PutFieldBCD }


// _____________________________________________________________________________
{**
  Jvr - 13/11/2000 13:56:49 - Result returns True if NOT Blank.<P>
}
function TDBICustomListDataConnection.GetFieldFloat(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
begin
  // Field has no value - blank
  Result := TDBIPropType.IsTypeKind(DataObject, FieldName, [tkFloat]);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  case FieldProps[FieldNo].iFldType of
    fldSingle: PSingle(FieldBuffer)^ := GetFloatProp(DataObject, FieldName);
    fldFloat: PDouble(FieldBuffer)^ := GetFloatProp(DataObject, FieldName);
    fldFLOATIEEE: PExtended(FieldBuffer)^ := GetFloatProp(DataObject, FieldName);
  end;
end;  { GetFieldFloat }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 11:54:34<P>
}
procedure TDBICustomListDataConnection.PutFieldFloat(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
begin
  if not TDBIPropType.IsTypeKind(DataObject, FieldName, [tkFloat]) then
  begin
    Exit;
  end;

  // Assign data to object property
  if (FieldBuffer = nil) then begin
    SetFloatProp(DataObject, FieldName, 0.00);
  end
  else begin
    case FieldProps[FieldNo].iFldType of
      fldSingle: SetFloatProp(DataObject, FieldName, PSingle(FieldBuffer)^);
      fldFloat: SetFloatProp(DataObject, FieldName, PDouble(FieldBuffer)^);
      fldFLOATIEEE: SetFloatProp(DataObject, FieldName, PExtended(FieldBuffer)^);
    end;
  end;
end;  { PutFieldFloat }


// _____________________________________________________________________________
{**
  Jvr - 13/11/2000 14:18:10 - Result returns True if NOT Blank.<BR>
  Jvr - 08/02/2002 14:35:29 - Now supports blank values<BR>
  Jvr - 16/04/2002 10:11:16 - Now supports all date and time fields<BR>
  Jvr - 17/04/2002 19:58:01 - Added extract Datetime validation code<BR>
  Jvr - 21/10/2002 14:33:22 - DBIMinDateTime now represents a blank TDateTime<P>
}
function TDBICustomListDataConnection.GetFieldDateTime(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
var
  TimeStamp: TTimeStamp;  { TTimeStamp declared in SysUtils }
  DateTimeValue: Double;

begin
  // Field has no value - blank
  Result := TDBIPropType.IsTypeKind(DataObject, FieldName, [tkFloat]);

  DateTimeValue := GetFloatProp(DataObject, FieldName);
  Result := Result and
    (DateTimeValue > DBIMinDateTime) and (DateTimeValue <= DBIMaxDateTime);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  if not Result then Exit;

  TimeStamp := DateTimeToTimeStamp(DateTimeValue);
  case FieldProps[FieldNo].iFldType of
    fldDate: PDateTimeRec(FieldBuffer)^.Date := TimeStamp.Date;
    fldTime: PDateTimeRec(FieldBuffer)^.Time := TimeStamp.Time;
    fldTimeStamp: PDateTimeRec(FieldBuffer)^.DateTime := TimeStampToMSecs(TimeStamp);
  end;
end;  { GetFieldDateTime }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 11:57:10<P>
  Jvr - 08/02/2002 14:36:06 - Now supports blank values<P>
  Jvr - 16/04/2002 10:14:48 - Now supports all date and time fields.<P>
}
procedure TDBICustomListDataConnection.PutFieldDateTime(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
var
  TimeStamp: TTimeStamp;  { TTimeStamp declared in SysUtils }
  FloatValue: Double;

begin
  if not TDBIPropType.IsTypeKind(DataObject, FieldName, [tkFloat]) then
  begin
    Exit;
  end;

  if (FieldBuffer = nil) then begin
    FloatValue := DBIMinDateTime;
  end
  else begin
    case FieldProps[FieldNo].iFldType of
      fldDate: TimeStamp.Date := PDateTimeRec(FieldBuffer)^.Date;
      fldTime: TimeStamp.Time := PDateTimeRec(FieldBuffer)^.Time;
      fldTimeStamp: TimeStamp := MSecsToTimeStamp(PDateTimeRec(FieldBuffer)^.DateTime);
    end;  { ftDateTime }

    FloatValue := TimeStampToDateTime(TimeStamp);
  end;
  SetFloatProp(DataObject, FieldName, FloatValue);
end;  { PutFieldDateTime }


// _____________________________________________________________________________
{**
  Jvr - 12/02/2001 14:36:18 - FieldBuffer is the TDBIBlob Object Position and is
                              One Based (0=Blank).
                              Result returns True if NOT Blank.<BR>
  Jvr - 31/08/2001 12:10:11 - Added TStrings support for Memo fields<P>
}
function TDBICustomListDataConnection.GetFieldMemo(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
var
  DataString: TDBIString;
  DataSize: Integer;

begin
  Assert(FieldNo < Length(FieldProps));
  Assert(Assigned(DataObject));

  try
    DataString := DBIAnsiGetStrProp(DataObject, FieldName);
    DataSize := Length(DataString);
    PDBIObject(FieldBuffer)^ := nil;

    // Specify the data size in the FieldBuffer, Result = True if data exists
    if (DataSize > 0) then begin
      PDBIObject(FieldBuffer)^ := DataObject;
    end;

    Result := Assigned(PDBIObject(FieldBuffer)^);
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'GetFieldMemo::1265',
        'Unable to Get Blob Data for Field "%s"', [FieldName]
        );
  end;
end;  { GetFieldMemo }


// _____________________________________________________________________________
{**
  Jvr - 12/02/2001 14:39:41.<BR>
  Jvr - 31/08/2001 13:11:57 - Added TStrings support for Memo fields<P>
}
procedure TDBICustomListDataConnection.PutFieldMemo(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
var
  BlobObject: TObject;
  DataString: TDBIString;

begin
  Assert(FieldNo < Length(FieldProps));
  Assert(Assigned(DataObject));

  try
    DataString := '';

    if (FieldBuffer <> nil) then begin
      BlobObject := PDBIObject(FieldBuffer)^;
      if (BlobObject is TDBIBlob) then begin
        DataString := (BlobObject as TDBIBlob).AsString;

        FreeAndNil(BlobObject);
      end

      // If the blob object is the actual data object then the memo was NOT changed
      else if Assigned(BlobObject) and (CompareText(FClassTypeName, BlobObject.ClassName) = 0) then begin
        DataString := DBIAnsiGetStrProp(BlobObject, FieldName);
      end

      // If the blob object is Unassigned then the data is blank.
      // Otherwise if the blob object is assigned and of an unexpected type, then raise an exception
      else if Assigned(BlobObject) then begin
        raise EDBIException.Create(Self, 'PutFieldMemo::1305',
          'Unexpected data object type: "%s"', [BlobObject.ClassName]
          );
      end;

      // Update the FieldBuffer with the DataObject Reference,
      PDBIObject(FieldBuffer)^ := DataObject;
    end;

    // Set the specified propertyb to the value in DataString
    DBIAnsiSetStrProp(DataObject, FieldName, DataString);

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'PutFieldMemo::1320', 'Unable to Put Blob Data for Field "%s"', [FieldName]);
  end;
end;  { PutFieldMemo }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2013 06:54:22.<P>
}
function TDBICustomListDataConnection.GetFieldNullFlags(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
begin
  // Field has no value - blank
  Result := TDBIPropType.IsTypeKind(DataObject, FieldName, [tkInt64]);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  if (FieldBuffer = nil) or not Result then Exit;

  PInt64(FieldBuffer)^ := GetInt64Prop(DataObject, FieldName);
end;  { GetFieldNullFlags }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2013 12:01:46.<P>
}
procedure TDBICustomListDataConnection.PutFieldNullFlags(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
begin
  if not TDBIPropType.IsTypeKind(DataObject, FieldName, [tkInt64]) then Exit;

  // Assign data to object property
  if (FieldBuffer = nil) then begin
    SetInt64Prop(DataObject, FieldName, Int64(0));
  end
  else begin
    SetInt64Prop(DataObject, FieldName, PInt64(FieldBuffer)^);
  end;
end;  { PutFieldNullFlags }


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 11:59:10.<P>
}
function TDBICustomListDataConnection.GetFieldDataset(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
begin
  Result := False;
  Assert(FieldNo < Length(FieldProps));
  Assert(Assigned(DataObject));

  try
    TDBIPropType.Check(DataObject, FieldName, [tkClass]);

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'GetFieldDataset::1390',
        'Unable to Get Dataset Data for Field "%s"',
        [FieldName]
        );
  end;
end;  { GetFieldDataset }


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 11:54:19.<P>
}
procedure TDBICustomListDataConnection.PutFieldDataset(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
begin
  Assert(FieldNo < Length(FieldProps));
  Assert(Assigned(DataObject));

  try
    TDBIPropType.Check(DataObject, FieldName, [tkClass]);

    raise EDBINotImplementedException.Create(Self, 'PutFieldDataset::1415');
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'PutFieldDataset::1420',
        'Unable to Put Dataset Data for Field "%s"',
        [FieldName]
        );
  end;
end;  { PutFieldDataset }


// _____________________________________________________________________________
{**
  Jvr - 15/10/2001 11:35:19.<P>
}
function TDBICustomListDataConnection.GetFieldADT(
  DataObject: TObject;
  var FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  ): Boolean;
var
  PropObject: TObject;
  Index: Integer;

begin
  TDBIPropType.Check(DataObject, FieldName, [tkClass]);

  // If FieldBuffer parameter is nil, then only return a True or False value
  // indicating if there is data present or not (eg. not blank)
  Result := (FieldBuffer <> nil);
  if not Result then Exit;

  PropObject := GetObjectProp(DataObject, FieldName, TObject);

  // If Fieldbuffer has a value of zero then it is a nil reference
  // and there is no value.
  Result := PropObject <> nil;

  if Result then begin
    TObject(Pointer(FieldBuffer)^) := PropObject;

    // Push (Fieldprops size) * FieldObject on to the FieldObject stack
    for Index := 0 to FieldProps[FieldNo].iUnits1 - 1 do begin
      FFieldObjectStack.Add(PropObject);
    end;
  end;  { if }
end;  { GetFieldADT }


// _____________________________________________________________________________
{**
  Jvr - 15/10/2001 11:52:24.<P>
}
procedure TDBICustomListDataConnection.PutFieldADT(
  DataObject: TObject;
  const FieldBuffer: TDBIFieldBuffer;
  const FieldName: TDBIFieldName;
  const FieldNo: Word
  );
var
  PropInfo: PPropInfo;
  PropClass: TClass;
  PropObject: TObject;
  Index: Integer;

begin
  TDBIPropType.Check(DataObject, FieldName, [tkClass]);

  // NOTE:
  // When inserting a new record/object it seems that FieldBuffer is 'nil'
  // When updating/posting an existing object the field buffer is assigned
  if (FieldBuffer = nil) then begin
    { TODO 1 -ojvr -cTDBICustomListDataConnection.PutFieldADT() :
      22/03/2005 16:48:58
      I don't know why I wanted to raise an exception here,
      Time will tell if it is required
      }

{##JVR
    Error(nil, Caller, '1510',
      'An unexpected blank value for an ADT parent field "%s" was encountered',
      [FieldName]
      );
}
  end;

  PropObject := GetObjectProp(DataObject, FieldName, TObject);

  // If we have no object then create one
  if not Assigned(PropObject) then begin
    PropInfo := GetPropInfo(DataObject, FieldName);
    PropClass := TDBIPropType.GetTypeData(PropInfo)^.ClassType;
    PropObject := PropClass.Create;

    // Assign data from field buffer
    SetObjectProp(DataObject, PropInfo, PropObject);
  end;

  // Push the field object on to the FieldObject stack (Fieldprops size times)
  for Index := 0 to FieldProps[FieldNo].iUnits1 - 1 do begin
    FFieldObjectStack.Add(PropObject);
  end;
end;  { PutFieldADT }


// _____________________________________________________________________________
{**
  Jvr - 15/08/2002 10:42:17.<P>
}
function TDBICustomListDataConnection.CreateObject(
  EventType: TDBICreateObjectEventType
  ): TObject;
begin
  Result := nil;

  if Assigned(FOnCreateObject) then begin
    FOnCreateObject(Owner, Result, ClassTypeName, EventType);
  end;

  if not Assigned(Result) then begin
    Result := CreateItem;
  end;
end;  { CreateObject }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:42:00<P>
}
function TDBICustomListDataConnection.{%H-}GetItem(Index: Integer): TObject;
begin
  raise EDBINotImplementedException.Create(Self, 'GetItem::1550',
    'GetItem has NOT been implemented in the derived class!', []);
end;  { GetItem }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:46:15<P>
}
function TDBICustomListDataConnection.{%H-}GetRecordCount(
  const StatusFilter: DSAttr
  ): Integer;
begin
  raise EDBINotImplementedException.Create(Self, 'GetRecordCount::1565',
    'GetRecordCount has NOT been implemented in the derived class!', []);
end;  { GetRecordCount }


procedure TDBICustomListDataConnection.New;
begin
  raise EDBINotImplementedException.Create(Self, 'New::1570',
    'New has NOT been implemented in the derived class!', []);
end;  { New }


// _____________________________________________________________________________
{**
  Jvr - 03/05/2002 14:37:42.<P>
}
procedure TDBICustomListDataConnection.SetActive(const Value: Boolean);
begin
  inherited SetActive(Value);

  //##JVR This should probably be in the base class TDBIDataConnection
  if not Value then begin
    FieldCount := 0;
  end;
end;  { SetActive }


// _____________________________________________________________________________
{**
  Jvr - 04/10/2002 11:13:38.<P>
}
procedure TDBICustomListDataConnection.SetObjectValidationProc(
  const Value: TDBIMethodName
  );
begin
  FObjectValidationProc := Value;

  if (FObjectValidationProc <> '') then begin
    Include(FOptions, osObjectValidation);
  end;
end;  { SetObjectValidationProc }


// _____________________________________________________________________________
{**
  Jvr - 04/10/2002 11:34:27.<P>
}
procedure TDBICustomListDataConnection.SetOptions(
  const Value: TDBIListDataConnectionOptions
  );
begin
  FOptions := Value;
end;  { SetOptions }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:34:11<P>
}
procedure TDBICustomListDataConnection.SetItem(Index: Integer; const Value: TObject);
begin
  raise EDBINotImplementedException.Create(Self, 'SetItem::1625',
    'SetItem has NOT been implemented in the derived class!', []);
end;  { SetItem }


// _____________________________________________________________________________
{**
  Jvr - 03/12/2002 15:01:32.<P>
}
procedure TDBICustomListDataConnection.DataEvent(
  DataObject: TObject;
  Event: TDBIObjectDataEvent
  );
var
  Method: TMethod;

begin
  if (DataObject <> nil) and (FObjectDataEventProc <> '') then begin
    Method.Code := DataObject.MethodAddress(FObjectValidationProc);
    if (Method.Code = nil) then begin
      raise EDBIException.Create(Self, 'DataEvent::1645',
        'Object DataEvent procedure "%s" not found', [FObjectDataEventProc]
        );
    end;

    Method.Data := DataObject;
    TDBIObjectDataEventProc(Method)(Owner, Event);
  end;  { if }
end;


// _____________________________________________________________________________
{**
  Jvr - 03/10/2002 16:06:54.<P>
}
procedure TDBICustomListDataConnection.ValidateObject(DataObject: TObject);
var
  Method: TMethod;

begin
  if (DataObject <> nil) and (FObjectValidationProc <> '') then begin
    Method.Code := DataObject.MethodAddress(FObjectValidationProc);
    if (Method.Code = nil) then begin
      raise EDBIException.Create(Self, 'ValidateObject::1670',
        'Object validation procedure "%s" not found', [FObjectValidationProc]
        );
    end;

    Method.Data := DataObject;
    TDBIObjectValidationProc(Method);
  end;  { if }
end;  { ValidateObject }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 11:08:46 - Initial code<BR>
  Jvr - 20/09/2002 13:15:53 - Added field validation via a validation Object<BR>
  Jvr - 28/11/2002 15:01:39 - Added WriteThrough mode<P>
}
procedure TDBICustomListDataConnection.Update(const Position: Integer; const Buffer);
var
  DataObject: TObject;
//##JVR  PRevertBuffer: TDBIRecordBuffer;

begin
  try
    if (Position < 0) or (Position >= GetCount(dsRecAll)) then begin
      raise EDBIException.Create(Self, 'Update::1695', 'Record "%d" out of legal range', [Position]);
    end;

    DataObject := Items[Position];

    if (DataObject = nil) then begin
      raise EDBIException.Create(Self, 'Update::1700', 'Item "%d" is nil', [Position]);
    end
    else if (@Buffer <> nil) then begin
      ValidateObject(FValidationObject);
      DataEvent(DataObject, dePost);
      PutData(DataObject, Buffer);
    end;  { if }

(*##JVR - This is possibly the prefereable thing to do ???
      PRevertBuffer := AllocMem(SizeOf(Char) * LogicalBufferSize);
      try
        // Save the original data
        GetData(DataObject, PRevertBuffer^);
        try
          // Update the object with the new data
          PutData(DataObject, Buffer);

          // Then send the post event to the data object
          ValidateObject(DataObject);

        except
          // Revert the object back to its previous state using the saved data
          PutData(DataObject, PRevertBuffer^);

          // re-raise the exception
          raise;
        end;  { try..except }
      finally
        FreeMem(PRevertBuffer);
      end;
    end;  { if }
*)

    // Pass in the record number (not the record position)
    NotifyDataEventCallBacks(Position+1, dbiRecordModified, @Buffer);

  finally
    // Cleanup validation object
    FValidationObject.Free;
    FValidationObject := nil;
  end;  { try..finally }
end;  { Update }


// _____________________________________________________________________________
{**
  Jvr - 21/05/2002 20:36:03.<P>
}
procedure TDBICustomListDataConnection.Reset;
begin
//##JVR  GetMetaData;
end;  { Reset }


// _____________________________________________________________________________
{**
  If Initialise is True
    GetData is called and the Buffer will be updated from the Validation object,

  Otherwise
    SetData is called and the Validation object is updated from the Buffer.

  Jvr - 24/09/2002 12:11:00 - Initial code<BR>
}
procedure TDBICustomListDataConnection.SyncRecordBuffer(
  var Buffer;
  const Initialise: Boolean
  );
begin
  if not (osObjectValidation in FOptions) then begin
    Exit;
  end;

  if (FValidationObject = nil) then begin
    FValidationObject := CreateObject(coValidate);
  end;

  Assert(FValidationObject <> nil);

  if Initialise then begin
    GetData(FValidationObject, Buffer);
  end
  else begin
    PutData(FValidationObject, Buffer);
  end;  { if }
end;  { SyncRecordBuffer }


// _____________________________________________________________________________
{**
  Jvr - 27/05/2002 16:22:06.<P>
}
procedure TDBICustomListDataConnection.ValidateMetaData;
begin
  if GetClass(ClassTypeName) = nil then begin
    raise EDBIException.Create(Self, 'ValidateMetaData::1795', SClassNotFound, [ClassTypeName]);
  end;
end;  { ValidateMetaData }


// _____________________________________________________________________________
{**
  Provides field validation via the DataObject's property Setters.

  Jvr - 20/09/2002 11:29:27 - Initial Code<BR>
}
procedure TDBICustomListDataConnection.ValidateField(
  const Position: Integer;
  const FieldNo: Word;
  pFldBuf: Pointer
  );
var
  FieldName: TDBIFieldName;
  PropInfo: PPropInfo;
  PutFieldProc: TPutObjectFieldProc;

begin
  // If object validation is not turned on then Exit
  if not (osObjectValidation in FOptions) then begin
    Exit;
  end;  { if }


  // If the validation object reference is nil the we have either
  // - just inserted an object
  //     or
  // - just started editing an object
  if (FValidationObject = nil) then begin
    FValidationObject := CreateObject(coValidate);
  end;

  Assert(FValidationObject <> nil);


  FieldName := GetFieldName(FieldNo);
  PropInfo := GetPropInfo(FValidationObject, FieldName);
  if (PropInfo <> nil) then begin
    // This check sometimes causes a failure
    // on legal constructs. It would be removed
    // if it wasn't so handy.
    if not Assigned(PropInfo.SetProc) then begin
      if (osErrorOnReadonlyProperty in Options) then begin
        raise EDBIException.Create(Self, 'ValidateField::1840',
          SErrorReadOnlyProperty,
          [FValidationObject.ClassName + '.' + TDBIPropName(PropInfo.Name)]
          );
      end

      // Otherwise Exit, field needs no validation, it is a readonly field
      else begin
        Exit;
      end;
    end;  { if }

    case FieldProps[FieldNo].iFldType of
      fldUNKNOWN:    PutFieldProc := PutFieldUnknown;
      fldZSTRING:    PutFieldProc := PutFieldString;
      fldDATE:       PutFieldProc := PutFieldDateTime;
      fldBLOB:       PutFieldProc := PutFieldMemo;
      fldBOOL:       PutFieldProc := PutFieldBoolean;
      fldINT16:      PutFieldProc := PutFieldInteger;
      fldINT32:      PutFieldProc := PutFieldInteger;
      fldFLOAT:      PutFieldProc := PutFieldFloat;
      fldBCD:        PutFieldProc := PutFieldBCD;
      fldBYTES:      PutFieldProc := PutFieldUnknown;
      fldTIME:       PutFieldProc := PutFieldDateTime;
      fldTIMESTAMP:  PutFieldProc := PutFieldDateTime;
      fldUINT16:     PutFieldProc := PutFieldInteger;
      fldUINT32:     PutFieldProc := PutFieldInteger;
      fldFLOATIEEE:  PutFieldProc := PutFieldFloat;
      fldVARBYTES:   PutFieldProc := PutFieldUnknown;
      fldLOCKINFO:   PutFieldProc := PutFieldUnknown;
      fldCURSOR:     PutFieldProc := PutFieldUnknown;
      fldINT64:      PutFieldProc := PutFieldInteger;
      fldUINT64:     PutFieldProc := PutFieldInteger;
      fldADT:        PutFieldProc := PutFieldADT;
      fldARRAY:      PutFieldProc := PutFieldUnknown;
      fldREF:        PutFieldProc := PutFieldUnknown;
      fldTABLE:      PutFieldProc := PutFieldDataset;
      fldDATETIME:   PutFieldProc := PutFieldUnknown;
      fldFMTBCD:     PutFieldProc := PutFieldBCD;
      fldWIDESTRING: PutFieldProc := PutFieldString;
      fldINT8:       PutFieldProc := PutFieldInteger;
      fldUINT8:      PutFieldProc := PutFieldInteger;
      fldSINGLE:     PutFieldProc := PutFieldFloat;
      fldUNICODE:    PutFieldProc := PutFieldString;
    else
      PutFieldProc := PutFieldUnknown;
    end;  { case }

    PutFieldProc(FValidationObject, pFldBuf, FieldName, FieldNo);
  end;  { if }

{##JVR
  // Now Free the Blob Objects in the BlobData list (if there are any)
  // This will free any Blob Objects left hanging around by GetData as well
  // The Objects Blob placed in FBlobData by GetData should
  // probably be Cleared elsewhere but I don't know of an appropriate place.
  FBlobData.Clear;
}
end;  { ValidateField }


// _____________________________________________________________________________
{**
  Jvr - 28/01/2003 15:57:24.<P>
}
procedure TDBICustomListDataConnection.NotifyDataEvent(
  DataObject: TObject;
  Event: TDBIDataChangeEventType;
  ItemIndex: Integer
  );
var
  pRecBuf: TDBIRecordBuffer;
  FieldNo: Integer;

begin
  if (ItemIndex < 0) then begin
    if (Event = dbiRecordInserted) then begin
      ItemIndex := GetCount(dsRecAll)-1;
    end
    else begin
      ItemIndex := IndexOfItem(DataObject);
      if (ItemIndex < 0) then begin
        raise EDBIException.Create(Self, 'NotifyDataEvent::1925', 'Object not found', []);
      end;
    end;
  end;

  GetMem(pRecBuf, LogicalBufferSize);
  try
    GetData(DataObject, pRecBuf^);

    for FieldNo := 0 to FieldCount-1 do begin
      Indices.AddUpdatedField(@(FieldProps[FieldNo]));
    end;

    NotifyDataEventCallBacks(ItemIndex+1, Event, pRecBuf);
  finally
    FreeMem(pRecBuf);
  end;
end;  { NotifyDataEvent }


// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 13:22:01.<P>
}
function TDBICustomListDataConnection.Lock: Boolean;
begin
  Result := False;
end;  { Lock }


// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 13:22:27.<P>
}
function TDBICustomListDataConnection.Unlock: Boolean;
begin
  Result := False;
end;  { Unlock }


// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 13:23:19.<P>
}
function TDBICustomListDataConnection.LockRecord(const Position: Integer): Boolean;
begin
  Result := True;
end;  { LockRecord }


// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 13:23:56.<P>
}
function TDBICustomListDataConnection.UnlockRecord(const Position: Integer): Boolean;
begin
  Result := True;
end;  { UnlockRecord }


// _____________________________________________________________________________
{**
  Jvr - 12/02/2001 12:15:46.<P>
}
procedure TDBICustomListDataConnection.GetBlob(
  const Position: LongWord;
  const FieldNo: LongWord;
  const OffSet: LongWord;
  PData: Pointer;
  out Size: LongWord
  );
var
  BlobText: TDBIString;
  BlobObject: TObject;
  BlobClassName: String;

begin
  BlobClassName := '';

  // Get the DataObject using the Position as the address of the object (0=Blank)
  if (Position = 0) then begin
    Size := 0;
    Exit;
  end;

  try
    // If PData parameter is nil, then only return the Size value
    // indicating the size of the data
    BlobObject := TObject(Position);
    Assert(Assigned(BlobObject));
    Assert(Integer(FieldNo) < Length(FieldProps));
    BlobClassName := BlobObject.ClassName;

    if (BlobObject is TDBIBlob) then begin
      BlobText := (BlobObject as TDBIBlob).AsString;
    end
    else begin
      BlobText := DBIAnsiGetStrProp(BlobObject, GetFieldName(FieldNo));
    end;

    Size := Length(BlobText);
    if Assigned(PData) and (Size > 0) then begin
      Move(PAnsiChar(BlobText)^, PData^, Size);
    end;

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'GetBlob::2030',
        'Unable to Get Blob Data for Field %s[%d]', [BlobClassName, FieldNo]
        );
  end;
end;  { GetBlob }


// _____________________________________________________________________________
{**
  Jvr - 12/02/2001 12:21:14.<P>
}
function TDBICustomListDataConnection.PutBlob(
  const Position: LongWord;
  const FieldNo: LongWord;
  const OffSet: LongWord;
  PData: Pointer;
  const Size: LongWord
  ): Integer;
begin
  Result := 0;  // Blank

  try
    // Create a wrapper object for the blob data and return the address of this object
    if (Size > 0) then begin
      Result := Integer(TDBIBlob.Create(PData, Size));
    end;
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'PutBlob::2060',
        'Unable to Put Blob Data for FieldNo "%d"', [FieldNo]
        );
  end;
end;  { PutBlob }


// _____________________________________________________________________________
{**
  Jvr - 27/02/2001 16:59:38.<P>
}
procedure CopyBlobs(
  const Position: Integer;
  var RecordBuffer;
  Source: TDBIDataConnection;
  Target: TDBIDataConnection
  );

var
  FieldNo: Integer;
  FieldData: Integer;
  Size: LongWord;
  PRecBuf: Pointer;
  PFldBuf: Pointer;
  PData: Pointer;

begin
  Size := 0;
  
  // Append Blob data to target
  for FieldNo := 0 to Source.FieldCount-1 do begin
    if (Source.FieldProps[FieldNo].iFldType = fldBlob) { IsBlob } then begin
      PRecBuf := @(TDBIRecordBuffer(@RecordBuffer)[Source.FieldProps[FieldNo].iFldOffsInRec]);
      PFldBuf := @FieldData;

      // Get the Blob position from the logical record buffer
      Move(PRecBuf^, PFldBuf^, Source.FieldProps[FieldNo].iFldLen);

      if (FieldData > 0) then begin
        // Get the blob Size
        Source.GetBlob(FieldData, FieldNo, 0, nil, Size);

        // Get the Blob Data
        PData := AllocMem(Size);
        try
          Source.GetBlob(FieldData, FieldNo, 0, PData, Size);
          FieldData := 0; { New Blob }
          FieldData := Target.PutBlob(FieldData, FieldNo, 0, PData, Size);
        finally
          FreeMem(PData);
        end;  { try..finally }
      end

      // No Blob Data
      else begin
        FieldData := 0;
      end;  { if }

      // Set the new Blob position in the logical record buffer
      Move(PFldBuf^, PRecBuf^, Source.FieldProps[FieldNo].iFldLen);
    end;  { if }
  end;  { for }
end;  { CopyBlobs }


// _____________________________________________________________________________
{**
  Jvr - 12/06/2013 10:29:22 - Initial code.<br>
}
procedure TDBICustomListDataConnection.LoadFromStream(
  AStream: TStream;
  const Format: TDBIDataFormat
  );
begin
  if (Format <> dfXbase) and (Format <> dfXbasePlus) then begin
    raise EDBIException.Create(Self, 'LoadFromStream::2135',
      'Only Xbase streams are supported', []
      );
  end
  else begin
    LoadFromXBaseStream(AStream);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/07/2002 16:02:55 - Initial code.<br>
  Jvr - 09/12/2004 18:29:49 - Replaced RecordAttribute with DataInfo.<p>
}
procedure TDBICustomListDataConnection.LoadFromXBaseStream(AStream: TStream);
var
  LocalConnection: TDBIXBaseDataConnection;
  RecordBuffer: TDBIRecordBuffer;
  DataInfo: TDBIDataInfo;
//##JVR  RecordAttribute: DSAttr;
  Position: Integer;

begin
  LocalConnection := TDBIXBaseDataConnection.Create(Owner);
  try
    LocalConnection.DataStream := AStream;
    LocalConnection.Open;

    // Get and set the DatasetProps
    FieldCount := LocalConnection.FieldCount;
    LogicalBufferSize := LocalConnection.LogicalBufferSize;

    // Get & set the FieldProps
    FieldProps := LocalConnection.FieldProps;

    // Create record buffer
    RecordBuffer := AllocMem(LogicalBufferSize);
    try
      // Now read in all the records
      for Position := 0 to LocalConnection.GetCount(dsRecAll) - 1 do begin
        // Get record from source
        {RecordAttribute := }LocalConnection.Get(Position, RecordBuffer^, @DataInfo);
//##JVR        if (RecordAttribute and StatusFilter) = StatusFilter then begin
//##JVR          CopyBlobs(Position, RecordBuffer^, LocalConnection, Self);

          // Append data  to target
          Append(RecordBuffer^);
//##JVR        end;
      end;  { for }
    finally
      FreeMem(RecordBuffer);
    end;
    LocalConnection.Close;
  finally
    LocalConnection.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Load a ObjectList DataConnection from a file, Xml or dbf.
}
procedure TDBICustomListDataConnection.LoadFromFile(
  AFileName: String;
  const Format: TDBIDataFormat
  );
begin
  if (Format in [dfXbase, dfXbasePlus]) then begin
    LoadFromXBaseFile(AFileName);
  end
  else begin
    raise EDBIException.Create(Self, 'LoadFromFile::2205', 'Unsupported file format', []);
  end;
end;  { LoadFromFile }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 14:30:35 - Initial code.<br>
  Jvr - 15/05/2001 14:21:05 - Added StatusFilter filtering functionality.<br>
  Jvr - 09/12/2004 18:39:13 - Replaced RecordAttribute with DataInfo.<p>
}
procedure TDBICustomListDataConnection.LoadFromXBaseFile(const AFileName: TFileName);
var
  LocalConnection: TDBIXBaseDataConnection;
  RecordBuffer: TDBIRecordBuffer;
  DataInfo: TDBIDataInfo;
//##JVR  RecordAttribute: DSAttr;
  Position: Integer;

begin
  LocalConnection := TDBIXBaseDataConnection.Create(Owner);
  try
    LocalConnection.FileName := AFileName;
    LocalConnection.Exclusive := Exclusive;
    LocalConnection.ReadOnly := ReadOnly;
    LocalConnection.Open;

    // Get and set the DatasetProps
    FieldCount := LocalConnection.FieldCount;
    LogicalBufferSize := LocalConnection.LogicalBufferSize;

    // Get & set the FieldProps
    FieldProps := LocalConnection.FieldProps;

    // Create record buffer
    RecordBuffer := AllocMem(LogicalBufferSize);
    try
      // Now read in all the records
      for Position := 0 to LocalConnection.GetCount(dsRecAll) - 1 do begin
        // Get record from source
        {RecordAttribute := }LocalConnection.Get(Position, RecordBuffer^, @DataInfo);
//##JVR        if (RecordAttribute and StatusFilter) = StatusFilter then begin
          CopyBlobs(Position, RecordBuffer^, LocalConnection, Self);

          // Append data  to target
          Append(RecordBuffer^);
//##JVR        end;
      end;  { for }
    finally
      FreeMem(RecordBuffer);
    end;
    LocalConnection.Close;
  finally
    LocalConnection.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 09/12/2004 18:50:51 - Initial code.<p>
}
procedure TDBICustomListDataConnection.SaveToStream(
  AStream: TStream;
  const Format: TDBIDataFormat
  );
var
  LocalConnection: TDBIXBaseDataConnection;
  RecordBuffer: TDBIRecordBuffer;
  DataInfo: TDBIDataInfo;
//##JVR  RecordAttribute: Byte;
  Position: Integer;

begin
  if (Format = dfXML) then begin
    raise EDBIException.Create(Self, 'SaveToStream::2280', 'Only XBase streams are supported', []);
  end;

  LocalConnection := TDBIXBaseDataConnection.Create(Owner);
  try
    AStream.Size := 0;
    LocalConnection.DataStream := AStream;

    // Extended fieldnames support
    if (Format = dfXbasePlus) then begin
      LocalConnection.Flags := LocalConnection.Flags + [xfExtendedFields];
    end;

    // Get & set the FieldProps
    LocalConnection.CreateDS(FieldCount, pDSFLDDesc(FieldProps), TDBINameBuffer(TDBIString(ClassTypeName)));
    LocalConnection.Open;

    // Create record buffer
    RecordBuffer := AllocMem(LogicalBufferSize);
    try
      // Now read in all the records
      for Position := 0 to GetCount(dsRecAll) - 1 do begin
        // Get record from source, if not a deleted record then copy
        {RecordAttribute := }Get(Position, RecordBuffer^, @DataInfo);
//##JVR        if (RecordAttribute and StatusFilter) = StatusFilter then begin
//##JVR          CopyBlobs(Position, RecordBuffer^, Self, LocalConnection);

          // Append RecordBuffer to target
          LocalConnection.Append(RecordBuffer^);
//##JVR        end;
      end;  { for }

    finally
      FreeMem(RecordBuffer);
    end;  { try..finally }

    LocalConnection.Close;
  finally
    LocalConnection.Free;
  end;  { try..finally }
end;  { SaveToStream }


// _____________________________________________________________________________
{**
  Jvr - 23/02/2001 16:16:43 - Initial code<BR>
  Jvr - 14/02/2003 15:54:45 - Added limited support for extended fieldnames<P>
}
procedure TDBICustomListDataConnection.SaveToFile(
  AFileName: String;
  const Format: TDBIDataFormat
  );
var
  LocalConnection: TDBIXBaseDataConnection;
  RecordBuffer: TDBIRecordBuffer;
  DataInfo: TDBIDataInfo;
//##JVR  RecordAttribute: Byte;
  Position: Integer;

begin
  if (Format = dfXML) then begin
    raise EDBIException.Create(Self, 'SaveToFil::2340', 'Saving to XML not supported Yet!', []);
  end;

  LocalConnection := TDBIXBaseDataConnection.Create(Owner);
  try
    LocalConnection.FileName := AFileName;

    // Extended fieldnames support
    if (Format = dfXbasePlus) then begin
      LocalConnection.Flags := LocalConnection.Flags + [xfExtendedFields];
    end;

    // Get & set the FieldProps
    LocalConnection.CreateDS(FieldCount, pDSFLDDesc(FieldProps), TDBINameBuffer(TDBIString(ClassTypeName)));
    LocalConnection.Open;

    // Create record buffer
    RecordBuffer := AllocMem(LogicalBufferSize);
    try
      // Now read in all the records
      for Position := 0 to GetCount(dsRecAll) - 1 do begin
        // Get record from source, if not a deleted record then copy
        {RecordAttribute := }Get(Position, RecordBuffer^, @DataInfo);
//##JVR        if (RecordAttribute and StatusFilter) = StatusFilter then begin
          CopyBlobs(Position, RecordBuffer^, Self, LocalConnection);

          // Append RecordBuffer to target
          LocalConnection.Append(RecordBuffer^);
//##JVR        end;
      end;  { for }

    finally
      FreeMem(RecordBuffer);
    end;  { try..finally }

    LocalConnection.Close;
  finally
    LocalConnection.Free;
  end;  { try..finally }
end;  { SaveToFile }


// _____________________________________________________________________________
{**
  Jvr - 21/09/2005 11:36:17 - Initial code.<br>
}
function TDBICustomListDataConnection.GetClassKind: TPersistentClass;
begin
  Result := Classes.GetClass(FClassTypeName);
end;  { GetClassKind }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 16:33:59<P>
}
procedure TDBICustomListDataConnection.SetClassTypeName(const Value: String);
begin
  FClassTypeName := Value;
end;  { SetClassTypeName }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:26:48<P>
}
function TDBICustomListDataConnection.GetFieldName(const FieldNo: Integer): TDBIFieldName;
begin
  Result := TDBIFieldName(FieldProps[FieldNo].szName);
end;


// _____________________________________________________________________________
{**
  Jvr - 03/05/2002 13:45:12 - Initial code.<br>
  Jvr - 30/03/2009 15:20:52 - Added Currency field type.<br>
}
type
  TDBIGetMetaDataProc = procedure(
    PropInfo: PPropInfo;
    var Size: Integer;
    var Precision: Integer
    ) of object;

procedure TDBICustomListDataConnection.GetMetaData;
var
  FieldNo: Integer;
  FieldDescs: TFieldDescList;

  function IsNullFlagField(PFieldDesc: pDSFLDDesc): Boolean;
  begin
    Result := DBICompareText(PFieldDesc^.szName, FieldName_NullFlags) = 0;
  end;


  // For the object list dataset either all fields are nullable or they are not!
  function InitializeNullFlags(const IsNullable: Boolean; const FieldCount: Word): Boolean;
  var
    NullFlagsIndex: Integer;
    FieldNo: Integer;

  begin
    NullFlagsIndex := 0;
    
    // Null field flag information
    Result := (FieldCount > 0) and IsNullable;

    NullFlags.IsNullable := Result;
    for FieldNo := 0 to FieldCount - 1 do begin
      // Create Index values for the physical fields Null-Flags
      if NullFlags.SetNullIndex(FieldNo, Result, NullFlagsIndex) then begin
        Inc(NullFlagsIndex);
      end;
    end;
  end;


  function AddFieldDesc(PropInfo: PPropInfo; var AFieldNo: Integer): pDSFLDDesc;
  var
    Attributes: TFieldAttributes;
    FieldName: TDBIFieldName;

  begin
    // Adjust the array size
    SetLength(FieldDescs, AFieldNo+1);
    Result := @FieldDescs[AFieldNo];

    // Clear the field description
    FillChar(Result^, SizeOf(DSFLDDesc), 0);

    // Assign the field name
    Result^.iFieldID := AFieldNo + 1;
    FieldName := TDBIFieldName(PropInfo.Name);
    StrLCopy(Result^.szName, PDBIChar(TDBIString(FieldName)), SizeOf(Result^.szName));

    // Set Field Attributes
    Attributes := [];

    if IsNullFlagField(Result) then begin
      Include(Attributes, db.faReadOnly);
      Include(Attributes, db.faHiddenCol);
    end
    else begin
      // If property is readonly then make field readonly
      if not Assigned(PropInfo.SetProc) then begin
        Include(Attributes, db.faReadonly);
      end;

      // If property stored is false then set Attribute to db.faUnNamed to indicate not to include it
      if not Assigned(PropInfo.StoredProc) then begin
        Include(Attributes, db.faUnNamed);
      end;

{##NULL
      // Is Field nullable
      if NullFlags.IsNullable then begin
        Exclude(Attributes, db.faRequired);
      end
      else begin
        Include(Attributes, db.faRequired);
      end;
//}
    end;

    // Initialise the Fieldtype according to the map
    // Below we will refine the actual types
    Result^.iFldType := FieldKindMap[PropInfo^.PropType^.Kind];
    Result^.iFldSubType := FieldSubKindMap[PropInfo^.PropType^.Kind];
    Result^.iFldAttr := PByte(@Attributes)^;
    Result^.iUnits1 := 0;
    Result^.iUnits2 := 0;

    // if specified in Options to use AnsiString type as default,
    // Map the iFldType to "fldZSTRING" when defined as "fldWIDESTRING"
    if (Result^.iFldType = fldWIDESTRING) and (osStringFieldsDefaultToAnsi in FOptions) then begin
      Result^.iFldType := fldZSTRING;
      Result^.iFldSubType := fldUNKNOWN;
    end;

    // If iFldType is of type "fldWIDESTRING" then map to "fldUNICODE"
    if Result^.iFldType = fldWIDESTRING  then begin
      Result^.iFldType := fldUNICODE;
    end;

    // If fieldtype is not unknown then increment the field number
    if (Result^.iFldType <> fldUNKNOWN) then begin
      Inc(AFieldNo);
    end;

    // If field type is invalid then raise exception
    if (Result^.iFldType = fldUNKNOWN) then begin
      raise EDBIException.Create(Self, 'GetMetaData::AddFieldDesc::2525',
        'Unsupported property kind "%s" for field %s', [
        TypInfo.GetEnumName(TypeInfo(TTypeKind), Ord(PropInfo^.PropType^.Kind)),
        FieldName
        ]);
    end;
  end;  { AddFieldDesc }


  function EncodeFieldDescs(DataClass: TClass; var AFieldNo: Integer): Integer;
  var
    PropertyList: TList;
    PropInfo: PPropInfo;
    TypeData: PTypeData;
    PropClass: TClass;
    Index: Integer;
    PFieldDesc: pDSFLDDesc;
    Method: TMethod;
    IsNullable: Boolean;

  begin
    Result := AFieldNo;
    IsNullable := False;
    PFieldDesc := nil;

    if (DataClass = nil) then begin
      Exit;
    end;

    PropertyList := TList.Create;
    try
      DBIGetPropertyList(DataClass.ClassInfo, PropertyList);

      // Iterate the published properties and
      // Create Field-description records for the valid entries
      for Index := 0 to PropertyList.Count - 1 do begin
        PropInfo := PropertyList[Index];
        TypeData := TDBIPropType.GetTypeData(PropInfo);

        // If it is NOT a class type property then add a new Field-Description record
        if (PropInfo^.PropType^.Kind <> tkClass) then begin
          PFieldDesc := AddFieldDesc(PropInfo, AFieldNo);
        end;

        // Update remaining FieldDesc attributes based on the property type
        case PropInfo^.PropType^.Kind of
          tkString, tkLString, tkWString {$ifdef DELPHIXE2}, tkUString {$endif} {$ifdef fpc} , tkAString {$endif} : begin
            if (PFieldDesc^.iFldType = fldZSTRING) then begin
              PFieldDesc^.iUnits1 := FStringFieldSize;
            end
            else begin
              PFieldDesc^.iUnits1 := FStringFieldSize * 2;
            end;
          end;  { tkString, tkLString, tkWString, tkAString, tkUString }

          tkChar, tkWChar: begin
            if (PFieldDesc^.iFldType = fldZSTRING) then begin
              PFieldDesc^.iUnits1 := 1;
            end
            else begin
              PFieldDesc^.iUnits1 := 2;
            end;
          end;  { tkChar, tkWChar }

          tkFloat: begin
            // Is it a TDateTime?
            if DBICompareText(PropInfo^.PropType^.Name, 'TDateTime') = 0 then begin
              PFieldDesc^.iFldType := fldTimeStamp;
            end

            // Otherwise it's a floating point field
            else begin
              PFieldDesc^.iFldType := fldFloat;
              PFieldDesc^.iUnits2 := 4;

              // Check if the field is of type Currency
              if Assigned(TypeData) then begin
                PFieldDesc^.iFldType := FieldFloatTypeMap[TypeData^.FloatType];

                // Check if the field is of type Currency
                if (TypeData^.FloatType = ftCurr) then begin
                  PFieldDesc^.iFldSubType := fldstMONEY;
                end;
              end;
            end;
          end;  { tkFloat }

          tkInteger: begin
            // Check if the field is of type Currency
            if Assigned(TypeData) then begin
              PFieldDesc^.iFldType := FieldOrdTypeMap[TypeData^.OrdType];
            end;
          end;  { tkInteger }

          tkInt64: begin
            // Special case - _NULLFLAGS
            IsNullable := IsNullFlagField(PFieldDesc);
            if IsNullable then begin
              PFieldDesc^.iFldType := fldBYTES;
              PFieldDesc^.iFldSubType := fldstNONE;
              PFieldDesc^.iFldLen := SizeOf(Int64);
              PFieldDesc^.iUnits1 := 4;
              PFieldDesc^.iUnits2 := 0;
              PFieldDesc^.iUnits2 := 0;
            end;
          end;  { tkInt64 }

          tkSet, tkEnumeration: begin
            if DBICompareText(PropInfo^.PropType^.Name, 'Boolean') = 0 then begin
              PFieldDesc^.iFldType := fldBOOL;
            end
            else begin
              PFieldDesc^.iFldType := fldINT32;
            end;
          end;  { tkSet, tkEnumeration }

          tkClass: begin
            // Get the property class type (it must be registered)
            PropClass := TypeData^.ClassType;
            Assert(PropClass <> nil);

            // It's a 'TObjectList' or derived from it
            if PropClass.InheritsFrom(TObjectList) then begin
              PFieldDesc := AddFieldDesc(PropInfo, AFieldNo);
              PFieldDesc^.iFldType := fldTable;
              PFieldDesc^.iUnits1 := 0; // Possible candidate for sub recordcount

              // Create the Childefs for the nested class
              //##CHILDDEFS - CreateFieldDefs(GetClass(PropInfo.Name), FieldDef.ChildDefs);

            end

            // It's derived from 'TStrings'
            else if PropClass.InheritsFrom(TStrings) then begin
              PFieldDesc := AddFieldDesc(PropInfo, AFieldNo);
              PFieldDesc^.iFldType := fldBlob;
              PFieldDesc^.iFldSubType := fldstMemo;
            end

            // Otherwise it's some other object - use 'Abstract Data Type'
            else begin
              if (PropClass.ClassInfo <> nil) then begin
                if TypInfo.GetTypeData(PropClass.ClassInfo)^.PropCount > 0 then begin
                  PFieldDesc := AddFieldDesc(PropInfo, AFieldNo);
                  PFieldDesc^.iFldType := fldADT;
                  PFieldDesc^.iUnits1 := TypInfo.GetTypeData(PropClass.ClassInfo)^.PropCount;
                  EncodeFieldDescs(PropClass, AFieldNo);
                end;
              end;
            end;
          end;  { tkClass }

        end;

        // Call the object published _GetMetaData class method if there is one
        Method.Code := DataClass.MethodAddress('_GetMetaData');
        if (Method.Code <> nil) then begin
          Method.Data := DataClass;
          TDBIGetMetaDataProc(Method)(PropInfo, PFieldDesc^.iUnits1, PFieldDesc^.iUnits2);
        end;

      end;  { for }

    finally
      PropertyList.Free;
    end;  { try..finally }

    Result := AFieldNo - Result;

    InitializeNullFlags(IsNullable, Length(FieldDescs));
  end;  { EncodeFieldDescs }


begin
  FieldNo := 0;
  EncodeFieldDescs(GetClass(ClassTypeName), FieldNo);
  SetFieldProps(FieldDescs);
end;  { GetMetaData }


end.

