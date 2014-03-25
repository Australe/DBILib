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

  Notes:
                 TDBIDataConnection
                   - Position * RecordSize + DataOffset is the physical
                     file address of the Data record
                   - Position is '0' based

                 TDBICursor
                   - Position is the current Record No / Index Item
                     and is '1' based
                   - Position = 0 => BOF
                   - Position = RecordCount + 1 => EOF

  Change History:
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 13/01/1999 10:01:07 | Jvr | Initial Release
  1.1 | 09/01/1999 11:55:42 | Jvr | Redesigned Interface
  1.2 | 11/02/1999 10:30:00 | Jvr | Interface comprises TDBIBase & TDBICursor
  1.3 | 21/11/2000 16:49:26 | Jvr | Merged DBIXBase
  1.4 | 02/12/2012 07:48:12 | Jvr | Unicode conversion
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBIInterfaces;

{$I DBICompilers.inc}

{$ifdef fpc}
  {$asmmode intel}
  {$mode delphi}
{$endif}

{ .$define _AGGREGATES True}

interface

uses
  Classes, SysUtils, TypInfo, DBIIntfConsts, DB, Contnrs, DBIConst, DBIStrings,
  DBIUTils, DBIFilters, DBIIndices;

type
  TDBIFieldDef = class(TFieldDef)
{$ifdef fpc}
  protected
    function GetChildDefs: TFieldDefs;
    procedure SetChildDefs(Value: TFieldDefs);

  public
    function HasChildDefs: Boolean;

    property ChildDefs: TFieldDefs read GetChildDefs write SetChildDefs stored HasChildDefs;
{$endif}
  end;


{$ifdef DELPHI2009}
  TDBIByte = Byte;
  PDBIByte = PByte;

  TDBIRecordElement = Byte;
  TDBIRecordBuffer = PByte;
{$else}
  TDBIByte = AnsiChar;
  PDBIByte = PAnsiChar;

  TDBIRecordElement = AnsiChar;
  TDBIRecordBuffer = PAnsiChar;
{$endif}


{$ifdef DELPHIXE2}
  TDBIDataEventInfo = NativeInt;
{$else}
  TDBIDataEventInfo = LongInt;
{$endif}


{$ifdef DELPHIXE3}
  TDBIBookmark = TBookmark;
  TDBIRecordData = PByte;
  TDBIValueBuffer = TArray<Byte>;
{$else}
  TDBIBookmark = Pointer;
  TDBIRecordData = Pointer;
  TDBIValueBuffer = Pointer;
{$endif}


  TDBIFieldBuffer = PByte;

type
  TDBIFieldKindMap = array[TTypeKind] of Byte;

const
{$ifdef fpc}
  FieldKindMap: TDBIFieldKindMap = (
    fldUNKNOWN,                        // tkUnknown,
    fldINT32,                          // tkInteger,
    fldZSTRING,                        // tkChar,
    fldINT32,                          // tkEnumeration,
    fldFLOAT,                          // tkFloat,
    fldINT32,                          // tkSet,
    fldUNKNOWN,                        // tkMethod,
    fldZSTRING,                        // tkSString,
    fldZSTRING,                        // tkLString,
    fldZSTRING,                        // tkAString,
    fldZSTRING,                        // tkWString,
    fldUNKNOWN,                        // tkVariant,
    fldARRAY,                          // tkArray,
    fldADT,                            // tkRecord,
    fldUNKNOWN,                        // tkInterface,
    fldTABLE,                          // tkClass,
    fldADT,                            // tkObject,
    fldZSTRING,                        // tkWChar,
    fldBOOL,                           // tkBool,
    fldINT64,                          // tkInt64,
    fldINT64,                          // tkQWord,
    fldUNKNOWN,                        // tkDynArray,
    fldUNKNOWN,                        // tkInterfaceRaw,
    fldUNKNOWN,                        // tkProcVar,
    fldWIDESTRING,                     // tkUString,
    fldWIDESTRING,                     // tkUChar,
    fldUNKNOWN                         // tkHelper
    );
{$else}
  FieldKindMap: TDBIFieldKindMap = (
    fldUNKNOWN,                        // tkUnknown
    fldINT32,                          // tkInteger
    fldZSTRING,                        // tkChar
    fldINT32,                          // tkEnumeration
    fldFLOAT,                          // tkFloat
    fldZSTRING,                        // tkString, tkAString
    fldINT32,                          // tkSet
    fldTABLE,                          // tkClass
    fldUNKNOWN,                        // tkMethod
    fldWIDESTRING,                     // tkWChar
    fldZSTRING,                        // tkLString
    fldWIDESTRING,                     // tkWString
    fldUNKNOWN,                        // tkVariant
    fldARRAY,                          // tkArray
    fldUNKNOWN,                        // tkRecord
    fldUNKNOWN,                        // tkInterface
    fldINT64,                          // tkInt64
    fldUNKNOWN                         // tkDynArray
  {$ifdef DELPHIXE2} ,
    fldWIDESTRING,                     // tkUString
    fldREF,                            // tkClassRef
    fldREF,                            // tkPointer
    fldREF                             // tkProcedure
  {$endif}
    );
{$endif}


const
  FieldOrdTypeMap: array[TOrdType] of Byte = (
{$ifdef DELPHIXE2}
    fldINT8,                           // otSByte
    fldUINT8,                          // otUByte
{$else}
    fldINT16,                          // otSByte
    fldUINT16,                         // otUByte
{$endif}
    fldINT16,                          // otSWord
    fldUINT16,                         // otUWord
    fldINT32,                          // otSLong
    fldUINT32                          // otULong
    );

const
  FieldFloatTypeMap: array[TFloatType] of Byte = (
    fldSINGLE,                         // ftSingle
    fldFLOAT,                          // ftDouble
    fldEXTENDED,                       // ftExtended
    fldFLOAT,                          // ftComp, Behaves like an INT but is a float
    fldFLOAT                           // ftCurr
    );

const
  FieldSubKindMap: TDBIFieldKindMap = (
{$ifdef fpc}
    fldUNKNOWN,                        // tkUnknown
    fldUNKNOWN,                        // tkInteger
    fldUNKNOWN,                        // tkChar
    fldUNKNOWN,                        // tkEnumeration
    fldUNKNOWN,                        // tkFloat
    fldUNKNOWN,                        // tkSet
    fldUNKNOWN,                        // tkMethod
    fldUNKNOWN,                        // tkSString
    fldUNKNOWN,                        // tkLString
    fldUNKNOWN,                        // tkAString
    fldstUNICODE,                      // tkWString
    fldUNKNOWN,                        // tkVariant
    fldUNKNOWN,                        // tkArray
    fldUNKNOWN,                        // tkRecord
    fldUNKNOWN,                        // tkInterface
    fldUNKNOWN,                        // tkClass
    fldUNKNOWN,                        // tkObject
    fldstUNICODE,                      // tkWChar
    fldUNKNOWN,                        // tkBool
    fldUNKNOWN,                        // tkInt64
    fldUNKNOWN,                        // tkQWord
    fldUNKNOWN,                        // tkDynArray
    fldUNKNOWN,                        // tkInterfaceRaw
    fldUNKNOWN,                        // tkProcVar
    fldUNKNOWN,                        // tkUString
    fldUNKNOWN,                        // tkUChar
    fldUNKNOWN                         // tkHelper
{$else}
    fldUNKNOWN,                        // tkUnknown
    fldUNKNOWN,                        // tkInteger
    fldUNKNOWN,                        // tkChar
    fldUNKNOWN,                        // tkEnumeration
    fldUNKNOWN,                        // tkFloat
    fldUNKNOWN,                        // tkString
    fldUNKNOWN,                        // tkSet
    fldUNKNOWN,                        // tkClass
    fldUNKNOWN,                        // tkMethod
  {$ifdef DELPHI2006}
    fldUNKNOWN,                        // tkWChar
  {$else}
    fldstUNICODE,                      // tkWChar
  {$endif}
    fldUNKNOWN,                        // tkLString
  {$ifdef DELPHI2006}
    fldUNKNOWN,                        // tkWString
  {$else}
    fldstUNICODE,                      // tkWString
  {$endif}
    fldUNKNOWN,                        // tkVariant
    fldUNKNOWN,                        // tkArray
    fldUNKNOWN,                        // tkRecord
    fldUNKNOWN,                        // tkInterface
    fldUNKNOWN,                        // tkInt64
    fldUNKNOWN                         // tkDynArray
  {$ifdef DELPHIXE2} ,
    fldUNKNOWN,                        // tkUString
    fldUNKNOWN,                        // tkClassRef
    fldUNKNOWN,                        // tkPointer
    fldUNKNOWN                         // tkProcedure
  {$endif}
{$endif}
    );


type
  TDBIFieldKindTypeMap = array[TTypeKind] of TFieldType;

const
{$ifdef fpc}
  FieldKindTypeMap: TDBIFieldKindTypeMap = (
    ftUnknown,                         // tkUnknown,
    ftInteger,                         // tkInteger,
    ftString,                          // tkChar,
    ftInteger,                         // tkEnumeration,
    ftFloat,                           // tkFloat,
    ftInteger,                         // tkSet,
    ftUnknown,                         // tkMethod,
    ftString,                          // tkSString,
    ftString,                          // tkLString,
    ftString,                          // tkAString,
    ftWideString,                      // tkWString,
    ftVariant,                         // tkVariant,
    ftArray,                           // tkArray,
    ftADT,                             // tkRecord,
    ftInterface,                       // tkInterface,
    ftDataSet,                         // tkClass,
    ftADT,                             // tkObject,
    ftWideString,                      // tkWChar,
    ftBoolean,                         // tkBool,
    ftLargeint,                        // tkInt64,
    ftLargeint,                        // tkQWord,
    ftReference,                       // tkDynArray,
    ftInterface,                       // tkInterfaceRaw,
    ftReference,                       // tkProcVar,
    ftWideString,                      // tkUString,
    ftWideString,                      // tkUChar,
    ftUnknown                          // tkHelper
    );
{$else}
  FieldKindTypeMap: TDBIFieldKindTypeMap = (
    ftUnknown,                         // tkUnknown
    ftInteger,                         // tkInteger
    ftString,                          // tkChar
    ftInteger,                         // tkEnumeration
    ftFloat,                           // tkFloat
    ftString,                          // tkString
    ftInteger,                         // tkSet
    ftDataSet,                         // tkClass
    ftUnknown,                         // tkMethod
    ftWideString,                      // tkWChar
    ftString,                          // tkLString
    ftWideString,                      // tkWString
    ftVariant,                         // tkVariant
    ftArray,                           // tkArray
    ftADT,                             // tkRecord
    ftInterface,                       // tkInterface
    ftLargeint,                        // tkInt64
    ftArray                            // tkDynArray
  {$ifdef DELPHIXE2} ,
    ftWideString,                      // tkUString
    ftReference,                       // tkClassRef
    ftReference,                       // tkPointer
    ftReference                        // tkProcedure
  {$endif}
    );
{$endif}


const
  FieldUnits1Map: array[fldUNKNOWN..fldUINT8] of Word = (
    0,                                 // fldUNKNOWN
    0,                                 // fldZSTRING
    1,                                 // fldDATE
    1,                                 // fldBLOB
    1,                                 // fldBOOL
    1,                                 // fldINT16
    1,                                 // fldINT32
    1,                                 // fldFLOAT
    1,                                 // fldBCD
    1,                                 // fldBYTES
    1,                                 // fldTIME
    1,                                 // fldTIMESTAMP
    1,                                 // fldUINT16
    1,                                 // fldUINT32
    1,                                 // fldFLOATIEEE
    1,                                 // fldVARBYTES
    1,                                 // fldLOCKINFO
    1,                                 // fldCURSOR
    1,                                 // fldINT64
    1,                                 // fldUINT64
    1,                                 // fldADT
    1,                                 // fldARRAY
    1,                                 // fldREF
    1,                                 // fldTABLE
    1,                                 // fldDATETIME
    1,                                 // fldFMTBCD
    0,                                 // fldWIDESTRING
    0,                                 // fldSINGLE
    0,                                 // fldINT8
    0                                  // fldUINT8
    );


type
  IDBIBase = interface
    function Open: DBIResult;

    function CreateDS(            { Create empty dataset }
        iFields: LongWord;        { Number of fields }
        pFldDes: pDSFLDDesc;      { Array of field descriptors }
        pszName: TDBINameBuffer   { Name (optional) }
    ): DBIResult;

    function AddField(            { Add a field to the dataset }
        pFldDes: pDSFLDDesc       { Field descriptor }
    ): DBIResult;

{$ifdef _UNUSED}
    function AppendData(          { Appends data packet to dataset. }
        Packet: TDBIDataArray;    { Data packet }
        bEof: LongBool            { NOT USED..If True, this is last packet }
    ): DBIResult;

    function GetOptParameter(       { Returns optional parameter (unknown to dataset) }
        iNo      : LongWord;        { Number 1..iOptAttr }
        iFldNo   : LongWord;        { 0 if not field attribute }
    var ppName   : Pointer;         { returns ptr to name }
    var piType   : LongWord;        { returns type }
    var piLen    : LongWord;        { returns length }
    var ppValue  : Pointer          { returns ptr to value }
    ): DBIResult;

    function AddOptParameter(       { Adds optional parameter to dataset }
        iFldNo   : LongWord;        { 0 if not field attribute }
        pszAttr  : TDBIParamBuffer; { ptr to name }
        iType    : LongWord;        { type }
        iLen     : LongWord;        { length }
        pValue   : Pointer          { ptr to value }
    ): DBIResult;
{$endif}

    function GetProps(              { Get dataset properties }
    out Prop : DSProps
    ): DBIResult;

    function GetFieldDescs(         { Get field descriptors }
        Fields  : pDSFLDDesc        { Array of fields descriptors (output) }
    ): DBIResult;

    function GetIndexDescs(         { Get index descriptors }
        p1: PDSIDXDesc              { Array of index descriptors (output) }
    ): DBIResult;

{$ifdef _UNUSED}
    function GetDelta(              { Extract delta from dataset }
    out DsDelta: IDSBase            { Delta in a dataset }
    ): DBIResult;

    function StreamDS(              { Create data packet from the dataset }
    out Packet  : PVarArray         { Return data packet }
    ): DBIResult;

    function AcceptChanges: DBIResult; { Accept all current changes }

    function PutBlank(              { Put blank value }
        pRecBuf      : Pointer;     { RecBuf OR }
        iRecNo       : LongWord;    { iRecNo }
        iFldNo       : LongWord;
        iBlankValue  : LongWord
    ): DBIResult;
{$endif}

    function CreateIndex(           { Create, and add an index }
    const IdxDesc  : DSIDXDesc
    ): DBIResult;

    function RemoveIndex(           { Remove index of given name }
        pszName  : TDBIIndexName
    ): DBIResult;

{$ifdef _UNUSED}
    function GetErrorString(        { Retrieve error string }
        iErrCode  : DBIResult;
        pString   : TDBIMsgBuffer
    ): DBIResult;

    function FldCmp(                { Compare field values returns 0 if equal }
        iFldType  : LongWord;       { Fieldtype }
        pFld1     : Pointer;        { NULL if blank }
        pFld2     : Pointer;        { NULL if blank }
        iUnits1   : LongWord;
        iUnits2   : LongWord
    ): Integer;
{$endif}

    function GetProp(                   { Get property }
        eProp           : TDBIBaseProperty;
        piPropValue     : PDBIPropValue
    ): DBIResult; overload;

    function GetDataProp(                   { Get property }
        eProp       : TDBIBaseProperty;
        pPropData   : PDBIPropData
    ): DBIResult; overload;

    function SetProp(                   { Set property }
        eProp      : TDBIBaseProperty;
        iPropValue : TDBIPropValue
    ): DBIResult; overload;

    function SetDataProp(                   { Set property }
        eProp      : TDBIBaseProperty;
        pPropData  : PDBIPropData
    ): DBIResult; overload;

{$ifdef _UNUSED}
    function SetFieldCalculation(       { Register fieldcalculation on this field }
        iClientData  : TDBIClientData;  { Client data }
        pfCalc       : pfDSCalcField    { Callback function, NULL to remove }
    ): DBIResult;

    function Reconcile(                 { Reconcile update errors }
        DeltaPacket   : PVarArray;      { Delta data packet }
        ErrorPacket   : PVarArray;      { NULL if all changes accepted }
        iClientData   : TDBIClientData;
        pfReconcile   : pfDSReconcile   { Callback-fn (called for each error) }
    ): DBIResult;

    function Refresh(                   { Refresh dataset }
        NewPacket    : PVarArray;       { New updated packet }
        iClientData  : TDBIClientData;  { Client data }
        pfReconcile  : pfDSReconcile    { Callback for resolving conflicts }
    ): DBIResult;
{$endif}

    function Reset: DBIResult;          { Remove all data from dataset }

{$ifdef _UNUSED}
    function RollBack(                  { Rollback changes to this savepoint }
        iSavePoint  : SAVEPOINT
    ): DBIResult;
{$endif}

    function GetEmbeddedDS(
        iFieldID  : LongWord;           { FieldID of embedded table (0 : get the first one) }
    out DsDet     : IDBIBase            { Returns the ds of the embedded table }
    ): DBIResult;

{$ifdef _UNUSED}
    function MakeEmbedded(
        DsDet             : IDSBase;         { Embed this dataset }
        iFieldsLink       : LongWord;
        piFieldsM         : PLongWord;       { Fields in Master }
        piFieldsD         : PLongWord;       { Fields in Detail }
        pMasterFieldName  : TDBIFieldName;   { Name of new link field in master, NULL if using default name }
        pDetailFieldName  : TDBIFieldName    { Name of new link field in detail, NULL if using defaultname }
    ): DBIResult;

    function RefreshRecords(            { Refresh specific records }
        NewDataPacket   : PVarArray;    { Datapacket containing refreshed records }
        iRecNo          : LongWord;     { Refresh this specific record (0 if more than one.Unique key req.) }
        iClientData     : TDBIClientData;
        pfReconcile     : pfDSReconcile { (NULL) Callback for resolving conflicts }
    ): DBIResult;

    function ReleaseBlobs(              { Release all uncommitted blobs }
        iBlobId  : LongWord             { 0: all uncommitted blobs, otherwise : specific blob }
    ): DBIResult;

    function Clone(                     { Clones the structure of the dsbase, including details if any }
         iPType : LongWord;             { 0:normal-ds, 1:delta-ds, 2:error-ds }
         bRecurse : LongBool;           { TRUE:create complete tree-structure }
         bCloneOptParams: LongBool;     { TRUE:clone all optional parameters (normal only) }
         var DataSet: IDSBase           { Returned dsbase }
    ): DBIResult;

    function Reconcile_MD(
        pDsRec          : IDSBase;           { Ds for info }
        pDeltaPacket    : PVarArray;         { Delta pickle }
        pErrorPacket    : PVarArray;         { NULL if all changes accepted }
        iClientData     : TDBIClientData;
        pfReconcile_MD  : pfDSReconcile_MD   { Callback-fn (called for each error) }
    ): DBIResult;

   function DropOptParameter(
           iFldNo: LongWord;            { 0 if not field attribute }
           pName: TDBINameBuffer        { Name of attribute to delete }
        ): DBIResult;
{$endif}

    function EncodeFieldDescs(
        FieldDefs : TFieldDefs
    ): DBIResult;

  end;  { IDBIBase }



  IDBICursor = interface
     function InitCursor(            { Associate Cursor with a DataSet }
       DataSet: IDBIBase
    ): DBIResult;

    function CloneCursor(           { Clone cursor from cursor }
       Cursor: IDBICursor
    ): DBIResult;

    function GetCursorProps(        { Get cursor properties }
    out p1: DSProps
    ): DBIResult;

    function GetIndexDescs(         { Get index descriptors }
        bCurrentOnly  : LongBool;   { Only return 'current' indexdesc, otherwise all }
    out IdxDesc       : DSIDXDesc
    ): DBIResult;

    function GetFieldDescs(         { Get field descriptors }
        p1  : pDSFLDDesc
    ): DBIResult;

    function GetCurrentRecord(      { Return record at current cursorposition }
        pRecBuf  : Pointer
    ): DBIResult;

{$ifdef _UNUSED}
    function GetRecordBlock(        { Return block of records }
        piRecs   : PLongWord;
        pRecBuf  : Pointer
    ): DBIResult;
{$endif}

    function GetCurrentBookMark(    { Get bookmark for current position }
        pBookMark  : Pointer
    ): DBIResult;

    function GetSequenceNumber(     { Get Sequence number of current position }
    out iSeq  : LongWord
    ): DBIResult;

    function GetRecordAttribute(    { Get record attribute of current position }
    out Attr  : DSAttr
    ): DBIResult;

    function GetRecordCount(        { Number of records in active view }
    out iRecs  : Integer
    ): DBIResult;

    function MoveToBOF: DBIResult;  { Set to beginning of table (BOF) }

    function MoveToEOF: DBIResult;  { Set to end of table (EOF) }

    function MoveRelative(i: Integer): DBIResult;

    function MoveToSeqNo(i: LongWord): DBIResult;

    function MoveToRecNo(i: LongWord): DBIResult;

    function MoveToBookMark(        { Goto bookmark }
      pBookMark: Pointer
    ): DBIResult;

{$ifdef _UNUSED}
    function MoveToKey(             { Goto key }
        SearchCond  : DBSearchCond;
        iFields     : LongWord;
        iPartLen    : LongWord;
        pRecBuf     : Pointer
    ): DBIResult;
{$endif}

    function CompareBookMarks(      { Compare two bookmark (positions) -1, 0, 1 }
        pBookMark1  : Pointer;
        pBookMark2  : Pointer;
    out iCmp        : Integer
    ): DBIResult;

{$ifdef _UNUSED}
    function ExtractKey(            { Extract key from record }
        pRecBuf  : Pointer;
        pKeyBuf  : Pointer
    ): DBIResult;
{$endif}

    function GetRecordForKey(       { Return (first) record with given key }
        iFields   : LongWord;
        iPartLen  : LongWord;
        pKey      : Pointer;
        pRecBuf   : Pointer
    ): DBIResult;

    function GetField(              { Extract field value from record buffer }
        pRecBuf   : Pointer;
        iFieldNo  : LongWord;
        pFldBuf   : Pointer;
    out bBlank    : LongBool        { Returns TRUE/FALSE if blank }
    ): DBIResult;

    function PutField(              { Put field value into record buffer }
        pRecBuf   : Pointer;
        iFieldNo  : LongWord;
        pFldBuf   : Pointer         { If NULL, adds a blank value }
    ): DBIResult;


    { Blob functions }

    function GetBlobLen(            { Return length of blob }
        pRecBuf   : Pointer;
        iFieldNo  : LongWord;
    out iLength   : LongWord
    ): DBIResult;

    function GetBlob(               { Return blob }
        pRecBuf   : Pointer;
        iFieldNo  : LongWord;
        iOffSet   : LongWord;       { Starting position }
        pBuf      : Pointer;
    out iLength   : LongWord        { No of bytes to be read/ returns number read }
    ): DBIResult;

    function PutBlob(               { Write blob data }
        pRecBuf   : Pointer;
        iFieldNo  : LongWord;
        iOffSet   : LongWord;       { Starting position }
        pBuf      : Pointer;
        iLength   : LongWord
    ): DBIResult;

    function InitRecord(            { Initialize record buffer (for insertion) }
        pRecBuf  : Pointer
    ): DBIResult;

    function DeleteRecord: DBIResult; { Delete current record }

    function ModifyRecord(          { Modify current record }
        pRecBuf  : Pointer
    ): DBIResult;

    function InsertRecord(          { Insert new record }
        pRecBuf  : Pointer
    ): DBIResult;

    // This is not part of the standard interface,
    // It was added to allow writethrough of fielddata
    function SyncRecord(
        pRecBuf  : Pointer
    ): DBIResult;

{$ifdef _UNUSED}
    function UndoLastChange(        { Undo last update }
        bFollowChange  : LongBool
    ): DBIResult;
{$endif}

    function AddFilter(             { Add a canexpr-filter to this cursor }
        pcanExpr  : Pointer;        { Can expr }
        iLen      : LongWord;       { Length of canexpr }
    var hFilter   : hDSFilter
    ): DBIResult;

    function DropFilter(            { Drop a filter }
       hFilter  : hDSFilter
    ): DBIResult;

    function SetRange(              { Set a range on a cursor }
        iFields    : LongWord;
        pKey1      : Pointer;
        bKey1Incl  : LongBool;
        pKey2      : Pointer;
        bKey2Incl  : LongBool
    ): DBIResult;

    function DropRange: DBIResult;  { Remove current range }

    function SortOnFields(          { Sort on fields }
        iFields    : LongWord;
        piFields   : PPointer;      { NULL -> all fields }
        pDescending: PLongBool;     { NULL -> all ascending }
        pCaseInsensitive: PLongBool { NULL -> all case-sensitive }
    ): DBIResult;

    function UseIndexOrder(         { Switch to index order }
        pszName  : TDBIIndexName
    ): DBIResult;

    function SetNotifyCallBack(           { Called when posting changes to dataset }
        iClientData  : TDBIClientData;
        pfCallBack   : pfCHANGECallBack   { Call back fn being registered }
    ): DBIResult;

    function AddFilterCallBack(     { Add a canexpr-filter to this cursor }
        iClientData  : TDBIClientData; { Client supplied data }
        pfFilter     : pfDSFilter;  { ptr to filter function }
    var hFilter      : hDSFilter
    ): DBIResult;

    function VerifyField(           { Verify if field value is valid }
        iFieldNo  : LongWord;
        pFldBuf   : Pointer
    ): DBIResult;

    function GetProp(               { Get property }
        eProp            : TDBICursorProperty;
        piPropValue      : PDBIPropValue
    ): DBIResult; overload;

    function GetDataProp(               { Get property }
        eProp            : TDBICursorProperty;
        pPropData        : PDBIPropData
    ): DBIResult; overload;

    function RevertRecord: DBIResult; { Restore current record }

{$ifdef _UNUSED}
    function LocateWithFilter(
        pCanExpr  : Pointer;        { NULL -> use previous }
        iLen      : LongWord        { Length of canexpr }
    ): DBIResult;
{$endif}

    function AddAggregate(
        iFlds     : LongWord;       { Defines grouping  (0 if global) }
        iCanLen   : LongWord;       { Length of canexpr (0 if grouping only) }
        pCanExpr  : Pointer;        { Canexpression for aggregate }
    var hAgg      : hDSAggregate    { returns aggregate handle }
    ): DBIResult;

    function DropAggregate(
        hAgg  : hDSAggregate
    ): DBIResult;

    function GetAggregateValue(
        hAgg     : hDSAggregate;
        pValue   : Pointer;
    var bBlank   : LongBool
    ): DBIResult;

    function GetAggregateDesc(
        hAgg   : hDSAggregate;
    var Desc   : DSFLDDesc
    ): DBIResult;

{$ifdef _UNUSED}
    function MoveToNextSubGroup(
        iFields  : LongWord
    ): DBIResult;
{$endif}

    function GetSubGroupState(
        iFields        : LongWord;
    var iSubGroupState : GROUPSTATE
    ): DBIResult;

    function LinkCursors(
        iFieldsM   : LongWord;
        piFieldsM  : PLongWord;     { Fields in Master }
        piFieldsD  : PLongWord;     { Fields in Detail }
        hCurDet    : IDBICursor     { Detail cursor to link }
    ): DBIResult;

{$ifdef _UNUSED}
    function ResyncWithMaster: DBIResult; { If this is a detail, reset range }
{$endif}

    function SetProp(               { Set property }
        eProp       : TDBICursorProperty;     { Property to set }
        iPropValue  : TDBIPropValue { Property value (or pointer to value) }
    ): DBIResult; overload;

    function SetDataProp(               { Set property }
        eProp       : TDBICursorProperty;     { Property to set }
        pPropData   : PDBIPropData  { Property value (or pointer to value) }
    ): DBIResult; overload;

   function GetRecordNumber(  { Return record number of current record, if any }
        out iRecNo: LongWord
    ): DBIResult;

{$ifdef _UNUSED}
    function GetRowRequestPacket(   { Get packet describing the curent 'path',
                                       for delayed fetching/refreshing }
        bKeysOnly       : LongBool; { Only include unique keys in packet }
        bFetchAllBlobs  : LongBool; { fetch all blobs for 'current'record }
        bFetchAllDetails: LongBool; { fetch all details for 'current' record }
        bInclMetaData   : LongBool; { Include metadata in packet }
        out Packet      : PVarArray { returns datapacket with row description }
    ): DBIResult;

   function RefreshRecord(          { Refresh details/blobs for this record, and all
                                      'current' records above, if any }
        Packet  : PVarArray         { New updated pickle }
    ): DBIResult;
{$endif}

    function GetDebugInfo(
      var Data: String
    ): DBIResult;
  end;  { IDBICursor }


type
  IDBIAppServer = interface
    function  AS_ApplyUpdates(
      const ProviderName: WideString;
      Delta: OleVariant;
      MaxErrors: Integer;
      out ErrorCount: Integer;
      var OwnerData: OleVariant
      ): OleVariant; //##JVR safecall;

    function  AS_GetRecords(
      const ProviderName: WideString;
      Count: Integer;
      out RecsOut: Integer;
      Options: Integer;
      const CommandText: WideString;
      var Params: OleVariant;
      var OwnerData: OleVariant
      ): OleVariant; //##JVR safecall;

    function  AS_DataRequest(
      const ProviderName: WideString;
      Data: OleVariant
      ): OleVariant; //##JVR safecall;

    function  AS_GetProviderNames: OleVariant; //##JVR safecall;
    function  AS_GetParams(
      const ProviderName: WideString;
      var OwnerData: OleVariant
      ): OleVariant; //##JVR safecall;
    function  AS_RowRequest(
      const ProviderName: WideString;
      Row: OleVariant;
      RequestType: Integer;
      var OwnerData: OleVariant
      ): OleVariant; //##JVR safecall;

    procedure AS_Execute(
      const ProviderName: WideString;
      const CommandText: WideString;
      var Params: OleVariant;
      var OwnerData: OleVariant
      ); //##JVR safecall;
  end;


type
  TDBICustomObject = class(TPersistent)
  protected
    procedure Error(
      E: Exception;
      const Caller: String;
      const Reference: String;
      const ErrMsg: String;
      Args: array of const
      );
  end;


type
  TDBIConnectionStatus = (csNoChange, csDeactivate);
  TDBIDataConnectionMode = (cmData, cmFields);
  TDBIDataChangeEventType = (
    dbiRecordDeleted,
    dbiRecordModified,
    dbiRecordInserted,
    dbiBasePropChanged
    );
  TDBIDataChangeEvent = procedure(
    iClientData: TDBIClientData;
    DataChangeEventType: TDBIDataChangeEventType;
    pRecBuf: Pointer
    ) of object;

type
  TDBIRecordLoadMode = (
    rlmLoadAllRecords,
    rlmLoadCurrentRecord
    );


type
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
    |  <DBInterface.pas>  |
    +---------------------+

    </CODE>
  }
  TDBIGetCurrentRecordProc = procedure(
    const Position: Integer;
    out Buffer;
    DataInfo: PDataInfo
    ) of object;

  TDBIDataConnection = class(TDBICustomObject)
  private
    FOwner: TObject;
    FActive: Boolean;
    FExclusive: Boolean;
    FFileName: TFileName;
    FModified: Boolean;
    FReadOnly: Boolean;
    FCanModify: Boolean;
    FStreamMode: TDBIStreamMode;

    // Logical Dataset metadata
    FLogicalBufferSize: Integer;

    // DataEventCallBack list
    FDataEventCallBackList: TObjectList;

    FFieldProps: TFieldDescList;
    FIndices: TDBIIndexList;

    FMode: TDBIDataConnectionMode;
    FGetCurrentRecord: TDBIGetCurrentRecordProc;

    // Null field support
    FNullFlags: TDBINullFlags;

  protected
    procedure AddDataEventCallBack(DataEventCallBack: TDBIDataChangeEvent);
    function FindDataEventCallBack(DataEventCallBack: TDBIDataChangeEvent): Integer;
    procedure RemoveDataEventCallBack(DataEventCallBack: TDBIDataChangeEvent);
    procedure NotifyDataEventCallBacks(
      iClientData: TDBIClientData;
      DataEventType: TDBIDataChangeEventType;
      pRecBuf: Pointer
      );

    // Field data methods to access fieldprops as a dataset
    procedure GetFieldRecord(
      const Position: Integer;
      out Buffer;
      DataInfo: PDataInfo
      );

    property Owner: TObject read FOwner;
    property GetCurrentRecord: TDBIGetCurrentRecordProc
      read FGetCurrentRecord
      write FGetCurrentRecord;


  protected
    procedure AddField(pFldDes: pDSFLDDesc);
    procedure EncodeFieldDescs(FieldDefs: TFieldDefs);

    function GetActive: Boolean; virtual;
    procedure GetDatasetProps(var Prop: DSProps); virtual;
    function GetFieldCount: Integer; virtual;
    function GetFileName: TFileName; virtual;
    procedure GetFieldProps(Fields: pDSFLDDesc); virtual;
    function GetFieldSize(pFldDes: pDSFLDDesc): Integer;
    function GetFlatViewFieldCount: Integer;
    function GetLogicalBufferSize: Integer; virtual;
    function GetRecordCount(const StatusFilter: DSAttr): Integer; virtual; abstract;

    procedure GetMetaData; virtual; abstract;
    // It seems logical to include WriteMetadata here although it isn't
    // used in all descendants (e.g. List dataconnections

    procedure SetActive(const Value: Boolean); virtual;
    procedure SetExclusive(Index: TDBIConnectionStatus; const Value: Boolean); virtual;
    procedure SetFieldCount(const Value: Integer); virtual;
    procedure SetFieldProps(const Fields: TFieldDescList); virtual;
    procedure SetFileName(const Value: TFileName); virtual;
    procedure SetLogicalBufferSize(const Value: Integer); virtual;
    procedure SetMode(const Value: TDBIDataConnectionMode);
    procedure SetReadOnly(Index: TDBIConnectionStatus; const Value: Boolean); virtual;
    procedure SetStreamMode(AStreamMode: TDBIStreamMode; IsValid: Boolean);
{$ifdef _UNUSED}
    procedure SetRecordCount(const Value: Integer); virtual; abstract;
    property RecordCount: Integer index DSAttr(dsRecAll) read GetCount;
{$endif}
    property CanModify: Boolean read FCanModify write FCanModify;
    property FileName: TFileName read GetFileName write SetFileName;
    property Indices: TDBIIndexList read FIndices;
    property NullFlags: TDBINullFlags read FNullFlags;
    property StreamMode: TDBIStreamMode read FStreamMode;

  public
    constructor Create(AOwner: TObject); virtual;
    constructor CreateEmbedded(
      AOwner: TDBIDataConnection;
      const IFieldID: Integer
      );  virtual;

    destructor Destroy; override;
    // Dataset based operations
    procedure CreateDS(iFields: LongWord; pFldDes: pDSFLDDesc; pszName: TDBINameBuffer); virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    procedure Release; virtual;
    procedure Reset; virtual;
    procedure SyncRecordBuffer(var Buffer; const Initialise: Boolean); virtual;
    procedure ValidateMetaData; virtual;
    procedure ValidateField(
      const Position: Integer;
      const FieldNo: Word;
      pFldBuf: Pointer
      ); virtual;

    // Record based operations
    procedure New; virtual; abstract;
    function Append(const Buffer): Integer; virtual; abstract;
    procedure Cancel; virtual; abstract;
    procedure Delete(const Position: Integer); virtual; abstract;
    procedure Get(
      const Position: Integer;
      out Buffer;
      DataInfo: PDataInfo
      ); virtual; abstract;
    function GetCount(const StatusFilter: Integer = dsRecActive): Integer;
{$ifdef _UNUSED}
    procedure Sync(const Position: Integer; const Buffer); virtual; abstract;
{$endif}
    procedure Update(const Position: Integer; const Buffer); virtual; abstract;

    // Load methods
    procedure LoadFromDataset(
      ADataset: TDataset;
      RecordLoadMode: TDBIRecordLoadMode = rlmLoadAllRecords
      ); virtual;
    procedure LoadFromFile(
      AFileName: String;
      const Format: TDBIDataFormat
      ); virtual;
    procedure LoadFromStream(
      AStream: TStream;
      const Format: TDBIDataFormat
      ); virtual;
    procedure SaveToFile(
      AFileName: String;
      const Format: TDBIDataFormat
      ); virtual;
    procedure SaveToStream(
      AStream: TStream;
      const Format: TDBIDataFormat
      ); virtual;

    // Locking methods
    function Lock: Boolean; virtual; abstract;
    function Unlock: Boolean; virtual; abstract;
    function LockRecord(const Position: Integer): Boolean; virtual; abstract;
    function UnlockRecord(const Position: Integer): Boolean; virtual; abstract;


    // Blob methods
    procedure GetBlob(
      const Position: LongWord;
      const FieldNo: LongWord;
      const OffSet: LongWord;
      PData: Pointer;
      out Size: LongWord
      ); virtual; abstract;

    function PutBlob(
      const Position: LongWord;
      const FieldNo: LongWord;
      const OffSet: LongWord;
      PData: Pointer;
      const Size: LongWord
      ): Integer; virtual; abstract;
{$ifdef _UNUSED}
    function GetBlobLen(
      const Position: LongWord;
      const FieldNo: LongWord
      ): Integer; virtual; abstract;
{$endif}

    // General Properties
    property Active: Boolean read GetActive write SetActive;
    property Exclusive: Boolean index csDeActivate read FExclusive write SetExclusive;
    property FieldCount: Integer read GetFieldCount write SetFieldCount;
    property FieldProps: TFieldDescList read FFieldProps write SetFieldProps;
    property LogicalBufferSize: Integer read GetLogicalBufferSize write SetLogicalBufferSize;
    property Mode: TDBIDataConnectionMode read FMode write SetMode;
    property Modified: Boolean read FModified write FModified;
    property Readonly: Boolean index csDeActivate read FReadOnly write SetReadOnly;

  end;  { TDBIDataConnection }

  TDBIDataConnectionClass = class of TDBIDataConnection;



  TDBILock = class(TObject)
  private
    FDataConnection: TDBIDataConnection;
    FLockData: TDBILockData;

  protected
    function Lock: DBIResult; virtual; abstract;
    function Unlock: DBIResult; virtual; abstract;

  public
    constructor Create(
      ADataConnection: TDBIDataConnection;
      ALockData: TDBILockData
      ); virtual;

  end;  { TDBILock }


  TDBIRecordLock = class(TDBILock)
  protected
    function Lock: DBIResult; override;
    function Unlock: DBIResult; override;

  end;  { TDBIRecordLock }


  TDBILockList = class(TDBICustomObject)
  private
    FLocks: TList;
    FDataConnection: TDBIDataConnection;
    FFullLocked: Boolean;

  protected
    function GetCount: Integer;
    function Find(ALockData: TDBILockData): TDBILock;
    function SetFullLock(const Value: Boolean): DBIResult;

  public
    constructor Create(ADataConnection: TDBIDataConnection);
    destructor Destroy; override;

    function ClearAll: Boolean;

    function Add(ALockData: TDBILockData): DBIResult;
    function Locked(ALockData: TDBILockData): DBIResult;
    function Remove(ALockData: TDBILockData): DBIResult;

    property Count: Integer read GetCount;
    property FullLocked: Boolean read FFullLocked;
  end;  { TDBILockList }



  // ___________________________________________________________________________
  {**
    <CODE>

          +---------+
          | TObject |
          +---------+
               |
               |
     +-------------------+     +-----------------+
     |     TDBIBase      |<>---|  TDBIndexList   |
     |  <DBIXbase.pas>   |     | <DBIndices.pas> |
     +-------------------+     +-----------------+

    </CODE>
  }
  TDBIBase = class(TInterfacedObject, IDBIBase)
  private
    FDataConnection: TDBIDataConnection;// Object that takes care of the raw data access
    FEmbeddedDataConnections: TObjectList;
    FDSProps: DSProps;                  // Dataset properties
{$ifdef _UNUSED}
    FDSFldProps: TFieldDescList;        // Array of Field description properties
{$endif}
    FIndices: TDBIIndexList;            // List of Indices and description properties
    FLocks: TDBILockList;               // List of Locks

  protected
    procedure Error(
      E: Exception;
      const Caller: String;
      const Reference: String;
      const ErrMsg: String;
      Args: array of const
      );

    function CreateDefaultIndex: Integer;

    // Callback routine to deal with actions initiated by the DataConnection
    // (e.g.) Add, Delete, Extract, Update in the TObjectlist
    // Maintains Indices
    procedure DataEventNotify(
      iClientData: TDBIClientData;
      DataEventType: TDBIDataChangeEventType;
      pRecBuf: Pointer
      );

  public
    constructor Create(ADataConnection: TDBIDataConnection); virtual;
    destructor Destroy; override;
    procedure Close;

  protected
    function Open: DBIResult;

    function CreateDS(iFields: LongWord; pFldDes: pDSFLDDesc; pszName: TDBINameBuffer): DBIResult;
    function AddField(pFldDes: pDSFLDDesc): DBIResult;
{$ifdef _UNUSED}
    function AppendData(Packet: TDBIDataArray; bEof: LongBool): DBIResult;
{$endif}
    function GetProps(out Prop: DSProps): DBIResult;
    function GetFieldDescs(Fields: pDSFLDDesc): DBIResult;
    function SetFieldDescs(Fields: pDSFLDDesc): DBIResult;
    function GetIndexDescs(p1: PDSIDXDesc): DBIResult;
    function CreateIndex(const IdxDesc: DSIDXDesc): DBIResult;
    function RemoveIndex(pszName: TDBIIndexName): DBIResult;
    function GetProp(eProp: TDBIBaseProperty; piPropValue: PDBIPropValue): DBIResult;
    function GetDataProp(eProp: TDBIBaseProperty; pPropData: PDBIPropData): DBIResult;
    function SetProp(eProp: TDBIBaseProperty; iPropValue: TDBIPropValue): DBIResult;
    function SetDataProp(eProp: TDBIBaseProperty; pPropData: PDBIPropData): DBIResult;
    function Reset: DBIResult;
    function GetEmbeddedDS(iFieldID: LongWord; out DsDet: IDBIBase): DBIResult;
    function EncodeFieldDescs(FieldDefs: TFieldDefs): DBIResult;

  end;  { TDBIBase }


  // ___________________________________________________________________________
  {**
    <CODE>

         +---------+
         | TObject |
         +---------+
              |
              |               +-------------------+
    +-------------------+     |    IDBIBase       |
    |    TDBICursor     |<>---| <DBInterface.pas> |
    | <DBInterface.pas> |     +-------------------+
    |                   |
    |                   |     +-------------------------+
    |                   |<>---|   TDBIDataConnection    |
    +-------------------+     | <DBIDataConnections.pas>|
                              +-------------------------+

    </CODE>
  }
  TDBICursor = class(TInterfacedObject, IDBICursor)
  private
    FDSBase: IDBIBase;                  // Dataset associated with cursor
    FDataConnection: TDBIDataConnection;// Pointer associated with the Data
    FDSLocateProps: DSIDXDesc;          // Locate properties, not exposed
    FPosition: Integer;                 // Current Cursor position in dataset
    FSelectedIndex: Integer;            // Selected Index for this DS Cursor
    FPriorIndex: Integer;               // Index No. Prior to the Selected one
    FIndices: TDBIIndexList;            // Pointer to the DSBase.FIndices field
    FRecInfoOffset: Word;               // Offset to extra rec info in recbuf
    FLocks: TDBILockList;               // List of Locks
    FFilterLock: Boolean;               // Filterlock active status

    FCursorProps: DSProps;              // CursorProps & FieldProps, initialised
    FFieldProps: TFieldDescList;        // in InitCursor & CloneCursor

    // Helper Methods
    function CreateIndex(Filter: TDBIDatasetFilter): DBIResult;
    function SelectIndex(const IndexNo: Integer): DBIResult;

    function InternalSetLock(const Position: Integer; const Lock: Boolean): DBIResult;
    function SetFilterLock(LockData: TDBILockData): DBIResult;

    procedure Error(
      E: Exception;
      const Caller: String;
      const Reference: String;
      const ErrMsg: String;
      Args: array of const
      );

    function CheckPosition(const Direction: Integer): DBIResult;
    function GetActiveIndex: TDBIndex;
    procedure SetPosition(const Value: Integer);

  protected
    // Implementation of the interface
    function InitCursor(DataSet: IDBIBase): DBIResult; virtual;
    function CloneCursor(Cursor: IDBICursor): DBIResult; virtual;
    function GetCursorProps(out p1: DSProps): DBIResult; virtual;
{$ifdef _UNUSED}
    function GetDataConnection: TDBIDataConnection;
{$endif}
    function GetIndexDescs(bCurrentOnly: LongBool;
      out IdxDesc: DSIDXDesc): DBIResult; virtual;
    function GetFieldDescs(p1: pDSFLDDesc): DBIResult; virtual;
    function GetCurrentRecord(pRecBuf: Pointer): DBIResult; virtual;
    function GetCurrentBookMark(pBookMark: Pointer): DBIResult; virtual;
    function GetSequenceNumber(out iSeq: LongWord): DBIResult; virtual;
    function GetRecordAttribute(out Attr: DSAttr): DBIResult; virtual;
    function GetRecordCount(out iRecs: Integer): DBIResult; virtual;
    function MoveToBOF: DBIResult; virtual;
    function MoveToEOF: DBIResult; virtual;
    function MoveRelative(i: Integer): DBIResult; virtual;
    function MoveToSeqNo(i: LongWord): DBIResult; virtual;
    function MoveToRecNo(i: LongWord): DBIResult; virtual;
    function MoveToBookMark(pBookMark: Pointer): DBIResult; virtual;
{$ifdef _UNUSED}
    function MoveToKey(SearchCond: DBISearchCond; iFields: LongWord;
      iPartLen: LongWord; pRecBuf: Pointer): DBIResult; override;
{$endif}
    function CompareBookMarks(
      pBookMark1: Pointer;
      pBookMark2: Pointer;
      out iCmp: Integer
      ): DBIResult; virtual;

    function GetRecordForKey(
      iFields: LongWord;
      iPartLen: LongWord;
      pKey: Pointer;
      pRecBuf: Pointer
      ): DBIResult; virtual;

    function GetField(
      pRecBuf: Pointer;
      iFieldNo: LongWord;
      pFldBuf: Pointer;
      out bBlank: LongBool
      ): DBIResult; virtual;

    function PutField(
      pRecBuf: Pointer;
      iFieldNo: LongWord;
      pFldBuf: Pointer
      ): DBIResult; virtual;

    function VerifyField(
      iFieldNo: LongWord;
      pFldBuf: Pointer
      ): DBIResult; virtual;

    { Blob functions }
    function GetBlobLen(
      pRecBuf: Pointer;
      iFieldNo: LongWord;
      out iLength: LongWord
      ): DBIResult; virtual;

    function GetBlob(
      pRecBuf: Pointer;
      iFieldNo: LongWord;
      iOffSet: LongWord;
      pBuf: Pointer;
      out iLength: LongWord
      ): DBIResult; virtual;

    function PutBlob(
      pRecBuf: Pointer;
      iFieldNo: LongWord;
      iOffSet: LongWord;
      pBuf: Pointer;
      iLength: LongWord
      ): DBIResult; virtual;

    function InitRecord(pRecBuf: Pointer): DBIResult; virtual;
    function DeleteRecord: DBIResult; virtual;
    function ModifyRecord(pRecBuf: Pointer): DBIResult; virtual;
    function SyncRecord(pRecBuf: Pointer): DBIResult; virtual;
    function InsertRecord(pRecBuf: Pointer): DBIResult; virtual;
    function RevertRecord: DBIResult; virtual;

    function AddFilter(
      pcanExpr: Pointer;
      iLen: LongWord;
      var hFilter: hDSFilter
      ): DBIResult; virtual;

    function DropFilter(hFilter: hDSFilter): DBIResult; virtual;

    function SetRange(
      iFields: LongWord;
      pKey1: Pointer;
      bKey1Incl: LongBool;
      pKey2: Pointer;
      bKey2Incl: LongBool
      ): DBIResult; virtual;
    function DropRange: DBIResult; virtual;

    function SortOnFields(
      iFields: LongWord;
      piFields: PPointer;
      pDescending: PLongBool;
      pCaseInsensitive: PLongBool
      ): DBIResult; virtual;

    function UseIndexOrder(pszName: TDBIIndexName): DBIResult; virtual;

    function SetNotifyCallBack(
      iClientData: TDBIClientData;
      pfCallBack: pfCHANGECallBack
      ): DBIResult;

    function AddFilterCallBack(
      iClientData: TDBIClientData;
      pfFilter: pfDSFilter;
      var hFilter: hDSFilter
      ): DBIResult;

    function AddAggregate(
        iFlds     : LongWord;          { Defines grouping  (0 if global) }
        iCanLen   : LongWord;          { Length of canexpr (0 if grouping only) }
        pCanExpr  : Pointer;           { Canexpression for aggregate }
    var hAgg      : hDSAggregate       { returns aggregate handle }
    ): DBIResult;

    function DropAggregate(
        hAgg  : hDSAggregate
    ): DBIResult;

    function GetAggregateValue(
        hAgg     : hDSAggregate;
        pValue   : Pointer;
    var bBlank   : LongBool
    ): DBIResult;

    function GetAggregateDesc(
        hAgg   : hDSAggregate;
    var Desc   : DSFLDDesc
    ): DBIResult;

    function GetDebugInfo(
      var Data: String
    ): DBIResult;

    function GetSubGroupState(
      iFields: LongWord;
      var iSubGroupState: GROUPSTATE
    ): DBIResult;

    function LinkCursors(
      iFieldsM: LongWord;
      piFieldsM: PLongWord;         { Fields in Master }
      piFieldsD: PLongWord;         { Fields in Detail }
      hCurDet: IDBICursor           { Detail cursor to link }
    ): DBIResult;

    function GetProp(eProp: TDBICursorProperty; piPropValue: PDBIPropValue): DBIResult; virtual;
    function GetDataProp(eProp: TDBICursorProperty; pPropData: PDBIPropData): DBIResult; virtual;
    function SetProp(eProp: TDBICursorProperty; iPropValue: TDBIPropValue): DBIResult; virtual;
    function SetDataProp(eProp: TDBICursorProperty; pPropData: PDBIPropData): DBIResult; virtual;
    function GetRecordNumber(out iRecNo: LongWord): DBIResult; virtual;

    property DataConnection: TDBIDataConnection read FDataConnection; //##CONNECTION GetDataConnection;
    
  public
    constructor Create;

    property Position: Integer read FPosition write SetPosition;
    property ActiveIndex: TDBIndex read GetActiveIndex;

  end;  { TDBICursor }


implementation

{ TODO 1 -oJvr -cGeneral :
  It is probably worth considering making Position 0-based in TDBICursor
  to match Position in TDBIDataConnection.  This means Indices need
  0-based values stored as well, instead of using record numbers.
}

uses
  SysConst,
{$ifdef DELPHI6}
  FmtBcd,
  Types,
{$endif}
  Dialogs,
  DBIFileStreams;

const
  // CheckPosition constants
  DirectionForward = 1;
  DirectionCurrent = 0;
{$ifdef _UNUSED}
  DirectionReverse = -1;
{$endif}

type
  TDataEventCallBackObject = class(TObject)
  private
    FDataChangeMethod: TDBIDataChangeEvent;

    property DataChangeMethod: TDBIDataChangeEvent
      read FDataChangeMethod
      write FDataChangeMethod;
  end;  { TDataEventCallBackObject }



{ TDBIDataConnection }

// _____________________________________________________________________________
{**
  Jvr - 23/11/2000 15:00:40<P>
}
procedure TDBIDataConnection.AddDataEventCallBack(
  DataEventCallBack: TDBIDataChangeEvent);
var
  DataEventCallBackObject: TDataEventCallBackObject;

begin
  if (FDataEventCallBackList = nil) then Exit;

  if (FindDataEventCallBack(DataEventCallBack) < 0) then begin
    DataEventCallBackObject := TDataEventCallBackObject.Create;
    DataEventCallBackObject.DataChangeMethod := DataEventCallBack;
    FDataEventCallBackList.Add(DataEventCallBackObject);
  end;
end;  { AddDataEventCallBack }


// _____________________________________________________________________________
{**
  Jvr - 14/12/2000 15:54:04<P>
}
procedure TDBIDataConnection.AddField(pFldDes: pDSFLDDesc);
var
  FieldNo: Integer;

begin
  try
    FieldNo := FieldCount;
    FieldCount := FieldCount + 1;

    // Assign the field description
    FFieldProps[FieldNo] := pFldDes^;
    with FFieldProps[FieldNo] do begin
      iFieldID := FieldNo + 1;
      iFldLen := GetFieldSize(pFldDes); //##JVR iFldType, iFldSubType, iUnits1, iUnits2);

      if not ((iFldType = fldUNICODE) or (iFldType = fldWIDESTRING)) then begin
        iUnits1 := FieldUnits1Map[iFldType];
      end;
    end;  { with }

    // Calculate Field Offsets
    FLogicalBufferSize := Default_LogicalBufferFirstFieldOffset;
    for FieldNo := 0 to FieldCount - 1 do begin
      with FFieldProps[FieldNo] do begin
        iFldOffsInRec := FLogicalBufferSize;
        Inc(FLogicalBufferSize, iFldLen);
      end;
    end;  { for }

    // Calculate Field Null-Flag Offsets
    for FieldNo := 0 to FieldCount - 1 do begin
      FFieldProps[FieldNo].iNullOffsInRec := FLogicalBufferSize + FieldNo;
    end;  { for }

    // Allow for FieldCount * Null-Flags at the end of the Buffer
    Inc(FLogicalBufferSize, FieldCount);

  except
    Error(nil, 'AddField', '1020', 'Failed to create new field "%s"',
      [TDBIFieldName(pFldDes^.szName)]
      );
  end;  { try..except }
end;  { AddField }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 15:18:34<P>
}
procedure TDBIDataConnection.Close;
begin
  Active := False;
end;  { Close }


// _____________________________________________________________________________
{**
  Jvr - 23/11/2000 15:01:01 - Initial code<P>
  Jvr - 29/01/2003 12:30:17 - Moved Indices from TDBIBase to the DataConnection<P>
}
constructor TDBIDataConnection.Create(AOwner: TObject);
begin
  inherited Create;

  FOwner := AOwner;
  FActive := False;
  FExclusive := False;
  FModified := False;
  FReadOnly := False;
  FCanModify := False;
  FStreamMode := smMemoryStream;

  FMode := cmData;  { cmData, cmFields }
  FGetCurrentRecord := Get;

  FIndices := TDBIIndexList.Create;
  FNullFlags := TDBINullFlags.Create;
  FDataEventCallBackList := TObjectList.Create;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 21/11/2000 14:58:47<P>
}
procedure TDBIDataConnection.CreateDS(
  iFields: LongWord;
  pFldDes: pDSFLDDesc;
  pszName: TDBINameBuffer
  );
begin
  // Clear the Fieldprops first
  FieldCount := 0;

  // Set the logical field properties
  SetFieldProps(TFieldDescList(pFldDes));
end;  { CreateDS }


// _____________________________________________________________________________
{**
  Jvr - 23/03/2005 10:53:19 - Initial code.<br>
}
constructor TDBIDataConnection.CreateEmbedded(
  AOwner: TDBIDataConnection;
  const iFieldID: Integer
  );
begin
  Create(AOwner);
end;  { CreateEmbedded }


// _____________________________________________________________________________
{**
  Jvr - 23/11/2000 15:02:15 - Initial code<P>
  Jvr - 29/01/2003 12:29:44 - Moved Indices from TDBIBase to the DataConnection<P>
}
destructor TDBIDataConnection.Destroy;
begin
  Close;
  Release;

  FIndices.Free;
  FIndices := nil;

  FNullFlags.Free;
  FNullFlags := nil;

  FDataEventCallBackList.Free;
  FDataEventCallBackList := nil;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 21/12/2000 14:17:16<P>
}
procedure TDBIDataConnection.EncodeFieldDescs(FieldDefs: TFieldDefs);
var
  DescNo: Integer;
  FieldDefCount: Integer;

  procedure GetFieldDefCount(FieldDefs: TFieldDefs; var Count: Integer);
  var
    Index: Integer;

  begin
    Inc(Count, FieldDefs.Count);

    for Index := 0 to FieldDefs.Count - 1 do begin
      with TDBIFieldDef(FieldDefs[Index]) do begin
        if HasChildDefs then begin
          GetFieldDefCount(ChildDefs, Count);
        end;
      end;  { with }
    end;  { for }
  end;  { GetFieldDefCount }


  procedure EncodeFieldDesc(
    var FieldDesc: DSFLDDesc;
    const Name: TDBIString;
    DataType: TFieldType;
    Size: Integer;
    Precision: Integer;
    Calculated: Boolean;
    Attributes: TFieldAttributes
    );
  begin
    FillChar(FieldDesc, SizeOf(FieldDesc), #0);
    StrLCopy(FieldDesc.szName, TDBINameBuffer(TDBIString(Name)), SizeOf(FieldDesc.szName)-1);

    FieldDesc.iFldType := FieldTypeMap[DataType];
    FieldDesc.iUnits1 := FieldUnits1Map[FieldDesc.iFldType];
    if FieldDesc.iFldType = fldWIDESTRING  then
      FieldDesc.iFldType := fldUNICODE;

    FieldDesc.iFldSubType := FldSubTypeMap[DataType];
    FieldDesc.bCalculated := Calculated;
    FieldDesc.iFldAttr := PByte(@Attributes)^;

    case DataType of
      ftADT, ftArray, ftDataSet, ftString, ftFixedChar, ftGUID, ftBytes,
      ftVarBytes, ftBlob..ftTypedBinary, ftOraClob, ftOraBlob:
        FieldDesc.iUnits1 := Size;

      ftWideString {$ifdef DELPHI2006}, ftFixedWideChar, ftWideMemo {$endif} :
        FieldDesc.iUnits1 := Size * 2;

      ftBCD {$ifdef DELPHI6}, ftFMTBcd {$endif} :
        begin
          { Default precision is 32, Size = Scale }
          if (Precision > 0) and (Precision <= 32) then
            FieldDesc.iUnits1 := Precision
          else
            FieldDesc.iUnits1 := 32;

          FieldDesc.iUnits2 := Size;  {Scale}
        end;

    ftFloat:
      begin
        FieldDesc.iUnits1 := Size;
        FieldDesc.iUnits2 := Precision;
      end;

    end;
  end;  { EncodeFieldDesc }


  procedure EncodeFieldDescs(
    FieldDefs: TFieldDefs;
    FieldDescs: TFieldDescList;
    var DescNo: Integer
    );
  var
    FieldNo: Integer;

  begin
    // Loop through the field definitions and create a corresponding field description
    for FieldNo := 0 to FieldDefs.Count - 1 do begin
      with TDBIFieldDef(FieldDefs[FieldNo]) do begin
        EncodeFieldDesc(FieldDescs[DescNo], TDBIString(Name), DataType, Size, Precision, False, Attributes);
        Inc(DescNo);
        // If we have child definitions then process them
        if HasChildDefs then begin
          if (DataType = ftDataSet) then begin
            GetFieldDefCount(ChildDefs, FieldDescs[DescNo-1].iUnits2);
          end;
          EncodeFieldDescs(ChildDefs, FieldDescs, DescNo);
        end;
      end;  { with }
    end;  { for }
  end;  { EncodeFieldDescs }

begin
  FieldDefCount := 0;
  GetFieldDefCount(FieldDefs, FieldDefCount);
  if (FieldDefCount = 0) then begin
    Exit;
  end;

  SetLength(FFieldProps, FieldDefCount);
  DescNo := 0;
  EncodeFieldDescs(FieldDefs, FFieldProps, DescNo);

  // Update offsets & logicalbuffersize
  SetFieldProps(FFieldProps);
end;  { EncodeFieldDescs }


// _____________________________________________________________________________
{**
  Jvr - 23/11/2000 16:02:49<P>
}
function TDBIDataConnection.FindDataEventCallBack(
  DataEventCallBack: TDBIDataChangeEvent): Integer;
var
  DataEventCallBackObject: TDataEventCallBackObject;
  Index: Integer;

begin
  Result := -1;
  if (FDataEventCallBackList = nil) then Exit;

  for Index := 0 to FDataEventCallBackList.Count - 1 do begin
    DataEventCallBackObject := FDataEventCallBackList[Index]as TDataEventCallBackObject;
    if (TMethod(DataEventCallBackObject.DataChangeMethod).Code = TMethod(DataEventCallBack).Code) and
       (TMethod(DataEventCallBackObject.DataChangeMethod).Data = TMethod(DataEventCallBack).Data) then begin
      Result := Index;
      Break;
    end;
  end;  { for }
end;  { FindDataEventCallBack }


// _____________________________________________________________________________
{**
  Jvr - 02/11/2000 15:10:22<P>
}
function TDBIDataConnection.GetActive: Boolean;
begin
  Result := FActive;
end;  { GetActive }


// _____________________________________________________________________________
{**
  Jvr - 05/12/2000 11:19:56<P>
}
function TDBIDataConnection.GetCount(
  const StatusFilter: Integer = dsRecActive
  ): Integer;
begin
  if (FMode = cmFields) then begin
    Result := Length(FFieldProps);
  end
  else begin
    Result := GetRecordCount(StatusFilter);
  end;
end;  { GetCount }


// _____________________________________________________________________________
{**
  Jvr - 13/07/2004 13:56:44 - Updated code - elevated GetMetaData from derived<p>
}
procedure TDBIDataConnection.GetDatasetProps(var Prop: DSProps);
begin
  if (FieldCount = 0) then begin
    GetMetaData;
  end;

  if FMode = cmFields then begin
    Prop.iFields := 13; //##JVR Length(FieldPropsFLDDesc);
    Prop.iRecBufSize := SizeOf(FieldPropsFLDDesc[0]);
    Prop.bReadOnly := True;
  end
  else begin
    Prop.iFields := GetFlatViewFieldCount;
    Prop.iRecBufSize := LogicalBufferSize;
    Prop.bReadOnly := not FCanModify;
  end;
end;  { GetDatasetProps }


function TDBIDataConnection.GetFieldCount: Integer;
begin
  Result := Length(FFieldProps);
end;  { GetFieldCount }


// _____________________________________________________________________________
{**
  Jvr - 03/11/2000 14:02:05 - Get Field Descriptions.<P>

  The record structure describing the field attibutes.
  <CODE>
  DSFLDDesc = packed record
    szName          : DBINAME;       Field name
    iFldType        : Integer;       Field type
    iFldSubType     : Integer;       Field subtype (if applicable)
    iUnits1         : Integer;       Number of AnsiChars, precision etc
    iUnits2         : Integer;       Decimal places etc.
    iFldLen         : Integer;       Length in bytes (computed)
    iFldOffsInRec   : Integer;       Offset to field  in record buffer
    iNullOffsInRec  : Integer;       Offset to null flag (1byte) in record buffer
    iFieldID        : Word;          FieldID of this field
    iFieldIDParent  : Word;          FieldID of parent, if any (part of ADT or ARRAY)
    bCalculated     : LongBool;      Field is Calculated
    iFldAttr        : Integer;       Field attributes
    iOptParameters  : Integer;       Number of optional parameters for field
  end;
  </CODE>
}
procedure TDBIDataConnection.GetFieldProps(Fields: pDSFLDDesc);
  procedure ExtractFieldProps(Fields: pDSFLDDesc);
  var
    Index: Integer;
    ChildIndex: Integer;
    FieldIndex: Integer;

  begin
    ChildIndex := 0;
    FieldIndex := 0;

    for Index := Low(FFieldProps) to High(FFieldProps) do begin
      // If Child field then skip
      if ChildIndex > 0 then begin
        Dec(ChildIndex);
        Continue;
      end;

      // If nested dataset then eliminate the child fields
      if (FFieldProps[Index].iFldType = fldTABLE) then begin
        ChildIndex := FFieldProps[Index].iUnits1;
      end;

      TFieldDescList(Fields)[FieldIndex] := FFieldProps[Index];
      TFieldDescList(Fields)[FieldIndex].iFieldID := FieldIndex+1;

      if ChildIndex > 0 then begin
        TFieldDescList(Fields)[FieldIndex].iUnits1 := ChildIndex + 1;
        TFieldDescList(Fields)[FieldIndex].iUnits2 := 0;
      end;

      Inc(FieldIndex);
    end;
  end;  { ExtractFieldProps }
  

begin
  // Copy the result to the Prop parameter
  if (FMode = cmFields) then begin
    Move(
      (@FieldPropsFLDDesc[0])^,
      Fields^,
      SizeOf(FieldPropsFLDDesc[0]) * Length(FieldPropsFLDDesc)
      );
  end
  else begin
    ExtractFieldProps(Fields);
  end;
end;  { GetFieldProps }


// _____________________________________________________________________________
{**
  Jvr - 04/12/2000 14:50:05.<BR>
  Jvr - 08/05/2001 11:05:08 - Corrected the position test, the number of records
                              are the number of fields in the table structure,
                              NOT the number of fields in the internal
                              Field properties table structure!<P>
}
procedure TDBIDataConnection.GetFieldRecord(
  const Position: Integer;
  out Buffer;
  DataInfo: PDataInfo
  );
begin
  // If DataInfo is assigned then populate DataInfo
  if Assigned(DataInfo) then begin
    DataInfo.Attribute := dsRecUnmodified;
    DataInfo.Data := nil;
  end;

  if (Position < 0) or (Position >= Length(FFieldProps{FieldPropsFLDDesc})) then begin
    Error(nil, 'GetFieldRecord', '1610', 'Record "%d" out of legal range', [Position]);
  end;

  // If buffer is nil then we only want the status
  if (@Buffer <> nil) then begin
    Move((@FFieldProps[Position])^, Buffer, SizeOf(FFieldProps[0]));
  end;
end;  { GetFieldRecord }


// _____________________________________________________________________________
{**
  Jvr - 06/12/2000 16:14:05 - Initila code<BR>
  Jvr - 17/09/2002 13:39:27 - Added BCD support<P>
}
function TDBIDataConnection.GetFieldSize(pFldDes: pDSFLDDesc): Integer;
begin
  if (pFldDes^.iUnits1) <= 0 then begin
    if (pFldDes^.iFldType in [fldDATE..fldDATETIME, fldINT8, fldUINT8]) then begin
      pFldDes^.iUnits1 := 1;
    end;
  end;

  case pFldDes^.iFldType of
    fldUNKNOWN:    Result := sizeofUNKNOWN;
    fldZSTRING:    Result := pFldDes^.iUnits1 + 1;
    fldDATE:       Result := sizeofDATE;
//  fldBLOB:       See below!
    fldBOOL:       Result := sizeofBOOL;
    fldINT16:      Result := sizeofINT16;
    fldINT32:      Result := sizeofINT32;
    fldFLOAT:      Result := sizeofFLOAT;
{$ifndef fpc}
    fldBCD:        Result := SizeOf(TBcd);     // Delphi uses a TBCD FieldBuffer
{$else}
    fldBCD:        Result := SizeOf(Currency); // FCP uses a currency FieldBuffer
{$endif}
    fldBYTES:      Result := pFldDes^.iUnits1;
    fldTIME:       Result := sizeofTIME;
    fldTIMESTAMP:  Result := sizeofTIMESTAMP;
    fldUINT16:     Result := sizeofUINT16;
    fldUINT32:     Result := sizeofUINT32;
    fldFLOATIEEE:  Result := sizeofFLOATIEEE;
    fldVARBYTES:   Result := sizeofVARBYTES;
    fldLOCKINFO:   Result := sizeofLOCKINFO;
    fldCURSOR:     Result := sizeofCURSOR;
    fldINT64:      Result := sizeofINT64;
    fldUINT64:     Result := sizeofINT64;
    fldADT:        Result := sizeofADT;
    fldARRAY:      Result := sizeofARRAY;
    fldREF:        Result := sizeofREF;
    fldTABLE:      Result := sizeofTABLE;
    fldDATETIME:   Result := sizeofDATETIME;
    fldFMTBCD:     Result := SizeOf(TBcd);
    fldWIDESTRING: Result := pFldDes^.iUnits1 + 2;
    fldINT8:       Result := sizeofINT8;
    fldUINT8:      Result := sizeofUINT8;
    fldSINGLE:     Result := sizeofSINGLE;
    fldUNICODE:    Result := pFldDes^.iUnits1 + 2;

    fldBLOB: begin
      case pFldDes^.iFldSubType of
        fldstMEMO: Result := sizeofMemo;
      else
        Result := sizeofBLOB;
      end;
    end;  { fldBLOB }

  else
    Result := sizeofUNKNOWN;
  end;  { case }

  if (Result <= 0) then begin
    Error(nil, 'GetFieldSize', '1600', 'Unknown Data Type "%d"', [pFldDes^.iFldType]);
  end;
end;  { GetFieldSize }


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 16:12:48 - Initial code.<br>
}
function TDBIDataConnection.GetFileName: TFileName;
begin
  Result := FFileName;
end;  { GetFileName }


// _____________________________________________________________________________
{**
  Jvr - 23/03/2005 17:36:40 - Initial code.<br>
}
function TDBIDataConnection.GetFlatViewFieldCount: Integer;
var
  Index: Integer;
  ChildIndex: Integer;
  FieldIndex: Integer;

begin
  ChildIndex := 0;
  FieldIndex := 0;

  for Index := Low(FFieldProps) to High(FFieldProps) do begin
    if (ChildIndex > 0) then begin
      Dec(ChildIndex);
      Continue;
    end;

    if (FFieldProps[Index].iFldType = fldTable) then begin
      ChildIndex := FFieldProps[Index].iUnits1;
    end;

    Inc(FieldIndex);
  end;

  Result := FieldIndex;
end;  { GetFlatViewFieldCount }


function TDBIDataConnection.GetLogicalBufferSize: Integer;
begin
  Result := FLogicalBufferSize;
end;  { GetLogicalBufferSize }


// _____________________________________________________________________________
{**
  Jvr - 05/12/2000 13:27:26.<br>
  Jvr - 25/05/2001 13:36:36 - No longer using GetCurrentRecord() because the
                              implementation of different vendors can't be
                              relied upon (including Borland) [pretty sad!].<br>
  Jvr - 10/08/2004 12:32:41 - Added Loadoptions parameter.<p>
}
type
  TLoadFromDataset = class(TDataset);
  PDateTimeRec = ^TDateTimeRec;

procedure TDBIDataConnection.LoadFromDataset(
  ADataset: TDataset;
  RecordLoadMode: TDBIRecordLoadMode
  );
var
  RecordBuffer: TDBIRecordBuffer;
  DisabledState: Boolean;
  TempProps: TFieldDescList;

  procedure LoadBufferFromFields(Fields: TFields; Buffer: TDBIRecordBuffer);
  var
    FieldNo: Integer;
    DataField: TField;
    Offset: Integer;

    BooleanValue: Boolean;
    FloatValue: Double;
    Int16Value: SmallInt;
    Int32Value: Integer;
    Int64Value: Int64;

  begin
    for FieldNo := 0 to FieldCount - 1 do begin
      DataField := Fields[FieldNo];
      Offset := FieldProps[FieldNo].iFldOffsInRec;

      case FieldProps[FieldNo].iFldType of
        fldZSTRING: begin
          Move(TDBIRecordBuffer(DataField.AsString)^, Buffer[Offset], DataField.DataSize)
        end;

        fldFLOAT: begin
          FloatValue := DataField.AsFloat;
          Move(FloatValue, Buffer[Offset], DataField.DataSize)
        end;

        fldINT16: begin
          Int16Value := DataField.AsInteger;
          Move(Int16Value, Buffer[Offset], DataField.DataSize)
        end;

        fldINT32, fldREF: begin
          Int32Value := DataField.AsInteger;
          Move(Int32Value, Buffer[Offset], DataField.DataSize)
        end;

        fldBOOL: begin
          BooleanValue := DataField.AsBoolean;
          Move(BooleanValue, Buffer[Offset], DataField.DataSize)
        end;

        fldINT64: begin
          Int64Value := DataField.AsInteger;
          Move(Int64Value, Buffer[Offset], DataField.DataSize)
        end;

        fldTIMESTAMP: begin
          PDateTimeRec(@Buffer[Offset])^.DateTime :=
            TimeStampToMSecs(DateTimeToTimeStamp(DataField.AsDateTime));
        end;

        fldBCD: begin
          FloatValue := DataField.AsFloat;
          Move(FloatValue, Buffer[Offset], DataField.DataSize)
        end;

        else begin
          Error(nil, 'LoadBufferFromFields', '1675', 'Field type not Supported', []);
        end;

      end; { case }
    end;  { for }
  end;  { LoadBufferFromFields }


begin
  if not ADataset.Active then begin
    ADataset.Open;
  end;

  // If we have predefined fielddefs then save them.
  TempProps := FieldProps;

  // Get and set the DatasetProps
  FieldCount := ADataset.FieldCount;
  LogicalBufferSize := TLoadFromDataset(ADataset).GetRecordSize;
  EncodeFieldDescs(ADataset.FieldDefs);

  // Create record buffer
  RecordBuffer := AllocMem(LogicalBufferSize);
  DisabledState := ADataset.ControlsDisabled;
  try
    if not DisabledState then begin
      ADataset.DisableControls;
    end;

    if (RecordLoadMode = rlmLoadCurrentRecord) then begin
      // Get record from source
      LoadBufferFromFields(ADataset.Fields, RecordBuffer);
      Append(RecordBuffer^);
    end

    // Otherwise load all records
    else {RecordLoadMode = rlmLoadAllRecords} begin
      ADataset.First;
      while not ADataset.Eof do begin
        // Get record from source
        LoadBufferFromFields(ADataset.Fields, RecordBuffer);
        Append(RecordBuffer^);

        ADataset.Next;
      end;  { while }
    end;  { if }

  finally
    if not DisabledState then begin
      ADataset.EnableControls;
    end;

    FreeMem(RecordBuffer);
  end;

  // If we had predefined fielddefs then restore them.
  if (Length(TempProps) > 0) then begin
    FieldProps := TempProps;
  end;
end;  { LoadFromDataset }


// _____________________________________________________________________________
{**
  Jvr - 28/06/2005 13:26:28 - Initial code.<br>
}
procedure TDBIDataConnection.LoadFromFile(
  AFileName: String;
  const Format: TDBIDataFormat
  );
begin
  Close;
  Release;

  // If filename is not blank then set it, otherwise use the existing one
  if (AFileName <> '') then begin
    FFileName := AFileName;
  end;

  SetStreamMode(smLoadFromFile, FileName <> '');
end;  { LoadFromFile }


// _____________________________________________________________________________
{**
  Jvr - 30/06/2005 13:07:49 - Initial code.<br>
}
procedure TDBIDataConnection.LoadFromStream(
  AStream: TStream;
  const Format: TDBIDataFormat
  );
begin
  Assert(AStream <> nil);

  Close;
  Release;

  FFileName := '';
  SetStreamMode(smLoadFromStream, AStream <> nil);
end;  { LoadFromStream }


// _____________________________________________________________________________
{**
  Jvr - 23/11/2000 16:21:09<P>
}
procedure TDBIDataConnection.NotifyDataEventCallBacks(
  iClientData: TDBIClientData;
  DataEventType: TDBIDataChangeEventType;
  pRecBuf: Pointer
  );
var
  DataEventCallBackObject: TDataEventCallBackObject;
  Index: Integer;

begin
  for Index := 0 to FDataEventCallBackList.Count - 1 do begin
    DataEventCallBackObject := FDataEventCallBackList[Index]as TDataEventCallBackObject;
    if Assigned(DataEventCallBackObject.DataChangeMethod) then begin
      DataEventCallBackObject.DataChangeMethod(iClientData, DataEventType, pRecBuf);
    end;
  end;  { for }
end;  { NotifyDataEventCallBacks }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 15:19:20<P>
}
procedure TDBIDataConnection.Open;
begin
  Active := True;
end;  { Open }


// _____________________________________________________________________________
{**
  Release or Free data associated with the dataset.

  Jvr - 30/06/2005 14:26:28 - Initial code.<br>
}
procedure TDBIDataConnection.Release;
begin
  // Do nothing in the base class
end;  { Release }


// _____________________________________________________________________________
{**
  Jvr - 23/11/2000 15:00:59<P>
}
procedure TDBIDataConnection.RemoveDataEventCallBack(
  DataEventCallBack: TDBIDataChangeEvent);
var
  ItemIndex: Integer;

begin
  if (FDataEventCallBackList = nil) then Exit;

  ItemIndex := FindDataEventCallBack(DataEventCallBack);
  if (ItemIndex >= 0) then begin
    FDataEventCallBackList.Delete(ItemIndex);
  end
  else begin
    Error(nil, 'RemoveDataEventCallBack', '1675',
      'Attempted to remove a non-existent data event notify callback', []);
  end;
end;  { RemoveDataEventCallBack }


// _____________________________________________________________________________
{**
  Remove all data from dataset. - This is not appropriate in our case

  Jvr - 12/02/2001 18:17:08.<P>
}
procedure TDBIDataConnection.Reset;
begin
  // Do nothing in the base class
end;  { Reset }


// _____________________________________________________________________________
{**
  Jvr - 28/06/2005 13:40:06 - Initial code.<br>
}
procedure TDBIDataConnection.SaveToFile(
  AFileName: String;
  const Format: TDBIDataFormat
  );
const
  Caller = 'SaveToFile';

var
  Stream: TStream;

begin
  if (AFileName = '') then begin
    Error(nil, Caller, '1360', 'Invalid FileName', []);
  end;

  if not Active then begin
    Error(nil, Caller, '1365', 'DataConnection not active', []);
  end;

  if (Format = dfCDS) then begin
    Error(nil, Caller, '1370', 'Saving to XML not Supported Yet!', []);
  end;

  Stream := TDBIFileStream.CreateFileStream(AFileName, fmCreate);
  try
    SaveToStream(Stream, Format);
  finally
    Stream.Free;
  end;
end;  { SaveToFile }


// _____________________________________________________________________________
{**
  Jvr - 02/11/2013 13:31:22<P>
}
procedure TDBIDataConnection.SaveToStream(AStream: TStream; const Format: TDBIDataFormat);
begin
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']);
end;


// _____________________________________________________________________________
{**
  Jvr - 02/11/2000 15:11:18<P>
}
procedure TDBIDataConnection.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;  { SetActive }


// _____________________________________________________________________________
{**
  Jvr - 08/04/2002 13:12:00.<P>
}
procedure TDBIDataConnection.SetExclusive(Index: TDBIConnectionStatus; const Value: Boolean);
begin
  if (Index = csDeActivate) then Close;

  FExclusive := Value;
end;  { SetExclusive }


procedure TDBIDataConnection.SetFieldCount(const Value: Integer);
begin
  SetLength(FFieldProps, Value);
end;  { SetFieldCount }


// _____________________________________________________________________________
{**
  Set the Logical field properties.

  Jvr - 28/11/2000 14:36:38<BR>
  Jvr - 20/12/2000 15:00:59 - Now updates Field & null offsets + LogicalBufferSize<P>
}
procedure TDBIDataConnection.SetFieldProps(const Fields: TFieldDescList);
const
  Caller = 'SetFieldProps';

var
  FieldNo: Integer;

begin
  try
    FFieldProps := Fields;

    // Calculate Logical RecordSize + Logical Field Offsets
    FLogicalBufferSize := Default_LogicalBufferFirstFieldOffset;

    for FieldNo := 0 to FieldCount - 1 do begin
      with FFieldProps[FieldNo] do begin
        iFieldID := FieldNo+1;
        iFldLen := GetFieldSize(@FFieldProps[FieldNo]);
        iFldOffsInRec := FLogicalBufferSize;
        Inc(FLogicalBufferSize, iFldLen);
      end;
    end;  { for }

    // Calculate Field Null-Flag Offsets
    for FieldNo := 0 to FieldCount - 1 do begin
      with FFieldProps[FieldNo] do begin
        iNullOffsInRec := FLogicalBufferSize + FieldNo;
      end;
    end;  { for }

    // Allow for FieldCount * Null-Flags at the end of the Buffer
    Inc(FLogicalBufferSize, FieldCount);

  except
    Error(nil, Caller, '2495', 'Failed to set dataset field properties', []);
  end;  { try..except }
end;  { SetFieldProps }


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 16:46:33 - Initial code.<br>
}
procedure TDBIDataConnection.SetFileName(const Value: TFileName);
begin
  Close;
  Release;

  FFileName := Value;
  SetStreamMode(smFileStream, FFileName <> '');
end;  { SetFileName }


// _____________________________________________________________________________
{**
  Jvr - 30/06/2005 14:38:32 - Updated code.<br>
}
procedure TDBIDataConnection.SetLogicalBufferSize(const Value: Integer);
begin
  FLogicalBufferSize := Value;
end;  { SetLogicalBufferSize }


// _____________________________________________________________________________
{**
  Jvr - 04/12/2000 18:24:51<P>
}
procedure TDBIDataConnection.SetMode(const Value: TDBIDataConnectionMode);
begin
  FMode := Value;

  if (Value = cmFields) then begin
    FGetCurrentRecord := GetFieldRecord;
  end
  else begin
    FGetCurrentRecord := Get;
  end;  { if }
end;  { SetMode }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 14:20:14<P>
}
procedure TDBIDataConnection.SetReadOnly(Index: TDBIConnectionStatus; const Value: Boolean);
begin
  if (Index = csDeActivate) then Close;

  FReadOnly := Value;
end;  { SetReadOnly }


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 22:30:39 - Initial code.<br>
}
procedure TDBIDataConnection.SetStreamMode(AStreamMode: TDBIStreamMode; IsValid: Boolean);
begin
  if IsValid then begin
    FStreamMode := AStreamMode;
  end
  else begin
    FStreamMode := smMemoryStream;
  end;
end;  { SetStreamMode }


// _____________________________________________________________________________
{**
  If Initialise is True the Buffer will be updated from the Edit source,
  Otherwise the Edit Source is updated from the Buffer.

  Jvr - 24/09/2002 12:16:25.<P>
}
procedure TDBIDataConnection.SyncRecordBuffer(
  var Buffer;
  const Initialise: Boolean
  );
begin
  // Do nothing in the base class
end;  { SyncRecordBuffer }


// _____________________________________________________________________________
{**
  Jvr - 20/09/2002 11:28:35.<P>
}
procedure TDBIDataConnection.ValidateField(
  const Position: Integer;
  const FieldNo: Word;
  pFldBuf: Pointer
  );
begin
  // Do nothing in the base class
end;


// _____________________________________________________________________________
{**
  This method is used bi IDBIBase to validate the state of a DataConnection
  to make sure it is ready to be accessed/opened.
  A derived class would raise an exception to indicate what the invalid state
  was of the DataConnection.

  Jvr - 27/05/2002 16:17:38.<P>
}
procedure TDBIDataConnection.ValidateMetaData;
begin
  // Do nothing in the base class
end;  { ValidateMetadata }





{ TDBILock }

// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 17:44:03.<P>
}
constructor TDBILock.Create(
  ADataConnection: TDBIDataConnection;
  ALockData: TDBILockData
  );
begin
  inherited Create;

  FDataConnection := ADataConnection;
  FLockData := ALockData;
end;  { Create }





{ TDBIRecordLock }

// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 17:59:18.<P>
}
function TDBIRecordLock.Lock: DBIResult;
begin
  if FDataConnection.LockRecord(Integer(FLockData.iResource)) then begin
    Result := DBIERR_NONE;
  end
  else begin
    Result := DBIERR_ALREADYLOCKED;
  end;
end;  { Lock }


// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 17:59:56.<P>
}
function TDBIRecordLock.Unlock: DBIResult;
begin
  if FDataConnection.UnLockRecord(Integer(FLockData.iResource)) then begin
    Result := DBIERR_NONE;
  end
  else begin
    Result := DBIERR_ALREADYLOCKED;
  end;
end;  { Unlock }





{ TDBILockList }

// _____________________________________________________________________________
{**
  Jvr - 12/04/2002 16:59:28.<P>
}
constructor TDBILockList.Create(ADataConnection: TDBIDataConnection);
begin
  inherited Create;

  FDataConnection := ADataConnection;
  FFullLocked := False;
  FLocks := TList.Create;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 12/04/2002 17:00:49.<P>
  Jvr - 19/06/2002 16:02:59 - Added try/finally blocks to ensure that
                              everything is handled properly even if an
                              exception is raised by locks left hanging.
                              !Messy! but necessary <P>
}
destructor TDBILockList.Destroy;
const
  Caller = 'Destroy';
  ErrLocksHanging = '%s left %d lock(s) hanging, the last locked resource ID = %d';

var
  LockItem: TDBILock;

begin
  try
    if (FLocks.Count > 0) then begin
      try
        LockItem := TDBILock(FLocks[FLocks.Count-1]);
        Error(nil, Caller, '1955', ErrLocksHanging, [
          LockItem.FDataConnection.Classname,
          FLocks.Count,
          LockItem.FLockData.iResource
          ]);

      finally
        ClearAll;
      end;
    end;  { if }

  finally
    FLocks.Free;
    FLocks := nil;

    SetFullLock(False);

    inherited Destroy;
  end;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 12/04/2002 15:39:33.<P>
}
function TDBILockList.Add(ALockData: TDBILockData): DBIResult;
var
  LockItem: TDBILock;

begin
  Assert(ALockData.iMode in lkLockModes);
  Assert(ALockData.pReference <> nil);
  Assert(ALockData.bLocked = True);

  // If it's a ltFulllock
  if (ALockData.iType = ltFullLock) then begin
    Assert(ALockData.iMode = lkExplicit);
    Result := SetFullLock(ALockData.bLocked);
    Exit;
  end;  { if  }

  // Otherwise it's a ltRecordLock
  Assert(ALockData.iType = ltRecordLock);
  Assert(ALockData.iResource > 0);
  LockItem := Find(ALockData);

  // If this record is not locked at all
  if (LockItem = nil) then begin
    // If a physical lock is obtained then add a lock token to the list
    if FFullLocked or FDataConnection.LockRecord(ALockData.iResource) then begin
      LockItem := TDBIRecordLock.Create(FDataConnection, ALockData);
      FLocks.Add(LockItem);
      Result := DBIERR_NONE;
    end

    // Otherwise somebody else has this record locked
    else begin
      Result := DBIERR_LOCKED;
    end;
  end

  // Does this cursor already have a lock on this record
  else if (LockItem.FLockData.pReference = ALockData.pReference) then begin
    // Upgrade lock if required
    if (LockItem.FLockData.iMode < ALockData.iMode) then begin
      LockItem.FLockData.iMode := ALockData.iMode;
    end;
    Result := DBIERR_NONE;
  end

  // Otherwise another cursor already has a lock on this record
  else begin
    Result := DBIERR_ALREADYLOCKED;
  end;  { if }
end;  { Add }


// _____________________________________________________________________________
{**
  Check to see if the specified Resource is locked.

  Jvr - 05/06/2002 16:01:30.<P>
}
function TDBILockList.Locked(ALockData: TDBILockData): DBIResult;
begin
  Result := DBIERR_NONE;
  Assert(ALockData.iMode in lkLockModes);
  Assert(ALockData.pReference <> nil);

  if (ALockData.iType = ltFullLock) then begin
    ALockData.bLocked := FullLocked;
  end
  else {ALockData.iType = ltRecordLock} begin
    Assert(ALockData.iType = ltRecordLock);
    Assert(ALockData.iResource > 0);
    ALockData.bLocked := (Find(ALockData) <> nil);
  end;

  // Does the found Lock item have a Mode, smaller or equal to the passed in value
  // if (LockItem.FLockData.iMode <= ALockData.iMode) then begin
  // end;
end;  { Locked }


// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 16:47:15.<P>
}
function TDBILockList.Remove(ALockData: TDBILockData): DBIResult;
const
  Caller = 'Remove';

var
  LockItem: TDBILock;

begin
  Result := DBIERR_NONE;
  Assert(ALockData.iMode in lkLockModes);
  Assert(ALockData.pReference <> nil);
  Assert(ALockData.bLocked = False);

  // If it's a ltFulllock
  if (ALockData.iType = ltFullLock) then begin
    Assert(ALockData.iMode = lkExplicit);
    SetFullLock(ALockData.bLocked);
    Exit;
  end;  { if }

  // Otherwise it's a ltRecordLock
  Assert(ALockData.iType = ltRecordLock);
  Assert(ALockData.iResource > 0);
  LockItem := Find(ALockData);

  // If the Lockitem was foun then proceed
  if (LockItem <> nil) then begin
    // If the found Lock item has a Mode, smaller or equal to the passed in
    // value, then remove it
    if (LockItem.FLockData.iMode <= ALockData.iMode) then begin
      // If the physical lock can be removed
      if FFullLocked or FDataConnection.UnlockRecord(ALockData.iResource) then begin
        FLocks.Remove(LockItem);
        LockItem.Free;
        Result := DBIERR_NONE;
      end

      // Otherwise something is really wrong
      else begin
        Error(nil, Caller, '2065', SystemErrorMessageParam, []);
      end;
    end

    // Otherwise the Lock has a higher iMode status so don't touch it
    else begin
{     Result := DBIERR_NOTLOCKED; //##NOP }
    end;
  end

  // Otherwise the record wasn't locked
  else begin
    Result := DBIERR_NOTLOCKED;
  end;  { if }
end;  { Remove }


// _____________________________________________________________________________
{**
  ClearAll doesn't care who's got what locked,
  it justs make sure everything is unlocked and the list is cleared.

  Jvr - 16/04/2002 17:06:41.<P>
}
function TDBILockList.ClearAll: Boolean;
var
  Index: Integer;
  LockItem: TDBILock;

begin
  Result := False;

  for Index := FLocks.Count - 1 downto 0 do begin
    LockItem := TDBILock(FLocks[Index]);
    if not FFullLocked then begin
      Result := FDataConnection.UnlockRecord(LockItem.FLockData.iResource);
    end;
    FLocks.Delete(Index);
    LockItem.Free;
  end;  { for }
end;  { ClearAll }


// _____________________________________________________________________________
{**
  Jvr - 13/06/2002 14:48:50.<P>
}
function TDBILockList.GetCount: Integer;
begin
  Result := FLocks.Count;
end;


// _____________________________________________________________________________
{**
  Jvr - 22/04/2002 13:30:29.<P>
}
function TDBILockList.Find(ALockData: TDBILockData): TDBILock;
var
  Index: Integer;
  LockItem: TDBILock;
  Found: Boolean;

begin
  Result := nil;

  // Search in reverse order.
  // I'm just guessing that this is the more likely order (LILO)
  for Index := FLocks.Count - 1 downto 0 do begin
    LockItem := TDBILock(FLocks[Index]);
    Found :=
      (LockItem.FLockData.iType = ALockData.iType) and
      (LockItem.FLockData.iResource = ALockData.iResource) and
      (LockItem.FLockData.pReference = ALockData.Preference);

    if Found then begin
      Result := LockItem;
      Break;
    end;
  end;  { for }
end;  { Find }


// _____________________________________________________________________________
{**
  Jvr - 22/04/2002 15:18:22.<P>
}
function TDBILockList.SetFullLock(const Value: Boolean): DBIResult;
begin
  Result := DBIERR_NONE;

  if (Value <> FFullLocked) then begin
    // First Clear the list, then update the FullLocked property
    ClearAll;

    // Set full lock
    if Value then begin
      if not FDataConnection.Lock then begin
        Result := DBIERR_FILELOCKED;
      end
      else begin
        FFullLocked := Value;
      end;
    end

    // Reset full lock
    else begin
      FFullLocked := Value;
      if not FDataConnection.Unlock then begin
        Result := DBIERR_NOTLOCKED;
      end;
    end;
  end;  { if }
end;  { SetFullLocked }





{ TDBIBase }

// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 14:59:41.<BR>
  Jvr - 16/03/2001 16:43:14 - Added Initialisation code for StatusFilter<BR>
  Jvr - 21/09/2001 19:07:43 - Added FEmbeddedDataConnection Code<P>
  Jvr - 29/01/2003 12:23:51 - Moved the Indices to the DataConnection<P>
}
constructor TDBIBase.Create(ADataConnection: TDBIDataConnection);
begin
  inherited Create;
  Assert(ADataConnection <> nil);

  // Initialise the Status filter to default to all active records
  FDSProps.iUnused[auxStatusFilter] := dsRecActive;

  FDataConnection := ADataConnection;
  FEmbeddedDataConnections := TObjectList.Create;
  FIndices := ADataConnection.FIndices; //##JVRTDBIndexList.Create;
  FLocks := TDBILockList.Create(ADataConnection);

  // Register a callback method to deal with the (e.g Indices) on all dataevents
  FDataConnection.AddDataEventCallBack(DataEventNotify);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 21/09/2001 19:07:40.<P>
  Jvr - 19/06/2002 16:11:12 - The FLocks destructor may raise an exception if
                              any locks are left hanging. We therefore need to
                              trap this in a try finally block to ensure that
                              the rest of the destructor proceeds normally<P>
  Jvr - 29/01/2003 12:24:25 - Moved the Indices to the DataConnection<P>
}
destructor TDBIBase.Destroy;
begin
  if Assigned(FDataConnection) then begin
    FDataConnection.RemoveDataEventCallBack(DataEventNotify);
  end;

  try
    FLocks.Free;
    Flocks := nil;

  finally
    Close;

    FIndices := nil;  // Release Interface
    FEmbeddedDataConnections.Free;
    FEmbeddedDataConnections := nil;

    inherited Destroy;
  end;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 13:47:43 - Merged TStreamDBInterface<P>
}
function TDBIBase.Open: DBIResult;
begin
  Result := DBIERR_NONE;

  Assert(Assigned(FDataConnection));
  FDataConnection.Open;

  CreateDefaultIndex;
end;  { Open }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 13:48:52 - Merged TStreamDBInterface<P>
}
procedure TDBIBase.Close;
begin
  Assert(Assigned(FDataConnection));
  FDataConnection.Close;

  FIndices.Clear;
end;  { Close }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2000 16:52:43.<P>
}
function TDBIBase.CreateDS(
  iFields: LongWord;
  pFldDes: pDSFLDDesc;
  pszName: TDBINameBuffer
  ): DBIResult;
begin
  Result := DBIERR_NONE;

  Assert(Assigned(FDataConnection));
  FDataConnection.CreateDS(iFields, pFldDes, pszName);
end;  { CreateDS }


// _____________________________________________________________________________
{**
  Jvr - 14/12/2000 15:52:05<P>
}
function TDBIBase.AddField(pFldDes: pDSFLDDesc): DBIResult;
begin
  Result := DBIERR_NONE;

  Assert(Assigned(FDataConnection));
  FDataConnection.AddField(pFldDes);
end;  { AddField }


// _____________________________________________________________________________
{**
  Jvr - 14/12/2000 15:55:37<P>

  DSProps = packed record
    szName           : DBIPATH;       Name, if any
    iFields          : Integer;       Number of columns
    iRecBufSize      : Integer;       Size of record buffer
    iBookMarkSize    : Integer;       Size of bookmark
    
   [bReadOnly = not CanModify]
    bReadOnly        : LongBool;      Dataset is not updateable

    iIndexes         : Integer;       Number of indexes on dataset
    iOptParams       : Integer;       Number of optional parameters
    bDelta           : LongBool;      This is a delta dataset
    iLCID            : Integer;       Language used
    iUnused          : packed array[0..7] of Integer;  Reserved
  end;
}
function TDBIBase.GetProps(out Prop: DSProps): DBIResult;
begin
  Result := DBIERR_NONE;
  Assert(Assigned(FDataConnection));

  if (FDSProps.iFields <= 0) then begin
    FDataConnection.GetDatasetProps(FDSProps);

    // Get the Bookmark size
    FDSProps.iBookmarkSize := SizeOf(TBookmarkInfo);
  end;  { if }

  // Copy the result to the Prop parameter
  Prop := FDSProps;
end;  { GetProps }



// _____________________________________________________________________________
{**
  Get Field Descriptions.<P>

  The record structure describing the field attibutes.
  <CODE>
  DSFLDDesc = packed record
    szName          : DBINAME;       Field name
    iFldType        : Integer;       Field type
    iFldSubType     : Integer;       Field subtype (if applicable)
    iUnits1         : Integer;       Number of AnsiChars, precision etc
    iUnits2         : Integer;       Decimal places etc.
    iFldLen         : Integer;       Length in bytes (computed)
    iFldOffsInRec   : Integer;       Offset to field  in record buffer
    iNullOffsInRec  : Integer;       Offset to null flag (1byte) in record buffer
    iFieldID        : Word;          FieldID of this field
    iFieldIDParent  : Word;          FieldID of parent, if any (part of ADT or ARRAY)
    bCalculated     : LongBool;      Field is Calculated
    iFldAttr        : Integer;       Field attributes
    iOptParameters  : Integer;       Number of optional parameters for field
  end;
  </CODE>
}
function TDBIBase.GetFieldDescs(Fields: pDSFLDDesc): DBIResult;
begin
  Result := DBIERR_NONE;

  Assert(Assigned(FDataConnection));
  FDataConnection.GetFieldProps(Fields);
end;  { GetFieldDescs }


// _____________________________________________________________________________
{**
  Jvr - 20/12/2000 13:30:02<P> 
}
function TDBIBase.SetFieldDescs(Fields: pDSFLDDesc): DBIResult;
begin
  Result := DBIERR_NONE;

  Assert(Assigned(FDataConnection));
  Assert(Length(TFieldDescList(Fields)) > 0);
  FDataConnection.SetFieldProps(TFieldDescList(Fields));
end;  { SetFieldDescs }


// _____________________________________________________________________________
{**
  Create a new Index.<P>

  The record structure describing the index.
  <CODE>
  DSIDXDesc = packed record
    szName    : DBINAME;            // IndexName
    iFields   : Integer;            // Number of fields in order (0 -> base order)
    iKeyFields: DSKEY;              // FieldNumbers (First field = 0)
    iKeyLen   : Integer;            // Total length of key (computed)
    bUnique   : LongBool;
    bDescending  : DSKEYBOOL;       // TRUE ->Descending
    bCaseInsensitive : DSKEYBOOL;
  end;
  </CODE>
  <P>
  Jvr - 14/03/2001 13:37:39 - We now use the Physical RecordCount to iterate
                              through the records to build an index. We also
                              manage deleted records according to what behaviour
                              is perscribed by the StatusFilter property <BR>

  Jvr - 09/05/2002 17:27:43 - Adding functionality to provide filtering using
                              indices.<P>

  <B>Notes:</B>>BR>
  In the future an exclusive lock (physical or semaphore) will probably need
  to be placed on the file, to prevent writing to the file, during the process
  of building an index. (In single user or exclusive use this will obviously
  not be an issue)<P>

  We are consciously including deleted (actually all) items in the indices
  because that's the way I believe foxpro does it.  Then depending on what the
  filter status is, we filter out the records we are not interested in. (this is
  done in (Moveto, GetCurrentRecord, etc). This way of doing things should also
  simplify the intergration of user defined filters at a later stage.<P>

  We are also using the FDataconnection.GetCurrentRecord in this method instead
  of the Local GetCurrentRecord because the local version raises an exception if
  the record attributes don't match those specified in the StausFilter.<P>
}
function TDBIBase.CreateIndex(const IdxDesc: DSIDXDesc): DBIResult;
var
  Status: DBIResult;
  NewIndex: TDBIndex;
  LocalCursor: TDBICursor;
  RecordBuffer: TDBICharacterBuffer;
  pRecBuf: Pointer;
  IndexType: Integer;
  FieldProps: TFieldDescList;
  NbrOfRecords: Integer;
  KeyName: String;

begin
  Result := DBIERR_NONE;
  KeyName := TDBIFieldName(IdxDesc.szName);

  SetLength(FieldProps, FDSProps.iFields);
  Check(GetFieldDescs(PDSFldDesc(FieldProps)));

  if FIndices.IndexOf(TDBIIndexName(KeyName)) >= 0 then begin
    Result := DBIERR_NAMENOTUNIQUE;
    Exit;
  end;

  IndexType := FieldProps[IdxDesc.iKeyFields[0]-1].iFldType;
  if (IndexType = fldBLOB) then begin
    Result := DBIERR_INVALIDFLDTYPE;
    Exit;
  end;

  NewIndex := FIndices.CreateIndex(IndexType, IdxDesc);

  // Tell DataSet how many Indices there are
  // Im doing it this way in order to standardise
  SetProp(basePropINDEXCOUNT, TDBIPropValue(FIndices.Count));

  LocalCursor := TDBICursor.Create;
  try
    with LocalCursor do begin
      Status := InitCursor(Self);
      // While Position in TDBICursor is 1-based, TDBIData Connection is 0-based
      // and we are calling the GetCurrentRecord directly on FDataConnection,
      // therefore in this routine we need to make our loop Zero-based!
      Position := 0;
      NbrOfRecords := FDataConnection.GetCount(dsRecAll);

      // Build the index by iterating through all records
      while (Position < NbrOfRecords) and (Status = DBIERR_NONE) do begin
        Status := CheckPosition(DirectionForward);
        FDataConnection.GetCurrentRecord(Position, RecordBuffer, nil);
        pRecBuf := @RecordBuffer[0];
        NewIndex.InsertItem(pRecBuf, @(FieldProps[IdxDesc.iKeyFields[0]-1]), Position+1);
        Position := Position + 1;
      end;  { while }
    end;  { with }
  finally
    LocalCursor.Free;
  end;
end;  { CreateIndex }


// _____________________________________________________________________________
{**
  Jvr - 21/11/2000 14:34:05<P>
}
function TDBIBase.GetIndexDescs(p1: PDSIDXDesc): DBIResult;
begin
  Result := DBIERR_NONE;
  Move((@(FIndices.Descs)[0])^, p1^, SizeOf(FIndices.Descs[0]) * Length(FIndices.Descs));
end;  { GetIndexDescs }


// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 17:06:17.<P>
}
function TDBIBase.RemoveIndex(pszName: TDBIIndexName): DBIResult;
var
  Index: Integer;

begin
  Result := DBIERR_NOSUCHINDEX;

  Index :=  FIndices.IndexOf(pszName);
  if (Index = 0) then begin
    Result := DBIERR_ACTIVEINDEX;
    Exit;
  end
  else if (Index > 0) then begin
    Result := DBIERR_NONE;
    FIndices.DeleteIndex(Index);
  end;

  // Tell the DataSet how many Indices there are
  FDSProps.iIndexes := FIndices.Count;
end;  { RemoveIndex }


// _____________________________________________________________________________
{**
  Jvr - 26/10/2000 14:15:27<P>
}
function TDBIBase.GetProp(eProp: TDBIBaseProperty; piPropValue: PDBIPropValue): DBIResult;
const
  Caller = 'GetProp';

begin
  Result := DBIERR_NONE;
  Assert(Assigned(FDataConnection));

  case eProp of
    basepropREADONLY: begin
      PBoolean(piPropValue)^ := FDataConnection.ReadOnly;
    end;  { basepropREADONLY }

    basepropEXCLUSIVE_ACCESS: begin
      PBoolean(piPropValue)^ := Boolean(FDSProps.iUnused[auxExclusiveAccess]);
    end;  { basepropEXCLUSIVE }

    basepropINDEXCOUNT: begin
      PInteger(piPropValue)^ := FDSProps.iIndexes;
    end;  { basepropINDEXCOUNT }

    basepropCHANGEINDEX_VIEW: begin
      pDSAttr(piPropValue)^ := DSAttr(Byte((FDSProps.iUnused[auxStatusFilter])));
    end;  { basepropCHANGEINDEX_VIEW }

    else begin
      Error(nil,
        Caller, '3420', 'Getting property of type "%s" not supported!',
        [GetEnumName(TypeInfo(TDBIBaseProperty), Ord(eProp))]
        );
    end;  { Default }
  end;  { case }
end;  { GetProp }


// _____________________________________________________________________________
{**
  Jvr - 01/03/2013 14:18:39<P>
}
function TDBIBase.GetDataProp(eProp: TDBIBaseProperty; pPropData: PDBIPropData): DBIResult;
const
  Caller = 'GetDataProp';

begin
  Result := DBIERR_NONE;
  Assert(Assigned(FDataConnection));

  case eProp of
    basepropRESOURCELOCK: begin
      FLocks.Locked(PDBILockData(pPropData)^);
    end;  { basepropRESOURCELOCK }

    basepropDATACONNECTION: begin
      TDBIDataConnection(pPropData^) := FDataConnection;
    end;  { basepropDATACONNECTION }

    basepropINDICES: begin
      TDBIIndexList(pPropData^) := FIndices;
    end;  { basepropINDICES }

    basepropLOCKLIST: begin
      TDBILockList(pPropData^) := FLocks;
    end;  { basepropLOCKLIST }

    else begin
      Error(nil,
        Caller, '3455', 'Getting property of type "%s" not supported!',
        [GetEnumName(TypeInfo(TDBIBaseProperty), Ord(eProp))]
        );
    end;  { Default }
  end;  { case }
end;  { GetProp }


// _____________________________________________________________________________
{**
  Jvr - 26/10/2000 14:24:21<P>
}
function TDBIBase.SetProp(eProp: TDBIBaseProperty; iPropValue: TDBIPropValue): DBIResult;
const
  Caller = 'SetProp';

begin
  Result := DBIERR_NONE;
  Assert(Assigned(FDataConnection));

  case eProp of
    basepropREADONLY: begin
      FDataConnection.SetReadOnly(csNoChange, Boolean(iPropValue));
    end;  { dbipropREADONLY }

    basepropEXCLUSIVE_ACCESS: begin
      FDSProps.iUnused[auxExclusiveAccess] := iPropValue;
    end;  { dbipropEXCLUSIVE }

    basepropINDEXCOUNT: begin
      FDSProps.iIndexes := Integer(iPropValue);
    end;  { basepropINDEXCOUNT }

    basepropCHANGEINDEX_VIEW: begin
      FDSProps.iUnused[auxStatusFilter] := iPropValue;
    end;  { sourcepropCHANGEINDEX_VIEW }

    else begin
      Error(nil,
        Caller, '3495',
        'Setting property of type "%s" not supported.',
        [GetEnumName(TypeInfo(TDBIBaseProperty), Ord(eProp))]
        );
    end;  { Default }

    // Notify the attached cursors that a property has changed
    FDataConnection.NotifyDataEventCallBacks(
      iPropValue,
      dbiBasePropChanged,
      Pointer(eProp)
      );
  end;  { case }
end;  { SetProp }


// _____________________________________________________________________________
{**
  Jvr - 01/03/2013 14:23:14<P>
}
function TDBIBase.SetDataProp(eProp: TDBIBaseProperty; pPropData: PDBIPropData): DBIResult;
const
  Caller = 'SetDataProp';

begin
  Result := DBIERR_NONE;
  Assert(Assigned(FDataConnection));

  case eProp of
    basepropRESOURCELOCK: begin
      if PDBILockData(pPropData)^.bLocked then begin
        Result := FLocks.Add(PDBILockData(pPropData)^);
      end
      else begin
        Result := FLocks.Remove(PDBILockData(pPropData)^);
      end;
    end;  { basepropRESOURCELOCK }

    else begin
      Error(nil,
        Caller, '3530',
        'Setting property of type "%s" not supported.',
        [GetEnumName(TypeInfo(TDBIBaseProperty), Ord(eProp))]
        );
    end;  { Default }

    // Notify the attached cursors that a property has changed
    FDataConnection.NotifyDataEventCallBacks(
      TDBIClientData(pPropData), dbiBasePropChanged, Pointer(eProp)
      );
  end;  { case }
end;  { SetProp }


// _____________________________________________________________________________
{**
  Jvr - 12/02/2001 18:12:19.<P>
}
function TDBIBase.Reset: DBIResult;
begin
  Result := DBIERR_NONE;
  Assert(Assigned(FDataConnection));

  FDataConnection.Reset;
end;  { Reset }


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 12:25:13.<P>
}
function TDBIBase.GetEmbeddedDS(iFieldID: LongWord; out DsDet: IDBIBase): DBIResult;
var
  DataConnection: TDBIDataConnection;
  DataConnectionRef: TDBIDataConnectionClass;

//  FieldDefCount: Integer;
  FieldProps: TFieldDescList;
  NestedProps: TFieldDescList;
  PFieldDescs: pDSFLDDesc;

begin
  Result := DBIERR_NONE;

//##JVR  Error(nil, 'GetEmbeddedDS', '2080', 'Not implemented Yet!', []);

  Assert(Assigned(FDataConnection));

  // We need to create a new DataConnection here and add that to a list of
  // embedded data connections so that the correct one can be retrieved by
  // Field No.
  // It can also then be freed in the destructor.
  DataConnectionRef := TDBIDataConnectionClass(FDataConnection.ClassType);
//##JVR  DataConnection := DataConnectionRef.CreateEmbedded(FDataConnection, iFieldID);
  DataConnection := DataConnectionRef.Create(FDataConnection.Owner);
  Assert(Assigned(DataConnection));

  // NOTE:
  // Possibly it may not be need to keep a list of embedded dataconnections
  // because the interface should probably take care of freeing it when it's
  // reference count is Zero, IOW when it goes out of scope.
  DsDet := TDBIBase.Create(DataConnection);

  // iField is the field no of the nested dataset and will be used to
  // get the fielddef information required from the parent.
  // Initialise the CursorProps Record & the FieldProps array
//##JVR  Check(GetCursorProps(FCursorProps));
  SetLength(FieldProps, FDataConnection.FieldCount);
  Check(GetFieldDescs(PDSFldDesc(FieldProps)));

//##JVR  Check(GetCursorProps(FCursorProps));
  SetLength(NestedProps, FieldProps[iFieldID].iUnits1);
  NestedProps[0] := FieldProps[iFieldID];
  NestedProps[1] := FieldProps[iFieldID+1];

  PFieldDescs := pDSFLDDesc(NestedProps);
  Check(DsDet.CreateDS(FieldProps[iFieldID].iUnits1, PFieldDescs, TDBINameBuffer('Nested')));


//##JVR Error(nil, 'GetEmbeddedDS', '2080', 'Not implemented Yet! - Work in progress!!! - 24/09/2001 12:08:36', []);

//##JVR  FEmbeddedDataConnections.Add(DataConnection);
end;  { GetEmbeddedDS }


// _____________________________________________________________________________
{**
  Jvr - 21/12/2000 14:24:42<P>
}
function TDBIBase.EncodeFieldDescs(FieldDefs: TFieldDefs): DBIResult;
begin
  Result := DBIERR_NONE;

  Assert(Assigned(FDataConnection));
  FDataConnection.EncodeFieldDescs(FieldDefs);
end;  { EncodeFieldDescs }





{ TDBIBase }

// _____________________________________________________________________________
{**
  Create the DEFAULT_ORDER index.

  Jvr - 04/06/2002 12:45:35.<P>
}
function TDBIBase.CreateDefaultIndex: Integer;
var
  IndexDesc: DSIDXDesc;
  NewIndex: TDBIndex;

begin
  // Index Zero
  Result := 0;

  // If we already have indices, don't bother
  if (FIndices.Count > 0) then Exit;

  // Create Default_Order Index Entry
  with IndexDesc do begin
    szName := szDEFAULT_ORDER;
    iFields := 0;
    FillChar(iKeyFields, SizeOf(iKeyFields), #0);
    iKeyLen := 0;
    bUnique := True;
    FillChar(bDescending, SizeOf(bDescending), False);
    FillChar(bCaseInsensitive, SizeOf(bCaseInsensitive), False);
  end;  { with }

  // Tell the DataSet how many Indices there are
  FDSProps.iIndexes := 1;

  // To indicate that a Default-index is to be built, fldUNKNOWN is used.
  // The fact that it is the first index also indicates that it is the
  // Default-index
  NewIndex := FIndices.CreateIndex(fldUNKNOWN, IndexDesc);
  (NewIndex as TDBIDefaultIndex).SetCount(FDataConnection.GetCount(dsRecAll));
end;  { CreateDefaultIndex }


// _____________________________________________________________________________
{**
  DataEventNotify is currently used to update the memory indices.

  Jvr - 23/11/2000 14:33:08 - Added RecNo (iClientData) to the parameters.<BR>
  Jvr - 22/07/2002 11:51:07 - Moved method from the TDBICursor to the TDBIBase<P>
}
procedure TDBIBase.DataEventNotify(
  iClientData: TDBIClientData;
  DataEventType: TDBIDataChangeEventType;
  pRecBuf: Pointer
  );
var
  RecNo: Integer;

begin
  RecNo := iClientData;

  case DataEventType of
    // Remove the entries from the indices related to this RecNo.
    dbiRecordDeleted: FIndices.DeleteIndicesItem(RecNo, pRecBuf);

    // Update the entries in the indices related to this RecNo.
    dbiRecordModified: FIndices.ModifyIndicesItem(RecNo, pRecBuf);

    // Insert entries into the indices related to this RecNo.
    dbiRecordInserted: FIndices.InsertIndicesItem(RecNo, pRecBuf);

    dbiBasePropChanged: begin
      { TODO 2 -oJvr -cTDBICursor.DataEventNotify() :
        This is probably where we need to deal with new lists being assigned
        eProp = basepropDATASETCHANGED
      }
    end;  { dbiBasePropChanged }


    else begin
      Error(nil, 'DataEventNotify', '2925', 'Illegal data event type', []);
    end;  { Default }
  end;  { case }
end;  { DataEventNotify }


// _____________________________________________________________________________
{**
  Jvr - 01/11/2000 15:14:26<P>
}
procedure TDBIBase.Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
var
  Address: Pointer;

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

  DebugInfo := 'DBIInterfaces::' + Self.ClassName + '::' + Caller + '::' + Reference + #13;
{$else}
  DebugInfo := '';
{$endif}

  raise EDBIException.CreateFmt(DebugInfo + ErrMsg + ErrorInfo, Args) at Address;
end;  { Error }





{ TDBICursor }

// _____________________________________________________________________________
{**
  Jvr - 23/07/2002 17:30:06 - Initialise Filterlock.<P>
}
constructor TDBICursor.Create;
begin
  inherited Create;

  // Filterlock is disabled at initialisation
  FFilterLock := False;

  // Initialise the FFieldProps to zero fields
  SetLength(FFieldProps, 0);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 28/10/2002 15:17:27 - Added class fields CursorProps & FieldProps<P>
}
function TDBICursor.InitCursor(DataSet: IDBIBase): DBIResult;
begin
  Result := DBIERR_NONE;

  if (DataSet = nil) then begin
    Error(nil, 'InitCursor', '580', 'DBInterface not initialised properly', []);
  end;

  // Initialise the FRecInfoOffset to cause an exception if not set
  FRecInfoOffset := $FFFF;

  // Select Default_Order for this cursor
  FSelectedIndex := 0;
  FPriorIndex := FSelectedIndex;

  // Position at BOF
  Position := 0;

  // Get the DSBase
  FDSBase := DataSet;

  // Get pointers to fields in the DSBase
  FDSBase.GetDataProp(basepropDATACONNECTION, PDBIPropData(@FDataConnection));
  FDSBase.GetDataProp(basepropINDICES, PDBIPropData(@FIndices));
  FDSBase.GetDataProp(basepropLOCKLIST, PDBIPropData(@FLocks));

  if not DataConnection.Active then begin
    FDSBase.Open;

    // Initialise the CursorProps Record & the FieldProps array
    Check(GetCursorProps(FCursorProps));
    SetLength(FFieldProps, FCursorProps.iFields);
    Check(GetFieldDescs(PDSFldDesc(FFieldProps)));
  end;
end;  { InitCursor }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2001 00:32:23 - It seems that a cloned cursor also requires an
                              FIndicesUpdatedList.<BR>

  Jvr - 22/07/2002 12:38:32 - Moved the FIndicesUpdatedList to the IndexList.<BR>
  Jvr - 28/10/2002 15:18:28 - Added class fields CursorProps & FieldProps<P>
}
function TDBICursor.CloneCursor(Cursor: IDBICursor): DBIResult;
begin
  Result := DBIERR_NONE;

  if (Cursor = nil) then begin
    Error(nil, 'CloneCursor', '2130', 'DBICursor not initialised properly', []);
  end;

  Cursor.GetDataProp(cursorpropDSBASE, PDBIPropData(@FDSBase));
  FDSBase.GetDataProp(basepropDATACONNECTION, PDBIPropData(@FDataConnection));
  FDSBase.GetDataProp(basepropINDICES, PDBIPropData(@FIndices));
  FDSBase.GetDataProp(basepropLOCKLIST, PDBIPropData(@FLocks));

  Cursor.GetProp(cursorpropRECORDPOSITION, PDBIPropValue(@FPosition));
  Cursor.GetProp(cursorpropSELECTED_INDEX, PDBIPropValue(@FSelectedIndex));
  Cursor.GetProp(cursorpropRECINFO_OFFSET, PDBIPropValue(@FRecInfoOffset));
  FPriorIndex := FSelectedIndex;

  // Initialise the CursorProps Record & the FieldProps array
  Check(GetCursorProps(FCursorProps));
  SetLength(FFieldProps, FCursorProps.iFields);
  Check(GetFieldDescs(PDSFldDesc(FFieldProps)));
end;  { CloneCursor }


// _____________________________________________________________________________
{**
  Jvr - 02/11/2000 11:42:41<P>
}
function TDBICursor.GetCursorProps(out p1: DSProps): DBIResult;
begin
  Assert(Assigned(FDSBase));

  // Copy the result to the p1 parameter
  Result := FDSBase.GetProps(p1);
end;  { GetCursorProps }

{$ifdef _UNUSED}
// _____________________________________________________________________________
{**
  Jvr - 02/04/2009 13:16:01 - Initial code.<br>

  I might want to reinstate this in the future to replace FDataConnection
}
function TDBICursor.GetDataConnection: TDBIDataConnection;
begin
  Assert(Assigned(FDSBase));

  FDSBase.GetDataProp(basepropDATACONNECTION, PDBIPropData(@Result));
end;  { GetDataConnection }
{$endif}

// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:29:36 - Now uses DSBase method to obtain data<P>

  Move((@(FIndices.Descs)[0])^, p1^, SizeOf(FIndices.Descs[0]) * Length(FIndices.Descs));
}
function TDBICursor.GetIndexDescs(
  bCurrentOnly: LongBool;
  out IdxDesc: DSIDXDesc
  ): DBIResult;

begin
  if (Length(FIndices.Descs) <= 0) then begin
    Error(nil, 'GetIndexDescs', '1715', 'Indices not initialised', []);
  end;

  if bCurrentOnly then begin
    IdxDesc := FIndices.Descs[FSelectedIndex];
    Result := DBIERR_NONE;
  end
  else begin
    { TODO -oJvr -cTDBICursor.GetIndexDescs() :
      This is probably the way to do it
      IdxDesc := PDSIDXDesc(@(FIndices.Descs))^;
    }
    Result := DBIERR_NOTINDEXED;
    { TODO 5 -oJvr -cTDBICursor.GetIndexDescs() :
      Not implemented Yet! - function returns: Index Description
    }
    Error(nil, 'GetIndexDescs', '1730', 'Not implemented Yet!', []);
  end;  { if }
end;  { GetIndexDescs }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:39:59 - Now uses DSBase method to obtain data<P>
}
function TDBICursor.GetFieldDescs(p1: pDSFLDDesc): DBIResult;
begin
  Assert(Assigned(FDSBase));

  // Copy the result to the Prop parameter
  Result := FDSBase.GetFieldDescs(p1);
end;  { GetFieldDescs }


// _____________________________________________________________________________
{**
  Jvr - 16/03/2001 14:50:42 - if pRecBuf is nil then we only want to know if the
                              current record is within the valid range.<BR>
  Jvr - 19/06/2002 14:45:53 - Now checking the return values for locking inplace

  <B>Notes:</B><BR>
  We need to check that the attributes returned by
  TDBIDataConnection.GetCurrentRecord are present in the StatusFilter<BR>
    > Return DBIERR_NONE<BR>
  Otherwise it is not a legal record<BR>
    > Return DBIERR_KEYORRECDELETED<BR>
  <P>
}
function TDBICursor.GetCurrentRecord(pRecBuf: Pointer): DBIResult;
const
  Caller = 'GetCurrentRecord';

var
  RecordPosition: Integer;
//##JVR  RecordAttribute: DSAttr;
  DataInfo: TDBIDataInfo;
  StatusFilter: DSAttr;
  PRecordInfo: PRecInfo;

begin
  Assert(Assigned(DataConnection));

  Result := CheckPosition(DirectionCurrent);
  { TODO 5 -oJvr -cTXBaseDBICursor.GetCurrentRecord() :
    This probably needs to be changed to do a proper status
    lookup from the DataConnection
  }
  if (Result <> DBIERR_NONE) or (pRecBuf = nil) then begin
    Exit;
  end;

  try
    RecordPosition := ActiveIndex.RecNo[Position] - 1;
    Check(FDSBase.GetProp(basepropCHANGEINDEX_VIEW, PDBIPropValue(@StatusFilter)));

    // Position the PRecordInfo pointer at the correct offset of the record buffer
    PRecordInfo := PRecInfo(TDBIRecordBuffer(pRecBuf) + FRecInfoOffset);
    try
      // Just a check to make sure that it's ok to reuse dsRecOrg
      if (PRecordInfo^.Attribute and dsRecLockReset) = dsRecOrg then begin
        Error(nil, Caller, '3050',
          'dsRecOrg [%s], is overloaded in it''s use.',
          ['Original record (was changed)']
          );
      end;

      // Lock record if requested
      if ((PRecordInfo^.Attribute and dsRecLockReset) = dsRecLockSet) then begin
        Result := InternalSetLock(RecordPosition, True);
        if (Result <> DBIERR_NONE) then begin
          Exit;
        end;
      end;

      // Get the record data including RecordAttributes + Data/Object References
      {RecordAttribute := }DataConnection.GetCurrentRecord(RecordPosition, pRecBuf^, @DataInfo);
      PRecordInfo^.Data := DataInfo.Data;

      // Unlock record if requested (edit mode)
      if ((PRecordInfo^.Attribute and dsRecLockReset) = dsRecLockReset) then begin
        Check(InternalSetLock(RecordPosition, False));
      end;

//##JVR      if (RecordAttribute and StatusFilter) = RecordAttribute then begin
      if (DataInfo.Attribute and StatusFilter) = DataInfo.Attribute then begin
        Result := DBIERR_NONE;
      end
      else begin
        Result := DBIERR_KEYORRECDELETED;
      end;  { if }

    finally
      { TODO 1 -ojvr -cTDBICursor.GetCurrentRecord() :
        Perhaps we should be doing more cleaning up here to make sure the buffer
        is returned in a predictable state.
      }
      // Always clear the lock flag to prevent unexpected locking due to buffering
      PRecordInfo^.Attribute := PRecordInfo^.Attribute and dsRecLockClear;
    end;  { try..finally }

  except
    on E: Exception do
      Error(E, Caller, '2910', 'Failed to get data for current record', []);
//##JVR      Error(E, Caller, '2910', 'Failed to get data for current record', []);
  end;  { try..except }
end;  { GetCurrentRecord }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:43:22<P>
}
function TDBICursor.GetCurrentBookMark(pBookMark: Pointer): DBIResult;
begin
  Result := DBIERR_NONE;
  Move(Position, pBookMark^, Sizeof(Position));
end;  { GetCurrentBookMark }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:45:38<P>
}
function TDBICursor.GetSequenceNumber(out iSeq: LongWord): DBIResult;
begin
  Result := DBIERR_NONE;
  iSeq := Position;
end;  { GetSequenceNumber }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:48:46<P>
}
function TDBICursor.GetRecordAttribute(out Attr: DSAttr): DBIResult;
begin
  Result := DBIERR_NONE;

  { TODO 5 -oJvr -cTDBICursor.GetRecordAttribute() :
    This is a temporary measure only and needs to be closely looked at
  }
  Attr := dsRecUnmodified;
end;  { GetRecordAttribute }


// _____________________________________________________________________________
{**
  Get the record count based on the passed in parameter.
  This is a 'Bit of a hack!' and relys on the caller always clearing or
  setting the passed in variable to a specific expected value (VERY BAD).<P>
  If the passed in value is clear (Zero) then the dsRecAll value is returned,
  otherwise the returned value is dependent on the passed in value except
  when an idex or filter is being used, then the count is obtained from the
  index/filter.<P>

  NOTE:<br>
  I really am not happy with the current state of affairs to do with the
  RecordCount and will need to think some more about how to manage this.<P>

  Should I use the StatusFilter to control this? Probably NOT because
  the StatusFilter is there to control the display of records, not the scope
  of the RecordCount.<P>
  
  Jvr - 27/10/2000 15:39:16 - Now uses Getprop to obtain data<BR>
  Jvr - 19/06/2002 16:52:59 - Added code to request a RecordCount refresh<BR>
  Jvr - 09/07/2002 12:42:18 - Added code to get the active RecordCount<P>
}
function TDBICursor.GetRecordCount(out iRecs: Integer): DBIResult;
begin
  Result := DBIERR_NONE;

  if (FSelectedIndex > 0) then begin
    iRecs := ActiveIndex.Count;
  end
  else if (iRecs = dsRecRefresh) then begin
    iRecs := DataConnection.GetCount(dsRecRefresh);
  end
  { TODO -oJvr -cTDBICursor.GetRecordCount() :
    ActiveRecordCount hasbeendisabled for now
    This needs further work!!!

    03/04/2009 13:08:40

    ##RECORDCOUNT
    else if (iRecs = dsRecActive) then begin
      iRecs := DataConnection.GetCount(dsRecActive);
    end
  }
  else begin
    iRecs := DataConnection.GetCount(dsRecAll);
  end;
end;  { GetRecordCount }

(*##JVR
const
  Caller = 'GetRecordCount';

var
  Position: Integer;
  Attributes: DSAttr;
  PRecBuf: Pointer;

begin
  Result := -1;
  PRecBuf := nil;

  // If Physical Recordcount is Zero then return 0 no matter what
  if FHeader.RecordCount = 0 then begin
    Result := 0;
    Exit;
  end;

  // If display all then Get the RecordCount from the metadata (Header)
  if (StatusFilter and dsRecAll) = dsRecAll then begin
    Result := FHeader.RecordCount;
  end

  // Count all active records
  else if (StatusFilter and dsRecActive) = dsRecActive then begin
    Result := 0;

    for Position := 0 to FHeader.RecordCount-1 do begin
      Attributes := GetCurrentRecord(Position, PRecBuf^);
      if not ((Attributes and dsRecDeleted) = dsRecDeleted) then begin
        Inc(Result);
      end;  { if }
    end;  { for }
  end

  // Count all deleted records
  else if (StatusFilter and dsRecDeleted) = dsRecDeleted then begin
    Result := 0;

    for Position := 0 to FHeader.RecordCount-1 do begin
      Attributes := FDataConnection.GetCurrentRecord(Position, PRecBuf^);
      if ((Attributes and dsRecDeleted) = dsRecDeleted) then begin
        Inc(Result);
      end;  { if }
    end;  { for }
  end

  // Otherwise it's an error
  else begin
    Error(nil, Caller, '1115',
      '"%d" is Not a valid RecordCount StatusFilter',
      [StatusFilter]
      );
  end;  { if }
end;
*)

// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:5058<P>
}
function TDBICursor.MoveToBOF: DBIResult;
begin
  Result := DBIERR_NONE;
  Position := 0;
end;  { MoveToBOF }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:52:51<P>
}
function TDBICursor.MoveToEOF: DBIResult;
var
  RecCount: Integer;

begin
  Result := DBIERR_NONE;

  // Request the underlying DataConnection to get the latest record count
  RecCount := dsRecRefresh;

  GetRecordCount(RecCount);
  Position := RecCount + 1;
end;  { MoveToEOF }


// _____________________________________________________________________________
{**
  Move to the next valid record, passing all filters currently enabled.

  Jvr - 16/03/2001 17:50:31 - Move to the next record meeting the StatusFilter's
                              specified attributes.<br>
  Jvr - 02/05/2001 17:44:45 - Added Code based (FilterCallback) filtering.<br>
  Jvr - 11/05/2001 14:49:07 - Improved Filtering capabilities.<br>
  Jvr - 31/05/2002 12:14:22 - Filtering moved to the Indices.<br>
  Jvr - 09/12/2004 17:26:56 - Replaced RecordAttribute with DataInfo<p>

  <b>Notes:</b><br>
  Using an index should simplify this whole thing because one would assume
  that only active records are indexed and therefore no filtering is required.<P>

  But guess what, according to some preliminary experiments done by BMC,
  deleted records are maintained in the index and therefore still need to be
  filtered. So the existing code is probably spot on!<p>

}
function TDBICursor.MoveRelative(i: Integer): DBIResult;
var
  StatusFilter: DSAttr;
//##JVR  RecordAttribute: DSAttr;
  DataInfo: TDBIDataInfo;
  RecordPosition: Integer;
  PRecBuf: TDBIRecordBuffer;

begin
  Result := DBIERR_NONE;
  PRecBuf := nil;
//##JVR  RecordAttribute := dsRecUnmodified;
  DataInfo.Attribute := dsRecUnModified;
  try
    // Get the status-values property
    FDSBase.GetProp(basepropCHANGEINDEX_VIEW, PDBIPropValue(@StatusFilter));
      // Filter the records according to their Status
      repeat
        Position := Position + i;
        Result := CheckPosition(i);

        if (Result = DBIERR_NONE) then begin
          RecordPosition := ActiveIndex.RecNo[Position] - 1;
          {RecordAttribute := }DataConnection.GetCurrentRecord(RecordPosition, PRecBuf^, @DataInfo);
        end
        else begin
          Break;
        end;
      until (DataInfo.Attribute and StatusFilter) = DataInfo.Attribute;
//##JVR      until (RecordAttribute and StatusFilter) = RecordAttribute;
  except
    on E: Exception do
      Error(E, 'MoveRelative', '2525', 'Failed to move to next record in dataset', []);
  end;
end;  { MoveRelative }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:54:46<P>
}
function TDBICursor.MoveToSeqNo(i: LongWord): DBIResult;
begin
  Position := i;
  Result := CheckPosition(DirectionCurrent);
end;  { MoveToSeqNo }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:57:21<P>
}
function TDBICursor.MoveToRecNo(i: LongWord): DBIResult;
begin
  Assert(Assigned(FIndices));
  Position := ActiveIndex.SeqNo[i];

  Result := CheckPosition(DirectionCurrent);
end;  { MoveToRecNo }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 15:59:16<P>
}
function TDBICursor.MoveToBookMark(pBookMark: Pointer): DBIResult;
begin
  Position := Integer(TBookmarkInfo(pBookmark^));
  Result := CheckPosition(DirectionCurrent);
end;  { MoveToBookMark }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 16:02:12<P>
}
function TDBICursor.CompareBookMarks(pBookMark1, pBookMark2: Pointer;
  out iCmp: Integer): DBIResult;
begin
  Result := DBIERR_NONE;

  // bookmarks are equal if they are both nil or they both have the same value
  if (pBookmark1 = pBookmark2) then begin
    iCmp := 0;
  end
  else begin
    iCmp := 1;

    if TBookmarkInfo(pBookmark1^) = TBookmarkInfo(pBookmark2^) then begin
      iCmp := 0;
    end;
  end;  { if }
end;  { CompareBookMarks }


// _____________________________________________________________________________
{**
  Find the record to match the fielddata in pRecBuf maching the fields specified
  in pKey. iFields specifies how many fields are in the pKey and iPartLen
  indicates if the value is to be an exact match or only a partial match.
  <P>
  Jvr - 01/02/2001 16:28:49 - Initial version<BR>
  Jvr - 11/09/2001 14:47:22 - Added functionality to deal with Deleted Records<BR>
  Jvr - 14/06/2002 13:18:21 - Now initialising the pRecBuf to prevent mishaps.<P>
  Jvr - 28/10/2002 15:19:04 - Now using class fields CursorProps & FieldProps<P>
}
function TDBICursor.GetRecordForKey(
  iFields: LongWord;
  iPartLen: LongWord;
  pKey: Pointer;
  pRecBuf: Pointer
  ): DBIResult;
var
  Index: Integer;
  FieldCount: Integer;
  Status: DBIResult;
  KeyValues: TZStringList;
  Buffer: TDBICharacterBuffer;
  IsBlank: LongBool;
  Found: Boolean;
  PRecordInfo: PRecInfo;

  function GetFieldCount: Integer;
  begin
    // Determine the number of fields to compare
    if (FDSLocateProps.iFields > Integer(iFields)) and
      (FFieldProps[FDSLocateProps.iKeyFields[FDSLocateProps.iFields - 1] - 1].iFldType = fldZSTRING) and
      (iPartLen > 0) then
    begin
      Result := FDSLocateProps.iFields;
    end
    else begin
      // iFields is actually the field-Id of the last searchable field in the list
      Result := iFields;
    end;  { if }
  end;  { GetFieldCount }


  function DoCompare(iFieldNo: Integer; pKeyValue, pBuffer: TDBIParamBuffer; Size: Integer;
    bCaseInsensitive: Boolean): Boolean;
  begin
    // If the field is of type string then use string comparison
    if (FFieldProps[iFieldNo-1].iFldType = fldZSTRING) then begin
{##JVR
      // If size is zero then exact match
      if (Size = 0) then begin
        Size := StrLen(pBuffer);
      end
      else begin
        Size := StrLen(pKeyValue);
      end;
}
      if bCaseInsensitive then begin
        if (Size = 0) then begin
          Result := StrIComp(pKeyValue, pBuffer) = 0;
        end
        else begin
          Result := StrLIComp(pKeyValue, pBuffer, StrLen(pKeyValue)) = 0;
        end;
      end
      else begin
        if (Size = 0) then begin
          Result := StrComp(pKeyValue, pBuffer) = 0;
        end
        else begin
          Result := StrLComp(pKeyValue, pBuffer, StrLen(pKeyValue)) = 0;
        end;
      end;  { if }
    end
    // Else NOT a string - do a binary compare
    else begin
      Size := FFieldProps[iFieldNo-1].iFldLen;
      Result := CompareMem(pKeyValue, pBuffer, Size);
    end;  { if }
  end;  { DoCompare }

begin
  Found := False;
  Status := DBIERR_NONE;

  // Setup buffers for KeyValues;
  FieldCount := GetFieldCount;
  SetLength(KeyValues, FieldCount);
  for Index := 0 to FieldCount - 1 do begin
    FillChar(KeyValues[Index], SizeOf(KeyValues[Index]), 0);
    GetField(pRecBuf, FDSLocateProps.iKeyFields[Index],
      Pointer(@KeyValues[Index][0]), IsBlank);

    if FDSLocateProps.bDescending[Index] then begin
      Error(nil, 'GetRecordForKey', '3505', 'Descending searches not implemented Yet!', []);
    end;
  end;  { for }

  // Initialise the PRecordInfo with appropriate values
  PRecordInfo := PRecInfo(TDBIRecordBuffer(pRecBuf) + FRecInfoOffset);
  PRecordInfo^.BookMarkFlag := bfInserted;
  PRecordInfo^.RecordNumber := -1;
  PRecordInfo^.RecordIdent := -1;
  PRecordInfo^.Attribute := dsRecUnmodified;
  PRecordInfo^.Data := nil;

  // Check if ActiveIndex can perform search
  // If not, then find an Index that matches the search criteria
  // If found use index to find required value
  // Else do an Unkeyed search

  // Do a 'Keyed Search'
  FDSLocateProps.bUnique := iPartLen = 0;
  Index := FIndices.FindMatchingIndex(FSelectedIndex, @FDSLocateProps);
  if (Index <> 0) then begin
    Found := FIndices[Index].Locate(@KeyValues, @FDSLocateProps, Index);
    if Found then begin
      Position := Index + 1;
      Status := GetCurrentRecord(pRecBuf);
    end
    else begin
      Status := DBIERR_EOF;
    end;
  end

  // Perform an 'UnKeyed Search'
  else begin
    Position := 1;

    while (Status <> DBIERR_EOF) do begin
      // Get the record data
      Status := GetCurrentRecord(pRecBuf);

      // This deals with deleted records and such
      if (Status <> DBIERR_NONE) then begin
        Found := False;
        Position := Position + 1;
        Continue;
      end;

      for Index := 0 to FieldCount - 1 do begin
        // Get Value and Compare with KeyValue
        FillChar(Buffer, SizeOf(Buffer), 0);
        GetField(pRecBuf, FDSLocateProps.iKeyFields[Index],
          Pointer(@Buffer[0]), IsBlank);
        Found := DoCompare(FDSLocateProps.iKeyFields[Index], KeyValues[Index],
          Buffer, iPartLen, FDSLocateProps.bCaseInsensitive[Index]);

        if not Found then Break;
      end;  { for }

      if Found then begin
        Status := DBIERR_NONE;
        Break;
      end;

      Position := Position + 1;
      Status := CheckPosition(DirectionCurrent);
    end;  { while }
  end;  { if }

  Result := Status;
end;  { GetRecordForKey }


// _____________________________________________________________________________
{**
  Jvr - 06/11/2000 17:44:17 - Get the field data from the logical buffer<BR>
  Jvr - 10/05/2001 14:53:18 - Added support for null field values.<P>
}
function TDBICursor.GetField(
  pRecBuf: Pointer;
  iFieldNo: LongWord;
  pFldBuf: Pointer;
  out bBlank: LongBool
  ): DBIResult;
begin
  Result := DBIERR_NONE;
  bBlank := False;

  { DONE 5 -oJvr -cTDBICursor.GetField() : NullFlags }
  // Nulls only supported if iNullOffsInRec > Zero
  if (FFieldProps[iFieldNo-1].iNullOffsInRec > 0) then begin
    bBlank := Boolean(Byte(TDBIRecordBuffer(pRecBuf)[FFieldProps[iFieldNo-1].iNullOffsInRec]));
  end;

  // If FieldBuffer parameter is nil, then only return a True or False value
  // in bBlank indicating if the field is Blank or not
  if (pFldBuf <> nil) and not bBlank then begin
    Inc(TDBIRecordBuffer(pRecBuf), FFieldProps[iFieldNo-1].iFldOffsInRec);

    case FFieldProps[iFieldNo-1].iFldType of
      fldWIDESTRING, fldUNICODE: begin
        PWord(pFldBuf)^ := Length(PWideChar(pRecBuf)) * 2;
        Move(pRecBuf^, (PDBIByte(pFldBuf) + SizeOf(Word))^, PWord(pFldBuf)^);
      end;
    else
      Move(pRecBuf^, pFldBuf^, FFieldProps[iFieldNo-1].iFldLen);
    end;
  end;
end;  { GetField }


// _____________________________________________________________________________
{**
  Jvr - 14/11/2000 14:55:10 - Put the field data into the Logical Buffer<BR>
  Jvr - 10/05/2001 14:50:57 - Added support for Null values.<BR>
  Jvr - 28/10/2002 15:20:13 - Now using class fields CursorProps & FieldProps<P>
}
function TDBICursor.PutField(
  pRecBuf: Pointer;
  iFieldNo: LongWord;
  pFldBuf: Pointer
  ): DBIResult;
var
  Offset: Integer;
  IsBlank: Boolean;
  Size: Word;
  PData: PDBIByte;

begin
  Result := DBIERR_NONE;
  Offset := FFieldProps[iFieldNo-1].iFldOffsInRec;
  Size := FFieldProps[iFieldNo-1].iFldLen;
  PData := pFldBuf;

  // Clear the field in the record buffer to all #0's - We don't want corrupt field data!
  FillChar(TDBIRecordBuffer(pRecBuf)[Offset], Size, 0);

  // If the field buffer is nil then the field is blank.
  IsBlank := (pFldBuf = nil);

  // Otherwise transfer the data from the fieldbuffer to the record buffer
  if not IsBlank then begin
    case FFieldProps[iFieldNo-1].iFldType of
      fldWIDESTRING, fldUNICODE: begin
        Size := PWord(pFldBuf)^;
        Inc(PData, SizeOf(Word));
      end;
    end;

    Move(PData^, TDBIRecordBuffer(pRecBuf)[Offset], Size);
    { TODO 1 -oJvr -cTDBICursor.PutField(Testing only) :
      This is for testing only to make sure that the recordbuffer matches the
      Stored data. In the future we may need to implement this to provide
      Write-Through functionality (e.g. when using objects and object lists.
    }

    // Update pRecBuf from the DataConnection Edit-source (Validation object?)
    DataConnection.SyncRecordBuffer(pRecBuf^, True);
  end;

  { DONE 5 -oJvr -cTDBICursor.PutField() : NullFlags }
  // Set the Null-Flag for this field to the 'IsBlank' value
  // If iNullOffsInRec is Zero or less then Nulls are not supported
  if (FFieldProps[iFieldNo-1].iNullOffsInRec > 0) then begin
    Byte(TDBIRecordBuffer(pRecBuf)[FFieldProps[iFieldNo-1].iNullOffsInRec]) := Ord(IsBlank);
  end;


  // Update Indices Modification List
  FIndices.AddUpdatedField(@(FFieldProps[iFieldNo-1]));
{##JVR
  if (FIndices.FieldsUpdated.IndexOf(TObject(@(FFieldProps[iFieldNo-1]))) = -1) then begin
    FIndices.FieldsUpdated.Add(TObject(@(FFieldProps[iFieldNo-1])));
  end;
}
end;  { PutField }


// _____________________________________________________________________________
{**
  Jvr - 20/09/2002 11:07:04.<P>
}
function TDBICursor.VerifyField(
  iFieldNo: LongWord;
  pFldBuf: Pointer
  ): DBIResult;
begin
  Result := DBIERR_NONE;
  Assert(Assigned(DataConnection));
  DataConnection.ValidateField(Position, iFieldNo-1, pFldBuf);
end;  { VerifyField }


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 14:01:16.<P>
}
function TDBICursor.GetBlob(
  pRecBuf: Pointer;
  iFieldNo: LongWord;
  iOffSet: LongWord;
  pBuf: Pointer;
  out iLength: LongWord
  ): DBIResult;
var
  Position: LongWord;
  IsBlank: LongBool;

begin
  Result := DBIERR_NONE;

  GetField(pRecBuf, iFieldNo, Pointer(@Position), IsBlank);
  if not IsBlank then begin
    DataConnection.GetBlob(Position, iFieldNo-1, iOffset, pBuf, iLength);
  end;
end;  { GetBlob }


// _____________________________________________________________________________
{**
  Jvr - 09/02/2001 13:59:08.<P>
}
function TDBICursor.GetBlobLen(
  pRecBuf: Pointer;
  iFieldNo: LongWord;
  out iLength: LongWord
  ): DBIResult;
var
  Position: LongWord;
  IsBlank: LongBool;

begin
  Result := DBIERR_NONE;
  iLength := 0;

  GetField(pRecBuf, iFieldNo, Pointer(@Position), IsBlank);
  if not IsBlank then begin
    DataConnection.GetBlob(Position, iFieldNo-1, 0, nil, iLength);
  end;
end;  { GetBlobLen }


// _____________________________________________________________________________
{**
  Jvr - 07/02/2001 15:59:02.<P>

  <B>Notes:</B><BR>
  If a record is new (has just been added) then IsBlank will be False but the
  position will be Zero<P>
}
function TDBICursor.PutBlob(
  pRecBuf: Pointer;
  iFieldNo: LongWord;
  iOffSet: LongWord;
  pBuf: Pointer;
  iLength: LongWord
  ): DBIResult;
var
  Position: Integer;
  IsBlank: LongBool;

begin
  Result := DBIERR_NONE;
  Position := 0;
  GetField(pRecBuf, iFieldNo, Pointer(@Position), IsBlank);

  // NewRecord but the blob is blank
  if (Position = 0) and (iLength = 0) and not IsBlank then begin
    Exit;
  end

  // If field was blank 'IsBlank" then set position to zero
  // to indicate that no Blob exists for this record
  else if IsBlank then begin
    Position := 0;
  end;
  Position := DataConnection.PutBlob(Position, iFieldNo-1, iOffset, pBuf, iLength);
  PutField(pRecBuf, iFieldNo, Pointer(@Position));
end;  { PutBlob }


// _____________________________________________________________________________
{**
  Initialise a new (empty) record buffer.  All data is set to null and
  the null flags are all initialised to #2.<P>

  Normally a null flag is either 0 (for False) or 1 (for True), but Borland
  have decided to initialise the null flags to #2 for a new record. This will
  equate to 'True' (indicating value is Null, no data) so I've decided follow
  Borland's lead and do this as well.

  Jvr - 04/06/2001 14:09:05 - Added initialisation of NullFlags to #2<BR>
  Jvr - 24/09/2002 12:19:34 - We now call DataConnection.InitialiseBuffer
                              to prepare the recordbuffer.<P>
}
function TDBICursor.InitRecord(pRecBuf: Pointer): DBIResult;
begin
  Result := DBIERR_NONE;
  try
    FillChar(pRecBuf^, FCursorProps.iRecBufSize, #0);
    FillChar(TDBIRecordBuffer(pRecBuf)[FFieldProps[0].iNullOffsInRec], FCursorProps.iFields, #2);

    // Initialise pRecBuf from the DataConnection Edit-source (Class defaults object?)
    // Update pRecBuf from the DataConnection Edit-source (Validation object?)
    DataConnection.SyncRecordBuffer(pRecBuf^, True);
  except
    on E: Exception do
      Error(E, 'InitRecord', '1060', 'Failed to Initialise Record Buffer', []);
  end;
end;  { InitRecord }


// _____________________________________________________________________________
{**
  Jvr - 03/11/2000 16:17:14<P>
}
function TDBICursor.DeleteRecord: DBIResult;
var
  RecordPosition: Integer;

begin
  Assert(Assigned(FIndices));
  Assert(Assigned(DataConnection));

  RecordPosition := ActiveIndex.RecNo[Position] - 1;
  Result := InternalSetLock(RecordPosition, True);
  if (Result = DBIERR_NONE) then begin
    try
      DataConnection.Delete(RecordPosition);
    finally
      Result := InternalSetLock(RecordPosition, False);
    end;
  end;  { if }

{##JVR - Update the indices.
  FIndices.DeleteIndicesItem(Position);
}

  { TODO 5 -oJvr -cTDBICursor.DeleteRecord() :
    Don't forget to change the record count in both the Props as well as in
    the actual data file.   What about the Indices?
  }
end;  { DeleteRecord }


// _____________________________________________________________________________
{**
  Jvr - 01/06/2002 17:44:00 - If the underlying DataConnection raises an
                              exception, we do NOT unlock the resource.<BR>
  Jvr - 28/10/2002 16:05:01 - Now using class fields CursorProps & FieldProps<P>
}
function TDBICursor.ModifyRecord(pRecBuf: Pointer): DBIResult;
var
  RecNo: Integer;
  RecordPosition: Integer;

begin
  Assert(Assigned(FIndices));
  Assert(Assigned(DataConnection));

  RecNo := ActiveIndex.RecNo[Position];
  RecordPosition := RecNo - 1;

  DataConnection.Update(RecordPosition, pRecBuf^);

  // Unlock Record after posting the data
  // (only if posting doesn't raise an exception)
  Result := InternalSetLock(RecordPosition, False);

{##JVR - Update the indices
  RecNo := FIndices.ModifyIndicesItem(RecNo, FSelectedIndex, pRecBuf);
}
  Position := ActiveIndex.SeqNo[RecNo];
end;  { ModifyRecord }


// _____________________________________________________________________________
{**
  This is not part of the clientdataset stanadard interface.
  I have added it to allow writethrough of fielddata, mainly for objects!

  Jvr - 03/06/2005 14:13:08 - Initial code.<br>
}
function TDBICursor.SyncRecord(pRecBuf: Pointer): DBIResult;
var
  RecNo: Integer;
  RecordPosition: Integer;

begin
  Result := DBIERR_NONE;
  Assert(Assigned(FIndices));
  Assert(Assigned(DataConnection));

  RecNo := ActiveIndex.RecNo[Position];
  RecordPosition := RecNo - 1;

  DataConnection.{Sync}Update(RecordPosition, pRecBuf^);

//##JVR  Position := ActiveIndex.SeqNo[RecNo];
end;  { SyncRecord }


// _____________________________________________________________________________
{**
  Jvr - 21/03/2000 14:03:52.<P>
}
function TDBICursor.InsertRecord(pRecBuf: Pointer): DBIResult;
var
  RecordPosition: Integer;
  RecNo: Integer;

begin
  RecNo := -1;
  Assert(Assigned(FIndices));
  Assert(Assigned(DataConnection));

  { TODO 2 -oJvr -cTDBICursor.InsertRecord() :
    At this stage we only support appending a record. (not inserting at position)
    This is fine for Xbase files but not good enough fot TDBIObjectListDatasets
  }
  RecordPosition := DataConnection.GetCount(dsRecAll);
  Result := InternalSetLock(RecordPosition, True);
  if (Result = DBIERR_NONE) then begin
    try
      RecNo := DataConnection.Append(pRecBuf^);
    finally
      Result := InternalSetLock(RecordPosition, False);
    end;
  end;  { if }

  // This should never happen ???
  Assert(RecNo = (RecordPosition + 1), 'New Record Number should equal the physical record count');

{##JVR - Update the indices
  RecNo := FIndices.InsertIndicesItem(RecNo, FSelectedIndex, pRecBuf);
}
  Position := ActiveIndex.SeqNo[RecNo];
end;  { InsertRecord }


// _____________________________________________________________________________
{**
  Jvr - 20/09/2002 13:40:18.<P>
}
function TDBICursor.RevertRecord: DBIResult;
begin
  Result := DBIERR_NONE;
  Assert(Assigned(DataConnection));
  DataConnection.Cancel;
end;  { RevertRecord }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2001 23:46:40<P>
}
function TDBICursor.AddFilter(
  pcanExpr: Pointer;
  iLen: LongWord;
  var hFilter: hDSFilter
  ): DBIResult;
begin
  Result := DBIERR_NONE;

  { TODO 5 -oJvr -cTDBICursor.AddFilter() :
    AddFilter - Not Implemented Yet!
  }
  Error(nil, 'AddFilter', '3000', 'Not Implemented Yet!', []);
end;  { AddFilter }


// _____________________________________________________________________________
{**
  Jvr - 02/05/2001 12:40:43 - Remove a filter from the List using the
                              Filter Handle (Object Address) as a reference.<BR>
  Jvr - 31/05/2002 13:43:57 - Filters are now handled in the Indices<P>
}
function TDBICursor.DropFilter(hFilter: hDSFilter): DBIResult;
var
  DatasetFilter: TDBIDatasetFilter;

begin
  Result := DBIERR_NONE;
  Assert(hFilter <> nil);
  DatasetFilter := TDBIDatasetFilter(hFilter);

  // Select the appropriate index
  SelectIndex(FPriorIndex);

  // Delete the Filter Index - this will free the filter as well
  FIndices.DeleteIndex(DatasetFilter.IndexNo);
end;  { DropFilter }


// _____________________________________________________________________________
{**
  Setrange sets up a filter for the current selected index.
  
  <CODE>
  Parameters:
    iFields   = The key field count
    pKey1     = A Pointer to the Buffer to Fieldvalues of Startkey
    bKey1Incl = If (True) pkey1 is to be included in the result set, else not
    pKey2     = A Pointer to the Buffer to Fieldvalues of Startkey
    bKey2Incl = If (True) pkey2 is to be included in the result set, else not
  </CODE>

  Jvr - 10/10/2002 12:43:15.<P>
}
function TDBICursor.SetRange(
  iFields: LongWord;
  pKey1: Pointer;
  bKey1Incl: LongBool;
  pKey2: Pointer;
  bKey2Incl: LongBool
  ): DBIResult;
const
  TrueOrFalse: array[Boolean] of String = ('False', 'True');

var
  k1, k2: TDBIString;

  function GetKeyValues(
    iFields: LongWord;
    pRecBuf: Pointer
    ): DBIResult;
  var
    KeyValues: TZStringList;
    IsBlank: LongBool;
    FieldID: Integer;

    function GetFieldCount: Integer;
    begin
      // Determine the number of fields to compare
      if (FDSLocateProps.iFields > Integer(iFields)) and
        (FFieldProps[FDSLocateProps.iKeyFields[FDSLocateProps.iFields - 1] - 1].iFldType = fldZSTRING) then
      begin
        Result := FDSLocateProps.iFields;
      end
      else begin
        // iFields is actually the field-Id of the last searchable field in the list
        Result := iFields;
      end;  { if }
    end;  { GetFieldCount }


  begin
    Result := DBIERR_NONE;

    FieldID := ActiveIndex.PIndexDesc^.iKeyFields[0];
(*##JVR
    // Setup buffers for KeyValues;
    FieldCount := 4; //GetFieldCount;
    SetLength(KeyValues, FieldCount);
    for Index := 0 to FieldCount-1 do begin
      FillChar(KeyValues[Index], SizeOf(KeyValues[Index]), 0);
      GetField(pRecBuf, Index+1, //FDSLocateProps.iKeyFields[Index],
        Pointer(@KeyValues[Index][0]), IsBlank);

        ShowMessage(String(TDBIRecordBuffer(Pointer(@KeyValues[Index][0]))));
    end;  { for }
//*)


    SetLength(KeyValues, 1);
    FillChar(KeyValues[0], SizeOf(KeyValues[0]), 0);
    GetField(pRecBuf, FieldID, Pointer(@KeyValues[0][0]), IsBlank);

    ShowMessage(String(TDBIString(TDBIMsgBuffer(Pointer(@KeyValues[0][0])))));
  end;


begin
//##JVR  Result := DBIERR_NONE;

  { TODO 5 -oJvr -cTDBICursor.SetRange() :
    SetRange - Not Implemented Yet!
  }
//##JVR  Error(nil, 'SetRange', '3980', 'Not Implemented Yet!', []);

  k1 := TDBIString(TDBIParamBuffer(pKey1));
  k2 := TDBIString(TDBIParamBuffer(pKey2));

  ShowMessageFmt(
    'Field Count: %d'#13'Key1: %s'#13'Inc Key1: %s'#13'Key2: %s'#13'Inc key2: %s',
    [iFields, k1, TrueOrFalse[bKey1Incl], k2, TrueOrFalse[bKey2Incl]]
    );

  Result := GetKeyValues(ifields, pKey1);
end;  { SetRange }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBICursor.DropRange: DBIResult;
begin
  Result := DBIERR_NONE;

  { TODO 5 -oJvr -cTDBICursor.DropRange() :
    DropRange - Not Implemented Yet!
  }
  ShowMessage('TDBICursor::DropRange::4725::Not Implemented Yet!');
//  Error(nil, 'DropRange', '3020', 'Not Implemented Yet!', []);
end;  { DropRange }


// _____________________________________________________________________________
{**
  Jvr - 06/02/2002 17:02:00 - Fixed error relating to no of key fields.<P>
}
type
  PDSKEYBOOL = ^DSKEYBOOL;

  // PDBILongWordList maps on to TList::List: PPointerList (see classes.pas)
  PDBILongWordList = ^TDBILongWordList;
  TDBILongWordList = array[0..$FFFE] of LongWord;

function TDBICursor.SortOnFields(iFields: LongWord; piFields: PPointer;
  pDescending: PLongBool; pCaseInsensitive: PLongBool): DBIResult;
var
  Index: Integer;
{$ifdef _AGGREGATES}
  IndexName: String;
{$endif}

  function GetBoolValue(pValue: PLongBool; Offset: Integer): LongBool;
  begin
    Result := False;

    if Assigned(pValue) then begin
      Result := PDSKEYBOOL(pValue)^[Offset];
    end;
  end;  { GetBoolValue }

begin
  Result := DBIERR_NONE;
  Assert(DataConnection <> nil);

  // If the number of key fields is greater than the number of fields
  // then ERROR!
  if (iFields > LongWord(DataConnection.FieldCount)) then begin
    Result := DBIERR_INVALIDFIELDNAME;
    Exit;
  end;

  FDSLocateProps.iFields := iFields;
  for Index := 0 to iFields - 1 do begin
    FDSLocateProps.iKeyFields[Index] := PDBILongWordList(piFields)^[Index];
    FDSLocateProps.bDescending[Index] := GetBoolValue(pDescending, Index);
    FDSLocateProps.bCaseInsensitive[Index] := GetBoolValue(pCaseInsensitive, Index);
  end;  { for }

{$ifdef _AGGREGATES}
  // This is a hack for now - it requires an existing index
  Index := PDBILongWordList(piFields)^[0];
  IndexName := String(DataConnection.FieldProps[Index-1].szName);

  Assert(FIndices.IndexOf(PChar(IndexName)) > -1, 'Index not available for this Field');
  UseIndexOrder(PChar(IndexName));
{$endif}
end;  { SortOnFields }


// _____________________________________________________________________________
{**
  Jvr - 31/05/2002 15:23:16 - Split some of the implementation out to a new
                              helper function, SelectIndex().<P>
}
function TDBICursor.UseIndexOrder(pszName: TDBIIndexName): DBIResult;
var
  DBIndex: TDBIndex;
  IndexName: String;
  DisplayOrder: TDBIDisplayOrder;
  DatasetFilter: TDBIDatasetFilter;

  function GetDisplayOrder(var IndexData: String): TDBIDisplayOrder;
  var
    Offset: Integer;

  begin
    Result := doAscending;

    Offset := Pos(';', IndexData);
    if (Offset > 0) then begin
      if (CompareText(Copy(IndexData, Offset+1, 128), 'Descending') = 0) then begin
        Result := doDescending;
      end;
      SetLength(IndexData, Offset-1);
    end;
  end;  { GetDisplayOrder }


  function UseIndex(const IndexNo: Integer): DBIResult;
  begin
    Result := DBIERR_NOSUCHINDEX;

    if (IndexNo >= 0) and (IndexNo < FIndices.Count) then begin
      // If the Active Index has a filter the re-create the filtered index
      // based on the newly selected index
      if Assigned(DatasetFilter) then begin
        // De-attach the filter from the index
        ActiveIndex.Filter := nil;

        // Select the new index
        FSelectedIndex := IndexNo;
        DBIndex := ActiveIndex;

        // Delete the current filtered index
        FIndices.DeleteIndex(DatasetFilter.IndexNo);

        // We just deleted the filter index so find the current Index
        FSelectedIndex := FIndices.IndexOfIndex(DBIndex);

        // Create a new filtered index base on the currently selected index
        Result := CreateIndex(DatasetFilter);

        if (Result = DBIERR_NONE) then begin
          Result := SelectIndex(DatasetFilter.IndexNo);
        end;
      end

      // Otherwise select the unfiltered index
      else begin
        Result := SelectIndex(IndexNo);
      end;  { if }
    end;  { if }
  end;  { UseIndex }

begin
  // Get the Filter from the Active Index if there is one
  DatasetFilter := ActiveIndex.Filter;

  if (pszName = nil) or (pszName^ = '') then begin
    Result := UseIndex(0);
  end
  else begin
    IndexName := pszName;
    DisplayOrder := GetDisplayOrder(IndexName);
    Result := UseIndex(FIndices.IndexOf(TDBIIndexName(IndexName)));
    ActiveIndex.DisplayOrder := DisplayOrder;
  end;
end;  { UseIndexOrder }


// _____________________________________________________________________________
{**
  Jvr - 04/12/2000 12:19:23<P>
}
function TDBICursor.SetNotifyCallBack(
  iClientData: TDBIClientData;
  pfCallBack: pfCHANGECallBack
  ): DBIResult;
begin
  Result := DBIERR_NONE;

  Error(nil, 'SetNotifyCallBack', '4790', 'Not implemented Yet!', []);
end;  { SetNotifyCallBack }


// _____________________________________________________________________________
{**
  Jvr - 22/03/2001 23:57:35 - Add a Filter (Callback) to the list.<BR>
  Jvr - 31/05/2002 13:43:57 - Filters are now handled in the Indices<P>
}
function TDBICursor.AddFilterCallBack(
  iClientData: LongWord;
  pfFilter: pfDSFilter;
  var hFilter: hDSFilter
  ): DBIResult;
var
  DatasetFilter: TDBIDatasetFilter;
  DisplayOrder: TDBIDisplayOrder;

begin
  DatasetFilter := TDBIDatasetCodeFilter.Create;
  DatasetFilter.Owner := TObject(iClientData);
  DatasetFilter.FilterType := dfCallBack;
  DatasetFilter.FilterProc := pfFilter;
  hFilter := DatasetFilter;

  DisplayOrder := ActiveIndex.DisplayOrder;
  Result := CreateIndex(DatasetFilter);
  if (Result = DBIERR_NONE) then begin
    Result := SelectIndex(DatasetFilter.IndexNo);
//##JVR Result := UseIndexOrder(szDEFAULT_FILTER);
    ActiveIndex.DisplayOrder := DisplayOrder;
  end;
end;  { AddFilterCallBack }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:20:07 - Initial code.<br />
}
function TDBICursor.AddAggregate(
  iFlds, iCanLen: LongWord;
  pCanExpr: Pointer;
  var hAgg: hDSAggregate
  ): DBIResult;
begin
  Result := DBIERR_NONE;

//{##AGGREGATES
  ShowMessageFmt(
    'DBIInterfaces::TDBICursor::AddAggregate::4895::Not implemented Yet!'#13 +
    'GroupLevel = "%d", Parser.DataSize = "%d", Parser.FilterDara = "Do not care", Aggregate Handle = "Integer()"',
    [iFlds, icanLen, pCanExpr]
    );
(*
//}
  ActiveIndex.GroupLevel := 1;
//*)
end;  { AddAggregate }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:21:05 - Initial code.<br />
}
function TDBICursor.DropAggregate(hAgg: hDSAggregate): DBIResult;
begin
  Result := DBIERR_NONE;

  ShowMessageFmt('DBIInterfaces::TDBICursor::DropAggregate::4905::Not implemented Yet!', []);
end;  { DropAggregate }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:23:41 - Initial code.<br />
}
function TDBICursor.GetAggregateDesc(
  hAgg: hDSAggregate;  { in }
  var Desc: DSFLDDesc  {out }
  ): DBIResult;
begin
  Result := DBIERR_NONE;

  Desc.szName := 'jvr';  // cds leaves this blank
  Desc.iFldType := fldInt32;
  Desc.iFldSubType := 0;
  Desc.iUnits1 := 0;
  Desc.iUnits2 := 0;
  Desc.iFldLen := sizeofINT32;
  Desc.iFldOffsInRec := 0;
  Desc.iNullOffsInRec := 0;
  Desc.iFieldID := 1;    // cds leaves this as 0
  Desc.iFieldIDParent := 0;
  Desc.bCalculated := False;
  Desc.iFldAttr := 0;
  Desc.iOptParameters := 0;

//  ShowMessageFmt('DBIInterfaces::TDBICursor::GetAggregateDesc::4915::Not implemented Yet!', []);
end;  { GetAggregateDesc }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:19:35 - Initial code.<br />
}
function TDBICursor.GetAggregateValue(
  hAgg: hDSAggregate;
  pValue: Pointer;
  var bBlank: LongBool
  ): DBIResult;
begin
  Result := DBIERR_NONE;
  bBlank := False;
  Integer(pValue^) := ActiveIndex.GetSubGroupCount(1, Position);

//  ShowMessageFmt('DBIInterfaces::TDBICursor::GetAggregateValue::4930::Not implemented Yet!', []);
end;


// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 13:13:25 - Initial code.<br />
}
function TDBICursor.GetDebugInfo(var Data: String): DBIResult;
begin
  Result := DBIERR_NONE;

  Data := ActiveIndex.GetDebugInfo(Position);
end;  { GetSubGroupState }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 10:24:20 - Initial code.<br />
}
function TDBICursor.GetSubGroupState(
  iFields: LongWord;
  var iSubGroupState: GROUPSTATE
  ): DBIResult;
begin
  Result := DBIERR_NONE;

{##AGGREGATES
  ShowMessageFmt(
    'DBIInterfaces::TDBICursor::GetSubGroupState::4895::Not implemented Yet!'#13 +
    'GroupLevel = "%d"',
    [iFields]
    );
(*
//}
  iSubGroupState := ActiveIndex.GetSubGroupState(0, Position);
//*)
end;  { GetSubGroupState }


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 12:30:43.<P>
}
function TDBICursor.LinkCursors(
  iFieldsM: LongWord;
  piFieldsM: PLongWord;
  piFieldsD: PLongWord;
  hCurDet: IDBICursor
  ): DBIResult;
begin
  Result := DBIERR_NONE;

  ShowMessageFmt('DBIInterfaces::TDBICursor::LinkCursors::4965::Not implemented Yet!', []);
end;  { LinkCursors }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBICursor.GetRecordNumber(out iRecNo: LongWord): DBIResult;
begin
  Result := DBIERR_NONE;
  Assert(Assigned(FIndices));

  iRecNo := ActiveIndex.RecNo[Position];
end;  { GetRecordNumber }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 15:01:25 - Initial Code<BR>
  Jvr - 28/10/2002 16:05:54 - Now using class fields CursorProps & FieldProps<P>
}
function TDBICursor.CreateIndex(Filter: TDBIDatasetFilter): DBIResult;
var
  IndexDesc: DSIDXDesc;

  Status: DBIResult;
  NewIndex: TDBIndex;
  LocalCursor: TDBICursor;
  RecordBuffer: TDBICharacterBuffer;
  pRecBuf: Pointer;
  pFieldDesc: pDSFLDDesc;
  IndexField: Integer;
  IndexType: Integer;
  RecCount: Integer;

begin
  Result := DBIERR_NONE;
  IndexDesc := ActiveIndex.PIndexDesc^;
  StrCopy(IndexDesc.szName, szDEFAULT_FILTER);

  if FIndices.IndexOf(szDEFAULT_FILTER) >= 0 then begin
    Result := DBIERR_NAMENOTUNIQUE;
    Exit;
  end;

  // An IndexField of 0 (Zero) denotes a IndexType of 'Primary', eg Natural Order
  IndexField := IndexDesc.iKeyFields[0];
  if (IndexField > 0) then begin
    IndexType := FFieldProps[IndexField-1].iFldType;
    if (IndexType = fldBLOB) then begin
      Result := DBIERR_INVALIDFLDTYPE;
      Exit;
    end;
  end
  else begin
    IndexType := fldUNKNOWN;
  end;

  NewIndex := FIndices.CreateIndex(IndexType, IndexDesc);
  NewIndex.Filter := Filter;
  Filter.IndexNo := FIndices.IndexOfIndex(NewIndex);

  // Tell DataSet how many Indices there are
  FDSBase.SetProp(basePropINDEXCOUNT, TDBIPropValue(FIndices.Count));

  LocalCursor := TDBICursor.Create;
  try
    Status := LocalCursor.InitCursor(FDSBase);
    // While Position in TDBICursor is 1-based, TDBIData Connection is 0-based
    // and we are calling the GetCurrentRecord directly on FDataConnection,
    // therefore in this routine we need to make our loop Zero-based!
    LocalCursor.Position := 0;
    RecCount := LocalCursor.DataConnection.GetCount(dsRecAll);

    // Build the index by iterating through all records
    while (LocalCursor.Position < RecCount) and (Status = DBIERR_NONE) do begin
      Status := LocalCursor.CheckPosition(DirectionForward);
      LocalCursor.DataConnection.GetCurrentRecord(
        LocalCursor.Position,
        RecordBuffer,
        nil
        );

      pRecBuf := @RecordBuffer[0];

      // If Index Field(s) have been specified, then use the supplied IndexDesc
      if (IndexField > 0) then begin
        pFieldDesc := @(FFieldProps[IndexField-1]);
      end

      // Otherwise no Key fields are specified, use the (Natural Order)
      else begin
        pFieldDesc := nil;
      end;
      NewIndex.InsertItem(pRecBuf, pFieldDesc, LocalCursor.Position+1);

      LocalCursor.Position := LocalCursor.Position + 1;
    end;  { while }
  finally
    LocalCursor.Free;
  end;
end;  { CreateIndex }


// _____________________________________________________________________________
{**
  Jvr - 31/05/2002 14:20:45.<P>
}
function TDBICursor.SelectIndex(const IndexNo: Integer): DBIResult;
var
  Normalised: Boolean;
  RecNo: Integer;

  function NormalisePosition: Boolean;
  var
    RecCount: Integer;
    
  begin
    Result := True;

    // Set the RecCount to the wanted RecordCount filter
    RecCount := dsRecAll;
    Check(GetRecordCount(RecCount));

    // BOF
    if (RecCount <= 0) or (Position <= 0) then begin
      Position := 0;
    end

    // EOF
    else if (Position > RecCount) then begin
      Position := RecCount+1;
    end

    // Position is within limits
    else begin
      Result := False;
    end;
  end;  { NormalisePosition }

begin
  Result := DBIERR_NOSUCHINDEX;
  RecNo := 1;

  if (IndexNo >= 0) then begin
    Normalised := NormalisePosition;

    // Preserve Position in DataSet if possible
    if not Normalised then begin
      RecNo := ActiveIndex.RecNo[Position];
    end;

    FPriorIndex := FSelectedIndex;
    FSelectedIndex := IndexNo;

    // Restore Position in Dataset if possible
    if not Normalised then begin
      Position := ActiveIndex.SeqNo[RecNo];
    end;

    Result := DBIERR_NONE;
  end;  { if }
end;  { SelectIndex }


// _____________________________________________________________________________
{**
  Jvr - 18/04/2002 15:17:05.<P>
}
function TDBICursor.InternalSetLock(
  const Position: Integer;
  const Lock: Boolean
  ): DBIResult;
const
  LockModeMap: array[Boolean] of TDBILockMode = (lkImplicit, lkExplicit);

var
  LockData: TDBILockData;

begin
  Result := DBIERR_NONE;

  LockData.iResource := Position + 1;
  LockData.pReference := Self;
  LockData.iMode := LockModeMap[FFilterLock];
  LockData.iType := ltRecordLock;
  LockData.bLocked := Lock;

  // If a filter lock has been applied and we are attempting to unlock a record
  if FFilterLock and not Lock then begin
    // Don't Implicitly unlock records when a filter lock has been applied
  end
  else begin
    Result := FDSBase.SetDataProp(basepropRESOURCELOCK, PDBIPropData(@LockData));
  end;
end;  { SetLock }


// _____________________________________________________________________________
{**
  Jvr - 05/06/2002 20:42:42.<P>
}
function TDBICursor.SetFilterLock(LockData: TDBILockData): DBIResult;
const
  Caller = 'SetFilterLock';

  function LockRecords: DBIResult;
  var
    SeqNo: Integer;

  begin
    Result := DBIERR_NONE;
    for SeqNo := 1 to ActiveIndex.Count do begin
      // Get the Record Number from the Active-index
      LockData.iResource := ActiveIndex.RecNo[SeqNo];

      // Attempt to lock the record
      Result := FDSBase.SetDataProp(basepropRESOURCELOCK, PDBIPropData(@LockData));
      if (Result <> DBIERR_NONE) then begin
        Break;
      end;
    end;  { for }


    // Something went WRONG, reset all set locks (Preserve Result)
    if (Result <> DBIERR_NONE) then begin
      // Set the bLocked parameter to reset the set locks
      LockData.bLocked := False;

      while (SeqNo > 0) do begin
        // Set the Record Number from the Active-index
        LockData.iResource := ActiveIndex.RecNo[SeqNo];

        // Attempt to unlock the record
        FDSBase.SetDataProp(basepropRESOURCELOCK, PDBIPropData(@LockData));

        // Next record
        Dec(SeqNo);
      end;  { while }
    end;  { if }
  end;  { LockRecords }


  function UnlockRecords: DBIResult;
  var
    SeqNo: Integer;

  begin
    Result := DBIERR_NONE;
    for SeqNo := ActiveIndex.Count downto 1 do begin
      // Get the Record Number from the Active-index
      LockData.iResource := ActiveIndex.RecNo[SeqNo];

      // Attempt to lock the record (Preserve the 1st Result if error occurs)
      if (Result <> DBIERR_NONE) then begin
        FDSBase.SetDataProp(basepropRESOURCELOCK, PDBIPropData(@LockData));
      end
      else begin
        Result := FDSBase.SetDataProp(basepropRESOURCELOCK, PDBIPropData(@LockData));
      end;
    end;  { for }
  end;  { UnlockRecords }

begin
  Result := DBIERR_NONE;

  // Is this a Filter lock and is the lock valid
  Assert(LockData.iType = ltFilterLock);
  Assert(Boolean(LockData.bLocked) in [False..True]);

  if (ActiveIndex.Filter = nil) then begin
    Error(nil, Caller, '4355',
      'No Filter active, ltFilterLock can not be applied', []);
  end;

  if LockData.bLocked and (LockData.iResource <= 0) then begin
    Error(nil, Caller, '4365',
      'The specified maximum filter lock count of %d is illegal, ' +
      'ltFilterLock can not be applied',
      [LockData.iResource]
      );
  end;

  if LockData.bLocked and (ActiveIndex.Count > LockData.iResource) then begin
    Error(nil, Caller, '4375',
      'Filtered RecordCount = %d, ' +
      'This exceeds the specified maximum filter lock count of %d, ' +
      'ltFilterLock can not be applied',
      [ActiveIndex.Count, LockData.iResource]
      );
  end;

  if LockData.bLocked and FLocks.FullLocked then begin
    Error(nil, Caller, '4380',
      'A fulllock has been applied to this dataset, ' +
      'ltFilterLock can not be applied when other locks are pending',
      []
      );
  end;

  if LockData.bLocked and (FLocks.Count > 0) then begin
    Error(nil, Caller, '4385',
      '"%d" locks pending, ' +
      'ltFilterLock can not be applied when other locks are pending',
      [FLocks.Count]
      );
  end;

  if LockData.bLocked and FFilterLock then begin
    Error(nil, Caller, '4390',
      'Filterlock already applied, ' +
      'ltFilterLock can not be applied when other locks are pending',
      []
      );
  end;

  if not (LockData.bLocked or FFilterLock) then begin
    Error(nil, Caller, '4395',
      'No Filterlock pending, ' +
      'ltFilterLock can not be removed when it has not been applied',
      []
      );
  end;

{##JVR
  if LockData.bLocked and (ActiveIndex.Count <= 0) then begin
    Error(nil, Caller, '4370',
      '"%d" Records in Filtered Dataset, ltFilterLock can not be applied',
      [ActiveIndex.Count]
      );
  end;
}

  { DONE -oJvr -cSetFilterLock :
    We should be able to put the dataset in filter lock mode
    even if there are no records to start off with that meet the filter
  }
  if (ActiveIndex.Count > 0) then begin
    // Setup the locking parameters
    LockData.pReference := Self;
    LockData.iMode := lkExplicit;
    LockData.iType := ltRecordLock;

    // Lock/Unlock All records in Filter
    if (LockData.bLocked) then begin
      Result := LockRecords;
    end
    else begin
      Result := UnlockRecords
    end;  { if }
  end;  { if }

  FFilterLock := LockData.bLocked;
end;  { SetFilterLock }


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 14:24:44<P>
}
function TDBICursor.GetProp(eProp: TDBICursorProperty; piPropValue: PDBIPropValue): DBIResult;
const
  Caller = 'GetProp';

begin
  Result := DBIERR_NONE;

  case eProp of
    cursorpropRECORDPOSITION: begin
      PInteger(piPropValue)^ := Position;
    end;  { cursorpropRECORDPOSITION }

    cursorpropSELECTED_INDEX: begin
      PInteger(piPropValue)^ := FSelectedIndex;
    end;  { cursorpropSELECTED_INDEX }

    cursorpropRECINFO_OFFSET: begin
      PInteger(piPropValue)^ := FRecInfoOffset;
    end;  { cursorpropRECINFO_OFFSET }

    else begin
      Error(nil,
        Caller, '5710', 'Getting property of type "%s" not supported!',
        [GetEnumName(TypeInfo(TDBICursorProperty), Ord(eProp))]
        );
    end;  { Default }
  end  { case };
end;  { GetProp }


function TDBICursor.GetDataProp(eProp: TDBICursorProperty; pPropData: PDBIPropData): DBIResult;
const
  Caller = 'GetDataProp';

var
  PLockData: PDBILockData;

begin
  Result := DBIERR_NONE;

  case eProp of
    cursorpropDSBASE: begin
      IDBIBase(pPropData^) := FDSBase;
    end;  { cursorpropDSBASE }

    cursorpropRESOURCELOCK: begin
      PLockData := pPropData;
      if (PLockData.iType = ltFilterLock) then begin
        PLockData.bLocked := FFilterLock;
      end
      else begin
        PLockData.iMode := lkExplicit;
        PLockData.pReference := Self;
        Result := FDSBase.GetDataProp(basePropRESOURCELOCK, PLockData);
      end;
    end;  { cursorpropRESOURCELOCK }

    else begin
      Error(nil, Caller, '5745',
        'Getting property DATA of type "%s" not supported!',
        [GetEnumName(TypeInfo(TDBICursorProperty), Ord(eProp))]
        );
    end;  { Default }
  end  { case };
end;  { GetProp }


// _____________________________________________________________________________
{**
  Jvr - 05/06/2002 17:56:55.<P>
}
function TDBICursor.SetProp(eProp: TDBICursorProperty; iPropValue: TDBIPropValue): DBIResult;
const
  Caller = 'SetProp';

begin
  Result := DBIERR_NONE;

  case eProp of
    cursorpropRECINFO_OFFSET: begin
      FRecInfoOffset := iPropValue;
    end;  { cursorpropRECINFO_OFFSET }

  else
    Error(nil,
      Caller, '5775', 'Setting property of type "%s" not supported!',
      [GetEnumName(TypeInfo(TDBICursorProperty), Ord(eProp))]
      );
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/03/2013 14:13:12.<P>
}
function TDBICursor.SetDataProp(eProp: TDBICursorProperty; pPropData: PDBIPropData): DBIResult;
const
  Caller = 'SetDataProp';

var
  PLockData: PDBILockData;

begin
  Result := DBIERR_NONE;

  case eProp of
    cursorpropRESOURCELOCK: begin
      PLockData := pPropData;
      PLockData.iMode := lkExplicit;
      PLockData.pReference := Self;

      if (PLockData.iType = ltFilterLock) then begin
        Result := SetFilterLock(PLockData^);
      end
      else begin
        Result := FDSBase.SetDataProp(basePropRESOURCELOCK, PDBIPropData(pPropData));
      end;
    end;

  else
    Error(nil,
      Caller, '5805', 'Setting property of type "%s" not supported!',
      [GetEnumName(TypeInfo(TDBICursorProperty), Ord(eProp))]
      );
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/11/2000 14:34:05<P>
}
procedure TDBICursor.Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
var
  Address: Pointer;

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

  DebugInfo := 'DBIInterfaces::' + Self.ClassName + '::' + Caller + '::' + Reference + #13;
{$else}
  DebugInfo := '';
{$endif}

  raise EDBIException.CreateFmt(DebugInfo + ErrMsg + ErrorInfo, Args) at Address;
end;  { Error }


// _____________________________________________________________________________
{**
  Jvr - 30/05/2002 16:53:56.<BR>
  Jvr - 19/06/2002 17:27:44 - RecCount needs to be initialised because
                              Delphi/Pascal don't, and the passed in value
                              to GetRecordCount is checked before returning
                              a result in the same parameter<BR>
  Jvr - 19/06/2002 18:17:19 - Check EOF now refreshes the RecCount to make
                              sure that we are really at the EOF.
                              Some else may have entered some data.<P>
}
function TDBICursor.CheckPosition(const Direction: Integer): DBIResult;
var
  RecCount: Integer;

begin
  Result := DBIERR_NONE;
  RecCount := 0;
  GetRecordCount(RecCount);

  { Current/Next - Check EOF }
  if (Position > RecCount) and (Direction >= 0) then begin
    // Make sure that this really is the EOF
    RecCount := dsRecRefresh;
    GetRecordCount(RecCount);
    
    if (Position > RecCount) then begin
      Position := RecCount + 1;
      Result := DBIERR_EOF;
    end;
  end

  { Current/Prior - Check BOF }
  else if (Position < 1) and (Direction <= 0) then begin
    Position := 0;
    Result := DBIERR_BOF;
  end;  { if }
end;  { CheckPosition }


// _____________________________________________________________________________
{**
  Jvr - 14/05/2002 13:29:45.<P>
}
function TDBICursor.GetActiveIndex: TDBIndex;
begin
  Result := FIndices[FSelectedIndex];
end;  { GetActiveIndex }


// _____________________________________________________________________________
{**
  Jvr - 03/11/2000 13:40:00 - This include for debugging purposes<P>
}
procedure TDBICursor.SetPosition(const Value: Integer);
begin
  FPosition := Value;
end;  { SetPosition }





{ TDBICustomObject }

// _____________________________________________________________________________
{**
  Jvr - 16/04/2002 17:06:17.<P>
}
procedure TDBICustomObject.Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
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





{ TDBIFieldDef }

{$ifdef fpc }
function TDBIFieldDef.GetChildDefs: TFieldDefs;
begin
  raise Exception.Create('TDBIFieldDef.GetChildDefs() Not implemented!');
end;

function TDBIFieldDef.HasChildDefs: Boolean;
begin
  Result := False; //##JVR (FChildDefs <> nil) and (FChildDefs.Count > 0);
end;

procedure TDBIFieldDef.SetChildDefs(Value: TFieldDefs);
begin
  raise Exception.Create('TDBIFieldDef.SetChildDefs() Not implemented!');
end;
{$endif}


end.


