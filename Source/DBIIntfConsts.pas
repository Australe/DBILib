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
  1.0 | 03/11/2005 12:13:46 | Jvr | Initial Release
  1.1 | 01/12/2012 13:52:43 | Jvr | Unicode conversion
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIIntfConsts;

{$I DBICompilers.inc}

{$MINENUMSIZE 4}

interface

uses
  Classes, {$ifndef fpc} DBCommon, {$endif} {$ifdef DELPHI6} FmtBcd, {$endif} DB;

const

{ Record attributes }

  dsRecUnmodified    = $0000;       { Unmodified record }
  dsRecOrg           = $0001;       { Original record (was changed) }
  dsRecDeleted       = $0002;       { Record was deleted }
  dsRecNew           = $0004;       { Record was inserted }
  dsRecModified      = $0008;       { Record was changed }
  dsUnused           = $0020;       { Record not used anymore (hole) }
  dsDetUpd           = $0040;       { Detail modification  Ins/Del/Mod. }
                                    { Can be combined with other status. }
  dsIsNotVisible     = dsRecDeleted or dsRecOrg or dsUnused;
  dsIsVisible        = not (dsRecDeleted or dsRecOrg or dsUnused);

const
  // Extension to Record attributes in DSIntf.pas
  // NOTE: Although the constants are defined as WORDs
  //       they are actually stored as BYTEs
  //
  // At this stage (Delphi 6) $0010 is NOT begin used by ClientDataset
  // so I'm using it to refer to all active records
  // If Borland should change their mind and use the above value then
  // dsActive will have to be redefined

{ Record attributes }
(*
  dsRecUnmodified    = $0000;       { Unmodified record }
  dsRecOrg           = $0001;       { Original record (was changed) }
  dsRecDeleted       = $0002;       { Record was deleted }
  dsRecNew           = $0004;       { Record was inserted }
  dsRecModified      = $0008;       { Record was changed }
  dsUnused           = $0020;       { Record not used anymore (hole) }
  dsDetUpd           = $0040;       { Detail modification  Ins/Del/Mod. }
                                    { Can be combined with other status. }
  dsIsNotVisible     = dsRecDeleted or dsRecOrg or dsUnused;
  dsIsVisible        = not (dsRecDeleted or dsRecOrg or dsUnused);
*)
  dsRecAllMask       = $007F;       { All physical records mask }
  dsRecActive        = $0010;       { All active records - Mainly for XBase }
  dsRecAll           = dsRecDeleted or dsRecActive;
  dsRecRefresh       = $FF;         { Special Flag used in MoveToEOF ??? }

  // I hope borland has nothing planned for this value ???
  dsRecLockSet       = $0080;       { Set RecordLock request,   implicit locking }
  dsRecLockReset     = dsRecLockSet or dsRecOrg;
  { Reusing dsRecOrg for now }      { Reset RecordLock request, implicit locking }
  dsRecLockClear     = $007E;       { Clear RecordLock request, implicit locking }



{ Field attributes }

  fldAttrHIDDEN      = $0001;       { Field is hidden }
  fldAttrREADONLY    = $0002;       { Field is readonly }
  fldAttrREQUIRED    = $0004;       { Field value required }
  fldAttrLINK        = $0008;       { Linking field }

  BLANK_NULL         = 1;           { 'real' NULL }
  BLANK_NOTCHANGED   = 2;           { Not changed , compared to original value }

  MAXKEYFIELDS       = 16;

{ Master Detail Semantics DSBase.SetProp(dspropMD_SEMANTICS, Value) }

  mdCASCADEDEL       = $0004;
  mdCASCADEMOD       = $0008;  { Allow master link field to be changed (cascade change to details) }
  mdALLOWLINKCHANGE  = $0010;  { Allow detail linkfields to be changed (fly-away) }

type
{$ifndef DELPHI6}
  PByte = ^Byte;
  PWord = ^Word;
  PBoolean = ^Boolean;
  PInteger = ^Integer;
  PLongWord = ^LongWord;
  PPointer = ^Pointer;
{$endif}
  PLongBool = ^LongBool;

  DBIResult = Word;                 { Function result }

  DBSearchCond = (                  { Search condition for keys }
    keySEARCHEQ,                    {   =   }
    keySEARCHGT,                    {   >   }
    keySEARCHGEQ                    {   >=  }
  );

  MIDASNAME            = packed array [0..31] of AnsiChar; { holds a name }
  MIDASPATH            = packed array [0..260] of AnsiChar; { holds a DOS path }

  
  { Generic Types - Used to port Delphi code to XE3 }

  TDBIFieldName = String;
  TDBINumberString = String;
  TDBIPropName = String;
  PDBIPropData = Pointer;

  PDBIPropValue = Pointer;
  TDBIPropValue = LongWord;         { JVR - Unsigned Data Pointer for stored properties }

  TDBIClientData = LongWord;        { JVR - Unsigned Data Pointer passed in callback routines }

  TDBIAttribute = PAnsiChar;
  TDBIFieldData = PAnsiChar;
  TDBILinkName = PAnsiChar;
  TDBIIndexName = PChar;
  TDBIMsgBuffer = PAnsiChar;
  TDBINameBuffer = PAnsiChar;
  TDBIParamBuffer = PAnsiChar;
  TDBIStringBuffer = PAnsiChar;

  TDBIMethodName = ShortString;

{$ifdef DELPHI2009}
  TDBIRecBuf = PByte;
{$else}
  TDBIRecBuf = PAnsiChar;
{$endif}

{ Native Types }

  TDBIDataPacket = TStream;

  pDSAttr = ^DSAttr;
  DSAttr = type Byte;

  phDSFilter = ^hDSFilter;
  hDSFilter = type Pointer;

  GROUPSTATE = (
    grSTATEMIDDLE,                  { Record is neither the first or the last in the group }
    grSTATEFIRST,                   { Record is the first in the group }
    grSTATELAST,                    { Record is the last in the group }
    grSTATEFIRSTLAST
  );

  phDSAggregate = ^hDSAggregate;
  hDSAggregate = type Integer;

  AGGSTATE = (
    aggSTATEUNDEF,                  { State is undefined. Needs recalc. }
    aggSTATEOK,
    aggSTATEBLANK,                  { This is a blank value }
    aggSTATEERROR                   { An error occurred previously }
  );

  AGGVALUE = packed record
    iCnt      : Integer;            { Count of records in each }
    eAggState : AGGSTATE;           { State of value }
    Value     : record end;         { First byte of value }
  end;

  pDSBOOKMRK = ^DSBOOKMRK;
  DSBOOKMRK = packed record
    iPos   : Integer;               { Position in given order }
    iState : Integer;               { State of cursor }
    iRecNo : Integer;               { Record number }
    iSeqNo : Integer;               { Version number of order }
    iOrderID : Integer;             { Defines Order }
  end;

  pSAVEPOINT = ^SAVEPOINT;
  SAVEPOINT = type Integer;

  DSKEY = packed array[0..MAXKEYFIELDS-1] of Integer;
  DSKEYBOOL = packed array[0..MAXKEYFIELDS-1] of LongBool;

{ Dataset and Cursor Properties }

  DSProp = (
    dspropLOGCHANGES,               { rw LongBool,   Log changes for undo/delta }
    dspropREADONLY,                 { rw LongBool,   Disable dataset updates }
    dspropNOOFCHANGES,              { r  UINT32, Number of changes }
    dspropCONSTRAINTS_DISABLED,     { rw LongBool,   Disable constraints }
    dspropDSISPARTIAL,              { rw LongBool,   Dataset is parital }
    dspropRECORDSINDS,              { r  UINT32, Records in dataset }
    dspropAUTOINC_DISABLED,         { rw LongBool,   Autoinc disabled }
    dspropISDELTA,                  { r  LongBool,   Dataset is a delta }
    dspropDONTINCLMETADATA,         { rw LongBool,   Exclude metadata in StreamDS }
    dspropINCLBLOBSINDELTA,         { rw UINT32, include blobs with lengths <= than }
                                    { this value in delta for 'original' record }
    dspropGETSAVEPOINT,             { r  SAVEPOINT, return savepoint for current update state. }
    dspropCOMPRESSARRAYS,           { rw LongBool(TRUE), if true expands fielddescriptors for arrays }
    dspropMD_SEMANTICS,             { rw UINT32(0), Master/detail semantics }
    dspropFIELD_FULLNAME,           { r  in: UINT32 (FieldID), out: zstring (full name) }
    dspropFIELDID_FORNAME,          { r  in: zstring(full name), out:UINT32 (fieldid) }
    dspropFIELDID_FORPARENT,        { r  in: UINT32 (FieldID), out: UINT32 (FieldID) }
    dspropCHANGEINDEX_VIEW,         { rw DSAttr (UINT32) (update attributes), any combination, 0->show all }
    dspropGETUNIQUEINDEX,           { r  DSIDX, internal use, returns first unique index, if any }
    dspropREMOTE_UPDATEMODE,        { rw UINT32, 0: where key, 1: where all, 3: where ch }
    dspropXML_STREAMMODE,
    dspropDISABLEDELETES,           { unused }
    dspropDISABLEINSERTS,           { unused }
    dspropDISABLEEDITS,             { unused }
    dspropDISABLESTRINGTRIM,        { rw LongBool: disable automatic trimming of strings }
    dspropDATAHASCHANGED,
    dspropUTF8METADATA,             { rw BOOL, 0:META data is ANSI encoding. (default) 1:UTF8 encoding. }
    dspropUTF8ERRORMSG,             { rw BOOL, 0:Error message is ANSI encoding. (default) 1:UTF8 encoding. }
    dspropANSICODEPAGE              { rw INT32, Internal ANSI codepage. }
  );

  CURProp = (
    curpropCONSTRAINT_ERROR_MESSAGE,{ r pCHAR,   Constraint Error Message }
    curpropDSBASE,                  { r pDSBASE, Underlying DSBASE) }
    curpropSETCRACK,                { w Sets crack-value to supplied value (DBERR_NOCURRREC) }
    curpropGETORG_RECBUF            { r returns recordbuffer for original record, error if none }
  );

  pDSProps = ^DSProps;
  DSProps = packed record
    szName           : MIDASPATH;    { Name, if any }
    iFields          : Integer;      { Number of columns }
    iRecBufSize      : Integer;      { Size of record buffer }
    iBookMarkSize    : Integer;      { Size of bookmark }
    bReadOnly        : LongBool;     { Dataset is not updateable }
    iIndexes         : Integer;      { Number of indexes on dataset }
    iOptParams       : Integer;      { Number of optional parameters }
    bDelta           : LongBool;     { This is a delta dataset }
    iLCID            : Integer;      { Language used }
    iUnused          : packed array[0..7] of Integer; { Reserved }
  end;

{ Field Descriptor }

  pDSFLDDesc = ^DSFLDDesc;
  DSFLDDesc = packed record
    szName          : MIDASNAME;    { Field name }
    iFldType        : Integer;      { Field type }
    iFldSubType     : Integer;      { Field subtype (if applicable) }
    iUnits1         : Integer;      { Number of Chars, precision etc }
    iUnits2         : Integer;      { Decimal places etc. }
    iFldLen         : Integer;      { Length in bytes (computed) }
    iFldOffsInRec   : Integer;      { Offset to field  in record buffer }
    iNullOffsInRec  : Integer;      { Offset to null flag (1byte) in record buffer }
    iFieldID        : Word;         { FieldID of this field }
    iFieldIDParent  : Word;         { FieldID of parent, if any (part of ADT or ARRAY) }
    bCalculated     : LongBool;     { Field is Calculated }
    iFldAttr        : Integer;      { Field attributes }
    iOptParameters  : Integer;      { Number of optional parameters for field }
  end;

{  Index descriptor }

  pDSIDXDesc = ^DSIDXDesc;
  DSIDXDesc = packed record
    szName    : MIDASNAME;          { IndexName }
    iFields   : Integer;            { Number of fields in order (0 -> base order) }
    iKeyFields: DSKEY;              { FieldNumbers }
    iKeyLen   : Integer;            { Total length of key (computed) }
    bUnique   : LongBool;
    bDescending  : DSKEYBOOL;       { TRUE ->Descending }
    bCaseInsensitive : DSKEYBOOL;
  end;

{ Callbacks }

  pfCHANGECallBack = procedure(     { Change Notification callback }
    iClientData  : TDBIClientData   { Client data }
  ); stdcall;

  pfDSFilter = function(            { Filter callback }
    iClientData  : TDBIClientData;  { Client data }
    pRecBuf      : Pointer          { Record buffer }
  ): LongBool; stdcall;

  pfDSCalcField = function(         { Calculated field callback }
    iClientData  : TDBIClientData;  { Client data }
    pRecBuf      : Pointer          { Current record-buffer }
  ): DBIResult; stdcall;

  dsCBRType = Integer;              { Return value for reconcile callback }
  pdsCBRType = ^dsCBRType;

  pfDSReconcile = function(          { Reconcile callback }
    iClientData   : TDBIClientData;  { Client data }
    iRslt         : Integer;         { Result of previous callback }
    iAction       : DSAttr;          { Update request Insert/Modify/Delete }
    iResponse     : dsCBRType;       { Resolver response }
    iErrCode      : Integer;         { Native error-code }
    pErrMessage   : TDBIMsgBuffer;   { Native errormessage if any }
    pErrContext   : TDBIMsgBuffer;   { 1-level error context, if any }
    pRecUpd       : Pointer;         { Record that failed update }
    pRecOrg       : Pointer;         { Original record, if any }
    pRecConflict  : Pointer          { Conflicting record, if any }
  ): dsCBRType; stdcall;

  pfDSReconcile_MD = function(
    iClientData   : TDBIClientData;
    iRslt         : Integer;         { Result of previous callback. If set, the previuos parameters are repeated. }
    iAction       : DSAttr;          { Update request Insert/Modify/Delete }
    iErrResponse  : dsCBRType;       { Resolver response }
    iErrCode      : Integer;         { Native error-code, (BDE or ..) }
    pErrMessage   : TDBIMsgBuffer;   { Native errormessage, if any (otherwise NULL) }
    pErrContext   : TDBIMsgBuffer;   { 1-level error context, if any (otherwise NULL) }
    pRecUpd       : PByte;           { Record that failed update }
    pRecOrg       : PByte;           { Original record, if any }
    pRecConflict  : PByte;           { Conflicting error, if any }
    iLevels       : Integer;         { Number of levels to error0level }
    piFieldIDs    : PInteger         { Array of fieldIDS to navigate to error-dataset }
): dsCBRType;

  pfFLDComp = function(             { Field compare callback }
    iClientData  : TDBIClientData;  { Client callback data }
    pVal1        : Pointer;         { Fieldvalue 1 (NULL if blank) }
    pVal2        : Pointer          { Fieldvalue 2 (NULL if blank) }
  ): Integer;                       { returns -1 if pVal1 < pVal2, }
                                    { 0 if equal, +1 if pVal1 > pVal2 }

{ Resolver & Reconcile callback return values }

const
  dscbrSKIP          = 1;   { Skip this operation (resolver : report error) }
  dscbrABORT         = 2;   { Abort the callback session (reconcile or resolve) }
                            { (resolver : undo all changes). }
  dscbrMERGE         = 3;   { Merge the changes  (resolver : 'simple' merge) }
                            { (reconcile : update original. Keep change). }
  { Resolving only }
  dscbrAPPLY         = 4;   { Overwrite the current record with new values. }
  dscbrIGNORE        = 5;   { Ignore the update request. Don't report error. }

  { Reconcile only }
  dscbrCORRECT       = 4;   { Overwrite change with new values. }
  dscbrCANCEL        = 5;   { Cancel change (remove from delta). }
  dscbrREFRESH       = 6;   { Update original record. Cancel change. }

{ Defines for SetXmlMode/GetXmlMode }

  xmlUNTYPED         =  1;  { Forces XML Data }
  xmlXMLDATATYPED    =  2;  { Not used }
  xmlXMLSCHEMA       =  4;  { Get XML Meta Data }
  xmlXMLUTF8         =  8;  { Encode data using UTF8 }

  xmlON              = xmlXMLSCHEMA or xmlUNTYPED;
  xmlUTF8            = xmlON or xmlXMLUTF8;
  xmlOFF             =  0;


{ Packet Creation }

type
  TPcktAttrArea = (fldAttrArea, pcktAttrArea);
  TPcktFldStatus = (fldIsChanged, fldIsNull, fldIsUnChanged);

  PDSDataPacketFldDesc = ^TDSDataPacketFldDesc;
  TDSDataPacketFldDesc = packed record
    szFieldName: MIDASNAME;         { Column Name }
    iFieldType: Integer;            { Column Type }
    iAttributes: Word;              { Column attributes }
  end;


const
  // This is an extension to the constants defined in DSIntf
  szDEFAULT_FILTER   = 'DEFAULT_FILTER'; { Index used for the default ordering }
                                         { of the FILTERED dataset }

const
{ Do not localize }
  szUNIQUE_KEY       = 'UNIQUE_KEY';  { Series of unique keys to enforce on the client }
  szPRIMARY_KEY      = 'PRIMARY_KEY'; { Primary key used in RowRequest and for key information }
  szDEFAULT_ORDER    = 'DEFAULT_ORDER'; { Index used for the default ordering of the dataset }
  szCHANGEINDEX      = 'CHANGEINDEX';
  szCHANGE_LOG       = 'CHANGE_LOG';
  szSERVER_COL       = 'SERVER_COL';
  szCONSTRAINTS      = 'CONSTRAINTS';
  szDATASET_CONTEXT  = 'DATASET_CONTEXT';
  szDATASET_DELTA    = 'DATASET_DELTA';
  szREADONLY         = 'READONLY'; { Specifies the packet is read only }
  szSUBTYPE          = 'SUBTYPE'; { Field Subtype }
  szDECIMALS         = 'DECIMALS'; { Field decimal precision }
  szWIDTH            = 'WIDTH'; { Field width }
  szFIELDNAME        = 'FIELDNAME'; { Field name when length > 32 }
  szLCID             = 'LCID'; { Locale ID that the packet comes from }
  szBDEDOMX          = 'BDEDOMAIN_X'; { Server side field constraints }
  szBDERECX          = 'BDERECORD_X'; { Server side record constraints }
  szBDEDEFX          = 'BDEDEFAULT_X'; { Server side default values }
  szAUTOINCVALUE     = 'AUTOINCVALUE';
  szELEMENTS         = 'ELEMENTS';
  szTABLE_NAME       = 'TABLE_NAME'; { Table name used for resolving the packet - deprecated}
  szMD_FIELDLINKS    = 'MD_FIELDLINKS'; { Master detail field relationships }
  szTYPENAME         = 'TYPENAME'; { Field type name.  Used for object fields }
  szUPDATEMODE       = 'UPDATEMODE'; { Update mode }
  szFIELDPROPS       = 'FIELDPROPS'; { Delphi transferable field properties }
  szPROVFLAGS        = 'PROVFLAGS'; { Provider flags }
  szORIGIN           = 'ORIGIN'; { Field origin }
  szMD_SEMANTICS     = 'MD_SEMANTICS'; { Master detail semantic properties }
  szSERVERCALC       = 'SERVER_CALC'; { A server side calculated field }
  szBDEDOMCL         = 'BDEDOMAIN_CL'; { Client side field constraints }
  szBDERECCL         = 'BDERECORD_CL'; { Client side record constraints }
  szBDEDEFCL         = 'BDEDEFAULT_CL'; { Client side default values }
  szDISABLE_INSERTS  = 'DISABLE_INSERTS'; { Disable inserting records }
  szDISABLE_DELETES  = 'DISABLE_DELETES'; { Disable deleting records }
  szDISABLE_EDITS    = 'DISABLE_EDITS'; { Disable editing records }
  szNO_RESET_CALL    = 'NO_RESET_CALL'; { Specifies not to call reset when the client closes the data }
  szMINVALUE         = 'MINVALUE'; { Minimum value for the field }
  szMAXVALUE         = 'MAXVALUE'; { Maximum value for the field }

{$ifdef _UNUSED}
  szstMEMO           = 'Text';
  szstWideMEMO       = 'WideText';
  szstBINARY         = 'Binary';
  szstFMTMEMO        = 'Formatted';
  szstOLEOBJ         = 'Ole';
  szstGRAPHIC        = 'Graphics';
  szstDBSOLEOBJ      = 'dBASEOle';
  szstTYPEDBINARY    = 'TypedBinary';
  szstMONEY          = 'Money';
  szstAUTOINC        = 'Autoinc';
  szstADTNESTEDTABLE = 'ADTNestedTable';
  szstFIXEDCHAR      = 'FixedChar';
  szstREFNESTEDTABLE = 'Reference';
  szstGUID           = 'Guid';
  szstACCOLEOBJ      = 'AccessOle';
  szstHMEMO          = 'HMemo';
  szstHBINARY        = 'HBinary';

  dsfldUNKNOWN       = 0;           { Unknown }
  dsfldINT           = 1;           { signed integer }
  dsfldUINT          = 2;           { Unsigned integer }
  dsfldBOOL          = 3;           { Boolean }
  dsfldFLOATIEEE     = 4;           { IEEE float }
  dsfldBCD           = 5;           { BCD }
  dsfldDATE          = 6;           { Date     (32 bit) }
  dsfldTIME          = 7;           { Time     (32 bit) }
  dsfldTIMESTAMP     = 8;           { Time-stamp  (64 bit) }
  dsfldZSTRING       = 9;           { Multi-byte string }
  dsfldUNICODE       = 10;          { unicode string }
  dsfldBYTES         = 11;          { bytes }
  dsfldADT           = 12;          { ADT (Abstract Data Type) }
  dsfldARRAY         = 13;          { Array type (not attribute) }
  dsfldEMBEDDEDTBL   = 14;          { Embedded (nested table type) }
  dsfldREF           = 15;          { Reference }
  dsfldDATETIME      = 17;          { Datetime struct for DB Express }
  dsfldFMTBCD        = 18;          { BCD Variant type }
  dsfldSINGLE        = 27;          { 32 bit floating point }
  dsfldDATETIMEOFFSET = 36;         { DatetimeOffset struct for DB Express }

  dsSizeBitsLen      = 16;          { no. bits indicating fld size }
  dsSizeBitsMask     = $0000FFFF;   { mask to retrieve fld size }
  dsTypeBitsMask     = $003F0000;   { mask to retrieve Type info }
  dsVaryingFldType   = $00400000;   { Varying attribute type. }
  dsArrayFldType     = $00800000;   { Array attribute type. }

  dsPseudoFldType    = $01000000;   {Composite. Bits 1..15 gives number of elements }
  dsCompArrayFldType = $02000000;   { Compressed array }
  dsEmbeddedFldType  = $04000000;   { Embedded table }
  dsIncInDelta       = $80000000;   { For optional parameters only:include parameter in delta }

  dskeyCASEINSENSITIVE  = $4000;
  dskeyDESCENDING       = $8000;

  dsDELAYEDBIT       = $80000000;   { Length/number is not present }

  PACKETVERSION_1     = 1;
  PACKETVERSION_2     = 2;
  PACKETVERSION_3     = 3;

  dsCASCADEDELETES   = 1;
  dsCASCADEUPDATES   = 2;
{$endif}

{ Field Types (Logical) - Originally from BDE.PAS }
  fldUNKNOWN         = 0;
  fldZSTRING         = 1;               { Null terminated string }
  fldDATE            = 2;               { Date     (32 bit) }
  fldBLOB            = 3;               { Blob }
  fldBOOL            = 4;               { Boolean  (16 bit) }
  fldINT16           = 5;               { 16 bit signed number }
  fldINT32           = 6;               { 32 bit signed number }
  fldFLOAT           = 7;               { 64 bit floating point }
  fldBCD             = 8;               { BCD }
  fldBYTES           = 9;               { Fixed number of bytes }
  fldTIME            = 10;              { Time        (32 bit) }
  fldTIMESTAMP       = 11;              { Time-stamp  (64 bit) }
  fldUINT16          = 12;              { Unsigned 16 bit integer }
  fldUINT32          = 13;              { Unsigned 32 bit integer }
  fldFLOATIEEE       = 14;              { 80-bit IEEE float }
  fldVARBYTES        = 15;              { Length prefixed var bytes }
  fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
  fldCURSOR          = 17;              { For Oracle Cursor type }
  fldINT64           = 18;              { 64 bit signed number }
  fldUINT64          = 19;              { Unsigned 64 bit integer }
  fldADT             = 20;              { Abstract datatype (structure) }
  fldARRAY           = 21;              { Array field type }
  fldREF             = 22;              { Reference to ADT }
  fldTABLE           = 23;              { Nested table (reference) }
  fldDATETIME        = 24;              { Datetime structure for DBExpress }
  fldFMTBCD          = 25;              { BCD Variant type: required by Midas, same as BCD for DBExpress}
  fldWIDESTRING      = 26;              { UCS2 null terminated string }
  fldSINGLE          = 27;              { 32 bit floating point }
  fldINT8            = 28;              { 8 bit signed number as defined in alctypes.h }
  fldUINT8           = 29;              { Unsigned 8 bit integer (Byte) as defined in alctypes.h }

  MAXLOGFLDTYPES     = 30;              { Number of logical fieldtypes }


{ Additional (non-BDE fieldtypes }
  fldUNICODE         = $1007;           { Unicode }
  fldDATETIMEOFFSET  = 36;              { DatetimeOffset structure for DBExpress }

{$ifdef DELPHI2009}
  fldEXTENDED        = fldFLOATIEEE;
{$else}
  fldEXTENDED        = fldFLOAT;        { Extended is mapped to DOUBLE }
{$endif}

{ Sub Types (Logical) }

{ fldFLOAT subtype }
  fldstMONEY         = 21;              { Money }

{ fldBLOB subtypes }
  fldstNONE = 0;
  fldstMEMO          = 22;              { Text Memo }
  fldstBINARY        = 23;              { Binary data }
  fldstFMTMEMO       = 24;              { Formatted Text }
  fldstOLEOBJ        = 25;              { OLE object (Paradox) }
  fldstGRAPHIC       = 26;              { Graphics object }
  fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
  fldstTYPEDBINARY   = 28;              { Typed Binary data }
  fldstACCOLEOBJ     = 30;              { Access OLE object }
  fldstWIDEMEMO      = 32;              { WideString Memo }
  fldstHMEMO         = 33;              { CLOB }
  fldstHBINARY       = 34;              { BLOB }
  fldstBFILE         = 36;              { BFILE }

{ fldZSTRING/fldWIDESTRING subtype }
  fldstPASSWORD      = 1;               { Password }
  fldstFIXED         = 31;              { CHAR type }
  fldstUNICODE       = 32;              { Unicode }
  fldstGUID          = 38;              { GUID }
  fldstORAINTERVAL   = 40;              { ORACLE INTERVAL }

{ fldDATETIME subtype }
  fldstORATIMESTAMP  = 39;              { ORACLE TIMESTAMP }

{ fldINT32 subtype }
  fldstAUTOINC       = 29;

{ fldTABLE subtype }
  fldstReference     = 70;

const
  // Dataset TFieldType Aliases
  ftSigned16         = ftSmallInt;
  ftSigned32         = ftInteger;
  ftUnsigned16       = ftWord;
  ftAnsiString       = ftString;
  ftUnicodeString    = ftWideString;

{$ifdef Delphi2009}
  ftSigned8          = ftShortInt;
  ftUnsigned8        = ftByte;
  ftUnsigned32       = ftLongWord;
  ftFloat4           = ftSingle;
  ftFloatIEEE        = ftExtended;
  ftDefaultString    = ftWideString;
{$else}
  ftSigned8          = ftSmallInt;
  ftUnsigned8        = ftWord;
  ftUnsigned32       = ftInteger;
  ftFloat4           = ftFloat;
  ftFloatIEEE        = ftFloat;
  ftDefaultString    = ftString;
{$endif}


const
{##DELPHI5
  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING); //34..35
//}

{##DELPHI6
  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING, fldDATETIME, fldFMTBCD); //34..37
//}

{##DELPHI7
  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldZSTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING, fldDATETIME, fldFMTBCD); //34..37
//}

{##DELPHI2006
  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldWIDESTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING, fldDATETIME, fldFMTBCD, //34..37
    fldWIDESTRING, fldBLOB, fldDATETIME, fldZSTRING); // 38..41
//}

{##DELPHI2007
  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldWIDESTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING, fldDATETIME, fldFMTBCD, //34..37
    fldWIDESTRING, fldBLOB, fldDATETIME, fldZSTRING); // 38..41
//}

{##DELPHIXE2
  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldWIDESTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING, fldDATETIME, fldFMTBCD, //34..37
    fldWIDESTRING, fldBLOB, fldDATETIME, fldZSTRING, // 38..41
    fldUINT32, fldINT8, fldUINT8, fldFLOATIEEE, fldUnknown, fldUnknown, fldUnknown, //42..48
    fldDATETIMEOFFSET, fldUNKNOWN, fldSINGLE); // 49..51
//}

{##DELPHIXE3
  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING, fldWIDESTRING, fldINT64, fldADT, // 20..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING, fldDATETIME, fldFMTBCD, //34..37
    fldWIDESTRING, fldBLOB, fldDATETIME, fldZSTRING, // 38..41
    fldUINT32, fldINT8, fldUINT8, fldFLOATIEEE, fldUnknown, fldUnknown, fldUnknown, //42..48
    fldDATETIMEOFFSET, fldUNKNOWN, fldSINGLE); // 49..51
//}

{##LAZARUS
  TFieldType = (
    ftUnknown, ftString, ftSmallint, ftInteger, ftWord, ftBoolean, // 0..5
    ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime, ftBytes, // 6..12
    ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, // 13..19
    ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, ftLargeint, ftADT, // 20..26
    ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface, // 27..33
    ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, //34..37
    ftFixedWideChar, ftWideMemo); // 38..39
//}


  FieldTypeMap: TFieldMap = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL, // 0..5
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES, // 6..12
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB, // 13..19
    fldBLOB, fldBLOB, fldCURSOR, fldZSTRING,  // 20..23
    fldWIDESTRING, fldINT64, fldADT, // 24..26
    fldArray, fldREF, fldTABLE, fldBLOB, fldBLOB, fldUNKNOWN, fldUNKNOWN, // 27..33
    fldUNKNOWN, fldZSTRING {$ifdef DELPHI6} , fldDATETIME, fldFMTBCD {$endif} //36..37
    {$ifdef DELPHI2006} , fldWIDESTRING, fldBLOB {$ifndef fpc}, fldDATETIME, fldZSTRING {$endif} {$endif} // 38..41
    {$ifdef DELPHIXE2} ,
    fldUINT32, fldINT8, fldUINT8, fldEXTENDED, fldUnknown, fldUnknown, fldUnknown, //42..48
    fldDATETIMEOFFSET, fldUNKNOWN, fldSINGLE // 49..51
    {$endif}
    );

  FldSubTypeMap: array[TFieldType] of Word = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstAUTOINC,
    fldstBINARY, fldstMEMO, fldstGRAPHIC, fldstFMTMEMO, fldstOLEOBJ,
    fldstDBSOLEOBJ, fldstTYPEDBINARY, 0, fldstFIXED, {$ifdef DELPHI2006} 0 {$else} fldstUNICODE {$endif} ,
    0, 0, 0, 0, 0, fldstHBINARY, fldstHMEMO, 0, 0, 0, fldstGUID
    {$ifdef DELPHI6} , fldDATETIME, 0 {$endif}
    {$ifdef DELPHI2006} , fldstFIXED, fldstWIDEMEMO {$ifndef fpc}, fldstORATIMESTAMP, fldstORAINTERVAL {$endif} {$endif}
    {$ifdef DELPHIXE2} , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 {$endif}
    );

  DataTypeMap: array[0..MAXLOGFLDTYPES - 1] of TFieldType = (
    ftUnknown, ftString, ftDate, ftBlob, ftBoolean, ftSmallint,
    ftInteger, ftFloat, ftBCD, ftBytes, ftTime, ftDateTime,
    ftWord, ftUnsigned32, ftFloatIEEE, ftVarBytes, ftUnknown, ftUnknown,
    ftLargeInt, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet,
    {$ifdef DELPHI6} ftTimeStamp, ftFMTBcd, {$else} ftDateTime, ftUnknown, {$endif}
    ftWideString, {$ifdef DELPHI2009} ftSingle, ftShortint, ftByte {$else} ftFloat, ftSmallint, ftSmallint {$endif}
    );

  BlobTypeMap: array[fldstMEMO..fldstBFILE] of TFieldType = (
    ftMemo, ftBlob, ftFmtMemo, ftParadoxOle, ftGraphic, ftDBaseOle,
    ftTypedBinary, ftBlob, ftBlob, ftBlob, ftBlob, ftOraClob,
    ftOraBlob, ftBlob, ftBlob);

{ Error Codes }

const
  DBERR_NONE                   = 0;
  DBERR_BOF                    = $2201;
  DBERR_EOF                    = $2202;
  DBERR_NOSUCHINDEX            = $270D;

  ERRCAT_ALC  = $40;
  ERRBASE_ALC = $4000;

  ERRCODE_DELTAISEMPTY       = 1;   { Delta is empty }
  ERRCODE_NOTHINGTOUNDO      = 2;   { Nothing to undo }
  ERRCODE_NOMETADATA         = 3;   { Datapacket contains no meta data }
  ERRCODE_CANNOTAPPEND       = 4;   { Trying to append data to a non-partial }
  ERRCODE_DATAPACKETMISMATCH = 5;   { Mismatch in datapacket }
  ERRCODE_ABORTED            = 6;   { Operation was aborted }
  ERRCODE_CANCELLED          = 7;   { Operation was cancelled }
  ERRCODE_NEWERVERSIONREQ    = 8;   { Newer version required }
  ERRCODE_BLOBNOTFETCHED     = 9;   { Blob has not been fetched }
  ERRCODE_DETAILSNOTFETCHED  = 10;  { Details has not been fetched }
  ERRCODE_NOMASTERRECORD     = 11;  { no corresponding master record found }
  ERRCODE_LINKFIELDSNOTUNIQUE= 12;  { Linkfields must be unique }
  ERRCODE_FLYAWAY_WRONGORDER = 13;  { Special case: wrong order of updates for fly-away }
  ERRCODE_NOCASCADEDUPDATES  = 14;  { Cascaded updates is not enabled }

  DBERR_DELTAISEMPTY        = ERRBASE_ALC + ERRCODE_DELTAISEMPTY;
  DBERR_NOTHINGTOUNDO       = ERRBASE_ALC + ERRCODE_NOTHINGTOUNDO;
  DBERR_NOMETADATA          = ERRBASE_ALC + ERRCODE_NOMETADATA;
  DBERR_CANNOTAPPEND        = ERRBASE_ALC + ERRCODE_CANNOTAPPEND;
  DBERR_DATAPACKETMISMATCH  = ERRBASE_ALC + ERRCODE_DATAPACKETMISMATCH;
  DBERR_ABORTED             = ERRBASE_ALC + ERRCODE_ABORTED;
  DBERR_CANCELLED           = ERRBASE_ALC + ERRCODE_CANCELLED;
  DBERR_NEWERVERSIONREQ     = ERRBASE_ALC + ERRCODE_NEWERVERSIONREQ;
  DBERR_BLOBNOTFETCHED      = ERRBASE_ALC + ERRCODE_BLOBNOTFETCHED;
  DBERR_DETAILSNOTFETCHED   = ERRBASE_ALC + ERRCODE_DETAILSNOTFETCHED;
  DBERR_NOMASTERRECORD      = ERRBASE_ALC + ERRCODE_NOMASTERRECORD;
  DBERR_LINKFIELDSNOTUNIQUE = ERRBASE_ALC + ERRCODE_LINKFIELDSNOTUNIQUE;
  DBERR_FLYAWAY_WRONGORDER  = ERRBASE_ALC + ERRCODE_FLYAWAY_WRONGORDER;
  DBERR_NOCASCADEDUPDATES   = ERRBASE_ALC + ERRCODE_NOCASCADEDUPDATES;


const
  // General Constants
  DBIMAXFLDSINKEY     = 16;                { Max fields in a key }
  STOREDDATEFORMAT    = 'yyyyMMdd';

  Default_LogicalBufferFirstFieldOffset = 0;
  Default_TemporaryRecordBufferSize = 8192;
  Default_StringFieldLength = 255;

  FieldName_NullFlags = '_NullFlags';      { Fieldname of Null Flags }

{$ifndef DELPHI6}
  sLineBreak = {$IFDEF LINUX} #10 {$ELSE} #13#10 {$ENDIF};
{$endif}

  
type
  // Local character buffer declared on stack
  PDBICharacterBuffer = ^TDBICharacterBuffer;
  TDBICharacterBuffer = array[0..dsMaxStringSize] of AnsiChar;

  PZStringList = ^TZStringList;
  TZStringList = array of TDBICharacterBuffer;

  // Field descriptions
  PFieldDescList = ^TFieldDescList;
  TFieldDescList = array of DSFLDDesc;

  // Index descriptions
  PIndexDescList = ^TIndexDescList;
  TIndexDescList = array of DSIDXDesc;

  // Chunk of raw data
  PDBIDataArray = ^TDBIDataArray;
  TDBIDataArray = array of Byte;

  TKeyIndex = (kiLookup, kiRangeStart, kiRangeEnd, kiCurRangeStart,
    kiCurRangeEnd, kiSave);

  PRecInfo = ^TDBIRecInfo;
  TDBIRecInfo = packed record
    RecordNumber: Longint;
    BookmarkFlag: TBookmarkFlag;
    Attribute: DSAttr;
    RecordIdent: Longint;

    // Jvr - 09/12/2004 11:21:49
    // This is to support object references in a TObjectlistDataset
    Data: Pointer;
  end;

  // Jvr - 09/12/2004 11:20:22
  // Added DataInfo structure to allow exchanging data with dataconnections
  PDataInfo = ^TDBIDataInfo;
  TDBIDataInfo = record
    Attribute: DSAttr;
    Data: Pointer;
  end;

  PKeyBuffer = ^TKeyBuffer;
  TKeyBuffer = record
    Modified: Boolean;
    Exclusive: Boolean;
    FieldCount: Integer;
    Data: record end;
  end;

  TBookmarkInfo = LongWord;

type
  // Locking types
  TDBILockType = (ltFullLock, ltRecordLock, ltFilterLock);

  // Locking modes
  TDBILockMode = ({lkNone, }lkImplicit, lkExplicit);
  TDBILockModes = set of TDBILockMode;
const
  lkLockModes = [lkImplicit, lkExplicit];


type
  PDBILockData = ^TDBILockData;
  TDBILockData = record
    iResource: Integer;                // Record Number (1 based)
    pReference: Pointer;               // Self pointer of a TDBICursor
    iMode: TDBILockMode;               // lkImplicit, lkExplicit
    iType: TDBILockType;               // ltFullLock, ltRecordLock, ltFilterLock
    bLocked: LongBool;                 // True, False
  end;

  
const
  // auxillary constants to access data in the DSProps.iUnused Array of DSBase
  auxStreamMode = 0;                { DSProps.iUnused[0] }
  auxDataStream = 1;                { DSProps.iUnused[1] }
  auxMemoStream = 2;                { DSProps.iUnused[2] }
  auxMemoryIndices = 3;             { DSProps.iUnused[3] }
  auxProp4 = 4;                     { DSProps.iUnused[4] }
  auxStatusFilter = 5;              { DSProps.iUnused[5] }
  auxXmlStreamMode = 6;             { DSProps.iUnused[6] }
  auxExclusiveAccess = 7;           { DSProps.iUnused[7] }


type
  TDBIBaseProperty = (
    basepropLOGCHANGES,              { rw BOOL,   Log changes for undo/delta }
    basepropREADONLY,                { rw BOOL,   Disable dataset updates }
    basepropNOOFCHANGES,             { r  UINT32, Number of changes }
    basepropCONSTRAINTS_DISABLED,    { rw BOOL,   Disable constraints }
    basepropDSISPARTIAL,             { rw BOOL,   Dataset is parital }
    basepropRECORDSINDS,             { r  UINT32, Records in dataset }
    basepropAUTOINC_DISABLED,        { rw BOOL,   Autoinc disabled }
    basepropISDELTA,                 { r  BOOL,   Dataset is a delta }
    basepropDONTINCLMETADATA,        { rw BOOL,   Exclude metadata in StreamDS }
    basepropINCLBLOBSINDELTA,        { rw UINT32, include blobs with lengths <= than }
                                       { this value in delta for 'original' record }
    basepropGETSAVEPOINT,            { r  SAVEPOINT, return savepoint for current update state. }
    basepropCOMPRESSARRAYS,          { rw BOOL(TRUE), if true expands fielddescriptors for arrays }
    basepropMD_SEMANTICS,            { rw UINT32(0), Master/detail semantics }
    basepropFIELD_FULLNAME,          { r  in: UINT32 (FieldID), out: zstring (full name) }
    basepropFIELDID_FORNAME,         { r  in: zstring(full name), out:UINT32 (fieldid) }
    basepropFIELDID_FORPARENT,       { r  in: UINT32 (FieldID), out: UINT32 (FieldID) }
    basepropCHANGEINDEX_VIEW,        { rw DSAttr (UINT32) (update attributes), any combination, 0->show all }
    basepropGETUNIQUEINDEX,          { r  DSIDX, internal use, returns first unique index, if any }
    basepropREMOTE_UPDATEMODE,       { rw UINT32, 0: where key, 1: where all, 3: where ch }
    basepropXML_STREAMMODE,

    basepropDATAHASCHANGED,

    // Additional Source Properties
    basepropDATASETCHANGED,          { r  underlying dataset has changed, e.g. assign new list }
    basepropDATACONNECTION,          { r  pDATACONNECTION, >> DBIXBASE.DATACONNECTION }
    basepropINDICES,                 { r  pINDICES,        >> DBIXBASE.INDICES }
    basepropINDEXCOUNT,              { rw UINT32,          >> DBIXBASE.FDSProps.iIndexes }
    basepropLOCKLIST,                { r  pLOCKLIST,       >> DBIXBASE.FLocks }
    basepropEXCLUSIVE_ACCESS,        { rw BOOL, provide Exclusive Access }
    basepropSTREAMMODE,              { rw TDBIStreamMode, method of obtaining the data }
//##JVR    basepropRECORDLOCK               { rw TDBILockData, Lock/Unlock record }
    basepropRESOURCELOCK             { rw TDBILockData, Lock/Unlock dataset/record/filter }
  );

  TDBICursorProperty = (
    cursorpropCONSTRAINT_ERROR_MESSAGE,{ r pCHAR, Constraint Error Message }
    cursorpropDSBASE,                  { r pDSBASE, Underlying DSBASE) }
    cursorpropSETCRACK,                { w Sets crack-value to supplied value (DBERR_NOCURRREC) }
    cursorpropGETORG_RECBUF,           { r returns recordbuffer for original record, error if none }

    // Additional Cursor Properties
    cursorpropSELECTED_INDEX,          { rw UINT, 0=Primary, 1..n User defined Indices }
    cursorpropRECINFO_OFFSET,          {  w UNINT, RecInfo offset in logical buffer }
    cursorpropRECORDPOSITION,          { rw UINT, 1-based }
    cursorpropRESOURCELOCK             { rw BOOL, Lock/Unlock dataset/record/filter }
  );


const
  // Data Field Sizes (Memory in Bytes)
  sizeofUNKNOWN   =  0;                // No storage required
  sizeofZSTRING   = -1;                // Dependant on the set string length
  sizeofDATE      =  4;                // fldDATE
  sizeofBLOB      =  4;                // fldBLOB
  sizeofBOOL      =  2;                // fldBOOL
  sizeofINT16     =  SizeOf(SmallInt); // fldINT16
  sizeofINT32     =  SizeOf(LongInt);  // fldINT32
  sizeofFLOAT     =  SizeOf(Double);   // fldFLOAT
  sizeofBCD       =  SizeOf(TBcd); {18}// fldBCD
  sizeofBYTES     = -1;                // fldBYTES
  sizeofTIME      =  4;                // fldTIME
  sizeofTIMESTAMP =  SizeOf(TDateTime);// fldTIMESTAMP
  sizeofUINT16    =  SizeOf(SmallInt); // fldUINT16
  sizeofUINT32    =  SizeOf(LongInt);  // fldUINT32
  sizeofFLOATIEEE =  SizeOf(Extended); // fldFLOATIEEE
  sizeofVARBYTES  = -1;                // fldVARBYTES
  sizeofLOCKINFO  = -1;                // fldLOCKINFO
  sizeofCURSOR    = -1;                // fldCURSOR
  sizeofINT64     =  SizeOf(Int64);    // fldINT64
  sizeofUINT64    =  SizeOf(Int64);    // fldUINT64
  sizeofADT       =  4;                // fldADT
  sizeofARRAY     = -1;                // fldARRAY
  sizeofREF       = -1;                // fldREF
  sizeofTABLE     =  4;                // fldTABLE
  sizeofDATETIME  =  SizeOf(TDateTime);// fldDATETIME
  sizeofFMTBCD    =  SizeOf(TBcd);     // fldFMTBCD
  sizeofINT8      =  SizeOf(ShortInt); // fldINT8
  sizeofUINT8     =  SizeOf(ShortInt); // fldUINT8
  sizeofSINGLE    =  SizeOf(Single);   // fldSINGLE
  sizeofWORD      =  SizeOf(Word);     // Word
  sizeofLONGWORD  =  SizeOf(LongWord); // DWord
  sizeofCURRENCY  =  SizeOf(Currency); // Currency
  sizeofAUTOINC   =  SizeOf(LongInt);  // Int32
  sizeofMEMO      =  4;                // Pointer
  sizeofDOUBLE    =  SizeOf(Double);   // Double
  sizeofNUMBER    =  8;                // Double
  sizeofNULLFLAGS =  2;                // Int16
  sizeofDELETED   =  1;                // Byte



{ This following section are the field definitions for a dataset to access
  the Field definitions of a specified dataset.

  Ideally we should be able to look at every derived dataset's field-definitions
  in a table. (create & edit as well)
}
const
  FieldNameSize = SizeOf(MIDASNAME);

  FieldPropsFLDDesc: array[0..12] of DSFLDDesc = (
    // 01. szName          : MIDASNAME;      { Field name }
    (
    szName          : 'FieldName';
    iFldType        : fldZSTRING;
    iFldSubType     : fldstNONE;
    iUnits1         : FieldNameSize-1;
    iUnits2         : 0;
    iFldLen         : FieldNameSize;
    iFldOffsInRec   : 0;
    iNullOffsInRec  : 0;
    iFieldID        : 1;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 02. iFldType        : Integer;      { Field type }
    (
    szName          : 'FieldType';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize;
    iNullOffsInRec  : 0;
    iFieldID        : 2;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 03. iFldSubType     : Integer;      { Field subtype (if applicable) }
    (
    szName          : 'FieldSubType';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (1 * sizeofINT32);
    iNullOffsInRec  : 0;
    iFieldID        : 3;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 04. iUnits1         : Integer;      { Number of Chars, precision etc }
    (
    szName          : 'FieldSize';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (2 * sizeofINT32);
    iNullOffsInRec  : 0;
    iFieldID        : 4;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 05. iUnits2         : Integer;      { Decimal places etc. }
    (
    szName          : 'DecimalPlaces';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (3 * sizeofINT32);
    iNullOffsInRec  : 0;
    iFieldID        : 5;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 06. iFldLen         : Integer;      { Length in bytes (computed) }
    (
    szName          : 'FieldLength';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (4 * sizeofINT32);
    iNullOffsInRec  : 0;
    iFieldID        : 6;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 07. iFldOffsInRec   : Integer;      { Offset to field  in record buffer }
    (
    szName          : 'FieldOffset';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (5 * sizeofINT32);
    iNullOffsInRec  : 0;
    iFieldID        : 7;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 08. iNullOffsInRec  : Integer;      { Offset to null flag (1byte) in record buffer }
    (
    szName          : 'NullOffset';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (6 * sizeofINT32);
    iNullOffsInRec  : 0;
    iFieldID        : 8;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 09. iFieldID        : Word;         { FieldID of this field }
    (
    szName          : 'FieldId';
    iFldType        : fldINT16;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT16;
    iFldOffsInRec   : FieldNameSize + (7 * sizeofINT32);
    iNullOffsInRec  : 0;
    iFieldID        : 9;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 10. iFieldIDParent  : Word;         { FieldID of parent, if any (part of ADT or ARRAY) }
    (
    szName          : 'FieldParent';
    iFldType        : fldINT16;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT16;
    iFldOffsInRec   : FieldNameSize + (7 * sizeofINT32) + (1 * sizeofINT16);
    iNullOffsInRec  : 0;
    iFieldID        : 10;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 11. bCalculated     : Bool;         { Field is Calculated }
    (
    szName          : 'Calculated';
    iFldType        : fldBOOL;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofBOOL;
    iFldOffsInRec   : FieldNameSize + (7 * sizeofINT32) + (2 * sizeofINT16);
    iNullOffsInRec  : 0;
    iFieldID        : 11;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 12. iFldAttr        : Integer;      { Field attributes }
    (
    szName          : 'FieldAttribute';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (7 * sizeofINT32) + (2 * sizeofINT16) + sizeofBOOL;
    iNullOffsInRec  : 0;
    iFieldID        : 12;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    ),

    // 13. iOptParameters  : Integer;      { Number of optional parameters for field }
    (
    szName          : 'Parameters';
    iFldType        : fldINT32;
    iFldSubType     : fldstNONE;
    iUnits1         : 0;
    iUnits2         : 0;
    iFldLen         : sizeofINT32;
    iFldOffsInRec   : FieldNameSize + (8 * sizeofINT32) + (2 * sizeofINT16) + sizeofBOOL;
    iNullOffsInRec  : 0;
    iFieldID        : 13;
    iFieldIDParent  : 0;
    bCalculated     : False;
    iFldAttr        : 0;
    iOptParameters  : 0;
    )
  );  { FieldPropsFLDDesc }



resourcestring
  SCannotCreateDataSet = 'No fields defined.  Cannot create dataset';
  SNotBrowsing = 'Cannot perform this operation on a dataset not in browse mode';

// =============================================================================
// MIDAS CONSTANTS (Borrowed)
// =============================================================================
(*
const
{ Do not localize }
  MIDAS_CatDesc = 'Borland MIDAS Application Servers';
  CATID_MIDASAppServer: TGUID = '{13E85B3C-9508-11D2-AB63-00C04FA35CFA}';
  SCatImplBaseKey = 'CLSID\%s\Implemented Categories';
  SCatImplKey = SCatImplBaseKey + '\%s';
  MIDAS_DLL = 'MIDAS.DLL';
  SClsid = 'CLSID\';
  SPooled = 'Pooled';
  SMaxObjects = 'MaxObjects';
  STimeout = 'Timeout';
  SSingleton = 'Singleton';
  SSockets = 'Sockets';
  SWeb = 'Web';
  SFlagOn = '1';
  SFlagOff = '0';
*)
resourcestring
(*##MIDAS
  { App Server }
  SProviderNotExported = 'Provider not exported: %s';

  { DBClient }
  SNoDataProvider = 'Missing data provider or data packet';
  SInvalidDataPacket = 'Invalid data packet';
  SRefreshError = 'Must apply updates before refreshing data';
  SProviderInvalid = 'Invalid provider. Provider was freed by the application server';
  SServerNameBlank = 'Cannot connect, %s must contain a valid ServerName or ServerGUID';
  SRepositoryIdBlank = 'Cannot connect, %s must contain a valid repository id';
  SAggsGroupingLevel = 'Grouping level exceeds current index field count';
//*)
  SAggsNoSuchLevel = 'Grouping level not defined';
(*##MIDAS
  SNoCircularReference = 'Circular provider references not allowed';
  SErrorLoadingMidas = 'Error loading MIDAS.DLL';
  SCannotCreateDataSet = 'No fields defined.  Cannot create dataset';

  { MConnect }
  SSocketReadError = 'Error reading from socket';
  SInvalidProviderName = 'Provider name "%s" was not recognized by the server';
  SBadVariantType = 'Unsupported variant type: %s';
  SInvalidAction = 'Invalid action received: %d';

  { Resolver }
  SInvalidResponse = 'Invalid response';
  SRecordNotFound = 'Record not found';
  STooManyRecordsModified = 'Update affected more than 1 record.';

  { Provider }
  SInvalidOptParamType = 'Value cannot be stored in an optional parameter';
  SMissingDataSet = 'Missing DataSet property';
  SConstraintFailed = 'Record or field constraint failed.';
  SField = 'Field';
  SReadOnlyProvider = 'Cannot apply updates to a ReadOnly provider';
  SNoKeySpecified = 'Unable to find record.  No key specified';
  SFieldNameTooLong = 'Field name cannot be longer then %d characters.  Try ' +
                      'setting ObjectView to True on the dataset';
  SNoDataSets = 'Cannot resolve to dataset when using nested datasets or references';
  SRecConstFail = 'Preparation of record constraint failed with error "%s"';
  SFieldConstFail = 'Preparation of field constraint failed with error "%s"';
  SDefExprFail = 'Preparation of default expression failed with error "%s"';
  SArrayElementError = 'Array elements of type %s are not supported';
  SNoTableName = 'Unable to resolve records.  Table name not found.';
*)
  SNoEditsAllowed = 'Modifications are not allowed';
  SNoDeletesAllowed = 'Deletes are not allowed';
  SNoInsertsAllowed = 'Inserts are not allowed';
  SCannotChangeCommandText = 'CommandText changes are not allowed';
  SErrorReadOnlyProperty = '"%s" is a readonly property';
  SUnknownNameResult = 'Unknown';
  SReadHeaderFailed = 'Cannot read Header information';
  SReadFieldsFailed = 'Unable to read Fields Structure';
  SWriteDataFailed = 'Failed to write data to stream';
    
{$ifdef fpc}
resourcestring
  SInvalidFieldSize = 'Invalid field size';
  SInvalidFieldKind = 'Invalid FieldKind';
  SInvalidFieldRegistration = 'Invalid field registration';
  SUnknownFieldType = 'Field ''%s'' is of an unknown type';
  SFieldNameMissing = 'Field name missing';
  SDuplicateFieldName = 'Duplicate field name ''%s''';
  SFieldNotFound = 'Field ''%s'' not found';
  SFieldAccessError = 'Cannot access field ''%s'' as type %s';
  SFieldValueError = 'Invalid value for field ''%s''';
  SFieldRangeError = '%g is not a valid value for field ''%s''. The allowed range is %g to %g';
  SInvalidIntegerValue = '''%s'' is not a valid integer value for field ''%s''';
  SInvalidBoolValue = '''%s'' is not a valid boolean value for field ''%s''';
  SInvalidFloatValue = '''%s'' is not a valid floating point value for field ''%s''';
  SFieldTypeMismatch = 'Type mismatch for field ''%s'', expecting: %s actual: %s';
  SFieldSizeMismatch = 'Size mismatch for field ''%s'', expecting: %d actual: %d';
  SInvalidVarByteArray = 'Invalid variant type or size for field ''%s''';
  SFieldOutOfRange = 'Value of field ''%s'' is out of range';
  SBCDOverflow = '(Overflow)';
  SFieldRequired = 'Field ''%s'' must have a value';
  SDataSetMissing = 'Field ''%s'' has no dataset';
  SInvalidCalcType = 'Field ''%s'' cannot be a calculated or lookup field';
  SFieldReadOnly = 'Field ''%s'' cannot be modified';
  SFieldIndexError = 'Field index out of range';
  SNoFieldIndexes = 'No index currently active';
  SNotIndexField = 'Field ''%s'' is not indexed and cannot be modified';
  SIndexFieldMissing = 'Cannot access index field ''%s''';
  SDuplicateIndexName = 'Duplicate index name ''%s''';
  SNoIndexForFields = 'No index for fields ''%s''';
  SIndexNotFound = 'Index ''%s'' not found';
  SDuplicateName = 'Duplicate name ''%s'' in %s';
  SCircularDataLink = 'Circular datalinks are not allowed';
  SLookupInfoError = 'Lookup information for field ''%s'' is incomplete';
  SDataSourceChange = 'DataSource cannot be changed';
  SNoNestedMasterSource = 'Nested datasets cannot have a MasterSource';
  SDataSetOpen = 'Cannot perform this operation on an open dataset';
  SNotEditing = 'Dataset not in edit or insert mode';
  SDataSetClosed = 'Cannot perform this operation on a closed dataset';
  SDataSetEmpty = 'Cannot perform this operation on an empty dataset';
  SDataSetReadOnly = 'Cannot modify a read-only dataset';
  SNestedDataSetClass = 'Nested dataset must inherit from %s';
  SExprTermination = 'Filter expression incorrectly terminated';
  SExprNameError = 'Unterminated field name';
  SExprStringError = 'Unterminated string constant';
  SExprInvalidChar = 'Invalid filter expression character: ''%s''';
  SExprNoLParen = '''('' expected but %s found';
  SExprNoRParen = ''')'' expected but %s found';
  SExprNoRParenOrComma = ''')'' or '','' expected but %s found';
  SExprExpected = 'Expression expected but %s found';
  SExprBadField = 'Field ''%s'' cannot be used in a filter expression';
  SExprBadNullTest = 'NULL only allowed with ''='' and ''<>''';
  SExprRangeError = 'Constant out of range';
  SExprNotBoolean = 'Field ''%s'' is not of type Boolean';
  SExprIncorrect = 'Incorrectly formed filter expression';
  SExprNothing = 'nothing';
  SExprTypeMis = 'Type mismatch in expression';
  SExprBadScope = 'Operation cannot mix aggregate value with record-varying value';
  SExprNoArith = 'Arithmetic in filter expressions not supported';
  SExprNotAgg = 'Expression is not an aggregate expression';
  SExprBadConst = 'Constant is not correct type %s';
  SExprNoAggFilter = 'Aggregate expressions not allowed in filters';
  SExprEmptyInList = 'IN predicate list may not be empty';
  SInvalidKeywordUse = 'Invalid use of keyword';
  STextFalse = 'False';
  STextTrue = 'True';
  SParameterNotFound = 'Parameter ''%s'' not found';
  SInvalidVersion = 'Unable to load bind parameters';
  SParamTooBig = 'Parameter ''%s'', cannot save data larger than %d bytes';
  SBadFieldType = 'Field ''%s'' is of an unsupported type';
  SAggActive = 'Property may not be modified while aggregate is active';
  SProviderSQLNotSupported = 'SQL not supported: %s';
  SProviderExecuteNotSupported = 'Execute not supported: %s';
  SExprNoAggOnCalcs = 'Field ''%s'' is not the correct type of calculated field to be used in an aggregate, use an internalcalc';
  SRecordChanged = 'Record changed by another user';

  { DBCtrls }
  SFirstRecord = 'First record';
  SPriorRecord = 'Prior record';
  SNextRecord = 'Next record';
  SLastRecord = 'Last record';
  SInsertRecord = 'Insert record';
  SDeleteRecord = 'Delete record';
  SEditRecord = 'Edit record';
  SPostEdit = 'Post edit';
  SCancelEdit = 'Cancel edit';
  SRefreshRecord = 'Refresh data';
  SDeleteRecordQuestion = 'Delete record?';
  SDeleteMultipleRecordsQuestion = 'Delete all selected records?';
  SRecordNotFound = 'Record not found';
  SDataSourceFixed = 'Operation not allowed in a DBCtrlGrid';
  SNotReplicatable = 'Control cannot be used in a DBCtrlGrid';
  SPropDefByLookup = 'Property already defined by lookup field';
  STooManyColumns = 'Grid requested to display more than 256 columns';

  { DBLogDlg }
  SRemoteLogin = 'Remote Login';

  { DBOleEdt }
  SDataBindings = 'Data Bindings...';
{$endif}

{$ifndef DELPHI6}
  SCannotCreateDir = 'Unable to create directory';
{$endif}



{============================================================================}
{                            Error Categories                                }
{============================================================================}

{
  function ErrCat(rslt: Word): Word;
  function ErrCode(rslt: Word): Word;
}

const
  ERRCAT_NONE                   = 0;      {  0   No error }
  ERRCAT_SYSTEM                 = $21;    {  33  System related (Fatal Error) }
  ERRCAT_NOTFOUND               = $22;    {  34  Object of interest Not Found }
  ERRCAT_DATACORRUPT            = $23;    {  35  Physical Data Corruption }
  ERRCAT_IO                     = $24;    {  36  I/O related error }
  ERRCAT_LIMIT                  = $25;    {  37  Resource or Limit error }
  ERRCAT_INTEGRITY              = $26;    {  38  Integrity Violation }
  ERRCAT_INVALIDREQ             = $27;    {  39  Invalid Request }
  ERRCAT_LOCKCONFLICT           = $28;    {  40  Locking/Contention related }
  ERRCAT_SECURITY               = $29;    {  41  Access Violation - Security related }
  ERRCAT_INVALIDCONTEXT         = $2A;    {  42  Invalid context }
  ERRCAT_OS                     = $2B;    {  43  Os Error not handled by Idapi }
  ERRCAT_NETWORK                = $2C;    {  44  Network related }
  ERRCAT_OPTPARAM               = $2D;    {  45  Optional parameter related }
  ERRCAT_QUERY                  = $2E;    {  46  Query related }
  ERRCAT_VERSION                = $2F;    {  47  Version Mismatch Category }
  ERRCAT_CAPABILITY             = $30;    {  48  Capability not supported }
  ERRCAT_CONFIG                 = $31;    {  49  System configuration error }
  ERRCAT_WARNING                = $32;    {  50 }
  ERRCAT_OTHER                  = $33;    {  51  Miscellaneous }
  ERRCAT_COMPATIBILITY          = $34;    {  52  Compatibility related }
  ERRCAT_REPOSITORY             = $35;    {  53  Data Repository related }

  ERRCAT_DRIVER                 = $3E;    {  62  Driver related }
  ERRCAT_RC                     = $3F;    {  63  Internal }


  ERRBASE_NONE                  = 0;      { No error }
  ERRBASE_SYSTEM                = $2100;  { System related (Fatal Error) }
  ERRBASE_NOTFOUND              = $2200;  { Object of interest Not Found }
  ERRBASE_DATACORRUPT           = $2300;  { Physical Data Corruption }
  ERRBASE_IO                    = $2400;  { I/O related error }
  ERRBASE_LIMIT                 = $2500;  { Resource or Limit error }
  ERRBASE_INTEGRITY             = $2600;  { Integrity Violation }
  ERRBASE_INVALIDREQ            = $2700;  { Invalid Request }
  ERRBASE_LOCKCONFLICT          = $2800;  { Locking/Contention related }
  ERRBASE_SEC                   = $2900;  { Access Violation - Security related }
  ERRBASE_IC                    = $2A00;  { Invalid context }
  ERRBASE_OS                    = $2B00;  { Os Error not handled by Idapi }
  ERRBASE_NETWORK               = $2C00;  { Network related }
  ERRBASE_OPTPARAM              = $2D00;  { Optional Parameter related }
  ERRBASE_QUERY                 = $2E00;  { Query related }
  ERRBASE_VERSION               = $2F00;  { Version Mismatch Category }
  ERRBASE_CAPABILITY            = $3000;  { Capability not supported }
  ERRBASE_CONFIG                = $3100;  { System configuration error }
  ERRBASE_WARNING               = $3200;
  ERRBASE_OTHER                 = $3300;  { Miscellaneous }
  ERRBASE_COMPATIBILITY         = $3400;  { Compatibility related }
  ERRBASE_REPOSITORY            = $3500;  { Data Repository related }

  ERRBASE_DRIVER                = $3E00;  { Driver related }
  ERRBASE_RC                    = $3F00;  { Internal }


{=============================================================================}
{                           Error Codes By Category                           }
{=============================================================================}

{ ERRCAT_NONE                  (0) }
{ ===========                      }

  ERRCODE_NONE                  = 0;

  DBIERR_NONE                   = (ERRBASE_NONE + ERRCODE_NONE);

{  ERRCAT_SYSTEM }
{  ============= }

  ERRCODE_SYSFILEOPEN           = 1;      { Cannot open a system file }
  ERRCODE_SYSFILEIO             = 2;      { I/O error on a system file }
  ERRCODE_SYSCORRUPT            = 3;      { Data structure corruption }
  ERRCODE_NOCONFIGFILE          = 4;      { Cannot find config file }
  ERRCODE_CFGCANNOTWRITE        = 5;      { Cannot write config file (READONLY) }
  ERRCODE_CFGMULTIFILE          = 6;      { Initializing with different ini file }
  ERRCODE_REENTERED             = 7;      { System has been illegally re-entered }
  ERRCODE_CANTFINDIDAPI         = 8;      { Cannot locate IDAPIxx.DLL }
  ERRCODE_CANTLOADIDAPI         = 9;      { Cannot load IDAPIxx.DLL }
  ERRCODE_CANTLOADLIBRARY       = 10;     { Cannot load a service DLL }
  ERRCODE_TEMPFILEERR           = 11;     { Cannot create or open temporary file }
  ERRCODE_MULTIPLEIDAPI         = 12;     { Trying to load multiple IDAPIxx.DLL }
  ERRCODE_SHAREDMEMCONFLICT     = 13;     { Shared memory conflict. }

  DBIERR_SYSFILEOPEN            = (ERRBASE_SYSTEM + ERRCODE_SYSFILEOPEN);
  DBIERR_SYSFILEIO              = (ERRBASE_SYSTEM + ERRCODE_SYSFILEIO);
  DBIERR_SYSCORRUPT             = (ERRBASE_SYSTEM + ERRCODE_SYSCORRUPT);
  DBIERR_NOCONFIGFILE           = (ERRBASE_SYSTEM + ERRCODE_NOCONFIGFILE);
  DBIERR_CFGCANNOTWRITE         = (ERRBASE_SYSTEM + ERRCODE_CFGCANNOTWRITE);
  DBIERR_CFGMULTIFILE           = (ERRBASE_SYSTEM + ERRCODE_CFGMULTIFILE);
  DBIERR_REENTERED              = (ERRBASE_SYSTEM + ERRCODE_REENTERED);
  DBIERR_CANTFINDIDAPI          = (ERRBASE_SYSTEM + ERRCODE_CANTFINDIDAPI);
  DBIERR_CANTLOADIDAPI          = (ERRBASE_SYSTEM + ERRCODE_CANTLOADIDAPI);
  DBIERR_CANTLOADLIBRARY        = (ERRBASE_SYSTEM + ERRCODE_CANTLOADLIBRARY);
  DBIERR_TEMPFILEERR            = (ERRBASE_SYSTEM + ERRCODE_TEMPFILEERR);
  DBIERR_MULTIPLEIDAPI          = (ERRBASE_SYSTEM + ERRCODE_MULTIPLEIDAPI);
  DBIERR_SHAREDMEMCONFLICT      = (ERRBASE_SYSTEM + ERRCODE_SHAREDMEMCONFLICT);

  DBIERR_CANTFINDODAPI = DBIERR_CANTFINDIDAPI;
  DBIERR_CANTLOADODAPI = DBIERR_CANTLOADIDAPI;

{  ERRCAT_NOTFOUND }
{  =============== }

  ERRCODE_BOF                   = 1;      { Beginning of Virtual table }
  ERRCODE_EOF                   = 2;      { End of Virtual table }
  ERRCODE_RECMOVED              = 3;      { Fly-away }
  ERRCODE_KEYORRECDELETED       = 4;      { Record Deleted/Key Modified }
  ERRCODE_NOCURRREC             = 5;      { No current record }
  ERRCODE_RECNOTFOUND           = 6;      { Record was not found }
  ERRCODE_ENDOFBLOB             = 7;      { End of Blob reached }
  ERRCODE_OBJNOTFOUND           = 8;      { Generic Not found }
  ERRCODE_FMLMEMBERNOTFOUND     = 9;      { Family member not found }
  ERRCODE_BLOBFILEMISSING       = 10;     { 0x0a Blob file for table is missing }
  ERRCODE_LDNOTFOUND            = 11;     { 0x0b Language driver not found }

  DBIERR_BOF                    = (ERRBASE_NOTFOUND + ERRCODE_BOF);
  DBIERR_EOF                    = (ERRBASE_NOTFOUND + ERRCODE_EOF);
  DBIERR_RECMOVED               = (ERRBASE_NOTFOUND + ERRCODE_RECMOVED);
  DBIERR_RECDELETED             = (ERRBASE_NOTFOUND + ERRCODE_KEYORRECDELETED);
  DBIERR_KEYORRECDELETED        = (ERRBASE_NOTFOUND + ERRCODE_KEYORRECDELETED);
  DBIERR_NOCURRREC              = (ERRBASE_NOTFOUND + ERRCODE_NOCURRREC);
  DBIERR_RECNOTFOUND            = (ERRBASE_NOTFOUND + ERRCODE_RECNOTFOUND);
  DBIERR_ENDOFBLOB              = (ERRBASE_NOTFOUND + ERRCODE_ENDOFBLOB);
  DBIERR_OBJNOTFOUND            = (ERRBASE_NOTFOUND + ERRCODE_OBJNOTFOUND);
  DBIERR_FMLMEMBERNOTFOUND      = (ERRBASE_NOTFOUND + ERRCODE_FMLMEMBERNOTFOUND);
  DBIERR_BLOBFILEMISSING        = (ERRBASE_NOTFOUND + ERRCODE_BLOBFILEMISSING);
  DBIERR_LDNOTFOUND             = (ERRBASE_NOTFOUND + ERRCODE_LDNOTFOUND);

{ ERRCAT_DATACORRUPT }
{ ================== }

  ERRCODE_HEADERCORRUPT         = 1;      { Corrupt Header }
  ERRCODE_FILECORRUPT           = 2;      { File corrupt - other than header }
  ERRCODE_MEMOCORRUPT           = 3;      { Memo file corrupted }
  ERRCODE_BMPCORRUPT            = 4;      { BitMap is corrupt (Internal error) }
  ERRCODE_INDEXCORRUPT          = 5;      { Index is corrupt }
  ERRCODE_CORRUPTLOCKFILE       = 6;      { Corrupt lock file }
  ERRCODE_FAMFILEINVALID        = 7;      { Corrupt family file }
  ERRCODE_VALFILECORRUPT        = 8;      { Val file is missing or corrupt }
  ERRCODE_FOREIGNINDEX          = 9;      { Index is in a foreign format - import first }


  DBIERR_HEADERCORRUPT          = (ERRBASE_DATACORRUPT + ERRCODE_HEADERCORRUPT);
  DBIERR_FILECORRUPT            = (ERRBASE_DATACORRUPT + ERRCODE_FILECORRUPT);
  DBIERR_MEMOCORRUPT            = (ERRBASE_DATACORRUPT + ERRCODE_MEMOCORRUPT);
  DBIERR_BMPCORRUPT             = (ERRBASE_DATACORRUPT + ERRCODE_BMPCORRUPT);
  DBIERR_INDEXCORRUPT           = (ERRBASE_DATACORRUPT + ERRCODE_INDEXCORRUPT);
  DBIERR_CORRUPTLOCKFILE        = (ERRBASE_DATACORRUPT + ERRCODE_CORRUPTLOCKFILE);
  DBIERR_FAMFILEINVALID         = (ERRBASE_DATACORRUPT + ERRCODE_FAMFILEINVALID);
  DBIERR_VALFILECORRUPT         = (ERRBASE_DATACORRUPT + ERRCODE_VALFILECORRUPT);
  DBIERR_FOREIGNINDEX           = (ERRBASE_DATACORRUPT + ERRCODE_FOREIGNINDEX);


{ ERRCAT_IO }
{ ========= }

  ERRCODE_READERR               = 1;      { Read failure (not expected) }
  ERRCODE_WRITEERR              = 2;      { Write failure (not expected) }
  ERRCODE_DIRNOACCESS           = 3;      { No access to dir }
  ERRCODE_FILEDELETEFAIL        = 4;      { File delete failed }
  ERRCODE_FILENOACCESS          = 5;      { No access to file }
  ERRCODE_ACCESSDISABLED        = 6;      { Access to table disabled (previous error) }

  DBIERR_READERR                = (ERRBASE_IO + ERRCODE_READERR);
  DBIERR_WRITEERR               = (ERRBASE_IO + ERRCODE_WRITEERR);
  DBIERR_DIRNOACCESS            = (ERRBASE_IO + ERRCODE_DIRNOACCESS);
  DBIERR_FILEDELETEFAIL         = (ERRBASE_IO + ERRCODE_FILEDELETEFAIL);
  DBIERR_FILENOACCESS           = (ERRBASE_IO + ERRCODE_FILENOACCESS);
  DBIERR_ACCESSDISABLED         = (ERRBASE_IO + ERRCODE_ACCESSDISABLED);

{ ERRCAT_LIMIT }
{ ============ }

  ERRCODE_NOMEMORY              = 1;      { Not enough Memory for this op }
  ERRCODE_NOFILEHANDLES         = 2;      { Not enough File handles }
  ERRCODE_NODISKSPACE           = 3;      { Not enough Disk space }
  ERRCODE_NOTEMPTBLSPACE        = 4;      { Temporary Table resource limit }
  ERRCODE_RECTOOBIG             = 5;      { Too big a record size for table }
  ERRCODE_CURSORLIMIT           = 6;      { Too many open cursors }
  ERRCODE_TABLEFULL             = 7;      { Table is full }
  ERRCODE_WSSESLIMIT            = 8;      { Too many sessions from this WS }
  ERRCODE_SERNUMLIMIT           = 9;      { Serial number limit (paradox) }
  ERRCODE_INTERNALLIMIT         = 10;     { 0x0a Some internal limit (see context) }
  ERRCODE_OPENTBLLIMIT          = 11;     { 0x0b Too many open tables }
  ERRCODE_TBLCURSORLIMIT        = 12;     { 0x0c Too many cursors per table }
  ERRCODE_RECLOCKLIMIT          = 13;     { 0x0d Too many record locks on table }
  ERRCODE_CLIENTSLIMIT          = 14;     { 0x0e Too many clients }
  ERRCODE_INDEXLIMIT            = 15;     { 0x0f Too many indexes (also in Table Create) }
  ERRCODE_SESSIONSLIMIT         = 16;     { 0x10 Too many sessions }
  ERRCODE_DBLIMIT               = 17;     { 0x11 Too many databases }
  ERRCODE_PASSWORDLIMIT         = 18;     { 0x12 Too many passwords }
  ERRCODE_DRIVERLIMIT           = 19;     { 0x13 Too many active drivers }
  ERRCODE_FLDLIMIT              = 20;     { 0x14 Too many Fields in Table Create }
  ERRCODE_TBLLOCKLIMIT          = 21;     { 0x15 Too many table locks }
  ERRCODE_OPENBLOBLIMIT         = 22;     { 0x16 Too many open blobs }
  ERRCODE_LOCKFILELIMIT         = 23;     { 0x17 Lock file has grown too big }
  ERRCODE_OPENQRYLIMIT          = 24;     { 0x18 Too many open queries }
  ERRCODE_THREADLIMIT           = 25;     { 0x19 Too many threads for client }
  ERRCODE_BLOBLIMIT             = 26;     { 0x1a Too many blobs }
  ERRCODE_PDX50NAMELIMIT        = 27;     { 0x1b Pathname is too long for a Paradox 5.0 or less table }
  ERRCODE_ROWFETCHLIMIT         = 28;     { 0x1c Row fetch limit }
  ERRCODE_LONGNAMENOTALLOWED    = 29;     { 0x1d Long name is not allowed for this tableversion }
  ERRCODE_NOSHAREDMEMORY        = 30;     { 0x1e Not enough shared Memory for this operation }

  DBIERR_NOMEMORY               = (ERRBASE_LIMIT + ERRCODE_NOMEMORY);
  DBIERR_NOFILEHANDLES          = (ERRBASE_LIMIT + ERRCODE_NOFILEHANDLES);
  DBIERR_NODISKSPACE            = (ERRBASE_LIMIT + ERRCODE_NODISKSPACE);
  DBIERR_NOTEMPTBLSPACE         = (ERRBASE_LIMIT + ERRCODE_NOTEMPTBLSPACE);
  DBIERR_RECTOOBIG              = (ERRBASE_LIMIT + ERRCODE_RECTOOBIG);
  DBIERR_CURSORLIMIT            = (ERRBASE_LIMIT + ERRCODE_CURSORLIMIT);
  DBIERR_TABLEFULL              = (ERRBASE_LIMIT + ERRCODE_TABLEFULL);
  DBIERR_WSSESLIMIT             = (ERRBASE_LIMIT + ERRCODE_WSSESLIMIT);
  DBIERR_SERNUMLIMIT            = (ERRBASE_LIMIT + ERRCODE_SERNUMLIMIT);
  DBIERR_INTERNALLIMIT          = (ERRBASE_LIMIT + ERRCODE_INTERNALLIMIT);
  DBIERR_OPENTBLLIMIT           = (ERRBASE_LIMIT + ERRCODE_OPENTBLLIMIT);
  DBIERR_TBLCURSORLIMIT         = (ERRBASE_LIMIT + ERRCODE_TBLCURSORLIMIT);
  DBIERR_RECLOCKLIMIT           = (ERRBASE_LIMIT + ERRCODE_RECLOCKLIMIT);
  DBIERR_CLIENTSLIMIT           = (ERRBASE_LIMIT + ERRCODE_CLIENTSLIMIT);
  DBIERR_INDEXLIMIT             = (ERRBASE_LIMIT + ERRCODE_INDEXLIMIT);
  DBIERR_SESSIONSLIMIT          = (ERRBASE_LIMIT + ERRCODE_SESSIONSLIMIT);
  DBIERR_DBLIMIT                = (ERRBASE_LIMIT + ERRCODE_DBLIMIT);
  DBIERR_PASSWORDLIMIT          = (ERRBASE_LIMIT + ERRCODE_PASSWORDLIMIT);
  DBIERR_DRIVERLIMIT            = (ERRBASE_LIMIT + ERRCODE_DRIVERLIMIT);
  DBIERR_FLDLIMIT               = (ERRBASE_LIMIT + ERRCODE_FLDLIMIT);
  DBIERR_TBLLOCKLIMIT           = (ERRBASE_LIMIT + ERRCODE_TBLLOCKLIMIT);
  DBIERR_OPENBLOBLIMIT          = (ERRBASE_LIMIT + ERRCODE_OPENBLOBLIMIT);
  DBIERR_LOCKFILELIMIT          = (ERRBASE_LIMIT + ERRCODE_LOCKFILELIMIT);
  DBIERR_OPENQRYLIMIT           = (ERRBASE_LIMIT + ERRCODE_OPENQRYLIMIT);
  DBIERR_THREADLIMIT            = (ERRBASE_LIMIT + ERRCODE_THREADLIMIT);
  DBIERR_BLOBLIMIT              = (ERRBASE_LIMIT + ERRCODE_BLOBLIMIT);
  DBIERR_PDX50NAMELIMIT         = (ERRBASE_LIMIT + ERRCODE_PDX50NAMELIMIT);
  DBIERR_ROWFETCHLIMIT          = (ERRBASE_LIMIT + ERRCODE_ROWFETCHLIMIT);
  DBIERR_LONGNAMENOTALLOWED     = (ERRBASE_LIMIT + ERRCODE_LONGNAMENOTALLOWED);
  DBIERR_NOSHAREDMEMORY         = (ERRBASE_LIMIT + ERRCODE_NOSHAREDMEMORY);


{ ERRCAT_INTEGRITY }
{ ================ }

  ERRCODE_KEYVIOL               = 1;      { Key violation }
  ERRCODE_MINVALERR             = 2;      { Min val check failed }
  ERRCODE_MAXVALERR             = 3;      { Max val check failed }
  ERRCODE_REQDERR               = 4;      { Field value required }
  ERRCODE_FORIEGNKEYERR         = 5;      { Master record missing }
  ERRCODE_DETAILRECORDSEXIST    = 6;      { Cannot MODIFY or DELETE this Master record }
  ERRCODE_MASTERTBLLEVEL        = 7;      { Master Table Level is incorrect }
  ERRCODE_LOOKUPTABLEERR        = 8;      { Field value out of lookup tbl range }
  ERRCODE_LOOKUPTBLOPENERR      = 9;      { Lookup Table Open failed }
  ERRCODE_DETAILTBLOPENERR      = 10;     { 0x0a Detail Table Open failed }
  ERRCODE_MASTERTBLOPENERR      = 11;     { 0x0b Master Table Open failed }
  ERRCODE_FIELDISBLANK          = 12;     { 0x0c Field is blank }

  ERRCODE_MASTEREXISTS          = 13;     { 0x0d Master Table exists }
  ERRCODE_MASTERTBLOPEN         = 14;     { 0x0e Master Table is open }

  ERRCODE_DETAILTABLESEXIST     = 15;     { 0x0f Detail Tables exist ( cannot delete, rename ... ) }
  ERRCODE_DETAILRECEXISTEMPTY   = 16;     { 0x10 Cannot empty because details exist }
  ERRCODE_MASTERREFERENCEERR    = 17;     { 0x11 Cannot modify while adding self referencing Referential Integrity }
  ERRCODE_DETAILTBLOPEN         = 18;     { 0x12 Detail Table is opened }
  ERRCODE_DEPENDENTSMUSTBEEMPTY = 19;     { 0x13 Cannot make a master a detail of another table if its details are not empty. }
  ERRCODE_RINTREQINDEX          = 20;     { 0x14 Ref. integrity fields must be indexed }
  ERRCODE_LINKEDTBLPROTECTED    = 21;     { 0x15 Master Table is protected ( requires password to open) }
  ERRCODE_FIELDMULTILINKED      = 22;     { 0x16 Field has more than one master }
  ERRCODE_EXPRVALERR            = 23;     { 0x17 Expr val check failed }

  DBIERR_KEYVIOL                = (ERRBASE_INTEGRITY + ERRCODE_KEYVIOL);
  DBIERR_MINVALERR              = (ERRBASE_INTEGRITY + ERRCODE_MINVALERR);
  DBIERR_MAXVALERR              = (ERRBASE_INTEGRITY + ERRCODE_MAXVALERR);
  DBIERR_REQDERR                = (ERRBASE_INTEGRITY + ERRCODE_REQDERR);
  DBIERR_FORIEGNKEYERR          = (ERRBASE_INTEGRITY + ERRCODE_FORIEGNKEYERR);
  DBIERR_DETAILRECORDSEXIST     = (ERRBASE_INTEGRITY + ERRCODE_DETAILRECORDSEXIST);
  DBIERR_MASTERTBLLEVEL         = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLLEVEL);
  DBIERR_LOOKUPTABLEERR         = (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTABLEERR);
  DBIERR_LOOKUPTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_LOOKUPTBLOPENERR);
  DBIERR_DETAILTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPENERR);
  DBIERR_MASTERTBLOPENERR       = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPENERR);
  DBIERR_FIELDISBLANK           = (ERRBASE_INTEGRITY + ERRCODE_FIELDISBLANK);
  DBIERR_MASTEREXISTS           = (ERRBASE_INTEGRITY + ERRCODE_MASTEREXISTS);
  DBIERR_MASTERTBLOPEN          = (ERRBASE_INTEGRITY + ERRCODE_MASTERTBLOPEN);
  DBIERR_DETAILTABLESEXIST      = (ERRBASE_INTEGRITY + ERRCODE_DETAILTABLESEXIST);
  DBIERR_DETAILRECEXISTEMPTY    = (ERRBASE_INTEGRITY + ERRCODE_DETAILRECEXISTEMPTY);
  DBIERR_MASTERREFERENCEERR     = (ERRBASE_INTEGRITY + ERRCODE_MASTERREFERENCEERR);
  DBIERR_DETAILTBLOPEN          = (ERRBASE_INTEGRITY + ERRCODE_DETAILTBLOPEN);
  DBIERR_DEPENDENTSMUSTBEEMPTY  = (ERRBASE_INTEGRITY + ERRCODE_DEPENDENTSMUSTBEEMPTY);
  DBIERR_RINTREQINDEX           = (ERRBASE_INTEGRITY + ERRCODE_RINTREQINDEX);
  DBIERR_LINKEDTBLPROTECTED     = (ERRBASE_INTEGRITY + ERRCODE_LINKEDTBLPROTECTED);
  DBIERR_FIELDMULTILINKED       = (ERRBASE_INTEGRITY + ERRCODE_FIELDMULTILINKED);
  DBIERR_EXPRVALERR             = (ERRBASE_INTEGRITY + ERRCODE_EXPRVALERR);


{ ERRCAT_INVALIDREQ }
{ ================= }

  ERRCODE_OUTOFRANGE            = 1;      { Number out of range (e.g field no) }
  ERRCODE_INVALIDPARAM          = 2;      { Generic invalid parameter }
  ERRCODE_INVALIDFILENAME       = 3;      { Invalid file name }
  ERRCODE_NOSUCHFILE            = 4;      { No such file }
  ERRCODE_INVALIDOPTION         = 5;      { Invalid option for a parameter }
  ERRCODE_INVALIDHNDL           = 6;      { Invalid handle to the function }
  ERRCODE_UNKNOWNTBLTYPE        = 7;      { Table type given not known }
  ERRCODE_UNKNOWNFILE           = 8;      { Dont know how to open file }
  ERRCODE_PRIMARYKEYREDEFINE    = 9;      { Cannot redefine primary key }
  ERRCODE_INVALIDRINTDESCNUM    = 10;     { 0x0a Cannot change this RINTDesc }
  ERRCODE_KEYFLDTYPEMISMATCH    = 11;     { 0x0b Foreign & Primary Key Mismatch }
  ERRCODE_INVALIDMODIFYREQUEST  = 12;     { 0x0c Invalid modify request }
  ERRCODE_NOSUCHINDEX           = 13;     { 0x0d Index does not exist }
  ERRCODE_INVALIDBLOBOFFSET     = 14;     { 0x0e Invalid Offset into the Blob }
  ERRCODE_INVALIDDESCNUM        = 15;     { 0x0f Invalid descriptor number }
  ERRCODE_INVALIDFLDTYPE        = 16;     { 0x10 Invalid field type }
  ERRCODE_INVALIDFLDDESC        = 17;     { 0x11 Invalid field descriptor }
  ERRCODE_INVALIDFLDXFORM       = 18;     { 0x12 Invalid field transform }
  ERRCODE_INVALIDRECSTRUCT      = 19;     { 0x13 Invalid record structure }
  ERRCODE_INVALIDDESC           = 20;     { 0x14 Generic: invalid descriptor }
  ERRCODE_INVALIDINDEXSTRUCT    = 21;     { 0x15 Invalid array of indexes descriptors }
  ERRCODE_INVALIDVCHKSTRUCT     = 22;     { 0x16 Invalid array of  val. check descriptors }
  ERRCODE_INVALIDRINTSTRUCT     = 23;     { 0x17 Invalid array of ref. integrity descriptors }
  ERRCODE_INVALIDRESTRTBLORDER  = 24;     { 0x18 Invalid ordering of tables during restructure }
  ERRCODE_NAMENOTUNIQUE         = 25;     { 0x19 Name not unique in this context }
  ERRCODE_INDEXNAMEREQUIRED     = 26;     { 0x1a Index name required }
  ERRCODE_INVALIDSESHANDLE      = 27;     { 0x1b Invalid ses handle }
  ERRCODE_INVALIDRESTROP        = 28;     { 0x1c Invalid restructure operation }
  ERRCODE_UNKNOWNDRIVER         = 29;     { 0x1d Driver not known to system }
  ERRCODE_UNKNOWNDB             = 30;     { 0x1e Unknown db }
  ERRCODE_INVALIDPASSWORD       = 31;     { 0x1f Invalid password given }
  ERRCODE_NOCALLBACK            = 32;     { 0x20 No callback function }
  ERRCODE_INVALIDCALLBACKBUFLEN = 33;     { 0x21 Invalid callback buffer length }
  ERRCODE_INVALIDDIR            = 34;     { 0x22 Invalid directory }
  ERRCODE_INVALIDXLATION        = 35;     { 0x23 Translate Error - Translate DID NOT happen }
  ERRCODE_DIFFERENTTABLES       = 36;     { 0x24 Cannot Set Cursor of one Table to another }
  ERRCODE_INVALIDBOOKMARK       = 37;     { 0x25 Bookmarks does not match table, etc. }
  ERRCODE_INVALIDINDEXNAME      = 38;     { 0x26 Index/Tag Name is invalid }
  ERRCODE_INVALIDIDXDESC        = 39;     { 0x27 Invalid index descriptor }
  ERRCODE_NOSUCHTABLE           = 40;     { 0x28 No such table }
  ERRCODE_USECOUNT              = 41;     { 0x29 Table has too many users }
  ERRCODE_INVALIDKEY            = 42;     { 0x2a Key does not pass filter condition }
  ERRCODE_INDEXEXISTS           = 43;     { 0x2b Index already exists }
  ERRCODE_INDEXOPEN             = 44;     { 0x2c Index is open }
  ERRCODE_INVALIDBLOBLEN        = 45;     { 0x2d Invalid Blob Length }
  ERRCODE_INVALIDBLOBHANDLE     = 46;     { 0x2e Invalid Blob handle (in record buffer) }
  ERRCODE_TABLEOPEN             = 47;     { 0x2f Table is open }
  ERRCODE_NEEDRESTRUCTURE       = 48;     { 0x30 Need to do (hard) restructure }
  ERRCODE_INVALIDMODE           = 49;     { 0x31 Invalid mode }
  ERRCODE_CANNOTCLOSE           = 50;     { 0x32 Cannot close index }
  ERRCODE_ACTIVEINDEX           = 51;     { 0x33 Index is being used to order tbl }
  ERRCODE_INVALIDUSRPASS        = 52;     { 0x34 Bad user name or password }
  ERRCODE_MULTILEVELCASCADE     = 53;     { 0x35 Multi level Cascade not supported }
  ERRCODE_INVALIDFIELDNAME      = 54;     { 0x36 Invalid field name }
  ERRCODE_INVALIDTABLENAME      = 55;     { 0x37 Invalid table name }
  ERRCODE_INVALIDLINKEXPR       = 56;     { 0x38 Invalid linked cursor expression }
  ERRCODE_NAMERESERVED          = 57;     { 0x39 Name is reserved }
  ERRCODE_INVALIDFILEEXTN       = 58;     { 0x3a Invalid file extention }
  ERRCODE_INVALIDLANGDRV        = 59;     { 0x3b Invalid language driver }
  ERRCODE_ALIASNOTOPEN          = 60;     { 0x3c Requested alias in not open }
  ERRCODE_INCOMPATRECSTRUCTS    = 61;     { 0x3d Incompatible record structures }
  ERRCODE_RESERVEDDOSNAME       = 62;     { 0x3e Reserved dos name }
  ERRCODE_DESTMUSTBEINDEXED     = 63;     { 0x3f Destination must be indexed }
  ERRCODE_INVALIDINDEXTYPE      = 64;     { 0x40 Invalid index type }
  ERRCODE_LANGDRVMISMATCH       = 65;     { 0x41 Language driver of table and index do not match }
  ERRCODE_NOSUCHFILTER          = 66;     { 0x42 Filter handle is invalid }
  ERRCODE_INVALIDFILTER         = 67;     { 0x43 Invalid filter }

  ERRCODE_INVALIDTABLECREATE    = 68;     { 0x44 Bad table create request (exact prob unknown) }
  ERRCODE_INVALIDTABLEDELETE    = 69;     { 0x45 Bad table delete request (exact prob unknown) }
  ERRCODE_INVALIDINDEXCREATE    = 70;     { 0x46 Bad index create request (exact prob unknown) }
  ERRCODE_INVALIDINDEXDELETE    = 71;     { 0x47 Bad index delete request (exact prob unknown) }
  ERRCODE_INVALIDTABLE          = 72;     { 0x48 Invalid table name specified }
  ERRCODE_MULTIRESULTS          = 73;     { 0X49 Multi results }
  ERRCODE_INVALIDTIME           = 74;     { 0X4A Multi results }
  ERRCODE_INVALIDDATE           = 75;     { 0X4B Multi results }
  ERRCODE_INVALIDTIMESTAMP      = 76;     { 0X4C Multi results }
  ERRCODE_DIFFERENTPATH         = 77;     { 0X4d Tables in different paths }
  ERRCODE_MISMATCHARGS          = 78;     { 0x4e MisMatch in the # of arguments }
  ERRCODE_FUNCTIONNOTFOUND      = 79;     { 0x4f Loaderlib cant find a func in the DLL (bad version?) }
  ERRCODE_MUSTUSEBASEORDER      = 80;     { 0x50 Must use baseorder for this operation }
  ERRCODE_INVALIDPROCEDURENAME  = 81;     { 0x51 Invalid procedure name }
  ERRCODE_INVALIDFLDMAP         = 82;     { 0x52 invalid field map }


  DBIERR_OUTOFRANGE             = (ERRBASE_INVALIDREQ + ERRCODE_OUTOFRANGE);
  DBIERR_INVALIDPARAM           = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPARAM);
  DBIERR_INVALIDFILENAME        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILENAME);
  DBIERR_NOSUCHFILE             = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHFILE);
  DBIERR_INVALIDOPTION          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDOPTION);
  DBIERR_INVALIDHNDL            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDHNDL);
  DBIERR_UNKNOWNTBLTYPE         = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNTBLTYPE);
  DBIERR_UNKNOWNFILE            = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNFILE);
  DBIERR_PRIMARYKEYREDEFINE     = (ERRBASE_INVALIDREQ + ERRCODE_PRIMARYKEYREDEFINE);
  DBIERR_INVALIDRINTDESCNUM     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRINTDESCNUM);
  DBIERR_KEYFLDTYPEMISMATCH     = (ERRBASE_INVALIDREQ + ERRCODE_KEYFLDTYPEMISMATCH);
  DBIERR_INVALIDMODIFYREQUEST   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDMODIFYREQUEST);
  DBIERR_NOSUCHINDEX            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHINDEX);
  DBIERR_INVALIDBLOBOFFSET      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBOFFSET);
  DBIERR_INVALIDDESCNUM         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDESCNUM);
  DBIERR_INVALIDFLDTYPE         = (ERRBASE_INVALIDREQ +  ERRCODE_INVALIDFLDTYPE);
  DBIERR_INVALIDFLDDESC         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDDESC);
  DBIERR_INVALIDFLDXFORM        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDXFORM);
  DBIERR_INVALIDRECSTRUCT       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRECSTRUCT);
  DBIERR_INVALIDDESC            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDESC);
  DBIERR_INVALIDINDEXSTRUCT     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXSTRUCT);
  DBIERR_INVALIDVCHKSTRUCT      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDVCHKSTRUCT);
  DBIERR_INVALIDRINTSTRUCT      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRINTSTRUCT);
  DBIERR_INVALIDRESTRTBLORDER   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRESTRTBLORDER);
  DBIERR_NAMENOTUNIQUE          = (ERRBASE_INVALIDREQ + ERRCODE_NAMENOTUNIQUE);
  DBIERR_INDEXNAMEREQUIRED      = (ERRBASE_INVALIDREQ + ERRCODE_INDEXNAMEREQUIRED);
  DBIERR_INVALIDSESHANDLE       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDSESHANDLE);
  DBIERR_INVALIDRESTROP         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDRESTROP);
  DBIERR_UNKNOWNDRIVER          = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNDRIVER);
  DBIERR_UNKNOWNDB              = (ERRBASE_INVALIDREQ + ERRCODE_UNKNOWNDB);
  DBIERR_INVALIDPASSWORD        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPASSWORD);
  DBIERR_NOCALLBACK             = (ERRBASE_INVALIDREQ + ERRCODE_NOCALLBACK);
  DBIERR_INVALIDCALLBACKBUFLEN  = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDCALLBACKBUFLEN );
  DBIERR_INVALIDDIR             = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDIR);
  DBIERR_INVALIDXLATION         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDXLATION);
  DBIERR_DIFFERENTTABLES        = (ERRBASE_INVALIDREQ + ERRCODE_DIFFERENTTABLES);
  DBIERR_INVALIDBOOKMARK        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBOOKMARK);
  DBIERR_INVALIDINDEXNAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXNAME);
  DBIERR_INVALIDIDXDESC         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDIDXDESC);
  DBIERR_NOSUCHTABLE            = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHTABLE);
  DBIERR_USECOUNT               = (ERRBASE_INVALIDREQ + ERRCODE_USECOUNT);
  DBIERR_INVALIDKEY             = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDKEY);
  DBIERR_INDEXEXISTS            = (ERRBASE_INVALIDREQ + ERRCODE_INDEXEXISTS);
  DBIERR_INDEXOPEN              = (ERRBASE_INVALIDREQ + ERRCODE_INDEXOPEN);
  DBIERR_INVALIDBLOBLEN         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBLEN);
  DBIERR_INVALIDBLOBHANDLE      = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDBLOBHANDLE);
  DBIERR_TABLEOPEN              = (ERRBASE_INVALIDREQ + ERRCODE_TABLEOPEN);
  DBIERR_NEEDRESTRUCTURE        = (ERRBASE_INVALIDREQ + ERRCODE_NEEDRESTRUCTURE);
  DBIERR_INVALIDMODE            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDMODE);
  DBIERR_CANNOTCLOSE            = (ERRBASE_INVALIDREQ + ERRCODE_CANNOTCLOSE);
  DBIERR_ACTIVEINDEX            = (ERRBASE_INVALIDREQ + ERRCODE_ACTIVEINDEX);
  DBIERR_INVALIDUSRPASS         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDUSRPASS);
  DBIERR_MULTILEVELCASCADE      = (ERRBASE_INVALIDREQ + ERRCODE_MULTILEVELCASCADE);
  DBIERR_INVALIDFIELDNAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFIELDNAME);
  DBIERR_INVALIDTABLENAME       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLENAME);
  DBIERR_INVALIDLINKEXPR        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDLINKEXPR);
  DBIERR_NAMERESERVED           = (ERRBASE_INVALIDREQ + ERRCODE_NAMERESERVED);
  DBIERR_INVALIDFILEEXTN        = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILEEXTN);
  DBIERR_INVALIDLANGDRV         = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDLANGDRV);
  DBIERR_ALIASNOTOPEN           = (ERRBASE_INVALIDREQ + ERRCODE_ALIASNOTOPEN);
  DBIERR_INCOMPATRECSTRUCTS     = (ERRBASE_INVALIDREQ + ERRCODE_INCOMPATRECSTRUCTS);
  DBIERR_RESERVEDOSNAME         = (ERRBASE_INVALIDREQ + ERRCODE_RESERVEDDOSNAME);
  DBIERR_DESTMUSTBEINDEXED      = (ERRBASE_INVALIDREQ + ERRCODE_DESTMUSTBEINDEXED);
  DBIERR_INVALIDINDEXTYPE       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXTYPE);
  DBIERR_LANGDRVMISMATCH        = (ERRBASE_INVALIDREQ + ERRCODE_LANGDRVMISMATCH);
  DBIERR_NOSUCHFILTER           = (ERRBASE_INVALIDREQ + ERRCODE_NOSUCHFILTER);
  DBIERR_INVALIDFILTER          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFILTER);
  DBIERR_INVALIDTABLECREATE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLECREATE);
  DBIERR_INVALIDTABLEDELETE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLEDELETE);
  DBIERR_INVALIDINDEXCREATE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXCREATE);
  DBIERR_INVALIDINDEXDELETE     = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDINDEXDELETE);
  DBIERR_INVALIDTABLE           = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTABLE);
  DBIERR_MULTIRESULTS           = (ERRBASE_INVALIDREQ + ERRCODE_MULTIRESULTS);
  DBIERR_INVALIDTIME            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTIME);
  DBIERR_INVALIDDATE            = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDDATE);
  DBIERR_INVALIDTIMESTAMP       = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDTIMESTAMP);
  DBIERR_DIFFERENTPATH          = (ERRBASE_INVALIDREQ + ERRCODE_DIFFERENTPATH);
  DBIERR_MISMATCHARGS           = (ERRBASE_INVALIDREQ + ERRCODE_MISMATCHARGS);
  DBIERR_FUNCTIONNOTFOUND       = (ERRBASE_INVALIDREQ + ERRCODE_FUNCTIONNOTFOUND);
  DBIERR_MUSTUSEBASEORDER       = (ERRBASE_INVALIDREQ + ERRCODE_MUSTUSEBASEORDER);
  DBIERR_INVALIDPROCEDURENAME   = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDPROCEDURENAME);
  DBIERR_INVALIDFLDMAP          = (ERRBASE_INVALIDREQ + ERRCODE_INVALIDFLDMAP);

{ ERRCAT_LOCKCONFLICT }
{ =================== }

  ERRCODE_LOCKED                = 1;
  ERRCODE_UNLOCKFAILED          = 2;
  ERRCODE_FILEBUSY              = 3;
  ERRCODE_DIRBUSY               = 4;
  ERRCODE_FILELOCKED            = 5;
  ERRCODE_DIRLOCKED             = 6;
  ERRCODE_ALREADYLOCKED         = 7;
  ERRCODE_NOTLOCKED             = 8;
  ERRCODE_LOCKTIMEOUT           = 9;
  ERRCODE_GROUPLOCKED           = 10;     { 0x0a }
  ERRCODE_LOSTTBLLOCK           = 11;     { 0x0b }
  ERRCODE_LOSTEXCLACCESS        = 12;     { 0x0c }
  ERRCODE_NEEDEXCLACCESS        = 13;     { 0x0d }
  ERRCODE_RECGROUPCONFLICT      = 14;     { 0x0e }
  ERRCODE_DEADLOCK              = 15;
  ERRCODE_ACTIVETRAN            = 16;
  ERRCODE_NOACTIVETRAN          = 17;
  ERRCODE_RECLOCKFAILED         = 18;
  ERRCODE_OPTRECLOCKFAILED      = 19;
  ERRCODE_OPTRECLOCKRECDEL      = 20;
  ERRCODE_LOCKEDRECS            = 21;
  ERRCODE_NEEDWRITELOCK         = 22;
  ERRCODE_ENLISTFAILED          = 23; 

  DBIERR_LOCKED                 = (ERRBASE_LOCKCONFLICT + ERRCODE_LOCKED);
  DBIERR_UNLOCKFAILED           = (ERRBASE_LOCKCONFLICT + ERRCODE_UNLOCKFAILED);
  DBIERR_FILEBUSY               = (ERRBASE_LOCKCONFLICT + ERRCODE_FILEBUSY);
  DBIERR_DIRBUSY                = (ERRBASE_LOCKCONFLICT + ERRCODE_DIRBUSY);
  DBIERR_FILELOCKED             = (ERRBASE_LOCKCONFLICT + ERRCODE_FILELOCKED);
  DBIERR_DIRLOCKED              = (ERRBASE_LOCKCONFLICT + ERRCODE_DIRLOCKED);
  DBIERR_ALREADYLOCKED          = (ERRBASE_LOCKCONFLICT + ERRCODE_ALREADYLOCKED);
  DBIERR_NOTLOCKED              = (ERRBASE_LOCKCONFLICT + ERRCODE_NOTLOCKED);
  DBIERR_LOCKTIMEOUT            = (ERRBASE_LOCKCONFLICT + ERRCODE_LOCKTIMEOUT);
  DBIERR_GROUPLOCKED            = (ERRBASE_LOCKCONFLICT + ERRCODE_GROUPLOCKED);
  DBIERR_LOSTTBLLOCK            = (ERRBASE_LOCKCONFLICT + ERRCODE_LOSTTBLLOCK);
  DBIERR_LOSTEXCLACCESS         = (ERRBASE_LOCKCONFLICT + ERRCODE_LOSTEXCLACCESS);
  DBIERR_NEEDEXCLACCESS         = (ERRBASE_LOCKCONFLICT  + ERRCODE_NEEDEXCLACCESS);
  DBIERR_RECGROUPCONFLICT       = (ERRBASE_LOCKCONFLICT + ERRCODE_RECGROUPCONFLICT);
  DBIERR_DEADLOCK               = (ERRBASE_LOCKCONFLICT + ERRCODE_DEADLOCK);
  DBIERR_ACTIVETRAN             = (ERRBASE_LOCKCONFLICT + ERRCODE_ACTIVETRAN);
  DBIERR_NOACTIVETRAN           = (ERRBASE_LOCKCONFLICT + ERRCODE_NOACTIVETRAN);
  DBIERR_RECLOCKFAILED          = (ERRBASE_LOCKCONFLICT + ERRCODE_RECLOCKFAILED);
  DBIERR_OPTRECLOCKFAILED       = (ERRBASE_LOCKCONFLICT + ERRCODE_OPTRECLOCKFAILED);
  DBIERR_OPTRECLOCKRECDEL       = (ERRBASE_LOCKCONFLICT + ERRCODE_OPTRECLOCKRECDEL);
  DBIERR_ENLISTFAILED           = (ERRBASE_LOCKCONFLICT + ERRCODE_ENLISTFAILED); 

{ ERRCAT_SECURITY }
{ =============== }

  ERRCODE_NOTSUFFFIELDRIGHTS    = 1;      { Not sufficient field  rights for operation }
  ERRCODE_NOTSUFFTABLERIGHTS    = 2;      { Not sufficient table  rights for operation }
  ERRCODE_NOTSUFFFAMILYRIGHTS   = 3;      { Not sufficient family rights for operation }
  ERRCODE_READONLYDIR           = 4;      { Is a read-only directory }
  ERRCODE_READONLYDB            = 5;      { Database is read-only }
  ERRCODE_READONLYFLD           = 6;      { Trying to modify read-only field }
  ERRCODE_TBLENCRYPTED          = 7;      { Table is encrypted (dBASE only) }
  ERRCODE_NOTSUFFSQLRIGHTS      = 8;      { Not sufficient sql rights for operation }


  DBIERR_NOTSUFFFIELDRIGHTS     = (ERRBASE_SEC + ERRCODE_NOTSUFFFIELDRIGHTS);
  DBIERR_NOTSUFFTABLERIGHTS     = (ERRBASE_SEC + ERRCODE_NOTSUFFTABLERIGHTS);
  DBIERR_NOTSUFFFAMILYRIGHTS    = (ERRBASE_SEC + ERRCODE_NOTSUFFFAMILYRIGHTS);
  DBIERR_READONLYDIR            = (ERRBASE_SEC + ERRCODE_READONLYDIR);
  DBIERR_READONLYDB             = (ERRBASE_SEC + ERRCODE_READONLYDB);
  DBIERR_READONLYFLD            = (ERRBASE_SEC + ERRCODE_READONLYFLD);
  DBIERR_TBLENCRYPTED           = (ERRBASE_SEC + ERRCODE_TBLENCRYPTED);
  DBIERR_NOTSUFFSQLRIGHTS       = (ERRBASE_SEC + ERRCODE_NOTSUFFSQLRIGHTS);


{ ERRCAT_INVALIDCONTEXT }
{ ===================== }

  ERRCODE_NOTABLOB              = 1;      { Field is not a blob }
  ERRCODE_BLOBOPENED            = 2;      { Blob already opened }
  ERRCODE_BLOBNOTOPENED         = 3;      { Blob not opened }
  ERRCODE_NA                    = 4;      { Operation not applicable }
  ERRCODE_NOTINDEXED            = 5;      { Table is not indexed }
  ERRCODE_NOTINITIALIZED        = 6;      { Engine not initialized }
  ERRCODE_MULTIPLEINIT          = 7;      { Attempt to re-initialize engine }
  ERRCODE_NOTSAMESESSION        = 8;      { Attempt to mix objs from diff ses }
  ERRCODE_PDXDRIVERNOTACTIVE    = 9;      { Paradox driver not active }
  ERRCODE_DRIVERNOTLOADED       = 10;     { 0x0a Driver not loaded }
  ERRCODE_TABLEREADONLY         = 11;     { 0x0b Table is read only }
  ERRCODE_NOASSOCINDEX          = 12;     { 0x0c No index associated with the cursor }
  ERRCODE_HASOPENCURSORS        = 13;     { 0x0d Has open cursors }
  ERRCODE_NOTABLESUPPORT        = 14;     { 0x0e Op cannot be done on this table }
  ERRCODE_INDEXREADONLY         = 15;     { 0x0f Index is read only }
  ERRCODE_NOUNIQUERECS          = 16;     { 0x10 Records are not unique }
  ERRCODE_NOTCURSESSION         = 17;     { 0x11 Not the current/active session }
  ERRCODE_INVALIDKEYWORD        = 18;     { 0x12 Invalid use of keyword. }
  ERRCODE_CONNECTINUSE          = 19;     { 0x13 Connection in use }
  ERRCODE_CONNECTNOTSHARED      = 20;     { 0x14 Passthru SQL connection not share }


  DBIERR_NOTABLOB               = (ERRBASE_IC + ERRCODE_NOTABLOB);
  DBIERR_BLOBOPENED             = (ERRBASE_IC + ERRCODE_BLOBOPENED);
  DBIERR_BLOBNOTOPENED          = (ERRBASE_IC + ERRCODE_BLOBNOTOPENED);
  DBIERR_NA                     = (ERRBASE_IC + ERRCODE_NA);
  DBIERR_NOTINDEXED             = (ERRBASE_IC + ERRCODE_NOTINDEXED);
  DBIERR_NOTINITIALIZED         = (ERRBASE_IC + ERRCODE_NOTINITIALIZED);
  DBIERR_MULTIPLEINIT           = (ERRBASE_IC + ERRCODE_MULTIPLEINIT);
  DBIERR_NOTSAMESESSION         = (ERRBASE_IC + ERRCODE_NOTSAMESESSION);
  DBIERR_PDXDRIVERNOTACTIVE     = (ERRBASE_IC + ERRCODE_PDXDRIVERNOTACTIVE);
  DBIERR_DRIVERNOTLOADED        = (ERRBASE_IC + ERRCODE_DRIVERNOTLOADED);
  DBIERR_TABLEREADONLY          = (ERRBASE_IC + ERRCODE_TABLEREADONLY);
  DBIERR_NOASSOCINDEX           = (ERRBASE_IC + ERRCODE_NOASSOCINDEX);
  DBIERR_HASOPENCURSORS         = (ERRBASE_IC + ERRCODE_HASOPENCURSORS);
  DBIERR_NOTABLESUPPORT         = (ERRBASE_IC + ERRCODE_NOTABLESUPPORT);
  DBIERR_INDEXREADONLY          = (ERRBASE_IC + ERRCODE_INDEXREADONLY);
  DBIERR_NOUNIQUERECS           = (ERRBASE_IC + ERRCODE_NOUNIQUERECS);
  DBIERR_NOTCURSESSION          = (ERRBASE_IC + ERRCODE_NOTCURSESSION);
  DBIERR_INVALIDKEYWORD         = (ERRBASE_IC + ERRCODE_INVALIDKEYWORD);
  DBIERR_CONNECTINUSE           = (ERRBASE_IC + ERRCODE_CONNECTINUSE);
  DBIERR_CONNECTNOTSHARED       = (ERRBASE_IC + ERRCODE_CONNECTNOTSHARED);


{ ERRCAT_OS }
{ ========= }
{ DOS extended errors: }

  ERRCODE_OSEINVFNC             = 1;      { Invalid function number }
  ERRCODE_OSENOENT              = 2;      { No such file or directory }
  ERRCODE_OSENOPATH             = 3;      { Path not found }
  ERRCODE_OSEMFILE              = 4;      { Too many open files }
  ERRCODE_OSEACCES              = 5;      { Permission denied }
  ERRCODE_OSEBADF               = 6;      { Bad file number }
  ERRCODE_OSECONTR              = 7;      { Memory blocks destroyed }
  ERRCODE_OSENOMEM              = 8;      { Not enough core }
  ERRCODE_OSEINVMEM             = 9;      { Invalid memory block address }
  ERRCODE_OSEINVENV             = 10;     { 0x0a Invalid environment }
  ERRCODE_OSEINVFMT             = 11;     { 0x0b Invalid format }
  ERRCODE_OSEINVACC             = 12;     { 0x0c Invalid access code }
  ERRCODE_OSEINVDAT             = 13;     { 0x0d Invalid data }
  ERRCODE_OSENODEV              = 15;     { 0x0f No such device }
  ERRCODE_OSECURDIR             = 16;     { 0x10 Attempt to remove curdir }
  ERRCODE_OSENOTSAM             = 17;     { 0x11 Not same device }
  ERRCODE_OSENMFILE             = 18;     { 0x12 No more files }
  ERRCODE_OSEINVAL              = 19;     { 0x13 Invalid argument }
  ERRCODE_OSE2BIG               = 20;     { 0x14 Arg list too long }
  ERRCODE_OSENOEXEC             = 21;     { 0x15 Exec format error }
  ERRCODE_OSEXDEV               = 22;     { 0x16 Cross-device link }
  ERRCODE_OSEDOM                = 33;     { 0x21 Math argument }
  ERRCODE_OSERANGE              = 34;     { 0x22 Result to large }
  ERRCODE_OSEEXIST              = 35;     { 0x23 File already exists }
  ERRCODE_OSUNKNOWN             = 39;     { 0x27 Unkown | illegal error from rtl }

  ERRCODE_OSSHAREVIOL           = 50;     { 0x32 Share viol, ext. err 0x20 }
  ERRCODE_OSLOCKVIOL            = 51;     { 0x33 Lock viol, ext. err 0x21 }
  ERRCODE_OSINT24FAIL           = 52;     { 0x34 INT24 called }
  ERRCODE_OSDRIVENOTREADY       = 53;     { 0x35 Drive not ready }



{ OTHER Os errors: }
{ 1. idapi errors  }
{ 2. errors from non-dos systems ( i.e. NOVELL ) }

  ERRCODE_NOTEXACT              = 100;    { 0x64 Not exact read/write }
  ERRCODE_OSNETERR              = 101;    { 0x65 Generic network error }
  ERRCODE_OSUNKNOWNSRVERR       = 102;    { 0x66 Error from file server }
  ERRCODE_SERVERNOMEMORY        = 103;    { 0x67 Server out of memory }
  ERRCODE_OSALREADYLOCKED       = 104;    { 0x68 Record already locked (by you) }
  ERRCODE_OSNOTLOCKED           = 105;    { 0x69 Record not locked }
  ERRCODE_NOSERVERSW            = 106;    { 0x6a Server software not running the workstation/server }


  DBIERR_OSEINVFNC              = ( ERRBASE_OS + ERRCODE_OSEINVFNC );
  DBIERR_OSENOENT               = ( ERRBASE_OS + ERRCODE_OSENOENT );
  DBIERR_OSENOPATH              = ( ERRBASE_OS + ERRCODE_OSENOPATH );
  DBIERR_OSEMFILE               = ( ERRBASE_OS + ERRCODE_OSEMFILE );
  DBIERR_OSEACCES               = ( ERRBASE_OS + ERRCODE_OSEACCES );
  DBIERR_OSEBADF                = ( ERRBASE_OS + ERRCODE_OSEBADF );
  DBIERR_OSECONTR               = ( ERRBASE_OS + ERRCODE_OSECONTR );
  DBIERR_OSENOMEM               = ( ERRBASE_OS + ERRCODE_OSENOMEM );
  DBIERR_OSEINVMEM              = ( ERRBASE_OS + ERRCODE_OSEINVMEM );
  DBIERR_OSEINVENV              = ( ERRBASE_OS + ERRCODE_OSEINVENV );
  DBIERR_OSEINVFMT              = ( ERRBASE_OS + ERRCODE_OSEINVFMT );
  DBIERR_OSEINVACC              = ( ERRBASE_OS + ERRCODE_OSEINVACC );
  DBIERR_OSEINVDAT              = ( ERRBASE_OS + ERRCODE_OSEINVDAT );
  DBIERR_OSENODEV               = ( ERRBASE_OS + ERRCODE_OSENODEV );
  DBIERR_OSECURDIR              = ( ERRBASE_OS + ERRCODE_OSECURDIR );
  DBIERR_OSENOTSAM              = ( ERRBASE_OS + ERRCODE_OSENOTSAM );
  DBIERR_OSENMFILE              = ( ERRBASE_OS + ERRCODE_OSENMFILE );
  DBIERR_OSEINVAL               = ( ERRBASE_OS + ERRCODE_OSEINVAL );
  DBIERR_OSE2BIG                = ( ERRBASE_OS + ERRCODE_OSE2BIG );
  DBIERR_OSENOEXEC              = ( ERRBASE_OS + ERRCODE_OSENOEXEC );
  DBIERR_OSEXDEV                = ( ERRBASE_OS + ERRCODE_OSEXDEV );
  DBIERR_OSEDOM                 = ( ERRBASE_OS + ERRCODE_OSEDOM );
  DBIERR_OSERANGE               = ( ERRBASE_OS + ERRCODE_OSERANGE );
  DBIERR_OSEEXIST               = ( ERRBASE_OS + ERRCODE_OSEEXIST );
  DBIERR_OSUNKNOWN              = ( ERRBASE_OS + ERRCODE_OSUNKNOWN );
  DBIERR_OSSHAREVIOL            = ( ERRBASE_OS + ERRCODE_OSSHAREVIOL );
  DBIERR_OSLOCKVIOL             = ( ERRBASE_OS + ERRCODE_OSLOCKVIOL );
  DBIERR_OSNETERR               = ( ERRBASE_OS + ERRCODE_OSNETERR );
  DBIERR_OSINT24FAIL            = ( ERRBASE_OS + ERRCODE_OSINT24FAIL );
  DBIERR_OSDRIVENOTREADY        = ( ERRBASE_OS + ERRCODE_OSDRIVENOTREADY );


  DBIERR_NOTEXACT               = ( ERRBASE_OS + ERRCODE_NOTEXACT );
  DBIERR_OSUNKNOWNSRVERR        = ( ERRBASE_OS + ERRCODE_OSUNKNOWNSRVERR );
  DBIERR_SERVERNOMEMORY         = ( ERRBASE_OS + ERRCODE_SERVERNOMEMORY );
  DBIERR_OSALREADYLOCKED        = ( ERRBASE_OS + ERRCODE_OSALREADYLOCKED );
  DBIERR_OSNOTLOCKED            = ( ERRBASE_OS + ERRCODE_OSNOTLOCKED );
  DBIERR_NOSERVERSW             = ( ERRBASE_OS + ERRCODE_NOSERVERSW);

{ ERRCAT_NETWORK }
{ ============== }

  ERRCODE_NETINITERR            = 1;      { Net init failed }
  ERRCODE_NETUSERLIMIT          = 2;      { Net user limit exceeded }
  ERRCODE_NETFILEVERSION        = 3;      { Wrong net file version }
  ERRCODE_NETFILELOCKED         = 4;      { Not able to lock net file }
  ERRCODE_DIRNOTPRIVATE         = 5;
  ERRCODE_NETMULTIPLE           = 6;      { Multiple net files in use }
  ERRCODE_NETUNKNOWN            = 7;      { Unknown net error }
  ERRCODE_SHAREDFILE            = 8;      { Cannot access a shared file }
  ERRCODE_SHARENOTLOADED        = 9;      { Share not loaded }
  ERRCODE_NOTONANETWORK         = 10;     { 0x0a Not an Network }
  ERRCODE_SQLCOMMLOST           = 11;     { 0x0b Lost Communication with SQL server }
  ERRCODE_SERVERCOMMLOST        = 12;     { 0x0c Lost Communication with IDAPI server }
  ERRCODE_SQLSERVERNOTFOUND     = 13;     { 0x0d SQL Server not found }
  ERRCODE_SERVERNOTFOUND        = 14;     { 0x0e SQL Server not found }

  DBIERR_NETINITERR             = (ERRBASE_NETWORK + ERRCODE_NETINITERR);
  DBIERR_NETUSERLIMIT           = (ERRBASE_NETWORK + ERRCODE_NETUSERLIMIT);
  DBIERR_NETFILEVERSION         = (ERRBASE_NETWORK + ERRCODE_NETFILEVERSION);
  DBIERR_NETFILELOCKED          = (ERRBASE_NETWORK + ERRCODE_NETFILELOCKED);
  DBIERR_DIRNOTPRIVATE          = (ERRBASE_NETWORK + ERRCODE_DIRNOTPRIVATE);
  DBIERR_NETMULTIPLE            = (ERRBASE_NETWORK + ERRCODE_NETMULTIPLE);
  DBIERR_NETUNKNOWN             = (ERRBASE_NETWORK + ERRCODE_NETUNKNOWN);
  DBIERR_SHAREDFILE             = (ERRBASE_NETWORK + ERRCODE_SHAREDFILE);
  DBIERR_SHARENOTLOADED         = (ERRBASE_NETWORK + ERRCODE_SHARENOTLOADED);
  DBIERR_NOTONANETWORK          = (ERRBASE_NETWORK + ERRCODE_NOTONANETWORK);
  DBIERR_SQLCOMMLOST            = (ERRBASE_NETWORK + ERRCODE_SQLCOMMLOST);
  DBIERR_SERVERCOMMLOST         = (ERRBASE_NETWORK + ERRCODE_SERVERCOMMLOST);
  DBIERR_SQLSERVERNOTFOUND      = (ERRBASE_NETWORK + ERRCODE_SQLSERVERNOTFOUND);
  DBIERR_SERVERNOTFOUND         = (ERRBASE_NETWORK + ERRCODE_SERVERNOTFOUND);

{ ERRCAT_DRIVER }
{ ============= }

  ERRCODE_WRONGDRVNAME          = 1;      { Wrong driver name }
  ERRCODE_WRONGSYSVER           = 2;      { Wrong system version }
  ERRCODE_WRONGDRVVER           = 3;      { Wrong driver version }
  ERRCODE_WRONGDRVTYPE          = 4;      { Wrong driver type }
  ERRCODE_CANNOTLOADDRV         = 5;      { Can not load driver }
  ERRCODE_CANNOTLOADLDDRV       = 6;      { Can not load language driver }
  ERRCODE_VENDINITFAIL          = 7;      { Vendor init failure }
  ERRCODE_DRIVERRESTRICTED      = 8;      { Client not enabled for this driver }


  DBIERR_WRONGDRVNAME           = (ERRBASE_DRIVER + ERRCODE_WRONGDRVNAME);
  DBIERR_WRONGSYSVER            = (ERRBASE_DRIVER + ERRCODE_WRONGSYSVER);
  DBIERR_WRONGDRVVER            = (ERRBASE_DRIVER + ERRCODE_WRONGDRVVER);
  DBIERR_WRONGDRVTYPE           = (ERRBASE_DRIVER + ERRCODE_WRONGDRVTYPE);
  DBIERR_CANNOTLOADDRV          = (ERRBASE_DRIVER + ERRCODE_CANNOTLOADDRV);
  DBIERR_CANNOTLOADLDDRV        = (ERRBASE_DRIVER + ERRCODE_CANNOTLOADLDDRV);
  DBIERR_VENDINITFAIL           = (ERRBASE_DRIVER + ERRCODE_VENDINITFAIL);
  DBIERR_DRIVERRESTRICTED       = (ERRBASE_DRIVER + ERRCODE_DRIVERRESTRICTED);


implementation
{$R DBISTRING.RES}

end.
