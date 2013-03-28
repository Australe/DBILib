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
  1.0 | 01/07/2005 12:24:36 | Jvr | Initial Release
  ______________________________________________________________________________
}

unit DBIXbaseConsts;

interface

uses
  DBIIntfConsts;
  
(*
const
  TDBIXbaseVersionCodes: array[TDBIXbaseVersion] of Byte = (
    $02;             // FoxBase
    $FB;             // FoxBase, with memo

    $03;             // FoxPro, no memo
    $F5;             // FoxPro 2.x (or earlier with memo)

    $30;             // Visual Foxpro
{
    $03;             // dbase-III, no memo
    $83;             // dbase-III, with memo

    $04;             // dbase-IV, no memo
    $8B;             // dbase-IV, with memo
    $43;             // dBase-IV SQL Table files, no memo
    $63;             // dBase-IV SQL System files, no memo
    $83;             // dBase-IV SQL Table files, with memo
    $CB;             // dBase-IV SQL System files, with memo

    $05;             // dbase-V, no memo
}
  );
*)

{##JVR
type
  TDBIXbaseDataTypeProperty = record
    FieldType: Integer;
    FieldSubType: Integer;
    Size: Integer;
    Decimals: Integer;
    FieldLength: Integer;
    Attributes: TFieldAttributes
  );
}

type
  TDBIXbaseCodePage = Byte;
    
const
  // Xbase language values, (Foxpro) Code page
  cpDosUSA          = $01;   // 01h DOS USA code page 437
  cpDosMULTILINGUAL = $02;   // 02h DOS Multilingual code page 850
  cpWindowsANSI     = $03;   // 03h Windows ANSI code page 1251
  cpWindowsEE       = $C8;   // C8h Windows EE code page 1250
  cpDosEE           = $64;   // 64h EE MS-DOS code page 852
  cpDosNORDIC       = $65;   // 65h Nordic MS-DOS code page 865
  cpDosRUSSIAN      = $66;   // 66h Russian MS-DOS code page 866


type
  TDBIXbaseVersion = (
    xbUnknownXbaseVersion,
    xbFoxBase,
    xbFoxPro,
    xbVisualFoxPro,
    xbDbase3,
    xbDbase4,
    xbDbase5
  );


const
  // DataStream Versions                // Byte 0 of XBase Files
  DSVer_FoxBase =           $02;        // FoxBase
  DSVer_FoxBase_Memo =      $FB;        // FoxBase with Memo
  DSVer_FoxPro =            $F5;        // FoxPro with Memo
  DSVer_DBase3 =            $03;        // FoxBase+ & DBase3, no memo
  DSVer_DBase3_Memo =       $83;        // DBase3 with memo
  DSVer_DBase4 =            $04;        // DBase4, no memo
  DSVer_DBase5 =            $05;        // DBase5, no memo
  DSVer_VisualFoxpro =      $30;        // Visual Foxpro

  DSVer_dBaseIVTable =      $43;        // dBase-IV SQL Table files, no memo
  DSVer_dbaseIVSystem =     $63;        // dBase-IV SQL System files, no memo
  DSVer_dbaseIV_Memo =      $83;        // dBase-IV SQL System files, with memo
  DSVer_dbaseIVTable_Memo = $CB;        // dBase-IV SQL System files, with memo


const
  // Xbase Data Types (Physical)
  DT_DOUBLE       = 'B';               { 64 bit floating point }
  DT_CHARACTER    = 'C';               { Null terminated string }
  DT_DATE         = 'D';               { Date     (32 bit) }
  DT_FLOAT        = 'F';               { 64 bit floating point }
  DT_GENERAL      = 'G';               { Blob }
  DT_INTEGER      = 'I';               { 32 bit signed number }
  DT_LOGICAL      = 'L';               { Boolean  (16 bit) }
  DT_MEMO         = 'M';               { Memo }
  DT_NUMERIC      = 'N';               { General Number format }
  DT_DATETIME     = 'T';               { DateTime (32 bit) }
  DT_CURRENCY     = 'Y';               { 64 bit floating point currency }
  DT_NULLFLAGS    = '0';               { Type is Zero (NOT 'O') }
  DT_DELETED      = '*';
  DT_DELFLAG      = '*';               { Deleted Flag Character }
  DT_AUTOINCFLAG  = $FF;               { Value in the decimal field - FieldSize[1] }

  // General Constants
  Xbase_MaxNameLen    = 10;            { Name limit (table, field etc) }
  
  // Locking consts
  // NOTE: A value of $0 indicates: not yet known
  XbaseLockOffsets: array[TDBIXbaseVersion] of LongWord = (
    $00000000, //            - xbUnknownXbaseVersion
    $00000000, // ($02, $FB) - xbFoxBase
    $00000000, // ($F5, $FB) - xbFoxPro
    $7FFFFFFE, // ($30)      - xbVisualFoxPro
    $40000000, // ($03, $83) - xbDbase3
    $00000000, // ($04, $8B) - xbDbase4
    $00000000  // ($05, $CB) - xbDbase5
  );


type
  TDBIXbasePhysicalDataTypes = 'A'..'Z';

type
  TDBIXbaseFieldMap = array[TDBIXbasePhysicalDataTypes] of Integer;

const
  XbaseFieldBaseTypeMap: TDBIXbaseFieldMap = (
    fldUNKNOWN,      // A
    fldFLOAT,        // DT_DOUBLE
    fldZSTRING,      // DT_CHARACTER
    fldDATE,         // DT_DATE
    fldUNKNOWN,      // E
    fldBCD,          // DT_FLOAT
    fldBLOB,         // DT_GENERAL
    fldUNKNOWN,      // H
    fldINT32,        // DT_INTEGER
    fldUNKNOWN,      // J
    fldUNKNOWN,      // K
    fldBOOL,         // DT_LOGICAL
    fldBLOB,         // DT_MEMO
    fldBCD,          // DT_NUMERIC
    fldBYTES,        // DT_NULLFLAGS
    fldUNKNOWN,      // P
    fldUNKNOWN,      // Q
    fldUNKNOWN,      // R
    fldUNKNOWN,      // S
    fldTIMESTAMP,    // DT_DATETIME
    fldUNKNOWN,      // U
    fldUNKNOWN,      // V
    fldUNKNOWN,      // W
    fldUNKNOWN,      // X
    fldFLOAT,        // DT_CURRENCY
    fldUNKNOWN       // Z
    );

  XbaseFieldSubTypeMap: TDBIXbaseFieldMap = (
    fldstNONE,       // A
    fldstNONE,       // DT_DOUBLE
    fldstNONE,       // DT_CHARACTER
    fldstNONE,       // DT_DATE
    fldstNONE,       // E
    fldstNONE,       // DT_FLOAT
    fldstNONE,       // DT_GENERAL
    fldstNONE,       // H
    fldstNONE,       // DT_INTEGER
    fldstNONE,       // J
    fldstNONE,       // K
    fldstNONE,       // DT_LOGICAL
    fldstMEMO,       // DT_MEMO
    fldstNONE,       // DT_NUMERIC
    fldstNONE,       // DT_NULLFLAGS
    fldstNONE,       // P
    fldstNONE,       // Q
    fldstNONE,       // R
    fldstNONE,       // S
    fldstNONE,       // DT_DATETIME
    fldstNONE,       // U
    fldstNONE,       // V
    fldstNONE,       // W
    fldstNONE,       // X
    fldstMONEY,      // DT_CURRENCY
    fldstNONE        // Z
    );

  XbaseFieldLengthMap: TDBIXbaseFieldMap = (
    sizeofUNKNOWN,   // A
    sizeofDOUBLE,    // DT_DOUBLE
    sizeofUNKNOWN,   // DT_CHARACTER
    sizeofDATE,      // DT_DATE
    sizeofUNKNOWN,   // E
    sizeofBCD,       // DT_FLOAT
    sizeofBLOB,      // DT_GENERAL
    sizeofUNKNOWN,   // H
    sizeofINT32,     // DT_INTEGER
    sizeofUNKNOWN,   // J
    sizeofUNKNOWN,   // K
    sizeofBOOL,      // DT_LOGICAL
    sizeofMEMO,      // DT_MEMO
    sizeofBCD,       // DT_NUMERIC
    sizeofNULLFLAGS, // DT_NULLFLAGS
    sizeofUNKNOWN,   // P
    sizeofUNKNOWN,   // Q
    sizeofUNKNOWN,   // R
    sizeofUNKNOWN,   // S
    sizeofDATETIME,  // DT_DATETIME
    sizeofUNKNOWN,   // U
    sizeofUNKNOWN,   // V
    sizeofUNKNOWN,   // W
    sizeofUNKNOWN,   // X
    sizeofCURRENCY,  // DT_CURRENCY
    sizeofUNKNOWN    // Z
    );

    
(*##JVR
const
  TDBIXbaseDataTypeProperties = array['A'..'Z'] of Integer (
    {} ( // A-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
{#} ), ( // DT_DOUBLE
      FieldType: fldFLOAT;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofDOUBLE;
      Attributes: []
{#} ), ( // DT_CHARACTER
      FieldType: fldZSTRING;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
{#} ), ( // DT_DATE
      FieldType: fldDATE;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofDATE;
      Attributes: []
    ), ( // E-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
{#} ), ( // DT_FLOAT
      FieldType: fldBCD;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: SizeOf(TBcd);
      Attributes: []
{#} ), ( // DT_GENERAL
      FieldType: fldBLOB;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofBLOB;
      Attributes: []
    ), ( // H-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
{#} ), ( // DT_INTEGER
      FieldType: fldINT32;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofINTEGER;
      Attributes: []
    ), ( // J-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    ), ( // K-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
{#} ), ( // DT_LOGICAL
      FieldType: fldBOOL;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofBOOL;
      Attributes: []
{#} ), ( // DT_MEMO
      FieldType: fldBLOB;
      FieldSubType: fldstMEMO;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofMEMO;
      Attributes: []
{#} ), ( // DT_NUMERIC
      FieldType: fldBCD;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: SizeOf(TBcd);
      Attributes: []
{#} ), ( // DT_NULLFLAGS
      FieldType: fldBYTES;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: [faReadOnly]
    ), ( // P-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    ), ( // Q-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    ), ( // R-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    ), ( // S-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
{#} ), ( // DT_DATETIME
      FieldType: fldTIMESTAMP;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofDATETIME;
      Attributes: []
    ), ( // U-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    ), ( // V-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    ), ( // W-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    ), ( // X-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
{#} ), ( // DT_CURRENCY
      FieldType: fldFLOAT;
      FieldSubType: fldstMONEY;
      Size: 0;
      Decimals: 0;
      FieldLength: sizeofCURRENCY;
      Attributes: []
    ), ( // Z-Reserved
      FieldType: fldUNKNOWN;
      FieldSubType: fldstNONE;
      Size: 0;
      Decimals: 0;
      FieldLength: 0;
      Attributes: []
    )
  );
*)
implementation

end.
