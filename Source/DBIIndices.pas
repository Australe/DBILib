// _____________________________________________________________________________
{**
  <H5>Copyright</H5> 
  Better Innovative Technical Services Pty Ltd<BR>
  Copyright (C) 1999, All rights reserved.<!--

  Project:       DBILib
  Files(s):      DBIndices.pas
  Classes:       TDBIndex, TDBIndexList
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 17/02/1999 15:12:23 | JVR | Initial Release
  1.1 | 29/05/2002 16:15:48 | JVR | Added Index Filtering
  ______________________________________________________________________________
  </CODE>
}

{#omcodecop off : jvr : native api code}

unit DBIIndices;

interface

{$I DBICompilers.inc}

{$ifdef fpc}
  {$asmmode intel}
  {$mode delphi}
{$endif}

{$define _AGGREGATES True}

// Disable Typed @ operator, Controls the type of pointer returned by @ operator.
{$T-}

uses
  Classes, SysUtils, DB, DBIConst, DBIStrings, DBIIntfConsts, DBIFilters;

const
  LocateCaseMap: array[Boolean] of TLocateOption = (TLocateOption(0), loCaseInsensitive);


type
{$ifdef _AGGREGATES}
  TDBIGroup = class;
  TDBIGroupState = GROUPSTATE;
{$endif}


  // ___________________________________________________________________________
  {**
    NOTES:
    Both Sequence- and Record-Number are 1 based. This includes the parameters,
    as well as the return values.
  }
  TDBIDisplayOrder = (doAscending, doDescending);

  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+

    </CODE>
  }
  TDBIndex = class(TObject)
  private
    FIndexDesc: pDSIDXDesc;                           // iKeyFields is '1' based
    FFilter: TDBIDatasetFilter;

  protected
    function DataFiltered(
      StatusFilter: DSAttr;
      RecordAttribute: DSAttr;
      const RecordBuffer
      ): Boolean;

    function GetCount: Integer; virtual; abstract;
{$ifdef _AGGREGATES}
    function GetGroup(const Level: Integer): TDBIGroup; virtual; abstract;
{$endif}
    function GetName: TDBIString;
    procedure SetName(const Value: TDBIString);
    function GetRecNo(Value: Integer): Integer; virtual; abstract;
    function GetSeqNo(Value: Integer): Integer; virtual; abstract;

    function GetDisplayOrder: TDBIDisplayOrder; virtual; abstract;
    procedure SetDisplayOrder(Value: TDBIDisplayOrder); virtual; abstract;

    function GetStrings: TDBIStrings; virtual; abstract;

    { Indices }
    procedure InternalDelete(
      const pRecBuf;
      RecordNumber: Integer
      ); virtual; abstract;

    function InternalInsert(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; virtual; abstract;

    function InternalModify(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; virtual; abstract;

  public
    constructor Create(PIndexDesc: pDSIDXDesc); virtual;
    destructor Destroy; override;

    procedure DeleteItem(
      const pRecBuf;
      const RecordNumber: Integer
      );

    function InsertItem(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer;

    function ModifyItem(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer;

    function IsFieldInIndex(const FieldId: Integer): Boolean; virtual;
    function Locate(
      PKeyValues: pZStringList;
      PIndexDesc: pDSIDXDesc;
      var Index: Integer
      ): Boolean; virtual;

{$ifdef _AGGREGATES}
    function GetGroupLevel: Integer; virtual;

    function GetDebugInfo(const Position: Integer): String; virtual;

    function GetSubGroupCount(
      const Level: Integer;
      const Position: Integer
      ): Integer; virtual;

    function GetSubGroupState(
      const Level: Integer;
      const Position: Integer
      ): TDBIGroupState; virtual;

    procedure SetGroupLevel(const Value: Integer); virtual;
{$endif}

    property Count: Integer read GetCount;
    property PIndexDesc: pDSIDXDesc read FIndexDesc;  // iKeyFields is '1' based
    property Name: TDBIString read GetName write SetName;
    property DisplayOrder: TDBIDisplayOrder
      read GetDisplayOrder
      write SetDisplayOrder;
    property Filter: TDBIDatasetFilter read FFilter write FFilter;
    property Items: TDBIStrings read GetStrings;
    property RecNo[Value: Integer]: Integer read GetRecNo;
    property SeqNo[Value: Integer]: Integer read GetSeqNo;

{$ifdef _AGGREGATES}
    property Groups[const Level: Integer]: TDBIGroup read GetGroup;
    property GroupLevel: Integer read GetGroupLevel write SetGroupLevel;
{$endif}
  end;


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+     +---------------+
    |  TDBIndexList   |<>---|     TList     |
    | <DBIndices.pas> |     | <Classes.pas> |
    +-----------------+     +---------------+

    </CODE>
  }
  TDBIIndexList = class(TObject)
  private
    FIndexList: TList;
    FIndexDescs: TIndexDescList;        // Array of Index description properties
    FFieldsUpdated: TList;              // List of fields updated in the last
                                        // Add/Update (Edit/Insert)

  protected
    procedure Error(
      E: Exception;
      const Caller: String;
      const Reference: String;
      const ErrMsg: String;
      Args: array of const
      );

    function GetCount: Integer;

    function GetIndex(Index: Integer): TDBIndex;
    procedure SetIndex(Index: Integer; Value: TDBIndex);

    function GetIndexByName(Name: TDBIString): TDBIndex;
    procedure SetIndexByName(Name: TDBIString; Value: TDBIndex);

    function GetDescs(Index: Integer): DSIDXDesc;
    procedure SetDescs(Index: Integer; Value: DSIDXDesc);

    procedure ResyncIndexDescs;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Clear;
    function CreateIndex(IndexType: Integer; const IndexDesc: DSIDXDesc): TDBIndex;
    procedure DeleteIndex(Index: Integer);
    function FindMatchingIndex(Selected: Integer; PIndexDesc: pDSIDXDesc): Integer;
    function IndexOf(IndexName: TDBIIndexName): Integer;
    function IndexOfIndex(DBIndex: TDBIndex): Integer;

    // Methods to Delete/Insert/Modify entries to all indices
    procedure DeleteIndicesItem(const RecNo: Integer; pRecBuf: Pointer);
    function InsertIndicesItem(const RecNo: Integer; pRecBuf: Pointer): Integer;
    function ModifyIndicesItem(const RecNo: Integer; pRecBuf: Pointer): Integer;

    procedure AddUpdatedField(pFieldDesc: pDSFLDDesc);

    property Count: Integer read GetCount;
    property Descs: TIndexDescList read FIndexDescs write FIndexDescs;
    property Indices[Index: Integer]: TDBIndex read GetIndex write SetIndex; default;
    property IndicesByName[Name: TDBIString]: TDBIndex
      read GetIndexByName
      write SetIndexByName;

    property FieldsUpdated: TList read FFieldsUpdated write FFieldsUpdated;

  end;  { TDBIndexList }


  // ___________________________________________________________________________
  {**
    <CODE>

       +---------+
       | TObject |
       +---------+
            |
            |
    +-------------------+
    | TDBIStringList    |
    | <CDBIStrings.pas> |
    +-------------------+
            |
            |
   +-----------------------+
   | TDBICustomIndexItems  |
   | <DBIndices.pas>       |
   +-----------------------+

    </CODE>
  }
  TDBICompareFunction = function(
    const KeyValue: TDBIString;
    const CompareValue: TDBIString;
    bPartialKey:Boolean;
    bCaseInsensitive: Boolean
    ): Integer of object;


  TDBICustomIndexItems = class(TDBIStringList)
  private
    FIndexOptions: TIndexOptions;
    FCompare: TDBICompareFunction;

  protected
    function GetString(Index: Integer; bReverseOrder: Boolean): TDBIString;

    function InternalCompare(
      const KeyValue: TDBIString;
      const CompareValue: TDBIString;
      bPartialKey: Boolean;
      bCaseInsensitive: Boolean;
      bDescending: Boolean
      ): Integer;

    function InternalFind(
      const Value: TDBIString;
      var Index: Integer;
      bPartialKey: Boolean;
      bCaseInsensitive: Boolean;
      bDescending: Boolean;
      bReverseOrder: Boolean
      ): Boolean;

  public
    function StringCompare(
      const KeyValue: TDBIString;
      const CompareValue: TDBIString;
      bPartialKey: Boolean;
      bCaseInsensitive: Boolean
      ): Integer;

    function Find(const S: TDBIString; var Index: Integer): Boolean; override;
    function ModifyItem(ItemNo: Integer; const Value: TDBIString): Integer;

    property IndexOptions: TIndexOptions read FIndexOptions write FIndexOptions;
    property CompareFunction: TDBICompareFunction read FCompare write FCompare;
  end;  { TDBICustomIndexItems }




{$ifdef _AGGREGATES}
  // ___________________________________________________________________________
  {**
    <CODE>

    +-----------------+                           +----------------------+
    | Index: TDBIndex |-------------------------->| Items: TDBIStingList |
    |                 |                           +----------------------+
    |                 |                                  |
    |                 |   +-----------------------+      | Sequ -> Key -> RecNo
    |                 |   | Groups: TDBIGroupList |      + -- 0 -> abc -> ?
    |                 |-->|                       |      + -- 1 -> def -> ?
    +-----------------+   | Items: TList          |      + -- n -> xxx -> ?
                          +-----------------------+
                            |
                            | Group-level
                            + - 0     +-----------------------+
                            + - n --->| Group: TDBIGroup      |
                                      |                       |
                                      | Items: TDBICustomIndexItems |
                                      |      (sorted, nodups) |
                                      +-----------------------+
                                        |
                                        | Sequ -> Key -> SubGroup
                                        + -- 0 -> abc
                                        + -- 1 -> def     +------------------+
                                        + -- n -> xxx --->| TDBISubGroupList |
                                                          |                  |
                                                          | Items: TList     |
                                                          +------------------+
                                                            |
                                                            | Sequ -> Item-Pos
                                                            + -- 0 -> ?
                                                            + -- 1 -> ?
                                                            + -- n -> ?

  GROUPSTATE = (
    grSTATEMIDDLE,      // Record is neither the first or the last in the group
    grSTATEFIRST,       // Record is the first in the group
    grSTATELAST,        // Record is the last in the group
    grSTATEFIRSTLAST
  );

  Key := Index.Items[Position];
  Group := Index.Groups[GroupLevel];
  SubGroupIndex := Group.IndexOf(Key);
  SubGroup := Group[SubGroupIndex];
  SubGroupIndex := SubGroup.IndexOf(TObject(Position));

  if (SubGroupIndex < 0) then
    SubGroupState := grSTATEFIRSTLAST (not found)
  else if (SubGroupIndex = 0) then
    SubGroupState := grSTATEFIRST
  else if (SubGroupIndex = Count-1) then
    SubGroupState := grSTATELAST
  else
    SubGroupState := grSTATEMIDDLE;


    </CODE>
  }
  TDBISubGroupZZ = class
  private
    FItems: TList;

  protected
    function GetSubGroupState(const Position: Integer): TDBIGroupState;

  public
    constructor Create;
    destructor Destroy; override;

  end;  { TDBISubGroup }


  TDBISubGroup = TList; // TDBICustomIndexItems;
  TDBISubGroupList = TDBICustomIndexItems;

  TDBIGroup = class
  private
    FItems: TDBISubGroupList;

  protected
    function AddSubGroupItem(const Key: TDBIString; const RecNo: Integer): Integer;
    function CheckSubGroup(const Key: TDBIString): Integer;
    function GetDebugInfo(const Key: TDBIString; const RecNo: Integer): String;
    function GetSubGroupItemCount(const Key: TDBIString; const RecNo: Integer): Integer;
    function GetSubGroupItemState(const Key: TDBIString; const RecNo: Integer): TDBIGroupState;
    procedure RemoveSubGroupItem(const Key: TDBIString; const RecNo: Integer);

  public
    constructor Create(PIndexDesc: pDSIDXDesc);
    destructor Destroy; override;


  end;


  TDBIGroupList = class(TObject)
  private
    FItems: TList;

  protected
    function GetGroup(const Index: Integer): TDBIGroup;

    function GetLevel: Integer;
    procedure SetLevel(const Value: Integer);

  public
    constructor Create(PIndexDesc: pDSIDXDesc);
    destructor Destroy; override;

    property GroupLevel: Integer read GetLevel write SetLevel;
    property Groups[const Index: Integer]: TDBIGroup read GetGroup; default;

  end;  { TDBIGroupList }

{$endif}


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIDefaultIndex  |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBIDefaultIndex = class(TDBIndex)
  private
    FCount: Integer;
    
  protected
    function GetCount: Integer; override;
    function GetRecNo(Value: Integer): Integer; override;
    function GetSeqNo(Value: Integer): Integer; override;

{$ifdef _AGGREGATES}
    function GetGroup(const Index: Integer): TDBIGroup; override;
{$endif}
    function GetStrings: TDBIStrings; override;

    function GetDisplayOrder: TDBIDisplayOrder; override;
    procedure SetDisplayOrder(Value: TDBIDisplayOrder); override;

    procedure InternalDelete(
      const pRecBuf;
      RecordNumber: Integer
      ); override;

    function InternalInsert(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; override;

    function InternalModify(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; override;

  public
    procedure SetCount(const Value: Integer);

  end;  { TDBIDefaultIndex }


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIStandardIndex |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBIStandardIndex = class(TDBIndex)
  private
{$ifdef _AGGREGATES}
    FGroups: TDBIGroupList;
{$endif}
    FItems: TDBICustomIndexItems;
    FDisplayOrder: TDBIDisplayOrder;

    function GetFieldAsString(const RecordBuffer; pFieldDesc: PDSFLDDesc): TDBIString;

  protected
    procedure Error(
      E: Exception;
      const Caller: String;
      const Reference: String;
      const ErrMsg: String;
      Args: array of const
      );

    function GetCount: Integer; override;
    function GetRecNo(Value: Integer): Integer; override;
    function GetSeqNo(Value: Integer): Integer; override;

    function GetDisplayOrder: TDBIDisplayOrder; override;
    procedure SetDisplayOrder(Value: TDBIDisplayOrder); override;

{$ifdef _AGGREGATES}
    function GetGroup(const Index: Integer): TDBIGroup; override;
{$endif}
    function GetStrings: TDBIStrings; override;

    procedure InternalDelete(
      const pRecBuf;
      RecordNumber: Integer
      ); override;

    function InternalInsert(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; override;

    function InternalModify(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; override;

  public
    constructor Create(PIndexDesc: pDSIDXDesc); override;
    destructor Destroy; override;

    function IsFieldInIndex(const FieldId: Integer): Boolean; override;
    function Locate(
      pKeyValues: PZStringList;
      pLocateDesc: pDSIDXDesc;
      var Index: Integer
      ): Boolean; override;

{$ifdef _AGGREGATES}
    function GetGroupLevel: Integer; override;

    function GetDebugInfo(const Position: Integer): String; override;

    function GetSubGroupCount(
      const Level: Integer;
      const Position: Integer
      ): Integer; override;

    function GetSubGroupState(
      const Level: Integer;
      const Position: Integer
      ): TDBIGroupState; override;

    procedure SetGroupLevel(const Value: Integer); override;
{$endif}

  end;  { TDBIStandardIndex }


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIStandardIndex |
   |  <DBIndices.pas>  |
   +-------------------+
             |
             |
   +-------------------+
   | TDBIPrimaryIndex  |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBINaturalOrderIndex = class(TDBIStandardIndex)
  protected
  {##JVR
    function GetCount: Integer; override;
    function GetRecNo(const Value: Integer): Integer; override;
    function GetSeqNo(const Value: Integer): Integer; override;

    function GetStrings: TDBIStrings; override;

    function GetDisplayOrder: TDBIDisplayOrder; override;
    procedure SetDisplayOrder(Value: TDBIDisplayOrder); override;

    procedure InternalDelete(
      const pRecBuf;
      RecordNumber: Integer
      ); override;
}
    function InternalInsert(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; override;

    function InternalModify(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecordNumber: Integer
      ): Integer; override;

  public
    constructor Create(PIndexDesc: pDSIDXDesc); override;

  end;  { TDBIPrimaryIndex }


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIStandardIndex |
   |  <DBIndices.pas>  |
   +-------------------+
             |
             |
   +-------------------+
   |  TDBIStringIndex  |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBIStringIndex = class(TDBIStandardIndex)
  end;  { TDBIStringIndex }


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIStandardIndex |
   |  <DBIndices.pas>  |
   +-------------------+
             |
             |
   +-------------------+
   | TDBIBooleanIndex  |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBIBooleanIndex = class(TDBIStandardIndex)
  end;  { TDBIBooleanIndex }


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIStandardIndex |
   |  <DBIndices.pas>  |
   +-------------------+
             |
             |
   +-------------------+
   |   TDBIDateIndex   |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBIDateIndex = class(TDBIStandardIndex)
  public
    function Locate(
      pKeyValues: PZStringList;
      pLocateDesc: pDSIDXDesc;
      var Index: Integer
      ): Boolean; override;

  end;  { TDBIDateIndex }


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIStandardIndex |
   |  <DBIndices.pas>  |
   +-------------------+
             |
             |
   +-------------------+
   |  TDBINumberIndex  |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBINumberIndex = class(TDBIStandardIndex)
  protected
    function NumberCompare(
      const KeyValue: TDBIString;
      const CompareValue: TDBIString;
      bPartialKey: Boolean;
      bCaseInsensitive: Boolean
      ): Integer;

  public
    constructor Create(PIndexDesc: pDSIDXDesc); override;
    function Locate(
      pKeyValues: PZStringList;
      pLocateDesc: pDSIDXDesc;
      var Index: Integer
      ): Boolean; override;

  end;  { TDBINumberIndex }


  // ___________________________________________________________________________
  {**
    <CODE>

        +---------+
        | TObject |
        +---------+
             |
             |
    +-----------------+
    |    TDBIndex     |
    | <DBIndices.pas> |
    +-----------------+
             |
             |
   +-------------------+
   | TDBIStandardIndex |
   |  <DBIndices.pas>  |
   +-------------------+
             |
             |
   +-------------------+
   | TDBIIntegerIndex  |
   |  <DBIndices.pas>  |
   +-------------------+

    </CODE>
  }
  TDBIIntegerIndex = class(TDBIStandardIndex)
  protected
    function InternalInsert(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecNo: Integer
      ): Integer; override;

    function InternalModify(
      const pRecBuf;
      pFieldDesc: pDSFLDDesc;
      RecNo: Integer
      ): Integer; override;

  public
    constructor Create(PIndexDesc: pDSIDXDesc); override;

  end;  { TDBIIntegerIndex }





implementation

uses
  {$ifndef fpc} Consts, {$endif} Windows, Dialogs, TypInfo;

const
  UnitName = 'DBIIndices';


// =============================================================================
// TDBIndex public methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 16:15:48 - Added Filtering<P>
}
constructor TDBIndex.Create(PIndexDesc: pDSIDXDesc);
begin
  FIndexDesc := PIndexDesc;
  FFilter := nil;

  inherited Create;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 31/05/2002 13:39:23.<P>
}
destructor TDBIndex.Destroy;
begin
  FFilter.Free;
  FFilter := nil;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 16:16:37 - Added Index Filtering<BR>
  Jvr - 22/07/2002 14:34:17 - 1st passed in parameter changed SeqNo -> RecNo<P>
}
procedure TDBIndex.DeleteItem(
  const pRecBuf;
  const RecordNumber: Integer
  );
begin
  if DataFiltered(dsRecAll, dsRecAll, pRecBuf) then begin
    InternalDelete(pRecBuf, RecordNumber);
  end;
end;  { DeleteItem }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 16:17:18 - Added Index Filtering<P>
}
function TDBIndex.InsertItem(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
begin
  Result := -1;

  if DataFiltered(dsRecAll, dsRecAll, pRecBuf) then begin
    Result := InternalInsert(pRecBuf, pFieldDesc, RecordNumber);
  end;
end;  { InsertItem }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 16:18:34 - Added Index Filtering<P>
}
function TDBIndex.ModifyItem(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
begin
  Result := -1;

  if DataFiltered(dsRecAll, dsRecAll, pRecBuf) then begin
    Result := InternalModify(pRecBuf, pFieldDesc, RecordNumber);
  end;
end;  { ModifyItem }


// _____________________________________________________________________________
{**
  Jvr - 30/05/2002 12:33:25.<P>
}
function TDBIndex.IsFieldInIndex(const FieldId: Integer): Boolean;
begin
  Result := False;
end;  { IsFieldInIndex }


// _____________________________________________________________________________
{**
  Jvr - 30/05/2002 12:32:24.<P>
}
function TDBIndex.Locate(
  PKeyValues: pZStringList;
  PIndexDesc: pDSIDXDesc;
  var Index: Integer
  ): Boolean;
begin
  Result := False;
end;  { Locate }


{$ifdef _AGGREGATES}
// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 15:13:56 - Initial code.<br />
}
function TDBIndex.GetGroupLevel: Integer;
begin
  Result := 0;
end;  { GetGroupLevel }


// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 13:21:36 - Initial code.<br />
}
function TDBIndex.GetDebugInfo(const Position: Integer): String;
begin
  Result := Format('Seq[%-4.4d] RecNo[%-4.4d]', [Position-1, RecNo[Position]-1]);
end;  { GetDebugInfo }


// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 22:57:38 - Initial code.<br />
}
function TDBIndex.GetSubGroupCount(
  const Level: Integer;
  const Position: Integer
  ): Integer;
begin
  Result := Count;
end;  { GetSubGroupCount }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 15:03:39 - Initial code.<br />
}
function TDBIndex.GetSubGroupState(
  const Level: Integer;
  const Position: Integer
  ): TDBIGroupState;
begin
  Result := grSTATEFIRST;
end;  { GetSubGroupState }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 15:15:06 - Initial code.<br />
}
procedure TDBIndex.SetGroupLevel(const Value: Integer);
begin
  // NOP
end;  { SetGroupLevel }


{$endif}



// =============================================================================
// 'TDBIndex' protected methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 16:29:21.<P>
}
function TDBIndex.DataFiltered(
  StatusFilter: DSAttr;
  RecordAttribute: DSAttr;
  const RecordBuffer
  ): Boolean;
begin
  Result := (FFilter = nil) or FFilter.Filter(
    PDBIChar(RecordBuffer),
    RecordAttribute,
    StatusFilter
    );
end;  { DataFiltered }


// _____________________________________________________________________________
{**
  Jvr - 30/05/2002 13:00:03.<P>
}
function TDBIndex.GetName: TDBIString;
begin
  Result := TDBIString(FIndexDesc^.szName);
end;  { GetName }


// _____________________________________________________________________________
{**
  Jvr - 30/05/2002 13:01:53.<P>
}
procedure TDBIndex.SetName(const Value: TDBIString);
begin
  StrLCopy(FIndexDesc^.szName, PDBIChar(Value), SizeOf(FIndexDesc^.szName));
end;  { SetName }




// =============================================================================
// 'TDBIndexList' public methods
// =============================================================================

// _____________________________________________________________________________
{**
  ..<P>
}
constructor TDBIIndexList.Create;
begin
  inherited Create;

  FFieldsUpdated := TList.Create;
  FIndexList := TList.Create;
end;  { Create }


// _____________________________________________________________________________
{**
  ..<P>
}
destructor TDBIIndexList.Destroy;
var
  Index: Integer;
  Item: TDBIndex;

begin
  for Index := FIndexList.Count-1 downto 0 do begin
    Item := FIndexList.Items[Index];
    FIndexList.Items[Index] := nil;
    FIndexList.Delete(Index);
    Item.Free;
  end;  { for }

  FIndexList.Clear;
  FIndexList.Free;
  FIndexList := nil;

  SetLength(FIndexDescs, 0);

  FFieldsUpdated.Free;
  FFieldsUpdated := nil;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 23/01/2001 11:22:00 - Clear all indices except the primary index.
}
procedure TDBIIndexList.Clear;
var
  Index: Integer;
  Item: TDBIndex;

begin
  try
    for Index := FIndexList.Count-1 downto 1 do begin
      Item := FIndexList.Items[Index];
      FIndexList.Items[Index] := nil;
      FIndexList.Delete(Index);
      Item.Free;
    end;  { for }

    SetLength(FIndexDescs, 1);

  except
    on E: Exception do begin
      Error(E, 'Clear', '1485', 'Failed to clear the indicies', []);
    end;
  end;
end;  { Clear }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIIndexList.CreateIndex(IndexType: Integer; const IndexDesc: DSIDXDesc): TDBIndex;
var
  Index: Integer;

begin
  Index := Length(FIndexDescs);
  SetLength(FIndexDescs, Index + 1);
  FillChar(FIndexDescs[Index], SizeOf(DSIDXDesc), 0);
  FIndexDescs[Index] := IndexDesc;

  case IndexType of
    fldUNKNOWN: begin
      if (FIndexList.Count = 0) then begin
        FIndexList.Add(TDBIDefaultIndex.Create(@FIndexDescs[Index]));
      end
      else begin
        FIndexList.Add(TDBINaturalOrderIndex.Create(@FIndexDescs[Index]));
      end;
    end;  { fldUNKNOWN }

//##JVR    fldUNKNOWN: FIndexList.Add(TDBIPrimaryIndex.Create(@FIndexDescs[Index]));
    fldBOOL: FIndexList.Add(TDBIBooleanIndex.Create(@FIndexDescs[Index]));
    fldDATE: FIndexList.Add(TDBIDateIndex.Create(@FIndexDescs[Index]));
//##JVR    fldINTEGER: FIndexList.Add(TDBIIntegerIndex.Create(@FIndexDescs[Index]));
//##JVR    fldNUMBER: FIndexList.Add(TDBIIntegerIndex.Create(@FIndexDescs[Index]));
    fldINT16: FIndexList.Add(TDBIIntegerIndex.Create(@FIndexDescs[Index]));
    fldINT32: FIndexList.Add(TDBIIntegerIndex.Create(@FIndexDescs[Index]));
    fldINT64: FIndexList.Add(TDBIIntegerIndex.Create(@FIndexDescs[Index]));
    fldZSTRING: FIndexList.Add(TDBIStringIndex.Create(@FIndexDescs[Index]));

  else
    Error(nil, 'CreateIndex', '1520',
      'Failed to Create Index %s, "%d" type indices not supported',
      [IndexDesc.szName, IndexType]
    );
  end;  { case }

  ResyncIndexDescs;
  Result := FIndexList.Items[FIndexList.Count-1]; //##JVR Index];
end;  { CreateIndex }


// _____________________________________________________________________________
{**
  ..<P>
}
procedure TDBIIndexList.DeleteIndex(Index: Integer);
var
  Delta: Integer;
  Item: TDBIndex;

begin
  try
    Item := FIndexList.Items[Index];
    FIndexList.Delete(Index);
    Item.Free;
//##JVR    Item := nil;

    Delta := Count - Index;
    if (Delta > 0) then begin
      Move((@FIndexDescs[Index+1])^, (@FIndexDescs[Index])^, SizeOf(DSIDXDesc) * Delta);
    end;

    SetLength(FIndexDescs, Count);
    ResyncIndexDescs;

  except
    on E: Exception do begin
      Error(E, 'DeleteIndex', '935', 'Failed to Delete Index %d', [Index]);
    end;
  end;
end;  { DeleteIndex }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIIndexList.FindMatchingIndex(
  Selected: Integer;
  PIndexDesc: pDSIDXDesc
  ): Integer;
var
  Index: Integer;

  function CheckFields(IndexNo: Integer): Boolean;
  var
    FieldCount: Integer;

  begin
    Result := False;
    if (PIndexDesc^.iFields > FIndexDescs[IndexNo].iFields) then Exit;

    for FieldCount := 0 to PIndexDesc^.iFields -1 do begin
      Result := PIndexDesc^.iKeyFields[FieldCount] =
        FIndexDescs[IndexNo].iKeyFields[FieldCount];

      if not Result then Break;
    end;  { for }
  end;  { DoFieldsMatch }

  function CheckOrder(IndexNo: Integer): Boolean;
  begin
    Result := True;
  end;  { CheckDescendingMatch }

  function CheckCase(IndexNo: Integer): Boolean;
  begin
    Result := True;
  end;  { CheckCaseInsensitiveMatch }

begin
  Result := 0;

  if CheckFields(Selected) and CheckCase(Selected) and CheckOrder(Selected) then
  begin
    Result := Selected;
    Exit;
  end;

  for Index := 0 to FIndexList.Count - 1 do begin
    if CheckFields(Index) and CheckCase(Index) and CheckOrder(Index) then begin
      Result := Index;
      Break;
    end;
  end;  { for }
end;  { FindMatchingIndex }


// _____________________________________________________________________________
{**
  Jvr - 28/10/2010 12:47:58 - Updated code.<br />

  Swapped StrComp for the case insensitive StrIComp
}
function TDBIIndexList.IndexOf(IndexName: TDBIIndexName): Integer;
var
  Index: Integer;
  KeyName: String;

begin
  Result := -1;

  for Index := 0 to FIndexList.Count-1 do begin
    KeyName := TDBIFieldName(FIndexDescs[Index].szName);
    if (StrLIComp(TDBIIndexName(KeyName), IndexName, Length(KeyName)) = 0) then begin
      Result := Index;
      Break;
    end;
  end;  { for }
end;  { IndexOf }


// _____________________________________________________________________________
{**
  Jvr - 31/05/2002 15:13:35.<P>
}
function TDBIIndexList.IndexOfIndex(DBIndex: TDBIndex): Integer;
begin
  Result := FIndexList.IndexOf(DBIndex);
end;  { IndexOfIndex }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 17:47:42.<BR>
  Jvr - 22/07/2002 14:33:09 - 1st passed in parameter changed SeqNo -> RecNo<P>
}
procedure TDBIIndexList.DeleteIndicesItem(
  const RecNo: Integer;
  pRecBuf: Pointer
  );
var
  Index: Integer;
  DBIndex: TDBIndex;

begin
  for Index := 0 to FIndexList.Count-1 do begin
    DBIndex := FIndexList.Items[Index];
//##JVR    if (RecNo <= DBIndex.Count) then begin
      DBIndex.DeleteItem(pRecBuf, RecNo);
//##JVR    end;
  end;  { for }

  FieldsUpdated.Clear;
end;  { DeleteIndicesItem }


// _____________________________________________________________________________
{**
  InsertIndicesItem Iterates first through the Indices base on the DefaultIndex
  including the default index and adds an entry to each one at the end.
  Then we iterate through the rest of the indices, making sure that any index
  selected, is NOT based on the Default index. For each selected index we add
  an entry (including filters) that has the field, for which we are updating,
  included in it's index definition.<P>

  Jvr - 21/03/2000 15:23:49.<P>
  Jvr - 17/06/2002 14:07:19 - Now handles all indices based on the DefaultIndex
  Jvr - 22/07/2002 12:30:17 - FieldsUpdated is now on the IndexList<P>
}
function TDBIIndexList.InsertIndicesItem(
  const RecNo: Integer;
  pRecBuf: Pointer
  ): Integer;
var
  ItemNo: Integer;
  Index: Integer;
  DBIndex: TDBIndex;
  pFieldDesc: PDSFLDDesc;

begin
  Result := RecNo;

  // 'pFieldDesc' has no purpose for indices based on the DefaultIndex
  pFieldDesc := nil;

  // Insert an entry into all indices based on the DefaultIndex
  for Index := 0 to FIndexList.Count-1 do begin
    DBIndex := FIndexList.Items[Index];

    // Add Value to DefaultIndex (DEFAULT_ORDER = Record Number)
    // and all indices based on the DefaultIndex
    if (DBIndex.FIndexDesc.iFields = 0) and (DBIndex.FIndexDesc.iKeyLen = 0) then begin
      DBIndex.InsertItem(pRecBuf, pFieldDesc, RecNo);
    end;  { if }
  end;  { for }


  // Add Value to Secondary Indices
  for ItemNo := 0 to FieldsUpdated.Count - 1 do begin
    pFieldDesc := pDSFLDDesc(FieldsUpdated[ItemNo]);

    // Start at Index = 1 because the DefaultIndex (Index = 0) has already been done
    for Index := 1 to FIndexList.Count-1 do begin
      DBIndex := FIndexList.Items[Index];

      // If it's an index based on the DefaultIndex then skip it
      if (DBIndex.FIndexDesc.iFields = 0) and (DBIndex.FIndexDesc.iKeyLen = 0) then begin
        Continue;
      end;

      // If the field is part of the Index then Insert the item
      if DBIndex.IsFieldInIndex(pFieldDesc^.iFieldID) then begin
        DBIndex.InsertItem(pRecBuf, pFieldDesc, RecNo);
      end;
    end;  { for }
  end;  { for }

  FieldsUpdated.Clear;
end;  { InsertIndicesItem }


// _____________________________________________________________________________
{**
  NOTE: SelectedIndex is NOT used and probably can be removed.
  In modifyIndecesItem we iterate through the Indices starting at Index One
  because index zero is the Default index and needs no updating.  Any Filters
  based on the Default require no updating but we update then at the moment
  anyway because it doesn't change anything so who cares.<P>

  Jvr - 31/05/2002 11:32:18.<BR>
  Jvr - 22/07/2002 12:29:15 - FieldsUpdated is now on the IndexList<P>
}
function TDBIIndexList.ModifyIndicesItem(
  const RecNo: Integer;
  pRecBuf: Pointer
  ): Integer;
var
  ItemNo: Integer;
  Index: Integer;
  DBIndex: TDBIndex;
  pFieldDesc: PDSFLDDesc;

begin
  Result := RecNo;

  for ItemNo := 0 to FieldsUpdated.Count - 1 do begin
    pFieldDesc := pDSFLDDesc(FieldsUpdated[ItemNo]);

    // Start at Index = 1 because the primary index (Index = 0) needs no updating
    for Index := 1 to FIndexList.Count-1 do begin
      DBIndex := FIndexList.Items[Index];
      if DBIndex.IsFieldInIndex(pFieldDesc^.iFieldID) then begin
        DBIndex.ModifyItem(pRecBuf, pFieldDesc, RecNo);
      end;  { if }
    end;  { for }
  end;  { for }

  FieldsUpdated.Clear;
end;  { ModifyIndicesItem }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 12:02:46.<P>
}
procedure TDBIIndexList.AddUpdatedField(pFieldDesc: pDSFLDDesc);
begin
  // Update Indices Modification List
  if (FieldsUpdated.IndexOf(TObject(pFieldDesc)) = -1) then begin
    FieldsUpdated.Add(TObject(pFieldDesc));
  end;
end;  { AddUpdatedField }



// =============================================================================
// 'TDBIndexList' protected methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 02/04/01 15:52:44.<P>
}
procedure TDBIIndexList.Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
begin
{$IFDEF DebugExceptions}
  raise EDBIException.CreateFmt(
    UnitName + '::' + Self.ClassName + '::' + Caller + '::' + Reference + #13 + ErrMsg, Args);
{$ELSE}
  raise EDBIException.CreateFmt(ErrMsg, Args);
{$ENDIF DebugExceptions}
end;  { Error }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIIndexList.GetCount: Integer;
begin
  Result := FIndexList.Count;
end;  { GetCount }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIIndexList.GetDescs(Index: Integer): DSIDXDesc;
begin
  Result := FIndexDescs[Index];
end;  { GetDescs }


// _____________________________________________________________________________
{**
  ..<P>
}
procedure TDBIIndexList.SetDescs(Index: Integer; Value: DSIDXDesc);
begin
  FIndexDescs[Index] := Value;
end;  { SetDescs }


// _____________________________________________________________________________
{**
  Resync the TDBIndex Description pointers (TDBIndex::FindexDesc) to the
  address of the matching records in the TDBIndexList::FIndexDescs array.

  Jvr - 03/06/2002 16:35:10.<P>
}
procedure TDBIIndexList.ResyncIndexDescs;
var
  IndexNo: Integer;
  DBIndex: TDBIndex;

begin
  for IndexNo := 0 to Count -1 do begin
    DBIndex := TDBIndex(FIndexList[IndexNo]);
    DBIndex.FIndexDesc := @(FIndexDescs[IndexNo]);
  end;
end;  { UpdateDescs }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIIndexList.GetIndex(Index: Integer): TDBIndex;
begin
  Result := TDBIndex(FIndexList.Items[Index]);
end;  { GetIndex }


// _____________________________________________________________________________
{**
  ..<P>
}
procedure TDBIIndexList.SetIndex(Index: Integer; Value: TDBIndex);
begin
  FIndexList.Items[Index] := Value;
end;  { SetIndex }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIIndexList.GetIndexByName(Name: TDBIString): TDBIndex;
var
  Index: Integer;

begin
  Result := nil;

  for Index := 0 to FIndexList.Count - 1 do begin
    if (StrLIComp(FIndexDescs[Index].szName, PDBIChar(Name), SizeOf(FIndexDescs[Index].szName)) = 0) then begin
      Result := FIndexList[Index];
      Exit;
    end;
  end;

  Error(nil, 'GetIndexByName', '1820', 'Illegal index name: ', [Name]);
end;  { GetIndexByName }


// _____________________________________________________________________________
{**
  ..<P>
}
procedure TDBIIndexList.SetIndexByName(Name: TDBIString; Value: TDBIndex);
var
  Index: Integer;

begin
  for Index := 0 to FIndexList.Count - 1 do begin
    if (StrLIComp(FIndexDescs[Index].szName, PDBIChar(Name), SizeOf(FIndexDescs[Index].szName)) = 0) then begin
      FIndexList[Index] := Value;
      Exit;
    end;
  end;

  Error(nil, 'SetIndexByName', '1840', 'Illegal index name: ', [Name]);
end;  { SetIndexByName }





// =============================================================================
// 'TDBICustomIndexItems' public methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 13:27:23.<P>
}
function TDBICustomIndexItems.StringCompare(
  const KeyValue: TDBIString;
  const CompareValue: TDBIString;
  bPartialKey: Boolean;
  bCaseInsensitive: Boolean
  ): Integer;
var
  Flags: longWord;
  KeyLength: Integer;

begin
  Flags := 0;

  if bCaseInsensitive then Flags := NORM_IGNORECASE;

  if bPartialKey then begin
    KeyLength := Length(CompareValue);
  end
  else begin
    KeyLength := Length(KeyValue);
  end;

  Result := windows.CompareStringA(
    LOCALE_USER_DEFAULT,
    Flags,
    PDBIChar(KeyValue),
    KeyLength,
    PDBIChar(CompareValue),
    Length(CompareValue)
    ) - 2;
end;  { StringCompare }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 18:52:08.<P>
}
function TDBICustomIndexItems.ModifyItem(ItemNo: Integer; const Value: TDBIString): Integer;
var
  Obj: TObject;

begin
  Obj := Objects[ItemNo];
  Delete(ItemNo);
  Result := AddObject(Value, Obj);
end;  { ModifyItem }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBICustomIndexItems.Find(const S: TDBIString; var Index: Integer): Boolean;
begin
  Result := InternalFind(S, Index,
    False, { ParialKey is False }
    ixCaseInsensitive in FIndexOptions,
    ixDescending in FIndexOptions,
    False
  );
end;  { Find }


// _____________________________________________________________________________
{**
  This code was adapted from the Find code in TStringlist and has therefore
  NOT had the variable names updated to keep the code similar to the original
  in case of modifications to the version in TStringList.

  Jvr - 29/05/2002 18:55:44.<P>
}
function TDBICustomIndexItems.InternalFind(
  const Value: TDBIString;
  var Index: Integer;
  bPartialKey: Boolean;
  bCaseInsensitive: Boolean;
  bDescending: Boolean;
  bReverseOrder: Boolean
  ): Boolean;
var
  L, H, I, C: Integer;

begin
  Result := False;
  L := 0;
  H := Count - 1;
  while (L <= H) do begin
    I := (L + H) shr 1;
    C := InternalCompare(GetString(I, bReverseOrder), Value, bPartialKey,
      bCaseInsensitive, bDescending);

    if (C < 0) then begin
      L := I + 1;
    end
    else begin
      H := I - 1;
      if (C = 0) then begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;  { if }
  end;  { while }

  Index := L;
end;  { InternalFind }



// =============================================================================
// 'TDBICustomIndexItems' protected methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 18:53:31.<P>
}
function TDBICustomIndexItems.GetString(Index: Integer; bReverseOrder: Boolean): TDBIString;
begin
  if (bReverseOrder) then begin
    Result := Strings[Count - 1 - Index];
  end
  else begin
    Result := Strings[Index];
  end;
end;  { GetString }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 18:54:33.<P>
}
function TDBICustomIndexItems.InternalCompare(
  const KeyValue: TDBIString;
  const CompareValue: TDBIString;
  bPartialKey: Boolean;
  bCaseInsensitive: Boolean;
  bDescending: Boolean
  ): Integer;
begin
  if Assigned(FCompare) then begin
    Result := FCompare(KeyValue, CompareValue, bPartialKey, bCaseInsensitive);
  end
  else begin
    Result := StringCompare(KeyValue, CompareValue, bPartialKey, bCaseInsensitive);
  end;

  if bDescending then begin
    Result := Result * -1;
  end;
end;  { InternalCompare }





// =============================================================================
// 'TDBIDefaultIndex' methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:41:23.<P>
}
function TDBIDefaultIndex.GetCount: Integer;
begin
//  Error(nil, 'GetCount', '1505',
  raise Exception.CreateFmt(
    '%s::%s::%s::%s'#13'GetCount should never be called on the default index',
    [UnitName, Self.Classname, 'GetCount', '1505']
    );
  Result := FCount;
end;  { GetCount }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:35:33.<P>
}
function TDBIDefaultIndex.GetDisplayOrder: TDBIDisplayOrder;
begin
  Result := doAscending;
end;  { GetDisplayOrder }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:37:01.<P>
}
function TDBIDefaultIndex.GetRecNo(Value: Integer): Integer;
begin
  Result := Value;
end;  { GetRecNo }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:36:25.<P>
}
function TDBIDefaultIndex.GetSeqNo(Value: Integer): Integer;
begin
  Result := Value;
end;  { GetSeqNo }


{$ifdef _AGGREGATES}

// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 16:36:23 - Initial code.<br />
}
function TDBIDefaultIndex.GetGroup(const Index: Integer): TDBIGroup;
begin
  Result := nil;

  Assert(Assigned(Result));
end;  { GetGroup }

{$endif}


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:35:58 - Initial code.<br />
}
function TDBIDefaultIndex.GetStrings: TDBIStrings;
begin
  Result := nil;
end;  { GetStrings }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:38:54.<BR>
  Jvr - 22/07/2002 14:34:57 - 1st passed in parameter changed SeqNo -> RecNo<P>
}
procedure TDBIDefaultIndex.InternalDelete(const pRecBuf; RecordNumber: Integer);
begin
  Dec(FCount);
end;  { InternalDelete }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:38:33.<P>
}
function TDBIDefaultIndex.InternalInsert(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
begin
  Inc(FCount);
  Result := RecordNumber;
end;  { InternalInsert }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:37:58.<P>
}
function TDBIDefaultIndex.InternalModify(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
begin
  // Do Nothing
  Result := RecordNumber;
end;  { InternalModify }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:50:40.<P>
}
procedure TDBIDefaultIndex.SetCount(const Value: Integer);
begin
  FCount := Value;
end;  { SetCount }


// _____________________________________________________________________________
{**
  Jvr - 04/06/2002 12:34:54.<P>
}
procedure TDBIDefaultIndex.SetDisplayOrder(Value: TDBIDisplayOrder);
begin
  // Do Nothing
end;  { SetDisplayOrder }





// =============================================================================
// 'TDBIStandardIndex' public methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:48:46 - Updated code.<br />

<pre>
  pDSIDXDesc = ^DSIDXDesc;
  DSIDXDesc = packed record
    szName    : DBINAME;            // IndexName
    iFields   : Integer;            // Number of fields in order (0 -> base order)
    iKeyFields: DSKEY;              // FieldNumbers
    iKeyLen   : Integer;            // Total length of key (computed)
    bUnique   : LongBool;
    bDescending  : DSKEYBOOL;       // TRUE ->Descending
    bCaseInsensitive : DSKEYBOOL;
  end;

  ixPrimary	        The index is the primary index of the table.
  ixUnique	        Each value in the index is unique; there are no duplicates.
  ixDescending	    The index imposes a descending sort order.
  ixExpression	    The index is based on a dBASE key expression.
  ixCaseInsensitive	The index sorts records case insensitively.
  ixNonMaintained	  The index is not automatically updated when the table is opened.
</pre>
}
constructor TDBIStandardIndex.Create(PIndexDesc: pDSIDXDesc);
const
  DuplicatesMap: array[Boolean] of TDuplicates = (dupAccept, dupError);
{##JVR
  IndexUniqueMap: array[Boolean] of TIndexOption = (TIndexOption(0), ixUnique);
  IndexOrderMap: array[Boolean] of TIndexOption = (TIndexOption(0), ixDescending);
  IndexCaseMap: array[Boolean] of TIndexOption = (TIndexOption(0), ixCaseInsensitive);
//}
begin
  inherited Create(PIndexDesc);

  FItems := TDBICustomIndexItems.Create;

  FItems.FIndexOptions := [ixNonMaintained];
  if PIndexDesc^.bUnique then Include(FItems.FIndexOptions, ixUnique);
  if PIndexDesc^.bDescending[0] then Include(FItems.FIndexOptions, ixDescending);
  if PIndexDesc^.bCaseInsensitive[0] then Include(FItems.FIndexOptions, ixUnique);
{##JVR!!!
  FItems.FIndexOptions := [
    IndexUniqueMap[Boolean(PIndexDesc^.bUnique)],
    IndexOrderMap[Boolean(PIndexDesc^.bDescending[0])],
    IndexCaseMap[Boolean(PIndexDesc^.bCaseInsensitive[0])],
    ixNonMaintained
  ];
//}
  FItems.Duplicates := DuplicatesMap[Boolean(PIndexDesc^.bUnique)];
  FDisplayOrder := doAscending;

  // Ensure that items are inserted in sorted order (InternalFind defines the order)
  FItems.Sorted := True;

{$ifdef _AGGREGATES}
  FGroups := TDBIGroupList.Create(PIndexDesc);
{$endif}
end;


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:48:32 - Updated code.<br />
}
destructor TDBIStandardIndex.Destroy;
begin
{$ifdef _AGGREGATES}
  FGroups.Free;
  FGroups := nil;
{$endif}

  FItems.Free;
  FItems := nil;

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIStandardIndex.IsFieldInIndex(const FieldId: Integer): Boolean;
var
  Index: Integer;

begin
  Result := False;

  for Index := 0 to DBIMAXFLDSINKEY-1 do begin
    if (FieldId = FIndexDesc^.iKeyFields[Index]) then begin
      Result := True;
      Break;
    end;
  end;  { for }
end;  { IsFieldInIndex }


// _____________________________________________________________________________
{**
  FDisplayOrder is not a very good solution, but will do for now

  pLocateDesc^.bDescending[n] would be better but isn't fully implemented
  by borland. (The value is always False)  Also when setting an index by name
  Borland have NOT provided an option/parameter to allow using the index in
  reversed order.  This is why FDisplayOrder has been implemented as a backdoor
  mechanism to still provide the required functionality.

  Jvr - 29/05/2002 19:00:18.<P>
}
function TDBIStandardIndex.Locate(
  pKeyValues: PZStringList;
  pLocateDesc: pDSIDXDesc;
  var Index: Integer
  ): Boolean;
var
  IsDescending: Boolean;
  pLocateValue: PDBIChar;

begin
  if (pKeyValues = nil) then begin
    Result := False;
    Exit;
  end;

  if (FDisplayOrder = doDescending) then begin
    IsDescending := not (ixDescending in FItems.FIndexOptions);
  end
  else begin
    IsDescending := (ixDescending in FItems.FIndexOptions);
  end;

  pLocateValue := pKeyValues^[0];

  // Result indicates if it was found or not
  Result := FItems.InternalFind(
    // String to locate
    TDBIString(pLocateValue),

    // The return value, The Index of the item in the stringlist
    Index,

    // The 'not bUnique' flag represents the bPartialKey flag
    not pLocateDesc^.bUnique,

    // Case sensitivity
    pLocateDesc^.bCaseInsensitive[0],

    // CompareOrder, The direction in which the value is compared.
    // * Alphabetically or Reversed
    IsDescending,

     // DisplayOrder, The order in which items are to be searched
    (FDisplayOrder = doDescending)
  );
end;  { Locate }


{$ifdef _AGGREGATES}
// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 15:13:56 - Initial code.<br />
}
function TDBIStandardIndex.GetGroupLevel: Integer;
begin
  Result := 0;
end;  { GetGroupLevel }


// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 13:25:15 - Initial code.<br />
}
function TDBIStandardIndex.GetDebugInfo(const Position: Integer): String;
var
  Key: TDBIString;

begin
//  Result := inherited GetDebugInfo(Seq);
  Key := FItems[Position-1];
//  Result := Format('Seq=%-4.4d RecNo=%-4.4d key=%s', [Seq-1, RecNo[Seq]-1, key]);

  Result := Groups[0].GetDebugInfo(Key, RecNo[Position]-1);
end;  { GetSubGroupState }


// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 22:59:15 - Initial code.<br />
}
function TDBIStandardIndex.GetSubGroupCount(
  const Level: Integer;
  const Position: Integer
  ): Integer;
var
  Key: TDBIString;

begin
  Key := FItems[Position-1];

  Result := Groups[0].GetSubGroupItemCount(Key, RecNo[Position]-1);
end;  { GetSubGroupCount }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 15:03:39 - Initial code.<br />
}
function TDBIStandardIndex.GetSubGroupState(
  const Level: Integer;
  const Position: Integer
  ): TDBIGroupState;
var
  Key: TDBIString;

begin
  Key := FItems[Position-1];

  Result := Groups[0].GetSubGroupItemState(Key, RecNo[Position]-1);
end;  { GetSubGroupState }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 15:15:06 - Initial code.<br />
}
procedure TDBIStandardIndex.SetGroupLevel(const Value: Integer);
begin
  // NOP
end;  { SetGroupLevel }


{$endif}



// =============================================================================
// 'TDBIStandardIndex' protected methods
// =============================================================================


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 17:03:52.<BR>
  Jvr - 22/07/2002 14:35:16 - 1st passed in parameter changed SeqNo -> RecNo<P>
}
procedure TDBIStandardIndex.InternalDelete(
  const pRecBuf;
  RecordNumber: Integer
  );
var
  Seq: Integer;
  Key: TDBIString;

begin
  Seq := FItems.IndexOfObject(TObject(RecordNumber-1));
  if (Seq > -1) then begin
    Key := FItems[Seq];

    // Delete Index Key Item
    FItems.Delete(Seq);

{$ifdef _AGGREGATES}
    // Delete Group Key Item
    FGroups[0].RemoveSubGroupItem(Key, RecordNumber-1); //Seq);
{$endif}
  end;
end;  { InternalDelete }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 17:04:05.<P>
}
function TDBIStandardIndex.InternalInsert(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
var
  Key: TDBIString;

begin
  Key := GetFieldAsString(pRecBuf, pFieldDesc);

  // Add Index Key Item
  Result := FItems.AddObject(Key, TObject(RecordNumber-1));

{$ifdef _AGGREGATES}
  // Add Group Key Item
  FGroups[0].AddSubGroupItem(Key, RecordNumber-1); //Result);
{$endif}
end;  { InternalInsert }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 17:05:10.<P>
}
function TDBIStandardIndex.InternalModify(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
var
  Seq: Integer;
  Key: TDBIString;

begin
  Result := RecordNumber;
  Seq := FItems.IndexOfObject(TObject(RecordNumber-1));
  if (Seq > -1) then begin
    Key := FItems[Seq];

    // Modify Index Key Item
    FItems.ModifyItem(Seq, GetFieldAsString(pRecBuf, pFieldDesc));

{$ifdef _AGGREGATES}
    // Modify Group Key Item
    FGroups[0].RemoveSubGroupItem(Key, RecordNumber-1); //Seq);
    FGroups[0].AddSubGroupItem(Key, RecordNumber-1); //Seq);
{$endif}
  end;
end;  { InternalModify }


// _____________________________________________________________________________
{**
  Jvr - 02/04/01 15:21:18.<P>
}
procedure TDBIStandardIndex.Error(
  E: Exception;
  const Caller: String;
  const Reference: String;
  const ErrMsg: String;
  Args: array of const
  );
begin
{$IFDEF DebugExceptions}
  raise EDBIException.CreateFmt(
    UnitName + '::' + Self.ClassName + '::' + Caller + '::' + Reference + #13 + ErrMsg, Args);
{$ELSE}
  raise EDBIException.CreateFmt(ErrMsg, Args);
{$ENDIF DebugExceptions}
end;  { Error }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIStandardIndex.GetCount: Integer;
begin
  Result := FItems.Count;
end;  { GetCount }


// _____________________________________________________________________________
{**
  Jvr - 30/05/2002 16:29:58 - With filters we now need to do range checking ???<P>
}
function TDBIStandardIndex.GetRecNo(Value: Integer): Integer;
begin
  if (FDisplayOrder = doDescending) then begin
    Result := Integer(FItems.Objects[FItems.Count - Value]) + 1;
  end
  else begin
    Result := Integer(FItems.Objects[Value-1]) + 1;
  end;
end;  { GetRecNo }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIStandardIndex.GetSeqNo(Value: Integer): Integer;
begin
  { TODO 4 -oJvr -cTDBIStandardIndex.GetSeqNo() :
    This function returns 0 if the recno cannot be found, when it probably
    should raise an exception
    Nope this seems just right, because it will set the position to BOF
  }
  if (FDisplayOrder = doDescending) then begin
    Result := FItems.Count - FItems.IndexOfObject(TObject(Value-1));
  end
  else begin
    Result := FItems.IndexOfObject(TObject(Value-1)) + 1;
  end;
end;  { GetSeqNo }


// _____________________________________________________________________________
{**
  ..<P>
}
function TDBIStandardIndex.GetDisplayOrder: TDBIDisplayOrder;
begin
  Result := FDisplayOrder;
end;  { GetDisplayOrder }


// _____________________________________________________________________________
{**
  ..<P>
}
procedure TDBIStandardIndex.SetDisplayOrder(Value: TDBIDisplayOrder);
begin
  FDisplayOrder := Value;
end;  { GetDisplayOrder }


{$ifdef _AGGREGATES}

// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:52:47 - Initial code.<br />
}
function TDBIStandardIndex.GetGroup(const Index: Integer): TDBIGroup;
begin
  Assert((Index >= 0) and (Index < FGroups.GroupLevel));

  Result := FGroups[0] //##JVR Index];
end;  { GetGroup }

{$endif}


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:50:15 - Updated code.<br />
}
function TDBIStandardIndex.GetStrings: TDBIStrings;
begin
  Result := FItems;
end;  { GetStrings }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 13:28:56 - Fixed TrimRightSize,
                              it was returning the Position, not the Size<P>
}
function TDBIStandardIndex.GetFieldAsString(
  const RecordBuffer;
  pFieldDesc: PDSFLDDesc
  ): TDBIString;
var
  Size: Integer;

  function TrimRightSize(Data: PDBIChar; Count: Integer): Integer;
  var
    Index: Integer;

  begin
    Index := Count;
    while (Index > 0) and (Data[Index] <= ' ') do Dec(Index);

    // Return the Length, not the offset???
    Result := Index+1;
  end;

begin
  Size := TrimRightSize(
    @PDBIChar(RecordBuffer)[pFieldDesc^.iFldOffsInRec],
    //##Jvr - 10/10/2002 13:21:49 passing in the strlen, not the iUnits
    DBIStrLen(@PDBIChar(RecordBuffer)[pFieldDesc^.iFldOffsInRec])
//    pFieldDesc^.iUnits1
    );
  SetLength(Result, Size);
  Move(PDBIChar(RecordBuffer)[pFieldDesc^.iFldOffsInRec], PDBIChar(Result)^, Size);
end;  { GetFieldAsString }





// =============================================================================
// 'TDBINaturalOrderIndex' methods
// =============================================================================

// _____________________________________________________________________________
{**
  DON'T sort the NaturalOrder Index index (It is ordered by record number).

  Jvr - 29/05/2002 19:04:27.<P>
}
constructor TDBINaturalOrderIndex.Create(PIndexDesc: pDSIDXDesc);
begin
  inherited Create(PIndexDesc);

  FItems.Duplicates := dupIgnore;
  FItems.Sorted := False;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 17:06:05.<P>
}
function TDBINaturalOrderIndex.InternalInsert(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
begin
  Result := FItems.AddObject('', TObject(RecordNumber-1));
end;  { InternalInsert }


// _____________________________________________________________________________
{**
  For the NaturalOrder Index nothing needs to be done when a record is modified.

  Jvr - 29/05/2002 17:07:12.<P>
}
function TDBINaturalOrderIndex.InternalModify(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecordNumber: Integer
  ): Integer;
begin
  Result := RecordNumber;
end;  { InternalModify }





// =============================================================================
// 'TDBIIntegerIndex' methods
// =============================================================================

// _____________________________________________________________________________
{**
  ..<P>
}
constructor TDBIIntegerIndex.Create(PIndexDesc: pDSIDXDesc);
begin
  inherited Create(PIndexDesc);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 17:10:59.<P>
}
function TDBIIntegerIndex.InternalInsert(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecNo: Integer
  ): Integer;
begin
  // ##JVR - NOTES
  // This is obviously wrong.
  // The item needs to be added in sorted order, thus the data should be
  // formatted before adding???
  // OR !!
  // We use a different compare routine to sort Integers

  { TODO 5 -oJvr -cTDBIntegerIndex.InternalInsert() :
    Not implemented Yet!
  }
  Result := -1;
  Error(nil, 'InternalInsert', '1275', 'Not implemented Yet!', []);
end;  { InternalInsert }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 17:11:36.<P>
}
function TDBIIntegerIndex.InternalModify(
  const pRecBuf;
  pFieldDesc: pDSFLDDesc;
  RecNo: Integer
  ): Integer;
begin
  Result := RecNo;
  { TODO 5 -oJvr -cTDBIntegerIndex.InternalModify() :
    Not implemented Yet!
  }
  Error(nil, 'InternalModify', '1300', 'Not implemented Yet!', []);
end;  { InternalModify }





// =============================================================================
// 'TDBIDateIndex' protected methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 19:11:29.<P>
}
function TDBIDateIndex.Locate(
  pKeyValues: PZStringList;
  pLocateDesc: pDSIDXDesc;
  var Index: Integer
  ): Boolean;
var
  pLocateValue: PDBIChar;
  TimeStamp: TTimeStamp;               // TTimeStamp declared in SysUtils
  DateTime: TDateTime;
  DateString: String;                  // Use Default string type for conversion!
  KeyValue: TDBIString;

begin
  // Convert TDateTime To String (See TXBaseDBICursor.PutFieldDateTime)
  pLocateValue := pKeyValues^[0];
  TimeStamp.Date := TDateTimeRec(Pointer(pLocateValue)^).Date;
  DateTime := TimeStampToDateTime(TimeStamp);
  DateTimeToString(DateString, STOREDDATEFORMAT, DateTime);
  { TODO 5 -oLhe -cTDBIDateIndex.Locate() :
    30/04/1999
    Commented out - allow for different system settings
  }
  {DateString := DateTimeToStr(DateTime);
  DateString := Copy(DateString, 7, 4) +
                Copy(DateString, 4, 2) +
                Copy(DateString, 1, 2);}

  // Convert to AnsiString!
  KeyValue := TDBIString(DateString);

  // Don't forget the null terminator #0
  Move(PDBIChar(KeyValue)^, pKeyValues^[0], Length(KeyValue)+1);

  Result := inherited Locate(pKeyValues, pLocateDesc, Index);
end;  { Locate }





// =============================================================================
// 'TDBINumberIndex' protected methods
// =============================================================================

constructor TDBINumberIndex.Create(PIndexDesc: pDSIDXDesc);
begin
  inherited Create(PIndexDesc);

  FItems.CompareFunction := NumberCompare;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 19:14:41.<P>
}
function TDBINumberIndex.Locate(
  pKeyValues: PZStringList;
  pLocateDesc: pDSIDXDesc;
  var Index: Integer): Boolean;
var
  pLocateValue: PDBIChar;
  DoubleValue: Double;

begin
  // Convert Number To String (See TXBaseDBICursor.PutFieldFloat)
  //pLocateValue := pKeyValues^[0];
  Move(pKeyValues^[0], DoubleValue, SizeOf(Double));
  pLocateValue := PDBIChar(DBIFloatToStr(DoubleValue));

  // Don't forget the null terminator #0
  Move(pLocateValue^, pKeyValues^[0], StrLen(pLocateValue)+1);

  Result := inherited Locate(pKeyValues, pLocateDesc, Index);
end;  { Locate }


// _____________________________________________________________________________
{**
  Jvr - 29/05/2002 19:15:16.<P>
}
function TDBINumberIndex.NumberCompare(
  const KeyValue: TDBIString;
  const CompareValue: TDBIString;
  bPartialKey: Boolean;
  bCaseInsensitive: Boolean
  ): Integer;
var
  KeyDouble: Extended;
  CompareDouble: Extended;

begin
  TextToFloat(PDBIChar(KeyValue), KeyDouble, fvExtended);
  TextToFloat(PDBIChar(CompareValue), CompareDouble, fvExtended);

  if (CompareDouble > KeyDouble) then begin
    Result := -1;
  end
  else if CompareDouble < KeyDouble then begin
    Result := 1;
  end
  else begin
    Result := 0;
  end;
end;  { NumberCompare }




{$ifdef _AGGREGATES}

// =============================================================================
// 'TDBISubGroup' implementation
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 10:31:46 - Initial code.<br />
}
constructor TDBISubGroupZZ.Create;
begin
  inherited Create;

  FItems := TList.Create;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 10:34:35 - Initial code.<br />
}
destructor TDBISubGroupZZ.Destroy;
begin
  FItems.Free;
  FItems := nil;

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 10:35:45 - Initial code.<br />
}
function TDBISubGroupZZ.GetSubGroupState(const Position: Integer): TDBIGroupState;
var
  SubGroupIndex: Integer;

begin
  SubGroupIndex := FItems.IndexOf(TObject(Position));

  if (SubGroupIndex < 0) then
    Result := grSTATEFIRSTLAST {not found}
  else if (SubGroupIndex = 0) then
    Result := grSTATEFIRST
  else if (SubGroupIndex = FItems.Count-1) then
    Result := grSTATELAST
  else
    Result := grSTATEMIDDLE;
end;  { GetSubGroupState }





// =============================================================================
// 'TDBIGroup' implementation
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 10:40:56 - Initial code.<br />
}
constructor TDBIGroup.Create(PIndexDesc: pDSIDXDesc);
{##JVR
const
  IndexUniqueMap: array[Boolean] of TIndexOption = (TIndexOption(0), ixUnique);
  IndexOrderMap: array[Boolean] of TIndexOption = (TIndexOption(0), ixDescending);
  IndexCaseMap: array[Boolean] of TIndexOption = (TIndexOption(0), ixCaseInsensitive);
  DuplicatesMap: array[Boolean] of TDuplicates = (dupIgnore, dupError);
//}
begin
  inherited Create;

  FItems := TDBISubGroupList.Create;
{##JVR!!!
  FItems.FIndexOptions := [
    IndexUniqueMap[Boolean(PIndexDesc^.bUnique)],
    IndexOrderMap[Boolean(PIndexDesc^.bDescending[0])],
    IndexCaseMap[Boolean(PIndexDesc^.bCaseInsensitive[0])],
    ixNonMaintained
  ];
//}
  FItems.FIndexOptions := [ixNonMaintained];
  if PIndexDesc^.bUnique then Include(FItems.FIndexOptions, ixUnique);
  if PIndexDesc^.bDescending[0] then Include(FItems.FIndexOptions, ixDescending);
  if PIndexDesc^.bCaseInsensitive[0] then Include(FItems.FIndexOptions, ixUnique);

  FItems.Duplicates := dupIgnore; //DuplicatesMap[Boolean(PIndexDesc^.bUnique)];
//  FDisplayOrder := doAscending;

  // Ensure that items are inserted in sorted order (InternalFind defines the order)
  FItems.Sorted := True;
 end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 10:42:58 - Initial code.<br />
}
destructor TDBIGroup.Destroy;
begin
  inherited Destroy;

  FItems.Free;
  FItems := nil;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 19:59:54 - Initial code.<br />
}
function TDBIGroup.AddSubGroupItem(const Key: TDBIString; const RecNo: Integer): Integer;
var
  SubGroup: TObject;

begin
  // Add SubGroup Key
  Result := FItems.IndexOf(Key);

  if (Result < 0) then
    Result := FItems.Add(Key);

  if (Result < 0) then
    Exit;

  if not Assigned(FItems.Objects[Result]) then begin
    SubGroup := TDBISubGroup.Create;
    FItems.Objects[Result] := SubGroup;
  end
  else begin
    SubGroup := FItems.Objects[Result];
  end;

  // Ok we have a SubGroup, now add the item
  if (SubGroup is TDBISubGroupList) then begin
    with SubGroup as TDBISubGroupList do begin
      Duplicates := dupAccept;
      Sorted := True;
      Result := AddObject(Key, TObject(RecNo));
    end;
  end
  else begin
//    Result := (SubGroup as TList).Add(TObject(RecNo));
    Result := 0;
    (SubGroup as TList).Insert(Result, TObject(RecNo));
  end;

end;  { AddSubGroupItem }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 21:15:54 - Initial code.<br />
}
function TDBIGroup.CheckSubGroup(const Key: TDBIString): Integer;
begin
  Result := FItems.IndexOf(Key);

  if (Result >= 0) and not Assigned(FItems.Objects[Result]) then begin
    FItems.Objects[Result] := TDBISubGroup.Create;
  end;
end;  { GetSubGroup }


// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 13:50:59 - Initial code.<br />
}
function TDBIGroup.GetDebugInfo(const Key: TDBIString; const RecNo: Integer): String;
var
  SubGroup: TObject;
  SubGroupIndex: Integer;
  ItemIndex: Integer;
  State: TDBIString;
  Count: Integer;

begin
  Result := Format('RecID=%-4d key=%s', [RecNo, Key]);

  SubGroupIndex := FItems.IndexOf(Key);
  Result := Format('RecID=%-4d sub=%-4d key=%s', [RecNo, SubGroupIndex, Key]);

  if (SubGroupIndex < 0) then
    Exit;

  SubGroup := FItems.Objects[SubGroupIndex];
  if not Assigned(SubGroup) then
    Exit;

  if (SubGroup is TStringList) then begin
    ItemIndex := (SubGroup as TStringList).IndexOfObject(TObject(RecNo));
    Count := (SubGroup as TStringList).Count;
  end
  else begin
    ItemIndex := (SubGroup as TList).IndexOf(TObject(RecNo));
    Count := (SubGroup as TList).Count;
  end;

  if (ItemIndex < 0) then
    State := 'NOT FOUND'
  else if (ItemIndex = 0) then
    State := 'FIRST'
  else if (ItemIndex = Count-1) then
    State := 'LAST'
  else
    State := 'MIDDLE';

  Result := Format('RecID=%4.4d sub=%4.4d idx=%4.4d State=%s', [RecNo, SubGroupIndex, ItemIndex, State]);
end;  { GetDebugInfo }


// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 23:00:57 - Initial code.<br />
}
function TDBIGroup.GetSubGroupItemCount(const Key: TDBIString; const RecNo: Integer): Integer;
var
  SubGroup: TObject;
  SubGroupIndex: Integer;

begin
  Result := 0;

  SubGroupIndex := FItems.IndexOf(Key);
  if (SubGroupIndex < 0) then
    Exit;

  SubGroup := FItems.Objects[SubGroupIndex];
  if not Assigned(SubGroup) then
    Exit;

  if (SubGroup is TStringList) then begin
    Result := (SubGroup as TStringList).Count;
  end
  else begin
   Result := (SubGroup as TList).Count;
  end;
end;  { GetSubGroupItemCount }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 11:07:24 - Initial code.<br />
}
function TDBIGroup.GetSubGroupItemState(const Key: TDBIString; const RecNo: Integer): TDBIGroupState;
var
  SubGroup: TObject;
  SubGroupIndex: Integer;
  ItemIndex: Integer;
  Count: Integer;

begin
  Result := grSTATEFIRST;

  SubGroupIndex := FItems.IndexOf(Key);
  if (SubGroupIndex < 0) then
    Exit;

  SubGroup := FItems.Objects[SubGroupIndex];
  if not Assigned(SubGroup) then
    Exit;

  if (SubGroup is TStringList) then begin
    ItemIndex := (SubGroup as TStringList).IndexOfObject(TObject(RecNo));
    Count := (SubGroup as TStringList).Count;
  end
  else begin
    ItemIndex := (SubGroup as TList).IndexOf(TObject(RecNo));
    Count := (SubGroup as TList).Count;
  end;

  if (ItemIndex < 0) then
    Result := grSTATEMIDDLE // grSTATEFIRSTLAST {not found}
  else if (ItemIndex = 0) then
    Result := grSTATEFIRST
  else if (ItemIndex = Count-1) then
    Result := grSTATELAST
  else
    Result := grSTATEMIDDLE;
end;  { GetSubGroupItemState }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 17:44:12 - Initial code.<br />
}
procedure TDBIGroup.RemoveSubGroupItem(const Key: TDBIString; const RecNo: Integer);
var
  SubGroup: TDBISubGroup;
  SubGroupIndex: Integer;
  ItemIndex: Integer;

  function GetItemIndex(List: TObject; const ID: Integer): Integer;
  begin
    if (List is TStringList) then
      Result := (List as TStringList).IndexOfObject(TObject(ID))
    else
      Result := (List as TList).IndexOf(TObject(ID))
  end;

begin
  // Retrieve the SubGroup by Key
  SubGroupIndex := CheckSubGroup(Key);
  if (SubGroupIndex < 0) then Exit;

  // Ok we have a SubGroup, proceed with Removal of item
  SubGroup := FItems.Objects[SubGroupIndex] as TDBISubGroup;
  ItemIndex := GetItemIndex(SubGroup, RecNo);
  if (ItemIndex < 0) then Exit;

  // Ok Item found, proceed with Removal
  SubGroup.Delete(ItemIndex);

  // NOTE: If The SubGroup is Empty it Should be Deleted
  if (SubGroup.Count = 0) then begin
    FItems.Delete(SubGroupIndex);

    SubGroup.Free;
  end;
end;  { RemoveSubGroupItem }





// =============================================================================
// 'TDBIGroupList' implementation
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:13:17 - Initial code.<br />
}
constructor TDBIGroupList.Create(PIndexDesc: pDSIDXDesc);
begin
  inherited Create;

  FItems := TList.Create;

  // Testing Only
  FItems.Add(TDBIGroup.Create(PIndexDesc));
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:14:06 - Initial code.<br />
}
destructor TDBIGroupList.Destroy;
var
  Index: Integer;

begin
  for Index := FItems.Count-1 downto 0 do
    TDBIGroup(FItems[Index]).Free;

  FItems.Free;
  FItems := nil;;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:17:32 - Initial code.<br />
}
function TDBIGroupList.GetGroup(const Index: Integer): TDBIGroup;
begin
  Result := FItems[0];

{##JVR
  if (Index >= 0) and (Index < FItems.Count) then
    Result := FItems[Index]
  else
    Result := nil;
//}
end;  { GetGroup }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:18:45 - Initial code.<br />
}
function TDBIGroupList.GetLevel: Integer;
begin
  Result := FItems.Count;
end;  { GetLevel }


// _____________________________________________________________________________
{**
  Jvr - 10/10/2008 14:20:21 - Initial code.<br />
}
procedure TDBIGroupList.SetLevel(const Value: Integer);
begin
//  FItems[1] := TDBIGroup.Create;
end;

(*##JVR
var
  ItemIndex: Integer;
  Item: TDBIGroup;

begin
  Assert(Value >= 0);

  // Clear Groups
  if (Value < FItems.Count) then begin
    for ItemIndex := FItems.Count-1 downto Value-1 do begin
      Item := FItems[ItemIndex];
      FItems.Delete(ItemIndex);
      Item.Free;
    end;
  end
  
  // Create Groups
  else begin
    for ItemIndex := FItems.Count-1 to Value-1 do begin
      Item := TDBIGroup.Create;
      FItems.Add(Item);
    end;
  end;
end;  { SetLevel }
//*)
{$endif}


end.



