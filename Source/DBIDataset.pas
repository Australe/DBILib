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
  1.0 | 12/01/1999 11:53:25 | JVR | Initial Release
  1.1 | 23/03/1999 11:05:00 | JVR | SavetoFile now checks for modification first
  1.2 | 22/03/2001 10:10:00 | JVR | Added Calculated Fields
  1.3 | 23/03/2001 11:18:05 | JVR | Added Filters mechanism
  1.4 | 31/11/2012 07:46:04 | Jvr | Unicode conversion
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIDataset;

{$I DBICompilers.inc}

interface

uses
{$ifdef Delphi2009}
  System.Generics.Collections,
{$endif}
  Classes, SysUtils, Windows, db, DBIConst, DBIIntfConsts, DBIInterfaces;


type

{ Exceptions }

  EDBIDatasetError = class(EDatabaseError)
  private
    FErrorCode: DBIResult;
  public
    constructor Create(Message: string; ErrorCode: DBIResult);
    property ErrorCode: DBIResult read FErrorCode;
  end;

{$ifdef _CONNECTIONBROKER}
  EReconcileError = class(EDBIDatasetError)
  private
    FContext: string;
    FPreviousError: DBIResult;
  public
    constructor Create(NativeError, Context: string;
      ErrorCode, PreviousError: DBIResult);
    property Context: string read FContext;
    property PreviousError: DBIResult read FPreviousError;
  end;

{ TCustomRemoteServer }

  TGetUsernameEvent = procedure(Sender: TObject; var Username: string) of object;

  TCustomRemoteServer = class(TCustomConnection)
  private
    FAppServer: Variant;
    FOnGetUsername: TGetUsernameEvent;
  protected
    function GetAppServer: Variant; virtual;
    procedure SetAppServer(Value: Variant); virtual;

    property OnGetUsername: TGetUsernameEvent read FOnGetUsername write FOnGetUsername;
  public
    constructor Create(AOwner: TComponent); override;
    function GetServer: IAppServer; virtual;
    function GetServerList: OleVariant; virtual;
    procedure GetProviderNames(Proc: TGetStrProc); virtual;
    property AppServer: Variant read GetAppServer;
  end;

{ TConnectionBroker }

  TConnectionBroker = class(TCustomRemoteServer)
  private
    FConnection: TCustomRemoteServer;
{$IFDEF MSWINDOWS}
    FStreamedConnected: Boolean;
{$ENDIF}
    procedure SetConnection(const Value: TCustomRemoteServer);
  protected
{$IFDEF MSWINDOWS}
    function GetAppServer: Variant; override;
    function GetConnected: Boolean; override;
    procedure Loaded; override;
{$ENDIF}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetConnected(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetServer: IAppServer; override;
  published
    property Connected;
    property Connection: TCustomRemoteServer read FConnection write SetConnection;
    property LoginPrompt default False;
    property AfterConnect;
    property AfterDisconnect;
    property BeforeConnect;
    property BeforeDisconnect;
    property OnGetUsername;
    property OnLogin;
  end;

{$endif}

  TDBIDataSet = class;

{$ifdef _AGGREGATES}

{ TDBIAggregate }

  TDBIAggregate = class;
  TDBIAggregates = class;
  TDBIAggUpdateEvent = procedure(Agg: TDBIAggregate) of object;

  TDBIAggregate = class(TCollectionItem)
  private
    FExpression: string;
    FFldDesc: DSFLDDesc;
    FHAggregate: hDSAggregate;
    FAggregateName: string;
    FGroupingLevel: Integer;
    FDataSet: TDBIDataset;
    FIndexName: string;
    FDataBuffer: Array of Byte;
    FDataType: TFieldType;
    FDataSize: Integer;
    FDependentFields: TBits;
    FRecBufOfs: Integer;
    FInUse: Boolean;
    FActive: Boolean;
    FVisible: Boolean;
    FOutOfDate: Boolean;
    FOnUpdate: TDBIAggUpdateEvent;
    procedure SetActive(Value: Boolean);
    procedure SetExpression(const Text: string);
    procedure SetGroupingLevel(GroupingLevel: Integer);
    procedure SetIndexName(Value: String);

  public //##AGGREGATES "was" protected
    procedure Activate;
    property DependentFields: TBits read FDependentFields;
    property RecBufOfs: Integer read FRecBufOfs write FRecBufOfs;

  public
    constructor Create(Aggregates: TDBIAggregates; ADataSet: TDBIDataSet); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDisplayName: string; override;
    function Value: Variant;
    property AggHandle: hDSAggregate read FHAggregate write FHAggregate;
    property InUse: Boolean read FInUse write FInUse default False;
    property DataSet: TDBIDataSet read FDataSet;
    property DataSize: Integer read FDataSize;
    property DataType: TFieldType read FDataType;
  published
    property Active: Boolean read FActive write SetActive default False;
    property AggregateName: string read FAggregateName write FAggregateName;
    property Expression: string read FExpression write SetExpression;
    property GroupingLevel: Integer read FGroupingLevel write SetGroupingLevel default 0;
    property IndexName: string read FIndexName write SetIndexName;
    property Visible: Boolean read FVisible write FVisible default True;
    property OnUpdate: TDBIAggUpdateEvent read FOnUpdate write FOnUpdate;

  end;  { TDBIAggregate }



{ TDBIAggregates }

  TDBIAggregates = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TDBIAggregate;
    procedure SetItem(Index: Integer; Value: TDBIAggregate);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    function Add: TDBIAggregate;
    procedure Clear;
    function Find(const DisplayName: string): TDBIAggregate;
    function IndexOf(const DisplayName: string): Integer;
    property Items[Index: Integer]: TDBIAggregate read GetItem write SetItem; default;

  end;

{$endif}


{ TDBIDataSet }

  TDBILoadMode = (lmCreateDataset, lmDisableSourceEvents);
  TDBILoadModes =  set of TDBILoadMode;

  TDataSetOption = (
    doDisableInserts,
    doDisableDeletes,
    doDisableEdits,
    doNoResetCall,
    doWriteThrough      // if set then data is written immediately to target
    );
  TDataSetOptions = set of TDataSetOption;

  TDBIInternalOption = (ioCreateDataset, ioCreating, ioLoading, ioCreateFields);
  TDBIInternalOptions = set of TDBIInternalOption;

  TDBILockOption = (loExceptionOnFail);
  TDBILockOptions = set of TDBILockOption;

  TFetchOption = (foRecord, foBlobs, foDetails);
  TFetchOptions = set of TFetchOption;

{$ifdef DelphiXE3}
  TDBIFieldList = TList<TField>;
{$else}
  TDBIFieldList = TList;
{$endif}


  TDBIBytesField = class(TBytesField)
  protected
    function GetAsString: String; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  end;


{$ifdef DelphiXE3}
  TDBIExtendedField = class(TExtendedField)
  protected
    function GetAsExtended: Extended; override;
  end;
{$endif}


{$ifdef fpc}
  TObjectField = class(TField)
  end;

  TADTField = class(TObjectField)
  end;

  TDatasetField = class(TObjectField)
  end;
{$endif}


  TDBICustomDataset = class(TDataset {$ifdef fpc}, IFPObserver {$endif} )
  protected
    function GetFieldClass(FieldType: TFieldType): TFieldClass; override;

{$ifdef fpc}
  private
    FObjectView: Boolean;
    FDatasetField: TField;

  protected
    procedure CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef); virtual;
    procedure CreateFields; override;

    procedure DefChanged(Sender: TObject); virtual; abstract;
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);

    function GetFieldData(FieldNo: Integer; Buffer: TDBIValueBuffer): Boolean; overload; virtual;
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; virtual;
    procedure OpenParentDataSet(ParentDataSet: TDBICustomDataset);
    procedure SetDataSetField(const Value: TDataSetField); virtual;

    property DataSetField: TField read FDatasetField;
    property ObjectView: Boolean read FObjectView write FObjectView;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

{$endif}

  end;


  TDBIDataset = class(TDBICustomDataset)
  private
    FDataConnection: TDBIDataConnection;
    FInternalOptions: TDBIInternalOptions;
{$ifdef _AGGREGATES}
    FActiveAggLists: TList;
    FAggFieldsUpdated: TBits;
    FAggFieldsInit: Boolean;
    FAggFieldsSize: Integer;
    FAggGrpIndOfs: Integer;
    FAggFieldsOfs: Integer;
    FAggGrpIndSize: Integer;
    FAggregates: TDBIAggregates;
    FAggregatesActive: Boolean;
{$endif}
{$ifdef _UNUSED}
    FCommandText: WideString;
{$endif}
    FDSBase: IDBIBase;
    FDSCursor: IDBICursor;
    FDSOptions: TDataSetOptions;
    FLookupCursor: IDBICursor;
    FFindCursor: IDBICursor;
    FCloneSource: TDBIDataSet;
{$ifdef _UNUSED}
    FReconcileDataSet: TDBIDataSet;
{$endif}
    FSavedPacket: TDBIDataPacket;
{$ifdef _UNUSED}
    FDeltaPacket: TDataPacket;
    FParams: TParams;
{$endif}
    FIndexDefs: TIndexDefs;
    FIndexName: string;
    FExprFilter: HDSFilter;
    FFuncFilter: HDSFilter;
{$ifdef _UNUSED}
    FFileName: string;
{$endif}
    FFilterBuffer: TDBIRecordBuffer;
    FGroupingLevel: Integer;
    FLastParentBM: array of Byte;
    FMasterLink: TMasterDataLink;
    FIndexFieldMap: DSKEY;
    FKeyBuffers: array[TKeyIndex] of PKeyBuffer;
    FKeyBuffer: PKeyBuffer;
    FNewValueBuffer: TDBIRecordBuffer;
    FOldValueBuffer: TDBIRecordBuffer;
    FCurValueBuffer: TDBIRecordBuffer;
    FIndexFieldCount: Integer;
    FIndexGroupingLevel: Integer;
{$ifdef _UNUSED}
    FAppServer: IDBIAppServer;
    FProviderName: string;
    FRemoteServer: TCustomRemoteServer;
{$endif}
    FPacketRecords: Integer;
    FConstDisableCount: Integer;
{$ifdef _AGGREGATES}
    FMaxAggGroupingLevel: Integer;
{$endif}
    FParentDataSet: TDBIDataSet;
    { Word & Byte size data members }
    FKeySize: Word;
    FRecordSize: Word;         { Physical size of record }
    FBookmarkOfs: Word;        { Offset to bookmark data in recbuf }
    FRecInfoOfs: Word;         { Offset to extra rec info in recbuf }
    FRecBufSize: Word;         { Total size of recbuf }
    FReadOnly: Boolean;
    FFieldsIndex: Boolean;
    FCanModify: Boolean;
    FExclusive: Boolean;
{$ifdef _UNUSED}
    FInReconcileCallback: Boolean;
{$endif}
    FNotifyCallback: Boolean;
{$ifdef _UNUSED}
    FOpeningFile: Boolean;
{$endif}
    FProviderEOF: Boolean;
    FFetchOnDemand: Boolean;
    FStoreDefs: Boolean;
    FSavePacketOnClose: Boolean;
{$ifdef _UNUSED}
    FOnReconcileError: TReconcileErrorEvent;
{$endif}
    FStatusFilter: TUpdateStatusSet;
{$ifdef _UNUSED}
    FBeforeApplyUpdates: TRemoteEvent;
    FAfterApplyUpdates: TRemoteEvent;
    FBeforeGetRecords: TRemoteEvent;
    FAfterGetRecords: TRemoteEvent;
    FBeforeRowRequest: TRemoteEvent;
    FAfterRowRequest: TRemoteEvent;
    FBeforeExecute: TRemoteEvent;
    FAfterExecute: TRemoteEvent;
    FBeforeGetParams: TRemoteEvent;
    FAfterGetParams: TRemoteEvent;
    FConnectionBroker: TConnectionBroker;
{$endif}
    FRanged: Boolean;

  private
    function GetActiveBuffer: TDBIRecBuf; {$ifdef Delphi2009} inline; {$endif}
    function GetCalcBuffer: TDBIRecBuf; {$ifdef Delphi2009} inline; {$endif}
    function GetTempBuffer: TDBIRecBuf; {$ifdef Delphi2009} inline; {$endif}

  protected
    procedure AddExprFilter(const Expr: string; Options: TFilterOptions);
    procedure AddFuncFilter;
{$ifdef _UNUSED}
    function CalcFieldsCallBack(RecBuf: TDBIRecordBuffer): DBIResult; stdcall;
{$endif}
    procedure CheckFieldProps;
    procedure CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef); override;
    procedure CheckMasterRange;
{$ifdef _AGGREGATES}
    procedure ClearActiveAggs;
{$endif}
    procedure ClearSavedPacket;
{$ifdef _AGGREGATES}
    procedure CloseAggs;
{$endif}
    function CreateDSBase: IDBIBase; virtual;
    function CreateDSCursor(SourceCursor: IDBICursor): IDBICursor;
    procedure DecodeIndexDesc(const IndexDesc: DSIDXDesc;
      out Name, Fields, DescFields, CaseInsFields: string; out Options: TIndexOptions);
    procedure EncodeFieldDesc(out FieldDesc: DSFLDDesc; const Name: string;
      DataType: TFieldType; Size, Precision: Integer; Calculated: Boolean;
      Attributes: TFieldAttributes);
    procedure EncodeIndexDesc(out IndexDesc: DSIDXDesc;
      const Name, Fields, DescFields, CaseInsFields: string; Options: TIndexOptions);
    function FilterCallback(RecBuf: TDBIRecordBuffer): LongBool; stdcall;
{$ifdef _AGGREGATES}
    procedure DoAggUpdates(IsUpdate: Boolean);
    function  GetActiveAggs(Index: Integer) : TList;
{$endif}
    function GetActiveRecBuf(out RecBuf: TDBIRecordBuffer): Boolean;
{$ifdef _AGGREGATES}
    procedure GetAggFieldData(Buffer: TDBIRecordBuffer);
{$endif}
{$ifdef _UNUSED}
    function GetChangeCount: Int64;
{$endif}
    function GetData: TDBIDataPacket;
    function GetDataSize: Integer;
{$ifdef _UNUSED}
    function GetDelta: OleVariant;
{$endif}
    function GetIndexDefs: TIndexDefs;
    function GetIndexFieldNames: string;
    function GetIndexName: string;
    function GetLogChanges: Boolean;
    function GetMasterFields: string;
    function GetProviderEOF: Boolean;
{$ifdef _UNUSED}
    function GetSavePoint: Int64;
    function GetHasAppServer: Boolean;
{$endif}
    procedure InitBufferPointers(GetProps: Boolean);
{$ifdef _AGGREGATES}
    function InternalGetGroupState(Level: Integer): TGroupPosInds;
{$endif}
    procedure InternalFetch(Options: TFetchOptions);
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure NotifyCallback; stdcall;
    procedure ReadData(Stream: TStream);
    procedure ReadInternalOptions(Reader: TReader);
{$ifdef _UNUSED}
    function ReconcileCallback(iRslt: Integer; iUpdateKind: DSAttr;
      iResAction: dsCBRType; iErrCode: Integer; pErrMessage, pErrContext: Pointer;
      pRecUpd, pRecOrg, pRecConflict: Pointer; iLevels: Integer;
      piFieldIDs: PInteger): dsCBRType; stdcall;
{$endif}
{$ifdef _AGGREGATES}
    procedure ResetAgg(Agg: TDBIAggregate; DeleteFirst: Boolean);
    procedure ResetAllAggs(Value: Boolean);
    procedure ResetGrouping;
    procedure SetAggsActive(Value: Boolean);
{$endif}
{$ifdef _UNUSED}
    procedure SetConnectionBroker(const Value: TConnectionBroker);
{$endif}
    procedure SaveDataPacket(Format: TDBIDataFormat = dfXML);
    procedure SetData(const Value: TDBIDataPacket);
    procedure SetDataSource(Value: TDataSource);
{$ifdef _UNUSED}
    procedure SetDisableStringTrim(Value: Boolean);
{$endif}
    procedure SetIndex(const Value: string; FieldsIndex: Boolean);
    procedure SetIndexDefs(Value: TIndexDefs);
    function GetFileName: String; virtual;
    procedure SetFileName(const Value: string); virtual;
    procedure SetIndexFieldNames(const Value: string);
    procedure SetIndexName(const Value: string);
    procedure SetLogChanges(Value: Boolean);
    procedure SetMasterFields(const Value: string);
    procedure SetNotifyCallback;
{$ifdef _UNUSED}
    procedure SetParams(Value: TParams);
    procedure SetProviderName(const Value: string);
{$endif}
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
{$ifdef _UNUSED}
    procedure SetSavePoint(Value: Int64);
{$endif}
    procedure SortOnFields(Cursor: IDBICursor; const Fields: String;
      CaseInsensitive, Descending: Boolean);
{$ifdef _UNUSED}
    procedure SetupConstraints;
    procedure SetupInternalCalcFields(Add: Boolean);
{$endif}
    procedure WriteData(Stream: TStream);
    procedure WriteInternalOptions(Writer: TWriter);
    procedure SetStatusFilter(const Value: TUpdateStatusSet);
    function GetXMLData: string;
    procedure SetXMLData(const Value: string);

  protected
    { IProviderSupport }
{$ifdef _UNUSED}
    function PSGetCommandText: string; override;
    function PSGetCommandType: TPSCommandType; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
    { IDataSetCommandSupport }
    function GetCommandStates(const ACommand: string): TDataSetCommandStates; virtual;
    procedure ExecuteCommand(const ACommand: string; const Args: array of const); virtual;
{$endif}
  protected
    { DataIntf Helper functions }
{$ifdef _UNUSED}
    procedure DoAfterApplyUpdates(var OwnerData: OleVariant); virtual;
    procedure DoBeforeApplyUpdates(var OwnerData: OleVariant); virtual;
    function DoApplyUpdates(Delta: OleVariant; MaxErrors: Integer; out ErrorCount: Integer): OleVariant; virtual;
    procedure DoAfterGetParams(var OwnerData: OleVariant); virtual;
    procedure DoBeforeGetParams(var OwnerData: OleVariant); virtual;
    procedure DoAfterGetRecords(var OwnerData: OleVariant); virtual;
    procedure DoBeforeGetRecords(var OwnerData: OleVariant); virtual;
    function DoGetRecords(Count: Integer; out RecsOut: Integer; Options: Integer;
       const CommandText: WideString; Params: OleVariant): OleVariant; virtual;
    procedure DoAfterRowRequest(var OwnerData: OleVariant); virtual;
    procedure DoBeforeRowRequest(var OwnerData: OleVariant); virtual;
    function DoRowRequest(Row: OleVariant; RequestType: Integer): OleVariant; virtual;
    procedure DoAfterExecute(var OwnerData: OleVariant); virtual;
    procedure DoBeforeExecute(var OwnerData: OleVariant); virtual;
    procedure DoExecute(Params: OleVariant); virtual;
{$endif}
    procedure ActivateFilters;
{$ifdef _UNUSED}
    procedure AddDataPacket(const Data: OleVariant; HitEOF: Boolean); virtual;
{$endif}
    procedure AddFieldDesc(FieldDescs: TFieldDescList; var DescNo: Integer;
      var FieldID: Integer; FieldDefs: TFieldDefs);
    procedure AllocKeyBuffers;
    function AllocRecordBuffer: TDBIRecordBuffer; override;
    procedure Check(Status: DBIResult);
    procedure CheckDetailRecords; virtual;
    procedure CheckProviderEOF; virtual;
    procedure CheckSetKeyMode;
    procedure ClearCalcFields(Buffer: TDBIRecordBuffer); override;
    procedure CloseCursor; override;
    procedure {%H-}DataConvert(Field: TField; Source: TDBIValueBuffer; {$ifdef DelphiXE4} var {$endif} Dest: TDBIValueBuffer; ToNative: Boolean); overload; override;
{$ifdef DelphiXE3}
    procedure DataConvert(Field: TField; Source: Pointer; Dest: Pointer; ToNative: Boolean); overload; override; //##JVR deprecated 'Use overloaded method instead';
{$endif DelphiXE3}
    procedure DataEvent(Event: TDataEvent; Info: TDBIDataEventInfo); override;  {##NEW Info: NativeInt }
    procedure DeactivateFilters;
    procedure DefChanged(Sender: TObject); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DestroyLookupCursor; virtual;
    procedure DoBeforeInsert; override;
    procedure DoOnNewRecord; override;
{$ifdef _UNUSED}
    procedure FetchMoreData(All: Boolean); virtual;
{$endif}
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure FreeKeyBuffers;
    procedure FreeRecordBuffer(var Buffer: TDBIRecordBuffer); override;
{$ifdef _AGGREGATES}
    function GetAggregateValue(Field: TField): Variant; override;
{$endif}
{$ifdef _UNUSED}
    function GetAppServer: IAppServer; virtual;
{$endif}
    procedure GetBookmarkData(Buffer: TDBIRecordBuffer; Data: TDBIBookmark); overload; override;
    function GetBookmarkFlag(Buffer: TDBIRecordBuffer): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    function GetIndexField(Index: Integer): TField;
    function GetIndexFieldCount: Integer;
{$ifdef _UNUSED}
    function GetIsClone: Boolean; virtual;
{$endif}
    function GetIsIndexField(Field: TField): Boolean; override;
    function GetKeyBuffer(KeyIndex: TKeyIndex): PKeyBuffer;
    function GetKeyExclusive: Boolean;
    function GetKeyFieldCount: Integer;
    function GetRecord(Buffer: TDBIRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Integer; overload; override;
    function GetRecNo: Integer; override;
    function GetRecordSize: Word; override;
{$ifdef _UNUSED}
    function GetRemoteServer: TCustomRemoteServer; virtual;
{$endif}
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
    function InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;
    procedure InitRecord(Buffer: TDBIRecordBuffer); override;
    procedure InternalAddRecord(Buffer: TDBIRecordData; Append: Boolean); overload; override;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;
    procedure InternalFirst; override;
    function InternalGetOptionalParam(const ParamName: string;
      FieldNo: Integer = 0): OleVariant;
    procedure InternalSetOptionalParam(const ParamName: string; const Value: OleVariant;
      IncludeInDelta: Boolean = False; FieldNo: Integer = 0);
    procedure InternalGotoBookmark(Bookmark: TDBIBookmark); overload; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TDBIRecordBuffer); override;
    procedure InternalInsert; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TDBIRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure Loaded; override;
    function LocateRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean): Boolean;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PostKeyBuffer(Commit: Boolean);
{$ifdef _UNUSED}
    procedure RefreshInternalCalcFields(Buffer: TDBIRecordBuffer); override;
{$endif}
    procedure ReadDataPacket(Stream: TStream; ReadSize: Boolean);
{$ifdef _AGGREGATES}
    procedure ResetAggField(Field: TField); override;
{$endif}
    function ResetCursorRange: Boolean;
{$ifdef _AGGREGATES}
    procedure SetAggregates(Value: TDBIAggregates); virtual;
{$endif}
    procedure SetAltRecBuffers(Old, New, Cur: TDBIRecordBuffer);
{$ifdef _UNUSED}
    procedure SetAppServer(Value: IAppServer); virtual;
{$endif}
    procedure SetBookmarkData(Buffer: TDBIRecordBuffer; Data: TDBIBookmark); overload; override;
    procedure SetBookmarkFlag(Buffer: TDBIRecordBuffer; Value: TBookmarkFlag); override;
{$ifdef _UNUSED}
    procedure SetCommandText(Value: WideString); virtual;
{$endif}
    function SetCursorRange: Boolean;
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure {%H-}SetFieldData(Field: TField; Buffer: TDBIValueBuffer); overload; override;
    procedure SetFilterData(const Text: string; Options: TFilterOptions);
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetIndexField(Index: Integer; Value: TField);
    procedure SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
    procedure SetKeyExclusive(Value: Boolean);
    procedure SetKeyFieldCount(Value: Integer);
    procedure SetKeyFields(KeyIndex: TKeyIndex; const Values: array of const);
    procedure SetLinkRanges(MasterFields: TDBIFieldList{<TField>});
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetProviderEOF(Value: Boolean); virtual;
    procedure SetRecNo(Value: Integer); override;
{$ifdef _UNUSED}
    procedure SetRemoteServer(Value: TCustomRemoteServer); virtual;
{$endif}
    procedure SwitchToIndex(const IndexName: string);
    procedure SyncCursors(Cursor1, Cursor2: IDBICursor);
    procedure UpdateIndexDefs; override;
    procedure WriteDataPacket(Stream: TStream; WriteSize: Boolean;
      Format: TDBIDataFormat = dfXML);
    function ConstraintsStored: Boolean;
{$ifdef _AGGREGATES}
    property Aggregates: TDBIAggregates read FAggregates write SetAggregates;
    property AggregatesActive: Boolean read FAggregatesActive write SetAggsActive default False;
{$endif}
{$ifdef _UNUSED}
    property AutoCalcFields;
    property CommandText: WideString read FCommandText write SetCommandText;
    property Constraints stored ConstraintsStored;
    property DisableStringTrim: Boolean read FDisableStringTrim write SetDisableStringTrim default False;
{$endif}
    property DSBase: IDBIBase read FDSBase write FDSBase;
    property DSCursor: IDBICursor read FDSCursor;
{$ifdef _UNUSED}
    property Filter;
    property Filtered;
    property FilterOptions;
    property FieldDefs stored FStoreDefs;
    property IndexDefs: TIndexDefs read GetIndexDefs write SetIndexDefs stored FStoreDefs;
    property IndexName: string read GetIndexName write SetIndexName;
    property IsClone: Boolean read GetIsClone;
{$endif}
  protected
    //##JVR - This should be public but will remain protected until fully tested.
    property FetchOnDemand: Boolean read FFetchOnDemand write FFetchOnDemand default True;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
  public
{$ifdef _UNUSED}
    property ObjectView default True;
    property Params: TParams read FParams write SetParams;
{$endif}
  protected
    //##JVR - This should be public but will remain protected until fully tested.
    property ProviderEOF: Boolean read GetProviderEOF write SetProviderEOF;
  public
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
{$ifdef _UNUSED}
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnReconcileError: TReconcileErrorEvent read FOnReconcileError write FOnReconcileError;
    property BeforeApplyUpdates: TRemoteEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property AfterApplyUpdates: TRemoteEvent read FAfterApplyUpdates write FAfterApplyUpdates;
    property BeforeGetRecords: TRemoteEvent read FBeforeGetRecords write FBeforeGetRecords;
    property AfterGetRecords: TRemoteEvent read FAfterGetRecords write FAfterGetRecords;
    property BeforeRowRequest: TRemoteEvent read FBeforeRowRequest write FBeforeRowRequest;
    property AfterRowRequest: TRemoteEvent read FAfterRowRequest write FAfterRowRequest;
    property BeforeExecute: TRemoteEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TRemoteEvent read FAfterExecute write FAfterExecute;
    property BeforeGetParams: TRemoteEvent read FBeforeGetParams write FBeforeGetParams;
    property AfterGetParams: TRemoteEvent read FAfterGetParams write FAfterGetParams;
{$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active;
    property DataSetField;
{$ifdef _UNUSED}
    property ProviderName: string read FProviderName write SetProviderName;
{$endif}
    property FileName: string read GetFileName write SetFileName;
{$ifdef _UNUSED}
    property ConnectionBroker: TConnectionBroker read FConnectionBroker write SetConnectionBroker;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
{$endif}
  protected
    //##JVR - This should be public but will remain protected until fully tested.
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property PacketRecords: Integer read FPacketRecords write FPacketRecords default -1;
  public
{$ifdef _UNUSED}
    property RemoteServer: TCustomRemoteServer read GetRemoteServer write SetRemoteServer;
{$endif}
    procedure AddIndex(const Name, Fields: string; Options: TIndexOptions;
      const DescFields: string = ''; const CaseInsFields: string = '';
      const GroupingLevel: Integer = 0);
{$ifdef _UNUSED}
    procedure AppendData(const Data: OleVariant; HitEOF: Boolean);
{$endif}
    procedure ApplyRange;
{$ifdef _UNUSED}
    function ApplyUpdates(MaxErrors: Integer): Integer; virtual;
{$endif}
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
{$ifdef _UNUSED}
    procedure Cancel; override;
{$endif}
    procedure CancelRange;
{$ifdef _UNUSED}
    procedure CancelUpdates;
{$endif}
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure CreateDataSet; virtual;
    procedure CloneCursor(Source: TDBIDataset; Reset: Boolean;
      KeepSettings: Boolean = False); virtual;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function ConstraintsDisabled: Boolean;
{$ifdef _UNUSED}
    function DataRequest(Data: OleVariant): OleVariant; virtual;
{$endif}
    procedure DeleteIndex(const Name: string);
    procedure DisableConstraints;
    procedure EnableConstraints;
    procedure EditKey;
    procedure EditRangeEnd;
    procedure EditRangeStart;
    procedure EmptyDataSet; virtual;
{$ifdef _UNUSED}
    procedure Execute; virtual;
{$endif}
    procedure FetchBlobs;
    procedure FetchDetails;
    procedure RefreshRecord;
{$ifdef _UNUSED}
    procedure FetchParams;
{$endif}
    function FindKey(const KeyValues: array of const): Boolean; virtual;
    procedure FindNearest(const KeyValues: array of const);
    function GetCurrentRecord(Buffer: TDBIRecordBuffer): Boolean; override;
    function GetFieldData(Field: TField; {$ifdef DelphiXE4} var {$endif} Buffer: TDBIValueBuffer): Boolean; overload; override;
    function GetFieldData(FieldNo: Integer; {$ifdef DelphiXE4} var {$endif} Buffer: TDBIValueBuffer): Boolean; overload; override;
{$ifdef DelphiXE3}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override; //##JVR deprecated 'Use overloaded method instead';
    function GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean; overload; override; //##JVR deprecated 'Use overloaded method instead';
{$endif DelphiXE3}
{$ifdef _AGGREGATES}
    function GetGroupState(Level: Integer): TGroupPosInds;
{$endif}
    procedure GetIndexInfo(IndexName: string);
    procedure GetIndexNames(List: TStrings);
{$ifdef _UNUSED}
    function GetNextPacket: Integer; virtual;
{$endif}
    function GetOptionalParam(const ParamName: string): OleVariant;
{$ifdef _UNUSED}
    procedure GotoCurrent(DataSet: TDBIDataset);
{$endif}
    function GotoKey: Boolean;
    procedure GotoNearest;
{$ifdef _UNUSED}
    property HasAppServer: Boolean read GetHasAppServer;
{$endif}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    procedure LoadFromFile(AFileName: string = ''; Format: TDBIDataFormat = dfDefault; OpenMode: TDBIOpenMode = omOpen);
    procedure LoadFromStream(AStream: TStream; Format: TDBIDataFormat = dfXbase; OpenMode: TDBIOpenMode = omOpen);
{$ifdef _UNUSED}
    procedure MergeChangeLog;
    procedure Post; override;
    function Reconcile(const Results: OleVariant): Boolean;
    procedure RevertRecord;
{$endif}
    procedure SaveToFile(AFileName: string = ''; Format: TDBIDataFormat = dfDefault);
    procedure SaveToStream(AStream: TStream; Format: TDBIDataFormat = dfXbase);
    procedure SetKey;
    procedure SetOptionalParam(const ParamName: string; const Value: OleVariant;
      IncludeInDelta: Boolean = False); virtual;
{$ifdef _UNUSED}
    procedure SetProvider(Provider: TComponent);
{$endif}
    procedure SetRange(const StartValues, EndValues: array of const);
    procedure SetRangeEnd;
    procedure SetRangeStart;
{$ifdef _UNUSED}
    function UndoLastChange(FollowChange: Boolean): Boolean;
{$endif}
    function UpdateStatus: TUpdateStatus; override;
{$ifdef _AGGREGATES}
    property ActiveAggs[Index: Integer] : TList read GetActiveAggs;
{$endif}
{$ifdef _UNUSED}
    property ChangeCount: Int64 read GetChangeCount;
{$endif}
    property CloneSource: TDBIDataSet read FCloneSource;
    property Data: TDBIDataPacket read GetData write SetData;
    property DataSize: Integer read GetDataSize;
    property XMLData: string read GetXMLData write SetXMLData;
{$ifdef _UNUSED}
    property AppServer: IAppServer read GetAppServer write SetAppServer;
    property Delta: OleVariant read GetDelta;
{$endif}
    property GroupingLevel: Integer read FGroupingLevel;
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property IndexFields[Index: Integer]: TField read GetIndexField write SetIndexField;
{$ifdef _UNUSED}
    property KeyExclusive: Boolean read GetKeyExclusive write SetKeyExclusive;
    property KeyFieldCount: Integer read GetKeyFieldCount write SetKeyFieldCount;
    property KeySize: Word read FKeySize;
    property LogChanges: Boolean read GetLogChanges write SetLogChanges;
    property SavePoint: Int64 read GetSavePoint write SetSavePoint;
{$endif}
    property StatusFilter: TUpdateStatusSet read FStatusFilter write SetStatusFilter;
    property Ranged: Boolean read FRanged;

  protected
    function SelectFormat(
      const AFileName: String;
      const Format: TDBIDataFormat
      ): TDBIDataFormat;

    procedure CheckIsInitialised(const Context: String);

    function GetActiveRecInfo(out RecInfo: PRecInfo): Boolean;

    procedure SetDataConnection(Value: TDBIDataConnection);
    function GetExclusive: Boolean;
    procedure SetExclusive(const Value: Boolean);
//##JVR    function GetLocked: Boolean;
//##JVR    procedure SetLocked(const Value: Boolean);
    function GetRecordNumber: Integer;

    procedure SetActive(Value: Boolean); override;
    procedure SetRecordNumber(Value: Integer);

  public
{$ifdef _AGGREGATES}
    function GetDebugInfo: String;
{$endif}

//##JVR    procedure UpdateIndicesForRecord(RecNo: Integer);

    function IsSequenced: Boolean; override;

    procedure LoadFromDataset(
      ADataset: TDataset;
      LoadModes: TDBILoadModes = [lmDisableSourceEvents]
    );

    function Locked(LockType: TDBILockType; const Value: Integer = 0): Boolean;
    function Lock(
      LockType: TDBILockType;
      const Value: Integer = 0;
      Options: TDBILockOptions = [loExceptionOnFail]
      ): Boolean;
    procedure Unlock(LockType: TDBILockType; const Value: Integer = 0);

    {**
      I am still unsure if I really want to expose this interface,
      We shall see :-)
    }
    property DataConnection: TDBIDataConnection read FDataConnection;

    {**
      RecordNumber is the physical record number of the current record. This is the
      equivalent of the Dbase/Foxpro/Xbase record number.<P>
    }
    property RecordNumber: Integer read GetRecordNumber write SetRecordNumber;

    {**
      Dataset options is based on the TProviderOptions of TDatasetProvider
      and is not normally surfaced in other datasets.
      It may be surfaced here because it can be quite useful to control user
      access for inserts, deletes and edits.<P>
    }
    property DatasetOptions: TDatasetOptions read FDSOptions write FDSOptions default [];
//    property Options: TDBIDataSetOptions read GetDataSetOptions write SetDataSetOptions;

    property Exclusive: Boolean read GetExclusive write SetExclusive default False;
//##JVR    property Locked: Boolean read GetLocked write SetLocked default False;

{##JVR
  end;

  TDBIDataSet = class(TCustomDBIDataSet)
//}
  published

    property FieldDefs stored FStoreDefs;
    property IndexDefs: TIndexDefs read GetIndexDefs write SetIndexDefs stored FStoreDefs;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: string read GetIndexName write SetIndexName;
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;

  end;  { TDBIDataSet }
  TDBIDatasetClass = class of TDBIDataset;



  TDBIBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TDBIDataSet;
    FBuffer: TDBIRecordBuffer;
    FFieldNo: Integer;
    _Modified: Boolean;

  protected
    function GetModified: Boolean;
    procedure InternalHandleException;
    procedure ReadBlobData;
    procedure SetModified(const Value: Boolean);

    property Modified: Boolean read GetModified write SetModified;

  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode; PRecBuf: TDBIRecordBuffer = nil);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Truncate;
  end;


{$ifdef _REMOTESERVER}
const
  AllParamTypes = [ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult];

function PackageParams(Params: TParams; Types: TParamTypes = AllParamTypes): OleVariant;
procedure UnpackParams(const Source: OleVariant; Dest: TParams);
{$endif}

const
  AllRecords = -1;

implementation


uses
  SysConst, TypInfo,
{$ifdef DelphiXE4}
  AnsiStrings,
{$endif}
{$ifdef DELPHI6}
  Types, RtlConsts, Variants, FmtBcd, MaskUtils,
{$else}
  Forms,
{$endif DELPHI6}
{$ifndef fpc}
  Consts, DBConsts, DBCommon, Mask,
{$endif}
  DBIUtils, DBIStrings, DBIXmlUtils, DBIDataPacketReaders, DBIDataPacketWriters;

{ Exceptions }

constructor EDBIDatasetError.Create(Message: string; ErrorCode: DBIResult);
begin
  FErrorCode := ErrorCode;
  inherited Create(Message);
end;

{$ifdef _REMOTESERVER}
constructor EReconcileError.Create(NativeError, Context: string;
  ErrorCode, PreviousError: DBIResult);
begin
  FContext := Context;
  FPreviousError := PreviousError;
  inherited Create(NativeError, ErrorCode);
end;

{ Utility functions }

function PackageParams(Params: TParams; Types: TParamTypes = AllParamTypes): OleVariant;
var
  I, Idx, Count: Integer;
begin
  Result := NULL;
  Count := 0;
  for I := 0 to Params.Count - 1 do
    if Params[I].ParamType in Types then Inc(Count);
  if Count > 0 then
  begin
    Idx := 0;
    Result := VarArrayCreate([0, Count - 1], varVariant);
    for I := 0 to Params.Count - 1 do
      if Params[I].ParamType in Types then
        begin
        if VarIsCustom(Params[I].Value) then
          Result[Idx] := VarArrayOf([Params[I].Name, VarToStr(Params[I].Value), Ord(Params[I].DataType), Ord(Params[I].ParamType),
                                     Params[I].Size, Params[I].Precision, Params[I].NumericScale])
          else
          Result[Idx] := VarArrayOf([Params[I].Name, Params[I].Value, Ord(Params[I].DataType), Ord(Params[I].ParamType),
                                     Params[I].Size, Params[I].Precision, Params[I].NumericScale]);
          Inc(Idx);
        end;
  end;
end;

procedure UnpackParams(const Source: OleVariant; Dest: TParams);
var
  TempParams: TParams;
  HighBound, i: Integer;
  LParam: TParam;
begin
  if not VarIsNull(Source) and VarIsArray(Source) and VarIsArray(Source[0]) then
  begin
    TempParams := TParams.Create;
    try
      for i := 0 to VarArrayHighBound(Source, 1) do
      begin
        HighBound := VarArrayHighBound(Source[i], 1);
        LParam := TParam(TempParams.Add);
        LParam.Name := Source[I][0];
          if HighBound > 1 then
          LParam.DataType := TFieldType(Source[I][2]);
          if HighBound > 2 then
          LParam.ParamType := TParamType(Source[I][3]);
          if HighBound > 3 then
          LParam.Size := Source[I][4];
          if HighBound > 4 then
          LParam.Precision := Source[I][5];
          if HighBound > 5 then
          LParam.NumericScale := Source[I][6];
        LParam.Value := Source[I][1];  // Value must be set last
      end;
      Dest.Assign(TempParams);
    finally
      TempParams.Free;
    end;
  end;
end;

{ TCustomRemoteServer }

constructor TCustomRemoteServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCustomRemoteServer.GetServerList: OleVariant;
begin
  Result := NULL;
end;

function TCustomRemoteServer.GetServer: IAppServer;
begin
  Result := nil;
end;

procedure TCustomRemoteServer.GetProviderNames(Proc: TGetStrProc);
var
  List: Variant;
  I: Integer;
  Server: IAppServer;
begin
  Connected := True;
  VarClear(List);
  try
    Server := GetServer;
    if Server <> nil then
      List := Server.AS_GetProviderNames
    else
      List := AppServer.AS_GetProviderNames
  except
    { Assume any errors means the list is not available. }
  end;
  if VarIsArray(List) and (VarArrayDimCount(List) = 1) then
    for I := VarArrayLowBound(List, 1) to VarArrayHighBound(List, 1) do
      Proc(List[I]);
end;

function TCustomRemoteServer.GetAppServer: Variant;
begin
  Result := FAppServer;
end;

procedure TCustomRemoteServer.SetAppServer(Value: Variant);
begin
  FAppServer := Value;
end;
{$endif}




{ TDBIDataSet}

type
  // Declare as friend class
  TDBIDataConnectionFriend = class(DBIInterfaces.TDBIDataConnection);


// _____________________________________________________________________________
{**
  Jvr - 17/09/2016 13:58:35 - XE4 Conversion.<P>
}
function TDBIDataset.GetActiveBuffer: TDBIRecBuf;
begin
  Result := TDBIRecBuf(ActiveBuffer);
end;

function TDBIDataset.GetCalcBuffer: TDBIRecBuf;
begin
  Result := TDBIRecBuf(CalcBuffer);
end;

function TDBIDataset.GetTempBuffer: TDBIRecBuf;
begin
  Result := TDBIRecBuf(TempBuffer);
end;


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 15:38:53 - Added support for nested datasets.<P>
}
constructor TDBIDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInternalOptions := [];
  FRanged := False;
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  FPacketRecords := AllRecords;
  FFetchOnDemand := True;
{$ifdef _UNUSED}
  FParams := TParams.Create(Self);
{$endif}
{$ifdef _AGGREGATES}
  FAggregates := TDBIAggregates.Create(Self);
  FActiveAggLists := TList.Create;
{$endif}
{$ifdef _UNUSED}
  FOpeningFile := False;
  FDisableStringTrim := False;
{$endif}
  ObjectView := True;
end;


// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 17:34:05<P>
}
destructor TDBIDataset.Destroy;
begin
  // Because of the creation order of DSBase inherited needs to be called first
  FSavePacketOnClose := False;

  inherited Destroy;

  ClearSavedPacket;
{$ifdef _UNUSED}
  FreeDataPacket(FDeltaPacket);
  SetRemoteServer(nil);
  SetConnectionBroker(nil);
  AppServer := nil;
{$endif}

  FMasterLink.Free;
  FIndexDefs.Free;
  
{$ifdef _UNUSED}
  FParams.Free;
{$endif}

{$ifdef _AGGREGATES}
  FAggregates.Free;
  ClearActiveAggs;
  FActiveAggLists.Free;
  FAggFieldsUpdated.Free;
{$endif}

  FDataConnection.Free;
  FDataConnection := nil;
end;  { Destroy }




// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 10:56:31<P>
}
function TDBIDataset.CreateDSBase: IDBIBase;
begin
  Assert(Assigned(FDataConnection));
  Result := TDBIBase.Create(FDataConnection);
end;


// _____________________________________________________________________________
{**
  Jvr - 26/10/2000 12:39:37<P>
}
function TDBIDataset.CreateDSCursor(SourceCursor: IDBICursor): IDBICursor;
begin
  Result := TDBICursor.Create;

  if Assigned(SourceCursor) then
    Check(Result.CloneCursor(SourceCursor))
  else
    Check(Result.InitCursor(FDSBase));
end;

{$ifdef _UNUSED}
procedure TDBIDataset.SetCommandText(Value: WideString);
var
  SQL: String;
  List: TParams;
begin
  if FCommandText <> Value then
  begin
    FCommandText := Value;
    if Value <> '' then
    begin
      List := TParams.Create(Self);
      try
        SQL := copy(Value, 1, Length(Value));
        List.ParseSQL(SQL, True);
        List.AssignValues(FParams);
        FParams.Clear;
        FParams.Assign(List);
      finally
        List.Free;
      end;
    end
    else
      FParams.Clear;
  end;
end;

procedure TDBIDataset.SetDisableStringTrim(Value: Boolean);
begin
  CheckInactive;
  FDisableStringTrim := Value;
end;

procedure TDBIDataset.SetParams(Value: TParams);
begin
  FParams.Assign(Value);
end;
{$endif}

procedure TDBIDataset.SetOptionalParam(const ParamName: string;
  const Value: OleVariant; IncludeInDelta: Boolean);
begin
  InternalSetOptionalParam(ParamName, Value, IncludeInDelta, 0);
end;

procedure TDBIDataset.InternalSetOptionalParam(const ParamName: string;
  const Value: OleVariant; IncludeInDelta: Boolean; FieldNo: Integer);
begin
  //##NOP
end;
{$ifdef _UNUSED}
const
  ParamTypeMap: array[varSmallInt..varInt64] of Integer =
    ( dsfldINT, dsfldINT, dsfldFLOATIEEE, dsfldFLOATIEEE, dsfldBCD,
      dsfldFLOATIEEE, dsfldZSTRING, 0, 0, dsfldBOOL, 0, 0, 0, 0, 0, dsfldINT,
      dsfldINT, dsfldINT, dsfldFLOATIEEE);
  ParamTypeSize: array[varSmallInt..varInt64] of Integer =
    ( SizeOf(SmallInt), SizeOf(Integer), SizeOf(Single), SizeOf(Double),
      SizeOf(Currency), SizeOf(TDateTime), 0, 0, 0, SizeOf(WordBool), 0, 0, 0,
      0, 0, SizeOf(Byte), SizeOf(SmallInt), SizeOf(Integer), SizeOf(Int64));
var
  ParamType, ParamLen, t, l: DWord;
  S: string;
  PValue: Pointer;
  Unlock: Boolean;
  TimeStr: string;
  TimeStampRec: TSQLTimeStamp;
  FByteBuffer: array of Byte;
begin
  if not Assigned(FDSBase) then CheckActive;
  if ((VarType(Value) and varTypeMask) in [varSmallInt, varInteger, varSingle,
      varDouble, varCurrency, varDate, varOleStr, varBoolean, varByte, varWord,
      varLongWord, varInt64]) and
     ((not VarIsArray(Value)) or (VarType(Value) and varTypeMask = varByte)) then
  begin
    Unlock := False;
    try
      ParamType := ParamTypeMap[VarType(Value) and varTypeMask];
      ParamLen := ParamTypeSize[VarType(Value) and varTypeMask];
      if varType(Value) = varOleStr then
      begin
        TimeStr := VarToStr(Value);
        if ((Pos(FormatSettings.TimeSeparator, TimeStr) > 0) or (Pos(FormatSettings.DateSeparator, TimeStr) > 0)) and
          TryStrToSqlTimeStamp(TimeStr, TimeStampRec) then
        begin
          ParamType := dsfldTIMESTAMP;
          ParamLen := sizeof(TSQLTimeStamp);
        end;
      end;
      if ParamType = dsfldZSTRING then
      begin
        S := Value;
        PValue := PAnsiChar(S);
        ParamLen := Length(S) + 1;
      end
      else if VarIsArray(Value) then
      begin
        ParamType := dsfldBYTES;
        ParamLen := 1 + (VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1));
        SetLength(FByteBuffer,ParamLen+sizeof(Integer));
        PInteger(FByteBuffer)^ := ParamLen;
        PValue := VarArrayLock(Value);
        try
          Move(PValue^, FByteBuffer[SizeOf(Integer)], ParamLen);
          Inc(ParamLen, SizeOf(Integer));
        finally
          VarArrayUnlock(Value);
        end;
        PValue := FByteBuffer;
      end
      else if (VarType(Value) and varByRef) = varByRef then
        PValue := TVarData(Value).VPointer
      else
        PValue := @TVarData(Value).VPointer;
      ParamType := ParamType shl dsSizeBitsLen or ParamLen;
      if IncludeInDelta then
        ParamType := ParamType or dsIncInDelta;
      Name := PAnsiChar(ParamName);
      if FDSBase.GetOptParameter(0, FieldNo, Name, t, l, v) = 0 then
        Check(FDSBase.DropOptParameter(FieldNo, PAnsiChar(ParamName)));
      Check(FDSBase.AddOptParameter(FieldNo, PAnsiChar(ParamName), ParamType, ParamLen, P));
    finally
      if Unlock then
        VarArrayUnlock(Value);
    end;
  end
  else
    DatabaseError(SInvalidOptParamType, Self);
end;
{$endif}
function TDBIDataset.GetOptionalParam(const ParamName: string): OleVariant;
begin
  Result := InternalGetOptionalParam(ParamName);
end;

function TDBIDataset.InternalGetOptionalParam(const ParamName: string;
  FieldNo: Integer = 0): OleVariant;
begin
  VarClear(Result{%H-});
end;

{$ifdef _UNUSED}
var
  ParamType, ParamLen: LongWord;
  PValue, PNameOut, PVarData: Pointer;

begin
  if not Assigned(FDSBase) then CheckActive;
  VarClear(Result);
  PNameOut := PAnsiChar(ParamName);// Passed as "var" to GetOptParameter so we need a temp.
  if FDSBase.GetOptParameter(0, FieldNo, PNameOut, ParamType, ParamLen, PValue) <> 0 then
    Exit;
  ParamType := (ParamType and dsTypeBitsMask) shr dsSizeBitsLen;
  if (ParamType = dsfldBYTES) or
     ((ParamType in [dsfldINT, dsfldUINT]) and (ParamLen > 4 )) then
    begin
      Result := VarArrayCreate([0, ParamLen-sizeof(Integer)], varByte);
      PVarData := VarArrayLock(Result);
      try
        Move((PByte(PValue) + SizeOf(Integer))^, PVarData^, ParamLen-SizeOf(Integer));
      finally
        VarArrayUnlock(Result);
      end;
    end
    else
    begin
      case ParamType of
        dsfldINT,
        dsfldUINT:
        begin
          case ParamLen of
            1: Result := Byte(PValue^);
            2: Result := SmallInt(PValue^);
            4: Result := Integer(PValue^);
          end;
        end;
        dsfldBOOL: Result := WordBool(PValue^);
        dsfldSINGLE: Result := Single(PValue^);
        dsfldFLOATIEEE: Result := Double(PValue^);
        dsfldBCD: Result := Currency(PValue^);
        dsfldDATE: Result := TDateTimeRec(PValue^).Date - DateDelta;
        dsfldTIME: Result := TDateTimeRec(PValue^).Time / MSecsPerDay;
        dsfldTIMESTAMP: Result := (TDateTimeRec(PValue^).DateTime / MSecsPerDay) - DateDelta;
        dsfldDATETIME: Result := VarSQLTimeStampCreate(TSQLTimeStamp(PValue^));
        dsfldDATETIMEOFFSET: Result := VarSQLTimeStampOffsetCreate(TSQLTimeStampOffset(PValue^));
        dsfldZSTRING,
        dsfldUNICODE: Result := UTF8ToUnicodeString(PValue);
      else
        VarClear(Result);
      end;
    end;
end;
{$endif}


procedure TDBIDataset.OpenCursor(InfoQuery: Boolean);

{$ifdef _UNUSED}
  procedure CheckCircularLinks;
  var
    ProvComp: TComponent;
  begin
    if Assigned(MasterSource) and Assigned(Owner) and (not Assigned(RemoteServer))
       and (ProviderName <> '') and (not Assigned(ConnectionBroker)) then
    begin
      ProvComp := Owner.FindComponent(ProviderName);
      if Assigned(ProvComp) and (ProvComp is TDataSetProvider) and
                 Assigned(MasterSource.DataSet) and
                 Assigned(TDataSetProvider(ProvComp).DataSet)  then
        if TDataSetProvider(ProvComp).DataSet = MasterSource.DataSet then
          DatabaseError(SCircularDataLink, MasterSource.DataSet);
    end;
  end;

var
  RecsOut: Integer;
  Options: TGetRecordOptions;
  DataPacket: TDataPacket;
  Stream: TFileStream;
{$endif}

begin
  FProviderEOF := True;
  FSavePacketOnClose := False;
{$ifdef _UNUSED}
  CheckCircularLinks;
  if not FOpeningFile and (FileName <> '') and FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      ReadDataPacket(Stream, False);
    finally
      Stream.Free;
    end;
  end;
{$endif}
  if DataSetField <> nil then
  begin
    FParentDataSet := DataSetField.DataSet as TDBIDataSet;
    OpenParentDataSet(FParentDataSet);
    FParentDataSet.Check(FParentDataSet.DSBase.GetEmbeddedDS(DataSetField.FieldNo, FDSBase));
    FieldDefs.HiddenFields := FParentDataSet.FieldDefs.HiddenFields;
  end

  // This is only called on an open (or Active := True)
  // IOW      - If CreateDataset is called then this is NOT called
  // Notabene - This is still called by the designer when the dataset has
  //            Active = True (dfm)
  else if not Assigned(FDSBase) then
  begin
{$ifdef _UNUSED}
    if Assigned(FSavedPacket) then
      DataPacket := FSavedPacket
    else
    begin
      Options := [grMetaData];
      DataPacket := VarToDataPacket(DoGetRecords(FPacketRecords, RecsOut,
        Byte(Options), CommandText, PackageParams(Params)));
      ProviderEOF := RecsOut <> FPacketRecords;
    end;
    if not Assigned(DataPacket) then DatabaseError(SNoDataProvider, Self);
{$endif}

    // ##JVR - 27/05/2002 16:11:31
    // Raise the equivalent exception of no provider in the
    // Dataconnection.ValidateMetadata method
    Assert(FDataConnection <> nil);
    if not (ioLoading in FInternalOptions) then begin
      if not (csDesigning in ComponentState) then FDataConnection.ValidateMetaData;
    end;

//##WIP
    FDSBase := CreateDSBase;
    if (FieldDefs.Count > 0) then begin
      FDSBase.EncodeFieldDescs(FieldDefs);
//##JVR      Check(FDSBase.CreateDS(FieldDefs.Count, pDSFLDDesc(FieldDescs), TDBINameBuffer(TDBIString(Name))));
    end;
//##WIP


{$ifdef _UNUSED}
    Check(FDSBase.AppendData(DataPacket, ProviderEOF));
{$endif}
  end;

  inherited OpenCursor(InfoQuery);

  if not InfoQuery and Assigned(FCloneSource) and not FCloneSource.BOF then
  begin
    SyncCursors(FDSCursor, FCloneSource.FDSCursor);
    CursorPosChanged;
    Resync([]);
  end;

  { DSBase now has the data packet so we don't need to hold on to it }

  { We are using the Loaded() method to load the data packet into the dataset
    This means we cannot clear the packet here but must do it in the loaded() method.

  ClearSavedPacket;
  }
  FSavePacketOnClose := True;
end;


{$ifdef _UNUSED}
procedure TDBIDataset.DoAfterGetParams(var OwnerData: OleVariant);
begin
  if Assigned(FAfterGetParams) then FAfterGetParams(Self, OwnerData);
end;

procedure TDBIDataset.DoBeforeGetParams(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeGetParams) then FBeforeGetParams(Self, OwnerData);
end;

procedure TDBIDataset.FetchParams;
var
  OwnerData: OleVariant;
begin
  DoBeforeGetParams(OwnerData);
  UnpackParams(AppServer.AS_GetParams(ProviderName, OwnerData), Params);
  DoAfterGetParams(OwnerData);
end;

procedure TDBIDataset.Check(Status: DBIResult);
var
  ErrMsg: array[0..2048] of AnsiChar;
begin
  if Status <> 0 then
  begin
    FDSBase.GetErrorString(Status, ErrMsg);
    raise EDBClient.Create(ErrMsg, Status);
  end;
end;
{$endif}

procedure TDBIDataset.Check(Status: DBIResult);
const
  ErrUnknown = 'An unexpected error occurre in the DBI engine while performing a dataset operation';

var
  ErrMsg: String;

begin
  if Status <> 0 then
  begin
    ErrMsg := LoadStr(Status);
    if (ErrMsg = '') then ErrMsg := ErrUnknown;
    raise EDBIDatasetError.Create(ErrMsg, Status);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 26/10/2000 13:24:56<P>
}
{ TODO 5 -oJvr -cTDBIDataSet.CloseCursor() :
  26/10/2000
  CloseCursor should be partially implemented in the derived class as it contains
  implementation specific stuff for saving to a file.
}
procedure TDBIDataset.CloseCursor;
{$ifdef _UNUSED}
var
  Params: OleVariant;
  RecsOut: Integer;
  Options: TGetRecordOptions;
  ChangesMade: LongWord;
{$endif}
begin
{$ifdef _UNUSED}
  ChangesMade := NativeUInt(True);
  if Assigned(FDSBase) then
    FDSBase.GetProp(dspropDATAHASCHANGED, PDBIPropValue(@ChangesMade));
  if (FileName <> '') and not (csDesigning in ComponentState) and
     (LongBool(ChangesMade) or not(FileExists(FileName))) then
    SaveToFile(FileName);
{$endif}

  inherited CloseCursor;

{$ifdef _UNUSED}
  if HasAppServer then
  begin
    if not (csDestroying in ComponentState) then
    begin
      if FMasterLink.Active and (FMasterLink.Fields.Count > 0) and
        (PacketRecords = 0) then
        Params := Null
      else
        Params := Unassigned;
      if not (doNoResetCall in FDSOptions) then
      begin
        Options := [grReset];
        DoGetRecords(0, RecsOut, Byte(Options), '', Unassigned);
      end;
      FAppServer := nil;
    end;
  end
  else if FSavePacketOnClose and (FileName = '') and (ProviderName = '') and
     (FParentDataSet = nil) then
    SaveDataPacket;
{$endif}

  FLookupCursor := nil;
  FFindCursor := nil;
  FDSCursor := nil;

  // I put this here because if locks are left hanging an exception is raised
  // and the application then causes an access violation.
  try
    FDSBase := nil;
  except
    InternalHandleException;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 08/10/2002 17:45:33.<P>
}
procedure TDBIDataset.DefChanged(Sender: TObject);
begin
  if not (ioCreateFields in FInternalOptions) then begin
    FStoreDefs := True;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 01/12/2000 15:10:27.<P>
}
procedure TDBIDataset.InternalInitFieldDefs;
var
  FieldID, I: Integer;
  FieldDescs: TFieldDescList;
  CursorProps: DSProps;
begin
{$ifdef _UNUSED}
  FDSBase.SetProp(dspropCOMPRESSARRAYS, TDBIPropValue(True));
{$endif}
  Check(FDSBase.GetProps(CursorProps));
  SetLength(FieldDescs, CursorProps.iFields);
  Check(FDSBase.GetFieldDescs(PDSFldDesc(FieldDescs)));
  FieldDefs.Clear;
  I := 0;
  FieldID := 1;
  while I < CursorProps.iFields do
    AddFieldDesc(FieldDescs, I, FieldID, FieldDefs);
end;

procedure TDBIDataset.CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef);
const
{$ifdef fpc}
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    ftUnknown, ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat,
    ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftBytes, ftVarBytes,
    ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor,
    ftFixedChar, ftWideString, ftLargeint, ftADT, ftArray, ftReference, ftDataSet,
    ftOraBlob, ftOraClob, ftVariant, ftInterface, ftIDispatch, ftGuid,
    ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo
    );
{$else}
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    ftUnknown, ftString, ftInteger, ftInteger, ftInteger, ftBoolean, ftFloat,  // 0..6
    ftFloat, ftBCD, ftDateTime, ftDateTime, ftDateTime, ftBytes, ftVarBytes, // 7..13
    ftInteger, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftBlob, ftUnknown, // 14..22
    ftString, ftString, ftLargeInt, ftADT, ftArray, ftReference, ftDataSet, // 23..29
    ftBlob, ftBlob, ftVariant, ftInterface, ftInterface, ftString // 30..35
  {$ifdef DELPHI2006}
    ,
    ftTimeStamp, ftFMTBcd, // 36..37
    ftString, ftBlob, ftOraTimeStamp, ftOraInterval // 38..41
  {$endif}
  {$ifdef DELPHI2009}
    ,
    ftLongWord, ftInteger, ftInteger, db.ftExtended, ftConnection, ftParams, ftStream, //42..48
    ftTimeStampOffset, ftObject, db.ftSingle // 49..51
  {$endif}
    );
{$endif}
  CheckTypeSizes = [ftBytes, ftVarBytes, ftBCD, ftReference];

  CheckTypeSizesSmaller = [ftString, ftFixedChar, ftWideString{$ifdef DELPHI2006} , ftFixedWideChar {$endif}];

begin
  // If NO FieldDefs or Fields have been defined
  if DefaultFields then begin
    if (BaseFieldTypes[Field.DataType] <> BaseFieldTypes[FieldDef.DataType]) then
      DatabaseErrorFmt(SFieldTypeMismatch, [Field.DisplayName,
        FieldTypeNames[Field.DataType], FieldTypeNames[FieldDef.DataType]], Self);
    if (Field.DataType in CheckTypeSizes) and (Field.Size <> FieldDef.Size) then
      DatabaseErrorFmt(SFieldSizeMismatch, [Field.DisplayName, Field.Size,
        FieldDef.Size], Self);
    // ME - 5/17/04 - Ensure writable character fields are not too large (Amended D7 patch)
    if ((Field.DataType in CheckTypeSizesSmaller) and (Field.Size > FieldDef.Size) and not Field.ReadOnly) then
      Field.Size := FieldDef.Size;
  end

  // Otherwise if custom FieldDefs and / or Fields have been defined
  else begin
    inherited CheckFieldCompatibility(Field, FieldDef);

    //##JVR - 18/01/2013 - Added size check for character fields
    if (Field.DataType in CheckTypeSizesSmaller) and (Field.Size <> FieldDef.Size) then
      DatabaseErrorFmt(SFieldSizeMismatch, [Field.DisplayName, Field.Size,
        FieldDef.Size], Self);
  end;
end;


{$ifndef DELPHI6}
function VarIsClear(const V: Variant): Boolean;
begin
  Result := VarIsEmpty(V);
end;
{$endif}


{$ifdef fpc}
procedure TDBIDataset.CheckFieldProps;
begin
  //##NOP
end;
{$else}
type
  TPropReader = class(TReader);

procedure TDBIDataset.CheckFieldProps;

  procedure GetTypeName(Field: TObjectField);
  var
    V: Variant;
    i: Integer;
  begin
    V := InternalGetOptionalParam(szTYPENAME, Field.FieldNo);
    if not VarIsNull(V) and not VarIsClear(V) then
      Field.ObjectType := V;
    if Field.DataType in [ftADT, ftArray] then
      for i := 0 to Field.FieldCount - 1 do
        if Field.Fields[i] is TObjectField then
          GetTypeName(TObjectField(Field.Fields[i]));
  end;

var
  V: Variant;
  P: Pointer;
  Stream: TMemoryStream;
  Reader: TPropReader;
  i: Integer;
begin
  Stream := TMemoryStream.Create;
  try
    for i := 0 to FieldCount - 1 do
    begin
      if Fields[i] is TObjectField then
        GetTypeName(TObjectField(Fields[i]));
      V := InternalGetOptionalParam(szORIGIN, Fields[i].FieldNo);
      if not VarIsNull(V) and not VarIsClear(V) then
        Fields[i].Origin := VarToStr(V);
      V := InternalGetOptionalParam(szPROVFLAGS, Fields[i].FieldNo);
      if not (VarIsNull(V) or VarIsClear(V)) then
        Fields[i].ProviderFlags := TProviderFlags(Byte(V));
      V := InternalGetOptionalParam(szFIELDPROPS, Fields[i].FieldNo);
      if VarIsNull(V) or VarIsClear(V) or not VarIsArray(V) then continue;
      Stream.Size := VarArrayHighBound(V, 1);
      P := VarArrayLock(V);
      try
        Stream.Position := 0;
        Stream.Write(P^, Stream.Size);
        Stream.Position := 0;
      finally
        VarArrayUnlock(V);
      end;
      V := NULL;
      Reader := TPropReader.Create(Stream, 1024);
      try
        Reader.ReadListBegin;
        while not Reader.EndOfList do
          Reader.ReadProperty(Fields[i]);
      finally
        Stream.Clear;
        Reader.Free;
      end;
    end;
  finally
    Stream.Free;
  end;
end;
{$endif}

procedure TDBIDataset.InternalOpen;

  function GetBoolParam(const ParamName: string): Boolean;
  var
    V: OleVariant;
  begin
    V := GetOptionalParam(ParamName);
    Result := not VarIsNull(V) and not VarIsClear(V) and (VarType(V) = varBoolean);
    if Result then
      Result := V;
  end;

var
  CursorProps: DSProps;

begin
  if Assigned(FCloneSource) then
    FDSCursor := CreateDSCursor(FCloneSource.FDSCursor)
  else
  begin
{$ifdef _UNUSED}
    if not IsClone then
      SetupInternalCalcFields(True);
{$endif}
    FDSCursor := CreateDSCursor(nil);
  end;

  if DataSetField <> nil then
  begin
    if FParentDataSet.State = dsInActive then
    begin
      {FParentDataSet}DSCursor.MoveToBOF;
      {FParentDataSet}DSCursor.MoveRelative(1);
    end;
    Check({FParentDataSet}DSCursor.LinkCursors(0, nil, nil, Self.FDSCursor));
  end;

  { TODO 1 -oJvr -cTDBIDataSet.InternalOpen() :
    TDatasetOptions
    No support for TDatasetOptions is currently available in the underlying
    Dataconnection.
  }
{$ifdef _UNUSED}
  FDSOptions := [];
  if GetBoolParam(szDISABLE_EDITS) then
    Include(FDSOptions, doDisableEdits);
  if GetBoolParam(szDISABLE_INSERTS) then
    Include(FDSOptions, doDisableInserts);
  if GetBoolParam(szDISABLE_DELETES) then
    Include(FDSOptions, doDisableDeletes);
  if GetBoolParam(szNO_RESET_CALL) then
    Include(FDSOptions, doNoResetCall);
{$endif}

  Check(FDSCursor.GetCursorProps(CursorProps));
  try
    { TODO 5 -oJvr -cTDBIDataSet.InternalOpen() :
      26/10/2000
      This stuff should probably be done in the
      "InternalInitFieldDefs" or "InitBufferPointers", then GetCursorProps
      would only need be called once.
    }
    FRecordSize := CursorProps.iRecBufSize;
    BookmarkSize := CursorProps.iBookmarkSize;
    SetLength(FLastParentBM, BookMarkSize);
    FCanModify := not CursorProps.bReadOnly;
    FDSBase.GetProp(basepropEXCLUSIVE_ACCESS, PDBIPropValue(@FExclusive));

    FieldDefs.Updated := False;
    FieldDefs.Update;
{$ifndef fpc}
    FieldDefList.Update;
{$endif}    
    IndexDefs.Updated := False;
    GetIndexInfo('');

  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'InternalOpen::1951', 'Failed to initialise FieldDefs', []);
  end;

  { Populate Fields from FieldDefs }
  if DefaultFields then CreateFields;
  BindFields(True);

{$ifdef _UNUSED}
  CheckFieldProps;
{$endif}
  AllocKeyBuffers;
  FDSCursor.MoveToBOF;

  if not Assigned(FCloneSource) then
  begin
{$ifdef _UNUSED}
    if InternalCalcFields and not (csDesigning in ComponentState) then
      Check(FDSBase.SetFieldCalculation(TDBIClientData(Self),
        @TDBIDataSet.CalcFieldsCallback));
{$endif}
    if FIndexName <> '' then
       if FFieldsIndex then
         SortOnFields(FDSCursor, FIndexName, False, False)
       else
         SwitchToIndex(FIndexName);

    CheckMasterRange;
{$ifdef _UNUSED}
    if FReadOnly then FDSBase.SetProp(dspropREADONLY, TDBIPropValue(True));
{$endif}
{$ifdef _AGGREGATES}
    ResetAllAggs(FAggregatesActive);
{$endif}
//##AGGREGATES ResetGrouping;  //##AGGREGATES - When Aggregates are fully implemented, remove this!

    // Set Properties
    FDSBase.SetProp(basepropREADONLY, TDBIPropValue(FReadOnly));
    FDSBase.SetProp(basepropEXCLUSIVE_ACCESS, TDBIPropValue(FExclusive));

    if Filtered then ActivateFilters;
  end;

  InitBufferPointers(False);
  if (DataSetField <> nil) and FetchOnDemand then
    CheckDetailRecords;

{$ifdef _UNUSED}
  SetupConstraints;
{$endif}
end;


// _____________________________________________________________________________
{**
  Closes the DataStream. This method is called when Active is set to False.

  InternalClose is called by the Fields Editor in design mode, so the actual
  table may not be open.<P>

  Jvr - 17/09/2001 17:10:56 - Brought the code back in line with TClientDataset.<P>
}
procedure TDBIDataset.InternalClose;
begin
  if Filtered then DeactivateFilters;
  FreeKeyBuffers;
{$ifdef _UNUSED}
  if not Assigned(FCloneSource) then
    SetupInternalCalcFields(False);
{$endif}
  BindFields(False);  // Destroy the TField components if no persistent fields
  if DefaultFields then DestroyFields;
{$ifdef _AGGREGATES}
  CloseAggs;
{$endif}
  FIndexFieldCount := 0;
  FKeySize := 0;
  FDSCursor := nil;
  FLookupCursor := nil;
  FFindCursor := nil;
  FNotifyCallback := False;
end;


// _____________________________________________________________________________
{**
  Jvr - 25/03/2009 11:20:54 - Updated code.<br>

  <b>NOTES:</b>
  <p>This is where we would refresh any buffers we are using
  between the data and TDataSet's internal record buffers.</p>

  <p>In ClientDataset FDSBase.Reset() is used to clear the cached data. In
  TDBIDataset it fullfills the same role as in clearing/purging any data.
  The difference is that the data in TDBIDataset is accessed directly through
  FDSBase and FDSCursor.  This means calling FDSBase.Reset removes all actual
  data.
  It is therefore not appropriate to call it here because we want to
  refresh the data, not remove it.

  I don't believe that not calling it here has any adverse side effects.
  </p>
}
procedure TDBIDataset.InternalRefresh;
var
  SeqNo: LongWord;
{$ifdef _UNUSED}
  RecCountProp: TDBIPropValue;
  RecCount, RecsOut: Integer;
  DataPacket: TDataPacket;
{$endif}
begin
  CheckBrowseMode;

{$ifdef _UNUSED}
  if ChangeCount > 0 then
    DatabaseError(SRefreshError, Self);
{$endif}
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) and
     ((DataSetField <> nil){ or (PacketRecords <> -1)}) then
  begin
{$ifdef _UNUSED}
    FDSBase.Reset;
{$endif}
    if FetchOnDemand then CheckDetailRecords;
  end
  else
  begin
    FDSCursor.GetSequenceNumber(SeqNo);
{$ifdef _UNUSED}
    if not ProviderEOF then
    begin
      FDSBase.GetProp(dspropRECORDSINDS, PDBIPropValue(@RecCountProp));
      RecCount := RecCountProp;
    end
    else
       RecCount := AllRecords;

    DataPacket := VarToDataPacket(DoGetRecords(RecCount, RecsOut, Byte(Options), '', Unassigned));
    ProviderEOF := RecsOut <> RecCount;
    FDSBase.Reset;
    FDSBase.SetProp(dspropDSISPARTIAL, TDBIPropValue(False));
    Check(FDSBase.AppendData(DataPacket, ProviderEOF));
{$endif}

    FDSCursor.MoveToSeqNo(SeqNo);
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 26/10/2000 13:22:08.<P>
}
function TDBIDataset.IsCursorOpen: Boolean;
begin
  Result := FDSCursor <> nil;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 17:14:11.<P>
}
procedure TDBIDataset.InternalHandleException;
begin
{$ifndef DELPHI6}
  Application.HandleException(Self);
{$else}
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Self);
{$endif}
end;


function TDBIDataset.GetData: TDBIDataPacket;
begin
  if Active then begin
    CheckBrowseMode;
    FDSBase.SetProp(basepropXML_STREAMMODE, TDBIPropValue(xmlON)); //##JVR TDBIPropValue(xmlOFF));
    ClearSavedPacket;
    FSavedPacket := TDBIXmlData.CreateDataPacket(Self);
{$ifdef _UNUSED}
    Check(FDSBase.StreamDS(DataPacket));
  end
  else begin
    SafeArrayCheck(SafeArrayCopy(FSavedPacket, DataPacket));
{$endif}
  end;

{$ifdef _UNUSED}
  DataPacketToVariant(DataPacket, Result);
{$endif}
//## FSavedPacket is not assigned a value if we're not "Active".
  Result := FSavedPacket;
end;


procedure TDBIDataset.SetData(const Value: TDBIDataPacket);
begin
  FSavePacketOnClose := False;
  Close;
  ClearSavedPacket;
  if Assigned(Value) then
  begin
    FSavedPacket := TDBIXmlData.CreateDataPacket(Value);
    TDBIXmlData.LoadFromDataPacket(FSavedPacket, Self);
{$ifdef _UNUSED}
    SafeArrayCheck(SafeArrayCopy(VarToDataPacket(Value), FSavedPacket));
    Open;
{$endif}
  end;
end;


function TDBIDataset.GetXMLData: string;
begin
  if Active then begin
    CheckBrowseMode;
    FDSBase.SetProp(basepropXML_STREAMMODE, TDBIPropValue(xmlON));

    Result := TDBIXmlData.DatasetToXmlString(Self);
{$ifdef _UNUSED}
    Check(FDSBase.StreamDS(DataPacket));
    DataPacketToVariant(DataPacket, VarPacket);
    Result := VariantArrayToString(VarPacket);
{$endif}
  end
  else begin
    Result := '';
  end;
end;


procedure TDBIDataset.SetXMLData(const Value: string);
var
  DataPacket: TDBIDataPacket;

begin
  DataPacket := TDBIXmlData.CreateDataPacket(Value);
  try
    SetData(DataPacket);
  finally
    TDBIXmlData.FreeDataPacket(DataPacket);
  end;
end;


procedure TDBIDataset.ClearSavedPacket;
begin
  TDBIXmlData.FreeDataPacket(FSavedPacket);
end;


procedure TDBIDataset.SaveDataPacket(Format: TDBIDataFormat = dfXML);
const
  StreamMode: array[Boolean] of TDBIPropValue = (xmlOFF, xmlON);
begin
  if Assigned(FDSBase) and (DataSetField = nil) then
  begin
    FDSBase.SetProp(basepropXML_STREAMMODE, TDBIPropValue(StreamMode[Format=dfXML]));
    ClearSavedPacket;
    FSavedPacket := TDBIXmlData.CreateDataPacket(Self);
{$ifdef _UNUSED}
    Check(FDSBase.StreamDS(FSavedPacket));
{$endif}
  end;
end;


function TDBIDataset.GetDataSize: Integer;
begin
  if Assigned(DataSetField) then
    Result := -1
  else if Active then
  begin
    SaveDataPacket;
    Result := TDBIXmlData.DataPacketSize(FSavedPacket);
    ClearSavedPacket;
  end
  else if Assigned(FSavedPacket) then
    Result := TDBIXmlData.DataPacketSize(FSavedPacket)
  else
    Result := 0;
end;


{$ifdef _UNUSED}
procedure TDBIDataset.FetchMoreData(All: Boolean);
var
  Count: Integer;
  RecsOut: Integer;
begin
  if All then
    Count := AllRecords
  else
    Count := FPacketRecords;

  if Count = 0 then Exit;
  AddDataPacket(DoGetRecords(Count, RecsOut, 0, '', Unassigned), RecsOut <> Count);
  ProviderEOF := RecsOut <> Count;
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 16:32:59 - Initial code.<br>
  Jvr - 31/10/2010 16:32:59 - only allow "foDetails" calls.<br>
}
procedure TDBIDataset.InternalFetch(Options: TFetchOptions);
{$ifdef _UNUSED}
var
  DataPacket: TDataPacket;
  NewData: OleVariant;
  BaseDS: TClientDataSet;
{$endif}
begin
  if (foRecord in Options) or (foBlobs in Options) then begin
    raise EDBIException.Create(Self, 'InternalFetch::2235',
      'Only "Option = foDetails" is permitted, "foRecord, foBlobs" NOT implemented', []
      );
  end;

  CheckActive;
  UpdateCursorPos;

{$ifdef _UNUSED}
  Check(DSCursor.GetRowRequestPacket(foRecord in Options, foBlobs in Options,
    foDetails in Options, True, DataPacket));
  DataPacketToVariant(DataPacket, NewData);
  BaseDS := Self;
  while Assigned(BaseDS.FParentDataSet) do BaseDS := BaseDS.FParentDataSet;
  NewData := BaseDS.DoRowRequest(NewData, Byte(Options));
  UpdateCursorPos;
  Check(DSCursor.RefreshRecord(VarToDataPacket(NewData)));
{$endif}

  if not Active then Exit;
  DSCursor.GetCurrentRecord(GetActiveBuffer);

  if Options = [foDetails] then
    DataEvent(deDataSetChange, 0);
end;

procedure TDBIDataset.FetchBlobs;
begin
  InternalFetch([foBlobs]);
end;

procedure TDBIDataset.FetchDetails;
begin
  InternalFetch([foDetails]);
end;

procedure TDBIDataset.RefreshRecord;
begin
  InternalFetch([foRecord]);
  Resync([]);
end;

procedure TDBIDataset.CheckProviderEOF;
begin
{$ifdef _UNUSED}
  if HasAppServer and not ProviderEOF and FFetchOnDemand and (FPacketRecords <> 0) then
    FetchMoreData(True);
{$endif}
end;

{$ifdef _UNUSED}
procedure TDBIDataset.AddDataPacket(const Data: OleVariant; HitEOF: Boolean);
begin
  Check(FDSBase.AppendData(VarToDataPacket(Data), HitEOF));
end;

procedure TDBIDataset.AppendData(const Data: OleVariant; HitEOF: Boolean);
begin
  if not Active then
  begin
    Self.Data := Data;
    if not HitEOF then
      FDSBase.SetProp(dspropDSISPARTIAL, TDBIPropValue(False));
  end
  else
  begin
    AddDataPacket(Data, HitEOF);
    if State <> dsBrowse then Exit;
    if IsEmpty then
      First
    else
    begin
      UpdateCursorPos;
      Resync([]);
    end;
  end;
end;

function TDBIDataset.GetNextPacket: Integer;
begin
  CheckActive;
  if ProviderEOF then
    Result := 0
  else
  begin
    UpdateCursorPos;
    if (FPacketRecords = 0) and FMasterLink.Active and
       (FMasterLink.Fields.Count > 0) then CheckDetailRecords
    else
    begin
      AddDataPacket(DoGetRecords(FPacketRecords, Result, 0, '', Unassigned),
        Result <> FPacketRecords);
      ProviderEOF := Result <> FPacketRecords;
    end;
    Resync([]);
  end;
end;

procedure TDBIDataset.SetProviderName(const Value: string);
begin
  if Value = FProviderName then Exit;
  if (Value <> '') then
  begin
    CheckInactive;
    ClearSavedPacket;
  end;
  FAppServer := nil;
  FProviderName := Value;
end;

procedure TDBIDataset.SetProvider(Provider: TComponent);
begin
  if Provider is TCustomProvider then
    AppServer := TLocalAppServer.Create(TCustomProvider(Provider))
  else if Provider is TDataset then
    AppServer := TLocalAppServer.Create(TDataset(Provider))
  else
    AppServer := nil;
end;

function TDBIDataset.GetAppServer: IAppServer;
var
  ProvComp: TComponent;
  DS: TObject;
begin
  if not HasAppServer then
  begin
    if ProviderName <> '' then
      if Assigned(RemoteServer) then
        FAppServer := RemoteServer.GetServer
      else if Assigned(ConnectionBroker) then
        FAppServer := ConnectionBroker.GetServer
      else
      begin
        if Assigned(Owner) then
        begin
          ProvComp := Owner.FindComponent(ProviderName);
          if Assigned(ProvComp) and (ProvComp is TCustomProvider) then
          begin
            DS := GetObjectProperty(ProvComp, 'DataSet');
            if Assigned(DS) and (DS = Self) then
              DatabaseError(SNoCircularReference, Self);
            FAppServer := TLocalAppServer.Create(TCustomProvider(ProvComp));
          end;
        end;
      end;
    if not HasAppServer then
      DatabaseError(SNoDataProvider, Self);
  end;
  Result := FAppServer;
end;


function TDBIDataset.GetHasAppServer: Boolean;
begin
  Result := Assigned(FAppServer);
end;


procedure TDBIDataset.SetAppServer(Value: IAppServer);
begin
  FAppServer := Value;
  if Assigned(Value) then
    ClearSavedPacket;
end;
{$endif}


procedure TDBIDataset.SetProviderEOF(Value: Boolean);
begin
  FProviderEOF := Value;
  if Assigned(FCloneSource) then
    FCloneSource.ProviderEOF := Value;
end;


function TDBIDataset.GetProviderEOF: Boolean;
begin
  if Assigned(FCloneSource) then
    FProviderEOF := FCloneSource.ProviderEOF;
  Result := FProviderEOF;
end;


{$ifdef _UNUSED}
function TDBIDataset.GetRemoteServer: TCustomRemoteServer;
begin
  Result := FRemoteServer;
end;

procedure TDBIDataset.SetRemoteServer(Value: TCustomRemoteServer);
begin
  if Value = FRemoteServer then Exit;
  if Assigned(Value) and Assigned(ConnectionBroker) then
    SetConnectionBroker(Nil);
  AppServer := nil;
  if Assigned(FRemoteServer) then FRemoteServer.UnRegisterClient(Self);
  FRemoteServer := Value;
  if Assigned(Value) then
  begin
    CheckInactive;
    Value.RegisterClient(Self);
    ClearSavedPacket;
    Value.FreeNotification(Self);
  end;
  FRemoteServer := Value;
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 12/03/2009 10:19:30 - Initial code.<br>
}
procedure TDBIDataset.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

{$ifdef _UNUSED}
  if (Operation = opRemove) and (AComponent = RemoteServer) then
    RemoteServer := nil;
{$endif}

  if (Operation = opRemove) and (AComponent = FCloneSource) then
  begin
    FProviderEOF := FCloneSource.ProviderEOF;
    FCloneSource := nil;
  end;
  
{$ifdef _UNUSED}
  if (Operation = opRemove) and (AComponent = FConnectionBroker) then
    FConnectionBroker:= nil;
{$endif}
end;



// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 19:27:09 - Initial code.<br />
}
procedure TDBIDataset.DataEvent(Event: TDataEvent; Info: TDBIDataEventInfo);
begin
{$ifdef _AGGREGATES}
  case Event of
{$ifdef _UNUSED}
    deParentScroll: MasterChanged(Self);
    deDataSetScroll,
    deDataSetChange: SetAltRecBuffers(nil, nil, nil);
{$endif}
    deFieldListChange: FAggFieldsInit := False;
{$ifdef _UNUSED}
    deConnectChange:
      if not LongBool(Info) then
        AppServer := nil;
{$endif}
  end;
{$endif}

  inherited;
end;


{$ifdef _UNUSED}
function TDBIDataset.GetDelta: OleVariant;
var
  FDeltaDS: IDSBase;
  TempPacket: TDataPacket;
begin
  CheckBrowseMode;
  Check(FDSBase.GetDelta(FDeltaDS));
  FreeDataPacket(FDeltaPacket);
  Check(FDeltaDS.StreamDS(FDeltaPacket));
  SafeArrayCheck(SafeArrayCopy(FDeltaPacket, TempPacket));
  DataPacketToVariant(TempPacket, Result);
end;

procedure TDBIDataset.Execute;
begin
  DoExecute(PackageParams(Params));
end;

procedure TDBIDataset.ExecuteCommand(const ACommand: string;
  const Args: array of const);
var
  MaxErrors: Integer;
begin
  if SameText(ACommand, sApplyUpdatesDataSetCommand) then
  begin
    if Length(Args) = 1 then
      MaxErrors := Args[0].VInteger
    else
      MaxErrors := -1;
    ApplyUpdates(MaxErrors);
  end
  else if SameText(ACommand, sCancelUpdatesDataSetCommand) then
  begin
    CancelUpdates;
  end
  else
    inherited;
end;

function TDBIDataset.DataRequest(Data: OleVariant): OleVariant;
begin
  Result := AppServer.AS_DataRequest(ProviderName, Data);
end;

function TDBIDataset.ApplyUpdates(MaxErrors: Integer): Integer;
var
  RootDataset: TDBIDataset;
begin
  CheckBrowseMode;
  RootDataset := Self;
  while RootDataset.FParentDataSet <> nil do
    RootDataset := RootDataset.FParentDataset;
  if RootDataset.ChangeCount = 0 then
    Result := 0
  else
    RootDataset.Reconcile(RootDataset.DoApplyUpdates(RootDataset.Delta, MaxErrors, Result));
end;

procedure TDBIDataset.MergeChangeLog;
begin
  CheckBrowseMode;
  FDSBase.AcceptChanges;
  UpdateCursorPos;
  Resync([]);
end;
{$endif}


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 10/05/2005 10:12:49 - Initial code.<br>
}
procedure TDBIDataset.SetAltRecBuffers(Old, New, Cur: TDBIRecordBuffer);
begin
  FOldValueBuffer := Old;
  FNewValueBuffer := New;
  FCurValueBuffer := Cur;
end;


{$ifdef _UNUSED}
function TDBIDataset.ReconcileCallback(
    iRslt         : Integer;   { Previous error if any }
    iUpdateKind   : DSAttr;    { Update request Insert/Modify/Delete }
    iResAction    : dsCBRType; { Resolver response }
    iErrCode      : Integer;   { Native error-code, (BDE or ..) }
    pErrMessage,               { Native errormessage, if any (otherwise Null) }
    pErrContext   : Pointer;   { 1-level error context, if any (otherwise Null) }
    pRecUpd,                   { Record that failed update }
    pRecOrg,                   { Original record, if any }
    pRecConflict  : Pointer;   { Conflicting error, if any }
    iLevels       : Integer;   { Number of levels to error0level }
    piFieldIDs    : PInteger   { Array of fieldIDS to navigate to error-dataset }
): dsCBRType;
var
  I: Integer;
  Action: TReconcileAction;
  UpdateKind: TUpdateKind;
  DataSet: TDBIDataset;
  E: EReconcileError;
  ReconcileInfo: TReconcileInfo;
begin
  FInReconcileCallback := True;
  try
    if iUpdateKind = dsRecDeleted then
      UpdateKind := ukDelete
    else if iUpdateKind = dsRecNew then
      UpdateKind := ukInsert
    else
      UpdateKind := ukModify;
    if iResAction = dscbrSkip then
      Action := raSkip
    else
      Action := raAbort;
    FReconcileDataSet.First;
    E := EReconcileError.Create(pErrMessage, pErrContext, iErrCode, iRslt);
    try
      DataSet := FReconcileDataSet;
      for I := 1 to iLevels do
      begin
         DataSet := TDBIDataset((DataSet.Fields.FieldByNumber(piFieldIDs^) as TDataSetField).NestedDataSet);
         inc(piFieldIDs);
      end;
      if UpdateKind = ukDelete then
        DataSet.SetAltRecBuffers(pRecUpd, pRecOrg, pRecConflict)
      else
        DataSet.SetAltRecBuffers(pRecOrg, pRecUpd, pRecConflict);
      ReconcileInfo.DataSet := Dataset;
      ReconcileInfo.UpdateKind := UpdateKind;
      ReconcileInfo.ReconcileError := E;
      ReconcileInfo.ActionRef := @Action;
      DataEvent(deReconcileError, Integer(@ReconcileInfo));
      if Assigned(FOnReconcileError) then
        FOnReconcileError(DataSet, E, UpdateKind, Action);
    finally
      E.Free;
    end;
  except
    InternalHandleException;
    Action := raAbort;
  end;
  Result := Ord(Action) + 1;
  FInReconcileCallback := False;
end;

function TDBIDataset.Reconcile(const Results: OleVariant): Boolean;

  procedure AddFieldProps(RecDataSet, SourceDataSet: TDataSet);
  const
    FieldPropNames: array [0..13] of string = ('ConstraintErrorMessage', 'Currency',          // Do not localize
      'CustomConstraint', 'DisplayFormat', 'DisplayLabel', 'Visible', 'DisplayFormat',       // Do not localize
      'EditFormat', 'ReadOnly', 'MinValue', 'MaxValue', 'currency', 'ReadOnly', 'Required'); // Do not localize
  var
    I, J: Integer;
    SourceField, RecDataField: TField;
    PropVal: variant;
  begin
    RecDataSet.Name := SourceDataSet.Name;
    RecDataSet.Open;
    for I := 0 to RecDataSet.FieldCount - 1 do
    begin
      SourceField := SourceDataSet.FindField(RecDataSet.Fields[I].FieldName);
      if Assigned(SourceField) then
      begin
        { Use type info to copy the field properties to the reconcile dataset }
        RecDataField := RecDataSet.Fields[I];
        for J := Low(FieldPropNames) to High(FieldPropNames) do
          if TypInfo.GetPropInfo(SourceField.ClassInfo, FieldPropNames[J]) <> nil then
          begin
            PropVal := TypInfo.GetPropValue(SourceField, FieldPropNames[J]);
            if not VarIsNull(PropVal) and
                (TypInfo.GetPropValue(RecDataField, FieldPropNames[J]) <> PropVal) then
              TypInfo.SetPropValue(RecDataField, FieldPropNames[J], PropVal);
          end;
        { Recurse into any nested datasets as well }
        if (SourceField.DataType = ftDataSet) and (RecDataField.DataType = ftDataSet) then
          AddFieldProps((RecDataField as TDataSetField).NestedDataSet,
            (SourceField as TDataSetField).NestedDataSet);
      end;
    end;
  end;

var
  RCB: Pointer;
begin
  if VarIsNull(Results) then
    MergeChangeLog
  else
  begin
    UpdateCursorPos;
    RCB := @TDBIDataset.ReconcileCallback;
    FReconcileDataSet := TDBIDataset.Create(Self);
    try
      Check(FDSBase.Clone(0, True, False, FReconcileDataSet.FDSBase));
      FReconcileDataSet.ObjectView := True;
      AddFieldProps(FReconcileDataSet, Self);
      Check(FDSBase.Reconcile_MD(FReconcileDataSet.FDSBase, FDeltaPacket,
        VarToDataPacket(Results), TDBIClientData(Self), RCB));
    finally
      FReconcileDataSet.Free;
      FReconcileDataSet := nil;
    end;
    Resync([]);
  end;
  Result := (ChangeCount = 0);
end;
{$endif}


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 04/12/2000 12:07:28.<P>
}
procedure TDBIDataset.NotifyCallback;
begin
  try
    if State = dsBrowse then
    begin
      UpdateCursorPos;
      Resync([]);
{$ifdef _UNUSED}
      UpdateCursorPos;
{$endif}
    end;
  except
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 04/12/2000 12:07:1S<P>
}
procedure TDBIDataset.SetNotifyCallback;
begin
  if not FNotifyCallback then
  begin
    Check(FDSCursor.SetNotifyCallBack(TDBIClientData(Self), @TDBIDataSet.NotifyCallback));
    FNotifyCallback := True;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 04/12/2000 12:07:08.<P>
}
procedure TDBIDataset.CloneCursor(Source: TDBIDataSet; Reset, KeepSettings: Boolean);
begin
  Source.CheckActive;
  Close;
  FDSBase := Source.DSBase;
  Source.UpdateCursorPos;
  FCloneSource := Source;
  FParentDataSet := Source.FParentDataSet;
  if Reset then
  begin
    Filtered := False;
    Filter := '';
    OnFilterRecord := nil;
    IndexName := '';
    MasterSource := nil;
    MasterFields := '';
    ReadOnly := False;
{$ifdef _UNUSED}
    RemoteServer := nil;
    ProviderName := '';
    AppServer := nil;
{$endif}
  end
  else if not KeepSettings then
  begin
    Filter := Source.Filter;
    OnFilterRecord := Source.OnFilterRecord;
    FilterOptions := Source.FilterOptions;
    Filtered := Source.Filtered;
    if Source.IndexName <> '' then
      IndexName := Source.IndexName
    else
      IndexFieldNames := Source.IndexFieldNames;

    MasterSource := Source.MasterSource;
    MasterFields := Source.MasterFields;
    ReadOnly := Source.ReadOnly;
{$ifdef _UNUSED}
    RemoteServer := Source.RemoteServer;
    ProviderName := Source.ProviderName;
    if Source.HasAppServer then
      AppServer := Source.AppServer;
{$endif}
  end;
  Open;
  if Reset then
  begin
    if Source.FExprFilter <> nil then FDSCursor.DropFilter(Source.FExprFilter);
    if Source.FFuncFilter <> nil then FDSCursor.DropFilter(Source.FFuncFilter);
    CancelRange;
    Resync([]);
  end;

//##JVR - Callbacks not supported at this stage.
//##JVR   I'm not sure what purpose they would serve here.
{$ifdef _UNUSED}
  SetNotifyCallback;
  Source.SetNotifyCallback;
{$endif}
end;


// _____________________________________________________________________________
{**
  Jvr - 01/12/2000 14:59:30<BR>
  Jvr - 02/02/2001 17:27:58 - Added floating point precision for Xbase<P>
}
procedure TDBIDataset.EncodeFieldDesc(out FieldDesc: DSFLDDesc;
  const Name: string; DataType: TFieldType; Size, Precision: Integer;
  Calculated: Boolean; Attributes: TFieldAttributes);
begin
  FillChar(FieldDesc{%H-}, SizeOf(FieldDesc), #0);
  FieldDesc.szName := TDBIString(Name);

  FieldDesc.iFldType := FieldTypeMap[DataType];

  if FieldDesc.iFldType = fldWIDESTRING  then
    FieldDesc.iFldType := fldUNICODE;

  FieldDesc.iFldSubType := FldSubTypeMap[DataType];
  FieldDesc.bCalculated := Calculated;
  FieldDesc.iFldAttr := Integer(PByte(@Attributes)^);

  case DataType of
    ftADT, ftArray, ftDataSet, ftString, ftFixedChar, ftGUID, ftBytes,
    ftVarBytes, ftBlob..ftTypedBinary, ftOraClob, ftOraBlob:
      FieldDesc.iUnits1 := Size;

    ftWideString {$ifdef DELPHI2006}, ftFixedWideChar, ftWideMemo {$endif} :
      FieldDesc.iUnits1 := Size * 2;

    ftBCD {$ifdef DELPHI6} , ftFMTBcd {$endif} :
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
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 28/03/2000 11:24:07.<BR>
  Jvr - 26/10/2000 11:35:16 - We now use Interfaces<P>
}
procedure TDBIDataset.CreateDataset;

  procedure GetFieldDefCount(FieldDefs: TFieldDefs; var Count: Integer);
  var
    I: Integer;
  begin
    Inc(Count, FieldDefs.Count);
    for I := 0 to FieldDefs.Count - 1 do
      if TDBIFieldDef.HasChildDefs(FieldDefs[I]) then
        GetFieldDefCount(TDBIFieldDef.ChildDefs(FieldDefs[I]), Count);
  end;

  procedure EncodeFieldDescs(FieldDefs: TFieldDefs; FieldDescs: TFieldDescList;
    var DescNo: Integer);
  var
    I: Integer;
  begin
    for I := 0 to FieldDefs.Count - 1 do
    begin
      EncodeFieldDesc(FieldDescs[DescNo], FieldDefs[I].Name, FieldDefs[I].DataType, FieldDefs[I].Size, FieldDefs[I].Precision, False, FieldDefs[I].Attributes);
      Inc(DescNo);

      if TDBIFieldDef.HasChildDefs(FieldDefs[I]) then
      begin
        if FieldDefs[I].DataType = ftDataSet then
          GetFieldDefCount(TDBIFieldDef.ChildDefs(FieldDefs[I]), FieldDescs[DescNo-1].iUnits2);

        EncodeFieldDescs(TDBIFieldDef.ChildDefs(FieldDefs[I]), FieldDescs, DescNo);
      end;
    end;
  end;


{$ifdef _UNUSED}
  procedure CheckLongFieldNames;
  var
    I, FldNameLen: Integer;
    FieldDef: TFieldDef;
  begin
    for I := 0 to FieldDefs.Count - 1 do
    begin
      FieldDef := FieldDefs[I];
//##UNICODE
      FldNameLen := Length(FieldDef.Name);
      if FldNameLen >= SizeOf(MIDASNAME) then
        InternalSetOptionalParam(szFIELDNAME, FieldDef.Name, True, FieldDef.FieldNo);
    end;
  end;
{$endif}


  procedure CreateIndexes;
  var
    I: Integer;
    IndexDesc: DSIdxDesc;
  begin
    for I := 0 to IndexDefs.Count - 1 do
    begin
      EncodeIndexDesc(IndexDesc, IndexDefs[I].Name, IndexDefs[I].Fields, IndexDefs[I].DescFields, IndexDefs[I].CaseInsFields, IndexDefs[I].Options);
      Check(FDSBase.CreateIndex(IndexDesc));
    end;
  end;

var
  FieldDefCount, DescNo: Integer;
  FieldDescs: TFieldDescList;

begin
  // Set the property to indicate the dataset is to be created on loading
  Include(FInternalOptions, ioCreateDataset);
  Include(FInternalOptions, ioCreateFields);

  try
    CheckInactive;
    InitFieldDefsFromFields;
    TDBIFieldDef.SyncFieldDefs(FieldDefs);
    FieldDefCount := 0;
    GetFieldDefCount(FieldDefs, FieldDefCount);
    if FieldDefCount = 0 then
      DatabaseError(SCannotCreateDataSet);
    SetLength(FieldDescs, FieldDefCount);
    DescNo := 0;
    EncodeFieldDescs(FieldDefs, FieldDescs, DescNo);
  finally
    Exclude(FInternalOptions, ioCreateFields);
  end;

  FDSBase := CreateDSBase;
  try
    Check(FDSBase.CreateDS(FieldDefCount, pDSFLDDesc(FieldDescs), TDBINameBuffer(TDBIString(Name))));
{$ifdef _UNUSED}
    CheckLongFieldNames;
    CreateIndexes;
{$endif}

  except
    FDSBase := nil;
    FCloneSource := nil;
    raise;
  end;
  Open;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 25/03/2009 08:51:42 - Initial code.<br>
}
procedure TDBIDataset.EmptyDataSet;
begin
  CheckBrowseMode;
  Check(FDSBase.Reset);
  FDSBase.SetProp(basepropDATAHASCHANGED, TDBIPropValue(True));
  ProviderEOF := True;
  Resync([]);
  InitRecord(ActiveBuffer);
end;


{$ifdef _UNUSED}
procedure TDBIDataset.SetupInternalCalcFields(Add: Boolean);
var
  Precision, I: Integer;
  FieldDesc: DSFLDDesc;
begin
  if Add and not DefaultFields then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      if Fields[I].FieldKind = fkInternalCalc then
      begin
        if Fields[I].DataType in [ftBCD,ftFMTBcd] then
          Precision := TBCDField(Fields[I]).Precision
        else
          Precision := 0;
        EncodeFieldDesc(FieldDesc, Fields[I].FieldName, Fields[I].DataType, Fields[I].Size, Precision, True, []);
        Check(FDSBase.AddField(@FieldDesc));
      end;
    end;
  end
  else if InternalCalcFields then
  begin
    Check(FDSBase.SetFieldCalculation(0, nil));
    Check(FDSBase.AddField(nil));
  end;
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 13:48:24 - Initial code.<br>
}
procedure TDBIDataset.WriteDataPacket(Stream: TStream; WriteSize: Boolean;
  Format: TDBIDataFormat = dfXML);
var
  Size: Integer;

begin
  if Active then begin
    CheckBrowseMode;
  end;

  if IsCursorOpen then begin
    CheckProviderEOF;
    SaveDataPacket(Format);
  end;

  if Assigned(FSavedPacket) then begin
    Size := TDBIXmlData.DataPacketSize(FSavedPacket);

    if WriteSize then begin
      Stream.Write(Size, SizeOf(Size));
    end;
	//## You are copying from "FSavedPacket's" current position - Is that what you want?
    Stream.CopyFrom(FSavedPacket, Size);

    if Active then begin
      ClearSavedPacket;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 26/11/2014 13:24:12 - Initial code.<br>
}
procedure TDBIDataset.ReadDataPacket(Stream: TStream; ReadSize: Boolean);
var
  Size: Integer;

begin
  if ReadSize then begin
    Stream.ReadBuffer(Size, SizeOf(Size));
  end
  else begin
    Size := Stream.Size - Stream.Position;
  end;

  if (Size > 0) then begin
    ClearSavedPacket;
    try
      FSavedPacket := TDBIXmlData.CreateDataPacket(Stream, Size);
    except
      ClearSavedPacket;
      raise;
    end;
  end;
end;



// _____________________________________________________________________________
{**
  Jvr - 27/06/2005 16:05:44 - Initial code.<br>
}
procedure TDBIDataset.LoadFromStream(
  AStream: TStream;
  Format: TDBIDataFormat;
  OpenMode: TDBIOpenMode
  );
begin
  if (Format = dfXML) then begin
    Close;
    DataConnection.FileName := '';

    TDBIXmlDataPacketReader.LoadFromStream(AStream, Self);
  end
  else if (Format = dfJSON) then begin
    TDBIJsonDataPacketReader.LoadFromStream(AStream, Self);
  end
  else if (Format = dfCSV) then begin
    TDBICsvDataPacketReader.LoadFromStream(AStream, Self);
  end
  else begin
    Close;
    DataConnection.LoadFromStream(AStream, Format);

    // Activate the dataset
    case OpenMode of
      omOpen: Open;
      omCreateDataset: CreateDataset;
    end;
  end;
end;  { LoadFromStream }


// _____________________________________________________________________________
{**
  Jvr - 27/06/2005 16:04:11 - Initial code.<br>
}
procedure TDBIDataset.SaveToStream(AStream: TStream; Format: TDBIDataFormat);
begin
  if (Format = dfXML) then begin
    TDBIXmlDataPacketWriter.SaveToStream(AStream, Self);
  end
  else if (Format = dfJSON) then begin
    TDBIJsonDataPacketWriter.SaveToStream(AStream, Self);
  end
  else if (Format = dfCSV) then begin
    TDBICsvDataPacketWriter.SaveToStream(AStream, Self);
  end
  else begin
    DataConnection.SaveToStream(AStream, Format);
  end;
end;  { SaveToStream }


// _____________________________________________________________________________
{**
  Jvr - 27/06/2005 15:54:16 - Initial code.<br>
  Jvr - 27/06/2005 15:56:08 - Made code generic for different file formats.<br>
}
procedure TDBIDataset.LoadFromFile(
  AFileName: string = '';
  Format: TDBIDataFormat = dfDefault;
  OpenMode: TDBIOpenMode = omOpen
  );

begin
  if (AFileName = '') then AFileName := GetFileName;
  if (AFileName = '') then DatabaseError(SInvalidFilename, Self);

  Format := SelectFormat(AFileName, Format);

  if (Format = dfXML) then begin
    Close;
    DataConnection.FileName := '';

//    TDBIXmlData.LoadFromFile(AFileName, Self);
    TDBIXmlDataPacketReader.LoadFromFile(AFileName, Self);
  end
  else if (Format = dfJSON) then begin
    TDBIJsonDataPacketReader.LoadFromFile(AFileName, Self);
  end
  else if (Format = dfCSV) then begin
    TDBICsvDataPacketReader.LoadFromFile(AFileName, Self);
  end
  else begin
    Close;
    DataConnection.LoadFromFile(AFileName, Format);

    // Activate the dataset
    case OpenMode of
      omOpen: Open;
      omCreateDataset: CreateDataset;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 10/05/2005 16:10:42 - Initial code.<br>
  Jvr - 27/06/2005 15:44:58 - Made code generic for different file formats.<br>
}
procedure TDBIDataset.SaveToFile(AFileName: String; Format: TDBIDataFormat);
var
  DBCursor: TDBIDataset;

begin
  // Prevent writing a zero-byte file
  if Active or (Assigned(FDSBase) and (DatasetField = nil)) then begin
    if (AFileName = '') then AFileName := GetFileName;
    if (AFileName = '') then DatabaseError(SInvalidFilename, Self);

    DBCursor := TDBIDatasetClass(ClassType).Create(nil);
    try
      DBCursor.CloneCursor(Self, True);

      Format := SelectFormat(AFileName, Format);
      if (Format = dfXML) then begin
        TDBIXmlDataPacketWriter.SaveToFile(AFileName, DBCursor);
      end
      else if (Format = dfJSON) then begin
        TDBIJsonDataPacketWriter.SaveToFile(AFileName, DBCursor);
      end
      else if (Format = dfCSV) then begin
        TDBICsvDataPacketWriter.SaveToFile(AFileName, DBCursor);
      end
      else begin
        DataConnection.SaveToFile(AFileName, Format);
      end;
    finally
      DBCursor.Free;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Introduced specifically for TDBIDataset
  Jvr - 27/06/2005 15:50:23 - Initial code.<br>
}
function TDBIDataset.SelectFormat(
  const AFileName: String;
  const Format: TDBIDataFormat
  ): TDBIDataFormat;
var
  Extension: String;

begin
  if Format <> dfDefault then begin
    Result := Format;
    Exit;
  end;

  Extension := LowerCase(ExtractFileExt(AFileName));
  if (Extension = '.json') then
    Result := dfJSON
  else if (Extension = '.xml') then
    Result := dfXML
  else if (Extension = '.cds') then
    Result := dfXML
  else if (Extension = '.csv') then
    Result := dfCSV
  else if (Extension = '.txt') then
    Result := dfCSV
  else
    Result := dfXbasePlus;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 11/05/2005 13:06:01 - Initial code.<br>
}
procedure TDBIDataset.SetLogChanges(Value: Boolean);
begin
  CheckBrowseMode;
  Check(FDSBase.SetProp(basepropLOGCHANGES, TDBIPropValue(Value)));
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 11/05/2005 13:06:45 - Initial code.<br>
}
function TDBIDataset.GetLogChanges: Boolean;
var
  LogChanges: TDBIPropValue;
begin
  CheckBrowseMode;
  Check(FDSBase.GetProp(basepropLOGCHANGES, PDBIPropValue(@LogChanges)));
  Result := Boolean(LogChanges);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 17:15:50.<P>
}
function TDBIDataset.GetCanModify: Boolean;
begin
  Result := FCanModify and not ReadOnly;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 26/10/2000 13:54:04<P>
}
procedure TDBIDataset.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
  if Assigned(FDSBase) then
  begin
    Check(FDSBase.SetProp(basepropREADONLY, TDBIPropValue(Value)));
    FCanModify := not Value;
  end;
end;


function TDBIDataset.ConstraintsDisabled: Boolean;
begin
  Result := FConstDisableCount > 0;
end;


procedure TDBIDataset.DisableConstraints;
begin
  if FConstDisableCount = 0 then
    Check(FDSBase.SetProp(basepropCONSTRAINTS_DISABLED, TDBIPropValue(True)));
  Inc(FConstDisableCount);
end;


procedure TDBIDataset.EnableConstraints;
begin
  if FConstDisableCount <> 0 then
  begin
    Dec(FConstDisableCount);
    if FConstDisableCount = 0 then
      Check(FDSBase.SetProp(basepropCONSTRAINTS_DISABLED, TDBIPropValue(False)));
  end;
end;



{ Record Functions }

// _____________________________________________________________________________
{**
  Logical Buffer = Record buffer + CalcFields buffer + RecInfo + Bookmark.

  Jvr - 17/09/2001 17:03:14.<BR>
  Jvr - 18/04/2002 14:01:25 - Added code to allow the underlying cursor access
                              to the TRecInfo record in the logical buffer<P>
}
procedure TDBIDataset.InitBufferPointers(GetProps: Boolean);
var
  CursorProps: DSProps;

begin
  if GetProps then 
  begin
    Check(FDSCursor.GetCursorProps(CursorProps));
    BookmarkSize := CursorProps.iBookmarkSize;
    SetLength(FLastParentBM, BookMarkSize);
    FRecordSize := CursorProps.iRecBufSize;
  end;

  { Compute offsets to various record buffer segments }
  FRecInfoOfs := FRecordSize + CalcFieldsSize;
  FBookmarkOfs := FRecInfoOfs + SizeOf(TDBIRecInfo);

{$ifdef _AGGREGATES}
  FAggGrpIndOfs := FBookmarkOfs + BookMarkSize;
  FAggFieldsOfs := FAggGrpIndOfs + FAggGrpIndSize;
  FRecBufSize := FAggFieldsOfs + FAggFieldsSize;
{$else}
  FRecBufSize := FBookmarkOfs + BookmarkSize;
{$endif}

  // Tell the underlying cursor what the offset is for the RecInfo record.
  // This so we can access the RecInfo Data from with-in the TDSCursor
  Check(FDSCursor.SetProp(cursorpropRECINFO_OFFSET, TDBIPropValue(FRecInfoOfs)));
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 26/10/2000 13:33:13.<P>
}
function TDBIDataset.AllocRecordBuffer: TDBIRecordBuffer;
begin
  Result := AllocMem(FRecBufSize);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 26/10/2000 13:33:24.<P>
}
procedure TDBIDataset.FreeRecordBuffer(var Buffer: TDBIRecordBuffer);
begin
  FreeMem(Buffer);
end;


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 17:13:41.<P>
}
procedure TDBIDataset.InternalInitRecord(Buffer: TDBIRecordBuffer);
begin
  CheckIsInitialised('InternalInitRecord::3375');
  Check(FDSCursor.InitRecord(Buffer));
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:54:44.<P>
}
procedure TDBIDataset.ClearCalcFields(Buffer: TDBIRecordBuffer);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 26/10/2000 13:31:05.<P>
}
procedure TDBIDataset.InitRecord(Buffer: TDBIRecordBuffer);
begin
  inherited InitRecord(Buffer);

  PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.BookMarkFlag := bfInserted;
  PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.RecordNumber := -1;
  PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.RecordIdent := -1;
  PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.Attribute := dsRecNew;
  PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.Data := nil;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 01/03/2001 13:22:36.<P>
}
function TDBIDataset.GetRecord(Buffer: TDBIRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Status: DBIResult;

begin
  case GetMode of
    gmNext:
      begin
        Status := FDSCursor.MoveRelative(1);
{$ifdef _UNUSED}
        if (Status = DBERR_EOF) and not ProviderEOF and FFetchOnDemand then
        begin
          FDSCursor.MoveRelative(-1);
          FetchMoreData(False);
          Status := FDSCursor.MoveRelative(1);
        end;
{$endif}
      end;

    gmPrior: Status := FDSCursor.MoveRelative(-1);
  else
    Status := DBIERR_NONE;
  end;

  if Status = DBIERR_NONE then
    Status := FDSCursor.GetCurrentRecord(Buffer);


  case Status of
    DBIERR_NONE:
      begin
{$ifdef _AGGREGATES}
        if (AggFields.Count > 0) and AggregatesActive then
          GetAggFieldData(Buffer);
{$endif}

        PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.BookmarkFlag := bfCurrent;
        FDSCursor.GetSequenceNumber(LongWord(PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.RecordNumber));
        FDSCursor.GetRecordNumber(LongWord(PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.RecordIdent));
        FDSCursor.GetRecordAttribute(PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.Attribute);

        GetCalcFields(Buffer);
        Check(FDSCursor.GetCurrentBookmark(TDBIRecordBuffer(Buffer) + FBookmarkOfs));
        Result := grOK;
      end;
    DBIERR_BOF: Result := grBOF;
    DBIERR_EOF: Result := grEOF;
  else
    Result := grError;
    if DoCheck then Check(Status);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 05/03/2002 13:30:09.<P>
}
function TDBIDataset.GetCurrentRecord(Buffer: TDBIRecordBuffer): Boolean;
begin
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then 
  begin
    UpdateCursorPos;
    Result := (FDSCursor.GetCurrentRecord(Buffer) = DBIERR_NONE);
  end
  else
    Result := False;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 17:36:08 - Added support for nested datasets.<P>
  Jvr - 19/06/2002 17:19:20 - Result is initialised because the passed in value
                              to GetRecordCount may have some significance.
}
function TDBIDataset.GetRecordCount: Integer;
var
  Status: DBIResult;

begin
  Result := dsRecActive;

  CheckActive;
  if (FParentDataSet <> nil) and (FParentDataSet.State <> dsInsert) then
    FParentDataSet.UpdateCursorPos;

  Status := FDSCursor.GetRecordCount(Result);

  if Status <> DBERR_DETAILSNOTFETCHED then
    Check(Status);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Returns the dataset cursors current sequence number.<P>

  Because of Delphi's internal record buffering, we must read the stored
  record number, not the current physical file position.<P>

  @param  Result  Current sequence number

  Jvr - 17/09/2001 17:22:48 - OKASIS.<P>
}
function TDBIDataset.GetRecNo: Integer;
var
  BufPtr: TDBIRecordBuffer;

begin
  CheckActive;

  if State = dsInternalCalc then
    Result := -1
  else
  begin
    if State = dsCalcFields then
      BufPtr := GetCalcBuffer
    else
      BufPtr := GetActiveBuffer;

    Result := PRecInfo(BufPtr + FRecInfoOfs)^.RecordNumber;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  SetRecNo sets the dataset cursor to the specified sequence number.<P>
  See SetRecId to position the cursor on a physical record number.<P>

  <B>NOTES:</B><BR>
  This method has not Yet been properly implemented.

  Checking Filtered is a hack and will have to be removed from the 'if'
  statement once filters have been properly implemented.<P>

  Jvr - 17/09/2001 17:23:20.<P>
  Jvr - 19/06/2002 17:57:04 - Now that filtering has been implemented
                              properly we can put the method back to
                              as it is in TClientDataset<P>
}
procedure TDBIDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if Value <> RecNo then
  begin
    DoBeforeScroll;
    Check(FDSCursor.MoveToSeqNo(Value));
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Returns the physical size of the record.<P>

  @param  Result  Record Size
}
function TDBIDataset.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;


// _____________________________________________________________________________
{**
  Jvr - 05/03/2002 13:37:46.<P>
}
{$HINTS OFF}
function TDBIDataset.GetActiveRecBuf(out RecBuf: TDBIRecordBuffer): Boolean;

  function GetOriginalBuffer: TDBIRecordBuffer;
  begin
    UpdateCursorPos;
    Result := GetTempBuffer;
    raise EDBINotImplementedException.Create(Self, 'GetOriginalBuffer::3605');
{$ifdef _UNUSED}
    if FDSCursor.GetProp(curpropGETORG_RECBUF, PDBIPropValue(@Result)) <> DBERR_NONE then
      GetCurrentRecord(Result);
{$endif}
  end;

begin
  case State of
    dsBlockRead,
    dsBrowse: if IsEmpty then
                RecBuf := nil
              else
                RecBuf := GetActiveBuffer;

    dsEdit, dsInsert: RecBuf := GetActiveBuffer;
    dsSetKey: RecBuf := TDBIRecordBuffer(FKeyBuffer) + SizeOf(TKeyBuffer);
    dsCalcFields,
    dsInternalCalc: RecBuf := GetCalcBuffer;
    dsFilter: RecBuf := FFilterBuffer;
    dsNewValue: RecBuf := @FNewValueBuffer[0];
    
    dsOldValue: if FOldValueBuffer <> nil then
                  RecBuf := @FOldValueBuffer[0]
                else
                  RecBuf := @GetOriginalBuffer[0];

    dsCurValue: RecBuf := @FCurValueBuffer[0];
    dsInActive: RecBuf := nil;
  else
    RecBuf := nil;
  end;

  Result := RecBuf <> nil;
end;
{$HINTS ON}


// _____________________________________________________________________________
{**
  This is a newly introduced method.
  
  Jvr - 09/12/2004 21:22:36 - Initial code.<p>
}
function TDBIDataset.GetActiveRecInfo(out RecInfo: PRecInfo): Boolean;
var
  RecBuf: TDBIRecordBuffer;

begin
  Result := GetActiveRecBuf(RecBuf);
  if Result then begin
    RecInfo := PRecInfo(TDBIRecordBuffer(RecBuf) + FRecInfoOfs);
  end;
end;


{$ifdef _UNUSED}
function TDBIDataset.GetChangeCount: Int64;
begin
  Result := 0;
  if Active then
    Check(FDSBase.GetProp(dspropNOOFCHANGES, PDBIPropValue(@Result)));
end;

function TDBIDataset.GetCommandStates(
  const ACommand: string): TDataSetCommandStates;
begin
  if SameText(ACommand, sApplyUpdatesDataSetCommand) or
     SameText(ACommand, sCancelUpdatesDataSetCommand) then
  begin
    Result := [dcSupported];
    if ChangeCount > 0 then
      Include(Result, dcEnabled);
  end
  else
    Result := [];
end;
{$endif}

// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 01/03/2001 13:23:43.<P>
}
function TDBIDataset.UpdateStatus: TUpdateStatus;
var
  BufPtr: TDBIRecordBuffer;
  Attr: Byte;

begin
  CheckActive;
  if State = dsInternalCalc then
    Result := usUnModified
  else
  begin
    if State = dsCalcFields then
      BufPtr := GetCalcBuffer
    else
      BufPtr := GetActiveBuffer;

    Attr := PRecInfo(TDBIRecordBuffer(BufPtr) + FRecInfoOfs)^.Attribute;
    if (Attr and dsRecModified) <> 0 then
      Result := usModified
    else if (Attr and dsRecDeleted) <> 0 then
      Result := usDeleted
    else if (Attr and dsRecNew) <> 0 then
      Result := usInserted
    else
      Result := usUnModified;
  end;
end;



{ Field Related }

// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 01/12/2000 15:08:40.<BR>
  Jvr - 05/02/2001 14:08:33 - Xbase species a precision value,
                              we now set that as the default in the fielddefs<BR>
}
type
  PFieldAttributes = ^TFieldAttributes;

procedure TDBIDataset.AddFieldDesc(FieldDescs: TFieldDescList;
  var DescNo: Integer; var FieldID: Integer; FieldDefs: TFieldDefs);
var
  LPrecision, I: Integer;
  LType: TFieldType;
  LSize: Integer;
  LName: string;
  FieldDesc: DSFLDDesc;
  LFieldDef: TFieldDef;

begin
  FieldDesc := FieldDescs[DescNo];
  Inc(DescNo);
  if ((fldAttrLINK and FieldDesc.iFldAttr) <> 0) then
  begin
    Inc(FieldID);
    Exit;
  end;
    LName := String(FieldDesc.szName);
  I := 0;
  while FieldDefs.IndexOf(LName) >= 0 do
  begin
    Inc(I);
    LName := Format('%s_%d', [FieldDesc.szName, I]);
  end;

  if FieldDesc.iFldType < MAXLOGFLDTYPES then
    LType := DataTypeMap[FieldDesc.iFldType]
  else if FieldDesc.iFldType = fldUNICODE then
    LType := ftWideString
{$ifdef DELPHI2009}
  else if FieldDesc.iFldType = fldDATETIMEOFFSET then
    LType := ftTimeStampOffset
{$endif}
  else
    LType := ftUnknown;

  LSize := 0;
  LPrecision := 0;
  case FieldDesc.iFldType of
{$ifdef fpc}
    fldADT:
      LType := ftUnknown;
{$else}
    fldADT,
{$endif}
    fldZSTRING, fldBYTES, fldVARBYTES, fldArray:
    begin
      LSize := FieldDesc.iUnits1;
      if FieldDesc.iFldSubType = fldstGuid then
        LType := ftGuid;
    end;
    fldUNICODE:
      LSize := FieldDesc.iUnits1 div 2;
    fldINT16, fldUINT16:
      if FieldDesc.iFldLen <> 2 then LType := ftUnknown;
    fldINT32:
      if FieldDesc.iFldSubType = fldstAUTOINC then LType := ftAutoInc;
    fldFLOAT, fldFLOATIEEE:
      if FieldDesc.iFldSubType = fldstMONEY then LType := ftCurrency;

    {$ifdef DELPHI6} fldFMTBCD, {$endif} fldBCD:
      begin
        LSize := Abs(FieldDesc.iUnits2);
        LPrecision := FieldDesc.iUnits1;
{$ifdef DELPHI6}
        if FieldDesc.iFldType = fldFMTBCD then
          LType := ftFMTBcd;
{$endif}
      end;
    fldBLOB:
      begin
        LSize := FieldDesc.iUnits1;
        if ( (FieldDesc.iFldSubType >= fldstMEMO) and (FieldDesc.iFldSubType <= fldstTYPEDBINARY))
            or (FieldDesc.iFldSubType = fldstWIDEMEMO)
            or (FieldDesc.iFldSubType = fldstHMEMO)
            or (FieldDesc.iFldSubType = fldstHBINARY) then
          LType := BlobTypeMap[FieldDesc.iFldSubType];
      end;
{$ifdef fpc}
    fldTABLE:
      LType := ftUnknown;
{$else}
    fldTABLE:
      begin
        { TODO 1 -oJvr -cTDBIDataSet.AddFieldDesc() :
          01/03/2002 15:16:10
          Changed my mind - this probably won't help anyway
        }

{
        //##Jvr - 01/03/2002 14:50:06
        // This differs from TClientdataset but I believe is required
        // Why do this? - Because it then matches the fldADT type!
        //                and I'm relying on the iunits1 to determine the
        //                subobject fieldcount.
        FSize := iUnits1;
}
        //##Jvr - 15/10/2001 12:07:57
      if FieldDesc.iFldSubType = fldstREFERENCE then LType := ftReference;
    end;
{$endif}
  end;

  if LType <> ftUnknown then
  begin
    LFieldDef := FieldDefs.AddFieldDef;
{$ifndef fpc}
    LFieldDef.FieldNo := FieldID;
{$endif}
    Inc(FieldID);
    LFieldDef.Name := LName;
    LFieldDef.DataType := LType;
    LFieldDef.Size := LSize;
    LFieldDef.Precision := LPrecision;
    LFieldDef.Attributes := PFieldAttributes(@(FieldDesc.iFldAttr))^;
{$ifdef fpc}
    LFieldDef.Required := faRequired in LFieldDef.Attributes;
{$endif}
    if FieldDesc.iFldSubType = fldstFIXED then
      LFieldDef.Attributes := LFieldDef.Attributes + [faFixed];
    LFieldDef.InternalCalcField := FieldDesc.bCalculated;
    case LType of
      ftADT:
        for I := 0 to FieldDesc.iUnits1 - 1 do
          AddFieldDesc(FieldDescs, DescNo, FieldID, TDBIFieldDef.ChildDefs(LFieldDef));
      ftArray:
        begin
          I := FieldID;
          AddFieldDesc(FieldDescs, DescNo, I, TDBIFieldDef.ChildDefs(LFieldDef));
          Inc(FieldID, FieldDesc.iUnits2);
        end;
    end; { case }
  end;
end;


// _____________________________________________________________________________
{**
  Get the data for the given field from the active buffer and stick it
  in the given buffer.  Return False if the field value is null; otherwise
  return True. Buffer may be nil if TDataSet is checking for null only.<P>

  @param  Field  The TField Object we are dealing with
  @param  Buffer  A pointer to the record buffer
  @param  Result  False if Null, True otherwise

  <P>
  Jvr - 22/03/2001 00:44:10 - Added Code to deal with Calculated fields<P>
}
function TDBIDataset.GetFieldData(Field: TField; {$ifdef DelphiXE4} var {$endif} Buffer: TDBIValueBuffer): Boolean;
var
  IsBlank: LongBool;
  RecBuf: TDBIRecordBuffer;
begin
  Result := False;
  Assert(Assigned(FDSCursor));

  if GetActiveRecBuf(RecBuf) then
    if Field.FieldKind in [fkData, fkInternalCalc] then
    begin
      Check(FDSCursor.GetField(RecBuf, Field.FieldNo, Buffer, IsBlank));
      Result := not IsBlank;
    end
    else if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
    begin
      Inc(RecBuf, FRecordSize + Field.Offset);
      Result := Boolean(RecBuf[0]);
      if Result and (Buffer <> nil) then
{$ifdef DelphiXE3}
        Move(RecBuf[1], Buffer[0], Field.DataSize);
{$else}
        Move(RecBuf[1], Buffer^, Field.DataSize);
{$endif}
    end;
end;

// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 09/05/2005 20:40:03 - Initial code.<br>
}
function TDBIDataset.GetFieldData(FieldNo: Integer; {$ifdef DelphiXE4} var {$endif} Buffer: TDBIValueBuffer): Boolean;
var
  RecBuf: TDBIRecordBuffer;
  IsBlank: LongBool;
begin
  Result := GetActiveRecBuf(RecBuf);
  if Result then
  begin
    Check(FDSCursor.GetField(RecBuf, FieldNo, Buffer, IsBlank));
    Result := not IsBlank;
  end;
end;


{$ifdef DelphiXE3}
function TDBIDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  IsBlank: LongBool;
  RecBuf: PByte;
begin
  Result := False;
  if GetActiveRecBuf(RecBuf) then
    with Field do
      if FieldKind in [fkData, fkInternalCalc] then
      begin
        Check(FDSCursor.GetField(RecBuf, FieldNo, Buffer, IsBlank));
        Result := not IsBlank;
      end else
        if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
        begin
          Inc(RecBuf, FRecordSize + Offset);
          Result := Boolean(RecBuf[0]);
          if Result and (Buffer <> nil) then
            Move(RecBuf[1], Buffer^, DataSize);
        end;
end;

function TDBIDataset.GetFieldData(FieldNo: Integer; Buffer: Pointer): Boolean;
var
  RecBuf: PByte;
  IsBlank: LongBool;
begin
  Result := GetActiveRecBuf(RecBuf);
  if Result then
  begin
    Check(FDSCursor.GetField(RecBuf, FieldNo, Buffer, IsBlank));
    Result := not IsBlank;
  end;
end;
{$endif DelphiXE3}


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 09/05/2005 20:40:43 - Initial code.<br>
}
function TDBIDataset.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;

  function CheckNotChanged(Buffer: TDBIRecordBuffer): Variant;
  var
    IsBlank: Integer;
  begin
    if (Buffer = nil) then
      IsBlank := BLANK_NOTCHANGED
    else
      Check(FDSCursor.GetField(Buffer, Field.FieldNo, nil, LongBool(IsBlank)));
    if IsBlank = BLANK_NOTCHANGED then
      Result := UnAssigned
    else if IsBlank =  BLANK_NULL then
      Result := Null
    else
      Result := inherited GetStateFieldValue(State, Field);
  end;

begin
  case State of
    dsNewValue:
      if FNewValueBuffer = nil then
      begin
        FNewValueBuffer := TDBIRecordBuffer(ActiveBuffer);
        try
          Result := CheckNotChanged(FNewValueBuffer);
        finally
          FNewValueBuffer := nil;
        end;
      end
      else
        Result := CheckNotChanged(FNewValueBuffer);
    dsCurValue: Result := CheckNotChanged(FCurValueBuffer);
  else
    Result := inherited GetStateFieldValue(State, Field);
  end;
end;


// _____________________________________________________________________________
{**
  SetFieldData differs from TClientDataset. It has additional code to
  synchronise the DataConnection with the RecordBuffer.<P>

  Jvr - 17/09/2001 16:54:08 - Original code<BR>
  Jvr - 20/09/2002 11:17:47 - Updated to include field validation<BR>
  Jvr - 24/09/2002 15:08:25 - Added Recordbuffer Synchronisation<BR>
  Jvr - 08/10/2002 17:11:06 - Added code for master/detail datasets<P>
  Jvr - 03/06/2005 14:43:49 - Added write-through.<br>
}
procedure TDBIDataset.SetFieldData(Field: TField; Buffer: TDBIValueBuffer);
var
  RecBuf: TDBIRecordBuffer;
begin
//##JVR  FIndexFieldCount := 0;
  CheckIsInitialised('SetFieldData::4025');

  if not (State in dsWriteModes) then DatabaseError(SNotEditing, Self);
  if (State = dsSetKey) and ((Field.FieldNo < 0) or (FIndexFieldCount > 0) and
    not Field.IsIndexField) then DatabaseErrorFmt(SNotIndexField, [Field.DisplayName]);
  GetActiveRecBuf(RecBuf);

  if Field.FieldKind in [fkData, fkInternalCalc] then
  begin
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);

    // Update the Dataconnection Edit source (Validation object?) from pRecBuf
    FDataConnection.SyncRecordBuffer(RecBuf^, False);

    Field.Validate(Buffer);
    if State in [dsEdit, dsInsert] then
      Check(FDSCursor.VerifyField(Field.FieldNo, Buffer));
    Check(FDSCursor.PutField(RecBuf, Field.FieldNo, Buffer));

{$ifdef _AGGREGATES}
    if FAggFieldsUpdated <> nil then
      FAggFieldsUpdated[Field.FieldNo-1] := True;
{$endif}
  end
  else
  { TODO 1 -oJvr -cTDBIDataSet.SetFieldData() :
    11/04/2016 15:03:51
    Important: Testcase required for this code.
  }
  begin
    if State = dsInternalCalc then Exit;
    Inc(RecBuf, FRecordSize + Field.Offset);
{$ifndef DelphiXE3}
    Boolean(RecBuf[0]) := PLongBool(Buffer)^;
    if Boolean(RecBuf[0]) then Move(Buffer^, RecBuf[1], Field.DataSize);
{$else}
    Boolean(RecBuf[0]) := LongBool(Buffer[0]);
    if Boolean(RecBuf[0]) then Move(Buffer[0], RecBuf[1], Field.DataSize);
{$endif}
  end;
  if not (State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, TDBIPropValue(Field));

  //##JVR Experimenting!
  if (State in dsEditModes) and (doWriteThrough in DatasetOptions) then begin
    FDSCursor.SyncRecord(RecBuf);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 05/03/2002 13:41:13.<P>
}
function TDBIDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
{$ifdef _UNUSED}
var
  Status: DBIResult;
  BlobLen: LongWord;
  Buffer: TDBIRecordBuffer;
begin
  if GetActiveRecBuf(Buffer) then
  begin
    Status := FDSCursor.GetBlobLen(Buffer, Field.FieldNo, BlobLen);
    if (Status = DBIERR_BLOBNOTFETCHED) and FetchOnDemand then
      FetchBlobs;
  end;
{$endif}
begin
  Result := TDBIBlobStream.Create(Field as TBlobField, Mode);
end;

{$ifdef _UNUSED}
procedure TDBIDataset.RefreshInternalCalcFields(Buffer: TDBIRecordBuffer);
begin
  CalculateFields(TDBIRecordBuffer(Buffer));
end;

function TDBIDataset.CalcFieldsCallBack(RecBuf: TDBIRecordBuffer): DBIResult;
var
  SaveState: TDataSetState;
begin
  try
    SaveState := SetTempState(dsInternalCalc);
    try
      CalculateFields(RecBuf);
    finally
      RestoreState(SaveState);
    end;
  except
  end;
  Result := 0;
end;
{$endif}

procedure TDBIDataset.DataConvert(
  Field: TField; Source: TDBIValueBuffer; {$ifdef DelphiXE4} var {$endif} Dest: TDBIValueBuffer; ToNative: Boolean);
var
  ByteLen, StrLen: Integer;

  function WStrLen(const Str: PWideChar): Cardinal;
  var
    P : PWideChar;
  begin
    P := Str;
    while (P^ <> #0) do Inc(P);
    Result := (P - Str);
  end;

begin
  if Field.DataType = ftWideString then
  begin
    if ToNative then
    begin
      // Convert from null terminated to length prefixed
      StrLen := Length(PWideChar({$ifdef DELPHI2006} Source {$else} Source^ {$endif}));
      if StrLen > Field.Size then
        StrLen := Field.Size;
      ByteLen := StrLen * 2;

      PWord(Dest)^ := ByteLen;
      Move(PDBIByte({$ifdef DELPHI2006} Source {$else} Source^ {$endif})^, (PDBIByte(Dest) + SizeOf(Word))^, ByteLen);
    end else
    begin
      // Convert from length prefixed to null terminated
      ByteLen := PWord(Source)^;
      StrLen := ByteLen div 2;
{$ifdef DELPHI2006}
      Move((PDBIByte(Source) + SizeOf(Word))^, PDBIByte(Dest)^, ByteLen);
      (PWideChar(Dest) + StrLen)^ := #$00;
{$else}
      SetString(WideString(Dest^), PWideChar(PDBIByte(Source) + SizeOf(Word)), StrLen);
{$endif}
    end;
  end else
    inherited;
end;


{$ifdef DelphiXE3}
procedure TDBIDataset.DataConvert(
  Field: TField; Source: Pointer; Dest: Pointer; ToNative: Boolean);
var
  ByteLen, StrLen: Integer;
begin
  if Field.DataType = ftWideString then
  begin
    if ToNative then
    begin
      // Convert from null terminated to length prefixed
      StrLen := Length(PChar(Source));
      if StrLen > Field.Size then
        StrLen := Field.Size;
      Word(Dest^) := StrLen * 2; // length prefix is byte count
      Move(PChar(Source)^, (PChar(Dest)+1)^, Word(Dest^));
    end else
    begin
      // Convert from length prefixed to null terminated
      ByteLen := Word(Source^);
      Move((PChar(Source)+1)^, PChar(Dest)^, ByteLen);
      (PChar(Dest) + (ByteLen div 2))^ := #$00;
    end;
  end else
{$WARN SYMBOL_DEPRECATED OFF}
    inherited;
{$WARN SYMBOL_DEPRECATED DEFAULT}
end;
{$endif DelphiXE3}






{ Navigation / Editing }

// _____________________________________________________________________________
{**
  Jvr - 05/03/2002 13:43:18.<P>
}
procedure TDBIDataset.InternalFirst;
begin
  CheckIsInitialised('InternalFirst::4205');
  Check(FDSCursor.MoveToBOF);
end;


// _____________________________________________________________________________
{**
  Jvr - 12/01/1999 17:36:22.<P>
}
procedure TDBIDataset.InternalLast;
begin
  CheckIsInitialised('InternalLast::4215');
  Check(FDSCursor.MoveToEOF);
end;


// _____________________________________________________________________________
{**
  Jvr - 05/03/2002 13:44:03.<P>
}
procedure TDBIDataset.InternalPost;
begin
  CheckIsInitialised('InternalPost::4225');

  if State = dsEdit then
    Check(FDSCursor.ModifyRecord(GetActiveBuffer))
  else
    Check(FDSCursor.InsertRecord(GetActiveBuffer));

    { TODO 5 -oJvr -cTDBIDataSet.InternalPost() :
      InsertRecord still needs some work, it appends only (inserts only at eof)
    }

{$ifdef _AGGREGATES}
  if AggregatesActive then
    DoAggUpdates(State = dsEdit);
{$endif}
end;


// _____________________________________________________________________________
{**
  InternalCancel differs from TClientDataset in that it needs to Reset the
  Attribute to dsRecLockReset in the RecInfo Record.  Also InternalCancel calls
  RevertRecord on the Cursor to notify the engine of the cancel event.

  The dsRecLockSet Attribute only needs Resetting if an Edit was cancelled. If
  an Insert gets cancelled then no lock was placed.<P>

  The dsRecLockReset Attribute in the RecInfo record will be picked up in
  the TDBICursor.GetCurrentRecord method via the following method path:<P>

  <PRE>
  - TDataset.Cancel
    |
    +- TDBIDataset.InternalCancel  <-- we are here
     - TDataSet.FreeFieldBuffers;
     - TDataSet.SetState(dsBrowse);
     - TDataSet.Resync
       |
       +- TDataset.GetRecord
          |
          +- TDBICursor.GetCurrentRecord
  </PRE>

  Jvr - 05/03/2002 13:44:49.<BR>
  Jvr - 06/06/2002 14:20:02 - Added code to unlock a record being cancelled<P>
  Jvr - 20/09/2002 13:36:19 - Added RevertRecord call
                              to notify the engine of the cancel operation<P>
}
procedure TDBIDataset.InternalCancel;
var
  PRecordInfo: PRecInfo;

begin
  if (State = dsEdit) then begin
    PRecordInfo := PRecInfo(TDBIRecordBuffer(TempBuffer) + FRecInfoOfs);

    // Just a check to make sure that it's ok to reuse dsRecOrg
    if (PRecordInfo^.Attribute and dsRecLockReset) = dsRecOrg then begin
      raise EDBIException.Create(Self, 'InternalCancel::4290',
        'Attribute "dsRecOrg" is overloaded in it''s use, Original record was changed', []
        );
    end;

    // Set RecordLock Reset flag
    PRecordInfo^.Attribute := PRecordInfo^.Attribute or dsRecLockReset;
  end;

  Check(FDSCursor.RevertRecord);

{$ifdef _UNUSED}
  if BlobFieldCount > 0 then
    FDSBase.ReleaseBlobs(0);
{$endif}
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 14:04:51 - Added TDatasetOptions support<P>
}
procedure TDBIDataset.InternalDelete;
begin
  if doDisableDeletes in FDSOptions then
    DatabaseError(SNoDeletesAllowed, Self);

  Check(DSCursor.DeleteRecord);
{$ifdef _AGGREGATES}
  if AggregatesActive then
    DoAggUpdates(False);
{$endif}
end;


// _____________________________________________________________________________
{**
  Refresh the current record.
  Jvr - 17/09/2001 14:07:28 - Added TDatasetOptions support<P>
}
procedure TDBIDataset.InternalEdit;
var
  PRecordInfo: PRecInfo;

begin
  if doDisableEdits in FDSOptions then
    DatabaseError(SNoEditsAllowed, Self);

  CheckIsInitialised('InternalEdit::4330');

  PRecordInfo := PRecInfo(TDBIRecordBuffer(ActiveBuffer) + FRecInfoOfs);
  Check(FDSCursor.MovetoSeqNo(PRecordInfo^.RecordNumber));

  // Set RecordLock flag
  PRecordInfo^.Attribute := PRecordInfo^.Attribute or dsRecLockSet;

  // Get the record
  Check(FDSCursor.GetCurrentRecord(GetActiveBuffer));

  // Clear RecordLock flag - do this otherwise buffered records cause problems
//##JVR  RecInfo.Attribute := RecInfo.Attribute and dsRecLockClear;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 12:34:42.<P>
}
procedure TDBIDataset.DoBeforeInsert;
begin
  if doDisableInserts in FDSOptions then
    DatabaseError(SNoInsertsAllowed, Self);

  inherited DoBeforeInsert;

  if (DataSetField <> nil) then
  begin
    { Force inserted master to post before allowing insert on nested dataset }
    if DataSetField.DataSet.State = dsInsert then
      DataSetField.DataSet.Post;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/03/2000 14:29:03.<P>
}
procedure TDBIDataset.InternalInsert;
begin
  { TODO 5 -oJvr -cTDBIDataSet.InternalInsert() :
    I'm not sure what the purpose of 'curpropSETCRACK' is, but it is
    NOT required for the way I'm currently performing an insert.
  }
{$ifdef _UNUSED}
  DSCursor.SetProp(curpropSETCRACK, TDBIPropValue(0));
{$endif}
  CursorPosChanged;
end;


{$ifdef _UNUSED}
procedure TDBIDataset.Post;
var
  CursorProps: DSProps;
begin
  Check(FDSBase.GetProps(CursorProps));
  if CursorProps.bDelta and (State in [dsEdit, dsInsert]) then
  begin
    UpdateRecord;
    InternalPost;
    SetState(dsBrowse);
  end
  else
  begin
    inherited Post;
    if State = dsSetKey then
      PostKeyBuffer(True);
  end;
end;

procedure TDBIDataset.Cancel;
begin
  inherited Cancel;
  if State = dsSetKey then
    PostKeyBuffer(False);
end;
{$endif}

// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 21/03/2000 14:37:17.<P>
}
procedure TDBIDataset.InternalAddRecord(Buffer: TDBIRecordData; Append: Boolean);
begin
  if Append then FDSCursor.MoveToEOF;
  Check(FDSCursor.InsertRecord(Buffer));
end;



// _____________________________________________________________________________
{**
  OKASIS.
  Position physical file to bookmarked record.<P>

  Jvr - 17/09/2001 16:53:36.<P>
}
procedure TDBIDataset.InternalGotoBookmark(Bookmark: TDBIBookmark);
begin
  CheckIsInitialised('InternalGotoBookmark::4435');
  Check(FDSCursor.MoveToBookmark(Bookmark));
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:53:19.<P>
}
procedure TDBIDataset.InternalSetToRecord(Buffer: TDBIRecordBuffer);
begin
//##JVR  InternalGotoBookmark(TBookmark(Buffer + FBookmarkOfs));

  CheckIsInitialised('InternalSetToRecord::4450');
  Check(FDSCursor.MoveToBookmark(@Buffer[FBookmarkOfs]));
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:52:22.<P>
}
function TDBIDataset.GetBookmarkFlag(Buffer: TDBIRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.BookmarkFlag;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:52:11.<P>
}
procedure TDBIDataset.SetBookmarkFlag(Buffer: TDBIRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(TDBIRecordBuffer(Buffer) + FRecInfoOfs)^.BookmarkFlag := Value;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:53:02.<P>
}
procedure TDBIDataset.GetBookmarkData(Buffer: TDBIRecordBuffer; Data: TDBIBookmark);
begin
{$ifdef DelphiXE3}
  Move(Buffer[FBookmarkOfs], Data[0], BookmarkSize);
{$else}
  Move(Buffer[FBookmarkOfs], Data^, BookmarkSize);
{$endif}
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:52:43.<P>
}
procedure TDBIDataset.SetBookmarkData(Buffer: TDBIRecordBuffer; Data: TDBIBookmark);
begin
{$ifdef DelphiXE3}
  Move(Data[0], GetActiveBuffer[FBookmarkOfs], BookmarkSize);
{$else}
  Move(Data^, ActiveBuffer[FBookmarkOfs], BookmarkSize);
{$endif}
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:51:53.<P>
}
function TDBIDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, -1),(1, 0));

begin
  CheckIsInitialised('CompareBookmarks::4516');

  { Check for uninitialized bookmarks }
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    if Assigned(FDSBase) then begin
      Check(FDSCursor.CompareBookmarks(Bookmark1, Bookmark2, Result));
    end;

    if Result = 2 then Result := 0;
  end;
end;


// _____________________________________________________________________________
{**
  The underlying DBIBase Interface still needs to check for deleted records.

  Jvr - 17/09/2001 16:50:49.<P>
}
function TDBIDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := FDSBase <> nil;
  try
    if Result then
    begin
      CursorPosChanged;
      Result := (FDSCursor.MoveToBookmark(Bookmark) = DBIERR_NONE) and
        (FDSCursor.GetCurrentRecord(nil) = DBIERR_NONE);
    end;
  except
    on E: Exception do
      raise EDBIException.Create(Self, E, 'BookmarkValid::4555', 'Failed to validate bookmark', []);
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 12/01/1999 11:53:25.<P>
}
procedure TDBIDataset.SyncCursors(Cursor1, Cursor2: IDBICursor);
var
  BM: DSBOOKMRK;
begin
  Cursor2.GetCurrentBookmark(@BM);
  Cursor1.MoveToBookmark(@BM);
end;


{$ifdef _UNUSED}
function TDBIDataset.UndoLastChange(FollowChange: Boolean): Boolean;
begin
  Cancel;
  CheckBrowseMode;
  UpdateCursorPos;
  Result := (FDSCursor.UndoLastChange(FollowChange) = DBERR_NONE);
  if Result then
  begin
    if FollowChange then CursorPosChanged;
    Resync([]);
  end;
end;

procedure TDBIDataset.RevertRecord;
begin
  Cancel;
  CheckBrowseMode;
  UpdateCursorPos;
  Check(FDSCursor.RevertRecord);
  Resync([]);
end;

function TDBIDataset.GetSavePoint: Integer;
begin
  Result := 0;
  CheckBrowseMode;
  FDSBase.GetProp(dspropGETSAVEPOINT, PDBIPropValue(@Result));
end;

procedure TDBIDataset.SetSavePoint(Value: Integer);
begin
  Cancel;
  CheckBrowseMode;
  UpdateCursorPos;
  Check(FDSBase.RollBack(Value));
  CursorPosChanged;
  Resync([]);
end;

procedure TDBIDataset.CancelUpdates;
begin
  SetSavePoint(0);
end;
{$endif}


{ Indexes }

procedure TDBIDataset.UpdateIndexDefs;
var
  I: Integer;
  CursorProps: DSProps;
  IndexDescs: array of DSIDXDesc;
  Opts: TIndexOptions;
  IdxName, Flds: string;
  DescFlds, CaseInsFlds: string;
  LIndexDef: TIndexDef;

begin
  if (csDesigning in ComponentState) and (IndexDefs.Count > 0) then Exit;

  if Active and not IndexDefs.Updated then
  begin
    FieldDefs.Update;
    Check(FDSCursor.GetCursorProps(CursorProps));
    SetLength(IndexDescs, CursorProps.iIndexes);
    IndexDefs.Clear;
    Check(FDSBase.GetIndexDescs(PDSIDXDesc(IndexDescs)));

    for I := 0 to CursorProps.iIndexes - 1 do
    begin
      DecodeIndexDesc(IndexDescs[I], IdxName, Flds, DescFlds, CaseInsFlds, Opts);
      LIndexDef := IndexDefs.AddIndexDef;
      LIndexDef.Name := IdxName;
      LIndexDef.Fields := Flds;
      LIndexDef.DescFields := DescFlds;
      LIndexDef.CaseInsFields := CaseInsFlds;
      LIndexDef.Options := Opts;
    end;
    IndexDefs.Updated := True;
  end;
end;


procedure TDBIDataset.DecodeIndexDesc(const IndexDesc: DSIDXDesc;
  out Name, Fields, DescFields, CaseInsFields: string; out Options: TIndexOptions);

  procedure ConcatField(var FieldList: string; const FieldName: string);
  begin
    if FieldList = '' then
      FieldList := FieldName
    else
      FieldList := Format('%s;%s', [FieldList, FieldName]);
  end;

  procedure CheckOption(const FieldList: string; var OptionFields: string;
    Option: TIndexOption);
  begin
    if (FieldList <> '') and (OptionFields = FieldList) then
    begin
      Include(Options, Option);
      OptionFields := '';
    end;
  end;

{$ifdef _UNUSED}
  function FindNameFromId(const FieldId: Integer; var FieldName: string): Boolean;
  begin
    { When using nested datasets, the linking field is left out of the FieldDefList which
      creates a "hole".  The code below looks first in the normal spot, and in the location
      adjusted for the hole. }
    if (FieldId <= FieldDefList.Count) and (FieldDefList[FieldId - 1].FieldNo = FieldId) then
      FieldName := FieldDefList.Strings[FieldId - 1]
    else if InternalCalcFields and Assigned(FParentDataSet) and
      (FieldId-2 < FieldDefList.Count) and (FieldDefList[FieldId - 2].FieldNo = FieldId) then
      FieldName := FieldDefList.Strings[FieldId - 2]
    else
      FieldName := '';
    Result := FieldName <> '';
  end;
{$endif}

var
  I: Integer;
  FieldName: string;
begin
{$ifndef fpc}
  FieldDefList.Update;
{$endif}  
  Name := String(IndexDesc.szName);
  Fields := '';
  DescFields := '';
  CaseInsFields := '';
  for I := 0 to IndexDesc.iFields - 1 do
  begin
{$ifdef fpc}
    if IndexDesc.iKeyFields[I] <= Self.Fields.Count then
    begin
      FieldName := Self.Fields[IndexDesc.iKeyFields[I] - 1].FieldName;
      ConcatField(Fields, FieldName);
    end
{$else}
    if IndexDesc.iKeyFields[I] <= FieldDefList.Count then
    begin
      FieldName := FieldDefList.Strings[IndexDesc.iKeyFields[I] - 1];
      ConcatField(Fields, FieldName);
    end
{$endif}
    else
      FieldName := '';

    if IndexDesc.bDescending[I] then
      ConcatField(DescFields, FieldName);
    if IndexDesc.bCaseInsensitive[I] then
      ConcatField(CaseInsFields, FieldName);
  end;
  Options := [];
  if IndexDesc.bUnique then Include(Options, ixUnique);
  CheckOption(Fields, DescFields, ixDescending);
  CheckOption(Fields, CaseInsFields, ixCaseInsensitive);

end;

procedure TDBIDataset.GetIndexNames(List: TStrings);
begin
  IndexDefs.Update;
  IndexDefs.GetItemNames(List);
end;

function TDBIDataset.GetIndexDefs: TIndexDefs;
begin
  if FIndexDefs = nil then
    // Don't create indexes when serializing
    if not (csWriting in ComponentState) then
      FIndexDefs := TIndexDefs.Create(Self);
  Result := FIndexDefs;
end;

procedure TDBIDataset.SetIndexDefs(Value: TIndexDefs);
begin
  IndexDefs.Assign(Value);
end;

procedure TDBIDataset.GetIndexInfo(IndexName: string);
var
  Index: Integer;
  IndexDesc: DSIDXDesc;
begin
  if FDSCursor.GetIndexDescs(True, IndexDesc) = 0 then
  begin
    FIndexFieldCount := IndexDesc.iFields;
    FIndexFieldMap := IndexDesc.iKeyFields;
    FKeySize := IndexDesc.iKeyLen;
  end;
  Index := IndexDefs.IndexOf(IndexName);
  if Index <> -1 then
    FIndexGroupingLevel := {$ifndef fpc} IndexDefs[Index].GroupingLevel {$else} 0 {$endif}
  else
    FIndexGroupingLevel := 0;
end;

procedure TDBIDataset.SwitchToIndex(const IndexName: string);
var
  Status: DBIResult;
  IndexDesc: DSIDXDesc;
  LIndexDef: TIndexDef;
begin
  ResetCursorRange;
  Status := FDSCursor.UseIndexOrder(TDBIIndexName(IndexName));
  if Status <> DBIERR_NONE then
  begin
    if Status = DBIERR_NOSUCHINDEX then
    begin
      LIndexDef := IndexDefs.Find(IndexName);
      EncodeIndexDesc(IndexDesc, LIndexDef.Name, LIndexDef.Fields, LIndexDef.DescFields, LIndexDef.CaseInsFields, LIndexDef.Options);
      Check(FDSBase.CreateIndex(IndexDesc));
      Check(FDSCursor.UseIndexOrder(TDBIIndexName(IndexName)));
    end
    else
      Check(Status);
  end;
  GetIndexInfo(IndexName);
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 19:54:13 - Initial code.<br />
}
procedure TDBIDataset.SetIndex(const Value: string; FieldsIndex: Boolean);
{$ifdef _PARENTDATASET}
var
  SeqNo: LongWord;
{$endif}
begin
  if Active then
  begin
    CheckBrowseMode;
    UpdateCursorPos;
    CheckProviderEOF;
    if (FIndexName <> Value) or (FFieldsIndex <> FieldsIndex) then
    begin
      if FieldsIndex then
        SortOnFields(FDSCursor, Value, False, False)
      else
        SwitchToIndex(Value);
      FIndexName := Value;
      FFieldsIndex := FieldsIndex;
{$ifdef _PARENTDATASET}
      if FParentDataSet <> nil then
      begin
        FParentDataSet.DSCursor.GetSequenceNumber(SeqNo);
        FParentDataSet.DSCursor.MoveToBOF;
        FParentDataSet.DSCursor.MoveToSeqNo(SeqNo);
      end;
{$endif}
{$ifdef _AGGREGATES}
      if FAggregatesActive then
      begin
        FAggFieldsInit := False;
        ResetAllAggs(FAggregatesActive);
        SetBufListSize(0);
        InitBufferPointers(True);
        try
          SetBufListSize(BufferCount + 1);
        except
          SetState(dsInactive);
          CloseCursor;
          raise;
        end;
      end;
{$endif}
      ResetCursorRange;
      CheckMasterRange;
      Resync([]);
    end;
  end;
  FIndexName := Value;
  FFieldsIndex := FieldsIndex;
end;

{$ifdef DELPHI6}
  {$WARN SYMBOL_DEPRECATED OFF}
{$endif}
procedure TDBIDataset.EncodeIndexDesc(out IndexDesc: DSIDXDesc;
  const Name, Fields, DescFields, CaseInsFields: string; Options: TIndexOptions);

  function IndexFieldOfs(const FieldName: string): Integer;
  var
    FieldNo: Integer;
  begin
    FieldNo := FieldDefs.Find(FieldName).FieldNo;
    for Result := 0 to IndexDesc.iFields - 1 do
      if IndexDesc.iKeyfields[Result] = FieldNo then Exit;
    DatabaseErrorFmt(SIndexFieldMissing, [FieldName], Self);
    Result := -1;
  end;

var
  Pos: Integer;
  Descending,
  CaseInsensitive: LongBool;
begin
  FillChar(IndexDesc{%H-}, SizeOf(IndexDesc), 0);
  with IndexDesc do
  begin
    IndexDesc.szName := TDBIString(Name);

    bUnique := ixUnique in Options;
    Descending := (ixDescending in Options) and (DescFields = '');
    CaseInsensitive := (ixCaseInsensitive in Options) and (CaseInsFields = '');
    Pos := 1;
    while (Pos <= Length(Fields)) and (iFields < MAXKEYFIELDS) do
    begin
{$ifndef fpc}
      iKeyFields[iFields] :=
        FieldDefList.FieldByName(ExtractFieldName(Fields, Pos)).FieldNo;
{$else}
      iKeyFields[iFields] :=
        Self.Fields.FieldByName(ExtractFieldName(Fields, Pos)).FieldNo;
{$endif}
      bDescending[iFields] := Descending;
      bCaseInsensitive[iFields] := CaseInsensitive;
      Inc(iFields);
    end;
    Pos := 1;
    while Pos <= Length(DescFields) do
      bDescending[IndexFieldOfs(ExtractFieldName(DescFields, Pos))] := True;
    Pos := 1;
    while Pos <= Length(CaseInsFields) do
      bCaseInsensitive[IndexFieldOfs(ExtractFieldName(CaseInsFields, Pos))] := True;
  end;
end;
{$ifdef DELPHI6}
  {$WARN SYMBOL_DEPRECATED ON}
{$endif}


procedure TDBIDataset.AddIndex(const Name, Fields: string;
  Options: TIndexOptions; const DescFields, CaseInsFields: string;
  const GroupingLevel: Integer);
var
  IndexDesc: DSIDXDesc;
  IndexDef: TIndexDef;
begin
  CheckBrowseMode;
  FieldDefs.Update;
  EncodeIndexDesc(IndexDesc, Name, Fields, DescFields, CaseInsFields, Options);
  CursorPosChanged;
  Check(FDSBase.CreateIndex(IndexDesc));
  if GroupingLevel > 0 then
  begin
    IndexDefs.Update;
    IndexDef := IndexDefs.Find(Name);
    if IndexDef <> nil then
      {$ifndef fpc} IndexDef.GroupingLevel := GroupingLevel {$endif} ;
  end
  else
    IndexDefs.Updated := False;
end;

procedure TDBIDataset.DeleteIndex(const Name: string);
begin
  CheckBrowseMode;
  if AnsiCompareText(Name, IndexName) = 0 then IndexName := '';
  Check(FDSBase.RemoveIndex(TDBIIndexName(Name)));
  IndexDefs.Updated := False;
end;


// _____________________________________________________________________________
{**
  Jvr - NOTE: this one is now different?
}
function TDBIDataset.GetIndexField(Index: Integer): TField;
var
  FieldNo: Integer;
  FieldName: string;
begin
  if (Index < 0) or (Index >= FIndexFieldCount) then
    DatabaseError(SFieldIndexError, Self);
  FieldNo := FIndexFieldMap[Index];
  Result := FieldByNumber(FieldNo);
  if Result = nil then
  begin
    if FieldNo-1 < FieldDefs.Count then
      FieldName := FieldDefs[FieldNo - 1].Name
    else
      FieldName := IntToStr(FieldNo);
    DatabaseErrorFmt(SIndexFieldMissing, [FieldName], Self);
  end;
end;

function TDBIDataset.GetIsIndexField(Field: TField): Boolean;
var
  I: Integer;
begin
  Result := False;

  if Field.FieldNo > 0 then
    for I := 0 to FIndexFieldCount - 1 do
      if FIndexFieldMap[I] = Field.FieldNo then
      begin
        Result := True;
        Exit;
      end;
end;

function TDBIDataset.GetIndexName: string;
begin
  if FFieldsIndex then
    Result := ''
  else
    Result := FIndexName;
end;                         

// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 15:34:15 - Initial code.<br>
}
function TDBIDataset.GetFileName: String;
begin
  Result := TDBIDataConnectionFriend(DataConnection).FileName;
end;


// _____________________________________________________________________________
{**
  Jvr - 29/06/2005 15:41:47 - Initial code.<br>
}
procedure TDBIDataset.SetFileName(const Value: string);
begin
  CheckInactive;
  TDBIDataConnectionFriend(DataConnection).FileName := Value;

  if not (csReading in ComponentState) then begin
    DataEvent(dePropertyChange, 0);
  end;
end;


procedure TDBIDataset.SetIndexName(const Value: string);
begin
  SetIndex(Value, False);
end;

procedure TDBIDataset.SetIndexField(Index: Integer; Value: TField);
begin
  GetIndexField(Index).Assign(Value);
end;

function TDBIDataset.GetIndexFieldNames: string;
begin
  if FFieldsIndex then
    Result := FIndexName
  else
    Result := '';
end;

procedure TDBIDataset.SetIndexFieldNames(const Value: string);
begin
  SetIndex(Value, Value <> '');
end;

function TDBIDataset.GetIndexFieldCount: Integer;
begin
  Result := FIndexFieldCount;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/09/2004 15:56:05 - Updated code.<p>
}
procedure TDBIDataset.SortOnFields(Cursor: IDBICursor; const Fields: string;
  CaseInsensitive, Descending: Boolean);
var
  I: Integer;
  FieldList: TDBIFieldList;
  DescFlags, CaseFlags: DSKEYBOOL;

  function GetFlags(Flag: LongBool; out FlagArray: DSKEYBOOL): Pointer;
  var
    J: Integer;
  begin
    if not Flag then
      Result := nil
    else
    begin
      for J := 0 to FieldList.Count - 1 do
        FlagArray[J] := True;
      Result := @FlagArray;
    end;
  end;

begin
  FieldList := TDBIFieldList.Create;
  try
    GetFieldList(FieldList, Fields);
    for I := 0 to FieldList.Count - 1 do
      if TField(FieldList[I]).FieldNo > 0 then
        FieldList[I] := Pointer(TField(FieldList[I]).FieldNo)
      else
        DatabaseError(SFieldIndexError, Self);
    Check(Cursor.SortOnFields(FieldList.Count, PPointer(FieldList.List),
      GetFlags(Descending, DescFlags), GetFlags(CaseInsensitive, CaseFlags)));
    GetIndexInfo('');
  finally
    FieldList.Free;
  end;
end;

{ Ranges / Keys }

procedure TDBIDataset.AllocKeyBuffers;
var
  KeyIndex: TKeyIndex;
begin
  try
    for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
      FKeyBuffers[KeyIndex] := AllocMem(SizeOf(TKeyBuffer) + FRecordSize);
    if Assigned(FCloneSource) then
      for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
        Move(FCloneSource.FKeyBuffers[KeyIndex]^, FKeyBuffers[KeyIndex]^,
          SizeOf(TKeyBuffer) + FRecordSize);
  except
    FreeKeyBuffers;
    raise;
  end;
end;

function TDBIDataset.GetKeyBuffer(KeyIndex: TKeyIndex): PKeyBuffer;
begin
  Result := FKeyBuffers[KeyIndex];
end;

procedure TDBIDataset.FreeKeyBuffers;
var
  KeyIndex: TKeyIndex;
begin
  for KeyIndex := Low(TKeyIndex) to High(TKeyIndex) do
    DisposeMem(FKeyBuffers[KeyIndex], SizeOf(TKeyBuffer) + FRecordSize);
end;

function TDBIDataset.InitKeyBuffer(Buffer: PKeyBuffer): PKeyBuffer;
begin
  FillChar(Buffer^, SizeOf(TKeyBuffer) + FRecordSize, 0);
  Check(FDSCursor.InitRecord(TDBIRecordBuffer(Buffer) + SizeOf(TKeyBuffer)));
  Result := Buffer;
end;

procedure TDBIDataset.CheckSetKeyMode;
begin
  if State <> dsSetKey then DatabaseError(SNotEditing, Self);
end;

// _____________________________________________________________________________
{**
  Jvr - 10/10/2002 11:47:26.<P>
}
function TDBIDataset.SetCursorRange: Boolean;
var
  RangeStart, RangeEnd: PKeyBuffer;
  StartKey, EndKey: TDBIRecordBuffer;
begin
  Result := False;
  if not (
    BuffersEqual(FKeyBuffers[kiRangeStart], FKeyBuffers[kiCurRangeStart],
    SizeOf(TKeyBuffer) + FRecordSize) and
    BuffersEqual(FKeyBuffers[kiRangeEnd], FKeyBuffers[kiCurRangeEnd],
    SizeOf(TKeyBuffer) + FRecordSize)) then
  begin
    CheckProviderEOF;
    RangeStart := FKeyBuffers[kiRangeStart];
    RangeEnd := FKeyBuffers[kiRangeEnd];
    StartKey := TDBIRecordBuffer(RangeStart) + SizeOf(TKeyBuffer);
    if not RangeStart.Modified then              //##JVR - 20121031
      StartKey := nil;                           //##JVR - 20121031
    EndKey := TDBIRecordBuffer(RangeEnd) + SizeOf(TKeyBuffer);
    if not RangeEnd.Modified then                //##JVR - 20121031
      EndKey := nil;                             //##JVR - 20121031
    Check(FDSCursor.SetRange(RangeStart.FieldCount, StartKey,
      not RangeStart.Exclusive, EndKey, not RangeEnd.Exclusive));
    Move(FKeyBuffers[kiRangeStart]^, FKeyBuffers[kiCurRangeStart]^,
      SizeOf(TKeyBuffer) + FRecordSize);
    Move(FKeyBuffers[kiRangeEnd]^, FKeyBuffers[kiCurRangeEnd]^,
      SizeOf(TKeyBuffer) + FRecordSize);
    DestroyLookupCursor;
    Result := True;
  end;
end;

function TDBIDataset.ResetCursorRange: Boolean;
begin
  Result := False;
  if FKeyBuffers[kiCurRangeStart].Modified or
    FKeyBuffers[kiCurRangeEnd].Modified then
  begin
    Check(FDSCursor.DropRange);
    InitKeyBuffer(FKeyBuffers[kiCurRangeStart]);
    InitKeyBuffer(FKeyBuffers[kiCurRangeEnd]);
    DestroyLookupCursor;
    Result := True;
  end;
end;

procedure TDBIDataset.SetLinkRanges(MasterFields: TDBIFieldList);
var
  I: Integer;
  SaveState: TDataSetState;
begin
  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := InitKeyBuffer(FKeyBuffers[kiRangeStart]);
    FKeyBuffer^.Modified := True;
    for I := 0 to MasterFields.Count - 1 do
      GetIndexField(I).Assign(TField(MasterFields[I]));
    FKeyBuffer^.FieldCount := MasterFields.Count;
  finally
    RestoreState(SaveState);
  end;
  Move(FKeyBuffers[kiRangeStart]^, FKeyBuffers[kiRangeEnd]^,
    SizeOf(TKeyBuffer) + FRecordSize);
end;

procedure TDBIDataset.SetKeyBuffer(KeyIndex: TKeyIndex; Clear: Boolean);
begin
  CheckBrowseMode;
  FKeyBuffer := FKeyBuffers[KeyIndex];
  Move(FKeyBuffer^, FKeyBuffers[kiSave]^, SizeOf(TKeyBuffer) + FRecordSize);
  if Clear then InitKeyBuffer(FKeyBuffer);
  SetState(dsSetKey);
  SetModified(FKeyBuffer.Modified);
  DataEvent(deDataSetChange, 0);
end;

procedure TDBIDataset.PostKeyBuffer(Commit: Boolean);
begin
  DataEvent(deCheckBrowseMode, 0);
  if Commit then
    FKeyBuffer.Modified := Modified
  else
    Move(FKeyBuffers[kiSave]^, FKeyBuffer^, SizeOf(TKeyBuffer) + FRecordSize);

  SetState(dsBrowse);
  DataEvent(deDataSetChange, 0);
end;

function TDBIDataset.GetKeyExclusive: Boolean;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.Exclusive;
end;

procedure TDBIDataset.SetKeyExclusive(Value: Boolean);
begin
  CheckSetKeyMode;
  FKeyBuffer.Exclusive := Value;
end;

function TDBIDataset.GetKeyFieldCount: Integer;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer.FieldCount;
end;

procedure TDBIDataset.SetKeyFieldCount(Value: Integer);
begin
  CheckSetKeyMode;
  FKeyBuffer.FieldCount := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 17/09/2001 13:46:23 - Added nested dataset support.<P>
}
procedure TDBIDataset.SetKeyFields(KeyIndex: TKeyIndex;
  const Values: array of const);
var
  I: Integer;
  SaveState: TDataSetState;
begin
  if FIndexFieldCount = 0 then DatabaseError(SNoFieldIndexes, Self);
  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := InitKeyBuffer(FKeyBuffers[KeyIndex]);
    if FParentDataSet = nil then
    begin
      for I := 0 to High(Values) do GetIndexField(I).AssignValue(Values[I]);
      FKeyBuffer^.FieldCount := High(Values) + 1;
    end
    else
    begin
      { Skip the linking field for nested datasets }
      for I := 0 to High(Values) do GetIndexField(I+1).AssignValue(Values[I]);
      FKeyBuffer^.FieldCount := High(Values) + 1;
    end;
    FKeyBuffer^.Modified := Modified;
  finally
    RestoreState(SaveState);
  end;
end;


function TDBIDataset.FindKey(const KeyValues: array of const): Boolean;
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  Result := GotoKey;
end;

procedure TDBIDataset.FindNearest(const KeyValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  GotoNearest
end;

function TDBIDataset.GotoKey: Boolean;
var
  KeyBuffer: PKeyBuffer;
  RecBuffer: TDBIRecordBuffer;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;
  CheckProviderEOF;
  KeyBuffer := FKeyBuffers[kiLookup];
  RecBuffer := TDBIRecordBuffer(KeyBuffer) + SizeOf(TKeyBuffer);
  Result := FDSCursor.GetRecordForKey(KeyBuffer.FieldCount, 0, RecBuffer, nil) = 0;
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TDBIDataset.GotoNearest;
begin
  raise EDBINotImplementedException.Create(Self, 'GotoNearest::5325');
end;
{$ifdef _UNUSED}
var
  SearchCond: DBISearchCond;
  KeyBuffer: PKeyBuffer;
  RecBuffer: TDBIRecordBuffer;
begin
  CheckBrowseMode;
  CheckProviderEOF;
  KeyBuffer := FKeyBuffers[kiLookup];
  RecBuffer := TDBIRecordBuffer(KeyBuffer) + SizeOf(TKeyBuffer);
  if KeyBuffer^.Exclusive then
    SearchCond := keySEARCHGT else
    SearchCond := keySEARCHGEQ;
  Check(FDSCursor.MoveToKey(SearchCond, KeyBuffer.FieldCount, 0, RecBuffer));
  Resync([rmCenter]);
end;
{$endif}


procedure TDBIDataset.SetKey;
begin
  SetKeyBuffer(kiLookup, True);
end;

procedure TDBIDataset.EditKey;
begin
  SetKeyBuffer(kiLookup, False);
end;

procedure TDBIDataset.ApplyRange;
begin
  CheckBrowseMode;
  if SetCursorRange then First;
  FRanged := True;
end;

procedure TDBIDataset.CancelRange;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  if ResetCursorRange then Resync([]);
  FRanged := False;
end;

procedure TDBIDataset.SetRange(const StartValues, EndValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiRangeStart, StartValues);
  SetKeyFields(kiRangeEnd, EndValues);
  ApplyRange;
end;

procedure TDBIDataset.SetRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, True);
end;

procedure TDBIDataset.SetRangeStart;
begin
  SetKeyBuffer(kiRangeStart, True);
end;

procedure TDBIDataset.EditRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, False);
end;

procedure TDBIDataset.EditRangeStart;
begin
  SetKeyBuffer(kiRangeStart, False);
end;



{ Master / Detail }

// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 13:53:14.<P>
}
procedure TDBIDataset.CheckDetailRecords;
var
  I, RecCount: Integer;
  MasterValues: OleVariant;
  Status: DBIResult;

begin
  Status := FDSCursor.GetRecordCount(RecCount);
  if DataSetField <> nil then
  begin
    if (Status = DBERR_DETAILSNOTFETCHED) and FetchOnDemand then
      FParentDataSet.FetchDetails;
  end
  else
  begin
    if (RecCount = 0) {and HasAppServer and not ProviderEOF and
      (FPacketRecords = 0)} and not MasterSource.DataSet.IsEmpty and
      (MasterSource.DataSet.State <> dsInsert) then
    begin
      MasterValues := VarArrayCreate([0, FMasterLink.Fields.Count - 1], varVariant);
      for I := 0 to FMasterLink.Fields.Count - 1 do
        MasterValues[I] := VarArrayOf([IndexFields[I].FieldName, TField(FMasterLink.Fields[I]).Value, IndexFields[I].DataType]);
{$ifdef _UNUSED}
      AddDataPacket(DoGetRecords(-1, RecCount, 0, '', MasterValues), False);
{$endif}
      if Active then First;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 13:55:05.<P>
}
procedure TDBIDataset.CheckMasterRange;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
  begin
    SetLinkRanges(FMasterLink.Fields);
    SetCursorRange;

    if FetchOnDemand then CheckDetailRecords;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 13:56:17.<P>
}
procedure TDBIDataset.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  if DataSetField = nil then
  begin
(*##JVR - Not used, may be required in the future
    //##JVR Custom Master/Detail mechanism
    if Assigned(FOnMasterChanged) then begin
      FOnMasterChanged(FMasterLink.Fields);
    end;
*)
{$ifdef _UNUSED}
    SetLinkRanges(FMasterLink.Fields);
    ApplyRange;
    if FetchOnDemand then CheckDetailRecords;
{$endif}
  end
  else
  begin
    if FParentDataSet.State = dsInsert then
      First
    else if not CompareMem(FLastParentBM, @PChar(FParentDataSet.ActiveBuffer)[FParentDataset.FBookmarkOfs], FParentDataSet.BookmarkSize) then
    begin
      if FetchOnDemand then CheckDetailRecords;
      First;
      Move(FParentDataSet.GetActiveBuffer[FParentDataSet.FBookmarkOfs], FLastParentBM[0], FParentDataSet.BookmarkSize);
    end
    else
    begin
      UpdateCursorPos;
      Resync([]);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 13:57:29.<P>
}
procedure TDBIDataset.MasterDisabled(Sender: TObject);
begin
  CancelRange;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 13:57:43.<P>
}
procedure TDBIDataset.SetDataSetField(const Value: TDataSetField);
begin
  if Assigned(Value) then
  begin
    Close;
{$ifdef _UNUSED}
    ProviderName := '';
    RemoteServer := nil;
    FileName := '';
{$endif}
  end;

  inherited;
end;


// _____________________________________________________________________________
{**
  This method serves to indicate if the dataset was created/Activated from Rtti information.<P>
  Thus in designmode and loading.<P>
  When the inherited loaded gets called, csloading is no longer true!<P>

  Jvr - 27/05/2002 18:13:43.<P>
}
procedure TDBIDataset.Loaded;
begin
  Include(FInternalOptions, ioLoading);
  try
    inherited Loaded;
  finally
    Exclude(FInternalOptions, ioLoading);
  end;

  if Assigned(FSavedPacket) then begin
    TDBIXmlData.LoadData(FSavedPacket, Self);
    ClearSavedPacket;
  end;
end;


procedure TDBIDataset.ReadData(Stream: TStream);
begin
  ReadDataPacket(Stream, True);
end;


// _____________________________________________________________________________
{**
  Jvr - 28/05/2002 15:54:24.<P>
}
procedure TDBIDataset.ReadInternalOptions(Reader: TReader);

  function ReadStr: AnsiString;
{$ifdef fpc}
  var
    Size: Byte;

  begin
    Reader.Read(Size, 1);
    SetLength(Result, Size);
    if Size > 0 then begin
      Reader.Read(Pointer(@Result[1])^, Size);
    end;
  end;
{$else}
  begin
    Result := AnsiString(Reader.ReadStr);
  end;
{$endif}


var
  EnumName: String;
  Value: Integer;
  
begin
  FInternalOptions := [];

  if (Reader.NextValue = vaSet) then begin
    Reader.ReadValue;

    while True do begin
      EnumName := String(ReadStr);
      if (EnumName = '') then Break;

      Value := GetEnumValue(TypeInfo(TDBIInternalOption), EnumName);
      if (Value = -1) then begin
        raise EDBIException.Create(Self, 'ReadInternalOptions::5625', SInvalidPropertyValue, []);
      end;
      Include(FInternalOptions, TDBIInternalOption(Value));
    end;
  end;
end;


procedure TDBIDataset.WriteData(Stream: TStream);
begin
  WriteDataPacket(Stream, True);
end;


// _____________________________________________________________________________
{**
  Jvr - 28/05/2002 15:54:40.<P>
}
procedure TDBIDataset.WriteInternalOptions(Writer: TWriter);

  procedure WriteValue(Value: TValueType);
  const
    Size = {$ifdef fpc} 1; {$else} SizeOf(Value); {$endif}
  begin
    Writer.Write(Value, Size);
  end;


  procedure WriteStr(const Value: AnsiString);
{$ifdef fpc}
  var
    Len: Integer;
    Size: Byte;

  begin
    Len := Length(Value);
    if (Len > 255) then begin
      Len := 255;
    end;

    Size := Len;
    Writer.Write(Size, 1);
    if (Len > 0) then begin
      Writer.Write(Value[1], Len);
    end;
  end;
{$else}
  begin
    Writer.WriteStr(Value);
  end;
{$endif}


var
  Option: TDBIInternalOption;

begin
  WriteValue(vaSet);
  for Option := Low(TDBIInternalOption) to High(TDBIInternalOption) do begin
    if (Option in FInternalOptions) then begin
      WriteStr(TDBIString(GetEnumName(TypeInfo(TDBIInternalOption), Ord(Option))));
    end;
  end;
  WriteStr('');
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 15:22:44.<P>
}
function TDBIDataset.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 15:23:00.<P>
}
procedure TDBIDataset.SetDataSource(Value: TDataSource);
begin
  if (Value <> nil) and (DataSetField <> nil) then
    DatabaseError(SNoNestedMasterSource, Self);

  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink, Self);

  FMasterLink.DataSource := Value;

{$ifdef _UNUSED}
  if not (csDesigning in ComponentState) or
    (csLoading in ComponentState) then Exit;

  if Assigned(Value) then
  begin
    if FPacketRecords = -1 then FPacketRecords := 0;
  end
  else
  begin
    if FPacketRecords = 0 then FPacketRecords := -1;
  end;
{$endif}
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 15:25:55.<P>
}
function TDBIDataset.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;


// _____________________________________________________________________________
{** OKASIS
  Jvr - 17/09/2001 15:26:11.<P>
}
procedure TDBIDataset.SetMasterFields(const Value: string);
begin
  FMasterLink.FieldNames := Value;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 15:30:40.<P>
}
procedure TDBIDataset.DoOnNewRecord;
var
  I: Integer;

begin
  if DataSetField = nil then
    if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
      for I := 0 to FMasterLink.Fields.Count - 1 do
        IndexFields[I] := TField(FMasterLink.Fields[I]);

{$ifdef _AGGREGATES}
  if (AggFields.Count > 0) and AggregatesActive then
    GetAggFieldData(ActiveBuffer);
{$endif}

  inherited DoOnNewRecord;
end;


// _____________________________________________________________________________
{**
  Jvr - 28/05/2002 14:31:57.<P>
}
procedure TDBIDataset.DefineProperties(Filer: TFiler);
  function StoreData: Boolean;
  begin
    Result := Active and (DataSetField = nil) and (FCloneSource = nil);
    if Result and Assigned(Filer.Ancestor) then begin
      Result := not TDBIDataset(Filer.Ancestor).Active or (TDBIDataset(Filer.Ancestor).DataSize <> Self.DataSize);
    end;
  end;

  function StoreInternalOptions: Boolean;
  begin
    Result := FInternalOptions <> [];
  end;

begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('InternalOptions', ReadInternalOptions, WriteInternalOptions, StoreInternalOptions);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, StoreData);
end;




{ Filters }

// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 17:40:14.<P>
}
procedure TDBIDataset.ActivateFilters;
begin
  if Filter <> '' then
    AddExprFilter(Filter, FilterOptions);
  if Assigned(OnFilterRecord) then
    AddFuncFilter;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 23:08:45.<P>
}
procedure TDBIDataset.DeactivateFilters;
begin
  if FFuncFilter <> nil then
  begin
    FDSCursor.DropFilter(FFuncFilter);
    FFuncFilter := nil;
  end;
  if FExprFilter <> nil then
  begin
    FDSCursor.DropFilter(FExprFilter);
    FExprFilter := nil;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 23:06:35.<P>
}
procedure TDBIDataset.AddExprFilter(const Expr: string; Options: TFilterOptions);
{$ifdef fpc}
begin
{$else}
var
  LExprParser: TExprParser;
begin
  if FExprFilter <> nil then FDSCursor.DropFilter(FExprFilter);
  if Expr <> '' then
  begin
    LExprParser := TExprParser.Create(Self, Expr, Options, [poExtSyntax], '', nil, FieldTypeMap{$ifdef DELPHIXE2}, True{$endif});
    try
      CheckProviderEOF;
      Check(FDSCursor.AddFilter(LExprParser.FilterData, LExprParser.DataSize, FExprFilter));
    finally
      LExprParser.Free;
    end;
  end;
{$endif}
end;

function TDBIDataset.FilterCallback(RecBuf: TDBIRecordBuffer): LongBool;
var
  SaveState: TDataSetState;
  Accept: Boolean;
begin
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := RecBuf;
  try
    Accept := True;
    OnFilterRecord(Self, Accept);
  except
    InternalHandleException;
  end;
  RestoreState(SaveState);
  Result := Accept;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 23:05:08<P>
}
procedure TDBIDataset.AddFuncFilter;
begin
  if FFuncFilter <> nil then FDSCursor.DropFilter(FFuncFilter);

  CheckProviderEOF;
  Check(FDSCursor.AddFilterCallBack(TDBIClientData(Self), @TDBIDataSet.FilterCallback,
    FFuncFilter));
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 23:03:32.<P>
}
procedure TDBIDataset.SetFilterData(const Text: string; Options: TFilterOptions);
begin
  if Active and Filtered then
  begin
    CheckBrowseMode;
    if (Filter <> Text) or (FilterOptions <> Options) then
      AddExprFilter(Text, Options);
    DestroyLookupCursor;
    First;
  end;
  inherited SetFilterText(Text);
  inherited SetFilterOptions(Options);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 23:02:56.<P>
}
procedure TDBIDataset.SetFilterText(const Value: string);
begin
  SetFilterData(Value, FilterOptions);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 23:02:15.<P>
}
procedure TDBIDataset.SetFilterOptions(Value: TFilterOptions);
begin
  SetFilterData(Filter, Value);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 22:59:22.<P>
}
procedure TDBIDataset.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active and Filtered then
  begin
    CheckBrowseMode;

    if Assigned(OnFilterRecord) <> Assigned(Value) then
    begin
      if Assigned(Value) then
      begin
        inherited SetOnFilterRecord(Value);
        AddFuncFilter;
      end
      else
        FDSCursor.DropFilter(FFuncFilter);

    end;
    DestroyLookupCursor;
    First;
  end;
  inherited SetOnFilterRecord(Value);
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 22/03/2001 22:58:53.<P>
}
procedure TDBIDataset.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;

    if Filtered <> Value then
    begin
      DestroyLookupCursor;
      FDSCursor.MoveToBOF;
      if Value then ActivateFilters else DeactivateFilters;
      StatusFilter := [];
      inherited SetFiltered(Value);
    end;
    First;
  end
  else
    inherited SetFiltered(Value);
end;


// _____________________________________________________________________________
{**
  Jvr - 01/03/2001 13:11:57 - Set the filter for the display of records.<P>

  <B>Notes:</B><BR>
  It is obvious that this record filtering mechanism doesn't map very well
  to the purpose required by the Xbase datasets and Object datasets.<P>

  Borland map usUnmodified to dsRecUnmodified which is a constant set to '0'.
  I have created a new constant, dsRecActive to use instead of dsUnmodified,
  because it is not possible to filter on Zero, causing no end of problems.
}
procedure TDBIDataset.SetStatusFilter(const Value: TUpdateStatusSet);
var
  StatusValues: Integer;

begin
  CheckBrowseMode;

  if Value <> [] then
  begin
    StatusValues := 0;

    // Jvr - this one added by me to deal with all active records
    if (usUnmodified in Value) or (usModified in Value) or (usInserted in Value) then begin
      StatusValues := dsRecActive;
    end;
{$ifdef _UNUSED}
    if usModified in Value then
      StatusValues := dsRecModified;

    if usInserted in Value then
      StatusValues := StatusValues + dsRecNew;
{$endif}
    if usDeleted in Value then
      StatusValues := StatusValues + dsRecDeleted;

    if Filtered then Filtered := False;

//##JVR    IndexName := szCHANGEINDEX;
    Check(FDSBase.SetProp(basepropCHANGEINDEX_VIEW, TDBIPropValue(StatusValues)));
  end
  else
  begin
{$ifdef _UNUSED}
    if IndexName = szCHANGEINDEX then
      IndexName := szDEFAULT_ORDER;

    Check(FDSBase.SetProp(dspropCHANGEINDEX_VIEW, TDBIPropValue(0)));
{$endif}
    //##JVR - New Code
    StatusValues := dsRecActive;
    Check(FDSBase.SetProp(basepropCHANGEINDEX_VIEW, TDBIPropValue(StatusValues)));
  end;

  FStatusFilter := Value;
  Resync([]);
end;



{ Lookups }

function TDBIDataset.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Status: DBIResult;
  Cursor: IDBICursor;
begin
  CheckBrowseMode;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;
  CheckProviderEOF;
  DoBeforeScroll;
  if not Filtered then
  begin
    if Restart then FFindCursor := nil;
    if not Assigned(FFindCursor) then
    begin
      ActivateFilters;
      try
        FFindCursor := CreateDSCursor(FDSCursor)
      finally
        DeactivateFilters;
      end;
    end
    else
      if not Restart then SyncCursors(FFindCursor, FDSCursor);
    Cursor := FFindCursor;
  end
  else
    Cursor := FDSCursor;
  if GoForward then
  begin
    if Restart then Check(Cursor.MoveToBOF);
    Status := Cursor.MoveRelative(1);
  end
  else
  begin
    if Restart then Check(Cursor.MoveToEOF);
    Status := Cursor.MoveRelative(-1);
  end;
  if Cursor <> FDSCursor then
  begin
    SyncCursors(FDSCursor, FFindCursor);
    Status := FDSCursor.GetCurrentRecord(nil);
  end;
  if Status = DBERR_NONE then
  begin
    Resync([rmExact, rmCenter]);
    SetFound(True);
  end;
  Result := Found;
  if Result then DoAfterScroll;
end;


// _____________________________________________________________________________
{**
  This method is not the same as in TClientdataset.

  In TClientdataset FLookupCursor is NOT assigned to nil. I'm not sure why this
  is the case.  Maybe we shouldn't be freeing FLookupCursor. At this stage to
  prevent memory leaks we will continue to do so!

  Jvr - 17/09/2001 17:04:27.<P>
}
procedure TDBIDataset.DestroyLookupCursor;
begin
  FLookupCursor := nil;
  FFindCursor := nil;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:50:25.<P>
}
function TDBIDataset.LocateRecord(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions;
  SyncCursor: Boolean): Boolean;
var
  Fields: TDBIFieldList;
  Buffer: TDBIRecordBuffer;
  I, FieldCount, PartialLength: Integer;
  Status: DBIResult;
  CaseInsensitive: Boolean;

begin
  CheckIsInitialised('LocateRecord::6050');

  CheckBrowseMode;
  CursorPosChanged;
  CheckProviderEOF;
  Buffer := GetTempBuffer;
  Fields := TDBIFieldList.Create;
  try
    GetFieldList(Fields, KeyFields);
    CaseInsensitive := loCaseInsensitive in Options;

    // Always create a new LookupCursor - Indices and / or Filters may have changed
    FLookupCursor := CreateDSCursor(FDSCursor);

    SortOnFields(FLookupCursor, KeyFields, CaseInsensitive, False);
    FFilterBuffer := Buffer;
    SetTempState(dsFilter);
    try
      InitRecord(Buffer);
      FieldCount := Fields.Count;

      if FieldCount = 1 then begin
        TField(Fields.First).Value := KeyValues;
      end
      else begin
        for I := 0 to FieldCount - 1 do begin
          TField(Fields[I]).Value := KeyValues[I];
        end;
      end;  { if }

      PartialLength := 0;
      if (loPartialKey in Options) and
        (TField(Fields.Last).DataType = ftString) then
      begin
        Dec(FieldCount);
        PartialLength := Length(TField(Fields.Last).AsString);
      end;  { if }

      Status := FLookupCursor.GetRecordForKey(FieldCount, PartialLength, Buffer, Buffer);
    finally
      RestoreState(dsBrowse);
    end;  { try..finally }

    if SyncCursor and (Status = DBIERR_NONE) then
      SyncCursors(FDSCursor, FLookupCursor);

  finally
    Fields.Free;
  end;  { try..finally }
  Result := Status = DBIERR_NONE;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:50:07.<P>
}
function TDBIDataset.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := Null;
  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TDBIRecordBuffer(TempBuffer));
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  OKASIS.
  Jvr - 17/09/2001 16:49:40.<P>
}
function TDBIDataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;


{$ifdef _UNUSED}
procedure TDBIDataset.GotoCurrent(DataSet: TDBIDataset);
begin
  CheckBrowseMode;
  CheckProviderEOF;
  DataSet.CheckActive;
  BookMark := DataSet.BookMark;
end;
{$endif}



{ Aggregates }

{$ifdef _AGGREGATES}

// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 21:06:22 - Initial code.<br />
}
function AggValueAsVariant(Agg: TDBIAggregate; Buffer: Pointer): Variant;
var
  C: Currency;
begin
  case Agg.DataType of
{$ifdef DELPHIXE2}
    ftByte:
      Result := Byte(Buffer^);
{$endif}
    ftWord:
      Result := Word(Buffer^);
{$ifdef DELPHIXE2}
    ftLongWord:
      Result := LongWord(Buffer^);
    ftShortInt:
      Result := ShortInt(Buffer^);
{$endif}
    ftSmallInt:
      Result := SmallInt(Buffer^);
    ftInteger:
      Result := LongInt(Buffer^);
{$ifdef DELPHI6}
    ftLargeint:
      Result := Largeint(Buffer^);
{$endif}
    ftBoolean:
      Result := WordBool(Buffer^);
{$ifdef DELPHI2009}
    TFieldType.ftSingle:
      Result := Single(Buffer^);
{$endif}
    ftFloat:
      Result := Double(Buffer^);
    ftBCD {$ifdef DELPHI6} , ftFMTBcd {$endif} :
      begin
        BCDToCurr(TBcd(Buffer^), C);
        Result := C;
      end;
    ftDate:
      Result := VarFromDateTime(TDateTimeRec(Buffer^).Date - DateDelta);
    ftTime:
      Result := VarFromDateTime(TDateTimeRec(Buffer^).Time / MSecsPerDay);
    ftDateTime:
      Result := VarFromDateTime((TDateTimeRec(Buffer^).DateTime / MSecsPerDay) - DateDelta);
{$ifdef _UNUSED}
    ftTimeStamp:
      Result := VarSQLTimeStampCreate(TSQLTimeStamp(Buffer^));
    ftTimeStampOffset:
      Result := VarSQLTimeStampOffsetCreate(TSQLTimeStampOffset(Buffer^));
{$endif}    
    ftString, ftGUID:
      Result := AnsiString(PAnsiChar(Buffer));
    ftWideString:
      begin
        VarClear(Result);
        with TVarData(Result) do
        begin
          VType := varOleStr;
          SetString(PWideString(@VOleStr)^, PWideChar(TDBIRecordBuffer(Buffer)+2), Word(Buffer^) div 2);
        end;
      end;
    else
      Result := Null;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 19:57:31 - Initial code.<br />
}
procedure TDBIDataset.SetAggregates(Value: TDBIAggregates);
begin
  FAggregates.Assign(Value);
end;


procedure TDBIDataset.SetAggsActive(Value: Boolean);
begin
  if FAggregatesActive <> Value then
  begin
    FAggregatesActive := Value;
    if FAggregatesActive and Active then
    begin
      ResetAllAggs(FAggregatesActive);
      if AggFields.Count > 0 then
      begin
        UpdateCursorPos;
        Resync([]);
      end;
    end;
  end;
end;

procedure TDBIDataset.ClearActiveAggs;
var
  I: Integer;
begin
  if FActiveAggLists <> nil then
    for I:= 0 to FActiveAggLists.Count - 1  do
      if FActiveAggLists[I] <> nil then TList(FActiveAggLists[I]).Free;
  FActiveAggLists.Clear;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:37:03 - Initial code.<br />
}
procedure TDBIDataset.CloseAggs;
var
  I: Integer;
  Field: TAggregateField;
begin
  for I := 0 to FAggregates.Count - 1 do
    begin
      if (FAggregates[I].AggHandle <> 0) and (FDSCursor <> nil) then
        Check(FDSCursor.DropAggregate(FAggregates[I].AggHandle));
      FAggregates[I].AggHandle := 0;
      FAggregates[I].FInUse := False;
    end;
  for I := 0 to AggFields.Count - 1 do
  begin
    Field := AggFields[I] as TAggregateField;
    if Field.Handle <> nil then
    begin
       TDBIAggregate(Field.Handle).Free;
       Field.Handle := nil;
    end;
  end;
  FAggFieldsInit := False;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 11:10:22 - Initial code.<br />
}
procedure TDBIDataset.ResetGrouping;

  function HasAggs(Level: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FAggregates.Count - 1 do
      if TDBIAggregate(FAggregates[I]).GroupingLevel = Level then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  I: Integer;
  Agg: TDBIAggregate;
begin
  FGroupingLevel := FMaxAggGroupingLevel;
  if FIndexGroupingLevel > FGroupingLevel then
    FGroupingLevel := FIndexGroupingLevel;
  for I:= 1 to FGroupingLevel do
  begin
    if not HasAggs(I) then
    begin
      Agg := FAggregates.Add;
      Agg.GroupingLevel := I;
      Agg.IndexName := FIndexName;
      Agg.Active := True;
      Agg.Activate;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:36:20 - Initial code.<br />
}
procedure TDBIDataset.ResetAgg(Agg: TDBIAggregate; DeleteFirst: Boolean);
var
  I, J: Integer;
begin
  if DeleteFirst then
    for I:=0 to FActiveAggLists.Count - 1 do
    begin
      J := TList(FActiveAggLists.Items[I]).IndexOf(Agg);
      if J <> -1 then
      begin
        TList(FActiveAggLists.Items[I]).Delete(J);
        TList(FActiveAggLists.Items[I]).Pack;
        TList(FActiveAggLists.Items[I]).Capacity := TList(FActiveAggLists.Items[I]).Count;

        Break;
      end;
    end;

  if Agg.Active and ((AnsiCompareText(Agg.IndexName, FIndexName) = 0)
     or (Agg.GroupingLevel = 0)) then
  begin
//##AGREGATES    if Agg.DataSet = nil then Agg.FDataSet := Self;
    if Agg.DataSet = nil then Agg.FDataSet := Self;
    Agg.Activate;
//##AGREGATES    Agg.FInUse := True;
    Agg.FInUse := True;
    if Agg.GroupingLevel > FMaxAggGroupingLevel then
      FMaxAggGroupingLevel := Agg.GroupingLevel;
    while FActiveAggLists.Count <= Agg.GroupingLevel do
      FActiveAggLists.Add(TList.Create);
    if Agg.Expression <> '' then
      TList(FActiveAggLists.Items[Agg.GroupingLevel]).Add(Agg);
  end
  else
  begin
//##AGREGATES    Agg.FInUse := False;
    Agg.FInUse := False;
    if Agg.AggHandle <> 0 then
    begin
      DSCursor.DropAggregate(Agg.AggHandle);
      Agg.AggHandle := 0;
    end
  end;
  if FMaxAggGroupingLevel > GroupingLevel then
    FGroupingLevel := FMaxAggGroupingLevel;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 18:55:50 - Initial code.<br />
}
procedure TDBIDataset.ResetAllAggs(Value: Boolean);

  procedure CreateAggsFromAggFields;
  var
    I, MaxGrp: Integer;
    Agg: TDBIAggregate;
    Field: TAggregateField;
  begin
    { Link persistent aggregate fields with TAggregate objects }
    MaxGrp := 0;
    FAggFieldsSize := 0;
    for I := 0 to AggFields.Count - 1 do
    begin
      Field := AggFields[I] as TAggregateField;
      if (Field.GroupingLevel = 0) or (Field.IndexName = FIndexName) then
      begin
        if Field.GroupingLevel > MaxGrp then
          MaxGrp := Field.GroupingLevel;
        if Field.Handle = nil then
        begin
          Agg := TDBIAggregate.Create(nil, Self);
          Agg.Assign(Field);
          if not Agg.Active and (Agg.Expression <> '') then
          begin
            Agg.Active := True;
            Agg.Active := False;
          end;
          Field.Handle := Agg;
          Field.ResultType := Agg.DataType;
        end
        else
        begin
          Agg := TDBIAggregate(Field.Handle);
          Field.ResultType := Agg.DataType;
        end;
        Agg.RecBufOfs := FAggFieldsSize;
        Inc(FAggFieldsSize, Agg.DataSize + 1); { add one for null ind. }
      end;
    end;
    FAggGrpIndSize := MaxGrp * Sizeof(TGroupPosInds);
    FAggFieldsInit := True;
  end;
var
  I: Integer;
  Agg: TDBIAggregate;
  Field: TAggregateField;
begin
{$ifdef _PARENTDATASETS}
  if (FParentDataset <> nil) and (csLoading in FParentDataSet.ComponentState) then Exit;
{$endif}
  ClearActiveAggs;
  if FAggFieldsUpdated = nil then
    FAggFieldsUpdated := TBits.Create;
  if AggFields.Count + FAggregates.Count = 0 then Exit;
  FGroupingLevel := 0;
  FMaxAggGroupingLevel := 0;
  if not FAggFieldsInit then
    CreateAggsFromAggFields;
  if Assigned(DSCursor) and FAggregatesActive then
  begin
    for I := 0 to FAggregates.Count - 1 do
    begin
//##AGGREGATES      Agg := FAggregates.GetItem(I);
      Agg := FAggregates.Items[I];
      if Value then
        ResetAgg(Agg, False) else
//##AGGREGATES        Agg.FInUse := False;
        Agg.InUse := False;
    end;
    for I := 0 to AggFields.Count - 1 do
    begin
      Field := AggFields[I] as TAggregateField;
      if Field.Handle <> nil then
      begin
        if (Field.GroupingLevel <> 0) and (Field.IndexName <> FIndexName) then
           TDBIAggregate(Field.Handle).FInUse := False
        else
           TDBIAggregate(Field.Handle).FInUse := True;
      end;
    end;
    ResetGrouping;
    DoAggUpdates(False);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 10:17:32 - Initial code.<br />
}
function TDBIDataset.InternalGetGroupState(Level: Integer): TGroupPosInds;
var
  Status: DBIResult;
  DSGrpState: GROUPSTATE;

begin
  Status := FDSCursor.GetSubGroupState(Level, DSGrpState);
  if (Status = DBERR_NONE) or (Status = DBERR_BOF) or (Status = DBERR_EOF) then
    case DSGrpState of
      grSTATEMIDDLE: Result := [gbMiddle];
      grSTATEFIRST: Result := [gbFirst];
      grSTATELAST: Result := [gbLast];
      grSTATEFIRSTLAST: Result := [gbFirst, gbLast];
    end
  else
    Result := [];
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 10:19:03 - Initial code.<br />
}
function TDBIDataset.GetDebugInfo: String;
var
  Status: DBIResult;

begin
  UpdateCursorPos;

  Status := FDSCursor.GetDebugInfo(Result);
  if (Status <> DBERR_NONE) then
    Result := 'Error!';

end;  { GetDebugInfo }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 10:19:03 - Initial code.<br />
}
function TDBIDataset.GetGroupState(Level: Integer): TGroupPosInds;
begin
  if not Active or not AggregatesActive or (FIndexName = '') then
    Result := []
  else
  begin
    if Level > FGroupingLevel then
      DatabaseError(SAggsNoSuchLevel, Self);
    UpdateCursorPos;
    Result := InternalGetGroupState(Level);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:35:02 - Initial code.<br />
}
function TDBIDataset.GetActiveAggs(Index: Integer): TList;
begin
  if Index < FActiveAggLists.Count then
    Result := FActiveAggLists.Items[Index]
  else
    Result := nil;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:35:46 - Initial code.<br />
}
procedure TDBIDataset.DoAggUpdates(IsUpdate: Boolean);

  function Intersect(List1, List2: TBits): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to List1.Size - 1 do
      if List1[I] and List2[I] then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

var
  I: Integer;
  Agg: TDBIAggregate;
begin
  for I := 0 to FAggregates.Count - 1 do
  begin
    Agg := FAggregates.Items[I];
    if Assigned(Agg.OnUpdate) and Agg.Active and Agg.InUse then
    begin
      if not IsUpdate or Intersect(FAggFieldsUpdated, Agg.DependentFields) then
        Agg.OnUpdate(Agg);
    end;
  end;
  for I := 0 to FAggFieldsUpdated.Size - 1 do
    FAggFieldsUpdated[I] := False;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:37:32 - Initial code.<br />
}
procedure TDBIDataset.GetAggFieldData(Buffer: TDBIRecordBuffer);
type
  PTGroupPosInds = ^TGroupPosInds;
var
  I: Integer;
  Agg: TDBIAggregate;
  Blank: LongBool;
  PAggData: TDBIRecordBuffer;
begin
  for I := 0 to AggFields.Count - 1 do
  begin
    Agg := TDBIAggregate(TAggregateField(AggFields[I]).Handle);
    if (Agg <> nil) and Agg.InUse then
    begin
      PAggData := TDBIRecordBuffer(Buffer) + FAggFieldsOfs + Agg.RecBufOfs;
      if Agg.InUse  and Agg.Active and (FDSCursor.GetAggregateValue(Agg.AggHandle,
         Pointer(PAggData+1), Blank) = DBERR_NONE) and not Blank then
        PAggData[0] := TDBIRecordElement(#0)
      else
        PAggData[0] := TDBIRecordElement(#1);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:38:20 - Initial code.<br />
}
function TDBIDataset.GetAggregateValue(Field: TField): Variant;
var
  Agg: TDBIAggregate;
  RecBuf: TDBIRecordBuffer;

begin
  Result := NULL;

 if FAggregatesActive and GetActiveRecBuf(RecBuf) then
  begin
    Agg := TDBIAggregate(TAggregateField(Field).Handle);

    if Agg <> nil then
    begin
      if Agg.InUse then
      begin
        Inc(RecBuf, FAggFieldsOfs + Agg.RecBufOfs);
        if RecBuf[0] = TDBIRecordElement(#1) then
          Result := NULL
        else
          Result := AggValueAsVariant(Agg, RecBuf + 1)
      end
      else
        Result := NULL;

    end;

  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:46:57 - Initial code.<br />
}
procedure TDBIDataset.ResetAggField(Field: TField);
var
  I: Integer;
  Agg: TDBIAggregate;
  AggF: TAggregateField;

begin
  for I := 0 to AggFields.Count - 1 do
    if AggFields[I] = Field then
    begin
      AggF := AggFields[I] as TAggregateField;
      Agg := TDBIAggregate(AggF.Handle);
      if Agg <> nil then
      begin
        Agg.Active := False;
        Agg.Assign(AggF);
      end;
    end;
end;
{$endif}


{$ifdef _UNUSED}
procedure TDBIDataset.DoAfterApplyUpdates(var OwnerData: OleVariant);
begin
  if Assigned(FAfterApplyUpdates) then FAfterApplyUpdates(Self, OwnerData);
end;

procedure TDBIDataset.DoBeforeApplyUpdates(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeApplyUpdates) then FBeforeApplyUpdates(Self, OwnerData);
end;

function TDBIDataset.DoApplyUpdates(Delta: OleVariant; MaxErrors: Integer;
  out ErrorCount: Integer): OleVariant;
var
  OwnerData: OleVariant;
begin
  DoBeforeApplyUpdates(OwnerData);
  Result := AppServer.AS_ApplyUpdates(ProviderName, Delta, MaxErrors, ErrorCount, OwnerData);
  DoAfterApplyUpdates(OwnerData);
end;

procedure TDBIDataset.DoBeforeGetRecords(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeGetRecords) then FBeforeGetRecords(Self, OwnerData);
end;

procedure TDBIDataset.DoAfterGetRecords(var OwnerData: OleVariant);
begin
  if Assigned(FAfterGetRecords) then FAfterGetRecords(Self, OwnerData);
end;

function TDBIDataset.DoGetRecords(Count: Integer; out RecsOut: Integer;
  Options: Integer; const CommandText: WideString; Params: OleVariant): OleVariant;
var
  OwnerData: OleVariant;
begin
  DoBeforeGetRecords(OwnerData);
  if VarIsEmpty(Params) and (Self.Params.Count > 0) then
    Params := PackageParams(Self.Params);
  Result := AppServer.AS_GetRecords(ProviderName, Count, RecsOut, Options,
    CommandText, Params, OwnerData);
  UnPackParams(Params, Self.Params);
  DoAfterGetRecords(OwnerData);
end;

procedure TDBIDataset.DoAfterRowRequest(var OwnerData: OleVariant);
begin
  if Assigned(FAfterRowRequest) then FAfterRowRequest(Self, OwnerData);
end;

procedure TDBIDataset.DoBeforeRowRequest(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeRowRequest) then FBeforeRowRequest(Self, OwnerData);
end;

function TDBIDataset.DoRowRequest(Row: OleVariant; RequestType: Integer): OleVariant;
var
  OwnerData: OleVariant;
begin
  DoBeforeRowRequest(OwnerData);
  Result := AppServer.AS_RowRequest(ProviderName, Row, RequestType, OwnerData);
  DoAfterRowRequest(OwnerData);
end;

procedure TDBIDataset.DoAfterExecute(var OwnerData: OleVariant);
begin
  if Assigned(FAfterExecute) then FAfterExecute(Self, OwnerData);
end;

procedure TDBIDataset.DoBeforeExecute(var OwnerData: OleVariant);
begin
  if Assigned(FBeforeExecute) then FBeforeExecute(Self, OwnerData);
end;

procedure TDBIDataset.DoExecute(Params: OleVariant);
var
  OwnerData: OleVariant;
begin
  DoBeforeExecute(OwnerData);
  AppServer.AS_Execute(ProviderName, CommandText, Params, OwnerData);
  UnPackParams(Params, Self.Params);
  DoAfterExecute(OwnerData);
end;
{$endif}

function TDBIDataset.ConstraintsStored: Boolean;
begin
  Result := Constraints.Count > 0;
end;

{$ifdef _UNUSED}
procedure TDBIDataset.SetupConstraints;
type
  TConstraintType = (ctField, ctRecord, ctDefault);

  procedure AddSQLExprAttr(ExprParser: TExprParser; const ExprText, ExprErrMsg,
    FieldName: string; FieldIndex: Integer; ConstraintType: TConstraintType;
    Required: Boolean);
  type
    PSQLExprInfo = ^TSQLExprInfo;
    TSQLExprInfo = packed record
      iErrStrLen: Integer;
      iFldNum: Integer;
      bReqExpr: BYTE;
    end;
  const
    TypeStr: array[TConstraintType] of string = (szBDEDOMCL, szBDERECCL, szBDEDEFCL);
    Attr: Integer = dsVaryingFldType or SizeOf(Integer) or (dsfldBYTES shl dsSizeBitsLen);
  var
    AttrType: Pointer;
    Len, AttrSize: Integer;
    SQLExprInfo: PSQLExprInfo;
    Options: TParserOptions;
    ErrorStr, LBuffer: TArray<Byte>;
    M: TMarshaller;
  begin
    try
      SetLength(LBuffer, 4096);
      if ExprText = '' then Exit;
      if (ConstraintType <> ctDefault) and (ExprErrMsg = '') then
      begin
        if (ConstraintType = ctField) and (FieldName <> '') then
          ErrorStr := TEncoding.UTF8.GetBytes(Format('%s %s: %s %s',[SConstraintFailed, SField, FieldName, ExprText]))
        else
          ErrorStr := TEncoding.UTF8.GetBytes(Format('%s %s',[SConstraintFailed, ExprText]));
      end
      else
        ErrorStr := TEncoding.UTF8.GetBytes(ExprErrMsg);
      Len := Length(ErrorStr);
      SetLength(ErrorStr, Length(ErrorStr) + 1);
      ErrorStr[Length(ErrorStr)-1] := 0;
      if (Len > 0) then Inc(Len);
      SQLExprInfo := @LBuffer[0];
      SQLExprInfo.iErrStrLen := Len;
      SQLExprInfo.iFldNum := FieldIndex;
      SQLExprInfo.bReqExpr := Ord(Required);
      Options := [poExtSyntax];
      if ConstraintType = ctDefault then Include(Options, poDefaultExpr);
      if ConstraintType = ctRecord then Include(Options, poUseOrigNames);
      if FieldName <> '' then Include(Options, poFieldNameGiven);
      ExprParser.SetExprParams(ExprText, [], Options, FieldName);
      Move(ExprParser.FilterData[0], LBuffer[SizeOf(TSQLExprInfo) + Len ], ExprParser.DataSize);
      AttrSize := ExprParser.DataSize + SizeOf(TSQLExprInfo) + Len;
      if Len > 0 then
      begin
        if Length(ErrorStr) < (Length(LBuffer) - SizeOf(TSQLExprInfo)) then
          Move(ErrorStr[0], LBuffer[SizeOf(TSQLExprInfo)], Length(ErrorStr))
        else
          Move(ErrorStr[0], LBuffer[SizeOf(TSQLExprInfo)], Length(LBuffer) - SizeOf(TSQLExprInfo));
      end;
      AttrType := M.AsAnsi(TypeStr[ConstraintType]).ToPointer;
      Check(FDSBase.AddOptParameter(0, AttrType, Attr, AttrSize + SizeOf(Integer),
        LBuffer));
    finally
      LBuffer := nil;
    end;
  end;

var
  I: Integer;
  ExprParser: TExprParser;
  ErrMsg: string;
begin
  ExprParser := TExprParser.Create(Self, '', [], [], '', nil, FieldTypeMap, True);
  try
    if Constraints.Count > 0 then
    begin
      try
        for I := 0 to Constraints.Count - 1 do
        begin
          AddSQLExprAttr(ExprParser, Constraints[I].ImportedConstraint, Constraints[I].ErrorMessage, '', 0,
            ctRecord, False);
          AddSQLExprAttr(ExprParser, Constraints[I].CustomConstraint, Constraints[I].ErrorMessage, '', 0,
            ctRecord, False);
        end;
      except
        if Name <> '' then
          ErrMsg := Format('%s: %s',[Name, SRecConstFail])
        else
          ErrMsg := SRecConstFail;
        if ExceptObject is Exception then
          raise EDSWriter.CreateFmt(ErrMsg, [Exception(ExceptObject).Message])
        else
          raise EDSWriter.CreateFmt(ErrMsg, ['']);
      end;
    end;
    for I := 0 to FieldList.Count - 1 do
    begin
      try
        AddSQLExprAttr(ExprParser, FieldList[I].DefaultExpression, '', FieldList[I].FullName, FieldList[I].FieldNo,
          ctDefault, False);
      except
        if FieldList[I].Name <> '' then
          ErrMsg := Format('%s: %s',[FieldList[I].Name, SDefExprFail])
        else if FieldList[I].DataSet.Name <> '' then
          ErrMsg := Format('%s.%s: %s',[FieldList[I].Name, FieldList[I].FullName, SDefExprFail])
        else
          ErrMsg := Format('%s: %s',[FieldList[I].FullName, SDefExprFail]);
        if ExceptObject is Exception then
          raise EDSWriter.CreateFmt(ErrMsg, [Exception(ExceptObject).Message])
        else
          raise EDSWriter.CreateFmt(ErrMsg, ['']);
      end;
      try
        AddSQLExprAttr(ExprParser, FieldList[I].ImportedConstraint, FieldList[I].ConstraintErrorMessage,
          FieldList[I].FullName, FieldList[I].FieldNo, ctField, False);
        AddSQLExprAttr(ExprParser, FieldList[I].CustomConstraint, FieldList[I].ConstraintErrorMessage,
          FieldList[I].FullName, FieldList[I].FieldNo, ctField, False);
      except
        if FieldList[I].Name <> '' then
          ErrMsg := Format('%s: %s',[FieldList[I].Name, SFieldConstFail])
        else if FieldList[I].DataSet.Name <> '' then
          ErrMsg := Format('%s.%s: %s',[FieldList[I].Name, FieldList[I].FullName, SFieldConstFail])
        else
          ErrMsg := Format('%s: %s',[FieldList[I].FullName, SFieldConstFail]);
        if ExceptObject is Exception then
          raise EDSWriter.CreateFmt(ErrMsg, [Exception(ExceptObject).Message])
        else
          raise EDSWriter.CreateFmt(ErrMsg, ['']);
      end;
    end;
  finally
    ExprParser.Free;
  end;
end;

procedure TDBIDataset.SetConnectionBroker(
  const Value: TConnectionBroker);
begin
  if Assigned(FConnectionBroker) then FConnectionBroker.UnRegisterClient(Self);
  FConnectionBroker := Value;
  if Assigned(Value) then
  begin
    Value.RegisterClient(Self);
    Value.FreeNotification(Self);
    SetRemoteServer(Nil);
  end;
end;

function TDBIDataset.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result := inherited GetIndexDefs(IndexDefs, IndexTypes);
end;

function TDBIDataset.PSGetCommandText: string;
begin
  Result := CommandText;
end;

function TDBIDataset.PSGetCommandType: TPSCommandType;
begin
  Result := ctUnknown;
end;

function TDBIDataset.GetIsClone: Boolean;
var
  BaseCDS: TDBIDataset;
begin
  BaseCDS := Self;
  while Assigned(BaseCDS.DataSetField) do
    BaseCDS := BaseCDS.DataSetField.DataSet as TDBIDataset;
  Result := Assigned(BaseCDS.CloneSource);
end;
{$endif}



{ TClientBlobStream }

// _____________________________________________________________________________
{**
  Create a blob stream.

  Jvr - 30/05/2001 17:31:09 - Added the PRecbuf parameter to allow for Blob
                              operations on external buffers.
                              (see LoadFromDataset)<P>
}
constructor TDBIBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode; PRecBuf: TDBIRecordBuffer);
begin
  FField := Field;
  FFieldNo := FField.FieldNo;
  FDataSet := FField.DataSet as TDBIDataSet;
  if PRecBuf <> nil then begin
    FBuffer := PRecBuf;
  end
  else if not FDataSet.GetActiveRecBuf(FBuffer) then begin
    Exit;
  end;

  if Mode <> bmRead then
  begin
    if FField.ReadOnly then
      FField.ReadOnly := False;
//##JVR      DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName], FDataSet);

    // If we are not using an external buffer (PRecbuf = nil)
    // and the dataset is not in an edit state, then raise an exception
    if (PRecBuf = nil) and not (FDataSet.State in [dsEdit, dsInsert]) then begin
      DatabaseError(SNotEditing, FDataSet);
    end;
  end;

  if (Mode = bmWrite) then begin
    Truncate;
  end
  else begin
    ReadBlobData;
  end;
end;


// _____________________________________________________________________________
{**
  ..<P>
}
destructor TDBIBlobStream.Destroy;
begin
  if Modified then
  try
    FDataSet.Check(FDataSet.FDSCursor.PutBlob(FBuffer, FFieldNo, 0, Memory, Size));
    FField.Modified := True;
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    InternalHandleException;
  end;

  inherited Destroy;
end;


function TDBIBlobStream.GetModified: Boolean;
begin
  Result := _Modified;
end;


procedure TDBIBlobStream.InternalHandleException;
begin
{$ifndef DELPHI6}
  Application.HandleException(Self);
{$else}
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Self);
{$endif}
end;


// _____________________________________________________________________________
{**
  ..<P>
}
{$IFDEF DBDEBUG}
procedure TDBIBlobStream.ReadBlobData;
var
  TestData: TStringStream;
  TextBuffer: String;
  HexBuffer: String;
  Count, Index: Integer;

  function IsAscii(AChar: Integer): Boolean;
  begin
    Result := (AChar >= $20) and (AChar < $7F);
  end;

begin
  try
    for Count := 0 to FDataSet.RecordSize-1 do begin
      if IsAscii(Ord(FBuffer[Count])) then begin
        TextBuffer := TextBuffer + Format('%s', [FBuffer[Count]]);
      end
      else begin
        TextBuffer := TextBuffer + Format('{%2.2x}', [Ord(FBuffer[Count])]);
      end;
    end;  { for }

    for Index := 0 to FDataSet.RecordSize-1 do begin
      if (Index Mod 16 = 0) then begin
        HexBuffer := HexBuffer + #13 + ' |' +
          Format('%16.16s', [Copy(FBuffer, Index, 16)]) +
          Format('[%4.4x] ', [Index]);
      end;

      HexBuffer := HexBuffer + Format(' %2.2x', [Ord(FBuffer[Index])]);
    end;  { for }
    HexBuffer := HexBuffer + #13#13;

  except
    on E: Exception do
      raise Exception.Create('Failed to get field data');
  end;  { try..except }

  try
    TestData := TStringStream.Create(HexBuffer + TextBuffer);
    try
      LoadFromStream(TestData);
    finally
      TestData.Free;
    end;
  except
    on E: Exception do
      Exception.Create('Failed to read Blob data');
  end;
end;

{$else}

procedure TDBIBlobStream.ReadBlobData;
var
  BlobLen: LongWord;

begin
  FDataSet.Check(FDataSet.FDSCursor.GetBlobLen(FBuffer, FFieldNo, BlobLen));
  if BlobLen > 0 then
  begin
    Position := 0;
    SetSize(BlobLen);
    FDataSet.Check(FDataSet.FDSCursor.GetBlob(FBuffer, FFieldNo, 0, Memory, BlobLen));
  end;
end;

{$endif}


procedure TDBIBlobStream.SetModified(const Value: Boolean);
begin
  _Modified := Value;
end;


function TDBIBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  Modified := True;
end;

procedure TDBIBlobStream.Truncate;
begin
  Clear;
  Modified := True;
end;





{$ifdef _AGGREGATES}

{ TDBIAggregates }

constructor TDBIAggregates.Create(Owner: TPersistent);
begin
  inherited Create(TDBIAggregate);
  FOwner := Owner;
end;

// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 16:05:36 - Initial code.<br />
}
function TDBIAggregates.Add: TDBIAggregate;
begin
  Result := TDBIAggregate(inherited Add);
  Result.FDataSet := TDBIDataset(GetOwner);
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 16:06:47 - Initial code.<br />
}
procedure TDBIAggregates.Clear;
var
  DataSet: TDBIDataset;
begin
  inherited Clear;

  DataSet := TDBIDataset(GetOwner);
  if DataSet <> nil then
    DataSet.ResetAllAggs(DataSet.AggregatesActive);
end;


function TDBIAggregates.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TDBIAggregates.GetItem(Index: Integer): TDBIAggregate;
begin
  Result := TDBIAggregate(inherited GetItem(Index));
end;

procedure TDBIAggregates.SetItem(Index: Integer; Value: TDBIAggregate);
begin
  inherited SetItem(Index, Value);
end;

function TDBIAggregates.IndexOf(const DisplayName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiCompareText(TDBIAggregate(Items[Result]).DisplayName, DisplayName) = 0 then Exit;
  Result := -1;
end;

function TDBIAggregates.Find(const DisplayName: string): TDBIAggregate;
var
  I: Integer;
begin
  I := IndexOf(DisplayName);
  if I < 0 then Result := nil else Result := TDBIAggregate(Items[I]);
end;





{ TDBIAggregate }

// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 15:46:43 - Initial code.<br />
}
constructor TDBIAggregate.Create(Aggregates: TDBIAggregates; ADataSet: TDBIDataSet);
begin
  Assert(ADataset is TDBIDataset);

  FDataSet := ADataSet;
  inherited Create(Aggregates);
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 15:53:44 - Initial code.<br />
}
destructor TDBIAggregate.Destroy;
var
  I: Integer;
begin
  if Assigned(FDataSet) and Assigned(FDataSet.AggFields) then
    for I := 0 to FDataSet.AggFields.Count - 1 do
    begin
      if FHAggregate <> 0 then
      begin
        FDataset.FDSCursor.DropAggregate(FHAggregate);
        FHAggregate := 0;
      end;
      if TAggregateField(FDataSet.AggFields[I]).Handle = Self then TAggregateField(FDataSet.AggFields[I]).Handle := nil;
    end;

  if FDependentFields <> nil then
    FDependentFields.Free;

  inherited Destroy;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:58:20 - Initial code.<br />
}
procedure TDBIAggregate.Activate;
var
  Parser: TExprParser;

begin
  if FOutOfDate and (FHAggregate <> 0) then
  begin
    FDataSet.Check(FDataSet.DSCursor.DropAggregate(FHAggregate));
    FHAggregate := 0;
  end;
  FOutOfDate := False;
  if FHAggregate = 0 then
  begin
    if FDependentFields = nil then
      FDependentFields := TBits.Create;
    if FExpression <> '' then
    begin
      Parser := TExprParser.Create(FDataSet, FExpression, [],
        [poExtSyntax, poAggregate, poFieldDepend], '', FDependentFields, FieldTypeMap{$ifdef DELPHIXE2}, True{$endif});
      try
        FDataset.Check(FDataSet.DSCursor.AddAggregate(GroupingLevel,
          Parser.DataSize, Parser.FilterData, FHAggregate));
        FDataset.Check(FDataSet.DSCursor.GetAggregateDesc(FHAggregate, FFldDesc));
        SetLength(FDataBuffer, FFldDesc.iFldLen);
        if FFldDesc.iFldType < MAXLOGFLDTYPES then
          FDataType := DataTypeMap[FFldDesc.iFldType]
        else if FFldDesc.iFldType = fldUNICODE then
          FDataType := ftWideString
{$ifdef DELPHI2009}
        else if FFldDesc.iFldType = fldDATETIMEOFFSET then
          FDataType := ftTimeStampOffset
{$endif}          
        else
          FDataType := ftUnknown;
        FDataSize := FFldDesc.iFldLen;
      finally
        Parser.Free;
      end;
    end
    else
      FDataSet.Check(FDataSet.DSCursor.AddAggregate(GroupingLevel, 0, nil,
        FHAggregate));
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 15:39:57 - Initial code.<br />
}
procedure TDBIAggregate.Assign(Source: TPersistent);
begin
  if Source is TDBIAggregate then
  begin
    DisplayName := TDBIAggregate(Source).Displayname;
    Visible := TDBIAggregate(Source).Visible;
    Expression := TDBIAggregate(Source).Expression;
    IndexName := TDBIAggregate(Source).IndexName;
    GroupingLevel := TDBIAggregate(Source).GroupingLevel;
    Active := TDBIAggregate(Source).Active;
  end
  else if Source is TAggregateField then
  begin
    DisplayName := TAggregateField(Source).DisplayName;
    Visible := TAggregateField(Source).Visible;
    Expression := TAggregateField(Source).Expression;
    IndexName := TAggregateField(Source).IndexName;
    GroupingLevel := TAggregateField(Source).GroupingLevel;
    Active := TAggregateField(Source).Active;
  end
  else
    inherited Assign(Source);
end;


function TDBIAggregate.GetDisplayName: string;
begin
  Result := FAggregateName;
  if Result = '' then Result := Expression;
  if Result = '' then Result := inherited GetDisplayName;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 20:58:55 - Initial code.<br />
}
procedure TDBIAggregate.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if (FDataSet <> nil) and (FDataSet.FDSCursor <> nil) then
    try
{$ifdef _AGGREGATES}
      FDataSet.ResetAgg(Self, True);
{$endif}
    except
      FActive := False;
      raise;
    end;
  end;
end;


procedure TDBIAggregate.SetExpression(const Text: string);
begin
  if ( FDataSet <> nil ) and (FExpression <> Text ) and Active
    and not (csLoading in FDataSet.ComponentState) then
    DatabaseError(SAggActive, FDataSet);
  if Text <> FExpression then
    FOutOfDate := True;
  FExpression := Text;
end;

procedure TDBIAggregate.SetGroupingLevel(GroupingLevel: Integer);
begin
  if ( FDataSet <> nil ) and (GroupingLevel <> FGroupingLevel ) and Active
    and not (csLoading in FDataSet.ComponentState) then
    DatabaseError(SAggActive, FDataSet);
  if GroupingLevel <> FGroupingLevel then
    FOutOfDate := True;
  FGroupingLevel := GroupingLevel;
end;

procedure TDBIAggregate.SetIndexName(Value: String);
begin
  if ( FDataSet <> nil ) and (FIndexName <> Value) and Active
    and not (csLoading in FDataSet.ComponentState) then
    DatabaseError(SAggActive, FDataSet);
  FIndexName := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/10/2008 21:04:53 - Initial code.<br />
}
function TDBIAggregate.Value: Variant;
var
  Blank: LongBool;
begin
  Result := Null;
  if InUse and Active and Assigned(FDataSet) then
  begin
    FDataSet.UpdateCursorPos;
    FDataSet.DSCursor.GetAggregateValue(FHAggregate, Pointer(FDataBuffer), Blank);
    if Blank then
      Result := NULL
    else
      Result := AggValueAsVariant(Self, FDataBuffer);
  end;
end;

{$endif}





{ TConnectionBroker }

{$ifdef _CONNECTIONBROKER}

constructor TConnectionBroker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RCS;
end;

function TConnectionBroker.GetServer: IAppServer;
begin
  if Connection <> nil then
    Result := Connection.GetServer
  else
    Result := nil;
end;

function TConnectionBroker.GetAppServer: Variant;
begin
  if not Assigned(Connection) then
    raise Exception.CreateRes(@SNoParentConnection);
  Result := Connection.GetServer;    
end;

function TConnectionBroker.GetConnected: Boolean; 
begin
  Result := inherited GetConnected;
  if Assigned(Connection) then
    Result := Connection.Connected;
end;

procedure TConnectionBroker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FConnection) then
    Connection:= nil;
end;

procedure TConnectionBroker.Loaded;
begin
  inherited Loaded;
  if StreamedConnected then
    SetConnected(True);
end;

procedure TConnectionBroker.SetConnected(Value: Boolean);
begin
  if csLoading in ComponentState then
  begin
    FStreamedConnected := True;
    exit;
  end;

  if Value and (Connection = nil) then
    raise Exception.Create(SConnectionMissing);

  FStreamedConnected := False;
  inherited SetConnected(Value);
  if Assigned(Connection) then
    Connection.Connected := Value;
end;

procedure TConnectionBroker.SetConnection(
  const Value: TCustomRemoteServer);
begin
  if FConnection <> Value then
  begin
    if Value = Self then
      raise Exception.Create(SNoCircularConnection)
    else
      if Assigned(Value) and (Value is TConnectionBroker) then
        raise Exception.Create(SNoConnectToBroker);
    FConnection := Value;
  end;
end;

{$endif}




{ =wip }


// =============================================================================
// Methods unique to TDBIDataset which do NOT exist in TClientDataset.
// =============================================================================

(*##JVR
// _____________________________________________________________________________
{**
  Jvr - 15/04/2002 10:52:53.<P>
}
function TDBIDataset.GetLocked: Boolean;
begin
  Result := False;

  if Assigned(FDSBase) then begin
    Check(FDSBase.GetProp(basepropFULLLOCK, PDBIPropValue(@Result)));
  end;
end;  { GetLocked }


// _____________________________________________________________________________
{**
  Jvr - 15/04/2002 10:54:15.<P>
}
procedure TDBIDataset.SetLocked(const Value: Boolean);
begin
  CheckActive;

  if (State <> dsBrowse) then begin
    DatabaseError(SNotBrowsing, Self);
  end;

  if Assigned(FDSBase) then begin
    Check(FDSBase.SetProp(basepropFULLLOCK, TDBIPropValue(Value)));
  end;
end;  { SetLocked }
*)

// _____________________________________________________________________________
{**
  This method is specific to TDBIDataset and does NOT exist in TClientDataset.

  Jvr - 10/08/2004 13:51:20 - Initial code.<p>
}
procedure TDBIDataset.SetDataConnection(Value: TDBIDataConnection);
begin
  FDataConnection := Value;
end;


// _____________________________________________________________________________
{**
  This method is specific to TDBIDataset and does NOT exist in TClientDataset.

  Jvr - 31/10/2000 12:31:22<P>
}
function TDBIDataset.GetExclusive: Boolean;
begin
  Result := FExclusive;
end;


// _____________________________________________________________________________
{**
  This method is specific to TDBIDataset and does NOT exist in TClientDataset.

  Jvr - 31/10/2000 12:31:15<P>
}
procedure TDBIDataset.SetExclusive(const Value: Boolean);
begin
  FExclusive := Value;

  if Assigned(FDSBase) then begin
    Check(FDSBase.SetProp(basepropEXCLUSIVE_ACCESS, TDBIPropValue(Value)));
  end;
end;


// _____________________________________________________________________________
{**
  This method is specific to TDBIDataset and does NOT exist in TClientDataset.
  It is present because of DBMode. If set to cmFields then the dataset may not
  be edited.  This may change in the future. Editing the fields structure would
  be useful.

  Jvr - 26/10/2000 14:01:50<P>
}
function TDBIDataset.GetReadOnly: Boolean;
begin
  Result := FReadOnly or (FDataConnection.Mode = cmFields);
end;


// _____________________________________________________________________________
{**
  Returns the datasets cursor curent physical record number.

  Because of Delphi's internal record buffering, we must read the stored
  record identity.

  This method is unique to TDBIDataset and does NOT exist in TClientDataset.<P>

  @param  Result  Current record number

  Jvr - 17/09/2001 17:25:55 - Initial code<BR>
  Jvr - 08/10/2002 10:25:40 - Method name change (was GetRecID)<P>
}
function TDBIDataset.GetRecordNumber: Integer;
var
  BufPtr: TDBIRecordBuffer;

begin
  CheckActive;

  if State = dsInternalCalc then begin
    Result := -1
  end
  else begin
    if (State = dsCalcFields) then begin
      raise EDBIException.Create(Self, 'GetRecordNumber::7605', 'Not Tested for (State = dsCalcFields)', []);
//##JVR      BufPtr := GetCalcBuffer;
    end
    else begin
      BufPtr := GetActiveBuffer;
    end;

    Result := PRecInfo(TDBIRecordBuffer(BufPtr) + FRecInfoOfs)^.RecordIdent;
  end;  { if }
end;


// _____________________________________________________________________________
{**
  Sets the dataset cursor to the specified physical record number.

  This method is unique to TDBIDataset and does NOT exits in TClientDataset.<P>

  Jvr - 17/09/2001 17:35:19 - Initial code<P>
  Jvr - 08/10/2002 10:26:39 - Method name change (was SetRecID)<P>
}
procedure TDBIDataset.SetRecordNumber(Value: Integer);
begin
  CheckIsInitialised('SetRecId::7620');
  CheckBrowseMode;
  if (Value <> RecordNumber) then begin
    if (Value <= GetRecordCount) then begin
      DoBeforeScroll;
      Check(FDSCursor.MoveToRecNo(Value));
      Resync([rmCenter]);
      DoAfterScroll;
    end
    else begin
      raise EDBIException.Create(Self, 'SetRecId::7635',
        'Attempt to MoveCursor to RecNo [%d] where there are only [%d] records',
        [Value, GetRecordCount]
        );
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 02/05/2001 15:20:30 - Now that filters are supportted we need to change
                              indicate this for scrolling.<P>

  <B>Notes</B><BR>
  TDBClientDataset does a better job of their filtering & RecordCount and
  therefore IsSequenced() always returns True.
}
function TDBIDataset.IsSequenced: Boolean;
begin
  Result := {##JVR not Filtered; //}True; //(FRecNoStatus = rnParadox) and (not Filtered);
end;


// _____________________________________________________________________________
{**
  Load a new Dataset from an existing Dataset.

  Jvr - 25/05/2001 13:36:36 - No longer using GetCurrentRecord() because the
                              implementation of different vendors can't be
                              relied upon (including Borland) [pretty sad!]<BR>
  Jvr - 13/06/2001 13:15:51 - Added Field mapping to take care of different
                              order of fields in source and destination datasets.
                              Also deals with unequal number of fields and/or
                              fields not present in the source or destination
                              dataset.<BR>
  Glk - 04/10/2001 15:22:08 - Added functionanlity to allow converting from
                              integers to booleans.<P>
}
type
  PDateTimeRec = ^TDateTimeRec;
  PBoolean = ^Boolean;

procedure TDBIDataset.LoadFromDataset(
  ADataset: TDataset;
  LoadModes: TDBILoadModes = [lmDisableSourceEvents]
  );
var
  RecordBuffer: TDBIRecordBuffer;
  DisabledState: Boolean;
  LogicalBufferSize: Integer;
  FieldMap: TDBIFieldMap;

  procedure LoadBufferFromFields(Buffer: TDBIRecordBuffer);
  var
    FieldNo: Integer;
    DataField: TField;
    DataSize: Integer;
    AnsiData: AnsiString;
    WideData: WideString;
    TargetFieldNo: Integer;
    FieldOffset: Integer;
    NullOffset: Integer;
    IsBlank: Boolean;
    BlobStream: TDBIBlobStream;

  begin
    for FieldNo := 0 to FieldMap.FieldCount - 1 do begin
      DataField := FieldMap.SourceFields[FieldNo];
///left off here Assert(DataField.DataType = FieldMap.TargetFields[FieldNo].DataType);
      TargetFieldNo := FieldMap.TargetFields[FieldNo].FieldNo - 1;
      FieldOffset := FDataConnection.FieldProps[TargetFieldNo].iFldOffsInRec;
      IsBlank := DataField.IsNull;

      try
        // If the field is not null then process it
        if not IsBlank then begin
          case FDataConnection.FieldProps[TargetFieldNo].iFldType of
            fldWIDESTRING, fldUNICODE: begin
              // Make sure that the Data written to the buffer doesn't overrun
              WideData := WideString(DataField.AsString);
              DataSize := Length(WideData);
              if DataSize > FieldMap.TargetFields[FieldNo].Size then
                DataSize := FieldMap.TargetFields[FieldNo].Size;

              Move(PWideChar(WideData)^, Buffer[FieldOffset], 2 * DataSize);
              IsBlank := IsBlank or (DataSize = 0);
            end;

            fldZSTRING: begin
              // Make sure that the Data written to the buffer doesn't overrun
              AnsiData := AnsiString(DataField.AsString);
              DataSize := Length(AnsiData);
              if DataSize > FieldMap.TargetFields[FieldNo].Size then
                DataSize := FieldMap.TargetFields[FieldNo].Size;

              Move(PAnsiChar(AnsiData)^, Buffer[FieldOffset], DataSize);
              IsBlank := IsBlank or (DataSize = 0);
            end;

            fldBOOL: begin
              case DataField.DataType of
                ftBoolean: begin
                  PBoolean(@Buffer[FieldOffset])^ := DataField.AsBoolean;
                end;

                ftString, ftFixedChar, ftWideString, ftMemo: begin
                  DataSize := Length(DataField.AsString);
                  if (DataSize = 1) then
                    PBoolean(@Buffer[FieldOffset])^ :=
                      (TDBIString(DataField.AsString)[1] in ['y', 'Y', 't', 'T'])
                  else if (DataSize > 1) then
                    PBoolean(@Buffer[FieldOffset])^ :=
                      (CompareText(DataField.AsString, 'True') = 0) or
                      (CompareText(DataField.AsString, 'Yes') = 0)
                  else
                    PBoolean(@Buffer[FieldOffset])^ := False;
                end;

              else
                PBoolean(@Buffer[FieldOffset])^ := DataField.AsInteger <> 0;
              end;  { case }
 {##JVR
              if (DataField.DataType <> ftBoolean) then begin
                PBoolean(@Buffer[FieldOffset])^ := DataField.AsInteger <> 0;
              end
              else begin
                PBoolean(@Buffer[FieldOffset])^ := DataField.AsBoolean;
              end;
 //}
            end;

            fldINT8: PShortInt(@Buffer[FieldOffset])^ := DataField.AsInteger;
            fldUINT8: PByte(@Buffer[FieldOffset])^ := Byte(DataField.AsInteger);
            fldINT16: PSmallInt(@Buffer[FieldOffset])^ := DataField.AsInteger;
            fldUINT16: PWord(@Buffer[FieldOffset])^ := Word(DataField.AsInteger);
            fldINT32: PInteger(@Buffer[FieldOffset])^ := DataField.AsInteger;
            fldUINT32: PLongWord(@Buffer[FieldOffset])^ := LongWord(DataField.AsInteger);

            fldINT64, fldUINT64: begin
              if (DataField.Datatype = ftLargeint) then begin
                PInt64(@Buffer[FieldOffset])^ :=
                  (DataField as TLargeIntField).AsLargeInt;
              end
              else begin
                PInt64(@Buffer[FieldOffset])^ := DataField.AsInteger;
              end;
            end;  { fldINT64 }

            fldFLOAT: begin
              PDouble(@Buffer[FieldOffset])^ := DataField.AsFloat;
            end;

            fldSINGLE: begin
              PSingle(@Buffer[FieldOffset])^ := DataField.AsFloat;
            end;

{$ifdef DELPHIXE3}
            fldFLOATIEEE: begin
              if DataField is TExtendedField then begin
                PExtended(@Buffer[FieldOffset])^ := (DataField as TExtendedField).AsExtended;
              end
              else begin
                PExtended(@Buffer[FieldOffset])^ := DataField.AsFloat;
              end;
            end;
{$endif}

            fldDATE: begin
              PDateTimeRec(@Buffer[FieldOffset])^.Date :=
                DateTimeToTimeStamp(DataField.AsDateTime).Date;

  //##JVR            IsBlank := IsBlank or (DataField.AsDateTime = 0);
            end;  { fldDATE }

            fldTIMESTAMP: begin
              PDateTimeRec(@Buffer[FieldOffset])^.DateTime :=
                TimeStampToMSecs(DateTimeToTimeStamp(DataField.AsDateTime));

  //##JVR            IsBlank := IsBlank or (DataField.AsDateTime = 0);
            end;  { fldTIMESTAMP }
  {
            fldBCD: begin
              FloatValue := DataField.AsFloat;
              Move(FloatValue, Buffer[FieldOffset], DataField.DataSize);
            end;
  }
            fldBLOB: begin
              { TODO 3 -oJvr -cTDBIDataSet.LoadFromDataset() :
                Binary Blob fields are not fully supported yet, the data may need
                to be saved to a memory stream. The data can then be Move()...ed
                into a PAnsiChar buffer instead of using a 'String' buffer.
              }
              IsBlank := IsBlank or ((DataField as TBlobField).BlobSize = 0);
              if not IsBlank then begin
                AnsiData := AnsiString(DataField.AsString);
                BlobStream := TDBIBlobStream.Create(
                  Self.Fields[TargetFieldNo] as TBlobField,
                  bmRead, //##JVR bmWrite,
                  RecordBuffer
                  );
                try
                  // This writes the data to the Blob Stream
                  BlobStream.Write{##JVR Buffer}(
                    Pointer(AnsiData)^,
                    (DataField as TBlobField).BlobSize
                    );
                finally
                  // This writes the blob to file.
                  BlobStream.Free;
                end;
              end;  { if }
            end;  { fldBLOB }

            else begin
              raise EDBIException.Create(Self, 'LoadBufferFromFields::7850',
                'Field type "%d: not Supported',
                [FDataConnection.FieldProps[TargetFieldNo].iFldType]
                );
            end;  { Default }

          end; { case }
        end;  { if not DataField.IsNull }

        { DONE 5 -oJvr -cTDBIDataSet.LoadFromDataset() : NullFlags }
        // Set the Null-Flag for this field to the 'IsBlank' value
        // If iNullOffsInRec is Zero or less then Nulls are not supported
        NullOffset := FDataConnection.FieldProps[TargetFieldNo].iNullOffsInRec;
        if (NullOffset > 0) then begin
          Byte(TDBIRecordBuffer(RecordBuffer)[NullOffset]) := Ord(IsBlank);
        end;
      except
        on E: Exception do begin
          raise EDBIException.Create(Self, E, 'LoadBufferFromFields::7870',
            'Failed to Convert Field[%0:d] "%1:s" of type "%2:s" ' +
            'to Field[%3d] "%4:s" of type "%5:s"', [
            FieldMap.SourceFields[FieldNo].FieldNo,
            FieldMap.SourceFields[FieldNo].FieldName,
            GetEnumName(TypeInfo(TFieldType), Ord(FieldMap.SourceFields[FieldNo].DataType)),
            FieldMap.TargetFields[FieldNo].FieldNo,
            FieldMap.TargetFields[FieldNo].FieldName,
            GetEnumName(TypeInfo(TFieldType), Ord(FieldMap.TargetFields[FieldNo].DataType))
            ]);
        end;
      end;  { try..except }
    end;  { for }
  end;  { LoadBufferFromFields }


begin
  if not ADataset.Active then begin
    ADataset.Open;
  end;

  // Only create dataset if requested in the LoadMode parameter
  if (lmCreateDataset in LoadModes) then begin
    // Create the Physical Dataset
    FieldDefs.Assign(ADataset.FieldDefs);
    CreateDataSet;
  end

  else if not Active then begin
    Open;
  end;


  // Create temporary record buffer
  LogicalBufferSize := GetRecordSize;
  RecordBuffer := AllocMem(LogicalBufferSize);
  DisabledState := ADataset.ControlsDisabled;
  try
    if (lmDisableSourceEvents in LoadModes) and not DisabledState then begin
      ADataset.DisableControls;
    end;

    try
      FieldMap := TDBIFieldMap.Create(ADataset, Self);
      try
        ADataset.First;
        while not ADataset.Eof do begin
          // Initialise Recordbuffer for next record
          FDSCursor.InitRecord(RecordBuffer);

          // Get record from source
          LoadBufferFromFields(TDBIRecordBuffer(RecordBuffer));

          FDSCursor.InsertRecord(RecordBuffer);
          ADataset.Next;
        end;  { while }
      finally
        FieldMap.Free;
      end;  { try..finally }

    except
      on E: Exception do
        raise EDBIException.Create(Self, E, 'LoadFromDataset::7930',
          'Failed to insert Record "%d"', [ADataset.RecNo]
          );
    end;  { try..except }

  finally
    if (lmDisableSourceEvents in LoadModes) and not DisabledState then begin
      ADataset.EnableControls;
    end;

    FreeMem(RecordBuffer);

    Refresh;
  end;  { try..finally }
end;  { LoadFromDataset }


// _____________________________________________________________________________
{**
  ..<P>
}
procedure TDBIDataset.CheckIsInitialised(const Context: String);
begin
  if (not Assigned(FDSBase)) or (not Assigned(FDSCursor)) then begin
    raise EDBIException.Create(Self, Context,
      'Database Interface/Cursor has not been properly initialised.', []
      );
  end;
end;  { CheckInitialised }


// _____________________________________________________________________________
{**
  SetActive will sometimes call CreateDataset instead of calling the inherited
  SetActive.
  The conditions for this is as follows:<BR>
  <UL>
    <LI>The Value parameter needs to be True
    <LI>The Value parameter should differ from the current state
    <LI>The internal options indicate that this dataset was created, not opened.
    <LI>The internal options indicate that SetActive is begin called by Loaded()
    <LI>The ComponentState indicates that we are NOT reading from the DFM
    <LI>The internal options indicate that we are NOT already in CreateDataset
  </UL>

  Otherwise the inherited SetActive is called at all times.<P>
  SetActive works closely together with the Loaded() method like TDBIDataset's
  ancestor TDataset.<P>

  <B>The internal options property:</B><BR>
  <UL>
    <LI>The TDBIDataset.Loaded method controls the ioLoading option.
    <LI>The TDBIDataset.SetActive method controls the ioCreating option.
    <LI>The TDBIDataset.CreateDatset and TDBIDataset.SetActive methods control
        the ioCreateDataset option.
  </UL>

  Jvr - 27/05/2002 17:45:36.<P>
}
procedure TDBIDataset.SetActive(Value: Boolean);
var
  ValidState: Boolean;

begin
  if {(Active <> Value) and} not Value then begin
    Exclude(FInternalOptions, ioCreateDataset);
  end;
{##JVR
  ValidState :=
    Value and                                // Open Dataset
    (Active <> Value) and                    // Value is not current state
    (ioCreateDataset in FInternalOptions) and// The Dataset should be Created
    (ioLoading in FInternalOptions) and      // SetActive is called from Loaded
    (not (csReading in ComponentState)) and  // SetActive is not called from DFM
    (not (ioCreating in FInternalOptions));  // Not already doing CreateDataset
//}
  ValidState := Value;
  ValidState := ValidState and (Active <> Value);
  ValidState := ValidState and (ioCreateDataset in FInternalOptions);
  ValidState := ValidState and (ioLoading in FInternalOptions);
  ValidState := ValidState and (not (csReading in ComponentState));
  ValidState := ValidState and (not (ioCreating in FInternalOptions));

  if ValidState then begin
    Include(FInternalOptions, ioCreating);
    try
      CreateDataset;
    finally
      Exclude(FInternalOptions, ioCreating);
    end;
  end
  else begin
    inherited SetActive(Value);
  end;

end;


// _____________________________________________________________________________
{**
  Jvr - 15/04/2002 10:52:53.<P>
}
function TDBIDataset.Locked(LockType: TDBILockType; const Value: Integer = 0): Boolean;
var
  LockData: TDBILockData;

begin
  CheckActive;

  LockData.iType := LockType;
  LockData.iResource := Value;
  Check(FDSCursor.GetDataProp(cursorpropRESOURCELOCK, PDBIPropData(@LockData)));
  Result := LockData.bLocked;
end;  { Locked }


// _____________________________________________________________________________
{**
  Locks a TDBIDataSet

  Call Lock to lock a TDBIDataSet to prevent other applications from placing a
  particular type of lock on the Dataset.<P>

  Lock clears all remaining Record Locks for all Cursors.<P>

  Jvr - 15/04/2002 10:54:15.<BR>
  Jvr - 21/08/2002 16:13:33 - If value is not specified for a ltRecordLock then
                              lock the current record.<P>
}
function TDBIDataset.Lock(
  LockType: TDBILockType;
  const Value: Integer = 0;
  Options: TDBILockOptions = [loExceptionOnFail]
  ): Boolean;
var
  LockData: TDBILockData;
  ReturnValue: DBIResult;

begin
  CheckActive;

  if (State <> dsBrowse) then begin
    DatabaseError(SNotBrowsing, Self);
  end;

  if (LockType = ltRecordLock) and (Value = 0) then begin
    LockData.iResource := RecordNumber;
  end
  else begin
    LockData.iResource := Value;
  end;

  LockData.iType := LockType;
  LockData.bLocked := True;

  ReturnValue := FDSCursor.SetDataProp(cursorpropRESOURCELOCK, PDBIPropData(@LockData));
  Result := ReturnValue = DBIERR_NONE;
  if (loExceptionOnFail in Options) then begin
    Check(ReturnValue);
  end;
end;  { Lock }


// _____________________________________________________________________________
{**
  Removes a previously applied lock on a TDBIDataset

  Call Unlock to remove a lock previously applied to a TDBIDataset. <P>

  Unlock clears all remaining Record Locks for all Cursors.<P>

  Jvr - 05/06/2002 14:59:55.<BR>
  Jvr - 21/08/2002 16:14:43 - If value is not specified for a ltRecordLock then
                              unlock the current record.<P>
}
procedure TDBIDataset.Unlock(LockType: TDBILockType; const Value: Integer = 0);
var
  LockData: TDBILockData;

begin
  CheckActive;

  if (State <> dsBrowse) then begin
    DatabaseError(SNotBrowsing, Self);
  end;

  if (LockType = ltRecordLock) and (Value = 0) then begin
    LockData.iResource := RecordNumber;
  end
  else begin
    LockData.iResource := Value;
  end;

  LockData.iType := LockType;
  LockData.bLocked := False;
  Check(FDSCursor.SetDataProp(cursorpropRESOURCELOCK, PDBIPropData(@LockData)));
end;  { Unlock }






{ TDBICustomDataset }

function TDBICustomDataset.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  Result := inherited GetFieldClass(FieldType);

  case FieldType of
    db.ftBytes:    Result := TDBIBytesField;
{$ifdef DelphiXE3}
    db.ftExtended: Result := TDBIExtendedField;
{$endif}
  end;
end;  { GetFieldClass }


{$ifdef fpc}
constructor TDBICustomDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FieldDefs.FPOAttachObserver(Self);
end;


destructor TDBICustomDataset.Destroy;
begin
  FieldDefs.FPODetachObserver(Self);

  inherited Destroy;
end;

procedure TDBICustomDataset.CheckFieldCompatibility(Field: TField; FieldDef: TFieldDef);
begin
  raise EDBINotImplementedException.Create(Self, 'CheckFieldCompatibility::8130');
end;


procedure TDBICustomDataset.CreateFields;
var
  I: Integer;
begin
{$ifdef _UNUSED}
  if ObjectView then
  begin
{$endif}
    for I := 0 to FieldDefs.Count - 1 do
      with FieldDefs[I] do
        if (DataType <> ftUnknown) and
          not ((faHiddenCol in Attributes) and not FIeldDefs.HiddenFields) then
          CreateField(Self);
{$ifdef _UNUSED}
  end else
  begin
    for I := 0 to FieldDefList.Count - 1 do
      with FieldDefList[I] do
        if (DataType <> ftUnknown) and not (DataType in ObjectFieldTypes) and
          not ((faHiddenCol in Attributes) and not FIeldDefs.HiddenFields) then
          CreateField(Self, nil, FieldDefList.Strings[I]);
  end;
{$endif}
end;

procedure TDBICustomDataset.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if (Operation = ooChange) then begin
    DefChanged(Data);
  end;
end;


function TDBICustomDataset.GetFieldData(FieldNo: Integer;
  Buffer: TDBIValueBuffer): Boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;


function TDBICustomDataset.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
var
  SaveState: TDataSetState;
begin
  if Field.FieldKind in [fkData, fkInternalCalc] then
  begin
    SaveState := Self.State;
    SetTempState(State);
    try
      Result := Field.AsVariant;
    finally
      RestoreState(SaveState);
    end;
  end else
    Result := NULL;
end;


procedure TDBICustomDataset.OpenParentDataSet(ParentDataSet: TDBICustomDataset);
begin
  if not ParentDataSet.IsCursorOpen then
  begin
    { Temporarily set the our State to dsOpening to prevent recursive calls to
      Open by TDataSetField.Bind }
    SetTempState(dsOpening);     //##JVR FState := dsOpening;
    try
      ParentDataSet.Open;
    finally
      RestoreState(dsInActive); //##JVR FState := dsInActive;
    end;
  end;
  if ParentDataSet.State <> dsInsert then
    ParentDataSet.UpdateCursorPos;
end;


procedure TDBICustomDataset.SetDataSetField(const Value: TDataSetField);
begin
  raise EDBINotImplementedException.Create(Self, 'SetDatasetField::8345');
end;
{$endif}





{ TDBIExtendedField }

{$ifdef DelphiXE3}
function TDBIExtendedField.GetAsExtended: Extended;
var
  Data: TValueBuffer;
begin
  SetLength(Data, SizeOf(Extended));
  if not GetData(Data) then
    Result := 0
  else
  {$ifdef DelphiXE8}
    Result := TBitConverter.InTo<Extended>(Data);
  {$else}
    Result := TBitConverter.ToExtended(Data);
  {$endif}
end;
{$endif}




{ TDBIBytesField }

function TDBIBytesField.GetAsString: String;
var
  Bytes: array[0..255] of Byte;
  Index: Word;
  Size: Word;

begin
  Result := '';
  Size := GetDataSize;
  Assert(Size <= 255);
  FillChar(Bytes{%H-}, 255, #0);
{$warnings off}
  GetData(@Bytes);
{$warnings on}
  for Index := 0 to Size-1 do begin
    if Bytes[Index] > 0 then begin
      Result := Result + IntToHex(Bytes[Index], 2);
    end;
  end;
end;


procedure TDBIBytesField.GetText(var Text: string; DisplayText: Boolean);
begin
  if DisplayText and (EditMaskPtr <> '') then begin
    Text := FormatMaskText(EditMaskPtr, GetAsString);
  end
  else begin
    Text := GetAsString;
  end;
end;


end.
