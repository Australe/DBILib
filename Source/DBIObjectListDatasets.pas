// _____________________________________________________________________________
{**
  <H5>Copyright</H5> 
  Better Innovative Technical Services Pty Ltd<BR>
  Copyright (C) 2000, All rights reserved.<!--

  Project:       DBILib
  Files(s):      DBIObjectListDatasets.pas
  Classes:       TObjectListDataset, TDBICustomObjectListDataset
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 08/11/2000 10:53:46 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

unit DBIObjectListDatasets;

{$I DBICompilers.inc}

interface

uses
  Classes, DB, Contnrs, DBIDataset, DBIConst, DBIIntfConsts, DBIStrings,
  DBIInterfaces, DBICustomListDataConnections;

type
  TDBICustomListDataset = class(TDBIDataSet)
  protected
    procedure OOPS(
      const Caller: String;
      const Reference: String;
      const Msg: String;
      Args: array of const
      );

  protected
    function GetClassTypeName: String;
    function GetMode: TDBIDataConnectionMode;
    function GetObjectValidationProc: TDBIMethodName;
    function GetOnCreateObject: TDBIOnCreateObjectEvent;
    function GetOptions: TDBIListDataConnectionOptions;
//##RECORDCOUNT    function GetRecordCount: Integer; override;
    function GetStringFieldSize: Integer;

    procedure SetClassTypeName(const Value: String); virtual;
    procedure SetMode(const Value: TDBIDataConnectionMode);
    procedure SetObjectValidationProc(const Value: TDBIMethodName);
    procedure SetOnCreateObject(const Value: TDBIOnCreateObjectEvent);
    procedure SetOptions(const Value: TDBIListDataConnectionOptions);
    procedure SetStringFieldSize(const Value: Integer);

    procedure InitFieldDefsFromMetaData; virtual;

    property ClassTypeName: String
      read GetClassTypeName write SetClassTypeName;

    property Mode: TDBIDataConnectionMode
      read GetMode write SetMode default cmData;

    property Options: TDBIListDataConnectionOptions
      read GetOptions write SetOptions default osDefaultListDataConnectionOptions;

    property StringFieldSize: Integer
      read GetStringFieldSize write SetStringFieldSize default Default_StringFieldLength;

    property ObjectValidationProc: TDBIMethodName
      read GetObjectValidationProc write SetObjectValidationProc;

    property OnCreateObject: TDBIOnCreateObjectEvent
      read GetOnCreateObject write SetOnCreateObject;


  public
    function AppendObject(DataObject: TObject): Integer;
    function RemoveObject(DataObject: TObject): Integer;
    function UpdateObject(DataObject: TObject): Integer;
//##JVR    procedure CreateDataset; override;

    procedure NotifyDataEvent(
      DataObject: TObject;
      Event: TDBIDataChangeEventType;
      ItemIndex: Integer = -1
      );

  end;  { TDBICustomListDataset }


  TDBICustomObjectListDataset = class(TDBICustomListDataset)
  private
    FDataSource: TDataSource;
    function GetDataSouce: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    
  protected
    function GetData: TObject;
    function GetMode: TDBIDataConnectionMode;
    function GetList: TObjectList;
    procedure SetMode(const Value: TDBIDataConnectionMode);
    procedure SetList(Value: TObjectList);

    property Mode: TDBIDataConnectionMode read GetMode write SetMode default cmData;

  public
    constructor Create(AOwner: TComponent); override;

    property List: TObjectList read GetList write SetList;
    property Data: TObject read GetData;
    property SourceDataSource: TDataSource read GetDataSouce write SetDataSource;
  end;  { TDBICustomObjectListDataset }


  TObjectListDataset = class(TDBICustomObjectListDataset)
  published
    property Active;
    property ClassTypeName;
    property DataSetField;
    property Mode;              //** cmData or cmFields
    property ObjectValidationProc;
    property ObjectView default True;
    property ReadOnly;
    property StringFieldSize;


    // Wip
    property MasterFields;
    property MasterSource;

//##JVR    property Filter;
    property Filtered;
    property FilterOptions;
    property Options;


    // Events
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

    // Added to CustomListDataset
    property OnCreateObject;
  end;  { TObjectListDataset }


  TDBICustomStringsDataset = class(TDBICustomListDataset)
  private
    function GetOwnsObjects: Boolean;
  protected
    function GetList: TStrings;
    procedure SetList(Value: TStrings);
    procedure SetOwnsObjects(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;

    property List: TStrings read GetList write SetList;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;

  end;  { TDBICustomStringsDataset }


  TStringsDataset = class(TDBICustomStringsDataset)
  published
    property Active;
    property ClassTypeName;
    property DataSetField;
    property Mode;
    property ObjectValidationProc;
    property ObjectView default True;
    property ReadOnly;
    property StringFieldSize;


    // Wip
//##JVR    property Filter;
    property Filtered;
    property FilterOptions;
    property Options;

    // Events
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

    // Added to CustomListDataset
    property OnCreateObject;
  end;  { TStringsDataset }



implementation

uses
{$IFDEF DELPHI6}
  RtlConsts,
{$ENDIF}
  Consts,
  SysUtils,
  TypInfo,
  DBIUtils,
  DBIXmlUtils,
  DBIObjectListDataConnections,
  DBIStringsDataConnections;

const
  Unitname = 'DBIObjectListDatasets';


// =============================================================================
// 'TDBICustomListDataset' protected methods
// =============================================================================

procedure TDBICustomListDataset.OOPS(
  const Caller: String;
  const Reference: String;
  const Msg: String;
  Args: array of const
  );
begin
  raise Exception.CreateFmt(
    'OOPS::' +
    UnitName + '::' +
    Self.ClassName + '::' +
    Caller + '::' +
    Reference + '::' +
    Msg,
    Args
    );
end;


// _____________________________________________________________________________
{**
  Jvr - 28/05/2001 14:03:34.<P>
}
procedure TDBICustomListDataset.InitFieldDefsFromMetaData;
const
  Caller = 'InitFieldDefsFromMetaData';

  procedure GetPropertyList(ClassInfo: PTypeInfo; List: TList);
  var
    Index, PropCount: Integer;
     PropList: PPropList;

  begin
    if not Assigned(ClassInfo) then begin
      raise Exception.Create(
        'No Runtime class information available for this class "' +
        '!'#13 +
        'Make sure your class is derived from TPersistent or that {$M+} is used.'#13 +
        'Watch out for forward declarations!'
      );
    end;


    PropCount := GetTypeData(ClassInfo)^.PropCount;
    if PropCount > 0 then begin
      GetMem(PropList, PropCount * SizeOf(Pointer));
      try
        GetPropInfos(ClassInfo, PropList);
        List.Count := PropCount;
        for Index := 0 to Pred(PropCount) do begin
          List.Items[Index] := PropList[Index];
        end;
      finally
        FreeMem(PropList);
      end;
    end;
  end;  { GetPropertyList }


  function GetClassType(const ClassName: String): TClass;
  begin
    Result := GetClass(ClassName);

    // This section may need to be expanded to accommodate more standard class types
    if (Result = nil) then begin
      if SameText(ClassName, TObjectList.ClassName) then begin
        Result := TObjectList;
      end
      else if SameText(ClassName, TList.ClassName) then begin
        Result := TList;
      end;
    end;  { if }

    if (Result = nil) then begin
      Error(nil, Caller+'::GetClassType', '235', SClassNotFound, [ClassName]);
    end;
  end;  { GetClassType }


  procedure CreateFieldDefs(DataClass: TClass; AFieldDefs: TFieldDefs);
  var
    PropertyList: TList;
    PropInfo: PPropInfo;
    PropClass: TClass;
    Index: Integer;
    FieldDef: TFieldDef;

  begin
    PropertyList := TList.Create;
    try
      GetPropertyList(DataClass.ClassInfo, PropertyList);
      for Index := 0 to PropertyList.Count - 1 do begin
        PropInfo := PropertyList[Index];

        FieldDef := AFieldDefs.AddFieldDef;
        FieldDef.Name := String(PropInfo.Name);
//##JVR        FieldDef.DataType := Field.DataType;
//##JVR        FieldDef.Size := Field.Size;

        { TODO 1 -oJvr -cTDBICustomListDataset.InitFieldDefsFromMetaData() :
          CreateFieldDefs
          Property (Field) is required, Not sure how this would be supported!!!

        if Field.Required then begin
          FieldDef.Attributes := [faRequired];
        end;
        }
        // If Property (Field) is readonly
        if not Assigned(PropInfo.SetProc) then begin
          FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];
        end;

        { TODO 1 -oJvr -cTDBICustomListDataset.InitFieldDefsFromMetaData() :
          CreateFieldDefs
          Not actively supporting BCD

        if (Field.DataType = ftBCD) and (Field is TBCDField) then begin
          FieldDef.Precision := TBCDField(Field).Precision;
        end;
        }

        { TODO 1 -oJvr -cTDBICustomListDataset.InitFieldDefsFromMetaData() :
          CreateFieldDefs
          No support for sub-objects currently available

        if Field is TObjectField then begin
          CreateFieldDefs(TObjectField(Field).Fields, FieldDef.ChildDefs);
        end;
        }

        // Create FieldDef items base on the property type
        case PropInfo^.PropType^.Kind of
          tkClass: begin
            // Get the property class type (it must be registered)
//##JVR            PropClass := GetClassType(PropInfo^.PropType^.Name);
            PropClass := GetTypeData(PropInfo^.PropType^).ClassType;
            Assert(PropClass <> nil);

            // It's a 'TObjectList' or derived from it
            if PropClass.InheritsFrom(TObjectList) then begin
              FieldDef.DataType := ftDataSet;
              FieldDef.Size := 0;  // Possible candidate for sub recordcount

              { TODO 3 -oJvr -cTDBICustomListDataset.InitFieldDefsFromMetaData() :
                CreateFieldDefs
                Instead of using RegisterClassAlias we need to write our own
                registration mechanism to deal with list properties.
              }
              // Create the Childefs for the nested class
              CreateFieldDefs(GetClass(String(PropInfo.Name)), FieldDef.ChildDefs);
            end

            // It's derived from 'TStrings'
            else if PropClass.InheritsFrom(TStrings) then begin
              FieldDef.DataType := ftMemo;
              FieldDef.Size := 0;
            end

            // Otherwise it's some other object - use 'Abstract Data Type'
            else begin
              FieldDef.DataType := ftADT;
              FieldDef.Size := 0;

              // Create the Childefs for the nested class
              CreateFieldDefs(PropClass, FieldDef.ChildDefs);
            end;
          end;  { tkClass }

          tkString, tkWString, tkLString: begin
            FieldDef.DataType := ftString;
            FieldDef.Size := StringFieldSize;
          end;  { tkString, tkWString, tkLString }

          tkInteger: begin
            FieldDef.DataType := ftInteger;
          end;  { tkInteger }

          tkInt64: begin
            FieldDef.DataType := ftLargeInt;
          end;  { tkInt64 }

          tkFloat: begin
            if DBICompareText(PropInfo^.PropType^.Name, 'TDateTime') = 0 then begin
              FieldDef.DataType := ftDateTime;
            end
            else begin
              FieldDef.DataType := ftFloat;
              FieldDef.Precision := 4;
            end;
          end;  { tkFloat }

          tkChar: begin
            FieldDef.DataType := ftString;
            FieldDef.Size := 1;
          end;  { tkChar }

          tkSet, tkEnumeration: begin
            if DBICompareText(PropInfo^.PropType^.Name, 'Boolean') = 0 then begin
              FieldDef.DataType := ftBoolean;
            end
            else begin
              FieldDef.DataType := ftInteger;
            end;
          end;  { tkSet, tkEnumeration }

        else
{
          tkUnknown,
          tkInteger,
          tkChar,
          tkEnumeration,
          tkFloat,
          tkString,
          tkSet,
          tkClass,
          tkMethod,
          tkWChar,
          tkLString,
          tkWString,
          tkVariant,
          tkArray,
          tkRecord,
          tkInterface
}
          Error(nil, Caller, '290', 'Unsupported data type', []);
        end;  { case }
      end;  { for }
    finally
      PropertyList.Free;
    end;  { try..finally }
  end;  { CreateFieldDefs }

begin
  // Create FieldDefs from persistent fields if needed
  if (FieldDefs.Count = 0) then begin
//##JVR    Inc(FieldDefs.FInternalUpdateCount);

    FieldDefs.BeginUpdate;
    try
      CreateFieldDefs(GetClass(ClassTypeName), FieldDefs);
    finally
      FieldDefs.EndUpdate;
//##JVR      Dec(FieldDefs.FInternalUpdateCount);
    end;
  end;
end;  { InitFieldDefsFromMetaData }

(*
// _____________________________________________________________________________
{**
  Jvr - 28/05/2001 14:02:28.<P>
}
procedure TDBICustomListDataset.CreateDataset;
begin
  InitFieldDefsFromMetaData;

  inherited CreateDataset;
end;
*)

// =============================================================================
// 'TDBICustomListDataset' Public Methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 15:07:56.<P>
}
function TDBICustomListDataset.AppendObject(DataObject: TObject): Integer;
var
  ListConnection: TDBICustomListDataConnection;

begin
  DisableControls;
  try
    ListConnection := DataConnection as TDBICustomListDataConnection;
    Result := ListConnection.AddItem(DataObject);
    ListConnection.NotifyDataEvent(DataObject, dbiRecordInserted, Result);

    Resync([]);
  finally
    EnableControls;
  end;
end;  { AppendObject }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 15:24:34.<P>
}
function TDBICustomListDataset.RemoveObject(DataObject: TObject): Integer;
var
  ListConnection: TDBICustomListDataConnection;

begin
  DisableControls;
  try
    ListConnection := DataConnection as TDBICustomListDataConnection;
    ListConnection.NotifyDataEvent(DataObject, dbiRecordDeleted, -1);
    Result := ListConnection.RemoveItem(DataObject);

    Resync([]);
  finally
    EnableControls;
  end;
end;  { RemoveObject }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 15:35:10.<P>
}
function TDBICustomListDataset.UpdateObject(DataObject: TObject): Integer;
var
  ListConnection: TDBICustomListDataConnection;

begin
  DisableControls;
  try
    ListConnection := DataConnection as TDBICustomListDataConnection;
    Result := ListConnection.IndexOfItem(DataObject);
    ListConnection.NotifyDataEvent(DataObject, dbiRecordModified, Result);

    Resync([]);
  finally
    EnableControls;
  end;
end;  { UpdateObject }


// _____________________________________________________________________________
{**
  Jvr - 28/01/2003 16:17:01.<P>
}
procedure TDBICustomListDataset.NotifyDataEvent(
  DataObject: TObject;
  Event: TDBIDataChangeEventType;
  ItemIndex: Integer = -1
  );
const
  Caller = 'NotifyDataset';

begin
  CheckActive;

  if (Event = dbiBasePropChanged) then begin
    Error(nil, Caller, '685',
      'Illegal Event parameter, valid Events are %s, %s & %s',
      ['dbiRecordInserted', 'dbiRecordModified', 'dbiRecordDeleted']
      );
  end;

  DisableControls;
  try
    (DataConnection as TDBICustomListDataConnection).NotifyDataEvent(
      DataObject,
      Event,
      ItemIndex
      );

    Resync([]);
  finally
    EnableControls;
  end;
end;  { NotifyDataEvent }



// =============================================================================
// 'TDBICustomListDataset' Protected Methods
// =============================================================================

function TDBICustomListDataset.GetClassTypeName: String;
begin
  Result := (DataConnection as TDBICustomListDataConnection).ClassTypeName;
end;


function TDBICustomListDataset.GetMode: TDBIDataConnectionMode;
begin
  Result := DataConnection.Mode;
end;  { GetMode }


// _____________________________________________________________________________
{**
  Jvr - 03/10/2002 16:58:10.<P>
}
function TDBICustomListDataset.GetObjectValidationProc: TDBIMethodName;
begin
  Result := (DataConnection as TDBICustomListDataConnection).ObjectValidationProc;
end;  { GetObjectValidationProc }


// _____________________________________________________________________________
{**
  Jvr - 15/08/2002 16:09:15.<P>
}
function TDBICustomListDataset.GetOnCreateObject: TDBIOnCreateObjectEvent;
begin
  Result := (DataConnection as TDBICustomListDataConnection).OnCreateObject;
end;  { GetOnCreateObject }


// _____________________________________________________________________________
{**
  Mbs - 07/11/2002 10:35:15.<P>

function TDBICustomListDataset.GetOnCreateValidationObject: TDBIOnCreateObjectEvent;
begin
  Result := (DataConnection as TDBICustomListDataConnection).OnCreateValidationObject;
end; } { GetOnCreateValidationObject }


// _____________________________________________________________________________
{**
  Jvr - 18/10/2001 11:06:59.<P>
}
function TDBICustomListDataset.GetOptions: TDBIListDataConnectionOptions;
begin
  Result := (DataConnection as TDBICustomListDataConnection).Options;
end;  { GetOptions }

(*##GETRECORDCOUNT
// _____________________________________________________________________________
{**
  Jvr - 08/03/2002 16:26:01.<P>
}
function TDBICustomListDataset.GetRecordCount: Integer;
begin
  if Filtered then begin
    Result := inherited GetRecordCount;
  end
  else begin
    Result := (DataConnection as TDBICustomListDataConnection).GetCount(dsRecActive);
  end;
end;  { GetRecordCount }
//*)

// _____________________________________________________________________________
{**
  Jvr - 15/08/2002 16:12:51.<P>
}
function TDBICustomListDataset.GetStringFieldSize: Integer;
begin
  Result := (DataConnection as TDBICustomListDataConnection).StringFieldSize;
end;  { GetStringFieldSize }


// _____________________________________________________________________________
{**
  Jvr - 27/11/2000 16:40:35<P>
}
procedure TDBICustomListDataset.SetClassTypeName(const Value: String);
begin
  CheckInActive;

  // Raise exception if class is not registered (runtime only)
  if not (csDesigning in ComponentState) and (Value <> '') then begin
//##JVR  if (Value <> '') then begin
    Classes.FindClass(Value);
  end;

  (DataConnection as TDBICustomListDataConnection).ClassTypeName := Value;
end;  { SetClassTypeName }


procedure TDBICustomListDataset.SetMode(const Value: TDBIDataConnectionMode);
begin
  DataConnection.Mode := Value;
end;  { SetMode }


// _____________________________________________________________________________
{**
  Jvr - 03/10/2002 16:59:32.<P>
}
procedure TDBICustomListDataset.SetObjectValidationProc(const Value: TDBIMethodName);
begin
  (DataConnection as TDBICustomListDataConnection).ObjectValidationProc := Value;
end;  { SetObjectValidationProc }


// _____________________________________________________________________________
{**
  Jvr - 15/08/2002 16:09:53.<P>
}
procedure TDBICustomListDataset.SetOnCreateObject(
  const Value: TDBIOnCreateObjectEvent
  );
begin
  (DataConnection as TDBICustomListDataConnection).OnCreateObject := Value;
end;  { SetOnCreateObject }


// _____________________________________________________________________________
{**
  Mbs - 07/11/2002 10:35:53.<P>

procedure TDBICustomListDataset.SetOnCreateValidationObject(
  const Value: TDBIOnCreateObjectEvent
  );
begin
  (DataConnection as TDBICustomListDataConnection).OnCreateValidationObject := Value;
end; } { SetOnCreateValidationObject }


// _____________________________________________________________________________
{**
  Jvr - 18/10/2001 11:07:16.<P>
}
procedure TDBICustomListDataset.SetOptions(
  const Value: TDBIListDataConnectionOptions
  );
begin
  CheckInActive;

  (DataConnection as TDBICustomListDataConnection).Options := Value;
end;  { SetOptions }


// _____________________________________________________________________________
{**
  Jvr - 15/08/2002 16:11:58.<P>
}
procedure TDBICustomListDataset.SetStringFieldSize(const Value: Integer);
begin
  (DataConnection as TDBICustomListDataConnection).StringFieldSize := Value;
end;  { SetStringFieldSize }





// =============================================================================
// 'TDBICustomObjectListDataset' methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 17:35:34<P>
}
constructor TDBICustomObjectListDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDataConnection(TDBIObjectListDataConnection.Create(Self));
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 08/12/2004 15:42:16 - Initial code.<p>
}
function TDBICustomObjectListDataset.GetData: TObject;
var
  RecInfo: PRecInfo;

begin
  Result := nil;
  if GetActiveRecInfo(RecInfo) then begin
    Result := RecInfo^.Data;
  end;
end;  { GetData }


// _____________________________________________________________________________
{**
  Jvr - 01/03/2002 13:42:33.<P>
}
function TDBICustomObjectListDataset.GetMode: TDBIDataConnectionMode;
begin
  Result := DataConnection.Mode;
end;  { GetMode }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 15:35:02.<P>
}
function TDBICustomObjectListDataset.GetList: TObjectList;
begin
  Result := (DataConnection as TDBIObjectListDataConnection).List;
end;  { GetList }


// _____________________________________________________________________________
{**
  Jvr - 01/03/2002 13:43:51.<P>
}
procedure TDBICustomObjectListDataset.SetMode(const Value: TDBIDataConnectionMode);
begin
  CheckInActive;

  DataConnection.Mode := Value;
end;  { SetMode }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 15:36:00.<P>
}
procedure TDBICustomObjectListDataset.SetList(Value: TObjectList);
begin
  (DataConnection as TDBIObjectListDataConnection).List := Value;

//##JVR
  try
    if (State = dsBrowse) then begin
ClearBuffers;
      UpdateCursorPos;
      Resync([]);
    end;
  except
  end;

//##JVR  NotifyCallback;
end;  { SetList }







// =============================================================================
// 'TDBICustomStringsDataset' methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 17:35:34<P>
}
constructor TDBICustomStringsDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDataConnection(TDBIStringsDataConnection.Create(Self));
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 15:35:02.<P>
}
function TDBICustomStringsDataset.GetList: TStrings;
begin
  Result := (DataConnection as TDBIStringsDataConnection).List;
end;  { GetList }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:40:00.<P>
}
function TDBICustomStringsDataset.GetOwnsObjects: Boolean;
begin
  Result := (DataConnection as TDBIStringsDataConnection).OwnsObjects;
end;  { GetOwnsObjects }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 15:36:00.<P>
}
procedure TDBICustomStringsDataset.SetList(Value: TStrings);
begin
  (DataConnection as TDBIStringsDataConnection).List := Value;

  NotifyCallback;
end;  { SetList }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:37:32.<P>
}
procedure TDBICustomStringsDataset.SetOwnsObjects(const Value: Boolean);
begin
  (DataConnection as TDBIStringsDataConnection).OwnsObjects := Value;
end;  { SetOwnsObjects }


// _____________________________________________________________________________
{**
  Jvr - 29/12/2003 13:54:48 - Initial code.<p>
}
function TDBICustomObjectListDataset.GetDataSouce: TDataSource;
begin
  Result := FDataSource;
end;


// _____________________________________________________________________________
{**
  Jvr - 29/12/2003 13:55:30 - Initial code.<p>
}
procedure TDBICustomObjectListDataset.SetDataSource(const Value: TDataSource);
var
  Dataset: TDBIDataset;

begin
  FDataSource := Value;
  Dataset := Value.Dataset as TDBIDataset;

  CloneCursor(Dataset, False, False);
end;




end.

