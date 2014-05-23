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
  1.0 | 08/11/2000 10:53:46 | Jvr | Initial Release
  ______________________________________________________________________________
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

    property MasterFields;
    property MasterSource;

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



  TDBICustomCollectionDataset = class(TDBICustomListDataset)
  protected
    function GetCollection: TCollection;
    function GetData: TObject;
    procedure SetCollection(Value: TCollection);

  public
    constructor Create(AOwner: TComponent); override;

    property Collection: TCollection read GetCollection write SetCollection;
    property Data: TObject read GetData;

  end;  { TDBICustomCollectionDataset }


  TCollectionDataset = class(TDBICustomCollectionDataset)
  published
    property Active;
    property Mode;
    property ObjectValidationProc;
    property ObjectView default True;
    property ReadOnly;
    property StringFieldSize;

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
  end;  { TCollectionDataset }


  TDBICustomComponentDataset = class(TDBICustomListDataset)
  protected
    function GetComponent: TComponent;
    function GetData: TObject;
    procedure SetComponent(Value: TComponent);

  public
    constructor Create(AOwner: TComponent); override;

    property Component: TComponent read GetComponent write SetComponent;
    property Data: TObject read GetData;

  end;  { TDBICustomComponentDataset }


  TComponentDataset = class(TDBICustomComponentDataset)
  published
    property Active;
    property ClassTypeName;
    property Mode;
    property ObjectValidationProc;
    property ObjectView default True;
    property ReadOnly;
    property StringFieldSize;

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
  end;  { TComponentDataset }


implementation

uses
{$ifdef DELPHI6}
  RtlConsts,
{$endif}
{$ifndef fpc}
  Consts,
{$endif}
  SysUtils,
  TypInfo,
  DBIUtils,
  DBIComponentDataConnections,
  DBICollectionDataConnections,
  DBIObjectListDataConnections,
  DBIStringsDataConnections;



{ TDBICustomListDataset }

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





{ TDBICustomObjectListDataset }

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





{ TDBICustomStringsDataset }

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





{ TDBICustomCollectionDataset }

// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 17:35:34<P>
}
constructor TDBICustomCollectionDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDataConnection(TDBICollectionDataConnection.Create(Self));
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 15:35:02.<P>
}
function TDBICustomCollectionDataset.GetCollection: TCollection;
begin
  Result := (DataConnection as TDBICollectionDataConnection).Collection;
end;  { GetCollection }


// _____________________________________________________________________________
{**
  Jvr - 14/10/2009 09:57:52 - Initial code.<br />
}
function TDBICustomCollectionDataset.GetData: TObject;
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
  Jvr - 07/01/2002 15:36:00.<P>
}
procedure TDBICustomCollectionDataset.SetCollection(Value: TCollection);
begin
  (DataConnection as TDBICollectionDataConnection).Collection := Value;

  NotifyCallback;
end;  { SetCollection }





{ TDBICustomComponentDataset }

// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 09:06:37 - Initial code.<br />
}
constructor TDBICustomComponentDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDataConnection(TDBIComponentDataConnection.Create(Self));
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 09:07:02 - Initial code.<br />
}
function TDBICustomComponentDataset.GetComponent: TComponent;
begin
  Result := (DataConnection as TDBIComponentDataConnection).Component;
end;  { GetComponent }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 09:08:16 - Initial code.<br />
}
function TDBICustomComponentDataset.GetData: TObject;
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
  Jvr - 21/07/2010 09:09:29 - Initial code.<br />
}
procedure TDBICustomComponentDataset.SetComponent(Value: TComponent);
begin
  (DataConnection as TDBIComponentDataConnection).Component := Value;

  NotifyCallback;
end;  { SetComponent }




end.

