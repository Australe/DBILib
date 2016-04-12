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
  1.0 | 07/01/2002 13:35:03 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIObjectListDataConnections;

{$I DBICompilers.inc}

interface

uses
  Classes, SysUtils, Contnrs, DBIConst, DBIInterfaces, DBIIntfConsts,
  DBICustomListDataConnections;

type
  TDBIObjectListDataConnection = class(TDBICustomListDataConnection)
  private
    FListMode: TDBIListDataConnectionListMode;
    FObjectList: TObjectList;

  protected
    procedure ClearList(AListMode: TDBIListDataConnectionListMode);
    function GetOwnsObject: Boolean;

    procedure SetObjectList(Value: TObjectList);
    procedure SetOwnsObjects(const Value: Boolean);

    function GetItem(Index: Integer): TObject; override;
    procedure SetItem(Index: Integer; const Value: TObject); override;
    function GetRecordCount(const StatusFilter: DSAttr): Integer; override;

  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    procedure Reset; override;
    function AddItem(Item: TObject): Integer; override;
    procedure DeleteItem(const Index: Integer); override;
    function IndexOfItem(Item: TObject): Integer; override;
    function RemoveItem(Item: TObject): Integer; override;

    property List: TObjectList read FObjectList write SetObjectList;
    property OwnsObjects: Boolean read GetOwnsObject write SetOwnsObjects;

  end;  { TDBIObjectListDataConnection }


implementation

{$ifdef Delphi2009}
uses
  Types;
{$endif}


{ TDBIObjectListDataConnection }

// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 16:17:41<P>
}
constructor TDBIObjectListDataConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FObjectList := TObjectList.Create;
  FListMode := lmInternal;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 16:19:27<P>
}
destructor TDBIObjectListDataConnection.Destroy;
begin
  ClearList(lmNone);

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 25/03/2009 08:50:15 - Initial code.<br>

  Remove all data from the list
}
procedure TDBIObjectListDataConnection.Reset;
begin
  inherited Reset;

  if Assigned(FObjectList) then FObjectList.Clear;
end;  { Reset }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 14:06:44.<P>
}
function TDBIObjectListDataConnection.AddItem(Item: TObject): Integer;
begin
  { TODO 5 -oJvr -cTDBIObjectListDataConnection.AddItem() :
    We need to initialise the Ordinal properties
    to the default values specified in the class definition
  }
  Result := FObjectlist.Add(Item);
end;  { AddItem }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 15:03:28.<P>
}
procedure TDBIObjectListDataConnection.DeleteItem(const Index: Integer);
begin
  Assert(Assigned(FObjectList));

  FObjectList.Delete(Index);
end;  { DeleteItem }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 14:34:05.<P>
}
function TDBIObjectListDataConnection.IndexOfItem(Item: TObject): Integer;
begin
  Assert(Assigned(FObjectList));

  Result := FObjectList.IndexOf(Item);
end;  { IndexOfItem }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 15:29:26.<P>
}
function TDBIObjectListDataConnection.RemoveItem(Item: TObject): Integer;
begin
  Result := FObjectlist.Remove(Item);
end;  { RemoveItem }


// _____________________________________________________________________________
{**
  Jvr - 11/12/2000 16:59:00<P>
}
procedure TDBIObjectListDataConnection.ClearList(
  AListMode: TDBIListDataConnectionListMode
  );
begin
  Assert(Assigned(FObjectList));

  if (FListMode = lmInternal) then begin
    if (AListMode <> lmInternal) then begin
      FObjectList.Free;
      FObjectList := nil;
    end;
  end
  else if (FListMode = lmExternal) and (AListMode = lmInternal) then begin
    FObjectList := TObjectList.Create;
  end;  { if }

  FListMode := AListMode;
end;  { ClearList }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:42:00<P>
}
function TDBIObjectListDataConnection.GetItem(Index: Integer): TObject;
begin
  Assert(Assigned(FObjectList));

  Result := FObjectList.Items[Index];
end;  { GetItem }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:46:15<P>
}
function TDBIObjectListDataConnection.GetRecordCount(
  const StatusFilter: DSAttr
  ): Integer;
begin
  Assert(Assigned(FObjectList));

  Result := FObjectList.Count;
end;  { GetRecordCount }


// _____________________________________________________________________________
{**
  Jvr - 10/11/2000 13:34:55<P>
}
function TDBIObjectListDataConnection.GetOwnsObject: Boolean;
begin
  Assert(Assigned(FObjectList));

  Result := FObjectList.OwnsObjects;
end;  { GetOwnsObject }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:34:11<P>
}
procedure TDBIObjectListDataConnection.SetItem(Index: Integer; const Value: TObject);
begin
  Assert(Assigned(FObjectList));

  FObjectList.Items[Index] := Value;
end;  { SetItem }


// _____________________________________________________________________________
{**
  Jvr - 11/12/2000 17:06:00.<BR>
  Jvr - 09/10/2001 12:45:40 - Added NotifyDataEventCallBacks routine to indicate
                              that the List has changed. Possibly using the enum
                              'dbiBasePropChanged' is incorrect and a new enum,
                              to indicate that the underlying physical dataset
                              has changed, should be created.<P>
}
procedure TDBIObjectListDataConnection.SetObjectList(Value: TObjectList);
begin
  if (Value = nil) then begin
    ClearList(lmInternal);
  end
  else begin
    ClearList(lmExternal);
    FObjectList := Value;
  end;  { if }

  NotifyDataEventCallBacks(
    LongWord(VALUE),
    dbiBasePropChanged,
    TDBIRecBuf(basepropDATASETCHANGED)
    );
end;  { SetObjectList }


// _____________________________________________________________________________
{**
  Jvr - 10/11/2000 13:35:12<P>
}
procedure TDBIObjectListDataConnection.SetOwnsObjects(const Value: Boolean);
begin
  Assert(Assigned(FObjectList));

  FObjectList.OwnsObjects := Value;
end;  { SetOwnsObjects }


end.

