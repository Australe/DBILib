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
  1.0 | 21/07/2010 09:23:22 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIComponentDataConnections;

interface

uses
  Classes, SysUtils, DB, DBIConst, DBIInterfaces, DBIIntfConsts,
  DBICustomListDataConnections;

type
  TDBIComponentDataConnection = class(TDBICustomListDataConnection)
  private
    FListMode: TDBIListDataConnectionListMode;
    FComponent: TComponent;

  protected
    procedure ClearList(AListMode: TDBIListDataConnectionListMode);

    function GetClassTypeName: String; override;
    function GetItem(Index: Integer): TObject; override;
    function GetRecordCount(const StatusFilter: DSAttr): Integer; override;

    procedure SetComponent(Value: TComponent);
    procedure SetItem(Index: Integer; const Item: TObject); override;

  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    procedure Reset; override;
    function AddItem(Item: TObject): Integer; override;
    function CreateItem: TObject; override;
    procedure DeleteItem(const Index: Integer); override;
    function IndexOfItem(Item: TObject): Integer; override;
    function RemoveItem(Item: TObject): Integer; override;

    property Component: TComponent read FComponent write SetComponent;

  end;


implementation


{ TDBIComponentDataConnection }

// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 14:06:44.<P>
}
function TDBIComponentDataConnection.AddItem(Item: TObject): Integer;
begin
  Assert(Item is TComponent);

  Result := (Item as TComponent).ComponentIndex;
end;  { AddItem }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 16:17:41<P>
}
constructor TDBIComponentDataConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  ClearList(lmNone);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 16:19:27<P>
}
destructor TDBIComponentDataConnection.Destroy;
begin
  ClearList(lmNone);

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 12:02:19 - Initial code.<br />
}
procedure TDBIComponentDataConnection.ClearList(
  AListMode: TDBIListDataConnectionListMode
  );
begin
  FComponent := nil;
  FListMode := AListMode;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 12:43:52 - Initial code.<br />
}
function TDBIComponentDataConnection.CreateItem: TObject;
var
  ClassRef: TComponentClass;

begin
  Assert(Assigned(FComponent));

  ClassRef := TComponentClass(FindClass(ClassTypeName));
  Result := ClassRef.Create(FComponent);
end;  { CreateItem }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 15:03:28.<P>
}
procedure TDBIComponentDataConnection.DeleteItem(const Index: Integer);
begin
  Assert(Assigned(FComponent));

  FComponent.Components[Index].Free;
end;  { DeleteItem }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 12:04:53 - Initial code.<br />
}
function TDBIComponentDataConnection.GetClassTypeName: String;
begin
  Assert(Assigned(FComponent));

  Result := inherited GetClassTypeName;

  if (Result = '') then begin
    Result := FComponent.ClassName;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 12:01:10 - Initial code.<br />
}
function TDBIComponentDataConnection.GetItem(Index: Integer): TObject;
begin
  Assert(Assigned(FComponent));

  Result := FComponent.Components[Index];
end;


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 12:01:23 - Initial code.<br />
}
function TDBIComponentDataConnection.GetRecordCount(
  const StatusFilter: DSAttr
  ): Integer;
begin
  Assert(Assigned(FComponent));

  Result := FComponent.ComponentCount;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 14:34:05.<P>
}
function TDBIComponentDataConnection.IndexOfItem(Item: TObject): Integer;
begin
  Assert(Assigned(FComponent));
  Assert(Item is TComponent);

  Result := (Item as TComponent).ComponentIndex;
end;  { IndexOfItem }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 15:29:26.<P>
}
function TDBIComponentDataConnection.RemoveItem(Item: TObject): Integer;
begin
  Assert(Assigned(FComponent));
  Assert(Item is TComponent);

  Result := (Item as TComponent).ComponentIndex;
  (Item as TComponent).Free;
end;  { RemoveItem }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 09:50:15 - Initial code.<br>

  Remove all data from the list
}
procedure TDBIComponentDataConnection.Reset;
var
  Index: Integer;

begin
  inherited Reset;

  if Assigned(FComponent) then begin
    for Index := FComponent.ComponentCount-1 downto 0 do begin
      FComponent.Components[Index].Free;
    end;
  end;
end;  { Reset }


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 12:45:40 - Added NotifyDataEventCallBacks routine to indicate
                              that the List has changed. Possibly using the enum
                              'dbiBasePropChanged' is incorrect and a new enum,
                              to indicate that the underlying physical dataset
                              has changed, should be created.<P>
}
procedure TDBIComponentDataConnection.SetComponent(Value: TComponent);
begin
  if (Value = nil) then begin
    ClearList(lmNone);
  end
  else begin
    ClearList(lmExternal);
    FComponent := Value;
  end;  { if }

  NotifyDataEventCallBacks(
    LongWord(Value),
    dbiBasePropChanged,
    Pointer(basepropDATASETCHANGED)
    );
end;


// _____________________________________________________________________________
{**
  Jvr - 21/07/2010 12:02:37 - Initial code.<br />
}
procedure TDBIComponentDataConnection.SetItem(Index: Integer; const Item: TObject);
begin
  raise EDBIException.Create(Self, 'SetItem::255',
    'Not possible to set an item by index in the Components list', []
    );
end;


end.

