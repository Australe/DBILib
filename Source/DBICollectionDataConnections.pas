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
  1.0 | 19/07/2010 08:08:11 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBICollectionDataConnections;

{$I DBICompilers.inc}

interface

uses
  Classes, SysUtils, DB, DBIConst, DBIInterfaces, DBIIntfConsts,
  DBICustomListDataConnections;

type
  TDBICollectionDataConnection = class(TDBICustomListDataConnection)
  private
    FListMode: TDBIListDataConnectionListMode;
    FCollection: TCollection;

  protected
    procedure ClearList(AListMode: TDBIListDataConnectionListMode);

    function GetClassTypeName: String; override;
    function GetItem(Index: Integer): TObject; override;
    procedure SetItem(Index: Integer; const Item: TObject); override;
    function GetRecordCount(const StatusFilter: DSAttr): Integer; override;

    procedure SetCollection(Value: TCollection);

  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    procedure Reset; override;
    function AddItem(Item: TObject): Integer; override;
    function CreateItem: TObject; override;
    procedure DeleteItem(const Index: Integer); override;
    function IndexOfItem(Item: TObject): Integer; override;
    function RemoveItem(Item: TObject): Integer; override;

    property Collection: TCollection read FCollection write SetCollection;

  end;


implementation


{ TDBICollectionDataConnection }

// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 08:15:24 - Initial code.<br>
}
constructor TDBICollectionDataConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  ClearList(lmNone);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 08:18:32 - Initial code.<br>
}
destructor TDBICollectionDataConnection.Destroy;
begin
  ClearList(lmNone);

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 08:42:01 - Initial code.<br>

  Remove all data from the list
}
procedure TDBICollectionDataConnection.Reset;
begin
  inherited Reset;

  if Assigned(FCollection) then FCollection.Clear;
end;  { Reset }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 08:59:51 - Initial code.<br>
}
function TDBICollectionDataConnection.AddItem(Item: TObject): Integer;
begin
  Assert(Item is TCollectionItem);

  Result := (Item as TCollectionItem).Index;
end;  { AddItem }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:01:02 - Initial code.<br>
}
function TDBICollectionDataConnection.CreateItem: TObject;
begin
  Result := FCollection.Add;
end;  { CreateItem }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:07:44 - Initial code.<br>
}
procedure TDBICollectionDataConnection.DeleteItem(const Index: Integer);
begin
  Assert(Assigned(FCollection));

  FCollection.Delete(Index);
end;  { DeleteItem }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:12:13 - Initial code.<br>
}
function TDBICollectionDataConnection.IndexOfItem(Item: TObject): Integer;
begin
  Assert(Assigned(FCollection));
  Assert(Item is TCollectionItem);

  Result := (Item as TCollectionItem).Index;
end;  { IndexOfItem }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:14:59 - Initial code.<br>
}
function TDBICollectionDataConnection.RemoveItem(Item: TObject): Integer;
begin
  Assert(Assigned(FCollection));
  Assert(Item is TCollectionItem);

  Result := (Item as TCollectionItem).Index;
  (Item as TCollectionItem).Free;
end;  { RemoveItem }



// =============================================================================
// 'TObjectListDataConnection' protected methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:21:23 - Initial code.<br>
}
procedure TDBICollectionDataConnection.ClearList(
  AListMode: TDBIListDataConnectionListMode
  );
begin
  FCollection := nil;
  FListMode := AListMode;
end;  { ClearList }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:26:12 - Initial code.<br>
}
function TDBICollectionDataConnection.GetClassTypeName: String;
begin
  Assert(Assigned(FCollection));

  Result := FCollection.ItemClass.ClassName;
end;  { GetClassTypeName ]


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:28:53 - Initial code.<br>
}
function TDBICollectionDataConnection.GetItem(Index: Integer): TObject;
begin
  Assert(Assigned(FCollection));

  Result := FCollection.Items[Index];
end;  { GetItem }


// _____________________________________________________________________________
{**
  Jvr -19/07/2010 09:31:16 - Initial code.<br>
}
function TDBICollectionDataConnection.GetRecordCount(
  const StatusFilter: DSAttr
  ): Integer;
begin
  Assert(Assigned(FCollection));

  Result := FCollection.Count;
end;  { GetRecordCount }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:33:45 - Initial code.<br>
}
procedure TDBICollectionDataConnection.SetItem(Index: Integer; const Item: TObject);
begin
  Assert(Assigned(FCollection));
  Assert(Item is TCollectionItem);

  FCollection.Items[Index] := Item as TCollectionItem;
end;  { SetItem }


// _____________________________________________________________________________
{**
  Jvr - 19/07/2010 09:36:17 - Added NotifyDataEventCallBacks routine to indicate
                              that the List has changed. Possibly using the enum
                              'dbiBasePropChanged' is incorrect and a new enum,
                              to indicate that the underlying physical dataset
                              has changed, should be created.<P>
}
procedure TDBICollectionDataConnection.SetCollection(Value: TCollection);
begin
  if (Value = nil) then begin
    ClearList(lmNone);
  end
  else begin
    ClearList(lmExternal);
    FCollection := Value;
  end;  { if }

  NotifyDataEventCallBacks(
    LongWord(VALUE),
    dbiBasePropChanged,
    TDBIRecBuf(basepropDATASETCHANGED)
    );
end;  { SetCollectrion }


end.
