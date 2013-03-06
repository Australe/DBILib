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
  1.0 | 07/01/2002 16:07:40 | Jvr | Initial Release
  ______________________________________________________________________________
}

unit DBIStringsDataConnections;

interface

uses
  Classes, SysUtils, DBIConst, DBIInterfaces, DBIIntfConsts, DBICustomListDataConnections;

type
  TDBIStringsDataConnection = class(TDBICustomListDataConnection)
  private
    FOwnsObjects: Boolean;
    FListMode: TDBIListDataConnectionListMode;
    FStrings: TStrings;

  protected
    procedure ClearList(AListMode: TDBIListDataConnectionListMode);
    function GetOwnsObject: Boolean;

    procedure SetStrings(Value: TStrings);
    procedure SetOwnsObjects(const Value: Boolean);

    function GetItem(Index: Integer): TObject; override;
    procedure SetItem(Index: Integer; const Value: TObject); override;
    function GetRecordCount(const StatusFilter: DSAttr): Integer; override;

  public
    constructor Create(AOwner: TObject); override;
    destructor Destroy; override;

    function AddItem(Item: TObject): Integer; override;
    procedure DeleteItem(const Index: Integer); override;
    function IndexOfItem(Item: TObject): Integer; override;
    function RemoveItem(Item: TObject): Integer; override;

    property List: TStrings read FStrings write SetStrings;
    property OwnsObjects: Boolean read GetOwnsObject write SetOwnsObjects;

  end;  { TDBIStringsDataConnection }


implementation


{ TDBIStringsDataConnection }

// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 16:17:41<P>
}
constructor TDBIStringsDataConnection.Create(AOwner: TObject);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create;
  FListMode := lmInternal;
  FOwnsObjects := True;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 09/11/2000 16:19:27<P>
}
destructor TDBIStringsDataConnection.Destroy;
begin
  ClearList(lmNone);

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 14:06:44.<P>
}
function TDBIStringsDataConnection.AddItem(Item: TObject): Integer;
begin
  { TODO 5 -oJvr -cTDBIStringsDataConnection.AddItem() :
    We need to initialise the Ordinal properties
    to the default values specified in the class definition
  }
  Result := FStrings.AddObject('', Item);
end;  { AddItem }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:18:18.<P>
}
procedure TDBIStringsDataConnection.DeleteItem(const Index: Integer);
begin
  Assert(Assigned(FStrings));

  // Free object referenced by Index in list if OwnsObject property is True
  if FOwnsObjects then begin
    FStrings.Objects[Index].Free;
    FStrings.Objects[Index] := nil;
  end;

  FStrings.Delete(Index);
end;  { DeleteItem }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 14:36:44.<P>
}
function TDBIStringsDataConnection.IndexOfItem(Item: TObject): Integer;
begin
  Assert(Assigned(FStrings));

  Result := FStrings.IndexOfObject(Item);
end;  { IndexOfItem }


// _____________________________________________________________________________
{**
  Jvr - 29/01/2003 15:30:52.<P>
}
function TDBIStringsDataConnection.RemoveItem(Item: TObject): Integer;
begin
  Result := FStrings.IndexOfObject(Item);
  Assert(Result > -1, 'Object not found');

  FStrings.Delete(Result);
end;  { RemoveItem }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:09:26.<P>
}
procedure TDBIStringsDataConnection.ClearList(
  AListMode: TDBIListDataConnectionListMode
  );
var
  Index: Integer;

begin
  if Assigned(FStrings) and (FListMode = lmInternal) then begin
    // Free objects in list if OwnsObject property is True
    if FOwnsObjects then begin
      for Index := FStrings.Count -1 downto 0 do begin
        FStrings.Objects[Index].Free;
        FStrings.Objects[Index] := nil;
      end;
    end;

    FStrings.Free;
    FStrings := nil;
  end;

  FListMode := AListMode;
end;  { ClearList }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:20:38.<P>
}
function TDBIStringsDataConnection.GetItem(Index: Integer): TObject;
begin
  Assert(Assigned(FStrings));

  Result := FStrings.Objects[Index];
end;  { GetItem }


// _____________________________________________________________________________
{**
  Jvr - 08/11/2000 13:46:15<P>
}
function TDBIStringsDataConnection.GetRecordCount(
  const StatusFilter: DSAttr
  ): Integer;
begin
  Assert(Assigned(FStrings));

  Result := FStrings.Count;
end;  { GetRecordCount }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:19:09.<P>
}
function TDBIStringsDataConnection.GetOwnsObject: Boolean;
begin
  Result := FOwnsObjects;
end;  { GetOwnsObject }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:19:39.<P>
}
procedure TDBIStringsDataConnection.SetItem(Index: Integer; const Value: TObject);
begin
  Assert(Assigned(FStrings));

  FStrings.Objects[Index] := Value;
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
procedure TDBIStringsDataConnection.SetStrings(Value: TStrings);
begin
  if (Value <> nil) then begin
    ClearList(lmExternal);
    FStrings := Value;
  end
  else begin
    ClearList(lmNone);
  end;  { if }

  NotifyDataEventCallBacks(
    LongWord(Value),
    dbiBasePropChanged,
    Pointer(basepropDATASETCHANGED)
    );
end;  { SetStrings }


// _____________________________________________________________________________
{**
  Jvr - 07/01/2002 16:20:12.<P>
}
procedure TDBIStringsDataConnection.SetOwnsObjects(const Value: Boolean);
begin
  FOwnsObjects := Value;
end;  { SetOwnsObjects }


end.

