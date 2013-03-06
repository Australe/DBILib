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
  1.0 | 27/01/99 10:59:59 | JVR | Initial Release
  ______________________________________________________________________________
}
unit DBIXbaseDatasets;

interface

uses
  Classes, DB, DBIInterfaces, DBIDataSet, DBIXbaseConsts, DBIXbaseDataConnections;

type
  TDBICustomXBaseDataset = class(TDBIDataSet)
  protected
    function GetDate: TDateTime;
    function GetFlags: TDBIXbaseFlags;
    function GetMode: TDBIDataConnectionMode;
    function GetOptions: TDBIXbaseDataConnectionOptions;
    function GetVersion: TDBIXbaseVersion;

    procedure SetDate(const Value: TDateTime);
    procedure SetFlags(const Value: TDBIXbaseFlags);
    procedure SetMode(const Value: TDBIDataConnectionMode);
    procedure SetOptions(const Value: TDBIXbaseDataConnectionOptions);
    procedure SetVersion(const Value: TDBIXbaseVersion);

    function GetStream: TStream;
    procedure SetStream(const Value: TStream);

    property Date: TDateTime
      read GetDate write SetDate;

    property DataStream: TStream
      read GetStream write SetStream;

    property Mode: TDBIDataConnectionMode
      read GetMode write SetMode default cmData;

    property Options: TDBIXbaseDataConnectionOptions
      read GetOptions write SetOptions default [xsStrictHeader];

    property Version: TDBIXbaseVersion
      read GetVersion write SetVersion default xbVisualFoxpro;

    property Flags: TDBIXbaseFlags
      read GetFlags write SetFlags default [];

  public
    constructor Create(AOwner: TComponent); override;

    function GetRecordCount(const StatusFilter: Integer): Integer; overload;

  end;  { TDBICustomXBaseDataset }


  TXBaseDataset = class(TDBICustomXBaseDataset)
  published
    property Active;

    property Date;                //** Header Date
    property Exclusive;           //** Share mode: Exclusive or shared
//##JVR    property FieldDefs;
    property ReadOnly;            //** OpenMode: ReadOnly or ReadWrite

    property DataStream;          //** Assign your own Datastream
    property FileName;            //** Full path to Xbase file
    property Mode;                //** cmData or cmFields
    property Options;             //** xsShowDeleted, xsShowNullFlags, etc
    property Version;             //** xbVisualFoxPro, xbDbase3, etc
    property Flags;               //** xfCompoundIndex, xfExtendedFields, etc

//##JVR    property Filter;
    property Filtered;
    property FilterOptions;

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

  end;  { TXBaseDataset }


implementation

uses
  SysUtils, DBIUtils;


{ TDBICustomXBaseDataset }

// _____________________________________________________________________________
{**
  Jvr - 27/10/2000 17:35:34<P>
}
constructor TDBICustomXBaseDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDataConnection(TDBIXbaseDataConnection.Create(Self));
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 30/04/2002 11:46:46.<P>
}
function TDBICustomXBaseDataset.GetDate: TDateTime;
begin
  if Active then begin
    Result := (DataConnection as TDBIXbaseDataConnection).Date;
  end
  else begin
    Result := Trunc(Now);
  end;
end;  { GetDate }


// _____________________________________________________________________________
{**
  Jvr - 14/02/2003 13:30:41.<P>
}
function TDBICustomXBaseDataset.GetFlags: TDBIXbaseFlags;
begin
  Result := (DataConnection as TDBIXbaseDataConnection).Flags;
end;  { GetFlags }


// _____________________________________________________________________________
{**
  Jvr - 14/02/2003 13:29:01.<P>
}
function TDBICustomXBaseDataset.GetMode: TDBIDataConnectionMode;
begin
  Result := DataConnection.Mode;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/03/2001 17:10:08.<P>
}
function TDBICustomXBaseDataset.GetOptions: TDBIXbaseDataConnectionOptions;
begin
  Result := (DataConnection as TDBIXbaseDataConnection).Options;
end;  { GetOptions }


// _____________________________________________________________________________
{**
  Jvr - 09/07/2002 14:24:32.<P>
}
function TDBICustomXBaseDataset.GetRecordCount(
  const StatusFilter: Integer
  ): Integer;
begin
  if Filtered then begin
    Result := inherited GetRecordCount;
  end
  else begin
    Result := (DataConnection as TDBIXbaseDataConnection).GetCount(StatusFilter);
  end;
end;  { GetRecordCount }


// _____________________________________________________________________________
{**
  Jvr - 08/03/2001 18:29:38.<P>
}
function TDBICustomXBaseDataset.GetVersion: TDBIXbaseVersion;
begin
  if Active then begin
    Result := (DataConnection as TDBIXbaseDataConnection).Version;
  end
  else begin
    Result := xbVisualFoxPro;
  end;
end;  { GetVersion }


// _____________________________________________________________________________
{**
  Jvr - 30/04/2002 11:48:34.<P>
}
procedure TDBICustomXBaseDataset.SetDate(const Value: TDateTime);
begin
  if not (csReading in ComponentState) then begin
    (DataConnection as TDBIXbaseDataConnection).Date := Value;
  end;
end;  { SetDate }


// _____________________________________________________________________________
{**
  Jvr - 14/02/2003 13:31:22.<P>
}
procedure TDBICustomXBaseDataset.SetFlags(const Value: TDBIXbaseFlags);
begin
  CheckInactive;

  if csReading in ComponentState then begin
    (DataConnection as TDBIXbaseDataConnection).Flags := Value;
  end
  else if ((DataConnection as TDBIXbaseDataConnection).Flags <> Value) then begin
    (DataConnection as TDBIXbaseDataConnection).Flags := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;  { SetFlags }


// _____________________________________________________________________________
{**
  Jvr - 14/02/2003 13:27:33.<P>
}
procedure TDBICustomXBaseDataset.SetMode(const Value: TDBIDataConnectionMode);
begin
  CheckInActive;

  DataConnection.Mode := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/03/2001 17:10:41.<P>
}
procedure TDBICustomXBaseDataset.SetOptions(
  const Value: TDBIXbaseDataConnectionOptions
  );
const
  Caller = 'SetOptions';

var
  NewOptions: TDBIXbaseDataConnectionOptions;

begin
  CheckInActive;

  NewOptions := Value;

  if xsShowDeleted in Value then begin
    Exclude(NewOptions, xsShowDeleted);
    Error(nil, Caller, '280', 'Option "xsShowDeleted" not implemented yet!', []);
  end;

  (DataConnection as TDBIXbaseDataConnection).Options := Value;
end;  { SetOptions }


// _____________________________________________________________________________
{**
  Jvr - 08/03/2001 18:31:29.<P>
}
procedure TDBICustomXBaseDataset.SetVersion(const Value: TDBIXbaseVersion);
const
  Caller = 'SetVersion';

begin
  CheckInActive;

  Error(nil, Caller, '210', 'Not implemented yet!', []);
end;  { SetVersion }


// _____________________________________________________________________________
{**
  Jvr - 13/10/2003 12:24:21 - Initial code.<p>
}
function TDBICustomXBaseDataset.GetStream: TStream;
begin
  Result := (DataConnection as TDBIXbaseDataConnection).DataStream;
end;  { GetStream }


// _____________________________________________________________________________
{**
  Jvr - 13/10/2003 12:25:45 - Initial code.<p>
}
procedure TDBICustomXBaseDataset.SetStream(const Value: TStream);
begin
  (DataConnection as TDBIXbaseDataConnection).DataStream := Value;
end;  { SetStream }



end.
