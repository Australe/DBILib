// _____________________________________________________________________________
{**
  <H5>Copyright</H5>
  ObjectMastery Pty Ltd<BR>
  Copyright (C) 2002, All rights reserved.

  Project:       DBIcl
  Files(s):      DBIPropertyEditors
  Classes:       N/A
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 08/10/2002 18:08:31 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

unit DBIPropertyEditors;

{$I DBICompilers.inc}

interface

uses
  FldLinks, DBIObjectListDatasets;

type
  TODSFieldLinkProperty = class(TFieldLinkProperty)
  private
    FODS: TObjectlistDataSet;

  protected
    function GetIndexFieldNames: String; override;
    function GetMasterFields: String; override;
    procedure SetIndexFieldNames(const Value: String); override;
    procedure SetMasterFields(const Value: String); override;

  public
    procedure Edit; override;

  end;


procedure Register;


implementation

uses
{$ifdef DELPHI6}
  DesignIntf,
{$else}
  DsgnIntf,
{$endif}  
  dbReg;

  
// =============================================================================
// 'TODSFieldLinkProperty' methods
// =============================================================================

// _____________________________________________________________________________
{**
  Jvr - 08/10/2002 18:12:25.<P>
}
procedure TODSFieldLinkProperty.Edit;
var
  Opened: Boolean;
//##JVR  PacketRecords: Integer;

begin
  FODS := DataSet as TObjectListDataset;
//##JVR  PacketRecords := FODS.PacketRecords;
  Opened := FODS.FieldCount = 0;
  try
    if Opened then begin
//##JVR      FODS.PacketRecords := 0;
      FODS.Open;
    end;

    inherited Edit;

  finally
    if Opened then begin
      FODS.Close;
//##JVR      FODS.PacketRecords := PacketRecords;
    end;
  end;
end;  { Edit }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2002 18:14:04.<P>
}
function TODSFieldLinkProperty.GetIndexFieldNames: String;
begin
  Result := FODS.IndexFieldNames;
end;  { GetIndexFieldNames }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2002 18:14:15.<P>
}
function TODSFieldLinkProperty.GetMasterFields: String;
begin
  Result := FODS.MasterFields;
end;  { GetMasterFields }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2002 18:14:57.<P>
}
procedure TODSFieldLinkProperty.SetIndexFieldNames(const Value: string);
begin
  FODS.IndexFieldNames := Value;
end;  { SetIndexFieldNames }


// _____________________________________________________________________________
{**
  Jvr - 08/10/2002 18:15:30.<P>
}
procedure TODSFieldLinkProperty.SetMasterFields(const Value: string);
begin
  FODS.MasterFields := Value;
end;  { SetMasterFields }





// _____________________________________________________________________________
{**
  Jvr - 08/10/2002 18:09:54.<P>
}
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TObjectListDataSet, 'IndexName', TIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TObjectListDataSet, 'MasterFields', TODSFieldLinkProperty);
end;

end.
