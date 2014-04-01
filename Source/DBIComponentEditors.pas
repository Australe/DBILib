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
  1.0 | 12/12/2000 18:24:47 | Jvr | Initial Release
  ______________________________________________________________________________
}

unit DBIComponentEditors;

{$I DBICompilers.inc}

interface

uses
  Classes, Forms, Dialogs, DB, DBReg, DBIConst, DBIObjectListDatasets,
  {$ifdef Delphi6} DesignIntf, {$else} DsgnIntf, {$endif} DSDesign;

type
  TObjectListDatasetEditor = class(TDataSetEditor)
  private
    FCanCreate: Boolean;

  protected
    function GetDSDesignerClass: TDSDesignerClass; override;

  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;

  end;  { TObjectListDatasetEditor }


//##JVRfunction EditClientDataSet(ADataSet: TClientDataSet; ADesigner: IDesigner): Boolean;

function LoadFromFile(ADataSet: TDBICustomListDataset): Boolean;
function GetLoadFromFileName: String;
procedure SaveToFile(ADataSet: TDBICustomListDataset; DataFormat: TDBIDataFormat = dfXbase);

procedure Register;


implementation

uses
  SysUtils, Consts, Controls, ComCtrls, DBIDesigners;

type
{
  TdseAction = (
    dseFetchParams,
    dseAssignLocalData,
    dseLoadFromFile,
    dseCreateDataset,
    dseSaveToFile,
    dseClearData
    );
}

  TdseAction = (
    dseLoadFromFile,
    dseCreateDataset,
    dseSaveToFile
    );

const
{
  dseVerbs = array[TdseAction] of String = (
    'Fetch &Params',
    'Assign L&ocal Data...',
    'Load fro&m file...',
    '&Create Dataset',
    'Save to fi&le...',
    '&Clear Data'
    );
}
  dseVerbs: array[TdseAction] of String = (
    'Load fro&m file...',
    '&Create Dataset',
    'Save to fi&le...'
    );


  sXbaseDataFilter = 'Xbase files (*.dbf)|*.dbf|All Files (*.*)|*.*';
  sClientDataFilter = 'Client Dataset (*.cds)|*.cds|All Files (*.*)|*.*';
  sPipeSeperatedDataFilter = 'PSV files (*.psv)|*.dbf|All Files (*.*)|*.*';

{##JVR
function EditClientDataSet(ADataSet: TClientDataSet; ADesigner: IDesigner): Boolean;
begin
  with TClientDataForm.Create(Application) do
  try
    Caption := Format(SClientDataSetEditor, [ADataSet.Owner.Name, DotSep, ADataSet.Name]);
    FDataSet := ADataSet;
    FDesigner := ADesigner;
    Result := Edit;
  finally
    Free;
  end;
end;
}

// _____________________________________________________________________________
{**
  Jvr - 05/09/2002 12:38:00.<P>
}
function GetLoadFromFileName: String;
begin
  with TOpenDialog.Create(nil) do begin
    try
      Title := sOpenFileTitle;
      DefaultExt := '.dbf';
      Filter := sXbaseDataFilter;

      if Execute then begin
        Result := FileName;
      end;

    finally
      Free;
    end;  { try..finally }
  end;  { with }
end;  { GetLoadFromFileName }


// _____________________________________________________________________________
{**
  Jvr - 05/09/2002 12:45:57.<P>
}
function LoadFromFile(ADataSet: TDBICustomListDataset): Boolean;
var
  FileName: String;

begin
  FileName := GetLoadFromFileName;
  Result := FileName <> '';
  if Result then begin
    ADataSet.LoadFromFile(FileName);
  end;
end;  { LoadFromFile }


// _____________________________________________________________________________
{**
  Jvr - 05/09/2002 12:36:06.<P>
}
procedure SaveToFile(
  ADataSet: TDBICustomListDataset;
  DataFormat: TDBIDataFormat = dfXbase
  );
begin
  with TSaveDialog.Create(nil) do begin
    try
      Options := Options + [ofOverwritePrompt];
      if (DataFormat = dfXbase) then begin
        DefaultExt := 'dbf';
        Filter := sXbaseDataFilter;
      end
      else if (DataFormat = dfCDS) then begin
        DefaultExt := 'cds';
        Filter := sClientDataFilter;
      end
      else if (DataFormat = dfPSV) then begin
        DefaultExt := 'psv';
        Filter := sPipeSeperatedDataFilter;
      end
      else begin
        raise EDBIException.Create(
          '"Xbase", "CDS" and "PSV" are the only formats currently supported'
          );
//##JVR        DefaultExt := 'xml';
//##JVR        Filter := SXMLClientDataFilter;
      end;

      if Execute then begin
        ADataSet.SaveToFile(FileName, DataFormat);
      end;
    finally
      Free;
    end;  { try..finally }
  end;  { with }
end;  { SaveToFile }





// =============================================================================
// 'TObjectListDatasetEditor' methods
// =============================================================================

type

{ TODSDesigner }

  TODSDesigner = class(TDSDesigner)
  private
//##JVR    FPacketRecords: Integer;
  public
    procedure BeginUpdateFieldDefs; override;
    procedure EndUpdateFieldDefs; override;
    function SupportsAggregates: Boolean; override;
    function SupportsInternalCalc: Boolean; override;
  end;

procedure TODSDesigner.BeginUpdateFieldDefs;
begin
{##JVR
  FPacketRecords := 0;
  if not DataSet.Active then
  begin
    DataSet.FieldDefs.Updated := False;
    FPacketRecords := (DataSet as TClientDataSet).PacketRecords;
    if FPacketRecords <> 0 then
      (DataSet as TClientDataSet).PacketRecords := 0;
  end;
//}
  inherited BeginUpdateFieldDefs;
end;

procedure TODSDesigner.EndUpdateFieldDefs;
begin
  inherited EndUpdateFieldDefs;
{##JVR
  if FPacketRecords <> 0 then
    (DataSet as TClientDataSet).PacketRecords := FPacketRecords;
//}
end;

function TODSDesigner.SupportsAggregates: Boolean;
begin
  Result := True;
end;

function TODSDesigner.SupportsInternalCalc: Boolean;
begin
  Result := True;
end;

// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 17:15:22 - Initial code.<br />
}
function TObjectListDatasetEditor.GetDSDesignerClass: TDSDesignerClass;
begin
  Result := TODSDesigner;
end;

// _____________________________________________________________________________
{**
  Jvr - 12/12/2000 17:41:59<P>
}
procedure TObjectListDatasetEditor.ExecuteVerb(Index: Integer);
begin
  if (Index <= inherited GetVerbCount - 1) then begin
    TDBIFieldsEditor.ShowFieldsEditor(Designer, TDataSet(Component), GetDSDesignerClass);
  end
  else begin
    Dec(Index, inherited GetVerbCount);

    if (Index > 0{2}) and not FCanCreate then begin
      Inc(Index);
    end;

    case TdseAction(Index) of
(*
      dseFetchParams: begin
        TClientDataSet(Component).FetchParams;
        Designer.Modified;
      end;  { 0 }

      dseAssignLocalData: if EditClientDataSet(TClientDataSet(Component), Designer) then begin
           Designer.Modified;
      end;  { 1 }
*)
      dseLoadFromFile: begin
//##JVR        ShowMessage('Load from file - Not Implemented Yet!');

        if LoadFromFile(TDBICustomListDataset(Component)) then begin
          Designer.Modified;
        end;
      end;  { dseLoadFromFile }

      dseCreateDataset: begin
        TDBICustomListDataset(Component).CreateDataSet;
        Designer.Modified;
      end;  { dseCreateDataset }

      dseSaveToFile: begin
//##JVR        ShowMessage('Save to file - Not Implemented Yet!');

        SaveToFile(TDBICustomListDataset(Component));
      end;  { dseSaveToFile }
(*
      dseClearData: begin
        TClientDataSet(Component).Data := NULL;
        Designer.Modified;
      end;  { 5 }
*)
    else
        ShowMessage('Illegal selection!');
    end;  { case }
  end;  { if }
end;  { ExecuteVerb }


// _____________________________________________________________________________
{**
  Jvr - 12/12/2000 17:42:03<P>
}
function TObjectListDatasetEditor.GetVerb(Index: Integer): string;
begin
  if (Index <= inherited GetVerbCount - 1) then begin
    Result := inherited GetVerb(Index);
  end
  else begin
    Dec(Index, inherited GetVerbCount);

    if (Index > 0{2}) and not FCanCreate then begin
      Inc(Index);
    end;
    Result := dseVerbs[TdseAction(Index)];
(*
    case Index of
      0: Result := SFetchParams;
      1: Result := SClientDSAssignData;
      2: Result := SLoadFromFile;
      3: Result := SCreateDataSet;
      4: Result := SSaveToFile;
      5: Result := SClientDSClearData;
    end;  { case }

    // New
    case Index of
      0: Result := SLoadFromFile;
      1: Result := SCreateDataSet;
      2: Result := SSaveToFile;
    end;  { case }
*)
  end;  { if }
end;  { GetVerb }


// _____________________________________________________________________________
{**
  Jvr - 12/12/2000 17:42:07<P>
}
function TObjectListDatasetEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1{ + 3};
  FCanCreate := False;

  with TDBICustomListDataset(Component) do begin
    // If Dataset is active or has data then don't allow CreateDataset
    if Active {or (DataSize > 0)} then begin
      Inc(Result, 1); //2);
    end;

    FCanCreate := not Active and ((FieldCount > 0) or (FieldDefs.Count > 0));

    if FCanCreate then begin
      Inc(Result, 1);
    end;
  end;  { with }
end;  { GetVerbCount }





procedure Register;
begin
  RegisterComponentEditor(TDBICustomListDataset, TObjectListDatasetEditor);
end;

end.
