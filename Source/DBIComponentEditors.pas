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
  1.0 | 12/12/2000 18:24:47 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIComponentEditors;

{$I DBICompilers.inc}

interface

uses
{$ifdef fpc}
  ComponentEditors, FieldsEditor,
{$else}
  Forms, DB, DBReg,
  {$ifdef Delphi6} DesignIntf, {$else} DsgnIntf, {$endif} DSDesign,
{$endif}
  Classes, Dialogs, DBIConst, DBIObjectListDatasets, DBIDesigners;

{$ifdef fpc}
type
  TDataSetEditor = class(TFieldsComponentEditor)
  end;
{$endif}


type
  TObjectListDatasetEditor = class(TDataSetEditor)
  private
    FCanCreate: Boolean;

  protected
{$ifndef fpc}
    function GetDSDesignerClass: TDSDesignerClass; override;
{$endif}

    class function GetFileName: String;
    class function LoadFromFile(ADataSet: TDBICustomListDataset): Boolean;
    class procedure SaveToFile(ADataSet: TDBICustomListDataset; DataFormat: TDBIDataFormat = dfXbase);

  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;

  end;


procedure Register;


implementation

uses
  SysUtils, Controls;

type
  TdseAction = (
    dseLoadFromFile,
    dseCreateDataset,
    dseSaveToFile
    );

const
  dseVerbs: array[TdseAction] of String = (
    'Load fro&m file...',
    '&Create Dataset',
    'Save to fi&le...'
    );

  SClientDataFilter = 'Client Dataset (*.cds)|*.cds|XML Files (*.xml)|*.xml|All Files (*.*)|*.*';
  SPipeSeperatedDataFilter = 'PSV files (*.psv)|*.psv|All Files (*.*)|*.*';
  SXbaseDataFilter = 'Xbase files (*.dbf)|*.dbf|All Files (*.*)|*.*';



{ TODSDesigner }

{$ifndef fpc}
type
  TODSDesigner = class(TDSDesigner)
  public
    procedure BeginUpdateFieldDefs; override;
    procedure EndUpdateFieldDefs; override;
    function SupportsAggregates: Boolean; override;
    function SupportsInternalCalc: Boolean; override;
  end;

procedure TODSDesigner.BeginUpdateFieldDefs;
begin
(*##JVR
  FPacketRecords := 0;
  if not DataSet.Active then
  begin
    DataSet.FieldDefs.Updated := False;
    FPacketRecords := (DataSet as TDBICustomListDataset).PacketRecords;
    if FPacketRecords <> 0 then
      (DataSet as TDBICustomListDataset).PacketRecords := 0;
  end;
//*)
  inherited BeginUpdateFieldDefs;
end;

procedure TODSDesigner.EndUpdateFieldDefs;
begin
  inherited EndUpdateFieldDefs;
(*##JVR
  if FPacketRecords <> 0 then
    (DataSet as TClientDataSet).PacketRecords := FPacketRecords;
//*)
end;

function TODSDesigner.SupportsAggregates: Boolean;
begin
  Result := True;
end;

function TODSDesigner.SupportsInternalCalc: Boolean;
begin
  Result := True;
end;
{$endif}





{ TObjectListDatasetEditor }

// _____________________________________________________________________________
{**
  Jvr - 12/12/2000 17:41:59<P>
}
procedure TObjectListDatasetEditor.ExecuteVerb(Index: Integer);
begin
  if (Index <= inherited GetVerbCount - 1) then begin
{$ifdef fpc}
    TDBIFieldsEditor.ShowFieldsEditor(Self);
{$else}
    TDBIFieldsEditor.ShowFieldsEditor(Designer, TDataSet(Component), GetDSDesignerClass);
{$endif}
  end
  else begin
    Dec(Index, inherited GetVerbCount);

    if (Index > 0) and not FCanCreate then begin
      Inc(Index);
    end;

    case TdseAction(Index) of
      dseLoadFromFile: begin
        if LoadFromFile(TDBICustomListDataset(Component)) then begin
          Designer.Modified;
        end;
      end;

      dseCreateDataset: begin
        TDBICustomListDataset(Component).CreateDataSet;
        Designer.Modified;
      end;

      dseSaveToFile: begin
        SaveToFile(TDBICustomListDataset(Component));
      end;
    else
      ShowMessage('Illegal selection!');
    end;
  end;
end;  { ExecuteVerb }


{$ifndef fpc}
// _____________________________________________________________________________
{**
  Jvr - 11/10/2008 17:15:22 - Initial code.<br />
}
function TObjectListDatasetEditor.GetDSDesignerClass: TDSDesignerClass;
begin
  Result := TODSDesigner;
end;
{$endif}


// _____________________________________________________________________________
{**
  Jvr - 05/09/2002 12:38:00.<P>
}
class function TObjectListDatasetEditor.GetFileName: String;
const
  OpenFileTitle = 'Open';

begin
  with TOpenDialog.Create(nil) do begin
    try
      Title := OpenFileTitle;
      DefaultExt := '.dbf';
      Filter := sXbaseDataFilter;

      if Execute then begin
        Result := FileName;
      end;

    finally
      Free;
    end;
  end;
end;  { GetFileName }


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

    if (Index > 0) and not FCanCreate then begin
      Inc(Index);
    end;
    Result := dseVerbs[TdseAction(Index)];
  end;
end;  { GetVerb }


// _____________________________________________________________________________
{**
  Jvr - 12/12/2000 17:42:07<P>
}
function TObjectListDatasetEditor.GetVerbCount: Integer;
begin
  Result := inherited GetVerbCount + 1;
  FCanCreate := False;

  with TDBICustomListDataset(Component) do begin
    // If Dataset is active or has data then don't allow CreateDataset
    if Active {or (DataSize > 0)} then begin
      Inc(Result, 1);
    end;

    FCanCreate := not Active and ((FieldCount > 0) or (FieldDefs.Count > 0));

    if FCanCreate then begin
      Inc(Result, 1);
    end;
  end;
end;  { GetVerbCount }


// _____________________________________________________________________________
{**
  Jvr - 05/09/2002 12:45:57.<P>
}
class function TObjectListDatasetEditor.LoadFromFile(ADataSet: TDBICustomListDataset): Boolean;
var
  FileName: String;

begin
  FileName := GetFileName;
  Result := FileName <> '';
  if Result then begin
    ADataSet.LoadFromFile(FileName);
  end;
end;  { LoadFromFile }


// _____________________________________________________________________________
{**
  Jvr - 05/09/2002 12:36:06.<P>
}
class procedure TObjectListDatasetEditor.SaveToFile(
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
      else if (DataFormat = dfXML) then begin
        DefaultExt := 'xml';
        Filter := sClientDataFilter;
      end
      else if (DataFormat = dfPSV) then begin
        DefaultExt := 'psv';
        Filter := sPipeSeperatedDataFilter;
      end
      else begin
        raise EDBIException.Create(
          '"Xbase", "XML" and "PSV" are the only formats currently supported'
          );
      end;

      if Execute then begin
        ADataSet.SaveToFile(FileName, DataFormat);
      end;
    finally
      Free;
    end;
  end;
end;  { SaveToFile }




procedure Register;
begin
  RegisterComponentEditor(TDBICustomListDataset, TObjectListDatasetEditor);
end;


end.
