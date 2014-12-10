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
  1.0 | 29/05/2002 16:54:01 | Jvr | Initial Release - moved from DBIUtils
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIFilters;

interface

uses
  Windows, Classes, DBIIntfConsts, DBIStrings;

type
  TDBIDatasetFilterEvent = function(PRecBuf: PDBIChar): Bool of object;
  TDBIDatasetFilterType = (
    dfStatus,
    dfExpression,
    dfCallback
    );

  TDBIDatasetFilter = class(TPersistent)
  private
    FOwner: TObject;
    FFilterType: TDBIDatasetFilterType;
    FFilterProc: pfDSFilter;
    FIndexNo: Integer;

  public
    function Filter(
      PRecBuf: PDBIChar;
      RecordAttribute: DSAttr;
      StatusFilter: DSAttr
      ): Boolean; virtual; abstract;

    property Owner: TObject read FOwner write FOwner;
    property FilterType: TDBIDatasetFilterType read FFilterType write FFilterType;
    property FilterProc: pfDSFilter read FFilterProc write FFilterProc;
    property IndexNo: Integer read FIndexNo write FIndexNo;

  end;


  TDBIDatasetStatusFilter = class(TDBIDatasetFilter)
  public
    function Filter(
      PRecBuf: PDBIChar;
      RecordAttribute: DSAttr;
      StatusFilter: DSAttr
      ): Boolean; override;

  end;


  TDBIDatasetCodeFilter = class(TDBIDatasetFilter)
  public
    function Filter(
      PRecBuf: PDBIChar;
      RecordAttribute: DSAttr;
      StatusFilter: DSAttr
      ): Boolean; override;

  end;


implementation


{ TDBIDatasetStatusFilter }

// _____________________________________________________________________________
{**
  Jvr - 11/05/2001 12:29:49.<P>
}
function TDBIDatasetStatusFilter.Filter(
  PRecBuf: PDBIChar;
  RecordAttribute: DSAttr;
  StatusFilter: DSAttr
  ): Boolean;
begin
  Result := (RecordAttribute and StatusFilter) = RecordAttribute;
end;



{ TDBIDatasetCodeFilter }

// _____________________________________________________________________________
{**
  Jvr - 11/05/2001 12:25:20.<P>
}
function TDBIDatasetCodeFilter.Filter(
  PRecBuf: PDBIChar;
  RecordAttribute: DSAttr;
  StatusFilter: DSAttr
  ): Boolean;
begin
  Result := True;

  if Assigned(FFilterProc) then begin
    Result := FFilterProc(Integer(FOwner), PRecBuf);
  end;
end;  { Filter }


end.
