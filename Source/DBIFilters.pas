// _____________________________________________________________________________
{**
  <!--
  ObjectMastery Pty Ltd
  Copyright (C) 2002, All rights reserved.

  Project:       omDBLib
  Files(s):      DBIFilters
  Classes:       N/A
  Author:        John Vander Reest
  -->

  <H5>Description</H5>
  N/A

  <H5>Notes</H5>
  N/A

  <H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 29/05/2002 16:54:01 | Jvr | Initial Release - moved from DBIUtils
  ______________________________________________________________________________
  </CODE>
}

{#omcodecop off : jvr : native api code}

unit DBIFilters;

interface

uses
  Windows, Classes, DBIIntfConsts, DBIStrings;

//##JVRconst
//##JVR  GInternalFilterCount = 1;

type
  TDBIDatasetFilterEvent = function(PRecBuf: PDBIChar): Bool of object;
  TDBIDatasetFilterType = (
    dfStatus,
    dfExpression,
    dfCallback
    );

  TDBIDatasetFilter = class(TObject)
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

  end;  { TDBIDatasetFilter }


  TDBIDatasetStatusFilter = class(TDBIDatasetFilter)
  public
    function Filter(
      PRecBuf: PDBIChar;
      RecordAttribute: DSAttr;
      StatusFilter: DSAttr
      ): Boolean; override;

  end;  { TDBIDatasetStatusFilter }


  TDBIDatasetCodeFilter = class(TDBIDatasetFilter)
  public
    function Filter(
      PRecBuf: PDBIChar;
      RecordAttribute: DSAttr;
      StatusFilter: DSAttr
      ): Boolean; override;

  end;  { TDBIDatasetCodeFilter }


implementation

const
  UnitName = 'DBIFilters';


// =============================================================================
// 'TDBIDatasetStatusFilter' methods
// =============================================================================

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



// =============================================================================
// 'TDBIDatasetCodeFilter' methods
// =============================================================================

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
