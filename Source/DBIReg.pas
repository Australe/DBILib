// _____________________________________________________________________________
{**
  <H5>Copyright</H5> 
  Better Innovative Technical Services Pty Ltd<BR>
  Copyright (C) 2007, All rights reserved.<!--

  Project:       DBILib
  Files(s):      DBIReg.pas
  Classes:       ...
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 30/10/2007 09:05:17 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

unit DBIReg;

interface

uses
  Classes;

procedure Register;

implementation

uses
  DBIConst, DBIObjectListDatasets, DBIXbaseDatasets;

procedure Register;
begin
  RegisterComponents(DBIComponentPage, [TObjectListDataset]);
  RegisterComponents(DBIComponentPage, [TStringsDataset]);
  RegisterComponents(DBIComponentPage, [TXBaseDataset]);
end;


end.
