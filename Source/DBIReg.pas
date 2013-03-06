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
  1.0 | 30/10/2007 09:05:17 | Jvr | Initial Release
  ______________________________________________________________________________
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
