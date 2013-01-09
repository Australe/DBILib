// _____________________________________________________________________________
{**
  <H5>Copyright</H5>
  Better Innovative Technical Services Pty Ltd<BR>
  Copyright (C) 1997-2003, All rights reserved.

  Project:       DBILib
  Files(s):      DBIDateTimeUtils.pas
  Classes:       N/A
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 10/13/2003 3:06:18 PM | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

unit DBIDateTimeUtils;

interface

type
  TDBIDateOrder = (doMDY, doDMY, doYMD);

function GetDateOrder(const DateFormat: string): TDBIDateOrder;

implementation

// _____________________________________________________________________________
{**
  Jvr - 10/13/2003 3:08:31 PM - Initial code.<p>

  Taken from SysUtils (D5)
}
function GetDateOrder(const DateFormat: string): TDBIDateOrder;
var
  Index: Integer;

begin
  Result := doMDY;
  Index := 1;

  while Index <= Length(DateFormat) do begin
    case Chr(Ord(DateFormat[Index]) and $DF) of
      'E': Result := doYMD;
      'Y': Result := doYMD;
      'M': Result := doMDY;
      'D': Result := doDMY;
    else
      Inc(Index);
      Continue;
    end;
    
    Exit;
  end;
  Result := doMDY;
end;  { GetDateOrder }


end.
