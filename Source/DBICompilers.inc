{$IFNDEF _VERSION_INFO}
  {$DEFINE _VERSION_INFO}

  {$B-} { Complete Boolean Evaluation }
  {$R+} { Range-Checking }
  {$T-} { Typed @ operator }
  {$X+} { Extended syntax }
  {$P+} { Open string params }
  {$J+} { Writeable structured consts }
  {$H+} { Use long strings by default }

  {$DEFINE VCL}
  {. $DEFINE _AGGREGATES}

  //##JVR - Temporary Fix
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}


  {$IFDEF VER120} { Borland Delphi 4.0 }
    {$DEFINE DELPHI4}
  {$ENDIF}

  {$IFDEF VER130} { Borland Delphi 5.0 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
  {$ENDIF}

  {$IFDEF VER140} { Borland Delphi 6.0 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
  {$ENDIF}

  {$IFDEF VER150} { Borland Delphi 7.0 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
  {$ENDIF}

  {$IFDEF VER160} { Borland Delphi 8.0 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
  {$ENDIF}

  {$IFDEF VER170} { Borland Delphi 2005 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
  {$ENDIF}

  {$IFDEF VER180} { Borland Delphi 2006 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
  {$ENDIF}

  {$IFDEF VER185} { Borland Delphi 2007 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
  {$ENDIF}

  {$IFDEF FPC} { Laxarus 1.0 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
  {$ENDIF}

  {$IFDEF VER200} { Borland Delphi 2009 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2009}
  {$ENDIF}

  {$IFDEF VER210} { Borland Delphi 2010 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
  {$ENDIF}

  {$IFDEF VER220} { Borland Delphi XE }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIxe}
  {$ENDIF}

  {$IFDEF VER230} { Borland Delphi XE2 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIxe}
    {$DEFINE DELPHIxe2}
  {$ENDIF}

  {$IFDEF VER240} { Borland Delphi XE3 }
    {$DEFINE DELPHI4}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI8}
    {$DEFINE DELPHI2005}
    {$DEFINE DELPHI2006}
    {$DEFINE DELPHI2007}
    {$DEFINE DELPHI2009}
    {$DEFINE DELPHI2010}
    {$DEFINE DELPHIxe}
    {$DEFINE DELPHIxe2}
    {$DEFINE DELPHIxe3}
  {$ENDIF}

{$ENDIF _VERSION_INFO}
