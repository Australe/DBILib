program DBILibTestHarness;
uses
  FastMM4,
  omFastMMLogging,
  omFastMMLoggingUtils,
  MidasLib,
  Forms,
  Windows,
  SysUtils,
  omLocals,
  omMVCManagers,
  omApplications,
  omActivitySelfTests,
  DBILibAllUnitTests;

procedure SetupFastMMLoggingFromCmdLine;
// Need this function defined here because FastMM can only be used in the main dpr.
begin
  omFastMMLogging.SetFastMMLoggingContext(omFastMMLoggingUtils.GetFastMMLoggingContext(CmdLine, True));
  FastMM4.DoLeakLogging := omFastMMLogging.GetDoLeakLogging;
  FastMM4.CustomReportMemoryLeaksOnShutdownProc := omFastMMLogging.AppendEventLog;
  FastMM4.ShowLeakMessages := omFastMMLogging.GetShowLeakMessage;
end;

{$R *.RES}
begin
  SetApplicationClass(TomApplication);
  TomApplication.LogStartupInfo;

  Application.Initialize;
  Application.MainFormOnTaskBar := True;

  MVCManager := Local(TomMVCManager.Create(nil, 'Dummy -Activity SelfTest')).Obj as TomMVCManager;

  SetupFastMMLoggingFromCmdLine;

  Application.Run;
end.


