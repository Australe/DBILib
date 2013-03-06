program DBILibTestHarness;
uses
  FastMM4,
  Forms,
  Windows,
  SysUtils,
  DBClient,
  omLocals,
  omMVCManagers,
  omApplications,
  omActivitySelfTests,
  DBILibAllUnitTests;


begin
  Application.Initialize;

  SetApplicationClass(TomApplication);

  MVCManager := Local(TomMVCManager.Create(nil, 'Dummy -Activity SelfTest')).Obj as TomMVCManager;

  Application.Run;
end.

