program DBILibTestHarness;
uses
  Forms,
  Windows,
  SysUtils,
  DB,
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

