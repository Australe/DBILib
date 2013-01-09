program TestDesigner;

uses
  Forms,
  DBClient,
  FMainXE3 in 'FMainXE3.pas' {FormMainXE3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainXE3, FormMainXE3);
  Application.Run;
end.
