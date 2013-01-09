program TestDesignerD5;

uses
  Forms,
  TestData in 'TestData.pas',
  FMain in 'FMain.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
