unit FMainXE3;

interface

uses
  SysUtils, Windows, Forms, Dialogs, DB, Grids, DBGrids, DBCtrls, Controls, Buttons, Classes, ExtCtrls,
  Contnrs, DBIDataset, DBIObjectListDatasets, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, cxDBData, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridLevel, cxClasses, cxGridCustomView,
  cxGrid;

type
  TFormMainXE3 = class(TForm)
    Panel1: TPanel;
    DBNavigator1: TDBNavigator;
    DataSource: TDataSource;
    SpeedButton1: TSpeedButton;
    ODS: TObjectListDataset;
    ODSEnvironment: TStringField;
    ODSFullName: TStringField;
    ODSValue: TStringField;
    ODSApplication: TStringField;
    ODSPath: TStringField;
    ODSName: TStringField;
    cxGrid1DBTableView1: TcxGridDBTableView;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxGrid1DBTableView1Environment: TcxGridDBColumn;
    cxGrid1DBTableView1FullName: TcxGridDBColumn;
    cxGrid1DBTableView1Value: TcxGridDBColumn;
    cxGrid1DBTableView1Application: TcxGridDBColumn;
    cxGrid1DBTableView1Path: TcxGridDBColumn;
    cxGrid1DBTableView1Name: TcxGridDBColumn;
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMainXE3: TFormMainXE3;

implementation

{$R *.dfm}

uses
  TestData;
  
procedure TFormMainXE3.SpeedButton1Click(Sender: TObject);
begin
  if not (Assigned(DataSource.DataSet) and DataSource.DataSet.Active) then begin
//{##JVR
    cxGrid1DBTableView1.DataController.DataSource := nil;
    cxGrid1DBTableView1.ClearItems;

    TTestData.AddData(ODS);

    cxGrid1DBTableView1.DataController.DataSource := DataSource;
    cxGrid1DBTableView1.DataController.CreateAllItems;
(*##JVR
//}
    TTestData.AddData(ODS);
//*)
  end;
end;

end.
