object FormMainXE3: TFormMainXE3
  Left = 518
  Top = 530
  Caption = 'Dbi Form design test'
  ClientHeight = 300
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 562
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 497
      Top = 8
      Width = 23
      Height = 22
      OnClick = SpeedButton1Click
    end
    object DBNavigator1: TDBNavigator
      Left = 16
      Top = 10
      Width = 240
      Height = 25
      DataSource = DataSource
      Flat = True
      TabOrder = 0
    end
  end
  object cxGrid1: TcxGrid
    Left = 0
    Top = 41
    Width = 562
    Height = 259
    Align = alClient
    TabOrder = 1
    object cxGrid1DBTableView1: TcxGridDBTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.DataSource = DataSource
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      object cxGrid1DBTableView1Environment: TcxGridDBColumn
        DataBinding.FieldName = 'Environment'
      end
      object cxGrid1DBTableView1FullName: TcxGridDBColumn
        DataBinding.FieldName = 'FullName'
      end
      object cxGrid1DBTableView1Value: TcxGridDBColumn
        DataBinding.FieldName = 'Value'
      end
      object cxGrid1DBTableView1Application: TcxGridDBColumn
        DataBinding.FieldName = 'Application'
      end
      object cxGrid1DBTableView1Path: TcxGridDBColumn
        DataBinding.FieldName = 'Path'
      end
      object cxGrid1DBTableView1Name: TcxGridDBColumn
        DataBinding.FieldName = 'Name'
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1DBTableView1
    end
  end
  object DataSource: TDataSource
    DataSet = ODS
    Left = 360
    Top = 8
  end
  object ODS: TObjectListDataset
    ClassTypeName = 'TTestData'
    ObjectValidationProc = 'Validate'
    ReadOnly = True
    Options = [osStringFieldsDefaultToAnsi, osObjectValidation]
    Left = 312
    Top = 8
    object ODSEnvironment: TStringField
      FieldName = 'Environment'
    end
    object ODSFullName: TStringField
      FieldName = 'FullName'
    end
    object ODSValue: TStringField
      FieldName = 'Value'
    end
    object ODSApplication: TStringField
      FieldName = 'Application'
    end
    object ODSPath: TStringField
      FieldName = 'Path'
      Size = 80
    end
    object ODSName: TStringField
      FieldName = 'Name'
    end
  end
end
