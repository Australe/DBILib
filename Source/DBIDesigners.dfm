object FormODSDesigner: TFormODSDesigner
  Left = 505
  Top = 634
  Width = 428
  Height = 399
  Caption = 'ObjectlistDataset Designer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000FFFFFFF00FFFF000FBBBBBB00BBBBB00FB00000000000000FBF
    FFFF00FFFF000FBBBBBB00BBBBB00FB00000000000000FBFFFFF00FFFF000FBB
    BBBB00BBBBB00FB00000000000000FBFFFFF00FFFF000FBBBBBB00BBBBB00FB0
    0000000000000FB000000000000000B00000000000000000000000000000FFFF
    000080C3000080C100009FFF000080C3000080C100009FFF000080C3000080C1
    00009FFF000080C3000080C100009FFF00009FFF0000DFFF0000FFFF0000}
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 0
    Top = 30
    Width = 412
    Height = 331
    ActivePage = TabSheetPreferences
    Align = alClient
    TabOrder = 0
    TabPosition = tpBottom
    OnChange = PageControlChange
    object TabSheetPreferences: TTabSheet
      Caption = 'Preferences'
      object PanelSettings: TPanel
        Left = 0
        Top = 0
        Width = 404
        Height = 303
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 10
        Color = clWindow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        PopupMenu = PopupMenuPreferences
        TabOrder = 0
        object PanelIntro: TPanel
          Left = 10
          Top = 10
          Width = 384
          Height = 76
          Align = alTop
          BevelOuter = bvNone
          Color = clWindow
          TabOrder = 0
          object LabelIntro1: TLabel
            Left = 0
            Top = 0
            Width = 384
            Height = 32
            Align = alTop
            Caption = 
              'The ObjectListDataset designer allows you to generate code for a ' +
              'business object  from your pre-defined fields.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clInactiveCaption
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object BevelIntro: TBevel
            Left = 0
            Top = 32
            Width = 384
            Height = 12
            Align = alTop
            Shape = bsSpacer
          end
          object LabelIntro2: TLabel
            Left = 0
            Top = 44
            Width = 384
            Height = 32
            Align = alTop
            Caption = 
              'To generate the Business Object code alter the preferences to your ' +
              'likings and select the Code-Tab.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clInactiveCaption
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
        end
        object PanelName: TPanel
          Left = 10
          Top = 86
          Width = 384
          Height = 52
          Align = alTop
          BevelOuter = bvNone
          Color = clWindow
          TabOrder = 1
          object BevelName: TBevel
            Left = 0
            Top = 0
            Width = 384
            Height = 35
            Align = alTop
            Shape = bsBottomLine
          end
          object LabelName: TLabel
            Left = 0
            Top = 35
            Width = 384
            Height = 16
            Align = alTop
            Caption = '&Name of your Business Object (no spaces)'
            FocusControl = EditName
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clInactiveCaption
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object EditName: TEdit
            Left = 0
            Top = 15
            Width = 300
            Height = 17
            BorderStyle = bsNone
            TabOrder = 0
            Text = 'Untitled1'
          end
        end
        object PanelClass: TPanel
          Left = 10
          Top = 138
          Width = 384
          Height = 70
          Align = alTop
          BevelOuter = bvNone
          Color = clWindow
          TabOrder = 2
          object BevelClass: TBevel
            Left = 0
            Top = 0
            Width = 384
            Height = 35
            Align = alTop
            Shape = bsBottomLine
          end
          object LabelClass: TLabel
            Left = 0
            Top = 35
            Width = 384
            Height = 32
            Align = alTop
            Caption = 
              'Provides you with a Custom class.  Generally you will not need this'
            FocusControl = CheckBoxCustomClass
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clInactiveCaption
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object CheckBoxCustomClass: TCheckBox
            Left = 0
            Top = 15
            Width = 300
            Height = 17
            Caption = 'Custom &Class'
            TabOrder = 0
          end
        end
        object PanelAccessors: TPanel
          Left = 10
          Top = 208
          Width = 384
          Height = 52
          Align = alTop
          BevelOuter = bvNone
          Color = clWindow
          TabOrder = 3
          object BevelAccessors: TBevel
            Left = 0
            Top = 0
            Width = 384
            Height = 35
            Align = alTop
            Shape = bsBottomLine
          end
          object LabelAccessors: TLabel
            Left = 0
            Top = 35
            Width = 384
            Height = 16
            Align = alTop
            Caption = 'Provides Getters and Setters for your properties, '
            FocusControl = CheckBoxCustomClass
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clInactiveCaption
            Font.Height = -13
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object CheckBoxAccessors: TCheckBox
            Left = 0
            Top = 15
            Width = 97
            Height = 17
            Caption = '&Accessors'
            TabOrder = 0
          end
        end
      end
    end
    object TabSheetCode: TTabSheet
      Caption = 'Code'
      ImageIndex = 1
      object MemoBusinessObject: TMemo
        Left = 0
        Top = 0
        Width = 404
        Height = 303
        Align = alClient
        BorderStyle = bsNone
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = PopupMenuCode
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object PanelBusinessObject: TPanel
    Left = 0
    Top = 0
    Width = 412
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    Visible = False
    object SpeedButton1: TSpeedButton
      Left = 4
      Top = 2
      Width = 23
      Height = 22
      Caption = '>>'
      Flat = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = GenerateExecute
    end
  end
  object PopupMenuCode: TPopupMenu
    Left = 72
    object EditCutItem: TMenuItem
      Caption = 'Cu&t'
      ShortCut = 16472
      OnClick = EditCutItemExecute
    end
    object EditCopyItem: TMenuItem
      Caption = '&Copy'
      ShortCut = 16451
      OnClick = EditCopyItemExecute
    end
    object EditPasteItem: TMenuItem
      Caption = '&Paste'
      ShortCut = 16470
      OnClick = EditPasteItemExecute
    end
    object EditDeleteItem: TMenuItem
      Caption = '&Delete'
      ShortCut = 46
      OnClick = EditDeleteItemExecute
    end
    object EditSelectAllItem: TMenuItem
      Caption = 'Se&lect All'
      ShortCut = 16449
      OnClick = EditSelectAllExecute
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object EditSaveItem: TMenuItem
      Caption = '&Save'
      ShortCut = 16467
      OnClick = EditSaveItemExecute
    end
  end
  object PopupMenuPreferences: TPopupMenu
    Left = 104
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.pas'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShareAware, ofEnableSizing]
    Left = 40
  end
end
