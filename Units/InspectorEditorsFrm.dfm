object InspectorEditorsForm: TInspectorEditorsForm
  Left = 200
  Top = 134
  BorderStyle = bsDialog
  Caption = 'Inspector Items Editor'
  ClientHeight = 373
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 8
    Top = 8
    Width = 145
    Height = 161
    Style = lbOwnerDrawFixed
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBoxClick
    OnDrawItem = ListBoxDrawItem
  end
  object GroupBox: TGroupBox
    Left = 160
    Top = 8
    Width = 305
    Height = 161
    Caption = 'Editor: '
    TabOrder = 0
    object KindLabel: TLabel
      Left = 8
      Top = 28
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Kind:'
      FocusControl = KindComboBox
    end
    object NameLabel: TLabel
      Left = 8
      Top = 60
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Name:'
      FocusControl = NameEdit
    end
    object ValueLabel: TLabel
      Left = 8
      Top = 92
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Value:'
      FocusControl = ValueEdit
    end
    object IndexLabel: TLabel
      Left = 88
      Top = 124
      Width = 50
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Index:'
      FocusControl = IndexEdit
    end
    object KindComboBox: TComboBox
      Left = 64
      Top = 24
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object NameEdit: TEdit
      Left = 64
      Top = 56
      Width = 145
      Height = 21
      TabOrder = 1
      OnChange = NameEditChange
    end
    object ValueEdit: TEdit
      Left = 64
      Top = 88
      Width = 145
      Height = 21
      TabOrder = 2
      OnChange = ValueEditChange
    end
    object AddButton: TButton
      Left = 224
      Top = 24
      Width = 65
      Height = 25
      Caption = '&Add'
      TabOrder = 4
      OnClick = AddButtonClick
    end
    object RemoveButton: TButton
      Left = 224
      Top = 56
      Width = 65
      Height = 25
      Caption = '&Remove'
      Enabled = False
      TabOrder = 5
      OnClick = RemoveButtonClick
    end
    object IndexEdit: TEdit
      Left = 144
      Top = 120
      Width = 49
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object UpDown: TUpDown
      Left = 193
      Top = 120
      Width = 17
      Height = 21
      Associate = IndexEdit
      Max = 0
      TabOrder = 6
      OnChangingEx = UpDownChangingEx
    end
  end
  object OKButtopn: TButton
    Left = 310
    Top = 176
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 390
    Top = 176
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
