object FolderBarsForm: TFolderBarsForm
  Left = 406
  Top = 165
  BorderStyle = bsDialog
  Caption = 'Folder Bars Editor'
  ClientHeight = 373
  ClientWidth = 548
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
    Width = 161
    Height = 193
    Style = lbOwnerDrawVariable
    ItemHeight = 16
    TabOrder = 3
    OnClick = ListBoxClick
    OnDrawItem = ListBoxDrawItem
    OnMeasureItem = ListBoxMeasureItem
  end
  object GroupBox: TGroupBox
    Left = 176
    Top = 8
    Width = 305
    Height = 193
    Caption = 'Folder Bar: '
    TabOrder = 0
    object CaptionLabel: TLabel
      Left = 8
      Top = 24
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Ca&ption:'
      FocusControl = CaptionEdit
      Layout = tlCenter
    end
    object IndexLabel: TLabel
      Left = 80
      Top = 88
      Width = 50
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Inde&x:'
      FocusControl = IndexEdit
      Layout = tlCenter
    end
    object SelectedIndexLabel: TLabel
      Left = 16
      Top = 152
      Width = 114
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Selected Index:'
      FocusControl = CaptionEdit
      Layout = tlCenter
    end
    object ImageIndexLabel: TLabel
      Left = 16
      Top = 120
      Width = 114
      Height = 21
      Alignment = taRightJustify
      AutoSize = False
      Caption = '&Image Index:'
      FocusControl = CaptionEdit
      Layout = tlCenter
    end
    object CaptionEdit: TEdit
      Left = 64
      Top = 24
      Width = 145
      Height = 21
      TabOrder = 0
      OnChange = CaptionEditChange
    end
    object NewItemButton: TButton
      Left = 224
      Top = 56
      Width = 65
      Height = 25
      Caption = 'New Ite&m'
      TabOrder = 9
      OnClick = NewItemButtonClick
    end
    object RemoveButton: TButton
      Left = 224
      Top = 88
      Width = 65
      Height = 25
      Caption = '&Remove'
      Enabled = False
      TabOrder = 6
      OnClick = RemoveButtonClick
    end
    object IndexEdit: TEdit
      Left = 136
      Top = 88
      Width = 57
      Height = 21
      ReadOnly = True
      TabOrder = 3
      Text = '0'
    end
    object IndexSpin: TUpDown
      Left = 193
      Top = 88
      Width = 16
      Height = 21
      Associate = IndexEdit
      Max = 9999
      TabOrder = 8
      OnChangingEx = IndexSpinChangingEx
    end
    object VisibleBox: TCheckBox
      Left = 144
      Top = 56
      Width = 73
      Height = 17
      Caption = '&Visible'
      TabOrder = 2
      OnClick = VisibleBoxClick
    end
    object EnabledBox: TCheckBox
      Left = 64
      Top = 56
      Width = 73
      Height = 17
      Caption = '&Enabled'
      TabOrder = 1
      OnClick = EnabledBoxClick
    end
    object NewBarButton: TButton
      Left = 224
      Top = 24
      Width = 65
      Height = 25
      Caption = 'New &Bar'
      TabOrder = 7
      OnClick = NewBarButtonClick
    end
    object SelectedIndexEdit: TImageListEdit
      Left = 136
      Top = 120
      Width = 75
      Height = 21
      TabOrder = 4
      TabStop = True
      ParentColor = False
      DisplayCount = 5
      ImageIndex = -1
      OnChange = ImageIndexEditChange
      DesignSize = (
        71
        17)
    end
    object ImageIndexEdit: TImageListEdit
      Left = 136
      Top = 152
      Width = 75
      Height = 21
      TabOrder = 5
      TabStop = True
      ParentColor = False
      DisplayCount = 5
      ImageIndex = -1
      OnChange = SelectedIndexEditChange
      DesignSize = (
        71
        17)
    end
  end
  object OKButtopn: TButton
    Left = 326
    Top = 208
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 406
    Top = 208
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
