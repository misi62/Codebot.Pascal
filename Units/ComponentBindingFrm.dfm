object ComponentBindingForm: TComponentBindingForm
  Left = 240
  Top = 119
  BorderStyle = bsDialog
  Caption = 'Binding Editor'
  ClientHeight = 469
  ClientWidth = 862
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 75
    Height = 13
    Caption = 'Available items:'
  end
  object Label2: TLabel
    Left = 240
    Top = 8
    Width = 73
    Height = 13
    Caption = 'Selected items:'
  end
  object SourceList: TListBox
    Left = 8
    Top = 24
    Width = 193
    Height = 249
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnDblClick = AddButtonClick
  end
  object DestList: TListBox
    Left = 240
    Top = 24
    Width = 193
    Height = 249
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 5
    OnDblClick = RemoveButtonClick
  end
  object AddButton: TButton
    Left = 208
    Top = 80
    Width = 25
    Height = 25
    Caption = '>'
    TabOrder = 1
    OnClick = AddButtonClick
  end
  object AddAllButton: TButton
    Left = 208
    Top = 112
    Width = 25
    Height = 25
    Caption = '>>'
    TabOrder = 2
    OnClick = AddAllButtonClick
  end
  object RemoveAllButton: TButton
    Left = 208
    Top = 144
    Width = 25
    Height = 25
    Caption = '<<'
    TabOrder = 3
    OnClick = RemoveAllButtonClick
  end
  object RemoveButton: TButton
    Left = 208
    Top = 176
    Width = 25
    Height = 25
    Caption = '<'
    TabOrder = 4
    OnClick = RemoveButtonClick
  end
  object CancelButton: TButton
    Left = 358
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object OKButton: TButton
    Left = 278
    Top = 280
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 6
  end
end
