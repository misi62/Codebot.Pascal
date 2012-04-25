object NamedStringsEditorForm: TNamedStringsEditorForm
  Left = 322
  Top = 148
  BorderStyle = bsDialog
  Caption = 'Named Strings Editor'
  ClientHeight = 471
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ItemLabel: TLabel
    Left = 8
    Top = 8
    Width = 26
    Height = 13
    Caption = 'Item:'
  end
  object BodyLabel: TLabel
    Left = 224
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Body:'
  end
  object HorizontalBar: THorizontalBar
    Left = 8
    Top = 224
    Width = 513
    Height = 40
    Color = clBtnFace
    ParentColor = False
    Themed = False
    Flat = False
    Transparent = False
    UseBackground = False
  end
  object BodyMemo: TMemo
    Left = 224
    Top = 24
    Width = 297
    Height = 233
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnExit = BodyMemoExit
  end
  object ItemListBox: TListBox
    Left = 8
    Top = 56
    Width = 129
    Height = 201
    ItemHeight = 13
    TabOrder = 2
    OnClick = ItemListBoxClick
  end
  object OkButton: TButton
    Left = 366
    Top = 272
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 7
  end
  object CancelButton: TButton
    Left = 446
    Top = 272
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object AddButton: TButton
    Left = 144
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 3
    OnClick = AddButtonClick
  end
  object RemoveButton: TButton
    Left = 144
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Remove'
    TabOrder = 4
    OnClick = RemoveButtonClick
  end
  object ItemEdit: TEdit
    Left = 8
    Top = 24
    Width = 129
    Height = 21
    TabOrder = 0
    OnChange = ItemEditChange
  end
  object MoveUpButton: TButton
    Tag = -1
    Left = 144
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Move Up'
    TabOrder = 5
    OnClick = MoveButtonClick
  end
  object MoveDownButton: TButton
    Tag = 1
    Left = 144
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Move Down'
    TabOrder = 6
    OnClick = MoveButtonClick
  end
end
