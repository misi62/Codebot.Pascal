object MacroForm: TMacroForm
  Left = 272
  Top = 157
  BorderStyle = bsDialog
  Caption = 'Keyboard Macros'
  ClientHeight = 606
  ClientWidth = 862
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
  object Bar: THorizontalBar
    Left = 0
    Top = 136
    Width = 313
    Height = 48
    Color = clBtnFace
    ParentColor = False
    Themed = True
    Flat = False
    Transparent = False
  end
  object SelectLabel: TLabel
    Left = 8
    Top = 8
    Width = 74
    Height = 13
    Caption = 'Select a macro:'
  end
  object List: TListBox
    Left = 8
    Top = 24
    Width = 289
    Height = 153
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListClick
    OnDblClick = ListDblClick
    OnKeyDown = ListKeyDown
  end
  object OkButton: TButton
    Left = 144
    Top = 192
    Width = 75
    Height = 25
    Caption = '&OK'
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 224
    Top = 192
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
