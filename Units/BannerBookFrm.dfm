object BannerBookForm: TBannerBookForm
  Left = 238
  Top = 161
  BorderStyle = bsDialog
  Caption = 'Banner Book Editor'
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
  object Pages: TMemo
    Left = 8
    Top = 8
    Width = 369
    Height = 225
    TabOrder = 0
  end
  object OkButton: TButton
    Left = 224
    Top = 240
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 304
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
