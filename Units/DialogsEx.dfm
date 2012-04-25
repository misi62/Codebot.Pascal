object MessageForm: TMessageForm
  Left = 351
  Top = 139
  BorderStyle = bsDialog
  Caption = 'Message'
  ClientHeight = 596
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object MessageLabel: TLabel
    Left = 72
    Top = 24
    Width = 433
    Height = 129
    AutoSize = False
    BiDiMode = bdRightToLeftNoAlign
    Caption = 'MessageLabel'
    ParentBiDiMode = False
    Transparent = True
    WordWrap = True
  end
  object MessageIcon: TImage
    Left = 8
    Top = 8
    Width = 54
    Height = 54
    Center = True
  end
  object B4: TButton
    Left = 192
    Top = 168
    Width = 75
    Height = 25
    Caption = 'B4'
    TabOrder = 1
    Visible = False
  end
  object B3: TButton
    Left = 272
    Top = 168
    Width = 75
    Height = 25
    Caption = 'B3'
    TabOrder = 2
    Visible = False
  end
  object B2: TButton
    Left = 352
    Top = 168
    Width = 75
    Height = 25
    Caption = 'B2'
    TabOrder = 3
    Visible = False
  end
  object B1: TButton
    Left = 432
    Top = 168
    Width = 75
    Height = 25
    Caption = 'B1'
    TabOrder = 4
    Visible = False
  end
  object InputEdit: TEdit
    Left = 72
    Top = 40
    Width = 433
    Height = 21
    TabOrder = 0
    Visible = False
  end
end
