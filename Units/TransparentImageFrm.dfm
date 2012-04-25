object TransparentImageForm: TTransparentImageForm
  Left = 339
  Top = 260
  BorderStyle = bsDialog
  Caption = 'Transparent Images Editor'
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
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object HorizontalBar1: THorizontalBar
    Left = 0
    Top = 114
    Width = 401
    Height = 20
    Color = clBtnFace
    ParentColor = False
    Themed = True
    Flat = False
    Transparent = False
  end
  object Background1: TBackground
    Left = 8
    Top = 8
    Width = 374
    Height = 84
    Color = clAppWorkSpace
    OnPaint = Background1Paint
  end
  object Button1: TButton
    Left = 72
    Top = 144
    Width = 75
    Height = 25
    Caption = 'Cle&ar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 152
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Load'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 232
    Top = 144
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
  end
  object Button4: TButton
    Left = 312
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ScrollBar1: TScrollBar
    Left = 8
    Top = 104
    Width = 377
    Height = 17
    PageSize = 0
    TabOrder = 4
    TabStop = False
    OnChange = ScrollBar1Change
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Portable Network Graphics (*.png)|*.png'
    Left = 16
    Top = 144
  end
end
