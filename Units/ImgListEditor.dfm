object TransparentImagesEditorForm: TTransparentImagesEditorForm
  Left = 264
  Top = 299
  Width = 684
  Height = 462
  Caption = 'Image List Editor'
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
  DesignSize = (
    676
    431)
  PixelsPerInch = 96
  TextHeight = 13
  object HorizontalBar1: THorizontalBar
    Left = 0
    Top = 168
    Width = 449
    Height = 48
    Anchors = [akLeft, akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
    Themed = False
    Flat = False
    Transparent = False
  end
  object HelpLabel: TLabel
    Left = 8
    Top = 224
    Width = 279
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'Hold shift to drag and reorder images'
    Layout = tlCenter
  end
  object OpenButton: TButton
    Left = 368
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Open...'
    TabOrder = 1
    OnClick = OpenClick
  end
  object DeleteButton: TButton
    Left = 368
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    Enabled = False
    TabOrder = 3
    OnClick = DeleteClick
  end
  object ResetButton: TButton
    Left = 368
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Reset'
    Enabled = False
    TabOrder = 4
    OnClick = ResetClick
  end
  object OkButton: TButton
    Left = 288
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 5
  end
  object CancelButton: TButton
    Left = 368
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object Frame: TFramedWindow
    Left = 8
    Top = 8
    Width = 353
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object SaveButton: TButton
    Left = 368
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Save...'
    Enabled = False
    TabOrder = 2
    OnClick = SaveClick
  end
  object OpenDialog: TOpenPictureDialog
    DefaultExt = 'png'
    Filter = 'Portable Network Graphics (*.png)|*.png'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 16
    Top = 16
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'png'
    Filter = 'Portable Network Graphics (*.png)|*.png'
    Left = 16
    Top = 48
  end
end
