object TransparentImagesEditorForm: TTransparentImagesEditorForm
  Left = 243
  Top = 248
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
  OnShow = FormShow
  DesignSize = (
    676
    428)
  PixelsPerInch = 96
  TextHeight = 13
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
  object Bar: THorizontalBar
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
    UseBackground = False
  end
  object OpenButton: TButton
    Left = 368
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Open...'
    TabOrder = 0
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
    TabOrder = 2
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
    TabOrder = 3
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
    TabOrder = 6
    Visible = False
  end
  object CancelButton: TButton
    Left = 368
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 5
    OnClick = CancelButtonClick
  end
  object SaveButton: TButton
    Left = 368
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Save...'
    Enabled = False
    TabOrder = 1
    OnClick = SaveClick
  end
  object Frame: TFramedWindow
    Left = 8
    Top = 8
    Width = 353
    Height = 201
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentColor = False
    TabOrder = 7
  end
  object CopyButton: TButton
    Left = 368
    Top = 136
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Copy'
    Enabled = False
    TabOrder = 4
    OnClick = CopyButtonClick
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
