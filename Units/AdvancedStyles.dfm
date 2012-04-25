object AdvancedStylesForm: TAdvancedStylesForm
  Left = 290
  Top = 256
  BorderStyle = bsDialog
  Caption = 'Advanced'
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
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object OffsetGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 81
    Caption = 'Band caption offet'
    TabOrder = 0
    object Label15: TLabel
      Left = 112
      Top = 24
      Width = 10
      Height = 13
      Hint = 'X offset caption'
      Caption = '&X:'
    end
    object Label1: TLabel
      Left = 192
      Top = 24
      Width = 10
      Height = 13
      Hint = 'Y offset caption'
      Caption = '&Y:'
    end
    object XEdit: TEdit
      Left = 112
      Top = 40
      Width = 41
      Height = 21
      Hint = 'X offset caption'
      TabOrder = 3
      Text = '0'
      OnExit = XEditExit
    end
    object XSpinner: TUpDown
      Left = 153
      Top = 40
      Width = 16
      Height = 21
      Hint = 'X offset caption'
      Associate = XEdit
      Min = -999
      Max = 999
      TabOrder = 4
      OnChangingEx = XSpinnerChangingEx
    end
    object YEdit: TEdit
      Left = 192
      Top = 40
      Width = 41
      Height = 21
      Hint = 'Y offset caption'
      TabOrder = 5
      Text = '0'
      OnExit = YEditExit
    end
    object YSpinner: TUpDown
      Left = 233
      Top = 40
      Width = 16
      Height = 21
      Hint = 'Y offset caption'
      Associate = YEdit
      Min = -999
      Max = 999
      TabOrder = 6
      OnChangingEx = YSpinnerChangingEx
    end
    object LeftButton: TImageButton
      Left = 16
      Top = 37
      Width = 26
      Height = 26
      Hint = 'Left align caption'
      AutoPopup = True
      Caption = 'ImageButton1'
      CaptionPosition = cpHidden
      Down = False
      FocusedRect = False
      ImageIndex = 67
      Images = ResourceModule.GlassImages
      Kind = bkButton
      Locked = False
      OwnerDrawn = False
      SharedImages = False
      Style = bsFlat
      TabOrder = 0
      TabStop = True
      Toggle = True
      OnClick = BandAlignmentClick
    end
    object CenterButton: TImageButton
      Tag = 1
      Left = 44
      Top = 37
      Width = 26
      Height = 26
      Hint = 'Center align caption'
      AutoPopup = True
      CaptionPosition = cpHidden
      Down = False
      FocusedRect = False
      ImageIndex = 68
      Images = ResourceModule.GlassImages
      Kind = bkButton
      Locked = False
      OwnerDrawn = False
      SharedImages = False
      Style = bsFlat
      TabOrder = 1
      TabStop = True
      Toggle = True
      OnClick = BandAlignmentClick
    end
    object RightButton: TImageButton
      Tag = 2
      Left = 71
      Top = 37
      Width = 26
      Height = 26
      Hint = 'Right align caption'
      AutoPopup = True
      Caption = 'ImageButton1'
      CaptionPosition = cpHidden
      Down = False
      FocusedRect = False
      ImageIndex = 70
      Images = ResourceModule.GlassImages
      Kind = bkButton
      Locked = False
      OwnerDrawn = False
      SharedImages = False
      Style = bsFlat
      TabOrder = 2
      TabStop = True
      Toggle = True
      OnClick = BandAlignmentClick
    end
  end
end
