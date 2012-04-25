object CameraForm: TCameraForm
  Left = 130
  Top = 227
  Width = 788
  Height = 731
  Caption = 'Photo Capture'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    780
    697)
  PixelsPerInch = 96
  TextHeight = 13
  object ZoomLabel: TLabel
    Left = 16
    Top = 276
    Width = 33
    Height = 25
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Zoom:'
    Enabled = False
    Layout = tlCenter
  end
  object ZoomBar: TSlideBar
    Left = 56
    Top = 275
    Width = 177
    Height = 26
    Anchors = [akLeft, akBottom]
    Enabled = False
    Kind = sbHorizontal
    Max = 10.000000000000000000
    Step = 1.000000000000000000
    Tracking = False
    OnChange = ZoomBarChange
  end
  object SizeLabel: TLabel
    Left = 16
    Top = 249
    Width = 33
    Height = 25
    Anchors = [akLeft, akBottom]
    AutoSize = False
    Caption = 'Size:'
    Enabled = False
    Layout = tlCenter
  end
  object ProgressIndicator: TIndeterminateProgress
    Left = 8
    Top = 379
    Width = 377
    Height = 25
    Status = psReady
    Anchors = [akLeft, akBottom]
  end
  object PhotoList: TPanel
    Left = 248
    Top = 0
    Width = 313
    Height = 372
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 7
  end
  object OkButton: TButton
    Left = 398
    Top = 379
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 5
    OnClick = OkButtonClick
  end
  object CancelButton: TButton
    Left = 478
    Top = 379
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object CaptuteButton: TButton
    Left = 160
    Top = 331
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'C&apture'
    Enabled = False
    TabOrder = 4
    OnClick = CaptuteButtonClick
  end
  object FlashCheckBox: TCheckBox
    Left = 16
    Top = 303
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Camera flash'
    Enabled = False
    TabOrder = 2
    OnClick = FlashCheckBoxClick
  end
  object SizeComboBox: TComboBox
    Left = 56
    Top = 251
    Width = 177
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    Enabled = False
    ItemHeight = 13
    TabOrder = 1
    OnChange = SizeComboBoxChange
    Items.Strings = (
      'Small'
      'Medium'
      'Large'
      'Extra Large')
  end
  object CameraList: TDrawList
    Left = 16
    Top = 40
    Width = 217
    Height = 196
    Anchors = [akLeft, akTop, akBottom]
    ParentColor = False
    TabOrder = 0
    AutoScroll = True
    HotTrack = False
    ItemHeight = 150
    OnDblClick = CameraListDblClick
    OnDrawItem = CamerDrawItem
    OnSelectItem = CameraListSelectItem
  end
  object ReconnectButton: TButton
    Left = 80
    Top = 331
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Reconnect'
    TabOrder = 3
    OnClick = ReconnectButtonClick
  end
  object CameraTimer: TTimer
    Interval = 100
    OnTimer = CameraTimerTimer
    Left = 24
    Top = 200
  end
end
