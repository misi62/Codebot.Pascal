object VisualLogFrame: TVisualLogFrame
  Left = 0
  Top = 0
  Width = 487
  Height = 375
  TabOrder = 0
  object LogFilesLabel: TLabel
    Left = 0
    Top = 0
    Width = 72
    Height = 13
    Caption = 'List of log files:'
  end
  object BackButton: TButton
    Left = 200
    Top = 256
    Width = 75
    Height = 25
    Caption = '&Back'
    TabOrder = 0
  end
  object LogList: TShellView
    Left = 0
    Top = 16
    Width = 273
    Height = 233
    SpecialFolder = sfDesktop
    TabOrder = 1
    ViewMode = vmDetails
  end
end
