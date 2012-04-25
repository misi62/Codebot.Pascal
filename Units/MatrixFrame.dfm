object MatrixGrid: TMatrixGrid
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Color = clBtnFace
  ParentColor = False
  TabOrder = 0
  DesignSize = (
    320
    240)
  object PhotoLabel: TLabel
    Left = 4
    Top = 4
    Width = 125
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '3 attributes found'
    Layout = tlCenter
  end
  object MatrixButton: TImageButton
    Left = 132
    Top = 4
    Width = 19
    Height = 18
    AllowFocus = False
    Anchors = [akTop, akRight]
    AutoSize = True
    AutoPopup = False
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    FocusedRect = False
    ImageIndex = 36
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 1
    Toggle = False
  end
  object ClearButon: TImageButton
    Left = 160
    Top = 4
    Width = 19
    Height = 18
    AllowFocus = False
    Anchors = [akTop, akRight]
    AutoSize = True
    AutoPopup = True
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    FocusedRect = False
    ImageIndex = 33
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 2
    Toggle = False
  end
  object Matrix: TInspector
    Left = 2
    Top = 34
    Width = 188
    Height = 120
    Cursor = crArrow
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoPopup = False
    BorderStyle = bsNone
    CharCase = ecNormal
    Color = clBtnFace
    ItemHeight = 17
    ItemIndex = -1
    IncrementalSearch = False
    ParentColor = False
    ReadOnly = False
    ShowExceptions = False
    SplitPos = 96
    Style = isClassic
    TabOrder = 0
    WantEnter = False
  end
end
