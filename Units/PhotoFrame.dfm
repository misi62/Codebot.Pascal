object PhotoCapture: TPhotoCapture
  Left = 0
  Top = 0
  Width = 454
  Height = 373
  Color = clBtnFace
  ParentColor = False
  TabOrder = 0
  TabStop = True
  DesignSize = (
    454
    373)
  object Image: TImage
    Left = 4
    Top = 36
    Width = 183
    Height = 133
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    OnDblClick = ImageDblClick
    OnMouseDown = ImageMouseDown
  end
  object PhotoLabel: TLabel
    Left = 4
    Top = 4
    Width = 125
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'No photo'
    Layout = tlCenter
    OnMouseDown = ImageMouseDown
  end
  object DimesionLabel: TLabel
    Left = 8
    Top = 176
    Width = 177
    Height = 25
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Transparent = True
    Layout = tlCenter
  end
  object MoveLeftButton: TImageButton
    Left = 4
    Top = 176
    Width = 27
    Height = 26
    AllowFocus = False
    Anchors = [akLeft, akBottom]
    AutoSize = True
    AutoPopup = False
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    Enabled = False
    FocusedRect = False
    ImageIndex = 14
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 2
    Toggle = False
    OnClick = MoveLeftButtonClick
  end
  object PriorButton: TImageButton
    Left = 28
    Top = 176
    Width = 27
    Height = 26
    AllowFocus = False
    Anchors = [akLeft, akBottom]
    AutoSize = True
    AutoPopup = False
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    Enabled = False
    FocusedRect = False
    ImageIndex = 10
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 3
    Toggle = False
    OnClick = PriorButtonClick
  end
  object NextButton: TImageButton
    Left = 136
    Top = 176
    Width = 27
    Height = 26
    AllowFocus = False
    Anchors = [akRight, akBottom]
    AutoSize = True
    AutoPopup = True
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    Enabled = False
    FocusedRect = False
    ImageIndex = 11
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 4
    Toggle = False
    OnClick = NextButtonClick
  end
  object MoveRightButton: TImageButton
    Left = 160
    Top = 176
    Width = 27
    Height = 26
    AllowFocus = False
    Anchors = [akRight, akBottom]
    AutoSize = True
    AutoPopup = True
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    Enabled = False
    FocusedRect = False
    ImageIndex = 15
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 5
    Toggle = False
    OnClick = MoveRightButtonClick
  end
  object PhotoButton: TImageButton
    Left = 132
    Top = 4
    Width = 27
    Height = 26
    AllowFocus = False
    Anchors = [akTop, akRight]
    AutoSize = True
    AutoPopup = True
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    FocusedRect = False
    ImageIndex = 89
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 0
    Toggle = False
    OnClick = PhotoButtonClick
  end
  object ClearButon: TImageButton
    Left = 160
    Top = 4
    Width = 27
    Height = 26
    AllowFocus = False
    Anchors = [akTop, akRight]
    AutoSize = True
    AutoPopup = True
    CaptionPosition = cpHidden
    Color = clBtnFace
    Down = False
    Enabled = False
    FocusedRect = False
    ImageIndex = 33
    Images = ResourceModule.GlassImages
    Kind = bkButton
    Locked = False
    OwnerDrawn = False
    ParentColor = False
    SharedImages = False
    Style = bsTransparent
    TabOrder = 1
    Toggle = False
    OnClick = ClearButonClick
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 8
    Top = 136
  end
end
