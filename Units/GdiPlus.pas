(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  Private Build Release December 2008                 *)
(*                                                      *)
(********************************************************)

unit GdiPlus;

interface

{$I CODEBOT.INC}
{$ALIGN ON}
{$MINENUMSIZE 4}

uses
  Windows, ActiveX;

type
  Int16 = type Smallint;
  UInt16 = type Word;
  PUInt16 = ^UInt16;
  UInt32 = type Cardinal;

{ IDirectDrawSurface7 stub }

  IDirectDrawSurface7 = interface
    ['{06675A80-3B9B-11D2-B92F-00609797EA5B}']
  end;

const
  FlatnessDefault = 0.25;

type
  GraphicsState = UInt;
  GraphicsContainer = UInt;
  FillMode = (
    FillModeAlternate,
    FillModeWinding);
  TFillMode = FillMode;
{$IFDEF D6_UP}
  QualityMode = (
    QualityModeInvalid = -1,
    QualityModeDefault = 0,
    QualityModeLow = 1,
    QualityModeHigh = 2);
  TQualityMode = QualityMode;
{$ELSE}
  QualityMode = Integer;

const
  QualityModeInvalid = -1;
  QualityModeDefault = 0;
  QualityModeLow = 1;
  QualityModeHigh = 2;
{$ENDIF}
type
  CompositingMode = (
    CompositingModeSourceOver,
    CompositingModeSourceCopy);
  TCompositingMode = CompositingMode;
{$IFDEF D6_UP}
  CompositingQuality = (
    CompositingQualityInvalid = Ord(QualityModeInvalid),
    CompositingQualityDefault = Ord(QualityModeDefault),
    CompositingQualityHighSpeed = Ord(QualityModeLow),
    CompositingQualityHighQuality = Ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear);
  TCompositingQuality = CompositingQuality;
{$ELSE}
  CompositingQuality = Integer;

const
  CompositingQualityInvalid = QualityModeInvalid;
  CompositingQualityDefault = QualityModeDefault;
  CompositingQualityHighSpeed = QualityModeLow;
  CompositingQualityHighQuality = QualityModeHigh;
  CompositingQualityGammaCorrected = 3;
  CompositingQualityAssumeLinear = 4;

type
  TCompositingQuality = CompositingQuality;
{$ENDIF}
  Unit_ = (
    UnitWorld,
    UnitDisplay,
    UnitPixel,
    UnitPoint,
    UnitInch,
    UnitDocument,
    UnitMillimeter);
  TUnit = Unit_;
{$IFDEF D6_UP}
  MetafileFrameUnit = (
    MetafileFrameUnitPixel = Ord(UnitPixel),
    MetafileFrameUnitPoint = Ord(UnitPoint),
    MetafileFrameUnitInch = Ord(UnitInch),
    MetafileFrameUnitDocument = Ord(UnitDocument),
    MetafileFrameUnitMillimeter = Ord(UnitMillimeter),
    MetafileFrameUnitGdi);
  TMetafileFrameUnit = MetafileFrameUnit;
{$ELSE}
  MetafileFrameUnit = Integer;

const
  MetafileFrameUnitPixel = 2;
  MetafileFrameUnitPoint = 3;
  MetafileFrameUnitInch = 4;
  MetafileFrameUnitDocument = 5;
  MetafileFrameUnitMillimeter = 6;
  MetafileFrameUnitGdi = 7;

type
  TMetafileFrameUnit = MetafileFrameUnit;
{$ENDIF}
  CoordinateSpace = (
    CoordinateSpaceWorld,
    CoordinateSpacePage,
    CoordinateSpaceDevice);
  TCoordinateSpace = CoordinateSpace;
  WrapMode = (
    WrapModeTile,
    WrapModeTileFlipX,
    WrapModeTileFlipY,
    WrapModeTileFlipXY,
    WrapModeClamp);
  TWrapMode = WrapMode;
  HatchStyle = (
    HatchStyleHorizontal,
    HatchStyleVertical,
    HatchStyleForwardDiagonal,
    HatchStyleBackwardDiagonal,
    HatchStyleCross,
    HatchStyleDiagonalCross,
    HatchStyle05Percent,
    HatchStyle10Percent,
    HatchStyle20Percent,
    HatchStyle25Percent,
    HatchStyle30Percent,
    HatchStyle40Percent,
    HatchStyle50Percent,
    HatchStyle60Percent,
    HatchStyle70Percent,
    HatchStyle75Percent,
    HatchStyle80Percent,
    HatchStyle90Percent,
    HatchStyleLightDownwardDiagonal,
    HatchStyleLightUpwardDiagonal,
    HatchStyleDarkDownwardDiagonal,
    HatchStyleDarkUpwardDiagonal,
    HatchStyleWideDownwardDiagonal,
    HatchStyleWideUpwardDiagonal,
    HatchStyleLightVertical,
    HatchStyleLightHorizontal,
    HatchStyleNarrowVertical,
    HatchStyleNarrowHorizontal,
    HatchStyleDarkVertical,
    HatchStyleDarkHorizontal,
    HatchStyleDashedDownwardDiagonal,
    HatchStyleDashedUpwardDiagonal,
    HatchStyleDashedHorizontal,
    HatchStyleDashedVertical,
    HatchStyleSmallConfetti,
    HatchStyleLargeConfetti,
    HatchStyleZigZag,
    HatchStyleWave,
    HatchStyleDiagonalBrick,
    HatchStyleHorizontalBrick,
    HatchStyleWeave,
    HatchStylePlaid,
    HatchStyleDivot,
    HatchStyleDottedGrid,
    HatchStyleDottedDiamond,
    HatchStyleShingle,
    HatchStyleTrellis,
    HatchStyleSphere,
    HatchStyleSmallGrid,
    HatchStyleSmallCheckerBoard,
    HatchStyleLargeCheckerBoard,
    HatchStyleOutlinedDiamond,
    HatchStyleSolidDiamond,
    HatchStyleTotal);

const
  HatchStyleLargeGrid = HatchStyleCross;
  HatchStyleMin = HatchStyleHorizontal;
  HatchStyleMax = HatchStyleSolidDiamond;

type
  THatchStyle = HatchStyle;
  DashStyle = (
    DashStyleSolid,
    DashStyleDash,
    DashStyleDot,
    DashStyleDashDot,
    DashStyleDashDotDot,
    DashStyleCustom);
  TDashStyle = DashStyle;
{$IFDEF D6_UP}
  DashCap = (
    DashCapFlat = 0,
    DashCapRound = 2,
    DashCapTriangle = 3);
  TDashCap = DashCap;
{$ELSE}
  DashCap = Integer;

const
  DashCapFlat = 0;
  DashCapRound = 2;
  DashCapTriangle = 3;

type
  TDashCap = DashCap;
{$ENDIF}
{$IFDEF D6_UP}
  LineCap = (
    LineCapFlat = 0,
    LineCapSquare = 1,
    LineCapRound = 2,
    LineCapTriangle = 3,
    LineCapNoAnchor = $10,
    LineCapSquareAnchor = $11,
    LineCapRoundAnchor = $12,
    LineCapDiamondAnchor = $13,
    LineCapArrowAnchor = $14,
    LineCapCustom = $FF,
    LineCapAnchorMask = $F0);
  TLineCap = LineCap;
{$ELSE}
  LineCap = Integer;

const
  LineCapFlat = 0;
  LineCapSquare = 1;
  LineCapRound = 2;
  LineCapTriangle = 3;
  LineCapNoAnchor = $10;
  LineCapSquareAnchor = $11;
  LineCapRoundAnchor = $12;
  LineCapDiamondAnchor = $13;
  LineCapArrowAnchor = $14;
  LineCapCustom = $FF;
  LineCapAnchorMask = $F0;

type
  TLineCap = LineCap;
{$ENDIF}
  CustomLineCapType = (
    CustomLineCapTypeDefault,
    CustomLineCapTypeAdjustableArrow);
  TCustomLineCapType = CustomLineCapType;
  LineJoin = (
    LineJoinMiter,
    LineJoinBevel,
    LineJoinRound,
    LineJoinMiterClipped);
  TLineJoin = LineJoin;
{$IFDEF D6_UP}
{$Z1}
  PathPointType = (
    PathPointTypeStart = $00,
    PathPointTypeLine = $01,
    PathPointTypeBezier = $03,
    PathPointTypePathTypeMask = $07,
    PathPointTypeDashMode = $10,
    PathPointTypePathMarker = $20,
    PathPointTypeCloseSubpath = $80,
    PathPointTypeBezier3 = $03);
  TPathPointType = PathPointType;
{$Z4}
{$ELSE}
  PathPointType = Byte;

const
  PathPointTypeStart: Byte = $00;
  PathPointTypeLine: Byte = $01;
  PathPointTypeBezier: Byte = $03;
  PathPointTypePathTypeMask: Byte = $07;
  PathPointTypeDashMode: Byte = $10;
  PathPointTypePathMarker: Byte = $20;
  PathPointTypeCloseSubpath: Byte = $80;
  PathPointTypeBezier3: Byte = $03;

type
  TPathPointType = PathPointType;
{$ENDIF}
  WarpMode = (
    WarpModePerspective,
    WarpModeBilinear);
  TWarpMode = WarpMode;
  LinearGradientMode = (
    LinearGradientModeHorizontal,
    LinearGradientModeVertical,
    LinearGradientModeForwardDiagonal,
    LinearGradientModeBackwardDiagonal);
  TLinearGradientMode = LinearGradientMode;
  CombineMode = (
    CombineModeReplace,
    CombineModeIntersect,
    CombineModeUnion,
    CombineModeXor,
    CombineModeExclude,
    CombineModeComplement);
  TCombineMode = CombineMode;
  ImageType = (
    ImageTypeUnknown,
    ImageTypeBitmap,
    ImageTypeMetafile);
  TImageType = ImageType;
{$IFDEF D6_UP}
  InterpolationMode = (
    InterpolationModeInvalid = Ord(QualityModeInvalid),
    InterpolationModeDefault = Ord(QualityModeDefault),
    InterpolationModeLowQuality = Ord(QualityModeLow),
    InterpolationModeHighQuality = Ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic);
  TInterpolationMode = InterpolationMode;
{$ELSE}
  InterpolationMode = Integer;

const
  InterpolationModeInvalid = QualityModeInvalid;
  InterpolationModeDefault = QualityModeDefault;
  InterpolationModeLowQuality = QualityModeLow;
  InterpolationModeHighQuality = QualityModeHigh;
  InterpolationModeBilinear = 3;
  InterpolationModeBicubic = 4;
  InterpolationModeNearestNeighbor = 5;
  InterpolationModeHighQualityBilinear = 6;
  InterpolationModeHighQualityBicubic = 7;

type
  TInterpolationMode = InterpolationMode;
{$ENDIF}
  PenAlignment = (
    PenAlignmentCenter,
    PenAlignmentInset);
  TPenAlignment = PenAlignment;
  BrushType = (
    BrushTypeSolidColor,
    BrushTypeHatchFill,
    BrushTypeTextureFill,
    BrushTypePathGradient,
    BrushTypeLinearGradient);
  TBrushType = BrushType;
{$IFDEF D6_UP}
  PenType = (
    PenTypeSolidColor = Ord(BrushTypeSolidColor),
    PenTypeHatchFill = Ord(BrushTypeHatchFill),
    PenTypeTextureFill = Ord(BrushTypeTextureFill),
    PenTypePathGradient = Ord(BrushTypePathGradient),
    PenTypeLinearGradient = Ord(BrushTypeLinearGradient),
    PenTypeUnknown = -1);
  TPenType = PenType;
{$ELSE}
  PenType = Integer;

const
  PenTypeSolidColor = 0;
  PenTypeHatchFill = 1;
  PenTypeTextureFill = 2;
  PenTypePathGradient = 3;
  PenTypeLinearGradient = 4;
  PenTypeUnknown = -1;

type
  TPenType = PenType;
{$ENDIF}
  MatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend);
  TMatrixOrder = MatrixOrder;
  GenericFontFamily = (
    GenericFontFamilySerif,
    GenericFontFamilySansSerif,
    GenericFontFamilyMonospace);
  TGenericFontFamily = GenericFontFamily;

type
  FontStyle = Integer;

const
  FontStyleRegular = Integer(0);
  FontStyleBold = Integer(1);
  FontStyleItalic = Integer(2);
  FontStyleBoldItalic = Integer(3);
  FontStyleUnderline = Integer(4);
  FontStyleStrikeout = Integer(8);

type
  TFontStyle = FontStyle;

{$IFDEF D6_UP}
  SmoothingMode = (
    SmoothingModeInvalid = Ord(QualityModeInvalid),
    SmoothingModeDefault = Ord(QualityModeDefault),
    SmoothingModeHighSpeed = Ord(QualityModeLow),
    SmoothingModeHighQuality = Ord(QualityModeHigh),
    SmoothingModeNone,
    SmoothingModeAntiAlias);
  TSmoothingMode = SmoothingMode;
{$ELSE}
  SmoothingMode = Integer;

const
  SmoothingModeInvalid = QualityModeInvalid;
  SmoothingModeDefault = QualityModeDefault;
  SmoothingModeHighSpeed = QualityModeLow;
  SmoothingModeHighQuality = QualityModeHigh;
  SmoothingModeNone = 3;
  SmoothingModeAntiAlias = 4;

type
  TSmoothingMode = SmoothingMode;
{$ENDIF}
{$IFDEF D6_UP}
  PixelOffsetMode = (
    PixelOffsetModeInvalid = Ord(QualityModeInvalid),
    PixelOffsetModeDefault = Ord(QualityModeDefault),
    PixelOffsetModeHighSpeed = Ord(QualityModeLow),
    PixelOffsetModeHighQuality = Ord(QualityModeHigh),
    PixelOffsetModeNone,
    PixelOffsetModeHalf);
  TPixelOffsetMode = PixelOffsetMode;
{$ELSE}
  PixelOffsetMode = Integer;

const
  PixelOffsetModeInvalid = QualityModeInvalid;
  PixelOffsetModeDefault = QualityModeDefault;
  PixelOffsetModeHighSpeed = QualityModeLow;
  PixelOffsetModeHighQuality = QualityModeHigh;
  PixelOffsetModeNone = 3;
  PixelOffsetModeHalf = 4;

type
  TPixelOffsetMode = PixelOffsetMode;
{$ENDIF}
  TextRenderingHint = (
    TextRenderingHintSystemDefault,
    TextRenderingHintSingleBitPerPixelGridFit,
    TextRenderingHintSingleBitPerPixel,
    TextRenderingHintAntiAliasGridFit,
    TextRenderingHintAntiAlias,
    TextRenderingHintClearTypeGridFit);
  TTextRenderingHint = TextRenderingHint;
  MetafileType = (
    MetafileTypeInvalid,
    MetafileTypeWmf,
    MetafileTypeWmfPlaceable,
    MetafileTypeEmf,
    MetafileTypeEmfPlusOnly,
    MetafileTypeEmfPlusDual);
  TMetafileType = MetafileType;
{$IFDEF D6_UP}
  EmfType = (
    EmfTypeEmfOnly = Ord(MetafileTypeEmf),
    EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly),
    EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual));
  TEmfType = EmfType;
{$ELSE}
  EmfType = Integer;

const
  EmfTypeEmfOnly = Ord(MetafileTypeEmf);
  EmfTypeEmfPlusOnly = Ord(MetafileTypeEmfPlusOnly);
  EmfTypeEmfPlusDual = Ord(MetafileTypeEmfPlusDual);

type
  TEmfType = EmfType;
{$ENDIF}
  ObjectType = (
    ObjectTypeInvalid,
    ObjectTypeBrush,
    ObjectTypePen,
    ObjectTypePath,
    ObjectTypeRegion,
    ObjectTypeImage,
    ObjectTypeFont,
    ObjectTypeStringFormat,
    ObjectTypeImageAttributes,
    ObjectTypeCustomLineCap);
  TObjectType = ObjectType;

const
  ObjectTypeMax = ObjectTypeCustomLineCap;
  ObjectTypeMin = ObjectTypeBrush;

const
  GDIP_EMFPLUS_RECORD_BASE = $00004000;
  GDIP_WMF_RECORD_BASE = $00010000;
{$IFDEF D6_UP}
type
  EmfPlusRecordType = (
    WmfRecordTypeSetBkColor = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetBkMode = (META_SETBKMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapMode = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetROP2 = (META_SETROP2 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetRelAbs = (META_SETRELABS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPolyFillMode = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetStretchBltMode = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextCharExtra = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextColor = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextJustification = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowOrg = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetWindowExt = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportOrg = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetViewportExt = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetWindowOrg = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleWindowExt = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetViewportOrg = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeScaleViewportExt = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeLineTo = (META_LINETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeMoveTo = (META_MOVETO or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExcludeClipRect = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeIntersectClipRect = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeArc = (META_ARC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEllipse = (META_ELLIPSE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFloodFill = (META_FLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePie = (META_PIE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRectangle = (META_RECTANGLE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRoundRect = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePatBlt = (META_PATBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSaveDC = (META_SAVEDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPixel = (META_SETPIXEL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeOffsetClipRgn = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeTextOut = (META_TEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeBitBlt = (META_BITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchBlt = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolygon = (META_POLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyline = (META_POLYLINE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEscape = (META_ESCAPE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRestoreDC = (META_RESTOREDC or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFillRegion = (META_FILLREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeFrameRegion = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeInvertRegion = (META_INVERTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePaintRegion = (META_PAINTREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectClipRegion = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectObject = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetTextAlign = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDrawText = ($062F or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeChord = (META_CHORD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetMapperFlags = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtTextOut = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetDIBToDev = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSelectPalette = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeRealizePalette = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAnimatePalette = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetPalEntries = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE),
    WmfRecordTypePolyPolygon = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResizePalette = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBBitBlt = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBStretchBlt = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDIBCreatePatternBrush = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStretchDIB = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeExtFloodFill = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeSetLayout = ($0149 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeResetDC = ($014C or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStartDoc = ($014D or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeStartPage = ($004F or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEndPage = ($0050 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeAbortDoc = ($0052 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeEndDoc = ($005E or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeDeleteObject = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePalette = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrush = ($00F8 or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePatternBrush = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreatePenIndirect = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateFontIndirect = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBrushIndirect = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmapIndirect = ($02FD or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateBitmap = ($06FE or GDIP_WMF_RECORD_BASE),
    WmfRecordTypeCreateRegion = (META_CREATEREGION or GDIP_WMF_RECORD_BASE),
    EmfRecordTypeHeader = EMR_HEADER,
    EmfRecordTypePolyBezier = EMR_POLYBEZIER,
    EmfRecordTypePolygon = EMR_POLYGON,
    EmfRecordTypePolyline = EMR_POLYLINE,
    EmfRecordTypePolyBezierTo = EMR_POLYBEZIERTO,
    EmfRecordTypePolyLineTo = EMR_POLYLINETO,
    EmfRecordTypePolyPolyline = EMR_POLYPOLYLINE,
    EmfRecordTypePolyPolygon = EMR_POLYPOLYGON,
    EmfRecordTypeSetWindowExtEx = EMR_SETWINDOWEXTEX,
    EmfRecordTypeSetWindowOrgEx = EMR_SETWINDOWORGEX,
    EmfRecordTypeSetViewportExtEx = EMR_SETVIEWPORTEXTEX,
    EmfRecordTypeSetViewportOrgEx = EMR_SETVIEWPORTORGEX,
    EmfRecordTypeSetBrushOrgEx = EMR_SETBRUSHORGEX,
    EmfRecordTypeEOF = EMR_EOF,
    EmfRecordTypeSetPixelV = EMR_SETPIXELV,
    EmfRecordTypeSetMapperFlags = EMR_SETMAPPERFLAGS,
    EmfRecordTypeSetMapMode = EMR_SETMAPMODE,
    EmfRecordTypeSetBkMode = EMR_SETBKMODE,
    EmfRecordTypeSetPolyFillMode = EMR_SETPOLYFILLMODE,
    EmfRecordTypeSetROP2 = EMR_SETROP2,
    EmfRecordTypeSetStretchBltMode = EMR_SETSTRETCHBLTMODE,
    EmfRecordTypeSetTextAlign = EMR_SETTEXTALIGN,
    EmfRecordTypeSetColorAdjustment = EMR_SETCOLORADJUSTMENT,
    EmfRecordTypeSetTextColor = EMR_SETTEXTCOLOR,
    EmfRecordTypeSetBkColor = EMR_SETBKCOLOR,
    EmfRecordTypeOffsetClipRgn = EMR_OFFSETCLIPRGN,
    EmfRecordTypeMoveToEx = EMR_MOVETOEX,
    EmfRecordTypeSetMetaRgn = EMR_SETMETARGN,
    EmfRecordTypeExcludeClipRect = EMR_EXCLUDECLIPRECT,
    EmfRecordTypeIntersectClipRect = EMR_INTERSECTCLIPRECT,
    EmfRecordTypeScaleViewportExtEx = EMR_SCALEVIEWPORTEXTEX,
    EmfRecordTypeScaleWindowExtEx = EMR_SCALEWINDOWEXTEX,
    EmfRecordTypeSaveDC = EMR_SAVEDC,
    EmfRecordTypeRestoreDC = EMR_RESTOREDC,
    EmfRecordTypeSetWorldTransform = EMR_SETWORLDTRANSFORM,
    EmfRecordTypeModifyWorldTransform = EMR_MODIFYWORLDTRANSFORM,
    EmfRecordTypeSelectObject = EMR_SELECTOBJECT,
    EmfRecordTypeCreatePen = EMR_CREATEPEN,
    EmfRecordTypeCreateBrushIndirect = EMR_CREATEBRUSHINDIRECT,
    EmfRecordTypeDeleteObject = EMR_DELETEOBJECT,
    EmfRecordTypeAngleArc = EMR_ANGLEARC,
    EmfRecordTypeEllipse = EMR_ELLIPSE,
    EmfRecordTypeRectangle = EMR_RECTANGLE,
    EmfRecordTypeRoundRect = EMR_ROUNDRECT,
    EmfRecordTypeArc = EMR_ARC,
    EmfRecordTypeChord = EMR_CHORD,
    EmfRecordTypePie = EMR_PIE,
    EmfRecordTypeSelectPalette = EMR_SELECTPALETTE,
    EmfRecordTypeCreatePalette = EMR_CREATEPALETTE,
    EmfRecordTypeSetPaletteEntries = EMR_SETPALETTEENTRIES,
    EmfRecordTypeResizePalette = EMR_RESIZEPALETTE,
    EmfRecordTypeRealizePalette = EMR_REALIZEPALETTE,
    EmfRecordTypeExtFloodFill = EMR_EXTFLOODFILL,
    EmfRecordTypeLineTo = EMR_LINETO,
    EmfRecordTypeArcTo = EMR_ARCTO,
    EmfRecordTypePolyDraw = EMR_POLYDRAW,
    EmfRecordTypeSetArcDirection = EMR_SETARCDIRECTION,
    EmfRecordTypeSetMiterLimit = EMR_SETMITERLIMIT,
    EmfRecordTypeBeginPath = EMR_BEGINPATH,
    EmfRecordTypeEndPath = EMR_ENDPATH,
    EmfRecordTypeCloseFigure = EMR_CLOSEFIGURE,
    EmfRecordTypeFillPath = EMR_FILLPATH,
    EmfRecordTypeStrokeAndFillPath = EMR_STROKEANDFILLPATH,
    EmfRecordTypeStrokePath = EMR_STROKEPATH,
    EmfRecordTypeFlattenPath = EMR_FLATTENPATH,
    EmfRecordTypeWidenPath = EMR_WIDENPATH,
    EmfRecordTypeSelectClipPath = EMR_SELECTCLIPPATH,
    EmfRecordTypeAbortPath = EMR_ABORTPATH,
    EmfRecordTypeReserved_069 = 69,
    EmfRecordTypeGdiComment = EMR_GDICOMMENT,
    EmfRecordTypeFillRgn = EMR_FILLRGN,
    EmfRecordTypeFrameRgn = EMR_FRAMERGN,
    EmfRecordTypeInvertRgn = EMR_INVERTRGN,
    EmfRecordTypePaintRgn = EMR_PAINTRGN,
    EmfRecordTypeExtSelectClipRgn = EMR_EXTSELECTCLIPRGN,
    EmfRecordTypeBitBlt = EMR_BITBLT,
    EmfRecordTypeStretchBlt = EMR_STRETCHBLT,
    EmfRecordTypeMaskBlt = EMR_MASKBLT,
    EmfRecordTypePlgBlt = EMR_PLGBLT,
    EmfRecordTypeSetDIBitsToDevice = EMR_SETDIBITSTODEVICE,
    EmfRecordTypeStretchDIBits = EMR_STRETCHDIBITS,
    EmfRecordTypeExtCreateFontIndirect = EMR_EXTCREATEFONTINDIRECTW,
    EmfRecordTypeExtTextOutA = EMR_EXTTEXTOUTA,
    EmfRecordTypeExtTextOutW = EMR_EXTTEXTOUTW,
    EmfRecordTypePolyBezier16 = EMR_POLYBEZIER16,
    EmfRecordTypePolygon16 = EMR_POLYGON16,
    EmfRecordTypePolyline16 = EMR_POLYLINE16,
    EmfRecordTypePolyBezierTo16 = EMR_POLYBEZIERTO16,
    EmfRecordTypePolylineTo16 = EMR_POLYLINETO16,
    EmfRecordTypePolyPolyline16 = EMR_POLYPOLYLINE16,
    EmfRecordTypePolyPolygon16 = EMR_POLYPOLYGON16,
    EmfRecordTypePolyDraw16 = EMR_POLYDRAW16,
    EmfRecordTypeCreateMonoBrush = EMR_CREATEMONOBRUSH,
    EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT,
    EmfRecordTypeExtCreatePen = EMR_EXTCREATEPEN,
    EmfRecordTypePolyTextOutA = EMR_POLYTEXTOUTA,
    EmfRecordTypePolyTextOutW = EMR_POLYTEXTOUTW,
    EmfRecordTypeSetICMMode = 98,
    EmfRecordTypeCreateColorSpace = 99,
    EmfRecordTypeSetColorSpace = 100,
    EmfRecordTypeDeleteColorSpace = 101,
    EmfRecordTypeGLSRecord = 102,
    EmfRecordTypeGLSBoundedRecord = 103,
    EmfRecordTypePixelFormat = 104,
    EmfRecordTypeDrawEscape = 105,
    EmfRecordTypeExtEscape = 106,
    EmfRecordTypeStartDoc = 107,
    EmfRecordTypeSmallTextOut = 108,
    EmfRecordTypeForceUFIMapping = 109,
    EmfRecordTypeNamedEscape = 110,
    EmfRecordTypeColorCorrectPalette = 111,
    EmfRecordTypeSetICMProfileA = 112,
    EmfRecordTypeSetICMProfileW = 113,
    EmfRecordTypeAlphaBlend = 114,
    EmfRecordTypeSetLayout = 115,
    EmfRecordTypeTransparentBlt = 116,
    EmfRecordTypeReserved_117 = 117,
    EmfRecordTypeGradientFill = 118,
    EmfRecordTypeSetLinkedUFIs = 119,
    EmfRecordTypeSetTextJustification = 120,
    EmfRecordTypeColorMatchToTargetW = 121,
    EmfRecordTypeCreateColorSpaceW = 122,
    EmfRecordTypeMax = 122,
    EmfRecordTypeMin = 1,
    EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE,
    EmfPlusRecordTypeHeader,
    EmfPlusRecordTypeEndOfFile,
    EmfPlusRecordTypeComment,
    EmfPlusRecordTypeGetDC,
    EmfPlusRecordTypeMultiFormatStart,
    EmfPlusRecordTypeMultiFormatSection,
    EmfPlusRecordTypeMultiFormatEnd,
    EmfPlusRecordTypeObject,
    EmfPlusRecordTypeClear,
    EmfPlusRecordTypeFillRects,
    EmfPlusRecordTypeDrawRects,
    EmfPlusRecordTypeFillPolygon,
    EmfPlusRecordTypeDrawLines,
    EmfPlusRecordTypeFillEllipse,
    EmfPlusRecordTypeDrawEllipse,
    EmfPlusRecordTypeFillPie,
    EmfPlusRecordTypeDrawPie,
    EmfPlusRecordTypeDrawArc,
    EmfPlusRecordTypeFillRegion,
    EmfPlusRecordTypeFillPath,
    EmfPlusRecordTypeDrawPath,
    EmfPlusRecordTypeFillClosedCurve,
    EmfPlusRecordTypeDrawClosedCurve,
    EmfPlusRecordTypeDrawCurve,
    EmfPlusRecordTypeDrawBeziers,
    EmfPlusRecordTypeDrawImage,
    EmfPlusRecordTypeDrawImagePoints,
    EmfPlusRecordTypeDrawString,
    EmfPlusRecordTypeSetRenderingOrigin,
    EmfPlusRecordTypeSetAntiAliasMode,
    EmfPlusRecordTypeSetTextRenderingHint,
    EmfPlusRecordTypeSetTextContrast,
    EmfPlusRecordTypeSetInterpolationMode,
    EmfPlusRecordTypeSetPixelOffsetMode,
    EmfPlusRecordTypeSetCompositingMode,
    EmfPlusRecordTypeSetCompositingQuality,
    EmfPlusRecordTypeSave,
    EmfPlusRecordTypeRestore,
    EmfPlusRecordTypeBeginContainer,
    EmfPlusRecordTypeBeginContainerNoParams,
    EmfPlusRecordTypeEndContainer,
    EmfPlusRecordTypeSetWorldTransform,
    EmfPlusRecordTypeResetWorldTransform,
    EmfPlusRecordTypeMultiplyWorldTransform,
    EmfPlusRecordTypeTranslateWorldTransform,
    EmfPlusRecordTypeScaleWorldTransform,
    EmfPlusRecordTypeRotateWorldTransform,
    EmfPlusRecordTypeSetPageTransform,
    EmfPlusRecordTypeResetClip,
    EmfPlusRecordTypeSetClipRect,
    EmfPlusRecordTypeSetClipPath,
    EmfPlusRecordTypeSetClipRegion,
    EmfPlusRecordTypeOffsetClip,
    EmfPlusRecordTypeDrawDriverString,
    EmfPlusRecordTotal,
    EmfPlusRecordTypeMax = EMFPLUSRECORDTOTAL - 1,
    EmfPlusRecordTypeMin = EMFPLUSRECORDTYPEHEADER);
  TEmfPlusRecordType = EmfPlusRecordType;
{$ELSE}
type
  EmfPlusRecordType = Integer;

const
  WmfRecordTypeSetBkColor = (META_SETBKCOLOR or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetBkMode = (META_SETBKMODE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetMapMode = (META_SETMAPMODE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetROP2 = (META_SETROP2 or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetRelAbs = (META_SETRELABS or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetPolyFillMode = (META_SETPOLYFILLMODE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetStretchBltMode = (META_SETSTRETCHBLTMODE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetTextCharExtra = (META_SETTEXTCHAREXTRA or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetTextColor = (META_SETTEXTCOLOR or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetTextJustification = (META_SETTEXTJUSTIFICATION or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetWindowOrg = (META_SETWINDOWORG or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetWindowExt = (META_SETWINDOWEXT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetViewportOrg = (META_SETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetViewportExt = (META_SETVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeOffsetWindowOrg = (META_OFFSETWINDOWORG or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeScaleWindowExt = (META_SCALEWINDOWEXT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeOffsetViewportOrg = (META_OFFSETVIEWPORTORG or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeScaleViewportExt = (META_SCALEVIEWPORTEXT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeLineTo = (META_LINETO or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeMoveTo = (META_MOVETO or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeExcludeClipRect = (META_EXCLUDECLIPRECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeIntersectClipRect = (META_INTERSECTCLIPRECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeArc = (META_ARC or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeEllipse = (META_ELLIPSE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeFloodFill = (META_FLOODFILL or GDIP_WMF_RECORD_BASE);
  WmfRecordTypePie = (META_PIE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeRectangle = (META_RECTANGLE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeRoundRect = (META_ROUNDRECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypePatBlt = (META_PATBLT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSaveDC = (META_SAVEDC or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetPixel = (META_SETPIXEL or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeOffsetClipRgn = (META_OFFSETCLIPRGN or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeTextOut = (META_TEXTOUT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeBitBlt = (META_BITBLT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeStretchBlt = (META_STRETCHBLT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypePolygon = (META_POLYGON or GDIP_WMF_RECORD_BASE);
  WmfRecordTypePolyline = (META_POLYLINE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeEscape = (META_ESCAPE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeRestoreDC = (META_RESTOREDC or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeFillRegion = (META_FILLREGION or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeFrameRegion = (META_FRAMEREGION or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeInvertRegion = (META_INVERTREGION or GDIP_WMF_RECORD_BASE);
  WmfRecordTypePaintRegion = (META_PAINTREGION or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSelectClipRegion = (META_SELECTCLIPREGION or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSelectObject = (META_SELECTOBJECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetTextAlign = (META_SETTEXTALIGN or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeDrawText = ($062F or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeChord = (META_CHORD or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetMapperFlags = (META_SETMAPPERFLAGS or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeExtTextOut = (META_EXTTEXTOUT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetDIBToDev = (META_SETDIBTODEV or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSelectPalette = (META_SELECTPALETTE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeRealizePalette = (META_REALIZEPALETTE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeAnimatePalette = (META_ANIMATEPALETTE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetPalEntries = (META_SETPALENTRIES or GDIP_WMF_RECORD_BASE);
  WmfRecordTypePolyPolygon = (META_POLYPOLYGON or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeResizePalette = (META_RESIZEPALETTE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeDIBBitBlt = (META_DIBBITBLT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeDIBStretchBlt = (META_DIBSTRETCHBLT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeDIBCreatePatternBrush = (META_DIBCREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeStretchDIB = (META_STRETCHDIB or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeExtFloodFill = (META_EXTFLOODFILL or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeSetLayout = ($0149 or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeResetDC = ($014C or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeStartDoc = ($014D or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeStartPage = ($004F or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeEndPage = ($0050 or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeAbortDoc = ($0052 or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeEndDoc = ($005E or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeDeleteObject = (META_DELETEOBJECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreatePalette = (META_CREATEPALETTE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreateBrush = ($00F8 or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreatePatternBrush = (META_CREATEPATTERNBRUSH or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreatePenIndirect = (META_CREATEPENINDIRECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreateFontIndirect = (META_CREATEFONTINDIRECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreateBrushIndirect = (META_CREATEBRUSHINDIRECT or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreateBitmapIndirect = ($02FD or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreateBitmap = ($06FE or GDIP_WMF_RECORD_BASE);
  WmfRecordTypeCreateRegion = (META_CREATEREGION or GDIP_WMF_RECORD_BASE);
  EmfRecordTypeHeader = EMR_HEADER;
  EmfRecordTypePolyBezier = EMR_POLYBEZIER;
  EmfRecordTypePolygon = EMR_POLYGON;
  EmfRecordTypePolyline = EMR_POLYLINE;
  EmfRecordTypePolyBezierTo = EMR_POLYBEZIERTO;
  EmfRecordTypePolyLineTo = EMR_POLYLINETO;
  EmfRecordTypePolyPolyline = EMR_POLYPOLYLINE;
  EmfRecordTypePolyPolygon = EMR_POLYPOLYGON;
  EmfRecordTypeSetWindowExtEx = EMR_SETWINDOWEXTEX;
  EmfRecordTypeSetWindowOrgEx = EMR_SETWINDOWORGEX;
  EmfRecordTypeSetViewportExtEx = EMR_SETVIEWPORTEXTEX;
  EmfRecordTypeSetViewportOrgEx = EMR_SETVIEWPORTORGEX;
  EmfRecordTypeSetBrushOrgEx = EMR_SETBRUSHORGEX;
  EmfRecordTypeEOF = EMR_EOF;
  EmfRecordTypeSetPixelV = EMR_SETPIXELV;
  EmfRecordTypeSetMapperFlags = EMR_SETMAPPERFLAGS;
  EmfRecordTypeSetMapMode = EMR_SETMAPMODE;
  EmfRecordTypeSetBkMode = EMR_SETBKMODE;
  EmfRecordTypeSetPolyFillMode = EMR_SETPOLYFILLMODE;
  EmfRecordTypeSetROP2 = EMR_SETROP2;
  EmfRecordTypeSetStretchBltMode = EMR_SETSTRETCHBLTMODE;
  EmfRecordTypeSetTextAlign = EMR_SETTEXTALIGN;
  EmfRecordTypeSetColorAdjustment = EMR_SETCOLORADJUSTMENT;
  EmfRecordTypeSetTextColor = EMR_SETTEXTCOLOR;
  EmfRecordTypeSetBkColor = EMR_SETBKColor;
  EmfRecordTypeOffsetClipRgn = EMR_OFFSETCLIPRGN;
  EmfRecordTypeMoveToEx = EMR_MOVETOEX;
  EmfRecordTypeSetMetaRgn = EMR_SETMETARGN;
  EmfRecordTypeExcludeClipRect = EMR_EXCLUDECLIPRECT;
  EmfRecordTypeIntersectClipRect = EMR_INTERSECTCLIPRECT;
  EmfRecordTypeScaleViewportExtEx = EMR_SCALEVIEWPORTEXTEX;
  EmfRecordTypeScaleWindowExtEx = EMR_SCALEWINDOWEXTEX;
  EmfRecordTypeSaveDC = EMR_SAVEDC;
  EmfRecordTypeRestoreDC = EMR_RESTOREDC;
  EmfRecordTypeSetWorldTransform = EMR_SETWORLDTRANSFORM;
  EmfRecordTypeModifyWorldTransform = EMR_MODIFYWORLDTRANSFORM;
  EmfRecordTypeSelectObject = EMR_SELECTOBJECT;
  EmfRecordTypeCreatePen = EMR_CREATEPEN;
  EmfRecordTypeCreateBrushIndirect = EMR_CREATEBRUSHINDIRECT;
  EmfRecordTypeDeleteObject = EMR_DELETEOBJECT;
  EmfRecordTypeAngleArc = EMR_ANGLEARC;
  EmfRecordTypeEllipse = EMR_ELLIPSE;
  EmfRecordTypeRectangle = EMR_RECTANGLE;
  EmfRecordTypeRoundRect = EMR_ROUNDRECT;
  EmfRecordTypeArc = EMR_ARC;
  EmfRecordTypeChord = EMR_CHORD;
  EmfRecordTypePie = EMR_PIE;
  EmfRecordTypeSelectPalette = EMR_SELECTPALETTE;
  EmfRecordTypeCreatePalette = EMR_CREATEPALETTE;
  EmfRecordTypeSetPaletteEntries = EMR_SETPALETTEENTRIES;
  EmfRecordTypeResizePalette = EMR_RESIZEPALETTE;
  EmfRecordTypeRealizePalette = EMR_REALIZEPALETTE;
  EmfRecordTypeExtFloodFill = EMR_EXTFLOODFILL;
  EmfRecordTypeLineTo = EMR_LINETO;
  EmfRecordTypeArcTo = EMR_ARCTO;
  EmfRecordTypePolyDraw = EMR_POLYDRAW;
  EmfRecordTypeSetArcDirection = EMR_SETARCDIRECTION;
  EmfRecordTypeSetMiterLimit = EMR_SETMITERLIMIT;
  EmfRecordTypeBeginPath = EMR_BEGINPATH;
  EmfRecordTypeEndPath = EMR_ENDPATH;
  EmfRecordTypeCloseFigure = EMR_CLOSEFIGURE;
  EmfRecordTypeFillPath = EMR_FILLPATH;
  EmfRecordTypeStrokeAndFillPath = EMR_STROKEANDFILLPATH;
  EmfRecordTypeStrokePath = EMR_STROKEPATH;
  EmfRecordTypeFlattenPath = EMR_FLATTENPATH;
  EmfRecordTypeWidenPath = EMR_WIDENPATH;
  EmfRecordTypeSelectClipPath = EMR_SELECTCLIPPATH;
  EmfRecordTypeAbortPath = EMR_ABORTPATH;
  EmfRecordTypeReserved_069 = 69;
  EmfRecordTypeGdiComment = EMR_GDICOMMENT;
  EmfRecordTypeFillRgn = EMR_FILLRGN;
  EmfRecordTypeFrameRgn = EMR_FRAMERGN;
  EmfRecordTypeInvertRgn = EMR_INVERTRGN;
  EmfRecordTypePaintRgn = EMR_PAINTRGN;
  EmfRecordTypeExtSelectClipRgn = EMR_EXTSELECTCLIPRGN;
  EmfRecordTypeBitBlt = EMR_BITBLT;
  EmfRecordTypeStretchBlt = EMR_STRETCHBLT;
  EmfRecordTypeMaskBlt = EMR_MASKBLT;
  EmfRecordTypePlgBlt = EMR_PLGBLT;
  EmfRecordTypeSetDIBitsToDevice = EMR_SETDIBITSTODEVICE;
  EmfRecordTypeStretchDIBits = EMR_STRETCHDIBITS;
  EmfRecordTypeExtCreateFontIndirect = EMR_EXTCREATEFONTINDIRECTW;
  EmfRecordTypeExtTextOutA = EMR_EXTTEXTOUTA;
  EmfRecordTypeExtTextOutW = EMR_EXTTEXTOUTW;
  EmfRecordTypePolyBezier16 = EMR_POLYBEZIER16;
  EmfRecordTypePolygon16 = EMR_POLYGON16;
  EmfRecordTypePolyline16 = EMR_POLYLINE16;
  EmfRecordTypePolyBezierTo16 = EMR_POLYBEZIERTO16;
  EmfRecordTypePolylineTo16 = EMR_POLYLINETO16;
  EmfRecordTypePolyPolyline16 = EMR_POLYPOLYLINE16;
  EmfRecordTypePolyPolygon16 = EMR_POLYPOLYGON16;
  EmfRecordTypePolyDraw16 = EMR_POLYDRAW16;
  EmfRecordTypeCreateMonoBrush = EMR_CREATEMONOBRUSH;
  EmfRecordTypeCreateDIBPatternBrushPt = EMR_CREATEDIBPATTERNBRUSHPT;
  EmfRecordTypeExtCreatePen = EMR_EXTCREATEPEN;
  EmfRecordTypePolyTextOutA = EMR_POLYTEXTOUTA;
  EmfRecordTypePolyTextOutW = EMR_POLYTEXTOUTW;
  EmfRecordTypeSetICMMode = 98;
  EmfRecordTypeCreateColorSpace = 99;
  EmfRecordTypeSetColorSpace = 100;
  EmfRecordTypeDeleteColorSpace = 101;
  EmfRecordTypeGLSRecord = 102;
  EmfRecordTypeGLSBoundedRecord = 103;
  EmfRecordTypePixelFormat = 104;
  EmfRecordTypeDrawEscape = 105;
  EmfRecordTypeExtEscape = 106;
  EmfRecordTypeStartDoc = 107;
  EmfRecordTypeSmallTextOut = 108;
  EmfRecordTypeForceUFIMapping = 109;
  EmfRecordTypeNamedEscape = 110;
  EmfRecordTypeColorCorrectPalette = 111;
  EmfRecordTypeSetICMProfileA = 112;
  EmfRecordTypeSetICMProfileW = 113;
  EmfRecordTypeAlphaBlend = 114;
  EmfRecordTypeSetLayout = 115;
  EmfRecordTypeTransparentBlt = 116;
  EmfRecordTypeReserved_117 = 117;
  EmfRecordTypeGradientFill = 118;
  EmfRecordTypeSetLinkedUFIs = 119;
  EmfRecordTypeSetTextJustification = 120;
  EmfRecordTypeColorMatchToTargetW = 121;
  EmfRecordTypeCreateColorSpaceW = 122;
  EmfRecordTypeMax = 122;
  EmfRecordTypeMin = 1;
  EmfPlusRecordTypeInvalid = GDIP_EMFPLUS_RECORD_BASE;
  EmfPlusRecordTypeHeader = GDIP_EMFPLUS_RECORD_BASE + 1;
  EmfPlusRecordTypeEndOfFile = GDIP_EMFPLUS_RECORD_BASE + 2;
  EmfPlusRecordTypeComment = GDIP_EMFPLUS_RECORD_BASE + 3;
  EmfPlusRecordTypeGetDC = GDIP_EMFPLUS_RECORD_BASE + 4;
  EmfPlusRecordTypeMultiFormatStart = GDIP_EMFPLUS_RECORD_BASE + 5;
  EmfPlusRecordTypeMultiFormatSection = GDIP_EMFPLUS_RECORD_BASE + 6;
  EmfPlusRecordTypeMultiFormatEnd = GDIP_EMFPLUS_RECORD_BASE + 7;
  EmfPlusRecordTypeObject = GDIP_EMFPLUS_RECORD_BASE + 8;
  EmfPlusRecordTypeClear = GDIP_EMFPLUS_RECORD_BASE + 9;
  EmfPlusRecordTypeFillRects = GDIP_EMFPLUS_RECORD_BASE + 10;
  EmfPlusRecordTypeDrawRects = GDIP_EMFPLUS_RECORD_BASE + 11;
  EmfPlusRecordTypeFillPolygon = GDIP_EMFPLUS_RECORD_BASE + 12;
  EmfPlusRecordTypeDrawLines = GDIP_EMFPLUS_RECORD_BASE + 13;
  EmfPlusRecordTypeFillEllipse = GDIP_EMFPLUS_RECORD_BASE + 14;
  EmfPlusRecordTypeDrawEllipse = GDIP_EMFPLUS_RECORD_BASE + 15;
  EmfPlusRecordTypeFillPie = GDIP_EMFPLUS_RECORD_BASE + 16;
  EmfPlusRecordTypeDrawPie = GDIP_EMFPLUS_RECORD_BASE + 17;
  EmfPlusRecordTypeDrawArc = GDIP_EMFPLUS_RECORD_BASE + 18;
  EmfPlusRecordTypeFillRegion = GDIP_EMFPLUS_RECORD_BASE + 19;
  EmfPlusRecordTypeFillPath = GDIP_EMFPLUS_RECORD_BASE + 20;
  EmfPlusRecordTypeDrawPath = GDIP_EMFPLUS_RECORD_BASE + 21;
  EmfPlusRecordTypeFillClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 22;
  EmfPlusRecordTypeDrawClosedCurve = GDIP_EMFPLUS_RECORD_BASE + 23;
  EmfPlusRecordTypeDrawCurve = GDIP_EMFPLUS_RECORD_BASE + 24;
  EmfPlusRecordTypeDrawBeziers = GDIP_EMFPLUS_RECORD_BASE + 25;
  EmfPlusRecordTypeDrawImage = GDIP_EMFPLUS_RECORD_BASE + 26;
  EmfPlusRecordTypeDrawImagePoints = GDIP_EMFPLUS_RECORD_BASE + 27;
  EmfPlusRecordTypeDrawString = GDIP_EMFPLUS_RECORD_BASE + 28;
  EmfPlusRecordTypeSetRenderingOrigin = GDIP_EMFPLUS_RECORD_BASE + 29;
  EmfPlusRecordTypeSetAntiAliasMode = GDIP_EMFPLUS_RECORD_BASE + 30;
  EmfPlusRecordTypeSetTextRenderingHint = GDIP_EMFPLUS_RECORD_BASE + 31;
  EmfPlusRecordTypeSetTextContrast = GDIP_EMFPLUS_RECORD_BASE + 32;
  EmfPlusRecordTypeSetInterpolationMode = GDIP_EMFPLUS_RECORD_BASE + 33;
  EmfPlusRecordTypeSetPixelOffsetMode = GDIP_EMFPLUS_RECORD_BASE + 34;
  EmfPlusRecordTypeSetCompositingMode = GDIP_EMFPLUS_RECORD_BASE + 35;
  EmfPlusRecordTypeSetCompositingQuality = GDIP_EMFPLUS_RECORD_BASE + 36;
  EmfPlusRecordTypeSave = GDIP_EMFPLUS_RECORD_BASE + 37;
  EmfPlusRecordTypeRestore = GDIP_EMFPLUS_RECORD_BASE + 38;
  EmfPlusRecordTypeBeginContainer = GDIP_EMFPLUS_RECORD_BASE + 39;
  EmfPlusRecordTypeBeginContainerNoParams = GDIP_EMFPLUS_RECORD_BASE + 40;
  EmfPlusRecordTypeEndContainer = GDIP_EMFPLUS_RECORD_BASE + 41;
  EmfPlusRecordTypeSetWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 42;
  EmfPlusRecordTypeResetWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 43;
  EmfPlusRecordTypeMultiplyWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 44;
  EmfPlusRecordTypeTranslateWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 45;
  EmfPlusRecordTypeScaleWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 46;
  EmfPlusRecordTypeRotateWorldTransform = GDIP_EMFPLUS_RECORD_BASE + 47;
  EmfPlusRecordTypeSetPageTransform = GDIP_EMFPLUS_RECORD_BASE + 48;
  EmfPlusRecordTypeResetClip = GDIP_EMFPLUS_RECORD_BASE + 49;
  EmfPlusRecordTypeSetClipRect = GDIP_EMFPLUS_RECORD_BASE + 50;
  EmfPlusRecordTypeSetClipPath = GDIP_EMFPLUS_RECORD_BASE + 51;
  EmfPlusRecordTypeSetClipRegion = GDIP_EMFPLUS_RECORD_BASE + 52;
  EmfPlusRecordTypeOffsetClip = GDIP_EMFPLUS_RECORD_BASE + 53;
  EmfPlusRecordTypeDrawDriverString = GDIP_EMFPLUS_RECORD_BASE + 54;
  EmfPlusRecordTotal = GDIP_EMFPLUS_RECORD_BASE + 55;
  EmfPlusRecordTypeMax = EmfPlusRecordTotal - 1;
  EmfPlusRecordTypeMin = EmfPlusRecordTypeHeader;

type
  TEmfPlusRecordType = EmfPlusRecordType;
{$ENDIF}
  StringFormatFlags = Integer;

const
  StringFormatFlagsDirectionRightToLeft = $00000001;
  StringFormatFlagsDirectionVertical = $00000002;
  StringFormatFlagsNoFitBlackBox = $00000004;
  StringFormatFlagsDisplayFormatControl = $00000020;
  StringFormatFlagsNoFontFallback = $00000400;
  StringFormatFlagsMeasureTrailingSpaces = $00000800;
  StringFormatFlagsNoWrap = $00001000;
  StringFormatFlagsLineLimit = $00002000;
  StringFormatFlagsNoClip = $00004000;

type
  TStringFormatFlags = StringFormatFlags;
  StringTrimming = (
    StringTrimmingNone,
    StringTrimmingCharacter,
    StringTrimmingWord,
    StringTrimmingEllipsisCharacter,
    StringTrimmingEllipsisWord,
    StringTrimmingEllipsisPath);
  TStringTrimming = StringTrimming;
  StringDigitSubstitute = (
    StringDigitSubstituteUser,
    StringDigitSubstituteNone,
    StringDigitSubstituteNational,
    StringDigitSubstituteTraditional);
  TStringDigitSubstitute = StringDigitSubstitute;
  PStringDigitSubstitute = ^TStringDigitSubstitute;
  HotkeyPrefix = (
    HotkeyPrefixNone,
    HotkeyPrefixShow,
    HotkeyPrefixHide);
  THotkeyPrefix = HotkeyPrefix;
  StringAlignment = (
    StringAlignmentNear,
    StringAlignmentCenter,
    StringAlignmentFar);
  TStringAlignment = StringAlignment;
  DriverStringOptions = Integer;

const
  DriverStringOptionsCmapLookup = 1;
  DriverStringOptionsVertical = 2;
  DriverStringOptionsRealizedAdvance = 4;
  DriverStringOptionsLimitSubpixel = 8;

type
  TDriverStringOptions = DriverStringOptions;
  FlushIntention = (
    FlushIntentionFlush,
    FlushIntentionSync);
  TFlushIntention = FlushIntention;
  EncoderParameterValueType = Integer;

const
  EncoderParameterValueTypeByte: Integer = 1;
  EncoderParameterValueTypeASCII: Integer = 2;
  EncoderParameterValueTypeShort: Integer = 3;
  EncoderParameterValueTypeLong: Integer = 4;
  EncoderParameterValueTypeRational: Integer = 5;
  EncoderParameterValueTypeLongRange: Integer = 6;
  EncoderParameterValueTypeUndefined: Integer = 7;
  EncoderParameterValueTypeRationalRange: Integer = 8;

type
  TEncoderParameterValueType = EncoderParameterValueType;
  EncoderValue = (
    EncoderValueColorTypeCMYK,
    EncoderValueColorTypeYCCK,
    EncoderValueCompressionLZW,
    EncoderValueCompressionCCITT3,
    EncoderValueCompressionCCITT4,
    EncoderValueCompressionRle,
    EncoderValueCompressionNone,
    EncoderValueScanMethodInterlaced,
    EncoderValueScanMethodNonInterlaced,
    EncoderValueVersionGif87,
    EncoderValueVersionGif89,
    EncoderValueRenderProgressive,
    EncoderValueRenderNonProgressive,
    EncoderValueTransformRotate90,
    EncoderValueTransformRotate180,
    EncoderValueTransformRotate270,
    EncoderValueTransformFlipHorizontal,
    EncoderValueTransformFlipVertical,
    EncoderValueMultiFrame,
    EncoderValueLastFrame,
    EncoderValueFlush,
    EncoderValueFrameDimensionTime,
    EncoderValueFrameDimensionResolution,
    EncoderValueFrameDimensionPage);
  TEncoderValue = EncoderValue;
{$IFDEF D6_UP}
  EmfToWmfBitsFlags = (
    EmfToWmfBitsFlagsDefault = $00000000,
    EmfToWmfBitsFlagsEmbedEmf = $00000001,
    EmfToWmfBitsFlagsIncludePlaceable = $00000002,
    EmfToWmfBitsFlagsNoXORClip = $00000004);
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ELSE}
  EmfToWmfBitsFlags = Integer;

const
  EmfToWmfBitsFlagsDefault = $00000000;
  EmfToWmfBitsFlagsEmbedEmf = $00000001;
  EmfToWmfBitsFlagsIncludePlaceable = $00000002;
  EmfToWmfBitsFlagsNoXORClip = $00000004;

type
  TEmfToWmfBitsFlags = EmfToWmfBitsFlags;
{$ENDIF}

  ImageAbort = function: BOOL; stdcall;
  DrawImageAbort = ImageAbort;
  GetThumbnailImageAbort = ImageAbort;
  EnumerateMetafileProc = function(recordType: EmfPlusRecordType; flags: UInt;
    dataSize: UInt; data: PByte; callbackData: Pointer): BOOL; stdcall;

const
  FLT_MAX = 3.402823466e+38;
  FLT_MIN = 1.175494351e-38;
  REAL_MAX = FLT_MAX;
  REAL_MIN = FLT_MIN;
  REAL_TOLERANCE = (FLT_MIN * 100);
  REAL_EPSILON = 1.192092896e-07;

type
  Status = (
    Ok,
    GenericError,
    InvalidParameter,
    OutOfMemory,
    ObjectBusy,
    InsufficientBuffer,
    NotImplemented,
    Win32Error,
    WrongState,
    Aborted,
    FileNotFound,
    ValueOverflow,
    AccessDenied,
    UnknownImageFormat,
    FontFamilyNotFound,
    FontStyleNotFound,
    NotTrueTypeFont,
    UnsupportedGdiPlusVersion,
    GdiPlusNotInitialized,
    PropertyNotFound,
    PropertyNotSupported);
  TStatus = Status;

  TSizeF = packed record
    Width: Single;
    Height: Single;
  end;
  PSizeF = ^TSizeF;

  TSizeI = packed record
    Width: Integer;
    Height: Integer;
  end;
  PSizeI = ^TSizeI;

  TPointF = packed record
    X: Single;
    Y: Single;
  end;
  PPointF = ^TPointF;
  TPointFs = array of TPointF;

  TPointI = packed record
    X: Integer;
    Y: Integer;
  end;
  PPointI = ^TPointI;
  TPointIs = array of TPointI;

  TRectF = packed record
    X: Single;
    Y: Single;
    Width: Single;
    Height: Single;
  end;
  PRectF = ^TRectF;
  TRectFs = array of TRectF;

  TRectI = packed record
    X: Integer;
    Y: Integer;
    Width: Integer;
    Height: Integer;
  end;
  PRectI = ^TRectI;
  TRectIs = array of TRectI;

  TPathData = packed record
    Count: Integer;
    Points: array of PointF;
    Types: array of Byte;
  end;

  PCharacterRange = ^TCharacterRange;

  TCharacterRange = packed record
    First: Integer;
    Length: Integer;
  end;

function MakeCharacterRange(First, Length: Integer): TCharacterRange;

type
  DebugEventLevel = (
    DebugEventLevelFatal,
    DebugEventLevelWarning);
  TDebugEventLevel = DebugEventLevel;
  DebugEventProc = procedure(level: DebugEventLevel; message: Pchar); stdcall;
  NotificationHookProc = function(out token: ULONG): Status; stdcall;
  NotificationUnhookProc = procedure(token: ULONG); stdcall;

  GdiPlusStartupInput = packed record
    GdiPlusVersion: Cardinal;
    DebugEventCallback: DebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs: BOOL;
  end;
  TGdiPlusStartupInput = GdiPlusStartupInput;
  PGdiPlusStartupInput = ^TGdiPlusStartupInput;

  GdiPlusStartupOutput = packed record
    NotificationHook: NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  TGdiPlusStartupOutput = GdiPlusStartupOutput;
  PGdiPlusStartupOutput = ^TGdiPlusStartupOutput;

type
  ARGB = DWORD;
  TArgb = ARGB;
  PArgb = ^TArgb;

  TArgbStruct = packed record
    B, G, R, A: Byte;
  end;
  PArgbStruct = ^TArgbStruct;

  ARGB64 = Int64;
  TArgb4 = ARGB64;
  PArgb4 = ^TArgb4;

const
  ALPHA_SHIFT = 24;
  RED_SHIFT = 16;
  GREEN_SHIFT = 8;
  BLUE_SHIFT = 0;
  ALPHA_MASK = (ARGB($FF) shl ALPHA_SHIFT);

type
  PixelFormat = Integer;
  TPixelFormat = PixelFormat;

const
  PixelFormatIndexed = $00010000;
  PixelFormatGDI = $00020000;
  PixelFormatAlpha = $00040000;
  PixelFormatPAlpha = $00080000;
  PixelFormatExtended = $00100000;
  PixelFormatCanonical = $00200000;
  PixelFormatUndefined = 0;
  PixelFormatDontCare = 0;
  PixelFormat1bppIndexed = (1 or (1 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat4bppIndexed = (2 or (4 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat8bppIndexed = (3 or (8 shl 8) or PixelFormatIndexed or PixelFormatGDI);
  PixelFormat16bppGrayScale = (4 or (16 shl 8) or PixelFormatExtended);
  PixelFormat16bppRGB555 = (5 or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppRGB565 = (6 or (16 shl 8) or PixelFormatGDI);
  PixelFormat16bppARGB1555 = (7 or (16 shl 8) or PixelFormatAlpha or PixelFormatGDI);
  PixelFormat24bppRGB = (8 or (24 shl 8) or PixelFormatGDI);
  PixelFormat32bppRGB = (9 or (32 shl 8) or PixelFormatGDI);
  PixelFormat32bppARGB = (10 or (32 shl 8) or PixelFormatAlpha or PixelFormatGDI or PixelFormatCanonical);
  PixelFormat32bppPARGB = (11 or (32 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatGDI);
  PixelFormat48bppRGB = (12 or (48 shl 8) or PixelFormatExtended);
  PixelFormat64bppARGB = (13 or (64 shl 8) or PixelFormatAlpha or PixelFormatCanonical or PixelFormatExtended);
  PixelFormat64bppPARGB = (14 or (64 shl 8) or PixelFormatAlpha or PixelFormatPAlpha or PixelFormatExtended);
  PixelFormatMax = 15;

function GetPixelFormatSize(F: PixelFormat): UInt;
function IsIndexedPixelFormat(F: PixelFormat): BOOL;
function IsAlphaPixelFormat(F: PixelFormat): BOOL;
function IsExtendedPixelFormat(F: PixelFormat): BOOL;
function IsCanonicalPixelFormat(F: PixelFormat): BOOL;
{$IFDEF D6_UP}
type
  PaletteFlags = (
    PaletteFlagsHasAlpha = $0001,
    PaletteFlagsGrayScale = $0002,
    PaletteFlagsHalftone = $0004);
  TPaletteFlags = PaletteFlags;
{$ELSE}
type
  PaletteFlags = Integer;

const
  PaletteFlagsHasAlpha = $0001;
  PaletteFlagsGrayScale = $0002;
  PaletteFlagsHalftone = $0004;

type
  TPaletteFlags = PaletteFlags;
{$ENDIF}
  ColorPalette = packed record
    Flags: UInt;
    Count: UInt;
    Entries: array [0..0] of ARGB;
  end;
  TColorPalette = ColorPalette;
  PColorPalette = ^TColorPalette;

  ColorMode = (
    ColorModeARGB32,
    ColorModeARGB64);
  TColorMode = ColorMode;
  ColorChannelFlags = (
    ColorChannelFlagsC,
    ColorChannelFlagsM,
    ColorChannelFlagsY,
    ColorChannelFlagsK,
    ColorChannelFlagsLast);
  TColorChannelFlags = ColorChannelFlags;

const
  aclAliceBlue = $FFF0F8FF;
  aclAntiqueWhite = $FFFAEBD7;
  aclAqua = $FF00FFFF;
  aclAquamarine = $FF7FFFD4;
  aclAzure = $FFF0FFFF;
  aclBeige = $FFF5F5DC;
  aclBisque = $FFFFE4C4;
  aclBlack = $FF000000;
  aclBlanchedAlmond = $FFFFEBCD;
  aclBlue = $FF0000FF;
  aclBlueViolet = $FF8A2BE2;
  aclBrown = $FFA52A2A;
  aclBurlyWood = $FFDEB887;
  aclCadetBlue = $FF5F9EA0;
  aclChartreuse = $FF7FFF00;
  aclChocolate = $FFD2691E;
  aclCoral = $FFFF7F50;
  aclCornflowerBlue = $FF6495ED;
  aclCornsilk = $FFFFF8DC;
  aclCrimson = $FFDC143C;
  aclCyan = $FF00FFFF;
  aclDarkBlue = $FF00008B;
  aclDarkCyan = $FF008B8B;
  aclDarkGoldenrod = $FFB8860B;
  aclDarkGray = $FFA9A9A9;
  aclDarkGreen = $FF006400;
  aclDarkKhaki = $FFBDB76B;
  aclDarkMagenta = $FF8B008B;
  aclDarkOliveGreen = $FF556B2F;
  aclDarkOrange = $FFFF8C00;
  aclDarkOrchid = $FF9932CC;
  aclDarkRed = $FF8B0000;
  aclDarkSalmon = $FFE9967A;
  aclDarkSeaGreen = $FF8FBC8B;
  aclDarkSlateBlue = $FF483D8B;
  aclDarkSlateGray = $FF2F4F4F;
  aclDarkTurquoise = $FF00CED1;
  aclDarkViolet = $FF9400D3;
  aclDeepPink = $FFFF1493;
  aclDeepSkyBlue = $FF00BFFF;
  aclDimGray = $FF696969;
  aclDodgerBlue = $FF1E90FF;
  aclFirebrick = $FFB22222;
  aclFloralWhite = $FFFFFAF0;
  aclForestGreen = $FF228B22;
  aclFuchsia = $FFFF00FF;
  aclGainsboro = $FFDCDCDC;
  aclGhostWhite = $FFF8F8FF;
  aclGold = $FFFFD700;
  aclGoldenrod = $FFDAA520;
  aclGray = $FF808080;
  aclGreen = $FF008000;
  aclGreenYellow = $FFADFF2F;
  aclHoneydew = $FFF0FFF0;
  aclHotPink = $FFFF69B4;
  aclIndianRed = $FFCD5C5C;
  aclIndigo = $FF4B0082;
  aclIvory = $FFFFFFF0;
  aclKhaki = $FFF0E68C;
  aclLavender = $FFE6E6FA;
  aclLavenderBlush = $FFFFF0F5;
  aclLawnGreen = $FF7CFC00;
  aclLemonChiffon = $FFFFFACD;
  aclLightBlue = $FFADD8E6;
  aclLightCoral = $FFF08080;
  aclLightCyan = $FFE0FFFF;
  aclLightGoldenrodYellow = $FFFAFAD2;
  aclLightGray = $FFD3D3D3;
  aclLightGreen = $FF90EE90;
  aclLightPink = $FFFFB6C1;
  aclLightSalmon = $FFFFA07A;
  aclLightSeaGreen = $FF20B2AA;
  aclLightSkyBlue = $FF87CEFA;
  aclLightSlateGray = $FF778899;
  aclLightSteelBlue = $FFB0C4DE;
  aclLightYellow = $FFFFFFE0;
  aclLime = $FF00FF00;
  aclLimeGreen = $FF32CD32;
  aclLinen = $FFFAF0E6;
  aclMagenta = $FFFF00FF;
  aclMaroon = $FF800000;
  aclMediumAquamarine = $FF66CDAA;
  aclMediumBlue = $FF0000CD;
  aclMediumOrchid = $FFBA55D3;
  aclMediumPurple = $FF9370DB;
  aclMediumSeaGreen = $FF3CB371;
  aclMediumSlateBlue = $FF7B68EE;
  aclMediumSpringGreen = $FF00FA9A;
  aclMediumTurquoise = $FF48D1CC;
  aclMediumVioletRed = $FFC71585;
  aclMidnightBlue = $FF191970;
  aclMintCream = $FFF5FFFA;
  aclMistyRose = $FFFFE4E1;
  aclMoccasin = $FFFFE4B5;
  aclNavajoWhite = $FFFFDEAD;
  aclNavy = $FF000080;
  aclOldLace = $FFFDF5E6;
  aclOlive = $FF808000;
  aclOliveDrab = $FF6B8E23;
  aclOrange = $FFFFA500;
  aclOrangeRed = $FFFF4500;
  aclOrchid = $FFDA70D6;
  aclPaleGoldenrod = $FFEEE8AA;
  aclPaleGreen = $FF98FB98;
  aclPaleTurquoise = $FFAFEEEE;
  aclPaleVioletRed = $FFDB7093;
  aclPapayaWhip = $FFFFEFD5;
  aclPeachPuff = $FFFFDAB9;
  aclPeru = $FFCD853F;
  aclPink = $FFFFC0CB;
  aclPlum = $FFDDA0DD;
  aclPowderBlue = $FFB0E0E6;
  aclPurple = $FF800080;
  aclRed = $FFFF0000;
  aclRosyBrown = $FFBC8F8F;
  aclRoyalBlue = $FF4169E1;
  aclSaddleBrown = $FF8B4513;
  aclSalmon = $FFFA8072;
  aclSandyBrown = $FFF4A460;
  aclSeaGreen = $FF2E8B57;
  aclSeaShell = $FFFFF5EE;
  aclSienna = $FFA0522D;
  aclSilver = $FFC0C0C0;
  aclSkyBlue = $FF87CEEB;
  aclSlateBlue = $FF6A5ACD;
  aclSlateGray = $FF708090;
  aclSnow = $FFFFFAFA;
  aclSpringGreen = $FF00FF7F;
  aclSteelBlue = $FF4682B4;
  aclTan = $FFD2B48C;
  aclTeal = $FF008080;
  aclThistle = $FFD8BFD8;
  aclTomato = $FFFF6347;
  aclTransparent = $00FFFFFF;
  aclTurquoise = $FF40E0D0;
  aclViolet = $FFEE82EE;
  aclWheat = $FFF5DEB3;
  aclWhite = $FFFFFFFF;
  aclWhiteSmoke = $FFF5F5F5;
  aclYellow = $FFFFFF00;
  aclYellowGreen = $FF9ACD32;
  AlphaShift = 24;
  RedShift = 16;
  GreenShift = 8;
  BlueShift = 0;
  AlphaMask = $FF000000;
  RedMask = $00FF0000;
  GreenMask = $0000FF00;
  BlueMask = $000000FF;

function NewArgb(A, R, G, B: Byte): TArgb; overload;
function NewArgb(Argb: TArgb; Alpha: Byte): TArgb; overload;

function GetAlpha(Color: TArgb): Byte;
function GetRed(Color: TArgb): Byte;
function GetGreen(Color: TArgb): Byte;
function GetBlue(Color: TArgb): Byte;

function ColorRefToArgb(Color: TColorRef): TArgb;
function ArgbToColorRef(Color: TArgb): TColorRef;

type
  RECTL = Windows.TRect;
  SIZEL = Windows.TSize;

  ENHMETAHEADER3 = packed record
    iType: DWORD;
    nSize: DWORD;
    rclBounds: RECTL;
    rclFrame: RECTL;
    dSignature: DWORD;
    nVersion: DWORD;
    nBytes: DWORD;
    nRecords: DWORD;
    nHandles: Word;
    sReserved: Word;
    nDescription: DWORD;
    offDescription: DWORD;
    nPalEntries: DWORD;
    szlDevice: SIZEL;
    szlMillimeters: SIZEL;
  end;
  TEnhMetaHeader3 = ENHMETAHEADER3;
  PEnhMetaHeader3 = ^TEnhMetaHeader3;

  TPWMFRect16 = packed record
    Left: Int16;
    Top: Int16;
    Right: Int16;
    Bottom: Int16;
  end;
  PPWMFRect16 = ^TPWMFRect16;

  WmfPlaceableFileHeader = packed record
    Key: UInt32;
    Hmf: Int16;
    BoundingBox: TPWMFRect16;
    Inch: Int16;
    Reserved: UInt32;
    Checksum: Int16;
  end;
  TWmfPlaceableFileHeader = WmfPlaceableFileHeader;
  PWmfPlaceableFileHeader = ^TWmfPlaceableFileHeader;

const
  GDIP_EMFPLUSFLAGS_DISPLAY = $00000001;

const
  ImageFormatUndefined: TGUID = '{B96B3CA9-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatMemoryBMP: TGUID = '{B96B3CAA-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatBMP: TGUID = '{B96B3CAB-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatEMF: TGUID = '{B96B3CAC-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatWMF: TGUID = '{B96B3CAD-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatJPEG: TGUID = '{B96B3CAE-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatPNG: TGUID = '{B96B3CAF-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatGIF: TGUID = '{B96B3CB0-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatTIFF: TGUID = '{B96B3CB1-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatEXIF: TGUID = '{B96B3CB2-0728-11D3-9D7B-0000F81EF32E}';
  ImageFormatIcon: TGUID = '{B96B3CB5-0728-11D3-9D7B-0000F81EF32E}';
  FrameDimensionTime: TGUID = '{6AEDBD6D-3FB5-418A-83A6-7F45229DC872}';
  FrameDimensionResolution: TGUID = '{84236F7B-3BD3-428F-8DAB-4EA1439CA315}';
  FrameDimensionPage: TGUID = '{7462DC86-6180-4C7E-8E3F-EE7333A7A483}';
  FormatIDImageInformation: TGUID = '{E5836CBE-5EEF-4F1D-ACDE-AE4C43B608CE}';
  FormatIDJpegAppHeaders: TGUID = '{1C4AFDCD-6177-43CF-ABC7-5F51AF39EE85}';
  EncoderCompression: TGUID = '{E09D739D-CCD4-44EE-8EBA-3FBF8BE4FC58}';
  EncoderColorDepth: TGUID = '{66087055-AD66-4C7C-9A18-38A2310B8337}';
  EncoderScanMethod: TGUID = '{3A4E2661-3109-4E56-8536-42C156E7DCFA}';
  EncoderVersion: TGUID = '{24D18C76-814A-41A4-BF53-1C219CCCF797}';
  EncoderRenderMethod: TGUID = '{6D42C53A-229A-4825-8BB7-5C99E2B9A8B8}';
  EncoderQuality: TGUID = '{1D5BE4B5-FA4A-452D-9CDD-5DB35105E7EB}';
  EncoderTransformation: TGUID = '{8D0EB2D1-A58E-4EA8-AA14-108074B7B6F9}';
  EncoderLuminanceTable: TGUID = '{EDB33BCE-0266-4A77-B904-27216099E717}';
  EncoderChrominanceTable: TGUID = '{F2E455DC-09B3-4316-8260-676ADA32481C}';
  EncoderSaveFlag: TGUID = '{292266FC-AC40-47BF-8CFC-A85B89A655DE}';
  CodecIImageBytes: TGUID = '{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}';

type
  IImageBytes = interface(IUnknown)
    ['{025D1823-6C7D-447B-BBDB-A3CBC3DFA2FC}']
    function CountBytes(out pcb: UInt): HResult; stdcall;
    function LockBytes(cb: UInt; ulOffset: ULONG; out ppvBytes: Pointer): HResult; stdcall;
    function UnlockBytes(pvBytes: Pointer; cb: UInt; ulOffset: ULONG): HResult; stdcall;
  end;

  ImageCodecInfo = packed record
    Clsid: TGUID;
    FormatID: TGUID;
    CodecName: PWChar;
    DllName: PWChar;
    FormatDescription: PWChar;
    FilenameExtension: PWChar;
    MimeType: PWChar;
    Flags: DWORD;
    Version: DWORD;
    SigCount: DWORD;
    SigSize: DWORD;
    SigPattern: PByte;
    SigMask: PByte;
  end;
  TImageCodecInfo = ImageCodecInfo;
  PImageCodecInfo = ^TImageCodecInfo;
{$IFDEF D6_UP}
  ImageCodecFlags = (
    ImageCodecFlagsEncoder = $00000001,
    ImageCodecFlagsDecoder = $00000002,
    ImageCodecFlagsSupportBitmap = $00000004,
    ImageCodecFlagsSupportVector = $00000008,
    ImageCodecFlagsSeekableEncode = $00000010,
    ImageCodecFlagsBlockingDecode = $00000020,
    ImageCodecFlagsBuiltin = $00010000,
    ImageCodecFlagsSystem = $00020000,
    ImageCodecFlagsUser = $00040000);
  TImageCodecFlags = ImageCodecFlags;
{$ELSE}
  ImageCodecFlags = Integer;

const
  ImageCodecFlagsEncoder = $00000001;
  ImageCodecFlagsDecoder = $00000002;
  ImageCodecFlagsSupportBitmap = $00000004;
  ImageCodecFlagsSupportVector = $00000008;
  ImageCodecFlagsSeekableEncode = $00000010;
  ImageCodecFlagsBlockingDecode = $00000020;
  ImageCodecFlagsBuiltin = $00010000;
  ImageCodecFlagsSystem = $00020000;
  ImageCodecFlagsUser = $00040000;

type
  TImageCodecFlags = ImageCodecFlags;
{$ENDIF}
  ImageLockMode = Integer;

const
  ImageLockModeRead = $0001;
  ImageLockModeWrite = $0002;
  ImageLockModeUserInputBuf = $0004;

type
  TImageLockMode = ImageLockMode;

  BitmapData = packed record
    Width: UInt;
    Height: UInt;
    Stride: Integer;
    PixelFormat: TPixelFormat;
    Scan0: Pointer;
    Reserved: UInt;
  end;
  TBitmapData = BitmapData;
  PBitmapData = ^TBitmapData;
{$IFDEF D6_UP}
  ImageFlags = (
    ImageFlagsNone = 0,
    ImageFlagsScalable = $0001,
    ImageFlagsHasAlpha = $0002,
    ImageFlagsHasTranslucent = $0004,
    ImageFlagsPartiallyScalable = $0008,
    ImageFlagsColorSpaceRGB = $0010,
    ImageFlagsColorSpaceCMYK = $0020,
    ImageFlagsColorSpaceGRAY = $0040,
    ImageFlagsColorSpaceYCBCR = $0080,
    ImageFlagsColorSpaceYCCK = $0100,
    ImageFlagsHasRealDPI = $1000,
    ImageFlagsHasRealPixelSize = $2000,
    ImageFlagsReadOnly = $00010000,
    ImageFlagsCaching = $00020000);
  TImageFlags = ImageFlags;
{$ELSE}
  ImageFlags = Integer;

const
  ImageFlagsNone = 0;
  ImageFlagsScalable = $0001;
  ImageFlagsHasAlpha = $0002;
  ImageFlagsHasTranslucent = $0004;
  ImageFlagsPartiallyScalable = $0008;
  ImageFlagsColorSpaceRGB = $0010;
  ImageFlagsColorSpaceCMYK = $0020;
  ImageFlagsColorSpaceGRAY = $0040;
  ImageFlagsColorSpaceYCBCR = $0080;
  ImageFlagsColorSpaceYCCK = $0100;
  ImageFlagsHasRealDPI = $1000;
  ImageFlagsHasRealPixelSize = $2000;
  ImageFlagsReadOnly = $00010000;
  ImageFlagsCaching = $00020000;

type
  TImageFlags = ImageFlags;
{$ENDIF}
{$IFDEF D6_UP}
  RotateFlipType = (
    RotateNoneFlipNone = 0,
    Rotate90FlipNone = 1,
    Rotate180FlipNone = 2,
    Rotate270FlipNone = 3,
    RotateNoneFlipX = 4,
    Rotate90FlipX = 5,
    Rotate180FlipX = 6,
    Rotate270FlipX = 7,
    RotateNoneFlipY = Rotate180FlipX,
    Rotate90FlipY = Rotate270FlipX,
    Rotate180FlipY = RotateNoneFlipX,
    Rotate270FlipY = Rotate90FlipX,
    RotateNoneFlipXY = Rotate180FlipNone,
    Rotate90FlipXY = Rotate270FlipNone,
    Rotate180FlipXY = RotateNoneFlipNone,
    Rotate270FlipXY = Rotate90FlipNone);
  TRotateFlipType = RotateFlipType;
{$ELSE}
  RotateFlipType = (
    RotateNoneFlipNone,
    Rotate90FlipNone,
    Rotate180FlipNone,
    Rotate270FlipNone,
    RotateNoneFlipX,
    Rotate90FlipX,
    Rotate180FlipX,
    Rotate270FlipX);

const
  RotateNoneFlipY = Rotate180FlipX;
  Rotate90FlipY = Rotate270FlipX;
  Rotate180FlipY = RotateNoneFlipX;
  Rotate270FlipY = Rotate90FlipX;
  RotateNoneFlipXY = Rotate180FlipNone;
  Rotate90FlipXY = Rotate270FlipNone;
  Rotate180FlipXY = RotateNoneFlipNone;
  Rotate270FlipXY = Rotate90FlipNone;

type
  TRotateFlipType = RotateFlipType;
{$ENDIF}
  EncoderParameter = packed record
    Guid: TGUID;
    NumberOfValues: ULONG;
    Type_: ULONG;
    Value: Pointer;
  end;
  TEncoderParameter = EncoderParameter;
  PEncoderParameter = ^TEncoderParameter;

  EncoderParameters = packed record
    Count: UInt;
    Parameter: array[0..0] of TEncoderParameter;
  end;
  TEncoderParameters = EncoderParameters;
  PEncoderParameters = ^TEncoderParameters;

  PropertyItem = record
    id: PROPID;
    length: ULONG;
    type_: Word;
    value: Pointer;
  end;
  TPropertyItem = PropertyItem;
  PPropertyItem = ^TPropertyItem;

const
  PropertyTagTypeByte: Integer = 1;
  PropertyTagTypeASCII: Integer = 2;
  PropertyTagTypeShort: Integer = 3;
  PropertyTagTypeLong: Integer = 4;
  PropertyTagTypeRational: Integer = 5;
  PropertyTagTypeUndefined: Integer = 7;
  PropertyTagTypeSLONG: Integer = 9;
  PropertyTagTypeSRational: Integer = 10;
  PropertyTagExifIFD = $8769;
  PropertyTagGpsIFD = $8825;
  PropertyTagNewSubfileType = $00FE;
  PropertyTagSubfileType = $00FF;
  PropertyTagImageWidth = $0100;
  PropertyTagImageHeight = $0101;
  PropertyTagBitsPerSample = $0102;
  PropertyTagCompression = $0103;
  PropertyTagPhotometricInterp = $0106;
  PropertyTagThreshHolding = $0107;
  PropertyTagCellWidth = $0108;
  PropertyTagCellHeight = $0109;
  PropertyTagFillOrder = $010A;
  PropertyTagDocumentName = $010D;
  PropertyTagImageDescription = $010E;
  PropertyTagEquipMake = $010F;
  PropertyTagEquipModel = $0110;
  PropertyTagStripOffsets = $0111;
  PropertyTagOrientation = $0112;
  PropertyTagSamplesPerPixel = $0115;
  PropertyTagRowsPerStrip = $0116;
  PropertyTagStripBytesCount = $0117;
  PropertyTagMinSampleValue = $0118;
  PropertyTagMaxSampleValue = $0119;
  PropertyTagXResolution = $011A;
  PropertyTagYResolution = $011B;
  PropertyTagPlanarConfig = $011C;
  PropertyTagPageName = $011D;
  PropertyTagXPosition = $011E;
  PropertyTagYPosition = $011F;
  PropertyTagFreeOffset = $0120;
  PropertyTagFreeByteCounts = $0121;
  PropertyTagGrayResponseUnit = $0122;
  PropertyTagGrayResponseCurve = $0123;
  PropertyTagT4Option = $0124;
  PropertyTagT6Option = $0125;
  PropertyTagResolutionUnit = $0128;
  PropertyTagPageNumber = $0129;
  PropertyTagTransferFuncition = $012D;
  PropertyTagSoftwareUsed = $0131;
  PropertyTagDateTime = $0132;
  PropertyTagArtist = $013B;
  PropertyTagHostComputer = $013C;
  PropertyTagPredictor = $013D;
  PropertyTagWhitePoint = $013E;
  PropertyTagPrimaryChromaticities = $013F;
  PropertyTagColorMap = $0140;
  PropertyTagHalftoneHints = $0141;
  PropertyTagTileWidth = $0142;
  PropertyTagTileLength = $0143;
  PropertyTagTileOffset = $0144;
  PropertyTagTileByteCounts = $0145;
  PropertyTagInkSet = $014C;
  PropertyTagInkNames = $014D;
  PropertyTagNumberOfInks = $014E;
  PropertyTagDotRange = $0150;
  PropertyTagTargetPrinter = $0151;
  PropertyTagExtraSamples = $0152;
  PropertyTagSampleFormat = $0153;
  PropertyTagSMinSampleValue = $0154;
  PropertyTagSMaxSampleValue = $0155;
  PropertyTagTransferRange = $0156;
  PropertyTagJPEGProc = $0200;
  PropertyTagJPEGInterFormat = $0201;
  PropertyTagJPEGInterLength = $0202;
  PropertyTagJPEGRestartInterval = $0203;
  PropertyTagJPEGLosslessPredictors = $0205;
  PropertyTagJPEGPointTransforms = $0206;
  PropertyTagJPEGQTables = $0207;
  PropertyTagJPEGDCTables = $0208;
  PropertyTagJPEGACTables = $0209;
  PropertyTagYCbCrCoefficients = $0211;
  PropertyTagYCbCrSubsampling = $0212;
  PropertyTagYCbCrPositioning = $0213;
  PropertyTagREFBlackWhite = $0214;
  PropertyTagICCProfile = $8773;
  PropertyTagGamma = $0301;
  PropertyTagICCProfileDescriptor = $0302;
  PropertyTagSRGBRenderingIntent = $0303;
  PropertyTagImageTitle = $0320;
  PropertyTagCopyright = $8298;
  PropertyTagResolutionXUnit = $5001;
  PropertyTagResolutionYUnit = $5002;
  PropertyTagResolutionXLengthUnit = $5003;
  PropertyTagResolutionYLengthUnit = $5004;
  PropertyTagPrintFlags = $5005;
  PropertyTagPrintFlagsVersion = $5006;
  PropertyTagPrintFlagsCrop = $5007;
  PropertyTagPrintFlagsBleedWidth = $5008;
  PropertyTagPrintFlagsBleedWidthScale = $5009;
  PropertyTagHalftoneLPI = $500A;
  PropertyTagHalftoneLPIUnit = $500B;
  PropertyTagHalftoneDegree = $500C;
  PropertyTagHalftoneShape = $500D;
  PropertyTagHalftoneMisc = $500E;
  PropertyTagHalftoneScreen = $500F;
  PropertyTagJPEGQuality = $5010;
  PropertyTagGridSize = $5011;
  PropertyTagThumbnailFormat = $5012;
  PropertyTagThumbnailWidth = $5013;
  PropertyTagThumbnailHeight = $5014;
  PropertyTagThumbnailColorDepth = $5015;
  PropertyTagThumbnailPlanes = $5016;
  PropertyTagThumbnailRawBytes = $5017;
  PropertyTagThumbnailSize = $5018;
  PropertyTagThumbnailCompressedSize = $5019;
  PropertyTagColorTransferFunction = $501A;
  PropertyTagThumbnailData = $501B;
  PropertyTagThumbnailImageWidth = $5020;
  PropertyTagThumbnailImageHeight = $5021;
  PropertyTagThumbnailBitsPerSample = $5022;
  PropertyTagThumbnailCompression = $5023;
  PropertyTagThumbnailPhotometricInterp = $5024;
  PropertyTagThumbnailImageDescription = $5025;
  PropertyTagThumbnailEquipMake = $5026;
  PropertyTagThumbnailEquipModel = $5027;
  PropertyTagThumbnailStripOffsets = $5028;
  PropertyTagThumbnailOrientation = $5029;
  PropertyTagThumbnailSamplesPerPixel = $502A;
  PropertyTagThumbnailRowsPerStrip = $502B;
  PropertyTagThumbnailStripBytesCount = $502C;
  PropertyTagThumbnailResolutionX = $502D;
  PropertyTagThumbnailResolutionY = $502E;
  PropertyTagThumbnailPlanarConfig = $502F;
  PropertyTagThumbnailResolutionUnit = $5030;
  PropertyTagThumbnailTransferFunction = $5031;
  PropertyTagThumbnailSoftwareUsed = $5032;
  PropertyTagThumbnailDateTime = $5033;
  PropertyTagThumbnailArtist = $5034;
  PropertyTagThumbnailWhitePoint = $5035;
  PropertyTagThumbnailPrimaryChromaticities = $5036;
  PropertyTagThumbnailYCbCrCoefficients = $5037;
  PropertyTagThumbnailYCbCrSubsampling = $5038;
  PropertyTagThumbnailYCbCrPositioning = $5039;
  PropertyTagThumbnailRefBlackWhite = $503A;
  PropertyTagThumbnailCopyRight = $503B;
  PropertyTagLuminanceTable = $5090;
  PropertyTagChrominanceTable = $5091;
  PropertyTagFrameDelay = $5100;
  PropertyTagLoopCount = $5101;
  PropertyTagPixelUnit = $5110;
  PropertyTagPixelPerUnitX = $5111;
  PropertyTagPixelPerUnitY = $5112;
  PropertyTagPaletteHistogram = $5113;
  PropertyTagExifExposureTime = $829A;
  PropertyTagExifFNumber = $829D;
  PropertyTagExifExposureProg = $8822;
  PropertyTagExifSpectralSense = $8824;
  PropertyTagExifISOSpeed = $8827;
  PropertyTagExifOECF = $8828;
  PropertyTagExifVer = $9000;
  PropertyTagExifDTOrig = $9003;
  PropertyTagExifDTDigitized = $9004;
  PropertyTagExifCompConfig = $9101;
  PropertyTagExifCompBPP = $9102;
  PropertyTagExifShutterSpeed = $9201;
  PropertyTagExifAperture = $9202;
  PropertyTagExifBrightness = $9203;
  PropertyTagExifExposureBias = $9204;
  PropertyTagExifMaxAperture = $9205;
  PropertyTagExifSubjectDist = $9206;
  PropertyTagExifMeteringMode = $9207;
  PropertyTagExifLightSource = $9208;
  PropertyTagExifFlash = $9209;
  PropertyTagExifFocalLength = $920A;
  PropertyTagExifMakerNote = $927C;
  PropertyTagExifUserComment = $9286;
  PropertyTagExifDTSubsec = $9290;
  PropertyTagExifDTOrigSS = $9291;
  PropertyTagExifDTDigSS = $9292;
  PropertyTagExifFPXVer = $A000;
  PropertyTagExifColorSpace = $A001;
  PropertyTagExifPixXDim = $A002;
  PropertyTagExifPixYDim = $A003;
  PropertyTagExifRelatedWav = $A004;
  PropertyTagExifInterop = $A005;
  PropertyTagExifFlashEnergy = $A20B;
  PropertyTagExifSpatialFR = $A20C;
  PropertyTagExifFocalXRes = $A20E;
  PropertyTagExifFocalYRes = $A20F;
  PropertyTagExifFocalResUnit = $A210;
  PropertyTagExifSubjectLoc = $A214;
  PropertyTagExifExposureIndex = $A215;
  PropertyTagExifSensingMethod = $A217;
  PropertyTagExifFileSource = $A300;
  PropertyTagExifSceneType = $A301;
  PropertyTagExifCfaPattern = $A302;
  PropertyTagGpsVer = $0000;
  PropertyTagGpsLatitudeRef = $0001;
  PropertyTagGpsLatitude = $0002;
  PropertyTagGpsLongitudeRef = $0003;
  PropertyTagGpsLongitude = $0004;
  PropertyTagGpsAltitudeRef = $0005;
  PropertyTagGpsAltitude = $0006;
  PropertyTagGpsGpsTime = $0007;
  PropertyTagGpsGpsSatellites = $0008;
  PropertyTagGpsGpsStatus = $0009;
  PropertyTagGpsGpsMeasureMode = $00A;
  PropertyTagGpsGpsDop = $000B;
  PropertyTagGpsSpeedRef = $000C;
  PropertyTagGpsSpeed = $000D;
  PropertyTagGpsTrackRef = $000E;
  PropertyTagGpsTrack = $000F;
  PropertyTagGpsImgDirRef = $0010;
  PropertyTagGpsImgDir = $0011;
  PropertyTagGpsMapDatum = $0012;
  PropertyTagGpsDestLatRef = $0013;
  PropertyTagGpsDestLat = $0014;
  PropertyTagGpsDestLongRef = $0015;
  PropertyTagGpsDestLong = $0016;
  PropertyTagGpsDestBearRef = $0017;
  PropertyTagGpsDestBear = $0018;
  PropertyTagGpsDestDistRef = $0019;
  PropertyTagGpsDestDist = $001A;

type
  ColorMatrixFlags = (
    ColorMatrixFlagsDefault,
    ColorMatrixFlagsSkipGrays,
    ColorMatrixFlagsAltGray);
  TColorMatrixFlags = ColorMatrixFlags;
  ColorAdjustType = (
    ColorAdjustTypeDefault,
    ColorAdjustTypeBitmap,
    ColorAdjustTypeBrush,
    ColorAdjustTypePen,
    ColorAdjustTypeText,
    ColorAdjustTypeCount,
    ColorAdjustTypeAny);
  TColorAdjustType = ColorAdjustType;

{ Gdi types }

  TColorMatrix = packed array[0..4, 0..4] of Single;
  PColorMatrix = ^TColorMatrix;

  TColorMap = packed record
    OldColor: TArgb;
    NewColor: TArgb;
  end;
  PColorMap = ^TColorMap;

  TColorRow = array[0..4] of Single;
  PColorRow = ^TColorRow;

  TColorTransform = record
    Gamma: Single;
    Brightness: Single;
    Contrast: Single;
    Saturation: Single;
    Opacity: Single;
    Greyscale: Boolean;
    Negative: Boolean;
  end;

{ Native object pointers }

  GpGraphics = Pointer;
  GpBrush = Pointer;
  GpTexture = Pointer;
  GpSolidFill = Pointer;
  GpLineGradient = Pointer;
  GpPathGradient = Pointer;
  GpHatch = Pointer;
  GpPen = Pointer;
  GpCustomLineCap = Pointer;
  GpAdjustableArrowCap = Pointer;
  GpImage = Pointer;
  GpBitmap = Pointer;
  GpMatrix = Pointer;
  GpMetafile = Pointer;
  GpImageAttributes = Pointer;
  GpPath = Pointer;
  GpRegion = Pointer;
  GpPathIterator = Pointer;
  GpFontFamily = Pointer;
  GpFont = Pointer;
  GpStringFormat = Pointer;
  GpFontCollection = Pointer;
  GpCachedBitmap = Pointer;
  GpStatus = TStatus;

{ Image codecs }

const
  BmpFormat = 'image/bmp';
  GifFormat = 'image/gif';
  PngFormat = 'image/png';
  JpgFormat = 'image/jpeg';
  TifFormat = 'image/tiff';

function GetEncoderClsid(const Format: WideString; out Clsid: TGUID): Boolean;

function NewColor(R, G, B: Byte; A: Byte = $FF): TArgb; overload;
function NewColor(Color: Longint; A: Byte = $FF): TArgb; overload;
function NewBlend(Color1, Color2: Longint; Percent: Byte = 50; A: Byte = $FF): TArgb;

function NewColorMatrix: TColorMatrix; overload;
function NewOpacityMatrix(Opacity: Single): TColorMatrix;
function NewColorMatrix(R, G, B, A: Byte): TColorMatrix; overload;
function NewColorMatrix(R, G, B, A: Single): TColorMatrix; overload;
function NewColorMatrix(ColorRef: TColorRef): TColorMatrix; overload;
function NewColorMatrix(const Transform: TColorTransform): TColorMatrix; overload;

function NewColorTransform: TColorTransform;

procedure ColorFill(C: PSingle; Data: array of Single);
function ColorMultiply(const A, B: TColorMatrix): TColorMatrix;
function ColorBrightness(const M: TColorMatrix; B: Single): TColorMatrix;
function ColorContrast(const M: TColorMatrix; C: Single): TColorMatrix;
function ColorSaturate(const M: TColorMatrix; S: Single): TColorMatrix;
function ColorOpacity(const M: TColorMatrix; O: Single): TColorMatrix;
function ColorGreyscale(const M: TColorMatrix): TColorMatrix;
function ColorNegative(const M: TColorMatrix): TColorMatrix;
function ColorIntensity(Color: Longint; Intensity: Single; A: Byte = $FF): TArgb; overload;
function ColorV(Color: Longint; V: Single; A: Byte = $FF): TArgb; overload;

procedure SetOpacity(var C: TArgb; Opacity: Byte);
function GetOpacity(C: TArgb): Byte;

function NewOpacity(C: TArgb; Opacity: Byte): TArgb;

{ Point and rect type routines }

function PointI(Point: TPoint): TPointF; overload;
function PointI(X, Y: Single): TPointF; overload;
function RectF(const Rect: TRect): TRectF; overload;
function RectF(X, Y, W, H: Single): TRectF; overload;
procedure InflateRectF(var Rect: TRectF; X, Y: Single);
function OffsetRectF(var Rect: TRectF; X, Y: Single): TRectF;

{ Gdi plus interfaces }

type
  IGdiInterface = interface;
  IGdiCloneable = interface;
  IGdiGraphics = interface;
  IGdiPen = interface;
  IGdiBrush = interface;
  IGdiHatchBrush = interface;
  IGdiTextureBrush = interface;
  IGdiLinearGradientBrush = interface;
  IGdiPathGradientBrush = interface;
  IGdiGraphicsPath = interface;
  IGdiGraphicsPathIterator = interface;
  IGdiRegion = interface;
  IGdiSolidBrush = interface;
  IGdiMatrix = interface;
  IGdiImage = interface;
  IGdiBitmap = interface;
  IGdiMetafile = interface;
  IGdiImageAttributes = interface;
  IGdiCachedBitmap = interface;
  IGdiFontFamily = interface;
  IGdiFontCollection = interface;
  IGdiInstalledFontCollection = interface;
  IGdiPrivateFontCollection = interface;
  IGdiFont = interface;
  IGdiStringFormat = interface;
  IGdiCustomLineCap = interface;
  IGdiAdjustableArrowCap = interface;

{ IInterface }

  IGdiInterface = interface
    ['{0DD7CE40-4DAB-4C72-8CF9-9EB8B32808EC}']
    function GetInstance: TObject;
    function GetLastStatus: TStatus;
    procedure SetLastStatus(Value: TStatus);
    function SetStatus(Status: TStatus): TStatus;
    property Instance: TObject read GetInstance;
    property LastStatus: TStatus read GetLastStatus write SetLastStatus;
  end;

{ IGdiCloneable }

  IGdiCloneable = interface(IGdiInterface)
    ['{C70A9C51-B5B8-4D04-8B9E-0738B93478E8}']
    function Clone: IGdiInterface;
  end;

{ IGdiRegion }

  IGdiRegion = interface(IGdiInterface)
    ['{458D642B-D6E1-4117-9F51-0B28ECB85F66}']
    procedure SetNativeRegion(Region: GpRegion);
    function GetNativeRegion: GpRegion;
    function FromHRGN(Rgn: HRGN): IGdiRegion;
    function Clone: IGdiRegion;
    function MakeInfinite: TStatus;
    function MakeEmpty: TStatus;
    function GetDataSize: UInt;
    function GetData(Buffer: PByte; BufferSize: UInt; SizeFilled: PUInt = nil): TStatus;
    function Intersect(const Rect: TRectI): TStatus; overload;
    function Intersect(const Rect: TRectF): TStatus; overload;
    function Intersect(Path: IGdiGraphicsPath): TStatus; overload;
    function Intersect(Region: IGdiRegion): TStatus; overload;
    function Union(const Rect: TRectI): TStatus; overload;
    function Union(const Rect: TRectF): TStatus; overload;
    function Union(Path: IGdiGraphicsPath): TStatus; overload;
    function Union(Region: IGdiRegion): TStatus; overload;
    function Xor_(const Rect: TRectI): TStatus; overload;
    function Xor_(const Rect: TRectF): TStatus; overload;
    function Xor_(Path: IGdiGraphicsPath): TStatus; overload;
    function Xor_(Region: IGdiRegion): TStatus; overload;
    function Exclude(const Rect: TRectI): TStatus; overload;
    function Exclude(const Rect: TRectF): TStatus; overload;
    function Exclude(Path: IGdiGraphicsPath): TStatus; overload;
    function Exclude(Region: IGdiRegion): TStatus; overload;
    function Complement(const Rect: TRectI): TStatus; overload;
    function Complement(const Rect: TRectF): TStatus; overload;
    function Complement(Path: IGdiGraphicsPath): TStatus; overload;
    function Complement(Region: IGdiRegion): TStatus; overload;
    function Translate(DX, DY: Single): TStatus; overload;
    function Translate(DX, DY: Integer): TStatus; overload;
    function Transform(Matrix: IGdiMatrix): TStatus;
    function GetBounds(out Rect: TRectI; G: IGdiGraphics): TStatus; overload;
    function GetBounds(out Rect: TRectF; G: IGdiGraphics): TStatus; overload;
    function GetHRGN(G: IGdiGraphics): HRGN;
    function IsEmpty(G: IGdiGraphics): Boolean;
    function IsInfinite(G: IGdiGraphics): Boolean;
    function IsVisible(X, Y: Integer; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(const Point: TPointI; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Single; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(const Point: TPointF; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Integer; G: IGdiGraphics): Boolean; overload;
    function IsVisible(const Rect: TRectI; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Single; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(const Rect: TRectF; G: IGdiGraphics = nil): Boolean; overload;
    function Equals(Region: IGdiRegion; G: IGdiGraphics): Boolean;
    function GetRegionScansCount(Matrix: IGdiMatrix): UInt;
    function GetRegionScans(Matrix: IGdiMatrix; Rects: PRectF; out Count: Integer): TStatus; overload;
    function GetRegionScans(Matrix: IGdiMatrix; Rects: PRectI; out Count: Integer): TStatus; overload;
    property NativeRegion: GpRegion read GetNativeRegion write SetNativeRegion;
  end;

{ IGdiImage }

  IGdiImage = interface(IGdiInterface)
    ['{30B1EBB1-DE42-41F6-A65B-7D75090B016C}']
    function GetNativeImage: GpImage;
    procedure SetNativeImage(Image: GpImage);
    function FromFile(Filename: WideString; UseEmbeddedColorManagement: Boolean = False): IGdiImage;
    function FromStream(Stream: IStream; UseEmbeddedColorManagement: Boolean = False): IGdiImage;
    function Clone: IGdiImage;
    function Save(Filename: WideString; const clsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(Stream: IStream; const clsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function SaveAdd(EncoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(NewImage: IGdiImage; EncoderParams: PEncoderParameters): TStatus; overload;
    function GetType: TImageType;
    function GetPhysicalDimension(out Size: TSizeF): TStatus;
    function GetBounds(out SrcRect: TRectF; out SrcUnit: TUnit): TStatus;
    function GetWidth: UInt;
    function GetHeight: UInt;
    function GetHorizontalResolution: Single;
    function GetVerticalResolution: Single;
    function GetFlags: UInt;
    function GetRawFormat(out Format: TGUID): TStatus;
    function GetPixelFormat: TPixelFormat;
    function GetPaletteSize: Integer;
    function GetPalette(palette: PColorPalette; Size: Integer): TStatus;
    function SetPalette(palette: PColorPalette): TStatus;
    function GetThumbnailImage(ThumbWidth, ThumbHeight: UInt;
      Callback: GetThumbnailImageAbort = nil; CallbackData: Pointer = nil): IGdiImage;
    function GetFrameDimensionsCount: UInt;
    function GetFrameDimensionsList(DimensionIDs: PGUID; Count: UInt): TStatus;
    function GetFrameCount(const DimensionID: TGUID): UInt;
    function SelectActiveFrame(const DimensionID: TGUID; FrameIndex: UInt): TStatus;
    function RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
    function GetPropertyCount: UInt;
    function GetPropertyIdList(numOfProperty: UInt; list: PPropID): TStatus;
    function GetPropertyItemSize(PropId: PropID): UInt;
    function GetPropertyItem(PropId: PropID; PropSize: UInt; Buffer: PPropertyItem): TStatus;
    function GetPropertySize(out TotalBufferSize, NumProperties: UInt): TStatus;
    function GetAllPropertyItems(TotalBufferSize, NumProperties: UInt;
      AllItems: PPropertyItem): TStatus;
    function RemovePropertyItem(PropId: TPropID): TStatus;
    function SetPropertyItem(const item: TPropertyItem): TStatus;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID): UInt;
    function GetEncoderParameterList(const clsidEncoder: TGUID; Size: UInt;
      Buffer: PEncoderParameters): TStatus;
    property NativeImage: GpImage read GetNativeImage write SetNativeImage;
    property Width: Uint read GetWidth;
    property Height: Uint read GetHeight;
  end;

{ IGdiBitmap }

  IGdiBitmap = interface(IGdiImage)
    ['{AA293640-16A1-4521-8C78-C08727E52BEB}']
    function Clone(Rect: TRectI; Format: TPixelFormat): IGdiBitmap; overload;
    function Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IGdiBitmap; overload;
    function Clone(Rect: TRectF; Format: TPixelFormat): IGdiBitmap; overload;
    function Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IGdiBitmap; overload;
    function LockBits(Rect: TRectI; Flags: UInt; Format: TPixelFormat; out LockedBitmapData: TBitmapData): TStatus;
    function UnlockBits(var LockedBitmapData: TBitmapData): TStatus;
    function GetPixel(X, Y: Integer; out Color: TArgb): TStatus;
    function SetPixel(X, Y: Integer; Color: TArgb): TStatus;
    function SetResolution(XDPI, YDPI: Single): TStatus;
    function FromDiRectDrawSurface7(surface: IDirectDrawSurface7): IGdiBitmap;
    function FromBitmapInfo(var BitmapInfo: TBitmapInfo; BitmapData: Pointer): IGdiBitmap;
    function FromHBitmap(Bmp: HBitmap; Pal: HPALETTE): IGdiBitmap;
    function FromHICON(Icon: HICON): IGdiBitmap;
    function FromResource(hInstance: HMODULE; BitmapName: WideString): IGdiBitmap;
    function GetHBitmap(ColorBackground: TArgb; out BmpReturn: HBitmap): TStatus;
    function GetHICON(out Icon: HICON): TStatus;
  end;

{ IGdiCachedBitmap }

  IGdiCachedBitmap = interface(IGdiInterface)
    ['{42FFAC17-5A6D-4A32-984B-C717470807C3}']
    function GetNativeCachedBitmap: GpCachedBitmap;
    property NativeCachedBitmap: GpCachedBitmap read GetNativeCachedBitmap;
  end;

{ IGdiImageAttributes }

  IGdiImageAttributes = interface(IGdiInterface)
    ['{0099A086-9E1B-4E5E-BDA0-3CE73F1FF713}']
    function GetNativeImageAttr: GpImageAttributes;
    procedure SetNativeImageAttr(ImageAttr: GpImageAttributes);
    function Clone: IGdiImageAttributes;
    function SetToIdentity(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function Reset(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrix(const ColorMatrix: TColorMatrix;
      Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrix(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrices(const ColorMatrix: TColorMatrix; const GrayMatrix: TColorMatrix;
      Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetThreshold(threshold: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearThreshold(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetGamma(gamma: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearGamma(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorKey(ColorLow, ColorHigh: TArgb; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorKey(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannel(channelFlags: TColorChannelFlags; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannel(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannelColorProfile(ColorProfileFilename: WideString;
      Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannelColorProfile(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetRemapTable(MapSize: Cardinal; Map: PColorMap; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearRemapTable(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetBrushRemapTable(MapSize: cardinal; Map: PColorMap): TStatus;
    function ClearBrushRemapTable: TStatus;
    function SetWrapMode(Wrap: TWrapMode; Color: TArgb = aclBlack; clamp: Boolean = False): TStatus;
    function GetAdjustedPalette(ColorPalette: PColorPalette; ColorAdjustType: TColorAdjustType): TStatus;
    property NativeImageAttr: GpImageAttributes read GetNativeImageAttr write SetNativeImageAttr;
  end;

  TMatrixArray = array[0..5] of Single;

{ IGdiMatrix }

  IGdiMatrix = interface(IGdiInterface)
    ['{89744ABF-734D-4546-ACE0-D0425A846FC7}']
    function GetNativeMatrix: GpMatrix;
    procedure SetNativeMatrix(Matrix: GpMatrix);
    function Clone: IGdiMatrix;
    function GetElements(const M: TMatrixArray): TStatus;
    function SetElements(M11, M12, M21, M22, DX, DY: Single): TStatus;
    function OffsetX: Single;
    function OffsetY: Single;
    function Reset: TStatus;
    function Multiply(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Translate(OffsetX, OffsetY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Scale(ScaleX, ScaleY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Rotate(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateAt(Angle: Single; const center: TPointF; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Shear(ShearX, ShearY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Invert: TStatus;
    function TransformPoints(Pts: PPointF; Count: Integer = 1): TStatus; overload;
    function TransformPoints(Pts: PPointI; Count: Integer = 1): TStatus; overload;
    function TransformVectors(Pts: PPointF; Count: Integer = 1): TStatus; overload;
    function TransformVectors(Pts: PPointI; Count: Integer = 1): TStatus; overload;
    function IsInvertible: Boolean;
    function IsIdentity: Boolean;
    function Equals(Matrix: IGdiMatrix): Boolean;
    property NativeMatrix: GpMatrix read GetNativeMatrix write SetNativeMatrix;
  end;

{ IGdiBrush }

  IGdiBrush = interface(IGdiInterface)
    ['{2EF2751F-7E05-4F12-8F6B-B78754F2F4A9}']
    procedure SetNativeBrush(Brush: GpBrush);
    function GetNativeBrush: GpBrush;
    function Clone: IGdiBrush;
    function GetType: TBrushType;
    property NativeBrush: GpBrush read GetNativeBrush write SetNativeBrush;
  end;

{ IGdiSolidBrush }

  IGdiSolidBrush = interface(IGdiBrush)
    ['{3A357B12-1869-416F-83FF-7D7F33CA1AEE}']
    function GetColor(out Color: TArgb): TStatus;
    function SetColor(Color: TArgb): TStatus;
  end;

{ IGdiTextureBrush }

  IGdiTextureBrush = interface(IGdiBrush)
    ['{6DC90CEF-D71F-4CC3-81A9-16D01A63A49C}']
    function SetTransform(Matrix: IGdiMatrix): TStatus;
    function GetTransform(Matrix: IGdiMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: MatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
    function GetImage: IGdiImage;
  end;

{ IGdiLinearGradientBrush }

  IGdiLinearGradientBrush = interface(IGdiBrush)
    ['{C2BBB736-EB2D-4762-B16B-8465FAFBCAD8}']
    function SetLinearColors(Color1, Color2: TArgb): TStatus;
    function GetLinearColors(out Color1, Color2: TArgb): TStatus;
    function GetRectangle(out Rect: TRectF): TStatus; overload;
    function GetRectangle(out Rect: TRectI): TStatus; overload;
    function SetGammaCorrection(UseGammaCorrection: Boolean): TStatus;
    function GetGammaCorrection: Boolean;
    function GetBlendCount: Integer;
    function SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle; Count: Integer): TStatus;
    function SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
    function SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
    function SetTransform(Matrix: IGdiMatrix): TStatus;
    function GetTransform(Matrix: IGdiMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
  end;

{ IGdiPathGradientBrush }

  IGdiPathGradientBrush = interface
    ['{F9DCB376-53BA-4D36-A8A9-70ABC64EACB3}']
    function GetCenterColor(out Color: TArgb): TStatus;
    function SetCenterColor(Color: TArgb): TStatus;
    function GetPointCount: Integer;
    function GetSurroundColorCount: Integer;
    function GetSurroundColors(Colors: PArgb; var Count: Integer): TStatus;
    function SetSurroundColors(Colors: PArgb; var Count: Integer): TStatus;
    function GetGraphicsPath(Path: IGdiGraphicsPath): TStatus;
    function SetGraphicsPath(Path: IGdiGraphicsPath): TStatus;
    function GetCenterPoint(out Point: TPointF): TStatus; overload;
    function GetCenterPoint(out Point: TPointI): TStatus; overload;
    function SetCenterPoint(Point: TPointF): TStatus; overload;
    function SetCenterPoint(Point: TPointI): TStatus; overload;
    function GetRectangle(out Rect: TRectF): TStatus; overload;
    function GetRectangle(out Rect: TRectI): TStatus; overload;
    function SetGammaCorrection(UseGammaCorrection: Boolean): TStatus; overload;
    function GetGammaCorrection: Boolean; overload;
    function GetBlendCount: Integer;
    function GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle;
      Count: Integer): TStatus;
    function GetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle;
      Count: Integer): TStatus;
    function SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
    function SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
    function GetTransform(Matrix: IGdiMatrix): TStatus;
    function SetTransform(Matrix: IGdiMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetFocusScales(out XScale, YScale: Single): TStatus;
    function SetFocusScales(XScale, YScale: Single): TStatus;
    function GetWrapMode: TWrapMode;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
  end;

{ IGdiHatchBrush }

  IGdiHatchBrush = interface(IGdiBrush)
    ['{92527FC5-E207-49C3-9B7F-D39A13057C37}']
    function GetHatchStyle: THatchStyle;
    function GetForegrounDColor(out Color: TArgb): TStatus;
    function GetBackgrounDColor(out Color: TArgb): TStatus;
  end;

{ IGdiPen }

  IGdiPen = interface(IGdiInterface)
    ['{919AEC81-886C-4618-80F7-F8E088899A93}']
    procedure SetNativePen(Pen: GpPen);
    function GetNativePen: GpPen;
    function Clone: IGdiPen;
    procedure SetWidth(Width: Single);
    function GetWidth: Single;
    function SetLineCap(StartCap, EndCap: TLineCap; DashCap: TDashCap): TStatus;
    function SetStartCap(StartCap: TLineCap): TStatus;
    function SetEndCap(EndCap: TLineCap): TStatus;
    function SetDashCap(DashCap: TDashCap): TStatus;
    function GetStartCap: TLineCap;
    function GetEndCap: TLineCap;
    function GetDashCap: TDashCap;
    function SetLineJoin(lineJoin: TLineJoin): TStatus;
    function GetLineJoin: TLineJoin;
    function SetCustomStartCap(CustomCap: IGdiCustomLineCap): TStatus;
    function GetCustomStartCap(CustomCap: IGdiCustomLineCap): TStatus;
    function SetCustomEndCap(CustomCap: IGdiCustomLineCap): TStatus;
    function GetCustomEndCap(CustomCap: IGdiCustomLineCap): TStatus;
    function SetMiterLimit(MiterLimit: Single): TStatus;
    function GetMiterLimit: Single;
    function SetAlignment(penAlignment: TPenAlignment): TStatus;
    function GetAlignment: TPenAlignment;
    function SetTransform(Matrix: IGdiMatrix): TStatus;
    function GetTransform(Matrix: IGdiMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetPenType: TPenType;
    function SetColor(Color: TArgb): TStatus;
    function SetBrush(Brush: IGdiBrush): TStatus;
    function GetColor(out Color: TArgb): TStatus;
    function GetBrush: IGdiBrush;
    function GetDashStyle: TDashStyle;
    function SetDashStyle(DashStyle: TDashStyle): TStatus;
    function GetDashOffset: Single;
    function SetDashOffset(DashOffset: Single): TStatus;
    function SetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
    function GetDashPatternCount: Integer;
    function GetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
    function SetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
    function GetCompoundArrayCount: Integer;
    function GetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
    property NativePen: GpPen read GetNativePen write SetNativePen;
    property Width: Single read GetWidth write SetWidth;
  end;

{ IGdiGraphicsPath }

  IGdiGraphicsPath = interface(IGdiInterface)
    ['{FB0A7A1D-6A88-4685-B0CA-8020A89DC17F}']
    procedure SetNativePath(Path: GpPath);
    function GetNativePath: GpPath;
    function Clone: IGdiGraphicsPath;
    function Reset: TStatus;
    function GetFillMode: TFillMode;
    function SetFillMode(Fillmode: TFillMode): TStatus;
    function GetPathData(var PathData: TPathData): TStatus;
    function StartFigure: TStatus;
    function CloseFigure: TStatus;
    function CloseAllFigures: TStatus;
    function SetMarker: TStatus;
    function ClearMarkers: TStatus;
    function Reverse: TStatus;
    function GetLastPoint(out lastPoint: TPointF): TStatus;
    function AddLine(const Pt1, Pt2: TPointF): TStatus; overload;
    function AddLine(X1, Y1, X2, Y2: Single): TStatus; overload;
    function AddLines(Points: PPointF; Count: Integer): TStatus; overload;
    function AddLine(const Pt1, Pt2: TPointI): TStatus; overload;
    function AddLine(X1, Y1, X2, Y2: Integer): TStatus; overload;
    function AddLines(Points: PPointI; Count: Integer): TStatus; overload;
    function AddArc(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddArc(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function AddArc(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddArc(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddBezier(Pt1, Pt2, Pt3, Pt4: TPointF): TStatus; overload;
    function AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus; overload;
    function AddBeziers(Points: PPointF; Count: Integer): TStatus; overload;
    function AddBezier(Pt1, Pt2, Pt3, Pt4: TPointI): TStatus; overload;
    function AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus; overload;
    function AddBeziers(Points: PPointI; Count: Integer): TStatus; overload;
    function AddCurve(Points: PPointF; Count: Integer): TStatus; overload;
    function AddCurve(Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function AddCurve(Points: PPointF; Count, Offset, NumberOfSegments: Integer; Tension: Single): TStatus; overload;
    function AddCurve(Points: PPointI; Count: Integer): TStatus; overload;
    function AddCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function AddCurve(Points: PPointI; Count, Offset, NumberOfSegments: Integer; Tension: Single): TStatus; overload;
    function AddClosedCurve(Points: PPointF; Count: Integer): TStatus; overload;
    function AddClosedCurve(Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function AddClosedCurve(Points: PPointI; Count: Integer): TStatus; overload;
    function AddClosedCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function AddRectangle(Rect: TRectF): TStatus; overload;
    function AddRectangles(Rects: PRectF; Count: Integer): TStatus; overload;
    function AddRectangle(Rect: TRectI): TStatus; overload;
    function AddRectangles(Rects: PRectI; Count: Integer): TStatus; overload;
    function AddEllipse(Rect: TRectF): TStatus; overload;
    function AddEllipse(X, Y, Width, Height: Single): TStatus; overload;
    function AddEllipse(Rect: TRectI): TStatus; overload;
    function AddEllipse(X, Y, Width, Height: Integer): TStatus; overload;
    function AddPie(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPie(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPie(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPie(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPolygon(Points: PPointF; Count: Integer): TStatus; overload;
    function AddPolygon(Points: PPointI; Count: Integer): TStatus; overload;
    function AddPath(AddingPath: IGdiGraphicsPath; Connect: Boolean): TStatus;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; Origin: TPointF; Format: IGdiStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; LayoutRect: TRectF; Format: IGdiStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; Origin: TPointI; Format: IGdiStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; LayoutRect: TRectI; Format: IGdiStringFormat): TStatus; overload;
    function Transform(Matrix: IGdiMatrix): TStatus;
    function GetBounds(out Bounds: TRectF; Matrix: IGdiMatrix = nil; Pen: IGdiPen = nil): TStatus; overload;
    function GetBounds(out Bounds: TRectI; Matrix: IGdiMatrix = nil; Pen: IGdiPen = nil): TStatus; overload;
    function Flatten(Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Widen(Pen: IGdiPen; Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Outline(Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Warp(DestPoints: PPointF; Count: Integer; SrcRect: TRectF;
      Matrix: IGdiMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
      Flatness: Single = FlatnessDefault): TStatus;
    function GetPointCount: Integer;
    function GetPathTypes(Types: PByte; Count: Integer): TStatus;
    function GetPathPoints(Points: PPointF; Count: Integer): TStatus; overload;
    function GetPathPoints(Points: PPointI; Count: Integer): TStatus; overload;
    function IsVisible(Point: TPointF; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Single; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(Point: TPointI; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Integer; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(Point: TPointF; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(X, Y: Single; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(Point: TPointI; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(X, Y: Integer; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
    property NativePath: GpPath read GetNativePath write SetNativePath;
  end;

{ IGdiGraphicsPathIterator }

  IGdiGraphicsPathIterator = interface(IGdiInterface)
    ['{47122E25-C0FE-4C5F-B515-AE00D3AA3573}']
    function NextSubPath(out StartIndex, EndIndex: Integer; out IsClosed: Boolean): Integer; overload;
    function NextSubPath(Path: IGdiGraphicsPath; out IsClosed: Boolean): Integer; overload;
    function NextPathType(out PathType: TPathPointType; out StartIndex, EndIndex: Integer): Integer;
    function NextMarker(out StartIndex, EndIndex: Integer): Integer; overload;
    function NextMarker(Path: IGdiGraphicsPath): Integer; overload;
    function GetCount: Integer;
    function GetSubPathCount: Integer;
    function HasCurve: Boolean;
    procedure Rewind;
    function Enumerate(Points: PPointF; Types: PByte; Count: Integer): Integer;
    function CopyData(Points: PPointF; Types: PByte; StartIndex, EndIndex: Integer): Integer;
  end;

{ IGdiGraphics }

  IGdiGraphics = interface(IGdiInterface)
    ['{2B6ED984-40B9-4F9F-B67B-E30B68AA3C04}']
    function GetNativeGraphics: GpGraphics;
    procedure SetNativeGraphics(Graphics: GpGraphics);
    function FromHDC(DC: HDC): IGdiGraphics; overload;
    function FromHDC(DC: HDC; hdevice: THandle): IGdiGraphics; overload;
    function FromHWND(hwnd: HWND; ICM: Boolean = False): IGdiGraphics;
    function FromImage(Image: IGdiImage): IGdiGraphics;
    procedure Flush(intention: TFlushIntention = FlushIntentionFlush);
    function GetHDC: HDC;
    procedure ReleaseHDC(DC: HDC);
    function SetRenderingOrigin(X, Y: Integer): TStatus;
    function GetRenderingOrigin(out X, Y: Integer): TStatus;
    function SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
    function GetCompositingMode: TCompositingMode;
    function SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
    function GetCompositingQuality: TCompositingQuality;
    procedure SetTextRenderingHint(NewMode: TTextRenderingHint);
    function GetTextRenderingHint: TTextRenderingHint;
    function SetTextContrast(contrast: UInt): TStatus;
    function GetTextContrast: UInt;
    function GetInterpolationMode: TInterpolationMode;
    function SetInterpolationMode(interpolationMode: TInterpolationMode): TStatus;
    function GetSmoothingMode: TSmoothingMode;
    procedure SetSmoothingMode(smoothingMode: TSmoothingMode);
    function GetPixelOffsetMode: TPixelOffsetMode;
    function SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
    procedure SetTransform(Matrix: IGdiMatrix);
    function  GetTransform: IGdiMatrix;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetPageUnit(Unit_: TUnit): TStatus;
    function SetPageScale(Scale: Single): TStatus;
    function GetPageUnit: TUnit;
    function GetPageScale: Single;
    function GetDPIX: Single;
    function GetDPIY: Single;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    function TransformPoints(DestSpace: TCoordinateSpace; SrcSpace: TCoordinateSpace;
      Pts: PPointF; Count: Integer): TStatus; overload;
    function TransformPoints(DestSpace: TCoordinateSpace; SrcSpace: TCoordinateSpace;
      Pts: PPointI; Count: Integer): TStatus; overload;
    function GetNearestColor(var Color: TArgb): TStatus;
    function Overlay(Wnd: HWND; Opacity: Byte = 0): TStatus;
    function Draw(DC: HDC; X, Y: Integer): TStatus; overload;
    function Draw(DC: HDC; X, Y, Width, Height: Integer): TStatus; overload;
    function Draw(DC: HDC; const Rect: TRectI): TStatus; overload;
    function DrawLine(Pen: IGdiPen; X1, Y1, X2, Y2: Single): TStatus; overload;
    function DrawLine(Pen: IGdiPen; const Pt1, Pt2: TPointF): TStatus; overload;
    function DrawLines(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawLine(Pen: IGdiPen; X1, Y1, X2, Y2: Integer): TStatus; overload;
    function DrawLine(Pen: IGdiPen; const Pt1, Pt2: TPointI): TStatus; overload;
    function DrawLines(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawArc(Pen: IGdiPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IGdiPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IGdiPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IGdiPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; const Pt1, Pt2, Pt3, Pt4: TPointF): TStatus; overload;
    function DrawBeziers(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; const Pt1, Pt2, Pt3, Pt4: TPointI): TStatus; overload;
    function DrawBeziers(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; const Rect: TRectF): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawRectangles(Pen: IGdiPen; Rects: PRectF; Count: Integer): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; const Rect: TRectI): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawRectangles(Pen: IGdiPen; Rects: PRectI; Count: Integer): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; const Rect: TRectF): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; const Rect: TRectI): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawPie(Pen: IGdiPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IGdiPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IGdiPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IGdiPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPolygon(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawPolygon(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawPath(Pen: IGdiPen; Path: IGdiGraphicsPath): TStatus;
    function DrawCurve(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointF; Count, Offset,
      NumberOfSegments: Integer; Tension: Single = 0.5): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointI; Count, Offset, NumberOfSegments: Integer;
      Tension: Single = 0.5): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function DrawCachedBitmap(Bitmap: IGdiCachedBitmap; X, Y: Integer): TStatus;
    function DrawImage(Image: IGdiImage; const Point: TPointF): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y: Single): TStatus; overload;
    function DrawImage(Image: IGdiImage; const Rect: TRectF): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, Width, Height: Single): TStatus; overload;
    function DrawImage(Image: IGdiImage; const Point: TPointI): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; const Rect: TRectI): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointF; Count: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointI; Count: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit): TStatus; overload;
    function DrawImage(Image: IGdiImage; const DestRect: TRectF; SrcX, SrcY,
      SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointF; Count: Integer;
      SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, SrcX, SrcY, SrcWidth,
      SrcHeight: Integer; SrcUnit: TUnit): TStatus; overload;
    function DrawImage(Image: IGdiImage; const DestRect: TRectI; SrcX, SrcY,
      SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointI;
      Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function Clear(Color: TArgb): TStatus;
    function FillRectangle(Brush: IGdiBrush; const Rect: TRectF): TStatus; overload;
    function FillRectangle(Brush: IGdiBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillRectangles(Brush: IGdiBrush; Rects: PRectF; Count: Integer): TStatus; overload;
    function FillRectangle(Brush: IGdiBrush; const Rect: TRectI): TStatus; overload;
    function FillRectangle(Brush: IGdiBrush; X, Y, Width, Height: Integer): TStatus; overload;
    function FillRectangles(Brush: IGdiBrush; Rects: PRectI; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointF; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointF; Count: Integer; FillMode: TFillMode): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointI; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointI; Count: Integer; FillMode: TFillMode): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; const Rect: TRectF): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; const Rect: TRectI): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; X, Y, Width, Height: Integer): TStatus; overload;
    function FillPie(Brush: IGdiBrush; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IGdiBrush; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IGdiBrush; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IGdiBrush; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPath(Brush: IGdiBrush; Path: IGdiGraphicsPath): TStatus;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointF; Count: Integer): TStatus; overload;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointF; Count: Integer;
      FillMode: TFillMode; Tension: Single = 0.5): TStatus; overload;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointI; Count: Integer): TStatus; overload;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointI; Count: Integer;
      FillMode: TFillMode; Tension: Single = 0.5): TStatus; overload;
    function FillRegion(Brush: IGdiBrush; Region: IGdiRegion): TStatus;
    function DrawString(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; StringFormat: IGdiStringFormat; Brush: IGdiBrush): TStatus; overload;
    function DrawString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; Brush: IGdiBrush): TStatus; overload;
    function DrawString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; StringFormat: IGdiStringFormat; Brush: IGdiBrush): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; StringFormat: IGdiStringFormat; out BoundingBox: TRectF;
      CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const LayoutRectSize: TSizeF; StringFormat: IGdiStringFormat; out Size: TSizeF;
      CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; StringFormat: IGdiStringFormat;
      out BoundingBox: TRectF): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; out BoundingBox: TRectF): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; out BoundingBox: TRectF): TStatus; overload;
    function MeasureCharacterRanges(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; StringFormat: IGdiStringFormat; RegionCount: Integer;
      const Regions: array of IGdiRegion): TStatus; overload;
    function DrawDriverString(Text: PUInt16; Length: Integer; Font: IGdiFont;
      Brush: IGdiBrush; Positions: PPointF; Flags: Integer; Matrix: IGdiMatrix): TStatus;
    function MeasureDriverString(Text: PUInt16; Length: Integer; Font: IGdiFont;
      Positions: PPointF; Flags: Integer; Matrix: IGdiMatrix;
      out BoundingBox: TRectF): TStatus;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointF;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointI;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectF;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectI;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointF;
      Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointI;
      Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointF;
      const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointI;
      const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectF;
      const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect, SrcRect: TRectI;
      SrcUnit: TUnit; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointF;
      Count: Integer; const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointI;
      Count: Integer; const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function SetClip(G: IGdiGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TRectI; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Path: IGdiGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Region: IGdiRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rgn: HRGN; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function IntersectClip(const Rect: TRectF): TStatus; overload;
    function IntersectClip(const Rect: TRectI): TStatus; overload;
    function IntersectClip(Region: IGdiRegion): TStatus; overload;
    function ExcludeClip(const Rect: TRectF): TStatus; overload;
    function ExcludeClip(const Rect: TRectI): TStatus; overload;
    function ExcludeClip(Region: IGdiRegion): TStatus; overload;
    function ResetClip: TStatus;
    function TranslateClip(DX, DY: Single): TStatus; overload;
    function TranslateClip(DX, DY: Integer): TStatus; overload;
    function GetClip(Region: IGdiRegion): TStatus;
    function GetClipBounds(out Rect: TRectF): TStatus; overload;
    function GetClipBounds(out Rect: TRectI): TStatus; overload;
    function IsClipEmpty: Boolean;
    function GetVisibleClipBounds(out Rect: TRectF): TStatus; overload;
    function GetVisibleClipBounds(out Rect: TRectI): TStatus; overload;
    function IsVisibleClipEmpty: Boolean;
    function IsVisible(X, Y: Integer): Boolean; overload;
    function IsVisible(const Point: TPointI): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Integer): Boolean; overload;
    function IsVisible(const Rect: TRectI): Boolean; overload;
    function IsVisible(X, Y: Single): Boolean; overload;
    function IsVisible(const Point: TPointF): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Single): Boolean; overload;
    function IsVisible(const Rect: TRectF): Boolean; overload;
    function Save: GraphicsState;
    function Restore(gstate: GraphicsState): TStatus;
    function BeginContainer(const DstRect, SrcRect: TRectF; Unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer(const DstRect, SrcRect: TRectI; Unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer: GraphicsContainer; overload;
    function EnDContainer(state: GraphicsContainer): TStatus;
    function AddMetafileComment(data: PByte; SizeData: UInt): TStatus;
    function GetHalftonePalette: HPALETTE;
    property NativeGraphics: GpGraphics read GetNativeGraphics write SetNativeGraphics;
    property Opacity: Byte read GetOpacity write SetOpacity;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property TextRenderingHint: TTextRenderingHint read GetTextRenderingHint write SetTextRenderingHint;
    property SmoothingMode: TSmoothingMode read GetSmoothingMode write SetSmoothingMode;
    property Transform: IGdiMatrix read GetTransform write SetTransform;
  end;

{ IGdiMetafile }

  IGdiMetafile = interface(IGdiImage)
    ['{6D48F2AA-FD33-4602-85DE-21176880A261}']
    function GetHENHMETAFILE: HENHMETAFILE;
    function PlayRecord(recordType: TEmfPlusRecordType; Flags, dataSize: UInt; data: PByte): TStatus;
    function SetDownLevelRasterizationLimit(MetafileRasterizationLimitDPI: UInt): TStatus;
    function GetDownLevelRasterizationLimit: UInt;
    function EmfToWmfBits(Emf: HENHMETAFILE; cbData16: UInt; pData16: PByte;
      MapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UInt;
  end;

{ IGdiFontFamily }

  IGdiFontFamily = interface(IGdiInterface)
    ['{42D3A3EA-C7CB-4556-B63C-04A55518487C}']
    function GetNativeFamily: GpFontFamily;
    procedure SetNativeFamily(Value: GpFontFamily);
    function GetFamilyName(out Name: string; Language: LangId = 0): TStatus;
    function Clone: IGdiFontFamily;
    function IsAvailable: Boolean;
    function IsStyleAvailable(Style: Integer): Boolean;
    function GetEmHeight(Style: Integer): UInt16;
    function GetCellAscent(Style: Integer): UInt16;
    function GetCellDescent(Style: Integer): UInt16;
    function GetLineSpacing(Style: Integer): UInt16;
    property NativeFamily: GpFontFamily read GetNativeFamily write SetNativeFamily;
  end;

{ IGdiFontCollection }

  IGdiFontCollection = interface(IGdiInterface)
    ['{0DB76E8B-3D3C-4BC5-B332-AD068BC67516}']
    function GetNativeFontCollection: GpFontCollection;
    procedure SetNativeFontCollection(Value: GpFontCollection);
    function GetFamilyCount: Integer;
    function GetFamilies(NumSought: Integer; out Families: array of IGdiFontFamily;
      out NumFound: Integer): TStatus;
    property NativeFontCollection: GpFontCollection read GetNativeFontCollection write SetNativeFontCollection;
  end;

{ IGdiInstalledFontCollection }

  IGdiInstalledFontCollection = interface(IGdiFontCollection)
    ['{60D3D4D9-7C9B-48F4-8FAF-1CFDA1DD6F84}']
  end;

{ IGdiPrivateFontCollection }

  IGdiPrivateFontCollection = interface(IGdiFontCollection)
    ['{B5FD887F-B2CA-4739-9337-A0C2687A360A}']
    function AddFontFile(Filename: WideString): TStatus;
    function AddMemoryFont(Memory: Pointer; Length: Integer): TStatus;
  end;

{ IGdiFont }

  IGdiFont = interface(IGdiInterface)
    ['{E77D1C63-C857-4AD5-9105-3E8FAE3D931E}']
    function GetNativeFont: GpFont;
    procedure SetNativeFont(Font: GpFont);
    function GetLogFontA(G: IGdiGraphics; out LogFontA: TLogFontA): TStatus;
    function GetLogFontW(G: IGdiGraphics; out LogFontW: TLogFontW): TStatus;
    function Clone: IGdiFont;
    function IsAvailable: Boolean;
    function GetStyle: Integer;
    function GetSize: Single;
    function GetUnit: TUnit;
    function GetHeight(Graphics: IGdiGraphics): Single; overload;
    function GetHeight(DPI: Single): Single; overload;
    function GetFamily(Family: IGdiFontFamily): TStatus;
    property NativeFont: GpFont read GetNativeFont write SetNativeFont;
  end;

{ IGdiCustomLineCap }

  IGdiCustomLineCap = interface(IGdiInterface)
    ['{B1EB7F58-B814-408C-B0FA-AE0517DC8671}']
    function GetNativeCap: GpCustomLineCap;
    procedure SetNativeCap(Cap: GpCustomLineCap);
    function Clone: IGdiCustomLineCap;
    function SetStrokeCap(StrokeCap: TLineCap): TStatus;
    function SetStrokeCaps(StartCap, EndCap: TLineCap): TStatus;
    function GetStrokeCaps(out StartCap, EndCap: TLineCap): TStatus;
    function SetStrokeJoin(lineJoin: TLineJoin): TStatus;
    function GetStrokeJoin: TLineJoin;
    function SetBaseCap(BaseCap: TLineCap): TStatus;
    function GetBaseCap: TLineCap;
    function SetBaseInset(Inset: Single): TStatus;
    function GetBaseInset: Single;
    function SetWidthScale(WidthScale: Single): TStatus;
    function GetWidthScale: Single;
    property NativeCap: GpCustomLineCap read GetNativeCap write SetNativeCap;
  end;

{ IGdiAdjustableArrowCap }

  IGdiAdjustableArrowCap = interface(IGdiCustomLineCap)
    ['{B1EB7F58-B814-408C-B0FA-AE0517DC8671}']
    function SetHeight(Height: Single): TStatus;
    function GetHeight: Single;
    function SetWidth(Width: Single): TStatus;
    function GetWidth: Single;
    function SetMiddleInset(middleInset: Single): TStatus;
    function GetMiddleInset: Single;
    function SetFillState(IsFilled: Boolean): TStatus;
    function IsFilled: Boolean;
  end;

{ IGdiStringFormat }

  IGdiStringFormat = interface(IGdiInterface)
    ['{0775F556-63E2-49C6-83A7-14E411D0D338}']
    function GetNativeFormat: GpStringFormat;
    procedure SetNativeFormat(Value: GpStringFormat);
    function Clone: IGdiStringFormat;
    function SetFormatFlags(Flags: Integer): TStatus;
    function GetFormatFlags: Integer;
    function SetAlignment(Align: TStringAlignment): TStatus;
    function GetAlignment: TStringAlignment;
    function SetLineAlignment(Align: TStringAlignment): TStatus;
    function GetLineAlignment: TStringAlignment;
    function SetHotkeyPrefix(HotkeyPrefix: THotkeyPrefix): TStatus;
    function GetHotkeyPrefix: THotkeyPrefix;
    function SetTabStops(FirstTabOffset: Single; Count: Integer; TabStops: PSingle): TStatus;
    function GetTabStopCount: Integer;
    function GetTabStops(Count: Integer; FirstTabOffset, TabStops: PSingle): TStatus;
    function SetDigitSubstitution(Language: LangId; substitute: TStringDigitSubstitute): TStatus;
    function GetDigitSubstitutionLanguage: LangId;
    function GetDigitSubstitutionMethod: TStringDigitSubstitute;
    function SetTrimming(Trimming: TStringTrimming): TStatus;
    function GetTrimming: TStringTrimming;
    function SetMeasurableCharacterRanges(rangeCount: Integer; ranges: PCharacterRange): TStatus;
    function GetMeasurableCharacterRangeCount: Integer;
    property NativeFormat: GpStringFormat read GetNativeFormat write SetNativeFormat;
  end;

{ Value type creation routines }

function NewPointI(X, Y: Integer): TPointI;
function NewPointF(X, Y: Single): TPointF;

function NewSizeI(Width, Height: Integer): TSizeI; overload;
function NewSizeI(const Size: TSizeF): TSizeI; overload;
function NewSizeF(Width, Height: Single): TSizeF; overload;
function NewSizeF(const Size: TSizeI): TSizeF; overload;

function NewRectI(X, Y, Width, Height: Integer): TRectI; overload;
function NewRectI(Width, Height: Integer): TRectI; overload;
function NewRectI(const R: TRect): TRectI; overload;
function NewRectF(X, Y, Width, Height: Single): TRectF; overload;
function NewRectF(Width, Height: Single): TRectF; overload;
function NewRectF(const R: TRect): TRectF; overload;
function NewRectF(const R: TRectI): TRectF; overload;

{ Instance type creation routines }

function NewGraphics(DC: HDC): IGdiGraphics; overload;
function NewGraphics(Image: IGdiImage): IGdiGraphics; overload;
function NewGraphics(Width, Height: Integer): IGdiGraphics; overload;
function NewPen(RGBA: TArgb; StrokeWidth: Single = 1): IGdiPen;
function NewSolidBrush(RGBA: TArgb): IGdiSolidBrush;
function NewHatchBrush(Style: THatchStyle; ForeColor: TArgb): IGdiHatchBrush; overload;
function NewHatchBrush(Style: THatchStyle; ForeColor: TArgb; BackColor: TArgb): IGdiHatchBrush; overload;
function NewLinearGradientBrush(const Rect: TRectI; Angle: Single; C1, C2: TArgb): IGdiLinearGradientBrush; overload;
function NewLinearGradientBrush(const Rect: TRectF; Angle: Single; C1, C2: TArgb): IGdiLinearGradientBrush; overload;
function NewLinearGradientBrush(const Rect: TRectF; Angle: Single; Colors: array of TArgb; Stops: array of Single): IGdiLinearGradientBrush; overload;
function NewCheckerBrush(C1, C2: TArgb; Size: Integer): IGdiTextureBrush;
function NewFont(Font: HFont): IGdiFont; overload;
function NewFont(DC: HDC; Font: HFont): IGdiFont; overload;
function NewFont(const Name: string; Size: Single; Style: TFontStyle; Unit_: TUnit = UnitPoint): IGdiFont; overload;
function NewStringFormat: IGdiStringFormat; overload;
function NewStringFormat(HAlign, VAlign: TStringAlignment; Wrap: Boolean = False): IGdiStringFormat; overload;
function NewBitmap(Width, Height: Integer): IGdiBitmap; overload;
function NewBitmap(Width, Height: Integer; PixelFormat: TPixelFormat): IGdiBitmap; overload;
function NewBitmap(Width, Height, Stride: Integer; PixelFormat: TPixelFormat; ScanLine: PByte): IGdiBitmap; overload;
function NewBitmap(Width, Height: Integer; Target: IGdiGraphics): IGdiBitmap; overload;
function NewBitmap(Stream: IStream): IGdiBitmap; overload;
function NewBitmap(const FileName: string): IGdiBitmap; overload;
function NewBitmap(Bitmap: HBITMAP): IGdiBitmap; overload;
function NewImageAttributes: IGdiImageAttributes; overload;
function NewImageAttributes(Transform: TColorTransform): IGdiImageAttributes; overload;
function NewGraphicsPath: IGdiGraphicsPath;
function NewMatrix: IGdiMatrix;

{ API functions }

function GdiplusStartup(out token: ULONG; input: PGdiPlusStartupInput; output: PGdiPlusStartupOutput): GpStatus; stdcall;
procedure GdiplusShutdown(token: ULONG); stdcall;
function GdipAlloc(size: ULONG): Pointer; stdcall;
procedure GdipFree(ptr: Pointer); stdcall;
function GdipCreatePath(brushMode: TFillMode; out path: GpPath): GpStatus; stdcall;
function GdipCreatePath2(v1: PPointF; v2: PByte; v3: Integer; v4: TFillMode; out path: GpPath): GpStatus; stdcall;
function GdipCreatePath2I(v1: PPointI; v2: PByte; v3: Integer; v4: TFillMode; out path: GpPath): GpStatus; stdcall;
function GdipClonePath(path: GpPath; out clonePath: GpPath): GpStatus; stdcall;
function GdipDeletePath(path: GpPath): GpStatus; stdcall;
function GdipResetPath(path: GpPath): GpStatus; stdcall;
function GdipGetPointCount(path: GpPath; out count: Integer): GpStatus; stdcall;
function GdipGetPathTypes(path: GpPath; types: PByte; count: Integer): GpStatus; stdcall;
function GdipGetPathPoints(v1: GpPath; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipGetPathPointsI(v1: GpPath; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipGetPathFillMode(path: GpPath; var fillmode: TFillMode): GpStatus; stdcall;
function GdipSetPathFillMode(path: GpPath; fillmode: TFillMode): GpStatus; stdcall;
function GdipGetPathData(path: GpPath; pathData: Pointer): GpStatus; stdcall;
function GdipStartPathFigure(path: GpPath): GpStatus; stdcall;
function GdipClosePathFigure(path: GpPath): GpStatus; stdcall;
function GdipClosePathFigures(path: GpPath): GpStatus; stdcall;
function GdipSetPathMarker(path: GpPath): GpStatus; stdcall;
function GdipClearPathMarkers(path: GpPath): GpStatus; stdcall;
function GdipReversePath(path: GpPath): GpStatus; stdcall;
function GdipGetPathLastPoint(path: GpPath; lastPoint: PRectF): GpStatus; stdcall;
function GdipAddPathLine(path: GpPath; x1, y1, x2, y2: Single): GpStatus; stdcall;
function GdipAddPathLine2(path: GpPath; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipAddPathArc(path: GpPath; x, y, width, height, startAngle, sweepAngle: Single): GpStatus; stdcall;
function GdipAddPathBezier(path: GpPath; x1, y1, x2, y2, x3, y3, x4, y4: Single): GpStatus; stdcall;
function GdipAddPathBeziers(path: GpPath; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipAddPathCurve(path: GpPath; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipAddPathCurve2(path: GpPath; points: PPointF; count: Integer; tension: Single): GpStatus; stdcall;
function GdipAddPathCurve3(path: GpPath; points: PPointF; count: Integer; offset: Integer; numberOfSegments: Integer; tension: Single): GpStatus; stdcall;
function GdipAddPathClosedCurve(path: GpPath; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipAddPathClosedCurve2(path: GpPath; points: PPointF; count: Integer; tension: Single): GpStatus; stdcall;
function GdipAddPathRectangle(path: GpPath; x: Single; y: Single; width: Single; height: Single): GpStatus; stdcall;
function GdipAddPathRectangles(path: GpPath; rects: PRectF; count: Integer): GpStatus; stdcall;
function GdipAddPathEllipse(path: GpPath; x: Single; y: Single; width: Single; height: Single): GpStatus; stdcall;
function GdipAddPathPie(path: GpPath; x: Single; y: Single; width: Single; height: Single; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipAddPathPolygon(path: GpPath; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipAddPathPath(path: GpPath; addingPath: GpPath; connect: Bool): GpStatus; stdcall;
function GdipAddPathString(path: GpPath; string_: PWChar; length: Integer; family: GpFontFamily; style: Integer; emSize: Single; layoutRect: PRectF; format: GpStringFormat): GpStatus; stdcall;
function GdipAddPathStringI(path: GpPath; string_: PWChar; length: Integer; family: GpFontFamily; style: Integer; emSize: Single; layoutRect: PRectI; format: GpStringFormat): GpStatus; stdcall;
function GdipAddPathLineI(path: GpPath; x1: Integer; y1: Integer; x2: Integer; y2: Integer): GpStatus; stdcall;
function GdipAddPathLine2I(path: GpPath; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipAddPathArcI(path: GpPath; x: Integer; y: Integer; width: Integer; height: Integer; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipAddPathBezierI(path: GpPath; x1: Integer; y1: Integer; x2: Integer; y2: Integer; x3: Integer; y3: Integer; x4: Integer; y4: Integer): GpStatus; stdcall;
function GdipAddPathBeziersI(path: GpPath; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipAddPathCurveI(path: GpPath; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipAddPathCurve2I(path: GpPath; points: PPointI; count: Integer; tension: Single): GpStatus; stdcall;
function GdipAddPathCurve3I(path: GpPath; points: PPointI; count: Integer; offset: Integer; numberOfSegments: Integer; tension: Single): GpStatus; stdcall;
function GdipAddPathClosedCurveI(path: GpPath; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipAddPathClosedCurve2I(path: GpPath; points: PPointI; count: Integer; tension: Single): GpStatus; stdcall;
function GdipAddPathRectangleI(path: GpPath; x: Integer; y: Integer; width: Integer; height: Integer): GpStatus; stdcall;
function GdipAddPathRectanglesI(path: GpPath; rects: PRectI; count: Integer): GpStatus; stdcall;
function GdipAddPathEllipseI(path: GpPath; x: Integer; y: Integer; width: Integer; height: Integer): GpStatus; stdcall;
function GdipAddPathPieI(path: GpPath; x: Integer; y: Integer; width: Integer; height: Integer; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipAddPathPolygonI(path: GpPath; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipFlattenPath(path: GpPath; matrix: GpMatrix; flatness: Single): GpStatus; stdcall;
function GdipWindingModeOutline(path: GpPath; matrix: GpMatrix; flatness: Single): GpStatus; stdcall;
function GdipWidenPath(nativePath: GpPath; pen: GpPen; matrix: GpMatrix; flatness: Single): GpStatus; stdcall;
function GdipWarpPath(path: GpPath; matrix: GpMatrix; points: PPointF; count: Integer; srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single; warpMode: WARPMODE; flatness: Single): GpStatus; stdcall;
function GdipTransformPath(path: GpPath; matrix: GpMatrix): GpStatus; stdcall;
function GdipGetPathWorldBounds(path: GpPath; bounds: PRectF; matrix: GpMatrix; pen: GpPen): GpStatus; stdcall;
function GdipGetPathWorldBoundsI(path: GpPath; bounds: PRectI; matrix: GpMatrix; pen: GpPen): GpStatus; stdcall;
function GdipIsVisiblePathPoint(path: GpPath; x: Single; y: Single; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsVisiblePathPointI(path: GpPath; x: Integer; y: Integer; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsOutlineVisiblePathPoint(path: GpPath; x: Single; y: Single; pen: GpPen; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsOutlineVisiblePathPointI(path: GpPath; x: Integer; y: Integer; pen: GpPen; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipCreatePathIter(out iterator: GpPathIterator; path: GpPath): GpStatus; stdcall;
function GdipDeletePathIter(iterator: GpPathIterator): GpStatus; stdcall;
function GdipPathIterNextSubpath(iterator: GpPathIterator; var ResultCount: Integer; var startIndex: Integer; var endIndex: Integer; out isClosed: Bool): GpStatus; stdcall;
function GdipPathIterNextSubpathPath(iterator: GpPathIterator; var ResultCount: Integer; path: GpPath; out isClosed: Bool): GpStatus; stdcall;
function GdipPathIterNextPathType(iterator: GpPathIterator; var ResultCount: Integer; pathType: PByte; var startIndex: Integer; var endIndex: Integer): GpStatus; stdcall;
function GdipPathIterNextMarker(iterator: GpPathIterator; var ResultCount: Integer; var startIndex: Integer; var endIndex: Integer): GpStatus; stdcall;
function GdipPathIterNextMarkerPath(iterator: GpPathIterator; var ResultCount: Integer; path: GpPath): GpStatus; stdcall;
function GdipPathIterGetCount(iterator: GpPathIterator; out count: Integer): GpStatus; stdcall;
function GdipPathIterGetSubpathCount(iterator: GpPathIterator; out count: Integer): GpStatus; stdcall;
function GdipPathIterIsValid(iterator: GpPathIterator; out valid: Bool): GpStatus; stdcall;
function GdipPathIterHasCurve(iterator: GpPathIterator; out hasCurve: Bool): GpStatus; stdcall;
function GdipPathIterRewind(iterator: GpPathIterator): GpStatus; stdcall;
function GdipPathIterEnumerate(iterator: GpPathIterator; var ResultCount: Integer; points: PPointF; types: PByte; count: Integer): GpStatus; stdcall;
function GdipPathIterCopyData(iterator: GpPathIterator; var ResultCount: Integer; points: PPointF; types: PByte; startIndex: Integer; endIndex: Integer): GpStatus; stdcall;
function GdipCreateMatrix(out matrix: GpMatrix): GpStatus; stdcall;
function GdipCreateMatrix2(m11: Single; m12: Single; m21: Single; m22: Single; dx: Single; dy: Single; out matrix: GpMatrix): GpStatus; stdcall;
function GdipCreateMatrix3(rect: PRectF; dstplg: PRectF; out matrix: GpMatrix): GpStatus; stdcall;
function GdipCreateMatrix3I(rect: PRectI; dstplg: PPointI; out matrix: GpMatrix): GpStatus; stdcall;
function GdipCloneMatrix(matrix: GpMatrix; out cloneMatrix: GpMatrix): GpStatus; stdcall;
function GdipDeleteMatrix(matrix: GpMatrix): GpStatus; stdcall;
function GdipSetMatrixElements(matrix: GpMatrix; m11: Single; m12: Single; m21: Single; m22: Single; dx: Single; dy: Single): GpStatus; stdcall;
function GdipMultiplyMatrix(matrix: GpMatrix; matrix2: GpMatrix; order: TMatrixOrder): GpStatus; stdcall;
function GdipTranslateMatrix(matrix: GpMatrix; offsetX: Single; offsetY: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipScaleMatrix(matrix: GpMatrix; scaleX: Single; scaleY: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipRotateMatrix(matrix: GpMatrix; angle: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipShearMatrix(matrix: GpMatrix; shearX: Single; shearY: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipInvertMatrix(matrix: GpMatrix): GpStatus; stdcall;
function GdipTransformMatrixPoints(matrix: GpMatrix; pts: PPointF; count: Integer): GpStatus; stdcall;
function GdipTransformMatrixPointsI(matrix: GpMatrix; pts: PPointI; count: Integer): GpStatus; stdcall;
function GdipVectorTransformMatrixPoints(matrix: GpMatrix; pts: PPointF; count: Integer): GpStatus; stdcall;
function GdipVectorTransformMatrixPointsI(matrix: GpMatrix; pts: PPointI; count: Integer): GpStatus; stdcall;
function GdipGetMatrixElements(matrix: GpMatrix; matrixOut: PSingle): GpStatus; stdcall;
function GdipIsMatrixInvertible(matrix: GpMatrix; out Result: Bool): GpStatus; stdcall;
function GdipIsMatrixIdentity(matrix: GpMatrix; out Result: Bool): GpStatus; stdcall;
function GdipIsMatrixEqual(matrix: GpMatrix; matrix2: GpMatrix; out Result: Bool): GpStatus; stdcall;
function GdipCreateRegion(out region: GpRegion): GpStatus; stdcall;
function GdipCreateRegionRect(rect: PRectF; out region: GpRegion): GpStatus; stdcall;
function GdipCreateRegionRectI(rect: PRectI; out region: GpRegion): GpStatus; stdcall;
function GdipCreateRegionPath(path: GpPath; out region: GpRegion): GpStatus; stdcall;
function GdipCreateRegionRgnData(regionData: PByte; size: Integer; out region: GpRegion): GpStatus; stdcall;
function GdipCreateRegionHrgn(hRgn: HRGN; out region: GpRegion): GpStatus; stdcall;
function GdipCloneRegion(region: GpRegion; out cloneRegion: GpRegion): GpStatus; stdcall;
function GdipDeleteRegion(region: GpRegion): GpStatus; stdcall;
function GdipSetInfinite(region: GpRegion): GpStatus; stdcall;
function GdipSetEmpty(region: GpRegion): GpStatus; stdcall;
function GdipCombineRegionRect(region: GpRegion; rect: PRectF; CombineMode: CombineMode): GpStatus; stdcall;
function GdipCombineRegionRectI(region: GpRegion; rect: PRectI; CombineMode: CombineMode): GpStatus; stdcall;
function GdipCombineRegionPath(region: GpRegion; path: GpPath; CombineMode: CombineMode): GpStatus; stdcall;
function GdipCombineRegionRegion(region: GpRegion; region2: GpRegion; CombineMode: CombineMode): GpStatus; stdcall;
function GdipTranslateRegion(region: GpRegion; dx: Single; dy: Single): GpStatus; stdcall;
function GdipTranslateRegionI(region: GpRegion; dx: Integer; dy: Integer): GpStatus; stdcall;
function GdipTransformRegion(region: GpRegion; matrix: GpMatrix): GpStatus; stdcall;
function GdipGetRegionBounds(region: GpRegion; graphics: GpGraphics; rect: PRectF): GpStatus; stdcall;
function GdipGetRegionBoundsI(region: GpRegion; graphics: GpGraphics; rect: PRectI): GpStatus; stdcall;
function GdipGetRegionHRgn(region: GpRegion; graphics: GpGraphics; out hRgn: HRGN): GpStatus; stdcall;
function GdipIsEmptyRegion(region: GpRegion; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsInfiniteRegion(region: GpRegion; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsEqualRegion(region: GpRegion; region2: GpRegion; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipGetRegionDataSize(region: GpRegion; out bufferSize: UInt): GpStatus; stdcall;
function GdipGetRegionData(region: GpRegion; buffer: PByte; bufferSize: UInt; sizeFilled: PUINT): GpStatus; stdcall;
function GdipIsVisibleRegionPoint(region: GpRegion; x: Single; y: Single; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsVisibleRegionPointI(region: GpRegion; x: Integer; y: Integer; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsVisibleRegionRect(region: GpRegion; x: Single; y: Single; width: Single; height: Single; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipIsVisibleRegionRectI(region: GpRegion; x: Integer; y: Integer; width: Integer; height: Integer; graphics: GpGraphics; out Result: Bool): GpStatus; stdcall;
function GdipGetRegionScansCount(region: GpRegion; out count: UInt; matrix: GpMatrix): GpStatus; stdcall;
function GdipGetRegionScans(region: GpRegion; rects: PRectF; out count: Integer; matrix: GpMatrix): GpStatus; stdcall;
function GdipGetRegionScansI(region: GpRegion; rects: PRectI; out count: Integer; matrix: GpMatrix): GpStatus; stdcall;
function GdipCloneBrush(brush: GpBrush; out cloneBrush: GpBrush): GpStatus; stdcall;
function GdipDeleteBrush(brush: GpBrush): GpStatus; stdcall;
function GdipGetBrushType(brush: GpBrush; out type_: TBrushType): GpStatus; stdcall;
function GdipCreateHatchBrush(hatchstyle: Integer; forecol: TArgb; backcol: TArgb; out brush: GpHatch): GpStatus; stdcall;
function GdipGetHatchStyle(brush: GpHatch; out hatchstyle: THatchStyle): GpStatus; stdcall;
function GdipGetHatchForegroundColor(brush: GpHatch; out forecol: TArgb): GpStatus; stdcall;
function GdipGetHatchBackgroundColor(brush: GpHatch; out backcol: TArgb): GpStatus; stdcall;
function GdipCreateTexture(image: GpImage; wrapmode: TWrapMode; var texture: GpTexture): GpStatus; stdcall;
function GdipCreateTexture2(image: GpImage; wrapmode: TWrapMode; x: Single; y: Single; width: Single; height: Single; out texture: GpTexture): GpStatus; stdcall;
function GdipCreateTextureIA(image: GpImage; imageAttributes: GpImageAttributes; x: Single; y: Single; width: Single; height: Single; out texture: GpTexture): GpStatus; stdcall;
function GdipCreateTexture2I(image: GpImage; wrapmode: TWrapMode; x: Integer; y: Integer; width: Integer; height: Integer; out texture: GpTexture): GpStatus; stdcall;
function GdipCreateTextureIAI(image: GpImage; imageAttributes: GpImageAttributes; x: Integer; y: Integer; width: Integer; height: Integer; out texture: GpTexture): GpStatus; stdcall;
function GdipGetTextureTransform(brush: GpTexture; matrix: GpMatrix): GpStatus; stdcall;
function GdipSetTextureTransform(brush: GpTexture; matrix: GpMatrix): GpStatus; stdcall;
function GdipResetTextureTransform(brush: GpTexture): GpStatus; stdcall;
function GdipMultiplyTextureTransform(brush: GpTexture; matrix: GpMatrix; order: TMatrixOrder): GpStatus; stdcall;
function GdipTranslateTextureTransform(brush: GpTexture; dx: Single; dy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipScaleTextureTransform(brush: GpTexture; sx: Single; sy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipRotateTextureTransform(brush: GpTexture; angle: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipSetTextureWrapMode(brush: GpTexture; wrapmode: TWrapMode): GpStatus; stdcall;
function GdipGetTextureWrapMode(brush: GpTexture; var wrapmode: TWrapMode): GpStatus; stdcall;
function GdipGetTextureImage(brush: GpTexture; out image: GpImage): GpStatus; stdcall;
function GdipCreateSolidFill(Color: TArgb; out brush: GpSolidFill): GpStatus; stdcall;
function GdipSetSolidFillColor(brush: GpSolidFill; Color: TArgb): GpStatus; stdcall;
function GdipGetSolidFillColor(brush: GpSolidFill; out Color: TArgb): GpStatus; stdcall;
function GdipCreateLineBrush(point1: PRectF; point2: PRectF; Color1: TArgb; Color2: TArgb; wrapMode: TWrapMode; out lineGradient: GpLineGradient): GpStatus; stdcall;
function GdipCreateLineBrushI(point1: PPointI; point2: PPointI; Color1: TArgb; Color2: TArgb; wrapMode: TWrapMode; out lineGradient: GpLineGradient): GpStatus; stdcall;
function GdipCreateLineBrushFromRect(rect: PRectF; Color1: TArgb; Color2: TArgb; mode: LinearGradientMode; wrapMode: TWrapMode; out lineGradient: GpLineGradient): GpStatus; stdcall;
function GdipCreateLineBrushFromRectI(rect: PRectI; Color1: TArgb; Color2: TArgb; mode: LinearGradientMode; wrapMode: TWrapMode; out lineGradient: GpLineGradient): GpStatus; stdcall;
function GdipCreateLineBrushFromRectWithAngle(rect: PRectF; Color1: TArgb; Color2: TArgb; angle: Single; isAngleScalable: Bool; wrapMode: TWrapMode; out lineGradient: GpLineGradient): GpStatus; stdcall;
function GdipCreateLineBrushFromRectWithAngleI(rect: PRectI; Color1: TArgb; Color2: TArgb; angle: Single; isAngleScalable: Bool; wrapMode: TWrapMode; out lineGradient: GpLineGradient): GpStatus; stdcall;
function GdipSetLineColors(brush: GpLineGradient; Color1: TArgb; Color2: TArgb): GpStatus; stdcall;
function GdipGetLineColors(brush: GpLineGradient; Colors: PArgb): GpStatus; stdcall;
function GdipGetLineRect(brush: GpLineGradient; rect: PRectF): GpStatus; stdcall;
function GdipGetLineRectI(brush: GpLineGradient; rect: PRectI): GpStatus; stdcall;
function GdipSetLineGammaCorrection(brush: GpLineGradient; useGammaCorrection: Bool): GpStatus; stdcall;
function GdipGetLineGammaCorrection(brush: GpLineGradient; out useGammaCorrection: Bool): GpStatus; stdcall;
function GdipGetLineBlendCount(brush: GpLineGradient; out count: Integer): GpStatus; stdcall;
function GdipGetLineBlend(brush: GpLineGradient; blend: PSingle; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipSetLineBlend(brush: GpLineGradient; blend: PSingle; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipGetLinePresetBlendCount(brush: GpLineGradient; out count: Integer): GpStatus; stdcall;
function GdipGetLinePresetBlend(brush: GpLineGradient; blend: PArgb; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipSetLinePresetBlend(brush: GpLineGradient; blend: PArgb; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipSetLineSigmaBlend(brush: GpLineGradient; focus: Single; scale: Single): GpStatus; stdcall;
function GdipSetLineLinearBlend(brush: GpLineGradient; focus: Single; scale: Single): GpStatus; stdcall;
function GdipSetLineWrapMode(brush: GpLineGradient; wrapmode: TWrapMode): GpStatus; stdcall;
function GdipGetLineWrapMode(brush: GpLineGradient; out wrapmode: TWrapMode): GpStatus; stdcall;
function GdipGetLineTransform(brush: GpLineGradient; matrix: GpMatrix): GpStatus; stdcall;
function GdipSetLineTransform(brush: GpLineGradient; matrix: GpMatrix): GpStatus; stdcall;
function GdipResetLineTransform(brush: GpLineGradient): GpStatus; stdcall;
function GdipMultiplyLineTransform(brush: GpLineGradient; matrix: GpMatrix; order: TMatrixOrder): GpStatus; stdcall;
function GdipTranslateLineTransform(brush: GpLineGradient; dx: Single; dy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipScaleLineTransform(brush: GpLineGradient; sx: Single; sy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipRotateLineTransform(brush: GpLineGradient; angle: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipCreatePathGradient(points: PPointF; count: Integer; wrapMode: TWrapMode; out polyGradient: GpPathGradient): GpStatus; stdcall;
function GdipCreatePathGradientI(points: PPointI; count: Integer; wrapMode: TWrapMode; out polyGradient: GpPathGradient): GpStatus; stdcall;
function GdipCreatePathGradientFromPath(path: GpPath; out polyGradient: GpPathGradient): GpStatus; stdcall;
function GdipGetPathGradientCenterColor(brush: GpPathGradient; out Colors: TArgb): GpStatus; stdcall;
function GdipSetPathGradientCenterColor(brush: GpPathGradient; Colors: TArgb): GpStatus; stdcall;
function GdipGetPathGradientSurroundColorsWithCount(brush: GpPathGradient; Color: PArgb; var count: Integer): GpStatus; stdcall;
function GdipSetPathGradientSurroundColorsWithCount(brush: GpPathGradient; Color: PArgb; var count: Integer): GpStatus; stdcall;
function GdipGetPathGradientPath(brush: GpPathGradient; path: GpPath): GpStatus; stdcall;
function GdipSetPathGradientPath(brush: GpPathGradient; path: GpPath): GpStatus; stdcall;
function GdipGetPathGradientCenterPoint(brush: GpPathGradient; points: PPointF): GpStatus; stdcall;
function GdipGetPathGradientCenterPointI(brush: GpPathGradient; points: PPointI): GpStatus; stdcall;
function GdipSetPathGradientCenterPoint(brush: GpPathGradient; points: PPointF): GpStatus; stdcall;
function GdipSetPathGradientCenterPointI(brush: GpPathGradient; points: PPointI): GpStatus; stdcall;
function GdipGetPathGradientRect(brush: GpPathGradient; rect: PRectF): GpStatus; stdcall;
function GdipGetPathGradientRectI(brush: GpPathGradient; rect: PRectI): GpStatus; stdcall;
function GdipGetPathGradientPointCount(brush: GpPathGradient; var count: Integer): GpStatus; stdcall;
function GdipGetPathGradientSurroundColorCount(brush: GpPathGradient; var count: Integer): GpStatus; stdcall;
function GdipSetPathGradientGammaCorrection(brush: GpPathGradient; useGammaCorrection: Bool): GpStatus; stdcall;
function GdipGetPathGradientGammaCorrection(brush: GpPathGradient; var useGammaCorrection: Bool): GpStatus; stdcall;
function GdipGetPathGradientBlendCount(brush: GpPathGradient; var count: Integer): GpStatus; stdcall;
function GdipGetPathGradientBlend(brush: GpPathGradient; blend: PSingle; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipSetPathGradientBlend(brush: GpPathGradient; blend: PSingle; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipGetPathGradientPresetBlendCount(brush: GpPathGradient; var count: Integer): GpStatus; stdcall;
function GdipGetPathGradientPresetBlend(brush: GpPathGradient; blend: PArgb; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipSetPathGradientPresetBlend(brush: GpPathGradient; blend: PArgb; positions: PSingle; count: Integer): GpStatus; stdcall;
function GdipSetPathGradientSigmaBlend(brush: GpPathGradient; focus: Single; scale: Single): GpStatus; stdcall;
function GdipSetPathGradientLinearBlend(brush: GpPathGradient; focus: Single; scale: Single): GpStatus; stdcall;
function GdipGetPathGradientWrapMode(brush: GpPathGradient; var wrapmode: TWrapMode): GpStatus; stdcall;
function GdipSetPathGradientWrapMode(brush: GpPathGradient; wrapmode: TWrapMode): GpStatus; stdcall;
function GdipGetPathGradientTransform(brush: GpPathGradient; matrix: GpMatrix): GpStatus; stdcall;
function GdipSetPathGradientTransform(brush: GpPathGradient; matrix: GpMatrix): GpStatus; stdcall;
function GdipResetPathGradientTransform(brush: GpPathGradient): GpStatus; stdcall;
function GdipMultiplyPathGradientTransform(brush: GpPathGradient; matrix: GpMatrix; order: TMatrixOrder): GpStatus; stdcall;
function GdipTranslatePathGradientTransform(brush: GpPathGradient; dx: Single; dy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipScalePathGradientTransform(brush: GpPathGradient; sx: Single; sy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipRotatePathGradientTransform(brush: GpPathGradient; angle: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipGetPathGradientFocusScales(brush: GpPathGradient; var xScale: Single; var yScale: Single): GpStatus; stdcall;
function GdipSetPathGradientFocusScales(brush: GpPathGradient; xScale: Single; yScale: Single): GpStatus; stdcall;
function GdipCreatePen1(Color: TArgb; width: Single; unit_: TUnit; out pen: GpPen): GpStatus; stdcall;
function GdipCreatePen2(brush: GpBrush; width: Single; unit_: TUnit; out pen: GpPen): GpStatus; stdcall;
function GdipClonePen(pen: GpPen; out clonepen: GpPen): GpStatus; stdcall;
function GdipDeletePen(pen: GpPen): GpStatus; stdcall;
function GdipSetPenWidth(pen: GpPen; width: Single): GpStatus; stdcall;
function GdipGetPenWidth(pen: GpPen; out width: Single): GpStatus; stdcall;
function GdipSetPenUnit(pen: GpPen; unit_: TUnit): GpStatus; stdcall;
function GdipGetPenUnit(pen: GpPen; var unit_: TUnit): GpStatus; stdcall;
function GdipSetPenLineCap197819(pen: GpPen; startCap: TLineCap; endCap: TLineCap; dashCap: TDashCap): GpStatus; stdcall;
function GdipSetPenStartCap(pen: GpPen; startCap: TLineCap): GpStatus; stdcall;
function GdipSetPenEndCap(pen: GpPen; endCap: TLineCap): GpStatus; stdcall;
function GdipSetPenDashCap197819(pen: GpPen; dashCap: TDashCap): GpStatus; stdcall;
function GdipGetPenStartCap(pen: GpPen; out startCap: TLineCap): GpStatus; stdcall;
function GdipGetPenEndCap(pen: GpPen; out endCap: TLineCap): GpStatus; stdcall;
function GdipGetPenDashCap197819(pen: GpPen; out dashCap: TDashCap): GpStatus; stdcall;
function GdipSetPenLineJoin(pen: GpPen; lineJoin: TLineJoin): GpStatus; stdcall;
function GdipGetPenLineJoin(pen: GpPen; var lineJoin: TLineJoin): GpStatus; stdcall;
function GdipSetPenCustomStartCap(pen: GpPen; customCap: GpCustomLineCap): GpStatus; stdcall;
function GdipGetPenCustomStartCap(pen: GpPen; out customCap: GpCustomLineCap): GpStatus; stdcall;
function GdipSetPenCustomEndCap(pen: GpPen; customCap: GpCustomLineCap): GpStatus; stdcall;
function GdipGetPenCustomEndCap(pen: GpPen; out customCap: GpCustomLineCap): GpStatus; stdcall;
function GdipSetPenMiterLimit(pen: GpPen; miterLimit: Single): GpStatus; stdcall;
function GdipGetPenMiterLimit(pen: GpPen; out miterLimit: Single): GpStatus; stdcall;
function GdipSetPenMode(pen: GpPen; penMode: TPenAlignment): GpStatus; stdcall;
function GdipGetPenMode(pen: GpPen; var penMode: TPenAlignment): GpStatus; stdcall;
function GdipSetPenTransform(pen: GpPen; matrix: GpMatrix): GpStatus; stdcall;
function GdipGetPenTransform(pen: GpPen; matrix: GpMatrix): GpStatus; stdcall;
function GdipResetPenTransform(pen: GpPen): GpStatus; stdcall;
function GdipMultiplyPenTransform(pen: GpPen; matrix: GpMatrix; order: TMatrixOrder): GpStatus; stdcall;
function GdipTranslatePenTransform(pen: GpPen; dx: Single; dy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipScalePenTransform(pen: GpPen; sx: Single; sy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipRotatePenTransform(pen: GpPen; angle: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipSetPenColor(pen: GpPen; aRGB: TArgb): GpStatus; stdcall;
function GdipGetPenColor(pen: GpPen; out aRGB: TArgb): GpStatus; stdcall;
function GdipSetPenBrushFill(pen: GpPen; brush: GpBrush): GpStatus; stdcall;
function GdipGetPenBrushFill(pen: GpPen; out brush: GpBrush): GpStatus; stdcall;
function GdipGetPenFillType(pen: GpPen; out type_: TPenType): GpStatus; stdcall;
function GdipGetPenDashStyle(pen: GpPen; out dashstyle: TDashStyle): GpStatus; stdcall;
function GdipSetPenDashStyle(pen: GpPen; dashstyle: TDashStyle): GpStatus; stdcall;
function GdipGetPenDashOffset(pen: GpPen; out offset: Single): GpStatus; stdcall;
function GdipSetPenDashOffset(pen: GpPen; offset: Single): GpStatus; stdcall;
function GdipGetPenDashCount(pen: GpPen; var count: Integer): GpStatus; stdcall;
function GdipSetPenDashArray(pen: GpPen; dash: PSingle; count: Integer): GpStatus; stdcall;
function GdipGetPenDashArray(pen: GpPen; dash: PSingle; count: Integer): GpStatus; stdcall;
function GdipGetPenCompoundCount(pen: GpPen; out count: Integer): GpStatus; stdcall;
function GdipSetPenCompoundArray(pen: GpPen; dash: PSingle; count: Integer): GpStatus; stdcall;
function GdipGetPenCompoundArray(pen: GpPen; dash: PSingle; count: Integer): GpStatus; stdcall;
function GdipCreateCustomLineCap(fillPath: GpPath; strokePath: GpPath; baseCap: TLineCap; baseInset: Single; out customCap: GpCustomLineCap): GpStatus; stdcall;
function GdipDeleteCustomLineCap(customCap: GpCustomLineCap): GpStatus; stdcall;
function GdipCloneCustomLineCap(customCap: GpCustomLineCap; out clonedCap: GpCustomLineCap): GpStatus; stdcall;
function GdipGetCustomLineCapType(customCap: GpCustomLineCap; var capType: CUSTOMLINECAPTYPE): GpStatus; stdcall;
function GdipSetCustomLineCapStrokeCaps(customCap: GpCustomLineCap; startCap: TLineCap; endCap: TLineCap): GpStatus; stdcall;
function GdipGetCustomLineCapStrokeCaps(customCap: GpCustomLineCap; var startCap: TLineCap; var endCap: TLineCap): GpStatus; stdcall;
function GdipSetCustomLineCapStrokeJoin(customCap: GpCustomLineCap; lineJoin: TLineJoin): GpStatus; stdcall;
function GdipGetCustomLineCapStrokeJoin(customCap: GpCustomLineCap; var lineJoin: TLineJoin): GpStatus; stdcall;
function GdipSetCustomLineCapBaseCap(customCap: GpCustomLineCap; baseCap: TLineCap): GpStatus; stdcall;
function GdipGetCustomLineCapBaseCap(customCap: GpCustomLineCap; var baseCap: TLineCap): GpStatus; stdcall;
function GdipSetCustomLineCapBaseInset(customCap: GpCustomLineCap; inset: Single): GpStatus; stdcall;
function GdipGetCustomLineCapBaseInset(customCap: GpCustomLineCap; var inset: Single): GpStatus; stdcall;
function GdipSetCustomLineCapWidthScale(customCap: GpCustomLineCap; widthScale: Single): GpStatus; stdcall;
function GdipGetCustomLineCapWidthScale(customCap: GpCustomLineCap; var widthScale: Single): GpStatus; stdcall;
function GdipCreateAdjustableArrowCap(height: Single; width: Single; isFilled: Bool; out cap: GpAdjustableArrowCap): GpStatus; stdcall;
function GdipSetAdjustableArrowCapHeight(cap: GpAdjustableArrowCap; height: Single): GpStatus; stdcall;
function GdipGetAdjustableArrowCapHeight(cap: GpAdjustableArrowCap; var height: Single): GpStatus; stdcall;
function GdipSetAdjustableArrowCapWidth(cap: GpAdjustableArrowCap; width: Single): GpStatus; stdcall;
function GdipGetAdjustableArrowCapWidth(cap: GpAdjustableArrowCap; var width: Single): GpStatus; stdcall;
function GdipSetAdjustableArrowCapMiddleInset(cap: GpAdjustableArrowCap; middleInset: Single): GpStatus; stdcall;
function GdipGetAdjustableArrowCapMiddleInset(cap: GpAdjustableArrowCap; var middleInset: Single): GpStatus; stdcall;
function GdipSetAdjustableArrowCapFillState(cap: GpAdjustableArrowCap; fillState: Bool): GpStatus; stdcall;
function GdipGetAdjustableArrowCapFillState(cap: GpAdjustableArrowCap; var fillState: Bool): GpStatus; stdcall;
function GdipLoadImageFromStream(stream: ISTREAM; out image: GpImage): GpStatus; stdcall;
function GdipLoadImageFromFile(filename: PWChar; out image: GpImage): GpStatus; stdcall;
function GdipLoadImageFromStreamICM(stream: ISTREAM; out image: GpImage): GpStatus; stdcall;
function GdipLoadImageFromFileICM(filename: PWChar; out image: GpImage): GpStatus; stdcall;
function GdipCloneImage(image: GpImage; out cloneImage: GpImage): GpStatus; stdcall;
function GdipDisposeImage(image: GpImage): GpStatus; stdcall;
function GdipSaveImageToFile(image: GpImage; filename: PWChar; clsidEncoder: PGUID; encoderParams: PENCODERPARAMETERS): GpStatus; stdcall;
function GdipSaveImageToStream(image: GpImage; stream: ISTREAM; clsidEncoder: PGUID; encoderParams: PENCODERPARAMETERS): GpStatus; stdcall;
function GdipSaveAdd(image: GpImage; encoderParams: PENCODERPARAMETERS): GpStatus; stdcall;
function GdipSaveAddImage(image: GpImage; newImage: GpImage; encoderParams: PENCODERPARAMETERS): GpStatus; stdcall;
function GdipGetImageGraphicsContext(image: GpImage; out graphics: GpGraphics): GpStatus; stdcall;
function GdipGetImageBounds(image: GpImage; srcRect: PRectF; var srcUnit: TUnit): GpStatus; stdcall;
function GdipGetImageDimension(image: GpImage; var width: Single; var height: Single): GpStatus; stdcall;
function GdipGetImageType(image: GpImage; var type_: IMAGETYPE): GpStatus; stdcall;
function GdipGetImageWidth(image: GpImage; var width: UInt): GpStatus; stdcall;
function GdipGetImageHeight(image: GpImage; var height: UInt): GpStatus; stdcall;
function GdipGetImageHorizontalResolution(image: GpImage; var resolution: Single): GpStatus; stdcall;
function GdipGetImageVerticalResolution(image: GpImage; var resolution: Single): GpStatus; stdcall;
function GdipGetImageFlags(image: GpImage; var flags: UInt): GpStatus; stdcall;
function GdipGetImageRawFormat(image: GpImage; format: PGUID): GpStatus; stdcall;
function GdipGetImagePixelFormat(image: GpImage; out format: TPixelFormat): GpStatus; stdcall;
function GdipGetImageThumbnail(image: GpImage; thumbWidth: UInt; thumbHeight: UInt; out thumbImage: GpImage; callback: GETTHUMBNAILIMAGEABORT; callbackData: Pointer): GpStatus; stdcall;
function GdipGetEncoderParameterListSize(image: GpImage; clsidEncoder: PGUID; out size: UInt): GpStatus; stdcall;
function GdipGetEncoderParameterList(image: GpImage; clsidEncoder: PGUID; size: UInt; buffer: PENCODERPARAMETERS): GpStatus; stdcall;
function GdipImageGetFrameDimensionsCount(image: GpImage; var count: UInt): GpStatus; stdcall;
function GdipImageGetFrameDimensionsList(image: GpImage; dimensionIDs: PGUID; count: UInt): GpStatus; stdcall;
function GdipImageGetFrameCount(image: GpImage; dimensionID: PGUID; var count: UInt): GpStatus; stdcall;
function GdipImageSelectActiveFrame(image: GpImage; dimensionID: PGUID; frameIndex: UInt): GpStatus; stdcall;
function GdipImageRotateFlip(image: GpImage; rfType: ROTATEFLIPTYPE): GpStatus; stdcall;
function GdipGetImagePalette(image: GpImage; palette: PColorPalette; size: Integer): GpStatus; stdcall;
function GdipSetImagePalette(image: GpImage; palette: PColorPalette): GpStatus; stdcall;
function GdipGetImagePaletteSize(image: GpImage; var size: Integer): GpStatus; stdcall;
function GdipGetPropertyCount(image: GpImage; var numOfProperty: UInt): GpStatus; stdcall;
function GdipGetPropertyIdList(image: GpImage; numOfProperty: UInt; list: PPROPID): GpStatus; stdcall;
function GdipGetPropertyItemSize(image: GpImage; propId: PROPID; var size: UInt): GpStatus; stdcall;
function GdipGetPropertyItem(image: GpImage; propId: PROPID; propSize: UInt; buffer: PPROPERTYITEM): GpStatus; stdcall;
function GdipGetPropertySize(image: GpImage; var totalBufferSize: UInt; var numProperties: UInt): GpStatus; stdcall;
function GdipGetAllPropertyItems(image: GpImage; totalBufferSize: UInt; numProperties: UInt; allItems: PPROPERTYITEM): GpStatus; stdcall;
function GdipRemovePropertyItem(image: GpImage; propId: PROPID): GpStatus; stdcall;
function GdipSetPropertyItem(image: GpImage; item: PPROPERTYITEM): GpStatus; stdcall;
function GdipImageForceValidation(image: GpImage): GpStatus; stdcall;
function GdipCreateBitmapFromStream(stream: ISTREAM; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromFile(filename: PWChar; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromStreamICM(stream: ISTREAM; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromFileICM(filename: PWChar; var bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromScan0(width: Integer; height: Integer; stride: Integer; format: PixelFormat; scan0: PByte; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromGraphics(width: Integer; height: Integer; target: GpGraphics; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromDirectDrawSurface(surface: IDirectDrawSurface7; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromGdiDib(gdiBitmapInfo: PBitmapInfo; gdiBitmapData: Pointer; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateBitmapFromHBITMAP(hbm: HBITMAP; hpal: HPALETTE; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateHBITMAPFromBitmap(bitmap: GpBitmap; out hbmReturn: HBITMAP; background: TArgb): GpStatus; stdcall;
function GdipCreateBitmapFromHICON(hicon: HICON; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCreateHICONFromBitmap(bitmap: GpBitmap; out hbmReturn: HICON): GpStatus; stdcall;
function GdipCreateBitmapFromResource(hInstance: HMODULE; lpBitmapName: PWChar; out bitmap: GpBitmap): GpStatus; stdcall;
function GdipCloneBitmapArea(x: Single; y: Single; width: Single; height: Single; format: PixelFormat; srcBitmap: GpBitmap; out dstBitmap: GpBitmap): GpStatus; stdcall;
function GdipCloneBitmapAreaI(x: Integer; y: Integer; width: Integer; height: Integer; format: PixelFormat; srcBitmap: GpBitmap; out dstBitmap: GpBitmap): GpStatus; stdcall;
function GdipBitmapLockBits(bitmap: GpBitmap; rect: PRectI; flags: UInt; format: PixelFormat; lockedBitmapData: PBITMAPDATA): GpStatus; stdcall;
function GdipBitmapUnlockBits(bitmap: GpBitmap; lockedBitmapData: PBITMAPDATA): GpStatus; stdcall;
function GdipBitmapGetPixel(bitmap: GpBitmap; x: Integer; y: Integer; var Color: TArgb): GpStatus; stdcall;
function GdipBitmapSetPixel(bitmap: GpBitmap; x: Integer; y: Integer; Color: TArgb): GpStatus; stdcall;
function GdipBitmapSetResolution(bitmap: GpBitmap; xdpi: Single; ydpi: Single): GpStatus; stdcall;
function GdipCreateImageAttributes(out imageattr: GpImageAttributes): GpStatus; stdcall;
function GdipCloneImageAttributes(imageattr: GpImageAttributes; out cloneImageattr: GpImageAttributes): GpStatus; stdcall;
function GdipDisposeImageAttributes(imageattr: GpImageAttributes): GpStatus; stdcall;
function GdipSetImageAttributesToIdentity(imageattr: GpImageAttributes; type_: ColorAdjustType): GpStatus; stdcall;
function GdipResetImageAttributes(imageattr: GpImageAttributes; type_: ColorAdjustType): GpStatus; stdcall;
function GdipSetImageAttributesColorMatrix(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool; ColorMatrix: PColorMatrix; grayMatrix: PColorMatrix; flags: ColorMATRIXFLAGS): GpStatus; stdcall;
function GdipSetImageAttributesThreshold(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool; threshold: Single): GpStatus; stdcall;
function GdipSetImageAttributesGamma(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool; gamma: Single): GpStatus; stdcall;
function GdipSetImageAttributesNoOp(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool): GpStatus; stdcall;
function GdipSetImageAttributesColorKeys(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool; ColorLow: TArgb; ColorHigh: TArgb): GpStatus; stdcall;
function GdipSetImageAttributesOutputChannel(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool; channelFlags: ColorCHANNELFLAGS): GpStatus; stdcall;
function GdipSetImageAttributesOutputChannelColorProfile(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool; ColorProfileFilename: PWChar): GpStatus; stdcall;
function GdipSetImageAttributesRemapTable(imageattr: GpImageAttributes; type_: ColorAdjustType; enableFlag: Bool; mapSize: UInt; map: PColorMap): GpStatus; stdcall;
function GdipSetImageAttributesWrapMode(imageAttr: GpImageAttributes; wrap: WRAPMODE; aRGB: TArgb; clamp: Bool): GpStatus; stdcall;
function GdipSetImageAttributesICMMode(imageAttr: GpImageAttributes; on_: Bool): GpStatus; stdcall;
function GdipGetImageAttributesAdjustedPalette(imageAttr: GpImageAttributes; ColorPalette: PColorPalette; ColorAdjustType: ColorAdjustType): GpStatus; stdcall;
function GdipFlush(graphics: GpGraphics; intention: TFlushIntention): GpStatus; stdcall;
function GdipCreateFromHDC(hdc: HDC; out graphics: GpGraphics): GpStatus; stdcall;
function GdipCreateFromHDC2(hdc: HDC; hDevice: THandle; out graphics: GpGraphics): GpStatus; stdcall;
function GdipCreateFromHWND(hwnd: HWND; out graphics: GpGraphics): GpStatus; stdcall;
function GdipCreateFromHWNDICM(hwnd: HWND; out graphics: GpGraphics): GpStatus; stdcall;
function GdipDeleteGraphics(graphics: GpGraphics): GpStatus; stdcall;
function GdipGetDC(graphics: GpGraphics; var hdc: HDC): GpStatus; stdcall;
function GdipReleaseDC(graphics: GpGraphics; hdc: HDC): GpStatus; stdcall;
function GdipSetCompositingMode(graphics: GpGraphics; compositingMode: COMPOSITINGMODE): GpStatus; stdcall;
function GdipGetCompositingMode(graphics: GpGraphics; var compositingMode: COMPOSITINGMODE): GpStatus; stdcall;
function GdipSetRenderingOrigin(graphics: GpGraphics; x: Integer; y: Integer): GpStatus; stdcall;
function GdipGetRenderingOrigin(graphics: GpGraphics; var x: Integer; var y: Integer): GpStatus; stdcall;
function GdipSetCompositingQuality(graphics: GpGraphics; compositingQuality: COMPOSITINGQUALITY): GpStatus; stdcall;
function GdipGetCompositingQuality(graphics: GpGraphics; var compositingQuality: COMPOSITINGQUALITY): GpStatus; stdcall;
function GdipSetSmoothingMode(graphics: GpGraphics; smoothingMode: SMOOTHINGMODE): GpStatus; stdcall;
function GdipGetSmoothingMode(graphics: GpGraphics; var smoothingMode: SMOOTHINGMODE): GpStatus; stdcall;
function GdipSetPixelOffsetMode(graphics: GpGraphics; pixelOffsetMode: PIXELOFFSETMODE): GpStatus; stdcall;
function GdipGetPixelOffsetMode(graphics: GpGraphics; var pixelOffsetMode: PIXELOFFSETMODE): GpStatus; stdcall;
function GdipSetTextRenderingHint(graphics: GpGraphics; mode: TEXTRENDERINGHINT): GpStatus; stdcall;
function GdipGetTextRenderingHint(graphics: GpGraphics; var mode: TEXTRENDERINGHINT): GpStatus; stdcall;
function GdipSetTextContrast(graphics: GpGraphics; contrast: Integer): GpStatus; stdcall;
function GdipGetTextContrast(graphics: GpGraphics; var contrast: UInt): GpStatus; stdcall;
function GdipSetInterpolationMode(graphics: GpGraphics; interpolationMode: INTERPOLATIONMODE): GpStatus; stdcall;
function GdipGetInterpolationMode(graphics: GpGraphics; var interpolationMode: INTERPOLATIONMODE): GpStatus; stdcall;
function GdipSetWorldTransform(graphics: GpGraphics; matrix: GpMatrix): GpStatus; stdcall;
function GdipResetWorldTransform(graphics: GpGraphics): GpStatus; stdcall;
function GdipMultiplyWorldTransform(graphics: GpGraphics; matrix: GpMatrix; order: TMatrixOrder): GpStatus; stdcall;
function GdipTranslateWorldTransform(graphics: GpGraphics; dx: Single; dy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipScaleWorldTransform(graphics: GpGraphics; sx: Single; sy: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipRotateWorldTransform(graphics: GpGraphics; angle: Single; order: TMatrixOrder): GpStatus; stdcall;
function GdipGetWorldTransform(graphics: GpGraphics; matrix: GpMatrix): GpStatus; stdcall;
function GdipResetPageTransform(graphics: GpGraphics): GpStatus; stdcall;
function GdipGetPageUnit(graphics: GpGraphics; var unit_: TUnit): GpStatus; stdcall;
function GdipGetPageScale(graphics: GpGraphics; var scale: Single): GpStatus; stdcall;
function GdipSetPageUnit(graphics: GpGraphics; unit_: TUnit): GpStatus; stdcall;
function GdipSetPageScale(graphics: GpGraphics; scale: Single): GpStatus; stdcall;
function GdipGetDpiX(graphics: GpGraphics; var dpi: Single): GpStatus; stdcall;
function GdipGetDpiY(graphics: GpGraphics; var dpi: Single): GpStatus; stdcall;
function GdipTransformPoints(graphics: GpGraphics; destSpace: TCoordinateSpace; srcSpace: TCoordinateSpace; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipTransformPointsI(graphics: GpGraphics; destSpace: TCoordinateSpace; srcSpace: TCoordinateSpace; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipGetNearestColor(graphics: GpGraphics; argb: PArgb): GpStatus; stdcall;
function GdipCreateHalftonePalette: HPALETTE; stdcall;
function GdipDrawLine(graphics: GpGraphics; pen: GpPen; x1: Single; y1: Single; x2: Single; y2: Single): GpStatus; stdcall;
function GdipDrawLineI(graphics: GpGraphics; pen: GpPen; x1: Integer; y1: Integer; x2: Integer; y2: Integer): GpStatus; stdcall;
function GdipDrawLines(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipDrawLinesI(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipDrawArc(graphics: GpGraphics; pen: GpPen; x: Single; y: Single; width: Single; height: Single; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipDrawArcI(graphics: GpGraphics; pen: GpPen; x: Integer; y: Integer; width: Integer; height: Integer; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipDrawBezier(graphics: GpGraphics; pen: GpPen; x1: Single; y1: Single; x2: Single; y2: Single; x3: Single; y3: Single; x4: Single; y4: Single): GpStatus; stdcall;
function GdipDrawBezierI(graphics: GpGraphics; pen: GpPen; x1: Integer; y1: Integer; x2: Integer; y2: Integer; x3: Integer; y3: Integer; x4: Integer; y4: Integer): GpStatus; stdcall;
function GdipDrawBeziers(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipDrawBeziersI(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipDrawRectangle(graphics: GpGraphics; pen: GpPen; x: Single; y: Single; width: Single; height: Single): GpStatus; stdcall;
function GdipDrawRectangleI(graphics: GpGraphics; pen: GpPen; x: Integer; y: Integer; width: Integer; height: Integer): GpStatus; stdcall;
function GdipDrawRectangles(graphics: GpGraphics; pen: GpPen; rects: PRectF; count: Integer): GpStatus; stdcall;
function GdipDrawRectanglesI(graphics: GpGraphics; pen: GpPen; rects: PRectI; count: Integer): GpStatus; stdcall;
function GdipDrawEllipse(graphics: GpGraphics; pen: GpPen; x: Single; y: Single; width: Single; height: Single): GpStatus; stdcall;
function GdipDrawEllipseI(graphics: GpGraphics; pen: GpPen; x: Integer; y: Integer; width: Integer; height: Integer): GpStatus; stdcall;
function GdipDrawPie(graphics: GpGraphics; pen: GpPen; x: Single; y: Single; width: Single; height: Single; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipDrawPieI(graphics: GpGraphics; pen: GpPen; x: Integer; y: Integer; width: Integer; height: Integer; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipDrawPolygon(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipDrawPolygonI(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipDrawPath(graphics: GpGraphics; pen: GpPen; path: GpPath): GpStatus; stdcall;
function GdipDrawCurve(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipDrawCurveI(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipDrawCurve2(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer; tension: Single): GpStatus; stdcall;
function GdipDrawCurve2I(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer; tension: Single): GpStatus; stdcall;
function GdipDrawCurve3(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer; offset: Integer; numberOfSegments: Integer; tension: Single): GpStatus; stdcall;
function GdipDrawCurve3I(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer; offset: Integer; numberOfSegments: Integer; tension: Single): GpStatus; stdcall;
function GdipDrawClosedCurve(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipDrawClosedCurveI(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipDrawClosedCurve2(graphics: GpGraphics; pen: GpPen; points: PPointF; count: Integer; tension: Single): GpStatus; stdcall;
function GdipDrawClosedCurve2I(graphics: GpGraphics; pen: GpPen; points: PPointI; count: Integer; tension: Single): GpStatus; stdcall;
function GdipGraphicsClear(graphics: GpGraphics; Color: TArgb): GpStatus; stdcall;
function GdipFillRectangle(graphics: GpGraphics; brush: GpBrush; x: Single; y: Single; width: Single; height: Single): GpStatus; stdcall;
function GdipFillRectangleI(graphics: GpGraphics; brush: GpBrush; x: Integer; y: Integer; width: Integer; height: Integer): GpStatus; stdcall;
function GdipFillRectangles(graphics: GpGraphics; brush: GpBrush; rects: PRectF; count: Integer): GpStatus; stdcall;
function GdipFillRectanglesI(graphics: GpGraphics; brush: GpBrush; rects: PRectI; count: Integer): GpStatus; stdcall;
function GdipFillPolygon(graphics: GpGraphics; brush: GpBrush; points: PPointF; count: Integer; fillMode: TFillMode): GpStatus; stdcall;
function GdipFillPolygonI(graphics: GpGraphics; brush: GpBrush; points: PPointI; count: Integer; fillMode: TFillMode): GpStatus; stdcall;
function GdipFillPolygon2(graphics: GpGraphics; brush: GpBrush; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipFillPolygon2I(graphics: GpGraphics; brush: GpBrush; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipFillEllipse(graphics: GpGraphics; brush: GpBrush; x: Single; y: Single; width: Single; height: Single): GpStatus; stdcall;
function GdipFillEllipseI(graphics: GpGraphics; brush: GpBrush; x: Integer; y: Integer; width: Integer; height: Integer): GpStatus; stdcall;
function GdipFillPie(graphics: GpGraphics; brush: GpBrush; x: Single; y: Single; width: Single; height: Single; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipFillPieI(graphics: GpGraphics; brush: GpBrush; x: Integer; y: Integer; width: Integer; height: Integer; startAngle: Single; sweepAngle: Single): GpStatus; stdcall;
function GdipFillPath(graphics: GpGraphics; brush: GpBrush; path: GpPath): GpStatus; stdcall;
function GdipFillClosedCurve(graphics: GpGraphics; brush: GpBrush; points: PPointF; count: Integer): GpStatus; stdcall;
function GdipFillClosedCurveI(graphics: GpGraphics; brush: GpBrush; points: PPointI; count: Integer): GpStatus; stdcall;
function GdipFillClosedCurve2(graphics: GpGraphics; brush: GpBrush; points: PPointF; count: Integer; tension: Single; fillMode: TFillMode): GpStatus; stdcall;
function GdipFillClosedCurve2I(graphics: GpGraphics; brush: GpBrush; points: PPointI; count: Integer; tension: Single; fillMode: TFillMode): GpStatus; stdcall;
function GdipFillRegion(graphics: GpGraphics; brush: GpBrush; region: GpRegion): GpStatus; stdcall;
function GdipDrawImage(graphics: GpGraphics; image: GpImage; x: Single; y: Single): GpStatus; stdcall;
function GdipDrawImageI(graphics: GpGraphics; image: GpImage; x: Integer; y: Integer): GpStatus; stdcall;
function GdipDrawImageRect(graphics: GpGraphics; image: GpImage; x: Single; y: Single; width: Single; height: Single): GpStatus; stdcall;
function GdipDrawImageRectI(graphics: GpGraphics; image: GpImage; x: Integer; y: Integer; width: Integer; height: Integer): GpStatus; stdcall;
function GdipDrawImagePoints(graphics: GpGraphics; image: GpImage; dstpoints: PPointF; count: Integer): GpStatus; stdcall;
function GdipDrawImagePointsI(graphics: GpGraphics; image: GpImage; dstpoints: PPointI; count: Integer): GpStatus; stdcall;
function GdipDrawImagePointRect(graphics: GpGraphics; image: GpImage; x: Single; y: Single; srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single; srcUnit: TUnit): GpStatus; stdcall;
function GdipDrawImagePointRectI(graphics: GpGraphics; image: GpImage; x: Integer; y: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer; srcUnit: TUnit): GpStatus; stdcall;
function GdipDrawImageRectRect(graphics: GpGraphics; image: GpImage; dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single; srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single; srcUnit: TUnit; imageAttributes: GpImageAttributes; callback: DRAWIMAGEABORT; callbackData: Pointer): GpStatus; stdcall;
function GdipDrawImageRectRectI(graphics: GpGraphics; image: GpImage; dstx: Integer; dsty: Integer; dstwidth: Integer; dstheight: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer; srcUnit: TUnit; imageAttributes: GpImageAttributes; callback: DRAWIMAGEABORT; callbackData: Pointer): GpStatus; stdcall;
function GdipDrawImagePointsRect(graphics: GpGraphics; image: GpImage; points: PPointF; count: Integer; srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single; srcUnit: TUnit; imageAttributes: GpImageAttributes; callback: DRAWIMAGEABORT; callbackData: Pointer): GpStatus; stdcall;
function GdipDrawImagePointsRectI(graphics: GpGraphics; image: GpImage; points: PPointI; count: Integer; srcx: Integer; srcy: Integer; srcwidth: Integer; srcheight: Integer; srcUnit: TUnit; imageAttributes: GpImageAttributes; callback: DRAWIMAGEABORT; callbackData: Pointer): GpStatus; stdcall;
function GdipEnumerateMetafileDestPoint(graphics: GpGraphics; metafile: GpMetafile; destPoint: PRectF; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileDestPointI(graphics: GpGraphics; metafile: GpMetafile; destPoint: PPointI; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileDestRect(graphics: GpGraphics; metafile: GpMetafile; destRect: PRectF; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileDestRectI(graphics: GpGraphics; metafile: GpMetafile; destRect: PRectI; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileDestPoints(graphics: GpGraphics; metafile: GpMetafile; destpoints: PPointF; count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileDestPointsI(graphics: GpGraphics; metafile: GpMetafile; destPoints: PPointI; count: Integer; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileSrcRectDestPoint(graphics: GpGraphics; metafile: GpMetafile; destPoint: PRectF; srcRect: PRectF; srcUnit: TUNIT; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileSrcRectDestPointI(graphics: GpGraphics; metafile: GpMetafile; destPoint: PPointI; srcRect: PRectI; srcUnit: TUNIT; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileSrcRectDestRect(graphics: GpGraphics; metafile: GpMetafile; destRect: PRectF; srcRect: PRectF; srcUnit: TUNIT; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileSrcRectDestRectI(graphics: GpGraphics; metafile: GpMetafile; destRect: PRectI; srcRect: PRectI; srcUnit: TUNIT; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileSrcRectDestPoints(graphics: GpGraphics; metafile: GpMetafile; destpoints: PPointF; count: Integer; srcRect: PRectF; srcUnit: TUNIT; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipEnumerateMetafileSrcRectDestPointsI(graphics: GpGraphics; metafile: GpMetafile; destPoints: PPointI; count: Integer; srcRect: PRectI; srcUnit: TUNIT; callback: EnumerateMetafileProc; callbackData: Pointer; imageAttributes: GpImageAttributes): GpStatus; stdcall;
function GdipPlayMetafileRecord(metafile: GpMetafile; recordType: EmfPlusRecordType; flags: UInt; dataSize: UInt; data: PByte): GpStatus; stdcall;
function GdipSetClipGraphics(graphics: GpGraphics; srcgraphics: GpGraphics; CombineMode: CombineMode): GpStatus; stdcall;
function GdipSetClipRect(graphics: GpGraphics; x: Single; y: Single; width: Single; height: Single; CombineMode: CombineMode): GpStatus; stdcall;
function GdipSetClipRectI(graphics: GpGraphics; x: Integer; y: Integer; width: Integer; height: Integer; CombineMode: CombineMode): GpStatus; stdcall;
function GdipSetClipPath(graphics: GpGraphics; path: GpPath; CombineMode: CombineMode): GpStatus; stdcall;
function GdipSetClipRegion(graphics: GpGraphics; region: GpRegion; CombineMode: CombineMode): GpStatus; stdcall;
function GdipSetClipHrgn(graphics: GpGraphics; hRgn: HRGN; CombineMode: CombineMode): GpStatus; stdcall;
function GdipResetClip(graphics: GpGraphics): GpStatus; stdcall;
function GdipTranslateClip(graphics: GpGraphics; dx: Single; dy: Single): GpStatus; stdcall;
function GdipTranslateClipI(graphics: GpGraphics; dx: Integer; dy: Integer): GpStatus; stdcall;
function GdipGetClip(graphics: GpGraphics; region: GpRegion): GpStatus; stdcall;
function GdipGetClipBounds(graphics: GpGraphics; rect: PRectF): GpStatus; stdcall;
function GdipGetClipBoundsI(graphics: GpGraphics; rect: PRectI): GpStatus; stdcall;
function GdipIsClipEmpty(graphics: GpGraphics; Result: PBool): GpStatus; stdcall;
function GdipGetVisibleClipBounds(graphics: GpGraphics; rect: PRectF): GpStatus; stdcall;
function GdipGetVisibleClipBoundsI(graphics: GpGraphics; rect: PRectI): GpStatus; stdcall;
function GdipIsVisibleClipEmpty(graphics: GpGraphics; var Result: Bool): GpStatus; stdcall;
function GdipIsVisiblePoint(graphics: GpGraphics; x: Single; y: Single; var Result: Bool): GpStatus; stdcall;
function GdipIsVisiblePointI(graphics: GpGraphics; x: Integer; y: Integer; var Result: Bool): GpStatus; stdcall;
function GdipIsVisibleRect(graphics: GpGraphics; x: Single; y: Single; width: Single; height: Single; var Result: Bool): GpStatus; stdcall;
function GdipIsVisibleRectI(graphics: GpGraphics; x: Integer; y: Integer; width: Integer; height: Integer; var Result: Bool): GpStatus; stdcall;
function GdipSaveGraphics(graphics: GpGraphics; var state: GraphicsState): GpStatus; stdcall;
function GdipRestoreGraphics(graphics: GpGraphics; state: GraphicsState): GpStatus; stdcall;
function GdipBeginContainer(graphics: GpGraphics; dstrect: PRectF; srcrect: PRectF; unit_: TUnit; var state: GraphicsContainer): GpStatus; stdcall;
function GdipBeginContainerI(graphics: GpGraphics; dstrect: PRectI; srcrect: PRectI; unit_: TUnit; var state: GraphicsContainer): GpStatus; stdcall;
function GdipBeginContainer2(graphics: GpGraphics; var state: GraphicsContainer): GpStatus; stdcall;
function GdipEndContainer(graphics: GpGraphics; state: GraphicsContainer): GpStatus; stdcall;
function GdipGetMetafileHeaderFromWmf(hWmf: HMETAFILE; wmfPlaceableFileHeader: PWmfPlaceableFileHeader; header: Pointer): GpStatus; stdcall;
function GdipGetMetafileHeaderFromEmf(hEmf: HENHMETAFILE; header: Pointer): GpStatus; stdcall;
function GdipGetMetafileHeaderFromFile(filename: PWChar; header: Pointer): GpStatus; stdcall;
function GdipGetMetafileHeaderFromStream(stream: ISTREAM; header: Pointer): GpStatus; stdcall;
function GdipGetMetafileHeaderFromMetafile(metafile: GpMetafile; header: Pointer): GpStatus; stdcall;
function GdipGetHemfFromMetafile(metafile: GpMetafile; var hEmf: HENHMETAFILE): GpStatus; stdcall;
function GdipCreateStreamOnFile(filename: PWChar; access: UInt; out stream: ISTREAM): GpStatus; stdcall;
function GdipCreateMetafileFromWmf(hWmf: HMETAFILE; deleteWmf: Bool; wmfPlaceableFileHeader: PWmfPlaceableFileHeader; out metafile: GpMetafile): GpStatus; stdcall;
function GdipCreateMetafileFromEmf(hEmf: HENHMETAFILE; deleteEmf: Bool; out metafile: GpMetafile): GpStatus; stdcall;
function GdipCreateMetafileFromFile(file_: PWChar; out metafile: GpMetafile): GpStatus; stdcall;
function GdipCreateMetafileFromWmfFile(file_: PWChar; wmfPlaceableFileHeader: PWmfPlaceableFileHeader; out metafile: GpMetafile): GpStatus; stdcall;
function GdipCreateMetafileFromStream(stream: ISTREAM; out metafile: GpMetafile): GpStatus; stdcall;
function GdipRecordMetafile(referenceHdc: HDC; type_: EMFTYPE; frameRect: PRectF; frameUnit: MetafileFrameUnit; description: PWChar; out metafile: GpMetafile): GpStatus; stdcall;
function GdipRecordMetafileI(referenceHdc: HDC; type_: EMFTYPE; frameRect: PRectI; frameUnit: MetafileFrameUnit; description: PWChar; out metafile: GpMetafile): GpStatus; stdcall;
function GdipRecordMetafileFileName(fileName: PWChar; referenceHdc: HDC; type_: EMFTYPE; frameRect: PRectF; frameUnit: MetafileFrameUnit; description: PWChar; out metafile: GpMetafile): GpStatus; stdcall;
function GdipRecordMetafileFileNameI(fileName: PWChar; referenceHdc: HDC; type_: EMFTYPE; frameRect: PRectI; frameUnit: MetafileFrameUnit; description: PWChar; out metafile: GpMetafile): GpStatus; stdcall;
function GdipRecordMetafileStream(stream: ISTREAM; referenceHdc: HDC; type_: EMFTYPE; frameRect: PRectF; frameUnit: MetafileFrameUnit; description: PWChar; out metafile: GpMetafile): GpStatus; stdcall;
function GdipRecordMetafileStreamI(stream: ISTREAM; referenceHdc: HDC; type_: EMFTYPE; frameRect: PRectI; frameUnit: MetafileFrameUnit; description: PWChar; out metafile: GpMetafile): GpStatus; stdcall;
function GdipSetMetafileDownLevelRasterizationLimit(metafile: GpMetafile; metafileRasterizationLimitDpi: UInt): GpStatus; stdcall;
function GdipGetMetafileDownLevelRasterizationLimit(metafile: GpMetafile; var metafileRasterizationLimitDpi: UInt): GpStatus; stdcall;
function GdipGetImageDecodersSize(out numDecoders: UInt; out size: UInt): GpStatus; stdcall;
function GdipGetImageDecoders(numDecoders: UInt; size: UInt; decoders: PIMAGECODECINFO): GpStatus; stdcall;
function GdipGetImageEncodersSize(out numEncoders: UInt; out size: UInt): GpStatus; stdcall;
function GdipGetImageEncoders(numEncoders: UInt; size: UInt; encoders: PIMAGECODECINFO): GpStatus; stdcall;
function GdipComment(graphics: GpGraphics; sizeData: UInt; data: PByte): GpStatus; stdcall;
function GdipCreateFontFamilyFromName(name: PWChar; fontCollection: GpFontCollection; out FontFamily: GpFontFamily): GpStatus; stdcall;
function GdipDeleteFontFamily(FontFamily: GpFontFamily): GpStatus; stdcall;
function GdipCloneFontFamily(FontFamily: GpFontFamily; out clonedFontFamily: GpFontFamily): GpStatus; stdcall;
function GdipGetGenericFontFamilySansSerif(out nativeFamily: GpFontFamily): GpStatus; stdcall;
function GdipGetGenericFontFamilySerif(out nativeFamily: GpFontFamily): GpStatus; stdcall;
function GdipGetGenericFontFamilyMonospace(out nativeFamily: GpFontFamily): GpStatus; stdcall;
function GdipGetFamilyName(family: GpFontFamily; name: PWideChar; language: LangId): GpStatus; stdcall;
function GdipIsStyleAvailable(family: GpFontFamily; style: Integer; var IsStyleAvailable: Bool): GpStatus; stdcall;
function GdipFontCollectionEnumerable(fontCollection: GpFontCollection; graphics: GpGraphics; var numFound: Integer): GpStatus; stdcall;
function GdipFontCollectionEnumerate(fontCollection: GpFontCollection; numSought: Integer; gpfamilies: array of GpFontFamily; var numFound: Integer; graphics: GpGraphics): GpStatus; stdcall;
function GdipGetEmHeight(family: GpFontFamily; style: Integer; out EmHeight: UInt16): GpStatus; stdcall;
function GdipGetCellAscent(family: GpFontFamily; style: Integer; var CellAscent: UInt16): GpStatus; stdcall;
function GdipGetCellDescent(family: GpFontFamily; style: Integer; var CellDescent: UInt16): GpStatus; stdcall;
function GdipGetLineSpacing(family: GpFontFamily; style: Integer; var LineSpacing: UInt16): GpStatus; stdcall;
function GdipCreateFontFromDC(hdc: HDC; out font: GpFont): GpStatus; stdcall;
function GdipCreateFontFromLogfontA(hdc: HDC; logfont: PLOGFONTA; out font: GpFont): GpStatus; stdcall;
function GdipCreateFontFromLogfontW(hdc: HDC; logfont: PLOGFONTW; out font: GpFont): GpStatus; stdcall;
function GdipCreateFont(fontFamily: GpFontFamily; emSize: Single; style: Integer; unit_: Integer; out font: GpFont): GpStatus; stdcall;
function GdipCloneFont(font: GpFont; out cloneFont: GpFont): GpStatus; stdcall;
function GdipDeleteFont(font: GpFont): GpStatus; stdcall;
function GdipGetFamily(font: GpFont; out family: GpFontFamily): GpStatus; stdcall;
function GdipGetFontStyle(font: GpFont; var style: Integer): GpStatus; stdcall;
function GdipGetFontSize(font: GpFont; var size: Single): GpStatus; stdcall;
function GdipGetFontUnit(font: GpFont; var unit_: TUNIT): GpStatus; stdcall;
function GdipGetFontHeight(font: GpFont; graphics: GpGraphics; var height: Single): GpStatus; stdcall;
function GdipGetFontHeightGivenDPI(font: GpFont; dpi: Single; var height: Single): GpStatus; stdcall;
function GdipGetLogFontA(font: GpFont; graphics: GpGraphics; var logfontA: LOGFONTA): GpStatus; stdcall;
function GdipGetLogFontW(font: GpFont; graphics: GpGraphics; var logfontW: LOGFONTW): GpStatus; stdcall;
function GdipNewInstalledFontCollection(out fontCollection: GpFontCollection): GpStatus; stdcall;
function GdipNewPrivateFontCollection(out fontCollection: GpFontCollection): GpStatus; stdcall;
function GdipDeletePrivateFontCollection(out fontCollection: GpFontCollection): GpStatus; stdcall;
function GdipGetFontCollectionFamilyCount(fontCollection: GpFontCollection; var numFound: Integer): GpStatus; stdcall;
function GdipGetFontCollectionFamilyList(fontCollection: GpFontCollection; numSought: Integer; gpfamilies: GpFontFamily; var numFound: Integer): GpStatus; stdcall;
function GdipPrivateAddFontFile(fontCollection: GpFontCollection; filename: PWChar): GpStatus; stdcall;
function GdipPrivateAddMemoryFont(fontCollection: GpFontCollection; memory: Pointer; length: Integer): GpStatus; stdcall;
function GdipDrawString(graphics: GpGraphics; string_: PWChar; length: Integer; font: GpFont; layoutRect: PRectF; stringFormat: GpStringFormat; brush: GpBrush): GpStatus; stdcall;
function GdipMeasureString(graphics: GpGraphics; string_: PWChar; length: Integer; font: GpFont; layoutRect: PRectF; stringFormat: GpStringFormat; boundingBox: PRectF; codepointsFitted: PInteger; linesFilled: PInteger): GpStatus; stdcall;
function GdipMeasureCharacterRanges(graphics: GpGraphics; string_: PWChar; length: Integer; font: GpFont; layoutRect: PRectF; stringFormat: GpStringFormat; regionCount: Integer; const regions: GpRegion): GpStatus; stdcall;
function GdipDrawDriverString(graphics: GpGraphics; const text: PUInt16; length: Integer; const font: GpFont; const brush: GpBrush; const positions: PPointF; flags: Integer; const matrix: GpMatrix): GpStatus; stdcall;
function GdipMeasureDriverString(graphics: GpGraphics; text: PUInt16; length: Integer; font: GpFont; positions: PPointF; flags: Integer; matrix: GpMatrix; boundingBox: PRectF): GpStatus; stdcall;
function GdipCreateStringFormat(formatAttributes: Integer; language: LangId; out format: GpStringFormat): GpStatus; stdcall;
function GdipStringFormatGetGenericDefault(out format: GpStringFormat): GpStatus; stdcall;
function GdipStringFormatGetGenericTypographic(out format: GpStringFormat): GpStatus; stdcall;
function GdipDeleteStringFormat(format: GpStringFormat): GpStatus; stdcall;
function GdipCloneStringFormat(format: GpStringFormat; out newFormat: GpStringFormat): GpStatus; stdcall;
function GdipSetStringFormatFlags(format: GpStringFormat; flags: Integer): GpStatus; stdcall;
function GdipGetStringFormatFlags(format: GpStringFormat; out flags: Integer): GpStatus; stdcall;
function GdipSetStringFormatAlign(format: GpStringFormat; align: StringAlignment): GpStatus; stdcall;
function GdipGetStringFormatAlign(format: GpStringFormat; out align: StringAlignment): GpStatus; stdcall;
function GdipSetStringFormatLineAlign(format: GpStringFormat; align: StringAlignment): GpStatus; stdcall;
function GdipGetStringFormatLineAlign(format: GpStringFormat; out align: StringAlignment): GpStatus; stdcall;
function GdipSetStringFormatTrimming(format: GpStringFormat; trimming: StringTrimming): GpStatus; stdcall;
function GdipGetStringFormatTrimming(format: GpStringFormat; out trimming: StringTrimming): GpStatus; stdcall;
function GdipSetStringFormatHotkeyPrefix(format: GpStringFormat; hotkeyPrefix: Integer): GpStatus; stdcall;
function GdipGetStringFormatHotkeyPrefix(format: GpStringFormat; out hotkeyPrefix: Integer): GpStatus; stdcall;
function GdipSetStringFormatTabStops(format: GpStringFormat; firstTabOffset: Single; count: Integer; tabStops: PSingle): GpStatus; stdcall;
function GdipGetStringFormatTabStops(format: GpStringFormat; count: Integer; firstTabOffset: PSingle; tabStops: PSingle): GpStatus; stdcall;
function GdipGetStringFormatTabStopCount(format: GpStringFormat; out count: Integer): GpStatus; stdcall;
function GdipSetStringFormatDigitSubstitution(format: GpStringFormat; language: LangId; substitute: StringDigitSubstitute): GpStatus; stdcall;
function GdipGetStringFormatDigitSubstitution(format: GpStringFormat; language: PUINT; substitute: PStringDigitSubstitute): GpStatus; stdcall;
function GdipGetStringFormatMeasurableCharacterRangeCount(format: GpStringFormat; out count: Integer): GpStatus; stdcall;
function GdipSetStringFormatMeasurableCharacterRanges(format: GpStringFormat; rangeCount: Integer; ranges: PCharacterRange): GpStatus; stdcall;
function GdipCreateCachedBitmap(bitmap: GpBitmap; graphics: GpGraphics; out cachedBitmap: GpCachedBitmap): GpStatus; stdcall;
function GdipDeleteCachedBitmap(cachedBitmap: GpCachedBitmap): GpStatus; stdcall;
function GdipDrawCachedBitmap(graphics: GpGraphics; cachedBitmap: GpCachedBitmap; x: Integer; y: Integer): GpStatus; stdcall;
function GdipEmfToWmfBits(hemf: HENHMETAFILE; cbData16: UInt; pData16: PByte; iMapMode: Integer; eFlags: Integer): UInt; stdcall;

implementation

function GetEncoderClsid(const Format: WideString; out Clsid: TGUID): Boolean;
var
  Num, Size: UInt;
  Codecs, Item: PImageCodecInfo;
  I: Integer;
begin
  Result := False;
  GdipGetImageEncodersSize(Num, Size);
  GetMem(Codecs, Size);
  GdipGetImageEncoders(Num, Size, Codecs);
  Item := Codecs;
  for I := 0 to Num - 1 do
  begin
    if Format = Item.MimeType then
    begin
      Clsid := Item.Clsid;
      Result := True;
      Break;
    end;
    Inc(Item);
  end;
  FreeMem(Codecs);
end;

function NewArgb(A, R, G, B: Byte): TArgb; overload;
begin
  Result := ((DWORD(B) shl BlueShift) or (DWORD(G) shl GreenShift) or
    (DWORD(R) shl RedShift) or (DWORD(A) shl AlphaShift));
end;

function NewArgb(Argb: TArgb; Alpha: Byte): TArgb;
begin
  Result := (Argb and $FFFFFF) or (DWORD(Alpha) shl AlphaShift);
end;

function GetAlpha(Color: TArgb): Byte;
begin
  Result := Byte(Color shr AlphaShift);
end;

function GetRed(Color: TArgb): Byte;
begin
  Result := Byte(Color shr RedShift);
end;

function GetGreen(Color: TArgb): Byte;
begin
  Result := Byte(Color shr GreenShift);
end;

function GetBlue(Color: TArgb): Byte;
begin
  Result := Byte(Color shr BlueShift);
end;

function ColorRefToArgb(Color: TColorRef): TArgb;
begin
  Result := NewArgb(255, GetRValue(Color), GetGValue(Color), GetBValue(Color));
end;

function ArgbToColorRef(Color: TArgb): TColorRef;
begin
  Result := RGB(GetRed(Color), GetGreen(Color), GetBlue(Color));
end;

function NewColorMatrix: TColorMatrix;
begin
  FillChar(Result, SizeOf(Result), #0);
  Result[0, 0] := 1;
  Result[1, 1] := 1;
  Result[2, 2] := 1;
  Result[3, 3] := 1;
  Result[4, 4] := 1;
end;

function NewOpacityMatrix(Opacity: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[3, 3] := Opacity;
end;

function NewColorMatrix(R, G, B, A: Byte): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := R / $FF;
  Result[1, 1] := G / $FF;
  Result[2, 2] := B / $FF;
  Result[3, 3] := A / $FF;
end;

function NewColorMatrix(R, G, B, A: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := R;
  Result[1, 1] := G;
  Result[2, 2] := B;
  Result[3, 3] := A;
end;

type
	TColorBytes = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Alpha: Byte;
  end;
	PColorBytes = ^TColorBytes;

function ColorBytes(R, G, B, A: Byte): TColorBytes;
begin
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;
  Result.Alpha := A;
end;

function ColorToBytes(Color: TColorRef): TColorBytes;
begin
  Result := TColorBytes(Color shl $8 and TColorBytes(Color).Blue);
end;

function NewColorMatrix(ColorRef: TColorRef): TColorMatrix;
begin
  with ColorToBytes(ColorRef) do
    Result := NewColorMatrix(Red, Green, Blue, Alpha);
end;

function NewColorMatrix(const Transform: TColorTransform): TColorMatrix;
begin
  Result := NewColorMatrix;
  if Transform.Brightness <> 0 then
    Result := ColorBrightness(Result, Transform.Brightness);
  if Transform.Contrast <> 1 then
    Result := ColorContrast(Result, Transform.Contrast);
  if Transform.Opacity <> 1 then
    Result := ColorOpacity(Result, Transform.Opacity);
  if Transform.Saturation <> 1 then
    Result := ColorSaturate(Result, Transform.Saturation);
  if Transform.Greyscale then
    Result := ColorGreyscale(Result);
  if Transform.Negative then
    Result := ColorNegative(Result);
end;

procedure ColorFill(C: PSingle; Data: array of Single);
var
  I: Integer;
begin
  for I := Low(Data) to High(Data) do
  begin
    C^ := Data[I];
    Inc(C);
  end;
end;

function ColorMultiply(const A, B: TColorMatrix): TColorMatrix;
var
  Col, Row: TColorRow;
  X, Y, Z: Integer;
  S: single;
begin
  for X := Low(Result[0]) to High(Result[0]) do
  begin
    for Y := Low(Col) to High(Col) do
      Col[Y] := A[Y, X];
    for Z := Low(Col) to High(Col) do
    begin
      Row := TColorRow(B[Z]);
      S := 0;
      for Y := Low(Row) to High(Row) do
        S := S + Row[Y] * Col[Y];
      Result[Z, X] := S;
    end;
  end;
end;

function ColorBrightness(const M: TColorMatrix; B: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  ColorFill(@Result[4, 0], [B, B, B, 0, 1]);
  Result := ColorMultiply(Result, M);
end;

function ColorContrast(const M: TColorMatrix; C: Single): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := C;
  Result[1, 1] := C;
  Result[2, 2] := C;
  ColorFill(@Result[4, 0], [0.001, 0.001, 0.001, 0, 0]);
  Result := ColorMultiply(Result, M);
end;

function ColorSaturate(const M: TColorMatrix; S: Single): TColorMatrix;
var
  C, R, G, B: Single;
begin
  C := 1 - S;
  R := 0.3086 * C;
  G := 0.6094 * C;
  B := 0.0820 * C;
  Result := NewColorMatrix;
  ColorFill(@Result[0, 0], [R + S, R, R]);
  ColorFill(@Result[1, 0], [G, G + S, G]);
  ColorFill(@Result[2, 0], [B, B, B + S]);
  Result := ColorMultiply(Result, M);
end;

function ColorOpacity(const M: TColorMatrix; O: Single): TColorMatrix;
begin
  Result := ColorMultiply(NewOpacityMatrix(O), M);
end;

function ColorGreyscale(const M: TColorMatrix): TColorMatrix;
begin
  Result := NewColorMatrix;
  ColorFill(@Result[0, 0], [0.30, 0.30, 0.30]);
  ColorFill(@Result[1, 0], [0.59, 0.59, 0.59]);
  ColorFill(@Result[2, 0], [0.11, 0.11, 0.11]);
  Result := ColorMultiply(Result, M);
end;

function ColorNegative(const M: TColorMatrix): TColorMatrix;
begin
  Result := NewColorMatrix;
  Result[0, 0] := -1;
  Result[1, 1] := -1;
  Result[2, 2] := -1;
  Result := ColorMultiply(Result, M);
end;

function NewColorTransform: TColorTransform;
begin
  with Result do
  begin
    Gamma := 1;
    Brightness := 0;
    Contrast := 1;
    Saturation := 1;
    Opacity := 1;
    Greyscale := False;
    Negative := False;
  end;
end;

{ Color routines }

function Convert(Color: Longint): TArgb;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF)
  else
    Result := Color;
end;

function ConvertBytes(Color: Longint): TColorBytes;
begin
  if Color < 0 then
    Result := TColorBytes(GetSysColor(Color and $000000FF))
  else
    Result := TColorBytes(Color);
end;

function NewColor(R, G, B: Byte; A: Byte = $FF): TArgb;
begin
  Result := (ARGB(A) shl ALPHA_SHIFT) or (ARGB(R) shl RED_SHIFT) or
    (ARGB(G) shl GREEN_SHIFT) or ARGB(B)
end;

function NewColor(Color: Longint; A: Byte = $FF): TArgb;
var
  C: TColorBytes absolute Result;
begin
  C := TColorBytes(Convert(Color));
	C.Alpha := C.Blue;
  C.Blue := C.Red;
  C.Red := C.Alpha;
  C.Alpha := A;
end;

function NewBlend(Color1, Color2: Longint; Percent: Byte = 50; A: Byte = $FF): TArgb;
var
	R, I: Single;
  C1, C2: TColorBytes;
  C: TColorBytes absolute Result;
begin
	if Percent > 99 then
  	Result := NewColor(Color1, A)
	else if Percent < 1 then
  	Result := NewColor(Color2, A)
	else
  begin
  	R := Percent / 100;
    I := 1 - R;
	  C1 := TColorBytes(Convert(Color1));
		C1.Alpha := C1.Blue;
	  C1.Blue := C1.Red;
	  C1.Red := C1.Alpha;
	  C2 := TColorBytes(Convert(Color2));
		C2.Alpha := C2.Blue;
	  C2.Blue := C2.Red;
	  C2.Red := C2.Alpha;
		C.Red := Round(C1.Red * R + C2.Red * I);
	  C.Green := Round(C1.Green * R + C2.Green * I);
	  C.Blue := Round(C1.Blue * R + C2.Blue * I);
	  C.Alpha := A;
  end;
end;

function ColorIntensity(Color: Longint; Intensity: Single; A: Byte = $FF): TArgb; overload;
var
  C: TColorBytes absolute Result;
begin
  C := TColorBytes(Convert(Color));
	C.Alpha := C.Blue;
  C.Blue := C.Red;
  C.Red := C.Alpha;
  C.Alpha := A;
  C.Red := Round(C.Red * Intensity);
  C.Green := Round(C.Green * Intensity);
  C.Blue := Round(C.Blue * Intensity);
end;

function ColorV(Color: Longint; V: Single; A: Byte = $FF): TArgb; overload;
var
  B: Byte;
  C: TColorBytes absolute Result;

  function Clamp(A: Byte): Byte;
  var
    T: Single;
  begin
    T := (A + B) * V;
    if T > High(Byte) then
      Result := High(Byte)
    else if T < 0 then
      Result := 0
    else
      Result := Round(T);
  end;

begin
  if V < -2 then V := -2 else if V > 2 then V := 2;
  if Abs(V) > 1 then B := High(Byte) else B := 0;
  C := TColorBytes(Convert(Color));
	C.Alpha := C.Blue;
  C.Blue := C.Red;
  C.Red := C.Alpha;
  C.Alpha := A;
  C.Red := Clamp(C.Red);
  C.Green := Clamp(C.Green);
  C.Blue := Clamp(C.Blue);
end;

procedure SetOpacity(var C: TArgb; Opacity: Byte);
begin
  C := (C and $FFFFFF) or Opacity shl 24;
end;

function GetOpacity(C: TArgb): Byte;
begin
  Result := C shr 24;
end;

function NewOpacity(C: TArgb; Opacity: Byte): TArgb;
begin
  SetOpacity(C, Opacity);
  Result := C;
end;

{ Point and rect routines }

function PointI(Point: TPoint): TPointF;
begin
  Result.X := Point.X;
  Result.Y := Point.Y;
end;

function PointI(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function RectF(const Rect: TRect): TRectF;
begin
  Result.X := Rect.Left;
  Result.Y := Rect.Top;
  Result.Width := Rect.Right - Rect.Left;
  Result.Height := Rect.Bottom - Rect.Top;
end;

function RectF(X, Y, W, H: Single): TRectF;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := W;
  Result.Height := H;
end;

procedure InflateRectF(var Rect: TRectF; X, Y: Single);
begin
  Rect.X := Rect.X - X;
  Rect.Y := Rect.Y - Y;
  Rect.Width := Rect.Width + X * 2;
  Rect.Height := Rect.Height + Y * 2;
end;

function OffsetRectF(var Rect: TRectF; X, Y: Single): TRectF;
begin
  Rect.X := Rect.X + X;
  Rect.Y := Rect.Y + Y;
end;

{ TObject }

var
  StartupInput: TGdiPlusStartupInput;
  GdiPlusToken: ULONG;

{ TObject }

procedure GdipLoad;
var
  P: Pointer;
begin
  P := GdipAlloc(SizeOf(Pointer));
  if P = nil then
  begin
    StartupInput.DebugEventCallback := nil;
    StartupInput.SuppressBackgroundThread := False;
    StartupInput.SuppressExternalCodecs := False;
    StartupInput.GdiplusVersion := 1;
    GdiplusStartup(GdiPlusToken, @StartupInput, nil);
  end
  else
    GdipFree(P);
end;

procedure GdipUnload;
begin
  GdiplusShutdown(GdiPlusToken);
end;

type
  TGdiObject = class(TInterfacedObject, IGdiInterface)
  protected
    FLastStatus: TStatus;
    function GetInstance: TObject;
    function GetLastStatus: TStatus;
    procedure SetLastStatus(Value: TStatus);
    function SetStatus(Status: TStatus): TStatus;
  public
    constructor Create;
  end;

constructor TGdiObject.Create;
begin
  inherited Create;
  GdipLoad;
end;

function TGdiObject.GetInstance: TObject;
begin
  Result := Self;
end;

function TGdiObject.GetLastStatus: TStatus;
begin
  Result := FLastStatus;
  FLastStatus := Ok;
end;

procedure TGdiObject.SetLastStatus(Value: TStatus);
begin
  SetStatus(Value);
end;

function TGdiObject.SetStatus(Status: TStatus): TStatus;
begin
  if Status <> Ok then
    FLastStatus := Status;
  Result := Status;
end;

{ Gdi plus classes }

type
  TGdiGraphics = class;
  TGdiPen = class;
  TGdiBrush = class;
  TGdiMatrix = class;
  TGdiBitmap = class;
  TGdiMetafile = class;
  TGdiFontFamily = class;
  TGdiGraphicsPath = class;
  TGdIGdiRegion = class;
  TGdiImage = class;
  TGdiHatchBrush = class;
  TGdiSolidBrush = class;
  TGdiLinearGradientBrush = class;
  TGdiGradientBrush = class;
  TGdiFont = class;
  TGdiFontCollection = class;
  TGdiInstalledFontCollection = class;
  TGdiPrivateFontCollection = class;
  TGdiImageAttributes = class;
  TGdiCachedBitmap = class;
  TGdiStringFormat = class;

{ TGdIGdiRegion }

  TGdIGdiRegion = class(TGdiObject, IGdiRegion)
  private
    FNativeRegion: GpRegion;
    procedure SetNativeRegion(Region: GpRegion);
    function GetNativeRegion: GpRegion;
  public
    constructor Create; overload;
    constructor Create(Region: GpRegion); overload;
    constructor Create(Rect: TRectF); overload;
    constructor Create(Rect: TRectI); overload;
    constructor Create(Path: IGdiGraphicsPath); overload;
    constructor Create(RegionData: PByte; Size: Integer); overload;
    constructor Create(Rgn: HRGN); overload;
    destructor Destroy; override;
    function FromHRGN(Rgn: HRGN): IGdiRegion;
    function Clone: IGdiRegion;
    function MakeInfinite: TStatus;
    function MakeEmpty: TStatus;
    function GetDataSize: UInt;
    function GetData(Buffer: PByte; BufferSize: UInt;
      SizeFilled: PUInt = nil): TStatus;
    function Intersect(const Rect: TRectI): TStatus; overload;
    function Intersect(const Rect: TRectF): TStatus; overload;
    function Intersect(Path: IGdiGraphicsPath): TStatus; overload;
    function Intersect(Region: IGdiRegion): TStatus; overload;
    function Union(const Rect: TRectI): TStatus; overload;
    function Union(const Rect: TRectF): TStatus; overload;
    function Union(Path: IGdiGraphicsPath): TStatus; overload;
    function Union(Region: IGdiRegion): TStatus; overload;
    function Xor_(const Rect: TRectI): TStatus; overload;
    function Xor_(const Rect: TRectF): TStatus; overload;
    function Xor_(Path: IGdiGraphicsPath): TStatus; overload;
    function Xor_(Region: IGdiRegion): TStatus; overload;
    function Exclude(const Rect: TRectI): TStatus; overload;
    function Exclude(const Rect: TRectF): TStatus; overload;
    function Exclude(Path: IGdiGraphicsPath): TStatus; overload;
    function Exclude(Region: IGdiRegion): TStatus; overload;
    function Complement(const Rect: TRectI): TStatus; overload;
    function Complement(const Rect: TRectF): TStatus; overload;
    function Complement(Path: IGdiGraphicsPath): TStatus; overload;
    function Complement(Region: IGdiRegion): TStatus; overload;
    function Translate(DX, DY: Single): TStatus; overload;
    function Translate(DX, DY: Integer): TStatus; overload;
    function Transform(Matrix: IGdiMatrix): TStatus;
    function GetBounds(out Rect: TRectI; G: IGdiGraphics): TStatus; overload;
    function GetBounds(out Rect: TRectF; G: IGdiGraphics): TStatus; overload;
    function GetHRGN(G: IGdiGraphics): HRGN;
    function IsEmpty(G: IGdiGraphics): Boolean;
    function IsInfinite(G: IGdiGraphics): Boolean;
    function IsVisible(X, Y: Integer; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(const Point: TPointI; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Single; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(const Point: TPointF; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Integer; G: IGdiGraphics): Boolean; overload;
    function IsVisible(const Rect: TRectI; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Single; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(const Rect: TRectF; G: IGdiGraphics = nil): Boolean; overload;
    function Equals(Region: IGdiRegion; G: IGdiGraphics): Boolean; reintroduce;
    function GetRegionScansCount(Matrix: IGdiMatrix): UInt;
    function GetRegionScans(Matrix: IGdiMatrix; Rects: PRectF; out Count: Integer): TStatus; overload;
    function GetRegionScans(Matrix: IGdiMatrix; Rects: PRectI; out Count: Integer): TStatus; overload;
  end;

{ TGdiFontFamily }

  TGdiFontFamily = class(TGdiObject, IGdiFontFamily)
  private
    FNativeFamily: GpFontFamily;
    function GetNativeFamily: GpFontFamily;
    procedure SetNativeFamily(Value: GpFontFamily);
  public
    constructor Create; overload;
    constructor Create(Orig: GpFontFamily; Status: TStatus); overload;
    constructor Create(Name: WideString; FontCollection: IGdiFontCollection = nil); overload;
    destructor Destroy; override;
    class function GenericSansSerif: IGdiFontFamily;
    class function GenericSerif: IGdiFontFamily;
    class function GenericMonospace: IGdiFontFamily;
    function GetFamilyName(out Name: string; Language: LangId = 0): TStatus;
    function Clone: IGdiFontFamily;
    function IsAvailable: Boolean;
    function IsStyleAvailable(Style: Integer): Boolean;
    function GetEmHeight(Style: Integer): UInt16;
    function GetCellAscent(Style: Integer): UInt16;
    function GetCellDescent(Style: Integer): UInt16;
    function GetLineSpacing(Style: Integer): UInt16;
  end;

{ TGdiFontCollection }

  TGdiFontCollection = class(TGdiObject, IGdiFontCollection)
  private
    FNativeFontCollection: GpFontCollection;
    function GetNativeFontCollection: GpFontCollection;
    procedure SetNativeFontCollection(Value: GpFontCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function GetFamilyCount: Integer;
    function GetFamilies(NumSought: Integer; out Families: array of IGdiFontFamily;
      out NumFound: Integer): TStatus;
  end;

{ TGdiInstalledFontCollection }

  TGdiInstalledFontCollection = class(TGdiFontCollection, IGdiInstalledFontCollection)
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TGdiPrivateFontCollection }

  TGdiPrivateFontCollection = class(TGdiFontCollection, IGdiPrivateFontCollection)
  public
    constructor Create;
    destructor Destroy; override;
    function AddFontFile(Filename: WideString): TStatus;
    function AddMemoryFont(Memory: Pointer; Length: Integer): TStatus;
  end;

{ TGdiFont }

  TGdiFont = class(TGdiObject, IGdiFont)
  private
    FNativeFont: GpFont;
    function GetNativeFont: GpFont;
    procedure SetNativeFont(Font: GpFont);
    constructor Create(Font: GpFont; Status: TStatus); overload;
  public
    constructor Create(DC: HDC); overload;
    constructor Create(DC: HDC; LogFont: PLogFontA); overload;
    constructor Create(DC: HDC; LogFont: PLogFontW); overload;
    constructor Create(DC: HDC; Font: HFont); overload;
    constructor Create(Family: IGdiFontFamily; EmSize: Single;
      Style: TFontStyle = FontStyleRegular;
      Unit_: TUnit = UnitPoint); overload;
    constructor Create(FamilyName: WideString; EmSize: Single;
      Style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint;
      FontCollection: IGdiFontCollection = nil); overload;
    function GetLogFontA(G: IGdiGraphics; out LogFontA: TLogFontA): TStatus;
    function GetLogFontW(G: IGdiGraphics; out LogFontW: TLogFontW): TStatus;
    function Clone: IGdiFont;
    destructor Destroy; override;
    function IsAvailable: Boolean;
    function GetStyle: Integer;
    function GetSize: Single;
    function GetUnit: TUnit;
    function GetHeight(Graphics: IGdiGraphics): Single; overload;
    function GetHeight(DPI: Single): Single; overload;
    function GetFamily(Family: IGdiFontFamily): TStatus;
  end;

{ TGdiImage }

  TGdiImage = class(TGdiObject, IGdiImage)
  private
    FNativeImage: GpImage;
    function GetNativeImage: GpImage;
    procedure SetNativeImage(Image: GpImage);
  public
    constructor Create; overload;
    constructor Create(Image: GpImage; Status: TStatus); overload;
    constructor Create(Filename: WideString; UseEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(Stream: IStream; UseEmbeddedColorManagement: Boolean = False); overload;
    function FromFile(Filename: WideString; UseEmbeddedColorManagement: Boolean = False): IGdiImage; virtual;
    function FromStream(Stream: IStream; UseEmbeddedColorManagement: Boolean = False): IGdiImage; virtual;
    destructor Destroy; override;
    function Clone: IGdiImage;
    function Save(Filename: WideString; const clsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(Stream: IStream; const clsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function SaveAdd(EncoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(NewImage: IGdiImage; EncoderParams: PEncoderParameters): TStatus; overload;
    function GetType: TImageType;
    function GetPhysicalDimension(out Size: TSizeF): TStatus;
    function GetBounds(out SrcRect: TRectF; out SrcUnit: TUnit): TStatus;
    function GetWidth: UInt;
    function GetHeight: UInt;
    function GetHorizontalResolution: Single;
    function GetVerticalResolution: Single;
    function GetFlags: UInt;
    function GetRawFormat(out Format: TGUID): TStatus;
    function GetPixelFormat: TPixelFormat;
    function GetPaletteSize: Integer;
    function GetPalette(palette: PColorPalette; Size: Integer): TStatus;
    function SetPalette(palette: PColorPalette): TStatus;
    function GetThumbnailImage(ThumbWidth, ThumbHeight: UInt;
      Callback: GetThumbnailImageAbort = nil; CallbackData: Pointer = nil): IGdiImage;
    function GetFrameDimensionsCount: UInt;
    function GetFrameDimensionsList(DimensionIDs: PGUID; Count: UInt): TStatus;
    function GetFrameCount(const DimensionID: TGUID): UInt;
    function SelectActiveFrame(const DimensionID: TGUID; FrameIndex: UInt): TStatus;
    function RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
    function GetPropertyCount: UInt;
    function GetPropertyIdList(numOfProperty: UInt; list: PPropID): TStatus;
    function GetPropertyItemSize(PropId: PropID): UInt;
    function GetPropertyItem(PropId: PropID; PropSize: UInt; Buffer: PPropertyItem): TStatus;
    function GetPropertySize(out TotalBufferSize, NumProperties: UInt): TStatus;
    function GetAllPropertyItems(TotalBufferSize, NumProperties: UInt;
      AllItems: PPropertyItem): TStatus;
    function RemovePropertyItem(PropId: TPropID): TStatus;
    function SetPropertyItem(const item: TPropertyItem): TStatus;
    function GetEncoderParameterListSize(const clsidEncoder: TGUID): UInt;
    function GetEncoderParameterList(const clsidEncoder: TGUID; Size: UInt;
      Buffer: PEncoderParameters): TStatus;
  end;

{ TGdiBitmap }

  TGdiBitmap = class(TGdiImage, IGdiBitmap)
  public
    constructor Create(Bitmap: GpBitmap); overload;
    constructor Create(Filename: WideString; UseEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(Stream: IStream; UseEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(Width, Height, Stride: Integer; Format: TPixelFormat; scan0: PByte); overload;
    constructor Create(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppArgb); overload;
    constructor Create(Width, Height: Integer; Target: IGdiGraphics); overload;
    function FromFile(Filename: WideString; UseEmbeddedColorManagement: Boolean = False): IGdiImage; override;
    function FromStream(Stream: IStream; UseEmbeddedColorManagement: Boolean = False): IGdiImage; override;
    function Clone(Rect: TRectI; Format: TPixelFormat): IGdiBitmap; overload;
    function Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IGdiBitmap; overload;
    function Clone(Rect: TRectF; Format: TPixelFormat): IGdiBitmap; overload;
    function Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IGdiBitmap; overload;
    function LockBits(Rect: TRectI; Flags: UInt; Format: TPixelFormat; out LockedBitmapData: TBitmapData): TStatus;
    function UnlockBits(var LockedBitmapData: TBitmapData): TStatus;
    function GetPixel(X, Y: Integer; out Color: TArgb): TStatus;
    function SetPixel(X, Y: Integer; Color: TArgb): TStatus;
    function SetResolution(XDPI, YDPI: Single): TStatus;
    constructor Create(surface: IDirectDrawSurface7); overload;
    constructor Create(var BitmapInfo: TBitmapInfo; BitmapData: Pointer); overload;
    constructor Create(Bmp: HBitmap; Pal: HPALETTE); overload;
    constructor Create(Icon: HICON); overload;
    constructor Create(hInstance: HMODULE; BitmapName: WideString); overload;
    function FromDiRectDrawSurface7(surface: IDirectDrawSurface7): IGdiBitmap;
    function FromBitmapInfo(var BitmapInfo: TBitmapInfo; BitmapData: Pointer): IGdiBitmap;
    function FromHBitmap(Bmp: HBitmap; Pal: HPALETTE): IGdiBitmap;
    function FromHICON(Icon: HICON): IGdiBitmap;
    function FromResource(hInstance: HMODULE; BitmapName: WideString): IGdiBitmap;
    function GetHBitmap(ColorBackground: TArgb; out Bmp: HBitmap): TStatus;
    function GetHICON(out Icon: HICON): TStatus;
  end;

{ TCustomLineCap }

  TCustomLineCap = class(TGdiObject, IGdiCustomLineCap)
  private
    FNativeCap: GpCustomLineCap;
    function GetNativeCap: GpCustomLineCap;
    procedure SetNativeCap(Cap: GpCustomLineCap);
  public
    constructor Create; overload;
    constructor Create(Cap: GpCustomLineCap; Status: TStatus); overload;
    constructor Create(FillPath, StrokePath: IGdiGraphicsPath;
      BaseCap: TLineCap = LineCapFlat;
      BaseInset: Single = 0); overload;
    destructor Destroy; override;
    function Clone: IGdiCustomLineCap;
    function SetStrokeCap(StrokeCap: TLineCap): TStatus;
    function SetStrokeCaps(StartCap, EndCap: TLineCap): TStatus;
    function GetStrokeCaps(out StartCap, EndCap: TLineCap): TStatus;
    function SetStrokeJoin(lineJoin: TLineJoin): TStatus;
    function GetStrokeJoin: TLineJoin;
    function SetBaseCap(BaseCap: TLineCap): TStatus;
    function GetBaseCap: TLineCap;
    function SetBaseInset(Inset: Single): TStatus;
    function GetBaseInset: Single;
    function SetWidthScale(WidthScale: Single): TStatus;
    function GetWidthScale: Single;
  end;

{ TGdiCachedBitmap }

  TGdiCachedBitmap = class(TGdiObject, IGdiCachedBitmap)
  private
    FNativeCachedBitmap: GpCachedBitmap;
    function GetNativeCachedBitmap: GpCachedBitmap;
  public
    constructor Create(Bitmap: IGdiBitmap; Graphics: IGdiGraphics);
    destructor Destroy; override;
  end;

{ TGdiImageAttributes }

  TGdiImageAttributes = class(TGdiObject, IGdiImageAttributes)
  private
    FNativeImageAttr: GpImageAttributes;
    function GetNativeImageAttr: GpImageAttributes;
    procedure SetNativeImageAttr(ImageAttr: GpImageAttributes);
  public
    constructor Create; overload;
    constructor Create(ImageAttr: GpImageAttributes; Status: GpStatus); overload;
    destructor Destroy; override;
    function Clone: IGdiImageAttributes;
    function SetToIdentity(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function Reset(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrix(const ColorMatrix: TColorMatrix;
      mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrix(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorMatrices(const ColorMatrix: TColorMatrix; const grayMatrix: TColorMatrix;
      mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
      Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetThreshold(threshold: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearThreshold(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetGamma(gamma: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearGamma(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetColorKey(ColorLow, ColorHigh: TArgb; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearColorKey(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannel(channelFlags: TColorChannelFlags; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannel(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetOutputChannelColorProfile(ColorProfileFilename: WideString;
      Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearOutputChannelColorProfile(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetRemapTable(mapSize: cardinal; map: PColorMap; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function ClearRemapTable(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
    function SetBrushRemapTable(mapSize: cardinal; map: PColorMap): TStatus;
    function ClearBrushRemapTable: TStatus;
    function SetWrapMode(Wrap: TWrapMode; Color: TArgb = aclBlack; clamp: Boolean = False): TStatus;
    function GetAdjustedPalette(ColorPalette: PColorPalette; ColorAdjustType: TColorAdjustType): TStatus;
  end;

{ TGdiMatrix }

  TGdiMatrix = class(TGdiObject, IGdiMatrix)
  private
    FNativeMatrix: GpMatrix;
    function GetNativeMatrix: GpMatrix;
    procedure SetNativeMatrix(Matrix: GpMatrix);
  public
    constructor Create; overload;
    constructor Create(Matrix: GpMatrix); overload;
    constructor Create(M11, M12, M21, M22, DX, DY: Single); overload;
    constructor Create(const Rect: TRectF; const PlgPts: TPointF); overload;
    constructor Create(const Rect: TRectI; const PlgPts: TPointI); overload;
    destructor Destroy; override;
    function Clone: IGdiMatrix;
    function GetElements(const M: TMatrixArray): TStatus;
    function SetElements(M11, M12, M21, M22, DX, DY: Single): TStatus;
    function OffsetX: Single;
    function OffsetY: Single;
    function Reset: TStatus;
    function Multiply(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Translate(OffsetX, OffsetY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Scale(ScaleX, ScaleY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Rotate(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateAt(Angle: Single; const center: TPointF; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Shear(ShearX, ShearY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function Invert: TStatus;
    function TransformPoints(Pts: PPointF; Count: Integer = 1): TStatus; overload;
    function TransformPoints(Pts: PPointI; Count: Integer = 1): TStatus; overload;
    function TransformVectors(Pts: PPointF; Count: Integer = 1): TStatus; overload;
    function TransformVectors(Pts: PPointI; Count: Integer = 1): TStatus; overload;
    function IsInvertible: Boolean;
    function IsIdentity: Boolean;
    function Equals(Matrix: IGdiMatrix): Boolean; reintroduce;
  end;

{ TGdiBrush }

  TGdiBrush = class(TGdiObject, IGdiBrush)
  private
    FNativeBrush: GpBrush;
    procedure SetNativeBrush(Brush: GpBrush);
    function GetNativeBrush: GpBrush;
  public
    constructor Create; overload;
    constructor Create(Brush: GpBrush; Status: TStatus); overload;
    destructor Destroy; override;
    function Clone: IGdiBrush; virtual;
    function GetType: TBrushType;
  end;

{ TGdiSolidBrush }

  TGdiSolidBrush = class(TGdiBrush, IGdiSolidBrush)
  public
    constructor Create(Color: TArgb); overload;
    constructor Create; overload;
    function GetColor(out Color: TArgb): TStatus;
    function SetColor(Color: TArgb): TStatus;
  end;

{ TTextureBrush }

  TTextureBrush = class(TGdiBrush, IGdiTextureBrush)
  public
    constructor Create(Image: IGdiImage; WrapMode: TWrapMode = WrapModeTile); overload;
    constructor Create(Image: IGdiImage; WrapMode: TWrapMode; DstRect: TRectF); overload;
    constructor Create(Image: IGdiImage; DstRect: TRectF; ImageAttributes: IGdiImageAttributes = nil); overload;
    constructor Create(Image: IGdiImage; DstRect: TRectI; ImageAttributes: IGdiImageAttributes = nil); overload;
    constructor Create(Image: IGdiImage; WrapMode: TWrapMode; DstRect: TRectI); overload;
    constructor Create(Image: IGdiImage; WrapMode: TWrapMode; DstX, DstY, DstWidth,
      DstHeight: Single); overload;
    constructor Create(Image: IGdiImage; WrapMode: TWrapMode; DstX, DstY, DstWidth,
      DstHeight: Integer); overload;
    constructor Create; overload;
    function SetTransform(Matrix: IGdiMatrix): TStatus;
    function GetTransform(Matrix: IGdiMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: MatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
    function GetImage: IGdiImage;
  end;

{ TGdiLinearGradientBrush }

  TGdiLinearGradientBrush = class(TGdiBrush, IGdiLinearGradientBrush)
  public
    constructor Create(const Point1, Point2: TPointF; Color1,
      Color2: TArgb); overload;
    constructor Create(const Point1, Point2: TPointI; Color1,
      Color2: TArgb); overload;
    constructor Create(Rect: TRectF; Color1, Color2: TArgb;
      mode: TLinearGradientMode); overload;
    constructor Create(Rect: TRectI; Color1, Color2: TArgb;
      mode: TLinearGradientMode); overload;
    constructor Create(Rect: TRectF; Color1, Color2: TArgb; Angle: Single;
      isAngleScalable: Boolean = False); overload;
    constructor Create(Rect: TRectI; Color1, Color2: TArgb; Angle: Single;
      isAngleScalable: Boolean = False); overload;
    function SetLinearColors(Color1, Color2: TArgb): TStatus;
    function GetLinearColors(out Color1, Color2: TArgb): TStatus;
    function GetRectangle(out Rect: TRectF): TStatus; overload;
    function GetRectangle(out Rect: TRectI): TStatus; overload;
    function SetGammaCorrection(UseGammaCorrection: Boolean): TStatus;
    function GetGammaCorrection: Boolean;
    function GetBlendCount: Integer;
    function SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle; Count: Integer): TStatus;
    function SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
    function SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
    function SetTransform(Matrix: IGdiMatrix): TStatus;
    function GetTransform(Matrix: IGdiMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
  end;

{ TGdiHatchBrush }

  TGdiHatchBrush = class(TGdiBrush, IGdiHatchBrush)
  public
    constructor Create; overload;
    constructor Create(HatchStyle: THatchStyle; ForeColor: TArgb; BackColor: TArgb = aclBlack); overload;
    function GetHatchStyle: THatchStyle;
    function GetForegrounDColor(out Color: TArgb): TStatus;
    function GetBackgrounDColor(out Color: TArgb): TStatus;
  end;

{ TGdiPen }

  TGdiPen = class(TGdiObject, IGdiPen)
  private
    FNativePen: GpPen;
    procedure SetNativePen(Pen: GpPen);
    function GetNativePen: GpPen;
  public
    constructor Create(Pen: GpPen; Status: TStatus); overload;
    constructor Create(Color: TArgb; Width: Single = 1); overload;
    constructor Create(Brush: IGdiBrush; Width: Single = 1); overload;
    destructor Destroy; override;
    function Clone: IGdiPen; overload;
    procedure SetWidth(Width: Single);
    function GetWidth: Single;
    function SetLineCap(StartCap, EndCap: TLineCap; DashCap: TDashCap): TStatus;
    function SetStartCap(StartCap: TLineCap): TStatus;
    function SetEndCap(EndCap: TLineCap): TStatus;
    function SetDashCap(DashCap: TDashCap): TStatus;
    function GetStartCap: TLineCap;
    function GetEndCap: TLineCap;
    function GetDashCap: TDashCap;
    function SetLineJoin(lineJoin: TLineJoin): TStatus;
    function GetLineJoin: TLineJoin;
    function SetCustomStartCap(CustomCap: IGdiCustomLineCap): TStatus;
    function GetCustomStartCap(CustomCap: IGdiCustomLineCap): TStatus;
    function SetCustomEndCap(CustomCap: IGdiCustomLineCap): TStatus;
    function GetCustomEndCap(CustomCap: IGdiCustomLineCap): TStatus;
    function SetMiterLimit(MiterLimit: Single): TStatus;
    function GetMiterLimit: Single;
    function SetAlignment(PenAlignment: TPenAlignment): TStatus;
    function GetAlignment: TPenAlignment;
    function SetTransform(Matrix: IGdiMatrix): TStatus;
    function GetTransform(Matrix: IGdiMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetPenType: TPenType;
    function SetColor(Color: TArgb): TStatus;
    function SetBrush(Brush: IGdiBrush): TStatus;
    function GetColor(out Color: TArgb): TStatus;
    function GetBrush: IGdiBrush;
    function GetDashStyle: TDashStyle;
    function SetDashStyle(DashStyle: TDashStyle): TStatus;
    function GetDashOffset: Single;
    function SetDashOffset(DashOffset: Single): TStatus;
    function SetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
    function GetDashPatternCount: Integer;
    function GetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
    function SetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
    function GetCompoundArrayCount: Integer;
    function GetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
  end;

{ TGdiStringFormat }

  TGdiStringFormat = class(TGdiObject, IGdiStringFormat)
  private
    FNativeFormat: GpStringFormat;
    function GetNativeFormat: GpStringFormat;
    procedure SetNativeFormat(Value: GpStringFormat);
  public
    constructor Create(ClonedStringFormat: GpStringFormat; Status: TStatus); overload;
    constructor Create(FormatFlags: Integer = 0; Language: LangId = LANG_NEUTRAL); overload;
    constructor Create(Format: IGdiStringFormat); overload;
    destructor Destroy; override;
    class function GenericDefault: IGdiStringFormat;
    class function GenericTypoGraphic: IGdiStringFormat;
    function Clone: IGdiStringFormat;
    function SetFormatFlags(Flags: Integer): TStatus;
    function GetFormatFlags: Integer;
    function SetAlignment(Align: TStringAlignment): TStatus;
    function GetAlignment: TStringAlignment;
    function SetLineAlignment(Align: TStringAlignment): TStatus;
    function GetLineAlignment: TStringAlignment;
    function SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TStatus;
    function GetHotkeyPrefix: THotkeyPrefix;
    function SetTabStops(FirstTabOffset: Single; Count: Integer; TabStops: PSingle): TStatus;
    function GetTabStopCount: Integer;
    function GetTabStops(Count: Integer; FirstTabOffset, TabStops: PSingle): TStatus;
    function SetDigitSubstitution(Language: LangId; substitute: TStringDigitSubstitute): TStatus;
    function GetDigitSubstitutionLanguage: LangId;
    function GetDigitSubstitutionMethod: TStringDigitSubstitute;
    function SetTrimming(Trimming: TStringTrimming): TStatus;
    function GetTrimming: TStringTrimming;
    function SetMeasurableCharacterRanges(rangeCount: Integer; ranges: PCharacterRange): TStatus;
    function GetMeasurableCharacterRangeCount: Integer;
  end;

{ TGdiGraphicsPath }

  TGdiGraphicsPath = class(TGdiObject, IGdiGraphicsPath)
  private
    FNativePath: GpPath;
    function GetNativePath: GpPath;
    procedure SetNativePath(Path: GpPath);
  public
    constructor Create(Path: GpPath); overload;
    constructor Create(Path: IGdiGraphicsPath); overload;
    constructor Create(FillMode: TFillMode = FillModeAlternate); overload;
    constructor Create(Points: PPointF; Types: PByte; Count: Integer;
      FillMode: TFillMode = FillModeAlternate); overload;
    constructor Create(Points: PPointI; Types: PByte; Count: Integer;
      FillMode: TFillMode = FillModeAlternate); overload;
    destructor Destroy; override;
    function Clone: IGdiGraphicsPath;
    function Reset: TStatus;
    function GetFillMode: TFillMode;
    function SetFillMode(Fillmode: TFillMode): TStatus;
    function GetPathData(var PathData: TPathData): TStatus;
    function StartFigure: TStatus;
    function CloseFigure: TStatus;
    function CloseAllFigures: TStatus;
    function SetMarker: TStatus;
    function ClearMarkers: TStatus;
    function Reverse: TStatus;
    function GetLastPoint(out lastPoint: TPointF): TStatus;
    function AddLine(const Pt1, Pt2: TPointF): TStatus; overload;
    function AddLine(X1, Y1, X2, Y2: Single): TStatus; overload;
    function AddLines(Points: PPointF; Count: Integer): TStatus; overload;
    function AddLine(const Pt1, Pt2: TPointI): TStatus; overload;
    function AddLine(X1, Y1, X2, Y2: Integer): TStatus; overload;
    function AddLines(Points: PPointI; Count: Integer): TStatus; overload;
    function AddArc(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddArc(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function AddArc(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddArc(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddBezier(Pt1, Pt2, Pt3, Pt4: TPointF): TStatus; overload;
    function AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus; overload;
    function AddBeziers(Points: PPointF; Count: Integer): TStatus; overload;
    function AddBezier(Pt1, Pt2, Pt3, Pt4: TPointI): TStatus; overload;
    function AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus; overload;
    function AddBeziers(Points: PPointI; Count: Integer): TStatus; overload;
    function AddCurve(Points: PPointF; Count: Integer): TStatus; overload;
    function AddCurve(Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function AddCurve(Points: PPointF; Count, Offset, NumberOfSegments: Integer; Tension: Single): TStatus; overload;
    function AddCurve(Points: PPointI; Count: Integer): TStatus; overload;
    function AddCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function AddCurve(Points: PPointI; Count, Offset, NumberOfSegments: Integer; Tension: Single): TStatus; overload;
    function AddClosedCurve(Points: PPointF; Count: Integer): TStatus; overload;
    function AddClosedCurve(Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function AddClosedCurve(Points: PPointI; Count: Integer): TStatus; overload;
    function AddClosedCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function AddRectangle(Rect: TRectF): TStatus; overload;
    function AddRectangles(Rects: PRectF; Count: Integer): TStatus; overload;
    function AddRectangle(Rect: TRectI): TStatus; overload;
    function AddRectangles(Rects: PRectI; Count: Integer): TStatus; overload;
    function AddEllipse(Rect: TRectF): TStatus; overload;
    function AddEllipse(X, Y, Width, Height: Single): TStatus; overload;
    function AddEllipse(Rect: TRectI): TStatus; overload;
    function AddEllipse(X, Y, Width, Height: Integer): TStatus; overload;
    function AddPie(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPie(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPie(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPie(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function AddPolygon(Points: PPointF; Count: Integer): TStatus; overload;
    function AddPolygon(Points: PPointI; Count: Integer): TStatus; overload;
    function AddPath(AddingPath: IGdiGraphicsPath; Connect: Boolean): TStatus;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; Origin: TPointF; Format: IGdiStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; LayoutRect: TRectF; Format: IGdiStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; Origin: TPointI; Format: IGdiStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IGdiFontFamily;
      Style: Integer; EmSize: Single; LayoutRect: TRectI; Format: IGdiStringFormat): TStatus; overload;
    function Transform(Matrix: IGdiMatrix): TStatus;
    function GetBounds(out Bounds: TRectF; Matrix: IGdiMatrix = nil; Pen: IGdiPen = nil): TStatus; overload;
    function GetBounds(out Bounds: TRectI; Matrix: IGdiMatrix = nil; Pen: IGdiPen = nil): TStatus; overload;
    function Flatten(Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Widen(Pen: IGdiPen; Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Outline(Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Warp(DestPoints: PPointF; Count: Integer; SrcRect: TRectF;
      Matrix: IGdiMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
      Flatness: Single = FlatnessDefault): TStatus;
    function GetPointCount: Integer;
    function GetPathTypes(Types: PByte; Count: Integer): TStatus;
    function GetPathPoints(Points: PPointF; Count: Integer): TStatus; overload;
    function GetPathPoints(Points: PPointI; Count: Integer): TStatus; overload;
    function IsVisible(Point: TPointF; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Single; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(Point: TPointI; G: IGdiGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Integer; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(Point: TPointF; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(X, Y: Single; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(Point: TPointI; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
    function IsOutlineVisible(X, Y: Integer; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean; overload;
  end;

{ TGdiGraphicsPathIterator }

  TGdiGraphicsPathIterator = class(TGdiObject, IGdiGraphicsPathIterator)
  private
    FNativeIterator: GpPathIterator;
    procedure SetNativeIterator(FNativeIterator: GpPathIterator);
  public
    constructor Create(Path: IGdiGraphicsPath);
    destructor Destroy; override;
    function NextSubPath(out StartIndex, EndIndex: Integer; out IsClosed: Boolean): Integer; overload;
    function NextSubPath(Path: IGdiGraphicsPath; out IsClosed: Boolean): Integer; overload;
    function NextPathType(out PathType: TPathPointType; out StartIndex, EndIndex: Integer): Integer;
    function NextMarker(out StartIndex, EndIndex: Integer): Integer; overload;
    function NextMarker(Path: IGdiGraphicsPath): Integer; overload;
    function GetCount: Integer;
    function GetSubPathCount: Integer;
    function HasCurve: Boolean;
    procedure Rewind;
    function Enumerate(Points: PPointF; Types: PByte; Count: Integer): Integer;
    function CopyData(Points: PPointF; Types: PByte; StartIndex, EndIndex: Integer): Integer;
  end;

{ TGdiGradientBrush }

  TGdiGradientBrush = class(TGdiBrush)
  public
    constructor Create; overload;
    constructor Create(Path: IGdiGraphicsPath); overload;
    constructor Create(Points: PPointF; Count: Integer;
      WrapMode: TWrapMode = WrapModeClamp); overload;
    constructor Create(Points: PPointI; Count: Integer;
      WrapMode: TWrapMode = WrapModeClamp); overload;
    function GetCenterColor(out Color: TArgb): TStatus;
    function SetCenterColor(Color: TArgb): TStatus;
    function GetPointCount: Integer;
    function GetSurrounDColorCount: Integer;
    function GetSurrounDColors(Colors: PArgb; var Count: Integer): TStatus;
    function SetSurrounDColors(Colors: PArgb; var Count: Integer): TStatus;
    function GeTGdiGraphicsPath(Path: IGdiGraphicsPath): TStatus;
    function SeTGdiGraphicsPath(Path: IGdiGraphicsPath): TStatus;
    function GetCenterPoint(out Point: TPointF): TStatus; overload;
    function GetCenterPoint(out Point: TPointI): TStatus; overload;
    function SetCenterPoint(Point: TPointF): TStatus; overload;
    function SetCenterPoint(Point: TPointI): TStatus; overload;
    function GetRectangle(out Rect: TRectF): TStatus; overload;
    function GetRectangle(out Rect: TRectI): TStatus; overload;
    function SetGammaCorrection(UseGammaCorrection: Boolean): TStatus; overload;
    function GetGammaCorrection: Boolean; overload;
    function GetBlendCount: Integer;
    function GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
    function GetInterpolationColorCount: Integer;
    function SetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle;
      Count: Integer): TStatus;
    function GetInterpolationColors(PresetColors: PArgb;
      BlendPositions: PSingle; Count: Integer): TStatus;
    function SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
    function SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
    procedure SetTransform(Matrix: IGdiMatrix);
    function  GetTransform: IGdiMatrix;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single;
      Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetFocusScales(out XScale, YScale: Single): TStatus;
    function SetFocusScales(XScale, YScale: Single): TStatus;
    function GetWrapMode: TWrapMode;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
  end;

{ TGdiGraphics }

  TGdiGraphicsBitmap = record
    DC: HDC;
    Handle: HBITMAP;
    Bits: Pointer;
    Width: Integer;
    Height: Integer;
    Opacity: Byte;
  end;
  PGdiGraphicsBitmap = ^TGdiGraphicsBitmap;

  TGdiGraphics = class(TGdiObject, IGdiGraphics)
  private
    FNativeGraphics: GpGraphics;
    FBitmap: PGdiGraphicsBitmap;
    function GetNativeGraphics: GpGraphics;
    procedure SetNativeGraphics(Graphics: GpGraphics);
  public
    constructor Create(Graphics: GpGraphics); overload;
    constructor Create(DC: HDC); overload;
    constructor Create(DC: HDC; Device: THandle); overload;
    constructor Create(Wnd: HWND; ICM: Boolean); overload;
    constructor Create(Image: IGdiImage); overload;
    constructor Create(Width, Height: Integer); overload;
    destructor Destroy; override;
    function FromHDC(DC: HDC): IGdiGraphics; overload;
    function FromHDC(DC: HDC; Device: THandle): IGdiGraphics; overload;
    function FromHWND(hwnd: HWND; ICM: Boolean = False): IGdiGraphics;
    function FromImage(Image: IGdiImage): IGdiGraphics;
    procedure Flush(intention: TFlushIntention = FlushIntentionFlush);
    function GetHDC: HDC;
    procedure ReleaseHDC(DC: HDC);
    function SetRenderingOrigin(X, Y: Integer): TStatus;
    function GetRenderingOrigin(out X, Y: Integer): TStatus;
    function SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
    function GetCompositingMode: TCompositingMode;
    function SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
    function GetCompositingQuality: TCompositingQuality;
    procedure SetTextRenderingHint(NewMode: TTextRenderingHint);
    function GetTextRenderingHint: TTextRenderingHint;
    function SetTextContrast(contrast: UInt): TStatus;
    function GetTextContrast: UInt;
    function GetInterpolationMode: TInterpolationMode;
    function SetInterpolationMode(interpolationMode: TInterpolationMode): TStatus;
    function GetSmoothingMode: TSmoothingMode;
    procedure SetSmoothingMode(smoothingMode: TSmoothingMode);
    function GetPixelOffsetMode: TPixelOffsetMode;
    function SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    procedure SetTransform(Matrix: IGdiMatrix);
    function GetTransform: IGdiMatrix;
    function SetPageUnit(Unit_: TUnit): TStatus;
    function SetPageScale(Scale: Single): TStatus;
    function GetPageUnit: TUnit;
    function GetPageScale: Single;
    function GetDPIX: Single;
    function GetDPIY: Single;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetOpacity: Byte;
    procedure SetOpacity(Value: Byte);
    function TransformPoints(DestSpace: TCoordinateSpace; SrcSpace: TCoordinateSpace;
      Pts: PPointF; Count: Integer): TStatus; overload;
    function TransformPoints(DestSpace: TCoordinateSpace; SrcSpace: TCoordinateSpace;
      Pts: PPointI; Count: Integer): TStatus; overload;
    function GetNearestColor(var Color: TArgb): TStatus;
    function Overlay(Wnd: HWND; Opacity: Byte = 0): TStatus;
    function Draw(DC: HDC; X, Y: Integer): TStatus; overload;
    function Draw(DC: HDC; X, Y, Width, Height: Integer): TStatus; overload;
    function Draw(DC: HDC; const Rect: TRectI): TStatus; overload;
    function Draw(DC: HDC; X, Y: Single): TStatus; overload;
    function Draw(DC: HDC; X, Y, Width, Height: Single): TStatus; overload;
    function Draw(DC: HDC; const Rect: TRectF): TStatus; overload;
    function DrawLine(Pen: IGdiPen; X1, Y1, X2, Y2: Single): TStatus; overload;
    function DrawLine(Pen: IGdiPen; const Pt1, Pt2: TPointF): TStatus; overload;
    function DrawLines(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawLine(Pen: IGdiPen; X1, Y1, X2, Y2: Integer): TStatus; overload;
    function DrawLine(Pen: IGdiPen; const Pt1, Pt2: TPointI): TStatus; overload;
    function DrawLines(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawArc(Pen: IGdiPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IGdiPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IGdiPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IGdiPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; const Pt1, Pt2, Pt3, Pt4: TPointF): TStatus; overload;
    function DrawBeziers(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus; overload;
    function DrawBezier(Pen: IGdiPen; const Pt1, Pt2, Pt3, Pt4: TPointI): TStatus; overload;
    function DrawBeziers(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; const Rect: TRectF): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawRectangles(Pen: IGdiPen; Rects: PRectF; Count: Integer): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; const Rect: TRectI): TStatus; overload;
    function DrawRectangle(Pen: IGdiPen; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawRectangles(Pen: IGdiPen; Rects: PRectI; Count: Integer): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; const Rect: TRectF): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; const Rect: TRectI): TStatus; overload;
    function DrawEllipse(Pen: IGdiPen; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawPie(Pen: IGdiPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IGdiPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IGdiPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IGdiPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPolygon(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawPolygon(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawPath(Pen: IGdiPen; Path: IGdiGraphicsPath): TStatus;
    function DrawCurve(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointF; Count, Offset,
      NumberOfSegments: Integer; Tension: Single = 0.5): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function DrawCurve(Pen: IGdiPen; Points: PPointI; Count, Offset, NumberOfSegments: Integer;
      Tension: Single = 0.5): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawClosedCurve(Pen: IGdiPen; Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function DrawString(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; StringFormat: IGdiStringFormat; Brush: IGdiBrush): TStatus; overload;
    function DrawString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; Brush: IGdiBrush): TStatus; overload;
    function DrawString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; StringFormat: IGdiStringFormat; Brush: IGdiBrush): TStatus; overload;
    function DrawCachedBitmap(Bitmap: IGdiCachedBitmap; X, Y: Integer): TStatus;
    function DrawImage(Image: IGdiImage; const Point: TPointF): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y: Single): TStatus; overload;
    function DrawImage(Image: IGdiImage; const Rect: TRectF): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, Width, Height: Single): TStatus; overload;
    function DrawImage(Image: IGdiImage; const Point: TPointI): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; const Rect: TRectI): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointF; Count: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointI; Count: Integer): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit): TStatus; overload;
    function DrawImage(Image: IGdiImage; const DestRect: TRectF; SrcX, SrcY,
      SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointF; Count: Integer;
      SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IGdiImage; X, Y, SrcX, SrcY, SrcWidth,
      SrcHeight: Integer; SrcUnit: TUnit): TStatus; overload;
    function DrawImage(Image: IGdiImage; const DestRect: TRectI; SrcX, SrcY,
      SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IGdiImage; DestPoints: PPointI;
      Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
      ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function Clear(Color: TArgb): TStatus;
    function FillRectangle(Brush: IGdiBrush; const Rect: TRectF): TStatus; overload;
    function FillRectangle(Brush: IGdiBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillRectangles(Brush: IGdiBrush; Rects: PRectF; Count: Integer): TStatus; overload;
    function FillRectangle(Brush: IGdiBrush; const Rect: TRectI): TStatus; overload;
    function FillRectangle(Brush: IGdiBrush; X, Y, Width, Height: Integer): TStatus; overload;
    function FillRectangles(Brush: IGdiBrush; Rects: PRectI; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointF; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointF; Count: Integer; FillMode: TFillMode): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointI; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IGdiBrush; Points: PPointI; Count: Integer; FillMode: TFillMode): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; const Rect: TRectF): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; const Rect: TRectI): TStatus; overload;
    function FillEllipse(Brush: IGdiBrush; X, Y, Width, Height: Integer): TStatus; overload;
    function FillPie(Brush: IGdiBrush; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IGdiBrush; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IGdiBrush; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IGdiBrush; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPath(Brush: IGdiBrush; Path: IGdiGraphicsPath): TStatus;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointF; Count: Integer): TStatus; overload;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointF; Count: Integer;
      FillMode: TFillMode; Tension: Single = 0.5): TStatus; overload;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointI; Count: Integer): TStatus; overload;
    function FillClosedCurve(Brush: IGdiBrush; Points: PPointI; Count: Integer;
      FillMode: TFillMode; Tension: Single = 0.5): TStatus; overload;
    function FillRegion(Brush: IGdiBrush; Region: IGdiRegion): TStatus;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; StringFormat: IGdiStringFormat; out BoundingBox: TRectF;
      CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const LayoutRectSize: TSizeF; StringFormat: IGdiStringFormat; out Size: TSizeF;
      CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; StringFormat: IGdiStringFormat;
      out BoundingBox: TRectF): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; out BoundingBox: TRectF): TStatus; overload;
    function MeasureString(Text: WideString; Font: IGdiFont;
      const Origin: TPointF; out BoundingBox: TRectF): TStatus; overload;
    function MeasureCharacterRanges(Text: WideString; Font: IGdiFont;
      const LayoutRect: TRectF; StringFormat: IGdiStringFormat; RegionCount: Integer;
      const Regions: array of IGdiRegion): TStatus; overload;
    function DrawDriverString(Text: PUInt16; Length: Integer; Font: IGdiFont;
      Brush: IGdiBrush; Positions: PPointF; Flags: Integer; Matrix: IGdiMatrix): TStatus;
    function MeasureDriverString(Text: PUInt16; Length: Integer; Font: IGdiFont;
      Positions: PPointF; Flags: Integer; Matrix: IGdiMatrix;
      out BoundingBox: TRectF): TStatus;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointF;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointI;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectF;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectI;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointF;
      Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointI;
      Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointF;
      const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointI;
      const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectF;
      const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; const DestRect, SrcRect: TRectI;
      SrcUnit: TUnit; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointF;
      Count: Integer; const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointI;
      Count: Integer; const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus; overload;
    function SetClip(G: IGdiGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TRectI; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Path: IGdiGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Region: IGdiRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rgn: HRGN; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function IntersectClip(const Rect: TRectF): TStatus; overload;
    function IntersectClip(const Rect: TRectI): TStatus; overload;
    function IntersectClip(Region: IGdiRegion): TStatus; overload;
    function ExcludeClip(const Rect: TRectF): TStatus; overload;
    function ExcludeClip(const Rect: TRectI): TStatus; overload;
    function ExcludeClip(Region: IGdiRegion): TStatus; overload;
    function ResetClip: TStatus;
    function TranslateClip(DX, DY: Single): TStatus; overload;
    function TranslateClip(DX, DY: Integer): TStatus; overload;
    function GetClip(Region: IGdiRegion): TStatus;
    function GetClipBounds(out Rect: TRectF): TStatus; overload;
    function GetClipBounds(out Rect: TRectI): TStatus; overload;
    function IsClipEmpty: Boolean;
    function GetVisibleClipBounds(out Rect: TRectF): TStatus; overload;
    function GetVisibleClipBounds(out Rect: TRectI): TStatus; overload;
    function IsVisibleClipEmpty: Boolean;
    function IsVisible(X, Y: Integer): Boolean; overload;
    function IsVisible(const Point: TPointI): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Integer): Boolean; overload;
    function IsVisible(const Rect: TRectI): Boolean; overload;
    function IsVisible(X, Y: Single): Boolean; overload;
    function IsVisible(const Point: TPointF): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Single): Boolean; overload;
    function IsVisible(const Rect: TRectF): Boolean; overload;
    function Save: GraphicsState;
    function Restore(gstate: GraphicsState): TStatus;
    function BeginContainer(const DstRect, SrcRect: TRectF; Unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer(const DstRect, SrcRect: TRectI; Unit_: TUnit): GraphicsContainer; overload;
    function BeginContainer: GraphicsContainer; overload;
    function EnDContainer(state: GraphicsContainer): TStatus;
    function AddMetafileComment(data: PByte; SizeData: UInt): TStatus;
    function GetHalftonePalette: HPALETTE;
  end;

{ TAdjustableArrowCap }

  TAdjustableArrowCap = class(TCustomLineCap, IGdiAdjustableArrowCap)
  public
    constructor Create(Height, Width: Single; IsFilled: Boolean = True);
    function SetHeight(Height: Single): TStatus;
    function GetHeight: Single;
    function SetWidth(Width: Single): TStatus;
    function GetWidth: Single;
    function SetMiddleInset(middleInset: Single): TStatus;
    function GetMiddleInset: Single;
    function SetFillState(IsFilled: Boolean): TStatus;
    function IsFilled: Boolean;
  end;

{ TGdiMetafile }

  TGdiMetafile = class(TGdiImage, IGdiMetafile)
  public
    constructor Create(Wmf: HMETAFILE; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader;
      DeleteWmf: Boolean = False); overload;
    constructor Create(Emf: HENHMETAFILE; DeleteEmf: Boolean = False); overload;
    constructor Create(Filename: WideString); overload;
    constructor Create(Filename: WideString; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader); overload;
    constructor Create(Stream: IStream); overload;
    constructor Create(RefDC: HDC; Type_: TEmfType = EmfTypeEmfPlusDual;
      Description: PWChar = nil); overload;
    constructor Create(RefDC: HDC; FrameRect: TRectF;
      FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create(RefDC: HDC; FrameRect: TRectI;
      FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create(Filename: WideString; RefDC: HDC;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create(Filename: WideString; RefDC: HDC; FrameRect: TRectF;
      FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create(Filename: WideString; RefDC: HDC; FrameRect: TRectI;
      FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create(Stream: IStream; RefDC: HDC;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create(Stream: IStream; RefDC: HDC; FrameRect: TRectF;
      FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create(Stream: IStream; RefDC: HDC; FrameRect: TRectI;
      FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
      Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil); overload;
    constructor Create; overload;
    function GetHENHMETAFILE: HENHMETAFILE;
    function PlayRecord(recordType: TEmfPlusRecordType; Flags, dataSize: UInt; data: PByte): TStatus;
    function SetDownLevelRasterizationLimit(MetafileRasterizationLimitDPI: UInt): TStatus;
    function GetDownLevelRasterizationLimit: UInt;
    function EmfToWmfBits(Emf: HENHMETAFILE; cbData16: UInt; pData16: PByte;
      MapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UInt;
  end;

var
  GenericSansSerifFontFamily: IGdiFontFamily = nil;
  GenericSerifFontFamily: IGdiFontFamily = nil;
  GenericMonospaceFontFamily: IGdiFontFamily = nil;
  GenericTypoGraphicStringFormatBuffer: IGdiStringFormat = nil;
  GenericDefaultStringFormatBuffer: IGdiStringFormat = nil;

{ TGdiImageAttributes }

constructor TGdiImageAttributes.Create;
begin
  inherited Create;
  FLastStatus := GdipCreateImageAttributes(FNativeImageAttr);
end;

destructor TGdiImageAttributes.Destroy;
begin
  GdipDisposeImageAttributes(FNativeImageAttr);
  inherited Destroy;
end;

function TGdiImageAttributes.Clone: IGdiImageAttributes;
var
  Clone: GpImageAttributes;
begin
  SetStatus(GdipCloneImageAttributes(FNativeImageAttr, Clone));
  Result := TGdiImageAttributes.Create(Clone, FLastStatus);
end;

function TGdiImageAttributes.SetToIdentity(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesToIdentity(FNativeImageAttr, Type_));
end;

function TGdiImageAttributes.Reset(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdiPresetImageAttributes(FNativeImageAttr, Type_));
end;

function TGdiImageAttributes.SetColorMatrix(const ColorMatrix: TColorMatrix;
  mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr,
    Type_, True, @ColorMatrix, nil, mode));
end;

function TGdiImageAttributes.ClearColorMatrix(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr, Type_,
    False, nil, nil, ColorMatrixFlagsDefault));
end;

function TGdiImageAttributes.SetColorMatrices(const ColorMatrix: TColorMatrix;
  const GrayMatrix: TColorMatrix; Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr, Type_,
    True, @ColorMatrix, @GrayMatrix, Mode));
end;

function TGdiImageAttributes.ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr,
    Type_, False, nil, nil, ColorMatrixFlagsDefault));
end;

function TGdiImageAttributes.SetThreshold(threshold: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(FNativeImageAttr, Type_,
    True, threshold));
end;

function TGdiImageAttributes.ClearThreshold(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(FNativeImageAttr, Type_,
    False, 0));
end;

function TGdiImageAttributes.SetGamma(gamma: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(FNativeImageAttr, Type_, True, gamma));
end;

function TGdiImageAttributes.ClearGamma(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(FNativeImageAttr, Type_, False, 0));
end;

function TGdiImageAttributes.SetNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(FNativeImageAttr, Type_, True));
end;

function TGdiImageAttributes.ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(FNativeImageAttr, Type_, False));
end;

function TGdiImageAttributes.SetColorKey(ColorLow, ColorHigh: TArgb;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(FNativeImageAttr, Type_,
    True, ColorLow, ColorHigh));
end;

function TGdiImageAttributes.ClearColorKey(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(FNativeImageAttr, Type_,
    False, 0, 0));
end;

function TGdiImageAttributes.SetOutputChannel(channelFlags: TColorChannelFlags;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    Type_, True, channelFlags));
end;

function TGdiImageAttributes.ClearOutputChannel(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    Type_, False, ColorChannelFlagsLast));
end;

function TGdiImageAttributes.SetOutputChannelColorProfile(ColorProfileFilename: WideString;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    Type_, True, PWideChar(ColorProfileFilename)));
end;

function TGdiImageAttributes.ClearOutputChannelColorProfile(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    Type_, False, nil));
end;

function TGdiImageAttributes.SetRemapTable(mapSize: cardinal; map: PColorMap;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(FNativeImageAttr, Type_,
    True, mapSize, map));
end;

function TGdiImageAttributes.ClearRemapTable(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(FNativeImageAttr, Type_,
    False, 0, nil));
end;

function TGdiImageAttributes.SetBrushRemapTable(MapSize: cardinal; Map: PColorMap): TStatus;
begin
  Result := SetRemapTable(MapSize, Map, ColorAdjustTypeBrush);
end;

function TGdiImageAttributes.ClearBrushRemapTable: TStatus;
begin
  Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

function TGdiImageAttributes.SetWrapMode(Wrap: TWrapMode; Color: TArgb = aclBlack;
  clamp: Boolean = False): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesWrapMode(FNativeImageAttr, Wrap, Color, clamp));
end;

function TGdiImageAttributes.GetAdjustedPalette(ColorPalette: PColorPalette;
  ColorAdjustType: TColorAdjustType): TStatus;
begin
  Result := SetStatus(GdipGetImageAttributesAdjustedPalette(FNativeImageAttr,
    ColorPalette, ColorAdjustType));
end;

constructor TGdiImageAttributes.Create(ImageAttr: GpImageAttributes; Status: TStatus);
begin
  inherited Create;
  SetNativeImageAttr(ImageAttr);
  FLastStatus := Status;
end;

function TGdiImageAttributes.GetNativeImageAttr: GpImageAttributes;
begin
  Result := FNativeImageAttr;
end;

procedure TGdiImageAttributes.SetNativeImageAttr(ImageAttr: GpImageAttributes);
begin
  Self.FNativeImageAttr := ImageAttr;
end;

constructor TGdiMatrix.Create;
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix(Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TGdiMatrix.Create(M11, M12, M21, M22, DX, DY: Single);
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix2(M11, M12, M21, M22, DX, DY, Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TGdiMatrix.Create(const Rect: TRectF; const PlgPts: TPointF);
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix3(@Rect, @PlgPts, Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TGdiMatrix.Create(const Rect: TRectI; const PlgPts: TPointI);
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix3I(@Rect, @PlgPts, Matrix);
  SetNativeMatrix(Matrix);
end;

destructor TGdiMatrix.Destroy;
begin
  GdipDeleteMatrix(FNativeMatrix);
end;

function TGdiMatrix.Clone: IGdiMatrix;
var
  CloneMatrix: GpMatrix;
begin
  CloneMatrix := nil;
  SetStatus(GdipCloneMatrix(FNativeMatrix, CloneMatrix));
  if (FLastStatus <> Ok) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TGdiMatrix.Create(CloneMatrix) as IGdiMatrix;
end;

function TGdiMatrix.GetElements(const M: TMatrixArray): TStatus;
begin
  Result := SetStatus(GdipGetMatrixElements(FNativeMatrix, @M));
end;

function TGdiMatrix.SetElements(M11, M12, M21, M22, DX, DY: Single): TStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(FNativeMatrix,
    M11, M12, M21, M22, DX, DY));
end;

function TGdiMatrix.OffsetX: Single;
var
  elements: TMatrixArray;
begin
  if (GetElements(elements) = Ok) then
    Result := elements[4]
  else
    Result := 0;
end;

function TGdiMatrix.OffsetY: Single;
var
  elements: TMatrixArray;
begin
  if (GetElements(elements) = Ok) then
    Result := elements[5]
  else
    Result := 0;
end;

function TGdiMatrix.Reset: TStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(FNativeMatrix, 1, 0, 0, 1,
    0, 0));
end;

function TGdiMatrix.Multiply(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyMatrix(FNativeMatrix, Matrix.NativeMatrix, Order));
end;

function TGdiMatrix.Translate(OffsetX, OffsetY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateMatrix(FNativeMatrix, OffsetX, OffsetY, Order));
end;

function TGdiMatrix.Scale(ScaleX, ScaleY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleMatrix(FNativeMatrix, ScaleX, ScaleY, Order));
end;

function TGdiMatrix.Rotate(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateMatrix(FNativeMatrix, Angle, Order));
end;

function TGdiMatrix.RotateAt(Angle: Single; const center: TPointF; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  if (Order = MatrixOrderPrepend) then
  begin
    SetStatus(GdipTranslateMatrix(FNativeMatrix, center.X, center.Y, Order));
    SetStatus(GdipRotateMatrix(FNativeMatrix, Angle, Order));
    Result := SetStatus(GdipTranslateMatrix(FNativeMatrix, -center.X, -center.Y,
      Order));
  end
  else
  begin
    SetStatus(GdipTranslateMatrix(FNativeMatrix, -center.X, -center.Y, Order));
    SetStatus(GdipRotateMatrix(FNativeMatrix, Angle, Order));
    Result := SetStatus(GdipTranslateMatrix(FNativeMatrix, center.X, center.Y,
      Order));
  end;
end;

function TGdiMatrix.Shear(ShearX, ShearY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipShearMatrix(FNativeMatrix, ShearX, ShearY, Order));
end;

function TGdiMatrix.Invert: TStatus;
begin
  Result := SetStatus(GdipInvertMatrix(FNativeMatrix));
end;

function TGdiMatrix.TransformPoints(Pts: PPointF; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPoints(FNativeMatrix, Pts, Count));
end;

function TGdiMatrix.TransformPoints(Pts: PPointI; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPointsI(FNativeMatrix, Pts, Count));
end;

function TGdiMatrix.TransformVectors(Pts: PPointF; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPoints(FNativeMatrix, Pts, Count));
end;

function TGdiMatrix.TransformVectors(Pts: PPointI; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPointsI(FNativeMatrix, Pts, Count));
end;

function TGdiMatrix.IsInvertible: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsMatrixInvertible(FNativeMatrix, B));
  Result := B;
end;

function TGdiMatrix.IsIdentity: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsMatrixIdentity(FNativeMatrix, B));
  Result := B;
end;

function TGdiMatrix.Equals(Matrix: IGdiMatrix): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsMatrixEqual(FNativeMatrix, Matrix.NativeMatrix, B));
  Result := B;
end;

constructor TGdiMatrix.Create(Matrix: GpMatrix);
begin
  inherited Create;
  FLastStatus := Ok;
  SetNativeMatrix(Matrix);
end;

function TGdiMatrix.GetNativeMatrix: GpMatrix;
begin
  Result := FNativeMatrix;
end;

procedure TGdiMatrix.SetNativeMatrix(Matrix: GpMatrix);
begin
  Self.FNativeMatrix := Matrix;
end;

constructor TGdiStringFormat.Create(FormatFlags: Integer = 0; Language: LangId = LANG_NEUTRAL);
begin
  inherited Create;
  FNativeFormat := nil;
  FLastStatus := GdipCreateStringFormat(FormatFlags, Language, FNativeFormat);
end;

class function TGdiStringFormat.GenericDefault: IGdiStringFormat;
var
  F: GpStringFormat;
  S: TStatus;
begin
  if GenericDefaultStringFormatBuffer = nil then
  begin
    GenericDefaultStringFormatBuffer := TGdiStringFormat.Create;
    S := GdipStringFormatGetGenericDefault(F);
    GenericDefaultStringFormatBuffer.NativeFormat := F;
    GenericDefaultStringFormatBuffer.LastStatus := S;
  end;
  Result := GenericDefaultStringFormatBuffer;
end;

class function TGdiStringFormat.GenericTypoGraphic: IGdiStringFormat;
var
  F: GpStringFormat;
  S: TStatus;
begin
  if GenericTypoGraphicStringFormatBuffer = nil then
  begin
    GenericTypoGraphicStringFormatBuffer := TGdiStringFormat.Create;
    S := GdipStringFormatGetGenericTypoGraphic(F);
    GenericDefaultStringFormatBuffer.NativeFormat := F;
    GenericDefaultStringFormatBuffer.LastStatus := S;
  end;
  Result := GenericTypoGraphicStringFormatBuffer;
end;

constructor TGdiStringFormat.Create(Format: IGdiStringFormat);
var
  Gpstf: GpStringFormat;
begin
  inherited Create;
  FNativeFormat := nil;
  if Assigned(Format) then
    Gpstf := Format.NativeFormat
  else
    Gpstf := nil;
  FLastStatus := GdipCloneStringFormat(gpstf, FNativeFormat);
end;

function TGdiStringFormat.Clone: IGdiStringFormat;
var
  ClonedStringFormat: GpStringFormat;
begin
  ClonedStringFormat := nil;
  FLastStatus := GdipCloneStringFormat(FNativeFormat, ClonedStringFormat);
  if FLastStatus = Ok then
    Result := TGdiStringFormat.Create(ClonedStringFormat, FLastStatus)
  else
    Result := nil;
end;

destructor TGdiStringFormat.Destroy;
begin
  GdipDeleteStringFormat(FNativeFormat);
end;

function TGdiStringFormat.SetFormatFlags(Flags: Integer): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatFlags(FNativeFormat, Flags));
end;

function TGdiStringFormat.GetFormatFlags: Integer;
begin
  SetStatus(GdipGetStringFormatFlags(FNativeFormat, Result));
end;

function TGdiStringFormat.SetAlignment(Align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatAlign(FNativeFormat, Align));
end;

function TGdiStringFormat.GetAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatAlign(FNativeFormat, Result));
end;

function TGdiStringFormat.SetLineAlignment(Align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatLineAlign(FNativeFormat, Align));
end;

function TGdiStringFormat.GetLineAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatLineAlign(FNativeFormat, Result));
end;

function TGdiStringFormat.SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatHotkeyPrefix(FNativeFormat, Integer(hotkeyPrefix)));
end;

function TGdiStringFormat.GetHotkeyPrefix: THotkeyPrefix;
var
  HotkeyPrefix: Integer;
begin
  SetStatus(GdipGetStringFormatHotkeyPrefix(FNativeFormat, HotkeyPrefix));
  Result := THotkeyPrefix(HotkeyPrefix);
end;

function TGdiStringFormat.SetTabStops(FirstTabOffset: Single; Count: Integer; TabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTabStops(FNativeFormat, FirstTabOffset, Count, TabStops));
end;

function TGdiStringFormat.GetTabStopCount: Integer;
begin
  SetStatus(GdipGetStringFormatTabStopCount(FNativeFormat, Result));
end;

function TGdiStringFormat.GetTabStops(Count: Integer; FirstTabOffset, TabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipGetStringFormatTabStops(FNativeFormat, Count, FirstTabOffset, TabStops));
end;

function TGdiStringFormat.SetDigitSubstitution(Language: LangId; substitute: TStringDigitSubstitute): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatDigitSubstitution(FNativeFormat, Language, substitute));
end;

function TGdiStringFormat.GetDigitSubstitutionLanguage: LangId;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(FNativeFormat, @Result, nil));
end;

function TGdiStringFormat.GetDigitSubstitutionMethod: TStringDigitSubstitute;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(FNativeFormat, nil, @Result));
end;

function TGdiStringFormat.SetTrimming(Trimming: TStringTrimming): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTrimming(FNativeFormat, Trimming));
end;

function TGdiStringFormat.GetTrimming: TStringTrimming;
begin
  SetStatus(GdipGetStringFormatTrimming(FNativeFormat, Result));
end;

function TGdiStringFormat.SetMeasurableCharacterRanges(rangeCount: Integer;
  ranges: PCharacterRange): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatMeasurableCharacterRanges(FNativeFormat,
    rangeCount, ranges));
end;

function TGdiStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  SetStatus(GdipGetStringFormatMeasurableCharacterRangeCount(FNativeFormat, Result));
end;

function TGdiStringFormat.GetNativeFormat: GpStringFormat;
begin
  Result := FNativeFormat;
end;

procedure TGdiStringFormat.SetNativeFormat(Value: GpStringFormat);
begin
  FNativeFormat := Value;
end;

constructor TGdiStringFormat.Create(ClonedStringFormat: GpStringFormat; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativeFormat := ClonedStringFormat;
end;

constructor TAdjustableArrowCap.Create(Height, Width: Single; IsFilled: Boolean = True);
var
  Cap: GpAdjustableArrowCap;
begin
  inherited Create;
  Cap := nil;
  FLastStatus := GdipCreateAdjustableArrowCap(Height, Width, IsFilled, Cap);
  SetNativeCap(Cap);
end;

function TAdjustableArrowCap.SetHeight(Height: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), Height));
end;

function TAdjustableArrowCap.GetHeight: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapHeight(GpAdjustableArrowCap(FNativeCap), Result));
end;

function TAdjustableArrowCap.SetWidth(Width: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), Width));
end;

function TAdjustableArrowCap.GetWidth: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapWidth(GpAdjustableArrowCap(FNativeCap), Result));
end;

function TAdjustableArrowCap.SetMiddleInset(middleInset: Single): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapMiddleInset(GpAdjustableArrowCap(FNativeCap), middleInset));
end;

function TAdjustableArrowCap.GetMiddleInset: Single;
begin
  SetStatus(GdipGetAdjustableArrowCapMiddleInset(
    GpAdjustableArrowCap(FNativeCap), Result));
end;

function TAdjustableArrowCap.SetFillState(IsFilled: Boolean): TStatus;
begin
  Result := SetStatus(GdipSetAdjustableArrowCapFillState(
    GpAdjustableArrowCap(FNativeCap), IsFilled));
end;

function TAdjustableArrowCap.IsFilled: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipGetAdjustableArrowCapFillState(GpAdjustableArrowCap(FNativeCap), B));
  Result := B;
end;

constructor TGdiMetafile.Create(Wmf: HMETAFILE;
  var wmfPlaceableFileHeader: TWmfPlaceableFileHeader; DeleteWmf: Boolean = False);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromWmf(Wmf, DeleteWmf, @wmfPlaceableFileHeader, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Emf: HENHMETAFILE; DeleteEmf: Boolean = False);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromEmf(Emf, DeleteEmf, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Filename: WideString);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromFile(PWideChar(Filename), Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Filename: WideString; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromWmfFile(PWideChar(Filename), @wmfPlaceableFileHeader, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Stream: IStream);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromStream(Stream, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(RefDC: HDC; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafile(RefDC, Type_, nil, MetafileFrameUnitGdi,
    Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(RefDC: HDC; FrameRect: TRectF;
  FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafile(RefDC, Type_, @FrameRect, FrameUnit,
    Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(RefDC: HDC; FrameRect: TRectI;
  FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi;
  Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafileI(RefDC, Type_, @FrameRect, FrameUnit,
    Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Filename: WideString; RefDC: HDC;
  Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafileFilename(PWideChar(Filename),
    RefDC, Type_, nil, MetafileFrameUnitGdi, Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Filename: WideString; RefDC: HDC; FrameRect: TRectF;
  FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafileFilename(PWideChar(Filename), RefDC,
    Type_, @FrameRect, FrameUnit, Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Filename: WideString; RefDC: HDC; FrameRect: TRectI;
  FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafileFilenameI(PWideChar(Filename),
    RefDC, Type_, @FrameRect, FrameUnit, Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Stream: IStream; RefDC: HDC;
  Type_: TEmfType = EmfTypeEmfPlusDual; Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafileStream(Stream, RefDC, Type_, nil,
    MetafileFrameUnitGdi, Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Stream: IStream; RefDC: HDC; FrameRect: TRectF;
  FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafileStream(Stream, RefDC, Type_, @FrameRect, FrameUnit, Description, Metafile);
  SetNativeImage(Metafile);
end;

constructor TGdiMetafile.Create(Stream: IStream; RefDC: HDC; FrameRect: TRectI;
  FrameUnit: TMetafileFrameUnit = MetafileFrameUnitGdi; Type_: TEmfType = EmfTypeEmfPlusDual;
  Description: PWChar = nil);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipRecordMetafileStreamI(Stream, RefDC, Type_, @FrameRect, FrameUnit, Description, Metafile);
  SetNativeImage(Metafile);
end;

function TGdiMetafile.GetHENHMETAFILE: HENHMETAFILE;
begin
  SetStatus(GdipGetHemfFromMetafile(FNativeImage, Result));
end;

function TGdiMetafile.PlayRecord(recordType: TEmfPlusRecordType; Flags, dataSize: UInt;
  data: PByte): TStatus;
begin
  Result := SetStatus(GdipPlayMetafileRecord(GpMetafile(FNativeImage),
    recordType, Flags, dataSize, data));
end;

function TGdiMetafile.SetDownLevelRasterizationLimit(MetafileRasterizationLimitDPI: UInt): TStatus;
begin
  Result := SetStatus(GdipSetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), MetafileRasterizationLimitDPI));
end;

function TGdiMetafile.GetDownLevelRasterizationLimit: UInt;
var
  MetafileRasterizationLimitDPI: UInt;
begin
  MetafileRasterizationLimitDPI := 0;
  SetStatus(GdipGetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), MetafileRasterizationLimitDPI));
  Result := MetafileRasterizationLimitDPI;
end;

function TGdiMetafile.EmfToWmfBits(Emf: HENHMETAFILE; cbData16: UInt; pData16: PByte;
  MapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UInt;
begin
  Result := GdipEmfToWmfBits(Emf, cbData16, pData16, MapMode, Integer(eFlags));
end;

constructor TGdiMetafile.Create;
begin
  inherited Create;
  SetNativeImage(nil);
  FLastStatus := Ok;
end;

function TGdIGdiRegion.GetNativeRegion: GpRegion;
begin
  Result := FNativeRegion;
end;

constructor TGdIGdiRegion.Create;
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegion(Region);
  SetNativeRegion(Region);
end;

constructor TGdIGdiRegion.Create(Rect: TRectF);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionRect(@Rect, Region);
  SetNativeRegion(Region);
end;

constructor TGdIGdiRegion.Create(Rect: TRectI);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionRectI(@Rect, Region);
  SetNativeRegion(Region);
end;

constructor TGdIGdiRegion.Create(Path: IGdiGraphicsPath);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionPath(Path.NativePath, Region);
  SetNativeRegion(Region);
end;

constructor TGdIGdiRegion.Create(RegionData: PByte; Size: Integer);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionRgnData(RegionData, Size, Region);
  SetNativeRegion(Region);
end;

constructor TGdIGdiRegion.Create(Rgn: HRGN);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionHrgn(Rgn, Region);
  SetNativeRegion(Region);
end;

function TGdIGdiRegion.FromHRGN(Rgn: HRGN): IGdiRegion;
var
  Region: GpRegion;
begin
  Region := nil;
  if GdipCreateRegionHrgn(Rgn, Region) = Ok then
  begin
    Result := TGdIGdiRegion.Create(Region);
    if Result = nil then
      GdipDeleteRegion(Region);
    Exit;
  end
  else
    Result := nil;
end;

destructor TGdIGdiRegion.Destroy;
begin
  GdipDeleteRegion(FNativeRegion);
end;

function TGdIGdiRegion.Clone: IGdiRegion;
var
  Region: GpRegion;
begin
  Region := nil;
  SetStatus(GdipCloneRegion(FNativeRegion, Region));
  Result := TGdIGdiRegion.Create(Region);
end;

function TGdIGdiRegion.MakeInfinite: TStatus;
begin
  Result := SetStatus(GdipSetInfinite(FNativeRegion));
end;

function TGdIGdiRegion.MakeEmpty: TStatus;
begin
  Result := SetStatus(GdipSetEmpty(FNativeRegion));
end;

function TGdIGdiRegion.GetDataSize: UInt;
var
  BufferSize: UInt;
begin
  BufferSize := 0;
  SetStatus(GdipGetRegionDataSize(FNativeRegion, BufferSize));
  Result := BufferSize;
end;

function TGdIGdiRegion.GetData(Buffer: PByte; BufferSize: UInt; SizeFilled: PUInt = nil): TStatus;
begin
  Result := SetStatus(GdipGetRegionData(FNativeRegion, Buffer, BufferSize, SizeFilled));
end;

function TGdIGdiRegion.Intersect(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeIntersect));
end;

function TGdIGdiRegion.Intersect(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeIntersect));
end;

function TGdIGdiRegion.Intersect(Path: IGdiGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath,
    CombineModeIntersect));
end;

function TGdIGdiRegion.Intersect(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion, Region.NativeRegion,
    CombineModeIntersect));
end;

function TGdIGdiRegion.Union(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeUnion));
end;

function TGdIGdiRegion.Union(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeUnion));
end;

function TGdIGdiRegion.Union(Path: IGdiGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath, CombineModeUnion));
end;

function TGdIGdiRegion.Union(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion, Region.NativeRegion,
    CombineModeUnion));
end;

function TGdIGdiRegion.Xor_(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeXor));
end;

function TGdIGdiRegion.Xor_(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeXor));
end;

function TGdIGdiRegion.Xor_(Path: IGdiGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath, CombineModeXor));
end;

function TGdIGdiRegion.Xor_(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion, Region.NativeRegion,
    CombineModeXor));
end;

function TGdIGdiRegion.Exclude(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeExclude));
end;

function TGdIGdiRegion.Exclude(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeExclude));
end;

function TGdIGdiRegion.Exclude(Path: IGdiGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath, CombineModeExclude));
end;

function TGdIGdiRegion.Exclude(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion,
    Region.NativeRegion,
    CombineModeExclude));
end;

function TGdIGdiRegion.Complement(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect,
    CombineModeComplement));
end;

function TGdIGdiRegion.Complement(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect,
    CombineModeComplement));
end;

function TGdIGdiRegion.Complement(Path: IGdiGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion,
    Path.NativePath,
    CombineModeComplement));
end;

function TGdIGdiRegion.Complement(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion,
    Region.NativeRegion,
    CombineModeComplement));
end;

function TGdIGdiRegion.Translate(DX, DY: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateRegion(FNativeRegion, DX, DY));
end;

function TGdIGdiRegion.Translate(DX, DY: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateRegionI(FNativeRegion, DX, DY));
end;

function TGdIGdiRegion.Transform(Matrix: IGdiMatrix): TStatus;
begin
  Result := SetStatus(GdipTransformRegion(FNativeRegion,
    Matrix.NativeMatrix));
end;

function TGdIGdiRegion.GetBounds(out Rect: TRectI; G: IGdiGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBoundsI(FNativeRegion,
    G.NativeGraphics, @Rect));
end;

function TGdIGdiRegion.GetBounds(out Rect: TRectF; G: IGdiGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBounds(FNativeRegion,
    G.NativeGraphics, @Rect));
end;

function TGdIGdiRegion.GetHRGN(G: IGdiGraphics): HRGN;
begin
  SetStatus(GdipGetRegionHRgn(FNativeRegion, G.NativeGraphics, Result));
end;

function TGdIGdiRegion.IsEmpty(G: IGdiGraphics): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsEmptyRegion(FNativeRegion, G.NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsInfinite(G: IGdiGraphics): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsInfiniteRegion(FNativeRegion, G.NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(X, Y: Integer; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionPointI(FNativeRegion, X, Y, NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(const Point: TPointI; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionPointI(FNativeRegion, Point.X, Point.Y,
    NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(X, Y: Single; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionPoint(FNativeRegion, X, Y, NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(const Point: TPointF; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionPoint(FNativeRegion, Point.X, Point.Y,
    NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(X, Y, Width, Height: Integer; G: IGdiGraphics): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionRectI(FNativeRegion, X, Y, Width, Height,
    NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(const Rect: TRectI; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionRectI(FNativeRegion, Rect.X, Rect.Y, Rect.Width,
    Rect.Height, NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(X, Y, Width, Height: Single; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionRect(FNativeRegion, X, Y, Width, Height,
    NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.IsVisible(const Rect: TRectF; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisibleRegionRect(FNativeRegion, Rect.X, Rect.Y, Rect.Width,
    Rect.Height, NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.Equals(Region: IGdiRegion; G: IGdiGraphics): Boolean;
var
  NativeRegion: GpRegion;
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeRegion := nil;
  NativeGraphics := nil;
  if Region <> nil then
    NativeRegion := Region.NativeRegion;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsEqualRegion(FNativeRegion, NativeRegion, NativeGraphics, B));
  Result := B;
end;

function TGdIGdiRegion.GetRegionScansCount(Matrix: IGdiMatrix): UInt;
var
  Count: UInt;
begin
  Count := 0;
  SetStatus(GdipGetRegionScansCount(FNativeRegion, Count, Matrix.NativeMatrix));
  Result := Count;
end;

function TGdIGdiRegion.GetRegionScans(Matrix: IGdiMatrix; Rects: PRectF; out Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScans(FNativeRegion,
    Rects,
    Count,
    Matrix.NativeMatrix));
end;

function TGdIGdiRegion.GetRegionScans(Matrix: IGdiMatrix; Rects: PRectI; out Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScansI(FNativeRegion,
    Rects,
    Count,
    Matrix.NativeMatrix));
end;

constructor TGdIGdiRegion.Create(Region: GpRegion);
begin
  inherited Create;
  SetNativeRegion(Region);
end;

procedure TGdIGdiRegion.SetNativeRegion(Region: GpRegion);
begin
  Self.FNativeRegion := Region;
end;

constructor TCustomLineCap.Create(FillPath, StrokePath: IGdiGraphicsPath;
  BaseCap: TLineCap = LineCapFlat; BaseInset: Single = 0);
var
  NativeFillPath, NativeStrokePath: GpPath;
begin
  inherited Create;
  FNativeCap := nil;
  NativeFillPath := nil;
  NativeStrokePath := nil;
  if FillPath <> nil then
    NativeFillPath := FillPath.NativePath;
  if StrokePath <> nil then
    NativeStrokePath := StrokePath.NativePath;
  FLastStatus := GdipCreateCustomLineCap(NativeFillPath, NativeStrokePath,
    BaseCap, BaseInset, FNativeCap);
end;

destructor TCustomLineCap.Destroy;
begin
  GdipDeleteCustomLineCap(FNativeCap);
end;

function TCustomLineCap.Clone: IGdiCustomLineCap;
var
  newFNativeLineCap: GpCustomLineCap;
begin
  newFNativeLineCap := nil;
  SetStatus(GdipCloneCustomLineCap(FNativeCap, newFNativeLineCap));
  if (FLastStatus = Ok) then
  begin
    Result := TCustomLineCap.Create(newFNativeLineCap, FLastStatus);
    if (Result = nil) then
      SetStatus(GdipDeleteCustomLineCap(newFNativeLineCap));
  end
  else
    Result := nil;
end;

function TCustomLineCap.SetStrokeCap(StrokeCap: TLineCap): TStatus;
begin
  Result := SetStrokeCaps(StrokeCap, StrokeCap);
end;

function TCustomLineCap.SetStrokeCaps(StartCap, EndCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeCaps(FNativeCap, StartCap, EndCap));
end;

function TCustomLineCap.GetStrokeCaps(out StartCap, EndCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipGetCustomLineCapStrokeCaps(FNativeCap, StartCap, EndCap));
end;

function TCustomLineCap.SetStrokeJoin(lineJoin: TLineJoin): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapStrokeJoin(FNativeCap, lineJoin));
end;

function TCustomLineCap.GetStrokeJoin: TLineJoin;
begin
  SetStatus(GdipGetCustomLineCapStrokeJoin(FNativeCap, Result));
end;

function TCustomLineCap.SetBaseCap(BaseCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseCap(FNativeCap, BaseCap));
end;

function TCustomLineCap.GetBaseCap: TLineCap;
begin
  SetStatus(GdipGetCustomLineCapBaseCap(FNativeCap, Result));
end;

function TCustomLineCap.SetBaseInset(Inset: Single): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapBaseInset(FNativeCap, Inset));
end;

function TCustomLineCap.GetBaseInset: Single;
begin
  SetStatus(GdipGetCustomLineCapBaseInset(FNativeCap, Result));
end;

function TCustomLineCap.SetWidthScale(WidthScale: Single): TStatus;
begin
  Result := SetStatus(GdipSetCustomLineCapWidthScale(FNativeCap, WidthScale));
end;

function TCustomLineCap.GetWidthScale: Single;
begin
  SetStatus(GdipGetCustomLineCapWidthScale(FNativeCap, Result));
end;

constructor TCustomLineCap.Create;
begin
  inherited Create;
  FNativeCap := nil;
  FLastStatus := Ok;
end;

constructor TCustomLineCap.Create(Cap: GpCustomLineCap; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativeCap := Cap;
end;

function TCustomLineCap.GetNativeCap: GpCustomLineCap;
begin
  Result := FNativeCap;
end;

procedure TCustomLineCap.SetNativeCap(Cap: GpCustomLineCap);
begin
  FNativeCap := Cap;
end;

constructor TGdiCachedBitmap.Create(Bitmap: IGdiBitmap; Graphics: IGdiGraphics);
begin
  inherited Create;
  FNativeCachedBitmap := nil;
  FLastStatus := GdipCreateCachedBitmap(
    GpBitmap(Bitmap.NativeImage),
    Graphics.NativeGraphics,
    FNativeCachedBitmap);
end;

destructor TGdiCachedBitmap.Destroy;
begin
  GdipDeleteCachedBitmap(FNativeCachedBitmap);
end;

function TGdiCachedBitmap.GetNativeCachedBitmap: GpCachedBitmap;
begin
  Result := FNativeCachedBitmap;
end;

{ TGdiPen }

constructor TGdiPen.Create(Pen: GpPen; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativePen := Pen;
end;

constructor TGdiPen.Create(Color: TArgb; Width: Single = 1);
var
  Unit_: TUnit;
begin
  inherited Create;
  Unit_ := UnitWorld;
  FNativePen := nil;
  FLastStatus := GdipCreatePen1(Color, Width, Unit_, FNativePen);
end;

constructor TGdiPen.Create(Brush: IGdiBrush; Width: Single = 1);
var
  Unit_: TUnit;
begin
  inherited Create;
  Unit_ := UnitWorld;
  FNativePen := nil;
  FLastStatus := GdipCreatePen2(Brush.NativeBrush, Width, Unit_, FNativePen);
end;

destructor TGdiPen.Destroy;
begin
  GdipDeletePen(FNativePen);
  inherited Destroy;
end;

function TGdiPen.GetNativePen: GpPen;
begin
  Result := FNativePen;
end;

procedure TGdiPen.SetNativePen(Pen: GpPen);
begin
  Self.FNativePen := Pen;
end;

function TGdiPen.Clone: IGdiPen;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  FLastStatus := GdipClonePen(FNativePen, NativePen);
  Result := TGdiPen.Create(NativePen, FLastStatus);
end;

procedure TGdiPen.SetWidth(Width: Single);
begin
  SetStatus(GdipSetPenWidth(FNativePen, Width));
end;

function TGdiPen.GetWidth: Single;
begin
  SetStatus(GdipGetPenWidth(FNativePen, Result));
end;

function TGdiPen.SetLineCap(StartCap, EndCap: TLineCap; DashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenLineCap197819(FNativePen, StartCap, EndCap, DashCap));
end;

function TGdiPen.SetStartCap(StartCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenStartCap(FNativePen, StartCap));
end;

function TGdiPen.SetEndCap(EndCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenEndCap(FNativePen, EndCap));
end;

function TGdiPen.SetDashCap(DashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenDashCap197819(FNativePen, DashCap));
end;

function TGdiPen.GetStartCap: TLineCap;
begin
  SetStatus(GdipGetPenStartCap(FNativePen, Result));
end;

function TGdiPen.GetEndCap: TLineCap;
begin
  SetStatus(GdipGetPenEndCap(FNativePen, Result));
end;

function TGdiPen.GetDashCap: TDashCap;
begin
  SetStatus(GdipGetPenDashCap197819(FNativePen, Result));
end;

function TGdiPen.SetLineJoin(lineJoin: TLineJoin): TStatus;
begin
  Result := SetStatus(GdipSetPenLineJoin(FNativePen, lineJoin));
end;

function TGdiPen.GetLineJoin: TLineJoin;
begin
  SetStatus(GdipGetPenLineJoin(FNativePen, Result));
end;

function TGdiPen.SetCustomStartCap(CustomCap: IGdiCustomLineCap): TStatus;
var
  NativeCap: GpCustomLineCap;
begin
  NativeCap := nil;
  if CustomCap <> nil then
    NativeCap := CustomCap.NativeCap;
  Result := SetStatus(GdipSetPenCustomStartCap(FNativePen, NativeCap));
end;

function TGdiPen.GetCustomStartCap(CustomCap: IGdiCustomLineCap): TStatus;
var
  NativeCap: GpCustomLineCap;
begin
  if CustomCap = nil then
    Result := SetStatus(InvalidParameter)
  else
  begin
    NativeCap := CustomCap.NativeCap;
    Result := SetStatus(GdipGetPenCustomStartCap(FNativePen, NativeCap));
    CustomCap.NativeCap := NativeCap;
  end;
end;

function TGdiPen.SetCustomEndCap(CustomCap: IGdiCustomLineCap): TStatus;
var
  NativeCap: GpCustomLineCap;
begin
  NativeCap := nil;
  if CustomCap <> nil then
    NativeCap := CustomCap.NativeCap;
  Result := SetStatus(GdipSetPenCustomEndCap(FNativePen, NativeCap));
end;

function TGdiPen.GetCustomEndCap(CustomCap: IGdiCustomLineCap): TStatus;
var
  Cap: GpCustomLineCap;
begin
  if CustomCap = nil then
    Result := SetStatus(InvalidParameter)
  else
  begin
    Cap := CustomCap.NativeCap;
    Result := SetStatus(GdipGetPenCustomEndCap(FNativePen, Cap));
    CustomCap.NativeCap := Cap;
  end;
end;

function TGdiPen.SetMiterLimit(MiterLimit: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenMiterLimit(FNativePen, MiterLimit));
end;

function TGdiPen.GetMiterLimit: Single;
begin
  SetStatus(GdipGetPenMiterLimit(FNativePen, Result));
end;

function TGdiPen.SetAlignment(penAlignment: TPenAlignment): TStatus;
begin
  Result := SetStatus(GdipSetPenMode(FNativePen, penAlignment));
end;

function TGdiPen.GetAlignment: TPenAlignment;
begin
  SetStatus(GdipGetPenMode(FNativePen, Result));
end;

function TGdiPen.SetTransform(Matrix: IGdiMatrix): TStatus;
begin
  Result := SetStatus(GdipSetPenTransform(FNativePen, Matrix.NativeMatrix));
end;

function TGdiPen.GetTransform(Matrix: IGdiMatrix): TStatus;
begin
  Result := SetStatus(GdipGetPenTransform(FNativePen, Matrix.NativeMatrix));
end;

function TGdiPen.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetPenTransform(FNativePen));
end;

function TGdiPen.MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPenTransform(FNativePen, Matrix.NativeMatrix, Order));
end;

function TGdiPen.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePenTransform(FNativePen, DX, DY, Order));
end;

function TGdiPen.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePenTransform(FNativePen, SX, SY, Order));
end;

function TGdiPen.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePenTransform(FNativePen, Angle, Order));
end;

function TGdiPen.GetPenType: TPenType;
begin
  SetStatus(GdipGetPenFillType(FNativePen, Result));
end;

function TGdiPen.SetColor(Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipSetPenColor(FNativePen, Color));
end;

function TGdiPen.SetBrush(Brush: IGdiBrush): TStatus;
begin
  Result := SetStatus(GdipSetPenBrushFill(FNativePen, Brush.NativeBrush));
end;

function TGdiPen.GetColor(out Color: TArgb): TStatus;
var
  PenType: TPenType;
  ARGB: DWORD;
begin
  PenType := GetPenType;
  if PenType <> PenTypeSoliDColor then
  begin
    Result := WrongState;
    Exit;
  end;
  SetStatus(GdipGetPenColor(FNativePen, ARGB));
  if FLastStatus = Ok then
    Color := ARGB;
  Result := FLastStatus;
end;

function TGdiPen.GetBrush: IGdiBrush;
var
  PenType: TPenType;
  Brush: IGdiBrush;
  FNativeBrush: GpBrush;
begin
  PenType := GetPenType;
  Brush := nil;
  case PenType of
    PenTypeSoliDColor: Brush := TGdiSolidBrush.Create;
    PenTypeHatchFill: Brush := TGdiHatchBrush.Create;
    PenTypeTextureFill: Brush := TTextureBrush.Create;
    PenTypePathGradient: Brush := TGdiBrush.Create;
    PenTypeLinearGradient: Brush := TGdiLinearGradientBrush.Create;
  end;
  if Brush <> nil then
  begin
    SetStatus(GdipGetPenBrushFill(FNativePen, FNativeBrush));
    Brush.SetNativeBrush(FNativeBrush);
  end;
  Result := Brush;
end;

function TGdiPen.GetDashStyle: TDashStyle;
begin
  SetStatus(GdipGetPenDashStyle(FNativePen, Result));
end;

function TGdiPen.SetDashStyle(DashStyle: TDashStyle): TStatus;
begin
  Result := SetStatus(GdipSetPenDashStyle(FNativePen, DashStyle));
end;

function TGdiPen.GetDashOffset: Single;
begin
  SetStatus(GdipGetPenDashOffset(FNativePen, Result));
end;

function TGdiPen.SetDashOffset(DashOffset: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenDashOffset(FNativePen, DashOffset));
end;

function TGdiPen.SetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenDashArray(FNativePen, DashArray, Count));
end;

function TGdiPen.GetDashPatternCount: Integer;
begin
  SetStatus(GdipGetPenDashCount(FNativePen, Result));
end;

function TGdiPen.GetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPenDashArray(FNativePen, DashArray, Count));
end;

function TGdiPen.SetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenCompoundArray(FNativePen, CompoundArray, Count));
end;

function TGdiPen.GetCompoundArrayCount: Integer;
begin
  SetStatus(GdipGetPenCompoundCount(FNativePen, Result));
end;

function TGdiPen.GetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
begin
  if Count <= 0 then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCompoundArray(FNativePen, CompoundArray, Count));
end;

{ TGdiBrush }

constructor TGdiBrush.Create;
begin
  inherited Create;
end;

constructor TGdiBrush.Create(Brush: GpBrush; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativeBrush := Brush;
end;

destructor TGdiBrush.Destroy;
begin
  GdipDeleteBrush(FNativeBrush);
  inherited Destroy;
end;

function TGdiBrush.GetNativeBrush: GpBrush;
begin
  Result := Self.FNativeBrush;
end;

procedure TGdiBrush.SetNativeBrush(Brush: GpBrush);
begin
  Self.FNativeBrush := Brush;
end;

function TGdiBrush.Clone: IGdiBrush;
var
  NativeBrush: GpBrush;
  NewBrush: IGdiBrush;
begin
  NativeBrush := nil;
  SetStatus(GdipCloneBrush(FNativeBrush, NativeBrush));
  NewBrush := TGdiBrush.Create(NativeBrush, FLastStatus);
  if NewBrush = nil then
    GdipDeleteBrush(NativeBrush);
  Result := NewBrush;
end;

function TGdiBrush.GetType: TBrushType;
begin
  Result := TBrushType(-1);
  SetStatus(GdipGetBrushType(FNativeBrush, Result));
end;

{ TGdiSolidBrush }

constructor TGdiSolidBrush.Create;
begin
  inherited Create;
end;

constructor TGdiSolidBrush.Create(Color: TArgb);
var
  Brush: GpSolidFill;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateSolidFill(Color, Brush);
  SetNativeBrush(Brush);
end;

function TGdiSolidBrush.GetColor(out Color: TArgb): TStatus;
begin
  SetStatus(GdipGetSolidFillColor(GPSOLIDFill(FNativeBrush), Color));
  Result := FLastStatus;
end;

function TGdiSolidBrush.SetColor(Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipSetSolidFillColor(GpSolidFill(FNativeBrush),
    Color));
end;

{ TTextureBrush }

constructor TTextureBrush.Create(Image: IGdiImage; WrapMode: TWrapMode = WrapModeTile);
var
  Texture: GpTexture;
begin
  inherited Create;
  FLastStatus := GdipCreateTexture(Image.NativeImage, WrapMode, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IGdiImage; WrapMode: TWrapMode; DstRect: TRectF);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2(Image.NativeImage, WrapMode, DstRect.X,
    DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IGdiImage; DstRect: TRectF; ImageAttributes: IGdiImageAttributes = nil);
var
  Texture: GpTexture;
  ImgAtt: GpImageAttributes;
begin
  inherited Create;
  Texture := nil;
  if Assigned(ImageAttributes) then
    ImgAtt := ImageAttributes.NativeImageAttr
  else
    ImgAtt := nil;
  FLastStatus := GdipCreateTextureIA(Image.NativeImage, ImgAtt, DstRect.X,
    DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IGdiImage; DstRect: TRectI; ImageAttributes: IGdiImageAttributes = nil);
var
  Texture: GpTexture;
  ImgAtt: GpImageAttributes;
begin
  inherited Create;
  Texture := nil;
  if Assigned(ImageAttributes) then
    ImgAtt := ImageAttributes.NativeImageAttr
  else
    ImgAtt := nil;
  FLastStatus := GdipCreateTextureIAI(Image.NativeImage, ImgAtt, DstRect.X,
    DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IGdiImage; WrapMode: TWrapMode; DstRect: TRectI);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2I(Image.NativeImage, WrapMode, DstRect.X,
    DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IGdiImage; WrapMode: TWrapMode; DstX, DstY, DstWidth, DstHeight: Single);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2(Image.NativeImage, WrapMode, DstX, DstY,
    DstWidth, DstHeight, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IGdiImage; WrapMode: TWrapMode; DstX, DstY, DstWidth, DstHeight: Integer);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2I(Image.NativeImage, WrapMode, DstX, DstY,
    DstWidth, DstHeight, Texture);
  SetNativeBrush(Texture);
end;

function TTextureBrush.SetTransform(Matrix: IGdiMatrix): TStatus;
begin
  Result := SetStatus(GdipSetTextureTransform(GpTexture(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TTextureBrush.GetTransform(Matrix: IGdiMatrix): TStatus;
begin
  Result := SetStatus(GdipGetTextureTransform(GpTexture(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TTextureBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetTextureTransform(GpTexture(FNativeBrush)));
end;

function TTextureBrush.MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyTextureTransform(GpTexture(FNativeBrush),
    Matrix.NativeMatrix, Order));
end;

function TTextureBrush.TranslateTransform(DX, DY: Single; Order: MatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateTextureTransform(GpTexture(FNativeBrush),
    DX, DY, Order));
end;

function TTextureBrush.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleTextureTransform(GpTexture(FNativeBrush),
    SX, SY, Order));
end;

function TTextureBrush.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateTextureTransform(GpTexture(FNativeBrush),
    Angle, Order));
end;

function TTextureBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetTextureWrapMode(GpTexture(FNativeBrush), WrapMode));
end;

function TTextureBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetTextureWrapMode(GpTexture(FNativeBrush), Result));
end;

function TTextureBrush.GetImage: IGdiImage;
var
  Image: GpImage;
begin
  SetStatus(GdipGetTextureImage(GpTexture(FNativeBrush), Image));
  Result := TGdiImage.Create(Image, FLastStatus);
  if (Result = nil) then
    GdipDisposeImage(Image);
end;

constructor TTextureBrush.Create;
begin
  inherited Create;
end;

{ TGdiLinearGradientBrush }

constructor TGdiLinearGradientBrush.Create(const Point1, Point2: TPointF; Color1, Color2: TArgb);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrush(@Point1, @Point2, Color1, Color2, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TGdiLinearGradientBrush.Create(const Point1, Point2: TPointI; Color1, Color2: TArgb);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushI(@Point1, @Point2, Color1,
    Color2, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TGdiLinearGradientBrush.Create(Rect: TRectF; Color1, Color2: TArgb; mode: TLinearGradientMode);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRect(@Rect, Color1,
    Color2, mode, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TGdiLinearGradientBrush.Create(Rect: TRectI; Color1, Color2: TArgb; mode: TLinearGradientMode);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRectI(@Rect, Color1,
    Color2, mode, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TGdiLinearGradientBrush.Create(Rect: TRectF; Color1, Color2: TArgb; Angle: Single; isAngleScalable: Boolean = False);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRectWithAngle(@Rect, Color1,
    Color2, Angle, isAngleScalable, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TGdiLinearGradientBrush.Create(Rect: TRectI; Color1, Color2: TArgb; Angle: Single; isAngleScalable: Boolean = False);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRectWithAngleI(@Rect, Color1,
    Color2, Angle, isAngleScalable, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

function TGdiLinearGradientBrush.SetLinearColors(Color1, Color2: TArgb): TStatus;
begin
  Result := SetStatus(GdipSetLineColors(GpLineGradient(FNativeBrush),
    Color1, Color2));
end;

function TGdiLinearGradientBrush.GetLinearColors(out Color1, Color2: TArgb): TStatus;
var
  Colors: array[0..1] of TArgb;
begin
  SetStatus(GdipGetLineColors(GpLineGradient(FNativeBrush), @Colors));
  if (FLastStatus = Ok) then
  begin
    Color1 := Colors[0];
    Color2 := Colors[1];
  end;
  Result := FLastStatus;
end;

function TGdiLinearGradientBrush.GetRectangle(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetLineRect(GpLineGradient(FNativeBrush), @Rect));
end;

function TGdiLinearGradientBrush.GetRectangle(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetLineRectI(GpLineGradient(FNativeBrush), @Rect));
end;

function TGdiLinearGradientBrush.SetGammaCorrection(UseGammaCorrection: Boolean): TStatus;
begin
  Result := SetStatus(GdipSetLineGammaCorrection(GpLineGradient(FNativeBrush),
    UseGammaCorrection));
end;

function TGdiLinearGradientBrush.GetGammaCorrection: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipGetLineGammaCorrection(GpLineGradient(FNativeBrush), B));
  Result := B;
end;

function TGdiLinearGradientBrush.GetBlendCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetLineBlendCount(GpLineGradient(FNativeBrush), Count));
  Result := Count;
end;

function TGdiLinearGradientBrush.SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetLineBlend(GpLineGradient(FNativeBrush),
    BlendFactors, BlendPositions, Count));
end;

function TGdiLinearGradientBrush.GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  if ((Count <= 0) or (BlendFactors = nil) or (BlendPositions = nil)) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLineBlend(GpLineGradient(FNativeBrush), BlendFactors,
      BlendPositions, Count));
end;

function TGdiLinearGradientBrush.GetInterpolationColorCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetLinePresetBlendCount(GpLineGradient(FNativeBrush), Count));
  Result := Count;
end;

function TGdiLinearGradientBrush.SetInterpolationColors(PresetColors: PArgb;
  BlendPositions: PSingle; Count: Integer): TStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipSetLinePresetBlend(GpLineGradient(FNativeBrush),
      PArgb(PresetColors), BlendPositions, Count));
end;

function TGdiLinearGradientBrush.GetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle; Count: Integer): TStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLinePresetBlend(GpLineGradient(FNativeBrush),
      PArgb(PresetColors), BlendPositions, Count));
end;

function TGdiLinearGradientBrush.SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetLineSigmaBlend(GpLineGradient(FNativeBrush), Focus, Scale));
end;

function TGdiLinearGradientBrush.SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetLineLinearBlend(GpLineGradient(FNativeBrush), Focus, Scale));
end;

function TGdiLinearGradientBrush.SetTransform(Matrix: IGdiMatrix): TStatus;
begin
  Result := SetStatus(GdipSetLineTransform(GpLineGradient(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TGdiLinearGradientBrush.GetTransform(Matrix: IGdiMatrix): TStatus;
begin
  Result := SetStatus(GdipGetLineTransform(GpLineGradient(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TGdiLinearGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetLineTransform(GpLineGradient(FNativeBrush)));
end;

function TGdiLinearGradientBrush.MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyLineTransform(GpLineGradient(FNativeBrush),
    Matrix.NativeMatrix,
    Order));
end;

function TGdiLinearGradientBrush.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateLineTransform(GpLineGradient(FNativeBrush),
    DX, DY, Order));
end;

function TGdiLinearGradientBrush.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleLineTransform(GpLineGradient(FNativeBrush),
    SX, SY, Order));
end;

function TGdiLinearGradientBrush.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateLineTransform(GpLineGradient(FNativeBrush),
    Angle, Order));
end;

function TGdiLinearGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetLineWrapMode(GpLineGradient(FNativeBrush), WrapMode));
end;

function TGdiLinearGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetLineWrapMode(GpLineGradient(FNativeBrush), Result));
end;

{ TGdiHatchBrush }

constructor TGdiHatchBrush.Create(HatchStyle: THatchStyle; ForeColor: TArgb; BackColor: TArgb = aclBlack);
var
  Brush: GpHatch;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateHatchBrush(Integer(HatchStyle), ForeColor, BackColor, Brush);
  SetNativeBrush(Brush);
end;

function TGdiHatchBrush.GetHatchStyle: THatchStyle;
begin
  SetStatus(GdipGetHatchStyle(GpHatch(FNativeBrush), Result));
end;

function TGdiHatchBrush.GetForegrounDColor(out Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGetHatchForegrounDColor(GpHatch(FNativeBrush), Color));
end;

function TGdiHatchBrush.GetBackgrounDColor(out Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGetHatchBackgrounDColor(GpHatch(FNativeBrush), Color));
end;

constructor TGdiHatchBrush.Create;
begin
  inherited Create;
end;

constructor TGdiImage.Create(Filename: WideString;
  UseEmbeddedColorManagement: Boolean = False);
begin
  inherited Create;
  FNativeImage := nil;
  if (UseEmbeddedColorManagement) then
    FLastStatus := GdipLoadImageFromFileICM(
      PWideChar(Filename),
      FNativeImage)
  else
    FLastStatus := GdipLoadImageFromFile(
      PWideChar(Filename),
      FNativeImage);
end;

constructor TGdiImage.Create(Stream: IStream;
  UseEmbeddedColorManagement: Boolean = False);
begin
  inherited Create;
  FNativeImage := nil;
  if (UseEmbeddedColorManagement) then
    FLastStatus := GdipLoadImageFromStreamICM(Stream, FNativeImage)
  else
    FLastStatus := GdipLoadImageFromStream(Stream, FNativeImage);
end;

function TGdiImage.FromFile(Filename: WideString;
  UseEmbeddedColorManagement: Boolean = False): IGdiImage;
begin
  Result := TGdiImage.Create(
    PWideChar(Filename),
    UseEmbeddedColorManagement);
end;

function TGdiImage.FromStream(Stream: IStream;
  UseEmbeddedColorManagement: Boolean = False): IGdiImage;
begin
  Result := TGdiImage.Create(Stream, UseEmbeddedColorManagement);
end;

destructor TGdiImage.Destroy;
begin
  GdipDisposeImage(FNativeImage);
  inherited Destroy;
end;

function TGdiImage.Clone: IGdiImage;
var
  CloneImage: GpImage;
begin
  CloneImage := nil;
  SetStatus(GdipCloneImage(FNativeImage, CloneImage));
  Result := TGdiImage.Create(CloneImage, FLastStatus);
end;

function TGdiImage.Save(Filename: WideString; const clsidEncoder: TGUID;
  EncoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToFile(FNativeImage,
    PWideChar(Filename), @clsidEncoder,
    EncoderParams));
end;

function TGdiImage.Save(Stream: IStream; const clsidEncoder: TGUID;
  EncoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToStream(FNativeImage,
    Stream, @clsidEncoder,
    EncoderParams));
end;

function TGdiImage.SaveAdd(EncoderParams: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipSaveAdd(FNativeImage,
    EncoderParams));
end;

function TGdiImage.SaveAdd(NewImage: IGdiImage;
  EncoderParams: PEncoderParameters): TStatus;
begin
  if (NewImage = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipSaveAddImage(FNativeImage,
    NewImage.NativeImage,
    EncoderParams));
end;

function TGdiImage.GetType: TImageType;
begin
  SetStatus(GdipGetImageType(FNativeImage, Result));
end;

function TGdiImage.GetPhysicalDimension(out Size: TSizeF): TStatus;
var
  Width, Height: Single;
  Status: TStatus;
begin
  Status := SetStatus(GdipGetImageDimension(FNativeImage, Width, Height));
  Size.Width := Width;
  Size.Height := Height;
  Result := Status;
end;

function TGdiImage.GetBounds(out SrcRect: TRectF; out SrcUnit: TUnit): TStatus;
begin
  Result := SetStatus(GdipGetImageBounds(FNativeImage, @SrcRect, SrcUnit));
end;

function TGdiImage.GetWidth: UInt;
var
  Width: UInt;
begin
  Width := 0;
  SetStatus(GdipGetImageWidth(FNativeImage, Width));
  Result := Width;
end;

function TGdiImage.GetHeight: UInt;
var
  Height: UInt;
begin
  Height := 0;
  SetStatus(GdipGetImageHeight(FNativeImage, Height));
  Result := Height;
end;

function TGdiImage.GetHorizontalResolution: Single;
var
  resolution: Single;
begin
  resolution := 0;
  SetStatus(GdipGetImageHorizontalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TGdiImage.GetVerticalResolution: Single;
var
  resolution: Single;
begin
  resolution := 0;
  SetStatus(GdipGetImageVerticalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TGdiImage.GetFlags: UInt;
var
  Flags: UInt;
begin
  Flags := 0;
  SetStatus(GdipGetImageFlags(FNativeImage, Flags));
  Result := Flags;
end;

function TGdiImage.GetRawFormat(out Format: TGUID): TStatus;
begin
  Result := SetStatus(GdipGetImageRawFormat(FNativeImage, @Format));
end;

function TGdiImage.GetPixelFormat: TPixelFormat;
begin
  SetStatus(GdipGetImagePixelFormat(FNativeImage, Result));
end;

function TGdiImage.GetPaletteSize: Integer;
var
  Size: Integer;
begin
  Size := 0;
  SetStatus(GdipGetImagePaletteSize(FNativeImage, Size));
  Result := Size;
end;

function TGdiImage.GetPalette(palette: PColorPalette; Size: Integer): TStatus;
begin
  Result := SetStatus(GdipGetImagePalette(FNativeImage, palette, Size));
end;

function TGdiImage.SetPalette(palette: PColorPalette): TStatus;
begin
  Result := SetStatus(GdipSetImagePalette(FNativeImage, palette));
end;

function TGdiImage.GetThumbnailImage(ThumbWidth, ThumbHeight: UInt;
  Callback: GetThumbnailImageAbort = nil;
  CallbackData: Pointer = nil): IGdiImage;
var
  ThumbImage: GpImage;
  NewImage: IGdiImage;
begin
  ThumbImage := nil;
  SetStatus(GdipGetImageThumbnail(FNativeImage,
    ThumbWidth, ThumbHeight,
    ThumbImage,
    Callback, CallbackData));
  NewImage := TGdiImage.Create(ThumbImage, FLastStatus);
  if (NewImage = nil) then
    GdipDisposeImage(ThumbImage);
  Result := NewImage;
end;

function TGdiImage.GetFrameDimensionsCount: UInt;
var
  Count: UInt;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameDimensionsCount(FNativeImage, Count));
  Result := Count;
end;

function TGdiImage.GetFrameDimensionsList(DimensionIDs: PGUID; Count: UInt): TStatus;
begin
  Result := SetStatus(GdipImageGetFrameDimensionsList(FNativeImage, DimensionIDs, Count));
end;

function TGdiImage.GetFrameCount(const DimensionID: TGUID): UInt;
var
  Count: UInt;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameCount(FNativeImage, @DimensionID, Count));
  Result := Count;
end;

function TGdiImage.SelectActiveFrame(const DimensionID: TGUID; FrameIndex: UInt): TStatus;
begin
  Result := SetStatus(GdipImageSelectActiveFrame(FNativeImage, @DimensionID,
    FrameIndex));
end;

function TGdiImage.RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
begin
  Result := SetStatus(GdipImageRotateFlip(FNativeImage,
    rotateFlipType));
end;

function TGdiImage.GetPropertyCount: UInt;
var
  NumProperty: UInt;
begin
  NumProperty := 0;
  SetStatus(GdipGetPropertyCount(FNativeImage, NumProperty));
  Result := NumProperty;
end;

function TGdiImage.GetPropertyIdList(numOfProperty: UInt; list: PPropID): TStatus;
begin
  Result := SetStatus(GdipGetPropertyIdList(FNativeImage, numOfProperty, list));
end;

function TGdiImage.GetPropertyItemSize(PropId: PropID): UInt;
var
  Size: UInt;
begin
  Size := 0;
  SetStatus(GdipGetPropertyItemSize(FNativeImage, PropId, Size));
  Result := Size;
end;

function TGdiImage.GetPropertyItem(PropId: PropID; PropSize: UInt;
  Buffer: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetPropertyItem(FNativeImage,
    PropId, PropSize, Buffer));
end;

function TGdiImage.GetPropertySize(out TotalBufferSize, NumProperties: UInt): TStatus;
begin
  Result := SetStatus(GdipGetPropertySize(FNativeImage,
    TotalBufferSize, NumProperties));
end;

function TGdiImage.GetAllPropertyItems(TotalBufferSize, NumProperties: UInt;
  AllItems: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetAllPropertyItems(FNativeImage,
    TotalBufferSize, NumProperties, AllItems));
end;

function TGdiImage.RemovePropertyItem(PropId: TPropID): TStatus;
begin
  Result := SetStatus(GdipRemovePropertyItem(FNativeImage, PropId));
end;

function TGdiImage.SetPropertyItem(const item: TPropertyItem): TStatus;
begin
  Result := SetStatus(GdipSetPropertyItem(FNativeImage, @item));
end;

function TGdiImage.GetEncoderParameterListSize(const clsidEncoder: TGUID): UInt;
var
  Size: UInt;
begin
  Size := 0;
  SetStatus(GdipGetEncoderParameterListSize(FNativeImage, @clsidEncoder, Size));
  Result := Size;
end;

function TGdiImage.GetEncoderParameterList(const clsidEncoder: TGUID; Size: UInt;
  Buffer: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipGetEncoderParameterList(FNativeImage, @clsidEncoder,
    Size,
    Buffer));
end;

constructor TGdiImage.Create;
begin
  inherited Create;
end;

constructor TGdiImage.Create(Image: GpImage; Status: TStatus);
begin
  inherited Create;
  FNativeImage := Image;
  FLastStatus := Status;
end;

function TGdiImage.GetNativeImage: GpImage;
begin
  Result := FNativeImage;
end;

procedure TGdiImage.SetNativeImage(Image: GpImage);
begin
  FNativeImage := Image;
end;

{ TGdiBitmap }

constructor TGdiBitmap.Create(Filename: WideString; UseEmbeddedColorManagement: Boolean = False);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  if UseEmbeddedColorManagement then
    FLastStatus := GdipCreateBitmapFromFileICM(PWideChar(Filename), Bitmap)
  else
    FLastStatus := GdipCreateBitmapFromFile(PWideChar(Filename), Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TGdiBitmap.Create(Stream: IStream; UseEmbeddedColorManagement: Boolean = False);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  if UseEmbeddedColorManagement then
    FLastStatus := GdipCreateBitmapFromStreamICM(Stream, Bitmap)
  else
    FLastStatus := GdipCreateBitmapFromStream(Stream, Bitmap);
  SetNativeImage(Bitmap);
end;

function TGdiBitmap.FromFile(Filename: WideString; UseEmbeddedColorManagement: Boolean = False): IGdiImage;
begin
  Result := TGdiBitmap.Create(PWideChar(Filename),UseEmbeddedColorManagement);
end;

function TGdiBitmap.FromStream(Stream: IStream; UseEmbeddedColorManagement: Boolean = False): IGdiImage;
begin
  Result := TGdiBitmap.Create(Stream,UseEmbeddedColorManagement);
end;

constructor TGdiBitmap.Create(Width, Height, Stride: Integer; Format: TPixelFormat; scan0: PByte);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromScan0(Width, Height, Stride, Format, scan0, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TGdiBitmap.Create(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppArgb);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromScan0(Width, Height, 0, Format, nil, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TGdiBitmap.Create(Width, Height: Integer; Target: IGdiGraphics);
var
  Bitmap: GpBitmap;
  NativeGraphics: GpGraphics;
begin
  inherited Create;
  Bitmap := nil;
  NativeGraphics := nil;
  if Target <> nil then
    NativeGraphics := Target.NativeGraphics;
  FLastStatus := GdipCreateBitmapFromGraphics(Width, Height, NativeGraphics, Bitmap);
  SetNativeImage(Bitmap);
end;

function TGdiBitmap.Clone(Rect: TRectI; Format: TPixelFormat): IGdiBitmap;
begin
  Result := Clone(Rect.X, Rect.Y, Rect.Width, Rect.Height, Format);
end;

function TGdiBitmap.Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IGdiBitmap;
var
  Bitmap: IGdiBitmap;
  DstBitmap: GpBitmap;
begin
  DstBitmap := nil;
  FLastStatus := GdipCloneBitmapAreaI(X, Y, Width, Height, Format,
    FNativeImage, DstBitmap);
  if FLastStatus = Ok then
  begin
    Bitmap := TGdiBitmap.Create(DstBitmap);
    if Bitmap = nil then
      GdipDisposeImage(DstBitmap);
    Result := Bitmap;
    Exit;
  end
  else
    Result := nil;
end;

function TGdiBitmap.Clone(Rect: TRectF; Format: TPixelFormat): IGdiBitmap;
begin
  Result := Clone(Rect.X, Rect.Y, Rect.Width, Rect.Height, Format);
end;

function TGdiBitmap.Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IGdiBitmap;
var
  Bitmap: IGdiBitmap;
  DstBitmap: GpBitmap;
begin
  DstBitmap := nil;
  SetStatus(GdipCloneBitmapArea(X, Y, Width, Height, Format,
    FNativeImage, DstBitmap));
  if FLastStatus = Ok then
  begin
    Bitmap := TGdiBitmap.Create(DstBitmap);
    if Bitmap = nil then
      GdipDisposeImage(DstBitmap);
    Result := Bitmap;
  end
  else
    Result := nil;
end;

function TGdiBitmap.LockBits(Rect: TRectI; Flags: UInt; Format: TPixelFormat;
  out LockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapLockBits(
    FNativeImage, @Rect, Flags, Format, @LockedBitmapData));
end;

function TGdiBitmap.UnlockBits(var LockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapUnlockBits(
    FNativeImage, @LockedBitmapData));
end;

function TGdiBitmap.GetPixel(X, Y: Integer; out Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipBitmapGetPixel(FNativeImage, X, Y, Color));
end;

function TGdiBitmap.SetPixel(X, Y: Integer; Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipBitmapSetPixel(FNativeImage, X, Y, Color));
end;

function TGdiBitmap.SetResolution(XDPI, YDPI: Single): TStatus;
begin
  Result := SetStatus(GdipBitmapSetResolution(FNativeImage, XDPI, YDPI));
end;

constructor TGdiBitmap.Create(surface: IDirectDrawSurface7);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromDiRectDrawSurface(surface, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TGdiBitmap.Create(var BitmapInfo: TBitmapInfo; BitmapData: Pointer);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromGdiDib(@BitmapInfo, BitmapData, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TGdiBitmap.Create(Bmp: HBitmap; Pal: HPALETTE);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromHBitmap(Bmp, Pal, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TGdiBitmap.Create(Icon: HICON);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  GdipCreateBitmapFromHICON(Icon, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TGdiBitmap.Create(hInstance: HMODULE; BitmapName: WideString);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromResource(hInstance, PWideChar(BitmapName), Bitmap);
  SetNativeImage(Bitmap);
end;

function TGdiBitmap.FromDiRectDrawSurface7(surface: IDirectDrawSurface7): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(surface);
end;

function TGdiBitmap.FromBitmapInfo(var BitmapInfo: TBitmapInfo; BitmapData: Pointer): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(BitmapInfo, BitmapData);
end;

function TGdiBitmap.FromHBitmap(Bmp: HBitmap; Pal: HPALETTE): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Bmp, Pal);
end;

function TGdiBitmap.FromHICON(Icon: HICON): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Icon);
end;

function TGdiBitmap.FromResource(hInstance: HMODULE; BitmapName: WideString): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(hInstance, PWideChar(BitmapName));
end;

function TGdiBitmap.GetHBitmap(ColorBackground: TArgb; out Bmp: HBitmap): TStatus;
begin
  Result := SetStatus(GdipCreateHBitmapFromBitmap(FNativeImage, Bmp, ColorBackground));
end;

function TGdiBitmap.GetHICON(out Icon: HICON): TStatus;
begin
  Result := SetStatus(GdipCreateHICONFromBitmap( FNativeImage, Icon));
end;

constructor TGdiBitmap.Create(Bitmap: GpBitmap);
begin
  inherited Create;
  FLastStatus := Ok;
  SetNativeImage(Bitmap);
end;

{ TGdiGraphics }

function CreateGraphicsBitmap(Width, Height: Integer): PGdiGraphicsBitmap;
var
  BitmapInfo: TBitmapinfo;
begin
  Result := nil;
  if (Width < 1) or (Height < 1) then
    Exit;
  New(result);
  Result.DC := CreateCompatibleDC(0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;
  with Result^ do
  begin
    Handle := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
    SelectObject(DC, Handle);
  end;
  Result.Opacity := High(Byte);
  Result.Width := Width;
  Result.Height := Height;
end;

procedure DestroyGraphicsBitmap(Bitmap: PGdiGraphicsBitmap);
begin
	if Bitmap <> nil then
  begin
	  DeleteDC(Bitmap.DC);
  	DeleteObject(Bitmap.Handle);
    Dispose(Bitmap);
  end;
end;

function TGdiGraphics.FromHDC(DC: HDC): IGdiGraphics;
begin
  Result := TGdiGraphics.Create(DC);
end;

function TGdiGraphics.FromHDC(DC: HDC; Device: THandle): IGdiGraphics;
begin
  Result := TGdiGraphics.Create(DC, Device);
end;

function TGdiGraphics.FromHWND(hwnd: HWND; ICM: Boolean = False): IGdiGraphics;
begin
  Result := TGdiGraphics.Create(hwnd, ICM);
end;

function TGdiGraphics.FromImage(Image: IGdiImage): IGdiGraphics;
begin
  Result := TGdiGraphics.Create(Image);
end;

constructor TGdiGraphics.Create(DC: HDC);
var
  Graphics: GpGraphics;
begin
  inherited Create;
  Graphics := nil;
  FLastStatus := GdipCreateFromHDC(DC, Graphics);
  SetNativeGraphics(Graphics);
end;

constructor TGdiGraphics.Create(DC: HDC; Device: THandle);
var
  Graphics: GpGraphics;
begin
  inherited Create;
  Graphics := nil;
  FLastStatus := GdipCreateFromHDC2(DC, Device, Graphics);
  SetNativeGraphics(Graphics);
end;

constructor TGdiGraphics.Create(Wnd: HWND; ICM: Boolean);
var
  Graphics: GpGraphics;
begin
  inherited Create;
  Graphics := nil;
  if ICM then
    FLastStatus := GdipCreateFromHWNDICM(Wnd, Graphics)
  else
    FLastStatus := GdipCreateFromHWND(Wnd, Graphics);
  SetNativeGraphics(Graphics);
end;

constructor TGdiGraphics.Create(Image: IGdiImage);
var
  Graphics: GpGraphics;
begin
  inherited Create;
  Graphics := nil;
  if (Image <> nil) then
    FLastStatus := GdipGetImageGraphicsConText(Image.NativeImage, Graphics);
  SetNativeGraphics(Graphics);
end;

constructor TGdiGraphics.Create(Width, Height: Integer);
begin
  FBitmap := CreateGraphicsBitmap(Width, Height);
  if FBitmap <> nil then
    SetLastStatus(Ok)
  else
    SetLastStatus(InvalidParameter);
  Create(FBitmap.DC);
end;

destructor TGdiGraphics.Destroy;
begin
  if FNativeGraphics <> nil then
    GdipDeleteGraphics(FNativeGraphics);
  if FBitmap <> nil then
    DestroyGraphicsBitmap(FBitmap);
  inherited Destroy;
end;

procedure TGdiGraphics.Flush(intention: TFlushIntention = FlushIntentionFlush);
begin
  GdipFlush(FNativeGraphics, intention);
end;

function TGdiGraphics.GetHDC: HDC;
begin
  SetStatus(GdipGetDC(FNativeGraphics, Result));
end;

procedure TGdiGraphics.ReleaseHDC(DC: HDC);
begin
  SetStatus(GdipReleaseDC(FNativeGraphics, DC));
end;

function TGdiGraphics.SetRenderingOrigin(X, Y: Integer): TStatus;
begin
  Result := SetStatus(GdipSetRenderingOrigin(FNativeGraphics, X, Y));
end;

function TGdiGraphics.GetRenderingOrigin(out X, Y: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRenderingOrigin(FNativeGraphics, X, Y));
end;

function TGdiGraphics.SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
begin
  Result := SetStatus(GdipSetCompositingMode(FNativeGraphics,
    CompositingMode));
end;

function TGdiGraphics.GetCompositingMode: TCompositingMode;
begin
  SetStatus(GdipGetCompositingMode(FNativeGraphics, Result));
end;

function TGdiGraphics.SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
begin
  Result := SetStatus(GdipSetCompositingQuality(FNativeGraphics, CompositingQuality));
end;

function TGdiGraphics.GetCompositingQuality: TCompositingQuality;
begin
  SetStatus(GdipGetCompositingQuality(FNativeGraphics, Result));
end;

procedure TGdiGraphics.SetTextRenderingHint(NewMode: TTextRenderingHint);
begin
  SetStatus(GdipSetTextRenderingHint(FNativeGraphics, NewMode));
end;

function TGdiGraphics.GetTextRenderingHint: TTextRenderingHint;
begin
  SetStatus(GdipGetTextRenderingHint(FNativeGraphics, Result));
end;

function TGdiGraphics.SetTextContrast(contrast: UInt): TStatus;
begin
  Result := SetStatus(GdipSetTextContrast(FNativeGraphics, contrast));
end;

function TGdiGraphics.GetTextContrast: UInt;
begin
  SetStatus(GdipGetTextContrast(FNativeGraphics, Result));
end;

function TGdiGraphics.GetInterpolationMode: TInterpolationMode;
var
  mode: TInterpolationMode;
begin
  mode := InterpolationModeInvalid;
  SetStatus(GdipGetInterpolationMode(FNativeGraphics, mode));
  Result := mode;
end;

function TGdiGraphics.SetInterpolationMode(interpolationMode: TInterpolationMode): TStatus;
begin
  Result := SetStatus(GdipSetInterpolationMode(FNativeGraphics,
    interpolationMode));
end;

function TGdiGraphics.GetSmoothingMode: TSmoothingMode;
var
  smoothingMode: TSmoothingMode;
begin
  smoothingMode := SmoothingModeInvalid;
  SetStatus(GdipGetSmoothingMode(FNativeGraphics, smoothingMode));
  Result := smoothingMode;
end;

procedure TGdiGraphics.SetSmoothingMode(smoothingMode: TSmoothingMode);
begin
  SetStatus(GdipSetSmoothingMode(FNativeGraphics, smoothingMode));
end;

function TGdiGraphics.GetPixelOffsetMode: TPixelOffsetMode;
var
  pixelOffsetMode: TPixelOffsetMode;
begin
  pixelOffsetMode := PixelOffsetModeInvalid;
  SetStatus(GdipGetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
  Result := pixelOffsetMode;
end;

function TGdiGraphics.SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
begin
  Result := SetStatus(GdipSetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
end;

procedure  TGdiGraphics.SetTransform(Matrix: IGdiMatrix);
begin
  if Matrix <> nil then
    SetStatus(GdipSetWorldTransform(FNativeGraphics, Matrix.NativeMatrix))
  else
    SetStatus(GdipSetWorldTransform(FNativeGraphics, nil));
end;

function TGdiGraphics.GetTransform: IGdiMatrix;
begin
  Result := NewMatrix;
  SetStatus(GdipGetWorldTransform(FNativeGraphics, Result.NativeMatrix));
end;

function TGdiGraphics.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetWorldTransform(FNativeGraphics));
end;

function TGdiGraphics.MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyWorldTransform(FNativeGraphics,
    Matrix.NativeMatrix,
    Order));
end;

function TGdiGraphics.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateWorldTransform(FNativeGraphics,
    DX, DY, Order));
end;

function TGdiGraphics.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleWorldTransform(FNativeGraphics,
    SX, SY, Order));
end;

function TGdiGraphics.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateWorldTransform(FNativeGraphics,
    Angle, Order));
end;

function TGdiGraphics.SetPageUnit(Unit_: TUnit): TStatus;
begin
  Result := SetStatus(GdipSetPageUnit(FNativeGraphics, Unit_));
end;

function TGdiGraphics.SetPageScale(Scale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPageScale(FNativeGraphics, Scale));
end;

function TGdiGraphics.GetPageUnit: TUnit;
begin
  SetStatus(GdipGetPageUnit(FNativeGraphics, Result));
end;

function TGdiGraphics.GetPageScale: Single;
begin
  SetStatus(GdipGetPageScale(FNativeGraphics, Result));
end;

function TGdiGraphics.GetDPIX: Single;
begin
  SetStatus(GdipGetDPIX(FNativeGraphics, Result));
end;

function TGdiGraphics.GetDPIY: Single;
begin
  SetStatus(GdipGetDPIY(FNativeGraphics, Result));
end;

function TGdiGraphics.GetWidth: Integer;
begin
  if FBitmap <> nil then
    Result := FBitmap.Width
  else
    Result := 0;
end;

function TGdiGraphics.GetHeight: Integer;
begin
  if FBitmap <> nil then
    Result := FBitmap.Height
  else
    Result := 0;
end;

function TGdiGraphics.GetOpacity: Byte;
begin
  if FBitmap <> nil then
    Result := FBitmap.Opacity
  else
    Result := 0;
end;

procedure TGdiGraphics.SetOpacity(Value: Byte);
begin
  if FBitmap <> nil then
    FBitmap.Opacity := Value;
end;

function TGdiGraphics.TransformPoints(DestSpace: TCoordinateSpace;
  SrcSpace: TCoordinateSpace;
  Pts: PPointF;
  Count: Integer): TStatus;
begin
  Result := SetStatus(GdipTransformPoints(FNativeGraphics,
    DestSpace, SrcSpace, Pts, Count));
end;

function TGdiGraphics.TransformPoints(DestSpace: TCoordinateSpace;
  SrcSpace: TCoordinateSpace;
  Pts: PPointI;
  Count: Integer): TStatus;
begin
  Result := SetStatus(GdipTransformPointsI(FNativeGraphics,
    DestSpace, SrcSpace, Pts, Count));
end;

function TGdiGraphics.GetNearestColor(var Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGetNearestColor(FNativeGraphics, @Color));
end;

function GraphicsOverlay(Wnd: HWND; const Bitmap: PGdiGraphicsBitmap; Opacity: Byte = 0): Boolean;
var
	Blend: TBlendFunction;
  Rect: TRect;
	P1, P2: TPoint;
  S: TSize;
  DC: HDC;
begin
	Result := False;
	if (Bitmap = nil) or (Bitmap.DC = 0) then Exit;
  Result := True;
	SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
	GetWindowRect(Wnd, Rect);
	P1.X := Rect.Left;
	P1.Y := Rect.Top;
	with Blend do
	begin
		BlendOp := AC_SRC_OVER;
		BlendFlags := 0;
	  if Opacity = 0 then
  		SourceConstantAlpha := Bitmap.Opacity
		else
			SourceConstantAlpha := Opacity;
		AlphaFormat := AC_SRC_ALPHA;
	end;
	DC := GetDC(0);
	P2.X := 0;
	P2.Y := 0;
	S.cx := Bitmap.Width;
	S.cy := Bitmap.Height;
	UpdateLayeredWindow(Wnd, DC, @P1, @S, Bitmap.DC,
		@P2, 0, @Blend, ULW_ALPHA);
	ReleaseDC(0, DC);
end;

function TGdiGraphics.Overlay(Wnd: HWND; Opacity: Byte = 0): TStatus;
begin
	if GraphicsOverlay(Wnd, FBitmap, Opacity) then
  	Result := SetStatus(Ok)
	else
    Result := SetStatus(GenericError);
end;

function BitmapDraw(DC: HDC; X, Y, Width, Height: Integer; Bitmap: PGdiGraphicsBitmap): Boolean;
var
	Func: TBlendFunction;
begin
  Result := Bitmap <> nil;
	if Result and (Bitmap.Opacity > 0) then
  begin
    Func.BlendOp := 0;
    Func.BlendFlags := 0;
		Func.SourceConstantAlpha := Bitmap.Opacity;
		Func.AlphaFormat := AC_SRC_ALPHA;
		AlphaBlend(DC, X, Y, Width, Height, Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, Func);
	end;
end;

function TGdiGraphics.Draw(DC: HDC; X, Y: Integer): TStatus;
begin
  Result := Draw(DC, X, Y, GetWidth, GetHeight);
end;

function TGdiGraphics.Draw(DC: HDC; X, Y, Width, Height: Integer): TStatus;
begin
  if BitmapDraw(DC, X, Y, Width, Height, FBitmap) then
    Result := SetStatus(Ok)
  else
    Result := SetStatus(GenericError);
end;

function TGdiGraphics.Draw(DC: HDC; const Rect: TRectI): TStatus;
begin
  Result := Draw(DC, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.Draw(DC: HDC; X, Y: Single): TStatus;
begin
  Result := Draw(DC, Round(X), Round(Y), GetWidth, GetHeight);
end;

function TGdiGraphics.Draw(DC: HDC; X, Y, Width, Height: Single): TStatus;
begin
  Result := Draw(DC, Round(X), Round(Y), Round(Width), Round(Height));
end;

function TGdiGraphics.Draw(DC: HDC; const Rect: TRectF): TStatus;
begin
  Result := Draw(DC, Round(Rect.X), Round(Rect.Y), Round(Rect.Width), Round(Rect.Height));
end;

function TGdiGraphics.DrawLine(Pen: IGdiPen; X1, Y1, X2, Y2: Single): TStatus;
begin
  Result := SetStatus(GdipDrawLine(FNativeGraphics,
    Pen.NativePen, X1, Y1, X2, Y2));
end;

function TGdiGraphics.DrawLine(Pen: IGdiPen; const Pt1, Pt2: TPointF): TStatus;
begin
  Result := DrawLine(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGdiGraphics.DrawLines(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLines(FNativeGraphics, Pen.NativePen, Points, Count));
end;

function TGdiGraphics.DrawLine(Pen: IGdiPen; X1, Y1, X2, Y2: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLineI(FNativeGraphics,
    Pen.NativePen, X1, Y1, X2, Y2));
end;

function TGdiGraphics.DrawLine(Pen: IGdiPen; const Pt1, Pt2: TPointI): TStatus;
begin
  Result := DrawLine(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGdiGraphics.DrawLines(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLinesI(FNativeGraphics, Pen.NativePen, Points, Count));
end;

function TGdiGraphics.DrawArc(Pen: IGdiPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawArc(FNativeGraphics, NativePen,
    X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphics.DrawArc(Pen: IGdiPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawArc(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGdiGraphics.DrawArc(Pen: IGdiPen; X, Y, Width, Height: Integer; StartAngle,
  SweepAngle: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawArcI(FNativeGraphics,
    NativePen, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphics.DrawArc(Pen: IGdiPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawArc(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGdiGraphics.DrawBezier(Pen: IGdiPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBezier(FNativeGraphics,
    NativePen, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGdiGraphics.DrawBezier(Pen: IGdiPen; const Pt1, Pt2, Pt3, Pt4: TPointF): TStatus;
begin
  Result := DrawBezier(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGdiGraphics.DrawBeziers(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBeziers(FNativeGraphics, NativePen, Points, Count));
end;

function TGdiGraphics.DrawBezier(Pen: IGdiPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBezierI(FNativeGraphics,
    NativePen, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGdiGraphics.DrawBezier(Pen: IGdiPen; const Pt1, Pt2, Pt3, Pt4: TPointI): TStatus;
begin
  Result := DrawBezier(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGdiGraphics.DrawBeziers(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBeziersI(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGdiGraphics.DrawRectangle(Pen: IGdiPen; const Rect: TRectF): TStatus;
begin
  Result := DrawRectangle(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.DrawRectangle(Pen: IGdiPen; X, Y, Width, Height: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectangle(FNativeGraphics,
    NativePen, X, Y, Width, Height));
end;

function TGdiGraphics.DrawRectangles(Pen: IGdiPen; Rects: PRectF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectangles(FNativeGraphics, NativePen, Rects, Count));
end;

function TGdiGraphics.DrawRectangle(Pen: IGdiPen; const Rect: TRectI): TStatus;
begin
  Result := DrawRectangle(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.DrawRectangle(Pen: IGdiPen; X, Y, Width, Height: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectangleI(FNativeGraphics,
    NativePen, X, Y, Width, Height));
end;

function TGdiGraphics.DrawRectangles(Pen: IGdiPen; Rects: PRectI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectanglesI(FNativeGraphics,
    NativePen, Rects, Count));
end;

function TGdiGraphics.DrawEllipse(Pen: IGdiPen; const Rect: TRectF): TStatus;
begin
  Result := DrawEllipse(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.DrawEllipse(Pen: IGdiPen; X, Y, Width, Height: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawEllipse(FNativeGraphics,
    NativePen, X, Y, Width, Height));
end;

function TGdiGraphics.DrawEllipse(Pen: IGdiPen; const Rect: TRectI): TStatus;
begin
  Result := DrawEllipse(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.DrawEllipse(Pen: IGdiPen; X, Y, Width, Height: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawEllipseI(FNativeGraphics, NativePen, X, Y, Width, Height));
end;

function TGdiGraphics.DrawPie(Pen: IGdiPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawPie(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGdiGraphics.DrawPie(Pen: IGdiPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawPie(FNativeGraphics,
    NativePen, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphics.DrawPie(Pen: IGdiPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawPie(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGdiGraphics.DrawPie(Pen: IGdiPen; X, Y, Width, Height: Integer;
  StartAngle, SweepAngle: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawPieI(FNativeGraphics,
    NativePen, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphics.DrawPolygon(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawPolygon(FNativeGraphics, NativePen, Points, Count));
end;

function TGdiGraphics.DrawPolygon(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawPolygonI(FNativeGraphics, NativePen, Points, Count));
end;

function TGdiGraphics.DrawPath(Pen: IGdiPen; Path: IGdiGraphicsPath): TStatus;
var
  NativePen: GpPen;
  NativePath: GpPath;
begin
  NativePen := nil;
  NativePath := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  if Path <> nil then
    NativePath := Path.NativePath;
  Result := SetStatus(GdipDrawPath(FNativeGraphics, NativePen, NativePath));
end;

function TGdiGraphics.DrawCurve(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGdiGraphics.DrawCurve(Pen: IGdiPen; Points: PPointF; Count: Integer; Tension: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve2(FNativeGraphics,
    NativePen, Points, Count, Tension));
end;

function TGdiGraphics.DrawCurve(Pen: IGdiPen; Points: PPointF; Count, Offset,
  NumberOfSegments: Integer; Tension: Single = 0.5): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve3(FNativeGraphics,
    NativePen, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TGdiGraphics.DrawCurve(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurveI(FNativeGraphics, NativePen, Points, Count));
end;

function TGdiGraphics.DrawCurve(Pen: IGdiPen; Points: PPointI; Count: Integer; Tension: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve2I(FNativeGraphics,
    NativePen, Points, Count, Tension));
end;

function TGdiGraphics.DrawCurve(Pen: IGdiPen; Points: PPointI; Count, Offset,
  NumberOfSegments: Integer; Tension: Single = 0.5): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve3I(FNativeGraphics, NativePen, Points,
    Count, Offset, NumberOfSegments, Tension));
end;

function TGdiGraphics.DrawClosedCurve(Pen: IGdiPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawClosedCurve(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGdiGraphics.DrawClosedCurve(Pen: IGdiPen; Points: PPointF; Count: Integer;
  Tension: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawClosedCurve2(FNativeGraphics,
    NativePen, Points, Count, Tension));
end;

function TGdiGraphics.DrawClosedCurve(Pen: IGdiPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawClosedCurveI(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGdiGraphics.DrawClosedCurve(Pen: IGdiPen; Points: PPointI;
  Count: Integer; Tension: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawClosedCurve2I(FNativeGraphics,
    NativePen, Points, Count, Tension));
end;

function TGdiGraphics.Clear(Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGraphicsClear( FNativeGraphics, Color));
end;

function TGdiGraphics.FillRectangle(Brush: IGdiBrush; const Rect: TRectF): TStatus;
begin
  Result := FillRectangle(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.FillRectangle(Brush: IGdiBrush; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipFillRectangle(FNativeGraphics,
    Brush.NativeBrush, X, Y, Width, Height));
end;

function TGdiGraphics.FillRectangles(Brush: IGdiBrush; Rects: PRectF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangles(FNativeGraphics,
    Brush.NativeBrush, Rects, Count));
end;

function TGdiGraphics.FillRectangle(Brush: IGdiBrush; const Rect: TRectI): TStatus;
begin
  Result := FillRectangle(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.FillRectangle(Brush: IGdiBrush; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangleI(FNativeGraphics,
    Brush.NativeBrush,
    X,
    Y,
    Width,
    Height));
end;

function TGdiGraphics.FillRectangles(Brush: IGdiBrush; Rects: PRectI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectanglesI(FNativeGraphics,
    Brush.NativeBrush,
    Rects,
    Count));
end;

function TGdiGraphics.FillPolygon(Brush: IGdiBrush; Points: PPointF; Count: Integer): TStatus;
begin
  Result := FillPolygon(Brush, Points, Count, FillModeAlternate);
end;

function TGdiGraphics.FillPolygon(Brush: IGdiBrush; Points: PPointF; Count: Integer;
  FillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygon(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count, FillMode));
end;

function TGdiGraphics.FillPolygon(Brush: IGdiBrush; Points: PPointI; Count: Integer): TStatus;
begin
  Result := FillPolygon(Brush, Points, Count, FillModeAlternate);
end;

function TGdiGraphics.FillPolygon(Brush: IGdiBrush; Points: PPointI; Count: Integer;
  FillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygonI(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count,
    FillMode));
end;

function TGdiGraphics.FillEllipse(Brush: IGdiBrush; const Rect: TRectF): TStatus;
begin
  Result := FillEllipse(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.FillEllipse(Brush: IGdiBrush; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipFillEllipse(FNativeGraphics,
    Brush.NativeBrush, X, Y,
    Width, Height));
end;

function TGdiGraphics.FillEllipse(Brush: IGdiBrush; const Rect: TRectI): TStatus;
begin
  Result := FillEllipse(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.FillEllipse(Brush: IGdiBrush; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipFillEllipseI(FNativeGraphics,
    Brush.NativeBrush,
    X,
    Y,
    Width,
    Height));
end;

function TGdiGraphics.FillPie(Brush: IGdiBrush; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := FillPie(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGdiGraphics.FillPie(Brush: IGdiBrush; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipFillPie(FNativeGraphics,
    Brush.NativeBrush, X, Y,
    Width, Height, StartAngle,
    SweepAngle));
end;

function TGdiGraphics.FillPie(Brush: IGdiBrush; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := FillPie(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGdiGraphics.FillPie(Brush: IGdiBrush; X, Y, Width, Height: Integer; StartAngle,
  SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipFillPieI(FNativeGraphics,
    Brush.NativeBrush,
    X,
    Y,
    Width,
    Height,
    StartAngle,
    SweepAngle));
end;

function TGdiGraphics.FillPath(Brush: IGdiBrush; Path: IGdiGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipFillPath(FNativeGraphics,
    Brush.NativeBrush,
    Path.NativePath));
end;

function TGdiGraphics.FillClosedCurve(Brush: IGdiBrush; Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count));
end;

function TGdiGraphics.FillClosedCurve(Brush: IGdiBrush; Points: PPointF; Count: Integer;
  FillMode: TFillMode; Tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count,
    Tension, FillMode));
end;

function TGdiGraphics.FillClosedCurve(Brush: IGdiBrush; Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurveI(FNativeGraphics,
    Brush.NativeBrush,
    Points,
    Count));
end;

function TGdiGraphics.FillClosedCurve(Brush: IGdiBrush; Points: PPointI;
  Count: Integer; FillMode: TFillMode; Tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2I(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count,
    Tension, FillMode));
end;

function TGdiGraphics.FillRegion(Brush: IGdiBrush; Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipFillRegion(FNativeGraphics,
    Brush.NativeBrush,
    Region.NativeRegion));
end;

function TGdiGraphics.DrawString(Text: WideString; Font: IGdiFont;
  const LayoutRect: TRectF; StringFormat: IGdiStringFormat; Brush: IGdiBrush): TStatus;
var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;
begin
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  if Assigned(StringFormat) then
    nStringFormat := StringFormat.NativeFormat
  else
    nStringFormat := nil;
  if Assigned(Brush) then
    nbrush := Brush.NativeBrush
  else
    nbrush := nil;
  Result := SetStatus(GdipDrawString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @LayoutRect,
    nStringFormat,
    nbrush));
end;

function TGdiGraphics.DrawString(Text: WideString; Font: IGdiFont;
  const Origin: TPointF; Brush: IGdiBrush): TStatus;
var
  Rect: TRectF;
  nFont: GpFont;
  nBrush: GpBrush;
begin
  Rect.X := Origin.X;
  Rect.Y := Origin.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  if Assigned(Brush) then
    nBrush := Brush.NativeBrush
  else
    nBrush := nil;
  Result := SetStatus(GdipDrawString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @Rect,
    nil,
    nbrush));
end;

function TGdiGraphics.DrawString(Text: WideString; Font: IGdiFont;
  const Origin: TPointF; StringFormat: IGdiStringFormat; Brush: IGdiBrush): TStatus;
var
  Rect: TRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
  nBrush: GpBrush;
begin
  Rect.X := Origin.X;
  Rect.Y := Origin.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  if Assigned(StringFormat) then
    nStringFormat := StringFormat.NativeFormat
  else
    nStringFormat := nil;
  if Assigned(Brush) then
    nbrush := Brush.NativeBrush
  else
    nbrush := nil;
  Result := SetStatus(GdipDrawString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @Rect,
    nStringFormat,
    nbrush));
end;

function TGdiGraphics.MeasureString(Text: WideString; Font: IGdiFont;
  const LayoutRect: TRectF; StringFormat: IGdiStringFormat; out BoundingBox: TRectF;
  CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus;
var
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  if Assigned(StringFormat) then
    nStringFormat := StringFormat.NativeFormat
  else
    nStringFormat := nil;
  Result := SetStatus(GdipMeasureString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @LayoutRect,
    nStringFormat, @BoundingBox,
    CodePointsFitted,
    LinesFilled));
end;

function TGdiGraphics.MeasureString(Text: WideString; Font: IGdiFont;
  const LayoutRectSize: TSizeF; StringFormat: IGdiStringFormat; out Size: TSizeF;
  CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus;
var
  LayoutRect, BoundingBox: TRectF;
  Status: TStatus;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  LayoutRect.X := 0;
  LayoutRect.Y := 0;
  LayoutRect.Width := LayoutRectSize.Width;
  LayoutRect.Height := LayoutRectSize.Height;
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  if Assigned(StringFormat) then
    nStringFormat := StringFormat.NativeFormat
  else
    nStringFormat := nil;
  Status := SetStatus(GdipMeasureString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @LayoutRect,
    nStringFormat, @BoundingBox,
    CodePointsFitted,
    LinesFilled));
  if (Status = Ok) then
  begin
    Size.Width := BoundingBox.Width;
    Size.Height := BoundingBox.Height;
  end;
  Result := Status;
end;

function TGdiGraphics.MeasureString(Text: WideString; Font: IGdiFont;
  const Origin: TPointF; StringFormat: IGdiStringFormat; out BoundingBox: TRectF): TStatus;
var
  Rect: TRectF;
  nFont: GpFont;
  nStringFormat: GpStringFormat;
begin
  Rect.X := Origin.X;
  Rect.Y := Origin.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  if Assigned(StringFormat) then
    nStringFormat := StringFormat.NativeFormat
  else
    nStringFormat := nil;
  Result := SetStatus(GdipMeasureString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @Rect,
    nStringFormat, @BoundingBox,
    nil,
    nil));
end;

function TGdiGraphics.MeasureString(Text: WideString; Font: IGdiFont;
  const LayoutRect: TRectF; out BoundingBox: TRectF): TStatus;
var
  nFont: GpFont;
begin
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  Result := SetStatus(GdipMeasureString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @LayoutRect,
    nil, @BoundingBox,
    nil,
    nil));
end;

function TGdiGraphics.MeasureString(Text: WideString; Font: IGdiFont;
  const Origin: TPointF; out BoundingBox: TRectF): TStatus;
var
  nFont: GpFont;
  Rect: TRectF;
begin
  if Assigned(Font) then
    nFont := Font.NativeFont
  else
    nFont := nil;
  Rect.X := Origin.X;
  Rect.Y := Origin.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  Result := SetStatus(GdipMeasureString(
    FNativeGraphics,
    PWideChar(Text),
    Length(Text),
    nFont, @Rect,
    nil, @BoundingBox,
    nil,
    nil));
end;

function TGdiGraphics.MeasureCharacterRanges(Text: WideString; Font: IGdiFont;
  const LayoutRect: TRectF; StringFormat: IGdiStringFormat; RegionCount: Integer;
  const Regions: array of IGdiRegion): TStatus;
var
  FNativeRegions: Pointer;
  I: Integer;
  Status: TStatus;
  NativeFont: GpFont;
  NativeFormat: GpStringFormat;
type
  TArrayGpRegion = array of GpRegion;
begin
  NativeFont := nil;
  NativeFormat := nil;
  if Font <> nil then
    NativeFont := Font.NativeFont;
  if StringFormat <> nil then
    NativeFormat := StringFormat.NativeFormat;
  if RegionCount <= 0 then
  begin
    Result := InvalidParameter;
    Exit;
  end;
  GetMem(FNativeRegions, SizeOf(GpRegion) * RegionCount);
  for I := 0 to RegionCount - 1 do
    TArrayGpRegion(FNativeRegions)[I] := Regions[I].NativeRegion;
  Status := SetStatus(GdipMeasureCharacterRanges(
    FNativeGraphics, PWideChar(Text), Length(Text), NativeFont, @LayoutRect,
    NativeFormat, RegionCount, FNativeRegions));
  FreeMem(FNativeRegions, SizeOf(GpRegion) * RegionCount);
  Result := Status;
end;

function TGdiGraphics.DrawDriverString(Text: PUInt16; Length: Integer; Font: IGdiFont; Brush: IGdiBrush; Positions: PPointF; Flags: Integer; Matrix: IGdiMatrix): TStatus;
var
  NativeFont: GpFont;
  NativeBrush: GpBrush;
  NativeMatrix: GpMatrix;
begin
  NativeFont := nil;
  NativeBrush := nil;
  NativeMatrix := nil;
  if Font <> nil then
    NativeFont := Font.NativeFont;
  if Brush <> nil then
    NativeBrush := Brush.NativeBrush;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipDrawDriverString(
    FNativeGraphics, Text, Length, NativeFont, NativeBrush, Positions,
      Flags, NativeMatrix));
end;

function TGdiGraphics.MeasureDriverString(Text: PUInt16; Length: Integer; Font: IGdiFont;
  Positions: PPointF; Flags: Integer; Matrix: IGdiMatrix;
  out BoundingBox: TRectF): TStatus;
var
  NativeFont: GpFont;
  NativeMatrix: GpMatrix;
begin
  NativeFont := nil;
  NativeMatrix := nil;
  if Font <> nil then
    NativeFont := Font.NativeFont;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipMeasureDriverString(
    FNativeGraphics, Text, Length, NativeFont, Positions, Flags, NativeMatrix, @BoundingBox));
end;

function TGdiGraphics.DrawCachedBitmap(Bitmap: IGdiCachedBitmap; X, Y: Integer): TStatus;
var
  NativeCachedBitmap: GpCachedBitmap;
begin
  NativeCachedBitmap := nil;
  if Bitmap <> nil then
    NativeCachedBitmap := Bitmap.NativeCachedBitmap;
  Result := SetStatus(GdipDrawCachedBitmap(
    FNativeGraphics, NativeCachedBitmap, X, Y));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; const Point: TPointF): TStatus;
begin
  Result := DrawImage(Image, Point.X, Point.Y);
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; X, Y: Single): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImage(FNativeGraphics, NativeImage, X, Y));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; const Rect: TRectF): TStatus;
begin
  Result := DrawImage(Image, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; X, Y, Width, Height: Single): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImageRect(FNativeGraphics,
    NativeImage, X, Y, Width, Height));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; const Point: TPointI): TStatus;
begin
  Result := DrawImage(Image, Point.X, Point.Y);
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; X, Y: Integer): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImageI(FNativeGraphics, NativeImage, X, Y));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; const Rect: TRectI): TStatus;
begin
  Result := DrawImage(Image, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; X, Y, Width, Height: Integer): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImageRectI(FNativeGraphics,
    NativeImage, X, Y, Width, Height));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; DestPoints: PPointF; Count: Integer): TStatus;
var
  NativeImage: GpImage;
begin
  if ((Count <> 3) and (Count <> 4)) or (DestPoints = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImagePoints(FNativeGraphics,
    NativeImage, DestPoints, Count));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; DestPoints: PPointI; Count: Integer): TStatus;
var
  NativeImage: GpImage;
begin
  if ((Count <> 3) and (Count <> 4)) or (DestPoints = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImagePointsI(FNativeGraphics,
    NativeImage, DestPoints, Count));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single;
  SrcUnit: TUnit): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImagePointRect(FNativeGraphics,
    NativeImage, X, Y, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; const DestRect: TRectF; SrcX, SrcY, SrcWidth, SrcHeight: Single;
  SrcUnit: TUnit; ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
  CallbackData: Pointer = nil): TStatus;
var
  NativeImage: GpImage;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeImage := nil;
  NativeImageAttributes := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipDrawImageRectRect(FNativeGraphics,
    NativeImage, DestRect.X, DestRect.Y, DestRect.Width, DestRect.Height,
    SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit, NativeImageAttributes,
    Callback, CallbackData));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; DestPoints: PPointF; Count: Integer;
  SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
  ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
  CallbackData: Pointer = nil): TStatus;
var
  NativeImage: GpImage;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeImage := nil;
  NativeImageAttributes := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipDrawImagePointsRect(FNativeGraphics,
    NativeImage, DestPoints, Count,
    SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit,
    NativeImageAttributes, Callback, CallbackData));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Integer;
  SrcUnit: TUnit): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImagePointRectI(FNativeGraphics,
    NativeImage, X, Y, SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; const DestRect: TRectI; SrcX, SrcY, SrcWidth,
  SrcHeight: Integer; SrcUnit: TUnit; ImageAttributes: IGdiImageAttributes = nil;
  Callback: DrawImageAbort = nil; CallbackData: Pointer = nil): TStatus;
var
  NativeImage: GpImage;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeImage := nil;
  NativeImageAttributes := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipDrawImageRectRectI(FNativeGraphics,
    NativeImage, DestRect.X, DestRect.Y, DestRect.Width, DestRect.Height,
    SrcX, SrcY, SrcWidth, SrcHeight, SrcUnit,
    NativeImageAttributes, Callback, CallbackData));
end;

function TGdiGraphics.DrawImage(Image: IGdiImage; DestPoints: PPointI;
  Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
  ImageAttributes: IGdiImageAttributes = nil; Callback: DrawImageAbort = nil;
  CallbackData: Pointer = nil): TStatus;
var
  NativeImage: GpImage;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeImage := nil;
  NativeImageAttributes := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipDrawImagePointsRectI(FNativeGraphics,
    NativeImage, DestPoints, Count, SrcX, SrcY, SrcWidth,
    SrcHeight, SrcUnit, NativeImageAttributes, Callback, CallbackData));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointF;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileDestPoint(
    FNativeGraphics, NativeMetafile, @DestPoint,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointI;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileDestPointI(
    FNativeGraphics, NativeMetafile, @DestPoint,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectF;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileDestRect(
    FNativeGraphics, NativeMetafile, @DestRect,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectI;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileDestRectI(
    FNativeGraphics, NativeMetafile, @DestRect,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointF;
  Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileDestPoints(
    FNativeGraphics, NativeMetafile, DestPoints, Count,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointI;
  Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileDestPointsI(
    FNativeGraphics, NativeMetafile, DestPoints, Count, Callback,
    CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointF;
  const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoint(
    FNativeGraphics, NativeMetafile, @DestPoint, @SrcRect, SrcUnit,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestPoint: TPointI;
  const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointI(
    FNativeGraphics, NativeMetafile, @DestPoint, @SrcRect, SrcUnit,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestRect: TRectF;
  const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestRect(
    FNativeGraphics, NativeMetafile, @DestRect, @SrcRect, SrcUnit,
    Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; const DestRect, SrcRect: TRectI;
  SrcUnit: TUnit; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestRectI(
    FNativeGraphics, NativeMetafile, @DestRect, @SrcRect, SrcUnit, Callback,
    CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointF;
  Count: Integer; const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPoints(
    FNativeGraphics, NativeMetafile, DestPoints, Count, @SrcRect,
    SrcUnit, Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.EnumerateMetafile(Metafile: IGdiMetafile; DestPoints: PPointI;
  Count: Integer; const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IGdiImageAttributes = nil): TStatus;
var
  NativeMetafile: GpMetafile;
  NativeImageAttributes: GpImageAttributes;
begin
  NativeMetafile := nil;
  NativeImageAttributes := nil;
  if Metafile <> nil then
    NativeMetafile := GpMetafile(Metafile.NativeImage);
  if ImageAttributes <> nil then
    NativeImageAttributes := ImageAttributes.NativeImageAttr;
  Result := SetStatus(GdipEnumerateMetafileSrcRectDestPointsI(
    FNativeGraphics, NativeMetafile, DestPoints, Count, @SrcRect,
    SrcUnit, Callback, CallbackData, NativeImageAttributes));
end;

function TGdiGraphics.SetClip(G: IGdiGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipGraphics(FNativeGraphics,
    G.NativeGraphics, CombineMode));
end;

function TGdiGraphics.SetClip(Rect: TRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineMode));
end;

function TGdiGraphics.SetClip(Rect: TRectI; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineMode));
end;

function TGdiGraphics.SetClip(Path: IGdiGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipPath(FNativeGraphics,
    Path.NativePath, CombineMode));
end;

function TGdiGraphics.SetClip(Region: IGdiRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(FNativeGraphics,
    Region.NativeRegion, CombineMode));
end;

function TGdiGraphics.SetClip(Rgn: HRGN; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipHrgn(FNativeGraphics, Rgn, CombineMode));
end;

function TGdiGraphics.IntersectClip(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeIntersect));
end;

function TGdiGraphics.IntersectClip(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeIntersect));
end;

function TGdiGraphics.IntersectClip(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(FNativeGraphics,
    Region.NativeRegion,
    CombineModeIntersect));
end;

function TGdiGraphics.ExcludeClip(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeExclude));
end;

function TGdiGraphics.ExcludeClip(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeExclude));
end;

function TGdiGraphics.ExcludeClip(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(FNativeGraphics,
    Region.NativeRegion, CombineModeExclude));
end;

function TGdiGraphics.ResetClip: TStatus;
begin
  Result := SetStatus(GdiPresetClip(FNativeGraphics));
end;

function TGdiGraphics.TranslateClip(DX, DY: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateClip(FNativeGraphics, DX, DY));
end;

function TGdiGraphics.TranslateClip(DX, DY: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateClipI(FNativeGraphics, DX, DY));
end;

function TGdiGraphics.GetClip(Region: IGdiRegion): TStatus;
begin
  Result := SetStatus(GdipGetClip(FNativeGraphics, Region.NativeRegion));
end;

function TGdiGraphics.GetClipBounds(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetClipBounds(FNativeGraphics, @Rect));
end;

function TGdiGraphics.GetClipBounds(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetClipBoundsI(FNativeGraphics, @Rect));
end;

function TGdiGraphics.IsClipEmpty: Boolean;
begin
  SetStatus(GdipIsClipEmpty(FNativeGraphics, @Result));
end;

function TGdiGraphics.GetVisibleClipBounds(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBounds(FNativeGraphics, @Rect));
end;

function TGdiGraphics.GetVisibleClipBounds(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBoundsI(FNativeGraphics, @Rect));
end;

function TGdiGraphics.IsVisibleClipEmpty: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleClipEmpty(FNativeGraphics, B));
  Result := B;
end;

function TGdiGraphics.IsVisible(X, Y: Integer): Boolean;
var
  Pt: TPointI;
begin
  Pt.X := X;
  Pt.Y := Y;
  Result := IsVisible(Pt);
end;

function TGdiGraphics.IsVisible(const Point: TPointI): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisiblePointI(FNativeGraphics, Point.X, Point.Y, B));
  Result := B;
end;

function TGdiGraphics.IsVisible(X, Y, Width, Height: Integer): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRectI(FNativeGraphics, X, Y, Width, Height, B));
  Result := B;
end;

function TGdiGraphics.IsVisible(const Rect: TRectI): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRectI(FNativeGraphics, Rect.X, Rect.Y, Rect.Width, Rect.Height, B));
  Result := B;
end;

function TGdiGraphics.IsVisible(X, Y: Single): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisiblePoint(FNativeGraphics, X, Y, B));
  Result := B;
end;

function TGdiGraphics.IsVisible(const Point: TPointF): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisiblePoint(FNativeGraphics, Point.X, Point.Y, B));
  Result := B;
end;

function TGdiGraphics.IsVisible(X, Y, Width, Height: Single): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRect(FNativeGraphics, X, Y, Width, Height, B));
  Result := B;
end;

function TGdiGraphics.IsVisible(const Rect: TRectF): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRect(FNativeGraphics, Rect.X, Rect.Y, Rect.Width,
    Rect.Height, B));
  Result := B;
end;

function TGdiGraphics.Save: GraphicsState;
begin
  SetStatus(GdipSaveGraphics(FNativeGraphics, Result));
end;

function TGdiGraphics.Restore(gstate: GraphicsState): TStatus;
begin
  Result := SetStatus(GdipRestoreGraphics(FNativeGraphics,
    gstate));
end;

function TGdiGraphics.BeginContainer(const DstRect, SrcRect: TRectF; Unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainer(FNativeGraphics, @DstRect, @SrcRect, Unit_, Result));
end;

function TGdiGraphics.BeginContainer(const DstRect, SrcRect: TRectI; Unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainerI(FNativeGraphics, @DstRect, @SrcRect, Unit_, Result));
end;

function TGdiGraphics.BeginContainer: GraphicsContainer;
begin
  SetStatus(GdipBeginContainer2(FNativeGraphics, Result));
end;

function TGdiGraphics.EnDContainer(state: GraphicsContainer): TStatus;
begin
  Result := SetStatus(GdipEnDContainer(FNativeGraphics, state));
end;

function TGdiGraphics.AddMetafileComment(data: PByte; SizeData: UInt): TStatus;
begin
  Result := SetStatus(GdipComment(FNativeGraphics, SizeData, data));
end;

function TGdiGraphics.GetHalftonePalette: HPALETTE;
begin
  Result := GdipCreateHalftonePalette;
end;

constructor TGdiGraphics.Create(Graphics: GpGraphics);
begin
  inherited Create;
  FLastStatus := Ok;
  SetNativeGraphics(Graphics);
end;

function TGdiGraphics.GetNativeGraphics: GpGraphics;
begin
  Result := FNativeGraphics;
end;

procedure TGdiGraphics.SetNativeGraphics(Graphics: GpGraphics);
begin
  Self.FNativeGraphics := Graphics;
end;

{ TGdiFontFamily }

constructor TGdiFontFamily.Create;
begin
  inherited Create;
end;

constructor TGdiFontFamily.Create(Name: WideString; FontCollection: IGdiFontCollection = nil);
var
  NativeFontCollection: GpFontCollection;
begin
  inherited Create;
  NativeFontCollection := nil;
  if Assigned(FontCollection) then
    NativeFontCollection := FontCollection.NativeFontCollection;
  FLastStatus := GdipCreateFontFamilyFromName(PWideChar(Name), NativeFontCollection, FNativeFamily);
end;

destructor TGdiFontFamily.Destroy;
begin
  GdipDeleteFontFamily(FNativeFamily);
end;

class function TGdiFontFamily.GenericSansSerif: IGdiFontFamily;
var
  FontFamily: GpFontFamily;
begin
  if GenericSansSerifFontFamily <> nil then
  begin
    Result := GenericSansSerifFontFamily;
    Exit;
  end;
  GenericSansSerifFontFamily := TGdiFontFamily.Create;
  GenericSansSerifFontFamily.LastStatus := GdipGetGenericFontFamilySansSerif(FontFamily);
  GenericSansSerifFontFamily.NativeFamily := FontFamily;
  Result := GenericSansSerifFontFamily;
end;

class function TGdiFontFamily.GenericSerif: IGdiFontFamily;
var
  FontFamily: GpFontFamily;
begin
  if GenericSerifFontFamily <> nil then
  begin
    Result := GenericSerifFontFamily;
    Exit;
  end;
  GenericSerifFontFamily := TGdiFontFamily.Create;
  GenericSerifFontFamily.LastStatus := GdipGetGenericFontFamilySerif(FontFamily);
  GenericSerifFontFamily.NativeFamily := FontFamily;
  Result := GenericSerifFontFamily;
end;

class function TGdiFontFamily.GenericMonospace: IGdiFontFamily;
var
  FontFamily: GpFontFamily;
begin
  if GenericMonospaceFontFamily <> nil then
  begin
    Result := GenericMonospaceFontFamily;
    Exit;
  end;
  GenericMonospaceFontFamily := TGdiFontFamily.Create;
  GenericMonospaceFontFamily.LastStatus := GdipGetGenericFontFamilyMonospace(FontFamily);
  GenericMonospaceFontFamily.NativeFamily := FontFamily;
  Result := GenericMonospaceFontFamily;
end;

function TGdiFontFamily.GetFamilyName(out Name: string; Language: LangId = 0): TStatus;
var
  S: array[0..LF_FACESIZE - 1] of WideChar;
begin
  Result := SetStatus(GdipGetFamilyName(FNativeFamily, @S, Language));
  Name := S;
end;

function TGdiFontFamily.Clone: IGdiFontFamily;
var
  ClonedFamily: GpFontFamily;
begin
  ClonedFamily := nil;
  SetStatus(GdipCloneFontFamily(FNativeFamily, ClonedFamily));
  Result := TGdiFontFamily.Create(ClonedFamily, FLastStatus);
end;

function TGdiFontFamily.IsAvailable: Boolean;
begin
  Result := (FNativeFamily <> nil);
end;

function TGdiFontFamily.IsStyleAvailable(Style: Integer): Boolean;
var
  Status: TStatus;
  B: BOOL;
begin
  Status := SetStatus(GdipIsStyleAvailable(FNativeFamily, Style, B));
  if Status <> Ok then
    B := False;
  Result := B;
end;

function TGdiFontFamily.GetEmHeight(Style: Integer): UInt16;
begin
  SetStatus(GdipGetEmHeight(FNativeFamily, Style, Result));
end;

function TGdiFontFamily.GetCellAscent(Style: Integer): UInt16;
begin
  SetStatus(GdipGetCellAscent(FNativeFamily, Style, Result));
end;

function TGdiFontFamily.GetCellDescent(Style: Integer): UInt16;
begin
  SetStatus(GdipGetCellDescent(FNativeFamily, Style, Result));
end;

function TGdiFontFamily.GetLineSpacing(Style: Integer): UInt16;
begin
  SetStatus(GdipGetLineSpacing(FNativeFamily, Style, Result));
end;

function TGdiFontFamily.GetNativeFamily: GpFontFamily;
begin
  Result := FNativeFamily;
end;

procedure TGdiFontFamily.SetNativeFamily(Value: GpFontFamily);
begin
  FNativeFamily := Value;
end;

constructor TGdiFontFamily.Create(Orig: GpFontFamily; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativeFamily := Orig;
end;

constructor TGdiFont.Create(DC: HDC);
var
  Font: GpFont;
begin
  inherited Create;
  Font := nil;
  FLastStatus := GdipCreateFontFromDC(DC, Font);
  SetNativeFont(Font);
end;

constructor TGdiFont.Create(DC: HDC; LogFont: PLogFontA);
var
  Font: GpFont;
begin
  inherited Create;
  Font := nil;
  if LogFont <> nil then
    FLastStatus := GdipCreateFontFromLogFontA(DC, LogFont, Font)
  else
    FLastStatus := GdipCreateFontFromDC(DC, Font);
  SetNativeFont(Font);
end;

constructor TGdiFont.Create(DC: HDC; LogFont: PLogFontW);
var
  Font: GpFont;
begin
  inherited Create;
  Font := nil;
  if LogFont <> nil then
    FLastStatus := GdipCreateFontFromLogFontW(DC, LogFont, Font)
  else
    FLastStatus := GdipCreateFontFromDC(DC, Font);
  SetNativeFont(Font);
end;

constructor TGdiFont.Create(DC: HDC; Font: HFont);
var
  NativeFont: GpFont;
  LogFont: LogFontA;
begin
  inherited Create;
  NativeFont := nil;
  if Font <> 0 then
  begin
    if Boolean(GetObjectA(Font, SizeOf(LogFontA), @LogFont)) then
      FLastStatus := GdipCreateFontFromLogFontA(DC, @LogFont, NativeFont)
    else
      FLastStatus := GdipCreateFontFromDC(DC, NativeFont);
  end
  else
    FLastStatus := GdipCreateFontFromDC(DC, NativeFont);
  SetNativeFont(NativeFont);
end;

constructor TGdiFont.Create(Family: IGdiFontFamily; EmSize: Single;
  Style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint);
var
  Font: GpFont;
  FontFamily: GpFontFamily;
begin
  inherited Create;
  Font := nil;
  if Family <> nil then
    FontFamily := Family.NativeFamily
  else
    FontFamily := nil;
  FLastStatus := GdipCreateFont(FontFamily, EmSize, Integer(Style), Integer(Unit_), Font);
  SetNativeFont(Font);
end;

constructor TGdiFont.Create(FamilyName: WideString; EmSize: Single;
  Style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint;
  FontCollection: IGdiFontCollection = nil);
var
  Family: IGdiFontFamily;
  FNativeFamily: GpFontFamily;
begin
  inherited Create;
  FNativeFont := nil;
  Family := TGdiFontFamily.Create(FamilyName, FontCollection);
  FNativeFamily := Family.NativeFamily;
  FLastStatus := Family.GetLastStatus;
  if (FLastStatus <> Ok) then
  begin
    FNativeFamily := TGdiFontFamily.GenericSansSerif.NativeFamily;
    FLastStatus := TGdiFontFamily.GenericSansSerif.LastStatus;
    if (FLastStatus <> Ok) then
      Exit;
  end;
  FLastStatus := GdipCreateFont(FNativeFamily, EmSize,
    Integer(Style), Integer(Unit_), FNativeFont);
  if FLastStatus <> Ok then
  begin
    FNativeFamily := TGdiFontFamily.GenericSansSerif.NativeFamily;
    FLastStatus := TGdiFontFamily.GenericSansSerif.LastStatus;
    if FLastStatus <> Ok then
      Exit;
    FLastStatus := GdipCreateFont(FNativeFamily, EmSize, Integer(Style),
      Integer(Unit_), FNativeFont);
  end;
end;

function TGdiFont.GetLogFontA(G: IGdiGraphics; out LogFontA: TLogFontA): TStatus;
var
  NativeGraphics: GpGraphics;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  Result := SetStatus(GdipGetLogFontA(FNativeFont, NativeGraphics, LogFontA));
end;

function TGdiFont.GetLogFontW(G: IGdiGraphics; out LogFontW: TLogFontW): TStatus;
var
  NativeGraphics: GpGraphics;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  Result := SetStatus(GdipGetLogFontW(FNativeFont, NativeGraphics, LogFontW));
end;

function TGdiFont.Clone: IGdiFont;
var
  CloneFont: GpFont;
begin
  CloneFont := nil;
  SetStatus(GdipCloneFont(FNativeFont, CloneFont));
  Result := TGdiFont.Create(CloneFont, FLastStatus);
end;

destructor TGdiFont.Destroy;
begin
  GdipDeleteFont(FNativeFont);
end;

function TGdiFont.IsAvailable: Boolean;
begin
  Result := (FNativeFont <> nil);
end;

function TGdiFont.GetStyle: Integer;
begin
  SetStatus(GdipGetFontStyle(FNativeFont, Result));
end;

function TGdiFont.GetSize: Single;
begin
  SetStatus(GdipGetFontSize(FNativeFont, Result));
end;

function TGdiFont.GetUnit: TUnit;
begin
  SetStatus(GdipGetFontUnit(FNativeFont, Result));
end;

function TGdiFont.GetHeight(Graphics: IGdiGraphics): Single;
var
  NativeGraphics: GpGraphics;
begin
  NativeGraphics := nil;
  if Graphics <> nil then
    NativeGraphics := Graphics.NativeGraphics;
  SetStatus(GdipGetFontHeight(FNativeFont, NativeGraphics, Result));
end;

function TGdiFont.GetHeight(DPI: Single): Single;
begin
  SetStatus(GdipGetFontHeightGivenDPI(FNativeFont, DPI, Result));
end;

function TGdiFont.GetFamily(Family: IGdiFontFamily): TStatus;
var
  Status: TStatus;
  nFamily: GpFontFamily;
begin
  if (Family = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Status := GdipGetFamily(FNativeFont, nFamily);
  Family.NativeFamily := nFamily;
  Family.SetStatus(Status);
  Result := SetStatus(Status);
end;

function TGdiFont.GetNativeFont: GpFont;
begin
  Result := FNativeFont;
end;

procedure TGdiFont.SetNativeFont(Font: GpFont);
begin
  FNativeFont := Font;
end;

constructor TGdiFont.Create(Font: GpFont; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  SetNativeFont(Font);
end;

constructor TGdiFontCollection.Create;
begin
  inherited Create;
  FNativeFontCollection := nil;
end;

destructor TGdiFontCollection.Destroy;
begin
  inherited Destroy;
end;

function TGdiFontCollection.GetFamilyCount: Integer;
var
  NumFound: Integer;
begin
  NumFound := 0;
  FLastStatus := GdipGetFontCollectionFamilyCount(FNativeFontCollection, NumFound);
  Result := NumFound;
end;

function TGdiFontCollection.GetFamilies(NumSought: Integer; out Families: array of IGdiFontFamily;
  out NumFound: Integer): TStatus;
var
  F: GpFontFamily;
  NativeFamilyList: Pointer;
  Status: TStatus;
  I: Integer;
type
  ArrGpFontFamily = array of GpFontFamily;
begin
  if (NumSought <= 0) or (Length(Families) = 0) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  NumFound := 0;
  GetMem(NativeFamilyList, NumSought * SizeOf(GpFontFamily));
  if NativeFamilyList = nil then
  begin
    Result := SetStatus(OutOfMemory);
    Exit;
  end;
  try
    Status := SetStatus(GdipGetFontCollectionFamilyList(
      FNativeFontCollection,
      NumSought,
      NativeFamilyList,
      NumFound));
    if Status = Ok then
      for I := 0 to NumFound - 1 do
      begin
        GdipCloneFontFamily(ArrGpFontFamily(NativeFamilyList)[I], F);
        Families[I].NativeFamily := F;
      end;
  finally
    FreeMem(NativeFamilyList);
  end;
  Result := Status;
end;

function TGdiFontCollection.GetNativeFontCollection: GpFontCollection;
begin
  Result := FNativeFontCollection;
end;

procedure TGdiFontCollection.SetNativeFontCollection(Value: GpFontCollection);
begin
  FNativeFontCollection := Value;
end;

constructor TGdiInstalledFontCollection.Create;
begin
  inherited Create;
  FNativeFontCollection := nil;
  FLastStatus := GdipNewInstalledFontCollection(FNativeFontCollection);
end;

destructor TGdiInstalledFontCollection.Destroy;
begin
  inherited Destroy;
end;

constructor TGdiPrivateFontCollection.Create;
begin
  inherited Create;
  FNativeFontCollection := nil;
  FLastStatus := GdipNewPrivateFontCollection(FNativeFontCollection);
end;

destructor TGdiPrivateFontCollection.Destroy;
begin
  GdipDeletePrivateFontCollection(FNativeFontCollection);
  inherited Destroy;
end;

function TGdiPrivateFontCollection.AddFontFile(Filename: WideString): TStatus;
begin
  Result := SetStatus(GdipPrivateAddFontFile(FNativeFontCollection, PWideChar(Filename)));
end;

function TGdiPrivateFontCollection.AddMemoryFont(Memory: Pointer; Length: Integer): TStatus;
begin
  Result := SetStatus(GdipPrivateAddMemoryFont(FNativeFontCollection,
    Memory, Length));
end;

function TGdiGraphicsPath.GetNativePath: GpPath;
begin
  Result := FNativePath;
end;

procedure TGdiGraphicsPath.SetNativePath(Path: GpPath);
begin
  FNativePath := Path;
end;

constructor TGdiGraphicsPath.Create(FillMode: TFillMode = FillModeAlternate);
begin
  inherited Create;
  FNativePath := nil;
  FLastStatus := GdipCreatePath(FillMode, FNativePath);
end;

constructor TGdiGraphicsPath.Create(Points: PPointF; Types: PByte; Count: Integer;
  FillMode: TFillMode = FillModeAlternate);
begin
  inherited Create;
  FNativePath := nil;
  FLastStatus := GdipCreatePath2(Points, Types, Count, FillMode, FNativePath);
end;

constructor TGdiGraphicsPath.Create(Points: PPointI; Types: PByte; Count: Integer;
  FillMode: TFillMode = FillModeAlternate);
begin
  inherited Create;
  FNativePath := nil;
  FLastStatus := GdipCreatePath2I(Points, Types, Count, FillMode, FNativePath);
end;

destructor TGdiGraphicsPath.Destroy;
begin
  GdipDeletePath(FNativePath);
end;

function TGdiGraphicsPath.Clone: IGdiGraphicsPath;
var
  ClonePath: GpPath;
begin
  ClonePath := nil;
  SetStatus(GdipClonePath(FNativePath, ClonePath));
  Result := TGdiGraphicsPath.Create(ClonePath) as IGdiGraphicsPath;
end;

function TGdiGraphicsPath.Reset: TStatus;
begin
  Result := SetStatus(GdiPresetPath(FNativePath));
end;

function TGdiGraphicsPath.GetFillMode: TFillMode;
var
  FMode: TFillMode;
begin
  FMode := FillModeAlternate;
  SetStatus(GdipGetPathFillMode(FNativePath, Result));
  Result := FMode;
end;

function TGdiGraphicsPath.SetFillMode(Fillmode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipSetPathFillMode(FNativePath, Fillmode));
end;

function TGdiGraphicsPath.GetPathData(var PathData: TPathData): TStatus;
var
  Count: Integer;
begin
  Count := GetPointCount;
  if Count < 0 then
  begin
    PathData.Count := 0;
    PathData.Points := nil;
    PathData.Types := nil;
    Result := FLastStatus;
  end
  else
  begin
    PathData.Count := Count;
    SetLength(PathData.Points, Count);
    SetLength(PathData.Types, Count);
    Result := SetStatus(GdipGetPathData(FNativePath, @PathData));
  end;
end;

function TGdiGraphicsPath.StartFigure: TStatus;
begin
  Result := SetStatus(GdipStartPathFigure(FNativePath));
end;

function TGdiGraphicsPath.CloseFigure: TStatus;
begin
  Result := SetStatus(GdipClosePathFigure(FNativePath));
end;

function TGdiGraphicsPath.CloseAllFigures: TStatus;
begin
  Result := SetStatus(GdipClosePathFigures(FNativePath));
end;

function TGdiGraphicsPath.SetMarker: TStatus;
begin
  Result := SetStatus(GdipSetPathMarker(FNativePath));
end;

function TGdiGraphicsPath.ClearMarkers: TStatus;
begin
  Result := SetStatus(GdipClearPathMarkers(FNativePath));
end;

function TGdiGraphicsPath.Reverse: TStatus;
begin
  Result := SetStatus(GdipReversePath(FNativePath));
end;

function TGdiGraphicsPath.GetLastPoint(out lastPoint: TPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathLastPoint(FNativePath, @lastPoint));
end;

function TGdiGraphicsPath.AddLine(const Pt1, Pt2: TPointF): TStatus;
begin
  Result := AddLine(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGdiGraphicsPath.AddLine(X1, Y1, X2, Y2: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathLine(FNativePath, X1, Y1,
    X2, Y2));
end;

function TGdiGraphicsPath.AddLines(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddLine(const Pt1, Pt2: TPointI): TStatus;
begin
  Result := AddLine(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGdiGraphicsPath.AddLine(X1, Y1, X2, Y2: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLineI(FNativePath, X1, Y1, X2, Y2));
end;

function TGdiGraphicsPath.AddLines(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2I(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddArc(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGdiGraphicsPath.AddArc(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArc(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphicsPath.AddArc(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGdiGraphicsPath.AddArc(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArcI(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphicsPath.AddBezier(Pt1, Pt2, Pt3, Pt4: TPointF): TStatus;
begin
  Result := AddBezier(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGdiGraphicsPath.AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathBezier(FNativePath, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGdiGraphicsPath.AddBeziers(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziers(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddBezier(Pt1, Pt2, Pt3, Pt4: TPointI): TStatus;
begin
  Result := AddBezier(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGdiGraphicsPath.AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBezierI(FNativePath, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGdiGraphicsPath.AddBeziers(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziersI(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddCurve(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddCurve(Points: PPointF; Count: Integer;
  Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2(FNativePath, Points, Count, Tension));
end;

function TGdiGraphicsPath.AddCurve(Points: PPointF; Count, Offset,
  NumberOfSegments: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3(FNativePath, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TGdiGraphicsPath.AddCurve(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurveI(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2I(FNativePath, Points, Count, Tension));
end;

function TGdiGraphicsPath.AddCurve(Points: PPointI; Count, Offset,
  NumberOfSegments: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3I(FNativePath, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TGdiGraphicsPath.AddClosedCurve(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddClosedCurve(Points: PPointF; Count: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2(FNativePath, Points, Count, Tension));
end;

function TGdiGraphicsPath.AddClosedCurve(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurveI(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddClosedCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2I(FNativePath, Points, Count, Tension));
end;

function TGdiGraphicsPath.AddRectangle(Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangle(FNativePath, Rect.X, Rect.Y, Rect.Width, Rect.Height));
end;

function TGdiGraphicsPath.AddRectangles(Rects: PRectF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangles(FNativePath, Rects, Count));
end;

function TGdiGraphicsPath.AddRectangle(Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangleI(FNativePath, Rect.X, Rect.Y, Rect.Width, Rect.Height));
end;

function TGdiGraphicsPath.AddRectangles(Rects: PRectI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectanglesI(FNativePath, Rects, Count));
end;

function TGdiGraphicsPath.AddEllipse(Rect: TRectF): TStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphicsPath.AddEllipse(X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipse(FNativePath, X, Y, Width, Height));
end;

function TGdiGraphicsPath.AddEllipse(Rect: TRectI): TStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGdiGraphicsPath.AddEllipse(X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipseI(FNativePath, X, Y, Width, Height));
end;

function TGdiGraphicsPath.AddPie(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddPie(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGdiGraphicsPath.AddPie(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPie(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphicsPath.AddPie(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddPie(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGdiGraphicsPath.AddPie(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPieI(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGdiGraphicsPath.AddPolygon(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygon(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddPolygon(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygonI(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.AddPath(AddingPath: IGdiGraphicsPath; Connect: Boolean): TStatus;
var
  NativePath: GpPath;
begin
  NativePath := nil;
  if AddingPath <> nil then
    NativePath := AddingPath.NativePath;
  Result := SetStatus(GdipAddPathPath(FNativePath, NativePath, Connect));
end;

function TGdiGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IGdiFontFamily; Style: Integer; EmSize: Single; Origin: TPointF;
  Format: IGdiStringFormat): TStatus;
var
  Rect: TRectF;
  NativeFontFamily: GpFontFamily;
  NativeStringFormat: GpStringFormat;
begin
  Rect.X := Origin.X;
  Rect.Y := Origin.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  NativeFontFamily := nil;
  NativeStringFormat := nil;
  if Family <> nil then
    NativeFontFamily := Family.NativeFamily;
  if Format <> nil then
    NativeStringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathString(FNativePath, PWideChar(Text), Length,
    NativeFontFamily, Style, EmSize, @Rect, NativeStringFormat));
end;

function TGdiGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IGdiFontFamily; Style: Integer; EmSize: Single; LayoutRect: TRectF;
  Format: IGdiStringFormat): TStatus;
var
  NativeFontFamily: GpFontFamily;
  NativeStringFormat: GpStringFormat;
begin
  NativeFontFamily := nil;
  NativeStringFormat := nil;
  if Family <> nil then
    NativeFontFamily := Family.NativeFamily;
  if Format <> nil then
    NativeStringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathString(FNativePath, PWideChar(Text), Length,
    NativeFontFamily, Style, EmSize, @LayoutRect, NativeStringFormat));
end;

function TGdiGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IGdiFontFamily; Style: Integer; EmSize: Single; Origin: TPointI;
  Format: IGdiStringFormat): TStatus;
var
  Rect: TRectI;
  FontFamily: GpFontFamily;
  StringFormat: GpStringFormat;
begin
  Rect.X := Origin.X;
  Rect.Y := Origin.Y;
  Rect.Width := 0;
  Rect.Height := 0;
  FontFamily := nil;
  StringFormat := nil;
  if Family <> nil then
    FontFamily := Family.NativeFamily;
  if Format <> nil then
    StringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathStringI(FNativePath, PWideChar(Text), Length, FontFamily,
    Style, EmSize, @Rect, StringFormat));
end;

function TGdiGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IGdiFontFamily; Style: Integer; EmSize: Single; LayoutRect: TRectI;
  Format: IGdiStringFormat): TStatus;
var
  FontFamily: GpFontFamily;
  StringFormat: GpStringFormat;
begin
  FontFamily := nil;
  StringFormat := nil;
  if Family <> nil then
    FontFamily := Family.NativeFamily;
  if Format <> nil then
    StringFormat := Format.NativeFormat;
  Result := SetStatus(GdipAddPathStringI(FNativePath, PWideChar(Text), Length, FontFamily,
    Style, EmSize, @LayoutRect, StringFormat));
end;

function TGdiGraphicsPath.Transform(Matrix: IGdiMatrix): TStatus;
begin
  if Matrix <> nil then
    Result := SetStatus(GdipTransformPath(FNativePath, Matrix.NativeMatrix))
  else
    Result := Ok;
end;

function TGdiGraphicsPath.GetBounds(out Bounds: TRectF; Matrix: IGdiMatrix = nil; Pen: IGdiPen = nil): TStatus;
var
  NativeMatrix: GpMatrix;
  NativePen: GpPen;
begin
  NativeMatrix := nil;
  NativePen := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipGetPathWorldBounds(FNativePath, @Bounds, NativeMatrix, NativePen));
end;

function TGdiGraphicsPath.GetBounds(out Bounds: TRectI; Matrix: IGdiMatrix = nil; Pen: IGdiPen = nil): TStatus;
var
  NativeMatrix: GpMatrix;
  NativePen: GpPen;
begin
  NativeMatrix := nil;
  NativePen := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipGetPathWorldBoundsI(FNativePath, @Bounds, NativeMatrix, NativePen));
end;

function TGdiGraphicsPath.Flatten(Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
var
  NativeMatrix: GpMatrix;
begin
  NativeMatrix := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipFlattenPath(FNativePath, NativeMatrix, Flatness));
end;

function TGdiGraphicsPath.Widen(Pen: IGdiPen; Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
var
  NativeMatrix: GpMatrix;
begin
  NativeMatrix := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipWidenPath(FNativePath, Pen.NativePen, NativeMatrix, Flatness));
end;

function TGdiGraphicsPath.Outline(Matrix: IGdiMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
var
  NativeMatrix: GpMatrix;
begin
  NativeMatrix := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipWindingModeOutline(FNativePath, NativeMatrix, Flatness));
end;

function TGdiGraphicsPath.Warp(DestPoints: PPointF; Count: Integer; SrcRect: TRectF;
  Matrix: IGdiMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
  Flatness: Single = FlatnessDefault): TStatus;
var
  NativeMatrix: GpMatrix;
begin
  NativeMatrix := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipWarpPath(FNativePath, NativeMatrix, DestPoints,
    Count, SrcRect.X, SrcRect.Y, SrcRect.Width, SrcRect.Height,
    warpMode, Flatness));
end;

function TGdiGraphicsPath.GetPointCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPointCount(FNativePath, Count));
  Result := Count;
end;

function TGdiGraphicsPath.GetPathTypes(Types: PByte; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathTypes(FNativePath, Types, Count));
end;

function TGdiGraphicsPath.GetPathPoints(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPoints(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.GetPathPoints(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPointsI(FNativePath, Points, Count));
end;

function TGdiGraphicsPath.IsVisible(Point: TPointF; G: IGdiGraphics = nil): Boolean;
begin
  Result := IsVisible(Point.X, Point.Y, G);
end;

function TGdiGraphicsPath.IsVisible(X, Y: Single; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisiblePathPoint(FNativePath, X, Y, NativeGraphics, B));
  Result := B;
end;

function TGdiGraphicsPath.IsVisible(Point: TPointI; G: IGdiGraphics = nil): Boolean;
begin
  Result := IsVisible(Point.X, Point.Y, G);
end;

function TGdiGraphicsPath.IsVisible(X, Y: Integer; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  B: BOOL;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  B := False;
  SetStatus(GdipIsVisiblePathPointI(FNativePath, X, Y, NativeGraphics, B));
  Result := B;
end;

function TGdiGraphicsPath.IsOutlineVisible(Point: TPointF; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean;
begin
  Result := IsOutlineVisible(Point.X, Point.Y, Pen, G);
end;

function TGdiGraphicsPath.IsOutlineVisible(X, Y: Single; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  NativePen: GpPen;
  B: BOOL;
begin
  NativeGraphics := nil;
  NativePen := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  B := False;
  SetStatus(GdipIsOutlineVisiblePathPoint(FNativePath, X, Y, NativePen,
    NativeGraphics, B));
  Result := B;
end;

function TGdiGraphicsPath.IsOutlineVisible(Point: TPointI; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean;
begin
  Result := IsOutlineVisible(Point.X, Point.Y, Pen, G);
end;

function TGdiGraphicsPath.IsOutlineVisible(X, Y: Integer; Pen: IGdiPen; G: IGdiGraphics = nil): Boolean;
var
  NativeGraphics: GpGraphics;
  NativePen: GpPen;
  B: BOOL;
begin
  NativeGraphics := nil;
  NativePen := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  B := False;
  SetStatus(GdipIsOutlineVisiblePathPointI(FNativePath, X, Y, NativePen,
    NativeGraphics, B));
  Result := B;
end;

constructor TGdiGraphicsPath.Create(Path: IGdiGraphicsPath);
var
  ClonePath: GpPath;
begin
  inherited Create;
  ClonePath := nil;
  SetStatus(GdipClonePath(Path.NativePath, ClonePath));
  SetNativePath(ClonePath);
end;

constructor TGdiGraphicsPath.Create(Path: GpPath);
begin
  inherited Create;
  FLastStatus := Ok;
  FNativePath := Path;
end;

{ TGdiGraphicsPathIterator }

constructor TGdiGraphicsPathIterator.Create(Path: IGdiGraphicsPath);
var
  NativePath: GpPath;
  Iter: GpPathIterator;
begin
  inherited Create;
  NativePath := nil;
  if NativePath <> nil then
    NativePath := Path.NativePath;
  Iter := nil;
  FLastStatus := GdipCreatePathIter(Iter, NativePath);
  SetNativeIterator(Iter);
end;

destructor TGdiGraphicsPathIterator.Destroy;
begin
  GdipDeletePathIter(FNativeIterator);
end;

function TGdiGraphicsPathIterator.NextSubPath(out StartIndex, EndIndex: Integer; out IsClosed: Boolean): Integer;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipPathIterNextSubPath(FNativeIterator, Result, StartIndex, EndIndex, B));
  IsClosed := B;
end;

function TGdiGraphicsPathIterator.NextSubPath(Path: IGdiGraphicsPath; out IsClosed: Boolean): Integer;
var
  NativePath: GpPath;
  B: BOOL;
begin
  NativePath := nil;
  if Path <> nil then
    NativePath := Path.NativePath;
  B := False;
  SetStatus(GdipPathIterNextSubPathPath(FNativeIterator, Result, NativePath, B));
  IsClosed := B;
end;

function TGdiGraphicsPathIterator.NextPathType(out PathType: TPathPointType; out StartIndex, EndIndex: Integer): Integer;
var
  ResultCount: Integer;
begin
  SetStatus(GdipPathIterNextPathType(FNativeIterator, ResultCount, @PathType,
    StartIndex, EndIndex));
  Result := ResultCount;
end;

function TGdiGraphicsPathIterator.NextMarker(out StartIndex, EndIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterNextMarker(FNativeIterator, Result, StartIndex, EndIndex));
end;

function TGdiGraphicsPathIterator.NextMarker(Path: IGdiGraphicsPath): Integer;
var
  FNativePath: GpPath;
begin
  FNativePath := nil;
  if Assigned(Path) then
    FNativePath := Path.NativePath;
  SetStatus(GdipPathIterNextMarkerPath(FNativeIterator, Result, FNativePath));
end;

function TGdiGraphicsPathIterator.GetCount: Integer;
begin
  SetStatus(GdipPathIterGetCount(FNativeIterator, Result));
end;

function TGdiGraphicsPathIterator.GetSubPathCount: Integer;
begin
  SetStatus(GdipPathIterGetSubPathCount(FNativeIterator, Result));
end;

function TGdiGraphicsPathIterator.HasCurve: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipPathIterHasCurve(FNativeIterator, B));
  Result := B;
end;

procedure TGdiGraphicsPathIterator.Rewind;
begin
  SetStatus(GdipPathIterRewind(FNativeIterator));
end;

function TGdiGraphicsPathIterator.Enumerate(Points: PPointF; Types: PByte;
  Count: Integer): Integer;
begin
  SetStatus(GdipPathIterEnumerate(FNativeIterator, Result, Points, Types, Count));
end;

function TGdiGraphicsPathIterator.CopyData(Points: PPointF; Types: PByte;
  StartIndex, EndIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterCopyData(FNativeIterator, Result, Points, Types,
    StartIndex, EndIndex));
end;

procedure TGdiGraphicsPathIterator.SetNativeIterator(FNativeIterator: GpPathIterator);
begin
  Self.FNativeIterator := FNativeIterator;
end;

constructor TGdiGradientBrush.Create(Points: PPointF; Count: Integer; WrapMode: TWrapMode = WrapModeClamp);
var
  Brush: GpPathGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreatePathGradient(Points, Count, WrapMode, Brush);
  SetNativeBrush(Brush);
end;

{ TGdiGradientBrush }

constructor TGdiGradientBrush.Create(Points: PPointI; Count: Integer; WrapMode: TWrapMode = WrapModeClamp);
var
  Brush: GpPathGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreatePathGradientI(Points, Count, WrapMode, Brush);
  SetNativeBrush(Brush);
end;

constructor TGdiGradientBrush.Create(Path: IGdiGraphicsPath);
var
  Brush: GpPathGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreatePathGradientFromPath(Path.NativePath, Brush);
  SetNativeBrush(Brush);
end;

function TGdiGradientBrush.GetCenterColor(out Color: TArgb): TStatus;
begin
  SetStatus(GdipGetPathGradientCenterColor(GpPathGradient(FNativeBrush), Color));
  Result := FLastStatus;
end;

function TGdiGradientBrush.SetCenterColor(Color: TArgb): TStatus;
begin
  SetStatus(GdipSetPathGradientCenterColor(GpPathGradient(FNativeBrush), Color));
  Result := FLastStatus;
end;

function TGdiGradientBrush.GetPointCount: Integer;
begin
  SetStatus(GdipGetPathGradientPointCount(GpPathGradient(FNativeBrush), Result));
end;

function TGdiGradientBrush.GetSurrounDColorCount: Integer;
begin
  SetStatus(GdipGetPathGradientSurrounDColorCount(GpPathGradient(FNativeBrush), Result));
end;

function TGdiGradientBrush.GetSurrounDColors(Colors: PArgb; var Count: Integer): TStatus;
var
  C: Integer;
begin
  if Colors = nil then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  SetStatus(GdipGetPathGradientSurrounDColorCount(GpPathGradient(FNativeBrush), C));
  if FLastStatus <> Ok then
  begin
    Result := FLastStatus;
    Exit;
  end;
  if (Count < C) or (C <= 0) then
  begin
    Result := SetStatus(InsufficientBuffer);
    Exit;
  end;
  SetStatus(GdipGetPathGradientSurrounDColorsWithCount(GpPathGradient(FNativeBrush), Colors, C));
  if FLastStatus = Ok then
    Count := C;
  Result := FLastStatus;
end;

function TGdiGradientBrush.SetSurrounDColors(Colors: PArgb; var Count: Integer): TStatus;
var
  C: Integer;
begin
  if Colors = nil then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  C := GetPointCount;
  if (Count > C) or (C <= 0) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  C := Count;
  SetStatus(GdipSetPathGradientSurrounDColorsWithCount(
    GpPathGradient(FNativeBrush), Colors, C));
  if FLastStatus = Ok then
    Count := C;
  Result := FLastStatus;
end;

function TGdiGradientBrush.GeTGdiGraphicsPath(Path: IGdiGraphicsPath): TStatus;
begin
  if Path = nil then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipGetPathGradientPath(GpPathGradient(FNativeBrush), Path.NativePath));
end;

function TGdiGradientBrush.SeTGdiGraphicsPath(Path: IGdiGraphicsPath): TStatus;
begin
  if Path = nil then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipSetPathGradientPath(GpPathGradient(FNativeBrush), Path.NativePath));
end;

function TGdiGradientBrush.GetCenterPoint(out Point: TPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPoint(GpPathGradient(FNativeBrush), @Point));
end;

function TGdiGradientBrush.GetCenterPoint(out Point: TPointI): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPointI(GpPathGradient(FNativeBrush), @Point));
end;

function TGdiGradientBrush.SetCenterPoint(Point: TPointF): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPoint(GpPathGradient(FNativeBrush), @Point));
end;

function TGdiGradientBrush.SetCenterPoint(Point: TPointI): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPointI(GpPathGradient(FNativeBrush), @Point));
end;

function TGdiGradientBrush.GetRectangle(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRect(GpPathGradient(FNativeBrush), @Rect));
end;

function TGdiGradientBrush.GetRectangle(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRectI(GpPathGradient(FNativeBrush), @Rect));
end;

function TGdiGradientBrush.SetGammaCorrection(UseGammaCorrection: Boolean): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientGammaCorrection(GpPathGradient(FNativeBrush),
    UseGammaCorrection));
end;

function TGdiGradientBrush.GetGammaCorrection: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipGetPathGradientGammaCorrection(GpPathGradient(FNativeBrush), B));
  Result := B;
end;

function TGdiGradientBrush.GetBlendCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientBlendCount(GpPathGradient(FNativeBrush), Count));
  Result := Count;
end;

function TGdiGradientBrush.GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientBlend(
    GpPathGradient(FNativeBrush),
    BlendFactors, BlendPositions, Count));
end;

function TGdiGradientBrush.SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientBlend(
    GpPathGradient(FNativeBrush), BlendFactors, BlendPositions, Count));
end;

function TGdiGradientBrush.GetInterpolationColorCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientPresetBlendCount(GpPathGradient(FNativeBrush), Count));
  Result := Count;
end;

function TGdiGradientBrush.SetInterpolationColors(PresetColors: PArgb;
  BlendPositions: PSingle; Count: Integer): TStatus;
var
  Status: TStatus;
begin
  if (Count <= 0) or (PresetColors = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Status := SetStatus(GdipSetPathGradientPresetBlend(GpPathGradient(FNativeBrush),
    PresetColors, BlendPositions, Count));
  Result := Status;
end;

function TGdiGradientBrush.GetInterpolationColors(PresetColors: PArgb;
  BlendPositions: PSingle; Count: Integer): TStatus;
var
  Status: GpStatus;
  Argbs, A, B: PArgb;
  I: Integer;
begin
  if (Count <= 0) or (PresetColors = nil) then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  GetMem(Argbs, Count * SizeOf(ARGB));
  if Argbs = nil then
  begin
    Result := SetStatus(OutOfMemory);
    Exit;
  end;
  try
    Status := SetStatus(GdipGetPathGradientPresetBlend(FNativeBrush, Argbs,
      BlendPositions, Count));
    A := PresetColors;
    B := Argbs;
    for I := 0 to Count - 1 do
    begin
      A^ := B^;
      Inc(A);
      Inc(B);
    end;
  finally
    FreeMem(Argbs);
  end;
  Result := Status;
end;

function TGdiGradientBrush.SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientSigmaBlend(GpPathGradient(FNativeBrush), Focus, Scale));
end;

function TGdiGradientBrush.SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientLinearBlend(GpPathGradient(FNativeBrush), Focus, Scale));
end;

function TGdiGradientBrush.GetTransform: IGdiMatrix;
begin
  Result := NewMatrix;
  SetStatus(GdipGetPathGradientTransform(GpPathGradient(FNativeBrush), Result.NativeMatrix));
end;

procedure TGdiGradientBrush.SetTransform(Matrix: IGdiMatrix);
begin
  SetStatus(GdipSetPathGradientTransform(
    GpPathGradient(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TGdiGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetPathGradientTransform(
    GpPathGradient(FNativeBrush)));
end;

function TGdiGradientBrush.MultiplyTransform(Matrix: IGdiMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPathGradientTransform(
    GpPathGradient(FNativeBrush), Matrix.NativeMatrix, Order));
end;

function TGdiGradientBrush.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePathGradientTransform(
    GpPathGradient(FNativeBrush), DX, DY, Order));
end;

function TGdiGradientBrush.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePathGradientTransform(
    GpPathGradient(FNativeBrush), SX, SY, Order));
end;

function TGdiGradientBrush.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePathGradientTransform(
    GpPathGradient(FNativeBrush), Angle, Order));
end;

function TGdiGradientBrush.GetFocusScales(out XScale, YScale: Single): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientFocusScales(
    GpPathGradient(FNativeBrush), XScale, YScale));
end;

function TGdiGradientBrush.SetFocusScales(XScale, YScale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientFocusScales(
    GpPathGradient(FNativeBrush), XScale, YScale));
end;

function TGdiGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetPathGradientWrapMode(GpPathGradient(FNativeBrush), Result));
end;

function TGdiGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientWrapMode(
    GpPathGradient(FNativeBrush), WrapMode));
end;

constructor TGdiGradientBrush.Create;
begin
  inherited Create;
end;

{ Value type creation routines }

function NewPointI(X, Y: Integer): TPointI;
begin
  Result.X := X;
  Result.Y := Y;
end;

function NewPointF(X, Y: Single): TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function NewSizeI(Width, Height: Integer): TSizeI;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

function NewSizeI(const Size: TSizeF): TSizeI;
begin
  with Size do
  begin
    Result.Width := Round(Width);
    Result.Height := Round(Height);
  end;
end;

function NewSizeF(Width, Height: Single): TSizeF;
begin
  Result.Width := Width;
  Result.Height := Height;
end;

function NewSizeF(const Size: TSizeI): TSizeF;
begin
  with Size do
  begin
    Result.Width := Width;
    Result.Height := Height;
  end;
end;

function NewRectI(X, Y, Width, Height: Integer): TRectI;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := Width;
  Result.Height := Height;
end;

function NewRectI(Width, Height: Integer): TRectI;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

function NewRectI(const R: TRect): TRectI;
begin
  Result.X := R.Left;
  Result.Y := R.Top;
  Result.Width := R.Right - R.Left;
  Result.Height := R.Bottom - R.Top;
end;

function NewRectF(X, Y, Width, Height: Single): TRectF; overload;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := Width;
  Result.Height := Height;
end;

function NewRectF(Width, Height: Single): TRectF; overload;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

function NewRectF(const R: TRect): TRectF;
begin
  Result.X := R.Left;
  Result.Y := R.Top;
  Result.Width := R.Right - R.Left;
  Result.Height := R.Bottom - R.Top;
end;

function NewRectF(const R: TRectI): TRectF;
begin
  with R do
  begin
    Result.X := X;
    Result.Y := Y;
    Result.Width := Width;
    Result.Height := Height;
  end;
end;

{ Instance type creation routines }

function NewGraphics(DC: HDC): IGdiGraphics;
begin
  Result := TGdiGraphics.Create(DC);
  Result.SetSmoothingMode(SmoothingModeAntiAlias);
end;

function NewGraphics(Image: IGdiImage): IGdiGraphics;
begin
  Result := TGdiGraphics.Create(Image);
  Result.SetSmoothingMode(SmoothingModeAntiAlias);
end;

function NewGraphics(Width, Height: Integer): IGdiGraphics; overload;
begin
  if (Width > 0) and (Height > 0) then
  begin
    Result := TGdiGraphics.Create(Width, Height);
    Result.SetSmoothingMode(SmoothingModeAntiAlias);
    Result.Clear(0);
  end
  else
    Result := nil;
end;

function NewPen(RGBA: TArgb; StrokeWidth: Single = 1): IGdiPen;
begin
  Result := TGdiPen.Create(TArgb(RGBA), StrokeWidth);
end;

function NewSolidBrush(RGBA: TArgb): IGdiSolidBrush;
begin
  Result := TGdiSolidBrush.Create(TArgb(RGBA));
end;

function NewCheckerBrush(C1, C2: TArgb; Size: Integer): IGdiTextureBrush;
var
  G: IGdiGraphics;
  B: IGdiBitmap;
  F: IGdiSolidBrush;
  R: TRectF;
begin
  B := TGdiBitmap.Create(Size * 2, Size * 2);
  G := TGdiGraphics.Create(B);
  F := TGdiSolidBrush.Create(C1);
  R.X := 0;
  R.Y := 0;
  R.Width := Size * 2;
  R.Height := Size * 2;
  G.FillRectangle(F, R);
  F := TGdiSolidBrush.Create(C2);
  R.Y := Size;
  R.Width := R.Y;
  R.Height := R.Y;
  G.FillRectangle(F, R);
  R.X := R.Y;
  R.Y := 0;
  G.FillRectangle(F, R);
  Result := TTextureBrush.Create(B);
end;

function NewHatchBrush(Style: THatchStyle; ForeColor: TArgb): IGdiHatchBrush;
begin
  Result := TGdiHatchBrush.Create(Style, ForeColor, 0);
end;

function NewHatchBrush(Style: THatchStyle; ForeColor: TArgb; BackColor: TArgb): IGdiHatchBrush;
begin
  Result := TGdiHatchBrush.Create(Style, ForeColor, BackColor);
end;

function NewLinearGradientBrush(const Rect: TRectI; Angle: Single;
  C1, C2: TArgb): IGdiLinearGradientBrush;
begin
  Result := TGdiLinearGradientBrush.Create(Rect, C1, C2, Angle);
end;

function NewLinearGradientBrush(const Rect: TRectF; Angle: Single;
  C1, C2: TArgb): IGdiLinearGradientBrush;
begin
  Result := TGdiLinearGradientBrush.Create(Rect, C1, C2, Angle);
end;

function NewLinearGradientBrush(const Rect: TRectF; Angle: Single;
  Colors: array of TArgb; Stops: array of Single): IGdiLinearGradientBrush; overload;
begin
  Result := TGdiLinearGradientBrush.Create(Rect, $FF000000, $FF000000, Angle);
  if (Length(Colors) > 0) and (Length(Colors) = Length(Stops)) then
    Result.SetInterpolationColors(@Colors[0], @Stops[0], Length(Colors));
end;

function NewFont(Font: HFont): IGdiFont;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    Result := TGdiFont.Create(DC, Font);
  finally
    ReleaseDC(0, DC);
  end;
end;

function NewFont(DC: HDC; Font: HFont): IGdiFont;
begin
  Result := TGdiFont.Create(DC, Font);
end;

function NewFont(const Name: string; Size: Single; Style: TFontStyle; Unit_: TUnit = UnitPoint): IGdiFont;
begin
  Result := TGdiFont.Create(Name, Size, Style, Unit_);
end;

function NewStringFormat: IGdiStringFormat;
begin
  Result := TGdiStringFormat.Create;
end;

function NewStringFormat(HAlign, VAlign: TStringAlignment; Wrap: Boolean = False): IGdiStringFormat;
var
  Format: Integer;
begin
  if Wrap then
    Format := StringFormatFlagsNoWrap
  else
    Format := 0;
  Result := TGdiStringFormat.Create(Format);
  Result.SetAlignment(HAlign);
  Result.SetLineAlignment(VAlign);
  if not Wrap then
    Result.SetTrimming(StringTrimmingEllipsisCharacter);
end;

function NewBitmap(Width, Height: Integer): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Width, Height);
end;

function NewBitmap(Width, Height: Integer; PixelFormat: Integer): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Width, Height, PixelFormat);
end;

function NewBitmap(Width, Height, Stride: Integer; PixelFormat: TPixelFormat; ScanLine: PByte): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Width, Height, Stride, PixelFormat, ScanLine);
end;

function NewBitmap(Width, Height: Integer; Target: IGdiGraphics): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Width, Height, Target);
end;

function NewBitmap(Stream: IStream): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Stream);
end;

function NewBitmap(const FileName: string): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(FileName);
end;

function NewBitmap(Bitmap: HBITMAP): IGdiBitmap; overload;
begin
  Result := TGdiBitmap.Create(Bitmap, 0);
end;

function NewImageAttributes: IGdiImageAttributes;
begin
  Result := TGdiImageAttributes.Create;
end;

function NewImageAttributes(Transform: TColorTransform): IGdiImageAttributes;
begin
  Result := TGdiImageAttributes.Create;
  Result.SetColorMatrix(NewColorMatrix(Transform));
  if Transform.Gamma <> 1 then
    Result.SetGamma(Transform.Gamma);
end;

function NewGraphicsPath: IGdiGraphicsPath;
begin
  Result := TGdiGraphicsPath.Create;
end;

function NewMatrix: IGdiMatrix;
begin
  Result := TGdiMatrix.Create;
end;

const
  GdiPlusLib = 'gdiplus.dll';

function GdipAlloc; external GdiPlusLib;
procedure GdipFree; external GdiPlusLib;
function GdiplusStartup; external GdiPlusLib;
procedure GdiplusShutdown; external GdiPlusLib;
function GdipCreatePath; external GdiPlusLib;
function GdipCreatePath2; external GdiPlusLib;
function GdipCreatePath2I; external GdiPlusLib;
function GdipClonePath; external GdiPlusLib;
function GdipDeletePath; external GdiPlusLib;
function GdipResetPath; external GdiPlusLib;
function GdipGetPointCount; external GdiPlusLib;
function GdipGetPathTypes; external GdiPlusLib;
function GdipGetPathPoints; external GdiPlusLib;
function GdipGetPathPointsI; external GdiPlusLib;
function GdipGetPathFillMode; external GdiPlusLib;
function GdipSetPathFillMode; external GdiPlusLib;
function GdipGetPathData; external GdiPlusLib;
function GdipStartPathFigure; external GdiPlusLib;
function GdipClosePathFigure; external GdiPlusLib;
function GdipClosePathFigures; external GdiPlusLib;
function GdipSetPathMarker; external GdiPlusLib;
function GdipClearPathMarkers; external GdiPlusLib;
function GdipReversePath; external GdiPlusLib;
function GdipGetPathLastPoint; external GdiPlusLib;
function GdipAddPathLine; external GdiPlusLib;
function GdipAddPathLine2; external GdiPlusLib;
function GdipAddPathArc; external GdiPlusLib;
function GdipAddPathBezier; external GdiPlusLib;
function GdipAddPathBeziers; external GdiPlusLib;
function GdipAddPathCurve; external GdiPlusLib;
function GdipAddPathCurve2; external GdiPlusLib;
function GdipAddPathCurve3; external GdiPlusLib;
function GdipAddPathClosedCurve; external GdiPlusLib;
function GdipAddPathClosedCurve2; external GdiPlusLib;
function GdipAddPathRectangle; external GdiPlusLib;
function GdipAddPathRectangles; external GdiPlusLib;
function GdipAddPathEllipse; external GdiPlusLib;
function GdipAddPathPie; external GdiPlusLib;
function GdipAddPathPolygon; external GdiPlusLib;
function GdipAddPathPath; external GdiPlusLib;
function GdipAddPathString; external GdiPlusLib;
function GdipAddPathStringI; external GdiPlusLib;
function GdipAddPathLineI; external GdiPlusLib;
function GdipAddPathLine2I; external GdiPlusLib;
function GdipAddPathArcI; external GdiPlusLib;
function GdipAddPathBezierI; external GdiPlusLib;
function GdipAddPathBeziersI; external GdiPlusLib;
function GdipAddPathCurveI; external GdiPlusLib;
function GdipAddPathCurve2I; external GdiPlusLib;
function GdipAddPathCurve3I; external GdiPlusLib;
function GdipAddPathClosedCurveI; external GdiPlusLib;
function GdipAddPathClosedCurve2I; external GdiPlusLib;
function GdipAddPathRectangleI; external GdiPlusLib;
function GdipAddPathRectanglesI; external GdiPlusLib;
function GdipAddPathEllipseI; external GdiPlusLib;
function GdipAddPathPieI; external GdiPlusLib;
function GdipAddPathPolygonI; external GdiPlusLib;
function GdipFlattenPath; external GdiPlusLib;
function GdipWindingModeOutline; external GdiPlusLib;
function GdipWidenPath; external GdiPlusLib;
function GdipWarpPath; external GdiPlusLib;
function GdipTransformPath; external GdiPlusLib;
function GdipGetPathWorldBounds; external GdiPlusLib;
function GdipGetPathWorldBoundsI; external GdiPlusLib;
function GdipIsVisiblePathPoint; external GdiPlusLib;
function GdipIsVisiblePathPointI; external GdiPlusLib;
function GdipIsOutlineVisiblePathPoint; external GdiPlusLib;
function GdipIsOutlineVisiblePathPointI; external GdiPlusLib;
function GdipCreatePathIter; external GdiPlusLib;
function GdipDeletePathIter; external GdiPlusLib;
function GdipPathIterNextSubpath; external GdiPlusLib;
function GdipPathIterNextSubpathPath; external GdiPlusLib;
function GdipPathIterNextPathType; external GdiPlusLib;
function GdipPathIterNextMarker; external GdiPlusLib;
function GdipPathIterNextMarkerPath; external GdiPlusLib;
function GdipPathIterGetCount; external GdiPlusLib;
function GdipPathIterGetSubpathCount; external GdiPlusLib;
function GdipPathIterIsValid; external GdiPlusLib;
function GdipPathIterHasCurve; external GdiPlusLib;
function GdipPathIterRewind; external GdiPlusLib;
function GdipPathIterEnumerate; external GdiPlusLib;
function GdipPathIterCopyData; external GdiPlusLib;
function GdipCreateMatrix; external GdiPlusLib;
function GdipCreateMatrix2; external GdiPlusLib;
function GdipCreateMatrix3; external GdiPlusLib;
function GdipCreateMatrix3I; external GdiPlusLib;
function GdipCloneMatrix; external GdiPlusLib;
function GdipDeleteMatrix; external GdiPlusLib;
function GdipSetMatrixElements; external GdiPlusLib;
function GdipMultiplyMatrix; external GdiPlusLib;
function GdipTranslateMatrix; external GdiPlusLib;
function GdipScaleMatrix; external GdiPlusLib;
function GdipRotateMatrix; external GdiPlusLib;
function GdipShearMatrix; external GdiPlusLib;
function GdipInvertMatrix; external GdiPlusLib;
function GdipTransformMatrixPoints; external GdiPlusLib;
function GdipTransformMatrixPointsI; external GdiPlusLib;
function GdipVectorTransformMatrixPoints; external GdiPlusLib;
function GdipVectorTransformMatrixPointsI; external GdiPlusLib;
function GdipGetMatrixElements; external GdiPlusLib;
function GdipIsMatrixInvertible; external GdiPlusLib;
function GdipIsMatrixIdentity; external GdiPlusLib;
function GdipIsMatrixEqual; external GdiPlusLib;
function GdipCreateRegion; external GdiPlusLib;
function GdipCreateRegionRect; external GdiPlusLib;
function GdipCreateRegionRectI; external GdiPlusLib;
function GdipCreateRegionPath; external GdiPlusLib;
function GdipCreateRegionRgnData; external GdiPlusLib;
function GdipCreateRegionHrgn; external GdiPlusLib;
function GdipCloneRegion; external GdiPlusLib;
function GdipDeleteRegion; external GdiPlusLib;
function GdipSetInfinite; external GdiPlusLib;
function GdipSetEmpty; external GdiPlusLib;
function GdipCombineRegionRect; external GdiPlusLib;
function GdipCombineRegionRectI; external GdiPlusLib;
function GdipCombineRegionPath; external GdiPlusLib;
function GdipCombineRegionRegion; external GdiPlusLib;
function GdipTranslateRegion; external GdiPlusLib;
function GdipTranslateRegionI; external GdiPlusLib;
function GdipTransformRegion; external GdiPlusLib;
function GdipGetRegionBounds; external GdiPlusLib;
function GdipGetRegionBoundsI; external GdiPlusLib;
function GdipGetRegionHRgn; external GdiPlusLib;
function GdipIsEmptyRegion; external GdiPlusLib;
function GdipIsInfiniteRegion; external GdiPlusLib;
function GdipIsEqualRegion; external GdiPlusLib;
function GdipGetRegionDataSize; external GdiPlusLib;
function GdipGetRegionData; external GdiPlusLib;
function GdipIsVisibleRegionPoint; external GdiPlusLib;
function GdipIsVisibleRegionPointI; external GdiPlusLib;
function GdipIsVisibleRegionRect; external GdiPlusLib;
function GdipIsVisibleRegionRectI; external GdiPlusLib;
function GdipGetRegionScansCount; external GdiPlusLib;
function GdipGetRegionScans; external GdiPlusLib;
function GdipGetRegionScansI; external GdiPlusLib;
function GdipCloneBrush; external GdiPlusLib;
function GdipDeleteBrush; external GdiPlusLib;
function GdipGetBrushType; external GdiPlusLib;
function GdipCreateHatchBrush; external GdiPlusLib;
function GdipGetHatchStyle; external GdiPlusLib;
function GdipGetHatchForegroundColor; external GdiPlusLib;
function GdipGetHatchBackgroundColor; external GdiPlusLib;
function GdipCreateTexture; external GdiPlusLib;
function GdipCreateTexture2; external GdiPlusLib;
function GdipCreateTextureIA; external GdiPlusLib;
function GdipCreateTexture2I; external GdiPlusLib;
function GdipCreateTextureIAI; external GdiPlusLib;
function GdipGetTextureTransform; external GdiPlusLib;
function GdipSetTextureTransform; external GdiPlusLib;
function GdipResetTextureTransform; external GdiPlusLib;
function GdipMultiplyTextureTransform; external GdiPlusLib;
function GdipTranslateTextureTransform; external GdiPlusLib;
function GdipScaleTextureTransform; external GdiPlusLib;
function GdipRotateTextureTransform; external GdiPlusLib;
function GdipSetTextureWrapMode; external GdiPlusLib;
function GdipGetTextureWrapMode; external GdiPlusLib;
function GdipGetTextureImage; external GdiPlusLib;
function GdipCreateSolidFill; external GdiPlusLib;
function GdipSetSolidFillColor; external GdiPlusLib;
function GdipGetSolidFillColor; external GdiPlusLib;
function GdipCreateLineBrush; external GdiPlusLib;
function GdipCreateLineBrushI; external GdiPlusLib;
function GdipCreateLineBrushFromRect; external GdiPlusLib;
function GdipCreateLineBrushFromRectI; external GdiPlusLib;
function GdipCreateLineBrushFromRectWithAngle; external GdiPlusLib;
function GdipCreateLineBrushFromRectWithAngleI; external GdiPlusLib;
function GdipSetLineColors; external GdiPlusLib;
function GdipGetLineColors; external GdiPlusLib;
function GdipGetLineRect; external GdiPlusLib;
function GdipGetLineRectI; external GdiPlusLib;
function GdipSetLineGammaCorrection; external GdiPlusLib;
function GdipGetLineGammaCorrection; external GdiPlusLib;
function GdipGetLineBlendCount; external GdiPlusLib;
function GdipGetLineBlend; external GdiPlusLib;
function GdipSetLineBlend; external GdiPlusLib;
function GdipGetLinePresetBlendCount; external GdiPlusLib;
function GdipGetLinePresetBlend; external GdiPlusLib;
function GdipSetLinePresetBlend; external GdiPlusLib;
function GdipSetLineSigmaBlend; external GdiPlusLib;
function GdipSetLineLinearBlend; external GdiPlusLib;
function GdipSetLineWrapMode; external GdiPlusLib;
function GdipGetLineWrapMode; external GdiPlusLib;
function GdipGetLineTransform; external GdiPlusLib;
function GdipSetLineTransform; external GdiPlusLib;
function GdipResetLineTransform; external GdiPlusLib;
function GdipMultiplyLineTransform; external GdiPlusLib;
function GdipTranslateLineTransform; external GdiPlusLib;
function GdipScaleLineTransform; external GdiPlusLib;
function GdipRotateLineTransform; external GdiPlusLib;
function GdipCreatePathGradient; external GdiPlusLib;
function GdipCreatePathGradientI; external GdiPlusLib;
function GdipCreatePathGradientFromPath; external GdiPlusLib;
function GdipGetPathGradientCenterColor; external GdiPlusLib;
function GdipSetPathGradientCenterColor; external GdiPlusLib;
function GdipGetPathGradientSurroundColorsWithCount; external GdiPlusLib;
function GdipSetPathGradientSurroundColorsWithCount; external GdiPlusLib;
function GdipGetPathGradientPath; external GdiPlusLib;
function GdipSetPathGradientPath; external GdiPlusLib;
function GdipGetPathGradientCenterPoint; external GdiPlusLib;
function GdipGetPathGradientCenterPointI; external GdiPlusLib;
function GdipSetPathGradientCenterPoint; external GdiPlusLib;
function GdipSetPathGradientCenterPointI; external GdiPlusLib;
function GdipGetPathGradientRect; external GdiPlusLib;
function GdipGetPathGradientRectI; external GdiPlusLib;
function GdipGetPathGradientPointCount; external GdiPlusLib;
function GdipGetPathGradientSurroundColorCount; external GdiPlusLib;
function GdipSetPathGradientGammaCorrection; external GdiPlusLib;
function GdipGetPathGradientGammaCorrection; external GdiPlusLib;
function GdipGetPathGradientBlendCount; external GdiPlusLib;
function GdipGetPathGradientBlend; external GdiPlusLib;
function GdipSetPathGradientBlend; external GdiPlusLib;
function GdipGetPathGradientPresetBlendCount; external GdiPlusLib;
function GdipGetPathGradientPresetBlend; external GdiPlusLib;
function GdipSetPathGradientPresetBlend; external GdiPlusLib;
function GdipSetPathGradientSigmaBlend; external GdiPlusLib;
function GdipSetPathGradientLinearBlend; external GdiPlusLib;
function GdipGetPathGradientWrapMode; external GdiPlusLib;
function GdipSetPathGradientWrapMode; external GdiPlusLib;
function GdipGetPathGradientTransform; external GdiPlusLib;
function GdipSetPathGradientTransform; external GdiPlusLib;
function GdipResetPathGradientTransform; external GdiPlusLib;
function GdipMultiplyPathGradientTransform; external GdiPlusLib;
function GdipTranslatePathGradientTransform; external GdiPlusLib;
function GdipScalePathGradientTransform; external GdiPlusLib;
function GdipRotatePathGradientTransform; external GdiPlusLib;
function GdipGetPathGradientFocusScales; external GdiPlusLib;
function GdipSetPathGradientFocusScales; external GdiPlusLib;
function GdipCreatePen1; external GdiPlusLib;
function GdipCreatePen2; external GdiPlusLib;
function GdipClonePen; external GdiPlusLib;
function GdipDeletePen; external GdiPlusLib;
function GdipSetPenWidth; external GdiPlusLib;
function GdipGetPenWidth; external GdiPlusLib;
function GdipSetPenUnit; external GdiPlusLib;
function GdipGetPenUnit; external GdiPlusLib;
function GdipSetPenLineCap197819; external GdiPlusLib;
function GdipSetPenStartCap; external GdiPlusLib;
function GdipSetPenEndCap; external GdiPlusLib;
function GdipSetPenDashCap197819; external GdiPlusLib;
function GdipGetPenStartCap; external GdiPlusLib;
function GdipGetPenEndCap; external GdiPlusLib;
function GdipGetPenDashCap197819; external GdiPlusLib;
function GdipSetPenLineJoin; external GdiPlusLib;
function GdipGetPenLineJoin; external GdiPlusLib;
function GdipSetPenCustomStartCap; external GdiPlusLib;
function GdipGetPenCustomStartCap; external GdiPlusLib;
function GdipSetPenCustomEndCap; external GdiPlusLib;
function GdipGetPenCustomEndCap; external GdiPlusLib;
function GdipSetPenMiterLimit; external GdiPlusLib;
function GdipGetPenMiterLimit; external GdiPlusLib;
function GdipSetPenMode; external GdiPlusLib;
function GdipGetPenMode; external GdiPlusLib;
function GdipSetPenTransform; external GdiPlusLib;
function GdipGetPenTransform; external GdiPlusLib;
function GdipResetPenTransform; external GdiPlusLib;
function GdipMultiplyPenTransform; external GdiPlusLib;
function GdipTranslatePenTransform; external GdiPlusLib;
function GdipScalePenTransform; external GdiPlusLib;
function GdipRotatePenTransform; external GdiPlusLib;
function GdipSetPenColor; external GdiPlusLib;
function GdipGetPenColor; external GdiPlusLib;
function GdipSetPenBrushFill; external GdiPlusLib;
function GdipGetPenBrushFill; external GdiPlusLib;
function GdipGetPenFillType; external GdiPlusLib;
function GdipGetPenDashStyle; external GdiPlusLib;
function GdipSetPenDashStyle; external GdiPlusLib;
function GdipGetPenDashOffset; external GdiPlusLib;
function GdipSetPenDashOffset; external GdiPlusLib;
function GdipGetPenDashCount; external GdiPlusLib;
function GdipSetPenDashArray; external GdiPlusLib;
function GdipGetPenDashArray; external GdiPlusLib;
function GdipGetPenCompoundCount; external GdiPlusLib;
function GdipSetPenCompoundArray; external GdiPlusLib;
function GdipGetPenCompoundArray; external GdiPlusLib;
function GdipCreateCustomLineCap; external GdiPlusLib;
function GdipDeleteCustomLineCap; external GdiPlusLib;
function GdipCloneCustomLineCap; external GdiPlusLib;
function GdipGetCustomLineCapType; external GdiPlusLib;
function GdipSetCustomLineCapStrokeCaps; external GdiPlusLib;
function GdipGetCustomLineCapStrokeCaps; external GdiPlusLib;
function GdipSetCustomLineCapStrokeJoin; external GdiPlusLib;
function GdipGetCustomLineCapStrokeJoin; external GdiPlusLib;
function GdipSetCustomLineCapBaseCap; external GdiPlusLib;
function GdipGetCustomLineCapBaseCap; external GdiPlusLib;
function GdipSetCustomLineCapBaseInset; external GdiPlusLib;
function GdipGetCustomLineCapBaseInset; external GdiPlusLib;
function GdipSetCustomLineCapWidthScale; external GdiPlusLib;
function GdipGetCustomLineCapWidthScale; external GdiPlusLib;
function GdipCreateAdjustableArrowCap; external GdiPlusLib;
function GdipSetAdjustableArrowCapHeight; external GdiPlusLib;
function GdipGetAdjustableArrowCapHeight; external GdiPlusLib;
function GdipSetAdjustableArrowCapWidth; external GdiPlusLib;
function GdipGetAdjustableArrowCapWidth; external GdiPlusLib;
function GdipSetAdjustableArrowCapMiddleInset; external GdiPlusLib;
function GdipGetAdjustableArrowCapMiddleInset; external GdiPlusLib;
function GdipSetAdjustableArrowCapFillState; external GdiPlusLib;
function GdipGetAdjustableArrowCapFillState; external GdiPlusLib;
function GdipLoadImageFromStream; external GdiPlusLib;
function GdipLoadImageFromFile; external GdiPlusLib;
function GdipLoadImageFromStreamICM; external GdiPlusLib;
function GdipLoadImageFromFileICM; external GdiPlusLib;
function GdipCloneImage; external GdiPlusLib;
function GdipDisposeImage; external GdiPlusLib;
function GdipSaveImageToFile; external GdiPlusLib;
function GdipSaveImageToStream; external GdiPlusLib;
function GdipSaveAdd; external GdiPlusLib;
function GdipSaveAddImage; external GdiPlusLib;
function GdipGetImageGraphicsContext; external GdiPlusLib;
function GdipGetImageBounds; external GdiPlusLib;
function GdipGetImageDimension; external GdiPlusLib;
function GdipGetImageType; external GdiPlusLib;
function GdipGetImageWidth; external GdiPlusLib;
function GdipGetImageHeight; external GdiPlusLib;
function GdipGetImageHorizontalResolution; external GdiPlusLib;
function GdipGetImageVerticalResolution; external GdiPlusLib;
function GdipGetImageFlags; external GdiPlusLib;
function GdipGetImageRawFormat; external GdiPlusLib;
function GdipGetImagePixelFormat; external GdiPlusLib;
function GdipGetImageThumbnail; external GdiPlusLib;
function GdipGetEncoderParameterListSize; external GdiPlusLib;
function GdipGetEncoderParameterList; external GdiPlusLib;
function GdipImageGetFrameDimensionsCount; external GdiPlusLib;
function GdipImageGetFrameDimensionsList; external GdiPlusLib;
function GdipImageGetFrameCount; external GdiPlusLib;
function GdipImageSelectActiveFrame; external GdiPlusLib;
function GdipImageRotateFlip; external GdiPlusLib;
function GdipGetImagePalette; external GdiPlusLib;
function GdipSetImagePalette; external GdiPlusLib;
function GdipGetImagePaletteSize; external GdiPlusLib;
function GdipGetPropertyCount; external GdiPlusLib;
function GdipGetPropertyIdList; external GdiPlusLib;
function GdipGetPropertyItemSize; external GdiPlusLib;
function GdipGetPropertyItem; external GdiPlusLib;
function GdipGetPropertySize; external GdiPlusLib;
function GdipGetAllPropertyItems; external GdiPlusLib;
function GdipRemovePropertyItem; external GdiPlusLib;
function GdipSetPropertyItem; external GdiPlusLib;
function GdipImageForceValidation; external GdiPlusLib;
function GdipCreateBitmapFromStream; external GdiPlusLib;
function GdipCreateBitmapFromFile; external GdiPlusLib;
function GdipCreateBitmapFromStreamICM; external GdiPlusLib;
function GdipCreateBitmapFromFileICM; external GdiPlusLib;
function GdipCreateBitmapFromScan0; external GdiPlusLib;
function GdipCreateBitmapFromGraphics; external GdiPlusLib;
function GdipCreateBitmapFromDirectDrawSurface; external GdiPlusLib;
function GdipCreateBitmapFromGdiDib; external GdiPlusLib;
function GdipCreateBitmapFromHBITMAP; external GdiPlusLib;
function GdipCreateHBITMAPFromBitmap; external GdiPlusLib;
function GdipCreateBitmapFromHICON; external GdiPlusLib;
function GdipCreateHICONFromBitmap; external GdiPlusLib;
function GdipCreateBitmapFromResource; external GdiPlusLib;
function GdipCloneBitmapArea; external GdiPlusLib;
function GdipCloneBitmapAreaI; external GdiPlusLib;
function GdipBitmapLockBits; external GdiPlusLib;
function GdipBitmapUnlockBits; external GdiPlusLib;
function GdipBitmapGetPixel; external GdiPlusLib;
function GdipBitmapSetPixel; external GdiPlusLib;
function GdipBitmapSetResolution; external GdiPlusLib;
function GdipCreateImageAttributes; external GdiPlusLib;
function GdipCloneImageAttributes; external GdiPlusLib;
function GdipDisposeImageAttributes; external GdiPlusLib;
function GdipSetImageAttributesToIdentity; external GdiPlusLib;
function GdipResetImageAttributes; external GdiPlusLib;
function GdipSetImageAttributesColorMatrix; external GdiPlusLib;
function GdipSetImageAttributesThreshold; external GdiPlusLib;
function GdipSetImageAttributesGamma; external GdiPlusLib;
function GdipSetImageAttributesNoOp; external GdiPlusLib;
function GdipSetImageAttributesColorKeys; external GdiPlusLib;
function GdipSetImageAttributesOutputChannel; external GdiPlusLib;
function GdipSetImageAttributesOutputChannelColorProfile; external GdiPlusLib;
function GdipSetImageAttributesRemapTable; external GdiPlusLib;
function GdipSetImageAttributesWrapMode; external GdiPlusLib;
function GdipSetImageAttributesICMMode; external GdiPlusLib;
function GdipGetImageAttributesAdjustedPalette; external GdiPlusLib;
function GdipFlush; external GdiPlusLib;
function GdipCreateFromHDC; external GdiPlusLib;
function GdipCreateFromHDC2; external GdiPlusLib;
function GdipCreateFromHWND; external GdiPlusLib;
function GdipCreateFromHWNDICM; external GdiPlusLib;
function GdipDeleteGraphics; external GdiPlusLib;
function GdipGetDC; external GdiPlusLib;
function GdipReleaseDC; external GdiPlusLib;
function GdipSetCompositingMode; external GdiPlusLib;
function GdipGetCompositingMode; external GdiPlusLib;
function GdipSetRenderingOrigin; external GdiPlusLib;
function GdipGetRenderingOrigin; external GdiPlusLib;
function GdipSetCompositingQuality; external GdiPlusLib;
function GdipGetCompositingQuality; external GdiPlusLib;
function GdipSetSmoothingMode; external GdiPlusLib;
function GdipGetSmoothingMode; external GdiPlusLib;
function GdipSetPixelOffsetMode; external GdiPlusLib;
function GdipGetPixelOffsetMode; external GdiPlusLib;
function GdipSetTextRenderingHint; external GdiPlusLib;
function GdipGetTextRenderingHint; external GdiPlusLib;
function GdipSetTextContrast; external GdiPlusLib;
function GdipGetTextContrast; external GdiPlusLib;
function GdipSetInterpolationMode; external GdiPlusLib;
function GdipGetInterpolationMode; external GdiPlusLib;
function GdipSetWorldTransform; external GdiPlusLib;
function GdipResetWorldTransform; external GdiPlusLib;
function GdipMultiplyWorldTransform; external GdiPlusLib;
function GdipTranslateWorldTransform; external GdiPlusLib;
function GdipScaleWorldTransform; external GdiPlusLib;
function GdipRotateWorldTransform; external GdiPlusLib;
function GdipGetWorldTransform; external GdiPlusLib;
function GdipResetPageTransform; external GdiPlusLib;
function GdipGetPageUnit; external GdiPlusLib;
function GdipGetPageScale; external GdiPlusLib;
function GdipSetPageUnit; external GdiPlusLib;
function GdipSetPageScale; external GdiPlusLib;
function GdipGetDpiX; external GdiPlusLib;
function GdipGetDpiY; external GdiPlusLib;
function GdipTransformPoints; external GdiPlusLib;
function GdipTransformPointsI; external GdiPlusLib;
function GdipGetNearestColor; external GdiPlusLib;
function GdipCreateHalftonePalette; external GdiPlusLib;
function GdipDrawLine; external GdiPlusLib;
function GdipDrawLineI; external GdiPlusLib;
function GdipDrawLines; external GdiPlusLib;
function GdipDrawLinesI; external GdiPlusLib;
function GdipDrawArc; external GdiPlusLib;
function GdipDrawArcI; external GdiPlusLib;
function GdipDrawBezier; external GdiPlusLib;
function GdipDrawBezierI; external GdiPlusLib;
function GdipDrawBeziers; external GdiPlusLib;
function GdipDrawBeziersI; external GdiPlusLib;
function GdipDrawRectangle; external GdiPlusLib;
function GdipDrawRectangleI; external GdiPlusLib;
function GdipDrawRectangles; external GdiPlusLib;
function GdipDrawRectanglesI; external GdiPlusLib;
function GdipDrawEllipse; external GdiPlusLib;
function GdipDrawEllipseI; external GdiPlusLib;
function GdipDrawPie; external GdiPlusLib;
function GdipDrawPieI; external GdiPlusLib;
function GdipDrawPolygon; external GdiPlusLib;
function GdipDrawPolygonI; external GdiPlusLib;
function GdipDrawPath; external GdiPlusLib;
function GdipDrawCurve; external GdiPlusLib;
function GdipDrawCurveI; external GdiPlusLib;
function GdipDrawCurve2; external GdiPlusLib;
function GdipDrawCurve2I; external GdiPlusLib;
function GdipDrawCurve3; external GdiPlusLib;
function GdipDrawCurve3I; external GdiPlusLib;
function GdipDrawClosedCurve; external GdiPlusLib;
function GdipDrawClosedCurveI; external GdiPlusLib;
function GdipDrawClosedCurve2; external GdiPlusLib;
function GdipDrawClosedCurve2I; external GdiPlusLib;
function GdipGraphicsClear; external GdiPlusLib;
function GdipFillRectangle; external GdiPlusLib;
function GdipFillRectangleI; external GdiPlusLib;
function GdipFillRectangles; external GdiPlusLib;
function GdipFillRectanglesI; external GdiPlusLib;
function GdipFillPolygon; external GdiPlusLib;
function GdipFillPolygonI; external GdiPlusLib;
function GdipFillPolygon2; external GdiPlusLib;
function GdipFillPolygon2I; external GdiPlusLib;
function GdipFillEllipse; external GdiPlusLib;
function GdipFillEllipseI; external GdiPlusLib;
function GdipFillPie; external GdiPlusLib;
function GdipFillPieI; external GdiPlusLib;
function GdipFillPath; external GdiPlusLib;
function GdipFillClosedCurve; external GdiPlusLib;
function GdipFillClosedCurveI; external GdiPlusLib;
function GdipFillClosedCurve2; external GdiPlusLib;
function GdipFillClosedCurve2I; external GdiPlusLib;
function GdipFillRegion; external GdiPlusLib;
function GdipDrawImage; external GdiPlusLib;
function GdipDrawImageI; external GdiPlusLib;
function GdipDrawImageRect; external GdiPlusLib;
function GdipDrawImageRectI; external GdiPlusLib;
function GdipDrawImagePoints; external GdiPlusLib;
function GdipDrawImagePointsI; external GdiPlusLib;
function GdipDrawImagePointRect; external GdiPlusLib;
function GdipDrawImagePointRectI; external GdiPlusLib;
function GdipDrawImageRectRect; external GdiPlusLib;
function GdipDrawImageRectRectI; external GdiPlusLib;
function GdipDrawImagePointsRect; external GdiPlusLib;
function GdipDrawImagePointsRectI; external GdiPlusLib;
function GdipEnumerateMetafileDestPoint; external GdiPlusLib;
function GdipEnumerateMetafileDestPointI; external GdiPlusLib;
function GdipEnumerateMetafileDestRect; external GdiPlusLib;
function GdipEnumerateMetafileDestRectI; external GdiPlusLib;
function GdipEnumerateMetafileDestPoints; external GdiPlusLib;
function GdipEnumerateMetafileDestPointsI; external GdiPlusLib;
function GdipEnumerateMetafileSrcRectDestPoint; external GdiPlusLib;
function GdipEnumerateMetafileSrcRectDestPointI; external GdiPlusLib;
function GdipEnumerateMetafileSrcRectDestRect; external GdiPlusLib;
function GdipEnumerateMetafileSrcRectDestRectI; external GdiPlusLib;
function GdipEnumerateMetafileSrcRectDestPoints; external GdiPlusLib;
function GdipEnumerateMetafileSrcRectDestPointsI; external GdiPlusLib;
function GdipPlayMetafileRecord; external GdiPlusLib;
function GdipSetClipGraphics; external GdiPlusLib;
function GdipSetClipRect; external GdiPlusLib;
function GdipSetClipRectI; external GdiPlusLib;
function GdipSetClipPath; external GdiPlusLib;
function GdipSetClipRegion; external GdiPlusLib;
function GdipSetClipHrgn; external GdiPlusLib;
function GdipResetClip; external GdiPlusLib;
function GdipTranslateClip; external GdiPlusLib;
function GdipTranslateClipI; external GdiPlusLib;
function GdipGetClip; external GdiPlusLib;
function GdipGetClipBounds; external GdiPlusLib;
function GdipGetClipBoundsI; external GdiPlusLib;
function GdipIsClipEmpty; external GdiPlusLib;
function GdipGetVisibleClipBounds; external GdiPlusLib;
function GdipGetVisibleClipBoundsI; external GdiPlusLib;
function GdipIsVisibleClipEmpty; external GdiPlusLib;
function GdipIsVisiblePoint; external GdiPlusLib;
function GdipIsVisiblePointI; external GdiPlusLib;
function GdipIsVisibleRect; external GdiPlusLib;
function GdipIsVisibleRectI; external GdiPlusLib;
function GdipSaveGraphics; external GdiPlusLib;
function GdipRestoreGraphics; external GdiPlusLib;
function GdipBeginContainer; external GdiPlusLib;
function GdipBeginContainerI; external GdiPlusLib;
function GdipBeginContainer2; external GdiPlusLib;
function GdipEndContainer; external GdiPlusLib;
function GdipGetMetafileHeaderFromWmf; external GdiPlusLib;
function GdipGetMetafileHeaderFromEmf; external GdiPlusLib;
function GdipGetMetafileHeaderFromFile; external GdiPlusLib;
function GdipGetMetafileHeaderFromStream; external GdiPlusLib;
function GdipGetMetafileHeaderFromMetafile; external GdiPlusLib;
function GdipGetHemfFromMetafile; external GdiPlusLib;
function GdipCreateStreamOnFile; external GdiPlusLib;
function GdipCreateMetafileFromWmf; external GdiPlusLib;
function GdipCreateMetafileFromEmf; external GdiPlusLib;
function GdipCreateMetafileFromFile; external GdiPlusLib;
function GdipCreateMetafileFromWmfFile; external GdiPlusLib;
function GdipCreateMetafileFromStream; external GdiPlusLib;
function GdipRecordMetafile; external GdiPlusLib;
function GdipRecordMetafileI; external GdiPlusLib;
function GdipRecordMetafileFileName; external GdiPlusLib;
function GdipRecordMetafileFileNameI; external GdiPlusLib;
function GdipRecordMetafileStream; external GdiPlusLib;
function GdipRecordMetafileStreamI; external GdiPlusLib;
function GdipSetMetafileDownLevelRasterizationLimit; external GdiPlusLib;
function GdipGetMetafileDownLevelRasterizationLimit; external GdiPlusLib;
function GdipGetImageDecodersSize; external GdiPlusLib;
function GdipGetImageDecoders; external GdiPlusLib;
function GdipGetImageEncodersSize; external GdiPlusLib;
function GdipGetImageEncoders; external GdiPlusLib;
function GdipComment; external GdiPlusLib;
function GdipCreateFontFamilyFromName; external GdiPlusLib;
function GdipDeleteFontFamily; external GdiPlusLib;
function GdipCloneFontFamily; external GdiPlusLib;
function GdipGetGenericFontFamilySansSerif; external GdiPlusLib;
function GdipGetGenericFontFamilySerif; external GdiPlusLib;
function GdipGetGenericFontFamilyMonospace; external GdiPlusLib;
function GdipGetFamilyName; external GdiPlusLib;
function GdipIsStyleAvailable; external GdiPlusLib;
function GdipFontCollectionEnumerable; external GdiPlusLib;
function GdipFontCollectionEnumerate; external GdiPlusLib;
function GdipGetEmHeight; external GdiPlusLib;
function GdipGetCellAscent; external GdiPlusLib;
function GdipGetCellDescent; external GdiPlusLib;
function GdipGetLineSpacing; external GdiPlusLib;
function GdipCreateFontFromDC; external GdiPlusLib;
function GdipCreateFontFromLogfontA; external GdiPlusLib;
function GdipCreateFontFromLogfontW; external GdiPlusLib;
function GdipCreateFont; external GdiPlusLib;
function GdipCloneFont; external GdiPlusLib;
function GdipDeleteFont; external GdiPlusLib;
function GdipGetFamily; external GdiPlusLib;
function GdipGetFontStyle; external GdiPlusLib;
function GdipGetFontSize; external GdiPlusLib;
function GdipGetFontUnit; external GdiPlusLib;
function GdipGetFontHeight; external GdiPlusLib;
function GdipGetFontHeightGivenDPI; external GdiPlusLib;
function GdipGetLogFontA; external GdiPlusLib;
function GdipGetLogFontW; external GdiPlusLib;
function GdipNewInstalledFontCollection; external GdiPlusLib;
function GdipNewPrivateFontCollection; external GdiPlusLib;
function GdipDeletePrivateFontCollection; external GdiPlusLib;
function GdipGetFontCollectionFamilyCount; external GdiPlusLib;
function GdipGetFontCollectionFamilyList; external GdiPlusLib;
function GdipPrivateAddFontFile; external GdiPlusLib;
function GdipPrivateAddMemoryFont; external GdiPlusLib;
function GdipDrawString; external GdiPlusLib;
function GdipMeasureString; external GdiPlusLib;
function GdipMeasureCharacterRanges; external GdiPlusLib;
function GdipDrawDriverString; external GdiPlusLib;
function GdipMeasureDriverString; external GdiPlusLib;
function GdipCreateStringFormat; external GdiPlusLib;
function GdipStringFormatGetGenericDefault; external GdiPlusLib;
function GdipStringFormatGetGenericTypographic; external GdiPlusLib;
function GdipDeleteStringFormat; external GdiPlusLib;
function GdipCloneStringFormat; external GdiPlusLib;
function GdipSetStringFormatFlags; external GdiPlusLib;
function GdipGetStringFormatFlags; external GdiPlusLib;
function GdipSetStringFormatAlign; external GdiPlusLib;
function GdipGetStringFormatAlign; external GdiPlusLib;
function GdipSetStringFormatLineAlign; external GdiPlusLib;
function GdipGetStringFormatLineAlign; external GdiPlusLib;
function GdipSetStringFormatTrimming; external GdiPlusLib;
function GdipGetStringFormatTrimming; external GdiPlusLib;
function GdipSetStringFormatHotkeyPrefix; external GdiPlusLib;
function GdipGetStringFormatHotkeyPrefix; external GdiPlusLib;
function GdipSetStringFormatTabStops; external GdiPlusLib;
function GdipGetStringFormatTabStops; external GdiPlusLib;
function GdipGetStringFormatTabStopCount; external GdiPlusLib;
function GdipSetStringFormatDigitSubstitution; external GdiPlusLib;
function GdipGetStringFormatDigitSubstitution; external GdiPlusLib;
function GdipGetStringFormatMeasurableCharacterRangeCount; external GdiPlusLib;
function GdipSetStringFormatMeasurableCharacterRanges; external GdiPlusLib;
function GdipCreateCachedBitmap; external GdiPlusLib;
function GdipDeleteCachedBitmap; external GdiPlusLib;
function GdipDrawCachedBitmap; external GdiPlusLib;
function GdipEmfToWmfBits; external GdiPlusLib;

function MakeCharacterRange(First, Length: Integer): TCharacterRange;
begin
  Result.First := First;
  Result.Length := Length;
end;

function GetPixelFormatSize(F: PixelFormat): UInt;
begin
  Result := (F shr 8) and $FF;
end;

function IsIndexedPixelFormat(F: PixelFormat): BOOL;
begin
  Result := (F and PixelFormatIndexed) <> 0;
end;

function IsAlphaPixelFormat(F: PixelFormat): BOOL;
begin
  Result := (F and PixelFormatAlpha) <> 0;
end;

function IsExtendedPixelFormat(F: PixelFormat): BOOL;
begin
  Result := (F and PixelFormatExtended) <> 0;
end;

function IsCanonicalPixelFormat(F: PixelFormat): BOOL;
begin
  Result := (F and PixelFormatCanonical) <> 0;
end;

initialization
  GdipLoad;
finalization
  GdipUnload;
end.
