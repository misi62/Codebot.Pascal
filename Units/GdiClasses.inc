{ Gdi plus classes }

type
  TGraphics = class;
  TPen = class;
  TBrush = class;
  TMatrix = class;
  TBitmap = class;
  TMetafile = class;
  TFontFamily = class;
  TGraphicsPath = class;
  TRegion = class;
  TImage = class;
  THatchBrush = class;
  TSolidBrush = class;
  TLinearGradientBrush = class;
  TPathGradientBrush = class;
  TFont = class;
  TFontCollection = class;
  TInstalledFontCollection = class;
  TPrivateFontCollection = class;
  TImageAttributes = class;
  TCachedBitmap = class;
  TStringFormat = class;

{ TRegion }

  TRegion = class(TGdiObject, IRegion)
  private
    FNativeRegion: GpRegion;
    procedure SetNativeRegion(Region: GpRegion);
    function GetNativeRegion: GpRegion;
  public
    constructor Create; overload;
    constructor Create(Region: GpRegion); overload;
    constructor Create(Rect: TRectF); overload;
    constructor Create(Rect: TRectI); overload;
    constructor Create(Path: IGraphicsPath); overload;
    constructor Create(RegionData: PByte; Size: Integer); overload;
    constructor Create(Rgn: HRGN); overload;
    destructor Destroy; override;
    function FromHRGN(Rgn: HRGN): IRegion;
    function Clone: IRegion;
    function MakeInfinite: TStatus;
    function MakeEmpty: TStatus;
    function GetDataSize: UInt;
    function GetData(Buffer: PByte; BufferSize: UInt;
      SizeFilled: PUInt = nil): TStatus;
    function Intersect(const Rect: TRectI): TStatus; overload;
    function Intersect(const Rect: TRectF): TStatus; overload;
    function Intersect(Path: IGraphicsPath): TStatus; overload;
    function Intersect(Region: IRegion): TStatus; overload;
    function Union(const Rect: TRectI): TStatus; overload;
    function Union(const Rect: TRectF): TStatus; overload;
    function Union(Path: IGraphicsPath): TStatus; overload;
    function Union(Region: IRegion): TStatus; overload;
    function Xor_(const Rect: TRectI): TStatus; overload;
    function Xor_(const Rect: TRectF): TStatus; overload;
    function Xor_(Path: IGraphicsPath): TStatus; overload;
    function Xor_(Region: IRegion): TStatus; overload;
    function Exclude(const Rect: TRectI): TStatus; overload;
    function Exclude(const Rect: TRectF): TStatus; overload;
    function Exclude(Path: IGraphicsPath): TStatus; overload;
    function Exclude(Region: IRegion): TStatus; overload;
    function Complement(const Rect: TRectI): TStatus; overload;
    function Complement(const Rect: TRectF): TStatus; overload;
    function Complement(Path: IGraphicsPath): TStatus; overload;
    function Complement(Region: IRegion): TStatus; overload;
    function Translate(DX, DY: Single): TStatus; overload;
    function Translate(DX, DY: Integer): TStatus; overload;
    function Transform(Matrix: IMatrix): TStatus;
    function GetBounds(out Rect: TRectI; G: IGraphics): TStatus; overload;
    function GetBounds(out Rect: TRectF; G: IGraphics): TStatus; overload;
    function GetHRGN(G: IGraphics): HRGN;
    function IsEmpty(G: IGraphics): Boolean;
    function IsInfinite(G: IGraphics): Boolean;
    function IsVisible(X, Y: Integer; G: IGraphics = nil): Boolean; overload;
    function IsVisible(const Point: TPointI; G: IGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Single; G: IGraphics = nil): Boolean; overload;
    function IsVisible(const Point: TPointF; G: IGraphics = nil): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Integer; G: IGraphics): Boolean; overload;
    function IsVisible(const Rect: TRectI; G: IGraphics = nil): Boolean; overload;
    function IsVisible(X, Y, Width, Height: Single; G: IGraphics = nil): Boolean; overload;
    function IsVisible(const Rect: TRectF; G: IGraphics = nil): Boolean; overload;
    function Equals(Region: IRegion; G: IGraphics): Boolean;
    function GetRegionScansCount(Matrix: IMatrix): UInt;
    function GetRegionScans(Matrix: IMatrix; Rects: PRectF; out Count: Integer): TStatus; overload;
    function GetRegionScans(Matrix: IMatrix; Rects: PRectI; out Count: Integer): TStatus; overload;
  end;

{ TFontFamily }

  TFontFamily = class(TGdiObject, IFontFamily)
  private
    FNativeFamily: GpFontFamily;
    function GetNativeFamily: GpFontFamily;
    procedure SetNativeFamily(Value: GpFontFamily);
  public
    constructor Create; overload;
    constructor Create(Orig: GpFontFamily; Status: TStatus); overload;
    constructor Create(Name: WideString; FontCollection: IFontCollection = nil); overload;
    destructor Destroy; override;
    class function GenericSansSerif: IFontFamily;
    class function GenericSerif: IFontFamily;
    class function GenericMonospace: IFontFamily;
    function GetFamilyName(out Name: string; Language: LangId = 0): TStatus;
    function Clone: IFontFamily;
    function IsAvailable: Boolean;
    function IsStyleAvailable(Style: Integer): Boolean;
    function GetEmHeight(Style: Integer): UInt16;
    function GetCellAscent(Style: Integer): UInt16;
    function GetCellDescent(Style: Integer): UInt16;
    function GetLineSpacing(Style: Integer): UInt16;
  end;

{ TFontCollection }

  TFontCollection = class(TGdiObject, IFontCollection)
  private
    FNativeFontCollection: GpFontCollection;
    function GetNativeFontCollection: GpFontCollection;
    procedure SetNativeFontCollection(Value: GpFontCollection);
  public
    constructor Create;
    destructor Destroy; override;
    function GetFamilyCount: Integer;
    function GetFamilies(NumSought: Integer; out Families: array of IFontFamily;
      out NumFound: Integer): TStatus;
  end;

{ TInstalledFontCollection }

  TInstalledFontCollection = class(TFontCollection, IInstalledFontCollection)
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TPrivateFontCollection }

  TPrivateFontCollection = class(TFontCollection, IPrivateFontCollection)
  public
    constructor Create; 
    destructor Destroy; override;
    function AddFontFile(Filename: WideString): TStatus;
    function AddMemoryFont(Memory: Pointer; Length: Integer): TStatus;
  end;

{ TFont }

  TFont = class(TGdiObject, IFont)
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
    constructor Create(Family: IFontFamily; EmSize: Single;
      Style: TFontStyle = FontStyleRegular;
      Unit_: TUnit = UnitPoint); overload;
    constructor Create(FamilyName: WideString; EmSize: Single;
      Style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint;
      FontCollection: IFontCollection = nil); overload;
    function GetLogFontA(G: IGraphics; out LogFontA: TLogFontA): TStatus;
    function GetLogFontW(G: IGraphics; out LogFontW: TLogFontW): TStatus;
    function Clone: IFont;
    destructor Destroy; override;
    function IsAvailable: Boolean;
    function GetStyle: Integer;
    function GetSize: Single;
    function GetUnit: TUnit;
    function GetHeight(Graphics: IGraphics): Single; overload;
    function GetHeight(DPI: Single): Single; overload;
    function GetFamily(Family: IFontFamily): TStatus;
  end;

{ TImage }

  TImage = class(TGdiObject, IImage)
  private
    FNativeImage: GpImage;
    function GetNativeImage: GpImage;
    procedure SetNativeImage(Image: GpImage);
  public
    constructor Create; overload;
    constructor Create(Image: GpImage; Status: TStatus); overload;
    constructor Create(Filename: WideString; UseEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(Stream: IStream; UseEmbeddedColorManagement: Boolean = False); overload;
    function FromFile(Filename: WideString; UseEmbeddedColorManagement: Boolean = False): IImage; virtual;
    function FromStream(Stream: IStream; UseEmbeddedColorManagement: Boolean = False): IImage; virtual;
    destructor Destroy; override;
    function Clone: IImage;
    function Save(Filename: WideString; const clsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function Save(Stream: IStream; const clsidEncoder: TGUID;
      EncoderParams: PEncoderParameters = nil): TStatus; overload;
    function SaveAdd(EncoderParams: PEncoderParameters): TStatus; overload;
    function SaveAdd(NewImage: IImage; EncoderParams: PEncoderParameters): TStatus; overload;
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
      Callback: GetThumbnailImageAbort = nil; CallbackData: Pointer = nil): IImage;
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

{ TBitmap }

  TBitmap = class(TImage, IBitmap)
  public
    constructor Create(Bitmap: GpBitmap); overload;
    constructor Create(Filename: WideString; UseEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(Stream: IStream; UseEmbeddedColorManagement: Boolean = False); overload;
    constructor Create(Width, Height, Stride: Integer; Format: TPixelFormat; scan0: PByte); overload;
    constructor Create(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppArgb); overload;
    constructor Create(Width, Height: Integer; Target: IGraphics); overload;
    function FromFile(Filename: WideString; UseEmbeddedColorManagement: Boolean = False): IImage; override;
    function FromStream(Stream: IStream; UseEmbeddedColorManagement: Boolean = False): IImage; override;
    function Clone(Rect: TRectI; Format: TPixelFormat): IBitmap; overload;
    function Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IBitmap; overload;
    function Clone(Rect: TRectF; Format: TPixelFormat): IBitmap; overload;
    function Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IBitmap; overload;
    function LockBits(Rect: TRectI; Flags: UInt; Format: TPixelFormat; out LockedBitmapData: TBitmapData): TStatus;
    function UnlockBits(var LockedBitmapData: TBitmapData): TStatus;
    function GetPixel(X, Y: Integer; out Color: TArgb): TStatus;
    function SetPixel(X, Y: Integer; Color: TArgb): TStatus;
    function SetResolution(XDPI, YDPI: Single): TStatus;
    constructor Create(surface: IDirectDrawSurface7); overload;
    constructor Create(var BitmapInfo: TBitmapINFO; BitmapData: Pointer); overload;
    constructor Create(Bmp: HBitmap; Pal: HPALETTE); overload;
    constructor Create(Icon: HICON); overload;
    constructor Create(hInstance: HMODULE; BitmapName: WideString); overload;
    function FromDiRectDrawSurface7(surface: IDirectDrawSurface7): IBitmap;
    function FromBitmapINFO(var BitmapInfo: TBitmapINFO; BitmapData: Pointer): IBitmap;
    function FromHBitmap(Bmp: HBitmap; Pal: HPALETTE): IBitmap;
    function FromHICON(Icon: HICON): IBitmap;
    function FromResource(hInstance: HMODULE; BitmapName: WideString): IBitmap;
    function GetHBitmap(ColorBackground: TArgb; out Bmp: HBitmap): TStatus;
    function GetHICON(out Icon: HICON): TStatus;
  end;

{ TCustomLineCap }

  TCustomLineCap = class(TGdiObject, ICustomLineCap)
  private
    FNativeCap: GpCustomLineCap;
    function GetNativeCap: GpCustomLineCap;
    procedure SetNativeCap(Cap: GpCustomLineCap);
  public
    constructor Create; overload;
    constructor Create(Cap: GpCustomLineCap; Status: TStatus); overload;
    constructor Create(FillPath, StrokePath: IGraphicsPath;
      BaseCap: TLineCap = LineCapFlat;
      BaseInset: Single = 0); overload;
    destructor Destroy; override;
    function Clone: ICustomLineCap;
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

{ TCachedBitmap }

  TCachedBitmap = class(TGdiObject, ICachedBitmap)
  private
    FNativeCachedBitmap: GpCachedBitmap;
    function GetNativeCachedBitmap: GpCachedBitmap;
  public
    constructor Create(Bitmap: IBitmap; Graphics: IGraphics);
    destructor Destroy; override;
  end;

{ TImageAttributes }

  TImageAttributes = class(TGdiObject, IImageAttributes)
  private
    FNativeImageAttr: GpImageAttributes;
    function GetNativeImageAttr: GpImageAttributes;
    procedure SetNativeImageAttr(ImageAttr: GpImageAttributes);
  public
    constructor Create; overload;
    constructor Create(ImageAttr: GpImageAttributes; Status: GpStatus); overload;
    destructor Destroy; override;
    function Clone: IImageAttributes;
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

{ TMatrix }

  TMatrix = class(TGdiObject, IMatrix)
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
    function Clone: IMatrix;
    function GetElements(const M: TMatrixArray): TStatus;
    function SetElements(M11, M12, M21, M22, DX, DY: Single): TStatus;
    function OffsetX: Single;
    function OffsetY: Single;
    function Reset: TStatus;
    function Multiply(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
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
    function Equals(Matrix: IMatrix): Boolean;
  end;

{ TBrush }

  TBrush = class(TGdiObject, IBrush)
  private
    FNativeBrush: GpBrush;
    procedure SetNativeBrush(Brush: GpBrush);
    function GetNativeBrush: GpBrush;
  public
    constructor Create; overload;
    constructor Create(Brush: GpBrush; Status: TStatus); overload;
    destructor Destroy; override;
    function Clone: IBrush; virtual;
    function GetType: TBrushType;
  end;

{ TSolidBrush }

  TSolidBrush = class(TBrush, ISolidBrush)
  public
    constructor Create(Color: TArgb); overload;
    constructor Create; overload;
    function GetColor(out Color: TArgb): TStatus;
    function SetColor(Color: TArgb): TStatus;
  end;

{ TTextureBrush }

  TTextureBrush = class(TBrush, ITextureBrush)
  public
    constructor Create(Image: IImage; WrapMode: TWrapMode = WrapModeTile); overload;
    constructor Create(Image: IImage; WrapMode: TWrapMode; DstRect: TRectF); overload;
    constructor Create(Image: IImage; DstRect: TRectF; ImageAttributes: IImageAttributes = nil); overload;
    constructor Create(Image: IImage; DstRect: TRectI; ImageAttributes: IImageAttributes = nil); overload;
    constructor Create(Image: IImage; WrapMode: TWrapMode; DstRect: TRectI); overload;
    constructor Create(Image: IImage; WrapMode: TWrapMode; DstX, DstY, DstWidth,
      DstHeight: Single); overload;
    constructor Create(Image: IImage; WrapMode: TWrapMode; DstX, DstY, DstWidth,
      DstHeight: Integer); overload;
    constructor Create; overload;
    function SetTransform(Matrix: IMatrix): TStatus;
    function GetTransform(Matrix: IMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: MatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
    function GetImage: IImage;
  end;

{ TLinearGradientBrush }

  TLinearGradientBrush = class(TBrush, ILinearGradientBrush)
  public
    constructor Create; overload;
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
    function SetTransform(Matrix: IMatrix): TStatus;
    function GetTransform(Matrix: IMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function SetWrapMode(WrapMode: TWrapMode): TStatus;
    function GetWrapMode: TWrapMode;
  end;

{ THatchBrush }

  THatchBrush = class(TBrush, IHatchBrush)
  public
    constructor Create; overload;
    constructor Create(HatchStyle: THatchStyle; ForeColor: TArgb; BackColor: TArgb = aclBlack); overload;
    function GetHatchStyle: THatchStyle;
    function GetForegrounDColor(out Color: TArgb): TStatus;
    function GetBackgrounDColor(out Color: TArgb): TStatus;
  end;

{ TPen }

  TPen = class(TGdiObject, IPen)
  private
    FNativePen: GpPen;
    procedure SetNativePen(Pen: GpPen);
    function GetNativePen: GpPen;
  public
    constructor Create(Pen: GpPen; Status: TStatus); overload;
    constructor Create(Color: TArgb; Width: Single = 1); overload;
    constructor Create(Brush: IBrush; Width: Single = 1); overload;
    destructor Destroy; override;
    function Clone: IPen; overload;
    function SetWidth(Width: Single): TStatus;
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
    function SetCustomStartCap(CustomCap: ICustomLineCap): TStatus;
    function GetCustomStartCap(CustomCap: ICustomLineCap): TStatus;
    function SetCustomEndCap(CustomCap: ICustomLineCap): TStatus;
    function GetCustomEndCap(CustomCap: ICustomLineCap): TStatus;
    function SetMiterLimit(MiterLimit: Single): TStatus;
    function GetMiterLimit: Single;
    function SetAlignment(penAlignment: TPenAlignment): TStatus;
    function GetAlignment: TPenAlignment;
    function SetTransform(Matrix: IMatrix): TStatus;
    function GetTransform(Matrix: IMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetPenType: TPenType;
    function SetColor(Color: TArgb): TStatus;
    function SetBrush(Brush: IBrush): TStatus;
    function GetColor(out Color: TArgb): TStatus;
    function GetBrush: IBrush;
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

{ TStringFormat }

  TStringFormat = class(TGdiObject, IStringFormat)
  private
    FNativeFormat: GpStringFormat;
    function GetNativeFormat: GpStringFormat;
    procedure SetNativeFormat(Value: GpStringFormat);
  public
    constructor Create(ClonedStringFormat: GpStringFormat; Status: TStatus); overload;
    constructor Create(FormatFlags: Integer = 0; Language: LangId = LANG_NEUTRAL); overload;
    constructor Create(Format: IStringFormat); overload;
    destructor Destroy; override;
    class function GenericDefault: IStringFormat;
    class function GenericTypoGraphic: IStringFormat;
    function Clone: IStringFormat;
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

{ TGraphicsPath }

  TGraphicsPath = class(TGdiObject, IGraphicsPath)
  private
    FNativePath: GpPath;
    function GetNativePath: GpPath;
    procedure SetNativePath(Path: GpPath);
  public
    constructor Create(Path: GpPath); overload;
    constructor Create(Path: IGraphicsPath); overload;
    constructor Create(FillMode: TFillMode = FillModeAlternate); overload;
    constructor Create(Points: PPointF; Types: PByte; Count: Integer;
      FillMode: TFillMode = FillModeAlternate); overload;
    constructor Create(Points: PPointI; Types: PByte; Count: Integer;
      FillMode: TFillMode = FillModeAlternate); overload;
    destructor Destroy; override;
    function Clone: IGraphicsPath;
    function Reset: TStatus;
    function GetFillMode: TFillMode;
    function SetFillMode(Fillmode: TFillMode): TStatus;
    function GetPathData(PathData: TPathData): TStatus;
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
    function AddPath(AddingPath: IGraphicsPath; Connect: Boolean): TStatus;
    function AddString(Text: WideString; Length: Integer; Family: IFontFamily;
      Style: Integer; EmSize: Single; Origin: TPointF; Format: IStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IFontFamily;
      Style: Integer; EmSize: Single; LayoutRect: TRectF; Format: IStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IFontFamily;
      Style: Integer; EmSize: Single; Origin: TPointI; Format: IStringFormat): TStatus; overload;
    function AddString(Text: WideString; Length: Integer; Family: IFontFamily;
      Style: Integer; EmSize: Single; LayoutRect: TRectI; Format: IStringFormat): TStatus; overload;
    function Transform(Matrix: IMatrix): TStatus;
    function GetBounds(out Bounds: TRectF; Matrix: IMatrix = nil; Pen: IPen = nil): TStatus; overload;
    function GetBounds(out Bounds: TRectI; Matrix: IMatrix = nil; Pen: IPen = nil): TStatus; overload;
    function Flatten(Matrix: IMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Widen(Pen: IPen; Matrix: IMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Outline(Matrix: IMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
    function Warp(DestPoints: PPointF; Count: Integer; SrcRect: TRectF;
      Matrix: IMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
      Flatness: Single = FlatnessDefault): TStatus;
    function GetPointCount: Integer;
    function GetPathTypes(Types: PByte; Count: Integer): TStatus;
    function GetPathPoints(Points: PPointF; Count: Integer): TStatus; overload;
    function GetPathPoints(Points: PPointI; Count: Integer): TStatus; overload;
    function IsVisible(Point: TPointF; G: IGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Single; G: IGraphics = nil): Boolean; overload;
    function IsVisible(Point: TPointI; G: IGraphics = nil): Boolean; overload;
    function IsVisible(X, Y: Integer; G: IGraphics = nil): Boolean; overload;
    function IsOutlineVisible(Point: TPointF; Pen: IPen; G: IGraphics = nil): Boolean; overload;
    function IsOutlineVisible(X, Y: Single; Pen: IPen; G: IGraphics = nil): Boolean; overload;
    function IsOutlineVisible(Point: TPointI; Pen: IPen; G: IGraphics = nil): Boolean; overload;
    function IsOutlineVisible(X, Y: Integer; Pen: IPen; G: IGraphics = nil): Boolean; overload;
  end;

{ TGraphicsPathIterator }

  TGraphicsPathIterator = class(TGdiObject, IGraphicsPathIterator)
  private
    FNativeIterator: GpPathIterator;
    procedure SetNativeIterator(FNativeIterator: GpPathIterator);
  public
    constructor Create(Path: IGraphicsPath);
    destructor Destroy; override;
    function NextSubPath(out StartIndex, EndIndex: Integer; out IsClosed: Boolean): Integer; overload;
    function NextSubPath(Path: IGraphicsPath; out IsClosed: Boolean): Integer; overload;
    function NextPathType(out PathType: TPathPointType; out StartIndex, EndIndex: Integer): Integer;
    function NextMarker(out StartIndex, EndIndex: Integer): Integer; overload;
    function NextMarker(Path: IGraphicsPath): Integer; overload;
    function GetCount: Integer;
    function GetSubPathCount: Integer;
    function HasCurve: Boolean;
    procedure Rewind;
    function Enumerate(Points: PPointF; Types: PByte; Count: Integer): Integer;
    function CopyData(Points: PPointF; Types: PByte; StartIndex, EndIndex: Integer): Integer;
  end;

{ TPathGradientBrush }

  TPathGradientBrush = class(TBrush)
  public
    constructor Create; overload;
    constructor Create(Path: IGraphicsPath); overload;
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
    function GetGraphicsPath(Path: IGraphicsPath): TStatus;
    function SetGraphicsPath(Path: IGraphicsPath): TStatus;
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
    function GetTransform(Matrix: IMatrix): TStatus;
    function SetTransform(Matrix: IMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IMatrix;
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

{ TGraphics }

  TGraphics = class(TGdiObject, IGraphics)
  private
    FNativeGraphics: GpGraphics;
    function GetNativeGraphics: GpGraphics;
    procedure SetNativeGraphics(Graphics: GpGraphics);
  public
    constructor Create(Graphics: GpGraphics); overload;
    constructor Create(DC: HDC); overload;
    constructor Create(DC: HDC; Device: THandle); overload;
    constructor Create(Wnd: HWND; ICM: Boolean); overload;
    constructor Create(Image: IImage); overload;
    function FromHDC(DC: HDC): IGraphics; overload;
    function FromHDC(DC: HDC; Device: THandle): IGraphics; overload;
    function FromHWND(hwnd: HWND; ICM: Boolean = False): IGraphics;
    function FromImage(Image: IImage): IGraphics;
    destructor Destroy; override;
    procedure Flush(intention: TFlushIntention = FlushIntentionFlush);
    function GetHDC: HDC;
    procedure ReleaseHDC(DC: HDC);
    function SetRenderingOrigin(X, Y: Integer): TStatus;
    function GetRenderingOrigin(out X, Y: Integer): TStatus;
    function SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
    function GetCompositingMode: TCompositingMode;
    function SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
    function GetCompositingQuality: TCompositingQuality;
    function SetTextRenderingHint(NewMode: TTextRenderingHint): TStatus;
    function GetTextRenderingHint: TTextRenderingHint;
    function SetTextContrast(contrast: UInt): TStatus;
    function GetTextContrast: UInt;
    function GetInterpolationMode: TInterpolationMode;
    function SetInterpolationMode(interpolationMode: TInterpolationMode): TStatus;
    function GetSmoothingMode: TSmoothingMode;
    function SetSmoothingMode(smoothingMode: TSmoothingMode): TStatus;
    function GetPixelOffsetMode: TPixelOffsetMode;
    function SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
    function SetTransform(Matrix: IMatrix): TStatus;
    function ResetTransform: TStatus;
    function MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
    function GetTransform(Matrix: IMatrix): TStatus;
    function SetPageUnit(Unit_: TUnit): TStatus;
    function SetPageScale(Scale: Single): TStatus;
    function GetPageUnit: TUnit;
    function GetPageScale: Single;
    function GetDPIX: Single;
    function GetDPIY: Single;
    function TransformPoints(DestSpace: TCoordinateSpace; SrcSpace: TCoordinateSpace;
      Pts: PPointF; Count: Integer): TStatus; overload;
    function TransformPoints(DestSpace: TCoordinateSpace; SrcSpace: TCoordinateSpace;
      Pts: PPointI; Count: Integer): TStatus; overload;
    function GetNearestColor(var Color: TArgb): TStatus;
    function DrawLine(Pen: IPen; X1, Y1, X2, Y2: Single): TStatus; overload;
    function DrawLine(Pen: IPen; const Pt1, Pt2: TPointF): TStatus; overload;
    function DrawLines(Pen: IPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawLine(Pen: IPen; X1, Y1, X2, Y2: Integer): TStatus; overload;
    function DrawLine(Pen: IPen; const Pt1, Pt2: TPointI): TStatus; overload;
    function DrawLines(Pen: IPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawArc(Pen: IPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawArc(Pen: IPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawBezier(Pen: IPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus; overload;
    function DrawBezier(Pen: IPen; const Pt1, Pt2, Pt3, Pt4: TPointF): TStatus; overload;
    function DrawBeziers(Pen: IPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawBezier(Pen: IPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus; overload;
    function DrawBezier(Pen: IPen; const Pt1, Pt2, Pt3, Pt4: TPointI): TStatus; overload;
    function DrawBeziers(Pen: IPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawRectangle(Pen: IPen; const Rect: TRectF): TStatus; overload;
    function DrawRectangle(Pen: IPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawRectangles(Pen: IPen; Rects: PRectF; Count: Integer): TStatus; overload;
    function DrawRectangle(Pen: IPen; const Rect: TRectI): TStatus; overload;
    function DrawRectangle(Pen: IPen; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawRectangles(Pen: IPen; Rects: PRectI; Count: Integer): TStatus; overload;
    function DrawEllipse(Pen: IPen; const Rect: TRectF): TStatus; overload;
    function DrawEllipse(Pen: IPen; X, Y, Width, Height: Single): TStatus; overload;
    function DrawEllipse(Pen: IPen; const Rect: TRectI): TStatus; overload;
    function DrawEllipse(Pen: IPen; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawPie(Pen: IPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPie(Pen: IPen; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function DrawPolygon(Pen: IPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawPolygon(Pen: IPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawPath(Pen: IPen; Path: IGraphicsPath): TStatus;
    function DrawCurve(Pen: IPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawCurve(Pen: IPen; Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function DrawCurve(Pen: IPen; Points: PPointF; Count, Offset,
      NumberOfSegments: Integer; Tension: Single = 0.5): TStatus; overload;
    function DrawCurve(Pen: IPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawCurve(Pen: IPen; Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function DrawCurve(Pen: IPen; Points: PPointI; Count, Offset, NumberOfSegments: Integer;
      Tension: Single = 0.5): TStatus; overload;
    function DrawClosedCurve(Pen: IPen; Points: PPointF; Count: Integer): TStatus; overload;
    function DrawClosedCurve(Pen: IPen; Points: PPointF; Count: Integer; Tension: Single): TStatus; overload;
    function DrawClosedCurve(Pen: IPen; Points: PPointI; Count: Integer): TStatus; overload;
    function DrawClosedCurve(Pen: IPen; Points: PPointI; Count: Integer; Tension: Single): TStatus; overload;
    function Clear(Color: TArgb): TStatus;
    function FillRectangle(Brush: IBrush; const Rect: TRectF): TStatus; overload;
    function FillRectangle(Brush: IBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillRectangles(Brush: IBrush; Rects: PRectF; Count: Integer): TStatus; overload;
    function FillRectangle(Brush: IBrush; const Rect: TRectI): TStatus; overload;
    function FillRectangle(Brush: IBrush; X, Y, Width, Height: Integer): TStatus; overload;
    function FillRectangles(Brush: IBrush; Rects: PRectI; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IBrush; Points: PPointF; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IBrush; Points: PPointF; Count: Integer; FillMode: TFillMode): TStatus; overload;
    function FillPolygon(Brush: IBrush; Points: PPointI; Count: Integer): TStatus; overload;
    function FillPolygon(Brush: IBrush; Points: PPointI; Count: Integer; FillMode: TFillMode): TStatus; overload;
    function FillEllipse(Brush: IBrush; const Rect: TRectF): TStatus; overload;
    function FillEllipse(Brush: IBrush; X, Y, Width, Height: Single): TStatus; overload;
    function FillEllipse(Brush: IBrush; const Rect: TRectI): TStatus; overload;
    function FillEllipse(Brush: IBrush; X, Y, Width, Height: Integer): TStatus; overload;
    function FillPie(Brush: IBrush; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IBrush; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IBrush; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPie(Brush: IBrush; X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus; overload;
    function FillPath(Brush: IBrush; Path: IGraphicsPath): TStatus;
    function FillClosedCurve(Brush: IBrush; Points: PPointF; Count: Integer): TStatus; overload;
    function FillClosedCurve(Brush: IBrush; Points: PPointF; Count: Integer;
      FillMode: TFillMode; Tension: Single = 0.5): TStatus; overload;
    function FillClosedCurve(Brush: IBrush; Points: PPointI; Count: Integer): TStatus; overload;
    function FillClosedCurve(Brush: IBrush; Points: PPointI; Count: Integer;
      FillMode: TFillMode; Tension: Single = 0.5): TStatus; overload;
    function FillRegion(Brush: IBrush; Region: IRegion): TStatus;
    function DrawString(Text: WideString; Font: IFont;
      const LayoutRect: TRectF; StringFormat: IStringFormat; Brush: IBrush): TStatus; overload;
    function DrawString(Text: WideString; Font: IFont;
      const Origin: TPointF; Brush: IBrush): TStatus; overload;
    function DrawString(Text: WideString; Font: IFont;
      const Origin: TPointF; StringFormat: IStringFormat; Brush: IBrush): TStatus; overload;
    function MeasureString(Text: WideString; Font: IFont;
      const LayoutRect: TRectF; StringFormat: IStringFormat; out BoundingBox: TRectF;
      CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(Text: WideString; Font: IFont;
      const LayoutRectSize: TSizeF; StringFormat: IStringFormat; out Size: TSizeF;
      CodePointsFitted: PInteger = nil; LinesFilled: PInteger = nil): TStatus; overload;
    function MeasureString(Text: WideString; Font: IFont;
      const Origin: TPointF; StringFormat: IStringFormat;
      out BoundingBox: TRectF): TStatus; overload;
    function MeasureString(Text: WideString; Font: IFont;
      const LayoutRect: TRectF; out BoundingBox: TRectF): TStatus; overload;
    function MeasureString(Text: WideString; Font: IFont;
      const Origin: TPointF; out BoundingBox: TRectF): TStatus; overload;
    function MeasureCharacterRanges(Text: WideString; Font: IFont;
      const LayoutRect: TRectF; StringFormat: IStringFormat; RegionCount: Integer;
      const Regions: array of IRegion): TStatus; overload;
    function DrawDriverString(Text: PUInt16; Length: Integer; Font: IFont;
      Brush: IBrush; Positions: PPointF; Flags: Integer; Matrix: IMatrix): TStatus;
    function MeasureDriverString(Text: PUInt16; Length: Integer; Font: IFont;
      Positions: PPointF; Flags: Integer; Matrix: IMatrix;
      out BoundingBox: TRectF): TStatus;
    function DrawCachedBitmap(Bitmap: ICachedBitmap; X, Y: Integer): TStatus;
    function DrawImage(Image: IImage; const Point: TPointF): TStatus; overload;
    function DrawImage(Image: IImage; X, Y: Single): TStatus; overload;
    function DrawImage(Image: IImage; const Rect: TRectF): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, Width, Height: Single): TStatus; overload;
    function DrawImage(Image: IImage; const Point: TPointI): TStatus; overload;
    function DrawImage(Image: IImage; X, Y: Integer): TStatus; overload;
    function DrawImage(Image: IImage; const Rect: TRectI): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, Width, Height: Integer): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointI; Count: Integer): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit): TStatus; overload;
    function DrawImage(Image: IImage; const DestRect: TRectF; SrcX, SrcY,
      SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
      ImageAttributes: IImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer;
      SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
      ImageAttributes: IImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth,
      SrcHeight: Integer; SrcUnit: TUnit): TStatus; overload;
    function DrawImage(Image: IImage; const DestRect: TRectI; SrcX, SrcY,
      SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
      ImageAttributes: IImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function DrawImage(Image: IImage; DestPoints: PPointI;
      Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
      ImageAttributes: IImageAttributes = nil; Callback: DrawImageAbort = nil;
      CallbackData: Pointer = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointF;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointI;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestRect: TRectF;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestRect: TRectI;
      Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointF;
      Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointI;
      Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointF;
      const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointI;
      const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestRect: TRectF;
      const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; const DestRect, SrcRect: TRectI;
      SrcUnit: TUnit; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
      ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointF;
      Count: Integer; const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointI;
      Count: Integer; const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
      CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus; overload;
    function SetClip(G: IGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rect: TRectI; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Path: IGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Region: IRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function SetClip(Rgn: HRGN; CombineMode: TCombineMode = CombineModeReplace): TStatus; overload;
    function IntersectClip(const Rect: TRectF): TStatus; overload;
    function IntersectClip(const Rect: TRectI): TStatus; overload;
    function IntersectClip(Region: IRegion): TStatus; overload;
    function ExcludeClip(const Rect: TRectF): TStatus; overload;
    function ExcludeClip(const Rect: TRectI): TStatus; overload;
    function ExcludeClip(Region: IRegion): TStatus; overload;
    function ResetClip: TStatus;
    function TranslateClip(DX, DY: Single): TStatus; overload;
    function TranslateClip(DX, DY: Integer): TStatus; overload;
    function GetClip(Region: IRegion): TStatus;
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

  TAdjustableArrowCap = class(TCustomLineCap, IAdjustableArrowCap)
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

{ TMetafile }

  TMetafile = class(TImage, IMetafile)
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
  GenericSansSerifFontFamily: IFontFamily = nil;
  GenericSerifFontFamily: IFontFamily = nil;
  GenericMonospaceFontFamily: IFontFamily = nil;
  GenericTypoGraphicStringFormatBuffer: IStringFormat = nil;
  GenericDefaultStringFormatBuffer: IStringFormat = nil;

{ TImageAttributes }

constructor TImageAttributes.Create;
begin
  inherited Create;
  FLastStatus := GdipCreateImageAttributes(FNativeImageAttr);
end;

destructor TImageAttributes.Destroy;
begin
  GdipDisposeImageAttributes(FNativeImageAttr);
  inherited Destroy;
end;

function TImageAttributes.Clone: IImageAttributes;
var
  Clone: GpImageAttributes;
begin
  SetStatus(GdipCloneImageAttributes(FNativeImageAttr, Clone));
  Result := TImageAttributes.Create(Clone, FLastStatus);
end;

function TImageAttributes.SetToIdentity(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesToIdentity(FNativeImageAttr, Type_));
end;

function TImageAttributes.Reset(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdiPresetImageAttributes(FNativeImageAttr, Type_));
end;

function TImageAttributes.SetColorMatrix(const ColorMatrix: TColorMatrix;
  mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr,
    Type_, True, @ColorMatrix, nil, mode));
end;

function TImageAttributes.ClearColorMatrix(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr, Type_,
    False, nil, nil, ColorMatrixFlagsDefault));
end;

function TImageAttributes.SetColorMatrices(const ColorMatrix: TColorMatrix;
  const GrayMatrix: TColorMatrix; Mode: TColorMatrixFlags = ColorMatrixFlagsDefault;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr, Type_,
    True, @ColorMatrix, @GrayMatrix, Mode));
end;

function TImageAttributes.ClearColorMatrices(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorMatrix(FNativeImageAttr,
    Type_, False, nil, nil, ColorMatrixFlagsDefault));
end;

function TImageAttributes.SetThreshold(threshold: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(FNativeImageAttr, Type_,
    True, threshold));
end;

function TImageAttributes.ClearThreshold(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesThreshold(FNativeImageAttr, Type_,
    False, 0));
end;

function TImageAttributes.SetGamma(gamma: Single; Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(FNativeImageAttr, Type_, True, gamma));
end;

function TImageAttributes.ClearGamma(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesGamma(FNativeImageAttr, Type_, False, 0));
end;

function TImageAttributes.SetNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(FNativeImageAttr, Type_, True));
end;

function TImageAttributes.ClearNoOp(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesNoOp(FNativeImageAttr, Type_, False));
end;

function TImageAttributes.SetColorKey(ColorLow, ColorHigh: TArgb;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(FNativeImageAttr, Type_,
    True, ColorLow, ColorHigh));
end;

function TImageAttributes.ClearColorKey(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesColorKeys(FNativeImageAttr, Type_,
    False, 0, 0));
end;

function TImageAttributes.SetOutputChannel(channelFlags: TColorChannelFlags;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    Type_, True, channelFlags));
end;

function TImageAttributes.ClearOutputChannel(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannel(FNativeImageAttr,
    Type_, False, ColorChannelFlagsLast));
end;

function TImageAttributes.SetOutputChannelColorProfile(ColorProfileFilename: WideString;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    Type_, True, PWideChar(ColorProfileFilename)));
end;

function TImageAttributes.ClearOutputChannelColorProfile(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesOutputChannelColorProfile(FNativeImageAttr,
    Type_, False, nil));
end;

function TImageAttributes.SetRemapTable(mapSize: cardinal; map: PColorMap;
  Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(FNativeImageAttr, Type_,
    True, mapSize, map));
end;

function TImageAttributes.ClearRemapTable(Type_: TColorAdjustType = ColorAdjustTypeDefault): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesRemapTable(FNativeImageAttr, Type_,
    False, 0, nil));
end;

function TImageAttributes.SetBrushRemapTable(MapSize: cardinal; Map: PColorMap): TStatus;
begin
  Result := SetRemapTable(MapSize, Map, ColorAdjustTypeBrush);
end;

function TImageAttributes.ClearBrushRemapTable: TStatus;
begin
  Result := ClearRemapTable(ColorAdjustTypeBrush);
end;

function TImageAttributes.SetWrapMode(Wrap: TWrapMode; Color: TArgb = aclBlack;
  clamp: Boolean = False): TStatus;
begin
  Result := SetStatus(GdipSetImageAttributesWrapMode(FNativeImageAttr, Wrap, Color, clamp));
end;

function TImageAttributes.GetAdjustedPalette(ColorPalette: PColorPalette;
  ColorAdjustType: TColorAdjustType): TStatus;
begin
  Result := SetStatus(GdipGetImageAttributesAdjustedPalette(FNativeImageAttr,
    ColorPalette, ColorAdjustType));
end;

constructor TImageAttributes.Create(ImageAttr: GpImageAttributes; Status: TStatus);
begin
  inherited Create;
  SetNativeImageAttr(ImageAttr);
  FLastStatus := Status;
end;

function TImageAttributes.GetNativeImageAttr: GpImageAttributes;
begin
  Result := FNativeImageAttr;
end;

procedure TImageAttributes.SetNativeImageAttr(ImageAttr: GpImageAttributes);
begin
  Self.FNativeImageAttr := ImageAttr;
end;

constructor TMatrix.Create;
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix(Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TMatrix.Create(M11, M12, M21, M22, DX, DY: Single);
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix2(M11, M12, M21, M22, DX, DY, Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TMatrix.Create(const Rect: TRectF; const PlgPts: TPointF);
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix3(@Rect, @PlgPts, Matrix);
  SetNativeMatrix(Matrix);
end;

constructor TMatrix.Create(const Rect: TRectI; const PlgPts: TPointI);
var
  Matrix: GpMatrix;
begin
  inherited Create;
  Matrix := nil;
  FLastStatus := GdipCreateMatrix3I(@Rect, @PlgPts, Matrix);
  SetNativeMatrix(Matrix);
end;

destructor TMatrix.Destroy;
begin
  GdipDeleteMatrix(FNativeMatrix);
end;

function TMatrix.Clone: IMatrix;
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
  Result := TMatrix.Create(CloneMatrix) as IMatrix;
end;

function TMatrix.GetElements(const M: TMatrixArray): TStatus;
begin
  Result := SetStatus(GdipGetMatrixElements(FNativeMatrix, @M));
end;

function TMatrix.SetElements(M11, M12, M21, M22, DX, DY: Single): TStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(FNativeMatrix,
    M11, M12, M21, M22, DX, DY));
end;

function TMatrix.OffsetX: Single;
var
  elements: TMatrixArray;
begin
  if (GetElements(elements) = Ok) then
    Result := elements[4]
  else
    Result := 0;
end;

function TMatrix.OffsetY: Single;
var
  elements: TMatrixArray;
begin
  if (GetElements(elements) = Ok) then
    Result := elements[5]
  else
    Result := 0;
end;

function TMatrix.Reset: TStatus;
begin
  Result := SetStatus(GdipSetMatrixElements(FNativeMatrix, 1, 0, 0, 1,
    0, 0));
end;

function TMatrix.Multiply(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyMatrix(FNativeMatrix, Matrix.NativeMatrix, Order));
end;

function TMatrix.Translate(OffsetX, OffsetY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateMatrix(FNativeMatrix, OffsetX, OffsetY, Order));
end;

function TMatrix.Scale(ScaleX, ScaleY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleMatrix(FNativeMatrix, ScaleX, ScaleY, Order));
end;

function TMatrix.Rotate(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateMatrix(FNativeMatrix, Angle, Order));
end;

function TMatrix.RotateAt(Angle: Single; const center: TPointF; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
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

function TMatrix.Shear(ShearX, ShearY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipShearMatrix(FNativeMatrix, ShearX, ShearY, Order));
end;

function TMatrix.Invert: TStatus;
begin
  Result := SetStatus(GdipInvertMatrix(FNativeMatrix));
end;

function TMatrix.TransformPoints(Pts: PPointF; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPoints(FNativeMatrix, Pts, Count));
end;

function TMatrix.TransformPoints(Pts: PPointI; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipTransformMatrixPointsI(FNativeMatrix, Pts, Count));
end;

function TMatrix.TransformVectors(Pts: PPointF; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPoints(FNativeMatrix, Pts, Count));
end;

function TMatrix.TransformVectors(Pts: PPointI; Count: Integer = 1): TStatus;
begin
  Result := SetStatus(GdipVectorTransformMatrixPointsI(FNativeMatrix, Pts, Count));
end;

function TMatrix.IsInvertible: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsMatrixInvertible(FNativeMatrix, B));
  Result := B;
end;

function TMatrix.IsIdentity: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsMatrixIdentity(FNativeMatrix, B));
  Result := B;
end;

function TMatrix.Equals(Matrix: IMatrix): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsMatrixEqual(FNativeMatrix, Matrix.NativeMatrix, B));
  Result := B;
end;

constructor TMatrix.Create(Matrix: GpMatrix);
begin
  inherited Create;
  FLastStatus := Ok;
  SetNativeMatrix(Matrix);
end;

function TMatrix.GetNativeMatrix: GpMatrix;
begin
  Result := FNativeMatrix;
end;

procedure TMatrix.SetNativeMatrix(Matrix: GpMatrix);
begin
  Self.FNativeMatrix := Matrix;
end;

constructor TStringFormat.Create(FormatFlags: Integer = 0; Language: LangId = LANG_NEUTRAL);
begin
  inherited Create;
  FNativeFormat := nil;
  FLastStatus := GdipCreateStringFormat(FormatFlags, Language, FNativeFormat);
end;

class function TStringFormat.GenericDefault: IStringFormat;
var
  F: GpStringFormat;
  S: TStatus;
begin
  if GenericDefaultStringFormatBuffer = nil then
  begin
    GenericDefaultStringFormatBuffer := TStringFormat.Create;
    S := GdipStringFormatGetGenericDefault(F);
    GenericDefaultStringFormatBuffer.NativeFormat := F;
    GenericDefaultStringFormatBuffer.LastStatus := S;
  end;
  Result := GenericDefaultStringFormatBuffer;
end;

class function TStringFormat.GenericTypoGraphic: IStringFormat;
var
  F: GpStringFormat;
  S: TStatus;
begin
  if GenericTypoGraphicStringFormatBuffer = nil then
  begin
    GenericTypoGraphicStringFormatBuffer := TStringFormat.Create;
    S := GdipStringFormatGetGenericTypoGraphic(F);
    GenericDefaultStringFormatBuffer.NativeFormat := F;
    GenericDefaultStringFormatBuffer.LastStatus := S;
  end;
  Result := GenericTypoGraphicStringFormatBuffer;
end;

constructor TStringFormat.Create(Format: IStringFormat);
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

function TStringFormat.Clone: IStringFormat;
var
  ClonedStringFormat: GpStringFormat;
begin
  ClonedStringFormat := nil;
  FLastStatus := GdipCloneStringFormat(FNativeFormat, ClonedStringFormat);
  if FLastStatus = Ok then
    Result := TStringFormat.Create(ClonedStringFormat, FLastStatus)
  else
    Result := nil;
end;

destructor TStringFormat.Destroy;
begin
  GdipDeleteStringFormat(FNativeFormat);
end;

function TStringFormat.SetFormatFlags(Flags: Integer): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatFlags(FNativeFormat, Flags));
end;

function TStringFormat.GetFormatFlags: Integer;
begin
  SetStatus(GdipGetStringFormatFlags(FNativeFormat, Result));
end;

function TStringFormat.SetAlignment(Align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatAlign(FNativeFormat, Align));
end;

function TStringFormat.GetAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatAlign(FNativeFormat, Result));
end;

function TStringFormat.SetLineAlignment(Align: TStringAlignment): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatLineAlign(FNativeFormat, Align));
end;

function TStringFormat.GetLineAlignment: TStringAlignment;
begin
  SetStatus(GdipGetStringFormatLineAlign(FNativeFormat, Result));
end;

function TStringFormat.SetHotkeyPrefix(hotkeyPrefix: THotkeyPrefix): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatHotkeyPrefix(FNativeFormat, Integer(hotkeyPrefix)));
end;

function TStringFormat.GetHotkeyPrefix: THotkeyPrefix;
var
  HotkeyPrefix: Integer;
begin
  SetStatus(GdipGetStringFormatHotkeyPrefix(FNativeFormat, HotkeyPrefix));
  Result := THotkeyPrefix(HotkeyPrefix);
end;

function TStringFormat.SetTabStops(FirstTabOffset: Single; Count: Integer; TabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTabStops(FNativeFormat, FirstTabOffset, Count, TabStops));
end;

function TStringFormat.GetTabStopCount: Integer;
begin
  SetStatus(GdipGetStringFormatTabStopCount(FNativeFormat, Result));
end;

function TStringFormat.GetTabStops(Count: Integer; FirstTabOffset, TabStops: PSingle): TStatus;
begin
  Result := SetStatus(GdipGetStringFormatTabStops(FNativeFormat, Count, FirstTabOffset, TabStops));
end;

function TStringFormat.SetDigitSubstitution(Language: LangId; substitute: TStringDigitSubstitute): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatDigitSubstitution(FNativeFormat, Language, substitute));
end;

function TStringFormat.GetDigitSubstitutionLanguage: LangId;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(FNativeFormat, @Result, nil));
end;

function TStringFormat.GetDigitSubstitutionMethod: TStringDigitSubstitute;
begin
  SetStatus(GdipGetStringFormatDigitSubstitution(FNativeFormat, nil, @Result));
end;

function TStringFormat.SetTrimming(Trimming: TStringTrimming): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatTrimming(FNativeFormat, Trimming));
end;

function TStringFormat.GetTrimming: TStringTrimming;
begin
  SetStatus(GdipGetStringFormatTrimming(FNativeFormat, Result));
end;

function TStringFormat.SetMeasurableCharacterRanges(rangeCount: Integer;
  ranges: PCharacterRange): TStatus;
begin
  Result := SetStatus(GdipSetStringFormatMeasurableCharacterRanges(FNativeFormat,
    rangeCount, ranges));
end;

function TStringFormat.GetMeasurableCharacterRangeCount: Integer;
begin
  SetStatus(GdipGetStringFormatMeasurableCharacterRangeCount(FNativeFormat, Result));
end;

function TStringFormat.GetNativeFormat: GpStringFormat;
begin
  Result := FNativeFormat;
end;

procedure TStringFormat.SetNativeFormat(Value: GpStringFormat);
begin
  FNativeFormat := Value;
end;

constructor TStringFormat.Create(ClonedStringFormat: GpStringFormat; Status: TStatus);
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

constructor TMetafile.Create(Wmf: HMETAFILE;
  var wmfPlaceableFileHeader: TWmfPlaceableFileHeader; DeleteWmf: Boolean = False);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromWmf(Wmf, DeleteWmf, @wmfPlaceableFileHeader, Metafile);
  SetNativeImage(Metafile);
end;

constructor TMetafile.Create(Emf: HENHMETAFILE; DeleteEmf: Boolean = False);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromEmf(Emf, DeleteEmf, Metafile);
  SetNativeImage(Metafile);
end;

constructor TMetafile.Create(Filename: WideString);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromFile(PWideChar(Filename), Metafile);
  SetNativeImage(Metafile);
end;

constructor TMetafile.Create(Filename: WideString; var wmfPlaceableFileHeader: TWmfPlaceableFileHeader);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromWmfFile(PWideChar(Filename), @wmfPlaceableFileHeader, Metafile);
  SetNativeImage(Metafile);
end;

constructor TMetafile.Create(Stream: IStream);
var
  Metafile: GpMetafile;
begin
  inherited Create;
  Metafile := nil;
  FLastStatus := GdipCreateMetafileFromStream(Stream, Metafile);
  SetNativeImage(Metafile);
end;

constructor TMetafile.Create(RefDC: HDC; Type_: TEmfType = EmfTypeEmfPlusDual;
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

constructor TMetafile.Create(RefDC: HDC; FrameRect: TRectF;
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

constructor TMetafile.Create(RefDC: HDC; FrameRect: TRectI;
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

constructor TMetafile.Create(Filename: WideString; RefDC: HDC;
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

constructor TMetafile.Create(Filename: WideString; RefDC: HDC; FrameRect: TRectF;
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

constructor TMetafile.Create(Filename: WideString; RefDC: HDC; FrameRect: TRectI;
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

constructor TMetafile.Create(Stream: IStream; RefDC: HDC;
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

constructor TMetafile.Create(Stream: IStream; RefDC: HDC; FrameRect: TRectF;
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

constructor TMetafile.Create(Stream: IStream; RefDC: HDC; FrameRect: TRectI;
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

function TMetafile.GetHENHMETAFILE: HENHMETAFILE;
begin
  SetStatus(GdipGetHemfFromMetafile(FNativeImage, Result));
end;

function TMetafile.PlayRecord(recordType: TEmfPlusRecordType; Flags, dataSize: UInt;
  data: PByte): TStatus;
begin
  Result := SetStatus(GdipPlayMetafileRecord(GpMetafile(FNativeImage),
    recordType, Flags, dataSize, data));
end;

function TMetafile.SetDownLevelRasterizationLimit(MetafileRasterizationLimitDPI: UInt): TStatus;
begin
  Result := SetStatus(GdipSetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), MetafileRasterizationLimitDPI));
end;

function TMetafile.GetDownLevelRasterizationLimit: UInt;
var
  MetafileRasterizationLimitDPI: UInt;
begin
  MetafileRasterizationLimitDPI := 0;
  SetStatus(GdipGetMetafileDownLevelRasterizationLimit(
    GpMetafile(FNativeImage), MetafileRasterizationLimitDPI));
  Result := MetafileRasterizationLimitDPI;
end;

function TMetafile.EmfToWmfBits(Emf: HENHMETAFILE; cbData16: UInt; pData16: PByte;
  MapMode: Integer = MM_ANISOTROPIC; eFlags: TEmfToWmfBitsFlags = EmfToWmfBitsFlagsDefault): UInt;
begin
  Result := GdipEmfToWmfBits(Emf, cbData16, pData16, MapMode, Integer(eFlags));
end;

constructor TMetafile.Create;
begin
  inherited Create;
  SetNativeImage(nil);
  FLastStatus := Ok;
end;

function TRegion.GetNativeRegion: GpRegion;
begin
  Result := FNativeRegion;
end;

constructor TRegion.Create;
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegion(Region);
  SetNativeRegion(Region);
end;

constructor TRegion.Create(Rect: TRectF);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionRect(@Rect, Region);
  SetNativeRegion(Region);
end;

constructor TRegion.Create(Rect: TRectI);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionRectI(@Rect, Region);
  SetNativeRegion(Region);
end;

constructor TRegion.Create(Path: IGraphicsPath);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionPath(Path.NativePath, Region);
  SetNativeRegion(Region);
end;

constructor TRegion.Create(RegionData: PByte; Size: Integer);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionRgnData(RegionData, Size, Region);
  SetNativeRegion(Region);
end;

constructor TRegion.Create(Rgn: HRGN);
var
  Region: GpRegion;
begin
  inherited Create;
  Region := nil;
  FLastStatus := GdipCreateRegionHrgn(Rgn, Region);
  SetNativeRegion(Region);
end;

function TRegion.FromHRGN(Rgn: HRGN): IRegion;
var
  Region: GpRegion;
begin
  Region := nil;
  if GdipCreateRegionHrgn(Rgn, Region) = Ok then
  begin
    Result := TRegion.Create(Region);
    if Result = nil then
      GdipDeleteRegion(Region);
    Exit;
  end
  else
    Result := nil;
end;

destructor TRegion.Destroy;
begin
  GdipDeleteRegion(FNativeRegion);
end;

function TRegion.Clone: IRegion;
var
  Region: GpRegion;
begin
  Region := nil;
  SetStatus(GdipCloneRegion(FNativeRegion, Region));
  Result := TRegion.Create(Region);
end;

function TRegion.MakeInfinite: TStatus;
begin
  Result := SetStatus(GdipSetInfinite(FNativeRegion));
end;

function TRegion.MakeEmpty: TStatus;
begin
  Result := SetStatus(GdipSetEmpty(FNativeRegion));
end;

function TRegion.GetDataSize: UInt;
var
  BufferSize: UInt;
begin
  BufferSize := 0;
  SetStatus(GdipGetRegionDataSize(FNativeRegion, BufferSize));
  Result := BufferSize;
end;

function TRegion.GetData(Buffer: PByte; BufferSize: UInt; SizeFilled: PUInt = nil): TStatus;
begin
  Result := SetStatus(GdipGetRegionData(FNativeRegion, Buffer, BufferSize, SizeFilled));
end;

function TRegion.Intersect(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeIntersect));
end;

function TRegion.Intersect(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeIntersect));
end;

function TRegion.Intersect(Path: IGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath,
    CombineModeIntersect));
end;

function TRegion.Intersect(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion, Region.NativeRegion,
    CombineModeIntersect));
end;

function TRegion.Union(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeUnion));
end;

function TRegion.Union(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeUnion));
end;

function TRegion.Union(Path: IGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath, CombineModeUnion));
end;

function TRegion.Union(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion, Region.NativeRegion,
    CombineModeUnion));
end;

function TRegion.Xor_(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeXor));
end;

function TRegion.Xor_(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeXor));
end;

function TRegion.Xor_(Path: IGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath, CombineModeXor));
end;

function TRegion.Xor_(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion, Region.NativeRegion,
    CombineModeXor));
end;

function TRegion.Exclude(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect, CombineModeExclude));
end;

function TRegion.Exclude(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect, CombineModeExclude));
end;

function TRegion.Exclude(Path: IGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion, Path.NativePath, CombineModeExclude));
end;

function TRegion.Exclude(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion,
    Region.NativeRegion,
    CombineModeExclude));
end;

function TRegion.Complement(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRectI(FNativeRegion, @Rect,
    CombineModeComplement));
end;

function TRegion.Complement(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRect(FNativeRegion, @Rect,
    CombineModeComplement));
end;

function TRegion.Complement(Path: IGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipCombineRegionPath(FNativeRegion,
    Path.NativePath,
    CombineModeComplement));
end;

function TRegion.Complement(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipCombineRegionRegion(FNativeRegion,
    Region.NativeRegion,
    CombineModeComplement));
end;

function TRegion.Translate(DX, DY: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateRegion(FNativeRegion, DX, DY));
end;

function TRegion.Translate(DX, DY: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateRegionI(FNativeRegion, DX, DY));
end;

function TRegion.Transform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipTransformRegion(FNativeRegion,
    Matrix.NativeMatrix));
end;

function TRegion.GetBounds(out Rect: TRectI; G: IGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBoundsI(FNativeRegion,
    G.NativeGraphics, @Rect));
end;

function TRegion.GetBounds(out Rect: TRectF; G: IGraphics): TStatus;
begin
  Result := SetStatus(GdipGetRegionBounds(FNativeRegion,
    G.NativeGraphics, @Rect));
end;

function TRegion.GetHRGN(G: IGraphics): HRGN;
begin
  SetStatus(GdipGetRegionHRgn(FNativeRegion, G.NativeGraphics, Result));
end;

function TRegion.IsEmpty(G: IGraphics): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsEmptyRegion(FNativeRegion, G.NativeGraphics, B));
  Result := B;
end;

function TRegion.IsInfinite(G: IGraphics): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsInfiniteRegion(FNativeRegion, G.NativeGraphics, B));
  Result := B;
end;

function TRegion.IsVisible(X, Y: Integer; G: IGraphics = nil): Boolean;
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

function TRegion.IsVisible(const Point: TPointI; G: IGraphics = nil): Boolean;
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

function TRegion.IsVisible(X, Y: Single; G: IGraphics = nil): Boolean;
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

function TRegion.IsVisible(const Point: TPointF; G: IGraphics = nil): Boolean;
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

function TRegion.IsVisible(X, Y, Width, Height: Integer; G: IGraphics): Boolean;
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

function TRegion.IsVisible(const Rect: TRectI; G: IGraphics = nil): Boolean;
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

function TRegion.IsVisible(X, Y, Width, Height: Single; G: IGraphics = nil): Boolean;
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

function TRegion.IsVisible(const Rect: TRectF; G: IGraphics = nil): Boolean;
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

function TRegion.Equals(Region: IRegion; G: IGraphics): Boolean;
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

function TRegion.GetRegionScansCount(Matrix: IMatrix): UInt;
var
  Count: UInt;
begin
  Count := 0;
  SetStatus(GdipGetRegionScansCount(FNativeRegion, Count, Matrix.NativeMatrix));
  Result := Count;
end;

function TRegion.GetRegionScans(Matrix: IMatrix; Rects: PRectF; out Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScans(FNativeRegion,
    Rects,
    Count,
    Matrix.NativeMatrix));
end;

function TRegion.GetRegionScans(Matrix: IMatrix; Rects: PRectI; out Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRegionScansI(FNativeRegion,
    Rects,
    Count,
    Matrix.NativeMatrix));
end;

constructor TRegion.Create(Region: GpRegion);
begin
  inherited Create;
  SetNativeRegion(Region);
end;

procedure TRegion.SetNativeRegion(Region: GpRegion);
begin
  Self.FNativeRegion := Region;
end;

constructor TCustomLineCap.Create(FillPath, StrokePath: IGraphicsPath;
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

function TCustomLineCap.Clone: ICustomLineCap;
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

constructor TCachedBitmap.Create(Bitmap: IBitmap; Graphics: IGraphics);
begin
  inherited Create;
  FNativeCachedBitmap := nil;
  FLastStatus := GdipCreateCachedBitmap(
    GpBitmap(Bitmap.NativeImage),
    Graphics.NativeGraphics,
    FNativeCachedBitmap);
end;

destructor TCachedBitmap.Destroy;
begin
  GdipDeleteCachedBitmap(FNativeCachedBitmap);
end;

function TCachedBitmap.GetNativeCachedBitmap: GpCachedBitmap;
begin
  Result := FNativeCachedBitmap;
end;

{ TPen }

constructor TPen.Create(Pen: GpPen; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativePen := Pen;
end;

constructor TPen.Create(Color: TArgb; Width: Single = 1);
var
  Unit_: TUnit;
begin
  inherited Create;
  Unit_ := UnitWorld;
  FNativePen := nil;
  FLastStatus := GdipCreatePen1(Color, Width, Unit_, FNativePen);
end;

constructor TPen.Create(Brush: IBrush; Width: Single = 1);
var
  Unit_: TUnit;
begin
  inherited Create;
  Unit_ := UnitWorld;
  FNativePen := nil;
  FLastStatus := GdipCreatePen2(Brush.NativeBrush, Width, Unit_, FNativePen);
end;

destructor TPen.Destroy;
begin
  GdipDeletePen(FNativePen);
  inherited Destroy;
end;

function TPen.GetNativePen: GpPen;
begin
  Result := FNativePen;
end;

procedure TPen.SetNativePen(Pen: GpPen);
begin
  Self.FNativePen := Pen;
end;

function TPen.Clone: IPen;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  FLastStatus := GdipClonePen(FNativePen, NativePen);
  Result := TPen.Create(NativePen, FLastStatus);
end;

function TPen.SetWidth(Width: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenWidth(FNativePen, Width));
end;

function TPen.GetWidth: Single;
begin
  SetStatus(GdipGetPenWidth(FNativePen, Result));
end;

function TPen.SetLineCap(StartCap, EndCap: TLineCap; DashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenLineCap197819(FNativePen, StartCap, EndCap, DashCap));
end;

function TPen.SetStartCap(StartCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenStartCap(FNativePen, StartCap));
end;

function TPen.SetEndCap(EndCap: TLineCap): TStatus;
begin
  Result := SetStatus(GdipSetPenEndCap(FNativePen, EndCap));
end;

function TPen.SetDashCap(DashCap: TDashCap): TStatus;
begin
  Result := SetStatus(GdipSetPenDashCap197819(FNativePen, DashCap));
end;

function TPen.GetStartCap: TLineCap;
begin
  SetStatus(GdipGetPenStartCap(FNativePen, Result));
end;

function TPen.GetEndCap: TLineCap;
begin
  SetStatus(GdipGetPenEndCap(FNativePen, Result));
end;

function TPen.GetDashCap: TDashCap;
begin
  SetStatus(GdipGetPenDashCap197819(FNativePen, Result));
end;

function TPen.SetLineJoin(lineJoin: TLineJoin): TStatus;
begin
  Result := SetStatus(GdipSetPenLineJoin(FNativePen, lineJoin));
end;

function TPen.GetLineJoin: TLineJoin;
begin
  SetStatus(GdipGetPenLineJoin(FNativePen, Result));
end;

function TPen.SetCustomStartCap(CustomCap: ICustomLineCap): TStatus;
var
  NativeCap: GpCustomLineCap;
begin
  NativeCap := nil;
  if CustomCap <> nil then
    NativeCap := CustomCap.NativeCap;
  Result := SetStatus(GdipSetPenCustomStartCap(FNativePen, NativeCap));
end;

function TPen.GetCustomStartCap(CustomCap: ICustomLineCap): TStatus;
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

function TPen.SetCustomEndCap(CustomCap: ICustomLineCap): TStatus;
var
  NativeCap: GpCustomLineCap;
begin
  NativeCap := nil;
  if CustomCap <> nil then
    NativeCap := CustomCap.NativeCap;
  Result := SetStatus(GdipSetPenCustomEndCap(FNativePen, NativeCap));
end;

function TPen.GetCustomEndCap(CustomCap: ICustomLineCap): TStatus;
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

function TPen.SetMiterLimit(MiterLimit: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenMiterLimit(FNativePen, MiterLimit));
end;

function TPen.GetMiterLimit: Single;
begin
  SetStatus(GdipGetPenMiterLimit(FNativePen, Result));
end;

function TPen.SetAlignment(penAlignment: TPenAlignment): TStatus;
begin
  Result := SetStatus(GdipSetPenMode(FNativePen, penAlignment));
end;

function TPen.GetAlignment: TPenAlignment;
begin
  SetStatus(GdipGetPenMode(FNativePen, Result));
end;

function TPen.SetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipSetPenTransform(FNativePen, Matrix.NativeMatrix));
end;

function TPen.GetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipGetPenTransform(FNativePen, Matrix.NativeMatrix));
end;

function TPen.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetPenTransform(FNativePen));
end;

function TPen.MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPenTransform(FNativePen, Matrix.NativeMatrix, Order));
end;

function TPen.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePenTransform(FNativePen, DX, DY, Order));
end;

function TPen.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePenTransform(FNativePen, SX, SY, Order));
end;

function TPen.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePenTransform(FNativePen, Angle, Order));
end;

function TPen.GetPenType: TPenType;
begin
  SetStatus(GdipGetPenFillType(FNativePen, Result));
end;

function TPen.SetColor(Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipSetPenColor(FNativePen, Color));
end;

function TPen.SetBrush(Brush: IBrush): TStatus;
begin
  Result := SetStatus(GdipSetPenBrushFill(FNativePen, Brush.NativeBrush));
end;

function TPen.GetColor(out Color: TArgb): TStatus;
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

function TPen.GetBrush: IBrush;
var
  PenType: TPenType;
  Brush: IBrush;
  FNativeBrush: GpBrush;
begin
  PenType := GetPenType;
  Brush := nil;
  case PenType of
    PenTypeSoliDColor: Brush := TSolidBrush.Create;
    PenTypeHatchFill: Brush := THatchBrush.Create;
    PenTypeTextureFill: Brush := TTextureBrush.Create;
    PenTypePathGradient: Brush := TBrush.Create;
    PenTypeLinearGradient: Brush := TLinearGradientBrush.Create;
  end;
  if Brush <> nil then
  begin
    SetStatus(GdipGetPenBrushFill(FNativePen, FNativeBrush));
    Brush.SetNativeBrush(FNativeBrush);
  end;
  Result := Brush;
end;

function TPen.GetDashStyle: TDashStyle;
begin
  SetStatus(GdipGetPenDashStyle(FNativePen, Result));
end;

function TPen.SetDashStyle(DashStyle: TDashStyle): TStatus;
begin
  Result := SetStatus(GdipSetPenDashStyle(FNativePen, DashStyle));
end;

function TPen.GetDashOffset: Single;
begin
  SetStatus(GdipGetPenDashOffset(FNativePen, Result));
end;

function TPen.SetDashOffset(DashOffset: Single): TStatus;
begin
  Result := SetStatus(GdipSetPenDashOffset(FNativePen, DashOffset));
end;

function TPen.SetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenDashArray(FNativePen, DashArray, Count));
end;

function TPen.GetDashPatternCount: Integer;
begin
  SetStatus(GdipGetPenDashCount(FNativePen, Result));
end;

function TPen.GetDashPattern(DashArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPenDashArray(FNativePen, DashArray, Count));
end;

function TPen.SetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPenCompoundArray(FNativePen, CompoundArray, Count));
end;

function TPen.GetCompoundArrayCount: Integer;
begin
  SetStatus(GdipGetPenCompoundCount(FNativePen, Result));
end;

function TPen.GetCompoundArray(CompoundArray: PSingle; Count: Integer): TStatus;
begin
  if Count <= 0 then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetPenCompoundArray(FNativePen, CompoundArray, Count));
end;

{ TBrush }

constructor TBrush.Create;
begin
  inherited Create;
end;

constructor TBrush.Create(Brush: GpBrush; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativeBrush := Brush;
end;

destructor TBrush.Destroy;
begin
  GdipDeleteBrush(FNativeBrush);
  inherited Destroy;
end;

function TBrush.GetNativeBrush: GpBrush;
begin
  Result := Self.FNativeBrush;
end;

procedure TBrush.SetNativeBrush(Brush: GpBrush);
begin
  Self.FNativeBrush := Brush;
end;

function TBrush.Clone: IBrush;
var
  NativeBrush: GpBrush;
  NewBrush: IBrush;
begin
  NativeBrush := nil;
  SetStatus(GdipCloneBrush(FNativeBrush, NativeBrush));
  NewBrush := TBrush.Create(NativeBrush, FLastStatus);
  if NewBrush = nil then
    GdipDeleteBrush(NativeBrush);
  Result := NewBrush;
end;

function TBrush.GetType: TBrushType;
begin
  Result := TBrushType(-1);
  SetStatus(GdipGetBrushType(FNativeBrush, Result));
end;

{ TSolidBrush }

constructor TSolidBrush.Create;
begin
  inherited Create;
end;

constructor TSolidBrush.Create(Color: TArgb);
var
  Brush: GpSolidFill;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateSolidFill(Color, Brush);
  SetNativeBrush(Brush);
end;

function TSolidBrush.GetColor(out Color: TArgb): TStatus;
begin
  SetStatus(GdipGetSolidFillColor(GPSOLIDFill(FNativeBrush), Color));
  Result := FLastStatus;
end;

function TSolidBrush.SetColor(Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipSetSolidFillColor(GpSolidFill(FNativeBrush),
    Color));
end;

{ TTextureBrush }

constructor TTextureBrush.Create(Image: IImage; WrapMode: TWrapMode = WrapModeTile);
var
  Texture: GpTexture;
begin
  inherited Create;
  FLastStatus := GdipCreateTexture(Image.NativeImage, WrapMode, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IImage; WrapMode: TWrapMode; DstRect: TRectF);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2(Image.NativeImage, WrapMode, DstRect.X,
    DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IImage; DstRect: TRectF; ImageAttributes: IImageAttributes = nil);
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

constructor TTextureBrush.Create(Image: IImage; DstRect: TRectI; ImageAttributes: IImageAttributes = nil);
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

constructor TTextureBrush.Create(Image: IImage; WrapMode: TWrapMode; DstRect: TRectI);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2I(Image.NativeImage, WrapMode, DstRect.X,
    DstRect.Y, DstRect.Width, DstRect.Height, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IImage; WrapMode: TWrapMode; DstX, DstY, DstWidth, DstHeight: Single);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2(Image.NativeImage, WrapMode, DstX, DstY,
    DstWidth, DstHeight, Texture);
  SetNativeBrush(Texture);
end;

constructor TTextureBrush.Create(Image: IImage; WrapMode: TWrapMode; DstX, DstY, DstWidth, DstHeight: Integer);
var
  Texture: GpTexture;
begin
  inherited Create;
  Texture := nil;
  FLastStatus := GdipCreateTexture2I(Image.NativeImage, WrapMode, DstX, DstY,
    DstWidth, DstHeight, Texture);
  SetNativeBrush(Texture);
end;

function TTextureBrush.SetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipSetTextureTransform(GpTexture(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TTextureBrush.GetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipGetTextureTransform(GpTexture(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TTextureBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetTextureTransform(GpTexture(FNativeBrush)));
end;

function TTextureBrush.MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
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

function TTextureBrush.GetImage: IImage;
var
  Image: GpImage;
begin
  SetStatus(GdipGetTextureImage(GpTexture(FNativeBrush), Image));
  Result := TImage.Create(Image, FLastStatus);
  if (Result = nil) then
    GdipDisposeImage(Image);
end;

constructor TTextureBrush.Create;
begin
  inherited Create;
end;

{ TLinearGradientBrush }

constructor TLinearGradientBrush.Create;
begin
  inherited Create;
end;

constructor TLinearGradientBrush.Create(const Point1, Point2: TPointF; Color1, Color2: TArgb);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrush(@Point1, @Point2, Color1, Color2, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TLinearGradientBrush.Create(const Point1, Point2: TPointI; Color1, Color2: TArgb);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushI(@Point1, @Point2, Color1,
    Color2, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TLinearGradientBrush.Create(Rect: TRectF; Color1, Color2: TArgb; mode: TLinearGradientMode);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRect(@Rect, Color1,
    Color2, mode, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TLinearGradientBrush.Create(Rect: TRectI; Color1, Color2: TArgb; mode: TLinearGradientMode);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRectI(@Rect, Color1,
    Color2, mode, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TLinearGradientBrush.Create(Rect: TRectF; Color1, Color2: TArgb; Angle: Single; isAngleScalable: Boolean = False);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRectWithAngle(@Rect, Color1,
    Color2, Angle, isAngleScalable, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

constructor TLinearGradientBrush.Create(Rect: TRectI; Color1, Color2: TArgb; Angle: Single; isAngleScalable: Boolean = False);
var
  Brush: GpLineGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateLineBrushFromRectWithAngleI(@Rect, Color1,
    Color2, Angle, isAngleScalable, WrapModeTile, Brush);
  SetNativeBrush(Brush);
end;

function TLinearGradientBrush.SetLinearColors(Color1, Color2: TArgb): TStatus;
begin
  Result := SetStatus(GdipSetLineColors(GpLineGradient(FNativeBrush),
    Color1, Color2));
end;

function TLinearGradientBrush.GetLinearColors(out Color1, Color2: TArgb): TStatus;
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

function TLinearGradientBrush.GetRectangle(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetLineRect(GpLineGradient(FNativeBrush), @Rect));
end;

function TLinearGradientBrush.GetRectangle(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetLineRectI(GpLineGradient(FNativeBrush), @Rect));
end;

function TLinearGradientBrush.SetGammaCorrection(UseGammaCorrection: Boolean): TStatus;
begin
  Result := SetStatus(GdipSetLineGammaCorrection(GpLineGradient(FNativeBrush),
    UseGammaCorrection));
end;

function TLinearGradientBrush.GetGammaCorrection: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipGetLineGammaCorrection(GpLineGradient(FNativeBrush), B));
  Result := B;
end;

function TLinearGradientBrush.GetBlendCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetLineBlendCount(GpLineGradient(FNativeBrush), Count));
  Result := Count;
end;

function TLinearGradientBrush.SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetLineBlend(GpLineGradient(FNativeBrush),
    BlendFactors, BlendPositions, Count));
end;

function TLinearGradientBrush.GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  if ((Count <= 0) or (BlendFactors = nil) or (BlendPositions = nil)) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLineBlend(GpLineGradient(FNativeBrush), BlendFactors,
      BlendPositions, Count));
end;

function TLinearGradientBrush.GetInterpolationColorCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetLinePresetBlendCount(GpLineGradient(FNativeBrush), Count));
  Result := Count;
end;

function TLinearGradientBrush.SetInterpolationColors(PresetColors: PArgb;
  BlendPositions: PSingle; Count: Integer): TStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipSetLinePresetBlend(GpLineGradient(FNativeBrush),
      PArgb(PresetColors), BlendPositions, Count));
end;

function TLinearGradientBrush.GetInterpolationColors(PresetColors: PArgb; BlendPositions: PSingle; Count: Integer): TStatus;
begin
  if (Count <= 0) then
    Result := SetStatus(InvalidParameter)
  else
    Result := SetStatus(GdipGetLinePresetBlend(GpLineGradient(FNativeBrush),
      PArgb(PresetColors), BlendPositions, Count));
end;

function TLinearGradientBrush.SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetLineSigmaBlend(GpLineGradient(FNativeBrush), Focus, Scale));
end;

function TLinearGradientBrush.SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetLineLinearBlend(GpLineGradient(FNativeBrush), Focus, Scale));
end;

function TLinearGradientBrush.SetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipSetLineTransform(GpLineGradient(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TLinearGradientBrush.GetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipGetLineTransform(GpLineGradient(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TLinearGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetLineTransform(GpLineGradient(FNativeBrush)));
end;

function TLinearGradientBrush.MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyLineTransform(GpLineGradient(FNativeBrush),
    Matrix.NativeMatrix,
    Order));
end;

function TLinearGradientBrush.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateLineTransform(GpLineGradient(FNativeBrush),
    DX, DY, Order));
end;

function TLinearGradientBrush.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleLineTransform(GpLineGradient(FNativeBrush),
    SX, SY, Order));
end;

function TLinearGradientBrush.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateLineTransform(GpLineGradient(FNativeBrush),
    Angle, Order));
end;

function TLinearGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetLineWrapMode(GpLineGradient(FNativeBrush), WrapMode));
end;

function TLinearGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetLineWrapMode(GpLineGradient(FNativeBrush), Result));
end;

{ THatchBrush }

constructor THatchBrush.Create(HatchStyle: THatchStyle; ForeColor: TArgb; BackColor: TArgb = aclBlack);
var
  Brush: GpHatch;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreateHatchBrush(Integer(HatchStyle), ForeColor, BackColor, Brush);
  SetNativeBrush(Brush);
end;

function THatchBrush.GetHatchStyle: THatchStyle;
begin
  SetStatus(GdipGetHatchStyle(GpHatch(FNativeBrush), Result));
end;

function THatchBrush.GetForegrounDColor(out Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGetHatchForegrounDColor(GpHatch(FNativeBrush), Color));
end;

function THatchBrush.GetBackgrounDColor(out Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGetHatchBackgrounDColor(GpHatch(FNativeBrush), Color));
end;

constructor THatchBrush.Create;
begin
  inherited Create;
end;

constructor TImage.Create(Filename: WideString;
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

constructor TImage.Create(Stream: IStream;
  UseEmbeddedColorManagement: Boolean = False);
begin
  inherited Create;
  FNativeImage := nil;
  if (UseEmbeddedColorManagement) then
    FLastStatus := GdipLoadImageFromStreamICM(Stream, FNativeImage)
  else
    FLastStatus := GdipLoadImageFromStream(Stream, FNativeImage);
end;

function TImage.FromFile(Filename: WideString;
  UseEmbeddedColorManagement: Boolean = False): IImage;
begin
  Result := TImage.Create(
    PWideChar(Filename),
    UseEmbeddedColorManagement);
end;

function TImage.FromStream(Stream: IStream;
  UseEmbeddedColorManagement: Boolean = False): IImage;
begin
  Result := TImage.Create(
    Stream,
    UseEmbeddedColorManagement);
end;

destructor TImage.Destroy;
begin
  GdipDisposeImage(FNativeImage);
end;

function TImage.Clone: IImage;
var
  CloneImage: GpImage;
begin
  CloneImage := nil;
  SetStatus(GdipCloneImage(FNativeImage, CloneImage));
  Result := TImage.Create(CloneImage, FLastStatus);
end;

function TImage.Save(Filename: WideString; const clsidEncoder: TGUID;
  EncoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToFile(FNativeImage,
    PWideChar(Filename), @clsidEncoder,
    EncoderParams));
end;

function TImage.Save(Stream: IStream; const clsidEncoder: TGUID;
  EncoderParams: PEncoderParameters = nil): TStatus;
begin
  Result := SetStatus(GdipSaveImageToStream(FNativeImage,
    Stream, @clsidEncoder,
    EncoderParams));
end;

function TImage.SaveAdd(EncoderParams: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipSaveAdd(FNativeImage,
    EncoderParams));
end;

function TImage.SaveAdd(NewImage: IImage;
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

function TImage.GetType: TImageType;
begin
  SetStatus(GdipGetImageType(FNativeImage, Result));
end;

function TImage.GetPhysicalDimension(out Size: TSizeF): TStatus;
var
  Width, Height: Single;
  Status: TStatus;
begin
  Status := SetStatus(GdipGetImageDimension(FNativeImage, Width, Height));
  Size.Width := Width;
  Size.Height := Height;
  Result := Status;
end;

function TImage.GetBounds(out SrcRect: TRectF; out SrcUnit: TUnit): TStatus;
begin
  Result := SetStatus(GdipGetImageBounds(FNativeImage, @SrcRect, SrcUnit));
end;

function TImage.GetWidth: UInt;
var
  Width: UInt;
begin
  Width := 0;
  SetStatus(GdipGetImageWidth(FNativeImage, Width));
  Result := Width;
end;

function TImage.GetHeight: UInt;
var
  Height: UInt;
begin
  Height := 0;
  SetStatus(GdipGetImageHeight(FNativeImage, Height));
  Result := Height;
end;

function TImage.GetHorizontalResolution: Single;
var
  resolution: Single;
begin
  resolution := 0;
  SetStatus(GdipGetImageHorizontalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TImage.GetVerticalResolution: Single;
var
  resolution: Single;
begin
  resolution := 0;
  SetStatus(GdipGetImageVerticalResolution(FNativeImage, resolution));
  Result := resolution;
end;

function TImage.GetFlags: UInt;
var
  Flags: UInt;
begin
  Flags := 0;
  SetStatus(GdipGetImageFlags(FNativeImage, Flags));
  Result := Flags;
end;

function TImage.GetRawFormat(out Format: TGUID): TStatus;
begin
  Result := SetStatus(GdipGetImageRawFormat(FNativeImage, @Format));
end;

function TImage.GetPixelFormat: TPixelFormat;
begin
  SetStatus(GdipGetImagePixelFormat(FNativeImage, Result));
end;

function TImage.GetPaletteSize: Integer;
var
  Size: Integer;
begin
  Size := 0;
  SetStatus(GdipGetImagePaletteSize(FNativeImage, Size));
  Result := Size;
end;

function TImage.GetPalette(palette: PColorPalette; Size: Integer): TStatus;
begin
  Result := SetStatus(GdipGetImagePalette(FNativeImage, palette, Size));
end;

function TImage.SetPalette(palette: PColorPalette): TStatus;
begin
  Result := SetStatus(GdipSetImagePalette(FNativeImage, palette));
end;

function TImage.GetThumbnailImage(ThumbWidth, ThumbHeight: UInt;
  Callback: GetThumbnailImageAbort = nil;
  CallbackData: Pointer = nil): IImage;
var
  ThumbImage: GpImage;
  NewImage: IImage;
begin
  ThumbImage := nil;
  SetStatus(GdipGetImageThumbnail(FNativeImage,
    ThumbWidth, ThumbHeight,
    ThumbImage,
    Callback, CallbackData));
  NewImage := TImage.Create(ThumbImage, FLastStatus);
  if (NewImage = nil) then
    GdipDisposeImage(ThumbImage);
  Result := NewImage;
end;

function TImage.GetFrameDimensionsCount: UInt;
var
  Count: UInt;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameDimensionsCount(FNativeImage, Count));
  Result := Count;
end;

function TImage.GetFrameDimensionsList(DimensionIDs: PGUID; Count: UInt): TStatus;
begin
  Result := SetStatus(GdipImageGetFrameDimensionsList(FNativeImage, DimensionIDs, Count));
end;

function TImage.GetFrameCount(const DimensionID: TGUID): UInt;
var
  Count: UInt;
begin
  Count := 0;
  SetStatus(GdipImageGetFrameCount(FNativeImage, @DimensionID, Count));
  Result := Count;
end;

function TImage.SelectActiveFrame(const DimensionID: TGUID; FrameIndex: UInt): TStatus;
begin
  Result := SetStatus(GdipImageSelectActiveFrame(FNativeImage, @DimensionID,
    FrameIndex));
end;

function TImage.RotateFlip(rotateFlipType: TRotateFlipType): TStatus;
begin
  Result := SetStatus(GdipImageRotateFlip(FNativeImage,
    rotateFlipType));
end;

function TImage.GetPropertyCount: UInt;
var
  NumProperty: UInt;
begin
  NumProperty := 0;
  SetStatus(GdipGetPropertyCount(FNativeImage, NumProperty));
  Result := NumProperty;
end;

function TImage.GetPropertyIdList(numOfProperty: UInt; list: PPropID): TStatus;
begin
  Result := SetStatus(GdipGetPropertyIdList(FNativeImage, numOfProperty, list));
end;

function TImage.GetPropertyItemSize(PropId: PropID): UInt;
var
  Size: UInt;
begin
  Size := 0;
  SetStatus(GdipGetPropertyItemSize(FNativeImage, PropId, Size));
  Result := Size;
end;

function TImage.GetPropertyItem(PropId: PropID; PropSize: UInt;
  Buffer: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetPropertyItem(FNativeImage,
    PropId, PropSize, Buffer));
end;

function TImage.GetPropertySize(out TotalBufferSize, NumProperties: UInt): TStatus;
begin
  Result := SetStatus(GdipGetPropertySize(FNativeImage,
    TotalBufferSize, NumProperties));
end;

function TImage.GetAllPropertyItems(TotalBufferSize, NumProperties: UInt;
  AllItems: PPropertyItem): TStatus;
begin
  Result := SetStatus(GdipGetAllPropertyItems(FNativeImage,
    TotalBufferSize, NumProperties, AllItems));
end;

function TImage.RemovePropertyItem(PropId: TPropID): TStatus;
begin
  Result := SetStatus(GdipRemovePropertyItem(FNativeImage, PropId));
end;

function TImage.SetPropertyItem(const item: TPropertyItem): TStatus;
begin
  Result := SetStatus(GdipSetPropertyItem(FNativeImage, @item));
end;

function TImage.GetEncoderParameterListSize(const clsidEncoder: TGUID): UInt;
var
  Size: UInt;
begin
  Size := 0;
  SetStatus(GdipGetEncoderParameterListSize(FNativeImage, @clsidEncoder, Size));
  Result := Size;
end;

function TImage.GetEncoderParameterList(const clsidEncoder: TGUID; Size: UInt;
  Buffer: PEncoderParameters): TStatus;
begin
  Result := SetStatus(GdipGetEncoderParameterList(FNativeImage, @clsidEncoder,
    Size,
    Buffer));
end;

constructor TImage.Create;
begin
  inherited Create;
end;

constructor TImage.Create(Image: GpImage; Status: TStatus);
begin
  inherited Create;
  FNativeImage := Image;
  FLastStatus := Status;
end;

function TImage.GetNativeImage: GpImage;
begin
  Result := FNativeImage;
end;

procedure TImage.SetNativeImage(Image: GpImage);
begin
  FNativeImage := Image;
end;

constructor TBitmap.Create(Filename: WideString; UseEmbeddedColorManagement: Boolean = False);
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

constructor TBitmap.Create(Stream: IStream; UseEmbeddedColorManagement: Boolean = False);
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

function TBitmap.FromFile(Filename: WideString; UseEmbeddedColorManagement: Boolean = False): IImage;
begin
  Result := TBitmap.Create(PWideChar(Filename),UseEmbeddedColorManagement);
end;

function TBitmap.FromStream(Stream: IStream; UseEmbeddedColorManagement: Boolean = False): IImage;
begin
  Result := TBitmap.Create(Stream,UseEmbeddedColorManagement);
end;

constructor TBitmap.Create(Width, Height, Stride: Integer; Format: TPixelFormat; scan0: PByte);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromScan0(Width, Height, Stride, Format, scan0, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TBitmap.Create(Width, Height: Integer; Format: TPixelFormat = PixelFormat32bppArgb);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromScan0(Width, Height, 0, Format, nil, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TBitmap.Create(Width, Height: Integer; Target: IGraphics);
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

function TBitmap.Clone(Rect: TRectI; Format: TPixelFormat): IBitmap;
begin
  Result := Clone(Rect.X, Rect.Y, Rect.Width, Rect.Height, Format);
end;

function TBitmap.Clone(X, Y, Width, Height: Integer; Format: TPixelFormat): IBitmap;
var
  Bitmap: IBitmap;
  DstBitmap: GpBitmap;
begin
  DstBitmap := nil;
  FLastStatus := GdipCloneBitmapAreaI(X, Y, Width, Height, Format,
    FNativeImage, DstBitmap);
  if FLastStatus = Ok then
  begin
    Bitmap := TBitmap.Create(DstBitmap);
    if Bitmap = nil then
      GdipDisposeImage(DstBitmap);
    Result := Bitmap;
    Exit;
  end
  else
    Result := nil;
end;

function TBitmap.Clone(Rect: TRectF; Format: TPixelFormat): IBitmap;
begin
  Result := Clone(Rect.X, Rect.Y, Rect.Width, Rect.Height, Format);
end;

function TBitmap.Clone(X, Y, Width, Height: Single; Format: TPixelFormat): IBitmap;
var
  Bitmap: IBitmap;
  DstBitmap: GpBitmap;
begin
  DstBitmap := nil;
  SetStatus(GdipCloneBitmapArea(X, Y, Width, Height, Format,
    FNativeImage, DstBitmap));
  if FLastStatus = Ok then
  begin
    Bitmap := TBitmap.Create(DstBitmap);
    if Bitmap = nil then
      GdipDisposeImage(DstBitmap);
    Result := Bitmap;
  end
  else
    Result := nil;
end;

function TBitmap.LockBits(Rect: TRectI; Flags: UInt; Format: TPixelFormat;
  out LockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapLockBits(
    FNativeImage, @Rect, Flags, Format, @LockedBitmapData));
end;

function TBitmap.UnlockBits(var LockedBitmapData: TBitmapData): TStatus;
begin
  Result := SetStatus(GdipBitmapUnlockBits(
    FNativeImage, @LockedBitmapData));
end;

function TBitmap.GetPixel(X, Y: Integer; out Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipBitmapGetPixel(FNativeImage, X, Y, Color));
end;

function TBitmap.SetPixel(X, Y: Integer; Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipBitmapSetPixel(FNativeImage, X, Y, Color));
end;

function TBitmap.SetResolution(XDPI, YDPI: Single): TStatus;
begin
  Result := SetStatus(GdipBitmapSetResolution(FNativeImage, XDPI, YDPI));
end;

constructor TBitmap.Create(surface: IDirectDrawSurface7);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromDiRectDrawSurface(surface, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TBitmap.Create(var BitmapInfo: TBitmapINFO; BitmapData: Pointer);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromGdiDib(@BitmapInfo, BitmapData, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TBitmap.Create(Bmp: HBitmap; Pal: HPALETTE);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromHBitmap(Bmp, Pal, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TBitmap.Create(Icon: HICON);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  GdipCreateBitmapFromHICON(Icon, Bitmap);
  SetNativeImage(Bitmap);
end;

constructor TBitmap.Create(hInstance: HMODULE; BitmapName: WideString);
var
  Bitmap: GpBitmap;
begin
  inherited Create;
  Bitmap := nil;
  FLastStatus := GdipCreateBitmapFromResource(hInstance, PWideChar(BitmapName), Bitmap);
  SetNativeImage(Bitmap);
end;

function TBitmap.FromDiRectDrawSurface7(surface: IDirectDrawSurface7): IBitmap;
begin
  Result := TBitmap.Create(surface);
end;

function TBitmap.FromBitmapINFO(var BitmapInfo: TBitmapINFO; BitmapData: Pointer): IBitmap;
begin
  Result := TBitmap.Create(BitmapInfo, BitmapData);
end;

function TBitmap.FromHBitmap(Bmp: HBitmap; Pal: HPALETTE): IBitmap;
begin
  Result := TBitmap.Create(Bmp, Pal);
end;

function TBitmap.FromHICON(Icon: HICON): IBitmap;
begin
  Result := TBitmap.Create(Icon);
end;

function TBitmap.FromResource(hInstance: HMODULE; BitmapName: WideString): IBitmap;
begin
  Result := TBitmap.Create(hInstance, PWideChar(BitmapName));
end;

function TBitmap.GetHBitmap(ColorBackground: TArgb; out Bmp: HBitmap): TStatus;
begin
  Result := SetStatus(GdipCreateHBitmapFromBitmap( FNativeImage, Bmp, ColorBackground));
end;

function TBitmap.GetHICON(out Icon: HICON): TStatus;
begin
  Result := SetStatus(GdipCreateHICONFromBitmap( FNativeImage, Icon));
end;

constructor TBitmap.Create(Bitmap: GpBitmap);
begin
  inherited Create;
  FLastStatus := Ok;
  SetNativeImage(Bitmap);
end;

function TGraphics.FromHDC(DC: HDC): IGraphics;
begin
  Result := TGraphics.Create(DC);
end;

function TGraphics.FromHDC(DC: HDC; Device: THandle): IGraphics;
begin
  Result := TGraphics.Create(DC, Device);
end;

function TGraphics.FromHWND(hwnd: HWND; ICM: Boolean = False): IGraphics;
begin
  Result := TGraphics.Create(hwnd, ICM);
end;

function TGraphics.FromImage(Image: IImage): IGraphics;
begin
  Result := TGraphics.Create(Image);
end;

constructor TGraphics.Create(DC: HDC);
var
  Graphics: GpGraphics;
begin
  inherited Create;
  Graphics := nil;
  FLastStatus := GdipCreateFromHDC(DC, Graphics);
  SetNativeGraphics(Graphics);
end;

constructor TGraphics.Create(DC: HDC; Device: THandle);
var
  Graphics: GpGraphics;
begin
  inherited Create;
  Graphics := nil;
  FLastStatus := GdipCreateFromHDC2(DC, Device, Graphics);
  SetNativeGraphics(Graphics);
end;

constructor TGraphics.Create(Wnd: HWND; ICM: Boolean);
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

constructor TGraphics.Create(Image: IImage);
var
  Graphics: GpGraphics;
begin
  inherited Create;
  Graphics := nil;
  if (Image <> nil) then
    FLastStatus := GdipGetImageGraphicsConText(Image.NativeImage, Graphics);
  SetNativeGraphics(Graphics);
end;

destructor TGraphics.Destroy;
begin
  GdipDeleteGraphics(FNativeGraphics);
  inherited Destroy;
end;

procedure TGraphics.Flush(intention: TFlushIntention = FlushIntentionFlush);
begin
  GdipFlush(FNativeGraphics, intention);
end;

function TGraphics.GetHDC: HDC;
begin
  SetStatus(GdipGetDC(FNativeGraphics, Result));
end;

procedure TGraphics.ReleaseHDC(DC: HDC);
begin
  SetStatus(GdipReleaseDC(FNativeGraphics, DC));
end;

function TGraphics.SetRenderingOrigin(X, Y: Integer): TStatus;
begin
  Result := SetStatus(GdipSetRenderingOrigin(FNativeGraphics, X, Y));
end;

function TGraphics.GetRenderingOrigin(out X, Y: Integer): TStatus;
begin
  Result := SetStatus(GdipGetRenderingOrigin(FNativeGraphics, X, Y));
end;

function TGraphics.SetCompositingMode(CompositingMode: TCompositingMode): TStatus;
begin
  Result := SetStatus(GdipSetCompositingMode(FNativeGraphics,
    CompositingMode));
end;

function TGraphics.GetCompositingMode: TCompositingMode;
begin
  SetStatus(GdipGetCompositingMode(FNativeGraphics, Result));
end;

function TGraphics.SetCompositingQuality(CompositingQuality: TCompositingQuality): TStatus;
begin
  Result := SetStatus(GdipSetCompositingQuality(FNativeGraphics, CompositingQuality));
end;

function TGraphics.GetCompositingQuality: TCompositingQuality;
begin
  SetStatus(GdipGetCompositingQuality(FNativeGraphics, Result));
end;

function TGraphics.SetTextRenderingHint(NewMode: TTextRenderingHint): TStatus;
begin
  Result := SetStatus(GdipSetTextRenderingHint(FNativeGraphics, NewMode));
end;

function TGraphics.GetTextRenderingHint: TTextRenderingHint;
begin
  SetStatus(GdipGetTextRenderingHint(FNativeGraphics, Result));
end;

function TGraphics.SetTextContrast(contrast: UInt): TStatus;
begin
  Result := SetStatus(GdipSetTextContrast(FNativeGraphics, contrast));
end;

function TGraphics.GetTextContrast: UInt;
begin
  SetStatus(GdipGetTextContrast(FNativeGraphics, Result));
end;

function TGraphics.GetInterpolationMode: TInterpolationMode;
var
  mode: TInterpolationMode;
begin
  mode := InterpolationModeInvalid;
  SetStatus(GdipGetInterpolationMode(FNativeGraphics, mode));
  Result := mode;
end;

function TGraphics.SetInterpolationMode(interpolationMode: TInterpolationMode): TStatus;
begin
  Result := SetStatus(GdipSetInterpolationMode(FNativeGraphics,
    interpolationMode));
end;

function TGraphics.GetSmoothingMode: TSmoothingMode;
var
  smoothingMode: TSmoothingMode;
begin
  smoothingMode := SmoothingModeInvalid;
  SetStatus(GdipGetSmoothingMode(FNativeGraphics, smoothingMode));
  Result := smoothingMode;
end;

function TGraphics.SetSmoothingMode(smoothingMode: TSmoothingMode): TStatus;
begin
  Result := SetStatus(GdipSetSmoothingMode(FNativeGraphics, smoothingMode));
end;

function TGraphics.GetPixelOffsetMode: TPixelOffsetMode;
var
  pixelOffsetMode: TPixelOffsetMode;
begin
  pixelOffsetMode := PixelOffsetModeInvalid;
  SetStatus(GdipGetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
  Result := pixelOffsetMode;
end;

function TGraphics.SetPixelOffsetMode(pixelOffsetMode: TPixelOffsetMode): TStatus;
begin
  Result := SetStatus(GdipSetPixelOffsetMode(FNativeGraphics, pixelOffsetMode));
end;

function TGraphics.SetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipSetWorldTransform(FNativeGraphics, Matrix.NativeMatrix));
end;

function TGraphics.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetWorldTransform(FNativeGraphics));
end;

function TGraphics.MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyWorldTransform(FNativeGraphics,
    Matrix.NativeMatrix,
    Order));
end;

function TGraphics.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslateWorldTransform(FNativeGraphics,
    DX, DY, Order));
end;

function TGraphics.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScaleWorldTransform(FNativeGraphics,
    SX, SY, Order));
end;

function TGraphics.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotateWorldTransform(FNativeGraphics,
    Angle, Order));
end;

function TGraphics.GetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipGetWorldTransform(FNativeGraphics,
    Matrix.NativeMatrix));
end;

function TGraphics.SetPageUnit(Unit_: TUnit): TStatus;
begin
  Result := SetStatus(GdipSetPageUnit(FNativeGraphics, Unit_));
end;

function TGraphics.SetPageScale(Scale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPageScale(FNativeGraphics, Scale));
end;

function TGraphics.GetPageUnit: TUnit;
begin
  SetStatus(GdipGetPageUnit(FNativeGraphics, Result));
end;

function TGraphics.GetPageScale: Single;
begin
  SetStatus(GdipGetPageScale(FNativeGraphics, Result));
end;

function TGraphics.GetDPIX: Single;
begin
  SetStatus(GdipGetDPIX(FNativeGraphics, Result));
end;

function TGraphics.GetDPIY: Single;
begin
  SetStatus(GdipGetDPIY(FNativeGraphics, Result));
end;

function TGraphics.TransformPoints(DestSpace: TCoordinateSpace;
  SrcSpace: TCoordinateSpace;
  Pts: PPointF;
  Count: Integer): TStatus;
begin
  Result := SetStatus(GdipTransformPoints(FNativeGraphics,
    DestSpace, SrcSpace, Pts, Count));
end;

function TGraphics.TransformPoints(DestSpace: TCoordinateSpace;
  SrcSpace: TCoordinateSpace;
  Pts: PPointI;
  Count: Integer): TStatus;
begin
  Result := SetStatus(GdipTransformPointsI(FNativeGraphics,
    DestSpace, SrcSpace, Pts, Count));
end;

function TGraphics.GetNearestColor(var Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGetNearestColor(FNativeGraphics, @Color));
end;

function TGraphics.DrawLine(Pen: IPen; X1, Y1, X2, Y2: Single): TStatus;
begin
  Result := SetStatus(GdipDrawLine(FNativeGraphics,
    Pen.NativePen, X1, Y1, X2, Y2));
end;

function TGraphics.DrawLine(Pen: IPen; const Pt1, Pt2: TPointF): TStatus;
begin
  Result := DrawLine(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGraphics.DrawLines(Pen: IPen; Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLines(FNativeGraphics, Pen.NativePen, Points, Count));
end;

function TGraphics.DrawLine(Pen: IPen; X1, Y1, X2, Y2: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLineI(FNativeGraphics,
    Pen.NativePen, X1, Y1, X2, Y2));
end;

function TGraphics.DrawLine(Pen: IPen; const Pt1, Pt2: TPointI): TStatus;
begin
  Result := DrawLine(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGraphics.DrawLines(Pen: IPen; Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipDrawLinesI(FNativeGraphics, Pen.NativePen, Points, Count));
end;

function TGraphics.DrawArc(Pen: IPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawArc(FNativeGraphics, NativePen,
    X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGraphics.DrawArc(Pen: IPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawArc(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGraphics.DrawArc(Pen: IPen; X, Y, Width, Height: Integer; StartAngle,
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

function TGraphics.DrawArc(Pen: IPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawArc(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGraphics.DrawBezier(Pen: IPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBezier(FNativeGraphics,
    NativePen, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGraphics.DrawBezier(Pen: IPen; const Pt1, Pt2, Pt3, Pt4: TPointF): TStatus;
begin
  Result := DrawBezier(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGraphics.DrawBeziers(Pen: IPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBeziers(FNativeGraphics, NativePen, Points, Count));
end;

function TGraphics.DrawBezier(Pen: IPen; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBezierI(FNativeGraphics,
    NativePen, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGraphics.DrawBezier(Pen: IPen; const Pt1, Pt2, Pt3, Pt4: TPointI): TStatus;
begin
  Result := DrawBezier(Pen, Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGraphics.DrawBeziers(Pen: IPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawBeziersI(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGraphics.DrawRectangle(Pen: IPen; const Rect: TRectF): TStatus;
begin
  Result := DrawRectangle(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.DrawRectangle(Pen: IPen; X, Y, Width, Height: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectangle(FNativeGraphics,
    NativePen, X, Y, Width, Height));
end;

function TGraphics.DrawRectangles(Pen: IPen; Rects: PRectF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectangles(FNativeGraphics, NativePen, Rects, Count));
end;

function TGraphics.DrawRectangle(Pen: IPen; const Rect: TRectI): TStatus;
begin
  Result := DrawRectangle(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.DrawRectangle(Pen: IPen; X, Y, Width, Height: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectangleI(FNativeGraphics,
    NativePen, X, Y, Width, Height));
end;

function TGraphics.DrawRectangles(Pen: IPen; Rects: PRectI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawRectanglesI(FNativeGraphics,
    NativePen, Rects, Count));
end;

function TGraphics.DrawEllipse(Pen: IPen; const Rect: TRectF): TStatus;
begin
  Result := DrawEllipse(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.DrawEllipse(Pen: IPen; X, Y, Width, Height: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawEllipse(FNativeGraphics,
    NativePen, X, Y, Width, Height));
end;

function TGraphics.DrawEllipse(Pen: IPen; const Rect: TRectI): TStatus;
begin
  Result := DrawEllipse(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.DrawEllipse(Pen: IPen; X, Y, Width, Height: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawEllipseI(FNativeGraphics, NativePen, X, Y, Width, Height));
end;

function TGraphics.DrawPie(Pen: IPen; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawPie(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGraphics.DrawPie(Pen: IPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawPie(FNativeGraphics,
    NativePen, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGraphics.DrawPie(Pen: IPen; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := DrawPie(Pen, Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGraphics.DrawPie(Pen: IPen; X, Y, Width, Height: Integer;
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

function TGraphics.DrawPolygon(Pen: IPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawPolygon(FNativeGraphics, NativePen, Points, Count));
end;

function TGraphics.DrawPolygon(Pen: IPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawPolygonI(FNativeGraphics, NativePen, Points, Count));
end;

function TGraphics.DrawPath(Pen: IPen; Path: IGraphicsPath): TStatus;
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

function TGraphics.DrawCurve(Pen: IPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGraphics.DrawCurve(Pen: IPen; Points: PPointF; Count: Integer; Tension: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve2(FNativeGraphics,
    NativePen, Points, Count, Tension));
end;

function TGraphics.DrawCurve(Pen: IPen; Points: PPointF; Count, Offset,
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

function TGraphics.DrawCurve(Pen: IPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurveI(FNativeGraphics, NativePen, Points, Count));
end;

function TGraphics.DrawCurve(Pen: IPen; Points: PPointI; Count: Integer; Tension: Single): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawCurve2I(FNativeGraphics,
    NativePen, Points, Count, Tension));
end;

function TGraphics.DrawCurve(Pen: IPen; Points: PPointI; Count, Offset,
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

function TGraphics.DrawClosedCurve(Pen: IPen; Points: PPointF; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawClosedCurve(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGraphics.DrawClosedCurve(Pen: IPen; Points: PPointF; Count: Integer;
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

function TGraphics.DrawClosedCurve(Pen: IPen; Points: PPointI; Count: Integer): TStatus;
var
  NativePen: GpPen;
begin
  NativePen := nil;
  if Pen <> nil then
    NativePen := Pen.NativePen;
  Result := SetStatus(GdipDrawClosedCurveI(FNativeGraphics,
    NativePen, Points, Count));
end;

function TGraphics.DrawClosedCurve(Pen: IPen; Points: PPointI;
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

function TGraphics.Clear(Color: TArgb): TStatus;
begin
  Result := SetStatus(GdipGraphicsClear( FNativeGraphics, Color));
end;

function TGraphics.FillRectangle(Brush: IBrush; const Rect: TRectF): TStatus;
begin
  Result := FillRectangle(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.FillRectangle(Brush: IBrush; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipFillRectangle(FNativeGraphics,
    Brush.NativeBrush, X, Y,
    Width, Height));
end;

function TGraphics.FillRectangles(Brush: IBrush; Rects: PRectF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangles(FNativeGraphics,
    Brush.NativeBrush,
    Rects, Count));
end;

function TGraphics.FillRectangle(Brush: IBrush; const Rect: TRectI): TStatus;
begin
  Result := FillRectangle(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.FillRectangle(Brush: IBrush; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectangleI(FNativeGraphics,
    Brush.NativeBrush,
    X,
    Y,
    Width,
    Height));
end;

function TGraphics.FillRectangles(Brush: IBrush; Rects: PRectI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillRectanglesI(FNativeGraphics,
    Brush.NativeBrush,
    Rects,
    Count));
end;

function TGraphics.FillPolygon(Brush: IBrush; Points: PPointF; Count: Integer): TStatus;
begin
  Result := FillPolygon(Brush, Points, Count, FillModeAlternate);
end;

function TGraphics.FillPolygon(Brush: IBrush; Points: PPointF; Count: Integer;
  FillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygon(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count, FillMode));
end;

function TGraphics.FillPolygon(Brush: IBrush; Points: PPointI; Count: Integer): TStatus;
begin
  Result := FillPolygon(Brush, Points, Count, FillModeAlternate);
end;

function TGraphics.FillPolygon(Brush: IBrush; Points: PPointI; Count: Integer;
  FillMode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipFillPolygonI(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count,
    FillMode));
end;

function TGraphics.FillEllipse(Brush: IBrush; const Rect: TRectF): TStatus;
begin
  Result := FillEllipse(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.FillEllipse(Brush: IBrush; X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipFillEllipse(FNativeGraphics,
    Brush.NativeBrush, X, Y,
    Width, Height));
end;

function TGraphics.FillEllipse(Brush: IBrush; const Rect: TRectI): TStatus;
begin
  Result := FillEllipse(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.FillEllipse(Brush: IBrush; X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipFillEllipseI(FNativeGraphics,
    Brush.NativeBrush,
    X,
    Y,
    Width,
    Height));
end;

function TGraphics.FillPie(Brush: IBrush; const Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := FillPie(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGraphics.FillPie(Brush: IBrush; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipFillPie(FNativeGraphics,
    Brush.NativeBrush, X, Y,
    Width, Height, StartAngle,
    SweepAngle));
end;

function TGraphics.FillPie(Brush: IBrush; const Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := FillPie(Brush, Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGraphics.FillPie(Brush: IBrush; X, Y, Width, Height: Integer; StartAngle,
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

function TGraphics.FillPath(Brush: IBrush; Path: IGraphicsPath): TStatus;
begin
  Result := SetStatus(GdipFillPath(FNativeGraphics,
    Brush.NativeBrush,
    Path.NativePath));
end;

function TGraphics.FillClosedCurve(Brush: IBrush; Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count));
end;

function TGraphics.FillClosedCurve(Brush: IBrush; Points: PPointF; Count: Integer;
  FillMode: TFillMode; Tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count,
    Tension, FillMode));
end;

function TGraphics.FillClosedCurve(Brush: IBrush; Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurveI(FNativeGraphics,
    Brush.NativeBrush,
    Points,
    Count));
end;

function TGraphics.FillClosedCurve(Brush: IBrush; Points: PPointI;
  Count: Integer; FillMode: TFillMode; Tension: Single = 0.5): TStatus;
begin
  Result := SetStatus(GdipFillClosedCurve2I(FNativeGraphics,
    Brush.NativeBrush,
    Points, Count,
    Tension, FillMode));
end;

function TGraphics.FillRegion(Brush: IBrush; Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipFillRegion(FNativeGraphics,
    Brush.NativeBrush,
    Region.NativeRegion));
end;

function TGraphics.DrawString(Text: WideString; Font: IFont;
  const LayoutRect: TRectF; StringFormat: IStringFormat; Brush: IBrush): TStatus;
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

function TGraphics.DrawString(Text: WideString; Font: IFont;
  const Origin: TPointF; Brush: IBrush): TStatus;
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

function TGraphics.DrawString(Text: WideString; Font: IFont;
  const Origin: TPointF; StringFormat: IStringFormat; Brush: IBrush): TStatus;
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

function TGraphics.MeasureString(Text: WideString; Font: IFont;
  const LayoutRect: TRectF; StringFormat: IStringFormat; out BoundingBox: TRectF;
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

function TGraphics.MeasureString(Text: WideString; Font: IFont;
  const LayoutRectSize: TSizeF; StringFormat: IStringFormat; out Size: TSizeF;
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

function TGraphics.MeasureString(Text: WideString; Font: IFont;
  const Origin: TPointF; StringFormat: IStringFormat; out BoundingBox: TRectF): TStatus;
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

function TGraphics.MeasureString(Text: WideString; Font: IFont;
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

function TGraphics.MeasureString(Text: WideString; Font: IFont;
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

function TGraphics.MeasureCharacterRanges(Text: WideString; Font: IFont;
  const LayoutRect: TRectF; StringFormat: IStringFormat; RegionCount: Integer;
  const Regions: array of IRegion): TStatus;
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

function TGraphics.DrawDriverString(Text: PUInt16; Length: Integer; Font: IFont; Brush: IBrush; Positions: PPointF; Flags: Integer; Matrix: IMatrix): TStatus;
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

function TGraphics.MeasureDriverString(Text: PUInt16; Length: Integer; Font: IFont;
  Positions: PPointF; Flags: Integer; Matrix: IMatrix;
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

function TGraphics.DrawCachedBitmap(Bitmap: ICachedBitmap; X, Y: Integer): TStatus;
var
  NativeCachedBitmap: GpCachedBitmap;
begin
  NativeCachedBitmap := nil;
  if Bitmap <> nil then
    NativeCachedBitmap := Bitmap.NativeCachedBitmap;
  Result := SetStatus(GdipDrawCachedBitmap(
    FNativeGraphics, NativeCachedBitmap, X, Y));
end;

function TGraphics.DrawImage(Image: IImage; const Point: TPointF): TStatus;
begin
  Result := DrawImage(Image, Point.X, Point.Y);
end;

function TGraphics.DrawImage(Image: IImage; X, Y: Single): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImage(FNativeGraphics, NativeImage, X, Y));
end;

function TGraphics.DrawImage(Image: IImage; const Rect: TRectF): TStatus;
begin
  Result := DrawImage(Image, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.DrawImage(Image: IImage; X, Y, Width, Height: Single): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImageRect(FNativeGraphics,
    NativeImage, X, Y, Width, Height));
end;

function TGraphics.DrawImage(Image: IImage; const Point: TPointI): TStatus;
begin
  Result := DrawImage(Image, Point.X, Point.Y);
end;

function TGraphics.DrawImage(Image: IImage; X, Y: Integer): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImageI(FNativeGraphics, NativeImage, X, Y));
end;

function TGraphics.DrawImage(Image: IImage; const Rect: TRectI): TStatus;
begin
  Result := DrawImage(Image, Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphics.DrawImage(Image: IImage; X, Y, Width, Height: Integer): TStatus;
var
  NativeImage: GpImage;
begin
  NativeImage := nil;
  if Image <> nil then
    NativeImage := Image.NativeImage;
  Result := SetStatus(GdipDrawImageRectI(FNativeGraphics,
    NativeImage, X, Y, Width, Height));
end;

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer): TStatus;
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

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointI; Count: Integer): TStatus;
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

function TGraphics.DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Single;
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

function TGraphics.DrawImage(Image: IImage; const DestRect: TRectF; SrcX, SrcY, SrcWidth, SrcHeight: Single;
  SrcUnit: TUnit; ImageAttributes: IImageAttributes = nil; Callback: DrawImageAbort = nil;
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

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointF; Count: Integer;
  SrcX, SrcY, SrcWidth, SrcHeight: Single; SrcUnit: TUnit;
  ImageAttributes: IImageAttributes = nil; Callback: DrawImageAbort = nil;
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

function TGraphics.DrawImage(Image: IImage; X, Y, SrcX, SrcY, SrcWidth, SrcHeight: Integer;
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

function TGraphics.DrawImage(Image: IImage; const DestRect: TRectI; SrcX, SrcY, SrcWidth,
  SrcHeight: Integer; SrcUnit: TUnit; ImageAttributes: IImageAttributes = nil;
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

function TGraphics.DrawImage(Image: IImage; DestPoints: PPointI;
  Count, SrcX, SrcY, SrcWidth, SrcHeight: Integer; SrcUnit: TUnit;
  ImageAttributes: IImageAttributes = nil; Callback: DrawImageAbort = nil;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointF;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointI;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestRect: TRectF;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestRect: TRectI;
  Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointF;
  Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointI;
  Count: Integer; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointF;
  const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestPoint: TPointI;
  const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestRect: TRectF;
  const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; const DestRect, SrcRect: TRectI;
  SrcUnit: TUnit; Callback: EnumerateMetafileProc; CallbackData: Pointer = nil;
  ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointF;
  Count: Integer; const SrcRect: TRectF; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.EnumerateMetafile(Metafile: IMetafile; DestPoints: PPointI;
  Count: Integer; const SrcRect: TRectI; SrcUnit: TUnit; Callback: EnumerateMetafileProc;
  CallbackData: Pointer = nil; ImageAttributes: IImageAttributes = nil): TStatus;
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

function TGraphics.SetClip(G: IGraphics; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipGraphics(FNativeGraphics,
    G.NativeGraphics, CombineMode));
end;

function TGraphics.SetClip(Rect: TRectF; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineMode));
end;

function TGraphics.SetClip(Rect: TRectI; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineMode));
end;

function TGraphics.SetClip(Path: IGraphicsPath; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipPath(FNativeGraphics,
    Path.NativePath, CombineMode));
end;

function TGraphics.SetClip(Region: IRegion; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(FNativeGraphics,
    Region.NativeRegion, CombineMode));
end;

function TGraphics.SetClip(Rgn: HRGN; CombineMode: TCombineMode = CombineModeReplace): TStatus;
begin
  Result := SetStatus(GdipSetClipHrgn(FNativeGraphics, Rgn, CombineMode));
end;

function TGraphics.IntersectClip(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeIntersect));
end;

function TGraphics.IntersectClip(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeIntersect));
end;

function TGraphics.IntersectClip(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(FNativeGraphics,
    Region.NativeRegion,
    CombineModeIntersect));
end;

function TGraphics.ExcludeClip(const Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipSetClipRect(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeExclude));
end;

function TGraphics.ExcludeClip(const Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipSetClipRectI(FNativeGraphics,
    Rect.X, Rect.Y, Rect.Width, Rect.Height, CombineModeExclude));
end;

function TGraphics.ExcludeClip(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipSetClipRegion(FNativeGraphics,
    Region.NativeRegion, CombineModeExclude));
end;

function TGraphics.ResetClip: TStatus;
begin
  Result := SetStatus(GdiPresetClip(FNativeGraphics));
end;

function TGraphics.TranslateClip(DX, DY: Single): TStatus;
begin
  Result := SetStatus(GdipTranslateClip(FNativeGraphics, DX, DY));
end;

function TGraphics.TranslateClip(DX, DY: Integer): TStatus;
begin
  Result := SetStatus(GdipTranslateClipI(FNativeGraphics, DX, DY));
end;

function TGraphics.GetClip(Region: IRegion): TStatus;
begin
  Result := SetStatus(GdipGetClip(FNativeGraphics, Region.NativeRegion));
end;

function TGraphics.GetClipBounds(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetClipBounds(FNativeGraphics, @Rect));
end;

function TGraphics.GetClipBounds(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetClipBoundsI(FNativeGraphics, @Rect));
end;

function TGraphics.IsClipEmpty: Boolean;
begin
  SetStatus(GdipIsClipEmpty(FNativeGraphics, @Result));
end;

function TGraphics.GetVisibleClipBounds(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBounds(FNativeGraphics, @Rect));
end;

function TGraphics.GetVisibleClipBounds(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetVisibleClipBoundsI(FNativeGraphics, @Rect));
end;

function TGraphics.IsVisibleClipEmpty: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleClipEmpty(FNativeGraphics, B));
  Result := B;
end;

function TGraphics.IsVisible(X, Y: Integer): Boolean;
var
  Pt: TPointI;
begin
  Pt.X := X;
  Pt.Y := Y;
  Result := IsVisible(Pt);
end;

function TGraphics.IsVisible(const Point: TPointI): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisiblePointI(FNativeGraphics, Point.X, Point.Y, B));
  Result := B;
end;

function TGraphics.IsVisible(X, Y, Width, Height: Integer): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRectI(FNativeGraphics, X, Y, Width, Height, B));
  Result := B;
end;

function TGraphics.IsVisible(const Rect: TRectI): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRectI(FNativeGraphics, Rect.X, Rect.Y, Rect.Width, Rect.Height, B));
  Result := B;
end;

function TGraphics.IsVisible(X, Y: Single): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisiblePoint(FNativeGraphics, X, Y, B));
  Result := B;
end;

function TGraphics.IsVisible(const Point: TPointF): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisiblePoint(FNativeGraphics, Point.X, Point.Y, B));
  Result := B;
end;

function TGraphics.IsVisible(X, Y, Width, Height: Single): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRect(FNativeGraphics, X, Y, Width, Height, B));
  Result := B;
end;

function TGraphics.IsVisible(const Rect: TRectF): Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipIsVisibleRect(FNativeGraphics, Rect.X, Rect.Y, Rect.Width,
    Rect.Height, B));
  Result := B;
end;

function TGraphics.Save: GraphicsState;
begin
  SetStatus(GdipSaveGraphics(FNativeGraphics, Result));
end;

function TGraphics.Restore(gstate: GraphicsState): TStatus;
begin
  Result := SetStatus(GdipRestoreGraphics(FNativeGraphics,
    gstate));
end;

function TGraphics.BeginContainer(const DstRect, SrcRect: TRectF; Unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainer(FNativeGraphics, @DstRect, @SrcRect, Unit_, Result));
end;

function TGraphics.BeginContainer(const DstRect, SrcRect: TRectI; Unit_: TUnit): GraphicsContainer;
begin
  SetStatus(GdipBeginContainerI(FNativeGraphics, @DstRect, @SrcRect, Unit_, Result));
end;

function TGraphics.BeginContainer: GraphicsContainer;
begin
  SetStatus(GdipBeginContainer2(FNativeGraphics, Result));
end;

function TGraphics.EnDContainer(state: GraphicsContainer): TStatus;
begin
  Result := SetStatus(GdipEnDContainer(FNativeGraphics, state));
end;

function TGraphics.AddMetafileComment(data: PByte; SizeData: UInt): TStatus;
begin
  Result := SetStatus(GdipComment(FNativeGraphics, SizeData, data));
end;

function TGraphics.GetHalftonePalette: HPALETTE;
begin
  Result := GdipCreateHalftonePalette;
end;

constructor TGraphics.Create(Graphics: GpGraphics);
begin
  inherited Create;
  FLastStatus := Ok;
  SetNativeGraphics(Graphics);
end;

function TGraphics.GetNativeGraphics: GpGraphics;
begin
  Result := FNativeGraphics;
end;

procedure TGraphics.SetNativeGraphics(Graphics: GpGraphics);
begin
  Self.FNativeGraphics := Graphics;
end;

{ TFontFamily }

constructor TFontFamily.Create;
begin
  inherited Create;
end;

constructor TFontFamily.Create(Name: WideString; FontCollection: IFontCollection = nil);
var
  NativeFontCollection: GpFontCollection;
begin
  inherited Create;
  NativeFontCollection := nil;
  if Assigned(FontCollection) then
    NativeFontCollection := FontCollection.NativeFontCollection;
  FLastStatus := GdipCreateFontFamilyFromName(PWideChar(Name), NativeFontCollection, FNativeFamily);
end;

destructor TFontFamily.Destroy;
begin
  GdipDeleteFontFamily(FNativeFamily);
end;

class function TFontFamily.GenericSansSerif: IFontFamily;
var
  FontFamily: GpFontFamily;
begin
  if GenericSansSerifFontFamily <> nil then
  begin
    Result := GenericSansSerifFontFamily;
    Exit;
  end;
  GenericSansSerifFontFamily := TFontFamily.Create;
  GenericSansSerifFontFamily.LastStatus := GdipGetGenericFontFamilySansSerif(FontFamily);
  GenericSansSerifFontFamily.NativeFamily := FontFamily;
  Result := GenericSansSerifFontFamily;
end;

class function TFontFamily.GenericSerif: IFontFamily;
var
  FontFamily: GpFontFamily;
begin
  if GenericSerifFontFamily <> nil then
  begin
    Result := GenericSerifFontFamily;
    Exit;
  end;
  GenericSerifFontFamily := TFontFamily.Create;
  GenericSerifFontFamily.LastStatus := GdipGetGenericFontFamilySerif(FontFamily);
  GenericSerifFontFamily.NativeFamily := FontFamily;
  Result := GenericSerifFontFamily;
end;

class function TFontFamily.GenericMonospace: IFontFamily;
var
  FontFamily: GpFontFamily;
begin
  if GenericMonospaceFontFamily <> nil then
  begin
    Result := GenericMonospaceFontFamily;
    Exit;
  end;
  GenericMonospaceFontFamily := TFontFamily.Create;
  GenericMonospaceFontFamily.LastStatus := GdipGetGenericFontFamilyMonospace(FontFamily);
  GenericMonospaceFontFamily.NativeFamily := FontFamily;
  Result := GenericMonospaceFontFamily;
end;

function TFontFamily.GetFamilyName(out Name: string; Language: LangId = 0): TStatus;
var
  S: array[0..LF_FACESIZE - 1] of WideChar;
begin
  Result := SetStatus(GdipGetFamilyName(FNativeFamily, @S, Language));
  Name := S;
end;

function TFontFamily.Clone: IFontFamily;
var
  ClonedFamily: GpFontFamily;
begin
  ClonedFamily := nil;
  SetStatus(GdipCloneFontFamily(FNativeFamily, ClonedFamily));
  Result := TFontFamily.Create(ClonedFamily, FLastStatus);
end;

function TFontFamily.IsAvailable: Boolean;
begin
  Result := (FNativeFamily <> nil);
end;

function TFontFamily.IsStyleAvailable(Style: Integer): Boolean;
var
  Status: TStatus;
  B: BOOL;
begin
  Status := SetStatus(GdipIsStyleAvailable(FNativeFamily, Style, B));
  if Status <> Ok then
    B := False;
  Result := B;
end;

function TFontFamily.GetEmHeight(Style: Integer): UInt16;
begin
  SetStatus(GdipGetEmHeight(FNativeFamily, Style, Result));
end;

function TFontFamily.GetCellAscent(Style: Integer): UInt16;
begin
  SetStatus(GdipGetCellAscent(FNativeFamily, Style, Result));
end;

function TFontFamily.GetCellDescent(Style: Integer): UInt16;
begin
  SetStatus(GdipGetCellDescent(FNativeFamily, Style, Result));
end;

function TFontFamily.GetLineSpacing(Style: Integer): UInt16;
begin
  SetStatus(GdipGetLineSpacing(FNativeFamily, Style, Result));
end;

function TFontFamily.GetNativeFamily: GpFontFamily;
begin
  Result := FNativeFamily;
end;

procedure TFontFamily.SetNativeFamily(Value: GpFontFamily);
begin
  FNativeFamily := Value;
end;

constructor TFontFamily.Create(Orig: GpFontFamily; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  FNativeFamily := Orig;
end;

constructor TFont.Create(DC: HDC);
var
  Font: GpFont;
begin
  inherited Create;
  Font := nil;
  FLastStatus := GdipCreateFontFromDC(DC, Font);
  SetNativeFont(Font);
end;

constructor TFont.Create(DC: HDC; LogFont: PLogFontA);
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

constructor TFont.Create(DC: HDC; LogFont: PLogFontW);
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

constructor TFont.Create(DC: HDC; Font: HFont);
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

constructor TFont.Create(Family: IFontFamily; EmSize: Single;
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

constructor TFont.Create(FamilyName: WideString; EmSize: Single;
  Style: TFontStyle = FontStyleRegular; Unit_: TUnit = UnitPoint;
  FontCollection: IFontCollection = nil);
var
  Family: IFontFamily;
  FNativeFamily: GpFontFamily;
begin
  inherited Create;
  FNativeFont := nil;
  Family := TFontFamily.Create(FamilyName, FontCollection);
  FNativeFamily := Family.NativeFamily;
  FLastStatus := Family.GetLastStatus;
  if (FLastStatus <> Ok) then
  begin
    FNativeFamily := TFontFamily.GenericSansSerif.NativeFamily;
    FLastStatus := TFontFamily.GenericSansSerif.LastStatus;
    if (FLastStatus <> Ok) then
      Exit;
  end;
  FLastStatus := GdipCreateFont(FNativeFamily, EmSize,
    Integer(Style), Integer(Unit_), FNativeFont);
  if FLastStatus <> Ok then
  begin
    FNativeFamily := TFontFamily.GenericSansSerif.NativeFamily;
    FLastStatus := TFontFamily.GenericSansSerif.LastStatus;
    if FLastStatus <> Ok then
      Exit;
    FLastStatus := GdipCreateFont(FNativeFamily, EmSize, Integer(Style),
      Integer(Unit_), FNativeFont);
  end;
end;

function TFont.GetLogFontA(G: IGraphics; out LogFontA: TLogFontA): TStatus;
var
  NativeGraphics: GpGraphics;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  Result := SetStatus(GdipGetLogFontA(FNativeFont, NativeGraphics, LogFontA));
end;

function TFont.GetLogFontW(G: IGraphics; out LogFontW: TLogFontW): TStatus;
var
  NativeGraphics: GpGraphics;
begin
  NativeGraphics := nil;
  if G <> nil then
    NativeGraphics := G.NativeGraphics;
  Result := SetStatus(GdipGetLogFontW(FNativeFont, NativeGraphics, LogFontW));
end;

function TFont.Clone: IFont;
var
  CloneFont: GpFont;
begin
  CloneFont := nil;
  SetStatus(GdipCloneFont(FNativeFont, CloneFont));
  Result := TFont.Create(CloneFont, FLastStatus);
end;

destructor TFont.Destroy;
begin
  GdipDeleteFont(FNativeFont);
end;

function TFont.IsAvailable: Boolean;
begin
  Result := (FNativeFont <> nil);
end;

function TFont.GetStyle: Integer;
begin
  SetStatus(GdipGetFontStyle(FNativeFont, Result));
end;

function TFont.GetSize: Single;
begin
  SetStatus(GdipGetFontSize(FNativeFont, Result));
end;

function TFont.GetUnit: TUnit;
begin
  SetStatus(GdipGetFontUnit(FNativeFont, Result));
end;

function TFont.GetHeight(Graphics: IGraphics): Single;
var
  NativeGraphics: GpGraphics;
begin
  NativeGraphics := nil;
  if Graphics <> nil then
    NativeGraphics := Graphics.NativeGraphics;
  SetStatus(GdipGetFontHeight(FNativeFont, NativeGraphics, Result));
end;

function TFont.GetHeight(DPI: Single): Single;
begin
  SetStatus(GdipGetFontHeightGivenDPI(FNativeFont, DPI, Result));
end;

function TFont.GetFamily(Family: IFontFamily): TStatus;
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

function TFont.GetNativeFont: GpFont;
begin
  Result := FNativeFont;
end;

procedure TFont.SetNativeFont(Font: GpFont);
begin
  FNativeFont := Font;
end;

constructor TFont.Create(Font: GpFont; Status: TStatus);
begin
  inherited Create;
  FLastStatus := Status;
  SetNativeFont(Font);
end;

constructor TFontCollection.Create;
begin
  inherited Create;
  FNativeFontCollection := nil;
end;

destructor TFontCollection.Destroy;
begin
  inherited Destroy;
end;

function TFontCollection.GetFamilyCount: Integer;
var
  NumFound: Integer;
begin
  NumFound := 0;
  FLastStatus := GdipGetFontCollectionFamilyCount(FNativeFontCollection, NumFound);
  Result := NumFound;
end;

function TFontCollection.GetFamilies(NumSought: Integer; out Families: array of IFontFamily;
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

function TFontCollection.GetNativeFontCollection: GpFontCollection;
begin
  Result := FNativeFontCollection;
end;

procedure TFontCollection.SetNativeFontCollection(Value: GpFontCollection);
begin
  FNativeFontCollection := Value;
end;

constructor TInstalledFontCollection.Create;
begin
  inherited Create;
  FNativeFontCollection := nil;
  FLastStatus := GdipNewInstalledFontCollection(FNativeFontCollection);
end;

destructor TInstalledFontCollection.Destroy;
begin
  inherited Destroy;
end;

constructor TPrivateFontCollection.Create;
begin
  inherited Create;
  FNativeFontCollection := nil;
  FLastStatus := GdipNewPrivateFontCollection(FNativeFontCollection);
end;

destructor TPrivateFontCollection.Destroy;
begin
  GdipDeletePrivateFontCollection(FNativeFontCollection);
  inherited Destroy;
end;

function TPrivateFontCollection.AddFontFile(Filename: WideString): TStatus;
begin
  Result := SetStatus(GdipPrivateAddFontFile(FNativeFontCollection, PWideChar(Filename)));
end;

function TPrivateFontCollection.AddMemoryFont(Memory: Pointer; Length: Integer): TStatus;
begin
  Result := SetStatus(GdipPrivateAddMemoryFont(FNativeFontCollection,
    Memory, Length));
end;

function TGraphicsPath.GetNativePath: GpPath;
begin
  Result := FNativePath;
end;

procedure TGraphicsPath.SetNativePath(Path: GpPath);
begin
  FNativePath := Path;
end;

constructor TGraphicsPath.Create(FillMode: TFillMode = FillModeAlternate);
begin
  inherited Create;
  FNativePath := nil;
  FLastStatus := GdipCreatePath(FillMode, FNativePath);
end;

constructor TGraphicsPath.Create(Points: PPointF; Types: PByte; Count: Integer;
  FillMode: TFillMode = FillModeAlternate);
begin
  inherited Create;
  FNativePath := nil;
  FLastStatus := GdipCreatePath2(Points, Types, Count, FillMode, FNativePath);
end;

constructor TGraphicsPath.Create(Points: PPointI; Types: PByte; Count: Integer;
  FillMode: TFillMode = FillModeAlternate);
begin
  inherited Create;
  FNativePath := nil;
  FLastStatus := GdipCreatePath2I(Points, Types, Count, FillMode, FNativePath);
end;

destructor TGraphicsPath.Destroy;
begin
  GdipDeletePath(FNativePath);
end;

function TGraphicsPath.Clone: IGraphicsPath;
var
  ClonePath: GpPath;
begin
  ClonePath := nil;
  SetStatus(GdipClonePath(FNativePath, ClonePath));
  Result := TGraphicsPath.Create(ClonePath) as IGraphicsPath;
end;

function TGraphicsPath.Reset: TStatus;
begin
  Result := SetStatus(GdiPresetPath(FNativePath));
end;

function TGraphicsPath.GetFillMode: TFillMode;
var
  FMode: TFillMode;
begin
  FMode := FillModeAlternate;
  SetStatus(GdipGetPathFillMode(FNativePath, Result));
  Result := FMode;
end;

function TGraphicsPath.SetFillMode(Fillmode: TFillMode): TStatus;
begin
  Result := SetStatus(GdipSetPathFillMode(FNativePath, Fillmode));
end;

function TGraphicsPath.GetPathData(PathData: TPathData): TStatus;
var
  Count: Integer;
begin
  Count := GetPointCount;
  if (Count <= 0) or ((PathData.Count > 0) and (PathData.Count < Count)) then
  begin
    PathData.Count := 0;
    if Assigned(PathData.Points) then
    begin
      FreeMem(PathData.Points);
      PathData.Points := nil;
    end;
    if Assigned(PathData.Types) then
    begin
      FreeMem(PathData.Types);
      PathData.Types := nil;
    end;
    if (Count <= 0) then
    begin
      Result := FLastStatus;
      Exit;
    end;
  end;
  if PathData.Count = 0 then
  begin
    GetMem(PathData.Points, SizeOf(TPointF) * Count);
    if (PathData.Points = nil) then
    begin
      Result := SetStatus(OutOfMemory);
      Exit;
    end;
    GetMem(PathData.Types, Count);
    if (PathData.Types = nil) then
    begin
      FreeMem(PathData.Points);
      PathData.Points := nil;
      Result := SetStatus(OutOfMemory);
      Exit;
    end;
    PathData.Count := Count;
  end;
  Result := SetStatus(GdipGetPathData(FNativePath, @PathData.Count));
end;

function TGraphicsPath.StartFigure: TStatus;
begin
  Result := SetStatus(GdipStartPathFigure(FNativePath));
end;

function TGraphicsPath.CloseFigure: TStatus;
begin
  Result := SetStatus(GdipClosePathFigure(FNativePath));
end;

function TGraphicsPath.CloseAllFigures: TStatus;
begin
  Result := SetStatus(GdipClosePathFigures(FNativePath));
end;

function TGraphicsPath.SetMarker: TStatus;
begin
  Result := SetStatus(GdipSetPathMarker(FNativePath));
end;

function TGraphicsPath.ClearMarkers: TStatus;
begin
  Result := SetStatus(GdipClearPathMarkers(FNativePath));
end;

function TGraphicsPath.Reverse: TStatus;
begin
  Result := SetStatus(GdipReversePath(FNativePath));
end;

function TGraphicsPath.GetLastPoint(out lastPoint: TPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathLastPoint(FNativePath, @lastPoint));
end;

function TGraphicsPath.AddLine(const Pt1, Pt2: TPointF): TStatus;
begin
  Result := AddLine(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGraphicsPath.AddLine(X1, Y1, X2, Y2: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathLine(FNativePath, X1, Y1,
    X2, Y2));
end;

function TGraphicsPath.AddLines(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2(FNativePath, Points, Count));
end;

function TGraphicsPath.AddLine(const Pt1, Pt2: TPointI): TStatus;
begin
  Result := AddLine(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y);
end;

function TGraphicsPath.AddLine(X1, Y1, X2, Y2: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLineI(FNativePath, X1, Y1, X2, Y2));
end;

function TGraphicsPath.AddLines(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathLine2I(FNativePath, Points, Count));
end;

function TGraphicsPath.AddArc(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height,
    StartAngle, SweepAngle);
end;

function TGraphicsPath.AddArc(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArc(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGraphicsPath.AddArc(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddArc(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGraphicsPath.AddArc(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathArcI(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGraphicsPath.AddBezier(Pt1, Pt2, Pt3, Pt4: TPointF): TStatus;
begin
  Result := AddBezier(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGraphicsPath.AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathBezier(FNativePath, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGraphicsPath.AddBeziers(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziers(FNativePath, Points, Count));
end;

function TGraphicsPath.AddBezier(Pt1, Pt2, Pt3, Pt4: TPointI): TStatus;
begin
  Result := AddBezier(Pt1.X, Pt1.Y, Pt2.X, Pt2.Y, Pt3.X, Pt3.Y, Pt4.X, Pt4.Y);
end;

function TGraphicsPath.AddBezier(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBezierI(FNativePath, X1, Y1, X2, Y2, X3, Y3, X4, Y4));
end;

function TGraphicsPath.AddBeziers(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathBeziersI(FNativePath, Points, Count));
end;

function TGraphicsPath.AddCurve(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve(FNativePath, Points, Count));
end;

function TGraphicsPath.AddCurve(Points: PPointF; Count: Integer;
  Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2(FNativePath, Points, Count, Tension));
end;

function TGraphicsPath.AddCurve(Points: PPointF; Count, Offset,
  NumberOfSegments: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3(FNativePath, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TGraphicsPath.AddCurve(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathCurveI(FNativePath, Points, Count));
end;

function TGraphicsPath.AddCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve2I(FNativePath, Points, Count, Tension));
end;

function TGraphicsPath.AddCurve(Points: PPointI; Count, Offset,
  NumberOfSegments: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathCurve3I(FNativePath, Points, Count, Offset, NumberOfSegments, Tension));
end;

function TGraphicsPath.AddClosedCurve(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve(FNativePath, Points, Count));
end;

function TGraphicsPath.AddClosedCurve(Points: PPointF; Count: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2(FNativePath, Points, Count, Tension));
end;

function TGraphicsPath.AddClosedCurve(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurveI(FNativePath, Points, Count));
end;

function TGraphicsPath.AddClosedCurve(Points: PPointI; Count: Integer; Tension: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathClosedCurve2I(FNativePath, Points, Count, Tension));
end;

function TGraphicsPath.AddRectangle(Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangle(FNativePath, Rect.X, Rect.Y, Rect.Width, Rect.Height));
end;

function TGraphicsPath.AddRectangles(Rects: PRectF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangles(FNativePath, Rects, Count));
end;

function TGraphicsPath.AddRectangle(Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipAddPathRectangleI(FNativePath, Rect.X, Rect.Y, Rect.Width, Rect.Height));
end;

function TGraphicsPath.AddRectangles(Rects: PRectI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathRectanglesI(FNativePath, Rects, Count));
end;

function TGraphicsPath.AddEllipse(Rect: TRectF): TStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphicsPath.AddEllipse(X, Y, Width, Height: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipse(FNativePath, X, Y, Width, Height));
end;

function TGraphicsPath.AddEllipse(Rect: TRectI): TStatus;
begin
  Result := AddEllipse(Rect.X, Rect.Y, Rect.Width, Rect.Height);
end;

function TGraphicsPath.AddEllipse(X, Y, Width, Height: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathEllipseI(FNativePath, X, Y, Width, Height));
end;

function TGraphicsPath.AddPie(Rect: TRectF; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddPie(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGraphicsPath.AddPie(X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPie(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGraphicsPath.AddPie(Rect: TRectI; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := AddPie(Rect.X, Rect.Y, Rect.Width, Rect.Height, StartAngle, SweepAngle);
end;

function TGraphicsPath.AddPie(X, Y, Width, Height: Integer; StartAngle, SweepAngle: Single): TStatus;
begin
  Result := SetStatus(GdipAddPathPieI(FNativePath, X, Y, Width, Height, StartAngle, SweepAngle));
end;

function TGraphicsPath.AddPolygon(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygon(FNativePath, Points, Count));
end;

function TGraphicsPath.AddPolygon(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipAddPathPolygonI(FNativePath, Points, Count));
end;

function TGraphicsPath.AddPath(AddingPath: IGraphicsPath; Connect: Boolean): TStatus;
var
  NativePath: GpPath;
begin
  NativePath := nil;
  if AddingPath <> nil then
    NativePath := AddingPath.NativePath;
  Result := SetStatus(GdipAddPathPath(FNativePath, NativePath, Connect));
end;

function TGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IFontFamily; Style: Integer; EmSize: Single; Origin: TPointF;
  Format: IStringFormat): TStatus;
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

function TGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IFontFamily; Style: Integer; EmSize: Single; LayoutRect: TRectF;
  Format: IStringFormat): TStatus;
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

function TGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IFontFamily; Style: Integer; EmSize: Single; Origin: TPointI;
  Format: IStringFormat): TStatus;
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

function TGraphicsPath.AddString(Text: WideString; Length: Integer;
  Family: IFontFamily; Style: Integer; EmSize: Single; LayoutRect: TRectI;
  Format: IStringFormat): TStatus;
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

function TGraphicsPath.Transform(Matrix: IMatrix): TStatus;
begin
  if Matrix <> nil then
    Result := SetStatus(GdipTransformPath(FNativePath, Matrix.NativeMatrix))
  else
    Result := Ok;
end;

function TGraphicsPath.GetBounds(out Bounds: TRectF; Matrix: IMatrix = nil; Pen: IPen = nil): TStatus;
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

function TGraphicsPath.GetBounds(out Bounds: TRectI; Matrix: IMatrix = nil; Pen: IPen = nil): TStatus;
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

function TGraphicsPath.Flatten(Matrix: IMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
var
  NativeMatrix: GpMatrix;
begin
  NativeMatrix := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipFlattenPath(FNativePath, NativeMatrix, Flatness));
end;

function TGraphicsPath.Widen(Pen: IPen; Matrix: IMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
var
  NativeMatrix: GpMatrix;
begin
  NativeMatrix := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipWidenPath(FNativePath, Pen.NativePen, NativeMatrix, Flatness));
end;

function TGraphicsPath.Outline(Matrix: IMatrix = nil; Flatness: Single = FlatnessDefault): TStatus;
var
  NativeMatrix: GpMatrix;
begin
  NativeMatrix := nil;
  if Matrix <> nil then
    NativeMatrix := Matrix.NativeMatrix;
  Result := SetStatus(GdipWindingModeOutline(FNativePath, NativeMatrix, Flatness));
end;

function TGraphicsPath.Warp(DestPoints: PPointF; Count: Integer; SrcRect: TRectF;
  Matrix: IMatrix = nil; warpMode: TWarpMode = WarpModePerspective;
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

function TGraphicsPath.GetPointCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPointCount(FNativePath, Count));
  Result := Count;
end;

function TGraphicsPath.GetPathTypes(Types: PByte; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathTypes(FNativePath, Types, Count));
end;

function TGraphicsPath.GetPathPoints(Points: PPointF; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPoints(FNativePath, Points, Count));
end;

function TGraphicsPath.GetPathPoints(Points: PPointI; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathPointsI(FNativePath, Points, Count));
end;

function TGraphicsPath.IsVisible(Point: TPointF; G: IGraphics = nil): Boolean;
begin
  Result := IsVisible(Point.X, Point.Y, G);
end;

function TGraphicsPath.IsVisible(X, Y: Single; G: IGraphics = nil): Boolean;
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

function TGraphicsPath.IsVisible(Point: TPointI; G: IGraphics = nil): Boolean;
begin
  Result := IsVisible(Point.X, Point.Y, G);
end;

function TGraphicsPath.IsVisible(X, Y: Integer; G: IGraphics = nil): Boolean;
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

function TGraphicsPath.IsOutlineVisible(Point: TPointF; Pen: IPen; G: IGraphics = nil): Boolean;
begin
  Result := IsOutlineVisible(Point.X, Point.Y, Pen, G);
end;

function TGraphicsPath.IsOutlineVisible(X, Y: Single; Pen: IPen; G: IGraphics = nil): Boolean;
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

function TGraphicsPath.IsOutlineVisible(Point: TPointI; Pen: IPen; G: IGraphics = nil): Boolean;
begin
  Result := IsOutlineVisible(Point.X, Point.Y, Pen, G);
end;

function TGraphicsPath.IsOutlineVisible(X, Y: Integer; Pen: IPen; G: IGraphics = nil): Boolean;
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

constructor TGraphicsPath.Create(Path: IGraphicsPath);
var
  ClonePath: GpPath;
begin
  inherited Create;
  ClonePath := nil;
  SetStatus(GdipClonePath(Path.NativePath, ClonePath));
  SetNativePath(ClonePath);
end;

constructor TGraphicsPath.Create(Path: GpPath);
begin
  inherited Create;
  FLastStatus := Ok;
  FNativePath := Path;
end;

{ TGraphicsPathIterator }

constructor TGraphicsPathIterator.Create(Path: IGraphicsPath);
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

destructor TGraphicsPathIterator.Destroy;
begin
  GdipDeletePathIter(FNativeIterator);
end;

function TGraphicsPathIterator.NextSubPath(out StartIndex, EndIndex: Integer; out IsClosed: Boolean): Integer;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipPathIterNextSubPath(FNativeIterator, Result, StartIndex, EndIndex, B));
  IsClosed := B;
end;

function TGraphicsPathIterator.NextSubPath(Path: IGraphicsPath; out IsClosed: Boolean): Integer;
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

function TGraphicsPathIterator.NextPathType(out PathType: TPathPointType; out StartIndex, EndIndex: Integer): Integer;
var
  ResultCount: Integer;
begin
  SetStatus(GdipPathIterNextPathType(FNativeIterator, ResultCount, @PathType,
    StartIndex, EndIndex));
  Result := ResultCount;
end;

function TGraphicsPathIterator.NextMarker(out StartIndex, EndIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterNextMarker(FNativeIterator, Result, StartIndex, EndIndex));
end;

function TGraphicsPathIterator.NextMarker(Path: IGraphicsPath): Integer;
var
  FNativePath: GpPath;
begin
  FNativePath := nil;
  if Assigned(Path) then
    FNativePath := Path.NativePath;
  SetStatus(GdipPathIterNextMarkerPath(FNativeIterator, Result, FNativePath));
end;

function TGraphicsPathIterator.GetCount: Integer;
begin
  SetStatus(GdipPathIterGetCount(FNativeIterator, Result));
end;

function TGraphicsPathIterator.GetSubPathCount: Integer;
begin
  SetStatus(GdipPathIterGetSubPathCount(FNativeIterator, Result));
end;

function TGraphicsPathIterator.HasCurve: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipPathIterHasCurve(FNativeIterator, B));
  Result := B;
end;

procedure TGraphicsPathIterator.Rewind;
begin
  SetStatus(GdipPathIterRewind(FNativeIterator));
end;

function TGraphicsPathIterator.Enumerate(Points: PPointF; Types: PByte;
  Count: Integer): Integer;
begin
  SetStatus(GdipPathIterEnumerate(FNativeIterator, Result, Points, Types, Count));
end;

function TGraphicsPathIterator.CopyData(Points: PPointF; Types: PByte;
  StartIndex, EndIndex: Integer): Integer;
begin
  SetStatus(GdipPathIterCopyData(FNativeIterator, Result, Points, Types,
    StartIndex, EndIndex));
end;

procedure TGraphicsPathIterator.SetNativeIterator(FNativeIterator: GpPathIterator);
begin
  Self.FNativeIterator := FNativeIterator;
end;

constructor TPathGradientBrush.Create(Points: PPointF; Count: Integer; WrapMode: TWrapMode = WrapModeClamp);
var
  Brush: GpPathGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreatePathGradient(Points, Count, WrapMode, Brush);
  SetNativeBrush(Brush);
end;

{ TPathGradientBrush }

constructor TPathGradientBrush.Create(Points: PPointI; Count: Integer; WrapMode: TWrapMode = WrapModeClamp);
var
  Brush: GpPathGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreatePathGradientI(Points, Count, WrapMode, Brush);
  SetNativeBrush(Brush);
end;

constructor TPathGradientBrush.Create(Path: IGraphicsPath);
var
  Brush: GpPathGradient;
begin
  inherited Create;
  Brush := nil;
  FLastStatus := GdipCreatePathGradientFromPath(Path.NativePath, Brush);
  SetNativeBrush(Brush);
end;

function TPathGradientBrush.GetCenterColor(out Color: TArgb): TStatus;
begin
  SetStatus(GdipGetPathGradientCenterColor(GpPathGradient(FNativeBrush), Color));
  Result := FLastStatus;
end;

function TPathGradientBrush.SetCenterColor(Color: TArgb): TStatus;
begin
  SetStatus(GdipSetPathGradientCenterColor(GpPathGradient(FNativeBrush), Color));
  Result := FLastStatus;
end;

function TPathGradientBrush.GetPointCount: Integer;
begin
  SetStatus(GdipGetPathGradientPointCount(GpPathGradient(FNativeBrush), Result));
end;

function TPathGradientBrush.GetSurrounDColorCount: Integer;
begin
  SetStatus(GdipGetPathGradientSurrounDColorCount(GpPathGradient(FNativeBrush), Result));
end;

function TPathGradientBrush.GetSurrounDColors(Colors: PArgb; var Count: Integer): TStatus;
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

function TPathGradientBrush.SetSurrounDColors(Colors: PArgb; var Count: Integer): TStatus;
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

function TPathGradientBrush.GetGraphicsPath(Path: IGraphicsPath): TStatus;
begin
  if Path = nil then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipGetPathGradientPath(GpPathGradient(FNativeBrush), Path.NativePath));
end;

function TPathGradientBrush.SetGraphicsPath(Path: IGraphicsPath): TStatus;
begin
  if Path = nil then
  begin
    Result := SetStatus(InvalidParameter);
    Exit;
  end;
  Result := SetStatus(GdipSetPathGradientPath(GpPathGradient(FNativeBrush), Path.NativePath));
end;

function TPathGradientBrush.GetCenterPoint(out Point: TPointF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPoint(GpPathGradient(FNativeBrush), @Point));
end;

function TPathGradientBrush.GetCenterPoint(out Point: TPointI): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientCenterPointI(GpPathGradient(FNativeBrush), @Point));
end;

function TPathGradientBrush.SetCenterPoint(Point: TPointF): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPoint(GpPathGradient(FNativeBrush), @Point));
end;

function TPathGradientBrush.SetCenterPoint(Point: TPointI): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientCenterPointI(GpPathGradient(FNativeBrush), @Point));
end;

function TPathGradientBrush.GetRectangle(out Rect: TRectF): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRect(GpPathGradient(FNativeBrush), @Rect));
end;

function TPathGradientBrush.GetRectangle(out Rect: TRectI): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientRectI(GpPathGradient(FNativeBrush), @Rect));
end;

function TPathGradientBrush.SetGammaCorrection(UseGammaCorrection: Boolean): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientGammaCorrection(GpPathGradient(FNativeBrush),
    UseGammaCorrection));
end;

function TPathGradientBrush.GetGammaCorrection: Boolean;
var
  B: BOOL;
begin
  B := False;
  SetStatus(GdipGetPathGradientGammaCorrection(GpPathGradient(FNativeBrush), B));
  Result := B;
end;

function TPathGradientBrush.GetBlendCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientBlendCount(GpPathGradient(FNativeBrush), Count));
  Result := Count;
end;

function TPathGradientBrush.GetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientBlend(
    GpPathGradient(FNativeBrush),
    BlendFactors, BlendPositions, Count));
end;

function TPathGradientBrush.SetBlend(BlendFactors, BlendPositions: PSingle; Count: Integer): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientBlend(
    GpPathGradient(FNativeBrush), BlendFactors, BlendPositions, Count));
end;

function TPathGradientBrush.GetInterpolationColorCount: Integer;
var
  Count: Integer;
begin
  Count := 0;
  SetStatus(GdipGetPathGradientPresetBlendCount(GpPathGradient(FNativeBrush), Count));
  Result := Count;
end;

function TPathGradientBrush.SetInterpolationColors(PresetColors: PArgb;
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

function TPathGradientBrush.GetInterpolationColors(PresetColors: PArgb;
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

function TPathGradientBrush.SetBlendBellShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientSigmaBlend(GpPathGradient(FNativeBrush), Focus, Scale));
end;

function TPathGradientBrush.SetBlendTriangularShape(Focus: Single; Scale: Single = 1): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientLinearBlend(GpPathGradient(FNativeBrush), Focus, Scale));
end;

function TPathGradientBrush.GetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientTransform(GpPathGradient(FNativeBrush), Matrix.NativeMatrix));
end;

function TPathGradientBrush.SetTransform(Matrix: IMatrix): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientTransform(
    GpPathGradient(FNativeBrush),
    Matrix.NativeMatrix));
end;

function TPathGradientBrush.ResetTransform: TStatus;
begin
  Result := SetStatus(GdiPresetPathGradientTransform(
    GpPathGradient(FNativeBrush)));
end;

function TPathGradientBrush.MultiplyTransform(Matrix: IMatrix; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipMultiplyPathGradientTransform(
    GpPathGradient(FNativeBrush), Matrix.NativeMatrix, Order));
end;

function TPathGradientBrush.TranslateTransform(DX, DY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipTranslatePathGradientTransform(
    GpPathGradient(FNativeBrush), DX, DY, Order));
end;

function TPathGradientBrush.ScaleTransform(SX, SY: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipScalePathGradientTransform(
    GpPathGradient(FNativeBrush), SX, SY, Order));
end;

function TPathGradientBrush.RotateTransform(Angle: Single; Order: TMatrixOrder = MatrixOrderPrepend): TStatus;
begin
  Result := SetStatus(GdipRotatePathGradientTransform(
    GpPathGradient(FNativeBrush), Angle, Order));
end;

function TPathGradientBrush.GetFocusScales(out XScale, YScale: Single): TStatus;
begin
  Result := SetStatus(GdipGetPathGradientFocusScales(
    GpPathGradient(FNativeBrush), XScale, YScale));
end;

function TPathGradientBrush.SetFocusScales(XScale, YScale: Single): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientFocusScales(
    GpPathGradient(FNativeBrush), XScale, YScale));
end;

function TPathGradientBrush.GetWrapMode: TWrapMode;
begin
  SetStatus(GdipGetPathGradientWrapMode(GpPathGradient(FNativeBrush), Result));
end;

function TPathGradientBrush.SetWrapMode(WrapMode: TWrapMode): TStatus;
begin
  Result := SetStatus(GdipSetPathGradientWrapMode(
    GpPathGradient(FNativeBrush), WrapMode));
end;

constructor TPathGradientBrush.Create;
begin
  inherited Create;
end;
