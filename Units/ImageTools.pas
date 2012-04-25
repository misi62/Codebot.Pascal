(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.05 Open Source Released 2009                   *)
(*                                                      *)
(********************************************************)
unit ImageTools;

interface

uses
	Windows, Messages, ActiveX, SysUtils, Classes, ComObj, Graphics,
  BaseTypes, GdiPlus, ImageCodecs;

{ TImageBitmap }

type
	TImageBitmapFormat = string;

const
  BmpFormat = 'bmp';
  GifFormat = 'gif';
  JpgFormat = 'jpg';
	PngFormat = 'png';
	TifFormat = 'tiff';

var
	DefaultFormat: TImageBitmapFormat = PngFormat;

type
	TImageBitmap = class(TGraphic)
  private
  	FFactory: IWICImagingFactory;
  	FCanvas: TCanvas;
		FBitmap: TFastBitmap;
    FFormat: TImageBitmapFormat;
    FWidth: Integer;
    FHeight: Integer;
    FStride: Integer;
    FOpacity: Byte;
    FScaleX: Single;
    FScaleY: Single;
    FPixelDepth: TPixelDepth;
    function AllowBlit(out Func: TBlendFunction; Opacity: Byte): Boolean;
    function GetBitmap: TFastBitmap;
    function GetBits: Pointer;
    function GetBounds: TRect;
    function GetScanline(Row: Integer): Pointer;
    function GetSize: Integer;
    procedure SetImageBitmapFormat(const Value: string);
    procedure SetPixelDepth(const Value: TPixelDepth);
  protected
    procedure HandleNeeded(AllowChange: Boolean = True);
    procedure DestroyHandle;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetCanvas: TCanvas; virtual;
    function GetEmpty: Boolean; override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
  public
  	constructor Create; override;
  	destructor Destroy; override;
    procedure RequestBitmap(out Bitmap: TFastBitmap);
    procedure Assign(Source: TPersistent); override;
    procedure Blit(DC: HDC; const Rect: TRect; Opacity: Byte = $FF); overload;
    procedure Blit(DC: HDC; X, Y, Index: Integer; Opacity: Byte = $FF); overload;
    procedure Blit(DC: HDC; const Rect: TRect; const Borders: TRect; Opacity: Byte = $FF); overload;
    procedure Clear(const Rect: TRect);
    procedure Load(Stream: TStream; const AFormat: TImageBitmapFormat);
		procedure Save(Stream: TStream; const AFormat: TImageBitmapFormat);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromFile(const Filename: string); override;
    procedure SaveToFile(const Filename: string); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    procedure LoadFromResourceName(const ResName: string);
    procedure LoadFromResourceID(ResID: Integer);
		property Format: TImageBitmapFormat read FFormat write SetImageBitmapFormat;
    property Bitmap: TFastBitmap read GetBitmap;
    property Bits: Pointer read GetBits;
    property Canvas: TCanvas read GetCanvas;
    property Bounds: TRect read GetBounds;
    property Opacity: Byte read FOpacity write FOpacity;
    property PixelDepth: TPixelDepth read FPixelDepth write SetPixelDepth;
    property ScaleX: Single read FScaleX write FScaleX;
    property ScaleY: Single read FScaleY write FScaleY;
    property Scanline[Row: Integer]: Pointer read GetScanLine;
    property Stride: Integer read FStride;
    property Size: Integer read GetSize;
  end;

{ The following are extensible bitmap operations }

{ Color mixing operations }
procedure ImageSaturate(Bitmap: TImageBitmap; Color: TColor);
procedure ImageScreen(Bitmap: TImageBitmap; Color: TColor);
procedure ImageColorize(Bitmap: TImageBitmap; Color: TColor);
{ Convert fade the alpha channel across the bitmap in a direction }
procedure ImageFade(Bitmap: TImageBitmap; Direction: TDirection);
{ Convert the image to greyscale }
procedure ImageGrayscale(Bitmap: TImageBitmap);

implementation

uses
	Consts, StrConst;

procedure InvalidOperation(Str: PResStringRec);
begin
  raise EInvalidGraphicOperation.CreateRes(Str);
end;

constructor TImageBitmap.Create;
begin
	inherited Create;
  if LoadWinCodecs then
	  CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER,
  	  IID_IWICImagingFactory, FFactory);
  FFormat := DefaultFormat;
  FPixelDepth := pd32;
  FScaleX := 1;
  FScaleY := 1;
  FOpacity := $FF;
end;

destructor TImageBitmap.Destroy;
begin
	DestroyHandle;
  inherited Destroy;
end;

procedure TImageBitmap.HandleNeeded(AllowChange: Boolean = True);
begin
	if IsFastBitmap(FBitmap) then Exit;
  if (FWidth < 1) or (FHeight < 1) then
    InvalidOperation(@SInvalidGraphicSize);
	FBitmap := CreateFastBitmap(FWidth, -FHeight, FPixelDepth);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := FBitmap.DC;
  FStride := ScanlineStride(FBitmap);
  if AllowChange then
	  Changed(Self);
end;

procedure TImageBitmap.DestroyHandle;
begin
	if not IsFastBitmap(FBitmap) then Exit;
  FreeAndNil(FCanvas);
	DestroyFastBitmap(FBitmap);
  Changed(Self);
end;

procedure TImageBitmap.Assign(Source: TPersistent);
var
	Image: TImageBitmap absolute Source;
begin
	if Source is TImageBitmap then
  begin
  	Height := Image.Height;
    Width := Image.Width;
    PixelDepth := Image.PixelDepth;
    Opacity := Image.Opacity;
    Format := Image.Format;
    ScaleX := Image.ScaleX;
    ScaleY := Image.ScaleY;
  	if Image.Empty then
    	DestroyHandle
		else
    begin
    	HandleNeeded;
      FStride := ScanlineStride(FBitmap);
      Move(Image.FBitmap.Bits^, FBitmap.Bits^, FStride * FHeight);
    end;
  end
  else
	  inherited Assign(Source);
end;

procedure TImageBitmap.RequestBitmap(out Bitmap: TFastBitmap);
begin
	HandleNeeded;
  Bitmap := FBitmap;
end;

procedure TImageBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
	Blit(ACanvas.Handle, Rect);
end;

function TImageBitmap.AllowBlit(out Func: TBlendFunction; Opacity: Byte): Boolean;
var
  Alpha: Byte;
begin
  Result := False;
  if Empty then Exit;
  Alpha := Opacity;
  if Alpha = $FF then
  	Alpha := FOpacity;
  if Alpha = 0 then
  	Exit;
  Result := True;
  FillChar(Func, SizeOf(Func), #0);
	Func.SourceConstantAlpha := Alpha;
  if FPixelDepth = pd32 then
    Func.AlphaFormat := AC_SRC_ALPHA;
end;

procedure TImageBitmap.Blit(DC: HDC; const Rect: TRect; Opacity: Byte = $FF);
var
	Func: TBlendFunction;
  W, H: Integer;
begin
  if AllowBlit(Func, Opacity) then
  begin
    W := Width;
    H := Height;
    if FScaleX <> 1 then
	  	W := Round(W * FScaleX);
    if FScaleY <> 1 then
	  	H := Round(H * FScaleY);
  	AlphaBlend(DC, Rect.Left, Rect.Top, W, H,
    	FBitmap.DC, 0, 0, FWidth, FHeight, Func);
  end;
end;

procedure TImageBitmap.Blit(DC: HDC; X, Y, Index: Integer; Opacity: Byte = $FF);
var
	Func: TBlendFunction;
  W, H: Integer;
begin
  if AllowBlit(Func, Opacity) then
  begin
    if Width > Height then
	    W := FHeight
  	else
	    W := FWidth;
  	H := W;
    if FScaleX <> 1 then
		  W := Round(W * FScaleX);
    if FScaleY <> 1 then
	  	H := Round(H * FScaleY);
    { Support for both horizontal and vertical image strips }
    if Width > Height then
		  AlphaBlend(DC, X, Y, W, H,
    		FBitmap.DC, Index * FHeight, 0, FHeight, FHeight, Func)
	  else
		  AlphaBlend(DC, X, Y, W, H,
    		FBitmap.DC, 0, Index * FWidth, FWidth, FWidth, Func);
  end;
end;

procedure TImageBitmap.Blit(DC: HDC; const Rect: TRect; const Borders: TRect; Opacity: Byte = $FF);
var
	Func: TBlendFunction;
begin
  if AllowBlit(Func, Opacity) then
    with Borders do
    begin
    	AlphaBlend(DC, Rect.Left, Rect.Top, Left, Top,
        FBitmap.DC, 0, 0, Left, Top, Func);
    	AlphaBlend(DC, Rect.Left + Left, Rect.Top, WidthOf(Rect) - (Left + Right), Top,
        FBitmap.DC, Left, 0, Width - (Left + Right), Top, Func);
    	AlphaBlend(DC, Rect.Right - Right, Rect.Top, Right, Top,
        FBitmap.DC, Width - Right, 0, Right, Top, Func);

    	AlphaBlend(DC, Rect.Left, Rect.Top + Top, Left, HeightOf(Rect) - (Top + Bottom),
        FBitmap.DC, 0, Top, Left, Height - (Top + Bottom), Func);
    	AlphaBlend(DC, Rect.Left + Left, Rect.Top + Top, WidthOf(Rect) - (Left + Right), HeightOf(Rect) - (Top + Bottom),
        FBitmap.DC, Left, Top, Width - (Left + Right), Height - (Top + Bottom), Func);
    	AlphaBlend(DC, Rect.Right - Right, Rect.Top + Top, Right, HeightOf(Rect) - (Top + Bottom),
        FBitmap.DC, Width - Right, Top, Right, Height - (Top + Bottom), Func);

    	AlphaBlend(DC, Rect.Left, Rect.Bottom - Bottom, Left, Bottom,
        FBitmap.DC, 0, Height - Bottom, Left, Bottom, Func);
    	AlphaBlend(DC, Rect.Left + Left, Rect.Bottom - Bottom, WidthOf(Rect) - (Left + Right), Bottom,
        FBitmap.DC, Left, Height - Bottom, Width - (Left + Right), Bottom, Func);
    	AlphaBlend(DC, Rect.Right - Right, Rect.Bottom - Bottom, Right, Bottom,
        FBitmap.DC, Width - Right, Height - Bottom, Right, Bottom, Func);
    end;
end;

procedure TImageBitmap.Clear(const Rect: TRect);
var
  RGBA: PRGBA;
  R: TRect;
  I: Integer;
begin
  if Empty then Exit;
  HandleNeeded(False);
  if PixelDepth = pd32 then
  begin
    R := Rect;
    if R.Left < 0 then R.Left := 0;
    if R.Top < 0 then R.Top := 0;
    if R.Right > Width then R.Right := Width;
    if R.Bottom > Height then R.Bottom := Height;
    for I := Height - R.Top - 1 downto Height - R.Bottom do
    begin
      RGBA := Scanline[I];
      Inc(RGBA, R.Left);
      FillChar(RGBA^, WidthOf(R) * SizeOf(TRGBA), #0);
    end;
  end
  else
    FillRect(FBitmap.DC, Rect, GetStockObject(BLACK_BRUSH));
end;

function IsGuidEqual(const A, B: TGUID): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(A));
end;

procedure Premultiply(const Bitmap: TFastBitmap);
var
  Mix: Boolean;
  W, H: Integer;
  P: PRGBA;
  R: Single;
begin
	if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    Mix := False;
    for H := 0 to Bitmap.Height - 1 do
    begin
      P := Bitmap.Bits;
      Inc(P, H * Bitmap.Width);
      for W := 0 to Bitmap.Width - 1 do
      begin
        Mix := ((P.Blue + P.Green + P.Red > 0) and (P.Alpha = 0)) or
          (P.Blue > P.Alpha) or (P.Green > P.Alpha) or (P.Red > P.Alpha);
        if Mix then Break;
        Inc(P);
      end;
      if Mix then Break;
    end;
    if Mix then
      for H := 0 to Bitmap.Height - 1 do
      begin
        P := Bitmap.Bits;
        Inc(P, H * Bitmap.Width);
        for W := 0 to Bitmap.Width - 1 do
        begin
          if P.Alpha < $FF then
          begin
            R := P.Alpha / $FF;
            P.Blue := Round(P.Blue * R);
            P.Green := Round(P.Green * R);
            P.Red := Round(P.Red * R);
          end;
          Inc(P);
        end;
      end;
  end;
end;


type
  TSharedStream = class(TStream)
  private
    FStream: TStream;
    FStart: Int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(Stream: TStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ TSharedStream }

constructor TSharedStream.Create(Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FStart := FStream.Position;
end;

function TSharedStream.GetSize: Int64;
begin
  Result := FStream.Size - FStart;
end;

procedure TSharedStream.SetSize(NewSize: Longint);
begin
  FStream.Size := NewSize + FStart;
end;

procedure TSharedStream.SetSize(const NewSize: Int64);
begin
  FStream.Size := NewSize + FStart;
end;

function TSharedStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Count);
end;

function TSharedStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Count);
end;

function TSharedStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      begin
        if Offset < 0 then
          Offset := 0;
        Result := FStream.Seek(Offset + FStart, Origin) - FStart;
      end;
    soFromCurrent:
      begin
        if FStream.Position + Offset < FStart then
          Offset := FStart - FStream.Position;
        Result := FStream.Seek(Offset, Origin) - FStart;
      end;
    soFromEnd:
      begin
        if Position + Offset < FStart then
          Offset := FStart - Position;
        Result := FStream.Seek(Offset, Origin) - FStart;
      end;
  else
    Result := 0;
  end;
end;

function TSharedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  O: Int64;
begin
  O := Offset;
  case Origin of
    soBeginning:
      begin
        if O < 0 then
          O := 0;
        Result := FStream.Seek(O + FStart, Origin) - FStart;
      end;
    soCurrent:
      begin
        if FStream.Position + O < FStart then
          O := FStart - FStream.Position;
        Result := FStream.Seek(O, Origin) - FStart;
      end;
    soEnd:
      begin
        if FStream.Position + O < FStart then
          O := FStart - FStream.Position;
        Result := FStream.Seek(O, Origin) - FStart;
      end;
  else
    Result := 0;
  end;
end;

procedure TImageBitmap.Load(Stream: TStream; const AFormat: TImageBitmapFormat);
var
	Adapter: IStream;

	procedure LoadWicBitmap;
  var
    BitmapDecoder: IWICBitmapDecoder;
	  BitmapFrameDecode: IWICBitmapFrameDecode;
  	Converter: IWICFormatConverter;
	  Source: IWICBitmapSource;
	  W, H: LongWord;
  	G: TGUID;
  begin
		OleCheck(FFactory.CreateDecoderFromStream(Adapter, nil,
      WICDecodeMetadataCacheOnLoad, BitmapDecoder));
	  OleCheck(BitmapDecoder.GetFrame(0, BitmapFrameDecode));
	  OleCheck(BitmapFrameDecode.GetPixelFormat(G));
  	if IsGuidEqual(G, GUID_WICPixelFormat32bppBGRA) then
    	Source := BitmapFrameDecode
	  else
  	begin
	    OleCheck(FFactory.CreateFormatConverter(Converter));
    	OleCheck(Converter.Initialize(BitmapFrameDecode, GUID_WICPixelFormat32bppBGRA,
      	WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeCustom));
	    Source := Converter;
  	end;
  	OleCheck(Source.GetSize(W, H));
    FWidth := W;
    FHeight := H;
    HandleNeeded(False);
    if IsFastBitmap(FBitmap) then
	  	OleCheck(Source.CopyPixels(nil, FStride, FStride * FHeight, FBitmap.Bits))
  end;

  procedure LoadGdiBitmap;
  var
  	GdiBitmap: IGdiBitmap;
    Rect: TRectI;
    Data: TBitmapData;
  begin
  	GdiBitmap := NewBitmap(Adapter);
    if GdiBitmap.LastStatus <> Ok then
    	Exit;
    Rect.X := 0;
    Rect.Y := 0;
    Rect.Width := GdiBitmap.Width;
    Rect.Height := GdiBitmap.Height;
    FWidth := Rect.Width;
    FHeight := Rect.Height;
    HandleNeeded(False);
		if GdiBitmap.LockBits(Rect, ImageLockModeRead, PixelFormat32bppARGB, Data) <> Ok then
      InvalidOperation(@SCouldNotLockBits);
    Move(Data.Scan0^, FBitmap.Bits^, FStride *  FHeight);
    GdiBitmap.UnlockBits(Data);
  end;

var
  Share: TSharedStream;
begin
	DestroyHandle;
  FPixelDepth := pd32;
	FWidth := 0;
  FHeight := 0;
  FStride := 0;
  Format := LowerCase(AFormat);
  Share := TSharedStream.Create(Stream);
  Adapter := TStreamAdapter.Create(Share, soOwned);
  if FFactory <> nil then
	  LoadWicBitmap
  else
    LoadGdiBitmap;
	if IsFastBitmap(FBitmap) then
  	Premultiply(FBitmap)
	else
  begin
  	FHeight := 0;
    FWidth := 0;
    FStride := 0;
	end;
	Changed(Self);
end;

{var
  MustCopy: Boolean;
  Memory: TMemoryStream;
begin
	DestroyHandle;
  FPixelDepth := pd32;
	FWidth := 0;
  FHeight := 0;
  FStride := 0;
  Format := LowerCase(AFormat);
  MustCopy := Stream.Position > 0;
  if MustCopy then
    Memory := TMemoryStream.Create
  else
    Memory := nil;
  try
    if MustCopy then
    begin
      Memory.CopyFrom(Stream, Stream.Size - Stream.Position);
  	  Adapter := TStreamAdapter.Create(Memory);
    end
    else
  	  Adapter := TStreamAdapter.Create(Stream);
    if FFactory <> nil then
		  LoadWicBitmap
    else
  		LoadGdiBitmap;
  finally
    Memory.Free;
  end;
	if IsFastBitmap(FBitmap) then
  	Premultiply(FBitmap)
	else
  begin
  	FHeight := 0;
    FWidth := 0;
    FStride := 0;
	end;
	Changed(Self);
end;}

procedure TImageBitmap.Save(Stream: TStream; const AFormat: TImageBitmapFormat);
var
	Adapter: IStream;

	procedure SaveWicBitmap;
  var
	  SaveStream: IWICStream;
		BitmapInstance: IWICBitmap;
	  BitmapSource: IWICBitmapSource;
	  BitmapEncoder: IWICBitmapEncoder;
	  BitmapFrameEncode: IWICBitmapFrameEncode;
	  PropertyBag: IPropertyBag2;
	  Converter: IWICFormatConverter;
	  Palette: IWICPalette;
	  S: WideString;
	  G: TGUID;
  begin
    OleCheck(FFactory.CreateBitmapFromMemory(FWidth, FHeight,
      GUID_WICPixelFormat32bppBGRA, FWidth * 4, FHeight * FWidth * 4, Bits,
      BitmapInstance));
  	S := Format;
  	OleCheck(WICMapShortNameToGuid(PWideChar(S), G));
    BitmapSource := BitmapInstance;
    OleCheck(FFactory.CreateEncoder(G, nil, BitmapEncoder));
    OleCheck(FFactory.CreateStream(SaveStream));
    OleCheck(SaveStream.InitializeFromIStream(Adapter));
    OleCheck(BitmapEncoder.Initialize(SaveStream, WICBitmapEncoderNoCache));
    OleCheck(BitmapEncoder.CreateNewFrame(BitmapFrameEncode, PropertyBag));
    OleCheck(BitmapFrameEncode.Initialize(PropertyBag));
    OleCheck(BitmapFrameEncode.SetSize(FWidth, FHeight));
    G := GUID_WICPixelFormat32bppBGRA;
    OleCheck(BitmapFrameEncode.SetPixelFormat(G));
    if not IsGuidEqual(GUID_WICPixelFormat32bppBGRA, G) then
    begin
      OleCheck(FFactory.CreateFormatConverter(Converter));
      if IsGuidEqual(GUID_WICPixelFormat8bppIndexed, G) then
      begin
        OleCheck(FFactory.CreatePalette(Palette));
        OleCheck(Palette.InitializeFromBitmap(BitmapSource, $100, False));
        OleCheck(BitmapFrameEncode.SetPalette(Palette));
        OleCheck(Converter.Initialize(BitmapSource, G,
          WICBitmapDitherTypeErrorDiffusion, Palette, 0,
          WICBitmapPaletteTypeMedianCut));
      end
      else
        OleCheck(Converter.Initialize(BitmapSource, G,
          WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeCustom));
      BitmapSource := Converter;
    end;
    OleCheck(BitmapFrameEncode.WriteSource(BitmapSource, nil));
    OleCheck(BitmapFrameEncode.Commit);
    OleCheck(BitmapEncoder.Commit);
  end;

  procedure SaveGdiBitmap;
  var
  	GdiBitmap: IGdiBitmap;
    Rect: TRectI;
    Data: TBitmapData;
    B: Boolean;
    G: TGUID;
  begin
    Rect.X := 0;
    Rect.Y := 0;
    Rect.Width := FWidth;
    Rect.Height := FHeight;
  	GdiBitmap := NewBitmap(FWidth, FHeight);
		if GdiBitmap.LockBits(Rect, ImageLockModeRead, PixelFormat32bppARGB, Data) <> Ok then
      InvalidOperation(@SCouldNotLockBits);
		Move(FBitmap.Bits^, Data.Scan0^, FWidth * FHeight * 4);
		GdiBitmap.UnlockBits(Data);
    B := GetEncoderClsid('image/' + Format, G);
		if B then
	    GdiBitmap.Save(Adapter, G)
		else
    	InvalidOperation(@SInvalidGraphicFormat);
  end;

begin
	if Empty then Exit;
  HandleNeeded(False);
  Adapter := TStreamAdapter.Create(Stream);
  FFormat := LowerCase(AFormat);
  {if FFactory <> nil then
		SaveWicBitmap
  else}
		SaveGdiBitmap;
end;

procedure TImageBitmap.LoadFromStream(Stream: TStream);
begin
	Load(Stream, Format);
end;

procedure TImageBitmap.SaveToStream(Stream: TStream);
begin
	Save(Stream, Format);
end;

procedure TImageBitmap.LoadFromResourceName(const ResName: string);
var
	Stream: TStream;
begin
  Stream := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
  try
  	LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TImageBitmap.LoadFromResourceID(ResID: Integer);
var
	Stream: TStream;
begin
	Stream := TResourceStream.CreateFromID(HInstance, ResID, RT_RCDATA);
  try
  	LoadFromStream(Stream);
  finally
  	Stream.Free;
  end;
end;

function ExtractFormat(const Filename: string): string;
begin
	Result := Copy(ExtractFileExt(LowerCase(Filename)), 2, MAX_PATH);
end;

procedure TImageBitmap.LoadFromFile(const Filename: string);
begin
	Format := ExtractFormat(Filename);
  inherited LoadfromFile(Filename);
end;

procedure TImageBitmap.SaveToFile(const Filename: string);
begin
	Format := ExtractFormat(Filename);
  inherited SaveToFile(Filename);
end;

procedure TImageBitmap.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
end;

procedure TImageBitmap.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
end;

function TImageBitmap.GetCanvas: TCanvas;
begin
	HandleNeeded;
  Result := FCanvas;
end;

function TImageBitmap.GetBitmap: TFastBitmap;
begin
  RequestBitmap(Result);
end;

function TImageBitmap.GetBits: Pointer;
begin
	HandleNeeded;
	Result := FBitmap.Bits;
end;

function TImageBitmap.GetBounds: TRect;
begin
  Result := Rect(0, 0, FWidth, FHeight);
end;

function TImageBitmap.GetScanline(Row: Integer): Pointer;
var
	B: PByte absolute Result;
begin
	HandleNeeded;
	if (Row < 0) or (Row > FBitmap.Height - 1) then
		InvalidOperation(@SScanLine);
	Result := FBitmap.Bits;
  Inc(B, FStride * Row);
end;

function TImageBitmap.GetTransparent: Boolean;
begin
	Result := True;
end;

procedure TImageBitmap.SetTransparent(Value: Boolean);
begin
end;

function TImageBitmap.GetEmpty: Boolean;
begin
	Result := (FWidth = 0) or (FHeight = 0);
end;

procedure TImageBitmap.SetHeight(Value: Integer);
begin
	if Value < 0 then
	  Value := 0;
  if Value <> FHeight then
  begin
  	FHeight := Value;
    DestroyHandle;
	end;
end;

procedure TImageBitmap.SetWidth(Value: Integer);
begin
	if Value < 0 then
	  Value := 0;
  if Value <> FWidth then
  begin
  	FWidth := Value;
    DestroyHandle;
	end;
end;

function TImageBitmap.GetHeight: Integer;
begin
	Result := FHeight;
end;

function TImageBitmap.GetWidth: Integer;
begin
	Result := FWidth;
end;

function TImageBitmap.GetSize: Integer;
begin
	if FWidth > FHeight then
  	Result := FHeight
	else
  	Result := FWidth;
end;

procedure TImageBitmap.SetImageBitmapFormat(const Value: string);
var
	Success: Boolean;
	S: WideString;
  G: TGUID;
begin
  if Value <> FFormat then
  begin
  	Success := False;
  	S := LowerCase(Value);
  	if FFactory <> nil then
  	begin
      Success := WICMapShortNameToGuid(PWideChar(S), G) = S_OK;
      if Success then
  			FFormat := S;
    end;
  	if not Success then
    	if S = 'png' then
  			FFormat := PngFormat
      else if S = 'bmp' then
  			FFormat := BmpFormat
      else if S = 'jpg' then
  			FFormat := JpgFormat
      else if S = 'jpeg' then
  			FFormat := JpgFormat
      else if S = 'gif' then
  			FFormat := GifFormat
      else if S = 'tif' then
  			FFormat := TifFormat
      else if S = 'tiff' then
  			FFormat := TifFormat;
  		{ else use the current format }
  end;
end;

procedure TImageBitmap.SetPixelDepth(const Value: TPixelDepth);
begin
  if Value <> PixelDepth then
  begin
  	FPixelDepth := Value;
    DestroyHandle;
  end;
end;

{ Extensible bitmap operations }

procedure ImageSaturate(Bitmap: TImageBitmap; Color: TColor);
var
  B: TFastBitmap;
  RGBA: PRGBA;
  C: TRGBA;
  A, L: Single;
  I: Integer;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  RGBA := B.Bits;
  C := ColorToRGBA(Color);
  for I := 0 to B.Width * B.Height - 1 do
  begin
    L := (RGBA.Red + RGBA.Green + RGBA.Blue) / (3 * $FF);
    A := RGBA.Alpha / $FF;
    if L < 0.5 then
    begin
      RGBA.Red := Round(L * 2 * C.Red * A);
      RGBA.Green := Round(L * 2 * C.Green * A);
      RGBA.Blue := Round(L * 2 * C.Green * A);
    end
    else
    begin
      RGBA.Red := Round((((1 - L) / 0.5) * C.Red + ((L - 0.5) * 2) * $FF) * A);
      RGBA.Green := Round((((1 - L) / 0.5) * C.Green + ((L - 0.5) * 2) * $FF) * A);
      RGBA.Blue := Round((((1 - L) / 0.5) * C.Blue + ((L - 0.5) * 2) * $FF) * A);
    end;
    Inc(RGBA);
  end;
end;

procedure ImageScreen(Bitmap: TImageBitmap; Color: TColor);
var
  B: TFastBitmap;
  RGBA: PRGBA;
  C: TRGBA;
  A, L: Single;
  I: Integer;
begin
  if Bitmap.Empty then
    Exit;
  Bitmap.RequestBitmap(B);
  RGBA := B.Bits;
  C := ColorToRGBA(Color);
  for I := 0 to B.Width * B.Height - 1 do
  begin
    L := (RGBA.Red + RGBA.Green + RGBA.Blue) / (3 * $FF);
    A := RGBA.Alpha / $FF;
    RGBA.Red := Round(((1 - L) * C.Red + L * $FF) * A);
    RGBA.Green := Round(((1 - L) * C.Green + L * $FF) * A);
    RGBA.Blue := Round(((1 - L) * C.Blue + L * $FF) * A);
    Inc(RGBA);
  end;
end;

procedure ImageColorize(Bitmap: TImageBitmap; Color: TColor);
var
  B: TFastBitmap;
  RGBA: PRGBA;
  C: TRGBA;
  A: Single;
  I: Integer;
begin
  if Bitmap.Empty then
    Exit;
	Bitmap.RequestBitmap(B);
  RGBA := B.Bits;
  C := ColorToRGBA(Color);
  for I := 0 to B.Width * B.Height - 1 do
  begin
    if RGBA.Alpha = 0 then
    begin
      Inc(RGBA);
      Continue;
    end;
    if RGBA.Alpha = $FF then
    begin
      RGBA.Red := C.Red;
      RGBA.Green := C.Green;
      RGBA.Blue := C.Blue;
      Inc(RGBA);
      Continue;
    end;
    A := RGBA.Alpha / $FF;
    RGBA.Red := Round(C.Red * A);
    RGBA.Green := Round(C.Green * A);
    RGBA.Blue := Round(C.Blue * A);
    Inc(RGBA);
  end;
end;

procedure ImageFade(Bitmap: TImageBitmap; Direction: TDirection);
var
  B: TFastBitmap;
  RGBA: PRGBA;
  A: Single;
  X, Y: Integer;
begin
  if Bitmap.Empty then
    Exit;
	Bitmap.RequestBitmap(B);
  RGBA := B.Bits;
  A := 1;
  for Y := 0 to B.Height - 1 do
    for X := 0 to B.Width - 1 do
    begin
      if RGBA.Alpha = 0 then
      begin
        Inc(RGBA);
        Continue;
      end;
      case Direction of
        drUp:
          begin
            if Y = 0 then
            begin
              Inc(RGBA);
              Continue;
            end;
            A := 1 - Y / B.Height;
          end;
        drDown:
          begin
            if Y = B.Height - 1 then
            begin
              Inc(RGBA);
              Continue;
            end;
            A := Y / B.Height;
          end;
        drLeft:
          begin
            if X = B.Width - 1 then
            begin
              Inc(RGBA);
              Continue;
            end;
            A := X / B.Width;
          end;
        drRight:
          begin
            if X = 0 then
            begin
              Inc(RGBA);
              Continue;
            end;
            A := 1 - X / B.Width;
          end;
      else
        Exit;
      end;
      RGBA.Red := Round(RGBA.Red * A);
      RGBA.Green := Round(RGBA.Green * A);
      RGBA.Blue := Round(RGBA.Blue * A);
      RGBA.Alpha := Round(RGBA.Alpha * A);
      Inc(RGBA);
    end;
end;

procedure ImageGrayscale(Bitmap: TImageBitmap);
var
  A: PRGBA;
  B: TFastBitmap;
  X, Y: Integer;
begin
  if Bitmap.Empty then
    Exit;
	Bitmap.RequestBitmap(B);
  A := B.Bits;
  for Y := 0 to B.Height - 1 do
    for X := 0 to B.Width - 1 do
		begin
    	A.Red := Round(0.3 * A.Red + 0.6 * A.Green + 0.1 * A.Blue);
      A.Blue := A.Red;
      A.Green := A.Red;
			Inc(A);
    end;
end;

end.
