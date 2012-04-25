(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)
unit GdiIntf;

interface

{$I CODEBOT.INC}

uses
  Windows, ActiveX, SysUtils, Math, Classes, Graphics, BaseTypes, GdiPlus;

{ Image routines }

type
  TScaleKind = (skNone, skPercent, skWidth, skHeight);

  TScaleInfo = record
    Kind: TScaleKind;
    Value: Integer;
  end;

function GdiRequireScale(Width, Height: Integer; const Scale: TScaleInfo): Boolean; overload;
function GdiRequireScale(Bitmap: IGdiBitmap; const Scale: TScaleInfo): Boolean; overload;
function GdiScaleBitmap(Bitmap: IGdiBitmap; const Scale: TScaleInfo): IGdiBitmap;

{ Watermark positions are layed out as follows:

    +---+---+---+
    | 0 | 1 | 2 |
    +---+---+---+
    | 3 | 4 | 5 |
    +---+---+---+
    | 6 | 7 | 8 |
    +---+---+---+ }

type
  TWatermarkInfo = record
    Enabled: Boolean;
    FileName: string;
    Bitmap: IGdiBitmap;
    Grayscale: Boolean;
    Opacity: Single;
    Position: Integer;
    Offset: TPointI;
  end;

procedure GdiDrawWatermark(B: IGdiBitmap; const Watermark: TWatermarkInfo); overload;
procedure GdiDrawWatermark(G: IGdiGraphics; Rect: TRectF; const Watermark: TWatermarkInfo); overload;

function GdiUnlockBitmap(const FileName: string; Filled: Boolean = True; Color: TARGB = $FFFFFFFF): IGdiBitmap;
procedure GdiSaveImage(Bitmap: IGdiBitmap; const FileName: string; const Format: string = JpgFormat);

type
  TBitmapTagInfo = record
    Enabled: Boolean;
    Bitmap: IGdiBitmap;
    Tag: string;
  end;

procedure GdiSetBitmapTag(Bitmap: IGdiBitmap; const Tag: string);
function GdiGetBitmapTag(Bitmap: IGdiBitmap): string;

function GdiSaveJpeg(Bitmap: IGdiBitmap; const FileName: string;
  Compression: LongInt = 80; const Tag: string = ''): IGdiBitmap; overload;
function GdiSaveJpeg(Bitmap: IGdiBitmap; const FileName: string;
  const Scale: TScaleInfo; Compression: LongInt = 80; const Tag: string = ''): IGdiBitmap; overload;

function GdiImageResize(Bitmap: IGdiBitmap; Size: TSizeI): IGdiBitmap;
procedure GdiImageThumbnail(var Bitmap: IGdiBitmap; MaxSize: TSizeI); overload;
procedure GdiImageThumbnail(const SourceFile, DestFile: string;
  MaxSize: TSizeI; out OriginalSize: TSizeI; const Format: string = JpgFormat); overload;

{ Graphic routines  }

procedure GdiClear(G: IGdiGraphics; const R: TRectF);
procedure GdiFillRectColor(G: IGdiGraphics; const R: TRectF; C: TArgb);
procedure GdiDrawLine(G: IGdiGraphics; P: IGdiPen; const A, B: TVertex);

function NewRectangle(X, Y, Width, Height: Single): IGdiGraphicsPath;
function NewRoundRect(X, Y, Width, Height, Radius: Single): IGdiGraphicsPath; overload;
function NewRoundedRect(X, Y, Width, Height, Radius: Single): IGdiGraphicsPath; overload;
function NewRoundedRect(const Rect: TRectF; Radius: Single): IGdiGraphicsPath; overload;
function NewRoundRect(const Rect: TRectF; Radius: Single): IGdiGraphicsPath; overload;
function NewChamfer(const Rect: TRectF; Chamfer: Single): IGdiGraphicsPath; overload;
function NewChamfer(Width, Height, Chamfer: Single): IGdiGraphicsPath; overload;
function NewArrow(Size, Stem, Trunk: Single): IGdiGraphicsPath;
function NewCrossMark(const Rect: TRectF): IGdiGraphicsPath;

{ Text routines }

function GdiMeasureText(G: IGdiGraphics; F: IGdiFont; const S: string): TSizeF;

procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TArgb;
  const S: WideString; const Rect: TRectF; Direction: TDirection;
  Wrap: Boolean = False); overload;
procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TArgb;
  const S: WideString; const Point: TPointF; HAlign, VAlign: TStringAlignment); overload;
procedure GdiDrawTextAngle(G: IGdiGraphics; F: IGdiFont; Fill: TArgb;
  const S: WideString; const Point: TPointF; HAlign, VAlign: TStringAlignment;
  Angle: Single);

{ Draw routines }

procedure GdiDrawPath(G: IGdiGraphics; P: IGdiPen; B: IGdiBrush;
  S: IGdiGraphicsPath; DeltaX: Single = 0; DeltaY: Single = 0; DeltaAngle: Single = 0;
  ScaleX: Single = 1; ScaleY: Single = 1);

procedure GdiDrawRectState(G: IGdiGraphics; Rect: TRectF; State: TDrawState);

procedure GdiDrawStyleRoundRect(DC: HDC; Rect: TRect; Light: Boolean = False;
  Radius: Single = 7);

procedure GdiDrawBar(G: IGdiGraphics; Rect: TRectF; Back: TArgb);

{ Path routines }

procedure GdiAddLineToPath(P: IGdiGraphicsPath; const A, B: TVertex);
procedure GdiLinePath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float; Capped: Boolean = False);
procedure GdiArrowPath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float);
procedure GdiArrowCirclePath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float);
procedure GdiFillAndStrokePath(G: IGdiGraphics; P: IGdiGraphicsPath;
  Fill, Stroke: TArgb; StrokeWidth: Integer = 1);
procedure GdiStrokePath(G: IGdiGraphics; P: IGdiGraphicsPath; Stroke: TArgb;
  StrokeWidth: Integer = 1);

{ Draw routines }

procedure GdiDrawButton(G: IGdiGraphics; const Rect: TRectF; State: TDrawState);
procedure GdiDrawMenuRect(G: IGdiGraphics; const Rect: TRectF);

{ Matrix stack }

procedure GdiPushMatrix(G: IGdiGraphics);
procedure GdiPopMatrix(G: IGdiGraphics);

implementation

{ Image routines }

procedure GdiDrawWatermark(B: IGdiBitmap; const Watermark: TWatermarkInfo);
var
  G: IGdiGraphics;
  R: TRectF;
begin
  if (not Watermark.Enabled) or (Watermark.Opacity < 1) or (Watermark.Bitmap = nil)
    or (Watermark.Bitmap.GetWidth < 1) then
  Exit;
  G := NewGraphics(B);
  R := NewRectF(B.GetWidth, B.GetHeight);
  GdiDrawWatermark(G, R, Watermark);
end;

procedure GdiDrawWatermark(G: IGdiGraphics; Rect: TRectF; const Watermark: TWatermarkInfo);
var
  B: IGdiBitmap;
  R: TRectF;
  A: IGdiImageAttributes;
  M: TColorMatrix;
begin
  if (not Watermark.Enabled) or (Watermark.Opacity < 1) or (Watermark.Bitmap = nil)
    or (Watermark.Bitmap.GetWidth < 1) then
    Exit;
  B := Watermark.Bitmap;
  R := NewRectF(0, 0, B.GetWidth, B.GetHeight);
  if (Watermark.Opacity < 100) or Watermark.Grayscale then
  begin
    A := NewImageAttributes;
    if Watermark.Opacity < 100 then
      M := NewOpacityMatrix(Watermark.Opacity / 100)
    else
      M := NewColorMatrix;
    if Watermark.Grayscale then
      M := ColorGreyscale(M);
    A.SetColorMatrix(M);
  end
  else
    A := nil;
  case Watermark.Position of
    0, 3, 6:
      R.X := Rect.X + Watermark.Offset.X;
    1, 4, 7:
      R.X := Rect.X + Rect.Width / 2 - R.Width / 2 + Watermark.Offset.X;
  else
    R.X := Rect.X + Rect.Width - R.Width - Watermark.Offset.X;
  end;
  case Watermark.Position of
    0, 1, 2:
      R.Y := Rect.Y + Watermark.Offset.Y;
    3, 4, 5:
      R.Y := Rect.Y + Rect.Height / 2 - R.Height / 2 + Watermark.Offset.Y;
  else
    R.Y := Rect.Y + Rect.Height - R.Height - Watermark.Offset.Y;
  end;
  R.X := Round(R.X);
  R.Y := Round(R.Y);
  G.DrawImage(B, R, 0, 0, R.Width, R.Height, UnitPixel, A);
end;

type
  TFileMemoryStream = class(TMemoryStream)
  public
    constructor Create(const FileName: string; Mode: Word);
  end;

constructor TFileMemoryStream.Create(const FileName: string; Mode: Word);
var
  S: TStream;
begin
  inherited Create;
  S := TFileStream.Create(FileName, Mode);;
  try
    CopyFrom(S, 0);
  finally
    S.Free;
  end;
  Position := 0;
end;

function GdiUnlockBitmap(const FileName: string; Filled: Boolean = True;
  Color: TARGB = $FFFFFFFF): IGdiBitmap;
var
  S: TStream;
  A: IStream;
  W, H: Integer;
  B: IGdiBitmap;
  G: IGdiGraphics;
begin
  Result := nil;
  if FileExists(FileName) then
  begin
    S := TFileMemoryStream.Create(FileName, fmOpenRead);
    A := TStreamAdapter.Create(S, soOwned);
    try
      Result := NewBitmap(A);
    except
      Result := nil;
    end;
  end;
  if (Result <> nil) and Filled and (UpperCase(ExtractFileExt(FileName)) = '.PNG') then
  begin
    W := Result.GetWidth;
    H := Result.GetHeight;
    B := NewBitmap(W, H);
    G := NewGraphics(B);
    G.FillRectangle(NewSolidBrush(Color), 0, 0, W, H);
    G.DrawImage(Result, 0, 0, W, H);
    Result := B;
  end;
end;

procedure GdiSaveImage(Bitmap: IGdiBitmap; const FileName: string; const Format: string = JpgFormat);
var
  C: TGUID;
begin
  GetEncoderClsid(Format, C);
  Bitmap.Save(FileName, C);
end;

procedure GdiSetBitmapTag(Bitmap: IGdiBitmap; const Tag: string);
var
  Prop: TPropertyItem;
begin
  if Tag <> '' then
  begin
    Prop.id := PropertyTagExifUserComment;
    Prop.length := Length(Tag) + 1;
    Prop.type_ := PropertyTagTypeASCII;
    Prop.value := PChar(Tag);
    Bitmap.SetPropertyItem(Prop);
  end;
end;

function GdiGetBitmapTag(Bitmap: IGdiBitmap): string;
var
  B: PByte;
  P: PChar;
  I: Integer;
begin
  Result := '';
  I := Bitmap.GetPropertyItemSize(PropertyTagExifUserComment);
  if I > 0 then
  begin
    GetMem(B, I);
    try
      Bitmap.GetPropertyItem(PropertyTagExifUserComment, I, PPropertyItem(B));
      P := PChar(B);
      Inc(P, SizeOf(TPropertyItem));
      Result := P;
    finally
      FreeMem(B);
    end;
  end;
end;

function GdiSaveJpeg(Bitmap: IGdiBitmap; const FileName: string; Compression: LongInt = 80; const Tag: string = ''): IGdiBitmap;
var
  C: TGUID;
  P: TEncoderParameters;
begin
  GdiSetBitmapTag(Bitmap, Tag);
  GetEncoderClsid(JpgFormat, C);
  P.Count := 1;
  P.Parameter[0].Guid := EncoderQuality;
  P.Parameter[0].Type_ := EncoderParameterValueTypeLong;
  P.Parameter[0].NumberOfValues := 1;
  P.Parameter[0].Value := @Compression;
  Bitmap.Save(FileName, C, @P);
  Result := Bitmap;
end;

function GdiRequireScale(Width, Height: Integer; const Scale: TScaleInfo): Boolean; overload;
var
  I: Integer;
begin
  Result := False;
  I := Scale.Value;
  if I < 1 then
    I := 1
  else if Scale.Value > 9999 then
    I := 9999;
  case Scale.Kind of
    skPercent: Result := I <> 100;
    skWidth: Result := Width > I;
    skHeight: Result := Height > I;
  end;
end;

function GdiRequireScale(Bitmap: IGdiBitmap; const Scale: TScaleInfo): Boolean;
begin
  Result := GdiRequireScale(Bitmap.GetWidth, Bitmap.GetHeight, Scale);
end;

function GdiScaleBitmap(Bitmap: IGdiBitmap; const Scale: TScaleInfo): IGdiBitmap;
var
  B: IGdiBitmap;
  I: Cardinal;
begin
  I := Scale.Value;
  if I < 1 then
    I := 1
  else if Scale.Value > 9999 then
    I := 9999;
  B := Bitmap;
  case Scale.Kind of
    skPercent:
      if I <> 100 then
        B := GdiImageResize(Bitmap, NewSizeI(Round(B.GetWidth * (I / 100)),
          Round(B.GetHeight * (I / 100))));
    skWidth:
      if B.GetWidth > I then
        B := GdiImageResize(Bitmap, NewSizeI(I, Round(B.GetHeight /  B.GetWidth * I)));
    skHeight:
      if B.GetHeight > I then
        B := GdiImageResize(Bitmap, NewSizeI(Round(B.GetWidth /  B.GetHeight * I), I));
  end;
  Result := B;
end;

function GdiSaveJpeg(Bitmap: IGdiBitmap; const FileName: string;
  const Scale: TScaleInfo; Compression: LongInt = 80; const Tag: string = ''): IGdiBitmap;
begin
  Result := GdiSaveJpeg(GdiScaleBitmap(Bitmap, Scale), FileName, Compression, Tag);
end;

function GdiImageResize(Bitmap: IGdiBitmap; Size: TSizeI): IGdiBitmap;
var
  B: IGdiBitmap;
  G: IGdiGraphics;
begin
  if Size.Width < 1 then Size.Width := 1;
  if Size.Height < 1 then Size.Height := 1;
  B := NewBitmap(Size.Width, Size.Height);
  G := NewGraphics(B);
  G.SetInterpolationMode(InterpolationModeHighQuality);
  G.DrawImage(Bitmap, 0, 0, Size.Width, Size.Height);
  Result := B;
end;

procedure GdiImageThumbnail(var Bitmap: IGdiBitmap; MaxSize: TSizeI);
var
  B: IGdiBitmap;
  G: IGdiGraphics;
  W, H: Integer;
  SX, SY: Single;
begin
  W := Bitmap.GetWidth;
  H := Bitmap.GetHeight;
  if (W = 0) or (H = 0) then Exit;
  if (W <= MaxSize.Width) and (H <= MaxSize.Height) then Exit;
  SX := W / MaxSize.Width;
  SY := H / MaxSize.Height;
  if SX > SY then
  begin
    W := MaxSize.Width;
    H := Round(W * (SY / SX));
  end
  else
  begin
    H := MaxSize.Height;
    W := Round(H * (SX /SY));
  end;
  B := NewBitmap(W, H);
  G := NewGraphics(B);
  G.SetInterpolationMode(InterpolationModeHighQuality);
  G.DrawImage(Bitmap, 0, 0, W, H);
  Bitmap := B;
end;

procedure GdiImageThumbnail(const SourceFile, DestFile: string;
  MaxSize: TSizeI; out OriginalSize: TSizeI; const Format: string = JpgFormat);
var
  B: IGdiBitmap;
begin
  B := NewBitmap(SourceFile);
  OriginalSize.Width := B.GetWidth;
  OriginalSize.Height := B.GetHeight;
  GdiImageThumbnail(B, MaxSize);
  GdiSaveImage(B, DestFile, Format);
end;

{ Graphic routines  }

procedure GdiClear(G: IGdiGraphics; const R: TRectF);
var
  B: IGdiBrush;
begin
  B := NewCheckerBrush(aclWhite, aclSilver, 10);
  G.FillRectangle(B, R);
end;

procedure GdiFillRectColor(G: IGdiGraphics; const R: TRectF; C: TArgb);
begin
  G.FillRectangle(NewSolidBrush(C), R);
end;

procedure GdiDrawLine(G: IGdiGraphics; P: IGdiPen; const A, B: TVertex);
begin
  G.DrawLine(P, A.X, A.Y, B.X, B.Y);
end;

{ Text routines }

function GdiMeasureText(G: IGdiGraphics; F: IGdiFont; const S: string): TSizeF;
var
  Rect: TRectF;
begin
  Rect := NewRectF(0, 0, High(Word), High(Word));
  G.MeasureString(S, F, Rect, Rect);
  Result.Width := Rect.Width;
  Result.Height := Rect.Height;
end;

procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TArgb;
  const S: WideString; const Rect: TRectF; Direction: TDirection;
  Wrap: Boolean = False);
var
  HAlign, VAlign: TStringAlignment;
begin
  case Direction of
    drLeft:
      begin
        HAlign := StringAlignmentNear;
        VAlign := StringAlignmentCenter;
      end;
    drUp:
      begin
        HAlign := StringAlignmentCenter;
        VAlign := StringAlignmentNear;
      end;
    drRight:
      begin
        HAlign := StringAlignmentFar;
        VAlign := StringAlignmentCenter;
      end;
    drDown:
      begin
        HAlign := StringAlignmentCenter;
        VAlign := StringAlignmentFar;
      end;
    drCenter:
      begin
        HAlign := StringAlignmentCenter;
        VAlign := StringAlignmentCenter;
      end;
    drFill:
      begin
        HAlign := StringAlignmentNear;
        VAlign := StringAlignmentNear;
      end;
    drWrap:
      begin
        HAlign := StringAlignmentNear;
        VAlign := StringAlignmentNear;
        Wrap := True;
      end;
  else
    HAlign := StringAlignmentCenter;
    VAlign := StringAlignmentCenter;
  end;
  G.DrawString(S, F, Rect, NewStringFormat(HAlign, VAlign, Wrap),
    NewSolidBrush(Fill));
end;

procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TArgb;
  const S: WideString; const Point: TPointF; HAlign, VAlign: TStringAlignment);
begin
  G.DrawString(S, F, Point, NewStringFormat(HAlign, VAlign), NewSolidBrush(Fill));
end;

procedure GdiDrawTextAngle(G: IGdiGraphics; F: IGdiFont; Fill: TArgb;
  const S: WideString; const Point: TPointF; HAlign, VAlign: TStringAlignment;
  Angle: Single);
begin
  GdiPushMatrix(G);
  try
    G.TranslateTransform(Point.X, Point.Y);
    G.RotateTransform(-Angle);
    G.TranslateTransform(-Point.X, -Point.Y);
    G.DrawString(S, F, Point, NewStringFormat(HAlign, VAlign), NewSolidBrush(Fill));
  finally
    GdiPopMatrix(G);
  end;
end;

function NewRectangle(X, Y, Width, Height: Single): IGdiGraphicsPath;
begin
  Result := NewGraphicsPath;
  Result.AddRectangle(NewRectF(X, Y, Width, Height));
end;

function NewRoundRect(X, Y, Width, Height, Radius: Single): IGdiGraphicsPath;
begin
  Result := NewGraphicsPath;
  if Radius < 0 then Radius := 0.1;
  if Width / 2 < Radius then
    Radius := Width / 2 - 0.1;
  if Height / 2 <= Radius then
    Radius := Height / 2 - 0.1;
  Result.AddLine(X + Radius, Y, X + Width - Radius, Y);
  Result.AddArc(X + Width - (Radius * 2), Y, Radius * 2, Radius * 2, 270, 90);
  Result.AddLine(X + Width, Y + Radius, X + Width, Y + Height - Radius);
  Result.AddArc(X + Width - (Radius * 2), Y + Height - (Radius * 2), Radius * 2, Radius * 2, 0, 90);
  Result.AddLine(X + Width - Radius, Y + Height, X + Radius, Y + Height);
  Result.AddArc(X, Y + Height - (Radius * 2), Radius * 2, Radius * 2, 90, 90);
  Result.AddLine(X, Y + Height - Radius, X, Y + Radius);
  Result.AddArc(X, Y, Radius * 2, Radius * 2, 180, 90);
  Result.CloseFigure;
end;

function NewRoundRect(const Rect: TRectF; Radius: Single): IGdiGraphicsPath;
begin
  Result := NewRoundRect(Rect.X, Rect.Y, Rect.Width, Rect.Height, Radius);
end;

function Sqr(X: Extended): Extended;
begin
  Result := X * X;
end;

function NewRoundedRect(X, Y, Width, Height, Radius: Single): IGdiGraphicsPath; overload;
var
  Offset, Theta: Single;
begin
  if Radius < Height / 2 then
    Radius := Height / 2;
  Offset := Radius - Sqrt(Sqr(Radius) - Sqr(Height / 2));
  Theta := 90 - ArcCos((Height / 2) / Radius) * 180 / PI;
  Result := NewGraphicsPath;
  Result.AddLine(X + Offset, Y, X + Width - Offset, Y);
  Result.AddArc(X + Width - (Radius * 2), Y + Height / 2 - Radius, Radius * 2, Radius * 2, -Theta, Theta * 2);
  Result.AddLine(X + Width - Offset, Y + Height, X + Offset, Y + Height);
  Result.AddArc(X, Y + Height / 2 - Radius, Radius * 2, Radius * 2, 180 - Theta, Theta * 2);
  Result.CloseFigure;
end;

function NewRoundedRect(const Rect: TRectF; Radius: Single): IGdiGraphicsPath; overload;
begin
  Result := NewRoundedRect(Rect.X, Rect.Y, Rect.Width, Rect.Height, Radius);
end;

function NewChamfer(const Rect: TRectF; Chamfer: Single): IGdiGraphicsPath;
begin
  Result := NewGraphicsPath;
  Result.AddLine(Rect.X + Chamfer, Rect.Y, Rect.X + Rect.Width - Chamfer, Rect.Y);
  Result.AddLine(Rect.X + Rect.Width - Chamfer, Rect.Y, Rect.X + Rect.Width, Rect.Y + Chamfer);
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + Chamfer, Rect.X + Rect.Width, Rect.Y + Rect.Height - Chamfer);
  Result.AddLine(Rect.X + Rect.Width, Rect.Y + Rect.Height - Chamfer, Rect.X + Rect.Width - Chamfer, Rect.Y + Rect.Height);
  Result.AddLine(Rect.X + Rect.Width - Chamfer, Rect.Y + Rect.Height, Rect.X + Chamfer, Rect.Y + Rect.Height);
  Result.AddLine(Rect.X + Chamfer, Rect.Y + Rect.Height, Rect.X, Rect.Y + Rect.Height - Chamfer);
  Result.AddLine(Rect.X, Rect.Y + Rect.Height - Chamfer, Rect.X, Rect.Y + Chamfer);
  Result.CloseFigure;
end;

function NewChamfer(Width, Height, Chamfer: Single): IGdiGraphicsPath;
begin
  Result := NewGraphicsPath;
  Result.AddLine(Chamfer, 0, Width - Chamfer, 0);
  Result.AddLine(Width - Chamfer, 0, Width, Chamfer);
  Result.AddLine(Width, Chamfer, Width, Height - Chamfer);
  Result.AddLine(Width, Height - Chamfer, Width - Chamfer, Height);
  Result.AddLine(Width - Chamfer, Height, Chamfer, Height);
  Result.AddLine(Chamfer, Height, 0, Height - Chamfer);
  Result.AddLine(0, Height - Chamfer, 0, Chamfer);
  Result.CloseFigure;
end;

function WeighPoint(A, B: TPointF; Weight: Single): TPointF;
begin
  Result.X := A.X * Weight + B.X * (1 - Weight);
  Result.Y := A.Y * Weight + B.Y * (1 - Weight);
end;

function NewArrow(Size, Stem, Trunk: Single): IGdiGraphicsPath;
var
  A, B: TPointF;
begin
  Result := NewGraphicsPath;
  A := WeighPoint(NewPointF(Size, 0), NewPointF(0, Size), 1 - ((1 - Trunk) / 2));
  B := WeighPoint(NewPointF(Size, 0), NewPointF(0, Size), (1 - Trunk) / 2);
  Result.AddLine(0, 0, Size, 0);
  Result.AddLine(Size, 0, A.X, A.Y);
  Result.AddLine(A.X, A.Y, A.X + Stem, A.Y + Stem);
  Result.AddLine(A.X + Stem, A.Y + Stem, B.X + Stem, B.Y + Stem);
  Result.AddLine(B.X + Stem, B.Y + Stem, B.X, B.Y);
  Result.AddLine(B.X, B.Y, 0, Size);
  Result.CloseFigure;
end;

function NewCrossMark(const Rect: TRectF): IGdiGraphicsPath;
const
  A = 0.80;
  B = 0.20;
begin
  Result := NewGraphicsPath;
  with Rect do
  begin
    Result.AddLine(X, Y + Height * B, X + Width * B, Y);
    Result.AddLine(X + Width / 2, Y + Height / 2 - Height * B,
      X + Width / 2, Y + Height / 2 - Height * B);
    Result.AddLine(X + Width * A, Y, X + Width, Y + Height * B);
    Result.AddLine(X + Width / 2 + Width * B, Y + Height / 2,
      X + Width / 2 + Width * B, Y + Height / 2);
    Result.AddLine(X + Width, Y + Height * A, X + Width * A, Y + Height);
    Result.AddLine(X + Width / 2, Y + Height / 2 + Height * B,
      X + Width / 2, Y + Height / 2 + Height * B);
    Result.AddLine(X + Width * B, Y + Height, X, Y + Height * A);
    Result.AddLine(X + Width / 2 - Width * B, Y + Height / 2,
      X + Width / 2 - Width * B, Y + Height / 2);
  end;
  Result.CloseFigure;
end;

{ Drawing routines }

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

type
  TQuad = array[0..3] of Byte;

function Blend(ForeColor: TColor; BackColor: TColor; Percent: Byte = 50; Opacity: Byte = $FF): Cardinal; overload;
var
  RGB: TQuad absolute Result;
  F: TQuad;
  B: TQuad;
begin
  F := TQuad(ColorToRGB(ForeColor));
  B := TQuad(ColorToRGB(BackColor));
  RGB[0] := (F[2] * Percent div 100) + (B[2] * (100 - Percent) div 100);
  RGB[1] := (F[1] * Percent div 100) + (B[1] * (100 - Percent) div 100);
  RGB[2] := (F[0] * Percent div 100) + (B[0] * (100 - Percent) div 100);
  RGB[3] := Opacity;
end;

function Blend(Color: TColor; Opacity: Byte = $FF): Cardinal; overload;
var
  RGB: TQuad absolute Result;
  B: Byte;
begin
  RGB := TQuad(ColorToRGB(Color));
  B := RGB[0];
  RGB[0] := RGB[2];
  RGB[2] := B;
  RGB[3] := Opacity;
end;

procedure GdiDrawPath(G: IGdiGraphics; P: IGdiPen; B: IGdiBrush;
  S: IGdiGraphicsPath; DeltaX: Single = 0; DeltaY: Single = 0; DeltaAngle: Single = 0;
  ScaleX: Single = 1; ScaleY: Single = 1);
begin
  if (DeltaX <> 0) or (DeltaY <> 0) or (DeltaAngle <> 0) or
    (ScaleX <> 1) or (ScaleY <> 1) then
  begin
    GdiPushMatrix(G);
    if (DeltaX <> 0) or (DeltaY <> 0) then
      G.TranslateTransform(DeltaX, DeltaY);
    if DeltaAngle <> 0 then
      G.RotateTransform(DeltaAngle);
    if (ScaleX <> 1) or (ScaleY <> 1) then
      G.ScaleTransform(ScaleX, ScaleY);
  end;
  if B <> nil then
    G.FillPath(B, S);
  if P <> nil then
    G.DrawPath(P, S);
  if (DeltaX <> 0) or (DeltaY <> 0) or (DeltaAngle <> 0) or
    (ScaleX <> 1) or (ScaleY <> 1) then
    GdiPopMatrix(G);
end;

procedure GdiDrawRectState(G: IGdiGraphics; Rect: TRectF; State: TDrawState);
const
  PenWidth = 1;
var
  C: TArgb;
  P: IGdiPen;
  B: IGdiBrush;
  S: IGdiGraphicsPath;
begin
  if [dsHot, dsSelected] * State = [] then Exit;
  Rect.Width := Rect.Width - 1;
  Rect.Height := Rect.Height - 1;
  C := NewColor(clHighlight);
  P := nil;
  B := nil;
  if dsFocused in State then
    if dsSelected in State then
    begin
      P := NewPen(C, PenWidth);
      if dspRESSED in State then
        B := NewSolidBrush(NewOpacity(C, $70))
      else
        B := NewLinearGradientBrush(Rect, 270, NewOpacity(C, $70), NewOpacity(C, $20))
    end
    else
      P := NewPen(C, PenWidth)
  else if dsSelected in State then
  begin
    P := NewPen(NewOpacity(C, $D0), PenWidth);
    B := NewLinearGradientBrush(Rect, 270, NewOpacity(C, $40), NewOpacity(C, $10))
  end
  else
    P := NewPen(NewOpacity(C, $D0), PenWidth);
  S := NewRoundRect(Rect, 2);
  if B <> nil then
    G.FillPath(B, S);
  G.DrawPath(P, S);
end;

procedure GdiDrawStyleRoundRect(DC: HDC; Rect: TRect; Light: Boolean = False; Radius: Single = 7);
var
  Bitmap: TFastBitmap;
  G: IGdiGraphics;
  P: IGdiPen;
  B: IGdiBrush;
  R: IGdiGraphicsPath;
begin
  Bitmap := CreateFastBitmap(WidthOf(Rect), HeightOf(Rect), pd32);
  if Bitmap.DC = 0 then Exit;
  G := NewGraphics(Bitmap.DC);
  if Light then
  begin
    P := NewPen(Blend(clHighlight, clWindow, 66), 1);
    B := NewSolidBrush(Blend(clHighlight, clWindow, 20));
  end
  else
  begin
    P := NewPen(Blend(clHighlight), 1);
    B := NewSolidBrush(Blend(clHighlight, clWindow, 33));
  end;
  R := NewRoundRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1, Radius);
  G.FillPath(B, R);
  G.DrawPath(P, R);
  AlphaDraw(DC, Rect.Left, Rect.Top, Bitmap);
  DestroyFastBitmap(Bitmap);
end;

procedure GdiDrawBar(G: IGdiGraphics; Rect: TRectF; Back: TArgb);
var
  Fore: TArgb;
  B: IGdiLinearGradientBrush;
begin
  Fore := NewColor(clHighlight);
  Rect.Y := Round(Rect.Y + Rect.Height) - 2;
  Rect.Height := 2;
  B := NewLinearGradientBrush(Rect, 0,
    [Back, Fore, Fore, Back], [0, 0.3, 0.7, 1]);
  G.FillRectangle(B, Rect);
end;

{ Path routines }

procedure GdiAddLineToPath(P: IGdiGraphicsPath; const A, B: TVertex);
begin
  P.AddLine(A.X, A.Y, B.X, B.Y);
end;

procedure GdiLinePath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float; Capped: Boolean = False);
var
  W: Float;
  P1, P2, P3, P4: TVertex;
begin
  W := Width / 2;
  P1 := A;
  P2 := B;
  P3 := B;
  P4 := A;
  VertexOffset(W, P1, P2);
  VertexOffset(W, P3, P4);
  P.StartFigure;
  GdiAddLineToPath(P, P1, P2);
  GdiAddLineToPath(P, P2, P3);
  GdiAddLineToPath(P, P3, P4);
  GdiAddLineToPath(P, P4, P1);
  P.CloseFigure;
  if Capped then
  begin
    P1 := VertexAverage(P1, P4);
    P2 := VertexAverage(P2, P3);
    P.AddEllipse(P1.X - W, P1.Y - W, Width, Width);
    P.AddEllipse(P2.X - W, P2.Y - W, Width, Width);
  end;
  P.Outline;
end;

procedure GdiArrowPath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float);
var
  W: Float;
  V1, V2, P1, P2, P3, P4: TVertex;
begin
  W := Width / 2;
  V1 := A;
  V2 := B;
  VertexOffset(1, V1, V2);
  V2 := VertexNormalize(VertexSubtract(V1, A));
  V1 := VertexNormalize(VertexSubtract(B, A));
  P1 := VertexSubtract(A, VertexMultiply(V1, W));
  P2 := VertexSubtract(B, VertexMultiply(V1, W));
  P3 := P2;
  P4 := P1;
  VertexOffset(W, P1, P2);
  VertexOffset(W, P3, P4);
  P.StartFigure;
  GdiAddLineToPath(P, P1, P2);
  GdiAddLineToPath(P, P2, VertexAdd(P2, VertexMultiply(V2, W)));
  GdiAddLineToPath(P, VertexAdd(P2, VertexMultiply(V2, W)),
    VertexAdd(B, VertexMultiply(V1, Width)));
  GdiAddLineToPath(P, VertexAdd(B, VertexMultiply(V1, Width)),
    VertexAdd(P3, VertexMultiply(V2, -W)));
  GdiAddLineToPath(P, VertexAdd(P3, VertexMultiply(V2, -W)), P3);
  GdiAddLineToPath(P, P3, P4);
  GdiAddLineToPath(P, P4, P1);
  P.CloseFigure;
end;

procedure GdiArrowCirclePath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float);
begin
  GdiArrowPath(P, A, B, Width);
  P.AddEllipse(A.X - Width, A.Y - Width, Width * 2, Width * 2);
  P.Outline;
end;

procedure GdiFillAndStrokePath(G: IGdiGraphics; P: IGdiGraphicsPath;
  Fill, Stroke: TArgb; StrokeWidth: Integer = 1);
begin
  G.FillPath(NewSolidBrush(Fill), P);
  G.DrawPath(NewPen(Stroke, StrokeWidth), P);
end;

procedure GdiStrokePath(G: IGdiGraphics; P: IGdiGraphicsPath; Stroke: TArgb;
  StrokeWidth: Integer = 1);
begin
  G.DrawPath(NewPen(Stroke, StrokeWidth), P);
end;

{ Draw routines }

procedure GdiDrawButton(G: IGdiGraphics; const Rect: TRectF; State: TDrawState);
var
  R: IGdiGraphicsPath;
  B: IGdiBrush;
  P: IGdiPen;
  C1, C2: TArgb;
begin
  if [dsHot, dsSelected] * State = [] then Exit;
  R := NewRoundRect(Rect, 8);
  if dsHot in State then
    if dsPressed in State then
    begin
      C1 := aclBlack;
      SetOpacity(C1, $10);
      C2 := aclBlack;
      SetOpacity(C2, $20);
      B := NewSolidBrush(C2);
      SetOpacity(C2, $50);
      P := NewPen(C2, 1.5);
    end
    else
    begin
      C1 := aclWhite;
      SetOpacity(C1, $8F);
      C2 := aclWhite;
      SetOpacity(C2, $8F);
      B := NewSolidBrush(C2);
      P := NewPen(C2, 1.5);
    end
  else
  begin
    C1 := aclWhite;
    SetOpacity(C1, $30);
    C2 := aclWhite;
    SetOpacity(C2, $7F);
    B := NewLinearGradientBrush(Rect, 90, C1, C2);
    SetOpacity(C2, $90);
    P := NewPen(C2, 1.5);
  end;
  G.FillPath(B, R);
  G.DrawPath(P, R);
end;

procedure GdiDrawMenuRect(G: IGdiGraphics; const Rect: TRectF);
var
  S: IGdiGraphicsPath;
  B: IGdiBrush;
  P: IGdiPen;
begin
  S := NewRoundRect(Rect, 2);
  P := NewPen(NewColor(clMenuHighlight));
  B := NewLinearGradientBrush(Rect, 90, NewColor(clMenuHighlight, $20), NewColor(clMenuHighlight, $80));
  G.FillPath(B, S);
  G.DrawPath(P, S);
end;

{ Matrix routines }

var
  MatrixStack: IInterfaceList;

procedure GdiPushMatrix(G: IGdiGraphics);
begin
  if MatrixStack = nil then
    MatrixStack := TInterfaceList.Create;
  MatrixStack.Add(G.Transform);
end;

procedure GdiPopMatrix(G: IGdiGraphics);
var
  M: IGdiMatrix;
  I: Integer;
begin
  if (MatrixStack = nil) or (MatrixStack.Count = 0) then Exit;
  I := MatrixStack.Count - 1;
  M := MatrixStack[I] as IGdiMatrix;
  MatrixStack.Delete(I);
  G.SetTransform(M);
end;

{var
  DC: HDC;
  R: TRectF;
  G: IGdiGraphics;
  B: IGdiBrush;
  P: IGdiPen;
  S: IGdiGraphicsPath;
begin
  DC := Canvas.Handle;
  FillRectColor(DC, ClientRect, Color);
  R := NewRectI(10, 10, 13, 13);
  G := NewGraphics(DC);
  P := NewPen(GdiColorV(clRed, 0.7), 0.39);
  B := NewLinearGradientBrush(R, 70,
    [GdiColorV(clRed, 1.2), GdiColorV(clRed, 0.8), GdiColorV(clRed, 0.6), GdiColorV(clRed, 1)],
    [0, 0.4, 0.5, 1]);
  S := NewRoundRect(R, 2);
  GdiPathDraw(G, P, B, S);
  GdiInflateRect(R, -2.5, -2.5);
  P := NewPen(GdiColorV(clWhite, 0.8), 0.26);
  B := NewLinearGradientBrush(R, 90,
    [GdiColorV(clWhite, 1), GdiColorV(clWhite, 0.9), GdiColorV(clWhite, 0.7), GdiColorV(clWhite, 1)],
    [0, 0.4, 0.5, 1]);
  S := NewCrossMark(R);
  GdiPathDraw(G, P, B, S);
end;}

end.
