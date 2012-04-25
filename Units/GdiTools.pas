
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  Private Build Release December 2008                 *)
(*                                                      *)
(********************************************************)

unit GdiTools;

interface

{$I CODEBOT.INC}

uses
  Classes, Graphics, Windows, ActiveX, GdiPlus, GdiIntf, BaseTypes;

{ Extra object creation routines }

function NewRectangle(X, Y, Width, Height: Single): IGdiGraphicsPath;
function NewRoundRect(X, Y, Width, Height, Radius: Single): IGdiGraphicsPath; overload;
function NewRoundRect(const Rect: TGdiRectF; Radius: Single): IGdiGraphicsPath; overload;
function NewChamfer(const Rect: TGdiRectF; Chamfer: Single): IGdiGraphicsPath; overload;
function NewChamfer(Width, Height, Chamfer: Single): IGdiGraphicsPath; overload;
function NewArrow(Size, Stem, Trunk: Single): IGdiGraphicsPath;
function NewCrossMark(const Rect: TGdiRectF): IGdiGraphicsPath;

{ Graphic routines  }

procedure GdiClear(G: IGdiGraphics; const R: TGdiRectF);
procedure GdiFillRectColor(G: IGdiGraphics; const R: TGdiRectF; C: TGdiColor);
procedure GdiDrawLine(G: IGdiGraphics; P: IGdiPen; const A, B: TVertex);

{ Text routines }

function GdiMeasureText(G: IGdiGraphics; F: IGdiFont; const S: string): TVertex;
procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TGdiColor;
  const S: WideString; const Rect: TGdiRectF; Direction: TDirection;
  SingleLine: Boolean = True); overload;
procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TGdiColor;
  const S: WideString; const Point: TGdiPointF; Direction: TDirection); overload;
procedure GdiDrawTextAngle(G: IGdiGraphics; F: IGdiFont; Fill: TGdiColor;
  const S: WideString; const Point: TGdiPointF; Direction: TDirection;
  Angle: Single);

{ Draw routines }

procedure GdiDrawPath(G: IGdiGraphics; P: IGdiPen; B: IGdiBrush;
  S: IGdiGraphicsPath; DeltaX: Single = 0; DeltaY: Single = 0; DeltaAngle: Single = 0;
  ScaleX: Single = 1; ScaleY: Single = 1);

procedure GdiDrawStyleRoundRect(DC: HDC; Rect: TRect; Light: Boolean = False;
  Radius: Single = 7);

procedure GdiDrawBar(G: IGdiGraphics; Rect: TGdiRectF; Back: TGdiColor);

{ Path routines }

procedure GdiAddLineToPath(P: IGdiGraphicsPath; const A, B: TVertex);
procedure GdiLinePath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float; Capped: Boolean = False);
procedure GdiArrowPath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float);
procedure GdiArrowCirclePath(P: IGdiGraphicsPath; const A, B: TVertex; Width: Float);
procedure GdiFillAndStrokePath(G: IGdiGraphics; P: IGdiGraphicsPath;
  Fill, Stroke: TGdiColor; StrokeWidth: Integer = 1);
procedure GdiStrokePath(G: IGdiGraphics; P: IGdiGraphicsPath; Stroke: TGdiColor;
  StrokeWidth: Integer = 1);

{ Matrix stack }

procedure GdiPushMatrix(G: IGdiGraphics);
procedure GdiPopMatrix(G: IGdiGraphics);

implementation

function GdiColor(R, G, B: Byte; A: Byte = $FF): TGdiColor;
begin
  Result := TGdiColor(RGBA(R, G, B, A));
end;

function Convert(Color: Longint): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF)
  else
    Result := Color;
end;

function GdiColor(Color: Longint; A: Byte = $FF): TGdiColor;
var
  C: TRGBA absolute Result;
begin
  C := TRGBA(Convert(Color));
	C.Alpha := C.Blue;
  C.Blue := C.Red;
  C.Red := C.Alpha;
  C.Alpha := A;
end;

function GdiColorIntensity(Color: Longint; Intensity: Single; A: Byte = $FF): TGdiColor; overload;
var
  C: TRGBA absolute Result;
begin
  C := TRGBA(Convert(Color));
	C.Alpha := C.Blue;
  C.Blue := C.Red;
  C.Red := C.Alpha;
  C.Alpha := A;
  C.Red := Round(C.Red * Intensity);
  C.Green := Round(C.Green * Intensity);
  C.Blue := Round(C.Blue * Intensity);
end;

function GdiColorV(Color: Longint; V: Single; A: Byte = $FF): TGdiColor; overload;
var
  B: Byte;
  C: TRGBA absolute Result;

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
  C := TRGBA(Convert(Color));
	C.Alpha := C.Blue;
  C.Blue := C.Red;
  C.Red := C.Alpha;
  C.Alpha := A;
  C.Red := Clamp(C.Red);
  C.Green := Clamp(C.Green);
  C.Blue := Clamp(C.Blue);
end;

procedure GdiSetOpacity(var C: TGdiColor; Opactiy: Byte);
begin
  C := (C and $FFFFFF) or Opactiy shl 24;
end;

function GdiGetOpacity(C: TGdiColor): Byte;
begin
  Result := C shr 24;
end;

function GdiPoint(X, Y: Single): TGdiPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;

function GdiPoint(V: TVertex): TGdiPointF;
begin
  Result.X := V.X;
  Result.Y := V.Y;
end;

function GdiRect(const Rect: TRect): TGdiRectF;
begin
  Result.X := Rect.Left;
  Result.Y := Rect.Top;
  Result.Width := Rect.Right - Rect.Left;
  Result.Height := Rect.Bottom - Rect.Top;
end;

function GdiRect(X, Y, W, H: Single): TGdiRectF;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := W;
  Result.Height := H;
end;

procedure GdiInflateRect(var Rect: TGdiRectF; X, Y: Single);
begin
  Rect.X := Rect.X - X;
  Rect.Y := Rect.Y - Y;
  Rect.Width := Rect.Width + X * 2;
  Rect.Height := Rect.Height + Y * 2;
end;

function GdiOffsetRect(var Rect: TGdiRectF; X, Y: Single): TGdiRectF;
begin
  Rect.X := Rect.X + X;
  Rect.Y := Rect.Y + Y;
end;

procedure GdiClear(G: IGdiGraphics; const R: TGdiRectF);
var
  B: IGdiBrush;
begin
  B := NewCheckerBrush(GdiColor(clWhite), GdiColor(clSilver), 10);
  G.FillRectangle(B, R);
end;

procedure GdiFillRectColor(G: IGdiGraphics; const R: TGdiRectF; C: TGdiColor);
begin
  G.FillRectangle(NewSolidBrush(C), R);
end;

procedure GdiDrawLine(G: IGdiGraphics; P: IGdiPen; const A, B: TVertex);
begin
  G.DrawLine(P, A.X, A.Y, B.X, B.Y);
end;

{ Text routines }

function GdiMeasureText(G: IGdiGraphics; F: IGdiFont; const S: string): TVertex;
var
  Rect: TGdiRectF;
begin
  Rect := GdiRect(0, 0, High(Word), High(Word));
  G.MeasureString(S, F, Rect, Rect);
  Result.X := Rect.Width;
  Result.Y := Rect.Height;
end;

procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TGdiColor;
  const S: WideString; const Rect: TGdiRectF; Direction: TDirection;
  SingleLine: Boolean = True); overload;
begin
  G.DrawString(S, F, Rect, NewStringFormat(Direction, SingleLine),
    NewSolidBrush(Fill));
end;

procedure GdiDrawText(G: IGdiGraphics; F: IGdiFont; Fill: TGdiColor;
  const S: WideString; const Point: TGdiPointF; Direction: TDirection); overload;
begin
  G.DrawString(S, F, Point, NewStringFormat(Direction), NewSolidBrush(Fill));
end;

procedure GdiDrawTextAngle(G: IGdiGraphics; F: IGdiFont; Fill: TGdiColor;
  const S: WideString; const Point: TGdiPointF; Direction: TDirection;
  Angle: Single); overload;
begin
  GdiPushMatrix(G);
  try
    G.TranslateTransform(Point.X, Point.Y);
    G.RotateTransform(-Angle);
    G.TranslateTransform(-Point.X, -Point.Y);
    G.DrawString(S, F, Point, NewStringFormat(Direction), NewSolidBrush(Fill));
  finally
    GdiPopMatrix(G);
  end;
end;

function NewGraphics(DC: HDC): IGdiGraphics;
begin
  Result := TGdiGraphics.Create(DC);
  Result.SetSmoothingMode(SmoothingModeAntiAlias);
end;

function NewPen(RGBA: TGdiColor; StrokeWidth: Single = 1): IGdiPen;
begin
  Result := TGdiPen.Create(TGdiColor(RGBA), StrokeWidth);
end;

function NewSolidBrush(RGBA: TGdiColor): IGdiSolidBrush;
begin
  Result := TGdiSolidBrush.Create(TGdiColor(RGBA));
end;

function NewCheckerBrush(C1, C2: TGdiColor; Size: Integer): IGdiTextureBrush;
var
  G: IGdiGraphics;
  B: IGdiBitmap;
  F: IGdiSolidBrush;
  R: TGdiRectF;
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
  Result := TGdiTextureBrush.Create(B);
end;

function NewHatchBrush(Style: THatchStyle; ForeColor: TGdiColor): IGdiHatchBrush;
begin
  Result := TGdiHatchBrush.Create(Style, ForeColor, 0);
end;

function NewHatchBrush(Style: THatchStyle; ForeColor: TGdiColor; BackColor: TGdiColor): IGdiHatchBrush;
begin
  Result := TGdiHatchBrush.Create(Style, ForeColor, BackColor);
end;

function NewLinearGradientBrush(const Rect: TGdiRectF; Angle: Single;
  C1, C2: TGdiColor): IGdiLinearGradientBrush;
begin
  Result := TGdiLinearGradientBrush.Create(Rect, C1, C2, Angle);
end;

function NewLinearGradientBrush(const Rect: TGdiRectF; Angle: Single;
  Colors: array of TGdiColor; Stops: array of Single): IGdiLinearGradientBrush; overload;
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

function NewFont(const Name: string; Size: Integer; Style: TFontStyles): IGdiFont;
const
  BoolBytes: array[Boolean] of Byte = (0, 1);
var
  LogFont: TLogFont;
  DC: HDC;
  S: string;
begin
  FillChar(LogFont, SizeOf(TLogFont), #0);
  DC := GetDC(0);
  try
    LogFont.lfHeight := -MulDiv(Size, GetDeviceCaps(DC, LOGPIXELSY), 72);
    LogFont.lfItalic := BoolBytes[fsItalic in Style];
    LogFont.lfUnderline := BoolBytes[fsUnderline  in Style];
    LogFont.lfStrikeOut := BoolBytes[fsStrikeOut  in Style];
    if fsBold in Style then
      LogFont.lfWeight := 600
    else
      LogFont.lfWeight := 300;
    LogFont.lfQuality := CLEARTYPE_QUALITY or PROOF_QUALITY;
    LogFont.lfOutPrecision := OUT_DEVICE_PRECIS;
    LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    S := Name;
    if Length(S) > 31 then
      SetLength(S, 31);
    Move(PChar(S)^, LogFont.lfFaceName, Length(S));
    Result := TGdiFont.Create(DC, PLogFont(@LogFont));
  finally
    ReleaseDC(0, DC);
  end;
end;

function NewStringFormat: IGdiStringFormat;
begin
  Result := TGdiStringFormat.Create;
end;

function NewStringFormat(Direction: TDirection; SingleLine: Boolean = False): IGdiStringFormat; overload;
var
  Format: Integer;
  H, V: TStringAlignment;
begin
  case Direction of
    drLeft:
      begin
        H := StringAlignmentNear;
        V := StringAlignmentCenter;
      end;
    drUp:
      begin
        H := StringAlignmentCenter;
        V := StringAlignmentNear;
      end;
    drRight:
      begin
        H := StringAlignmentFar;
        V := StringAlignmentCenter;
      end;
    drDown:
      begin
        H := StringAlignmentCenter;
        V := StringAlignmentFar;
      end;
    drCenter:
      begin
        H := StringAlignmentCenter;
        V := StringAlignmentCenter;
      end;
    drFill:
      begin
        H := StringAlignmentNear;
        V := StringAlignmentNear;
      end;
    drWrap:
      begin
        H := StringAlignmentNear;
        V := StringAlignmentCenter;
      end;
  else
    H := StringAlignmentNear;
    V := StringAlignmentNear;
  end;
  if SingleLine then
    Format := StringFormatFlagsNoWrap
  else
    Format := 0;
  Result := TGdiStringFormat.Create(Format);
  Result.SetLineAlignment(V);
  Result.SetAlignment(H);
  if not SingleLine then
    Result.SetTrimming(StringTrimmingEllipsisCharacter);
end;

function NewBitmap(Width, Height: Integer): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(Width, Height);
end;

function NewBitmap(const FileName: string): IGdiBitmap;
begin
  Result := TGdiBitmap.Create(FileName);
end;

function NewBitmap(ResID: Integer): IGdiBitmap;
var
	Stream: IStream;
begin
  Stream := TStreamAdapter.Create(TResourceStream.CreateFromID(HInstance,
    ResID, RT_RCDATA), soOwned);
  Result := TGdiBitmap.Create(Stream);
end;

function NewPath: IGdiGraphicsPath;
begin
  Result := TGdiGraphicsPath.Create;
end;

function NewRectangle(X, Y, Width, Height: Single): IGdiGraphicsPath;
begin
  Result := TGdiGraphicsPath.Create;
  Result.AddRectangle(GdiRect(X, Y, Width, Height));
end;

function NewRoundRect(X, Y, Width, Height, Radius: Single): IGdiGraphicsPath;
begin
  Result := TGdiGraphicsPath.Create;
  Result.AddLine(X + Radius, Y, X + Width - (Radius * 2), Y);
  Result.AddArc(X + Width - (Radius * 2), Y, Radius * 2, Radius * 2, 270, 90);
  Result.AddLine(X + Width, Y + Radius, X + Width, Y + Height - (Radius * 2));
  Result.AddArc(X + Width - (Radius * 2), Y + Height - (Radius * 2), Radius * 2, Radius * 2, 0, 90);
  Result.AddLine(X + Width - (Radius * 2), Y + Height, X + Radius, Y + Height);
  Result.AddArc(X, Y + Height - (Radius * 2), Radius * 2, Radius * 2, 90, 90);
  Result.AddLine(X, Y + Height - (Radius * 2), X, Y + Radius);
  Result.AddArc(X, Y, Radius * 2, Radius * 2, 180, 90);
  Result.CloseFigure;
end;

function NewRoundRect(const Rect: TGdiRectF; Radius: Single): IGdiGraphicsPath;
begin
  Result := NewRoundRect(Rect.X, Rect.Y, Rect.Width, Rect.Height, Radius);
end;

function NewChamfer(const Rect: TGdiRectF; Chamfer: Single): IGdiGraphicsPath;
begin
  Result := TGdiGraphicsPath.Create;
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
  Result := TGdiGraphicsPath.Create;
  Result.AddLine(Chamfer, 0, Width - Chamfer, 0);
  Result.AddLine(Width - Chamfer, 0, Width, Chamfer);
  Result.AddLine(Width, Chamfer, Width, Height - Chamfer);
  Result.AddLine(Width, Height - Chamfer, Width - Chamfer, Height);
  Result.AddLine(Width - Chamfer, Height, Chamfer, Height);
  Result.AddLine(Chamfer, Height, 0, Height - Chamfer);
  Result.AddLine(0, Height - Chamfer, 0, Chamfer);
  Result.CloseFigure;
end;

function WeighPoint(A, B: TGdiPointF; Weight: Single): TGdiPointF;
begin
  Result.X := A.X * Weight + B.X * (1 - Weight);
  Result.Y := A.Y * Weight + B.Y * (1 - Weight);
end;

function NewArrow(Size, Stem, Trunk: Single): IGdiGraphicsPath;
var
  A, B: TGdiPointF;
begin
  Result := TGdiGraphicsPath.Create;
  A := WeighPoint(GdiPoint(Size, 0), GdiPoint(0, Size), 1 - ((1 - Trunk) / 2));
  B := WeighPoint(GdiPoint(Size, 0), GdiPoint(0, Size), (1 - Trunk) / 2);
  Result.AddLine(0, 0, Size, 0);
  Result.AddLine(Size, 0, A.X, A.Y);
  Result.AddLine(A.X, A.Y, A.X + Stem, A.Y + Stem);
  Result.AddLine(A.X + Stem, A.Y + Stem, B.X + Stem, B.Y + Stem);
  Result.AddLine(B.X + Stem, B.Y + Stem, B.X, B.Y);
  Result.AddLine(B.X, B.Y, 0, Size);
  Result.CloseFigure;
end;

function NewCrossMark(const Rect: TGdiRectF): IGdiGraphicsPath;
const
  A = 0.80;
  B = 0.20;
begin
  Result := TGdiGraphicsPath.Create;
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

procedure GdiDrawBar(G: IGdiGraphics; Rect: TGdiRectF; Back: TGdiColor);
var
  Fore: TGdiColor;
  B: IGdiLinearGradientBrush;
begin
  Fore := GdiColor(clHighlight);
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
  Fill, Stroke: TGdiColor; StrokeWidth: Integer = 1);
begin
  G.FillPath(NewSolidBrush(Fill), P);
  G.DrawPath(NewPen(Stroke, StrokeWidth), P);
end;

procedure GdiStrokePath(G: IGdiGraphics; P: IGdiGraphicsPath; Stroke: TGdiColor;
  StrokeWidth: Integer = 1);
begin
  G.DrawPath(NewPen(Stroke, StrokeWidth), P);
end;

{ Matrix routines }

function NewMatrix: IGdiMatrix;
begin
  Result := TGdiMatrix.Create;
end;

var
  MatrixStack: IInterfaceList;

procedure GdiPushMatrix(G: IGdiGraphics);
var
  M: IGdiMatrix;
begin
  if MatrixStack = nil then
    MatrixStack := TInterfaceList.Create;
  M := NewMatrix;
  G.GetTransform(M);
  MatrixStack.Add(M);
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
  R: TGdiRectF;
  G: IGdiGraphics;
  B: IGdiBrush;
  P: IGdiPen;
  S: IGdiGraphicsPath;
begin
  DC := Canvas.Handle;
  FillRectColor(DC, ClientRect, Color);
  R := GdiRect(10, 10, 13, 13);
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
