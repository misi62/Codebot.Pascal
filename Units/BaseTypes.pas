
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit BaseTypes;

interface

{$I CODEBOT.INC}

uses
  Windows;

{$IFNDEF D12_UP}
type
  TSysCharSet = set of Char;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
{$ENDIF}

type
  TDrawStateItem = (dsDisabled, dsPressed, dsSelected, dsHot, dsFocused, dsChecked,
    dsExpanded, dsDefaulted, dsThin, dsFlat, dsBackground, dsCustom);
  TDrawState = set of TDrawStateItem;

function OwnerToDrawState(State: TOwnerDrawState): TDrawState;

{ Font support }

const
  CLEARTYPE_QUALITY = 5;

{ Geometric types }

function IsXpOrLater: Boolean;
function IsVistaOrLater: Boolean;

type
{$IFDEF PRECISE}
  Float = Double;
{$ELSE}
  Float = Single;
{$ENDIF}

  TIntegerArray = array of Integer;
  TFloatArray = array of Float;

  TRGB = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
  end;
  PRGB = ^TRGB;

  TRGBA = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Alpha: Byte;
  end;
  PRGBA = ^TRGBA;

  THSL = record
    Hue: Single;
    Saturation: Single;
    Lightness: Single;
  end;
  PHSL = ^THSL;

  TRGBAFloat = record
    Blue: Single;
    Green: Single;
    Red: Single;
    Alpha: Single;
  end;
  PRGBAFloat = TRGBAFloat;

type
  TVec2 = record
    {$IFDEF D15_UP}
    class operator Add(const A, B: TVec2): TVec2; inline;
    class operator Subtract(const A, B: TVec2): TVec2; inline;
    class operator Multiply(const A, B: TVec2): TVec2; inline;
    class operator Multiply(const A: TVec2; B: Float): TVec2; inline;
    class operator Divide(const A, B: TVec2): TVec2; inline;
    class operator Divide(const A: TVec2; B: Float): TVec2; inline;
    {$ENDIF}
    case Integer of
    0: (X, Y: Float);
    1: (S, T: Float);
    2: (V: array[0..1] of Float);
  end;
  PVec2 = ^TVec2;
  TVec2Array = array of TVec2;

  TVec3 = record
    {$IFDEF D15_UP}
    function Cross(const V: TVec3): TVec3; inline;
    function Dot(const V: TVec3): Float; inline;
    procedure Normalize; inline;
    class operator Add(const A, B: TVec3): TVec3; inline;
    class operator Subtract(const A, B: TVec3): TVec3; inline;
    class operator Multiply(const A, B: TVec3): TVec3; inline;
    class operator Multiply(const A: TVec3; B: Float): TVec3; inline;
    class operator Divide(const A, B: TVec3): TVec3; inline;
    class operator Divide(const A: TVec3; B: Float): TVec3; inline;
    {$ENDIF}
    case Integer of
    0: (X, Y, Z: Float);
    1: (R, G, B: Float);
    2: (Red, Green, Blue: Float);
    3: (Heading, Pitch, Roll: Float);
    4: (Hue, Saturation, Lightness: Float);
    5: (V: array[0..2] of Float);
  end;
  PVec3 = ^TVec3;
  TVec3Array = array of TVec3;

  TVec4 = record
    {$IFDEF D15_UP}
    class operator Add(const A, B: TVec4): TVec4; inline;
    class operator Subtract(const A, B: TVec4): TVec4; inline;
    class operator Multiply(const A, B: TVec4): TVec4; inline;
    class operator Multiply(const A: TVec4; B: Float): TVec4; inline;
    class operator Divide(const A, B: TVec4): TVec4; inline;
    class operator Divide(const A: TVec4; B: Float): TVec4; inline;
    {$ENDIF}
    case Integer of
    0: (X, Y, Z, W: Float);
    1: (R, G, B, A: Float);
    2: (Red, Green, Blue, Alpha: Float);
    3: (XY: TVec2);
    4: (XYZ: TVec3);
    6: (V: array[0..3] of Float);
  end;
  PVec4 = ^TVec4;
  TVec4Array = array of TVec4;

function Vec(const X, Y: Float): TVec2; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec(const X, Y, Z: Float): TVec3; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec(const X, Y, Z, W: Float): TVec4; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec2(X: Float): TVec2; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec2(const X, Y: Float): TVec2; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec2(const V: TVec3): TVec2; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec2(const V: TVec4): TVec2; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec3(X: Float): TVec3; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec3(const X, Y, Z: Float): TVec3; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec3(const V: TVec4): TVec3; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec4(X: Float): TVec4; overload; {$IFDEF D12_UP}inline;{$ENDIF}
function Vec4(const X, Y, Z, W: Float): TVec4; overload; {$IFDEF D12_UP}inline;{$ENDIF}

type
  TMatrix = array[0..3, 0..3] of Float;
  PMatrix = ^TMatrix;

type
  TPoints = array of TPoint;
  TPolygon = type TPoints;
  TRectangle = array[0..3] of TPoint;
  TRects = array of TRect;

  TVertex = TVec2;
  PVertex = PVec2;

  TVertexLine = record
    A, B: TVertex;
  end;
  PVertexLine = ^TVertexLine;

  TVertexRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Float);
      1: (TopLeft, BottomRight: TVertex);
  end;
  PVertexRect = ^TVertexRect;

  TColorVertex = record
    Color: TRGBAFloat;
    Vertex: TVertex;
    Weight: Single;
  end;
  PColorVertex = ^TColorVertex;

  TColorVertices = array of TColorVertex;

  TVector = TVertex;
  PVector = PVertex;

  TVertices = array of TVertex;
  PVertices = ^TVertices;

  TVertexLines = array of TVertexLine;
  PVertexLines = ^TVertexLines;

  TVertexIndex = array of Integer;
  PVertexIndex = ^TVertexIndex;

  TVectors = TVertices;

  TFloatPoint = TVec2;
  TFloatPoints = TVec2Array;
  TFloatPolygon = type TVec2Array;

  TFloatRect = TVertexRect;
  PFloatRect = PVertexRect;

  TFloatRectangle = array[0..3] of TFloatPoint;

  TDirection = (drLeft, drUp, drRight, drDown, drCenter, drFill, drWrap, drFlow);
  TDirections = set of TDirection;

const
  IdentityMatrix: TMatrix = (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));

function VertexRect(Left, Top, Right, Bottom: Float): TVertexRect;
function Remainder(const Quotient, Divisor: Float): Float;
function Equals(const A, B: TVertex): Boolean;

function RGBA(R, G, B, A: Byte): TRGBA;
function RGBAComplement(C: TRGBA): TRGBA;
function ColorToRGBA(Color: Longint): TRGBA;
function RGBAFloat(R, G, B, A: Single): TRGBAFloat;
function HSL(H, S, L: Single): THSL;

function HSLtoRGBA(const HSL: THSL): TRGBA;
function RGBAtoHSL(RGBA: TRGBA): THSL;
function RGBAFloatToHSL(RGBA: TRGBAFloat): THSL;

function Vertex(const X, Y: Float): TVertex;
function VertexAdd(const A, B: TVertex): TVertex;
function VertexSubtract(const A, B: TVertex): TVertex;
function VertexMultiply(const A: TVertex; Factor: Float): TVertex;

function VertexAngle(const V: TVertex): Float;
function VertexRotate(const V: TVertex; const Angle: Float): TVertex;
function VertexDistance(const A, B: TVertex): Float;

function VertexAverage(const A, B: TVertex): TVertex;
function VertexNormalize(const V: TVertex): TVertex;
procedure VertexOffset(Distance: Float; var A, B: TVertex);

procedure VerticesAdd(var List: TVertices; const X, Y: Float); overload;
procedure VerticesAdd(var List: TVertices; const V: TVertex); overload;

function MatrixAdd(const A, B: TMatrix): TMatrix;
function MatrixSubtract(const A, B: TMatrix): TMatrix;
function MatrixMultiply(const A, B: TMatrix): TMatrix;
function MatrixDivide(const A, B: TMatrix): TMatrix;
function MatrixScale(const M: TMatrix; X, Y: Float): TMatrix;
function MatrixTranslate(const M: TMatrix; X, Y: Float): TMatrix;
function MatrixRotate(const M: TMatrix; const Delta: Float): TMatrix;
function MatrixTransform(const M: TMatrix; const V: TVertex): TVertex;

function LineTo(DC: HDC; X, Y: Integer): BOOL; overload;
function LineTo(DC: HDC; const X, Y: Float): BOOL; overload;
function LineTo(DC: HDC; const V: TVertex): BOOL; overload;
function LineTo(DC: HDC; const V, Pivot, Scale: TVertex): BOOL; overload;
function LineTo(DC: HDC; const V: TVertex; const Matrix: TMatrix): BOOL; overload;

function MoveToEx(DC: HDC; X, Y: Integer; P: PPoint = nil): BOOL; overload;
function MoveToEx(DC: HDC; const X, Y: Float): BOOL; overload;
function MoveTo(DC: HDC; const X, Y: Float): BOOL; overload;
function MoveTo(DC: HDC; const V: TVertex): BOOL; overload;
function MoveTo(DC: HDC; const V, Pivot, Scale: TVertex): BOOL; overload;
function MoveTo(DC: HDC; const V: TVertex; const Matrix: TMatrix): BOOL; overload;

function Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): BOOL; overload;
function Ellipse(DC: HDC; const X, Y, Radius: Float): BOOL; overload;
function Ellipse(DC: HDC; const Center: TVertex; const Radius: Float): BOOL; overload;
function Ellipse(DC: HDC; const Pivot, Scale, Center: TVertex; const Radius: Float): BOOL; overload;
function Ellipse(DC: HDC; const Center: TVertex; const Radius: Float; const Matrix: TMatrix): BOOL; overload;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): BOOL; overload;
function Rectangle(DC: HDC; const Rect: TVertexRect; const Matrix: TMatrix): BOOL; overload;

function Pie(DC: HDC; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): BOOL; overload;
function Pie(DC: HDC; const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Float): BOOL; overload;
function Pie(DC: HDC; const Center: TVertex; const Radius: Float; const V1, V2: TVertex): BOOL; overload;

type
  TVertexList = class(TObject)
  private
    FList: TVertices;
    FCount: Integer;
    FWorld: PMatrix;
    procedure CheckGrow;
    function Get(Index: Integer): TVertex;
    procedure Put(Index: Integer; const Value: TVertex);
    function GetX(Index: Integer): Float;
    procedure SetX(Index: Integer; const Value: Float);
    function GetY(Index: Integer): Float;
    procedure SetY(Index: Integer; const Value: Float);
  public
    procedure Draw(DC: HDC); virtual;
    procedure Add(const X, Y: Float); overload;
    procedure Add(const V: TVertex); overload;
    procedure Remove(Index: Integer);
    procedure Clear;
    procedure Copy(Source: TVertexList);
    procedure Reference(var List: TVertices);
    property Count: Integer read FCount;
    property Vert[Index: Integer]: TVertex read Get write Put; default;
    property X[Index: Integer]: Float read GetX write SetX;
    property Y[Index: Integer]: Float read GetY write SetY;
    property World: PMatrix read FWorld write FWorld;
  end;

{ Colors and FastBitmap }

function HeightOf(const Rect: TRect): Integer;
function WidthOf(const Rect: TRect): Integer;

type
  TPixelDepth = (pd24, pd32);

  TFastBitmap = record
    DC: HDC;
    Handle: HBITMAP;
    OldBitmap: HBITMAP;
    Bits: Pointer;
    Width: Integer;
    Height: Integer;
    Depth: TPixelDepth;
  end;
  PFastBitmap = ^TFastBitmap;
  TFastBitmaps = array of TFastBitmap;

function CreateFastBitmap(Width, Height: Integer; Depth: TPixelDepth = pd24): TFastBitmap; overload;
function CreateFastBitmap(const Rect: TRect; Depth: TPixelDepth = pd24): TFastBitmap; overload;
procedure DestroyFastBitmap(var Bitmap: TFastBitmap);
function IsFastBitmap(const Bitmap: TFastBitmap): Boolean;

function CreatePixel(RGBA: TRGBA): TFastBitmap;

function ScanlineStride(const Bitmap: TFastBitmap): Integer;
procedure FillColor(const Bitmap: TFastBitmap; RGBA: TRGBA);

type
  TDrawProc = procedure(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap);

procedure BlitDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap);
procedure BlitDrawBits(DC: HDC; X, Y: Integer; Bits: Pointer;
  Width, Height: Integer; Depth: TPixelDepth = pd24);  overload;
procedure BlitDrawBits(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap);  overload;
procedure AlphaDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap; Opacity: Byte = $FF); overload;
procedure AlphaDraw(DC: HDC; const Rect: TRect; const Bitmap: TFastBitmap; Opacity: Byte = $FF); overload;

procedure AlphaDrawRect(DC: HDC; const Source: TRect; const Bitmap: TFastBitmap; const Dest: TRect; Opacity: Byte = $FF);
procedure AlphaFill(Bitmap: TFastBitmap; Alpha: Byte = $FF);
procedure AlphaRect(Bitmap: TFastBitmap; Rect: TRect; Alpha: Byte = $FF);

const
   DefTerminator = '|';

type
  TStringArray = array of string;
  TIntegers = array of Integer;

{ Basic string splitting routines }

function Split(const Source: string; Terminator: Char): TStringArray;
function FieldCount(const Source: string; Terminator: Char): Integer;
function FieldValue(const Source: string; Terminator: Char; Index: Integer): string; overload;
function FieldValue(const Source: string; Index: Integer): string; overload;
function FieldValueInt(const Source: string; Terminator: Char; Index: Integer): string; overload;
function FieldValueInt(const Source: string; Index: Integer): string; overload;
function IntFieldValue(const Source: string; Index: Integer): Integer; overload;
function FieldSearch(const Key, Source: string; Terminator: Char): Boolean;

procedure DebugLog(const S: string);

{$IFDEF D5}
const
  AC_SRC_OVER = $00;
  AC_SRC_ALPHA = $01;
  AC_SRC_NO_PREMULT_ALPHA = $01;
{$ENDIF}

implementation

{$IFNDEF D12_UP}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

function OwnerToDrawState(State: TOwnerDrawState): TDrawState;
begin
  Result := [];
  if odSelected in State then
    Include(Result, dsSelected);
  if odDisabled in State then
    Include(Result, dsDisabled);
  if odFocused in State then
    Include(Result, dsFocused);
  if odHotLight in State then
    Include(Result, dsHot);
end;

function IsXpOrLater: Boolean;
begin
  Result := GetVersion and $FF >= 5;
end;

function IsVistaOrLater: Boolean;
begin
  Result := GetVersion and $FF >= 6;
end;

function Floor(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;

function Min(const A, B: Single): Single; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Double): Double; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: Extended): Extended; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Single): Single; overload;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Double): Double; overload;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Extended): Extended; overload;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

{ Vec }

function Vec(const X, Y: Float): TVec2;
begin
  Result.X := X; Result.Y := Y;
end;

function Vec(const X, Y, Z: Float): TVec3;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function Vec(const X, Y, Z, W: Float): TVec4;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z; Result.W := W;
end;

function Vec2(X: Float): TVec2;
begin
  Result.X := X; Result.Y := X;
end;

function Vec2(const X, Y: Float): TVec2;
begin
  Result.X := X; Result.Y := Y;
end;

function Vec2(const V: TVec3): TVec2;
begin
  Result.X := V.X; Result.Y := V.Y;
end;

function Vec2(const V: TVec4): TVec2;
begin
  Result.X := V.X; Result.Y := V.Y;
end;

function Vec3(X: Float): TVec3;
begin
  Result.X := X; Result.Y := X; Result.Z := X;
end;

function Vec3(const X, Y, Z: Float): TVec3;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function Vec3(const V: TVec4): TVec3;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := V.Z;
end;

function Vec4(X: Float): TVec4;
begin
  Result.X := X; Result.Y := X; Result.Z := X; Result.W := X;
end;

function Vec4(const X, Y, Z, W: Float): TVec4;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z; Result.W := W;
end;

function VertexRect(Left, Top, Right, Bottom: Float): TVertexRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function Remainder(const Quotient, Divisor: Float): Float;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Quotient - Trunc(Quotient / Divisor) * Divisor;
end;

function Compare(P1, P2: Pointer; Length: Integer): Boolean;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SAR     ECX,2
        JS      @@1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

function Equals(const A, B: TVertex): Boolean;
begin
  Result := Compare(@A, @B, SizeOf(A));
end;

function RGBA(R, G, B, A: Byte): TRGBA;
begin
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;
  Result.Alpha := A;
end;

function RGBAComplement(C: TRGBA): TRGBA;
begin
  Result := RGBA($FF - C.Red, $FF - C.Green, $FF - C.Blue, $FF);
end;

function ColorToRGB(Color: Longint): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF)
  else
    Result := Color;
end;

function ColorToRGBA(Color: Longint): TRGBA;
var
  B: Byte;
begin
  Result := TRGBA(ColorToRGB(Color));
  B := Result.Blue;
  Result.Blue := Result.Red;
  Result.Red := B;
end;

function RGBAFloat(R, G, B, A: Single): TRGBAFloat;
begin
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;
  Result.Alpha := A;
end;

function HSL(H, S, L: Single): THSL;
begin
  Result.Hue := H;
  Result.Saturation := S;
  Result.Lightness := L;
end;

function HSLtoRGBA(const HSL: THSL): TRGBA;
const
  OneOverThree = 1 / 3;
var
  M1, M2: Single;
  R, G, B: Byte;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else V := M1;
    Result := Round(255 * V);
  end;

begin
  if HSL.Saturation = 0 then
  begin
    R := Round(255 * HSL.Lightness);
    G := R;
    B := R;
  end
  else
  begin
    if HSL.Lightness <= 0.5 then M2 := HSL.Lightness * (1 + HSL.Saturation)
    else M2 := HSL.Lightness + HSL.Saturation - HSL.Lightness * HSL.Saturation;
    M1 := 2 * HSL.Lightness - M2;
    R := HueToColor(HSL.Hue + OneOverThree);
    G := HueToColor(HSL.Hue);
    B := HueToColor(HSL.Hue - OneOverThree)
  end;
  Result.Red := R;
  Result.Green := G;
  Result.Blue := B;
  Result.Alpha := $FF;
end;

function RGBAtoHSL(RGBA: TRGBA): THSL;
var
  R, G, B, D, CMax, CMin: Single;
begin
  R := RGBA.Red / 255;
  G := RGBA.Green / 255;
  B := RGBA.Blue / 255;
  CMax := Max(R, Max(G, B));
  CMin := Min(R, Min(G, B));
  Result.Lightness := (CMax + CMin) / 2;
  if CMax = CMin then
  begin
    Result.Hue := 0;
    Result.Saturation := 0
  end
  else
  begin
    D := CMax - CMin;
    if Result.Lightness < 0.5 then Result.Saturation := D / (CMax + CMin)
    else Result.Saturation := D / (2 - CMax - CMin);
    if R = CMax then Result.Hue := (G - B) / D
    else
      if G = CMax then Result.Hue  := 2 + (B - R) / D
      else Result.Hue := 4 + (R - G) / D;
    Result.Hue := Result.Hue / 6;
    if Result.Hue < 0 then Result.Hue := Result.Hue + 1
  end;
end;

function RGBAFloatToHSL(RGBA: TRGBAFloat): THSL;
var
  R, G, B, D, CMax, CMin: Single;
begin
  R := RGBA.Red;
  G := RGBA.Green;
  B := RGBA.Blue;
  CMax := Max(R, Max(G, B));
  CMin := Min(R, Min(G, B));
  Result.Lightness := (CMax + CMin) / 2;
  if CMax = CMin then
  begin
    Result.Hue := 0;
    Result.Saturation := 0
  end
  else
  begin
    D := CMax - CMin;
    if Result.Lightness < 0.5 then Result.Saturation := D / (CMax + CMin)
    else Result.Saturation := D / (2 - CMax - CMin);
    if R = CMax then Result.Hue := (G - B) / D
    else
      if G = CMax then Result.Hue  := 2 + (B - R) / D
      else Result.Hue := 4 + (R - G) / D;
    Result.Hue := Result.Hue / 6;
    if Result.Hue < 0 then Result.Hue := Result.Hue + 1
  end;
end;

{ Vertex type routines }

function Vertex(const X, Y: Float): TVertex;
begin
  Result.X := X;
  Result.Y := Y;
end;

function VertexAdd(const A, B: TVertex): TVertex;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function VertexSubtract(const A, B: TVertex): TVertex;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function VertexMultiply(const A: TVertex; Factor: Float): TVertex;
begin
  Result.X := A.X * Factor;
  Result.Y := A.Y * Factor;
end;

function VertexAngle(const V: TVertex): Float;
begin
  if V.Y = 0 then
    if V.X < 0 then
      Result := 270
    else if V.X > 0 then
      Result := 90
    else
      Result := 0
  else
  begin
    Result := -ArcTan(V.X / V.Y) * (180 / PI);
    if V.Y > 0 then
      Result := Result + 180
    else if V.X < 0 then
      Result := 360 + Result
  end;
end;

function VertexRotate(const V: TVertex; const Angle: Float): TVertex;
var
  Radians: Single;
begin
  Radians :=   Angle * (PI / 180);
  Result.X := (V.X * Cos(Radians)) - (V.Y * Sin(Radians));
  Result.Y := (V.X * Sin(Radians)) + (V.Y * Cos(Radians));
end;

function VertexDistance(const A, B: TVertex): Float;
var
  X, Y: Float;
begin
  X := A.X - B.X;
  Y := A.Y - B.Y;
  Result := Sqrt(X * X + Y * Y);
end;

function VertexAverage(const A, B: TVertex): TVertex;
begin
  Result.X := (A.X + B.X) / 2;
  Result.Y := (A.Y + B.Y) / 2;
end;

function VertexNormalize(const V: TVertex): TVertex;
var
  Ratio: Single;
begin
  Ratio := Sqrt(V.X * V.X + V.Y * V.Y);
  if Ratio > 0 then
  begin
    Ratio := 1 / Ratio;
    Result.X := V.X * Ratio;
    Result.Y := V.Y * Ratio;
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

procedure VertexOffset(Distance: Float; var A, B: TVertex);
var
  Delta: TVertex;
begin
  Delta.Y := A.X - B.X;
  Delta.X := A.Y - B.Y;
  Delta := VertexNormalize(Delta);
  A.X := A.X + Delta.X * -Distance;
  A.Y := A.Y + Delta.Y * Distance;
  B.X := B.X + Delta.X * -Distance;
  B.Y := B.Y + Delta.Y * Distance;
end;

procedure VerticesAdd(var List: TVertices; const X, Y: Float);
var
  I: Integer;
begin
  I := Length(List);
  Setlength(List, I + 1);
  List[I].X := X;
  List[I].Y := Y;
end;

procedure VerticesAdd(var List: TVertices; const V: TVertex);
var
  I: Integer;
begin
  I := Length(List);
  Setlength(List, I + 1);
  List[I] := V;
end;

{ Vertex type routines }

function MatrixAdd(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[X, Y] + B[X, Y];
end;

function MatrixSubtract(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[X, Y] - B[X, Y];
end;

function MatrixMultiply(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[0, Y] * B[X, 0] + A[1, Y] * B[X, 1] + A[2, Y] *
      B[X, 2] + A[3, Y] * B[X, 3];
end;

function MatrixDivide(const A, B: TMatrix): TMatrix;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result[X, Y] := A[0, Y] / B[X, 0] + A[1, Y] / B[X, 1] + A[2, Y] / B[X, 2];
end;

function MatrixScale(const M: TMatrix; X, Y: Float): TMatrix;
var
  S: TMatrix;
begin
  S := IdentityMatrix;
  S[0, 0] := X;
  S[1, 1] := Y;
  Result := MatrixMultiply(M, S);
end;

function MatrixTranslate(const M: TMatrix; X, Y: Float): TMatrix;
var
  T: TMatrix;
begin
  T := IdentityMatrix;
  T[3, 0] := X;
  T[3, 1] := Y;
  Result := MatrixMultiply(M, T);
end;

function MatrixRotate(const M: TMatrix; const Delta: Float): TMatrix;
var
  D: Float;
  R: TMatrix;
begin
  Result := M;
  D := Delta * (PI / 180);
  if D <> 0 then
  begin
    R := IdentityMatrix;
    R[0, 0] := Cos(D);
    R[1, 0] := Sin(D);
    R[0, 1] := -R[1, 0];
    R[1, 1] := R[0, 0];
    Result := MatrixMultiply(Result, R);
  end;
end;

function MatrixTransform(const M: TMatrix; const V: TVertex): TVertex;
begin
  Result.X := M[0, 0] * V.X + M[1, 0] * V.Y + M[3, 0];
  Result.Y := M[0, 1] * V.X + M[1, 1] * V.Y + M[3, 1];
end;

{ GDI routines }

function LineTo(DC: HDC; X, Y: Integer): BOOL;
begin
  Result := Windows.LineTo(DC, X, Y);
end;

function LineTo(DC: HDC; const X, Y: Float): BOOL;
begin
  Result := Windows.LineTo(DC, Round(X), Round(Y));
end;

function LineTo(DC: HDC; const V: TVertex): BOOL;
begin
  Result := Windows.LineTo(DC, Round(V.X), Round(V.Y));
end;

function LineTo(DC: HDC; const V, Pivot, Scale: TVertex): BOOL;
begin
  Result := Windows.LineTo(DC, Round((V.X - Pivot.X) * Scale.X + Pivot.X),
    Round((V.Y - Pivot.Y) * Scale.Y + Pivot.Y));
end;

function LineTo(DC: HDC; const V: TVertex; const Matrix: TMatrix): BOOL;
begin
  with MatrixTransform(Matrix, V) do
    Result := Windows.LineTo(DC, Round(X), Round(Y));
end;

function MoveToEx(DC: HDC; X, Y: Integer; P: PPoint = nil): BOOL;
begin
  Result := Windows.MoveToEx(DC, X, Y, P);
end;

function MoveToEx(DC: HDC; const X, Y: Float): BOOL;
begin
  Result := Windows.MoveToEx(DC, Round(X), Round(Y), nil);
end;

function MoveTo(DC: HDC; const X, Y: Float): BOOL; overload;
begin
  Result := Windows.MoveToEx(DC, Round(X), Round(Y), nil);
end;

function MoveTo(DC: HDC; const V: TVertex): BOOL;
begin
  Result := Windows.MoveToEx(DC, Round(V.X), Round(V.Y), nil);
end;

function MoveTo(DC: HDC; const V, Pivot, Scale: TVertex): BOOL;
begin
  Result := Windows.MoveToEx(DC, Round((V.X - Pivot.X) * Scale.X + Pivot.X),
    Round((V.Y - Pivot.Y) * Scale.Y + Pivot.Y), nil);
end;

function MoveTo(DC: HDC; const V: TVertex; const Matrix: TMatrix): BOOL;
begin
  with MatrixTransform(Matrix, V) do
    Result := Windows.MoveToEx(DC, Round(X), Round(Y), nil);
end;

function Ellipse(DC: HDC; X1, Y1, X2, Y2: Integer): BOOL;
begin
  Result := Windows.Ellipse(DC, X1, Y1, X2, Y2);
end;

function Ellipse(DC: HDC; const X, Y, Radius: Float): BOOL;
begin
  Result := Windows.Ellipse(DC, Round(X - Radius), Round(Y - Radius),
    Round(X + Radius), Round(Y + Radius));
end;

function Ellipse(DC: HDC; const Center: TVertex; const Radius: Float): BOOL;
begin
  Result := Windows.Ellipse(DC, Round(Center.X - Radius), Round(Center.Y - Radius),
    Round(Center.X + Radius * 2), Round(Center.Y + Radius * 2));
end;

function Ellipse(DC: HDC; const Pivot, Scale, Center: TVertex; const Radius: Float): BOOL;
begin
  Result := Windows.Ellipse(DC,
    Round((Center.X - Pivot.X - Radius) * Scale.X + Pivot.X),
    Round((Center.Y - Pivot.Y - Radius) * Scale.Y + Pivot.Y),
    Round((Center.X - Pivot.X + Radius) * Scale.X + Pivot.X),
    Round((Center.Y - Pivot.Y + Radius) * Scale.Y + Pivot.Y));
end;

function Ellipse(DC: HDC; const Center: TVertex; const Radius: Float; const Matrix: TMatrix): BOOL;
var
  A, B: TVertex;
begin
  A := MatrixTransform(Matrix, Vertex(Center.X - Radius, Center.Y - Radius));
  B := MatrixTransform(Matrix, Vertex(Center.X + Radius, Center.Y + Radius));
  Result := Windows.Ellipse(DC, Round(A.X), Round(A.Y), Round(B.X), Round(B.Y));
end;

function Rectangle(DC: HDC; X1, Y1, X2, Y2: Integer): BOOL;
begin
  Result := Windows.Rectangle(DC, X1, Y1, X2, Y2);
end;

function Rectangle(DC: HDC; const Rect: TVertexRect; const Matrix: TMatrix): BOOL;
var
  A, B: TVertex;
begin
  A := MatrixTransform(Matrix, Rect.TopLeft);
  B := MatrixTransform(Matrix, Rect.BottomRight);
  Result := Windows.Rectangle(DC, Round(A.X), Round(A.Y), Round(B.X), Round(B.Y));
end;

function Pie(DC: HDC; X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer): BOOL;
begin
  Result := Windows.Pie(DC, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
end;

function Pie(DC: HDC; const X1, Y1, X2, Y2, X3, Y3, X4, Y4: Float): BOOL;
begin
  Result := Windows.Pie(DC, Round(X1), Round(Y1), Round(X2), Round(Y2),
    Round(X3), Round(Y3), Round(X4), Round(Y4));
end;

function Pie(DC: HDC; const Center: TVertex; const Radius: Float; const V1, V2: TVertex): BOOL;
begin
  Result := Windows.Pie(DC, Round(Center.X - Radius), Round(Center.Y - Radius),
    Round(Center.X + Radius), Round(Center.Y + Radius),
    Round(V1.X), Round(V1.Y), Round(V2.X), Round(V2.Y));
end;

{ TVertexList }

procedure TVertexList.CheckGrow;
const
  GrowSize = 10;
begin
  if FCount = Length(FList) then
    SetLength(FList, Length(FList) + GrowSize + Length(FList) shr 1);
end;

procedure TVertexList.Draw(DC: HDC);
var
  P: PMatrix;
  I: Integer;
begin
  if FCount < 3 then Exit;
  if FWorld <> nil then
    P := FWorld
  else
    P := @IdentityMatrix;
  MoveTo(DC, FList[0], P^);
  for I := 1 to FCount - 1 do
    LineTo(DC, FList[I], P^);
  LineTo(DC, FList[0], P^);
end;

procedure TVertexList.Add(const X, Y: Float);
begin
  CheckGrow;
  FList[FCount].X := X;
  FList[FCount].Y := Y;
  Inc(FCount);
end;

procedure TVertexList.Add(const V: TVertex);
begin
  CheckGrow;
  FList[FCount] := V;
  Inc(FCount);
end;

procedure TVertexList.Remove(Index: Integer);
begin
  if (Index > -1) and (Index < FCount) then
    if Index = FCount - 1 then
      Dec(FCount)
    else
    begin
      Move(FList[Index + 1], FList[Index], (FCount - Index + 1) * SizeOf(TVertex));
      Dec(FCount);
      if FCount = 0 then
        FList := nil;
    end;
end;

procedure TVertexList.Clear;
begin
  FList := nil;
  FCount := 0;
end;

procedure TVertexList.Copy(Source: TVertexList);
begin
  if Source.FCount = 0 then
  begin
    FList := nil;
    FCount := 0;
  end
  else
  begin
    SetLength(FList, Length(Source.FList));
    Move(Source.FList[0], FList[0], Length(FList) * SizeOf(TVertex));
    FCount := Source.FCount;
  end;
  FWorld := Source.FWorld;
end;

procedure TVertexList.Reference(var List: TVertices);
begin
  if FCount > 0 then
    List := FList
  else
    List := nil;
end;

function TVertexList.Get(Index: Integer): TVertex;
begin
  Result := FList[Index];
  if FWorld <> nil then
    Result := MatrixTransform(FWorld^, Result);
end;

procedure TVertexList.Put(Index: Integer; const Value: TVertex);
begin
  FList[Index] := Value;
end;

function TVertexList.GetX(Index: Integer): Float;
begin
  Result := Get(Index).X;
end;

procedure TVertexList.SetX(Index: Integer; const Value: Float);
begin
  FList[Index].X := Value;
end;

function TVertexList.GetY(Index: Integer): Float;
begin
  Result := Get(Index).Y;
end;

procedure TVertexList.SetY(Index: Integer; const Value: Float);
begin
  FList[Index].Y := Value;
end;

{ TFastBitmap routines }

const
  Depths: array[TPixelDepth] of Integer = (24, 32);

function CreateFastBitmap(Width, Height: Integer; Depth: TPixelDepth = pd24): TFastBitmap;
var
  BitmapInfo: TBitmapinfo;
begin
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  if (Width < 1) or (Height = 0) then
    Exit;
  Result.DC := CreateCompatibleDC(0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := Depths[Depth];
    biCompression := BI_RGB;
  end;
  with Result do
    Handle := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
  Result.Width := Width;
  Result.Height := Height;
  if Result.Height < 0 then
    Result.Height := -Result.Height;
  Result.Depth := Depth;
  with Result do
    OldBitmap := SelectObject(DC, Handle);
end;

function CreateFastBitmap(const Rect: TRect; Depth: TPixelDepth = pd24): TFastBitmap; overload;
begin
  Result := CreateFastBitmap(WidthOf(Rect), HeightOf(Rect), Depth);
end;

function CreatePixel(RGBA: TRGBA): TFastBitmap;
var
  R: Single;
begin
  Result := CreateFastBitmap(1, 1, pd32);
  R := RGBA.Alpha / $FF;
  RGBA.Red := Round(RGBA.Red * R);
  RGBA.Green := Round(RGBA.Green * R);
  RGBA.Blue := Round(RGBA.Blue * R);
  PRGBA(Result.Bits)^ := RGBA;
end;

procedure DestroyFastBitmap(var Bitmap: TFastBitmap);
begin
  if Bitmap.DC <> 0 then
  begin
    SelectObject(Bitmap.DC, Bitmap.OldBitmap);
    DeleteObject(Bitmap.Handle);
    DeleteDC(Bitmap.DC);
    FillChar(Bitmap, SizeOf(Bitmap), #0);
  end;
end;

function IsFastBitmap(const Bitmap: TFastBitmap): Boolean;
begin
  Result := Bitmap.DC <> 0;
end;

function ScanlineStride(const Bitmap: TFastBitmap): Integer;
begin
  if Bitmap.Depth = pd24 then
    Result := Bitmap.Width * SizeOf(TRGB)
  else
    Result := Bitmap.Width * SizeOf(TRGBA);
  if Result mod SizeOf(DWORD) > 0 then
    Inc(Result, SizeOf(DWORD) - Result mod SizeOf(DWORD));
end;

procedure FillColor(const Bitmap: TFastBitmap; RGBA: TRGBA);
var
  C: PRGBA;
  I: Integer;
begin
  if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    C := Bitmap.Bits;
    for I := 0 to Bitmap.Height * Bitmap.Width do
    begin
      C^ := RGBA;
      Inc(C);
    end;
  end;
end;

procedure BlitDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap);
begin
  BitBlt(DC, X, Y, Bitmap.Width, Bitmap.Height, Bitmap.DC, 0, 0, SRCCOPY);
end;

procedure BlitDrawBits(DC: HDC; X, Y: Integer; Bits: Pointer;
  Width, Height: Integer; Depth: TPixelDepth = pd24);
var
  BitmapInfo: TBitmapInfo;
begin
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := Width;
    biHeight := Ord(Height);
    biPlanes := 1;
    biBitCount := Depths[Depth];
    biCompression := BI_RGB;
  end;
  StretchDIBits(DC, X, Y, Width, Height, 0, 0, Width,
    Height, Bits, BitmapInfo, DIB_RGB_COLORS, SRCCOPY);
end;

procedure BlitDrawBits(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap);
begin
  BlitDrawBits(DC, X, Y, Bitmap.Bits, Bitmap.Width, Bitmap.Height, Bitmap.Depth);
end;

procedure AlphaDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
begin
  if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    Func.BlendOp := 0;
    Func.BlendFlags := 0;
    Func.SourceConstantAlpha := Opacity;
    Func.AlphaFormat := AC_SRC_ALPHA;
    AlphaBlend(DC, X, Y, Bitmap.Width,
      Bitmap.Height, Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, Func);
  end;
end;

procedure AlphaDraw(DC: HDC; const Rect: TRect; const Bitmap: TFastBitmap; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
begin
  if IsFastBitmap(Bitmap) and (Opacity > 0) then
  begin
    Func.BlendOp := 0;
    Func.BlendFlags := 0;
    Func.SourceConstantAlpha := Opacity;
    if Bitmap.Depth = pd32 then
      Func.AlphaFormat := AC_SRC_ALPHA
    else
      Func.AlphaFormat := 0;
    AlphaBlend(DC, Rect.Left, Rect.Top, WidthOf(Rect),
      HeightOf(Rect), Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, Func);
  end;
end;

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

procedure AlphaDrawRect(DC: HDC; const Source: TRect; const Bitmap: TFastBitmap; const Dest: TRect; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
begin
  if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    FillChar(Func, SizeOf(Func), #0);
    Func.SourceConstantAlpha := Opacity;
    Func.AlphaFormat := AC_SRC_ALPHA;
    AlphaBlend(DC, Source.Left, Source.Top, WidthOf(Source),
      HeightOf(Source), Bitmap.DC, Dest.Left, Dest.Top, WidthOf(Dest),
      HeightOf(Dest), Func);
  end;
end;

procedure AlphaFill(Bitmap: TFastBitmap; Alpha: Byte = $FF);
var
  P: PRGBA;
  X, Y: Integer;
begin
  if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    P := Bitmap.Bits;
    for X := 0 to Bitmap.Width - 1 do
      for Y := 0 to Bitmap.Height - 1 do
      begin
        P.Alpha := Alpha;
        Inc(P);
      end;
  end;
end;

procedure AlphaRect(Bitmap: TFastBitmap; Rect: TRect; Alpha: Byte = $FF);
var
  P: PRGBA;
  X, Y: Integer;
begin
  if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    if Rect.Left < 0 then Rect.Left := 0;
    if Rect.Top < 0 then Rect.Top := 0;
    if Rect.Right > Bitmap.Width - 1 then Rect.Right := Bitmap.Width - 1;
    if Rect.Bottom > Bitmap.Height - 1 then Rect.Bottom := Bitmap.Height - 1;
    for Y := Rect.Top to Rect.Bottom do
    begin
      P := Bitmap.Bits;
      Inc(P, Bitmap.Width * Y + Rect.Left);
      for X := Rect.Left to Rect.Right do
      begin
        P.Alpha := Alpha;
        Inc(P)
      end;
    end;
  end;
end;

function IntToStr(I: Integer): string;
var
  S: AnsiString;
begin
  Str(I, S);
  Result := S;
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := Default;
end;

function Split(const Source: string; Terminator: Char): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, FieldCount(Source, Terminator));
  for I := Low(Result) to High(Result) do
    Result[I] := FieldValue(Source, Terminator, I);
end;

function FieldCount(const Source: string; Terminator: Char): Integer;
var
  P: PChar;
begin
  Result := 0;
  if Source <> '' then
  begin
    P := PChar(Source);
    repeat
      if P^ = Terminator then
        Inc(Result);
      Inc(P);
    until P^ = #0;
    if (Result > 0) and (P[-1] <> Terminator) then
      Inc(Result);
  end;
  if Result = 0 then Result := 1;
end;

function FieldValue(const Source: string; Terminator: Char; Index: Integer): string;
var
  Start, P: PChar;
  I: Integer;
begin
  Result := '';
  if Source <> '' then
  begin
    Start := PChar(Source);
    P := Start;
    I := 0;
    while P^ > #0 do
    begin
      if P^ = Terminator then
      begin
        if I = Index then
        begin
          SetString(Result, Start, Integer(P - Start));
          Exit;
        end;
        Start := P;
        Inc(Start);
        Inc(I);
      end;
      Inc(P);
    end;
    if I = Index then
      SetString(Result, Start, Integer(P - Start));
  end;
end;

function FieldValue(const Source: string; Index: Integer): string;
begin
  Result := FieldValue(Source, DefTerminator, Index);
end;

function FieldValueInt(const Source: string; Terminator: Char; Index: Integer): string;
var
  I: Integer;
begin
  Result := FieldValue(Source, Terminator, Index);
  I := StrToIntDef(Result, -1);
  if I <> -1 then
    Result := IntToStr(I);
end;

function FieldValueInt(const Source: string; Index: Integer): string; overload;
var
  I: Integer;
begin
  Result := FieldValue(Source, Index);
  I := StrToIntDef(Result, -1);
  if I <> -1 then
    Result := IntToStr(I);
end;

function IntFieldValue(const Source: string; Index: Integer): Integer; overload;
begin
  Result := StrToIntDef(FieldValue(Source, Index), 0);
end;

function FieldSearch(const Key, Source: string; Terminator: Char): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FieldCount(Source, Terminator) -1 do
    if Key = FieldValue(Source, Terminator, I) then
    begin
      Result := True;
      Break;
    end;
end;

var
  Debugged: Boolean;

procedure FileWriteString(const FileName, Value: string;
  CreateFlag: Cardinal = CREATE_NEW);
var
  F: THandle;
  Bytes: Cardinal;
begin
  if Value = '' then Exit;
  F := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, CreateFlag,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    SetFilePointer(F, 0, nil, FILE_END);
    WriteFile(F, PChar(Value)^, Length(Value), Bytes, nil);
  finally
    CloseHandle(F);
  end;
end;

procedure FileWrite(const FileName, Value: string);
var
  F: THandle;
  Bytes: Cardinal;
begin
  F := CreateFile(PChar(FileName), GENERIC_WRITE, 0, nil, OPEN_ALWAYS,
    FILE_ATTRIBUTE_NORMAL, 0);
  if F <> INVALID_HANDLE_VALUE then
  try
    SetFilePointer(F, 0, nil, FILE_END);
    WriteFile(F, PChar(Value)^, Length(Value), Bytes, nil);
  finally
    CloseHandle(F);
  end;
end;

procedure FileWriteLn(const FileName, Value: string);
begin
  FileWrite(FileName, Value + #13#10);
end;

{.$DEFINE DEBUGLOG}

procedure DebugLog(const S: string);
const
  DebugFile = 'c:\debug.txt';
begin
  {$IFNDEF DEBUGLOG}
  Exit;
  {$ENDIF}
  if not Debugged then
    FileWriteString(DebugFile, ParamStr(0) + #13#10);
  Debugged := True;
  FileWriteLn(DebugFile, S);
end;

const
  ole32    = 'ole32.dll';

function CoInitialize(pvReserved: Pointer): HResult; stdcall; external ole32 name 'CoInitialize';

initialization
  CoInitialize(nil);
end.
