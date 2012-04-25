
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ColorCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls, Forms, BaseTypes, GraphTools, GdiPlus, GdiIntf,
  ImgList, ProviderTools, FormTools, SlideCtrls, PopCtrls,
  XMLObjects, XMLParser, ComObj, ShellAPI, ShlIntf, ShlTools,
  ShlCtrls, Dialogs, ShlObj, BtnCtrls;


type
  {TColorEdit = class(TCutomControl)
  private
    FAutoHeight: Boolean;
    FArgb: TArgb;
    function GetColorAll: DWORD;
    procedure SetColorAll(Value: DWORD)
    function GetAlpha: Byte;
    procedure SetAlpha(Value: Alpha);
    function GetRed: Byte;
    procedure SetAlpha(Value: Red);
    function GetGreen: Byte;
    procedure SetAlpha(Value: Green);
    function GetBlue: Byte;
    procedure SetAlpha(Value: Blue);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ColorAlpha: Byte read GetAlpha write SetAlpha;
    property ColorRed: Byte read GetRed write SetRed;
    property ColorGreen: Byte read GetGreen write SetGreen;
    property ColorBlue: Byte read GetBlue write SetBlue;
  published
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;
    property ColorAll: DWORD read GetColorAll write SetColorAll;
    property Align;
    property Anchors;
    property Enabled;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyUp;
    property OnKeyDown;
    property OnKeyPress;
  end;}

{ TColorPickerControl }

  TColorPickerControl = class(TGraphicControl)
  private
    FBitmap: TFastBitmap;
    FMousePos: TPoint;
    FBorder: Boolean;
    FOnChange: TNotifyEvent;
    procedure CheckChangeMouse(X, Y: Integer);
    procedure SetBorder(Value: Boolean);
  protected
    procedure Change;
    procedure ChangeBitmap;
    procedure ChangeMouse(X, Y: Integer); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintBitmap(B: TFastBitmap); virtual; abstract;
    procedure PaintOverlays; virtual; abstract;
    property MousePos: TPoint read FMousePos write FMousePos;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    destructor Destroy; override;
    function GetColor: TColor; virtual; abstract;
    procedure SetColor(C: TColor); virtual; abstract;
    property Border: Boolean read FBorder write SetBorder;
  end;

{ THuePicker }

  THueStyle = (hsLinear, hsRadial);

  THuePicker = class(TColorPickerControl)
  private
    FHue: Single;
    FStyle: THueStyle;
    procedure SetHue(Value: Single);
    procedure SetStyle(Value: THueStyle);
  protected
    procedure ChangeMouse(X, Y: Integer); override;
    procedure PaintBitmap(B: TFastBitmap); override;
    procedure PaintOverlays; override;
  public
    function GetColor: TColor; override;
    procedure SetColor(C: TColor); override;
  published
    property Hue: Single read FHue write SetHue;
    property Style: THueStyle read FStyle write SetStyle;
    property OnChange;
  end;

  {THueCircle = class(TColorPickerControl)
  private
    FHue: Single;
    FStyle: THueStyle;
    procedure SetHue(Value: Single);
    procedure SetStyle(Value: THueStyle);
  protected
    procedure ChangeMouse(X, Y: Integer); override;
    procedure PaintBitmap(B: TFastBitmap); override;
    procedure PaintOverlays; override;
  public
    function GetColor: TColor; override;
    procedure SetColor(C: TColor); override;
  published
    property Hue: Single read FHue write SetHue;
    property Style: THueStyle read FStyle write SetStyle;
    property OnChange;
  end;}

{ TSaturationPicker }

  TSaturationStyle = (ssSaturate, ssDesaturate);

  TSaturationPicker = class(TColorPickerControl)
  private
    FHue: Single;
    FSaturation: Single;
    FLightness: Single;
    FStyle: TSaturationStyle;
    procedure SetHue(Value: Single);
    procedure SetSaturation(Value: Single);
    procedure SetLightness(Value: Single);
    procedure SetStyle(Value: TSaturationStyle);
  protected
    procedure ChangeMouse(X, Y: Integer); override;
    procedure PaintBitmap(B: TFastBitmap); override;
    procedure PaintOverlays; override;
  public
    function GetColor: TColor; override;
    procedure SetColor(C: TColor); override;
  published
    property Hue: Single read FHue write SetHue;
    property Saturation: Single read FSaturation write SetSaturation;
    property Lightness: Single read FLightness write SetLightness;
    property Style: TSaturationStyle read FStyle write SetStyle;
    property OnChange;
  end;

{ TAlphaPicker }

  TAlphaStyle = (asGradient, asSolid);

  TAlphaPicker = class(TColorPickerControl)
  private
    FCheckerSize: Integer;
    FColorAlpha: Single;
    FStyle: TAlphaStyle;
    procedure SetCheckerSize(Value: Integer);
    procedure SetColorAlpha(Value: Single);
    procedure SetStyle(Value: TAlphaStyle);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
  protected
    procedure ChangeMouse(X, Y: Integer); override;
    procedure PaintBitmap(B: TFastBitmap); override;
    procedure PaintOverlays; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetColor: TColor; override;
    procedure SetColor(C: TColor); override;
  published
    property Border;
    property Color;
    property CheckerSize: Integer read FCheckerSize write SetCheckerSize;
    property ColorAlpha: Single read FColorAlpha write SetColorAlpha;
    property Style: TAlphaStyle read FStyle write SetStyle;
    property OnChange;
  end;

{ TAnglePicker }

  TAnglePicker = class(TGraphicControl)
  private
    FAngle: Single;
    FStroke: Single;
    FOnChange: TNotifyEvent;
    procedure SetAngle(Value: Single);
    procedure SetStroke(Value: Single);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Enabled;
    property Color;
    property Angle: Single read FAngle write SetAngle;
    property Stroke: Single read FStroke write SetStroke;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TColorSlideEdit }

  TColorSlideKind = (cskRed, cskGreen, cskBlue, cskHue,
    cskSaturation, cskLightness, cskAlpha);

  TColorSlideEdit = class(TCustomSlideEdit)
  private
    FKind: TColorSlideKind;
    FColor: TColor;
    FTrackPoint: TPoint;
    FTrackValue: Integer;
    FTracking: Boolean;
    procedure SlideDrawBackground(Sender: TObject;
      Canvas: TCanvas; Rect: TRect; State: TDrawState);
    procedure SetKind(Value: TColorSlideKind);
  protected
    procedure AdjustEdit; override;
    procedure AdjustHeight; override;
    procedure DoChange; override;
    function GetButtonRect: TRect; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property RefColor: TColor read FColor;
    procedure UpdateAlpha(Alpha: Byte);
    procedure UpdateColor(Color: TColor);
    procedure UpdateHue(Hue: Single);
    procedure UpdateHSL(HSL: THSL);
  published
    property Visible;
    property Enabled;
    property Kind: TColorSlideKind read FKind write SetKind;
    property Position;
    property OnValueChange;
  end;

implementation

const
  OuterRing = 4;
  InnerRing = 3;

{ TColorPickerControl }

destructor TColorPickerControl.Destroy;
begin
  DestroyFastBitmap(FBitmap);
  inherited Destroy;
end;

procedure TColorPickerControl.ChangeBitmap;
begin
  DestroyFastBitmap(FBitmap);
end;

procedure TColorPickerControl.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Invalidate;
end;

procedure TColorPickerControl.CheckChangeMouse(X, Y: Integer);
begin
  if X < 0 then X := 0 else if X > Width - 1 then X := Width - 1;
  if Y < 0 then Y := 0 else if Y > Height - 1 then Y := Height - 1;
  if (FMousePos.X <> X) or (FMousePos.Y <> Y) then
  begin
    FMousePos.X := X;
    FMousePos.Y := Y;
    ChangeMouse(X, Y);
    Invalidate;
  end;
end;

procedure TColorPickerControl.SetBorder(Value: Boolean);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    Invalidate;
  end;
end;

procedure TColorPickerControl.ChangeMouse(X, Y: Integer);
begin
end;

procedure TColorPickerControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if MouseCapture then
    CheckChangeMouse(X, Y);
end;

procedure TColorPickerControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
    CheckChangeMouse(X, Y);
end;

procedure TColorPickerControl.Paint;
var
  DC: HDC;
  W, H: Integer;
  P: HPEN;
begin
  if (FBitmap.Width <> Width) or (FBitmap.Height <> Height) then
  begin
    DestroyFastBitmap(FBitmap);
  end;
  if FBitmap.DC = 0 then
  begin
    FBitmap := CreateFastBitmap(Width, Height, pd32);
    PaintBitmap(FBitmap);
  end;
  DC := Canvas.Handle;
  BlitDraw(DC, 0, 0, FBitmap);
  PaintOverlays;
  if FBorder then
  begin
    W := Width - 1;
    H := Height - 1;
    P := SelectObject(DC, GetStockObject(BLACK_PEN));
    MoveTo(DC, 0, 0);
    LineTo(DC, W, 0);
    LineTo(DC, W, H);
    LineTo(DC, 0, H);
    LineTo(DC, 0, 0);
    SelectObject(DC, P);
  end;
end;

{ THuePicker }

procedure THuePicker.ChangeMouse(X, Y: Integer);
var
  W, H, D: Integer;
begin
  if (Width < 1) or (Height < 1) then Exit;
  W := Width;
  H := Height;
  if FStyle = hsLinear then
  begin
    Hue := X / W;
  end
  else
  begin
    D := W + W + H + H;
    if (Y < X) and (Y < W - X) and (Y < H div 2) then
      Hue := X / D
    else if (W - X < H - Y) and (X > W div 2) then
      Hue := (W + Y) / D
    else if (H - Y <= X) and (H - Y <= W - X) then
      Hue := (W + W + H - X) / D
    else
      Hue := (W + W + H + H - Y) / D;
  end;
end;

procedure THuePicker.PaintBitmap(B: TFastBitmap);
begin
  if FStyle = hsLinear then
    DrawHueLinear(B.DC, Rect(0, 0, B.Width, B.Height))
  else
    DrawHueRadial(B.DC, Rect(0, 0, B.Width, B.Height))
end;

procedure THuePicker.PaintOverlays;
const
  ArrowSize = 6;
var
  DC: HDC;
  P: HPEN;
  B: HBRUSH;
  R, W, H, D, X: Integer;
begin
  DC:= Canvas.Handle;
  P := SelectObject(DC, GetStockObject(BLACK_PEN));
  B := SelectObject(DC, GetStockObject(BLACK_BRUSH));
  R := SetROP2(DC, R2_NOT);
  if FStyle = hsLinear then
  begin
    X := Round(Width * Hue);
    MoveToEx(DC, X, 0, nil);
    LineTo(DC, X, Height);
  end
  else
  begin
    W := Width;
    H := Height;
    D := W + W + H + H;
    X := Round(D * Hue);
    BeginPath(DC);
    if X < W then
    begin
      MoveToEx(DC, X - ArrowSize, 0, nil);
      LineTo(DC, X + ArrowSize, 0);
      LineTo(DC, X, ArrowSize);
      LineTo(DC, X - ArrowSize, 0);
    end
    else if X < W + H then
    begin
      X := X - W;
      MoveToEx(DC, W, X - ArrowSize, nil);
      LineTo(DC, W, X + ArrowSize);
      LineTo(DC, W - ArrowSize, X);
      LineTo(DC, W, X - ArrowSize);
    end
    else if X < W + W + H then
    begin
      X := X - W - H;
      MoveToEx(DC, W - X - ArrowSize, H, nil);
      LineTo(DC, W - X + ArrowSize, H);
      LineTo(DC, W - X, H - ArrowSize);
      LineTo(DC, W - X - ArrowSize, H);
    end
    else
    begin
      X := X - W - W - H;
      MoveToEx(DC, 0, H - X - ArrowSize, nil);
      LineTo(DC, 0, H - X + ArrowSize);
      LineTo(DC, ArrowSize, H - X);
      LineTo(DC, 0, H - X - ArrowSize);
    end;
    EndPath(DC);
    FillPath(DC);
    StrokePath(DC);
  end;
  SetROP2(DC, R);
  SelectObject(DC, B);
  SelectObject(DC, P);
end;

function THuePicker.GetColor: TColor;
begin
  Result := HSLToColor(HSL(Hue, 1, 0.5));
end;

procedure THuePicker.SetColor(C: TColor);
begin
  Hue := ColorToHSL(C).Hue;
end;

procedure THuePicker.SetHue(Value: Single);
begin
  if Value < 0 then Value := 0 else if Value > 1 then Value := 1;
  if Value <> FHue then
  begin
    FHue := Value;
    Change;
  end;
end;

procedure THuePicker.SetStyle(Value: THueStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    ChangeBitmap;
    Invalidate;
  end;
end;

{ TSaturationPicker }

procedure TSaturationPicker.ChangeMouse(X, Y: Integer);
var
  C, D: TRGBA;
  H: THSL;
  Black, White: Single;
begin
  if (Width < 1) or (Height < 1) then Exit;
  if FStyle = ssSaturate then
  begin
    if X <= 0 then
      Saturation := 0
    else if X >= Width - 1 then
      Saturation := 1
    else
      Saturation := X / Width;
    if Y <= 0 then
      Lightness := 0
    else if Y >= Height - 1 then
      Lightness := 1
    else
      Lightness := Y / Height;
  end
  else
  begin
    C := HSLtoRGBA(HSL(Hue, 1, 0.5));
    D := RGBA($FF - C.Red, $FF - C.Green, $FF - C.Blue, $FF);
    Black := 1 - Y / Height;
    White := 1 - X / Width;
    C.Red := Round((C.Red + D.Red * White) * Black);
    C.Green := Round((C.Green + D.Green * White) * Black);
    C.Blue := Round((C.Blue + D.Blue * White) * Black);
    H := RGBAToHSL(C);
    Saturation := H.Saturation;
    Lightness := H.Lightness;
  end;
end;

procedure TSaturationPicker.PaintBitmap(B: TFastBitmap);
begin
  if FStyle = ssSaturate then
    DrawSaturationBox(B.DC, Rect(0, 0, B.Width, B.Height), FHue)
  else
    DrawDesaturationBox(B.DC, Rect(0, 0, B.Width, B.Height), FHue);
end;

procedure TSaturationPicker.PaintOverlays;
var
  DC: HDC;
  X, Y: Integer;
begin
  DC := Canvas.Handle;
  if FStyle = ssSaturate then
  begin
    X := Round(Width * Saturation);
    Y := Round(Height * Lightness);
  end
  else
  begin
    X := MousePos.X;
    Y := MousePos.Y;
  end;
  {X := MousePos.X;
  Y := MousePos.Y;}
  DrawInverseRing(DC, X, Y + 1, InnerRing, OuterRing);
end;

procedure TSaturationPicker.SetHue(Value: Single);
begin
  if Value < 0 then Value := 0 else if Value > 1 then Value := 1;
  if Value <> FHue then
  begin
    FHue := Value;
    ChangeBitmap;
    Change;
  end;
end;

procedure TSaturationPicker.SetSaturation(Value: Single);
begin
  if Value < 0 then Value := 0 else if Value > 1 then Value := 1;
  if Value <> FSaturation then
  begin
    FSaturation := Value;
    Change;
  end;
end;

procedure TSaturationPicker.SetLightness(Value: Single);
begin
  if Value < 0 then Value := 0 else if Value > 1 then Value := 1;
  if Value <> FLightness then
  begin
    FLightness := Value;
    Change;
  end;
end;

procedure TSaturationPicker.SetStyle(Value: TSaturationStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    ChangeBitmap;
  end;
end;

function TSaturationPicker.GetColor: TColor;
begin
  Result := HSLToColor(HSL(Hue, Saturation, Lightness));
end;

procedure TSaturationPicker.SetColor(C: TColor);
var
  HSL: THSL;
begin
  HSL := ColorToHSL(C);
  if HSL.Hue <> FHue then
  begin
    FHue := HSL.Hue;
    FSaturation := HSL.Saturation;
    FLightness := HSL.Lightness;
    ChangeBitmap;
    Change;
  end
  else if (HSL.Saturation <> FSaturation) or (HSL.Lightness <> FLightness) then
  begin
    FSaturation := HSL.Saturation;
    FLightness := HSL.Lightness;
    Change;
  end;
end;

{ TAlphaPicker }

constructor TAlphaPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckerSize := 10;
  ParentColor := False;
  Color := 0;
end;

procedure TAlphaPicker.ChangeMouse(X, Y: Integer);
begin
  if Width < 1 then Exit;
  if FStyle = asSolid then Exit;
  if X < 1 then
    ColorAlpha := 0
  else if X > Width - 2 then
    ColorAlpha := 1
  else
    ColorAlpha := X / Width
end;

procedure TAlphaPicker.PaintBitmap(B: TFastBitmap);
var
  RGBA: TRGBA;
begin
  RGBA := ColorToRGBA(Color);
  RGBA.Alpha := Round(ColorAlpha * $FF);
  if FStyle = asGradient then
    DrawAlphaBox(B.DC, Rect(0, 0, B.Width, B.Height), RGBA, FCheckerSize)
  else
    DrawAlphaColor(B.DC, Rect(0, 0, B.Width, B.Height), RGBA, FCheckerSize);
end;

procedure TAlphaPicker.PaintOverlays;
const
  ArrowSize = 6;
var
  DC: HDC;
  P: HPEN;
  R, X: Integer;
begin
  if FStyle = asSolid then Exit;
  DC := Canvas.Handle;
  P := SelectObject(DC, GetStockObject(BLACK_PEN));
  R := SetROP2(DC, R2_NOT);
  X := Round(Width * ColorAlpha);
  if X = Width then
    Dec(X);
  MoveToEx(DC, X - 1, 0, nil);
  LineTo(DC, X - 1, Height);
  MoveToEx(DC, X, 0, nil);
  LineTo(DC, X, Height);
  MoveToEx(DC, X + 1, 0, nil);
  LineTo(DC, X + 1, Height);
  SetROP2(DC, R);
  SelectObject(DC, P);
end;

procedure TAlphaPicker.SetCheckerSize(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 20 then Value := 20;
  if Value <> FCheckerSize then
  begin
    FCheckerSize := Value;
    Invalidate;
  end;
end;

procedure TAlphaPicker.SetColorAlpha(Value: Single);
begin
  if Value < 0 then Value := 0 else if Value > 1 then Value := 1;
  if Value <> FColorAlpha then
  begin
    FColorAlpha := Value;
    if FStyle = asSolid then
      ChangeBitmap;
    Change;
  end;
end;

procedure TAlphaPicker.SetStyle(Value: TAlphaStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    ChangeBitmap;
    Invalidate;
  end;
end;

function TAlphaPicker.GetColor: TColor;
begin
  Result := Color;
end;

procedure TAlphaPicker.SetColor(C: TColor);
begin
  Color := C;
end;

procedure TAlphaPicker.CMColorChanged(var Message: TMessage);
begin
  inherited;
  ChangeBitmap;
  Change;
end;

{ TAnglePicker }

constructor TAnglePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  Color := clThemeBorder;
  Width := 48;
  Height := 48;
  FStroke := 2;
end;

procedure TAnglePicker.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  V: TVertex;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if MouseCapture then
  begin
    V := Vertex(X - Width / 2, Y - Height / 2);
    Angle :=  Round(VertexAngle(V));
    Invalidate;
  end;
end;

procedure TAnglePicker.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  V: TVertex;
begin
  inherited MouseMove(Shift, X, Y);
  if MouseCapture then
  begin
    V := Vertex(X - Width / 2, Y - Height / 2);
    Angle :=  Round(VertexAngle(V));
  end;
end;

procedure TAnglePicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Invalidate;
end;

procedure TAnglePicker.Paint;
var
  BaseColor: TColor;
  Size, OffsetX, OffsetY: Integer;
  Bitmap: TFastBitmap;
  G: IGdiGraphics;
  P: IGdiPen;
  R: TRectF;
  A, B, C: TVertex;
begin
  if ThemePainter.Enabled then
  begin
    if MouseCapture then
      BaseColor := Blend(clHighlight, clWindow, 75)
    else
      BaseColor := Blend(clHighlight, clWindow)
  end
  else
    BaseColor := cl3DDkShadow;
  if Width > Height then
  begin
    Size := Height;
    OffsetX := (Width - Size) div 2;
    OffsetY := 0;
  end
  else
  begin
    Size := Width;
    OffsetX := 0;
    OffsetY := (Height - Size) div 2;
  end;
  if Size < 1 then Exit;
  Bitmap := CreateFastBitmap(Size, Size, pd32);
  G := NewGraphics(Bitmap.DC);
  R := NewRectF(1, 1, Size - 2, Size - 2);
  InflateRectF(R, -0.5, -0.5);
  if ThemePainter.Enabled then
    G.FillEllipse(NewLinearGradientBrush(R, FAngle - 90, NewColor(BaseColor), NewColor(Blend(BaseColor, clWindow, 30))), R)
  else
    G.FillEllipse(NewSolidBrush(NewColor(Blend(BaseColor, clWindow))), R);
  InflateRectF(R, 0.5, 0.5);
  P := NewPen(NewColor(BaseColor));
  G.DrawEllipse(P, R);
  P := NewPen(NewColor(Blend(BaseColor, 0)), FStroke);
  C := Vertex(Size / 2, Size / 2);
  R.X := C.X;
  R.Y := C.Y;
  R.Width := 0;
  R.Height := 0;
  InflateRectF(R, FStroke * 1.5, FStroke * 1.5);
  G.DrawEllipse(P, R);
  A := Vertex(0, FStroke * -1.5);
  A := VertexRotate(A, FAngle);
  A := VertexAdd(A, C);
  B := Vertex(0, (Size - FStroke) / -2);
  B := VertexRotate(B, FAngle);
  B := VertexAdd(B, C);
  G.DrawLine(P, A.X, A.Y, B.X, B.Y);
  BaseTypes.AlphaDraw(Canvas.Handle, OffsetX, OffsetY, Bitmap);
  DestroyFastBitmap(Bitmap);
end;

procedure TAnglePicker.SetAngle(Value: Single);
begin
  if Value <> FAngle then
  begin
    FAngle := Value;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TAnglePicker.SetStroke(Value: Single);
begin
  if Value < 0 then
    Value := 0;
  if Value > 50 then
    Value := 50;
  if Value <> FStroke then
  begin
    FStroke := Value;
    Invalidate;
  end;
end;

{ TColorSlideEdit }

const
  ColorBarHeight = 6;

constructor TColorSlideEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  Max := 255;
  Width := 52;
  OnDrawBackground := SlideDrawBackground;
end;

procedure TColorSlideEdit.AdjustEdit;
var
  Rect: TRect;
begin
  Rect.Left := 1;
  Rect.Top := 1;
  Rect.Right := InternalWidth - GetSystemMetrics(SM_CXVSCROLL) - 1;
  Rect.Bottom := InternalHeight - 1;
  Dec(Rect.Bottom, ColorBarHeight);
  if Images <> nil then
    Inc(Rect.Left, Images.Width + 4);
  DoPlaceEdit(Rect);
  InnerEdit.BoundsRect := Rect;
end;

procedure TColorSlideEdit.AdjustHeight;
begin
	if AutoHeight then
    Height := CalcEditHeight(Font) + ColorBarHeight;
  AdjustEdit;
end;

procedure TColorSlideEdit.DoChange;
begin
  inherited DoChange;
  Invalidate;
end;

function TColorSlideEdit.GetButtonRect: TRect;
begin
  Result := inherited GetButtonRect;
  Dec(Result.Bottom, ColorBarHeight);
end;

procedure TColorSlideEdit.Paint;
var
  DC: HDC;
  Pen: HPEN;
  P: Single;
  R: TRect;
  G: IGdiGraphics;
  B: IGdiBrush;
begin
  inherited Paint;
  DC := Canvas.Handle;
  P := Position / Max;
  R := ClientRect;
  R.Top := R.Bottom - ColorBarHeight + 1;
  case Kind of
    cskRed:
      begin
        FillRectColor(DC, R, clBlack);
        R.Right := R.Left + Round(WidthOf(R) * P);
        FillRectColor(DC, R, clRed);
      end;
    cskGreen:
      begin
        FillRectColor(DC, R, clBlack);
        R.Right := R.Left + Round(WidthOf(R) * P);
        FillRectColor(DC, R, clLime);
      end;
    cskBlue:
      begin
        FillRectColor(DC, R, clBlack);
        R.Right := R.Left + Round(WidthOf(R) * P);
        FillRectColor(DC, R, clBlue);
      end;
    cskAlpha:
      begin
        FillRectChecker(DC, R, 3);
        G := NewGraphics(DC);
        B := NewLinearGradientBrush(NewRectF(R), 0, 0, NewColor(FColor));
        G.FillRectangle(B, NewRectF(R));
      end;
    cskHue: FillRectColor(DC, R, HueToColor(P));
    cskSaturation:
      DrawGradient(DC, R, HSLToColor(HSL(ColorToHSL(FColor).Hue, 0, 0.5)) , FColor, drRight);
    cskLightness:
      begin
        R.Right := WidthOf(R) div 2;
        DrawGradient(DC, R, clBlack, FColor, drRight);
        R.Left := R.Right;
        R.Right := ClientWidth;
        DrawGradient(DC, R, FColor, clWhite, drRight);
      end;
  end;
  R := ClientRect;
  R.Top := R.Bottom - ColorBarHeight + 1;
  R.Right := Round(ClientWidth * P);
  if R.Right < 1 then R.Right := 1;
  Pen := SelectObject(DC, GetStockObject(WHITE_PEN));
  MoveTo(DC, R.Right - 2, R.Top);
  LineTo(DC, R.Right - 2, R.Bottom);
  MoveTo(DC, R.Right, R.Top);
  LineTo(DC, R.Right, R.Bottom);
  SelectObject(DC, GetStockObject(BLACK_PEN));
  MoveTo(DC, R.Right - 1, R.Top);
  LineTo(DC, R.Right - 1, R.Bottom);
  SelectObject(DC, Pen);
end;

procedure TColorSlideEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    R := ClientRect;
    R.Top := R.Bottom - ColorBarHeight;
    FTrackPoint := Point(X, Y);
    FTrackValue := Round(Position);
    FTracking := PtInRect(R, FTrackPoint);
  end;
end;

procedure TColorSlideEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FTracking then
    Position := FTrackValue + X  - FTrackPoint.X;
end;

procedure TColorSlideEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FTracking := False;
end;

procedure TColorSlideEdit.SlideDrawBackground(Sender: TObject;
  Canvas: TCanvas; Rect: TRect; State: TDrawState);
var
  DC: HDC;
  R: TRect;
  G: IGdiGraphics;
  B: IGdiBrush;
begin
  InflateRect(Rect, 0, -5);
  DC := Canvas.Handle;
  R := Rect;
  case Kind of
    cskRed: DrawGradient(DC, R, 0, clRed, drRight);
    cskGreen: DrawGradient(DC, R, 0, clLime, drRight);
    cskBlue: DrawGradient(DC, R, 0, clBlue, drRight);
    cskAlpha:
      begin
        FillRectChecker(DC, R, 4);
        G := NewGraphics(DC);
        B := NewLinearGradientBrush(NewRectF(R), 0, 0, NewColor(FColor));
        G.FillRectangle(B, NewRectF(R));
      end;
    cskHue: DrawHueLinear(DC, Rect);
    cskSaturation: DrawGradient(DC, R, HSLToColor(HSL(ColorToHSL(FColor).Hue, 0, 0.5)) , FColor, drRight);
    cskLightness:
      begin
        R.Right := WidthOf(R) div 2;
        DrawGradient(DC, R, clBlack, FColor, drRight);
        R.Left := R.Right;
        R.Right := Rect.Right;
        DrawGradient(DC, R, FColor, clWhite, drRight);
      end;
  end;
end;

procedure TColorSlideEdit.UpdateAlpha(Alpha: Byte);
var
  Event: TNotifyEvent;
begin
  if FKind = cskAlpha then
  begin
    Event := OnValueChange;
    try
      OnValueChange := nil;
      Position := Alpha;
      Invalidate;
    finally
      OnValueChange := Event;
    end;
  end;
end;

procedure TColorSlideEdit.UpdateColor(Color: TColor);
var
  Event: TNotifyEvent;
  RGBA: TRGBA;
begin
  if FKind in [cskRed..cskAlpha] then
  begin
    Color := ColorToRGB(Color);
    if Color <> FColor then
    begin
      FColor := Color;
      Event := OnValueChange;
      try
        OnValueChange := nil;
        RGBA := ColorToRGBA(FColor);
        case FKind of
          cskRed: Position := RGBA.Red;
          cskGreen: Position := RGBA.Green;
          cskBlue: Position := RGBA.Blue;
        end;
        Invalidate;
      finally
        OnValueChange := Event;
      end;
    end;
  end;
end;

procedure TColorSlideEdit.UpdateHue(Hue: Single);
var
  Event: TNotifyEvent;
begin
  if FKind in [cskHue, cskSaturation, cskLightness] then
  begin
    FColor := HSLToColor(HSL(Hue, 1.0, 0.5));
    Event := OnValueChange;
    try
      OnValueChange := nil;
      case FKind of
        cskHue: Position := Round(Hue * Max);
      end;
      Invalidate;
    finally
      OnValueChange := Event;
    end;
  end;
end;

procedure TColorSlideEdit.UpdateHSL(HSL: THSL);
var
  Event: TNotifyEvent;
begin
  if FKind in [cskSaturation, cskLightness] then
  begin
    Event := OnValueChange;
    try
      OnValueChange := nil;
      case FKind of
        cskSaturation: Position := Round(HSL.Saturation * Max);
        cskLightness: Position := Round(HSL.Lightness * Max);
      end;
      Invalidate;
    finally
      OnValueChange := Event;
    end;
  end;
end;

procedure TColorSlideEdit.SetKind(Value: TColorSlideKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    Invalidate;
  end;
end;

end.
