unit StepCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  BaseTypes, GdiPlus, GdiIntf;

type
  TStepClickEvent = procedure (Sender: TObject; StepIndex: Integer) of object;

  TStepBubbles = class(TGraphicControl)
  private
    FStepIndex: Integer;
    FHotStepIndex: Integer;
    FDownStepIndex: Integer;
    FSteps: TStrings;
    FStepRects: TRects;
    FHotTrack: Boolean;
    FTransparent: Boolean;
    FBoldFont: IGdiFont;
    FOnStep: TNotifyEvent;
    FOnStepClick: TStepClickEvent;
    procedure StepsChange(Sender: TObject);
    procedure SetStepIndex(Value: Integer);
    procedure SetSteps(Value: TStrings);
    procedure SetTransparent(Value: Boolean);
  protected
    procedure AdjustSize; override;
    function StepFromPoint(const Point: TPoint): Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure PaintBitmap(Bitmap: TFastBitmap; out FinalSize: TSize); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property StepIndex: Integer read FStepIndex write SetStepIndex;
    property Steps: TStrings read FSteps write SetSteps;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property OnStep: TNotifyEvent read FOnStep write FOnStep;
    property OnStepClick: TStepClickEvent read FOnStepClick write FOnStepClick;
    property AutoSize;
    property Anchors;
    property Color;
    property ParentColor;
    property Cursor;
    property Enabled;
    property Font;
    property ParentFont;
    property Visible;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
  end;

implementation

constructor TStepBubbles.Create(AOwner: TComponent);
var
  S: TStringList;
begin
  inherited Create(AOwner);
  S := TStringList.Create;
  S.Add('Step one');
  S.Add('Step two');
  S.Add('Step three');
  S.Add('Done');
  S.OnChange := StepsChange;
  FSteps := S;
  FStepIndex := 0;
  FDownStepIndex := -1;
  FHotStepIndex := -1;
  SetLength(FStepRects, FSteps.Count);
  FBoldFont := NewFont('Arial Black', 16, 0);
  AutoSize := True;
end;

destructor TStepBubbles.Destroy;
begin
  FSteps.Free;
  inherited Destroy;
end;

function TStepBubbles.StepFromPoint(const Point: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FStepRects) to High(FStepRects) do
    if PtInRect(FStepRects[I], Point) then
    begin
      Result := I;
      Break;
    end;
end;

procedure TStepBubbles.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    I := StepFromPoint(Point(X, Y));
    if FDownStepIndex <> I then
    begin
      FDownStepIndex := I;
      if FHotTrack then
        Invalidate;
    end;
  end;
end;

procedure TStepBubbles.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  I := StepFromPoint(Point(X, Y));
  if FHotStepIndex <> I then
  begin
    FHotStepIndex := I;
    if FHotTrack then
      Invalidate;
  end;
end;

procedure TStepBubbles.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I, J: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (FDownStepIndex > -1) then
  begin
    I := StepFromPoint(Point(X, Y));
    J := FDownStepIndex;
    FDownStepIndex := -1;
    if FHotTrack then
      Invalidate;
    if (J = I) and Assigned(FOnStepClick) then
      FOnStepClick(Self, I);
  end;
end;

procedure DrawBubbleArrow(G: IGdiGraphics; F, B: IGdiFont; const Caption: string;
  Step: Integer; const Line: TVertexLine; Width: Float; Fore, Back: TColor);
var
  Angle: Single;
  P: IGdiGraphicsPath;
begin
  P := NewGraphicsPath;
  GdiArrowCirclePath(P, Line.A, Line.B, Width - 2);
  GdiFillAndStrokePath(G, P, NewColor(Back), NewColor(Fore), 2);
  P := NewGraphicsPath;
  GdiArrowCirclePath(P, Line.A, Line.B, Width);
  GdiStrokePath(G, P, NewColor(Back), 2);
  Angle := 90 - VertexAngle(VertexSubtract(Line.B, Line.A));
  if Angle < 0 then
    Angle := 360 + Angle;
  if (Angle > 90) and (Angle < 270) then
    Angle := Angle - 180;
  GdiDrawTextAngle(G, F, NewColor(Fore), Caption, TPointF(VertexAverage(Line.A, Line.B)), StringAlignmentCenter, StringAlignmentCenter, Angle);
  GdiDrawText(G, B, NewColor(Fore), IntToStr(Step + 1), TPointF(Line.A), StringAlignmentCenter, StringAlignmentCenter);
end;

procedure DrawBubble(G: IGdiGraphics; F: IGdiFont; const Caption: string;
  const Line: TVertexLine; Width: Float; Fore, Back: TColor);
var
  Angle: Single;
  P: IGdiGraphicsPath;
begin
  P := NewGraphicsPath;
  GdiLinePath(P, Line.A, Line.B, Width - 2, True);
  GdiFillAndStrokePath(G, P, NewColor(Back), NewColor(Fore), 2);
  P := NewGraphicsPath;
  GdiLinePath(P, Line.A, Line.B, Width, True);
  GdiStrokePath(G, P, NewColor(Back), 2);
  Angle := 90 - VertexAngle(VertexSubtract(Line.B, Line.A));
  if Angle < 0 then
    Angle := 360 + Angle;
  if (Angle > 90) and (Angle < 270) then
    Angle := Angle - 180;
  GdiDrawTextAngle(G, F, NewColor(Fore), Caption, TPointF(VertexAverage(Line.A, Line.B)),
    StringAlignmentCenter, StringAlignmentCenter, Angle);
end;

type
  TQuad = array[0..3] of Byte;

function Blend(ForeColor: TColor; BackColor: TColor; Percent: Byte = 50): TColor;
var
  RGB: TQuad absolute Result;
  F: TQuad;
  B: TQuad;
begin
  F := TQuad(ColorToRGB(ForeColor));
  B := TQuad(ColorToRGB(BackColor));
  RGB[0] := (F[0] * Percent div 100) + (B[0] * (100 - Percent) div 100);
  RGB[1] := (F[1] * Percent div 100) + (B[1] * (100 - Percent) div 100);
  RGB[2] := (F[2] * Percent div 100) + (B[2] * (100 - Percent) div 100);
  RGB[3] := 0;
end;

procedure TStepBubbles.PaintBitmap(Bitmap: TFastBitmap; out FinalSize: TSize);
const
  ArrowWidth = 20;
  Offset = 32;
var
  G: IGdiGraphics;
  F: IGdiFont;
  V: TVertex;
  Lines: TVertexLines;
  Fore, Back: TColor;
  I: Integer;
begin
  G := NewGraphics(Bitmap.DC);
  F := NewFont(Font.Handle);
  SetLength(Lines, FSteps.Count);
  V.X := -ArrowWidth div 2 + 2;
  V.Y := ArrowWidth + 2;
  for I := 0 to FSteps.Count - 1 do
  begin
    V.X := V.X + ArrowWidth + ArrowWidth div 2;
    Lines[I].A := V;
    FStepRects[I].TopLeft := Point(Round(V.X), Round(V.Y - ArrowWidth div 2));
    if I = FSteps.Count - 1 then
      V.X := V.X + GdiMeasureText(G, FBoldFont, FSteps[I]).Width + Offset
    else
      V.X := V.X + GdiMeasureText(G, F, FSteps[I]).Width + Offset;
    Lines[I].B := V;
    FStepRects[I].BottomRight := Point(Round(V.X), Round(V.Y + ArrowWidth div 2));
    if I = FSteps.Count - 1 then
    begin
      Lines[I].A := Vertex(Lines[I].A.X - ArrowWidth div 4, Lines[I].A.Y);
      Lines[I].B := Vertex(Lines[I].B.X - ArrowWidth - Offset, Lines[I].B.Y);
      FStepRects[I].TopLeft := Point(Round(Lines[I].A.X), Round(Lines[I].A.Y - ArrowWidth));
      FStepRects[I].BottomRight := Point(Round(Lines[I].B.X), Round(Lines[I].B.Y + ArrowWidth));
    end;
  end;
  for I := FSteps.Count - 1 downto 0 do
  begin
    Fore := clHighlightText;
    Back := Blend(clHighlight, clHighlightText, 80);
    if I = FStepIndex then
      Back := Blend(clHighlight, clWindowText, 80);
    if FHotTrack then
    begin
      if I > FStepIndex then
        if I = FHotStepIndex then
          if I = FDownStepIndex then
            Back := Blend(cl3DDkShadow, clWindowText)
          else
            Back := clBtnShadow
        else
          Back := cl3DDkShadow
      else if I = FHotStepIndex then
        if I = FDownStepIndex then
          Back := Blend(clHighlight, clWindowText, 70)
        else
          Back := Blend(clHighlight, clHighlightText, 60);
    end
    else if I > FStepIndex then
      Back := cl3DDkShadow;
    if I = FSteps.Count - 1 then
      DrawBubble(G, FBoldFont, FSteps[I], Lines[I], ArrowWidth * 2, Fore, Back)
    else
      DrawBubbleArrow(G, F, FBoldFont, FSteps[I], I, Lines[I], ArrowWidth, Fore, Back);
  end;
  FinalSize.cx := ArrowWidth * 2 + 4;
  FinalSize.cy := ArrowWidth * 2 + 4;
  I := Length(FStepRects) - 1;
  if I > -1 then
    FinalSize.cx := FStepRects[I].Right + ArrowWidth + 2;
end;

procedure TStepBubbles.Paint;
var
  Bitmap: TFastBitmap;
  DC: HDC;
  S: TSize;
begin
  if (Width < 1) or (Height < 1) then Exit;
  Bitmap := CreateFastBitmap(Width, Height, pd32);
  try
    PaintBitmap(Bitmap, S);
    if not FTransparent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ClientRect);
    end;
    DC := Canvas.Handle;
    AlphaDraw(DC, 0, 0, Bitmap);
  finally
    DestroyFastBitmap(Bitmap);
  end;
end;

procedure TStepBubbles.AdjustSize;
var
  Bitmap: TFastBitmap;
  S: TSize;
begin
  if not (csLoading in ComponentState) then
  begin
    if AutoSize then
    begin
      Bitmap := CreateFastBitmap(1, 1, pd32);
      try
        PaintBitmap(Bitmap, S);
      finally
        DestroyFastBitmap(Bitmap);
      end;
    end
    else
    begin
      S.cx := Width;
      S.cy := Height;
    end;
    SetBounds(Left, Top, S.cx, S.cy);
  end;
end;

procedure TStepBubbles.StepsChange(Sender: TObject);
var
  I: Integer;
begin
  FDownStepIndex := -1;
  FHotStepIndex := -1;
  StepIndex := FStepIndex;
  SetLength(FStepRects, FSteps.Count);
  for I := Low(FStepRects) to High(FStepRects) do
    FStepRects[I] := Rect(0, 0, 0, 0);
  if AutoSize then
    AdjustSize;
  Invalidate;
end;

procedure TStepBubbles.SetStepIndex(Value: Integer);
begin
  if Value < -1 then
    Value := -1
  else if Value > FSteps.Count - 1 then
    Value := FSteps.Count - 1;
  if Value <> FStepIndex then
  begin
    FStepIndex := Value;
    if Assigned(FOnStep) then
      FOnStep(Self);
    Invalidate;
  end;
end;

procedure TStepBubbles.SetSteps(Value: TStrings);
begin
  FSteps.Assign(Value);
  StepIndex := -1;
  Invalidate;
end;

procedure TStepBubbles.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque] 
    else
      ControlStyle := ControlStyle + [csOpaque];
    FTransparent := Value;
    Invalidate;
  end;
end;

end.
