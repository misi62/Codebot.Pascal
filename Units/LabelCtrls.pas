unit LabelCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  BaseTypes, GraphTools, GdiPlus, GdiIntf;

type
  TDragRectControl = class(TWinControl)
  private
    FOnMoved: TNotifyEvent;
    FImage: IGdiImage;
    procedure WMNCLButtonDblClick(var Message: TMessage); message WM_NCLBUTTONDBLCLK ;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMWindowPosChanged(var Message: TMessage); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetImage(Value: IGdiImage);
  public
    constructor Create(AOwner: TComponent); override;
    property Image: IGdiImage read FImage write SetImage;
  published
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  end;

  TWrapBlock = record
    Rect: TRect;
    Index: Integer;
    Start: Integer;
    Length: Integer;
  end;

  TWrapLine = array of TWrapBlock;
  TWrapLines = array of TWrapLine;
  TMargins = TRects;

  TApplyMarginsEvent = procedure(Sender: TObject; var Margins: TMargins) of object;

  TWrapLabel = class(TGraphicControl)
  private
    FLines: TWrapLines;
    FStrings: TStrings;
    FOnApplyMargins: TApplyMarginsEvent;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddMargin(var Margins: TMargins; const Rect: TRect); overload;
    procedure AddMargin(var Margins: TMargins; Control: TControl; Border: Integer = 0); overload;
    procedure DefaultHandler(var Message); override;
    procedure Change;
  published
    property Align;
    property Anchors;
    property Caption;
    property Visible;
    property Font;
    property OnApplyMargins: TApplyMarginsEvent read FOnApplyMargins write FOnApplyMargins;
  end;

implementation

{ TDragRectControl }

constructor TDragRectControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  Color := clRed;
  Width := 100;
  Height :=  100;
end;

procedure TDragRectControl.SetImage(Value: IGdiImage);
begin
  FImage := Value;
  Width := FImage.Width shr 1;
  Height := FImage.Height shr 1;
end;

procedure TDragRectControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRectI;
  G: IGdiGraphics;
  B: IGdiBrush;
begin
  R := NewRectI(Width, Height);
  G := NewGraphics(R.Width, R.Height);
  if FImage <> nil then
  begin
    G.Clear(aclWhite);
    G.DrawImage(FImage, R);
  end
  else
  begin
  B := NewLinearGradientBrush(NewRectF(R), 60, NewColor(clWindow), NewColor(clSilver));
  G.FillRectangle(B, R);
  B := NewHatchBrush(HatchStyleDiagonalBrick, NewColor(clBlack));
  G.FillRectangle(B, R);
  Dec(R.Width);
  Dec(R.Height);
  G.DrawRectangle(NewPen(NewColor(cl3DDkShadow)), R);
  end;
  G.Draw(Message.DC, 0, 0);
  Message.Result := 1;
end;

procedure TDragRectControl.WMNCHitTest(var Message: TWMNCHitTest);
const
  Grip = 20;
var
  P: TPoint;
begin
  with Message do
  begin
    P := ScreenToClient(Point(XPos, YPos));
    if (P.X > Width - Grip) and (P.Y > Height - Grip) then
      Result := HTBOTTOMRIGHT
    else
      Result := HTCAPTION;
  end;
end;

procedure TDragRectControl.WMNCLButtonDblClick(var Message: TMessage);
begin
  inherited;
  SendToBack;
end;

procedure TDragRectControl.WMWindowPosChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMoved) then
    FOnMoved(Self)
end;

{ TWrapLabel }

constructor TWrapLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStrings := TStringList.Create;
  ControlStyle := ControlStyle - [csOpaque];
end;

destructor TWrapLabel.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

procedure TWrapLabel.AddMargin(var Margins: TMargins; const Rect: TRect);
begin
  SetLength(Margins, Length(Margins) + 1);
  Margins[Length(Margins) - 1] := Rect;
end;

procedure TWrapLabel.AddMargin(var Margins: TMargins; Control: TControl; Border: Integer = 0);
var
  R: TRect;
  W, H: Integer;
  C, S: TPoint;
begin
  W := Control.Width;
  H := Control.Height;
  C := Control.ClientToScreen(Point(0, 0));
  S := ClientToScreen(Point(0, 0));
  C.X := C.X - S.X;
  C.Y := C.Y - S.Y;
  R := Rect(C.X - Border, C.Y - Border, C.X + W + Border, C.Y + H + Border);
  SetLength(Margins, Length(Margins) + 1);
  Margins[Length(Margins) - 1] := R;
end;

procedure TWrapLabel.DefaultHandler(var Message);
var
  S: string;
  P: PChar;
begin
  with TMessage(Message) do
    case Msg of
      WM_GETTEXT:
        begin
          S := FStrings.Text;
          P := PChar(S);
          Result := StrLen(StrLCopy(PChar(LParam), P, WParam - 1));
        end;
      WM_GETTEXTLENGTH:
        Result := Length(FStrings.Text);
      WM_SETTEXT:
        begin
          FStrings.Text := StrPas(PChar(LParam));
          SendDockNotification(Msg, WParam, LParam);
          Change;
        end;
    else
      inherited;
    end;
end;

procedure TWrapLabel.Change;
begin
  FLines := nil;
  Invalidate;
end;

procedure TWrapLabel.Resize;
begin
  inherited Resize;
  Change;
end;

type
  TMarginRange = record
    Left: Integer;
    Right: Integer;
  end;

  TMarginRanges = array of TMarginRange;

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

procedure SortRanges(var Ranges: TMarginRanges);
var
  Swapped: Boolean;
  Swap: TMarginRange;
  I: Integer;
begin
  repeat
    Swapped := False;
    for I := Low(Ranges) to High(Ranges) - 1 do
    begin
      Swapped := Ranges[I].Left > Ranges[I + 1].Left;
      if Swapped then
      begin
        Swap := Ranges[I];
        Ranges[I] := Ranges[I + 1];
        Ranges[I + 1] := Swap;
        Break;
      end;
    end;
  until not Swapped;
end;

function CombineRange(const A, B: TMarginRange; out Merge: TMarginRange): Boolean;
begin
  Result := ((A.Right >= B.Left) and (A.Left <= B.Right)) or
    ((B.Right >= A.Left) and (B.Left <= A.Right));
  Merge.Left := Min(A.Left, B.Left);
  Merge.Right := Max(A.Right, B.Right);
end;

function CombineRanges(const Ranges: TMarginRanges): TMarginRanges;
var
  Merge: TMarginRange;
  Count: Integer;
  I, J: Integer;
begin
  Result := Ranges;
  Count := Length(Ranges);
  for I := 0 to Count - 1 do
      for J := I + 1 to Count - 1 do
      if CombineRange(Ranges[I], Ranges[J], Merge) then
      begin
        if J < Count - 1 then
          Move(Pointer(@Ranges[J])^, Pointer(@Result[I])^, (Count - J) * SizeOf(TMarginRange));
        Result[I] := Merge;
        SetLength(Result, Count - 1);
        Result := CombineRanges(Result);
        Exit;
      end;
  SortRanges(Result);
end;

function RangesToLine(const Ranges: TMarginRanges; Rect: TRect): TWrapLine;
var
  Skip: Boolean;
  I, J: Integer;
begin
  I := Length(Ranges) + 1;
  Skip := Ranges[Length(Ranges) - 1].Right >= Rect.Right;
  if Skip then
    Dec(I);
  Skip := Ranges[0].Left <= Rect.Left;
  if Ranges[0].Left <= Rect.Left then
    Dec(I);
  Result := nil;
  if I < 1 then
    Exit;
  J := -1;
  if Skip then
    Inc(J);
  SetLength(Result, I);
  for I := Low(Result) to High(Result) do
  begin
    Result[I].Rect := Rect;
    Result[I].Index := -1;
    Result[I].Start := 0;
    Result[I].Length := 0;
    if I + J > -1 then
      Result[I].Rect.Left := Ranges[I + J].Right;
    if I < High(Result) then
      Result[I].Rect.Right := Ranges[I + J + 1].Left
    else if Ranges[Length(Ranges) - 1].Right >= Rect.Right then
      Result[I].Rect.Right := Ranges[High(Result) + J + 1].Left;
  end;
end;

procedure AddRange(var Ranges: TMarginRanges; Left, Right: Integer);
begin
  SetLength(Ranges, Length(Ranges) + 1);
  Ranges[Length(Ranges) - 1].Left := Left;
  Ranges[Length(Ranges) - 1].Right := Right;
end;

procedure TWrapLabel.Paint;

  procedure ApplyMargins;
  var
    Margins: TMargins;
    Ranges: TMarginRanges;
    R: TRect;
    I, J: Integer;
  begin
    Margins := nil;
    { Request margins in client space }
    if Assigned(FOnApplyMargins) then
      FOnApplyMargins(Self, Margins);
    { If there are no margins to be applied then exit }
    if Length(Margins) = 0 then Exit;
    for I := Low(FLines) to High(FLines) do
    begin
      Ranges := nil;
      R := FLines[I, 0].Rect;
      for J := Low(Margins) to High(Margins) do
      begin
        { Skip lines which do not intersect margins }
        if Margins[J].Right < 0 then Continue;
        if Margins[J].Left >= Width then Continue;
        if Margins[J].Bottom < R.Top then Continue;
        if Margins[J].Top > R.Bottom  then Continue;
        { We have an intersection }
        AddRange(Ranges, Margins[J].Left, Margins[J].Right);
      end;
      { If no margins were found in the line, skip to the next line }
      if Length(Ranges) = 0 then Continue;
      { Combine and sort margin ranges }
      Ranges := CombineRanges(Ranges);
      { Define our wrap blocks for this line given the margin ranges }
      FLines[I] := RangesToLine(Ranges, R);
    end;
  end;

  procedure ApplyWrap(DC: HDC);
  var
    Index, LineIndex, Chars, CharIndex, Written: Integer;
    Paragraph: string;
    Widths: TIntegerArray;
    Size: TSize;
    I, J: Integer;
  begin
    LineIndex := 0;
    for Index := 0 to FStrings.Count - 1 do
    begin
      Paragraph := FStrings[Index];
      Chars := Length(Paragraph);
      CharIndex := 1;
      SetLength(Widths, Chars);
      for I := LineIndex to High(FLines) do
      begin
        Inc(LineIndex);
        for J := Low(FLines[I]) to High(FLines[I]) do
        begin
          if WidthOf(FLines[I, J].Rect) < 2 then Continue;
          if Chars < 1 then Break;
          GetTextExtentExPoint(DC, PChar(@Paragraph[CharIndex]), Chars,
            WidthOf(FLines[I, J].Rect), @Written, @Widths[0], Size);
          if Chars - Written > 0 then
            while (Written > 0) and (Paragraph[CharIndex + Written - 1] > ' ') do
              Dec(Written);
          FLines[I, J].Index := Index;
          FLines[I, J].Start := CharIndex;
          FLines[I, J].Length := Written;
          Inc(CharIndex, Written);
          Dec(Chars, Written);
        end;
        if Chars < 1 then Break;
      end;
    end;
  end;

const
  Flags = DT_LEFT or DT_VCENTER or DT_NOCLIP or DT_SINGLELINE or DT_END_ELLIPSIS ;
var
  DC: HDC;
  R: TRect;
  F: HFONT;
  H: Integer;
  B: TWrapBlock;
  S: string;
  I, J, K: Integer;
begin
  DC := Canvas.Handle;
  R := ClientRect;
  // FillRectOutline(DC, R, clBtnShadow, psDot);
  { Select the curent font }
  F := SelectObject(DC, Font.Handle);
  H := FontHeight(DC);
  { Count the renderable lines }
  I := Height div H;
  { If no lines can be rendered then exit }
  if I < 1 then
  begin
    SelectObject(DC, F);
    Exit;
  end;
  { If the lines have changed }
  if Length(FLines) <> I then
  begin
    R.Bottom := H;
    { Calculate the lines }
    SetLength(FLines, I);
    for I := Low(FLines) to High(FLines) do
    begin
      SetLength(FLines[I], 1);
      FLines[I, 0].Rect := R;
      FLines[I, 0].Index := -1;
      FLines[I, 0].Start := 0;
      FLines[I, 0].Length := 0;
      R.Top := R.Bottom;
      R.Bottom := R.Top + H;
    end;
    ApplyMargins;
    ApplyWrap(DC);
  end;
  { Draw our lines }
  SetBkMode(DC, TRANSPARENT);
  S := '';
  K := -1;
  for I := Low(FLines) to High(FLines) do
    for J := Low(FLines[I]) to High(FLines[I]) do
    begin
      B := FLines[I, J];
      if WidthOf(B.Rect) < 2 then Continue;
      // FillRectOutline(DC, B.Rect, clBtnShadow, psDot);
      if B.Index < 0 then Continue;
      if K <> B.Index then
      begin
        K := B.Index;
        S := FStrings[K];
      end;
      if B.Start > 0 then
        if (I = High(FLines)) and (J = High(FLines[I])) then
          DrawTextEx(DC, PChar(@S[B.Start]), -1, B.Rect, Flags, nil)
        else
          DrawTextEx(DC, PChar(@S[B.Start]), B.Length, B.Rect, Flags, nil);
    end;
  SelectObject(DC, F);
end;

procedure TWrapLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Change;
end;

end.
