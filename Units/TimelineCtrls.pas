unit TimeLineCtrls;

(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

interface

uses
  Windows, Messages, SysUtils, Classes;


{ TTimelineBarItems }




type
  TTimelineBarItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TTimelineBarItem;
    procedure Put(Index: Integer; Value: TTimelineBarItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TTimelineBarItem;
    function FindItemID(ID: Integer): TTimelineBarItem;
    function Insert(Index: Integer): TTimelineBarItem;
    property Items[Index: Integer]: TTimelineBarItem read Get write Put; default;
  end;

{ TTimelineBar }

  TTimelineBar = class(TProviderWindow)
  private
    FBitmap: TFastBitmap;
    FBarMargin: Integer;
    FBarHeight: Integer;
    FItems: TTimelineBarItems;
    FPosition: Single;
    FHotIndex: Integer;
    FDownIndex: Integer;
    FSplitIndex: Integer;
    FSplitHotIndex: Integer;
    FSelectedIndex: Integer;
    FStep: Single;
    FOnPositionChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FOnSplit: TNotifyEvent;
    function GetMaxDuration: Single;
    function GetMaxWidth: Single;
    function GetTotalTime: Single;
    procedure SetBarMargin(Value: Integer);
    procedure SetBarHeight(Value: Integer);
    procedure SetItems(Value: TTimelineBarItems);
    procedure SetPosition(Value: Single);
    procedure SetHotIndex(Value: Integer);
    procedure SetDownIndex(Value: Integer);
    procedure SetSplitHotIndex(Value: Integer);
    procedure SetSplitIndex(Value: Integer);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetStep(Value: Single);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure Sort;
    procedure ItemsChanged; virtual;
    function SplitFromPoint(const Point: TPoint): Integer;
    function ItemFromPoint(const Point: TPoint): TTimelineBarItem;
    function PositionFromPoint(const Point: TPoint): Single;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property MaxDuration: Single read GetMaxDuration;
    property MaxWidth: Single read GetMaxWidth;
    property HotIndex: Integer read FHotIndex write SetHotIndex;
    property DownIndex: Integer read FDownIndex write SetDownIndex;
    property SplitHotIndex: Integer read FSplitHotIndex write SetSplitHotIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Split;
    procedure Merge;
    property SplitIndex: Integer read FSplitIndex write SetSplitIndex;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property TotalTime: Single read GetTotalTime;
  published
    property BarMargin: Integer read FBarMargin write SetBarMargin;
    property BarHeight: Integer read FBarHeight write SetBarHeight;
    property Items: TTimelineBarItems read FItems write SetItems;
    property Step: Single read FStep write SetStep;
    property Position: Single read FPosition write SetPosition;
    property OnPositionChange: TNotifyEvent read FOnPositionChange write FOnPositionChange;
    property OnSplit: TNotifyEvent read FOnSplit write FOnSplit;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Align;
    property Color;
    property ParentColor;
  end;

implementation

const
  MinDelta = 1.0E-8;

{ TTimelineBarItem }

constructor TTimelineBarItem.Create(Collection: TCollection);
begin
	inherited Create(Collection);
end;

procedure TTimelineBarItem.Assign(Source: TPersistent);
var
  EditItem: TTimelineBarItem absolute Source;
begin
  if Source is TTimelineBarItem then
  begin
    if EditItem.FStart < 0 then
      FStart := 0
    else
      FStart := EditItem.FStart;
    if EditItem.FDuration < 0 then
      FDuration := 0
    else
      FDuration := EditItem.FDuration;
    FState := EditItem.FState;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TTimelineBarItem.SetStart(Value: Single);
begin
  if FStart < 0 then FStart := 0;
  if FStart <> Value then
  begin
    FStart := Value;
    Changed(False);
  end;
end;

procedure TTimelineBarItem.SetDuration(Value: Single);
begin
  if FDuration < 0 then FDuration := 0;
  if FDuration <> Value then
  begin
    FDuration := Value;
    Changed(False);
  end;
end;

procedure TTimelineBarItem.SetState(Value: Integer);
begin
  if FState <> Value then
  begin
    FState := Value;
    Changed(False);
  end;
end;

{ TTimelineBarItems }

constructor TTimelineBarItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTimelineBarItem);
end;

function TTimelineBarItems.Add: TTimelineBarItem;
begin
  Result := TTimelineBarItem(inherited Add);
end;

function TTimelineBarItems.FindItemID(ID: Integer): TTimelineBarItem;
begin
  Result := TTimelineBarItem(inherited FindItemID(ID));
end;

function TTimelineBarItems.Insert(Index: Integer): TTimelineBarItem;
begin
  Result := TTimelineBarItem(inherited Insert(Index));
end;

procedure TTimelineBarItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TTimelineBar then
  	TTimelineBar(GetOwner).ItemsChanged;
end;

function TTimelineBarItems.Get(Index: Integer): TTimelineBarItem;
begin
  Result := TTimelineBarItem(GetItem(Index));
end;

procedure TTimelineBarItems.Put(Index: Integer; Value: TTimelineBarItem);
begin
  SetItem(Index, Value);
end;

{ TTimelineBar }

constructor TTimelineBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBarHeight := 32;
  FBarMargin := 8;
  FHotIndex := -1;
  FDownIndex := -1;
  FSelectedIndex := -1;
  FSplitHotIndex := -1;
  FSplitIndex := -1;
  FItems := TTimelineBarItems.Create(Self);
  FItems.Add.FDuration := 100;
  Width := 512;
  Height := 64;
end;

destructor TTimelineBar.Destroy;
begin
  DestroyFastBitmap(FBitmap);
  FItems.Free;
  inherited Destroy;
end;

procedure TTimelineBar.Split;
var
  T, N: TTimelineBarItem;
  W: Single;
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    T := Items[I];
    if T.FStart >= Position then
      Continue;
    if T.FStart + T.FDuration <= Position then
      Continue;
    W := T.FDuration;
    T.FDuration := Position - T.FStart;
    N := Items.Add;
    N.FStart := Position;
    N.FDuration := W - T.FDuration;
    Invalidate;
    Sort;
    SplitIndex := N.Index - 1;
    {if Assigned(FOnSplit) then
      FOnSplit(Self);}
    Break;
  end;
end;

procedure TTimelineBar.Merge;
var
  T, M: TTimelineBarItem;
  D, S: Single;
  I: Integer;
begin
  if SplitIndex < 0 then Exit;
  T := Items[SplitIndex];
  S := T.FStart + T.FDuration;
  D := 0;
  Items.BeginUpdate;
  try
    for I := Items.Count - 1 downto 0 do
    begin
      M := Items[I];
      if M.FStart = S then
      begin
        if M.FDuration > D then
          D := M.FDuration;
        M.Free;
      end;
    end;
    T.FDuration := T.FDuration + D;
  finally
    Items.EndUpdate;
  end;
  SplitIndex := -1;
  SplitHotIndex := -1;
end;

function CompareItems(Item1, Item2: Pointer): Integer;
var
  A: TTimelineBarItem absolute Item1;
  B: TTimelineBarItem absolute Item2;
  T: Single;
begin
  T := A.Start - B.Start;
  if T > 0 then
    Result := 1
  else if T < 0 then
    Result := -1
  else
    Result := 0;
end;

procedure TTimelineBar.Sort;
var
  L: TList;
  I: Integer;
begin
  Items.BeginUpdate;
  L := TList.Create;
  try
    for I := 0 to Items.Count - 1 do
      L.Add(Items[I]);
    L.Sort(CompareItems);
    for I := 0 to Items.Count - 1 do
      TCollectionItem(L[I]).Index := I;
  finally
    L.Free;
    Items.EndUpdate;
  end;
end;

procedure TTimelineBar.Paint;
const
  DisableOpacity = $50;

    function DrawState(Graphics: IGdiGraphics; const Rect: TRectF; State: TDrawState): IGdiPen;
    var
      Pen: IgdiPen;
      C: TArgb;
    begin
      C := NewColor(Blend(clHighlight, 0, 70));
      if dsDisabled in State then
        C := NewOpacity(C, DisableOpacity);
      Pen := NewPen(C);
      Graphics.DrawRectangle(Pen, Rect);
    end;

    function FillState(Graphics: IGdiGraphics; const Rect: TRectF; State: TDrawState): IGdiBrush;
    var
      Brush: IGdiBrUsh;
      A, B: TArgb;
      C: TColor;
    begin
      if dsHot in State then
        if dsPressed in State then
        begin
          C := Blend(clHighlight, clBtnShadow, 80);
          A := NewColor(C);
          B := A;
        end
        else
        begin
          C := Blend(clHighlight, clBtnFace);
          A := NewColor(C);
          B := NewColor(Blend(clHighlight, clWindow, 20));
        end
      else if dsSelected in State then
      begin
        C := Blend(clHighlight, clBtnFace);
        A := NewColor(C);
        B := A;
      end
      else
      begin
        C := Blend(clHighlight, clBtnFace);
        A := NewColor(C);
        B := A;
      end;
      if dsDisabled in State then
      begin
        SetOpacity(A, DisableOpacity);
        SetOpacity(B, DisableOpacity);
      end;
      Brush := NewLinearGradientBrush(Rect, 90, A, B);
      Graphics.FillRectangle(Brush, Rect);
      {if dsSelected in State then
      begin
        A := aclRed;
        if dsDisabled in State then
          SetOpacity(A, DisableOpacity);
        Brush := NewSolidBrush(A); //(HatchStyleBackwardDiagonal, A);
        Graphics.FillRectangle(Brush, Rect);
      end;}
    end;

var
  State: TDrawState;
  G: IGdiGraphics;
  B: IGdiBrush;
  P: IGdiPen;
  R: TRectF;
  D: Single;
  W: Single;
  T: TTimelineBarItem;
  I: Integer;
begin
  if (Width < 1) or (Height < 1) then Exit;
  { Create our drawing buffer bitmap }
  if (FBitmap.Width <> Width) or (FBitmap.Height <> Height) then
  begin
    DestroyFastBitmap(FBitmap);
    FBitmap := CreateFastBitmap(Width, Height, pd32);
  end;
  try
    R := NewRectF(0, 0, FBitmap.Width, FBitmap.Height);
    G := NewGraphics(FBitmap.DC);
    G.Clear(NewColor(Color));
    { Draw the background }
    // B := NewHatchBrush(HatchStyleBackwardDiagonal, NewColor(Blend(Color, clBlack, 90)));
    // G.FillRectangle(B, R);
    B := NewLinearGradientBrush(R, 90,
      [NewColor(Blend(Color, clBlack, 70)), NewColor(Color, 0), NewColor(Color, 0), NewColor(Blend(Color, clBlack, 90))],
      [0, 0.4, 0.9, 1]);
    G.FillRectangle(B, R);
    { Get the max timeline duration }
    D := MaxDuration;
    if D = 0 then
      Exit;
    { Get the max timeline width in pixels }
    W := MaxWidth;
    if W < 1 then
      Exit;
    { Calculate the vertical portion of the bar rect }
    R.Y := FBitmap.Height div 2 - FBarHeight div 2;
    R.Height := FBarHeight;
    { Draw the bar items }
    for I := 0 to Items.Count - 1 do
    begin
      T := Items[I];
      R.X := FBarMargin + Round(W * (T.FStart / D));
      R.Width := Round(W * (T.FDuration / D)) - 1;
      if R.Width < 1 then Continue;
      { We assume that state > 0 means the item is in some disabled state }
      State := [];
      if T.Index = FSelectedIndex then
        Include(State, dsSelected);
      if T.State > 0 then
       Include(State, dsDisabled);
      if I = FHotIndex then
        Include(State, dsHot);
      if I = FDownIndex then
        Include(State, dsPressed);
      DrawState(G, R, State);
      FillState(G, R, State);
    end;
    if FSelectedIndex > -1 then
    begin
      T := Items[FSelectedIndex];
      R.X := FBarMargin + Round(W * (T.FStart / D));
      R.Width := Round(W * (T.FDuration / D)) - 1;
      G.DrawRectangle(NewPen(NewColor(Blend(clYellow, 0, 85)), 3), R);
    end;
    if FSplitIndex > -1 then
    begin
      T := Items[FSplitIndex];
      R.X := FBarMargin + Round(W * ((T.FStart + T.FDuration) / D)) - 1;
      R.Width := 3;
      G.FillRectangle(NewSolidBrush(aclYellow), R);
    end;
    if FSplitHotIndex > -1 then
    begin
      T := Items[FSplitHotIndex];
      R.X := FBarMargin + Round(W * ((T.FStart + T.FDuration) / D)) - 1;
      R.Width := 3;
      G.FillRectangle(NewSolidBrush(aclYellow), R);
    end;
    R.Y := 0;
    R.Height := FBitmap.Height;
    P := NewPen(NewOpacity(aclBlack, $70), 3);
    R.X := FBarMargin + Round(W * (Position / D)) - 1;
    G.DrawLine(P, R.X, R.Y, R.X, R.Height);
    P := NewPen(NewOpacity(aclWhite, $A0));
    G.DrawLine(P, R.X, R.Y, R.X, R.Height);
  finally
    AlphaDraw(Canvas.Handle, 0, 0, FBitmap);
  end;
end;

procedure TTimelineBar.ItemsChanged;
begin
  Invalidate;
end;

function TTimelineBar.SplitFromPoint(const Point: TPoint): Integer;
var
  D, W: Single;
  T: TTimelineBarItem;
  I, J: Integer;
begin
  Result := -1;
  if Items.Count < 2 then Exit;
  if (Point.X < FBarMargin) or (Point.X > Width - BarMargin) then
    Exit;
  W := Height div 2 - FBarHeight div 2;
  if (Point.Y < W) or (Point.Y > W + FBarHeight) then
    Exit;
  D := MaxDuration;
  if D < 0 then Exit;
  W := MaxWidth;
  if W < 1 then Exit;
  for I := 0 to Items.Count - 2 do
  begin
    T := Items[I];
    J := FBarMargin + Round(W * ((T.FStart + T.FDuration) / D));
    if (Point.X > J - 5) and (Point.X < J + 5) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TTimelineBar.ItemFromPoint(const Point: TPoint): TTimelineBarItem;
var
  D, W: Single;
  T: TTimelineBarItem;
  I, J: Integer;
begin
  Result := nil;
  if (Point.X < FBarMargin) or (Point.X > Width - BarMargin) then
    Exit;
  W := Height div 2 - FBarHeight div 2;
  if (Point.Y < W) or (Point.Y > W + FBarHeight) then
    Exit;
  D := MaxDuration;
  if D < 0 then Exit;
  W := MaxWidth;
  if W < 1 then Exit;
  for I := 0 to Items.Count - 1 do
  begin
    T := Items[I];
    J := FBarMargin + Round(W * (T.FStart / D));
    if (Point.X > J - 1) and (Point.X < J + Round(W * (T.FDuration / D))) then
    begin
      Result := T;
      Break;
    end;
  end;
end;

function TTimelineBar.PositionFromPoint(const Point: TPoint): Single;
var
  D, W: Single;
begin
  D := MaxDuration;
  if D < 0 then Exit;
  W := MaxWidth;
  Result := 0;
  if Point.X < FBarMargin then
    Exit;
  Result := D;
  if Point.X > Width - FBarMargin then
    Exit;
  Result := D * ((Point.X - FBarMargin) / W);
end;

procedure TTimelineBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  T: TTimelineBarItem;
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    P := Point(X, Y);
    I := SplitFromPoint(P);
    SplitHotIndex := I;
    if SplitHotIndex > -1 then
    begin
      SplitIndex := SplitHotIndex;
      SelectedIndex := -1;
      HotIndex := -1;
      DownIndex := -1;
      Exit;
    end
    else
    begin
      SplitHotIndex := -1;
      SplitIndex := -1;
    end;
    T := ItemFromPoint(P);
    if T <> nil then
    begin
      SelectedIndex := T.Index;
      HotIndex := T.Index;
      DownIndex := T.Index;
      Exit;
    end
    else
    begin
      HotIndex := -1;
      DownIndex := -1;
    end;
    Position := PositionFromPoint(P);
  end;
end;

procedure TTimelineBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  T, N: TTimelineBarItem;
  S, D, M, C: Single;
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  P := Point(X, Y);
  if MouseCapture then
  begin
    if SplitIndex > -1 then
    begin
      T := Items[SplitIndex];
      S := T.FStart + T.FDuration;
      M := MaxDuration;
      for I := 0 to Items.Count - 1 do
      begin
        if I = SplitIndex then
          Continue;
        N := Items[I];
        if N.FStart = S then
        begin
          if N.FStart + N.FDuration < M then
            M := N.FStart + N.FDuration;
        end;
      end;
      C := PositionFromPoint(P);
      if C <= T.FStart then
        C := T.FStart + MinDelta
      else if C > M - MinDelta then
        C := M - MinDelta;
      T.FDuration := C - T.FStart;
      for I := 0 to Items.Count - 1 do
      begin
        if I = SplitIndex then
          Continue;
        N := Items[I];
        if N.FStart = S then
        begin
          M := N.FStart + N.FDuration;
          N.FStart := T.FStart + T.FDuration;
          N.FDuration := M - N.FStart;
        end;
      end;
      Invalidate;
      if Assigned(FOnSplit) then
        FOnSplit(Self);
      Exit;
    end;
    T := ItemFromPoint(P);
    if T <> nil then
    begin
      if T.Index = SelectedIndex then
        HotIndex := SelectedIndex
      else
        HotIndex := -1;
    end
    else
      HotIndex := -1;
    if DownIndex < 0 then
      Position := PositionFromPoint(P);
  end
  else
  begin
    I := SplitFromPoint(P);
    SplitHotIndex := I;
    if SplitHotIndex > -1 then
    begin
      HotIndex := -1;
      DownIndex := -1;
      Exit;
    end;
    T := ItemFromPoint(P);
    if T <> nil then
    begin
      HotIndex := T.Index;
      Exit;
    end
    else
      HotIndex := -1;
  end;
end;

procedure TTimelineBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    DownIndex := -1;
  end;
end;

function TTimelineBar.GetMaxDuration: Single;
var
  T: TTimelineBarItem;
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    T := Items[I];
    if T.FStart + T.FDuration > Result then Result := T.FStart + T.FDuration;
  end;
end;

function TTimelineBar.GetMaxWidth: Single;
begin
  Result := Width - FBarMargin * 2;
end;

procedure TTimelineBar.SetBarMargin(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FBarMargin then
  begin
    FBarMargin := Value;
    Invalidate;
  end;
end;

function TTimelineBar.GetTotalTime: Single;
begin
  Result := GetMaxDuration;
end;

procedure TTimelineBar.SetBarHeight(Value: Integer);
begin
  if Value < 3 then Value := 3;
  if Value <> FBarHeight then
  begin
    FBarHeight := Value;
    Invalidate;
  end;
end;

procedure TTimelineBar.SetItems(Value: TTimelineBarItems);
begin
  Items.Assign(Value);
end;

procedure TTimelineBar.SetPosition(Value: Single);
begin
  if FPosition <> Value then
  begin
    SelectedIndex := -1;
    SplitIndex := -1;
    FPosition := Value;
    Invalidate;
    if Assigned(FOnPositionChange) then
      FOnPositionChange(Self);
  end;
end;

procedure TTimelineBar.SetHotIndex(Value: Integer);
begin
  if Value <> FHotIndex then
  begin
    FHotIndex := Value;
    Invalidate;
  end;
end;

procedure TTimelineBar.SetDownIndex(Value: Integer);
begin
  if Value <> FDownIndex then
  begin
    FDownIndex := Value;
    Invalidate;
  end;
end;

procedure TTimelineBar.SetSelectedIndex(Value: Integer);
begin
  if Value <> FSelectedIndex then
  begin
    FSelectedIndex := Value;
    Invalidate;
    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end;
end;

procedure TTimelineBar.SetSplitHotIndex(Value: Integer);
begin
  if Value <> FSplitHotIndex then
  begin
    FSplitHotIndex := Value;
    if Value > -1 then
      Cursor := crHsplit
    else
      Cursor := crDefault;
    Invalidate;
  end;
end;

procedure TTimelineBar.SetSplitIndex(Value: Integer);
begin
  if Value <> FSplitIndex then
  begin
    SelectedIndex := -1;
    FSplitIndex := Value;
    Invalidate;
    if Assigned(FOnSplit) then
      FOnSplit(Self);
  end;
end;

procedure TTimelineBar.SetStep(Value: Single);
begin
  if Value < 0.001 then
    Value := 0.001;
  if Value <> FStep then
  begin
    FStep := Value;

  end;

end;

procedure TTimelineBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

end.
