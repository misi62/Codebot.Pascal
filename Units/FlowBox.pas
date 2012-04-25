unit FlowBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  BaseTypes, GraphTools;

const
  WM_CHILDFOCUS       = WM_USER + $0AC1A;
  WM_CHILDKILLFOCUS   = WM_CHILDFOCUS + 1;

{ TFlowGroup }

type
  TFlowGroup = class(TCollectionItem)
  private
    FName: string;
    FHelp: string;
    FHeight: Integer;
    FWasVisible: Boolean;
    FVisible: Boolean;
    FColumn: Integer;
    FRect: TRect;
    procedure SetName(const Value: string);
    procedure SetHeight(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write SetName;
    property Help: string read FHelp write FHelp;
    property Height: Integer read FHeight write SetHeight;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TFlowGroups }

  TFlowGroups = class(TOwnedCollection)
  private
    function Get(Index: Integer): TFlowGroup;
    procedure Put(Index: Integer; Value: TFlowGroup);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TFlowGroup;
    function FindGroup(ID: Integer): TFlowGroup; overload;
    function FindGroup(const Name: string): TFlowGroup; overload;
    function Insert(Index: Integer): TFlowGroup;
    property Group[Index: Integer]: TFlowGroup read Get write Put; default;
  end;

{ TFlowBox }

  TRects = array of TRect;

  TDrawGroupEvent = procedure(Sender: TObject; Group: TFlowGroup;
    Rect: TRect; State: TDrawState) of object;

  TGroupChangeEvent = procedure(Sender: TObject; Index: Integer;
    Rect: TRect) of object;

  TFlowBox = class(TCustomControl)
  private
    FActive: Boolean;
    FAllowResize: Boolean;
    FAutoFit: Boolean;
    FBitmap: TFastBitmap;
    FBitmapChanged: Boolean;
    FFocusGroup: Integer;
    FColumnWidth: Integer;
    FPadding: Integer;
    FGroups: TFlowGroups;
    FChanged: Boolean;
    FSizeIndex: Integer;
    FSizePoint: TPoint;
    FSizeMin: Integer;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawGroup: TDrawGroupEvent;
    FOnGroupChange: TGroupChangeEvent;
    function CanSize: Boolean;
    procedure SetAutoFit(Value: Boolean);
    procedure SetColumnWidth(Value: Integer);
    procedure SetPadding(Value: Integer);
    procedure SetGroups(Value: TFlowGroups);
    function GetSizeRects: TRects;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMChildFocus(var Message: TMessage); message WM_CHILDFOCUS;
    procedure WMChildKillFocus(var Message: TMessage); message WM_CHILDKILLFOCUS;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FocusChanged(Control: TWinControl);
    procedure DrawGroup(Group: TFlowGroup);
    procedure GroupChange; virtual;
    procedure Loaded; override;
    function MinHeight(Group: TFlowGroup): Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintBackground(BackgroundDC: HDC); virtual;
    procedure PlaceControls;
    procedure Resize; override;
    procedure UpdateGroups;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    procedure FitHeight;
    procedure FitWidth;
    procedure Invalidate; override;
    function ControlInGroup(Control: TControl; Group: TFlowGroup): Boolean;
  published
    property Align;
    property AllowResize: Boolean read FAllowResize write FAllowResize;
    property Anchors;
    property AutoFit: Boolean read FAutoFit write SetAutoFit;
    property ColumnWidth: Integer read FColumnWidth write SetColumnWidth;
    property Enabled;
    property Groups: TFlowGroups read FGroups write SetGroups;
    property Padding: Integer read FPadding write SetPadding;
    property TabStop;
    property Visible;
    property PopupMenu;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawGroup: TDrawGroupEvent read FOnDrawGroup write FOnDrawGroup;
    property OnGroupChange: TGroupChangeEvent read FOnGroupChange write FOnGroupChange;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
  end;

implementation

{ TFlowGroup }

const
  MinGroupHeight = 8;

constructor TFlowGroup.Create(Collection: TCollection);
begin
	inherited Create(Collection);
  FHeight := 80;
  FVisible := True;
end;

procedure TFlowGroup.Assign(Source: TPersistent);
var
  EditItem: TFlowGroup absolute Source;
begin
  if Source is TFlowGroup then
  begin
    FName := EditItem.Name;
    FHelp :=  EditItem.Help;
    FHeight := EditItem.Height;
    FVisible := EditItem.Visible;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TFlowGroup.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    FName := Value;
    Changed(False);
  end;
end;

procedure TFlowGroup.SetHeight(Value: Integer);
begin
  if Value < MinGroupHeight then
    Value := MinGroupHeight;
  if Value <> FHeight then
  begin
    FHeight := Value;
    Changed(False);
  end;
end;

procedure TFlowGroup.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

{ TFlowGroups }

constructor TFlowGroups.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TFlowGroup);
end;

function TFlowGroups.Add: TFlowGroup;
begin
  Result := TFlowGroup(inherited Add);
end;

function TFlowGroups.FindGroup(ID: Integer): TFlowGroup;
begin
  Result := TFlowGroup(FindItemID(ID));
end;

function TFlowGroups.FindGroup(const Name: string): TFlowGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Get(I).Name = Name then
    begin
      Result := Get(I);
      Break;
    end;
end;

function TFlowGroups.Insert(Index: Integer): TFlowGroup;
begin
  Result := TFlowGroup(inherited Insert(Index));
end;

procedure TFlowGroups.Update(Item: TCollectionItem);
begin
  if GetOwner is TFlowBox then
  	TFlowBox(GetOwner).UpdateGroups;
end;

function TFlowGroups.Get(Index: Integer): TFlowGroup;
begin
  Result := TFlowGroup(GetItem(Index));
end;

procedure TFlowGroups.Put(Index: Integer; Value: TFlowGroup);
begin
  SetItem(Index, Value);
end;

{ TFlowBox }

function RectInRect(const A, B: TRect): Boolean;
begin
  Result := (B.Top >= A.Top) and (B.Left >= A.Left) and
    (B.Right <= A.Right) and (B.Bottom <= A.Bottom);
end;

constructor TFlowBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmapChanged := True;
  FFocusGroup := -1;
  FSizeIndex := -1;
  FGroups := TFlowGroups.Create(Self);
  FPadding := 8;
  FColumnWidth := 80 * 3;
  ControlStyle := ControlStyle + [csDoubleClicks, csAcceptsControls,
    csCaptureMouse, csClickEvents] - [csSetCaption];
  Width := FColumnWidth;
  Height := 240;
end;

destructor TFlowBox.Destroy;
begin
  FGroups.Free;
  DestroyFastBitmap(FBitmap);
  inherited Destroy;
end;

procedure TFlowBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.style := Params.WindowClass.style and (not (CS_VREDRAW or CS_HREDRAW));
end;

procedure TFlowBox.Loaded;
begin
  inherited Loaded;
  UpdateGroups;
end;

function TFlowBox.MinHeight(Group: TFlowGroup): Integer;

  function Max(A, B: Integer): Integer;
  begin
    if A > B then Result := A else Result := B;
  end;

var
  R: TRect;
  C: TControl;
  H, I: Integer;
begin
  R := Group.FRect;
  H := R.Top + MinGroupHeight;
  for I := 0 to ControlCount - 1 do
  begin
    C := Controls[I];
    if RectInRect(R, C.BoundsRect) then
      H := Max(H, C.Top + C.Height);
  end;
  Result := H - R.Top;
end;

function TFlowBox.ControlInGroup(Control: TControl; Group: TFlowGroup): Boolean;
begin
  Result := (Control <> nil) and (Group <> nil) and (Control.Parent = Self) and
    RectInRect(Group.FRect, Control.BoundsRect);
end;

procedure TFlowBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  R: TRects;
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  R := nil;
  if CanSize and (Button = mbLeft) then
  begin
    FSizeIndex := -1;
    P := Point(X, Y);
    R := GetSizeRects;
    for I := Low(R) to High(R) do
      if PtInRect(R[I], P) then
      begin
        FSizeIndex := I;
        Break;
      end;
    if FSizeIndex > -1 then
    begin
      FSizePoint := P;
      FSizeMin := MinHeight(FGroups[FSizeIndex]);
    end;
  end;
end;

procedure TFlowBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  G: TFlowGroup;
  DY, H: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FSizeIndex > -1 then
  begin
    G := FGroups[FSizeIndex];
    DY := Y - FSizePoint.Y;
    FSizePoint := Point(X, Y);
    H := G.Height + DY;
    if H < FSizeMin then
      H := FSizeMin;
    G.Height := H;
  end;
end;

procedure TFlowBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FSizeIndex := -1;
    if csDesigning in ComponentState then
    begin
      Screen.Cursor := crDefault;
      GetParentForm(Self).Designer.Modified;
    end;
  end;
end;

procedure TFlowBox.FitHeight;
var
  Heights: array of Integer;
  H, I: Integer;
begin
  if FGroups.Count = 0 then
    Exit;
  SetLength(Heights, FGroups.Count);
  for I := 0 to FGroups.Count - 1 do
    Heights[I] := MinHeight(FGroups[I]);
  FGroups.BeginUpdate;
  for I := Low(Heights) to High(Heights) do
  begin
    H := Heights[I];
    if H mod MinGroupHeight > 0 then
      H := (H div MinGroupHeight) * MinGroupHeight + MinGroupHeight;
    FGroups[I].Height := H;
  end;
  FGroups.EndUpdate;
end;

procedure TFlowBox.FitWidth;
var
  G: TFlowGroup;
  W, X: Integer;
  I: Integer;
begin
  if FGroups.Count = 0 then
    Exit;
  X := ColumnWidth + FPadding * 2;
  for I := 0 to FGroups.Count - 1 do
  begin
    G := FGroups[I];
    W := G.FRect.Right + FPadding;
    if W > X then
      X := W;
  end;
  if Width <> X then
    Width := X;
end;

procedure TFlowBox.Invalidate;
begin
  FBitmapChanged := True;
  inherited Invalidate;
end;

procedure TFlowBox.DrawGroup(Group: TFlowGroup);
var
  State: TDrawState;
begin
  if Assigned(FOnDrawGroup) then
  begin
    State := [];
    if FFocusGroup = Group.Index then
      Include(State, dsFocused);
    FOnDrawGroup(Self, Group, Group.FRect, State);
  end;
end;

procedure TFlowBox.FocusChanged(Control: TWinControl);
var
  Group: Integer;
  R: TRect;
  I: Integer;
begin
  Group := -1;
  if Control <> nil then
  begin
    R := Control.BoundsRect;
    for I := 0 to FGroups.Count - 1 do
      if RectInRect(FGroups[I].FRect, R) then
      begin
        Group := I;
        Break;
      end;
  end;
  if Group <> FFocusGroup then
  begin
    FFocusGroup := Group;
    Invalidate;
    for I := 0 to ControlCount - 1 do
      Controls[I].Invalidate;
    GroupChange;
  end;
end;

procedure TFlowBox.GroupChange;
var
  R: TRect;
begin
  if Assigned(FOnGroupChange) then
  begin
    if FFocusGroup > -1 then
      R := FGroups[FFocusGroup].FRect
    else
      FillChar(R, SizeOf(R), #0);
    FOnGroupChange(Self, FFocusGroup, R);
  end;
end;

procedure TFlowBox.Paint;
begin
  inherited;
  //PaintBackground(Canvas.Handle);
end;

procedure TFlowBox.PaintBackground(BackgroundDC: HDC);
const
  Matching = 75;

  procedure DrawGroupInfo(DC: HDC; Rect: TRect; Index: Integer);
  var
    F: HFont;
    S: string;
    P: TSize;
  begin
    F := SelectObject(DC, Font.Handle);
    S := Trim(FGroups[Index].Name);
    if S = '' then
      S := '(unnammed)';
    S := Format('%d: %s', [Index, S]);
    P := CalcCaptionSize(DC, S);
    Rect.Bottom := Rect.Top + P.cy;
    Rect.Left := Rect.Right - P.cx - 6;
    FillRectColor(DC, Rect, Color);
    DrawRectOutline(DC, Rect, Blend(Color, 0, Matching));
    SetTextColor(DC, Blend(Color, 0, Matching));
    SetBkMode(DC, TRANSPARENT);
    DrawText(DC, PChar(S), -1, Rect,
      DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_NOCLIP);
    SelectObject(DC, F);
  end;

var
  DC: HDC;
  B: HBRUSH;
  R: TRect;
  Group: TFlowGroup;
  C, I: Integer;
begin
  if Assigned(FOnDrawBackground) or Assigned(FOnDrawGroup) then
  begin
    if FBitmap.DC = 0 then
    begin
      FBitmap := CreateFastBitmap(Width, Height, pd32);
      FBitmapChanged := True;
    end;
    DC := FBitmap.DC;
  end
  else
  begin
    DestroyFastBitmap(FBitmap);
    DC := BackgroundDC;
    FBitmapChanged := True;
  end;
  if FBitmapChanged then
  begin
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        if Assigned(FOnDrawBackground) then
          FOnDrawBackground(Self)
        else
          FillRectColor(DC, ClientRect, Color);
        if csDesigning in ComponentState then
          FillRectOutline(DC, ClientRect, clBtnShadow, psDot);
        SetBkColor(DC, ColorToRGB(Color));
        B := GetBrush(Blend(Color, 0, Matching), bsFDiagonal);
        try
          C := 0;
          for I := 0 to FGroups.Count - 1 do
          begin
            Group := FGroups[I];
            if not Group.Visible then Continue;
            if C <> Group.FColumn then
            begin
              C := Group.FColumn;
              R := Group.FRect;
              R.Left := R.Left - FPadding;
              R.Top := 0;
              R.Bottom := Height;
              R.Right := R.Left + 1;
              //FillRectColor(DC, R, Color);
              InflateRect(R, 0, -FPadding);
              //FillRectColor(DC, R, Blend(cl3DDkShadow, clBtnShadow));
            end;
            if csDesigning in ComponentState then
            begin
              R := Group.FRect;
              FillRect(DC, R, B);
              DrawRectOutline(DC, R, Blend(Color, 0, Matching));
              DrawGroupInfo(DC, R, I);
            end
            else
              DrawGroup(Group);
          end;
        finally
          DeleteObject(B);
        end;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  end;
  FBitmapChanged := False;
  if BackgroundDC = FBitmap.DC then
    PaintControls(DC, nil)
  else if Assigned(FOnDrawGroup) then
    BitBlt(BackgroundDC, 0, 0, FBitmap.Width, FBitmap.Height, FBitmap.DC,
      0, 0, SRCCOPY);
end;

procedure TFlowBox.PlaceControls;
var
  OldRects, NewRects: TRects;

  function CalcChange: Boolean;
  const
    Offset = -1500;
    Grid = 20;
  var
    R, S: TRect;
    B: TFlowGroup;
    C, H, I: Integer;
  begin
    Result := False;
    R.Top := 0;
    R.Left := FPadding;
    R.Right := R.Left + FColumnWidth;
    R.Bottom := 0;
    C := 0;
    H := Height;
    for I := 0 to FGroups.Count - 1 do
    begin
      S := R;
      B := FGroups[I];
      if B.FWasVisible <> B.FVisible then
        Result := True;
      B.FWasVisible := B.FVisible;
      R.Top := R.Bottom + FPadding;
      R.Bottom := R.Top + B.Height;
      if not B.FVisible then
        OffsetRect(R, ((I + 1) mod Grid) * Offset, ((I + 1) div Grid) * Offset)
      else if (R.Top > FPadding) and (R.Bottom > H) then
      begin
        Inc(C);
        R.Top := FPadding;
        R.Bottom := R.Top + B.Height;
        R.Left := R.Right + FPadding * 2;
        R.Right := R.Left + FColumnWidth;
      end;
      if B.FColumn <> C then
        Result := True;
      B.FColumn := C;
      OldRects[I] := B.FRect;
      B.FRect := R;
      NewRects[I] := B.FRect;
      if not B.FVisible then
        R := S;
    end;
  end;

  procedure MoveControls;
  var
    C: TControl;
    R: TRect;
    W, H: Integer;
    I, J, K: Integer;
  begin
    for I := 0 to ControlCount - 1 do
    begin
      C := Controls[I];
      R := C.BoundsRect;
      K := -1;
      for J := Low(OldRects) to High(OldRects) do
        if RectInRect(OldRects[J], R) then
        begin
          K := J;
          Break;
        end;
      if K < 0 then
      begin
        Tag := 1;
        Continue;
      end;
      W := WidthOf(R);
      H := HeightOf(R);
      R.Left := R.Left - OldRects[K].Left + NewRects[K].Left;
      R.Top := R.Top - OldRects[K].Top + NewRects[K].Top;
      R.Right := R.Left + W;
      R.Bottom := R.Top + H;
      C.BoundsRect := R;
      C.Visible := FGroups[K].Visible;
    end;
  end;

  procedure PaintDividers;
  var
    G: TFlowGroup;
    DC: HDC;
    R: TRect;
    C, I: Integer;
  begin
    if not HandleAllocated then Exit;
    Exit;
    DC := 0;
    C := 0;
    for I := 0 to FGroups.Count - 1 do
    begin
      G := FGroups[I];
      if G.FColumn = C then Continue;
      C := G.FColumn;
      R := G.FRect;
      R.Left := R.Left - FPadding;
      R.Top := 0;
      R.Bottom := Height;
      R.Right := R.Left + 1;
      if DC = 0 then
        DC := GetDC(Handle);
      FillRectColor(DC, R, Color);
      InflateRect(R, 0, -FPadding);
      FillRectColor(DC, R, Blend(cl3DDkShadow, clBtnShadow));
    end;
    if DC <> 0 then
      ReleaseDC(Handle, DC);
  end;

begin
  SetLength(OldRects, FGroups.Count);
  SetLength(NewRects, FGroups.Count);
  if CalcChange or FChanged then
  begin
    DisableAlign;
    MoveControls;
    EnableAlign;
    Invalidate;
  end;
  FChanged := False;
  PaintDividers;
  if AutoFit then
    FitWidth;
end;

type
  TTabControl = class(TWinControl)
  public
    property TabOrder;
  end;

function SortControls(Item1, Item2: Pointer): Integer;
var
  A: TTabControl absolute Item1;
  B: TTabControl absolute Item2;
  F: TFlowBox;
  AC, BC: Integer;
begin
  if A = B then
  begin
    Result := 0;
    Exit;
  end;
  F := A.Parent as TFlowBox;
  AC := A.Left div (F.ColumnWidth + F.Padding * 2);
  BC := B.Left div (F.ColumnWidth + F.Padding * 2);
  if AC < BC then
    Result := -1
  else if AC > BC then
    Result := 1
  else if A.Top < B.Top then
    Result := -1
  else if A.Top > B.Top  then
    Result := 1
  else if A.Left < B.Left then
    Result := -1
  else if A.Left > B.Left then
    Result := 1
  else
    Result := 0;
end;

procedure TFlowBox.UpdateGroups;
var
  L: TList;
  I: Integer;
begin
  FChanged := True;
  PlaceControls;
  L := TList.Create;
  try
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TWinControl then
        L.Add(Controls[I]);
    L.Sort(SortControls);
    for I := 0 to L.Count - 1 do
      TTabControl(L[I]).TabOrder := I;
  finally
    L.Free;
  end;
  if (FFocusGroup > -1) and (GetParentForm(Self) <> nil) then
    FocusChanged(GetParentForm(Self).ActiveControl);
end;

function TFlowBox.CanSize: Boolean;
begin
  Result := FAllowResize or (csDesigning in ComponentState);
end;

procedure TFlowBox.SetAutoFit(Value: Boolean);
begin
  if Value <> FAutoFit then
  begin
    FAutoFit := Value;
    if FAutoFit then
      FitWidth;
  end;
end;

procedure TFlowBox.SetColumnWidth(Value: Integer);
begin
  if Value < 10 then Value := 10;
  if Value <> FColumnWidth then
  begin
    FColumnWidth := Value;
    UpdateGroups;
  end;
end;

procedure TFlowBox.SetGroups(Value: TFlowGroups);
begin
  FGroups.Assign(Value);
end;

procedure TFlowBox.SetPadding(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FPadding then
  begin
    FPadding := Value;
    UpdateGroups;
  end;
end;

function TFlowBox.GetSizeRects: TRects;
const
  SizeRange = 4;
var
  R: TRect;
  I: Integer;
begin
  SetLength(Result, FGroups.Count);
  for I := 0 to FGroups.Count - 1 do
  begin
    R := FGroups[I].FRect;
    R.Top := R.Bottom;
    R.Bottom := R.Top + SizeRange;
    Result[I] := R;
  end;
end;

procedure TFlowBox.Resize;
var
  W: TWinControl;
  DC: HDC;
  H: HWND;
  I: Integer;
begin
  inherited Resize;
  if (not HandleAllocated) then Exit; //  (not IsWindowVisible(Handle))
  DestroyFastBitmap(FBitmap);
  PlaceControls;
  if Assigned(FOnDrawBackground) then
  begin
    Invalidate;
    UpdateWindow(Handle);
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TWinControl then
      begin
        W := TWinControl(Controls[I]);
        if (not W.Visible) or (W.Parent <> Self) or (not W.HandleAllocated) then Continue;
        H := W.Handle;
        InvalidateRgn(H, 0, False);
        UpdateWindow(H);
      end;
  end;
  if Assigned(FOnDrawBackground) or Assigned(FOnDrawGroup) then
  begin
    FBitmap := CreateFastBitmap(Width, Height, pd32);
    FBitmapChanged := True;
    SendMessage(Handle, WM_ERASEBKGND, FBitmap.DC, 0);
    DC := GetDC(Handle);
    BitBlt(DC, 0, 0, FBitmap.Width, FBitmap.Height, FBitmap.DC, 0, 0, SRCCOPY);
    ReleaseDC(Handle, DC);
  end;
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TFlowBox.CMDesignHitTest(var Message: TCMDesignHitTest);
var
	P: TPoint;
  R: TRects;
  I: Integer;
begin
	inherited;
  R := nil;
	Message.Result := 0;
  if FSizeIndex > -1 then
  	Message.Result := 1
  else
  begin
    P := SmallPointToPoint(Message.Pos);
    R := GetSizeRects;
    for I := Low(R) to High(R) do
      if PtInRect(R[I], P) then
      begin
      	Message.Result := 1;
        Break;
      end;
  end;
  if Message.Result = 1 then
    Screen.Cursor := crVSplit
  else
    Screen.Cursor := crDefault;
end;

procedure TFlowBox.CMFocusChanged(var Message: TCMFocusChanged);
var
  W: TWinControl;
begin
  inherited;
  if FActive then
  begin
    W := Message.Sender;
    while (W <> nil) and (W.Parent <> Self) do
      W := W.Parent;
    FocusChanged(W);
  end;
end;

procedure TFlowBox.WMChildFocus(var Message: TMessage);
begin
  inherited;
  FActive := True;
  Invalidate;
end;

procedure TFlowBox.WMChildKillFocus(var Message: TMessage);
begin
  inherited;
  FActive := False;
  FocusChanged(nil);
  Invalidate;
end;

procedure TFlowBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if Message.DC <> 0 then
    PaintBackground(Message.DC);
  if csDesigning in ComponentState then
    FillRectOutline(Message.DC, ClientRect, clBtnShadow, psDot);
  Message.Result := 1;
end;

procedure TFlowBox.WMSetCursor(var Message: TWMSetCursor);
var
  C: TCursor;
  P: TPoint;
  R: TRects;
  I: Integer;
begin
  R := nil;
  if CanSize and (FSizeIndex = -1) then
  begin
    C := crDefault;
    GetCursorPos(P);
    P := ScreenToClient(P);
    R := GetSizeRects;
    for I := Low(R) to High(R) do
      if PtInRect(R[I], P) then
      begin
        C := crVSplit;
        Break;
      end;
    Cursor := C;
  end;
  inherited;
end;

end.
