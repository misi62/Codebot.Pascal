
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  2.00.00 Private Build                               *)
(*                                                      *)
(********************************************************)

unit GridCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ImgList, Forms,
  BaseTypes, GraphTools, ProviderTools, FormTools;

{ TScrollWindow }

type
  TScrollData = record
    Left, Top, Width, Height: Integer;
  end;

  TScrollKind = (skVertical, skHorizontal);

  TScrollChangeEvent = procedure(Sender: TObject; X, Y: Integer) of object;

  TScrollWindow = class(TFramedWindow)
  private
    FScrollData: TScrollData;
    FLineHeight: Integer;
    FLineWidth: Integer;
    FUpdateRef: Integer;
    FUpdating: Boolean;
    FOnScrollChange: TScrollChangeEvent;
    procedure UpdateScrollBars;
    function GetScrollWidth: Integer;
    function GetScrollHeight: Integer;
    function GetCanUpdate: Boolean;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function IsBarVisible(Kind: TScrollKind): Boolean;
    procedure SetScrollData(const Value: TScrollData);
    function GetScrollData: TScrollData;
    procedure ScrollTo(X, Y: Integer); overload;
    procedure ScrollTo(const Rect: TRect); overload;
    procedure UpdateChanged;
    procedure DoScroll(X, Y: Integer); virtual;
    procedure DoUpdate; virtual;
    property LineWidth: Integer read FLineWidth write FLineWidth;
    property LineHeight: Integer read FLineHeight write FLineHeight;
    property OnScrollChange: TScrollChangeEvent read FOnScrollChange write FOnScrollChange;
  public
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property CanUpdate: Boolean read GetCanUpdate;
    property Anchors;
    property Canvas;
    property Color;
    property ScrollWidth: Integer read GetScrollWidth;
    property ScrollHeight: Integer read GetScrollHeight;
    property OnResize;
  end;

{ TGridCellManager }

  PGridNode = ^TGridNode;
  TGridNode = record
    Size: Integer;
    Index: Integer;
    Next: PGridNode;
  end;

  TGridWall = record
    NearWall: Integer;
    FarWall: Integer;
  end;

  TGridCellManager = class(TObject)
  private
    FCols: PGridNode;
    FRows: PGridNode;
    FDefColWidth: Integer;
    FDefRowHeight: Integer;
    FColCount: Integer;
    FRowCount: Integer;
    procedure SetColCount(Value: Integer);
    procedure SetRowCount(Value: Integer);
    function GetCol(Index: Integer): Integer;
    procedure SetCol(Index: Integer; Value: Integer);
    function GetRow(Index: Integer): Integer;
    procedure SetRow(Index: Integer; Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function GetCell(X, Y: Integer): TRect;
    property DefColWidth: Integer read FDefColWidth write FDefColWidth;
    property DefRowHeight: Integer read FDefRowHeight write FDefRowHeight;
    property ColCount: Integer read FColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount;
    property RowHeight[Index: Integer]: Integer read GetRow write SetRow;
    property ColWidth[Index: Integer]: Integer read GetCol write SetCol;
  end;

  TGridCoord = TPoint;
  TGridHitTest = (ghNothing, ghIcon, ghCheckbox);

  TGridHitTestEvent = procedure(Sender: TObject; X, Y: Integer;
    var HitTest: TGridHitTest) of object;
  TGridSelectionEvent = procedure(Sender: TObject; Col, Row: Integer;
    var Allow: Boolean) of object;

  TMergeCellParams = record
    Col, Row, Width, Height: Integer;
    Merged: Boolean;
  end;

  TGridCellMergeEvent = procedure(Sender: TObject; var Params: TMergeCellParams) of object;

  TDrawRowEvent = procedure(Sender: TObject; Row: Integer; Rect: TRect;
    var DefaultDraw: Boolean) of object;
  TDrawCellEvent = procedure(Sender: TObject; Col, Row: Integer; Rect: TRect;
    State: TDrawState) of object;
  TDrawIndexSectionEvent = procedure(Sender: TObject; Section, Index: Integer; Rect: TRect;
    State: TDrawState) of object;

  TContentGrid = class;

{ IContentGridProvider }

  IContentGridProvider = interface(IControlProvider)
    ['{964620D3-D2B2-4F98-9D7C-CB0DAB68B3AA}']
    procedure Resize(Grid: TContentGrid);
    procedure HitTest(Grid: TContentGrid; X, Y: Integer;
      var HitTest: TGridHitTest);
    procedure HotTrack(Grid: TContentGrid; Col, Row: Integer;
      var Allow: Boolean);
    procedure Select(Grid: TContentGrid; Col, Row: Integer;
      var Allow: Boolean);
    procedure DrawBackground(Grid: TContentGrid; DC: HDC);
    procedure DrawRow(Grid: TContentGrid; Row: Integer; Rect: TRect;
      var DefaultDraw: Boolean);
    procedure DrawCell(Grid: TContentGrid; Col, Row: Integer; Rect: TRect;
      State: TDrawState);
  end;

{ TContentGrid }

  TContentGrid = class(TScrollWindow)
  private
    FChanged: Boolean;
    FManager: TGridCellManager;
    FHotTrack: TGridCoord;
    FSelection: TGridCoord;
    FMouseTrack: Boolean;
    FTimerActive: Boolean;
    FAutoScroll: Boolean;
    FSingleColumn: Boolean;
    FOnDrawCell: TDrawCellEvent;
    FOnDrawRow: TDrawRowEvent;
    FOnHitTest: TGridHitTestEvent;
    FOnHotTrack: TGridSelectionEvent;
    FOnSelection: TGridSelectionEvent;
    FOnDrawIndexSection: TDrawIndexSectionEvent;
    FOnMergeCell: TGridCellMergeEvent;
    procedure GridChanged;
    procedure StartTimer;
    procedure StopTimer;
    function GetColWidths(Col: Integer): Integer;
    procedure SetColWidths(Col: Integer; Value: Integer);
    function GetRowHeights(Row: Integer): Integer;
    procedure SetRowHeights(Row: Integer; Value: Integer);
    function GetColCount: Integer;
    procedure SetColCount(Value: Integer);
    function GetRowCount: Integer;
    procedure SetRowCount(Value: Integer);
    function GetDefColWidth: Integer;
    procedure SetDefColWidth(Value: Integer);
    function GetDefRowHeight: Integer;
    procedure SetDefRowHeight(Value: Integer);
    procedure SetSelection(Value: TGridCoord);
    procedure SetSingleColumn(Value: Boolean);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetGridProvider: IContentGridProvider;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure DoUpdate; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetHotTrack(const Value: TGridCoord);
    property GridProvider: IContentGridProvider read GetGridProvider;
    property OnDrawIndexSection: TDrawIndexSectionEvent read FOnDrawIndexSection write FOnDrawIndexSection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function RectFromCoord(X, Y: Integer): TRect;
    property HotTrack: TGridCoord read FHotTrack;
    function CoordFromPoint(X, Y: Integer): TGridCoord;
    property ColWidths[Col: Integer]: Integer read GetColWidths write SetColWidths;
    property RowHeights[Row: Integer]: Integer read GetRowHeights write SetRowHeights;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Selection: TGridCoord read FSelection write SetSelection;
    property OnMergeCell: TGridCellMergeEvent read FOnMergeCell write FOnMergeCell;
    property OnScrollChange;
  published
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default True;
    property DefColWidth: Integer read GetDefColWidth write SetDefColWidth;
    property DefRowHeight: Integer read GetDefRowHeight write SetDefRowHeight;
    property ColCount: Integer read GetColCount write SetColCount;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property SingleColumn: Boolean read FSingleColumn write SetSingleColumn default False;
    property OnHitTest: TGridHitTestEvent read FOnHitTest write FOnHitTest;
    property OnSelection: TGridSelectionEvent read FOnSelection write FOnSelection;
    property OnHotTrack: TGridSelectionEvent read FOnHotTrack write FOnHotTrack;
    property OnDrawRow: TDrawRowEvent read FOnDrawRow write FOnDrawRow;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property Anchors;
    property Align;
    property Color;
    property Constraints;
    property Cursor;
    property Enabled;
    property TabOrder;
    property PopupMenu;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property HelpContext;
    property Hint;
    property ShowHint;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnResize;
  end;

  TImageListGrid = class(TContentGrid)
  private
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TCustomImageList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Images: TCustomImageList read FImages write SetImages;
  end;

function ScrollData(ALeft, ATop, aWidth, AHeight: Integer): TScrollData;
function Compare(const A, B: TGridCoord): Boolean;

implementation

uses Types;

function ScrollData(ALeft, ATop, aWidth, AHeight: Integer): TScrollData;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Width := AWidth;
    Height := AHeight;
  end;
end;

function Compare(const A, B: TGridCoord): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

{ TScrollWindow }

procedure TScrollWindow.CreateWnd;
begin
  inherited CreateWnd;
  UpdateChanged;
end;

function TScrollWindow.IsBarVisible(Kind: TScrollKind): Boolean;
var
  BothVisible: Boolean;
  W, H: Integer;
begin
  W := InternalWidth;
  H := InternalHeight;
  BothVisible := (FScrollData.Width + GetSystemMetrics(SM_CXVSCROLL) - FScrollData.Left > W) and
    (FScrollData.Height + GetSystemMetrics(SM_CYHSCROLL) - FScrollData.Top > H);
  if Kind = skHorizontal then
    Result := BothVisible or (FScrollData.Width - FScrollData.Left > W) or (FScrollData.Left > 0)
  else
    Result := BothVisible or (FScrollData.Height - FScrollData.Top > H) or (FScrollData.Top > 0);
end;

procedure TScrollWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and (not (CS_HREDRAW or CS_VREDRAW));
end;

function TScrollWindow.GetScrollWidth: Integer;
var
  X: Integer;
begin
  X := InternalWidth;
  if IsBarVisible(skVertical) then
    Result := X - GetSystemMetrics(SM_CXVSCROLL)
  else
    Result := X;
  if Result < 0 then
    Result := 0;
end;

function TScrollWindow.GetScrollHeight: Integer;
var
  Y: Integer;
begin
  Y := InternalHeight;
  if IsBarVisible(skHorizontal) then
    Result := Y - GetSystemMetrics(SM_CYHSCROLL)
  else
    Result := Y;
  if Result < 0 then
    Result := 0;
end;

function TScrollWindow.GetScrollData: TScrollData;
begin
  Result := FScrollData;
end;

procedure TScrollWindow.SetScrollData(const Value: TScrollData);
begin
  if (FScrollData.Left <> Value.Left) or (FScrollData.Top <> Value.Top) or
    (FScrollData.Width <> Value.Width) or (FScrollData.Height <> Value.Height) then
  begin
    FScrollData := Value;
    if FScrollData.Left < 0 then
      FScrollData.Left := 0
    else if FScrollData.Left > FScrollData.Width then
      FScrollData.Left := FScrollData.Width;
    if FScrollData.Top < 0 then
      FScrollData.Top := 0
    else if FScrollData.Top > FScrollData.Height then
      FScrollData.Top := FScrollData.Height;
    UpdateChanged;
  end;
end;

procedure TScrollWindow.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  BeginUpdate;
  try
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  finally
    EndUpdate;
  end;
end;

procedure TScrollWindow.ScrollTo(X, Y: Integer);
var
  A, B: TScrollData;
  DX, DY: Integer;
begin
  A := FScrollData;
  B := A;
  B.Left := X;
  B.Top := Y;
  FScrolLData := B;
  if X > FScrollData.Width - ScrollWidth then
    X := FScrollData.Width - ScrollWidth;
  if X < 0 then
    X := 0;
  if Y > FScrollData.Height - ScrollHeight then
    Y := FScrollData.Height - ScrollHeight;
  if Y < 0 then
    Y := 0;
  FScrolLData := A;
  DX := FScrollData.Left - X;
  DY := FScrollData.Top - Y;
  if (DX <> 0) or (DY <> 0) then
  begin
    FScrollData.Left := X;
    FScrollData.Top := Y;
    UpdateChanged;
    if DoubleBuffered then
      Invalidate
    else
      ScrollWindow(Handle, DX, DY, nil, nil);
  end;
end;

procedure TScrollWindow.ScrollTo(const Rect: TRect);
var
  D: TScrollData;
  X, Y: Integer;
begin
  D := GetScrollData;
  X := D.Left;
  Y := D.Top;
  if Rect.Left < D.Left then
    X := Rect.Left
  else if Rect.Right > D.Left + ScrollWidth then
    X := Rect.Right - ScrollWidth;
  if Rect.Top < D.Top then
    Y := Rect.Top
  else if Rect.Bottom > D.Top + ScrollHeight then
    Y := Rect.Bottom - ScrollHeight;
  ScrollTo(X, Y);
end;

procedure TScrollWindow.UpdateChanged;
begin
  if (FUpdateRef < 1) and (not FUpdating) then
  begin
    FUpdating := True;
    try
      DoUpdate;
      UpdateScrollBars;
    finally
      FUpdating := False;
    end;
  end;
end;

function TScrollWindow.GetCanUpdate: Boolean;
begin
  Result := (not FUpdating) and (FUpdateRef < 1);
end;

procedure TScrollWindow.DoScroll(X, Y: Integer);
begin
  if Assigned(FOnScrollChange) then
    FOnScrollChange(Self, X, Y);
end;

procedure TScrollWindow.DoUpdate;
begin
end;

procedure TScrollWindow.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

procedure TScrollWindow.EndUpdate;
begin
  Dec(FUpdateRef);
  UpdateChanged;
end;

procedure TScrollWindow.UpdateScrollBars;
var
  Info: TScrollInfo;
  DX, DY: Integer;
begin
  if not HandleAllocated then Exit;
  with FScrollData do
  begin
    if IsBarVisible(skVertical) then
      DX := GetSystemMetrics(SM_CXVSCROLL)
    else
      DX := 0;
    if (Left + InternalWidth - DX > Width) and (Left > 0) then
    begin
      DX := GetScrollPos(Handle, SB_HORZ) + ScrollWidth - Width;
      Left := Left - DX;
      if Left < 0 then
      begin
        DX := DX + Left;
        Left := 0;
      end;
    end
    else
      DX := 0;
    if IsBarVisible(skHorizontal) then
      DY := GetSystemMetrics(SM_CYHSCROLL)
    else
      DY := 0;
    if (Top + InternalHeight - DY > Height) and (Top > 0) then
    begin
      DY := GetScrollPos(Handle, SB_VERT) + ScrollHeight - Height;
      Top := Top - DY;
      if Top < 0 then
      begin
        DY := DY + Top;
        Top := 0;
      end;
    end
    else
      DY := 0;
    if DX + DY > 0 then
      if DoubleBuffered then
        Repaint
      else
        ScrollWindow(Handle, DX, DY, nil, nil);
  end;
  with Info do
  begin
    cbSize := SizeOf(Info);
    fMask := SIF_PAGE or SIF_POS or SIF_RANGE;
    nMin := 0;
    nMax := FScrollData.Height - 1;
    nPage := ScrollHeight;
    nPos := FScrollData.Top;
    SetScrollInfo(Handle, SB_VERT, Info, True);
    nMax := FScrollData.Width - 1;
    nPage := ScrollWidth;
    nPos := FScrollData.Left;
    SetScrollInfo(Handle, SB_HORZ, Info, True);
    DoScroll(FScrollData.Left, FScrollData.Top);
  end;
end;

procedure TScrollWindow.WMVScroll(var Msg: TWMVScroll);
var
  X, Y: Integer;
begin
  X := FScrollData.Left;
  Y := FScrollData.Top;
  case Msg.ScrollCode of
    SB_BOTTOM: ScrollTo(X, FScrollData.Height);
    SB_LINEDOWN: ScrollTo(X, Y + FLineHeight);
    SB_LINEUP: ScrollTo(X, Y - FLineHeight);
    SB_PAGEDOWN: ScrollTo(X, Y + ScrollHeight);
    SB_PAGEUP:  ScrollTo(X, Y - ScrollHeight);
    SB_THUMBTRACK: ScrollTo(X, Msg.Pos);
    SB_TOP: ScrollTo(X, 0);
  end;
end;

procedure TScrollWindow.WMHScroll(var Msg: TWMHScroll);
var
  X, Y: Integer;
begin
  X := FScrollData.Left;
  Y := FScrollData.Top;
  case Msg.ScrollCode of
    SB_BOTTOM: ScrollTo(ScrollWidth, Y);
    SB_LINEDOWN: ScrollTo(X + FLineWidth, Y);
    SB_LINEUP: ScrollTo(X - FLineWidth, Y);
    SB_PAGEDOWN: ScrollTo(X + ScrollWidth, Y);
    SB_PAGEUP:  ScrollTo(X - ScrollWidth, Y);
    SB_THUMBTRACK: ScrollTo(Msg.Pos, Y);
    SB_TOP: ScrollTo(0, Y);
  end;
end;

{ TGridCellManager }

procedure ClearGridNodes(var Root: PGridNode);
var
  N: PGridNode;
begin
  while Root <> nil do
  begin
    N := Root;
    Root := N.Next;
    Dispose(N);
  end;
end;

function FindGridNode(Root: PGridNode; Index: Integer; out Parent: PGridNode): PGridNode;
begin
  Parent := nil;
  Result := nil;
  while Root <> nil do
    if Root.Index = Index then
    begin
      Result := Root;
      Break;
    end
    else if Root.Index < Index then
    begin
      Parent := Root;
      Root := Root.Next;
    end
    else
      Break;
end;

procedure ResizeGridNode(var Root: PGridNode; Size, Index, DefSize: Integer);
var
  P, N: PGridNode;
begin
  N := FindGridNode(Root, Index, P);
  if N <> nil then
  begin
    if N.Size = DefSize then
    begin
      if P = nil then
        Root := N.Next
      else
        P.Next := N.Next;
      Dispose(N);
    end
    else
      N.Size := Size
  end
  else if Size <> DefSize then
  begin
    New(N);
    N.Size := Size;
    N.Index := Index;
    N.Next := nil;
    if Root = nil then
      Root := N
    else
    begin
      N.Next := P.Next;
      P.Next := N;
    end;
  end;
end;

procedure TruncGridNode(var Root: PGridNode; Count: Integer);
var
  P, N, C: PGridNode;
begin
  N := FindGridNode(Root, Count - 1, P);
  while N <> nil do
  begin
    C := N;
    N := N.Next;
    Dispose(C);
  end;
  if P <> nil then
    P.Next := nil
  else
    Root := nil;
end;

function QueryGridNode(Root: PGridNode; Index, DefSize: Integer): TGridWall;
var
  I: Integer;
begin
  Result.NearWall := 0;
  Result.FarWall := DefSize;
  I := 0;
  while Root <> nil do
    if Root.Index = Index then
    begin
      Result.FarWall := Root.Size;
      Break;
    end
    else if Root.Index <= I then
    begin
      Result.NearWall := Result.NearWall + Root.Size;
      Root := Root.Next;
      Inc(I);
    end
    else
      Break;
  Result.NearWall := Result.NearWall + (Index -  I) * DefSize;
  Result.FarWall := Result.NearWall + Result.FarWall;
end;

constructor TGridCellManager.Create;
begin
  inherited Destroy;
end;

destructor TGridCellManager.Destroy;
begin
  ClearGridNodes(FCols);
  ClearGridNodes(FRows);
  inherited Destroy;
end;

function TGridCellManager.GetCell(X, Y: Integer): TRect;
var
  C, R: TGridWall;
begin
  if (X > -1) and (X < FColCount) and (Y > -1) and (Y < FRowCount) then
  begin
    C := QueryGridNode(FCols, X, FDefColWidth);
    R := QueryGridNode(FRows, Y, FDefRowHeight);
    Result.Left := C.NearWall;
    Result.Top := R.NearWall;
    Result.Right := C.FarWall;
    Result.Bottom := R.FarWall;
  end
  else
  begin
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := 0;
    Result.Bottom := 0;
  end;
end;

procedure TGridCellManager.SetColCount(Value: Integer);
begin
  TruncGridNode(FCols, Value);
  FColCount := Value;
end;

procedure TGridCellManager.SetRowCount(Value: Integer);
begin
  TruncGridNode(FRows, Value);
  FRowCount := Value;
end;

function TGridCellManager.GetCol(Index: Integer): Integer;
var
  P: PGridNode;
begin
  if (Index > -1) and (Index < FColCount) then
  begin
    P := FindGridNode(FCols, Index, P);
    if P <> nil then
      Result := P.Size
    else
      Result := FDefColWidth;
  end
  else
    Result := 0;
end;

procedure TGridCellManager.SetCol(Index: Integer; Value: Integer);
begin
  if (Index > -1) and (Index < FColCount) then
    ResizeGridNode(FCols, Value, Index, FDefColWidth);
end;

function TGridCellManager.GetRow(Index: Integer): Integer;
var
  P: PGridNode;
begin
  if (Index > -1) and (Index < FRowCount) then
  begin
    P := FindGridNode(FRows, Index, P);
    if P <> nil then
      Result := P.Size
    else
      Result := FDefRowHeight;
  end
  else
    Result := 0;
end;

procedure TGridCellManager.SetRow(Index: Integer; Value: Integer);
begin
  if (Index > -1) and (Index < FRowCount) then
    ResizeGridNode(FRows, Value, Index, FDefRowHeight);
end;

{ TContentGrid }

constructor TContentGrid.Create(AOwner: TComponent);
begin
  FManager := TGridCellManager.Create;
  inherited Create(AOwner);
  FAutoScroll := True;
  FHotTrack.X := -1;
  FHotTrack.Y := -1;
  ParentColor := False;
  ControlStyle := ControlStyle + [csCaptureMouse, csDoubleClicks, csClickEvents, csOpaque];
  Color := clWindow;
  Width := 8 * 24;
  Height := 8 * 24;
  BeginUpdate;
  ColCount := 5;
  RowCount := 5;
  DefColWidth := 75;
  DefRowHeight := 25;
  EndUpdate;
end;

destructor TContentGrid.Destroy;
begin
  FManager.Free;
  inherited Destroy;
end;

function TContentGrid.GetGridProvider: IContentGridProvider;
begin
  if not Supports(Provider, IContentGridProvider, Result) then
    Result := nil;
end;

procedure TContentGrid.GridChanged;
begin
  FChanged := True;
  UpdateChanged;
end;

procedure TContentGrid.StartTimer;
begin
  if FTimerActive then
    Exit;
  FTimerActive := True;
  SetTimer(Handle, 0, 33, nil);
end;

procedure TContentGrid.StopTimer;
begin
  if not FTimerActive then
    Exit;
  FTimerActive := False;
  KillTimer(Handle, 0);
end;

procedure TContentGrid.DoUpdate;
var
  C: TRect;
  D: TScrollData;
begin
  if not FChanged then Exit;
  FChanged := False;
  C := FManager.GetCell(ColCount - 1, RowCount - 1);
  D := GetScrollData;
  D.Width := C.Right;
  D.Height := C.Bottom;
  SetScrollData(D);
  if HandleAllocated then
    if DoubleBuffered then
      Invalidate
    else
      InvalidateRect(Handle, nil, True);
end;

function TContentGrid.GetColWidths(Col: Integer): Integer;
begin
  Result := FManager.ColWidth[Col];
end;

procedure TContentGrid.SetColWidths(Col: Integer; Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FManager.ColWidth[Col] then
  begin
    FManager.ColWidth[Col] := Value;
    GridChanged;
  end;
end;

function TContentGrid.GetRowHeights(Row: Integer): Integer;
begin
  Result := FManager.RowHeight[Row];
end;

procedure TContentGrid.SetRowHeights(Row: Integer; Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FManager.RowHeight[Row] then
  begin
    FManager.RowHeight[Row] := Value;
    GridChanged;
  end;
end;

function TContentGrid.GetColCount: Integer;
begin
  Result := FManager.ColCount;
end;

procedure TContentGrid.SetColCount(Value: Integer);
begin
  if FSingleColumn then
    Value := 1;
  if Value < 1 then
    Value := 0;
  FManager.ColCount := Value;
  FSelection.X := -1;
  FSelection.Y := -1;
  GridChanged;
end;

function TContentGrid.GetRowCount: Integer;
begin
  Result := FManager.RowCount;
end;

procedure TContentGrid.SetRowCount(Value: Integer);
begin
  if Value < 1 then
    Value := 0;
  FManager.RowCount := Value;
  FSelection.X := -1;
  FSelection.Y := -1;
  GridChanged;
end;

function TContentGrid.GetDefColWidth: Integer;
begin
  Result := FManager.DefColWidth;
end;

procedure TContentGrid.SetDefColWidth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FManager.DefColWidth then
  begin
    FManager.DefColWidth := Value;
    GridChanged;
  end;
end;

function TContentGrid.GetDefRowHeight: Integer;
begin
  Result := FManager.DefRowHeight;
end;

procedure TContentGrid.SetDefRowHeight(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FManager.DefRowHeight then
  begin
    FManager.DefRowHeight := Value;
    GridChanged;
  end;
end;

{ For testing purposes

procedure TContentGrid.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  FillRectColor(Msg.DC, ClientRect, Color);
  Msg.Result := 1;
end; }

procedure TContentGrid.WMTimer(var Msg: TWMTimer);
var
  A: TGridCoord;
  P: TPoint;
  I: Integer;
begin
  A := Selection;
  P := ScreenToClient(Mouse.CursorPos);
  if P.Y < 0 then
  begin
    I := 0;
    repeat
      if A.Y = 0 then Break;
      Dec(A.Y);
      Selection := A;
    until Compare(Selection, A) or (A.Y < I);
    if (A.Y < I) and (Selection.Y > I) then
      ScrollTo(GetScrollData.Left, 0);
  end
  else if P.Y > ScrollHeight then
  begin
    I := RowCount - 1;
    repeat
      Inc(A.Y);
      Selection := A;
    until (Selection.Y = A.Y) or (A.Y > I);
    if (A.Y > I) and (Selection.Y < I) then
      ScrollTo(GetScrollData.Left, GetScrollData.Height - ScrollHeight);
  end
  else if FSingleColumn then
  begin
  end
  else if P.X < 0 then
  begin
    I := 0;
    repeat
      Dec(A.X);
      Selection := A;
    until Compare(Selection, A) or (A.X < I);
    if A.X < I then
      ScrollTo(0, GetScrollData.Top);
  end
  else if P.X > ScrollWidth then
  begin
    I := ColCount - 1;
    repeat
      Inc(A.X);
      Selection := A;
    until Compare(Selection, A) or (A.X > I);
    if A.X > I then
      ScrollTo(GetScrollData.Width - ScrollWidth, GetScrollData.Top);
  end
end;

procedure TContentGrid.WMSetFocus(var Msg: TWMSetFocus);
var
  R: TRect;
begin
  with Selection do
    R := RectFromCoord(X, Y);
  InvalidateRect(Handle, @R, True);
  with FHotTrack do
    R := RectFromCoord(X, Y);
  InvalidateRect(Handle, @R, True);
end;

procedure TContentGrid.WMKillFocus(var Msg: TWMKillFocus);
var
  R: TRect;
begin
  with Selection do
    R := RectFromCoord(X, Y);
  InvalidateRect(Handle, @R, True);
  with FHotTrack do
    R := RectFromCoord(X, Y);
  InvalidateRect(Handle, @R, True);
end;

function TContentGrid.RectFromCoord(X, Y: Integer): TRect;
var
  D: TScrollData;
begin
  Result := FManager.GetCell(X, Y);
  D := GetScrollData;
  OffsetRect(Result, -D.Left, -D.Top);
  if FSingleColumn then
  begin
    Result.Left := 0;
    Result.Right := ScrollWidth;
  end;
end;

function TContentGrid.CoordFromPoint(X, Y: Integer): TGridCoord;
var
  D: TScrollData;
  Min, Max: Integer;
  R: TRect;
  I: Integer;
begin
  Result.X := -1;
  Result.Y := -1;
  D := GetScrollData;
  X := X + D.Left;
  Y := Y + D.Top;
  Min := 0;
  Max := ColCount;
  if FSingleColumn then
    Result.X := 0
  else
  repeat
    I := (Min + Max) div 2;
    R := FManager.GetCell(I, 0);
    if X < R.Left then
      Max := I
    else if X > R.Right then
      Min := I
    else
    begin
      Result.X := I;
      Break;
    end;
    if Min = Max then
      Break;
    if Max - Min = 1 then
      Max := Min;
  until False;
  Min := 0;
  Max := RowCount;
  repeat
    I := (Min + Max) div 2;
    R := FManager.GetCell(0, I);
    if Y < R.Top then
      Max := I
    else if Y > R.Bottom then
      Min := I
    else
    begin
      Result.Y := I;
      Break;
    end;
    if Min = Max then
      Break;
    if Max - Min = 1 then
      Max := Min;
  until False;
end;

procedure TContentGrid.Paint;
var
  Rect: TRect;
  Min, Max: TGridCoord;
  DefaultDraw: Boolean;
  State: TDrawState;
  X, Y: Integer;
begin
  if DoubleBuffered and (GridProvider <> nil) then
    GridProvider.DrawBackground(Self, Canvas.Handle);
  IntersectRect(Rect, Canvas.ClipRect, ClientRect);
  Min := CoordFromPoint(Rect.Left, Rect.Top);
  if Min.X < 0 then Min.X := 0;
  if Min.Y < 0 then Min.Y := 0;
  Max := CoordFromPoint(Rect.Right, Rect.Bottom);
  if Max.X < 0 then Max.X := ColCount - 1;
  if Max.Y < 0 then Max.Y := RowCount - 1;
  for Y := Min.Y to Max.Y do
    for X := Min.X to Max.X  do
    begin
      DefaultDraw := True;
      if X = Min.X then
      begin
        Rect := RectFromCoord(0, Y);
        Rect.Right := RectFromCoord(ColCount - 1, Y).Right;
        if GridProvider <> nil then
          GridProvider.DrawRow(Self, Y, Rect, DefaultDraw);
        if Assigned(FOnDrawRow) then
          FOnDrawRow(Self, Y, Rect, DefaultDraw);
        if not DefaultDraw then
          Break;
      end;
      Rect := RectFromCoord(X, Y);
      State := [];
      if (FSelection.X = X) and (FSelection.Y = Y) then
      begin
        Include(State, dsSelected);
        if Focused then
          Include(State, dsFocused);
        if FMouseTrack then
        begin
          Include(State, dsPressed);
          Include(State, dsHot);
        end;
      end;
      if (FHotTrack.X = X) and (FHotTrack.Y = Y) then
        Include(State, dsHot);
      if GridProvider <> nil then
        GridProvider.DrawCell(Self, X, Y, Rect, State);
      if Assigned(FOnDrawCell) then
        FOnDrawCell(Self, X, Y, Rect, State);
    end;
end;

procedure TContentGrid.SetHotTrack(const Value: TGridCoord);
var
  Allow: Boolean;
  R: TRect;
begin
  if (Value.X <> FHotTrack.X) or (Value.Y <> FHotTrack.Y) then
  begin
    if (FHotTrack.X > -1) and (FHotTrack.Y > -1) then
    begin
      R := RectFromCoord(FHotTrack.X, FHotTrack.Y);
      InvalidateRect(Handle, @R, True);
    end;
    if (Value.X < 0) or (Value.Y < 0) then
    begin
      FHotTrack.X := -1;
      FHotTrack.Y := -1;
      Exit;
    end;
    Allow := True;
    if GridProvider <> nil then
      GridProvider.HotTrack(Self, Value.X, Value.Y, Allow);
    if Assigned(FOnHotTrack) then
      FOnHotTrack(Self, Value.X, Value.Y, Allow);
    if not Allow then
    begin
      FHotTrack.X := -1;
      FHotTrack.Y := -1;
      Exit;
    end;
    FHotTrack := Value;
    R := RectFromCoord(FHotTrack.X, FHotTrack.Y);
    InvalidateRect(Handle, @R, True);
  end;
end;

procedure TContentGrid.SetSelection(Value: TGridCoord);

  function ValueOutOfRange: Boolean;
  begin
    Result := (Value.X < 0) or (Value.X > ColCount - 1) or
      (Value.Y < 0) or (Value.Y > RowCount -  1);
  end;

var
  Allow: Boolean;
  P: TMergeCellParams;
  R: TRect;
  D: TScrollData;
begin
  if (Value.X < 0) or (Value.Y < 0)  then
  begin
    Value.X := -1;
    Value.Y := -1;
  end;
  if (Value.X = FSelection.X) and (Value.Y = FSelection.Y) then
    Exit;
  if FSingleColumn and (Value.Y < 0) then
  begin
    Allow := True;
    if GridProvider <> nil then
      GridProvider.Select(Self, Value.X, Value.Y, Allow);
    if Assigned(FOnSelection) then
      FOnSelection(Self, Value.X, Value.Y, Allow);
    if not Allow then Exit;
    R := RectFromCoord(FSelection.X, FSelection.Y);
    InvalidateRect(Handle, @R, True);
    FSelection := Value;
    Exit;
  end;
  if (ValueOutOfRange or (Value.X = FSelection.X) and (Value.Y = FSelection.Y)) then
    Exit;
  if Assigned(FOnMergeCell) then
  begin
    P.Col := Value.X;
    P.Row := Value.Y;
    P.Width := 1;
    P.Height := 1;
    P.Merged := False;
    FOnMergeCell(Self, P);
    if P.Merged then
    begin
      Value.X := P.Col;
      Value.Y := P.Row;
    end;
  end;
  if ValueOutOfRange or ((Value.X = FSelection.X) and (Value.Y = FSelection.Y)) then
    Exit;
  Allow := True;
  if GridProvider <> nil then
    GridProvider.Select(Self, Value.X, Value.Y, Allow);
  if Assigned(FOnSelection) then
    FOnSelection(Self, Value.X, Value.Y, Allow);
  if not Allow then Exit;
  R := RectFromCoord(FHotTrack.X, FHotTrack.Y);
  InvalidateRect(Handle, @R, True);
  FHotTrack.X := -1;
  FHotTrack.Y := -1;
  R := RectFromCoord(FSelection.X, FSelection.Y);
  InvalidateRect(Handle, @R, True);
  FSelection := Value;
  R := RectFromCoord(FSelection.X, FSelection.Y);
  InvalidateRect(Handle, @R, True);
  D := GetScrollData;
  OffsetRect(R, D.Left, D.Top);
  if FAutoScroll then
    ScrollTo(R);
  LineHeight := HeightOf(R);
  LineWidth := WidthOf(R);
  if FSingleColumn and (Value.Y = RowCount - 1) then
    Invalidate;
end;

procedure TContentGrid.SetSingleColumn(Value: Boolean);
var
  A: TGridCoord;
begin
  if Value <> FSingleColumn then
  begin
    FSingleColumn := Value;
    if FSingleColumn then
      ColCount := 1;
    A.X := -1;
    A.Y := -1;
    Selection := A;
    Invalidate;
  end;
end;

function TContentGrid.GetItemIndex: Integer;
begin
  if FSingleColumn then
    if RowCount = 0 then
      Result := -1
    else
      Result := Selection.Y
  else
    Result := -1;
end;

procedure TContentGrid.SetItemIndex(Value: Integer);
var
  A: TGridCoord;
begin
  if FSingleColumn then
  begin
    if Value < 0 then
    begin
      A.X := -1;
      A.Y := -1;
    end
    else
    begin
      A.X := 0;
      A.Y := Value;
    end;
    Selection := A;
  end;
end;

procedure TContentGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  A, B: TGridCoord;
  I: Integer;
begin
  inherited KeyDown(Key, Shift);
  A := Selection;
  if (A.X < 0) or (A.Y < 0) then Exit;
  if ssCtrl in Shift then
    case Key of
      VK_HOME:
        begin
          A.X := 0;
          A.Y := 0;
          Selection := A;
        end;
      VK_END:
        begin
          A.X := ColCount - 1;
          A.Y := RowCount - 1;
          Selection := A;
        end;
    end
  else
    case Key of
      VK_HOME:
        begin
          A.X := 0;
          Selection := A;
        end;
      VK_END:
        begin
          A.X := ColCount - 1;
          Selection := A;
        end;
      VK_UP:
        begin
          I := 0;
          repeat
            if A.Y = 0 then Break;
            Dec(A.Y);
            Selection := A;
          until (Selection.Y = A.Y) or (A.Y < I);
          if (A.Y < I) and (Selection.Y > I) then
            ScrollTo(GetScrollData.Left, 0);
        end;
      VK_DOWN:
        begin
          I := RowCount - 1;
          repeat
            Inc(A.Y);
            Selection := A;
          until (Selection.Y = A.Y) or (A.Y > I);
          if (A.Y > I) and (Selection.Y < I) then
            ScrollTo(GetScrollData.Left, GetScrollData.Height - ScrollHeight);
        end;
      VK_LEFT:
        if not FSingleColumn then
        repeat
          Dec(A.X);
          if A.X < 0 then
          begin
            A.X := ColCount - 1;
            Dec(A.Y);
          end;
          Selection := A;
          if A.Y < 0 then
          begin
            ScrollTo(0, 0);
            Break;
          end;
        until Compare(A, Selection);
      VK_RIGHT:
        if not FSingleColumn then
        repeat
          Inc(A.X);
          if A.X > ColCount - 1 then
          begin
            A.X := 0;
            Inc(A.Y);
          end;
          Selection := A;
          if A.Y > RowCount - 1 then
          begin
            with GetScrollData do
              ScrollTo(Width - ScrollWidth, Height - ScrollHeight);
            Break;
          end;
        until Compare(A, Selection);
      VK_PRIOR:
        begin
          A.Y := CoordFromPoint(0, -ScrollHeight).Y;
          B.Y := CoordFromPoint(0, 1).Y;
          if B.Y < Selection.Y then
            A.Y := B.Y;
          Selection := A;
        end;
      VK_NEXT:
        begin
          A.Y := CoordFromPoint(0, ScrollHeight * 2).Y;
          B.Y := CoordFromPoint(0, ScrollHeight).Y;
          if B.Y > Selection.Y then
            A.Y := B.Y
          else if A.Y < 0 then
            A.Y := RowCount - 1;
          Selection := A;
        end;
    end;
end;

procedure TContentGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  G: TGridCoord;
  R: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if CanFocus and (Button = mbLeft) then
  begin
    G.X := -1;
    G.Y := -1;
    SetHotTrack(G);
    SetFocus;
    G := CoordFromPoint(X, Y);
    if FSingleColumn and (G.Y < 0) then
      G := Selection;
    Selection := G;
    if Compare(Selection, G) then
    begin
      R := RectFromCoord(G.X, G.Y);
      InvalidateRect(Handle, @R, True); // False
    end;
    FMouseTrack := True;
  end;
end;

procedure TContentGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  G: TGridCoord;
begin
  inherited MouseMove(Shift, X, Y);
  if FMouseTrack then
    if (X < 0) or (Y < 0) or (X > ScrollWidth) or (Y > ScrollHeight) then
      StartTimer
    else
    begin
      StopTimer;
      G := CoordFromPoint(X, Y);
      if (not FSingleColumn) or (G.Y > -1) then
        Selection := G;
    end
  else
    SetHotTrack(CoordFromPoint(X, Y));
end;

procedure TContentGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  G: TGridCoord;
  R: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    StopTimer;
    G := Selection;
    R := RectFromCoord(G.X, G.Y);
    InvalidateRect(Handle, @R, True); // False
    FMouseTrack := False;
  end;
end;

procedure TContentGrid.CMMouseLeave(var Msg: TMessage);
var
  G: TGridCoord;
begin
  inherited;
  G.X := -1;
  G.Y := -1;
  SetHotTrack(G);
end;

procedure TContentGrid.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if (not DoubleBuffered) and (GridProvider <> nil) then
  begin
    GridProvider.DrawBackground(Self, Msg.DC);
    Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TContentGrid.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

function TContentGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then
    if WheelDelta > 0 then
      SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0)
    else
      SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TContentGrid.Resize;
begin
  if FSingleColumn then
    ColWidths[0] := ScrollWidth - GetSystemMetrics(SM_CXVSCROLL);
  if GridProvider <> nil then
    GridProvider.Resize(Self);
  inherited Resize;
end;

constructor TImageListGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TImageListGrid.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TImageListGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

procedure TImageListGrid.ImageListChange(Sender: TObject);
begin
  if GridProvider <> nil then
    GridProvider.Init(Self);
  Invalidate;
end;

procedure TImageListGrid.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    if GridProvider <> nil then
      GridProvider.Init(Self);
    Repaint;
  end;
end;

{ TDefaultImageListGridProvider }

type
  TDefaultImageListGridProvider = class(TControlProvider, IContentGridProvider)
  private
    procedure GridChanged(Control: TControl);
  protected
    procedure Init(Control: TControl); override;
    procedure Resize(Grid: TContentGrid);
    procedure HitTest(Grid: TContentGrid; X, Y: Integer;
      var HitTest: TGridHitTest);
    procedure HotTrack(Grid: TContentGrid; Col, Row: Integer;
      var Allow: Boolean);
    procedure Select(Grid: TContentGrid; Col, Row: Integer;
      var Allow: Boolean);
    procedure DrawBackground(Grid: TContentGrid; DC: HDC);
    procedure DrawRow(Grid: TContentGrid; Row: Integer; Rect: TRect;
      var DefaultDraw: Boolean);
    procedure DrawCell(Grid: TContentGrid; Col, Row: Integer; Rect: TRect;
      State: TDrawState);
  end;

procedure TDefaultImageListGridProvider.GridChanged(Control: TControl);
const
  ImageBorder = 32;
var
  ImageGrid: TImageListGrid absolute Control;
  WasAutoScroll: Boolean;
  S: TGridCoord;
  W, H, I, J: Integer;
begin
  if not (Control is TImageListGrid) then Exit;
  with ImageGrid do
  begin
    BeginUpdate;
    try
      WasAutoScroll := AutoScroll;
      AutoScroll := False;
      S := Selection;
      try
        if (Images = nil) or (Images.Count = 0) then
        begin
          ColCount := 0;
          RowCount := 0;
          Exit;
        end;
        J := S.X + S.Y * ColCount;
        W := Images.Width;
        H := Images.Height;
        I := Images.Count;
        DefColWidth := W + ImageBorder div 2;
        DefRowHeight := H + ImageBorder;
        ColCount := ScrollWidth div DefColWidth;
        if ColCount < 1 then
          ColCount := 1;
        if I = 0 then
          RowCount := 1
        else
          RowCount := (I - 1) div ColCount + 1;
        if J > 0 then
        begin
          S.Y := J div ColCount;
          S.X := J - S.Y * ColCount;
        end;
      finally
        Selection := S;
        AutoScroll := WasAutoScroll;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TDefaultImageListGridProvider.Init(Control: TControl);
begin
  GridChanged(Control);
end;

procedure TDefaultImageListGridProvider.Resize(Grid: TContentGrid);
begin
  if Grid.ScrollWidth div Grid.DefColWidth <> Grid.ColCount then
    GridChanged(Grid);
end;

procedure TDefaultImageListGridProvider.HitTest(Grid: TContentGrid; X, Y: Integer;
  var HitTest: TGridHitTest);
begin
end;

procedure TDefaultImageListGridProvider.HotTrack(Grid: TContentGrid; Col, Row: Integer;
  var Allow: Boolean);
begin
end;

procedure TDefaultImageListGridProvider.Select(Grid: TContentGrid; Col, Row: Integer;
  var Allow: Boolean);
var
  ImageGrid: TImageListGrid absolute Grid;
begin
  if (not (Grid is TImageListGrid)) or (ImageGrid.Images = nil) then
  begin
    Allow := False;
    Exit;
  end;
  Allow := Col + Row * ImageGrid.ColCount < ImageGrid.Images.Count;
end;

procedure TDefaultImageListGridProvider.DrawBackground(Grid: TContentGrid; DC: HDC);
var
  R: TRect;
  B: HBRUSH;
  X, Y: Integer;
begin
  R := Grid.ClientRect;
  B := GetBrush(clWhite, clSilver, 10);
  Y := GetScrollPos(Grid.Handle, SB_VERT);
  X := GetScrollPos(Grid.Handle, SB_HORZ);
  SetBrushOrgEx(Dc, -X, -Y, nil);
  FillRect(DC, R, B);
  DeleteObject(B);
end;

procedure TDefaultImageListGridProvider.DrawRow(Grid: TContentGrid; Row: Integer; Rect: TRect;
  var DefaultDraw: Boolean);
begin
end;

procedure TDefaultImageListGridProvider.DrawCell(Grid: TContentGrid; Col, Row: Integer; Rect: TRect;
  State: TDrawState);
var
  ImageGrid: TImageListGrid absolute Grid;
  DC: HDC;
  F: HFONT;
  I: Integer;
begin
  if (not (Grid is TImageListGrid)) or (ImageGrid.Images = nil) then Exit;
  with ImageGrid do
  begin
    DC := ImageGrid.Canvas.Handle;
    I := Col + Row * ColCount;
    if I > Images.Count - 1 then Exit;
    InflateRect(Rect, -2, -2);
    F := SelectFontStyle(DC, [fsBold]);
    SetTextColor(DC, ColorToRGB(clWindowText));
    if dsPressed in State then
    begin
      FillRectColorAlpha(DC, Rect, clHighlight, $D0);
      FillRectOutline(DC, Rect, clHighlight);
      SetTextColor(DC, ColorToRGB(clHighlightText));
    end
    else
    if dsSelected in State then
    begin
      FillRectColorAlpha(DC, Rect, clHighlight, $A0);
      FillRectOutline(DC, Rect, clHighlight);
      SetTextColor(DC, ColorToRGB(clHighlightText));
    end
    else if dsHot in State then
    begin
      FillRectColorAlpha(DC, Rect, clHighlight, $20);
      FillRectOutline(DC, Rect, clHighlight);
    end;
    Rect.Top := Rect.Top + (HeightOf(Rect) - Images.Height) div 2 - FontHeight(DC) div 2;
    Images.Draw(Canvas, Rect.Left + (WidthOf(Rect) - Images.Width) div 2,
      Rect.Top, I);
    Rect.Top := Rect.Top + Images.Height;
    DrawCaption(DC, IntToStr(I), Rect, drCenter);
    OverwriteObject(DC, F);
  end;
end;



  (* TTestGrid = class(TScrollWindow)
  private
    FGridSize: Integer;
    FSelection: TGridCoord;
    FMouseTrack: Boolean;
    procedure SetGridSize(Value: Integer);
    procedure SetSelection(Value: TGridCoord);
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    function RectFromCoord(X, Y: Integer): TRect;
    function CoordFromPoint(X, Y: Integer): TGridCoord;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property GridSize: Integer read FGridSize write SetGridSize;
    property Selection: TGridCoord read FSelection write SetSelection;
  end; *)


  { TTestGrid }

(* function TTestGrid.RectFromCoord(X, Y: Integer): TRect;
var
  D: TScrollData;
begin
  D := GetScrollData;
  with Result do
  begin
    Left := X * FGridSize - D.Left;
    Top := Y * FGridSize - D.Top;
    if Left + FGridSize > D.Width then
      Left := D.Width - FGridSize;
    if Top + FGridSize > D.Height then
      Top := D.Height - FGridSize;
    Right := Left + FGridSize;
    Bottom := Top + FGridSize;
  end;
end;

function TTestGrid.CoordFromPoint(X, Y: Integer): TGridCoord;
var
  D: TScrollData;
begin
  D := GetScrollData;
  Result.X := (D.Left + X) div FGridSize;
  Result.Y := (D.Top + Y) div FGridSize;
end;

constructor TTestGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LineHeight := 5;
  LineWidth := 5;
  Width := 200;
  Height := 200;
  GridSize := 30;
end;

procedure TTestGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  S: TGridCoord;
begin
  inherited KeyDown(Key, Shift);
  S := Selection;
  case Key of
    VK_UP: Dec(S.Y);
    VK_DOWN: Inc(S.Y);
    VK_LEFT: Dec(S.X);
    VK_RIGHT: Inc(S.X);
  end;
  Selection := S;
end;

procedure TTestGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    SetFocus;
    Selection := CoordFromPoint(X, Y);
    FMouseTrack := True;
  end;
end;

procedure TTestGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if FMouseTrack then
    Selection := CoordFromPoint(X, Y);
end;

procedure TTestGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FMouseTrack := False;
end;

procedure TTestGrid.Paint;
var
  DC: HDC;
  R: TRect;
  X, Y, W, H: Integer;
  D: TScrollData;
  I: Integer;
begin
  DC := Canvas.Handle;
  R := ClientRect;
  W := WidthOf(R);
  H := HeightOf(R);
  D := GetScrollData;
  I := D.Width div FGridSize;
  for Y := 0 to D.Height div FGridSize - 1 do
    for X := 0 to D.Width div FGridSize  - 1 do
    begin
      R.Left := X * FGridSize;
      R.Top := Y * FGridSize;
      R.Right := R.Left + FGridSize;
      R.Bottom := R.Top + FGridSize;
      OffsetRect(R, -D.Left, -D.Top);
      if (R.Right < 0) or (R.Bottom < 0) then
        Continue;
      if R.Left > W - 1 then
        Continue;
      if R.Top > H - 1 then
        Break;
      if (X = FSelection.X) and (Y = FSelection.Y) then
        FillRectColor(DC, R, clHighlight)
      else if Odd(X + Y) then
        FillRectColor(DC, R, clWhite)
      else
        FillRectColor(DC, R, clSilver);
      DrawCaption(DC, IntToStr(X + Y * I), R, drCenter);
    end;
end;

procedure TTestGrid.SetGridSize(Value: Integer);
begin
  if Value <> FGridSize then
  begin
    FGridSize := Value;
    SetScrollData(ScrollData(0, 0, FGridSize * 20, FGridSize * 20));
  end;
end;

procedure TTestGrid.SetSelection(Value: TGridCoord);
var
  R: TRect;
  D: TScrollData;
begin
  if Value.X < 0 then Value.X := 0 else if Value.X > 19 then Value.X := 19;
  if Value.Y < 0 then Value.Y := 0 else if Value.Y > 19 then Value.Y := 19;
  if (Value.X <> FSelection.X) or (Value.Y <> FSelection.Y) then
  begin
    R := RectFromCoord(FSelection.X, FSelection.Y);
    InvalidateRect(Handle, @R, False);
    FSelection := Value;
    R := RectFromCoord(FSelection.X, FSelection.Y);
    InvalidateRect(Handle, @R, False);
    D := GetScrollData;
    OffsetRect(R, D.Left, D.Top);
    ScrollTo(R);
  end;
end;

procedure TTestGrid.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end; *)

initialization
  RegisterDefaultProvider(TDefaultImageListGridProvider, TImageListGrid);
end.
