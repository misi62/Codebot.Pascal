
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit TabCtrls;

interface

{$I CODEBOT.INC}

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  BaseTypes, GraphTools, FormTools, SuplCtrls;

{ TDrawTabItem }

type
  TDrawTabItem = class(TCollectionItem)
  private
    FTabRect: TRect;
    FCloseRect: TRect;
    FCaption: TCaption;
    FEnabled: Boolean;
    FVisible: Boolean;
    procedure SetCaption(Value: TCaption);
    procedure SetEnabled(Value: Boolean);
    procedure SetVisible(Value: Boolean);
	protected
    property TabRect: TRect read FTabRect write FTabRect;
  	property CloseRect: TRect read FCloseRect write FCloseRect;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
	published
    property Caption: TCaption read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TDrawTabItems }

  TDrawTabItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TDrawTabItem;
    procedure Put(Index: Integer; Value: TDrawTabItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TDrawTabItem;
    function FindItemID(ID: Integer): TDrawTabItem;
    function Insert(Index: Integer): TDrawTabItem;
    property Items[Index: Integer]: TDrawTabItem read Get write Put; default;
  end;

{ TCustomDrawTabs }

  TTabRects = record
    Prior: TRect;
    Tab: TRect;
    Close: TRect;
  end;

  TMeasureTabEvent = procedure(Sender: TObject; Index: Integer;
    var Rects: TTabRects; State: TDrawState) of object;
  TDrawTabEvent = procedure(Sender: TObject; Index: Integer; Rect: TRect;
    State: TDrawState) of object;

  TTabCloseAction = (taNone, taHide, taFree);
  TCloseTabEvent = procedure(Sender: TObject; Index: Integer;
    var Action: TTabCloseAction) of object;

  TDrawTabStyler = class(TComponent)
  protected
    procedure MeasureTab(Sender: TObject; Index: Integer;
      var Rects: TTabRects; State: TDrawState); virtual; abstract;
    procedure DrawBackground(Sender: TObject); virtual; abstract;
    procedure DrawClose(Sender: TObject; Index: Integer; Rect: TRect;
      State: TDrawState); virtual; abstract;
    procedure DrawTab(Sender: TObject; Index: Integer; Rect: TRect;
      State: TDrawState);  virtual; abstract;
  end;

  TCustomDrawTabs = class(TCustomControl)
  private
    FTabs: TDrawTabItems;
    FCloseDownIndex: Integer;
    FCloseHotIndex: Integer;
    FTabDownIndex: Integer;
    FTabHotIndex: Integer;
    FTabIndex: Integer;
    { FTabLeft: Integer; }
    FStyler: TDrawTabStyler;
    FOnCloseTab: TCloseTabEvent;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawClose: TDrawTabEvent;
    FOnDrawTab: TDrawTabEvent;
    FOnMeasureTab: TMeasureTabEvent;
    FOnSelectItem: TNotifyEvent;
    procedure SetCloseHotIndex(Value: Integer);
    procedure SetTabHotIndex(Value: Integer);
    procedure SetTabs(Value: TDrawTabItems);
    procedure SetTabIndex(Value: Integer);
    { procedure SetTabLeft(Value: Integer); }
    procedure SetStyler(Value: TDrawTabStyler);
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
		procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    function GetStyler: TDrawTabStyler;
    procedure CloseTab(Index: Integer); virtual;
    procedure DrawBackground; virtual;
    procedure DrawClose(Index: Integer; Rect: TRect; State: TDrawState); virtual;
    procedure DrawTab(Index: Integer; Rect: TRect; State: TDrawState); virtual;
    procedure MeasureTab(Index: Integer; var Rects: TTabRects; State: TDrawState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure TabsChanged;
  	procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property CloseHotIndex: Integer read FCloseHotIndex write SetCloseHotIndex;
    property TabIndex: Integer read FTabIndex write SetTabIndex;
    property TabHotIndex: Integer read FTabHotIndex write SetTabHotIndex;
    { property TabLeft: Integer read FTabLeft write SetTabLeft; }
    property Styler: TDrawTabStyler read FStyler write SetStyler;
    property OnCloseTab: TCloseTabEvent read FOnCloseTab write FOnCloseTab;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawClose: TDrawTabEvent read FOnDrawClose write FOnDrawClose;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnMeasureTab: TMeasureTabEvent read FOnMeasureTab write FOnMeasureTab;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
  	property Tabs: TDrawTabItems read FTabs write SetTabs;
  end;

{ TDrawTabs }

  TDrawTabs = class(TCustomDrawTabs)
  published
    property Align;
    property Anchors;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnDblClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property Styler;
    property TabIndex;
  	property Tabs;
    property OnSelectItem;
    property OnCloseTab;
    property OnDrawBackground;
    property OnDrawClose;
    property OnDrawTab;
    property OnMeasureTab;
  end;

implementation

{$R tabctrls.res}

{ TDrawTabItem }

constructor TDrawTabItem.Create(Collection: TCollection);
begin
	inherited Create(Collection);
  FEnabled := True;
	FVisible := True;
end;

procedure TDrawTabItem.Assign(Source: TPersistent);
var
  EditItem: TDrawTabItem absolute Source;
begin
  if Source is TDrawTabItem then
  begin
    FCaption := EditItem.Caption;
    FVisible := EditItem.Visible;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TDrawTabItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TDrawTabItem.SetEnabled(Value: Boolean);
begin
  if Value <> Fenabled then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TDrawTabItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

{ TDrawTabItems }

constructor TDrawTabItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TDrawTabItem);
end;

function TDrawTabItems.Add: TDrawTabItem;
begin
  Result := TDrawTabItem(inherited Add);
end;

function TDrawTabItems.FindItemID(ID: Integer): TDrawTabItem;
begin
  Result := TDrawTabItem(inherited FindItemID(ID));
end;

function TDrawTabItems.Insert(Index: Integer): TDrawTabItem;
begin
  Result := TDrawTabItem(GetItem(Index));
end;

procedure TDrawTabItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TCustomDrawTabs then
  	TCustomDrawTabs(GetOwner).TabsChanged;
end;

function TDrawTabItems.Get(Index: Integer): TDrawTabItem;
begin
  Result := TDrawTabItem(GetItem(Index));
end;

procedure TDrawTabItems.Put(Index: Integer; Value: TDrawTabItem);
begin
  SetItem(Index, Value);
end;

{ TJavaTabStyler }

type
  TJavaTabStyler = class(TDrawTabStyler)
  private
    FCloseCold: TAlphaImage;
    FCloseHot: TAlphaImage;
    FClosePressed: TAlphaImage;
  protected
    procedure MeasureTab(Sender: TObject; Index: Integer;
      var Rects: TTabRects; State: TDrawState); override;
    procedure DrawBackground(Sender: TObject); override;
    procedure DrawClose(Sender: TObject; Index: Integer; Rect: TRect;
      State: TDrawState); override;
    procedure DrawTab(Sender: TObject; Index: Integer; Rect: TRect;
      State: TDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  CaptionBorder = 9;

constructor TJavaTabStyler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCloseCold := TAlphaImage.Create;
  //FCloseCold.LoadFromResourceID(8821);
  FCloseHot := TAlphaImage.Create;
  //FCloseHot.LoadFromResourceID(8902);
  FClosePressed := TAlphaImage.Create;
  //FClosePressed.LoadFromResourceID(8903);
end;

destructor TJavaTabStyler.Destroy;
begin
  FCloseCold.Free;
  FCloseHot.Free;
  FClosePressed.Free;
  inherited Destroy;
end;

procedure TJavaTabStyler.MeasureTab(Sender: TObject; Index: Integer;
  var Rects: TTabRects; State: TDrawState);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  DC: HDC;
  F: HFONT;
begin
  DC := DrawTab.Canvas.Handle;
  with Rects do
  begin
    if Index = 0 then
    begin
      Tab.Left := CaptionBorder;
      Tab.Bottom := DrawTab.Height;
      Tab.Top := Tab.Bottom - 20;
    end
    else
    begin
      Tab := Prior;
      Slide(Tab, drRight, 4);
    end;
    F := 0;
    if dsDefaulted in State then
      F := SelectObject(DC, GetFont(DC, [fsBold]));
    Tab.Right := Tab.Left + CaptionBorder * 4 + CalculateCaptionSize(DC,
      DrawTab.Tabs[Index].Caption).cx;
    if dsDefaulted in State then
      OverwriteObject(DC, F);
    if FCloseCold.Width > 0 then
    begin
      Close := Tab;
      InflateRect(Close, -3, -3);
      Close.Left := Close.Right - HeightOf(Close) - 1;
    end;
  end;
end;

procedure TJavaTabStyler.DrawBackground(Sender: TObject);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  DC: HDC;
  R: TRect;
begin
  DC := DrawTab.Canvas.Handle;
  R := DrawTab.ClientRect;
  DrawGradient(DC, R, Blend(clBtnFace, clBtnShadow, 33), Blend(clBtnFace, clBtnHighlight, 33), drRight);
  R.Top := R.Bottom - 1;
  FillRectColor(DC, R, Blend(clBtnFace, clBtnShadow, 33));
end;

procedure TJavaTabStyler.DrawClose(Sender: TObject; Index: Integer; Rect: TRect;
  State: TDrawState);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  Graphic: TAlphaImage;
begin
  Graphic := FCloseHot;
  Graphic.Opacity := $A0;
  if dsDisabled in State then
  begin
    Graphic := FCloseCold;
    Graphic.Opacity := $80;
  end;
  if dsHot in State then
  begin
    Graphic.Opacity := $FF;
    if dsPressed in State then
      Graphic := FClosePressed
    else
      Graphic := FCloseHot
  end
  else if dsDefaulted in State then
  begin
    Graphic.Opacity := $FF;
    Graphic := FCloseCold;
  end
  else
    Graphic := FCloseCold;
  DrawTab.Canvas.Draw(Rect.Left, Rect.Top, Graphic);
end;

procedure TJavaTabStyler.DrawTab(Sender: TObject; Index: Integer; Rect: TRect;
  State: TDrawState);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  DC: HDC;
  Tab: array[0..6] of TPoint;
  R: TRect;
  B: HBRUSH;
  P: HPEN;
  F: HFONT;
begin
  DC := DrawTab.Canvas.Handle;
  Tab[0] := Point(Rect.Right, Rect.Bottom);
  Tab[1] := Point(Rect.Right, Rect.Top + 4);
  Tab[2] := Point(Rect.Right - 2, Rect.Top + 1);
  Tab[3] := Point(Rect.Right - 4, Rect.Top);
  Tab[4] := Point(Rect.Left + 14, Rect.Top);
  Tab[5] := Point(Rect.Left + 8, Rect.Top + 2);
  Tab[6] := Point(Rect.Left - 10, Rect.Bottom);
  { The other direction ...

  Tab[0] := Point(Rect.Left, Rect.Bottom);
  Tab[1] := Point(Rect.Left, Rect.Top + 4);
  Tab[2] := Point(Rect.Left + 2, Rect.Top + 1);
  Tab[3] := Point(Rect.Left + 4, Rect.Top);
  Tab[4] := Point(Rect.Right - 14, Rect.Top);
  Tab[5] := Point(Rect.Right - 8, Rect.Top + 2);
  Tab[6] := Point(Rect.Right + 10, Rect.Bottom); }
  BeginPath(DC);
  Polygon(DC, Tab, 7);
  EndPath(DC);
  SelectClipPath(DC, RGN_COPY);
  R := Rect;
  R.Left := R.Left - 11;
  if dsDefaulted in State then
    DrawGradient(DC, R, Blend(clBtnFace, clBtnHighlight, 25), clBtnFace, drDown)
  else
  begin
    Dec(R.Bottom);
    FillRectColor(DC, R, clBtnFace);
    begin
      R.Top := R.Bottom - 3;
      DrawGradient(DC, R, clBtnFace, Blend(clBtnFace, clBtnShadow), drDown);
    end;
  end;
  SelectClipRgn(DC, 0);
  P := SelectObject(DC, GetPen(clBtnShadow));
  B := SelectObject(DC, GetStockObject(NULL_BRUSH));
  Polygon(DC, Tab, 7);
  OverwriteObject(DC, P);
  SelectObject(DC, B);
  Rect.Left := Rect.Left + CaptionBorder + 6;
  OffsetRect(Rect, 0, 1);
  if dsHot in State then
    SetTextColor(DC, Blend(clWindowText, clWindow, 60))
  else
    SetTextColor(DC, ColorToRGB(clWindowText));
  F := 0;
  if dsDefaulted in State then
    F := SelectObject(DC, GetFont(DC, [fsBold]));
  DrawCaption(DC, DrawTab.Tabs[Index].Caption, Rect, drLeft, not (dsDisabled in State));
  if dsDefaulted in State then
    OverwriteObject(DC, F);
end;

{$R firetabs.res}

type
  TFirefoxTabStyler = class(TDrawTabStyler)
  private
    FCloseNormal: TAlphaImage;
    FCloseHot: TAlphaImage;
    FCloseDown: TAlphaImage;
    FCloseDisabled: TAlphaImage;
    FFireLeft: TAlphaImage;
    FFireBody: TAlphaImage;
    FFireRight: TAlphaImage;
    FFireLine: TAlphaImage;
  protected
    procedure MeasureTab(Sender: TObject; Index: Integer;
      var Rects: TTabRects; State: TDrawState); override;
    procedure DrawBackground(Sender: TObject); override;
    procedure DrawClose(Sender: TObject; Index: Integer; Rect: TRect;
      State: TDrawState); override;
    procedure DrawTab(Sender: TObject; Index: Integer; Rect: TRect;
      State: TDrawState);  override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure DrawImage(DC: HDC; Image: TAlphaImage; const Rect: TRect);
var
	Func: TBlendFunction;
begin
  if Image.Empty then Exit;
  FillChar(Func, SizeOf(Func), #0);
	Func.SourceConstantAlpha := Image.Opacity;
  if Image.Bitmap.Depth = pd32 then
		Func.AlphaFormat := AC_SRC_ALPHA;
	Windows.AlphaBlend(DC, Rect.Left, Rect.Top, WidthOf(Rect),
  	HeightOf(Rect), Image.Bitmap.DC, 0, 0, Image.Width, Image.Height, Func);
end;

procedure FillRoundRectColor(DC: HDC; const Rect: TRect; Radius: Integer; Color: TColor);
var
  B: HBRUSH;
  P: HPEN;
begin
  B := SelectObject(DC, GetBrush(Color));
  P := SelectObject(DC, GetStockObject(NULL_PEN));
  with Rect do
    RoundRect(DC, Left, Top, Right, Bottom, Radius, Radius);
  SelectObject(DC, P);
  OverwriteObject(DC, B);
end;

constructor TFirefoxTabStyler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCloseNormal := TAlphaImage.Create;
  FCloseHot := TAlphaImage.Create;
  FCloseDown := TAlphaImage.Create;
  FCloseDisabled := TAlphaImage.Create;
  FFireLeft := TAlphaImage.Create;
  FFireBody := TAlphaImage.Create;
  FFireRight := TAlphaImage.Create;
  FFireLine := TAlphaImage.Create;
  FCloseNormal.LoadFromResourceID(1341);
  FCloseHot.LoadFromResourceID(1342);
  FCloseDown.LoadFromResourceID(1343);
  FCloseDisabled.LoadFromResourceID(1344);
  FFireLeft.LoadFromResourceID(1345);
  FFireBody.LoadFromResourceID(1346);
  FFireRight.LoadFromResourceID(1347);
  FFireLine.LoadFromResourceID(1348);
  FCloseDisabled.Opacity := $A0;
  FFireLine.Opacity := $C0;
end;

destructor TFirefoxTabStyler.Destroy;
begin
  FCloseNormal.Free;
  FCloseHot.Free;
  FCloseDown.Free;
  FCloseDisabled.Free;
  FFireLeft.Free;
  FFireBody.Free;
  FFireRight.Free;
  FFireLine.Free;
  inherited Destroy;
end;

const
  FireTabSpace = 22;

procedure TFirefoxTabStyler.MeasureTab(Sender: TObject; Index: Integer;
  var Rects: TTabRects; State: TDrawState);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  DC: HDC;
  F: HFONT;
begin
  DC := DrawTab.Canvas.Handle;
  with Rects do
  begin
    if Index = 0 then
    begin
      Tab.Left := 5;
      Tab.Bottom := DrawTab.Height;
      Tab.Top := Tab.Bottom - FFireLeft.Height - 3;
    end
    else
    begin
      Tab := Prior;
      Slide(Tab, drRight);
    end;
    F := SelectObject(DC, GetFont(DC, [fsBold]));
    Tab.Right := Tab.Left + FireTabSpace * 2 + CalculateCaptionSize(DC,
      DrawTab.Tabs[Index].Caption).cx + FCloseNormal.Width;
    if WidthOf(Tab) < 120 then
      Tab.Right := Tab.Left + 120;
    OverwriteObject(DC, F);
    Close := Tab;
    Close.Top := Tab.Top + 8;
    Close.Left := Tab.Right - FCloseNormal.Width - 8;
    Close.Right := Close.Left + FCloseNormal.Width;
    Close.Bottom := Close.Top + FCloseNormal.Height;
  end;
end;

procedure TFirefoxTabStyler.DrawBackground(Sender: TObject);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  DC: HDC;
  Rect, R: TRect;
begin
  DC := DrawTab.Canvas.Handle;
  Rect := DrawTab.ClientRect;
  R := Rect;
  FillRectColor(DC, R, clBtnFace);
  R.Bottom := R.Bottom - 20;
  DrawGradient(DC, R, Blend(clBtnFace, clBtnShadow, 30), clBtnFace, drDown);
  R := Rect;
  R.Top := R.Bottom - 10;
  DrawGradient(DC, R, Blend(clBtnFace, clBtnShadow, 70), clBtnFace, drUp);
  R.Top := R.Bottom - 3;
  FillRectColor(DC, R, clBtnFace);
  R.Top := R.Bottom - FFireLine.Height;
  DrawImage(DC, FFireLine, R);
end;

procedure TFirefoxTabStyler.DrawClose(Sender: TObject; Index: Integer; Rect: TRect;
  State: TDrawState);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  Image: TAlphaImage;
begin
  if dsDisabled in State then
    Image := FCloseDisabled
  else if [dsPressed, dsHot] * State = [dsPressed, dsHot] then
    Image := FCloseDown
  else if dsHot in State then
    Image := FCloseHot
  else if dsDefaulted in State then
    Image := FCloseNormal
  else
    Image := FCloseDisabled;
  DrawTab.Canvas.Draw(Rect.Left, Rect.Top, Image);
end;

procedure TFirefoxTabStyler.DrawTab(Sender: TObject; Index: Integer; Rect: TRect;
  State: TDrawState);
var
  DrawTab: TCustomDrawTabs absolute Sender;
  DC: HDC;
  R: TRect;
  F: HFONT;
begin
  DC := DrawTab.Canvas.Handle;
  R := Rect;
  InflateRect(R, -1, -1);
  Inc(R.Bottom, 10);
  if [dsDefaulted, dsHot] * State <> [] then
    FillRoundRectColor(DC, R, 10, clBtnFace)
  else
    FillRoundRectColor(DC, R, 10, Blend(clBtnFace, clBtnShadow, 70));
  R := Rect;
  R.Right := R.Left + FFireLeft.Width;
  R.Bottom := R.Top + FFireLeft.Height;
  DrawImage(DC, FFireLeft, R);
  R.Left := R.Right;
  R.Right := Rect.Right - FFireLeft.Width;
  DrawImage(DC, FFireBody, R);
  R.Left := R.Right;
  R.Right := Rect.Right;
  R.Right := R.Left + FFireRight.Width;
  DrawImage(DC, FFireRight, R);
  F := 0;
  if dsDefaulted in State then
    F := SelectObject(DC, GetFont(DC, [fsBold]));
  R := Rect;
  R.Right := R.Right - FCloseNormal.Width;
  DrawCaption(DC, DrawTab.Tabs[Index].Caption, R, drCenter, not (dsDisabled in State));
  if dsDefaulted in State then
    OverwriteObject(DC, F);
  if not (dsDefaulted in State) then
  begin
    R := Rect;
    R.Top := R.Bottom - 3;
    FillRectColor(DC, R, clBtnFace);
    R.Top := R.Bottom - FFireLine.Height;
    DrawImage(DC, FFireLine, R);
  end;
end;

var
  InternalDefaultStyler: TObject;

function DefaultStyler: TDrawTabStyler;
begin
  if InternalDefaultStyler = nil then
    InternalDefaultStyler := TJavaTabStyler.Create(Application);
  Result := TDrawTabStyler(InternalDefaultStyler);
end;

{ TCustomDrawTabs }

constructor TCustomDrawTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse];
  DoubleBuffered := True;
	FTabs := TDrawTabItems.Create(Self);
  FTabIndex := -1;
  FTabDownIndex := -1;
  FTabHotIndex := -1;
  FCloseDownIndex := -1;
  FCloseHotIndex := -1;
  Height := 32;
  Width := 192;
end;

destructor TCustomDrawTabs.Destroy;
begin
  FTabs.Free;
  inherited Destroy;
end;

function TCustomDrawTabs.GetStyler: TDrawTabStyler;
begin
  Result := FStyler;
  if Result = nil then
    Result := DefaultStyler;
end;

procedure TCustomDrawTabs.CloseTab(Index: Integer);
var
  Action: TTabCloseAction;
  T: TDrawTabItem;
  I: Integer;
begin
  Action := taFree;
  if Assigned(FOnCloseTab) then
    FOnCloseTab(Self, Index, Action);
  if Action in [taFree, taHide] then
  begin
    T := FTabs[Index];
    if Action = taHide then
      T := nil;
    try
      FTabs[Index].Visible := False;
      if FTabIndex < 0 then Exit;
      for I := Index + 1 to FTabs.Count - 1 do
        if FTabs[I].Visible and FTabs[I].Enabled then
        begin
          FTabIndex := -1;
          TabIndex := I - 1;
          Exit;
        end;
      for I := Index - 1 downto 0 do
        if FTabs[I].Visible and FTabs[I].Enabled then
        begin
          FTabIndex := -1;
          TabIndex := I;
          Exit;
        end;
    finally
      T.Free
    end;
  end;
end;

procedure TCustomDrawTabs.DrawBackground;
begin
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self)
  else
    GetStyler.DrawBackground(Self);
end;

procedure TCustomDrawTabs.DrawClose(Index: Integer; Rect: TRect; State: TDrawState);
begin
  if Assigned(FOnDrawClose) then
    FOnDrawClose(Self, Index, Rect, State)
  else
    GetStyler.DrawClose(Self, Index, Rect, State);
end;

procedure TCustomDrawTabs.DrawTab(Index: Integer; Rect: TRect; State: TDrawState);
begin
  if Assigned(FOnDrawTab) then
    FOnDrawTab(Self, Index, Rect, State)
  else
    GetStyler.DrawTab(Self, Index, Rect, State);
end;

procedure TCustomDrawTabs.MeasureTab(Index: Integer; var Rects: TTabRects; State: TDrawState);
begin
  if Assigned(FOnMeasureTab) then
    FOnMeasureTab(Self, Index, Rects, State)
  else
    GetStyler.MeasureTab(Self, Index, Rects, State);
end;

procedure TCustomDrawTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
	P: TPoint;
  T: TDrawTabItem;
	I: Integer;
begin
	if Button = mbLeft then
  begin
		P := Point(X, Y);
    FTabDownIndex := -1;
    FCloseDownIndex := -1;
		for I := 0 to FTabs.Count - 1 do
    begin
      T := FTabs[I];
      if (csDesigning in ComponentState) or (T.Enabled and T.Visible) then
        if PtInRect(T.CloseRect, P) then
	      begin
  	    	FCloseDownIndex := I;
          Repaint;
  	    end
        else if PtInRect(T.TabRect, P) then
	      begin
  	    	FTabDownIndex := I;
      	  Break;
  	    end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomDrawTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	P: TPoint;
  H, C: Integer;
	I: Integer;
begin
	if csDesigning in ComponentState then Exit;
	P := Point(X, Y);
  H := -1;
  C := -1;
	for I := 0 to FTabs.Count - 1 do
  begin
  	if PtInRect(FTabs[I].CloseRect, P) then
    	C := I;
  	if PtInRect(FTabs[I].TabRect, P) then
    	H := I;
    if (H > -1) or (C > -1) then
      Break;
  end;
	TabHotIndex := H;
  CloseHotIndex := C;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomDrawTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
	Form: TCustomForm;
  T: TDrawTabItem;
	P: TPoint;
	I: Integer;
begin
	if Button = mbLeft then
  begin
		P := Point(X, Y);
		for I := FTabs.Count - 1 downto 0 do
    begin
      T := FTabs[I];
      if (csDesigning in ComponentState) or (T.Enabled and T.Visible) then
        if FCloseDownIndex > -1 then
          if (FCloseDownIndex = I) and PtInRect(FTabs[I].CloseRect, P) then
            CloseTab(I)
          else
        else if FTabDownIndex > -1 then
          if (FTabDownIndex = I) and PtInRect(FTabs[I].TabRect, P) then
          begin
          	TabIndex := I;
			      if csDesigning in ComponentState then
			      begin
        			Form := GetParentForm(Self);
		    	    if (Form <> nil) and (Form.Designer <> nil) then
        		    Form.Designer.Modified;
          	  Break;
  			    end;
          end;
    end;
    if FCloseDownIndex > -1 then
    begin
      FCloseDownIndex := -1;
      Repaint;
    end;
    FTabDownIndex := -1;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomDrawTabs.Paint;

  function TabState(Item: TDrawTabItem; Index: Integer): TDrawState;
  begin
    Result := [];
    if Index = FTabIndex then
      Include(Result, dsDefaulted);
    if not Item.Enabled then
      Include(Result, dsDisabled)
    else if (FCloseDownIndex = -1) and ((FTabDownIndex = -1) or (FTabDownIndex = Index)) then
    begin
      if FTabDownIndex = Index then
        Include(Result, dsPressed);
      if FTabHotIndex = Index then
        Include(Result, dsHot);
    end;
  end;

  function CloseState(Item: TDrawTabItem; Index: Integer): TDrawState;
  begin
    Result := [];
    if Index = FTabIndex then
      Include(Result, dsDefaulted);
    if csDesigning in ComponentState then
      Exit;
    if not Item.Enabled then
      Include(Result, dsDisabled)
    else if (FCloseDownIndex = -1) or (FCloseDownIndex = Index) then
    begin
      if FCloseDownIndex = Index then
        Include(Result, dsPressed);
      if FCloseHotIndex = Index then
        Include(Result, dsHot);
    end;
  end;

var
  TabRects: TTabRects;
  Item: TDrawTabItem;
  I: Integer;
begin
  DrawBackground;
  FillChar(TabRects, SizeOf(TabRects), #0);
  for I := 0 to FTabs.Count - 1 do
  begin
    Item := FTabs[I];
    if not (Item.Visible or (csDesigning in ComponentState)) then
      Continue;
    MeasureTab(I, TabRects, TabState(Item, I));
    Item.FTabRect := TabRects.Tab;
    Item.FCloseRect := TabRects.Close;
    TabRects.Prior := TabRects.Tab;
  end;
  for I := 0 to FTabs.Count - 1 do
  begin
    Item := FTabs[I];
    if not (Item.Visible or (csDesigning in ComponentState)) then
      Continue;
    if I = FTabIndex then
      Continue;
    DrawTab(I, Item.FTabRect, TabState(Item, I));
    DrawClose(I, Item.FCloseRect, CloseState(Item, I));
  end;
  if (FTabIndex > -1) and (FTabIndex < FTabs.Count) then
  begin
    Item := FTabs[FTabIndex];
    if Item.Visible or (csDesigning in ComponentState) then
    begin
      DrawTab(FTabIndex, Item.FTabRect, TabState(Item, FTabIndex));
      DrawClose(FTabIndex, Item.FCloseRect, CloseState(Item, FTabIndex));
    end;
  end;
end;

procedure TCustomDrawTabs.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (AComponent <> nil) and (AComponent = FStyler) then
      FStyler := nil;
end;

procedure TCustomDrawTabs.TabsChanged;
begin
	Repaint;
end;

procedure TCustomDrawTabs.SetCloseHotIndex(Value: Integer);
begin
	if Value <> FCloseHotIndex then
  begin
	  FCloseHotIndex := Value;
    Repaint;
  end;
end;

procedure TCustomDrawTabs.SetTabHotIndex(Value: Integer);
begin
	if Value <> FTabHotIndex then
  begin
	  FTabHotIndex := Value;
    Repaint;
  end;
end;

procedure TCustomDrawTabs.SetTabs(Value: TDrawTabItems);
begin
  FTabs.Assign(Value);
  Repaint;
end;

procedure TCustomDrawTabs.SetTabIndex(Value: Integer);
begin
  if Value <> FTabIndex then
  begin
    FTabIndex := Value;
    Repaint;
    if Assigned(FOnSelectItem) then
      FOnSelectItem(Self);
  end;
end;

procedure TCustomDrawTabs.SetStyler(Value: TDrawTabStyler);
begin
  if Value <> FStyler then
  begin
    if FStyler <> nil then
      FStyler.RemoveFreeNotification(Self);
    FStyler := Value;
    if FStyler <> nil then
      FStyler.FreeNotification(Self);
    Repaint;
  end;
end;

procedure TCustomDrawTabs.KeyDown(var Key: Word; Shift: TShiftState);

	function FindPriorItem: Integer;
  var
    I: Integer;
  begin
  	Result := TabIndex;
    if Result < 0 then Exit;
    for I := Result - 1 downto 0 do
    	if Tabs[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
    for I := Tabs.Count - 1 downto 0 do
    	if Tabs[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
  end;

	function FindNextItem: Integer;
  var
    I: Integer;
  begin
  	Result := TabIndex;
    if Result < 0 then Exit;
    for I := Result + 1 to Tabs.Count - 1 do
    	if Tabs[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
    for I := 0 to Tabs.Count - 1 do
    	if Tabs[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
  end;

begin
  inherited KeyDown(Key, Shift);
  {case Key of
  	VK_UP, VK_LEFT: TabIndex := FindPriorItem;
		VK_DOWN, VK_RIGHT: TabIndex := FindNextItem;
	end}
end;

procedure TCustomDrawTabs.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
	P: TPoint;
	I: Integer;
begin
	inherited;
	Msg.Result := 0;
  P := Point(Msg.XPos, Msg.YPos);
	for I := 0 to FTabs.Count - 1 do
 		if PtInRect(FTabs[I].TabRect, P) then
    begin
			Msg.Result := 1;
   	  Break;
    end;
end;

procedure TCustomDrawTabs.CMEnabledChanged(var Msg: TMessage);
begin
	inherited;
  Repaint;
end;

procedure TCustomDrawTabs.CMMouseLeave(var Msg: TMessage);
begin
	inherited;
	TabHotIndex := -1;
end;

{procedure TCustomDrawTabs.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  inherited;
  Canvas.Lock;
  try
    Canvas.Handle := Msg.DC;
    try
    finally
      Canvas.Handle := 0;
    end;
  finally
    Canvas.Unlock;
  end;
  Msg.Result := 1;
end;}

end.
