
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit SuplCtrls;

interface

{$I CODEBOT.INC}

uses
  Classes, Controls, Windows, Menus, Forms, Messages, SysUtils,
  StdCtrls, Graphics, ComCtrls, TypInfo, Buttons, ExtCtrls, DB,
  BaseTypes, GraphTools, MathTools, FormTools,
  {$IFDEF GDIPLUS}GdiPlus, GdiIntf,{$ENDIF}
  ImgList, BtnCtrls;

{ TInfoBox }

type
{$IFDEF GDIPLUS}
  TInfoBox = class(TGraphicControl)
  private
    FOnPaint: TNotifyEvent;
    FAlignment: TAlignment;
    FOpacity: Byte;
    FRadius: Byte;
    FUseText: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetOpacity(Value: Byte);
    procedure SetRadius(Value: Byte);
    procedure SetUseText(Value: Boolean);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Opacity: Byte read FOpacity write SetOpacity;
    property Radius: Byte read FRadius write SetRadius;
    property UseText: Boolean read FUseText write SetUseText;
    property Align;
    property Anchors;
    property Enabled;
    property Caption;
    property Font;
    property Visible;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

procedure DrawInfoBox(DC: HDC; Rect: TRect; ForeColor: TColor; Radius: Integer = 10);
{$ENDIF}

{ TShadowLabel }

type
	TShadowLabel = class(TLabel)
  private
    FShadow: Boolean;
    FShadowColor: TColor;
    procedure SetShadow(Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
	protected
    procedure AdjustBounds; override;
		procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
	public
  	constructor Create(AOwner: TComponent); override;
  published
  	property Shadow: Boolean read FShadow write SetShadow;
    property ShadowColor: TColor read FShadowColor write SetShadowColor;
  end;

{ TCustomCaptionBox }

type
  TCaptionStyle = (csClose, csStick, csNone);

  (*TCustomCaptionBox = class(TFramedWindow)
  private
    FActive: Boolean;
    FButton: TThemeGlyphButton;
    FCaptionHeight: Integer;
    FPanel: TEmptyWindow;
    FLabel: TLabel;
    FStyle: TCaptionStyle;
    FOnClose: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FOnStick: TNotifyEvent;
    procedure MouseActivate;
    procedure ButtonClick(Sender: TObject);
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetActive(Value: Boolean);
    procedure SetCaptionHeight(Value: Integer);
    procedure SetStyle(Value: TCaptionStyle);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMChildFocus(var Message: TMessage); message WM_CHILDFOCUS;
    procedure WMChildKillFocus(var Message: TMessage); message WM_CHILDKILLFOCUS;
  protected
    procedure CloseQuery;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure UpdateControls;
    property CaptionHeight: Integer read FCaptionHeight write SetCaptionHeight;
    property Active: Boolean read FACtive write SetActive;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write
      FOnCloseQuery;
    property OnStick: TNotifyEvent read FOnStick write FOnStick;
  public
    constructor Create(AOwner: TComponent); override;
    property Style: TCaptionStyle read FStyle write SetStyle default csClose;
    procedure Close;
    procedure Stick;
  end;

{ TCaptionBox }

  TCaptionBox = class(TCustomCaptionBox)
  published
    property Align;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
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
    property Style;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnClose;
    property OnCloseQuery;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
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
    property OnStick;
  end;      *)

{ TExpandableBox }

	TExpandableStyle = (esGroupBox, esButtonBox, esExploreBar);

  TExpandableBox = class(TCustomControl)
  private
    FAllowCollapse: Boolean;
    FAutoHeight: Boolean;
    FBoxClick: Boolean;
    FCaptionRect: TRect;
    FExpanded: Boolean;
    FExpandedHeight: Integer;
    FNodeDown: Boolean;
    FNodePressed: Boolean;
    FStyle: TExpandableStyle;
    FVisibleData: TVisibleDataArray;
    FOnCollapse: TNotifyEvent;
    FOnExpand: TNotifyEvent;
    function GetCaptionRect: TRect;
    function GetNodeRect: TRect;
    procedure SetAllowCollapse(Value: Boolean);
    procedure SetAutoHeight(Value: Boolean);
    procedure SetExpanded(Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
    procedure SetStyle(Value: TExpandableStyle);
		procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    function GetClientRect: TRect; override;
    procedure Resize; override;
    property CaptionRect: TRect read GetCaptionRect;
    property NodeRect: TRect read GetNodeRect;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property AllowCollapse: Boolean read FAllowCollapse write SetAllowCollapse;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style: TExpandableStyle read FStyle write SetStyle default esGroupBox;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TLineGrid }

  TLineGrid = class(TCustomControl)
  private
    FAmplitude: Double;
    FBorderStyle: TBorderStyle;
    FGridSize: Integer;
    FGridColor: TColor;
    FLog: TIntegerArray;
    FPen: TPen;
    FVelocity: Double;
    procedure StyleChanged(Sender: TObject);
    procedure SetAmplitude(const Value: Double);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetGridColor(Value: TColor);
    procedure SetGridSize(Value: Integer);
    procedure SetPen(Value: TPen);
    procedure SetVelocity(const Value: Double);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Log(Value: Integer);
  published
    property Align;
    property Amplitude: Double read FAmplitude write SetAmplitude;
    property Anchors;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GridColor: TColor read FGridColor write SetGridColor default clGreen;
    property GridSize: Integer read FGridSize write SetGridSize default 12;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property Pen: TPen read FPen write SetPen;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property Velocity: Double read FVelocity write SetVelocity;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
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
  end;

{ TIntegerEdit }

  TIntegerEdit = class(TCustomEdit)
  private
    FAlignment: TAlignment;
    FAllowBlank: Boolean;
    FDisplayZero: Boolean;
    FIntValue: Integer;
    FMax: Integer;
    FMin: Integer;
    FOnCalcRect: TCalcRectEvent;
    procedure SetIntValue(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  published
    property Anchors;
    property AllowBlank: Boolean read FAllowBlank write FAllowBlank;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DisplayZero: Boolean read FDisplayZero write FDisplayZero;
    property Enabled;
    property Font;
    property HideSelection;
    property IntValue: Integer read FIntValue write SetIntValue;
    property MaxLength;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCalcRect: TCalcRectEvent read FOnCalcRect
      write FOnCalcRect;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TCashEdit }

  TCashEditOption = (moDollarSign, moForceAmount);
  TCashEditOptions = set of TCashEditOption;

  TCashEdit = class(TCustomEdit)
  private
    FAmount: Double;
    FDecimals: Integer;
    FOptions: TCashEditOptions;
    FRevertText: string;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function GetAmountText: string;
    procedure SetAmount(const Value: Double);
    procedure SetDecimals(Value: Integer);
    procedure SetOptions(Value: TCashEditOptions);
  protected
    property AmountText: string read GetAmountText;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Amount: Double read FAmount write SetAmount;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property Decimals: Integer read FDecimals write SetDecimals default 2;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property Options: TCashEditOptions read FOptions write SetOptions;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TBackground }

  TBackground = class(TGraphicControl)
  private
  	FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
  	constructor Create(AOwner: TComponent); override;
    procedure Draw;
    procedure PlaceControl(Control: TControl);
    property Canvas;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
  end;

	{ THorizontalBar }

  THorizontalBar = class(TGraphicControl, IUnknown, IIgnoreMargin)
  private
  	FOnPaint: TNotifyEvent;
    FThemed: Boolean;
    FTransparent: Boolean;
    FFlat: Boolean;
    FUseBackground: Boolean;
    procedure SetThemed(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetUseBackground(Value: Boolean);
  protected
    procedure Paint; override;
  public
  	constructor Create(AOwner: TComponent); override;
    procedure Draw;
    property Canvas;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentShowHint;
    property ParentColor;
    property ShowHint;
    property Visible;
    property Themed: Boolean read FThemed write SetThemed;
    property Flat: Boolean read FFlat write SetFlat;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property UseBackground: Boolean read FUseBackground write SetUseBackground;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure DefineBarBackground(Image: TGraphic; Alignment: TAlignment);

implementation

type
  PComponentState = ^TComponentState;

{$IFDEF GDIPLUS}
{ TInfoBox }

procedure DrawInfoBox(DC: HDC; Rect: TRect; ForeColor: TColor; Radius: Integer = 10);
var
  G: IGdiGraphics;
  A: TRectF;
  S: IGdiGraphicsPath;
  P: IGdiPen;
  B: IGdiBrush;
begin
  G := NewGraphics(DC);
  G.Clear(NewColor(0, 0));
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  A := NewRectF(Rect);
  S := NewRoundRect(A, Radius);
  P := NewPen(NewColor(ForeColor), 1);
  B := NewLinearGradientBrush(A, 90, NewColor(clInfoBk), NewColor(clWindow));
  G.FillPath(B, S);
  B := NewHatchBrush(HatchStyleForwardDiagonal, NewColor(clInfoBk));
  G.FillPath(B, S);
  G.DrawPath(P, S);
end;

procedure TInfoBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FUseText then
    Invalidate;
end;

procedure TInfoBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if FUseText then
    Invalidate;
end;

constructor TInfoBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FRadius := 10;
  FOpacity := High(Byte);
end;

procedure TInfoBox.Paint;
var
  DC: HDC;
  C: TColor;
  F: HFONT;
  B: TFastBitmap;
  R: TRect;
  S: TSize;
begin
  inherited Paint;
  if FUseText and (Caption = '') then Exit;
  DC := Canvas.Handle;
  if FUseText then
  begin
    F := SelectObject(DC, Font.Handle);
    S := CalcCaptionSize(DC, Caption);
    Inc(S.cx, 16);
    Inc(S.cy, 12);
  end
  else
  begin
    F := 0;
    S.cx := Width;
    S.cy := Height;
  end;
  B := CreateFastBitmap(S.cx, S.cy, pd32);
  if IsFastBitmap(B) then
  try
    //BitBlt(B.DC, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
    R := Rect(0, 0, B.Width, B.Height);
    DrawInfoBox(B.DC, R, Font.Color, FRadius);
    if FUseText then
    begin
      case FAlignment of
        taLeftJustify: ;
        taRightJustify:
          begin
            R.Left := Width - S.cx;
            R.Right := Width;
          end;
        taCenter:
          begin
            R.Left := (Width - S.cx) div 2;
            R.Right := R.Left + S.cx;
          end;
      end;
      R.Top := (Height - S.cy) div 2;
      R.Bottom := R.Top + S.cy;
      BaseTypes.AlphaDraw(Canvas.Handle, R, B, FOpacity);
      C := SetTextColor(DC, ColorToRGB(Font.Color));
      DrawCaption(DC, Caption, R, drCenter);
      SetTextColor(DC, C);
    end
    else
      BaseTypes.AlphaDraw(Canvas.Handle, R, B, FOpacity);
  finally
    DestroyFastBitmap(B);
  end;
  if FUseText then
    SelectObject(DC, F);
  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TInfoBox.SetAlignment(Value: TAlignment);
begin
  if Value <> FAlignment then
  begin
    FAlignment := Value;
    if FUseText then
      Invalidate;
  end;
end;

procedure TInfoBox.SetOpacity(Value: Byte);
begin
  if Value <> FOpacity then
  begin
    FOpacity := Value;
    Invalidate;
  end;
end;

procedure TInfoBox.SetRadius(Value: Byte);
begin
  if Value <> FRadius then
  begin
    FRadius := Value;
    Invalidate;
  end;
end;

procedure TInfoBox.SetUseText(Value: Boolean);
begin
  if Value <> FUseText then
  begin
    FUseText := Value;
    Invalidate;
  end;
end;
{$ENDIF}

{ TShadowLabel }

procedure TShadowLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  X: Integer;
  Rect: TRect;
  AAlignment: TAlignment;
begin
  if not (csReading in ComponentState) and AutoSize then
  begin
    Rect := ClientRect;
    DC := GetDC(0);
    Canvas.Handle := DC;
    DoDrawText(Rect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    X := Left;
    AAlignment := Alignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - Rect.Right);
    SetBounds(X, Top, Rect.Right + 2, Rect.Bottom + 2);
  end;
end;

constructor TShadowLabel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FShadowColor := clWhite;
  ParentFont := False;
  Font := Screen.IconFont;
  Transparent := True;
  Layout := tlCenter;
end;

procedure TShadowLabel.DoDrawText(var Rect: TRect; Flags: Integer);

	procedure InternalDraw(var Rect: TRect; Flags: Integer; Color: TColor);
	var
	  Text: string;
	begin
	  Text := GetLabelText;
	  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
	    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
	  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
	  Flags := DrawTextBiDiModeFlags(Flags);
    Canvas.Font.Color := Color;
	  if not Enabled then
	  begin
	    OffsetRect(Rect, 1, 1);
	    Canvas.Font.Color := clBtnHighlight;
	    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
	    OffsetRect(Rect, -1, -1);
	    Canvas.Font.Color := clBtnShadow;
	    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
	  end
	  else
	    DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
	end;

var
	R: TRect;
begin
	Canvas.Font := Font;
  if Enabled and FShadow then
  begin
		R := Rect;
    OffsetRect(R, 1, 1);
  	InternalDraw(R, Flags, FShadowColor);
		R := Rect;
    OffsetRect(R, 1, 1);
  	InternalDraw(R, Flags, FShadowColor);
		R := Rect;
  	InternalDraw(R, Flags, Font.Color);
		Inc(R.Right, 2);
		Inc(R.Bottom, 2);
    Rect := R;
  end
  else
  	InternalDraw(Rect, Flags, Font.Color);
end;

procedure TShadowLabel.SetShadow(Value: Boolean);
begin
	if Value <> FShadow then
  begin
	  FShadow := Value;
    Repaint;
	end;
end;

procedure TShadowLabel.SetShadowColor(const Value: TColor);
begin
	if FShadowColor <> Value then
  begin
  	FShadowColor := Value;
	  Repaint;
  end;
end;

{ TCustomCaptionBox }

(*constructor TCustomCaptionBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  FCaptionHeight := GetSystemMetrics(SM_CYVSCROLL) + 3;
  Height := 200;
  Width := 100;
  FPanel := TEmptyWindow.Create(Self);
  with FPanel do
  begin
    Parent := Self;
    Align := alTop;
    Color := clInactiveCaption;
    Height := GetSystemMetrics(SM_CYVSCROLL) + 4;
    FPanel.OnMouseDown := ControlMouseDown;
  end;
  FLabel := TLabel.Create(Self);
  with FLabel do
  begin
    Parent := FPanel;
    Transparent := True;
    ParentFont := True;
    Align := alLeft;
    Layout := tlCenter;
    Font.Color := clInactiveCaptionText;
    OnMouseDown := ControlMouseDown;
  end;
  FButton := TThemeGlyphButton.Create(Self);
  with FButton do
  begin
    Parent := FPanel;
    Kind := tgClose;
    Style := bsBeveled;
    Align := alRight;
    Color := clInactiveCaptionText;
    OnClick := ButtonClick;
    OnMouseDown := ControlMouseDown;
  end;
end;

procedure TCustomCaptionBox.Close;
begin
  Style := csClose;
  CloseQuery;
end;

procedure TCustomCaptionBox.CloseQuery;
var
  CanClose: Boolean;
begin
  CanClose := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, CanClose);
  if CanClose then
  begin
    if Assigned(FOnClose) then
      FOnClose(Self);
    if Style = csClose then
      Hide;
  end;
end;

procedure TCustomCaptionBox.Stick;
begin
  Style := csStick;
  if Assigned(FOnStick) then
    FOnStick(Self);
end;

procedure TCustomCaptionBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.style := CS_DBLCLKS or CS_HREDRAW or CS_VREDRAW;
  end;
end;

procedure TCustomCaptionBox.Loaded;
begin
  inherited Loaded;
  if Parent <> nil then HandleNeeded;
  UpdateControls;
  //FActive := not FActive;
  //Active := not Active;
end;

procedure TCustomCaptionBox.MouseActivate;
begin
  Active := True;
  if IsChild(Handle, GetFocus) then Exit;
  SelectNext(nil, True, True);
  if not IsChild(Handle, GetFocus) then SetFocus;
end;

procedure TCustomCaptionBox.UpdateControls;
begin
  if HandleAllocated then
  begin
    FPanel.Font := Font;
    FLabel.Font := Font;
    FLabel.Font.Color := clInactiveCaptionText;
    Canvas.Font := Font;
    if FCaptionHeight < Canvas.TextHeight(' ') then
      FCaptionHeight := Canvas.TextHeight(' ');
    if FCaptionHeight < GetSystemMetrics(SM_CYVSCROLL) + 4 then
      FCaptionHeight := GetSystemMetrics(SM_CYVSCROLL) + 4;
    FPanel.Height := FCaptionHeight;
  end;
end;

procedure TCustomCaptionBox.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
    begin
      FPanel.Color := clActiveCaption;
      FLabel.Font.Color := clCaptionText;
      FButton.Color := clCaptionText;
    end
    else
    begin
      FPanel.Color := clInactiveCaption;
      FLabel.Font.Color := clInactiveCaptionText;
      FButton.Color := clInactiveCaptionText;
      FActive := False;
    end;
  end;
end;

procedure TCustomCaptionBox.SetCaptionHeight(Value: Integer);
begin
  if Value <> FCaptionHeight then
  begin
    FCaptionHeight := Value;
    UpdateControls;
  end;
end;

procedure TCustomCaptionBox.SetStyle(Value: TCaptionStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
		FButton.Visible := True;
    case FStyle of
    	csClose: FButton.Kind := tgClose;
			csStick: FButton.Kind := tgPin;
    else
    	FButton.Visible := False;
    end;
    Repaint;
  end;
end;

procedure TCustomCaptionBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateControls;
end;

procedure TCustomCaptionBox.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  Active := not Active;
  Active := not Active;
end;

procedure TCustomCaptionBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  FLabel.Caption := ' ' + Text;
end;

procedure TCustomCaptionBox.WMChildFocus(var Message: TMessage);
begin
  inherited;
  Active := True;
end;

procedure TCustomCaptionBox.WMChildKillFocus(var Message: TMessage);
begin
  inherited;
  Active := False;
end;

procedure TCustomCaptionBox.ButtonClick(Sender: TObject);
begin
  if Style = csClose then
    Close
  else if Style = csStick then
    Stick;
end;

procedure TCustomCaptionBox.ControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseActivate;
end;*)

{ TExpandableBox }

constructor TExpandableBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowCollapse := true;
  FExpandedHeight := 150;
  FExpanded := True;
  // Color := clBtnFace;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks, csReplicatable];
  Height := FExpandedHeight;
  Width := 325;
  Canvas.Brush.Color := Color;
end;

procedure TExpandableBox.AlignControls(AControl: TControl; var Rect: TRect);
var
	H: Integer;
	C: TControl;
	I: Integer;
begin
	inherited AlignControls(AControl, Rect);
  if not (FAutoHeight and FExpanded) then Exit;
  H := CaptionRect.Bottom;
  for I := 0 to ControlCount - 1 do
  begin
		C := Controls[I];
    if akBottom in C.Anchors then Exit;
    if C.Top + C.Height > H then
    	H := C.Top + C.Height;
	end;
  if FStyle = esGroupBox then
  	Inc(H, 2);
	if H < CaptionRect.Bottom then
  	H := CaptionRect.Bottom;
	ExpandedHeight := H;
end;

procedure TExpandableBox.DblClick;
begin
  if not FBoxClick then
		Expanded := not Expanded;
end;

procedure TExpandableBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = VK_SPACE) or (Key = VK_RETURN) then
		Expanded := not Expanded;
end;

procedure TExpandableBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
begin
  FBoxClick := False;
  Rect := NodeRect;
  if (Button = mbLeft) and FAllowCollapse then
  begin
    if PtInRect(Rect, Point(X, Y)) then
    begin
      FBoxClick := True;
      FNodePressed := True;
      FNodeDown := True;
      InvalidateRect(Handle, @Rect, True);
    end;
	  if (Button = mbLeft) and TabStop then
  		SetFocus;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TExpandableBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Rect: TRect;
begin
  Rect := NodeRect;
  if PtInRect(Rect, Point(X, Y)) then
  	Cursor := crHandPoint
	else
  	Cursor := crDefault;
  if FNodePressed and (PtInRect(Rect, Point(X, Y)) <> FNodeDown) and FAllowCollapse then
  begin
    FNodeDown := not FNodeDown;
    InvalidateRect(Handle, @Rect, True);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TExpandableBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
begin
  Rect := NodeRect;
  if (Button = mbLeft) and FAllowCollapse then
  begin
    if FNodeDown then
      Expanded := not Expanded;
    FNodePressed := False;
    FNodeDown := False;
    InvalidateRect(Handle, @Rect, True);
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure DrawThemeExplorerBar(DC: HDC; const Caption: string; const Rect: TRect; State: TDrawState);
{var
	R: TRect;
  C: TColor;}
begin
	SelectClipRgn(DC, 0);
end;

procedure DrawThemeExpandableBand(DC: HDC; const Caption: string; const Rect: TRect; State: TDrawState);
var
	R: TRect;
  C: TColor;
begin
	SelectClipRgn(DC, 0);
	R := Rect;
  R.Bottom := R.Top + FontHeight(DC) * 2;
  if not ThemePainter.Enabled then
  	FillRectColor(DC, R, clBtnFace);
  DrawThemeThinButton(DC, R, [dsHot]);
  R.Right := R.Left + HeightOf(R);
	DrawThemeNode(DC, R, State);
  OffsetRect(R, HeightOf(R), 0);
  R.Right := Rect.Right - 2;
  DrawCaption(DC, Caption, R, drLeft, State * [dsDisabled] = []);
  if dsFocused in State then
  begin
  	with CalcCaptionSize(DC, Caption) do
    begin
	  	R.Right := R.Left + cX;
      R.Top := (HeightOf(R) - cY) shr 1;
      R.Bottom := R.Top + cY;
    end;
    InflateRect(R, 2, 2);
    C := SetTextColor(DC, 0);
    DrawFocusRect(DC, R);
    SetTextColor(DC, C);
  end;
end;

procedure TExpandableBox.Paint;
var
  Rect: TRect;
  DC: HDC;
  State: TDrawState;
begin
  Rect := inherited GetClientRect; // Classes.Rect(0, 0, Width, Height);
  DC := Canvas.Handle;
  State := [];
  if not Enabled then State := State + [dsDisabled];
  if FNodeDown then State := State + [dsPressed];
  if FExpanded then State := State + [dsExpanded];
  if Focused then State := State + [dsFocused];
  if FStyle = esExploreBar then
		DrawThemeExplorerBar(DC, Caption, Rect, State)
  else if FStyle = esButtonBox then
		DrawThemeExpandableBand(DC, Caption, Rect, State)
  else if FAllowCollapse then
    DrawThemeExpandableBox(DC, Caption, Rect, State)
  else
    DrawThemeGroupBox(DC, Caption, Rect, State);
end;

function TExpandableBox.GetClientRect: TRect;
var
  I: Integer;
begin
  Result := inherited GetClientRect;
  with Result do
  	if FStyle = esButtonBox then
    begin
    	Result := Rect(0, 0, Width, Height);
      Result.Top := CaptionRect.Bottom + 1;
      Result.Left := HeightOf(CaptionRect) shr 1;
    end
    else
	  begin
	    I := HeightOf(CaptionRect);
	    if FAllowCollapse and (I < NodeSize) then
	      I := NodeSize;
	    Top := I + 1;
	    if FAllowCollapse then
	      Left := Left + NodeSize div 2 + 2
	    else
	      Left := Left + 2;
	    Right := Right - 2;
	    Bottom := Bottom - 2;
	  end;
end;

procedure TExpandableBox.Resize;
begin
  inherited Resize;
  if FExpanded then
    FExpandedHeight := Height;
end;

function TExpandableBox.GetCaptionRect: TRect;
var
  DC: HDC;
  F: HFONT;
  Size: TSize;
begin
  if IsRectEmpty(FCaptionRect) then
  begin
    DC := GetDC(0);
    F := SelectObject(DC, Font.Handle);
    Size := CalcCaptionSize(DC, ' ');
    if FStyle = esButtonBox then
    	Inc(Size.cy, Size.cy);
    if Size.cy < NodeSize then
      Size.cy := NodeSize;
    FCaptionRect := Classes.Rect(0, 0, Width, Size.cy);
    SelectObject(DC, F);
    ReleaseDC(0, DC);
  end;
  Result :=  FCaptionRect;
end;

function TExpandableBox.GetNodeRect: TRect;
var
  I: Integer;
begin
  I := HeightOf(CaptionRect);
  if I < NodeSize then
    I := (I - NodeSize) div 2
  else
    I := 0;
  Result := Rect(0, I, NodeSize, I + NodeSize);
  if FStyle = esButtonBox then
  	OffsetRect(Result, (HeightOf(Result) - NodeSize) shr 1, 0);
end;

procedure TExpandableBox.SetAllowCollapse(Value: Boolean);
begin
  if Value <> FAllowCollapse then
  begin
    if not Value then
      Expanded := True;
    FAllowCollapse := Value;
    if HandleAllocated then
      SendMessage(Handle, WM_SIZE, 0, 0);
    Invalidate;
  end;
end;

procedure TExpandableBox.SetAutoHeight(Value: Boolean);
begin
	if Value <> FAutoHeight then
  begin
		FAutoHeight := Value;
    if FExpanded then
    	Realign;
  end;
end;

procedure TExpandableBox.SetExpanded(Value: Boolean);
var
  Event: TNotifyEvent;
  W: TWinControl;
  I: Integer;
begin
  Value := Value or (not FAllowCollapse);
  if Value <> FExpanded then
  begin
    FExpanded := Value;
    if Value then
    begin
      Height := FExpandedHeight;
      Event := FOnExpand;
      if not (csDesigning in ComponentState) then
      begin
  	    RestoreVisible(FVisibleData);
	      FVisibleData := nil;
      end;
			DisableAlign;
			for I := 0 to ControlCount - 1 do
      	if Controls[I] is TWinControl then
        begin
        	W := TWinControl(Controls[I]);
          if W.Align = alTop then
        		W.Top := W.TabOrder;
      	end;
			EnableAlign;
    end
    else
    begin
      if HeightOf(CaptionRect) > NodeSize then
        Height := CaptionRect.Bottom
      else
        Height := NodeSize;
      Event := FOnCollapse;
			if IsChild(Handle, GetFocus) then
	    	SetFocus;
	    if not (csDesigning in ComponentState) then
		    FVisibleData := SaveVisible(Self);
    end;
    if Assigned(Event) then
      Event(Self);
    Invalidate;
  end;
end;

var
	EmptyRect: TRect;

procedure TExpandableBox.SetExpandedHeight(const Value: Integer);
begin
  if Value <> FExpandedHeight then
  begin
    FExpandedHeight := Value;
    if Expanded then
      Height := FExpandedHeight;
		Invalidate;
  end;
end;

procedure TExpandableBox.SetStyle(Value: TExpandableStyle);
begin
	if Value <> FStyle then
  begin
  	FStyle := Value;
  	FCaptionRect := EmptyRect;
    if not Expanded then
    	Height := CaptionRect.Bottom;
		Realign;
    Repaint;
  end;
end;

procedure TExpandableBox.CMColorChanged(var Message: TMessage);
begin
  Canvas.Brush.Color := Color;
  Invalidate;
  inherited;
end;

procedure TExpandableBox.CMFontChanged(var Message: TMessage);
begin
  FillChar(FCaptionRect, SizeOf(FCaptionRect), #0);
  Canvas.Font := Font;
  Invalidate;
  inherited;
end;

procedure TExpandableBox.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TExpandableBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
	R: TRect;
begin
	R := inherited GetClientRect;
	{if FStyle = esExplorerBar then
  begin
  end
  else}
		FillRectColor(Message.DC, R, Color);
  Message.Result := 1;
end;

procedure TExpandableBox.WMSetFocus(var Message: TWMSetFocus);
begin
	Invalidate;
  if Assigned(OnEnter) then OnEnter(Self);
  inherited;
end;

procedure TExpandableBox.WMKillFocus(var Message: TWMSetFocus);
begin
	Invalidate;
  inherited;
end;

{ TLineGrid }

constructor TLineGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clBlack;
  Canvas.Brush.Color := Color;
  Height := 100;
  Width := 300;
  FAmplitude := 3;
  FBorderStyle := bsSingle;
  FGridSize := 12;
  FGridColor := clGreen;
  FPen := TPen.Create;
  FPen.Color := clLime;
  FPen.OnChange := StyleChanged;
  FVelocity := 6;
end;

destructor TLineGrid.Destroy;
begin
  FPen.Free;
  inherited Destroy;
end;

procedure TLineGrid.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := CS_DBLCLKS or CS_VREDRAW or CS_HREDRAW;
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
      begin
        Style := Style and not WS_BORDER;
        ExStyle := ExStyle or WS_EX_CLIENTEDGE;
      end
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TLineGrid.Log(Value: Integer);
var
  Delta: Integer;
  I: Integer;
begin
  I := Length(FLog);
  SetLength(FLog, I + 1);
  FLog[I] := Value;
  if FVelocity <> 0 then
  begin
    Delta := Round(I * FVelocity - ((I - 1) * FVelocity)) * -1;
    if Delta <> 0 then
      ScrollBy(Delta, 0);
  end;
end;

procedure TLineGrid.Paint;
var
  Rect: TRect;
  Delta: Integer;
  I: Integer;

  procedure DrawLines;
  var
    DC: HDC;
    PriorPen: HPEN;
    Size: Integer;
    FirstIndex: Integer;
    X, Y: Integer;
    I: Integer;
  begin
    DC := Canvas.Handle;
    PriorPen := SelectObject(DC, FPen.Handle);
    Size := Length(FLog) - 1;
    if FVelocity <> 0 then
      FirstIndex := Round(Length(FLog) - WidthOf(Rect) / FVelocity) - 1
    else
      FirstIndex := 0;
    if FirstIndex < 0 then
      FirstIndex := 0;
    X := 0;
    Y := 0;
    for I := FirstIndex to Length(FLog) - 1 do
    begin
      MoveToEx(DC, X, Y, nil);
      X := Round(Rect.Right - (Size - I) * FVelocity);
      Y := Round(Rect.Bottom - FLog[I] * FAmplitude);
      if I > FirstIndex then
        LineTo(DC, X, Y);
    end;
    SelectObject(DC, PriorPen);
  end;

begin
  with Canvas do
  begin
    Rect := ClientRect;
    FillRect(Rect);
    if FGridSize > 3 then
    begin
      Pen.Color := FGridColor;
      if FVelocity = 0 then
        Delta := 0
      else
        Delta := Round(Remainder(FVelocity * Length(FLog), FGridSize));
      for I := 0 to Rect.Bottom div FGridSize do
      begin
        MoveTo(0, Rect.Bottom - 1 - I * FGridSize);
        LineTo(Rect.Right, Rect.Bottom - 1 - I * FGridSize);
      end;
      for I := 0 to Rect.Right div FGridSize do
      begin
        MoveTo(Rect.Right - 1 - I * FGridSize - Delta, 0);
        LineTo(Rect.Right - 1 - I * FGridSize - Delta, Rect.Bottom);
      end;
    end;
    DrawLines;
  end;
end;

procedure TLineGrid.StyleChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TLineGrid.SetAmplitude(const Value: Double);
begin
  if Value <> FAmplitude then
  begin
    FAmplitude := Value;
    Invalidate;
  end;
end;

procedure TLineGrid.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TLineGrid.SetGridSize(Value: Integer);
begin
  if FGridSize <> Value then
  begin
    FGridSize := Value;
    Invalidate;
  end;
end;

procedure TLineGrid.SetGridColor(Value: TColor);
begin
  if Value <> FGridColor then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TLineGrid.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TLineGrid.SetVelocity(const Value: Double);
begin
  if Value <> FVelocity then
  begin
    FVelocity := Value;
    Invalidate;
  end;
end;

{ TIntegerEdit }

procedure TIntegerEdit.SetIntValue(Value: Integer);
begin
  if Value <> FIntValue then
  begin
    FIntValue := Value;
    if (FMax <> 0) and (FMin <> 0) then
    begin
	    if FIntValue > FMax then FIntValue := FMax;
  	  if FIntValue < FMin then FIntValue := FMin;
    end;
    if (FIntValue = 0) and (not FDisplayZero) then
      Text := ''
    else
      Text := IntToStr(FIntValue);
    Invalidate;
  end;
end;

procedure TIntegerEdit.SetMax(Value: Integer);
begin
  FMax := Value;
  if FMax < FMin then FMax := FMin;
  IntValue := FIntValue;
end;

procedure TIntegerEdit.SetMin(Value: Integer);
begin
  FMin := Value;
  if FMin > FMax then FMin := FMax;
  IntValue := FIntValue;
end;

procedure TIntegerEdit.CMTextChanged(var Message: TMessage);
var
  S: string;
  I: Integer;
begin
  inherited;
  if (not HandleAllocated) or (not Focused) then
  begin
    S := Trim(Text);
  	if Trim(Text) = '' then
	  	I := 0
  	else
	  	I := StrToIntDef(S, FIntValue);
    IntValue := I;
    if (S <> Text) and (S = '') and (AllowBlank) then
      Text := '';
  end;
end;

procedure TIntegerEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if Message.CharCode = VK_ESCAPE then
    if (FIntValue = 0) and (not FDisplayZero) then
      Text := ''
    else
      Text := IntToStr(FIntValue);
end;

procedure TIntegerEdit.WMKillFocus(var Message: TWMKillFocus);
var
  S: string;
begin
  inherited;
  S := Trim(Text);
	if S = '' then
		FIntValue := 0
	else
		FIntValue := StrToIntDef(Text, FIntValue);
  if (S = '') and AllowBlank then
    Text := ''
  else if (FIntValue = 0) and (not FDisplayZero) then
    Text := ''
  else
    Text := IntToStr(FIntValue);
  Invalidate;
end;

procedure TIntegerEdit.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  Rect: TRect;
  Brush: HBRUSH;
  PriorFont: HFONT;
  PriorColor: TColorRef;
  PriorMode: Cardinal;
begin
  if not Focused then
  begin
    BeginPaint(Handle, PS);
    with PS do
    try
      Windows.GetClientRect(Handle, Rect);
      Brush := CreateSolidBrush(ColorToRGB(Color));
      FillRect(hdc, Rect, Brush);
      DeleteObject(Brush);
      if FIntValue < FMin then
        FIntValue := FMin;
      if (FIntValue <> 0) or FDisplayZero then
      begin
        InflateRect(Rect, -1, 0);
        OffsetRect(Rect, 0, 1);
        if Assigned(FOnCalcRect) then
          FOnCalcRect(Self, Rect);
        PriorFont := SelectObject(hdc, Font.Handle);
        PriorColor := 0;
        if not Enabled then
          PriorColor := SetTextColor(hdc, GetSysColor(COLOR_BTNSHADOW));
        PriorMode := SetBkMode(hdc, TRANSPARENT);
        DrawText(hdc, PChar(IntToStr(FIntValue)), -1, Rect, DT_TOP or DT_LEFT or DT_SINGLELINE);
        SetBkMode(hdc, PriorMode);
        if PriorColor <> 0 then
          SetTextColor(hdc, PriorColor);
        SelectObject(hdc, PriorFont);
      end;
    finally
      EndPaint(Handle, PS);
    end;
    Message.Result := 0;
  end
  else
    inherited;
end;

{ TCashEdit }

function Round(const Value: Extended; Precision: Integer): Extended;
begin
  if Precision > 0 then
    Result := StrToFloat(Format('%.' + IntToStr(Precision) + 'f', [Value]))
  else
    Result := Trunc(Value)
end;

constructor TCashEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDecimals := 2;
  FOptions := [moDollarSign, moForceAmount];
  Text := AmountText;
end;

procedure TCashEdit.CMTextChanged(var Message: TMessage);
var
  S: string;
begin
  inherited;
  if (not HandleAllocated) or (not Focused) then
  begin
    S := Trim(Text);
    S := StringReplace(S, '$', '', [rfReplaceAll]);
    S := StringReplace(S, ',', '', [rfReplaceAll]);
    Amount := Round(StrToFloatDef(S, FAmount), FDecimals);
  end;
end;

procedure TCashEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
  if Message.CharCode = VK_ESCAPE then
    Text := FRevertText;
end;

procedure TCashEdit.WMKillFocus(var Message: TWMKillFocus);
var
  S: string;
begin
  inherited;
	S := Trim(Text);
  if S <> '' then
  begin
    if S[1] = '$' then
    begin
      S[1] := ' ';
    	S := Trim(S);
    end;
  end;
  FAmount := Round(StrToFloatDef(S, FAmount), FDecimals);
  Text := AmountText;
  Repaint;
end;

procedure TCashEdit.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  Rect: TRect;
  Brush: HBRUSH;
  PriorMode: Cardinal;
  PriorColor: Cardinal;
  PriorFont: HFONT;
begin
  if not Focused then
  begin
    BeginPaint(Handle, PS);
    with PS do
    try
      Windows.GetClientRect(Handle, Rect);
      Brush := CreateSolidBrush(ColorToRGB(Color));
      FillRect(hdc, Rect, Brush);
      DeleteObject(Brush);
      InflateRect(Rect, -2, 0);
      PriorFont := SelectObject(hdc, Font.Handle);
      PriorMode := SetBkMode(hdc, TRANSPARENT);
      if Enabled then
        DrawText(hdc, PChar(GetAmountText), -1, Rect, DT_RIGHT or DT_VCENTER or
          DT_SINGLELINE)
      else
      begin
        PriorColor := SetTextColor(hdc, GetSysColor(COLOR_GRAYTEXT));
        DrawText(hdc, PChar(GetAmountText), -1, Rect, DT_RIGHT or DT_VCENTER or
          DT_SINGLELINE);
         SetTextColor(hdc, PriorColor);
      end;
      SetBkMode(hdc, PriorMode);
      SelectObject(hdc, PriorFont);
    finally
      EndPaint(Handle, PS);
    end;
    Message.Result := 0;
  end
  else
    inherited;
end;

function TCashEdit.GetAmountText: string;
var
  S: string;
begin
  S := IntToStr(Abs(FDecimals));
  if moDollarSign in FOptions then
    S := S + 'm'
  else
    S := S + 'f';
  if FDecimals > 0 then
    Result := Format('%.' + S, [FAmount])
  else
    Result := Format('%-.' + S, [FAmount]);
end;

procedure TCashEdit.SetAmount(const Value: Double);
begin
  if Value <> FAmount then
  begin
    FAmount := Round(Value, FDecimals);
    FRevertText := FloatToStr(FAmount);
    Text := FRevertText;
    Repaint;
  end;
end;

procedure TCashEdit.SetDecimals(Value: Integer);
begin
  if Value <> FDecimals then
  begin
    FDecimals := Value;
    Invalidate;
  end;
end;

procedure TCashEdit.SetOptions(Value: TCashEditOptions);
begin
  FOptions := Value;
  Invalidate;
end;

{ TBackground }

constructor TBackground.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  Color := clAppWorkspace;
  ParentColor := False;
  SetBounds(Left, Top, 150, 100);
end;

procedure TBackground.Draw;
begin
	Paint;
end;

procedure TBackground.Paint;
begin
  FillRectColor(Canvas.Handle, ClientRect, Color);
	DrawThemeBorder(Canvas.Handle, ClientRect, [], 1, Left, Top);
	if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TBackground.PlaceControl(Control: TControl);
var
	R: TRect;
begin
	R := BoundsRect;
	InflateRect(R, -1, -1);
  if not ThemePainter.Enabled then
		InflateRect(R, -1, -1);
	Control.BoundsRect := R;
  Control.Anchors := Anchors;
end;

{ THorizontalBar }

var
  BarImage: TGraphic;
  BarAlignment: TAlignment;
  BarBackground: TColor;

procedure DefineBarBackground(Image: TGraphic; Alignment: TAlignment);
var
  B: TBitmap;
begin
  BarImage := Image;
  BarAlignment := Alignment;
  if BarImage <> nil then
  begin
    B := TBitmap.Create;
    try
      B.Width := 10;
      B.Height := 10;
      B.Canvas.Draw(0, 0, Image);
      BarBackground := B.Canvas.Pixels[0, 0];
    finally
      B.Free;
    end;
  end
  else
    BarBackground := clWindow;
end;

constructor THorizontalBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := False;
  SetBounds(Left, Top, 96, 48);
end;

procedure THorizontalBar.Draw;
begin
	Paint;
end;

procedure THorizontalBar.Paint;
var
  DC: HDC;
  Rect: TRect;
begin
  Rect := Classes.Rect(0, 0, Width, Height);
  DC := Canvas.Handle;
  DrawThemeSeperator(DC, Rect, Color, FThemed, FTransparent, FFlat);
  if FUseBackground and (BarImage <> nil) then
  begin
    FillRectColor(DC, Rect, BarBackground);
    case BarAlignment of
      taLeftJustify: Canvas.Draw(0, 0, BarImage);
      taRightJustify: Canvas.Draw(WidthOf(Rect) - BarImage.Width, 0, BarImage);
      taCenter: Canvas.Draw((WidthOf(Rect) - BarImage.Width) div 2, 0, BarImage);
    end;
    DrawThemeBar(DC, Rect, Color);
  end
  else
    DrawThemeSeperator(Canvas.Handle, Rect, Color, FThemed, FTransparent, FFlat);
	if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure THorizontalBar.SetFlat(Value: Boolean);
begin
	if Value <> FFlat then
  begin
  	FFlat := Value;
    Repaint;
  end;
end;

procedure THorizontalBar.SetThemed(Value: Boolean);
begin
	if Value <> FThemed then
  begin
	  FThemed := Value;
    Repaint;
  end;
end;

procedure THorizontalBar.SetTransparent(Value: Boolean);
begin
	if Value <> FTransparent then
  begin
	  FTransparent := Value;
    Repaint;
  end;
end;

procedure THorizontalBar.SetUseBackground(Value: Boolean);
begin
  if Value <> FUseBackground then
  begin
    FUseBackground := Value;
  end;
end;

end.
