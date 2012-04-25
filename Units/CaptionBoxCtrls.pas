unit CaptionBoxCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  FormTools, BaseTypes, GraphTools, ProviderTools, ImgListEx;

{ TCustomCaptionBox }

type
  TCustomHeaderSectionBox = class(TFramedWindow)
  private
    FActive: Boolean;
    FHeaderRect: TRect;
    FHeaderHeight: Integer;
    FOnDrawHeader: TDrawRectEvent;
    procedure MouseActivation;
    procedure SetActive(Value: Boolean);
    procedure SetHeaderHeight(Value: Integer);
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure WMChildSetFocus(var Msg: TMessage); message WM_CHILDSETFOCUS;
    procedure WMChildKillFocus(var Msg: TMessage); message WM_CHILDKILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property Active: Boolean read FActive write SetActive;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight;
    property OnDrawHeader: TDrawRectEvent read FOnDrawHeader write FOnDrawHeader;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  THeaderSectionBox = class(TCustomHeaderSectionBox)
  public
    property Canvas;
  published
    property Active;
    property Caption;
    property HeaderHeight;
    property OnDrawHeader;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TCaptionButtons = array of TRect;
  TCustomCaptionBox = class;

  ICaptionBoxProvider = interface(IControlProvider)
    ['{B2DBBD73-C455-4E1C-968E-8E3FECB56284}']
    function GetButtonCount: Integer;
    function GetButtonRect(Box: TCustomCaptionBox; Index: Integer): TRect;
    function GetHeaderAlign: TAlign;
    function GetHeaderRect(Box: TCustomCaptionBox): TRect;
    procedure DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState);
    procedure DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState);
    procedure ButtonClick(Box: TCustomCaptionBox; ButtonIndex: Integer);
    procedure DoubleClick(Box: TCustomCaptionBox);
  end;

  TCustomCaptionBox = class(TFramedImagesWindow)
  private
    FActive: Boolean;
    FHeaderRect: TRect;
    FHeaderHot: Boolean;
    FDownIndex: Integer;
    FHotIndex: Integer;
    FExpanded: Boolean;
    FExpandedHeight: Integer;
    FVisibleData: TVisibleDataArray;
    FSpaceDown: Boolean;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
    procedure MouseActivation;
    procedure SetActive(Value: Boolean);
    procedure SetExpanded(Value: Boolean);
    procedure SetExpandedHeight(const Value: Integer);
    function GetBoxProvider: ICaptionBoxProvider;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMChildSetFocus(var Msg: TMessage); message WM_CHILDSETFOCUS;
    procedure WMChildKillFocus(var Msg: TMessage); message WM_CHILDKILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure ButtonClick(Index: Integer);
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    property Active: Boolean read FActive write SetActive;
    property BoxProvider: ICaptionBoxProvider read GetBoxProvider;
    property Expanded: Boolean read FExpanded write SetExpanded default True;
    property ExpandedHeight: Integer read FExpandedHeight write SetExpandedHeight;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Expand;
    procedure Collapse;
    procedure Close;
  end;

{ TCaptionBox }

  TCaptionBox = class(TCustomCaptionBox)
  public
    property ParentBackground;
  published
    property Align;
    property Anchors;
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
    property TabOrder;
    property TabStop;
    property Expanded;
    property ExpandedHeight;
    property ProviderName;
    property Images;
    property ImageIndex;
    property OnClose;
    property OnCloseQuery;
    property OnExpand;
    property OnCollapse;
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
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

implementation

{$R CAPTIONBOXCTRLS.RES}

const
  RES_CAPTIONBOXCTRLS = 8801;

var
  InternalProviderImages: TObject;

function ProviderImages: TGlassImageList;

begin
  if InternalProviderImages = nil then
  begin
    Result := TGlassImageList.Create(Application);
    Result.Images.LoadFromResourceID(RES_CAPTIONBOXCTRLS);
    InternalProviderImages := Result;
  end
  else
    Result := TGlassImageList(InternalProviderImages);
end;

{ TDefaultCaptionBoxProvider }

type
  TBaseCaptionBoxProvider = class(TControlProvider, ICaptionBoxProvider)
  protected
    function GetButtonCount: Integer; virtual;
    function GetButtonRect(Box: TCustomCaptionBox; Index: Integer): TRect; virtual;
    function GetHeaderAlign: TAlign; virtual;
    function GetHeaderRect(Box: TCustomCaptionBox): TRect; virtual;
    procedure DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState); virtual;
    procedure DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState); virtual;
    procedure ButtonClick(Box: TCustomCaptionBox; ButtonIndex: Integer); virtual;
    procedure DoubleClick(Box: TCustomCaptionBox); virtual;
  end;

function TBaseCaptionBoxProvider.GetButtonCount: Integer;
begin
  Result := 1;
end;

function TBaseCaptionBoxProvider.GetButtonRect(Box: TCustomCaptionBox; Index: Integer): TRect;
begin
  Result := Box.ClientRect;
  Result.Bottom := Result.Top + GetSystemMetrics(SM_CYSMCAPTION) + 4;
  Result.Left := Result.Right - HeightOf(Result);
end;

function TBaseCaptionBoxProvider.GetHeaderAlign: TAlign;
begin
  Result := alTop;
end;

function TBaseCaptionBoxProvider.GetHeaderRect(Box: TCustomCaptionBox): TRect;
begin
  Result := Box.ClientRect;
  Result.Bottom := Result.Top + GetSystemMetrics(SM_CYSMCAPTION) + 4;
end;

procedure TBaseCaptionBoxProvider.DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState);
begin
end;

procedure TBaseCaptionBoxProvider.DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState);
begin
end;

procedure TBaseCaptionBoxProvider.ButtonClick(Box: TCustomCaptionBox; ButtonIndex: Integer);
begin
  Box.Close;
end;

procedure TBaseCaptionBoxProvider.DoubleClick(Box: TCustomCaptionBox);
begin
end;

type
  TDefaultCaptionBoxProvider = class(TBaseCaptionBoxProvider)
  protected
    procedure DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState); override;
    procedure DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState); override;
  end;

procedure TDefaultCaptionBoxProvider.DrawButton(Box: TCustomCaptionBox; Index: Integer;
  Rect: TRect; State: TDrawState);
begin
  if [dsSelected, dsHot] * State = [] then
    Include(State, dsDisabled);
  Rect.Top := Rect.Top + (HeightOf(Rect) - ProviderImages.Height) div 2;
  Rect.Left := Rect.Left + (WidthOf(Rect) - ProviderImages.Height) div 2;
  ProviderImages.DrawImage(Box.Canvas, Rect.Left, Rect.Top, 0, State);
end;

procedure TDefaultCaptionBoxProvider.DrawHeader(Box: TCustomCaptionBox; Rect: TRect;
  State: TDrawState);
var
  DC: HDC;
begin
  DC := Box.Canvas.Handle;
  if dsSelected in State then
    FillRectColor(DC, Rect, clActiveCaption)
  else
    FillRectColor(DC, Rect, clInactiveCaption);
  InflateRect(Rect, -HeightOf(Rect) div 4, 0);
  Dec(Rect.Right, HeightOf(Rect));
  if dsSelected in State then
    SetTextColor(DC, ColorToRGB(clCaptionText))
  else
    SetTextColor(DC, ColorToRGB(clInactiveCaptionText));
  DrawCaption(DC, Box.Caption, Rect, drLeft);
end;

{ TImageCaptionBoxProvider }

type
  TImageCaptionBoxProvider = class(TBaseCaptionBoxProvider)
  protected
    procedure DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState); override;
    procedure DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState); override;
  public
    class function GetProviderName: TProviderName; override;
  end;

class function TImageCaptionBoxProvider.GetProviderName: TProviderName;
begin
  Result := 'Image';
end;

procedure TImageCaptionBoxProvider.DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState);
begin
  if [dsSelected, dsHot] * State = [] then
    Include(State, dsDisabled);
  Rect.Top := Rect.Top + (HeightOf(Rect) - ProviderImages.Height) div 2;
  Rect.Left := Rect.Left + (WidthOf(Rect) - ProviderImages.Height) div 2;
  ProviderImages.DrawImage(Box.Canvas, Rect.Left, Rect.Top, 0, State);
end;

procedure TImageCaptionBoxProvider.DrawHeader(Box: TCustomCaptionBox;
  Rect: TRect; State: TDrawState);
var
  DC: HDC;
  A, B: TColor;
  R: TRect;
begin
  DC := Box.Canvas.Handle;
  if dsSelected in State then
  begin
    SetTextColor(DC, ColorToRGB(clHighlightText));
    A := clHighlight;
    B := clHighlight;
  end
  else if dsHot in State then
  begin
    A := Blend(clBtnFace, clBtnShadow, 90);
    B := Blend(clBtnFace, clBtnShadow, 60);
  end
  else
  begin
    A := Blend(clBtnFace, clBtnShadow, 40);
    B := Blend(clBtnFace, clBtnShadow, 10);
  end;
  DrawGradient(DC, Rect, A, B, drRight);
  R := Rect;
  Inc(R.Left, HeightOf(R) div 4);
  with Box do
    if (Images <> nil) and (Images.Count > 0) and (ImageIndex > -1) then
      R.Left := R.Left + Images.Height + 6;
  DrawCaption(DC, Box.Caption, R, drLeft, Box.Enabled);
  Rect.Top := Rect.Bottom;
  Rect.Bottom := Box.ClientHeight;
  DrawGradient(DC, Rect, Blend(clBtnFace, clWindow), clBtnFace, drRight);
  with Box do
    if (Images <> nil) and (Images.Count > 0) and (ImageIndex > -1) then
      Images.Draw(Canvas, 6, 1, ImageIndex, Enabled);
end;

{ TGradientCaptionBoxProvider }

type
  TGradientCaptionBoxProvider = class(TBaseCaptionBoxProvider)
  protected
    procedure DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState); override;
    procedure DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState); override;
  public
    class function GetProviderName: TProviderName; override;
  end;

class function TGradientCaptionBoxProvider.GetProviderName: TProviderName;
begin
  Result := 'Gradient';
end;

procedure TGradientCaptionBoxProvider.DrawButton(Box: TCustomCaptionBox; Index: Integer;
  Rect: TRect; State: TDrawState);
begin
  if [dsSelected, dsHot] * State = [] then
    Include(State, dsDisabled);
  Rect.Top := Rect.Top + (HeightOf(Rect) - ProviderImages.Height) div 2;
  Rect.Left := Rect.Left + (WidthOf(Rect) - ProviderImages.Height) div 2;
  ProviderImages.DrawImage(Box.Canvas, Rect.Left, Rect.Top, 0, State);
end;

procedure TGradientCaptionBoxProvider.DrawHeader(Box: TCustomCaptionBox; Rect: TRect;
  State: TDrawState);
var
  DC: HDC;
  A, B: TColor;
  F: HFONT;
begin
  DC := Box.Canvas.Handle;
  FillRectColor(DC, Rect, clThemeBorder);
  InflateRect(Rect, -1, -1);
  if dsSelected in State then
  begin
    A := clThemeBorder;
    B := Blend(clThemeBorder, clBlack, 75);
  end
  else if dsHot in State then
  begin
    A := Blend(clThemeBorder, clWhite);
    B := clThemeBorder;
  end
  else
  begin
    A := Blend(clThemeBorder, clWhite, 75);
    B := Blend(clThemeBorder, clBlack, 95);
  end;
  DrawGradient(DC, Rect, A, B, drDown);
  InflateRect(Rect, -HeightOf(Rect) div 4, 0);
  Dec(Rect.Right, HeightOf(Rect));
  F := SelectObject(DC, GetFont(DC, [fsBold]));
  SetTextColor(DC, ColorToRGB(clCaptionText));
  DrawCaption(DC, Box.Caption, Rect, drLeft);
  OverwriteObject(DC, F);
end;

{ TGroupCaptionBoxProvider }

type
  TGroupCaptionBoxProvider = class(TBaseCaptionBoxProvider)
  protected
    procedure Init(Control: TControl); override;
    function GetButtonRect(Box: TCustomCaptionBox; Index: Integer): TRect; override;
    procedure DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState); override;
    procedure DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState); override;
    procedure ButtonClick(Box: TCustomCaptionBox; ButtonIndex: Integer); override;
    procedure DoubleClick(Box: TCustomCaptionBox); override;
  public
    class function GetProviderName: TProviderName; override;
  end;

class function TGroupCaptionBoxProvider.GetProviderName: TProviderName;
begin
  Result := 'Grouped';
end;

procedure TGroupCaptionBoxProvider.Init(Control: TControl);
begin
  if Control is TCaptionBox then
    TCaptionBox(Control).BorderStyle := bsNone;
end;

function TGroupCaptionBoxProvider.GetButtonRect(Box: TCustomCaptionBox; Index: Integer): TRect;
begin
  Result := Box.ClientRect;
  Result.Bottom := Result.Top + GetSystemMetrics(SM_CYSMCAPTION);
  Result.Right := Result.Left + HeightOf(Result);
end;

procedure TGroupCaptionBoxProvider.DrawButton(Box: TCustomCaptionBox; Index: Integer;
  Rect: TRect; State: TDrawState);
var
  I: Integer;
begin
  if [dsSelected, dsHot] * State = [] then
    Include(State, dsDisabled);
  Rect.Top := Rect.Top + (HeightOf(Rect) - ProviderImages.Height) div 2;
  Rect.Left := Rect.Left + (WidthOf(Rect) - ProviderImages.Height) div 2;
  if Box.Expanded then
    I := 1
  else
    I := 2;
  ProviderImages.DrawImage(Box.Canvas, Rect.Left, Rect.Top, I, State);
end;

procedure TGroupCaptionBoxProvider.DrawHeader(Box: TCustomCaptionBox; Rect: TRect;
  State: TDrawState);
var
  DC: HDC;
begin
  DC := Box.Canvas.Handle;
  Rect.Bottom := Box.Height;
  FillRectColor(DC, Rect, Box.Color);
  if Box.Expanded then
    Include(State, dsExpanded);
  DrawThemeGroupBox(DC, Box.Caption, Rect, State);
end;

procedure TGroupCaptionBoxProvider.ButtonClick(Box: TCustomCaptionBox;
  ButtonIndex: Integer);
begin
  Box.Expanded := not Box.Expanded;
end;

procedure TGroupCaptionBoxProvider.DoubleClick(Box: TCustomCaptionBox);
begin
  Box.Expanded := not Box.Expanded;
end;

{ TCategoryCaptionBoxProvider }

type
  TCategoryCaptionBoxProvider = class(TGroupCaptionBoxProvider)
  protected
    procedure DrawButton(Box: TCustomCaptionBox; Index: Integer; Rect: TRect; State: TDrawState); override;
    procedure DrawHeader(Box: TCustomCaptionBox; Rect: TRect; State: TDrawState); override;
  public
    class function GetProviderName: TProviderName; override;
  end;

class function TCategoryCaptionBoxProvider.GetProviderName: TProviderName;
begin
  Result := 'Category';
end;

procedure TCategoryCaptionBoxProvider.DrawButton(Box: TCustomCaptionBox; Index: Integer;
  Rect: TRect; State: TDrawState);
var
  C: TColor;
begin
  if [dsSelected, dsHot] * State = [] then
    Include(State, dsDisabled);
  begin
    if ThemePainter.Enabled then
      C := clHighlight
    else
      C := clBtnShadow;
  ProviderImages.Images.DrawColor(Box.Canvas, 3, Rect.Left, Rect.Top, C, $FF);
  if dsPressed in State then
    ProviderImages.Images.DrawColor(Box.Canvas, 4, Rect.Left, Rect.Top, C, $80)
  else if dsHot in State then
    ProviderImages.Images.DrawColor(Box.Canvas, 4, Rect.Left, Rect.Top, C, $50)
  else
    ProviderImages.Images.DrawColor(Box.Canvas, 3, Rect.Left, Rect.Top, C, $FF);
  end;
//  else
  begin
    //FillRectColor(Box.Canvas.Handle, Rect, clBtnFace);
    // DrawThemeButton(Box.Canvas.Handle, Rect, State);
  end;
  if Box.Expanded then
    ProviderImages.DrawImage(Box.Canvas, Rect.Left + 1, Rect.Top, 5, [])
  else
    ProviderImages.DrawImage(Box.Canvas, Rect.Left + 1, Rect.Top + 1, 6, []);
end;

procedure TCategoryCaptionBoxProvider.DrawHeader(Box: TCustomCaptionBox; Rect: TRect;
  State: TDrawState);
var
  DC: HDC;
  R: TRect;
  C: TColor;
  F: HFONT;
begin
  DC := Box.Canvas.Handle;
  F := SelectObject(DC, GetFont(DC, [fsBold]));
  Rect.Left := Round(HeightOf(Rect) * 1.25);
  R := Rect;
  R.Right := R.Left + FontWidth(DC, Box.Caption);
    R.Top := R.Top + HeightOf(R) div 2;
    R.Top := R.Top - FontHeight(DC) div 2;
    R.Bottom := R.Top + FontHeight(DC);
    InflateRect(R, 2, 2);
  if not ThemePainter.Enabled then
  begin

  end;
  if dsHot in State then
    DrawStyleRect(DC, R, True)
  else if Box.Focused then
    DrawFocus(DC, R);
  DrawCaption(DC, Box.Caption, Rect, drLeft, Box.Enabled);
  Rect.Left := R.Right + 3;
  Rect.Top := Rect.Top + HeightOf(Rect)  div 2 - 1;
  Rect.Bottom := Rect.Top + 2;
  R := Rect;
  R.Right := R.Left + WidthOf(R) div 2;
  if ThemePainter.Enabled then
    C := clSelectedBorder
  else
    C := clBtnFace;
  DrawGradient(DC, R, Box.Color, C, drRight);
  R.Left := R.Right;
  R.Right := Rect.Right;
  DrawGradient(DC, R, Box.Color, C, drLeft);
  Dec(R.Left);
  //FillRectColor(DC, R, clSelectedBorder);
  OverwriteObject(DC, F);
end;

{ TCustomHeaderSectionBox }

constructor TCustomHeaderSectionBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csSetCaption, csOpaque,
    csReplicatable];
  FHeaderHeight := 20;
end;

procedure TCustomHeaderSectionBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.style := Params.WindowClass.style and (not (CS_VREDRAW or CS_HREDRAW));
end;

procedure TCustomHeaderSectionBox.AlignControls(AControl: TControl; var Rect: TRect);
var
  H: Integer;
  A, B: TRect;
  C: TControl;
  I: Integer;
begin
  if FHeaderHeight = 0 then
  begin
    inherited AlignControls(AControl, Rect);
    Exit;
  end;
  if not HandleAllocated or (csDestroying in ComponentState) then Exit;
  FHeaderRect := ClientRect;
  FHeaderRect.Bottom := FHeaderHeight;
  H := HeightOf(FHeaderRect);
  Rect.Top := H;
  A := Rect;
  inherited AlignControls(AControl, Rect);
  for I := 0 to ControlCount - 1 do
  begin
    C := Controls[I];
    B := C.BoundsRect;
    if C.Align <> alNone then
      Continue;
    if B.Top < A.Top then
    begin
      OffsetRect(B, 0, H - B.Top);
      C.BoundsRect := B;
    end;
  end;
end;

procedure TCustomHeaderSectionBox.Paint;
const
  BackgroundColors: array[Boolean] of TColor = (clInactiveCaption, clActiveCaption);
  TextColors: array[Boolean] of TColor = (clInactiveCaptionText, clCaptionText);
var
  DC: HDC;
  R: TRect;
begin
  inherited Paint;
  if FHeaderHeight > 0 then
  begin
    R := ClientRect;
    R.Bottom := HeaderHeight;
    if Assigned(FOnDrawHeader) then
      FOnDrawHeader(Self, Canvas, R)
    else
    begin
      DC := Canvas.Handle;
      FillRectColor(DC, R, BackgroundColors[Active]);
      SetTextColor(DC, ColorToRGB(TextColors[Active]));
      InflateRect(R, -2, -2);
      DrawCaption(DC, Caption, R, drLeft);
    end;
  end
end;

procedure TCustomHeaderSectionBox.MouseActivation;
begin
  Active := True;
  if IsChild(Handle, GetFocus) then Exit;
  SelectNext(nil, True, True);
  if not IsChild(Handle, GetFocus) then SetFocus;
end;

procedure TCustomHeaderSectionBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    MouseActivation;
end;

procedure TCustomHeaderSectionBox.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    Repaint;
  end;
end;

procedure TCustomHeaderSectionBox.SetHeaderHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FHeaderHeight then
  begin
    FHeaderHeight := Value;
    Realign;
    Repaint;
  end;
end;

procedure TCustomHeaderSectionBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCustomHeaderSectionBox.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCustomHeaderSectionBox.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCustomHeaderSectionBox.WMChildSetFocus(var Msg: TMessage);
begin
  inherited;
  Active := True;
end;

procedure TCustomHeaderSectionBox.WMChildKillFocus(var Msg: TMessage);
begin
  inherited;
  Active := False;
end;

procedure TCustomHeaderSectionBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Repaint;
end;

procedure TCustomHeaderSectionBox.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  Repaint;
end;

{ TCustomCaptionBox }

constructor TCustomCaptionBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDownIndex := -1;
  FHotIndex := -1;
  FExpanded := True;
  FExpandedHeight := 150;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csSetCaption, csOpaque,
    csReplicatable];
  Height := FExpandedHeight;
  Width := 325;
  Provider := nil;
end;

function TCustomCaptionBox.GetBoxProvider: ICaptionBoxProvider;
begin
  if not Supports(Provider, ICaptionBoxProvider, Result) then
    Result := nil;
end;

procedure TCustomCaptionBox.Expand;
begin
  Expanded := True;
end;

procedure TCustomCaptionBox.Collapse;
begin
  Expanded := False;
end;

procedure TCustomCaptionBox.Close;
var
  CanClose: Boolean;
begin
  if not Visible then Exit;
  CanClose := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, CanClose);
  if CanClose then
  begin
    if Assigned(FOnClose) then
      FOnClose(Self);
    Hide;
  end;
end;

procedure TCustomCaptionBox.MouseActivation;
begin
  Active := True;
  if IsChild(Handle, GetFocus) then Exit;
  SelectNext(nil, True, True);
  if not IsChild(Handle, GetFocus) then SetFocus;
end;

procedure TCustomCaptionBox.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    Repaint;
  end;
end;

procedure TCustomCaptionBox.SetExpanded(Value: Boolean);
var
  Event: TNotifyEvent;
  W: TWinControl;
  L: TList;
  H, I: Integer;
begin
  if BoxProvider = nil then Exit;
  if Value <> FExpanded then
  begin
    FExpanded := Value;
    if Value then
    begin
      Height := FExpandedHeight;
      Event := FOnExpand;
      if not (csDesigning in ComponentState) then
      begin
        L := TList.Create;
        try
          for I := 0 to ControlCount - 1 do
            L.Add(Controls[I]);
          for I := Low(FVisibleData) to High(FVisibleData) do
            if L.IndexOf(FVisibleData[I].Control) < 0 then
              FVisibleData[I].Control := nil;
        finally
          L.Free;
        end;
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
      FExpandedHeight := Height;
      H := HeightOf(FHeaderRect);
      if H < 1 then
      begin
        FHeaderRect := BoxProvider.GetHeaderRect(Self);
        H := HeightOf(FHeaderRect);
      end;
      if BorderStyle = bsSingle then
        Inc(H, 4);
      Height := H;
      Event := FOnCollapse;
			if IsChild(Handle, GetFocus) then
	    	SetFocus;
	    if not (csDesigning in ComponentState) then
		    FVisibleData := SaveVisible(Self);
    end;
    if Assigned(Event) then
      Event(Self);
    if HandleAllocated then
      SendMessage(Handle, WM_NCPAINT, 0, 0);
    Invalidate;
  end;
end;

procedure TCustomCaptionBox.SetExpandedHeight(const Value: Integer);
begin
  if Value <> FExpandedHeight then
  begin
    FExpandedHeight := Value;
    if FExpanded then
      Height := FExpandedHeight;
		Invalidate;
  end;
end;

procedure TCustomCaptionBox.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCustomCaptionBox.CMSysColorChange(var Msg: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCustomCaptionBox.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Repaint;
end;

procedure TCustomCaptionBox.CMMouseLeave(var Msg: TMessage);
var
  R: TRect;
begin
  inherited;
  if FHeaderHot then
  begin
    FHeaderHot := False;
    InvalidateRect(Handle, @FHeaderRect, True);
  end;
  if (FHotIndex > -1) and (BoxProvider <> nil) then
  begin
    R := BoxProvider.GetButtonRect(Self, FHotIndex);
    FHotIndex := -1;
    InvalidateRect(Handle, @R, True);
  end;
end;

procedure TCustomCaptionBox.WMChildSetFocus(var Msg: TMessage);
begin
  inherited;
  Active := True;
end;

procedure TCustomCaptionBox.WMChildKillFocus(var Msg: TMessage);
begin
  inherited;
  Active := False;
end;

procedure TCustomCaptionBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Repaint;
end;

procedure TCustomCaptionBox.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  Repaint;
end;

function Compare(const A, B: TRect): Boolean;
begin
  Result := CompareMem(@A, @B, SizeOf(A));
end;

procedure TCustomCaptionBox.AlignControls(AControl: TControl; var Rect: TRect);
var
  W, H: Integer;
  A, B: TRect;
  C: TControl;
  I: Integer;
begin
  if not HandleAllocated or (csDestroying in ComponentState) or (BoxProvider = nil) then Exit;
  FHeaderRect := BoxProvider.GetHeaderRect(Self);
  W := WidthOf(FHeaderRect);
  H := HeightOf(FHeaderRect);
  case BoxProvider.GetHeaderAlign of
    alLeft: Rect.Right := W;
    alTop: Rect.Top := H;
    alRight: Dec(Rect.Right, W);
    alBottom: Dec(Rect.Bottom, H);
  end;
  A := Rect;
  inherited AlignControls(AControl, Rect);
  if BoxProvider.GetHeaderAlign in [alLeft,alTop, alRight, alBottom] then
    for I := 0 to ControlCount - 1 do
    begin
      C := Controls[I];
      B := C.BoundsRect;
      if C.Align <> alNone then
        Continue;
      case BoxProvider.GetHeaderAlign of
        alLeft:
          if B.Left < A.Left then
          begin
            OffsetRect(B, W - B.Left, 0);
            C.BoundsRect := B;
          end;
        alTop:
          if B.Top < A.Top then
          begin
            OffsetRect(B, 0, H - B.Top);
            C.BoundsRect := B;
          end;
      end;
    end;
end;

procedure TCustomCaptionBox.ButtonClick(Index: Integer);
begin
  if BoxProvider <> nil then
    BoxProvider.ButtonClick(Self, Index);
end;

procedure TCustomCaptionBox.Paint;
var
  R: TRect;
  S: TDrawState;
  I: Integer;
begin
  inherited Paint;
  if BoxProvider = nil then Exit;
  R := BoxProvider.GetHeaderRect(Self);
  if not Compare(R, FHeaderRect) then
  begin
    FHeaderRect := R;
    Realign;
  end;
  S := [];
  if not Enabled then
    Include(S, dsDisabled);
  if Active then
    Include(S, dsSelected);
  if FHeaderHot then
    Include(S, dsHot);
  BoxProvider.DrawHeader(Self, R, S);
  for I := 0 to BoxProvider.GetButtonCount - 1 do
  begin
    S := [];
    if not Enabled then
      Include(S, dsDisabled);
    if Active then
      Include(S, dsSelected);
    if I = FHotIndex then
    begin
      Include(S, dsHot);
      if I = FDownIndex then
        Include(S, dsPressed);
    end;
    R := BoxProvider.GetButtonRect(Self, I);
    BoxProvider.DrawButton(Self, I, R, S);
  end;
end;

procedure TCustomCaptionBox.DblClick;
begin
  inherited DblClick;
  if BoxProvider <> nil then
    BoxProvider.DoubleClick(Self);
end;

procedure TCustomCaptionBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_SPACE then
    FSpaceDown := True;
end;

procedure TCustomCaptionBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key = VK_SPACE then
    if FSpaceDown then
    begin
      FSpaceDown := False;
      if BoxProvider <> nil then
        BoxProvider.ButtonClick(Self, 0);
    end;
end;

procedure TCustomCaptionBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  R: TRect;
  B: Integer;
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (BoxProvider <> nil) then
  begin
    P := Point(X, Y);
    B := FDownIndex;
    for I := 0 to BoxProvider.GetButtonCount - 1 do
    begin
      R := BoxProvider.GetButtonRect(Self, I);
      if PtInRect(R, P) then
      begin
        B := I;
        Break;
      end;
    end;
    if B > -1 then
    begin
      if B > -1 then
        InvalidateRect(Handle, @R, True);
      FDownIndex := B;
    end
    else
      MouseActivation;
  end;
end;

procedure TCustomCaptionBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  H: Boolean;
  R: TRect;
  B: Integer;
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if BoxProvider = nil then Exit;
  P := Point(X, Y);
  R := BoxProvider.GetHeaderRect(Self);
  H := PtInRect(FHeaderRect, P);
  if H <> FHeaderHot then
  begin
    FHeaderHot := H;
    InvalidateRect(Handle, @R, True);
  end;
  B := -1;
  for I := 0 to BoxProvider.GetButtonCount - 1 do
  begin
    R := BoxProvider.GetButtonRect(Self, I);
    if PtInRect(R, P) then
    begin
      B := I;
      Break;
    end;
  end;
  if B <> FHotIndex then
  begin
    if B > -1 then
      InvalidateRect(Handle, @R, True);
    if FHotIndex > -1 then
    begin
      R := BoxProvider.GetButtonRect(Self, FHotIndex);
      InvalidateRect(Handle, @R, True);
    end;
    FHotIndex := B;
  end;
end;

procedure TCustomCaptionBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  R: TRect;
  B: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (BoxProvider <> nil) then
  begin
    B := FDownIndex;
    FDownIndex := -1;
    if B > -1 then
    begin
      P := Point(X, Y);
      R := BoxProvider.GetButtonRect(Self, B);
      InvalidateRect(Handle, @R, True);
      if PtInRect(R, P) then
        ButtonClick(B);
    end;
  end;
end;

procedure TCustomCaptionBox.Resize;
begin
  if BoxProvider <> nil then
    BoxProvider.GetHeaderRect(Self);
end;

initialization
  RegisterDefaultProvider(TDefaultCaptionBoxProvider, TCaptionBox);
  RegisterProvider(TGradientCaptionBoxProvider, [TCaptionBox]);
  RegisterProvider(TGroupCaptionBoxProvider, [TCaptionBox]);
  RegisterProvider(TCategoryCaptionBoxProvider, [TCaptionBox]);
  RegisterProvider(TImageCaptionBoxProvider, [TCaptionBox]);
end.
