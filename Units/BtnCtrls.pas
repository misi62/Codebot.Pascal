
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit BtnCtrls;

interface

{$I CODEBOT.INC}

uses
  Classes, ImgList, Controls, Windows, Menus, Forms, Messages, SysUtils,
  StdCtrls, ActnList, Graphics, BaseTypes, GraphTools, ProviderTools,
  {$IFNDEF LITE}ScrollCtrls, PopCtrls, StrTools,{$ENDIF} FormTools;

{ TImageSpeedButton class }

type
  TDrawEvent = procedure(Control: TControl; Rect: TRect;
    DrawState: TDrawState; var DefaultDraw: Boolean) of object;

  TButtonStyle = (bsFramed, bsBeveled, bsFlat, bsTransparent);
  TPressedState = (psNone, psButtonDown, psMenuButtonDown);

{ Button options described

    boAutoFocus: Button will focus itself when pressed
    boAutoPopup: Popumenu will be automatically shown when the menu was clicked
    boAutoSize: Button will size itself based on caption, image, and padding
    boClean: Button will not display 3D elemments on its face
    boToggle: Clicking the button toggles its down state
    boGroup: Button uses the parent control to toggle down state
    boMenu: Button has a clickable down arrow icon,
    boSideways: When boAutoPopup is set the popup menu will appear to the side
    boLocked: Clicking the menu arrow clicks the entire button
    boWide: Button is the same width as if boMenu were set
    boOpaque: Images are drawn fully opaque }

  TButtonOption = (boAutoFocus, boAutoPopup, boAutoSize, boClean, boToggle, boGrouped, boMenu,
    boSideways, boLocked, boWide, boOpaque);

  TButtonOptions = set of TButtonOption;

  TImageSpeedButton = class(TProviderControl)
  private
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageChangeLink: TChangeLink;
    FPressed: Boolean;
    FHot: Boolean;
    FDown: Boolean;
    FToggle: Boolean;
    FOnDrawButton: TDrawEvent;
    FOnHotChange: TNotifyEvent;
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
    procedure ImageListChange(Sender: TObject);
    procedure SetDown(Value: Boolean);
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DrawButton(const Rect: TRect; State: TDrawState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property Hot: Boolean read FHot;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Down: Boolean read FDown write SetDown;
    property Toggle: Boolean read FToggle write FToggle;
    property Align;
    property Anchors;
    property Color;
    property Cursor;
    property Visible;
    property Enabled;
    property Hint;
    property ShowHint;
    property ParentShowHint;
    property OnClick;
    property OnDblClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnHotChange: TNotifyEvent read FOnHotChange write FOnHotChange;
    //property OnMouseTrack;
    property OnDrawButton: TDrawEvent read FOnDrawButton write FOnDrawButton;
  end;

{ TCustomImageButton class }

  TCustomImageButton = class(TProviderWindow)
  private
    FCaptionPosition: TCaptionPosition;
    FDown: Boolean;
    FMouseInControl: Boolean;
    FSpacePressed: Boolean;
    FPadding: Integer;
    FFocusedRect: Boolean;
    FImageIndex: Integer;
    FImageHotIndex: Integer;
    FImagePressedIndex: Integer;
    FImageDisabledIndex: Integer;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FOptions: TButtonOptions;
    FPressedState: TPressedState;
    FStyle: TButtonStyle;
    FOnMenuButtonClick: TNotifyEvent;
    FOnCustomDraw: TDrawEvent;
    procedure ImageListChange(Sender: TObject);
    function GetImageWidth: Integer;
    procedure SetCaptionPosition(Value: TCaptionPosition);
    procedure SetDown(Value: Boolean);
    procedure SetFocusedRect(Value: Boolean);
    procedure SetImageIndex(Index: Integer; Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetOptions(Value: TButtonOptions);
    procedure SetStyle(Value: TButtonStyle);
    {$IFDEF D7_UP}
    function GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
   {$ENDIF}
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTabStopChanged(var Message: TMessage); message CM_TABSTOPCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMWindowPosChanging(var Msg: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetPadding(const Value: Integer);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AdjustSize; override;
    procedure Loaded; override;
    function CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean; virtual;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MenuButtonClick; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property CaptionPosition: TCaptionPosition read FCaptionPosition write
      SetCaptionPosition;
    property FocusedRect: Boolean read FFocusedRect write SetFocusedRect default False;
    property ImageIndex: Integer index 0 read FImageIndex write SetImageIndex  default -1;
    property ImageHotIndex: Integer index 1 read FImageHotIndex write SetImageIndex default -1;
    property ImagePressedIndex: Integer index 2 read FImagePressedIndex write SetImageIndex default -1;
    property ImageDisabledIndex: Integer index 3 read FImageDisabledIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages default nil;
    property ImageWidth: Integer read GetImageWidth;
    property Options: TButtonOptions read FOptions write SetOptions;
    property Padding: Integer read FPadding write SetPadding default 0;
    property Style: TButtonStyle read FStyle write SetStyle;
    property Down: Boolean read FDown write SetDown;
    {$IFDEF D7_UP}
    property Transparent: Boolean read GetTransparent write SetTransparent default True;
   {$ENDIF}
    property OnCustomDraw: TDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnMenuButtonClick: TNotifyEvent read FOnMenuButtonClick write FOnMenuButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Popup; dynamic;
    procedure Repaint; override;
  published
    property Visible;
  end;

{ TImageButtonActionLink }

  TImageButtonActionLink = class(TWinControlActionLink)
  protected
    FClient: TCustomImageButton;
    procedure AssignClient(AClient: TObject); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
  end;

{ TImageButton }

  TImageButton = class(TCustomImageButton)
  public
    property Canvas;
  published
    property Action;
    property Anchors;
    property Align;
    property Caption;
    property CaptionPosition;
    property Color;
    property Down;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusedRect;
    property Font;
    property ImageIndex;
    property ImageHotIndex;
    property ImagePressedIndex;
    property ImageDisabledIndex;
    property Images;
    property Options;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Transparent;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCustomDraw;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMenuButtonClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TPopButton }

{$IFNDEF LITE}

  TPopButton = class(TCustomImageButton)
  private
    FOnChange: TNotifyEvent;
    FPopupForm: TCustomPopupForm;
    procedure PopupFormSelect(Sender: TObject);
  protected
    procedure Change; virtual;
    function CreatePopup: TCustomPopupForm; virtual; abstract;
    procedure MenuButtonClick; override;
    property PopupForm: TCustomPopupForm read FPopupForm;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
    procedure Click; override;
  published
    property Anchors;
    property ActionLink;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusedRect;
    property Font;
    property ImageIndex;
    property Images;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Options;
    property Style;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

{ TCheckListButton }

  TCheckListButton = class(TPopButton)
  private
    FPopupCheckList: TPopupCheckListForm;
    FPopupHeight: Integer;
    FOnClickCheck: TNotifyEvent;
    FOnClickItem: TNotifyEvent;
    function GetChecked(Index: Integer): Boolean;
    procedure SetChecked(Index: Integer; Value: Boolean);
    function GetCheckText: string;
    function GetFlat: Boolean;
    procedure SetFlat(Value: Boolean);
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; Value: Boolean);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
    function GetSizeable: Boolean;
    procedure SetSizeable(Value: Boolean);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetState(Index: Integer; Value: TCheckBoxState);
    function GetStatusText: string;
    procedure SetStatusText(Value: string);
    procedure SetPopupHeight(Value: Integer);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure DoClickCheck(Sender: TObject);
    procedure DoClickItem(Sender: TObject);
    procedure MenuButtonClick; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
    property CheckText: string read GetCheckText;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
    property PopupHeight: Integer read FPopupHeight write SetPopupHeight;
  published
    property Caption;
    property CaptionPosition;
    property Flat: Boolean read GetFlat write SetFlat;
    property Font;
    property Images;
    property ImageIndex;
    property Items: TStrings read GetItems write SetItems;
    property ShowHint;
    property StatusText: string read GetStatusText write SetStatusText;
    property Sizeable: Boolean read GetSizeable write SetSizeable;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnClickCheck: TNotifyEvent read FOnClickCheck write FOnClickCheck;
    property OnClickItem: TNotifyEvent read FOnClickItem write FOnClickItem;
    property OnContextPopup;
    property OnCustomDraw;
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

{ TColorGridButton }

  TColorGridButton = class(TPopButton)
  private
   function GetActiveColor: TColor;
   procedure SetActiveColor(Value: TColor);
   procedure CNColorPick(var Msg: TMessage); message CN_COLORPICK;
  protected
    function CreatePopup: TCustomPopupForm; override;
    function CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean; override;
  published
    property ActiveColor: TColor read GetActiveColor write SetActiveColor;
  end;

{ TBrushButton }

  TBrushButton = class(TPopButton)
  private
   function GetBrushStyle: TBrushStyle;
   procedure SetBrushStyle(Value: TBrushStyle);
   function GetMode: TListMode;
   procedure SetMode(Value: TListMode);
  protected
    function CreatePopup: TCustomPopupForm; override;
    function CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean; override;
  published
    property BrushStyle: TBrushStyle read GetBrushStyle write SetBrushStyle;
    property Mode: TListMode read GetMode write SetMode;
  end;

{ TPenButton }

  TPenButton = class(TPopButton)
  private
   function GetPenStyle: TPenStyle;
   procedure SetPenStyle(Value: TPenStyle);
   function GetMode: TListMode;
   procedure SetMode(Value: TListMode);
  protected
    function CreatePopup: TCustomPopupForm; override;
    function CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean; override;
  published
    property PenStyle: TPenStyle read GetPenStyle write SetPenStyle;
    property Mode: TListMode read GetMode write SetMode;
  end;

{$ENDIF}

{ TThemeGlyphButton }

  TThemeGlyphKind = (tgToolClose, tgClose, tgPin);

  TThemeGlyphButton = class(TGraphicControl)
  private
    FDown: Boolean;
    FKind: TThemeGlyphKind;
    FStyle: TButtonStyle;
    FMouseInControl: Boolean;
    procedure SetKind(Value: TThemeGlyphKind);
    procedure SetStyle(Value: TButtonStyle);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Kind: TThemeGlyphKind read FKind write SetKind;
    property Style: TButtonStyle read FStyle write SetStyle;
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

implementation

{ TImageSpeedButton }

constructor TImageSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csParentBackground, csDoubleClicks]; //
  Width := 24;
  Height := 24;
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TImageSpeedButton.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TImageSpeedButton.ImageListChange(Sender: TObject);
begin
  Repaint;
end;

procedure TImageSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FPressed := True;
    Repaint;
  end;
end;

procedure TImageSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    P := FPressed;
    FPressed := False;
    if P and PtInRect(ClientRect, Point(X, Y)) then
    begin
      if FToggle then
        FDown := not FDown;
      Repaint;
      Click;
    end
    else
      Repaint;
  end;
end;

procedure TImageSpeedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

procedure TImageSpeedButton.SetImages(Value: TCustomImageList);
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
    AdjustSize;
    Repaint;
  end;
end;

procedure TImageSpeedButton.SetImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Repaint;
  end;
end;

procedure TImageSpeedButton.SetDown(Value: Boolean);
begin
  if Value <> FDown then
  begin
    FDown := Value;
    Invalidate;
  end;
end;

procedure TImageSpeedButton.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  FHot := True;
  Invalidate;
  if Assigned(FOnHotChange) then
    FOnHotChange(Self);
end;

procedure TImageSpeedButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  FHot := False;
  Invalidate;
  if Assigned(FOnHotChange) then
    FOnHotChange(Self);
end;

procedure TImageSpeedButton.DrawButton(const Rect: TRect; State: TDrawState);
var
  DefaultDraw: Boolean;
begin
  if csDesigning in ComponentState then
  begin
    if dsChecked in State then
      DrawRectOutline(Canvas.Handle, Rect, clBlack)
    else
      DrawRectOutline(Canvas.Handle, Rect, cl3DDkShadow);
  end
  else
  begin
    DefaultDraw := True;
    if Assigned(FOnDrawButton) then
      FOnDrawButton(Self, Rect, State, DefaultDraw);
    if DefaultDraw then
      DrawThemeThinButton(Canvas.Handle, Rect, State);
  end;
end;

procedure TImageSpeedButton.Paint;
var
  S: TDrawState;
  I, W, H: Integer;
begin
  inherited Paint;
  S := [];
  if not Enabled then
    Include(S, dsDisabled);
  if FDown then
    Include(S, dsChecked);
  if FHot or FDown then
  begin
    Include(S, dsHot);
    if FPressed or FDown then
      Include(S, dsPressed);
  end;
  DrawButton(ClientRect, S);
  if FImages <> nil then
  begin
    I := FImages.Height;
    W := Width;
    H := Height;
    ImageListDraw(FImages, Canvas, (W - I) div 2, (H - I) div 2, FImageIndex, Enabled, FHot);
  end;
end;

{ TCustomImageButton }

const
  FLAT_AUTO = 1;
  FLAT_SPACE = 8;
  FLAT_MENUWIDTH = 12;

constructor TCustomImageButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentFont := True;
  if ThemePainter.Enabled then
  begin
    ControlStyle := ControlStyle + [csParentBackground, csCaptureMouse]  - [csClickEvents];
    ParentBackground := True;
  end
  else
  begin
    ControlStyle := ControlStyle + [csCaptureMouse]  - [csClickEvents];
  end;
  //TabStop := True;
  Height := 22;
  Width := 75;
  FOptions := [boAutoFocus];
  FImageIndex := -1;
  FImageIndex := -1;
  FImageHotIndex := -1;
  FImagePressedIndex := -1;
  FImageDisabledIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TCustomImageButton.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TCustomImageButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TCustomImageButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if (Sender = Action) and (Action is TCustomAction) then
  begin
    FImageHotIndex := -1;
    FImagePressedIndex := -1;
    FImageDisabledIndex := -1;
     Images := TCustomAction(Action).ActionList.Images;
    ImageIndex := TCustomAction(Action).ImageIndex;
    Repaint;
  end;
end;

procedure TCustomImageButton.ImageListChange(Sender: TObject);
begin
  AdjustSize;
end;

procedure TCustomImageButton.AdjustSize;
var
  CaptionSize: TSize;
  ImageSize: TSize;
  S: string;
begin
  if csLoading in ComponentState then Exit; //) or (not HandleAllocated)
  if boAutoSize in FOptions then
  begin
    S := Caption;
    if S = '' then S := 'Wg';
    CaptionSize := CalcCaptionSize(Canvas.Handle, S);
    if Caption = '' then
      CaptionSize.cx := 0;
    if CaptionPosition = cpHidden then
    begin
      CaptionSize.cx := 0;
       CaptionSize.cy := 0;
    end
    else
    begin
      Inc(CaptionSize.cx, 2 + FPadding);
      Inc(CaptionSize.cy, 2 + FPadding);
    end;
    if FImages <> nil then
    begin
      ImageSize.cx := FImages.Width + FPadding;
       ImageSize.cy := FImages.Height + FPadding;
    end
    else
    begin
      ImageSize.cx := FLAT_MENUWIDTH + FPadding;
       ImageSize.cy := FLAT_MENUWIDTH + FPadding;
    end;
    if CaptionSize.cy > ImageSize.cy then
      ImageSize.cy := CaptionSize.cy;
    if FOptions * [boMenu, boWide] <> [] then
      Inc(CaptionSize.cx, FLAT_MENUWIDTH div 2);
    {if CaptionPosition <> cpHidden then
      Inc(CaptionSize.cx, 3);}
    SetWindowPos(Handle, 0, 0, 0, ImageSize.cx + CaptionSize.cx,
      ImageSize.cy, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOZORDER);
    RequestAlign;
  end
  else
    inherited AdjustSize;
end;

procedure TCustomImageButton.Loaded;
begin
  inherited Loaded;
  AdjustSize;
end;

procedure TCustomImageButton.Click;
begin
  if boToggle in FOptions then
    Down := not Down;
  if boAutoPopup in Options then
    Popup;
  inherited Click;
end;

function TCustomImageButton.CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean;
begin
  Result := True;
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, Rect, DrawState, Result);
end;

function TCustomImageButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TImageButtonActionLink;
end;

procedure TCustomImageButton.MenuButtonClick;
begin
  Popup;
  if Assigned(FOnMenuButtonClick) then
    FOnMenuButtonClick(Self);
end;

procedure TCustomImageButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_RETURN then
  begin
    Click;
    SetCapture(0);
    Key := 0;
  end
  else if (Key = VK_SPACE) and (FPressedState <> psButtonDown) then
  begin
    SetCapture(0);
    FSpacePressed := True;
    FPressedState := psButtonDown;
    Repaint;
  end
  else if Key = VK_DOWN then
    Popup;
end;

procedure TCustomImageButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    if FPressedState = psButtonDown then
    begin
      MouseCapture := False;
      FSpacePressed := False;
      FPressedState := psNone;
      Repaint;
      Click;
    end;
end;

procedure TCustomImageButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Rect: TRect;
begin
  if not (csDesigning in ComponentState) and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    if boAutoFocus in FOptions then
      SetFocus;
    SetCapture(Handle);
    FMouseInControl := True;
    if Button = mbLeft then
    begin
      Rect := ClientRect;
      if [boMenu, boLocked] * FOptions = [boMenu] then
      begin
        with Rect do
          Right := Right - FLAT_MENUWIDTH -1;
        if PtInRect(Rect, Point(X, Y)) then
          FPressedState := psButtonDown
        else
          FPressedState := psMenuButtonDown;
      end
      else
        FPressedState := psButtonDown;
    end;
    Repaint;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomImageButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  PriorState: TPressedState;
begin
  if Button = mbLeft then
  begin
    PriorState := FPressedState;
    FPressedState := psNone;
    Repaint;
    if PtInRect(ClientRect, Point(X, Y)) then
      case PriorState of
        psButtonDown:
          Click;
        psMenuButtonDown:
          MenuButtonClick;
      end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure DrawWrapCaption(DC: HDC;  const Caption: string; Rect: TRect;
  Enabled: Boolean);
var
  DrawRect: TRect;
  PriorMode: Integer;
  PriorColor: COLORREF;
begin
  DrawRect := Rect;
  PriorMode := SetBkMode(DC, TRANSPARENT);
  if Enabled then
    DrawText(DC, PChar(Caption), -1, DrawRect, DT_NOCLIP or DT_CENTER or DT_WORDBREAK)
  else
  begin
    OffsetRect(DrawRect, 1, 1);
    PriorColor := SetTextColor(DC, GetSysColor(COLOR_BTNHIGHLIGHT));
    DrawText(DC, PChar(Caption), -1, DrawRect, DT_NOCLIP or DT_CENTER or DT_WORDBREAK);
    OffsetRect(DrawRect, -1, -1);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));
    DrawText(DC, PChar(Caption), -1, DrawRect, DT_NOCLIP or DT_CENTER or DT_WORDBREAK);
    SetTextColor(DC, PriorColor);
  end;
  SetBkMode(DC, PriorMode);
end;

{function StateToStr(State: TDrawState): string;
var
  I: TDrawStateItem;
begin
  Result := '';
  for I := Low(I) to High(I) do
    if I in State then
    begin
      if Result <> '' then
        Result := Result + ',';
      case I of
        dsDisabled: Result := Result + 'dsDisabled';
        dsPressed: Result := Result + 'dsPressed';
        dsSelected: Result := Result + 'dsSelected';
        dsHot: Result := Result + 'dsHot';
        dsFocused: Result := Result + 'dsFocused';
        dsChecked: Result := Result + 'dsChecked';
        dsExpanded: Result := Result + 'dsExpanded';
        dsDefaulted: Result := Result + 'dsDefaulted';
        dsThin: Result := Result + 'dsThin';
        dsFlat: Result := Result + 'dsFlat';
        dsBackground: Result := Result + 'dsBackground';
      end;
    end;
end;}

procedure TCustomImageButton.Paint;

  function Pressed(State: TPressedState): Boolean;
  begin
    Result := False; //(State = FPressedState) or ((FPressedState <> psNone) and (boLocked in FOptions));
  end;

  procedure DrawFrameButton(Rect: TRect; DrawState: TDrawState);
  var
    //FlatSpace: Integer;
    //Hot: Boolean;
    DC: HDC;
    F: HFONT;
    R: TRect;
    I: Integer;
  begin
    R := ClientRect;
    if FOptions * [boMenu, boWide] <> [] then
      Dec(R.Right, FLAT_SPACE + 2);
    if dsDisabled in DrawState then
        I := FImageDisabledIndex
    else if dsPressed in DrawState then
      I := FImagePressedIndex
    else if dsHot in DrawState then
      I := FImageHotIndex
    else
      I := FImageIndex;
    if I < 0 then
      I := FImageIndex;
    if not ThemePainter.Enabled and ([dsPressed, dsHot] * DrawState = [dsPressed, dsHot]) then
      OffsetRect(R, 1, 1);
    if FCaptionPosition = cpHidden then
    begin
      if FImages <> nil then
        ImageListDraw(FImages, Canvas, R.Left + (WidthOf(R) - FImages.Height) div 2,
          R.Top + (HeightOf(R) - FImages.Height) div 2, I, DrawState);
      Exit;
    end;
    if FImages <> nil then
      case FCaptionPosition of
        cpLeft:
          begin
            InflateRect(R, -2, 0);
            Dec(R.Right, FImages.Height + 2);
            ImageListDraw(FImages, Canvas, R.Right,
              R.Top + (HeightOf(R) - FImages.Height) div 2, I, DrawState);
          end;
        cpRight:
          begin
            InflateRect(R, -2, 0);
              ImageListDraw(FImages, Canvas, R.Left,
              R.Top + (HeightOf(R) - FImages.Height) div 2, I, DrawState);
            Inc(R.Left, FImages.Height + 2);
          end
      end;
      DC := Canvas.Handle;
      F := SelectObject(DC, Font.Handle);
      DrawCaption(DC, Caption, R, drCenter, Enabled);
      SelectObject(DC, F)

    (*if Caption <> '' then
      H := CalcMemoHeight(Canvas.Handle, Caption, WidthOf(R))
    else
      H := FontHeight(Canvas.Handle);
    if FImages <> nil then
    begin
      R.Top := R.Top + (R.Bottom - R.Top - FImages.Height) shr 1;
      case FCaptionPosition of
        cpLeft:
          begin
            R.Left := R.Right - FImages.Width - FlatSpace;
            if FOptions * [boMenu, boWide] <> [] then
              Dec(R.Left, FLAT_MENUWIDTH);
          end;
        cpTop:
          begin
            if FOptions * [boMenu, boWide] <> [] then
              R.Left := R.Left + (WidthOf(R) - FImages.Width - FLAT_MENUWIDTH) div 2
            else
              R.Left := R.Left + (WidthOf(R) - FImages.Width) div 2;
            R.Top := R.Top + H div 2;
          end;
        cpRight:
          begin
            R.Left := R.Left + FlatSpace;
          end;
        cpBottom:
          begin
            if FOptions * [boMenu, boWide] <> [] then
              R.Left := R.Left + (WidthOf(R) - FImages.Width - FLAT_MENUWIDTH) div 2
            else
              R.Left := R.Left + (WidthOf(R) - FImages.Width) div 2;
            R.Top := R.Top - H div 2;
          end;
      else
        R.Left := R.Left + (WidthOf(R) - FImages.Width) shr 1;
      end;
      //Hot := True;
      if dsDisabled in DrawState then
      begin
        I := FImageDisabledIndex;
        //Hot := I > -1;
      end
      else if dsPressed in DrawState then
        I := FImagePressedIndex
      else if dsHot in DrawState then
        I := FImageHotIndex
      else
      begin
        I := FImageIndex;
        //Hot := FImageHotIndex > -1;
      end;
      if I < 0 then
        I := FImageIndex;
      {if Enabled then
        Hot := Hot or (boOpaque in FOptions);}
      {if FOptions * [boMenu, boWide] <> [] then
        ImageListDraw(FImages, Canvas, R.Left - FLAT_SPACE + 2, R.Top, I, DrawState)
      else}
        ImageListDraw(FImages, Canvas, R.Left, R.Top, I, DrawState);
    end;
    if (Caption <> '') and (FCaptionPosition <> cpHidden) then
    begin
      if ImageWidth = 0 then
      begin
        R := ClientRect;
        R.Top := (HeightOf(R) - H) div 2;
      end
      else
      case CaptionPosition of
        cpLeft:
          begin
            R.Left := FLAT_SPACE;
            R.Right :=  Width - (FImages.Width + FLAT_SPACE * 2);
            if FOptions * [boMenu, boWide] <> [] then
              Dec(R.Right, FLAT_MENUWIDTH);
            H := CalcMemoHeight(Canvas.Handle, Caption, WidthOf(R));
            R.Bottom := Height;
            R.Top := (R.Bottom - H) div 2;
          end;
        cpRight:
          begin
            R.Left := FLAT_SPACE + R.Left + FImages.Width;
            R.Right := Width - FLAT_SPACE;
            if FOptions * [boMenu, boWide] <> [] then
              Dec(R.Right, FLAT_MENUWIDTH);
            H := CalcMemoHeight(Canvas.Handle, Caption, WidthOf(R));
            R.Bottom := Height;
            R.Top := (R.Bottom - H) div 2;
          end;
        cpBottom:
          begin
            R.Left := FLAT_SPACE;
            R.Right := Width - FLAT_SPACE;
            if FOptions * [boMenu, boWide] <> [] then
              Dec(R.Right, FLAT_MENUWIDTH);
            H := CalcMemoHeight(Canvas.Handle, Caption, WidthOf(R));
            R.Top := R.Top + FImages.Height;
            R.Bottom := Height;
            R.Top := R.Top + (HeightOf(R) - H) div 2;
          end;
        cpTop:
          begin
            R.Left := 0;
            R.Right := Width - FLAT_SPACE;
            if FOptions * [boMenu, boWide] <> [] then
              Dec(R.Right, FLAT_MENUWIDTH);
            H := CalcMemoHeight(Canvas.Handle, Caption, WidthOf(R));
            R.Bottom := R.Top;
            R.Top := 0;
            R.Top := R.Top + (HeightOf(R) - H) div 2;
          end;
      end;
      if FOptions * [boMenu, boWide] <> [] then
        Dec(R.Right, FLAT_SPACE);
      if not ThemePainter.Enabled then
        if [dsPressed] * DrawState = [dsPressed] then
          OffsetRect(R, 1, 1);
      DrawWrapCaption(Canvas.Handle, Caption, R, Enabled);
      if CaptionPosition = cpRight then
        OffsetRect(Rect, 3, 0);
    end;
    OffsetRect(Rect, 0, 1);
    if Focused and FocusedRect then
    begin
      Rect := ClientRect;
      if [boWide, boMenu] * FOptions <> [] then
        Dec(Rect.Right, FLAT_MENUWIDTH);
      DrawThemeButtonFocus(Canvas.Handle, Rect);
    end;*)
  end;

  procedure DrawFrameMenu(Rect: TRect; DrawState: TDrawFrameState);
  var
    BevelRect: TRect;
  begin
    if [boWide, boMenu] * FOptions = [boWide] then Exit;
    with ThemePainter do
      if Enabled then
      begin
        BevelRect := Rect;
        OffsetRect(BevelRect, -4, 0);
        InflateRect(BevelRect, 0, -3);
        Rect.Left := Rect.Left - 2;
        if boClean in FOptions then
        begin
          {Canvas.MoveTo(BevelRect.Left + 2, BevelRect.Top + 3);
          Canvas.LineTo(BevelRect.Left + 2, BevelRect.Bottom - 3);}
        end
        else
          DrawElement(Canvas.Handle, GetDetails(ttbSeparatorNormal), BevelRect);
        if not Enabled then
          DrawElement(Canvas.Handle, GetDetails(ttbSplitButtonDropDownDisabled), Rect)
        else if DrawState = dfPushed then
          DrawElement(Canvas.Handle, GetDetails(ttbSplitButtonDropDownPressed), Rect)
        else
          DrawElement(Canvas.Handle, GetDetails(ttbSplitButtonDropDownNormal), Rect);
      end
      else
      begin
        DrawFrame(Canvas.Handle, Rect, DrawState);
        {if Pressed(psButtonDown) then
          OffsetRect(Rect, 1, 1);}
        InflateRect(Rect, -1, 0);
        OffsetRect(Rect, -1, -1);
        if (FPressedState = psButtonDown) and (boLocked in FOptions) then
          OffsetRect(Rect, 1, 1);
        DrawArrow(Canvas.Handle, Rect, drDown, $000000, $FFFFFF, 15, Self.Enabled);
        InflateRect(Rect, 2, 0);
        OffsetRect(Rect, 0, 1);
        if (not (boClean in FOptions)) and((FPressedState <> psMenuButtonDown) or
          (boLocked in FOptions)) then
        begin
          BevelRect := Rect;
          InflateRect(BevelRect, -3, -3);
          Dec(BevelRect.Left, 4);
          BevelRect.Right := BevelRect.Left + 2;
          DrawFrame(Canvas.Handle, BevelRect, dfLowered);
        end;
      end;
  end;

var
  DC: HDC;
  Rect: TRect;
  ButtonRect: TRect;
  MenuRect: TRect;
  DrawState: TDrawState;
begin
  inherited Paint;
  DC := Canvas.Handle;
  Rect := ClientRect;
  if Transparent then
  begin
    MoveWindowOrg(DC, -Left, -Top);
    SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, Canvas.Handle);
    SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, Canvas.Handle);
    MoveWindowOrg(DC, Left, Top);
  end;
  if not ThemePainter.Enabled then
    FillRectColor(Canvas.Handle, Rect, Color);
  DrawState := [dsBackground];
  if FDown then
  begin
    Include(DrawState, dsPressed);
    Include(DrawState, dsHot);
  end;
  if not Enabled then
    Include(DrawState, dsDisabled);
  if Focused then
    Include(DrawState, dsFocused);
  if FSpacePressed then
    Include(DrawState, dsPressed)
  else if FDown or ((FPressedState = psButtonDown) and FMouseInControl) then
    Include(DrawState, dsPressed);
  if FMouseInControl or FSpacePressed then
    Include(DrawState, dsHot);
  if Focused and (not FFocusedRect) and (not FMouseInControl) then
    Include(DrawState, dsHot);
  if FStyle = bsBeveled then
    Include(DrawState, dsFlat);
  if CustomDraw(Rect, DrawState) then
    case FStyle of
      bsFramed: DrawThemeButton(Canvas.Handle, Rect, DrawState);
      bsBeveled: DrawThemeThinButton(Canvas.Handle, Rect, DrawState + [dsHot]);
      bsFlat: DrawThemeThinButton(Canvas.Handle, Rect, DrawState);
    end;
  Exclude(DrawState, dsBackground);
  ButtonRect := Rect;
  if FOptions * [boMenu, boWide] <> [] then
    Dec(ButtonRect.Right, FLAT_MENUWIDTH + 2);
  if (FImagePressedIndex < 0) and ((FPressedState = psButtonDown) or
    ((FPressedState = psMenuButtonDown) and (boLocked in FOptions))) then
    OffsetRect(ButtonRect, 1, 1);
  if CustomDraw(Rect, DrawState) then
    DrawFrameButton(Rect, DrawState);
  Rect := ClientRect;
  if FOptions * [boWide, boMenu] <> []  then
  begin
    MenuRect := Rect;
    with MenuRect do
      Left := Right - FLAT_MENUWIDTH;
    if (FPressedState = psMenuButtonDown) and (not (boLocked in FOptions)) then
      DrawFrameMenu(MenuRect, dfPushed)
    else
      DrawFrameMenu(MenuRect, dfFlat)
  end;
end;

procedure TCustomImageButton.Repaint;
begin
  if HandleAllocated then inherited Repaint;
end;

procedure TCustomImageButton.Popup;
var
  Handled: Boolean;
begin
  if PopupMenu <> nil then
  begin
    Handled := False;
    if Assigned(OnContextPopup) then
      OnContextPopup(Self, Mouse.CursorPos, Handled);
    if not Handled then
    begin
      FPressedState := psNone;
      FDown := False;
      FMouseInControl := False;
      with ClientToScreen(Classes.Point(-1, Height + 1)) do
        PopupMenu.Popup(X, Y);
      PostMessage(Handle, WM_CANCELMODE, 0, 0);
    end;
  end;
end;

procedure TCustomImageButton.SetCaptionPosition(Value: TCaptionPosition);
begin
  if Value <> FCaptionPosition then
  begin
    FCaptionPosition := Value;
    Realign;
    Repaint;
  end;
end;

procedure TCustomImageButton.SetFocusedRect(Value: Boolean);
begin
  if Value <> FFocusedRect then
  begin
    FFocusedRect := Value;
    Repaint;
  end;
end;

procedure TCustomImageButton.SetDown(Value: Boolean);
var
  A: TControl;
  B: TCustomImageButton absolute A;
  I: Integer;
begin
  if Value <> FDown then
  begin
    FDown := Value;
    if FDown and (boGrouped in FOptions) and (Parent <> nil) then
      for I := 0 to Parent.ControlCount - 1 do
      begin
        A := Parent.Controls[I];
        if A = Self then
          Continue;
        if (A is TCustomImageButton) and (boGrouped in B.FOptions) then
          B.Down := False;
      end;
    Repaint;
  end;
end;

procedure TCustomImageButton.SetImageIndex(Index: Integer; Value: Integer);
var
  I: PInteger;
begin
  I := @FImageIndex;
  Inc(I, Index);
  if Value <> I^ then
  begin
    I^ := Value;
    Repaint;
  end;
end;

procedure TCustomImageButton.SetImages(Value: TCustomImageList);
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
    if (Parent = nil) or (not HandleAllocated)then Exit;
    AdjustSize;
    Repaint;
  end;
end;

procedure TCustomImageButton.SetOptions(Value: TButtonOptions);
begin
  if Value <> FOptions then
  begin
    FOptions := Value;
    {if boAutoFocus in FOptions then
      TabStop := False;}
    if (not (boToggle in FOptions)) and FDown then
      FDown := False;
    AdjustSize;
    // Realign ?
    Repaint;
  end;
end;

function TCustomImageButton.GetImageWidth: Integer;
begin
  Result := 0;
  if FImages <> nil then
    Result := FImages.Width;
end;

procedure TCustomImageButton.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Realign;
    Repaint;
  end;
end;

function TCustomImageButton.GetTransparent: Boolean;
begin
  Result := csParentBackground in ControlStyle;
end;

procedure TCustomImageButton.SetTransparent(Value: Boolean);
begin
  if Value <> Transparent then
  begin
    if Value then
      ControlStyle := ControlStyle + [csParentBackground]
    else
      ControlStyle := ControlStyle - [csParentBackground];
    Repaint;
  end;
end;

procedure TCustomImageButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCustomImageButton.CMEnabledChanged(var Message: TMessage);
begin
  Repaint;
  inherited;
end;

procedure TCustomImageButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  //FMouseInControl := Message.Sender = Self;
  FPressedState := psNone;
  Repaint;
  inherited;
end;

procedure TCustomImageButton.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  Realign;
  Repaint;
  inherited;
end;

procedure TCustomImageButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;
  Repaint;
end;

procedure TCustomImageButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;
  Repaint;
end;

procedure TCustomImageButton.CMTabStopChanged(var Message: TMessage);
begin
  {if not (boAutoFocus in FOptions) then
    TabStop := False;}
  inherited;
end;

procedure TCustomImageButton.CMTextChanged(var Message: TMessage);
begin
  Realign;
  Repaint;
  inherited;
end;

procedure TCustomImageButton.WMCancelMode(var Message: TWMCancelMode);
begin
  Invalidate;
  inherited;
end;

procedure TCustomImageButton.WMKillFocus(var Message: TWMKillFocus);
begin
  MouseCapture := False;
  FPressedState := psNone;
  Repaint;
  inherited;
end;

procedure TCustomImageButton.WMSetFocus(var Message: TWMSetFocus);
begin
  MouseCapture := False;
  FPressedState := psNone;
  Repaint;
  inherited;
end;

procedure TCustomImageButton.WMWindowPosChanging(var Msg: TWMWindowPosChanging);
begin
  Msg.Result := 0;
end;

procedure TCustomImageButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if Transparent and DoubleBuffered then
  begin
    Message.Result := 1;
    Exit;
  end
  else
    inherited;
end;

{ TImageButtonAcionLink }

procedure TImageButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomImageButton;
end;

function TImageButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

procedure TImageButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
  begin
    FClient.ImageIndex := Value;
    if Action is TCustomAction then
      FClient.Images := TCustomAction(Action).ActionList.Images;;
  end;
end;


{ TPopButton }

{$IFNDEF LITE}

constructor TPopButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CaptionPosition := cpHidden;
  Options := Options + [boMenu, boLocked] - [boAutoSize];
  FPopupForm := CreatePopup;
  with FPopupForm do
  begin
    Associate := Self;
    OnSelect := PopupFormSelect;
  end;
end;

procedure TPopButton.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPopButton.Click;
begin
  inherited Click;
  Popup;
end;

procedure TPopButton.MenuButtonClick;
begin
  Click;
end;

procedure TPopButton.Popup;
begin
  FPopupForm.Popup;
end;

procedure TPopButton.PopupFormSelect(Sender: TObject);
begin
  Repaint;
  Change;
end;

procedure TCustomImageButton.SetPadding(const Value: Integer);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    AdjustSize;
  end;
end;

{ TCheckListButton }

constructor TCheckListButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := Options - [boLocked];
  FPopupCheckList := PopupForm as TPopupCheckListForm;
  with FPopupCheckList.CheckList do
  begin
    OnClick := DoClickItem;
    OnClickCheck := DoClickCheck;
  end;
end;

procedure TCheckListButton.DoClickCheck(Sender: TObject);
begin
  if Assigned(FOnClickCheck) then
    FOnClickCheck(Self);
end;

procedure TCheckListButton.DoClickItem(Sender: TObject);
begin
  if Assigned(FOnClickItem) then
    FOnClickItem(Self);
end;

function TCheckListButton.CreatePopup: TCustomPopupForm;
begin
  Result := TPopupCheckListForm.Create(Self);
end;

procedure TCheckListButton.Click;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

procedure TCheckListButton.MenuButtonClick;
begin
  Popup;
end;

procedure TCheckListButton.Resize;
begin
  inherited Resize;
  FPopupCheckList.Width := Width;
end;

function TCheckListButton.GetChecked(Index: Integer): Boolean;
begin
  Result := FPopupCheckList.CheckList.Checked[Index];
end;

procedure TCheckListButton.SetChecked(Index: Integer; Value: Boolean);
begin
  FPopupCheckList.CheckList.Checked[Index] := Value;
end;

function TCheckListButton.GetCheckText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Items.Count - 1 do
    if Checked[I] then
      Result := Result + Items[I] + ', ';
  if Result <> '' then
    SetLength(Result, Length(Result) - 2);
end;

function TCheckListButton.GetFlat: Boolean;
begin
  Result := FPopupCheckList.CheckList.Flat;
end;

procedure TCheckListButton.SetFlat(Value: Boolean);
begin
  FPopupCheckList.CheckList.Flat := Value;
end;

function TCheckListButton.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := FPopupCheckList.CheckList.ItemEnabled[Index];
end;

procedure TCheckListButton.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  FPopupCheckList.CheckList.ItemEnabled[Index] := Value;
end;

function TCheckListButton.GetItemIndex: Integer;
begin
  Result := FPopupCheckList.CheckList.ItemIndex;
end;

procedure TCheckListButton.SetItemIndex(Value: Integer);
begin
  FPopupCheckList.CheckList.ItemIndex := Value;
end;

function TCheckListButton.GetItems: TStrings;
begin
  Result := FPopupCheckList.CheckList.Items;
end;

procedure TCheckListButton.SetItems(Value: TStrings);
begin
  FPopupCheckList.CheckList.Items := Value;
end;

function TCheckListButton.GetSizeable: Boolean;
begin
  Result := FPopupCheckList.Sizeable;
end;

procedure TCheckListButton.SetSizeable(Value: Boolean);
begin
  FPopupCheckList.Sizeable := Value;
end;

function TCheckListButton.GetState(Index: Integer): TCheckBoxState;
begin
  Result := FPopupCheckList.CheckList.State[Index];
end;

procedure TCheckListButton.SetState(Index: Integer; Value: TCheckBoxState);
begin
  FPopupCheckList.CheckList.State[Index] := Value;
end;

function TCheckListButton.GetStatusText: string;
begin
  Result := FPopupCheckList.StatusText;
end;

procedure TCheckListButton.SetStatusText(Value: string);
begin
  FPopupCheckList.StatusText := Value;
end;

procedure TCheckListButton.SetPopupHeight(Value: Integer);
begin
  FPopupHeight := Value;
  if FPopupCheckList <> nil then
    FPopupCheckList.ClientHeight := Value;
end;

{ TColorGridButton }

function TColorGridButton.CreatePopup: TCustomPopupForm;
begin
  Result := TPopupColorGridForm.Create(Self);
end;

function TColorGridButton.CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean;
var
  DC: HDC;
begin
  Result := True;
  DC := Canvas.Handle;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  InflateRect(Rect, -5, -5);
  if FOptions * [boMenu, boWide] <> [] then
    Dec(Rect.Right, FLAT_MENUWIDTH - 1);
  FillRectColor(DC, Rect, clWindowText);
  InflateRect(Rect, -1, -1);
  FillRectColor(DC, Rect, ActiveColor);
end;

function TColorGridButton.GetActiveColor: TColor;
begin
  Result := (PopupForm as TPopupColorGridForm).ActiveColor;
end;

procedure TColorGridButton.SetActiveColor(Value: TColor);
begin
  (PopupForm as TPopupColorGridForm).ActiveColor := Value;
end;

procedure TColorGridButton.CNColorPick(var Msg: TMessage);
begin
  ActiveColor := Msg.WParam;
  Msg.Result := 0;
end;

{ TBrushButton }

function TBrushButton.CreatePopup: TCustomPopupForm;
begin
  Result := TPopupBrushForm.Create(Self);
end;

function TBrushButton.CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean;
var
  DC: HDC;
  DrawPen: HPEN;
  DrawBrush: HBRUSH;
begin
  Result := True;
  DC := Canvas.Handle;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  InflateRect(Rect, -5, -5);
  if FOptions * [boMenu, boWide] <> [] then
    Dec(Rect.Right, FLAT_MENUWIDTH - 1);
  DrawBrush := GetStockObject(NULL_BRUSH);
  case (PopupForm as TPopupBrushForm).BrushStyle of
    bsSolid: DrawBrush := CreateSolidBrush(0);
    bsClear: DrawBrush := GetStockObject(NULL_BRUSH);
    bsHorizontal: DrawBrush := CreateHatchBrush(HS_HORIZONTAL, 0);
    bsVertical: DrawBrush := CreateHatchBrush(HS_VERTICAL, 0);
    bsFDiagonal: DrawBrush := CreateHatchBrush(HS_FDIAGONAL, 0);
    bsBDiagonal: DrawBrush := CreateHatchBrush(HS_BDIAGONAL, 0);
    bsCross: DrawBrush := CreateHatchBrush(HS_CROSS, 0);
    bsDiagCross: DrawBrush := CreateHatchBrush(HS_DIAGCROSS, 0);
  end;
  FillRect(DC, Rect, DrawBrush);
  if (PopupForm as TPopupBrushForm).BrushStyle <> bsClear then
    DeleteObject(DrawBrush);
  DrawPen := SelectObject(DC, GetStockObject(BLACK_PEN));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, nil);
    LineTo(DC, Right - 1, Top);
    LineTo(DC, Right - 1, Bottom - 1);
    LineTo(DC, Left, Bottom - 1);
    LineTo(DC, Left, Top);
  end;
  SelectObject(DC, DrawPen);
end;

function TBrushButton.GetBrushStyle: TBrushStyle;
begin
  Result := (PopupForm as TPopupBrushForm).BrushStyle;
end;

procedure TBrushButton.SetBrushStyle(Value: TBrushStyle);
begin
  (PopupForm as TPopupBrushForm).BrushStyle := Value;
end;

function TBrushButton.GetMode: TListMode;
begin
  Result := (PopupForm as TPopupBrushForm).Mode;
end;

procedure TBrushButton.SetMode(Value: TListMode);
begin
  (PopupForm as TPopupBrushForm).Mode := Value;
end;

{ TPenButton }

function TPenButton.CreatePopup: TCustomPopupForm;
begin
  Result := TPopupPenForm.Create(Self);
end;

function TPenButton.CustomDraw(Rect: TRect; DrawState: TDrawState): Boolean;
var
  DC: HDC;
  DrawPen: HPEN;
begin
  Result := True;
  DC := Canvas.Handle;
  Dec(Rect.Right);
  Dec(Rect.Bottom);
  InflateRect(Rect, -5, -5);
  if FOptions * [boMenu, boWide] <> [] then
    Dec(Rect.Right, FLAT_MENUWIDTH - 1);
  DrawPen := 0;
  case (PopupForm as TPopupPenForm).PenStyle of
    psSolid: DrawPen := CreatePen(PS_SOLID, 1, 0);
    psDash: DrawPen := CreatePen(PS_DASH, 1, 0);
    psDot: DrawPen := CreatePen(PS_DOT, 1, 0);
    psDashDot: DrawPen := CreatePen(PS_DASHDOT, 1, 0);
    psDashDotDot: DrawPen := CreatePen(PS_DASHDOTDOT, 1, 0);
    psClear: DrawPen := CreatePen(PS_NULL, 1, 0);
    psInsideFrame: DrawPen := CreatePen(PS_INSIDEFRAME, 1, 0);
  end;
  DrawPen := SelectObject(DC, DrawPen);
  with Rect do
  begin
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2 - 1, nil);
    LineTo(DC, Left, Top + (Bottom - Top) div 2 - 1);
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2, nil);
    LineTo(DC, Right, Top + (Bottom - Top) div 2);
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2 + 1, nil);
    LineTo(DC, Right, Top + (Bottom - Top) div 2 + 1);
  end;
  OverwriteObject(DC, DrawPen);
end;

function TPenButton.GetPenStyle: TPenStyle;
begin
  Result := (PopupForm as TPopupPenForm).PenStyle;
end;

procedure TPenButton.SetPenStyle(Value: TPenStyle);
begin
  (PopupForm as TPopupPenForm).PenStyle := Value;
end;

function TPenButton.GetMode: TListMode;
begin
  Result := (PopupForm as TPopupPenForm).Mode;
end;

procedure TPenButton.SetMode(Value: TListMode);
begin
  (PopupForm as TPopupPenForm).Mode := Value;
end;

{$ENDIF}

{ TThemeGlyphButton }

constructor TThemeGlyphButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csFixedWidth, csFixedHeight];
  Color := clWindowFrame;
  Width := 22;
  Height := 22;
end;

destructor TThemeGlyphButton.Destroy;
begin
  inherited Destroy;
end;

procedure TThemeGlyphButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FDown := True;
    Repaint;
  end;
end;

procedure TThemeGlyphButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDown := False;
    Repaint;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TThemeGlyphButton.Paint;
var
  DC: HDC;
  Rect: TRect;
  State: TDrawState;
begin
  inherited Paint;
  DC := Canvas.Handle;
  Rect := ClientRect;
  if not Enabled then
    State := [dsDisabled]
  else if FDown and FMouseInControl then
    State := [dsPressed]
  else if FMouseInControl then
    State := [dsHot]
  else
    State := [];
  if FStyle = bsBeveled then
    State := State + [dsThin]
  else if FStyle = bsFlat then
    State := State + [dsFlat];
  case FKind of
    tgToolClose: DrawThemeToolClose(DC, Rect, State, Color);
    tgClose: DrawThemeClose(DC, Rect, State, Color);
    tgPin: DrawThemePin(DC, Rect, State, Color);
  end;
end;

procedure TThemeGlyphButton.SetKind(Value: TThemeGlyphKind);
begin
  if Value <> FKind then
  begin
    FKind := Value;
    Repaint;
  end;
end;

procedure TThemeGlyphButton.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Repaint;
  end;
end;

procedure TThemeGlyphButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FDown := False;
  FMouseInControl := False;
  Repaint;
end;

procedure TThemeGlyphButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;
  Repaint;
end;

procedure TThemeGlyphButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseInControl := False;
  Repaint;
end;

procedure TThemeGlyphButton.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  Repaint;
end;

end.
