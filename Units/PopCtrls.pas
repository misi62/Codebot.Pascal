
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit PopCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ComCtrls,
  Dialogs, BaseTypes, GraphTools, ExtCtrls, StdCtrls, Math, CheckLst,
  StrTools, ScrollCtrls, WinTools, BlendTools, ProviderTools
  {$IFDEF GDIPLUS}, GdiPlus, GdiIntf{$ENDIF};

{ TCustomPopupForm class }

type
  TCaptureKey = (ckNone, ckUp, ckDown, ckLeft, ckRight, ckPrior, ckNext,
    ckReturn, ckEscape, ckTab);

  TCaptureKeys = set of TCaptureKey;

  TTrackSize = record
    Min: TPoint;
    Max: TPoint;
  end;
  PTrackSize = ^TTrackSize;

  TCustomPopupForm = class(TCustomForm)
  private
    FAssociate: TWinControl;
    FAssociateFrame: TFrame;
    FAssociateFrameWidth: Integer;
    FCaptureKeys: TCaptureKeys;
    FCustomFrame: Boolean;
    FCustomFrameColor: TColor;
    FDefAssociateWindowProc: TWndMethod;
    FForwardControl: TWinControl;
    FForwardFocus: Boolean;
    FHorzOffset: Integer;
    FSendKeys: Boolean;
    FSizeable: Boolean;
    FStatusText: string;
    FShadow: THotTracker;
    FUseFade: Boolean;
    FUseShadow: Boolean;
    FTimer: TPerformanceTimer;
    FOnCancel: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    procedure AssociateWindowProc(var Message: TMessage);
    procedure SetAssociate(Value: TWinControl);
    procedure SetAssociateFrame(Value: TFrame);
    procedure SetForwardControl(Value: TWinControl);
    procedure SetSizeable(Value: Boolean);
    procedure SetStatusText(const Value: string);
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure AdjustControlSize; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure ForwardChange(NewControl: TWinControl); dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure QueryTrackSize(var Size: TTrackSize); virtual;
    procedure Select; virtual;
    procedure WndProc(var Message: TMessage); override;
    property CaptureKeys: TCaptureKeys read FCaptureKeys write FCaptureKeys;
    property ForwardControl: TWinControl read FForwardControl write
      SetForwardControl;
    property StatusText: string read FStatusText write SetStatusText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Popup; dynamic;
    procedure Cancel; virtual;
    property Associate: TWinControl read FAssociate write SetAssociate;
    property AssociateFrame: TFrame read FAssociateFrame write SetAssociateFrame;
    property ForwardFocus: Boolean read FForwardFocus write FForwardFocus default True;
    property HorzOffset: Integer read FHorzOffset write FHorzOffset;
    property SendKeys: Boolean read FSendKeys write FSendKeys;
    property Sizeable: Boolean read FSizeable write SetSizeable default True;
    property UseFade: Boolean read FUseFade write FUseFade;
    property UseShadow: Boolean read FUseShadow write FUseShadow;
    property CustomFrame: Boolean read FCustomFrame write FCustomFrame;
    property CustomFrameColor: TColor read FCustomFrameColor write FCustomFrameColor;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Color;
    property Visible;
  end;

  TPopupFormClass = class of TCustomPopupForm;

{ TPopupColorsForm }

function ColorToStr(Color: TColor): string;
function StrToColor(const Ident: string): TColor;
function StrToColorDef(const Ident: string; const Default: TColor): TColor;

type
  TColorCell = record
    Color: TColor;
    Rect: TRect;
  end;

  TColorGrid = array[0..19] of TColorCell;

  TPopupColorGridForm = class(TCustomPopupForm)
  private
    FActiveColor: TColor;
    FButtonRect: TRect;
    FColorGrid: TColorGrid;
    FColorRect: TRect;
    FHighlightIndex: Integer;
    procedure SetActiveColor(Value: TColor);
  protected
    function ColorIndexFromPoint(const Point: TPoint): Integer;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintItem(Index: Integer);
    procedure Popup; override;
    property ActiveColor: TColor read FActiveColor write SetActiveColor;
  end;

{ TPopupTree class }

  TPopupTree = class(TCustomTreeView)
  private
    procedure WMNCCalcSize(var Message: TMessage); message WM_NCCALCSIZE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Images;
    property Items;
    property Selected;
    property ShowLines;
    property ShowRoot;
    property TopItem;
  end;

{ TPopupTreeForm class }

  TPopupTreeForm = class(TCustomPopupForm)
  private
    FTree: TPopupTree;
    procedure TreeClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property StatusText;
    property Tree: TPopupTree read FTree;
  end;

 { TPopupImageListForm }

  TPopupImageListForm = class(TCustomPopupForm)
  private
    FImageDrawList: TImageDrawList;
  public
    constructor Create(AOwner: TComponent); override;
    property ImageDrawList: TImageDrawList read FImageDrawList;
  end;

{ TPopupCheckListForm }

  TPopupCheckListForm = class(TCustomPopupForm)
  private
    FCheckList: TCheckListBox;
  public
    constructor Create(AOwner: TComponent); override;
    property StatusText;
    property CheckList: TCheckListBox read FCheckList;
  end;

{ TPopupDateForm class }

  TPopupDateForm = class(TCustomPopupForm)
  private
    FCalendar: TMonthCalendar;
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    procedure CalendarClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
    property Date: TDateTime read GetDate write SetDate;
  end;

{ TCustomPopupListForm class }

  TListMode = (lmBrief, lmVerbose);

  TCustomPopupListForm = class(TCustomPopupForm)
  private
    FListBox: TListBox;
    FMode: TListMode;
    FDisplayColumn: Integer;
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
    procedure ListDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  protected
    function GetVerboseWidth: Integer; virtual;
    property ListBox: TListBox read FListBox;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Mode: TListMode read FMode write FMode;
    property VerboseWidth: Integer read GetVerboseWidth;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
    property DisplayColumn: Integer read FDisplayColumn write FDisplayColumn;
  end;

{ TPopupListForm }

  TPopupListForm = class(TCustomPopupListForm)
  public
    property Items;
    property ItemIndex;
  end;

{ TPopupBrushForm }

function BrushStyleToStr(BrushStyle: TBrushStyle): string;
function StrToBrushStyle(const Ident: string): TBrushStyle;
function StrToBrushStyleDef(const Ident: string; const Default: TBrushStyle): TBrushStyle;

type
  TPopupBrushForm = class(TCustomPopupListForm)
  private
    FBrushStyle: TBrushStyle;
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    function GetBrush: TBrush;
    procedure SetBrush(Value: TBrush);
    procedure SetBrushStyle(Value: TBrushStyle);
  protected
    procedure Select; override;
    function GetVerboseWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Brush: TBrush read GetBrush write SetBrush;
    property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle;
    property Mode;
  end;

{ TPopupPenForm }

function PenStyleToStr(PenStyle: TPenStyle): string;
function StrToPenStyle(const Ident: string): TPenStyle;
function StrToPenStyleDef(const Ident: string; const Default: TPenStyle): TPenStyle;

type
  TPopupPenForm = class(TCustomPopupListForm)
  private
    FPenStyle: TPenStyle;
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    function GetPen: TPen;
    procedure SetPen(Value: TPen);
    procedure SetPenStyle(Value: TPenStyle);
  protected
    procedure Select; override;
    function GetVerboseWidth: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Pen: TPen read GetPen write SetPen;
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle;
    property Mode;
  end;


{ TScrollingMenuItem }

type
  TScrollingMenuItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FDefaulted: Boolean;
    FImageIndex: Integer;
    procedure SetCaption(Value: TCaption);
    procedure SetImageIndex(Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Defaulted: Boolean read FDefaulted write FDefaulted;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

{ TScrollingMenuItems }

  TScrollingMenuItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TScrollingMenuItem;
    procedure Put(Index: Integer; Value: TScrollingMenuItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TScrollingMenuItem;
    function FindItemID(ID: Integer): TScrollingMenuItem;
    function Insert(Index: Integer): TScrollingMenuItem;
    property Items[Index: Integer]: TScrollingMenuItem read Get write Put; default;
  end;

{ TScrollingMenu }

  TScrollingMenu = class(TCustomDrawList)
  private
    FItems: TScrollingMenuItems;
    procedure ItemsChanged;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; var Rect: TRect; State: TDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnClick;
  published
    property Items: TScrollingMenuItems read FItems;
    property Images;
    property OnDrawBackground;
    property OnDrawItem;
  end;

{ TPopupScrollingMenu }

  TPopupScrollingMenu = class(TCustomPopupForm)
  private
    FDisplayCount: Integer;
    FMenu: TScrollingMenu;
    FMenuIndex: Integer;
    FMinWidth: Integer;
    procedure MenuClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
  published
    property DisplayCount: Integer read FDisplayCount write FDisplayCount;
    property MinWidth: Integer read FMinWidth write FMinWidth;
    property Menu: TScrollingMenu read FMenu;
    property MenuIndex: Integer read FMenuIndex;
    property Associate;
    property AssociateFrame;
    property OnCancel;
    property OnSelect;
  end;


implementation

uses
  StrConst;

function KeyToCaptureKey(Key: Word): TCaptureKey;
begin
  case Key of
    VK_UP:
      Result := ckUp;
    VK_DOWN:
      Result := ckDown;
    VK_LEFT:
      Result := ckLeft;
    VK_RIGHT:
      Result := ckRight;
    VK_PRIOR:
      Result := ckPrior;
    VK_NEXT:
      Result := ckNext;
    VK_RETURN:
      Result := ckReturn;
    VK_ESCAPE:
      Result := ckEscape;
    VK_TAB:
      Result := ckTab;
  else
    Result := ckNone;
  end;
end;

{ TCustomPopupForm }

constructor TCustomPopupForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Align := alNone;
  BorderStyle := bsNone;
  DesktopFont := True;
  Color := clWindow;
  Visible := False;
  ParentColor := False;
  ParentFont := False;
  Ctl3D := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  WindowState := wsNormal;
  DefaultMonitor := dmActiveForm;
  FCaptureKeys := [ckUp, ckDown, ckReturn, ckEscape];
  FTimer := TPerformanceTimer.Create;
  FUseFade := True;
  FUseShadow := True;
  FForwardFocus := True;
  FSizeable := False;
  FShadow := THotTracker.Create;
  FShadow.StandardBlur := True;
  FShadow.Border := 2;
  FShadow.Clipped := True;
  FShadow.Opacity := High(Byte) div 2;
  FShadow.X := 3;
  FShadow.Y := 3;
end;

destructor TCustomPopupForm.Destroy;
begin
  FTimer.Free;
  if FAssociate <> nil then
  begin
    FAssociate.RemoveFreeNotification(Self);
    FAssociate.WindowProc := FDefAssociateWindowProc;
  end;
  FShadow.Free;
  inherited Destroy;
end;

procedure TCustomPopupForm.AdjustControlSize;
var
  W, H: Integer;
begin
  W := ClientWidth;
  H := ClientHeight;
  if FAssociateFrame <> nil then
    H := H - FAssociateFrame.Height;
  if FForwardControl <> nil then
    with FForwardControl do
    begin
      Parent := Self;
      BoundsRect := Rect(0, 0, W, H);
      if (FAssociateFrame = nil) and FSizeable then
        Height := Height - GetSystemMetrics(SM_CYHSCROLL);
      Anchors := [akLeft, akTop, akRight, akBottom];
    end;
end;

procedure TCustomPopupForm.Cancel;
begin
  Hide;
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TCustomPopupForm.CreateParams(var Params: TCreateParams);
const
  ExStyle = WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  Style = WS_BORDER or WS_CHILD or WS_CLIPCHILDREN;
begin
  inherited CreateParams(Params);
  Params.ExStyle := ExStyle;
  Params.Style := Style;
  Params.WndParent := GetDesktopWindow;
end;

procedure TCustomPopupForm.AssociateWindowProc(var Message: TMessage);
begin
  with Message do
    if IsWindowVisible(Handle) then
      case Msg of
        WM_CANCELMODE, CM_CANCELMODE:
          begin
            Cancel;
            FDefAssociateWindowProc(Message);
          end;
        WM_CHAR:
          begin
            if FSendKeys and (FForwardControl <> nil) then
              SendMessage(FForwardControl.Handle, Msg, WParam, LParam);
            FDefAssociateWindowProc(Message);
          end;
        WM_KEYDOWN:
          case WParam of
            VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_SPACE:
              begin
                if FForwardControl <> nil then
                begin
                  SendMessage(FForwardControl.Handle, Msg, WParam, LParam);
                  if KeyToCaptureKey(LongRec(WParam).Lo) in FCaptureKeys then
                    WParam := 0;
                end;
                Result := 0;
              end;
            VK_RETURN:
              begin
                Select;
                Result := 0;
              end;
            VK_ESCAPE:
              begin
                Cancel;
                Result := 0;
              end;
            else
              FDefAssociateWindowProc(Message);
            end;
        WM_KILLFOCUS:
          begin
            Hide;
            FDefAssociateWindowProc(Message);
          end;
        WM_MOUSEWHEEL:
          begin
            if FForwardControl <> nil then
              PostMessage(FForwardControl.Handle, Msg, WParam, LParam);
            Result := 0;
          end;
      else
        FDefAssociateWindowProc(Message);
      end
    else
      FDefAssociateWindowProc(Message);
end;

procedure TCustomPopupForm.ForwardChange(NewControl: TWinControl);
begin
  if FForwardFocus then
  begin
    if (FForwardControl <> nil) and (FForwardControl.HandleAllocated) then
      SendMessage(FForwardControl.Handle, WM_KILLFOCUS, 0, 0);
    if (NewControl <> nil) and (NewControl.HandleAllocated) then
      SendMessage(NewControl.Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TCustomPopupForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FAssociate) then
  begin
    Hide;
    FAssociate.WindowProc := FDefAssociateWindowProc;
    FAssociate := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

procedure TCustomPopupForm.Paint;
var
  DC: HDC;
  Rect: TRect;
  Brush: HBRUSH;
begin
  if FSizeable then
  begin
    DC := Canvas.Handle;
    Windows.GetClientRect(Handle, Rect);
    Brush := CreateSolidBrush(ColorToRGB(Color));
    Dec(Rect.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    FillRect(DC, Rect, Brush);
    Rect.Top := Rect.Bottom;
    Inc(Rect.Bottom, GetSystemMetrics(SM_CYHSCROLL));
    DrawThemeStatus(DC, FStatusText, Rect);
    DeleteObject(Brush);
  end
  else
  begin
    Windows.GetClientRect(Handle, Rect);
    FillRectColor(Canvas.Handle, Rect, Color);
  end;
end;

procedure TCustomPopupForm.QueryTrackSize(var Size: TTrackSize);
begin
  Size.Min.x := GetSystemMetrics(SM_CXVSCROLL) + 2;
  Size.Min.y := 75;
end;

procedure TCustomPopupForm.WMTimer(var Message: TWMTimer);
const
  FadeDuration = 200;
var
  Delta: Int64;
begin
  inherited;
  if Message.TimerID = 10 then
  begin
    Delta :=  FTimer.ElapsedTime;
    if Delta > FadeDuration then
    begin
      if FUseShadow then
      begin
        FShadow.Opacity := High(Byte);
        FShadow.Update;
      end;
      AlphaBlend := False;
      AlphaBlendValue := $FF;
      KillTimer(Handle, 10);
      FTimer.Stop;
    end
    else
    begin
      AlphaBlendValue := Round((FTimer.ElapsedTime / FadeDuration) * $FF);
      if FUseShadow then
      begin
        FShadow.Opacity := AlphaBlendValue;
        FShadow.Update;
      end;
    end;
  end;
end;

procedure TCustomPopupForm.Popup;
var
  R: TRect;
  P: TPoint;
begin
  if FAssociate = nil then Exit;
  with FAssociate do
  begin
    P := Parent.ClientToScreen(BoundsRect.TopLeft);
    R := Rect(P.X + FHorzOffset, P.Y + Height + 1, P.X + Self.Width + FHorzOffset,
      P.Y + Height + 1 + Self.Height);
  end;
  if Screen.MonitorCount < 2 then
  begin
    if R.Right > Screen.Width then
      OffsetRect(R, Screen.Width - R.Right, 0);
    if R.Bottom > Screen.Height then
      OffsetRect(R, 0, -HeightOf(R) - FAssociate.Height - 2);
  end;
  BoundsRect := R;
  if FUseFade then
  begin
    AlphaBlendValue := 0;
    AlphaBlend := True;
    if FUseShadow then
      FShadow.Opacity := 0;
    FTimer.Stop;
    FTimer.Start;
    SetTimer(Handle, 10, 10, nil);
  end;
  Show;
  if (FForwardControl <> nil) and FForwardFocus then
    SendMessage(FForwardControl.Handle, WM_SETFOCUS, 0, 0);
end;

procedure TCustomPopupForm.Select;
begin
  Hide;
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TCustomPopupForm.WndProc(var Message: TMessage);
var
  Point: TPoint;
  Rect: TRect;
begin
  with Message do
    case Msg of
      WM_ERASEBKGND:
        Result := 0;
      WM_GETMINMAXINFO:
        with PMinMaxInfo(lParam)^ do
        begin
          QueryTrackSize(PTrackSize(@ptMinTrackSize)^);
          Result := 0;
        end;
      WM_NCHITTEST:
        begin
          Point.x := SmallInt(LongRec(LParam).Lo);
          Point.y := SmallInt(LongRec(LParam).Hi);
          Windows.ScreenToClient(Handle, Point);
          Windows.GetClientRect(Handle, Rect);
          if FSizeable and (Point.x > Rect.Right - 16) and
            (Point.y > Rect.Bottom - 16) then
            Result := HTBOTTOMRIGHT
          else
            Result := HTCLIENT;
        end;
      WM_SETCURSOR:
        begin
          case LongRec(LParam).Lo of
            HTBOTTOMRIGHT:
              SetCursor(LoadCursor(0, IDC_SIZENWSE));
            HTCLIENT:
              SetCursor(LoadCursor(0, IDC_ARROW));
          end;
          Result := 1;
        end;
      WM_SIZE:
        begin
          InvalidateRect(Handle, nil, False);
          inherited WndProc(Message);
        end;
    else
      inherited WndProc(Message);
    end;
end;

procedure TCustomPopupForm.SetAssociate(Value: TWinControl);
begin
  if Value <> FAssociate then
  begin
    Hide;
    if FAssociate <> nil then
    begin
      FAssociate.RemoveFreeNotification(Self);
      FAssociate.WindowProc := FDefAssociateWindowProc;
    end;
    FAssociate := Value;
    if FAssociate <> nil then
    begin
      FAssociate.FreeNotification(Self);
      FDefAssociateWindowProc :=  FAssociate.WindowProc;
      FAssociate.WindowProc := AssociateWindowProc;
      Width := FAssociate.Width;
      Height := 175;
    end;
  end;
end;

procedure TCustomPopupForm.SetAssociateFrame(Value: TFrame);
begin
  Hide;
  FAssociateFrame := Value;
  if FAssociateFrame <> nil then
  begin
    FAssociateFrame.Parent := Self;
    FAssociateFrameWidth := FAssociateFrame.Width - 2;
  end;
end;


procedure TCustomPopupForm.SetForwardControl(Value: TWinControl);
begin
  if Value <> FForwardControl then
  begin
    ForwardChange(Value);
    FForwardControl := Value;
  end;
end;

procedure TCustomPopupForm.SetSizeable(Value: Boolean);
begin
  if Value <> FSizeable then
  begin
    Hide;
    FSizeable := Value;
    AdjustControlSize;
  end;
end;

procedure TCustomPopupForm.SetStatusText(const Value: string);
begin
  if Value <> FStatusText then
  begin
    FStatusText := Value;
    Invalidate;
  end;
end;

procedure TCustomPopupForm.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  if FCustomFrame then
    FillRectOutline(DC, R, FCustomFrameColor)
  else if FUseShadow then
    DrawFrame(DC, R, dfRaised)
  else
    FillRectOutline(DC, R, clBtnFace);
  ReleaseDC(Handle, DC);
  Message.Result := 0;
end;

procedure TCustomPopupForm.WMShowWindow(var Message: TWMShowWindow);
begin
  inherited;
  if FUseShadow and (Message.Status = 0) then
    if Message.Show  then
    begin
      FShadow.Associate := Handle;
      FShadow.Update;
    end
    else
    begin
      FShadow.Visible := False;
      FShadow.Associate := 0;
    end;
end;

procedure TCustomPopupForm.WMSize(var Message: TWMSize);
begin
  inherited;
  if Visible and FUseShadow then
    FShadow.Update;
end;

{ TPopupColorGridForm }

const
  Colors: array[0..51] of TIdentMapEntry = (
    (Value: clBlack; Name: 'Black'),
    (Value: clMaroon; Name: 'Maroon'),
    (Value: clGreen; Name: 'Green'),
    (Value: clOlive; Name: 'Olive'),
    (Value: clNavy; Name: 'Navy'),
    (Value: clPurple; Name: 'Purple'),
    (Value: clTeal; Name: 'Teal'),
    (Value: clGray; Name: 'Gray'),
    (Value: clSilver; Name: 'Silver'),
    (Value: clRed; Name: 'Red'),
    (Value: clLime; Name: 'Lime'),
    (Value: clYellow; Name: 'Yellow'),
    (Value: clBlue; Name: 'Blue'),
    (Value: clFuchsia; Name: 'Fuchsia'),
    (Value: clAqua; Name: 'Aqua'),
    (Value: clWhite; Name: 'White'),
    (Value: clMoneyGreen; Name: 'Money Green'),
    (Value: clSkyBlue; Name: 'Sky Blue'),
    (Value: clCream; Name: 'Cream'),
    (Value: clMedGray; Name: 'Mediu Gray'),
    (Value: clActiveBorder; Name: 'Active Border'),
    (Value: clActiveCaption; Name: 'Active Caption'),
    (Value: clAppWorkSpace; Name: 'Application Work Space'),
    (Value: clBackground; Name: 'Background'),
    (Value: clBtnFace; Name: 'Button Face'),
    (Value: clBtnHighlight; Name: 'Button Highlight'),
    (Value: clBtnShadow; Name: 'Button Shadow'),
    (Value: clBtnText; Name: 'Button Text'),
    (Value: clCaptionText; Name: 'Caption Text'),
    (Value: clDefault; Name: 'Default'),
    (Value: clGradientActiveCaption; Name: 'Gradient Active Caption'),
    (Value: clGradientInactiveCaption; Name: 'Gradient Inactive Caption'),
    (Value: clGrayText; Name: 'Gray Text'),
    (Value: clHighlight; Name: 'Highlight'),
    (Value: clHighlightText; Name: 'Highlight Text'),
    (Value: clHotLight; Name: 'Hot Light'),
    (Value: clInactiveBorder; Name: 'Inactive Border'),
    (Value: clInactiveCaption; Name: 'Inactive Caption'),
    (Value: clInactiveCaptionText; Name: 'Inactive Caption Text'),
    (Value: clInfoBk; Name: 'Info Background'),
    (Value: clInfoText; Name: 'Info Text'),
    (Value: clMenu; Name: 'Menu'),
    (Value: clMenuBar; Name: 'MenuBar'),
    (Value: clMenuHighlight; Name: 'Menu Highlight'),
    (Value: clMenuText; Name: 'Menu Text'),
    (Value: clNone; Name: 'None'),
    (Value: clScrollBar; Name: 'Scroll Bar'),
    (Value: cl3DDkShadow; Name: '3D Dark Shadow'),
    (Value: cl3DLight; Name: '3D Light'),
    (Value: clWindow; Name: 'Window'),
    (Value: clWindowFrame; Name: 'Window Frame'),
    (Value: clWindowText; Name: 'Window Text'));

function ColorToStr(Color: TColor): string;
var
  RGB: TRGBQuad absolute Color;
begin
  if not IntToIdent(Ord(Color), Result, Colors) then
  begin
    Color := ColorToRGB(Color);
    Result := '#' + Format('%.2x%.2x%.2x', [RGB.rgbBlue, RGB.rgbGreen, RGB.rgbRed]);
  end;
end;

function StrToColor(const Ident: string): TColor;
var
  Color: Longint;
  RGB: TRGBQuad absolute Color;
  B: Byte;
  S: string;
begin
  if not IdentToInt(Ident, Color, Colors) then
    if (Length(Ident) > 1) and (Ident[1] = '#') then
    begin
      S := Ident;
      S[1] := '$';
      Color := StrToInt(S) and $FFFFFF;
      B := RGB.rgbBlue;
      RGB.rgbBlue := RGB.rgbRed;
      RGB.rgbRed := B;
    end
    else
      raise EConvertError(SInvalidPropertyValue);
  Result := TColor(Color);
end;

function StrToColorDef(const Ident: string; const Default: TColor): TColor;
var
  Color: Longint;
  RGB: TRGBQuad absolute Color;
  B: Byte;
  S: string;
begin
  if not IdentToInt(Ident, Color, Colors) then
    if (Length(Ident) > 1) and (Ident[1] = '#') then
    begin
      S := Ident;
      S[1] := '$';
      Color := StrToIntDef(S, Integer(ColorToRGB(Default))) and $FFFFFF;
      B := RGB.rgbBlue;
      RGB.rgbBlue := RGB.rgbRed;
      RGB.rgbRed := B;
    end
    else
      Color := Default;
  Result := TColor(Color);
end;

constructor TPopupColorGridForm.Create(AOwner: TComponent);
const
  StandardColors: array[0..19] of TColor = (clWhite, clBlack, clGray, clSilver,
    clRed, clMaroon, clYellow, clOlive, clLime, clGreen, clAqua, clTeal, clBlue,
    clNavy, clFuchsia, clPurple, clBtnFace, clBackGround, clInfoText,
    clBtnShadow);
var
  I: Integer;
begin
  inherited Create(AOwner);
  Color := clBtnFace;
  Sizeable := False;
  for I := Low(FColorGrid) to High(FColorGrid) do
    with FColorGrid[I], Rect do
    begin
      Left := (I mod 4) * 22 + 1 + I mod 4;
      Top := (I div 4) * 22 + 1 + I div 4;
      Right := Left + 21;
      Bottom := Top + 21;
      Color := StandardColors[I];
    end;
  FButtonRect := Rect(1, 121, 68, 143);
  FActiveColor := clWhite;
  FColorRect := Rect(70, 121, 91, 142);
end;

function TPopupColorGridForm.ColorIndexFromPoint(const Point: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(FColorGrid) to High(FColorGrid) do
    if PtInRect(FColorGrid[I].Rect, Point) then
    begin
      Result := I;
      Break;
    end;
  if (Result < 0) and PtInRect(FColorRect, Point) then
    Result := High(FColorGrid) + 1;
end;

procedure TPopupColorGridForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and (not WS_BORDER);
    ExStyle := ExStyle or WS_EX_DLGMODALFRAME;
  end;
end;

procedure TPopupColorGridForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if PtInRect(FButtonRect, Point(X, Y)) then
    with TColorDialog.Create(nil) do
    try
      if Execute then
      begin
        FActiveColor := Color;
        Select;
      end
      else
        Cancel;
    finally
      Free;
    end;
end;

procedure TPopupColorGridForm.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewIndex: Integer;
  OldIndex: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  NewIndex := ColorIndexFromPoint(Point(X, Y));
  if (NewIndex > -1) and (FHighlightIndex <> NewIndex) then
  begin
    OldIndex := FHighlightIndex;
    FHighlightIndex := NewIndex;
    PaintItem(OldIndex);
    PaintItem(NewIndex);
  end;
end;

procedure TPopupColorGridForm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  NewIndex: Integer;
  NewColor: TColor;
begin
  inherited MouseUp(Button, Shift, X, Y);
  NewColor := FActiveColor;
  NewIndex := ColorIndexFromPoint(Point(X, Y));
  if NewIndex > -1 then
  begin
    if NewIndex < High(FColorGrid) + 1 then
      NewColor := FColorGrid[NewIndex].Color;
    ActiveColor := NewColor;
  end;
end;

procedure TPopupColorGridForm.Paint;
var
  DC: HDC;
  I: Integer;
begin
  DC := Canvas.Handle;
  FillRectColor(DC, ClientRect, clBtnFace);
  for I := Low(FColorGrid) to High(FColorGrid) do
    PaintItem(I);
  PaintItem(High(FColorGrid) + 1);
  DrawThemeButton(DC, FButtonRect, []);
  DrawCaption(DC,  'Other ...', FButtonRect, drCenter);
  DrawDivider(DC, Rect(1, 117, 91, 119), ddVert);
end;

procedure TPopupColorGridForm.PaintItem(Index: Integer);
var
  DrawRect: TRect;
  DrawColor: TColor;
begin
  if Index < 0 then
    Exit;
  if Index > High(FColorGrid) then
  begin
    DrawRect := FColorRect;
    DrawColor := FActiveColor;
  end
  else
  with FColorGrid[Index] do
  begin
    DrawRect := Rect;
    DrawColor := Color;
  end;
  if Index = FHighlightIndex then
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(DrawRect);
    InflateRect(DrawRect, -1, -1);
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(DrawRect);
    InflateRect(DrawRect, -1, -1);
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(DrawRect);
    InflateRect(DrawRect, -1, -1);
    Canvas.Brush.Color := DrawColor;
    Canvas.FillRect(DrawRect);
  end
  else
  begin
    Canvas.Brush.Color := DrawColor;
    Canvas.FillRect(DrawRect);
    DrawThemeBorder(Canvas.Handle, DrawRect, []);
  end;
end;

procedure TPopupColorGridForm.Popup;
begin
  Width := 98;
  Height := 150;
  FHighlightIndex := High(FColorGrid) + 1;
  inherited Popup;
end;

procedure TPopupColorGridForm.SetActiveColor(Value: TColor);
begin
  if Value <> ActiveColor then
  begin
    FActiveColor := Value;
    Select;
  end
  else
    Cancel;
end;

{ TPopupTree }

procedure TPopupTree.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and (not WS_BORDER or WS_HSCROLL);
    ExStyle := ExStyle and (not WS_EX_CLIENTEDGE);
  end;
end;

procedure TPopupTree.WMNCCalcSize(var Message: TMessage);
var
  Style: Integer;
begin
  Style := GetWindowLong(Handle, GWL_STYLE);
  SetWindowLong(Handle, GWL_STYLE, Style and not (WS_BORDER or WS_HSCROLL));
  inherited;
end;

{ TPopupTreeForm }

constructor TPopupTreeForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTree := TPopupTree.Create(Self);
  ForwardControl := FTree;
  with FTree do
  begin
    ReadOnly := True;
    ShowLines := False;
    OnClick := TreeClick;
  end;
  AdjustControlSize;
end;

procedure TPopupTreeForm.TreeClick(Sender: TObject);
begin
  with FTree, ScreenToClient(Mouse.CursorPos) do
    if htOnItem in GetHitTestInfoAt(x, y) then
      Self.Select;
end;

{ TPopupImageListForm }

constructor TPopupImageListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageDrawList := TImageDrawList.Create(Self);
  ForwardControl := FImageDrawList;
  FImageDrawList.Parent := Self;
  FImageDrawList.Align := alClient;
  with FImageDrawList do
    BorderStyle := bsNone;
  Sizeable := False;
  AdjustControlSize;
end;

{ TPopupCheckListForm }

constructor TPopupCheckListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckList := TCheckListBox.Create(Self);
  ForwardControl := FCheckList;
  with FCheckList do
  begin
    BorderStyle := bsNone;
  end;
  AdjustControlSize;
end;

{ TPopupDateForm }

constructor TPopupDateForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sizeable := False;
  FCalendar := TMonthCalendar.Create(Self);
  with FCalendar do
  begin
    Parent := Self;
    OnClick := CalendarClick;
  end;
  ForwardControl := FCalendar;
end;

function TPopupDateForm.GetDate: TDateTime;
begin
  Result := Trunc(FCalendar.Date);
end;

procedure TPopupDateForm.SetDate(const Value: TDateTime);
begin
  FCalendar.Date := Trunc(Value);
end;

procedure TPopupDateForm.Popup;
begin
  if not FCalendar.HandleAllocated then
  begin
    Top := -3000;
    FCalendar.Width := 100;
    Show;
    UpdateWindow(Handle);
    FCalendar.Perform(WM_SIZE, 0, 0);
    Hide;
  end;
  Width := FCalendar.Width;
  Height := FCalendar.Height;
  inherited Popup;
end;

procedure TPopupDateForm.CalendarClick(Sender: TObject);
begin
  if ScreenToClient(Mouse.CursorPos).Y > 30 then
    Select;
end;

{ TCustomPopupListForm }

constructor TCustomPopupListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sizeable := False;
  FDisplayColumn := -1;
  FListBox := TListBox.Create(Self);
  with FListBox do
  begin
    BorderStyle := bsNone;
    OnMouseMove := ListBoxMouseMove;
    Style := lbOwnerDrawFixed;
    ItemHeight := 18;
    OnMouseUp := ListMouseUp;
    OnDrawItem := ListDrawItem;
  end;
  ForwardControl := FListBox;
  AdjustControlSize;
end;

procedure TCustomPopupListForm.ListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  List: TListBox absolute Control;
  DC: HDC;
  R: TRect;
  S: string;
begin
  DC := List.Canvas.Handle;
  R := Rect;
  if odSelected in State then
    DrawStyleRect(DC, R)
  else
    FillRect(DC, R, COLOR_WINDOW + 1);
  InflateRect(R, -3, 0);
  SetTextColor(DC, ColorToRGB(clWindowText));
  S := List.Items[Index];
  if FDisplayColumn > -1 then
    S := FieldValue(S, FDisplayColumn);
  if List.Items.Count > 0 then
    DrawCaption(DC, StringReplace(S, '&', '&&', [rfReplaceAll]), R, drLeft);
  SelectClipRect(DC, Rect, RGN_DIFF);
end;

procedure TCustomPopupListForm.Popup;
var
  I: Integer;
begin
  if FAssociate <> nil then
  begin
    Width := FAssociate.Width;
    FListBox.Font := TCustomPopupListForm(FAssociate).Font;
  end;
  I := FListBox.Items.Count;
  if I = 0 then
    I := 1
  else if I > 8 then
    I := 8;
  Height := I * FListBox.ItemHeight + 2;
  if Mode = lmVerbose then
  begin
    I := VerboseWidth;
    if I > Width then
      Width := I;
  end;
  inherited Popup;
end;

function TCustomPopupListForm.GetItemIndex: Integer;
begin
  Result := FListBox.ItemIndex;
end;

procedure TCustomPopupListForm.SetItemIndex(Value: Integer);
begin
  FListBox.ItemIndex := Value;
end;

function TCustomPopupListForm.GetItems: TStrings;
begin
  Result := FListBox.Items;
end;

procedure TCustomPopupListForm.SetItems(Value: TStrings);
begin
  FListBox.Items := Value;
end;

procedure TCustomPopupListForm.ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  with ListBox do
    ItemIndex := ItemAtPos(Point(X, Y), False);
end;

procedure TCustomPopupListForm.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if PtInRect(ClientRect, Point(X, Y)) then
    Select
  else
    Cancel;
end;

function TCustomPopupListForm.GetVerboseWidth: Integer;
begin
  Result := Width;
end;

{ TPopupBrushForm }

const
  BrushStyles: array[TBrushStyle] of TIdentMapEntry = (
    (Value: Ord(bsSolid); Name: 'Solid'),
    (Value: Ord(bsClear); Name: 'Clear'),
    (Value: Ord(bsHorizontal); Name: 'Horizontal'),
    (Value: Ord(bsVertical); Name: 'Vertical'),
    (Value: Ord(bsFDiagonal); Name: 'Forward Diagonal'),
    (Value: Ord(bsBDiagonal); Name: 'Back Diagonal'),
    (Value: Ord(bsCross); Name: 'Cross'),
    (Value: Ord(bsDiagCross); Name: 'Diagonal Cross'));

function BrushStyleToStr(BrushStyle: TBrushStyle): string;
begin
  if not IntToIdent(Ord(BrushStyle), Result, BrushStyles) then
    raise EConvertError(SInvalidPropertyValue);
end;

function StrToBrushStyle(const Ident: string): TBrushStyle;
var
  BrushStyle: Longint;
begin
  if not IdentToInt(Ident, BrushStyle, BrushStyles) then
    raise EConvertError(SInvalidPropertyValue);
  Result := TBrushStyle(BrushStyle);
end;

function StrToBrushStyleDef(const Ident: string; const Default: TBrushStyle): TBrushStyle;
var
  BrushStyle: Longint;
begin
  if IdentToInt(Ident, BrushStyle, BrushStyles) then
    Result := TBrushStyle(BrushStyle)
  else
    Result := Default;
end;

constructor TPopupBrushForm.Create(AOwner: TComponent);
var
  I: TBrushStyle;
begin
  inherited Create(AOwner);
  ForwardFocus := False;
  with ListBox do
  begin
    Style := lbOwnerDrawFixed;
    OnDrawItem := ListBoxDrawItem;
    Items.BeginUpdate;
    try
      for I := Low(TBrushStyle) to High(TBrushStyle) do
        Items.Add(BrushStyleToStr(I));
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TPopupBrushForm.ListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  BackColors: array[Boolean] of TColor = (clWindow, clHighlight);
  ForeColors: array[Boolean] of TColor = (clWindowText, clWindow);
var
  DC: HDC;
  DrawBrush: HBRUSH;
  ForeColor: TColor;
  DrawPen: HPEN;
  PriorPen: HPEN;
  PriorBrush: HBRUSH;
begin
  with Control as TListBox do
  begin
    DC := Canvas.Handle;
    DrawBrush := CreateSolidBrush(ColorToRGB(BackColors[odSelected in State]));
    FillRect(DC, Rect, DrawBrush);
    DeleteObject(DrawBrush);
    if Mode = lmVerbose then
      Rect.Right := Rect.Left + ItemHeight;
    InflateRect(Rect, -2, -1);
    ForeColor := ColorToRGB(ForeColors[odSelected in State]);
    case TBrushStyle(Index) of
      bsSolid: DrawBrush := CreateSolidBrush(ForeColor);
      bsClear: DrawBrush := CreateSolidBrush(ColorToRGB(BackColors[odSelected in State]));
      bsHorizontal: DrawBrush := CreateHatchBrush(HS_HORIZONTAL, ForeColor);
      bsVertical: DrawBrush := CreateHatchBrush(HS_VERTICAL, ForeColor);
      bsFDiagonal: DrawBrush := CreateHatchBrush(HS_FDIAGONAL, ForeColor);
      bsBDiagonal: DrawBrush := CreateHatchBrush(HS_BDIAGONAL, ForeColor);
      bsCross: DrawBrush := CreateHatchBrush(HS_CROSS, ForeColor);
      bsDiagCross: DrawBrush := CreateHatchBrush(HS_DIAGCROSS, ForeColor);
    end;
    DrawPen := CreatePen(PS_SOLID, 1, ForeColor);
    PriorBrush := SelectObject(DC, DrawBrush);
    PriorPen := SelectObject(DC, DrawPen);
    with Rect do
      Rectangle(DC, Left, Top, Right, Bottom);
    if Mode = lmVerbose then
    begin
      Rect.Right := ClientWidth - 2;
      Inc(Rect.Left, ItemHeight + 2);
      DrawText(DC, PChar(Items[Index]), -1, Rect, DR_LEFT);
    end;
    OverwriteObject(DC, PriorBrush);
    OverwriteObject(DC, PriorPen);
  end;
end;

procedure TPopupBrushForm.Select;
begin
  FBrushStyle := TBrushStyle(ListBox.ItemIndex);
  inherited Select;
end;

function TPopupBrushForm.GetBrush: TBrush;
begin
  Result := Listbox.Canvas.Brush;
  Result.Style := FBrushStyle;
end;

procedure TPopupBrushForm.SetBrush(Value: TBrush);
begin
  Listbox.Canvas.Brush := Value;
  BrushStyle := Value.Style;
end;

procedure TPopupBrushForm.SetBrushStyle(Value: TBrushStyle);
begin
  if Value <> FBrushStyle then
  begin
    FBrushStyle := Value;
    ListBox.ItemIndex := Ord(FBrushStyle);
    Select;
  end;
end;

function TPopupBrushForm.GetVerboseWidth: Integer;
begin
  with ListBox do
  begin
    Canvas.Font := Font;
    Result := Canvas.TextWidth('Forward Diagonal') + ItemHeight + 12;
  end;
end;

{ TPopupPenForm }

const
  PenStyles: array[psSolid..psInsideFrame] of TIdentMapEntry = (
    (Value: Ord(psSolid); Name: 'Solid'),
    (Value: Ord(psDash); Name: 'Dash'),
    (Value: Ord(psDot); Name: 'Dot'),
    (Value: Ord(psDashDot); Name: 'Dash Dot'),
    (Value: Ord(psDashDotDot); Name: 'Dash Dot Dot'),
    (Value: Ord(psClear); Name: 'Clear'),
    (Value: Ord(psInsideFrame); Name: 'Inside Frame'));

function PenStyleToStr(PenStyle: TPenStyle): string;
begin
  if PenStyle > psInsideFrame then PenStyle := psSolid;
  if not IntToIdent(Ord(PenStyle), Result, PenStyles) then
    raise EConvertError(SInvalidPropertyValue);
end;

function StrToPenStyle(const Ident: string): TPenStyle;
var
  PenStyle: Longint;
begin
  if not IdentToInt(Ident, PenStyle, PenStyles) then
    raise EConvertError(SInvalidPropertyValue);
  Result := TPenStyle(PenStyle);
end;

function StrToPenStyleDef(const Ident: string; const Default: TPenStyle): TPenStyle;
var
  PenStyle: Longint;
begin
  if IdentToInt(Ident, PenStyle, PenStyles) then
    Result := TPenStyle(PenStyle)
  else
    Result := Default;
end;


constructor TPopupPenForm.Create(AOwner: TComponent);
var
  I: TPenStyle;
begin
  inherited Create(AOwner);
  ForwardFocus := False;
  with ListBox do
  begin
    Style := lbOwnerDrawFixed;
    OnDrawItem := ListBoxDrawItem;
    Items.BeginUpdate;
    try
      for I := Low(TPenStyle) to High(TPenStyle) do
        Items.Add(PenStyleToStr(I));
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TPopupPenForm.ListBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  BackColors: array[Boolean] of TColor = (clWindow, clHighlight);
  DrawColors: array[Boolean] of TColor = (clWindowText, clWindow);
var
  DC: HDC;
  DrawBrush: HBRUSH;
  DrawColor: TColor;
  DrawPen: HPEN;
  PriorPen: HPEN;
  PriorMode: THandle;
begin
  with Control as TListBox do
  begin
    DC := Canvas.Handle;
    DrawBrush := CreateSolidBrush(ColorToRGB(BackColors[odSelected in State]));
    FillRect(DC, Rect, DrawBrush);
    DeleteObject(DrawBrush);
    if Mode = lmVerbose then
      Rect.Right := Rect.Left + ItemHeight * 3;
    InflateRect(Rect, -2, -1);
    DrawColor := ColorToRGB(DrawColors[odSelected in State]);
    DrawPen := 0;
    case TPenStyle(Index) of
      psSolid: DrawPen := CreatePen(PS_SOLID, 1, DrawColor);
      psDash: DrawPen := CreatePen(PS_DASH, 1, DrawColor);
      psDot: DrawPen := CreatePen(PS_DOT, 1, DrawColor);
      psDashDot: DrawPen := CreatePen(PS_DASHDOT, 1, DrawColor);
      psDashDotDot: DrawPen := CreatePen(PS_DASHDOTDOT, 1, DrawColor);
      psClear: DrawPen := CreatePen(PS_NULL, 1, DrawColor);
      psInsideFrame: DrawPen := CreatePen(PS_INSIDEFRAME, 1, DrawColor);
    end;
    PriorPen := SelectObject(DC, DrawPen);
    MoveToEx(DC, Rect.Left, Rect.Top + ItemHeight div 2 - 1, nil);
    LineTo(DC, Rect.Right, Rect.Top + ItemHeight div 2 - 1);
    MoveToEx(DC, Rect.Left, Rect.Top + ItemHeight div 2, nil);
    LineTo(DC, Rect.Right, Rect.Top + ItemHeight div 2);
    if Mode = lmVerbose then
    begin
      Rect.Right := ClientWidth - 2;
      Inc(Rect.Left, ItemHeight * 3 + 2);
      PriorMode := SetBkMode(DC, TRANSPARENT);
      DrawText(DC, PChar(Items[Index]), -1, Rect, DR_LEFT);
      SetBkMode(DC, PriorMode);
    end;
    OverwriteObject(DC, PriorPen);
  end;
end;

procedure TPopupPenForm.Select;
begin
  FPenStyle := TPenStyle(ListBox.ItemIndex);
  inherited Select;
end;

function TPopupPenForm.GetPen: TPen;
begin
  Result := Listbox.Canvas.Pen;
  Result.Style := FPenStyle;
end;

procedure TPopupPenForm.SetPen(Value: TPen);
begin
  Listbox.Canvas.Pen := Value;
  PenStyle := Value.Style;
end;

procedure TPopupPenForm.SetPenStyle(Value: TPenStyle);
begin
  if Value <> FPenStyle then
  begin
    FPenStyle := Value;
    ListBox.ItemIndex := Ord(FPenStyle);
    Select;
  end;
end;

function TPopupPenForm.GetVerboseWidth: Integer;
begin
  with ListBox do
  begin
    Canvas.Font := Font;
    Result := Canvas.TextWidth('Dash Dot Dot') + ItemHeight * 3 + 12;
  end;
end;

{ TScrollingMenuItem }

constructor TScrollingMenuItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageIndex := -1;
end;

procedure TScrollingMenuItem.Assign(Source: TPersistent);
var
  EditItem: TScrollingMenuItem absolute Source;
begin
  if Source is TScrollingMenuItem then
  begin
    FCaption := EditItem.Caption;
    FDefaulted := EditItem.Defaulted;
    FImageIndex := EditItem.ImageIndex;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TScrollingMenuItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TScrollingMenuItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

{ TScrollingMenuItems }

constructor TScrollingMenuItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScrollingMenuItem);
end;

function TScrollingMenuItems.Add: TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(inherited Add);
end;

function TScrollingMenuItems.FindItemID(ID: Integer): TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(inherited FindItemID(ID));
end;

function TScrollingMenuItems.Insert(Index: Integer): TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(inherited Insert(Index));
end;

procedure TScrollingMenuItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TScrollingMenu then
    TScrollingMenu(GetOwner).ItemsChanged;
end;

function TScrollingMenuItems.Get(Index: Integer): TScrollingMenuItem;
begin
  Result := TScrollingMenuItem(GetItem(Index));
end;

procedure TScrollingMenuItems.Put(Index: Integer; Value: TScrollingMenuItem);
begin
  SetItem(Index, Value);
end;

{ TScrollingMenu }

constructor TScrollingMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TScrollingMenuItems.Create(Self);
  ItemHeight := 22;
  Color := clMenu;
  DoubleBuffered := True;
  BorderStyle := bsNone;
  HotTrack := True;
end;

destructor TScrollingMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TScrollingMenu.ItemsChanged;
begin
  Count := FItems.Count;
  Invalidate;
end;

const
  GutterWidth = 32;

procedure TScrollingMenu.DrawBackground;
var
  DC: HDC;
  R: TRect;
begin
  if Assigned(OnDrawBackground) then
  begin
    OnDrawBackground(Self);
    Exit;
  end;
  DC := Canvas.Handle;
  R := ClientRect;
  R.Right := R.Left + GutterWidth;
  FillRectColor(DC, R, clMenuBar);
  R.Left := R.Right;
  R.Right := ClientWidth;
  FillRectColor(DC, R, Color);
end;

procedure TScrollingMenu.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  Item: TScrollingMenuItem;
  DC: HDC;
  F: HFONT;
  R: TRect;
begin
  if Assigned(OnDrawItem) then
  begin
    OnDrawItem(Self, Canvas, Index, Rect, State);
    Exit;
  end;
  Item := Items[Index];
  DC := Canvas.Handle;
  R := Rect;
  R.Right := GutterWidth;
  DrawRectEdge(DC, R, clBtnHighlight, drRight);
  Dec(R.Right);
  DrawRectEdge(DC, R, Blend(clBtnShadow, clMenu, 30), drRight);
  R := Rect;
  Dec(R.Right);
  if dsHot in State then
    DrawFancyMenuHighlightRect(DC, R);
  R := Rect;
  R.Right := R.Left + GutterWidth;
  ImageListDraw(Images, Canvas, R, Item.ImageIndex, []);
  R.Left := R.Right;
  R.Right := Rect.Right;
  InflateRect(R, -4, 0);
  if Item.Defaulted then
  begin
    F := SelectObject(DC, GetFont(DC, [fsBold]));
    DrawCaption(DC, Item.Caption, R, drLeft);
    OverwriteObject(DC, F);
  end
  else
    DrawCaption(DC, Item.Caption, R, drLeft);
end;

procedure TScrollingMenu.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if Message.WheelDelta < 0 then
    TopIndex := TopIndex + 3
  else
    TopIndex := TopIndex - 3;
  Message.Result := 1;
end;

{ TPopupScrollingMenu }

constructor TPopupScrollingMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisplayCount := 15;
  FMenu := TScrollingMenu.Create(Self);
  FMenu.Parent := Self;
  FMenu.OnClick := MenuClick;
  ForwardControl := FMenu;
  FMinWidth := 120;
end;

procedure TPopupScrollingMenu.Popup;

  function ComputeWidth: Integer;
  var
    DC: HDC;
    F: HFont;
    Extra: Integer;
    W: Integer;
    I: Integer;
  begin
    Result := FMinWidth;
    DC := Canvas.Handle;
    Extra := 52;
    if FMenu.Items.Count > FDisplayCount then
      Inc(Extra, GetSystemMetrics(SM_CXVSCROLL));
    F := 0;
    for I := 0 to FMenu.Items.Count - 1 do
    begin
      if FMenu.Items[I].Defaulted then
        F := SelectObject(DC, GetFont(DC, [fsBold]));
      W := FontWidth(DC, FMenu.Items[I].Caption) + Extra;
      if FMenu.Items[I].Defaulted then
        OverwriteObject(DC, F);
      if W > Result then
        Result := W;
    end;
  end;

var
  I: Integer;
begin
  I := FMenu.Count;
  if I = 0 then Exit;
  FMenu.ItemIndex := 0;
  if I > FDisplayCount then
    I := FDisplayCount;
  Width := ComputeWidth;
  Height := FMenu.ItemHeight * I + 2;
  FMenu.Width := ClientWidth;
  FMenu.Height := ClientHeight;
  if AssociateFrame <> nil then
  begin
    ClientHeight := ClientHeight + AssociateFrame.Height - 2;
    AssociateFrame.Left := 0;
    AssociateFrame.Top := ClientHeight - AssociateFrame.Height + 2;
    FMenu.Height := ClientHeight - AssociateFrame.Height + 2;
    if ClientWidth < FAssociateFrameWidth then
    begin
      ClientWidth := FAssociateFrameWidth - 2;
      FAssociateFrame.Width := FAssociateFrameWidth;
      FMenu.Width := ClientWidth;
    end
    else
    begin
      FAssociateFrame.Width := ClientWidth + 2;
      FMenu.Width := ClientWidth;
    end;
  end;
  inherited Popup;
end;

procedure TPopupScrollingMenu.MenuClick(Sender: TObject);
begin
  FMenuIndex := FMenu.ItemIndex;
  Select;
end;

end.
