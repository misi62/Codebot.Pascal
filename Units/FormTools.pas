
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit FormTools;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, ActiveX, SysUtils, Classes, Graphics, Controls, Forms,
  TypInfo, StdCtrls, Contnrs, ImgList, CommCtrl, SysConst, ShellAPI,
  BaseTypes, GraphTools, ProviderTools, GdiPlus;

type
  TControlBounds = class
  public
    Control: TControl;
    Bounds: TRect;
    constructor Create(Control: TControl);
  end;

function AdjustFonts(Form: TCustomForm): Double;
function AdjustFontBorders(Form: TCustomForm; Control: TControl; Offset: Integer = 8): Double;

procedure AlignCenter(Control: TControl); overload;
procedure AlignCenter(ControlA, ControlB: TControl); overload;

procedure AlignControlText(const Text: string; Margin: Integer; Control: TControl);
procedure FillControlText(const Text: string; Margin: Integer; Control: TControl);
procedure FillControlSpace(ControlA, ControlB: TControl; Margin: Integer);

function CalcTextBoxHeight: Integer;
function CalcEditHeight(Font: TFont): Integer;

procedure AdjustEditRect(Edit: TEdit; Rect: TRect);
procedure AdjustEditVerticalRect(Edit: TEdit; Rect: TRect);

type
  THotTrackEvent = procedure(Sender: TObject; TrackObject: TObject) of object;

  PComponent = ^TComponent;
  PForm = ^TForm;

  TComponentNotifier = class(TComponent)
  private
    FRef: PComponent;
    FEvent: TNotifyEvent;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor CreateNotifier(Ref: PComponent; Event: TNotifyEvent);
    destructor Destroy; override;
    procedure AddFreeNotifier(Component: TComponent);
  end;

  TInstanceMethod = procedure of object;

procedure CallMethod(Instance: TObject; const MethodName: string);

{ AnchorData }

type
  TAnchorData = record
    Control: TControl;
    Anchors: TAnchors;
  end;

  TAnchorDataArray = array of TAnchorData;

function SaveAnchors(Container: TWinControl): TAnchorDataArray;
procedure RestoreAnchors(const Data: TAnchorDataArray);

type
  TVisibleData = record
    Control: TControl;
    Visible: Boolean;
  end;

  TVisibleDataArray = array of TVisibleData;

function SaveVisible(Container: TWinControl): TVisibleDataArray;
procedure RestoreVisible(const Data: TVisibleDataArray);

procedure ScrollToRect(ScrollBox: TScrollBox; Rect: TRect); overload;
procedure ScrollToRect(ScrollBox: TScrollBox; Control: TControl; Rect: TRect); overload;

{ TControlTemplate class }

type
  TControlTemplate = class
  private
    FParent: TWinControl;
    FTemplate: TWinControl;
    FControls: TList;
  public
    constructor Create(Template: TWinControl);
    destructor Destroy; override;
    procedure Insert(Parent: TWinControl);
  end;

{ TCompositeLink }

  ICompositeLink = interface
    ['{ACBF2254-1F56-4947-813D-F7582FC5CE86}']
    procedure Composite(const Surface: TFastBitmap);
  end;

  TCompositeEvent = procedure(const Surface: TFastBitmap) of object;

  TCompositeLink = class(TInterfacedObject, ICompositeLink)
  private
    FEvent: TCompositeEvent;
  protected
    procedure Composite(const Surface: TFastBitmap);
  public
    constructor Create(Event: TCompositeEvent);
  end;

  TNullPanel = class(TWinControl)
  private
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property Enabled;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property Visible;
  end;

{ TPaintImage }

  TPaintImage = class(TImagesProviderControl)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Align;
    property Anchors;
    property Caption;
    property Images;
    property ImageIndex;
    property Visible;
    property OnClick;
    property Hint;
    property ShowHint;
    property ParentShowHint;
  end;

{ TPaintPanel }

  TPaintPanel = class(TCustomControl)
  private
    FAllowEraseBkgnd: Boolean;
    FBitmap: TFastBitmap;
    FBufferChanged: Boolean;
    FCompositeBitmap: TFastBitmap;
    FCompositeLinks: IInterfaceList;
    FOnCreateBitmap: TNotifyEvent;
    FOnDestroyBitmap: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateBitmap(var Bitmap: TFastBitmap; W, H: Integer); virtual;
    procedure DestroyBitmap(var Bitmap: TFastBitmap); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure VisibleChanging; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisposeBuffer;
    property AllowEraseBkgnd: Boolean read FAllowEraseBkgnd write FAllowEraseBkgnd;
    procedure AddCompositeLink(Link: ICompositeLink);
    procedure RemoveCompositeLink(Link: ICompositeLink);
    procedure Invalidate; override;
    procedure Revalidate; virtual;
    property Bitmap: TFastBitmap read FBitmap;
    property Canvas;
  published
    property Anchors;
    property Align;
    property Color;
    property ParentColor default False;
    property Visible;
    property Font;
    property ParentFont;
    property PopupMenu;
    property Hint;
    property ParentShowHint;
    property ParentBackground;
    property TabStop;
    property TabOrder;
    property Enabled;
    property OnCreateBitmap: TNotifyEvent read FOnCreateBitmap write FOnCreateBitmap;
    property OnDestroyBitmap: TNotifyEvent read FOnDestroyBitmap write FOnDestroyBitmap;
    property OnEnter;
    property OnExit;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnClick;
  end;

  TGraphicBufferedControl = class(TGraphicControl)
  private
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  end;

{ TEmptyWindow }

  TEmptyWindow = class(TProviderWindow)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color;
    property Font;
    property OnMouseDown;
  end;

{ TCustomFramedWindow }

  TFrameStyle = (fsEdit, fsCombo);

  TCustomFramedWindow = class(TProviderWindow)
  private
    FActive: Boolean;
    FBorderStyle: TBorderStyle;
    procedure Activate(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    function GetInternalWidth: Integer;
    procedure SetInternalWidth(Value: Integer);
    function GetInternalHeight: Integer;
    procedure SetInternalHeight(Value: Integer);
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property InternalWidth: Integer read GetInternalWidth write SetInternalWidth;
    property InternalHeight: Integer read GetInternalHeight write SetInternalHeight;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TFramedWindow }

  TFramedWindow = class(TCustomFramedWindow)
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property ParentColor;
    property Color;
    property Enabled;
    property TabOrder;
    property Visible;
  end;

{ TFramedImagesWindow }

  TFramedImagesWindow = class(TFramedWindow)
  private
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageChangeLink: TChangeLink;
    procedure ImageListChange(Sender: TObject);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FimageIndex write SetImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TFrameEdit }

  TFrameEdit = class(TCollectionItem)
  private
    FEdit: TEdit;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Edit: TEdit read FEdit;
  end;

{ TFrameEdits }

  TFrameEdits = class(TOwnedCollection)
  private
    function Get(Index: Integer): TFrameEdit;
    procedure Put(Index: Integer; Value: TFrameEdit);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TFrameEdit;
    function FindItemID(ID: Integer): TFrameEdit;
    function Insert(Index: Integer): TFrameEdit;
    property Items[Index: Integer]: TFrameEdit read Get write Put; default;
  end;

{ TCustomFramedEditWindow }

  TButtonRect = TRect;
  TButtonRects = array of TButtonRect;

  TCustomFramedEditWindow = class(TFramedImagesWindow)
  private
    FEdits: TFrameEdits;
    FAutoHeight: Boolean;
    FButtons: TButtonRects;
    FButtonHot: Integer;
    FButtonDown: Integer;
    FMouseDown: Boolean;
    procedure SetAutoHeight(Value: Boolean);
    function GetButtonCount: Integer;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    function GetButtonRect(Index: Integer): TButtonRect; virtual;
    procedure AlignEdits; virtual;
    procedure AdjustHeight;
    procedure ButtonClick(Index: Integer); dynamic;
    procedure ButtonHover(Index: Integer); dynamic;
    procedure ButtonClear;
    function ButtonDefine(Button: TButtonRect): Integer;
    procedure ButtonDraw(Index: Integer; Rect: TRect; State: TDrawState); virtual;
    function ButtonFromPoint(P: TPoint): Integer;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property ButtonHot: Integer read FButtonHot;
    property ButtonDown: Integer read FButtonDown;
    property ButtonCount: Integer read GetButtonCount;
    property ButtonRect[Index: Integer]: TButtonRect read GetButtonRect;
    property Edits: TFrameEdits read FEdits;
    property IsMouseDown: Boolean read FMouseDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TInputWindow }

  TInputWindow = class(TFramedWindow)
  private
    FClientBorder: Integer;
    //procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetClientBorder(Value: Integer);
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function GetClientRect: TRect; override;
    property ClientBorder: Integer read FClientBorder write SetClientBorder;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TabStop;
    property TabOrder;
    property Hint;
    property ShowHint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Color;
    property Font;
    property DragCursor;
    property DragKind;
    property DragMode;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseUp;
    property OnResize;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;
  end;

function FindTaskBar(out Rect: TRect): Integer;
procedure ShowTrayForm(Form: TCustomForm);

type
  _IMAGELISTDRAWPARAMS = packed record
    cbSize: DWORD;
    himl: HIMAGELIST;
    i: Integer;
    hdcDst: HDC;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    xBitmap: Integer;
    yBitmap: Integer;
    rgbBk: COLORREF;
    rgbFg: COLORREF;
    fStyle: UINT;
    dwRop: DWORD;
    fState: DWORD;
    Frame: DWORD;
    crEffect: COLORREF;
  end;

  PImageListDrawParams = ^TImageListDrawParams;
  TImageListDrawParams = _IMAGELISTDRAWPARAMS;

  IImageList = interface(IUnknown)
    ['{46EB5926-582E-4017-9FDF-E8998DAA0950}']
    function Add(Image, Mask: HBITMAP; var Index: Integer): HRESULT; stdcall;
    function ReplaceIcon(IndexToReplace: Integer; Icon: HICON; var Index: Integer): HRESULT; stdcall;
    function SetOverlayImage(iImage: Integer; iOverlay: Integer): HRESULT; stdcall;
    function Replace(Index: Integer; Image, Mask: HBITMAP): HRESULT; stdcall;
    function AddMasked(Image: HBITMAP; MaskColor: COLORREF; var Index: Integer): HRESULT; stdcall;
    function Draw(var DrawParams: TImageListDrawParams): HRESULT; stdcall;
    function Remove(Index: Integer): HRESULT; stdcall;
    function GetIcon(Index: Integer; Flags: UINT; var Icon: HICON): HRESULT; stdcall;
    function GetImageInfo(Index: Integer; var ImageInfo: TImageInfo): HRESULT; stdcall;
    function Copy(iDest: Integer; SourceList: IUnknown; iSource: Integer; Flags: UINT): HRESULT; stdcall;
    function Merge(i1: Integer; List2: IUnknown; i2, dx, dy: Integer; ID: TGUID; out ppvOut): HRESULT; stdcall;
    function Clone(ID: TGUID; out ppvOut): HRESULT; stdcall;
    function GetImageRect(Index: Integer; var rc: TRect): HRESULT; stdcall;
    function GetIconSize(var cx, cy: Integer): HRESULT; stdcall;
    function SetIconSize(cx, cy: Integer): HRESULT; stdcall;
    function GetImageCount(var Count: Integer): HRESULT; stdcall;
    function SetImageCount(NewCount: UINT): HRESULT; stdcall;
    function SetBkColor(BkColor: COLORREF; var OldColor: COLORREF): HRESULT; stdcall;
    function GetBkColor(var BkColor: COLORREF): HRESULT; stdcall;
    function BeginDrag(iTrack, dxHotSpot, dyHotSpot: Integer): HRESULT; stdcall;
    function EndDrag: HRESULT; stdcall;
    function DragEnter(hWndLock: HWND; x, y: Integer): HRESULT; stdcall;
    function DragLeave(hWndLock: HWND): HRESULT; stdcall;
    function DragMove(x, y: Integer): HRESULT; stdcall;
    function SetDragCursorImage(Image: IUnknown; iDrag, dxHotSpot, dyHotSpot: Integer): HRESULT; stdcall;
    function DragShowNoLock(fShow: BOOL): HRESULT; stdcall;
    function GetDragImage(var CurrentPos, HotSpot: TPoint; ID: TGUID; out ppvOut): HRESULT; stdcall;
    function GetItemFlags(i: Integer; var dwFlags: DWORD): HRESULT; stdcall;
    function GetOverlayImage(iOverlay: Integer; var iIndex: Integer): HRESULT; stdcall;
  end;

{procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; State: TDrawState); overload;
procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; State: TDrawState); overload;
procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  const Rect: TRect; Index: Integer; State: TDrawState); overload;}
{procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; Enabled: Boolean = True; Hotlight: Boolean = False); overload;}
{function ImageListSize(ImageList: TCustomImageList): TSize;}
function ImageListQueryInterface(ImageList: TCustomImageList; const IID: TGUID; out Obj): HRESULT;

var
  RegisterTransparentUnits: procedure(AOwner: TComponent);
  ErrorBoxProc: procedure(const Title, Msg: string);

procedure HandleExternalException(Sender: TObject);

implementation

constructor TControlBounds.Create(Control: TControl);
begin
  Self.Control := Control;
  Bounds := Control.BoundsRect;
end;

type
  THackForm = class(TCustomForm)
  end;

function AdjustFonts(Form: TCustomForm): Double;
var
  Controls: TObjectList;
  Item: TControlBounds;
  X, Y: Double;
  L: TLabel;
  S: TFontStyles;
  H: Integer;
  C: TColor;
  I: Integer;
begin
  FontRatio(X, Y);
  Result := X;
  Controls := TObjectList.Create;
  try
    THackForm(Form).DesktopFont := True;
    for I := 0 to Form.ComponentCount - 1 do
      if Form.Components[I] is TControl then
        Controls.Add(TControlBounds.Create(Form.Components[I] as TControl));
    for I := 0 to Controls.Count - 1 do
    begin
      Item := Controls[I] as TControlBounds;
      if Supports(Item.Control, IIgnoreResize) then Continue;
      with Item.Bounds do
      begin
        Left := Round(Left * X);
        Top := Round(Top * Y);
        Right := Round(Right * X);
        Bottom := Round(Bottom * Y);
      end;
      Item.Control.BoundsRect := Item.Bounds;
      if Item.Control is TLabel then
      begin
        L := TLabel(Item.Control);
        if L.Font.Handle <> Form.Font.Handle then
        begin
          S := L.Font.Style;
          H := L.Font.Height;
          C := L.Font.Color;
          L.Font.Assign(Form.Font);
          L.Font.Style := S;
          L.Font.Height := Round(H * Y);
          L.Font.Color := C;
        end;
      end;
    end;
  finally
    Controls.Free;
  end;
end;

function AdjustFontBorders(Form: TCustomForm; Control: TControl; Offset: Integer = 8): Double;
var
  X, Y: Double;
begin
   Result := AdjustFonts(Form);
  FontRatio(X, Y);
  Form.ClientWidth := Control.Left + Control.Width + Round(Offset * X);
  Form.ClientHeight := Control.Top + Control.Height + Round(Offset * Y);
end;

procedure AlignCenter(Control: TControl);
var
  P: TWinControl;
begin
  P := Control.Parent;
  if P = nil then Exit;
  Control.Top := Round((P.ClientHeight - Control.Height) / 2);
end;

procedure AlignCenter(ControlA, ControlB: TControl);
var
  A, B: Single;
begin
  A := ControlA.Top + ControlA.Height / 2;
  B := ControlB.Top + ControlB.Height / 2;
  ControlB.Top := Round(ControlB.Top + A - B);
end;

procedure AlignControlText(const Text: string; Margin: Integer; Control: TControl);
var
  P: TWinControl;
  DC: HDC;
  F: HFONT;
  I: Integer;
begin
  P := Control.Parent;
  if (P = nil) or (not P.HandleAllocated) then Exit;
  DC := GetDC(P.Handle);
  F := TNullPanel(P).Font.Handle;
  F := SelectObject(DC, F);
  I := FontWidth(DC, Text);
  SelectObject(DC, F);
  ReleaseDC(P.Handle, DC);
  Control.Left := Margin + I;
end;

procedure FillControlText(const Text: string; Margin: Integer; Control: TControl);
var
  P: TWinControl;
  DC: HDC;
  F: HFONT;
  I, J: Integer;
begin
  P := Control.Parent;
  if (P = nil) or (not P.HandleAllocated) then Exit;
  DC := GetDC(P.Handle);
  F := TNullPanel(P).Font.Handle;
  F := SelectObject(DC, F);
  I := FontWidth(DC, Text);
  SelectObject(DC, F);
  ReleaseDC(P.Handle, DC);
  I := Margin + I;
  J := I - Control.Left;
  Control.Left := I;
  Control.Width := Control.Width - J;
end;

procedure FillControlSpace(ControlA, ControlB: TControl; Margin: Integer);
var
  I, J: Integer;
begin
  I := ControlA.Left + ControlA.Width + Margin;
  J := I - ControlB.Left;
  ControlB.Left := I;
  ControlB.Width := ControlB.Width - J;
end;

var
  TextBoxHeight: Integer;

type
  TTextForm = class(TForm)
  end;

function CalcTextBoxHeight: Integer;
var
  F: TTextForm;
  E: TEdit;
begin
  if TextBoxHeight = 0 then
  begin
    F := TTextForm.CreateNew(Application);
    try
      F.DesktopFont := True;
      E := TEdit.Create(F);
      E.Parent := F;
      F.HandleNeeded;
      E.HandleNeeded;
      TextBoxHeight := E.Height;
    finally
      F.Free;
    end;
  end;
  Result := TextBoxHeight;
end;

function CalcEditHeight(Font: TFont): Integer;
begin
  Result := FontHeight(Font) + 8;
end;

procedure AdjustEditRect(Edit: TEdit; Rect: TRect);
var
  R: TRect;
begin
  R := Rect;
  OffsetRect(R, 3, 3);
  R.Right := Rect.Right - 3;
  R.Bottom := Rect.Bottom - 3;
  Edit.BoundsRect := R;
end;

procedure AdjustEditVerticalRect(Edit: TEdit; Rect: TRect);
var
  R: TRect;
begin
  R := Rect;
  R.Top := Rect.Top + 3;
  R.Bottom := Rect.Bottom - 3;
  R.Left := Edit.Left;
  R.Right := R.Left + Edit.Width;
  Edit.BoundsRect := R;
end;

constructor TComponentNotifier.CreateNotifier(Ref: PComponent; Event: TNotifyEvent);
begin
  inherited Create(nil);
  FRef := Ref;
  FEvent := Event;
end;

destructor TComponentNotifier.Destroy;
begin
  FRef^ := nil;
  inherited Destroy;
end;

procedure TComponentNotifier.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    FEvent(AComponent);
end;

procedure TComponentNotifier.AddFreeNotifier(Component: TComponent);
begin
  if Component <> nil then
    Component.FreeNotification(Self);
end;

procedure CallMethod(Instance: TObject; const MethodName: string);
var
  Method: TMethod;
  InstanceMethod: TInstanceMethod absolute Method;
begin
  Method.Code := Instance.MethodAddress(MethodName);
  Method.Data := Instance;
  if Method.Code <> nil then
    InstanceMethod;
end;

function SaveAnchors(Container: TWinControl): TAnchorDataArray;
var
  I: Integer;
begin
  SetLength(Result, Container.ControlCount);
  for I := 0 to Container.ControlCount - 1 do
  begin
    Result[I].Control := Container.Controls[I];
    with Result[I] do
    begin
      Anchors := Control.Anchors;
      Control.Anchors := [akTop, akLeft];
    end;
  end;
end;

procedure RestoreAnchors(const Data: TAnchorDataArray);
var
  I: Integer;
begin
  for I := Low(Data) to High(Data) do
    with Data[I] do
       Control.Anchors := Anchors;
end;

function SaveVisible(Container: TWinControl): TVisibleDataArray;
var
  I: Integer;
begin
  SetLength(Result, Container.ControlCount);
  for I := 0 to Container.ControlCount - 1 do
  begin
    Result[I].Control := Container.Controls[I];
    with Result[I] do
    begin
      Visible := Control.Visible;
      Control.Visible := False;
    end;
  end;
end;

procedure RestoreVisible(const Data: TVisibleDataArray);
var
  I: Integer;
begin
  for I := Low(Data) to High(Data) do
    with Data[I] do
      if Control = nil then
        Continue
      else
        Control.Visible := Visible;
end;

procedure ScrollToRect(ScrollBox: TScrollBox; Rect: TRect);
var
  NeedsScroll: Boolean;
begin
  with ScrollBox do
  begin
    NeedsScroll := (Rect.Left < 0) or (Rect.Right > ScrollBox.ClientWidth);
    if NeedsScroll then
      if (Rect.Left < 0) or (ClientWidth < WidthOf(Rect)) then
        HorzScrollBar.Position := HorzScrollBar.Position + Rect.Left
      else
        HorzScrollBar.Position := HorzScrollBar.Position + Rect.Left - ClientWidth + WidthOf(Rect);;
    NeedsScroll := (Rect.Top < 0) or (Rect.Bottom > ScrollBox.ClientHeight);
    if NeedsScroll then
      if (Rect.Top < 0) or (ClientHeight < HeightOf(Rect)) then
        VertScrollBar.Position := VertScrollBar.Position + Rect.Top
      else
        VertScrollBar.Position := VertScrollBar.Position + Rect.Top - ClientHeight + HeightOf(Rect);;
  end;
end;

procedure ScrollToRect(ScrollBox: TScrollBox; Control: TControl; Rect: TRect);
begin
  with ScrollBox do
  begin
    Dec(Rect.Left, HorzScrollBar.Margin);
    Inc(Rect.Right, HorzScrollBar.Margin);
    Dec(Rect.Top, VertScrollBar.Margin);
    Inc(Rect.Bottom, VertScrollBar.Margin);
    Rect.TopLeft := ScreenToClient(Control.ClientToScreen(Rect.TopLeft));
    Rect.BottomRight := ScreenToClient(Control.ClientToScreen(Rect.BottomRight));
    if Rect.Left < 0 then
      with HorzScrollBar do Position := Position + Rect.Left
    else if Rect.Right > ClientWidth then
    begin
      if Rect.Right - Rect.Left > ClientWidth then
        Rect.Right := Rect.Left + ClientWidth;
      with HorzScrollBar do Position := Position + Rect.Right - ClientWidth;
    end;
    if Rect.Top < 0 then
      with VertScrollBar do Position := Position + Rect.Top
    else if Rect.Bottom > ClientHeight then
    begin
      if Rect.Bottom - Rect.Top > ClientHeight then
        Rect.Bottom := Rect.Top + ClientHeight;
      with VertScrollBar do Position := Position + Rect.Bottom - ClientHeight;
    end;
  end;
end;

constructor TControlTemplate.Create(Template: TWinControl);
var
  I: Integer;
begin
  FParent := Template;
  FTemplate := Template;
  FControls := TList.Create;
  for I := 0 to Template.ControlCount - 1 do
    FControls.Add(Template.Controls[I]);
end;

destructor TControlTemplate.Destroy;
begin
  FControls.Free;
end;

procedure TControlTemplate.Insert(Parent: TWinControl);
var
  Control: TControl;
  AnchorsProp: PPropInfo;
  Anchors: TAnchors;
  X1, X2, Y1, Y2: Integer;
  I: Integer;
begin
  if FParent = Parent then
    Exit;
  FParent := Parent;
  if FParent = nil then
    FParent := FTemplate;
  for I := 0 to FControls.Count - 1 do
  begin
    Control := TControl(FControls[I]);
    if Control.Parent = FParent then Continue;
    AnchorsProp := GetPropInfo(Control, 'Anchors', [tkSet]);
    if AnchorsProp <> nil then
    begin
      Byte(Anchors) := GetOrdProp(Control, AnchorsProp);
      X1 := Control.Left;
      X2 := Control.Parent.ClientWidth - (Control.Left + Control.Width);
      Y1 := Control.Top;
      Y2 := Control.Parent.ClientHeight - (Control.Top + Control.Height);
    end
    else
    begin
      Anchors := [];
      X1 := 0;
      X2 := 0;
      Y1 := 0;
      Y2 := 0;
    end;
    Control.Parent := FParent;
    if [akLeft, akRight] * Anchors  = [akLeft, akRight] then
    begin
      Control.Left := X1;
      Control.Width := FParent.ClientWidth - (X1 + X2);
    end
    else if [akLeft] * Anchors  = [akLeft] then
      Control.Left := X1
    else if [akRight] * Anchors  = [akRight] then
      Control.Left := FParent.ClientWidth - (Control.Width + X2);
    if [akTop, akBottom] * Anchors  = [akTop, akBottom] then
    begin
      Control.Top := Y1;
      Control.Height := FParent.ClientHeight - (Y1 + Y2);
    end
    else if [akTop] * Anchors  = [akTop] then
      Control.Top := Y1
    else if [akBottom] * Anchors  = [akBottom] then
      Control.Top := FParent.ClientHeight - (Control.Height + Y2);
  end;
end;

{ TCompositeLink }

constructor TCompositeLink.Create(Event: TCompositeEvent);
begin
  inherited Create;
  FEvent := Event;
end;

procedure TCompositeLink.Composite(const Surface: TFastBitmap);
begin
  if Assigned(FEvent) then FEvent(Surface);
end;

{ TNullPanel }

constructor TNullPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls] - [csSetCaption];
  Width := 200;
  Height := 200;
end;

procedure TNullPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := 0; // WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TNullPanel.Resize;
begin
  inherited Resize;
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TNullPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if Message.DC <> 0 then
    FillRectColor(Message.DC, ClientRect, Color);
  if csDesigning in ComponentState then
    FillRectOutline(Message.DC, ClientRect, clBtnShadow, psDot);
  Message.Result := 1;
end;

{ TPaintPanel }

constructor TPaintPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // DoubleBuffered := True;
  ControlStyle := ControlStyle + [csDoubleClicks, csAcceptsControls,
    csCaptureMouse, csClickEvents] - [csSetCaption];
  Width := 160;
  Height := 160;
  FAllowEraseBkgnd := True;
end;

destructor TPaintPanel.Destroy;
begin
  if IsFastBitmap(FBitmap) then
    DestroyBitmap(FBitmap);
  DestroyFastBitmap(FCompositeBitmap);
  inherited Destroy;
end;

procedure TPaintPanel.DisposeBuffer;
begin
  FBufferChanged := True;
  if IsFastBitmap(FBitmap) then
    DestroyBitmap(FBitmap);
end;

procedure TPaintPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
end;

procedure TPaintPanel.AddCompositeLink(Link: ICompositeLink);
begin
  if FCompositeLinks = nil then
    FCompositeLinks := TInterfaceList.Create;
  if FCompositeLinks.IndexOf(Link) < 0 then
    FCompositeLinks.Add(Link);
end;

procedure TPaintPanel.RemoveCompositeLink(Link: ICompositeLink);
begin
  if FCompositeLinks <> nil then
    FCompositeLinks.Remove(Link);
  if FCompositeLinks.Count = 0 then
  begin
    FCompositeLinks := nil;
    DestroyFastBitmap(FCompositeBitmap);
  end;
end;

procedure TPaintPanel.Invalidate;
var
  W: TWinControl;
  H: HWND;
  I: Integer;
begin
  FBufferChanged := True;
  inherited Invalidate;
  if DoubleBuffered and HandleAllocated then
  begin
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
end;

procedure TPaintPanel.Revalidate;
begin
  if not (csDestroying in ComponentState) then
    Perform(CM_INVALIDATE, 0, 0);
end;

procedure TPaintPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if TabStop and (Button = mbLeft) then
    SetFocus;
end;

procedure TPaintPanel.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self)
  else if not ParentBackground then
    FillRectColor(Canvas.Handle, ClientRect, Color);
end;

procedure TPaintPanel.Resize;
begin
  Invalidate;
end;

procedure TPaintPanel.VisibleChanging;
begin
  if IsFastBitmap(FBitmap) then
    DestroyBitmap(FBitmap);
  inherited VisibleChanging;
end;

procedure TPaintPanel.CreateBitmap(var Bitmap: TFastBitmap; W, H: Integer);
begin
  FBufferChanged := True;
  Bitmap := CreateFastBitmap(W, -H, pd32);
  if Assigned(FOnCreateBitmap) then
    FOnCreateBitmap(Self);
end;

procedure TPaintPanel.DestroyBitmap(var Bitmap: TFastBitmap);
begin
  DestroyFastBitmap(Bitmap);
  if Assigned(FOnDestroyBitmap) then
    FOnDestroyBitmap(Self);
end;

procedure TPaintPanel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TPaintPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TPaintPanel.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPaintPanel.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TPaintPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  Wnd: HWND;
  R: TRect;
begin
  if DoubleBuffered then
  begin
    Message.Result := 1;
    if (Message.DC = FBitmap.DC) or (FBitmap.DC = 0) then
      Exit;
    {if FBitmap.DC = 0 then
    begin
      //Invalidate;
      Exit;
    end;}
    Wnd := WindowFromDC(Message.DC);
    if Wnd = Handle then
      Exit;
    {if Wnd <> 0 then
    begin
      GetWindowRect(Wnd, R);
      P := ScreenToClient(R.TopLeft);
      R.Right := P.X + WidthOf(R);
      R.Left := P.X;
      R.Bottom := P.Y + HeightOf(R);
      R.Top := P.Y;
    end
    else}
      R := ClientRect;
    BitBlt(Message.DC, R.Left, R.Top, WidthOf(R), HeightOf(R), FBitmap.DC,
          R.Left, R.Top, SRCCOPY);
    if csDesigning in ComponentState then
      FillRectOutline(Message.DC, ClientRect, clBtnShadow, psDot);
  end
  else if FAllowEraseBkgnd then
    inherited
  else
    Message.Result := 1;
end;

procedure TPaintPanel.WMPaint(var Message: TWMPaint);
var
  W, H: Integer;
  PS: TPaintStruct;
  DC: HDC;
  L: ICompositeLink;
  I: Integer;
begin
  W := Width;
  H := Height;
  if (W < 1) or (H < 1) then
  begin
    if IsFastBitmap(FBitmap) then
      DestroyBitmap(FBitmap);
    Exit;
  end
  else if (not DoubleBuffered) and IsFastBitmap(FBitmap) then
    DestroyBitmap(FBitmap);
  if not DoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    if (W <> FBitmap.Width) or (H <> FBitmap.Height) then
    begin
      if IsFastBitmap(FBitmap) then
        DestroyBitmap(FBitmap);
      CreateBitmap(FBitmap, W, H);
    end;
    DC := BeginPaint(Handle, PS);
    if FBufferChanged or (ControlCount > 0) then
    begin
      FBufferChanged := False;
      Perform(WM_ERASEBKGND, FBitmap.DC, FBitmap.DC);
      Message.DC := FBitmap.DC;
      WMPaint(Message);
      Message.DC := 0;
    end;
    if (FCompositeLinks <> nil) and (FCompositeLinks.Count > 0) then
    begin
      if (FCompositeBitmap.Width <> FBitmap.Width) or (FCompositeBitmap.Height
        <> FBitmap.Height) then
      begin
        DestroyFastBitmap(FCompositeBitmap);
        FCompositeBitmap := CreateFastBitmap(FBitmap.Width, FBitmap.Height, pd32);
      end;
      BitBlt(FCompositeBitmap.DC, 0, 0, FCompositeBitmap.Width,
        FCompositeBitmap.Height, FBitmap.DC, 0, 0, SRCCOPY);
      for I := 0 to FCompositeLinks.Count - 1 do
      begin
        L := FCompositeLinks[I] as ICompositeLink;
        L.Composite(FCompositeBitmap);
      end;
      BitBlt(DC, 0, 0, FCompositeBitmap.Width, FCompositeBitmap.Height,
        FCompositeBitmap.DC, 0, 0, SRCCOPY);
    end
    else
      BitBlt(DC, 0, 0, FBitmap.Width, FBitmap.Height, FBitmap.DC, 0, 0, SRCCOPY);
    EndPaint(Handle, PS);
  end;
end;

procedure TGraphicBufferedControl.WMPaint(var Message: TWMPaint);
var
  B: TFastBitmap;
begin
  if Message.DC <> 0 then
  begin
    Canvas.Lock;
    B := CreateFastBitmap(Width, Height, pd32);
    try
      Canvas.Handle := B.DC;
      try
        Paint;
      finally
        Canvas.Handle := 0;
      end;
      BaseTypes.AlphaDraw(Message.DC, 0, 0, B);
    finally
      DestroyFastBitmap(B);
      Canvas.Unlock;
    end;
  end;
end;

{ TEmptyWindow }

constructor TEmptyWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csOpaque, csDoubleClicks, csReplicatable];
end;

procedure TEmptyWindow.Paint;
begin
  FillRectColor(Canvas.Handle, GetClientRect, Color);
end;

{ TCustomFramedWindow }

constructor TCustomFramedWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  ParentColor := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Height := 96;
  Width := 96;
end;

procedure TCustomFramedWindow.Activate(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if HandleAllocated and (BorderStyle = bsSingle) and ThemePainter.Enabled then
    begin
      Invalidate;
      RedrawWindow(Handle, nil, 0, RDW_ERASE  or RDW_FRAME or RDW_INTERNALPAINT  or RDW_INVALIDATE);
    end;
  end;
end;

procedure TCustomFramedWindow.CMFocusChanged(var Message: TCMFocusChanged);
var
  A: Boolean;
begin
  A := Message.Sender <> nil;
  if A then A := (Message.Sender = Self) or (IsChild(Handle, Message.Sender.Handle));
  Activate(A);
end;

procedure TCustomFramedWindow.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomFramedWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style and (not WS_BORDER);
    if FBorderStyle = bsSingle then
      ExStyle := Params.ExStyle or WS_EX_CLIENTEDGE;
  end;
end;

function MouseInControl(Wnd: HWND): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  GetWindowRect(Wnd, R);
  GetCursorPos(P);
  Result := PtInRect(R, P);
end;

procedure TCustomFramedWindow.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  W: HWND;
  R: TRect;
  S: TDrawState;
begin
  inherited;
  if BorderStyle = bsSingle then
  begin
    DC := GetWindowDC(Handle);
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);
    S := [];
    if not Enabled then Include(S, dsDisabled);
    { if MouseInControl(Handle) then Include(S, dsHot); }
    W := GetFocus;
    if (W = Handle) or IsChild(Handle, W) then Include(S, dsFocused);
    InflateRect(R, -2, -2);
    SelectClipRect(DC, R, RGN_DIFF);
    InflateRect(R, 2, 2);
    DrawThemeEdit(DC, R, S);
    SelectClipRgn(DC, 0);
    ReleaseDC(Handle, DC);
  end;
end;

function TCustomFramedWindow.GetInternalWidth: Integer;
begin
  Result := Width;
  if FBorderStyle = bsSingle then
    Dec(Result, 4);
end;

procedure TCustomFramedWindow.SetInternalWidth(Value: Integer);
begin
  if FBorderStyle = bsSingle then
    Inc(Value, 4);
  Width := Value;
end;

function TCustomFramedWindow.GetInternalHeight: Integer;
begin
  Result := Height;
  if FBorderStyle = bsSingle then
    Dec(Result, 4);
end;

procedure TCustomFramedWindow.SetInternalHeight(Value: Integer);
begin
  if FBorderStyle = bsSingle then
    Inc(Value, 4);
  Height := Value;
end;

{ TFramedImagesWindow }

constructor TFramedImagesWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TFramedImagesWindow.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TFramedImagesWindow.ImageListChange(Sender: TObject);
begin
  Repaint;
end;

procedure TFramedImagesWindow.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

procedure TFramedImagesWindow.SetImages(Value: TCustomImageList);
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
    Repaint;
  end;
end;

procedure TFramedImagesWindow.SetImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Repaint;
  end;
end;

{ TFrameEdit }

constructor TFrameEdit.Create(Collection: TCollection);
var
  O: TOwnedCollection absolute Collection;
  P: TWinControl;
begin
  inherited Create(Collection);
  if (Collection is TOwnedCollection) and (O.Owner is TWinControl) then
  begin
    P := TWinControl(O.Owner);
    FEdit := TEdit.Create(P);
    FEdit.BorderStyle := bsNone;
    FEdit.ParentColor := True;
    FEdit.ParentFont := True;
    FEdit.Parent := P;
  end;
end;

destructor TFrameEdit.Destroy;
begin
  inherited Destroy;
  FEdit.Free;
end;

{ TFrameEdits }

constructor TFrameEdits.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TFrameEdit);
end;

function TFrameEdits.Add: TFrameEdit;
begin
  BeginUpdate;
  try
    Result := TFrameEdit(inherited Add);
  finally
    EndUpdate;
  end;
end;

function TFrameEdits.FindItemID(ID: Integer): TFrameEdit;
begin
  Result := TFrameEdit(inherited FindItemID(ID));
end;

function TFrameEdits.Insert(Index: Integer): TFrameEdit;
begin
  Result := TFrameEdit(inherited Insert(Index));
end;

procedure TFrameEdits.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TCustomFramedEditWindow then
    TCustomFramedEditWindow(GetOwner).AlignEdits;
end;

function TFrameEdits.Get(Index: Integer): TFrameEdit;
begin
  Result := TFrameEdit(GetItem(Index));
end;

procedure TFrameEdits.Put(Index: Integer; Value: TFrameEdit);
begin
  SetItem(Index, Value);
end;

{ TCustomFramedEditWindow }

constructor TCustomFramedEditWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoHeight := True;
  FButtonHot := -1;
  FButtonDown := -1;
  FEdits := TFrameEdits.Create(Self);
  Width := 120;
  AdjustHeight;
end;

destructor TCustomFramedEditWindow.Destroy;
begin
  FEdits.Free;
  inherited Destroy;
end;

procedure TCustomFramedEditWindow.AlignEdits;
var
  E: TEdit;
  H: Integer;
  I: Integer;
begin
  for I := 0 to FEdits.Count - 1 do
  begin
    E := FEdits[I].Edit;
    H := FontHeight(Font);
    E.Top := (InternalHeight - H) div 2;
    E.Height := H;
    E.TabStop := True;
    E.TabOrder := I;
  end;
end;

procedure TCustomFramedEditWindow.AdjustHeight;
begin
  Height := CalcEditHeight(Font);
  AlignEdits;
end;

procedure TCustomFramedEditWindow.ButtonClick(Index: Integer);
begin
end;

procedure TCustomFramedEditWindow.ButtonHover(Index: Integer);
begin
end;

procedure TCustomFramedEditWindow.ButtonClear;
begin
  FButtons := nil;
  FButtonHot := -1;
  FButtonDown := -1;
end;

function TCustomFramedEditWindow.ButtonDefine(Button: TButtonRect): Integer;
var
  I: Integer;
begin
  I := Length(FButtons);
  Result := I;
  SetLength(FButtons, I + 1);
  FButtons[I] := Button;
  FButtonHot := -1;
  FButtonDown := -1;
end;

procedure TCustomFramedEditWindow.ButtonDraw(Index: Integer; Rect: TRect; State: TDrawState);
begin
end;

function TCustomFramedEditWindow.ButtonFromPoint(P: TPoint): Integer;
var
  R: TRect;
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FButtons) - 1 do
  begin
    R := GetButtonRect(I);
    if PtInRect(R, P) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TCustomFramedEditWindow.GetButtonCount: Integer;
begin
  Result := Length(FButtons);
end;

function TCustomFramedEditWindow.GetButtonRect(Index: Integer): TButtonRect;
begin
  Result := FButtons[Index];
end;

procedure TCustomFramedEditWindow.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  HotChanged: Boolean;
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FMouseDown := True;
    I := ButtonFromPoint(Point(X, Y));
    if I > -1 then
    begin
      HotChanged := FButtonHot <> I;
      FButtonHot := I;
      FButtonDown := I;
      Invalidate;
      if HotChanged then
        ButtonHover(FButtonHot);
    end;
  end;
end;

procedure TCustomFramedEditWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHot: Integer;
  P: TPoint;
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if Length(FButtons) > 0 then
  begin
    if FMouseDown and (FButtonDown < 0) then Exit;
    NewHot := -1;
    P := Point(X, Y);
    if FButtonDown > -1 then
    begin
      if PtInRect(GetButtonRect(FButtonDown), P) then
        NewHot := FButtonDown;
    end
    else for I := 0 to Length(FButtons) - 1 do
      if PtInRect(GetButtonRect(I), P) then
      begin
        NewHot := I;
        Break;
      end;
    if NewHot <> FButtonHot then
    begin
      FButtonHot := NewHot;
      Invalidate;
      ButtonHover(FButtonHot);
    end;
  end;
end;

procedure TCustomFramedEditWindow.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  R: TRect;
  P: TPoint;
  I: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FMouseDown := False;
    if FButtonDown > -1 then
    begin
      I := FButtonDown;
      FButtonDown := -1;
      Invalidate;
      P := Point(X, Y);
      R := GetButtonRect(I);
      if PtInRect(R, P) then
        ButtonClick(I);
    end;
  end;
end;

procedure TCustomFramedEditWindow.Paint;
var
  S: TDrawState;
  I: Integer;
begin
  for I := 0 to ButtonCount - 1 do
  begin
    S := [];
    if not Enabled then
      S := [dsDisabled]
    else if Focused then 
      S := [dsFocused];
    if I = FButtonHot then
      Include(S, dsHot);
    if I = FButtonDown then
      Include(S, dsPRessed);
    ButtonDraw(I, GetButtonRect(I), S);
  end;
end;

procedure TCustomFramedEditWindow.SetAutoHeight(Value: Boolean);
begin
  if Value <> FAutoHeight then
  begin
    FAutoHeight := Value;
    if FAutoHeight then
      AdjustHeight;
  end;
end;

procedure TCustomFramedEditWindow.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FAutoHeight then
    AdjustHeight;
end;

procedure TCustomFramedEditWindow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (not FMouseDown) and (FButtonHot > -1) then
  begin
    FButtonHot := -1;
    Invalidate;
  end;
end;


{ TInputWindow }

constructor TInputWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csClickEvents, csCaptureMouse, csDoubleClicks];
  TabStop := True;
  ParentColor := False;
  ParentFont := True;
  Color := clWindow;
end;

{procedure TInputWindow.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  R: TRect;
begin
  R := GetClientRect;
  InflateRect(R, FClientBorder, FClientBorder);
  if Focused then
    FillRectColor(Msg.DC, R, clHighlight)
  else
    FillRectColor(Msg.DC, R, Color);
  Msg.Result := 1;
end;}

procedure TInputWindow.DoEnter;
begin
  inherited DoEnter;
  Repaint;
end;

procedure TInputWindow.DoExit;
begin
  inherited DoExit;
  Repaint;
end;

procedure TInputWindow.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    SetFocus;
end;

function TInputWindow.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  InflateRect(Result, -FClientBorder, -FClientBorder);
end;

procedure TInputWindow.SetClientBorder(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> FClientBorder then
  begin
    FClientBorder := Value;
    Realign;
    Repaint;
  end;
end;

function FindTaskBar(out Rect: TRect): Integer;
var
  AppBar: TAppBarData;
begin
  FillChar(Rect, SizeOf(Rect), #0);
  AppBar.cbSize := SizeOf(AppBar);
  if SHAppBarMessage(ABM_GETTASKBARPOS, AppBar) = 0 then
    Result := -1
  else
  begin
    Result := AppBar.uEdge;
    Rect := AppBar.rc;
  end;
end;

procedure ShowTrayForm(Form: TCustomForm);
const
  Offset = 8;
var
  R: TRect;
begin
  if not Form.Visible then
  begin
    case FindTaskBar(R) of
      ABE_LEFT:
        begin
          Form.Top := R.Bottom - Form.Height - Offset;
          Form.Left := R.Right + Offset;
        end;
      ABE_TOP:
        begin
          Form.Top := R.Bottom + Offset;
          Form.Left := R.Right - Form.Width - Offset;
        end;
      ABE_RIGHT:
        begin
          Form.Top := R.Bottom - Form.Height - Offset;
          Form.Left := R.Left - Form.Width - Offset;
        end;
      ABE_BOTTOM:
        begin
          Form.Top := R.Top- Form.Height - Offset;
          Form.Left := R.Right - Form.Width - Offset;
        end;
    end;
    Form.Show;
  end;
  Application.BringToFront;
end;

function GraphicsFromCanvasImplementation(Canvas: TCanvas): IGdiGraphics;
begin
  Result := NewGraphics(Canvas.Handle);
end;

var
  Initialized: Boolean;
  InternalQueryInterface: function(himl: HIMAGELIST; const IID: TGUID; out Obj): HRESULT; stdcall;

function LoadQueryInterface: Boolean;
var
  Module: HMODULE;
begin
  if Initialized then
    Result :=  @InternalQueryInterface <> nil
  else
  begin
    Initialized := True;
    Module := LoadLibrary(comctl32);
    @InternalQueryInterface := GetProcAddress(Module, 'HIMAGELIST_QueryInterface');
    FreeLibrary(Module);
    Result :=  @InternalQueryInterface <> nil
  end;
end;

function ImageListQueryInterface(ImageList: TCustomImageList; const IID: TGUID; out Obj): HRESULT;
begin
  if LoadQueryInterface then
    Result := InternalQueryInterface(ImageList.Handle, IID, Obj)
  else
    Result := E_FAIL;
end;

procedure HandleExternalException(Sender: TObject);
var
  Title: array[0..63] of Char;
  Buffer: array[0..1023] of Char;
begin
  if (ExceptObject is Exception) or (@ErrorBoxProc = nil) then
    Application.HandleException(Sender)
  else
  begin
    if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    LoadString(FindResourceHInstance(HInstance), PResStringRec(@SExceptTitle).Identifier,
      Title, SizeOf(Title));
    ErrorBoxProc(Title, Buffer);
  end;
end;

{ TPaintImage }

constructor TPaintImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque] + [csClickEvents];
end;

procedure TPaintImage.Paint;
begin
  if Assigned(OnPaint) then
    FOnPaint(Self)
  else
  begin
    if csDesigning in ComponentState then
      FillRectOutline(Canvas.Handle, ClientRect, clBtnShadow, psDot);
    if  Images <> nil then
      Images.Draw(Canvas, 0, 0, ImageIndex);
  end;
end;

initialization
  LoadQueryInterface;
end.
