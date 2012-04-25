
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit BtnEdit;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, CommCtrl, Controls, StdCtrls, Classes, Graphics, Forms, ImgList,
  BaseTypes, GraphTools, ProviderTools, FormTools, SysUtils, PopCtrls, StrTools,
  BtnCtrls, XMLObjects, XMLParser
  {$IFDEF GDIPLUS}, GdiPlus, GdiIntf{$ENDIF};

{ TCustomButtonEdit class }

type
  TButtonEditStyle = (beStandard, beClose, beEllipse, beQuestion, beOwnerDrawn);

  TOwnerDrawEvent = procedure (Control: TWinControl; Rect: TRect;
    State: TOwnerDrawState; var DefaultDraw: Boolean) of object;
  TSearchEvent = procedure (Sender: TObject; Index: Integer) of object;

  TCustomButtonEdit = class(TInputWindow)
  private
    FAutoHeight: Boolean;
    FButtonDown: Boolean;
    FButtonHot: Boolean;
    FButtonVisible: Boolean;
    FTextChanged: Boolean;
    FChangeLink: TChangeLink;
    FDefEditProc: TWndMethod;
    FEdit: TEdit;
    FFlat: Boolean;
    FFocused: Boolean;
    FGlyph: TBitmap;
    FGlyphAssigned: Boolean;
    FImageIndex: Integer;
    FImages: TCustomImageList;
    FNextControl: TWinControl;
    FTitle: string;
    FSearchStrings: TStrings;
    FStyle: TButtonEditStyle;
    FOnButtonClick: TNotifyEvent;
    FOnButtonPress: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnCustomDraw: TOwnerDrawEvent;
    FOnPlaceEdit: TCalcRectEvent;
    FOnSearch: TSearchEvent;
    FOnTab: TNotifyEvent;
    FWantTabs: Boolean;
    procedure EditChange(Sender: TObject);
    procedure EditProc(var Message: TMessage);
    procedure ImagesChange(Sender: TObject);
    procedure SetButtonVisible(Value: Boolean);
    function GetEditHandle: HWND;
    procedure SetFlat(Value: Boolean);
    procedure AssignGlyph;
    function  GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    procedure SetImageIndex(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetStyle(Value: TButtonEditStyle);
    { TCustomButtonEdit.TEdit  }
    function GetAutoSelect: Boolean;
    procedure SetAutoSelect(Value: Boolean);
    function GetCanUndo: Boolean;
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(Value: TEditCharCase);
    function GetHideSelection: Boolean;
    procedure SetHideSelection(Value: Boolean);
    function GetMaxLength: Integer;
    procedure SetMaxLength(Value: Integer);
    function GetModified: Boolean;
    procedure SetModified(Value: Boolean);
    function GetOEMConvert: Boolean;
    procedure SetOEMConvert(Value: Boolean);
    function GetPasswordChar: Char;
    procedure SetPasswordChar(Value: Char);
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetSelLength: Integer;
    procedure SetSelLength(Value: Integer);
    function GetSelStart: Integer;
    procedure SetSelStart(Value: Integer);
    function GetSelText: string;
    procedure SetSelText(Value: string);
    function GetTabStop: Boolean;
    procedure SetTabStop(Value: Boolean);
    procedure SetWantTabs(Value: Boolean);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMParentNotify(var Message: TWMParentNotify); message WM_PARENTNOTIFY;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    procedure AdjustSize; override;
    procedure AdjustEdit; virtual;
    procedure AdjustHeight; virtual;
    procedure DoButtonPress; dynamic;
    procedure DoButtonClick; dynamic;
    procedure DoChange; dynamic;
    procedure DoPlaceEdit(var Rect: TRect); dynamic;
    procedure DrawButtonGlyph(Rect: TRect); virtual;
    procedure ParseInput; virtual;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    function GetButtonRect: TRect; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    property InnerEdit: TEdit read FEdit;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight default True;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ButtonDown: Boolean read FButtonDown;
    property ButtonHot: Boolean read FButtonHot;
    property ButtonVisible: Boolean read FButtonVisible write SetButtonVisible;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored FGlyphAssigned;
    property TextChanged: Boolean read FTextChanged;
    property EditHandle: HWND read GetEditHandle;
    property Flat: Boolean read FFlat write SetFlat;
    property NextControl: TWinControl read FNextControl;
    property WantTabs: Boolean read FWantTabs write SetWantTabs;
    property SearchStrings: TStrings read FSearchStrings write FSearchStrings;
    property Style: TButtonEditStyle read FStyle write SetStyle;
    property TabStop: Boolean read GetTabStop write SetTabStop;
    property Title: string read FTitle write FTitle;
    property OnCustomDraw: TOwnerDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnButtonPress: TNotifyEvent read FOnButtonPress write FOnButtonPress;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnPlaceEdit: TCalcRectEvent read FOnPlaceEdit write FOnPlaceEdit;
    property OnSearch: TSearchEvent read FOnSearch write FOnSearch;
    property OnTab: TNotifyEvent read FOnTab write FOnTab;
    { TCustomButtonEdit.TEdit }
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property CanUndo: Boolean read GetCanUndo;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection default True;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property Modified: Boolean read GetModified write SetModified;
    property OEMConvert: Boolean read GetOEMConvert write SetOEMConvert default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateNextControl;
    { TCustomButtonEdit.TEdit }
    procedure Clear;
    procedure ClearSelection;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure Undo;
    procedure ClearUndo;
    function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    procedure SelectAll;
    procedure SetSelTextBuf(Buffer: PChar);
  end;

{ TButtonEdit }

  TButtonEdit = class(TCustomButtonEdit)
  public
    property ButtonDown;
    property Canvas;
    property NextControl;
    property SearchStrings;
    property TextChanged;
  published
    property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property Flat;
    property Glyph;
    property HideSelection;
    property Images;
    property ImageIndex;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Title;
    property Text;
    property Visible;
    property WantTabs;
    property OnButtonPress;
    property OnButtonClick;
    property OnChange;
    property OnClick;
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
    property OnPlaceEdit;
    property OnSearch;
    property OnStartDock;
    property OnStartDrag;
    property OnTab;
  end;

{ TPopupEdit }

  TPopupEdit = class(TCustomButtonEdit)
  private
    FPopupForm: TCustomPopupForm;
    FOnPopup: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FPopupHeight: Integer;
    function GetSizeable: Boolean;
    procedure SetSizeable(const Value: Boolean);
    procedure SetPopupHeight(Value: Integer);
  protected
    procedure DoButtonClick; override;
    procedure RequestPopup;
    function CreatePopup: TCustomPopupForm; virtual; abstract;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DoPopup; virtual;
    procedure DoCancel(Sender: TObject); virtual;
    procedure DoSelect(Sender: TObject); virtual;
    property Sizeable: Boolean read GetSizeable write SetSizeable;
    property PopupForm: TCustomPopupForm read FPopupForm;
    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  public
    constructor Create(AOwner: TComponent); override;
    procedure RealizeWidth;
    property PopupHeight: Integer read FPopupHeight write SetPopupHeight;
    procedure Popup;
    procedure Select;
    procedure Cancel;
  end;

{ TDatePopupEdit }

function ConvertDate(const Date: string; var Formatted: string; Default: TDate): TDate;

type
  TDatePopupEdit = class(TPopupEdit)
  private
    FPopupDateForm: TPopupDateForm;
    FAllowBlank: Boolean;
    function GetDate: TDate;
    procedure SetDate(const Value: TDate);
    function GetDateString: string;
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure DoSelect(Sender: TObject); override;
    procedure ParseInput; override;
  public
    constructor Create(AOwner: TComponent); override;
    property DateString: string read GetDateString;
  published
    property AllowBlank: Boolean read FAllowBlank write FAllowBlank;
    property Date: TDate read GetDate write SetDate;
    property BorderStyle;
    property Flat;
    property Text;
    property OnChange;
  end;

{ TListEdit }

  TListEdit = class(TPopupEdit)
  private
    FPopupListBox: TPopupListForm;
    function GetDisplayColumn: Integer;
    procedure SetDisplayColumn(Value: Integer);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure DoSelect(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SearchStrings;
    property DisplayColumn: Integer read GetDisplayColumn write SetDisplayColumn;
    property Canvas;
  published
    property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Ctl3D;
    property Font;
    property Flat;
    property HideSelection;
    property Images;
    property ImageIndex;
    property Items: TStrings read GetItems write SetItems;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property PasswordChar;
    property ParentFont;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property Sizeable;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonPress;
    property OnButtonClick;
    property OnCancel;
    property OnChange;
    property OnClick;
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
    property OnPlaceEdit;
    property OnSearch;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TImageListEdit }

  TImageListEdit = class(TPopupEdit)
  private
    FDisplayCount: Integer;
    FPopupImageList: TPopupImageListForm;
    FImageIndex: Integer;
    procedure ImageClick(Sender: TObject);
    procedure ImageSelectItem(Sender: TObject);
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
  protected
    procedure CreateWnd; override;
    function CreatePopup: TCustomPopupForm; override;
    procedure DoPopup; override;
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled;
    property DisplayCount: Integer read FDisplayCount write FDisplayCount;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property Images: TCustomImageList read GetImages write SetImages;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnEnter;
    property OnExit;
  end;

{ TCheckListEdit }

  TCheckListEdit = class(TPopupEdit)
  private
    FPopupCheckList: TPopupCheckListForm;
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
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure DoClickCheck(Sender: TObject);
    procedure DoClickItem(Sender: TObject);
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property CheckText: string read GetCheckText;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SearchStrings;
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  published
    property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Ctl3D;
    property Flat: Boolean read GetFlat write SetFlat;
    property Font;
    property HideSelection;
    property Images;
    property ImageIndex;
    property Items: TStrings read GetItems write SetItems;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property ShowHint;
    property StatusText: string read GetStatusText write SetStatusText;
    property Sizeable: Boolean read GetSizeable write SetSizeable;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonPress;
    property OnButtonClick;
    property OnCancel;
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
    property OnPlaceEdit;
    property OnSearch;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TImageEditItem }

  TImageEditItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FIndent: Integer;
    FSelectedIndex: Integer;
    FText: string;
    procedure SetImageIndex(const Value: Integer);
    procedure SetIndent(const Value: Integer);
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetText(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Indent: Integer read FIndent write SetIndent;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Text: string read FText write SetText;
  end;

{ TImageEditItems }

  TImageEditItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TImageEditItem;
    procedure Put(Index: Integer; const Value: TImageEditItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TImageEditItem;
    function FindItemID(ID: Integer): TImageEditItem;
    function Insert(Index: Integer): TImageEditItem;
    property Items[Index: Integer]: TImageEditItem read Get write Put;
  end;

{ TImageEdit }

  TImageEdit = class(TCustomButtonEdit)
  private
    FImageList: TImageList;
    FItems: TImageEditItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TPathEditBar }

  TPathEditBar = class(TCustomFramedEditWindow)
  private
    FAutoNext: Boolean;
    FAutoPopup: Boolean;
    FButtonWidth: Integer;
    FDoubleClicked: Boolean;
    FData: IDocument;
    FPath: IInterfaceList;
    FEdit: TEdit;
    FEditing: Boolean;
    FImageWidth: Integer;
    FButtonIndex: Integer;
    FPopupItems: TPopupScrollingMenu;
    FFolderItems: TPopupScrollingMenu;
    FImageColor: TColor;
    FDownImage: TAlphaImage;
    FRightImage: TAlphaImage;
    FScrollButton: TImageSpeedButton;
    FShowButtons: Boolean;
    FShowIcons: Boolean;
    FOnButtonDraw: TDrawIndexEvent;
    procedure EditExit(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ScrollButtonClick(Sender: TObject);
    procedure ScrollButtonDraw(Control: TControl; Rect: TRect;
      DrawState: TDrawState; var DefaultDraw: Boolean);
    procedure ScrollButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpdatePath;
    function GetButtonNode(Index: Integer): INode;
    procedure SetButtonWidth(Value: Integer);
    procedure SetShowButtons(Value: Boolean);
    procedure SetShowIcons(Value: Boolean);
    procedure SetEditing(Value: Boolean);
  protected
    procedure AlignEdits; override;
    function GetButtonRect(Index: Integer): TButtonRect; override;
    function CanDropDown(Index: Integer): Boolean; virtual;
    function Convert: string; virtual;
    procedure DblClick; override;
    procedure DataSelect(Node: INode); overload;
    procedure DataDefine(const Data: string);
    procedure EditingChange; virtual;
    procedure ButtonClick(Index: Integer); override;
    procedure ButtonHover(Index: Integer); override;
    procedure ButtonDraw(Index: Integer; Rect: TRect; State: TDrawState); override;
    procedure FolderSelect(Sender: TObject); virtual;
    procedure PopupSelect(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property Data: IDocument read FData;
    property ButtonNode[Index: Integer]: INode read GetButtonNode;
    property ButtonIndex: Integer read FButtonIndex;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property FolderItems: TPopupScrollingMenu read FFolderItems;
    property PopupItems: TPopupScrollingMenu read FPopupItems;
    property AutoNext: Boolean read FAutoNext write FAutoNext;
    property AutoPopup: Boolean read FAutoPopup write FAutoPopup;
    property Path: IInterfaceList read FPath;
    property Editing: Boolean read FEditing write SetEditing;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons default False;
    property ShowIcons: Boolean read FShowIcons write SetShowIcons default False;
    property OnButtonDraw: TDrawIndexEvent read FOnButtonDraw write FOnButtonDraw;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Parse(const Path: string); virtual;
  end;

implementation

type
  TPopupHack = class(TCustomPopupForm);

{ TCustomButtonEdit }

constructor TCustomButtonEdit.Create(AOwner: TComponent);
var
  DC: HDC;
  Ratio: Single;
begin
  inherited Create(AOwner);
  FAutoHeight := True;
  FButtonVisible := True;
  FImageIndex := -1;
  Color := clWindow;
  Canvas.Brush.Color := Color;
  ParentColor := False;
  inherited TabStop := False;
  DC := GetDC(GetDesktopWindow);
  Ratio := GetDeviceCaps(DC, LOGPIXELSX) / 96;
  Height := Round(24 * Ratio);
  Width := Round(121 * Ratio);
  ReleaseDC(GetDesktopWindow, DC);
  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
    Parent := Self;
    AutoSelect := False;
    Left := 1;
    Top := 1;
    Anchors := [akLeft, akTop, akRight];
    BorderStyle := bsNone;
    ParentColor := True;
    ParentFont := True;
    FDefEditProc := WindowProc;
    WindowProc := EditProc;
    OnChange := EditChange;
    OnKeyDown := EditKeyDown;
  end;
  AdjustHeight;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
end;

destructor TCustomButtonEdit.Destroy;
begin
  FEdit.WindowProc := FDefEditProc;
  Images := nil;
  FChangeLink.Free;
  inherited Destroy;
end;

procedure TCustomButtonEdit.AdjustEdit;
var
  Rect: TRect;
begin
  if Odd(FontHeight(Font)) then
  begin
    Rect.Top := 1;
    Rect.Bottom := InternalHeight - 1;
  end
  else
  begin
    Rect.Top := 2;
    Rect.Bottom := InternalHeight - 2;
  end;
  Rect.Left := 1;
  Rect.Right := InternalWidth - 1;
  if FImages <> nil then
    Inc(Rect.Left, FImages.Width + 4);
  if FButtonVisible then
    Dec(Rect.Right, GetSystemMetrics(SM_CXVSCROLL));
  DoPlaceEdit(Rect);
  InnerEdit.BoundsRect := Rect;
end;

procedure TCustomButtonEdit.AdjustHeight;
begin
  if FAutoHeight then
    Height := CalcEditHeight(Font);
  AdjustEdit;
end;

procedure TCustomButtonEdit.AdjustSize;
begin
  inherited AdjustSize;
  AdjustHeight;
end;

procedure TCustomButtonEdit.DoButtonPress;
begin
  if Assigned(FOnButtonPress) then
    FOnButtonPress(Self);
end;

procedure TCustomButtonEdit.DoButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

procedure TCustomButtonEdit.DoPlaceEdit(var Rect: TRect);
begin
  if Assigned(FOnPlaceEdit) then
    FOnPlaceEdit(Self, Rect);
end;

procedure TCustomButtonEdit.DrawButtonGlyph(Rect: TRect);
var
  DefaultDraw: Boolean;
  OwnerDrawState: TOwnerDrawState;
  State: TDrawState;
  X, Y: Integer;
  C: TColor;
begin
  DefaultDraw := True;
  if csDesigning in ComponentState then
    State := []
  else if not Enabled then
    State := [dsDisabled]
  else if FButtonDown then
    State := [dsPressed]
  else if FButtonHot then
    State := [dsHot]
  else
    State := [];
  DrawThemeScroll(Canvas.Handle, Rect, State);
  if Assigned(FOnCustomDraw)then
  begin
    OwnerDrawState := [];
    if FFocused or Focused then
      Include(OwnerDrawState, odFocused);
    FOnCustomDraw(Self, Rect, OwnerDrawState, DefaultDraw);
    Include(OwnerDrawState, odComboBoxEdit);
    FOnCustomDraw(Self, ClientRect, OwnerDrawState, DefaultDraw);
  end;
  if DefaultDraw then
    if (FGlyph <> nil) and (not FGlyph.Empty) then
    begin
      DrawThemeScroll(Canvas.Handle, Rect, State);
      AssignGlyph;
      X := Rect.Left + (WidthOf(Rect) - FGlyph.Width) div 2 + 1;
      Y := Rect.Top + (HeightOf(Rect) - FGlyph.Height) div 2;
      if (not ThemePainter.Enabled) and (dsPressed in State) then
      begin
        Inc(X);
        Inc(Y);
      end;
      if Enabled then
        GlyphBlendBlt(Canvas.Handle, FGlyph, X, Y, clWindowText)
      else
        GlyphBlendBlt(Canvas.Handle, FGlyph, X, Y, clBtnShadow);
    end
    else
    begin
      if Enabled then
        C := Blend(clWindowFrame, clBtnFace, 80)
      else
        C := Blend(clBtnFace, clWindowFrame, 70);
      if FButtonDown and (not ThemePainter.Enabled) then
        OffsetRect(Rect, 1, 1);
      case FStyle of
        beStandard: GlyphDraw(Canvas.Handle, Rect, gkVSpin, C);
        beClose: GlyphDraw(Canvas.Handle, Rect, gkClose, C);
        beEllipse: GlyphDraw(Canvas.Handle, Rect, gkEllipse, C);
        beQuestion: GlyphDraw(Canvas.Handle, Rect, gkQuestion, C);
      end;
    end;
end;

procedure TCustomButtonEdit.ParseInput;
begin
end;

procedure TCustomButtonEdit.EditChange(Sender: TObject);
begin
  FTextChanged := True;
  DoChange;
end;

procedure TCustomButtonEdit.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomButtonEdit.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) and (ssAlt in Shift) then
    DoButtonClick;
end;

procedure TCustomButtonEdit.EditProc(var Message: TMessage);

   procedure SelectNextControl;
   var
     Shift: Boolean;
     Control: TWinControl;
   begin
     Shift := GetKeyState(VK_SHIFT) > -1;
     Control := FEdit;
     while Control.Parent <> nil do
       Control := Control.Parent;
     TCustomButtonEdit(Control).SelectNext(FEdit, Shift, True);
   end;

var
  Point: TPoint;
  PriorLParam: Longint;
begin
  with Message do
    case Msg of
      WM_CANCELMODE, CM_CANCELMODE, WM_KILLFOCUS, WM_SETFOCUS,
      WM_KEYFIRST..WM_KEYLAST:
        begin
          case Msg of
            WM_KILLFOCUS, WM_SETFOCUS:
              begin
                FFocused := Msg = WM_SETFOCUS;
                if FFocused then
                begin
                  FTextChanged := False;
                  // AdjustHeight;
                end
                else
                  ParseInput;
                InvalidateRect(Handle, nil, False);
              end;
            WM_CHAR:
                if Message.WParam = 9 then
                begin
                  if FWantTabs then
                  begin
                    UpdateNextControl;
                    if Assigned(FOnTab) then
                      FOnTab(Self);
                  end
                  else
                    SelectNextControl;
                  Exit;
                end
                else if (FSearchStrings <> nil) and (Message.WParam > 31) then
                  SetTimer(Handle, 0, 10, nil);
              WM_KEYDOWN:
                if Message.WParam = VK_TAB then
                  Exit;
           end;
           WindowProc(Message);
         end;
      WM_GETDLGCODE:
        begin
          FDefEditProc(Message);
          Result := Result or DLGC_WANTTAB;
          Exit;
        end;
      WM_MOUSEFIRST..WM_MOUSELAST:
        begin
          Point := SmallPointToPoint(TSmallPoint(LParam));
          MapWindowPoints(FEdit.Handle, Handle, Point, 1);
          PriorLParam := LParam;
          TSmallPoint(LParam) := PointToSmallPoint(Point);
          WindowProc(Message);
          LParam := PriorLParam;
        end;
    end;
  FDefEditProc(Message);
end;

procedure TCustomButtonEdit.ImagesChange(Sender: TObject);
begin
  InvalidateRect(Handle, nil, False);
  AdjustHeight;
end;

function TCustomButtonEdit.GetButtonRect: TRect;
var
  I: Integer;
begin
  Result := ClientRect;
  I := Result.Left;
  Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL) - 1;
  if ThemePainter.Enabled then
  begin
    InflateRect(Result, 2, 2);
    Inc(Result.Left, 2);
  end;
  if Result.Left < I then
    Result.Left := I;
end;

procedure TCustomButtonEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ButtonRect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if csDesigning in ComponentState then Exit;
  if Button in [mbLeft, mbRight] then
  begin
    if CanFocus then
      if FEdit.Visible then
        FEdit.SetFocus
      else
        SetFocus;
    if FButtonVisible and (Button = mbLeft) then
    begin
      ButtonRect := GetButtonRect;
      FButtonDown := PtInRect(ButtonRect, Point(X, Y));
      if FButtonDown then
      begin
        InvalidateRect(Handle, @ButtonRect, False);
        UpdateWindow(Handle);
        DoButtonPress;
      end;
    end;
  end;
end;

procedure TCustomButtonEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ButtonRect: TRect;
  Hot: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  ButtonRect := GetButtonRect;
  Hot := PtInRect(ButtonRect, Point(X, Y));
  if Hot <> FButtonHot then
  begin
    FButtonHot := Hot;
    InvalidateRect(Handle, @ButtonRect, False);
  end;
end;

procedure TCustomButtonEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  ButtonRect: TRect;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FButtonDown and (Button = mbLeft) then
  begin
    FButtonDown := False;
    ButtonRect := GetButtonRect;
    InvalidateRect(Handle, @ButtonRect, False);
    UpdateWindow(Handle);
    if PtInRect(ButtonRect, Point(X, Y)) then
      DoButtonClick;
  end;
end;

procedure TCustomButtonEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    SetImages(nil);
end;

procedure TCustomButtonEdit.Paint;
begin
  FillRectColor(Canvas.Handle, ClientRect, Color);
  if FImages <> nil then
    ImageListDraw(FImages, Canvas, 2, Round((ClientHeight - FImages.Height) / 2),
      FImageIndex, [dsHot]);
  if FButtonVisible then
    DrawButtonGlyph(GetButtonRect);
  inherited Paint;
end;

procedure TCustomButtonEdit.WndProc(var Message: TMessage);
begin
  if HandleAllocated then
    with Message do
      case Msg of
        WM_CHAR:
          if Message.WParam = 9 then
          begin
            Message.Result := 0;
            Exit;
          end;
        WM_GETTEXT, WM_GETTEXTLENGTH, WM_SETTEXT, EM_GETSEL..EM_GETIMESTATUS:
          Result := SendMessage(FEdit.Handle, Msg, wParam, lParam);
        WM_SETFOCUS:
          begin
            inherited WndProc(Message);
            if FEdit.Visible then
              FEdit.SetFocus;
          end;
      else
        inherited WndProc(Message);
      end
  else
  begin
    with Message do
      case Msg of
        WM_SETTEXT:
          FEdit.Text := PChar(lParam);
      end;
    inherited WndProc(Message);
  end;
end;

function TCustomButtonEdit.GetEditHandle: HWND;
begin
  Result := FEdit.Handle;
end;

procedure TCustomButtonEdit.SetButtonVisible(Value: Boolean);
begin
  if Value <> FButtonVisible then
  begin
    FButtonVisible := Value;
    AdjustEdit;
  end;
end;

procedure TCustomButtonEdit.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
    FFlat := Value;
end;

procedure TCustomButtonEdit.AssignGlyph;
begin
  if (FGlyph = nil) or FGlyphAssigned then Exit;
  FGlyph.Transparent := True;
  FGlyph.TransparentColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  FGlyphAssigned := True;
end;


function TCustomButtonEdit.GetGlyph: TBitmap;
begin
  if (FGlyph = nil) and ([csDesigning] * ComponentState = []) then
    FGlyph := TBitmap.Create;
  Result := FGlyph;
end;

procedure TCustomButtonEdit.SetGlyph(Value: TBitmap);
begin
  if Value = nil then
  begin
    FGlyph.Free;
    FGlyph := nil;
    FGlyphAssigned := False;
  end
  else
  begin
    if FGlyph = nil then FGlyph := TBitmap.Create;
    FGlyph.Assign(Value);
  end;
  Invalidate;
end;

procedure TCustomButtonEdit.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    if HandleAllocated and (FImages <> nil) then
      InvalidateRect(Handle, nil, False);
  end;
end;

procedure TCustomButtonEdit.SetImages(Value: TCustomImageList);
begin
  if FImages <> nil then
  begin
    FImages.UnRegisterChanges(FChangeLink);
    FImages.RemoveFreeNotification(Self);
  end;
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FChangeLink);
    FImages.FreeNotification(Self);
  end;
  if not (csDestroying in ComponentState) then
  begin
    AdjustHeight;
    Invalidate;
  end;
end;

procedure TCustomButtonEdit.SetStyle(Value: TButtonEditStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    if FStyle = beOwnerDrawn then
    begin
      FEdit.Visible := False;
      inherited TabStop := TabStop;
      if FFocused then
        SetFocus;
    end
    else
    begin
      FEdit.Visible := True;
      inherited TabStop := False;
      if Focused then
        FEdit.SetFocus;
    end;
    Invalidate;
  end;
end;

procedure TCustomButtonEdit.UpdateNextControl;
begin
  FNextControl := TCustomButtonEdit(Parent).FindNextControl(Self,
    GetKeyState(VK_SHIFT) > -1, False, True);
  if not (FNextControl is TCustomButtonEdit) then
    FNextControl := TCustomButtonEdit(Parent).FindNextControl(Self,
      GetKeyState(VK_SHIFT) > -1, True, True);
end;

{ TCustomButtonEdit.TEdit  }

procedure TCustomButtonEdit.Clear;
begin
  FEdit.Clear;
end;

procedure TCustomButtonEdit.ClearSelection;
begin
  FEdit.ClearSelection;
end;

procedure TCustomButtonEdit.CopyToClipboard;
begin
  FEdit.CopyToClipboard;
end;

procedure TCustomButtonEdit.CutToClipboard;
begin
  FEdit.CutToClipboard;
end;

procedure TCustomButtonEdit.PasteFromClipboard;
begin
  FEdit.PasteFromClipboard;
end;

procedure TCustomButtonEdit.Undo;
begin
  FEdit.Undo;
end;

procedure TCustomButtonEdit.ClearUndo;
begin
  FEdit.ClearUndo;
end;

function TCustomButtonEdit.GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  Result := FEdit.GetSelTextBuf(Buffer, BufSize)
end;

procedure TCustomButtonEdit.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TCustomButtonEdit.SetSelTextBuf(Buffer: PChar);
begin
  FEdit.SetSelTextBuf(Buffer);
end;

function TCustomButtonEdit.GetAutoSelect: Boolean;
begin
  Result := FEdit.AutoSelect;
end;

procedure TCustomButtonEdit.SetAutoSelect(Value: Boolean);
begin
  FEdit.AutoSelect := Value;
end;

function TCustomButtonEdit.GetCanUndo: Boolean;
begin
  Result := FEdit.CanUndo;
end;

function TCustomButtonEdit.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;

procedure TCustomButtonEdit.SetCharCase(Value: TEditCharCase);
begin
  FEdit.CharCase := Value;
end;

function TCustomButtonEdit.GetHideSelection: Boolean;
begin
  Result := FEdit.HideSelection;
end;

procedure TCustomButtonEdit.SetHideSelection(Value: Boolean);
begin
  FEdit.HideSelection := Value;
end;

function TCustomButtonEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

procedure TCustomButtonEdit.SetMaxLength(Value: Integer);
begin
  FEdit.MaxLength := Value;
end;

function TCustomButtonEdit.GetModified: Boolean;
begin
  Result := FEdit.Modified;
end;

procedure TCustomButtonEdit.SetModified(Value: Boolean);
begin
  FEdit.Modified := Value;
end;

function TCustomButtonEdit.GetOEMConvert: Boolean;
begin
  Result := FEdit.OEMConvert;
end;

procedure TCustomButtonEdit.SetOEMConvert(Value: Boolean);
begin
  FEdit.OEMConvert := Value;
end;

function TCustomButtonEdit.GetPasswordChar: Char;
begin
  Result := FEdit.PasswordChar;
end;

procedure TCustomButtonEdit.SetPasswordChar(Value: Char);
begin
  FEdit.PasswordChar := Value;
end;

function TCustomButtonEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

procedure TCustomButtonEdit.SetReadOnly(Value: Boolean);
begin
  FEdit.ReadOnly := Value;
end;

function TCustomButtonEdit.GetSelLength: Integer;
begin
  Result := FEdit.SelLength;
end;

procedure TCustomButtonEdit.SetSelLength(Value: Integer);
begin
  FEdit.SelLength := Value;
end;

function TCustomButtonEdit.GetSelStart: Integer;
begin
  Result := FEdit.SelStart;
end;

procedure TCustomButtonEdit.SetSelStart(Value: Integer);
begin
  FEdit.SelStart := Value;
end;

function TCustomButtonEdit.GetSelText: string;
begin
  Result := FEdit.SelText;
end;

procedure TCustomButtonEdit.SetSelText(Value: string);
begin
  FEdit.SelText := Value;
end;

function TCustomButtonEdit.GetTabStop: Boolean;
begin
  Result := FEdit.TabStop;
end;

procedure TCustomButtonEdit.SetWantTabs(Value: Boolean);
begin
  if Value <> FWantTabs then
    FWantTabs := Value;
end;

procedure TCustomButtonEdit.SetTabStop(Value: Boolean);
begin
  FEdit.TabStop := Value;
end;

procedure TCustomButtonEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FEdit.Enabled := Enabled;
  InvalidateRect(Handle, nil, False);
end;

procedure TCustomButtonEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustHeight;
end;

procedure TCustomButtonEdit.CMMouseLeave(var Message: TMessage);
var
  ButtonRect: TRect;
begin
  inherited;
  if FButtonHot then
  begin
    FButtonHot := False;
    ButtonRect := GetButtonRect;
    InvalidateRect(Handle, @ButtonRect, False);
  end;
end;

procedure TCustomButtonEdit.WMParentNotify(var Message: TWMParentNotify);
begin
  if Message.Event = WM_CREATE then
    PostMessage(Handle, CM_FONTCHANGED, 0, 0);
  Message.Result := 0;
end;

procedure TCustomButtonEdit.WMKillFocus(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCustomButtonEdit.WMSetFocus(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCustomButtonEdit.WMTimer(var Message: TWMTimer);
var
  Index: Integer;
  S: string;
  I: Integer;
begin
  KillTimer(Handle, Message.TimerID);
  if FSearchStrings <> nil then
  begin
    Index := -1;
    S := UpperCase(Copy(Text, 0, SelStart));
    if S <> '' then
      for I := 0 to FSearchStrings.Count - 1 do
        if S = UpperCase(Copy(FSearchStrings[I], 0, Length(S))) then
        begin
          Index := I;
          Text := FSearchStrings[I];
          SelStart := Length(S);
          SelLength := Length(FSearchStrings[I]) - Length(S);
          Break;
        end;
    if Assigned(FOnSearch) then
      FOnSearch(Self, Index);
  end;
  Message.Result := 0;
end;

procedure TCustomButtonEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

{ TPopupEdit }

constructor TPopupEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupHeight := 75;
end;

procedure TPopupEdit.DoButtonClick;
begin
  inherited DoButtonClick;
  ParseInput;
  Popup;
end;

procedure TPopupEdit.RequestPopup;
begin
  if FPopupForm = nil then
  begin
    FPopupForm := CreatePopup;
    with FPopupForm do
    begin
      Associate := Self;
      Height := FPopupHeight;
      SendKeys := True;
      OnCancel := DoCancel;
      OnSelect := DoSelect;
    end;
  end;
end;

procedure TPopupEdit.CreateWnd;
begin
  inherited CreateWnd;
  RequestPopup;
end;

procedure TPopupEdit.DestroyWnd;
begin
  FPopupForm.Associate := nil;
  inherited DestroyWnd;
end;

procedure TPopupEdit.DoPopup;
begin
  if Assigned(FOnPopup) then
    FOnPopup(Self);
end;

procedure TPopupEdit.DoCancel(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

procedure TPopupEdit.DoSelect(Sender: TObject);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TPopupEdit.RealizeWidth;
begin
  if FPopupForm <> nil then
    FPopupForm.Width := Width;
end;

procedure TPopupEdit.Popup;
begin
  DoPopup;
  FPopupForm.Popup;
end;

procedure TPopupEdit.Select;
begin
  TPopupHack(FPopupForm).Select;
end;

procedure TPopupEdit.Cancel;
begin
  TPopupHack(FPopupForm).Cancel;
end;

function TPopupEdit.GetSizeable: Boolean;
begin
  RequestPopup;
  Result := FPopupForm.Sizeable;
end;

procedure TPopupEdit.SetSizeable(const Value: Boolean);
begin
  RequestPopup;
  FPopupForm.Sizeable := Value;
end;

procedure TPopupEdit.SetPopupHeight(Value: Integer);
begin
  FPopupHeight := Value;
  if FPopupForm <> nil then
    FPopupForm.ClientHeight := Value;
end;

{ TDatePopupEdit }

constructor TDatePopupEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupDateForm := TPopupDateForm.Create(Self);
end;

function TDatePopupEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FPopupDateForm;
  if not AllowBlank then
    Text := DateToStr(FPopupDateForm.Date);
end;

procedure TDatePopupEdit.DoSelect(Sender: TObject);
begin
  with FPopupDateForm do
    begin
      Text := DateToStr(Date);
      SelStart := 0;
      SelLength := High(Word);
      if Assigned(FOnSelect) then
        FOnSelect(Self);
    end;
end;

function ConvertDate(const Date: string; var Formatted: string; Default: TDate): TDate;
var
  S: string;
begin
  S := Copy(UpperCase(Trim(Date)), 1, 3);
  if S = 'TOD' then
  begin
    Formatted := 'Today';
    Result := Trunc(Now);
  end
  else if S = 'YES' then
  begin
    Formatted := 'Yesterday';
    Result := Trunc(Now) - 1;
  end
  else if S = 'SUN' then
  begin
    Formatted := 'Sunday';
    Result := Trunc(Now) - DayOfWeek(Now) + 1;
    if Result > Trunc(Now) then
      Result := Result - 7;
  end
  else if S = 'MON' then
  begin
    Formatted := 'Monday';
    Result := Trunc(Now) - DayOfWeek(Now) + 2;
    if Result > Trunc(Now) then
      Result := Result - 7;
  end
  else if S = 'TUE' then
  begin
    Formatted := 'Tuesday';
    Result := Trunc(Now) - DayOfWeek(Now) + 3;
    if Result > Trunc(Now) then
      Result := Result - 7;
  end
  else if S = 'WED' then
  begin
    Formatted := 'Wednesday';
    Result := Trunc(Now) - DayOfWeek(Now) + 4;
    if Result > Trunc(Now) then
      Result := Result - 7;
  end
  else if S = 'THU' then
  begin
    Formatted := 'Thursday';
    Result := Trunc(Now) - DayOfWeek(Now) + 5;
    if Result > Trunc(Now) then
      Result := Result - 7;
  end
  else if S = 'FRI' then
  begin
    Formatted := 'Friday';
    Result := Trunc(Now) - DayOfWeek(Now) + 6;
    if Result > Trunc(Now) then
      Result := Result - 7;
  end
  else if S = 'SAT' then
  begin
    Formatted := 'Saturday';
    Result := Trunc(Now) - DayOfWeek(Now) + 7;
    if Result > Trunc(Now) then
      Result := Result - 7;
  end
  else
  begin
    Result := StrToDateDef(Date, Default);
    Formatted := FormatDateTime(ShortDateFormat, Result);
  end;
end;

procedure TDatePopupEdit.ParseInput;
var
  S: string;
begin
  if FAllowBlank and (Trim(Text) = '') then
    Text := ''
  else
  begin
    FPopupDateForm.Date := ConvertDate(Text, S, Date);
    Text := S;
  end;
end;

function TDatePopupEdit.GetDate: TDate;
begin
  Result := FPopupDateForm.Date;
end;

procedure TDatePopupEdit.SetDate(const Value: TDate);
begin
  FPopupDateForm.Date := Value;
  Text := DateToStr(Value);
end;

function TDatePopupEdit.GetDateString: string;
var
  A, B: TDateTime;
  S: string;
begin
  S := Trim(Text);
  A := Now;
  B := StrToDateTimeDef(S, A);
  if A = B then
    S := ''
  else if B < StrToDate('01/01/1901') then
    S := ''
  else if B > StrToDate('01/01/2400') then
    S := '';
  if S <> '' then
    S := DateToStr(B);
  if (not FAllowBlank) and (S = '') then
    Result := DateToStr(Date)
  else
    Result := S;
end;

{ TListEdit }

constructor TListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupListBox := TPopupListForm.Create(Self);
end;

function TListEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FPopupListBox;
end;

procedure TListEdit.DoSelect(Sender: TObject);
var
  S: string;
begin
  with FPopupListBox do
    if ItemIndex > -1 then
    begin
      S := Items[ItemIndex];
      if DisplayColumn > -1 then
        S := FieldValue(S, DisplayColumn);
      Text := S;
      SelStart := 0;
      SelLength := Length(S);
      if Assigned(FOnSelect) then
        FOnSelect(Self);
    end;
end;

function TListEdit.GetDisplayColumn: Integer;
begin
  Result := FPopupListBox.DisplayColumn;
end;

procedure TListEdit.SetDisplayColumn(Value: Integer);
begin
  FPopupListBox.DisplayColumn := Value;
end;

function TListEdit.GetItemIndex: Integer;
begin
  Result := FPopupListBox.ItemIndex;
end;

procedure TListEdit.SetItemIndex(Value: Integer);
begin
  FPopupListBox.ItemIndex := Value;
end;

function TListEdit.GetItems: TStrings;
begin
  Result := FPopupListBox.Items;
end;

procedure TListEdit.SetItems(Value: TStrings);
begin
  FPopupListBox.Items := Value;
end;

{ TImageListEdit }

constructor TImageListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := True;
  Width := 75;
  FImageIndex := -1;
  FPopupImageList := TPopupImageListForm.Create(Self);
  FPopupImageList.ImageDrawList.OnClick := ImageClick;
  FPopupImageList.ImageDrawList.OnSelectItem := ImageSelectItem;
  FPopupImageList.Top := -9000;
  FPopupImageList.Show;
  FPopupImageList.ImageDrawList.ImageIndex := -1;
  FPopupImageList.Hide;
  FDisplayCount := 5;
end;

procedure TImageListEdit.CreateWnd;
begin
  inherited CreateWnd;
  Text := IntToStr(ImageIndex);
end;

function TImageListEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FPopupImageList;
end;

procedure TImageListEdit.DoPopup;
var
  I: Integer;
begin
  if FDisplayCount < 0 then
    FDisplayCount := 1;
  I := 1;
  if Images <> nil then
    I := I + Images.Count;
  if I > FDisplayCount then
    I := FDisplayCount;
  FPopupImageList.Height := FPopupImageList.ImageDrawList.LineHeight * I + 2;
  inherited DoPopup;
end;

procedure TImageListEdit.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited EditKeyDown(Sender, Key, Shift);
  if (not FPopupImageList.Visible) and (Shift = []) then
    case Key of
      VK_DOWN:
          ImageIndex := ImageIndex + 1;
      VK_UP:
          ImageIndex := ImageIndex - 1;
    end;
end;

procedure TImageListEdit.ImageClick(Sender: TObject);
begin
  Select;
end;

procedure TImageListEdit.ImageSelectItem(Sender: TObject);
begin
  FImageIndex := FPopupImageList.ImageDrawList.ImageIndex;
  Text := IntToStr(ImageIndex);
end;

function TImageListEdit.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TImageListEdit.SetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
  if FImageIndex < 0 then
    FImageIndex := -1;
  FPopupImageList.ImageDrawList.ImageIndex := FImageIndex;
  Text := IntToStr(FImageIndex);
end;

function TImageListEdit.GetImages: TCustomImageList;
begin
  Result := FPopupImageList.ImageDrawList.Images;
end;

procedure TImageListEdit.SetImages(Value: TCustomImageList);
begin
  FPopupImageList.ImageDrawList.Images := Value;
end;

{ TCheckListEdit }

constructor TCheckListEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPopupCheckList := TPopupCheckListForm.Create(Self);
  with FPopupCheckList.CheckList do
  begin
    OnClick := DoClickItem;
    OnClickCheck := DoClickCheck;
  end;
end;

function TCheckListEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FPopupCheckList;
end;

procedure TCheckListEdit.DoClickCheck(Sender: TObject);
begin
  if Assigned(FOnClickCheck) then
    FOnClickCheck(Self);
end;

procedure TCheckListEdit.DoClickItem(Sender: TObject);
begin
  if Assigned(FOnClickItem) then
    FOnClickItem(Self);
end;

procedure TCheckListEdit.Resize;
begin
  inherited Resize;
  FPopupCheckList.Width := Width;
end;

function TCheckListEdit.GetChecked(Index: Integer): Boolean;
begin
  Result := FPopupCheckList.CheckList.Checked[Index];
end;

procedure TCheckListEdit.SetChecked(Index: Integer; Value: Boolean);
begin
  FPopupCheckList.CheckList.Checked[Index] := Value;
end;

function TCheckListEdit.GetCheckText: string;
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

function TCheckListEdit.GetFlat: Boolean;
begin
  Result := FPopupCheckList.CheckList.Flat;
end;

procedure TCheckListEdit.SetFlat(Value: Boolean);
begin
  FPopupCheckList.CheckList.Flat := Value;
end;

function TCheckListEdit.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := FPopupCheckList.CheckList.ItemEnabled[Index];
end;

procedure TCheckListEdit.SetItemEnabled(Index: Integer; Value: Boolean);
begin
  FPopupCheckList.CheckList.ItemEnabled[Index] := Value;
end;

function TCheckListEdit.GetItemIndex: Integer;
begin
  Result := FPopupCheckList.CheckList.ItemIndex;
end;

procedure TCheckListEdit.SetItemIndex(Value: Integer);
begin
  FPopupCheckList.CheckList.ItemIndex := Value;
end;

function TCheckListEdit.GetItems: TStrings;
begin
  Result := FPopupCheckList.CheckList.Items;
end;

procedure TCheckListEdit.SetItems(Value: TStrings);
begin
  FPopupCheckList.CheckList.Items := Value;
end;

function TCheckListEdit.GetSizeable: Boolean;
begin
  Result := FPopupCheckList.Sizeable;
end;

procedure TCheckListEdit.SetSizeable(Value: Boolean);
begin
  FPopupCheckList.Sizeable := Value;
end;

function TCheckListEdit.GetState(Index: Integer): TCheckBoxState;
begin
  Result := FPopupCheckList.CheckList.State[Index];
end;

procedure TCheckListEdit.SetState(Index: Integer; Value: TCheckBoxState);
begin
  FPopupCheckList.CheckList.State[Index] := Value;
end;

function TCheckListEdit.GetStatusText: string;
begin
  Result := FPopupCheckList.StatusText;
end;

procedure TCheckListEdit.SetStatusText(Value: string);
begin
  FPopupCheckList.StatusText := Value;
end;

{ TImageEditItem }

procedure TImageEditItem.Assign(Source: TPersistent);
var
  EditItem: TImageEditItem absolute Source;
begin
  if Source is TImageEditItem then
  begin
    { TODO: Resolve changes here }
    FText := EditItem.Text;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TImageEditItem.SetImageIndex(const Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TImageEditItem.SetIndent(const Value: Integer);
begin
  if Value <> FIndent then
  begin
    FIndent := Value;
    Changed(False);
  end;
end;

procedure TImageEditItem.SetSelectedIndex(const Value: Integer);
begin
  if Value <> FSelectedIndex then
  begin
    FSelectedIndex := Value;
    Changed(False);
  end;
end;

procedure TImageEditItem.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    Changed(False);
  end;
end;

{ TImageEditItems }

constructor TImageEditItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TImageEditItem);
end;

function TImageEditItems.Add: TImageEditItem;
begin
  Result := TImageEditItem(inherited Add);
end;

function TImageEditItems.FindItemID(ID: Integer): TImageEditItem;
begin
  Result := TImageEditItem(inherited FindItemID(ID));
end;

function TImageEditItems.Insert(Index: Integer): TImageEditItem;
begin
  Result := TImageEditItem(GetItem(Index));
end;

procedure TImageEditItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TControl then (GetOwner as TControl).Invalidate;
end;

function TImageEditItems.Get(Index: Integer): TImageEditItem;
begin
  Result := TImageEditItem(GetItem(Index));
end;

procedure TImageEditItems.Put(Index: Integer; const Value: TImageEditItem);
begin
  SetItem(Index, Value);
end;

{ TImageEdit }

constructor TImageEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageList := TImageList.Create(Self);
  FItems := TImageEditItems.Create(Self);
end;

destructor TImageEdit.Destroy;
begin
  FImageList.Free;
  FItems.Free;
  inherited Destroy;
end;

{ TPathEditBar }

{$R pathedit.res}

const
  BreadPad: array[Boolean] of Integer = (3, 4);
  BreadGlyphWidth = 15;

procedure DrawBreadButton(DC: HDC; const Rect: TRect; State: TDrawState; Bordered: Boolean = False);
var
  StartColor, EndColor, EdgeColor: TColor;
  R: TRect;
begin
  FillRectColor(DC, Rect, clBtnHighlight);
  R := Rect;
  if ThemePainter.Enabled then
  begin
    if dsDisabled in State then
    begin
      StartColor := clBtnFace;
      EndColor := StartColor;
      EdgeColor := clBtnShadow;
    end
    else if dsHot in State then
      if dsPressed in State then
      begin
        StartColor := Blend(clHighlight, clWindow);
        EndColor := StartColor;
        EdgeColor := Blend(clHighlight, 0);
      end
      else
      begin
        StartColor := Blend(clHighlight, clWindow, 20);
        EndColor := Blend(clHighlight, clWindow);
        EdgeColor := Blend(clHighlight, clBtnShadow);
      end
    else if dsFocused in State then
    begin
      StartColor := Blend(clWindow, clBtnFace, 25);
      EndColor := Blend(clBtnShadow, clBtnFace);
      EdgeColor := clBtnShadow;
    end
    else
    begin
      StartColor := clBtnFace;
      EndColor := Blend(clBtnShadow, clBtnFace, 70);
      EdgeColor := clBtnShadow;
    end;
    if (R.Left > 0) and (dsHot in State) then
      Dec(R.Left);
    InflateRect(R, -2, 0);
    DrawGradientSplit(DC, R, Blend(clWindow, StartColor, 70), StartColor,
      StartColor, EndColor, drDown);
    InflateRect(R, 2, 0);
    if dsHot in State then
    begin
      DrawRectEdge(DC, R, EdgeColor, drLeft);
      DrawRectEdge(DC, R, EdgeColor, drRight);
      if dsPressed in State then
      begin
        Inc(R.Left);
        DrawRectEdge(DC, R, clBtnShadow, drLeft);
      end;
    end
    else
    begin
      DrawRectEdge(DC, R, EdgeColor, drRight);
      if Bordered then
        DrawRectEdge(DC, Rect, EdgeColor, drLeft);
    end;
  end
  else
  begin
    FillRectColor(DC, R, Blend(clBtnFace, clWindow, 66));
    InflateRect(R, 0, 1);
    if [dsPressed, dsHot] * State = [dsPressed, dsHot] then
      DrawThemeThinButton(DC, R, State)
    else
      DrawThemeThinButton(DC, R, [dsHot]);
  end;
end;

constructor TPathEditBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := CreateDocument;
  FButtonWidth := BreadGlyphWidth;
  FFolderItems := TPopupScrollingMenu.Create(Self);
  FFolderItems.Associate := Self;
  FFolderItems.OnSelect := FolderSelect;
  FPopupItems := TPopupScrollingMenu.Create(Self);
  FPopupItems.Associate := Self;
  FPopupItems.OnSelect := PopupSelect;
  FDownImage := TAlphaImage.Create;
  FDownImage.LoadFromResourceID(750);
  FRightImage := TAlphaImage.Create;
  FRightImage.LoadFromResourceID(751);
  Edits.BeginUpdate;
  FEdit := Edits.Add.Edit;
  FEdit.Parent := Self;
  FEdit.ControlStyle := FEdit.ControlStyle + [csNoDesignVisible];
  FEdit.Visible := False;
  FEdit.Left := 1;
  FEdit.Width := InternalWidth - 1;
  FEdit.Anchors := [akLeft, akTop, akRight, akBottom];
  FEdit.OnExit := EditExit;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnKeyPress := EditKeyPress;
  Edits.EndUpdate;
  FScrollButton := TImageSpeedButton.Create(Self);
  FScrollButton.Parent := Self;
  FScrollButton.ControlStyle := FEdit.ControlStyle + [csNoDesignVisible];
  FScrollButton.Align := alLeft;
  FScrollButton.Width := 24;
  FScrollButton.Visible := False;
  FScrollButton.OnMouseDown := ScrollButtonMouseDown;
  FScrollButton.OnDrawButton := ScrollButtonDraw;
  FScrollButton.OnClick := ScrollButtonClick;
  ControlStyle := ControlStyle + [csDoubleClicks];
  DoubleBuffered := True;
  TabStop := True;
  Width := 160;
end;

destructor TPathEditBar.Destroy;
begin
  FDownImage.Free;
  FRightImage.Free;
  inherited Destroy;
end;

procedure TPathEditBar.ScrollButtonClick(Sender: TObject);
var
  Menu: TScrollingMenu;
  Item: TScrollingMenuItem;
  F: IFiler;
  I: Integer;
begin
  Menu := FFolderItems.Menu;
  Menu.Items.BeginUpdate;
  try
    Menu.Items.Clear;
    for I := 0 to FPath.Count - 1 do
    begin
      F := INode(FPath[I]).Attributes.Filer;
      Item := Menu.Items.Add;
      Item.Caption := F.ReadString('name');
      Item.ImageIndex := F.ReadInteger('image');
    end;
  finally
    Menu.Items.EndUpdate;
  end;
  FFolderItems.Menu.Images := Images;
  FFolderItems.MinWidth := 0;
  FFolderItems.HorzOffset := 3;
  FFolderItems.Popup;
end;

procedure TPathEditBar.ScrollButtonDraw(Control: TControl; Rect: TRect;
  DrawState: TDrawState; var DefaultDraw: Boolean);
var
  DC: HDC;
  R: TRect;
  Border, Start, Stop: TColor;
begin
  DefaultDraw := False;
  DC := FScrollButton.Canvas.Handle;
  R := Rect;
  Inc(R.Bottom, 10);
  InflateRect(R, -1, 0);
  if ThemePainter.Enabled then
  begin
    Border := Blend(Color, 0);
    Start := Blend(Color, Font.Color, 60);
    Stop := Start;
    if dsHot in DrawState then
      if dsPressed in DrawState then
      begin
        Start := Blend(Color, Font.Color, 80);
        Stop := Start;
      end
      else
      begin
        Start := Blend(Color, Font.Color, 60);
        Stop := Blend(Color, Font.Color, 80);
      end
    else if Focused then
    begin
        Start := Blend(Color, Font.Color, 50);
        Stop := Blend(Color, Font.Color, 75);
    end;
    FillRoundRectFancy(DC, R, 4, Start, Stop, drDown, Border,
      Border, psSolid, bsClear);
  end
  else
  begin
    Inc(R.Bottom, 5);
    Inc(R.Top, 1);
    FillRectColor(DC, R, clBtnFace);
      if [dsHot, dsPressed] * DrawState = [dsHot, dsPressed] then
      begin
        DrawFrame(DC, R, dfPushed);
        OffsetRect(Rect, 1, 1);
      end
      else
        FillRectOutline(DC, R, clBtnShadow);
  end;
  R := Rect;
  Inc(R.Left, 2);
  OffsetRect(R, 0, 2);
  GlyphDraw(DC, R, gkChevronDown, Font.Color);
end;

procedure TPathEditBar.ScrollButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
end;

procedure TPathEditBar.ButtonClick(Index: Integer);
var
  R: TRect;
begin
  if FDoubleClicked then
  begin
    FButtonIndex := -1;
    FDoubleClicked := False;
    Editing := True;
    Exit;
  end;
  FButtonIndex := Index;
  if Index = 0 then
    DataSelect(FPath[0] as INode)
  else if Odd(Index) then
  begin
    R := ButtonRect[Index - 1];
    if BorderStyle = bsSingle then
      Inc(R.Left, 2);
    FPopupItems.HorzOffset := R.Left;
    FPopupItems.MinWidth := WidthOf(ButtonRect[Index - 1]) + WidthOf(ButtonRect[Index]);
    if FPopupItems.Menu.Items.Count = 0 then
    begin
      FPopupItems.OnSelect := nil;
      FPopupItems.Menu.Items.Add.Caption := '(empty)';
    end
    else
      FPopupItems.OnSelect := PopupSelect;
    FPopupItems.Popup;
  end
  else
    DataSelect(FPath[(Index) div 2] as INode)
end;

procedure TPathEditBar.ButtonHover(Index: Integer);
begin
  if FAutoPopup and FPopupItems.Visible and (Index > -1) and (Index div 2 <> FButtonIndex div 2) then
  begin
    FPopupItems.Cancel;
    ButtonClick((Index div 2) * 2 + 1);
  end;
end;

procedure TPathEditBar.ButtonDraw(Index: Integer; Rect: TRect; State: TDrawState);
var
  Image: TAlphaImage;
  Down: Boolean;
  R: TRect;
  N: INode;
  F: IFiler;
  C: TColor;
  X, Y, I: Integer;
begin
  Canvas.Font := Font;
  R := Rect;
  N := FPath[Index div 2] as INode;
  F := N.Attributes.Filer;
  Down := State * [dsHot, dsPressed] = [dsHot, dsPressed];
  if ButtonHot > -1 then
    if ButtonHot div 2 = Index div 2 then
    begin
      if (Index > 1) and Odd(Index) then
        Include(State, dsHot);
      if Assigned(FOnButtonDraw) then
        FOnButtonDraw(Self, Canvas, Index, R, State)
      else
        DrawBreadButton(Canvas.Handle, R, State, not FShowButtons)
    end
    else if FShowButtons or (dsHot in State) then
      if Assigned(FOnButtonDraw) then
        FOnButtonDraw(Self, Canvas, Index, R, State)
      else
        DrawBreadButton(Canvas.Handle, R, State);
  if Index = 0 then
  begin
    if Down then
      OffsetRect(R, 1, 1);
    OffsetRect(R, -1, 0);
    ImageListDraw(Images, Canvas, R, F.ReadInteger('image', -1), []);
  end
  else if Odd(Index) then
  begin
    C := ColorToRGB(Font.Color);
    if C <> FImageColor then
    begin
      FImageColor := C;
      FDownImage.LoadFromResourceID(750);
      FRightImage.LoadFromResourceID(751);
      FDownImage.Screen(C);
      FRightImage.Screen(C);
    end;
    if Down then
      Image := FDownImage
    else
      Image := FRightImage;
    X := R.Left + (WidthOf(R) - Image.Width) div 2;
    Y := R.Top + (HeightOf(R) - Image.Height) div 2;
    if Assigned(FOnButtonDraw) then
      OnButtonDraw(Self, Canvas, Index, Rect, State)
    else
      Canvas.Draw(X, Y, Image);
  end
  else
  begin
    if Down then
      OffsetRect(R, 1, 1);
    if FShowIcons then
      I := F.ReadInteger('image', -1)
    else
      I := -1;
    if (I > 0) and (FImageWidth > 0) then
    begin
      Inc(R.Left, BreadPad[FShowButtons]);
      R.Right := R.Left + FImageWidth;
      ImageListDraw(Images, Canvas, R, I, []);
      R.Left := R.Right;
      R.Right := Rect.Right;
    end;
    DrawCaption(Canvas.Handle, F.ReadString('name'), R, drCenter, True, True);
  end;
end;

procedure TPathEditBar.FolderSelect(Sender: TObject);
var
  N: INode;
begin
  N := INode(FPath[FFolderItems.MenuIndex]);
  DataSelect(N);
end;

procedure TPathEditBar.PopupSelect(Sender: TObject);
begin

end;

procedure TPathEditBar.EditExit(Sender: TObject);
begin
  Editing := False;
end;

procedure TPathEditBar.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        Editing := False;
      end;
    VK_RETURN:
      begin
        Editing := False;
        Parse(FEdit.Text);
      end;
  end;
end;

procedure TPathEditBar.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    Key := #0;
end;

procedure TPathEditBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_SPACE, VK_RETURN: Editing := not Editing;
  end;
end;

procedure TPathEditBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    SetFocus;
    FButtonIndex := ButtonFromPoint(Point(X, Y));
  end;
end;

procedure TPathEditBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and (FButtonIndex < 0) and (ButtonFromPoint(Point(X, Y)) < 0) then
    Editing := True;
end;

procedure TPathEditBar.UpdatePath;
var
  DC: HDC;
  PriorFont: HFONT;
  ItemWidth: Integer;
  N: INode;
  F: IFiler;
  R: TRect;
  I: Integer;
begin
  DC := GetDC(0);
  PriorFont := SelectObject(DC, Font.Handle);
  try
    ButtonClear;
    if Images <> nil then
      FImageWidth := Images.Height
    else
      FImageWidth := 0;
    for I := 0 to FPath.Count - 1 do
    begin
      N := FPath[I] as INode;
      if I = 0 then
      begin
        R := Rect(0, 0, FImageWidth + BreadPad[FShowButtons] * 2, InternalHeight);
        ButtonDefine(R);
        R.Left := R.Right;
        R.Right := R.Left + FButtonWidth;
      end
      else
      begin
        F := N.Attributes.Filer;
        ItemWidth := FontWidth(DC, F.ReadString('name')) + BreadPad[FShowButtons] * 2;
        if FShowIcons and (F.ReadInteger('image', -1) > -1) then
          Inc(ItemWidth, FImageWidth + BreadPad[FShowButtons]);
        R.Left := R.Right;
        R.Right := R.Left + ItemWidth;
        ButtonDefine(R);
        R.Left := R.Right;
        R.Right := R.Left + FButtonWidth;
      end;
      if CanDropDown(I) then
        ButtonDefine(R);
    end;
    Invalidate;
  finally
    SelectObject(DC, PriorFont);
  end;
end;

procedure TPathEditBar.DataDefine(const Data: string);
begin
  FData.Text := Data;
  FPath := TInterfaceList.Create;
  if FData.Root <> nil then
    FPath.Add(FData.Root);
  UpdatePath;
end;

procedure TPathEditBar.EditingChange;
begin
end;

function FindNamedNode(Root: INode; const Path: string): INode;
var
  S, XPath: string;
  I: Integer;
begin
  Result := Root;
  if Root = nil then
    Exit;
  S := Trim(Path);
  if S = '' then Exit;
  if S[1] = '/' then
    S[1] := ' ';
  if S[Length(S)] = '/' then
    S[Length(S)] := ' ';
  S := Trim(S);
  if S = '' then Exit;
  XPath := '/node';
  for I := 0 to FieldCount(S, '/') - 1 do
    XPath := XPath + '/node[@name="' + FieldValue(S, '/', I) + '"]';
  Result := Root.FindNode(XPath);
end;

procedure TPathEditBar.AlignEdits;
begin
  inherited AlignEdits;
  if (not FEditing) and (FPath <> nil) then
    UpdatePath;
end;

function TPathEditBar.GetButtonRect(Index: Integer): TButtonRect;
var
  R: TRect;
begin
  Result := inherited GetButtonRect(Index);
  if ButtonCount > 0 then
  begin
    R := inherited GetButtonRect(ButtonCount - 1);
    FScrollButton.Visible := R.Right > InternalWidth;
    if FScrollButton.Visible then
      OffsetRect(Result, InternalWidth - R.Right + 1, 0);
  end;
end;

function TPathEditBar.Convert: string;
var
  I: Integer;
begin
  Result := '/';
  for I := 1 to FPath.Count - 1 do
    Result := Result + INode(FPath[I]).Attributes.Filer.ReadString('name') + '/';
end;

function TPathEditBar.CanDropDown(Index: Integer): Boolean;
begin
  Result := True;
end;

procedure TPathEditBar.DblClick;
begin
  inherited DblClick;
  FDoubleClicked := True;
end;

procedure TPathEditBar.DataSelect(Node: INode);
begin
  if Node <> nil then
  begin
    ButtonClear;
    FPath := TInterfaceList.Create;
    while Node <> nil do
    begin
      FPath.Insert(0, Node);
      Node := Node.Parent;
    end;
    UpdatePath;
  end;
end;

procedure TPathEditBar.Parse(const Path: string);
begin
  DataSelect(FindNamedNode(FData.Root, Path));
end;

function TPathEditBar.GetButtonNode(Index: Integer): INode;
begin
  Result := INode(FPath[Index]);
end;

procedure TPathEditBar.SetButtonWidth(Value: Integer);
begin
  if Value < 5 then
    Value := 5;
  if Value <> FButtonWidth then
  begin
    FButtonWidth := Value;
    UpdatePath;
  end;
end;

procedure TPathEditBar.SetShowButtons(Value: Boolean);
begin
  if Value <> FShowButtons then
  begin
    FShowButtons := Value;
    if FPath.Count > 0 then
      DataSelect(INode(FPath[FPath.Count - 1]));
  end;
end;

procedure TPathEditBar.SetShowIcons(Value: Boolean);
begin
  if Value <> FShowIcons then
  begin
    FShowIcons := Value;
    if FPath.Count > 0 then
      DataSelect(INode(FPath[FPath.Count - 1]));
  end;
end;

procedure TPathEditBar.SetEditing(Value: Boolean);
begin
  if Value <> FEditing then
  begin
    FEditing := Value;
    if FEditing then
    begin
      ButtonClear;
      FScrollButton.Visible := False;
      FEdit.Width := ClientWidth - 1;
      FEdit.Height := ClientHeight - 1;
      FEdit.Text := Convert;
      FEdit.Visible := True;
      FEdit.SetFocus;
    end
    else
    begin
      if FPath.Count > 0 then
        DataSelect(INode(FPath[FPath.Count - 1]));
      FEdit.Visible := False;
      SetFocus;
    end;
    Invalidate;
    EditingChange;
  end;
end;

end.