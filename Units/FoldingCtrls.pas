
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit FoldingCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImgList,
  CommCtrl, BaseTypes, GraphTools, ProviderTools, FormTools;

type
  TCustomFoldingView = class;
  TFoldingBars = class;
  TFoldingBar = class;
  TFoldingItems = class;

  TFoldingItem = class(TCollectionItem)
  private
    FCaption: string;
    FEnabled: Boolean;
    FData: Pointer;
    FDataObject: Boolean;
    FImageIndex: Integer;
    FVisible: Boolean;
    FRect: TRect;
    FUnknown: IUnknown;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Click;
    property Data: Pointer read FData write FData;
    property DataObject: Boolean read FDataObject write FDataObject;
    property Unknown: IUnknown read FUnknown write FUnknown;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TFoldingItems }

  TFoldingItems = class(TCollection)
  private
    FFolding: TFoldingBar;
    FView: TCustomFoldingView;
  protected
    procedure Update(Item: TCollectionItem); override;
    function Get(Index: Integer): TFoldingItem;
    procedure Put(Index: Integer; Value: TFoldingItem);
  public
    constructor Create(Control: TControl; Folding: TFoldingBar);
    procedure Assign(Source: TPersistent); override;
    function Add: TFoldingItem;
    function Insert(Index: Integer): TFoldingItem;
    property Items[Index: Integer]: TFoldingItem read Get write Put; default;
    property Folding: TFoldingBar read FFolding;
    property View: TCustomFoldingView read FView;
  end;

{ TFoldingBar }

  TFoldingBar = class(TCollectionItem)
  private
    FCaption: string;
    FData: Pointer;
    FDataObject: Boolean;
    FImageIndex: Integer;
    FItems: TFoldingItems;
    FSelectedIndex: Integer;
    FVisible: Boolean;
    FRect: TRect;
    FUnknown: IUnknown;
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(Value: Integer);
    procedure SetItems(Value: TFoldingItems);
    procedure SetSelectedIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Data: Pointer read FData write FData;
    property DataObject: Boolean read FDataObject write FDataObject;
    property Unknown: IUnknown read FUnknown write FUnknown;
  published
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Items: TFoldingItems read FItems write SetItems;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TFoldingBars }

  TFoldingBars = class(TCollection)
  private
    FView: TCustomFoldingView;
  protected
    procedure Update(Item: TCollectionItem); override;
    function Get(Index: Integer): TFoldingBar;
    procedure Put(Index: Integer; Value: TFoldingBar);
  public
    constructor Create(Control: TControl);
    procedure Assign(Source: TPersistent); override;
    function Add: TFoldingBar;
    function Insert(Index: Integer): TFoldingBar;
    property Items[Index: Integer]: TFoldingBar read Get write Put; default;
    property View: TCustomFoldingView read FView;
  end;

{ TCustomFoldingView }

  IFoldingProvider = interface(IControlProvider)
    ['{909126BE-9DA6-490C-9847-5E7C244FE2D3}']
    function GetBarHeight(View: TCustomFoldingView): Integer;
    function GetItemHeight(View: TCustomFoldingView): Integer;
    procedure DrawBarBkgnd(View: TCustomFoldingView; Rect: TREct);
    procedure DrawBar(View: TCustomFoldingView; Bar: TFoldingBar; Rect: TRect; State: TDrawState);
    procedure DrawItemBkgnd(View: TCustomFoldingView; Rect: TRect);
    procedure DrawItem(View: TCustomFoldingView; Item: TFoldingItem; Rect: TRect; State: TDrawState);
  end;

  TFoldingItemEvent = procedure(Sender: TObject; Item: TFoldingItem) of object;
  TFoldingScrollButton = (fbScrollUp, fbScrollDown);
  TFoldingScrollButtons = set of TFoldingScrollButton;

  TCustomFoldingView = class(TFramedWindow)
  private
    FChangeLink: TChangeLink;
    FBars: TFoldingBars;
    FBarImages: TCustomImageList;
    FBarHotIndex: Integer;
    FBarDownIndex: Integer;
    FBarRect: TRect;
    FItemHotIndex: Integer;
    FItemDownIndex: Integer;
    FItemRect: TRect;
    FItemImages: TCustomImageList;
    FSelected: TFoldingBar;
    FBoldText: string;
    FOnChange: TNotifyEvent;
    FOnItemClick: TFoldingItemEvent;
    function GetFoldingProvider: IFoldingProvider;
    procedure ImagesChange(Sender: TObject);
    procedure UpdateImages(var InternalImages: TCustomImageList;
      ExternalImages: TCustomImageList);
    procedure SetBars(Value: TFoldingBars);
    procedure SetBarImages(Value: TCustomImageList);
    procedure SetItemImages(Value: TCustomImageList);
    procedure SetBoldText(const Value: string);
    procedure SetSelected(Value: TFoldingBar);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure BarsChange; dynamic;
    procedure DoItemClick(Item: TFoldingItem); dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    property BoldText: string read FBoldText write SetBoldText;
    property Bars: TFoldingBars read FBars write SetBars;
    property BarImages: TCustomImageList read FBarImages write SetBarImages;
    property ItemImages: TCustomImageList read FItemImages write SetItemImages;
    property FoldingProvider: IFoldingProvider read GetFoldingProvider;
    property Selected: TFoldingBar read FSelected write SetSelected;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnItemClick: TFoldingItemEvent read FOnItemClick write FOnItemClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AbsoluteIndex(Bar: TFoldingBar): Integer;
  end;

{ TFoldingView }

  TFoldingView = class(TCustomFoldingView)
  public
    property Selected;
    property BoldText;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property BarImages;
    property Bars;
    property ItemImages;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ProviderName;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnDblClick;
    property OnChange;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnItemClick;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TDefaultFoldingProvider }

type
  TDefaultFoldingProvider = class(TControlProvider, IFoldingProvider)
  protected
    procedure Init(Control: TControl); override;
    function GetBarHeight(View: TCustomFoldingView): Integer;
    function GetItemHeight(View: TCustomFoldingView): Integer;
    procedure DrawBarBkgnd(View: TCustomFoldingView; Rect: TRect);
    procedure DrawBar(View: TCustomFoldingView; Bar: TFoldingBar; Rect: TRect; State: TDrawState);
    procedure DrawItemBkgnd(View: TCustomFoldingView; Rect: TRect);
    procedure DrawItem(View: TCustomFoldingView; Item: TFoldingItem; Rect: TRect; State: TDrawState);
  end;

procedure TDefaultFoldingProvider.Init(Control: TControl);
var
  View: TCustomFoldingView absolute Control;
begin
  if Control is TCustomFoldingView then
    View.ParentBackground := False;
end;

function TDefaultFoldingProvider.GetBarHeight(View: TCustomFoldingView): Integer;
begin
  Result := FontHeight(View.Canvas.Handle) + 8;
end;

function TDefaultFoldingProvider.GetItemHeight(View: TCustomFoldingView): Integer;
begin
  Result := FontHeight(View.Canvas.Handle) + 4;
end;

procedure TDefaultFoldingProvider.DrawBarBkgnd(View: TCustomFoldingView; Rect: TRect);
begin
  with View do
    FillRectColor(Canvas.Handle, ClientRect, Color);
end;

procedure TDefaultFoldingProvider.DrawBar(View: TCustomFoldingView; Bar: TFoldingBar; Rect: TRect; State: TDrawState);
var
  DC: HDC;
  R: TRect;
begin
  DC := View.Canvas.Handle;
  DrawThemeHeader(DC, Rect, State);
  R := Rect;
  R.Left := R.Left + HeightOf(R) div 3;
  DrawCaption(DC, Bar.Caption, R, drLeft);
end;

procedure TDefaultFoldingProvider.DrawItemBkgnd(View: TCustomFoldingView; Rect: TRect);
var
  DC: HDC;
begin
  DC := View.Canvas.Handle;
  FillRectColor(DC, Rect, clWindow);
end;

procedure TDefaultFoldingProvider.DrawItem(View: TCustomFoldingView; Item: TFoldingItem; Rect: TRect; State: TDrawState);
var
  DC: HDC;
begin
  DC := View.Canvas.Handle;
  InflateRect(Rect, -2, 0);
  if dsHot in State then
    if dsPressed in State then
      DrawStyleRect(DC, Rect, False)
    else
      DrawStyleRect(DC, Rect, True);
  InflateRect(Rect, -5, 0);
  DrawCaption(DC, Item.Caption, Rect, drleft, not (dsDisabled in State));
end;


{$R FOLDGEL.RES}

{ TGelFoldingProvider }

type
  TGelFoldingProvider = class(TControlProvider, IFoldingProvider)
  private
    FEdge: TAlphaImage;
    FRoundGel: TAlphaImage;
    FRoundFill: TAlphaImage;
    FSquareGel: TAlphaImage;
    FSquareFill: TAlphaImage;
  protected
    function GetBarHeight(View: TCustomFoldingView): Integer;
    function GetItemHeight(View: TCustomFoldingView): Integer;
    procedure DrawBarBkgnd(View: TCustomFoldingView; Rect: TRect);
    procedure DrawBar(View: TCustomFoldingView; Bar: TFoldingBar; Rect: TRect; State: TDrawState);
    procedure DrawItemBkgnd(View: TCustomFoldingView; Rect: TRect);
    procedure DrawItem(View: TCustomFoldingView; Item: TFoldingItem; Rect: TRect; State: TDrawState);
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetProviderName: TProviderName; override;
  end;

class function TGelFoldingProvider.GetProviderName: TProviderName;
begin
  Result := 'Gel';
end;

constructor TGelFoldingProvider.Create;
const
  GelOpacity = $FF; //$C0;
begin
  inherited Create;
  FEdge := TAlphaImage.Create;
  FEdge.LoadFromResourceID(8901);
  FRoundGel := TAlphaImage.Create;
  FRoundGel.LoadFromResourceID(8902);
  FRoundGel.Opacity := GelOpacity;
  FRoundFill := TAlphaImage.Create;
  FRoundFill.LoadFromResourceID(8903);
  FSquareGel := TAlphaImage.Create;
  FSquareGel.LoadFromResourceID(8904);
  FSquareGel.Opacity := GelOpacity;
  FSquareFill := TAlphaImage.Create;
  FSquareFill.LoadFromResourceID(8905);
end;

destructor TGelFoldingProvider.Destroy;
begin
  FEdge.Free;
  FRoundGel.Free;
  FRoundFill.Free;
  FSquareGel.Free;
  FSquareFill.Free;
  inherited Destroy;
end;

function TGelFoldingProvider.GetBarHeight(View: TCustomFoldingView): Integer;
begin
  Result := FRoundGel.Height;
end;

function TGelFoldingProvider.GetItemHeight(View: TCustomFoldingView): Integer;
begin
  Result := FontHeight(View.Canvas.Handle) + 3;
end;

procedure TGelFoldingProvider.DrawBarBkgnd(View: TCustomFoldingView; Rect: TRect);
begin
  // View.DrawParentBackground(View.Handle, View.Canvas.Handle, nil, False);
end;

procedure TGelFoldingProvider.DrawBar(View: TCustomFoldingView; Bar: TFoldingBar; Rect: TRect; State: TDrawState);
var
  DC: HDC;
  A, B: TAlphaImage;
  R: TRect;
  F: HFONT;
  O: Byte;
  I: Integer;
begin
  DC := View.Canvas.Handle;
  {if [dsFocused, dsSelected] * State = [dsFocused, dsSelected]  then
    Include(State, dsHot);}
  R.Left := 10;
  R.Top := 0;
  R.Right := 10;
  R.Bottom := 0;
  if ThemePainter.Enabled then
  begin
    if View.AbsoluteIndex(Bar) < 1 then
    begin
      A := FRoundGel;
      B := FRoundFill;
    end
    else
    begin
      A := FSquareGel;
      B := FSquareFill;
    end;
    A.Blit(DC, Rect, R);
  end
  else
  begin
    B := nil;
    FillRectColor(DC, Rect, clBtnFace);
    DrawFrame(DC, Rect, dfRaised);
  end;
  R := Rect;
  R.Left := R.Left + HeightOf(R) div 3;
  I := -1;
  if View.BarImages <> nil then
    if dsSelected in State then
    begin
      I := Bar.SelectedIndex;
      if I < 0 then
        I := Bar.ImageIndex;
    end
    else
      I := Bar.ImageIndex;
  if I > -1 then
  begin
    ImageListDraw(View.BarImages, View.Canvas, R.Left div 2,
      R.Top + (HeightOf(R) - View.BarImages.Height) div 2 - 2, I, []);
    R.Left := R.Left + View.BarImages.Width;
  end;
  F := SelectFontStyle(DC, [fsBold], HeightOf(R) div 2 + 2);
  SetTextColor(DC, ColorToRGB(clWindowText));
  OffsetRect(R, 1, 1);
  DrawCaption(DC, Bar.Caption, R, drLeft);
  SetTextColor(DC, ColorToRGB(clWindow));
  OffsetRect(R, -1, -1);
  if ThemePainter.Enabled then
  DrawCaption(DC, Bar.Caption, R, drLeft);
  OverwriteObject(DC, F);
  SetTextColor(DC, ColorToRGB(clWindowText));
  if ThemePainter.Enabled then
  begin
    R.Left := 10;
    R.Top := 0;
    R.Right := 10;
    R.Bottom := 0;
    O := $00;
    if dsHot in State then
      if dsPressed in State then
      begin
        B.Colorize(clBlack);
        O := $17;
      end
      else
      begin
        B.Colorize(clWhite);
        O := $27;
      end;
    if O > $00 then
    begin
      B.Opacity := O;
      B.Blit(DC, Rect, R);
    end;
  end;
end;

procedure TGelFoldingProvider.DrawItemBkgnd(View: TCustomFoldingView; Rect: TRect);
var
  DC: HDC;
  R: TRect;
begin
  DC := View.Canvas.Handle;
  R := Rect;
  FillRectColor(DC, R, clWindow);
  Dec(R.Top);
  if ThemePainter.Enabled then
    FillRectOutline(DC, Rect, Blend(clHighlight, clBtnShadow, 75));
end;

procedure TGelFoldingProvider.DrawItem(View: TCustomFoldingView; Item: TFoldingItem; Rect: TRect; State: TDrawState);
var
  DC: HDC;
  F: HFONT;
begin
  DC := View.Canvas.Handle;
  InflateRect(Rect, -2, 0);
  Inc(Rect.Top);
  if dsHot in State then
    if dsPressed in State then
      DrawStyleRect(DC, Rect, False)
    else
      DrawStyleRect(DC, Rect, True);
  InflateRect(Rect, -5, 0);
  if (View.BoldText <> '') and (Item.Caption = View.BoldText) then
  begin
    F := GetFont(DC, [fsBold]);
    F := SelectObject(DC, F);
    DrawCaption(DC, Item.Caption, Rect, drleft, not (dsDisabled in State));
    OverwriteObject(DC, F);
  end
  else
    DrawCaption(DC, Item.Caption, Rect, drleft, not (dsDisabled in State));
end;

{ TFoldingItem }

constructor TFoldingItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEnabled := True;
  FVisible := True;
  FImageIndex := -1;
end;

destructor TFoldingItem.Destroy;
begin
  if FDataObject and (FData <> nil) then
    TObject(FData).Free;
  inherited Destroy;
end;

procedure TFoldingItem.Assign(Source: TPersistent);
var
  Item: TFoldingItem absolute Source;
begin
  if Source is TFoldingItem then
  begin
    Caption := Item.Caption;
    ImageIndex := Item.ImageIndex;
    Enabled := Item.Enabled;
    Visible := Item.Visible;
  end
  else
    inherited Assign(Source);
end;

procedure TFoldingItem.Click;
begin
end;

procedure TFoldingItem.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(True);
  end;
end;

procedure TFoldingItem.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed(True);
  end;
end;

procedure TFoldingItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(True);
  end;
end;

procedure TFoldingItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

{ TFoldingItems }

constructor TFoldingItems.Create(Control: TControl; Folding: TFoldingBar);
begin
  inherited Create(TFoldingItem);
  FFolding := Folding;
  if Control is TCustomFoldingView then
    FView := TCustomFoldingView(Control);
end;

procedure TFoldingItems.Assign(Source: TPersistent);
var
  FoldingItems: TFoldingItems absolute Source;
  I: Integer;
begin
  if Source is TFoldingItems then
  begin
    BeginUpdate;
    Clear;
    for I := 0 to FoldingItems.Count - 1 do
      Add.Assign(FoldingItems[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

function TFoldingItems.Add: TFoldingItem;
begin
  Result := inherited Add as TFoldingItem;
end;

function TFoldingItems.Insert(Index: Integer): TFoldingItem;
begin
  Result := inherited Insert(Index) as TFoldingItem;
end;

procedure TFoldingItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  TFoldingBars(FFolding.Collection).Changed;
end;

function TFoldingItems.Get(Index: Integer): TFoldingItem;
begin
  Result := GetItem(Index) as TFoldingItem;
end;

procedure TFoldingItems.Put(Index: Integer; Value: TFoldingItem);
begin
  SetItem(Index, Value);
end;

{ TFoldingBar }

constructor TFoldingBar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FItems := TFoldingItems.Create((Collection as TFoldingBars).View, Self);
  FImageIndex := -1;
  FSelectedIndex := -1;
end;

procedure TFoldingBar.Assign(Source: TPersistent);
var
  Bar: TFoldingBar absolute Source;
begin
  if Source is TFoldingBar then
  begin
    Caption := Bar.Caption;
    ImageIndex := Bar.ImageIndex;
    SelectedIndex := Bar.SelectedIndex;
    Visible := Bar.Visible;
    Items.Assign(Bar.Items);
  end
  else
    inherited Assign(Source);
end;

destructor TFoldingBar.Destroy;
begin
  FItems.Free;
  if FDataObject and (FData <> nil) then
    TObject(FData).Free;
  inherited Destroy;
end;

procedure TFoldingBar.SetCaption(const Value: string);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(True);
  end;
end;

procedure TFoldingBar.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(True);
  end;
end;

procedure TFoldingBar.SetItems(Value: TFoldingItems);
begin
  if Value <> FItems then
    FItems.Assign(Value);
end;

procedure TFoldingBar.SetSelectedIndex(Value: Integer);
begin
  if Value <> FSelectedIndex then
  begin
    FSelectedIndex := Value;
    Changed(True);
  end;
end;

procedure TFoldingBar.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
     FVisible := Value;
     Changed(True);
  end;
end;

{ TFoldingBars }

constructor TFoldingBars.Create(Control: TControl);
begin
  inherited Create(TFoldingBar);
  if Control is TCustomFoldingView then
    FView := TCustomFoldingView(Control);
end;

procedure TFoldingBars.Assign(Source: TPersistent);
var
  Bars: TFoldingBars absolute Source;
  I: Integer;
begin
  if Source is TFoldingBars then
  begin
    BeginUpdate;
    Clear;
    for I := 0 to Bars.Count - 1 do
      Add.Assign(Bars[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

function TFoldingBars.Add: TFoldingBar;
begin
  Result := TFoldingBar(inherited Add);
end;

function TFoldingBars.Insert(Index: Integer): TFoldingBar;
begin
  Result := TFoldingBar(inherited Insert(Index));
end;

procedure TFoldingBars.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FView <> nil then
    FView.BarsChange;
end;

function TFoldingBars.Get(Index: Integer): TFoldingBar;
begin
  if (Index > -1) and (Index < Count) then
    Result := GetItem(Index) as TFoldingBar
  else
    Result := nil;
end;

procedure TFoldingBars.Put(Index: Integer; Value: TFoldingBar);
begin
  SetItem(Index, Value);
end;

{ TCustomFoldingView }

constructor TCustomFoldingView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentBackground := True;
  ControlStyle := ControlStyle + [csParentBackground];
  FBarHotIndex := -1;
  FBarDownIndex := -1;
  FItemHotIndex := -1;
  FItemDownIndex := -1;
  Height := 250;
  Width := 150;
  FBars := TFoldingBars.Create(Self);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImagesChange;
end;

destructor TCustomFoldingView.Destroy;
begin
  UpdateImages(FBarImages, nil);
  UpdateImages(FItemImages, nil);
  FBars.Free;
  FChangeLink.Free;
  inherited Destroy;
end;

function TCustomFoldingView.AbsoluteIndex(Bar: TFoldingBar): Integer;
var
  Found: Boolean;
  I: Integer;
begin
  Found := False;
  Result := -1;
  if not Bar.Visible then Exit;
  for I := 0 to Bars.Count - 1 do
  begin
    if Bars[I].Visible then
      Inc(Result);
    Found := Bar = Bars[I];
    if Found then Break;
  end;
  if not Found then Result := -1;
end;

function TCustomFoldingView.GetFoldingProvider: IFoldingProvider;
begin
  if not Supports(Provider, IFoldingProvider, Result) then
    Result := nil;
end;

procedure TCustomFoldingView.ImagesChange(Sender: TObject);
begin
  Repaint;
end;

procedure TCustomFoldingView.BarsChange;
var
  Found: Boolean;
  I: Integer;
begin
  FBarHotIndex := -1;
  FBarDownIndex := -1;
  FItemHotIndex := -1;
  FItemDownIndex := -1;
  Found := False;
  if FSelected <> nil then
    for I := 0 to FBars.Count - 1 do
      if FBars[I] = FSelected then
      begin
        Found := True;
        Break;
      end;
  if not Found then
    FSelected := nil;
  Repaint;
end;

procedure TCustomFoldingView.DoItemClick(Item: TFoldingItem);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Item);
end;

procedure TCustomFoldingView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
end;

procedure TCustomFoldingView.Loaded;
begin
  inherited Loaded;
end;

procedure TCustomFoldingView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    SetFocus;
    if FBarHotIndex > -1 then
    begin
      FBarDownIndex := FBarHotIndex;
      InvalidateRect(Handle, @FBarRect, True);
    end
    else if FItemHotIndex > -1 then
    begin
      FItemDownIndex := FItemHotIndex;
      InvalidateRect(Handle, @FItemRect, False);
    end;
  end;
end;

procedure TCustomFoldingView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  BarHotIndex, ItemHotIndex: Integer;
  BarRect, ItemRect: TRect;
  //TrackObject: TObject;
  P: TPoint;
  I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  BarHotIndex := -1;
  ItemHotIndex := -1;
  P := Point(X, Y);
  for I := 0 to FBars.Count - 1 do
    if PtInRect(FBars[I].FRect, P) then
    begin
      BarHotIndex := I;
      BarRect := FBars[I].FRect;
      Break;
    end;
  if FSelected <> nil then
    for I := 0 to FSelected.Items.Count - 1 do
      if PtInRect(FSelected.Items[I].FRect, P) then
      begin
        ItemHotIndex := I;
        ItemRect := FSelected.Items[I].FRect;
        Break;
      end;
  //TrackObject := Self;
  if BarHotIndex <> FBarHotIndex then
  begin
    if FBarHotIndex > -1 then
      InvalidateRect(Handle, @FBarRect, True);
    FBarHotIndex := BarHotIndex;
    FBarRect := BarRect;
    if FBarHotIndex > -1 then
    begin
      InvalidateRect(Handle, @FBarRect, True);
      //TrackObject := FBars[FBarHotIndex];
    end;
    //else
      //TrackObject := nil;
  end;
  if ItemHotIndex <> FItemHotIndex then
  begin
    if FItemHotIndex > -1 then
      InvalidateRect(Handle, @FItemRect, False);
    FItemHotIndex := ItemHotIndex;
    FItemRect := ItemRect;
    if FItemHotIndex > -1 then
    begin
      InvalidateRect(Handle, @FItemRect, False);
     // TrackObject := FSelected.Items[I];
    end;
    //else if TrackObject = Self then
      //TrackObject := nil;
  end;
  {if TrackObject <> Self then
    DoMouseTrack(TrackObject);}
end;

procedure TCustomFoldingView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    if (FBarHotIndex > -1) and (FBarHotIndex = FBarDownIndex) then
    begin
      Selected := Bars[FBarDownIndex];
      InvalidateRect(Handle, @FBarRect, True);
    end
    else if FItemHotIndex > -1 then
    begin
      InvalidateRect(Handle, @FItemRect, False);
      if FItemDownIndex = FItemHotIndex then
      begin
        I := FItemDownIndex;
        FItemDownIndex := -1;
        DoItemClick(Selected.Items[I]);
      end;
    end;
    FBarDownIndex := -1;
    FItemDownIndex := -1;
  end;
end;

procedure TCustomFoldingView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent <> nil) then
    if AComponent = FBarImages then
      UpdateImages(FBarImages, nil)
    else if AComponent = FItemImages then
      UpdateImages(FItemImages, nil);
end;

procedure TCustomFoldingView.UpdateImages(var InternalImages: TCustomImageList;
  ExternalImages: TCustomImageList);
begin
  if InternalImages <> nil then
  begin
    InternalImages.UnRegisterChanges(FChangeLink);
    InternalImages.RemoveFreeNotification(Self);
  end;
  InternalImages := ExternalImages;
  if InternalImages <> nil then
  begin
    InternalImages.RegisterChanges(FChangeLink);
    InternalImages.FreeNotification(Self);
  end;
  Repaint;
end;

procedure TCustomFoldingView.Paint;
var
  BarHeight, ItemHeight: Integer;
  Bar: TFoldingBar;
  Item: TFoldingItem;
  DC: HDC;
  S: TDrawState;
  R, Back: TRect;
  I, J: Integer;
begin
  if FoldingProvider = nil then Exit;
  FillChar(R, SizeOf(R), #0);
  { Clear rectangles }
  for I := 0 to FBars.Count - 1 do
  begin
    Bar := FBars[I];
    Bar.FRect := R;
    for J := 0 to Bar.Items.Count - 1 do
    begin
      Item := Bar.Items[J];
      Item.FRect := R;
    end;
  end;
  BarHeight := FoldingProvider.GetBarHeight(Self);
  ItemHeight := FoldingProvider.GetItemHeight(Self);
  R := ClientRect;
  R.Bottom := BarHeight;
  { Calculate upper bar rectangles }
  for I := 0 to FBars.Count - 1 do
  begin
    Bar := FBars[I];
    if not Bar.Visible then Continue;
    Bar.FRect := R;
    Slide(R);
  end;
  R := ClientRect;
  R.Top := R.Bottom - BarHeight;
  J := ClientHeight;
  { Calculate lower bar rectangles }
  if FSelected <> nil then
    for I := FBars.Count - 1 downto 0  do
    begin
      Bar := FBars[I];
      if Bar = FSelected then
        Break;
      if not Bar.Visible then Continue;
      Bar.FRect := R;
      J := R.Top;
      Slide(R, drUp);
    end;
  { Calculate item rectangles }
  if (FSelected <> nil) and FSelected.Visible then
  begin
    R := FSelected.FRect;
    Slide(R);
    R.Bottom := R.Top + ItemHeight;
    for I := 0 to FSelected.Items.Count - 1 do
    begin
      if R.Top > J then
        Break;
      Item := FSelected.Items[I];
      if not Item.Visible then
        Continue;
      Item.FRect := R;
      Slide(R);
    end;
  end;
  { Draw }
  DC := Canvas.Handle;
  Back := ClientRect;
  {if DoubleBuffered then                  }
  FoldingProvider.DrawBarBkgnd(Self, Back);
  Back.Top := -1;
  Back.Bottom := -1;
  for I := 0 to FBars.Count - 1 do
  begin
    Bar := FBars[I];
    if not Bar.Visible then Continue;
    R := Bar.FRect;
    if Bar = FSelected then
      Back.Top := R.Top
    else if (Back.Top > -1) and (Back.Bottom = -1) then
      Back.Bottom := R.Top;
    S := [];
    if not Enabled then
      Include(S, dsDisabled)
    else if Focused then
      Include(S, dsFocused);
    if (Bar = FSelected) then
      Include(S, dsSelected);
    if I = FBarHotIndex then
      Include(S, dsHot);
    if I = FBarDownIndex then
      Include(S, dsPressed);
    FoldingProvider.DrawBar(Self, Bar, R, S);
    SelectClipRect(DC, R, RGN_DIFF);
  end;
  if Back.Bottom = -1 then
    Back.Bottom := ClientHeight;
  if (FSelected <> nil) and FSelected.Visible then
  begin
    FoldingProvider.DrawItemBkgnd(Self, Back);
    for I := 0 to FSelected.Items.Count - 1 do
    begin
      Item := FSelected.Items[I];
      R := Item.FRect;
      if HeightOf(R) < 1 then Continue;
      S := [];
      if not Enabled then
        Include(S, dsDisabled);
      if not Item.Enabled then
        Include(S, dsDisabled)
      else if Focused then
        Include(S, dsFocused);
      {if (Item = FSelected) then
        Include(S, dsSelected);}
      if I = FItemHotIndex then
        Include(S, dsHot);
      if I = FItemDownIndex then
        Include(S, dsPressed);
       FoldingProvider.DrawItem(Self, Item, R, S);
    end;
  end;
end;

procedure TCustomFoldingView.SetBarImages(Value: TCustomImageList);
begin
  if Value <> FBarImages then
  begin
    UpdateImages(FBarImages, Value);
    ImagesChange(FBarImages);
  end;
end;

procedure TCustomFoldingView.SetBars(Value: TFoldingBars);
begin
  if Value <> FBars then
    FBars.Assign(Value);
end;

procedure TCustomFoldingView.SetItemImages(Value: TCustomImageList);
begin
  if Value <> FItemImages then
  begin
    UpdateImages(FItemImages, Value);
    ImagesChange(FItemImages);
  end;
end;

procedure TCustomFoldingView.SetBoldText(const Value: string);
begin
  if Value <> FBoldText then
  begin
    FBoldText := Value;
    Invalidate;
  end;
end;

procedure TCustomFoldingView.SetSelected(Value: TFoldingBar);
var
  P: TSmallPoint;
begin
  if (Value.Collection = FBars) and (Value <> FSelected) then
  begin
    FSelected := Value;
    FBarHotIndex := -1;
    FBarDownIndex := -1;
    FItemHotIndex := -1;
    FItemDownIndex := -1;
    Repaint;
    P := PointToSmallPoint(ScreenToClient(Mouse.CursorPos));
    if HandleAllocated then
      PostMessage(Handle, WM_MOUSEMOVE, 0, Integer(P));
  end;
end;

procedure TCustomFoldingView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FBarHotIndex > -1 then
  begin
    FBarHotIndex := -1;
    InvalidateRect(Handle, @FBarRect, True);
  end;
  if FItemHotIndex > -1 then
  begin
    FItemHotIndex := -1;
    InvalidateRect(Handle, @FItemRect, True);
  end
end;

procedure TCustomFoldingView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TCustomFoldingView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

initialization
  RegisterDefaultProvider(TDefaultFoldingProvider, TFoldingView);
  RegisterProvider(TGelFoldingProvider, [TFoldingView]);
end.
