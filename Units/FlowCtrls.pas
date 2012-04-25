
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2007                   *)
(*                                                      *)
(********************************************************)

unit FlowCtrls;

interface

{$I CODEBOT.INC}

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, ImgList, Forms,
  Menus, ActnList, BlendTools, BaseTypes, GraphTools, FormTools, WinTools,
  BtnCtrls, DebugNotes, Dialogs, StrTools, XMLObjects;

{ TFlowSite }

type
	TFlowSite = class(TWinControl, IUnknown, IIgnoreResize, IIgnoreMargin)
  private
    FBeveled: Boolean;
  	FDockControls: TList;
    FSize: Integer;
    function CalculateDockPoint(X, Y: Integer): TPoint;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetBeveled(Value: Boolean);
    procedure SetSize(Value: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure AlignFlowBars; virtual;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function ValidateDock(const Rect: TRect; var Point: TPoint): Boolean;
	public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustAlignment;
  	procedure DockControl(Control: TControl; X, Y: Integer);
    procedure UndockControl(Control: TControl);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
	published
  	property Align;
    property Size: Integer read FSize write SetSize;
    property Beveled: Boolean read FBeveled write SetBeveled;
	end;

{ TFlowButton }

  TFlowButton = class(TCollectionItem)
  private
    FNotifier: TComponentNotifier;
  	FAction: TBasicAction;
    FActionLink: TActionLink;
    FCaption: TCaption;
    FDown: Boolean;
    FEnabled: Boolean;
    FGrouped: Boolean;
    FHint: string;
    FImageIndex: Integer;
    FName: string;
    FPopupMenu: TPopupMenu;
    FVisible: Boolean;
    FSeparator: Boolean;
    FToggle: Boolean;
    procedure FreeNotify(Sender: TObject);
    function GetImageButton: TImageButton;
    procedure SetAction(Value: TBasicAction);
    function GetButton: Integer;
    procedure SetEnabled(Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(Value: Integer);
    procedure SetCaption(const Value: TCaption);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure SetSeparator(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    function GetDown: Boolean;
    procedure SetDown(Value: Boolean);
    procedure SetGrouped(Value: Boolean);
    procedure SetToggle(Value: Boolean);
  protected
    property ImageButton: TImageButton read GetImageButton;
  public
		constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
		procedure ActionChange(Sender: TObject);
	published
  	property Action: TBasicAction read FAction write SetAction;
  	property Button: Integer read GetButton;
  	property Caption: TCaption read FCaption write SetCaption;
    property Down: Boolean read GetDown write SetDown;
  	property Enabled: Boolean read FEnabled write SetEnabled;
    property Grouped: Boolean read FGrouped write SetGrouped;
    property Hint: string read FHint write SetHint;
  	property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Name: string read FName write FName;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Separator: Boolean read FSeparator write SetSeparator;
    property Toggle: Boolean read FToggle write SetToggle;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TFlowButtonActionLink }

  TFlowButtonActionLink = class(TActionLink)
  private
    FClient: TFlowButton;
	protected
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetHint(const Value: string); override;
    procedure SetImageIndex(Value: Integer); override;
    procedure SetVisible(Value: Boolean); override;
    procedure SetOnExecute(Value: TNotifyEvent); override;
  end;

{ TFlowButtons }

  TFlowButtons = class(TOwnedCollection)
  private
    function Get(Index: Integer): TFlowButton;
    procedure Put(Index: Integer; Value: TFlowButton);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure Assign(Source: TPersistent); override;
    procedure SaveToXML(Node: INode);
    procedure LoadFromXML(Node: INode);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    function Add: TFlowButton;
    function FindItemID(ID: Integer): TFlowButton;
    function Insert(Index: Integer): TFlowButton;
    property Items[Index: Integer]: TFlowButton read Get write Put; default;
  end;

{ TCustomFlowBar }

type
  TButtonClickEvent = procedure(Sender: TObject; Button: Integer) of object;
  TOrientation = (orHorizontal, orVertical);

	TCustomFlowBar = class(TWinControl)
  private
    FDesignBounding: Boolean;
    FDocked: Boolean;
    FUnaligned: Boolean;
    FShadowTracker: THotTracker;
    FDockTracker: THotTracker;
    FDockAlign: TOrientation;
    FAllowFocus: Boolean;
    FButtons: TFlowButtons;
    FDockTarget: TObject;
    FDockPoint: TPoint;
    FDragable: Boolean;
    FHotLight: Boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FNewStyle: Boolean;
    FOrientation: TOrientation;
    FShowCaptions: Boolean;
    FPadding: Integer;
    FShadow: Boolean;
    FWide: Boolean;
    FOnButtonClick: TButtonClickEvent;
		procedure AlignButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonDraw(Control: TControl; Rect: TRect;
	    DrawState: TDrawState; var DefaultDraw: Boolean);
    procedure ButtonContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure SetDragable(Value: Boolean);
    function GetFlowSite: TFlowSite;
    procedure SetFlowSite(Value: TFlowSite);
    procedure SetOrientation(Value: TOrientation);
    procedure ImageListChange(Sender: TObject);
    procedure ValidateHotTracker;
    procedure SetAllowFocus(Value: Boolean);
    procedure SetButtons(Value: TFlowButtons);
    procedure SetImages(Value: TCustomImageList);
    procedure SetHotLight(Value: Boolean);
    procedure SetShowCaptions(Value: Boolean);
    procedure SetNewStyle(Value: Boolean);
    procedure SetPadding(Value: Integer);
    procedure SetShadow(Value: Boolean);
    function GetSize: Integer;
    procedure SetWide(Value: Boolean);
		procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure WMActivateApp(var Msg: TWMActivateApp); message WM_ACTIVATEAPP;
    procedure WMEnable(var Msg: TWMEnable); message WM_ENABLE;
		procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
		procedure WMNCActivate(var Msg: TMessage); message WM_NCACTIVATE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Flow(Control: TWinControl);
    procedure Unflow;
    procedure DoButtonClick(Button: Integer); dynamic;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintWindow(DC: HDC); override;
		procedure SetParent(AParent: TWinControl); override;
    property AllowFocus: Boolean read FAllowFocus write SetAllowFocus default False;
    property Dragable: Boolean read FDragable write SetDragable default True;
    property Buttons: TFlowButtons read FButtons write SetButtons;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property FlowSite: TFlowSite read GetFlowSite write SetFlowSite default nil;
    property HotLight: Boolean read FHotLight write SetHotLight default False;
  	property Images: TCustomImageList read FImages write SetImages;
    property NewStyle: Boolean read FNewStyle write SetNewStyle default False;
    property Orientation: TOrientation read FOrientation write SetOrientation default orHorizontal;
    property Padding: Integer read FPadding write SetPadding default 0;
    property Shadow: Boolean read FShadow write SetShadow;
    property Size: Integer read GetSize;
    property Wide: Boolean read FWide write SetWide;
  	property OnButtonClick: TButtonClickEvent read FOnButtonClick write FOnButtonClick;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

{ TFlowBar }

  TFlowBar = class(TCustomFlowBar)
  published
    property AllowFocus;
	  property Enabled;
  	property ParentShowHint;
    property ShowCaptions;
    property ShowHint;
    property Buttons;
    property Dragable;
    property FlowSite;
    property HotLight;
  	property Images;
    property Padding;
    property NewStyle;
  	property Visible;
    property Wide;
  	property OnButtonClick;
  end;

implementation

var
  InternalFlowParent: TCustomForm;

procedure DebugNote(const S: string);
begin
end;

type
  TFlowParent = class(TCustomForm)
  public
    destructor Destroy; override;
  end;

destructor TFlowParent.Destroy;
var
  C: TControl;
  I: Integer;
begin
  DebugNote('TFlowParent.Destroy');
  for I := ControlCount - 1 downto 0 do
  begin
    C := Controls[I];
    if C.Owner is TWinControl then
      C.Parent := TWinControl(C.Owner);
  end;
  inherited Destroy;
end;

function FlowParent: TCustomForm;
begin
  if InternalFlowParent = nil then
  begin
    InternalFlowParent := TFlowParent.CreateNew(Application);
    with InternalFlowParent do
    begin
      BorderStyle := bsNone;
      Width := 0;
      Height := 0;
      Show;
    end;
  end;
  Result := InternalFlowParent;
end;

const
	AlignBool: array[Boolean] of TOrientation = (orHorizontal, orVertical);

function ConvertAlign(Align: TAlign): TOrientation;
begin
	Result := AlignBool[Align in [alLeft, alRight]];
end;

{ TFlowSite }

const
	FlowAligns = [alTop..alRight];

constructor TFlowSite.Create(AOwner: TComponent);
begin
  FDockControls := TList.Create;
  inherited Create(AOwner);
	Align := alTop;
	ControlStyle := [];
end;

destructor TFlowSite.Destroy;
begin
  DebugNote('TFlowSite.Destroy');
  FDockControls.Free;
  inherited Destroy;
end;

procedure TFlowSite.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    style := style and (not (CS_HREDRAW	or CS_VREDRAW));
end;

function TFlowSite.CalculateDockPoint(X, Y: Integer): TPoint;
begin
	Result := Point(0, 0);
  if not (Align in FlowAligns) then Exit;
	Result := ClientToScreen(Point(0, 0));
	if Align in [alTop, alBottom] then
	begin
    if X - Result.X < -1 then
    	X := Result.X - 1
		else if X - Result.X > Width then
    	X := Result.X + Width;
		Y := Result.Y;
    Result := Point(X, Y);
  end
  else
  begin
    if Y - Result.Y < -1 then
    	Y := Result.Y - 1
		else if Y - Result.Y > Height then
    	Y := Result.Y + Height;
		X := Result.X;
    Result := Point(X, Y);
  end;
end;

const
  SiteDesignSize = 4;

procedure TFlowSite.AlignControls(AControl: TControl; var Rect: TRect);
var
	CalcSize, NewSize: Integer;
	O: TOrientation;
  F: TCustomFlowBar;
	I: Integer;
begin
  DebugNote('TFlowSite.AlignControls');
  if not (Align in FlowAligns) then
  begin
  	inherited AlignControls(AControl, Rect);
    Exit;
  end;
  DisableAlign;
  if Align in [alLeft, alRight] then
  	O := orVertical
	else
  	O := orHorizontal;
  for I := 0 to FDockControls.Count - 1 do
  	if TObject(FDockControls[I]) is TCustomFlowBar then
    begin
			F := TCustomFlowBar(FDockControls[I]);
			F.Orientation := O;
		end;
	CalcSize := 0;
	if FDockControls.Count = 0 then
  	CalcSize := FSize;
  if (csDesigning in ComponentState) and (CalcSize < 4) then
		CalcSize := SiteDesignSize;
	for I := 0 to FDockControls.Count - 1 do
	begin
    if not TControl(FDockControls[I]).Visible then
      Continue;
		if Align in [alLeft, alRight] then
			NewSize := TControl(FDockControls[I]).Width
		else
			NewSize := TControl(FDockControls[I]).Height;
    if NewSize > CalcSize then
    	CalcSize := NewSize;
  end;
	if Align in [alLeft, alRight] then
		Width := CalcSize
	else
		Height := CalcSize;
  EnableAlign;
  AlignFlowBars;
end;

procedure TFlowSite.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  DebugNote('TFlowSite.SetBounds');
  if (csDesigning in ComponentState) and (FDockControls.Count = 0) then
    if ConvertAlign(Align) = orHorizontal then
      AHeight := SiteDesignSize
    else
      AWidth := SiteDesignSize;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

function SortHorizontal(Item1, Item2: Pointer): Integer;
var
	A: TControl absolute Item1;
	B: TControl absolute Item2;
begin
	if A = B then
  	Result := 0
	else if A.Left < B.Left then
  	Result := -1
	else
  	Result := 1;
end;

function SortVertical(Item1, Item2: Pointer): Integer;
var
	A: TControl absolute Item1;
	B: TControl absolute Item2;
begin
	if A = B then
  	Result := 0
	else if A.Top < B.Top then
  	Result := -1
	else
  	Result := 1;
end;

procedure TFlowSite.AlignFlowBars;
var
	A, B: TControl;
	I, J: Integer;
begin
	if not ((Align in FlowAligns) and (FDockControls.Count > 0)) then Exit;
  DisableAlign;
  if Align in [alTop, alBottom] then
  begin
  	FDockControls.Sort(SortHorizontal);
    for I := 0 to FDockControls.Count - 1 do
    begin
    	A := TControl(FDockControls[I]);
      if not A.Visible then Continue;
      TFlowBar(A).TabOrder := I;
      if A.Left < 0 then
      	A.Left := 0;
			for J := I + 1 to FDockControls.Count - 1 do
      begin
        B := TControl(FDockControls[J]);
        if B.Left < A.Left + A.Width then
        	B.Left := A.Left + A.Width;
      end;
    end;
    for I := FDockControls.Count - 1 downto 0 do
    begin
    	A := TControl(FDockControls[I]);
      if not A.Visible then Continue;
      if A.Left + A.Width > Width then
      	A.Left := Width - A.Width;
			for J := I - 1 downto 0 do
      begin
        B := TControl(FDockControls[J]);
        if B.Left + B.Width > A.Left then
        	B.Left := A.Left - B.Width;
      end;
		end;
  end
  else
  begin
  	FDockControls.Sort(SortVertical);
    for I := 0 to FDockControls.Count - 1 do
    begin
    	A := TControl(FDockControls[I]);
      if not A.Visible then Continue;
      TFlowBar(A).TabOrder := I;
      if A.Top < 0 then
      	A.Top := 0;
			for J := I + 1 to FDockControls.Count - 1 do
      begin
        B := TControl(FDockControls[J]);
        if B.Top < A.Top + A.Height then
        	B.Top := A.Top + A.Height;
      end;
    end;
    for I := FDockControls.Count - 1 downto 0 do
    begin
    	A := TControl(FDockControls[I]);
      if not A.Visible then Continue;
      if A.Top + A.Height > Height then
      	A.Top := Height - A.Height;
			for J := I - 1 downto 0 do
      begin
        B := TControl(FDockControls[J]);
        if B.Top + B.Height > A.Top then
        	B.Top := A.Top - B.Height;
      end;
		end;
  end;
  EnableAlign;
end;

procedure TFlowSite.AdjustAlignment;
begin
 { case Align of
  	alLeft: Left := High(Word);
  	alTop: Top := -High(Word);
  	alRight: Left := High(Word);
  	alBottom: Top := -High(Word);
	end;}
end;

procedure TFlowSite.DockControl(Control: TControl; X, Y: Integer);
var
	OldSite: TFlowSite;
  P: TPoint;
begin
	if not ((Align in FlowAligns) and (FDockControls.IndexOf(Control) < 0)) then Exit;
	DisableAlign;
	if Control.Parent is TFlowSite then
  begin
    OldSite := TFlowSite(Control.Parent);
		OldSite.FDockControls.Remove(Control);
    OldSite.AdjustAlignment;
  end;
  FDockControls.Add(Control);
  if Control is TCustomFlowBar then
    (Control as TCustomFlowBar).Flow(Self)
  else
    Control.Parent := Self;
	if (X = 0) and (Y = 0) then
  	P := Point(0, 0)
	else
		P := ScreenToClient(Point(X, Y));
  if Align in [alTop, alBottom] then
  begin
  	if P.X < -1 then
	  	Control.Left := -1
		else if P.X > Width  then
    	Control.Left := Width
		else
    	Control.Left := P.X;
	  Control.Top := 0;
  end
  else
  begin
	  Control.Left := 0;
  	if P.Y < -1 then
	  	Control.Top := -1
		else if P.Y > Height then
    	Control.Top := Height
		else
    	Control.Top := P.Y;
  end;
  EnableAlign;
  AlignFlowBars;
  AdjustAlignment;
end;

procedure TFlowSite.UndockControl(Control: TControl);
begin
	if FDockControls.IndexOf(Control) = -1 then Exit;
	FDockControls.Remove(Control);
  if (Control.Parent = Self) and (Control is TCustomFlowbar) then
    (Control as TCustomFlowbar).Unflow;
  AdjustAlignment;
end;

function TFlowSite.ValidateDock(const Rect: TRect; var Point: TPoint): Boolean;
var
	A, B: TRect;
begin
	Result := False;
  if not (Enabled and Visible and (Align in FlowAligns)) then Exit;
  B := ClientRect;
	case Align of
		alLeft: if B.Left = B.Right then Inc(B.Right, 2);
    alTop: if B.Top = B.Bottom then Inc(B.Bottom, 2);
    alRight: if B.Left = B.Right then Dec(B.Left, 2);
    alBottom: if B.Top = B.Bottom then Dec(B.Top, 2);
	end;
  with ClientToScreen(Classes.Point(0, 0)) do
  	OffsetRect(B, X, Y);
  if IntersectRect(A, B, Rect) then
  begin
  	Point := CalculateDockPoint(Rect.Left, Rect.Top);
  	Result := True;
  end;
end;

procedure TFlowSite.SetBeveled(Value: Boolean);
begin
  if Value <> FBeveled then
  begin
    FBeveled := Value;
    if HandleAllocated then
      InvalidateWindows(Handle);
  end;
end;

procedure TFlowSite.SetSize(Value: Integer);
begin
	if Value < 0 then
  	Value := 0;
	if Value <> FSize then
  begin
	  FSize := Value;
    if FDockControls.Count = 0 then
    	Realign;
  end;
end;

procedure TFlowSite.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  Bitmap: TBitmap;
	FlowBar: TCustomFlowBar;
  P: HPEN;
  B: HBRUSH;
	R: TRect;
	I: Integer;
begin
	R := ClientRect;
  if csDesigning in ComponentState then
  begin
	  FillRectColor(Msg.DC, R, clBtnShadow);
		P := SelectObject(Msg.DC, GetPen(clWindowText, psDash));
    B := SelectObject(Msg.DC, GetBrush(clWindowText, bsFDiagonal));
		Rectangle(Msg.DC, R.Left, R.Top, R.Right, R.Bottom);
    OverwriteObject(Msg.DC, P);
    OverwriteObject(Msg.DC, B);
    if FDockControls.Count > 0 then
    begin
		  Bitmap := TBitmap.Create;
      try
			  for I := 0 to FDockControls.Count - 1 do
		  		if TObject(FDockControls[I]) is TCustomFlowBar then
	        begin
			    	FlowBar := TCustomFlowBar(FDockControls[I]);
			      Bitmap.Width := FlowBar.Width;
    		    Bitmap.Height := FlowBar.Height;
		  	    FlowBar.PaintTo(Bitmap.Canvas, 0, 0);
						BitBlt(Msg.DC, FlowBar.Left, FlowBar.Top, Bitmap.Width, Bitmap.Height,
    		    	Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
  	      end;
      finally
      	Bitmap.Free;
      end;
		end;
  end
  else
  begin
    Inc(R.Bottom, 5);
		if FDockControls.Count = 0 then
    	FillRectColor(Msg.DC, R, Color)
    else with ThemePainter do
			if Enabled then
		  	DrawElement(Msg.DC, GetDetails(trRebarRoot), R)
			else
		    FillRectColor(Msg.DC, R, Color);
    Dec(R.Bottom, 5);
    if FBeveled then
    begin
      case Align of
        alLeft: R.Left := R.Right - 1;
        alTop: R.Top := R.Bottom - 1;
        alRight: R.Right := R.Left + 1;
        alBottom: R.Bottom := R.Top + 1;
      end;
      FillRectColor(Msg.DC, R, Blend(clBtnFace, clBtnShadow, 65));
      case Align of
        alLeft: Slide(R, drLeft);
        alTop: Slide(R, drUp);
        alRight: Slide(R, drRight);
        alBottom: Slide(R, drDown);
      end;
      FillRectColor(Msg.DC, R, Blend(clBtnFace, clBtnShadow, 85));
    end;
	end;
	Msg.Result := 1;
end;

{ TFlowButton }

constructor TFlowButton.Create(Collection: TCollection);
begin
  FVisible := True;
  FEnabled := True;
  FImageIndex := Collection.Count;
  FNotifier := TComponentNotifier.CreateNotifier(@FNotifier, FreeNotify);
  inherited Create(Collection);
end;

destructor TFlowButton.Destroy;
begin
  FNotifier.Free;
  FActionLink.Free;
  inherited Destroy;
end;

procedure TFlowButton.FreeNotify(Sender: TObject);
begin
  if Sender = FPopupMenu then
    FPopupMenu := nil
  else if Sender = FAction then
    FAction := nil;
end;

function TFlowButton.GetImageButton: TImageButton;
var
  FlowBar: TCustomFlowBar;
  I: Integer;
begin
  Result := nil;
  if (Collection <> nil) and (Collection.Owner is TCustomFlowBar) then
  begin
    FlowBar := TCustomFlowBar(Collection.Owner);
    I := Index;
    if (I < FlowBar.ControlCount) and (FlowBar.Controls[I] is TImageButton) then
      Result := TImageButton(FlowBar.Controls[I]);
  end;
end;

procedure TFlowButton.Assign(Source: TPersistent);
var
  EditItem: TFlowButton absolute Source;
begin
  if Source is TFlowButton then
  begin
    FEnabled := EditItem.FEnabled;
    FHint := EditItem.FHint;
    FImageIndex := EditItem.FImageIndex;
    FVisible := EditItem.FVisible;
		Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TFlowButton.ActionChange(Sender: TObject);
begin
  if (Sender = FAction) and (FAction is TCustomAction) then
    with TCustomAction(FAction) do
	  begin
			FCaption := Caption;
			FEnabled := Enabled;
			FHint := Hint;
			FImageIndex := ImageIndex;
			FVisible := Visible;
	    Changed(False);
		end;
end;

procedure TFlowButton.SetAction(Value: TBasicAction);
begin
  if FNotifier <> nil then
    FNotifier.AddFreeNotifier(Value);
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
    FAction := nil;
  end
  else
  begin
  	FAction := Value;
    if FActionLink = nil then
      FActionLink := TFlowButtonActionLink.Create(Self);
    FActionLink.Action := FAction;
    FActionLink.OnChange := ActionChange;
    ActionChange(FAction);
  end;
end;

function TFlowButton.GetButton: Integer;
begin
	Result := ID;
end;

procedure TFlowButton.SetCaption(const Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;


function TFlowButton.GetDown: Boolean;
var
  B: TImageButton;
begin
  Result := FDown;
  B := ImageButton;
  if B <> nil then
    Result := B.Down;
end;

procedure TFlowButton.SetGrouped(Value: Boolean);
var
  B: TImageButton;
begin
  FGrouped := Value;
  B := ImageButton;
  if B <> nil then
    if FGrouped then
      B.Options := B.Options + [boGrouped]
    else
      B.Options := B.Options - [boGrouped];
end;

procedure TFlowButton.SetDown(Value: Boolean);
var
  B: TImageButton;
begin
  FDown := Value;
  B := ImageButton;
  if B <> nil then
  begin
    B.Down := FDown;
    FDown := B.Down;
  end;
end;

procedure TFlowButton.SetEnabled(Value: Boolean);
var
  B: TImageButton;
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    B := ImageButton;
    if B <> nil then
      B.Enabled := FEnabled;
    // Changed(False);
  end;
end;

procedure TFlowButton.SetHint(const Value: string);
var
  B: TImageButton;
begin
  if Value <> FHint then
  begin
    FHint := Value;
    B := ImageButton;
    if B <> nil then
      B.Hint := FHint;
  end;
end;

procedure TFlowButton.SetImageIndex(Value: Integer);
var
  B: TImageButton;
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    B := ImageButton;
    if B <> nil then
      B.ImageIndex := FImageIndex;
  end;
end;

procedure TFlowButton.SetPopupMenu(Value: TPopupMenu);
begin
  if FNotifier <> nil then
    FNotifier.AddFreeNotifier(Value);
  if Value <> FPopupMenu then
  begin
    FPopupMenu := Value;
    Changed(False);
  end;
end;

procedure TFlowButton.SetSeparator(Value: Boolean);
begin
  if Value <> FSeparator then
  begin
    FSeparator := Value;
    Changed(False);
  end;
end;

procedure TFlowButton.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

procedure TFlowButton.SetToggle(Value: Boolean);
var
  B: TImageButton;
begin
  FToggle := Value;
  B := ImageButton;
  if B <> nil then
    if FToggle then
      B.Options := B.Options + [boToggle]
    else
      B.Options := B.Options - [boToggle];
end;

{ TFlowButtonActionLink }

procedure TFlowButtonActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TFlowButton;
end;

function TFlowButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
		(FClient.Caption = (Action as TCustomAction).Caption);
end;

function TFlowButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    (FClient.Down = (Action as TCustomAction).Checked);
end;

function TFlowButtonActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and
    (FClient.Enabled = (Action as TCustomAction).Enabled);
end;

function TFlowButtonActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and
    (FClient.Hint = (Action as TCustomAction).Hint);
end;

function TFlowButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;

function TFlowButtonActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and
    (FClient.Visible = (Action as TCustomAction).Visible);
end;

function TFlowButtonActionLink.IsOnExecuteLinked: Boolean;
begin
	Result := True;
end;

procedure TFlowButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then FClient.Caption := Value;
end;

procedure TFlowButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then FClient.Down := Value;
end;

procedure TFlowButtonActionLink.SetEnabled(Value: Boolean);
begin
  if IsEnabledLinked then FClient.Enabled := Value;
end;

procedure TFlowButtonActionLink.SetHint(const Value: string);
begin
  if IsHintLinked then FClient.Hint := Value;
end;

procedure TFlowButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then FClient.ImageIndex := Value;
end;

procedure TFlowButtonActionLink.SetVisible(Value: Boolean);
begin
  if IsVisibleLinked then FClient.Visible := Value;
end;

procedure TFlowButtonActionLink.SetOnExecute(Value: TNotifyEvent);
begin
  FClient.Changed(False);
end;

{ TFlowButtons }

constructor TFlowButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TFlowButton);
end;

function TFlowButtons.Add: TFlowButton;
begin
  Result := TFlowButton(inherited Add);
end;

procedure TFlowButtons.Assign(Source: TPersistent);
var
	Strings: TStrings absolute Source;
	Document: IDocument;
begin
	if Source is TStrings then
  begin
  	Document := CreateDocument;
    Document.Text := Strings.Text;
    LoadFromXML(Document.Root);
  end
  else
  	inherited Assign(Source);
end;

procedure TFlowButtons.AssignTo(Dest: TPersistent);
var
	Strings: TStrings absolute Dest;
	Document: IDocument;
begin
	if Dest is TStrings then
  begin
  	Document := CreateDocument;
	  Document.Root := Document.CreateNode('FlowBar');
  	SaveToXML(Document.Root);
	  Strings.Text := Document.Text;
  end
  else
  	inherited;
end;

function TFlowButtons.FindItemID(ID: Integer): TFlowButton;
begin
  Result := TFlowButton(inherited FindItemID(ID));
end;

function TFlowButtons.Insert(Index: Integer): TFlowButton;
begin
  Result := TFlowButton(GetItem(Index));
end;

procedure TFlowButtons.Update(Item: TCollectionItem);
begin
  DebugNote('');
  DebugNote('Update 1 .. Count: ' + IntToStr(Count));
  inherited Update(Item);
  DebugNote('Update 2 .. Count: ' + IntToStr(Count));
  if GetOwner is TCustomFlowBar then
  	TCustomFlowBar(GetOwner).AlignButtons;
  DebugNote('Update 3 .. Count: ' + IntToStr(Count));
end;

function TFlowButtons.Get(Index: Integer): TFlowButton;
begin
  Result := TFlowButton(GetItem(Index));
end;

procedure TFlowButtons.Put(Index: Integer; Value: TFlowButton);
begin
  SetItem(Index, Value);
end;

procedure TFlowButtons.SaveToFile(const FileName: string);
var
	Document: IDocument;
begin
	Document := CreateDocument;
  Document.Root := Document.CreateNode('FlowBar');
  SaveToXML(Document.Root);
  Document.SaveToFile(FileName);
end;

procedure TFlowButtons.SaveToXML(Node: INode);
var
	Nodes: INodes;
  Child: INode;
  Filer: IFiler;
  Item: TFlowButton;
  I: Integer;
begin
	Nodes := Node.Nodes;
	for I := 0 to Count - 1 do
  begin
  	Item := Items[I];
    Child := Nodes.Add('button');
  	Filer := Child.Attributes.Filer;
    Filer.WriteInteger('id', Item.Button);
    Filer.WriteBool('enabled', Item.Enabled);
    Filer.WriteInteger('image', Item.ImageIndex);
    {if Item.MenuArrow then
	    Filer.WriteString('kind', 'menu')
		else
	    Filer.WriteString('kind', 'normal');}
    Filer.WriteString('name', Item.Name);
    if Item.Separator then
	    Filer.WriteBool('separator', Item.Separator);
    Filer.WriteBool('visible', Item.Visible);
    Filer := Child.Filer;
    Filer.WriteString('caption', Item.Caption);
    Filer.WriteString('hint', Item.Hint);
  end;
end;

procedure TFlowButtons.LoadFromFile(const FileName: string);
var
	Document: IDocument;
begin
	Document := CreateDocument;
  Document.LoadFromFile(FileName);
  Clear;
	LoadFromXML(Document.Root);
end;

procedure TFlowButtons.LoadFromXML(Node: INode);
var
	Nodes: INodes;
  Child: INode;
  Filer: IFiler;
  Item: TFlowButton;
  I: Integer;
begin
	if (Node = nil) or (Node.Name <> 'FlowBar') then Exit;
  Nodes := Node.FindNodes('button');
  if Nodes = nil then Exit;
  PInteger(@NextID)^ := 0;
	BeginUpdate;
  try
  	for I := 0 to Nodes.Count - 1 do
    begin
	  	Item := Add;
    	Child := Nodes[I];
      Filer := Child.Attributes.Filer;
  	  Item.Enabled := Filer.ReadBool('enabled');
    	Item.ImageIndex := Filer.ReadInteger('image');
      //Item.MenuArrow := Filer.ReadString('kind') = 'menu';
      Item.Name := Filer.ReadString('name');
      Item.Separator := Filer.ReadBool('separator');
	    Item.Visible := Filer.ReadBool('visible');
  	  Filer := Child.Filer;
	    Item.Caption := Filer.ReadString('caption');
	    Item.Hint := Filer.ReadString('hint');
    end;
  finally
  	EndUpdate;
  end;
end;

{ TFlowDesigner }

type
	IFlowDesigner = interface(IDesignerHook)
	['{A0637657-5823-40E9-A7FC-344491DA2129}']
  end;

	TFlowDesigner = class(TInterfacedObject, IDesignerNotify, IDesignerHook,
  	IFlowDesigner)
  private
  	FOwner: TComponent;
  	FNotify: IDesignerHook;
	protected
  	{ IDesignerNotify }
    procedure Modified;
    procedure Notification(AnObject: TPersistent; Operation: TOperation);
  	{ IDesignerNotify }
    function GetCustomForm: TCustomForm;
    procedure SetCustomForm(Value: TCustomForm);
    function GetIsControl: Boolean;
    procedure SetIsControl(Value: Boolean);
    function IsDesignMsg(Sender: TControl; var Message: TMessage): Boolean;
    {$IFDEF D12}
    procedure PaintMenu;
    {$ENDIF}
    procedure PaintGrid;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string);
    function UniqueName(const BaseName: string): string;
    function GetRoot: TComponent;
	public
  	constructor Create(AOwner: TComponent; Notify: IDesignerHook);
	end;

constructor TFlowDesigner.Create(AOwner: TComponent; Notify: IDesignerHook);
begin
  inherited Create;
  FOwner := AOwner;
  FNotify := Notify;
end;

{ TFlowDesigner.IDesignerNotify }

procedure TFlowDesigner.Modified;
begin
	FNotify.Modified;
end;

procedure TFlowDesigner.Notification(AnObject: TPersistent; Operation: TOperation);
begin
	FNotify.Notification(AnObject, Operation);
end;

{ TFlowDesigner.IDesignerNotify }

function TFlowDesigner.GetCustomForm: TCustomForm;
begin
  Result := FNotify.GetCustomForm;
end;

function TFlowDesigner.GetIsControl: Boolean;
begin
  Result := FNotify.GetIsControl;
end;

function TFlowDesigner.GetRoot: TComponent;
begin
  Result := FNotify.GetRoot;
end;

function TFlowDesigner.IsDesignMsg(Sender: TControl;
  var Message: TMessage): Boolean;
begin
	Result := FNotify.IsDesignMsg(Sender, Message);
end;

{$IFDEF D12}
procedure TFlowDesigner.PaintMenu;
begin
  FNotify.PaintMenu;
end;
{$ENDIF}

procedure TFlowDesigner.PaintGrid;
var
  Bitmap: TBitmap;
	FlowBar: TCustomFlowBar;
  DC: HDC;
	I: Integer;
begin
	FNotify.PaintGrid;
	DC := GetDC(TCustomForm(FOwner).Handle);
	SelectClipRgn(DC, 0);
  Bitmap := TBitmap.Create;
  try
	  for I := 0 to FOwner.ComponentCount - 1 do
  		if FOwner.Components[I] is TCustomFlowBar then
    	begin
	    	FlowBar := TCustomFlowBar(FOwner.Components[I]);
        if FlowBar.Parent <> FOwner then Continue;
	      Bitmap.Width := FlowBar.Width;
        Bitmap.Height := FlowBar.Height;
  	    FlowBar.PaintTo(Bitmap.Canvas, 0, 0);
				BitBlt(DC, FlowBar.Left, FlowBar.Top, Bitmap.Width, Bitmap.Height,
        	Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
    	end;
	finally
  	Bitmap.Free;
		ReleaseDC(TCustomForm(FOwner).Handle, DC);
  end;
end;

procedure TFlowDesigner.SetCustomForm(Value: TCustomForm);
begin
	FNotify.SetCustomForm(Value);
end;

procedure TFlowDesigner.SetIsControl(Value: Boolean);
begin
	FNotify.SetIsControl(Value);
end;

function TFlowDesigner.UniqueName(const BaseName: string): string;
begin
	FNotify.UniqueName(BaseName);
end;

procedure TFlowDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
	FNotify.ValidateRename(AComponent, CurName, NewName);
end;

{ TCustomFlowBar }

const
	ButtonDefaultSize = 16;
  ButtonBorder = 4;
  ButtonSeparator = 6;
  ButtonGrip = 11;

constructor TCustomFlowBar.Create(AOwner: TComponent);
begin
  DebugNote('Create');
  inherited Create(AOwner);
  TabStop := False;
  FShadow := True;
  FDocked := True;
	FButtons := TFlowButtons.Create(Self);
  FDragable := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FUnaligned := True;
  DesktopFont := True;
end;

destructor TCustomFlowBar.Destroy;
begin
  DebugNote('Destroy');
	Images := nil;
  FreeAndNil(FDockTracker);
	FreeAndNil(FShadowTracker);
	FreeAndNil(FButtons);
	inherited Destroy;
end;

procedure TCustomFlowBar.Flow(Control: TWinControl);
begin
  DebugNote('Flow');
  if FDocked then
    Parent := Control
  else
  begin
    FDocked := True;
    Parent := Control;
    RecreateWnd;
  end;
end;

procedure TCustomFlowBar.Unflow;
begin
  DebugNote('Unflow');
  if FDocked then
  begin
    FDocked := False;
    if csDesigning in ComponentState then
      Parent := Owner as TCustomForm
    else
    begin
      Visible := False;
      Parent := FlowParent;
      RecreateWnd;
      Visible := True;
    end;
  end;
end;

procedure TCustomFlowBar.CreateParams(var Params: TCreateParams);
begin
  DebugNote('CreateParams');
  inherited CreateParams(Params);
  if csDesigning in ComponentState then Exit;
  with Params do
    if FDocked then
    begin
      WndParent := Parent.Handle;
    end
    else
    begin
      Style := WS_POPUP;
      ExStyle := WS_EX_TOOLWINDOW;
      WndParent := 0;
    end;
end;

procedure TCustomFlowBar.AlignButtons;
var
  Button: TImageButton;
	Item: TFlowButton;
  OldOptions: array of record
    O: TButtonOptions;
    D: Boolean;
  end;    
  Options: TButtonOptions;
  NewOptions: TButtonOptions;
  X: Integer;
	I: Integer;
begin
  DebugNote('AlignButtons');
  if FUnaligned or ([csDestroying, csLoading, csReading, csWriting] * ComponentState <> []) then
    Exit;
  DebugNote('AlignButtons 1');
  DisableAlign;
  SetLength(OldOptions, FButtons.Count);
  for I := 0 to FButtons.Count - 1 do
  begin
    OldOptions[I].O := [];
    Item := FButtons[I];
    if Item.Grouped then
      Include(OldOptions[I].O, boGrouped);
    if Item.Toggle then
      Include(OldOptions[I].O, boToggle);
    OldOptions[I].D := Item.Down;
  end;
  try
  	for I := ControlCount - 1 downto FButtons.Count do
    	Controls[I].Free;
	  for I := ControlCount to FButtons.Count - 1 do
    begin
	  	Button := TImageButton.Create(Self);
      Button.FocusedRect := False;
      Button.Parent := Self;
    end;
  finally
    EnableAlign;
  end;
  DebugNote('AlignButtons 2');
  if FDragable then
		X := ButtonGrip
	else
  	X := 2;
  Options := [boClean, boAutoSize];
  if FAllowFocus then
    Options := Options + [boAutoFocus];
  if FHotLight then
    Options := Options + [boOpaque];
  if FWide and (FOrientation = orVertical) then
    Options := Options + [boWide];
	for I := 0 to FButtons.Count - 1 do
  begin
  	Button := Controls[I] as TImageButton;
    Item := FButtons[I];
    if I < Length(OldOptions) then
      NewOptions := OldOptions[I].O
    else
      NewOptions := [];
    if Item.PopupMenu <> nil then
      Button.Options := Options + NewOptions + [boMenu]
    else
      Button.Options := Options  + NewOptions;
    if I < Length(OldOptions) - 1 then
      Button.Down := OldOptions[I].D;
    Button.Padding := FPadding;
    Button.Caption := Item.Caption;
    if FShowCaptions and (Button.Caption <> '') and (FOrientation = orHorizontal) then
	    Button.CaptionPosition := cpRight
		else
	    Button.CaptionPosition := cpHidden;
    Button.Hint := Item.Hint;
    Button.ImageIndex := Item.ImageIndex;
    Button.ParentShowHint := True;
    Button.PopupMenu := Item.PopupMenu;
		Button.Enabled := Item.Enabled and Enabled;
    Button.Visible := Item.Visible;
    Button.TabStop := FAllowFocus;
    if csDesigning in ComponentState then
      Button.Style := bsTransparent
    else
      Button.Style := bsFlat;
		Button.Tag := Item.Button;
    Button.Images := FImages;
    if Item.Action <> nil then
	    Button.OnClick := Item.Action.OnExecute
    else
	    Button.OnClick := ButtonClick;
    Button.OnCustomDraw := ButtonDraw;
    Button.OnContextPopup := ButtonContextPopup;
    Button.Options := Button.Options + [boGrouped];
		//TCustomFlowBar(Button).AdjustSize;
    if Item.Visible then
    begin
		  if FOrientation = orHorizontal then
      begin
	    	Button.Left := X;
  	    Button.Top := 2;
      	Inc(X, Button.Width);
			end
      else
      begin
	    	Button.Top := X;
  	    Button.Left := 2;
      	Inc(X, Button.Height);
			end;
      Button.Realign;
			if Item.Separator then
				Inc(X, ButtonSeparator);
    end;
  end;
  FDesignBounding := True;
  try
    if FOrientation = orHorizontal then
		  Width := X + 2
    else
  		Height := X + 2;
  finally
    FDesignBounding := False;
  end;
  DebugNote('AlignButtons 4');
  if (csDesigning in ComponentState) and (Parent is TFlowSite) then
    Parent.Invalidate;
  DebugNote('AlignButtons 5');
end;

procedure TCustomFlowBar.ButtonClick(Sender: TObject);
begin
  DebugNote('ButtonClick');
	DoButtonClick((Sender as TComponent).Tag);
end;

procedure TCustomFlowBar.ButtonDraw(Control: TControl; Rect: TRect;
  DrawState: TDrawState; var DefaultDraw: Boolean);
var
  Button: TImageButton absolute Control;
  Light: Boolean;
	DC: HDC;
begin
	DefaultDraw := not ((dsBackground in DrawState) and FNewStyle);
  if not DefaultDraw then
  begin
    Light := not (dsPressed in DrawState);
  	DC := Button.Canvas.Handle;
    if [dsPressed, dsHot] * DrawState <> [] then
    begin
    	DrawStyleRect(DC, Rect, Light);
      if boMenu in Button.Options then
      begin
        Rect.Left := Rect.Right - 13;
      	DrawStyleRect(DC, Rect, Light);
      end;
    end;
  end;
end;

procedure TCustomFlowBar.ButtonContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Button: TImageButton absolute Sender;
  Item: TFlowButton;
begin
  DebugNote('ButtonContextPopup');
  Handled := True;
  Item := FButtons[Button.Tag];
  if Item.PopupMenu <> nil then
  begin
    MousePos := Button.ClientToScreen(Point(0, 0));
    with MousePos do
    begin
      if FOrientation = orHorizontal then
        Y := Y + Button.Height + 1
      else
        X := X + Button.Width + 1;
      Item.PopupMenu.Popup(X, Y);
    end;      
  end;
end;

procedure TCustomFlowBar.ImageListChange(Sender: TObject);
begin
  DebugNote('ImageListChange');
	Realign;
	AlignButtons;
end;

procedure TCustomFlowBar.AlignControls(AControl: TControl; var Rect: TRect);
var
	I: Integer;
begin
  DebugNote('AlignControls');
  if csDestroying in ComponentState then Exit;
	if Align in [alLeft, alRight] then Exit;
  if ControlCount > 0 then
    if FOrientation = orHorizontal then
      I := Controls[0].Height
    else
      I := Controls[0].Width
	else if FImages = nil then
  	I := ButtonDefaultSize
	else
  	I := FImages.Height;
	if FOrientation = orHorizontal then
		if Height <> I + ButtonBorder * 2 then Height := I + ButtonBorder
    else
  else
		if Width <> I + ButtonBorder * 2 then Width := I + ButtonBorder;
end;

procedure TCustomFlowBar.DoButtonClick(Button: Integer);
begin
  DebugNote('DoButtonClick');
	if Assigned(FOnButtonClick) then
  	FOnButtonClick(Self, Button);
end;

procedure TCustomFlowBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  DebugNote('Notification ' + AComponent.ClassName);
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
		if (AComponent <> nil) and (AComponent = FImages) then
      Images := nil;
end;

procedure TCustomFlowBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  F: TFlowSite;
begin
  DebugNote('SetBounds');
  if csDesigning in ComponentState then
  begin
    F := FlowSite;
    if F <> nil then
      case ConvertAlign(F.Align) of
        orHorizontal: ATop := 0;
      else
        ALeft := 0;
      end;
    DebugNote('SetBounds 1');
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
    {if not FDesignBounding then
    begin
      FDesignBounding := True;
      try
        DebugNote('SetBounds 2');
      	Realign;
      	AlignButtons;
        if F <> nil then
          F.AlignFlowBars;
      finally
        FDesignBounding := False;
      end;
    end;}
  end
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TCustomFlowBar.PaintWindow(DC: HDC);
var
	Bitmap: TBitmap;
  Control: TWinControl;
	Rect : TRect;
	I: Integer;
begin
	Rect := ClientRect;
  if FDragable then
  begin
		if FOrientation = orHorizontal then
	  	Rect.Right := ButtonGrip
		else
	  	Rect.Bottom := ButtonGrip;
		InflateRect(Rect, -1, -1);
		DrawThemeGripper(DC, Rect, Color, FOrientation = orHorizontal);
  end;
	for I := 0 to Buttons.Count - 1 do
  	if Buttons[I].Separator then
    begin
    	Rect := Controls[I].BoundsRect;
      if FOrientation = orHorizontal then
      begin
	      InflateRect(Rect, 0, -2);
  	    Slide(Rect, drRight);
    	  Rect.Right := Rect.Left + ButtonSeparator;
      	with ThemePainter do
      		if Enabled then
		      	DrawElement(DC, GetDetails(ttbSeparatorNormal), Rect)
					else
						DrawSeparator(DC, Rect, Color, True);
			end
      else
      begin
	      InflateRect(Rect, -2, 0);
  	    Slide(Rect);
    	  Rect.Bottom := Rect.Top + ButtonSeparator;
      	with ThemePainter do
      		if Enabled then
		      	DrawElement(DC, GetDetails(ttbSeparatorVertNormal), Rect)
					else
						DrawSeparator(DC, Rect, Color, False);
      end;
	  end;
	if csDesigning in ComponentState then
  begin
		Bitmap := TBitmap.Create;
    try
			for I := 0 to ControlCount - 1 do
      	if Controls[I] is TWinControl then
        begin
        	Control := TWinControl(Controls[I]);
          if not Control.Visible then Continue;
          Bitmap.Width := Control.Width;
          Bitmap.Height := Control.Height;
          Control.PaintTo(Bitmap.Canvas, 0, 0);
					BitBlt(DC, Control.Left, Control.Top, Bitmap.Width, Bitmap.Height,
  	      	Bitmap.Canvas.Handle, 0, 0, SRCCOPY);
        end;
		finally
			Bitmap.Free;
    end;
		FillRectOutline(DC, ClientRect, clBtnShadow);
  end
  else
  begin
		if not FDocked then
      if FShadow then
  			FillRectOutline(DC, ClientRect, clBtnShadow)
      else
  			FillRectOutline(DC, ClientRect, Blend(cl3DDkShadow, clBtnShadow));
		ValidateHotTracker;
  end;
end;

procedure TCustomFlowBar.ValidateHotTracker;
begin
  DebugNote('ValidateHotTracker');
	if IsWindowEnabled(Handle) and (not FDocked) and (FShadow) then
  begin
    if FShadowTracker <> nil then Exit;
		FShadowTracker := THotTracker.Create;
		with FShadowTracker do
    begin
      Associate := Self.Handle;
      Border := 2;
      Clipped := True;
      Color := 0;
      Opacity := $60;
      X := 3;
      Y := 3;
	    Update;
    end;
	end
  else if FShadowTracker <> nil then
  	FreeAndNil(FShadowTracker);
end;

procedure TCustomFlowBar.SetAllowFocus(Value: Boolean);
begin
  DebugNote('SetAllowFocus');
	if Value <> FAllowFocus then
  begin
	  FAllowFocus := Value;
    AlignButtons;
  end;
end;

procedure TCustomFlowBar.SetButtons(Value: TFlowButtons);
begin
  DebugNote('SetButtons');
  FButtons.Assign(Value);
end;

procedure TCustomFlowBar.SetDragable(Value: Boolean);
begin
  DebugNote('SetDragable');
	if Value <> FDragable then
  begin
	  FDragable := Value;
		AlignButtons;
    Invalidate;
  end;
end;

function TCustomFlowBar.GetFlowSite: TFlowSite;
begin
	if Parent is TFlowSite then
  	Result := TFlowSite(Parent)
	else
  	Result := nil;
end;

procedure TCustomFlowBar.SetFlowSite(Value: TFlowSite);
var
  P: TPoint;
  F: TCustomForm;
  S: TFlowSite;
begin
  DebugNote('SetFlowSite');
  if Value <> FlowSite then
  begin
    if Parent is TFlowSite then
      S := Parent as TFlowSite
    else
      S := nil;
    if Value <> nil then
    begin
      Orientation := ConvertAlign(Value.Align);
    	Value.DockControl(Self, 0, 0);
			FreeAndNil(FDockTracker);
    end
    else if csDesigning in ComponentState then
    begin
    { if S <> nil then
    begin
      S.AlignFlowBars;
      S.Realign;
      InvalidateRect(S.Handle, nil, True);
    end }
    end
    else if S <> nil then
    begin
      P := ClientToScreen(Point(0, 0));
      S.UndockControl(Self);
      SetWindowPos(Handle, HWND_TOPMOST, P.X, P.Y,
      	0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
      if (Owner is TCustomForm) then
      begin
        F := Owner as TCustomForm;
      	SendMessage(F.Handle, WM_NCACTIVATE, 1, 0);
      end;
			FreeAndNil(FDockTracker);
    end
  end;
end;

procedure TCustomFlowBar.SetImages(Value: TCustomImageList);
begin
  DebugNote('SetImages');
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
      AlignButtons;
  	end;
  end;
end;

procedure TCustomFlowBar.SetHotLight(Value: Boolean);
begin
  if Value <> FHotLight then
  begin
    FHotLight := Value;
  	AlignButtons;
  end;
end;

procedure TCustomFlowBar.SetNewStyle(Value: Boolean);
begin
  DebugNote('SetNewStyle');
  if Value <> FNewStyle then
  begin
  	FNewStyle := Value;
    if HandleAllocated then
			InvalidateWindows(Handle);
  end;
end;

procedure TCustomFlowBar.SetWide(Value: Boolean);
begin
  if Value <> FWide then
  begin
    FWide := Value;
    if FOrientation = orVertical then
    begin
	    Realign;
    	AlignButtons;
    end;
  end;
end;

procedure TCustomFlowBar.SetPadding(Value: Integer);
begin
  DebugNote('SetPadding');
  if Value <> FPadding then
  begin
    FPadding := Value;
	  Realign;
  	AlignButtons;
  end;
end;

procedure TCustomFlowBar.SetShadow(Value: Boolean);
begin
  if Value <> FShadow then
  begin
    FShadow := Value;
  	FreeAndNil(FShadowTracker);
    Invalidate;
  end;
end;

procedure TCustomFlowBar.SetOrientation(Value: TOrientation);
begin
  DebugNote('SetOrientation');
	if Value <> FOrientation then
  begin
	  FOrientation := Value;
    DebugNote('1');
    Realign;
    DebugNote('2');
		AlignButtons;
    DebugNote('3');
    Invalidate;
    DebugNote('4');
  end;
end;

procedure TCustomFlowBar.SetParent(AParent: TWinControl);
var
	FlowSite: TFlowSite;
  OldParent: TWinControl;
begin
  DebugNote('SetParent');
  OldParent := Parent;
	inherited SetParent(AParent);
	FDockTarget := nil;
  DebugNote(IntToStr(Integer(AParent)));
  if AParent <> nil then
    DebugNote(AParent.Name + ' = ' + AParent.ClassName);
  if AParent is TFlowSite then
	begin
    FDocked := True;
  	FlowSite := TFlowSite(AParent);
  	if FlowSite.Align in [alLeft, alRight] then
    	Orientation := orVertical
		else
    	Orientation := orHorizontal;
		FlowSite.DockControl(Self, 0, 0);
  	FDockTarget := FlowSite;
  end
  else if OldParent is TFlowSite then
    (OldParent as TFlowSite).UndockControl(Self);
end;

procedure TCustomFlowBar.SetShowCaptions(Value: Boolean);
begin
  DebugNote('SetShowCaptions');
  if Value <> FShowCaptions then
  begin
  	FShowCaptions := Value;
		AlignButtons;
  end;
end;

function TCustomFlowBar.GetSize: Integer;
begin
  DebugNote('GetSize:');
	if FOrientation = orHorizontal then
  	Result := Height
	else
  	Result := Width;
end;

procedure TCustomFlowBar.CMEnabledChanged(var Msg: TMessage);
var
	I: Integer;
begin
	inherited;
	for I := 0 to ControlCount - 1 do
  	Controls[I].Enabled := Enabled and Buttons[I].Enabled;
end;

procedure TCustomFlowBar.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  FreeAndNil(FShadowTracker);
end;

procedure TCustomFlowBar.WMActivateApp(var Msg: TWMActivateApp);
var
  Wnd: HWND;
begin
  inherited;
  if not FDocked then
    if Msg.Active then
      SetWindowPos(Handle, HWND_TOPMOST, 0, 0,
      	0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE)
  	else
    begin
      Wnd := GetForegroundWindow;
      SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0,
      	0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
      SetWindowPos(Handle, Wnd, 0, 0,
      	0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
    end;
end;

procedure TCustomFlowBar.WMEnable(var Msg: TWMEnable);
begin
	inherited;
	ValidateHotTracker;
end;

procedure TCustomFlowBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
	R: TRect;
  F: TFlowSite;
begin
	R := ClientRect;
  Inc(R.Bottom, 5);
	with ThemePainter do
		if Enabled then
	  	DrawElement(Msg.DC, GetDetails(trRebarRoot), R)
		else
	    FillRectColor(Msg.DC, R, Color);
  Dec(R.Bottom, 5);
  if Parent is TFlowSite then
    F := Parent as TFlowSite
  else
    F := nil;
  if (F <> nil) and F.FBeveled then
  begin
    case F.Align of
      alLeft: R.Left := R.Right - 1;
      alTop: R.Top := R.Bottom - 1;
      alRight: R.Right := R.Left + 1;
      alBottom: R.Bottom := R.Top + 1;
    end;
    FillRectColor(Msg.DC, R, Blend(clBtnFace, clBtnShadow, 65));
    case F.Align of
      alLeft: Slide(R, drLeft);
      alTop: Slide(R, drUp);
      alRight: Slide(R, drRight);
      alBottom: Slide(R, drDown);
    end;
    FillRectColor(Msg.DC, R, Blend(clBtnFace, clBtnShadow, 85));
  end;
	Msg.Result := 1;
  if FUnaligned then
  begin
    FUnaligned := False;
    AlignButtons;
    //SetTimer(Handle, 1, 10, nil);
  end;
end;

procedure TCustomFlowBar.WMExitSizeMove(var Msg: TMessage);
begin
  DebugNote('WMExitSizeMove');
	inherited;
  if csDesigning in ComponentState then
  begin
    if FlowSite <> nil then
      FlowSite.AlignFlowBars;
  end
  else
  begin
  	if (not FDocked) and (FDockTarget <> nil) then
  		TFlowSite(FDockTarget).DockControl(Self, FDockPoint.X, FDockPoint.Y);
  	if not FDocked then
  	  FDockTarget := nil;
  	FreeAndNil(FDockTracker);
    GetParentForm(Self).SetFocus;
  end;
end;

procedure TCustomFlowBar.WMMouseActivate(var Msg: TWMMouseActivate);
begin
  DebugNote('WMMouseActivate');
	Msg.Result := MA_NOACTIVATE;
end;

procedure TCustomFlowBar.WMNCActivate(var Msg: TMessage);
var
  Form: TCustomForm;
begin
	Msg.Result := DefWindowProc(Handle, Msg.Msg, 1, Msg.LParam);
  if (not FDocked) and (Owner is TCustomForm)then
  begin
    Form := Owner as TCustomForm;
  	SendMessage(Form.Handle, WM_NCACTIVATE, 1, 0);
  end;
end;

procedure TCustomFlowBar.WMNCHitTest(var Msg: TWMNCHitTest);
var
	P: TPoint;
begin
  if csDesigning in ComponentState then
    inherited
  else
  begin
  	P := ScreenToClient(SmallPointToPoint(Msg.Pos));
    if not FDragable then
  	  Msg.Result := HTCLIENT
    else if (FOrientation = orHorizontal) and (P.X < ButtonGrip) then
  	 	Msg.Result := HTCAPTION
  	else if (FOrientation = orVertical) and (P.Y < ButtonGrip) then
  	 	Msg.Result := HTCAPTION
  	else
  	  Msg.Result := HTCLIENT;
  end;
end;

procedure TCustomFlowBar.WMNCLButtonDown(var Msg: TWMNCLButtonDown);
var
	P: TPoint;
begin
  if (not (csDesigning in ComponentState)) and FDocked then
  begin
  	P := ScreenToClient(Point(Msg.XCursor, Msg.YCursor));
    if Orientation = orHorizontal then
    	Top := -10000
		else
    	Left := -10000;
    if Parent is TFlowSite then
			TFlowSite(Parent).UndockControl(Self)
		else
	  	Unflow;
    SetWindowPos(Handle, HWND_TOPMOST, Msg.XCursor - P.X, Msg.YCursor - P.Y,
    	0, 0, SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
    ValidateHotTracker;
	end;
	inherited;
end;

procedure TCustomFlowBar.WMMove(var Msg: TWMMove);

  procedure AlignTracker(FlowSite: TFlowSite; Container: TWinControl; Force: Boolean = False);
  var
  	R: TRect;
    A, B: TPoint;
    X, Y: Integer;
  begin
  	R := ClientRect;
    A := FDockPoint;
    if (FlowSite.Align = alBottom) and (FlowSite.FDockControls.Count = 0) then
    	Dec(A.Y, Size)
		else if (FlowSite.Align = alRight) and (FlowSite.FDockControls.Count = 0) then
    	Dec(A.X, Size);
	  Force := Force or (ConvertAlign(FlowSite.Align) <> FDockAlign);
    FDockAlign := ConvertAlign(FlowSite.Align);
		if FDockAlign <> FOrientation then
		begin
     	X := WidthOf(R);
			Y := HeightOf(R);
			R.Right := R.Left + Y;
			R.Bottom := R.Top + X;
		end;
    B := FlowSite.ScreenToClient(FDockPoint);
    if FDockAlign = orHorizontal then
	    if B.X + WidthOf(R) > FlowSite.Width then
      	A.X := A.X + (FlowSite.Width - (B.X + WidthOf(R)));
    if FDockAlign = orVertical then
	    if B.Y + HeightOf(R) > FlowSite.Height then
      	A.Y := A.Y + (FlowSite.Height - (B.Y + HeightOf(R)));
    if Force then
			FDockTracker.Update(@R, @A)
    else
      FDockTracker.Move(@A);
  end;

var
	SiteContainer: TWinControl;
  FlowSite: TFlowSite;
	R: TRect;
	I: Integer;
begin
  DebugNote('WMMove');
	inherited;
  if FShadowTracker <> nil then
  	FShadowTracker.Move;
	if not FDocked then
  begin
		FDockTarget := nil;
  	SiteContainer := Owner as TWinControl;
  	R := BoundsRect;
    for I := 0 to SiteContainer.ComponentCount - 1 do
    	if SiteContainer.Components[I] is TFlowSite then
      begin
      	FlowSite := TFlowSite(SiteContainer.Components[I]);
        if FlowSite.ValidateDock(R, FDockPoint) then
        begin
					FDockTarget := FlowSite;
          if FDockTracker = nil then
          begin
          	FDockTracker := THotTracker.Create;
            with FDockTracker do
            begin
            	Associate := Self.Handle;
              Blur := 0;
            	Border := 0;
              Opacity := $30;
              Color := 0;
            	Radius := 0;
            end;
						AlignTracker(FlowSite, SiteContainer, True);
          end
          else
						AlignTracker(FlowSite, SiteContainer);
        	Break;
        end;
      end;
		if FDockTarget = nil then
			FreeAndNil(FDockTracker);
  end;
end;

procedure TCustomFlowBar.WMSize(var Msg: TWMSize);
begin
  DebugNote('WMSize');
	if FShadowTracker <> nil then
  	FShadowTracker.Update;
end;

end.
