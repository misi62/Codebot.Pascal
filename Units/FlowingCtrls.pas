
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2007                   *)
(*                                                      *)
(********************************************************)

unit FlowingCtrls;

interface

{$I CODEBOT.INC}

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, ImgList, Forms,
  ActnList, BlendTools, GraphTools, FormTools, WinTools, BtnCtrls, XMLParser;

{ TFlowSite }

type
	TFlowSite = class(TWinControl, IUnknown, IIgnoreResize, IIgnoreMargin)
  private
  	FDockControls: TList;
    FSize: Integer;
    function CalculateDockPoint(X, Y: Integer): TPoint;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetSize(Value: Integer);
  protected
    procedure AlignFlowBars; virtual;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function ValidateDock(const Rect: TRect; var Point: TPoint): Boolean;
	public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustAlignment;
  	procedure DockControl(Control: TControl; X, Y: Integer);
    procedure UndockControl(Control: TControl);
	published
  	property Align;
    property Size: Integer read FSize write SetSize;
	end;

{ TFlowButton }

  TFlowButton = class(TCollectionItem)
  private
  	FAction: TBasicAction;
    FActionLink: TActionLink;
    FEnabled: Boolean;
    FHint: string;
    FImageIndex: Integer;
    FVisible: Boolean;
    FMenuArrow: Boolean;
    FCaption: TCaption;
    FSeparator: Boolean;
    FName: string;
    procedure SetAction(Value: TBasicAction);
    function GetButton: Integer;
    procedure SetEnabled(Value: Boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(Value: Integer);
    procedure SetCaption(const Value: TCaption);
    procedure SetMenuArrow(Value: Boolean);
    procedure SetSeparator(Value: Boolean);
    procedure SetVisible(Value: Boolean);
  public
		constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
		procedure ActionChange(Sender: TObject);
	published
  	property Action: TBasicAction read FAction write SetAction;
  	property Button: Integer read GetButton;
  	property Caption: TCaption read FCaption write SetCaption;
  	property Enabled: Boolean read FEnabled write SetEnabled;
    property Hint: string read FHint write SetHint;
  	property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property MenuArrow: Boolean read FMenuArrow write SetMenuArrow;
    property Name: string read FName write FName;
    property Separator: Boolean read FSeparator write SetSeparator;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TFlowButtonActionLink }

  TFlowButtonActionLink = class(TActionLink)
  private
    FClient: TFlowButton;
	protected
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;
    procedure SetCaption(const Value: string); override;
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

  TButtonClickEvent = procedure(Sender: TObject; Button: Integer) of object;
  TOrientation = (orHorizontal, orVertical);

	TCustomFlowBar = class(TCustomForm)
  private
  	FOwner: TComponent;
    FShadowTracker: THotTracker;
    FDockTracker: THotTracker;
    FDockAlign: TOrientation;
    FAllowFocus: Boolean;
    FButtons: TFlowButtons;
    FDockTarget: TObject;
    FDockPoint: TPoint;
    FDragable: Boolean;
    FHook: IDesignerHook;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FNewStyle: Boolean;
    FOrientation: TOrientation;
    FShowCaptions: Boolean;
    FOnButtonClick: TButtonClickEvent;
		procedure AlignButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ButtonDraw(Control: TWinControl; Rect: TRect;
	    DrawState: TDrawState; var DefaultDraw: Boolean);
    procedure SetDragable(Value: Boolean);
    function GetFlowSite: TFlowSite;
    procedure SetFlowSite(Value: TFlowSite);
    procedure SetOrientation(Value: TOrientation);
    procedure ImageListChange(Sender: TObject);
    procedure ValidateHotTracker;
    procedure SetAllowFocus(Value: Boolean);
    procedure SetButtons(Value: TFlowButtons);
    procedure SetImages(Value: TCustomImageList);
    procedure SetShowCaptions(const Value: Boolean);
    procedure SetNewStyle(const Value: Boolean);
		procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure WMEnable(var Msg: TWMEnable); message WM_ENABLE;
		procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
		procedure WMNCActivate(var Msg: TMessage); message WM_NCACTIVATE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    function GetSize: Integer;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure DoButtonClick(Button: Integer); dynamic;
    procedure Loaded; override;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaintWindow(DC: HDC); override;
		procedure SetParent(AParent: TWinControl); override;
    property AllowFocus: Boolean read FAllowFocus write SetAllowFocus default False;
    property Dragable: Boolean read FDragable write SetDragable default True;
    property Buttons: TFlowButtons read FButtons write SetButtons;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property FlowSite: TFlowSite read GetFlowSite write SetFlowSite default nil;
  	property Images: TCustomImageList read FImages write SetImages;
    property NewStyle: Boolean read FNewStyle write SetNewStyle default False;
    property Orientation: TOrientation read FOrientation write SetOrientation default orHorizontal;
    property Size: Integer read GetSize;
  	property OnButtonClick: TButtonClickEvent read FOnButtonClick write FOnButtonClick;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
	published
  	property PixelsPerInch;
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
    property Orientation;
  	property Images;
    property NewStyle;
  	property Visible;
  	property OnButtonClick;
  end;

implementation

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
  FDockControls.Free;
  inherited Destroy;
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

procedure TFlowSite.AlignControls(AControl: TControl; var Rect: TRect);
var
	CalcSize, NewSize: Integer;
	O: TOrientation;
  F: TCustomFlowBar;
	I: Integer;
begin
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
		CalcSize := 4;
	for I := 0 to FDockControls.Count - 1 do
	begin
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
  case Align of
  	alLeft: Left := High(Word);
  	alTop: Top := -High(Word);
  	alRight: Left := High(Word);
  	alBottom: Top := -High(Word);
	end;
end;

procedure TFlowSite.DockControl(Control: TControl; X, Y: Integer);
var
	OldSite: TFlowSite;
  P: TPoint;
begin
	if not ((Align in FlowAligns) and (FDockControls.IndexOf(Control) < 0)) then Exit;
	DisableAlign;
	OldSite := nil;
	if Control.Parent is TFlowSite then
    OldSite := TFlowSite(Control.Parent);
  FDockControls.Add(Control);
  if OldSite <> Self then
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
	if (OldSite <> nil) and (OldSite <> Self) then
		OldSite.FDockControls.Remove(Control);
end;

procedure TFlowSite.UndockControl(Control: TControl);
begin
	if FDockControls.IndexOf(Control) = -1 then Exit;
	FDockControls.Remove(Control);
  Control.Parent := nil;
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
	R: TRect;
  P: HPEN;
  B: HBRUSH;
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
		if Align in [alLeft, alRight] then
  		Inc(R.Bottom, 5);
		if FDockControls.Count = 0 then
    	FillRectColor(Msg.DC, R, Color)
    else with ThemePainter do
			if Enabled then
		  	DrawElement(Msg.DC, GetDetails(trRebarRoot), R)
			else
		  begin
		    FillRectColor(Msg.DC, R, Color);
		    R.Top := R.Bottom - 1;
		    FillRectColor(Msg.DC, R, Blend(Color, clWindowText, 75));
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
  inherited Create(Collection);
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
  if Value = nil then
  begin
    FActionLink.Free;
    FActionLink := nil;
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

procedure TFlowButton.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Changed(False);
  end;
end;

procedure TFlowButton.SetHint(const Value: string);
begin
  if Value <> FHint then
  begin
    FHint := Value;
    Changed(False);
  end;
end;

procedure TFlowButton.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TFlowButton.SetMenuArrow(Value: Boolean);
begin
  if Value <> FMenuArrow then
  begin
    FMenuArrow := Value;
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

procedure TFlowButtonActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TFlowButton;
end;

function TFlowButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
		(FClient.Caption = (Action as TCustomAction).Caption);
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
  inherited Update(Item);
  if GetOwner is TCustomFlowBar then
  	TCustomFlowBar(GetOwner).AlignButtons;
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
    if Item.MenuArrow then
	    Filer.WriteString('kind', 'menu')
		else
	    Filer.WriteString('kind', 'normal');
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
      Item.MenuArrow := Filer.ReadString('kind') = 'menu';
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
  FOwner := AOwner;
  if FOwner = nil then
  	FOwner := Application;
  if FOwner <> Application then
  	FOwner.FreeNotification(Self);
  if FOwner is TCustomForm then
  	FHook := TCustomForm(AOwner).Designer
	else
  	FHook := nil;
	if FHook = nil then
	  inherited CreateNew(FOwner)
	else
  begin
  	inherited CreateNew(FOwner);
		if not Supports(FHook, IFlowDesigner) then
	  	TCustomForm(FOwner).Designer := TFlowDesigner.Create(FOwner, FHook);
	end;
  BorderStyle := bsNone;
	ControlStyle := [];
  TabStop := False;
  if csDesigning in FOwner.ComponentState then
  begin
  	Width := 400;
	  Height := ButtonDefaultSize + ButtonBorder * 2;
  	Visible := True;
	end;
	FButtons := TFlowButtons.Create(Self);
  FDragable := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  DesktopFont := True;
end;

destructor TCustomFlowBar.Destroy;
begin
	FImages := nil;
	FreeAndNil(FDockTracker);
	FreeAndNil(FShadowTracker);
	inherited Destroy;
end;

procedure TCustomFlowBar.AlignButtons;
var
  Button: TImageButton;
	Item: TFlowButton;
  X: Integer;
	I: Integer;
begin
	for I := ControlCount - 1 downto FButtons.Count do
  	Controls[I].Free;
	for I := ControlCount to FButtons.Count - 1 do
  begin
		Button := TImageButton.Create(Self);
    Button.FocusedRect := False;
    Button.Parent := Self;
  end;
  if FDragable then
		X := ButtonGrip
	else
  	X := 2;
	for I := 0 to FButtons.Count - 1 do
  begin
  	Button := Controls[I] as TImageButton;
		Button.HandleNeeded;
    Item := FButtons[I];
    Button.AutoSize := True;
    Button.AllowFocus := FAllowFocus;
    Button.Caption := Item.Caption;
    if FShowCaptions and (Button.Caption <> '') and (FOrientation = orHorizontal) then
	    Button.CaptionPosition := cpRight
		else
	    Button.CaptionPosition := cpHidden;
		if Item.MenuArrow and (FOrientation = orHorizontal) then
	    Button.Kind := bkMenuButton
		else
	    Button.Kind := bkButton;
    Button.Hint := Item.Hint;
    Button.ImageIndex := Item.ImageIndex;
    Button.ParentShowHint := True;
		Button.Enabled := Item.Enabled and Enabled;
    Button.Visible := Item.Visible;
    Button.TabStop := True;
    Button.Style := bsFlat;
		Button.Tag := Item.Button;
    //Button.Images := nil;
    Button.Images := FImages;
    if Item.Action <> nil then
	    Button.OnClick := Item.Action.OnExecute
    else
	    Button.OnClick := ButtonClick;
    Button.OnCustomDraw := ButtonDraw;
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
			if Item.Separator then
				Inc(X, ButtonSeparator);
    end;
  end;
  if FOrientation = orHorizontal then
		Width := X + 2
  else
		Height := X + 2;
  if (csDesigning in ComponentState) and (Parent <> nil) then
  	Parent.Invalidate;
end;

procedure TCustomFlowBar.ButtonClick(Sender: TObject);
begin
	DoButtonClick((Sender as TComponent).Tag);
end;

procedure TCustomFlowBar.ButtonDraw(Control: TWinControl; Rect: TRect;
  DrawState: TDrawState; var DefaultDraw: Boolean);
var
	DC: HDC;
begin
	DefaultDraw := not ((dsBackground in DrawState) and FNewStyle);
  if not DefaultDraw then
  begin
  	DC := TImageButton(Control).Canvas.Handle;
  	if dsHot in DrawState then
    	DrawStyleRect(DC, Rect);
  end;
end;

procedure TCustomFlowBar.ImageListChange(Sender: TObject);
begin
	Realign;
	AlignButtons;
end;

procedure TCustomFlowBar.AlignControls(AControl: TControl; var Rect: TRect);
var
	I: Integer;
begin
	if Align in [alLeft, alRight] then Exit;
	if FImages = nil then
  	I := ButtonDefaultSize
	else
  	I := FImages.Height;
	if FOrientation = orHorizontal then
		if Height <> I + ButtonBorder * 2 then Height := I + ButtonBorder * 2
    else
  else
		if Width <> I + ButtonBorder * 2 then Width := I + ButtonBorder * 2;
end;

procedure TCustomFlowBar.DoButtonClick(Button: Integer);
begin
	if Assigned(FOnButtonClick) then
  	FOnButtonClick(Self, Button);
end;

procedure TCustomFlowBar.Loaded;
begin
	inherited Loaded;
  AlignButtons;
end;

procedure TCustomFlowBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
		if AComponent = FImages then
      FImages := nil
		else if AComponent = FOwner then
    begin
    	RemoveFreeNotification(FOwner);
    	FOwner := nil;
      Free;
    end;
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
		if Parent = nil then
			FillRectOutline(DC, ClientRect, clBtnShadow);
		ValidateHotTracker;
  end;
end;

procedure TCustomFlowBar.ValidateHotTracker;
begin
	if IsWindowEnabled(Handle) and (Parent = nil) then
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
var
	I: Integer;
begin
	if Value <> FAllowFocus then
  begin
	  FAllowFocus := Value;
		for I := 0 to ControlCount - 1 do
    	TImageButton(Controls[I]).AllowFocus := FAllowFocus;
  end;
end;

procedure TCustomFlowBar.SetButtons(Value: TFlowButtons);
begin
  FButtons.Assign(Value);
end;

procedure TCustomFlowBar.SetDragable(Value: Boolean);
begin
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
begin
	if Value <> nil then
  	Value.DockControl(Self, 0, 0)
	else
  	Parent := GetParentForm(Self);
end;

procedure TCustomFlowBar.SetImages(Value: TCustomImageList);
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
      AlignButtons;
  	end;
  end;
end;

procedure TCustomFlowBar.SetNewStyle(const Value: Boolean);
begin
  if Value <> FNewStyle then
  begin
  	FNewStyle := Value;
    if HandleAllocated then
			InvalidateWindows(Handle);
  end;
end;

procedure TCustomFlowBar.SetOrientation(Value: TOrientation);
begin
	if Value <> FOrientation then
  begin
	  FOrientation := Value;
    Realign;
		AlignButtons;
    Invalidate;
  end;
end;

procedure TCustomFlowBar.SetParent(AParent: TWinControl);
var
	FlowSite: TFlowSite;
  OldParent: TWinControl;
begin
	OldParent := Parent;
	inherited SetParent(AParent);
	if (csDesigning in ComponentState) and (OldParent <> nil) then
		OldParent.Invalidate;
	FDockTarget := nil;
  if Parent is TFlowSite then
	begin
  	FlowSite := TFlowSite(Parent);
  	if FlowSite.Align in [alLeft, alRight] then
    	Orientation := orVertical
		else
    	Orientation := orHorizontal;
		FlowSite.DockControl(Self, 0, 0);
  	FDockTarget := FlowSite;
  end;
	if (csDesigning in ComponentState) and (Parent <> nil) then
		Parent.Invalidate;
end;

procedure TCustomFlowBar.SetShowCaptions(const Value: Boolean);
begin
  if Value <> FShowCaptions then
  begin
  	FShowCaptions := Value;
		AlignButtons;
  end;
end;

function TCustomFlowBar.GetSize: Integer;
begin
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
  if (csDesigning in ComponentState) and (Parent <> nil) then
  	Parent.Invalidate;
end;

procedure TCustomFlowBar.WMEnable(var Msg: TWMEnable);
begin
	inherited;
	ValidateHotTracker;
end;

procedure TCustomFlowBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
	R: TRect;
begin
	R := ClientRect;
  if FOrientation = orVertical then
  	Inc(R.Bottom, 5);
	with ThemePainter do
		if Enabled then
	  	DrawElement(Msg.DC, GetDetails(trRebarRoot), R)
		else
	  begin
	    FillRectColor(Msg.DC, R, Color);
	    R.Top := R.Bottom - 1;
	    FillRectColor(Msg.DC, R, Blend(Color, clWindowText, 75));
	  end;
	Msg.Result := 1;
end;

procedure TCustomFlowBar.WMExitSizeMove(var Msg: TMessage);
begin
	inherited;
	if (Parent = nil) and (FDockTarget <> nil) then
		TFlowSite(FDockTarget).DockControl(Self, FDockPoint.X, FDockPoint.Y);
	if Parent = nil then
	  FDockTarget := nil;
	FreeAndNil(FDockTracker);
end;

procedure TCustomFlowBar.WMMouseActivate(var Msg: TWMMouseActivate);
begin
	Msg.Result := MA_NOACTIVATE;
end;

procedure TCustomFlowBar.WMNCActivate(var Msg: TMessage);
begin
	Msg.Result := DefWindowProc(Handle, Msg.Msg, 1, Msg.LParam);
  if FOwner is TWinControl then
  	SendMessage(TWinControl(FOwner).Handle, WM_NCACTIVATE, 1, 0);
end;

procedure TCustomFlowBar.WMNCHitTest(var Msg: TWMNCHitTest);
var
	P: TPoint;
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

procedure TCustomFlowBar.WMNCLButtonDown(var Msg: TWMNCLButtonDown);
var
	P: TPoint;
begin
  if Parent <> nil then
  begin
  	P := ScreenToClient(Point(Msg.XCursor, Msg.YCursor));
    if Orientation = orHorizontal then
    	Top := -10000
		else
    	Left := -10000;
    if Parent is TFlowSite then
			TFlowSite(Parent).UndockControl(Self)
		else
	  	Parent := nil;
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
	inherited;
  if FShadowTracker <> nil then
  	FShadowTracker.Move;
	if (Parent = nil) and (FOwner is TWinControl) then
  begin
		FDockTarget := nil;
  	SiteContainer := TWinControl(FOwner);
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
  if (csDesigning in ComponentState) and (Parent <> nil) then
  	Parent.Invalidate;
end;

procedure TCustomFlowBar.WMSize(var Msg: TWMSize);
begin
	if FShadowTracker <> nil then
  	FShadowTracker.Update;
  if (csDesigning in ComponentState) and (Parent <> nil) then
  	Parent.Invalidate;
end;

end.
