unit DesignCtrls;

interface

uses
	Windows, Messages, Classes, SysUtils, Controls, Graphics, GraphTools,
	WinTools, Forms, Dialogs, DialogsEx, FormTools, SuplCtrls, BlendTools,
  XMLParser, XMLPersist;

const
	CN_STYLE = WM_USER + $200;
	CN_DESIGNING = CN_STYLE + 1;
  CN_LAYOUTCHANGED = CN_DESIGNING + 1;

  SNoBinding = '<none>';

type
	TDesignRegion = record
		X, Y, Width, Height: Integer;
	end;

	TDesignStyle = (dsDefault, dsUnderscore, dsBox);
  TDesignState = set of (dsDesigning, dsLoading);

  TDesignInsertEvent = procedure(Sender: TObject; X, Y: Integer; var Inserted: Boolean) of object;

	TDesignStylizer = class(TComponent, IUnknown, IEventListener)
	private
    FDesignState: TDesignState;
    FActiveBand: TWinControl;
		FBackground: TPicture;
    FBackgroundFileName: string;
    FBand: TPicture;
    FBandFileName: string;
    FBandColor: TColor;
    FBandFont: TFont;
    FBandAlpha: TAlphaMap;
    FBandFill: Boolean;
    FBandAlignment: TAlignment;
    FBandOffsetX: Integer;
    FBandOffsetY: Integer;
		FBuffer: TFastBitmap;
		FChanged: Boolean;
		FColor: TColor;
		FDesigning: Boolean;
		FDesigner: TWinControl;
		FGrid: TBitmap;
		FGridSize: Integer;
		FStyle: TDesignStyle;
		FGripSize: Integer;
		FCaptionFont: TFont;
		FControlFont: TFont;
		FRegion: TDesignRegion;
    FOnSelectControl: TNotifyEvent;
    FActiveControl: TWinControl;
    FLocked: Boolean;
    FCaptionPosition: TCaptionPosition;
    FUndoIndex: Integer;
    FUndoList: TUndoList;
    FOnQueryInsert: TDesignInsertEvent;
		procedure DestroyBackground;
		procedure UpdateMessage(Msg: Cardinal; WinControl: TWinControl);
		procedure UpdateDesigners;
		procedure UpdateStyles;
		procedure UpdateRegion(const Region: TDesignRegion);
		procedure FontsChanged(Sender: TObject);
		procedure BackgroundChanged(Sender: TObject);
		procedure BandChanged(Sender: TObject);
    procedure SetActiveBand(Value: TWinControl);
    procedure SetActiveControl(Value: TWinControl);
    procedure SetBackgroundFileName(const Value: string);
    procedure SetBandFileName(const Value: string);
    procedure SetBandColor(Value: TColor);
    procedure SetBandFont(Value: TFont);
    procedure SetBandFill(Value: Boolean);
    procedure SetBandAlignment(Value: TAlignment);
    procedure SetBandOffsetX(Value: Integer);
    procedure SetBandOffsetY(Value: Integer);
		procedure SetCaptionFont(Value: TFont);
    procedure SetCaptionPosition(Value: TCaptionPosition);
		procedure SetControlFont(Value: TFont);
		procedure SetDesigning(Value: Boolean);
    procedure SetDesignState(Value: TDesignState);
		procedure SetDesigner(Value: TWinControl);
		procedure SetColor(Value: TColor);
		procedure SetGridSize(Value: Integer);
		procedure SetGripSize(Value: Integer);
    function GetLocked: Boolean;
		procedure SetStyle(Value: TDesignStyle);
    procedure SetOnSelectControl(Value: TNotifyEvent);
    procedure SetOnQueryInsert(Value: TDesignInsertEvent);
	protected
  	function QueryInsert(Control: TWinControl; X, Y: Integer): Boolean;
    { IEventListener }
		procedure ChangeEvent(Sender: TObject; EventId: Integer = 0);
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
    property ActiveBand: TWinControl read FActiveBand write SetActiveBand;
  	property ActiveControl: TWinControl read FActiveControl write SetActiveControl;
    procedure ControlPaint(Control: TWinControl; DC: HDC);
		procedure LayoutCopy(Designer: TWinControl);
		procedure LayoutDesigner(Designer: TWinControl);
    procedure RestoreState;
    procedure Refresh;
		procedure Update;
    property UndoList: TUndoList read FUndoList;
	published
		property Designing: Boolean read FDesigning write SetDesigning;
		property Background: TPicture read FBackground;
    property BackgroundFileName: string read FBackgroundFileName write SetBackgroundFileName;
		property Band: TPicture read FBand;
    property BandFileName: string read FBandFileName write SetBandFileName;
    property BandColor: TColor read FBandColor write SetBandColor;
    property BandFont: TFont read FBandFont write SetBandFont;
    property BandFill: Boolean read FBandFill write SetBandFill;
    property BandAlignment: TAlignment read FBandAlignment write SetBandAlignment;
    property BandOffsetX: Integer read FBandOffsetX write SetBandOffsetX;
    property BandOffsetY: Integer read FBandOffsetY write SetBandOffsetY;
		property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionPosition: TCaptionPosition read FCaptionPosition write SetCaptionPosition;
		property ControlFont: TFont read FControlFont write SetControlFont;
		property Color: TColor read FColor write SetColor;
		property Designer: TWinControl read FDesigner write SetDesigner;
		property GridSize: Integer read FGridSize write SetGridSize;
		property GripSize: Integer read FGripSize write SetGripSize;
    property Locked: Boolean read GetLocked write FLocked;
		property Style: TDesignStyle read FStyle write SetStyle;
    property DesignState: TDesignState read FDesignState write SetDesignState;
    property OnSelectControl: TNotifyEvent read FOnSelectControl write SetOnSelectControl;
    property OnQueryInsert: TDesignInsertEvent read FOnQueryInsert write SetOnQueryInsert;
	end;

{ TDesignHost }

	TDesignControl = (dcEdit, dcMemo, dcCombo, dcList, dcImage, dcButton, dcMatrix,
  	dcCategory, dcBand, dcCustom);

	TDesignHost = class(TCustomControl)
	private
	  FDataBinding: string;
		FDragging: Boolean;
		FDragGroup: TDesignHost;
		FDragMove: TPoint;
		FDragSize: TPoint;
		FStylizer: TDesignStylizer;
		FOnEditProperties: TNotifyEvent;
    FTrackControl: TWinControl;
    FNeedsAlign: Boolean;
		procedure MoveGroup(X, Y: Integer);
		procedure SizeGroup(X, Y: Integer);
		function GetStylizer: TDesignStylizer;
    function GetSelected: Boolean;
    procedure SetTrackControl(Value: TWinControl);
		procedure CMControlChange(var Msg: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
		procedure CNDesigning(var Msg: TMessage); message CN_DESIGNING;
    procedure CNLayoutChanged(var Msg: TMessage); message CN_LAYOUTCHANGED;
		procedure CNStyle(var Msg: TMessage); message CN_STYLE;
		procedure WMClose(var Msg: TWMClose); message WM_CLOSE;
		procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
		procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
		procedure WMMove(var Msg: TWMMove); message WM_MOVE;
		procedure WMSize(var Msg: TWMSize); message WM_SIZE;
		procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
		procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
		procedure WMNCLButtonDblClk(var Msg: TWMNCLButtonDblClk); message WM_NCLBUTTONDBLCLK;
		procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
		procedure WMKillFocus(var Msg: TWMSetFocus); message WM_KILLFOCUS;
	protected
		procedure AdjustLayout; virtual;
		procedure RequestLayout;
		procedure ClearSelection(Parent: Boolean = False);
    procedure BlendBackground(DC: HDC; const Rect: TRect);
    procedure ClipBackground(DC: HDC); virtual;
		procedure DoEditProperties; dynamic;
    function FindImmediate(Control: TWinControl): TWinControl;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure Paint; override;
    procedure SafeFocus;
    class function GetControlKind: TDesignControl; virtual; abstract;
    class function GetFixedHeight: Boolean; virtual;
    class function GetFixedWidth: Boolean; virtual;
		procedure SetParent(AParent: TWinControl); override;
		property Stylizer: TDesignStylizer read GetStylizer;
    property TrackControl: TWinControl read FTrackControl write SetTrackControl;
    property NeedsAlign: Boolean read FneedsAlign write FNeedsAlign;
	public
		constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
		procedure EditProperties;
		procedure Remove;
		property Caption;
    property ControlKind: TDesignControl read GetControlKind;
    property DataBinding: string read FDataBinding write FDataBinding;
    property FixedHeight: Boolean read GetFixedHeight;
    property FixedWidth: Boolean read GetFixedWidth;
    property Selected: Boolean read GetSelected;
		property OnEditProperties: TNotifyEvent read FOnEditProperties write FOnEditProperties;
	end;

	TDesignHostClass = class of TDesignHost;

	IDesignSurface = interface
	  ['{903C199C-5F74-411D-AEDF-65E0E9E117F8}']
		function AddControl(Control: TDesignControl): TDesignHost;
	end;

{ TDesignSurface }

	TDesignSurface = class(TScrollingWinControl, IUnknown, IDesignSurface)
	private
		FAligning: Boolean;
    FHosts: TList;
		FMouseDown: Boolean;
		FMousePoint: TPoint;
		FDragMove: TPoint;
		FStylizer: TDesignStylizer;
		procedure DrawDragRect;
    function GetHostCount: Integer;
    function GetHost(Index: Integer): TDesignHost;
		function GetStylizer: TDesignStylizer;
		procedure CMControlChange(var Msg: TCMControlChange); message CM_CONTROLCHANGE;
		procedure CNDesigning(var Msg: TMessage); message CN_DESIGNING;
    procedure CNLayoutChanged(var Msg: TMessage); message CN_LAYOUTCHANGED;
		procedure CNStyle(var Msg: TMessage); message CN_STYLE;
		procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
		procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
		procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
		procedure WMSize(var Msg: TWMSize); message WM_SIZE;
	protected
		procedure AlignControls(AControl: TControl; var Rect: TRect); override;
		procedure ClearSelection;
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function AddControl(Control: TDesignControl): TDesignHost;
    property HostCount: Integer read GetHostCount;
    property Hosts[Index: Integer]: TDesignHost read GetHost;
		property Stylizer: TDesignStylizer read GetStylizer;
	end;

function DefaultStylizer: TDesignStylizer;
procedure RegisterDesignControlClass(ClassType: TDesignHostClass);
function FindDesignControlClass(Control: TDesignControl): TDesignHostClass;
function ElementToStr(Control: TDesignControl; LongNames: Boolean = False): string;
function StrToElement(S: string): TDesignControl;

var
	DefaultHostEditorProc: function(Host: TDesignHost): Boolean;

implementation

var
	InternalDefaultStylizer: TObject;
	DesignControls: array[TDesignControl] of TDesignHostClass;

const
	ElementNames: array[TDesignControl] of string = (
    	'edit', 'memo', 'combo', 'list', 'image', 'button', 'matrix', 'category', 'band',
      'custom');
const
	LongElementNames: array[TDesignControl] of string = (
    	'text edit', 'text memo', 'combo box', 'list box', 'image capture',
      'command button', 'matrix grid', 'category edit', 'band group', 'custom edit');

function ElementToStr(Control: TDesignControl; LongNames: Boolean = False): string;
begin
  if LongNames then
		Result := LongElementNames[Control]
  else
		Result := ElementNames[Control];
end;

function StrToElement(S: string): TDesignControl;
var
	I: TDesignControl;
begin
	Result := dcEdit;
	S := LowerCase(Trim(S));
	for I := Low(ElementNames) to High(ElementNames) do
  	if S = ElementNames[I] then
    begin
    	Result := I;
    	Break;
    end;
end;

procedure RegisterDesignControlClass(ClassType: TDesignHostClass);
begin
	DesignControls[ClassType.GetControlKind] := ClassType;
end;

function FindDesignControlClass(Control: TDesignControl): TDesignHostClass;
begin
	Result := DesignControls[Control];
end;

procedure AddDebug(const S: string);
begin
end;

function DefaultStylizer: TDesignStylizer;
begin
	if InternalDefaultStylizer = nil then
 		InternalDefaultStylizer := TDesignStylizer.Create(Application);
	Result := TDesignStylizer(InternalDefaultStylizer);
end;

{ TDesignStylizer }

function EmptyDocument: IDocument;
begin
	Result := CreateDocument;
  Result.Text := '<?xml version="1.0"?>'#13#10'<layout><surface/></layout>';
end;

constructor TDesignStylizer.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FBackground := TPicture.Create;
	FBackground.OnChange := BackgroundChanged;
	FBand := TPicture.Create;
	FBand.OnChange := BandChanged;
  FBandColor := clBtnShadow;
  FBandFont := TFont.Create;
	FBandFont.Assign(Screen.IconFont);
  FBandFont.Color := clWhite;
  FBandFont.Size := 10;
  FBandFont.Style := [fsBold];
  FBandFont.OnChange := FontsChanged;
  FBandAlpha := TAlphaMap.Create;
  with FBandAlpha do
  begin
  	Width := $10;
  	Height := $10;
	  Canvas.Brush.Color := $7F7F7F;
    Canvas.FillRect(Rect(0, 0, Width, Height));
		Opacity := $7F;
    CommitOpacity;
  end;
  FBandFill := True;
  FCaptionPosition := cpTop;
	FColor := clBtnFace;
	FGrid := TBitmap.Create;
	FGrid.Width := FGridSize;
	FGrid.Height := FGridSize;
	FGridSize := 8;
	FGrid.Width := FGridSize;
	FGrid.Height := FGridSize;
	FGrid.Canvas.Brush.Color := clWhite;
	FGrid.Canvas.FillRect(Rect(0, 0, FGridSize, FGridSize));
	FGrid.Canvas.Pixels[0, 0] := 0;
	FGripSize := 8;
	FCaptionFont := TFont.Create;
	FCaptionFont.Assign(Screen.IconFont);
	FCaptionFont.OnChange := FontsChanged;
	FControlFont := TFont.Create;
	FControlFont.Assign(Screen.IconFont);
	FControlFont.OnChange := FontsChanged;
	FUndoList := TUndoList.Create(EmptyDocument);
  FUndoList.ChangeEvents.Subscribe(Self);
end;

destructor TDesignStylizer.Destroy;
begin
	Designer := nil;
	FBackground.Free;
	DestroyBackground;
  FBandFont.Free;
  FBandAlpha.Free;
	FCaptionFont.Free;
	FControlFont.Free;
  FUndoList.Free;
	inherited Destroy;
end;

procedure TDesignStylizer.DestroyBackground;
begin
	if IsFastBitmap(FBuffer) then
		DestroyFastBitmap(FBuffer);
  FillChar(FRegion, SizeOf(FRegion), 30);
end;

function CompareLayout(Item1, Item2: Pointer): Integer;
var
	A: TDesignHost absolute Item1;
	B: TDesignHost absolute Item2;
begin
	if A.Top > B.Top then
		Result := 1
	else if A.Top < B.Top then
		Result := -1
	else if A.Left < B.Left then
		Result := -1
	else if A.Left > B.Left then
		Result := 1
	else if A = B then
		Result := 0
	else if A.TabOrder < B.TabOrder then
		Result := 1
	else if Integer(A) < Integer(B) then
		Result := 1
	else
		Result := -1;
end;

procedure TDesignStylizer.Refresh;
begin
	if (FDesigner <> nil) and	FDesigner.HandleAllocated then
  	InvalidateWindows(FDesigner.Handle);
end;

{procedure TDesignStylizer.SaveStyle(Document: IDocument);

  procedure WriteColor(Color: TColor; Node: INode);
  begin
  end;

	procedure WriteFont(Font: TFont; Node: INode);
  begin
  end;

begin

end;}

procedure TDesignStylizer.LayoutCopy(Designer: TWinControl);

	procedure EnumNodes(Parent: INodes; Control: TWinControl);
  var
  	Child: INode;
    Filer: IFiler;
    H: TDesignHost;
    I: Integer;
  begin
  	if Control is TDesignSurface then
    begin
    	Child := Parent.Add('surface');
		end
		else if Control is TDesignHost then
    begin
    	Child := Parent.Add('element');
    	Filer := Child.Attributes.Filer;
    	H := TDesignHost(Control);
      Filer.WriteString('kind', ElementToStr(H.ControlKind));
      Filer.WriteInteger('left', H.Left + FRegion.X);
      Filer.WriteInteger('top', H.Top + FRegion.Y);
      Filer.WriteInteger('width', H.Width);
      Filer.WriteInteger('height', H.Height);
      if Self.ActiveBand = H then
	      Filer.WriteBool('active', True);
			Filer := Child.Filer;
      Filer.WriteString('binding', H.DataBinding);
      Filer.WriteString('caption', H.Caption);
      Filer.WriteString('help', H.Hint);
    end
    else
    	Exit;
    Parent := Child.Nodes;
    for I := 0 to Control.ControlCount - 1 do
    	if Control.Controls[I] is TWinControl then
      	EnumNodes(Parent, TWinControl(Control.Controls[I]));
  end;

var
	Document: IDocument;
begin
	if Self = InternalDefaultStylizer then Exit;
  if (Designer = nil) or UndoList.Suspended or ([dsDesigning, dsLoading] *
  	FDesignState <> [dsDesigning, dsLoading]) then
  	Exit;
  Document := CreateDocument;
  Document.Root := Document.CreateNode('layout');
  EnumNodes(Document.Root.Nodes, Designer);
  if FUndoIndex = FUndoList.ItemIndex then
  begin
  	Inc(FUndoIndex);
	  FUndoList.Update(Document)
	end;
  Exclude(FDesignState, dsLoading);
end;

procedure TDesignStylizer.LayoutDesigner(Designer: TWinControl);
var
	List: TList;

  procedure Layout(Index: Integer);
  var
  	Control, Item: TDesignHost;
    A, B: TRect;
    I: Integer;
  begin
  	if Index = List.Count then
    	Exit;
		Control := TDesignHost(List[Index]);
    A := Control.BoundsRect;
    for I := Index + 1 to List.Count - 1 do
    begin
    	Item := TDesignHost(List[I]);
      B := Item.BoundsRect;
      InflateRect(A, -1, 0);
      if IntersectRect(B, A, B) then
      begin
				InflateRect(A, 1, 0);
      	if A.Top = B.Top then
					Item.Left := A.Right
        else
	      	Item.Top := A.Bottom;
			end
      else
				InflateRect(A, 1, 0);
    end;
    Layout(Index + 1);
  end;

var
	H: TDesignHost;
	I: Integer;
begin
	if Self = InternalDefaultStylizer then Exit;
	if FUndoIndex <> FUndoList.ItemIndex then Exit;
	Include(FDesignState, dsLoading);
	List := TList.Create;
	try
		for I := 0 to Designer.ControlCount - 1 do
			if Designer.Controls[I] is TDesignHost then
			begin
				H := TDesignHost(Designer.Controls[I]);
				if H.Parent = Designer then
					List.Add(H);
				if H.ControlKind = dcBand then
        	LayoutDesigner(H);
			end;
		List.Sort(CompareLayout);
		for I := 0 to List.Count - 1 do
			TDesignHost(List[I]).TabOrder := I;
		Layout(0);
	finally
		List.Free;
	end;
  if Designer.HandleAllocated then
  	PostMessage(FDesigner.Handle, CN_LAYOUTCHANGED, 0, 0);
end;

function TDesignStylizer.QueryInsert(Control: TWinControl; X, Y: Integer): Boolean;
begin
	Result := False;
	if Self = InternalDefaultStylizer then Exit;
	if Designing and Assigned(FOnQueryInsert) then
  	FOnQueryInsert(Control, X, Y, Result);
end;

procedure TDesignStylizer.RestoreState;
var
	Band: TDesignHost;
  Surfaces: TList;

	procedure BuildControls(Parent: TWinControl; Node: INode);
  var
  	Surface: IDesignSurface;
  	Nodes: INodes;
    Child: INode;
    Filer: IFiler;
    H: TDesignHost;
    I: Integer;
  begin
  	if not Supports(Parent, IDesignSurface, Surface) then Exit;
		Surfaces.Add(Parent);
  	Nodes := Node.FindNodes('element');
    for I := 0 to Nodes.Count - 1 do
    begin
			Child := Nodes[I];
      Filer := Child.Attributes.Filer;
      H := Surface.AddControl(StrToElement(Filer.ReadString('kind')));
      H.SetBounds(Filer.ReadInteger('left'), Filer.ReadInteger('top'),
      	Filer.ReadInteger('width'), Filer.ReadInteger('height'));
			if Filer.ReadBool('active', False, False) then
      	Band := H;
			Filer := Child.Filer;
      H.DataBinding := Filer.ReadString('binding');
			H.Caption := Filer.ReadString('caption');
      H.Hint := Filer.ReadString('help');
			BuildControls(H, Child);
		end;
  end;

var
	Control: TWinControl;
	I, J: Integer;
begin
	if FDesigner = nil then Exit;
  FDesigner.HandleNeeded;
  if not FDesigner.HandleAllocated then Exit;
  ActiveControl := nil;
  ActiveBand := nil;
  Band := nil;
  Surfaces := TList.Create;
  LockWindowUpdate(FDesigner.Handle);
  UndoList.Suspended := True;
  try
		FUndoIndex := -1;
  	Include(FDesignState, dsLoading);
	  for I := FDesigner.ControlCount - 1 downto 0 do
  		FDesigner.Controls[I].Free;
		BuildControls(FDesigner, FUndoList.State.Root.FindNode('/layout/surface'));
    Update;
  	FUndoIndex := FUndoList.ItemIndex;
    { begin kludge }
		LayoutDesigner(FDesigner);
    for I := 0 to 2 do
    begin
			Exclude(FDesignState, dsLoading);
	    for J := Surfaces.Count - 1 downto 0 do
	    begin
	    	Control := TWinControl(Surfaces[J]);
	      if Control is TDesignHost then
	      	TDesignHost(Control).NeedsAlign := True;
				Control.Realign;
			end;
			LayoutDesigner(FDesigner);
    end;
		Exclude(FDesignState, dsLoading);
    { end kludge }
	finally
	  UndoList.Suspended := False;
	  LockWindowUpdate(0);
    Surfaces.Free;
	end;
  ActiveBand := Band;
end;

procedure TDesignStylizer.ChangeEvent(Sender: TObject; EventId: Integer = 0);
var
	List: TUndoList;
begin
	if Self = InternalDefaultStylizer then Exit;
  if Sender is TUndoList then
  begin
  	List := TUndoList(Sender);
		if List.ItemIndex <> FUndoIndex then
  	RestoreState;
	  FUndoIndex := List.ItemIndex;
  end;
end;

procedure TDesignStylizer.UpdateMessage(Msg: Cardinal; WinControl: TWinControl);
var
	MsgCopy: TMessage;
	I: Integer;
begin
	if Self = InternalDefaultStylizer then Exit;
	MsgCopy.Msg := Msg;
  MsgCopy.WParam := 0;
  MsgCopy.LParam := 0;
	WinControl.Dispatch(MsgCopy);
	for I := 0 to WinControl.ControlCount - 1 do
		if WinControl.Controls[I] is TDesignHost then
			UpdateMessage(Msg, TWinControl(WinControl.Controls[I]));
end;

procedure TDesignStylizer.UpdateDesigners;
begin
	if Self = InternalDefaultStylizer then Exit;
	if FDesigner <> nil then
		UpdateMessage(CN_DESIGNING, FDesigner);
end;

procedure TDesignStylizer.UpdateStyles;
begin
	if Self = InternalDefaultStylizer then Exit;
	if FDesigner = nil then Exit;
	UpdateMessage(CN_STYLE, FDesigner);
  if FDesigner.HandleAllocated then
  	InvalidateWindows(FDesigner.Handle);
end;

procedure TDesignStylizer.UpdateRegion(const Region: TDesignRegion);
var
	X, Y: Integer;
	Brush: HBRUSH;
	Canvas: TCanvas;
	Fill, Image: TFastBitmap;
begin
	if Self = InternalDefaultStylizer then Exit;
	if (not CompareMem(@FRegion, @Region, SizeOf(Region))) or (not IsFastBitmap(FBuffer)) then
	begin
		FRegion := Region;
		X := FRegion.X + FRegion.Width;
		Y := FRegion.Y + FRegion.Height;
		if X < 1 then X := 1;
		if Y < 1 then Y := 1;
		if (FBuffer.Width = X) and (FBuffer.Height = Y) then Exit;
		DestroyFastBitmap(FBuffer);
		FBuffer := CreateFastBitmap(X, Y);
    if FDesigning then
    begin
			Brush := GetBrush(FGrid);
			FillRect(FBuffer.DC, Rect(0, 0, X, Y), Brush);
			DeleteObject(Brush);
		end
    else
			FillRectColor(FBuffer.DC, Rect(0, 0, X, Y), clWhite);
		if FBackground.Graphic <> nil then
    begin
    	if FDesigning then
				Image := CreateFastBitmap(FBackground.Graphic.Width, FBackground.Graphic.Height)
			else
				Image := FBuffer;
    	Canvas := TCanvas.Create;
     	try
	     	Canvas.Handle := Image.DC;
    	 	Canvas.Draw(0, 0, FBackground.Graphic);
				if FDesigning then
					BitBlt(FBuffer.DC, 0, 0, FBuffer.Width, FBuffer.Height, Image.DC, 0, 0, SRCAND);
	    finally
				Canvas.Free;
				if FDesigning then
	        DestroyFastBitmap(Image);
     	end;
     	with FBackground.Graphic do
    	 	SelectClipRect(FBuffer.DC, Rect(0, 0, Width, Height), RGN_DIFF);
  	end;
  	Fill := CreateFastBitmap(10, 10);
    try
    	FillRectColor(Fill.DC, Rect(0, 0, 10, 10), FColor);
    	StretchBlt(FBuffer.DC, 0, 0, FBuffer.Width, FBuffer.Height, Fill.DC, 0, 0, Fill.Width,
    	Fill.Height, SRCAND);
  	finally
    	DestroyFastBitmap(Fill);
		end;
	end;
end;

procedure TDesignStylizer.Update;
begin
	if Self = InternalDefaultStylizer then Exit;
	if FChanged then
		UpdateStyles;
	FChanged := False;
end;

procedure TDesignStylizer.ControlPaint(Control: TWinControl; DC: HDC);
var
  P: TPoint;
begin
	if (Self = InternalDefaultStylizer) or (FDesigner = nil) then Exit;
  P := FDesigner.ScreenToClient((Control.ClientToScreen(Point(0, 0))));
	Inc(P.X, FRegion.X);
	Inc(P.Y, FRegion.Y);
	BitBlt(DC, 0, 0, Control.Width, Control.Height, FBuffer.DC, P.X, P.Y, SRCCOPY);
end;

procedure TDesignStylizer.BackgroundChanged(Sender: TObject);
var
	Canvas: TCanvas;
  Bits: TFastBitmap;
begin
	if Self = InternalDefaultStylizer then Exit;
	DestroyBackground;
  FChanged := True;
  if FBackground.Graphic = nil then Exit;
  if not FBackground.Graphic.Empty then
  begin
  	Canvas := TCanvas.Create;
	  Bits := CreateFastBitmap(FBackground.Graphic.Width, FBackground.Graphic.Height);
    try
    	Canvas.Handle := Bits.DC;
			Canvas.Draw(0, 0, FBackground.Graphic);
      Color := Canvas.Pixels[Bits.Width - 1, Bits.Height - 1];
    finally
    	DestroyFastBitmap(Bits);
    	Canvas.Free;
    end;
  end;
end;

procedure TDesignStylizer.BandChanged(Sender: TObject);
begin
	if Self = InternalDefaultStylizer then Exit;
	FChanged := True;
end;

procedure TDesignStylizer.FontsChanged(Sender: TObject);
begin
	if Self = InternalDefaultStylizer then Exit;
	FChanged := True;
end;

procedure TDesignStylizer.SetActiveBand(Value: TWinControl);

	procedure InvalidateControls(Child: TWinControl);
  var
  	I: Integer;
  begin
  	if Child = nil then Exit;
    Child.Invalidate;
    for I := 0 to Child.ControlCount - 1 do
    	if Child.Controls[I] is TDesignHost then
      	InvalidateControls(TDesignHost(Child.Controls[I]));
  end;

var
	Prior: TWinControl;
begin
	if Self = InternalDefaultStylizer then Exit;
  if not FBandFill then
  	Value := nil;
  if Value <> FActiveBand then
  begin
  	Prior := FActiveBand;
    FActiveBand := Value;
    if not Designing then
    begin
			InvalidateControls(Prior);
			InvalidateControls(FActiveBand);
    end;
  end;
end;

procedure TDesignStylizer.SetActiveControl(Value: TWinControl);

	function FindBand(Parent: TWinControl): TWinControl;
  begin
  	Result := nil;
  	if Parent = nil then Exit;
    if Parent is TDesignSurface then Exit;
		if (Parent is TDesignHost) and (TDesignHost(Parent).ControlKind = dcBand) then
			Result := Parent
		else
			Result := FindBand(Parent.Parent);
  end;

var
	Prior: TWinControl;
begin
	if Self = InternalDefaultStylizer then Exit;
  if Value <> FActiveControl then
  begin
  	Prior := FActiveControl;
    FActiveControl := Value;
	  if FBandFill then
	    ActiveBand := FindBand(FActiveControl)
		else
	    ActiveBand := nil;
    if Prior <> nil then
    	Prior.Invalidate;
		if FActiveControl <> nil then
    	FActiveControl.Invalidate;
		if Assigned(FOnSelectControl) then FOnSelectControl(Self);
  end;
end;

procedure ValidatePicture(Picture: TPicture; var OriginalFile: string; const NewFile: string);
var
	S: string;
begin
	S := Trim(NewFile);
  if FileExists(S) then
  try
  	Picture.LoadFromFile(S);
  except
  	MessageDlg('The specified picture is in an unsupported format.', mtError, [mbOk], 0);
    S := OriginalFile;
  end
  else
  begin
  	S := '';
		Picture.Graphic := nil;
	end;
	OriginalFile := S;
end;

procedure TDesignStylizer.SetBackgroundFileName(const Value: string);
begin
	if Self = InternalDefaultStylizer then Exit;
	ValidatePicture(FBackground, FBackgroundFileName, Value);
	FChanged := True;
end;

procedure TDesignStylizer.SetBandFileName(const Value: string);
begin
	if Self = InternalDefaultStylizer then Exit;
	ValidatePicture(FBand, FBandFileName, Value);
	FChanged := True;
end;

procedure TDesignStylizer.SetBandColor(Value: TColor);
begin
	if Self = InternalDefaultStylizer then Exit;
  if Value <> FBandColor then
  begin
		FBandColor := Value;
	  with FBandAlpha do
	  begin
		  Canvas.Brush.Color := FBandColor;
	    Canvas.FillRect(Rect(0, 0, Width, Height));
	    CommitOpacity;
	  end;
		FChanged := True;
  end;
end;

procedure TDesignStylizer.SetBandFont(Value: TFont);
begin
	if Self = InternalDefaultStylizer then Exit;
	FBandFont.Assign(Value);
	FChanged := True;
end;

procedure TDesignStylizer.SetBandFill(Value: Boolean);
begin
	if Self = InternalDefaultStylizer then Exit;
  FBandFill := Value;
end;

procedure TDesignStylizer.SetBandAlignment(Value: TAlignment);
begin
	if Self = InternalDefaultStylizer then Exit;
  if Value <> FBandAlignment then
  begin
  	FBandAlignment := Value;
		FChanged := True;
	end;
end;

procedure TDesignStylizer.SetBandOffsetX(Value: Integer);
begin
	if Self = InternalDefaultStylizer then Exit;
  if Value <> FBandOffsetX then
  begin
  	FBandOffsetX := Value;
		FChanged := True;
	end;
end;

procedure TDesignStylizer.SetBandOffsetY(Value: Integer);
begin
	if Self = InternalDefaultStylizer then Exit;
  if Value <> FBandOffsetY then
  begin
  	FBandOffsetY := Value;
		FChanged := True;
	end;
end;

procedure TDesignStylizer.SetDesigning(Value: Boolean);
begin
	if Self = InternalDefaultStylizer then Exit;
	if Value <> FDesigning then
	begin
		FDesigning := Value;
		DestroyBackground;
    if FDesigning then
	    Include(FDesignState, dsDesigning)
		else
	    Include(FDesignState, dsDesigning);
		UpdateDesigners;
	end;
end;

procedure TDesignStylizer.SetDesignState(Value: TDesignState);
begin
	if Value <> FDesignState then
  begin
		FDesignState := Value;
    Designing := dsDesigning in FDesignState;
  end;
end;

procedure TDesignStylizer.SetDesigner(Value: TWinControl);

	procedure UpdateDesigner(WinControl: TWinControl; Stylizer: TDesignStylizer);
	var
		I: Integer;
	begin
		if WinControl is TDesignHost then
			TDesignHost(WinControl).FStylizer := Stylizer
		else if WinControl is TDesignSurface then
			TDesignSurface(WinControl).FStylizer := Stylizer;
		for I := 0 to WinControl.ControlCount	- 1 do
			if WinControl.Controls[I] is TWinControl then
				UpdateDesigner(TWinControl(WinControl.Controls[I]), Stylizer);
	end;

begin
	if Self = InternalDefaultStylizer then Exit;
	if Value <> FDesigner then
	begin
		if FDesigner <> nil then
			UpdateDesigner(FDesigner, nil);
		FDesigner := Value;
		if FDesigner <> nil then
			UpdateDesigner(FDesigner, Self);
	end;
end;

procedure TDesignStylizer.SetColor(Value: TColor);
begin
	if Self = InternalDefaultStylizer then Exit;
	if Value <> FColor then
	begin
		DestroyBackground;
		FColor := Value;
    FChanged := True;
	end;
end;

procedure TDesignStylizer.SetGridSize(Value: Integer);
begin
	if Self = InternalDefaultStylizer then Exit;
	if Value < 4 then
		Value := 4
	else if Value > 50 then
		Value := 50;
	if Value <> FGridSize then
	begin
		FGridSize := Value;
		FGrid.Width := FGridSize;
		FGrid.Height := FGridSize;
		FGrid.Canvas.Brush.Color := clWhite;
		FGrid.Canvas.FillRect(Rect(0, 0, FGridSize, FGridSize));
		FGrid.Canvas.Pixels[0, 0] := 0;
		FChanged := True;
	end;
end;

procedure TDesignStylizer.SetGripSize(Value: Integer);
begin
	if Self = InternalDefaultStylizer then Exit;
	if Value < 2 then
		Value := 2
	else if Value > 16 then
		Value := 16;
	if Value <> FGripSize then
	begin
		FGripSize := Value;
		FChanged := True;
	end;
end;

function TDesignStylizer.GetLocked: Boolean;
begin
	Result := FLocked or (not FDesigning);
end;

procedure TDesignStylizer.SetStyle(Value: TDesignStyle);
begin
	if Self = InternalDefaultStylizer then Exit;
	if Value <> FStyle then
	begin
		FStyle := Value;
		FChanged := True;
	end;
end;

procedure TDesignStylizer.SetCaptionFont(Value: TFont);
begin
	if Self = InternalDefaultStylizer then Exit;
	FCaptionFont.Assign(Value);
end;

procedure TDesignStylizer.SetCaptionPosition(Value: TCaptionPosition);
begin
	if Self = InternalDefaultStylizer then Exit;
	if Value <> FCaptionPosition then
  begin
	  FCaptionPosition := Value;
    FChanged := True;
	end;
end;

procedure TDesignStylizer.SetControlFont(Value: TFont);
begin
	if Self = InternalDefaultStylizer then Exit;
	FControlFont.Assign(Value);
end;

procedure TDesignStylizer.SetOnQueryInsert(Value: TDesignInsertEvent);
begin
	if Self = InternalDefaultStylizer then Exit;
  FOnQueryInsert := Value;
end;

procedure TDesignStylizer.SetOnSelectControl(Value: TNotifyEvent);
begin
	if Self = InternalDefaultStylizer then Exit;
  FOnSelectControl := Value;
end;

{ TDesignHost }

const
	DefBandHeight = 32;

constructor TDesignHost.Create(AOwner: TComponent);
const
	MinSize = 20;
begin
	inherited Create(AOwner);
  ParentShowHint := True;
  ShowHint := True;
	Color := clBtnFace;
	ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
		csDoubleClicks, csReplicatable];
	if Stylizer.Designer is TDesignSurface then
		TDesignSurface(Stylizer.Designer).FHosts.Add(Self);
	TabStop := Stylizer.Designing;
end;

destructor TDesignHost.Destroy;
begin
	if Stylizer.Designer is TDesignSurface then
		TDesignSurface(Stylizer.Designer).FHosts.Remove(Self);
	if Stylizer.ActiveControl = Self then
		Stylizer.ActiveControl := nil;
	inherited Destroy;
end;

procedure TDesignHost.AdjustLayout;
begin
	with Constraints do
	begin
		MinWidth := 0;
		MaxWidth := 0;
		MinHeight := 0;
		MaxHeight := 0;
	end;
end;

procedure TDesignHost.ClearSelection(Parent: Boolean = False);
var
	WinControl: TWinControl;
	H: TDesignHost;
	I: Integer;
begin
	WinControl := Self;
	if Parent then
		WinControl := WinControl.Parent;
	if WinControl = nil then Exit;
	for I := 0 to WinControl.ControlCount - 1 do
		if WinControl.Controls[I] is TDesignHost then
		begin
			H := TDesignHost(WinControl.Controls[I]);
			if H.FDragGroup <> nil then
			begin
				H.FDragGroup := nil;
				H.Invalidate;
			end;
		end;
end;

procedure TDesignHost.ClipBackground(DC: HDC);
begin
end;

procedure TDesignHost.BlendBackground(DC: HDC; const Rect: TRect);
var
  BlendFunc: TBlendFunction;
  B: TBitmap;
begin
  if Stylizer.Designing then Exit;
  BlendFunc.BlendOp := AC_SRC_OVER;
  BlendFunc.BlendFlags := 0;
  BlendFunc.SourceConstantAlpha := $7F;
  BlendFunc.AlphaFormat := AC_SRC_OVER;
  B := Stylizer.FBandAlpha;
  Windows.AlphaBlend(DC, Rect.Left, Rect.Top, WidthOf(Rect), HeightOf(Rect),
  	B.Canvas.Handle, 0, 0, B.Width, B.Height, BlendFunc);
end;

procedure TDesignHost.MoveGroup(X, Y: Integer);
var
	H: TDesignHost;
	R: TRect;
begin
	if not Focused then Exit;
	H := FDragGroup;
	if H = nil then Exit;
	while H <> Self do
	begin
		R := H.BoundsRect;
		OffsetRect(R, X, Y);
		H.BoundsRect := R;
		H := H.FDragGroup
	end;
end;

procedure TDesignHost.SizeGroup(X, Y: Integer);
var
	H: TDesignHost;
	R: TRect;
begin
	if not Focused then Exit;
	H := FDragGroup;
	if H = nil then Exit;
	while H <> Self do
	begin
		R := H.BoundsRect;
		Inc(R.Right, X);
		Inc(R.Bottom, Y);
		H.BoundsRect := R;
		H := H.FDragGroup
	end;
end;

procedure TDesignHost.Remove;
var
	H: TDesignHost;
begin
	H := FDragGroup;
	if H <> nil then
		while H <> Self do
		begin
			PostMessage(H.Handle, WM_CLOSE, 0, 0);
			H := H.FDragGroup;
		end;
	PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TDesignHost.RequestLayout;
var
	R: TRect;
	W, H: Integer;
	I: Integer;
begin
	I := Stylizer.GridSize;
	if I < 1 then Exit;
	R := BoundsRect;
	OffsetRect(R, Stylizer.FRegion.X, Stylizer.FRegion.Y);
	W := WidthOf(R);
	H := HeightOf(R);
	R.Left := Round(R.Left / I) * I;
	R.Top := Round(R.Top / I) * I;
	R.Right := Round((R.Left + W) / I) * I + 1;
	R.Bottom := Round((R.Top + H) / I) * I + 1;
	OffsetRect(R, -Stylizer.FRegion.X, -Stylizer.FRegion.Y);
	BoundsRect := R;
	if Focused then
	begin
		RequestAlign;
		if not FDragging then
			Stylizer.LayoutDesigner(Parent);
	end;
end;

procedure TDesignHost.Paint;
const
	DashedColors: array[Boolean] of TColor = (cl3DDkShadow, clInactiveCaption);
var
	State: TDrawState;
begin
	if Stylizer.Designing then
	begin
  	State := [];
  	if Focused then
    	State := [dsFocused]
		else if Stylizer.ActiveControl = Self then
    	State := [dsHot];
		if (State <> []) or (FDragGroup <> nil) then
			DrawThemeThinButton(Canvas.Handle, ClientRect, [dsHot]);
		if State <> [] then
			DrawThemeDesigner(Canvas.Handle, ClientRect, Stylizer.GripSize,
      	FixedWidth, FixedHeight,  State)
		else if FDragGroup = nil then
			DrawDashedRect(Canvas.Handle, ClientRect, DashedColors[ThemePainter.Enabled], Color);
	end;
end;

procedure TDesignHost.SafeFocus;
begin
	// if Windows.GetFocus <> Handle then SetFocus;
  SetFocus;
end;

procedure TDesignHost.KeyDown(var Key: Word; Shift: TShiftState);
var
	I: Integer;
begin
	if not Stylizer.Locked then
	begin
		I := Stylizer.GridSize;
		if Key = VK_SPACE then
			EditProperties
		else if Key = VK_DELETE then
			Remove
		else if (Key = VK_ESCAPE) and (Parent <> nil) then
			Parent.SetFocus
		else if ssCtrl in Shift then
		case Key of
			VK_LEFT: Left := Left - I;
			VK_RIGHT: Left := Left + I;
			VK_UP: Top := Top - I;
			VK_DOWN: Top := Top + I;
		end
		else if ssShift in Shift then
		case Key of
			VK_LEFT: Width := Width - I;
			VK_RIGHT: Width := Width + I;
			VK_UP:	Height := Height - I;
			VK_DOWN: Height := Height + I;
		end;
		RequestLayout;
		{ TODO: use arrow keys without shift to change focus
		keybd_event(VK_TAB, 0, 0, 0);
		keybd_event(VK_TAB, 0, KEYEVENTF_KEYUP, 0);}
	end;
	inherited KeyDown(Key, Shift);
end;

procedure TDesignHost.EditProperties;
var
	WasLoading: Boolean;
	H: TDesignHost;
begin
	if Stylizer.Designing then
	begin
		DoEditProperties;
	 	H := FDragGroup;
		if H = nil then Exit;
    Stylizer.UndoList.Suspended := True;
		try
			while H <> Self do
			begin
				H.DoEditProperties;
				H := H.FDragGroup
			end;
		finally
	    Stylizer.UndoList.Suspended := False;
    end;
		WasLoading := dsLoading in Stylizer.FDesignState;
    Include(Stylizer.FDesignState, dsLoading);
    try
	    Stylizer.LayoutCopy(Stylizer.FDesigner);
		finally
    	if not WasLoading then
		    Exclude(Stylizer.FDesignState, dsLoading);
		end;      
	end;
end;

procedure TDesignHost.DoEditProperties;
begin
	if Assigned(DefaultHostEditorProc) then
		DefaultHostEditorProc(Self);
	if Assigned(FOnEditProperties) then
		FOnEditProperties(Self);
end;

function TDesignHost.FindImmediate(Control: TWinControl): TWinControl;
begin
	Result := nil;
	if Stylizer.Designing then Exit;
	while Control <> nil do
  	if Control.Parent = Self then
    	if Control is TDesignHost then
				Break
			else
      begin
      	Result := Control;
        Break;
			end
		else
			Control := Control.Parent;
end;

procedure TDesignHost.SetParent(AParent: TWinControl);
begin
	inherited SetParent(AParent);
	if AParent <> nil then
	begin
		HandleNeeded;
		AdjustLayout;
	end;
end;

class function TDesignHost.GetFixedHeight: Boolean;
begin
	Result := False;
end;

class function TDesignHost.GetFixedWidth: Boolean;
begin
	Result := False;
end;

function TDesignHost.GetSelected: Boolean;
begin
	Result := (FDragGroup <> nil) or (Stylizer.ActiveControl = Self);
end;

function TDesignHost.GetStylizer: TDesignStylizer;
begin
	if FStylizer = nil then
		if (Owner is TDesignSurface) and (TDesignSurface(Owner).FStylizer <> nil) then
			Result := TDesignSurface(Owner).FStylizer
		else if (Owner is TDesignHost) and (TDesignHost(Owner).FStylizer <> nil) then
			Result := TDesignHost(Owner).FStylizer
		else
			Result := DefaultStylizer
	else
		Result := FStylizer;
end;

procedure TDesignHost.SetTrackControl(Value: TWinControl);
begin
  if Stylizer.Designing then
  	Value := nil;
	if Value <> FTrackControl then
  begin
	  FTrackControl := Value;
    if (TrackControl <> nil) and (TrackControl.Parent = Self) then
	    Stylizer.ActiveControl := Self;
		if Stylizer.Style > dsDefault then
	  	Invalidate;
  end;
end;

type
	TControlHack = class(TControl);

procedure TDesignHost.CMControlChange(var Msg: TCMControlChange);
begin
	if Msg.Control is TDesignHost then
		if Msg.Inserting then
			TDesignHost(Msg.Control).FStylizer := FStylizer
		else
			TDesignHost(Msg.Control).FStylizer := nil;
	if Msg.Inserting then
	begin
		TControlHack(Msg.Control).ParentFont := False;
		if Msg.Control is TWinControl then
			TControlHack(Msg.Control).Font := Stylizer.ControlFont
		else
			TControlHack(Msg.Control).Font := Stylizer.CaptionFont;
	end;
  inherited;
end;

procedure TDesignHost.CMFocusChanged(var Msg: TCMFocusChanged);
begin
	TrackControl := FindImmediate(Msg.Sender);
	inherited;
end;

procedure TDesignHost.CNDesigning(var Msg: TMessage);
var
	I: Integer;
begin
	for I := 0 to ControlCount - 1 do
	begin
		if not (Controls[I] is TDesignHost) then
			Controls[I].Enabled := not Stylizer.Designing;
	end;
	TabStop := Stylizer.Designing;
  if HandleAllocated then
  	InvalidateWindows(Handle);
	//Invalidate;
end;

procedure TDesignHost.CNLayoutChanged(var Msg: TMessage);
begin
	if (Parent <> nil) and Parent.HandleAllocated then
  	SendMessage(Parent.Handle, CN_LAYOUTCHANGED, 0, 0);
end;

procedure TDesignHost.CNStyle(var Msg: TMessage);
var
	I: Integer;
begin
	for I := 0 to ControlCount - 1 do
	begin
		if not (Controls[I] is TDesignHost) then
			if Controls[I] is TWinControl then
				TControlHack(Controls[I]).Font := Stylizer.ControlFont
			else
				TControlHack(Controls[I]).Font := Stylizer.CaptionFont;
	end;
	AdjustLayout;
  inherited;
end;

procedure TDesignHost.WMClose(var Msg: TWMClose);
begin
	inherited;
	if not (csDestroying in ComponentState) then
		Free;
end;

procedure TDesignHost.WMEraseBkgnd(var Msg: TWMEraseBkgnd);

	function FindBand(Parent: TWinControl): Boolean;
  begin
	  Result := False;
  	if Parent = nil then Exit;
  	if Parent is TDesignSurface then Exit;
    if Parent = Stylizer.ActiveBand then
    	Result := True
		else
    	Result := FindBand(Parent.Parent);
  end;

var
	Rect: TRect;
begin
	ClipBackground(Msg.DC);
	if Stylizer.Designing then
  	FillRectColor(Msg.DC, ClientRect, Color)
	else
  begin
		Stylizer.ControlPaint(Self, Msg.DC);
    if Stylizer.FBandFill and FindBand(Parent) then
	    BlendBackground(Msg.DC, ClientRect);
	end;
	if FTrackControl <> nil then
  	if Stylizer.Style = dsUnderscore then
    begin
  		Rect := FTrackControl.BoundsRect;
    	InflateRect(Rect, Stylizer.GridSize - 2, Stylizer.GridSize - 2);
      Rect.Top := Rect.Bottom - Stylizer.GridSize;
      if ThemePainter.Enabled then
	      ThemePainter.DrawElement(Msg.DC, ThemePainter.GetDetails(ttbFlashButton), Rect)
			else
		    DrawThemeThinButton(Msg.DC, Rect, [dsFocused]);
    end
  	else if Stylizer.Style = dsBox then
		begin
  		Rect := FTrackControl.BoundsRect;
    	InflateRect(Rect, Stylizer.GridSize - 1, Stylizer.GridSize - 1);
      if ThemePainter.Enabled then
	      ThemePainter.DrawElement(Msg.DC, ThemePainter.GetDetails(ttbFlashButton), Rect)
			else
		    DrawThemeButton(Msg.DC, Rect, [dsFocused]);
  	end;
  Msg.Result := 1;
end;

procedure TDesignHost.WMExitSizeMove(var Msg: TMessage);
begin
	if Stylizer.Designing then
	begin
		FDragging := False;
		RequestLayout;
	end;
	inherited;
end;

procedure TDesignHost.WMGetDlgCode(var Msg: TMessage);
begin
	Msg.Result := DLGC_WANTARROWS;
end;

procedure TDesignHost.WMMove(var Msg: TWMMove);
begin
	inherited;
	if Stylizer.UndoList.Suspended then Exit;
	if Stylizer.Designing and Focused then
	begin
		if Msg.XPos < 0 then
		begin
			Msg.XPos := 0;
			FDragMove.X := 0;
		end;
		if Msg.YPos < 0 then
		begin
			Msg.YPos := 0;
			FDragMove.Y := 0;
		end;
		MoveGroup(Msg.XPos - FDragMove.X, Msg.YPos - FDragMove.Y);
		FDragMove := SmallPointToPoint(Msg.Pos);
	end;
	RequestAlign;
end;

procedure TDesignHost.WMSize(var Msg: TWMSize);
begin
	inherited;
	if Stylizer.Designing and Focused then
	begin
		SizeGroup(Width - FDragSize.X, Height - FDragSize.Y);
		FDragSize := Point(Width, Height);
	end;
end;

procedure TDesignHost.WMNCHitTest(var Msg: TWMNCHitTest);
var
	Rect, A, B: TRect;
	P: TPoint;
	I, X, Y: Integer;
begin
	if not Stylizer.Locked then
	begin
		if ssShift in KeyboardStateToShiftState then
		begin
			Msg.Result := HTBORDER;
			Exit;
		end;
		GetWindowRect(Handle, Rect);
		P := SmallPointToPoint(Msg.Pos);
		P.X := P.X - Rect.Left;
		P.Y := P.Y - Rect.Top;
		OffsetRect(Rect, -Rect.Left, -Rect.Top);
		I := Stylizer.GripSize;
		for X := 0 to 2 do
		begin
			A := Rect;
			case X of
				0:
					begin
						A.Left := (WidthOf(Rect) - I) shr 1;
						A.Right := A.Left + I;
					end;
				1: A.Right := I;
				2: A.Left := A.Right - I;
			end;
			for Y := 0 to 2 do
			begin
				if (X = 0) and (Y = 0) then Continue;
				B := A;
				case Y of
					0:
						begin
							B.Top := (HeightOf(Rect) - I) shr 1;
							B.Bottom := B.Top + I;
						end;
					1: B.Bottom := I;
					2: B.Top := A.Bottom - I;
				end;
				if PtInRect(B, P) then
				begin
					Msg.Result := 9 + Y * 3 + X;
					Exit;
				end;
			end;
		end;
		Msg.Result := HTCAPTION;
		if FDragging then
			FDragging := False;
	end
	else
		Msg.Result := HTCLIENT;
end;   

procedure TDesignHost.WMNCLButtonDown(var Msg: TWMNCLButtonDown);

	function FindPriorDragGroup: TDesignHost;
	begin
		Result := FDragGroup;
		while Result.FDragGroup <> Self do
			Result := Result.FDragGroup;
	end;

	function FindDragGroup: TDesignHost;
	var
		I: Integer;
	begin
		Result := nil;
		if FDragGroup <> nil then
			Result := FDragGroup
		else if Parent <> nil then
			for I := 0 to Parent.ControlCount - 1 do
				if (Parent.Controls[I] is TDesignHost) and
					(TDesignHost(Parent.Controls[I]).FDragGroup <> nil) then
				begin
					Result := TDesignHost(Parent.Controls[I]).FDragGroup;
					Break;
				end;
	end;

var
	H: TDesignHost;
  P: TPoint;
begin
	if Stylizer.Designing then
	begin
    P := ScreenToClient(Point(Msg.XCursor, Msg.YCursor));
  	if Stylizer.QueryInsert(Self, P.X, P.Y) then
    begin
    	ReleaseCapture;
      Msg.Result := HTCLIENT;
      Exit;
    end;
		if ssShift in KeyboardStateToShiftState then
		begin
			FDragging := False;
			if FDragGroup <> nil then
			begin
				H := FindPriorDragGroup;
				if H = FDragGroup	then
					H.FDragGroup := nil
				else
					H.FDragGroup := FDragGroup;
					H.SafeFocus;
				H.Invalidate;
				FDragGroup := nil;
			end
			else
			begin
				H := FindDragGroup;
				if H = nil then
				begin
					if FindControl(GetFocus) is TDesignHost then
					begin
						H := TDesignHost(FindControl(GetFocus));
						if H = Self then
							// do nothing
						else if H.Parent = Parent then
						begin
							H.FDragGroup := Self;
							FDragGroup := H;
							SafeFocus;
						end
						else
						begin
							H.ClearSelection(True);
							SafeFocus;
						end;
					end
					else
						SafeFocus;
				end
				else
				begin
					FDragGroup := H.FDragGroup;
					H.FDragGroup := Self;
				end;
			end;
			Invalidate;
		end
		else
		begin
			if FDragGroup = nil then
				ClearSelection(True);
			SafeFocus;
			FDragging := True;
		end;
	end;
	inherited;
end;

procedure TDesignHost.WMNCLButtonDblClk(var Msg: TWMNCLButtonDblClk);
begin
	if Stylizer.Designing then
		EditProperties;
	Msg.Result := 0;
end;

procedure TDesignHost.WMSetFocus(var Msg: TWMSetFocus);
begin
	FDragMove := Point(Left, Top);
	FDragSize := Point(Width, Height);
	ClearSelection;
  if FDragGroup = nil then
  	if Parent is TDesignHost then
	    TDesignHost(Parent).ClearSelection
    else if Parent is TDesignSurface then
	    TDesignSurface(Parent).ClearSelection;
	Stylizer.ActiveControl := Self;
	inherited;
end;

procedure TDesignHost.WMKillFocus(var Msg: TWMSetFocus);
begin
	Invalidate;
	inherited;
end;

{ TDesignSurface }

constructor TDesignSurface.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  ParentShowHint := False;
  ShowHint := True;
	Color := clBtnFace;
	ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
		csDoubleClicks, csReplicatable];
	VertScrollBar.Tracking := True;
	HorzScrollBar.Tracking := True;
  FHosts := TList.Create;
end;

destructor TDesignSurface.Destroy;
begin
	Stylizer.FDesigner := nil;
  FHosts.Free;
	inherited Destroy;
end;

function TDesignSurface.AddControl(Control: TDesignControl): TDesignHost;
begin
	Result := DesignControls[Control].Create(Self);
	Result.Parent := Self;
  Result.Caption := Result.ClassName;
end;

procedure TDesignSurface.AlignControls(AControl: TControl; var Rect: TRect);
var
	Changed: Boolean;
begin
  if Stylizer.UndoList.Suspended then Exit;
	inherited AlignControls(AControl, Rect);
	if (not FAligning) and (AControl <> nil) then
	begin
		FAligning := True;
		Changed := False;
		Rect := AControl.BoundsRect;
		if Rect.Top + VertScrollBar.Position < 0 then
		begin
			Rect.Top := - VertScrollBar.Position;
			Changed := True;
		end;
		if Rect.Left + HorzScrollBar.Position < 0 then
		begin
			Rect.Left := -HorzScrollBar.Position;
			Changed := True;
		end;
		if AControl is TWinControl and Changed then
		begin
			SetWindowPos(TWinControl(AControl).Handle, 0, Rect.Left, Rect.Top,
				0, 0, SWP_NOSIZE or	SWP_NOZORDER or SWP_NOSENDCHANGING );
		end;
		FAligning := False;
	end;
end;

procedure TDesignSurface.ClearSelection;
var
	H: TDesignHost;
	I: Integer;
begin
	for I := 0 to ControlCount - 1 do
		if Controls[I] is TDesignHost then
		begin
			H := TDesignHost(Controls[I]);
			if H.FDragGroup <> nil then
			begin
				H.FDragGroup := nil;
				H.Invalidate;
			end;
		end;
end;

procedure TDesignSurface.MouseDown(Button: TMouseButton;
	Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);
	if Stylizer.Designing and (Button = mbLeft) then
	begin
  	if Stylizer.QueryInsert(Self, X, Y) then
    begin
    	ReleaseCapture;
      Exit;
    end;
		SetFocus;
    Stylizer.ActiveControl := nil;
		FMouseDown := True;
		FMousePoint := Point(X, Y);
		FDragMove := FMousePoint;
	end;
end;

procedure TDesignSurface.DrawDragRect;
var
	DC: HDC;
	P: HPEN;
	B: HBRUSH;
	R: Cardinal;
begin
	DC := GetWindowDC(Handle);
	P := SelectObject(DC, GetPen(0, psDot));
	B := SelectObject(DC, GetStockObject(NULL_BRUSH));
	R := SetROP2(DC, R2_XORPEN);
	Rectangle(DC, FMousePoint.X, FMousePoint.Y, FDragMove.X, FDragMove.Y);
	SetROP2(DC, R);
	SelectObject(DC, B);
	OverwriteObject(DC, P);
	ReleaseDC(Handle, DC);
end;

procedure TDesignSurface.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
	inherited MouseMove(Shift, X, Y);
	if Stylizer.Designing and FMouseDown then
	begin
		DrawDragRect;
		FDragMove := Point(X, Y);
		DrawDragRect;
	end;
end;

procedure TDesignSurface.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);

	function ControlIntersect(Control: TControl): Boolean;
	var
		A, B: TRect;
	begin
		if Control.Parent <> Self then
			Result := False
		else if Control is TDesignHost then
	 	begin
			if FMousePoint.X > FDragMove.X then
			begin
				B.Left := FDragMove.X;
				B.Right := FMousePoint.X;
			end
			else
			begin
				B.Left := FMousePoint.X;
				B.Right := FDragMove.X;
			end;
			if FMousePoint.Y > FDragMove.Y then
			begin
				B.Top := FDragMove.Y;
				B.Bottom := FMousePoint.Y;
			end
			else
			begin
				B.Top := FMousePoint.Y;
				B.Bottom := FDragMove.Y;
			end;
			Result := IntersectRect(A, B, Control.BoundsRect);
		end
		else
			Result := False;
	end;

var
	First, Current: TDesignHost;
	I: Integer;
begin
	inherited MouseUp(Button, Shift, X, Y);
	if Stylizer.Designing and (Button = mbLeft) and FMouseDown then
	begin
		DrawDragRect;
		FMouseDown := False;
		First := nil;
		Current := nil;
		for I := 0 to ControlCount - 1 do
			if ControlIntersect(Controls[I]) then
			begin
				if First = nil then
					First := TDesignHost(Controls[I]);
				if Current = nil then
					Current := First
				else
				begin
					Current.FDragGroup := TDesignHost(Controls[I]);
					Current := Current.FDragGroup;
				end;
			end;
		if Current <> nil then
		begin
			Current.FDragGroup := First;
			Current := First;
			if Current.FDragGroup = First then
			begin
				Current.FDragGroup := nil;
				Current.SafeFocus;
			end
			else
			repeat
				Current.Invalidate;
				Current := Current.FDragGroup;
			until Current.FDragGroup = First;
      First.Invalidate;
			Current.SafeFocus;
		end;
	end;
end;

function TDesignSurface.GetHostCount: Integer;
begin
	Result := FHosts.Count;
end;

function TDesignSurface.GetHost(Index: Integer): TDesignHost;
begin
	Result := TDesignHost(FHosts[Index]);
end;

function TDesignSurface.GetStylizer: TDesignStylizer;
begin
	if FStylizer = nil then
		Result := Defaultstylizer
	else
		Result := FStylizer;
end;

procedure TDesignSurface.CMControlChange(var Msg: TCMControlChange);
begin
	if Msg.Control is TDesignHost then
		if Msg.Inserting then
			TDesignHost(Msg.Control).FStylizer := FStylizer
		else
			TDesignHost(Msg.Control).FStylizer := nil;
	inherited;
end;

procedure TDesignSurface.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
	Region: TDesignRegion;
begin
	Region.X := HorzScrollBar.Position;
	Region.Y := VertScrollBar.Position;
	Region.Width := Width;
	Region.Height := Height;
	Stylizer.UpdateRegion(Region);
	Stylizer.ControlPaint(Self, Msg.DC);
  Msg.Result := 1;
end;

procedure TDesignSurface.CNDesigning(var Msg: TMessage);
var
	DC: HDC;
begin
	if HandleAllocated then
  begin
  	DC := GetWindowDC(Handle);
  	SendMessage(Handle, WM_ERASEBKGND, DC, 0);
    ReleaseDC(Handle, DC);
	end;
end;

procedure TDesignSurface.CNLayoutChanged(var Msg: TMessage);
begin
	if Stylizer <> nil then
		Stylizer.LayoutCopy(Self);
end;

procedure TDesignSurface.CNStyle(var Msg: TMessage);
begin
	Invalidate;
end;

procedure TDesignSurface.WMHScroll(var Msg: TWMHScroll);
begin
	inherited;
	if Stylizer.Designing then
		Invalidate;
end;

procedure TDesignSurface.WMVScroll(var Msg: TWMVScroll);
begin
	inherited;
	if Stylizer.Designing then
		Invalidate;
end;

procedure TDesignSurface.WMSetFocus(var Msg: TWMSetFocus);
begin
	inherited;
	ClearSelection;
end;

procedure TDesignSurface.WMSize(var Msg: TWMSize);
begin
	inherited;
	if Stylizer.Designing then
		Invalidate;
end;

type
	TDesignSurfaceKeyHook = class(TObject)
	private
		FLastKey: TDateTime;
	public
		constructor Create;
		destructor Destroy; override;
		procedure KeyHook(Key: Word; State: Cardinal; var Remove: Boolean);
	end;

{ TDesignSurfaceKeyHook }

constructor TDesignSurfaceKeyHook.Create;
begin
	inherited Create;
	HookKeyboard(KeyHook);
end;

destructor TDesignSurfaceKeyHook.Destroy;
begin
	UnhookKeyboard(KeyHook);
	inherited Destroy;
end;

procedure TDesignSurfaceKeyHook.KeyHook(Key: Word; State: Cardinal;
	var Remove: Boolean);
const
	Interval = 1 / 24 / 60 / 60 / 10;
var
	Wnd: HWND;
begin
	case Key of
		VK_NEXT, VK_PRIOR, VK_HOME, VK_END:
		begin
			if Now - FLastKey < Interval then Exit;
			FLastKey := Now;
			if ((Key = VK_HOME) or (Key = VK_END)) and (not (ssCtrl in
				KeyboardStateToShiftState)) then
				Exit;
			Wnd := GetFocus;
			Wnd := GetParent(Wnd);
			repeat
				if FindControl(Wnd) is TDesignSurface then
				begin
					case Key of
						VK_NEXT: SendMessage(Wnd, WM_VSCROLL, SB_PAGEDOWN, 0);
						VK_PRIOR: SendMessage(Wnd, WM_VSCROLL, SB_PAGEUP, 0);
						VK_HOME: SendMessage(Wnd, WM_VSCROLL, SB_TOP, 0);
						VK_END: SendMessage(Wnd, WM_VSCROLL, SB_BOTTOM, 0);
					end;
					Break;
				end;
				Wnd := GetParent(Wnd);
			until (Wnd = 0) or (Wnd = GetDesktopWindow);
		end;
	end;
end;

var
	DesignSurfaceKeyHook: TDesignSurfaceKeyHook;

initialization
	DesignSurfaceKeyHook := TDesignSurfaceKeyHook.Create;
finalization
	DesignSurfaceKeyHook.Free
end.
