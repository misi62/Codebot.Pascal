unit BannerCtrls;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Graphics, ImgList, Forms,
  GraphTools, ProviderTools, FormTools, SuplCtrls, BaseTypes;

{ TBannerBook }

type
  TBannerBook = class(TFramedWindow)
  private
    FPageList: TList;
    FPages: TStrings;
    FPageIndex: Integer;
    FOnPageChanged: TNotifyEvent;
    procedure SetPages(Value: TStrings);
    procedure SetActivePage(const Value: string);
    function GetActivePage: string;
    function GetPage: TObject;
    procedure SetPageIndex(Value: Integer);
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetChildOwner: TComponent; override;
    procedure ReadState(Reader: TReader); override;
    procedure ShowControl(AControl: TControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Page: TObject read GetPage;
  published
    property ActivePage: string read GetActivePage write SetActivePage stored False;
    property Align;
    property Anchors;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Enabled;
    property Constraints;
    property PageIndex: Integer read FPageIndex write SetPageIndex default 0;
    property Pages: TStrings read FPages write SetPages stored False;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TBannerBook }

  TBannerItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: TCaption;
    FImageRect: TRect;
    FVisible: Boolean;
    procedure SetCaption(Value: TCaption);
    procedure SetImageIndex(Value: Integer);
    procedure SetVisible(Value: Boolean);
	protected
  	property ImageRect: TRect read FImageRect write FImageRect;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
	published
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Caption: TCaption read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible;
  end;

{ TBannerItems }

  TBannerItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TBannerItem;
    procedure Put(Index: Integer; Value: TBannerItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TBannerItem;
    function FindItemID(ID: Integer): TBannerItem;
    function Insert(Index: Integer): TBannerItem;
    property Items[Index: Integer]: TBannerItem read Get write Put; default;
  end;

{ TBanner }

  TBanner = class(TPaintPanel, IUnknown, IIgnoreMargin, IIgnoreResize)
  private
    FBannerBook: TBannerBook;
    FBackground: TGraphic;
    FItemIndex: Integer;
    FItems: TBannerItems;
    FImages: TCustomImageList;
    FDownIndex: Integer;
    FHotIndex: Integer;
    FOnDrawItem: TDrawIndexDefaultEvent;
    FOnSelectItem: TNotifyEvent;
    FAutoHeight: Boolean;
		FImageChangeLink: TChangeLink;
    procedure ImageListChange(Sender: TObject);
    procedure SetBannerBook(Value: TBannerBook);
    procedure SetHotIndex(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TBannerItems);
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
		procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  	procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property HotIndex: Integer read FHotIndex write SetHotIndex;
    procedure ItemsChanged;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Background: TGraphic read FBackground write FBackground;
  published
  	property Align;
  	property AutoHeight: Boolean read FAutoHeight write FAutoHeight default True;
    property BannerBook: TBannerBook read FBannerBook write SetBannerBook;
  	property Items: TBannerItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Images: TCustomImageList read FImages write SetImages;
    property Visible;
		property Font;
    property ParentFont;
    property Enabled;
    property TabOrder;
    property TabStop;
    property OnDrawItem: TDrawIndexDefaultEvent read FOnDrawItem write FOnDrawItem;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;
  end;

var
	RegisterPages: procedure (PageClass: TClass);
  PagesRegistered: Boolean;

implementation

{ TBannerPage }

type
  TBannerPage = class(TCustomControl)
  private
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Height stored False;
    property TabOrder stored False;
    property Visible stored False;
    property Width stored False;
  end;

{ TBannerBookStrings }

  TBannerBookStrings = class(TStrings)
  private
    FPageList: TList;
    FBannerBook: TBannerBook;
  protected
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
    procedure Put(Index: Integer; const S: string); override;
    function GetObject(Index: Integer): TObject; override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create(PageList: TList; BannerBook: TBannerBook);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

constructor TBannerBookStrings.Create(PageList: TList; BannerBook: TBannerBook);
begin
  inherited Create;
  FPageList := PageList;
  FBannerBook := BannerBook;
end;

function TBannerBookStrings.GetCount: Integer;
begin
  Result := FPageList.Count;
end;

function TBannerBookStrings.Get(Index: Integer): string;
begin
  Result := TBannerPage(FPageList[Index]).Caption;
end;

procedure TBannerBookStrings.Put(Index: Integer; const S: string);
begin
  TBannerPage(FPageList[Index]).Caption := S;
end;

function TBannerBookStrings.GetObject(Index: Integer): TObject;
begin
  Result := FPageList[Index];
end;

procedure TBannerBookStrings.SetUpdateState(Updating: Boolean);
begin
  { do nothing }
end;

procedure TBannerBookStrings.Clear;
var
  I: Integer;
begin
  for I := 0 to FPageList.Count - 1 do
    TObject(FPageList[I]).Free;
  FPageList.Clear;
end;

procedure TBannerBookStrings.Delete(Index: Integer);
var
  Form: TCustomForm;
begin
  TObject(FPageList[Index]).Free;
  FPageList.Delete(Index);
  FBannerBook.PageIndex := 0;
  if csDesigning in FBannerBook.ComponentState then
  begin
    Form := GetParentForm(FBannerBook);
    if (Form <> nil) and (Form.Designer <> nil) then
      Form.Designer.Modified;
  end;
end;

procedure TBannerBookStrings.Insert(Index: Integer; const S: string);
var
  Page: TBannerPage;
  Form: TCustomForm;
begin
  Page := TBannerPage.Create(FBannerBook.Owner);
  with Page do
  begin
    Parent := FBannerBook;
    Caption := S;
  end;
  FPageList.Insert(Index, Page);
  FBannerBook.PageIndex := Index;
  if csDesigning in FBannerBook.ComponentState then
  begin
		Form := GetParentForm(FBannerBook);
    if (Form <> nil) and (Form.Designer <> nil) then
      Form.Designer.Modified;
  end;
end;

procedure TBannerBookStrings.Move(CurIndex, NewIndex: Integer);
var
  AObject: TObject;
begin
  if CurIndex <> NewIndex then
  begin
    AObject := FPageList[CurIndex];
    FPageList[CurIndex] := FPageList[NewIndex];
    FPageList[NewIndex] := AObject;
  end;
end;

{ TBannerPage }

constructor TBannerPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Color := clBtnFace;
  Visible := False;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible,
    csParentBackground];
  Align := alClient;
end;

procedure TBannerPage.ReadState(Reader: TReader);
begin
  if Reader.Parent is TBannerBook then
    TBannerBook(Reader.Parent).FPageList.Add(Self);
  inherited ReadState(Reader);
end;

procedure TBannerPage.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
	FillRectColor(Msg.DC, ClientRect, Color);
	if csDesigning in ComponentState then
		FillRectOutline(Msg.DC, ClientRect, Blend(Color, 0));
	Msg.Result := 1;
end;

procedure TBannerPage.WMNCHitTest(var Msg: TWMNCHitTest);
begin
	Msg.Result := HTTRANSPARENT;
end;

{ TBannerBook }

var
	EmptyRect: TRect;
	Registered: Boolean;

constructor TBannerBook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
	if not Registered then
		RegisterPages(TBannerPage);
  Color := clBtnFace;
  Registered := True;
  Width := 150;
  Height := 150;
  FPageList := TList.Create;
  FPages := TBannerBookStrings.Create(FPageList, Self);
  FPageIndex := -1;
  FPages.Add('default');
  PageIndex := 0;
  //Exclude(FComponentStyle, csInheritable);
  ControlStyle := ControlStyle + [csParentBackground];
end;

destructor TBannerBook.Destroy;
begin
  FPages.Free;
  FPageList.Free;
  inherited Destroy;
end;

procedure TBannerBook.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

function TBannerBook.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TBannerBook.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to FPageList.Count - 1 do Proc(TControl(FPageList[I]));
end;

procedure TBannerBook.ReadState(Reader: TReader);
begin
  Pages.Clear;
  inherited ReadState(Reader);
  if (FPageIndex <> -1) and (FPageIndex >= 0) and (FPageIndex < FPageList.Count) then
    with TBannerPage(FPageList[FPageIndex]) do
    begin
      BringToFront;
      Visible := True;
      Align := alClient;
    end
  else FPageIndex := -1;
end;

procedure TBannerBook.ShowControl(AControl: TControl);
var
	OldPage: Integer;
  I: Integer;
begin
  for I := 0 to FPageList.Count - 1 do
    if FPageList[I] = AControl then
    begin
    	OldPage :=  FPageIndex;
      SetPageIndex(I);
      if OldPage > -1 then
	      SetPageIndex(OldPage);
      Exit;
    end;
  inherited ShowControl(AControl);
end;

procedure TBannerBook.SetPages(Value: TStrings);
var
	A, B: Integer;
  S: string;
	I: Integer;
begin
	FPages.BeginUpdate;
	try
 		{ remove duplicates }
  	for I := Value.Count - 1 downto 0 do
		begin
			S := Trim(Value[I]);
			if S = '' then Continue;
			if Value.IndexOf(S) < I then
			Value.Delete(I);
		end;
		{ add new items }
		for I := 0 to Value.Count - 1 do
		begin
			S := Trim(Value[I]);
			if S = '' then Continue;
			if FPages.IndexOf(S) < 0 then
			FPages.Add(S);
		end;
		{ remove deleted items }
		for I := FPages.Count - 1 downto 0 do
		begin
			S := Trim(FPages[I]);
			if S = '' then Continue;
			if Value.IndexOf(S) < 0 then
			FPages.Delete(I);
		end;
		{ change item ordering }
		for I := 0 to Value.Count - 1 do
		begin
			S := Trim(Value[I]);
			if S = '' then Continue;
			A := FPages.IndexOf(S);
			B := Value.IndexOf(S);
			if A <> B then
			FPages.Move(A, B);
		end;
		if FPages.Count = 0 then FPages.Add('default');
	finally
		FPages.EndUpdate;
	end;
end;

function TBannerBook.GetPage: TObject;
begin
	if FPageIndex > -1 then
		Result := TObject(FPageList[FPageIndex])
	else
		Result := nil;		
end;

procedure TBannerBook.SetPageIndex(Value: Integer);
var
	ParentForm: TCustomForm;
begin
	if Value < 0 then Value := 0;
	if csLoading in ComponentState then
	begin
		FPageIndex := Value;
		Exit;
	end;
	if (Value <> FPageIndex) and (Value > -1) and (Value < FPageList.Count) then
	begin
		ParentForm := GetParentForm(Self);
		if ParentForm <> nil then
			if ContainsControl(ParentForm.ActiveControl) then
				ParentForm.ActiveControl := Self;
		with TBannerPage(FPageList[Value]) do
		begin
			BringToFront;
			Visible := True;
			Align := alClient;
		end;
		if (FPageIndex > -1) and (FPageIndex < FPageList.Count) then
			TBannerPage(FPageList[FPageIndex]).Visible := False;
		FPageIndex := Value;
		if ParentForm <> nil then
			if ParentForm.ActiveControl = Self then SelectFirst;
		if Assigned(FOnPageChanged) then
			FOnPageChanged(Self);
	end;
end;

procedure TBannerBook.SetActivePage(const Value: string);
begin
	SetPageIndex(FPages.IndexOf(Value));
end;

function TBannerBook.GetActivePage: string;
begin
	Result := FPages[FPageIndex];
end;

procedure TBannerBook.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
	FillRectColor(Msg.DC, ClientRect, Color);
	if csDesigning in ComponentState then
		FillRectOutline(Msg.DC, ClientRect, Blend(Color, 0));
	Msg.Result := 1;
end;

{ TBannerItem }

constructor TBannerItem.Create(Collection: TCollection);
begin
	inherited Create(Collection);
  FImageIndex := Collection.Count - 1;
	FVisible := True;
end;

procedure TBannerItem.Assign(Source: TPersistent);
var
  EditItem: TBannerItem absolute Source;
begin
  if Source is TBannerItem then
  begin
		FImageIndex := EditItem.ImageIndex;
    FCaption := EditItem.Caption;
    FVisible := EditItem.Visible;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TBannerItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TBannerItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

procedure TBannerItem.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    Changed(False);
    if Collection.Owner is TComponent then
	    if not (csDesigning in TComponent(Collection.Owner).ComponentState) then
				FImageRect := EmptyRect;
  end;
end;

{ TBannerItems }

constructor TBannerItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TBannerItem);
end;

function TBannerItems.Add: TBannerItem;
begin
  Result := TBannerItem(inherited Add);
end;

function TBannerItems.FindItemID(ID: Integer): TBannerItem;
begin
  Result := TBannerItem(inherited FindItemID(ID));
end;

function TBannerItems.Insert(Index: Integer): TBannerItem;
begin
  Result := TBannerItem(inherited Insert(Index));
end;

procedure TBannerItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TBanner then
  	TBanner(GetOwner).ItemsChanged;
end;

function TBannerItems.Get(Index: Integer): TBannerItem;
begin
  Result := TBannerItem(GetItem(Index));
end;

procedure TBannerItems.Put(Index: Integer; Value: TBannerItem);
begin
  SetItem(Index, Value);
end;

{ TBanner }

constructor TBanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse];
  FAutoHeight := True;
	FItems := TBannerItems.Create(Self);
  FItemIndex := -1;
  FHotIndex := -1;
  FDownIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  // Align := alTop;
  Color := clWhite;
  ParentColor := False;
  ParentFont := True;
  Height := 48;
  Width := 96;
end;

destructor TBanner.Destroy;
begin
	BannerBook := nil;
  Images := nil;
  FImageChangeLink.Free;
	inherited Destroy;
end;

procedure TBanner.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited Notification(AComponent, Operation);
	if Operation = opRemove then
   	if AComponent = FBannerBook then
     	BannerBook := nil
		else if AComponent = FImages then
     	Images := nil;
end;


procedure TBanner.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	P: TPoint;
  H: Integer;
	I: Integer;
begin
	if csDesigning in ComponentState then Exit;
	P := Point(X, Y);
  H := -1;
	for I := 0 to FItems.Count - 1 do
  	if PtInRect(FItems[I].ImageRect, P) then
    begin
    	H := I;
      Break;
    end;
	HotIndex := H;
  inherited MouseMove(Shift, X, Y);
end;

procedure TBanner.Paint;

	function ImageHeight: Integer;
  begin
  	if FImages = nil then
    	Result := 32
		else
    	Result := FImages.Height;
  end;

	function ImageWidth: Integer;
  begin
  	if FImages = nil then
    	Result := 32
		else
    	Result := FImages.Width;
  end;

const
	Border = 8;
var
  DefaultDraw: Boolean;
  State: TDrawState;
	A, B: TRect;
  H, W, ImgWidth, ImageDelta: Integer;
	I: Integer;
begin
  if FBackground <> nil then
    Canvas.Draw(0, 0, FBackground)
  else
    DrawThemeSeperator(Canvas.Handle, ClientRect, Color, True);
	if FItemIndex > Items.Count then
  	FItemIndex := FItems.Count - 1;
 	Canvas.Font := Font;
   H := ImageHeight + Canvas.TextHeight('Wg');
   if H mod 10 > 0 then
   	H := (H div 10) * 10 + 10;
	if FAutoHeight and (H <> Height) then
     Height := H;
	A := ClientRect;
 	Canvas.Font := Font;
   A.Bottom := A.Bottom - 1;
   A.Right := 10;
	ImgWidth := ImageWidth + Border * 2;
 	for I := 0 to FItems.Count - 1 do
   begin
   	W := Canvas.TextWidth(FItems[I].Caption) + Border * 2;
     if W > ImgWidth then
			ImageDelta := ((W - ImgWidth) shr 1) + Border
     else
     begin
     	ImageDelta := Border;
       W := ImgWidth;
		end;
    if (not FItems[I].Visible) and (not (csDesigning in ComponentState)) then
    begin
    FItems[I].ImageRect := EmptyRect;
      Continue;
    end;
    B := A;
    OffsetRect(B, WidthOf(A), 0);
    B.Right := B.Left + W;
    A := B;
    FItems[I].ImageRect := A;
		InflateRect(A, -1, 0);
    State := [dsBackground];
    if I = FHotIndex then
    begin
      State := State + [dsHot];
      if I = FDownIndex then
        State := State + [dsPressed];
    end;
    if I = FItemIndex then
    begin
     	Canvas.Brush.Color := Blend(clInactiveCaptionText, clInactiveCaption, 70);
      State := State + [dsSelected];
    end
   	else if I = FHotIndex then
     	Canvas.Brush.Color := clInactiveCaptionText
    else
      Canvas.Brush.Color := Color;
    DefaultDraw := True;
    if Assigned(FOnDrawItem) then
      FOnDrawItem(Self, Canvas, I, A, State, DefaultDraw);
    if DefaultDraw then
  		Canvas.FillRect(A);
		InflateRect(A, 1, 0);
     B.Top := B.Top + ImageHeight;
     if Trim(FItems[I].Caption) = '' then
			ImageListDraw(FImages, Canvas, A.Left + ImageDelta, (Height - ImageHeight) shr 1,
       	FItems[I].ImageIndex, Enabled)
     else
     begin
				ImageListDraw(FImages, Canvas, A.Left + ImageDelta, (B.Top + HeightOf(B) div 2 -
					ImageHeight) shr 1, FItems[I].ImageIndex, Enabled);
				if FImages = nil then
        	B.Top := 0;
				DrawCaption(Canvas.Handle, FItems[I].Caption, B, drCenter);
     end;
	end;
end;

procedure TBanner.ItemsChanged;
var
	Strings: TStrings;
  S: string;
	I: Integer;
begin
	Invalidate;
	if (FBannerBook <> nil) and (csDesigning in ComponentState) then
  begin
  	Strings := TStringList.Create;
		try
    	for I := 0 to Items.Count - 1 do
      begin
      	S := Items[I].Caption;
        if Strings.IndexOf(S) > -1 then Continue;
        Strings.Add(S);
      end;
			FBannerBook.Pages := Strings;
      FBannerBook.PageIndex := FItemIndex;
    finally
    	Strings.Free;
    end;
	end;
end;

procedure TBanner.SetHotIndex(Value: Integer);
begin
	if Value <> FHotIndex then
  begin
	  FHotIndex := Value;
    Invalidate;
  end;
end;

procedure TBanner.SetImages(Value: TCustomImageList);
begin
	if Value <> FImages then
  begin
  	if FImages <> nil then
    begin
    	FImages.RemoveFreeNotification(Self);
    	FImages.UnRegisterChanges(FImageChangeLink);
		end;
	  FImages := Value;
  	if FImages <> nil then
    begin
    	FImages.FreeNotification(Self);
    	FImages.RegisterChanges(FImageChangeLink);
		end;
		Invalidate;
  end;
end;

procedure TBanner.SetItemIndex(Value: Integer);
begin
	if Value < -1 then
  	Value := -1;
	if Value > FItems.Count - 1 then
  	Value := FItems.Count -1;
  if Value <> FItemIndex then
  begin
  	FItemIndex := Value;
    Invalidate;
    if FBannerBook <> nil then
    	FBannerBook.PageIndex := FItemIndex;
    if Assigned(FOnSelectItem) then	FOnSelectItem(Self);
  end;
end;

procedure TBanner.SetItems(Value: TBannerItems);
begin
  FItems.Assign(Value);
  Invalidate;
end;

procedure TBanner.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
	P: TPoint;
	I: Integer;
begin
	if Button = mbLeft then
  begin
  	if not (csDesigning in ComponentState) then
		  SetFocus;
		P := Point(X, Y);
		for I := 0 to FItems.Count - 1 do
  		if PtInRect(FItems[I].ImageRect, P) then
	    begin
  	  	FDownIndex := I;
        Invalidate;
    	  Break;
	    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TBanner.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
	Form: TCustomForm;
	P: TPoint;
	I: Integer;
begin
	if Button = mbLeft then
  begin
		P := Point(X, Y);
		for I := 0 to FItems.Count - 1 do
  		if PtInRect(FItems[I].ImageRect, P) then
	    begin
  	  	if FDownIndex = I then
        begin
        	ItemIndex := I;
			    if csDesigning in ComponentState then
			    begin
      			Form := GetParentForm(Self);
		  	    if (Form <> nil) and (Form.Designer <> nil) then
    		    Form.Designer.Modified;
			    end;
				end;
    	  Break;
	    end;
    if FDownIndex > -1 then
    begin
      FDownIndex := -1;
      Invalidate;
    end;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TBanner.KeyDown(var Key: Word; Shift: TShiftState);

	function FindPriorItem: Integer;
  var
    I: Integer;
  begin
  	Result := ItemIndex;
    if Result < 0 then Exit;
    for I := Result - 1 downto 0 do
    	if Items[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
    for I := Items.Count - 1 downto 0 do
    	if Items[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
  end;

	function FindNextItem: Integer;
  var
    I: Integer;
  begin
  	Result := ItemIndex;
    if Result < 0 then Exit;
    for I := Result + 1 to Items.Count - 1 do
    	if Items[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
    for I := 0 to Items.Count - 1 do
    	if Items[I].Visible then
      begin
      	Result := I;
        Exit;
      end;
  end;

begin
  inherited KeyDown(Key, Shift);
  case Key of
  	VK_UP, VK_LEFT: ItemIndex := FindPriorItem;
		VK_DOWN, VK_RIGHT: ItemIndex := FindNextItem;
	end;
end;

procedure TBanner.ImageListChange(Sender: TObject);
begin
	Invalidate;
end;

procedure TBanner.SetBannerBook(Value: TBannerBook);
begin
	if Value <> FBannerBook then
  begin
  	if FBannerBook <> nil then
    	FBannerBook.RemoveFreeNotification(Self);
	  FBannerBook := Value;
  	if FBannerBook <> nil then
    	FBannerBook.FreeNotification(Self);
    if csDesigning in ComponentState then
	    ItemsChanged;
  end;
end;

procedure TBanner.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
	P: TPoint;
	I: Integer;
begin
	inherited;
	Msg.Result := 0;
  P := Point(Msg.XPos, Msg.YPos);
	for I := 0 to FItems.Count - 1 do
 		if PtInRect(FItems[I].ImageRect, P) then
    begin
			Msg.Result := 1;
   	  Break;
    end;
end;

procedure TBanner.CMEnabledChanged(var Msg: TMessage);
begin
	inherited;
  Invalidate;
end;

procedure TBanner.CMMouseLeave(var Msg: TMessage);
begin
	inherited;
	HotIndex := -1;
end;

procedure TBanner.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
{var
  C: TCanvas;}
begin
  {if FBackground <> nil then
  begin
    C := TCanvas.Create;
    try
      C.Handle := Msg.DC;
      C.Draw(0, 0, FBackground);
    finally
      C.Free;
    end;
  end
  else
    DrawThemeSeperator(Msg.DC, ClientRect, Color, True);}
  Msg.Result := 1;
end;

procedure TBanner.WMGetDlgCode(var Msg: TMessage);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure InternalRegisterPages(PageClass: TClass);
begin
	if not PageClass.InheritsFrom(TPersistent) then Exit;
  if PagesRegistered then Exit;
	if GetClass(PageClass.ClassName) = nil then
		RegisterClass(TPersistentClass(PageClass));
	PagesRegistered := True;
end;

initialization
	@RegisterPages := @InternalRegisterPages;
end.
