(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit SuplDBCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Menus, Forms,
  StdCtrls, ComCtrls, DB, DBCtrls, BaseTypes, GraphTools, StrTools, FormTools,
  {$IFDEF GDIAPI}GdiApi, GdiIntf,{$ENDIF}
  ImgList, ScrollCtrls, BtnCtrls;

type
  TButtonData = array[TNavigateBtn] of TImageSpeedButton;

  TDataButtonActionEvent = procedure(Sender: TObject; Index: TNavigateBtn;
    var AllowAction) of object;

  TDataButtons = class(TComponent)
  private
    FButtons: TButtonData;
    FDataLink: TDataLink;
    FEnabled: Boolean;
    FOnBeforeAction: TDataButtonActionEvent;
    FOnAfterAction: TDataButtonActionEvent;
    procedure SetEnabled(Value: Boolean);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    function GetButtonOrd(Index: Integer): TImageSpeedButton;
    procedure SetButtonOrd(Index: Integer; Value: TImageSpeedButton);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure EditingChanged;
    procedure DataChanged;
    procedure ActiveChanged;
    procedure SafeChangeEnable(Index: TNavigateBtn; Enabled: Boolean);
    function GetButton(Index: TNavigateBtn): TImageSpeedButton;
    procedure SetButton(Index: TNavigateBtn; Value: TImageSpeedButton);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ButtonClick(Button: TImageSpeedButton);
  published
    property ButtonFirst: TImageSpeedButton index 0 read GetButtonOrd write SetButtonOrd;
    property ButtonPrior: TImageSpeedButton index 1 read GetButtonOrd write SetButtonOrd;
    property ButtonNext: TImageSpeedButton index 2 read GetButtonOrd write SetButtonOrd;
    property ButtonLast: TImageSpeedButton index 3 read GetButtonOrd write SetButtonOrd;
    property ButtonInsert: TImageSpeedButton index 4 read GetButtonOrd write SetButtonOrd;
    property ButtonDelete: TImageSpeedButton index 5 read GetButtonOrd write SetButtonOrd;
    property ButtonEdit: TImageSpeedButton index 6 read GetButtonOrd write SetButtonOrd;
    property ButtonPost: TImageSpeedButton index 7 read GetButtonOrd write SetButtonOrd;
    property ButtonCancel: TImageSpeedButton index 8 read GetButtonOrd write SetButtonOrd;
    property ButtonRefresh: TImageSpeedButton index 9 read GetButtonOrd write SetButtonOrd;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnBeforeAction: TDataButtonActionEvent read FOnBeforeAction write FOnBeforeAction;
    property OnAfterAction: TDataButtonActionEvent read FOnAfterAction write FOnAfterAction;
  end;

{ TCustomDBDrawList }

  TCustomDBDrawList = class(TFramedWindow)
  private
    FActive: Boolean;
    FDataLink: TDataLink;
    FDisplayWidth: Integer;
    FColumnCount: Integer;
    FInternalFocused: Boolean;
    FItemHeight: Integer;
    FRecordCount: Integer;
    FRecordIndex: Integer;
    FTracking: Boolean;
    FScrollPos: Integer;
    FOnDrawBackground: TNotifyEvent;
    FOnDrawItem: TDrawItemEvent;
    procedure DataLinkChanged;
    procedure SelectItemAt(X, Y: Integer);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDisplayWidth(Value: Integer);
    procedure SetItemHeight(Value: Integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DrawItem(const Rect: TRect; Index: Integer); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer); override;
    procedure Paint; override;
    property ColumnCount: Integer read FColumnCount;
    property DisplayWidth: Integer read FDisplayWidth write SetDisplayWidth;
    property RecordIndex: Integer read FRecordIndex;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property InternalFocused: Boolean read FInternalFocused;
    property ItemHeight: Integer read FItemHeight write SetItemHeight default 18;
    property OnDrawBackground: TNotifyEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawItem: TDrawItemEvent read FOnDrawitem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

{ TDBDrawList }

  TDBDrawList = class(TCustomDBDrawList)
  public
    property Canvas;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property DataSource;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
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
    property OnDrawBackground;
    property OnDrawItem;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

{ TReportViewColumn }

  TCustomDBReportView = class;

  TReportViewColumn = class(TCollectionItem)
  private
    FSection: THeaderSection;
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetText: string;
    procedure SetText(Value: string);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property Text: string read GetText write SetText;
    property Width: Integer read GetWidth write SetWidth;
  end;

{ TReportViewColumns }

  TReportViewColumns = class(TCollection)
  private
    FReportView: TCustomDBReportView;
    function GetItem(Index: Integer): TReportViewColumn;
    procedure SetItem(Index: Integer; Value: TReportViewColumn);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ReportView: TCustomDBReportView);
    function Add: TReportViewColumn;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    property Items[Index: Integer]: TReportViewColumn read GetItem write SetItem; default;
  end;

{ TCustomDBReportView }

  TReportViewStyle = (rsColumn, rsGrid);

  TColumnClickEvent = procedure (Sender: TObject; Column: TReportViewColumn)
    of object;
  TCalcImageIndexEvent = procedure (Sender: TObject;
    var ImageIndex: Integer)  of object;
  TCalcTextEvent = procedure (Sender: TObject; Index: Integer;
    var Text: string) of object;

  TCustomDBReportView = class(TWinControl)
  private
    FBorderStyle: TBorderStyle;
    FColumns: TReportViewColumns;
    FDefaultDraw: Boolean;
    FDrawList: TDBDrawList;
    FForwarding: Boolean;
    FHeaderControl: THeaderControl;
    FImages: TCustomImageList;
    FDefDrawListProc: TWndMethod;
    FOnCalcImageIndex: TCalcImageIndexEvent;
    FOnCalcText: TCalcTextEvent;
    FOnColumnClick: TColumnClickEvent;
    FOnDrawItem: TDrawItemEvent;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetDefaultDraw(Value: Boolean);
    procedure SetColumns(Value: TReportViewColumns);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure SetImages(Value: TCustomImageList);
    function GetFlat: Boolean;
    procedure SetFlat(Value: Boolean);
    function GetStyle: TReportViewStyle;
    procedure SetStyle(Value: TReportViewStyle);
    procedure DrawListProc(var Message: TMessage);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure DoSectionClick(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure DoSectionResize(HeaderControl: THeaderControl;
      Section: THeaderSection);
    procedure WndProc(var Message: TMessage); override;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultDraw: Boolean read FDefaultDraw write SetDefaultDraw default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle
      default bsSingle;
    property Columns: TReportViewColumns read FColumns write SetColumns;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Style: TReportViewStyle read GetStyle write SetStyle default rsColumn;
    property OnCalcImageIndex: TCalcImageIndexEvent read
      FOnCalcImageIndex write FOnCalcImageIndex;
    property OnCalcText: TCalcTextEvent read FOnCalcText write
      FOnCalcText;
    property OnColumnClick: TColumnClickEvent read FOnColumnClick write
      FOnColumnClick;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

{ TDBReportView }

  TDBReportView = class(TCustomDBReportView)
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property DataSource;
    property DefaultDraw;
    property BiDiMode;
    property Caption;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property Images;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCalcImageIndex;
    property OnCalcText;
    property OnColumnClick;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDrawItem;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TDataButtonLink }

type
  TDataButtonLink = class(TDataLink)
  private
    FButtons: TDataButtons;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(Buttons: TDataButtons);
  end;

constructor TDataButtonLink.Create(Buttons: TDataButtons);
begin
  inherited Create;
  FButtons := Buttons;
  VisualControl := True;
end;

procedure TDataButtonLink.EditingChanged;
begin
  if FButtons <> nil then FButtons.EditingChanged;
end;

procedure TDataButtonLink.DataSetChanged;
begin
  if FButtons <> nil then FButtons.DataChanged;
end;

procedure TDataButtonLink.ActiveChanged;
begin
  if FButtons <> nil then FButtons.ActiveChanged;
end;

{ TDataButtons }

constructor TDataButtons.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FDataLink := TDataButtonLink.Create(Self);
end;

destructor TDataButtons.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TDataButtons.ButtonClick(Button: TImageSpeedButton);
var
  AllowAction: Boolean;
  B, I: TNavigateBtn;
begin
  if Button = nil then Exit;
  B := High(TNavigateBtn);
  for I := Low(TNavigateBtn) to High(TNavigateBtn) do
    if Button = FButtons[I] then
    begin
      B := I;
      Break;
    end
    else if I = B then
      Exit;
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    AllowAction := True;
    if Assigned(FOnBeforeAction) then
      FOnBeforeAction(Self, B, AllowAction);
    if not AllowAction then
      Exit;
    with DataSource.DataSet do
    begin
      case B of
        nbPrior: Prior;
        nbNext: Next;
        nbFirst: First;
        nbLast: Last;
        nbInsert: Insert;
        nbEdit: Edit;
        nbCancel: Cancel;
        nbPost: Post;
        nbRefresh: Refresh;
        nbDelete: Delete;
      end;
    end;
    if Assigned(FOnAfterAction) then
      FOnAfterAction(Self, B, AllowAction);
  end;
end;

procedure TDataButtons.SafeChangeEnable(Index: TNavigateBtn; Enabled: Boolean);
begin
  if FButtons[Index] <> nil then
    FButtons[Index].Enabled := Enabled;
end;

function TDataButtons.GetButton(Index: TNavigateBtn): TImageSpeedButton;
begin
  Result := FButtons[Index];
end;

procedure TDataButtons.SetButton(Index: TNavigateBtn; Value: TImageSpeedButton);
var
  OldValue: TImageSpeedButton;
  Found: Boolean;
  I: TNavigateBtn;
begin
  if FButtons[Index] <> Value then
  begin
    OldValue := FButtons[Index];
    FButtons[Index] := nil;
    if OldValue <> nil then
    begin
      Found := False;
      for I := Low(TNavigateBtn) to High(TNavigateBtn) do
        if FButtons[I] = OldValue then
        begin
          Found := True;
          Break;
        end;
      if not Found then
        OldValue.RemoveFreeNotification(Self);
    end;
    FButtons[Index] := Value;
    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      ActiveChanged;
    end;
  end;
end;

procedure TDataButtons.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  SafeChangeEnable(nbFirst, UpEnable);
  SafeChangeEnable(nbPrior, UpEnable);
  SafeChangeEnable(nbNext, DnEnable);
  SafeChangeEnable(nbLast, DnEnable);
  SafeChangeEnable(nbDelete, Enabled and FDataLink.Active and
    FDataLink.DataSet.CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF));
end;

procedure TDataButtons.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  SafeChangeEnable(nbInsert, CanModify);
  SafeChangeEnable(nbEdit, CanModify and not FDataLink.Editing);
  SafeChangeEnable(nbPost, CanModify and FDataLink.Editing);
  SafeChangeEnable(nbCancel, CanModify and FDataLink.Editing);
  SafeChangeEnable(nbRefresh, CanModify);
end;

procedure TDataButtons.ActiveChanged;
var
  I: TNavigateBtn;
begin
  if csLoading in ComponentState then Exit;
  if not (Enabled and FDataLink.Active) then
    for I := Low(FButtons) to High(FButtons) do
      SafeChangeEnable(I, False)
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDataButtons.SetDataSource(Value: TDataSource);
begin
  if Value <> FDataLink.DataSource then
  begin
    if FDataLink.DataSource <> nil then FDataLink.DataSource.FreeNotification(Self);
    FDataLink.DataSource := Value;
    ActiveChanged;
  end;
end;

function TDataButtons.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDataButtons.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: TNavigateBtn;
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TImageSpeedButton) then
    for I := Low(FButtons) to High(FButtons) do
      if FButtons[I] = AComponent then
        FButtons[I] := nil;
end;

procedure TDataButtons.Loaded;
begin
  inherited Loaded;
  ActiveChanged;
end;

procedure TDataButtons.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    ActiveChanged;
  end;
end;

function TDataButtons.GetButtonOrd(Index: Integer): TImageSpeedButton;
begin
  Result := GetButton(TNavigateBtn(Index));
end;

procedure TDataButtons.SetButtonOrd(Index: Integer; Value: TImageSpeedButton);
begin
  SetButton(TNavigateBtn(Index), Value);
end;

{ TDrawListLink }

type
  TDrawListLink = class(TDataLink)
  private
    FControl: TCustomDBDrawList;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
  public
    constructor Create(Control: TCustomDBDrawList);
  end;

constructor TDrawListLink.Create(Control: TCustomDBDrawList);
begin
  inherited Create;
  FControl := Control;
  ReadOnly := True;
end;

procedure TDrawListLink.ActiveChanged;
begin
  inherited ActiveChanged;
  if FControl <> nil then
    FControl.DataLinkChanged;
end;

procedure TDrawListLink.DataSetChanged;
begin
  inherited DataSetChanged;
  if FControl <> nil then
    FControl.DataLinkChanged;
end;

{ TCustomDBDrawList }

constructor TCustomDBDrawList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TDrawListLink.Create(Self);
  FItemHeight := 18;
  Color := clWindow;
  DoubleBuffered := True;
  ParentColor := False;
  TabStop := True;
  Width := 150;
  Height := 100;
end;

destructor TCustomDBDrawList.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

procedure TCustomDBDrawList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do 
    Style := Style or WS_VSCROLL;
end;

procedure TCustomDBDrawList.CreateWnd;
var
  ScrollInfo: TScrollInfo;
begin
  inherited CreateWnd;
  FillChar(ScrollInfo, SizeOf(TScrollInfo), #0);
  with ScrollInfo do
  begin
    cbSize := SizeOf(TScrollInfo);
    fMask := SIF_ALL;
    nMin := 0;
    nMax := 100;
    nPage := 10;
    nPos := FScrollPos;
  end;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
end;

function TCustomDBDrawList.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  with FDataLink do
    if (not Result) and (DataSet <> nil) then
      Result := DataSet.MoveBy(1) <> 0;
end;

function TCustomDBDrawList.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  with FDataLink do
    if (not Result) and (DataSet <> nil) then
      Result := DataSet.MoveBy(-1) <> 0;
end;

procedure TCustomDBDrawList.DrawItem(const Rect: TRect; Index: Integer);
var
  State: TOwnerDrawState;
begin
  if Assigned(FOnDrawitem) then
  begin
    if Index = RecordIndex then
    begin
      State := [odSelected];
      if FInternalFocused then
        Include(State, odFocused);
    end
    else
      State := [];
    FOnDrawItem(Self, Index, Rect, State);
  end;
end;

procedure TCustomDBDrawList.DataLinkChanged;
var
  Active: Boolean;
begin
  Active := FDataLink.Active;
  if Active then
  begin
    if not FActive then
    begin
      FActive := Active;
      FDataLink.BufferCount := ClientHeight div FItemHeight;;
    end;
    FColumnCount := FDataLink.DataSet.FieldCount;
    FRecordCount := FDataLink.RecordCount;
    FRecordIndex := FDataLink.ActiveRecord;
    if FDataLink.BOF then
      FScrollPos := 0
    else if FDataLink.EOF then
      FScrollPos := 100
    else
      FScrollPos := 45;
  end
  else
  begin
    FActive := Active;
    FColumnCount := -1;
    FRecordCount := -1;
    FRecordIndex := -1;
    FScrollPos := 0;
  end;
  if HandleAllocated then
    SetScrollPos(Handle, SB_VERT, FScrollPos, True);
  Invalidate;
end;

procedure TCustomDBDrawList.KeyDown(var Key: Word; Shift: TShiftState);
var
  Delta: Integer;
begin
  inherited KeyDown(Key, Shift);
  if not FActive then
    Exit;
  Delta := 0;
  case Key of
    VK_UP: Delta := -1;
    VK_DOWN: Delta := 1;
    VK_PRIOR: Delta := -FRecordCount;
    VK_NEXT: Delta := FRecordCount;
    VK_HOME: Delta := -MaxInt;
    VK_END: Delta := MaxInt;
  end;
  if Delta <> 0 then
  begin
    if Delta = -MaxInt then
      FDataLink.DataSet.First
    else if Delta = MaxInt then
      FDataLink.DataSet.Last
    else
      FDataLink.DataSet.MoveBy(Delta);
  end;
end;

procedure TCustomDBDrawList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TCustomDBDrawList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ssDouble in Shift then Exit;
  if Button = mbLeft then
  begin
    if CanFocus then
      SetFocus
    else
      PostMessage(Handle, WM_SETFOCUS, 0, 0);
    {else
    begin}
      MouseCapture := True;
      FTracking := True;
      SelectItemAt(X, Y);
    //end;
  end;
end;

procedure TCustomDBDrawList.Paint;
var
  DrawRect: TRect;
  PriorRecord: Integer;
  I: Integer;
begin
  DrawRect := ClientRect;
  if Assigned(FOnDrawBackground) then
    FOnDrawBackground(Self)
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(DrawRect);
  end;
  if FActive then
  begin
    PriorRecord := FDataLink.ActiveRecord;
    FDataLink.ActiveRecord := 0;
    with DrawRect do
    begin
      Bottom := Top;
      for I := 0 to FRecordCount - 1 do
      begin
        FDataLink.ActiveRecord := I;
        Inc(Bottom, FItemHeight);
        DrawItem(DrawRect, I);
        Inc(Top, FItemHeight);
      end;
    end;
    FDataLink.ActiveRecord := PriorRecord;
  end;
end;

procedure TCustomDBDrawList.SelectItemAt(X, Y: Integer);
var
  Delta: Integer;
begin
  if FActive then
  begin
    if Y < 0 then
      Y := 0;
    if Y >= ClientHeight then
      Y := ClientHeight - 1;
    Delta := Y div FItemHeight - FRecordIndex;
      FDataLink.DataSet.MoveBy(Delta);
  end;
end;

procedure TCustomDBDrawList.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if HandleAllocated then
    FDataLink.BufferCount := AHeight div FItemHeight;
end;

{procedure TCustomDBDrawList.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
    begin
      RecreateWnd;
      if not (csReading in ComponentState) then
        FDataLink.BufferCount := ClientHeight div FItemHeight;
    end;
  end;
end;}

function TCustomDBDrawList.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TCustomDBDrawList.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

procedure TCustomDBDrawList.SetDisplayWidth(Value: Integer);
begin
  if Value <> FDisplayWidth then
  begin
    FDisplayWidth := Value;
    //
  end;
end;

procedure TCustomDBDrawList.SetItemHeight(Value: Integer);
begin
  if Value < 1 then
    Exit;
  if Value <> FItemHeight then
  begin
    FItemHeight := Value;
    if FItemHeight < 1 then
      FItemHeight := 1;
    if HandleAllocated then
      FDataLink.BufferCount := ClientHeight div FItemHeight;
  end;
end;

procedure TCustomDBDrawList.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  Invalidate;
end;

procedure TCustomDBDrawList.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TCustomDBDrawList.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FInternalFocused := False;
  Invalidate;
end;

procedure TCustomDBDrawList.WMSetFocus(var message: TWMSetFocus);
begin
  inherited;
  FInternalFocused := True;
  Invalidate;
end;

procedure TCustomDBDrawList.WMVScroll(var Message: TWMVScroll);
begin
  if FActive then
    with Message, FDataLink.DataSet do
      case ScrollCode of
        SB_LINEUP: MoveBy(-1);
        SB_LINEDOWN: MoveBy(1);
        SB_PAGEUP: MoveBy(-FRecordCount);
        SB_PAGEDOWN: MoveBy(FRecordCount);
        SB_THUMBPOSITION:
          begin
            //
          end;
        SB_BOTTOM: Last;
        SB_TOP: First;
      end;
end;

{ TReportViewColumn }

constructor TReportViewColumn.Create(Collection: TCollection);
var
  Columns: TReportViewColumns absolute Collection;
begin
  inherited Create(Collection);
  FSection := Columns.FReportView.FHeaderControl.Sections.Add;
end;

destructor TReportViewColumn.Destroy;
begin
  FSection.Free;
  inherited Destroy;
end;

function TReportViewColumn.GetAlignment: TAlignment;
begin
  Result := FSection.Alignment;
end;

procedure TReportViewColumn.SetAlignment(Value: TAlignment);
begin
  FSection.Alignment := Value;
end;

function TReportViewColumn.GetText: string;
begin
  Result := FSection.Text;
end;

procedure TReportViewColumn.SetText(Value: string);
begin
  FSection.Text := Value;
end;

function TReportViewColumn.GetWidth: Integer;
begin
  Result := FSection.Width;
end;

procedure TReportViewColumn.SetWidth(Value: Integer);
begin
  FSection.Width := Value;
  Changed(True);
end;

{ TReportViewColumns }

constructor TReportViewColumns.Create(ReportView: TCustomDBReportView);
begin
  inherited Create(TReportViewColumn);
  FReportView := ReportView;
end;

function TReportViewColumns.Add: TReportViewColumn;
begin
  Result := TReportViewColumn(inherited Add);
end;

procedure TReportViewColumns.BeginUpdate;
begin
  inherited BeginUpdate;
  FReportView.FHeaderControl.Sections.BeginUpdate;
end;
procedure TReportViewColumns.EndUpdate;
begin
  inherited EndUpdate;
  FReportView.FHeaderControl.Sections.EndUpdate;
end;

procedure TReportViewColumns.Update(Item: TCollectionItem);
begin
  FReportView.FDrawList.Invalidate;
end;

function TReportViewColumns.GetItem(Index: Integer): TReportViewColumn;
begin
  Result := TReportViewColumn(inherited GetItem(Index));
end;

function TReportViewColumns.GetOwner: TPersistent;
begin
  Result := FReportView;
end;

procedure TReportViewColumns.SetItem(Index: Integer; Value: TReportViewColumn);
begin
  inherited SetItem(Index, Value);
end;

{ TCustomDBReportView }

constructor TCustomDBReportView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderStyle := bsSingle;
  FDefaultDraw := True;
  FHeaderControl := THeaderControl.Create(Self);
  with FHeaderControl do
  begin
    Parent := Self;
    FullDrag := False;
    OnSectionClick := DoSectionClick;
    OnSectionResize := DoSectionResize;
  end;
  FColumns := TReportViewColumns.Create(Self);
  FDrawList := TDBDrawList.Create(Self);
  with FDrawList do
  begin
    Parent := Self;
    Align := alClient;
    BorderStyle := bsNone;
    FDefDrawListProc := WindowProc;
    WindowProc := DrawListProc;
    OnDrawItem := DoDrawItem;
  end;
  Height := 200;
  Width := 200;
end;

destructor TCustomDBReportView.Destroy;
begin
  FColumns.Free;
  inherited Destroy;
end;

procedure TCustomDBReportView.WndProc(var Message: TMessage);
begin
  if not FForwarding then
    case Message.Msg of
      WM_KILLFOCUS, WM_SETFOCUS, WM_MOUSEFIRST..WM_MOUSELAST,
      WM_KEYFIRST..WM_KEYLAST:
        begin
          FForwarding := True;
          try
            FDefDrawListProc(Message);
          finally
            FForwarding := False;
          end;
        end;
    end;
  inherited WndProc(Message);
end;

procedure TCustomDBReportView.DrawListProc(var Message: TMessage);
begin
  if not FForwarding then
    case Message.Msg of
      WM_KILLFOCUS, WM_SETFOCUS, WM_MOUSEFIRST..WM_MOUSELAST,
      WM_KEYFIRST..WM_KEYLAST:
        begin
          FForwarding := True;
          try
            WindowProc(Message);
          finally
            FForwarding := False;
          end;
        end;
    end;
  FDefDrawListProc(Message);
end;

procedure TCustomDBReportView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
  end;
end;

procedure TCustomDBReportView.DoDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  BooleanIdents: array [Boolean] of string = ('False', 'True');
var
  Canvas: TCanvas;
  I: Integer;

  procedure DrawImage;
  var
    ImageIndex: Integer;
    PriorBlendColor: TColor;
    PriorDrawStyle: TDrawingStyle;
  begin
    ImageIndex := 0;
    if Assigned(FOnCalcImageIndex) then
      FOnCalcImageIndex(Self, ImageIndex);
    if FImages <> nil then
    begin
      PriorBlendColor := FImages.BlendColor;
      FImages.BlendColor := clHighlight;
      PriorDrawStyle := FImages.DrawingStyle;
      if odFocused in State then
        FImages.DrawingStyle := dsSelected
      else
        FImages.DrawingStyle := dsNormal;
      FImages.Draw(Canvas, Rect.Left, Rect.Top, ImageIndex);
      FImages.DrawingStyle := PriorDrawStyle;
      FImages.BlendColor := PriorBlendColor;
      Inc(Rect.Left, FImages.Width + 4);
    end;
  end;

  procedure DrawColumn(Index: Integer);
  var
    Text: string;
    Section: THeaderSection;

    procedure DrawCaptionColumn;
    var
      PriorBrushColor: TColor;
      PriorFontColor: TColor;
      FocusRect: TRect;
    begin
      Rect.Right := Section.Width - 2;
      if odFocused in State then
      begin
        PriorBrushColor := Canvas.Brush.Color;
        Canvas.Brush.Color := clHighlight;
        PriorFontColor := Canvas.Font.Color;
        Canvas.Font.Color := clWindow;
        FocusRect := CalcCaptionRect(Canvas.Handle, Text, Rect,
          AlignmentToDirection(Section.Alignment));
        InflateRect(FocusRect, 1, 1);
        Canvas.FillRect(FocusRect);
        InflateRect(FocusRect, 1, 0);
        Canvas.DrawFocusRect(FocusRect);
        DrawCaption(Canvas.Handle, Text, Rect, AlignmentToDirection(Section.Alignment));
        Canvas.Font.Color := PriorFontColor;
        Canvas.Brush.Color := PriorBrushColor;
      end
      else
        DrawCaption(Canvas.Handle, Text, Rect, AlignmentToDirection(Section.Alignment));
      Rect.Left := Rect.Right + 2;
    end;

    procedure DrawTextColumn;
    begin
      Rect.Right := Rect.Left + Section.Width - 2;
      DrawText(Canvas.Handle, PChar(Text), -1, Rect, DR_FORMAT or DT_VCENTER or
        DT_LEFT or DT_NOPREFIX);
      Rect.Left := Rect.Right + 2;
    end;

  var
    Field: TField;
  begin
    Text := '';
    if I < FDrawList.ColumnCount then
    begin
      Field := DataSource.DataSet.Fields[Columns[I].FSection.Index];
      case Field.DataType of
        ftString, ftSmallint, ftInteger, ftWord, ftFloat, ftAutoInc,
          ftWideString, ftLargeint:
          Text := Field.AsString;
        ftBoolean:
          Text := BooleanIdents[Field.AsBoolean];
        ftCurrency:
          Text := Format(DefaultCurrencyFormat, [Field.AsFloat]);
        ftDate:
          Text := FormatDateTime(DefaultDateFormat, Field.AsDateTime);
        ftTime:
          Text := FormatDateTime(DefaultTimeFormat, Field.AsDateTime);
        ftDateTime:
          Text := FormatDateTime(DefaultDateTimeFormat, Field.AsDateTime);
      end;
    end;
    if Assigned(FOnCalcText) then
      FOnCalcText(Self, I, Text);
    Section := FHeaderControl.Sections[I];
    Inc(Rect.Left, 2);
    if I = 0 then
      DrawCaptionColumn
    else
      DrawTextColumn;
    Dec(Rect.Left, 2);
  end;

begin
  if FHeaderControl.Sections.Count = 0 then
    Exit;
  if DefaultDraw then
  begin
    Inc(Rect.Left, 2);
    Canvas := TDBDrawList(Control).Canvas;
    DrawImage;
    for I := 0 to FHeaderControl.Sections.Count - 1 do
      DrawColumn(I);
  end
  else if Assigned(FOnDrawItem) then
    FOnDrawItem(Control, Index, Rect, State);
end;

procedure TCustomDBReportView.DoSectionClick(HeaderControl: THeaderControl;
  Section: THeaderSection);
begin
  if Assigned(FOnColumnClick) then
    FOnColumnClick(Self, FColumns[Section.Index]);
end;

procedure TCustomDBReportView.DoSectionResize(HeaderControl: THeaderControl;
  Section: THeaderSection);
var
  MinWidth: Integer;
begin
  if Section.Index = 0 then
  begin
    MinWidth := 10;
    if FImages <> nil then
      Inc(MinWidth, FImages.Width);
    if Section.Width < MinWidth then
      Section.Width := MinWidth;
  end;
  FDrawList.Invalidate;
end;

procedure TCustomDBReportView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  FDrawList.DataLinkChanged;
end;

procedure TCustomDBReportView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if HandleAllocated then
      RecreateWnd;
  end;
end;

procedure TCustomDBReportView.SetColumns(Value: TReportViewColumns);
begin
  FColumns.Assign(Value);
  Invalidate;
end;

function TCustomDBReportView.GetDataSource: TDataSource;
begin
  Result := FDrawList.DataSource;
end;

procedure TCustomDBReportView.SetDataSource(Value: TDataSource);
begin
  FDrawList.DataSource := Value;
end;

procedure TCustomDBReportView.SetDefaultDraw(Value: Boolean);
begin
  if Value <> FDefaultDraw then
  begin
    FDefaultDraw := Value;
    Invalidate;
  end;
end;

function TCustomDBReportView.GetFlat: Boolean;
begin
  Result := FHeaderControl.Style = hsFlat;
end;

procedure TCustomDBReportView.SetFlat(Value: Boolean);
const
  Styles: array[Boolean] of THeaderStyle = (hsButtons, hsFlat);
begin
  FHeaderControl.Style := Styles[Value];
end;

procedure TCustomDBReportView.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if FImages <> nil then
      FDrawList.ItemHeight := FImages.Height + 1
    else
      FDrawList.ItemHeight := 18;
    Invalidate;
  end;
end;

function TCustomDBReportView.GetStyle: TReportViewStyle;
const
  Styles: array[Boolean] of TReportViewStyle = (rsGrid, rsColumn);
begin
  Result := Styles[FHeaderControl.Visible];
end;

procedure TCustomDBReportView.SetStyle(Value: TReportViewStyle);
begin
  FHeaderControl.Visible := Value = rsColumn;
end;

procedure TCustomDBReportView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 0;
end;

end.
