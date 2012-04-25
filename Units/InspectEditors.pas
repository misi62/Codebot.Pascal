
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit InspectEditors;

interface

{$I CODEBOT.INC}

uses
  Classes, Windows, Forms, Dialogs, Messages, StrTools, InspectCtrls, Controls,
  GraphTools, PopCtrls, BtnEdit,  MathTools, SysUtils, Graphics, ShlTools,
  ShlCtrls, CommCtrl, ClassTools, TypInfo, BaseTypes;

const
  ekString        = 0;
  ekInteger       = ekString + 1;
  ekFloat         = ekInteger + 1;
  ekDate          = ekFloat + 1;
  ekMoney         = ekDate + 1;
  ekPicklist      = ekMoney + 1;
  ekBoolean       = ekPicklist + 1;
  ekColorGrid     = ekBoolean + 1;
  ekBrushStyle    = ekColorGrid + 1;
  ekPenStyle      = ekBrushStyle + 1;
  ekFont          = ekPenStyle + 1;
  ekFolder        = ekFont + 1;

{ EEditorValidationError error }

type
  EEditorValidationError = class(Exception);

{ TStringInspectorEditor class }

  TStringInspectorEditor = class(TInspectorEditor)
  private
    FEditorValue: string;
    procedure SetEditorValue(const Value: string);
  protected
    function GetAttributes: TEditorAttributes; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property EditorValue: string read FEditorValue write SetEditorValue;
  end;

{ TIntegerInspectorEditor }

  TIntegerInspectorEditor = class(TInspectorEditor)
  private
    FEditorValue: Integer;
    procedure SetEditorValue(Value: Integer);
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property EditorValue: Integer read FEditorValue write SetEditorValue;
  end;

{ TFloatInspectorEditor }

  TFloatInspectorEditor = class(TInspectorEditor)
  private
    FEditorValue: Double;
    procedure SetEditorValue(const Value: Double);
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property EditorValue: Double read FEditorValue write SetEditorValue;
  end;

{ TFloatInspectorEditor }

  TDateInspectorEditor = class(TInspectorEditor)
  private
    FEditorValue: TDateTime;
    procedure SetEditorValue(const Value: TDateTime);
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property EditorValue: TDateTime read FEditorValue write SetEditorValue;
  end;

{ TMoneyInspectorEditor }

  TMoneyInspectorEditor = class(TFloatInspectorEditor)
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  end;

{ TPopupInspectorEditor }

  TPopupInspectorEditor = class(TInspectorEditor)
  private
    FPopup: TCustomPopupForm;
    procedure PopupSelect(Sender: TObject);
  protected
    procedure Click; override;
    procedure Select; virtual;
    procedure SetActive(Value: Boolean); override;
    function GetAttributes: TEditorAttributes; override;
    function GetPopupClass: TPopupFormClass; virtual; abstract;
    property Popup: TCustomPopupForm read FPopup;
    property PopupClass: TPopupFormClass read GetPopupClass;
  public
    constructor Create(AOwner: TInspectorEditors); override;
    destructor Destroy; override;
  end;

{ TPickInspectorEditor }

  TCustomPickInspectorEditor = class(TPopupInspectorEditor)
  private
    FNullable: Boolean;
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
    function GetEditorValue: string;
    procedure SetEditorValue(Value: string);
  protected
    function GetPopupClass: TPopupFormClass; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    property Items: TStrings read GetItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property Nullable: Boolean read FNullable write FNullable;
    property EditorValue: string read GetEditorValue write SetEditorValue;
  end;

{ TPickInspectorEditor }

  TPickInspectorEditor = class(TCustomPickInspectorEditor)
  public
    property Items;
    property ItemIndex;
    property Nullable;
    property EditorValue;
  end;

{ TBooleanInspectorEditor }

  {TBooleanInspectorEditor = class(TCustomPickInspectorEditor)
  private
    FEditorValue: Boolean;
    procedure SetEditorValue(Value: Boolean);
  public
    constructor Create(AOwner: TInspectorEditors); override;
    property EditorValue: Boolean read FEditorVaue write SetEditorValue;
  end;}

{ TColorGridInspectorEditor }

  TColorGridInspectorEditor = class(TPopupInspectorEditor)
  private
    function GetEditorValue: TColor;
    procedure SetEditorValue(Value: TColor);
  protected
    procedure DrawInline(Canvas: TCanvas; Rect: TRect); override;
    function GetAttributes: TEditorAttributes; override;
    function GetPopupClass: TPopupFormClass; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property EditorValue: TColor read GetEditorValue write SetEditorValue;
  end;

{ TBrushStyleInspectorEditor }

  TBrushStyleInspectorEditor = class(TPopupInspectorEditor)
  private
    function GetEditorValue: TBrushStyle;
    procedure SetEditorValue(Value: TBrushStyle);
    function GetListMode: TListMode;
    procedure SetListMode(const Value: TListMode);
  protected
    procedure DrawInline(Canvas: TCanvas; Rect: TRect); override;
    function GetAttributes: TEditorAttributes; override;
    function GetPopupClass: TPopupFormClass; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property EditorValue: TBrushStyle read GetEditorValue write SetEditorValue;
    property Mode: TListMode read GetListMode write SetListMode;
  end;

{ TPenStyleInspectorEditor }

  TPenStyleInspectorEditor = class(TPopupInspectorEditor)
  private
    function GetEditorValue: TPenStyle;
    procedure SetEditorValue(Value: TPenStyle);
    function GetListMode: TListMode;
    procedure SetListMode(const Value: TListMode);
  protected
    procedure DrawInline(Canvas: TCanvas; Rect: TRect); override;
    function GetAttributes: TEditorAttributes; override;
    function GetPopupClass: TPopupFormClass; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
  public
    property EditorValue: TPenStyle read GetEditorValue write SetEditorValue;
    property Mode: TListMode read GetListMode write SetListMode;
  end;

{ TFontInspectorEditor }

  TFontInspectorEditor  = class(TInspectorEditor)
  private
    FFont: TFont;
  protected
    procedure Click; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetAttributes: TEditorAttributes; override;
  public
    constructor Create(AOwner: TInspectorEditors); override;
    destructor Destroy; override;
  end;

  TFolderInspectorEditor = class(TPopupInspectorEditor)
  private
    function GetEditorValue: TShellNode;
    procedure SetEditorValue(Value: TShellNode);
  protected
    procedure Click; override;
    procedure DrawInline(Canvas: TCanvas; Rect: TRect); override;
    function GetAttributes: TEditorAttributes; override;
    function GetPopupClass: TPopupFormClass; override;
    function GetText: string; override;
    procedure SetText(const Value: string); override;
	public
    property EditorValue: TShellNode read GetEditorValue write SetEditorValue;
  end;

type
  TPropertyInspector = class(TInspectObject)
  private
    FInstance: TObject;
    FList: TList;
    FIndex: Integer;
    procedure SetInstance(const Value: TObject);
    function GetPropInfo: PPropInfo;
  protected
    procedure BuildEditors(Editors: TInspectorEditors); override;
    procedure Edit; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  	function GoForward: Boolean;
    function GoBack: Boolean;
    procedure Remove(Instance: TObject);
    procedure Reset;
		property Instance: TObject read FInstance write SetInstance;
    property PropInfo: PPropInfo read GetPropInfo;
  end;

implementation

uses
  StrConst;

const
  DefaultAlignment = DT_LEFT or DT_TOP or DT_SINGLELINE or DT_VCENTER;

{ TStringInspectorEditor }

function TStringInspectorEditor.GetAttributes: TEditorAttributes;
begin
	Result := inherited GetAttributes;
 //	if FShowButton then
		Result := Result + [eaButton, eaEllipseButton];
end;

procedure TStringInspectorEditor.SetEditorValue(const Value: string);
begin
  if Value <> FEditorValue then
  begin
    FEditorValue := Value;
    Change;
  end;
end;

function TStringInspectorEditor.GetText: string;
begin
  Result := FEditorValue;
end;

procedure TStringInspectorEditor.SetText(const Value: string);
begin
  EditorValue := Value;
end;

{ TIntegerInspectorEditor }

procedure TIntegerInspectorEditor.SetEditorValue(Value: Integer);
begin
  if Value <> FEditorValue then
  begin
    FEditorValue := Value;
    Change;
  end;
end;

function TIntegerInspectorEditor.GetText: string;
begin
  Result := IntToStr(FEditorValue);
end;

procedure TIntegerInspectorEditor.SetText(const Value: string);
begin
  EditorValue := StrToIntDef(Value, EditorValue);
end;

{ TFloatInspectorEditor }

procedure TFloatInspectorEditor.SetEditorValue(const Value: Double);
begin
  if Value <> FEditorValue then
  begin
    FEditorValue := Value;
    Change;
  end;
end;

function TFloatInspectorEditor.GetText: string;
begin
  Result := FloatToStr(FEditorValue);
end;

procedure TFloatInspectorEditor.SetText(const Value: string);
begin
	EditorValue := StrToFloatDef(Value, EditorValue);
end;

{ TDateInspectorEditor }

procedure TDateInspectorEditor.SetEditorValue(const Value: TDateTime);
begin
  if Value <> FEditorValue then
  begin
    FEditorValue := Value;
    Change;
  end;
end;

function TDateInspectorEditor.GetText: string;
begin
  Result := FormatDateTime('mm/dd/yyyy', FEditorValue);
end;

procedure TDateInspectorEditor.SetText(const Value: string);
begin
	EditorValue := StrToDateDef(Value, EditorValue);
end;

{ TMoneyInspectorEditor }

function TMoneyInspectorEditor.GetText: string;
begin
  Result := Format('%.2m', [FEditorValue]);
end;

procedure TMoneyInspectorEditor.SetText(const Value: string);
var
  S: string;
  P: PChar;
begin
  S := Value;
  if S = '' then
    inherited SetText(S)
  else
  begin
    P := PChar(S);
    while CharInSet(P^, [' ', '$']) do
      Inc(P);
    inherited SetText(StrPas(P));
  end;
end;

{ TPopupInspectorEditor }

constructor TPopupInspectorEditor.Create(AOwner: TInspectorEditors);
begin
  FPopup := PopupClass.Create(nil);;
  inherited Create(AOwner);
end;

destructor TPopupInspectorEditor.Destroy;
begin
  Active := False;
  FPopup.Free;
  inherited Destroy;
end;

procedure TPopupInspectorEditor.Click;
begin
  FPopup.Associate := InplaceEdit;
  FPopup.OnSelect := PopupSelect;
  FPopup.Popup;
end;

procedure TPopupInspectorEditor.PopupSelect(Sender: TObject);
begin
  Select;
end;

procedure TPopupInspectorEditor.Select;
begin
  Change;
  DoSubmit;
end;

procedure TPopupInspectorEditor.SetActive(Value: Boolean);
var
  WasActive: Boolean;
begin
  WasActive := Active;
  inherited SetActive(Value);
  if Value <> WasActive then
    if Value then
    begin
      FPopup.Associate := InplaceEdit;
      FPopup.OnSelect := PopupSelect;
    end
    else
    begin
      FPopup.Associate := nil;
      FPopup.OnSelect := nil;
    end;
end;

function TPopupInspectorEditor.GetAttributes: TEditorAttributes;
begin
  Result := inherited GetAttributes + [eaButton];
end;

{ TCustomPickInspectorEditor }

function TCustomPickInspectorEditor.GetPopupClass: TPopupFormClass;
begin
  Result := TPopupListForm;
end;

function TCustomPickInspectorEditor.GetItems: TStrings;
begin
  with (Popup as TPopupListForm) do
    Result := Items;
end;

procedure TCustomPickInspectorEditor.SetItems(Value: TStrings);
begin
  with (Popup as TPopupListForm) do
    Items := Value;
end;

function TCustomPickInspectorEditor.GetItemIndex: Integer;
begin
  with (Popup as TPopupListForm) do
    Result := ItemIndex;
end;

procedure TCustomPickInspectorEditor.SetItemIndex(Value: Integer);
begin
  with (Popup as TPopupListForm) do
    ItemIndex := Value;
end;

function TCustomPickInspectorEditor.GetEditorValue: string;
begin
  Result := '';
  with (Popup as TPopupListForm) do
    if ItemIndex > -1 then
      Result := Items[ItemIndex];
end;

procedure TCustomPickInspectorEditor.SetEditorValue(Value: string);
var
  I: Integer;
begin
  with (Popup as TPopupListForm) do
    if (Value = '') and FNullable then
      ItemIndex := -1
    else
    begin
      I := Items.IndexOf(Value);
      if I > -1 then
        ItemIndex := I;
    end;
end;

function TCustomPickInspectorEditor.GetText: string;
begin
  Result := EditorValue;
end;

procedure TCustomPickInspectorEditor.SetText(const Value: string);
begin
  SetEditorValue(Value);
end;

{ TColorGridInspectorEditor }

procedure TColorGridInspectorEditor.DrawInline(Canvas: TCanvas; Rect: TRect);
var
  DC: HDC;
  DrawRect: TRect;
  PriorPen: HPEN;
  PriorBrush: HBRUSH;
begin
  DC := Canvas.Handle;
  DrawRect := Rect;
  InflateRect(DrawRect, 0, -3);
  OffsetRect(DrawRect, 0, -1);
  PriorBrush := SelectObject(DC, CreateSolidBrush(ColorToRGB(EditorValue)));
  PriorPen := SelectObject(DC, GetStockObject(BLACK_PEN));
  with DrawRect do
  begin
    Inc(Bottom);
    Right := Left + Bottom - Top;
    Rectangle(DC, Left, Top, Right, Bottom);
  end;
  OverwriteObject(DC, PriorBrush);
  SelectObject(DC, PriorPen);
  Rect.Left := DrawRect.Right + 2;
  DrawText(DC, PChar(Text), -1, Rect, DefaultAlignment);
end;

function TColorGridInspectorEditor.GetAttributes: TEditorAttributes;
begin
  Result := inherited GetAttributes + [eaDrawInline];
end;

function TColorGridInspectorEditor.GetPopupClass: TPopupFormClass;
begin
  Result := TPopupColorGridForm;
end;

function TColorGridInspectorEditor.GetEditorValue: TColor;
begin
  Result := (Popup as TPopupColorGridForm).ActiveColor;
end;

procedure TColorGridInspectorEditor.SetEditorValue(Value: TColor);
begin
 (Popup as TPopupColorGridForm).ActiveColor := Value;
end;

function TColorGridInspectorEditor.GetText: string;
begin
  Result := ColorToStr(EditorValue);
end;

procedure TColorGridInspectorEditor.SetText(const Value: string);
begin
  EditorValue := StrToColorDef(Trim(Value), EditorValue);
end;

{ TBrushStyleInspectorEditor }

procedure TBrushStyleInspectorEditor.DrawInline(Canvas: TCanvas; Rect: TRect);
var
  DC: HDC;
  DrawRect: TRect;
  PriorPen: HPEN;
  PriorBrush: HBRUSH;
begin
  DC := Canvas.Handle;
  DrawRect := Rect;
  InflateRect(DrawRect, 0, -3);
  OffsetRect(DrawRect, 0, -1);
  PriorBrush := SelectObject(DC, (Popup as TPopupBrushForm).Brush.Handle);
  PriorPen := SelectObject(DC, GetStockObject(BLACK_PEN));
  with DrawRect do
  begin
    Inc(Bottom);
    Right := Left + Bottom - Top;
    Rectangle(DC, Left, Top, Right, Bottom);
  end;
  SelectObject(DC, PriorBrush);
  SelectObject(DC, PriorPen);
  Rect.Left := DrawRect.Right + 2;
  DrawText(DC, PChar(Text), -1, Rect, DefaultAlignment);
end;

function TBrushStyleInspectorEditor.GetAttributes: TEditorAttributes;
begin
  Result := inherited GetAttributes + [eaDrawInline];
end;

function TBrushStyleInspectorEditor.GetPopupClass: TPopupFormClass;
begin
  Result := TPopupBrushForm;
end;

function TBrushStyleInspectorEditor.GetEditorValue: TBrushStyle;
begin
  Result := (Popup as TPopupBrushForm).BrushStyle;
end;

procedure TBrushStyleInspectorEditor.SetEditorValue(Value: TBrushStyle);
begin
 (Popup as TPopupBrushForm).BrushStyle := Value;
end;

function TBrushStyleInspectorEditor.GetListMode: TListMode;
begin
  Result := (Popup as TPopupBrushForm).Mode;
end;

procedure TBrushStyleInspectorEditor.SetListMode(const Value: TListMode);
begin
 (Popup as TPopupBrushForm).Mode := Value;
end;

function TBrushStyleInspectorEditor.GetText: string;
begin
  Result := BrushStyleToStr(EditorValue);
end;

procedure TBrushStyleInspectorEditor.SetText(const Value: string);
begin
  EditorValue := StrToBrushStyleDef(Value, EditorValue);
end;

{ TPenStyleInspectorEditor }

procedure TPenStyleInspectorEditor.DrawInline(Canvas: TCanvas; Rect: TRect);
var
  DC: HDC;
  DrawRect: TRect;
  PriorPen: HPEN;
begin
  DC := Canvas.Handle;
  DrawRect := Rect;
  InflateRect(DrawRect, 0, -3);
  OffsetRect(DrawRect, 0, -1);
  PriorPen := SelectObject(DC, (Popup as TPopupPenForm).Pen.Handle);
  with DrawRect do
  begin
    Right := GetSystemMetrics(SM_CXSCREEN);
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2, nil);
    LineTo(DC, Right, Top + (Bottom - Top) div 2);
    MoveToEx(DC, Left, Top + (Bottom - Top) div 2 + 1, nil);
    LineTo(DC, Right, Top + (Bottom - Top) div 2 + 1);
  end;
  SelectObject(DC, PriorPen);
  Rect.Left := DrawRect.Right + 2;
end;

function TPenStyleInspectorEditor.GetAttributes: TEditorAttributes;
begin
  Result := inherited GetAttributes + [eaDrawInline];
end;

function TPenStyleInspectorEditor.GetPopupClass: TPopupFormClass;
begin
  Result := TPopupPenForm;
end;

function TPenStyleInspectorEditor.GetEditorValue: TPenStyle;
begin
  Result := (Popup as TPopupPenForm).PenStyle;
end;

procedure TPenStyleInspectorEditor.SetEditorValue(Value: TPenStyle);
begin
 (Popup as TPopupPenForm).PenStyle := Value;
end;

function TPenStyleInspectorEditor.GetListMode: TListMode;
begin
  Result := (Popup as TPopupPenForm).Mode;
end;

procedure TPenStyleInspectorEditor.SetListMode(const Value: TListMode);
begin
 (Popup as TPopupPenForm).Mode := Value;
end;

function TPenStyleInspectorEditor.GetText: string;
begin
  Result := PenStyleToStr(EditorValue);
end;

procedure TPenStyleInspectorEditor.SetText(const Value: string);
begin
  EditorValue := StrToPenStyleDef(Value, EditorValue);
end;

{ TFontInspectorEditor }

constructor TFontInspectorEditor.Create(AOwner: TInspectorEditors);
begin
  FFont := TFont.Create;
  inherited Create(AOwner);
end;

destructor TFontInspectorEditor.Destroy;
begin
  inherited Destroy;
  FFont.Free;
end;

procedure TFontInspectorEditor.Click;
begin
  with TFontDialog.Create(Application) do
  try
    Font := FFont;
    if Execute then
    begin
      FFont.Assign(Font);
      Change;
      DoSubmit;
    end;
  finally
    Free;
  end;
end;

function TFontInspectorEditor.GetAttributes: TEditorAttributes;
begin
  Result := inherited GetAttributes + [eaButton, eaEllipseButton];
end;

function TFontInspectorEditor.GetText: string;
begin
  Result := FFont.Name;
end;

procedure TFontInspectorEditor.SetText(const Value: string);
var
	S: string;
	I: Integer;
begin
	S := Trim(UpperCase(Value));
  for I := 0 to Screen.Fonts.Count - 1 do
  	if Trim(UpperCase(Screen.Fonts[I])) = S then
    begin
    	FFont.Name := Screen.Fonts[I];
      Break;
		end;
end;

{ TFolderInspectorEditor }

procedure TFolderInspectorEditor.Click;
var
	Tree: TShellTreePopupForm;
begin
  Tree := Popup as TShellTreePopupForm;
  Tree.Width := Tree.Associate.Width;
  if Tree.Width < 200 then
  	Tree.Width := 200;
  inherited Click;
end;

procedure TFolderInspectorEditor.DrawInline(Canvas: TCanvas; Rect: TRect);
const
 ImageHeight = 16;
var
  Node: TShellTreeNode;
  Images: HIMAGELIST;
  DC: HDC;
  R: TRect;
begin
  if EditorValue <> nil then
  begin
  	Node := EditorValue as TShellTreeNode;
		Images := GetSysImages;
		DC := Canvas.Handle;
    R := Rect;
    if HeightOf(R) > ImageHeight - 1 then
    begin
		  ImageList_Draw(Images, Node.ImageIndex, DC, R.Left,
      	R.Top + (HeightOf(R) - ImageHeight) div 2, ILD_TRANSPARENT);
	    Inc(R.Left, 2 + ImageHeight);
    end;
		DrawText(DC, PChar(Text), -1, R, DefaultAlignment);
	end
end;

function TFolderInspectorEditor.GetAttributes: TEditorAttributes;
begin
  Result := inherited GetAttributes + [eaDrawInline];
end;

function TFolderInspectorEditor.GetPopupClass: TPopupFormClass;
begin
  Result := TShellTreePopupForm;
end;

function TFolderInspectorEditor.GetText: string;
begin
	Result := NodeToStr(EditorValue);
end;

procedure TFolderInspectorEditor.SetText(const Value: string);
var
	Node: TShellNode;
begin
	Node := StrToNodeDef(Trim(Value), nil);
  if Node <> nil then
  try
  	EditorValue := Node;
  finally
  	Node.Free;
  end;
end;

function TFolderInspectorEditor.GetEditorValue: TShellNode;
begin
	Result := (Popup as TShellTreePopupForm).ShellTree.SelectedNode;
end;

procedure TFolderInspectorEditor.SetEditorValue(Value: TShellNode);
begin
	(Popup as TShellTreePopupForm).ShellTree.SelectedNode := Value;
end;

type
  TPropertyInspectorEditor = class(TInspectorEditor)
  private
    FEditorValue: PPropInfo;
    FValue: string;
  protected
    function GetAttributes: TEditorAttributes; override;
    function GetText: string; override;
  public
    constructor Create(AOwner: TInspectorEditors; Instance: TObject; PropInfo: PPropInfo); reintroduce;
    property EditorValue: PPropInfo read FEditorValue;
  end;

constructor TPropertyInspectorEditor.Create(AOwner: TInspectorEditors;
  Instance: TObject; PropInfo: PPropInfo);
var
	I: Integer;
begin
  Name := PropInfo.Name;
	FEditorValue := PropInfo;
  FValue := GetPropValue(Instance, Name, True);
  if PropInfo.PropType^^.Kind = tkClass then
  begin
  	I := GetOrdProp(Instance, PropInfo);
    if I <> 0 then
    	FValue := '$' + IntToHex(I, 8)
		else
    	FValue := 'nil';      
  end
  else if PropInfo.PropType^ = TypeInfo(TColor) then
  	FValue := ColorToStr(TColor(GetOrdProp(Instance, PropInfo)));
  inherited Create(AOwner);
end;

function TPropertyInspectorEditor.GetAttributes: TEditorAttributes;
begin
	Result := [eaReadOnly];
  if FEditorValue.PropType^^.Kind = tkClass then
		Result := Result + [eaButton, eaEllipseButton];
end;

function TPropertyInspectorEditor.GetText: string;
begin
	Result := FValue;
end;

{ TPropertyInspector }

constructor TPropertyInspector.Create;
begin
	inherited Create;
  FList := TList.Create;
  FIndex := -1;
end;

destructor TPropertyInspector.Destroy;
begin
	FList.Free;
  inherited Destroy;
end;

procedure TPropertyInspector.BuildEditors(Editors: TInspectorEditors);
var
	Strings: TClassPropertyStrings;
  I: Integer;
begin
  if FInstance = nil then Exit;
  Strings := TClassPropertyStrings.Create;
  try
  	Strings.TypeKinds := tkProperties;
  	Strings.Instance := FInstance;
		for I := 0 to Strings.Count - 1 do
    try
			TPropertyInspectorEditor.Create(Editors, FInstance, Strings.GetPropInfo(I));
		except
    	// delete property
    end;
  finally
  	Strings.Free;
  end;
end;

procedure TPropertyInspector.Edit;
var
	P: PPropInfo;
  O: TObject;
begin
	if FInstance = nil then Exit;
  P := PropInfo;
  if P = nil then Exit;
  O := TObject(GetOrdProp(FInstance, P));
  if O = nil then Exit;
  SetInstance(O);
  inherited Edit;
end;

function TPropertyInspector.GoForward: Boolean;
begin
	if FIndex < FList.Count - 1 then
	begin
  	Inc(FIndex);
  	FInstance := TObject(FList[FIndex]);
	  Recreate;
  end;
  Result := FIndex < FList.Count - 1;
end;

function TPropertyInspector.GoBack: Boolean;
begin
	if FIndex > 0 then
	begin
  	Dec(FIndex);
  	FInstance := TObject(FList[FIndex]);
	  Recreate;
  end;
  Result := FIndex > 0;
end;

procedure TPropertyInspector.Remove(Instance: TObject);
var
	I: Integer;
begin
	if Instance = FInstance then
  begin
		Reset;
    Exit;
	end;
	for I := FList.Count - 1 downto 0 do
  	if FList[I] = Instance then
    	FList.Delete(I);
	if FIndex > FList.Count - 1 then
  	FIndex := FList.Count - 1;
end;

procedure TPropertyInspector.Reset;
begin
	FList.Clear;
  FInstance := nil;
  FIndex := -1;
	Recreate;
end;

function TPropertyInspector.GetPropInfo: PPropInfo;
begin
	if FInstance = nil then
  	Result := nil
	else if Editor is TPropertyInspectorEditor then
  	Result := TPropertyInspectorEditor(Editor).EditorValue
	else
  	Result := nil;
end;

procedure TPropertyInspector.SetInstance(const Value: TObject);
var
	I: Integer;
begin
	if Value = nil then Exit;
	if FInstance <> Value then
  begin
	  if not ((FIndex < FList.Count - 1) and (FList[FIndex] = Value)) then
		begin
	    if FIndex < FList.Count - 1 then
  	  	for I := FList.Count - 1 downto FIndex + 1 do
    	  	FList.Delete(I);
			FList.Add(Value);
  	  FIndex := FList.Count - 1;
    end;
  	FInstance := Value;
	  Recreate;
  end;
end;

{ TPropertyInspectorEditor }

initialization
  RegisterEditorKind(ekString, TStringInspectorEditor, 'Text');
  RegisterEditorKind(ekInteger, TIntegerInspectorEditor, 'Integer');
  RegisterEditorKind(ekFloat, TFloatInspectorEditor, 'Number');
  RegisterEditorKind(ekDate, TDateInspectorEditor, 'Date');
  RegisterEditorKind(ekMoney, TMoneyInspectorEditor, 'Money');
  RegisterEditorKind(ekPicklist, TPickInspectorEditor, 'Pick');
  RegisterEditorKind(ekColorGrid, TColorGridInspectorEditor, 'Color Grid');
  RegisterEditorKind(ekBrushStyle, TBrushStyleInspectorEditor, 'Fill Pattern');
  RegisterEditorKind(ekPenStyle, TPenStyleInspectorEditor, 'Line Type');
  RegisterEditorKind(ekFont, TFontInspectorEditor, 'Font');
  RegisterEditorKind(ekFolder, TFolderInspectorEditor, 'Folder');
end.
