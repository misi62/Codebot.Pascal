
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.20 Open Source Released 2008                   *)
(*                                                      *)
(********************************************************)

unit ThemeCtrls;


interface

{$I CODEBOT.INC}

uses
  Windows, Messages, Classes, Controls, Graphics, Forms, SysUtils, StrTools,
  { These are the references to the GDI+ import units which are required }
  GdiPlus, GdiIntf, BaseTypes, GraphTools, ImgList, ProviderTools, FormTools,
  { These are the units with controls to be themed }
  BaseThemes, StdCtrls, CaptionBoxCtrls, FlowBox, ProgressCtrls;

type
  IDragControl = interface
    ['{E38DBAE2-CF85-4B84-84E8-6817E9E23FAC}']
    procedure AllowDrag(Control: TControl; Enable: Boolean);
  end;

  IMultiSelect = interface
    ['{76B787F3-C82E-459D-896D-B35C3BBDE6D9}']
    procedure Init;
    procedure MoveSource(AllItems: Boolean);
    procedure MoveDest(AllItems: Boolean);
  end;

  ISimpleSwitch = interface
    ['{F371FD88-8CB0-4C38-A442-5043E8377D39}']
    function GetSimpleText: string;
    procedure SetSimpleText(const Value: string);
    property SimplyText: string read GetSimpleText write SetSimpleText;
  end;

  TSortDirection = (sdAscending, sdDescending);

  ISortColumns = interface
    ['{45F86EE7-40BD-44B3-AA69-DE5C4FE8172B}']
    function GetColumn: Integer;
    function GetDirection: TSortDirection;
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    procedure Reference(Child: ISortColumns);
    procedure Sort(Column: Integer; Direction: TSortDirection);
    property Column: Integer read GetColumn;
    property Direction: TSortDirection read GetDirection;
    property Sorted: Boolean read GetSorted write SetSorted;
  end;

{ Standard control themes }

function ThemeListBox(ListBox: TListBox): IUnknown; overload;
function ThemeListBox(ListBox: TListBox; const ColumnWidths: array of Integer): IUnknown; overload;

function ThemeMultiSelect(Source, Dest: TListBox): IUnknown; overload;
function ThemeMultiSelect(Source, Dest: TListBox; const ColumnWidths: array of Integer): IUnknown; overload;

procedure ThemeRowBox(ListBox: TListBox; Rows: Integer);

procedure ThemeListBoxImages(ListBox: TListBox; Images: TCustomImageList;
  ItemHeight: Integer; DefaultIndex: Integer; BaseIndex: Integer = 0);

procedure ThemeComboBox(ComboBox: TComboBox); overload;
procedure ThemeComboBox(ComboBox: TComboBox; const ColumnWidths: array of Integer); overload;

procedure ThemeComboBoxImages(ComboBox: TComboBox; SmallImages, LargeImages: TCustomImageList;
  ItemHeight: Integer; BaseIndex: Integer = 0);

{ Caption control themes }

function ThemeHeaderBox(Header: THeaderSectionBox): IUnknown; overload;
function ThemeHeaderBox(Header: THeaderSectionBox; const ColumnWidths: array of Integer): IUnknown; overload;

{ Flow control themes }

procedure ThemeFlowBox(FlowBox: TFlowBox);

{ Flow control themes }

procedure ThemeProgress(Progress: TDrawProgress; Window: HWND);

implementation

procedure DrawDragGrip(DC: HDC; Rect: TRect; Focused: Boolean);
const
  GripSize = 6;
var
  P: HPEN;
  B: HBRUSH;
begin
  Rect.Bottom := Rect.Bottom - 2;
  BeginPath(DC);
  MoveTo(DC, Rect.Left, Rect.Bottom + 1);
  LineTo(DC, Rect.Left, Rect.Bottom + 1 +- GripSize);
  LineTo(DC, Rect.Left + GripSize, Rect.Bottom + 1);
  LineTo(DC, Rect.Left, Rect.Bottom + 1);
  EndPath(DC);
  P := SelectObject(DC, GetPen(clHighlight));
  if Focused then
    B := SelectObject(DC, GetBrush(clHighlight))
  else
    B := SelectObject(DC, GetStockObject(NULL_BRUSH));
  StrokeAndFillPath(DC);
  if Focused then
    OverwriteObject(DC, B)
  else
    SelectObject(DC, B);
  OverwriteObject(DC, P);
end;

procedure DrawSortArrow(DC: HDC; Rect: TRect; Direction: TSortDirection);
const
  ArrowSize = 6;
var
  P: HPEN;
  B: HBRUSH;
begin
  Rect.Bottom := Rect.Bottom - 2;
  BeginPath(DC);
  if Direction = sdAscending then
  begin
    MoveTo(DC, Rect.Right, Rect.Bottom - ArrowSize);
    LineTo(DC, Rect.Right, Rect.Bottom);
    LineTo(DC, Rect.Right - ArrowSize, Rect.Bottom);
    LineTo(DC, Rect.Right, Rect.Bottom - ArrowSize);
  end
  else
  begin
    MoveTo(DC, Rect.Right, Rect.Top);
    LineTo(DC, Rect.Right, Rect.Top + ArrowSize);
    LineTo(DC, Rect.Right - ArrowSize, Rect.Top);
    LineTo(DC, Rect.Right, Rect.Top);
  end;
  EndPath(DC);
  P := SelectObject(DC, GetPen(clWindowText));
  B := SelectObject(DC, GetBrush(clWindowText));
  StrokeAndFillPath(DC);
  OverwriteObject(DC, P);
  OverwriteObject(DC, B);
end;

{ Standard control themes }

type
  TListBoxTheme = class(TThemeComponent, IDragControl, ISortColumns)
  private
    FAllowDrag: Boolean;
    FDragIndex: Integer;
    FItemIndex: Integer;
    FWidths: array of Integer;
    FSortColumn: Integer;
    FSortDirection: TSortDirection;
    FSorted: Boolean;
    function ListBox: TListBox;
    procedure InternalSort;
    procedure ListFocusChange(Sender: TObject);
    procedure ListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  protected
    { IDragControl }
    procedure AllowDrag(Control: TControl; Enable: Boolean);
    { ISortColumns }
    function GetColumn: Integer;
    function GetDirection: TSortDirection;
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    procedure Reference(Child: ISortColumns);
    procedure Sort(Column: Integer; Direction: TSortDirection);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetWidths(const W: array of Integer);
  end;

constructor TListBoxTheme.Create(AOwner: TComponent);
var
  L: TListBox absolute AOwner;
begin
  inherited Create(AOwner);
  FDragIndex := -1;
  FItemIndex := -1;
  if AOwner is TListBox then
  begin
    L.DoubleBuffered := True;
    L.Style := lbOwnerDrawFixed;
    L.ItemHeight := 18;
    L.OnEnter := ListFocusChange;
    L.OnExit := ListFocusChange;
    L.OnDrawItem := DrawItem;
  end;
end;

procedure TListBoxTheme.SetWidths(const W: array of Integer);
var
  I: Integer;
begin
  SetLength(FWidths, Length(W));
  for I := Low(W) to High(W) do
    FWidths[I] := W[I];
end;

function TListBoxTheme.ListBox: TListBox;
begin
  if Owner is TListBox then
    Result := TListBox(Owner)
  else
    Result := nil;
end;

procedure TListBoxTheme.ListFocusChange(Sender: TObject);
var
  L: TListBox absolute Sender;
begin
  L.Invalidate;
end;

procedure TListBoxTheme.ListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  L: TListBox absolute Sender;
begin
  if (Button= mbLeft) and (X < 8) then
    FDragIndex := L.ItemAtPos(Point(4, Y), True);
end;

procedure TListBoxTheme.ListMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  L: TListBox absolute Sender;
  I: Integer;
begin
  if FDragIndex > -1 then
  begin
    I := L.ItemAtPos(Point(4, Y), True);
    if (I > -1) and (I <> FDragIndex) then
    begin
      L.Items.Move(FDragIndex, I);
      L.ItemIndex := I;
      FDragIndex := I;
    end;
  end;
end;

procedure TListBoxTheme.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  L: TListBox absolute Sender;
begin
  if Button = mbLeft then
    FDragIndex := -1;
end;

procedure TListBoxTheme.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  Border = 3;
var
  L: TListBox absolute Control;
  DC: HDC;
  R: TRect;
  P: HPEN;
  S: string;
  I, J, A, B: Integer;
begin
  R := Rect;
  if FItemIndex <> L.ItemIndex then
  begin
    if FItemIndex > -1 then
    begin
      SendMessage(L.Handle, LB_GETITEMRECT, FItemIndex, Integer(@R));
      InflateRect(R, 0, 3);
      InvalidateRect(L.Handle, @R, True);
    end;
    FItemIndex := L.ItemIndex;
  end;
  DC := L.Canvas.Handle;
  R := Rect;
  if odSelected in State then
    if L.Focused then
        DrawStyleRect(DC, R)
    else
    begin
      FillRectOutline(DC, R, Blend(clHighlight, clWindow, 66));
      InflateRect(R, -1, -1);
      FillRectColor(DC, R, L.Color);
    end
  else
    FillRectColor(DC, R, L.Color);
  R := Rect;
  SetTextColor(DC, ColorToRGB(clWindowText));
  if Length(FWidths) > 0 then
  begin
    A := R.Left + Border;
    P := SelectObject(DC, GetPen(Blend(cl3dDkShadow, clBtnFace, 33)));
    for I := Low(FWidths) to High(FWidths) do
    begin
      if FWidths[I] = 0 then Continue;
      B := A + FWidths[I] - Border * 2;
      J := B + Border * 2;
      R.Left := A;
      R.Right := B;
      if R.Right > Rect.Right - Border then
        R.Right := Rect.Right - Border ;
      if I = High(FWidths) then
        R.Right := Rect.Right - Border;
      S := FieldValueInt(L.Items[Index], '|', I);
      DrawCaption(DC, StringReplace(S, '&', '&&', [rfReplaceAll]), R, drLeft);
      if I = High(FWidths) then Break;
      MoveToEx(DC, J, R.Top, nil);
      LineTo(DC, J, R.Bottom);
      A := J + Border;
      if A > Rect.Right then Break;
    end;
    if Index = L.Count - 1 then
    begin
      MoveToEx(DC, Rect.Left, Rect.Bottom, nil);
      LineTo(DC, Rect.Right, Rect.Bottom);
    end;
    OverwriteObject(DC, P);
  end
  else
  begin
    InflateRect(R, -Border, 0);
    S := L.Items[Index];
    DrawCaption(DC, StringReplace(S, '&', '&&', [rfReplaceAll]), R, drLeft);
  end;
  if FAllowDrag and (Index = L.ItemIndex) then
    DrawDragGrip(DC, Rect, L.Focused);
  SelectClipRect(DC, Rect, RGN_DIFF);
end;

{ TListBoxTheme.IDragControl }

procedure TListBoxTheme.AllowDrag(Control: TControl; Enable: Boolean);

  procedure UpdateDrag(L: TListBox);
  begin
    if Enable then
    begin
      L.OnMouseDown := ListMouseDown;
      L.OnMouseMove := ListMouseMove;
      L.OnMouseUp := ListMouseUp;
    end
    else
    begin
      L.OnMouseDown := nil;
      L.OnMouseMove := nil;
      L.OnMouseUp := nil;
    end;
  end;

begin
  if (Control = Owner) and (Owner is TListBox) then
  begin
    UpdateDrag(TListBox(Owner));
    FAllowDrag := Enable;
  end;
end;

{ TListBoxTheme.ISortColumns }

function TListBoxTheme.GetColumn: Integer;
begin
  Result := FSortColumn;
end;

function TListBoxTheme.GetDirection: TSortDirection;
begin
  Result := FSortDirection;
end;

function TListBoxTheme.GetSorted: Boolean;
begin
  Result := FSorted;
end;

procedure TListBoxTheme.SetSorted(Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted := Value;
    if ListBox <> nil then
      ListBox.Invalidate;
  end;
end;

procedure TListBoxTheme.Reference(Child: ISortColumns);
begin
end;

var
  InternalListBoxTheme: TListBoxTheme;

function ListCompare(Item1, Item2: Pointer): Integer;
var
  L: TListBox;
  A, B, C, D: string;
  I, J: Integer;
begin
  L := TListBox(InternalListBoxTheme.Owner);
  A := UpperCase(L.Items[Integer(Item1)]);
  C := FieldValue(A, '|', InternalListBoxTheme.FSortColumn);
  B := UpperCase(L.Items[Integer(Item2)]);
  D := FieldValue(B, '|', InternalListBoxTheme.FSortColumn);
  I := StrToIntDef(C, -1);
  J := StrToIntDef(D, -1);
  Result := 0;
  if (I > -1) and (J > -1) then
    Result := I - J
  else if C > D then
    Result := 1
  else if D > C then
    Result := -1;
  if Result = 0 then
    if A > B then
      Result := 1
    else if B > A then
      Result := -1;
  if InternalListBoxTheme.FSortDirection = sdDescending then
    Result := Result * -1;
end;

procedure TListBoxTheme.InternalSort;
var
  Items: TStrings;
  L: TList;
  S: TStrings;
  I, J: Integer;
begin
  Items := ListBox.Items;
  L := TList.Create;
  try
    for I := 0 to Items.Count - 1 do
      L.Add(Pointer(I));
    InternalListBoxTheme := Self;
    L.Sort(ListCompare);
    S := TStringList.Create;
    try
      for I := 0 to L.Count - 1 do
      begin
        J := Integer(L[I]);
        S.AddObject(Items.Strings[J], Items.Objects[J]);
      end;
    finally
      Items.Assign(S);
    end;
  finally
    L.Free;
  end;
end;

procedure TListBoxTheme.Sort(Column: Integer; Direction: TSortDirection);
var
  L: TListBox;
  S: string;
  I: Integer;
begin
  if not FSorted then
  begin
    FSortColumn := Column;
    FSortDirection := Direction;
  end
  else if (Column <> FSortColumn) or (Direction <> FSortDirection) then
  begin
    FSortColumn := Column;
    FSortDirection := Direction;
    L := ListBox;
    S := '';
    if L <> nil then
    begin
      I := L.ItemIndex;
      if I > -1 then
        S := L.Items[I];
      L.Items.BeginUpdate;
      InternalSort;
      if S <> '' then
      begin
        I := L.Items.IndexOf(S);
        L.ItemIndex := I;
      end;
      L.Items.EndUpdate;
      L.Invalidate;
    end;
  end;
end;

{ Theme instantiation }

function ThemeListBox(ListBox: TListBox): IUnknown;
begin
  Result := TListBoxTheme.Create(ListBox);
end;

function ThemeListBox(ListBox: TListBox; const ColumnWidths: array of Integer): IUnknown;
var
  T: TListBoxTheme;
begin
  T := TListBoxTheme.Create(ListBox);
  T.SetWidths(ColumnWidths);
  Result := T
end;

type
  TSelectStates = array of Boolean;

  TMultiSelectTheme = class(TThemeComponent, IMultiSelect, IDragControl)
  private
    FItems: TStringList;
    FDragIndex: Integer;
    FDragControl: TListBox;
    FSource: TListBox;
    FSourceAllowDrag: Boolean;
    FSourceIndex: Integer;
    FSourceSelected: TSelectStates;
    FDest: TListBox;
    FDestAllowDrag: Boolean;
    FDestSelected: TSelectStates;
    FDestIndex: Integer;
    FWidths: array of Integer;
    procedure ListDblClick(Sender: TObject);
    procedure ListFocusChange(Sender: TObject);
    procedure DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MoveAll(A, B: TListBox);
    procedure MoveSome(A, B: TListBox);
    procedure ListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    { IMultiSelect }
    procedure Init;
    procedure MoveSource(AllItems: Boolean);
    procedure MoveDest(AllItems: Boolean);
    { IDragControl }
    procedure AllowDrag(Control: TControl; Enable: Boolean);
  public
    constructor CreateFromLists(Source, Dest: TListBox);
    destructor Destroy; override;
    procedure SetWidths(const W: array of Integer);
  end;

constructor TMultiSelectTheme.CreateFromLists(Source, Dest: TListBox);
begin
  inherited Create(Source);
  FDragIndex := -1;
  FItems := TStringList.Create;
  FItems.Duplicates := dupIgnore;
  FSource := Source;
  FDest := Dest;
  Init;
end;

destructor TMultiSelectTheme.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TMultiSelectTheme.SetWidths(const W: array of Integer);
var
  I: Integer;
begin
  SetLength(FWidths, Length(W));
  for I := Low(W) to High(W) do
    FWidths[I] := W[I];
end;

procedure MultiStyleSelect(DC: HDC; Rect: TRect; Above, Below, Focused, Control: Boolean);
var
  B: HBRUSH;
begin
  B := GetBrush(Blend(clHighlight, clWindow, 66));
  FillRect(DC, Rect, B);
  DeleteObject(B);
  if not Control then
    B := GetBrush(clWindow)
  else if Focused then
    B := GetBrush(Blend(clHighlight, clWindow, 33))
  else
    B := GetBrush(Blend(clHighlight, clWindow, 20));
  InflateRect(Rect, -1, -1);
  if Above then
    Rect.Top := Rect.Top - 2;
  if Below then
    Rect.Bottom := Rect.Bottom + 2;
  FillRect(DC, Rect, B);
  DeleteObject(B);
end;

procedure TMultiSelectTheme.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  Border = 3;
var
  L: TListBox absolute Control;
  S: TSelectStates;
  Above, Below: Boolean;
  DC: HDC;
  R: TRect;
  P: HPEN;
  I, J, A, B: Integer;
begin
  if L = FSource then
  begin
    S := Self.FSourceSelected;
    if FSourceIndex <> L.ItemIndex then
    begin
      if FSourceIndex > -1 then
      begin
        SendMessage(L.Handle, LB_GETITEMRECT, FSourceIndex, Integer(@R));
        InflateRect(R, 0, 3);
        InvalidateRect(L.Handle, @R, True);
      end;
      FSourceIndex := L.ItemIndex;
    end;
  end
  else
  begin
    S := Self.FDestSelected;
    if FDestIndex <> L.ItemIndex then
    begin
      if FDestIndex > -1 then
      begin
        SendMessage(L.Handle, LB_GETITEMRECT, FDestIndex, Integer(@R));
        InflateRect(R, 0, 3);
        InvalidateRect(L.Handle, @R, True);
      end;
      FDestIndex := L.ItemIndex;
    end;
  end;
  if S[Index] <> (odSelected in State) then
  begin
    S[Index] := odSelected in State;
    InvalidateRect(L.Handle, nil, False);
  end;
  Above := (Index > 0) and (S[Index - 1]);
  Below := (Index < L.Items.Count - 1) and (S[Index + 1]);
  DC := L.Canvas.Handle;
  R := Rect;
  if odSelected in State then
    MultiStyleSelect(DC, R, Above, Below, Index = L.ItemIndex, L.Focused)
  else
  begin
    FillRectColor(DC, R, clWindow);
    if odFocused in State then
      FillRectOutline(DC, R, Blend(clHighlight, clWindow, 33));
    if Above then
    begin
      Slide(R, drUp);
      DrawItem(Control, Index - 1, R, [odSelected]);
      Slide(R, drDown);
    end;
    if Below then
    begin
      Slide(R, drDown);
      DrawItem(Control, Index + 1, R, [odSelected]);
      Slide(R, drUp);
    end;
  end;
  R := Rect;
  SetTextColor(DC, ColorToRGB(clWindowText));
  if Length(FWidths) > 0 then
  begin
    A := R.Left + Border;
    P := SelectObject(DC, GetPen(Blend(cl3dDkShadow, clBtnFace, 33)));
    for I := Low(FWidths) to High(FWidths) do
    begin
      B := A + FWidths[I] - Border * 2;
      J := B + Border * 2;
      R.Left := A;
      R.Right := B;
      if R.Right > Rect.Right - Border then
        R.Right := Rect.Right - Border ;
      if I = High(FWidths) then
        R.Right := Rect.Right - Border;
      DrawCaption(DC, FieldValueInt(L.Items[Index], '|', I), R, drLeft);
      if I = High(FWidths) then Break;
      MoveToEx(DC, J, R.Top, nil);
      LineTo(DC, J, R.Bottom);
      A := J + Border;
      if A > Rect.Right then Break;
    end;
    if Index = L.Count - 1 then
    begin
      MoveToEx(DC, Rect.Left, Rect.Bottom, nil);
      LineTo(DC, Rect.Right, Rect.Bottom);
    end;
    OverwriteObject(DC, P);
  end
  else
  begin
    InflateRect(R, -Border, 0);
    DrawCaption(DC, L.Items[Index], R, drLeft);
  end;
  if FSourceAllowDrag and (L = FSource) and (Index = L.ItemIndex) then
    DrawDragGrip(DC, Rect, L.Focused)
  else if FDestAllowDrag and (L = FDest) and (Index = L.ItemIndex) then
    DrawDragGrip(DC, Rect, L.Focused);
  SelectClipRect(DC, Rect, RGN_DIFF);
end;

procedure TMultiSelectTheme.ListDblClick(Sender: TObject);
var
  L: TListBox absolute Sender;
begin
  if L = FSource then
    MoveSource(False)
  else
    MoveDest(False);
end;

procedure TMultiSelectTheme.ListFocusChange(Sender: TObject);
var
  L: TListBox absolute Sender;
begin
  L.Invalidate;
end;

procedure TMultiSelectTheme.ListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  L: TListBox absolute Sender;
begin
  if (Button= mbLeft) and (X < 8) then
  begin
    FDragIndex := L.ItemAtPos(Point(4, Y), True);
    if FDragIndex > -1 then
    begin
      L.MultiSelect := False;
      FDragControl := L;
    end;
  end;
end;

procedure TMultiSelectTheme.ListMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  L: TListBox absolute Sender;
  I: Integer;
begin
  if Sender <> FDragControl then Exit;
  if FDragIndex > -1 then
  begin
    I := L.ItemAtPos(Point(4, Y), True);
    if (I > -1) and (I <> FDragIndex) then
    begin
      if L = FSource then
        FSourceSelected[FDragIndex] := False
      else
        FDestSelected[FDragIndex] := False;
      L.Items.Move(FDragIndex, I);
      L.ItemIndex := I;
      FDragIndex := I;
      if L = FSource then
      begin
        FSourceSelected[FDragIndex] := True;
        FSourceIndex := I;
      end
      else
      begin
        FDestSelected[FDragIndex] := True;
        FDestIndex := I;
      end;
    end;
  end;
end;

procedure TMultiSelectTheme.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  L: TListBox absolute Sender;
begin
  if Sender <> FDragControl then Exit;
  if Button = mbLeft then
  begin
    L.MultiSelect := True;
    if FDragIndex > -1 then
    begin
      if L = FSource then
        FSourceSelected[FDragIndex] := True
      else
        FDestSelected[FDragIndex] := True;
      L.Selected[FDragIndex] := True;
    end;
    FDragIndex := -1;
  end;
end;

{ TMultiSelectTheme.IMultiSelect }

procedure TMultiSelectTheme.Init;

  procedure InternalInit(L: TListBox);
  var
    S: TSelectStates;
    I: Integer;
  begin
    L.DoubleBuffered := True;
    L.Style := lbOwnerDrawFixed;
    L.ItemHeight := 18;
    L.MultiSelect := True;
    L.OnDrawItem := DrawItem;
    L.OnEnter := ListFocusChange;
    L.OnExit := ListFocusChange;
    L.OnDblClick := ListDblClick;
    FItems.Assign(L.Items);
    L.Items := FItems;
    SetLength(S, FItems.Count);
    for I := 0 to FItems.Count - 1 do
      S[I] := False;
    if L = FSource then
      FSourceSelected := S
    else
      FDestSelected := S;
  end;

begin
  FSourceIndex := -1;
  FDestIndex := -1;
  InternalInit(FSource);
  InternalInit(FDest);
  FItems.Clear;
  FItems.AddStrings(FSource.Items);
  FItems.AddStrings(FDest.Items);
end;

procedure TMultiSelectTheme.MoveAll(A, B: TListBox);
var
  S: TSelectStates;
  I: Integer;
begin
  if A.Items.Count = 0 then Exit;
  B.Items.BeginUpdate;
  try
    if A.Items.Count > 0 then
    begin
      A.Clear;
      B.Items.Assign(FItems);
    end;
    SetLength(S, FItems.Count);
    for I := Low(S) to High(S) do
    begin
      S[I] := True;
      B.Selected[I] := True;
    end;
    if B = FSource then
    begin
      FDestSelected := nil;
      FSourceSelected := S;
    end
    else
    begin
      FSourceSelected := nil;
      FDestSelected := S;
    end;
  finally
    B.Items.EndUpdate;
  end;
  if FItems.Count > 0 then
  begin
    PostMessage(B.Handle, WM_VSCROLL, SB_TOP, 0);
    B.ItemIndex := 0;
    B.SetFocus;
  end;
  A.Invalidate;
  B.Invalidate;
end;

procedure TMultiSelectTheme.MoveSome(A, B: TListBox);
var
  StringsA, StringsB, StringsC: TStringList;
  SA, SB: TSelectStates;
  I, J: Integer;
begin
  J := 0;
  for I := 0 to A.Count - 1 do
    if A.Selected[I] then
      Inc(J);
  if J = 0 then
  begin
    B.SetFocus;
    Exit;
  end;
  StringsA := TStringList.Create;
  //StringsA.Sorted := True;
  StringsA.Duplicates := dupIgnore;
  StringsB := TStringList.Create;
  //StringsB.Sorted := True;
  StringsB.Duplicates := dupIgnore;
  StringsC := TStringList.Create;
  //StringsC.Sorted := True;
  StringsC.Duplicates := dupIgnore;
  try
    { remove items from a }
    A.Items.BeginUpdate;
    try
      for I := A.Count - 1 downto 0 do
        if A.Selected[I] then
        begin
          StringsA.AddObject(A.Items[I], A.Items.Objects[I]);
          A.Items.Delete(I);
        end;
      SetLength(SA, A.Items.Count);
      for I := Low(SA) to High(SA) do
        SA[I] := False;
      if A = FSource then
        FSourceSelected := SA
      else
        FDestSelected := SA;
    finally
      A.Items.EndUpdate;
    end;
    if A.Items.Count > 0 then
    begin
      PostMessage(A.Handle, WM_VSCROLL, SB_TOP, 0);
      A.ItemIndex := 0;
    end;
    { copy items to strings c }
    StringsC.AddStrings(StringsA);
    StringsC.AddStrings(B.Items);
    { add items to b }
    B.Items.BeginUpdate;
    try
      for I := 0 to B.Count - 1 do
        if B.Selected[I] then
          StringsB.AddObject(B.Items[I], B.Items.Objects[I]);
      B.Items.Assign(StringsC);
      SetLength(SB, StringsC.Count);
      for I := Low(SB) to High(SB) do
      begin
        SB[I] := (StringsA.IndexOf(StringsC[I]) > -1) or (StringsB.IndexOf(StringsC[I]) > -1);
        B.Selected[I] := SB[I];
      end;
      if B = FSource then
        FSourceSelected := SB
      else
        FDestSelected := SB;
    finally
      B.Items.EndUpdate;
    end;
    if B.Items.Count > 0 then
    begin
      PostMessage(B.Handle, WM_VSCROLL, SB_TOP, 0);
      B.ItemIndex := 0;
    end;
    B.SetFocus;
  finally
    StringsC.Free;
    StringsB.Free;
    StringsA.Free;
  end;
  A.Invalidate;
  B.Invalidate;
end;

procedure TMultiSelectTheme.MoveSource(AllItems: Boolean);
begin
  if AllItems then
    MoveAll(FSource, FDest)
  else
    MoveSome(FSource, FDest);
end;

procedure TMultiSelectTheme.MoveDest(AllItems: Boolean);
begin
  if AllItems then
    MoveAll(FDest, FSource)
  else
    MoveSome(FDest, FSource);
end;

procedure TMultiSelectTheme.AllowDrag(Control: TControl; Enable: Boolean);

  procedure UpdateDrag(L: TListBox);
  begin
    if Enable then
    begin
      L.OnMouseDown := ListMouseDown;
      L.OnMouseMove := ListMouseMove;
      L.OnMouseUp := ListMouseUp;
    end
    else
    begin
      L.OnMouseDown := nil;
      L.OnMouseMove := nil;
      L.OnMouseUp := nil;
    end;
  end;

begin
  if Control = FSource then
  begin
    UpdateDrag(FSource);
    FSourceAllowDrag := Enable;
  end
  else if Control = FDest then
  begin
    UpdateDrag(FDest);
    FDestAllowDrag := Enable;
  end;
end;

{ Theme instantiation }

function ThemeMultiSelect(Source, Dest: TListBox): IUnknown;
var
  M: TMultiSelectTheme;
begin
  M := TMultiSelectTheme.CreateFromLists(Source, Dest);
  Result := M;
end;

function ThemeMultiSelect(Source, Dest: TListBox; const ColumnWidths: array of Integer): IUnknown;
var
  M: TMultiSelectTheme;
begin
  M := TMultiSelectTheme.CreateFromLists(Source, Dest);
  M.SetWidths(ColumnWidths);
  Result := M;
end;

type
  TListRowsTheme = class(TThemeComponent)
  private
    FRows: Integer;
    procedure DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  public
    constructor CreateRows(AOwner: TComponent; Rows: Integer);
  end;

constructor TListRowsTheme.CreateRows(AOwner: TComponent; Rows: Integer);
const
  RowHeight = 18;
var
  L: TListBox absolute AOwner;
begin
  inherited Create(AOwner);
  if Rows < 0 then Rows := 1;
  FRows := Rows;
  if AOwner is TListBox then
  begin
    L.Style := lbOwnerDrawFixed;
    L.ItemHeight := RowHeight * FRows;
    L.OnDrawItem := DrawItem;
  end;
end;

procedure TListRowsTheme.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  Border = 3;
  RowHeight = 18;
var
  L: TListBox absolute Control;
  DC: HDC;
  R: TRect;
  F: HFONT;
  S: string;
  I: Integer;
begin
  DC := L.Canvas.Handle;
  R := Rect;
  if odSelected in State then
    if L.Focused then
      DrawStyleRect(DC, R)
    else
    begin
      FillRectOutline(DC, R, Blend(clHighlight, clWindow, 66));
      InflateRect(R, -1, -1);
      FillRectColor(DC, R, L.Color);
    end
  else
    FillRectColor(DC, R, L.Color);
  R := Rect;
  SetTextColor(DC, ColorToRGB(clWindowText));
  InflateRect(R, -Border, 0);
  R.Bottom := R.Top + RowHeight;
  S := L.Items[Index];
  for I := 0 to FRows - 1 do
  begin
    if I = 0 then
    begin
      F := GetFont(DC, [fsBold]);
      F := SelectObject(DC, F);
      DrawCaption(DC, FieldValueInt(S, I), R, drLeft);
      OverwriteObject(DC, F);
      Inc(R.Left, 10);
      Inc(R.Top, 2);
    end
    else
      DrawCaption(DC, FieldValueInt(S, I), R, drLeft);
    Slide(R, drDown);
  end;
  SelectClipRect(DC, Rect, RGN_DIFF);
end;

procedure ThemeRowBox(ListBox: TListBox; Rows: Integer);
begin
  TListRowsTheme.CreateRows(ListBox, Rows);
end;

type
  TComboBoxTheme = class(TThemeComponent)
  private
    FWidths: array of Integer;
    procedure DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetWidths(const W: array of Integer);
  end;

constructor TComboBoxTheme.Create(AOwner: TComponent);
var
  L: TComboBox absolute AOwner;
begin
  inherited Create(AOwner);
  if AOwner is TComboBox then
  begin
    L.Style := csOwnerDrawFixed;
    L.ItemHeight := CalcTextBoxHeight - 6;
    L.OnDrawItem := DrawItem;
  end;
end;

procedure TComboBoxTheme.SetWidths(const W: array of Integer);
var
  I: Integer;
begin
  SetLength(FWidths, Length(W));
  for I := Low(W) to High(W) do
    FWidths[I] := W[I];
end;

procedure TComboBoxTheme.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  Border = 3;
var
  L: TComboBox absolute Control;
  DC: HDC;
  R: TRect;
  P: HPEN;
  I, J, A, B: Integer;
begin
  DC := L.Canvas.Handle;
  R := Rect;
  if odSelected in State then
    if odFocused in State then
      DrawStyleRect(DC, R)
    else
    begin
      FillRectOutline(DC, R, Blend(clHighlight, clWindow, 66));
      InflateRect(R, -1, -1);
      FillRectColor(DC, R, L.Color);
    end
  else
    FillRectColor(DC, R, L.Color);
  R := Rect;
  SetTextColor(DC, ColorToRGB(clWindowText));
  if Length(FWidths) > 0 then
  begin
    A := R.Left + Border;
    P := SelectObject(DC, GetPen(Blend(cl3dDkShadow, clBtnFace, 33)));
    for I := Low(FWidths) to High(FWidths) do
    begin
      if FWidths[I] < 1 then Continue;
      B := A + FWidths[I] - Border * 2;
      J := B + Border * 2;
      R.Left := A;
      R.Right := B;
      if R.Right > Rect.Right - Border then
        R.Right := Rect.Right - Border ;
      if I = High(FWidths) then
        R.Right := Rect.Right - Border;
      DrawCaption(DC, FieldValueInt(L.Items[Index], '|', I), R, drLeft);
      if I = High(FWidths) then Break;
      MoveToEx(DC, J, R.Top, nil);
      LineTo(DC, J, R.Bottom);
      A := J + Border;
      if A > Rect.Right then Break;
    end;
    if Index = L.Items.Count - 1 then
    begin
      MoveToEx(DC, Rect.Left, Rect.Bottom, nil);
      LineTo(DC, Rect.Right, Rect.Bottom);
    end;
    OverwriteObject(DC, P);
  end
  else
  begin
    InflateRect(R, -Border, 0);
    DrawCaption(DC, L.Items[Index], R, drLeft);
  end;
  SelectClipRect(DC, Rect, RGN_DIFF);
end;

procedure ThemeComboBox(ComboBox: TComboBox);
begin
  TComboBoxTheme.Create(ComboBox);
end;

procedure ThemeComboBox(ComboBox: TComboBox; const ColumnWidths: array of Integer);
var
  T: TComboBoxTheme;
begin
  T := TComboBoxTheme.Create(ComboBox);
  T.SetWidths(ColumnWidths);
end;


type
  TListBoxImagesTheme = class(TThemeComponent)
  private
    FItemHeight: Integer;
    FImages: TCustomImageList;
    FDefaultIndex: Integer;
    FBaseIndex: Integer;
    procedure ListFocusChange(Sender: TObject);
    procedure DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SetItemHeight(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property Images: TCustomImageList read FImages write FImages;
    property DefaultIndex: Integer read FDefaultIndex write FDefaultIndex;
    property BaseIndex: Integer read FBaseIndex write FBaseIndex;
  end;

constructor TListBoxImagesTheme.Create(AOwner: TComponent);
var
  L: TListBox absolute AOwner;
begin
  inherited Create(AOwner);
  if AOwner is TListBox then
  begin
    L.Style := lbOwnerDrawFixed;
    L.OnEnter := ListFocusChange;
    L.OnExit := ListFocusChange;
    L.OnDrawItem := DrawItem;
  end;
end;

procedure TListBoxImagesTheme.ListFocusChange(Sender: TObject);
var
  L: TListBox absolute Sender;
begin
  L.Invalidate;
end;

procedure TListBoxImagesTheme.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  Border = 3;
var
  L: TListBox absolute Control;
  DC: HDC;
  R: TRect;
  F: HFONT;
  S: string;
  I: Integer;
begin
  DC := L.Canvas.Handle;
  R := Rect;
  FillRectColor(DC, R, L.Color);
  if FImages <> nil then
  begin
    R.Right := Images.Width + 10;
    FillRectColor(DC, R, clBtnShadow);
    R.Left := R.Right;
    Inc(R.Right);
    FillRectColor(DC, R, cl3DDkShadow);
    Inc(R.Left, 3);
    R.Right := Rect.Right;
  end;
  S := L.Items[Index];
  I := Pos('|', S);
  if L.Focused then
    Include(State, odFocused);
    if odSelected in State then
      if odFocused in State then
        DrawStyleRect(DC, R)
      else
      begin
        FillRectOutline(DC, R, Blend(clHighlight, clWindow, 66));
        InflateRect(R, -1, -1);
        FillRectColor(DC, R, L.Color);
        InflateRect(R, 1, 1);
      end;
  R := Rect;
  InflateRect(R, -2, 0);
    if FImages <> nil then
    begin
      ImageListDraw(FImages, L.Canvas, R.Left + 3,
        R.Top + (HeightOf(R) - FImages.Height) div 2, FBaseIndex + Index, L.Enabled);
      R.Left := R.Left + FImages.Height + 12;
    end;
    SetTextColor(DC, ColorToRGB(clWindowText));
    InflateRect(R, -Border, 0);
    if I > 0 then
    begin
      F := SelectFontStyle(DC, [fsBold]);
      DrawCaption(DC, FieldValue(S, 0), R, drLeft);
      if Index <> FDefaultIndex then
        OverwriteObject(DC, F);
      DrawCaption(DC, FieldValue(S, 1), R, drRight);
      if Index = FDefaultIndex then
        OverwriteObject(DC, F);
    end
    else
      DrawCaption(DC, S, R, drLeft);
  SelectClipRect(DC, Rect, RGN_DIFF);
end;

procedure TListBoxImagesTheme.SetItemHeight(Value: Integer);
begin
  if Owner is TListBox then
    TListBox(Owner).ItemHeight := Value;
  FItemHeight := Value;
end;

procedure ThemeListBoxImages(ListBox: TListBox; Images: TCustomImageList;
  ItemHeight: Integer; DefaultIndex: Integer; BaseIndex: Integer = 0);
var
  L: TListBoxImagesTheme;
begin
  L := TListBoxImagesTheme.Create(ListBox);
  L.Images := Images;
  L.ItemHeight := ItemHeight;
  L.BaseIndex := BaseIndex;
  L.DefaultIndex := DefaultIndex;
end;


type
  TComboBoxImagesTheme = class(TThemeComponent)
  private
    FSmallImages: TCustomImageList;
    FItemHeight: Integer;
    FLargeImages: TCustomImageList;
    FBaseIndex: Integer;
    procedure DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MeasureItem(Control: TWinControl;
      Index: Integer; var Height: Integer);
    procedure SetLargeImages(Value: TCustomImageList);
  public
    constructor Create(AOwner: TComponent); override;
    property ItemHeight: Integer read FItemHeight write FItemHeight;
    property SmallImages: TCustomImageList read FSmallImages write FSmallImages;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property BaseIndex: Integer read FbaseIndex write FBaseIndex;
  end;

constructor TComboBoxImagesTheme.Create(AOwner: TComponent);
var
  C: TComboBox absolute AOwner;
begin
  inherited Create(AOwner);
  if AOwner is TComboBox then
  begin
    C.OnDrawItem := DrawItem;
    C.OnMeasureItem := MeasureItem;
    C.ItemHeight := CalcTextBoxHeight - 6;
    C.Style := csOwnerDrawVariable;
  end;
end;

procedure TComboBoxImagesTheme.MeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  if (Index < 0) or (FLargeImages = nil) then
    Height := CalcTextBoxHeight - 6
  else
    Height := FItemHeight;
end;

procedure TComboBoxImagesTheme.DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  Border = 3;
var
  L: TComboBox absolute Control;
  DC: HDC;
  R: TRect;
  F: HFONT;
  S: string;
  I, J: Integer;
begin
  DC := L.Canvas.Handle;
  R := Rect;
  FillRectColor(DC, R, L.Color);
  S := L.Items[Index];
  I := Pos('|', S);
  if odComboBoxEdit in State then
  begin
    if FSmallImages <> nil then
    begin
      J := (HeightOf(R) - 4) - FSmallImages.Height div 2;
      ImageListDraw(FSmallImages, L.Canvas, 3, J, FBaseIndex + Index, L.Enabled);
      Inc(R.Left, 20);
    end;
    if odSelected in State then
      if odFocused in State then
        DrawStyleRect(DC, R)
      else
      begin
        FillRectOutline(DC, R, Blend(clHighlight, clWindow, 66));
        InflateRect(R, -1, -1);
        FillRectColor(DC, R, L.Color);
      end;
    R := Rect;
    if FSmallImages <> nil then
      Inc(R.Left, 20);
    SetTextColor(DC, ColorToRGB(clWindowText));
    InflateRect(R, -Border, 0);
    if I > 0 then
      S := FieldValue(S, 1);
    DrawCaption(DC, S, R, drLeft);
  end
  else
  begin
    if odSelected in State then
      if odFocused in State then
        DrawStyleRect(DC, R)
      else
      begin
        FillRectOutline(DC, R, Blend(clHighlight, clWindow, 66));
        InflateRect(R, -1, -1);
        FillRectColor(DC, R, L.Color);
        InflateRect(R, 1, 1);
      end;
    if FLargeImages <> nil then
    begin
      ImageListDraw(FLargeImages, L.Canvas, R.Left + 3,
        R.Top + (HeightOf(R) - FLargeImages.Height) div 2, FBaseIndex + Index, L.Enabled);
      R.Left := R.Left + FLargeImages.Height + 6;
    end;
    SetTextColor(DC, ColorToRGB(clWindowText));
    InflateRect(R, -Border, 0);
    if I > 0 then
    begin
      R.Bottom := R.Top + HeightOf(R) div 2;
      OffsetRect(R, 0, 1);
      F := SelectFontStyle(DC, [fsBold]);
      DrawCaption(DC, FieldValue(S, 0), R, drLeft);
      OverwriteObject(DC, F);
      Slide(R, drDown, -4);
      DrawCaption(DC, FieldValue(S, 1), R, drLeft);
    end
    else
      DrawCaption(DC, S, R, drLeft);
  end;
  SelectClipRect(DC, Rect, RGN_DIFF);
end;

procedure TComboBoxImagesTheme.SetLargeImages(Value: TCustomImageList);
var
  L: TComboBox;
  S: TStrings;
  I: Integer;
begin
  FLargeImages := Value;
  if (Owner is TComboBox) and (FLargeImages <> nil) then
  begin
    L := Owner as TComboBox;
    I := L.ItemIndex;
    S := TStringList.Create;
    try
      S.Assign(L.Items);
      L.Items.Clear;
      L.ItemHeight := FItemHeight;
      L.Items.Assign(S);
    finally
      S.Free;
    end;
    L.ItemIndex := I;
  end;
end;

procedure ThemeComboBoxImages(ComboBox: TComboBox; SmallImages, LargeImages: TCustomImageList;
  ItemHeight: Integer; BaseIndex: Integer = 0);
var
  C: TComboBoxImagesTheme;
begin
  C := TComboBoxImagesTheme.Create(ComboBox);
  C.ItemHeight := ItemHeight;
  C.SmallImages := SmallImages;
  C.LargeImages := LargeImages;
  C.BaseIndex := BaseIndex;
end;

{ Caption control themes }

type
  THeaderBoxTheme = class(TComponent, ISimpleSwitch, ISortColumns)
  private
    FWidths: array of Integer;
    FChild: ISortColumns;
    FSimpleText: string;
    FSortPressColumn: Integer;
    FSortColumn: Integer;
    FSortDirection: TSortDirection;
    FSorted: Boolean;
    function Header: THeaderSectionBox;
    function GetHeaderRects: TRects;
    procedure HeaderMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HeaderMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DrawHeader(Sender: TObject; Canvas: TCanvas; Rect: TRect);
  protected
    { ISimpleSwitch }
    function GetSimpleText: string;
    procedure SetSimpleText(const Value: string);
    { ISortColumns }
    function GetColumn: Integer;
    function GetDirection: TSortDirection;
    function GetSorted: Boolean;
    procedure SetSorted(Value: Boolean);
    procedure Reference(Child: ISortColumns);
    procedure Sort(Column: Integer; Direction: TSortDirection);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetWidths(const W: array of Integer);
  end;

constructor THeaderBoxTheme.Create(AOwner: TComponent);
var
  H: THeaderSectionBox absolute AOwner;
begin
  inherited Create(AOwner);
  FSortPressColumn := -1;
  if AOwner is THeaderSectionBox then
  begin
    H.HeaderHeight := 20;
    H.OnDrawHeader := DrawHeader;
    H.OnMouseDown := HeaderMouseDown;
    H.OnMouseUp := HeaderMouseUp;
  end;
end;

function THeaderBoxTheme.Header: THeaderSectionBox;
begin
  if Owner is THeaderSectionBox then
    Result := THeaderSectionBox(Owner)
  else
    Result := nil;
end;

procedure THeaderBoxTheme.SetWidths(const W: array of Integer);
var
  I: Integer;
begin
  SetLength(FWidths, Length(W));
  for I := Low(W) to High(W) do
    FWidths[I] := W[I];
end;

function THeaderBoxTheme.GetHeaderRects: TRects;
const
  Border = 4;
var
  H: THeaderSectionBox;
  R: TRect;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), #0);
  Result := nil;
  H := Header;
  if H = nil then Exit;
  R := H.ClientRect;
  R.Bottom := H.HeaderHeight;
  SetLength(Result, High(FWidths) + 1);
  Result[0] := R;
  if Length(FWidths) > 0 then
    Result[0].Right := FWidths[0] + Border;
  for I := Low(FWidths) + 1 to High(FWidths) do
  begin
    R := Result[I - 1];
    R.Left := R.Right;
    if I = High(FWidths) then
      R.Right := H.ClientRect.Right
    else
      R.Right := R.Left + FWidths[I] + Border;
    Result[I] := R;
  end;
end;

procedure THeaderBoxTheme.HeaderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRects;
  P: TPoint;
  I: Integer;
begin
  R := nil;
  if (Button = mbLeft) and (FSimpleText = '') then
  begin
    R := GetHeaderRects;
    P := Point(X, Y);
    for I := Low(R) to High(R) do
      if PtInRect(R[I], P) then
      begin
        FSortPressColumn := I;
        Break;
      end;
  end;
end;

procedure THeaderBoxTheme.HeaderMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  R: TRects;
  P: TPoint;
  I, J, K: Integer;
begin
  R := nil;
  if ((Button = mbLeft) and (FSortPressColumn > -1)) and (FSimpleText = '') then
  begin
    R := GetHeaderRects;
    P := Point(X, Y);
    J := -1;
    K := FSortPressColumn;
    FSortPressColumn := -1;
    for I := Low(R) to High(R) do
      if PtInRect(R[I], P) then
      begin
        J := I;
        Break;
      end;
    if J <> K then Exit;
    if J = FSortColumn then
      if FSortDirection = sdAscending then
        Sort(J, sdDescending)
      else
        Sort(J, sdAscending)
    else
      Sort(J, sdAscending);
  end;
end;

procedure THeaderBoxTheme.DrawHeader(Sender: TObject; Canvas: TCanvas; Rect: TRect);
const
  Border = 4;
var
  H: THeaderSectionBox absolute Sender;
  DC: HDC;
  R: TRect;
  P: HPEN;
  S: string;
  I, J, A, B: Integer;
begin
  DC := H.Canvas.Handle;
  R := Rect;
  if H.Active then
    DrawGradient(DC, R, clBtnFace, Blend(clWindow, clBtnFace, 80), drRight)
  else
    DrawGradient(DC, R, Blend(clWindow, clBtnFace), clBtnFace, drRight);
  R.Top := R.Bottom  - 1;
  FillRectColor(DC, R, clBtnShadow);
  R.Top := Rect.Top;
  S := H.Caption;
  if FSimpleText <> '' then
  begin
    FillRoundRectFancy(DC, R, 6, clBtnFace, Blend(clBtnFace, clBtnShadow, 70), drDown, clBtnShadow, Blend(clBtnShadow, clBtnFace), psSolid, bsFDiagonal);
    R.Right := R.Left + HeightOf(R);
    DrawArrow(DC, R, drRight);
    R.Left := R.Right;
    R.Right := Rect.Right - Border;
    DrawCaption(DC, FSimpleText, R, drLeft);
  end
  else if Length(FWidths) > 0 then
  begin
    A := R.Left + Border;
    P := SelectObject(DC, GetPen(Blend(cl3dDkShadow, clBtnFace, 33)));
    for I := Low(FWidths) to High(FWidths) do
    begin
      B := A + FWidths[I] - Border * 2;
      J := B + Border * 2;
      R.Left := A;
      R.Right := B;
      if R.Right > Rect.Right - Border then
        R.Right := Rect.Right - Border ;
      if I = High(FWidths) then
        R.Right := Rect.Right - Border;
      DrawCaption(DC, FieldValueInt(S, '|', I), R, drLeft);
      if FSorted and (I = FSortColumn) then
        DrawSortArrow(DC, GetHeaderRects[I], FSortDirection);
      if I = High(FWidths) then Break;
      MoveToEx(DC, J, R.Top, nil);
      LineTo(DC, J, R.Bottom);
      A := J + Border;
      if A > Rect.Right then Break;
    end;
    OverwriteObject(DC, P);
  end
  else
  begin
    InflateRect(R, -2, 0);
    DrawCaption(DC, S, R, drLeft);
  end;
end;

function THeaderBoxTheme.GetSimpleText: string;
begin
  Result := FSimpleText;
end;

procedure THeaderBoxTheme.SetSimpleText(const Value: string);
begin
  if Value <> FSimpleText then
  begin
    FSimpleText := Value;
    if Owner is TWinControl then
      (Owner as TWinControl).Invalidate;
  end;
end;

{ THeaderBoxTheme.ISortColumns }

function THeaderBoxTheme.GetColumn: Integer;
begin
  Result := FSortColumn;
end;

function THeaderBoxTheme.GetDirection: TSortDirection;
begin
  Result := FSortDirection;
end;

function THeaderBoxTheme.GetSorted: Boolean;
begin
  Result := FSorted;
end;

procedure THeaderBoxTheme.SetSorted(Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted := Value;
    if FChild <> nil then
    begin
      FChild.Sorted := Value;
      FChild.Sort(FSortColumn, FSortDirection);
    end;
    if Header <> nil then
      Header.Invalidate;
  end;
end;

procedure THeaderBoxTheme.Reference(Child: ISortColumns);
begin
  FChild := Child;
  if FChild <> nil then
  begin
    FChild.Sorted := FSorted;
    FChild.Sort(FSortColumn, FSortDirection);
  end;
end;

procedure THeaderBoxTheme.Sort(Column: Integer; Direction: TSortDirection);
begin
  if not FSorted then
  begin
    FSortColumn := Column;
    FSortDirection := Direction;
    if FChild <> nil then
      FChild.Sort(Column, Direction);
  end
  else if (FSortColumn <> Column) or (FSortDirection <> Direction) and (FSimpleText = '') then
  begin
    FSortColumn := Column;
    FSortDirection := Direction;
    if FChild <> nil then
      FChild.Sort(Column, Direction);
    if Header <> nil then
      Header.Invalidate;
  end;
end;

function ThemeHeaderBox(Header: THeaderSectionBox): IUnknown;
begin
  Result := THeaderBoxTheme.Create(Header) as IUnknown;
end;

function ThemeHeaderBox(Header: THeaderSectionBox; const ColumnWidths: array of Integer): IUnknown;
var
  T: THeaderBoxTheme;
begin
  T := THeaderBoxTheme.Create(Header);
  T.SetWidths(ColumnWidths);
  Result := T;
end;

type
  TFlowBoxTheme = class(TComponent)
  private
    FInit: Boolean;
    procedure DrawGroup(Sender: TObject; Group: TFlowGroup;
      Rect: TRect; State: TDrawState);
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TFlowBoxTheme.Create(AOwner: TComponent);
var
  F: TFlowBox absolute AOwner;
begin
  inherited Create(AOwner);
  if AOwner is TFlowBox then
    F.OnDrawGroup := DrawGroup;
end;

procedure TFlowBoxTheme.DrawGroup(Sender: TObject; Group: TFlowGroup;
  Rect: TRect; State: TDrawState);
var
  Box: TFlowBox absolute Sender;
  DC: HDC;
  F: HFONT;
  P: IGdiPen;
  B: IGdiBrush;
  S: IGdiGraphicsPath;
begin
  if not FInit then
  begin
    FInit := True;
    Box.FitHeight;
  end;
  DC := Box.Canvas.Handle;
  InflateRect(Rect, Box.Padding div 2, Box.Padding div 4);
  if dsFocused in State then
  begin
    B := NewLinearGradientBrush(NewRectF(Rect), 90, NewColor(Blend(clHighlight, clWindow, 60), $A0),
      NewColor(Blend(clHighlight, clWindow, 10), $A0));
    P := NewPen(NewColor(Blend(clHighlight, clWindow, 80), $A0), 1);
  end
  else
  begin
    B := NewSolidBrush(NewColor(clBtnShadow, $15));
    P := NewPen(NewColor(clBtnShadow, $70), 1);
  end;
  S := NewRoundRect(NewRectF(Rect), 5);
  GdiDrawPath(NewGraphics(DC), P, B, S);
  if Group.Name <> '' then
  begin
    InflateRect(Rect, Round(Box.Padding * -1.5), -(Box.Padding div 3));
    F := 0;
    if dsFocused in State then
      F := SelectFontStyle(DC, [fsBold]);
    DrawCaption(DC, Group.Name, Rect, drWrap);
    if dsFocused in State then
      OverwriteObject(DC, F);
  end;
end;

procedure ThemeFlowBox(FlowBox: TFlowBox);
begin
  TFlowBoxTheme.Create(FlowBox);
end;

type
  TProgressTheme = class(TThemeComponent)
  private
    FWindow: HWND;
    procedure DoMeasure(Sender: TObject; var Bounds: TRect);
    procedure DoDraw(Sender: TObject; const Bitmap: TFastBitmap;
      var Params: TProgressDrawParams);
  public
    constructor Create(AOwner: TComponent); override;
    property Window: HWND read FWindow write FWindow ;
  end;

constructor TProgressTheme.Create(AOwner: TComponent);
var
  P: TDrawProgress absolute AOwner;
begin
  inherited Create(AOwner);
  if AOwner is TDrawProgress then
  begin
    P.OnMeasure := DoMeasure;
    P.OnDraw := DoDraw;
  end;
end;

procedure TProgressTheme.DoMeasure(Sender: TObject; var Bounds: TRect);
const
  DialogWidth = 200;
  DialogHeight = 200;
var
  R: TRect;
begin
  GetWindowRect(FWindow, R);
  R.Left := R.Left + WidthOf(R) div 2 - DialogWidth div 2;
  R.Top := R.Top + HeightOf(R) div 2 - DialogHeight div 2;
  R.Right := R.Left + DialogWidth;
  R.Bottom := R.Top + DialogHeight;
  Bounds := R;
end;

procedure DrawProgress(const Bitmap: TFastBitmap; Step: Integer);
const
  Scale = 500;
var
  G: IGdiGraphics;
  L: IGdiBrush;
  B: IGdiSolidBRush;
  P: IGdiPen;
  R: TRectF;
  O: IGdiGraphicsPath;
  F: array[0..3] of TPointF;
  I, S: Integer;
begin
  G := NewGraphics(Bitmap.DC);
  G.Clear(0);
  R := NewRectF(0, 0, Bitmap.Width, Bitmap.Height);
  B := NewSolidBrush(NewColor(0, $FF));
  L := NewLinearGradientBrush(R, 60, NewColor(clHighlight, $F0), NewColor(clHighlight, $00));
  O := NewRoundRect(R, 20);
  G.FillPath(L, O);
  R := NewRectF(4, 4, Bitmap.Width - 8, Bitmap.Height - 8);
  O := NewRoundRect(R, 18);
  P := NewPen(NewColor(clHighlight, $80), 8);
  G.DrawPath(P, O);
  R := NewRectF(0, 0, Bitmap.Width, Bitmap.Height);
  P := NewPen(NewColor(clHighlight, $A0), 4);
  B := NewSolidBrush($FFFFFFF);
  F[0] := NewPointF(-10, 0);
  F[1] := NewPointF(10, 0);
  F[2] := NewPointF(20, 100);
  F[3] := NewPointF(-20, 100);
  if Bitmap.Width < Bitmap.Height then
    S := Bitmap.Width
  else
    S := Bitmap.Height;
  for I := 0 to 11 do
  begin
    G.ResetTransform;
    G.TranslateTransform(Bitmap.Width / 2, Bitmap.Height / 2);
    G.ScaleTransform(S / Scale, S / Scale);
    G.RotateTransform((I + Step) * 30);
    G.TranslateTransform(0, 100);
    B.SetColor($00FFFFFF or ($FF000000 div 16) * (I + 1));
    G.FillPolygon(B, PPointF(@F), 4);
    G.DrawPolygon(P, PPointF(@F), 4);
  end;
end;

procedure TProgressTheme.DoDraw(Sender: TObject; const Bitmap: TFastBitmap;
  var Params: TProgressDrawParams);
begin
  DrawProgress(Bitmap, Params.Step);
  Inc(Params.Step);
  if Params.Step < 20 then
    Params.Opacity := Round(Params.Step / 20 * $F0)
  else
    Params.Opacity := $F0;
  Params.Interval := 50;
end;

procedure ThemeProgress(Progress: TDrawProgress; Window: HWND);
begin
  TProgressTheme.Create(Progress).Window := Window;
end;

end.
