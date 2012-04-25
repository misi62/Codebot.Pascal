unit GridProviders;

interface

uses
  Windows, SysUtils, Classes, Controls, GridCtrls, Graphics, basetypes, GraphTools,
  FormTools, ProviderTools, ImgListEx;

{ TGridSectionBook }

type
  TGridSection = class(TCollectionItem)
  private
    FCaption: TCaption;
    FItems: TStrings;
    FInterfaces: TInterfaceList;
    FHeaderRow: Integer;
    FRowCount: Integer;
    procedure SetCaption(Value: TCaption);
    procedure SetItems(Value: TStrings);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Items: TStrings read FItems write SetItems;
    property Interfaces: TInterfaceList read FInterfaces;
    property Caption: TCaption read FCaption write SetCaption;
  end;

{ TGridSections }

  TGridSections = class(TOwnedCollection)
  private
    FProvider: TObject;
    function Get(Index: Integer): TGridSection;
    procedure Put(Index: Integer; Value: TGridSection);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Provider: TObject);
    function Add: TGridSection;
    function FindItemID(ID: Integer): TGridSection;
    function Insert(Index: Integer): TGridSection;
    property Items[Index: Integer]: TGridSection read Get write Put; default;
  end;

{ TGroupedItemProvider }

  TGroupIndex = record
    SectionIndex: Integer;
    ItemIndex: Integer;
  end;

  TGroupedItemProvider = class(TControlProvider, IContentGridProvider)
  private
    FConnection: TContentGrid;
    FBrush: HBRUSH;
    FSectionSize: Integer;
    FCellWidth: Integer;
    FCellHeight: Integer;
    FSections: TGridSections;
    FColCount: Integer;
    FSuspended: Boolean;
    function HeaderIndex(Row: Integer): Integer;
    function GroupIndex(Col, Row: Integer): TGroupIndex;
    procedure SectionsChanged;
    procedure SetSections(Value: TGridSections);
    function GetSelection: TGroupIndex;
    procedure SetSelection(Value: TGroupIndex);
    procedure SetCellHeight(const Value: Integer);
    procedure SetCellWidth(const Value: Integer);
  protected
    procedure Init(Control: TControl); override;
    procedure Resize(Grid: TContentGrid);
    procedure HitTest(Grid: TContentGrid; X, Y: Integer;
      var HitTest: TGridHitTest);
    procedure HotTrack(Grid: TContentGrid; Col, Row: Integer;
      var Allow: Boolean);
    procedure Select(Grid: TContentGrid; Col, Row: Integer;
      var Allow: Boolean);
    procedure DrawBackground(Grid: TContentGrid; DC: HDC);
    procedure DrawRow(Grid: TContentGrid; Row: Integer; Rect: TRect;
      var DefaultDraw: Boolean);
    procedure DrawCell(Grid: TContentGrid; Col, Row: Integer; Rect: TRect;
      State: TDrawState);
  public
    constructor Create; override;
    destructor Destroy; override;
    property Suspended: Boolean read FSuspended write FSuspended;
    property Selection: TGroupIndex read GetSelection write SetSelection;
  published
    property Sections: TGridSections read FSections write SetSections;
    property CellWidth: Integer read FCellWidth write SetCellWidth;
    property CellHeight: Integer read FCellHeight write SetCellHeight;
  end;

var
  GroupedItemProvider: TGroupedItemProvider;

implementation

{ TGridSection }

constructor TGridSection.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FItems := TStringList.Create;
  FInterfaces := TInterfaceList.Create;
end;

destructor TGridSection.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TGridSection.Assign(Source: TPersistent);
var
  EditItem: TGridSection absolute Source;
begin
  if Source is TGridSection then
  begin
    FCaption := EditItem.Caption;
    FItems.Assign(EditItem.Items);
    FInterfaces.Clear;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TGridSection.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TGridSection.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
  Changed(False);
end;

{ TGridSections }

constructor TGridSections.Create(Provider: TObject);
begin
  FProvider := Provider;
  inherited Create(nil, TGridSection);
end;

function TGridSections.Add: TGridSection;
begin
  Result := TGridSection(inherited Add);
end;

function TGridSections.FindItemID(ID: Integer): TGridSection;
begin
  Result := TGridSection(inherited FindItemID(ID));
end;

function TGridSections.Insert(Index: Integer): TGridSection;
begin
  Result := TGridSection(GetItem(Index));
end;

procedure TGridSections.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if FProvider is TGroupedItemProvider then
    TGroupedItemProvider(FProvider).SectionsChanged;
end;

function TGridSections.Get(Index: Integer): TGridSection;
begin
  Result := TGridSection(GetItem(Index));
end;

procedure TGridSections.Put(Index: Integer; Value: TGridSection);
begin
  SetItem(Index, Value);
end;

{ TGroupedItemProvider }

constructor TGroupedItemProvider.Create;
begin
  inherited Create;
  GroupedItemProvider := Self;
  FSections := TGridSections.Create(Self);
  FSectionSize := 22;
  FCellWidth := 64;
  FCellHeight := 64;
  FBrush := GetBrush(clSilver, clWhite, 8);
end;


destructor TGroupedItemProvider.Destroy;
begin
  DeleteObject(FBrush);
  inherited Destroy;
end;

procedure TGroupedItemProvider.SectionsChanged;
var
  Grid: TContentGrid;
  Section: TGridSection;
  WasAutoScroll: Boolean;
  A: TGroupIndex;
  X, Y: Integer;
  I: Integer;
begin
  if (FConnection = nil) or FSuspended then Exit;
  Grid := FConnection;
  {with Grid do
  begin
    EnterUpdate;
    try
      WasAutoScroll := AutoScroll;
      AutoScroll := False;
      A := GetSelection;
      DefColWidth := FCellWidth;
      DefRowHeight := FCellHeight;
      ColCount := ScrollWidth div DefColWidth;
      if ColCount < 1 then
        ColCount := 1;
      FColCount := ColCount;
      X := FColCount;
      Y := 0;
      for I := 0 to FSections.Count - 1 do
      begin
        Inc(Y);
        Section := FSections[I];
        if X > 0 then
        begin
          Section.FRowCount := (Section.Items.Count - 1) div X + 1;
          if Section.FRowCount < 1 then
            Section.FRowCount := 1;
        end
        else
          Section.FRowCount := 1;
        Y := Y + Section.FRowCount;
      end;
      RowCount := Y;
      Y := 0;
      for X := 0 to FSections.Count - 1 do
      begin
        RowHeights[Y] := FSectionSize;
        Section := FSections[X];
        Section.FHeaderRow := Y;
        Inc(Y);
        for I := 0 to Section.FRowCount - 1 do
        begin
          RowHeights[Y] := 0;
          Inc(Y);
        end;
      end;
      SetSelection(A);
      AutoScroll := WasAutoScroll;
    finally
      ExitUpdate
    end;
  end;     }
end;

procedure TGroupedItemProvider.SetSections(Value: TGridSections);
begin
  FSections.Assign(Value);
end;

procedure TGroupedItemProvider.DrawCell(Grid: TContentGrid; Col, Row: Integer; Rect: TRect;
  State: TDrawState);
var
  DC: HDC;
  G: TGroupIndex;
  Section: TGridSection;
  R, S: TRect;
  I: Integer;
begin
  DC := Grid.Canvas.Handle;
  S := Rect;
  // FillRectColor(DC, Rect, Grid.Color);
  G := GroupIndex(Col, Row);
  if G.ItemIndex < 0 then Exit;
  R := Rect;
  InflateRect(Rect, -2, -2);
  Rect.Bottom := Rect.Bottom - 30;
  R.Top := Rect.Bottom + 2;
  InflateRect(R, -3, 0);
  if dsSelected in State then
    DrawStyleRect(DC, Rect, [dsPressed] * State = [])
  else if dsHot in State then
  begin
    {InflateRect(Rect, -5, -5);
    FillRectColor(DC, Rect, clBtnShadow);}
    InflateRect(Rect, -3, -3);
    DrawStyleRect(DC, Rect, False);
    //FillRectColor(DC, Rect, Blend(clSilver, clWindow, 10));
  end
  else
  begin
    {InflateRect(Rect, -5, -5);
    FillRectColor(DC, Rect, clBtnShadow);
    InflateRect(Rect, -1, -1);
    FillRectColor(DC, Rect, Blend(clSilver, clWindow));}
  end;
  Section := FSections[G.SectionIndex];
  {if Assigned(Grid.OnDrawIndexSection) then
  begin
    Grid.OnDrawIndexSection(Grid, G.SectionIndex, G.ItemIndex, S, State);
  end;}
  SetBkMode(DC, TRANSPARENT);
  DrawTextEx(DC, PChar(Section.Items[G.ItemIndex]), -1, R, DT_CENTER or DT_VCENTER or DT_BOTTOM or DT_WORDBREAK or DT_NOCLIP, nil);
end;

procedure TGroupedItemProvider.DrawBackground(Grid: TContentGrid; DC: HDC);
begin
  FillRect(DC, Grid.ClientRect, GetStockObject(WHITE_BRUSH));
end;

procedure TGroupedItemProvider.DrawRow(Grid: TContentGrid; Row: Integer; Rect: TRect;
  var DefaultDraw: Boolean);
var
  DC: HDC;
  F: HFONT;
  S: string;
  I: Integer;
begin
  I := HeaderIndex(Row);
  DefaultDraw  := I = -1;
  if DefaultDraw then
    Exit;
  DC := Grid.Canvas.Handle;
  Rect.Right := Grid.Width;
  FillRectColor(DC, Rect, Grid.Color);
  F := SelectObject(DC, GetFont(DC, [fsBold]));
  Inc(Rect.Left, 8);
  S := FSections[I].Caption;
  DrawCaption(DC, S, Rect, drLeft);
  Inc(Rect.Left, CalcCaptionSize(DC, S).cx);
  InflateRect(Rect, -8, 0);
  Rect.Top := Rect.Top + HeightOf(Rect) div 2 - 1;
  Rect.Bottom := Rect.Top + 2;
  DrawGradient(DC, Rect, Blend(clHighlight, clWindow), clWindow, drRight);
  OverwriteObject(DC, F);
end;

procedure TGroupedItemProvider.HitTest(Grid: TContentGrid; X, Y: Integer;
  var HitTest: TGridHitTest);
begin
end;

procedure TGroupedItemProvider.Init(Control: TControl);
begin
  if Control is TContentGrid then
    FConnection := TContentGrid(Control)
  else
    FConnection := nil;
  SectionsChanged;
end;

procedure TGroupedItemProvider.Resize(Grid: TContentGrid);
begin
  if Grid.ScrollWidth div Grid.DefColWidth <> FColCount then
    SectionsChanged;
end;

procedure TGroupedItemProvider.HotTrack(Grid: TContentGrid; Col, Row: Integer;
  var Allow: Boolean);
var
  G: TGroupIndex;
begin
  Allow := HeaderIndex(Row) = -1;
  if Allow then
  begin
    G := GroupIndex(Col, Row);
    Allow := (G.SectionIndex > -1) and (G.ItemIndex > -1);
  end;
end;

procedure TGroupedItemProvider.Select(Grid: TContentGrid; Col, Row: Integer;
  var Allow: Boolean);
var
  G: TGroupIndex;
begin
  Allow := HeaderIndex(Row) = -1;
  if Allow then
  begin
    G := GroupIndex(Col, Row);
    Allow := (G.SectionIndex > -1) and (G.ItemIndex > -1);
  end;
end;

function TGroupedItemProvider.GetSelection: TGroupIndex;
begin
  if FConnection = nil then
  begin
    Result.SectionIndex := -1;
    Result.ItemIndex := -1;
    Exit;
  end;
  with FConnection.Selection do
    Result := GroupIndex(X, Y);
end;

procedure TGroupedItemProvider.SetSelection(Value: TGroupIndex);
var
  C: TGridCoord;
  S: TGridSection;
begin
  if FConnection = nil then Exit;
  if (Value.SectionIndex < 0) or (Value.ItemIndex < 0) then Exit;
  S := FSections[Value.SectionIndex];
  C.Y := S.FHeaderRow + 1 + (Value.ItemIndex) div FColCount;
  C.X := Value.ItemIndex mod FColCount;
  FConnection.Selection := C;
end;

function TGroupedItemProvider.HeaderIndex(Row: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FSections.Count - 1 do
    if FSections[I].FHeaderRow = Row then
    begin
      Result := I;
      Break;
    end;
end;

function TGroupedItemProvider.GroupIndex(Col, Row: Integer): TGroupIndex;
var
  A, B: TGridSection;
  I: Integer;
begin
  B := nil;
  for I := 0 to FSections.Count - 1 do
  begin
    A := FSections[I];
    if A.FHeaderRow > Row then
      Break;
    B := A;
    Result.SectionIndex := I;
  end;
  A := B;
  if A <> nil then
  begin
    Result.ItemIndex := (Row - (A.FHeaderRow + 1)) * FColCount + Col;
    if Result.ItemIndex < 0 then
      Result.ItemIndex := -1
    else if Result.ItemIndex > A.Items.Count - 1 then
      Result.ItemIndex := -1;
  end
  else
  begin
    Result.SectionIndex := -1;
    Result.ItemIndex := -1;
  end;
end;

procedure TGroupedItemProvider.SetCellHeight(const Value: Integer);
begin
  if Value <> FCellHeight then
  begin
    FCellHeight := Value;
    SectionsChanged;
  end;
end;

procedure TGroupedItemProvider.SetCellWidth(const Value: Integer);
begin
  if Value <> FCellHeight then
  begin
    FCellWidth := Value;
    SectionsChanged;
  end;
end;

initialization
  // RegisterDefaultProvider(TGroupedItemProvider, TContentGrid);
end.
