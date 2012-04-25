unit ListCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  BaseTypes, GraphTools, ProviderTools, FormTools, ScrollCtrls, ImgListEx;

{ TCheckDrawList }

type
  TCheckDrawList = class;

  ICheckListProvider = interface(IControlProvider)
    ['{63104CFC-6D28-4DAC-ACD9-38D411BBC854}']
    function GetCheckRect(List: TCheckDrawList; Rect: TRect): TRect;
    procedure DrawBackground(List: TCheckDrawList);
    procedure DrawItem(List: TCheckDrawList; Index: Integer; Rect: TRect; State: TDrawState);
    procedure DrawCheck(List: TCheckDrawList; Rect: TRect; State: TDrawState);
  end;

{ TCheckBook }

  TCheckItem = class(TIntfCollectionItem)
  private
    FChecked: Boolean;
    FCaption: TCaption;
    FDescription: string;
    FImageIndex: Integer;
    procedure SetChecked(Value: Boolean);
    procedure SetCaption(Value: TCaption);
    procedure SetDescription(const Value: string);
    procedure SetImageIndex(Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Checked: Boolean read FChecked write SetChecked;
    property Caption: TCaption read FCaption write SetCaption;
    property Description: string read FDescription write SetDescription;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

{ TCheckItems }

  TCheckItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TCheckItem;
    procedure Put(Index: Integer; Value: TCheckItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TCheckItem;
    function FindItemID(ID: Integer): TCheckItem;
    function Insert(Index: Integer): TCheckItem;
    property Items[Index: Integer]: TCheckItem read Get write Put; default;
  end;

{ TCheckDrawList }

  TCheckDrawList = class(TCustomDrawList)
  private
    FItems: TCheckItems;
    FCheckDownIndex: Integer;
    FCheckHotIndex: Integer;
    FCheckHotRect: TRect;
    FSpaceDown: Boolean;
    function GetListProvider: ICheckListProvider;
    procedure SetItems(Value: TCheckItems);
  protected
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure ItemsChanged; dynamic;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property ListProvider: ICheckListProvider read GetListProvider;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property Cursor;
    property Images;
    property ItemHeight;
    property ProviderName;
    property Items: TCheckItems read FItems write SetItems;
    property TabStop;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

implementation

{$R LISTCTRLS.RES}

const
  RES_LISTCTRLS = 8802;

var
  InternalProviderImages: TObject;

function ProviderImages: TGlassImageList;

begin
  if InternalProviderImages = nil then
  begin
    Result := TGlassImageList.Create(Application);
    Result.Images.LoadFromResourceID(RES_LISTCTRLS);
    InternalProviderImages := Result;
  end
  else
    Result := TGlassImageList(InternalProviderImages);
end;

type
  TDefaultCheckListProvider = class(TControlProvider, ICheckListProvider)
  protected
    procedure Init(Control: TControl); override;
    function GetCheckRect(List: TCheckDrawList; Rect: TRect): TRect;
    procedure DrawBackground(List: TCheckDrawList);
    procedure DrawItem(List: TCheckDrawList; Index: Integer; Rect: TRect; State: TDrawState);
    procedure DrawCheck(List: TCheckDrawList; Rect: TRect; State: TDrawState);
  end;

procedure TDefaultCheckListProvider.Init(Control: TControl);
begin
  if Control is TCheckDrawList then
  begin
    TCheckDrawList(Control).ItemHeight := 68;
    TCheckDrawList(Control).HotTrack := True;
  end;
end;

function TDefaultCheckListProvider.GetCheckRect(List: TCheckDrawList; Rect: TRect): TRect;
begin
  Rect.Top := Rect.Top + HeightOf(Rect) div 2 - ProviderImages.Height div 2;
  Rect.Bottom := Rect.Top + ProviderImages.Height;
  Rect.Left := 0;
  Rect.Right := Rect.Left + ProviderImages.Height;
  Result := Rect;
end;

procedure TDefaultCheckListProvider.DrawBackground(List: TCheckDrawList);
var
  DC: HDC;
begin
  DC := List.Canvas.Handle;
  FillRectColor(DC, List.ClientRect, List.Color);
end;

procedure TDefaultCheckListProvider.DrawItem(List: TCheckDrawList; Index: Integer; Rect: TRect; State: TDrawState);
var
  DC: HDC;
  B: HBRUSH;
  R: TRect;
  F: HFONT;
  S: string;
begin
  DC := List.Canvas.Handle;
  R := Rect;
  R.Top := R.Bottom - 1;
  B := GetBrush(List.Color, clSelectedBorder);
  FillRect(DC, R, B);
  DeleteObject(B);
  R := Rect;
  R.Left := R.Left + ProviderImages.Height + 2;
  InflateRect(R, -2, -2);
  if dsSelected in State then
    DrawStyleRoundRect(DC, R, False, not List.Focused)
  else if dsHot in State then
    DrawStyleRoundRect(DC, R, True, not List.Focused);
  InflateRect(R, -5, -5);
  Rect := R;
  F := SelectFontStyle(DC, [fsBold]);
  R.Bottom := R.Top + FontHeight(DC);
  S := List.Items[Index].Caption;
  DrawCaption(DC, S, R, drLeft, List.Enabled);
  OverWriteObject(DC, F);
  R.Top := R.Bottom + 2;
  R.Bottom := Rect.Bottom + 2;
  S := List.Items[Index].Description;
  DrawCaption(DC, S, R, drWrap, List.Enabled);
end;

procedure TDefaultCheckListProvider.DrawCheck(List: TCheckDrawList; Rect: TRect;
  State: TDrawState);
var
  I: Integer;
begin
  if dsChecked in State then
    I := 1
  else
    I := 0;
  ProviderImages.DrawImage(List.Canvas, Rect.Left, Rect.Top, I, State);
end;

{ TCheckItem }

procedure TCheckItem.Assign(Source: TPersistent);
var
  EditItem: TCheckItem absolute Source;
begin
  if Source is TCheckItem then
  begin
    FChecked := EditItem.FChecked;
    FCaption := EditItem.Caption;
    FDescription := EditItem.FDescription;
    FImageIndex := EditItem.ImageIndex;
    Changed(False);
  end
  else
    inherited Assign(Source);
end;

procedure TCheckItem.SetChecked(Value: Boolean);
begin
  if Value <> FChecked then
  begin
    FChecked := Value;
    Changed(False);
  end;
end;

procedure TCheckItem.SetCaption(Value: TCaption);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TCheckItem.SetDescription(const Value: string);
begin
  if Value <> FDescription then
  begin
    FDescription := Value;
    Changed(False);
  end;
end;

procedure TCheckItem.SetImageIndex(Value: Integer);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;

{ TCheckItems }

constructor TCheckItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCheckItem);
end;

function TCheckItems.Add: TCheckItem;
begin
  Result := TCheckItem(inherited Add);
end;

function TCheckItems.FindItemID(ID: Integer): TCheckItem;
begin
  Result := TCheckItem(inherited FindItemID(ID));
end;

function TCheckItems.Insert(Index: Integer): TCheckItem;
begin
  Result := TCheckItem(inherited Insert(Index));
end;

procedure TCheckItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TCheckDrawList then
    TCheckDrawList(GetOwner).ItemsChanged;
end;

function TCheckItems.Get(Index: Integer): TCheckItem;
begin
  Result := TCheckItem(GetItem(Index));
end;

procedure TCheckItems.Put(Index: Integer; Value: TCheckItem);
begin
  SetItem(Index, Value);
end;

{ TCheckDrawList }

constructor TCheckDrawList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TCheckItems.Create(Self);
  FCheckDownIndex := -1;
  FCheckHotIndex := -1;
  TabStop := True;
end;

destructor TCheckDrawList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TCheckDrawList.GetListProvider: ICheckListProvider;
begin
  if not Supports(Provider, ICheckListProvider, Result) then
    Result := nil;
end;

procedure TCheckDrawList.DrawBackground;
begin
  if ListProvider <> nil then
    ListProvider.DrawBackground(Self);
end;

procedure TCheckDrawList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  R: TRect;
begin
  if ListProvider = nil then Exit;
  ListProvider.DrawItem(Self, Index, Rect, State);
  State := [];
  if not Enabled then
    Include(State, dsDisabled)
  else if Index = FCheckHotIndex then
    Include(State, dsHot);
  if Index = FCheckDownIndex then
    Include(State, dsPressed);
  if Items[Index].Checked then
    Include(State, dsChecked);
  if (Index = ItemIndex) and FSpaceDown then
  begin
    Include(State, dsPressed);
    Include(State, dsHot);
  end;
  R := Rect;
  R := ListProvider.GetCheckRect(Self, R);
  ListProvider.DrawCheck(Self, R, State);
end;

procedure TCheckDrawList.ItemsChanged;
begin
  FCheckDownIndex := -1;
  FCheckHotIndex := -1;
  Count := FItems.Count;
  Repaint;
end;

procedure TCheckDrawList.DblClick;
begin
  inherited;
  if ItemIndex > -1 then
    Items[ItemIndex].Checked := not Items[ItemIndex].Checked;
end;

procedure TCheckDrawList.KeyDown(var Key: Word; Shift: TShiftState);
var
  R: TRect;
begin
  inherited KeyDown(Key, Shift);
  if (Key = VK_SPACE) and (ItemIndex > -1) and (not FSpaceDown) then
  begin
    FSpaceDown := True;
    R := ItemRect(ItemIndex);
    if ListProvider <> nil then
      R := ListProvider.GetCheckRect(Self, R);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TCheckDrawList.KeyUp(var Key: Word; Shift: TShiftState);
var
  R: TRect;
begin
  inherited KeyUp(Key, Shift);
  if (Key = VK_SPACE) or (Key = VK_RETURN) then
    if FSpaceDown  or (Key = VK_RETURN) then
    begin
      FSpaceDown := False;
      if ItemIndex > -1 then
      begin
        Items[ItemIndex].FChecked := not Items[ItemIndex].FChecked;
        R := ItemRect(ItemIndex);
        if ListProvider <> nil then
          R := ListProvider.GetCheckRect(Self, R);
        InvalidateRect(Handle, @R, False);
      end;
    end;
end;

procedure TCheckDrawList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cursor: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (FCheckHotIndex > -1) then
  begin
    Cursor := Point(X, Y);
    if PtInRect(FCheckHotRect, Cursor) then
    begin
      FCheckDownIndex := FCheckHotIndex;
      InvalidateRect(Handle, @FCheckHotRect, False);
    end
    else
      FCheckDownIndex := -1;
  end;
end;

procedure TCheckDrawList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item, CheckHotRect: TRect;
  Cursor: TPoint;
  Index, CheckHotIndex, I: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if ListProvider = nil then Exit;
  Item := ItemRect(TopIndex);
  Cursor := Point(X, Y);
  CheckHotIndex := -1;
  for I := 0 to ClientHeight div ItemHeight do
  begin
    Index := TopIndex + I;
    if Index > Count - 1 then Break;
    CheckHotRect := ListProvider.GetCheckRect(Self, Item);
    if PtInRect(CheckHotRect, Cursor) then
    begin
      CheckHotIndex := Index;
      Break;
    end;
    Slide(Item);
  end;
  if FCheckHotIndex <> CheckHotIndex then
  begin
    if FCheckHotIndex > -1 then
      InvalidateRect(Handle, @FCheckHotRect, False);
    FCheckHotRect := CheckHotRect;
    FCheckHotIndex := CheckHotIndex;
    if FCheckHotIndex > -1 then
      InvalidateRect(Handle, @FCheckHotRect, False);
  end;
end;

procedure TCheckDrawList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Cursor: TPoint;
begin
  if (Button = mbLeft) and (FCheckDownIndex > -1) then
  begin
    if FCheckHotIndex > -1 then
    begin
      Cursor := Point(X, Y);
      if PtInRect(FCheckHotRect, Cursor) then
      begin
        Items[FCheckDownIndex].FChecked := not Items[FCheckDownIndex].FChecked;
        InvalidateRect(Handle, @FCheckHotRect, False);
      end;
    end;
    FCheckDownIndex := -1;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCheckDrawList.SetItems(Value: TCheckItems);
begin
  FItems.Assign(Value);
end;

initialization
  RegisterDefaultProvider(TDefaultCheckListProvider, TCheckDrawList);
end.
