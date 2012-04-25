
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit LayerCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  GraphTools, FormTools;

type
  TLayerState = (lsEmpty, lsVisible, lsHidden);

  TLayerGrid = class(TGraphicControl)
  private
    FLayers: array of TLayerState;
    FCurrentLayer: Integer;
    FDownIndex: Integer;
    FOnChange: TNotifyEvent;
    function GetLayer(Index: Integer): TLayerState;
    function GetLayerCount: Integer;
    procedure SetCurrentLayer(Value: Integer);
    procedure SetLayer(Index: Integer; Value: TLayerState);
    procedure SetLayerCount(Value: Integer);
  protected
    procedure Change;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property LayerCount: Integer read GetLayerCount write SetLayerCount;
    property Layers[Index: Integer]: TLayerState read GetLayer write SetLayer;
  published
    property Anchors;
    property Align;
    property Hint;
    property ParentShowHint;
    property ShowHint;
    property Enabled;
    property Visible;
    property CurrentLayer: Integer read FCurrentLayer write SetCurrentLayer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R layerctrls.res}

type
  TLayerImages = class(TComponent)
  public
    Visible: TAlphaImage;
    Hidden: TAlphaImage;
    Left: TAlphaImage;
    Right: TAlphaImage;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TLayerImages.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := TAlphaImage.Create;
  Hidden := TAlphaImage.Create;
  Left := TAlphaImage.Create;
  Right := TAlphaImage.Create;
  Visible.LoadFromResourceID(4551);
  Hidden.LoadFromResourceID(4552);
  Left.LoadFromResourceID(4553);
  Right.LoadFromResourceID(4554);
end;

destructor TLayerImages.Destroy;
begin
  Visible.Free;
  Hidden.Free;
  Left.Free;
  Right.Free;
  inherited Destroy;
end;

var
  InternalLayerImages: TObject;

function LayerImages: TLayerImages;
begin
  if InternalLayerImages = nil then
    InternalLayerImages := TLayerImages.Create(Application);
  Result := TLayerImages(InternalLayerImages);
end;

{ TLayerGrid }

const
  GridWidth = 20;
  GridHeight = 20;
  GridBevel = 8;
  ButtonWidth = 16;

constructor TLayerGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque] + [csDoubleClicks];
  SetLength(FLayers, 5);
  SetBounds(Left, Top, 0, 0);
  FDownIndex := -1;
end;

procedure TLayerGrid.Change;
begin
  Repaint;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TLayerGrid.GetLayer(Index: Integer): TLayerState;
begin
  Result := FLayers[Index];
end;

function TLayerGrid.GetLayerCount: Integer;
begin
  Result := Length(FLayers);
end;

procedure TLayerGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  I: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    I := X div GridWidth;
    if I = CurrentLayer then
      FDownIndex := I
    else
      FDownIndex := -1;
    if I < LayerCount then
      CurrentLayer := I;
  end;
end;

procedure TLayerGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  I: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    I := X div GridWidth;
    if FDownIndex = I then
      if I < LayerCount then
        if Layers[I] = lsVisible then
          Layers[I] := lsHidden
        else if Layers[I] = lsHidden then
          Layers[I] := lsVisible
      else
      begin

      end;
    FDownIndex := -1;
  end;
end;

procedure TLayerGrid.Paint;
var
  DC: HDC;
  R: TRect;
  P: HPEN;
  B: HBRUSH;
  I: Integer;
begin
  DC := Canvas.Handle;
  R := ClientRect;
  R.Right := GridWidth * LayerCount;
  P := SelectObject(DC, GetStockObject(BLACK_PEN));
  B := SelectObject(DC, GetStockObject(BLACK_BRUSH));
  BeginPath(DC);
  RoundRect(DC, R.Left, R.Top, R.Right, R.Bottom, GridBevel, GridBevel);
  EndPath(DC);
  SelectObject(DC, P);
  SelectObject(DC, B);
  SelectClipPath(DC, RGN_COPY);
  FillRectColor(DC, R, Color);
  if FCurrentLayer > -1 then
  begin
    R.Left := GridWidth * FCurrentLayer;
    R.Right := R.Left + GridWidth;
    FillRectColor(DC, R, clThemeBorder);
  end;
  SelectClipRgn(DC, 0);
  P := SelectObject(DC, GetPen(Blend(clBtnFace, clBtnShadow)));
  B := SelectObject(DC, GetStockObject(NULL_BRUSH));
  R := ClientRect;
  R.Right := GridWidth * LayerCount;
  RoundRect(DC, R.Left, R.Top, R.Right, R.Bottom, GridBevel, GridBevel);
  for I := 1 to LayerCount - 1 do
  begin
    MoveToEx(DC, GridWidth * I, 0, nil);
    LineTo(DC, GridWidth * I, GridHeight);
  end;
  MoveToEx(DC, GridWidth * LayerCount + ButtonWidth, 2, nil);
  LineTo(DC, GridWidth * LayerCount + ButtonWidth, GridHeight - 2);
  OverwriteObject(DC, P);
  SelectObject(DC, B);
  for I := 0 to LayerCount - 1 do
    case Layers[I] of
      lsVisible: Canvas.Draw(I * GridWidth + 3, 2, LayerImages.Visible);
      lsHidden: Canvas.Draw(I * GridWidth + 3, 2, LayerImages.Hidden);
    end;
  LayerImages.Left.Opacity := $80;
  LayerImages.Right.Opacity := $80;
  Canvas.Draw(GridWidth * LayerCount, 2, LayerImages.Left);
  Canvas.Draw(GridWidth * LayerCount + ButtonWidth + 1, 2, LayerImages.Right);
end;

procedure TLayerGrid.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, LayerCount * GridWidth + ButtonWidth * 2 + 1,  GridHeight);
end;

procedure TLayerGrid.SetCurrentLayer(Value: Integer);
begin
  if Value < -1 then
    Value := -1;
  if Value > 15 then
    Value := 15;
  if Value <> FCurrentLayer then
  begin
    FCurrentLayer := Value;
    Change;
  end;
end;

procedure TLayerGrid.SetLayer(Index: Integer; Value: TLayerState);
begin
  if FLayers[Index] <> Value then
  begin
    FLayers[Index] := Value;
    Change;
  end;
end;

procedure TLayerGrid.SetLayerCount(Value: Integer);
begin
  if Value < 0 then
    Value := 1
  else if Value > 16 then
    Value := 16;
  if Value <> LayerCount then
  begin
    SetLength(FLayers, Value);
    if FCurrentLayer > LayerCount then
      FCurrentLayer := LayerCount - 1;
    SetBounds(Left, Top, 0, 0);
    Change;
  end;
end;

end.
