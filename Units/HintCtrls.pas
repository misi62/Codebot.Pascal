
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit HintCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Math,
  BaseTypes, SysTools, GdiPlus, GdiIntf, GraphTools;

{ TArrowHintWindow }

type
  TArrowHintWindow = class(THintWindow)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
  end;

{ The EnableHints procedure }

procedure EnableHints(WindowClass: THintWindowClass);

{ ShortCut tips }

procedure ShortCutTip(Control: TControl; const Tip: string);
procedure ShortCutOpacity(Opacity: Byte);

implementation

var
  InternalTips: TObject;
  InternalHintFont: IInterface;
  InternalShortcutFont: IInterface;
  InternalHintingComponent: TComponent;
  InternalHintingControl: TControl;

type
  THintingComponent = class(TComponent)
  private
    procedure HandleHints(var HintStr: string; var CanShow: Boolean;
      var HintInfo: THintInfo);
  public
    procedure EnableHints;
  end;

procedure THintingComponent.EnableHints;
begin
  Application.OnShowHint := HandleHints;
end;

procedure THintingComponent.HandleHints(var HintStr: string; var CanShow: Boolean;
  var HintInfo: THintInfo);
begin
  InternalHintingControl := HintInfo.HintControl;
end;

procedure EnableHints(WindowClass: THintWindowClass);
begin
  if InternalHintingComponent = nil then
    InternalHintingComponent := THintingComponent.Create(Application);
  THintingComponent(InternalHintingComponent).EnableHints;
  HintWindowClass := WindowClass;
end;

function GdiArrowHint(Font: IGdiFont; const Hint: string): IGdiGraphics;
const
  HBorder = 12;
  VBorder = 4;
  Stem = 22;
  Trunk = 12;
  Radius = 4;
var
  DC: HDC;
  Bot: Single;
  Mid: Single;
  Wide: Single;
  TextColor: TArgb;
  G: IGdigraphics;
  P: IGdiGraphicsPath;
  B: IGdiBrush;
  R: TRectF;
  S: TSizeI;
begin
  DC := GetDC(0);
  G := NewGraphics(DC);
  S := NewSizeI(GdiMeasureText(G, Font, Hint));
  G := nil;
  ReleaseDC(0, DC);
  G := NewGraphics(S.Width + HBorder * 2 + 1, S.Height + VBorder * 2 + Stem + 1);
  R := NewRectF(G.Width - 1, S.Height + VBorder * 2 - 1);
  InflateRectF(R, -1, -1);
  P := NewGraphicsPath;
  with R do
  begin
    P.AddLine(X + Radius, Y, X + Width - Radius, Y);
    P.AddArc(X + Width - (Radius * 2), Y, Radius * 2, Radius * 2, 270, 90);
    P.AddLine(X + Width, Y + Radius, X + Width, Y + Height - Radius);
    P.AddArc(X + Width - (Radius * 2), Y + Height - (Radius * 2), Radius * 2, Radius * 2, 0, 90);
    Bot := Y + Height;
    Mid := (X + Width) / 2;
    Wide := Trunk / 2;
    P.AddLine(X + Width - Radius, Bot, Mid + Wide, Bot);
    P.AddLine(Mid + Wide, Bot + Stem - Trunk, Mid + Wide + Trunk / 2, Bot + Stem - Trunk);
    P.AddLine(Mid, Bot + Stem, Mid - Wide - Trunk / 2, Bot + Stem - Trunk);
    P.AddLine(Mid - Wide, Bot + Stem - Trunk, Mid - Wide, Bot);
    P.AddArc(X, Y + Height - (Radius * 2), Radius * 2, Radius * 2, 90, 90);
    P.AddLine(X, Y + Height - Radius, X, Y + Radius);
    P.AddArc(X, Y, Radius * 2, Radius * 2, 180, 90);
  end;
  P.CloseFigure;
  TextColor := NewBlend(clWindowText, clInfoBk, 75);
  R := NewRectF(G.Width - 1, S.Height + VBorder * 2 - 1);
  InflateRectF(R, -1, -1);
  G.DrawPath(NewPen(TextColor, 1.5), P);
  R := NewRectF(G.Width, G.Height);
  B := NewLinearGradientBrush(R, 90, NewColor(clWindow), NewBlend(clInfoBk, clRed, 95));
  G.FillPath(B, P);
  R := NewRectF(G.Width - 1, S.Height + VBorder * 2 - 1);
  OffsetRectF(R, 0, 1);
  GdiDrawText(G, Font, TextColor, Hint, R, drCenter);
  Result := G;
end;

{ TArrowHintWindow }

procedure TArrowHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
  P: TPoint;
begin
  if InternalHintingControl <> nil then
  begin
    P := InternalHintingControl.ClientToScreen(Point(0, 0));
    Left := P.X + (InternalHintingControl.Width - Width) div 2;
    Top := P.Y - Height;
    ShowWindow(Handle, SW_SHOWNA);
  end;
  InternalHintingControl := nil;
end;

function TArrowHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
var
  G: IGdiGraphics;
  F: TFont;
begin
  if InternalHintFont = nil then
  begin
    F := Screen.HintFont;
    InternalHintFont := NewFont(F.Name, F.Size, FontStyleRegular);
  end;
  G := GdiArrowHint(InternalHintFont as IGdiFont, AHint);
  G.Overlay(Handle);
  Result := Rect(0, 0, G.Width, G.Height);
  UpdateBoundsRect(Result);
end;

procedure TArrowHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    WindowClass.Style := WindowClass.Style and (not CS_DROPSHADOW);
    Style := WS_POPUP or WS_DISABLED;
    ExStyle := WS_EX_TOPMOST  or WS_EX_TOOLWINDOW or WS_EX_TRANSPARENT;
  end;
end;

type
  TShortCutTip = class(TComponent)
  private
    FWindow: TUtilityWindow;
    FChanged: Boolean;
    FTip: string;
    procedure SetTip(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Change;
    procedure Show(Opacity: Byte);
    procedure Hide;
    property Tip: string read FTip write SetTip;
  end;

{ TShortCutTip }

constructor TShortCutTip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindow := TUtilityWindow.Create(Self, 0, WS_POPUP or WS_DISABLED,
    WS_EX_TOPMOST  or WS_EX_TOOLWINDOW or WS_EX_TRANSPARENT);
  FChanged:= True;
end;

destructor TShortCutTip.Destroy;
begin
  FWindow.Free;
  inherited Destroy;
end;

procedure TShortCutTip.Change;
begin
  FChanged := True;
end;

procedure TShortCutTip.Show(Opacity: Byte);
const
  DefSize = 25;
var
  Control: TControl;
  Parent, Focus: HWND;
  Point: TPoint;
  Rect: TRect;
  G: IGdiGraphics;
  F: IGdiFont;
  R: TRectF;
  C: TArgb;
begin
  Control := Owner as TControl;
  Parent := GetParentForm(Control).Handle;
  Focus := GetFocus;
  if (Parent = Focus) or IsChild(Parent, Focus) then
  begin
    if FChanged then
    begin
      R := NewRectF(DefSize, DefSize);
      C := NewColor(clHighlight);
      G := NewGraphics(DefSize, DefSize);
      F := InternalShortcutFont as IGdiFont;
      InflateRectF(R, -2, -2);
      G.FillEllipse(NewSolidBrush(C), R);
      InflateRectF(R, -2, -2);
      G.FillEllipse(NewSolidBrush(NewColor(clWindow)), R);
      R := NewRectF(DefSize, DefSize);
      OffsetRectF(R, 1, 1);
      if (FTip <> '') and (FTip[1] in ['0'..'9']) then
        OffsetRectF(R, -1, 0);
      GdiDrawText(G, F, C, FTip, R, drCenter);
      G.Overlay(FWindow.Handle, Opacity);
      FChanged := False;
    end;
    Point.X := 0;
    Point.Y := 0;
    Point := Control.ClientToScreen(Point);
    Rect := Control.BoundsRect;
    OffsetRect(Rect, Point.X - Rect.Left, Point.Y - Rect.Top);
    SetWindowPos(FWindow.Handle, 0, Rect.Left + (WidthOf(Rect) - DefSize) div 2,
      Rect.Top + (HeightOf(Rect) - DefSize) div 2, 0, 0,
      SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
    ShowWindow(FWindow.Handle, SW_SHOWNA);
  end;
end;

procedure TShortCutTip.Hide;
begin
  ShowWindow(FWindow.Handle, SW_HIDE);
end;

procedure TShortCutTip.SetTip(const Value: string);
begin
  if Value <> FTip then
  begin
    FChanged := True;
    FTip := Value;
  end;
end;

type
  TShortCutTips = class(TComponent)
  private
    FTips: TList;
    FShowing: Boolean;
    FOpacity: Byte;
    procedure MessageHook(const WinProcStruct: TCWPStruct);
    procedure KeyboardHook(Key: Word; State: Cardinal; var Remove: Boolean);
    procedure SetOpacity(const Value: Byte);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(Tip: TShortCutTip);
    procedure Remove(Tip: TShortCutTip);
    procedure ShowTips(Opacity: Byte);
    procedure HideTips;
    property Opacity: Byte read FOpacity write SetOpacity;
  end;

function ShortCutTips: TShortCutTips;
begin
  if InternalTips = nil then
  begin
    InternalTips := TShortCutTips.Create(Application);
    InternalShortcutFont := NewFont('Arial Black', 14, FontStyleRegular, UnitPixel);
  end;
  Result := TShortCutTips(InternalTips);
end;

{ TShortCutTips }

constructor TShortCutTips.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTips := TList.Create;
  FOpacity := $FF;
  HookMessage(MessageHook);
  HookKeyboard(KeyboardHook);
end;

destructor TShortCutTips.Destroy;
var
  I: Integer;
begin
  UnhookKeyboard(KeyboardHook);
  UnhookMessage(MessageHook);
  for I := 0 to FTips.Count - 1 do
    RemoveFreeNotification(TComponent(FTips[I]));
  FTips.Free;
  inherited Destroy;
end;

procedure TShortCutTips.KeyboardHook(Key: Word; State: Cardinal; var Remove: Boolean);
begin
  if Key = VK_CONTROL then
    if State and $80000000 = 0 then
      ShowTips(FOpacity)
    else
      HideTips;
end;

procedure TShortCutTips.MessageHook(const WinProcStruct: TCWPStruct);
var
  W: TWinControl;
begin
  W := FindControl(WinProcStruct.hwnd);
  if W = nil then Exit;
  if W is THintWindow then Exit;
  case WinProcStruct.message of
    WM_MOVE,
    WM_SIZE,
    WM_SETFOCUS:
      HideTips;
  end;
end;

procedure TShortCutTips.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent is TShortCutTip) and (Operation = opRemove) then
    FTips.Remove(AComponent)
end;

procedure TShortCutTips.Add(Tip: TShortCutTip);
begin
  HideTips;
  if FTips.IndexOf(Tip) < 0 then
  begin
    FTips.Add(Tip);
    Tip.FreeNotification(Self);
  end;
end;

procedure TShortCutTips.Remove(Tip: TShortCutTip);
begin
  if FTips.IndexOf(Tip) > -1 then
  begin
    FTips.Remove(Tip);
    Tip.RemoveFreeNotification(Self);;
  end;
end;

procedure TShortCutTips.SetOpacity(const Value: Byte);
var
  Tip: TShortCutTip;
  I: Integer;
begin
  HideTips;
  if FOpacity <> Value then
  begin
    FOpacity := Value;
     for I := 0 to FTips.Count - 1 do
    begin
      Tip := TShortCutTip(FTips[I]);
      Tip.Change;
    end;
  end;
end;

procedure TShortCutTips.ShowTips(Opacity: Byte);
var
  Tip: TShortCutTip;
  I: Integer;
begin
  if not FShowing then
     for I := 0 to FTips.Count - 1 do
    begin
      Tip := TShortCutTip(FTips[I]);
      Tip.Show(Opacity);
    end;
  FShowing := True;
end;

procedure TShortCutTips.HideTips;
var
  I: Integer;
begin
  if FShowing then
    for I := 0 to FTips.Count - 1 do
      TShortCutTip(FTips[I]).Hide;
  FShowing := False;
end;

procedure ShortCutTip(Control: TControl; const Tip: string);
var
  ShortCutTip: TShortCutTip;
  I: Integer;
begin
  ShortCutTip := nil;
  for I := 0 to Control.ComponentCount - 1 do
    if Control.Components[I] is TShortCutTip then
    begin
      ShortCutTip := TShortCutTip(Control.Components[I]);
      Break;
    end;
  if Tip <> '' then
  begin
    if ShortCutTip = nil then
    begin
      ShortCutTip := TShortCutTip.Create(Control);
       ShortCutTips.Add(ShortCutTip);
    end;
    ShortCutTip.Tip := Tip;
  end
  else if ShortCutTip <> nil then
    ShortCutTip.Free;
end;

procedure ShortCutOpacity(Opacity: Byte);
begin
  ShortCutTips.Opacity := Opacity;
end;

end.
