unit SnapCtrls;

interface

uses
  Windows, Messages, ActiveX, SysUtils, Classes, Controls, Graphics, Forms,
  SysTools, BaseTypes, GraphTools, GdiPlus, GdiIntf, BlendTools;

const
  CM_CLICK = WM_USER + 1;

type
  TSnapGrabber = class(TObject)
  private
    FSnapWindow: TUtilityWindow;
    FBitmap: TFastBitmap;
    FCaptured: Boolean;
    FRect: TRect;
    FState: TDrawState;
    FButtonChanged: Boolean;
    FButtonIndex: Integer;
    FWindowHint: string;
    FTimerCount: Integer;
    FOnCancel: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnMove: TNotifyEvent;
    function GetHandle: THandle;
    function GetIconRect(Width: Integer = 0; Height: Integer = 0): TRect;
    function GetSnapRect: TRect;
    procedure UpdateBitmap(Width: Integer = 0; Height: Integer = 0);
    procedure SetButtonIndex(Value: Integer);
    procedure CMClick(var Message: TMessage); message CM_CLICK;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
  protected
    procedure Click;
    procedure Show;
    procedure Hide;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Popup(const Rect: TRect);
    property WindowHint: string read FWindowHint write FWindowHint;
    property Handle: THandle read GetHandle;
    property ButtonIndex: Integer read FButtonIndex write SetButtonIndex;
    property SnapRect: TRect read GetSnapRect;
    property OnMove: TNotifyEvent read FOnMove write FOnMove;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

type
  TNamedFastBitmap = record
    Name: string;
    Bitmap: TFastBitmap;
  end;

  TNamedFastBitmaps = array of TNamedFastBitmap;

procedure DefineSnapGrabberImages(Images: TNamedFastBitmaps);

procedure WriteLog(const S: string);


implementation

uses
  StrTools;

const
  MinSize = 40;
  ArrowOffset = 6;
  IconBorder = 1;

var
  GrabberImages: TNamedFastBitmaps;

procedure DefineSnapGrabberImages(Images: TNamedFastBitmaps);
begin
  GrabberImages := Images;
end;

{ TSnapWindow }

type
  TSnapWindow = class(TUtilityWindow)
  protected
    procedure CreateInfo(var Info: TCreateInfo); override;
  end;

procedure TSnapWindow.CreateInfo(var Info: TCreateInfo);
begin
  inherited CreateInfo(Info);
  Info.WndClass.style := CS_HREDRAW or CS_VREDRAW;
  Info.W := 200;
  Info.H := 150;
  Info.Style := WS_POPUP;
  Info.ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
end;

{ TSnapGrabber }

constructor TSnapGrabber.Create;
begin
  inherited Create;
  FSnapWindow := TSnapWindow.Create(Self);
end;

destructor TSnapGrabber.Destroy;
begin
  FSnapWindow.Free;
  DestroyFastBitmap(FBitmap);
  inherited Destroy;
end;

procedure TSnapGrabber.Popup(const Rect: TRect);
begin
  FRect := Rect;
  Show;
end;

procedure TSnapGrabber.Click;
begin
  PostMessage(FSnapWindow.Handle, CM_CLICK, 0, 0);
end;

procedure TSnapGrabber.Show;
var
  R: TRect;
begin
  R := FRect;
  SetWindowPos(FSnapWindow.Handle, HWND_TOPMOST, R.Left - ArrowOffset,
    R.Top - ArrowOffset, WidthOf(R) + ArrowOffset * 2,
    HeightOf(R) + ArrowOffset * 2, SWP_SHOWWINDOW or SWP_NOACTIVATE);
  UpdateBitmap(WidthOf(R) + ArrowOffset * 2, HeightOf(R) + ArrowOffset * 2);
  FTimerCount := 0;
  SetForegroundWindow(FSnapWindow.Handle);
  SetFocus(FSnapWindow.Handle);
  SetTimer(FSnapWindow.Handle, 1, 10, nil);
end;

procedure TSnapGrabber.Hide;
begin
  ShowWindow(FSnapWindow.Handle, SW_HIDE);
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

function TSnapGrabber.GetHandle: THandle;
begin
  Result := FSnapWindow.Handle;
end;

function TSnapGrabber.GetIconRect(Width: Integer = 0; Height: Integer = 0): TRect;
const
  IconWidth = 32;
var
  W, H: Integer;
begin
  GetClientRect(FSnapWindow.Handle, Result);
  if Width <> 0 then
    Result.Right := Width;
  if Height <> 0 then
    Result.Bottom := Height;
  W := IconWidth + IconBorder * 2;
  H := IconWidth + IconBorder * 2;
  Result.Left := (Result.Right - W) shr 1;
  Result.Right := Result.Left + W - 1;
  Result.Top := (Result.Bottom - H) shr 1;
  Result.Bottom := Result.Top + H - 1;
end;

function TSnapGrabber.GetSnapRect: TRect;
begin
  GetWindowRect(FSnapWindow.Handle, Result);
  InflateRect(Result, -ArrowOffset, -ArrowOffset);
end;

procedure DrawSizeRect(G: IGdiGraphics; Rect: TRectF);
var
  C: TArgb;
  P: IGdiPen;
  B: IGdiBrush;
  S: IGdiGraphicsPath;
  A: Single;
begin
  GdiPushMatrix(G);
  G.ResetTransform;
  G.TranslateTransform(Rect.X, Rect.Y);
  C := NewColor(clHighlight);
  P := NewPen(C, 1.5);
  C := NewColor(clSelectedBorder, $80);
  B := NewSolidBrush(C);
  S := NewChamfer(Rect.Width, Rect.Height, 13);
  GdiDrawPath(G, P, B, S);
  SetOpacity(C, $FF);
  B := NewSolidBrush(C);
  S := NewArrow(10, 4, 0.5);
  A := 0;
  GdiDrawPath(G, P, B, S);
  A := A + 45;
  GdiDrawPath(G, P, B, S, Rect.Width / 2, -ArrowOffset, A);
  A := A + 45;
  GdiDrawPath(G, P, B, S, Rect.Width, 0, A);
  A := A + 45;
  GdiDrawPath(G, P, B, S, Rect.Width + ArrowOffset, Rect.Height / 2, A);
  A := A + 45;
  GdiDrawPath(G, P, B, S, Rect.Width, Rect.Height, A);
  A := A + 45;
  GdiDrawPath(G, P, B, S, Rect.Width / 2, Rect.Height + ArrowOffset, A);
  A := A + 45;
  GdiDrawPath(G, P, B, S, 0, Rect.Height, A);
  A := A + 45;
  GdiDrawPath(G, P, B, S, -ArrowOffset, Rect.Height / 2, A);
  GdiPopMatrix(G);
end;

procedure DrawButton(G: IGdiGraphics; Rect: TRectF; State: TDrawState);
var
  C: TArgb;
  P: IGdiPen;
  B: IGdiSolidBrush;
  S: IGdiGraphicsPath;
begin
  P := nil;
  B := nil;
  if dsHot in State then
    if dsPressed in State then
    begin
      P := NewPen(NewColor(clHighlight), 1.5);
      B := NewSolidBrush(NewColor(clSelectedBorder));
    end
    else
    begin
      B := NewSolidBrush(NewColor(clHighlight));
    end
  else
  begin
    C := NewColor(Blend(clHighlight, clWindow, 66));
    B := NewSolidBrush(C);
  end;
  S := NewRoundRect(Rect, 5);
  GdiDrawPath(G, P, B, S);
end;

procedure TSnapGrabber.UpdateBitmap(Width: Integer = 0; Height: Integer = 0);

  procedure DrawInfoHint(Graphics: IGdiGraphics; Rect: TRectF; const Text: string);
  var
    F: IGdiFont;
    S: TSizeF;
    P: IGdiGraphicsPath;
    R: TRect;
  begin
    F := NewFont(Screen.IconFont.Handle);
    S := GdiMeasureText(Graphics, F, Text);
    Rect.Y := Rect.Y + Rect.Height + 8;
    Rect.Height := Round(S.Height * 1.2);
    Rect.X := Round(Rect.X + Rect.Width / 2 - S.Width / 2) - 4;
    Rect.Width := Round(S.Width) + 8;
    if Rect.X < ArrowOffset + IconBorder then Exit;
    if FBitmap.Height - (Rect.Y + Rect.Height) < ArrowOffset * 2 then Exit;
    P := NewRoundRect(Rect, 3);
    Graphics.FillPath(NewSolidBrush(NewColor(Blend(clHighlight, clWindow, 40))), P);
    Graphics.DrawPath(NewPen(NewColor(Blend(clHighlight, clWindow, 60))), P);
    GdiDrawText(Graphics, F, NewColor(0), Text, Rect, drCenter);
    R.Left := Round(Rect.X) + 2;
    R.Top := Round(Rect.Y) + 2;
    R.Right := Round(Rect.X + Rect.Width) - 2;
    R.Bottom := Round(Rect.Y + Rect.Height) - 2;
    // AlphaRect(FBitmap, R);
  end;

var
  G: IGdiGraphics;
  R: TRect;
  F: TRectF;
  S: string;
begin
  GetClientRect(FSnapWindow.Handle, R);
  if Width <> 0 then
    R.Right := Width;
  if Height <> 0 then
    R.Bottom := Height;
  if FButtonChanged or (R.Right <> FBitmap.Width) or (R.Bottom <> FBitmap.Height) then
  begin
    DestroyFastBitmap(FBitmap);
    FBitmap := CreateFastBitmap(WidthOf(R), HeightOf(R), pd32);
    FillRect(FBitmap.DC, R, COLOR_HIGHLIGHT);
    G := NewGraphics(FBitmap.DC);
    G.Clear(0);
    F := NewRectF(6, 6, FBitmap.Width - 13, FBitmap.Height - 13);
    DrawSizeRect(G, F);
    F := NewRectF(GetIconRect(FBitmap.Width, FBitmap.Height));
    DrawButton(G, F, FState);
    S := StringReplace(FWindowHint, '%w', IntToStr(FBitmap.Width), []);
    S := StringReplace(S, '%h', IntToStr(FBitmap.Height), []);
    if FButtonIndex > Length(GrabberImages) - 1 then
      FButtonIndex := Length(GrabberImages) - 1;
    if FButtonIndex > -1 then
      S := GrabberImages[FButtonIndex].Name + S;
    if S <> '' then
      DrawInfoHint(G, F, S);
    AlphaDraw(FBitmap.DC, (FBitmap.Width - GrabberImages[FButtonIndex].Bitmap.Width) div 2,
      (FBitmap.Height - GrabberImages[FButtonIndex].Bitmap.Height) div 2,
      GrabberImages[FButtonIndex].Bitmap);
    UpdateAlphaWindow(FSnapWindow.Handle, FBitmap);
  end;
end;

procedure TSnapGrabber.SetButtonIndex(Value: Integer);
begin
  if Value > Length(GrabberImages) - 1 then
    Value := Length(GrabberImages) - 1;
  if Value < 0 then
    Value := -1;
  if Value <> FButtonIndex then
  begin
    FButtonIndex := Value;
    FButtonChanged := True;
    UpdateBitmap;
    FButtonChanged := False;
  end;
end;

procedure TSnapGrabber.CMClick(var Message: TMessage);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
  Hide;
end;

procedure TSnapGrabber.WMTimer(var Message: TWMTimer);
begin
  inherited;
  if FTimerCount > 2 then
  begin
    KillTimer(FSnapWindow.Handle, 1);
    FTimerCount := 0;
  end;
  SetForegroundWindow(FSnapWindow.Handle);
  SetFocus(FSnapWindow.Handle);
end;

procedure TSnapGrabber.WMKeyDown(var Message: TWMKeyDown);
var
  R: TRect;
  Shift: Boolean;
begin
  inherited;
  case Message.CharCode of
    Ord('1')..Ord('9'): ButtonIndex := Message.CharCode - Ord('1');
    $61..$69: ButtonIndex := Message.CharCode - $61;
    VK_LEFT..VK_DOWN:
      begin
        GetWindowRect(FSnapWindow.Handle, R);
        Shift := GetKeyState(VK_SHIFT) < 0;
        if Shift then
          case Message.CharCode of
            VK_LEFT:
              begin
                Dec(R.Right);
                if WidthOf(R) < MinSize then Exit;
              end;
            VK_UP:
              begin
                Dec(R.Bottom);
                if HeightOf(R) < MinSize then Exit;
              end;
            VK_RIGHT: Inc(R.Right);
            VK_DOWN: Inc(R.Bottom);
          end
        else
          case Message.CharCode of
            VK_LEFT: OffsetRect(R, -1, 0);
            VK_UP: OffsetRect(R, 0, -1);
            VK_RIGHT: OffsetRect(R, 1, 0);
            VK_DOWN: OffsetRect(R, 0, 1);
          end;
        SetWindowPos(FSnapWindow.Handle, 0, R.Left,
          R.Top, WidthOf(R), HeightOf(R), SWP_NOZORDER or SWP_NOACTIVATE);
      end;
    VK_ESCAPE: Hide;
    VK_SPACE, VK_RETURN: Click;
  end;
end;

procedure TSnapGrabber.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  Message.MinMaxInfo.ptMinTrackSize.X := MinSize;
  Message.MinMaxInfo.ptMinTrackSize.Y := MinSize;
  Message.Result := 0;
end;

procedure TSnapGrabber.WMNCHitTest(var Message: TWMNCHitTest);
const
  GripSize = 20;
var
  Rect, A, B: TRect;
  P: TPoint;
  I, X, Y: Integer;
begin
  GetWindowRect(FSnapWindow.Handle, Rect);
  P := SmallPointToPoint(Message.Pos);
  P.X := P.X - Rect.Left;
  P.Y := P.Y - Rect.Top;
  OffsetRect(Rect, -Rect.Left, -Rect.Top);
  I := GripSize;
  if PtInRect(GetIconRect, P) then
  begin
    Message.Result := HTCLIENT;
    Exit;
  end;
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
        Message.Result := 9 + Y * 3 + X;
        Exit;
      end;
    end;
  end;
  Message.Result := HTCAPTION;
end;

procedure TSnapGrabber.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  UpdateBitmap(Message.WindowPos.cx, Message.WindowPos.cy);
  Message.Result := 0;
  if IsWindowVisible(FSnapWindow.Handle) then
    if Assigned(FOnMove) then FOnMove(Self);
end;

procedure TSnapGrabber.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  if not FCaptured then
    SetCapture(FSnapWindow.Handle);
  FCaptured := True;
  Include(FState, dsPressed);
  DestroyFastBitmap(FBitmap);
  UpdateBitmap;
end;

procedure TSnapGrabber.WMLButtonUp(var Message: TWMLButtonUp);
var
  P: TPoint;
begin
  ReleaseCapture;
  FCaptured := False;
  if dsPressed in FState then
  begin
    P := Point(Message.XPos, Message.YPos);
    if PtInRect(GetIconRect, P) then
      Click;
    Exclude(FState, dsPressed);
    DestroyFastBitmap(FBitmap);
    UpdateBitmap;
  end;
end;

procedure TSnapGrabber.WMMouseMove(var Message: TWMMouseMove);
var
  Hit: Boolean;
  P: TPoint;
begin
  if not (dsPressed in FState) then
  begin
    SetCapture(FSnapWindow.Handle);
    FCaptured := True;
  end;
  P := Point(Message.XPos, Message.YPos);
  Hit := PtInRect(GetIconRect, P);
  if Hit then
    if not (dsHot in FState) then
    begin
      Include(FState, dsHot);
      DestroyFastBitmap(FBitmap);
      UpdateBitmap;
    end;
  if not Hit then
  begin
    if dsHot in FState then
    begin
      Exclude(FState, dsHot);
      DestroyFastBitmap(FBitmap);
      UpdateBitmap;
    end;
    if not (dsPressed in FState) then
    begin
      ReleaseCapture;
      FCaptured := False;
    end;
  end;
end;

var
  LogCreated: Boolean;

procedure WriteLog(const S: string);
const
  LogName = 'c:\snapshop.log';
begin
  if FindSwitch('log') then
  begin
    if LogCreated then
      FileWriteLn(LogName, S)
    else
      FileWriteString(LogName, S + #13#10);
    LogCreated := True;
  end;
end;

end.
