
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  Private Build Release December 2008                 *)
(*                                                      *)
(********************************************************)

unit ProgressCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ImgList,
  BaseTypes, GraphTools, GdiPlus, GdiIntf;

{ TBasicProgress }

type
  TBasicProgress = class(TGraphicControl)
  private
    FActive: Boolean;
    procedure SetActive(Value: Boolean);
  protected
    procedure PaintBuffer(Bitmap: TFastBitmap); virtual;
    procedure UpdateProgress; virtual;
    property Active: Boolean read FActive write SetActive;
  public
    destructor Destroy; override;
  end;

{ TSpinProgress }

  TSpinProgress = class(TBasicProgress)
  private
    FStep: Integer;
    FForeground: TColor;
  protected
    procedure Paint; override;
    procedure PaintBuffer(Bitmap: TFastBitmap); override;
    procedure UpdateProgress; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Active;
    property Align;
    property Anchors;
    property Foreground: TColor read FForeground write FForeground;
  end;

{ TIndeterminateProgress }

  TProgressStatus = (psNone, psReady, psBusy, psError, psCustom);

  TIndeterminateProgress = class(TBasicProgress)
  private
    FBeam: Boolean;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageChangeLink: TChangeLink;
    FStatus: TProgressStatus;
    FOnStatusChange: TNotifyEvent;
    procedure SetBeam(Value: Boolean);
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
    procedure ImageListChange(Sender: TObject);
    procedure SetStatus(const Value: TProgressStatus);
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure PaintBuffer(Bitmap: TFastBitmap); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Beam: Boolean read FBeam write SetBeam default False;
    property Images: TCustomImageList read FImages write SetImages default nil;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Status: TProgressStatus read FStatus write SetStatus;
    property OnStatusChange: TNotifyEvent read FOnStatusChange write FOnStatusChange;
    property Anchors;
    property Align;
    property Caption;
    property Enabled;
    property Font;
    property Hint;
    property ParentShowHint;
    property ParentFont;
    property Visible;
  end;

  TProgressMeasureEvent = procedure(Sender: TObject; var Bounds: TRect) of object;

  TProgressDrawParams = record
    Step: Integer;
    Interval: Cardinal;
    Opacity: Byte
  end;

  TProgressDrawEvent = procedure(Sender: TObject; const Bitmap: TFastBitmap;
    var Params: TProgressDrawParams) of object;

  TDrawProgress = class(TComponent)
  private
    FActive: Boolean;
    FThread: TThread;
    FRefCount: Integer;
    FOnDraw: TProgressDrawEvent;
    FOnMeasure: TProgressMeasureEvent;
    procedure SetActive(Value: Boolean);
  protected
    function Measure(var Bounds: TRect): Boolean;
    function Draw(const Bitmap: TFastBitmap; var Params: TProgressDrawParams): Boolean;
  public
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    property Active: Boolean read FActive write SetActive;
    property RefCount: Integer read FRefCount;
  published
    property OnDraw: TProgressDrawEvent read FOnDraw write FOnDraw;
    property OnMeasure: TProgressMeasureEvent read FOnMeasure write FOnMeasure;
  end;

implementation

{ TIndeterminateProgress }

{$R PROGRESS.RES}

var
  InternalProgressThread: TObject;

type
  TProgressThread = class(TThread)
  private
    FReadyImages: TAlphaImage;
    FBusyImages: TAlphaImage;
    FErrorImages: TAlphaImage;
    FControls: TList;
    FIndexer: Integer;
    procedure UpdateControls;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Progress: TBasicProgress);
    procedure Remove(Progress: TBasicProgress);
  end;

constructor TProgressThread.Create;
begin
  FControls := TList.Create;
  FErrorImages := TAlphaImage.Create;
  FErrorImages.LoadFromResourceID(3003);
  FBusyImages := TAlphaImage.Create;
  FBusyImages.LoadFromResourceID(3001);
  FReadyImages := TAlphaImage.Create;
  FReadyImages.LoadFromResourceID(3002);
  inherited Create(False);
end;

destructor TProgressThread.Destroy;
begin
  inherited Destroy;
  FReadyImages.Free;
  FBusyImages.Free;
  FErrorImages.Free;
  FControls.Free;
end;

procedure TProgressThread.UpdateControls;
var
  I: Integer;
begin
  for I := 0 to FControls.Count - 1 do
    TBasicProgress(FControls[I]).UpdateProgress;
end;

procedure TProgressThread.Execute;
begin
  FreeOnTerminate := True;
  while not Terminated do
  begin
    FIndexer := (FIndexer + 1) mod 12;
    if FControls.Count > 0 then
      Synchronize(UpdateControls);
    Sleep(50);
  end;
end;

procedure TProgressThread.Add(Progress: TBasicProgress);
begin
  FControls.Add(Progress);
end;

procedure TProgressThread.Remove(Progress: TBasicProgress);
begin
  FControls.Remove(Progress);
end;

function ProgressThread: TProgressThread;
begin
  if InternalProgressThread = nil then
    InternalProgressThread := TProgressThread.Create;
  Result := TProgressThread(InternalProgressThread);
end;

destructor TBasicProgress.Destroy;
var
  Thread: TProgressThread;
begin
  if InternalProgressThread <> nil then
  begin
    Thread := TProgressThread(InternalProgressThread);
    Thread.Remove(Self);
    if Thread.FControls.Count = 0 then
    begin
      Thread.Terminate;
      InternalProgressThread := nil;
    end;
  end;
  inherited Destroy;
end;

procedure TBasicProgress.PaintBuffer(Bitmap: TFastBitmap);
begin
end;

procedure TBasicProgress.UpdateProgress;
begin
  if FActive then
    Paint;
end;

procedure TBasicProgress.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if FActive then
      ProgressThread.Add(Self)
    else
      ProgressThread.Remove(Self);
    Invalidate;      
  end;
end;

{ TSpinProgress }

constructor TSpinProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForeground := clBtnShadow;
end;

procedure TSpinProgress.Paint;
var
  B: TFastBitmap;
  S: Integer;
begin
  if Active then
  begin
    S := Width;
    if Height < S then S := Height;
    if S < 1 then Exit;
    B := CreateFastBitmap(S, S, pd32);
    try
      PaintBuffer(B);
      if Height > S then
        BaseTypes.AlphaDraw(Canvas.Handle, 0, (Height - S) shr 1, B)
      else
        BaseTypes.AlphaDraw(Canvas.Handle, (Width - S) shr 1, 0, B);
    finally
      DestroyFastBitmap(B);
    end;
  end;
  if csDesigning in ComponentState then
    FillRectOutline(Canvas.Handle, ClientRect, clBtnShadow, psDot);
end;

procedure DrawSpinProgress(const Bitmap: TFastBitmap; Step: Integer; Background, Foreground: TColor);
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
  G.Clear(NewColor(Background));
  R := NewRectF(0, 0, Bitmap.Width, Bitmap.Height);
  B := NewSolidBrush(NewColor(0, $FF));
  L := NewLinearGradientBrush(R, 60, NewColor(Foreground, $F0), NewColor(Foreground, $00));
  O := NewRoundRect(R, 20);
  G.FillPath(L, O);
  R := NewRectF(4, 4, Bitmap.Width - 8, Bitmap.Height - 8);
  O := NewRoundRect(R, 18);
  P := NewPen(NewColor(Foreground, $80), 6);
  G.DrawPath(P, O);
  R := NewRectF(0, 0, Bitmap.Width, Bitmap.Height);
  P := NewPen(NewColor(Foreground, $A0), 4);
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

procedure TSpinProgress.PaintBuffer(Bitmap: TFastBitmap);
begin
  DrawSpinProgress(Bitmap, FStep, Color, FForeground);
end;

procedure TSpinProgress.UpdateProgress;
begin
  Inc(FStep);
  inherited UpdateProgress;
end;

{ TIndeterminateProgress }

constructor TIndeterminateProgress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csSetCaption];
  Height := 24;
  Width := 100;
  FStatus := psReady;
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TIndeterminateProgress.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TIndeterminateProgress.PaintBuffer(Bitmap: TFastBitmap);
const
  TextOffset = 28;
var
  DC: HDC;
  P: TProgressThread;
  A: TAlphaImage;
  I, X, Y: Integer;
  R: TRect;
  O: Byte;
  C: TColor;
begin
  DC := Bitmap.DC;
  R := ClientRect;
  FillRectColor(DC, R, Color);
  P := ProgressThread;
  I := 0;
  case FStatus of
    psReady: A := P.FReadyImages;
    psBusy:
      begin
        A := P.FBusyImages;
        I := P.FIndexer;
      end;
    psError:
      A := P.FErrorImages;
  else
    A := nil;
  end;
  X := 0;
  if A <> nil then
  begin
    Y := (Height - A.Height) div 2;
    if Enabled then
      O := $FF
    else
      O := $90;
    A.Blit(DC, X, Y, I, O);
    R.Left := TextOffset;
  end
  else if (FImages <> nil) and (FImageIndex > -1) then
  begin
    //FImages.Draw();
  end;
  DrawCaption(DC, Caption, R, drLeft, Enabled);
  if FBeam then
  begin
    R.Left := R.Left + FontWidth(DC, Caption) + 4;
    R.Top := R.Top + HeightOf(R) div 2;
    R.Bottom := R.Top + 1;
    C := clHighlight;
    if not Enabled then
      C := clBtnShadow;
    if R.Left < R.Right then
      DrawGradient(DC, R, C, Color, drRight);
  end;
end;

procedure TIndeterminateProgress.Paint;
var
  B: TFastBitmap;
  F: HFONT;
begin
  inherited Paint;
  B := CreateFastBitmap(Width, Height, pd24);
  F := SelectObject(B.DC, Font.Handle);
  try
    PaintBuffer(B);
    BlitDraw(Canvas.Handle, 0, 0, B);
  finally
    SelectObject(B.DC, F);
    DestroyFastBitmap(B);
  end;
end;

procedure TIndeterminateProgress.SetBeam(Value: Boolean);
begin
  if Value <> FBeam then
  begin
    FBeam := Value;
    Invalidate;
  end;
end;

procedure TIndeterminateProgress.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

procedure TIndeterminateProgress.ImageListChange(Sender: TObject);
begin
  if Status = psCustom then
    Invalidate;
end;

procedure TIndeterminateProgress.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    if Status = psCustom then
      Invalidate;
  end;
end;

procedure TIndeterminateProgress.SetImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    if Status = psCustom then
      Invalidate;
  end;
end;

procedure TIndeterminateProgress.SetStatus(const Value: TProgressStatus);
begin
  if FStatus <> Value then
  begin
    if FStatus = psBusy then
      Active := False;
    FStatus := Value;
    if Assigned(FOnStatusChange) then
      FOnStatusChange(Self);
    if FStatus = psBusy then
      Active := True;
    Invalidate;
  end;
end;

procedure TIndeterminateProgress.CMTextChanged(var Msg: TMessage);
begin
	inherited;
  Invalidate;
end;

{ TDrawProgress }

type
  TDrawThread = class(TThread)
  private
    FProgress: TDrawProgress;
    FWindowClass: string;
    FWindow: HWND;
    { Window Messages }
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCLButtonDown(var Msg: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
  protected
    procedure Execute; override;
  public
    constructor Create(Progress: TDrawProgress);
  end;

constructor TDrawThread.Create(Progress: TDrawProgress);
begin
  FProgress := Progress;
  inherited Create(False);
end;

function DrawThreadProc(Wnd: HWND; Msg: Cardinal; wParam, lParam: Integer): Integer; stdcall;
var
  O: TObject;
  M: TMessage;
begin
  if Msg = WM_CREATE then
  begin
    O := TObject(PCreateStruct(lParam).lpCreateParams);
    SetWindowLong(Wnd, GWL_USERDATA, Integer(O));
  end
  else
    O := TObject(GetWindowLong(Wnd, GWL_USERDATA));
  if O <> nil then
  begin
    M.Msg := Msg;
    M.wParam := wParam;
    M.lParam := lParam;
    M.Result := -1;
    O.Dispatch(Msg);
    if M.Result = -1 then
      Result := DefWindowProc(Wnd, Msg, wParam, lParam)
    else
      Result := M.Result;
  end
  else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
end;

procedure UpdateAlphaWindow(Wnd: HWND; const Bitmap: TFastBitmap; Opacity: Byte = $FF);
var
	Blend: TBlendFunction;
  Rect: TRect;
	P1, P2: TPoint;
  S: TSize;
  DC: HDC;
begin
	SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
	GetWindowRect(Wnd, Rect);
	P1.X := Rect.Left;
	P1.Y := Rect.Top;
	with Blend do
	begin
		BlendOp := AC_SRC_OVER;
		BlendFlags := 0;
		SourceConstantAlpha := Opacity;
		AlphaFormat := AC_SRC_ALPHA;
	end;
	DC := GetDC(0);
	P2 := Point(0, 0);
	S.cx := Bitmap.Width;
	S.cy := Bitmap.Height;
	UpdateLayeredWindow(Wnd, DC, @P1, @S, Bitmap.DC,
		@P2, 0, @Blend, ULW_ALPHA);
	ReleaseDC(0, DC);
end;

procedure TDrawThread.Execute;
var
  WndClass: TWndClass;
  Msg: TMsg;
  Drawn: Boolean;
  R: TRect;
  B: TFastBitmap;
  P: TProgressDrawParams;
  A1, A2: Cardinal;
begin
  FillChar(R, SizeOf(R), #0);
  if not FProgress.Measure(R) then Exit;
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;
  B := CreateFastBitmap(R.Right - R.Left, R.Bottom - R.Top, pd32);
  try
    FWindowClass := 'ProgressWindow' + IntToStr(Handle);
    FillChar(WndClass, SizeOf(TWndClass), #0);
    WndClass.lpszClassName := PChar(FWindowClass);
    WndClass.style := 0;
    WndClass.lpfnWndProc := @DrawThreadProc;
    if Windows.RegisterClass(WndClass) <> 0 then
    try
      FWindow := CreateWindowEx(WS_EX_TOOLWINDOW or WS_EX_TOPMOST or WS_EX_NOACTIVATE or WS_EX_TRANSPARENT ,
        PChar(FWindowClass), nil, WS_POPUP or WS_DISABLED, R.Left, R.Top,
        B.Width, B.Height, 0, 0, 0, Self);
      UpdateAlphaWindow(FWindow, B, P.Opacity);
      ShowWindow(FWindow, SW_SHOWNOACTIVATE);
      FillChar(P, SizeOf(P), #0);
      P.Opacity := $FF;
      while not Terminated do
      begin
        A1 := GetTickCount;
        Drawn := FProgress.Draw(B, P);
        if Drawn then
          UpdateAlphaWindow(FWindow, B, P.Opacity);
        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
        A2 := GetTickCount;
        if A1 > A2 then
          A1 := A2;
        A1 := P.Interval - (A2 - A1);
        if A1 > 1000 then
          A1 := 1000;
        Sleep(A1);
      end;
      DestroyWindow(FWindow);
    finally
      Windows.UnregisterClass(PChar(FWindowClass), 0);
    end;
  finally
    DestroyFastBitmap(B);
  end;
end;

procedure TDrawThread.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
end;

procedure TDrawThread.WMMouseActivate(var Msg: TWMMouseActivate);
begin
  Msg.Result := MA_NOACTIVATE;
end;

procedure TDrawThread.WMNCLButtonDown(var Msg: TWMNCLButtonDown);
begin
  Msg.Result := 0;
end;

{ TDrawProgress }

destructor TDrawProgress.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TDrawProgress.Enter;
begin
  Inc(FRefCount);
  if FRefCount > 0 then
    Active := True;
end;

procedure TDrawProgress.Leave;
begin
  Dec(FRefCount);
  if FRefCount < 1 then
    Active := False;
end;

function TDrawProgress.Measure(var Bounds: TRect): Boolean;
begin
  Result := Assigned(FOnMeasure);
  if Result then
    FOnMeasure(Self, Bounds);
end;

function TDrawProgress.Draw(const Bitmap: TFastBitmap; var Params: TProgressDrawParams): Boolean;
begin
  Result := Assigned(FOnMeasure);
  if Result then
    FOnDraw(Self, Bitmap, Params);
end;

procedure TDrawProgress.SetActive(Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    if not (csDesigning in ComponentState) then
      if FActive then
        FThread := TDrawThread.Create(Self)
      else
        FreeAndNil(FThread);
  end;
end;

end.
