
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2008                   *)
(*                                                      *)
(********************************************************)

unit SlideCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, MathTools,
  GraphTools, BaseTypes, BtnEdit, PopCtrls;

{ TSlideBar }

type
  TSlideBarKind = (sbVertical, sbHorizontal);
  TFormatTextEvent = procedure(Sender: TObject; var Text: string) of object;

  TCustomSlideBar = class(TGraphicControl)
  private
  	FChanged: Boolean;
  	FKind: TSlideBarKind;
    FMin: Double;
    FMax: Double;
    FStep: Double;
    FTracking: Boolean;
    FPosition: Double;
    FAssociate: TControl;
    FDrawState: TDrawState;
    FMoving: Boolean;
    FOnChange: TNotifyEvent;
    FOnDrawBackground: TDrawStateEvent;
    FOnDrawThumb: TDrawStateEvent;
    FOnFormat: TFormatTextEvent;
    procedure SetAssociate(Value: TControl);
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetPosition(Value: Double);
    procedure SetKind(Value: TSlideBarKind);
    procedure SetDrawState(Value: TDrawState);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    function GetGripRect: TRect;
    procedure Change; dynamic;
    procedure DoChange; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property DrawState: TDrawState read FDrawState write SetDrawState;
    property Associate: TControl read FAssociate write SetAssociate;
    property Kind: TSlideBarKind read FKind write SetKind;
    property Min: Double read FMin write SetMin;
    property Max: Double read FMax write SetMax;
    property Step: Double read FStep write FStep;
    property Moving: Boolean read FMoving;
    property Tracking: Boolean read Ftracking write FTracking default True;
    property Position: Double read FPosition write SetPosition;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawBackground: TDrawStateEvent read FOnDrawBackground write FOnDrawBackground;
    property OnDrawThumb: TDrawStateEvent read FOnDrawThumb write FOnDrawThumb;
    property OnFormat: TFormatTextEvent read FOnFormat write FOnFormat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ForcePaint;
  end;

  TSlideBar = class(TCustomSlideBar)
  public
    property Canvas;
    property Moving;
  published
    property Align;
    property Associate;
    property Anchors;
    property Enabled;
    property Kind;
    property Min;
    property Max;
    property Step;
    property Tracking;
    property Position;
    property OnChange;
    property OnDrawBackground;
    property OnDrawThumb;
    property OnFormat;
  end;

  TPopupSlideForm = class(TCustomPopupForm)
  private
    FSlide: TSlideBar;
  public
    constructor Create(AOwner: TComponent); override;
    property Slide: TSlideBar read FSlide;
  end;

  TCustomSlideEdit = class(TCustomButtonEdit)
  private
    FPopup: TPopupSlideForm;
    FPopped: Boolean;
    FJustPopped: Boolean;
    FChanging: Boolean;
    FFormatted: Boolean;
    FOnFormat: TFormatTextEvent;
    FOnValueChange: TNotifyEvent;
    function PopupValid: Boolean;
    function GetMin: Double;
    procedure SetMin(const Value: Double);
    function GetMax: Double;
    procedure SetMax(const Value: Double);
    function GetPosition: Double;
    procedure SetPosition(const Value: Double);
    function GetStep: Double;
    procedure SetStep(const Value: Double);
    function GetOnDrawBackground: TDrawStateEvent;
    procedure SetOnDrawBackground(Value: TDrawStateEvent);
    function GetOnDrawThumb: TDrawStateEvent;
    procedure SetOnDrawThumb(Value: TDrawStateEvent);
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
  protected
    procedure DoButtonPress; override;
    procedure DoChange; override;
    procedure DoFormat(Sender: TObject; var Text: string); dynamic;
    procedure DoValueChange(Sender: TObject); dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    property Popped: Boolean read FPopped;
    property Min: Double read GetMin write SetMin;
    property Max: Double read GetMax write SetMax;
    property Position: Double read GetPosition write SetPosition;
    property Step: Double read GetStep write SetStep;
    property OnDrawBackground: TDrawStateEvent read GetOnDrawBackground write SetOnDrawBackground;
    property OnDrawThumb: TDrawStateEvent read GetOnDrawThumb write SetOnDrawThumb;
    property OnFormat: TFormatTextEvent read FOnFormat write FOnFormat;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TSlideEdit }

  TSlideEdit = class(TCustomSlideEdit)
  published
    property Min;
    property Max;
    property Position;
    property Step;
    property OnDrawBackground;
    property OnDrawThumb;
    property OnFormat;
    property OnValueChange;
  end;

implementation

{ TCustomSlideBar }

constructor TCustomSlideBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  Height := 100;
  Width := 50;
  FMax := 100;
  FMin := 0;
  FStep := 1;
  FTracking := True;
  if not Enabled then
	  FDrawState := [dsDisabled];
end;

destructor TCustomSlideBar.Destroy;
begin
  FOnChange := nil;
  FOnDrawBackground := nil;
  FOnDrawThumb := nil;
  FOnFormat := nil;
	Associate := nil;
  inherited Destroy;
end;

function TCustomSlideBar.GetGripRect: TRect;
var
  X, Y: Integer;
begin
	if FKind = sbVertical then
  begin
    X := Width div 2;
    Y := Height - (System.Round(Abs(Position - Min) * ((Height - 10) / (FMax - FMin))) + 5);
    Result := ClientRect;
    Result.Left := X - 5;
    Result.Right := X + 5;
    Result.Top := Y - 5;
    Result.Bottom := Y + 5;
    InflateRect(Result, 8, 4);
	end
  else
  begin
    X := (System.Round(Abs(Position - Min) * ((Width - 10) / (FMax - FMin))) + 5);
    Y := Height div 2;
    Result := ClientRect;
    Result.Left := X - 5;
    Result.Right := X + 5;
    Result.Top := Y - 5;
    Result.Bottom := Y + 5;
    InflateRect(Result, 4, 8);
  end;
end;

procedure TCustomSlideBar.Change;
begin
	FChanged := True;
  if FTracking then
  	DoChange;
end;

procedure TCustomSlideBar.DoChange;
begin
	if FChanged then
	  if Assigned(FOnChange) then FOnChange(Self);
	FChanged := False;    
end;

procedure TCustomSlideBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
	if (Operation = opRemove) and (FAssociate = AComponent) then
  	Associate := nil;
end;

procedure TCustomSlideBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	Range, Delta: Single;
begin
  inherited MouseMove(Shift, X, Y);
  if PtInRect(GetGripRect, Point(X, Y)) then
		DrawState := DrawState + [dsHot]
  else
		DrawState := DrawState - [dsHot];
  if FKind = sbVertical then
  begin
  	Range := Height;
    Delta := Y;
  end
  else
  begin
  	Range := Width;
    Delta := X;
  end;
  if Range = 0 then
  	Range := 0.000001;
  if (dsFocused in DrawState) and (FMax > FMin) then
	  if FKind = sbVertical then
  	  Position := (Range - Delta) / (Range / (FMax - FMin)) + FMin
    else
  	  Position := (Delta / Range) * (FMax - FMin) + FMin;
end;

procedure TCustomSlideBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FMoving := True;
	  DrawState := DrawState + [dsFocused];
  end;
end;

procedure TCustomSlideBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
	  DrawState := DrawState - [dsFocused];
    DoChange;
    FMoving := False;
	end;
end;

procedure TCustomSlideBar.ForcePaint;
begin
  Invalidate;
end;

procedure TCustomSlideBar.Paint;
var
  DC: HDC;
  Rect: TRect;
  B: TFastBitmap;
  C: TCanvas;
  X, Y: Integer;
begin
  Rect := ClientRect;
  if (not Parent.DoubleBuffered) and (Assigned(FOnDrawBackground) or Assigned(FOnDrawThumb)) then
  begin
    B := CreateFastBitmap(Width, Height);
    C := TCanvas.Create;
    C.Handle := B.DC;
  end
  else
    C := Canvas;
  try
    DC := C.Handle;
    if Assigned(FOnDrawBackground) then
      FOnDrawBackground(Self, C, Rect, FDrawState);
    if FKind = sbVertical then
    begin
      X := Width div 2;
      Y := Height - (System.Round(Abs(Position - Min) * ((Height - 10) / (FMax - FMin))) + 5);
      Rect.Left := X - 1;
      Rect.Right := X + 1;
      if not Assigned(FOnDrawBackground) then
        DrawThemeHorzSplit(DC, Rect, FDrawState);
      Rect.Top := Y - 1;
      Rect.Bottom := Y + 1;
      InflateRect(Rect, 8, 4);
      if Assigned(FOnDrawThumb) then
        FOnDrawThumb(Self, C, Rect, FDrawState)
      else
        DrawThemeVertThumb(DC, Rect, FDrawState);
    end
    else
    begin
      X := (System.Round(Abs(Position - Min) * ((Width - 10) / (FMax - FMin))) + 5);
      Y := Height div 2;
      Rect.Top := Y - 1;
      Rect.Bottom := Y + 1;
      if not Assigned(FOnDrawBackground) then
        DrawThemeVertSplit(DC, Rect, FDrawState);
      Rect.Left := X - 1;
      Rect.Right := X + 1;
      InflateRect(Rect, 4, 8);
      if Assigned(FOnDrawThumb) then
        FOnDrawThumb(Self, C, Rect, FDrawState)
      else
        DrawThemeHorzThumb(DC, Rect, FDrawState);
    end;
  finally
    if C <> Canvas then
    begin
      BitBlt(Canvas.Handle, 0, 0, Width, Height, B.DC, 0, 0, SRCCOPY);
      C.Handle := 0;
      C.Free;
      DestroyFastBitmap(B);
    end;
  end;
end;

procedure TCustomSlideBar.SetDrawState(Value: TDrawState);
begin
	if Value <> FDrawState then
  begin
		FDrawState := Value;
    ForcePaint;
  end;
end;

procedure TCustomSlideBar.SetKind(Value: TSlideBarKind);
var
	I: Integer;
begin
	if Value <> FKind then
  begin
  	FKind := Value;
    if csLoading in ComponentState then Exit;
    I := Width;
    Width := Height;
    Height := I;
    ForcePaint;
  end;
end;

procedure TCustomSlideBar.SetAssociate(Value: TControl);
var
  I: Double;
begin
	if FAssociate <> nil then
  	FAssociate.RemoveFreeNotification(Self);
	FAssociate := Value;
	if FAssociate <> nil then
  	FAssociate.FreeNotification(Self);
  if FAssociate <> nil then
  begin
    I := FPosition;
    FPosition := I - 1;
    Position := I;
  end;
end;

procedure TCustomSlideBar.SetMax(const Value: Double);
begin
  if Value <> FMax then
  begin
    if Value < FMin then
      FMax := FMin
    else
      FMax := Value;
    if FPosition > FMax then
      Position := FMax
    else
      ForcePaint;
  end;
end;

procedure TCustomSlideBar.SetMin(const Value: Double);
begin
  if Value <> FMin then
  begin
    if FMax < Value then
      FMin := FMax
    else
      FMin := Value;
    if FPosition < FMin then
      Position := FMin
    else
      ForcePaint;
  end;
end;

procedure InvalidateControlRect(Control: TControl; Rect: TRect);
var
	WinControl: TWinControl absolute Control;
begin
	if csDesigning in Control.ComponentState then
  	Control.Invalidate
	else if not Control.Visible then
  	Exit
	else if (Control is TGraphicControl) and
  	(Control.Parent <> nil) and	Control.Parent.HandleAllocated then
  begin
  	with Control.BoundsRect do
			OffsetRect(Rect, Left, Top);
    InvalidateRect(Control.Parent.Handle, @Rect, True)
  end
  else if (Control is TWinControl) and (WinControl.HandleAllocated) then
    InvalidateRect(WinControl.Handle, @Rect, False)
end;

procedure TCustomSlideBar.SetPosition(Value: Double);
var
  S: string;
begin
  if Value < FMin then
    Value := FMin
  else if Value > FMax then
    Value := FMax;
  if FStep > 0 then
    Value := Divide(Value, FStep);
  if Value <> FPosition then
  begin
    FPosition := Value;
    Change;
    if FAssociate <> nil then
    begin
      S := FloatToStr(FPosition);
      if Assigned(FOnFormat) then
        FOnFormat(Self, S);
      TCustomSlideBar(FAssociate).Text := S;
    end;
  	ForcePaint;
  end;
end;

procedure TCustomSlideBar.CMEnabledChanged(var Message: TMessage);
begin
	inherited;
  if Enabled then
	  DrawState := DrawState - [dsDisabled]
  else
	  DrawState := DrawState + [dsDisabled];
end;

procedure TCustomSlideBar.CMMouseLeave(var Message: TMessage);
begin
	inherited;
  DrawState := DrawState - [dsHot];
end;

{ TPopupSlideForm }

constructor TPopupSlideForm.Create(AOwner: TComponent);
var
  R: TRect;
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  Color := clBtnFace;
  Sizeable := False;
  FSlide := TSlideBar.Create(Self);
  FSlide.Kind := sbHorizontal;
  FSlide.Parent := Self;
  FSlide.Max := 1000;
  R := ClientRect;
  InflateRect(R, -4, -4);
  FSlide.BoundsRect := R;
  FSlide.Anchors := [akLeft, akTop, akRight, akBottom];
end;

{ TCustomSlideEdit }

constructor TCustomSlideEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Glyph := GlyphFind(gkHSpin);
  FPopup := TPopupSlideForm.Create(Self);
  FPopup.Slide.OnFormat := DoFormat;
  FPopup.Slide.OnChange := DoValueChange;
  FPopup.Associate := Self;
  FPopup.Slide.Associate := Self;
  
end;

function TCustomSlideEdit.PopupValid: Boolean;
begin
  Result := FPopup.Slide.Max > FPopup.Slide.Min;
end;

procedure TCustomSlideEdit.DoButtonPress;
var
  P: TPoint;
  A, B: Single;
begin
  inherited DoButtonPress;
  if PopupValid then
  begin
    FPopup.Height := 30;
    FPopup.Width := 150;
    P := ScreenToClient(Mouse.CursorPos);
    A := FPopup.Slide.Position;
    B := (A - FPopup.Slide.Min) / (FPopup.Slide.Max - FPopup.Slide.Min) *
      (FPopup.Slide.Width - 10) + 9;
    FPopup.HorzOffset := Round(P.X - B);
    FJustPopped := True;
    FPopup.Popup;
    FPopped := True;
  end;
end;

procedure TCustomSlideEdit.DoChange;
var
  S: string;
  I: Double;
begin
  if not HandleAllocated then Exit;
  if (not Popped) and (not FFormatted) then
  begin
    FChanging := True;
    S := Trim(Text);
    if S = '.' then S := '';
    I := StrToFloatDef(S, Position);
    Position := I;
    if I > 1000000 then
      I := 1000000
    else if I < -1000000 then
      I := -1000000;
    if I < Min then
    begin
      Text := FloatToStr(I);
      PostMessage(InnerEdit.Handle, EM_SETSEL, High(Word), High(Word));
    end
    else if I > Max then
    begin
      Text := FloatToStr(I);
      PostMessage(InnerEdit.Handle, EM_SETSEL, High(Word), High(Word));
    end;
    FChanging := False;
  end;
end;

procedure TCustomSlideEdit.DoFormat(Sender: TObject; var Text: string);
var
  Slider: TSlideBar absolute Sender;
  S: string;
  I: Integer;
begin
  if FChanging then Exit;
  S := '';
  if Assigned(FOnFormat) then
    FOnFormat(Self, Text);
  if S <> '' then
    Text := S
  else
  begin
    S := Format('%.2f', [Slider.Position]);
    for I := Length(S) downto 1 do
      if S[I] = '0' then
        S[I] := ' '
      else
        Break;
    for I := Length(S) downto 1 do
      if CharInSet(S[I],  [' ', '.']) then
        S[I] := ' '
      else
        Break;
    Text := Trim(S);
  end;
end;

procedure TCustomSlideEdit.DoValueChange(Sender: TObject);
begin
  if Assigned(FOnValueChange) then
    FOnValueChange(Self);
end;

procedure TCustomSlideEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_UP then
  begin
    Position := Position + Step;
    PostMessage(InnerEdit.Handle, EM_SETSEL, 0, 0);
  end
  else if Key = VK_DOWN then
  begin
    Position := Position - Step;
    PostMessage(InnerEdit.Handle, EM_SETSEL, 0, 0);
  end;
end;

procedure TCustomSlideEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  R: Single;
begin
  inherited MouseMove(Shift, X, Y);
  if FPopped and PopupValid then
  begin
    P := ClientToScreen(Point(X, Y));
    P := FPopup.Slide.ScreenToClient(P);
    P.X := P.X - 6;
    R := (P.X / (FPopup.Slide.Width - 10)) * (FPopup.Slide.Max - FPopup.Slide.Min) + FPopup.Slide.Min;
    if not FJustPopped then
      FPopup.Slide.Position := R;
    FJustPopped := False;
  end
end;

procedure TCustomSlideEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and FPopped then
  begin
    FPopup.Cancel;
    FPopped := False;
  end;
end;

function TCustomSlideEdit.GetMin: Double;
begin
  Result := FPopup.Slide.Min;
end;

procedure TCustomSlideEdit.SetMin(const Value: Double);
begin
  FPopup.Slide.Min := Value;
end;

function TCustomSlideEdit.GetMax: Double;
begin
  Result := FPopup.Slide.Max;
end;

procedure TCustomSlideEdit.SetMax(const Value: Double);
begin
  FPopup.Slide.Max := Value;
end;

function TCustomSlideEdit.GetPosition: Double;
begin
  Result := FPopup.Slide.Position;
end;

procedure TCustomSlideEdit.SetPosition(const Value: Double);
begin
  FPopup.Slide.Position := Value;
end;

function TCustomSlideEdit.GetStep: Double;
begin
  Result := FPopup.Slide.Step;
end;

procedure TCustomSlideEdit.SetStep(const Value: Double);
begin
  FPopup.Slide.Step := Value;
end;

function TCustomSlideEdit.GetOnDrawBackground: TDrawStateEvent;
begin
  Result := FPopup.Slide.OnDrawBackground;
end;

procedure TCustomSlideEdit.SetOnDrawBackground(Value: TDrawStateEvent);
begin
  FPopup.Slide.OnDrawBackground := Value;
end;

function TCustomSlideEdit.GetOnDrawThumb: TDrawStateEvent;
begin
  Result := FPopup.Slide.OnDrawThumb;
end;

procedure TCustomSlideEdit.SetOnDrawThumb(Value: TDrawStateEvent);
begin
  FPopup.Slide.OnDrawThumb := Value;
end;

procedure TCustomSlideEdit.WMKillFocus(var Message: TMessage);
var
  S: string;
begin
  inherited;
  S := Trim(Text);
  if (S <> '') and (S[1] = '.') then S := '0' + S;
  Text := S;
  S := Text;
  DoFormat(FPopup.Slide, S);
  Text := S;
end;

end.
