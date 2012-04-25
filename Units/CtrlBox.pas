
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit CtrlBox;

interface

{$I CODEBOT.INC}

uses
	Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls, Graphics,
  BaseTypes, GraphTools, FormTools;

{ TControlBoxButton }

type
  TControlBoxButton = class(TButton)
  private
		procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
  	procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
  end;

{ TControlBoxFrame }

	TControlBox = class;

	TControlBoxFrame = class(TFrame)
	private
  	FControlBox: TControlBox;
    FActive: Boolean;
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CNKeyDown(var Msg: TWMKeyDown); message CN_KEYDOWN;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
	protected
  	procedure ChangeState; virtual;
    procedure WndProc(var Message: TMessage); override;
  	property ControlBox: TControlBox read FControlBox write FControlBox;
	public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
	end;

  TControlBoxFrameClass = class of TControlBoxFrame;

{ TControlBox }

	TControlBox = class(TScrollingWinControl)
  private
    FBackground: TFastBitmap;
    FBorderStyle: TBorderStyle;
    FColorA: TColor;
    FColorB: TColor;
    FFrameCounter: Integer;
    FItemIndex: Integer;
    FItemFrame: TControlBoxFrame;
    FFrames: TList;
    FOnChange: TNotifyEvent;
    procedure PaintBorder;
    function GetActiveFrame: TControlBoxFrame;
    procedure SetActiveFrame(Value: TControlBoxFrame);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetColorA(const Value: TColor);
    procedure SetColorB(const Value: TColor);
    function GetFrame(Index: Integer): TControlBoxFrame;
    function GetFrameCount: Integer;
    procedure SetItemIndex(Value: Integer);
		procedure WMGetDlgCode(var Msg: TMessage); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Msg: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
		procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
	protected
    procedure Delete(Frame: TControlBoxFrame);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  	procedure PaintFrame(Frame: TControlBoxFrame; DC: HDC);
    procedure SelectFrame(Frame: TControlBoxFrame);
    procedure SelectPrior;
    procedure SelectNext;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
    function Add(FrameClass: TControlBoxFrameClass): TControlBoxFrame; overload;
    function Add(FrameClass: TCustomFrameClass): TCustomFrame; overload;
    procedure Remove(Frame: TControlBoxFrame);
    property ActiveFrame: TControlBoxFrame read GetActiveFrame write SetActiveFrame;
    property Frames[Index: Integer]: TControlBoxFrame read GetFrame;
		property FrameCount: Integer read GetFrameCount;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  published
  	property Anchors;
    property Align;
  	property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ColorA: TColor read FColorA write SetColorA default clHighlight;
    property ColorB: TColor read FColorB write SetColorB default clWindow;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property TabOrder;
    property TabStop;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TControlBoxButton }

procedure TControlBoxButton.WMGetDlgCode(var Msg: TMessage);
begin
	inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS;
end;

procedure TControlBoxButton.WMKeyDown(var Msg: TMessage);
begin
	inherited;
  if Parent <> nil then
  	Parent.Perform(CN_KEYDOWN, Msg.WParam, Msg.LParam);
end;

{ TControlBoxFrame }

constructor TControlBoxFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DesktopFont := True;
  TabStop := False;
  VertScrollBar.Visible := False;
  HorzScrollBar.Visible := False;
end;

destructor TControlBoxFrame.Destroy;
begin
	if (FControlBox <> nil) and (not (csDestroying in FControlBox.ComponentState)) then
  	FControlBox.Remove(Self);
	inherited Destroy;
end;

procedure TControlBoxFrame.ChangeState;
const
	Border = 8;
	FontColors: array[Boolean] of TColor = (clWindowText, clHighlightText);
var
	Anchors: TAnchorDataArray;
  Selected: Boolean;
	B: TButton;
  C: TControl;
	L: TLabel;
  X, Y: Integer;
	I: Integer;
begin
	if FControlBox <> nil then
  	Selected := FControlBox.ActiveFrame = Self
	else
  	Selected := False;
	Anchors := SaveAnchors(Self);
  X := Border;
  Y := Border;
	for I := 0 to ControlCount - 1 do
  begin
  	C := Controls[I];
  	if C.Left + C.Width + Border > X then
    	X := C.Left + C.Width + Border;
  	if C is TControlBoxButton then
    begin
			B := TButton(C);
      if FControlBox <> nil then
	      B.Visible := FControlBox.ActiveFrame = Self;
			if B.Visible then
      	if B.Top + B.Height + Border > Y then
        	Y := B.Top + B.Height + Border;
    end
  	else
    begin
     	if C.Top + C.Height + Border > Y then
       	Y := C.Top + C.Height + Border;
    	if C is TLabel then
    	begin
				L := TLabel(C);
				if Selected then
					L.Font.Color := FontColors[FControlBox.Focused or IsChild(Handle, Windows.GetFocus)]
    	  else
					L.Font.Color := FontColors[Selected];
	    end;
	  end;
	end;
	FActive := Selected;
  Width := X;
  Height := Y;
  Invalidate;
  UpdateWindow(Handle);
	RestoreAnchors(Anchors);
end;

procedure TControlBoxFrame.WndProc(var Message: TMessage);
begin
	case Message.Msg of
  	WM_LBUTTONDOWN:
			if FControlBox <> nil then
      begin
      	FActive := True;
				FControlBox.SelectFrame(Self);
        FControlBox.SetFocus;
				Invalidate;
        UpdateWindow(Handle);
			end;
	end;
  inherited WndProc(Message);
end;

procedure TControlBoxFrame.CMFocusChanged(var Msg: TCMFocusChanged);
begin
	inherited;
	if (FControlBox = nil) or	(FControlBox.ActiveFrame <> Self) then
  	Exit;
  if Msg.Sender is TControlBox then
    Exit;
	if FActive and (Msg.Sender = Self) then
    Exit

	{if (not FActive) and (Msg.Sender = Self) then
  	ChangeState}
	else if FActive and (not IsChild(Handle, Msg.Sender.Handle)) then
  	ChangeState
  else if (not FActive) and IsChild(Handle, Msg.Sender.Handle) then
  	ChangeState;
end;

procedure TControlBoxFrame.CNKeyDown(var Msg: TWMKeyDown);
begin
	if FControlBox <> nil then
  begin
	  case Msg.CharCode of
    	VK_UP: FControlBox.SelectPrior;
    	VK_DOWN: FControlBox.SelectNext;
		end;
    FControlBox.SetFocus;
	end;
end;

procedure TControlBoxFrame.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if FControlBox <> nil then
  	FControlBox.PaintFrame(Self, Msg.DC)
	else
	  FillRectColor(Msg.DC, ClientRect, Color);
  Msg.Result := 1;
end;

{ TControlBox }

constructor TControlBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColorA := clHighlight;
  FColorB := clWindow;
  FFrames := TList.Create;
  FItemIndex := -1;
  ParentColor := False;
  Color := clWindow;
  ControlStyle := [csAcceptsControls];
  Height := 96 * 3;
  Width := 96 * 2;
  TabStop := True;
  VertScrollBar.Tracking := True;
  HorzScrollBar.Tracking := True;
end;

destructor TControlBox.Destroy;
begin
	DestroyFastBitmap(FBackground);
	inherited Destroy;
  FFrames.Free;
end;

function TControlBox.Add(FrameClass: TControlBoxFrameClass): TControlBoxFrame;
begin
	Inc(FFrameCounter);
	Result := FrameClass.Create(Self);
  FFrames.Add(Result);
  Result.ControlBox := Self;
  Result.Name := FrameClass.ClassName + IntToStr(FFrameCounter);
  Result.Top := High(Word);
  Result.Parent := Self;
	Result.ChangeState;
  Result.Align := alTop;
end;

function TControlBox.Add(FrameClass: TCustomFrameClass): TCustomFrame;
begin
	Inc(FFrameCounter);
	Result := FrameClass.Create(Self);
  Result.Name := FrameClass.ClassName + IntToStr(FFrameCounter);
  Result.Top := High(Word);
  Result.Parent := Self;
  Result.Align := alTop;
end;

procedure TControlBox.Remove(Frame: TControlBoxFrame);
var
	I: Integer;
begin
	I := FFrames.IndexOf(Frame);
  if I > -1 then
  begin
  	FFrames.Delete(I);
    if not (csDestroying in ComponentState) then
		begin
    	FItemIndex := -1;
      FItemIndex := I;
    end;
  end;
end;

procedure TControlBox.Delete(Frame: TControlBoxFrame);
begin
	if Frame.ControlBox = Self then
  	Frame.Free;
end;

procedure TControlBox.SelectFrame(Frame: TControlBoxFrame);
begin
	if Frame.Enabled and Frame.Visible then
  begin
  	ItemIndex := FFrames.IndexOf(Frame);
    Frame.SetFocus;
    SetFocus;
	end;
end;

procedure TControlBox.SelectPrior;
var
	Frame: TControlBoxFrame;
	I: Integer;
begin
	I := ItemIndex - 1;
  if I < 0 then
  	SendMessage(Handle, WM_VSCROLL, SB_TOP,	0);
  while I > -1 do
	begin
  	Frame := Frames[I];
    if Frame.Enabled and Frame.Visible then
    begin
    	SelectFrame(Frame);
      Break;
    end;
    Dec(I);
  end;
end;

procedure TControlBox.SelectNext;
var
	Frame: TControlBoxFrame;
	I: Integer;
begin
	I := ItemIndex + 1;
  if I > FrameCount - 1 then
  	SendMessage(Handle, WM_VSCROLL, SB_BOTTOM,	0);
  while I < FrameCount do
	begin
  	Frame := Frames[I];
    if Frame.Enabled and Frame.Visible then
    begin
    	SelectFrame(Frame);
      Break;
    end;
    Inc(I);
  end;
end;

procedure TControlBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited KeyDown(Key, Shift);
  case Key of
   	VK_UP: SelectPrior;
   	VK_DOWN: SelectNext;
	end;
end;

procedure TControlBox.PaintFrame(Frame: TControlBoxFrame; DC: HDC);
var
	Rect: TRect;
  X, Y: Integer;
begin
	if not IsFastBitmap(FBackground) then
  begin
		FBackground := CreateFastBitmap($1, $FF);
  	DrawGradient(FBackground.DC, Classes.Rect(0, 0, $1, $FF), FColorA,
  		FColorB, drDown);
	end;
  Rect := Frame.ClientRect;
  if Frame = FItemFrame then
  	if Focused or IsChild(Handle, Windows.GetFocus) then
		begin
	  	FillRectColor(DC, Rect, FColorB);
			StretchBlt(DC, 0, 0, Rect.Right, $FF, FBackground.DC, 0, 0, 1, $FF, SRCCOPY);
		end
		else
	  	FillRectColor(DC, Rect, clBtnFace)
  else
  	FillRectColor(DC, Rect, Color);
	Y := Rect.Bottom - 1;
  X := Rect.Left;
  while X < Rect.Right do
  begin
  	SetPixel(DC, X, Y, $7F7F7F);
    Inc(X, 2);
  end;
end;

function TControlBox.GetActiveFrame: TControlBoxFrame;
begin
	if ItemIndex > -1 then
		Result := Frames[ItemIndex]
	else
  	Result := nil;
end;

procedure TControlBox.SetActiveFrame(Value: TControlBoxFrame);
begin
	if FFrames.IndexOf(Value) > -1 then
		SelectFrame(Value);
end;

procedure TControlBox.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    if FBorderStyle = bsNone then
    	BorderWidth := 0
		else if ThemePainter.Enabled then
    	BorderWidth := 1
		else
    	BorderWidth := 2
  end;
end;

procedure TControlBox.SetColorA(const Value: TColor);
begin
	if Value <> FColorA then
  begin
	  FColorA := Value;
    DestroyFastBitmap(FBackground);
  end;
end;

procedure TControlBox.SetColorB(const Value: TColor);
begin
	if Value <> FColorB then
  begin
	  FColorB := Value;
    DestroyFastBitmap(FBackground);
  end;
end;

function TControlBox.GetFrame(Index: Integer): TControlBoxFrame;
begin
	Result := TControlBoxFrame(FFrames[Index]);
end;

function TControlBox.GetFrameCount: Integer;
begin
	Result := FFrames.Count;
end;

procedure TControlBox.SetItemIndex(Value: Integer);
var
	Frame: TControlBoxFrame;
  I: Integer;
begin
	if Value < 0 then
  	Value := -1;
	if Value > FrameCount - 1 then
  	Value := FrameCount - 1;
	if Value <> FItemIndex then
  begin
		I := FItemIndex;
    FItemIndex := -1;
    FItemIndex := Value;
    FItemFrame := nil;
    if (I > -1) and (I < FrameCount) then
    begin
	    Frames[I].ChangeState;
      Frames[I].Invalidate;
      UpdateWindow(Frames[I].Handle);
		end;
		I := FItemIndex;
    FItemIndex := Value;
    if (I > -1) and (I < FrameCount) then
    begin
	    Frame := Frames[I];
      FItemFrame := Frame;
      Frame.ChangeState;
    end;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TControlBox.PaintBorder;
var
	DC: HDC;
  R: TRect;
begin
	if FBorderStyle = bsSingle then
  begin
		DC := GetWindowDC(Handle);
		R := BoundsRect;
	  OffsetRect(R, -Left, -Top);
  	DrawThemeBorder(DC, R, []);
	  ReleaseDC(Handle, DC);
  end;
end;

procedure TControlBox.WMGetDlgCode(var Msg: TMessage);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TControlBox.WMMouseWheel(var Msg: TWMMouseWheel);
begin
	if Msg.WheelDelta < 0 then
  begin
		SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
		SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
		SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
	end
	else
	begin
		SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
		SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
		SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
	end;
end;


procedure TControlBox.WMNCPaint(var Msg: TMessage);
begin
	inherited;
	PaintBorder;
end;

procedure TControlBox.WMSetFocus(var Msg: TWMSetFocus);
begin
	inherited;
  if ActiveFrame <> nil then
  begin
  	ActiveFrame.Invalidate;
    UpdateWindow(ActiveFrame.Handle);
  end;
end;

procedure TControlBox.WMSize(var Msg: TMessage);
begin
	inherited;
	PaintBorder;
end;

end.
