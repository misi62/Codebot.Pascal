unit DesignCtrlsEx;

interface

uses
	Windows, Messages, Classes, SysUtils, Controls, Graphics, GraphTools,
	Forms, StdCtrls, DesignCtrls, BtnEdit, SuplCtrls, FormTools, PhotoFrame,
  MatrixFrame, InspectCtrls, CategoryCtrls;

{ TControlHost }

type
	TControlHost = class(TDesignHost)
  private
  	FControl: TWinControl;
    FControlHandle: HWND;
    FControlWndProc: TFNWndProc;
  	FControlCaption: TShadowLabel;
		procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
	protected
		procedure AdjustLayout; override;
    class function GetControlClass: TWinControlClass; virtual; abstract;
    property Control: TWinControl read FControl;
    property ControlCaption: TShadowLabel read FControlCaption;
		procedure Paint; override;
	public
  	constructor Create(AOwner: TComponent); override;
  end;

{ TEditHost }

	TEditHost = class(TControlHost)
	protected
    class function GetControlClass: TWinControlClass; override;
    class function GetControlKind: TDesignControl; override;
    class function GetFixedHeight: Boolean; override;
  end;

{ TMemoHost }

	TMemoHost = class(TControlHost)
  private
    function GetMemo: TMemo;
	protected
    class function GetControlClass: TWinControlClass; override;
    class function GetControlKind: TDesignControl; override;
	public
  	constructor Create(AOwner: TComponent); override;
    property Memo: TMemo read GetMemo;
  end;

{ TComboHost }

	TComboHost = class(TControlHost)
	protected
    class function GetControlClass: TWinControlClass; override;
    class function GetControlKind: TDesignControl; override;
    class function GetFixedHeight: Boolean; override;
  end;

{ TListHost }

	TListHost = class(TControlHost)
  private
    function GetListBox: TListBox;
	protected
    class function GetControlClass: TWinControlClass; override;
    class function GetControlKind: TDesignControl; override;
	public
  	constructor Create(AOwner: TComponent); override;
    property ListBox: TListBox read GetListBox;
  end;

{ TButtonHost }

	TButtonHost = class(TDesignHost)
  private
  	FButton: TButton;
		procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
	protected
		procedure AdjustLayout; override;
    class function GetControlKind: TDesignControl; override;
	public
  	constructor Create(AOwner: TComponent); override;
    property Button: TButton read FButton;
  end;

{ TImageHost }

	TImageHost = class(TControlHost)
  private
    function GetPhotoCapture: TPhotoCapture;
	protected
    class function GetControlClass: TWinControlClass; override;
    class function GetControlKind: TDesignControl; override;
	public
    property PhotoCaptue: TPhotoCapture read GetPhotoCapture;
  end;

{ TMatrixHost }

	TMatrixHost = class(TControlHost)
  private
    function GetMatrix: TInspector;
	protected
		procedure AdjustLayout; override;
    class function GetControlClass: TWinControlClass; override;
    class function GetControlKind: TDesignControl; override;
	public
    property Matrix: TInspector read GetMatrix;
  end;

{ TCategoryHost }

	TCategoryHost = class(TControlHost)
	protected
    class function GetControlClass: TWinControlClass; override;
    class function GetControlKind: TDesignControl; override;
		class function GetFixedHeight: Boolean; override;
 end;

{ TBandHost }

	TBandHost = class(TDesignHost, IUnknown, IDesignSurface)
  private
  	function GetHeaderHeight: Integer;
		procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
	protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Paint; override;
    class function GetControlKind: TDesignControl; override;
    property HeaderHeight: Integer read GetHeaderHeight;
	public
  	constructor Create(AOwner: TComponent); override;
		function AddControl(Control: TDesignControl): TDesignHost;
  end;

implementation

function StyleHeight(Stylizer: TDesignStylizer; Controls: array of TControl): Integer;
var
  Grid: Integer;
  C: TControl;
  H: Integer;
	I: Integer;
begin
	Result := 0;
  Grid := Stylizer.GridSize;
	for I := Low(Controls) to High(Controls) do
	begin
   	C := Controls[I];
  	H := ((C.Height + C.Top + Grid) div Grid) * Grid;
    if H < C.Height + C.Top + Grid then
			H := H + Grid;
		if H > Result then
    	Result := H;
  end;
end;

{ TControlHost }

constructor TControlHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControl := GetControlClass.Create(Self);
  FControl.Parent := Self;
  FControlCaption := TShadowLabel.Create(Self);
  FControlCaption.Parent := Self;
  FControlCaption.AutoSize := True;
  FControlCaption.Transparent := True;
  FControlCaption.FocusControl := FControl;
end;

procedure TControlHost.AdjustLayout;
var
	P: TCaptionPosition;
	C: Integer;
	I: Integer;
begin
	inherited AdjustLayout;
  C := ColorToRGB(FControlCaption.Font.Color);
  case C of
  	$000000:
    	begin
      	FControlCaption.ShadowColor := $FFFFFF;
		  	FControlCaption.Shadow := True;
      end;
    $FFFFFF:
    	begin
      	FControlCaption.ShadowColor := $000000;
		  	FControlCaption.Shadow := True;
      end;
  else
  	FControlCaption.Shadow := False;
	end;
	I := Stylizer.GridSize;
	FControl.Anchors := [akLeft, akTop];
  P := Stylizer.CaptionPosition;
  if Trim(Caption) = '' then
  	P := cpHidden;
  case P of
  	cpTop:
    	begin
      	FControlCaption.Visible := True;
			  FControlCaption.Left := I;
			  FControlCaption.Top := 1;
        FControl.Left := I;
				FControl.Top := FControlCaption.Top + FControlCaption.Height + (I shr 1);
      end;
    cpLeft:
    	begin
      	FControlCaption.Visible := True;
			  FControlCaption.Left := I;
			  FControlCaption.Top := I;
        if Trim(FControlCaption.Caption) = '' then
        	FControl.Left := I
				else
        	FControl.Left := FControlCaption.Left + FControlCaption.Width + I;
				FControl.Top := I;
      end;
    cpHidden:
    	begin
      	FControlCaption.Visible := False;
        FControl.Left := I;
				FControl.Top := I;
      end;
	end;
	Width := FControl.Left + FControl.Width + I;
  if FControlCaption.Visible then
		Height := StyleHeight(Stylizer, [FControl, FControlCaption])
  else
		Height := StyleHeight(Stylizer, [FControl]);
	RequestLayout;
  FControl.Width := Width - FControl.Left - I;
  FControl.Anchors := [akLeft, akTop, akRight, akBottom];
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  if FixedHeight then
	  Constraints.MaxHeight := Height;
	Control.HandleNeeded;
  FControl.Enabled := not Stylizer.Designing;
  FControlCaption.Enabled := not Stylizer.Designing;
end;

procedure TControlHost.CMTextChanged(var Msg: TMessage);
begin
  FControlCaption.Caption := Text;
  AdjustLayout;
end;

function SubClassProc(Wnd: HWND; Msg: Cardinal; wParam, lParam: Integer): Integer; stdcall;
var
	H: TControlHost;
  DC: HDC;
  R: TRect;
begin
	H := TControlHost(GetWindowLong(Wnd, GWL_USERDATA));
  Result := CallWindowProc(H.FControlWndProc, Wnd, Msg, wParam, lParam);
  case Msg of
  	WM_PAINT, WM_NCPAINT, WM_SIZE, WM_ERASEBKGND:
    	begin
      	DC := GetWindowDC(Wnd);
        GetWindowRect(Wnd, R);
        OffsetRect(R, -R.Left, -R.Top);
        if ThemePainter.Enabled then
        	InflateRect(R, -1, -1)
				else
        	InflateRect(R, -2, -2);
				SelectClipRect(DC, R, RGN_DIFF);
        if ThemePainter.Enabled then
        	InflateRect(R, 1, 1)
				else
        	InflateRect(R, 2, 2);
				FillRectColor(DC, R, H.Stylizer.BandColor);
        ReleaseDC(Wnd, DC);
      end;
	end;
end;

procedure TControlHost.Paint;
begin
  inherited Paint;
  if (Control <> nil) and (FControl.Handle <> FControlHandle) then
  begin
  	FControlHandle := FControl.Handle;
    Integer(FControlWndProc) := GetWindowLong(FControlHandle, GWL_WNDPROC);
    SetWindowLong(FControlHandle, GWL_USERDATA,	Integer(Self));
    SetWindowLong(FControlHandle, GWL_WNDPROC,	Integer(@SubClassProc));
  end;
end;

{ TEditHost }

class function TEditHost.GetControlClass: TWinControlClass;
begin
	Result := TEdit;
end;

class function TEditHost.GetControlKind: TDesignControl;
begin
	Result := dcEdit;
end;

class function TEditHost.GetFixedHeight: Boolean;
begin
	Result := True;
end;

{ TMemoHost }

constructor TMemoHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Memo.Height := Stylizer.GridSize * 4;
	Memo.ScrollBars := ssVertical;
end;

function TMemoHost.GetMemo: TMemo;
begin
	Result := TMemo(Control);
end;

class function TMemoHost.GetControlClass: TWinControlClass;
begin
	Result := TMemo;
end;

class function TMemoHost.GetControlKind: TDesignControl;
begin
	Result := dcMemo;
end;

{ TComboHost }

class function TComboHost.GetControlClass: TWinControlClass;
begin
	Result := TListEdit;
end;

class function TComboHost.GetControlKind: TDesignControl;
begin
	Result := dcCombo;
end;

class function TComboHost.GetFixedHeight: Boolean;
begin
	Result := True;
end;

type
	TListBoxFix = class(TListBox)
  private
  	procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
		procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  end;

{ TListHost }

procedure TListBoxFix.WMNCPaint(var Msg: TMessage);
var
	DC: HDC;
  R: TRect;
  State: TDrawState;
begin
	DC := GetWindowDC(Handle);
  R := BoundsRect;
  OffsetRect(R, -R.Left, -R.Top);
  InflateRect(R, -2, -2);
  SelectClipRect(DC, R, RGN_DIFF);
  InflateRect(R, 2, 2);
  FillRectColor(DC, R, clWindow);
  if Enabled then
  	State := []
	else
  	State := [dsDisabled];
  DrawThemeBorder(DC, R, State);
  ReleaseDC(Handle, DC);
  Msg.Result := 0;
end;

procedure TListBoxFix.WMPaint(var Msg: TWMPaint);
var
	M: TMessage;
begin
	inherited;
  WMNCPaint(M);
end;

constructor TListHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListBox.Height := Stylizer.GridSize * 4;
end;

class function TListHost.GetControlClass: TWinControlClass;
begin
	Result := TListBoxFix;
end;

class function TListHost.GetControlKind: TDesignControl;
begin
	Result := dcList;
end;

{ TButtonHost }

constructor TButtonHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TButton.Create(Self);
  FButton.Parent := Self;
end;

procedure TButtonHost.AdjustLayout;
var
	I: Integer;
begin
	inherited AdjustLayout;
	I := Stylizer.GridSize;
  FButton.Top := I;
  FButton.Left := I;
  Height := FButton.Top + FButton.Height + I;
  Width := FButton.Width + I * 2;
	RequestLayout;
  FButton.Width := Width - I * 2;
  FButton.Anchors := [akLeft, akTop, akRight, akBottom];
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  FButton.Enabled := not Stylizer.Designing;
end;

class function TButtonHost.GetControlKind: TDesignControl;
begin
	Result := dcButton;
end;

procedure TButtonHost.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  FButton.Caption := Text;
end;

{ TImageHost }

function TImageHost.GetPhotoCapture: TPhotoCapture;
begin
	Result := TPhotoCapture(Control);
end;

class function TImageHost.GetControlClass: TWinControlClass;
begin
	Result := TPhotoCapture;
end;

class function TImageHost.GetControlKind: TDesignControl;
begin
	Result := dcImage;
end;

{ TMatrixHost }

function TMatrixHost.GetMatrix: TInspector;
begin
	Result := TMatrixGrid(Control).Matrix;
end;

class function TMatrixHost.GetControlClass: TWinControlClass;
begin
	Result := TMatrixGrid;
end;

class function TMatrixHost.GetControlKind: TDesignControl;
begin
	Result := dcMatrix;
end;

procedure TMatrixHost.AdjustLayout;
var
	I: Integer;
begin
  inherited AdjustLayout;
	{Matrix.Color := Blend(Stylizer.Color, clWhite); //Blend(Stylizer.Color, clWhite);
  TMatrixGrid(Control).HeaderColor := Blend(Stylizer.BandColor, clWhite, 75);}
	I := Matrix.ItemIndex;
  Matrix.ItemIndex := -1;
  Matrix.ItemIndex := I;
end;

{ TCategoryHost }

class function TCategoryHost.GetControlClass: TWinControlClass;
begin
	Result := TCategoryPicker;
end;

class function TCategoryHost.GetControlKind: TDesignControl;
begin
	Result := dcCategory;
end;

class function TCategoryHost.GetFixedHeight: Boolean;
begin
	Result := True;
end;

{ TBandHost }

constructor TBandHost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
	Width := Stylizer.GridSize * 12 - 1;
  Height :=	Stylizer.GridSize * 12 - 1;
end;

function TBandHost.AddControl(Control: TDesignControl): TDesignHost;
begin
	Result := FindDesignControlClass(Control).Create(Self);
	Result.Parent := Self;
	Result.Left := ControlCount * Stylizer.GridSize - 1;
	Result.Top := ControlCount * Stylizer.GridSize - 1;
  Result.Caption := Result.ClassName;
end;

procedure TBandHost.AlignControls(AControl: TControl; var Rect: TRect);
var
	Control: TControl;
  Header: Integer;
  Grid: Integer;
  W, H: Integer;
	I: Integer;
begin
	if dsLoading in Stylizer.DesignState then Exit;
	if (not NeedsAlign) and (AControl = nil) then Exit;
  Header := HeaderHeight + 2;
  Grid := Stylizer.GridSize;
  if (Header mod Grid) > 0 then
  	Header := (Header div Grid) * Grid + Grid;
	W := Stylizer.GridSize * 2;
  H := Header + Stylizer.GridSize * 2;
	for I := 0 to ControlCount - 1 do
  	if (Controls[I] is TDesignHost) and (Controls[I].Parent = Self) then
    begin
    	Control := Controls[I];
      if Control.Left < 0 then
				Control.Left := 0;
      if Control.Top < Header then
      	Control.Top := Header;
      if Control.Left + Control.Width > W then
      	W := Control.Left + Control.Width;
			if Control.Top + Control.Height > H then
      	H := Control.Top + Control.Height + Grid;
    end;
 	if NeedsAlign or (GetCapture = 0) then
  begin
  	if W > Width then
    	Width := W;
    if H > Height then
    	Height := H;
    Constraints.MinWidth := W;
    Constraints.MinHeight := H;
  end;
  NeedsAlign := False;
end;

procedure TBandHost.Paint;
var
	State: TDrawState;
	R: TRect;
  DC: HDC;
  C: TColor;
begin
	inherited Paint;
  R := ClientRect;
	R.Bottom := HeaderHeight;
	if R.Bottom = 0 then Exit;
  if Stylizer.Designing then
  begin
    InflateRect(R, -1, -1);
    FillRectColor(Canvas.Handle, R, clBtnShadow);
    State := [];
  	if Focused then
    	State := [dsFocused]
		else if Stylizer.ActiveControl = Self then
    	State := [dsHot];
    if State <> [] then
			DrawThemeDesigner(Canvas.Handle, ClientRect, Stylizer.GripSize,
      	FixedWidth, FixedHeight,  State);
  end
  else
  begin
		if (Stylizer.Band.Graphic <> nil) and (not Stylizer.Band.Graphic.Empty) then
	    Canvas.Draw(0, 0, Stylizer.Band.Graphic)
    else
    begin
    	Canvas.Brush.Color := Stylizer.BandColor;
      Canvas.FillRect(R);
    end;
  end;
	InflateRect(R, -HeightOf(R) div 4, 0);
  OffsetRect(R, Stylizer.BandOffsetX, Stylizer.BandOffsetY);
	DC := Canvas.Handle;
  C := ColorToRGB(Canvas.Font.Color);
  if (C = $0) or (C = $FFFFFF) then
  begin
  	C := (C xor $FFFFFF) and $FFFFFF;
		SetTextColor(DC, C);
  	OffsetRect(R, 1, 1);
		DrawCaption(DC, Caption, R, AlignmentToDirection(Stylizer.BandAlignment));
  	OffsetRect(R, -1, -1);
  	C := (C xor $FFFFFF) and $FFFFFF;
  end;
	SetTextColor(DC, C);
	DrawCaption(DC, Caption, R, AlignmentToDirection(Stylizer.BandAlignment));
  OffsetRect(R, -Stylizer.BandOffsetX, -Stylizer.BandOffsetY);
  R.Left := 0;
  R.Top := R.Bottom + 2;
  R.Right := Width;
  R.Bottom := Height;
  if Stylizer.ActiveBand = Self then
	  BlendBackground(DC, R);
end;

class function TBandHost.GetControlKind: TDesignControl;
begin
	Result := dcBand;
end;

function TBandHost.GetHeaderHeight: Integer;
var
	A, B: Integer;
begin
	if (Stylizer.Band.Graphic <> nil) and (not Stylizer.Band.Graphic.Empty) then
  	A := Stylizer.Band.Graphic.Height
	else
  	A := 0;
	Canvas.Font := Stylizer.BandFont;
  B := Round(Canvas.TextHeight('Wg') * 1.25);
  if not Odd(B) then
		Inc(B);
  if B > A then
  	Result := B
	else
  	Result := A;
end;

procedure TBandHost.CMTextChanged(var Msg: TMessage);
begin
	if HandleAllocated then
		Invalidate;
end;

function TListHost.GetListBox: TListBox;
begin
	Result := TListBox(Control);
end;

initialization
	RegisterDesignControlClass(TEditHost);
	RegisterDesignControlClass(TMemoHost);
	RegisterDesignControlClass(TComboHost);
	RegisterDesignControlClass(TListHost);
  RegisterDesignControlClass(TButtonHost);
  RegisterDesignControlClass(TImageHost);
  RegisterDesignControlClass(TMatrixHost);
	RegisterDesignControlClass(TCategoryHost);
	RegisterDesignControlClass(TBandHost);
end.
