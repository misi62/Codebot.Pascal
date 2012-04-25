unit MatrixFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, 
  Dialogs, ScrollCtrls, InspectCtrls, InspectEditors, BtnCtrls, StdCtrls,
  GraphTools, FormTools;

type
  TMatrixGrid = class(TFrame)
    PhotoLabel: TLabel;
    MatrixButton: TImageButton;
    ClearButon: TImageButton;
    Matrix: TInspector;
	private
	  FStipple: HBRUSH;
    FIsFocused: Boolean;
    FHeaderColor: TColor;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
	protected
    procedure PaintWindow(DC: HDC); override;
	public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property HeaderColor: TColor read FHeaderColor write FHeaderColor;
  end;

implementation

{$R *.dfm}

{ TMatrixGrid }

constructor TMatrixGrid.Create(AOwner: TComponent);
var
	A: TAnchorDataArray;
begin
  inherited Create(AOwner);
  ParentColor := False;
  Color := clBtnFace;
  FStipple := GetBrush(bbHalflight);
  A := SaveAnchors(Self);
  if ThemePainter.Enabled then
  	Matrix.Left := 1;
  Width := Matrix.Left * 2 + Matrix.Width;
  if ThemePainter.Enabled then
	  Height := Matrix.Top + Matrix.Height + 1
	else
	  Height := Matrix.Top + Matrix.Height + 2;
  RestoreAnchors(A);
  Width := 128;
  Height := 96;
  with Matrix.Editors.Add(ekPicklist) as TPickInspectorEditor do
  begin
  	Name := 'Brand';
    Items.Add('Brand A');
    Items.Add('Brand B');
    Items.Add('Brand C');
    Text := 'Brand A';
  end;
  with Matrix.Editors.Add(ekPicklist) as TPickInspectorEditor do
  begin
  	Name := 'Color';
    Items.Add('Red');
    Items.Add('Green');
    Items.Add('Blue');
    Text := 'Blue';
  end;
  with Matrix.Editors.Add(ekPicklist) as TPickInspectorEditor do
  begin
  	Name := 'Material';
    Items.Add('Cloth');
    Items.Add('Leather');
    Items.Add('Rubber');
    Text := 'Leather';
  end;
  FHeaderColor := Color;
end;

destructor TMatrixGrid.Destroy;
begin
	DeleteObject(FStipple);
	inherited Destroy;
end;

procedure TMatrixGrid.CMEnabledChanged(var Msg: TMessage);
var
	I: Integer;
begin
	inherited;
	for I := 0 to ControlCount - 1 do
  	Controls[I].Enabled := Enabled;
	{if Enabled then
		UpdatePhotoControls;}
	for I := 0 to ControlCount - 1 do
  	Controls[I].Invalidate;
end;

procedure TMatrixGrid.CMFocusChanged(var Msg: TCMFocusChanged);
var
	NewFocus: Boolean;
begin
	inherited;
	if Msg.Sender = nil then
  	NewFocus := False
	else
  	NewFocus := (Msg.Sender = Self) or IsChild(Handle, Msg.Sender.Handle);
	if NewFocus <> FIsFocused then
  begin
  	FIsFocused := NewFocus;
    Repaint;
  end;
end;


procedure TMatrixGrid.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  FillRectColor(Msg.DC, ClientRect, clBtnFace);
end;

procedure TMatrixGrid.PaintWindow(DC: HDC);
const
	Headers: array[Boolean] of TThemedHeader = (thHeaderItemNormal, thHeaderItemHot);
var
	A: TRect;
  State: TDrawState;
  C: TColor;
begin
	inherited PaintWindow(DC);
	A := ClientRect;
  A.Bottom := Matrix.Top;
	if ThemePainter.Enabled then
	begin
		ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Headers[FIsFocused]), A);
    {FillRectColor(DC, A, FHeaderColor);
    A.Top := A.Bottom - 1;
    FillRectColor(DC, A, Blend(FHeaderColor, 0));}
		C := GetPixel(DC, WidthOf(A) shr 1, 3) and $FFFFFF;
	end
  else
  begin
  	if FIsFocused then
	  	FillRect(DC, A, FStipple)
		else
	  	FillRectColor(DC, A, clBtnFace);
	  A.Top := A.Bottom - 1;
  	FillRectColor(DC, A, clBtnShadow);
		if FIsFocused then
	  	C := Blend(clBtnFace, clBtnHighlight)
		else
	  	C := clBtnFace;
  end;
  if C <> $FFFFFF then
  begin
		MatrixButton.Color := C;
		ClearButon.Color := C;
  end;
  if Enabled then
  	State := []
	else
  	State := [dsDisabled];
  DrawThemeBorder(DC, ClientRect, State);
end;


end.
