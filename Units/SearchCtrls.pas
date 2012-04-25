unit SearchCtrls;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  DialogsEx, ResourceData, BtnCtrls, StdCtrls, GraphTools, SuplCtrls;

type
  TSearchBar = class(TFrame)
    FindEdit: TEdit;
    NextButton:  TImageButton;
    PriorButton: TImageButton;
    FindButton: TImageButton;
    procedure CloseButtonClick(Sender: TObject);
	private
		procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
	protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
	public
	  constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TSeachBar }

constructor TSearchBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alBottom;
  BorderWidth := 2;
end;

procedure TSearchBar.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited AlignControls(AControl, Rect);
	ClientHeight := NextButton.Top + NextButton.Height;
	FindEdit.Top := (ClientHeight - FindEdit.Height) shr 1;
	PriorButton.Left := NextButton.Left + NextButton.Width + 2
end;

procedure TSearchBar.CloseButtonClick(Sender: TObject);
begin
	Hide;
end;

procedure TSearchBar.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
 	FillRectColor(Msg.DC, ClientRect, clBtnFace);
  Msg.Result := 1;
end;

procedure TSearchBar.WMNCPaint(var Msg: TMessage);
var
	DC: HDC;
  R: TRect;
begin
	DC := GetWindowDC(Handle);
	R := BoundsRect;
	OffsetRect(R, -Left, -Top);
	InflateRect(R, -BorderWidth, -BorderWidth);
	SelectClipRect(DC, R, RGN_DIFF);
	InflateRect(R, BorderWidth, BorderWidth);
	FillRectColor(DC, R, clBtnFace);
  R.Bottom := R.Top + 1;
	FillRectColor(DC, R, clBtnShadow);
	ReleaseDC(Handle, DC);
  Msg.Result := 0;
end;

end.
