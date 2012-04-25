unit CategoryCtrls;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, GraphTools,
  FormTools;

type
	TCategoryPicker = class(TInputWindow)
  private
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
	protected
    procedure Paint; override;
	public
  	constructor Create(AOwner: TComponent); override;    
	published
  	property Font;
  end;

implementation

{ TCategoryPicker }

constructor TCategoryPicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 256;
  BorderStyle := bsSingle;
end;

procedure TCategoryPicker.CMFontChanged(var Message: TMessage);
var
  DC: HDC;
  F: HFont;
  M: TTextMetric;
begin
	DC := GetDC(0);
	F := SelectObject(DC, Font.Handle);
	GetTextMetrics(DC, M);
	SelectObject(DC, F);
	ReleaseDC(0, DC);
	Height := M.tmHeight + 8;
end;

procedure TCategoryPicker.Paint;
const
	Items: array[0..3] of string = ('Musical Instruments', 'Guitar',
  	'Parts, Accessories', 'Bodies');
	Glyphs: array[Boolean] of TGlyphKind = (gkArrowRight, gkArrowRightDisabled);
var
	DC: HDC;
  F: HFont;
  C: TColor;
  A, B: TRect;
  I: Integer;
begin
	PaintBorder;
  DC := Canvas.Handle;
  F := SelectObject(DC, Font.Handle);
  if Enabled then
  	C := clWindowText
	else
  	C := clBtnShadow;
  SetTextColor(DC, ColortoRGB(C));
  A := ClientRect;
  FillRectColor(DC, A, Color);
  InflateRect(A, -6, 0);
  for I := Low(Items) to High(Items) do
  begin
	  DrawCaption(DC, Items[I], A, drLeft);
    if I = High(Items) then Exit;
	  Inc(A.Left, CalculateCaptionSize(DC, Items[I]).cx + 4);
	  B := A;
	  B.Right := B.Left + 16;
	  OffsetRect(B, 0, 1);
	  GlyphDraw(DC, B, Glyphs[Focused], C);
    A.Left := B.Right + 4;
  end;
  SelectObject(DC, F);
end;

end.
