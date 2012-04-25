
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit GlassImagesFrm;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grip, ExtCtrls, StdCtrls, SuplCtrls, GraphTools, ImageListEx,
  ExtDlgs;

type
  TGlassImagesForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    HorizontalBar1: THorizontalBar;
    ScrollBar: TScrollBar;
    ImageBox: TBackground;
    OpenPictureDialog: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure ImageBoxPaint(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FImages: TGlassImages;
    FBrush: TBitmap;
    procedure SetImages(const Value: TGlassImages);
  public
    property Images: TGlassImages read FImages write SetImages;
  end;

function EditGlassImages(Img: TGlassImages): Boolean;

implementation

{$R *.DFM}

function EditGlassImages(Img: TGlassImages): Boolean;
begin
	with TGlassImagesForm.Create(Application) do
	try
  	Images := Img;
    Result := ShowModal = mrOK;
    if Result then
      Img.Assign(Images);
  finally
  	Free;
  end;
end;

const
	ImageOffset = 20;

procedure TGlassImagesForm.FormCreate(Sender: TObject);
begin
	FImages := TGlassImages.Create;
 	AdjustFontBorders(Self, Button4);
  FBrush := TBitmap.Create;
  FBrush.Height :=  ImageOffset;
  FBrush.Width := ImageOffset;
  FBrush.Canvas.Brush.Color := clWhite;
  FBrush.Canvas.FillRect(Rect(0, 0, ImageOffset, ImageOffset));
  FBrush.Canvas.Brush.Color := clSilver;
  FBrush.Canvas.FillRect(Rect(0, ImageOffset div 2, ImageOffset div 2, ImageOffset));
  FBrush.Canvas.FillRect(Rect(ImageOffset div 2, 0, ImageOffset, ImageOffset div 2));
  ImageBox.Canvas.Brush.Bitmap := FBrush;
  ImageBox.Canvas.Font := Font;
  ImageBox.Canvas.Font.Style := [fsBold];
end;


procedure TGlassImagesForm.FormDestroy(Sender: TObject);
begin
	FBrush.Free;
	FImages.Free;
end;

procedure TGlassImagesForm.ImageBoxPaint(Sender: TObject);
var
	Size, Y, Index, I: Integer;
  R: TRect;
begin
  ImageBox.Canvas.FillRect(Rect(2, 2, ImageBox.Width - 2, ImageBox.Height - 2));
  Size := FImages.Size;
  if Size = 0 then Exit;
  Y := Round((ImageBox.Height - Size) / 2);
  Size := Size + ImageOffset;
  Index := ScrollBar.Position;
  R := Rect(2, ImageBox.Height - 22, Size + 2, ImageBox.Height - 2);
  for I := 0 to ImageBox.Width div Size do
  begin
  	if Index + I > FImages.Count - 1 then Exit;
  	FImages.Draw(ImageBox.Canvas, Index + I, I * Size + ImageOffset div 2, Y, [dsHot]);
    DrawCaption(ImageBox.Canvas.Handle, IntToStr(Index + I), R, drCenter);
    Slide(R, drRight);
  end;
end;

procedure TGlassImagesForm.SetImages(const Value: TGlassImages);
var
	I: Integer;
begin
  FImages.Assign(Value);
  I := FImages.Count;
  if I > 0 then
	  I := I - ImageBox.Width div (FImages.Size + ImageOffset);
  if I < 0 then
  	I := 0;
  ScrollBar.Max := I;
  ImageBox.Repaint;
end;

procedure TGlassImagesForm.ScrollBarChange(Sender: TObject);
begin
	ImageBox.Repaint;
end;

procedure TGlassImagesForm.Button1Click(Sender: TObject);
begin
	FImages.Clear;
  ScrollBar.Max := 0;
  ImageBox.Repaint;
end;

procedure TGlassImagesForm.Button2Click(Sender: TObject);
var
	I: Integer;
begin
  if OpenPictureDialog.Execute then
  begin
  	FImages.LoadFromFile(OpenPicturedialog.FileName);
	  I := FImages.Count;
	  if I > 0 then
		  I := I - ImageBox.Width div (FImages.Size + ImageOffset);
	  if I < 0 then
  		I := 0;
	  ScrollBar.Max := I;
	  ImageBox.Repaint;
	end;
end;

end.
