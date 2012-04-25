
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit TransparentImageFrm;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grip, ExtCtrls, StdCtrls, SuplCtrls, GraphTools, FormTools, ImgList,
  ExtDlgs;

type
  TTransparentImageForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    HorizontalBar1: THorizontalBar;
    ScrollBar1: TScrollBar;
    Background1: TBackground;
    OpenPictureDialog: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure Background1Paint(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FFileName: string;
    FBrush: TBitmap;
    FImages: TTransparentImage;
  public
    constructor Create(Image: TTransparentImage); reintroduce;
    property FileName: string read FFileName;
    property Images: TTransparentImage read FImages;
  end;

function EditTransparentImage(Image: TTransparentImage): Boolean;

implementation

{$R *.DFM}

function EditTransparentImage(Image: TTransparentImage): Boolean;
var
  Form: TTransparentImageForm;
begin
  Form := TTransparentImageForm.Create(Image);
  try
    Result := Form.ShowModal = mrOK;
    if Result then
      Image.Assign(Form.Images);
  finally
    Form.Free;
  end;
end;

const
  ImageOffset = 20;

constructor TTransparentImageForm.Create(Image: TTransparentImage);
begin
  inherited Create(Application);
  FImages := TTransparentImageClass(Image.ClassType).Create(Self);
  FImages.Assign(Image);
end;

procedure TTransparentImageForm.FormCreate(Sender: TObject);
begin
   AdjustFontBorders(Self, Button4);
  FBrush := TBitmap.Create;
  FBrush.Height :=  ImageOffset;
  FBrush.Width := ImageOffset;
  FBrush.Canvas.Brush.Color := clWhite;
  FBrush.Canvas.FillRect(Rect(0, 0, ImageOffset, ImageOffset));
  FBrush.Canvas.Brush.Color := clSilver;
  FBrush.Canvas.FillRect(Rect(0, ImageOffset div 2, ImageOffset div 2, ImageOffset));
  FBrush.Canvas.FillRect(Rect(ImageOffset div 2, 0, ImageOffset, ImageOffset div 2));
  Background1.Canvas.Brush.Bitmap := FBrush;
  Background1.Canvas.Font := Font;
  Background1.Canvas.Font.Style := [fsBold];
end;


procedure TTransparentImageForm.Background1Paint(Sender: TObject);
var
  Size, Y, Index, I: Integer;
  R: TRect;
begin
  Background1.Canvas.FillRect(Rect(2, 2, Background1.Width - 2, Background1.Height - 2));
  Size := FImages.Size;
  if Size = 0 then Exit;
  Y := Round((Background1.Height - Size) / 2);
  Size := Size + ImageOffset;
  Index := ScrollBar1.Position;
  R := Rect(2, Background1.Height - 22, Size + 2, Background1.Height - 2);
  for I := 0 to Background1.Width div Size - 1 do
  begin
    if Index + I > FImages.Count - 1 then Exit;
    FImages.Draw(Background1.Canvas, I * Size + ImageOffset div 2, Y, Index + I,
     dsFocus, itImage);
    DrawCaption(Background1.Canvas.Handle, IntToStr(Index + I), R, drCenter);
    Slide(R, drRight);
  end;
end;

procedure TTransparentImageForm.ScrollBar1Change(Sender: TObject);
begin
  Background1.Repaint;
end;

procedure TTransparentImageForm.Button1Click(Sender: TObject);
begin
  FImages.Unload;
  ScrollBar1.Max := 0;
  Background1.Repaint;
end;

procedure TTransparentImageForm.Button2Click(Sender: TObject);
var
  I: Integer;
begin
  if OpenPictureDialog.Execute then
  begin
    FFileName := OpenPicturedialog.FileName;
    FImages.Load(FFileName);
    I := FImages.Count;
    if I > 0 then
      I := I - Background1.Width div (FImages.Size + ImageOffset);
    if I < 0 then
      I := 0;
    ScrollBar1.Max := I;
    Background1.Repaint;
  end;
end;

procedure TTransparentImageForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // FImage := nil;
  Action := caHide;
end;

end.
