unit PhotoFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ResourceData, GraphTools, SuplCtrls, BtnCtrls,
  FormTools, StdCtrls, DialogsEx, DropMgr, PhotoCollection,
  ExtDlgs, ExtCtrls, ShellAPI;

type
  TPhotoCapture = class(TFrame)
    PhotoButton: TImageButton;
    ClearButon: TImageButton;
    MoveLeftButton: TImageButton;
    PriorButton: TImageButton;
    NextButton: TImageButton;
    MoveRightButton: TImageButton;
    Image: TImage;
    PhotoLabel: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    DimesionLabel: TLabel;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PhotoButtonClick(Sender: TObject);
    procedure ClearButonClick(Sender: TObject);
    procedure PriorButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure MoveLeftButtonClick(Sender: TObject);
    procedure MoveRightButtonClick(Sender: TObject);
    procedure ImageDblClick(Sender: TObject);
  private
    FBackground: HBRUSH;
    FStipple: HBRUSH;
    FIsFocused: Boolean;
    FPhotos: TPhotoItems;
    FPhotoCount: Integer;
    FPhotoIndex: Integer;
    procedure DropFiles(Sender: TObject; Files: TStrings);
    procedure PhotosChange(Sender: TObject);
    procedure UpdatePhotoControls;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFocusChanged(var Msg: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Photos: TPhotoItems read FPhotos;
  end;

implementation

{$R *.dfm}

{ TPhotoCapture }

constructor TPhotoCapture.Create(AOwner: TComponent);
var
  A: TAnchorDataArray;
begin
  inherited Create(AOwner);
  FBackground := GetBrush(bbChecker);
  FStipple := GetBrush(bbHalflight);
  FPhotos := TPhotoItems.Create(Self);
  FPhotos.OnChange := PhotosChange;
  FPhotoIndex := -1;
  A := SaveAnchors(Self);
  Width := MoveRightButton.Left + MoveRightButton.Width + 4;
  Height := MoveRightButton.Top + MoveRightButton.Height + 4;
  RestoreAnchors(A);
  Width := 128;
  Height := 128;
  DesktopFont := True;
end;

destructor TPhotoCapture.Destroy;
begin
  FPhotos.Free;
  DeleteObject(FStipple);
  DeleteObject(FBackground);
  inherited Destroy;
end;

procedure TPhotoCapture.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
  if HandleAllocated then
    DropManager.RegisterControl(Self, DropFiles);
end;

procedure TPhotoCapture.DropFiles(Sender: TObject; Files: TStrings);
const
  AllowedExtensions: array [0..4] of string = ('.BMP', '.PNG', '.WMF', '.JPG',
    '.JPEG');
var
  FileName, Ext: string;
  I, J: Integer;
begin
  FPhotos.BeginUpdate;
  try
    for I := 0 to Files.Count - 1 do
    begin
      FileName := Files[I];
      Ext := UpperCase(ExtractFileExt(FileName));
      for J := Low(AllowedExtensions) to High(AllowedExtensions) do
        if Ext = AllowedExtensions[J] then
        begin
          FPhotos.Add(FileName);
          Break;
        end;
    end;
  finally
    FPhotos.EndUpdate;
  end;
end;

procedure TPhotoCapture.PhotosChange(Sender: TObject);
begin
  if FPhotoCount < FPhotos.Count then
  begin
    FPhotoCount := FPhotos.Count;
    FPhotoIndex := FPhotoCount - 1;
  end
  else if FPhotoCount > FPhotos.Count then
  begin
    FPhotoCount := FPhotos.Count;
    if FPhotoIndex > FPhotoCount - 1 then
      FPhotoIndex := FPhotoCount - 1;
  end;
  if FPhotoIndex > -1 then
    Image.Picture.Assign(FPhotos[FPhotoIndex])
  else
    Image.Picture.Graphic := nil;
  UpdatePhotoControls;
end;

procedure TPhotoCapture.UpdatePhotoControls;
begin
  ClearButon.Enabled := FPhotoCount > 0;
  MoveLeftButton.Enabled := (FPhotoCount > 1) and (FPhotoIndex > 0);
  PriorButton.Enabled := (FPhotoCount > 1) and (FPhotoIndex > 0);
  NextButton.Enabled := (FPhotoCount > 1) and (FPhotoIndex < FPhotoCount - 1);
  MoveRightButton.Enabled := (FPhotoCount > 1) and (FPhotoIndex < FPhotoCount - 1);
  if FPhotoCount = 0 then
  begin
    PhotoLabel.Caption := 'No photo';
    DimesionLabel.Caption := '';
  end
  else
  begin
    PhotoLabel.Caption := Format('Photo %d of %d', [FPhotoIndex + 1, FPhotoCount]);
    with FPhotos[FPhotoIndex].Photo.Graphic do
      DimesionLabel.Caption := Format('%d x %d', [Width, Height]);
  end;
end;

procedure TPhotoCapture.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  FillRect(Msg.DC, ClientRect, FBackground);
end;

procedure TPhotoCapture.PaintWindow(DC: HDC);
const
  Headers: array[Boolean] of TThemedHeader = (thHeaderItemNormal, thHeaderItemHot);
var
  State: TDrawState;
  A, B: TRect;
  C: TColor;
begin
  inherited PaintWindow(DC);
  A := ClientRect;
  A.Bottom := ClearButon.Height + ClearButon.Top * 2;
  B := ClientRect;
  B.Bottom := Height + 5;
  B.Top := Height - (ClearButon.Height + ClearButon.Top * 2);
  if ThemePainter.Enabled then
  begin
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Headers[FIsFocused]), A);
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Headers[FIsFocused]), B);
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
      FillRect(DC, B, FStipple)
    else
      FillRectColor(DC, B, clBtnFace);
    if FIsFocused then
      C := Blend(clBtnFace, clBtnHighlight)
    else
      C := clBtnFace;
  end;
  if C <> $FFFFFF then
  begin
    PhotoButton.Color := C;
    ClearButon.Color := C;
    MoveLeftButton.Color := C;
    PriorButton.Color := C;
    NextButton.Color := C;
    MoveRightButton.Color := C;
  end;
  B.Bottom := B.Top + 1;
  FillRectColor(DC, B, clBtnShadow);
  if Enabled then
    State := []
  else
    State := [dsDisabled];
  DrawThemeBorder(DC, ClientRect, State);
end;

procedure TPhotoCapture.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
end;

procedure TPhotoCapture.PhotoButtonClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    FPhotos.Add(OpenPictureDialog.FileName);
end;

procedure TPhotoCapture.CMEnabledChanged(var Msg: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to ControlCount - 1 do
    Controls[I].Enabled := Enabled;
  if Enabled then
    UpdatePhotoControls;
  for I := 0 to ControlCount - 1 do
    Controls[I].Invalidate;
end;

procedure TPhotoCapture.CMFocusChanged(var Msg: TCMFocusChanged);
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

procedure TPhotoCapture.ClearButonClick(Sender: TObject);
begin
  SetFocus;
  if (FPhotoIndex > -1) and (FPhotoIndex < FPhotoCount) then
    FPhotos.Delete(FPhotoIndex);
end;

procedure TPhotoCapture.PriorButtonClick(Sender: TObject);
begin
  SetFocus;
  if FPhotoIndex > 0 then
  begin
    Dec(FPhotoIndex);
    PhotosChange(FPhotos);
  end;
end;

procedure TPhotoCapture.NextButtonClick(Sender: TObject);
begin
  SetFocus;
  if FPhotoIndex < FPhotoCount - 1 then
  begin
    Inc(FPhotoIndex);
    PhotosChange(FPhotos);
  end;
end;

procedure TPhotoCapture.MoveLeftButtonClick(Sender: TObject);
begin
  SetFocus;
  if FPhotoIndex > 0 then
  begin
    Dec(FPhotoIndex);
    FPhotos.Swap(FPhotoIndex, FPhotoIndex + 1);
  end;
end;

procedure TPhotoCapture.MoveRightButtonClick(Sender: TObject);
begin
  SetFocus;
  if FPhotoIndex < FPhotoCount - 1 then
  begin
    Inc(FPhotoIndex);
    FPhotos.Swap(FPhotoIndex, FPhotoIndex - 1);
  end;
end;

procedure TPhotoCapture.ImageDblClick(Sender: TObject);
begin
  if FPhotoIndex > -1 then
  if FileExists(FPhotos[FPhotoIndex].FileName) then
    ShellExecute(Handle, 'open', PChar(FPhotos[FPhotoIndex].FileName), nil, nil, SW_SHOW);

end;

end.
