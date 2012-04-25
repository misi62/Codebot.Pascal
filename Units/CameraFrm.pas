unit CameraFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PhotoCtrls, StdCtrls, FormTools, ScrollCtrls, BaseTypes, GraphTools,
  ComCtrls, SuplCtrls, Grip, ExtCtrls, CameraCtrls, Math, FileTools,
  WinTools, ProgressCtrls, ProviderTools, SlideCtrls, GdiPlus, GdiIntf;

{ TCameraForm }

const
  WM_FILECHANGE = WM_USER + $D;

type
  TCameraForm = class(TGripForm)
    OkButton: TButton;
    CancelButton: TButton;
    CaptuteButton: TButton;
    ZoomLabel: TLabel;
    FlashCheckBox: TCheckBox;
    SizeComboBox: TComboBox;
    ZoomBar: TSlideBar;
    SizeLabel: TLabel;
    CameraList: TDrawList;
    ReconnectButton: TButton;
    CameraTimer: TTimer;
    PhotoList: TPanel;
    ProgressIndicator: TIndeterminateProgress;
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SizeComboBoxChange(Sender: TObject);
    procedure ZoomBarChange(Sender: TObject);
    procedure FlashCheckBoxClick(Sender: TObject);
    procedure ReconnectButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CameraTimerTimer(Sender: TObject);
    procedure CaptuteButtonClick(Sender: TObject);
    procedure CameraListSelectItem(Sender: TObject);
    procedure CameraListDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CamerDrawItem(Sender: TObject; Canvas: TCanvas;
      Index: Integer; Rect: TRect; State: TDrawState);
  private
    FMonitor: TDirectoryMonitor;
    FDirectory: string;
    FAuthor: string;
    FBars: TBitmap;
    FPhotos: TStrings;
    FPhotoView: TPhotoView;
    FWatermark: TAlphaImage;
    FCameraImage: TAlphaImage;
    FCameraManager: TCameraManager;
    procedure SetDirectory(Value: string);
    procedure CameraManagerChange(Sender: TObject);
    procedure CameraManagerModeChange(Sender: TObject; Camera: TCamera);
    procedure CameraManagerSnap(Sender: TObject; Camera: TCamera);
    procedure CameraManagerViewFinder(Sender: TObject; Camera: TCamera);
    procedure UpdateCameraControls;
    function GetActiveCamera: TCamera;
    procedure FileChange(Sender: TObject; Action: TFileAction;
      const FileName: string);
    procedure WMFileChange(var Msg: TMessage); message WM_FILECHANGE;
  protected
    function GetOffsets: TSize; override;
  public
    property Directory: string read FDirectory write SetDirectory;
    property Author: string read FAuthor write FAuthor;
    property Photos: TStrings read FPhotos;
  end;

implementation

uses
  CanonObj,
  PowerShotObj;

{$R *.dfm}

{ TCameraForm }

procedure TCameraForm.SetDirectory(Value: string);
begin
  FPhotoView.Photos.Clear;
  FDirectory := Value;
end;

procedure TCameraForm.WMFileChange(var Msg: TMessage);
var
  PhotoItem: TPhotoItem;
begin
  PhotoItem := TPhotoItem(Msg.WParam);
  if not FileExists(PhotoItem.FileName) then
    PhotoItem.Free;
end;

procedure TCameraForm.FileChange(Sender: TObject; Action: TFileAction;
  const FileName: string);
var
  PhotoItem: TPhotoItem;
begin
  PhotoItem := FPhotoView.Photos.FindItemFile(FileName);
  if PhotoItem = nil then Exit;
  case Action of
    faRemoved:
      begin
        Sleep(100);
        PostMessage(Handle, WM_FILECHANGE, Integer(PhotoItem), 0);
      end;
    faAdded,
    faModified,
    faRenamedNewName:
      begin
        Sleep(100);
        FPhotoView.AddPhoto(FileName);
      end;
  end;
end;

procedure TCameraForm.FormCreate(Sender: TObject);
begin
  Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);
  { Hack ?}
  InitCanonSdk;
  InitPowerShotSdk;
  FBars := TBitmap.Create;
  FBars.LoadFromResourceID(HInstance, 606);
  FMonitor := TDirectoryMonitor.Create;
  FMonitor.Options := [moFileName, moSize, moCreation];
  FMonitor.OnFileChange := FileChange;
  FCameraImage := TAlphaImage.Create;
  FCameraImage.LoadFromResourceID(604);
  FPhotoView := TPhotoView.Create(Self);
  FPhotoView.Parent := PhotoList;
  FPhotoView.Align := alClient;
  FPhotos := TStringList.Create;
  CameraList.DoubleBuffered := True;
  FCameraManager := TCameraManager.Create;
  if FCameraManager <> nil then
  begin
    FCameraManager.OnChange := CameraManagerChange;
    FCameraManager.OnModeChange := CameraManagerModeChange;
    FCameraManager.OnSnap := CameraManagerSnap;
    FCameraManager.OnViewFinder := CameraManagerViewFinder;
  end;
end;

procedure TCameraForm.FormDestroy(Sender: TObject);
begin
  if FCameraManager <> nil then
  begin
    FCameraManager.OnChange := nil;
    FCameraManager.OnModeChange := nil;
    FCameraManager.OnSnap := nil;
    FCameraManager.OnViewFinder := nil;
  end;
  FCameraManager.Free;
  FCameraManager := nil;
  FMonitor.Free;
  FCameraImage.Free;
  FWatermark.Free;
  FPhotos.Free;
  FBars.Free;
  FinishCanonSdk;
  FinishPowerShotSdk;
end;

procedure TCameraForm.CameraManagerChange(Sender: TObject);
begin
  if FCameraManager = nil then Exit;
  CameraList.Count := FCameraManager.CameraCount;
  if (FCameraManager.CameraCount > 0) and (CameraList.ItemIndex < 0) then
    CameraList.ItemIndex := 0;
  UpdateCameraControls;
  CameraTimer.Enabled := True;
end;

function TCameraForm.GetActiveCamera: TCamera;
begin
  Result := nil;
  if (FCameraManager.CameraCount > 0) and (CameraList.Count > 0) and (CameraList.ItemIndex > -1) and
    (CameraList.ItemIndex < FCameraManager.CameraCount) then
    Result := FCameraManager.Cameras[CameraList.ItemIndex];
end;

procedure TCameraForm.UpdateCameraControls;
var
  Camera: TCamera;
  CanEnable: Boolean;
begin
  Camera := GetActiveCamera;
  CanEnable := Camera <> nil;
  SizeLabel.Enabled := CanEnable;
  SizeComboBox.Enabled := CanEnable;
  ZoomLabel.Enabled := CanEnable;
  ZoomBar.Enabled := CanEnable;
  FlashCheckBox.Enabled := CanEnable;
  CaptuteButton.Enabled := CanEnable;
  if Camera <> nil then
  begin
    SizeComboBox.ItemIndex := Ord(Camera.ImageSize) - 1;
    ZoomBar.Max := Camera.Query(prMaxZoom);
    ZoomBar.Position := Round(Camera.Zoom);
    FlashCheckBox.Checked := Camera.Flash;
  end;
end;

procedure TCameraForm.CameraManagerModeChange(Sender: TObject; Camera: TCamera);
begin
  UpdateCameraControls;
end;

procedure ResizeImage(FileName: string; Width, Height: Integer);
var
  S, D: IGdiBitmap;
  G: IGdiGraphics;
  E: TGUID;
begin
  S := NewBitmap(FileName);
  D := NewBitmap(Width, Height);
  G := NewGraphics(D);
  G.SetInterpolationMode(InterpolationModeHighQualityBicubic);
  G.DrawImage(S, NewRectF(0, 0, Width, Height), 0, 0, S.GetWidth, S.GetHeight, UnitPixel);
  S := nil;
  DeleteFile(FileName);
  GetEncoderClsid(JpgFormat, E);
  D.Save(FileName, E);
end;

procedure TCameraForm.CameraManagerSnap(Sender: TObject; Camera: TCamera);
var
  Item: TPhotoItem;
  S: string;
begin
  S := FDirectory + FormatDateTime('yyyymmdd.HHMMSS', Now) + '.jpg';
  while FileExists(S) do
  begin
    Sleep(1000);
    S := FormatDateTime('yyyymmdd.HHMMSS', Now) + '.jpg';
  end;
  try
    Camera.Snapshot.SaveToFile(S);
    case Camera.ImageSize of
      isMedium: ResizeImage(S, 800, 600);
      isLarge: ResizeImage(S, 1024, 768);
    end;
    Item := FPhotoView.AddPhoto(S);
    Item.Checked := True;
    { TODO: select new item }
  except
  end;
end;

procedure TCameraForm.CameraManagerViewFinder(Sender: TObject;
  Camera: TCamera);
begin
  CameraList.Invalidate;
end;

procedure TCameraForm.FormPaint(Sender: TObject);
var
  Theme: TThemedExplorerBar;
  DC: HDC;
  Rect: TRect;
  R: TRect;
begin
  Rect := Classes.Rect(8, 8, PhotoList.Left - 8, PhotoList.Height + 8);
  R := ClientRect;
  R.Bottom := PhotoList.Height;
  DC := Canvas.Handle;
  if ThemePainter.Enabled then
  begin
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(tebExplorerBarRoot), R);
    R := Rect;
    R.Bottom := R.Top + FontHeight(DC) * 2;
    Theme := tebNormalGroupHead;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
    Inc(R.Left, 28);
    ThemePainter.DrawText(DC, ThemePainter.GetDetails(tebNormalGroupHead), 'Camera Controls', R, DT_LEFT or DT_VCENTER or DT_SINGLELINE);
    R.Left := Rect.Left;
    R.Top := R.Bottom;
    R.Bottom := Rect.Bottom - 16;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(tebNormalGroupBackground), R);
  end
  else
  begin
    //Inc(R.Bottom, 2);
    R.Right := PhotoList.Left;
    FillRectColor(DC, R, Blend(clBtnShadow, clBtnFace));
    R := Rect;
    R.Bottom := R.Top + FontHeight(DC) * 2;
    FillRectColor(DC, R, clBtnFace);
    DrawFrame(DC, R, dfRaised);
    InflateRect(R, -12, 0);
    Inc(R.Left, 20);
    DrawCaption(DC, 'Camera Controls', R, drLeft);
    R.Left := Rect.Left;
    R.Top := R.Bottom;
    R.Bottom := Rect.Bottom - 16;
    R.Right := PhotoList.Left - 8;
    FillRectColor(DC, R, clBtnFace);
    DrawFrame(DC, R, dfRaised);
  end;
  Canvas.Draw(0, 4, FCameraImage);
end;

procedure TCameraForm.FormResize(Sender: TObject);
begin
  Invalidate;
end;

procedure TCameraForm.OkButtonClick(Sender: TObject);
var
  I: Integer;
begin
  FPhotos.Clear;
  for I := 0 to FPhotoView.Photos.Count - 1 do
    if FPhotoView.Photos[I].Checked then
      FPhotos.Add(FPhotoView.Photos[I].FileName);
  FPhotoView.Recycle(True);
end;

procedure TCameraForm.CamerDrawItem(Sender: TObject; Canvas: TCanvas;
  Index: Integer; Rect: TRect; State: TDrawState);
var
  Camera: TCamera;
  Bitmap: TBitmap;
  FastBitmap: TFastBitmap;
  A, B: PRGB;
  H, W: Integer;
  R: TRect;
  S: string;
begin
  Canvas := CameraList.Canvas;
  Canvas.Font := Font;
  Canvas.Brush.Color := clWindow;
  Canvas.FillRect(Rect);
  if Index > FCameraManager.CameraCount - 1 then
    Exit;
  Camera := FCameraManager.Cameras[Index];
  R := Rect;
  InflateRect(R, -1, -1);
  if dsSelected in State then
  begin
    if not ThemePainter.Enabled then
      FillRectColor(Canvas.Handle, R, clBtnFace);
    if dsFocused in State then
      DrawThemeButton(Canvas.Handle, R, [dsHot])
    else
      DrawThemeButton(Canvas.Handle, R, []);
  end;
  if Camera.View is TBitmap then
  begin
    Bitmap := TBitmap(Camera.View);
    if Bitmap.Empty then
      Bitmap := FBars;
    FastBitmap := CreateFastBitmap(Bitmap.Width shr 1, Bitmap.Height shr 1);
    A := FastBitmap.Bits;
    for H := FastBitmap.Height - 1 downto 0 do
    begin
      B := Bitmap.ScanLine[H shl 1];
      for W := 0 to FastBitmap.Width - 1 do
      begin
        A^ := B^;
        Inc(A);
        Inc(B, 2);
      end;
    end;
    BitBlt(Canvas.Handle, (WidthOf(Rect) - FastBitmap.Width) shr 1,
      Rect.Top + (HeightOf(Rect) - FastBitmap.Height) shr 1 - 8, Min(FastBitmap.Width, ClientWidth),
      FastBitmap.Height, FastBitmap.DC, 0, 0, SRCCOPY);
    DestroyFastBitmap(FastBitmap);
    R := Rect;
    R.Top := R.Bottom - 28;
    S := Camera.Query(prName);
    DrawCaption(Canvas.Handle, S, R, drCenter);
  end
  else
    Canvas.FillRect(Rect);
end;

procedure TCameraForm.SizeComboBoxChange(Sender: TObject);
var
  Camera: TCamera;
begin
  Camera := GetActiveCamera;
  if Camera <> nil then
  begin
    Camera.ImageSize := TImageSize(SizeComboBox.ItemIndex + 1);
    CameraTimer.Enabled := True;
  end;
end;

procedure TCameraForm.ZoomBarChange(Sender: TObject);
var
  Camera: TCamera;
begin
  Camera := GetActiveCamera;
  if Camera <> nil then
  begin
    Camera.Zoom := ZoomBar.Position;
    CameraTimer.Enabled := True;
  end;
end;

procedure TCameraForm.FlashCheckBoxClick(Sender: TObject);
var
  Camera: TCamera;
begin
  Camera := GetActiveCamera;
  if Camera <> nil then
  begin
    Camera.Flash := FlashCheckBox.Checked;
    CameraTimer.Enabled := True;
  end;
end;

procedure TCameraForm.ReconnectButtonClick(Sender: TObject);
begin
  ReconnectButton.Enabled := False;
  CameraList.Count := 0;
  UpdateCameraControls;
  FCameraManager.DetectCameras;
  CameraTimer.Enabled := True;
end;

procedure TCameraForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TCameraForm.CameraTimerTimer(Sender: TObject);
var
  ManagerBusy: Boolean;
  CameraBusy: Boolean;
  I: Integer;
begin
  if FCameraManager = nil then
  begin
    CameraTimer.Enabled := False;
    Exit;
  end;
  ManagerBusy := FCameraManager.Busy;
  CameraBusy := False;
  if not ManagerBusy then
    for I := FCameraManager.CameraCount - 1 downto 0 do
      if FCameraManager.Cameras[I].Busy then
      begin
        CameraBusy := True;
        Break;
      end;
  CameraTimer.Enabled := ManagerBusy or CameraBusy;
  ReconnectButton.Enabled := not CameraTimer.Enabled;
  if ManagerBusy then
  begin
    ProgressIndicator.Status := psBusy;
    ProgressIndicator.Caption := 'Searching for cameras';
  end
  else if CameraBusy then
  begin
    ProgressIndicator.Status := psBusy;
    ProgressIndicator.Caption := 'Processing command';
  end
  else
  begin
    ProgressIndicator.Status := psReady;
    if FCameraManager.CameraCount > 1 then
      ProgressIndicator.Caption := 'Cameras ready'
    else if FCameraManager.CameraCount = 1 then
      ProgressIndicator.Caption := 'Camera ready'
    else
    begin
      ProgressIndicator.Status := psError;
      ProgressIndicator.Caption := 'No cameras connected';
    end;
  end;
end;

procedure TCameraForm.CaptuteButtonClick(Sender: TObject);
begin
  FCameraManager.Snap;
  CameraTimer.Enabled := True;
end;

procedure TCameraForm.CameraListSelectItem(Sender: TObject);
begin
  UpdateCameraControls;
end;

procedure TCameraForm.CameraListDblClick(Sender: TObject);
var
  Camera: TCamera;
begin
  Camera := GetActiveCamera;
  if Camera <> nil then
  begin
    Camera.Snap;
    CameraTimer.Enabled := True;
  end;
end;

function TCameraForm.GetOffsets: TSize;
begin
  Result := inherited GetOffsets;
  Result.cx := 0;
end;

procedure TCameraForm.FormShow(Sender: TObject);
var
  S: TStringList;
  I: Integer;
begin
  if FCameraManager <> nil then
    if FCameraManager.CameraCount = 0 then
      FCameraManager.DetectCameras
    else
    begin
      for I := FCameraManager.CameraCount - 1 downto 0 do
        FCameraManager.Cameras[I].Mode := cmViewfinder;
      CameraManagerChange(FCameraManager);
    end;
  FPhotoView.Photos.Clear;
  if FDirectory = '' then Exit;
  FDirectory := IncludeTrailingPathDelimiter(FDirectory);
  S := TStringList.Create;
  try
    GetFileList(FDirectory, S, '*.jpg');
    FPhotoView.AddPhotos(S);
    for I := 0 to FPhotoView.Photos.Count - 1 do
      FPhotoView.Photos[I].Checked := False;
  finally
    S.Free;
  end;
  FMonitor.Directory := FDirectory;
  FMonitor.Start;
end;

procedure TCameraForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
{var
  I: Integer;}
begin
  FMonitor.Stop;
  CameraTimer.Enabled := False;
  {if FCameraManager <> nil then
    for I := FCameraManager.CameraCount - 1 downto 0 do
      FCameraManager.Cameras[I].Mode := cmRemote;}
end;



end.
