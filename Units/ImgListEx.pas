unit ImgListEx;

interface

uses
  Windows, Messages, CommCtrl, SysUtils, Classes, Graphics, ImgList,
  BaseTypes, GraphTools;
{$IFDEF GDIPLUS}
  GdiPlus,
{$ENDIF}

{ TTransparentImageList }

type
  TTransparentImageList = class;

  TTransparentImages = class(TPersistent)
  private
    FUpdateRef: Integer;
    FOwner: TTransparentImageList;
    procedure UpdateOwner;
    procedure DoRead(Stream: TStream);
    procedure DoWrite(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream); virtual; abstract;
    procedure WriteData(Stream: TStream); virtual; abstract;
    function GraphicClass: TGraphicClass; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetSize: Integer; virtual; abstract;
  public
    constructor Create(AOwner: TTransparentImageList); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromResourceID(ResID: Integer);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure Draw(Canvas: TCanvas; Index, X, Y: Integer;
      State: TDrawState = []); virtual; abstract;
    procedure DrawColor(Canvas: TCanvas; Index, X, Y: Integer;
      Color: TColor; Opacity: Byte = $FF); virtual;
    procedure Add(const FileName: string); overload;
    procedure Add(Files: TStrings); overload; virtual;
    procedure Add(Graphic: TGraphic); overload; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Remove(Index: Integer); virtual; abstract;
    procedure Move(CurIndex, NewIndex: Integer); virtual; abstract;
    property Count: Integer read GetCount;
    property Size: Integer read GetSize;
  end;

  TTransparentImagesClass = class of TTransparentImages;

  TTransparentImageList = class(TCustomImageList, IUnknown, IIndexedImages)
  private
    FUpdating: Boolean;
    FImages: TTransparentImages;
    procedure SetImages(Value: TTransparentImages);
    procedure ReadLeft(Reader: TReader);
    procedure ReadTop(Reader: TReader);
    procedure WriteLeft(Writer: TWriter);
    procedure WriteTop(Writer: TWriter);
  protected
    function IndexedImageGetSize: Integer;
    procedure IndexedImageDrawImage(Canvas: TCanvas; X, Y, Index: Integer;
      State: TDrawState = []);
    function IIndexedImages.GetSize = IndexedImageGetSize;
    procedure IIndexedImages.DrawImage = IndexedImageDrawImage;
    class function ImageClass: TTransparentImagesClass; virtual; abstract;
    procedure Change; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;
    procedure UpdateImages;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DrawImage(Canvas: TCanvas; X, Y, Index: Integer; State: TDrawState);
  published
    property Images: TTransparentImages read FImages write SetImages;
  end;

{ TImagesExtra }

{$IFDEF GDIPLUS}
  TImagesExtra = class(TTransparentImages)
  private
    FList: TList;
    function GetBitmap(Index: Integer): TBitmapPlus;
    procedure SetBitmap(Index: Integer; Value: TBitmapPlus);
  protected
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    function GraphicClass: TGraphicClass; override;
    function GetCount: Integer; override;
    function GetSize: Integer; override;
  public
    constructor Create(AOwner: TTransparentImageList); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Add(Graphic: TGraphic); override;
    procedure Clear; override;
    procedure Remove(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Draw(Canvas: TCanvas; Index, X, Y: Integer;
      State: TDrawState = []); override;
    property Bitmap[Index: Integer]: TBitmapPlus read GetBitmap write SetBitmap; default;
  end;

  TImageListExtra = class(TTransparentImageList)
  protected
    class function ImageClass: TTransparentImagesClass; override;
  end;
{$ENDIF}

  TGlassImages = class(TTransparentImages, IGlassImage)
  private
    FImage: IGlassImage;
  protected
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    function GraphicClass: TGraphicClass; override;
    function GetCount: Integer; override;
    function GetSize: Integer; override;
    property Image: IGlassImage read FImage implements IGlassImage;
  public
    constructor Create(AOwner: TTransparentImageList); override;
    procedure Assign(Source: TPersistent); override;
    procedure Add(Files: TStrings); override;
    procedure Add(Graphic: TGraphic); override;
    procedure Clear; override;
    procedure Remove(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    procedure Draw(Canvas: TCanvas; Index, X, Y: Integer;
      State: TDrawState = []); override;
    procedure DrawColor(Canvas: TCanvas; Index, X, Y: Integer;
      Color: TColor; Opacity: Byte = $FF); override;
  end;

  TGlassImageList = class(TTransparentImageList)
  protected
    class function ImageClass: TTransparentImagesClass; override;
  end;

implementation

constructor TTransparentImages.Create(AOwner: TTransparentImageList);
begin
  FOwner := AOwner;
end;

procedure TTransparentImages.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

procedure TTransparentImages.EndUpdate;
begin
  Dec(FUpdateRef);
  UpdateOwner;
end;

procedure TTransparentImages.UpdateOwner;
begin
  if (FOwner <> nil) and (FUpdateRef < 1) then
    FOwner.UpdateImages;
end;

procedure TTransparentImages.DoRead(Stream: TStream);
begin
  Inc(FUpdateRef);
  try
    Clear;
    ReadData(Stream);
  finally
    Dec(FUpdateRef);
  end;
end;

procedure TTransparentImages.DoWrite(Stream: TStream);
begin
  WriteData(Stream);
end;

procedure TTransparentImages.DefineProperties(Filer: TFiler);

  function CanWrite: Boolean;
  begin
    Result := Size > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', DoRead, DoWrite, CanWrite);
end;

procedure TTransparentImages.LoadFromStream(Stream: TStream);
begin
  BeginUpdate;
  try
    DoRead(Stream);
  finally
    EndUpdate;
  end;
end;

procedure TTransparentImages.LoadFromResourceID(ResID: Integer);
var
	Stream: TStream;
begin
	Stream := TResourceStream.CreateFromID(HInstance, ResID, RT_RCDATA);
  try
  	LoadFromStream(Stream);
  finally
  	Stream.Free;
  end;
end;

procedure TTransparentImages.SaveToStream(Stream: TStream);
begin
  DoWrite(Stream);
end;

procedure TTransparentImages.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTransparentImages.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TTransparentImages.Add(const FileName: string);
var
  G: TGraphic;
begin
  G := GraphicClass.Create;
  try
    G.LoadFromFile(FileName);
    Add(G);
  finally
    G.Free;
  end;
end;

procedure TTransparentImages.Add(Files: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Files.Count - 1 do
      Add(Files[I]);
  finally
    EndUpdate;
  end;
end;

procedure TTransparentImages.DrawColor(Canvas: TCanvas; Index, X, Y: Integer;
  Color: TColor; Opacity: Byte = $FF);
begin
end;

{ TTransparentImageList }

{$HINTS OFF}
type
  TComponentHack = class(TPersistent)
  private
    FOwner: TComponent;
    FName: TComponentName;
    FTag: Longint;
    FComponents: TList;
    FFreeNotifies: TList;
    FDesignInfo: Longint;
  end;
{$HINTS ON}

constructor TTransparentImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImages := ImageClass.Create(Self);
end;

destructor TTransparentImageList.Destroy;
begin
  Clear;
  FImages.Free;
  inherited Destroy;
end;

procedure TTransparentImageList.ReadLeft(Reader: TReader);
begin
  LongRec(TComponentHack(Self).FDesignInfo).Lo := Reader.ReadInteger;
end;

procedure TTransparentImageList.ReadTop(Reader: TReader);
begin
  LongRec(TComponentHack(Self).FDesignInfo).Hi := Reader.ReadInteger;
end;

procedure TTransparentImageList.WriteLeft(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(TComponentHack(Self).FDesignInfo).Lo);
end;

procedure TTransparentImageList.WriteTop(Writer: TWriter);
begin
  Writer.WriteInteger(LongRec(TComponentHack(Self).FDesignInfo).Hi);
end;

procedure TTransparentImageList.Change;
begin
  if not FUpdating then
    inherited Change;
end;

procedure TTransparentImageList.DefineProperties(Filer: TFiler);
var
  Ancestor: TComponentHack;
  Info: Longint;
begin
  Info := 0;
  Ancestor := TComponentHack(Filer.Ancestor);
  if Ancestor <> nil then Info := Ancestor.FDesignInfo;
  Filer.DefineProperty('Left', ReadLeft, WriteLeft,
    LongRec(TComponentHack(Self).FDesignInfo).Lo <> LongRec(Info).Lo);
  Filer.DefineProperty('Top', ReadTop, WriteTop,
    LongRec(TComponentHack(Self).FDesignInfo).Hi <> LongRec(Info).Hi);
end;

procedure TTransparentImageList.Assign(Source: TPersistent);
var
  ImageList: TTransparentImageList absolute Source;
begin
  if Source is TTransparentImageList then
    FImages.Assign(ImageList.FImages)
  else
    inherited Assign(Source);
end;

procedure TTransparentImageList.UpdateImages;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    FUpdating := True;
    try
      Clear;
      if FImages.Count < 1 then Exit;
        Bitmap.Width := FImages.Count;
      Bitmap.Height := 1;
      Width := 1;
      Height := 1;
      Add(Bitmap, nil);
      PInteger(@Width)^ := FImages.Size;
      PInteger(@Height)^ := FImages.Size;
    finally
      FUpdating := False;
      Change;
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure TTransparentImageList.DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
  Style: Cardinal; Enabled: Boolean = True);
var
  State: TDrawState;
begin
  State := [];
  if not Enabled then
    State := [dsDisabled]
  else if Style and ILD_FOCUS = ILD_FOCUS then
    State := [dsHot]
  else if Style and ILD_PRESSED = ILD_PRESSED then
    State := [dsPressed];
  FImages.Draw(Canvas, Index, X, Y, State);
end;

procedure TTransparentImageList.DrawImage(Canvas: TCanvas; X, Y, Index: Integer;
  State: TDrawState);
begin
  FImages.Draw(Canvas, Index, X, Y, State);
end;

procedure TTransparentImageList.Loaded;
begin
  inherited Loaded;
  UpdateImages;
end;

procedure TTransparentImageList.SetImages(Value: TTransparentImages);
begin
  FImages.Assign(Value);
end;

function TTransparentImageList.IndexedImageGetSize: Integer;
begin
  Result := FImages.Size;
end;

procedure TTransparentImageList.IndexedImageDrawImage(Canvas: TCanvas;
  X, Y, Index: Integer; State: TDrawState = []);
begin
  DrawImage(Canvas, X, Y, Index, State);
end;


{ TImagesExtra }

{$IFDEF GDIPLUS}
constructor TImagesExtra.Create(AOwner: TTransparentImageList);
begin
  inherited Create(AOwner);
  FList := TList.Create;
end;

destructor TImagesExtra.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TImagesExtra.Assign(Source: TPersistent);
var
  Images: TImagesExtra absolute Source;
  B: TBitmapPlus;
  I: Integer;
begin
  if Source is TImagesExtra then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to Images.Count - 1 do
      begin
        B := TBitmapPlus.Create;
        B.Assign(Images[I]);
        Add(B);
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;


function TImagesExtra.GetBitmap(Index: Integer): TBitmapPlus;
begin
  Result := TBitmapPlus(FList[Index]);
end;

procedure TImagesExtra.SetBitmap(Index: Integer; Value: TBitmapPlus);
begin
  TBitmapPlus(FList[Index]).Assign(Value);
end;

procedure TImagesExtra.ReadData(Stream: TStream);
var
  A, B: TBitmapPlus;
  G: IGraphics;
  P: TPoint;
  I: Integer;
begin
  Clear;
  A := TBitmapPlus.Create;
  try
    A.LoadFromStream(Stream);
    P.Y := A.Height;
    P.X := A.Width div P.Y;
    for I := 0 to P.X - 1 do
    begin
      B := TBitmapPlus.Create;
      B.Width := P.Y;
      B.Height := P.Y;
      G := NewGraphics(B.Bitmap);
      G.DrawImage(A.Bitmap, 0, 0, I * P.Y, 0, P.Y, P.Y);
      FList.Add(B);
    end;
  finally
    A.Free;
  end;
end;

procedure TImagesExtra.WriteData(Stream: TStream);
var
  B: TBitmapPlus;
  G: IGraphics;
  P: TPoint;
  I: Integer;
begin
  if Count = 0 then Exit;
  B := TBitmapPlus.Create;
  try
    P.Y := Size;
    P.X := Count;
    B.Width := P.X * P.Y;
    B.Height := P.Y;
    G := NewGraphics(B.Bitmap);
    for I := 0 to P.X - 1 do
      G.DrawImage(Bitmap[I].Bitmap, P.Y * I, 0, P.Y, P.Y);
    B.SaveToStream(Stream);
  finally
    B.Free;
  end;
end;

function TImagesExtra.GraphicClass: TGraphicClass;
begin
  Result := TBitmapPlus;
end;

function TImagesExtra.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TImagesExtra.GetSize: Integer;
begin
  if FList.Count = 0 then
    Result := 0
  else
    Result := TBitmapPlus(FList[0]).Height;
end;

procedure TImagesExtra.Add(Graphic: TGraphic);
var
  A, B: TBitmapPlus;
  D: TRectI;
  G: IGraphics;
  P: TPoint;
  I: Integer;
begin
  if Graphic is TBitmapPlus then
  begin
    BeginUpdate;
    try
      A := TBitmapPlus(Graphic);
      P.Y := A.Height;
      P.X := A.Width div P.Y;
      D := NewRect(P.Y, P.Y);
      for I := 0 to P.X - 1 do
      begin
        B := TBitmapPlus.Create;
        B.Width := P.Y;
        B.Height := P.Y;
        G := NewGraphics(B.Bitmap);
        G.DrawImage(A.Bitmap, D, I * P.Y, 0, P.Y, P.Y);
        FList.Add(B);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TImagesExtra.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to FList.Count - 1 do
      TObject(FList[I]).Free;
    FList.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TImagesExtra.Remove(Index: Integer);
var
  B: TBitmapPlus;
begin
  BeginUpdate;
  try
    B := Bitmap[Index];
    FList.Delete(Index);
    B.Free;
  finally
    EndUpdate;
  end;
end;

procedure TImagesExtra.Move(CurIndex, NewIndex: Integer);
begin
  BeginUpdate;
  try
    FList.Move(CurIndex, NewIndex);
  finally
    EndUpdate;
  end;
end;

procedure TImagesExtra.Draw(Canvas: TCanvas; Index, X, Y: Integer;
  State: TDrawState = []);
var
  C: TColorTransform;
  S, D: TRect;
begin
  if (Index < 0) or (Index > Count - 1) then Exit;
  C := NewColorTransform;
  if dsDisabled in State then
  begin
    C.Greyscale := True;
    C.Opacity := 0.5;
  end
  else if dsHot in State then
    if dsPressed in State then
      C.Brightness := -0.1
    else
      C.Brightness := 0.1;
  S := Rect(0, 0, Size, Size);
  D := S;
  OffsetRect(D, X, Y);
  with Bitmap[Index] do
  begin
    Transform(C);
    Blit(GraphicsFromCanvas(Canvas), S, D);
  end;
end;

{ TImageListExtra }

class function TImageListExtra.ImageClass: TTransparentImagesClass;
begin
  Result := TImagesExtra;
end;
{$ENDIF}

{ TGlassImages }

constructor TGlassImages.Create(AOwner: TTransparentImageList);
begin
  inherited Create(AOwner);
  FImage := CreateGlassImage;
end;

procedure TGlassImages.Assign(Source: TPersistent);
var
  G: TGlassImages absolute Source;
begin
  if Source is TGlassImages then
  begin
    BeginUpdate;
    try
      FImage.Graphic.Assign(G.FImage.Graphic);
    finally
      EndUpdate;
    end;
  end
  else if Source.InheritsFrom(GraphicClass) then
  begin
    BeginUpdate;
    try
      FImage.Graphic.Assign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TGlassImages.ReadData(Stream: TStream);
begin
  FImage.Graphic.LoadFromStream(Stream);
end;

procedure TGlassImages.WriteData(Stream: TStream);
begin
  FImage.Graphic.SaveToStream(Stream);
end;

function TGlassImages.GraphicClass: TGraphicClass;
begin
  Result := TGraphicClass(FImage.Graphic.ClassType);
end;

function TGlassImages.GetCount: Integer;
var
  G: TGraphic;
begin
  G := FImage.Graphic;
  if (G.Height = 0) or (G.Width = 0) then
    Result := 0
  else if  G.Height > G.Width then
    Result := G.Height div G.Width
  else
    Result := G.Width div G.Height;
end;

function TGlassImages.GetSize: Integer;
var
  G: TGraphic;
begin
  G := FImage.Graphic;
  if (G.Height = 0) or (G.Width = 0) then
    Result := 0
  else if  G.Height > G.Width then
    Result := G.Width
  else
    Result := G.Height;
end;

procedure TGlassImages.Add(Files: TStrings);
var
  A, B: IGlassImage;
  D: INativeDraw;
  R: TRect;
  I, J: Integer;
begin
  if Files.Count = 1 then
  begin
    Add(Files[0]);
    Exit;
  end;
  A := CreateGlassImage;
  J := 0;
  R := Rect(0, 0, 0, 0);
  for I := 0 to Files.Count - 1 do
  begin
    B := CreateGlassImage;
    B.Graphic.LoadFromFile(Files[I]);
    if I = 0 then
    begin
      J := B.Graphic.Height;
      R := Rect(0, 0, J, J);
      A.Graphic.Width := J * Files.Count;
      A.Graphic.Height := J;
    end;
    if Supports(B, INativeDraw, D) then
      D.DrawAlpha(R, A.DC, I * J, 0);
  end;
  if J > 0 then
    Add(A.Graphic);
end;

procedure TGlassImages.Add(Graphic: TGraphic);
var
  A, B: IGlassImage;
  D: INativeDraw;
  R: TRect;
  I, J: Integer;
begin
  BeginUpdate;
  try
    I := Count;
    J := Size;
    if I = 0 then
      FImage.Graphic.Assign(Graphic)
    else
    begin
      A := CreateGlassImage;
      A.Graphic.Assign(Graphic);
      B := CreateGlassImage;
      B.Graphic.Width := (I * J) + A.Graphic.Width;
      B.Graphic.Height := J;
      if Supports(FImage, INativeDraw, D) then
      begin
        R := Rect(0, 0, I * J, J);
        D.DrawAlpha(R, B.DC, 0, 0);
      end;
      if Supports(A, INativeDraw, D) then
      begin
        R := Rect(0, 0, A.Graphic.Width, A.Graphic.Height);
        D.DrawAlpha(R, B.DC, I * J, 0);
      end;
      FImage := B;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TGlassImages.Clear;
begin
  BeginUpdate;
  try
    FImage := CreateGlassImage;
  finally
    EndUpdate;
  end;
end;

procedure TGlassImages.Remove(Index: Integer);
var
  A: IGlassImage;
  D: INativeDraw;
  R: TRect;
  I, J: Integer;
begin
  I := Count;
  J := Size;
  if (Index < 0) or (Index > I - 1) then Exit;
  if I = 1 then
    Clear
  else if Supports(FImage, INativeDraw, D) then
  begin
    BeginUpdate;
    try
      A := CreateGlassImage;
      A.Graphic.Width := (I - 1) * J;
      A.Graphic.Height := J;
      if (Index = 0) or (Index = I - 1) then
      begin
        R := Rect(0, 0, A.Graphic.Width, J);
        if Index = 0 then
          OffsetRect(R, J, 0);
        D.DrawAlpha(R, A.DC, 0, 0);
      end
      else
      begin
        R := Rect(0, 0, J * Index, J);
        D.DrawAlpha(R, A.DC, 0, 0);
        R := Rect(J * (Index + 1), 0, A.Graphic.Width + J, J);
        D.DrawAlpha(R, A.DC, J * Index, 0);
      end;
      FImage := A;
    finally
      EndUpdate;
    end;
  end;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

procedure TGlassImages.Move(CurIndex, NewIndex: Integer);
var
  A: IGlassImage;
  D: INativeDraw;
  R: TRect;
  I, J: Integer;
begin
  I := Count;
  J := Size;
  if (CurIndex < 0) or (CurIndex > I - 1) or (NewIndex < 0) or
    (NewIndex > I - 1) or (CurIndex = NewIndex) then Exit;
  if Supports(FImage, INativeDraw, D) then
  begin
    BeginUpdate;
    try
      A := CreateGlassImage;
      A.Graphic.Width := J * I;
      A.Graphic.Height := J;
      if Min(CurIndex, NewIndex) > 0 then
      begin
        R := Rect(0, 0, Min(CurIndex, NewIndex) * J, J);
        D.DrawAlpha(R, A.DC, 0, 0);
      end;
      R := Rect(CurIndex * J, 0, (CurIndex + 1 ) * J, J);
      D.DrawAlpha(R, A.DC, NewIndex * J, 0);
      if CurIndex > NewIndex then
      begin
        R := Rect(NewIndex * J, 0, CurIndex * J, J);
        D.DrawAlpha(R, A.DC, (NewIndex + 1) * J, 0);
      end
      else
      begin
        R := Rect((CurIndex + 1) * J, 0, (NewIndex + 1) * J, J);
        D.DrawAlpha(R, A.DC, CurIndex * J, 0);
      end;
      if Max(CurIndex, NewIndex) < I - 1 then
      begin
        R := Rect((Max(CurIndex, NewIndex) + 1) * J, 0, A.Graphic.Width, J);
        D.DrawAlpha(R, A.DC, (Max(CurIndex, NewIndex) + 1) * J, 0);
      end;
      FImage := A;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TGlassImages.Draw(Canvas: TCanvas; Index, X, Y: Integer;
  State: TDrawState = []);
var
  G: TGraphic;
  R: TRect;
  I: Integer;
  D: INativeDraw;
begin
  if (Index < 0) or (Index > Count - 1) then Exit;
  G := FImage.Graphic;
  I := Size;
  R := Rect(0, 0, I, I);
  if G.Height > G.Width then
    OffsetRect(R, 0, I * Index)
  else
    OffsetRect(R, I * Index, 0);
  if Supports(FImage, INativeDraw, D) then
    D.DrawState(R, Canvas.Handle, X, Y, State);
end;

procedure TGlassImages.DrawColor(Canvas: TCanvas; Index, X, Y: Integer;
  Color: TColor; Opacity: Byte = $FF);
var
  G: TGraphic;
  R: TRect;
  I: Integer;
  D: INativeDraw;
begin
  if (Index < 0) or (Index > Count - 1) then Exit;
  G := FImage.Graphic;
  I := Size;
  R := Rect(0, 0, I, I);
  if G.Height > G.Width then
    OffsetRect(R, 0, I * Index)
  else
    OffsetRect(R, I * Index, 0);
  if Supports(FImage, INativeDraw, D) then
    D.DrawColor(R, Canvas.Handle, X, Y, Color, Opacity);
end;

{ TGlassImageList }

class function TGlassImageList.ImageClass: TTransparentImagesClass;
begin
  Result := TGlassImages;
end;

procedure PatchINT3;
var
  NOP: Byte;
  BytesWritten: DWORD;
  NtDll: THandle;
  P: Pointer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;
  NtDll := GetModuleHandle('NTDLL.DLL');
  if NtDll = 0 then
    Exit;
  P := GetProcAddress(NtDll, 'DbgBreakPoint');
  if P = nil then
    Exit;
  try
    if Char(P^) <> #$CC then
      Exit;
    NOP := $90;
    if WriteProcessMemory(GetCurrentProcess, P, @NOP, 1, BytesWritten) and
      (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, P, 1);
  except
    on EAccessViolation do ;
    else raise;
  end;
end;

end.
