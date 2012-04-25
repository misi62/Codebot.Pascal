
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit BlendTools;

interface

{$I CODEBOT.INC}

uses
	Windows, Messages, SysUtils, Classes, Graphics, GraphTools, WinTools,
  CommCtrl, SysTools, BaseTypes;

{ TAlphaMap }

type
	TAlphaMap = class(TBitmap)
  private
    FOpacity: Byte;
  public
  	constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure CopyMask(Bitmap: TBitmap; Color: TColor);
		procedure CommitOpacity;
    procedure FillMask(Color: TColor);
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromResourceID(ResID: Integer);
    procedure LoadFromResourceName(const ResName: string);
    procedure SaveToStream(Stream: TStream); override;
    procedure Opaque(const Rect: TRect);
    property Opacity: Byte read FOpacity write FOpacity;
  end;

{$IFDEF AlphaImageList}
	TAlphaImageList = class(TTransparentImage)
  private
  	FImages: IUnknown;
  protected
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X, Y: Integer;
      Style: Cardinal; Enabled: Boolean = True); override;
    procedure Initialize; override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
	public
		procedure AddAlpha(const FileName: string); overload;
		procedure AddAlpha(AlphaMap: TAlphaMap); overload;
  	procedure Load(const FileName: string); override;
    procedure Unload; override;
  end;
{$ENDIF}

{ THotWindow }

  THotWindow = class(TUtilityWindow)
  protected
		procedure CreateInfo(var Info: TCreateInfo); override;
  end;

{ THotTracker }

	THotTracker = class(TObject)
  private
    FAssociate: HWND;
    FClipped: Boolean;
  	FHotWindow: THotWindow;
    FStandardBlur: Boolean;
    FStandardBlurImage: TAlphaImage;
    FBlur: Double;
    FBorder: Integer;
    FThickness: Integer;
    FColor: TColor;
    FOpacity: Byte;
    FRadius: Integer;
    FX: Integer;
    FY: Integer;
    FWasVisible: Boolean;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetHandle: THandle;
    procedure WMActivateApp(var Msg: TWMActivateApp); message WM_ACTIVATEAPP;
  public
  	constructor Create;
    destructor Destroy; override;
  	procedure Update(NewRect: PRect = nil; NewPoint: PPoint = nil);
    procedure Move(NewPoint: PPoint = nil);
    property StandardBlur: Boolean read FStandardBlur write FStandardBlur;
    property Border: Integer read FBorder write FBorder;
    property Thickness: Integer read FThickness write FThickness;
		property Color: TColor read FColor write FColor;
    property Clipped: Boolean read FClipped write FClipped;
    property Blur: Double read FBlur write FBlur;
    property Opacity: Byte read FOpacity write FOpacity;
    property Handle: THandle read GetHandle;
    property Radius: Integer read FRadius write FRadius;
    property Visible: Boolean read GetVisible write SetVisible;
    property Associate: HWND read FAssociate write FAssociate;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

{ TFocusTracker }

	TFocusTracker = class(THotTracker)
  private
  	FEnabled: Boolean;
    FTranslateKeys: Boolean;
    procedure SetEnabled(Value: Boolean);
		procedure MessageHook(const Msg: TCWPStruct);
		procedure KeyboardHook(Key: Word; State: Cardinal; var Remove: Boolean);
  public
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property TranslateKeys: Boolean read FTranslateKeys write FTranslateKeys;
	end;

{ TLayeredImage }

  TLayeredImage = class(TObject)
    FAssociate: HWND;
    FHotWindow: TUtilityWindow;
    FX: Integer;
    FY: Integer;
    procedure Show(Value: Boolean);
    procedure Move;
		procedure MessageHook(const Msg: TCWPStruct);
  public
    constructor Create(Associate: HWND; Bitmap: TFastBitmap; X, Y: Integer);
    destructor Destroy; override;
  end;

procedure LoadAlphaWindow(Wnd: HWND; const FileName: string; Opacity: Byte = $FF);
procedure UpdateAlphaWindow(Wnd: HWND; Image: TGraphic; Canvas: TCanvas; Opacity: Byte = $FF); overload;
procedure UpdateAlphaWindow(Wnd: HWND; const Bitmap: TFastBitmap; Opacity: Byte = $FF); overload;

procedure BlurBitmap(Bitmap: TBitmap; const Radius: Double); overload;
procedure BlurBitmap(Bits: PRGB; W, H: Integer; const Radius: Double); overload;
procedure DrawBlurBubble(Bitmap: TBitmap; W, H, Thickness, Radius: Integer; Blur: Double);

function FocusTracker: TFocusTracker;

implementation

var
  Black: TRGBA;

{ TAlphaMap }

constructor TAlphaMap.Create;
begin
  inherited Create;
	PixelFormat := pf32bit;
  FOpacity := $FF;
end;

procedure TAlphaMap.Assign(Source: TPersistent);
var
	AlphaMap: TAlphaMap absolute Source;
  I: Integer;
begin
	if Source is TAlphaMap then
  begin
  	Opacity := AlphaMap.Opacity;
    for I := 0 to Height - 1 do
    	Move(AlphaMap.ScanLine[I]^, ScanLine[I]^, Width * SizeOf(TRGBA));
  end
  else
	  inherited Assign(Source);
	PixelFormat := pf32bit;
end;

procedure TAlphaMap.CopyMask(Bitmap: TBitmap; Color: TColor);
var
  Fill: TRGBA;
	Swap: Byte;
  A: PRGBA;
  B: PRGB;
  X, Y: Integer;
begin
	if Bitmap.PixelFormat <> pf24Bit then Exit;
	Width := Bitmap.Width;
  Height := Bitmap.Height;
  Fill := TRGBA(ColorToRGB(Color));
  Swap := Fill.Blue;
  Fill.Blue := Fill.Red;
  Fill.Red := Swap;
  for Y := 0 to Height - 1 do
  begin
    A := ScanLine[Y];
    B := Bitmap.Scanline[Y];
    for X := 0 to Width - 1 do
    begin
    	if B.Blue = 0 then
      	A^ := Black
			{ begin new code }
			else if B.Blue > $A0 then
      begin
      	PRGB(A)^ := PRGB(@Fill)^;
				A.Alpha := B.Blue;
			end
			{ end new code }
      else
      begin
  	  	A.Blue := Round(Fill.Blue * (B.Blue / $FF));
  	  	A.Green := Round(Fill.Green * (B.Green / $FF));
  	  	A.Red := Round(Fill.Red * (B.Red / $FF));
				A.Alpha := B.Blue;
			end;
      Inc(A);
      Inc(B);
    end;
	end;
end;

procedure TAlphaMap.CommitOpacity;
var
	A: PRGBA;
  X, Y: Integer;
begin
  for Y := 0 to Height - 1 do
  begin
    A := ScanLine[Y];
    for X := 0 to Width - 1 do
    begin
    	A.Alpha := FOpacity;
      Inc(A);
		end;
	end;
end;

procedure TAlphaMap.FillMask(Color: TColor);
var
  RGBA: TRGBA;
  RGB: TRGB absolute RGBA;
	Swap: Byte;
  A: PRGBA;
  X, Y: Integer;
begin
  RGBA := TRGBA(ColorToRGB(Color));
  Swap := RGBA.Blue;
  RGBA.Blue := RGBA.Red;
  RGBA.Red := Swap;
  for Y := 0 to Height - 1 do
  begin
    A := ScanLine[Y];
    for X := 0 to Width - 1 do
    begin
    	if A.Alpha > 0 then
				PRGB(A)^ := RGB;
      Inc(A);
    end;
	end;
end;

procedure TAlphaMap.LoadFromStream(Stream: TStream);
var
	Image: IGlassImage;
  A, B: PByte;
  Row: Integer;
begin
	Image := CreateGlassImage;
  Image.Graphic.LoadFromStream(Stream);
	Height := Image.Graphic.Height;
	Width := Image.Graphic.Width;
	for Row := 0 to Height - 1 do
	begin
		A := ScanLine[Row];
		B := Image.Scanline[Height - Row - 1];
    Move(B^, A^, Image.Stride);
	end;
end;

procedure TAlphaMap.LoadFromResourceID(ResID: Integer);
var
  Lib: THandle;
	Stream: TStream;
begin
  if IsLibrary then
    Lib := HInstance
  else
    Lib := MainInstance;
	Stream := TResourceStream.CreateFromID(Lib, ResID, RT_RCDATA);
  try
  	LoadFromStream(Stream);
  finally
  	Stream.Free;
  end;
end;

procedure TAlphaMap.LoadFromResourceName(const ResName: string);
var
  Lib: THandle;
	Stream: TStream;
begin
  if IsLibrary then
    Lib := HInstance
  else
    Lib := MainInstance;
	Stream := TResourceStream.Create(Lib, ResName, RT_RCDATA);
  try
  	LoadFromStream(Stream);
  finally
  	Stream.Free;
  end;
end;

procedure TAlphaMap.SaveToStream(Stream: TStream);
var
	Image: IGlassImage;
  A, B: PByte;
  Row: Integer;
begin
	Image := CreateGlassImage;
	Image.Graphic.Height := Height;
	Image.Graphic.Width := Width;
	for Row := 0 to Height - 1 do
	begin
		A := ScanLine[Row];
		B := Image.Scanline[Height - Row - 1];
    Move(A^, B^, Image.Stride);
	end;
  Image.Graphic.SaveToStream(Stream);
end;

procedure TAlphaMap.Opaque(const Rect: TRect);
var
	A: PRGBA;
  Row, Col: Integer;
begin
	if (Rect.Left < 0) or (Rect.Top < 0) or (Rect.Right > Width) or
  	(Rect.Bottom > Height) then	Exit;
	for Row := Rect.Top to Rect.Bottom - 1 do
	begin
		A := ScanLine[Row];
    Inc(A, Rect.Left);
    for Col := 0 to WidthOf(Rect) - 1 do
    begin
      A.Alpha := $FF;
    	Inc(A);
    end;
	end;
end;

{$IFDEF AlphaImageList}

{ IImageList }

procedure TAlphaImageList.Initialize;
begin
  inherited Initialize;
  Handle := ImageList_Create(Width, Height, ILC_COLOR32, AllocBy, AllocBy);
	ImageListQueryInterface(Self, IImageList, FImages);
end;

procedure TAlphaImageList.ReadData(Stream: TStream);
var
	Adapter: TStreamAdapter;
  Obj: IImageList;
begin
  Adapter := TStreamAdapter.Create(Stream);
  try
    Handle := ImageList_Read(Adapter);
	  if ImageListQueryInterface(Self, IImageList, FImages) = S_OK then
    begin
		  Obj := FImages as IImageList;
			Obj.GetIconSize(PInteger(@Width)^, PInteger(@Height)^);
		end;
  finally
    Adapter.Free;
  end;
end;

procedure TAlphaImageList.WriteData(Stream: TStream);
var
	Adapter: TStreamAdapter;
begin
  Adapter := TStreamAdapter.Create(Stream);
  try
    ImageList_Write(Handle, Adapter);
  finally
    Adapter.Free;
  end;
end;

procedure TAlphaImageList.AddAlpha(AlphaMap: TAlphaMap);
var
	Count, Size: Integer;
  Obj: IImageList;
begin
	if (AlphaMap.Height < 1) or (AlphaMap.Width < 1) then
  	Exit;
	if AlphaMap.Height > AlphaMap.Width then
  begin
  	Count := AlphaMap.Height div AlphaMap.Width;
    Size := AlphaMap.Width;
	end
	else
  begin
  	Count := AlphaMap.Width div AlphaMap.Height;
    Size := AlphaMap.Height;
	end;
  Clear;
  if FImages <> nil then
  begin
		PInteger(@Width)^ := Size;
		PInteger(@Height)^ := Size;
	  Obj := FImages as IImageList;
		Obj.SetIconSize(Size, Size);
	  Obj.Add(AlphaMap.Handle, AlphaMap.Handle, Count);
  	Change;
	end
  else
  begin
		PInteger(@Width)^ := Size;
		PInteger(@Height)^ := Size;
    ImageList_SetIconSize(Handle, Size, Size);
    ImageList_Add(Handle, AlphaMap.Handle, 0);
  	Change;
  end;
end;

procedure TAlphaImageList.AddAlpha(const FileName: string);
var
	AlphaMap: TAlphaMap;
begin
  AlphaMap := TAlphaMap.Create;
  try
  	AlphaMap.LoadFromFile(FileName);
    AddAlpha(AlphaMap);
  finally
  	AlphaMap.Free;
  end;
end;

procedure TAlphaImageList.DoDraw(Index: Integer; Canvas: TCanvas; X,
  Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  Params: TImageListDrawParams;
  Obj: IImageList;
begin
  if FImages <> nil then
  begin
	 	FillChar(Params, SizeOf(Params), #0);
	 	Params.cbSize := SizeOf(Params);
		Params.himl := Handle;
		Params.i := Index;
		Params.hdcDst := Canvas.Handle;
		Params.x := X;
		Params.y := Y;
		Params.rgbBk := CLR_NONE;
		if not Enabled then
			Params.fStyle := 1
		else if Style and ILD_FOCUS = ILD_FOCUS then
			Params.fStyle := 0
		else
	    Params.fStyle := ILD_BLEND25;
		if not Enabled then
	    Params.fState := 4;
		Obj := FImages as IImageList;
	 	Obj.Draw(Params);
    if Enabled and (Style and ILD_FOCUS <> ILD_FOCUS) then
    begin
		 	Obj.Draw(Params);
		 	Obj.Draw(Params);
		end;
  end
  else
  	inherited;
end;

procedure TAlphaImageList.Load(const FileName: string);
begin
	AddAlpha(FileName);
end;

procedure TAlphaImageList.Unload;
begin
	Clear;
end;
{$ENDIF}

{ THotWindow }

procedure THotWindow.CreateInfo(var Info: TCreateInfo);
begin
	inherited CreateInfo(Info);
  if Info.Parent = 0 then
		Info.Style := WS_POPUP or WS_DISABLED
  else
		Info.Style := WS_CHILD or WS_DISABLED;
	Info.ExStyle := WS_EX_TOPMOST	or WS_EX_TOOLWINDOW or WS_EX_TRANSPARENT;
end;

{ THotTracker }

constructor THotTracker.Create;
begin
	inherited Create;
	FHotWindow := THotWindow.Create(Self);
  FColor := clHighlight;
	FBlur := 2;
  FBorder := 15;
	FOpacity := $40;
	FRadius := 15;
end;

destructor THotTracker.Destroy;
begin
  FHotWindow.Free;
  FStandardBlurImage.Free;
  inherited Destroy;
end;

procedure THotTracker.Move(NewPoint: PPoint = nil);
var
	A, B: TRect;
begin
  if not IsWindow(FAssociate) then Exit;
  if NewPoint = nil then
  begin
		GetWindowRect(FAssociate, A);
  	GetWindowRect(FHotWindow.Handle, B);
	  MoveWindow(FHotWindow.Handle, FX + A.Left + Round((WidthOf(A) - WidthOf(B)) / 2),
		  FY + A.Top + Round((HeightOf(A) - HeightOf(B)) / 2), WidthOf(B),
    	HeightOf(B), False);
	end
  else
  	SetWindowPos(FHotWindow.Handle, FAssociate, NewPoint.X, NewPoint.Y, 0, 0,
    	SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
end;

procedure InvertAlphaImage(Image: TAlphaImage);
var
  P: PRGBA;
  X, Y: Integer;
begin
	if IsFastBitmap(Image.Bitmap) and (Image.Bitmap.Depth = pd32) then
  begin
    P := Image.Bitmap.Bits;
    for X := 0 to Image.Bitmap.Width - 1 do
      for Y := 0 to Image.Bitmap.Height - 1 do
      begin
        P.Alpha := P.Red;
        P.Red := 0;
        P.Green := 0;
        P.Blue := 0;
        Inc(P);
      end;
	end;
end;

procedure CreateBlur(Image: TAlphaImage; Width, Height, Thickness, Radius: Integer;
  Blur: Double);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
	try
		DrawBlurBubble(Bitmap, Width, Height, Thickness, Radius, Blur);
    Image.Width := Bitmap.Width;
    Image.Height := Bitmap.Height;
    Image.Canvas.Draw(0, 0, Bitmap);
 finally
		Bitmap.Free;
  end;
  InvertAlphaImage(Image);
end;

procedure THotTracker.Update(NewRect: PRect = nil; NewPoint: PPoint = nil);
var
	Rect: TRect;
  Image: TAlphaImage;
  W, H: Integer;
  A, B, C: HRGN;
begin
  if not IsWindow(FAssociate) then Exit;
  if NewRect <> nil then
  	Rect := NewRect^
	else
	  GetWindowRect(FAssociate, Rect);
  W := WidthOf(Rect) + FBorder + FThickness;
  H := HeightOf(Rect) + FBorder + FThickness;
  Image := TAlphaImage.Create;
  try
    if FStandardBlur then
    begin
      if FStandardBlurImage = nil then
      begin
        FStandardBlurImage := TAlphaImage.Create;
        CreateBlur(FStandardBlurImage, 100, 100, 0, 5, 4);
      end;
      Image.Width := W + 12;
      Image.Height := H + 12;
      FStandardBlurImage.Blit(Image.Bitmap.DC, Image.Bounds, 8);
    end
    else
      CreateBlur(Image, W, H, FThickness, FRadius, FBlur);
    W := Image.Width;
    H := Image.Height;
    { Changed to lighten up shadows in other controls }
		UpdateAlphaWindow(FHotWindow.Handle, Image.Bitmap, FOpacity div 2);
	finally
		Image.Free;
	end;
  if FClipped then
  begin
  	A := CreateRectRgn(0, 0, WidthOf(Rect), HeightOf(Rect));
    B := CreateRectRgn(0, 0, W, H);
    W := (W - WidthOf(Rect));
    if Odd(W) then Inc(W);
    H := (H - HeightOf(Rect));
    if Odd(H) then Inc(H);
    OffsetRgn(A, W div 2 - FX, H div 2 - FY);
  	C := CreateRectRgn(0, 0, 1, 1);
    CombineRgn(C, A, B, RGN_XOR);
    SetWindowRgn(FHotWindow.Handle, C, False);
    DeleteObject(A);
    DeleteObject(B);
  end;
  Move(NewPoint);
	Visible := True;
end;

function THotTracker.GetHandle: THandle;
begin
	Result := FHotWindow.Handle;
end;

function THotTracker.GetVisible: Boolean;
begin
	Result := IsWindowVisible(FHotWindow.Handle);
end;

procedure THotTracker.SetVisible(const Value: Boolean);
begin
	if Value <> Visible then
		if Value then
  		ShowWindow(FHotWindow.Handle, SW_SHOWNOACTIVATE)
		else
			ShowWindow(FHotWindow.Handle, SW_HIDE);
end;

procedure THotTracker.WMActivateApp(var Msg: TWMActivateApp);
begin
	inherited;
  if FAssociate <> 0 then
    if Msg.Active then
    	if FWasVisible then
      	Visible := True
  		else
	  else
    begin
    	FWasVisible := Visible;
      Visible := False;
    end;
end;

{ TFocusTracker }

destructor TFocusTracker.Destroy;
begin
	Enabled := False;
  inherited Destroy;
end;

procedure TFocusTracker.KeyboardHook(Key: Word; State: Cardinal;
  var Remove: Boolean);
begin
	if not FTranslateKeys then Exit;
	if (State and  $C0000000 = $C0000000) and (Key = VK_RETURN) then
  begin
		keybd_event(VK_TAB, 0, 0, 0);
		keybd_event(VK_TAB, 0, KEYEVENTF_KEYUP, 0);
  end;
end;

procedure TFocusTracker.MessageHook(const Msg: TCWPStruct);
begin
	case Msg.message of
    WM_SETFOCUS:
	    if IsProcessWindow(Msg.hwnd) then
  	  begin
    		Associate := Msg.hwnd;
				Update;
	    end;
    WM_WINDOWPOSCHANGING:
    	if IsChild(Msg.hwnd, Associate) then
      	Move;
    WM_KILLFOCUS:
      Visible := IsProcessWindow(Msg.wParam);
		WM_SIZE:
    	if Msg.hwnd = Associate then
      	Update;
  end;
end;

procedure TFocusTracker.SetEnabled(Value: Boolean);
begin
	if Value <> FEnabled then
  begin
  	FEnabled := Value;
    if FEnabled then
    begin
		  HookMessage(MessageHook);
		  HookKeyboard(KeyboardHook);
    end
    else
    begin
			UnhookMessage(MessageHook);
		  UnhookKeyboard(KeyboardHook);
      Visible := False;
    end;
	end;
end;

{ TLayeredImage }

constructor TLayeredImage.Create(Associate: HWND; Bitmap: TFastBitmap; X, Y: Integer);
begin
  inherited Create;
  FAssociate := Associate;
  FHotWindow := TUtilityWindow.Create(Self, 0, WS_POPUP or WS_DISABLED, WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE or WS_EX_TRANSPARENT);;
  FX := X;
  FY := Y;
  ShowWindow(FHotWindow.Handle, SW_SHOW);
  SetWindowPos(FHotWindow.Handle, 0, 0, 0, Bitmap.Width, Bitmap.Height,
    SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
  UpdateAlphaWindow(FHotWindow.Handle, Bitmap);
  HookMessage(MessageHook);
  Move;
  Show(IsWindowVisible(FAssociate));
end;

destructor TLayeredImage.Destroy;
begin
  UnhookMessage(MessageHook);
  FHotWindow.Free;
  inherited Destroy;
end;

procedure TLayeredImage.Show(Value: Boolean);
begin
  if Value then
    ShowWindow(FHotWindow.Handle, SW_SHOW)
  else
    ShowWindow(FHotWindow.Handle, SW_HIDE);
end;

procedure TLayeredImage.Move;
var
	A, B: TRect;
begin
  GetWindowRect(FAssociate, A);
  GetWindowRect(FHotWindow.Handle, B);
  MoveWindow(FHotWindow.Handle, FX + A.Left,
    FY + A.Top, WidthOf(B), HeightOf(B), False);
end;

{

procedure TLayeredImage.MessageHook(const Msg: TCWPStruct);
var
  Wnd: HWND;
	A, B: TRect;
  W: PWindowPos;
begin
  if Msg.hwnd <> FAssociate then Exit;
  W := PWindowPos(Msg.lParam);
  case Msg.message of
    WM_SHOWWINDOW:
      Show(Msg.wParam <> 0);
    WM_WINDOWPOSCHANGED:
      begin
        SetWindowPos(FHotWindow.Handle, HWND_TOP, 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
        Wnd := GetForegroundWindow;
        if GetWindowClassName(Wnd) = 'TConfigFtpForm' then
        begin
          SetWindowPos(FHotWindow.Handle, Wnd, 0, 0, 0, 0,
            SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
        end;
      end;
    WM_WINDOWPOSCHANGING:
      begin
        if (W.x <> 0) and (W.y <> 0) then
        begin
          SetWindowPos(FAssociate, FHotWindow.Handle, 0, 0, 0, 0,
            SWP_NOSIZE or SWP_NOACTIVATE  or SWP_NOMOVE);
          SetWindowPos(FHotWindow.Handle, 0, FX + W.x, FY + W.y, 0, 0,
            SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOZORDER);
        end;
      end;
  end;
end;

}

procedure TLayeredImage.MessageHook(const Msg: TCWPStruct);
var
  W: PWindowPos;
begin
  if Msg.hwnd <> FAssociate then Exit;
  W := PWindowPos(Msg.lParam);
  case Msg.message of
    WM_SHOWWINDOW:
      begin
        Show(Msg.wParam <> 0);
      end;
    WM_WINDOWPOSCHANGED:
      begin
        SetWindowPos(FHotWindow.Handle, HWND_TOP, 0, 0, 0, 0,
          SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOSENDCHANGING);
        if (W.hwndInsertAfter <> 0) and IsProcessWindow(W.hwndInsertAfter) then
          SetWindowPos(FHotWindow.Handle, W.hwndInsertAfter, 0, 0, 0, 0,
            SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOSENDCHANGING);
      end;
    WM_WINDOWPOSCHANGING:
      begin
        if (W.x <> 0) and (W.y <> 0) then
        begin
          SetWindowPos(FAssociate, FHotWindow.Handle, 0, 0, 0, 0,
            SWP_NOSIZE or SWP_NOACTIVATE  or SWP_NOMOVE or SWP_NOSENDCHANGING);
          SetWindowPos(FHotWindow.Handle, 0, FX + W.x, FY + W.y, 0, 0,
            SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOZORDER or SWP_NOSENDCHANGING);
        end;
      end;
  end;
end;

procedure LoadAlphaWindow(Wnd: HWND; const FileName: string; Opacity: Byte = $FF);
var
	AlphaMap: TAlphaMap;
begin
	AlphaMap := TAlphaMap.Create;
  try
  	AlphaMap.LoadFromFile(FileName);
		AlphaMap.Opacity := Opacity;
    UpdateAlphaWindow(Wnd, AlphaMap, AlphaMap.Canvas, AlphaMap.Opacity);
  finally
  	AlphaMap.Free;
  end;
end;

procedure UpdateAlphaWindow(Wnd: HWND; Image: TGraphic; Canvas: TCanvas; Opacity: Byte = $FF);
var
	Blend: TBlendFunction;
  Rect: TRect;
	P1, P2: TPoint;
  S: TSize;
  DC: HDC;
begin
	if Image.Height = 0 then Exit;
	SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
	GetWindowRect(Wnd, Rect);
	P1.X := Rect.Left;
	P1.Y := Rect.Top;
	with Blend do
	begin
		BlendOp := AC_SRC_OVER;
		BlendFlags := 0;
		SourceConstantAlpha := Opacity;
		AlphaFormat := AC_SRC_ALPHA;
	end;
	DC := GetDC(0);
	P2 := Point(0, 0);
	S.cx := Image.Width;
	S.cy := Image.Height;
	UpdateLayeredWindow(Wnd, DC, @P1, @S, Canvas.Handle,
		@P2, 0, @Blend, ULW_ALPHA);
	ReleaseDC(0, DC);
end;

procedure UpdateAlphaWindow(Wnd: HWND; const Bitmap: TFastBitmap; Opacity: Byte = $FF); overload;
var
	Blend: TBlendFunction;
  Rect: TRect;
	P1, P2: TPoint;
  S: TSize;
  DC: HDC;
begin
	if Bitmap.DC = 0 then Exit;
	SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
	GetWindowRect(Wnd, Rect);
	P1.X := Rect.Left;
	P1.Y := Rect.Top;
	with Blend do
	begin
		BlendOp := AC_SRC_OVER;
		BlendFlags := 0;
		SourceConstantAlpha := Opacity;
		AlphaFormat := AC_SRC_ALPHA;
	end;
	DC := GetDC(0);
	P2 := Point(0, 0);
	S.cx := Bitmap.Width;
	S.cy := Bitmap.Height;
	UpdateLayeredWindow(Wnd, DC, @P1, @S, Bitmap.DC,
		@P2, 0, @Blend, ULW_ALPHA);
	ReleaseDC(0, DC);
end;

type
  PRow = ^TRow;
  TRow = array [0..1000000] of TRGB;

  PPRows = ^TPRows;
  TPRows = array [0..1000000] of PRow;

  const
	MaxKernelSize = 100;

type
  TKernelSize = 1..MaxKernelSize;

  TKernel = record
	  Size: TKernelSize;
  	Weights: array[-MaxKernelSize..MaxKernelSize] of single;
  end;

procedure MakeGaussianKernel(var K: TKernel; Radius: Double;
  MaxData, Granularity: Double);
var
	Temp, Delta: Double;
  KernelSize: TKernelSize;
  I: Integer;
begin
  for I := Low(K.Weights) to High(K.Weights) do
  begin
  	Temp := I / Radius;
	  K.Weights[I] := exp(-Temp * Temp / 2);
  end;
  Temp := 0;
  for I := Low(K.Weights) to High(K.Weights) do
	  Temp := Temp + K.Weights[I];
  for I := Low(K.Weights) to High(K.Weights) do
	  K.Weights[I] := K.Weights[I] / Temp;
  KernelSize := MaxKernelSize;
  Delta := Granularity / (2*MaxData);
  Temp := 0;
  while (Temp < Delta) and (KernelSize > 1) do
  begin
	  Temp := Temp + 2 * K.Weights[KernelSize];
  	Dec(KernelSize);
  end;
  K.Size := KernelSize;
  Temp := 0;
  for I := -K.Size to K.Size do
	  Temp := Temp + K.Weights[I];
  for I := -K.Size to K.Size do
	  K.Weights[I] := K.Weights[I] / Temp;
end;

function TrimInt(Lower, Upper, I: Integer): Integer;
begin
	if (I <= Upper) and (I >= Lower) then
  	Result := I
	else if I > Upper then
	  Result := Upper
  else
  	Result := Lower;
end;

function TrimReal(Lower, Upper: Integer; D: Double): Integer;
begin
	if (D < Upper) and (D >= Lower) then
  	Result := Trunc(D)
	else if D > Upper then
		Result := Upper
	else
  	Result := Lower;
end;

procedure BlurRow(var Row: array of TRGB; K: TKernel; P: PRow);
var
  R, G, B: Double;
  W: Double;
  I, J: Integer;
begin
	for I := 0 to High(Row) do
  begin
	  B := 0;
  	G := 0;
	  R := 0;
  	for J := - K.Size to K.Size do
	  begin
  		W := K.Weights[J];
		  with Row[TrimInt(0, High(Row), I - J)] do
		  begin
			  B := B + W * Blue;
			  G := G + W * Green;
			  R := R + W * Red;
		  end;
	  end;
	  with P[I] do
	  begin
		  Blue := TrimReal(0, 255, B);
		  Green := TrimReal(0, 255, G);
		  Red := TrimReal(0, 255, R);
	  end;
  end;
	Move(P[0], Row[0], (High(Row) + 1) * SizeOf(TRGB));
end;

procedure BlurBitmap(Bitmap: TBitmap; const Radius: Double);
var
	Row, Col: Integer;
  Rows: PPRows;
  K: TKernel;
  ACol, P: PRow;
begin
	if Radius < 0.01 then Exit;
	if Bitmap.PixelFormat = pf32Bit then Exit;
	Bitmap.PixelFormat := pf24Bit;
	MakeGaussianKernel(K, Radius, 255, 1);
	GetMem(Rows, Bitmap.Height * SizeOf(PRow));
	GetMem(ACol, Bitmap.Height * SizeOf(TRGB));
	for Row := 0 to Bitmap.Height - 1 do
  	Rows[Row] := Bitmap.Scanline[Row];
	P := AllocMem(Bitmap.Width * SizeOf(TRGB));
	for Row := 0 to Bitmap.Height - 1 do
  	BlurRow(Slice(Rows[Row]^, Bitmap.Width), K, P);
	ReAllocMem(P, Bitmap.Height * SizeOf(TRGB));
	for Col := 0 to Bitmap.Width - 1 do
	begin
  	for Row := 0 to Bitmap.Height - 1 do
		  ACol[Row] := Rows[Row][Col];
	  BlurRow(Slice(ACol^, Bitmap.Height), K, P);
  	for Row := 0 to Bitmap.Height - 1 do
	  	Rows[Row][Col] := ACol[Row];
	end;
	FreeMem(Rows);
	FreeMem(ACol);
	ReAllocMem(P, 0);
end;

procedure BlurBitmap(Bits: PRGB; W, H: Integer; const Radius: Double);
var
	Row, Col: Integer;
  Rows: PPRows;
  K: TKernel;
  ACol, P: PRow;
begin
	if Radius < 0.01 then Exit;
	if (W < 2) or (H < 2) then Exit;
	MakeGaussianKernel(K, Radius, 255, 1);
	GetMem(Rows, H * SizeOf(PRow));
	GetMem(ACol, H * SizeOf(TRGB));
	for Row := 0 to H - 1 do
  begin
  	Rows[Row] := Pointer(Bits);
    Inc(Bits, W);
	end;
	P := AllocMem(W * SizeOf(TRGB));
	for Row := 0 to H - 1 do
  	BlurRow(Slice(Rows[Row]^, W), K, P);
	ReAllocMem(P, H * SizeOf(TRGB));
	for Col := 0 to W - 1 do
	begin
  	for Row := 0 to H - 1 do
		  ACol[Row] := Rows[Row][Col];
	  BlurRow(Slice(ACol^, H), K, P);
  	for Row := 0 to H - 1 do
	  	Rows[Row][Col] := ACol[Row];
	end;
	FreeMem(Rows);
	FreeMem(ACol);
	ReAllocMem(P, 0);
end;

procedure DrawBlurBubble(Bitmap: TBitmap; W, H, Thickness, Radius: Integer; Blur: Double);
var
	DC: HDC;
  P: HPEN;
  B: HBRUSH;
	I: Integer;
begin
	Bitmap.PixelFormat := pf24bit;
  I := Round(Blur * 4) + 1;
  Bitmap.Width := W + I + Thickness;
  Bitmap.Height := H + I + Thickness;
	DC := Bitmap.Canvas.Handle;
	FillRect(DC, Rect(0, 0, Bitmap.Width, Bitmap.Height), GetStockObject(BLACK_BRUSH));
  if Thickness > 0 then
  begin
    P := SelectObject(DC, CreatePen(PS_SOLID, Thickness, $FFFFFF));
    B := SelectObject(DC, GetStockObject(BLACK_BRUSH));
  end
  else
  begin
    P := SelectObject(DC, GetStockObject(WHITE_PEN));
    B := SelectObject(DC, GetStockObject(WHITE_BRUSH));
  end;
  RoundRect(DC, I shr 1 + Thickness, I shr 1 + Thickness, W + I shr 1 ,
    H + I shr 1, Radius, Radius);
  if Thickness > 0 then
  begin
    SelectObject(DC, B);
    DeleteObject(SelectObject(DC, P));
  end
  else
  begin
    SelectObject(DC, B);
    SelectObject(DC, P);
  end;
  BlurBitmap(Bitmap, Blur);
  { This code tests the shape of the bitmap:

  P := SelectObject(DC, GetStockObject(WHITE_PEN));
  B := SelectObject(DC, GetStockObject(WHITE_BRUSH));
  MoveToEx(DC, 0, 0, nil);
  LineTo(DC, Bitmap.Width - 1, 0);
  LineTo(DC, Bitmap.Width - 1, Bitmap.Height- 1);
  LineTo(DC, 0, Bitmap.Height- 1);
  LineTo(DC, 0, 0);
  SelectObject(DC, B);
  SelectObject(DC, P); }
end;

var
	InternalFocusTracker: TObject;

function FocusTracker: TFocusTracker;
begin
  if InternalFocusTracker = nil then
 		InternalFocusTracker := TFocusTracker.Create;
	Result := TFocusTracker(InternalFocusTracker);
end;

initialization
	InternalFocusTracker := nil;
finalization
	InternalFocusTracker.Free;
end.
