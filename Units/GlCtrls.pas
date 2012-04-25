
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit GLCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  BareOpenGL, BareOpenGLExt, BareGraphics, BareGraphicObjs;

{ IDrawDevice }

type
  TRefreshKind = (rkNone, rkPaint, rkIdle, rkThread);

  IDrawingDevice = interface(IUnknown)
  	['{71047618-EBBC-4841-AEDF-41A5DCDCC825}']
    function GetActive: Boolean;
    procedure SetActive(Value: Boolean);
    function GetDC: HDC;
    function GetRC: HGLRC;
    function GetFrameRate: Single;
		function GetRefreshKind: TRefreshKind;
    procedure SetRefreshKind(Value: TRefreshKind);
    function GetTimer: ITimer;
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
    function GetOnDraw: TNotifyEvent;
    procedure SetOnDraw(Value: TNotifyEvent);
    function GetOnLoad: TNotifyEvent;
    procedure SetOnLoad(Value: TNotifyEvent);
    function GetOnUnload: TNotifyEvent;
    procedure SetOnUnload(Value: TNotifyEvent);
  	procedure Draw;
    procedure Flip;
    property Active: Boolean read GetActive write SetActive;
    property DC: HDC read GetDC;
    property RC: HGLRC read GetRC;
    property FrameRate: Single read GetFrameRate;
		property RefreshKind: TRefreshKind read GetRefreshKind write SetRefreshKind;
    property Timer: ITimer read GetTimer;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property OnDraw: TNotifyEvent read GetOnDraw write SetOnDraw;
    property OnLoad: TNotifyEvent read GetOnLoad write SetOnLoad;
    property OnUnload: TNotifyEvent read GetOnUnload write SetOnUnload;
  end;

{ TOpenGLControl }

  TOpenGLControl = class(TWinControl, IUnknown, IDrawingDevice)
  private
  	FActive: Boolean;
    FDC: HDC;
    FRC: HGLRC;
    FFrameRate: Single;
    FTimer: TStandardTimer;
    FRefreshKind: TRefreshKind;
    FSamples: Byte;
    FTextWriter: TTextWriter;
    FOnDraw: TNotifyEvent;
    FOnUnload: TNotifyEvent;
    FOnLoad: TNotifyEvent;
    procedure RecreateFormat;
    procedure SetActive(Value: Boolean);
    procedure SetRefreshKind(Value: TRefreshKind);
    procedure SetSamples(Value: Byte);
    function GetTextWriter: TTextWriter;
    procedure WMCreate(var Msg); message WM_CREATE;
    procedure WMDestroy(var Msg); message WM_DESTROY;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure PaintWindow(DC: HDC); override;
    procedure Resize; override;
    { IDrawingDevice }
    function DeviceGetActive: Boolean;
    procedure DeviceSetActive(Value: Boolean);
    function DeviceGetDC: HDC;
    function DeviceGetRC: HGLRC;
    function DeviceGetFrameRate: Single;
		function DeviceGetRefreshKind: TRefreshKind;
    procedure DeviceSetRefreshKind(Value: TRefreshKind);
    function DeviceGetTimer: ITimer;
    function DeviceGetWidth: Integer;
    procedure DeviceSetWidth(Value: Integer);
    function DeviceGetHeight: Integer;
    procedure DeviceSetHeight(Value: Integer);
    function DeviceGetOnDraw: TNotifyEvent;
    procedure DeviceSetOnDraw(Value: TNotifyEvent);
    function DeviceGetOnLoad: TNotifyEvent;
    procedure DeviceSetOnLoad(Value: TNotifyEvent);
    function DeviceGetOnUnload: TNotifyEvent;
    procedure DeviceSetOnUnload(Value: TNotifyEvent);
  	procedure DeviceDraw;
    procedure DeviceFlip;
    { IDrawingDevice resolution }
    function IDrawingDevice.GetActive = DeviceGetActive;
    procedure IDrawingDevice.SetActive = DeviceSetActive;
    function IDrawingDevice.GetDC = DeviceGetDC;
    function IDrawingDevice.GetRC = DeviceGetRC;
    function IDrawingDevice.GetFrameRate = DeviceGetFrameRate;
		function IDrawingDevice.GetRefreshKind = DeviceGetRefreshKind;
    procedure IDrawingDevice.SetRefreshKind = DeviceSetRefreshKind;
    function IDrawingDevice.GetTimer = DeviceGetTimer;
    function IDrawingDevice.GetWidth = DeviceGetWidth;
    procedure IDrawingDevice.SetWidth = DeviceSetWidth;
    function IDrawingDevice.GetHeight = DeviceGetHeight;
    procedure IDrawingDevice.SetHeight = DeviceSetHeight;
    function IDrawingDevice.GetOnDraw = DeviceGetOnDraw;
    procedure IDrawingDevice.SetOnDraw = DeviceSetOnDraw;
    function IDrawingDevice.GetOnLoad = DeviceGetOnLoad;
    procedure IDrawingDevice.SetOnLoad = DeviceSetOnLoad;
    function IDrawingDevice.GetOnUnload = DeviceGetOnUnload;
    procedure IDrawingDevice.SetOnUnload = DeviceSetOnUnload;
  	procedure IDrawingDevice.Draw = DeviceDraw;
  	procedure IDrawingDevice.Flip = DeviceFlip;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LockContext: Boolean;
    procedure UnlockContext;
    procedure Draw; virtual;
    procedure Flip;
    property Active: Boolean read FActive write SetActive;
    property DC: HDC read FDC;
    property RC: HGLRC read FRC;
    property FrameRate: Single read FFrameRate;
    property TextWriter: TTextWriter read GetTextWriter;
    property Timer: TStandardTimer read FTimer;
  published
    property Align;
    property Anchors;
    property RefreshKind: TRefreshKind read FRefreshKind write SetRefreshKind;
    property Samples: Byte read FSamples write SetSamples default 0;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property OnUnload: TNotifyEvent read FOnUnload write FOnUnload;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnUnDock;
  end;

	{ TOpenGLBuffer }

	TOpenGLBuffer = class(TComponent, IUnknown, IDrawingDevice)
  private
  	FActive: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FBuffer: GLhandle;
  	FDC: HDC;
    FRC: HGLRC;
    FFrameRate: Single;
    FTimer: TStandardTimer;
    FRefreshKind: TRefreshKind;
    FOnLoad: TNotifyEvent;
    FOnDraw: TNotifyEvent;
    FOnUnload: TNotifyEvent;
    function CreateBuffer: Boolean;
    procedure DestroyBuffer;
    procedure SetActive(Value: Boolean);
    procedure SetRefreshKind(Value: TRefreshKind);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
	protected
    { IDrawingDevice }
    function DeviceGetActive: Boolean;
    procedure DeviceSetActive(Value: Boolean);
    function DeviceGetDC: HDC;
    function DeviceGetRC: HGLRC;
    function DeviceGetFrameRate: Single;
		function DeviceGetRefreshKind: TRefreshKind;
    procedure DeviceSetRefreshKind(Value: TRefreshKind);
    function DeviceGetTimer: ITimer;
    function DeviceGetWidth: Integer;
    procedure DeviceSetWidth(Value: Integer);
    function DeviceGetHeight: Integer;
    procedure DeviceSetHeight(Value: Integer);
    function DeviceGetOnDraw: TNotifyEvent;
    procedure DeviceSetOnDraw(Value: TNotifyEvent);
    function DeviceGetOnLoad: TNotifyEvent;
    procedure DeviceSetOnLoad(Value: TNotifyEvent);
    function DeviceGetOnUnload: TNotifyEvent;
    procedure DeviceSetOnUnload(Value: TNotifyEvent);
  	procedure DeviceDraw;
    procedure DeviceFlip;
    { IDrawingDevice resolution }
    function IDrawingDevice.GetActive = DeviceGetActive;
    procedure IDrawingDevice.SetActive = DeviceSetActive;
    function IDrawingDevice.GetDC = DeviceGetDC;
    function IDrawingDevice.GetRC = DeviceGetRC;
    function IDrawingDevice.GetFrameRate = DeviceGetFrameRate;
		function IDrawingDevice.GetRefreshKind = DeviceGetRefreshKind;
    procedure IDrawingDevice.SetRefreshKind = DeviceSetRefreshKind;
    function IDrawingDevice.GetTimer = DeviceGetTimer;
    function IDrawingDevice.GetWidth = DeviceGetWidth;
    procedure IDrawingDevice.SetWidth = DeviceSetWidth;
    function IDrawingDevice.GetHeight = DeviceGetHeight;
    procedure IDrawingDevice.SetHeight = DeviceSetHeight;
    function IDrawingDevice.GetOnDraw = DeviceGetOnDraw;
    procedure IDrawingDevice.SetOnDraw = DeviceSetOnDraw;
    function IDrawingDevice.GetOnLoad = DeviceGetOnLoad;
    procedure IDrawingDevice.SetOnLoad = DeviceSetOnLoad;
    function IDrawingDevice.GetOnUnload = DeviceGetOnUnload;
    procedure IDrawingDevice.SetOnUnload = DeviceSetOnUnload;
  	procedure IDrawingDevice.Draw = DeviceDraw;
  	procedure IDrawingDevice.Flip = DeviceFlip;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw;
    property Active: Boolean read FActive write SetActive;
    property DC: HDC read FDC;
    property RC: HGLRC read FRC;
    property FrameRate: Single read FFrameRate;
    property Timer: TStandardTimer read FTimer;
	published
    property RefreshKind: TRefreshKind read FRefreshKind write SetRefreshKind;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;
    property OnUnload: TNotifyEvent read FOnUnload write FOnUnload;
  end;

procedure BindGraphic(texture: GLuint; Bits: Pointer; Size: Integer); overload;
procedure BindGraphic(texture: GLuint; Graphic: TGraphic; Alpha: Boolean = False); overload;
procedure BindGraphic(texture: GLuint; const FileName: string; Alpha: Boolean = False); overload;

var
  DrawProc: TNotifyEvent;
  LoadProc: TNotifyEvent;
  UnloadProc: TNotifyEvent;

  DebugFormats: array[0..99] of Integer;
  DebugFormatCount: Cardinal;

  { GL Utility Toolkit routines }

  glutWireSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCube: procedure(size: GLdouble); stdcall;
  glutSolidCube: procedure(size: GLdouble); stdcall;
  glutWireTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutSolidTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutWireDodecahedron: procedure; stdcall;
  glutSolidDodecahedron: procedure; stdcall;
  glutWireTeapot: procedure(size: GLdouble); stdcall;
  glutSolidTeapot: procedure(size: GLdouble); stdcall;
  glutWireOctahedron: procedure; stdcall;
  glutSolidOctahedron: procedure; stdcall;
  glutWireTetrahedron: procedure; stdcall;
  glutSolidTetrahedron: procedure; stdcall;
  glutWireIcosahedron: procedure; stdcall;
  glutSolidIcosahedron: procedure; stdcall;

function LoadGlut(ModuleName: string = 'glut32.dll'): HMODULE;
procedure UnloadGlut(Module: HMODULE);

implementation

const
	WM_REFRESH = WM_USER + 1;

type
  TRefreshManager = class
  private
    FThread: TThread;
    FIdleDevices: TInterfaceList;
    FThreadDevices: TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed(Device: IDrawingDevice);
    procedure RefreshIdle(Sender: TObject; var Done: Boolean);
    procedure RefreshThread;
  end;

  TRefreshThread = class(TThread)
  private
    FManager: TRefreshManager;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TRefreshManager);
  end;

var
  Manager: TRefreshManager;

constructor TRefreshManager.Create;
begin
  inherited Create;
  FIdleDevices := TInterfaceList.Create;
  FThreadDevices := TInterfaceList.Create;
end;

destructor TRefreshManager.Destroy;
begin
  if FThread <> nil then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
  FIdleDevices.Free;
  FThreadDevices.Free;
  inherited Destroy;
end;

procedure TRefreshManager.Changed(Device: IDrawingDevice);
begin
  FIdleDevices.Remove(Device);
  FThreadDevices.Remove(Device);
  if Device.Active then
	  if Device.RefreshKind = rkIdle then
  	  FIdleDevices.Add(Device)
	  else if Device.RefreshKind = rkThread then
  	begin
    	FThreadDevices.Add(Device);
	    if FThread = nil then
  	    FThread := TRefreshThread.Create(Self);
	  end;
  if FIdleDevices.Count > 0 then
    Application.OnIdle := RefreshIdle
  else
    Application.OnIdle := nil;
  if (FThreadDevices.Count = 0) and (FThread <> nil) then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
end;

procedure TRefreshManager.RefreshIdle(Sender: TObject; var Done: Boolean);
var
  I: Integer;
begin
	//if FThread = nil then Exit;
  for I := 0 to FIdleDevices.Count - 1 do
    IDrawingDevice(FIdleDevices[I]).Draw;
  Done := False;
end;

procedure TRefreshManager.RefreshThread;
var
  I: Integer;
begin
	if FThread = nil then Exit;
  for I := 0 to FThreadDevices.Count - 1 do
    IDrawingDevice(FThreadDevices[I]).Draw;
end;

{ TRefreshThread }

constructor TRefreshThread.Create(Manager: TRefreshManager);
begin
  FManager := Manager;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TRefreshThread.Execute;
const
  Delay = 1000 div 45;
begin
  while not Terminated do
  begin
    Synchronize(FManager.RefreshThread);
    //SleepEx(Delay, False);
  end;
end;

{ TOpenGLControl }

constructor TOpenGLControl.Create(AOwner: TComponent);
begin
	glxPreloadExtensions;
  inherited Create(AOwner);
  if csDesigning in ComponentState then
    ControlState := ControlState + [csCustomPaint];
	FActive := True;
  FRefreshKind := rkThread;
  FTimer := TStandardTimer.Create;
  Width := 400;
  Height := 300;
end;

destructor TOpenGLControl.Destroy;
begin
	Active := False;
  FTimer.Free;
  inherited Destroy;
end;

procedure TOpenGLControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if csDesigning in ComponentState then Exit;
  with Params.WindowClass do
  begin
    style := style or CS_OWNDC;
    style := style and (not (CS_HREDRAW	or CS_VREDRAW));
  end;
end;

procedure TOpenGLControl.RecreateFormat;
var
	PriorRefresh: TRefreshKind;
  Descriptor: TPixelFormatDescriptor;
	DefInt: TPixelAttributes;
  DefFloat: TFloatPixelAttributes;
  Valid: GLboolean;
begin
	if csDesigning in ComponentState then
		Exit;
	PriorRefresh := RefreshKind;
  RefreshKind := rkNone;
  if LockContext then
  try
    FTextWriter.Free;
    FTextWriter := nil;
    if Assigned(FOnUnload) then
      FOnUnload(Self)
    else if Assigned(UnloadProc) then
      UnloadProc(Self);
  finally
    UnlockContext;
    wglDeleteContext(FRC);
    FRC := 0;
  end;
  if FDC <> 0 then
	  ReleaseDC(Handle, FDC);
  FDC := GetDC(Handle);
	FillChar(Descriptor, SizeOf(TPixelFormatDescriptor), #0);
	Valid := False;
	if WGL_ARB_PIXEL_FORMAT then
  begin
  	DefInt := DefPixelAttributes;
    DefFloat := DefPixelFloatFloatAttributes;
    if FSamples > 0 then
    begin
	    DefInt[DefSampleBuffers] := 1;
	    DefInt[DefSamples] := FSamples;
    end;
		Valid := wglChoosePixelFormat(FDC, @DefInt, @DefFloat, 1, DebugFormats[0],
    	DebugFormatCount) and (DebugFormatCount > 0) and SetPixelFormat(FDC, DebugFormats[0], nil);
  end;
  if not Valid then
  begin
	  with Descriptor do
	  begin
	    nSize := SizeOf(TPixelFormatDescriptor);
	    nVersion := 1;
	    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
	    iPixelType := PFD_TYPE_RGBA;
	    cColorBits := 24;
	    cDepthBits := 16;
	    cStencilBits := 1;
	    iLayerType := PFD_MAIN_PLANE;
	  end;
	  DebugFormats[0] := ChoosePixelFormat(FDC, @Descriptor);
    Valid := SetPixelFormat(FDC, DebugFormats[0], @Descriptor);
  end;
  if Valid then
  begin
    FRC := wglCreateContext(FDC);
    if LockContext then
    try
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glEnable(GL_BLEND);
      glEnable(GL_DEPTH_TEST);
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      gluPerspective(60, 4 / 3, 0.5, 1000);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      if Assigned(FOnLoad) then
        FOnLoad(Self)
      else if Assigned(LoadProc) then
        LoadProc(Self);
    finally
      UnlockContext;
    end;
  end;
  RefreshKind := PriorRefresh;
end;

procedure TOpenGLControl.WMCreate(var Msg);
begin
  inherited;
  RecreateFormat;
end;

procedure TOpenGLControl.WMDestroy(var Msg);
var
	PriorRefresh: TRefreshKind;
begin
  if csDesigning in ComponentState then
  begin
    inherited;
    Exit;
  end;
  PriorRefresh := FRefreshKind;
  FRefreshKind := rkNone;
  Manager.Changed(Self);
  FRefreshKind := PriorRefresh;
  if LockContext then
  try
    FTextWriter.Free;
    FTextWriter := nil;
    if Assigned(FOnUnload) then
      FOnUnload(Self)
    else if Assigned(UnloadProc) then
      UnloadProc(Self);
  finally
    UnlockContext;
    wglDeleteContext(FRC);
    FRC := 0;
  end;
  ReleaseDC(Handle, FDC);
  FDC := 0;
  inherited;
end;

procedure TOpenGLControl.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  if FRefreshKind = rkPaint then
  begin
    Draw;
    Msg.Result := 1;
  end
  else
    inherited;
end;

procedure TOpenGLControl.PaintWindow(DC: HDC);
var
  Pen: HPEN;
  Brush: HBRUSH;
begin
  if csDesigning in ComponentState then
  begin
    Pen := SelectObject(DC, CreatePen(PS_DASH, 0, 0));
    Brush := SelectObject(DC, GetStockObject(HOLLOW_BRUSH));
    Rectangle(DC, 0, 0, Width, Height);
    SelectObject(DC, Brush);
    DeleteObject(SelectObject(DC, Pen));
  end;
end;

procedure TOpenGLControl.Resize;
begin
  inherited Resize;
  if LockContext then
  try
    glViewport(0, 0, Width, Height);
  finally
    UnlockContext;
  end;
end;

procedure TOpenGLControl.Draw;
var
  Interval: Single;
begin
	if not FActive then Exit;
  if LockContext then
  try
    Interval := FTimer.Time;
    FTimer.Calculate;
    if FTimer.Time > Interval then
      FFrameRate := 1 / (FTimer.Time - Interval)
    else
      FFrameRate := -1;
    if Assigned(FOnDraw) then
      FOnDraw(Self)
    else if Assigned(DrawProc) then
      DrawProc(Self)
    else
    begin
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      Flip;
		end;
  finally
    UnlockContext;
  end;
end;

procedure TOpenGLControl.Flip;
begin
	SwapBuffers(FDC);
end;

function TOpenGLControl.LockContext: Boolean;
begin
  if FRC <> 0 then
  begin
    wglMakeCurrent(FDC, FRC);
    Result := True;
  end
  else
    Result := False;
end;

procedure TOpenGLControl.UnlockContext;
begin
  if FRC <> 0 then
    wglMakeCurrent(0, 0);
end;

procedure TOpenGLControl.SetActive(Value: Boolean);
begin
	if Value <> FActive then
  begin
  	FActive := Value;
    Manager.Changed(Self);
  end;
end;

procedure TOpenGLControl.SetRefreshKind(Value: TRefreshKind);
begin
  if Value <> FRefreshKind then
  begin
    FRefreshKind := Value;
    if csDesigning in ComponentState then
      Exit;
    Manager.Changed(Self);
  end;
end;

procedure TOpenGLControl.SetSamples(Value: Byte);
begin
	if Value > 16 then
  	Value := 16
	else if Value > 8 then
  	Value := 8
	else if Value > 4 then
  	Value := 4
	else if Value > 2 then
  	Value := 2
	else
  	Value := 0;
	if Value > MaxMultiSample then
  	Value := MaxMultiSample;
  if Value <> FSamples then
  begin
  	FSamples := Value;
    RecreateWnd;
  end;
end;

function TOpenGLControl.GetTextWriter: TTextWriter;
begin
  if FTextWriter = nil then
  begin
    FTextWriter := CreateTextWriter;
    FTextWriter.Shadow := True;
    FTextWriter.ShadowColor^ := fcBlack;
  end;
  Result := FTextWriter;
end;

{ TOpenGLControl.IDrawingDevice }

function TOpenGLControl.DeviceGetActive: Boolean;
begin
	Result := Active;
end;

procedure TOpenGLControl.DeviceSetActive(Value: Boolean);
begin
	Active := Value;
end;

function TOpenGLControl.DeviceGetDC: HDC;
begin
	Result := DC;
end;

function TOpenGLControl.DeviceGetRC: HGLRC;
begin
	Result := RC;
end;

function TOpenGLControl.DeviceGetFrameRate: Single;
begin
	Result := FrameRate;
end;

function TOpenGLControl.DeviceGetRefreshKind: TRefreshKind;
begin
	Result := RefreshKind;
end;

procedure TOpenGLControl.DeviceSetRefreshKind(Value: TRefreshKind);
begin
	RefreshKind := Value;
end;

function TOpenGLControl.DeviceGetTimer: ITimer;
begin
	Result := Timer;
end;

function TOpenGLControl.DeviceGetWidth: Integer;
begin
	Result := Width;
end;

procedure TOpenGLControl.DeviceSetWidth(Value: Integer);
begin
	Width := Value;
end;

function TOpenGLControl.DeviceGetHeight: Integer;
begin
	Result := Height;
end;

procedure TOpenGLControl.DeviceSetHeight(Value: Integer);
begin
	Height := Value;
end;

function TOpenGLControl.DeviceGetOnDraw: TNotifyEvent;
begin
	Result := OnDraw;
end;

procedure TOpenGLControl.DeviceSetOnDraw(Value: TNotifyEvent);
begin
	OnDraw := Value;
end;

function TOpenGLControl.DeviceGetOnLoad: TNotifyEvent;
begin
	Result := OnLoad;
end;

procedure TOpenGLControl.DeviceSetOnLoad(Value: TNotifyEvent);
begin
	OnLoad := Value;
end;

function TOpenGLControl.DeviceGetOnUnload: TNotifyEvent;
begin
	Result := OnUnload;
end;

procedure TOpenGLControl.DeviceSetOnUnload(Value: TNotifyEvent);
begin
	OnUnload := Value;
end;

procedure TOpenGLControl.DeviceDraw;
begin
	Draw;
end;

procedure TOpenGLControl.DeviceFlip;
begin
	Flip;
end;

{ TOpenGLBuffer }

constructor TOpenGLBuffer.Create(AOwner: TComponent);
begin
	glxPreloadExtensions;
	inherited Create(AOwner);
  FActive := WGL_ARB_PBUFFER;
	FWidth := 400;
  FHeight := 300;
end;

destructor TOpenGLBuffer.Destroy;
begin
	DestroyBuffer;
	inherited Destroy;
end;

function TOpenGLBuffer.CreateBuffer: Boolean;
const
	Attributes: array[0..23] of Integer = (
		WGL_DRAW_TO_PBUFFER, 1,
		WGL_SUPPORT_OPENGL, 1,
    WGL_DOUBLE_BUFFER, 0,
		WGL_ACCELERATION, WGL_FULL_ACCELERATION,
    WGL_PIXEL_TYPE, WGL_TYPE_RGBA,
    WGL_RED_BITS, 8,
    WGL_GREEN_BITS, 8,
    WGL_BLUE_BITS, 8,
		WGL_ALPHA_BITS, 8,
		WGL_DEPTH_BITS, 16,
		WGL_STENCIL_BITS, 1,
		0, 0);
var
  Lost: GLint;
	Wnd: HWND;
	PixelDC: HDC;
  Format: Integer;
  FormatCount: Cardinal;
begin
	Result := False;
	if not FActive then Exit;
  if FBuffer <> 0 then
  begin
  	Lost := 1;
    wglQueryPbuffer(FBuffer, WGL_PBUFFER_LOST, Lost);
    if Lost = 1 then
    begin
			wglDestroyPbuffer(FBuffer);
    	FBuffer := 0;
		end;
  end;
  if FBuffer = 0 then
  begin
  	if Owner is TWinControl then
    	Wnd := TWinControl(Owner).Handle
		else
    	Wnd := GetDesktopWindow;
  	PixelDC := GetDC(Wnd);
		if wglChoosePixelFormat(PixelDC, @Attributes, nil, 1, Format,
			FormatCount) and (FormatCount > 0) then
		begin
			FBuffer := wglCreatePbuffer(PixelDC, Format, FWidth, FHeight, nil);
	    if FBuffer <> 0 then
	    begin
				FDC := wglGetPbufferDC(FBuffer);
	      if FDC <> 0 then
	      begin
			    FRC := wglCreateContext(FDC);
	        if (FRC <> 0) and wglMakeCurrent(FDC, FRC) then
					begin
          	if FTimer = nil then
							FTimer := TStandardTimer.Create;
						FTimer.Calculate;
			      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
			      glEnable(GL_BLEND);
			      glEnable(GL_DEPTH_TEST);
			      glClearColor(0, 0, 0, 0);
			      glMatrixMode(GL_PROJECTION);
			      glLoadIdentity;
			      gluPerspective(60, FWidth / FHeight, 0.5, 1000);
			      glMatrixMode(GL_MODELVIEW);
			      glLoadIdentity;
			      if Assigned(FOnLoad) then
			        FOnLoad(Self);
				    Manager.Changed(Self);
          end
          else if FRC <> 0 then
          begin
						wglDeleteContext(FRC);
						FRC := 0;
          end;
          if FRC = 0 then
          begin
		      	wglReleasePbufferDC(FBuffer, FDC);
            FDC := 0;
          end;
				end;
        if FDC = 0 then
        begin
        	wglDestroyPbuffer(FBuffer);
          FBuffer := 0;
        end;
			end;
		end;
    ReleaseDC(Wnd, PixelDC);
  end;
  Result := FBuffer <> 0;
end;

procedure TOpenGLBuffer.DestroyBuffer;
var
  Lost: GLint;
begin
	if FBuffer <> 0 then
  begin
  	Lost := 1;
    wglQueryPbuffer(FBuffer, WGL_PBUFFER_LOST, Lost);
    if Lost = 1 then
    	FBuffer := 0;
  end;
	if FBuffer <> 0 then
  begin
  	if FRC <> 0 then
    begin
    	if wglMakeCurrent(FDC, FRC) then
      begin
		  	if Assigned(FOnUnload) then
  		  	FOnUnload(Self);
				wglMakeCurrent(0, 0);
			end;
			wglDeleteContext(FRC);
		end;
		FRC := 0;
  	if FDC <> 0 then
			wglReleasePbufferDC(FBuffer, FDC);
		FDC := 0;
		wglDestroyPbuffer(FBuffer);
		FBuffer := 0;
  end;
end;

procedure TOpenGLBuffer.Draw;
var
  Interval: Single;
begin
	if CreateBuffer and Assigned(FOnDraw) and wglMakeCurrent(FDC, FRC) then // then
  try
    Interval := FTimer.Time;
		FTimer.Calculate;
    if FTimer.Time > Interval then
      FFrameRate := 1 / (FTimer.Time - Interval)
    else
      FFrameRate := -1;
    FOnDraw(Self);
	finally
	  wglMakeCurrent(0, 0); //testing for memory leak
	end;
end;

procedure TOpenGLBuffer.SetActive(Value: Boolean);
begin
	Value := Value and WGL_ARB_PBUFFER;
	if Value <> FActive then
  begin
  	FActive := Value;
    if FActive then
    	CreateBuffer
		else
    	DestroyBuffer;
    Manager.Changed(Self);
  end;
end;

procedure TOpenGLBuffer.SetRefreshKind(Value: TRefreshKind);
begin
  if Value <> FRefreshKind then
  begin
    FRefreshKind := Value;
    if csDesigning in ComponentState then
      Exit;
    Manager.Changed(Self);
  end;
end;

procedure TOpenGLBuffer.SetWidth(Value: Integer);
begin
	if Value < 1 then Value := 1;
  if Value <> FWidth then
  begin
		DestroyBuffer;
    FWidth := Value;
  end;
end;

procedure TOpenGLBuffer.SetHeight(Value: Integer);
begin
	if Value < 1 then Value := 1;
  if Value <> FHeight then
  begin
		DestroyBuffer;
    FHeight := Value;
  end;
end;

{ TOpenGLBuffer.IDrawingDevice }

function TOpenGLBuffer.DeviceGetActive: Boolean;
begin
	Result := Active;
end;

procedure TOpenGLBuffer.DeviceSetActive(Value: Boolean);
begin
	Active := Value;
end;

function TOpenGLBuffer.DeviceGetDC: HDC;
begin
	Result := DC;
end;

function TOpenGLBuffer.DeviceGetRC: HGLRC;
begin
	Result := RC;
end;

function TOpenGLBuffer.DeviceGetFrameRate: Single;
begin
	Result := FrameRate;
end;

function TOpenGLBuffer.DeviceGetRefreshKind: TRefreshKind;
begin
	Result := RefreshKind;
end;

procedure TOpenGLBuffer.DeviceSetRefreshKind(Value: TRefreshKind);
begin
	RefreshKind := Value;
end;

function TOpenGLBuffer.DeviceGetTimer: ITimer;
begin
	Result := Timer;
end;

function TOpenGLBuffer.DeviceGetWidth: Integer;
begin
	Result := Width;
end;

procedure TOpenGLBuffer.DeviceSetWidth(Value: Integer);
begin
	Width := Value;
end;

function TOpenGLBuffer.DeviceGetHeight: Integer;
begin
	Result := Height;
end;

procedure TOpenGLBuffer.DeviceSetHeight(Value: Integer);
begin
	Height := Value;
end;

function TOpenGLBuffer.DeviceGetOnDraw: TNotifyEvent;
begin
	Result := OnDraw;
end;

procedure TOpenGLBuffer.DeviceSetOnDraw(Value: TNotifyEvent);
begin
	OnDraw := Value;
end;

function TOpenGLBuffer.DeviceGetOnLoad: TNotifyEvent;
begin
	Result := OnLoad;
end;

procedure TOpenGLBuffer.DeviceSetOnLoad(Value: TNotifyEvent);
begin
	OnLoad := Value;
end;

function TOpenGLBuffer.DeviceGetOnUnload: TNotifyEvent;
begin
	Result := OnUnload;
end;

procedure TOpenGLBuffer.DeviceSetOnUnload(Value: TNotifyEvent);
begin
	OnUnload := Value;
end;

procedure TOpenGLBuffer.DeviceDraw;
begin
	Draw;
end;

procedure TOpenGLBuffer.DeviceFlip;
begin
end;

procedure BindGraphic(texture: GLuint; Bits: Pointer; Size: Integer);
begin
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Size, Size, 0, GL_RGBA,
    GL_UNSIGNED_BYTE, Bits);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

procedure BindGraphic(texture: GLuint; Graphic: TGraphic; Alpha: Boolean = False);

  function Max(A, B: Integer): Integer;
  begin
    if A > B then Result := A else Result := B;
  end;

var
  Bitmap: TBitmap;
  Bits: Pointer;
  Source: PRGBTriple;
  Dest: PRGBQuad;
  A, B, C: Integer;
begin
  glBindTexture(GL_TEXTURE_2D, texture);
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf24bit;
    A := Max(Graphic.Width, Graphic.Height);
    B := 1;
    while B < A do B := B shl 1;
    Bitmap.Width := B;
    Bitmap.Height := B;
    Bitmap.Canvas.StretchDraw(Bitmap.Canvas.ClipRect, Graphic);
    GetMem(Bits, B * B * SizeOf(TRGBQuad));
    try
      Dest := Bits;
      for A := 0 to B - 1 do
      begin
        Source := Bitmap.ScanLine[A];
        for C := 0 to B - 1 do
        begin
          if Alpha then
          begin
            Dest.rgbReserved := (Source.rgbtRed + Source.rgbtGreen +
              Source.rgbtBlue) div 3;
            Dest.rgbRed := $FF;
            Dest.rgbGreen := $FF;
            Dest.rgbBlue := $FF;
          end
          else
          begin
            Dest.rgbBlue := Source.rgbtRed;
            Dest.rgbGreen := Source.rgbtGreen;
            Dest.rgbRed := Source.rgbtBlue;
            Dest.rgbReserved := $FF;
          end;
          Inc(Dest);
          Inc(Source);
        end;
      end;
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, B, B, 0, GL_RGBA,
        GL_UNSIGNED_BYTE, Bits);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    finally
      FreeMem(Bits);
    end;
  finally
    Bitmap.Free;
  end;
end;

procedure BindGraphic(texture: GLuint; const FileName: string; Alpha: Boolean = False);
var
  Picture: TPicture;
begin
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    BindGraphic(texture, Picture.Graphic, Alpha);
  finally
    Picture.Free;
  end;
end;

function LoadGlut(ModuleName: string = 'glut32.dll'): HMODULE;
begin
  Result := LoadLibrary(PChar(ModuleName));
  if Result <> 0 then
  begin
    @glutWireSphere := GetProcAddress(Result, 'glutWireSphere');
    @glutSolidSphere := GetProcAddress(Result, 'glutSolidSphere');
    @glutWireCone := GetProcAddress(Result, 'glutWireCone');
    @glutSolidCone := GetProcAddress(Result, 'glutSolidCone');
    @glutWireCube := GetProcAddress(Result, 'glutWireCube');
    @glutSolidCube := GetProcAddress(Result, 'glutSolidCube');
    @glutWireTorus := GetProcAddress(Result, 'glutWireTorus');
    @glutSolidTorus := GetProcAddress(Result, 'glutSolidTorus');
    @glutWireDodecahedron := GetProcAddress(Result, 'glutWireDodecahedron');
    @glutSolidDodecahedron := GetProcAddress(Result, 'glutSolidDodecahedron');
    @glutWireTeapot := GetProcAddress(Result, 'glutWireTeapot');
    @glutSolidTeapot := GetProcAddress(Result, 'glutSolidTeapot');
    @glutWireOctahedron := GetProcAddress(Result, 'glutWireOctahedron');
    @glutSolidOctahedron := GetProcAddress(Result, 'glutSolidOctahedron');
    @glutWireTetrahedron := GetProcAddress(Result, 'glutWireTetrahedron');
    @glutSolidTetrahedron := GetProcAddress(Result, 'glutSolidTetrahedron');
    @glutWireIcosahedron := GetProcAddress(Result, 'glutWireIcosahedron');
    @glutSolidIcosahedron := GetProcAddress(Result, 'glutSolidIcosahedron');
  end;
end;

procedure UnloadGlut(Module: HMODULE);
begin
  @glutWireSphere := nil;
  @glutSolidSphere := nil;
  @glutWireCone := nil;
  @glutSolidCone := nil;
  @glutWireCube := nil;
  @glutSolidCube := nil;
  @glutWireTorus := nil;
  @glutSolidTorus := nil;
  @glutWireDodecahedron := nil;
  @glutSolidDodecahedron := nil;
  @glutWireTeapot := nil;
  @glutSolidTeapot := nil;
  @glutWireOctahedron := nil;
  @glutSolidOctahedron := nil;
  @glutWireTetrahedron := nil;
  @glutSolidTetrahedron := nil;
  @glutWireIcosahedron := nil;
  @glutSolidIcosahedron := nil;
  if Module <> 0 then
    FreeLibrary(Module);
end;

initialization
  Manager := TRefreshManager.Create;
finalization
  Manager.Free;
end.

