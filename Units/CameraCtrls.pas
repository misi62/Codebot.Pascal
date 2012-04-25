
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2007                   *)
(*                                                      *)
(********************************************************)

unit CameraCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, ActiveX, Classes, SysUtils, Graphics,
  Forms, SysTools, WinTools;

{ ICamera }

type
  TCameraProp = Cardinal;

{ Camera properties }

const
	prName = 1;
	prMaxZoom = 2;

	FinderDelay = 1 / 24 / 60 / 60 / 4;

type
	TCameraManager = class;

	TCameraMode = (cmDisconnected, cmReady, cmRemote, cmViewfinder);
	TImageSize = (isUnknown, isSmall, isMedium, isLarge, isExLarge);

	ICamera = interface(IUnknown)
		['{96097929-5BA7-434B-B1F5-1AF14AF0751A}']
		function GetFlash: Boolean;
		procedure SetFlash(Value: Boolean);
		function GetHandle: THandle;
		function GetMode: TCameraMode;
		procedure SetMode(Value: TCameraMode);
    function GetOwner: TObject;
    procedure SetOwner(Value: TObject);
		function GetImageSize: TImageSize;
		procedure SetImageSize(Value: TImageSize);
		function GetSnapshot: TGraphic;
		function GetView: TGraphic;
		function GetZoom: Single;
		procedure SetZoom(Value: Single);
		function Query(Prop: TCameraProp): Variant;
		procedure Snap;
		property Flash: Boolean read GetFlash write SetFlash;
    property Mode: TCameraMode read GetMode write SetMode;
    property Owner: TObject read GetOwner write SetOwner;
		property Handle: THandle read GetHandle;
		property ImageSize: TImageSize read GetImageSize write SetImageSize;
		property Snapshot: TGraphic read GetSnapshot;
		property View: TGraphic read GetView;
		property Zoom: Single read GetZoom write SetZoom;
	end;

{ TCamera }

	TCamera = class(TObject)
	private
    FManager: TCameraManager;
		FCamera: ICamera;
    FWindow: TUtilityWindow;
		FThread: TCommandThread;
    procedure CommandExecute(Sender: TObject; Command: TCommand);
    function GetBusy: Boolean;
		function GetCameraIndex: Integer;
		function GetFlash: Boolean;
		procedure SetFlash(Value: Boolean);
		function GetHandle: THandle;
		function GetMode: TCameraMode;
		procedure SetMode(Value: TCameraMode);
		function GetImageSize: TImageSize;
		procedure SetImageSize(Value: TImageSize);
		function GetSnapshot: TGraphic;
		function GetView: TGraphic;
		function GetZoom: Single;
		procedure SetZoom(Value: Single);
	public
		constructor Create(Manager: TCameraManager; Camera: ICamera);
		destructor Destroy; override;
		function Query(Prop: TCameraProp): Variant;
		procedure Snap;
    property Busy: Boolean read GetBusy;
		property CameraIndex: Integer read GetCameraIndex;
		property Flash: Boolean read GetFlash write SetFlash;
    property Mode: TCameraMode read GetMode write SetMode;
		property Handle: THandle read GetHandle;
		property ImageSize: TImageSize read GetImageSize write SetImageSize;
		property Snapshot: TGraphic read GetSnapshot;
		property View: TGraphic read GetView;
		property Zoom: Single read GetZoom write SetZoom;
	end;

{ TCameraManager }

	TCameraEvent = procedure(Sender: TObject; Camera: TCamera) of object;

	TCameraManager = class(TObject)
	private
    FDestroying: Boolean;
		FCameras: TList;
    FWindow: TUtilityWindow;
		FThread: TThread;
		FOnChange: TNotifyEvent;
    FOnModeChange: TCameraEvent;
		FOnSnap: TCameraEvent;
		FOnViewFinder: TCameraEvent;
		function GetBusy: Boolean;
		function GetCameraCount: Integer;
		function GetCamera(Index: Integer): TCamera;
	protected
		procedure Change; virtual;
	public
		constructor Create;
		destructor Destroy; override;
    procedure DefaultHandler(var Msg); override;
		procedure DetectCameras;
		procedure ReleaseCameras;
		procedure Snap;
		property Busy: Boolean read GetBusy;
		property CameraCount: Integer read GetCameraCount;
		property Cameras[Index: Integer]: TCamera read GetCamera;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnModeChange: TCameraEvent read FOnModeChange write FOnModeChange;
		property OnSnap: TCameraEvent read FOnSnap write FOnSnap;
		property OnViewFinder: TCameraEvent read FOnViewFinder write FOnViewFinder;
	end;

{ TCameraScanner }

	TCameraScanner = procedure(Wnd: HWND);

procedure RegisterCameraScanner(Scanner: TCameraScanner);

{ Thread safe notifications }

procedure NotifyScanDone(Wnd: HWND);
procedure NotifyAddCamera(Wnd: HWND; Camera: ICamera);
procedure NotifyRemoveCamera(Wnd: HWND; Camera: ICamera);
procedure NotifyViewFinder(Wnd: HWND; Camera: ICamera);
procedure NotifySnapShot(Wnd: HWND; Camera: ICamera);
procedure NotifyModeChange(Wnd: HWND; Camera: ICamera);

implementation

{ Message constants }

const
  WM_SCANDONE = WM_USER + $F0;
  WM_ADDCAMERA = WM_SCANDONE + 1;
  WM_REMOVECAMERA = WM_ADDCAMERA + 1;
  WM_VIEWFINDER = WM_REMOVECAMERA + 1;
  WM_SNAPSHOT = WM_VIEWFINDER + 1;
  WM_MODECHANGE = WM_SNAPSHOT + 1;

{ Command constants }

	coFlash = 3;
  coImageSize = 4;
  coMode = 5;
  coZoom = 6;
  coSnap = 7;

{ TCamera }

constructor TCamera.Create(Manager: TCameraManager; Camera: ICamera);
begin
	inherited Create;
  FManager := Manager;
  Camera.Owner := Self;
	FCamera := Camera;
  FWindow := TUtilityWindow.Create(Self);
	FThread := TCommandThread.Create(CommandExecute, FWindow.Handle);
  Mode := cmViewfinder;
end;

destructor TCamera.Destroy;
begin
  FWindow.Free;
  FThread.Wait;
	inherited Destroy;
end;

procedure TCamera.CommandExecute(Sender: TObject; Command: TCommand);
var
  Camera: ICamera;
begin
  Camera := FCamera;
  if Camera = nil then Exit;
	case Command.Kind of
		coFlash: Camera.Flash := Command.Value;
		coImageSize: Camera.ImageSize := Command.Value;
    coMode: Camera.Mode := Command.Value;
		coSnap: Camera.Snap;
		coZoom: Camera.Zoom := Command.Value;
	end;
end;

function TCamera.Query(Prop: TCameraProp): Variant;
begin
	Result := FCamera.Query(Prop);
end;

procedure TCamera.Snap;
begin
	FThread.Push(coSnap);
end;

function TCamera.GetBusy: Boolean;
begin
	Result := FThread.Busy;
end;

function TCamera.GetCameraIndex: Integer;
begin
  Result := FManager.FCameras.IndexOf(Self);
end;

function TCamera.GetHandle: THandle;
begin
	Result := FCamera.GetHandle;
end;

function TCamera.GetMode: TCameraMode;
begin
	Result := FCamera.Mode;
end;

procedure TCamera.SetMode(Value: TCameraMode);
begin
	FThread.Push(coMode, Value);
end;

function TCamera.GetFlash: Boolean;
begin
	Result := FCamera.GetFlash;
end;

procedure TCamera.SetFlash(Value: Boolean);
begin
	FThread.Push(coFlash, Value);
end;

function TCamera.GetImageSize: TImageSize;
begin
	Result := FCamera.GetImageSize;
end;

procedure TCamera.SetImageSize(Value: TImageSize);
begin
	FThread.Push(coImageSize, Value);
end;

function TCamera.GetSnapshot: TGraphic;
begin
	Result := FCamera.Snapshot;
end;

function TCamera.GetView: TGraphic;
begin
	Result := FCamera.GetView;
end;

function TCamera.GetZoom: Single;
begin
	Result := FCamera.GetZoom;
end;

procedure TCamera.SetZoom(Value: Single);
begin
	FThread.Push(coZoom, Value);
end;

{ TScannerThread }

type
	PScannerLink = ^TScannerLink;
	TScannerLink = record
		Scanner: TCameraScanner;
		Link: PScannerLink;
	end;

var
	ScannerLinks: PScannerLink;

type
	TScannerThread = class(TThread)
	private
		FWnd: HWND;
	protected
		procedure Execute; override;
	public
		constructor Create(Wnd: HWND);
	end;

constructor TScannerThread.Create(Wnd: HWND);
begin
	FWnd := Wnd;
	inherited Create(False);
end;

procedure TScannerThread.Execute;
var
	Link: PScannerLink;
begin
  FreeOnTerminate := True;
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  Link := ScannerLinks;
  while Link <> nil do
  begin
    Link.Scanner(FWnd);
    Link := Link.Link;
	end;
  CoUninitialize;
  NotifyScanDone(FWnd);
end;

{ TCameraManager }

constructor TCameraManager.Create;
begin
	inherited Create;
	FCameras := TList.Create;
end;

destructor TCameraManager.Destroy;
begin
  FDestroying := True;
  ReleaseCameras;
	FCameras.Free;
	inherited Destroy;
end;

procedure TCameraManager.Change;
begin
  if Busy then
    Exit;
	if Assigned(FOnChange) then
		FOnChange(Self);
end;

procedure TCameraManager.DefaultHandler(var Msg);
var
  M: TMessage absolute Msg;
begin
  case M.Msg of
    WM_SCANDONE:
      begin
        FThread := nil;
        Change;
      end;
    WM_ADDCAMERA:
      begin
        FCameras.Add(TCamera.Create(Self, ICamera(M.WParam)));
        Change;
      end;
    WM_REMOVECAMERA:
      begin
        FCameras.Remove(ICamera(M.WParam).Owner);
        ICamera(M.WParam).Owner.Free;
        Change;
      end;
    WM_MODECHANGE:
      if Assigned(FOnModeChange) then
        FOnModeChange(Self, ICamera(M.WParam).Owner as TCamera);
    WM_VIEWFINDER:
      if Assigned(FOnViewFinder) then
        FOnViewFinder(Self, ICamera(M.WParam).Owner as TCamera);
    WM_SNAPSHOT:
      if Assigned(FOnSnap) then
        FOnSnap(Self, ICamera(M.WParam).Owner as TCamera);
  end;
end;

procedure TCameraManager.DetectCameras;
begin
  ReleaseCameras;
  FWindow := TUtilityWindow.Create(Self);
  FThread := TScannerThread.Create(FWindow.Handle);
end;

procedure TCameraManager.ReleaseCameras;
var
  I: Integer;
begin
	FWindow.Free;
  FWindow := nil;
  FThread.Free;
	for I := FCameras.Count - 1 downto 0 do
		TCamera(FCameras[I]).Mode := cmDisconnected;
	for I := FCameras.Count - 1 downto 0 do
		TObject(FCameras[I]).Free;
  FCameras.Clear;
  Change;
end;

procedure TCameraManager.Snap;
var
  I: Integer;
begin
	for I := FCameras.Count - 1 downto 0 do
		TCamera(FCameras[I]).Snap;
end;

function TCameraManager.GetBusy: Boolean;
begin
	Result := FThread <> nil;
end;

function TCameraManager.GetCameraCount: Integer;
begin
	if Busy then
		Result := 0
	else
		Result := FCameras.Count;
end;

function TCameraManager.GetCamera(Index: Integer): TCamera;
begin
	Result := TCamera(FCameras[Index]);
end;

{ RegisterCameraScanner }

procedure RegisterCameraScanner(Scanner: TCameraScanner);

	procedure AddLink(var Link: PScannerLink);
	begin
		if Link = nil then
		begin
			New(Link);
			Link.Scanner := Scanner;
			Link.Link := nil;
		end
		else if @Link.Scanner <> @Scanner then
			AddLink(Link.Link);
	end;

begin
	if @Scanner <> nil then
		AddLink(ScannerLinks);
end;

procedure DisposeLinks;

	procedure DisposeLink(Link: PScannerLink);
	begin
		if Link <> nil then
		begin
			DisposeLink(Link.Link);
			Dispose(Link);
		end;
	end;

begin
	DisposeLink(ScannerLinks);
end;

{ Notifications }

procedure NotifyScanDone(Wnd: HWND);
begin
  SendMessage(Wnd, WM_SCANDONE, 0, 0);
end;

procedure NotifyAddCamera(Wnd: HWND; Camera: ICamera);
begin
  SendMessage(Wnd, WM_ADDCAMERA, Integer(Camera), 0);
end;

procedure NotifyRemoveCamera(Wnd: HWND; Camera: ICamera);
begin
  SendMessage(Wnd, WM_REMOVECAMERA, Integer(Camera), 0);
end;

procedure NotifyViewFinder(Wnd: HWND; Camera: ICamera);
begin
  SendMessage(Wnd, WM_VIEWFINDER, Integer(Camera), 0);
end;

procedure NotifySnapShot(Wnd: HWND; Camera: ICamera);
begin
  SendMessage(Wnd, WM_SNAPSHOT, Integer(Camera), 0);
end;

procedure NotifyModeChange(Wnd: HWND; Camera: ICamera);
begin
  SendMessage(Wnd, WM_MODECHANGE, Integer(Camera), 0);
end;

initialization
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
finalization
	DisposeLinks;
  CoUninitialize;
end.
