
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2007                   *)
(*                                                      *)
(********************************************************)

unit CanonObj;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages,  Classes, SysUtils, Graphics, SysTools, CameraCtrls,
  CanonSdk, Jpeg;

procedure InitCanonSdk;
procedure FinishCanonSdk;

implementation

var
	SdkLoaded: Boolean;

{ TCanonCamera }

type
	TCanonCamera = class(TInterfacedObject, IUnknown, ICamera)
  private
    FWnd: HWND;
  	FSourceInfo: TCDSourceInfo;
    FMode: TCameraMode;
    FOwner: TObject;
  	FSource: cdHSource;
    FCapability: cdReleaseControlCap;
    FReleaseControl: Boolean;
    FSnapshot: TGraphic;
    FView: TBitmap;
    FViewFinder: Boolean;
    FRegisterEvent: Boolean;
    FEvent: cdHandle;
    FViewTimer: TDateTime;
    FLastImageSize: TImageSize;
	protected
  	{ ICamera }
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
  public
		constructor Create(Wnd: HWND; const SourceInfo: TCDSourceInfo);
    destructor Destroy; override;
  end;

{ TSyncObject }

type
	TCameraSync = class(TSyncObject)
  private
  	FCameraList: TList;
  public
  	constructor Create;
    destructor Destroy; override;
    procedure Add(Camera: TCanonCamera);
    procedure Remove(Camera: TCanonCamera);
    function FindCamera(Camera: TCanonCamera): Boolean;
  end;

var
	CameraSync: TCameraSync;

constructor TCameraSync.Create;
begin
	inherited Create;
	FCameraList := TList.Create;
end;

destructor TCameraSync.Destroy;
begin
  FCameraList.Free;
  inherited Destroy;
end;

procedure TCameraSync.Add(Camera: TCanonCamera);
begin
	FCameraList.Add(Camera);
end;

procedure TCameraSync.Remove(Camera: TCanonCamera);
begin
	FCameraList.Remove(Camera);
end;

function TCameraSync.FindCamera(Camera: TCanonCamera): Boolean;
begin
	Result := FCameraList.IndexOf(Camera) > -1;
end;

{ These callback events are not thread safe }

type
	TCallbackParams = record
  	EventID: cdReleaseEventID;
    Data: Pointer;
    Size: cdUInt32;
    Format: cdUInt32;
    Context: cdContext;
    Result: cdUInt32;
  end;
  PCallbackParams = ^TCallbackParams;

function CallbackParams(var Params: TCallbackParams;
	EventID: cdReleaseEventID; Data: Pointer; Size: cdUInt32;
  Format: cdUInt32; Context: cdContext): Pointer;
begin
	Params.EventID := EventID;
	Params.Data := Data;
	Params.Size := Size;
	Params.Format := Format;
	Params.Context := Context;
	Params.Result := CD_OK;
	Result := @Params;
end;

{ ReleaseCallback is called when a picture is taken }

function ReleaseCallback(EventID: cdReleaseEventID; Data: Pointer; Size: cdUInt32; Context: cdContext): cdUInt32; stdcall;
begin
  Result := CD_OK;
end;

function EventCallback(EventID: cdReleaseEventID; Data: Pointer; Size: cdUInt32; Context: cdContext): cdUInt32; stdcall;
var
  C: TCanonCamera;
begin
  Result := CD_OK;
	if CDEventSeverityMask(EventID) = CD_EVENT_SEVERITY_SHUTDOWN then
  begin
    C := TCanonCamera(Context);
    C.FMode := cmDisconnected;
    C.FSource := 0;
		NotifyRemoveCamera(C.FWnd, C);
	end;
end;

function ViewFinderCallback(Buf: Pointer; Size: cdUInt32; Format: cdUInt32; Context: cdContext): cdError; stdcall;
var
  C: TCanonCamera;
  T: TDateTime;
  S: TMemoryStream;
begin
  Result := CD_OK;
  C := TCanonCamera(Context);
  	T := Now;
    if C.FViewTimer > T then
      Exit
		else if	C.FViewTimer < T then
			C.FViewTimer := T + FinderDelay;
    S := TMemoryStream.Create;
    try
    	S.Write(Buf^, Size);
      S.Seek(soFromBeginning, 0);
      C.FView.LoadFromStream(S);
    finally
    	S.Free;
    end;
		NotifyViewFinder(C.FWnd, C);
end;

constructor TCanonCamera.Create(Wnd: HWND; const SourceInfo: TCDSourceInfo);

	function Capable: Boolean;
	const
		Requirements = CD_RELEASE_CONTROL_CAP_SUPPORT or CD_RELEASE_CONTROL_CAP_VIEWFINDER;
  begin
  	Result := FCapability and Requirements = Requirements;
  end;

begin
	inherited Create;
  FLastImageSize := isMedium;
  FWnd := Wnd;
  FSnapshot := TJPEGImage.Create;
	FView := TBitmap.Create;
  FSourceInfo := SourceInfo;
  SetMode(cmReady);
end;

destructor TCanonCamera.Destroy;
begin
  CameraSync.Remove(Self);
	SetMode(cmDisconnected);
  FSnapshot.Free;
	FView.Free;
	inherited Destroy;
end;

function TCanonCamera.GetMode: TCameraMode;
begin
	Result := FMode;
end;

{ TCanonCamera.ICamera }

procedure TCanonCamera.SetMode(Value: TCameraMode);
var
  PriorMode: TCameraMode;
begin
	if Value <> FMode then
	begin
    PriorMode := FMode;
		case Value of
    	cmDisconnected:
      	if FMode > Value then
      	begin
					if FViewFinder then
						CDTermViewfinder(FSource);
					if FReleaseControl then
		  	    CDExitReleaseControl(FSource);
			  	if FRegisterEvent then
						CDUnregisterEventCallbackFunction(FSource, FEvent);
					FViewFinder := False;
					FReleaseControl := False;
					FRegisterEvent := False;
			    Sleep(500);
					CDCloseSource(FSource);
		    	FSource := 0;
			  	FMode := Value;
        end;
			cmReady:
      	if FMode > Value then
      	begin
					if FViewFinder then
						CDTermViewfinder(FSource);
					if FReleaseControl then
		  	    CDExitReleaseControl(FSource);
					FViewFinder := False;
					FReleaseControl := False;
			    Sleep(500);
			  	FMode := Value;
        end
        else if CDOpenSource(FSourceInfo, FSource) = CD_OK then
        begin
      		FRegisterEvent := CDRegisterEventCallbackFunction(FSource, @EventCallback, cdContext(Self), FEvent) = CD_OK;
          FMode := cmReady;
        end;
			cmRemote:
      	if FMode = cmReady then
      	begin
          FView.Free;
          FView := TBitmap.Create;
					FReleaseControl := CDEnterReleaseControl(FSource, @ReleaseCallback, cdContext(Self)) = CD_OK;
					if FReleaseControl then
          begin
          	CDSelectReleaseDataKind(FSource, CD_REL_KIND_PICT_TO_PC);
            CDGetReleaseControlFaculty(FSource, FCapability);
            FMode := Value;
          end;
        end
        else if FMode = cmViewfinder then
        begin
					if FViewFinder then
						CDTermViewfinder(FSource);
					FViewFinder := False;
					FMode := Value;
          FView.Free;
          FView := TBitmap.Create;
        end;
			cmViewfinder:
      	begin
          SetMode(cmRemote);
          if FMode = cmRemote then
          begin
		      	FViewFinder := CDStartViewFinder(FSource, CD_FILE_FORMAT_BMP, @ViewFinderCallback, cdContext(Self)) = CD_OK;
            if FViewFinder then
							FMode := Value;
          end;
        end;
		end;
    if PriorMode <> FMode then
      NotifyModeChange(FWnd, Self);
  end;
end;

function TCanonCamera.GetOwner: TObject;
begin
  Result := FOwner;
end;

procedure TCanonCamera.SetOwner(Value: TObject);
begin
  FOwner := Value;
end;

function TCanonCamera.Query(Prop: TCameraProp): Variant;
var
	S: string;
	O, D: cdUInt32;
begin
	VarClear(Result);
	if FSource <> 0 then
  	case Prop of
			prName:
      	begin
					case FSourceInfo.PortType of
						CD_PORT_TYPE_STI:	S := FSourceInfo.u.STI.DeviceInternalName;
						CD_PORT_TYPE_WIA: S := FSourceInfo.u.WIA.pDIPDevID;
					else
						S := FSourceInfo.u.rsrvd.szLaunchedDeviceName;
					end;
					Result := S;
    		end;
			prMaxZoom:
  	  	begin
        	if CDGetMaximumZoomPos(FSource, D, O) = CD_OK then
          	Result := O
					else
          	Result := 1;
    	  end;
		end;
end;

function TCanonCamera.GetHandle: THandle;
begin
	Result := FSource;
end;

function TCanonCamera.GetFlash: Boolean;
var
	Mode: cdFlashMode;
  Compensation: cdCompensation;
begin
	Result := False;
	if (FSource <> 0) and (CDGetFlashSetting(FSource, Mode, Compensation) = CD_OK) then
		Result := (Mode and $F) <> CD_FLASH_MODE_OFF;
end;

procedure TCanonCamera.SetFlash(Value: Boolean);
var
	Mode: cdFlashMode;
  Compensation: cdCompensation;
begin
	if (FSource <> 0) and (CDGetFlashSetting(FSource, Mode, Compensation) = CD_OK) then
  begin
  	Mode := Mode and $F;
		if Value <> (Mode = CD_FLASH_MODE_ON) then
    begin
    	if Value then
      	Mode := CD_FLASH_MODE_ON
			else
      	Mode := CD_FLASH_MODE_OFF;
	  	CDSetFlashSetting(FSource, Mode, Compensation);
	  end;
	end;
end;

function TCanonCamera.GetImageSize: TImageSize;
var
	Quality: cdCompQuality;
	Size: cdImageSize;
begin
	Result := FLastImageSize;
	if (FSource <> 0) and (CDGetImageFormatAttribute(FSource, Quality, Size) = CD_OK) then
    case Size and $F of
			CD_IMAGE_SIZE_SMALL:
      	Result := isSmall;
			CD_IMAGE_SIZE_MEDIUM,
			CD_IMAGE_SIZE_MEDIUM1,
			CD_IMAGE_SIZE_MEDIUM2:
      	Result := isMedium;
			CD_IMAGE_SIZE_MEDIUM3:
      	Result := isLarge;
			CD_IMAGE_SIZE_LARGE:
      	Result := isExLarge;
		end;
end;

function TCanonCamera.GetSnapshot: TGraphic;
begin
	Result := FSnapshot;
end;

function TCanonCamera.GetView: TGraphic;
begin
	Result := FView;
end;

procedure TCanonCamera.SetImageSize(Value: TImageSize);
const
	Sizes: array[TImageSize] of cdImageSize =
		(0, CD_IMAGE_SIZE_SMALL, CD_IMAGE_SIZE_MEDIUM2, CD_IMAGE_SIZE_MEDIUM1, CD_IMAGE_SIZE_LARGE);
var
	Quality: cdCompQuality;
	Size: cdImageSize;
begin
	if Value = isUnknown then Exit;
	if FSource <> 0 then
  begin
    Quality := CD_COMP_QUALITY_FINE;
    Size := Sizes[Value];
    if CDSetImageFormatAttribute(FSource, Quality, Size) <> CD_OK then
      SetImageSize(Pred(Value));
	end;
end;

function TCanonCamera.GetZoom: Single;
var
	ZoomPos: cdUInt32;
begin
	Result := 0;
	if FSource <> 0 then
  begin
  	if CDGetZoomPos(FSource, ZoomPos) = CD_OK then
	    Result := ZoomPos;
  end;
end;

procedure TCanonCamera.SetZoom(Value: Single);
var
	ZoomCurrent, ZoomPos: cdUInt32;
begin
  if Value < 0 then
  	ZoomPos := 0
	else
		ZoomPos := Round(Value);
	if (FSource <> 0) and (CDGetZoomPos(FSource, ZoomCurrent) = CD_OK) and (ZoomCurrent <> ZoomPos) then
  	CDSetZoomPos(FSource, ZoomPos);
end;

procedure StreamOpen(Context: cdContext; Permission: cdPermission; var Err: cdError); stdcall;
begin
	Err := CD_OK;
end;

procedure	StreamClose(Context: cdContext; var Err: cdError); stdcall;
begin
	Err := CD_OK;
end;

procedure StreamRead(Context: cdContext; Buf: Pointer; var BufSize: cdUInt32; var Err: cdError); stdcall;
var
	Stream: TStream absolute Context;
begin
	Err := CD_OK;
  BufSize := Stream.Read(Buf^, BufSize);
end;

procedure StreamWrite(Context: cdContext; Buf: Pointer; var BufSize: cdUInt32; var Err: cdError); stdcall;
var
	Stream: TStream absolute Context;
begin
	Err := CD_OK;
  BufSize := Stream.Write(Buf^, BufSize);
end;

procedure StreamSeek(Context: cdContext; Whence: cdWhence; Offset: cdInt32; var Err: cdError); stdcall;
var
	Stream: TStream absolute Context;
begin
	Err := CD_OK;
  case Whence of
		CD_START: Stream.Seek(soFromBeginning, Offset);
		CD_CURRENT: Stream.Seek(soFromCurrent, Offset);
		CD_END: Stream.Seek(soFromEnd, Offset);
  end;
end;

function StreamTell(Context: cdContext; var Err: cdError): cdInt32; stdcall;
var
	Stream: TStream absolute Context;
begin
	Err := CD_OK;
  Result := Stream.Position;
end;

procedure CreateStgMedium(var StgMedium: TStgMedium; var Stream: TCameraStream);
begin
	StgMedium.MemType := CD_MEMTYPE_STREAM;
	StgMedium.Stream := @Stream;
  Stream.Open := @StreamOpen;
	Stream.Close := @StreamClose;
	Stream.Read := @StreamRead;
	Stream.Write := @StreamWrite;
	Stream.Seek := @StreamSeek;
	Stream.Tell := @StreamTell;
end;

function ReleaseProgress(Progress: cdUInt32; Status: cdProgressStatus; Context: cdContext): cdUInt32; stdcall;
begin
	Result := CD_OK;
end;

procedure TCanonCamera.Snap;
var
	PriorMode: TCameraMode;
	NumData: cdUInt32;
	Info: TReleaseImageInfo;
  Stream: TMemoryStream;
  StgMedium: TStgMedium;
  CameraStream: TCameraStream;
begin
	PriorMode := FMode;
	if FMode > cmReady then
  try
		CDSelectReleaseDataKind(FSource, CD_REL_KIND_PICT_TO_PC);
		CDGetReleaseControlFaculty(FSource, FCapability);
  	if (FMode > cmRemote) and (FCapability and CD_RELEASE_CONTROL_CAP_ABORT_VIEWFINDER = CD_RELEASE_CONTROL_CAP_ABORT_VIEWFINDER) then
			SetMode(cmRemote);
  	if CDRelease(FSource, 0, nil, 0, CD_PROG_NO_REPORT, NumData) = CD_OK then
    begin
      Stream := TMemoryStream.Create;
      try
	    	CreateStgMedium(StgMedium, CameraStream);
	      StgMedium.Stream.Context := cdContext(Stream);
	    	while NumData > 0 do
				begin
	        Stream.Clear;
					if (CDGetReleasedData(FSource, nil, 0, CD_PROG_NO_REPORT, Info, nil) = CD_OK) and
						(Info.DataType and CD_DATA_TYPE_PICTURE = CD_DATA_TYPE_PICTURE) and
	          (CDGetReleasedData(FSource, nil, 0, CD_PROG_NO_REPORT, Info, @StgMedium) = CD_OK) then
					begin
          	Stream.Seek(soFromBeginning, 0);
	          FSnapshot.LoadFromStream(Stream);
	        end;
					Dec(NumData);
				end;
			finally
				Stream.Free;
			end;
      NotifySnapShot(FWnd, Self)
		end;
  finally
    if PriorMode > FMode then
      SetMode(PriorMode);
	end;
end;

{ CanonCameraScanner }

procedure CanonCameraScanner(Wnd: HWND);
var
  Cameras: TInterfaceList;
	Enum: cdHEnum;
  Count: cdUInt32;
  I: Integer;
  SourceInfo: TCDSourceInfo;
begin
  if not SdkLoaded then Exit;
  Cameras := TInterfaceList.Create;
  try
    if CDEnumDeviceReset(1, Enum) = CD_OK then
    begin
  	  if CDGetDeviceCount(Enum, Count) = CD_OK then
      	while Count > 0 do
        begin
        	if CDEnumDeviceNext(Enum, SourceInfo) = CD_OK then
            Cameras.Add(TCanonCamera.Create(Wnd, SourceInfo) as ICamera);
          Dec(Count);
  			end;
      CDEnumDeviceRelease(Enum);
    end;
    for I := 0 to Cameras.Count - 1 do
      if ICamera(Cameras[I]).Mode = cmReady then
        NotifyAddCamera(Wnd, ICamera(Cameras[I]));
  finally
    Cameras.Clear;
  end;
end;

procedure StartHardware;
var
	V: TCDVersionInfo;
begin
	CameraSync := TCameraSync.Create;
	FillChar(V, SizeOf(V), #0);
	V.Size := SizeOf(TCDVersionInfo);
	V.MajorVersion := CDMajorVersion;
	V.MinorVersion := CDMinorVersion;
	SdkLoaded := CDStartSDK(V, 0) = CD_OK;
  if SdkLoaded then
  	RegisterCameraScanner(@CanonCameraScanner);
end;

procedure FinishHardware;
begin
	CDFinishSDK;
  CameraSync.Free;
end;

procedure InitCanonSdk;
begin
	SdkLoaded := LoadCanonSdk;
  if SdkLoaded then
	  StartHardware;
end;

procedure FinishCanonSdk;
begin
  if SdkLoaded then
		FinishHardware;
end;

end.
