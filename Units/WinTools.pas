
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit WinTools;

interface

{$I CODEBOT.INC}

uses
  Classes, Windows, Messages, ActiveX, SysUtils, ShellAPI, PSapi,
  FileTools, StrTools;

function NumCpuCores: Integer;

procedure ChangeUserAccess(Enabled: Boolean);

{ The GetWindowClassName function }

function GetWindowClassName(Wnd: HWND): string;

{ The GetWindowCaption function }

function GetWindowCaption(Wnd: HWND): string;

{ The IsWindowClass function }

function IsWindowClass(const ClassName: string; const Module: string = ''): Boolean;

{ The GetDesktopWindows procedure }

type
  TWindowStringFormat = set of (sfCaption, sfClassName, sfHandle, sfVisibility);

procedure GetDesktopWindows(Windows: TStrings; Format: TWindowStringFormat);

{ The GetChildWindows procedure }

procedure GetChildWindows(Parent: HWND; Windows: TStrings; Format: TWindowStringFormat);

{ The FindSimilarChild function }

function FindSimilarChild(Wnd: HWND; const ClassName: string): HWND;

{ The FindNestedChild function }

function FindNestedChild(Wnd: HWND; const ClassName: string; Index: Integer = 0): HWND;

{ The FindExactChild function }

function FindExactChild(Wnd: HWND; const ClassName, Caption: string): HWND;

{ The HideTaskbarIcon procedure }

procedure HideTaskbarIcon(Wnd: HWND);

{ The window position routines are used to query and modify the dimensions of
  a window using the TWindowPosition structure }

type
  TWindowPosition = record
  	case Boolean of
     False: (
	    Left: Integer;
  	  Top: Integer;
    	Width: Integer;
	    Height: Integer);
		True: (
    	Pos: TPoint;
      Size: TPoint);
  end;

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);
function IsWindowPosition(Wnd: HWND; const Pos: TWindowPosition): Boolean;

{ Bounds as related to the taskbar }

function GetTaskbarRect: TRect;
function GetTrayBounds(Width, Height: Integer; Offset: Integer = 10): TRect;

{ Global shortcut reoutines }

function RegisterShortCut(Wnd: HWND; Id: Integer; ShortCut: TShortCut): Boolean;
function UnregisterShortCut(Wnd: HWND; Id: Integer): Boolean;

{ The InvalidateWidows procedure }

procedure InvalidateWindows(Wnd: HWND);

{ The GetEnvironmentVariable function }

function GetEnvironmentVariable(const Name: string): string;

{ The TerminateProcess function }

function TerminateProcess(Process: THandle): Boolean;

{ The IsProcessWindow function }

function IsProcessWindow(Wnd: HWND): Boolean;

{ The IsProcess function }

function IsProcess(Process: THandle): Boolean;

{ The WindowFromPoint function returns any window, child or not, below the
  Point parameter. This function ignores hidden windows }

function WindowFromPoint(const Point: TPoint): HWND;

{ The GetDialogParent function }

function GetDialogParent(Wnd: HWND): HWND;

{ The ShutdownWindows procedure exits the current window session }

procedure ShutdownWindows;

{ Create process routines }

type
  TReadProc = procedure(const S: string; Data: Pointer);
  TWaitProc = procedure(Interval: Integer; Data: Pointer);

procedure CreateExclusiveProcess(const AppName: string);
function CreateProcessAndReturn(const AppName: string; ShowState: Integer): THandle;
function CreateProcessAndWait(const AppName: string; ShowState: Integer): Longword; overload;
function CreateProcessAndWait(const AppName: string; ShowState: Integer;
  Data: Pointer; WaitProc: TWaitProc): Longword; overload;

function CreateProcessAndRedirect(const AppName: string; Data: Pointer;
  ReadProc: TReadProc; WaitProc: TWaitProc): Boolean;

function IsWindowsXPOrLater: Boolean;

{ Task scheduling }

type
	TDay = (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);
  TDays = set of TDay;

const
	AllDays = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday];

{function ScheduleTask(Days: TDays; Time: TDateTime; const Command: string; Interactive: Boolean = False): Integer;
procedure DeleteTask(Task: Integer);}

function ScheduleTask(Days: TDays; Time: TDateTime; const Command: string; Interactive: Boolean = False): Integer; overload;
procedure DeleteTask(Task: Integer); overload;
procedure ScheduleTask(const Name, Command: string; Days: TDays; Time: TDateTime); overload;
procedure DeleteTask(const Name: string); overload;

{ The ScanProcessMemory procedure }

type
  TScanProc = procedure(Memory: Pointer; Size: Integer);

procedure ScanProcessMemory(ScanProc: TScanProc);

{ TLauncher class }

type
  TShowState = (ssHide, ssNormal, ssMinimized, ssMaximized);

  TLaunchOperation = (loOpen, loPrint, loExplore);

  TLaunchInfo = record
    ProcessInfo: TProcessInformation;
    StartupInfo: TStartupInfo;
    ProcessMask: DWORD;
    AfinityMask: DWORD;
  end;

  ELauncherError = class(Exception);

  TLauncher = class
  private
    FLaunchInfo: TLaunchInfo;
    FRunning: Boolean;
    FStartTick: Cardinal;
    FFileName: string;
    FInterval: Integer;
    FParams: string;
    FOperation: TLaunchOperation;
    FShowState: TShowState;
    FOnWait: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
    FOnLaunch: TNotifyEvent;
    procedure SetFileName(const Value: string);
    procedure SetInterval(const Value: Integer);
    procedure SetParams(const Value: string);
    function GetRunning: Boolean;
    function GetElapsedTime: Cardinal;
  public
    procedure Launch;
    procedure Terminate;
    procedure Wait;
    property Running: Boolean read GetRunning;
    property ElapsedTime: Cardinal read GetElapsedTime;
    property FileName: string read FFileName write SetFileName;
    property Params: string read FParams write SetParams;
    property Operation: TLaunchOperation read FOperation write FOperation;
    property ShowState: TShowState read FShowState write FShowState;
    property Interval: Integer read FInterval write SetInterval;
    property OnLaunch: TNotifyEvent read FOnLaunch write FOnLaunch;
    property OnWait: TNotifyEvent read FOnWait write FOnWait;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

{ TRedirector class }

  TPriorityClass = (pcDefault, pcIdle, pcNormal, pcHigh, pcRealtime);

  TDataEvent = procedure(Sender: TObject; Buffer: Pointer; Size: Integer) of object;

  TPipeError = record
    hRead: DWORD;
    hWrite: DWORD;
  end;

  TRedirector = class
  private
    FAvailable: Integer;
    FProcessInfo: TProcessInformation;
    FExitCode: Integer;
    FExecutable: string;
    FCommandline: string;
    FDefaultErrorMode: Boolean;
    FStartSuspended: Boolean;
    FKillOnDestroy: Boolean;
    FDirectory: string;
    FEnvironment: Pointer;
    FInitialPriority: TPriorityClass;
    FPipeInput: TPipeError;
    FPipeOutput: TPipeError;
    FPipeError: TPipeError;
    FThread: TThread;
    FShowState: TShowState;
    FOnData: TDataEvent;
    FOnErrorData: TDataEvent;
    FOnTerminated: TNotifyEvent;
    procedure ReadStdOutput;
    procedure ReadStdError;
    procedure ProcessTerminated;
  protected
    procedure Error(Msg: string);
    procedure WinError(Msg: string);
    procedure CreatePipes;
    procedure ClosePipes;
    function GetRunning: Boolean;
    function GetExitCode: Integer;
    function GetProcessID: Integer;
    function GetThreadID: Integer;
    function GetProcessHandle: Integer;
    procedure SetShowState(Value: TShowState);
    function GetThreadHandle: Integer;
    procedure SetExecutable(Value: string);
    function GetCommandLine: string;
    procedure SetCommandLine(Value: string);
    procedure SetDefaultErrorMode(Value: Boolean);
    procedure SetStartSuspended(Value: Boolean);
    procedure SetInitialPriority(Value: TPriorityClass);
    procedure SetDirectory(Value: string);
    procedure SetEnvironment(Value: Pointer);
  public
    destructor Destroy; override;
    procedure Terminate;
    function Execute: THandle;
    procedure SendData(Buffer: Pointer; BufferSize: Integer);
    procedure SendText(S: string);
    property Running: Boolean read GetRunning;
    property ExitCode: Integer read GetExitCode;
    property ProcessID: Integer read GetProcessID;
    property ProcessHandle: Integer read GetProcessHandle;
    property ThreadID: Integer read GetThreadID;
    property ThreadHandle: Integer read GetThreadHandle;
    property Environment: Pointer read FEnvironment write SetEnvironment;
    property KillOnDestroy: Boolean read FKillOnDestroy write FKillOnDestroy;
    property Executable: string read FExecutable write SetExecutable;
    property CommandLine: string read GetCommandLine write SetCommandLine;
    property ShowState: TShowState read FShowState write SetShowState;
    property DefaultErrorMode: Boolean read FDefaultErrorMode write SetDefaultErrorMode;
    property StartSuspended: Boolean read FStartSuspended write SetStartSuspended;
    property InitialPriority: TPriorityClass read FInitialPriority write SetInitialPriority;
    property Directory: string read FDirectory write SetDirectory;
    property OnData: TDataEvent read FOnData write FOnData;
    property OnErrorData: TDataEvent read FOnErrorData write FOnErrorData;
    property OnTerminated: TNotifyEvent read FOnTerminated write FOnTerminated;
  end;

{ TPerformanceTimer class }

  EPerformanceError = class(Exception);

  TPerformanceTimer = class(TObject)
  private
    FResolution: Int64;
    FStart: Int64;
    FStop: Int64;
    FTiming: Boolean;
    function GetElapsedTime: LongWord;
  public
    constructor Create;
    procedure Start;
    procedure Stop;
    property ElapsedTime: LongWord read GetElapsedTime;
  end;

{ TCommandThread class }

	TCommand = class
		Kind: Integer;
		Value: Variant;
    constructor Create(Kind: Integer); overload;
    constructor Create(Kind: Integer; Value: Variant); overload;
	end;

	TCommandEvent = procedure(Sender: TObject; Command: TCommand) of object;

	TCommandThread = class(TThread)
	private
  	FBusy: Boolean;
		FCommandEvent: TCommandEvent;
		FCommands: TList;
		FMutex: THandle;
    FWait: THandle;
    FWnd: HWND;
	protected
		procedure Execute; override;
		function Pop: TCommand;
	public
		constructor Create(CommandEvent: TCommandEvent; Wnd: HWND = 0);
		destructor Destroy; override;
    procedure Wait;
    procedure Push(Kind: Integer); overload;
		procedure Push(Kind: Integer; Value: Variant); overload;
    function SendRequest(Msg: Cardinal): Pointer;
    property Wnd: HWND read FWnd;
    property Busy: Boolean read FBusy;
	end;

{ TGlobalData class }

function GlobalDataExists(const Name: string): Boolean;

type
  TGlobalData = class(TObject)
  private
    FSize: Integer;
    FLocked: Boolean;
    FMap: THandle;
    FMutex: THandle;
    FData: Pointer;
    FCreator: Boolean;
    FName: string;
    function GetData: Pointer;
  public
    constructor Create(const Name: string; Size: Integer);
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
    property Data: Pointer read GetData;
    property Creator: Boolean read FCreator;
    property Locked: Boolean read FLocked;
    property Name: string read FName;
    property Size: Integer read FSize;
  end;

{ TMemoryMappedFile class }

  EFileMappingError = class(Exception);

  TMemoryMappedFile = class(TObject)
  private
    FFileName: string;
    FFile: HFile;
    FMap: THandle;
    FViewStart: PChar;
    FViewEnd: PChar;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property ViewStart: PChar read FViewStart;
    property ViewEnd: PChar read FViewEnd;
    property FileName: string read FFileName;
  end;

{ TBasePipe class }

  TPipeMode = (pmRead, pmWrite);

  TBasePipe = class(TObject)
  private
    FHandle: THandle;
    FConnected: Boolean;
    FMode: TPipeMode;
    FName: string;
  public
    constructor Create(const AName: string; AMode: TPipeMode); virtual;
    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    function Read(var Buffer; Count: LongWord): LongWord;
    function Write(const Buffer; Count: LongWord): LongWord;
    property Connected: Boolean read FConnected;
    property Handle: THandle read FHandle;
    property Mode: TPipeMode read FMode;
    property Name: string read FName;
  end;

{ TServerPipe class }

  TServerPipe = class(TBasePipe)
  public
    constructor Create(const AName: string; AMode: TPipeMode); override;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
  end;

{ TClientPipe class }

  TClientPipe = class(TBasePipe)
  private
    FRemoteMachine: string;
  public
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    property RemoteMachine: string read FRemoteMachine write FRemoteMachine;
  end;

{ TPipeThread class }

  TPipeThreadParams = record
    Thread: TThread;
    Pipe: TBasePipe;
    Instance: Pointer;
    Data: LongWord;
  end;

  TPipeProc = procedure(const Params: TPipeThreadParams);

  TPipeThread = class(TThread)
  private
    FParams: TPipeThreadParams;
    FPipeProc: TPipeProc;
  public
    constructor Create(const Params: TPipeThreadParams; PipeProc: TPipeProc);
    destructor Destroy; override;
    procedure Execute; override;
    property Terminated;
  end;

{ TDirectoryMonitor class }

  TMonitorOption = (moFileName, moDirName, moAttributes, moSize, moLastWrite,
    moLastAccess, moCreation, moSecurity);

  TMonitorOptions = set of TMonitorOption;

  TFileAction = (faAdded, faRemoved, faModified,
    faRenamedOldName, faRenamedNewName);

  TFileChangeEvent = procedure(Sender: TObject; Action: TFileAction;
    const FileName: string) of object;

  TDirectoryMonitor = class(TObject)
  private
    FThread: TThread;
    FDirectory: string;
    FWindowHandle: THandle;
    FMonitorOptions: TMonitorOptions;
    FWatchSubFolders: Boolean;
    FOnFileChange: TFileChangeEvent;
    procedure InternalWinProc(var Msg: TMessage);
    procedure SetDirectory(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Directory: string read FDirectory write SetDirectory;
    property Options: TMonitorOptions read FMonitorOptions write FMonitorOptions;
    property WatchSubFolders: Boolean read FWatchSubFolders write FWatchSubFolders;
    property OnFileChange: TFileChangeEvent read FOnFileChange write FOnFileChange;
  end;

procedure GetProcessModules(Strings: TStrings);
procedure EnumAllProcess(Strings: TStrings);

implementation

uses
  StrConst;

function NumCpuCores: Integer;
asm
  PUSH  EBX
  MOV		EAX, 1
  CPUID
  MOV		EAX, EBX
  AND		EAX, $FF0000
  SHR		EAX, $000010
  POP   EBX
  TEST	EAX, EAX
  JNZ   @done
  MOV		EAX, 1
  @done:
end;

procedure ChangeUserAccess(Enabled: Boolean);
const
	Keys: array[0..3] of string = (
		'HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer\NoLogoff',
		'HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\System\DisableTaskMgr',
		'HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\System\DisableLockWorkstation',
		'HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\System\DisableChangePassword');
var
	Flag: Integer;
  I: Integer;
begin
	if Enabled then Flag := 0 else Flag := 1;
	for I := Low(Keys) to High(Keys) do
  	RegWriteInt(Keys[I], Flag);
end;

function GetWindowClassName(Wnd: HWND): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsWindow(Wnd) and (GetClassName(Wnd, Buffer, MAX_PATH) <> 0) then
    Result := Buffer
  else
    Result := '';
end;

function GetWindowCaption(Wnd: HWND): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsWindow(Wnd) and (GetWindowText(Wnd, Buffer, MAX_PATH) <> 0) then
    Result := Buffer
  else
    Result := '';
end;

procedure GetWindowPosition(Wnd: HWND; var Pos: TWindowPosition);
begin
  GetWindowRect(Wnd, TRect(Pos));
  with Pos do
  begin
    Dec(Width, Left);
    Dec(Height, Top);
  end;
  MapWindowPoints(GetDesktopWindow, GetParent(Wnd), TRect(Pos).TopLeft, 1);
end;

procedure SetWindowPosition(Wnd: HWND; const Pos: TWindowPosition);
begin
  with Pos do
    MoveWindow(Wnd, Left, Top, Width, Height, True);
end;

var
  WindowsStringFormat: TWindowStringFormat;

function EnumWindowsCallback(Wnd: HWND; Windows: TStrings): BOOL; stdcall;
const
  WindowStatus: array[Boolean] of string = ('hidden', 'visible');
var
  S: string;
begin
  S := '';
  if sfCaption in WindowsStringFormat then
    S := '"' + GetWindowCaption(Wnd) + '"';
  if sfClassName in WindowsStringFormat then
    S := Trim(S + ' ' + GetWindowClassName(Wnd));
  if sfHandle in WindowsStringFormat then
    S := Trim(S + ' [' + IntToHex(Wnd, 8) + ']');
  if sfVisibility in WindowsStringFormat then
    S := Trim(S + ' (' + WindowStatus[IsWindowVisible(Wnd)] + ')');
  if  WindowsStringFormat = [sfCaption] then
    S := Copy(S, 2, Length(S) - 2);
  Windows.AddObject(S, TObject(Wnd));
  Result := True;
end;

procedure GetDesktopWindows(Windows: TStrings; Format: TWindowStringFormat);
begin
  Windows.BeginUpdate;
  try
    Windows.Clear;
    WindowsStringFormat := Format;
    EnumWindows(@EnumWindowsCallback, Integer(Windows));
  finally
    Windows.EndUpdate;
  end;
end;

procedure GetChildWindows(Parent: HWND; Windows: TStrings; Format: TWindowStringFormat);
begin
  Windows.BeginUpdate;
  try
    Windows.Clear;
    WindowsStringFormat := Format;
    EnumChildWindows(Parent, @EnumWindowsCallback, Integer(Windows));
  finally
    Windows.EndUpdate;
  end;
end;

type
  TSimilarChild = record
    ClassName: PChar;
    Caption: PChar;
    Wnd: HWND;
    Index: Integer;
  end;
  PSimilarChild = ^TSimilarChild;

function EnumSimilarChild(Wnd: HWND; SimilarChild: PSimilarChild): BOOL; stdcall;
var
  SearchClass: string;
  ChildClass: string;
  P: PChar;
begin
  Result := True;
  SearchClass := SimilarChild.ClassName;
  ChildClass := GetWindowClassName(Wnd);
  P := PChar(ChildClass);
  if SearchToken(P, SearchClass) then
  begin
    SimilarChild.Wnd := Wnd;
    Result := False;
  end;
end;

function FindSimilarChild(Wnd: HWND; const ClassName: string): HWND;
var
  Child: TSimilarChild;
begin
  Result := 0;
  if ClassName <> '' then
  begin
    Child.ClassName := PChar(ClassName);
    Child.Wnd := 0;
    EnumChildWindows(Wnd, @EnumSimilarChild, Integer(@Child));
    Result := Child.Wnd;
  end;
end;

function EnumNestedChild(Wnd: HWND; SimilarChild: PSimilarChild): BOOL; stdcall;
var
  SearchClass: string;
  ChildClass: string;
begin
  Result := True;
  SearchClass := UpperCase(SimilarChild.ClassName);
  ChildClass := UpperCase(GetWindowClassName(Wnd));
  if UpperCase(SearchClass) = UpperCase(ChildClass) then
    if SimilarChild.Index = 0 then
    begin
      SimilarChild.Wnd := Wnd;
      Result := False;
    end
    else
      Dec(SimilarChild.Index);
  {if Result then
  begin
    EnumChildWindows(Wnd, @EnumNestedChild, Integer(@Child));
    Result :=
  end;}
end;

function FindNestedChild(Wnd: HWND; const ClassName: string; Index: Integer = 0): HWND;
var
  Child: TSimilarChild;
begin
  Result := 0;
  if ClassName <> '' then
  begin
    Child.ClassName := PChar(ClassName);
    Child.Wnd := 0;
    Child.Index := Index;
    EnumChildWindows(Wnd, @EnumNestedChild, Integer(@Child));
    Result := Child.Wnd;
  end;
end;


function EnumExactChild(Wnd: HWND; SimilarChild: PSimilarChild): BOOL; stdcall;
var
  S: string;
begin
  Result := True;
  S := GetWindowClassName(Wnd);
  if AnsiStrComp(SimilarChild.ClassName, PChar(S)) = 0 then
  begin
    S := GetWindowCaption(Wnd);
    if AnsiStrComp(SimilarChild.Caption, PChar(S)) = 0 then
    begin
      SimilarChild.Wnd := Wnd;
      Result := False;
    end;
  end;
end;

function FindExactChild(Wnd: HWND; const ClassName, Caption: string): HWND;
var
  Child: TSimilarChild;
begin
  Result := 0;
  if (ClassName <> '') and (Caption <> '') then
  begin
    Child.ClassName := PChar(ClassName);
    Child.Caption := PChar(Caption);
    Child.Wnd := 0;
    EnumChildWindows(Wnd, @EnumExactChild, Integer(@Child));
    Result := Child.Wnd;
  end;
end;

procedure HideTaskbarIcon(Wnd: HWND);
begin
  SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE
    and not WS_EX_APPWINDOW) or WS_EX_TOOLWINDOW);
  ShowWindow(Wnd, SW_HIDE);
end;

function IsWindowClass(const ClassName: string;
  const Module: string = ''): Boolean;
var
  WndClass: TWndClass;
  Handle: HMODULE;
begin
  FillChar(WndClass, SizeOf(WndClass), #0);
  if Module <> '' then
  begin
    Handle := GetModuleHandle(PChar(Module));
    if Handle <> 0 then
      Result := GetClassInfo(Handle, PChar(ClassName), WndClass)
    else
      Result := False;
  end
  else
    Result :=  GetClassInfo(HInstance, PChar(ClassName), WndClass);
end;

function GetEnvironmentVariable(const Name: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if Windows.GetEnvironmentVariable(PChar(Name), Buffer, MAX_PATH) > 0 then
    Result := Buffer
  else
    Result := '';
end;

function TerminateProcess(Process: THandle): Boolean;
begin
  Process := OpenProcess(PROCESS_ALL_ACCESS, TRUE, Process);
  if (Process <> 0) then
  begin
   Result := Windows.TerminateProcess(Process, 0);
   CloseHandle(Process);
  end
  else
    Result := False;
end;

function IsProcessWindow(Wnd: HWND): Boolean;
var
  Process: THandle;
begin
  Result := IsWindow(Wnd);
  if Result then
  begin
    GetWindowThreadProcessId(Wnd, @Process);
    Result := Process = GetCurrentProcessID;
  end;
end;

function IsProcess(Process: THandle): Boolean;
var
  ExitCode: DWORD;
begin
  Result := GetExitCodeProcess(Process, ExitCode) and (ExitCode = STILL_ACTIVE);
end;

function IsWindowPosition(Wnd: HWND; const Pos: TWindowPosition): Boolean;
var
  CurrentPos: TWindowPosition;
begin
  GetWindowPosition(Wnd, CurrentPos);
  Result := CompareMem(@CurrentPos, @Pos, SizeOf(TWindowPosition));
end;

function GetTaskbarRect: TRect;
var
  W: HWND;
begin
  W := FindWindow('Shell_TrayWnd', nil);
  if W <> 0 then
    GetWindowRect(W, Result)
  else
    FillChar(Result, SizeOf(Result), #0);
end;

function GetTrayBounds(Width, Height: Integer; Offset: Integer = 10): TRect;
var
  R: TRect;
  SW, SH: Integer;
begin
  R := GetTaskbarRect;
  SW := GetSystemMetrics(SM_CXSCREEN);
  SH := GetSystemMetrics(SM_CYSCREEN);
  if (R.Left < 1) and (R.Top < 1) then
    if R.Bottom < SH then
      Result.TopLeft := Point(SW - Width - Offset, R.Bottom + Offset)
    else
      Result.TopLeft := Point(R.Right + Offset, SH - Height - Offset)
  else if R.Left < 1 then
    Result.TopLeft := Point(SW - Width - Offset, R.Top - Height - Offset)
  else
    Result.TopLeft := Point(R.Left - Width - Offset, SH - Height - Offset);
  Result.Right := Result.Left + Width;
  Result.Bottom := Result.Top + Height;
end;

function RegisterShortCut(Wnd: HWND; Id: Integer; ShortCut: TShortCut): Boolean;
var
  M, K: Cardinal;
begin
  M := 0;
  if ShortCut and scShift = scShift then
    M := M or MOD_SHIFT;
  if ShortCut and scCtrl = scCtrl then
    M := M or MOD_CONTROL;
  if ShortCut and scAlt = scAlt then
    M := M or MOD_ALT;
  K := ShortCut and $FFF;
  Result := RegisterHotKey(Wnd, Id, M, K);
end;

function UnregisterShortCut(Wnd: HWND; Id: Integer): Boolean;
begin
  Result := UnregisterHotKey(Wnd, Id);
end;

function InvalidateCallback(Wnd: HWND; Unused: Integer): BOOL; stdcall;
begin
	InvalidateRect(Wnd, nil, True);
	EnumChildWindows(Wnd, @InvalidateCallback, 0);
  Result := True;
end;

procedure InvalidateWindows(Wnd: HWND);
begin
	InvalidateRect(Wnd, nil, True);
	EnumChildWindows(Wnd, @InvalidateCallback, 0);
end;

function WindowFromPoint(const Point: TPoint): HWND;
var
  Wnd: HWND;
  P: TPoint;
begin
  Result := 0;
  Wnd := GetDesktopWindow;
  while (Wnd <> Result) and IsWindow(Wnd) do
  begin
    Result := Wnd;
    P := Point;
    ScreenToClient(Result, P);
    Wnd := ChildWindowFromPointEx(Result, P, CWP_SKIPINVISIBLE);
  end;
end;

function GetDialogParent(Wnd: HWND): HWND;
begin
  Result := Wnd;
  if IsWindow(Result) then
    while GetWindowLong(Result, GWL_STYLE) and WS_CHILD = WS_CHILD do
      Result := GetParent(Result)
  else
    Result := 0;
end;

function CreateProcessAndReturn(const AppName: string; ShowState: Integer): THandle;
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := ShowState;
  end;
  if CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
    Result := ProcessInfo.dwProcessId
  else
    Result := 0;
end;


function InternalCreateProcess(const AppName: string; ShowState: Integer; out ProcessInfo: TProcessInformation): Boolean;
var
  StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := ShowState;
  end;
  Result := CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo);
end;

function CreateProcessAndWait(const AppName: string; ShowState: Integer): Longword; overload;
var
  ProcessInfo: TProcessInformation;
begin
  Result := 0;
  if InternalCreateProcess(AppName, ShowState, ProcessInfo) then
	begin
		WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
		WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
		CloseHandle(ProcessInfo.hProcess);
		CloseHandle(ProcessInfo.hThread);
	end;
end;

function CreateProcessAndWait(const AppName: string; ShowState: Integer;
  Data: Pointer; WaitProc: TWaitProc): Longword;
var
  ProcessInfo: TProcessInformation;
  Interval: Integer;
  State: Longword;
begin
  Result := 0;
  if InternalCreateProcess(AppName, ShowState, ProcessInfo) then
    Exit;
  Interval := 0;
  repeat
    State := WaitForSingleObject(ProcessInfo.hProcess, 10);
    if (State = WAIT_TIMEOUT) and Assigned(WaitProc) then
    begin
      Inc(Interval, 10);
      WaitProc(Interval, Data);
    end;
  until State <> WAIT_TIMEOUT;
  WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
  WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
  GetExitCodeProcess(ProcessInfo.hProcess, Result);
	CloseHandle(ProcessInfo.hProcess);
	CloseHandle(ProcessInfo.hThread);
end;

procedure CreateDesktopTask(const AppName, Desktop: string);
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    StartupInfo.lpDesktop := Pointer(Desktop);
  end;
  if CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
	begin
		WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
		WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
		CloseHandle(ProcessInfo.hProcess);
		CloseHandle(ProcessInfo.hThread);
	end;
end;

procedure CreateExclusiveProcess(const AppName: string);
var
	DesktopName: string;
	OriginalThread, OriginalInput, NewDesktop: THandle;
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
begin
	DesktopName := 'ExclusiveDesktop';
  OriginalThread := GetThreadDesktop(GetCurrentThreadId);
  OriginalInput := OpenInputDesktop(0, False, DESKTOP_SWITCHDESKTOP);
  NewDesktop := CreateDesktop(PChar(DesktopName), nil, nil, 0, GENERIC_ALL, nil);
  SetThreadDesktop(NewDesktop);
  SwitchDesktop(NewDesktop);
  ChangeUserAccess(False);
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  with StartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    StartupInfo.lpDesktop := Pointer(DesktopName);
  end;
  if CreateProcess(nil, PChar(AppName), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
	begin
		WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
		WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
		CloseHandle(ProcessInfo.hProcess);
		CloseHandle(ProcessInfo.hThread);
	end;
  ChangeUserAccess(True);
  SwitchDesktop(OriginalInput);
  SetThreadDesktop(OriginalThread);
  CloseDesktop(NewDesktop);
end;

function CreateProcessAndRedirect(const AppName: string; Data: Pointer;
  ReadProc: TReadProc; WaitProc: TWaitProc): Boolean;

type
  TPipeHandles = record
    Read: THandle;
    Write: THandle;
  end;

  procedure InitializePipes(var Pipes: TPipeHandles; Inherit: PHandle);
  var
    SecAttr: TSecurityAttributes;
  begin
    SecAttr.nLength := SizeOf(SecAttr);
    SecAttr.lpSecurityDescriptor := nil;
    SecAttr.bInheritHandle := True;
    with Pipes do
    begin
      if not CreatePipe(Read, Write, @SecAttr, 1024) then RaiseLastWin32Error;
      if not DuplicateHandle(GetCurrentProcess, Read, GetCurrentProcess,
        @Inherit, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS) then
        RaiseLastWin32Error;
    end;
  end;

  procedure ReadData(Pipe: THandle);
  var
    Available: Cardinal;
    Bytes: Cardinal;
    S: string;
  begin
    if PeekNamedPipe(Pipe, nil, 0, nil, @Available, nil) and
      (Available > 0) then
    begin
      SetLength(S, Available);
      if ReadFile(Pipe, PChar(S)^, Available, Bytes, nil) then
      begin
        SetLength(S, StrLen(PChar(S)));
        if Assigned(ReadProc) then
          ReadProc(S, Data);
      end;
    end;
  end;

  procedure ClosePipes(Pipes: array of TPipeHandles);
  var
    I: Integer;
  begin
    for I := Low(Pipes) to High(Pipes) do
    begin
      if Pipes[I].Read <> 0 then CloseHandle(Pipes[I].Read);
      if Pipes[I].Write <> 0 then CloseHandle(Pipes[I].Write);
    end;
  end;

var
  InputPipes, OutputPipes, ErrorPipes: TPipeHandles;
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfo;
  StartTime: Cardinal;
begin
  FillChar(InputPipes, SizeOf(TPipeHandles), #0);
  FillChar(OutputPipes, SizeOf(TPipeHandles), #0);
  FillChar(ErrorPipes, SizeOf(TPipeHandles), #0);
  try
    InitializePipes(InputPipes, @InputPipes.Read);
    InitializePipes(OutputPipes, @OutputPipes.Write);
    InitializePipes(ErrorPipes, @ErrorPipes.Write);
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    with StartupInfo do
    begin
      cb := SizeOf(TStartupInfo);
      wShowWindow :=  SW_HIDE;
      hStdInput := InputPipes.Read;
      hStdOutput := OutputPipes.Write;
      hStdError := ErrorPipes.Write;
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    end;
    Result := CreateProcess(nil, PChar(AppName), nil, nil, True,
      CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
      ProcessInfo);
    if Result then
    try
      StartTime := GetTickCount;
      while IsProcess(ProcessInfo.hProcess) do
      begin
        ReadData(OutputPipes.Read);
        ReadData(ErrorPipes.Read);
        if Assigned(WaitProc) then
          WaitProc(GetTickCount - StartTime, Data);
      end;
    finally
      CloseHandle(ProcessInfo.hProcess);
      CloseHandle(ProcessInfo.hThread);
    end;
  finally
    ClosePipes([InputPipes, OutputPipes, ErrorPipes]);
  end;
end;

function IsWindowsXPOrLater: Boolean;
var
  V: DWORD;
begin
  V := GetVersion;
  if LongRec(V).Lo < 5 then
    Result := False
  else if LongRec(V).Lo > 5 then
    Result := True
  else
    Result := LongRec(V).Hi > 0;
end;

function ScheduleTask(Days: TDays; Time: TDateTime; const Command: string; Interactive: Boolean = False): Integer;
var
	TempFile, Params, S: string;
	Process: THandle;
  I: Integer;
begin
	Result := 0;
  if not FileExists(Command) then Exit;
	if Days = [] then Exit;
	TempFile := GetTempFileName;
  Params := GetConsolePath + ' /c at.exe ' + FormatDateTime('hh:mm:ssAM/PM', Time);
	if Days = AllDays then
  	Days := [];
	if Days <> [] then
  	Params := Params + ' /every:' + GetEnumString(TypeInfo(TDay), Byte(Days), False);
  if Interactive then
  	Params := Params + ' /interactive';
	Params := Params + ' ' + GetShortFileName(Command) + ' > ' + TempFile;
  if SysUtils.FindCmdLineSwitch('logtask') then
		FileWriteString('task.bat', Params);
	Process := CreateProcessAndReturn(Params, SW_HIDE);
  if Process <> 0 then
  begin
  	WaitForSingleObject(Process, INFINITE);
    Sleep(500);
    S := FileReadString(TempFile);
    for I := Length(S) downto 1 do
    	if S[I] = ' ' then
      begin
      	Result := StrToIntDef(Trim(Copy(S, I, Length(S))), 0);
        Break;
      end;
    DeleteFile(TempFile);
	end;
end;

procedure DeleteTask(Task: Integer);
begin
  CreateProcessAndReturn('at.exe ' + IntToStr(Task) + ' /delete', SW_HIDE);
end;

procedure ScheduleTask(const Name, Command: string; Days: TDays; Time: TDateTime);
var
  S: string;
  D: TDay;
begin
  if Days = [] then Exit;
  S := '';
  for D := Low(D) to High(D) do
    if D in Days then
    begin
      if S <> '' then S := S + ',';
      S := S + ShortDayNames[Ord(D) + 1];
    end;
  S := 'schtasks.exe /create /ru system /tn ' + Name + ' /tr ' + Command +
    ' /sc weekly /d ' + S + ' /st ' + FormatDateTime('hh:mm:ss', Time);
  CreateProcessAndWait(S, SW_HIDE);
end;

procedure DeleteTask(const Name: string);
begin
  CreateProcessAndWait('schtasks.exe /delete /tn ' + Name + ' /f', SW_HIDE);
end;

procedure ScanProcessMemory(ScanProc: TScanProc);
var
  SystemInfo: TSystemInfo;
  Process: THandle;
  Memory: Cardinal;
  MemoryInformation: TMemoryBasicInformation;
begin
  GetSystemInfo(SystemInfo);
  Process := GetCurrentProcess;
  Memory := 0;
  while Memory < Cardinal(SystemInfo.lpMaximumApplicationAddress) do
  begin
    MemoryInformation.RegionSize := 0;
    VirtualQueryEx(Process, Pointer(Memory), MemoryInformation,
      SizeOf(TMemoryBasicInformation));
    with MemoryInformation do
    begin
      Memory := Cardinal(BaseAddress) + Cardinal(RegionSize);
      if (AllocationProtect and PAGE_READWRITE = PAGE_READWRITE) and
        (Type_9 = MEM_PRIVATE) and (State = MEM_COMMIT) then
        ScanProc(BaseAddress, RegionSize);
    end;
  end;
end;

procedure ShutdownWindows;
var
  ProcessHandle: Integer;
  TokenHandle: THandle;
  TokenPrivileges: TTokenPrivileges;
  Dummy: TTokenPrivileges;
  Buffer: DWORD;
begin
  with TokenPrivileges, Privileges[0] do
  begin
    PrivilegeCount := 1;
    LookupPrivilegeValue(nil, 'SeShutdownPrivilege', LUID);
    Attributes := SE_PRIVILEGE_ENABLED;
  end;
  ProcessHandle := GetCurrentProcess;
  OpenProcessToken(ProcessHandle, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, TokenHandle);
  AdjustTokenPrivileges(TokenHandle, False, TokenPrivileges, SizeOf(TokenPrivileges),
    Dummy, Buffer);
  ExitWindowsEx(EWX_SHUTDOWN or EWX_FORCE or EWX_REBOOT, 0);
end;

{ TLauncher }

const
  ShowStates: array [TShowState] of Integer = (SW_HIDE, SW_SHOW,
    SW_SHOWMINIMIZED, SW_SHOWMAXIMIZED);

procedure TLauncher.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TLauncher.SetInterval(const Value: Integer);
begin
  FInterval := Value;
end;

procedure TLauncher.SetParams(const Value: string);
begin
  FParams := Value;
end;

procedure TLauncher.Launch;

  procedure Succeeded(Value: Boolean);
  begin
    if Value then
      if Assigned(FOnLaunch) then
        FOnLaunch(Self)
      else
    else
      raise Exception.Create(SLauncherFileError);
  end;

const
  LaunchOps: array [TLaunchOperation] of PChar = ('open', 'print', 'explore');
var
  ShortFileName: array [0..MAX_PATH] of Char;
  FileStr: string;
begin
  Terminate;
  if Trim(FFileName) = '' then
    Exit;
  GetShortPathName(PChar(FFileName), @ShortFileName, SizeOf(ShortFileName));
  if Operation = loOpen then
    with FLaunchInfo  do
    begin
      FillChar(FLaunchInfo, SizeOf(TLaunchInfo), #0);
      FileStr := '';
      if UpperCase(ExtractFileExt(FFileName)) <> '.EXE' then
      begin
        SetLength(FileStr, MAX_PATH);
        FindExecutable(ShortFileName, nil, PChar(FileSTr));
        SetLength(FileStr, StrLen(PChar(FileStr)));
        FileStr := FileStr + ' ' + Trim(StrPas(ShortFileName) + ' ' + FParams);
      end
      else
        FileStr := Trim(StrPas(ShortFileName) + ' ' + FParams);
      StartupInfo.cb := SizeOf(TStartupInfo);
      StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
      StartupInfo.wShowWindow := ShowStates[FShowState];
      Succeeded(CreateProcess(nil, PChar(FileStr), nil, nil, False, CREATE_NEW_CONSOLE or
        NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo));
      FStartTick := GetTickCount;
      FRunning := True;
    end
  else
    Succeeded(ShellExecute(0, LaunchOps[FOperation], ShortFileName, PChar(FParams),
      nil, ShowStates[FShowState]) > 32);
end;

procedure TLauncher.Terminate;
begin
  if GetRunning then
    with FLaunchInfo.ProcessInfo do
    begin
      try
        if TerminateProcess(hProcess) then
          if Assigned(FOnTerminate) then
            FOnTerminate(Self)
          else
        else
          raise Exception.Create(SLauncherTerminateError);
      finally
        FRunning := False;
      end;
    end;
end;

procedure TLauncher.Wait;
var
  TimeOut: Cardinal;
  WaitObject: Cardinal;
begin
  if GetRunning then
  begin
    TimeOut := FInterval;
    if TimeOut = 0 then
      TimeOut := INFINITE;
    repeat
      WaitObject := WaitForSingleObject(FLaunchInfo.ProcessInfo.hProcess, TimeOut);
      if Assigned(FOnWait) then
        FOnWait(Self);
    until WaitObject = WAIT_OBJECT_0;
    if Assigned(FOnTerminate) then
      FOnTerminate(Self);
    FRunning := False;
  end;
end;

function TLauncher.GetRunning: Boolean;
begin
  if FRunning then
    FRunning := IsProcess(FLaunchInfo.ProcessInfo.hProcess);
  Result := FRunning;
end;

function TLauncher.GetElapsedTime: Cardinal;
begin
  if GetRunning then
    Result := GetTickCount - FStartTick
  else
    Result := 0;
end;

{ TRedirector }

const
  DUPLICATE_CLOSE_SOURCE = 1;
  DUPLICATE_SAME_ACCESS  = 2;

type
  TRedirectorThread = class(TThread)
  protected
    FRedirector: TRedirector;
    procedure Execute; override;
  public
    constructor Create(ARedirector: TRedirector);
  end;

procedure TRedirector.Error(Msg: string);
begin
  TerminateProcess(ProcessHandle);
  raise Exception.Create(Msg);
end;

procedure TRedirector.WinError(Msg: string);
begin
  Error(Msg + IntToStr(GetLastError));
end;

procedure TRedirector.CreatePipes;
var
  SecAttr: TSecurityAttributes;
begin
  SecAttr.nLength := SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor := nil;
  SecAttr.bInheritHandle := True;
  with FPipeInput do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, 1024)
      then WinError('Error on STDIN pipe creation: ');
    if not DuplicateHandle(GetCurrentProcess, hRead, GetCurrentProcess,
      @hRead, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS) then
      WinError('Error on STDIN pipe duplication: ');
  end;
  with FPipeOutput do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, 1024) then
      WinError('Error on STDOUT pipe creation: ');
    if not DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess,
      @hWrite, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS) then
      WinError('Error on STDOUT pipe duplication: ');
  end;
  with FPipeError do
  begin
    if not CreatePipe(hRead, hWrite, @SecAttr, 1024)
      then WinError('Error on STDERR pipe creation: ');
    if not DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess,
             @hWrite, 0, True, DUPLICATE_CLOSE_SOURCE or DUPLICATE_SAME_ACCESS)
      then WinError('Error on STDERR pipe duplication: ');
  end;
end;

procedure TRedirector.ClosePipes;
begin
  with FPipeInput do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
  with FPipeOutput do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
  with FPipeError do
  begin
    if hRead <> 0 then CloseHandle(hRead);
    if hWrite <> 0 then CloseHandle(hWrite);
    hRead := 0;
    hWrite := 0;
  end;
end;

function TRedirector.GetRunning: Boolean;
begin
  Result := ProcessHandle <> 0;
  if (Result) and (not IsProcess(ProcessHandle)) then
    Terminate;
end;

function TRedirector.GetExitCode: Integer;
begin
  if Running then
    Result := STILL_ACTIVE
  else
    Result := FExitCode;
end;

function TRedirector.GetProcessID: Integer;
begin
  Result := FProcessInfo.dwProcessID;
end;

function TRedirector.GetThreadID: Integer;
begin
  Result := FProcessInfo.dwThreadID;
end;

function TRedirector.GetProcessHandle: Integer;
begin
  Result := FProcessInfo.hProcess;
  if Result <> 0 then
    if not IsProcess(FProcessInfo.hProcess) then
    begin
      CloseHandle(FProcessInfo.hProcess);
      FillChar(FProcessInfo, SizeOf(TProcessInformation), #0);
    end;
end;

function TRedirector.GetThreadHandle: Integer;
begin
  Result := FProcessInfo.hThread;
end;

procedure TRedirector.SetExecutable(Value: string);
begin
  if (ANSICompareText(Value, Executable) = 0) or not Running then
    FExecutable := Value
  else if Running then
    Error('Cannot change Executable while process is active');
end;

procedure TRedirector.SetCommandLine(Value: string);
begin
  if (ANSICompareText(Value, Commandline) = 0) or (not Running) then
    FCommandline := Value
  else if Running then
    Error('Cannot change Commandline while process is active');
end;

function TRedirector.GetCommandLine: string;
begin
  Result := FExecutable;
  if Result = '' then
    Result := FCommandline
  else
    Result := FExecutable + ' ' + FCommandline;
end;

procedure TRedirector.SetDefaultErrorMode(Value: Boolean);
begin
  if (Value = DefaultErrorMode) or (not Running) then
    FDefaultErrorMode := Value
  else if Running then
    Error('Cannot change DefaultErrorMode while process is active');
end;

procedure TRedirector.SetStartSuspended(Value: Boolean);
begin
  if (Value = DefaultErrorMode) or not Running then FStartSuspended := Value
  else if Running then Error('Cannot change StartSuspended while process is active');
end;

procedure TRedirector.SetInitialPriority(Value: TPriorityClass);
begin
  if (Value = InitialPriority) or not Running then FInitialPriority := Value
  else if Running then Error('Cannot change InititalPriority while process is active');
end;

procedure TRedirector.SetDirectory(Value: string);
begin
  if (ANSICompareText(Value, Directory) = 0) or(not Running) then FDirectory := Value
  else if Running then Error('Cannot change Directory while process is active');
end;

procedure TRedirector.SetEnvironment(Value: Pointer);
begin
  if (Value = Environment) or not Running then FEnvironment := Value
  else if Running then Error('Cannot change Environment while process is active');
end;

procedure TRedirector.SetShowState(Value: TShowState);
begin
  if (Value = FShowState) or not Running then FShowState := Value
  else if Running then Error('Cannot change ShowWindow while process is active');
end;

procedure TRedirector.ReadStdOutput;
var
  BytesRead: DWORD;
  Buffer: Pointer;
begin
  GetMem(Buffer, FAvailable);
  try
    if not ReadFile(FPipeOutput.hRead, Buffer^, FAvailable, BytesRead, nil) then
    begin
      FThread.Terminate;
      WinError('Error reading STDOUT pipe: ');
    end;
    if Assigned(FOnData) then
      FOnData(Self, Buffer, BytesRead);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TRedirector.ReadStdError;
var
  BytesRead: DWORD;
  Buffer: Pointer;
begin
  GetMem(Buffer, FAvailable);
  try
    if not ReadFile(FPipeError.hRead, Buffer^, FAvailable, BytesRead, nil) then
    begin
      FThread.Terminate;
      WinError('Error reading STDERR pipe: ');
    end;
    if Assigned(FOnErrorData) then
      FOnErrorData(Self, Buffer, BytesRead);
  finally
    FreeMem(Buffer);
  end;
end;

procedure TRedirector.ProcessTerminated;
begin
  if FThread <> nil then
    FThread.Terminate;
  FThread := nil;
  if Assigned(FOnTerminated) then FOnTerminated(Self);
  ClosePipes;
  TerminateProcess(FProcessInfo.dwProcessId);
  CloseHandle(FProcessInfo.hProcess);
  CloseHandle(FProcessInfo.hThread);
  FillChar(FProcessInfo, SizeOf(FProcessInfo), #0);
end;

procedure TRedirector.Terminate;
begin
  ProcessTerminated;
end;

function TRedirector.Execute: THandle;
var
  StartupInfo: TStartupInfo;
  szExecutable: PChar;
  szCommandline: PChar;
  szDirectory: PChar;
begin
  Result := 0;
  if Running then
    Error('Process is already active');
  if Trim(CommandLine)='' then
    Error('No commandline to run');
  try
    CreatePipes;
    FillChar(StartupInfo, SizeOf(StartupInfo), 0);
    with StartupInfo do
    begin
      cb := SizeOf(StartupInfo);
      wShowWindow :=  ShowStates[FShowState];
      hStdInput := FPipeInput.hRead;
      hStdOutput := FPipeOutput.hWrite;
      hStdError := FPipeError.hWrite;
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    end;
    if Trim(Executable) = '' then
      szExecutable := nil
    else
      szExecutable := PChar(FExecutable);
    if Trim(Commandline) = '' then
      szCommandline := nil
    else
      szCommandline := PChar(FCommandline);
    if Trim(Directory) = '' then
      szDirectory := nil
    else
      szDirectory := PChar(FDirectory);
    if CreateProcess(szExecutable, szCommandline, nil, nil, True,
     (CREATE_DEFAULT_ERROR_MODE and Integer(FDefaultErrorMode))
      or(CREATE_SUSPENDED and Integer(FStartSuspended)), Environment,
      szDirectory, StartupInfo, FProcessInfo) then
    begin
      Result := FProcessInfo.hProcess;
      // WaitForSingleObject(FProcessInfo.hProcess, 1500);
      FThread := TRedirectorThread.Create(Self);
    end
      else WinError('Error creating process: ');
  except
    on Exception do
    begin
      ClosePipes;
      CloseHandle(FProcessInfo.hProcess);
      CloseHandle(FProcessInfo.hThread);
      FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);
      raise;
    end;
  end;
end;

procedure TRedirector.SendData(Buffer: Pointer; BufferSize: Integer);
var
  BytesWritten: DWORD;
begin
  if not Running then
    Error('Can''t send data to an inactive process');
  if not WriteFile(FPipeInput.hWrite, Buffer^, BufferSize, BytesWritten, nil) then
    WinError('Error writing to STDIN pipe: ');
end;

procedure TRedirector.SendText(S: string);
begin
  SendData(PChar(S), Length(S));
end;

destructor TRedirector.Destroy;
begin
  Terminate;
  inherited Destroy;
end;

constructor TRedirectorThread.Create(ARedirector: TRedirector);
begin
  FRedirector := ARedirector;
  inherited Create(False);
end;

procedure TRedirectorThread.Execute;
var
  Idle: Boolean;
begin
  FreeOnTerminate := True;
  while not Terminated do
  begin
    Idle := True;
    if PeekNamedPipe(FRedirector.FPipeOutput.hRead, nil, 0, nil,
      @FRedirector.FAvailable, nil) and (FRedirector.FAvailable>0) then
    begin
      Synchronize(FRedirector.ReadStdOutput);
      Idle := False;
    end;
    if PeekNamedPipe(FRedirector.FPipeError.hRead, nil, 0, nil,
      @FRedirector.FAvailable, nil) and (FRedirector.FAvailable>0) then
    begin
      Synchronize(FRedirector.ReadStdError);
      Idle := False;
    end;
    if Idle and (WaitForSingleObject(FRedirector.ProcessHandle,
      100) = WAIT_OBJECT_0) then
    begin
      {if not Terminated then
        Synchronize(FRedirector.ProcessTerminated);}
    end;
  end;
end;

{ TPerformanceTimer }

constructor TPerformanceTimer.Create;
begin
  if not QueryPerformanceFrequency(FResolution) then
    RaiseLastWin32Error;
  {$IFDEF D5_UP}
     FResolution := FResolution div 1000;
  {$ELSE}
     FResolution := FResolution / 1000;
  {$ENDIF}
end;

function TPerformanceTimer.GetElapsedTime: LongWord;
var
  EndTime: Int64;
begin
  if FTiming then
  begin
    if not QueryPerformanceCounter(EndTime) then
      raise EPerformanceError.CreateFmt(STimerError, ['query']);
  end
  else
    EndTime := FStop;
  Result := (EndTime - FStart) div FResolution;
end;

procedure TPerformanceTimer.Start;
begin
  if not QueryPerformanceCounter(FStart) then
    raise EPerformanceError.CreateFmt(STimerError, ['start']);
  FTiming := True;
end;

procedure TPerformanceTimer.Stop;
begin
  if not QueryPerformanceCounter(FStop) then
    raise EPerformanceError.CreateFmt(STimerError, ['stop']);
  FTiming := False;
end;

{ TCommand class }

constructor TCommand.Create(Kind: Integer);
begin
	inherited Create;
	Self.Kind := Kind;
end;

constructor TCommand.Create(Kind: Integer; Value: Variant);
begin
	inherited Create;
	Self.Kind := Kind;
	Self.Value := Value;
end;

{ TCommandThread class }

constructor TCommandThread.Create(CommandEvent: TCommandEvent; Wnd: HWND = 0);
begin
	FCommands := TList.Create;
	FMutex := CreateMutex(nil, False, nil);
	FWait := CreateMutex(nil, False, nil);
	FCommandEvent := CommandEvent;
  FWnd := Wnd;
	inherited Create(False);
end;

destructor TCommandThread.Destroy;
begin
  Terminate;
	inherited Destroy;
  FCommands.Free;
	CloseHandle(FMutex);
end;

procedure TCommandThread.Wait;
begin
  Terminate;
	WaitForSingleObject(FWait, INFINITE);
	CloseHandle(FWait);
end;

function TCommandThread.Pop: TCommand;
begin
	Result := nil;
	WaitForSingleObject(FMutex, INFINITE);
  if FCommands.Count > 0 then
  begin
    Result := TCommand(FCommands[0]);
    FCommands.Delete(0);
  end;
  ReleaseMutex(FMutex);
end;

procedure TCommandThread.Push(Kind: Integer);
begin
	WaitForSingleObject(FMutex, INFINITE);
	Push(Kind, 0);
  ReleaseMutex(FMutex);
end;

procedure TCommandThread.Push(Kind: Integer; Value: Variant);
begin
	WaitForSingleObject(FMutex, INFINITE);
  FCommands.Add(TCommand.Create(Kind, Value));
  ReleaseMutex(FMutex);
end;

function TCommandThread.SendRequest(Msg: Cardinal): Pointer;
begin
  Result := nil;
  if (not Terminated) and (FWnd <> 0) and IsWindow(FWnd) then
    Result := Pointer(SendMessage(FWnd, Msg, 0, 0));
end;

procedure TCommandThread.Execute;
var
	Command: TCommand;
begin
	WaitForSingleObject(FWait, INFINITE);
  FreeOnTerminate := True;
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  FBusy := FCommands.Count > 0;
	while FBusy or (not Terminated) do
	begin
		Command := Pop;
    if Command <> nil then
    try
      FCommandEvent(Self, Command);
		finally
      Command.Free;
		end;
		Sleep(100);
    FBusy := FCommands.Count > 0;
	end;
  CoUninitialize;
  ReleaseMutex(FWait);
end;

{ TGlobalData }

function GlobalDataExists(const Name: string): Boolean;
var
  Mutex: THandle;
begin
  Mutex := CreateMutex(nil, False, PChar('Mutex' + Name));
  Result := GetLastError = ERROR_ALREADY_EXISTS;
  CloseHandle(Mutex);
end;

constructor TGlobalData.Create(const Name: string; Size: Integer);
begin
  FName := Name;
  FSize := Size;
  FMap := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, FSize,
    PChar('Map' + Name));
  if FMap <> 0 then
  begin
    FCreator := GetLastError <> ERROR_ALREADY_EXISTS;
    FMutex := CreateMutex(nil, False, PChar('Mutex' + Name));
    if FMutex = 0 then
      raise Exception.Create(SMutexCreateError)
  end
  else
    raise Exception.Create(SMapppingCreateError);
end;

destructor TGlobalData.Destroy;
begin
  UnLock;
  if FMap <> 0 then
    CloseHandle(FMap);
  if FMutex <> 0 then
    CloseHandle(FMutex);
end;

function TGlobalData.GetData: Pointer;
begin
  if FLocked then
    Result := FData
  else
    raise Exception.Create(SNotLocked);
end;

procedure TGlobalData.Lock;
begin
  if not FLocked then
  begin
    WaitForSingleObject(FMutex, INFINITE);
    FData := MapViewOfFile(FMap, FILE_MAP_ALL_ACCESS, 0, 0, FSize);
    FLocked  := True;
  end;
end;

procedure TGlobalData.UnLock;
begin
  if FLocked then
  begin
    UnmapViewOfFile(FData);
    ReleaseMutex(FMutex);
    FLocked  := False;
  end;
end;

{ TMemoryMappedFile }

constructor TMemoryMappedFile.Create(const AFileName: string);
begin
  FFileName := AFileName;
  FFile := FileOpen(FFileName, fmOpenRead);
  if FFile <> 0 then
  begin
    FMap := CreateFileMapping(FFile, nil, PAGE_READONLY, 0, 0, nil);
    if FMap <> 0 then
    begin
      FViewStart := MapViewOfFile(FMap, FILE_MAP_READ, 0, 0, 0);
      if Assigned(FViewStart) then
      begin
        FViewEnd := FViewStart;
        Inc(FViewEnd, Windows.GetFileSize(FFile, nil) + 1);
      end
      else
        raise EFileMappingError.Create(SViewMapError)
    end
    else
      raise EFileMappingError.Create(SMapppingCreateError)
  end
  else
    raise EFileMappingError.Create(SFileOpenError);
end;

destructor TMemoryMappedFile.Destroy;
begin
 if Assigned(FViewStart) then
   UnmapViewOfFile(FViewStart);
 if FMap <> 0 then
   CloseHandle(FMap);
 if FFile <> 0 then
   CloseHandle(FFile);
end;

{ TBasePipe }

const
  PIPE_BUFFER  = 1024*4;
  PIPE_TIMEOUT = 5000;

constructor TBasePipe.Create(const AName: string; AMode: TPipeMode);
begin
  inherited Create;
  FName := AName;
  FMode := AMode;
end;

function TBasePipe.Read(var Buffer; Count: LongWord): LongWord;
begin
  if FConnected then
    if FMode = pmRead then
      ReadFile(FHandle, Buffer, Count, Result, nil)
    else
      raise Exception.Create(SInvalidMode)
  else
    raise Exception.Create(SNotConnected);
end;

function TBasePipe.Write(const Buffer; Count: LongWord): LongWord;
begin
  if FConnected then
    if FMode = pmWrite then
      WriteFile(FHandle, Buffer, Count, Result, nil)
    else
      raise Exception.Create(SInvalidMode)
  else
    raise Exception.Create(SNotConnected);
end;

constructor TServerPipe.Create(const AName: string; AMode: TPipeMode);
const
  Modes: array [TPipeMode] of LongWord = (PIPE_ACCESS_INBOUND, PIPE_ACCESS_OUTBOUND);
begin
  inherited Create(AName, AMode);
  FHandle := CreateNamedPipe(PChar('\\.\pipe\' + FName), Modes[FMode], 0,
    PIPE_UNLIMITED_INSTANCES, PIPE_BUFFER, PIPE_BUFFER, PIPE_TIMEOUT, nil);
  if FHandle = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;
end;

destructor TServerPipe.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    Disconnect;
    CloseHandle(FHandle);
  end;
  inherited Destroy;
end;

procedure TServerPipe.Connect;
begin
  if not FConnected then
  begin
    FConnected := ConnectNamedPipe(FHandle, nil);
    if not FConnected then
    begin
      FConnected := GetLastError = ERROR_PIPE_CONNECTED;
      if not FConnected then
        RaiseLastWin32Error;
    end;
  end;
end;

procedure TServerPipe.Disconnect;
begin
  if FConnected then
  begin
    FlushFileBuffers(FHandle);
    DisconnectNamedPipe(FHandle);
    FConnected := False;
  end;
end;

{ TClientPipe }

destructor TClientPipe.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TClientPipe.Connect;
const
  Modes: array [TPipeMode] of LongWord = (GENERIC_READ, GENERIC_WRITE);
var
  ClientPipeName: string;
begin
    if RemoteMachine <> '' then
      ClientPipeName := '\\' + FRemoteMachine + '\pipe\' + FName
    else
      ClientPipeName := '\\.\pipe\' + FName;
    if WaitNamedPipe(PChar(ClientPipeName), PIPE_TIMEOUT) then
    begin
      FHandle := CreateFile(PChar(ClientPipeName), Modes[FMode], 0, nil,
        OPEN_EXISTING, 0, 0);
      if Handle <> INVALID_HANDLE_VALUE then
        FConnected := True
      else
      begin
        FHandle := 0;
        RaiseLastWin32Error;
      end;
    end;
end;

procedure TClientPipe.Disconnect;
begin
  if FConnected then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
    FConnected := False;
  end;
end;

{ TPipeThread }

constructor TPipeThread.Create(const Params: TPipeThreadParams; PipeProc: TPipeProc);
begin
  FParams := Params;
  FParams.Thread := Self;
  FPipeProc := PipeProc;
  inherited Create(False);
end;

destructor TPipeThread.Destroy;
begin
  with FParams do
  begin
    Pipe.Free;
    if Assigned(Instance) then
      PInteger(Instance)^ := 0;
  end;
  inherited Destroy;
end;

procedure TPipeThread.Execute;
begin
  FreeOnTerminate := True;
  while not Terminated do
    FPipeProc(FParams);
end;

{ TMonitorThread }

const
  MaxMonitorBuffer = 4096;
  CM_DIRECTORY_EVENT = WM_USER + $A;

type
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..MAX_PATH] Of WChar;
  end;
  PFileNotifyInformation = ^TFileNotifyInformation;

  TMonitorThread = class(TThread)
  private
    FDirectory: string;
    FCompletionPort: THandle;
    FDirHandle: THandle;
    FBuffer: Pointer;
    FBufferLength: DWORD;
    FOverlapped: TOverlapped;
    FOptions: TMonitorOptions;
    FNotifyMask: DWORD;
    FParentWindow: THandle;
    FWatchSubFolders: Boolean;
    procedure MakeOutput(FileInfo: PFileNotifyInformation);
    function GetNotifyMask: DWORD;
  protected
    procedure Execute; override;
  public
    constructor Create(const Directory: string; Options: TMonitorOptions;
      ParentWindow: THandle; WatchSubFolders: Boolean);
    destructor Destroy; override;
    property CompletionPort: THandle read FCompletionPort;
  end;

constructor TMonitorThread.Create(const Directory: string; Options: TMonitorOptions;
  ParentWindow: THandle; WatchSubFolders: Boolean);
const
  FILE_LIST_DIRECTORY = 1;
begin
  FWatchSubFolders := WatchSubFolders;
  FDirectory := Directory;
  FOptions := Options;
  FParentWindow := ParentWindow;
  FNotifyMask := GetNotifyMask;
  FDirHandle := CreateFile(PChar(FDirectory),
    FILE_LIST_DIRECTORY, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
    nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS Or FILE_FLAG_OVERLAPPED, 0);
  GetMem(FBuffer,MaxMonitorBuffer);
  FBufferLength := MaxMonitorBuffer;
  Inherited Create(False);
end;

destructor TMonitorThread.Destroy;
begin
  CloseHandle(FDirHandle);
  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TMonitorThread.Execute;
var
  O: POverlapped;
  I: DWORD;
begin
  O := @FOverlapped;
  FCompletionPort := CreateIoCompletionPort(FDirHandle, FCompletionPort, DWORD(FBuffer), 0);
  ZeroMemory(FBuffer, MaxMonitorBuffer);
  if ReadDirectoryChangesW(FDirHandle, FBuffer, MaxMonitorBuffer, FWatchSubFolders,
    FNotifyMask, @FBufferLength, @FOverlapped, nil) then
  repeat
    GetQueuedCompletionStatus(THandle(FCompletionPort), I, DWORD(FBuffer),
      O, INFINITE);
    if FBuffer <> nil then
    begin
      MakeOutput(PFileNotifyInformation(FBuffer));
      ZeroMemory(FBuffer, MaxMonitorBuffer);
      ReadDirectoryChangesW(FDirHandle, FBuffer, MaxMonitorBuffer, FWatchSubFolders,
        FNotifyMask, @FBufferLength, @FOverlapped, nil);
    end;
  until Terminated or (FBuffer = nil);
  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
end;

function TMonitorThread.GetNotifyMask: DWORD;
const
  NotifyFilter: array[TMonitorOption] of DWORD = (
    FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME,
    FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
    FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_LAST_ACCESS,
    FILE_NOTIFY_CHANGE_CREATION, FILE_NOTIFY_CHANGE_SECURITY);
var
  I: TMonitorOption;
begin
  Result := 0;
  for I := Low(I) to High(I) do
    if I in FOptions then
      Result := Result or NotifyFilter[I];
end;

procedure TMonitorThread.MakeOutput(FileInfo: PFileNotifyInformation);
var
  Offset: DWORD;
  S: string;
  P: PChar;
  I: Cardinal;
begin
  // Sleep(1000);
  repeat
    Offset := FileInfo.NextEntryOffset;
    S := FDirectory;
    I := 0;
    while (I < FileInfo.FileNameLength) and (FileInfo.FileName[i] < Chr($FF)) do
    begin
      S := S + FileInfo.FileName[I];
      Inc(I);
    end;
    P := StrAlloc(Length(S));
    StrPCopy(P, S);
    PostMessage(FParentWindow, CM_DIRECTORY_EVENT, FileInfo.Action, Integer(P));
    FileInfo := PFileNotifyInformation(Cardinal(FileInfo) + Offset);
  until Offset = 0;
end;

{ TDirectoryMonitor }

constructor TDirectoryMonitor.Create;
begin
  inherited Create;
  FWindowHandle := AllocateHWnd(InternalWinProc);
  FWatchSubFolders := True;
end;

destructor TDirectoryMonitor.Destroy;
begin
  Stop;
  DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

procedure TDirectoryMonitor.InternalWinProc(var Msg: TMessage);
const
  Actions: array[FILE_ACTION_ADDED..FILE_ACTION_RENAMED_NEW_NAME] Of TFileAction =
    (faAdded, faRemoved, faModified, faRenamedOldName, faRenamedNewName);
begin
  if Msg.Msg = CM_DIRECTORY_EVENT then
  begin
    if Assigned(FOnFileChange) then
      FOnFileChange(Self, Actions[Msg.wParam], string(Pchar(Msg.LParam)));
    StrDispose(PChar(Msg.LParam));
  end;
end;

procedure TDirectoryMonitor.SetDirectory(const Value: string);
begin
  Stop;
  FDirectory := IncludeTrailingPathDelimiter(Trim(Value));
end;

procedure TDirectoryMonitor.Start;
begin
  Stop;
  if DirectoryExists(FDirectory) then
    FThread := TMonitorThread.Create(FDirectory,FMonitorOptions,FWindowHandle, FWatchSubFolders);
end;

procedure TDirectoryMonitor.Stop;
begin
  if FThread = nil then Exit;
  PostQueuedCompletionStatus(TMonitorThread(FThread).CompletionPort, 0, 0, nil);
  FThread.Free;
  FThread := nil;
end;

procedure GetProcessModules(Strings: TStrings);
var
  ModuleList, Module: LPDWORD;
  Count: Cardinal;
  I: Integer;
begin
  EnumProcessModules(GetCurrentProcess, nil, 0, Count);
  GetMem(ModuleList, Count);
  Strings.BeginUpdate;
  try
    Module := ModuleList;
    EnumProcessModules(GetCurrentProcess, ModuleList, Count, Count);
    for I := 1 to Count div SizeOf(DWORD) do
    begin
      Strings.Add(GetModuleName(Module^));
      Inc(Module);
    end;
  finally
    Strings.EndUpdate;
    FreeMem(ModuleList);
  end;
end;

{function ProcessFileName(PID: DWORD): string;
  var
    Handle: THandle;
  begin
    Result := '';
    Handle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
    if Handle <> 0 then
      try
        SetLength(Result, MAX_PATH);
        if FullPath then
        begin
          if GetModuleFileNameEx(Handle, 0, PChar(Result), MAX_PATH) > 0 then
            SetLength(Result, StrLen(PChar(Result)))
          else
            Result := '';
        end
        else
        begin
          if GetModuleBaseNameA(Handle, 0, PChar(Result), MAX_PATH) > 0 then
            SetLength(Result, StrLen(PChar(Result)))
          else
            Result := '';
        end;
      finally
        CloseHandle(Handle);
      end;
  end;
 }
function ProcessModulesFileName(PID, MID: DWORD): string;
  begin
        SetLength(Result, MAX_PATH);
          if GetModuleFileNameEx(PID, MID, PChar(Result), MAX_PATH) > 0 then
            SetLength(Result, StrLen(PChar(Result)))
          else
            Result := '';
  end;

procedure GetMods(PID: THandle; Strings: TStrings);
var
  ModuleList, Module: LPDWORD;
  Count: Cardinal;
  I: Integer;
begin
  EnumProcessModules(PID, nil, 0, Count);
  GetMem(ModuleList, Count);
  try
    Module := ModuleList;
    if EnumProcessModules(PID, ModuleList, Count, Count) then
    for I := 1 to Count div SizeOf(DWORD) do
    begin
      Strings.Add('  - ' +  ProcessModulesFileName(PID, Module^));
      Inc(Module);
    end;
  finally
    FreeMem(ModuleList);
  end;
end;

const
  RsSystemIdleProcess = 'System Idle Process';
  RsSystemProcess = 'System Process';

function IsWinXP: Boolean;
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion = 5) and (Win32MinorVersion = 1);
end;

function IsWin2k: Boolean;
begin
  Result := (Win32MajorVersion >= 5) and
    (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

function IsWinNT4: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  Result := Result and (Win32MajorVersion = 4);
end;

function IsWin3X: Boolean;
begin
  Result := Win32Platform = VER_PLATFORM_WIN32_NT;
  Result := Result and (Win32MajorVersion = 3) and
    ((Win32MinorVersion = 1) or (Win32MinorVersion = 5) or
    (Win32MinorVersion = 51));
end;

procedure EnumAllProcess(Strings: TStrings);

  var
    PIDs: array [0..1024] of DWORD;
    Needed: DWORD;
    FileName: string;

  I: Integer;

  function ProcessFileName(PID: THandle): string;
  var
  Path: array[0..MAX_PATH] of Char;
  begin
          if GetModuleFileNameEx(PID, 0, Path, MAX_PATH) > 0 then
             Result := Path
          else
            Result := 'unknown process';
  end;

var
  PID: THandle;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
  if EnumProcesses(@PIDs, SizeOf(PIDs), Needed) then
  begin
    for I := 0 to (Needed div SizeOf(DWORD)) - 1 do
    begin
      PID := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PIDs[I]); //
        case PIDs[I] of
          0:
            FileName := RsSystemIdleProcess;
          2:
            if IsWinNT4 then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PID);
            8:
            if IsWin2k or IsWinXP then
              FileName := RsSystemProcess
            else
              FileName := ProcessFileName(PID);
            else
              FileName := ProcessFileName(PID);
          end;
      Strings.Add(FileName);

      GetMods(PID, Strings);
      CloseHandle(PID);
    end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

{function SystemQuery(const Statement: string): OleVariant;
var
 Locator, Service, Obj: OleVariant;
 Enum: IEnumVariant;
 I: Cardinal;
begin
  Locator := CreateOleObject('WbemScripting.SWbemLocator');
  Service := Locator.ConnectServer('.', 'root\cimv2');
  Obj := Service.ExecQuery(Statement, 'WQL');
  Enum := IUnknown(Obj._NewEnum) as IEnumVariant;
  if (Enum.Next(1, Obj, I) = S_OK) or (I <> 1) then
    Result := Obj
  else
    Result := Unassigned;
end;

function GetWindowsDriveLetter: string;
begin
  SetLength(Result, MAX_PATH);
  GetWindowsDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, 2);
end;

function GetSystemSerial: string;
var
  S: string;
  I: Integer;
begin
  Result := '';
  S := SystemQuery('select * from Win32_BIOS').SerialNumber;
  if Length(S) > 8 then
    SetLength(S, 8);
  while Length(S) < 8 do
    S := '0' + S;
  Result := S;
  S := SystemQuery('select * from Win32_LogicalDisk where name = ''' +
    GetWindowsDriveLetter + '''').VolumeSerialNumber;
  if Length(S) > 8 then
    SetLength(S, 8);
  while Length(S) < 8 do
    S := '0' + S;
  S := Result + S;
  Result := '';
  for I := 1 to Length(S) do
  begin
    Result := Result + S[I];
    if (I < Length(S)) and(I mod 4 = 0) then
      Result := Result + '-';
  end;
end;      }

end.
