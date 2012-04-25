
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit SysTools;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes;

{ TUtilityWindow class }

type
	TCreateInfo = record
    WndClass: TWndClass;
  	X, Y, W, H: Integer;
    Style: Cardinal;
    ExStyle: Cardinal;
    Parent: HWND;
  end;

  TUtilityWindow = class(TObject)
  private
    FOwner: TObject;
    FHandle: HWND;
	protected
  	procedure CreateInfo(var Info: TCreateInfo); virtual;
    property Owner: TObject read FOwner;
  public
    constructor Create(AOwner: TObject; Parent: HWND = 0; Style: Cardinal = 0; ExStyle: Cardinal = 0);
    destructor Destroy; override;
    property Handle: HWND read FHandle;
  end;

{ TSyncObject class }

  TSyncProc = procedure(Params: Pointer);

  TWMSync = packed record
    Msg: Cardinal;
    Proc: TSyncProc;
    Params: Pointer;
    Result: Longint;
  end;

  TSyncObject = class(TObject)
  private
  	FUtilityWindow: TUtilityWindow;
    procedure WMSync(var Msg: TWMSync); message WM_USER;
  public
    constructor Create;
    destructor Destroy; override;
  	procedure Sync(Proc: TSyncProc; Params: Pointer);
  end;

{ Hooks routines }

type
  TKeyboardHook = procedure(Key: Word; State: Cardinal; var Remove: Boolean) of object;
  TMouseHook = procedure(Msg: Cardinal; const HookStruct: TMouseHookStruct;
    var Remove: Boolean) of object;
  TMessageHook = procedure(const WinProcStruct: TCWPStruct) of object;

procedure HookKeyboard(Hook: TKeyboardHook);
procedure UnhookKeyboard(Hook: TKeyboardHook);
procedure HookMouse(Hook: TMouseHook);
procedure UnhookMouse(Hook: TMouseHook);
procedure HookMessage(Hook: TMessageHook);
procedure UnhookMessage(Hook: TMessageHook);

implementation

{ TUtilityWindow }

threadvar
  CreationWindow: TUtilityWindow;

function UtilityProc(Wnd: HWND; uMsg: Cardinal; wParam: LongInt; lParam: LongInt): Integer; stdcall;
var
  UtilityWindow: TUtilityWindow;
  Msg: TMessage;
begin
  if CreationWindow <> nil then
  begin
    UtilityWindow := CreationWindow;
    UtilityWindow.FHandle := Wnd;
    CreationWindow := nil;
    SetWindowLong(Wnd, GWL_USERDATA, Integer(UtilityWindow));
  end
  else
    UtilityWindow := TUtilityWindow(GetWindowLong(Wnd, GWL_USERDATA));
  Result := -1;
  if UtilityWindow <> nil then
  try
    Msg.Msg := uMsg;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    Msg.Result := -1;
    if UtilityWindow.FOwner <> nil then
      UtilityWindow.FOwner.Dispatch(Msg);
    Result := Msg.Result;
    if Msg.Msg = WM_DESTROY then
      UtilityWindow.FHandle := 0;
  except
    on E: Exception do
      MessageBox(0, PChar(E.ClassName + ': ' + E.Message), 'Error',
        MB_ICONERROR or MB_OK or MB_TASKMODAL);
  end;
  if Result = -1 then
    Result := DefWindowProc(Wnd, uMsg, wParam, lParam);
end;

procedure TUtilityWindow.CreateInfo(var Info: TCreateInfo);
begin
end;

constructor TUtilityWindow.Create(AOwner: TObject; Parent: HWND = 0; Style: Cardinal = 0; ExStyle: Cardinal = 0);
var
  WindowClass: string;
  WindowName: string;
  Info: TCreateInfo;
begin
  inherited Create;
  FOwner := AOwner;
  WindowClass := ClassName + IntToStr(HInstance);
  if FOwner <> nil then
    WindowName := FOwner.ClassName
  else
    WindowName := ClassName;
	FillChar(Info, SizeOf(Info), #0);
  Info.Parent := Parent;
  Info.Style := Style;
  Info.ExStyle := ExStyle;
  CreateInfo(Info);
  if not GetClassInfo(SysInit.HInstance, PChar(WindowClass), Info.WndClass) then
  begin
    with Info.WndClass do
    begin
      lpfnWndProc := @UtilityProc;
      lpszClassName := PChar(WindowClass);
      hInstance := SysInit.HInstance;
      Windows.RegisterClass(Info.WndClass);
    end;
  end;
  CreationWindow := Self;
  try
  	with Info do
	    CreateWindowEx(ExStyle, PChar(WindowClass), PChar(WindowName), Style,
      	X, Y, W, H, Parent, 0, 0, nil);
  except
    CreationWindow := nil;
  end;
  if FHandle = 0 then
    RaiseLastWin32Error;
end;

destructor TUtilityWindow.Destroy;
begin
  if FHandle <> 0 then
  begin
		SetWindowLong(FHandle, GWL_USERDATA, 0);
    DestroyWindow(FHandle);
	end;
end;

{ TSyncObject }

constructor TSyncObject.Create;
begin
	inherited Create;
	FUtilityWindow := TUtilityWindow.Create(Self);
end;

destructor TSyncObject.Destroy;
begin
	FUtilityWindow.Free;
	inherited Destroy;
end;

procedure TSyncObject.Sync(Proc: TSyncProc; Params: Pointer);
begin
	SendMessage(FUtilityWindow.Handle, WM_USER, Longint(@Proc), Longint(Params));
end;

procedure TSyncObject.WMSync(var Msg: TWMSync);
begin
	Msg.Proc(Msg.Params);
end;

{ Hook support routines }

type
  PMethod = ^TMethod;
  THookList = record
    Hook: HHOOK;
    Callbacks: TList;
  end;

procedure SetHook(var HookList: THookList; Kind: Integer; HookProc: TFarProc;
  const Method: TMethod);
var
  DynamicMethod: PMethod;
begin
  with HookList do
    if Hook = 0 then
    begin
      Hook := SetWindowsHookEx(Kind, HookProc, 0, GetCurrentThreadId);
      Callbacks := TList.Create;
    end;
  New(DynamicMethod);
  DynamicMethod^ := Method;
  HookList.Callbacks.Add(DynamicMethod);
end;

procedure ReleaseHook(var HookList: THookList; const Method: TMethod);
var
  DyanmicMethod: PMethod;
  I: Integer;
begin
  with HookList do
    if Hook <> 0 then
    begin
      for I := 0 to Callbacks.Count do
      begin
        DyanmicMethod := Callbacks[I];
        if (DyanmicMethod.Code = Method.Code) and
          (DyanmicMethod.Data = Method.Data) then
        begin
          Dispose(DyanmicMethod);
          Callbacks.Delete(I);
          Break;
        end;
      end;
      if Callbacks.Count = 0 then
      begin
        UnhookWindowsHookEx(Hook);
        Hook := 0;
        Callbacks.Free;
        Callbacks := nil;
      end;
    end
end;

var
  InternalKeyboardHooks: THookList;

function KeyboardHook(Code: Integer; wParam: LongInt; lParam: LongInt): LongInt; stdcall;
var
  Remove: Boolean;
  Method: TMethod;
  Callback: TKeyboardHook absolute Method;
  I: Integer;
begin
  with InternalKeyboardHooks do
    if Code < 0 then
      Result := CallNextHookEx(Hook, Code, wParam, lParam)
    else
    begin
      Remove := False;
      for I := 0 to Callbacks.Count - 1 do
      begin
        Method := PMethod(Callbacks[I])^;
        Callback(wParam, lParam, Remove);
      end;
      if Remove then Result := 1 else Result := 0;
    end;
end;

procedure HookKeyboard(Hook: TKeyboardHook);
var
  Method: TMethod absolute Hook;
begin
  SetHook(InternalKeyboardHooks, WH_KEYBOARD, @KeyboardHook, Method);
end;

procedure UnhookKeyboard(Hook: TKeyboardHook);
var
  Method: TMethod absolute Hook;
begin
  ReleaseHook(InternalKeyboardHooks, Method);
end;

var
  InternalMouseHooks: THookList;

function MouseHook(Code: Integer; Msg: Cardinal;
  HookStruct: PMouseHookStruct): Integer; stdcall;
var
  Remove: Boolean;
  Method: TMethod;
  Callback: TMouseHook absolute Method;
  I: Integer;
begin
  with InternalMouseHooks do
    if Code < 0 then
      Result := CallNextHookEx(Hook, Code, Msg, Integer(HookStruct))
    else
    begin
      Remove := False;
      for I := 0 to Callbacks.Count - 1 do
      begin
        Method := PMethod(Callbacks[I])^;
        Callback(Msg, HookStruct^, Remove);
      end;
      if Remove then Result := 1 else Result := 0;
    end;
end;

procedure HookMouse(Hook: TMouseHook);
var
  MethodParam: TMethod absolute Hook;
  Method: PMethod;
begin
  with InternalMouseHooks do
    if Hook = 0 then
    begin
      Hook := SetWindowsHookEx(WH_MOUSE, @MouseHook, 0, GetCurrentThreadId);
      Callbacks := TList.Create;
    end;
  New(Method);
  Method^ := MethodParam;
  InternalMouseHooks.Callbacks.Add(Method);
end;

procedure UnhookMouse(Hook: TMouseHook);
var
  MethodParam: TMethod absolute Hook;
  Method: PMethod;
  I: Integer;
begin
  with InternalMouseHooks do
    if Hook <> 0 then
    begin
      for I := 0 to Callbacks.Count do
      begin
        Method := Callbacks[I];
        if (Method.Code = MethodParam.Code) and
          (Method.Data = MethodParam.Data) then
        begin
          Dispose(Method);
          Callbacks.Delete(I);
          Break;
        end;
      end;
      if Callbacks.Count = 0 then
      begin
        UnhookWindowsHookEx(Hook);
        Hook := 0;
        Callbacks.Free;
        Callbacks := nil;
      end;
    end;
end;

var
  InternalMessageHooks: THookList;

function MessageHook(Code: Integer; CurrentProcess: Cardinal;
  HookStruct: PCWPStruct): Integer; stdcall;
var
  Method: TMethod;
  Callback: TMessageHook absolute Method;
  I: Integer;
begin
  with InternalMessageHooks do
    if Code < 0 then
      Result := CallNextHookEx(Hook, Code, CurrentProcess, Integer(HookStruct))
    else
    begin
			for I := 0 to Callbacks.Count - 1 do
      begin
    		Method := PMethod(Callbacks[I])^;
      	Callback(HookStruct^);
      end;
      Result := 0;
    end;
end;

procedure HookMessage(Hook: TMessageHook);
var
  MethodParam: TMethod absolute Hook;
  Method: PMethod;
begin
  with InternalMessageHooks do
    if Hook = 0 then
    begin
      Hook := SetWindowsHookEx(WH_CALLWNDPROC, @MessageHook, 0, GetCurrentThreadId);
      Callbacks := TList.Create;
    end;
  New(Method);
  Method^ := MethodParam;
  InternalMessageHooks.Callbacks.Add(Method);
end;

procedure UnhookMessage(Hook: TMessageHook);
var
  MethodParam: TMethod absolute Hook;
  Method: PMethod;
  I: Integer;
begin
  with InternalMessageHooks do
    if Hook <> 0 then
    begin
      for I := 0 to Callbacks.Count do
      begin
        Method := Callbacks[I];
        if (Method.Code = MethodParam.Code) and
          (Method.Data = MethodParam.Data) then
        begin
          Dispose(Method);
          Callbacks.Delete(I);
          Break;
        end;
      end;
      if Callbacks.Count = 0 then
      begin
        UnhookWindowsHookEx(Hook);
        Hook := 0;
        Callbacks.Free;
        Callbacks := nil;
      end;
    end;
end;

procedure ReleaseAllHooks;

  procedure ReleaseHooks(var Hooks: THookList);
  var
    I: Integer;
  begin
	  with Hooks do
  	  if Hook <> 0 then
    	begin
	      UnhookWindowsHookEx(Hook);
  	    Hook := 0;
    	  for I := 0 to Callbacks.Count - 1 do
        Dispose(Callbacks[I]);
      	Callbacks.Free;
	      Callbacks := nil;
  	  end;
  end;

begin
  ReleaseHooks(InternalKeyboardHooks);
  ReleaseHooks(InternalMouseHooks);
  ReleaseHooks(InternalMessageHooks);
end;

initialization
  InternalMouseHooks.Hook := 0;
finalization
  ReleaseAllHooks;
end.
