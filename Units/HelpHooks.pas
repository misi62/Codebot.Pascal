
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2008                   *)
(*                                                      *)
(********************************************************)

unit HelpHooks;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, WinTools, SysTools;

type
  THelpHookEvent = procedure(Sender: TObject; HelpControl: TControl) of object;

  THelpHook = class(TObject)
  private
    FActive: Boolean;
    FOnHover: THelpHookEvent;
    FOnFocus: THelpHookEvent;
    FFocusControl: TControl;
    FHoverControl: TControl;
    procedure MessageHook(const WinProcStruct: TCWPStruct);
    procedure MouseHook(Msg: Cardinal; const HookStruct: TMouseHookStruct;
      var Remove: Boolean);
  protected
    procedure Focus(HelpControl: TControl);
    procedure Hover(HelpControl: TControl);
  public
    constructor Create;
    destructor Destroy; override;
    property Active: Boolean read FActive write FActive;
    property OnFocus: THelpHookEvent read FOnFocus write FOnFocus;
    property OnHover: THelpHookEvent read FOnHover write FOnHover;
    property FocusControl: TControl read FFocusControl write FFocusControl;
    property HoverControl: TControl read FHoverControl write FHoverControl;
  end;

function HelpHook: THelpHook;

implementation

var
  InternalHelpHook: TObject;

constructor THelpHook.Create;
begin
  inherited Create;
  HookMessage(MessageHook);
  HookMouse(MouseHook);
  InternalHelpHook := Self;
end;

destructor THelpHook.Destroy;
begin
  InternalHelpHook := nil;
  UnhookMessage(MessageHook);
  UnhookMouse(MouseHook);
  inherited Destroy;
end;

procedure THelpHook.MouseHook(Msg: Cardinal; const HookStruct: TMouseHookStruct;
  var Remove: Boolean);
begin
  if Msg = WM_MOUSEMOVE then
    Hover(FindDragTarget(HookStruct.pt, True));
end;

procedure THelpHook.MessageHook(const WinProcStruct: TCWPStruct);
begin
  if WinProcStruct.message = WM_KILLFOCUS then
    Focus(FindControl(WinProcStruct.wParam))
  else if WinProcStruct.message = WM_SETFOCUS then
    Focus(FindControl(WinProcStruct.hwnd));
end;

procedure THelpHook.Hover(HelpControl: TControl);
begin
  if FActive and Assigned(FOnHover) and (HelpControl <> FHoverControl) then
    FOnHover(Self, HelpControl);
  FHoverControl := HelpControl;
end;

procedure THelpHook.Focus(HelpControl: TControl);
begin
  if FActive and Assigned(FOnFocus) and (HelpControl <> FFocusControl) then
    FOnFocus(Self, HelpControl);
  FFocusControl := HelpControl;
end;

function HelpHook: THelpHook;
begin
  if InternalHelpHook = nil then
    THelpHook.Create;
  Result := THelpHook(InternalHelpHook);
end;

initialization
  InternalHelpHook := nil;
finalization
  InternalHelpHook.Free;
end.
