
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit DropMgr;

interface

{$I CODEBOT.INC}

uses
  Classes, Controls, Windows, Messages, ShellAPI, SysUtils;

{ TDropManager class }

type
  TDropFilesEvent = procedure(Sender: TObject; Files: TStrings) of object;

  TDropManager = class
  private
    FList: TList;
    FUseUNC: Boolean;
    FOnDropFiles: TDropFilesEvent;
    FFilter: string;
    function FindDropControl(const AWinControl: TWinControl): TObject;
  protected
    procedure DoDropFiles(DropControl: TObject; Drop: THandle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterControl(const AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
    procedure UnregisterControl(const AWinControl: TWinControl);
    function ControlRegistered(const AWinControl: TWinControl): Boolean;
    property Filter: string read FFilter write FFilter;
    property UseUNC: Boolean read FUseUNC write FUseUNC;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
  end;

{ TDropFiles }

  TDropFiles = class(TComponent)
  private
  	FWinControl: TWinControl;
	  FOnDropFiles: TDropFilesEvent;
    procedure SetWinControl(const Value: TWinControl);
	protected
  	procedure DoDropFiles(Sender: TObject; Files: TStrings); dynamic;
		procedure Loaded; override;
		procedure Notification(AComponent: TComponent; Operation: TOperation); override;
	public
  	destructor Destroy; override;    
	published
  	property WinControl: TWinControl read FWinControl write SetWinControl;
    property OnDropFiles: TDropFilesEvent read FOnDropFiles write FOnDropFiles;
  end;

{ DropManager function }

function DropManager: TDropManager;

implementation

type
  TDropControl = class
  private
    FWndMethod: TWndMethod;
    FWinControl: TWinControl;
    FOnDropFiles: TDropFilesEvent;
  protected
    procedure ProcessMessage(var Msg: TMessage);
  public
    constructor Create(AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
    destructor Destroy; override;
    property Control: TWinControl read FWinControl;
  end;

{ TDropControl }

constructor TDropControl.Create(AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
begin
  FWinControl := AWinControl;
  FWndMethod := FWinControl.WindowProc;
  FWinControl.WindowProc := ProcessMessage;
  FOnDropFiles := DoDropFiles;
  DragAcceptFiles(FWinControl.Handle, True);
end;

destructor TDropControl.Destroy;
begin
  FWinControl.WindowProc := FWndMethod;
  DropManager.FList.Remove(Self);
  if FWinControl.HandleAllocated then
	  DragAcceptFiles(FWinControl.Handle, False);
end;

procedure TDropControl.ProcessMessage(var Msg: TMessage);
var
  DestroyMsgProc: TWndMethod;
begin
  case Msg.Msg of
    WM_CREATE:
      DragAcceptFiles(FWinControl.Handle, True);
    WM_DROPFILES:
    begin
      DropManager.DoDropFiles(Self, Msg.wParam);
      Msg.Result := 0;
    end;
    WM_DESTROY:
    begin
      DestroyMsgProc := FWndMethod;
      Destroy;
      DestroyMsgProc(Msg);
    end
    else
      FWndMethod(Msg);
  end;
end;

{ TDropManager }

constructor TDropManager.Create;
begin
  FList := TList.Create;
end;

destructor TDropManager.Destroy;
begin
  FList.Free;
end;

function TDropManager.FindDropControl(const AWinControl: TWinControl): TObject;
var
  j: Integer;
begin
  Result := nil;
  for j := 0 to FList.Count-1 do
  if TDropControl(FList[j]).Control = AWinControl then
  begin
    Result := FList[j];
    Break;
  end;
end;

procedure TDropManager.DoDropFiles(DropControl: TObject; Drop: THandle);
var
  Files: TStrings;
  Buffer: array [0..MAX_PATH] of Char;
  F: string;
  S: string;
  I: Integer;
begin
  Files := TStringList.Create;
  try
  	F := Trim(UpperCase(FFilter));
    for I := 0 to DragQueryFile(Drop, $FFFFFFFF, Buffer, MAX_PATH)-1 do
    begin
      DragQueryFile(Drop, I, Buffer, MAX_PATH);
      S := Buffer;
      if (F = '') or (UpperCase(ExtractFileExt(S)) = F) then
	      if FUseUNC then
  	      Files.Add(ExpandUNCFilename(S))
    	  else
      	  Files.Add(S);
    end;
    with TDropControl(DropControl) do
      if Assigned(FOnDropFiles) then
        FOnDropFiles(Control, Files)
      else if Assigned(Self.FOnDropFiles) then
        Self.FOnDropFiles(Control, Files)
      else
        SetWindowText(Control.Handle, PChar(Trim(Files.Text)));
  finally
    Files.Free;
    DragFinish(Drop);
  end;
end;

procedure TDropManager.RegisterControl(const AWinControl: TWinControl; DoDropFiles: TDropFilesEvent);
var
  DropControl: TObject;
begin
  DropControl := FindDropControl(AWinControl);
  if not Assigned(DropControl) then
  begin
    DropControl := TDropControl.Create(AWinControl, DoDropFiles);
    FList.Add(DropControl);
  end;
end;

procedure TDropManager.UnregisterControl(const AWinControl: TWinControl);
var
  DropControl: TObject;
begin
  DropControl := FindDropControl(AWinControl);
  if Assigned(DropControl) then
    DropControl.Free;
end;

function TDropManager.ControlRegistered(const AWinControl: TWinControl): Boolean;
begin
  Result := Assigned(FindDropControl(AWinControl));
end;

{ TDropFiles }

destructor TDropFiles.Destroy;
begin
	WinControl := nil;
  inherited Destroy;
end;

procedure TDropFiles.DoDropFiles(Sender: TObject; Files: TStrings);
begin
	if Assigned(FOnDropFiles) then FOnDropFiles(Self, Files);
end;

procedure TDropFiles.Loaded;
begin
	inherited Loaded;
  if FWinControl <> nil then
		DropManager.RegisterControl(FWinControl, DoDropFiles);
end;

procedure TDropFiles.Notification(AComponent: TComponent; Operation: TOperation);
begin
	inherited Notification(AComponent, Operation);
	if (Operation = opRemove) and (AComponent = FWinControl) then
  	WinControl := nil;
end;

procedure TDropFiles.SetWinControl(const Value: TWinControl);
begin
  if Value <> FWinControl then
  begin
  	if FWinControl <> nil then
    begin
    	FWinControl.RemoveFreeNotification(Self);
      DropManager.UnregisterControl(FWinControl);
    end;
	  FWinControl := Value;
  	if FWinControl <> nil then
    begin
    	FWinControl.FreeNotification(Self);
      if not (csLoading in ComponentState) then
	      DropManager.RegisterControl(FWinControl, DoDropFiles);
    end;
  end;
end;

var
  Manager: TObject;

function DropManager: TDropManager;
begin
  if Manager = nil then
    Manager := TDropManager.Create;
  Result := TDropManager(Manager);
end;

initialization
  Manager := nil;
finalization
  Manager.Free;
end.



