
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit ThreadTools;

interface

{$I CODEBOT.INC}

uses
	Windows, Messages, SysUtils, Classes, ActiveX, SysTools;

type
  TInterfaceListFilter = function (Item: IInterface): Boolean;
  TInterfaceListSort = function (Item1, Item2: IInterface): Integer;

  ISortInterfaceList = interface(IInterfaceList)
  ['{98191C65-EF68-4217-B958-1070C58AF9AF}']
    procedure Copy(List: ISortInterfaceList; Filter: TInterfaceListFilter);
    procedure Sort(Compare: TInterfaceListSort);
  end;

  TSortInterfaceList = class(TInterfacedObject, IInterfaceList, ISortInterfaceList)
  private
    FList: TThreadList;
    FLockedList: TList;
    FLockRef: Integer;
  protected
    { IInterfaceList }
    function Get(Index: Integer): IInterface;
    function GetCapacity: Integer;
    function GetCount: Integer;
    procedure Put(Index: Integer; const Item: IInterface);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TSortInterfaceList;
    function First: IInterface;
    function IndexOf(const Item: IInterface): Integer;
    function Add(const Item: IInterface): Integer;
    procedure Insert(Index: Integer; const Item: IInterface);
    function Last: IInterface;
    function Remove(const Item: IInterface): Integer;
    procedure Lock;
    procedure Unlock;
    { IInterfaceList }
    procedure Copy(List: ISortInterfaceList; Filter: TInterfaceListFilter);
    procedure Sort(Compare: TInterfaceListSort);
  public
    constructor Create;
    destructor Destroy; override;
  end;
{ TMessageThread }

const
  WM_SYNC = WM_USER + $A0;

type
  EThreadException = class(Exception);

  TSyncMethod = TThreadMethod;

  TSimpleThread = class(TThread)
  private
    FMethod: TThreadMethod;
  protected
    procedure Execute; override;
  public
    constructor Create(Ref: Pointer; Method: TThreadMethod);
    procedure Sync(Method: TThreadMethod);
  end;

  TThreadEvent = (teStart, teTerminate, teMessage, teException);

  TThreadEventParams = record
    Event: TThreadEvent;
    Message: string;
    Error: Exception;
  end;

  TMessageThreadEvent = procedure(Sender: TObject; const Params: TThreadEventParams) of object;

	TMessageThread = class(TThread)
	private
  	FAborted: Boolean;
    FThreadEvent: TMessageThreadEvent;
    FParams: TThreadEventParams;
    FSilent: Boolean;
    FUtilityWindow: TUtilityWindow;
    procedure Sync(Event: TThreadEvent; Message: string; Error: Exception);
    procedure WMSync(var Msg: TMessage); message WM_SYNC;
	protected
    procedure Execute; override;
    procedure Init; virtual;
    procedure Uninit; virtual;
    procedure Run; virtual; abstract;
  	procedure SyncStart; virtual;
  	procedure SyncTerminate; virtual;
  	procedure SyncMessage(const Msg: string); virtual;
  	procedure SyncException(E: Exception); virtual;
  	property Aborted: Boolean read FAborted;
	public
  	constructor Create(ThreadEvent: TMessageThreadEvent; Silent: Boolean = False);
    destructor Destroy; override;
		procedure Abort;
    procedure Display(const Msg: string);
  end;

function ThreadEventParams(Event: TThreadEvent; const Message: string;
  Error: Exception): TThreadEventParams;

implementation

uses
  RTLConsts;

constructor TSortInterfaceList.Create;
begin
  inherited Create;
  FList := TThreadList.Create;
end;

destructor TSortInterfaceList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TSortInterfaceList.Clear;
var
  I: Integer;
begin
    with FList.LockList do
    try
      for I := 0 to Count - 1 do
        IInterface(List[I]) := nil;
      Clear;
    finally
      Self.FList.UnlockList;
    end;
end;

procedure TSortInterfaceList.Delete(Index: Integer);
begin
  with FList.LockList do
  try
    Self.Put(Index, nil);
    Delete(Index);
  finally
    Self.FList.UnlockList;
  end;
end;

function TSortInterfaceList.Expand: TSortInterfaceList;
begin
  with FList.LockList do
  try
    Expand;
    Result := Self;
  finally
    Self.FList.Unlocklist;
  end;
end;

function TSortInterfaceList.First: IInterface;
begin
  Result := Get(0);
end;

function TSortInterfaceList.Get(Index: Integer): IInterface;
begin
  Lock;
  with FLockedList do
  try
    if (Index < 0) or (Index >= Count) then
      Error(@SListIndexError, Index);
    Result := IInterface(List[Index]);
  finally
    Unlock;
  end;
end;

function TSortInterfaceList.GetCapacity: Integer;
begin
  Lock;
  with FLockedList do
  try
    Result := Capacity;
  finally
    Unlock;
  end;
end;

function TSortInterfaceList.GetCount: Integer;
begin
  Lock;
  with FLockedList do
  try
    Result := Count;
  finally
    Unlock;
  end;
end;

function TSortInterfaceList.IndexOf(const Item: IInterface): Integer;
begin
  Lock;
  with FLockedList do
  try
    Result := IndexOf(Pointer(Item));
  finally
    Unlock;
  end;
end;

function TSortInterfaceList.Add(const Item: IInterface): Integer;
begin
  Lock;
  with FLockedList do
  try
    Result := Add(nil);
    IInterface(List[Result]) := Item;
  finally
    Unlock;
  end;
end;

procedure TSortInterfaceList.Insert(Index: Integer; const Item: IInterface);
begin
  Lock;
  with FLockedList do
  try
    Insert(Index, nil);
    IInterface(List[Index]) := Item;
  finally
    Unlock;
  end;
end;

function TSortInterfaceList.Last: IInterface;
begin
  Lock;
  with FLockedList do
  try
    Result := Self.Get(Count - 1);
  finally
    Unlock;
  end;
end;

procedure TSortInterfaceList.Put(Index: Integer; const Item: IInterface);
begin
  Lock;
  with FLockedList do
  try
    if (Index < 0) or (Index >= Count) then Error(@SListIndexError, Index);
    IInterface(List[Index]) := Item;
  finally
    Unlock;
  end;
end;

function TSortInterfaceList.Remove(const Item: IInterface): Integer;
begin
  Lock;
  with FLockedList do
  try
    Result := IndexOf(Pointer(Item));
    if Result > -1 then
    begin
      IInterface(List[Result]) := nil;
      Delete(Result);
    end;
  finally
    Unlock;
  end;
end;

procedure TSortInterfaceList.SetCapacity(NewCapacity: Integer);
begin
  Lock;
  with FLockedList do
  try
    Capacity := NewCapacity;
  finally
    Unlock;
  end;
end;

procedure TSortInterfaceList.SetCount(NewCount: Integer);
begin
  Lock;
  with FLockedList do
  try
    Count := NewCount;
  finally
    Unlock;
  end;
end;

procedure TSortInterfaceList.Exchange(Index1, Index2: Integer);
begin
  Lock;
  with FLockedList do
  try
    Exchange(Index1, Index2);
  finally
    Unlock;
  end;
end;

procedure TSortInterfaceList.Lock;
begin
  if FLockRef = 0 then
  begin
    InterlockedIncrement(FLockRef);
    FLockedList := FList.LockList;
  end;
end;

procedure TSortInterfaceList.Unlock;
begin
  if FLockRef = 1 then
  begin
    InterlockedDecrement(FLockRef);
    FLockedList := FList.LockList;
  end;
end;

procedure TSortInterfaceList.Copy(List: ISortInterfaceList; Filter: TInterfaceListFilter);
var
  Intf: IInterface;
  I: Integer;
begin
  Lock;
  try
    Clear;
    if Assigned(Filter) then
      for I := 0 to List.Count - 1 do
      begin
        Intf := List[I];
        if Filter(Intf) then
          Add(Intf)
      end
    else
    begin
      Self.SetCapacity(List.Capacity);
      for I := 0 to List.Count - 1 do
        Add(List[I]);
    end;
  finally
    Unlock;
  end;
end;

procedure TSortInterfaceList.Sort(Compare: TInterfaceListSort);
begin
  Lock;
  try
    FLockedList.Sort(TListSortCompare(@Compare));
  finally
    Unlock;
  end;
end;

function ThreadEventParams(Event: TThreadEvent; const Message: string;
  Error: Exception): TThreadEventParams;
begin
  Result.Event := Event;
  Result.Message := Message;
  Result.Error := Error;
end;

{ TSimpleThread }

type
  PSimpleThread = ^TSimpleThread;

procedure TSimpleThread.Execute;
begin
  FreeOnTerminate := True;
  FMethod;
end;

constructor TSimpleThread.Create(Ref: Pointer; Method: TThreadMethod);
begin
  PSimpleThread(Ref)^ := Self;
  FMethod := Method;
  inherited Create(False);
end;

procedure TSimpleThread.Sync(Method: TThreadMethod);
begin
  Synchronize(Method);
end;

{ TMessageThread }

constructor TMessageThread.Create(ThreadEvent: TMessageThreadEvent; Silent: Boolean = False);
begin
	FUtilityWindow := TUtilityWindow.Create(Self);
	FThreadEvent := ThreadEvent;
	FSilent := Silent;
  inherited Create(False);
end;

destructor TMessageThread.Destroy;
begin
  FUtilityWindow.Free;
	inherited Destroy;
end;

procedure TMessageThread.Init;
begin
end;

procedure TMessageThread.Uninit;
begin
end;

procedure TMessageThread.Execute;
begin
  FreeOnTerminate := True;
	CoInitialize(nil);
  try
    Init;
    try
      SyncStart;
      try
        Run;
      except
        on E: Exception do SyncException(E);
      end;
  	finally
      SyncTerminate;
    end;
  finally
    Uninit;
    CoUninitialize;
	end;
end;

procedure TMessageThread.Abort;
begin
	FAborted := True;
end;

procedure TMessageThread.Display(const Msg: string);
begin
  SyncMessage(Msg);
end;

procedure TMessageThread.Sync(Event: TThreadEvent; Message: string; Error: Exception);
begin
  if FSilent then Exit;
	if Assigned(FThreadEvent) then
  begin
    FParams.Event := Event;
    FParams.Message := Message;
    FParams.Error := Error;
    SendMessage(FUtilityWindow.Handle, WM_SYNC, 0, 0);
  end;
end;

procedure TMessageThread.WMSync(var Msg: TMessage);
begin
	if Assigned(FThreadEvent) then
    FThreadEvent(Self, FParams);
end;

procedure TMessageThread.SyncStart;
begin
  Sync(teStart, '', nil);
end;

procedure TMessageThread.SyncTerminate;
begin
  Sync(teTerminate, '', nil);
end;

procedure TMessageThread.SyncMessage(const Msg: string);
begin
  Sync(teMessage, Msg, nil);
end;

procedure TMessageThread.SyncException(E: Exception);
begin
  Sync(teException, '', E);
  Abort;
end;

end.
