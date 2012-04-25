
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit NetSock;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, WinSock, ActiveX, SysUtils, Classes, InetTools, SysTools;

{ TThreadSynchronizer }

var
  ThreadCount: Integer;
  SyncCount: Integer;
  SocketCount: Integer;
  ObjectCount: Integer;

type
  IThreadSynchronizer = interface(IInterface)
    ['{3443271E-D4FC-4D1A-B5C2-B307FBA6750D}']
    function GetData: IInterface;
    procedure SetData(Value: IInterface);
    procedure Lock;
    procedure Unlock;
    property Data: IInterface read GetData write SetData;
    function Sync(Msg: Cardinal; wParam, lParam: Integer): Integer;
  end;

  TThreadSynchronizer = class(TInterfacedObject, IThreadSynchronizer)
  private
    FCriticalSection: TRTLCriticalSection;
    FWnd: HWND;
    FData: IInterface;
    { IThreadSynchronizer }
    function GetData: IInterface;
    procedure SetData(Value: IInterface);
    procedure Lock;
    procedure Unlock;
    function Sync(Msg: Cardinal; wParam, lParam: Integer): Integer;
  public
    constructor Create(Wnd: HWND);
    destructor Destroy; override;
  end;

  TThreadExecuteProc = procedure(Thread: TThread);

const
  NM_THREAD_EVENT = WM_USER + $1B;

type
  TNMThreadMessage = packed record
    Msg: Cardinal;
    Data: Pointer;
    Thread: TThread;
    Result: Integer;
  end;

  TNetThreadEvent = (teStateChange, teWait, teDataAvailable, teWriteComplete, teAccept);
  TNetState = (nsDisconnected, nsListening, nsOpening, nsResolving, nsConnecting, nsConnected);

  TNetStateChangeEvent = procedure(Sender: TObject; OldState, NewState: TNetState) of object;
  TNetDataAvailableEvent = procedure(Sender: TObject; Data: Pointer; DataSize: Integer) of object;

{ TNetSocketLink }

  TNetSocketLink = class(TObject)
  private
    FConnection: TObject;
    FOnStateChange: TNetStateChangeEvent;
    FOnDataAvailable: TNetDataAvailableEvent;
    FOnWriteComplete: TNotifyEvent;
  protected
    procedure NotifyStateChange(Sender: TObject; OldState, NewState: TNetState);
    procedure NotifyDataAvailable(Sender: TObject; Data: Pointer; DataSize: Integer);
    procedure NotifyWriteComplete(Sender: TObject);
  public
    destructor Destroy; override;
    property OnStateChange: TNetStateChangeEvent read FOnStateChange write FOnStateChange;
    property OnDataAvailable: TNetDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
    property OnWriteComplete: TNotifyEvent read FOnWriteComplete write FOnWriteComplete;
  end;

{ TNetSocket }

  TNetSocket = class
  private
    FDestroying: Boolean;
    FWindow: TUtilityWindow;
    FSynchronizer: IThreadSynchronizer;
    FPort: Integer;
    FState: TNetState;
    FData: Pointer;
    FTag: Integer;
    FUnknown: IUnknown;
    FLinks: TList;
    FOnStateChange: TNetStateChangeEvent;
    procedure SetPort(Value: Integer);
  protected
    function GetActive: Boolean; virtual; abstract;
    procedure SetActive(Value: Boolean); virtual; abstract;
    procedure CreateThread(Proc: TThreadExecuteProc; var Thread: TThread); virtual; abstract;
    property Synchronizer: IThreadSynchronizer read FSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disconnect; virtual; abstract;
    property Active: Boolean read GetActive write SetActive;
    property Port: Integer read FPort write SetPort;
    property State: TNetState read FState;
    property Data: Pointer read FData write FData;
    property Tag: Integer read FTag write FTag;
    property Unknown: IUnknown read FUnknown write FUnknown;
    property OnStateChange: TNetStateChangeEvent read FOnStateChange write FOnStateChange;
  end;

{ TNetClientSocket }

  TNetClientSocket = class(TNetSocket)
  private
    FSocket: TSocket;
    FStreams: IInterfaceList;
    FLinks: TList;
    FReadThread: TThread;
    FWriteThread: TThread;
    FHost: string;
    FServer: Cardinal;
    FWaiting: Boolean;
    FHaveWritten: Boolean;
    FOnDataAvailable: TNetDataAvailableEvent;
    FOnWriteComplete: TNotifyEvent;
    procedure SetHost(const Value: string);
    function HandleStateChange(Thread: TThread; NewState: TNetState; Server: Cardinal): Boolean;
    function HandleDataAvailable(Thread: TThread; Buffer: Pointer; BufferSize: Cardinal): Boolean;
    function HandleWait(Thread: TThread): Boolean;
    procedure NMThreadEvent(var Message: TNMThreadMessage); message NM_THREAD_EVENT;
  protected
    procedure DoStateChange(OldState, NewState: TNetState); dynamic;
    procedure DoDataAvailable(Data: Pointer; DataSize: Integer); dynamic;
    procedure DoWriteComplete; dynamic;
    function GetActive: Boolean; override;
    procedure SetActive(Value: Boolean); override;
    procedure CreateThread(Proc: TThreadExecuteProc; var Thread: TThread); override;
    procedure ServerConnect(Socket: TSocket; Server: Cardinal; Port: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterLink(Link: TNetSocketLink);
    procedure UnregisterLink(Link: TNetSocketLink);
    procedure Connect;
    procedure Disconnect; override;
    procedure WriteBytes(Data: Pointer; DataSize: Integer);
    procedure WriteFile(const FileName: string);
    procedure WriteString(const Data: string);
    procedure WriteStream(Stream: TStream; Ownership: TStreamOwnership = soOwned);
    property Host: string read FHost write SetHost;
    property Server: Cardinal read FServer;
    property OnDataAvailable: TNetDataAvailableEvent read FOnDataAvailable write FOnDataAvailable;
    property OnWriteComplete: TNotifyEvent read FOnWriteComplete write FOnWriteComplete;
  end;

{ TNetSocketList }

  TNetSocketList = class(TObject)
  private
    FOwnsSockets: Boolean;
    FList: TList;
    function GetCount: Integer;
    function GetSocket(Index: Integer): TNetClientSocket;
  public
    constructor Create(OwnsSockets: Boolean);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Socket: TNetClientSocket);
    procedure Remove(Socket: TNetClientSocket);
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(Socket: TNetClientSocket): Integer;
    procedure Insert(Index: Integer; Socket: TNetClientSocket);
    procedure Move(CurIndex, NewIndex: Integer);
    property Count: Integer read GetCount;
    property Socket[Index: Integer]: TNetClientSocket read GetSocket; default;
  end;

{ TNetServerSocket }

  TNetClientSocketEvent = procedure(Sender: TObject; Client: TNetClientSocket) of object;

  TNetServerSocket = class(TNetSocket)
  private
    FSocket: TSocket;
    FClients: TNetSocketList;
    FLink: TNetSocketLink;
    FListenThread: TThread;
    FOnClientConnect: TNetClientSocketEvent;
    FOnClientDisconnect: TNetClientSocketEvent;
    procedure ClientStateChange(Sender: TObject; OldState, NewState: TNetState);
    function HandleStateChange(Thread: TThread): Boolean;
    function HandleAccept(Thread: TThread; Socket: TSocket; Server: Cardinal; Port: Integer): Boolean;
    procedure NMThreadEvent(var Message: TNMThreadMessage); message NM_THREAD_EVENT;
  protected
    function GetActive: Boolean; override;
    procedure SetActive(Value: Boolean); override;
    procedure CreateThread(Proc: TThreadExecuteProc; var Thread: TThread); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disconnect; override;
    procedure Listen;
    property Clients: TNetSocketList read FClients;
    property OnClientConnect: TNetClientSocketEvent read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect: TNetClientSocketEvent read FOnClientDisconnect write FOnClientDisconnect;
  end;

{ THttpHeaders }

  THttpHeaders = class(TObject)
  private
    FTopic: string;
    FLines: TStrings;
    FBuffer: PAnsiChar;
    FBufferSize: Integer;
    function GetCount: Integer;
    function GetKey(Index: Integer): string;
    function GetValue(Index: Integer): string;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    function Parse(Buffer: Pointer; BufferSize: Integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Key, Value: string);
    procedure Remove(const Key: string);
    procedure Delete(Index: Integer);
    function Find(const Key, Value: string): Boolean;
    function Lookup(const Key: string; out Value: string; const Default: string = ''): Boolean;
    property Keys[Index: Integer]: string read GetKey;
    property Values[Index: Integer]: string read GetValue;
    property Topic: string read FTopic write FTopic;
    property Count: Integer read GetCount;
    property Text: string read GetText write SetText;
  end;

{ THttpStreamer }

  THttpStreamer = class(TObject)
  private
    FHeaders: THttpHeaders;
    FLink: TNetSocketLink;
    FBody: TStream;
    FOwnership: TStreamOwnership;
  protected
    procedure ClientStateChange(Sender: TObject; OldState, NewState: TNetState); virtual;
    procedure ClientDataAvailable(Sender: TObject; Data: Pointer; DataSize: Integer); virtual;
    procedure ClientWriteComplete(Sender: TObject); virtual;
    procedure ReleaseBody;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(Socket: TNetClientSocket);
    procedure Detach(Socket: TNetClientSocket);
    procedure CopyBody(Stream: TStream; Ownership: TStreamOwnership = soOwned);
    procedure Reset;
    property Headers: THttpHeaders read FHeaders;
    property Body: TStream read FBody;
  end;

{ THttpRequestReader }

  THttpRequestReader = class(THttpStreamer)
  protected
    procedure ClientStateChange(Sender: TObject; OldState, NewState: TNetState); override;
    procedure ClientDataAvailable(Sender: TObject; Data: Pointer; DataSize: Integer); override;
    procedure ClientWriteComplete(Sender: TObject); virtual;
  end;

{ THttpReader }

    (*THttpReader = class(THttpStreamer)
  private
    FOnHeaderCreate: TNotifyEvent;
    FOnHeaderComplete: TNotifyEvent;
    FOnBodyCreate: TNotifyEvent;
    FOnBodyComplete: TNotifyEvent;
  public
    property OnHeaderCreate: TNotifyEvent read FOnHeaderCreate write FOnHeaderCreate;
    property OnHeaderComplete: TNotifyEvent read FOnHeaderComplete write FOnHeaderComplete;
    property OnBodyCreate: TNotifyEvent read FOnBodyCreate write FOnBodyCreate;
    property OnBodyComplete: TNotifyEvent read FOnBodyComplete write FOnBodyComplete;
  end;

{ THttpRequestReader }

  THttpRequestReader = class(THttpReader)
  private
    FContentSize: Integer;
  protected
    procedure SocketStateChange(Sender: TObject; OldState, NewState: TNetState); override;
    procedure SocketDataAvailable(Sender: TObject; Data: Pointer; DataSize: Integer); override;
    procedure SocketWriteComplete(Sender: TObject); override;
  end;

{ THttpRequestReader }

  THttpRequestReader = class(THttpReader)
  private
    FMethod: string;
    FResource: string
  public
    property Method: string read FMethod;
    property Resource: string read FResource;
  end;*)

  {THttpReader = class(TObject)
    procedure Clear;
    property Header: TStrings
    property ContentLength;
    property Body: TStream;
  end;

  THttpStreamer = class
  protected

  public
    constructor Create(Handler: THttpHandler)
  end;

  THttpReader = class(THttpStreamer)

    procedure Read(Data: Pointer; DataSize: Integer);
  end;

  THttpResponse = class(THttpHandler)
    property Status: string
    property StatusCode: Integer
    property Date:
  end;

  THttpWriter = class(THttpHandler)
  end;

  THttpResponseWriter = class(THttpReader)
  end;

  THttpReader = class(THttpHandler)
  property
    procedure
  end;}

implementation

const
  WriteBytesSize = 1024 * 5;
  ReadBytesSize = 1024;

{ TThreadSynchronizer }

constructor TThreadSynchronizer.Create(Wnd: HWND);
begin
  InterlockedIncrement(SyncCount);
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
  FWnd := Wnd;
end;

destructor TThreadSynchronizer.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited Destroy;
  InterlockedDecrement(SyncCount);
end;

{ TThreadSynchronizer.IThreadSynchronizer }

function TThreadSynchronizer.GetData: IInterface;
begin
  Result := FData;
end;

procedure TThreadSynchronizer.SetData(Value: IInterface);
begin
  FData := Value;
end;

procedure TThreadSynchronizer.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TThreadSynchronizer.Unlock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TThreadSynchronizer.Sync(Msg: Cardinal; wParam, lParam: Integer): Integer;
begin
  Result := SendMessage(FWnd, Msg, wParam, lParam);
end;

type
  TNetThreadData = record
    Socket: TSocket;
    ClientSocket: TSocket;
    Host: string;
    Server: Cardinal;
    Port: Integer;
    Buffer: Pointer;
    BufferSize: Cardinal;
    State: TNetState;
    Streams: IInterfaceList;
    Synchronizer: IThreadSynchronizer;
    Event: TNetThreadEvent;
    Result: Boolean;
  end;
  PNetThreadData = ^TNetThreadData;

{ TNetSocketThread }

type
  TNetSocketThread = class(TThread)
  private
    FProc: TThreadExecuteProc;
  protected
    procedure Execute; override;
  public
    Data: TNetThreadData;
    constructor Create(const ThreadData: TNetThreadData; const Proc: TThreadExecuteProc);
    property Terminated;
  end;

constructor TNetSocketThread.Create(const ThreadData: TNetThreadData; const Proc: TThreadExecuteProc);
begin
  FProc := Proc;
  Data := ThreadData;
  inherited Create(False);
end;

procedure TNetSocketThread.Execute;
begin
  FreeOnTerminate := True;
  InterlockedIncrement(ThreadCount);
  FProc(Self);
  InterlockedDecrement(ThreadCount);
end;

{ TNetSocket }

constructor TNetSocket.Create;
begin
  InterlockedIncrement(ObjectCount);
  inherited Create;
  SocketStartup;
  FWindow := TUTilityWindow.Create(Self);
  FSynchronizer := TThreadSynchronizer.Create(FWindow.Handle);
end;

destructor TNetSocket.Destroy;
begin
  FDestroying := True;
  Disconnect;
  FWindow.Free;
  inherited Destroy;
  InterlockedDecrement(ObjectCount);
end;

procedure TNetSocket.SetPort(Value: Integer);
begin
  if Value <> FPort then
  begin
    Disconnect;
    FPort := Value;
  end;
end;

{ Sync methods }

function SyncBool(Thread: TNetSocketThread; Event: TNetThreadEvent): Boolean;
begin
  with Thread do
  begin
    Data.Result := False;
    Data.Event := Event;
    Data.Result := Data.Synchronizer.Sync(NM_THREAD_EVENT, Integer(@Data), Integer(Thread)) = 1;
    Result := Data.Result;
  end;
end;

function CanStateChange(Thread: TNetSocketThread): Boolean;
begin
  Result := SyncBool(Thread, teStateChange);
end;

function CanDataAvailable(Thread: TNetSocketThread): Boolean;
begin
  if Thread.Data.BufferSize > 0 then
    Result := SyncBool(Thread, teDataAvailable)
  else
    Result := False;
end;

function CanAccept(Thread: TNetSocketThread): Boolean;
begin
  Result := SyncBool(Thread, teAccept);
end;

function CanWait(Thread: TNetSocketThread): Boolean;
begin
  Result := SyncBool(Thread, teWait);
end;

{ TNetSocketLink }

destructor TNetSocketLink.Destroy;
begin
  if FConnection is TNetClientSocket then
    TNetClientSocket(FConnection).UnregisterLink(Self);
  inherited Destroy;
end;

procedure TNetSocketLink.NotifyStateChange(Sender: TObject; OldState, NewState: TNetState);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Sender, OldState, NewState);
end;

procedure TNetSocketLink.NotifyDataAvailable(Sender: TObject; Data: Pointer; DataSize: Integer);
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(Sender, Data, DataSize);
end;

procedure TNetSocketLink.NotifyWriteComplete(Sender: TObject);
begin
  if Assigned(FOnWriteComplete) then
    FOnWriteComplete(Sender);
end;

{ TNetClientSocket }

function Resolve(const Host: string): Cardinal;
var
  HostEnt: PHostEnt;
begin
  Result := 0;
  HostEnt := gethostbyname(PAnsiChar(Host));
  if HostEnt <> nil then
    Result := PLongword(HostEnt^.h_addr_list^)^;
end;

procedure ClientReadProc(Thread: TThread);
var
  SocketThread: TNetSocketThread absolute Thread;
  Addr: TSockAddrIn;
begin
  SocketThread.Data.State := nsResolving;
  if not CanStateChange(SocketThread) then Exit;
  SocketThread.Data.Server := Resolve(SocketThread.Data.Host);
  if SocketThread.Data.Server = 0 then
    SocketThread.Data.State := nsDisconnected
  else
    SocketThread.Data.State := nsConnecting;
  if not CanStateChange(SocketThread) then Exit;
  Addr.sin_family := AF_INET;
  Addr.sin_addr.s_addr := SocketThread.Data.Server;
  Addr.sin_port := htons(SocketThread.Data.Port);
  if WinSock.connect(SocketThread.Data.Socket, Addr, SizeOf(Addr)) <> SOCKET_ERROR then
    SocketThread.Data.State := nsConnected
  else
    SocketThread.Data.State := nsDisconnected;
  if not CanStateChange(SocketThread) then Exit;
  GetMem(SocketThread.Data.Buffer, ReadBytesSize);
  try
    repeat
      SocketThread.Data.BufferSize := recv(SocketThread.Data.Socket,
        SocketThread.Data.Buffer^, ReadBytesSize, 0);
      if not CanDataAvailable(SocketThread) then Break;
    until False;
  finally
    FreeMem(SocketThread.Data.Buffer);
  end;
  SocketThread.Data.State := nsDisconnected;
  CanStateChange(SocketThread);
end;

procedure ClientWriteProc(Thread: TThread);
var
  SocketThread: TNetSocketThread absolute Thread;
  Stream: IStream;
  Bytes: PByte;
  DataRead, DataWrite, DataSend: Integer;
begin
  GetMem(SocketThread.Data.Buffer, WriteBytesSize);
  try
    while True do
    begin
      Stream := nil;
      SocketThread.Data.Synchronizer.Lock;
      try
        if SocketThread.Data.Streams.Count > 0 then
        begin
          Stream := SocketThread.Data.Streams[0] as IStream;
          SocketThread.Data.Streams.Delete(0);
        end;
      finally
        SocketThread.Data.Synchronizer.Unlock;
      end;
      if Stream = nil then
      begin
        if not CanWait(SocketThread) then
          Exit;
      end
      else
      repeat
        FillChar(SocketThread.Data.Buffer^, WriteBytesSize, #0);
        Stream.Read(SocketThread.Data.Buffer, WriteBytesSize, @DataRead);
        Bytes := SocketThread.Data.Buffer;
        DataWrite := DataRead;
        while DataWrite > 0 do
        begin
          DataSend := send(SocketThread.Data.Socket, SocketThread.Data.Buffer^, DataRead, 0);
          if DataSend = SOCKET_ERROR then Exit;
          Inc(Bytes, DataSend);
          Dec(DataWrite, DataSend);
        end;
      until DataRead < WriteBytesSize;
    end;
  finally
    FreeMem(SocketThread.Data.Buffer);
  end;
end;

procedure ServerReadProc(Thread: TThread);
var
  SocketThread: TNetSocketThread absolute Thread;
begin
  GetMem(SocketThread.Data.Buffer, ReadBytesSize);
  try
    repeat
      SocketThread.Data.BufferSize := recv(SocketThread.Data.Socket,
        SocketThread.Data.Buffer^, ReadBytesSize, 0);
      if not CanDataAvailable(SocketThread) then Break;
    until False;
  finally
    FreeMem(SocketThread.Data.Buffer);
  end;
  SocketThread.Data.State := nsDisconnected;
  CanStateChange(SocketThread);
end;

constructor TNetClientSocket.Create;
begin
  inherited Create;
  FLinks := TList.Create;
  FStreams := TInterfaceList.Create as IInterfaceList;
  Synchronizer.Data := FStreams;
end;

destructor TNetClientSocket.Destroy;
begin
  FDestroying := True;
  FOnStateChange := nil;
  Disconnect;
  if FWaiting then
    Synchronizer.Unlock;
  while FLinks.Count > 0 do
    UnregisterLink(TNetSocketLink(FLinks[0]));
  FLinks.Free;
  inherited Destroy;
end;

procedure TNetClientSocket.RegisterLink(Link: TNetSocketLink);
begin
  if FLinks.IndexOf(Link) < 0 then
  begin
    FLinks.Add(Link);
    Link.FConnection := Self;
  end;
end;

procedure TNetClientSocket.UnregisterLink(Link: TNetSocketLink);
begin
  if FLinks.IndexOf(Link) > -1 then
  begin
    Link.FConnection := nil;
    FLinks.Remove(Link);
  end;
end;

procedure TNetClientSocket.CreateThread(Proc: TThreadExecuteProc; var Thread: TThread);
var
  Data: TNetThreadData;
begin
  Data.Socket := FSocket;
  Data.Host := FHost;
  Data.Port := FPort;
  Data.Streams := FStreams;
  Data.Synchronizer := Synchronizer;
  Thread := TNetSocketThread.Create(Data, Proc);
end;

procedure TNetClientSocket.ServerConnect(Socket: TSocket; Server: Cardinal; Port: Integer);
begin
  Disconnect;
  if not FWaiting then
    Synchronizer.Lock;
  FStreams.Clear;
  FWaiting := False;
  FReadThread := nil;
  FWriteThread := nil;
  Synchronizer.Unlock;
  FSocket := Socket;
  FState := nsConnected;
  FServer := Server;
  FPort := Port;
  CreateThread(ServerReadProc, FReadThread);
  CreateThread(ClientWriteProc, FWriteThread);
end;

procedure TNetClientSocket.Connect;
begin
  Disconnect;
  FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  InterlockedIncrement(SocketCount);
  if FSocket <> INVALID_SOCKET then
  begin
    FState := nsOpening;
    CreateThread(ClientReadProc, FReadThread);
    if Assigned(FOnStateChange) then
      FOnStateChange(Self, nsDisconnected, FState);
  end;
end;

procedure TNetClientSocket.Disconnect;
var
  OldState: TNetState;
begin
  FHaveWritten := False;
  if FState > nsDisconnected then
  begin
    OldState := FState;
    FState := nsDisconnected;
    shutdown(FSocket, SD_BOTH);
    closesocket(FSocket);
    InterlockedDecrement(SocketCount);
    FSocket := 0;
    FWriteThread := nil;
    FReadThread := nil;
    if not FWaiting then
      Synchronizer.Lock;
    FStreams.Clear;
    FWaiting := False;
    Synchronizer.Unlock;
    DoStateChange(OldState, FState);
  end;
end;

procedure TNetClientSocket.WriteBytes(Data: Pointer; DataSize: Integer);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  Stream.Write(Data^, DataSize);
  WriteStream(Stream);
end;

procedure TNetClientSocket.WriteFile(const FileName: string);
begin
  WriteStream(TFileStream.Create(FileName, fmOpenRead));
end;

procedure TNetClientSocket.WriteString(const Data: string);
begin
  WriteStream(TStringStream.Create(Data));
end;

procedure TNetClientSocket.WriteStream(Stream: TStream; Ownership: TStreamOwnership = soOwned);
var
  C: TStream;
  S: IStream;
begin
  if Ownership = soReference then
  begin
    C := TMemoryStream.Create;
    C.CopyFrom(Stream, 0);
  end
  else
    C := Stream;
  S := TStreamAdapter.Create(Stream, Ownership);
  if not FWaiting then
    Synchronizer.Lock;
  try
    FHaveWritten := True;
    FStreams.Add(S)
  finally
    FWaiting := False;
    Synchronizer.Unlock;
  end;
end;

function TNetClientSocket.GetActive: Boolean;
begin
  Result := FState > nsDisconnected;
end;

procedure TNetClientSocket.SetActive(Value: Boolean);
begin
  if Value <> GetActive then
    if Value then
      Connect
    else
      Disconnect;
end;

procedure TNetClientSocket.SetHost(const Value: string);
begin
  if Value <> FHost then
  try
    Disconnect;
  finally
    FHost := Value;
  end;
end;

function TNetClientSocket.HandleStateChange(Thread: TThread; NewState: TNetState; Server: Cardinal): Boolean;
var
  CanContinue: Boolean;
  OldState: TNetState;
begin
  CanContinue := (FState <> nsDisconnected) and (Thread = FReadThread);
  if CanContinue then
  begin
    if NewState = nsDisconnected then
      Disconnect
    else
    begin
      OldState := FState;
      FState := NewState;
      if NewState > nsResolving then
        FServer := Server;
      if NewState = nsConnected then
        CreateThread(ClientWriteProc, FWriteThread);
      if OldState <> NewState then
        DoStateChange(OldState, NewState);
    end;
  end;
  Result := (FState <> nsDisconnected) and (Thread = FReadThread);
end;

function TNetClientSocket.HandleDataAvailable(Thread: TThread; Buffer: Pointer; BufferSize: Cardinal): Boolean;
begin
  Result := (FState <> nsDisconnected) and (Thread = FReadThread);
  if Result then
    DoDataAvailable(Buffer, BufferSize);
end;

function TNetClientSocket.HandleWait(Thread: TThread): Boolean;
begin
  Result := Thread = FWriteThread;
  if FWaiting then Exit;
  if Result then
  begin
    Synchronizer.Lock;
    FWaiting := FStreams.Count = 0;
    if not FWaiting then
      Synchronizer.Unlock;
  end;
  if FHaveWritten and FWaiting then
    DoWriteComplete;
end;

procedure TNetClientSocket.DoStateChange(OldState, NewState: TNetState);
var
  L: TNetSocketLink;
  I: Integer;
begin
  if NewState <> OldState then
  try
    if Assigned(FOnStateChange) then
      FOnStateChange(Self, OldState, NewState);
  finally
    I := 0;
    while I < FLinks.Count do
    begin
      L := TNetSocketLink(FLinks[I]);
      L.NotifyStateChange(Self, OldState, NewState);
      Inc(I);
    end;
  end;
end;

procedure TNetClientSocket.DoDataAvailable(Data: Pointer; DataSize: Integer);
var
  L: TNetSocketLink;
  I: Integer;
begin
  if DataSize > 0 then
  try
    if Assigned(FOnDataAvailable) then
      FOnDataAvailable(Self, Data, DataSize);
  finally
    I := 0;
    while I < FLinks.Count do
    begin
      L := TNetSocketLink(FLinks[I]);
      L.NotifyDataAvailable(Self, Data, DataSize);
      Inc(I);
    end;
  end;
end;

procedure TNetClientSocket.DoWriteComplete;
var
  L: TNetSocketLink;
  I: Integer;
begin
  try
    if Assigned(FOnWriteComplete) then
      FOnWriteComplete(Self);
  finally
    I := 0;
    while I < FLinks.Count do
    begin
      L := TNetSocketLink(FLinks[I]);
      L.NotifyWriteComplete(Self);
      Inc(I);
    end;
  end;
end;

procedure TNetClientSocket.NMThreadEvent(var Message: TNMThreadMessage);
const
  BoolInts: array[Boolean] of Integer = (0, 1);
var
  Data: PNetThreadData;
  Thread: TThread;
  I: Integer;
begin
  Data := Message.Data;
  Thread := Message.Thread;
  I := 0;
  case Data.Event of
    teStateChange: I := BoolInts[HandleStateChange(Thread, Data.State, Data.Server)];
    teWait: I := BoolInts[HandleWait(Thread)];
    teDataAvailable: I := BoolInts[HandleDataAvailable(Thread, Data.Buffer, Data.BufferSize)];
  end;
  Message.Result := I;
end;

{ TNetSocketList }

constructor TNetSocketList.Create(OwnsSockets: Boolean);
begin
  inherited Create;
  FList := TList.Create;
  FOwnsSockets := OwnsSockets;
end;

destructor TNetSocketList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TNetSocketList.Clear;
var
  I: Integer;
begin
  if FOwnsSockets then
    for I := 0 to FList.Count - 1 do
      TObject(FList[I]).Free;
  FList.Clear;
end;

procedure TNetSocketList.Add(Socket: TNetClientSocket);
var
  I: Integer;
begin
  I := FList.IndexOf(Socket);
  if I < 0 then
    FList.Add(Socket);
end;

procedure TNetSocketList.Remove(Socket: TNetClientSocket);
var
  I: Integer;
begin
  I := FList.IndexOf(Socket);
  if I > -1 then
  begin
    FList.Delete(I);
    if FOwnsSockets and (not Socket.FDestroying) then
      Socket.Free;
  end;
end;

procedure TNetSocketList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TNetSocketList.IndexOf(Socket: TNetClientSocket): Integer;
begin
  Result := FList.IndexOf(Socket);
end;

procedure TNetSocketList.Insert(Index: Integer; Socket: TNetClientSocket);
var
  I: Integer;
begin
  I := FList.IndexOf(Socket);
  if I < 0 then
    FList.Insert(I, Socket)
  else
    FList.Move(I, Index);    
end;

procedure TNetSocketList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TNetSocketList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TNetSocketList.GetSocket(Index: Integer): TNetClientSocket;
begin
  Result := TNetClientSocket(FList[Index]);
end;

{ TNetServerSocket }

constructor TNetServerSocket.Create;
begin
  inherited Create;
  FClients := TNetSocketList.Create(True);
  FLink := TNetSocketLink.Create;
  FLink.OnStateChange := ClientStateChange;
end;

destructor TNetServerSocket.Destroy;
begin
  FDestroying := True;
  FClients.Free;
  inherited Destroy;
end;

procedure TNetServerSocket.Disconnect;
begin
  if FState <> nsDisconnected then
  begin
    closesocket(FSocket);
    InterlockedDecrement(SocketCount);
    FSocket := 0;
    FState := nsDisconnected;
    FListenThread := nil;
    if Assigned(FOnStateChange) then
      FOnStateChange(Self, nsListening, nsDisconnected);
  end;
end;

procedure ListenProc(Thread: TThread);
var
  SocketThread: TNetSocketThread absolute Thread;
  Addr: TSockAddrIn;
  I: Integer;
begin
  I := SizeOf(Addr);
  while True do
  begin
    SocketThread.Data.ClientSocket := accept(SocketThread.Data.Socket, @Addr, @I);
    if SocketThread.Data.ClientSocket = INVALID_SOCKET then Break;
    InterlockedIncrement(SocketCount);
    SocketThread.Data.Server := Addr.sin_addr.S_addr;
    SocketThread.Data.Port := ntohs(Addr.sin_port);
    if not CanAccept(SocketThread) then
    begin
      closesocket(SocketThread.Data.ClientSocket);
      InterlockedDecrement(SocketCount);
    end;
  end;
  SocketThread.Data.State := nsDisconnected;
  CanStateChange(SocketThread);
end;

procedure TNetServerSocket.Listen;
var
  Addr: TSockAddrIn;
begin
  if FState = nsDisconnected then
  begin
    FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
    InterlockedIncrement(SocketCount);
    Addr.sin_family := AF_INET;
    Addr.sin_addr.s_addr := INADDR_ANY;
    Addr.sin_port := htons(Port);
    if bind(FSocket, Addr, SizeOf(Addr)) = SOCKET_ERROR then
    begin
      closesocket(FSocket);
      InterlockedDecrement(SocketCount);
    end
    else if WinSock.listen(FSocket, SOMAXCONN) = SOCKET_ERROR then
    begin
      closesocket(FSocket);
      InterlockedDecrement(SocketCount);
    end
    else
    begin
      FState := nsListening;
      CreateThread(ListenProc, FListenThread);
      if Assigned(FOnStateChange) then
        FOnStateChange(Self, nsDisconnected, nsListening);
    end;
  end;
end;

function TNetServerSocket.GetActive: Boolean;
begin
  Result := FState <> nsDisconnected;
end;

procedure TNetServerSocket.SetActive(Value: Boolean);
begin
  if Value <> (FState <> nsDisconnected) then
    if Value then
      Listen
    else
      Disconnect;
end;

procedure TNetServerSocket.CreateThread(Proc: TThreadExecuteProc; var Thread: TThread);
var
  Data: TNetThreadData;
begin
  Data.Socket := FSocket;
  Data.Port := FPort;
  Data.Synchronizer := Synchronizer;
  Thread := TNetSocketThread.Create(Data, Proc);
end;

procedure TNetServerSocket.ClientStateChange(Sender: TObject; OldState, NewState: TNetState);
var
  Socket: TNetClientSocket absolute Sender;
begin
  if (not FDestroying) and (NewState = nsDisconnected) then
  try
    if Assigned(FOnClientDisconnect) then
      FOnClientDisconnect(Self, Socket);
  finally
    FClients.Remove(Socket);
  end;
end;

function TNetServerSocket.HandleStateChange(Thread: TThread): Boolean;
begin
  if Thread = FListenThread then
    Disconnect;
  Result := False;
end;

function TNetServerSocket.HandleAccept(Thread: TThread; Socket: TSocket; Server: Cardinal; Port: Integer): Boolean;
var
  ClientSocket: TNetClientSocket;
  CanContinue: Boolean;
begin
  CanContinue := (Thread = FListenThread) and (FState = nsListening);
  if CanContinue then
  begin
    ClientSocket := TNetClientSocket.Create;
    FClients.Add(ClientSocket);
    ClientSocket.ServerConnect(Socket, Server, Port);
    ClientSocket.RegisterLink(FLink);
    if Assigned(FOnClientConnect) then
      FOnClientConnect(Self, ClientSocket);
  end;
  Result := CanContinue;
end;

procedure TNetServerSocket.NMThreadEvent(var Message: TNMThreadMessage);
const
  BoolInts: array[Boolean] of Integer = (0, 1);
var
  Data: PNetThreadData;
  Thread: TThread;
  I: Integer;
begin
  Data := Message.Data;
  Thread := Message.Thread;
  I := 0;
  case Data.Event of
    teStateChange: I := BoolInts[HandleStateChange(Thread)];
    teAccept: I := BoolInts[HandleAccept(Thread, Data.ClientSocket, Data.Server, Data.Port)];
  end;
  Message.Result := I;
end;

{ THttpHeaders }

constructor THttpHeaders.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
end;

destructor THttpHeaders.Destroy;
begin
  Clear;
  FLines.Free;
  inherited Destroy;
end;

procedure THttpHeaders.Add(const Key, Value: string);
var
  A, B: string;
  I: Integer;
begin
  A := UpperCase(Key);
  for I := 0 to FLines.Count - 1 do
  begin
    B := UpperCase(FLines[I]);
    if Pos(A, B) > 0 then
    begin
      FLines[I] := Key + ': ' + Value;
      Exit;
    end;
  end;
  FLines.Add(Key + ': ' + Value);
end;

function THttpHeaders.Parse(Buffer: Pointer; BufferSize: Integer): Pointer;
const
  Terminators: array[0..3] of string =
    (#13#10#13#10, #10#13#10#13, #10#10, #13#13);
var
  C: PAnsiChar;
  S: string;
  I, J: Integer;
begin
  C := Buffer;
  if (BufferSize < 1) or (FTopic <> '') then
    Exit
  else
  begin
    Dec(C, FBufferSize);
    if FBuffer = nil then
    begin
      GetMem(FBuffer, BufferSize);
      Move(Buffer^, FBuffer^, BufferSize);
    end
    else
    begin
      ReallocMem(FBuffer, FBufferSize + BufferSize);
      Inc(FBuffer, FBufferSize);
      Move(Buffer^, FBuffer^, BufferSize);
      Dec(FBuffer, FBufferSize);
    end;
    FBufferSize := FBufferSize + BufferSize;
    SetString(S, FBuffer, FBufferSize);
    J := 0;
    for I := Low(Terminators) to High(Terminators) do
    begin
      J := Pos(Terminators[I], S);
      if J > 0 then
      begin
        if Length(S) - (J + Length(Terminators[I])) < 0 then
          C := nil
        else
          Inc(C, J + Length(Terminators[I]) - 1);
        SetLength(S, J - 1);
        SetText(S);
        Break;
      end
    end;
  end;
  Result := C;
end;

procedure THttpHeaders.Clear;
begin
  FTopic := '';
  FLines.Clear;
  if FBuffer <> nil then
    FreeMem(FBuffer);
  FBuffer := nil;
  FBufferSize := 0;
end;

procedure THttpHeaders.Delete(Index: Integer);
begin
  FLines.Delete(Index);
end;

function THttpHeaders.Find(const Key, Value: string): Boolean;
var
  A, B: string;
  I: Integer;
begin
  Result := False;
  A := UpperCase(Key + ': ' + Value);
  for I := 0 to FLines.Count - 1 do
  begin
    B := UpperCase(FLines[I]);
    if A = B then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function THttpHeaders.GetCount: Integer;
begin
  Result := FLines.Count;
end;

function THttpHeaders.Lookup(const Key: string; out Value: string;
  const Default: string = ''): Boolean;
var
  S: string;
  I, J: Integer;
begin
  Value := Default;
  Result := False;
  S := UpperCase(Key + ':');
  for I := 0 to FLines.Count - 1 do
  begin
    J := Pos(S, UpperCase(FLines[I]));
    if J = 1 then
    begin
      Result := True;
      Value := FLines[I];
      Value := Trim(Copy(Value, Length(S) + 1, Length(Value)));
      Break;
    end;
  end;
end;

procedure THttpHeaders.Remove(const Key: string);
var
  S: string;
  I: Integer;
begin
  S := UpperCase(Key + ':');
  for I := 0 to FLines.Count - 1 do
    if Pos(S, UpperCase(FLines[I])) = 1 then
    begin
      FLines.Delete(I);
      Break;
    end;
end;

function THttpHeaders.GetKey(Index: Integer): string;
var
  I: Integer;
begin
  Result := FLines[Index];
  I := Pos(':', Result);
  if I > 0 then
    Result := Trim(Copy(Result, 1, I - 1));
end;

function THttpHeaders.GetValue(Index: Integer): string;
var
  I: Integer;
begin
  Result := FLines[Index];
  I := Pos(':', Result);
  if I > 0 then
    Result := Trim(Copy(Result, I + 1, Length(Result)));
end;

function THttpHeaders.GetText: string;
var
  S: string;
begin
  if FTopic <> '' then
  begin
    Result := Topic + #13#10;
    S := Trim(FLines.Text);
    if S <> '' then
      Result := Result + S + #13#10
    else
      Result := Result + #13#10;
  end
  else
    Result := '';
end;

procedure THttpHeaders.SetText(const Value: string);
var
  S: string;
begin
  S := Trim(Value);
  if S <> '' then
  begin
    FLines.Text := S;
    FTopic := FLines[0];
    FLines.Delete(0);
  end
  else
    Clear;
end;

{ THttpStreamer }

constructor THttpStreamer.Create;
begin
  FHeaders := THttpHeaders.Create;
  FLink := TNetSocketLink.Create;
  FLink.OnDataAvailable := ClientDataAvailable;
  FLink.OnStateChange := ClientStateChange;
  FLink.OnWriteComplete := ClientWriteComplete;
  inherited Create;
end;

destructor THttpStreamer.Destroy;
begin
  FHeaders.Free;
  FLink.Free;
  inherited Destroy;
end;

procedure THttpStreamer.Attach(Socket: TNetClientSocket);
begin
  Socket.RegisterLink(FLink);
end;

procedure THttpStreamer.Detach(Socket: TNetClientSocket);
begin
  Socket.UnregisterLink(FLink);
end;

procedure THttpStreamer.CopyBody(Stream: TStream;
  Ownership: TStreamOwnership);
begin
  ReleaseBody;
  FOwnership := Ownership;
  if FOwnership = soOwned then
    FBody := Stream
  else
  begin
    FBody := TMemoryStream.Create;
    FBody.CopyFrom(Stream, 0);
  end;
  FBody.Position := 0;
end;

procedure THttpStreamer.ReleaseBody;
begin
  if FOwnership = soOwned then
    FBody.Free;
  FBody := nil;
end;

procedure THttpStreamer.Reset;
begin
  FHeaders.Clear;
  ReleaseBody;
end;

procedure THttpStreamer.ClientDataAvailable(Sender: TObject; Data: Pointer;
  DataSize: Integer);
begin
end;

procedure THttpStreamer.ClientStateChange(Sender: TObject; OldState,
  NewState: TNetState);
begin
end;

procedure THttpStreamer.ClientWriteComplete(Sender: TObject);
begin
end;

{ THttpResponseReader }

  (*procedure THttpRequestReader.SocketDataAvailable(Sender: TObject; Data: Pointer;
  DataSize: Integer);
var
  WasParsed: Boolean;
  P: Pointer;
begin
  WasParsed := FHeaders.Parse(Data, DataSize, P);
  if WasParsed then
    if FHeaders.FTopic = '' then
    begin
    
    end;

  begin

//    (FHeaders.FTopic = '') then

  end
  if  then
  begin
  end
  else if
  begin

  end;
end;

procedure THttpRequestReader.SocketStateChange(Sender: TObject; OldState,
  NewState: TNetState);
begin

end;

procedure THttpRequestReader.SocketWriteComplete(Sender: TObject);
begin

end;*)

end.
