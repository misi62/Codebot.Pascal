
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit InetTools;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Registry, WinSock, WinInet, SysTools;


type
  EWinSocketError = class(Exception);

  TSocketAddress = record
    case Integer of
      1: (Address: Longint);
      2: (
        A: Byte;
        B: Byte;
        C: Byte;
        D: Byte);
  end;

  TUniformLocator = record
    Host: string;
    Resource: string;
  end;

function UniformLocator(const Url: string): TUniformLocator;

function HttpRequest(const Locator: TUniformLocator): string;
function HttpResponse: string;

function CheckInternet: Boolean;

{ The HostToAddr function takes a string value such as 'microsoft.com'
  and returns 207.46.250.119 }

function HostToAddr(const Host: string): Cardinal;

{ The AddrToHost function takes a numeric value such as 207.46.250.119
  and returns 'microsoft.com' }

function AddrToHost(Addr: Cardinal): string;

{ The AddrToStr function takes a numeric value such as 207.46.250.119
  and returns a string value of '207.46.250.119' }

function AddrToStr(Addr: Cardinal): string;

{ The AddrToStr function takes a string value such as '207.46.250.119'
  and returns a numeric value of 207.46.250.119 }

function StrToAddr(const Addr: string): Cardinal;

{ The SimpleRequest function }

type
  IRequest = interface(IUnknown)
    ['{9B1E655C-ACD7-4CDA-A167-1EF9CF9F3C0E}']
    function GetDomain: string;
    function GetAllocSize: Cardinal;
    procedure RequestHeader(out Header: string);
    procedure RequestWrite(var Buffer; BufferSize: Cardinal; out BytesWritten: Cardinal);
    procedure ResponseHeader(const Header: string);
    procedure ResponseRead(var Buffer; BufferSize: Cardinal);
    property Domain: string read GetDomain;
    property AllocSize: Cardinal read GetAllocSize;
  end;

function SimpleRequest(const Url: string): string; overload;
function SimpleRequest(Server: Cardinal; const Request: string): string; overload;
function SimpleRequest(const Domain, Request: string): string; overload;
procedure SimpleRequest(Request: IRequest); overload;

const
  WM_SOCKET = WM_USER + $100;
  SD_RECEIVE = 0;
  SD_SEND = 1;
  SD_BOTH = 2;

type
  TWMSocket = packed record
    Msg: Cardinal;
    Socket: TSocket;
    Event: Word;
    Error: Word;
    Result: Longint;
  end;

  TSocketErrorEvent = procedure(Sender: TObject; Operation: string;
    ErrorCode: Integer; var Handled: Boolean) of object;

  TTcpSocket = class(TObject)
  private
    FAddress: Longint;
    FConnected: Boolean;
    FReceiveBuffer: Pointer;
    FReceiveLength: Cardinal;
    FHost: string;
    FPort: Integer;
    FHandle: TSocket;
    FData: Pointer;
    FShuttingDown: Boolean;
    FWindow: TUtilityWindow;
    FOnAccept: TNotifyEvent;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnRead: TNotifyEvent;
    FOnWrite: TNotifyEvent;
    FOnError: TSocketErrorEvent;
    function Error(const Operation: string; ErrorCode: Integer = 0): Integer;
    function GetHandleCreated: Boolean;
    procedure SetReceiveLength(Value: Cardinal);
    procedure WMSocket(var Message: TWMSocket); message WM_SOCKET;
  protected
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure AsyncSelect(Mask: Integer);
  public
    constructor Create; overload;
    constructor Create(Socket: THandle; Address: Longint; Port: Integer); overload;
    destructor Destroy; override;
    procedure Close;
    procedure Connect;
    procedure Listen;
    function Accept: TTcpSocket;
    function Send(Buffer: Pointer; Len: Cardinal): Cardinal; overload;
    function Send(const Text: string): Cardinal; overload;
    function Receive(Buffer: Pointer; Len: Cardinal): Cardinal; overload;
    function Receive(var Text: string): Cardinal; overload;
    property Connected: Boolean read FConnected;
    property Address: Longint read FAddress write FAddress;
    property ReceiveLength: Cardinal read FReceiveLength write SetReceiveLength;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Handle: TSocket read FHandle;
    property HandleCreated: Boolean read GetHandleCreated;
    property Data: Pointer read FData write FData;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnRead: TNotifyEvent read FOnRead write FOnRead;
    property OnWrite: TNotifyEvent read FOnWrite write FOnWrite;
    property OnError: TSocketErrorEvent read FOnError write FOnError;
  end;

{ TInternetBuffer class }

type
  TInternetBuffer = class
  private
    FBlockSize: Integer;
    FBytesAvailable: DWORD;
    FBytesRead: DWORD;
    FData: AnsiString;
    FBlock: Pointer;
    function GetData: Pointer;
    procedure SetBlockSize(const Value: Integer);
    function GetDataSize: Integer;
    function GetAsString: string;
  public
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    property BlockSize: Integer read FBlockSize write SetBlockSize;
    property BytesAvailable: DWORD read FBytesAvailable;
    property BytesRead: DWORD read FBytesRead;
    property Data: Pointer read GetData;
    property DataSize: Integer read GetDataSize;
    property AsString: string read GetAsString;
  end;

{ TInternetSession class }

  TWriteBufferEvent = procedure (Sender: TObject; Data: Pointer; DataSize: Integer;
    var CanWrite: Boolean) of object;

  TInternetSession = class(TObject)
  private
    FAborted: Boolean;
    FSession: HINTERNET;
    FURL: string;
    FBuffer: TInternetBuffer;
    FOnDownload: TNotifyEvent;
    FOnWriteBuffer: TWriteBufferEvent;
    function GetConnected: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abort;
    procedure Connect;
    procedure Disconnect;
    procedure Download;
    property Buffer: TInternetBuffer read FBuffer;
    property Connected: Boolean read GetConnected;
    property URL: string read FURL write FURL;
    property OnDownload: TNotifyEvent read FOnDownload write FOnDownload;
    property OnWriteBuffer: TWriteBufferEvent read FOnWriteBuffer write FOnWriteBuffer;
  end;

{ TFileTransfer }

  TRemoteFindData = record
    Name: string;
    Size: Integer;
    Date: TDateTime;
    Attr: Integer;
  end;

  TFileTransfer = class(TObject)
  private
    FSession: HINTERNET;
    FConnection: HINTERNET;
    FFindHandle: HINTERNET;
    FHost: string;
    FPort: Word;
    FUserName: string;
    FPassword: string;
    FPassive: Boolean;
    procedure RaiseError;
    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    function Connect: Boolean;
    procedure Disconnect;
    function GetCurrectDirectory: string;
    function FindFirst(const Path: string; out FindData: TRemoteFindData): Boolean;
    function FindNext(out FindData: TRemoteFindData): Boolean;
    procedure FindClose;
    function DirectoryExists(const Dir: string): Boolean;
    function FileExists(const FileName: string): Boolean;
    function ChangeDir(const Dir: string): Boolean;
    function MakeDir(const Dir: string): Boolean;
    function RemoveDir(const Dir: string): Boolean;
    function Delete(const FileName: string): Boolean;
    function Rename(const OldName, NewName: string): Boolean;
    function PutFile(const LocalFile, RemoteFile: string): Boolean;
    function GetFile(const RemoteFile, LocalFile: string; Overwrite: Boolean = True): Boolean;
    property Connected: Boolean read GetConnected write SetConnected;
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Passive: Boolean read FPassive write FPassive;
  end;

function Download(const URL: string): string; overload;
procedure Download(const URL, FileName: string); overload;

procedure SocketStartup;
function SocketCheck(ResultCode: Integer; const Operation: string): Integer;

function SocketDownload(const URL: string): string;

procedure EnableProxy(Browser: HWND; Server, Port: string);
procedure DisableProxy(Browser: HWND);

procedure EnableCache;
procedure DisableCache;

implementation

uses
  StrConst;

function SocketCheck(ResultCode: Integer; const Operation: string): Integer;
begin
  Result := ResultCode;
  if Result < 0 then
  begin
    Result := WSAGetLastError;
    raise EWinSocketError.CreateFmt(SWinSocketError, [SysErrorMessage(Result),
      Result, Operation]);
  end;
end;

var
  SocketsStarted: Boolean;

procedure SocketStartup;
var
  Version: Word;
  Data: TWSAData;
begin
  if SocketsStarted then Exit;
  WordRec(Version).Lo := 2;
  WordRec(Version).Hi := 0;
  SocketCheck(WSAStartup(Version, Data), 'WSAStartup');
  SocketsStarted := True;
end;

function UniformLocator(const Url: string): TUniformLocator;
var
  S: string;
  I: Integer;
begin
  S := StringReplace(Trim(Url), ' ', '%20', [rfReplaceAll]);
  I := Pos('://', S);
  if I > 0 then
    S := Copy(S, I + 3, Length(S));
  I := Pos('/', S);
  if I > 0 then
  begin
    Result.Host := Copy(S, 1, I - 1);
    if Length(S) > I then
      Result.Resource := Copy(S, I, Length(S))
    else
      Result.Resource := '/';
  end
  else
  begin
    Result.Host := S;
    Result.Resource := '/';
  end;
end;

function HttpRequest(const Locator: TUniformLocator): string;
const
  Request =
    'GET %s HTTP/1.1'#13#10 +
    'Host: %s'#13#10 +
    'Connection: CLOSE'#13#10 +
    'Accept: */*'#13#10 +
    'Accept-Language: en-us'#13#10 +
    'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0)'#13#10 +
    #13#10;
begin
  Result := Format(Request, [Locator.Resource, Locator.Host]);
end;

function HttpResponse: string;
const
  Response =
    'HTTP/1.1 200 OK'#13#10 +
    'Connection: close'#13#10 +
    'Server: terraserver/1.0'#13#10 +
    'Content-Type: text/html'#13#10 +
    #13#10;
begin
  Result := Response;
end;

function CheckInternet: Boolean;
var
  HostEnt: PHostEnt;
begin
  SocketStartup;
  HostEnt := gethostbyname('google.com');
  Result := HostEnt <> nil;
end;

{function CheckInternet: Boolean;
const
  INTERNET_CONNECTION_CONFIGURED = $40;
  INTERNET_CONNECTION_OFFLINE = $20;
var
  State: DWORD;
  Session, Url: HInternet;
begin
  Result := InternetGetConnectedState(@State, 0);
  if Result and (State and INTERNET_CONNECTION_CONFIGURED = INTERNET_CONNECTION_CONFIGURED) then
  begin
    Session := InternetOpen('IsInternetAvailable',
      INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    try
      Url := InternetOpenURL(Session, 'http://www.google.com', nil, 0,0,0);
      Result := Url <> nil;
      InternetCloseHandle(Url);
    finally
      InternetCloseHandle(Session);
    end;
  end
  else if (State and INTERNET_CONNECTION_OFFLINE =
    INTERNET_CONNECTION_OFFLINE) then
      Result := False;
end;}

function HostToAddr(const Host: string): Cardinal;
var
  HostEnt: PHostEnt;
  S: AnsiString;
begin
  S := UTF8Encode(Host);
  SocketStartup;
  Result := 0;
  HostEnt := gethostbyname(PAnsiChar(S));
  if HostEnt <> nil then
    Result := PLongword(HostEnt^.h_addr_list^)^
  else
    SocketCheck(-1, 'gethostbyname');
end;

function AddrToHost(Addr: Cardinal): string;
var
  HostEnt: PHostEnt;
begin
  SocketStartup;
  HostEnt := gethostbyaddr(@Addr, SizeOf(Addr), AF_INET);
  if HostEnt <> nil then
    Result := HostEnt.h_name
  else
    Result := inet_ntoa(TInAddr(Addr));
end;

function AddrToStr(Addr: Cardinal): string;
begin
  SocketStartup;
  Result := inet_ntoa(TInAddr(Addr));
end;

function StrToAddr(const Addr: string): Cardinal;
var
  S: AnsiString;
begin
  S := UTF8Encode(Addr);
  SocketStartup;
  Result := inet_addr(PAnsiChar(S));
end;

function SimpleRequest(const Url: string): string; overload;
var
  U: TUniformLocator;
begin
  U := UniformLocator(Url);
  Result := SimpleRequest(U.Host, HttpRequest(U));
end;

function SimpleRequest(Server: Cardinal; const Request: string): string; overload;
const
  DefPort = 80;
  BufferSize = 1024 * 128;
var
  ResultCode: Integer;
  Sock: TSocket;
  Addr: TSockAddrIn;
  A, B: Integer;
  P: PChar;
  //S: string;
begin
  Result := '';
  SocketStartup;
  Sock := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  try
    Addr.sin_family := AF_INET;
    Addr.sin_addr.s_addr := Server;
    Addr.sin_port := htons(DefPort);
    ResultCode := connect(Sock, Addr, SizeOf(Addr));
    SocketCheck(ResultCode, 'connect');
    A := 0;
    B := Length(Request);
    P := PChar(Request);
    while A < B do
    begin
      ResultCode := send(Sock, P^, B - A, 0);
      SocketCheck(ResultCode, 'send');
      Inc(A, ResultCode);
    end;
    GetMem(P, BufferSize);
    try
      A := 0;
      repeat
        ResultCode := recv(Sock, P^, BufferSize, 0);
        SocketCheck(ResultCode, 'recv');
        if ResultCode > 0 then
        begin
          //SetString(S, P, ResultCode);
          //Result := Result + S;
          B := ResultCode;
          SetLength(Result, A + B);
          Move(P^, PChar(@Result[A + 1])^, B);
          Inc(A, B);
        end;
      until ResultCode < 1;
    finally
      FreeMem(P);
    end;
  finally
    closesocket(Sock);
  end;
end;

function SimpleRequest(const Domain, Request: string): string;
begin
  Result := SimpleRequest(HostToAddr(Domain), Request);
end;

procedure SimpleRequest(Request: IRequest);
const
  DefPort = 80;
var
  Domain: string;
  BufferSize: Cardinal;
  ResultCode: Integer;
  Sock: TSocket;
  Addr: TSockAddrIn;
  A, B: PChar;
  P: Pointer;
  S: string;
  I: Cardinal;
begin
  Domain := Request.Domain;
  BufferSize := Request.AllocSize;
  SocketStartup;
  Sock := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  GetMem(P, BufferSize);
  try
    Addr.sin_family := AF_INET;
    Addr.sin_addr.s_addr := HostToAddr(Domain);
    Addr.sin_port := htons(DefPort);
    ResultCode := connect(Sock, Addr, SizeOf(Addr));
    SocketCheck(ResultCode, 'connect');
    Request.RequestHeader(S);
    ResultCode := send(Sock, PChar(S)^, Length(S), 0);
    SocketCheck(ResultCode, 'send');
    I := 0;
    repeat
      Request.RequestWrite(P^, BufferSize, I);
      if I > 0 then
      begin
        ResultCode := send(Sock, P, I, 0);
        SocketCheck(ResultCode, 'send');
      end;
    until I = 0;
    FillChar(P^, BufferSize, #0);
    ResultCode := recv(Sock, P^, BufferSize, 0);
    SocketCheck(ResultCode, 'recv');
    A := PChar(P);
    B := AnsiStrPos(A, #13#10#13#10);
    Inc(B, 4);
    if B < A then Exit;
    I := Integer(B - A);
    SetString(S, A, I);
    Request.ResponseHeader(S);
    if I < BufferSize then
      Request.ResponseRead(B^, BufferSize - I);
    repeat
      ResultCode := recv(Sock, P^, BufferSize, 0);
      SocketCheck(ResultCode, 'recv');
      if ResultCode > 0 then
        Request.ResponseRead(B^, ResultCode);
    until ResultCode < 1;
  finally
    FreeMem(P);
    closesocket(Sock);
  end;
end;

{ TTcpSocket }

constructor TTcpSocket.Create;
begin
  SocketStartup;
  FWindow := TUtilityWindow.Create(Self);
  FHandle := INVALID_SOCKET;
  ReceiveLength := 50000;
end;

constructor TTcpSocket.Create(Socket: THandle; Address: Longint; Port: Integer);
begin
  Create;
  FHandle := Socket;
  if HandleCreated then
  begin
    FConnected := True;
    AsyncSelect(FD_READ or FD_WRITE or FD_CLOSE);
  end;
  FAddress := Address;
  FPort := Port;
end;

destructor TTcpSocket.Destroy;
begin
  Close;
  ReceiveLength := 0;
  FWindow.Free;
end;

procedure TTcpSocket.CreateHandle;
begin
  if FHandle = INVALID_SOCKET then
  begin
    FHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
    AsyncSelect(0);
  end;
end;

procedure TTcpSocket.DestroyHandle;
begin
  if HandleCreated then
  begin
    AsyncSelect(0);
    closesocket(FHandle);
    FHandle := INVALID_SOCKET;
    if FConnected and Assigned(FOnDisconnect) then
    begin
      FConnected := False;
      FOnDisconnect(Self);
    end
    else
      FConnected := False;
  end;
end;

procedure TTcpSocket.AsyncSelect(Mask: Integer);
begin
  if HandleCreated then
    WSAAsyncSelect(FHandle, FWindow.Handle, WM_SOCKET, Mask);
end;

function TTcpSocket.Error(const Operation: string; ErrorCode: Integer = 0): Integer;
var
  Handled: Boolean;
begin
  Handled := False;
  if ErrorCode = 0 then
    Result := WSAGetLastError
  else
    Result := ErrorCode;
  if Result <>  WSAEWOULDBLOCK then
  try
    if Assigned(FOnError) then
      FOnError(Self, Operation, Result, Handled);
    if not Handled then
      raise EWinSocketError.CreateFmt(SWinSocketError, [SysErrorMessage(Result),
        Result, Operation]);
  finally
    Close;
  end;
end;

procedure TTcpSocket.Close;
begin
  DestroyHandle;
end;

procedure TTcpSocket.Connect;
var
  Addr: TSockAddrIn;
begin
  Close;
  try
    Addr.sin_family := AF_INET;
    if FAddress <> 0 then
      Addr.sin_addr.s_addr := FAddress
    else
      Addr.sin_addr.s_addr := HostToAddr(FHost);
    Addr.sin_port := htons(FPort);
  except
    Error('lookup');
    Exit;
  end;
  CreateHandle;
  if HandleCreated then
  begin
    AsyncSelect(FD_CONNECT or FD_CLOSE);
    if WinSock.connect(FHandle, Addr, SizeOf(Addr)) = SOCKET_ERROR then
      Error('connect');
  end;
end;

procedure TTcpSocket.Listen;
var
  Addr: TSockAddrIn;
begin
  Close;
  FHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if HandleCreated then
  begin
    FConnected := True;
    Addr.sin_family := AF_INET;
    Addr.sin_addr.s_addr := INADDR_ANY;
    Addr.sin_port := htons(Port);
    if WinSock.bind(FHandle, Addr, SizeOf(Addr)) = SOCKET_ERROR then
      Error('bind')
    else if WinSock.listen(FHandle, SOMAXCONN) = SOCKET_ERROR then
      Error('listen')
    else
      AsyncSelect(FD_ACCEPT);
  end;
end;

function TTcpSocket.Accept: TTcpSocket;
var
  Addr: TSockAddrIn;
  Len: Integer;
  S: TSocket;
begin
  Result := nil;
  if HandleCreated then
  begin
    Len := SizeOf(Addr);
    S := WinSock.accept(FHandle, @Addr, @Len);
    if S <> INVALID_SOCKET then
      Result := TTcpSocket.Create(S, Addr.sin_addr.s_addr, ntohs(Addr.sin_port));
  end;
end;

function TTcpSocket.Send(Buffer: Pointer; Len: Cardinal): Cardinal;
var
  ErrorCode: Integer;
  Bytes: Integer;
begin
  Result := 0;
  if HandleCreated then
  begin
    ErrorCode := 0;
    Bytes := WinSock.send(FHandle, Buffer^, Len, 0);
    if Bytes = SOCKET_ERROR then
     begin
      ErrorCode := Error('send');
      Bytes := 0;
    end;
    Result := Bytes;
    if (Bytes = 0) and (ErrorCode <> WSAEWOULDBLOCK) then
      Close;
  end;
end;

function TTcpSocket.Send(const Text: string): Cardinal;
begin
  Result := Send(Pointer(Text), Length(Text));
end;

function TTcpSocket.Receive(Buffer: Pointer; Len: Cardinal): Cardinal;
var
  ErrorCode: Integer;
  Bytes: Integer;
begin
  Result := 0;
  if HandleCreated then
  begin
    ErrorCode := 0;
    Bytes := WinSock.recv(FHandle, Buffer^, Len, 0);
    if Bytes = SOCKET_ERROR then
    begin
      ErrorCode := Error('recv');
      Bytes := 0;
    end;
    Result := Bytes;
    if FShuttingDown then
      FShuttingDown := Bytes > 0
    else if (Bytes = 0) and (ErrorCode <> WSAEWOULDBLOCK) then
      Close;
  end;
end;

function TTcpSocket.Receive(var Text: string): Cardinal;
begin
  Result := 0;
  if FReceiveLength > 0 then
    Result := Receive(FReceiveBuffer, FReceiveLength);
  if Result > 0 then
    SetString(Text, PChar(FReceiveBuffer), Result)
  else
    Text := '';
end;

procedure TTcpSocket.WMSocket(var Message: TWMSocket);

  procedure Invoke(Event: TNotifyEvent);
  begin
    if Assigned(Event) then Event(Self);
  end;

begin
  if Message.Error <> 0 then
  case Message.Event of
    FD_READ: Error('recv', Message.Error);
    FD_WRITE: Error('send', Message.Error);
    FD_ACCEPT: Error('accept', Message.Error);
    FD_CONNECT: Error('connect', Message.Error);
    FD_CLOSE: Error('close', Message.Error);
  else
    Error('unknown', Message.Error);
  end
  else
  case Message.Event of
    FD_READ: Invoke(FOnRead);
    FD_WRITE: Invoke(FOnWrite);
    FD_ACCEPT: Invoke(FOnAccept);
    FD_CONNECT:
      begin
        FConnected := True;
        AsyncSelect(FD_READ or FD_WRITE or FD_CLOSE);
        Invoke(FOnConnect);
      end;
    FD_CLOSE:
      begin
        shutdown(FHandle, SD_SEND);
        FShuttingDown := True;
        if Assigned(FOnRead) then
          while FShuttingDown do Invoke(FOnRead);
        Close;
      end;
  end;
end;

function TTcpSocket.GetHandleCreated: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

procedure TTcpSocket.SetReceiveLength(Value: Cardinal);
begin
  if Value <> FReceiveLength then
  begin
    if FReceiveLength > 0 then
      FreeMem(FReceiveBuffer);
    FReceiveLength := Value;
    if FReceiveLength > 0 then
      GetMem(FReceiveBuffer, FReceiveLength);
  end;
end;

{ TInternetBuffer }

destructor TInternetBuffer.Destroy;
begin
  Clear;
  BlockSize := 0;
  inherited Destroy;
end;

procedure TInternetBuffer.Clear;
begin
  FBytesAvailable := 0;
  FBytesRead := 0;
  FData := '';
end;

procedure TInternetBuffer.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  if FData = '' then
    Exit;
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TInternetBuffer.SaveToStream(Stream: TStream);
begin
  if FData = '' then
    Exit;
  Stream.Write(PAnsiChar(FData)^, Length(FData));
end;

procedure TInternetBuffer.SetBlockSize(const Value: Integer);
begin
  if Value <> FBlockSize then
  begin
    FBlockSize := Value;
    if FBlock <> nil then
      FreeMem(FBlock);
    FBlock := nil;
    if FBlockSize > 0 then
      GetMem(FBlock, FBlockSize);
  end;
end;

function TInternetBuffer.GetData: Pointer;
begin
  Result := Pointer(FData);
end;

function TInternetBuffer.GetDataSize: Integer;
begin
  Result := Length(FData);
end;

function TInternetBuffer.GetAsString;
begin
  Result := FData;
end;

{ TInternetSession }

constructor TInternetSession.Create;
begin
  inherited Create;
  FBuffer := TInternetBuffer.Create;
  FBuffer.BlockSize := 1024;
end;

destructor TInternetSession.Destroy;
begin
  FBuffer.Free;
  Disconnect;
  inherited Destroy;
end;

procedure TInternetSession.Abort;
begin
  FAborted := True;
end;

procedure TInternetSession.Connect;
var
  S: string;
begin
  if FSession = nil then
  begin
    S := ClassName;
    FSession := InternetOpen(PChar(S), INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
    if FSession = nil then
      RaiseLastWin32Error;
  end;
end;

procedure TInternetSession.Disconnect;
begin
  if FSession <> nil then
  begin
    InternetCloseHandle(FSession);
    FSession := nil
  end;
end;

procedure TInternetSession.Download;

  function GetBytesAvailable(Connection: HINTERNET): DWORD;
  var
    Buffer, BufferSize, Reserved: DWORD;
  begin
     BufferSize := SizeOf(Buffer);
     Reserved := 0;
     if HttpQueryInfo(Connection, HTTP_QUERY_CONTENT_LENGTH or
        HTTP_QUERY_FLAG_NUMBER, @Buffer, BufferSize, Reserved) then
       Result := Buffer
     else
       Result := 0;
  end;

var
  Connection: HINTERNET;
  BytesRead: Cardinal;
  CanWrite: Boolean;
  P: PAnsiChar;
begin
  FAborted := False;
  FBuffer.Clear;
  if FURL = '' then Exit;
  Connection := InternetOpenUrl(FSession, PChar(FURL), nil, 0,
    INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS or INTERNET_FLAG_RELOAD, 0);
  if Connection = nil then
    RaiseLastOSError;
  try
    FBuffer.FBytesAvailable := GetBytesAvailable(Connection);
    InternetReadFile(Connection, FBuffer.FBlock, FBuffer.FBlockSize, BytesRead);
    while (not FAborted) and (BytesRead > 0) do
    begin
      Inc(FBuffer.FBytesRead, BytesRead);
      CanWrite := True;
      if Assigned(FOnWriteBuffer) then
        FOnWriteBuffer(Self, FBuffer.FBlock, BytesRead, CanWrite);
      if CanWrite then
      begin
        {$WARNINGS OFF}
        SetLength(FBuffer.FData, Length(FBuffer.FData) + BytesRead);
        P := PAnsiChar(Integer(FBuffer.FData) + Length(FBuffer.FData) - BytesRead);
        {$WARNINGS ON}
        Move(PAnsiChar(FBuffer.FBlock)^, PAnsiChar(P)^, BytesRead);
      end;
      InternetReadFile(Connection, FBuffer.FBlock, FBuffer.FBlockSize, BytesRead);
    end;
  finally
    InternetCloseHandle(Connection);
  end;
end;

function TInternetSession.GetConnected: Boolean;
begin
  Result := FSession <> nil;
end;

{ TFileTransfer }

constructor TFileTransfer.Create;
begin
  inherited Create;
  Port := 21;
  Passive := true;
end;

destructor TFileTransfer.Destroy;
begin
  Connected := False;
  inherited Destroy;
end;

function TFileTransfer.GetConnected: Boolean;
begin
  Result := FSession <> nil;
end;

procedure TFileTransfer.RaiseError;
begin
{var
  Error, Size: Cardinal;
  S: string;
begin
  Count := 1024;
  SetLength(S, Size);
  if InternetGetLastResponseInfo(Error, S, Size) then
  else}
  RaiseLastWin32Error;
  {if LastError <> 0 then
    Error := EOSError.CreateResFmt(@SOSError, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EOSError.CreateRes(@SUnkOSError);
  Error.ErrorCode := LastError;
  raise Error;}
end;

procedure TFileTransfer.SetConnected(Value: Boolean);
var
  P: Cardinal;
begin
  if Value and (FSession = nil) then
  begin
    FSession := InternetOpen(nil, INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);
    if FSession = nil then
      RaiseError;
    if FPassive then
      P := INTERNET_FLAG_PASSIVE
    else
      P := 0;
    FConnection := InternetConnect(FSession, PChar(FHost), FPort,
      PChar(FUserName), PChar(FPassword), INTERNET_SERVICE_FTP, P, 0);
    if FConnection = nil then
    begin
      InternetCloseHandle(FSession);
      RaiseError;
    end;
  end
  else if (not Value) and (FSession <> nil) then
  begin
    FindClose;
    InternetCloseHandle(FConnection);
    FConnection := nil;
    InternetCloseHandle(FSession);
    FSession := nil;
  end;
end;

(*var
  Connection: HINTERNET;
  BytesRead: Cardinal;
  CanWrite: Boolean;
  P: PChar;
begin
  FAborted := False;
  FBuffer.Clear;
  if FURL = '' then Exit;
  Connection := InternetOpenUrl(FSession, PChar(FURL), nil, 0,
    INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS or INTERNET_FLAG_RELOAD, 0);
  if Connection <> nil then
  try
    FBuffer.FBytesAvailable := GetBytesAvailable(Connection);
    InternetReadFile(Connection, FBuffer.FBlock, FBuffer.FBlockSize, BytesRead);
    while (not FAborted) and (BytesRead > 0) do
    begin
      Inc(FBuffer.FBytesRead, BytesRead);
      CanWrite := True;
      if Assigned(FOnWriteBuffer) then
        FOnWriteBuffer(Self, FBuffer.FBlock, BytesRead, CanWrite);
      if CanWrite then
      begin
        {$WARNINGS OFF}
        SetLength(FBuffer.FData, Length(FBuffer.FData) + BytesRead);
        P := PChar(Integer(FBuffer.FData) + Length(FBuffer.FData) - BytesRead);
        {$WARNINGS ON}
        Move(PChar(FBuffer.FBlock)^, PChar(P)^, BytesRead);
      end;
      InternetReadFile(Connection, FBuffer.FBlock, FBuffer.FBlockSize, BytesRead);

  end;
end;*)

function TFileTransfer.Connect: Boolean;
begin
  Connected := True;
  Result := Connected;
end;

procedure TFileTransfer.Disconnect;
begin
  Connected := False;
end;

function TFileTransfer.GetCurrectDirectory: string;
var
  S: string;
  I: Cardinal;
begin
  if Connected then
  begin
    I := MAX_PATH;
    SetLength(S, I);
    if FtpGetCurrentDirectory(FConnection, PChar(S), I) then
      Result := StrPas(PChar(@S[1]));
  end
  else
    Result := '';
end;

function FileTimeToDateTime(FileTime: TFileTime) : TDateTime;
var
  LocalTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := EncodeDate(1900,1,1);
  if FileTimeToLocalFileTime(FileTime, LocalTime) and
    FileTimeToSystemTime(LocalTime, SystemTime) then
  Result := SystemTimeToDateTime(SystemTime);
end;

procedure CopyFindData(const Data: TWin32FindData; out F: TRemoteFindData);
begin
  with F do
  begin
    Name := Data.cFileName;
    Size := Data.nFileSizeLow;
    Date := FileTimeToDateTime(Data.ftLastWriteTime);
    Attr := Data.dwFileAttributes;
  end;
end;

function TFileTransfer.FindFirst(const Path: string; out FindData: TRemoteFindData): Boolean;
var
  D: TWin32FindData;
begin
  Result := False;
  if not Connected then Exit;
  FindClose;
  FFindHandle := FtpFindFirstFile(FConnection, PChar(Path), D, 0, 0);
  Result := FFindHandle <> nil;
  if Result then
    CopyFindData(D, FindData)
  else if GetLastError = ERROR_INTERNET_EXTENDED_ERROR then
    RaiseError;
end;

function TFileTransfer.FindNext(out FindData: TRemoteFindData): Boolean;
var
  D: TWin32FindData;
begin
  Result := False;
  if not Connected then Exit;
  if FFindHandle <> nil then
  begin
    Result := InternetFindNextFile(FFindHandle, @D);
    if Result then
      CopyFindData(D, FindData)
  end
  else
    Result := FindFirst('', FindData);
end;

procedure TFileTransfer.FindClose;
begin
  if not Connected then Exit;
  if FFindHandle <> nil then
    InternetCloseHandle(FFindHandle);
  FFindHandle := nil;
end;

function TFileTransfer.DirectoryExists(const Dir: string): Boolean;
var
  D: TRemoteFindData;
begin
  if (Dir = '') or (Dir = '/') then
    Result := True
  else
    Result := FindFirst(Dir, D) and (D.Attr and faDirectory  = faDirectory);
  FindClose;
end;

function TFileTransfer.FileExists(const FileName: string): Boolean;
var
  D: TRemoteFindData;
begin
  Result := FindFirst(FileName, D) and (D.Attr and faDirectory  = 0);
  FindClose;
end;

function TFileTransfer.ChangeDir(const Dir: string): Boolean;
begin
  if Dir = '' then
    Result := True
  else
    Result := Connected and FtpSetCurrentDirectory(FConnection, PChar(Dir));
end;

function TFileTransfer.MakeDir(const Dir: string): Boolean;
begin
  if (Dir = '') or (Dir = '/') then
    Result :=  True
  else
    Result := Connected and FtpCreateDirectory(FConnection, PChar(Dir));
end;

function TFileTransfer.RemoveDir(const Dir: string): Boolean;
begin
  Result := Connected and FtpRemoveDirectory(FConnection, PChar(Dir));
end;

function TFileTransfer.Delete(const FileName: string): Boolean;
begin
  Result := Connected and FtpDeleteFile(FConnection, PChar(FileName));
end;

function TFileTransfer.Rename(const OldName, NewName: string): Boolean;
begin
  Result := Connected and FtpRenameFile(FConnection, PChar(OldName), PChar(NewName));
end;

function TFileTransfer.PutFile(const LocalFile, RemoteFile: string): Boolean;
begin
  Result := Connected and FtpPutFile(FConnection, PChar(LocalFile), PChar(RemoteFile), FTP_TRANSFER_TYPE_BINARY, 0);
end;

function TFileTransfer.GetFile(const RemoteFile, LocalFile: string; Overwrite: Boolean = True): Boolean;
begin
  Result := Connected and FtpGetFile(FConnection, PChar(RemoteFile), PChar(LocalFile), not Overwrite, FILE_ATTRIBUTE_NORMAL, FTP_TRANSFER_TYPE_BINARY, 0);
end;

function Download(const URL: string): string;
var
  Session: TInternetSession;
begin
  Session := TInternetSession.Create;
  try
    Session.URL := URL;
    Session.Connect;
    Session.Download;
    Result := Session.Buffer.AsString;
    Session.Disconnect;
  finally
    Session.Free;
  end;
end;


procedure Download(const URL, FileName: string);
var
  Session: TInternetSession;
begin
  Session := TInternetSession.Create;
  try
    Session.URL := URL;
    Session.Connect;
    Session.Download;
    Session.Buffer.SaveToFile(FileName);
    Session.Disconnect;
  finally
    Session.Free;
  end;
end;

function SocketDownload(const URL: string): string;
var
  Socket: TTcpSocket;
begin
  Socket := TTcpSocket.Create;
  try
  finally
    Socket.Free;
  end;
end;

procedure EnableProxy(Browser: HWND; Server, Port: string);
var
  Buffer: array [0..1024] of Byte;
  R: TRegistry;
  I: Integer;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', False);
    R.WriteInteger('ProxyEnable', 1);
    R.WriteString('ProxyServer', Server + ':' + Port);
    R.WriteString('ProxyOverride', '*.local;<local>');
    if R.OpenKey('Connections', False) then
    begin
      I := R.ReadBinaryData('DefaultConnectionSettings', Buffer, SizeOf(Buffer));
      if (I > 8) and (I < SizeOf(Buffer)) then
      begin
        Buffer[4] := Buffer[4] + 1;
        Buffer[8] := 3;
        R.WriteBinaryData('DefaultConnectionSettings', Buffer, I);
      end;
      if R.ValueExists('SavedLegacySettings') then
      begin
        I := R.ReadBinaryData('SavedLegacySettings', Buffer, SizeOf(Buffer));
        if (I > 8) and (I < SizeOf(Buffer)) then
        begin
          Buffer[4] := Buffer[4] + 1;
          Buffer[8] := 3;
          R.WriteBinaryData('SavedLegacySettings', Buffer, I);
        end;
      end;
    end;
    R.RootKey := HKEY_CURRENT_CONFIG;
    R.OpenKey('Software\Microsoft\windows\CurrentVersion\Internet Settings', True);
    R.WriteInteger('ProxyEnable', 1);
  finally
    R.Free;
  end;
  InternetSetOption(nil, INTERNET_OPTION_SETTINGS_CHANGED, nil, 0);
  InternetSetOption(nil, INTERNET_OPTION_REFRESH, nil, 0);
  if Browser <> 0 then
    SendMessage(Browser, WM_SETTINGCHANGE, 0, 0);
end;

procedure DisableProxy(Browser: HWND);
var
  Buffer: array [0..1024] of Byte;
  R: TRegistry;
  I: Integer;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', True);
    R.WriteInteger('ProxyEnable', 0);
    R.WriteString('ProxyServer', '');
    R.WriteString('ProxyOverride', '*.local');
    if R.OpenKey('Connections', False) then
    begin
      I := R.ReadBinaryData('DefaultConnectionSettings', Buffer, SizeOf(Buffer));
      if (I > 8) and (I < SizeOf(Buffer)) then
      begin
        Buffer[4] := Buffer[4] + 1;
        Buffer[8] := 1;
        R.WriteBinaryData('DefaultConnectionSettings', Buffer, I);
      end;
      if R.ValueExists('SavedLegacySettings') then
      begin
        I := R.ReadBinaryData('SavedLegacySettings', Buffer, SizeOf(Buffer));
        if (I > 8) and (I < SizeOf(Buffer)) then
        begin
          Buffer[4] := Buffer[4] + 1;
          Buffer[8] := 1;
          R.WriteBinaryData('SavedLegacySettings', Buffer, I);
        end;
      end;
    end;
    R.RootKey := HKEY_CURRENT_CONFIG;
    R.OpenKey('Software\Microsoft\windows\CurrentVersion\Internet Settings', True);
    R.WriteInteger('ProxyEnable', 0);
  finally
    R.Free;
  end;
  InternetSetOption(nil, INTERNET_OPTION_SETTINGS_CHANGED, nil, 0);
  InternetSetOption(nil, INTERNET_OPTION_REFRESH, nil, 0);
  if Browser <> 0 then
    SendMessage(Browser, WM_SETTINGCHANGE, 0, 0);
end;

procedure EnableCache;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', True);
    R.WriteInteger('SyncMode5', 4);
  finally
    R.Free;
  end;
end;

procedure DisableCache;
var
  R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKey('Software\Microsoft\Windows\CurrentVersion\Internet Settings', True);
    R.WriteInteger('SyncMode5', 3);
  finally
    R.Free;
  end;
end;


{
function LoadPage(Host: string): string;
var
  HostEnt: PHostEnt;
  Addr: TSockAddrIn;
  Sock: TSocket;
  B: array[0..1024] of Char;
  I: Integer;
  S: string;
begin
  SocketStartup;
  HostEnt := gethostbyname(PChar(Host));
  if HostEnt <> nil then
    Addr.sin_addr.s_addr := PLongword(HostEnt^.h_addr_list^)^
  else
    SocketCheck(1, 'gethostbyname');
  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(80);
  Sock := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  SocketCheck(connect(Sock, Addr, SizeOf(Addr)), 'connect');
  S := 'GET / HTTP/1.1'#13#10 +
    'Host: ' + Host + #13#10#13#10;
  send(Sock, PChar(S)^, Length(S), 0);
  Result := '';
  I := SizeOf(B);
  while I = SizeOf(B) do
  begin
    Sleep(100);
    I := recv(Sock, B, SizeOf(B), 0);
    if I > 0 then
    begin
      SetString(S, B, I);
      Result := Result + S;
    end;
  end;
  closesocket(Sock);
end;

}

const
   winetdll = 'wininet.dll';

const
  GROUP_OWNER_STORAGE_SIZE = 4;
  GROUPNAME_MAX_LENGTH = 120;
  CACHEGROUP_SEARCH_ALL = $00000000;
  CACHEGROUP_FLAG_FLUSHURL_ONDELETE = $00000002;


type
   PInternetCacheTimeStamps = ^TInternetCacheTimeStamps;
   TInternetCacheTimeStamps = record
      ftExpires: TFileTime;
      ftLastModified: TFileTime;
   end;
   PInternetCacheGroupInfo = ^TInternetCacheGroupInfo;
   TInternetCacheGroupInfo = record
      dwGroupSize: DWORD;
      dwGroupFlags: DWORD;
      dwGroupType: DWORD;
      dwDiskUsage: DWORD;
      dwDiskQuota: DWORD;
      dwOwnerStorage: array[0..GROUP_OWNER_STORAGE_SIZE - 1] of DWORD;
      szGroupName: array[0..GROUPNAME_MAX_LENGTH - 1] of AnsiChar;
   end;
   TEntryInfo = record
      SourceUrlName: string;
      LocalFileName: string;
      EntryType: DWORD;
      UseCount: DWORD;
      HitRate: DWORD;
      FSize: DWORD;
      LastModifiedTime: TDateTime;
      ExpireTime: TDateTime;
      LastAccessTime: TDateTime;
      LastSyncTime: TDateTime;
      HeaderInfo: string;
      FileExtension: string;
      ExemptDelta: DWORD;
   end;
   TGroupInfo = record
      DiskUsage: DWORD;
      DiskQuota: DWORD;
      OwnerStorage: array[0..GROUP_OWNER_STORAGE_SIZE - 1] of DWORD;
      GroupName: string;
   end;
   TContent = record
      Buffer: Pointer;
      BufferLength: Integer;
   end;
   TFilterOption = (NORMAL_ENTRY,
      STABLE_ENTRY,
      STICKY_ENTRY,
      COOKIE_ENTRY,
      URLHISTORY_ENTRY,
      TRACK_OFFLINE_ENTRY,
      TRACK_ONLINE_ENTRY,
      SPARSE_ENTRY,
      OCX_ENTRY);


function FindFirstUrlCacheGroup(dwFlags, dwFilter: DWORD;
      lpSearchCondition: Pointer; dwSearchCondition: DWORD;
      var Group: Int64; lpReserved: Pointer): THandle; stdcall; external winetdll;

function FindNextUrlCacheGroup(hFind: THandle; var GroupID: Int64; lpReserved: Pointer): BOOL; stdcall; external winetdll;

function SetUrlCacheGroupAttribute(gid: Int64; dwFlags, dwAttributes: DWORD; var lpGroupInfo: TInternetCacheGroupInfo;
      lpReserved: Pointer): BOOL; stdcall; external winetdll;

function GetUrlCacheGroupAttribute(gid: Int64; dwFlags, dwAttributes: DWORD;
      var GroupInfo: TInternetCacheGroupInfo; var dwGroupInfo: DWORD; lpReserved: Pointer): BOOL; stdcall; external winetdll;

procedure ClearCache;
var
  enumHandle: THandle;
  groupId: Int64;
  returnValue: BOOL;
  cacheEntryInfoBufferSizeInitial: Cardinal;
  cacheEntryInfoBufferSize: Cardinal;
  cacheEntryInfoBuffer: Pointer;
begin
  cacheEntryInfoBufferSizeInitial := 0;

  enumHandle := FindFirstUrlCacheGroup(0, CACHEGROUP_SEARCH_ALL, nil, 0, groupId, nil);
  if (enumHandle <> 0) and (ERROR_NO_MORE_ITEMS = GetLastError) then
    Exit;
  while true do
  begin
    returnValue := DeleteUrlCacheGroup(groupId, CACHEGROUP_FLAG_FLUSHURL_ONDELETE, nil);
                if (not returnValue) and (ERROR_FILE_NOT_FOUND = GetLastError) then
                begin
                    returnValue := FindNextUrlCacheGroup(enumHandle, groupId, nil);
                end;

                if (not returnValue) and ((ERROR_NO_MORE_ITEMS = GetLastError) or (ERROR_FILE_NOT_FOUND = GetLastError)) then
                    break;
  end;
  enumHandle := FindFirstUrlCacheEntry(nil, PInternetCacheEntryInfo(nil)^, cacheEntryInfoBufferSizeInitial);
  if (enumHandle <> 0) and (ERROR_NO_MORE_ITEMS = GetLastError) then
    Exit;
  cacheEntryInfoBufferSize := cacheEntryInfoBufferSizeInitial;
  GetMem(cacheEntryInfoBuffer, cacheEntryInfoBufferSize);
  enumHandle := FindFirstUrlCacheEntry(nil, PInternetCacheEntryInfo(cacheEntryInfoBuffer)^, cacheEntryInfoBufferSizeInitial);

            while(true) do
            begin
                //internetCacheEntry = (INTERNET_CACHE_ENTRY_INFOA)Marshal.PtrToStructure(cacheEntryInfoBuffer, typeof(INTERNET_CACHE_ENTRY_INFOA));

                cacheEntryInfoBufferSizeInitial := cacheEntryInfoBufferSize;
                returnValue := DeleteUrlCacheEntry(PInternetCacheEntryInfo(cacheEntryInfoBuffer).lpszSourceUrlName);
                if (not returnValue) then
                begin
                    returnValue := FindNextUrlCacheEntry(enumHandle, PInternetCacheEntryInfo(cacheEntryInfoBuffer)^, cacheEntryInfoBufferSizeInitial);
                end;
                if (not returnValue) and (ERROR_NO_MORE_ITEMS = GetLastError) then
                  Break;
                if (not returnValue) and (cacheEntryInfoBufferSizeInitial > cacheEntryInfoBufferSize) then
                begin
                    cacheEntryInfoBufferSize := cacheEntryInfoBufferSizeInitial;
                    FreeMem(cacheEntryInfoBuffer);
                    GetMem(cacheEntryInfoBuffer, cacheEntryInfoBufferSize);
                    FindNextUrlCacheEntry(enumHandle, PInternetCacheEntryInfo(cacheEntryInfoBuffer)^, cacheEntryInfoBufferSizeInitial);
                end;
            end;
            FreeMem(cacheEntryInfoBuffer);

end;

initialization
  SocketsStarted := False;
finalization
  if SocketsStarted then
    WSACleanup;
end.

