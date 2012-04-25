
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit ShlTools;

interface

{$I CODEBOT.INC}

uses
	Windows, Messages, ActiveX, ShellAPI, ShlObj, SysUtils, Classes, ComObj;

{ FileOperation function }

const
  foCopy = FO_COPY;
  foDelete = FO_DELETE;
  foMove = FO_MOVE;
  foRename = FO_RENAME;

type
  TOperationOption = (ooAllowUndo, ooConfirmMouse, ooFileOnly, ooMultiDestFile,
    ooNoConfirmation, ooNoConfirmMkDir, ooRenameCollision, ooSilent,
    ooSimpleProgress);
  TOperationOptions = set of TOperationOption;

function FileOperation(Window: HWND; const Source, Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooSimpleProgress]): Boolean; overload;
function FileOperation(Window: HWND; const Source: TStrings; const Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooSimpleProgress]): Boolean; overload;
procedure FileDelete(FileName: string; Recycle: Boolean = False);

{ The place where you should be storing settings }

function GetSettingsFolder(const AppName: string; Create: Boolean = False): string;
function GetSettingsFile(const AppName, FileName: string): string;

{ The place where you should be storing useable data }

function GetApplicationFolder(const AppName: string; Create: Boolean = False): string;
function GetApplicationFile(const AppName, FileName: string): string;

{ BrowseForFolder}

function BrowseForFolder(const HelpText: string; var Directory: string): Boolean;

{ ExecuteVerb procedure }

procedure ExecuteVerb(Handle: HWND; ItemList: PItemIDList; const Verb: string); overload;
procedure ExecuteVerb(Handle: HWND; const DisplayName: string; const Verb: string); overload;

{ Special folder locations }

const
  CSIDL_DESKTOP                 = $0000; { Desktop }
  CSIDL_PROGRAMS                = $0002; { Start Menu\Programs }
  CSIDL_PERSONAL                = $0005; { "My Documents" folder }
  CSIDL_FAVORITES               = $0006; { \Favorites }
  CSIDL_STARTUP                 = $0007; { Start Menu\Programs\Startup }
  CSIDL_RECENT                  = $0008; { \Recent }
  CSIDL_SENDTO                  = $0009; { \SendTo }
  CSIDL_STARTMENU               = $000B; { \Start Menu }
  CSIDL_MYMUSIC                 = $000D; { "My Music" folder }
  CSIDL_MYVIDEO                 = $000E; { "My Videos" folder }
  CSIDL_DESKTOPDIRECTORY        = $0010; { \Desktop }
  CSIDL_NETHOOD                 = $0013; { \nethood }
  CSIDL_FONTS                   = $0014; { windows\fonts }
  CSIDL_TEMPLATES               = $0015;
  CSIDL_COMMON_STARTMENU        = $0016; { All Users\Start Menu }
  CSIDL_COMMON_PROGRAMS         = $0017; { All Users\Start Menu\Programs }
  CSIDL_COMMON_STARTUP          = $0018; { All Users\Startup }
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019; { All Users\Desktop }
  CSIDL_APPDATA                 = $001A; { Application Data, new for NT4 }
  CSIDL_PRINTHOOD               = $001B; { \PrintHood }
  CSIDL_LOCAL_APPDATA           = $001C; { non roaming, user\Local Settings\Application Data }
  CSIDL_COMMON_FAVORITES        = $001F;
  CSIDL_INTERNET_CACHE          = $0020;
  CSIDL_COOKIES                 = $0021;
  CSIDL_HISTORY                 = $0022;
  CSIDL_COMMON_APPDATA          = $0023; { All Users\Application Data }
  CSIDL_WINDOWS                 = $0024; { GetWindowsDirectory() }
  CSIDL_SYSTEM                  = $0025; { GetSystemDirectory() }
  CSIDL_PROGRAM_FILES           = $0026; { C:\Program Files }
  CSIDL_MYPICTURES              = $0027; { "My Pictures", new for Win2K }
  CSIDL_PROFILE                 = $0028; { USERPROFILE }
  CSIDL_PROGRAM_FILES_COMMON    = $002B; { C:\Program Files\Common }
  CSIDL_COMMON_TEMPLATES        = $002D; { All Users\Templates }
  CSIDL_COMMON_DOCUMENTS        = $002E; { All Users\Documents }
  CSIDL_COMMON_ADMINTOOLS       = $002F; { All Users\Start Menu\Programs\Administrative Tools }
  CSIDL_ADMINTOOLS              = $0030; { \Start Menu\Programs\Administrative Tools }
  CSIDL_COMMON_MUSIC            = $0035; { All Users\My Music }
  CSIDL_COMMON_PICTURES         = $0036; { All Users\My Pictures }
  CSIDL_COMMON_VIDEO            = $0037; { All Users\My Video }
  CSIDL_RESOURCES               = $0038; { %windir%\Resources\, For theme and other windows resources. }
  CSIDL_RESOURCES_LOCALIZED     = $0039; { %windir%\Resources\, for theme and other windows specific resources. }
  CSIDL_CDBURN_AREA             = $003B; { \Local Settings\Application Data\Microsoft\CD Burning }

{ Undocumented shell folder CoClass IDs }

  CLSID_NetworkPlaces: TGUID = (
    D1:$208D2C60; D2:$3AEA; D3:$1069; D4:($A2,$D7,$08,$00,$2B,$30,$30,$9D));
  CLSID_NetworkDomain: TGUID = (
    D1:$46E06680; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  CLSID_NetworkServer: TGUID = (
    D1:$C0542A90; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  CLSID_NetworkShare: TGUID = (
    D1:$54A754C0; D2:$4BF0; D3:$11D1; D4:($83,$EE,$00,$A0,$C9,$0D,$C8,$49));
  CLSID_MyComputer: TGUID = (
    D1:$20D04FE0; D2:$3AEA; D3:$1069; D4:($A2,$D8,$08,$00,$2B,$30,$30,$9D));
  CLSID_Internet: TGUID = (
    D1:$871C5380; D2:$42A0; D3:$1069; D4:($A2,$EA,$08,$00,$2B,$30,$30,$9D));
  CLSID_ShellFSFolder: TGUID = (
    D1:$F3364BA0; D2:$65B9; D3:$11CE; D4:($A9,$BA,$00,$AA,$00,$4A,$E8,$37));
  CLSID_RecycleBin: TGUID = (
    D1:$645FF040; D2:$5081; D3:$101B; D4:($9F,$08,$00,$AA,$00,$2F,$95,$4E));
  CLSID_ControlPanel: TGUID = (
    D1:$21EC2020; D2:$3AEA; D3:$1069; D4:($A2,$DD,$08,$00,$2B,$30,$30,$9D));
  CLSID_Printers: TGUID = (
    D1:$2227A280; D2:$3AEA; D3:$1069; D4:($A2,$DE,$08,$00,$2B,$30,$30,$9D));
  CLSID_MyDocuments: TGUID = (
    D1:$450D8FBA; D2:$AD25; D3:$11D0; D4:($98,$A8,$08,$00,$36,$1B,$11,$03));
  CLSID_PrintersAndFaxes: TGUID = (
    D1:$2227A280; D2:$3AEA; D3:$1069; D4:($A2,$DE,$08,$00,$2B,$30,$30,$9D));

{ Item list manipulation routines }

function ILCreateFromPath(const Path: string): PItemIDList;
function ILAppendID(pidl: PItemIDList; const ItemID: TSHItemID;
  AddToEnd: Boolean): PItemIDList;
function ILClone(pidl: PItemIDList): PItemIDList;
function ILCloneFirst(pidl: PItemIDList): PItemIDList;
function ILCombine(pidl1, pidl2: PItemIDList): PItemIDList;
function ILFindChild(pidlParent: PItemIDList;
  pidlChild: PItemIDList): PItemIDList;
function ILFindLastID(pidl: PItemIDList): PItemIDList;
procedure ILFree(pidl: PItemIDList);
function ILGetCount(pidl: PItemIDList): Integer;
function ILGetNext(pidl: PItemIDList): PItemIDList;
function ILGetSize(pidl: PItemIDList): Integer;
function ILIsChild(pidlParent, pidlChild: PItemIDList): Boolean;
function ILIsEqual(pidl1, pidl2: PItemIDList): Boolean;
function ILIsBinaryEqual(pidl1, pidl2: PItemIDList): Boolean;
function ILIsParent(pidlParent, pidlChild: PItemIDList;
  Immediate: Boolean): Boolean;
function ILIsRoot(pidl: PItemIDList): Boolean;
function ILRemoveLastID(pidl: PItemIDList): Boolean;
function ILRename(pidlOld, pidlNew: PItemIDList): PItemIDList;

{ Shell helper functions }

function ExtractStrRet(const StrRet: TStrRet; pidl: PItemIDList): string;

{ TShellNode }

type
  TSpecialFolder = (
    sfDesktop, sfInternet, sfPrograms, sfControls, sfPrinters, sfPersonal,
    sfFavorites, sfStartup, sfRecent, sfSendto, sfBitBucket, sfStartmenu,
    sfDesktopDirectory, sfDrives, sfNetwork, sfNethood, sfFonts,
    sfTemplates, sfCommonStartMenu, sfCommonPrograms, sfCommonStartup,
    sfCommonDesktopDirectory, sfAppData, sfPrintHood, sfAltStartup,
    sfCommonAltStartup, sfCommonFavorites, sfInternetCache, sfCookies,
    sfHistory);

  TShellNode = class;

  TShellNodeArray = array of TShellNode;
  TShellNodeClass = class of TShellNode;

  TShellNode = class(TPersistent)
  private
    FAbsoluteList: PItemIDList;
    FItem: TShellNodeArray;
    FParent: TShellNode;
    FRelativeList: PItemIDList;
    FShellFolder: IShellFolder;
    FLockHandle: THandle;
    procedure RebuildAbsolute;
    function GetCount: Integer;
    function GetHasChildren: Boolean;
    function GetItem(Index: Integer): TShellNode;
    function GetName: string;
    function GetPath: string;
  protected
    function EnumFlags: Longword; virtual;
    procedure Initialize; virtual;
  public
    constructor Create(Parent: TShellNode; ItemList: PItemIDList);
    constructor CreateFromList(ItemList: PItemIDList);
    constructor CreateFromFolder(SpecialFolder: TSpecialFolder);
    constructor CreateFromObject(Folder: IShellFolder);
    constructor CreateFromPath(const Path: string);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function Clone(NodeClass: TShellNodeClass = nil): TShellNode;
    function Find(Node: TShellNode): TShellNode;
    function Add(Node: TShellNode): TShellNode;
    procedure Remove(Node: TShellNode);
    procedure Rename(Node: TShellNode); virtual;
		function Execute(Wnd: HWND; const Verb: string): Boolean;
    procedure Lock;
    procedure Unlock;
    function GetAttributes(Flags: UINT): UINT;
    function IsEqual(Node: TShellNode): Boolean;
    property AbsoluteList: PItemIDList read FAbsoluteList;
    property HasChildren: Boolean read GetHasChildren;
    property RelativeList: PItemIDList read FRelativeList;
    property ShellFolder: IShellFolder read FShellFolder;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TShellNode read GetItem; default;
    property Parent: TShellNode read FParent;
    property Name: string read GetName;
    property Path: string read GetPath;
  end;

function StrToNode(const Ident: string): TShellNode;
function StrToNodeDef(const Ident: string; const Default: TShellNode): TShellNode;
function NodeToStr(Node: TShellNode): string;
function SpecialFolderToStr(Folder: TSpecialFolder): string;

const
  {$EXTERNALSYM NIN_BALLOONSHOW}
  NIN_BALLOONSHOW       = WM_USER + 2;
  {$EXTERNALSYM NIN_BALLOONHIDE}
  NIN_BALLOONHIDE       = WM_USER + 3;
  {$EXTERNALSYM NIN_BALLOONTIMEOUT}
  NIN_BALLOONTIMEOUT    = WM_USER + 4;
  {$EXTERNALSYM NIN_BALLOONUSERCLICK}
  NIN_BALLOONUSERCLICK  = WM_USER + 5;

  {$EXTERNALSYM NIF_INFO}
  NIF_INFO       = $00000010;
  {$EXTERNALSYM NIIF_NONE}
  NIIF_NONE      = $00000000;
  {$EXTERNALSYM NIIF_INFO}
  NIIF_INFO      = $00000001;
  {$EXTERNALSYM NIIF_WARNING}
  NIIF_WARNING   = $00000002;
  {$EXTERNALSYM NIIF_ERROR}
  NIIF_ERROR     = $00000003;
  {$EXTERNALSYM NIIF_USER}
  NIIF_USER      = $00000004;
  {$EXTERNALSYM NIIF_ICON_MASK}
  NIIF_ICON_MASK = $0000000F;
  {$EXTERNALSYM NIIF_NOSOUND}
  NIIF_NOSOUND   = $00000010;

type
  PNotifyIconDataA = ^TNotifyIconDataA;
  PNotifyIconData = PNotifyIconDataA;
  {$EXTERNALSYM _NOTIFYICONDATAA}
  _NOTIFYICONDATAA = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of AnsiChar;
    case Integer of
      0: (
        uTimeout: UINT;
        szInfoTitle : array [0..63] of AnsiChar;
        dwInfoFlags: DWORD;
        guidItem: TGUID);
      1: (
        uVersion: UINT);
  end;
  {$EXTERNALSYM _NOTIFYICONDATA}
  _NOTIFYICONDATA = _NOTIFYICONDATAA;
  TNotifyIconDataA = _NOTIFYICONDATAA;
  TNotifyIconData = TNotifyIconDataA;
  {$EXTERNALSYM NOTIFYICONDATAA}
  NOTIFYICONDATAA = _NOTIFYICONDATAA;
  {$EXTERNALSYM NOTIFYICONDATA}
  NOTIFYICONDATA = NOTIFYICONDATAA;

type
  TTrayMessageKind = (tmInformation, tmWarning, tmError);

procedure TrayNotify(Handle: HWND; Kind: TTrayMessageKind; const Title, Tip: string);

implementation

uses
	StrConst;

var
  Malloc: IMalloc;

function FileOperation(Window: HWND; const Source, Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooSimpleProgress]): Boolean;
const
  Flags: array[TOperationOption] of Integer = (FOF_ALLOWUNDO, FOF_CONFIRMMOUSE,
    FOF_FILESONLY, FOF_MULTIDESTFILES, FOF_NOCONFIRMATION, FOF_NOCONFIRMMKDIR,
    FOF_RENAMEONCOLLISION, FOF_SILENT, FOF_SIMPLEPROGRESS);
var
  FileOpStruct: TSHFileOpStruct;
  LastMode: Integer;
  I: TOperationOption;
  A, B: PChar;
begin
  Result := False;
  LastMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  with FileOpStruct do
  try
    Wnd := Window;
    wFunc := Operation;
    A := nil;
    if Source <> '' then
    begin
  	  GetMem(A, Length(Source) + 2);
      FillChar(A^, Length(Source) + 2, #0);
	    Move(PChar(@Source[1])^, A^, Length(Source));
    end;
    pFrom := A;
    if A <> nil then
    	while A^ <> #0 do
      begin
      	if A^ = ',' then A^ := #0;
        Inc(A);
      end;
		A := pFrom;
    B := nil;
    if Dest <> '' then
    begin
  	  GetMem(B, Length(Dest) + 2);
      FillChar(B^, Length(Dest) + 2, #0);
	    Move(PChar(@Dest[1])^, B^, Length(Dest));
    end;
    pTo := B;
    if B <> nil then
    	while B^ <> #0 do
      begin
      	if B^ = ',' then B^ := #0;
        Inc(B);
      end;
    B := pTo;
    fFlags := 0;
    for I := Low(TOperationOption) to High(TOperationOption) do
      if I in Options then
        fFlags := fFlags or Flags[I];
    fAnyOperationsAborted := False;
    hNameMappings := nil;
    lpszProgressTitle := nil;
    try
	    if SHFileOperation(FileOpStruct) = 0 then
  	    Result := not fAnyOperationsAborted;
		finally
    	if A <> nil then
				FreeMem(A);
			if B <> nil then
				FreeMem(B);
		end;
  finally
    SetErrorMode(LastMode);
  end
end;

function FileOperation(Window: HWND; const Source: TStrings; const Dest: string; Operation: Integer;
  Options: TOperationOptions = [ooSimpleProgress]): Boolean;
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 0 to Source.Count - 1 do
  begin
    if S <> '' then
      S := S + #1 + Source[I]
    else
      S := Source[I];
  end;
  for I := 1 to Length(S) do
    if S[I] = #1 then
      S[I] := #0;
  Result := FileOperation(Window, S, Dest, Operation, Options);
end;

procedure FileDelete(FileName: string; Recycle: Boolean = False);
var
  SHFileOpStruct : TSHFileOpStruct;
begin
	FillChar(SHFileOpStruct, SizeOf(SHFileOpStruct), #0);
  with SHFileOpStruct do
  begin
    wFunc := FO_DELETE;
    pFrom := PChar(FileName + #0 + #0);
    if Recycle then
	    fFlags := FOF_SILENT or FOF_NOCONFIRMATION	or FOF_ALLOWUNDO
    else
	    fFlags := FOF_NOCONFIRMATION or FOF_SILENT;
  end;
  SHFileOperation(SHFileOpStruct);
end;

function GetFolder(Folder: TSpecialFolder; const AppName: string; Create: Boolean = False): string;
begin
  Result := IncludeTrailingPathDelimiter(SpecialFolderToStr(Folder)) +
    AppName;
  if Create then
    ForceDirectories(Result);
end;

function GetSettingsFolder(const AppName: string; Create: Boolean = False): string;
begin
  Result := GetFolder(sfAppData, AppName, Create);
end;

function GetSettingsFile(const AppName, FileName: string): string;
begin
  Result := GetSettingsFolder(AppName, True) + '\' + FileName;
end;

function GetApplicationFolder(const AppName: string; Create: Boolean = False): string;
begin
  Result := GetFolder(sfPersonal, AppName, Create);
end;

function GetApplicationFile(const AppName, FileName: string): string;
begin
  Result := GetApplicationFolder(AppName, True) + '\' + FileName;
end;

function BrowseProc(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
var
  R, WorkArea: TRect;
begin
  Result := 0;
	if uMsg = BFFM_INITIALIZED then
  begin
    if GetClientRect(Wnd, R) then
    begin
      SystemParametersInfo(SPI_GETWORKAREA, 0, @WorkArea, 0);
      R.Left := ((WorkArea.Right - WorkArea.Left - (R.Right - R.Left)) shr 1);
      R.Top := (WorkArea.Bottom - WorkArea.Top - (R.Bottom - R.Top)) shr 1;
    end;
    SetWindowPos(Wnd, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or SWP_NOSIZE
      or SWP_NOZORDER);
  end;
end;

function BrowseForFolder(const HelpText: string; var Directory: string): Boolean;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
  BrowseInfo: TBrowseInfo;
  Pidl: PItemIDList;
begin
	Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), #0);
  BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_RETURNONLYFSDIRS;
  BrowseInfo.hwndOwner := 0;
  BrowseInfo.pszDisplayName := Buffer;
  BrowseInfo.lpszTitle := PChar(HelpText);
  BrowseInfo.lpfn := @BrowseProc;
	Pidl := SHBrowseForFolder(BrowseInfo);
	if (Pidl <> nil) and (SHGetPathFromIDList(Pidl, Buffer)) then
  begin
  	Result := True;
    Directory := Buffer;
    Directory := IncludeTrailingPathDelimiter(Directory);
	end
	else
  	Result := False;
	if Pidl <> nil then
		CoTaskMemFree(Pidl);
	if BrowseInfo.pidlRoot <> nil then
    CoTaskMemFree(BrowseInfo.pidlRoot);
end;

{ ShowProperties }

procedure ExecuteVerb(Handle: HWND; ItemList: PItemIDList; const Verb: string);
var
  Desktop: IShellFolder;
  Folder: IShellFolder;
  ParentList: PItemIDList;
  RelativeList: PItemIDList;
  ContextMenu: IContextMenu;
  CommandInfo: TCMInvokeCommandInfo;
  S: AnsiString;
begin
  ParentList := ILClone(ItemList);
  if ParentList <> nil then
  try
    S := Verb;
    ILRemoveLastID(ParentList);
    OleCheck(SHGetDesktopFolder(Desktop));
    OleCheck(Desktop.BindToObject(ParentList, nil, IID_IShellFolder, Folder));
    RelativeList := ILFindChild(ParentList, ItemList);
    OleCheck(Folder.GetUIObjectOf(Handle, 1, RelativeList, IID_IContextMenu,
      nil, ContextMenu));
    FillChar(CommandInfo, SizeOf(TCMInvokeCommandInfo), #0);
    with CommandInfo do
    begin
      cbSize := SizeOf(TCMInvokeCommandInfo);
      hwnd := Handle;
      lpVerb := PAnsiChar(S);
      nShow := SW_SHOW;
    end;
    OleCheck(ContextMenu.InvokeCommand(CommandInfo));
  finally
    ILFree(ParentList);
  end;
end;

procedure ExecuteVerb(Handle: HWND; const DisplayName, Verb: string);
var
  ItemList: PItemIDList;
begin
  ItemList := ILCreateFromPath(PChar(DisplayName));
  try
    ExecuteVerb(Handle, ItemList, Verb)
  finally
    ILFree(ItemList);
  end;
end;

{ Item list manipulation routines }

const
  SpecialFolderMap: array[TSpecialFolder] of LongWord = (
    CSIDL_DESKTOP, CSIDL_INTERNET, CSIDL_PROGRAMS, CSIDL_CONTROLS,
    CSIDL_PRINTERS, CSIDL_PERSONAL, CSIDL_FAVORITES, CSIDL_STARTUP,
    CSIDL_RECENT, CSIDL_SENDTO, CSIDL_BITBUCKET, CSIDL_STARTMENU,
    CSIDL_DESKTOPDIRECTORY, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_NETHOOD,
    CSIDL_FONTS, CSIDL_TEMPLATES, CSIDL_COMMON_STARTMENU,
    CSIDL_COMMON_PROGRAMS, CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_APPDATA, CSIDL_PRINTHOOD,
    CSIDL_ALTSTARTUP, CSIDL_COMMON_ALTSTARTUP, CSIDL_COMMON_FAVORITES,
    CSIDL_INTERNET_CACHE, CSIDL_COOKIES, CSIDL_HISTORY);

var
  pidlDesktop: PItemIDList;
	Folders: array of TIdentMapEntry;

procedure InitFolderMap;
var
  Node: TShellNode;
	Folder: TSpecialFolder;
  I: Byte absolute Folder;
begin
	if Length(Folders) = 0 then
  begin
  	Setlength(Folders, Ord(High(Folder)) +1);
    for Folder := Low(Folder) to High(Folder) do
    begin
    	Folders[I].Value := I;
      Node := TShellNode.CreateFromFolder(Folder);
      try
      	Folders[I].Name := Node.Name;
      finally
      	Node.Free;
      end;
    end;
  end;
end;

function ILCreateFromPath(const Path: string): PItemIDList;
var
	Folder: TSpecialFolder;
  I: Byte absolute Folder;
  J: Integer;
	S: string;
  Desktop: IShellFolder;
  WideName: array[0..MAX_PATH] of WideChar;
  Dummy: Cardinal;
begin
	Result := nil;
  InitFolderMap;
	S := Trim(Path);
  if S = '' then Exit;
	if IdentToInt(S, J, Folders) then
	begin
  	I := J;
  	if Folder = sfDesktop then
	    Result := ILClone(pidlDesktop)
		else if SHGetSpecialFolderLocation(0, SpecialFolderMap[Folder], Result) <> S_OK then
    	Result := nil;
	end
	else
  begin
    OleCheck(SHGetDesktopFolder(Desktop));
    StringToWideChar(S, WideName, MAX_PATH);
    Dummy := 0;
    if Desktop.ParseDisplayName(0, nil, WideName, Dummy,
      Result, Dummy) <> S_OK then Result := nil;
  end;
end;

function ILAppendID(pidl: PItemIDList; const ItemID: TSHItemID;
  AddToEnd: Boolean): PItemIDList;
var
  ListSize: LongWord;
  P: PByte;
begin
  ListSize := ILGetSize(pidl);
  Result := Malloc.Alloc(ListSize + ItemID.cb + 2);
  P := Pointer(Result);
  FillMemory(Result, ListSize + ItemID.cb+ 2, 0);
  if AddToEnd then
  begin
    CopyMemory(P, pidl, ListSize);
    Inc(P, ListSize);
    CopyMemory(P, @ItemID, ItemID.cb);
  end
  else
  begin
    CopyMemory(P, @ItemID, ItemID.cb);
    Inc(P, ItemID.cb);
    CopyMemory(P, pidl, ListSize);
  end;
  ILFree(pidl);
end;

function UndocumentedILCombine(pidl1, pidl2: PItemIDList): PItemIdList; stdcall; external shell32 index 25;

function ILCombine(pidl1, pidl2: PItemIDList): PItemIDList;
{var
  Size1: LongWord;
  Size2: LongWord;
  P: PByte;
begin
  Result := nil;
  Size1 := ILGetSize(pidl1);
  Size2 :=  ILGetSize(pidl2);
  if Size1 + Size2 = 0 then
    Exit;
  Result := Malloc.Alloc(Size2 + Size1 + 2);
  P := Pointer(Result);
  FillMemory(Result, Size1 + Size2 + 2, 0);
  if Size1 > 0 then
  begin
    CopyMemory(P, pidl1, Size1);
    Inc(P, Size1);
  end;
  if Size2 > 0 then
    CopyMemory(P, pidl2, Size2);
end;}
begin
  Result := UndocumentedILCombine(pidl1, pidl2);
end;

function UndocumentedILClone(pidl : PItemIdList): PItemIdList; stdcall; external shell32 index 18;

function ILClone(pidl: PItemIDList): PItemIDList;
{var
  I: Integer;
begin
  Result := nil;
  I := ILGetSize(pidl);
  if I > 0 then
  begin
    Result := Malloc.Alloc(I + 2);
    FillMemory(Result, I + 2, 0);
    CopyMemory(Result, pidl, I);
  end
  else if ILIsEqual(pidl, )
end;}
begin
	Result := UndocumentedILClone(pidl);
end;

function ILCloneFirst(pidl: PItemIDList): PItemIDList;
begin
  Result := nil;
  if pidl <> nil then
  begin
    Result := Malloc.Alloc(pidl.mkid.cb + 2);
    FillMemory(Result, pidl.mkid.cb + 2, 0);
    CopyMemory(Result, pidl, pidl.mkid.cb);
  end;
end;

function ILFindChild(pidlParent: PItemIDList;
  pidlChild: PItemIDList): PItemIDList;
var
  Size: Integer;
begin
  Result := nil;
  Size := ILGetSize(pidlParent);
  if (Size < ILGetSize(pidlChild)) and CompareMem(pidlParent,  pidlChild,
    Size) then
  begin
    Result := pidlChild;
    Inc(PByte(Result), Size);
  end;
end;


function UndocumentedILFindLastID(pidl: PItemIDList): PItemIDList; stdcall; external shell32 index 16;

function ILFindLastID(pidl: PItemIDList): PItemIDList;
begin
  Result := UndocumentedILFindLastID(pidl);
end;
{
begin
  Result := nil;
  if pidl <> nil then
    repeat
      Result := pidl;
      Inc(PByte(pidl), pidl.mkid.cb);
    until pidl.mkid.cb = 0;
end;}

procedure ILFree(pidl: PItemIDList);
begin
  if pidl <> nil then
    CoTaskMemFree(pidl);
    //Malloc.Free(pidl);
end;

function ILGetCount(pidl: PItemIDList): Integer;
begin
  Result := 0;
  if pidl <> nil then
    repeat
      Inc(Result);
      Inc(PByte(pidl), pidl.mkid.cb);
    until pidl.mkid.cb = 0;
end;

function ILGetNext(pidl: PItemIDList): PItemIDList;
begin
  Result := nil;
  if pidl <> nil then
  begin
    Result := pidl;
    Inc(PByte(Result), Result.mkid.cb);
    if Result.mkid.cb = 0 then
      Result := nil;
  end;
end;

function ILGetSize(pidl: PItemIDList): Integer;
begin
  Result := 0;
  if pidl <> nil then
    repeat
      Inc(Result, pidl.mkid.cb);
      Inc(PByte(pidl), pidl.mkid.cb);
    until pidl.mkid.cb = 0;
end;

function ILIsChild(pidlParent, pidlChild: PItemIDList): Boolean;
var
  Size: Integer;
begin
	Result := False;
	if ILGetCount(pidlParent) = 1 then
  	Result := ILIsRoot(pidlParent) and (not (ILIsRoot(pidlChild)));
  if not Result then
  begin
	  Size := ILGetSize(pidlParent);
  	if Size < ILGetSize(pidlChild) then
  		Result := CompareMem(pidlParent, pidlChild, Size);
	end;
end;

function UndocumentedILIsEqual(pidl1, pidl2: PItemIDList): Boolean; stdcall; external shell32 index 21;

function ILIsEqual(pidl1, pidl2: PItemIDList): Boolean;
{var
  Size: Integer;
begin
  Size := ILGetSize(pidl1);
  Result := (Size = ILGetSize(pidl2)) and CompareMem(pidl1, pidl2, Size);
end;

5/6/2006 as per Gustavo comments at http://www.codebot.org/delphi/#477}
begin
  Result := UndocumentedILIsEqual(pidl1, pidl2);
end;

function ILIsBinaryEqual(pidl1, pidl2: PItemIDList): Boolean;
var
  Size: Integer;
begin
  Size := ILGetSize(pidl1);
  Result := (Size = ILGetSize(pidl2)) and CompareMem(pidl1, pidl2, Size);
end;

function UndocumentedILIsParent(pidl1, pidl2: PItemIDList; Immediate: BOOL): BOOL; stdcall; external shell32 index 23;

function ILIsParent(pidlParent, pidlChild: PItemIDList;
  Immediate: Boolean): Boolean;
{
begin
  Result := ILIsRoot(pidlParent);
  if Result then
    if Immediate then
      Result := ILGetCount(pidlChild) = 1
  else
  begin
    Result := ILFindChild(pidlParent, pidlChild) <> nil;
    if Result and Immediate then
      Result := ILGetCount(pidlParent) = ILGetCount(pidlChild) - 1;
   end;
end;}
begin
  Result := UndocumentedILIsParent(pidlParent, pidlChild, Immediate);
end;

function ILIsRoot(pidl: PItemIDList): Boolean;
begin
  Result := ILIsEqual(pidl, pidlDesktop);
end;


function UndocumentedILRemoveLastID(pidl: PItemIDList): BOOL; stdcall; external shell32 index 17;

function ILRemoveLastID(pidl: PItemIDList): Boolean;
{begin
  Result := UndocumentedILRemoveLastID(pdil);
end;}
var
  Item: PItemIDList;
begin
  Result := False;
  if pidl <> nil then
  begin
    Inc(PByte(pidl), pidl.mkid.cb);
    Result := pidl.mkid.cb <> 0;
    if Result then
    begin
      repeat
        Item := pidl;
        Inc(PByte(pidl), pidl.mkid.cb);
      until pidl.mkid.cb = 0;
      Item.mkid.cb := 0;
    end;
  end;
end;

function ILRename(pidlOld, pidlNew: PItemIDList): PItemIDList;
var
  Item: PItemIDList;
begin
  Result := nil;
  Item := ILClone(pidlOld);
  try
    if ILRemoveLastID(Item) then
      Result := ILCombine(Item, ILFindLastID(pidlNew));
  finally
    ILFree(Item);
  end;
end;

{ Shell helper functions }

function ExtractStrRet(const StrRet: TStrRet; pidl: PItemIDList): string;
begin
  Result := '';
  case StrRet.uType of
    STRRET_WSTR:
      begin
        Result := WideCharToString(StrRet.pOleStr);
        Malloc.Free(StrRet.pOleStr);
      end;
    STRRET_OFFSET: Result := PChar(LongWord(pidl) + StrRet.uOffset);
{$WARNINGS OFF}
    STRRET_CSTR: Result := StrRet.cStr;
{$WARNINGS ON}
  end;
end;

{ TShellNode }

constructor TShellNode.Create(Parent: TShellNode; ItemList: PItemIDList);
begin
  inherited Create;
  FParent := Parent;
  FRelativeList := ItemList;
  if FParent = nil then
  begin
    SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, FAbsoluteList);
    SHGetDesktopFolder(FShellFolder);
  end
  else
  begin
    FAbsoluteList := ILCombine(FParent.FAbsoluteList, FRelativeList);
    FParent.ShellFolder.BindToObject(FRelativeList, nil, IID_IShellFolder,
      FShellFolder);
  end;
  Initialize;
end;

constructor TShellNode.CreateFromList(ItemList: PItemIDList);
begin
  if (ItemList = nil) or ILIsRoot(ItemList) then
    Create(nil, nil)
  else
    Create(TShellNodeClass(ClassType).Create(nil, nil), ItemList);
end;

constructor TShellNode.CreateFromFolder(SpecialFolder: TSpecialFolder);
var
  ItemList: PItemIDList;
begin
  if SpecialFolder = sfDesktop then
    Create(nil, nil)
  else
  begin
    SHGetSpecialFolderLocation(0, SpecialFolderMap[SpecialFolder], ItemList);
    CreateFromList(ItemList);
  end;
end;

constructor TShellNode.CreateFromObject(Folder: IShellFolder);
begin
  inherited Create;
  FShellFolder := Folder;
end;

constructor TShellNode.CreateFromPath(const Path: string);
begin
  CreateFromList(ILCreateFromPath(Path));
end;

destructor TShellNode.Destroy;
begin
  Unlock;
  Clear;
  FShellFolder := nil;
  ILFree(FAbsoluteList);
  ILFree(FRelativeList);
  if (FParent <> nil) and (FParent.FItem = nil) then
    FParent.Free;
  inherited Destroy;
end;

procedure TShellNode.Assign(Source: TPersistent);
var
  SourceNode: TShellNode;
begin
  if Source is TShellNode then
  begin
    SourceNode := Source as TShellNode;
    Clear;
    ILFree(FAbsoluteList);
    ILFree(FRelativeList);
    FShellFolder := SourceNode.ShellFolder;
    FAbsoluteList := ILClone(SourceNode.AbsoluteList);
  end
  else
    inherited Assign(Source);
end;

procedure TShellNode.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(FItem) - 1 do
    FItem[I].Free;
  FItem := nil;
end;

function TShellNode.Clone(NodeClass: TShellNodeClass = nil): TShellNode;
begin
  if NodeClass = nil then
    NodeClass := TShellNodeClass(ClassType);
  Result := NodeClass.CreateFromList(ILClone(AbsoluteList));
  // Result.Assign(Self);
end;

function TShellNode.Execute(Wnd: HWND; const Verb: string): Boolean;
var
  ContextMenu: IContextMenu;
  CommandInfo: TCMInvokeCommandInfo;
  S: AnsiString;
begin
	if FParent = nil then
  begin
		Result := False;
  	Exit;
	end;
  S := AnsiString(Verb);
  OleCheck(FParent.ShellFolder.GetUIObjectOf(Wnd, 1,
    FRelativeList, IID_IContextMenu, nil, ContextMenu));
  FillChar(CommandInfo, SizeOf(TCMInvokeCommandInfo), #0);
  with CommandInfo do
  begin
    cbSize := SizeOf(TCMInvokeCommandInfo);
    hwnd := Wnd;
    lpVerb := PAnsiChar(S);
    nShow := SW_SHOWNORMAL;
  end;
  Result := ContextMenu.InvokeCommand(CommandInfo) = S_OK;
end;

procedure TShellNode.Lock;
const
  FILE_LIST_DIRECTORY = 1;
var
  S: string;
begin
  if FLockHandle = 0 then
  begin
    S := Path;
    if DirectoryExists(S) then
      FLockHandle := CreateFile(PChar(S), FILE_LIST_DIRECTORY, FILE_SHARE_READ or FILE_SHARE_WRITE,
        nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, 0);
  end;
end;

procedure TShellNode.Unlock;
begin
  if FLockHandle <> 0 then
  begin
    CloseHandle(FLockHandle);
    FLockHandle := 0;
  end;
end;

function TShellNode.Find(Node: TShellNode): TShellNode;
var
  D: IShellFolder;
  I: Integer;
begin
  Result := nil;
  if Length(FItem) = 0 then Exit;
  SHGetDesktopFolder(D);
  for I := 0 to Length(FItem) - 1 do
    if D.CompareIDs(0, FItem[I].AbsoluteList, Node.AbsoluteList) = 0 then
    begin
      Result := FItem[I];
      Break;
    end;
  if Result = nil then
    for I := 0 to Length(FItem) - 1 do
    begin
      Result := FItem[I].Find(Node);
      if Result <> nil then Break;
    end
end;

function TShellNode.Add(Node: TShellNode): TShellNode;
begin
  Result := TShellNodeClass(ClassType).Create(Self, ILClone(ILFindLastID(Node.AbsoluteList)));
  SetLength(FItem, Length(FItem) + 1);
  FItem[Length(FItem) - 1] := Result;
end;

procedure TShellNode.Remove(Node: TShellNode);
var
  Found: Boolean;
  I: Integer;
begin
  Found := False;
  for I := 0 to Length(FItem) - 1 do
    if Found then
      FItem[I - 1] := FItem[I]
    else
      Found := FItem[I] = Node;
  if Found then
  begin
    SetLength(FItem, Length(FItem) - 1);
    Node.FParent := nil;
  end;
end;

procedure TShellNode.RebuildAbsolute;
var
  LastAbsolute, LastRelative: PItemIDList;
  I: Integer;
begin
  LastAbsolute := FAbsoluteList;
  LastRelative := FRelativeList;
  FRelativeList := ILClone(ILFindLastID(FAbsoluteList));
  FAbsoluteList := ILCombine(Parent.FAbsoluteList, FRelativeList);
  ILFree(LastRelative);
  ILFree(LastAbsolute);
  FParent.ShellFolder.BindToObject(FRelativeList, nil, IID_IShellFolder,
    FShellFolder);
  for I := 0 to Length(FItem) - 1 do
    FItem[I].RebuildAbsolute;
end;

procedure TShellNode.Rename(Node: TShellNode);
var
  I: Integer;
begin
  if FParent <> nil then
  begin
    ILFree(FAbsoluteList);
    ILFree(FRelativeList);
    FAbsoluteList := ILCombine(Parent.AbsoluteList, ILFindLastID(Node.AbsoluteList));
    FRelativeList := ILClone(ILFindLastID(FAbsoluteList));
    FShellFolder := Node.ShellFolder;
    for I := 0 to Length(FItem) - 1 do
      FItem[I].RebuildAbsolute;
  end;
end;

{function TShellNode.Swap(Child: TShellNode): TShellNode;
begin
  Result := nil;
  if Child = nil then Exit;
  if not ILIsParent(AbsoluteList, Node.AbsoluteList, False) then
end;}

procedure TShellNode.Initialize;
begin
end;

function TShellNode.EnumFlags: Longword;
begin
  Result := SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN; // SHCONTF_NONFOLDERS or
end;

function TShellNode.GetCount: Integer;

  function Compare(Left, Right: TShellNode): Integer;
  begin
    Result :=  SmallInt(FShellFolder.CompareIDs(0, Left.FRelativeList,
      Right.FRelativeList));
  end;

  procedure Sort(L, R: Integer);
  var
    I, J: Integer;
    P, T: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := FItem[(L + R) shr 1];
      repeat
        while Compare(FItem[I], P) < 0 do
          Inc(I);
        while Compare(FItem[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          T := FItem[I];
          FItem[I] := FItem[J];
          FItem[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        Sort(L, J);
      L := I;
    until I >= R;
  end;

var
  Cursor: HCURSOR;
  EnumList: IEnumIDList;
  NewItem: PItemIDList;
  Dummy: LongWord;
begin
  Result := Length(FItem);
  if Result = 0 then // ) and HasChildren then
  begin
    Cursor := SetCursor(LoadCursor(0, IDC_WAIT));
    try
      if FShellFolder.EnumObjects(0, EnumFlags, EnumList) = S_OK then
      begin
        SetLength(FItem, 1000);
        try
          while EnumList.Next(1, NewItem, Dummy) = S_OK do
          begin
            Inc(Result);
            if Result  > Length(FItem) - 1 then
              SetLength(FItem, Length(FItem) + 1000);
            FItem[Result - 1] := TShellNodeClass(ClassType).Create(Self, NewItem);
          end;
        finally
          SetLength(FItem, Result);
        end;          
      end;
      if Length(FItem) > 0 then
        Sort(0, Length(FItem) - 1);
    finally
      Cursor := SetCursor(Cursor);
      DestroyCursor(Cursor);
    end;
  end;
end;

function TShellNode.GetAttributes(Flags: UINT): UINT;
begin
  if FParent <> nil then
  begin
    FParent.ShellFolder.GetAttributesOf(1, FRelativeList, Flags);
    Result := Flags;
  end
  else
    Result := 0;
end;

function TShellNode.IsEqual(Node: TShellNode): Boolean;
begin
  if Node = nil then
    Result := False
  else
    Result := ILIsEqual(FAbsoluteList, Node.AbsoluteList);
end;

function TShellNode.GetHasChildren: Boolean;
begin
  if FParent <> nil then
		Result := GetAttributes(SFGAO_HASSUBFOLDER) and SFGAO_HASSUBFOLDER = SFGAO_HASSUBFOLDER
  else
  	Result := True;
end;

function TShellNode.GetItem(Index: Integer): TShellNode;
begin
  Result := nil;
  if (Index < 0) or (Index > GetCount - 1) then
    Exit;
  Result := FItem[Index];
end;

function GetDisplayName(pidl: PitemIDList): string;
const
  Flags = SHGFI_DISPLAYNAME or SHGFI_PIDL;
var
  SHFileInfo: TSHFileInfo;
begin
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(pidl), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags);
	Result := StrPas(SHFileInfo.szDisplayName)
end;

function TShellNode.GetName: string;
var
  StrRet: TStrRet;
begin
  if ILIsRoot(FAbsoluteList) then
  begin
    ShellFolder.GetDisplayNameOf(nil, SHGDN_INFOLDER, StrRet);
    Result := ExtractStrRet(StrRet, nil);
  end
  else
    Result := GetDisplayName(FAbsoluteList);
end;

function TShellNode.GetPath: string;
var
  ItemPath: array[0..MAX_PATH] of Char;
begin
  if SHGetPathFromIDList(FAbsoluteList, ItemPath) then
    Result := ItemPath
  else
    Result := '';
end;

function StrToNode(const Ident: string): TShellNode;
var
	pidl: PItemIDList;
begin
	pidl := ILCreateFromPath(Ident);
  if pidl <> nil then
  	Result := TShellNode.CreateFromList(pidl)
  else
		raise EConvertError(SInvalidPropertyValue);
end;

function StrToNodeDef(const Ident: string; const Default: TShellNode): TShellNode;
var
	pidl: PItemIDList;
begin
	pidl := ILCreateFromPath(Ident);
  if pidl <> nil then
  	Result := TShellNode.CreateFromList(pidl)
  else
		Result := Default;
end;

function NodeToStr(Node: TShellNode): string;
begin
  Result := '';
  if Node <> nil then
  begin
	  Result := Node.Path;
    if Result = '' then
    	Result := Node.Name;
	end;
end;

function SpecialFolderToStr(Folder: TSpecialFolder): string;
var
	Node: TShellNode;
begin
	Node := TShellNode.CreateFromFolder(Folder);
  try
  	Result := NodeToStr(Node);
  finally
		Node.Free;
  end;
end;

procedure TrayNotify(Handle: HWND; Kind: TTrayMessageKind; const Title, Tip: string);
const
  MesageKinds: array[TTrayMessageKind] of Cardinal = (NIIF_INFO, NIIF_WARNING,
    NIIF_ERROR);
var
  IconData: TNotifyIconData;
begin
  FillChar(IconData, SizeOf(IconData), #0);
  IconData.cbSize := SizeOf(IconData);
  IconData.Wnd := Handle;
  IconData.uFlags := NIF_INFO;
  StrPLCopy(IconData.szInfo, Tip, SizeOf(IconData.szInfo) - 1);
  strPLCopy(IconData.szInfoTitle, Title, SizeOf(IconData.szInfoTitle) - 1);
  IconData.uTimeout := 3000;
  IconData.dwInfoFlags := MesageKinds[Kind];
  Shell_NotifyIcon(NIM_MODIFY, @IconData);
end;

initialization
  CoGetMalloc(1, Malloc);
  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, pidlDesktop);
finalization
  ILFree(pidlDesktop);
  Malloc := nil;
end.
