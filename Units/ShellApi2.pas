unit ShellAPI2;

{$WEAKPACKAGEUNIT}

interface

uses Windows, ActiveX, Messages;

function ExtractAssociatedIconEx(inst : HINST; IconPath : PChar; var IconIndex : word; var IconId : word) : HIcon; stdcall;
function ExtractAssociatedIconExA(inst : HINST; IconPath : PAnsiChar; var IconIndex : word; var IconId : word) : HIcon; stdcall;
function ExtractAssociatedIconExW(inst : HINST; IconPath : PWideChar;  var IconIndex : word; var IconId : word) : HIcon; stdcall;

const
  { AppBar stuff }
  {$EXTERNALSYM ABM_SETSTATE}
  ABM_SETSTATE         = $0000000a;

{ Shell File Operations }

const
  {$EXTERNALSYM FOF_NOCOPYSECURITYATTRIBS}
  FOF_NOCOPYSECURITYATTRIBS  = $0800;  // dont copy NT file Security Attributes
  {$EXTERNALSYM FOF_NORECURSION}
  FOF_NORECURSION            = $1000;  // don't recurse into directories.
  {$EXTERNALSYM FOF_NO_CONNECTED_ELEMENTS}
  FOF_NO_CONNECTED_ELEMENTS  = $2000;  // don't operate on connected elements.
  {$EXTERNALSYM FOF_WANTNUKEWARNING}
  FOF_WANTNUKEWARNING        = $4000;  // during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION)
  {$EXTERNALSYM FOF_NORECURSEREPARSE}
  FOF_NORECURSEREPARSE       = $8000;  // treat reparse points as objects, not containers

{ ShellExecute() and ShellExecuteEx() error codes }
const
  {$EXTERNALSYM SEE_MASK_HMONITOR}
  SEE_MASK_HMONITOR       = $00200000;
  {$EXTERNALSYM SEE_MASK_NOZONECHECKS}
  SEE_MASK_NOZONECHECKS   = $00800000;
  {$EXTERNALSYM SEE_MASK_NOQUERYCLASSSTORE}
  SEE_MASK_NOQUERYCLASSSTORE = $01000000;
  {$EXTERNALSYM SEE_MASK_WAITFORINPUTIDLE}
  SEE_MASK_WAITFORINPUTIDLE  = $02000000;
  {$EXTERNALSYM SEE_MASK_FLAG_LOG_USAGE}
  SEE_MASK_FLAG_LOG_USAGE    = $04000000;

{$EXTERNALSYM WinExecError}
procedure WinExecError(wnd : HWND; error : integer; lpstrFileName, lpstrTitle : LPCSTR); stdcall;
{$EXTERNALSYM WinExecErrorA}
procedure WinExecErrorA(wnd : HWND; error : integer; lpstrFileName, lpstrTitle : LPCSTR); stdcall;
{$EXTERNALSYM WinExecErrorW}
procedure WinExecErrorW(wnd : HWND; error : integer; lpstrFileName, lpstrTitle : LPCWSTR); stdcall;

type
  {$EXTERNALSYM SHCREATEPROCESSINFOW}
  SHCREATEPROCESSINFOW = packed record
    cbSize : DWORD;
    fMask : ULONG;
    wnd : HWND;
    pszFile : LPCWSTR;
    pszParameters : LPCWSTR;
    pszCurrentDirectory : LPCWSTR;
    hUserToken : THandle;
    lpProcessAttributes : PSecurityAttributes;
    lpThreadAttributes : PSecurityAttributes;
    bInheritHandles : bool;
    dwCreationFlags : DWORD;
    lpStartupInfo : PSTARTUPINFO;
    lpProcessInformation : PPROCESSINFORMATION;
  end;
  {$EXTERNALSYM PSHCREATEPROCESSINFOW}
  PSHCREATEPROCESSINFOW = ^SHCREATEPROCESSINFOW;

{$EXTERNALSYM SHCreateProcessAsUserW}
function SHCreateProcessAsUserW(pscpi : PSHCREATEPROCESSINFOW) : bool; stdcall;

// struct for query recycle bin info
type
  {$EXTERNALSYM SHQUERYRBINFO}
  SHQUERYRBINFO = packed record
    cbSize : DWORD;
    i64Size : int64;
    i64NumItems : int64;
  end;
  {$EXTERNALSYM LPSHQUERYRBINFO}
  LPSHQUERYRBINFO = ^SHQUERYRBINFO;


// flags for SHEmptyRecycleBin
//
const
  {$EXTERNALSYM SHERB_NOCONFIRMATION}
  SHERB_NOCONFIRMATION   = $00000001;
  {$EXTERNALSYM SHERB_NOPROGRESSUI}
  SHERB_NOPROGRESSUI     = $00000002;
  {$EXTERNALSYM SHERB_NOSOUND}
  SHERB_NOSOUND          = $00000004;


{$EXTERNALSYM SHQueryRecycleBin}
function SHQueryRecycleBin(pszRootPath : LPCSTR; pSHQueryRBInfo : LPSHQUERYRBINFO) : HResult; stdcall;
{$EXTERNALSYM SHQueryRecycleBinA}
function SHQueryRecycleBinA(pszRootPath : LPCSTR; pSHQueryRBInfo : LPSHQUERYRBINFO) : HResult; stdcall;
{$EXTERNALSYM SHQueryRecycleBinW}
function SHQueryRecycleBinW(pszRootPath : LPCWSTR; pSHQueryRBInfo : LPSHQUERYRBINFO) : HResult; stdcall;
{$EXTERNALSYM SHEmptyRecycleBin}
function SHEmptyRecycleBin(wnd : HWND; pszRootPath : LPCSTR; dwFlags : DWORD) : HResult; stdcall;
{$EXTERNALSYM SHEmptyRecycleBinA}
function SHEmptyRecycleBinA(wnd : HWND; pszRootPath : LPCSTR; dwFlags : DWORD) : HResult; stdcall;
{$EXTERNALSYM SHEmptyRecycleBinW}
function SHEmptyRecycleBinW(wnd : HWND; pszRootPath : LPCWSTR; dwFlags : DWORD) : HResult; stdcall;

type
  PNotifyIconDataA = ^TNotifyIconDataA;
  PNotifyIconDataW = ^TNotifyIconDataW;
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
    dwState : DWORD;
    dwStateMask : DWORD;
    szInfo : array [0..255] of AnsiChar;
    uTimeoutOrVersion : UINT;
    szInfoTitle : array [0..63] of AnsiChar;
    dwInfoFlags : DWORD;
    guidItem : TGUID;
  end;

  {$EXTERNALSYM _NOTIFYICONDATAW}
  _NOTIFYICONDATAW = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of WideChar;
    dwState : DWORD;
    dwStateMask : DWORD;
    szInfo : array [0..255] of WideChar;
    uTimeoutOrVersion : UINT;
    szInfoTitle : array [0..63] of WideChar;
    dwInfoFlags : DWORD;
    guidItem : TGUID;
  end;
  {$EXTERNALSYM _NOTIFYICONDATA}
  _NOTIFYICONDATA = _NOTIFYICONDATAA;
  TNotifyIconDataA = _NOTIFYICONDATAA;
  TNotifyIconDataW = _NOTIFYICONDATAW;
  TNotifyIconData = TNotifyIconDataA;
  {$EXTERNALSYM NOTIFYICONDATAA}
  NOTIFYICONDATAA = _NOTIFYICONDATAA;
  {$EXTERNALSYM NOTIFYICONDATAW}
  NOTIFYICONDATAW = _NOTIFYICONDATAW;
  {$EXTERNALSYM NOTIFYICONDATA}
  NOTIFYICONDATA = NOTIFYICONDATAA;

const
  {$EXTERNALSYM NIN_SELECT}
  NIN_SELECT          = (WM_USER + 0);
  {$EXTERNALSYM NINF_KEY}
  NINF_KEY            = 1;
  {$EXTERNALSYM NIN_KEYSELECT}
  NIN_KEYSELECT       = (NIN_SELECT or NINF_KEY);

  {$EXTERNALSYM NIN_BALLOONSHOW}
  NIN_BALLOONSHOW     = (WM_USER + 2);
  {$EXTERNALSYM NIN_BALLOONHIDE}
  NIN_BALLOONHIDE     = (WM_USER + 3);
  {$EXTERNALSYM NIN_BALLOONTIMEOUT}
  NIN_BALLOONTIMEOUT  = (WM_USER + 4);
  {$EXTERNALSYM NIN_BALLOONUSERCLICK}
  NIN_BALLOONUSERCLICK = (WM_USER + 5);

  {$EXTERNALSYM NIM_SETFOCUS}
  NIM_SETFOCUS    = $00000003;
  {$EXTERNALSYM NIM_SETVERSION}
  NIM_SETVERSION  = $00000004;
  {$EXTERNALSYM NOTIFYICON_VERSION}
  NOTIFYICON_VERSION = 3;

  {$EXTERNALSYM NIF_STATE}
  NIF_STATE       = $00000008;
  {$EXTERNALSYM NIF_INFO}
  NIF_INFO        = $00000010;
  {$EXTERNALSYM NIF_GUID}
  NIF_GUID        = $00000020;

  {$EXTERNALSYM NIS_HIDDEN}
  NIS_HIDDEN      = $00000001;
  {$EXTERNALSYM NIS_SHAREDICON}
  NIS_SHAREDICON  = $00000002;

  {$EXTERNALSYM NIIF_NONE}
  NIIF_NONE       = $00000000;
  // icon flags are mutually exclusive
  // and take only the lowest 2 bits
  {$EXTERNALSYM NIIF_INFO}
  NIIF_INFO       = $00000001;
  {$EXTERNALSYM NIIF_WARNING}
  NIIF_WARNING    = $00000002;
  {$EXTERNALSYM NIIF_ERROR}
  NIIF_ERROR      = $00000003;
  {$EXTERNALSYM NIIF_ICON_MASK}
  NIIF_ICON_MASK  = $0000000F;
  {$EXTERNALSYM NIIF_NOSOUND}
  NIIF_NOSOUND    = $00000010;


const
  {$EXTERNALSYM SHGFI_ATTR_SPECIFIED}
  SHGFI_ATTR_SPECIFIED    = $000020000;     // get only specified attributes
  {$EXTERNALSYM SHGFI_ADDOVERLAYS}
  SHGFI_ADDOVERLAYS       = $000000020;     // apply the appropriate overlays
  {$EXTERNALSYM SHGFI_OVERLAYINDEX}
  SHGFI_OVERLAYINDEX      = $000000040;     // Get the index of the overlay

{$EXTERNALSYM SHGetDiskFreeSpace}
function SHGetDiskFreeSpace(pszDirectoryName : LPCSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;
{$EXTERNALSYM SHGetDiskFreeSpaceEx}
function SHGetDiskFreeSpaceEx(pszDirectoryName : LPCSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;
{$EXTERNALSYM SHGetDiskFreeSpaceExA}
function SHGetDiskFreeSpaceExA(pszDirectoryName : LPCSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;
{$EXTERNALSYM SHGetDiskFreeSpaceExW}
function SHGetDiskFreeSpaceExW(pszDirectoryName : LPCWSTR; var pulFreeBytesAvailableToCaller,pulTotalNumberOfBytes,pulTotalNumberOfFreeBytes : int64) : bool; stdcall;

{$EXTERNALSYM SHGetNewLinkInfo}
function SHGetNewLinkInfo(pszLinkTo, pszDir : LPCSTR; pszName : LPSTR; var pfMustCopy : bool; uFlags : uint) : bool; stdcall;
{$EXTERNALSYM SHGetNewLinkInfoA}
function SHGetNewLinkInfoA(pszLinkTo, pszDir : LPCSTR; pszName : LPSTR; var pfMustCopy : bool; uFlags : uint) : bool; stdcall;
{$EXTERNALSYM SHGetNewLinkInfoW}
function SHGetNewLinkInfoW(pszLinkTo, pszDir : LPCWSTR; pszName : LPWSTR; var pfMustCopy : bool; uFlags : uint) : bool; stdcall;

const
  {$EXTERNALSYM SHGNLI_NOLNK}
  SHGNLI_NOLNK            = $000000008;     // don't add ".lnk" extension

// Printer stuff
const
  {$EXTERNALSYM PRINTACTION_OPEN}
  PRINTACTION_OPEN           = 0;
  {$EXTERNALSYM PRINTACTION_PROPERTIES}
  PRINTACTION_PROPERTIES     = 1;
  {$EXTERNALSYM PRINTACTION_NETINSTALL}
  PRINTACTION_NETINSTALL     = 2;
  {$EXTERNALSYM PRINTACTION_NETINSTALLLINK}
  PRINTACTION_NETINSTALLLINK = 3;
  {$EXTERNALSYM PRINTACTION_TESTPAGE}
  PRINTACTION_TESTPAGE       = 4;
  {$EXTERNALSYM PRINTACTION_OPENNETPRN}
  PRINTACTION_OPENNETPRN     = 5;
  {$EXTERNALSYM PRINTACTION_DOCUMENTDEFAULTS}
  PRINTACTION_DOCUMENTDEFAULTS = 6;
  {$EXTERNALSYM PRINTACTION_SERVERPROPERTIES}
  PRINTACTION_SERVERPROPERTIES = 7;

{$EXTERNALSYM SHInvokePrinterCommand}
function SHInvokePrinterCommand(wnd : HWND; uAction : UINT; lpBuf1, lpBuf2 : LPCSTR; fModal : BOOL) : bool; stdcall;
{$EXTERNALSYM SHInvokePrinterCommandA}
function SHInvokePrinterCommandA(wnd : HWND; uAction : UINT; lpBuf1, lpBuf2 : LPCSTR; fModal : BOOL) : bool; stdcall;
{$EXTERNALSYM SHInvokePrinterCommandW}
function SHInvokePrinterCommandW(wnd : HWND; uAction : UINT; lpBuf1, lpBuf2 : LPCSTR; fModal : BOOL) : bool; stdcall;

//
// The SHLoadNonloadedIconOverlayIdentifiers API causes the shell's
// icon overlay manager to load any registered icon overlay
// identifers that are not currently loaded.  This is useful if an
// overlay identifier did not load at shell startup but is needed
// and can be loaded at a later time.  Identifiers already loaded
// are not affected.  Overlay identifiers implement the
// IShellIconOverlayIdentifier interface.
//
// Returns:
//      S_OK
//
{$EXTERNALSYM SHLoadNonloadedIconOverlayIdentifiers}
function SHLoadNonloadedIconOverlayIdentifiers : hresult; stdcall;

//
// The SHIsFileAvailableOffline API determines whether a file
// or folder is available for offline use.
//
// Parameters:
//     pwszPath             file name to get info about
//     pdwStatus            (optional) OFFLINE_STATUS_* flags returned here
//
// Returns:
//     S_OK                 File/directory is available offline, unless
//                            OFFLINE_STATUS_INCOMPLETE is returned.
//     E_INVALIDARG         Path is invalid, or not a net path
//     E_FAIL               File/directory is not available offline
//
// Notes:
//     OFFLINE_STATUS_INCOMPLETE is never returned for directories.
//     Both OFFLINE_STATUS_LOCAL and OFFLINE_STATUS_REMOTE may be returned,
//     indicating "open in both places." This is common when the server is online.
//
{$EXTERNALSYM SHIsFileAvailableOffline}
function SHIsFileAvailableOffline(pwszPath : LPCWSTR; pdwStatus : LPDWORD) : hresult; stdcall;

const
  {$EXTERNALSYM OFFLINE_STATUS_LOCAL}
  OFFLINE_STATUS_LOCAL        = $0001;  // If open, it's open locally
  {$EXTERNALSYM OFFLINE_STATUS_REMOTE}
  OFFLINE_STATUS_REMOTE       = $0002;  // If open, it's open remotely
  {$EXTERNALSYM OFFLINE_STATUS_INCOMPLETE}
  OFFLINE_STATUS_INCOMPLETE   = $0004;  // The local copy is currently imcomplete.
                                            // The file will not be available offline
                                            // until it has been synchronized.

//  sets the specified path to use the string resource
//  as the UI instead of the file system name
{$EXTERNALSYM SHSetLocalizedName}
function SHSetLocalizedName(pszPath : LPWSTR; pszResModule : LPCWSTR; idsRes : integer) : HResult; stdcall;

//====== ShellMessageBox ================================================
// If lpcTitle is NULL, the title is taken from hWnd
// If lpcText is NULL, this is assumed to be an Out Of Memory message
// If the selector of lpcTitle or lpcText is NULL, the offset should be a
//     string resource ID
// The variable arguments must all be 32-bit values (even if fewer bits
//     are actually used)
// lpcText (or whatever string resource it causes to be loaded) should
//     be a formatting string similar to wsprintf except that only the
//     following formats are available:
//         %%              formats to a single '%'
//         %nn%s           the nn-th arg is a string which is inserted
//         %nn%ld          the nn-th arg is a DWORD, and formatted decimal
//         %nn%lx          the nn-th arg is a DWORD, and formatted hex
//     note that lengths are allowed on the %s, %ld, and %lx, just
//                         like wsprintf
//
{$EXTERNALSYM ShellMessageBox}
function ShellMessageBox(Instance : THandle; wnd : HWnd; Msg : PChar;
                          Title : PChar; uStyle : cardinal;
                          Format : PChar) : integer; cdecl;
{$EXTERNALSYM ShellMessageBoxA}
function ShellMessageBoxA(Instance : THandle; wnd : HWnd; Msg : PChar;
                          Title : PChar; uStyle : cardinal;
                          Format : PChar) : integer; cdecl;
{$EXTERNALSYM ShellMessageBoxW}
function ShellMessageBoxW(Instance : THandle; wnd : HWnd; Msg : PWideChar;
                          Title : PWideChar; uStyle : cardinal;
                          Format : PWideChar) : integer; cdecl;

{$EXTERNALSYM IsLFNDrive}
function IsLFNDrive(pszPath : LPCSTR) : bool; stdcall;
{$EXTERNALSYM IsLFNDriveA}
function IsLFNDriveA(pszPath : LPCSTR) : bool; stdcall;
{$EXTERNALSYM IsLFNDriveW}
function IsLFNDriveW(pszPath : LPCSTR) : bool; stdcall;

{$EXTERNALSYM SHEnumerateUnreadMailAccounts}
function SHEnumerateUnreadMailAccounts(hKeyUser : HKEY; dwIndex : DWORD; pszMailAddress : LPSTR; cchMailAddress : integer) : hresult; stdcall;
{$EXTERNALSYM SHEnumerateUnreadMailAccountsA}
function SHEnumerateUnreadMailAccountsA(hKeyUser : HKEY; dwIndex : DWORD; pszMailAddress : LPSTR; cchMailAddress : integer) : hresult; stdcall;
{$EXTERNALSYM SHEnumerateUnreadMailAccountsW}
function SHEnumerateUnreadMailAccountsW(hKeyUser : HKEY; dwIndex : DWORD; pszMailAddress : LPWSTR; cchMailAddress : integer) : hresult; stdcall;

{$EXTERNALSYM SHGetUnreadMailCount}
function SHGetUnreadMailCount(hKeyUser : HKEY; pszMailAddress : LPCSTR; var pdwCount : DWORD; var pFileTime : TFILETIME; pszShellExecuteCommand : LPSTR; cchShellExecuteCommand : integer) : hresult; stdcall;
{$EXTERNALSYM SHGetUnreadMailCountA}
function SHGetUnreadMailCountA(hKeyUser : HKEY; pszMailAddress : LPCSTR; var pdwCount : DWORD; var pFileTime : TFILETIME; pszShellExecuteCommand : LPSTR; cchShellExecuteCommand : integer) : hresult; stdcall;
{$EXTERNALSYM SHGetUnreadMailCountW}
function SHGetUnreadMailCountW(hKeyUser : HKEY; pszMailAddress : LPCWSTR; var pdwCount : DWORD; var pFileTime : TFILETIME; pszShellExecuteCommand : LPWSTR; cchShellExecuteCommand : integer) : hresult; stdcall;


{$EXTERNALSYM SHSetUnreadMailCount}
function SHSetUnreadMailCount(pszMailAddress : LPCSTR; dwCount : DWORD; pszShellExecuteCommand : LPCSTR) : hresult; stdcall;
{$EXTERNALSYM SHSetUnreadMailCountA}
function SHSetUnreadMailCountA(pszMailAddress : LPCSTR; dwCount : DWORD; pszShellExecuteCommand : LPCSTR) : hresult; stdcall;
{$EXTERNALSYM SHSetUnreadMailCountW}
function SHSetUnreadMailCountW(pszMailAddress : LPCWSTR; dwCount : DWORD; pszShellExecuteCommand : LPCWSTR) : hresult; stdcall;

{$EXTERNALSYM SHTestTokenMembership}
function SHTestTokenMembership(hToken : THandle; ulRID : cardinal) : bool; stdcall;

{$EXTERNALSYM SHGetImageList}
function SHGetImageList(iImageList : integer; riid : TIID; var ppvObj : Pointer) : hresult; stdcall;

const
  {$EXTERNALSYM SHIL_LARGE}
  SHIL_LARGE         = 0;   // normally 32x32
  {$EXTERNALSYM SHIL_SMALL}
  SHIL_SMALL         = 1;   // normally 16x16
  {$EXTERNALSYM SHIL_EXTRALARGE}
  SHIL_EXTRALARGE    = 2;
  {$EXTERNALSYM SHIL_SYSSMALL}
  SHIL_SYSSMALL      = 3;   // like SHIL_SMALL, but tracks system small icon metric correctly

  {$EXTERNALSYM SHIL_LAST}
  SHIL_LAST          = SHIL_SYSSMALL;

type
  {$EXTERNALSYM PFNCANSHAREFOLDERW}
  PFNCANSHAREFOLDERW = function (pszPath : LPCWSTR): hresult; stdcall;
  {$EXTERNALSYM PFNSHOWSHAREFOLDERUIW}
  PFNSHOWSHAREFOLDERUIW = function(hwndParent : hwnd; pszPath : LPCWSTR) : hresult; stdcall;

const
  shell32 = 'shell32.dll';

implementation

function ExtractAssociatedIconEx; external shell32 name 'ExtractAssociatedIconExA';
function ExtractAssociatedIconExA; external shell32 name 'ExtractAssociatedIconExA';
function ExtractAssociatedIconExW; external shell32 name 'ExtractAssociatedIconExW';
procedure WinExecError; external shell32 name 'WinExecErrorA';
procedure WinExecErrorA; external shell32 name 'WinExecErrorA';
procedure WinExecErrorW; external shell32 name 'WinExecErrorW';
function SHCreateProcessAsUserW; external shell32 name 'SHCreateProcessAsUserW';
function SHQueryRecycleBin; external shell32 name 'SHQueryRecycleBinA';
function SHQueryRecycleBinA; external shell32 name 'SHQueryRecycleBinA';
function SHQueryRecycleBinW; external shell32 name 'SHQueryRecycleBinW';
function SHEmptyRecycleBin; external shell32 name 'SHEmptyRecycleBinA';
function SHEmptyRecycleBinA; external shell32 name 'SHEmptyRecycleBinA';
function SHEmptyRecycleBinW; external shell32 name 'SHEmptyRecycleBinW';
function SHGetDiskFreeSpace; external shell32 name 'SHGetDiskFreeSpaceExA';
function SHGetDiskFreeSpaceEx; external shell32 name 'SHGetDiskFreeSpaceExA';
function SHGetDiskFreeSpaceExA; external shell32 name 'SHGetDiskFreeSpaceExA';
function SHGetDiskFreeSpaceExW; external shell32 name 'SHGetDiskFreeSpaceExW';
function SHGetNewLinkInfo; external shell32 name 'SHGetNewLinkInfoA';
function SHGetNewLinkInfoA; external shell32 name 'SHGetNewLinkInfoA';
function SHGetNewLinkInfoW; external shell32 name 'SHGetNewLinkInfoW';
function SHInvokePrinterCommand; external shell32 name 'SHInvokePrinterCommandA';
function SHInvokePrinterCommandA; external shell32 name 'SHInvokePrinterCommandA';
function SHInvokePrinterCommandW; external shell32 name 'SHInvokePrinterCommandW';
function SHLoadNonloadedIconOverlayIdentifiers; external shell32 name 'SHLoadNonloadedIconOverlayIdentifiers';
function SHIsFileAvailableOffline; external shell32 name 'SHIsFileAvailableOffline';
function SHSetLocalizedName; external shell32 name 'SHSetLocalizedName';
function ShellMessageBox; external shell32 name 'ShellMessageBoxA';
function ShellMessageBoxA; external shell32 name 'ShellMessageBoxA';
function ShellMessageBoxW; external shell32 name 'ShellMessageBoxW';
function IsLFNDrive; external shell32 name 'IsLFNDriveA';
function IsLFNDriveA; external shell32 name 'IsLFNDriveA';
function IsLFNDriveW; external shell32 name 'IsLFNDriveW';
function SHEnumerateUnreadMailAccounts; external shell32 name 'SHEnumerateUnreadMailAccountsA';
function SHEnumerateUnreadMailAccountsA; external shell32 name 'SHEnumerateUnreadMailAccountsA';
function SHEnumerateUnreadMailAccountsW; external shell32 name 'SHEnumerateUnreadMailAccountsW';
function SHGetUnreadMailCount; external shell32 name 'SHGetUnreadMailCountA';
function SHGetUnreadMailCountA; external shell32 name 'SHGetUnreadMailCountA';
function SHGetUnreadMailCountW; external shell32 name 'SHGetUnreadMailCountW';
function SHSetUnreadMailCount; external shell32 name 'SHSetUnreadMailCountA';
function SHSetUnreadMailCountA; external shell32 name 'SHSetUnreadMailCountA';
function SHSetUnreadMailCountW; external shell32 name 'SHSetUnreadMailCountW';
function SHTestTokenMembership; external shell32 name 'SHTestTokenMembership';
function SHGetImageList; external shell32 name 'SHGetImageList';

end.