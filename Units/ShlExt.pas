unit ShlExt;

interface

uses
  CommCtrl, ActiveX, Windows, ShlObj, ShlObj2, Messages;

const
  // New Interface IDs
  {$EXTERNALSYM IID_IBrowserFrameOptions}
  IID_IBrowserFrameOptions : TGUID = '{10DF43C8-1DBE-11D3-8B34-006097DF5BD4}';

  IID_IGetNameSpaceExtensionPointer: TGUID = '{287D4A71-439F-43A4-8D5B-0E0AE71E84A9}';

  IID_IPersistFreeThreadedObject: TGUID = '{C7264BF0-EDB6-11D1-8546-006008059368}';

  SID_IUIElement = '{EC6FE84F-DC14-4FBB-889F-EA50FE27FE0F}';
  SID_IUICommand = '{4026DFB9-7691-4142-B71C-DCF08EA4DD9C}';
  SID_IEnumUICommand = '{869447DA-9F84-4E2A-B92D-00642DC8A911}';


  // String constants for Interface IDs
  SID_IBrowserFrameOptions  = '{10DF43C8-1DBE-11D3-8B34-006097DF5BD4}';
  SID_IGetNameSpaceExtensionPointer = '{287D4A71-439F-43a4-8D5B-0E0AE71E84A9}';
  SID_IPersistFreeThreadedObject = '{C7264BF0-EDB6-11D1-8546-006008059368}';

type
  PPidlArray = ^TPidlArray;
  TPidlArray = array [0..255] of PItemIdList;
  PGuidArray = ^TGuidArray;
  TGuidArray = array [0..255] of TGUID;

{$IFDEF VER130}
type
  TIntegerArray = array [0..255] of integer;
  PIntegerArray = ^TIntegerArray;
{$ENDIF}

//==========================================================================
// Used by the IShellBrowser/IShellView objects.
//==========================================================================
const
  // The shell handles internally all the following ID's without calling
  // InvokeCommand()
  FCIDM_SHVIEWSHELL = $7000;

  // File menu options
  FCIDM_MENU_FILE_CREATESHORTCUT = FCIDM_SHVIEWSHELL + $0010;
  FCIDM_MENU_FILE_DELETE         = FCIDM_SHVIEWSHELL + $0011;
  FCIDM_MENU_FILE_RENAME         = FCIDM_SHVIEWSHELL + $0012;
  FCIDM_MENU_FILE_PROPERTIES     = FCIDM_SHVIEWSHELL + $0013;

  // Edit menu options
  FCIDM_MENU_EDIT_CUT            = FCIDM_SHVIEWSHELL + $0018;
  FCIDM_MENU_EDIT_COPY           = FCIDM_SHVIEWSHELL + $0019;
  FCIDM_MENU_EDIT_PASTE          = FCIDM_SHVIEWSHELL + $001A;
  FCIDM_MENU_EDIT_UNDO           = FCIDM_SHVIEWSHELL + $001B;
  FCIDM_MENU_EDIT_PASTESHORTCUT  = FCIDM_SHVIEWSHELL + $001C;

  // View Report styles
  FCIDM_MENU_VIEW_SELECTALL      = FCIDM_SHVIEWSHELL + $0021;
  FCIDM_MENU_VIEW_INVERTSELECTION = FCIDM_SHVIEWSHELL + $0022;

  FCIDM_MENU_VIEW_LARGEICONS     = FCIDM_SHVIEWSHELL + $0029;
  FCIDM_MENU_VIEW_SMALLICONS     = FCIDM_SHVIEWSHELL + $002A;
  FCIDM_MENU_VIEW_LIST           = FCIDM_SHVIEWSHELL + $002B;
  FCIDM_MENU_VIEW_DETAILS        = FCIDM_SHVIEWSHELL + $002C;

  FCIDM_MENU_VIEW_AUTOARRANGE    = FCIDM_SHVIEWSHELL + $0031;
  FCIDM_MENU_VIEW_LINEUPICONS    = FCIDM_SHVIEWSHELL + $0032;

  // Help menu
  FCIDM_MENU_HELP_HELPTOPICS     = FCIDM_SHVIEWSHELL + $0041;

  // The refresh command
  FCIDM_MENU_VIEW_REFRESH        = FCIDM_SHVIEWSHELL + $0103;

const
  SFVID_FIRST = FCIDM_SHVIEWFIRST + $2000;
  SFVID_LAST  = FCIDM_SHVIEWFIRST + $3000;
  SFVID_MENU_ARRANGE = SFVID_FIRST;

type
  TSFVCBColumnInfoStruct = record
    pidl : PITEMIDLIST;  // NULL if column header requested, else
                         // a simple pidl to the item whose details are needed.
    sci : TShellDetails; // return filled in with the details.
  end;
  PSFVCBColumnInfoStruct = ^TSFVCBColumnInfoStruct;
{$IFDEF VER130}
const
  SFVTI_ADDTOEND = 0;
  SFVTI_ADDTOFRONT = 1;
  SFVTI_OVERWRITE  = 2;
type
  SFVTIF = CARDINAL;
{$ELSE}
type
  // SFVTOOLBARINFO flags
  SFVTIF = (SFVTI_ADDTOEND = 0, SFVTI_ADDTOFRONT = 1, SFVTI_OVERWRITE  = 2);
{$ENDIF}
  TSFVCBToolBarInfo = record
    dwNumItems : DWord;
    dwFlags : DWord; // combination of the FCT_MERGE,... flags
  end;
  PSFVCBToolBarInfo = ^TSFVCBToolBarInfo;

  TSFVCBSelectInfo = record
    uOldState : DWORD; // 0
    uNewState :  DWORD; //LVIS_SELECTED, LVIS_FOCUSED,...
    pidl : PItemIdList;
  end;

  // Generic structure used by several messages
  TSFVCBInfo = record
    dwReserved : DWORD;
    dwReserved2 : DWORD;
    pidl : PItemIdList;
    dwUser : PDWORD;
  end;
  PSVCBInfo = ^TSFVCBInfo;

  // SFVCB_COPYHOOKCALLBACK structure
  TSFVCopyHookInfo = record
    wnd : HWND;
    wFunc : UINT;
    wFlags : UINT;
    szSrcFile : LPCSTR;
    dwSrcAttribs : DWORD;
    szDestFile : LPCSTR;
    dwDestAttribs : DWORD;
  end;
  PSFVCopyHookInfo = ^TSFVCopyHookInfo;

  TSFVM_WEBVIEW_LAYOUT_DATA = record
    flags : cardinal;
    pUnk : IUnknown; //IPreview3?
  end;
  PSFVM_WEBVIEW_LAYOUT_DATA = ^TSFVM_WEBVIEW_LAYOUT_DATA;

  TSFVM_WEBVIEW_CONTENT_DATA = packed record
    l1 : integer;
    l2 : integer;
    pUnk : IUnknown; // IUIElement
    pUnk2 : IUnknown; // IUIElement
    pEnum : IEnumIdList;
  end;
  PSFVM_WEBVIEW_CONTENT_DATA = ^TSFVM_WEBVIEW_CONTENT_DATA;

  TSFVM_WEBVIEW_TASKSECTION_DATA = record
    pEnum : IUnknown; // IEnumUICommand
    pEnum2 : IUnknown; // IEnumUICommand
  end;
  PSFVM_WEBVIEW_TASKSECTION_DATA = ^TSFVM_WEBVIEW_TASKSECTION_DATA;

  TSFVM_WEBVIEW_THEME_DATA = record
    pszTheme : PWideChar;
  end;
  PSFVM_WEBVIEW_THEME_DATA = ^TSFVM_WEBVIEW_THEME_DATA;

type
  IEnumShellItems = interface(IUnknown)
//    ['{4670AC35-34A6-4D2B-B7B6-CD665C6189A5}']
    function Next(celt: UINT; out rgelt: IShellItem; out pceltFetched: UINT): HResult; stdcall;
    function Skip(celt: UINT): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumShellItems): HResult; stdcall;
  end;

  IShellItemArray = interface(IUnknown)
//    ['{90CF20DE-73B4-4AA4-BA7A-82FF310AF24A}']
    // IShellItemArray methods
    function BindToHandler(pbc : IBindCtx; const rbhid : TGUID; const riid : TIID; out ppvOut): HResult; stdcall;
    function GetAttrributes(nEnum : integer; dwRequested : dword; out pdwResult : dword): HResult; stdcall;
    function GetCount(out pCount : UINT): HResult; stdcall;
    function GetItemAt(nIndex : uint; out ppItem : IShellItem) : HResult; stdcall;
    function EnumItems(out enumShellItems : IEnumShellItems): HResult; stdcall;
  end;

  IUIElement = interface(IUnknown)
    [SID_IUIElement]
    // IUIElement methods
    function get_Name(pItemArray : IShellItemArray; var bstrName : TBStr) : HResult; stdcall;
    function get_Icon(pItemArray : IShellItemArray; var bstrName : TBStr) : HResult; stdcall;
    function get_Tooltip(pItemArray : IShellItemArray; var bstrName : TBStr) : HResult; stdcall;
  end;

  UISTATE = TOLEEnum;

  IUICommand = interface(IUIElement)
    [SID_IUICommand]
    function get_CanonicalName(Guid : TGUID) : HResult; stdcall;
    function get_State(pItemArray : IShellItemArray; nRequested : integer; var pState : UIState) : HResult; stdcall;
    function Invoke(pItemArray : IShellItemArray; pCtx : IBindCtx) : HResult; stdcall;
  end;

  IEnumUICommand = interface(IUnknown)
    [SID_IEnumUICommand]
    // *** IEnumIDList methods ***
    function Next(celt: UINT; out rgelt: IUICommand; out pceltFetched: UINT): HResult; stdcall;
    function Skip(celt: UINT): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumUICommand): HResult; stdcall;
  end;

  // The interface IShellFiolderView is implemented by the IUnknown that
  // is passed to IObjectWithSite::SetSite.
  TItemSpacing = record
    cxSmall : integer;
    cySmall : integer;
    cxLarge : integer;
    cyLarge : integer;
  end;

  IShellFolderView = interface(IUnknown)
    ['{37A378C0-F82D-11CE-AE65-08002B2E1262}']
    // *** IShellFolderView methods ***
    function Rearrange(lParamSort : LPARAM) : HResult; stdcall;
    function GetArrangeParam(var plParamSort : LPARAM) : HResult; stdcall;
    function ArrangeGrid : HResult; stdcall;
    function AutoArrange : HResult; stdcall;
    function GetAutoArrange : HResult; stdcall;
    function AddObject(pidl : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function GetObject(var ppidl : PItemIdList; uItem : cardinal) : HResult; stdcall;
    function RemoveObject(pidl : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function GetObjectCount(var puCount : cardinal) : HResult; stdcall;
    function SetObjectCount(uCount : cardinal; dwFlags : cardinal) : HResult; stdcall;
    function UpdateObject(pidlOls,pidlNew : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function RefreshObject(pidl : PItemIdList; var puItem : cardinal) : HResult; stdcall;
    function SetRedraw(bRedraw : bool) : HResult; stdcall;
    function GetSelectedCount(var puSelected : cardinal) : HResult; stdcall;
    function GetSelectedObjects(var pidl : PPidlArray; var puItems : cardinal) : HResult; stdcall;
    function IsDropOnSource(DropTarget : IDropTarget) : HResult; stdcall;
    function GetDragPoint(var ppt : TPOINT) : HResult; stdcall;
    function GetDropPoint(var ppt : TPOINT) : HResult; stdcall;
    function MoveIcons(DataObject : IDataObject) : HResult; stdcall;
    function SetItemPos(pidl : PItemIdList; var ppt : TPOINT) : HResult; stdcall;
    function IsBkDropTarget(DropTarget : IDropTarget) : HResult; stdcall;
    function SetClipboard (bMove : BOOL) : HResult; stdcall;
    function SetPoints(DataObject : IDataObject) : HResult; stdcall;
    function GetItemSpacing(var spacing : TITEMSPACING) : HResult; stdcall;
    function SetCallback(pNewCB : IShellFolderViewCB; var ppOldCB : IShellFolderViewCB) : HResult; stdcall;
    function Select(dwFlags : cardinal) : HResult; stdcall;
    function QuerySupport(var pdwSupport : cardinal) : HResult; stdcall;
    function SetAutomationObject(disp : IDispatch) : HResult; stdcall;
  end;

const
  //                        uMsg       wParam             lParam
  // When the selection state of an item is changed (i.e. an item is selected or deselected)
  SFVM_SELECTIONCHANGED   = 8; //      idCmdFirst,nItem   TSFVCBSelectInfo struct
  SFVM_DRAWMENUITEM       = 9; //      idCmdFirst         pdis
  SFVM_MEASUREMENUITEM    = 10;//      idCmdFist          pmis
  // called when context menu exits, not main menu
  SFVM_EXITMENULOOP       = 11;//        0                 0
  // indicates that the IShellView object is being released.
  SFVM_VIEWRELEASE        = 12;//        -                lSelChangeInfo
  // Sent when beginning label edit.
  SFVM_GETNAMELENGTH      = 13;//        pidlItem         length
  // Called to indicate that the view window is being destroyed.
  SFVM_WINDOWCLOSING      = 16;//        hwnd             PDVSELCHANGEINFO
  SFVM_LISTREFRESHED      = 17;//         0               lSelChangeInfo
  // Sent to inform us that the list view has received the focus.
  SFVM_WINDOWFOCUSED      = 18;//         0               0
  SFVM_KILLFOCUS          = 19; //         0               0
  SFVM_REGISTERCOPYHOOK   = 20;//         0               0
  SFVM_COPYHOOKCALLBACK   = 21;//         -               LPCOPYHOOKINFO
  SFVM_NOTIFY               = 22;//         idFrom          LPNOTIFY
  SFVM_ADDINGOBJECT       = 29;//         pidl            PDVSELCHANGEINFO
  SFVM_REMOVINGOBJECT     = 30;//         pidl            PDVSELCHANGEINFO
  SFVM_UPDATESTATUSBAR    = 31;//         -               lSelChangeInfo
  SFVM_GETCOMMANDDIR      = 33;
  // Get an IStream interface
  SFVM_GETCOLUMNSTREAM    = 34;// READ/WRITE/READWRITE    IStream
  SFVM_CANSELECTALL       = 35;//                         lSelChangeInfo
  SFVM_SUPPORTSIDENTITY   = 37;//         0               0
  SFVM_ISCHILDOBJECT      = 38;
  SFVM_GETEXTVIEWS        = 40;
  SFVM_GETITEM            = 42;//       iItem             LPITMIDLIST*
  SFVM_SETITEM            = 43;//       iItem             LPITEMIDLIST
  SFVM_INDEXOFITEM        = 44;//       *iItem            LPITEMIDLIST
  SFVM_FINDITEM           = 45;//       *iItem            NM_FINDITEM*
  SFVM_WNDMAIN            = 46;//                         hwndMain
  SFVM_COLUMNCLICK2       = 50;//       nil               column index
  SFVM_STANDARDVIEWS      = 51;//                         BOOL *
  SFVM_REUSEEXTVIEW       = 52;//                         BOOL *
  SFVM_GETEMPTYTEXT       = 54;//      cchMax             pszText
  SFVM_GETITEMICONINDEX   = 55;//      iItem              int *piIcon
  SFVM_DONTCUSTOMIZE      = 56;//      -                  BOOL *pbDontCustomize
  SFVM_ISOWNERDATA        = 60;//      ISOWNERDATA        BOOL *
  SFVM_GETRANGEOBJECT     = 61;//      iWhich             ILVRange **
  SFVM_CACHEHINT          = 62;//      -                  NMLVCACHEHINT *
  SFVM_OVERRIDEITEMCOUNT  = 64;//      -                  UINT*
  SFVM_GETHELPTEXTW       = 65;//      idCmd,cchMax       pszText - unicode
  SFVM_GETTOOLTIPTEXTW    = 66;//      idCmd,cchMax       pszText - unicode
  SFVM_GETIPERSISTHISTORY = 67;//                         IPersistHistory **
  SFVM_GETHELPTEXTA       = 69;//      idCmd,cchMax       pszText - ansi
  SFVM_GETTOOLTIPTEXTA    = 70;//      idCmd,cchMax       pszText - ansi
  SFVM_GETICONOVERLAY     = 71;//      iItem              int iOverlayIndex
  SFVM_SETICONOVERLAY     = 72;//      iItem              int * piOverlayIndex
  SFVM_ALTERDROPEFFECT    = 73;//      DWORD*             IDataObject*

  // XP messages - not all id'ed yet
  SFVM_MESSAGE4A          = 74;
  SFVM_MESSAGE4B          = 75;
  SFVM_MESSAGE4C          = 76;
  SFVM_GET_CUSTOMVIEWINFO = 77;
  SFVM_MESSAGE4E          = 78;
  SFVM_ENUMERATEDITEMS    = 79;
  SFVM_GET_VIEW_DATA      = 80;
  SFVM_MESSAGE51          = 81;
  SFVM_GET_WEBVIEW_LAYOUT = 82;
  SFVM_GET_WEBVIEW_CONTENT= 83;
  SFVM_GET_WEBVIEW_TASKS  = 84;
  SFVM_MESSAGE55          = 85;
  SFVM_GET_WEBVIEW_THEME  = 86;
  SFVM_MESSAGE57          = 87;
  SFVM_MESSAGE58          = 88;
  SFVM_MESSAGE59          = 89;
  SFVM_MESSAGE5A          = 90;
  SFVM_MESSAGE5B          = 91;
  SFVM_GETDEFERREDVIEWSETTINGS = 92;

// The interface IDelegateFolder is used for items in Internet Explorer
// like FTP folders.
// The problem:
// The root of these items is not displayed.
// For example, you don't have Internet Explorer\FTP\aserver.com
// The FTP folder is not displayed.
// But of course there has to be a root for the
// FTP folders.
// Internet explorer can not know about ftp items and items of other
// protocols.
// So what they did is they embedded the pidls for items directly
// under the Internet Explorer root in an Internet Explorer pidl.
// The Internet Explorer pidls look like this:
// {total size}{sig}{child pidl}{rest of pidl}
// where {total size) is the size of the pidl, as always.
//       {sig} is a singature of 4 bytes
//       {child} is an ebmedded pidl
//       {rest of pidl} defines the protocol etc. and is used by IE.
// The child is a normal pidl like you are used to. As usual it
// starts with the size.
// Internet Explorer therefore calls SetItemMalloc and gives the child
// an IMalloc it can usxe to allocate pidls.
// These pidls are IE pidls of the above form, but the child pidl
// is not filled in.
// So after SetItemAlloc has been called, when ever you create a pidl
// you should call the IMalloc's Alloc, and put your pidl at offset 6
// of the returned buffer.
type
  IDelegateFolder = interface(IUnknown)
    [SID_IDelegateFolder]
    function SetItemAlloc(const Malloc : IMalloc) : HResult; stdcall;
  end;

/////////////////////////////////////////////////////////////////////////////
// Totally miscellaneous stuff

// ??? about IShellFolder ?
// This is the interface for a browser to "subclass" the main File Cabinet
// window.  Note that only the hwnd, message, wParam, and lParam fields of
// the msg structure are used.  The browser window will get a WM_NOTIFY
// message with NULL ID, FCN_MESSAGE as the code, and a far pointer to
// FCMSG_NOTIFY as the lParam.
type
  TFCMSGNotify = record
    hdr : TNMHDR;
    msg : TMSG;
    Result : LResult;
  end;

const
  FCN_MESSAGE = 100;

//---------------------------------------------------------------------------
// messages that can be send to the cabinet by other apps
//---------------------------------------------------------------------------
// Change the path of an existing folder.
// wParam:
//  0:    LPARAM is a string, handle the message immediately.
//  CSP_HANDLE:  LPARAM is a handle. handle the message immediately
//      and then free the handle.
//  CSP_REPOST:  LPARAM is a string, copy the string and handle the
//       message later.
//   CSP_REPOST|CSP_HANDLE:
//      LPARAM is a handle, just handle the message later
//      and free the handle then.
// lParam: LPSTR or HANDLE of path.
//
const
  CSP_REPOST  = $0001;
  CSP_HANDLE  =  $0002;

type
  // lpsv points to the Shell View extension that requested idle processing
  // uID is an app define identifier for the processor
  // returns: TRUE if there is more idle processing necessary, FALSE if all done
  // Note that the idle processor should do one "atomic" operation and return
  // as soon as possible.
  FCIdleProc = function (lpsv : Pointer; uID : UINT) : BOOL; stdcall;


///////////////////////////////////////////////////////////////////////
// typeDefine the undocumented interfaces
//
/////////////////////////////////////////////////////////////////////////////
// IBrowserFrameOptions
type
  TBrowserFrameOption = (bfoBrowserPersistSettings,
                         bfoRenameFolderOptionsToInternet,
                         bfoBothOptions,
                         bfoPreferInternetShortcut,
                         bfoBrowseNoInNewProcess,
                         bfoEnableHyperlinkTracking,
                         bfoUseIEOfflineSupport,
                         bfoSubstituteInternetStartPage,
                         bfoUseIELogoBanding,
                         bfoAddIEToCaptionBar,
                         bfoUseDialupRef,
                         bfoUseIEToolbar,
                         bfoNoParentFolderSupport,
                         bfoNoReopenNextRestart,
                         bfoGoHomePage,
                         bfoPreferIEProcess,
                         bfoShowNavigationCancelled) ;

  TBrowserFrameOptions = set of TBrowserFrameOption;

const
  bfoNone = [];
  bfoQueryAll  = [bfoBrowserPersistSettings..bfoShowNavigationCancelled];

type
  IBrowserFrameOptions = interface(IUnknown)
    [SID_IBrowserFrameOptions]
    function GetFrameOptions(dwRequested : DWORD; var pdwResult : DWORD) : HResult; stdcall;
  end;

const
  PID_STG_STORAGETYPE = 4; // The object's type VT_BSTR
  PID_STG_NAME = 10;       // The object's display name VT_BSTR
  PID_STG_SIZE = 12;       // The object's size VT_BSTR
  PID_STG_ATTRIBUTES = 13;  // The object's attributes VT_BSTR
  PID_STG_WRITETIME = 14;  // The object's modified attribute VT_BSTR

  FMTID_FileAttributes : TGUID = (
    D1:$8D72ACA1; D2:$0716; D3:$419A; D4:($9a,$c1,$ac,$b0,$7b,$18,$dc,$32));

  // Other PIDs
  PID_STG_DIRECTORY = 2;
  PID_STG_CLASSID = 3;
  PID_STG_VOLUME_ID = 5;
  PID_STG_PARENT_WORKID = 6;
  PID_STG_SECONDARYSTORE = 7;
  PID_STG_FILEINDEX = 8;
  PID_STG_LASTCHANGEUSN = 9;
  PID_STG_PATH = $0b;
  PID_STG_CREATETIME = $0f;
  PID_STG_ACCESSTIME = $10;
  PID_STG_CHANGETIME = $11;
  PID_STG_CONTENTS = $13;
  PID_STG_SHORTNAME = $14;
  PID_STG_MAX = PID_STG_SHORTNAME;
  CSTORAGEPROPERTY = $15;


//===========================================================================
//
// Shell Common Dialogs
//
//===========================================================================
// RunFileDlg flags
const
  RFF_NOBROWSE = $01;
  RFF_NODEFAULT = $02;
  RFF_CALCDIRECTORY = $04;
  RFF_NOLABEL = $08;
  RFF_NOSEPARATEMEM = $20;  // NT only

type
  // RunFileFlg notification structure
  NM_RUNFILEDLG = record
    hdr : NMHDR;
    lpFile : LPCSTR;
    lpDirectory : LPCSTR;
    nShow : integer;
  end;
  PNM_RUNFILEDLG = ^NM_RUNFILEDLG;

const
  // RunFileDlg notification return values
  RF_OK = $00;
  RF_CANCEL = $01;
  RF_RETRY  = $02;

type
  TRunFileDlg = procedure(hWndOwner : HWND; Icon : HICON; Directory,Title,Description : PChar; Flags : UInt); stdcall;
  TExitWindowsDialog = procedure(hWndOwner : HWND); stdcall;
  TSHFindComputer = function(pidlRoot,pidlSavedSearch : PItemIdList) : bool; stdcall;

var
  RunFileDlg : TRunFileDlg;
  ExitWindowsDialog : TExitWindowsDialog;
  SHFindComputer : TSHFindComputer;


type
  TSHHandleDiskFull = procedure(hWndOwner : HWND; uDrive : UINT); stdcall;
  TSHOutOfMemoryMessageBox = function(hWndOwner : HWND; Caption : PChar; uType : UINT) : integer; stdcall;
  TSHNetConnectionDialog = function(hWndOwner : HWND; RemoteName : PChar; dwType : cardinal) : cardinal; stdcall;
  TShellMessageBoxA = function(Instance : THandle; wnd : HWnd; Msg : PChar;
                               Title : PChar; uStyle : cardinal;
                               Format : PChar) : integer; stdcall;
var
  ShHandleDiskFull : TShHandleDiskFull;
  SHOutOfMemoryMessageBox : TSHOutOfMemoryMessageBox;
  SHNetConnectionDialog : TSHNetConnectionDialog;
  ShellMessageBoxA : TShellMessageBoxA;

//===========================================================================
//
// PIDL Manipulation Routines
//
//===========================================================================
type


  // returns the next pidl in a complex idlist
  TILGetNext = function (pidl : PItemIdList) : PItemIdList; stdcall;

  // Used to implement explorer parameters?
  TILGlobalClone = function(pidl : PItemIdList) : PItemIdList; stdcall;
  TILGlobalFree = procedure(pidl : PItemIdList); stdcall;


var
  ILGetNext : TILGetNext;
  ILGlobalClone : TILGlobalClone;
  ILGlobalFree : TILGlobalFree;

//===========================================================================
//
// Shell Notifications
//
//===========================================================================
// SHChangeNotifyRegister flags
const
  SHCNF_ACCEPT_INTERRUPTS     = $0001;
  SHCNF_ACCEPT_NON_INTERRUPTS = $0002;
  SHCNF_NO_PROXY              = $8000;  // NT only

type
  // SHChangeNotifyRegister structure
  TShChangeNotifyEntry = record
    pidlPath : PItemIdList;
    bWatchSubtree : bool;
  end;
  PShChangeNotifyEntry = ^TShChangeNotifyEntry;

type
  // DWORD item id structure
  DWordItemId = record
    cb : WORD;
    dwItem1 : dword;
    dwItem2 : dword;
  end;

//===========================================================================
//
// Cabinet Window Messages
//
//===========================================================================
const
  CWM_SETPATH           = WM_USER + 2;
  // Inform the File Cabinet that you want idle messages.
  // This should ONLY be used by File Cabinet extensions.
  // wParam: app define UINT (passed to FCIDLEPROC).
  // lParam: pointer to an FCIDLEPROC.
  // return: TRUE if successful; FALSE otherwise
  CWM_WANTIDLE          = WM_USER + 3;
  // get or set the FOLDERSETTINGS for a view
  // wParam: BOOL TRUE -> set to view info buffer, FALSE -> get view info buffer
  // lParam: LPFOLDERSETTINGS buffer to get or set view info
  CWM_GETSETCURRENTINFO  = WM_USER + 4;
  // selects the specified item in the current view
  // wParam: BOOL TRUE -> select, FALSE -> deselect
  // lParam: LPCSTR of the item ID (not display name), NULL -> all items
  CWM_SELECTITEM        = WM_USER + 5;
  CWM_SELECTITEMSTR     = WM_USER + 6;
  // tells the window to punt its wait cursor.  used for handoff
  // while thread inits
  CWM_STOPWAITING       = WM_USER + 6;
  // Get the IShellBrowser object associated with an hwndMain
  CWM_GETISHELLBROWSER  = WM_USER + 7;
  CWM_TESTPATH          = WM_USER + 9;
  CWM_STATECHANGE       = WM_USER + 10;
  CWM_GETPATH           = WM_USER + 12;

  // CWM_TESTPATH types
  CWTP_ISEQUAL  = 0;
  CWTP_ISCHILD  = 1;

type
  // CWM_TESTPATH structure
  CWTESTPATHSTRUCT = record
    dwType : dword;
    idl : ITEMIDLIST;
  end;
  LPCWTESTPATHSTRUCT = ^CWTESTPATHSTRUCT;

  procedure FileCabinet_SelectItem(wnd : HWND; bSel : boolean; lpidl : PItemIdList);
  // New way to get IShellBrowser etc interface
  function IUnknown_QueryService(punkSite : IUnknown; sid : TGUID; riid : TGUID; var ppv) : HResult;
  // Old way to get IShellBrowser etc interface
  function FileCabinet_GetIShellBrowser(wnd : HWND) : IShellBrowser;

//===========================================================================
//
// System Imagelist Routines
//
//===========================================================================
type
  TSHMapPIDLToSystemImageListIndex = function(psf : IShellFolder; pidl : PItemIdList; var Index : integer) : integer; stdcall;
  TFileIconInit = function(bFullInit : bool) : bool; stdcall;

var
  FileIconInit : TFileIconInit;

//===========================================================================
//
// File Menu Routines
//
//===========================================================================
const
  // FileMenu_Create nSelHeight constants
  FM_DEFAULT_SELHEIGHT = -1;
  FM_FULL_SELHEIGHT = 0;

  // FileMenu_Create flags
  FMF_SMALL_ICONS     = $00;
  FMF_LARGE_ICONS     = $08;
  FMF_NO_COLUMN_BREAK = $10;

const
  // FileMenu_AppendItem constants
  FM_SEPARATOR  = PChar(1);
  FM_BLANK_ICON = -1;
  FM_DEFAULT_HEIGHT = 0;

const
  // FileMenu_InsertUsingPidl flags
  FMF_NO_EMPTY_ITEM = $01;
  FMF_NO_PROGRAM_GROUPS = $04;

type
  TFNFMCallback = procedure (pidlFolder : PItemIdList; pidlFile : PItemIdList);

//===========================================================================
//
// Drag And Drop Routines
//
//===========================================================================

type
  TSHRegisterDragDrop = function(wnd : HWND; pDropTarget : IDropTarget) : HResult; stdcall;
  TSHRevokeDragDrop = function(wnd : HWND) : HResult; stdcall;
  TSHDoDragDrop = function(wnd : HWND; dtObj : IDataObject; dsrc : IDropSource;
                           OKEffect : DWORD; var Effect : dword) : HResult; stdcall;
  TDAD_DragEnter = function (WndTarget : HWND) : bool; stdcall;

var
  SHRegisterDragDrop : TSHRegisterDragDrop;
  SHRevokeDragDrop : TSHRevokeDragDrop;
  DAD_DragEnter : TDAD_DragEnter;

const
  // DAD_AutoScroll return values
  DAD_SCROLL_UP    = 1;
  DAD_SCROLL_DOWN  = 2;
  DAD_SCROLL_LEFT  = 4;
  DAD_SCROLL_RIGHT = 8;

type
  TDAD_SetDragImageFromListView = function(wnd : HWND; pt : TPoint) : bool; stdcall;
  TDAD_ShowDragImage = function(fShow : bool) : bool; stdcall;
  TCIDLData_CreateFromIDArray = function(pidlFolder : PItemIdList; cpidlFiles : DWORD; var pidlFiles : PItemIdList; DataObject : IDataObject) : HResult; stdcall;

var
  DAD_SetDragImageFromListView : TDAD_SetDragImageFromListView;
  CIDLData_CreateFromIDArray : TCIDLData_CreateFromIDArray;

//===========================================================================
//
// Path Manipulation Routines
//
//===========================================================================
type
  TPathAppend = function(szPath1,szPath2 : LPSTR) : PChar; stdcall;
  TPathCombine = function(destPath : LPSTR; Dir : LPCSTR; szfile : LPCSTR) : LPSTR; stdcall;
  TPathAddBackslash = function(szPath : LPSTR) : LPSTR; stdcall;
  TPathBuildRoot = function(path : LPSTR; Drive : integer) : LPSTR; stdcall;
  TPathFindFileName = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathFindExtension = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathGetExtension = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathGetArgs = function(szPath : LPCSTR) : LPSTR; stdcall;
  TPathGetDriveNumber = function(szPath : LPCSTR) : integer; stdcall;
  TPathRemoveFileSpec = function(szPath : LPSTR) : bool; stdcall;
  TPathRemoveBlanks = procedure(szPath : LPSTR); stdcall;
  TPathQuoteSpaces = procedure(szPath : LPSTR); stdcall;
  TPathUnquoteSpaces = procedure(szPath : LPSTR); stdcall;
  TPathIsUNC = function(szPath : LPCWSTR) : bool; stdcall;
  TPathIsRelative = function(szPath : LPCWSTR) : bool; stdcall;
  TPathIsRoot = function(szPath : LPCSTR) : bool; stdcall;

  TPathIsDirectory = function(szPath : LPCSTR) : bool; stdcall;
  TPathFileExists = function(szPath : LPCSTR) : bool; stdcall;
  TPathMatchSpec = function(szFile : LPCSTR; szSpec : LPCSTR) : bool; stdcall;
  TPathFindOnPath = procedure(szFile : LPSTR; var ppszPaths : LPCSTR); stdcall;
var
  PathAppend : TPathAppend;
  PathCombine : TPathCombine;
  PathAddBackslash : TPathAddBackslash;
  PathBuildRoot : TPathBuildRoot;
  PathFindFileName : TPathFindFileName;
  PathFindExtension : TPathFindExtension;
  PathGetExtension : TPathGetExtension;
  PathGetArgs : TPathGetArgs;
  PathGetDriveNumber : TPathGetDriveNumber;
  PathRemoveFileSpec : TPathRemoveFileSpec;
  PathRemoveBlanks : TPathRemoveBlanks;
  PathQuoteSpaces : TPathQuoteSpaces;
  PathUnquoteSpaces : TPathUnquoteSpaces;
  PathIsUNC : TPathIsUNC;
  PathIsRelative : TPathIsRelative;
  PathIsRoot : TPathIsRoot;
  PathIsDirectory : TPathIsDirectory;
  PathFileExists : TPathFileExists;
  PathMatchSpec : TPathMatchSpec;
  PathFindOnPath : TPathFindOnPath;

type
  TPathSetDlgItemPath = function(hDlg : HWND; nIDDlgItem : integer; szPath : LPCSTR) : bool; stdcall;

var
  PathSetDlgItemPath : TPathSetDlgItemPath;

type
  TPathStripPath = procedure(szPath : LPSTR); stdcall;
  TPathStripToRoot = function(szPath : LPSTR) : bool; stdcall;
  TPathRemoveArgs = procedure(szPath : LPWSTR); stdcall;
  TPathRemoveExtension = procedure(szPath : LPWSTR); stdcall;
  TPathParseIconLocation = function(szPath : LPWSTR) : integer; stdcall;
  TPathIsSameRoot = function(lpszPath1,lpszPath2 : LPCWSTR) : bool; stdcall;

var
  PathStripPath : TPathStripPath;
  PathStripToRoot : TPathStripToRoot;
  PathRemoveArgs : TPathRemoveArgs;
  PathRemoveExtension : TPathRemoveExtension;
  PathParseIconLocation : TPathParseIconLocation;
  PathIsSameRoot : TPathIsSameRoot;

//===========================================================================
//
// Shell Namespace Routines
//
//===========================================================================
// This function can be used to send some special messages to the cabinet window.
// These messages have mostly to do with gathering information about items and
// changing some attributes of some items.
// hwndCabinet: The window handle that was passed in IShellFolder::CreateViewObject
// uMsg: One of the SFVM_ constants that are described above
// lParam: a message-dependant value
// The return value depends on the message.
// You may need to call this function to enable sorting on a specified column.
// You use SFVM_REARRANGE for that.

const
//------------------------------------------------------------------------------
  // This message indicates that the user has clicked a column in the header of
  // the list control. The list needs to be rearranged.
  // lParam The index of the column on which to sort (starting at 0). This value
  // will be passed to IShellFolder::CompareIDs.
  SFVM_GETARRANGECOLUMN   = $0002;

  // Used to retrieve the number of items in the list.
  // Return value The number of items in the list
  SFVM_GETITEMCOUNT       = $0004;

  SFVM_GETITEMPIDL        = $0005;


  SFVM_SETREDRAW          = $0008;

  SFVM_ISDROPONSOURCE     = $000A;
  SFVM_MOVEICONS          = $000B;
  SFVM_GETDRAGPOINT       = $000C;
  SFVM_GETDROPPOINT       = $000D;
  SFVM_ISDROPONBACKGROUND = $000F;
  SFVM_TOGGLEAUTOARRANGE  = $0011;
  SFVM_LINEUPICONS        = $0012;
  SFVM_GETAUTOARRANGE     = $0013;

  // Used to learn how many items are selected in the list.
  // Return value The number of selected items
  SFVM_GETSELECTEDCOUNT   = $0014;
  SFVM_GETITEMSPACING     = $0015;
  SFVM_REFRESHOBJECT      = $0016;

function ShellFolderView_GetArrangeColumn(wnd : hwnd) : cardinal;
function ShellFolderView_GetItemCount(wnd : hwnd) : cardinal;
function ShellFolderView_GetItemPidl(wnd : HWND; index : cardinal) : PItemIdList;
function ShellFolderView_SetRedraw(wnd : HWND; mode : bool) : LPARAM;
function ShellFolderView_IsDropOnSource(wnd : HWND; target : IDropTarget) : bool;
procedure ShellFolderView_MoveIcons(wnd: HWND; target : IDropTarget);
function ShellFolderView_GetDragPoint(wnd : HWND; pt : PPoint) : bool;
function ShellFolderView_GetDropPoint(wnd : HWND; pt : PPoint) : bool;
function ShellFolderView_IsDropOnBackground(wnd : HWND; target : IDropTarget) : bool;
procedure ShellFolderView_ToggleAutoArrange(wnd : HWND);
procedure ShellFolderView_LineUpIcons(wnd : HWND);
function ShellFolderView_GetAutoArrange(wnd : HWND) : BOOL;
function ShellFolderView_GetSelectedCount(wnd : HWND) : cardinal;
function ShellFolderView_GetItemSpacing(wnd : HWND; var spacing : TItemSpacing) : bool;
function ShellFolderView_RefreshObject(wnd : HWND; pidl : PItemIdList): cardinal;
// Useful hack to refresh the shell folder window.
function ShellFolderView_RefreshAll(wnd : HWND) : bool;
//===========================================================================
//
// Misc Stuff
//
//===========================================================================
const
  // SHWaitForFileToOpen flags
  SHWFF_ADD = $01;
  SHWFF_REMOVE = $02;
  SHWFF_WAIT = $04;

const
  // RegisterShellHook types
  RSH_DEREGISTER       = 0;
  RSH_REGISTER         = 1;
  RSH_REGISTER_PROGMAN = 2;
  RSH_REGISTER_TASKMAN = 3;

const
  // SHCreateLinks flags
  SHCLF_PREFIXNAME      = 1;
  SHCLF_CREATEONDESKTOP = 2;

type
  TExtractAssociatedIconExA = function(hInst : THandle; IconPath : PChar;
                                       var IconIndex : word; var IconId : word) : HICON; stdcall;
var
  ExtractAssociatedIconExA : TExtractAssociatedIconExA;

type
  TSHGetNewLinkInfo = function(pszLinkTo : LPCTSTR;pszDir : LPCTSTR; szName : LPTSTR;
                               var pfMustCopy : bool; uFlags : cardinal) : bool; stdcall;

  TSHGetDiskFreeSpace = function(pszVolume : LPCTSTR; var pqwFreeCaller, pqwTot, pqwFree : int64) : bool; stdcall;
  TSHInvokePrinterCommand = function(wnd : HWnd;uAction : UINT; lpBuf1,lpBuf2 : LPCTSTR; fModal : bool) : bool; stdcall;
  TSHIsFileAvailableOffline = function(szPath : LPCWSTR;pdwStatus : PDWORD) : HResult; stdcall;

var
  SHGetNewLinkInfo : TSHGetNewLinkInfo;
  SHGetDiskFreeSpace : TSHGetDiskFreeSpace;
  SHInvokePrinterCommand : TSHInvokePrinterCommand;
  SHIsFileAvailableOffline : TSHIsFileAvailableOffline;

const
  CMF_FINDCMD = $80;
  CMF_BANDCMD = $20000;
  CMF_DVFILE  = $10000; // Indicates explorer is populating the file menu.

const
  // Definitions for CDefFolderMenu_Create2
  // Extra undocumented callback messages
  // (discovered by Maksym Schipka and Henk Devos)
//  DFM_MERGECONTEXTMENU = 1;      // uFlags       LPQCMINFO
//  DFM_INVOKECOMMAND = 2;         // idCmd        pszArgs
  DFM_CREATE = 3;                // AddRef?
  DFM_DESTROY = 4;               // Release
  DFM_GETHELPTEXTA = 5;          // idCmd,cchMax pszText
  DFM_MEASUREITEM =  6;          // same as WM_MEASUREITEM
  DFM_DRAWITEM = 7;              // same as WM_DRAWITEM
  DFM_INITMENUPOPUP = 8;         // same as WM_INITMENUPOPUP
  DFM_VALIDATECMD = 9;           // idCmd        0
  DFM_MERGECONTEXTMENU_TOP = 10; // uFlags       LPQCMINFO
  DFM_GETHELPTEXTW = 11;         // idCmd,cchMax pszText -Unicode
  DFM_INVOKECOMMANDEX = 12;      // idCmd        PDFMICS
  DFM_MAPCOMMANDNAME = 13;       // idCmd *      pszCommandName
//  DFM_GETDEFSTATICID = 14;       // idCmd *      0
  DFM_GETVERBW = 15;             // idCmd,cchMax pszText -Unicode
  DFM_GETVERBA = 16;             // idCmd,cchMax pszText -Ansi

  // Extra command IDs
  // (from Axel Sommerfeldt and Henk Devos)
  DFM_CMD_DELETE = cardinal(-1);
  DFM_CMD_CUT = cardinal(-2);
  DFM_CMD_COPY = cardinal(-3);
  DFM_CMD_CREATESHORTCUT  = cardinal(-4);
//  DFM_CMD_PROPERTIES  = UINT(-5);
  DFM_CMD_NEWFOLDER  = cardinal(-6);
  DFM_CMD_PASTE = cardinal(-7);
  DFM_CMD_VIEWLIST  = cardinal(-8);
  DFM_CMD_VIEWDETAILS  = cardinal(-9);
  DFM_CMD_PASTELINK = cardinal(-10);
  DFM_CMD_PASTESPECIAL = cardinal(-11);
  DFM_CMD_MODALPROP = cardinal(-12);

implementation

const
  CILGlobalClone = 20;
  CPathIsRoot = 29;
  CPathBuildRootW = 30;
  CPathFindExtension = 31;
  CPathAddBackslash = 32;
  CPathRemoveBlanks = 33;
  CPathFindFileName = 34;
  CPathRemoveFileSpec = 35;
  CPathAppend = 36;
  CPathCombineW = 37;
  CPathStripPath = 38;
  CPathIsUNCW = 39;
  CPathIsRelativeW = 40;
  CPathFileExists = 45;
  CPathMatchSpec = 46;
  CPathSetDlgItemPath = 48;
  CPathStripToRoot = 50;
  CPathGetArgs = 52;
  CPathQuoteSpaces = 55;
  CPathUnquoteSpaces = 56;
  CPathGetDriveNumber = 57;
  CExitWindowsDialog = 60;
  CRunFileDlg = 61;
  CCIDLData_CreateFromIDArray = 83;
  CSHRegisterDragDrop = 86;
  CSHRevokeDragDrop = 87;
  CSHFindComputer = 91;
  CSHOutOfMemoryMessageBox = 126;
  CPathFindOnPath = 145;
  CILGetNext = 153;
  CILGlobalFree = 156;
  CPathGetExtension = 158;
  CPathIsDirectory = 159;
  CSHNetConnectionDialog = 160;
  CDAD_SetDragImageFromListView = 177;
  CSHGetNewLinkInfoA = 179;
  CShellMessageBoxA = 183;
  CSHHandleDiskFull = 185;
  CPathParseIconLocation = 249;
  CPathRemoveExtension = 250;
  CPathRemoveArgs = 251;
  CExtractAssociatedIconExA = 261;
  CSHGetDiskFreeSpaceA = 311;
  CSHInvokePrinterCommandA = 335;
  CSHIsFileAvailableOffline = 337;
  CPathIsSameRoot = 650;
  CFileIconInit = 660;

var
  SaveExit: pointer;
  DLLHandle: THandle;
  ErrorMode: Integer;

//procedure NewExit; far;
//begin
//  ExitProc := SaveExit;
//  FreeLibrary(DLLHandle)
//end;

var
  DLLLoaded : boolean = False;

procedure LoadDLL;
begin
  if DLLLoaded then Exit;
  ErrorMode := SetErrorMode(SEM_NoOpenFileErrorBox);
  DLLHandle := LoadLibrary('SHELL32.DLL');
  if DLLHandle >= 32 then
  begin
    DLLLoaded := True;
//    SaveExit := ExitProc;
//    ExitProc := @NewExit;
    @ILGetNext  := GetProcAddress(DLLHandle,MakeIntResource(CILGetNext));
    @ILGlobalClone  := GetProcAddress(DLLHandle,MakeIntResource(CILGlobalClone));
    @ILGlobalFree  := GetProcAddress(DLLHandle,MakeIntResource(CILGlobalFree));
    @SHRegisterDragDrop  := GetProcAddress(DLLHandle,MakeIntResource(CSHRegisterDragDrop));
    @RunFileDlg := GetProcAddress(DLLHandle,MakeIntResource(CRunFileDlg));
    @ExitWindowsDialog := GetProcAddress(DLLHandle,MakeIntResource(CExitWindowsDialog));
    @SHFindComputer := GetProcAddress(DLLHandle,MakeIntResource(CSHFindComputer));
    @ShHandleDiskFull := GetProcAddress(DLLHandle,MakeIntResource(CShHandleDiskFull));
    @SHOutOfMemoryMessageBox := GetProcAddress(DLLHandle,MakeIntResource(CSHOutOfMemoryMessageBox));
    @SHNetConnectionDialog := GetProcAddress(DLLHandle,MakeIntResource(CSHNetConnectionDialog));
    @ShellMessageBoxA := GetProcAddress(DLLHandle,MakeIntResource(CShellMessageBoxA));
    @FileIconInit := GetProcAddress(DLLHandle,MakeIntResource(CFileIconInit));
    @SHRevokeDragDrop := GetProcAddress(DLLHandle,MakeIntResource(CSHRevokeDragDrop));
    @DAD_SetDragImageFromListView := GetProcAddress(DLLHandle,MakeIntResource(CDAD_SetDragImageFromListView));
    @CIDLData_CreateFromIDArray := GetProcAddress(DLLHandle,MakeIntResource(CCIDLData_CreateFromIDArray));
    @PathAppend := GetProcAddress(DLLHandle,MakeIntResource(CPathAppend));
    @PathCombine := GetProcAddress(DLLHandle,MakeIntResource(CPathCombineW));
    @PathAddBackslash := GetProcAddress(DLLHandle,MakeIntResource(CPathAddBackslash));
    @PathBuildRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathBuildRootW));
    @PathFindFileName := GetProcAddress(DLLHandle,MakeIntResource(CPathFindFileName));
    @PathFindExtension := GetProcAddress(DLLHandle,MakeIntResource(CPathFindExtension));
    @PathGetExtension := GetProcAddress(DLLHandle,MakeIntResource(CPathGetExtension));
    @PathGetArgs := GetProcAddress(DLLHandle,MakeIntResource(CPathGetArgs));
    @PathGetDriveNumber := GetProcAddress(DLLHandle,MakeIntResource(CPathGetDriveNumber));
    @PathRemoveFileSpec := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveFileSpec));
    @PathRemoveBlanks := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveBlanks));
    @PathQuoteSpaces := GetProcAddress(DLLHandle,MakeIntResource(CPathQuoteSpaces));
    @PathUnquoteSpaces := GetProcAddress(DLLHandle,MakeIntResource(CPathUnquoteSpaces));
    @PathIsUNC := GetProcAddress(DLLHandle,MakeIntResource(CPathIsUNCW));
    @PathIsRelative := GetProcAddress(DLLHandle,MakeIntResource(CPathIsRelativeW));
    @PathIsRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathIsRoot));
    @PathIsDirectory := GetProcAddress(DLLHandle,MakeIntResource(CPathIsDirectory));
    @PathFileExists := GetProcAddress(DLLHandle,MakeIntResource(CPathFileExists));
    @PathMatchSpec := GetProcAddress(DLLHandle,MakeIntResource(CPathMatchSpec));
    @PathFindOnPath := GetProcAddress(DLLHandle,MakeIntResource(CPathFindOnPath));
    @PathSetDlgItemPath := GetProcAddress(DLLHandle,MakeIntResource(CPathSetDlgItemPath));
    @PathStripPath := GetProcAddress(DLLHandle,MakeIntResource(CPathStripPath));
    @PathStripToRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathStripToRoot));
    @PathRemoveArgs := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveArgs));
    @PathRemoveExtension := GetProcAddress(DLLHandle,MakeIntResource(CPathRemoveExtension));
    @PathParseIconLocation := GetProcAddress(DLLHandle,MakeIntResource(CPathParseIconLocation));
    @PathIsSameRoot := GetProcAddress(DLLHandle,MakeIntResource(CPathIsSameRoot));
    @ExtractAssociatedIconExA := GetProcAddress(DLLHandle,MakeIntResource(CExtractAssociatedIconExA));
    @SHGetNewLinkInfo := GetProcAddress(DLLHandle,MakeIntResource(CSHGetNewLinkInfoA));
    @SHGetDiskFreeSpace := GetProcAddress(DLLHandle,MakeIntResource(CSHGetDiskFreeSpaceA));
    @SHInvokePrinterCommand := GetProcAddress(DLLHandle,MakeIntResource(CSHInvokePrinterCommandA));
    @SHIsFileAvailableOffline := GetProcAddress(DLLHandle,MakeIntResource(CSHIsFileAvailableOffline));
    @SHGetIconOverlayIndex := GetProcAddress(DLLHandle,'SHGetIconOverlayIndexA');
    @SHGetIconOverlayIndexA := GetProcAddress(DLLHandle,'SHGetIconOverlayIndexA');
    @SHGetIconOverlayIndexW := GetProcAddress(DLLHandle,'SHGetIconOverlayIndexW');
  end else begin
    DLLLoaded := False;
  end;
  SetErrorMode(ErrorMode)
end;

function IUnknown_QueryService(punkSite : IUnknown; sid : TGUID; riid : TGUID; var ppv) : HResult;
var
  psp : IServiceProvider;
begin
    Pointer(ppv) := nil;
    Result := E_FAIL;
    if Assigned(punkSite) then
    begin
      Result := punkSite.QueryInterface(IServiceProvider, psp);
      if SUCCEEDED(Result) then
        Result := psp.QueryService(sid, riid, ppv);
    end;
end;

function FileCabinet_GetIShellBrowser(wnd : HWND) : IShellBrowser;
begin
    Result := IShellBrowser(SendMessage(wnd, CWM_GETISHELLBROWSER, 0, 0));
end;

procedure FileCabinet_SelectItem(wnd : HWND; bSel : boolean; lpidl : PItemIdList);
begin
    SendMessage(wnd, CWM_SELECTITEM, Integer(bSel), Integer(lpidl));
end;

function ShellFolderView_GetArrangeColumn(wnd : hwnd) : cardinal;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_GETARRANGECOLUMN, 0);
end;

function ShellFolderView_GetItemCount(wnd : hwnd) : cardinal;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_GETITEMCOUNT, 0);
end;

function ShellFolderView_GetItemPidl(wnd : HWND; index : cardinal) : PItemIdList;
begin
  Result := PItemIdList(SHShellFolderView_Message(wnd, SFVM_GETITEMPIDL, index));
end;

function ShellFolderView_SetRedraw(wnd : HWND; mode : bool) : LPARAM;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_SETREDRAW, LPARAM(mode));
end;

function ShellFolderView_IsDropOnSource(wnd : HWND; target : IDropTarget) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_ISDROPONSOURCE, LPARAM(target)))
end;

procedure ShellFolderView_MoveIcons(wnd: HWND; target : IDropTarget);
begin
  SHShellFolderView_Message(wnd, SFVM_MOVEICONS, LPARAM(target));
end;

function ShellFolderView_GetDragPoint(wnd : HWND; pt : PPoint) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETDRAGPOINT, LPARAM(pt)));
end;

function ShellFolderView_GetDropPoint(wnd : HWND; pt : PPoint) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETDROPPOINT, LPARAM(pt)));
end;

function ShellFolderView_IsDropOnBackground(wnd : HWND; target : IDropTarget) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_ISDROPONBACKGROUND, LPARAM(target)));
end;

procedure ShellFolderView_ToggleAutoArrange(wnd : HWND);
begin
  SHShellFolderView_Message(wnd, SFVM_TOGGLEAUTOARRANGE, 0);
end;

procedure ShellFolderView_LineUpIcons(wnd : HWND);
begin
  SHShellFolderView_Message(wnd, SFVM_LINEUPICONS, 0);
end;

function ShellFolderView_GetAutoArrange(wnd : HWND) : BOOL;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETAUTOARRANGE, 0));
end;

function ShellFolderView_GetSelectedCount(wnd : HWND) : cardinal;
begin
  Result := Cardinal(SHShellFolderView_Message(wnd, SFVM_GETSELECTEDCOUNT, 0));
end;

function ShellFolderView_GetItemSpacing(wnd : HWND; var spacing : TItemSpacing) : bool;
begin
  Result := BOOL(SHShellFolderView_Message(wnd, SFVM_GETITEMSPACING, LPARAM(@spacing)));
end;

function ShellFolderView_RefreshObject(wnd : HWND; pidl : PItemIdList): cardinal;
begin
  Result := SHShellFolderView_Message(wnd, SFVM_REFRESHOBJECT, LPARAM(pidl));
end;

function ShellFolderView_RefreshAll(wnd : HWND) : bool;
begin
  Result := PostMessage(wnd, WM_COMMAND, FCIDM_MENU_VIEW_REFRESH, 0);
end;

initialization
  LoadDLL;
finalization
  FreeLibrary(DLLHandle)
end.