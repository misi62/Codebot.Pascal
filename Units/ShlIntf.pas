
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit ShlIntf;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, ActiveX, ShlObj;

const
  CLSID_AutoComplete: TGUID = (
    D1:$00BB2763; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  CLSID_ACLHistory: TGUID = (
    D1:$00BB2764; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  CLSID_ACListISF: TGUID = (
    D1:$03C036F1; D2:$A186; D3:$11D0; D4:($82,$4A,$00,$AA,$00,$5B,$43,$83));
  CLSID_ACLMRU: TGUID = (
    D1:$6756A641; D2:$DE71; D3:$11D0; D4:($83,$1B,$00,$AA,$00,$5B,$43,$83));
  CLSID_ACLMulti: TGUID = (
    D1:$00BB2765; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  CLSID_ExplorerBrowser: TGUID = (
    D1:$71F96385; D2:$DDD6; D3:$48D3; D4:($A0,$C1,$AE,$06,$E8,$B0,$55,$FB));
  IID_IShellView2: TGUID = (
    D1:$88E39E80; D2:$3578; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  IID_IFolderView: TGUID = (
    D1:$CDE725B0; D2:$CCC9; D3:$4519; D4:($91,$7E,$32,$5D,$72,$FA,$B4,$CE));
  IID_IShellFolderViewCB: TGUID = (
    D1:$2047E320; D2:$F2A9; D3:$11CE; D4:($AE,$65,$08,$00,$2B,$2E,$12,$62));
  IID_IServiceProvider: TGUID =(
    D1:$6D5140C1; D2:$7436; D3:$11CE; D4:($80,$34,$00,$AA,$00,$60,$09,$FA));
  IID_ICommDlgBrowser2: TGUID = (
    D1:$10339516; D2:$2894; D3:$11D2; D4:($90,$39,$00,$C0,$4F,$8E,$EB,$3E));
  IID_ICommDlgBrowser3: TGUID = (
    D1:$C8AD25A1; D2:$3294; D3:$41EE; D4:($81,$65,$71,$17,$4B,$D0,$1C,$57));
  IID_IExplorerBrowserEvents: TGUID = (
    D1:$361BBDC7; D2:$E6EE; D3:$BE58; D4:($BE,$58,$58,$E2,$24,$0C,$81,$0F));
  IID_IExplorerBrowser: TGUID = (
    D1:$DFD3B6B5; D2:$C10C; D3:$4BE9; D4:($85,$F6,$A6,$69,$69,$F4,$02,$F6));
  IID_IAutoComplete: TGUID = (
    D1:$00BB2762; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  IID_IAutoComplete2: TGUID = (
    D1:$EAC04BC0; D2:$3791; D3:$11D2; D4:($BB,$95,$00,$60,$97,$7B,$46,$4C));
  IID_IAutoCompList: TGUID = (
    D1:$00BB2760; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));
  IID_IACList: TGUID = (
    D1:$77A130B0; D2:$94FD; D3:$11D0; D4:($A5,$44,$00,$C0,$4F,$D7,$d0,$62));
  IID_IACList2: TGUID = (
    D1:$470141A0; D2:$5186; D3:$11D2; D4:($BB,$B6,$00,$60,$97,$7B,$46,$4C));
  IID_ICurrentWorkingDirectory: TGUID = (
    D1:$91956D21; D2:$9276; D3:$11D1; D4:($92,$1A,$00,$60,$97,$DF,$5B,$D4));
  IID_IObjMgr: TGUID = (
    D1:$00BB2761; D2:$6A77; D3:$11D0; D4:($A5,$35,$00,$C0,$4F,$D7,$D0,$62));

const
  SID_IShellView2              = '{88E39E80-3578-11CF-AE69-08002B2E1262}';
  SID_IFolderView              = '{CDE725B0-CCC9-4519-917E-325D72FAB4CE}';
  SID_IShellFolderViewCB       = '{2047E320-F2A9-11CE-AE65-08002B2E1262}';
  SID_IServiceProvider         = '{6D5140C1-7436-11CE-8034-00AA006009FA}';
  SID_ICommDlgBrowser2         = '{10339516-2894-11D2-9039-00C04F8EEB3E}';
  SID_ICommDlgBrowser3         = '{C8AD25A1-3294-41EE-8165-71174BD01C57}';
  SID_IExplorerBrowserEvents   = '{361BBDC7-E6EE-4E13-BE58-58E2240C810F}';
  SID_IExplorerBrowser         = '{DFD3B6B5-C10C-4BE9-85F6-A66969F402F6}';
  SID_IAutoComplete            = '{00BB2762-6A77-11D0-A535-00C04FD7D062}';
  SID_IAutoComplete2           = '{EAC04BC0-3791-11D2-BB95-0060977B464C}';
  SID_IACList                  = '{77A130B0-94FD-11D0-A544-00C04FD7d062}';
  SID_IACList2                 = '{470141A0-5186-11D2-BBB6-0060977B464C}';
  SID_ICurrentWorkingDirectory = '{91956D21-9276-11D1-921A-006097DF5BD4}';
  SID_IObjMgr                  = '{00BB2761-6A77-11D0-A535-00C04FD7D062}';

  // 0057D0E0-3573-11CF-AE69-08002B2E1262
  VID_LargeIcons: TGUID = (
    D1:$0057D0E0; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  // 089000C0-3573-11CF-AE69-08002B2E1262
  VID_SmallIcons: TGUID = (
    D1:$089000C0; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  // 0E1FA5E0-3573-11CF-AE69-08002B2E1262
  VID_List: TGUID = (
    D1:$0E1FA5E0; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  // 137E7700-3573-11CF-AE69-08002B2E1262
  VID_Details: TGUID = (
    D1:$137E7700; D2:$3573; D3:$11CF; D4:($AE,$69,$08,$00,$2B,$2E,$12,$62));
  // 5984FFE0-28D4-11CF-AE66-08002B2E1262
  VID_Tile: TGUID = (
    D1:$65F125E5; D2:$7BE1; D3:$4810; D4:($BA,$9D,$D2,$71,$C8,$43,$2C,$E3));
  // 8BEBB290-52D0-11d0-B7F4-00C04FD706EC
  VID_Thumbnails: TGUID = (
    D1:$8BEBB290; D2:$52D0; D3:$11D0; D4:($B7,$F4,$00,$C0,$4F,$D7,$06,$EC));
  // 8EEFA624-D1E9-445B-94B7-74FBCE2EA11A
  VID_ThumbStrip: TGUID = (
    D1:$8EEFA624; D2:$D1E9; D3:$445B; D4:($94,$B7,$74,$FB,$CE,$2E,$A1,$1A));

  WM_GETISHELLBROWSER = WM_USER + 7;

  FWF_AUTOARRANGE  = $1;
  FWF_ABBREVIATEDNAMES = $2;
  FWF_SNAPTOGRID = $4;
  FWF_OWNERDATA = $8;
  FWF_BESTFITWINDOW = $10;
  FWF_DESKTOP = $20;
  FWF_SINGLESEL = $40;
  FWF_NOSUBFOLDERS = $80;
  FWF_TRANSPARENT = $100;
  FWF_NOCLIENTEDGE = $200;
  FWF_NOSCROLL = $400;
  FWF_ALIGNLEFT = $800;
  FWF_NOICONS = $1000;
  FWF_SHOWSELALWAYS = $2000;
  FWF_NOVISIBLE = $4000;
  FWF_SINGLECLICKACTIVATE = $8000;
  FWF_NOWEBVIEW = $10000;
  FWF_HIDEFILENAMES = $20000;
  FWF_CHECKSELECT = $40000;

  FVM_ICON = 1;
  FVM_SMALLICON = 2;
  FVM_LIST = 3;
  FVM_DETAILS = 4;
  FVM_THUMBNAIL = 5;
  FVM_TILE = 6;
  FVM_THUMBSTRIP = 7;

  SVSI_DESELECT = $00000000;
  SVSI_SELECT = $00000001;
  SVSI_EDIT = $00000003;
  SVSI_DESELECTOTHERS = $00000004;
  SVSI_ENSUREVISIBLE = $00000008;
  SVSI_FOCUSED = $00000010;
  SVSI_TRANSLATEPT = $00000020;
  SVSI_SELECTIONMARK = $00000040;
  SVSI_POSITIONITEM = $00000080;
  SVSI_CHECK = $00000100;
  SVSI_NOSTATECHANGE = $80000000;

  CDB2N_CONTEXTMENU_DONE = $00000001;
  CDB2N_CONTEXTMENU_START = $00000002;

  CDB2GVF_SHOWALLFILES = $00000001;

  SBSP_DEFBROWSER         = $0000;
  SBSP_SAMEBROWSER        = $0001;
  SBSP_NEWBROWSER         = $0002;
  SBSP_DEFMODE            = $0000;
  SBSP_OPENMODE           = $0010;
  SBSP_EXPLOREMODE        = $0020;
  SBSP_HELPMODE           = $0040;
  SBSP_NOTRANSFERHIST     = $0080;
  SBSP_ABSOLUTE           = $0000;
  SBSP_RELATIVE           = $1000;
  SBSP_PARENT             = $2000;
  SBSP_NAVIGATEBACK       = $4000;
  SBSP_NAVIGATEFORWARD    = $8000;
  SBSP_ALLOW_AUTONAVIGATE = $00010000;

{ EXPLORER_BROWSER_OPTIONS }

  EBO_NONE                = $0;
  EBO_NAVIGATEONCE        = $1;
  EBO_SHOWFRAMES          = $2;
  EBO_ALWAYSNAVIGATE      = $4;
  EBO_NOTRAVELLOG         = $8;
  EBO_NOWRAPPERWINDOW     = $10;
  EBO_HTMLSHAREPOINTVIEW  = $20;

 { EXPLORER_BROWSER_FILL_FLAGS }

  EBF_NONE                 = $000;
  EBF_SELECTFROMDATAOBJECT = $100;
  EBF_NODROPTARGET         = $200;

  //                         uMsg   wParam             lParam
  SFVM_MERGEMENU          =  1;  // -                  LPQCMINFO
  SFVM_INVOKECOMMAND      =  2;  // idCmd              -
  SFVM_GETHELPTEXT        =  3;  // idCmd,cchMax       pszText
  SFVM_GETTOOLTIPTEXT     =  4;  // idCmd,cchMax       pszText
  SFVM_GETBUTTONINFO      =  5;  // -                  LPTBINFO
  SFVM_GETBUTTONS         =  6;  // idCmdFirst,cbtnMax LPTBBUTTON
  SFVM_INITMENUPOPUP      =  7;  // idCmdFirst,nIndex  hmenu
  SFVM_FSNOTIFY           = 14;  // LPCITEMIDLIST*     lEvent
  SFVM_WINDOWCREATED      = 15;  // hwnd               -
  SFVM_GETDETAILSOF       = 23;  // iColumn            DETAILSINFO*
  SFVM_COLUMNCLICK        = 24;  // iColumn            -
  SFVM_QUERYFSNOTIFY      = 25;  // -                  SHChangeNotifyEntry *
  SFVM_DEFITEMCOUNT       = 26;  // -                  UINT*
  SFVM_DEFVIEWMODE        = 27;  // -                  FOLDERVIEWMODE*
  SFVM_UNMERGEMENU        = 28;  // -                  hmenu
  SFVM_UPDATESTATUSBAR    = 31;  // fInitialize        -
  SFVM_BACKGROUNDENUM     = 32;  // -                  -
  SFVM_DIDDRAGDROP        = 36;  // dwEffect           IDataObject *
  SFVM_SETISFV            = 39;  // -                  IShellFolderView*
  SFVM_THISIDLIST         = 41;  // -                  LPITMIDLIST*
  SFVM_ADDPROPERTYPAGES   = 47;  // -                  SFVM_PROPPAGE_DATA *
  SFVM_BACKGROUNDENUMDONE = 48;  // -                  -
  SFVM_GETNOTIFY          = 49;  // LPITEMIDLIST*      LONG*
// Note: SFVM_QUERYSTANDARDVIEWS NOT USED: must use SFVM_GETVIEWDATA instead
  SFVM_GETSORTDEFAULTS    = 53;  // iDirection         iParamSort
  SFVM_SIZE               = 57;  // -                  -
  SFVM_GETZONE            = 58;  // -                  DWORD*
  SFVM_GETPANE            = 59;  // Pane ID            DWORD*
  SFVM_GETHELPTOPIC       = 63;  // -                  SFVM_HELPTOPIC_DATA *
  SFVM_GETANIMATION       = 68;  // HINSTANCE *        WCHAR *

type
  TCVWParams = record
    cbSize: DWORD;
    svPrev: IShellView;
    fs: PFolderSettings;
    sbOwner: IShellBrowser;
    rcView: TRect;
    pvid: PGUID;
    hwndView: HWND;
  end;

  IShellView2 = interface(IShellView)
    [SID_IShellView2]
    function GetView(var vid: TGUID; View: Cardinal): HResult; stdcall;
    function CreateViewWindow2(const Params: TCVWParams): HResult; stdcall;
    function HandleRename(pidlNew: PItemIDList): HResult; stdcall;
    function SelectAndPositionItem(pidlItem: PItemIDList; uFlags: Cardinal; ppt: PPoint): HResult; stdcall;
  end;

  IFolderView = interface(IUnknown)
    [SID_IFolderView]
    function GetCurrentViewMode(var pViewMode: uint): HResult; stdcall;
    function SetCurrentViewMode(ViewMode: uint): HResult; stdcall;
    function GetFolder(const riid: TIID; out ppv): HResult; stdcall;
    function Item(ItemIndex: integer; var pidl: PItemIdList): HResult; stdcall;
    function ItemCount(uFlags: UINT; var pcItems: integer): HResult; stdcall;
    function Items(uFlags: UINT; const riid: TIID; out ppv): HResult; stdcall;
    function GetSelectionMarkedItem(var piItem: integer): HResult; stdcall;
    function GetFocusedItem(var piItem: integer): HResult; stdcall;
    function GetItemPosition(pidl: PITEMIDLIST; var pt: PPoint): HResult; stdcall;
    function GetSpacing(var pt: PPOINT): HResult; stdcall;
    function GetDefaultSpacing(var pt: PPOINT): HResult; stdcall;
    function GetAutoArrange: HResult; stdcall;
    // like IShellView::SelectItem() by index
    function SelectItem(iItem: integer; dwFlags: dword): HResult; stdcall;
    function SelectAndPositionItems(cidl: uint; apidl: PITEMIDLIST; apt: PPOINT;
      dwFlags: dword): HResult; stdcall;
  end;

  IShellFolderViewCB = interface(IUnknown)
    [SID_IShellFolderViewCB]
    function MessageSFVCB(uMsg: Cardinal; wParam, lParam: Integer): HResult; stdcall;
  end;

  IServiceProvider = interface(IUnknown)
    [SID_IServiceProvider]
    function RemoteQueryService(const guidService, riid: TGUID;
      out ppvObject: IUnknown): HResult; stdcall;
  end;

  ICommDlgBrowser2 = interface(ICommDlgBrowser)
    [SID_ICommDlgBrowser2]
    function Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
      cchMax: Integer): HResult; stdcall;
    function GetViewFlags(out pdwFlags: DWORD): HResult; stdcall;
  end;

  ICommDlgBrowser3 = interface(ICommDlgBrowser2)
    [SID_ICommDlgBrowser3]
    function OnColumnClicked(ppshv: IShellView; iColumn: Integer): HResult; stdcall;
    function GetCurrentFilter(pszFileSpec:  PWideChar; cchFileSpec: Integer): HResult; stdcall;
    function OnPreViewCreated(ppshv: IShellView): HResult; stdcall;
  end;

  IExplorerBrowserEvents = interface(IUnknown)
    [SID_IExplorerBrowserEvents]
    function OnNavigationPending(pidlFolder: PItemIDList): HResult; stdcall;
    function OnViewCreated(dv: IShellView): HResult; stdcall;
    function OnNavigationComplete(pidlFolder: PItemIDList): HResult; stdcall;
    function OnNavigationFailed(pidlFolder: PItemIDList): HResult; stdcall;
  end;

  IExplorerBrowser = interface(IUnknown)
    [SID_IExplorerBrowser]
    function Initialize(hwndParent: HWND; const rc: TRect;
      const fs: TFolderSettings): HResult; stdcall;
    function Destroy: HResult; stdcall;
    function SetRect(phdwp: PHandle; const rcBrowser: TRect): HResult; stdcall;
    function SetPropertyBag(pszPropertyBag: PWideChar): HResult; stdcall;
    function SetEmptyText(pszEmptyText: PWideChar): HResult; stdcall;
    function SetFolderSettings(const fs: TFolderSettings): HResult; stdcall;
    function Advise(sbe: IExplorerBrowserEvents;
      out dwCookie: DWORD): HResult; stdcall;
    function Unadvise(dwCookie: DWORD): HResult; stdcall;
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out dwFlag: DWORD): HResult; stdcall;
    function BrowseToIDList(pidl: PITEMIDLIST;
      uFlags: Cardinal): HResult; stdcall;
    function BrowseToObject(unk: IUnknown; uFlags: Cardinal): HResult; stdcall;
    function FillFromObject(unk: IUnknown; dwFlags: DWORD): HResult; stdcall;
    function RemoveAll: HResult; stdcall;
    function GetCurrentView(const riid: TGUID; out view): HResult; stdcall;
  end;

  IAutoComplete = interface(IUnknown)
    [SID_IAutoComplete]
    function Init(hwndEdit: HWND; punkACL: IUnknown; pwszRegKeyPath: PWideChar;
      pwszQuickComplete: PWideChar): HResult; stdcall;
    function Enable(fEnable: Boolean): HResult; stdcall;
   end;

const
  ACO_NONE               = $0000;
  ACO_AUTOSUGGEST        = $0001;
  ACO_AUTOAPPEND         = $0002;
  ACO_SEARCH             = $0004;
  ACO_FILTERPREFIXES     = $0008;
  ACO_USETAB             = $0010;
  ACO_UPDOWNKEYDROPSLIST = $0020;
  ACO_RTLREADING         = $0040;

type
  IAutoComplete2 = interface(IAutoComplete)
    [SID_IAutoComplete2]
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out dwFlag: DWORD): HResult; stdcall;
  end;

  IACList = interface(IUnknown)
    [SID_IACList]
    function Expand(pszExpand: PWideChar): HResult; stdcall;
  end;

const
  ACLO_NONE            = 0;    // don't enumerate anything
  ACLO_CURRENTDIR      = 1;    // enumerate current directory
  ACLO_MYCOMPUTER      = 2;    // enumerate MyComputer
  ACLO_DESKTOP         = 4;    // enumerate Desktop Folder
  ACLO_FAVORITES       = 8;    // enumerate Favorites Folder
  ACLO_FILESYSONLY     = 16;   // enumerate only the file system

type
  IACList2 = interface(IACList)
    [SID_IACList2]
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

  ICurrentWorkingDirectory = interface(IUnknown)
    [SID_ICurrentWorkingDirectory]
    function GetDirectory(pwzPath: PWideChar; cchSize: DWORD): HResult; stdcall;
    function SetDirectory(pwzPath: PWideChar): HResult; stdcall;
  end;

  IObjMgr = interface(IUnknown)
    [SID_IObjMgr]
    function Append(punk: IUnknown): HResult; stdcall;
    function Remove(punk: IUnknown): HResult; stdcall;
  end;


type
  TSFVCreate = packed record
    cbSize: DWORD;
    shf: IShellFolder;
    svOuter: IShellView;
    sfvcb: IShellFolderViewCB;
  end;
  PSFVCreate = ^TSFVCreate;

function SHCreateShellFolderView(const csfv: TSFVCreate; out sv: IShellView): HResult; stdcall;

const
  SHCNE_RENAMEITEM                 = $00000001;
  SHCNE_CREATE                     = $00000002;
  SHCNE_DELETE                     = $00000004;
  SHCNE_MKDIR                      = $00000008;
  SHCNE_RMDIR                      = $00000010;
  SHCNE_MEDIAINSERTED              = $00000020;
  SHCNE_MEDIAREMOVED               = $00000040;
  SHCNE_DRIVEREMOVED               = $00000080;
  SHCNE_DRIVEADD                   = $00000100;
  SHCNE_NETSHARE                   = $00000200;
  SHCNE_NETUNSHARE                 = $00000400;
  SHCNE_ATTRIBUTES                 = $00000800;
  SHCNE_UPDATEDIR                  = $00001000;
  SHCNE_UPDATEITEM                 = $00002000;
  SHCNE_SERVERDISCONNECT           = $00004000;
  SHCNE_UPDATEIMAGE                = $00008000;
  SHCNE_DRIVEADDGUI                = $00010000;
  SHCNE_RENAMEFOLDER               = $00020000;
  SHCNE_FREESPACE                  = $00040000;
  SHCNE_EXTENDED_EVENT             = $04000000;
  SHCNE_ASSOCCHANGED               = $08000000;
  SHCNE_DISKEVENTS                 = $0002381F;
  SHCNE_GLOBALEVENTS               = $0C0581E0;
  SHCNE_ALLEVENTS                  = $7FFFFFFF;
  SHCNE_INTERRUPT                  = $80000000;

  SHCNEE_ORDERCHANGED              = $0002;
  SHCNEE_MSI_CHANGE                = $0004;
  SHCNEE_MSI_UNINSTALL             = $0005;

  SHCNF_IDLIST                     = $0000;
  SHCNF_PATHA                      = $0001;
  SHCNF_PRINTERA                   = $0002;
  SHCNF_DWORD                      = $0003;
  SHCNF_PATHW                      = $0005;
  SHCNF_PRINTERW                   = $0006;
  SHCNF_TYPE                       = $00FF;
  SHCNF_FLUSH                      = $1000;
  SHCNF_FLUSHNOWAIT                = $2000;

  SHCNF_ACCEPT_INTERRUPTS          = $0001;
  SHCNF_ACCEPT_NON_INTERRUPTS      = $0002;

  SHCNRF_InterruptLevel            = $0001;
  SHCNRF_ShellLevel                = $0002;
  SHCNRF_RecursiveInterrupt        = $1000;
  SHCNRF_NewDelivery               = $8000;

type
  TSHChangeNotifyEntry = record
    AbsoluteList: PItemIDList;
    WatchSubtree: BOOL;
  end;
  PSHChangeNotifyEntry = ^TSHChangeNotifyEntry;

  TSHNotifyStruct = packed record
    Item1: PItemIDList;
    Item2: PItemIDList;
  end;
  PSHNotifyStruct = ^TSHNotifyStruct;

const
  WM_SHELLNOTIFY = WM_USER + $50;

type
  TWMShellNotify = packed record
    Msg: Cardinal;
    Notify: PSHNotifyStruct;
    Code: Cardinal;
    Result: Longint;
  end;

function SHChangeNotifyRegister(Wnd: HWND; Flags: DWORD; EventMask: ULONG;
  Msg: UINT; Count: DWORD; const Items: TSHChangeNotifyEntry): THandle; stdcall;
function SHChangeNotifyDeregister(Notify: THandle): BOOL; stdcall;

implementation

const
  shell32 = 'shell32.dll';

function SHCreateShellFolderView; external shell32;
function SHChangeNotifyRegister; external shell32 index 2;
function SHChangeNotifyDeregister; external shell32 index 4;

end.
