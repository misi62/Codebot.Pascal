unit ShellDocView;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 5/17/2008 8:24:05 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\WINDOWS\system32\ieframe.dll (1)
// LIBID: {EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}
// LCID: 0
// Helpfile: 
// HelpString: Microsoft Internet Controls
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Parameter 'Type' of IWebBrowser.Type changed to 'Type_'
//   Hint: Parameter 'Property' of DWebBrowserEvents.PropertyChange changed to 'Property_'
//   Hint: Parameter 'Property' of IWebBrowserApp.PutProperty changed to 'Property_'
//   Hint: Parameter 'Property' of IWebBrowserApp.GetProperty changed to 'Property_'
//   Hint: Parameter 'Type' of IShellUIHelper.AddDesktopComponent changed to 'Type_'
//   Hint: Parameter 'var' of IShellNameSpace.Expand changed to 'var_'
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.

{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}

interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, OleServer, StdVCL, Variants;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  SHDocVwMajorVersion = 1;
  SHDocVwMinorVersion = 1;

  LIBID_SHDocVw: TGUID = '{EAB22AC0-30C1-11CF-A7EB-0000C05BAE0B}';

  IID_IWebBrowser: TGUID = '{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}';
  DIID_DWebBrowserEvents: TGUID = '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
  IID_IWebBrowserApp: TGUID = '{0002DF05-0000-0000-C000-000000000046}';
  IID_IWebBrowser2: TGUID = '{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}';
  DIID_DWebBrowserEvents2: TGUID = '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
  CLASS_WebBrowser_V1: TGUID = '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
  CLASS_WebBrowser: TGUID = '{8856F961-340A-11D0-A96B-00C04FD705A2}';
  CLASS_InternetExplorer: TGUID = '{0002DF01-0000-0000-C000-000000000046}';
  CLASS_ShellBrowserWindow: TGUID = '{C08AFD90-F2A1-11D1-8455-00A0C91F3880}';
  DIID_DShellWindowsEvents: TGUID = '{FE4106E0-399A-11D0-A48C-00A0C90A8F39}';
  IID_IShellWindows: TGUID = '{85CB6900-4D95-11CF-960C-0080C7F4EE85}';
  CLASS_ShellWindows: TGUID = '{9BA05972-F6A8-11CF-A442-00A0C90A8F39}';
  IID_IShellUIHelper: TGUID = '{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}';
  IID_IShellUIHelper2: TGUID = '{A7FE6EDA-1932-4281-B881-87B31B8BC52C}';
  CLASS_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
  DIID_DShellNameSpaceEvents: TGUID = '{55136806-B2DE-11D1-B9F2-00A0C98BC547}';
  IID_IShellFavoritesNameSpace: TGUID = '{55136804-B2DE-11D1-B9F2-00A0C98BC547}';
  IID_IShellNameSpace: TGUID = '{E572D3C9-37BE-4AE2-825D-D521763E3108}';
  CLASS_ShellNameSpace: TGUID = '{55136805-B2DE-11D1-B9F2-00A0C98BC547}';
  CLASS_ShellShellNameSpace: TGUID = '{2F2F1F96-2BC1-4B1C-BE28-EA3774F4676A}';
  IID_IScriptErrorList: TGUID = '{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}';
  CLASS_CScriptErrorList: TGUID = '{EFD01300-160F-11D2-BB2E-00805FF7EFCA}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum CommandStateChangeConstants
type
  CommandStateChangeConstants = TOleEnum;
const
  CSC_UPDATECOMMANDS = $FFFFFFFF;
  CSC_NAVIGATEFORWARD = $00000001;
  CSC_NAVIGATEBACK = $00000002;

// Constants for enum OLECMDID
type
  OLECMDID = TOleEnum;
const
  OLECMDID_OPEN = $00000001;
  OLECMDID_NEW = $00000002;
  OLECMDID_SAVE = $00000003;
  OLECMDID_SAVEAS = $00000004;
  OLECMDID_SAVECOPYAS = $00000005;
  OLECMDID_PRINT = $00000006;
  OLECMDID_PRINTPREVIEW = $00000007;
  OLECMDID_PAGESETUP = $00000008;
  OLECMDID_SPELL = $00000009;
  OLECMDID_PROPERTIES = $0000000A;
  OLECMDID_CUT = $0000000B;
  OLECMDID_COPY = $0000000C;
  OLECMDID_PASTE = $0000000D;
  OLECMDID_PASTESPECIAL = $0000000E;
  OLECMDID_UNDO = $0000000F;
  OLECMDID_REDO = $00000010;
  OLECMDID_SELECTALL = $00000011;
  OLECMDID_CLEARSELECTION = $00000012;
  OLECMDID_ZOOM = $00000013;
  OLECMDID_GETZOOMRANGE = $00000014;
  OLECMDID_UPDATECOMMANDS = $00000015;
  OLECMDID_REFRESH = $00000016;
  OLECMDID_STOP = $00000017;
  OLECMDID_HIDETOOLBARS = $00000018;
  OLECMDID_SETPROGRESSMAX = $00000019;
  OLECMDID_SETPROGRESSPOS = $0000001A;
  OLECMDID_SETPROGRESSTEXT = $0000001B;
  OLECMDID_SETTITLE = $0000001C;
  OLECMDID_SETDOWNLOADSTATE = $0000001D;
  OLECMDID_STOPDOWNLOAD = $0000001E;
  OLECMDID_ONTOOLBARACTIVATED = $0000001F;
  OLECMDID_FIND = $00000020;
  OLECMDID_DELETE = $00000021;
  OLECMDID_HTTPEQUIV = $00000022;
  OLECMDID_HTTPEQUIV_DONE = $00000023;
  OLECMDID_ENABLE_INTERACTION = $00000024;
  OLECMDID_ONUNLOAD = $00000025;
  OLECMDID_PROPERTYBAG2 = $00000026;
  OLECMDID_PREREFRESH = $00000027;
  OLECMDID_SHOWSCRIPTERROR = $00000028;
  OLECMDID_SHOWMESSAGE = $00000029;
  OLECMDID_SHOWFIND = $0000002A;
  OLECMDID_SHOWPAGESETUP = $0000002B;
  OLECMDID_SHOWPRINT = $0000002C;
  OLECMDID_CLOSE = $0000002D;
  OLECMDID_ALLOWUILESSSAVEAS = $0000002E;
  OLECMDID_DONTDOWNLOADCSS = $0000002F;
  OLECMDID_UPDATEPAGESTATUS = $00000030;
  OLECMDID_PRINT2 = $00000031;
  OLECMDID_PRINTPREVIEW2 = $00000032;
  OLECMDID_SETPRINTTEMPLATE = $00000033;
  OLECMDID_GETPRINTTEMPLATE = $00000034;
  OLECMDID_PAGEACTIONBLOCKED = $00000037;
  OLECMDID_PAGEACTIONUIQUERY = $00000038;
  OLECMDID_FOCUSVIEWCONTROLS = $00000039;
  OLECMDID_FOCUSVIEWCONTROLSQUERY = $0000003A;
  OLECMDID_SHOWPAGEACTIONMENU = $0000003B;
  OLECMDID_ADDTRAVELENTRY = $0000003C;
  OLECMDID_UPDATETRAVELENTRY = $0000003D;
  OLECMDID_UPDATEBACKFORWARDSTATE = $0000003E;
  OLECMDID_OPTICAL_ZOOM = $0000003F;
  OLECMDID_OPTICAL_GETZOOMRANGE = $00000040;
  OLECMDID_WINDOWSTATECHANGED = $00000041;

// Constants for enum OLECMDF
type
  OLECMDF = TOleEnum;
const
  OLECMDF_SUPPORTED = $00000001;
  OLECMDF_ENABLED = $00000002;
  OLECMDF_LATCHED = $00000004;
  OLECMDF_NINCHED = $00000008;
  OLECMDF_INVISIBLE = $00000010;
  OLECMDF_DEFHIDEONCTXTMENU = $00000020;

// Constants for enum OLECMDEXECOPT
type
  OLECMDEXECOPT = TOleEnum;
const
  OLECMDEXECOPT_DODEFAULT = $00000000;
  OLECMDEXECOPT_PROMPTUSER = $00000001;
  OLECMDEXECOPT_DONTPROMPTUSER = $00000002;
  OLECMDEXECOPT_SHOWHELP = $00000003;

// Constants for enum tagREADYSTATE
type
  tagREADYSTATE = TOleEnum;
const
  READYSTATE_UNINITIALIZED = $00000000;
  READYSTATE_LOADING = $00000001;
  READYSTATE_LOADED = $00000002;
  READYSTATE_INTERACTIVE = $00000003;
  READYSTATE_COMPLETE = $00000004;

// Constants for enum SecureLockIconConstants
type
  SecureLockIconConstants = TOleEnum;
const
  secureLockIconUnsecure = $00000000;
  secureLockIconMixed = $00000001;
  secureLockIconSecureUnknownBits = $00000002;
  secureLockIconSecure40Bit = $00000003;
  secureLockIconSecure56Bit = $00000004;
  secureLockIconSecureFortezza = $00000005;
  secureLockIconSecure128Bit = $00000006;

// Constants for enum ShellWindowTypeConstants
type
  ShellWindowTypeConstants = TOleEnum;
const
  SWC_EXPLORER = $00000000;
  SWC_BROWSER = $00000001;
  SWC_3RDPARTY = $00000002;
  SWC_CALLBACK = $00000004;
  SWC_DESKTOP = $00000008;

// Constants for enum ShellWindowFindWindowOptions
type
  ShellWindowFindWindowOptions = TOleEnum;
const
  SWFO_NEEDDISPATCH = $00000001;
  SWFO_INCLUDEPENDING = $00000002;
  SWFO_COOKIEPASSED = $00000004;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IWebBrowser = interface;
  IWebBrowserDisp = dispinterface;
  DWebBrowserEvents = dispinterface;
  IWebBrowserApp = interface;
  IWebBrowserAppDisp = dispinterface;
  IWebBrowser2 = interface;
  IWebBrowser2Disp = dispinterface;
  DWebBrowserEvents2 = dispinterface;
  DShellWindowsEvents = dispinterface;
  IShellWindows = interface;
  IShellWindowsDisp = dispinterface;
  IShellUIHelper = interface;
  IShellUIHelperDisp = dispinterface;
  IShellUIHelper2 = interface;
  IShellUIHelper2Disp = dispinterface;
  DShellNameSpaceEvents = dispinterface;
  IShellFavoritesNameSpace = interface;
  IShellFavoritesNameSpaceDisp = dispinterface;
  IShellNameSpace = interface;
  IShellNameSpaceDisp = dispinterface;
  IScriptErrorList = interface;
  IScriptErrorListDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  WebBrowser_V1 = IWebBrowser;
  WebBrowser = IWebBrowser2;
  InternetExplorer = IWebBrowser2;
  ShellBrowserWindow = IWebBrowser2;
  ShellWindows = IShellWindows;
  ShellUIHelper = IShellUIHelper2;
  ShellNameSpace = IShellNameSpace;
  ShellShellNameSpace = IShellNameSpace;
  CScriptErrorList = IScriptErrorList;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}


// *********************************************************************//
// Interface: IWebBrowser
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  IWebBrowser = interface(IDispatch)
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; safecall;
    procedure GoForward; safecall;
    procedure GoHome; safecall;
    procedure GoSearch; safecall;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); safecall;
    procedure Refresh; safecall;
    procedure Refresh2(var Level: OleVariant); safecall;
    procedure Stop; safecall;
    function Get_Application: IDispatch; safecall;
    function Get_Parent: IDispatch; safecall;
    function Get_Container: IDispatch; safecall;
    function Get_Document: IDispatch; safecall;
    function Get_TopLevelContainer: WordBool; safecall;
    function Get_type_: WideString; safecall;
    function Get_Left: Integer; safecall;
    procedure Set_Left(pl: Integer); safecall;
    function Get_Top: Integer; safecall;
    procedure Set_Top(pl: Integer); safecall;
    function Get_Width: Integer; safecall;
    procedure Set_Width(pl: Integer); safecall;
    function Get_Height: Integer; safecall;
    procedure Set_Height(pl: Integer); safecall;
    function Get_LocationName: WideString; safecall;
    function Get_LocationURL: WideString; safecall;
    function Get_Busy: WordBool; safecall;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Container: IDispatch read Get_Container;
    property Document: IDispatch read Get_Document;
    property TopLevelContainer: WordBool read Get_TopLevelContainer;
    property type_: WideString read Get_type_;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property LocationName: WideString read Get_LocationName;
    property LocationURL: WideString read Get_LocationURL;
    property Busy: WordBool read Get_Busy;
  end;

// *********************************************************************//
// DispIntf:  IWebBrowserDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  IWebBrowserDisp = dispinterface
    ['{EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// DispIntf:  DWebBrowserEvents
// Flags:     (4112) Hidden Dispatchable
// GUID:      {EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}
// *********************************************************************//
  DWebBrowserEvents = dispinterface
    ['{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}']
    procedure BeforeNavigate(const URL: WideString; Flags: Integer; 
                             const TargetFrameName: WideString; var PostData: OleVariant; 
                             const Headers: WideString; var Cancel: WordBool); dispid 100;
    procedure NavigateComplete(const URL: WideString); dispid 101;
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress: Integer; ProgressMax: Integer); dispid 108;
    procedure DownloadComplete; dispid 104;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure NewWindow(const URL: WideString; Flags: Integer; const TargetFrameName: WideString; 
                        var PostData: OleVariant; const Headers: WideString; var Processed: WordBool); dispid 107;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure FrameBeforeNavigate(const URL: WideString; Flags: Integer; 
                                  const TargetFrameName: WideString; var PostData: OleVariant; 
                                  const Headers: WideString; var Cancel: WordBool); dispid 200;
    procedure FrameNavigateComplete(const URL: WideString); dispid 201;
    procedure FrameNewWindow(const URL: WideString; Flags: Integer; 
                             const TargetFrameName: WideString; var PostData: OleVariant; 
                             const Headers: WideString; var Processed: WordBool); dispid 204;
    procedure Quit(var Cancel: WordBool); dispid 103;
    procedure WindowMove; dispid 109;
    procedure WindowResize; dispid 110;
    procedure WindowActivate; dispid 111;
    procedure PropertyChange(const Property_: WideString); dispid 112;
  end;

// *********************************************************************//
// Interface: IWebBrowserApp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0002DF05-0000-0000-C000-000000000046}
// *********************************************************************//
  IWebBrowserApp = interface(IWebBrowser)
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; safecall;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); safecall;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); safecall;
    function GetProperty(const Property_: WideString): OleVariant; safecall;
    function Get_Name: WideString; safecall;
    function Get_HWND: Integer; safecall;
    function Get_FullName: WideString; safecall;
    function Get_Path: WideString; safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(pBool: WordBool); safecall;
    function Get_StatusBar: WordBool; safecall;
    procedure Set_StatusBar(pBool: WordBool); safecall;
    function Get_StatusText: WideString; safecall;
    procedure Set_StatusText(const StatusText: WideString); safecall;
    function Get_ToolBar: SYSINT; safecall;
    procedure Set_ToolBar(Value: SYSINT); safecall;
    function Get_MenuBar: WordBool; safecall;
    procedure Set_MenuBar(Value: WordBool); safecall;
    function Get_FullScreen: WordBool; safecall;
    procedure Set_FullScreen(pbFullScreen: WordBool); safecall;
    property Name: WideString read Get_Name;
    property HWND: Integer read Get_HWND;
    property FullName: WideString read Get_FullName;
    property Path: WideString read Get_Path;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property StatusBar: WordBool read Get_StatusBar write Set_StatusBar;
    property StatusText: WideString read Get_StatusText write Set_StatusText;
    property ToolBar: SYSINT read Get_ToolBar write Set_ToolBar;
    property MenuBar: WordBool read Get_MenuBar write Set_MenuBar;
    property FullScreen: WordBool read Get_FullScreen write Set_FullScreen;
  end;

// *********************************************************************//
// DispIntf:  IWebBrowserAppDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {0002DF05-0000-0000-C000-000000000046}
// *********************************************************************//
  IWebBrowserAppDisp = dispinterface
    ['{0002DF05-0000-0000-C000-000000000046}']
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid -515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// Interface: IWebBrowser2
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}
// *********************************************************************//
  IWebBrowser2 = interface(IWebBrowserApp)
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant; 
                        var Headers: OleVariant); safecall;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; safecall;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                     var pvaOut: OleVariant); safecall;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                             var pvarSize: OleVariant); safecall;
    function Get_ReadyState: tagREADYSTATE; safecall;
    function Get_Offline: WordBool; safecall;
    procedure Set_Offline(pbOffline: WordBool); safecall;
    function Get_Silent: WordBool; safecall;
    procedure Set_Silent(pbSilent: WordBool); safecall;
    function Get_RegisterAsBrowser: WordBool; safecall;
    procedure Set_RegisterAsBrowser(pbRegister: WordBool); safecall;
    function Get_RegisterAsDropTarget: WordBool; safecall;
    procedure Set_RegisterAsDropTarget(pbRegister: WordBool); safecall;
    function Get_TheaterMode: WordBool; safecall;
    procedure Set_TheaterMode(pbRegister: WordBool); safecall;
    function Get_AddressBar: WordBool; safecall;
    procedure Set_AddressBar(Value: WordBool); safecall;
    function Get_Resizable: WordBool; safecall;
    procedure Set_Resizable(Value: WordBool); safecall;
    property ReadyState: tagREADYSTATE read Get_ReadyState;
    property Offline: WordBool read Get_Offline write Set_Offline;
    property Silent: WordBool read Get_Silent write Set_Silent;
    property RegisterAsBrowser: WordBool read Get_RegisterAsBrowser write Set_RegisterAsBrowser;
    property RegisterAsDropTarget: WordBool read Get_RegisterAsDropTarget write Set_RegisterAsDropTarget;
    property TheaterMode: WordBool read Get_TheaterMode write Set_TheaterMode;
    property AddressBar: WordBool read Get_AddressBar write Set_AddressBar;
    property Resizable: WordBool read Get_Resizable write Set_Resizable;
  end;

// *********************************************************************//
// DispIntf:  IWebBrowser2Disp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}
// *********************************************************************//
  IWebBrowser2Disp = dispinterface
    ['{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}']
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant; 
                        var Headers: OleVariant); dispid 500;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF; dispid 501;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                     var pvaOut: OleVariant); dispid 502;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                             var pvarSize: OleVariant); dispid 503;
    property ReadyState: tagREADYSTATE readonly dispid -525;
    property Offline: WordBool dispid 550;
    property Silent: WordBool dispid 551;
    property RegisterAsBrowser: WordBool dispid 552;
    property RegisterAsDropTarget: WordBool dispid 553;
    property TheaterMode: WordBool dispid 554;
    property AddressBar: WordBool dispid 555;
    property Resizable: WordBool dispid 556;
    procedure Quit; dispid 300;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT); dispid 301;
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant); dispid 302;
    function GetProperty(const Property_: WideString): OleVariant; dispid 303;
    property Name: WideString readonly dispid 0;
    property HWND: Integer readonly dispid -515;
    property FullName: WideString readonly dispid 400;
    property Path: WideString readonly dispid 401;
    property Visible: WordBool dispid 402;
    property StatusBar: WordBool dispid 403;
    property StatusText: WideString dispid 404;
    property ToolBar: SYSINT dispid 405;
    property MenuBar: WordBool dispid 406;
    property FullScreen: WordBool dispid 407;
    procedure GoBack; dispid 100;
    procedure GoForward; dispid 101;
    procedure GoHome; dispid 102;
    procedure GoSearch; dispid 103;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); dispid 104;
    procedure Refresh; dispid -550;
    procedure Refresh2(var Level: OleVariant); dispid 105;
    procedure Stop; dispid 106;
    property Application: IDispatch readonly dispid 200;
    property Parent: IDispatch readonly dispid 201;
    property Container: IDispatch readonly dispid 202;
    property Document: IDispatch readonly dispid 203;
    property TopLevelContainer: WordBool readonly dispid 204;
    property type_: WideString readonly dispid 205;
    property Left: Integer dispid 206;
    property Top: Integer dispid 207;
    property Width: Integer dispid 208;
    property Height: Integer dispid 209;
    property LocationName: WideString readonly dispid 210;
    property LocationURL: WideString readonly dispid 211;
    property Busy: WordBool readonly dispid 212;
  end;

// *********************************************************************//
// DispIntf:  DWebBrowserEvents2
// Flags:     (4112) Hidden Dispatchable
// GUID:      {34A715A0-6587-11D0-924A-0020AFC7AC4D}
// *********************************************************************//
  DWebBrowserEvents2 = dispinterface
    ['{34A715A0-6587-11D0-924A-0020AFC7AC4D}']
    procedure StatusTextChange(const Text: WideString); dispid 102;
    procedure ProgressChange(Progress: Integer; ProgressMax: Integer); dispid 108;
    procedure CommandStateChange(Command: Integer; Enable: WordBool); dispid 105;
    procedure DownloadBegin; dispid 106;
    procedure DownloadComplete; dispid 104;
    procedure TitleChange(const Text: WideString); dispid 113;
    procedure PropertyChange(const szProperty: WideString); dispid 112;
    procedure BeforeNavigate2(const pDisp: IDispatch; var URL: OleVariant; var Flags: OleVariant; 
                              var TargetFrameName: OleVariant; var PostData: OleVariant; 
                              var Headers: OleVariant; var Cancel: WordBool); dispid 250;
    procedure NewWindow2(var ppDisp: IDispatch; var Cancel: WordBool); dispid 251;
    procedure NavigateComplete2(const pDisp: IDispatch; var URL: OleVariant); dispid 252;
    procedure DocumentComplete(const pDisp: IDispatch; var URL: OleVariant); dispid 259;
    procedure OnQuit; dispid 253;
    procedure OnVisible(Visible: WordBool); dispid 254;
    procedure OnToolBar(ToolBar: WordBool); dispid 255;
    procedure OnMenuBar(MenuBar: WordBool); dispid 256;
    procedure OnStatusBar(StatusBar: WordBool); dispid 257;
    procedure OnFullScreen(FullScreen: WordBool); dispid 258;
    procedure OnTheaterMode(TheaterMode: WordBool); dispid 260;
    procedure WindowSetResizable(Resizable: WordBool); dispid 262;
    procedure WindowSetLeft(Left: Integer); dispid 264;
    procedure WindowSetTop(Top: Integer); dispid 265;
    procedure WindowSetWidth(Width: Integer); dispid 266;
    procedure WindowSetHeight(Height: Integer); dispid 267;
    procedure WindowClosing(IsChildWindow: WordBool; var Cancel: WordBool); dispid 263;
    procedure ClientToHostWindow(var CX: Integer; var CY: Integer); dispid 268;
    procedure SetSecureLockIcon(SecureLockIcon: Integer); dispid 269;
    procedure FileDownload(ActiveDocument: WordBool; var Cancel: WordBool); dispid 270;
    procedure NavigateError(const pDisp: IDispatch; var URL: OleVariant; var Frame: OleVariant; 
                            var StatusCode: OleVariant; var Cancel: WordBool); dispid 271;
    procedure PrintTemplateInstantiation(const pDisp: IDispatch); dispid 225;
    procedure PrintTemplateTeardown(const pDisp: IDispatch); dispid 226;
    procedure UpdatePageStatus(const pDisp: IDispatch; var nPage: OleVariant; var fDone: OleVariant); dispid 227;
    procedure PrivacyImpactedStateChange(bImpacted: WordBool); dispid 272;
    procedure NewWindow3(var ppDisp: IDispatch; var Cancel: WordBool; dwFlags: LongWord; 
                         const bstrUrlContext: WideString; const bstrUrl: WideString); dispid 273;
    procedure SetPhishingFilterStatus(PhishingFilterStatus: Integer); dispid 282;
    procedure WindowStateChanged(dwWindowStateFlags: LongWord; dwValidFlagsMask: LongWord); dispid 283;
  end;

// *********************************************************************//
// DispIntf:  DShellWindowsEvents
// Flags:     (4096) Dispatchable
// GUID:      {FE4106E0-399A-11D0-A48C-00A0C90A8F39}
// *********************************************************************//
  DShellWindowsEvents = dispinterface
    ['{FE4106E0-399A-11D0-A48C-00A0C90A8F39}']
    procedure WindowRegistered(lCookie: Integer); dispid 200;
    procedure WindowRevoked(lCookie: Integer); dispid 201;
  end;

// *********************************************************************//
// Interface: IShellWindows
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85CB6900-4D95-11CF-960C-0080C7F4EE85}
// *********************************************************************//
  IShellWindows = interface(IDispatch)
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    function Get_Count: Integer; safecall;
    function Item(index: OleVariant): IDispatch; safecall;
    function _NewEnum: IUnknown; safecall;
    procedure Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure RegisterPending(lThreadId: Integer; var pvarloc: OleVariant; 
                              var pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); safecall;
    procedure Revoke(lCookie: Integer); safecall;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); safecall;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); safecall;
    function FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; swClass: SYSINT; 
                          out pHWND: Integer; swfwOptions: SYSINT): IDispatch; safecall;
    procedure OnCreated(lCookie: Integer; const punk: IUnknown); safecall;
    procedure ProcessAttachDetach(fAttach: WordBool); safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IShellWindowsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {85CB6900-4D95-11CF-960C-0080C7F4EE85}
// *********************************************************************//
  IShellWindowsDisp = dispinterface
    ['{85CB6900-4D95-11CF-960C-0080C7F4EE85}']
    property Count: Integer readonly dispid 1610743808;
    function Item(index: OleVariant): IDispatch; dispid 0;
    function _NewEnum: IUnknown; dispid -4;
    procedure Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer); dispid 1610743811;
    procedure RegisterPending(lThreadId: Integer; var pvarloc: OleVariant; 
                              var pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer); dispid 1610743812;
    procedure Revoke(lCookie: Integer); dispid 1610743813;
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant); dispid 1610743814;
    procedure OnActivated(lCookie: Integer; fActive: WordBool); dispid 1610743815;
    function FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; swClass: SYSINT; 
                          out pHWND: Integer; swfwOptions: SYSINT): IDispatch; dispid 1610743816;
    procedure OnCreated(lCookie: Integer; const punk: IUnknown); dispid 1610743817;
    procedure ProcessAttachDetach(fAttach: WordBool); dispid 1610743818;
  end;

// *********************************************************************//
// Interface: IShellUIHelper
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}
// *********************************************************************//
  IShellUIHelper = interface(IDispatch)
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; safecall;
    procedure ResetSafeMode; safecall;
    procedure RefreshOfflineDesktop; safecall;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); safecall;
    procedure AddChannel(const URL: WideString); safecall;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant; 
                                  var Height: OleVariant); safecall;
    function IsSubscribed(const URL: WideString): WordBool; safecall;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                              var varTargetFrame: OleVariant); safecall;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); safecall;
    procedure AutoCompleteSaveForm(var Form: OleVariant); safecall;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                       var pvarTargetFrame: OleVariant); safecall;
    procedure AutoCompleteAttach(var Reserved: OleVariant); safecall;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IShellUIHelperDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}
// *********************************************************************//
  IShellUIHelperDisp = dispinterface
    ['{729FE2F8-1EA8-11D1-8F85-00C04FC2FBE1}']
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant; 
                                  var Height: OleVariant); dispid 6;
    function IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                              var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                       var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

// *********************************************************************//
// Interface: IShellUIHelper2
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7FE6EDA-1932-4281-B881-87B31B8BC52C}
// *********************************************************************//
  IShellUIHelper2 = interface(IShellUIHelper)
    ['{A7FE6EDA-1932-4281-B881-87B31B8BC52C}']
    procedure AddSearchProvider(const URL: WideString); safecall;
    procedure RunOnceShown; safecall;
    procedure SkipRunOnce; safecall;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString); safecall;
    function SqmEnabled: WordBool; safecall;
    function PhishingEnabled: WordBool; safecall;
    function BrandImageUri: WideString; safecall;
    procedure SkipTabsWelcome; safecall;
    procedure DiagnoseConnection; safecall;
    procedure CustomizeClearType(fSet: WordBool); safecall;
    function IsSearchProviderInstalled(const URL: WideString): LongWord; safecall;
    function IsSearchMigrated: WordBool; safecall;
    function DefaultSearchProvider: WideString; safecall;
    procedure RunOnceRequiredSettingsComplete(fComplete: WordBool); safecall;
    function RunOnceHasShown: WordBool; safecall;
    function SearchGuideUrl: WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IShellUIHelper2Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A7FE6EDA-1932-4281-B881-87B31B8BC52C}
// *********************************************************************//
  IShellUIHelper2Disp = dispinterface
    ['{A7FE6EDA-1932-4281-B881-87B31B8BC52C}']
    procedure AddSearchProvider(const URL: WideString); dispid 14;
    procedure RunOnceShown; dispid 15;
    procedure SkipRunOnce; dispid 16;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString); dispid 17;
    function SqmEnabled: WordBool; dispid 18;
    function PhishingEnabled: WordBool; dispid 19;
    function BrandImageUri: WideString; dispid 20;
    procedure SkipTabsWelcome; dispid 21;
    procedure DiagnoseConnection; dispid 22;
    procedure CustomizeClearType(fSet: WordBool); dispid 23;
    function IsSearchProviderInstalled(const URL: WideString): LongWord; dispid 24;
    function IsSearchMigrated: WordBool; dispid 25;
    function DefaultSearchProvider: WideString; dispid 26;
    procedure RunOnceRequiredSettingsComplete(fComplete: WordBool); dispid 27;
    function RunOnceHasShown: WordBool; dispid 28;
    function SearchGuideUrl: WideString; dispid 29;
    procedure ResetFirstBootMode; dispid 1;
    procedure ResetSafeMode; dispid 2;
    procedure RefreshOfflineDesktop; dispid 3;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); dispid 4;
    procedure AddChannel(const URL: WideString); dispid 5;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant; 
                                  var Height: OleVariant); dispid 6;
    function IsSubscribed(const URL: WideString): WordBool; dispid 7;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                              var varTargetFrame: OleVariant); dispid 8;
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString); dispid 9;
    procedure AutoCompleteSaveForm(var Form: OleVariant); dispid 10;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                       var pvarTargetFrame: OleVariant); dispid 11;
    procedure AutoCompleteAttach(var Reserved: OleVariant); dispid 12;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant; dispid 13;
  end;

// *********************************************************************//
// DispIntf:  DShellNameSpaceEvents
// Flags:     (4096) Dispatchable
// GUID:      {55136806-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  DShellNameSpaceEvents = dispinterface
    ['{55136806-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure FavoritesSelectionChange(cItems: Integer; hItem: Integer; const strName: WideString; 
                                       const strUrl: WideString; cVisits: Integer; 
                                       const strDate: WideString; fAvailableOffline: Integer); dispid 1;
    procedure SelectionChange; dispid 2;
    procedure DoubleClick; dispid 3;
    procedure Initialized; dispid 4;
  end;

// *********************************************************************//
// Interface: IShellFavoritesNameSpace
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {55136804-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  IShellFavoritesNameSpace = interface(IDispatch)
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; safecall;
    procedure MoveSelectionDown; safecall;
    procedure ResetSort; safecall;
    procedure NewFolder; safecall;
    procedure Synchronize; safecall;
    procedure Import; safecall;
    procedure Export; safecall;
    procedure InvokeContextMenuCommand(const strCommand: WideString); safecall;
    procedure MoveSelectionTo; safecall;
    function Get_SubscriptionsEnabled: WordBool; safecall;
    function CreateSubscriptionForSelection: WordBool; safecall;
    function DeleteSubscriptionForSelection: WordBool; safecall;
    procedure SetRoot(const bstrFullPath: WideString); safecall;
    property SubscriptionsEnabled: WordBool read Get_SubscriptionsEnabled;
  end;

// *********************************************************************//
// DispIntf:  IShellFavoritesNameSpaceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {55136804-B2DE-11D1-B9F2-00A0C98BC547}
// *********************************************************************//
  IShellFavoritesNameSpaceDisp = dispinterface
    ['{55136804-B2DE-11D1-B9F2-00A0C98BC547}']
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function CreateSubscriptionForSelection: WordBool; dispid 11;
    function DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
  end;

// *********************************************************************//
// Interface: IShellNameSpace
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E572D3C9-37BE-4AE2-825D-D521763E3108}
// *********************************************************************//
  IShellNameSpace = interface(IShellFavoritesNameSpace)
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    function Get_EnumOptions: Integer; safecall;
    procedure Set_EnumOptions(pgrfEnumFlags: Integer); safecall;
    function Get_SelectedItem: IDispatch; safecall;
    procedure Set_SelectedItem(const pItem: IDispatch); safecall;
    function Get_Root: OleVariant; safecall;
    procedure Set_Root(pvar: OleVariant); safecall;
    function Get_Depth: SYSINT; safecall;
    procedure Set_Depth(piDepth: SYSINT); safecall;
    function Get_Mode: SYSUINT; safecall;
    procedure Set_Mode(puMode: SYSUINT); safecall;
    function Get_Flags: LongWord; safecall;
    procedure Set_Flags(pdwFlags: LongWord); safecall;
    procedure Set_TVFlags(dwFlags: LongWord); safecall;
    function Get_TVFlags: LongWord; safecall;
    function Get_Columns: WideString; safecall;
    procedure Set_Columns(const bstrColumns: WideString); safecall;
    function Get_CountViewTypes: SYSINT; safecall;
    procedure SetViewType(iType: SYSINT); safecall;
    function SelectedItems: IDispatch; safecall;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); safecall;
    procedure UnselectAll; safecall;
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property SelectedItem: IDispatch read Get_SelectedItem write Set_SelectedItem;
    property Root: OleVariant read Get_Root write Set_Root;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property TVFlags: LongWord read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
    property CountViewTypes: SYSINT read Get_CountViewTypes;
  end;

// *********************************************************************//
// DispIntf:  IShellNameSpaceDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {E572D3C9-37BE-4AE2-825D-D521763E3108}
// *********************************************************************//
  IShellNameSpaceDisp = dispinterface
    ['{E572D3C9-37BE-4AE2-825D-D521763E3108}']
    property EnumOptions: Integer dispid 14;
    property SelectedItem: IDispatch dispid 15;
    property Root: OleVariant dispid 16;
    property Depth: SYSINT dispid 17;
    property Mode: SYSUINT dispid 18;
    property Flags: LongWord dispid 19;
    property TVFlags: LongWord dispid 20;
    property Columns: WideString dispid 21;
    property CountViewTypes: SYSINT readonly dispid 22;
    procedure SetViewType(iType: SYSINT); dispid 23;
    function SelectedItems: IDispatch; dispid 24;
    procedure Expand(var_: OleVariant; iDepth: SYSINT); dispid 25;
    procedure UnselectAll; dispid 26;
    procedure MoveSelectionUp; dispid 1;
    procedure MoveSelectionDown; dispid 2;
    procedure ResetSort; dispid 3;
    procedure NewFolder; dispid 4;
    procedure Synchronize; dispid 5;
    procedure Import; dispid 6;
    procedure Export; dispid 7;
    procedure InvokeContextMenuCommand(const strCommand: WideString); dispid 8;
    procedure MoveSelectionTo; dispid 9;
    property SubscriptionsEnabled: WordBool readonly dispid 10;
    function CreateSubscriptionForSelection: WordBool; dispid 11;
    function DeleteSubscriptionForSelection: WordBool; dispid 12;
    procedure SetRoot(const bstrFullPath: WideString); dispid 13;
  end;

// *********************************************************************//
// Interface: IScriptErrorList
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F3470F24-15FD-11D2-BB2E-00805FF7EFCA}
// *********************************************************************//
  IScriptErrorList = interface(IDispatch)
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; safecall;
    procedure retreatError; safecall;
    function canAdvanceError: Integer; safecall;
    function canRetreatError: Integer; safecall;
    function getErrorLine: Integer; safecall;
    function getErrorChar: Integer; safecall;
    function getErrorCode: Integer; safecall;
    function getErrorMsg: WideString; safecall;
    function getErrorUrl: WideString; safecall;
    function getAlwaysShowLockState: Integer; safecall;
    function getDetailsPaneOpen: Integer; safecall;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); safecall;
    function getPerErrorDisplay: Integer; safecall;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IScriptErrorListDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {F3470F24-15FD-11D2-BB2E-00805FF7EFCA}
// *********************************************************************//
  IScriptErrorListDisp = dispinterface
    ['{F3470F24-15FD-11D2-BB2E-00805FF7EFCA}']
    procedure advanceError; dispid 10;
    procedure retreatError; dispid 11;
    function canAdvanceError: Integer; dispid 12;
    function canRetreatError: Integer; dispid 13;
    function getErrorLine: Integer; dispid 14;
    function getErrorChar: Integer; dispid 15;
    function getErrorCode: Integer; dispid 16;
    function getErrorMsg: WideString; dispid 17;
    function getErrorUrl: WideString; dispid 18;
    function getAlwaysShowLockState: Integer; dispid 23;
    function getDetailsPaneOpen: Integer; dispid 19;
    procedure setDetailsPaneOpen(fDetailsPaneOpen: Integer); dispid 20;
    function getPerErrorDisplay: Integer; dispid 21;
    procedure setPerErrorDisplay(fPerErrorDisplay: Integer); dispid 22;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TWebBrowserEx_V1
// Help String      : WebBrowser Control
// Default Interface: IWebBrowser
// Def. Intf. DISP? : No
// Event   Interface: DWebBrowserEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TWebBrowserEx_V1BeforeNavigate = procedure(ASender: TObject; const URL: WideString; Flags: Integer; 
                                                             const TargetFrameName: WideString; 
                                                             var PostData: OleVariant; 
                                                             const Headers: WideString; 
                                                             var Cancel: WordBool) of object;
  TWebBrowserEx_V1NavigateComplete = procedure(ASender: TObject; const URL: WideString) of object;
  TWebBrowserEx_V1StatusTextChange = procedure(ASender: TObject; const Text: WideString) of object;
  TWebBrowserEx_V1ProgressChange = procedure(ASender: TObject; Progress: Integer; ProgressMax: Integer) of object;
  TWebBrowserEx_V1CommandStateChange = procedure(ASender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowserEx_V1NewWindow = procedure(ASender: TObject; const URL: WideString; Flags: Integer; 
                                                        const TargetFrameName: WideString; 
                                                        var PostData: OleVariant; 
                                                        const Headers: WideString; 
                                                        var Processed: WordBool) of object;
  TWebBrowserEx_V1TitleChange = procedure(ASender: TObject; const Text: WideString) of object;
  TWebBrowserEx_V1FrameBeforeNavigate = procedure(ASender: TObject; const URL: WideString; 
                                                                  Flags: Integer; 
                                                                  const TargetFrameName: WideString; 
                                                                  var PostData: OleVariant; 
                                                                  const Headers: WideString; 
                                                                  var Cancel: WordBool) of object;
  TWebBrowserEx_V1FrameNavigateComplete = procedure(ASender: TObject; const URL: WideString) of object;
  TWebBrowserEx_V1FrameNewWindow = procedure(ASender: TObject; const URL: WideString; Flags: Integer; 
                                                             const TargetFrameName: WideString; 
                                                             var PostData: OleVariant; 
                                                             const Headers: WideString; 
                                                             var Processed: WordBool) of object;
  TWebBrowserEx_V1Quit = procedure(ASender: TObject; var Cancel: WordBool) of object;
  TWebBrowserEx_V1PropertyChange = procedure(ASender: TObject; const Property_: WideString) of object;

  TWebBrowserEx_V1 = class(TOleControl)
  private
    FOnBeforeNavigate: TWebBrowserEx_V1BeforeNavigate;
    FOnNavigateComplete: TWebBrowserEx_V1NavigateComplete;
    FOnStatusTextChange: TWebBrowserEx_V1StatusTextChange;
    FOnProgressChange: TWebBrowserEx_V1ProgressChange;
    FOnDownloadComplete: TNotifyEvent;
    FOnCommandStateChange: TWebBrowserEx_V1CommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnNewWindow: TWebBrowserEx_V1NewWindow;
    FOnTitleChange: TWebBrowserEx_V1TitleChange;
    FOnFrameBeforeNavigate: TWebBrowserEx_V1FrameBeforeNavigate;
    FOnFrameNavigateComplete: TWebBrowserEx_V1FrameNavigateComplete;
    FOnFrameNewWindow: TWebBrowserEx_V1FrameNewWindow;
    FOnQuit: TWebBrowserEx_V1Quit;
    FOnWindowMove: TNotifyEvent;
    FOnWindowResize: TNotifyEvent;
    FOnWindowActivate: TNotifyEvent;
    FOnPropertyChange: TWebBrowserEx_V1PropertyChange;
    FIntf: IWebBrowser;
    function  GetControlInterface: IWebBrowser;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Application: IDispatch;
    function Get_Parent: IDispatch;
    function Get_Container: IDispatch;
    function Get_Document: IDispatch;
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); overload;
    procedure Refresh;
    procedure Refresh2; overload;
    procedure Refresh2(var Level: OleVariant); overload;
    procedure Stop;
    property  ControlInterface: IWebBrowser read GetControlInterface;
    property  DefaultInterface: IWebBrowser read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property OnBeforeNavigate: TWebBrowserEx_V1BeforeNavigate read FOnBeforeNavigate write FOnBeforeNavigate;
    property OnNavigateComplete: TWebBrowserEx_V1NavigateComplete read FOnNavigateComplete write FOnNavigateComplete;
    property OnStatusTextChange: TWebBrowserEx_V1StatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowserEx_V1ProgressChange read FOnProgressChange write FOnProgressChange;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnCommandStateChange: TWebBrowserEx_V1CommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnNewWindow: TWebBrowserEx_V1NewWindow read FOnNewWindow write FOnNewWindow;
    property OnTitleChange: TWebBrowserEx_V1TitleChange read FOnTitleChange write FOnTitleChange;
    property OnFrameBeforeNavigate: TWebBrowserEx_V1FrameBeforeNavigate read FOnFrameBeforeNavigate write FOnFrameBeforeNavigate;
    property OnFrameNavigateComplete: TWebBrowserEx_V1FrameNavigateComplete read FOnFrameNavigateComplete write FOnFrameNavigateComplete;
    property OnFrameNewWindow: TWebBrowserEx_V1FrameNewWindow read FOnFrameNewWindow write FOnFrameNewWindow;
    property OnQuit: TWebBrowserEx_V1Quit read FOnQuit write FOnQuit;
    property OnWindowMove: TNotifyEvent read FOnWindowMove write FOnWindowMove;
    property OnWindowResize: TNotifyEvent read FOnWindowResize write FOnWindowResize;
    property OnWindowActivate: TNotifyEvent read FOnWindowActivate write FOnWindowActivate;
    property OnPropertyChange: TWebBrowserEx_V1PropertyChange read FOnPropertyChange write FOnPropertyChange;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TWebBrowserEx
// Help String      : WebBrowser Control
// Default Interface: IWebBrowser2
// Def. Intf. DISP? : No
// Event   Interface: DWebBrowserEvents2
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TWebBrowserExStatusTextChange = procedure(ASender: TObject; const Text: WideString) of object;
  TWebBrowserExProgressChange = procedure(ASender: TObject; Progress: Integer; ProgressMax: Integer) of object;
  TWebBrowserExCommandStateChange = procedure(ASender: TObject; Command: Integer; Enable: WordBool) of object;
  TWebBrowserExTitleChange = procedure(ASender: TObject; const Text: WideString) of object;
  TWebBrowserExPropertyChange = procedure(ASender: TObject; const szProperty: WideString) of object;
  TWebBrowserExBeforeNavigate2 = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                           var URL: OleVariant; 
                                                           var Flags: OleVariant; 
                                                           var TargetFrameName: OleVariant; 
                                                           var PostData: OleVariant; 
                                                           var Headers: OleVariant; 
                                                           var Cancel: WordBool) of object;
  TWebBrowserExNewWindow2 = procedure(ASender: TObject; var ppDisp: IDispatch; var Cancel: WordBool) of object;
  TWebBrowserExNavigateComplete2 = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                             var URL: OleVariant) of object;
  TWebBrowserExDocumentComplete = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                            var URL: OleVariant) of object;
  TWebBrowserExOnVisible = procedure(ASender: TObject; Visible: WordBool) of object;
  TWebBrowserExOnToolBar = procedure(ASender: TObject; ToolBar: WordBool) of object;
  TWebBrowserExOnMenuBar = procedure(ASender: TObject; MenuBar: WordBool) of object;
  TWebBrowserExOnStatusBar = procedure(ASender: TObject; StatusBar: WordBool) of object;
  TWebBrowserExOnFullScreen = procedure(ASender: TObject; FullScreen: WordBool) of object;
  TWebBrowserExOnTheaterMode = procedure(ASender: TObject; TheaterMode: WordBool) of object;
  TWebBrowserExWindowSetResizable = procedure(ASender: TObject; Resizable: WordBool) of object;
  TWebBrowserExWindowSetLeft = procedure(ASender: TObject; Left: Integer) of object;
  TWebBrowserExWindowSetTop = procedure(ASender: TObject; Top: Integer) of object;
  TWebBrowserExWindowSetWidth = procedure(ASender: TObject; Width: Integer) of object;
  TWebBrowserExWindowSetHeight = procedure(ASender: TObject; Height: Integer) of object;
  TWebBrowserExWindowClosing = procedure(ASender: TObject; IsChildWindow: WordBool; 
                                                         var Cancel: WordBool) of object;
  TWebBrowserExClientToHostWindow = procedure(ASender: TObject; var CX: Integer; var CY: Integer) of object;
  TWebBrowserExSetSecureLockIcon = procedure(ASender: TObject; SecureLockIcon: Integer) of object;
  TWebBrowserExFileDownload = procedure(ASender: TObject; ActiveDocument: WordBool; 
                                                        var Cancel: WordBool) of object;
  TWebBrowserExNavigateError = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                         var URL: OleVariant; 
                                                         var Frame: OleVariant; 
                                                         var StatusCode: OleVariant; 
                                                         var Cancel: WordBool) of object;
  TWebBrowserExPrintTemplateInstantiation = procedure(ASender: TObject; const pDisp: IDispatch) of object;
  TWebBrowserExPrintTemplateTeardown = procedure(ASender: TObject; const pDisp: IDispatch) of object;
  TWebBrowserExUpdatePageStatus = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                            var nPage: OleVariant; 
                                                            var fDone: OleVariant) of object;
  TWebBrowserExPrivacyImpactedStateChange = procedure(ASender: TObject; bImpacted: WordBool) of object;
  TWebBrowserExNewWindow3 = procedure(ASender: TObject; var ppDisp: IDispatch; var Cancel: WordBool; 
                                                      dwFlags: LongWord; 
                                                      const bstrUrlContext: WideString; 
                                                      const bstrUrl: WideString) of object;
  TWebBrowserExSetPhishingFilterStatus = procedure(ASender: TObject; PhishingFilterStatus: Integer) of object;
  TWebBrowserExWindowStateChanged = procedure(ASender: TObject; dwWindowStateFlags: LongWord; 
                                                              dwValidFlagsMask: LongWord) of object;

  TWebBrowserEx = class(TOleControl)
  private
    FOnStatusTextChange: TWebBrowserExStatusTextChange;
    FOnProgressChange: TWebBrowserExProgressChange;
    FOnCommandStateChange: TWebBrowserExCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnDownloadComplete: TNotifyEvent;
    FOnTitleChange: TWebBrowserExTitleChange;
    FOnPropertyChange: TWebBrowserExPropertyChange;
    FOnBeforeNavigate2: TWebBrowserExBeforeNavigate2;
    FOnNewWindow2: TWebBrowserExNewWindow2;
    FOnNavigateComplete2: TWebBrowserExNavigateComplete2;
    FOnDocumentComplete: TWebBrowserExDocumentComplete;
    FOnQuit: TNotifyEvent;
    FOnVisible: TWebBrowserExOnVisible;
    FOnToolBar: TWebBrowserExOnToolBar;
    FOnMenuBar: TWebBrowserExOnMenuBar;
    FOnStatusBar: TWebBrowserExOnStatusBar;
    FOnFullScreen: TWebBrowserExOnFullScreen;
    FOnTheaterMode: TWebBrowserExOnTheaterMode;
    FOnWindowSetResizable: TWebBrowserExWindowSetResizable;
    FOnWindowSetLeft: TWebBrowserExWindowSetLeft;
    FOnWindowSetTop: TWebBrowserExWindowSetTop;
    FOnWindowSetWidth: TWebBrowserExWindowSetWidth;
    FOnWindowSetHeight: TWebBrowserExWindowSetHeight;
    FOnWindowClosing: TWebBrowserExWindowClosing;
    FOnClientToHostWindow: TWebBrowserExClientToHostWindow;
    FOnSetSecureLockIcon: TWebBrowserExSetSecureLockIcon;
    FOnFileDownload: TWebBrowserExFileDownload;
    FOnNavigateError: TWebBrowserExNavigateError;
    FOnPrintTemplateInstantiation: TWebBrowserExPrintTemplateInstantiation;
    FOnPrintTemplateTeardown: TWebBrowserExPrintTemplateTeardown;
    FOnUpdatePageStatus: TWebBrowserExUpdatePageStatus;
    FOnPrivacyImpactedStateChange: TWebBrowserExPrivacyImpactedStateChange;
    FOnNewWindow3: TWebBrowserExNewWindow3;
    FOnSetPhishingFilterStatus: TWebBrowserExSetPhishingFilterStatus;
    FOnWindowStateChanged: TWebBrowserExWindowStateChanged;
    FIntf: IWebBrowser2;
    function  GetControlInterface: IWebBrowser2;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Application: IDispatch;
    function Get_Parent: IDispatch;
    function Get_Container: IDispatch;
    function Get_Document: IDispatch;
  public
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); overload;
    procedure Refresh;
    procedure Refresh2; overload;
    procedure Refresh2(var Level: OleVariant); overload;
    procedure Stop;
    procedure Quit;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant);
    function GetProperty(const Property_: WideString): OleVariant;
    procedure Navigate2(var URL: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant; 
                        var Headers: OleVariant); overload;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                     var pvaOut: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                             var pvarSize: OleVariant); overload;
    property  ControlInterface: IWebBrowser2 read GetControlInterface;
    property  DefaultInterface: IWebBrowser2 read GetControlInterface;
    property Application: IDispatch index 200 read GetIDispatchProp;
    property Parent: IDispatch index 201 read GetIDispatchProp;
    property Container: IDispatch index 202 read GetIDispatchProp;
    property Document: IDispatch index 203 read GetIDispatchProp;
    property TopLevelContainer: WordBool index 204 read GetWordBoolProp;
    property type_: WideString index 205 read GetWideStringProp;
    property LocationName: WideString index 210 read GetWideStringProp;
    property LocationURL: WideString index 211 read GetWideStringProp;
    property Busy: WordBool index 212 read GetWordBoolProp;
    property Name: WideString index 0 read GetWideStringProp;
    property HWND: Integer index -515 read GetIntegerProp;
    property FullName: WideString index 400 read GetWideStringProp;
    property Path: WideString index 401 read GetWideStringProp;
    property ReadyState: TOleEnum index -525 read GetTOleEnumProp;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property Visible: WordBool index 402 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusBar: WordBool index 403 read GetWordBoolProp write SetWordBoolProp stored False;
    property StatusText: WideString index 404 read GetWideStringProp write SetWideStringProp stored False;
    property ToolBar: Integer index 405 read GetIntegerProp write SetIntegerProp stored False;
    property MenuBar: WordBool index 406 read GetWordBoolProp write SetWordBoolProp stored False;
    property FullScreen: WordBool index 407 read GetWordBoolProp write SetWordBoolProp stored False;
    property Offline: WordBool index 550 read GetWordBoolProp write SetWordBoolProp stored False;
    property Silent: WordBool index 551 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsBrowser: WordBool index 552 read GetWordBoolProp write SetWordBoolProp stored False;
    property RegisterAsDropTarget: WordBool index 553 read GetWordBoolProp write SetWordBoolProp stored False;
    property TheaterMode: WordBool index 554 read GetWordBoolProp write SetWordBoolProp stored False;
    property AddressBar: WordBool index 555 read GetWordBoolProp write SetWordBoolProp stored False;
    property Resizable: WordBool index 556 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnStatusTextChange: TWebBrowserExStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TWebBrowserExProgressChange read FOnProgressChange write FOnProgressChange;
    property OnCommandStateChange: TWebBrowserExCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnTitleChange: TWebBrowserExTitleChange read FOnTitleChange write FOnTitleChange;
    property OnPropertyChange: TWebBrowserExPropertyChange read FOnPropertyChange write FOnPropertyChange;
    property OnBeforeNavigate2: TWebBrowserExBeforeNavigate2 read FOnBeforeNavigate2 write FOnBeforeNavigate2;
    property OnNewWindow2: TWebBrowserExNewWindow2 read FOnNewWindow2 write FOnNewWindow2;
    property OnNavigateComplete2: TWebBrowserExNavigateComplete2 read FOnNavigateComplete2 write FOnNavigateComplete2;
    property OnDocumentComplete: TWebBrowserExDocumentComplete read FOnDocumentComplete write FOnDocumentComplete;
    property OnQuit: TNotifyEvent read FOnQuit write FOnQuit;
    property OnVisible: TWebBrowserExOnVisible read FOnVisible write FOnVisible;
    property OnToolBar: TWebBrowserExOnToolBar read FOnToolBar write FOnToolBar;
    property OnMenuBar: TWebBrowserExOnMenuBar read FOnMenuBar write FOnMenuBar;
    property OnStatusBar: TWebBrowserExOnStatusBar read FOnStatusBar write FOnStatusBar;
    property OnFullScreen: TWebBrowserExOnFullScreen read FOnFullScreen write FOnFullScreen;
    property OnTheaterMode: TWebBrowserExOnTheaterMode read FOnTheaterMode write FOnTheaterMode;
    property OnWindowSetResizable: TWebBrowserExWindowSetResizable read FOnWindowSetResizable write FOnWindowSetResizable;
    property OnWindowSetLeft: TWebBrowserExWindowSetLeft read FOnWindowSetLeft write FOnWindowSetLeft;
    property OnWindowSetTop: TWebBrowserExWindowSetTop read FOnWindowSetTop write FOnWindowSetTop;
    property OnWindowSetWidth: TWebBrowserExWindowSetWidth read FOnWindowSetWidth write FOnWindowSetWidth;
    property OnWindowSetHeight: TWebBrowserExWindowSetHeight read FOnWindowSetHeight write FOnWindowSetHeight;
    property OnWindowClosing: TWebBrowserExWindowClosing read FOnWindowClosing write FOnWindowClosing;
    property OnClientToHostWindow: TWebBrowserExClientToHostWindow read FOnClientToHostWindow write FOnClientToHostWindow;
    property OnSetSecureLockIcon: TWebBrowserExSetSecureLockIcon read FOnSetSecureLockIcon write FOnSetSecureLockIcon;
    property OnFileDownload: TWebBrowserExFileDownload read FOnFileDownload write FOnFileDownload;
    property OnNavigateError: TWebBrowserExNavigateError read FOnNavigateError write FOnNavigateError;
    property OnPrintTemplateInstantiation: TWebBrowserExPrintTemplateInstantiation read FOnPrintTemplateInstantiation write FOnPrintTemplateInstantiation;
    property OnPrintTemplateTeardown: TWebBrowserExPrintTemplateTeardown read FOnPrintTemplateTeardown write FOnPrintTemplateTeardown;
    property OnUpdatePageStatus: TWebBrowserExUpdatePageStatus read FOnUpdatePageStatus write FOnUpdatePageStatus;
    property OnPrivacyImpactedStateChange: TWebBrowserExPrivacyImpactedStateChange read FOnPrivacyImpactedStateChange write FOnPrivacyImpactedStateChange;
    property OnNewWindow3: TWebBrowserExNewWindow3 read FOnNewWindow3 write FOnNewWindow3;
    property OnSetPhishingFilterStatus: TWebBrowserExSetPhishingFilterStatus read FOnSetPhishingFilterStatus write FOnSetPhishingFilterStatus;
    property OnWindowStateChanged: TWebBrowserExWindowStateChanged read FOnWindowStateChanged write FOnWindowStateChanged;
  end;

// *********************************************************************//
// The Class CoInternetExplorer provides a Create and CreateRemote method to          
// create instances of the default interface IWebBrowser2 exposed by              
// the CoClass InternetExplorer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoInternetExplorer = class
    class function Create: IWebBrowser2;
    class function CreateRemote(const MachineName: string): IWebBrowser2;
  end;

  TInternetExplorerStatusTextChange = procedure(ASender: TObject; const Text: WideString) of object;
  TInternetExplorerProgressChange = procedure(ASender: TObject; Progress: Integer; 
                                                                ProgressMax: Integer) of object;
  TInternetExplorerCommandStateChange = procedure(ASender: TObject; Command: Integer; 
                                                                    Enable: WordBool) of object;
  TInternetExplorerTitleChange = procedure(ASender: TObject; const Text: WideString) of object;
  TInternetExplorerPropertyChange = procedure(ASender: TObject; const szProperty: WideString) of object;
  TInternetExplorerBeforeNavigate2 = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                                 var URL: OleVariant; 
                                                                 var Flags: OleVariant; 
                                                                 var TargetFrameName: OleVariant; 
                                                                 var PostData: OleVariant; 
                                                                 var Headers: OleVariant; 
                                                                 var Cancel: WordBool) of object;
  TInternetExplorerNewWindow2 = procedure(ASender: TObject; var ppDisp: IDispatch; 
                                                            var Cancel: WordBool) of object;
  TInternetExplorerNavigateComplete2 = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                                   var URL: OleVariant) of object;
  TInternetExplorerDocumentComplete = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                                  var URL: OleVariant) of object;
  TInternetExplorerOnVisible = procedure(ASender: TObject; Visible: WordBool) of object;
  TInternetExplorerOnToolBar = procedure(ASender: TObject; ToolBar: WordBool) of object;
  TInternetExplorerOnMenuBar = procedure(ASender: TObject; MenuBar: WordBool) of object;
  TInternetExplorerOnStatusBar = procedure(ASender: TObject; StatusBar: WordBool) of object;
  TInternetExplorerOnFullScreen = procedure(ASender: TObject; FullScreen: WordBool) of object;
  TInternetExplorerOnTheaterMode = procedure(ASender: TObject; TheaterMode: WordBool) of object;
  TInternetExplorerWindowSetResizable = procedure(ASender: TObject; Resizable: WordBool) of object;
  TInternetExplorerWindowSetLeft = procedure(ASender: TObject; Left: Integer) of object;
  TInternetExplorerWindowSetTop = procedure(ASender: TObject; Top: Integer) of object;
  TInternetExplorerWindowSetWidth = procedure(ASender: TObject; Width: Integer) of object;
  TInternetExplorerWindowSetHeight = procedure(ASender: TObject; Height: Integer) of object;
  TInternetExplorerWindowClosing = procedure(ASender: TObject; IsChildWindow: WordBool; 
                                                               var Cancel: WordBool) of object;
  TInternetExplorerClientToHostWindow = procedure(ASender: TObject; var CX: Integer; var CY: Integer) of object;
  TInternetExplorerSetSecureLockIcon = procedure(ASender: TObject; SecureLockIcon: Integer) of object;
  TInternetExplorerFileDownload = procedure(ASender: TObject; ActiveDocument: WordBool; 
                                                              var Cancel: WordBool) of object;
  TInternetExplorerNavigateError = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                               var URL: OleVariant; 
                                                               var Frame: OleVariant; 
                                                               var StatusCode: OleVariant; 
                                                               var Cancel: WordBool) of object;
  TInternetExplorerPrintTemplateInstantiation = procedure(ASender: TObject; const pDisp: IDispatch) of object;
  TInternetExplorerPrintTemplateTeardown = procedure(ASender: TObject; const pDisp: IDispatch) of object;
  TInternetExplorerUpdatePageStatus = procedure(ASender: TObject; const pDisp: IDispatch; 
                                                                  var nPage: OleVariant; 
                                                                  var fDone: OleVariant) of object;
  TInternetExplorerPrivacyImpactedStateChange = procedure(ASender: TObject; bImpacted: WordBool) of object;
  TInternetExplorerNewWindow3 = procedure(ASender: TObject; var ppDisp: IDispatch; 
                                                            var Cancel: WordBool; 
                                                            dwFlags: LongWord; 
                                                            const bstrUrlContext: WideString; 
                                                            const bstrUrl: WideString) of object;
  TInternetExplorerSetPhishingFilterStatus = procedure(ASender: TObject; PhishingFilterStatus: Integer) of object;
  TInternetExplorerWindowStateChanged = procedure(ASender: TObject; dwWindowStateFlags: LongWord; 
                                                                    dwValidFlagsMask: LongWord) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TInternetExplorer
// Help String      : Internet Explorer Application.
// Default Interface: IWebBrowser2
// Def. Intf. DISP? : No
// Event   Interface: DWebBrowserEvents2
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TInternetExplorerProperties= class;
{$ENDIF}
  TInternetExplorer = class(TOleServer)
  private
    FOnStatusTextChange: TInternetExplorerStatusTextChange;
    FOnProgressChange: TInternetExplorerProgressChange;
    FOnCommandStateChange: TInternetExplorerCommandStateChange;
    FOnDownloadBegin: TNotifyEvent;
    FOnDownloadComplete: TNotifyEvent;
    FOnTitleChange: TInternetExplorerTitleChange;
    FOnPropertyChange: TInternetExplorerPropertyChange;
    FOnBeforeNavigate2: TInternetExplorerBeforeNavigate2;
    FOnNewWindow2: TInternetExplorerNewWindow2;
    FOnNavigateComplete2: TInternetExplorerNavigateComplete2;
    FOnDocumentComplete: TInternetExplorerDocumentComplete;
    FOnQuit: TNotifyEvent;
    FOnVisible: TInternetExplorerOnVisible;
    FOnToolBar: TInternetExplorerOnToolBar;
    FOnMenuBar: TInternetExplorerOnMenuBar;
    FOnStatusBar: TInternetExplorerOnStatusBar;
    FOnFullScreen: TInternetExplorerOnFullScreen;
    FOnTheaterMode: TInternetExplorerOnTheaterMode;
    FOnWindowSetResizable: TInternetExplorerWindowSetResizable;
    FOnWindowSetLeft: TInternetExplorerWindowSetLeft;
    FOnWindowSetTop: TInternetExplorerWindowSetTop;
    FOnWindowSetWidth: TInternetExplorerWindowSetWidth;
    FOnWindowSetHeight: TInternetExplorerWindowSetHeight;
    FOnWindowClosing: TInternetExplorerWindowClosing;
    FOnClientToHostWindow: TInternetExplorerClientToHostWindow;
    FOnSetSecureLockIcon: TInternetExplorerSetSecureLockIcon;
    FOnFileDownload: TInternetExplorerFileDownload;
    FOnNavigateError: TInternetExplorerNavigateError;
    FOnPrintTemplateInstantiation: TInternetExplorerPrintTemplateInstantiation;
    FOnPrintTemplateTeardown: TInternetExplorerPrintTemplateTeardown;
    FOnUpdatePageStatus: TInternetExplorerUpdatePageStatus;
    FOnPrivacyImpactedStateChange: TInternetExplorerPrivacyImpactedStateChange;
    FOnNewWindow3: TInternetExplorerNewWindow3;
    FOnSetPhishingFilterStatus: TInternetExplorerSetPhishingFilterStatus;
    FOnWindowStateChanged: TInternetExplorerWindowStateChanged;
    FIntf:        IWebBrowser2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TInternetExplorerProperties;
    function      GetServerProperties: TInternetExplorerProperties;
{$ENDIF}
    function      GetDefaultInterface: IWebBrowser2;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Application: IDispatch;
    function Get_Parent: IDispatch;
    function Get_Container: IDispatch;
    function Get_Document: IDispatch;
    function Get_TopLevelContainer: WordBool;
    function Get_type_: WideString;
    function Get_Left: Integer;
    procedure Set_Left(pl: Integer);
    function Get_Top: Integer;
    procedure Set_Top(pl: Integer);
    function Get_Width: Integer;
    procedure Set_Width(pl: Integer);
    function Get_Height: Integer;
    procedure Set_Height(pl: Integer);
    function Get_LocationName: WideString;
    function Get_LocationURL: WideString;
    function Get_Busy: WordBool;
    function Get_Name: WideString;
    function Get_HWND: Integer;
    function Get_FullName: WideString;
    function Get_Path: WideString;
    function Get_Visible: WordBool;
    procedure Set_Visible(pBool: WordBool);
    function Get_StatusBar: WordBool;
    procedure Set_StatusBar(pBool: WordBool);
    function Get_StatusText: WideString;
    procedure Set_StatusText(const StatusText: WideString);
    function Get_ToolBar: SYSINT;
    procedure Set_ToolBar(Value: SYSINT);
    function Get_MenuBar: WordBool;
    procedure Set_MenuBar(Value: WordBool);
    function Get_FullScreen: WordBool;
    procedure Set_FullScreen(pbFullScreen: WordBool);
    function Get_ReadyState: tagREADYSTATE;
    function Get_Offline: WordBool;
    procedure Set_Offline(pbOffline: WordBool);
    function Get_Silent: WordBool;
    procedure Set_Silent(pbSilent: WordBool);
    function Get_RegisterAsBrowser: WordBool;
    procedure Set_RegisterAsBrowser(pbRegister: WordBool);
    function Get_RegisterAsDropTarget: WordBool;
    procedure Set_RegisterAsDropTarget(pbRegister: WordBool);
    function Get_TheaterMode: WordBool;
    procedure Set_TheaterMode(pbRegister: WordBool);
    function Get_AddressBar: WordBool;
    procedure Set_AddressBar(Value: WordBool);
    function Get_Resizable: WordBool;
    procedure Set_Resizable(Value: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IWebBrowser2);
    procedure Disconnect; override;
    procedure GoBack;
    procedure GoForward;
    procedure GoHome;
    procedure GoSearch;
    procedure Navigate(const URL: WideString); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate(const URL: WideString; var Flags: OleVariant; 
                       var TargetFrameName: OleVariant; var PostData: OleVariant; 
                       var Headers: OleVariant); overload;
    procedure Refresh;
    procedure Refresh2; overload;
    procedure Refresh2(var Level: OleVariant); overload;
    procedure Stop;
    procedure Quit;
    procedure ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
    procedure PutProperty(const Property_: WideString; vtValue: OleVariant);
    function GetProperty(const Property_: WideString): OleVariant;
    procedure Navigate2(var URL: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; var TargetFrameName: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant); overload;
    procedure Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                        var TargetFrameName: OleVariant; var PostData: OleVariant; 
                        var Headers: OleVariant); overload;
    function QueryStatusWB(cmdID: OLECMDID): OLECMDF;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant); overload;
    procedure ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                     var pvaOut: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant); overload;
    procedure ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                             var pvarSize: OleVariant); overload;
    property DefaultInterface: IWebBrowser2 read GetDefaultInterface;
    property Application: IDispatch read Get_Application;
    property Parent: IDispatch read Get_Parent;
    property Container: IDispatch read Get_Container;
    property Document: IDispatch read Get_Document;
    property TopLevelContainer: WordBool read Get_TopLevelContainer;
    property type_: WideString read Get_type_;
    property LocationName: WideString read Get_LocationName;
    property LocationURL: WideString read Get_LocationURL;
    property Busy: WordBool read Get_Busy;
    property Name: WideString read Get_Name;
    property HWND: Integer read Get_HWND;
    property FullName: WideString read Get_FullName;
    property Path: WideString read Get_Path;
    property ReadyState: tagREADYSTATE read Get_ReadyState;
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property StatusBar: WordBool read Get_StatusBar write Set_StatusBar;
    property StatusText: WideString read Get_StatusText write Set_StatusText;
    property ToolBar: SYSINT read Get_ToolBar write Set_ToolBar;
    property MenuBar: WordBool read Get_MenuBar write Set_MenuBar;
    property FullScreen: WordBool read Get_FullScreen write Set_FullScreen;
    property Offline: WordBool read Get_Offline write Set_Offline;
    property Silent: WordBool read Get_Silent write Set_Silent;
    property RegisterAsBrowser: WordBool read Get_RegisterAsBrowser write Set_RegisterAsBrowser;
    property RegisterAsDropTarget: WordBool read Get_RegisterAsDropTarget write Set_RegisterAsDropTarget;
    property TheaterMode: WordBool read Get_TheaterMode write Set_TheaterMode;
    property AddressBar: WordBool read Get_AddressBar write Set_AddressBar;
    property Resizable: WordBool read Get_Resizable write Set_Resizable;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TInternetExplorerProperties read GetServerProperties;
{$ENDIF}
    property OnStatusTextChange: TInternetExplorerStatusTextChange read FOnStatusTextChange write FOnStatusTextChange;
    property OnProgressChange: TInternetExplorerProgressChange read FOnProgressChange write FOnProgressChange;
    property OnCommandStateChange: TInternetExplorerCommandStateChange read FOnCommandStateChange write FOnCommandStateChange;
    property OnDownloadBegin: TNotifyEvent read FOnDownloadBegin write FOnDownloadBegin;
    property OnDownloadComplete: TNotifyEvent read FOnDownloadComplete write FOnDownloadComplete;
    property OnTitleChange: TInternetExplorerTitleChange read FOnTitleChange write FOnTitleChange;
    property OnPropertyChange: TInternetExplorerPropertyChange read FOnPropertyChange write FOnPropertyChange;
    property OnBeforeNavigate2: TInternetExplorerBeforeNavigate2 read FOnBeforeNavigate2 write FOnBeforeNavigate2;
    property OnNewWindow2: TInternetExplorerNewWindow2 read FOnNewWindow2 write FOnNewWindow2;
    property OnNavigateComplete2: TInternetExplorerNavigateComplete2 read FOnNavigateComplete2 write FOnNavigateComplete2;
    property OnDocumentComplete: TInternetExplorerDocumentComplete read FOnDocumentComplete write FOnDocumentComplete;
    property OnQuit: TNotifyEvent read FOnQuit write FOnQuit;
    property OnVisible: TInternetExplorerOnVisible read FOnVisible write FOnVisible;
    property OnToolBar: TInternetExplorerOnToolBar read FOnToolBar write FOnToolBar;
    property OnMenuBar: TInternetExplorerOnMenuBar read FOnMenuBar write FOnMenuBar;
    property OnStatusBar: TInternetExplorerOnStatusBar read FOnStatusBar write FOnStatusBar;
    property OnFullScreen: TInternetExplorerOnFullScreen read FOnFullScreen write FOnFullScreen;
    property OnTheaterMode: TInternetExplorerOnTheaterMode read FOnTheaterMode write FOnTheaterMode;
    property OnWindowSetResizable: TInternetExplorerWindowSetResizable read FOnWindowSetResizable write FOnWindowSetResizable;
    property OnWindowSetLeft: TInternetExplorerWindowSetLeft read FOnWindowSetLeft write FOnWindowSetLeft;
    property OnWindowSetTop: TInternetExplorerWindowSetTop read FOnWindowSetTop write FOnWindowSetTop;
    property OnWindowSetWidth: TInternetExplorerWindowSetWidth read FOnWindowSetWidth write FOnWindowSetWidth;
    property OnWindowSetHeight: TInternetExplorerWindowSetHeight read FOnWindowSetHeight write FOnWindowSetHeight;
    property OnWindowClosing: TInternetExplorerWindowClosing read FOnWindowClosing write FOnWindowClosing;
    property OnClientToHostWindow: TInternetExplorerClientToHostWindow read FOnClientToHostWindow write FOnClientToHostWindow;
    property OnSetSecureLockIcon: TInternetExplorerSetSecureLockIcon read FOnSetSecureLockIcon write FOnSetSecureLockIcon;
    property OnFileDownload: TInternetExplorerFileDownload read FOnFileDownload write FOnFileDownload;
    property OnNavigateError: TInternetExplorerNavigateError read FOnNavigateError write FOnNavigateError;
    property OnPrintTemplateInstantiation: TInternetExplorerPrintTemplateInstantiation read FOnPrintTemplateInstantiation write FOnPrintTemplateInstantiation;
    property OnPrintTemplateTeardown: TInternetExplorerPrintTemplateTeardown read FOnPrintTemplateTeardown write FOnPrintTemplateTeardown;
    property OnUpdatePageStatus: TInternetExplorerUpdatePageStatus read FOnUpdatePageStatus write FOnUpdatePageStatus;
    property OnPrivacyImpactedStateChange: TInternetExplorerPrivacyImpactedStateChange read FOnPrivacyImpactedStateChange write FOnPrivacyImpactedStateChange;
    property OnNewWindow3: TInternetExplorerNewWindow3 read FOnNewWindow3 write FOnNewWindow3;
    property OnSetPhishingFilterStatus: TInternetExplorerSetPhishingFilterStatus read FOnSetPhishingFilterStatus write FOnSetPhishingFilterStatus;
    property OnWindowStateChanged: TInternetExplorerWindowStateChanged read FOnWindowStateChanged write FOnWindowStateChanged;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TInternetExplorer
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TInternetExplorerProperties = class(TPersistent)
  private
    FServer:    TInternetExplorer;
    function    GetDefaultInterface: IWebBrowser2;
    constructor Create(AServer: TInternetExplorer);
  protected
    function Get_Application: IDispatch;
    function Get_Parent: IDispatch;
    function Get_Container: IDispatch;
    function Get_Document: IDispatch;
    function Get_TopLevelContainer: WordBool;
    function Get_type_: WideString;
    function Get_Left: Integer;
    procedure Set_Left(pl: Integer);
    function Get_Top: Integer;
    procedure Set_Top(pl: Integer);
    function Get_Width: Integer;
    procedure Set_Width(pl: Integer);
    function Get_Height: Integer;
    procedure Set_Height(pl: Integer);
    function Get_LocationName: WideString;
    function Get_LocationURL: WideString;
    function Get_Busy: WordBool;
    function Get_Name: WideString;
    function Get_HWND: Integer;
    function Get_FullName: WideString;
    function Get_Path: WideString;
    function Get_Visible: WordBool;
    procedure Set_Visible(pBool: WordBool);
    function Get_StatusBar: WordBool;
    procedure Set_StatusBar(pBool: WordBool);
    function Get_StatusText: WideString;
    procedure Set_StatusText(const StatusText: WideString);
    function Get_ToolBar: SYSINT;
    procedure Set_ToolBar(Value: SYSINT);
    function Get_MenuBar: WordBool;
    procedure Set_MenuBar(Value: WordBool);
    function Get_FullScreen: WordBool;
    procedure Set_FullScreen(pbFullScreen: WordBool);
    function Get_ReadyState: tagREADYSTATE;
    function Get_Offline: WordBool;
    procedure Set_Offline(pbOffline: WordBool);
    function Get_Silent: WordBool;
    procedure Set_Silent(pbSilent: WordBool);
    function Get_RegisterAsBrowser: WordBool;
    procedure Set_RegisterAsBrowser(pbRegister: WordBool);
    function Get_RegisterAsDropTarget: WordBool;
    procedure Set_RegisterAsDropTarget(pbRegister: WordBool);
    function Get_TheaterMode: WordBool;
    procedure Set_TheaterMode(pbRegister: WordBool);
    function Get_AddressBar: WordBool;
    procedure Set_AddressBar(Value: WordBool);
    function Get_Resizable: WordBool;
    procedure Set_Resizable(Value: WordBool);
  public
    property DefaultInterface: IWebBrowser2 read GetDefaultInterface;
  published
    property Left: Integer read Get_Left write Set_Left;
    property Top: Integer read Get_Top write Set_Top;
    property Width: Integer read Get_Width write Set_Width;
    property Height: Integer read Get_Height write Set_Height;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property StatusBar: WordBool read Get_StatusBar write Set_StatusBar;
    property StatusText: WideString read Get_StatusText write Set_StatusText;
    property ToolBar: SYSINT read Get_ToolBar write Set_ToolBar;
    property MenuBar: WordBool read Get_MenuBar write Set_MenuBar;
    property FullScreen: WordBool read Get_FullScreen write Set_FullScreen;
    property Offline: WordBool read Get_Offline write Set_Offline;
    property Silent: WordBool read Get_Silent write Set_Silent;
    property RegisterAsBrowser: WordBool read Get_RegisterAsBrowser write Set_RegisterAsBrowser;
    property RegisterAsDropTarget: WordBool read Get_RegisterAsDropTarget write Set_RegisterAsDropTarget;
    property TheaterMode: WordBool read Get_TheaterMode write Set_TheaterMode;
    property AddressBar: WordBool read Get_AddressBar write Set_AddressBar;
    property Resizable: WordBool read Get_Resizable write Set_Resizable;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoShellBrowserWindow provides a Create and CreateRemote method to          
// create instances of the default interface IWebBrowser2 exposed by              
// the CoClass ShellBrowserWindow. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellBrowserWindow = class
    class function Create: IWebBrowser2;
    class function CreateRemote(const MachineName: string): IWebBrowser2;
  end;

// *********************************************************************//
// The Class CoShellWindows provides a Create and CreateRemote method to          
// create instances of the default interface IShellWindows exposed by              
// the CoClass ShellWindows. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellWindows = class
    class function Create: IShellWindows;
    class function CreateRemote(const MachineName: string): IShellWindows;
  end;

  TShellWindowsWindowRegistered = procedure(ASender: TObject; lCookie: Integer) of object;
  TShellWindowsWindowRevoked = procedure(ASender: TObject; lCookie: Integer) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TShellWindows
// Help String      : ShellDispatch Load in Shell Context
// Default Interface: IShellWindows
// Def. Intf. DISP? : No
// Event   Interface: DShellWindowsEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TShellWindowsProperties= class;
{$ENDIF}
  TShellWindows = class(TOleServer)
  private
    FOnWindowRegistered: TShellWindowsWindowRegistered;
    FOnWindowRevoked: TShellWindowsWindowRevoked;
    FIntf:        IShellWindows;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TShellWindowsProperties;
    function      GetServerProperties: TShellWindowsProperties;
{$ENDIF}
    function      GetDefaultInterface: IShellWindows;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Count: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IShellWindows);
    procedure Disconnect; override;
    function Item: IDispatch; overload;
    function Item(index: OleVariant): IDispatch; overload;
    function _NewEnum: IUnknown;
    procedure Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; out plCookie: Integer);
    procedure RegisterPending(lThreadId: Integer; var pvarloc: OleVariant; 
                              var pvarlocRoot: OleVariant; swClass: SYSINT; out plCookie: Integer);
    procedure Revoke(lCookie: Integer);
    procedure OnNavigate(lCookie: Integer; var pvarloc: OleVariant);
    procedure OnActivated(lCookie: Integer; fActive: WordBool);
    function FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; swClass: SYSINT; 
                          out pHWND: Integer; swfwOptions: SYSINT): IDispatch;
    procedure OnCreated(lCookie: Integer; const punk: IUnknown);
    procedure ProcessAttachDetach(fAttach: WordBool);
    property DefaultInterface: IShellWindows read GetDefaultInterface;
    property Count: Integer read Get_Count;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TShellWindowsProperties read GetServerProperties;
{$ENDIF}
    property OnWindowRegistered: TShellWindowsWindowRegistered read FOnWindowRegistered write FOnWindowRegistered;
    property OnWindowRevoked: TShellWindowsWindowRevoked read FOnWindowRevoked write FOnWindowRevoked;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TShellWindows
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TShellWindowsProperties = class(TPersistent)
  private
    FServer:    TShellWindows;
    function    GetDefaultInterface: IShellWindows;
    constructor Create(AServer: TShellWindows);
  protected
    function Get_Count: Integer;
  public
    property DefaultInterface: IShellWindows read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoShellUIHelper provides a Create and CreateRemote method to          
// create instances of the default interface IShellUIHelper2 exposed by              
// the CoClass ShellUIHelper. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellUIHelper = class
    class function Create: IShellUIHelper2;
    class function CreateRemote(const MachineName: string): IShellUIHelper2;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TShellUIHelper
// Help String      : 
// Default Interface: IShellUIHelper2
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TShellUIHelperProperties= class;
{$ENDIF}
  TShellUIHelper = class(TOleServer)
  private
    FIntf:        IShellUIHelper2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TShellUIHelperProperties;
    function      GetServerProperties: TShellUIHelperProperties;
{$ENDIF}
    function      GetDefaultInterface: IShellUIHelper2;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IShellUIHelper2);
    procedure Disconnect; override;
    procedure ResetFirstBootMode;
    procedure ResetSafeMode;
    procedure RefreshOfflineDesktop;
    procedure AddFavorite(const URL: WideString); overload;
    procedure AddFavorite(const URL: WideString; var Title: OleVariant); overload;
    procedure AddChannel(const URL: WideString);
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString); overload;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant); overload;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant); overload;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant); overload;
    procedure AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                  var Left: OleVariant; var Top: OleVariant; var Width: OleVariant; 
                                  var Height: OleVariant); overload;
    function IsSubscribed(const URL: WideString): WordBool;
    procedure NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                              var varTargetFrame: OleVariant);
    procedure ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString);
    procedure AutoCompleteSaveForm; overload;
    procedure AutoCompleteSaveForm(var Form: OleVariant); overload;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString); overload;
    procedure AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                       var pvarTargetFrame: OleVariant); overload;
    procedure AutoCompleteAttach; overload;
    procedure AutoCompleteAttach(var Reserved: OleVariant); overload;
    function ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant;
    procedure AddSearchProvider(const URL: WideString);
    procedure RunOnceShown;
    procedure SkipRunOnce;
    procedure CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; const bstrLocale: WideString);
    function SqmEnabled: WordBool;
    function PhishingEnabled: WordBool;
    function BrandImageUri: WideString;
    procedure SkipTabsWelcome;
    procedure DiagnoseConnection;
    procedure CustomizeClearType(fSet: WordBool);
    function IsSearchProviderInstalled(const URL: WideString): LongWord;
    function IsSearchMigrated: WordBool;
    function DefaultSearchProvider: WideString;
    procedure RunOnceRequiredSettingsComplete(fComplete: WordBool);
    function RunOnceHasShown: WordBool;
    function SearchGuideUrl: WideString;
    property DefaultInterface: IShellUIHelper2 read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TShellUIHelperProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TShellUIHelper
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TShellUIHelperProperties = class(TPersistent)
  private
    FServer:    TShellUIHelper;
    function    GetDefaultInterface: IShellUIHelper2;
    constructor Create(AServer: TShellUIHelper);
  protected
  public
    property DefaultInterface: IShellUIHelper2 read GetDefaultInterface;
  published
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoShellNameSpace provides a Create and CreateRemote method to          
// create instances of the default interface IShellNameSpace exposed by              
// the CoClass ShellNameSpace. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellNameSpace = class
    class function Create: IShellNameSpace;
    class function CreateRemote(const MachineName: string): IShellNameSpace;
  end;

  TShellNameSpaceFavoritesSelectionChange = procedure(ASender: TObject; cItems: Integer; 
                                                                        hItem: Integer; 
                                                                        const strName: WideString; 
                                                                        const strUrl: WideString; 
                                                                        cVisits: Integer; 
                                                                        const strDate: WideString; 
                                                                        fAvailableOffline: Integer) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TShellNameSpace
// Help String      : 
// Default Interface: IShellNameSpace
// Def. Intf. DISP? : No
// Event   Interface: DShellNameSpaceEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TShellNameSpaceProperties= class;
{$ENDIF}
  TShellNameSpace = class(TOleServer)
  private
    FOnFavoritesSelectionChange: TShellNameSpaceFavoritesSelectionChange;
    FOnSelectionChange: TNotifyEvent;
    FOnDoubleClick: TNotifyEvent;
    FOnInitialized: TNotifyEvent;
    FIntf:        IShellNameSpace;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TShellNameSpaceProperties;
    function      GetServerProperties: TShellNameSpaceProperties;
{$ENDIF}
    function      GetDefaultInterface: IShellNameSpace;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_SubscriptionsEnabled: WordBool;
    function Get_EnumOptions: Integer;
    procedure Set_EnumOptions(pgrfEnumFlags: Integer);
    function Get_SelectedItem: IDispatch;
    procedure Set_SelectedItem(const pItem: IDispatch);
    function Get_Root: OleVariant;
    procedure Set_Root(pvar: OleVariant);
    function Get_Depth: SYSINT;
    procedure Set_Depth(piDepth: SYSINT);
    function Get_Mode: SYSUINT;
    procedure Set_Mode(puMode: SYSUINT);
    function Get_Flags: LongWord;
    procedure Set_Flags(pdwFlags: LongWord);
    procedure Set_TVFlags(dwFlags: LongWord);
    function Get_TVFlags: LongWord;
    function Get_Columns: WideString;
    procedure Set_Columns(const bstrColumns: WideString);
    function Get_CountViewTypes: SYSINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IShellNameSpace);
    procedure Disconnect; override;
    procedure MoveSelectionUp;
    procedure MoveSelectionDown;
    procedure ResetSort;
    procedure NewFolder;
    procedure Synchronize;
    procedure Import;
    procedure Export;
    procedure InvokeContextMenuCommand(const strCommand: WideString);
    procedure MoveSelectionTo;
    function CreateSubscriptionForSelection: WordBool;
    function DeleteSubscriptionForSelection: WordBool;
    procedure SetRoot(const bstrFullPath: WideString);
    procedure SetViewType(iType: SYSINT);
    function SelectedItems: IDispatch;
    procedure Expand(var_: OleVariant; iDepth: SYSINT);
    procedure UnselectAll;
    property DefaultInterface: IShellNameSpace read GetDefaultInterface;
    property SubscriptionsEnabled: WordBool read Get_SubscriptionsEnabled;
    property SelectedItem: IDispatch read Get_SelectedItem write Set_SelectedItem;
    property Root: OleVariant read Get_Root write Set_Root;
    property CountViewTypes: SYSINT read Get_CountViewTypes;
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property TVFlags: LongWord read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TShellNameSpaceProperties read GetServerProperties;
{$ENDIF}
    property OnFavoritesSelectionChange: TShellNameSpaceFavoritesSelectionChange read FOnFavoritesSelectionChange write FOnFavoritesSelectionChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TShellNameSpace
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TShellNameSpaceProperties = class(TPersistent)
  private
    FServer:    TShellNameSpace;
    function    GetDefaultInterface: IShellNameSpace;
    constructor Create(AServer: TShellNameSpace);
  protected
    function Get_SubscriptionsEnabled: WordBool;
    function Get_EnumOptions: Integer;
    procedure Set_EnumOptions(pgrfEnumFlags: Integer);
    function Get_SelectedItem: IDispatch;
    procedure Set_SelectedItem(const pItem: IDispatch);
    function Get_Root: OleVariant;
    procedure Set_Root(pvar: OleVariant);
    function Get_Depth: SYSINT;
    procedure Set_Depth(piDepth: SYSINT);
    function Get_Mode: SYSUINT;
    procedure Set_Mode(puMode: SYSUINT);
    function Get_Flags: LongWord;
    procedure Set_Flags(pdwFlags: LongWord);
    procedure Set_TVFlags(dwFlags: LongWord);
    function Get_TVFlags: LongWord;
    function Get_Columns: WideString;
    procedure Set_Columns(const bstrColumns: WideString);
    function Get_CountViewTypes: SYSINT;
  public
    property DefaultInterface: IShellNameSpace read GetDefaultInterface;
  published
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property TVFlags: LongWord read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoShellShellNameSpace provides a Create and CreateRemote method to          
// create instances of the default interface IShellNameSpace exposed by              
// the CoClass ShellShellNameSpace. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoShellShellNameSpace = class
    class function Create: IShellNameSpace;
    class function CreateRemote(const MachineName: string): IShellNameSpace;
  end;

  TShellShellNameSpaceFavoritesSelectionChange = procedure(ASender: TObject; cItems: Integer; 
                                                                             hItem: Integer; 
                                                                             const strName: WideString; 
                                                                             const strUrl: WideString; 
                                                                             cVisits: Integer; 
                                                                             const strDate: WideString; 
                                                                             fAvailableOffline: Integer) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TShellShellNameSpace
// Help String      : Shell ShellNameSpace Class
// Default Interface: IShellNameSpace
// Def. Intf. DISP? : No
// Event   Interface: DShellNameSpaceEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TShellShellNameSpaceProperties= class;
{$ENDIF}
  TShellShellNameSpace = class(TOleServer)
  private
    FOnFavoritesSelectionChange: TShellShellNameSpaceFavoritesSelectionChange;
    FOnSelectionChange: TNotifyEvent;
    FOnDoubleClick: TNotifyEvent;
    FOnInitialized: TNotifyEvent;
    FIntf:        IShellNameSpace;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TShellShellNameSpaceProperties;
    function      GetServerProperties: TShellShellNameSpaceProperties;
{$ENDIF}
    function      GetDefaultInterface: IShellNameSpace;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_SubscriptionsEnabled: WordBool;
    function Get_EnumOptions: Integer;
    procedure Set_EnumOptions(pgrfEnumFlags: Integer);
    function Get_SelectedItem: IDispatch;
    procedure Set_SelectedItem(const pItem: IDispatch);
    function Get_Root: OleVariant;
    procedure Set_Root(pvar: OleVariant);
    function Get_Depth: SYSINT;
    procedure Set_Depth(piDepth: SYSINT);
    function Get_Mode: SYSUINT;
    procedure Set_Mode(puMode: SYSUINT);
    function Get_Flags: LongWord;
    procedure Set_Flags(pdwFlags: LongWord);
    procedure Set_TVFlags(dwFlags: LongWord);
    function Get_TVFlags: LongWord;
    function Get_Columns: WideString;
    procedure Set_Columns(const bstrColumns: WideString);
    function Get_CountViewTypes: SYSINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IShellNameSpace);
    procedure Disconnect; override;
    procedure MoveSelectionUp;
    procedure MoveSelectionDown;
    procedure ResetSort;
    procedure NewFolder;
    procedure Synchronize;
    procedure Import;
    procedure Export;
    procedure InvokeContextMenuCommand(const strCommand: WideString);
    procedure MoveSelectionTo;
    function CreateSubscriptionForSelection: WordBool;
    function DeleteSubscriptionForSelection: WordBool;
    procedure SetRoot(const bstrFullPath: WideString);
    procedure SetViewType(iType: SYSINT);
    function SelectedItems: IDispatch;
    procedure Expand(var_: OleVariant; iDepth: SYSINT);
    procedure UnselectAll;
    property DefaultInterface: IShellNameSpace read GetDefaultInterface;
    property SubscriptionsEnabled: WordBool read Get_SubscriptionsEnabled;
    property SelectedItem: IDispatch read Get_SelectedItem write Set_SelectedItem;
    property Root: OleVariant read Get_Root write Set_Root;
    property CountViewTypes: SYSINT read Get_CountViewTypes;
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property TVFlags: LongWord read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TShellShellNameSpaceProperties read GetServerProperties;
{$ENDIF}
    property OnFavoritesSelectionChange: TShellShellNameSpaceFavoritesSelectionChange read FOnFavoritesSelectionChange write FOnFavoritesSelectionChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
    property OnInitialized: TNotifyEvent read FOnInitialized write FOnInitialized;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TShellShellNameSpace
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TShellShellNameSpaceProperties = class(TPersistent)
  private
    FServer:    TShellShellNameSpace;
    function    GetDefaultInterface: IShellNameSpace;
    constructor Create(AServer: TShellShellNameSpace);
  protected
    function Get_SubscriptionsEnabled: WordBool;
    function Get_EnumOptions: Integer;
    procedure Set_EnumOptions(pgrfEnumFlags: Integer);
    function Get_SelectedItem: IDispatch;
    procedure Set_SelectedItem(const pItem: IDispatch);
    function Get_Root: OleVariant;
    procedure Set_Root(pvar: OleVariant);
    function Get_Depth: SYSINT;
    procedure Set_Depth(piDepth: SYSINT);
    function Get_Mode: SYSUINT;
    procedure Set_Mode(puMode: SYSUINT);
    function Get_Flags: LongWord;
    procedure Set_Flags(pdwFlags: LongWord);
    procedure Set_TVFlags(dwFlags: LongWord);
    function Get_TVFlags: LongWord;
    function Get_Columns: WideString;
    procedure Set_Columns(const bstrColumns: WideString);
    function Get_CountViewTypes: SYSINT;
  public
    property DefaultInterface: IShellNameSpace read GetDefaultInterface;
  published
    property EnumOptions: Integer read Get_EnumOptions write Set_EnumOptions;
    property Depth: SYSINT read Get_Depth write Set_Depth;
    property Mode: SYSUINT read Get_Mode write Set_Mode;
    property Flags: LongWord read Get_Flags write Set_Flags;
    property TVFlags: LongWord read Get_TVFlags write Set_TVFlags;
    property Columns: WideString read Get_Columns write Set_Columns;
  end;
{$ENDIF}


// *********************************************************************//
// The Class CoCScriptErrorList provides a Create and CreateRemote method to          
// create instances of the default interface IScriptErrorList exposed by              
// the CoClass CScriptErrorList. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCScriptErrorList = class
    class function Create: IScriptErrorList;
    class function CreateRemote(const MachineName: string): IScriptErrorList;
  end;

implementation

uses ComObj;

procedure TWebBrowserEx_V1.InitControlData;
const
  CEventDispIDs: array [0..16] of DWORD = (
    $00000064, $00000065, $00000066, $0000006C, $00000068, $00000069,
    $0000006A, $0000006B, $00000071, $000000C8, $000000C9, $000000CC,
    $00000067, $0000006D, $0000006E, $0000006F, $00000070);
  CControlData: TControlData2 = (
    ClassID: '{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}';
    EventIID: '{EAB22AC2-30C1-11CF-A7EB-0000C05BAE0B}';
    EventCount: 17;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040111*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnBeforeNavigate) - Cardinal(Self);
end;

procedure TWebBrowserEx_V1.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TWebBrowserEx_V1.GetControlInterface: IWebBrowser;
begin
  CreateControl;
  Result := FIntf;
end;

function TWebBrowserEx_V1.Get_Application: IDispatch;
begin
    Result := DefaultInterface.Application;
end;

function TWebBrowserEx_V1.Get_Parent: IDispatch;
begin
    Result := DefaultInterface.Parent;
end;

function TWebBrowserEx_V1.Get_Container: IDispatch;
begin
    Result := DefaultInterface.Container;
end;

function TWebBrowserEx_V1.Get_Document: IDispatch;
begin
    Result := DefaultInterface.Document;
end;

procedure TWebBrowserEx_V1.GoBack;
begin
  DefaultInterface.GoBack;
end;

procedure TWebBrowserEx_V1.GoForward;
begin
  DefaultInterface.GoForward;
end;

procedure TWebBrowserEx_V1.GoHome;
begin
  DefaultInterface.GoHome;
end;

procedure TWebBrowserEx_V1.GoSearch;
begin
  DefaultInterface.GoSearch;
end;

procedure TWebBrowserEx_V1.Navigate(const URL: WideString);
begin
  DefaultInterface.Navigate(URL, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx_V1.Navigate(const URL: WideString; var Flags: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx_V1.Navigate(const URL: WideString; var Flags: OleVariant; 
                                  var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx_V1.Navigate(const URL: WideString; var Flags: OleVariant; 
                                  var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, EmptyParam);
end;

procedure TWebBrowserEx_V1.Navigate(const URL: WideString; var Flags: OleVariant; 
                                  var TargetFrameName: OleVariant; var PostData: OleVariant; 
                                  var Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowserEx_V1.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TWebBrowserEx_V1.Refresh2;
begin
  DefaultInterface.Refresh2(EmptyParam);
end;

procedure TWebBrowserEx_V1.Refresh2(var Level: OleVariant);
begin
  DefaultInterface.Refresh2(Level);
end;

procedure TWebBrowserEx_V1.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TWebBrowserEx.InitControlData;
const
  CEventDispIDs: array [0..34] of DWORD = (
    $00000066, $0000006C, $00000069, $0000006A, $00000068, $00000071,
    $00000070, $000000FA, $000000FB, $000000FC, $00000103, $000000FD,
    $000000FE, $000000FF, $00000100, $00000101, $00000102, $00000104,
    $00000106, $00000108, $00000109, $0000010A, $0000010B, $00000107,
    $0000010C, $0000010D, $0000010E, $0000010F, $000000E1, $000000E2,
    $000000E3, $00000110, $00000111, $0000011A, $0000011B);
  CControlData: TControlData2 = (
    ClassID: '{8856F961-340A-11D0-A96B-00C04FD705A2}';
    EventIID: '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
    EventCount: 35;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80040111*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnStatusTextChange) - Cardinal(Self);
end;

procedure TWebBrowserEx.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IWebBrowser2;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TWebBrowserEx.GetControlInterface: IWebBrowser2;
begin
  CreateControl;
  Result := FIntf;
end;

function TWebBrowserEx.Get_Application: IDispatch;
begin
    Result := DefaultInterface.Application;
end;

function TWebBrowserEx.Get_Parent: IDispatch;
begin
    Result := DefaultInterface.Parent;
end;

function TWebBrowserEx.Get_Container: IDispatch;
begin
    Result := DefaultInterface.Container;
end;

function TWebBrowserEx.Get_Document: IDispatch;
begin
    Result := DefaultInterface.Document;
end;

procedure TWebBrowserEx.GoBack;
begin
  DefaultInterface.GoBack;
end;

procedure TWebBrowserEx.GoForward;
begin
  DefaultInterface.GoForward;
end;

procedure TWebBrowserEx.GoHome;
begin
  DefaultInterface.GoHome;
end;

procedure TWebBrowserEx.GoSearch;
begin
  DefaultInterface.GoSearch;
end;

procedure TWebBrowserEx.Navigate(const URL: WideString);
begin
  DefaultInterface.Navigate(URL, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.Navigate(const URL: WideString; var Flags: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.Navigate(const URL: WideString; var Flags: OleVariant; 
                               var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.Navigate(const URL: WideString; var Flags: OleVariant; 
                               var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, EmptyParam);
end;

procedure TWebBrowserEx.Navigate(const URL: WideString; var Flags: OleVariant; 
                               var TargetFrameName: OleVariant; var PostData: OleVariant; 
                               var Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TWebBrowserEx.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TWebBrowserEx.Refresh2;
begin
  DefaultInterface.Refresh2(EmptyParam);
end;

procedure TWebBrowserEx.Refresh2(var Level: OleVariant);
begin
  DefaultInterface.Refresh2(Level);
end;

procedure TWebBrowserEx.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TWebBrowserEx.Quit;
begin
  DefaultInterface.Quit;
end;

procedure TWebBrowserEx.ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
begin
  DefaultInterface.ClientToWindow(pcx, pcy);
end;

procedure TWebBrowserEx.PutProperty(const Property_: WideString; vtValue: OleVariant);
begin
  DefaultInterface.PutProperty(Property_, vtValue);
end;

function TWebBrowserEx.GetProperty(const Property_: WideString): OleVariant;
begin
  Result := DefaultInterface.GetProperty(Property_);
end;

procedure TWebBrowserEx.Navigate2(var URL: OleVariant);
begin
  DefaultInterface.Navigate2(URL, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.Navigate2(var URL: OleVariant; var Flags: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, EmptyParam);
end;

procedure TWebBrowserEx.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                var TargetFrameName: OleVariant; var PostData: OleVariant; 
                                var Headers: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, Headers);
end;

function TWebBrowserEx.QueryStatusWB(cmdID: OLECMDID): OLECMDF;
begin
  Result := DefaultInterface.QueryStatusWB(cmdID);
end;

procedure TWebBrowserEx.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, EmptyParam);
end;

procedure TWebBrowserEx.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant; 
                             var pvaOut: OleVariant);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, pvaOut);
end;

procedure TWebBrowserEx.ShowBrowserBar(var pvaClsid: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, EmptyParam, EmptyParam);
end;

procedure TWebBrowserEx.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, EmptyParam);
end;

procedure TWebBrowserEx.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                                     var pvarSize: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, pvarSize);
end;

class function CoInternetExplorer.Create: IWebBrowser2;
begin
  Result := CreateComObject(CLASS_InternetExplorer) as IWebBrowser2;
end;

class function CoInternetExplorer.CreateRemote(const MachineName: string): IWebBrowser2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_InternetExplorer) as IWebBrowser2;
end;

procedure TInternetExplorer.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{0002DF01-0000-0000-C000-000000000046}';
    IntfIID:   '{D30C1661-CDAF-11D0-8A3E-00C04FC9E26E}';
    EventIID:  '{34A715A0-6587-11D0-924A-0020AFC7AC4D}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TInternetExplorer.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IWebBrowser2;
  end;
end;

procedure TInternetExplorer.ConnectTo(svrIntf: IWebBrowser2);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TInternetExplorer.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TInternetExplorer.GetDefaultInterface: IWebBrowser2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TInternetExplorer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TInternetExplorerProperties.Create(Self);
{$ENDIF}
end;

destructor TInternetExplorer.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TInternetExplorer.GetServerProperties: TInternetExplorerProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TInternetExplorer.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    102: if Assigned(FOnStatusTextChange) then
         FOnStatusTextChange(Self, Params[0] {const WideString});
    108: if Assigned(FOnProgressChange) then
         FOnProgressChange(Self,
                           Params[0] {Integer},
                           Params[1] {Integer});
    105: if Assigned(FOnCommandStateChange) then
         FOnCommandStateChange(Self,
                               Params[0] {Integer},
                               Params[1] {WordBool});
    106: if Assigned(FOnDownloadBegin) then
         FOnDownloadBegin(Self);
    104: if Assigned(FOnDownloadComplete) then
         FOnDownloadComplete(Self);
    113: if Assigned(FOnTitleChange) then
         FOnTitleChange(Self, Params[0] {const WideString});
    112: if Assigned(FOnPropertyChange) then
         FOnPropertyChange(Self, Params[0] {const WideString});
    250: if Assigned(FOnBeforeNavigate2) then
         FOnBeforeNavigate2(Self,
                            Params[0] {const IDispatch},
                            OleVariant((TVarData(Params[1]).VPointer)^) {var OleVariant},
                            OleVariant((TVarData(Params[2]).VPointer)^) {var OleVariant},
                            OleVariant((TVarData(Params[3]).VPointer)^) {var OleVariant},
                            OleVariant((TVarData(Params[4]).VPointer)^) {var OleVariant},
                            OleVariant((TVarData(Params[5]).VPointer)^) {var OleVariant},
                            WordBool((TVarData(Params[6]).VPointer)^) {var WordBool});
    251: if Assigned(FOnNewWindow2) then
         FOnNewWindow2(Self,
                       IDispatch((TVarData(Params[0]).VPointer)^) {var IDispatch},
                       WordBool((TVarData(Params[1]).VPointer)^) {var WordBool});
    252: if Assigned(FOnNavigateComplete2) then
         FOnNavigateComplete2(Self,
                              Params[0] {const IDispatch},
                              OleVariant((TVarData(Params[1]).VPointer)^) {var OleVariant});
    259: if Assigned(FOnDocumentComplete) then
         FOnDocumentComplete(Self,
                             Params[0] {const IDispatch},
                             OleVariant((TVarData(Params[1]).VPointer)^) {var OleVariant});
    253: if Assigned(FOnQuit) then
         FOnQuit(Self);
    254: if Assigned(FOnVisible) then
         FOnVisible(Self, Params[0] {WordBool});
    255: if Assigned(FOnToolBar) then
         FOnToolBar(Self, Params[0] {WordBool});
    256: if Assigned(FOnMenuBar) then
         FOnMenuBar(Self, Params[0] {WordBool});
    257: if Assigned(FOnStatusBar) then
         FOnStatusBar(Self, Params[0] {WordBool});
    258: if Assigned(FOnFullScreen) then
         FOnFullScreen(Self, Params[0] {WordBool});
    260: if Assigned(FOnTheaterMode) then
         FOnTheaterMode(Self, Params[0] {WordBool});
    262: if Assigned(FOnWindowSetResizable) then
         FOnWindowSetResizable(Self, Params[0] {WordBool});
    264: if Assigned(FOnWindowSetLeft) then
         FOnWindowSetLeft(Self, Params[0] {Integer});
    265: if Assigned(FOnWindowSetTop) then
         FOnWindowSetTop(Self, Params[0] {Integer});
    266: if Assigned(FOnWindowSetWidth) then
         FOnWindowSetWidth(Self, Params[0] {Integer});
    267: if Assigned(FOnWindowSetHeight) then
         FOnWindowSetHeight(Self, Params[0] {Integer});
    263: if Assigned(FOnWindowClosing) then
         FOnWindowClosing(Self,
                          Params[0] {WordBool},
                          WordBool((TVarData(Params[1]).VPointer)^) {var WordBool});
    268: if Assigned(FOnClientToHostWindow) then
         FOnClientToHostWindow(Self,
                               Integer((TVarData(Params[0]).VPointer)^) {var Integer},
                               Integer((TVarData(Params[1]).VPointer)^) {var Integer});
    269: if Assigned(FOnSetSecureLockIcon) then
         FOnSetSecureLockIcon(Self, Params[0] {Integer});
    270: if Assigned(FOnFileDownload) then
         FOnFileDownload(Self,
                         Params[0] {WordBool},
                         WordBool((TVarData(Params[1]).VPointer)^) {var WordBool});
    271: if Assigned(FOnNavigateError) then
         FOnNavigateError(Self,
                          Params[0] {const IDispatch},
                          OleVariant((TVarData(Params[1]).VPointer)^) {var OleVariant},
                          OleVariant((TVarData(Params[2]).VPointer)^) {var OleVariant},
                          OleVariant((TVarData(Params[3]).VPointer)^) {var OleVariant},
                          WordBool((TVarData(Params[4]).VPointer)^) {var WordBool});
    225: if Assigned(FOnPrintTemplateInstantiation) then
         FOnPrintTemplateInstantiation(Self, Params[0] {const IDispatch});
    226: if Assigned(FOnPrintTemplateTeardown) then
         FOnPrintTemplateTeardown(Self, Params[0] {const IDispatch});
    227: if Assigned(FOnUpdatePageStatus) then
         FOnUpdatePageStatus(Self,
                             Params[0] {const IDispatch},
                             OleVariant((TVarData(Params[1]).VPointer)^) {var OleVariant},
                             OleVariant((TVarData(Params[2]).VPointer)^) {var OleVariant});
    272: if Assigned(FOnPrivacyImpactedStateChange) then
         FOnPrivacyImpactedStateChange(Self, Params[0] {WordBool});
    273: if Assigned(FOnNewWindow3) then
         FOnNewWindow3(Self,
                       IDispatch((TVarData(Params[0]).VPointer)^) {var IDispatch},
                       WordBool((TVarData(Params[1]).VPointer)^) {var WordBool},
                       Params[2] {LongWord},
                       Params[3] {const WideString},
                       Params[4] {const WideString});
    282: if Assigned(FOnSetPhishingFilterStatus) then
         FOnSetPhishingFilterStatus(Self, Params[0] {Integer});
    283: if Assigned(FOnWindowStateChanged) then
         FOnWindowStateChanged(Self,
                               Params[0] {LongWord},
                               Params[1] {LongWord});
  end; {case DispID}
end;

function TInternetExplorer.Get_Application: IDispatch;
begin
    Result := DefaultInterface.Application;
end;

function TInternetExplorer.Get_Parent: IDispatch;
begin
    Result := DefaultInterface.Parent;
end;

function TInternetExplorer.Get_Container: IDispatch;
begin
    Result := DefaultInterface.Container;
end;

function TInternetExplorer.Get_Document: IDispatch;
begin
    Result := DefaultInterface.Document;
end;

function TInternetExplorer.Get_TopLevelContainer: WordBool;
begin
    Result := DefaultInterface.TopLevelContainer;
end;

function TInternetExplorer.Get_type_: WideString;
begin
    Result := DefaultInterface.type_;
end;

function TInternetExplorer.Get_Left: Integer;
begin
    Result := DefaultInterface.Left;
end;

procedure TInternetExplorer.Set_Left(pl: Integer);
begin
  DefaultInterface.Set_Left(pl);
end;

function TInternetExplorer.Get_Top: Integer;
begin
    Result := DefaultInterface.Top;
end;

procedure TInternetExplorer.Set_Top(pl: Integer);
begin
  DefaultInterface.Set_Top(pl);
end;

function TInternetExplorer.Get_Width: Integer;
begin
    Result := DefaultInterface.Width;
end;

procedure TInternetExplorer.Set_Width(pl: Integer);
begin
  DefaultInterface.Set_Width(pl);
end;

function TInternetExplorer.Get_Height: Integer;
begin
    Result := DefaultInterface.Height;
end;

procedure TInternetExplorer.Set_Height(pl: Integer);
begin
  DefaultInterface.Set_Height(pl);
end;

function TInternetExplorer.Get_LocationName: WideString;
begin
    Result := DefaultInterface.LocationName;
end;

function TInternetExplorer.Get_LocationURL: WideString;
begin
    Result := DefaultInterface.LocationURL;
end;

function TInternetExplorer.Get_Busy: WordBool;
begin
    Result := DefaultInterface.Busy;
end;

function TInternetExplorer.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

function TInternetExplorer.Get_HWND: Integer;
begin
    Result := DefaultInterface.HWND;
end;

function TInternetExplorer.Get_FullName: WideString;
begin
    Result := DefaultInterface.FullName;
end;

function TInternetExplorer.Get_Path: WideString;
begin
    Result := DefaultInterface.Path;
end;

function TInternetExplorer.Get_Visible: WordBool;
begin
    Result := DefaultInterface.Visible;
end;

procedure TInternetExplorer.Set_Visible(pBool: WordBool);
begin
  DefaultInterface.Set_Visible(pBool);
end;

function TInternetExplorer.Get_StatusBar: WordBool;
begin
    Result := DefaultInterface.StatusBar;
end;

procedure TInternetExplorer.Set_StatusBar(pBool: WordBool);
begin
  DefaultInterface.Set_StatusBar(pBool);
end;

function TInternetExplorer.Get_StatusText: WideString;
begin
    Result := DefaultInterface.StatusText;
end;

procedure TInternetExplorer.Set_StatusText(const StatusText: WideString);
  { Warning: The property StatusText has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.StatusText := StatusText;
end;

function TInternetExplorer.Get_ToolBar: SYSINT;
begin
    Result := DefaultInterface.ToolBar;
end;

procedure TInternetExplorer.Set_ToolBar(Value: SYSINT);
begin
  DefaultInterface.Set_ToolBar(Value);
end;

function TInternetExplorer.Get_MenuBar: WordBool;
begin
    Result := DefaultInterface.MenuBar;
end;

procedure TInternetExplorer.Set_MenuBar(Value: WordBool);
begin
  DefaultInterface.Set_MenuBar(Value);
end;

function TInternetExplorer.Get_FullScreen: WordBool;
begin
    Result := DefaultInterface.FullScreen;
end;

procedure TInternetExplorer.Set_FullScreen(pbFullScreen: WordBool);
begin
  DefaultInterface.Set_FullScreen(pbFullScreen);
end;

function TInternetExplorer.Get_ReadyState: tagREADYSTATE;
begin
    Result := DefaultInterface.ReadyState;
end;

function TInternetExplorer.Get_Offline: WordBool;
begin
    Result := DefaultInterface.Offline;
end;

procedure TInternetExplorer.Set_Offline(pbOffline: WordBool);
begin
  DefaultInterface.Set_Offline(pbOffline);
end;

function TInternetExplorer.Get_Silent: WordBool;
begin
    Result := DefaultInterface.Silent;
end;

procedure TInternetExplorer.Set_Silent(pbSilent: WordBool);
begin
  DefaultInterface.Set_Silent(pbSilent);
end;

function TInternetExplorer.Get_RegisterAsBrowser: WordBool;
begin
    Result := DefaultInterface.RegisterAsBrowser;
end;

procedure TInternetExplorer.Set_RegisterAsBrowser(pbRegister: WordBool);
begin
  DefaultInterface.Set_RegisterAsBrowser(pbRegister);
end;

function TInternetExplorer.Get_RegisterAsDropTarget: WordBool;
begin
    Result := DefaultInterface.RegisterAsDropTarget;
end;

procedure TInternetExplorer.Set_RegisterAsDropTarget(pbRegister: WordBool);
begin
  DefaultInterface.Set_RegisterAsDropTarget(pbRegister);
end;

function TInternetExplorer.Get_TheaterMode: WordBool;
begin
    Result := DefaultInterface.TheaterMode;
end;

procedure TInternetExplorer.Set_TheaterMode(pbRegister: WordBool);
begin
  DefaultInterface.Set_TheaterMode(pbRegister);
end;

function TInternetExplorer.Get_AddressBar: WordBool;
begin
    Result := DefaultInterface.AddressBar;
end;

procedure TInternetExplorer.Set_AddressBar(Value: WordBool);
begin
  DefaultInterface.Set_AddressBar(Value);
end;

function TInternetExplorer.Get_Resizable: WordBool;
begin
    Result := DefaultInterface.Resizable;
end;

procedure TInternetExplorer.Set_Resizable(Value: WordBool);
begin
  DefaultInterface.Set_Resizable(Value);
end;

procedure TInternetExplorer.GoBack;
begin
  DefaultInterface.GoBack;
end;

procedure TInternetExplorer.GoForward;
begin
  DefaultInterface.GoForward;
end;

procedure TInternetExplorer.GoHome;
begin
  DefaultInterface.GoHome;
end;

procedure TInternetExplorer.GoSearch;
begin
  DefaultInterface.GoSearch;
end;

procedure TInternetExplorer.Navigate(const URL: WideString);
begin
  DefaultInterface.Navigate(URL, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.Navigate(const URL: WideString; var Flags: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.Navigate(const URL: WideString; var Flags: OleVariant; 
                                     var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.Navigate(const URL: WideString; var Flags: OleVariant; 
                                     var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, EmptyParam);
end;

procedure TInternetExplorer.Navigate(const URL: WideString; var Flags: OleVariant; 
                                     var TargetFrameName: OleVariant; var PostData: OleVariant; 
                                     var Headers: OleVariant);
begin
  DefaultInterface.Navigate(URL, Flags, TargetFrameName, PostData, Headers);
end;

procedure TInternetExplorer.Refresh;
begin
  DefaultInterface.Refresh;
end;

procedure TInternetExplorer.Refresh2;
begin
  DefaultInterface.Refresh2(EmptyParam);
end;

procedure TInternetExplorer.Refresh2(var Level: OleVariant);
begin
  DefaultInterface.Refresh2(Level);
end;

procedure TInternetExplorer.Stop;
begin
  DefaultInterface.Stop;
end;

procedure TInternetExplorer.Quit;
begin
  DefaultInterface.Quit;
end;

procedure TInternetExplorer.ClientToWindow(var pcx: SYSINT; var pcy: SYSINT);
begin
  DefaultInterface.ClientToWindow(pcx, pcy);
end;

procedure TInternetExplorer.PutProperty(const Property_: WideString; vtValue: OleVariant);
begin
  DefaultInterface.PutProperty(Property_, vtValue);
end;

function TInternetExplorer.GetProperty(const Property_: WideString): OleVariant;
begin
  Result := DefaultInterface.GetProperty(Property_);
end;

procedure TInternetExplorer.Navigate2(var URL: OleVariant);
begin
  DefaultInterface.Navigate2(URL, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.Navigate2(var URL: OleVariant; var Flags: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                      var TargetFrameName: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                      var TargetFrameName: OleVariant; var PostData: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, EmptyParam);
end;

procedure TInternetExplorer.Navigate2(var URL: OleVariant; var Flags: OleVariant; 
                                      var TargetFrameName: OleVariant; var PostData: OleVariant; 
                                      var Headers: OleVariant);
begin
  DefaultInterface.Navigate2(URL, Flags, TargetFrameName, PostData, Headers);
end;

function TInternetExplorer.QueryStatusWB(cmdID: OLECMDID): OLECMDF;
begin
  Result := DefaultInterface.QueryStatusWB(cmdID);
end;

procedure TInternetExplorer.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; var pvaIn: OleVariant);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, EmptyParam);
end;

procedure TInternetExplorer.ExecWB(cmdID: OLECMDID; cmdexecopt: OLECMDEXECOPT; 
                                   var pvaIn: OleVariant; var pvaOut: OleVariant);
begin
  DefaultInterface.ExecWB(cmdID, cmdexecopt, pvaIn, pvaOut);
end;

procedure TInternetExplorer.ShowBrowserBar(var pvaClsid: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, EmptyParam, EmptyParam);
end;

procedure TInternetExplorer.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, EmptyParam);
end;

procedure TInternetExplorer.ShowBrowserBar(var pvaClsid: OleVariant; var pvarShow: OleVariant; 
                                           var pvarSize: OleVariant);
begin
  DefaultInterface.ShowBrowserBar(pvaClsid, pvarShow, pvarSize);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TInternetExplorerProperties.Create(AServer: TInternetExplorer);
begin
  inherited Create;
  FServer := AServer;
end;

function TInternetExplorerProperties.GetDefaultInterface: IWebBrowser2;
begin
  Result := FServer.DefaultInterface;
end;

function TInternetExplorerProperties.Get_Application: IDispatch;
begin
    Result := DefaultInterface.Application;
end;

function TInternetExplorerProperties.Get_Parent: IDispatch;
begin
    Result := DefaultInterface.Parent;
end;

function TInternetExplorerProperties.Get_Container: IDispatch;
begin
    Result := DefaultInterface.Container;
end;

function TInternetExplorerProperties.Get_Document: IDispatch;
begin
    Result := DefaultInterface.Document;
end;

function TInternetExplorerProperties.Get_TopLevelContainer: WordBool;
begin
    Result := DefaultInterface.TopLevelContainer;
end;

function TInternetExplorerProperties.Get_type_: WideString;
begin
    Result := DefaultInterface.type_;
end;

function TInternetExplorerProperties.Get_Left: Integer;
begin
    Result := DefaultInterface.Left;
end;

procedure TInternetExplorerProperties.Set_Left(pl: Integer);
begin
  DefaultInterface.Set_Left(pl);
end;

function TInternetExplorerProperties.Get_Top: Integer;
begin
    Result := DefaultInterface.Top;
end;

procedure TInternetExplorerProperties.Set_Top(pl: Integer);
begin
  DefaultInterface.Set_Top(pl);
end;

function TInternetExplorerProperties.Get_Width: Integer;
begin
    Result := DefaultInterface.Width;
end;

procedure TInternetExplorerProperties.Set_Width(pl: Integer);
begin
  DefaultInterface.Set_Width(pl);
end;

function TInternetExplorerProperties.Get_Height: Integer;
begin
    Result := DefaultInterface.Height;
end;

procedure TInternetExplorerProperties.Set_Height(pl: Integer);
begin
  DefaultInterface.Set_Height(pl);
end;

function TInternetExplorerProperties.Get_LocationName: WideString;
begin
    Result := DefaultInterface.LocationName;
end;

function TInternetExplorerProperties.Get_LocationURL: WideString;
begin
    Result := DefaultInterface.LocationURL;
end;

function TInternetExplorerProperties.Get_Busy: WordBool;
begin
    Result := DefaultInterface.Busy;
end;

function TInternetExplorerProperties.Get_Name: WideString;
begin
    Result := DefaultInterface.Name;
end;

function TInternetExplorerProperties.Get_HWND: Integer;
begin
    Result := DefaultInterface.HWND;
end;

function TInternetExplorerProperties.Get_FullName: WideString;
begin
    Result := DefaultInterface.FullName;
end;

function TInternetExplorerProperties.Get_Path: WideString;
begin
    Result := DefaultInterface.Path;
end;

function TInternetExplorerProperties.Get_Visible: WordBool;
begin
    Result := DefaultInterface.Visible;
end;

procedure TInternetExplorerProperties.Set_Visible(pBool: WordBool);
begin
  DefaultInterface.Set_Visible(pBool);
end;

function TInternetExplorerProperties.Get_StatusBar: WordBool;
begin
    Result := DefaultInterface.StatusBar;
end;

procedure TInternetExplorerProperties.Set_StatusBar(pBool: WordBool);
begin
  DefaultInterface.Set_StatusBar(pBool);
end;

function TInternetExplorerProperties.Get_StatusText: WideString;
begin
    Result := DefaultInterface.StatusText;
end;

procedure TInternetExplorerProperties.Set_StatusText(const StatusText: WideString);
  { Warning: The property StatusText has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.StatusText := StatusText;
end;

function TInternetExplorerProperties.Get_ToolBar: SYSINT;
begin
    Result := DefaultInterface.ToolBar;
end;

procedure TInternetExplorerProperties.Set_ToolBar(Value: SYSINT);
begin
  DefaultInterface.Set_ToolBar(Value);
end;

function TInternetExplorerProperties.Get_MenuBar: WordBool;
begin
    Result := DefaultInterface.MenuBar;
end;

procedure TInternetExplorerProperties.Set_MenuBar(Value: WordBool);
begin
  DefaultInterface.Set_MenuBar(Value);
end;

function TInternetExplorerProperties.Get_FullScreen: WordBool;
begin
    Result := DefaultInterface.FullScreen;
end;

procedure TInternetExplorerProperties.Set_FullScreen(pbFullScreen: WordBool);
begin
  DefaultInterface.Set_FullScreen(pbFullScreen);
end;

function TInternetExplorerProperties.Get_ReadyState: tagREADYSTATE;
begin
    Result := DefaultInterface.ReadyState;
end;

function TInternetExplorerProperties.Get_Offline: WordBool;
begin
    Result := DefaultInterface.Offline;
end;

procedure TInternetExplorerProperties.Set_Offline(pbOffline: WordBool);
begin
  DefaultInterface.Set_Offline(pbOffline);
end;

function TInternetExplorerProperties.Get_Silent: WordBool;
begin
    Result := DefaultInterface.Silent;
end;

procedure TInternetExplorerProperties.Set_Silent(pbSilent: WordBool);
begin
  DefaultInterface.Set_Silent(pbSilent);
end;

function TInternetExplorerProperties.Get_RegisterAsBrowser: WordBool;
begin
    Result := DefaultInterface.RegisterAsBrowser;
end;

procedure TInternetExplorerProperties.Set_RegisterAsBrowser(pbRegister: WordBool);
begin
  DefaultInterface.Set_RegisterAsBrowser(pbRegister);
end;

function TInternetExplorerProperties.Get_RegisterAsDropTarget: WordBool;
begin
    Result := DefaultInterface.RegisterAsDropTarget;
end;

procedure TInternetExplorerProperties.Set_RegisterAsDropTarget(pbRegister: WordBool);
begin
  DefaultInterface.Set_RegisterAsDropTarget(pbRegister);
end;

function TInternetExplorerProperties.Get_TheaterMode: WordBool;
begin
    Result := DefaultInterface.TheaterMode;
end;

procedure TInternetExplorerProperties.Set_TheaterMode(pbRegister: WordBool);
begin
  DefaultInterface.Set_TheaterMode(pbRegister);
end;

function TInternetExplorerProperties.Get_AddressBar: WordBool;
begin
    Result := DefaultInterface.AddressBar;
end;

procedure TInternetExplorerProperties.Set_AddressBar(Value: WordBool);
begin
  DefaultInterface.Set_AddressBar(Value);
end;

function TInternetExplorerProperties.Get_Resizable: WordBool;
begin
    Result := DefaultInterface.Resizable;
end;

procedure TInternetExplorerProperties.Set_Resizable(Value: WordBool);
begin
  DefaultInterface.Set_Resizable(Value);
end;

{$ENDIF}

class function CoShellBrowserWindow.Create: IWebBrowser2;
begin
  Result := CreateComObject(CLASS_ShellBrowserWindow) as IWebBrowser2;
end;

class function CoShellBrowserWindow.CreateRemote(const MachineName: string): IWebBrowser2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellBrowserWindow) as IWebBrowser2;
end;

class function CoShellWindows.Create: IShellWindows;
begin
  Result := CreateComObject(CLASS_ShellWindows) as IShellWindows;
end;

class function CoShellWindows.CreateRemote(const MachineName: string): IShellWindows;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellWindows) as IShellWindows;
end;

procedure TShellWindows.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{9BA05972-F6A8-11CF-A442-00A0C90A8F39}';
    IntfIID:   '{85CB6900-4D95-11CF-960C-0080C7F4EE85}';
    EventIID:  '{FE4106E0-399A-11D0-A48C-00A0C90A8F39}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TShellWindows.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IShellWindows;
  end;
end;

procedure TShellWindows.ConnectTo(svrIntf: IShellWindows);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TShellWindows.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TShellWindows.GetDefaultInterface: IShellWindows;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TShellWindows.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TShellWindowsProperties.Create(Self);
{$ENDIF}
end;

destructor TShellWindows.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TShellWindows.GetServerProperties: TShellWindowsProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TShellWindows.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    200: if Assigned(FOnWindowRegistered) then
         FOnWindowRegistered(Self, Params[0] {Integer});
    201: if Assigned(FOnWindowRevoked) then
         FOnWindowRevoked(Self, Params[0] {Integer});
  end; {case DispID}
end;

function TShellWindows.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

function TShellWindows.Item: IDispatch;
begin
  Result := DefaultInterface.Item(EmptyParam);
end;

function TShellWindows.Item(index: OleVariant): IDispatch;
begin
  Result := DefaultInterface.Item(index);
end;

function TShellWindows._NewEnum: IUnknown;
begin
  Result := DefaultInterface._NewEnum;
end;

procedure TShellWindows.Register(const pid: IDispatch; HWND: Integer; swClass: SYSINT; 
                                 out plCookie: Integer);
begin
  DefaultInterface.Register(pid, HWND, swClass, plCookie);
end;

procedure TShellWindows.RegisterPending(lThreadId: Integer; var pvarloc: OleVariant; 
                                        var pvarlocRoot: OleVariant; swClass: SYSINT; 
                                        out plCookie: Integer);
begin
  DefaultInterface.RegisterPending(lThreadId, pvarloc, pvarlocRoot, swClass, plCookie);
end;

procedure TShellWindows.Revoke(lCookie: Integer);
begin
  DefaultInterface.Revoke(lCookie);
end;

procedure TShellWindows.OnNavigate(lCookie: Integer; var pvarloc: OleVariant);
begin
  DefaultInterface.OnNavigate(lCookie, pvarloc);
end;

procedure TShellWindows.OnActivated(lCookie: Integer; fActive: WordBool);
begin
  DefaultInterface.OnActivated(lCookie, fActive);
end;

function TShellWindows.FindWindowSW(var pvarloc: OleVariant; var pvarlocRoot: OleVariant; 
                                    swClass: SYSINT; out pHWND: Integer; swfwOptions: SYSINT): IDispatch;
begin
  Result := DefaultInterface.FindWindowSW(pvarloc, pvarlocRoot, swClass, pHWND, swfwOptions);
end;

procedure TShellWindows.OnCreated(lCookie: Integer; const punk: IUnknown);
begin
  DefaultInterface.OnCreated(lCookie, punk);
end;

procedure TShellWindows.ProcessAttachDetach(fAttach: WordBool);
begin
  DefaultInterface.ProcessAttachDetach(fAttach);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TShellWindowsProperties.Create(AServer: TShellWindows);
begin
  inherited Create;
  FServer := AServer;
end;

function TShellWindowsProperties.GetDefaultInterface: IShellWindows;
begin
  Result := FServer.DefaultInterface;
end;

function TShellWindowsProperties.Get_Count: Integer;
begin
    Result := DefaultInterface.Count;
end;

{$ENDIF}

class function CoShellUIHelper.Create: IShellUIHelper2;
begin
  Result := CreateComObject(CLASS_ShellUIHelper) as IShellUIHelper2;
end;

class function CoShellUIHelper.CreateRemote(const MachineName: string): IShellUIHelper2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellUIHelper) as IShellUIHelper2;
end;

procedure TShellUIHelper.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
    IntfIID:   '{A7FE6EDA-1932-4281-B881-87B31B8BC52C}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TShellUIHelper.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IShellUIHelper2;
  end;
end;

procedure TShellUIHelper.ConnectTo(svrIntf: IShellUIHelper2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TShellUIHelper.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TShellUIHelper.GetDefaultInterface: IShellUIHelper2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TShellUIHelper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TShellUIHelperProperties.Create(Self);
{$ENDIF}
end;

destructor TShellUIHelper.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TShellUIHelper.GetServerProperties: TShellUIHelperProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TShellUIHelper.ResetFirstBootMode;
begin
  DefaultInterface.ResetFirstBootMode;
end;

procedure TShellUIHelper.ResetSafeMode;
begin
  DefaultInterface.ResetSafeMode;
end;

procedure TShellUIHelper.RefreshOfflineDesktop;
begin
  DefaultInterface.RefreshOfflineDesktop;
end;

procedure TShellUIHelper.AddFavorite(const URL: WideString);
begin
  DefaultInterface.AddFavorite(URL, EmptyParam);
end;

procedure TShellUIHelper.AddFavorite(const URL: WideString; var Title: OleVariant);
begin
  DefaultInterface.AddFavorite(URL, Title);
end;

procedure TShellUIHelper.AddChannel(const URL: WideString);
begin
  DefaultInterface.AddChannel(URL);
end;

procedure TShellUIHelper.AddDesktopComponent(const URL: WideString; const Type_: WideString);
begin
  DefaultInterface.AddDesktopComponent(URL, Type_, EmptyParam, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TShellUIHelper.AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                             var Left: OleVariant);
begin
  DefaultInterface.AddDesktopComponent(URL, Type_, Left, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TShellUIHelper.AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                             var Left: OleVariant; var Top: OleVariant);
begin
  DefaultInterface.AddDesktopComponent(URL, Type_, Left, Top, EmptyParam, EmptyParam);
end;

procedure TShellUIHelper.AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                             var Left: OleVariant; var Top: OleVariant; 
                                             var Width: OleVariant);
begin
  DefaultInterface.AddDesktopComponent(URL, Type_, Left, Top, Width, EmptyParam);
end;

procedure TShellUIHelper.AddDesktopComponent(const URL: WideString; const Type_: WideString; 
                                             var Left: OleVariant; var Top: OleVariant; 
                                             var Width: OleVariant; var Height: OleVariant);
begin
  DefaultInterface.AddDesktopComponent(URL, Type_, Left, Top, Width, Height);
end;

function TShellUIHelper.IsSubscribed(const URL: WideString): WordBool;
begin
  Result := DefaultInterface.IsSubscribed(URL);
end;

procedure TShellUIHelper.NavigateAndFind(const URL: WideString; const strQuery: WideString; 
                                         var varTargetFrame: OleVariant);
begin
  DefaultInterface.NavigateAndFind(URL, strQuery, varTargetFrame);
end;

procedure TShellUIHelper.ImportExportFavorites(fImport: WordBool; const strImpExpPath: WideString);
begin
  DefaultInterface.ImportExportFavorites(fImport, strImpExpPath);
end;

procedure TShellUIHelper.AutoCompleteSaveForm;
begin
  DefaultInterface.AutoCompleteSaveForm(EmptyParam);
end;

procedure TShellUIHelper.AutoCompleteSaveForm(var Form: OleVariant);
begin
  DefaultInterface.AutoCompleteSaveForm(Form);
end;

procedure TShellUIHelper.AutoScan(const strSearch: WideString; const strFailureUrl: WideString);
begin
  DefaultInterface.AutoScan(strSearch, strFailureUrl, EmptyParam);
end;

procedure TShellUIHelper.AutoScan(const strSearch: WideString; const strFailureUrl: WideString; 
                                  var pvarTargetFrame: OleVariant);
begin
  DefaultInterface.AutoScan(strSearch, strFailureUrl, pvarTargetFrame);
end;

procedure TShellUIHelper.AutoCompleteAttach;
begin
  DefaultInterface.AutoCompleteAttach(EmptyParam);
end;

procedure TShellUIHelper.AutoCompleteAttach(var Reserved: OleVariant);
begin
  DefaultInterface.AutoCompleteAttach(Reserved);
end;

function TShellUIHelper.ShowBrowserUI(const bstrName: WideString; var pvarIn: OleVariant): OleVariant;
begin
  Result := DefaultInterface.ShowBrowserUI(bstrName, pvarIn);
end;

procedure TShellUIHelper.AddSearchProvider(const URL: WideString);
begin
  DefaultInterface.AddSearchProvider(URL);
end;

procedure TShellUIHelper.RunOnceShown;
begin
  DefaultInterface.RunOnceShown;
end;

procedure TShellUIHelper.SkipRunOnce;
begin
  DefaultInterface.SkipRunOnce;
end;

procedure TShellUIHelper.CustomizeSettings(fSQM: WordBool; fPhishing: WordBool; 
                                           const bstrLocale: WideString);
begin
  DefaultInterface.CustomizeSettings(fSQM, fPhishing, bstrLocale);
end;

function TShellUIHelper.SqmEnabled: WordBool;
begin
  Result := DefaultInterface.SqmEnabled;
end;

function TShellUIHelper.PhishingEnabled: WordBool;
begin
  Result := DefaultInterface.PhishingEnabled;
end;

function TShellUIHelper.BrandImageUri: WideString;
begin
  Result := DefaultInterface.BrandImageUri;
end;

procedure TShellUIHelper.SkipTabsWelcome;
begin
  DefaultInterface.SkipTabsWelcome;
end;

procedure TShellUIHelper.DiagnoseConnection;
begin
  DefaultInterface.DiagnoseConnection;
end;

procedure TShellUIHelper.CustomizeClearType(fSet: WordBool);
begin
  DefaultInterface.CustomizeClearType(fSet);
end;

function TShellUIHelper.IsSearchProviderInstalled(const URL: WideString): LongWord;
begin
  Result := DefaultInterface.IsSearchProviderInstalled(URL);
end;

function TShellUIHelper.IsSearchMigrated: WordBool;
begin
  Result := DefaultInterface.IsSearchMigrated;
end;

function TShellUIHelper.DefaultSearchProvider: WideString;
begin
  Result := DefaultInterface.DefaultSearchProvider;
end;

procedure TShellUIHelper.RunOnceRequiredSettingsComplete(fComplete: WordBool);
begin
  DefaultInterface.RunOnceRequiredSettingsComplete(fComplete);
end;

function TShellUIHelper.RunOnceHasShown: WordBool;
begin
  Result := DefaultInterface.RunOnceHasShown;
end;

function TShellUIHelper.SearchGuideUrl: WideString;
begin
  Result := DefaultInterface.SearchGuideUrl;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TShellUIHelperProperties.Create(AServer: TShellUIHelper);
begin
  inherited Create;
  FServer := AServer;
end;

function TShellUIHelperProperties.GetDefaultInterface: IShellUIHelper2;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

class function CoShellNameSpace.Create: IShellNameSpace;
begin
  Result := CreateComObject(CLASS_ShellNameSpace) as IShellNameSpace;
end;

class function CoShellNameSpace.CreateRemote(const MachineName: string): IShellNameSpace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellNameSpace) as IShellNameSpace;
end;

procedure TShellNameSpace.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{55136805-B2DE-11D1-B9F2-00A0C98BC547}';
    IntfIID:   '{E572D3C9-37BE-4AE2-825D-D521763E3108}';
    EventIID:  '{55136806-B2DE-11D1-B9F2-00A0C98BC547}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TShellNameSpace.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IShellNameSpace;
  end;
end;

procedure TShellNameSpace.ConnectTo(svrIntf: IShellNameSpace);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TShellNameSpace.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TShellNameSpace.GetDefaultInterface: IShellNameSpace;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TShellNameSpace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TShellNameSpaceProperties.Create(Self);
{$ENDIF}
end;

destructor TShellNameSpace.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TShellNameSpace.GetServerProperties: TShellNameSpaceProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TShellNameSpace.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnFavoritesSelectionChange) then
         FOnFavoritesSelectionChange(Self,
                                     Params[0] {Integer},
                                     Params[1] {Integer},
                                     Params[2] {const WideString},
                                     Params[3] {const WideString},
                                     Params[4] {Integer},
                                     Params[5] {const WideString},
                                     Params[6] {Integer});
    2: if Assigned(FOnSelectionChange) then
         FOnSelectionChange(Self);
    3: if Assigned(FOnDoubleClick) then
         FOnDoubleClick(Self);
    4: if Assigned(FOnInitialized) then
         FOnInitialized(Self);
  end; {case DispID}
end;

function TShellNameSpace.Get_SubscriptionsEnabled: WordBool;
begin
    Result := DefaultInterface.SubscriptionsEnabled;
end;

function TShellNameSpace.Get_EnumOptions: Integer;
begin
    Result := DefaultInterface.EnumOptions;
end;

procedure TShellNameSpace.Set_EnumOptions(pgrfEnumFlags: Integer);
begin
  DefaultInterface.Set_EnumOptions(pgrfEnumFlags);
end;

function TShellNameSpace.Get_SelectedItem: IDispatch;
begin
    Result := DefaultInterface.SelectedItem;
end;

procedure TShellNameSpace.Set_SelectedItem(const pItem: IDispatch);
begin
  DefaultInterface.Set_SelectedItem(pItem);
end;

function TShellNameSpace.Get_Root: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Root;
end;

procedure TShellNameSpace.Set_Root(pvar: OleVariant);
begin
  DefaultInterface.Set_Root(pvar);
end;

function TShellNameSpace.Get_Depth: SYSINT;
begin
    Result := DefaultInterface.Depth;
end;

procedure TShellNameSpace.Set_Depth(piDepth: SYSINT);
begin
  DefaultInterface.Set_Depth(piDepth);
end;

function TShellNameSpace.Get_Mode: SYSUINT;
begin
    Result := DefaultInterface.Mode;
end;

procedure TShellNameSpace.Set_Mode(puMode: SYSUINT);
begin
  DefaultInterface.Set_Mode(puMode);
end;

function TShellNameSpace.Get_Flags: LongWord;
begin
    Result := DefaultInterface.Flags;
end;

procedure TShellNameSpace.Set_Flags(pdwFlags: LongWord);
begin
  DefaultInterface.Set_Flags(pdwFlags);
end;

procedure TShellNameSpace.Set_TVFlags(dwFlags: LongWord);
begin
  DefaultInterface.Set_TVFlags(dwFlags);
end;

function TShellNameSpace.Get_TVFlags: LongWord;
begin
    Result := DefaultInterface.TVFlags;
end;

function TShellNameSpace.Get_Columns: WideString;
begin
    Result := DefaultInterface.Columns;
end;

procedure TShellNameSpace.Set_Columns(const bstrColumns: WideString);
  { Warning: The property Columns has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Columns := bstrColumns;
end;

function TShellNameSpace.Get_CountViewTypes: SYSINT;
begin
    Result := DefaultInterface.CountViewTypes;
end;

procedure TShellNameSpace.MoveSelectionUp;
begin
  DefaultInterface.MoveSelectionUp;
end;

procedure TShellNameSpace.MoveSelectionDown;
begin
  DefaultInterface.MoveSelectionDown;
end;

procedure TShellNameSpace.ResetSort;
begin
  DefaultInterface.ResetSort;
end;

procedure TShellNameSpace.NewFolder;
begin
  DefaultInterface.NewFolder;
end;

procedure TShellNameSpace.Synchronize;
begin
  DefaultInterface.Synchronize;
end;

procedure TShellNameSpace.Import;
begin
  DefaultInterface.Import;
end;

procedure TShellNameSpace.Export;
begin
  DefaultInterface.Export;
end;

procedure TShellNameSpace.InvokeContextMenuCommand(const strCommand: WideString);
begin
  DefaultInterface.InvokeContextMenuCommand(strCommand);
end;

procedure TShellNameSpace.MoveSelectionTo;
begin
  DefaultInterface.MoveSelectionTo;
end;

function TShellNameSpace.CreateSubscriptionForSelection: WordBool;
begin
  Result := DefaultInterface.CreateSubscriptionForSelection;
end;

function TShellNameSpace.DeleteSubscriptionForSelection: WordBool;
begin
  Result := DefaultInterface.DeleteSubscriptionForSelection;
end;

procedure TShellNameSpace.SetRoot(const bstrFullPath: WideString);
begin
  DefaultInterface.SetRoot(bstrFullPath);
end;

procedure TShellNameSpace.SetViewType(iType: SYSINT);
begin
  DefaultInterface.SetViewType(iType);
end;

function TShellNameSpace.SelectedItems: IDispatch;
begin
  Result := DefaultInterface.SelectedItems;
end;

procedure TShellNameSpace.Expand(var_: OleVariant; iDepth: SYSINT);
begin
  DefaultInterface.Expand(var_, iDepth);
end;

procedure TShellNameSpace.UnselectAll;
begin
  DefaultInterface.UnselectAll;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TShellNameSpaceProperties.Create(AServer: TShellNameSpace);
begin
  inherited Create;
  FServer := AServer;
end;

function TShellNameSpaceProperties.GetDefaultInterface: IShellNameSpace;
begin
  Result := FServer.DefaultInterface;
end;

function TShellNameSpaceProperties.Get_SubscriptionsEnabled: WordBool;
begin
    Result := DefaultInterface.SubscriptionsEnabled;
end;

function TShellNameSpaceProperties.Get_EnumOptions: Integer;
begin
    Result := DefaultInterface.EnumOptions;
end;

procedure TShellNameSpaceProperties.Set_EnumOptions(pgrfEnumFlags: Integer);
begin
  DefaultInterface.Set_EnumOptions(pgrfEnumFlags);
end;

function TShellNameSpaceProperties.Get_SelectedItem: IDispatch;
begin
    Result := DefaultInterface.SelectedItem;
end;

procedure TShellNameSpaceProperties.Set_SelectedItem(const pItem: IDispatch);
begin
  DefaultInterface.Set_SelectedItem(pItem);
end;

function TShellNameSpaceProperties.Get_Root: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Root;
end;

procedure TShellNameSpaceProperties.Set_Root(pvar: OleVariant);
begin
  DefaultInterface.Set_Root(pvar);
end;

function TShellNameSpaceProperties.Get_Depth: SYSINT;
begin
    Result := DefaultInterface.Depth;
end;

procedure TShellNameSpaceProperties.Set_Depth(piDepth: SYSINT);
begin
  DefaultInterface.Set_Depth(piDepth);
end;

function TShellNameSpaceProperties.Get_Mode: SYSUINT;
begin
    Result := DefaultInterface.Mode;
end;

procedure TShellNameSpaceProperties.Set_Mode(puMode: SYSUINT);
begin
  DefaultInterface.Set_Mode(puMode);
end;

function TShellNameSpaceProperties.Get_Flags: LongWord;
begin
    Result := DefaultInterface.Flags;
end;

procedure TShellNameSpaceProperties.Set_Flags(pdwFlags: LongWord);
begin
  DefaultInterface.Set_Flags(pdwFlags);
end;

procedure TShellNameSpaceProperties.Set_TVFlags(dwFlags: LongWord);
begin
  DefaultInterface.Set_TVFlags(dwFlags);
end;

function TShellNameSpaceProperties.Get_TVFlags: LongWord;
begin
    Result := DefaultInterface.TVFlags;
end;

function TShellNameSpaceProperties.Get_Columns: WideString;
begin
    Result := DefaultInterface.Columns;
end;

procedure TShellNameSpaceProperties.Set_Columns(const bstrColumns: WideString);
  { Warning: The property Columns has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Columns := bstrColumns;
end;

function TShellNameSpaceProperties.Get_CountViewTypes: SYSINT;
begin
    Result := DefaultInterface.CountViewTypes;
end;

{$ENDIF}

class function CoShellShellNameSpace.Create: IShellNameSpace;
begin
  Result := CreateComObject(CLASS_ShellShellNameSpace) as IShellNameSpace;
end;

class function CoShellShellNameSpace.CreateRemote(const MachineName: string): IShellNameSpace;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_ShellShellNameSpace) as IShellNameSpace;
end;

procedure TShellShellNameSpace.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{2F2F1F96-2BC1-4B1C-BE28-EA3774F4676A}';
    IntfIID:   '{E572D3C9-37BE-4AE2-825D-D521763E3108}';
    EventIID:  '{55136806-B2DE-11D1-B9F2-00A0C98BC547}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TShellShellNameSpace.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IShellNameSpace;
  end;
end;

procedure TShellShellNameSpace.ConnectTo(svrIntf: IShellNameSpace);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TShellShellNameSpace.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TShellShellNameSpace.GetDefaultInterface: IShellNameSpace;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TShellShellNameSpace.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TShellShellNameSpaceProperties.Create(Self);
{$ENDIF}
end;

destructor TShellShellNameSpace.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TShellShellNameSpace.GetServerProperties: TShellShellNameSpaceProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TShellShellNameSpace.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnFavoritesSelectionChange) then
         FOnFavoritesSelectionChange(Self,
                                     Params[0] {Integer},
                                     Params[1] {Integer},
                                     Params[2] {const WideString},
                                     Params[3] {const WideString},
                                     Params[4] {Integer},
                                     Params[5] {const WideString},
                                     Params[6] {Integer});
    2: if Assigned(FOnSelectionChange) then
         FOnSelectionChange(Self);
    3: if Assigned(FOnDoubleClick) then
         FOnDoubleClick(Self);
    4: if Assigned(FOnInitialized) then
         FOnInitialized(Self);
  end; {case DispID}
end;

function TShellShellNameSpace.Get_SubscriptionsEnabled: WordBool;
begin
    Result := DefaultInterface.SubscriptionsEnabled;
end;

function TShellShellNameSpace.Get_EnumOptions: Integer;
begin
    Result := DefaultInterface.EnumOptions;
end;

procedure TShellShellNameSpace.Set_EnumOptions(pgrfEnumFlags: Integer);
begin
  DefaultInterface.Set_EnumOptions(pgrfEnumFlags);
end;

function TShellShellNameSpace.Get_SelectedItem: IDispatch;
begin
    Result := DefaultInterface.SelectedItem;
end;

procedure TShellShellNameSpace.Set_SelectedItem(const pItem: IDispatch);
begin
  DefaultInterface.Set_SelectedItem(pItem);
end;

function TShellShellNameSpace.Get_Root: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Root;
end;

procedure TShellShellNameSpace.Set_Root(pvar: OleVariant);
begin
  DefaultInterface.Set_Root(pvar);
end;

function TShellShellNameSpace.Get_Depth: SYSINT;
begin
    Result := DefaultInterface.Depth;
end;

procedure TShellShellNameSpace.Set_Depth(piDepth: SYSINT);
begin
  DefaultInterface.Set_Depth(piDepth);
end;

function TShellShellNameSpace.Get_Mode: SYSUINT;
begin
    Result := DefaultInterface.Mode;
end;

procedure TShellShellNameSpace.Set_Mode(puMode: SYSUINT);
begin
  DefaultInterface.Set_Mode(puMode);
end;

function TShellShellNameSpace.Get_Flags: LongWord;
begin
    Result := DefaultInterface.Flags;
end;

procedure TShellShellNameSpace.Set_Flags(pdwFlags: LongWord);
begin
  DefaultInterface.Set_Flags(pdwFlags);
end;

procedure TShellShellNameSpace.Set_TVFlags(dwFlags: LongWord);
begin
  DefaultInterface.Set_TVFlags(dwFlags);
end;

function TShellShellNameSpace.Get_TVFlags: LongWord;
begin
    Result := DefaultInterface.TVFlags;
end;

function TShellShellNameSpace.Get_Columns: WideString;
begin
    Result := DefaultInterface.Columns;
end;

procedure TShellShellNameSpace.Set_Columns(const bstrColumns: WideString);
  { Warning: The property Columns has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Columns := bstrColumns;
end;

function TShellShellNameSpace.Get_CountViewTypes: SYSINT;
begin
    Result := DefaultInterface.CountViewTypes;
end;

procedure TShellShellNameSpace.MoveSelectionUp;
begin
  DefaultInterface.MoveSelectionUp;
end;

procedure TShellShellNameSpace.MoveSelectionDown;
begin
  DefaultInterface.MoveSelectionDown;
end;

procedure TShellShellNameSpace.ResetSort;
begin
  DefaultInterface.ResetSort;
end;

procedure TShellShellNameSpace.NewFolder;
begin
  DefaultInterface.NewFolder;
end;

procedure TShellShellNameSpace.Synchronize;
begin
  DefaultInterface.Synchronize;
end;

procedure TShellShellNameSpace.Import;
begin
  DefaultInterface.Import;
end;

procedure TShellShellNameSpace.Export;
begin
  DefaultInterface.Export;
end;

procedure TShellShellNameSpace.InvokeContextMenuCommand(const strCommand: WideString);
begin
  DefaultInterface.InvokeContextMenuCommand(strCommand);
end;

procedure TShellShellNameSpace.MoveSelectionTo;
begin
  DefaultInterface.MoveSelectionTo;
end;

function TShellShellNameSpace.CreateSubscriptionForSelection: WordBool;
begin
  Result := DefaultInterface.CreateSubscriptionForSelection;
end;

function TShellShellNameSpace.DeleteSubscriptionForSelection: WordBool;
begin
  Result := DefaultInterface.DeleteSubscriptionForSelection;
end;

procedure TShellShellNameSpace.SetRoot(const bstrFullPath: WideString);
begin
  DefaultInterface.SetRoot(bstrFullPath);
end;

procedure TShellShellNameSpace.SetViewType(iType: SYSINT);
begin
  DefaultInterface.SetViewType(iType);
end;

function TShellShellNameSpace.SelectedItems: IDispatch;
begin
  Result := DefaultInterface.SelectedItems;
end;

procedure TShellShellNameSpace.Expand(var_: OleVariant; iDepth: SYSINT);
begin
  DefaultInterface.Expand(var_, iDepth);
end;

procedure TShellShellNameSpace.UnselectAll;
begin
  DefaultInterface.UnselectAll;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TShellShellNameSpaceProperties.Create(AServer: TShellShellNameSpace);
begin
  inherited Create;
  FServer := AServer;
end;

function TShellShellNameSpaceProperties.GetDefaultInterface: IShellNameSpace;
begin
  Result := FServer.DefaultInterface;
end;

function TShellShellNameSpaceProperties.Get_SubscriptionsEnabled: WordBool;
begin
    Result := DefaultInterface.SubscriptionsEnabled;
end;

function TShellShellNameSpaceProperties.Get_EnumOptions: Integer;
begin
    Result := DefaultInterface.EnumOptions;
end;

procedure TShellShellNameSpaceProperties.Set_EnumOptions(pgrfEnumFlags: Integer);
begin
  DefaultInterface.Set_EnumOptions(pgrfEnumFlags);
end;

function TShellShellNameSpaceProperties.Get_SelectedItem: IDispatch;
begin
    Result := DefaultInterface.SelectedItem;
end;

procedure TShellShellNameSpaceProperties.Set_SelectedItem(const pItem: IDispatch);
begin
  DefaultInterface.Set_SelectedItem(pItem);
end;

function TShellShellNameSpaceProperties.Get_Root: OleVariant;
var
  InterfaceVariant : OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  Result := InterfaceVariant.Root;
end;

procedure TShellShellNameSpaceProperties.Set_Root(pvar: OleVariant);
begin
  DefaultInterface.Set_Root(pvar);
end;

function TShellShellNameSpaceProperties.Get_Depth: SYSINT;
begin
    Result := DefaultInterface.Depth;
end;

procedure TShellShellNameSpaceProperties.Set_Depth(piDepth: SYSINT);
begin
  DefaultInterface.Set_Depth(piDepth);
end;

function TShellShellNameSpaceProperties.Get_Mode: SYSUINT;
begin
    Result := DefaultInterface.Mode;
end;

procedure TShellShellNameSpaceProperties.Set_Mode(puMode: SYSUINT);
begin
  DefaultInterface.Set_Mode(puMode);
end;

function TShellShellNameSpaceProperties.Get_Flags: LongWord;
begin
    Result := DefaultInterface.Flags;
end;

procedure TShellShellNameSpaceProperties.Set_Flags(pdwFlags: LongWord);
begin
  DefaultInterface.Set_Flags(pdwFlags);
end;

procedure TShellShellNameSpaceProperties.Set_TVFlags(dwFlags: LongWord);
begin
  DefaultInterface.Set_TVFlags(dwFlags);
end;

function TShellShellNameSpaceProperties.Get_TVFlags: LongWord;
begin
    Result := DefaultInterface.TVFlags;
end;

function TShellShellNameSpaceProperties.Get_Columns: WideString;
begin
    Result := DefaultInterface.Columns;
end;

procedure TShellShellNameSpaceProperties.Set_Columns(const bstrColumns: WideString);
  { Warning: The property Columns has a setter and a getter whose
    types do not match. Delphi was unable to generate a property of
    this sort and so is using a Variant as a passthrough. }
var
  InterfaceVariant: OleVariant;
begin
  InterfaceVariant := DefaultInterface;
  InterfaceVariant.Columns := bstrColumns;
end;

function TShellShellNameSpaceProperties.Get_CountViewTypes: SYSINT;
begin
    Result := DefaultInterface.CountViewTypes;
end;

{$ENDIF}

class function CoCScriptErrorList.Create: IScriptErrorList;
begin
  Result := CreateComObject(CLASS_CScriptErrorList) as IScriptErrorList;
end;

class function CoCScriptErrorList.CreateRemote(const MachineName: string): IScriptErrorList;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CScriptErrorList) as IScriptErrorList;
end;

end.
