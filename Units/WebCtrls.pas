unit WebCtrls;

interface

uses
	Windows, Messages, ActiveX, SysUtils, Classes, Controls, Forms, Graphics, GraphTools,
  MSHTML, ShellDocView, ComObj, Dialogs;

function ColorToHtml(Color: TColor): string;

type
  TDocHostUIInfo = record
    cbSize: ULONG;          // size of structure in bytes
    dwFlags: DWORD;         // flags that specify UI capabilities
    dwDoubleClick: DWORD;   // specified response to double click
    pchHostCss: PWChar;     // pointer to CSS rules
    pchHostNS: PWChar;      // pointer to namespace list for custom tags
  end;

	IDocHostUIHandler = interface(IUnknown)
    ['{bd3f23c0-d43e-11cf-893b-00aa00bdce1a}']
    { Display a shortcut menu }
    function ShowContextMenu(
      const dwID: DWORD;
      const ppt: PPOINT;
      const pcmdtReserved: IUnknown;
      const pdispReserved: IDispatch): HResult; stdcall;
    { Retrieves the user interface capabilities and requirements
      of the application that is hosting the web browser }
    function GetHostInfo(
      var pInfo: TDocHostUIInfo): HResult; stdcall;
    { Enables us to replace browser menus and toolbars etc }
    function ShowUI(
      const dwID: DWORD;
      const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget;
      const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HResult; stdcall;
    { Called when the browser removes its menus and toolbars.
      We remove menus and toolbars we displayed in ShowUI }
    function HideUI: HResult; stdcall;
    { Notifies that the command state has changed so the host
      can update toolbar buttons etc. }
    function UpdateUI: HResult; stdcall;
    { Called when a modal UI is displayed }
    function EnableModeless(
      const fEnable: BOOL): HResult; stdcall;
    { Called when the document window is activated or
      deactivated }
    function OnDocWindowActivate(
      const fActivate: BOOL): HResult; stdcall;
    { Called when the top-level frame window is activated or
      deactivated }
    function OnFrameWindowActivate(
      const fActivate: BOOL): HResult; stdcall;
    { Called when a frame or document's window's border is
      about to be changed }
    function ResizeBorder(
      const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow;
      const fFrameWindow: BOOL): HResult; stdcall;
    { Called when accelerator keys such as TAB are used }
    function TranslateAccelerator(
      const lpMsg: PMSG;
      const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HResult; stdcall;
    { Called by the web browser control to retrieve a registry
      subkey path that overrides the default IE registry settings }
    function GetOptionKeyPath(
      var pchKey: POLESTR;
      const dw: DWORD ): HResult; stdcall;
    { Called when the browser is used as a drop target and enables
      the host to supply an alternative IDropTarget interface }
    function GetDropTarget(
      const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HResult; stdcall;
    { Called to obtain our IDispatch interface. Used to enable the
      browser to call methods in the host (e.g. from JavaScript) }
    function GetExternal(
    	out ppDispatch: IDispatch): HResult; stdcall;
    { Gives the host an opportunity to modify the URL to be loaded }
    function TranslateUrl(
      const dwTranslate: DWORD;
      const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HResult; stdcall;
    { Allows us to replace the web browser data object. It enables
      us to block certain clipboard formats or support additional
      clipboard formats }
    function FilterDataObject(
      const pDO: IDataObject;
      out ppDORet: IDataObject): HResult; stdcall;
  end;

  TDocCommands = class(TObject)
  private
    FBrowser: TWebBrowserEx;
  protected
    procedure SafeInit(ForceLoad: Boolean = False);
  public
    function CmdStatus(Cmd: Longint): Integer;
    procedure CmdExec(Cmd: Longint; Prompt: Boolean; const A: OleVariant; var B: OleVariant); overload;
    procedure CmdExec(Cmd: Longint; Prompt: Boolean; const A: OleVariant); overload;
    procedure CmdExec(Cmd: Longint; Prompt: Boolean); overload;
  public
    constructor Create(Browser: TWebBrowserEx);
    procedure EditMode;
    { file commands }
    procedure New;
    procedure Open(const FileName: string = '');
    procedure Save;
    procedure SaveAs(const FileName: string = '');
    procedure Print(Silent: Boolean = False);
    procedure PrintPreview;
    procedure Properties;
    { edit commands }
    // IDM_OVERWRITE
    procedure Undo;
    procedure Redo;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure Delete;
    procedure SelectAll;
    procedure Find;
    procedure Replace;
    { formatting commands }
    procedure Bold;
    procedure Italic;
    procedure Underline;
    procedure StrikeThrough;
    procedure Indent;
    procedure Outdent;
    procedure OrderedList;
    procedure UnorderedList;
    (*procedure AlignLeft;
    procedure AlignCenter;
    procedure AlignRight;
    procedure JustifyFull;
    procedure JustifyLeft;
    procedure JustifyCenter;
    procedure JustifyRight;
    procedure JustifyGeneral;
    procedure BackColor(const Color: string);
    procedure ForeColor(const Color: string);
    { html commands }
    procedure Font;
    procedure FontName(const Name: string);
    procedure FontSize(Size: Integer);
    procedure InsertImage(const FileName: string = '');
    procedure InsertLink(const Url: string = '');
    procedure Unlink;
    procedure InsertCode;
    procedure InsertDiv;*)
    { properties }
    {property Editing: Boolean read GetEditing write SetEditing;
    property CanCopy: Boolean read GetCanCopy;
    property CanCut: Boolean read GetCanCut;
    property CanPaste: Boolean read GetCanPaste;
    property CanDelete: Boolean read GetCanDelete;}
  end;

	TDocOptions = set of (doBorder, doContextMenu, doScrollbars, doSelection);

  TDocBrowser = class(TWebBrowserEx, IOleInPlaceSite, IDocHostUIHandler)
	private
    FCommands: TDocCommands;
    FExtern: IDispatch;
		FOptions: TDocOptions;
    procedure WaitUntilReady;
  	function GetSource: string;
  	procedure SetSource(const Value: string);
  	function GetEditing: Boolean;
  	procedure SetEditing(Value: Boolean);
    //procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
	protected
    procedure CreateWnd; override;
  	{ TDocBrowser.IOleInPlaceSite }
    function GetWindow(out wnd: HWnd): HResult; stdcall;
    function CanInPlaceActivate: HResult; stdcall;
    function OnInPlaceActivate: HResult; stdcall;
    function OnUIActivate: HResult; stdcall;
    function GetWindowContext(out frame: IOleInPlaceFrame;
      out doc: IOleInPlaceUIWindow; out rcPosRect: TRect;
      out rcClipRect: TRect; out frameInfo: TOleInPlaceFrameInfo): HResult;
      stdcall;
    function Scroll(scrollExtent: TPoint): HResult; stdcall;
    function OnUIDeactivate(fUndoable: BOOL): HResult; stdcall;
    function OnInPlaceDeactivate: HResult; stdcall;
    function DiscardUndoState: HResult; stdcall;
    function DeactivateAndUndo: HResult; stdcall;
    function OnPosRectChange(const rcPosRect: TRect): HResult; stdcall;
  	{ TDocBrowser.IDocHostUIHandler }
    function ShowContextMenu(
      const dwID: DWORD;
      const ppt: PPOINT;
      const pcmdtReserved: IUnknown;
      const pdispReserved: IDispatch): HResult; stdcall;
    function GetHostInfo(
      var pInfo: TDocHostUIInfo): HResult; stdcall;
    function ShowUI(
      const dwID: DWORD;
      const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget;
      const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HResult; stdcall;
    function HideUI: HResult; stdcall;
    function UpdateUI: HResult; stdcall;
    function EnableModeless(
      const fEnable: BOOL): HResult; stdcall;
    function OnDocWindowActivate(
      const fActivate: BOOL): HResult; stdcall;
    function OnFrameWindowActivate(
      const fActivate: BOOL): HResult; stdcall;
    function ResizeBorder(
      const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow;
      const fFrameWindow: BOOL): HResult; stdcall;
    function TranslateAccelerator(
      const lpMsg: PMSG;
      const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HResult; stdcall;
    function GetOptionKeyPath(
      var pchKey: POLESTR;
      const dw: DWORD ): HResult; stdcall;
    function GetDropTarget(
      const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HResult; stdcall;
    function GetExternal(
    	out ppDispatch: IDispatch): HResult; stdcall;
    function TranslateUrl(
      const dwTranslate: DWORD;
      const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HResult; stdcall;
    function FilterDataObject(
      const pDO: IDataObject;
      out ppDORet: IDataObject): HResult; stdcall;
	public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Commands: TDocCommands read FCommands;
    property Extern: IDispatch read FExtern write FExtern;
    property Source: string read GetSource write SetSource;
    property Editing: Boolean read GetEditing write SetEditing;
	published
  	property Options: TDocOptions read FOptions write FOptions;
  end;

{ TArgList and ArgParam refer to parameters passed to script objects }

	TArgList = record
    Arguments: PVariantArgList;
  	Count: Integer;
	end;

function ArgParam(Index: Integer; const Args: TArgList): OleVariant;

{ TAutoDispatch }

type
	TAutoDispatch = class(TInterfacedObject, IDispatch)
  private
  	FMethods: TStrings;
    FProperties: TStrings;
  protected
  	function OnMethod(MethodIndex: Integer; const Args: TArgList): OleVariant; virtual;
    function OnGetProperty(PropIndex: Integer; const Args: TArgList): OleVariant; virtual;
    procedure OnSetProperty(PropIndex: Integer; const Args: TArgList; const Value: OleVariant); virtual;
		property Methods: TStrings read FMethods;
    property Properties: TStrings read FProperties;
  protected
  	{ IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
	public
  	constructor Create; virtual;
		destructor Destroy; override;
	end;

  TAutoDispatchClass = class of TAutoDispatch;

const
  CGID_MSHTML: TGUID = '{DE4BA900-59CA-11CF-9592-444553540000}';

  DOCHOSTUIDBLCLK_DEFAULT = 0;
  DOCHOSTUIDBLCLK_SHOWCODE = 2;
  DOCHOSTUIDBLCLK_SHOWPROPERTIES = 1;
  DOCHOSTUIFLAG_ACTIVATE_CLIENTHIT_ONLY = $0200;
  DOCHOSTUIFLAG_CODEPAGELINKEDFONTS = $0800;
  DOCHOSTUIFLAG_DIALOG = $0001;
  DOCHOSTUIFLAG_DISABLE_HELP_MENU = $0002;
  DOCHOSTUIFLAG_DISABLE_OFFSCREEN = $0040;
  DOCHOSTUIFLAG_DISABLE_SCRIPT_INACTIVE = $0010;
  DOCHOSTUIFLAG_DIV_BLOCKDEFAULT = $0100;
  DOCHOSTUIFLAG_ENABLE_FORMS_AUTOCOMPLETE = $4000;
  DOCHOSTUIFLAG_ENABLE_INPLACE_NAVIGATION = $10000;
  DOCHOSTUIFLAG_FLAT_SCROLLBAR = $0080;
  DOCHOSTUIFLAG_IME_ENABLE_RECONVERSION = $20000;
  DOCHOSTUIFLAG_NO3DBORDER = $0004;
  DOCHOSTUIFLAG_OPENNEWWIN = $0020;
  DOCHOSTUIFLAG_OVERRIDEBEHAVIORFACTORY = $0400;
  DOCHOSTUIFLAG_SCROLL_NO = $0008;
  DOCHOSTUIFLAG_URL_ENCODING_DISABLE_UTF8 = $1000;
  DOCHOSTUIFLAG_URL_ENCODING_ENABLE_UTF8 = $2000;
  DOCHOSTUITYPE_AUTHOR = 1;
  DOCHOSTUITYPE_BROWSE = 0;
  DOCHOSTUIFLAG_THEME = $00040000;
  DOCHOSTUIFLAG_NOTHEME = $00080000;

  CONTEXT_MENU_ANCHOR = 5;
  CONTEXT_MENU_CONTROL = 2;
  CONTEXT_MENU_DEBUG = 9;
  CONTEXT_MENU_DEFAULT = 0;
  CONTEXT_MENU_HSCROLL = 11;
  CONTEXT_MENU_IMAGE = 1;
  CONTEXT_MENU_IMGART = 8;
  CONTEXT_MENU_IMGDYNSRC = 7;
  CONTEXT_MENU_TABLE = 3;
  CONTEXT_MENU_TEXTSELECT = 4;
  CONTEXT_MENU_UNKNOWN = 6;
  CONTEXT_MENU_VSCROLL = 10;

  DISPID_AMBIENT_DLCONTROL = (-5512);
  DISPID_AMBIENT_USERAGENT = (-5513);

  ID_EDITMODE = 32801;
  ID_IE_CONTEXTMENU_ADDFAV = 2261;
  ID_IE_CONTEXTMENU_NEWWINDOW = 2137;
  ID_IE_CONTEXTMENU_REFRESH = 6042;
  ID_IE_FILE_ADDLOCAL = 377;
  ID_IE_FILE_ADDTRUST = 376;
  ID_IE_FILE_IMPORTEXPORT = 374;
  ID_IE_FILE_NEWCALL = 395;
  ID_IE_FILE_NEWMAIL = 279;
  ID_IE_FILE_NEWPEOPLE = 390;
  ID_IE_FILE_NEWPUBLISHINFO = 387;
  ID_IE_FILE_NEWWINDOW = 275;
  ID_IE_FILE_PAGESETUP = 259;
  ID_IE_FILE_PRINT = 260;
  ID_IE_FILE_PRINTPREVIEW = 277;
  ID_IE_FILE_SENDDESKTOPSHORTCUT = 284;
  ID_IE_FILE_SENDLINK = 283;
  ID_IE_FILE_SENDPAGE = 282;
  ID_IE_HELP_BESTPAGE = 346;
  ID_IE_HELP_ENHANCEDSECURITY = 375;
  ID_IE_HELP_FAQ = 343;
  ID_IE_HELP_FEEDBACK = 345;
  ID_IE_HELP_FREESTUFF = 341;
  ID_IE_HELP_HELPINDEX = 337;
  ID_IE_HELP_MSHOME = 348;
  ID_IE_HELP_NETSCAPEUSER = 351;
  ID_IE_HELP_ONLINESUPPORT = 344;
  ID_IE_HELP_PRODUCTUPDATE = 342;
  ID_IE_HELP_SEARCHWEB = 347;
  ID_IE_HELP_STARTPAGE = 350;
  ID_IE_HELP_VERSIONINFO = 336;
  ID_IE_HELP_VISITINTERNET = 349;
  ID_IE_HELP_WEBTUTORIAL = 338;

  IDM_1D = 2170;
  IDM_ADDFAVORITES = 2261;
  IDM_ADDRESS = 2189;
  IDM_ADDTOFAVOURITES = 2261;
  IDM_ALIGNBOTTOM = 1;
  IDM_ALIGNHORIZONTALCENTERS = 2;
  IDM_ALIGNLEFT = 3;
  IDM_ALIGNRIGHT = 4;
  IDM_ALIGNTOGRID = 5;
  IDM_ALIGNTOP = 6;
  IDM_ALIGNVERTICALCENTERS = 7;
  IDM_APPLYHEADING1 = 2255;
  IDM_APPLYHEADING2 = 2256;
  IDM_APPLYHEADING3 = 2257;
  IDM_APPLYNORMAL = 2254;
  IDM_ARRANGEBOTTOM = 8;
  IDM_ARRANGERIGHT = 9;
  IDM_AUTODETECT = 2329;
  IDM_BACK = 2282;
  IDM_BACKCOLOR = 51;
  IDM_BASELINEFONT1 = 2141;
  IDM_BASELINEFONT3 = 2143;
  IDM_BASELINEFONT4 = 2144;
  IDM_BASELINEFONT5 = 2145;
  IDM_BLINK = 2190;
  IDM_BLOCKFMT = 2234;
  IDM_BOLD = 52;
  IDM_BOOKMARK = 2123;
  IDM_BORDERCOLOR = 53;
  IDM_BREAKATNEXT = 2311;
  IDM_BRINGFORWARD = 10;
  IDM_BRINGTOFRONT = 11;
  IDM_BROWSEMODE = 2126;
  IDM_BUTTON = 2167;
  IDM_CANCEL = 89;
  IDM_CAPTIONINSERT = 2203;
  IDM_CELLINSERT = 2202;
  IDM_CELLMERGE = 2204;
  IDM_CELLPROPERTIES = 2211;
  IDM_CELLSELECT = 2206;
  IDM_CELLSPLIT = 2205;
  IDM_CENTERALIGNPARA = 2250;
  IDM_CENTERHORIZONTALLY = 12;
  IDM_CENTERVERTICALLY = 13;
  IDM_CHANGECASE = 2246;
  IDM_CHANGEFONT = 2240;
  IDM_CHANGEFONTSIZE = 2241;
  IDM_CHECKBOX = 2163;
  IDM_CHISELED = 64;
  IDM_CLEARSELECTION = 2007;
  IDM_CODE = 14;
  IDM_COLUMNINSERT = 2213;
  IDM_COLUMNSELECT = 2208;
  IDM_COMMENT = 2173;
  IDM_COMPOSESETTINGS = 2318;
  IDM_CONTEXTMENU = 2280;
  IDM_CONVERTOBJECT = 82;
  IDM_COPY = 15;
  IDM_COPYBACKGROUND = 2265;
  IDM_COPYCONTENT = 2291;
  IDM_COPYFORMAT = 2237;
  IDM_COPYSHORTCUT = 2262;
  IDM_CREATELINK = 2290;
  IDM_CREATESHORTCUT = 2266;
  IDM_CUSTOMCONTROL = 83;
  IDM_CUSTOMIZEITEM = 84;
  IDM_CUT = 16;
  IDM_DECFONTSIZE = 2243;
  IDM_DECFONTSIZE1PT = 2245;
  IDM_DELETE = 17;
  IDM_DELETEWORD = 92;
  IDM_DIV = 2191;
  IDM_DOCPROPERTIES = 2260;
  IDM_DROPDOWNBOX = 2165;
  IDM_DYNSRCPLAY = 2271;
  IDM_DYNSRCSTOP = 2272;
  IDM_EDITMODE = 2127;
  IDM_EDITSOURCE = 2122;
  IDM_ENABLE_INTERACTION = 2302;
  IDM_ENCODING = 2292;
  IDM_ETCHED = 65;
  IDM_FILE = 2172;
  IDM_FIND = 67;
  IDM_FLAT = 54;
  IDM_FOLLOW_ANCHOR = 2008;
  IDM_FOLLOWLINKC = 2136;
  IDM_FOLLOWLINKN = 2137;
  IDM_FONT = 90;
  IDM_FONTNAME = 18;
  IDM_FONTSIZE = 19;
  IDM_FORECOLOR = 55;
  IDM_FORM = 2181;
  IDM_FORMATMARK = 2132;
  IDM_FORWARD = 2283;
  IDM_GETBLOCKFMTS = 2233;
  IDM_GETBYTESDOWNLOADED = 2331;
  IDM_GETZOOM = 68;
  IDM_GOBACKWARD = 2282;
  IDM_GOFORWARD = 2283;
  IDM_GOTO = 2239;
  IDM_GROUP = 20;
  IDM_HELP_ABOUT = 2221;
  IDM_HELP_CONTENT = 2220;
  IDM_HELP_README = 2222;
  IDM_HORIZONTALLINE = 2150;
  IDM_HORIZSPACECONCATENATE = 21;
  IDM_HORIZSPACEDECREASE = 22;
  IDM_HORIZSPACEINCREASE = 23;
  IDM_HORIZSPACEMAKEEQUAL = 24;
  IDM_HTMLCONTAIN = 2159;
  IDM_HTMLEDITMODE = 2316;
  IDM_HTMLSOURCE = 2157;
  IDM_HYPERLINK = 2124;
  IDM_IFRAME = 2158;
  IDM_IMAGE = 2168;
  IDM_IMAGEMAP = 2171;
  IDM_IMGARTPLAY = 2274;
  IDM_IMGARTREWIND = 2276;
  IDM_IMGARTSTOP = 2275;
  IDM_IMPORT = 86;
  IDM_INCFONTSIZE = 2242;
  IDM_INCFONTSIZE1PT = 2244;
  IDM_INDENT = 2186;
  IDM_INSERTOBJECT = 25;
  IDM_INSFIELDSET = 2119;
  IDM_INSINPUTBUTTON = 2115;
  IDM_INSINPUTHIDDEN = 2312;
  IDM_INSINPUTIMAGE = 2114;
  IDM_INSINPUTPASSWORD = 2313;
  IDM_INSINPUTRESET = 2116;
  IDM_INSINPUTSUBMIT = 2117;
  IDM_INSINPUTUPLOAD = 2118;
  IDM_ITALIC = 56;
  IDM_JAVAAPPLET = 2175;
  IDM_JUSTIFYCENTER = 57;
  IDM_JUSTIFYFULL = 50;
  IDM_JUSTIFYGENERAL = 58;
  IDM_JUSTIFYLEFT = 59;
  IDM_JUSTIFYRIGHT = 60;
  IDM_LANGUAGE = 2292;
  IDM_LAUNCHDEBUGGER = 2310;
  IDM_LEFTALIGNPARA = 2251;
  IDM_LINEBREAKBOTH = 2154;
  IDM_LINEBREAKLEFT = 2152;
  IDM_LINEBREAKNORMAL = 2151;
  IDM_LINEBREAKRIGHT = 2153;
  IDM_LIST = 2183;
  IDM_LISTBOX = 2166;
  IDM_MARQUEE = 2182;
  IDM_MENUEXT_COUNT = 3733;
  IDM_MENUEXT_FIRST= 3700;
  IDM_MENUEXT_LAST= 3732;
  IDM_MIMECSET_FIRST= 3609;
  IDM_MIMECSET_LAST= 3640;
  IDM_MOVE = 88;
  IDM_MULTILEVELREDO = 30;
  IDM_MULTILEVELUNDO = 44;
  IDM_NEW = 2001;
  IDM_NEWPAGE = 87;
  IDM_NOACTIVATEDESIGNTIMECONTROLS = 2333;
  IDM_NOACTIVATEJAVAAPPLETS = 2334;
  IDM_NOACTIVATENORMALOLECONTROLS = 2332;
  IDM_NONBREAK = 2155;
  IDM_OBJECT = 2169;
  IDM_OBJECTVERBLIST0 = 72;
  IDM_OBJECTVERBLIST1 = 73;
  IDM_OBJECTVERBLIST2 = 74;
  IDM_OBJECTVERBLIST3 = 75;
  IDM_OBJECTVERBLIST4 = 76;
  IDM_OBJECTVERBLIST5 = 77;
  IDM_OBJECTVERBLIST6 = 78;
  IDM_OBJECTVERBLIST7 = 79;
  IDM_OBJECTVERBLIST8 = 80;
  IDM_OBJECTVERBLIST9 = 81;
  IDM_OPEN = 2000;
  IDM_OPENINNEWWINDOW = 2137;
  IDM_OPENLINK = 2136;
  IDM_OPTIONS = 2135;
  IDM_ORDERLIST = 2184;
  IDM_OUTDENT = 2187;
  IDM_OVERWRITE = 2314;
  IDM_PAGE = 2267;
  IDM_PAGEBREAK = 2177;
  IDM_PAGEINFO = 2231;
  IDM_PAGESETUP = 2004;
  IDM_PARAGRAPH = 2180;
  IDM_PARSECOMPLETE = 2315;
  IDM_PASTE = 26;
  IDM_PASTEFORMAT = 2238;
  IDM_PASTEINSERT = 2120;
  IDM_PASTESPECIAL = 2006;
  IDM_PERSISTSTREAMSYNC = 2341;
  IDM_PLUGIN = 2176;
  IDM_PREFORMATTED = 2188;
  IDM_PRESTOP = 2284;
  IDM_PRINT = 27;
  IDM_PRINTPREVIEW = 2003;
  IDM_PRINTQUERYJOBSPENDING = 2277;
  IDM_PRINTTARGET = 2273;
  IDM_PROPERTIES = 28;
  IDM_RADIOBUTTON = 2164;
  IDM_RAISED = 61;
  IDM_RCINSERT = 2201;
  IDM_REDO = 29;
  IDM_REFRESH = 2300;
  IDM_REGISTRYREFRESH = 2317;
  IDM_REMOVEFORMAT = 2230;
  IDM_REMOVEPARAFORMAT = 2253;
  IDM_RENAME = 85;
  IDM_REPLACE = 2121;
  IDM_RIGHTALIGNPARA = 2252;
  IDM_ROWINSERT = 2212;
  IDM_ROWSELECT = 2207;
  IDM_SAVE = 70;
  IDM_SAVEAS = 71;
  IDM_SAVEBACKGROUND = 2263;
  IDM_SAVECOPYAS = 2002;
  IDM_SAVEPICTURE = 2270;
  IDM_SAVETARGET = 2268;
  IDM_SCRIPT = 2174;
  IDM_SCRIPTDEBUGGER = 2330;
  IDM_SELECTALL = 31;
  IDM_SENDBACKWARD = 32;
  IDM_SENDTOBACK = 33;
  IDM_SETASBACKGROUND = 2264;
  IDM_SETASDESKTOPITEM = 2278;
  IDM_SETDIRTY = 2342;
  IDM_SETWALLPAPER = 2264;
  IDM_SHADOWED = 66;
  IDM_SHOWALIGNEDSITETAGS = 2321;
  IDM_SHOWALLTAGS = 2320;
  IDM_SHOWAREATAGS = 2325;
  IDM_SHOWCOMMENTTAGS = 2324;
  IDM_SHOWGRID = 69;
  IDM_SHOWHIDE_CODE = 2235;
  IDM_SHOWMISCTAGS = 2327;
  IDM_SHOWPICTURE = 2269;
  IDM_SHOWSCRIPTTAGS = 2322;
  IDM_SHOWSPECIALCHAR = 2249;
  IDM_SHOWSTYLETAGS = 2323;
  IDM_SHOWTABLE = 34;
  IDM_SHOWUNKNOWNTAGS = 2326;
  IDM_SHOWWBRTAGS = 2340;
  IDM_SHOWZEROBORDERATDESIGNTIME = 2328;
  IDM_SIZETOCONTROL = 35;
  IDM_SIZETOCONTROLHEIGHT = 36;
  IDM_SIZETOCONTROLWIDTH = 37;
  IDM_SIZETOFIT = 38;
  IDM_SIZETOGRID = 39;
  IDM_SNAPTOGRID = 40;
  IDM_SPECIALCHAR = 2156;
  IDM_SPELL = 2005;
  IDM_STATUSBAR = 2131;
  IDM_STOP = 2138;
  IDM_STOPDOWNLOAD = 2301;
  IDM_STRIKETHROUGH = 91;
  IDM_SUBSCRIPT = 2247;
  IDM_SUNKEN = 62;
  IDM_SUPERSCRIPT = 2248;
  IDM_TABLE = 2236;
  IDM_TABLEINSERT = 2200;
  IDM_TABLEPROPERTIES = 2210;
  IDM_TABLESELECT = 2209;
  IDM_TABORDER = 41;
  IDM_TELETYPE = 2232;
  IDM_TEXTAREA = 2162;
  IDM_TEXTBOX = 2161;
  IDM_TEXTONLY = 2133;
  IDM_TOOLBARS = 2130;
  IDM_TOOLBOX = 42;
  IDM_UNBOOKMARK = 2128;
  IDM_UNDERLINE = 63;
  IDM_UNDO = 43;
  IDM_UNGROUP = 45;
  IDM_UNKNOWN = 0;
  IDM_UNLINK = 2125;
  IDM_UNORDERLIST = 2185;
  IDM_VERTSPACECONCATENATE = 46;
  IDM_VERTSPACEDECREASE = 47;
  IDM_VERTSPACEINCREASE = 48;
  IDM_VERTSPACEMAKEEQUAL = 49;
  IDM_VIEWSOURCE = 2139;
  IDM_ZOOMPERCENT = 50;
  IDM_ZOOMPOPUP = 2140;

  IDM_NOFIXUPURLSONPASTE = 2335;
  IDM_EMPTYGLYPHTABLE = 2336;
  IDM_ADDTOGLYPHTABLE = 2337;
  IDM_REMOVEFROMGLYPHTABLE = 2338;
  IDM_REPLACEGLYPHCONTENTS = 2339;
  IDM_RUNURLSCRIPT = 2343;
  IDM_ZOOMRATIO = 2344;
  IDM_GETZOOMNUMERATOR = 2345;
  IDM_GETZOOMDENOMINATOR = 2346;
  { Commands for complex text }
  IDM_DIRLTR = 2350;                  
  IDM_DIRRTL = 2351;                  
  IDM_BLOCKDIRLTR = 2352;             
  IDM_BLOCKDIRRTL = 2353;
  IDM_INLINEDIRLTR = 2354;
  IDM_INLINEDIRRTL = 2355;
  IDM_ISTRUSTEDDLG = 2356;
  IDM_INSERTSPAN = 2357;
  IDM_LOCALIZEEDITOR = 2358;
  { XML mime viewer }
  IDM_SAVEPRETRANSFORMSOURCE = 2370;
  IDM_VIEWPRETRANSFORMSOURCE = 2371;
  { Scrollbar context menu }
  IDM_SCROLL_HERE = 2380;
  IDM_SCROLL_TOP = 2381;
  IDM_SCROLL_BOTTOM = 2382;
  IDM_SCROLL_PAGEUP = 2383;
  IDM_SCROLL_PAGEDOWN = 2384;
  IDM_SCROLL_UP = 2385;
  IDM_SCROLL_DOWN = 2386;
  IDM_SCROLL_LEFTEDGE = 2387;
  IDM_SCROLL_RIGHTEDGE = 2388;
  IDM_SCROLL_PAGELEFT = 2389;
  IDM_SCROLL_PAGERIGHT = 2390;
  IDM_SCROLL_LEFT = 2391;
  IDM_SCROLL_RIGHT = 2392;
  { IE 6 form editing commands }
  IDM_MULTIPLESELECTION = 2393;
  IDM_2D_POSITION = 2394;
  IDM_2D_ELEMENT = 2395;
  IDM_1D_ELEMENT = 2396;
  IDM_ABSOLUTE_POSITION = 2397;
  IDM_LIVERESIZE = 2398;
  IDM_ATOMICSELECTION = 2399;
  { Auto URL detection mode }
  IDM_AUTOURLDETECT_MODE = 2400;
  { Legacy IE50 compatible paste }
  IDM_IE50_PASTE = 2401;
  { IE50 paste mode }
  IDM_IE50_PASTE_MODE = 2402;
  IDM_GETIPRINT = 2403;
  { Disabling selection handles }
  IDM_DISABLE_EDITFOCUS_UI = 2404;
  { Visibility/display in design }
  IDM_RESPECTVISIBILITY_INDESIGN = 2405;
  { Set CSS mode }
  IDM_CSSEDITING_LEVEL = 2406;
  { New outdent }
  IDM_UI_OUTDENT = 2407;
  { Printing status }
  IDM_UPDATEPAGESTATUS = 2408;
  { IME reconversion }
  IDM_IME_ENABLE_RECONVERSION = 2409;
  IDM_KEEPSELECTION = 2410;
  IDM_UNLOADDOCUMENT = 2411;
  IDM_OVERRIDE_CURSOR = 2420;
  IDM_PEERHITTESTSAMEINEDIT = 2423;
  IDM_TRUSTAPPCACHE = 2425;
  IDM_BACKGROUNDIMAGECACHE = 2430;
  IDM_GETUSERACTIONTIME = 2431;
  IDM_BEGINUSERACTION = 2432;
  IDM_ENDUSERACTION = 2433;
  IDM_SETCUSTOMCURSOR = 2434;
  { Open Link in new tab }
  IDM_FOLLOWLINKT = 2435;
  IDM_DEFAULTBLOCK = 6046;

  WM_USER_STARTWALKING = WM_USER + 1;
  WM_XBUTTONDBLCLK = $020D;
  WM_XBUTTONDOWN = $020B;
  WM_XBUTTONUP = $020C;

implementation

function TaskAllocWideString(const S: string): PWChar;
var
  I: Integer;
begin
  I := Length(S) + 1;
  Result := CoTaskMemAlloc(I * SizeOf(WideChar));
  if Assigned(Result) then
    StringToWideChar(S, Result, I);
end;

function ColorToHtml(Color: TColor): string;
var
  C: array[0..3] of Byte absolute Color;
begin
  Color := ColorToRGB(Color);
  Result := Format('#%.2x%.2x%.2x', [C[0], C[1], C[2]]);
end;

{ TDocCommands }

function EmptyVar: OleVariant;
begin
  VarClear(Result);
end;

constructor TDocCommands.Create(Browser: TWebBrowserEx);
begin
  inherited Create;
  FBrowser := Browser;
end;

procedure TDocCommands.SafeInit(ForceLoad: Boolean = False);
begin
	if ForceLoad or (FBrowser.Document = nil) then
  	FBrowser.Navigate('about:blank');
  while FBrowser.ReadyState <> READYSTATE_COMPLETE do
  begin
    Application.ProcessMessages;
    Sleep(0);
  end;
end;

function TDocCommands.CmdStatus(Cmd: Longint): Integer;
var
  Command: IOleCommandTarget;
  Group: TGUID;
  OleCmd: TOleCmd;
begin
  Result := -1;
  SafeInit;
	if Supports(FBrowser.Document, IOleCommandTarget, Command) then
  begin
    Group := CGID_MSHTML;
    OleCmd.cmdID := Cmd;
    OleCmd.cmdf := 0;
    if Command.QueryStatus(@Group, 1, @OleCmd, nil) = S_OK then
      Result := OleCmd.cmdf;
  end;
end;

procedure TDocCommands.CmdExec(Cmd: Longint; Prompt: Boolean; const A: OleVariant; var B: OleVariant);
const
  Prompts: array[Boolean] of TOleEnum = (OLECMDEXECOPT_DONTPROMPTUSER, // OLECMDEXECOPT_DODEFAULT
    OLECMDEXECOPT_PROMPTUSER);
var
  Command: IOleCommandTarget;
  Group: TGUID;
begin
  SafeInit;
	if Supports(FBrowser.Document, IOleCommandTarget, Command) then
  begin
    Group := CGID_MSHTML;
    Command.Exec(@Group, Cmd, Prompts[Prompt], A, B);
  end;
end;

procedure TDocCommands.CmdExec(Cmd: Longint; Prompt: Boolean; const A: OleVariant);
begin
  CmdExec(Cmd, Prompt, A, POleVariant(nil)^);
end;

procedure TDocCommands.CmdExec(Cmd: Longint; Prompt: Boolean);
begin
  CmdExec(Cmd, Prompt, POleVariant(nil)^, POleVariant(nil)^);
end;

procedure TDocCommands.EditMode;
begin
  CmdExec(IDM_EDITMODE, False, True);
end;

procedure TDocCommands.New;
var
	Persist: IPersistStreamInit;
begin
  SafeInit(True);
	if Supports(FBrowser.Document, IPersistStreamInit, Persist) then
    Persist.InitNew;
end;

procedure TDocCommands.Open(const FileName: string = '');
begin
  CmdExec(IDM_OPEN, True);
end;

procedure TDocCommands.Save;
begin
  CmdExec(IDM_SAVE, True);
end;

procedure TDocCommands.SaveAs(const FileName: string = '');
begin
  CmdExec(IDM_SAVEAS, True);
end;

procedure TDocCommands.Print(Silent: Boolean = False);
begin
  CmdExec(IDM_PRINT, not Silent);
end;

procedure TDocCommands.PrintPreview;
begin
  CmdExec(IDM_PRINTPREVIEW, True);
end;

procedure TDocCommands.Properties;
begin
  CmdExec(IDM_PROPERTIES, True);
end;

{ Edit commands }

procedure TDocCommands.Undo;
begin
  CmdExec(IDM_UNDO, False);
end;

procedure TDocCommands.Redo;
begin
  CmdExec(IDM_REDO, False);
end;

procedure TDocCommands.Copy;
begin
  CmdExec(IDM_COPY, False);
end;

procedure TDocCommands.Cut;
begin
  CmdExec(IDM_CUT, False);
end;

procedure TDocCommands.Paste;
begin
  CmdExec(IDM_PASTE, False);
end;

procedure TDocCommands.Delete;
begin
  CmdExec(IDM_DELETE, False);
end;

procedure TDocCommands.SelectAll;
begin
  CmdExec(IDM_SELECTALL, False);
end;

procedure TDocCommands.Find;
begin
  CmdExec(IDM_FIND, True);
end;

procedure TDocCommands.Replace;
begin
  CmdExec(IDM_REPLACE, False);
end;

{ Formatting commands }

procedure TDocCommands.Bold;
begin
  CmdExec(IDM_BOLD, False);
end;

procedure TDocCommands.Italic;
begin
  CmdExec(IDM_ITALIC, False);
end;

procedure TDocCommands.Underline;
begin
  CmdExec(IDM_UNDERLINE, False);
end;

procedure TDocCommands.StrikeThrough;
begin
  CmdExec(IDM_STRIKETHROUGH, False);
end;

procedure TDocCommands.Indent;
begin
  CmdExec(IDM_INDENT, False);
end;

procedure TDocCommands.Outdent;
begin
  CmdExec(IDM_OUTDENT, False);
end;

procedure TDocCommands.OrderedList;
begin
  CmdExec(IDM_ORDERLIST, False);
end;

procedure TDocCommands.UnorderedList;
begin
  CmdExec(IDM_UNORDERLIST, False);
end;

{function TDocCommands.GetEditing: Boolean;
var
  F: Integer;
begin
  F := CmdStatus(IDM_EDITMODE);
  if F <> -1 then
    Result := F and OLECMDF_LATCHED = OLECMDF_LATCHED;
end;}

{ TDocBrowser }

constructor TDocBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommands := TDocCommands.Create(Self);
  Color := clBtnFace;
end;

destructor TDocBrowser.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

procedure TDocBrowser.WaitUntilReady;
var
  I: Integer;
begin
  I := 0;
  while ReadyState <> READYSTATE_COMPLETE do
  begin
    Inc(I);
    if I = 100 then Exit;
    Forms.Application.ProcessMessages;
    Sleep(0);
  end;
end;

(*procedure TDocBrowser.CmdBold;
begin
  CmdExec(IDM_BOLD, False);
end;

procedure TDocBrowser.CmdFontName(const Name: string);
begin
  CmdExec(IDM_FONTNAME, False, Name);
end;

procedure TDocBrowser.CmdImage(const FileName: string = '');
begin
  CmdExec(IDM_IMAGE, not FileExists(FileName), FileName)
end;

procedure TDocBrowser.CmdOrderedList;
begin
  CmdExec(IDM_ORDERLIST, False);
end;

procedure TDocBrowser.CmdUnorderList;
begin
  CmdExec(IDM_UNORDERLIST, False);
end;*)

function TDocBrowser.GetSource: string;
var
	Persist: IPersistStreamInit;
  StringStream: TStringStream;
  Adapter: IStream;
begin
  FCommands.SafeInit;
	if Supports(Document, IPersistStreamInit, Persist) then
  begin
    WaitUntilReady;
    StringStream := TStringStream.Create('');
    Adapter := TStreamAdapter.Create(StringStream, soOwned);
    if Persist.Save(Adapter, True) = S_OK then
      Result := StringStream.DataString;
  end;
end;

procedure TDocBrowser.SetSource(const Value: string);
var
	Persist: IPersistStreamInit;
  Adapter: IStream;
begin
  FCommands.SafeInit;
	if Supports(Document, IPersistStreamInit, Persist) then
  begin
  	if Persist.InitNew = S_OK then
    begin
    	Adapter := TStreamAdapter.Create(TStringStream.Create(Value), soOwned);
   		Persist.Load(Adapter);
    end;
    WaitUntilReady;
  end;
end;

function TDocBrowser.GetEditing: Boolean;
var
  Doc: IHTMLDocument2;
begin
	if Supports(Document, IHTMLDocument2, Doc) then
    Result := UpperCase(Doc.designMode) = 'ON'
  else
    Result := False;
end;

procedure TDocBrowser.SetEditing(Value: Boolean);
const
  ModeStates: array[Boolean] of string = ('Off', 'On');
var
  Doc: IHTMLDocument2;
  S: string;
begin
  FCommands.SafeInit;
  if Value <> GetEditing then
  begin
    S := Source;
  	if Supports(Document, IHTMLDocument2, Doc) then
      Doc.designMode := ModeStates[Value];
    Source := S;
  end;
end;  

procedure TDocBrowser.CreateWnd;
begin
  inherited CreateWnd;
  // Navigate('about:blank');
end;

{ TDocBrowser.IOleInPlaceSite }

function TDocBrowser.GetWindow(out wnd: HWnd): HResult;
begin
  Result := OleInPlaceSite_GetWindow(wnd);
end;

function TDocBrowser.CanInPlaceActivate: HResult;
begin
  Result := inherited CanInPlaceActivate;
end;

function TDocBrowser.OnInPlaceActivate: HResult;
begin
  Result := inherited OnInPlaceActivate;
end;

procedure TDocBrowser.WMSetFocus(var Message: TWMSetFocus);
{var
  V: IOleDocumentView;}
begin
  inherited;
  {if Supports(Document, IOleDocumentView, V) then
  begin
    V.UIActivate(False);
    V.UIActivate(True);
  end;}
end;

function TDocBrowser.OnUIActivate: HResult;
{var
  F: TCustomForm;}
begin
  { Override OnUIActivate to disable default buttons bug }
  Result := inherited OnUIActivate;
  {F := GetParentForm(Self);
  if F <> nil then
    F.SetFocusedControl(Self);}
end;

function TDocBrowser.GetWindowContext(out frame: IOleInPlaceFrame;
  out doc: IOleInPlaceUIWindow; out rcPosRect: TRect;
  out rcClipRect: TRect; out frameInfo: TOleInPlaceFrameInfo): HResult;
begin
  Result := inherited GetWindowContext(frame, doc, rcPosRect, rcClipRect, frameInfo);
end;

function TDocBrowser.Scroll(scrollExtent: TPoint): HResult;
begin
  Result := inherited Scroll(scrollExtent);
end;

function TDocBrowser.OnUIDeactivate(fUndoable: BOOL): HResult;
begin
  Result := inherited OnUIDeactivate(fUndoable);
end;

function TDocBrowser.OnInPlaceDeactivate: HResult;
begin
  Result := inherited OnInPlaceDeactivate;
end;

function TDocBrowser.DiscardUndoState: HResult;
begin
  Result := inherited DiscardUndoState;
end;

function TDocBrowser.DeactivateAndUndo: HResult;
begin
  Result := inherited DeactivateAndUndo;
end;

function TDocBrowser.OnPosRectChange(const rcPosRect: TRect): HResult;
begin
  Result := inherited OnPosRectChange(rcPosRect);
end;

{ TDocBrowser.IDocBrowser }

function TDocBrowser.EnableModeless(const fEnable: BOOL): HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
	Result := S_OK;
end;

function TDocBrowser.FilterDataObject(
  const pDO: IDataObject;
  out ppDORet: IDataObject): HResult;
begin
  { Return S_FALSE to show no data object supplied.
    We *must* also set ppDORet to nil }
  ppDORet := nil;
  Result := S_FALSE;
end;

function TDocBrowser.GetDropTarget(
  const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HResult;
begin
  { Return E_FAIL since no alternative drop target supplied.
    We *must* also set ppDropTarget to nil }
  ppDropTarget := nil;
  Result := E_FAIL;
end;

function TDocBrowser.GetExternal(
  out ppDispatch: IDispatch): HResult;
begin
  ppDispatch := FExtern;
  if ppDispatch = nil then
	  Result := E_FAIL
	else
	  Result := S_OK;
end;

function TDocBrowser.GetHostInfo(
  var pInfo: TDocHostUIInfo): HResult;
const
  DOCHOSTUIFLAG_SCROLL_NO = $00000008;
  DOCHOSTUIFLAG_NO3DBORDER = $00000004;
  DOCHOSTUIFLAG_DIALOG = $00000001;
  DOCHOSTUIFLAG_THEME = $00040000;
  DOCHOSTUIFLAG_NOTHEME = $00080000;
begin
  FillChar(pInfo, SizeOf(pInfo), #0);
  pInfo.cbSize := SizeOf(pInfo);
  pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_THEME;
  if not (doBorder in FOptions) then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_NO3DBORDER;
  // Set scroll bar visibility
  if not (doScrollbars in FOptions) then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_SCROLL_NO;
  // Decide if text can be selected
  if not (doSelection in FOptions) then
  pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_DIALOG;
  // modified ... we won't support client side CSS
  pInfo.pchHostCss := nil;
  Result := S_OK;
end;

function TDocBrowser.GetOptionKeyPath(
  var pchKey: POLESTR;
  const dw: DWORD): HResult;
begin
  { Return E_FAIL to indicate we failed to override
    default registry settings }
  Result := E_FAIL;
end;

function TDocBrowser.HideUI: HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TDocBrowser.OnDocWindowActivate(
  const fActivate: BOOL): HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TDocBrowser.OnFrameWindowActivate(
  const fActivate: BOOL): HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;

function TDocBrowser.ResizeBorder(
  const prcBorder: PRECT;
  const pUIWindow: IOleInPlaceUIWindow;
  const fFrameWindow: BOOL): HResult;
begin
  { Return S_FALSE to indicate we did nothing in response }
  Result := S_FALSE;
end;

function TDocBrowser.ShowContextMenu(
  const dwID: DWORD;
  const ppt: PPOINT;
  const pcmdtReserved: IInterface;
  const pdispReserved: IDispatch): HResult;
begin
  { Return S_FALSE to notify we didn't display a menu and to
    let browser display its own menu }
	if doContextMenu in FOptions then
	  Result := S_FALSE
  else
  begin
	  Result := S_OK;
    if PopupMenu <> nil then
      PopupMenu.Popup(ppt.X, ppt.Y);
  end
end;

function TDocBrowser.ShowUI(
  const dwID: DWORD;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget;
  const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HResult;
begin
  { Return S_OK to say we displayed own UI }
  Result := S_OK;
end;

function TDocBrowser.TranslateAccelerator(
  const lpMsg: PMSG;
  const pguidCmdGroup: PGUID;
  const nCmdID: DWORD): HResult;
begin
  { Return S_FALSE to indicate no accelerators are translated }
  Result := S_FALSE;
end;

function TDocBrowser.TranslateUrl(
  const dwTranslate: DWORD;
  const pchURLIn: POLESTR;
  var ppchURLOut: POLESTR): HResult;
begin
  { Return E_FAIL to indicate that no translations took place }
  Result := E_FAIL;
end;

function TDocBrowser.UpdateUI: HResult;
begin
  { Return S_OK to indicate we handled (ignored) OK }
  Result := S_OK;
end;    

{ TAutoDispatch }

function ArgParam(Index: Integer; const Args: TArgList): OleVariant;
begin
	Result := OleVariant(Args.Arguments[Args.Count - Index - 1]);
end;

type
	PPInteger = ^PInteger;

const
	BaseMethodDispid = $200;
	BasePropertyDispid = -$200;

  DISPATCH_METHOD         = $1;
  DISPATCH_PROPERTYGET    = $2;
  DISPATCH_PROPERTYPUT    = $4;

constructor TAutoDispatch.Create;
begin
	inherited Create;
	FMethods := TStringList.Create;
  FProperties := TStringList.Create;
end;

destructor TAutoDispatch.Destroy;
begin
	FProperties.Free;
	FMethods.Free;
  inherited Destroy;
end;

function TAutoDispatch.OnMethod(MethodIndex: Integer; const Args: TArgList): OleVariant;
begin
	VarClear(Result);
end;

function TAutoDispatch.OnGetProperty(PropIndex: Integer; const Args: TArgList): OleVariant;
begin
	VarClear(Result);
end;

procedure TAutoDispatch.OnSetProperty(PropIndex: Integer; const Args: TArgList; const Value: OleVariant);
begin
end;

{ TAutoDispatch.IDispatch }

function TAutoDispatch.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
	Pointer(TypeInfo) := nil;
	Result := S_OK;
end;

function TAutoDispatch.GetTypeInfoCount(out Count: Integer): HResult;
begin
	Count := 0;
	Result := S_OK;
end;

function TAutoDispatch.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
var
	S: WideString;
  I: Integer;
begin
	Result := DISP_E_UNKNOWNNAME;
	if NameCount = 1 then
  begin
  	S := UpperCase(PPWideChar(Names)^);
    I := FMethods.IndexOf(S);
    if I > -1 then
    begin
    	PInteger(DispIDs)^ := I + BaseMethodDispid;
    	Result := S_OK;
    end
    else
    begin
	    I := FProperties.IndexOf(S);
	    if I > -1 then
  	  begin
    		PInteger(DispIDs)^ := I + BasePropertyDispid;
    		Result := S_OK;
	    end;
		end;
	end
end;

function TAutoDispatch.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
var
	DispParams: PDispParams;
  Output: POleVariant absolute VarResult;
  ArgList: TArgList;
begin
	DispParams := @Params;
	Output := VarResult;
  ArgList.Count := DispParams.cArgs;
  ArgList.Arguments := DispParams.rgvarg;
	Result := DISP_E_BADINDEX;
	try
		if Flags and DISPATCH_METHOD = DISPATCH_METHOD then
    begin
			Dec(DispId, BaseMethodDispid);
      if (DispId > -1) and (DispId < FMethods.Count) then
      begin
        Result := S_OK;
        if Output <> nil then
	      	Output^ := OnMethod(DispId, ArgList)
				else
	      	OnMethod(DispId, ArgList);
			end;
    end
    else if (Flags = DISPATCH_PROPERTYGET) or (Flags = DISPATCH_PROPERTYPUT) then
    begin
			Dec(DispId, BasePropertyDispid);
      if (DispId > -1) and (DispId < FProperties.Count) then
      begin
        Result := S_OK;
      	if Flags = DISPATCH_PROPERTYGET  then
	      	Output^ := OnGetProperty(DispId, ArgList)
				else
        begin
          if DispParams.cArgs > 0 then
						OnSetProperty(DispId, ArgList, OleVariant(DispParams.rgvarg^[0]))
					else
          	Result := DISP_E_BADPARAMCOUNT;
				end;
			end;
    end
  except
  	// fill out error information
		Result := E_FAIL;
  end;
end;

end.

