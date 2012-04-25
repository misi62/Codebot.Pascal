
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit ShlCtrls;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, ActiveX, CommCtrl, ShellAPI, SysUtils, Classes, Controls,
  Graphics, Forms, ImgList, ComCtrls,  ShlObj, ComObj, FileTools, GraphTools,
  ProviderTools, FormTools, BtnEdit, PopCtrls, ScrollCtrls, BaseTypes,
  ShlIntf, ShlTools, XMLObjects, XMLParser, Dialogs;

{ The GetSysImages funtion returns the handle of the system image list }

function GetSysImages(SmallImages: Boolean = True): HIMAGELIST;

{ The GetFileImageIndex function returns the system image list image index of
  the file or directory specified by the FileName parameter }

function GetFileImageIndex(const FileName: string;
  SmallImages: Boolean = True): Integer;

function GetNodeImageIndex(Node: TShellNode;
  SmallImages: Boolean = True): Integer;

{ Shared shell ImageLists }

function ShellImageList(SmallImages: Boolean = True): TCustomImageList;

{ TShellImageNode }

type
  TShellImageNode = class(TShellNode)
  private
    FSmallIndex: Integer;
    FLargeIndex: Integer;
  protected
    procedure Initialize; override;
  public
    class procedure LoadImage(Folder: TSpecialFolder); overload;
    class procedure LoadImage(const Path: string); overload;
    class procedure LoadImage(const pidl: PItemIDList); overload;
    property SmallIndex: Integer read FSmallIndex;
    property LargeIndex: Integer read FLargeIndex;
  end;

{ TSmallShellNode }

  TSmallShellNode = class(TShellNode)
  private
    FFileSystem: Boolean;
    FImageIndex: Integer;
    FSelectedIndex: Integer;
    FShowFiles: Boolean;
    FData: Pointer;
    FInstance: TObject;
    FUnknown: IUnknown;
    function GetFilePath: string;
  protected
    procedure Initialize; override;
    function EnumFlags: Longword; override;
  public
    procedure Rename(Node: TShellNode); override;
    property ShowFiles: Boolean read FShowFiles write FShowFiles;
    property FilePath: string read GetFilePath;
    property FileSystem: Boolean read FFileSystem;
    property ImageIndex: Integer read FImageIndex;
    property SelectedIndex: Integer read FSelectedIndex;
    property Data: Pointer read FData write FData;
    property Instance: TObject read FInstance write FInstance;
    property Unknown: IUnknown read FUnknown write FUnknown;
  end;

{ Images }

  TShellImageList = class(TCustomImageList)
  private
    FImages: IImageList;
  protected
    procedure DoDraw(Index: Integer; Canvas: TCanvas; X,
      Y: Integer; Style: Cardinal; Enabled: Boolean); override;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
    procedure Loaded; override;
    procedure Setup; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSmallShellImages = class(TShellImageList)
  protected
    procedure Setup; override;
  end;

  TLargeShellImages = class(TShellImageList)
  protected
    procedure Setup; override;
  end;

{ TShellControl class }

  TShellControl = class(TFramedWindow, IUnknown)
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; reintroduce;
      stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TShellTreeNode class }

  TShellTree = class;

  TShellTreeNode = class(TSmallShellNode)
  private
    FHandle: Pointer;
    FTree: TShellTree;
    function GetParent: TShellTreeNode;
    function GetItem(Index: Integer): TShellTreeNode;
  protected
    property Tree: TShellTree read FTree write FTree;
    property Handle: Pointer read FHandle write FHandle;
  public
    procedure Expand;
    procedure Collapse;
    procedure Rename(Node: TShellNode); override;
    property Item[Index: Integer]: TShellTreeNode read GetItem; default;
    property Parent: TShellTreeNode read GetParent;
  end;

{ TShellTree class }

  TShellActionEvent = procedure(Sender: TObject; Node: TShellNode;
    var AllowAction: Boolean) of object;
  TShellNotifyEvent = procedure(Sender: TObject; Node: TShellNode) of
    object;

  TShellTree = class(TShellControl, IDropTarget)
  private
    FAllowContextMenu: Boolean;
    FContextMenu: IContextMenu;
    FMenu: HMENU;
    FRoot: TShellNode;
    FSelectedNode: TShellNode;
    FAutoExpand: Boolean;
    FHideSelection: Boolean;
    FHotTrack: Boolean;
    FRowSelect: Boolean;
    FShowButtons: Boolean;
    FShowLines: Boolean;
    FShowRoot: Boolean;
    FSpecialFolder: TSpecialFolder;
    FReadOnly: Boolean;
    FToolTips: Boolean;
    FNotify: THandle;
    FOnChange: TShellNotifyEvent;
    FOnCollapse: TShellNotifyEvent;
    FOnCollapsing: TShellActionEvent;
    FOnExpand: TShellNotifyEvent;
    FOnExpanding: TShellActionEvent;
    procedure HandleContextMenu;
    procedure SetAutoExpand(Value: Boolean);
    procedure SetShowButtons(Value: Boolean);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetShowLines(Value: Boolean);
    function GetIndent: Integer;
    procedure SetReadOnly(Value: Boolean);
    procedure SetShowRoot(Value: Boolean);
    procedure SetRoot(Value: TShellNode);
    procedure SetRowSelect(Value: Boolean);
    function GetSelectedNode: TShellNode;
    procedure SetSelectedNode(Value: TShellNode);
    procedure SetSpecialFolder(Value: TSpecialFolder);
    procedure SetToolTips(Value: Boolean);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMShellNotify(var Message: TWMShellNotify); message WM_SHELLNOTIFY;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoChange(Node: TShellNode); dynamic;
    procedure SetDragMode(Value: TDragMode); override;
    procedure InsertNode(Node: TShellTreeNode);
    function GetNode(TreeItem: HTREEITEM): TShellTreeNode;
    { IDropTarget }
    function DropTargetDragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DropTargetDragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DropTargetDragLeave: HResult; stdcall;
    function DropTargetDrop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function IDropTarget.DragEnter = DropTargetDragEnter;
    function IDropTarget.DragOver = DropTargetDragOver;
    function IDropTarget.DragLeave = DropTargetDragLeave;
    function IDropTarget.Drop = DropTargetDrop;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetNodeAt(X, Y: Integer): TShellTreeNode;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    property SelectedNode: TShellNode read GetSelectedNode write SetSelectedNode;
    property Root: TShellNode read FRoot write SetRoot;
  published
    property AllowContextMenu: Boolean read FAllowContextMenu write FAllowContextMenu;
    property AutoExpand: Boolean read FAutoExpand write SetAutoExpand;
    property HideSelection: Boolean read FHideSelection write SetHideSelection;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property Indent: Integer read GetIndent write SetIndent;
    property RowSelect: Boolean read FRowSelect write SetRowSelect;
    property SpecialFolder: TSpecialFolder read FSpecialFolder write
      SetSpecialFolder default sfDesktop;
    property ShowButtons: Boolean read FShowButtons write SetShowButtons;
    property ShowLines: Boolean read FShowLines write SetShowLines;
    property ShowRoot: Boolean read FShowRoot write SetShowRoot;
    property ToolTips: Boolean read FToolTips write SetToolTips;
    property OnChange: TShellNotifyEvent read FOnChange write FOnChange;
    property OnCollapse: TShellNotifyEvent read FOnCollapse write
      FOnCollapse;
    property OnCollapsing: TShellActionEvent read FOnCollapsing write
      FOnCollapsing;
    property OnExpand: TShellNotifyEvent read FOnExpand write
      FOnExpand;
    property OnExpanding: TShellActionEvent read FOnExpanding write
      FOnExpanding;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TPopupTreeForm }

  TShellTreePopupForm = class(TCustomPopupForm)
  private
    FShellTree: TShellTree;
    FSelectChanged: Boolean;
    FHoverNode: TShellTreeNode;
    FOnNodeHover: TShellNotifyEvent;
    procedure ShellTreeChange(Sender: TObject; Node: TShellNode);
    procedure ShellTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ShellTreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WMNCPaint(var Message: TWMNCPaint); message WM_NCPAINT;
  protected
    procedure DoNodeHover(Node: TShellTreeNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup; override;
  published
    property Associate;
    property Sizeable;
    property StatusText;
    property ShellTree: TShellTree read FShellTree;
    property OnNodeHover: TShellNotifyEvent read FOnNodeHover write FOnNodeHover;
    property OnCancel;
    property OnSelect;
  end;

{ TShellEdit }

  TShellEditOptions = set of (soDefaultAction, soNavigate, soShortName, soSuggest);

  TShellEdit = class(TPopupEdit)
  private
    FAutoComplete: IAutoComplete2;
    FShellTreePopupForm: TShellTreePopupForm;
    FOptions: TShellEditOptions;
    FStrings: IUnknown;
    FOnNodeHover: TShellNotifyEvent;
    procedure SelectionChanged;
    procedure TreeNodeHover(Sender: TObject; Node: TShellNode);
    function GetSelectedNode: TShellNode;
    procedure SetSelectedNode(Value: TShellNode);
    function GetHotTrack: Boolean;
    procedure SetHotTrack(Value: Boolean);
    procedure SetOptions(Value: TShellEditOptions);
    function GetRoot: TShellNode;
    procedure SetRoot(Value: TShellNode);
    function GetStatusText: string;
    procedure SetStatusText(Value: string);
    function GetSpecialFolder: TSpecialFolder;
    procedure SetSpecialFolder(Value: TSpecialFolder);
  protected
    function CreatePopup: TCustomPopupForm; override;
    procedure CreateWnd; override;
    procedure DoPopup; override;
    procedure DoCancel(Sender: TObject); override;
    procedure DoSelect(Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property SelectedNode: TShellNode read GetSelectedNode write SetSelectedNode;
    property Root: TShellNode read GetRoot write SetRoot;
    property ButtonDown;
    property Canvas;
    property NextControl;
    property SearchStrings;
    property TextChanged;
  published
    property Options: TShellEditOptions read FOptions write SetOptions;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack;
    property StatusText: string read GetStatusText write SetStatusText;
    property SpecialFolder: TSpecialFolder read GetSpecialFolder write SetSpecialFolder default sfDesktop;
    property OnNodeHover: TShellNotifyEvent read FOnNodeHover write FOnNodeHover;
    property AutoHeight;
    property BorderStyle;
    property ButtonVisible;
    property Anchors;
    property AutoSelect;
    property CharCase;
    property Ctl3D;
    property Enabled;
    property Font;
    property Flat;
    property Glyph;
    property HideSelection;
    property Images;
    property ImageIndex;
    property MaxLength;
    property Modified;
    property OEMConvert;
    property PasswordChar;
    property ReadOnly;
    property SelLength;
    property SelStart;
    property SelText;
    property ShowHint;
    property Sizeable;
    property Style;
    property TabOrder;
    property TabStop;
    property Title;
    property Text;
    property Visible;
    property WantTabs;
    property OnButtonPress;
    property OnButtonClick;
    property OnChange;
    property OnCancel;
    property OnClick;
    property OnContextPopup;
    property OnCustomDraw;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPlaceEdit;
    property OnPopup;
    property OnSearch;
    property OnStartDock;
    property OnStartDrag;
    property OnSelect;
    property OnTab;
  end;

{ TShellView }

  TWMSizing = packed record
    Msg: Cardinal;
    Side: Longint;
    Rect: PRect;
    Result: Longint;
  end;

  TShellViewMode = (vmIcon, vmSmallIcon, vmList, vmDetails, vmThumbnail,
    vmTile, vmThumbstrip);

  TShellView = class(TShellControl, IOleWindow, IShellBrowser, ICommDlgBrowser,
    ICommDlgBrowser2)
  private
    FAllowContextMenu: Boolean;
    FShellBrowser: IShellBrowser;
    FDefListViewProc: Pointer;
    FListViewInstance: Pointer;
    FListViewHandle: HWND;
    FDefShellViewProc: Pointer;
    FShellViewInstance: Pointer;
    FShellViewHandle: HWND;
    FDefaultKeys: Boolean;
    FRoot: TShellNode;
    FShellView: IShellView;
    FSpecialFolder: TSpecialFolder;
    FStream: IStream;
    FStatusBar: TStatusBar;
    FViewMode: TShellViewMode;
    FFocusedNode: TShellNode;
    FParentRoot: TShellNode;
    FOnChange: TShellNotifyEvent;
    FOnDefaultAction: TShellActionEvent;
    FOnIncludeItem: TShellActionEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnViewChanged: TNotifyEvent;
    procedure ListViewWndProc(var Message: TMessage);
    procedure ShellViewWndProc(var Message: TMessage);
    procedure SetParentRoot(Value: TShellNode);
    procedure SetRoot(Value: TShellNode);
    procedure SetSpecialFolder(Value: TSpecialFolder);
    procedure SetViewMode(Value: TShellViewMode);
    procedure UpdateTextColor;
    procedure UpdateTextBkColor;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMGetIShellBrowser(var Message: TMessage); message WM_GETISHELLBROWSER;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Message: TMessage); message WM_HSCROLL;
    procedure WMVScroll(var Message: TMessage); message WM_VSCROLL;
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoChange; dynamic;
    procedure DoViewChanged; dynamic;
    { IOleWindow }
    function GetWindow(out wnd: HWnd): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    { IShellBrowser }
    function InsertMenusSB(hMenuShared: HMENU;
      out MenuWidths: TOleMenuGroupWidths): HResult; stdcall;
    function SetMenuSB(hMenuShared: HMENU;
      hOleMenuReserved: HOLEMENU; hwndActiveObject: HWND): HResult; stdcall;
    function RemoveMenusSB(hMenuShared: HMENU): HResult; stdcall;
    function SetStatusTextSB(StatusText: POleStr): HResult; stdcall;
    function EnableModelessSB(Enable: BOOL): HResult; stdcall;
    function TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult; stdcall;
    function BrowseObject(pidl: PItemIDList; flags: UINT): HResult; stdcall;
    function GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult; stdcall;
    function GetControlWindow(ID: UINT; out Wnd: HWND): HResult; stdcall;
    function SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM;
      var Rslt: LResult): HResult; stdcall;
    function QueryActiveShellView(var ShellView: IShellView): HResult; stdcall;
    function OnViewWindowActive(var ShellView: IShellView): HResult; stdcall;
    function SetToolbarItems(TBButton: PTBButton;
      nButtons, uFlags: UINT): HResult; stdcall;
    { ICommDlgBrowser }
    function OnDefaultCommand(const ppshv: IShellView): HResult; stdcall;
    function OnStateChange(const ppshv: IShellView; Change: ULONG): HResult; stdcall;
    function IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult; stdcall;
    { ICommDlgBrowser2 }
    function Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult; stdcall;
    function GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
      cchMax: Integer): HResult; stdcall;
    function GetViewFlags(out pdwFlags: DWORD): HResult; stdcall;
    { ICommDlgBrowser3 }
    function OnColumnClicked(ppshv: IShellView; iColumn: Integer): HResult; stdcall;
    function GetCurrentFilter(pszFileSpec:  PWideChar; cchFileSpec: Integer): HResult; stdcall;
    function OnPreViewCreated(ppshv: IShellView): HResult; stdcall;
    { IServiceProvider }
    function SRemoteQueryService(const guidService, riid: TGUID;
      out ppvObject: IUnknown): HResult; stdcall;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ViewHandle: HWND read FListViewHandle;
    procedure ExecuteVerb(const Verb: string);
    procedure Explore(const Path: string);
    procedure Refresh; reintroduce;
    procedure Rename;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure Delete;
    procedure Properties;
    procedure GetItems(Strings: TStrings; Selection: Boolean = True; FilePath: Boolean = False);
    function GetFocusedNode: TShellNode;
    procedure Up;
    property ParentRoot: TShellNode read FParentRoot write SetParentRoot;
    property Root: TShellNode read FRoot write SetRoot;
    property ShellView: IShellView read FShellView;
  published
    property AllowContextMenu: Boolean read FAllowContextMenu write FAllowContextMenu;
    property DefaultKeys: Boolean read FDefaultKeys write FDefaultKeys default True;
    property SpecialFolder: TSpecialFolder read FSpecialFolder write
      SetSpecialFolder;
    property StatusBar: TStatusBar read FStatusBar write FStatusBar;
    property ViewMode: TShellViewMode read FViewMode write SetViewMode;
    property OnDefaultAction: TShellActionEvent read FOnDefaultAction write
      FOnDefaultAction;
    property OnChange: TShellNotifyEvent read FOnChange write FOnChange;
    property OnIncludeItem: TShellActionEvent read FOnIncludeItem write
      FOnIncludeItem;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnViewChanged: TNotifyEvent read FOnViewChanged write FOnViewChanged;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property Constraints;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TShellBubbleItem }

  TShellBubbleItem = class(TCollectionItem)
  private
    FNode: TShellImageNode;
    FPath: string;
    FSpecialFolder: TSpecialFolder;
    procedure SetPath(const Value: string);
    procedure SetSpecialFolder(Value: TSpecialFolder);
  public
    constructor Create(Collection: TCollection); override;
    constructor CreateFromFolder(Collection: TCollection; SpecialFolder: TSpecialFolder);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Node: TShellImageNode read FNode;
  published
    property Path: string read FPath write SetPath;
    property SpecialFolder: TSpecialFolder read FSpecialFolder write SetSpecialFolder;
  end;

{ TShellBubbleItems }

  TShellBubbleItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TShellBubbleItem;
    procedure SetItem(Index: Integer; Value: TShellBubbleItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TShellBubbleItem;
    function AddFolder(SpecialFolder: TSpecialFolder): TShellBubbleItem;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TShellBubbleItem read GetItem write SetItem; default;
  end;

{ TShellBubbles }

  TShellBubbleStyle = (sbSmall, sbLarge);

  TShellBubbles = class(TCustomBubbleList)
  private
    FItems: TShellBubbleItems;
    FNavigateRoot: Boolean;
    FStyle: TShellBubbleStyle;
    FTextLabels: Boolean;
    procedure SetItems(Value: TShellBubbleItems);
    procedure SetStyle(Value: TShellBubbleStyle);
    procedure SetTextLabels(Value: Boolean);
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
    procedure Update; override;
    property ItemIndex;
  published
    property Items: TShellBubbleItems read FItems write SetItems;
    property NavigateRoot: Boolean read FNavigateRoot write FNavigateRoot default False;
    property Style: TShellBubbleStyle read FStyle write SetStyle default sbLarge;
    property TextLabels: Boolean read FTextLabels write SetTextLabels default False;
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property DesktopFont;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop;
    property OnCanResize;
    property OnDblClick;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectItem;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TShellListBox }

  TShellListBox = class(TCustomDrawList)
  private
    FRoot: TShellNode;
    FItems: TList;
    FShowFiles: Boolean;
    procedure SetShowFiles(Value: Boolean);
    procedure SetRoot(Value: TShellNode);
    function GetSelectedNode: TShellNode;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    procedure DrawBackground; override;
    procedure DrawItem(Index: Integer; var Rect: TRect;
      State: TDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Root: TShellNode read FRoot write SetRoot;
    property SelectedNode: TShellNode read GetSelectedNode;
  published
    property ShowFiles: Boolean read FShowFiles write SetShowFiles;
    property OnClick;
  end;

{ TShellPathEditBar }

  TShellPathEditBar = class(TPathEditBar)
  private
    FAssociateFrame: TFrame;
    FAutoComplete: IAutoComplete2;
    FStrings: IUnknown;
    FSelectedNode: TShellNode;
    FRoot: TShellNode;
    FShowFiles: Boolean;
    FShowSuggest: Boolean;
    FSpecialFolder: TSpecialFolder;
    FOnChange: TShellNotifyEvent;
    FOnDefaultAction: TShellActionEvent;
    procedure DefaultAction(Node: TShellNode);
    function GetAssociateFrame: TFrame;
    procedure SetAssociateFrame(Value: TFrame);
    procedure SetSelectedNode(Value: TShellNode);
    procedure SetSpecialFolder(Value: TSpecialFolder);
    procedure SetRoot(Value: TShellNode);
    procedure SetShowFiles(Value: Boolean);
    procedure SetShowSuggest(Value: Boolean);
  protected
    procedure Loaded; override;
    function AddItem(Document: IDocument; Parent: INode; Item: TSmallShellNode): INode;
    procedure ButtonClick(Index: Integer); override;
    function Convert: string; override;
    function CanDropDown(Index: Integer): Boolean; override;
    procedure DoChange(Node: TShellNode); dynamic;
    procedure EditingChange; override;
    procedure FolderSelect(Sender: TObject); override;
    procedure PopupSelect(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Parse(const Path: string); override;
    property SelectedNode: TShellNode read FSelectedNode write SetSelectedNode;
    property Root: TShellNode read FRoot write SetRoot;
    property FolderItems;
    property PopupItems;
    property Editing;
  published
    property AssociateFrame: TFrame read GetAssociateFrame write SetAssociateFrame;
    property SpecialFolder: TSpecialFolder read FSpecialFolder write SetSpecialFolder default sfDesktop;
    property ShowFiles: Boolean read FShowFiles write SetShowFiles default False;
    property ShowSuggest: Boolean read FShowSuggest write SetShowSuggest default True;
    property OnChange: TShellNotifyEvent read FOnChange write FOnChange;
    property OnDefaultAction: TShellActionEvent read FOnDefaultAction write
      FOnDefaultAction;
    property Align;
    property Anchors;
    property AutoNext;
    property AutoPopup;
    property AutoSize;
    property BiDiMode;
    property BorderStyle;
    property ButtonWidth;
    property Constraints;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowButtons;
    property ShowHint;
    property ShowIcons;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnButtonDraw;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TShellBinding }

  TShellBinding = class(TComponentBinding)
  private
    FSpecialFolder: TSpecialFolder;
    procedure SetSpecialFolder(Value: TSpecialFolder);
  protected
    procedure Refresh; override;
    procedure DoChange(Sender: TObject; var Param); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SpecialFolder: TSpecialFolder read FSpecialFolder write SetSpecialFolder;
  end;

function FindTreeNode(Root: TShellTreeNode; Node: TShellNode): TShellTreeNode;
procedure HideSuggest;

var RegisterShellUnits: procedure(AOwner: TComponent);

implementation

const
  ShellImages: array[Boolean] of DWORD = (0, SHGFI_SMALLICON);

var
  ShellImageLists: array[Boolean] of TObject;

function GetSysImages(SmallImages: Boolean = True): HIMAGELIST;
var
  SHFileInfo: TSHFileInfo;
begin
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  Result := SHGetFileInfo('', 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or ShellImages[SmallImages]);
end;

function GetFileImageIndex(const FileName: string;
  SmallImages: Boolean = True): Integer;
var
  SHFileInfo: TSHFileInfo;
begin
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(FileName), 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or ShellImages[SmallImages]);
  Result := SHFileInfo.iIcon;
end;

function GetNodeImageIndex(Node: TShellNode;
  SmallImages: Boolean = True): Integer;
var
  SHFileInfo: TSHFileInfo;
begin
  if Node = nil then
  begin
    Result := -1;
    Exit;
  end;
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(Node.AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or ShellImages[SmallImages] or SHGFI_PIDL);
  Result := SHFileInfo.iIcon;
end;

function ShellImageList(SmallImages: Boolean = True): TCustomImageList;
begin
  Result := TCustomImageList(ShellImageLists[SmallImages]);
  if Result = nil then
  begin
    if SmallImages then
      Result := TSmallShellImages.Create(nil)
    else
      Result := TLargeShellImages.Create(nil);
    ShellImageLists[SmallImages] := Result;
  end;
end;

{ TShellImageNode }

procedure TShellImageNode.Initialize;
const
  Flags = SHGFI_SYSICONINDEX or SHGFI_PIDL;
var
  SHFileInfo: TSHFileInfo;
begin
  inherited Initialize;
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags or SHGFI_SMALLICON);
  FSmallIndex := SHFileInfo.iIcon;
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags or SHGFI_LARGEICON);
  FLargeIndex := SHFileInfo.iIcon;
end;

class procedure TShellImageNode.LoadImage(Folder: TSpecialFolder);
begin
  TShellImageNode.CreateFromFolder(Folder).Free;
end;

class procedure TShellImageNode.LoadImage(const Path: string);
begin
  TShellImageNode.CreateFromList(ILCreateFromPath(Path)).Free;
end;

class procedure TShellImageNode.LoadImage(const pidl: PItemIDList);
begin
  TShellImageNode.CreateFromList(ILClone(pidl)).Free;
end;

{ Images }

constructor TShellImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Setup;
end;

procedure TShellImageList.Setup;
begin
  ShareImages := True;
  DrawingStyle := dsTransparent;
  ImageList_SetBkColor(Handle, CLR_NONE);
  FImages := nil;
  ImageListQueryInterface(Self, IImageList, FImages);
end;

procedure TShellImageList.DoDraw(Index: Integer; Canvas: TCanvas; X,
  Y: Integer; Style: Cardinal; Enabled: Boolean);
var
  Params: TImageListDrawParams;
  Obj: IImageList;
begin
  if FImages <> nil then
  begin
     FillChar(Params, SizeOf(Params), #0);
     Params.cbSize := SizeOf(Params);
    Params.himl := Handle;
    Params.i := Index;
    Params.hdcDst := Canvas.Handle;
    Params.x := X;
    Params.y := Y;
    Params.rgbBk := CLR_NONE;
    if not Enabled then
      Params.fStyle := 1
    else
      Params.fStyle := 0;
    if not Enabled then
      Params.fState := 4;
    Obj := FImages as IImageList;
     Obj.Draw(Params);
    if Enabled and (Style and ILD_FOCUS <> ILD_FOCUS) then
    begin
       Obj.Draw(Params);
       Obj.Draw(Params);
    end;
  end
  else
    inherited;
end;

procedure TShellImageList.ReadData(Stream: TStream);
begin
end;

procedure TShellImageList.WriteData(Stream: TStream);
begin
end;

procedure TShellImageList.Loaded;
begin
  inherited Loaded;
  Setup;
end;

procedure TSmallShellImages.Setup;
begin
  Handle := GetSysImages(True);
  inherited Setup;
end;

procedure TLargeShellImages.Setup;
begin
  Handle := GetSysImages(False);
  inherited Setup;
end;

{ TShellControl }

constructor TShellControl.Create(AOwner: TComponent);
var
  O: TComponent;
begin
  inherited Create(AOwner);
  Color := clWindow;
  O := AOwner;
  while (O <> nil) and ((O is TCustomPopupForm) or (not (O is TCustomForm))) do
    O := O.Owner;
  if (O <> nil) and (@RegisterShellUnits <> nil) then
    RegisterShellUnits(O);
end;

function MouseInControl(Wnd: HWND): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  GetWindowRect(Wnd, R);
  GetCursorPos(P);
  Result := PtInRect(R, P);
end;

{ TShellControl.IUnknown }

const
  E_NOINTERFACE = HResult($80004002);

function TShellControl.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TShellControl._AddRef: Integer;
begin
  Result := 1;
end;

function TShellControl._Release: Integer;
begin
  Result := 1;
end;

{ TShellTreeNode }

procedure TShellTreeNode.Expand;
var
  I: Integer;
begin
  if TreeView_GetChild(FTree.Handle, Handle) = nil then
    for I := 0 to Count - 1 do
      FTree.InsertNode(Item[I]);
   TreeView_Expand(FTree.Handle, Handle, TVE_EXPAND);
end;

procedure TShellTreeNode.Collapse;
begin
   TreeView_Expand(FTree.Handle, Handle, TVE_COLLAPSE);
end;

procedure TShellTreeNode.Rename(Node: TShellNode);
var
  Item: TTVItem;
  S: string;
begin
  inherited Rename(Node);
  S := Name;
  with Item do
  begin
    mask := TVIF_TEXT;
    hItem := Handle;
    pszText := PChar(S);
    cchTextMax := Length(S);
  end;
  TreeView_SetItem(FTree.Handle, Item);
end;

function TShellTreeNode.GetParent: TShellTreeNode;
begin
  Result := TShellTreeNode(inherited Parent);
end;

function TShellTreeNode.GetItem(Index: Integer): TShellTreeNode;
begin
  Result := TShellTreeNode(inherited Item[index]);
end;

function FindTreeNode(Root: TShellTreeNode; Node: TShellNode): TShellTreeNode;
var
  I: Integer;
begin
  Result := nil;
  if ILIsEqual(Root.AbsoluteList, Node.AbsoluteList) then
  begin
    Result := Root;
    // Root.Expand;
  end
  else if ILIsParent(Root.AbsoluteList, Node.AbsoluteList, False) then
  begin
    Root.Expand;
    for I := 0 to Root.Count - 1 do
    begin
      Result := FindTreeNode(Root[I], Node);
      if Result <> nil then Break;
    end;
  end;
end;

{ TShellTree }

procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean);
var
  Style: Integer;
begin
  if Ctl.HandleAllocated then
  begin
    Style := GetWindowLong(Ctl.Handle, GWL_STYLE);
    if not UseStyle then Style := Style and not Value
    else Style := Style or Value;
    SetWindowLong(Ctl.Handle, GWL_STYLE, Style);
  end;
end;

constructor TShellTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csCaptureMouse] + [csDisplayDragImage,
    csReflector];
  Width := 121;
  Height := 97;
  TabStop := True;
  ReadOnly := True;
  ParentColor := False;
  FAllowContextMenu := True;
  FShowButtons := True;
  FShowRoot := False;
  FShowLines := True;
  FHideSelection := True;
  FToolTips := True;
  FSpecialFolder := sfDesktop;
  FRoot := TShellTreeNode.CreateFromFolder(FSpecialFolder);
end;

destructor TShellTree.Destroy;
begin
  if FNotify <> 0 then
    SHChangeNotifyDeregister(FNotify);
  FRoot.Free;
  inherited Destroy;
end;

procedure TShellTree.CreateParams(var Params: TCreateParams);
const
  LineStyles: array[Boolean] of DWORD = (0, TVS_HASLINES);
  RootStyles: array[Boolean] of DWORD = (0, TVS_LINESATROOT);
  ButtonStyles: array[Boolean] of DWORD = (0, TVS_HASBUTTONS);
  EditStyles: array[Boolean] of DWORD = (TVS_EDITLABELS, 0);
  HideSelections: array[Boolean] of DWORD = (TVS_SHOWSELALWAYS, 0);
  DragStyles: array[TDragMode] of DWORD = (TVS_DISABLEDRAGDROP, 0);
  RTLStyles: array[Boolean] of DWORD = (0, TVS_RTLREADING);
  ToolTipStyles: array[Boolean] of DWORD = (TVS_NOTOOLTIPS, 0);
  AutoExpandStyles: array[Boolean] of DWORD = (0, TVS_SINGLEEXPAND);
  HotTrackStyles: array[Boolean] of DWORD = (0, TVS_TRACKSELECT);
  RowSelectStyles: array[Boolean] of DWORD = (0, TVS_FULLROWSELECT);
begin
  InitCommonControl(ICC_TREEVIEW_CLASSES);
  inherited CreateParams(Params);
  CreateSubClass(Params, WC_TREEVIEW);
  with Params do
  begin
    Style := Style or LineStyles[FShowLines] or RootStyles[FShowRoot] or
      ButtonStyles[FShowButtons] or EditStyles[FReadOnly] or
      HideSelections[FHideSelection] or DragStyles[DragMode] or
      RTLStyles[UseRightToLeftReading] or ToolTipStyles[FToolTips] or
      AutoExpandStyles[FAutoExpand] or HotTrackStyles[FHotTrack] or
      RowSelectStyles[FRowSelect];
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TShellTree.CreateWnd;
var
  Entry: TSHChangeNotifyEntry;
  Images: HIMAGELIST;
begin
  inherited CreateWnd;
  Images := GetSysImages;
  TreeView_SetImageList(Handle, Images, TVSIL_NORMAL);
  InsertNode(TShellTreeNode(FRoot));
  SetReadOnly(ReadOnly);
  Entry.AbsoluteList := FRoot.AbsoluteList;
  Entry.WatchSubtree := True;
  FNotify := SHChangeNotifyRegister(Handle, SHCNRF_InterruptLevel or SHCNRF_ShellLevel or SHCNRF_RecursiveInterrupt,
    SHCNE_RENAMEFOLDER or SHCNE_MKDIR or SHCNE_RMDIR, WM_SHELLNOTIFY, 1, Entry);
  { RegisterDragDrop(Handle, Self); }
end;

procedure TShellTree.DoChange(Node: TShellNode);
begin
  if Binding <> nil then
    Binding.Change(Self, Node);
  if Assigned(FOnChange) then
    FOnChange(Self, Node);
end;

procedure TShellTree.CMColorChanged(var Message: TMessage);
begin
  inherited;
  TreeView_SetBkColor(Handle, ColorToRGB(Color));
end;

procedure TShellTree.CMFontChanged(var Message: TMessage);
begin
  inherited;
  TreeView_SetTextColor(Handle, ColorToRGB(Font.Color));
end;

procedure TShellTree.CNNotify(var Message: TWMNotify);
var
  Node: TShellTreeNode;
  AllowAction: Boolean;
  I: Integer;
begin
  with Message, PNMTreeView(NMHdr)^ do
    case NMHdr^.code of
      TVN_ITEMEXPANDING:
        begin
          Node := TShellTreeNode(itemNew.lParam);
          AllowAction := True;
          case action of
            TVE_COLLAPSE:
              if Assigned(FOnCollapsing) then
                FOnCollapsing(Self, Node, AllowAction);
            TVE_EXPAND:
              if Assigned(FOnExpanding) then
                FOnExpanding(Self, Node, AllowAction);
          end;
          if AllowAction then
          begin
            if TreeView_GetChild(Handle, itemNew.hItem) = nil then
              for I := 0 to Node.Count - 1 do
                InsertNode(Node[I]);
            if (Node.Count = 0) and (itemNew.cChildren = 1) then
            begin
              itemNew.mask := TVIF_CHILDREN;
              itemNew.cChildren := 0;
              TreeView_SetItem(Handle, itemNew);
            end;
            Message.Result := 0;
          end
          else
            Message.Result := 1;
        end;
      TVN_ITEMEXPANDED:
        begin
          Node := TShellTreeNode(itemNew.lParam);
          case action of
            TVE_COLLAPSE:
              if Assigned(FOnCollapse) then
                FOnCollapse(Self, Node);
            TVE_EXPAND:
              if Assigned(FOnExpand) then
                FOnExpand(Self, Node);
          end;
        end;
      TVN_SELCHANGED:
        begin
          if FSelectedNode <> nil then
            FSelectedNode.Unlock;
          Node := TShellTreeNode(itemNew.lParam);
          FSelectedNode := Node;
          FSelectedNode.Lock;
          DoChange(Node);
        end;
      NM_RCLICK:
        HandleContextMenu;
    end;
end;

procedure TShellTree.HandleContextMenu;
var
  Node: TShellTreeNode;
  Folder: IShellFolder;
  ItemList: PItemIDList;
begin
  if FAllowContextMenu then
  begin
    Node := TShellTreeNode(SelectedNode);
    if (Node <> nil) and (Node.Parent <> nil) then
    begin
      FMenu := CreatePopupMenu;
      try
        ItemList := Node.RelativeList;
        Folder := Node.Parent.ShellFolder;
        try
          OleCheck(Folder.GetUIObjectOf(Handle, 1, ItemList,
            IID_IContextMenu, nil, FContextMenu));
          OleCheck(FContextMenu.QueryContextMenu(FMenu, 0, 0, High(Word),
            CMF_NORMAL));
          with Mouse.CursorPos do
            TrackPopupMenu(FMenu, TPM_LEFTALIGN, x, y, 0, Handle, nil);
          Application.ProcessMessages;
        finally
          FContextMenu := nil;
        end;
      finally
        DestroyMenu(FMenu);
        FMenu := 0;
      end;
    end;
  end
  else if PopupMenu <> nil then
    PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TShellTree.WMCommand(var Message: TWMCommand);
var
  CommandInfo: TCMInvokeCommandInfo;
begin
  if (Message.NotifyCode = 0) and (FContextMenu <> nil) then
  begin
    FillChar(CommandInfo, SizeOf(TCMInvokeCommandInfo), #0);
    with CommandInfo do
    begin
      cbSize := SizeOf(TCMInvokeCommandInfo);
      hwnd := Handle;
      lpVerb := MakeIntResourceA(Message.ItemID);
      nShow := SW_SHOW;
    end;
    OleCheck(FContextMenu.InvokeCommand(CommandInfo));
  end;
  inherited;
end;

procedure TShellTree.WMContextMenu(var Message: TWMContextMenu);
begin
  inherited;
  HandleContextMenu;
end;

function GetPidlPath(var Item: PItemIDList): string;
var
  ItemPath: array[0..MAX_PATH] of Char;
begin
  FilLChar(ItemPath, SizeOf(ItemPath), #0);
  if SHGetPathFromIDList(Item, ItemPath) then
    Result := ItemPath
  else
    Result := '';
end;

procedure TShellTree.WMShellNotify(var Message: TWMShellNotify);
var
  A, B, Node: TShellNode;
  Item: PItemIDList;
  TreeItem: TTVItem;
begin
  inherited;
  case Message.Code of
    SHCNE_MKDIR:
      begin
        Item := ILClone(Message.Notify.Item1);
        ILRemoveLastID(Item);
        A := TShellNode.CreateFromList(Item);
        B := TShellNode.CreateFromList(ILClone(Message.Notify.Item1));
        try
          Node := FRoot.Find(A);
          if Node <> nil then
          begin
            with TreeItem do
            begin
              mask := TVIF_CHILDREN;
              hItem := TShellTreeNode(Node).Handle;
              cChildren := 1;
            end;
            TreeView_SetItem(Handle, TreeItem);
            B.Free;
            B := TShellNode.CreateFromPath(GetPidlPath(Message.Notify.Item1));
            InsertNode(Node.Add(B) as TShellTreeNode);
          end;
        finally
          B.Free;
          A.Free;
        end;
      end;
    SHCNE_RMDIR:
      begin
        A := TShellNode.CreateFromList(Message.Notify.Item1);
        try
          Node := FRoot.Find(A);
          if Node <> nil then
          begin
            B := Node.Parent;
            B.Remove(Node);
            TreeView_DeleteItem(Handle, TShellTreeNode(Node).Handle);
            if B.Count = 0 then
            begin
              with TreeItem do
              begin
                mask := TVIF_CHILDREN;
                hItem := TShellTreeNode(B).Handle;
                cChildren := 0;
              end;
              TreeView_SetItem(Handle, TreeItem);
            end;
            Node.Free;
          end;
        finally
          A.Free;
        end;
      end;
    SHCNE_RENAMEFOLDER:
      if ExtractFilePath(GetPidlPath(Message.Notify.Item1)) = ExtractFilePath(GetPidlPath(Message.Notify.Item2)) then
      begin
        A := TShellNode.CreateFromList(Message.Notify.Item1);
        B := TShellNode.CreateFromPath(GetPidlPath(Message.Notify.Item2));
        try
          Node := FRoot.Find(A);
          if Node <> nil then
            Node.Rename(B);
        finally
          B.Free;
          A.Free;
        end;
      end
      else
      begin
        A := TShellNode.CreateFromList(Message.Notify.Item1);
        try
          Node := FRoot.Find(A);
          if Node <> nil then
            Perform(WM_SHELLNOTIFY, Integer(@Message.Notify.Item1), SHCNE_RMDIR);
        finally
          A.Free;
        end;
        A := TShellNode.CreateFromList(Message.Notify.Item2);
        try
          Node := FRoot.Find(A);
          if Node = nil then
            Perform(WM_SHELLNOTIFY, Integer(@Message.Notify.Item2), SHCNE_MKDIR);
        finally
          A.Free;
        end;
      end;
  end;
end;

procedure TShellTree.WMPaint(var Message: TMessage);
begin
  Message.Result := CallWindowProc(DefWndProc, Handle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TShellTree.InsertNode(Node: TShellTreeNode);
var
  InsertStruct: TTVInsertStruct;
begin
  FillChar(InsertStruct, SizeOf(TTVInsertStruct), #0);
  if Node.Parent <> nil then
    InsertStruct.hParent := Node.Parent.Handle;
  InsertStruct.hInsertAfter := TVI_LAST;
  with InsertStruct.item do
  begin
    mask := TVIF_CHILDREN or TVIF_IMAGE or TVIF_PARAM or TVIF_SELECTEDIMAGE or
      TVIF_TEXT;
    pszText := PChar(Node.Name);
    iImage := Node.ImageIndex;
    iSelectedImage := Node.SelectedIndex;
    if Node.HasChildren then
      cChildren := 1;
    lParam := LongInt(Node);
  end;
  Node.Tree := Self;
  Node.Handle := TreeView_InsertItem(Handle, InsertStruct);
  if Node = FRoot then
    Node.Expand;
end;

function TShellTree.GetNode(TreeItem: HTREEITEM): TShellTreeNode;
var
  Item: TTVItem;
begin
  if TreeItem <> nil then
  begin
    FillChar(Item, SizeOf(TTVItem), #0);
    with Item do
    begin
      mask := TVIF_HANDLE or TVIF_PARAM;
      hItem := TreeItem;
    end;
    TreeView_GetItem(Handle, Item);
    Result := TShellTreeNode(Item.lParam)
  end
  else
    Result := nil;
end;

function TShellTree.GetNodeAt(X, Y: Integer): TShellTreeNode;
var
  HitTest: TTVHitTestInfo;
begin
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    if TreeView_HitTest(Handle, HitTest) <> nil then
      Result := GetNode(HitTest.hItem)
    else
      Result := nil;
  end;
end;

function TShellTree.GetHitTestInfoAt(X, Y: Integer): THitTests;
var
  HitTest: TTVHitTestInfo;
begin
  Result := [];
  with HitTest do
  begin
    pt.X := X;
    pt.Y := Y;
    TreeView_HitTest(Handle, HitTest);
    if (flags and TVHT_ABOVE) <> 0 then Include(Result, htAbove);
    if (flags and TVHT_BELOW) <> 0 then Include(Result, htBelow);
    if (flags and TVHT_NOWHERE) <> 0 then Include(Result, htNowhere);
    if (flags and TVHT_ONITEM) = TVHT_ONITEM then
      Include(Result, htOnItem)
    else
    begin
      if (flags and TVHT_ONITEM) <> 0 then Include(Result, htOnItem);
      if (flags and TVHT_ONITEMICON) <> 0 then Include(Result, htOnIcon);
      if (flags and TVHT_ONITEMLABEL) <> 0 then Include(Result, htOnLabel);
      if (flags and TVHT_ONITEMSTATEICON) <> 0 then Include(Result, htOnStateIcon);
    end;
    if (flags and TVHT_ONITEMBUTTON) <> 0 then Include(Result, htOnButton);
    if (flags and TVHT_ONITEMINDENT) <> 0 then Include(Result, htOnIndent);
    if (flags and TVHT_ONITEMRIGHT) <> 0 then Include(Result, htOnRight);
    if (flags and TVHT_TOLEFT) <> 0 then Include(Result, htToLeft);
    if (flags and TVHT_TORIGHT) <> 0 then Include(Result, htToRight);
  end;
end;

procedure TShellTree.SetAutoExpand(Value: Boolean);
begin
  if FAutoExpand <> Value then
  begin
    FAutoExpand := Value;
    SetComCtlStyle(Self, TVS_SINGLEEXPAND, Value);
  end;
end;

procedure TShellTree.SetHotTrack(Value: Boolean);
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    SetComCtlStyle(Self, TVS_TRACKSELECT, Value);
  end;
end;

procedure TShellTree.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    SetComCtlStyle(Self, TVS_FULLROWSELECT, Value);
  end;
end;

procedure TShellTree.SetToolTips(Value: Boolean);
begin
  if FToolTips <> Value then
  begin
    FToolTips := Value;
    SetComCtlStyle(Self, TVS_NOTOOLTIPS, not Value);
  end;
end;

procedure TShellTree.SetDragMode(Value: TDragMode);
begin
  if Value <> DragMode then
    SetComCtlStyle(Self, TVS_DISABLEDRAGDROP, Value = dmManual);
  inherited;
end;

procedure TShellTree.SetShowButtons(Value: Boolean);
begin
  if ShowButtons <> Value then
  begin
    FShowButtons := Value;
    SetComCtlStyle(Self, TVS_HASBUTTONS, Value);
  end;
end;

procedure TShellTree.SetHideSelection(Value: Boolean);
begin
  if HideSelection <> Value then
  begin
    FHideSelection := Value;
    SetComCtlStyle(Self, TVS_SHOWSELALWAYS, not Value);
    Invalidate;
  end;
end;

procedure TShellTree.SetIndent(Value: Integer);
begin
  if Value <> Indent then TreeView_SetIndent(Handle, Value);
end;

function TShellTree.GetIndent: Integer;
begin
  Result := TreeView_GetIndent(Handle)
end;

procedure TShellTree.SetShowLines(Value: Boolean);
begin
  if ShowLines <> Value then
  begin
    FShowLines := Value;
    SetComCtlStyle(Self, TVS_HASLINES, Value);
  end;
end;

procedure TShellTree.SetShowRoot(Value: Boolean);
begin
  if ShowRoot <> Value then
  begin
    FShowRoot := Value;
    SetComCtlStyle(Self, TVS_LINESATROOT, Value);
  end;
end;

procedure TShellTree.SetRoot(Value: TShellNode);
var
  NewRoot: TShellTreeNode;
  Entry: TSHChangeNotifyEntry;
begin
  if Value = nil then Exit;
  if (FRoot = nil) or (not ILIsEqual(FRoot.AbsoluteList, Value.AbsoluteList)) then
  begin
    if FNotify <> 0 then
      SHChangeNotifyDeregister(FNotify);
    FNotify := 0;
    if FSelectedNode <> nil then
      FSelectedNode.Unlock;
    FSelectedNode := nil;
    NewRoot := TShellTreeNode.CreateFromList(ILClone(Value.AbsoluteList));
    if HandleAllocated then
      TreeView_DeleteAllItems(Handle);
    FRoot.Free;
    FRoot := NewRoot;
    FSelectedNode := FRoot;
    FSelectedNode.Lock;
    InsertNode(TShellTreeNode(FRoot));
    if HandleAllocated then
      TreeView_SelectItem(Handle, TShellTreeNode(FRoot).Handle);
    if FNotify <> 0 then
      SHChangeNotifyDeregister(FNotify);
    Entry.AbsoluteList := FRoot.AbsoluteList;
    Entry.WatchSubtree := True;
    FNotify := SHChangeNotifyRegister(Handle, SHCNRF_InterruptLevel or SHCNRF_ShellLevel or SHCNRF_RecursiveInterrupt,
      SHCNE_RENAMEFOLDER or SHCNE_MKDIR or SHCNE_RMDIR, WM_SHELLNOTIFY, 1, Entry);
  end;
end;

procedure TShellTree.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
  SetComCtlStyle(Self, TVS_EDITLABELS, not Value);
end;

function TShellTree.GetSelectedNode: TShellNode;
begin
  if HandleAllocated then
    Result := GetNode(TreeView_GetSelection(Handle))
  else
    Result := nil;
end;

procedure TShellTree.SetSelectedNode(Value: TShellNode);
var
  Node: TShellTreeNode;
begin
  if Value = nil then Exit;
  if FSelectedNode <> nil then
  begin
    if ILIsEqual(Value.AbsoluteList, FSelectedNode.AbsoluteList) then Exit;
    FSelectedNode.Unlock;
  end;
  if Value <> nil then
    Node := FindTreeNode(TShellTreeNode(FRoot), Value)
  else
    Node := nil;
  if Node <> nil then
  begin
    TreeView_SelectItem(Handle, Node.Handle);
    FSelectedNode := Node;
    FSelectedNode.Lock;
  end;
end;

procedure TShellTree.SetSpecialFolder(Value: TSpecialFolder);
begin
  if Value <> FSpecialFolder then
  begin
    FSpecialFolder := Value;
    TreeView_DeleteAllItems(Handle);
    FRoot.Free;
    FRoot := TShellTreeNode.CreateFromFolder(FSpecialFolder);
    InsertNode(TShellTreeNode(FRoot));
    SelectedNode := FRoot;
  end;
end;

{ TShellTree.IDropTarget }

function TShellTree.DropTargetDragEnter(const dataObj: IDataObject; grfKeyState: Longint;
  pt: TPoint; var dwEffect: Longint): HResult;
begin
  Result := S_OK;
end;

function TShellTree.DropTargetDragOver(grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HResult; stdcall;
var
  HitTest: TTVHitTestInfo;
  TreeItem: HTreeItem;
begin
  HitTest.pt := ScreenToClient(pt);
  TreeItem := TreeView_HitTest(Handle, HitTest);
  TreeView_SelectDropTarget(Handle, TreeItem);
  if TreeItem = nil then
    dwEffect := DROPEFFECT_NONE
  else if MK_CONTROL and grfKeyState = MK_CONTROL then
    dwEffect := DROPEFFECT_COPY
  else if MK_ALT and grfKeyState = MK_ALT then
    dwEffect := DROPEFFECT_LINK
  else
    dwEffect := DROPEFFECT_MOVE;
  Result := S_OK;
end;

function TShellTree.DropTargetDragLeave: HResult; stdcall;
begin
  TreeView_SelectDropTarget(Handle, nil);
  Result := S_OK;
end;

procedure GetDataObjectFiles(DataObject: IDataObject; Files: TStrings);
const
  DRAG_ACCPECT_ALL = $FFFFFFFF;
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  Name: array[0..MAX_PATH] of Char;
  I, J: Integer;
begin
  Files.Clear;
  FormatEtc.cfFormat := CF_HDROP;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;
  if DataObject.GetData(FormatEtc, StgMedium) = S_OK then
  try
    I := DragQueryFile(StgMedium.hGlobal, DRAG_ACCPECT_ALL, nil, 0);
    try
      for J := 0 to I - 1 do
      begin
        DragQueryFile(StgMedium.hGlobal, J, Name, SizeOf(Name));
        Files.Add(Name);
      end;
    finally
      DragFinish(StgMedium.hGlobal);
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end;

function TShellTree.DropTargetDrop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HResult; stdcall;
var
  HitTest: TTVHitTestInfo;
  TreeItem: HTreeItem;
  //Node: TShellTreeNode;
begin
  HitTest.pt := ScreenToClient(pt);
  TreeItem := TreeView_HitTest(Handle, HitTest);
  TreeView_SelectDropTarget(Handle, nil);
  if TreeItem = nil then
  begin
    Result := S_OK;
    Exit;
  end;
  //Node := GetNode(TreeItem);
  Result := S_OK;
end;

{ TShellTreePopupForm }

constructor TShellTreePopupForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShellTree := TShellTree.Create(Self);
  with FShellTree do
  begin
    Parent := Self;
    BorderStyle := bsNone;
    SelectedNode := Root;
    OnChange := ShellTreeChange;
    OnMouseMove := ShellTreeMouseMove;
    OnMouseUp := ShellTreeMouseUp;
  end;
  ForwardControl := FShellTree;
  AdjustControlSize;
end;

procedure TShellTreePopupForm.Popup;
begin
  FSelectChanged := False;
  FHoverNode := nil;
  inherited Popup;
end;

procedure TShellTreePopupForm.DoNodeHover(Node: TShellTreeNode);
begin
  if Node <> FHoverNode then
  begin
    FHoverNode := Node;
    if Assigned(FOnNodeHover) then
      FOnNodeHover(Self, Node);
  end;
end;

procedure TShellTreePopupForm.ShellTreeChange(Sender: TObject; Node: TShellNode);
begin
  FSelectChanged := True;
end;

procedure TShellTreePopupForm.ShellTreeMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Node: TShellTreeNode;
begin
  Node := FShellTree.GetNodeAt(X, Y);
  if Node <> nil then
    DoNodeHover(Node);
end;

procedure TShellTreePopupForm.ShellTreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FSelectChanged then
    Select;
end;

procedure TShellTreePopupForm.WMNCPaint(var Message: TWMNCPaint);
var
  DC: HDC;
  R: TRect;
begin
  DC := GetWindowDC(Handle);
  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  DrawRectOutline(DC, R, cl3DDkShadow);
  ReleaseDC(Handle, DC);
  Message.Result := 0;
end;

{ TShellEdit }

constructor TShellEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Images := ShellImageList;
  FShellTreePopupForm := TShellTreePopupForm.Create(Self);
  FShellTreePopupForm.OnNodeHover := TreeNodeHover;
end;

function TShellEdit.CreatePopup: TCustomPopupForm;
begin
  Result := FShellTreePopupForm;
end;

procedure TShellEdit.CreateWnd;
begin
  inherited CreateWnd;
  FShellTreePopupForm.Height := 125;
  SelectionChanged;
end;

procedure TShellEdit.TreeNodeHover(Sender: TObject; Node: TShellNode);
begin
  if Assigned(FOnNodeHover) then
    FOnNodeHover(Self, Node);
end;

procedure TShellEdit.SelectionChanged;
var
  Node: TShellTreeNode;
  Folder: IPersistFolder;
begin
  Node := TShellTreeNode(FShellTreePopupForm.FShellTree.SelectedNode);
  if Node <> nil then
  begin
    ImageIndex := Node.ImageIndex;
    if Node.FileSystem and ((FOptions * [soShortName] = []) or (ILGetCount(Node.AbsoluteList) < 3)) then
      Text := Node.Path
    else
      Text := Node.Name;
    if HandleAllocated then
      SelStart := 0;
    if FStrings <> nil then
      if Supports(FStrings, IPersistFolder, Folder) then
        Folder.Initialize(Node.AbsoluteList);
    if Binding <> nil then
      Binding.Change(Self, Node);
  end;
end;

procedure TShellEdit.DoPopup;
begin
  if not Sizeable then PopupForm.Width := Width;
  if FAutoComplete <> nil then
  begin
    FAutoComplete.Enable(False);
    HideSuggest;
  end;
  inherited DoPopup;
end;

procedure TShellEdit.DoCancel(Sender: TObject);
begin
  if FAutoComplete <> nil then
    FAutoComplete.Enable(True);
  inherited DoCancel(Sender);
end;

procedure TShellEdit.DoSelect(Sender: TObject);
begin
  SelectionChanged;
  if FAutoComplete <> nil then
    FAutoComplete.Enable(True);
  inherited DoSelect(Sender);
end;

procedure TShellEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Item: PItemIDList;
  Node, PriorNode: TShellNode;
  S: string;
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_RETURN then
  begin
    if SelectedNode = nil then
      S := ''
    else
      S := IncludeTrailingPathDelimiter(SelectedNode.Path);
    Item := ILCreateFromPath(S + Trim(Text));
    if Item = nil then
      Item := ILCreateFromPath(Text);
    if Item <> nil then
    begin
      Node := TShellNode.CreateFromList(Item);
      if Node.ShellFolder <> nil then
      begin
        PriorNode := SelectedNode;
        SelectedNode := Node;
        Node.Free;
        Node := nil;
        if SelectedNode <> PriorNode then
          if Assigned(OnSelect) then
            OnSelect(Self);
      end;
      Node.Free;
    end;
  end
  else if Key = VK_ESCAPE then
    SelectionChanged;
end;

function TShellEdit.GetSelectedNode: TShellNode;
begin
  Result := FShellTreePopupForm.FShellTree.SelectedNode;
end;

procedure TShellEdit.SetSelectedNode(Value: TShellNode);
begin
  FShellTreePopupForm.FShellTree.SelectedNode := Value;
  SelectionChanged;
end;

function TShellEdit.GetRoot: TShellNode;
begin
  Result := FShellTreePopupForm.FShellTree.Root;
end;

procedure TShellEdit.SetRoot(Value: TShellNode);
begin
  if Value = nil then Exit;
  FShellTreePopupForm.FShellTree.Root := Value;
  SelectionChanged;
end;

function TShellEdit.GetHotTrack: Boolean;
begin
  Result := FShellTreePopupForm.FShellTree.HotTrack;
end;

procedure TShellEdit.SetHotTrack(Value: Boolean);
begin
  FShellTreePopupForm.FShellTree.HotTrack := Value;
end;

procedure TShellEdit.SetOptions(Value: TShellEditOptions);

  function Includes(ParentSet, ChildSet: TShellEditOptions): Boolean;
  begin
    Result := ParentSet * ChildSet = ChildSet;
  end;

  function OptionChanged(ChildSet: TShellEditOptions): Boolean;
  begin
    Result := not ((Value * ChildSet = ChildSet) = (FOptions * ChildSet = ChildSet));
  end;

begin
  if Value <> FOptions then
  begin
    if OptionChanged([soSuggest]) then
      if Includes(Value, [soSuggest]) then
      begin
        FAutoComplete := CreateComObject(CLSID_AutoComplete) as IAutoComplete2;
        FStrings := CreateComObject(CLSID_ACListISF);
        OleCheck(FAutoComplete.SetOptions(ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST));
        OleCheck(FAutoComplete.Init(EditHandle, FStrings, nil, nil));
      end
      else
      begin
        HideSuggest;
        FAutoComplete.Enable(False);
        FAutoComplete.SetOptions(ACO_NONE);
        FAutoComplete := nil;
        FStrings := nil;
      end;
    FOptions := Value;
    SelectionChanged;
  end;
end;

function TShellEdit.GetStatusText: string;
begin
  Result := FShellTreePopupForm.StatusText;
end;

procedure TShellEdit.SetStatusText(Value: string);
begin
  FShellTreePopupForm.StatusText := Value;
end;

function TShellEdit.GetSpecialFolder: TSpecialFolder;
begin
  Result := FShellTreePopupForm.FShellTree.SpecialFolder;
end;

procedure TShellEdit.SetSpecialFolder(Value: TSpecialFolder);
begin
  FShellTreePopupForm.FShellTree.SpecialFolder := Value;
  SelectionChanged;
end;

{ TShellView }

const
  ViewModes: array[TShellViewMode] of UINT = (
    FVM_ICON, FVM_SMALLICON, FVM_LIST, FVM_DETAILS, FVM_THUMBNAIL,
    FVM_TILE, FVM_THUMBSTRIP);

constructor TShellView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  TabStop := True;
  ParentColor := False;
  Color := clWindow;
  FAllowContextMenu := True;
  FShellBrowser := Self;
  FListViewInstance := MakeObjectInstance(ListViewWndProc);
  FShellViewInstance := MakeObjectInstance(ShellViewWndProc);
  FDefaultKeys := True;
  FRoot := TShellNode.CreateFromFolder(FSpecialFolder);
end;

destructor TShellView.Destroy;
begin
  if FShellView <> nil then
  begin
    SetWindowLong(FShellViewHandle, GWL_WNDPROC, Longint(FDefShellViewProc));
    SetWindowLong(FListViewHandle, GWL_WNDPROC, Longint(FDefListViewProc));
    FShellView.DestroyViewWindow;
    FShellView := nil;
  end;
  FStream := nil;
  FParentRoot.Free;
  FRoot.Free;
  FreeObjectInstance(FListViewInstance);
  FreeObjectInstance(FShellViewInstance);
  inherited Destroy;
end;

procedure TShellView.CreateWnd;
var
  Node: TShellNode;
begin
  inherited CreateWnd;
  Node := TShellNode.CreateFromFolder(FSpecialFolder);
  try
    Root := Node;
  finally
    Node.Free;
  end;
end;

procedure TShellView.Explore(const Path: string);
var
  Node: TShellNode;
begin
  HandleNeeded;
  Node := TShellNode.CreateFromList(ILCreateFromPath(Path));
  try
    Root := Node;
  finally
    Node.Free;
  end;
end;

procedure TShellView.Resize;
begin
  if FShellView = nil then
    Invalidate;
end;

procedure TShellView.KeyDown(var Key: Word; Shift: TShiftState);
var
  Wnd: HWND;
  Editing: Boolean;
begin
  inherited KeyDown(Key, Shift);
  Wnd := ListView_GetEditControl(FListViewHandle);
  Editing := (Wnd <> 0) and IsWindowVisible(Wnd);
  if Editing then Exit;
  if FDefaultKeys then
    case Key of
      VK_BACK: Up;
      VK_F2: Rename;
      VK_F5: Refresh;
      Ord('X'): if ssCtrl in Shift then Cut;
      Ord('C'): if ssCtrl in Shift then Copy;
      Ord('V'): if ssCtrl in Shift then Paste;
      VK_DELETE: Delete;
    end;
end;

procedure TShellView.DoChange;
var
  N: TShellNode;
begin
  N := FRoot;
  if Binding <> nil then
    Binding.Change(Self, N);
  if Assigned(FOnChange) then
    FOnChange(Self, N);
end;

procedure TShellView.DoViewChanged;
begin
  if Assigned(FOnViewChanged) then
    FOnViewChanged(Self);
end;

procedure TShellView.ListViewWndProc(var Message: TMessage);
var
  Form: TCustomForm;
begin
  with Message do
  try
    case Msg of
      WM_SETFOCUS:
        begin
          Form := GetParentForm(Self);
          if (Form <> nil) and Enabled and (not Form.SetFocusedControl(Self))
            then Exit;
        end;
      WM_KILLFOCUS:
        if csFocusing in ControlState then Exit;
      WM_KEYDOWN, WM_SYSKEYDOWN:
        if DoKeyDown(TWMKey(Message)) then Exit;
      WM_CHAR:
        if DoKeyPress(TWMKey(Message)) then Exit;
      WM_KEYUP, WM_SYSKEYUP:
        if DoKeyUp(TWMKey(Message)) then Exit;
      WM_NCHITTEST:
        if csDesigning in ComponentState then
        begin
          Result := HTTRANSPARENT;
          Exit;
        end;
      WM_MOUSEMOVE:
        begin
          with TWMMouseMove(Message) do MouseMove(KeysToShiftState(Keys),
            XPos, YPos);
          Application.HintMouseMessage(Self, Message);
        end;
      WM_CONTEXTMENU:
        if not FAllowContextMenu then
        begin
          if PopupMenu <> nil then
            PopupMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
          Exit;
        end;
      CN_KEYDOWN, CN_CHAR, CN_SYSKEYDOWN, CN_SYSCHAR:
        begin
          WndProc(Message);
          Exit;
        end;
    end;
    Result := CallWindowProc(FDefListViewProc, FListViewHandle, Msg, WParam, LParam);
    if (Msg = WM_LBUTTONDBLCLK) and (csDoubleClicks in ControlStyle) then
      DblClick;
  except
    HandleExternalException(Self);
  end;
end;

procedure TShellView.ShellViewWndProc(var Message: TMessage);
const
  DefaultMenu = $7900;
var
  MenuItemInfo: TMenuItemInfo;
begin
  with Message do
  try
    case Message.Msg of
      WM_NCHITTEST:
        if csDesigning in ComponentState then
        begin
          Result := HTTRANSPARENT;
          Exit;
        end;
      WM_INITMENUPOPUP:
        with TWMInitMenuPopup(Message) do
          if GetMenuItemID(MenuPopup, 0) = DefaultMenu then
          begin
            RemoveMenu(MenuPopup, 0, MF_BYPOSITION);
            RemoveMenu(MenuPopup, 0, MF_BYPOSITION);
            FillChar(MenuItemInfo, SizeOf(TMenuItemInfo), #0);
            with MenuItemInfo do
            begin
              cbSize := SizeOf(TMenuItemInfo);
              fMask := MIIM_STATE or MIIM_ID;
              fState := MFS_DEFAULT;
              wID := DefaultMenu;
            end;
            SetMenuItemInfo(MenuPopup, 0, True, MenuItemInfo);
          end;
    end;
    Result := CallWindowProc(FDefShellViewProc, FShellViewHandle, Msg, WParam,
      LParam);
  except
    HandleExternalException(Self);
  end;
end;

function TShellView.GetFocusedNode: TShellNode;
var
  F: IFolderView;
  L: PItemIDList;
  I: Integer;
begin
  Result := nil;
  if FShellView = nil then
    Exit;
  if Supports(FShellView, IFolderView, F) and (F.GetFocusedItem(I) = S_OK) and
    (F.Item(I, L) = S_OK) and (L <> nil) then
  begin
    if FFocusedNode = nil then
      FFocusedNode := TShellNode.Create(FRoot.Clone, L)
    else if ILIsEqual(FFocusedNode.RelativeList, L) then
      ILFree(L)
    else
    begin
      FFocusedNode.Free;
      FFocusedNode := nil;
      FFocusedNode := TShellNode.Create(FRoot.Clone, L);
    end;
    Result := FFocusedNode;
  end;
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

procedure TShellView.GetItems(Strings: TStrings; Selection: Boolean = True; FilePath: Boolean = False);
var
  View: IFolderView;
  Enum: IEnumIDList;
  Target: DWORD;
  A, B: PItemIDList;
  ItemPath: array[0..MAX_PATH] of Char;
  I: Cardinal;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    if ShellView.QueryInterface(IFolderView, View) <> S_OK then Exit;
    if Selection then
      Target := SVGIO_SELECTION
    else
      Target := SVGIO_ALLVIEW;
    if View.Items(Target, IEnumIDList, Enum) <> S_OK then Exit;
    while Enum.Next(1, A, I) = S_OK do
    begin
      if I <> 1 then Exit;
      B := ILCombine(Root.AbsoluteList, A);
      if FilePath then
        if SHGetPathFromIDList(B, ItemPath) then
          Strings.Add(ItemPath)
        else
      else
        Strings.Add(GetDisplayName(B));
      CoTaskMemFree(A);
      CoTaskMemFree(B);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TShellView.ExecuteVerb(const Verb: string);
var
  View: IFolderView;
  Enum: IEnumIDList;
  Item: PItemIDList;
  List: array of PItemIDList;
  ContextMenu: IContextMenu;
  CommandInfo: TCMInvokeCommandInfo;
  D: IShellFolder;
  S: AnsiString;
  C, I: Cardinal;
begin
  S := UpperCase(Trim(Verb));
  if S = '' then Exit;
  if S = 'RENAME' then
  begin
    Rename;
    Exit;
  end;
  if ShellView.QueryInterface(IFolderView, View) <> S_OK then Exit;
   if View.Items(SVGIO_SELECTION, IEnumIDList, Enum) <> S_OK then Exit;
    C := 0;
  while Enum.Next(1, Item, I) = S_OK do
   begin
    if I <> 1 then Exit;
    CoTaskMemFree(Item);
    Inc(C);
   end;
  SetLength(List, C);
  Enum.Reset;
  C := 0;
  while Enum.Next(1, Item, I) = S_OK do
  begin
    List[C] := Item;
    Inc(C);
  end;
  if C > 0 then
    OleCheck(Root.ShellFolder.GetUIObjectOf(Handle, C, List[0], IID_IContextMenu,
      nil, ContextMenu))
  else
  begin
    Item := Root.AbsoluteList;
    SHGetDesktopFolder(D);
    OleCheck(D.GetUIObjectOf(Handle, 1, Item, IID_IContextMenu,
      nil, ContextMenu));
  end;
  FillChar(CommandInfo, SizeOf(TCMInvokeCommandInfo), #0);
  with CommandInfo do
  begin
    cbSize := SizeOf(TCMInvokeCommandInfo);
    hwnd := Handle;
    lpVerb := PAnsiChar(S);
    fMask :=  CMF_EXPLORE;
    nShow := SW_SHOW;
  end;
  ContextMenu.InvokeCommand(CommandInfo);
  if C = 0 then Exit;
  for I := 0 to C - 1 do
    CoTaskMemFree(List[I]);
end;

procedure TShellView.Refresh;
begin
  LockWindowUpdate(FListViewHandle);
  FShellView.Refresh;
  LockWindowUpdate(0);
end;

procedure TShellView.Rename;
var
  I: Integer;
begin
  I := ListView_GetSelectedCount(FListViewHandle);
  if I > 0 then
  begin
    I := ListView_GetNextItem(FListViewHandle, -1, LVNI_SELECTED);
    ListView_EnsureVisible(FListViewHandle, I, False);
    ListView_EditLabelA(FListViewHandle, I);
  end;
end;

procedure TShellView.Copy;
begin
  ExecuteVerb('Copy');
end;

procedure TShellView.Cut;
begin
  ExecuteVerb('Cut');
end;

procedure TShellView.Paste;
begin
  ExecuteVerb('Paste');
end;

procedure TShellView.Delete;
begin
  ExecuteVerb('Delete');
end;

procedure TShellView.Properties;
begin
  ExecuteVerb('Properties');
end;

procedure TShellView.Up;
var
  Node: TShellNode;
  L: PItemIDList;
begin
  if ILIsRoot(FRoot.AbsoluteList) then Exit;
  L := ILClone(FRoot.AbsoluteList);
  if ILRemoveLastID(L) then
    Node := TShellNode.CreateFromList(L)
  else
    Node := TShellNode.CreateFromList(nil);
  Root := Node;
  Node.Free;
end;

procedure TShellView.SetParentRoot(Value: TShellNode);
var
  N: TShellNode;
begin
  FParentRoot.Free;
  if Value <> nil then
  begin
    FParentRoot := TShellNode.CreateFromList(ILClone(Value.AbsoluteList));
    if FRoot <> nil then
    begin
      N := TShellNode.CreateFromList(ILClone(FRoot.AbsoluteList));
      try
        Root := N;
      finally
        N.Free;
      end;
    end;
  end;
end;

procedure TShellView.SetRoot(Value: TShellNode);
var
  AllowAction: Boolean;
  WasFocused: Boolean;
  FolderSettings: TFolderSettings;
  ViewRect: TRect;
  OldView: IShellView;
  OldRoot: TShellNode;
  Browser: IShellBrowser;
  Edge: Integer;
begin
  if Value = nil then Exit;
  if Value.ShellFolder = nil then Exit;
  if FParentRoot <> nil then
    if not ILIsParent(FParentRoot.AbsoluteList, Value.AbsoluteList, False) then
      Value := ParentRoot;
  if IsWindow(FShellViewHandle) and ILIsEqual(FRoot.AbsoluteList, Value.AbsoluteList) then Exit;
  AllowAction := True;
  if Assigned(FOnDefaultAction) then
    FOnDefaultAction(Self, Value, AllowAction);
  if not AllowAction then Exit;
  OldRoot := nil;
  if HandleAllocated and (Value <> FRoot) then
  try
    OldRoot := FRoot;
    LockWindowUpdate(Handle);
    try
      FRoot := Value.Clone(TShellNode);
      Visible := True;
      FFocusedNode.Free;
      FFocusedNode := nil;
      WasFocused := IsChild(Handle, Windows.GetFocus);
      OldView := FShellView;
      if OldView <> nil then
      begin
        OldView.GetCurrentInfo(FolderSettings);
        SetWindowLong(FShellViewHandle, GWL_WNDPROC, Longint(FDefShellViewProc));
        SetWindowLong(FListViewHandle, GWL_WNDPROC, Longint(FDefListViewProc));
      end
      else with FolderSettings do
      begin
        ViewMode := ViewModes[FViewMode];
        fFlags := FWF_NOCLIENTEDGE;
      end;
      OleCheck(CreateStreamOnHGlobal(GlobalAlloc(GMEM_MOVEABLE or
        GMEM_DISCARDABLE, 0), True, FStream));
      OleCheck(FRoot.ShellFolder.CreateViewObject(Handle,
        IID_IShellView, FShellView));
      Edge := 0;
      ViewRect := Rect(Edge, Edge, ClientWidth - Edge, ClientHeight - Edge);
      Browser := FShellBrowser;
      OleCheck(FShellView.CreateViewWindow(OldView, FolderSettings, Browser,
        ViewRect, FShellViewHandle));
      FShellView.UIActivate(1);
      FListViewHandle := FindWindowEx(FShellViewHandle, 0, WC_LISTVIEW, nil);
      FDefListViewProc := Pointer(GetWindowLong(FListViewHandle, GWL_WNDPROC));
      SetWindowLong(FListViewHandle, GWL_WNDPROC, Longint(FListViewInstance));
      FDefShellViewProc := Pointer(GetWindowLong(FShellViewHandle, GWL_WNDPROC));
      SetWindowLong(FShellViewHandle, GWL_WNDPROC, Longint(FShellViewInstance));
      if OldView <> nil then
      begin
        OldView.UIActivate(0);
        OldView.DestroyViewWindow;
      end;
      if FListViewHandle <> 0 then
      begin
        UpdateTextColor;
        UpdateTextBkColor;
      end;
      if WasFocused then
        Windows.SetFocus(FShellViewHandle);
      OldRoot.Free;
      OldRoot := nil;
    finally
      LockWindowUpdate(0);
    end;
    Invalidate;
    UpdateWindow(Handle);
  except
    Root := OldRoot;
    OldRoot.Free;
    raise;
  end;
  DoChange;
end;

procedure TShellView.SetSpecialFolder(Value: TSpecialFolder);
var
  Node: TShellNode;
begin
  if Value <> FSpecialFolder then
  begin
    FSpecialFolder := Value;
    Node := TShellNode.CreateFromFolder(FSpecialFolder);
    try
      Root := Node;
    finally
      Node.Free;
    end;
  end;
end;

procedure TShellView.SetViewMode(Value: TShellViewMode);
var
  Changed: Boolean;
  F: IFolderView;
begin
  Changed := Value <> FViewmode;
  FViewMode := Value;
  if FShellView <> nil then
  begin
    if Supports(FShellView, IFolderView, F) then
      F.SetCurrentViewMode(ViewModes[Value]);
  end;
  if Changed then
    DoViewChanged;
end;

procedure TShellView.UpdateTextColor;
begin
  ListView_SetTextColor(FListViewHandle, ColorToRGB(Font.Color));
  InvalidateRect(FListViewHandle, nil, True);
end;

procedure TShellView.UpdateTextBkColor;
begin
  ListView_SetTextBkColor(FListViewHandle, ColorToRGB(Color));
  ListView_SetBkColor(FListViewHandle, ColorToRGB(Color));
  InvalidateRect(FListViewHandle, nil, True);
end;

{ TShellView.IOleWindow }

function TShellView.GetWindow(out wnd: HWnd): HResult;
begin
  Wnd := Handle;
  Result := S_OK;
end;

function TShellView.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := S_OK;
end;

{ TShellView.IShellBrowser }

function TShellView.InsertMenusSB(hMenuShared: HMENU;
  out MenuWidths: TOleMenuGroupWidths): HResult;
begin
  Result := S_OK;
end;

function TShellView.SetMenuSB(hMenuShared: HMENU;
  hOleMenuReserved: HOLEMENU; hwndActiveObject: HWND): HResult;
begin
  Result := S_OK;
end;

function TShellView.RemoveMenusSB(hMenuShared: HMENU): HResult;
begin
  Result := S_OK;
end;

function TShellView.SetStatusTextSB(StatusText: POleStr): HResult;
begin
  Result := S_OK;
end;

function TShellView.EnableModelessSB(Enable: BOOL): HResult;
begin
  Result := S_OK;
end;

function TShellView.TranslateAcceleratorSB(Msg: PMsg; ID: Word): HResult;
begin
  Result := S_OK;
end;

function TShellView.BrowseObject(pidl: PItemIDList; flags: UINT): HResult;
begin
  Result := S_OK;
end;

function TShellView.GetViewStateStream(Mode: DWORD; out Stream: IStream): HResult;
begin
  Stream := FStream;
  Result := S_OK;
end;

function TShellView.GetControlWindow(ID: UINT; out Wnd: HWND): HResult;
begin
  Wnd := 0;
  case ID of
    FCW_STATUS:
      if FStatusBar <> nil then
        Wnd := FStatusBar.Handle;
    { FCW_TREE:
      if FShellTree <> nil then
        Wnd := FShellTree.Handle; }
  end;
  Result := S_OK;
end;

function TShellView.SendControlMsg(ID, Msg: UINT; wParm: WPARAM; lParm: LPARAM;
  var Rslt: LResult): HResult;
begin
  Result := S_OK;
end;

function TShellView.QueryActiveShellView(var ShellView: IShellView): HResult;
begin
  Pointer(ShellView) := Pointer(FShellView);
  FShellView._AddRef;
  Result := S_OK;
end;

function TShellView.OnViewWindowActive(var ShellView: IShellView): HResult;
begin
  Result := S_OK;
end;

function TShellView.SetToolbarItems(TBButton: PTBButton;
  nButtons, uFlags: UINT): HResult;
begin
  Result := S_OK;
end;

{ TShellView.ICommDlgBrowser }

function TShellView.OnDefaultCommand(const ppshv: IShellView): HResult;
var
  AllowAction: Boolean;
  Node: TShellNode;
begin
  AllowAction := True;
  try
    Node := GetFocusedNode;
    if Node <> nil then
    try
      Node := Node.Clone;
      if FDefaultKeys then
      begin
        if GetKeyState(VK_MENU) > -1 then
        begin
          AllowAction := False;
          if Node.ShellFolder <> nil then
            Root := Node
          else
            AllowAction := True;
        end;
      end;
      if Assigned(FOnDefaultAction) then
        FOnDefaultAction(Self, Node, AllowAction);
    finally
      Node.Free;
    end;
  except
    HandleExternalException(Self);
  end;
  if AllowAction then
    Result := S_FALSE
  else
    Result := S_OK;
end;

function TShellView.OnStateChange(const ppshv: IShellView; Change: ULONG): HResult;
begin
  if Change and CDBOSC_SELCHANGE = CDBOSC_SELCHANGE then
  try
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self);
  except
    HandleExternalException(Self);
  end;
  Result := S_OK;
end;

function TShellView.IncludeObject(const ppshv: IShellView; pidl: PItemIDList): HResult;
var
  AllowAction: Boolean;
  Node: TShellNode;
begin
  AllowAction := True;
  if Assigned(FOnIncludeItem) then
  try
    Node := TShellNode.Create(FRoot.Clone, ILClone(pidl));
    try
      FOnIncludeItem(Self, Node, AllowAction);
    finally
      Node.Free;
    end;
  except
    HandleExternalException(Self);
  end;
  if AllowAction then
    Result := S_OK
  else
    Result := S_FALSE;
end;

{ TShellView.ICommDlgBrowser2 }

function TShellView.Notify(ppshv: IShellView; dwNotifyType: DWORD): HResult;
var
  View: TShellViewMode;
  F: IFolderView;
  M: UINT;
begin
  Result := S_OK;
  case dwNotifyType of
  CDB2N_CONTEXTMENU_DONE:
    if Supports(ppshv, IFolderView, F) and (F.GetCurrentViewMode(M) = S_OK) and
      (M <> ViewModes[FViewMode]) then
    for View := Low(View) to High(View) do
      if ViewModes[View] = M then
      begin
        FViewMode := View;
        try
          DoViewChanged;
        except
          HandleExternalException(Self);
        end;
        Break;
      end;
  end;
end;

function TShellView.GetDefaultMenuText(ppshv: IShellView; pszText: PWideChar;
  cchMax: Integer): HResult;
begin
  FillChar(pszText^, 2, #0);
  Result := S_OK;
end;

function TShellView.GetViewFlags(out pdwFlags: DWORD): HResult;
begin
  pdwFlags := CDB2GVF_SHOWALLFILES;
  Result := S_OK;
end;

function TShellView.OnColumnClicked(ppshv: IShellView; iColumn: Integer): HResult;
begin
  Result := S_OK;
end;

function TShellView.GetCurrentFilter(pszFileSpec:  PWideChar; cchFileSpec: Integer): HResult;
begin
  Result := S_OK;
end;

function TShellView.OnPreViewCreated(ppshv: IShellView): HResult;
begin
  Result := S_OK;
end;

{ Our folders }

const
  SID_SShellBrowser: TGUID = '{000214E2-0000-0000-C000-000000000046}';
  SID_STopLevelBrowser: TGUID = '{4C96BE40-915C-11CF-99D3-00AA004AE837}';
  SID_STopWindow: TGUID = '{49E1B500-4636-11D3-97F7-00C04F45D0B3}';

function TShellView.SRemoteQueryService(const guidService, riid: TGUID;
  out ppvObject: IUnknown): HResult;
begin
  if IsEqualGUID(guidService, SID_STopLevelBrowser) then
    Result := FShellBrowser.QueryInterface(riid, ppvObject)
  else
    Result := E_FAIL;
  {if @ShellDebug <> nil then
  begin
    ShellDebug(Self, guidService);
    ShellDebug(Self, riid);
  end;}
end;

procedure TShellView.CMColorChanged(var Message: TMessage);
begin
  inherited;
  if FListViewHandle <> 0 then UpdateTextBkColor;
end;

procedure TShellView.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if FListViewHandle <> 0 then
    UpdateTextColor;
end;

procedure TShellView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TShellView.WMGetIShellBrowser(var Message: TMessage);
begin
  if TShellTreeNode(FRoot).FileSystem then
    Message.Result := Integer(FShellBrowser)
  else
    Message.Result := 0;
end;

procedure TShellView.WMSize(var Message: TWMSize);
begin
  inherited;
  if FShellViewHandle <> 0 then
    with Message do
    begin
      SetWindowPos(FShellViewHandle, 0, 0, 0, Width,
        Height, SWP_NOZORDER or SWP_NOACTIVATE);
    end;
end;

procedure TShellView.WMHScroll(var Message: TMessage);
begin
  if FListViewHandle <> 0 then
    with Message do
      Result := SendMessage(FListViewHandle, Msg, WParam, LParam);
end;

procedure TShellView.WMVScroll(var Message: TMessage);
begin
  if FListViewHandle <> 0 then
    with Message do
      Result := SendMessage(FListViewHandle, Msg, WParam, LParam);
end;

procedure TShellView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if FListViewHandle <> 0 then
    Windows.SetFocus(FListViewHandle);
    {with Message do
      Result := SendMessage(FListViewHandle, Msg, WParam, LParam);}
end;

{ TShellBubbleItem }

constructor TShellBubbleItem.Create(Collection: TCollection);
begin
  CreateFromFolder(Collection, sfDesktop);
end;

constructor TShellBubbleItem.CreateFromFolder(Collection: TCollection;
  SpecialFolder: TSpecialFolder);
begin
  inherited Create(Collection);
  FNode := TShellImageNode.CreateFromFolder(SpecialFolder);
  FSpecialFolder := SpecialFolder;
end;

destructor TShellBubbleItem.Destroy;
begin
  FNode.Free;
  inherited Destroy;
end;

procedure TShellBubbleItem.Assign(Source: TPersistent);
var
  Item: TShellBubbleItem absolute Source;
  HasChanged: Boolean;
begin
  if Source is TShellBubbleItem then
  begin
    HasChanged := not FNode.IsEqual(Item.Node);
    if HasChanged then
    begin
      FNode.Free;
      FNode := TShellImageNode.CreateFromList(ILClone(Item.Node.AbsoluteList));
    end;
    FPath := Item.Path;
    FSpecialFolder := Item.SpecialFolder;
    if HasChanged then
      TShellBubbleItems(Collection).Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TShellBubbleItem.SetPath(const Value: string);
var
  pidl: PItemIDList;
begin
  if FPath <> Value then
  begin
    FPath := Value;
    FNode.Free;
    if FPath <> '' then
      pidl := ILCreateFromPath(FPath)
    else
      pidl := nil;
    if pidl = nil then
      FNode := TShellImageNode.CreateFromFolder(FSpecialFolder)
    else
      FNode := TShellImageNode.CreateFromList(pidl);
    TShellBubbleItems(Collection).Changed;
  end;
end;

procedure TShellBubbleItem.SetSpecialFolder(Value: TSpecialFolder);
var
  pidl: PItemIDList;
begin
  if FSpecialFolder <> Value then
  begin
    FSpecialFolder := Value;
    if FPath <> '' then
      pidl := ILCreateFromPath(FPath)
    else
      pidl := nil;
    if pidl = nil then
    begin
      FNode.Free;
      FNode := TShellImageNode.CreateFromFolder(FSpecialFolder);
      TShellBubbleItems(Collection).Changed;
    end
    else
      ILFree(pidl);
  end;
end;

{ TShellBubbleItems }

constructor TShellBubbleItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TShellBubbleItem);
end;

function TShellBubbleItems.Add: TShellBubbleItem;
begin
  Result := TShellBubbleItem.Create(Self);
end;

function TShellBubbleItems.AddFolder(SpecialFolder: TSpecialFolder): TShellBubbleItem;
begin
  Result := TShellBubbleItem.CreateFromFolder(Self, SpecialFolder);
end;

procedure TShellBubbleItems.Assign(Source: TPersistent);
var
  Items: TShellBubbleItems absolute Source;
  I: Integer;
begin
  if Source is TShellBubbleItems then
  begin
    BeginUpdate;
    Clear;
    for I := 0 to Items.Count - 1 do
      Add.Assign(Items[I]);
    EndUpdate;
  end
  else
    inherited Assign(Source);
end;

procedure TShellBubbleItems.Update(Item: TCollectionItem);
begin
  if GetOwner is TControl then
    (GetOwner as TControl).Update;
end;

function TShellBubbleItems.GetItem(Index: Integer): TShellBubbleItem;
begin
  Result := TShellBubbleItem(inherited Items[Index]);
end;

procedure TShellBubbleItems.SetItem(Index: Integer; Value: TShellBubbleItem);
begin
  GetItem(Index).Assign(Value);
end;

{ TShellBubbles }

constructor TShellBubbles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TShellBubbleItems.Create(Self);
  Style := sbLarge;
  HotTrack := True;
  Width := 79;
  Height := 204;
end;

destructor TShellBubbles.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TShellBubbles.Click;
var
  N: TShellNode;
begin
  if Clicked then
  begin
    N := Items[ItemIndex].Node;
    if Binding <> nil then
      Binding.Change(Self, N);
    inherited Click;
  end;
end;

procedure TShellBubbles.CreateWnd;
begin
  inherited CreateWnd;
  if (csDesigning in ComponentState) and (Items.Count = 0) then
  begin
    Items.AddFolder(sfRecent);
    Items.AddFolder(sfDesktop);
    Items.AddFolder(sfPersonal);
    Items.AddFolder(sfDrives);
  end;
end;

procedure TShellBubbles.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  Images: TCustomImageList;
  DrawRect: TRect;
  H, W: Integer;
  S: TSize;
  I: Integer;
begin
  inherited DrawItem(Index, Rect, State);
  Images := ShellImageList(FStyle = sbSmall);
  DrawRect := Rect;
  H := HeightOf(Rect);
  W := WidthOf(Rect);
  I := ShellImageList(FStyle = sbSmall).Height;
    case FStyle of
      sbSmall:
        if FTextLabels then
        begin
          ImageListDraw(Images, Canvas, DrawRect.Left + (H - I) div 2,
            DrawRect.Top + (H - I) div 2, Items[Index].Node.SmallIndex, Enabled);
          Inc(DrawRect.Left, H);
          Dec(DrawRect.Right, 5);
          DrawCaption(Canvas.Handle, Items[Index].Node.Name, DrawRect, drLeft, Enabled);
        end
        else
          ImageListDraw(Images, Canvas, DrawRect.Left + (W - I) div 2,
            DrawRect.Top + (H - I) div 2, Items[Index].Node.SmallIndex, Enabled);
      sbLarge:
        if FTextLabels then
        begin
          S := CalcCaptionSize(Canvas.Handle, ' ');
          ImageListDraw(Images, Canvas, DrawRect.Left + (W - I) div 2,
            DrawRect.Top + (H - I) div 2 - S.cY, Items[Index].Node.LargeIndex, Enabled);
          DrawRect.Top := DrawRect.Top + (H - I) div 2 + I - 10;
          DrawCaption(Canvas.Handle, Items[Index].Node.Name, DrawRect, drWrap, Enabled);
        end
        else
          ImageListDraw(Images, Canvas, DrawRect.Left + (W - I) div 2,
            DrawRect.Top + (H - I) div 2, Items[Index].Node.LargeIndex, Enabled);
    end;
end;

procedure TShellBubbles.Update;
begin
  inherited Update;
  if not (csDestroying in ComponentState) then
  begin
    Count := FItems.Count;
    Invalidate;
  end;
end;

procedure TShellBubbles.SetItems(Value: TShellBubbleItems);
begin
  if Value <> FItems then
    FItems.Assign(Value);
end;

const
  LargeHeights: array[Boolean] of Integer = (50 - 32, 75 - 32);

procedure TShellBubbles.SetStyle(Value: TShellBubbleStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    case FStyle of
      sbSmall: ItemHeight := 25;
      sbLarge: ItemHeight := ShellImageList(False).Height + LargeHeights[FTextLabels];
    end;
  end;
end;

procedure TShellBubbles.SetTextLabels(Value: Boolean);
begin
  if Value <> FTextLabels then
  begin
    FTextLabels := Value;
    if FStyle = sbLarge then
      ItemHeight := ShellImageList(False).Height + LargeHeights[FTextLabels];
    Invalidate;
  end;
end;

{ TShellListBox }

type
  IShellListItem = interface(IInterface)
    ['{A8BA3B5F-9FBF-4879-B71F-27C16299B60F}']
    function GetFolder: Boolean;
    function GetImageIndex: Integer;
    function GetName: string;
    function GetPath: string;
    property Folder: Boolean read GetFolder;
    property ImageIndex: Integer read GetImageIndex;
    property Name: string read GetName;
    property Path: string read GetPath;
  end;

  {TShellListItem = class(TInterfacedObject, IShellListItem)
  protected
    function GetFolder: Boolean;
    function GetImageIndex: Integer;
    function GetName: string;
    function GetPath: string;
    constructor Create()
  end;}

procedure TSmallShellNode.Initialize;
const
  Flags = SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_PIDL;
var
  SHFileInfo: TSHFileInfo;
  ItemList: PItemIDList;
  FileFlag: Longword;
begin
  inherited Initialize;
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo), Flags);
  FImageIndex := SHFileInfo.iIcon;
  FillChar(SHFileInfo, SizeOf(TSHFileInfo), #0);
  SHGetFileInfo(PChar(AbsoluteList), 0, SHFileInfo, SizeOf(TSHFileInfo),
    Flags or SHGFI_OPENICON);
  FSelectedIndex := SHFileInfo.iIcon;
  if Parent <> nil then
  begin
    ItemList := RelativeList;
    FileFlag := SFGAO_FILESYSTEM;
    Parent.ShellFolder.GetAttributesOf(1, ItemList, FileFlag);
    FFileSystem := FileFlag and SFGAO_FILESYSTEM = SFGAO_FILESYSTEM;
  end
end;

function TSmallShellNode.EnumFlags: Longword;
begin
  Result := SHCONTF_FOLDERS or SHCONTF_INCLUDEHIDDEN;
  if FShowFiles then
    Result := Result or SHCONTF_NONFOLDERS;
end;

procedure TSmallShellNode.Rename(Node: TShellNode);
begin
  inherited Rename(Node);
  Initialize;
end;

function TSmallShellNode.GetFilePath: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if FFileSystem and SHGetPathFromIDList(AbsoluteList, Buffer) then
    Result := Buffer
  else
    Result := '';
end;

{ TShellListBox }

constructor TShellListBox.Create(AOwner: TComponent);
var
  N: TShellNode;
  A, B: Integer;
begin
  inherited Create(AOwner);
  Color := clMenu;
  DoubleBuffered := True;
  BorderStyle := bsNone;
  HotTrack := True;
  Images := TSmallShellImages.Create(Self);
  A := FontHeight(Font) + 4;
  B := Images.Height + 4;
  if A > B then
    ItemHeight := A
  else
    ItemHeight := B;
  FItems := TList.Create;
  FShowFiles := True;
  N := TSmallShellNode.CreateFromList(nil);
  Root := N;
  N.Free;
end;

destructor TShellListBox.Destroy;
begin
  FItems.Free;
  FRoot.Free;
  inherited Destroy;
end;

procedure TShellListBox.DrawBackground;
var
  DC: HDC;
  R: TRect;
begin
  DC := Canvas.Handle;
  R := ClientRect;
  R.Right := R.Left + 25;
  FillRectColor(DC, R, clMenuBar);
  R.Left := R.Right;
  R.Right := ClientWidth;
  FillRectColor(DC, R, Color);
end;

procedure TShellListBox.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
var
  DC: HDC;
  N: TSmallShellNode;
  R: TRect;
begin
  DC := Canvas.Handle;
  N := TSmallShellNode(FItems[Index]);
  R := Rect;
  if dsHot in State then
  begin
    DrawMenuHighlightRect(DC, R);
    R.Right := R.Left + 25;
    InflateRect(R, -3, -3);
    FillRectColor(DC, R, clHighlight);
  end;
  R := Rect;
  R.Right := R.Left + 25;
  ImageListDraw(Images, Canvas, R, N.ImageIndex, []);
  R.Left := R.Right;
  R.Right := Rect.Right;
  InflateRect(R, -4, 0);
  DrawCaption(DC, N.Name, R, drLeft);
end;

procedure TShellListBox.SetRoot(Value: TShellNode);
var
  N: TSmallShellNode;
  I: Integer;
begin
  if Value = nil then Exit;
  if (FRoot = nil) or (not ILIsEqual(FRoot.AbsoluteList, Value.AbsoluteList)) then
  begin
    FRoot.Free;
    N := TSmallShellNode.CreateFromList(ILClone(Value.AbsoluteList));
    N.ShowFiles := FShowFiles;
    FRoot := N;
    FItems.Clear;
    for I := 0 to FRoot.Count - 1 do
    begin
      N := TSmallShellNode(FRoot.Item[I]);
      FItems.Add(N);
    end;
    Count := FItems.Count;
  end;
end;

procedure TShellListBox.SetShowFiles(Value: Boolean);
var
  N: TShellNode;
begin
  if Value <> FShowFiles then
  begin
    N := FRoot;
    Root := N;
    N.Free;
  end;
end;

function TShellListBox.GetSelectedNode: TShellNode;
begin
  Result := nil;
  if ItemIndex > -1 then
    Result := TShellNode(FItems[ItemIndex]);
end;

procedure TShellListBox.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if Message.WheelDelta < 0 then
    TopIndex := TopIndex + 1
  else
    TopIndex := TopIndex - 1;
  Message.Result := 1;
end;

{ TShellPathEditBar }

procedure BuildShellMenu(Menu: TScrollingMenu; Node: TSmallShellNode; Default: TShellNode; ShowFiles: Boolean = False);
var
  Item: TScrollingMenuItem;
  N: TSmallShellNode;
  I: Integer;
begin
  Node.ShowFiles := ShowFiles;
  Menu.Items.BeginUpdate;
  try
    Menu.Items.Clear;
    for I := 0 to Node.Count - 1 do
    begin
      N := TSmallShellNode(Node[I]);
      Item := Menu.Items.Add;
      Item.Caption := N.Name;
      Item.Defaulted := N = Default;
      Item.ImageIndex := N.ImageIndex;
    end;
  finally
    Menu.Items.EndUpdate;
  end;
end;

constructor TShellPathEditBar.Create(AOwner: TComponent);
var
  N: TShellNode;
begin
  inherited Create(AOwner);
  FShowSuggest := True;
  FShowFiles := False;
  Images := TSmallShellImages.Create(Self);
  PopupItems.Menu.Images := Images;
  N := TShellNode.Create(nil, nil);
  Root := N;
  N.Free;
end;

destructor TShellPathEditBar.Destroy;
begin
  FRoot.Free;
  inherited Destroy;
end;

procedure TShellPathEditBar.Loaded;
begin
  inherited Loaded;
  AssociateFrame := FAssociateFrame;
end;

procedure ShellOpen(Node: TShellNode);
var
  Info: TShellExecuteInfo;
begin
  FillChar(Info, SizeOf(Info), #0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_INVOKEIDLIST or SEE_MASK_IDLIST;
  Info.lpIDList := Node.AbsoluteList;
  Info.nShow := SW_SHOW;
  ShellExecuteEx(@Info);
end;

procedure TShellPathEditBar.DefaultAction(Node: TShellNode);
var
  AllowAction: Boolean;
begin
  AllowAction := True;
  if Assigned(FOnDefaultAction) then
    FOnDefaultAction(Self, Node, AllowAction);
  if AllowAction then
    if Node.ShellFolder = nil then
      ShellOpen(Node)
    else
      SelectedNode := Node;
end;

procedure TShellPathEditBar.Parse(const Path: string);
var
  Item: PItemIDList;
  N: TShellNode;
begin
  Item := ILCreateFromPath(Trim(Path));
  if Item = nil then Exit;
  N := TShellNode.CreateFromList(Item);
  try
    DefaultAction(N);
  finally
    N.Free;
  end;
end;

function TShellPathEditBar.AddItem(Document: IDocument; Parent: INode; Item: TSmallShellNode): INode;
var
  F: IFiler;
begin
  if Parent = nil then
  begin
    Result := Document.CreateNode('node');
    Document.Root := Result;
  end
  else
  begin
    Result := Parent.FindNode('node[@name="' + Item.Name + '"]');
    if Result <> nil then Exit;
    Result := Parent.Nodes.Add('node');
  end;
  F := Result.Attributes.Filer;
  F.WriteString('name', Item.Name);
  F.WriteInteger('image', Item.ImageIndex);
  F.WriteInteger('id', Integer(Item));
end;

procedure TShellPathEditBar.ButtonClick(Index: Integer);
var
  N: INode;
  S, D: TShellNode;
begin
  N := ButtonNode[Index div 2];
  S := TShellNode(N.Attributes.Filer.ReadInteger('id'));
  if Odd(Index) and (Index < ButtonCount - 1) then
    D :=  TShellNode(ButtonNode[Index div 2 + 1].Attributes.Filer.ReadInteger('id'))
  else
    D := nil;
  if Odd(Index) then
    BuildShellMenu(PopupItems.Menu, S as TSmallShellNode, D, FShowFiles)
  else
    SelectedNode := S;
  inherited;
end;

function TShellPathEditBar.Convert: string;
var
  N: INode;
  S: TSmallShellNode;
begin
  Result := '';
  if Path.Count > 0 then
  begin
    N := INode(Path[Path.Count - 1]);
    S := TSmallShellNode(N.Attributes.Filer.ReadInteger('id'));
    Result := S.Path;
    if Result = '' then
      Result := S.Name;
  end;
end;

function TShellPathEditBar.CanDropDown(Index: Integer): Boolean;
var
  N: INode;
  S: TShellNode;
begin
  N := ButtonNode[Index];
  S := TShellNode(N.Attributes.Filer.ReadInteger('id'));
  Result := S.Count > 0;
end;

procedure TShellPathEditBar.EditingChange;
var
  CanSuggest: Boolean;
begin
  CanSuggest := Editing and FShowSuggest;
  if CanSuggest then
  begin
    if FAutoComplete = nil then
    begin
      FAutoComplete := CreateComObject(CLSID_AutoComplete) as IAutoComplete2;
      FStrings := CreateComObject(CLSID_ACListISF);
      OleCheck(FAutoComplete.SetOptions(ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST));
      OleCheck(FAutoComplete.Init(Edits[0].Edit.Handle, FStrings, nil, nil));
    end;
    OleCheck(FAutoComplete.SetOptions(ACO_AUTOSUGGEST or ACO_UPDOWNKEYDROPSLIST));
    FAutoComplete.Enable(True);
    HideSuggest;
  end
  else if FAutoComplete <> nil then
  begin
    HideSuggest;
    FAutoComplete.Enable(False);
    FAutoComplete.SetOptions(ACO_NONE);
  end;
end;

procedure TShellPathEditBar.FolderSelect(Sender: TObject);
var
  N: INode;
  S: TSmallShellNode;
begin
  N := ButtonNode[FolderItems.MenuIndex];
  S := TSmallShellNode(N.Attributes.Filer.ReadInteger('id'));
  DefaultAction(S);
end;

procedure TShellPathEditBar.PopupSelect(Sender: TObject);
var
  N: INode;
  S: TSmallShellNode;
begin
  N := ButtonNode[ButtonIndex div 2];
  S := TSmallShellNode(N.Attributes.Filer.ReadInteger('id'));
  S := TSmallShellNode(S.Item[PopupItems.MenuIndex]);
  DefaultAction(S);
  if AutoNext then
    ButtonClick(ButtonCount - 1);
end;

procedure TShellPathEditBar.DoChange(Node: TShellNode);
var
  N: TShellNode;
begin
  N := Node;
  if Binding <> nil then
    Binding.Change(Self, N);
  if Assigned(FOnChange) then
    FOnChange(Self, N);
end;

function TShellPathEditBar.GetAssociateFrame: TFrame;
begin
  if [csLoading, csDesigning] * ComponentState <> [] then
    Result := FAssociateFrame
  else
    Result := PopupItems.AssociateFrame;
end;

procedure TShellPathEditBar.SetAssociateFrame(Value: TFrame);
begin
  if [csLoading, csDesigning] * ComponentState <> [] then
    FAssociateFrame := Value
  else
    PopupItems.AssociateFrame := Value;
end;

procedure TShellPathEditBar.SetSelectedNode(Value: TShellNode);

  function FindShellNode(Root, Node: TShellNode): TShellNode;
  var
    I: Integer;
  begin
    Result := nil;
    TSmallShellNode(Root).ShowFiles := FShowFiles;
    if ILIsEqual(Root.AbsoluteList, Node.AbsoluteList) then
    begin
      Result := Root;
      Result.Count;
    end
    else if ILIsParent(Root.AbsoluteList, Node.AbsoluteList, False) then
    for I := 0 to Root.Count - 1 do
    begin
      Result := FindShellNode(Root[I], Node);
      if Result <> nil then Break;
    end;
  end;

  procedure InsurePath(var Node: INode; Item: TShellNode);
  var
    I: Integer;
  begin
    if Item = FRoot then
      Node := Data.Root
    else
    begin
      InsurePath(Node, Item.Parent);
      Node := AddItem(Data, Node, TSmallShellNode(Item));
      for I := 0 to Item.Count - 1 do
        AddItem(Data, Node, TSmallShellNode(Item[I]));
    end;
  end;

var
  S: TShellNode;
  N: INode;
begin
  if Value = nil then Exit;
  if (FSelectedNode <> nil) and ILIsEqual(FSelectedNode.AbsoluteList, Value.AbsoluteList) then Exit;
  S := FindShellNode(FRoot, Value);
  if S = nil then Exit;
  FSelectedNode := S;
  N := nil;
  InsurePath(N, S);
  DoChange(S);
  DataSelect(N);
end;

procedure TShellPathEditBar.SetSpecialFolder(Value: TSpecialFolder);
var
  Node: TShellNode;
begin
  FSpecialFolder := Value;
  Node := TShellNode.CreateFromFolder(Value);
  try
    Root := Node;
  finally
    Node.Free;
  end;
end;

procedure TShellPathEditBar.SetRoot(Value: TShellNode);
var
  OldRoot: TShellNode;
  D: IDocument;
begin
  if Value = nil then Exit;
  if (FRoot <> nil) and
    ILIsEqual(FRoot.AbsoluteList, Value.AbsoluteList) then Exit;
  OldRoot := FRoot;
  try
    FRoot := Value.Clone(TSmallShellNode);
    BuildShellMenu(PopupItems.Menu, FRoot as TSmallShellNode, nil, FShowFiles);
    D := CreateDocument;
    AddItem(D, nil, FRoot  as TSmallShellNode);
    DataDefine(D.Text);
    FSelectedNode := nil;
    SelectedNode := FRoot;
  finally
    OldRoot.Free;
  end;
end;

procedure TShellPathEditBar.SetShowFiles(Value: Boolean);
var
  R, C: TShellNode;
begin
  if Value <> FShowFiles then
  begin
    FShowFiles := Value;
    R := FRoot;
    if FSelectedNode <> R then
      C := FSelectedNode.Clone
    else
      C := nil;
    R.Clear;
    FRoot := nil;
    try
      Root := R;
      if C <> nil then
        SelectedNode := C;
    finally
      R.Free;
      C.Free;
    end;
  end;
end;

procedure TShellPathEditBar.SetShowSuggest(Value: Boolean);
begin
  if Value <> FShowSuggest then
  begin
    FShowSuggest := Value;
    Editing := False;
  end;
end;

{ TShellBinding }

constructor TShellBinding.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AcceptClass([TShellTree, TShellView, TShellBubbles, TShellEdit, TShellPathEditBar]);;
end;

procedure TShellBinding.SetSpecialFolder(Value: TSpecialFolder);
var
  N: TShellNode;
begin
  FSpecialFolder := Value;
  if csLoading in ComponentState then
    Exit;
  N := TShellNode.CreateFromFolder(FSpecialFolder);
  try
    DoChange(Self, N);
  finally
    N.Free;
  end;
end;

procedure TShellBinding.Refresh;
var
  N: TShellNode;
begin
  N := TShellNode.CreateFromFolder(FSpecialFolder);
  try
    DoChange(Self, N);
  finally
    N.Free;
  end;
end;

procedure TShellBinding.DoChange(Sender: TObject; var Param);
var
  ParamNode: TShellNode absolute Param;
  Root: TShellNode;
  Node: TShellNode;
  C: TComponent;
  I: Integer;
begin
  Root := nil;
  Node := ParamNode;
  if Sender = Self then
    Root := Node
  else if (Sender is TShellBubbles) and TShellBubbles(Sender).NavigateRoot then
    Root := Node
  else if Sender is TShellTree then
    Root := TShellTree(Sender).Root
  else if Sender is TShellEdit then
    Root := TShellEdit(Sender).Root
  else if Sender is TShellPathEditBar then
    Root := TShellPathEditBar(Sender).Root;
  for I := 0 to NotifyCount - 1 do
  begin
    C := NotifyComponent[I];
    if C = Sender then Continue;
    if C is TShellTree then
    begin
      TShellTree(C).Root := Root;
      TShellTree(C).SelectedNode := Node;
    end
    else if C is TShellEdit then
    begin
      TShellEdit(C).Root := Root;
      TShellEdit(C).SelectedNode := Node;
    end
    else if C is TShellPathEditBar then
    begin
      TShellPathEditBar(C).Root := Root;
      TShellPathEditBar(C).SelectedNode := Node;
    end
    else if C is TShellView then
      if Sender = Self then
        TShellView(C).ParentRoot := Node
      else
        TShellView(C).Root := Node;
  end;
end;

procedure HideSuggest;
const
  AutoClass = 'Auto-Suggest Dropdown';
var
  Wnd: HWND;
begin
  Wnd := FindWindow(AutoClass, nil);
  if Wnd <> 0 then
    ShowWindow(Wnd, SW_HIDE);
end;

procedure InitializeImages;
const
  AppNames: array[0..25] of string = (
    'cmd.exe', 'ahui.exe', 'calc.exe', 'charmap.exe', 'cleanmgr.exe',
    'magnify.exe', 'migpwd.exe', 'mobsync.exe', 'narrator.exe', 'mstsc.exe',
    'osk.exe', 'osuninst.exe', 'rasphone.exe', 'rtcshare.exe', 'sndrecI.exe',
    'sndvol32.exe', 'tourstart.exe', 'utilman.exe', 'wiaacmgr.exe', 'wuauclt1.exe',
    'notepad.exe', 'winmine.exe', 'freecell.exe', 'mshearts.exe', 'sol.exe',
    'spider.exe');
var
  Folder: TSpecialFolder;
  S: string;
  I: Integer;
begin
  for Folder := Low(Folder) to High(Folder) do
    TShellImageNode.LoadImage(Folder);
  S := GetSystemPath;
  for I := Low(AppNames) to High(AppNames) do
    TShellImageNode.LoadImage(S + AppNames[I]);
end;

{ TShellBinding }

initialization
  OleInitialize(nil);
  IsMultiThread := True;
  InitializeImages;
finalization
  ShellImageLists[False].Free;
  ShellImageLists[False] := nil;
  ShellImageLists[True].Free;
  ShellImageLists[True] := nil;
  OleUninitialize;
end.
