
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit ProviderTools;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ImgList,
  BaseTypes, GraphTools{$IFDEF GDIPLUS}, GdiPlus{$ENDIF};

const
  WM_CHILDSETFOCUS    = WM_USER + $0AC1A;
  WM_CHILDKILLFOCUS   = WM_CHILDSETFOCUS + 1;

{ TComponentBinding }

type
  IComponentBinding = interface
    ['{290139B2-AEF0-443A-82AD-7FB8057339CD}']
    function GetInstance: TObject;
    procedure Change(Sender: TObject; var Param);
    property Instance: TObject read GetInstance;
  end;

  TComponentBinding = class(TComponent, IUnknown, IComponentBinding)
  private
    FClasses: TList;
    FComponents: TList;
    FNames: TStrings;
    FChanging: Boolean;
    FUpdateRef: Integer;
    procedure HandleRefresh;
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    function GetNotifyComponent(Index: Integer): TComponent;
    function GetNotifyCount: Integer;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; reintroduce; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IChangeBinding }
    function GetInstance: TObject;
    procedure Change(Sender: TObject; var Param);
    { TComponentBinding }
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Refresh; virtual;
    procedure AcceptClass(AClass: TClass); overload;
    procedure AcceptClass(AClassList: array of TClass); overload;
    procedure DoChange(Sender: TObject; var Param); virtual;
    procedure DoAdd(Component: TComponent); virtual;
    procedure DoRemove(Component: TComponent); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property NotifyComponent[Index: Integer]: TComponent read GetNotifyComponent;
    property NotifyCount: Integer read GetNotifyCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Add(Component: TComponent);
    procedure Remove(Component: TComponent);
    procedure Clear;
    function Accept(Component: TComponent): Boolean;
    function Contains(Component: TComponent): Boolean;
  end;

  TComponentBindingClass = class of TComponentBinding;

type
  TVerticalDirection = (vdNone, vdUp, vdDown);
  THorizontalDirection = (hdNone, hdLeft, hdRight);

{ TControlProvider class }

const
  SDefaultProviderName = '(default provider)';

type
  TIntfCollectionItem = class(TCollectionItem)
  private
    FData: Pointer;
    FInstance: TObject;
    FUnknown: IUnknown;
    FTag: Integer;
  public
    property Data: Pointer read FData write FData;
    property Instance: TObject read FInstance write FInstance;
    property Unknown: IUnknown read FUnknown write FUnknown;
    property Tag: Integer read FTag write FTag;
  end;

  TProviderName = type string;

  IControlProvider = interface(IUnknown)
    ['{991A55C4-FFA0-48C4-9286-9156CCA46C3D}']
    function GetInstanceProviderName: TProviderName;
    function GetInstanceProviderClass: TClass;
    procedure Init(Control: TControl);
    property ProviderName: TProviderName read GetInstanceProviderName;
    property ProviderClass: TClass read GetInstanceProviderClass;
  end;

  TControlProvider = class(TInterfacedObject, IControlProvider)
  protected
    class function GetProviderName: TProviderName; virtual;
    function GetInstanceProviderName: TProviderName;
    function GetInstanceProviderClass: TClass;
    procedure Init(Control: TControl); virtual;
    property ProviderName: TProviderName read GetInstanceProviderName;
    property ProviderClass: TClass read GetInstanceProviderClass;
  public
    constructor Create; virtual;
  end;

  TControlProviderClass = class of TControlProvider;

  TMouseHoverEvent = procedure(Sender: TObject; HoverObject: TObject) of object;

{ TProviderGraphicControl }

  TProviderControl = class(TGraphicControl)
  private
    FBinding: IComponentBinding;
    FProvider: IControlProvider;
    FUnknown: IUnknown;
    FOnMouseHover: TMouseHoverEvent;
    procedure SetProvider(Value: IControlProvider);
    function GetProviderName: TProviderName;
    procedure SetProviderName(const Value: TProviderName);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DoMouseHover(HoverObject: TObject); virtual;
    property Binding: IComponentBinding read FBinding write FBinding;
    property Provider: IControlProvider read FProvider write SetProvider;
    property ProviderName: TProviderName read GetProviderName write SetProviderName;
  public
    constructor Create(AOwner: TComponent); override;
    property Unknown: IUnknown read FUnknown write FUnknown;
    property OnMouseHover: TMouseHoverEvent read FOnMouseHover write FOnMouseHover;
    {$IFDEF GDIPLUS}
  private
    FGraphics: IGdiGraphics;
    function GetGraphics: IGdiGraphics;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    property Graphics: IGdiGraphics read GetGraphics;
    {$ENDIF}
  end;

{ TImagesProviderControl }

  TImagesProviderControl = class(TProviderControl)
  private
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageChangeLink: TChangeLink;
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ImageListChange(Sender: TObject); virtual;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FimageIndex write SetImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TProviderWindow }

  TProviderWindow = class(TCustomControl)
  private
    FBinding: IComponentBinding;
    FProvider: IControlProvider;
    FUnknown: IUnknown;
    FOnMouseHover: TMouseHoverEvent;
    procedure SetProvider(Value: IControlProvider);
    function GetProviderName: TProviderName;
    procedure SetProviderName(const Value: TProviderName);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure DoMouseHover(HoverObject: TObject); virtual;
    property Binding: IComponentBinding read FBinding write FBinding;
    property Provider: IControlProvider read FProvider write SetProvider;
    property ProviderName: TProviderName read GetProviderName write SetProviderName;
  public
    constructor Create(AOwner: TComponent); override;
    property Unknown: IUnknown read FUnknown write FUnknown;
    property OnMouseHover: TMouseHoverEvent read FOnMouseHover write FOnMouseHover;
    {$IFDEF GDIPLUS}
  private
    FGraphics: IGdiGraphics;
    function GetGraphics: IGdiGraphics;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    property Graphics: IGdiGraphics read GetGraphics;
    {$ENDIF}
  end;

{ TImagesCustomControl }

  TImagesProviderWindow = class(TProviderWindow)
  private
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageChangeLink: TChangeLink;
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ImageListChange(Sender: TObject); virtual;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FimageIndex write SetImageIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ ImageList drawing routines }

procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; State: TDrawState); overload;
procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  const Rect: TRect; Index: Integer; State: TDrawState); overload;
procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; Enabled: Boolean = True; Hotlight: Boolean = False); overload;
function ImageListSize(ImageList: TCustomImageList): TSize;

{ Find and Register routines }

procedure FindProviderNames(ControlClass: TControlClass; Strings: TStrings);
function FindProvider(ControlClass: TControlClass; const Name: string = ''): IControlProvider;

procedure RegisterProvider(ProviderClass: TControlProviderClass;
  const ControlClasses: array of TControlClass);
procedure RegisterDefaultProvider(ProviderClass: TControlProviderClass;
  ControlClass: TControlClass);

implementation

{$IFDEF D5}
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;
var
  U: IUnknown;
begin
  Result := (Instance <> nil) and
    ((Instance.GetInterface(IUnknown, U) and SysUtils.Supports(U, IID, Intf)) or
    Instance.GetInterface(IID, Intf));
end;

function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
var
  Temp: IUnknown;
begin
  Result := FormTools.Supports(Instance, IID, Temp);
end;
{$ENDIF}

function ImageListSize(ImageList: TCustomImageList): TSize;
var
  Images: IIndexedImages;
begin
  if Supports(ImageList, IIndexedImages, Images) then
  begin
    Result.cx := Images.Size;
    Result.cy := Result.cx;
  end
  else
  begin
    Result.cx := ImageList.Width;
    Result.cy := ImageList.Height;
  end;
end;

procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; State: TDrawState);
{$IFDEF D7_UP}
const
  HotStyles: array[Boolean] of TDrawingStyle = (dsNormal, dsFocus);
{$ENDIF}
var
  Images: IIndexedImages;
begin
  if ImageList = nil then Exit;
  if Supports(ImageList, IIndexedImages, Images) then
    Images.DrawImage(Canvas, X, Y, Index, State)
  else
{$IFDEF D7_UP}
  ImageList.Draw(Canvas, X, Y, Index, dsNormal, itImage, [dsDisabled] * State = []);
{$ELSE}
  ImageList.Draw(Canvas, X, Y, Index, [dsDisabled] * State = []);
{$ENDIF}
end;

procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  const Rect: TRect; Index: Integer; State: TDrawState); overload;
var
  Images: IIndexedImages;
  I: Integer;
begin
  if (ImageList = nil) or (Index < 0) then Exit;
  if Supports(ImageList, IIndexedImages, Images) then
    I := Images.Size
  else
    I := ImageList.Height;
  ImageListDraw(ImageList, Canvas, Rect.Left + (WidthOf(Rect) - I) div 2,
    Rect.Top + (HeightOf(Rect) - I) div 2, Index, State);
end;

procedure ImageListDraw(ImageList: TCustomImageList; Canvas: TCanvas;
  X, Y, Index: Integer; Enabled: Boolean = True; Hotlight: Boolean = False);
var
  State: TDrawState;
begin
  if ImageList = nil then Exit;
  State := [];
  if not Enabled then
    Include(State, dsDisabled);
  // if Hotlight then
  Include(State, dsHot);
  ImageListDraw(ImageList, Canvas, X, Y, Index, State);
end;

{ TComponentBinding }

constructor TComponentBinding.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClasses := TList.Create;
  FComponents := TList.Create;
  FNames := TStringList.Create;
end;

destructor TComponentBinding.Destroy;
begin
  Clear;
  FNames.Free;
  inherited Destroy;
end;

procedure TComponentBinding.HandleRefresh;
begin
  if csLoading in ComponentState then Exit;
  if FUpdateRef < 1 then Refresh;
end;

procedure TComponentBinding.Assign(Source: TPersistent);
var
  B: TComponentBinding absolute Source;
  I: Integer;
begin
  if Source.ClassType = ClassType then
  begin
    Clear;
    BeginUpdate;
    try
      for I := 0 to B.NotifyCount - 1 do
        Add(B.NotifyComponent[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TComponentBinding.BeginUpdate;
begin
  Inc(FUpdateRef);
end;

procedure TComponentBinding.EndUpdate;
begin
  Dec(FUpdateRef);
  HandleRefresh;
end;

procedure TComponentBinding.ReadData(Reader: TReader);
begin
  Reader.ReadListBegin;
  FNames.Clear;
  while not Reader.EndOfList do
    FNames.Add(Reader.ReadString);
  Reader.ReadListEnd;
end;

procedure TComponentBinding.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FComponents.Count - 1 do
    Writer.WriteString(TComponent(FComponents[I]).Name);
  Writer.WriteListEnd;
end;

procedure TComponentBinding.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      {if Filer.Ancestor is ClassType then
        Result := not Equals(TStrings(Filer.Ancestor))}
    end
    else
      Result := FComponents.Count > 0;
  end;

begin
  Filer.DefineProperty('Bindings', ReadData, WriteData, DoWrite);
end;

procedure TComponentBinding.Loaded;
var
  I: Integer;
begin
  inherited Loaded;
  BeginUpdate;
  try
    for I := 0 to FNames.Count - 1 do
      Add(Owner.FindComponent(FNames[I]));
  finally
    EndUpdate;
  end;
end;

procedure TComponentBinding.Refresh;
begin
end;

procedure TComponentBinding.Add(Component: TComponent);
var
  G: TProviderControl absolute Component;
  C: TProviderWindow absolute Component;
begin
  if Accept(Component) then
    if not Contains(Component) then
    begin
      FComponents.Add(Component);
      Component.FreeNotification(Self);
      if Component is TProviderControl then
        G.Binding := Self
      else if Component is TProviderWindow then
        C.Binding := Self;
      DoAdd(Component);
      HandleRefresh;
    end;
end;

procedure TComponentBinding.Remove(Component: TComponent);
var
  G: TProviderControl absolute Component;
  C: TProviderWindow absolute Component;
begin
  if Contains(Component) then
  begin
    FComponents.Remove(Component);
    Component.RemoveFreeNotification(Self);
    if (Component is TProviderControl) and (G.Binding <> nil) and (G.Binding.Instance = Self) then
      G.Binding := nil;
    if (Component is TProviderWindow) and (C.Binding <> nil) and (C.Binding.Instance = Self) then
      C.Binding := nil;
    DoRemove(Component);
  end;
end;

procedure TComponentBinding.Clear;
begin
  while NotifyCount > 0 do
    Remove(NotifyComponent[0]);
end;

function TComponentBinding.Accept(Component: TComponent): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (Component = nil) or (Component.Name = '') then Exit;
  for I := 0 to FClasses.Count - 1 do
  begin
    Result := Component.ClassType = FClasses[I];
    if Result then Exit;
  end;
end;

function TComponentBinding.Contains(Component: TComponent): Boolean;
begin
  Result := FComponents.IndexOf(Component) > -1;
end;

procedure TComponentBinding.AcceptClass(AClass: TClass);
begin
  if FClasses.IndexOf(AClass) < 0 then
    FClasses.Add(AClass);
end;

procedure TComponentBinding.AcceptClass(AClassList: array of TClass);
var
  I: Integer;
begin
  for I := Low(AClassList) to High(AClassList) do
    FClasses.Add(AClassList[I]);
end;

procedure TComponentBinding.DoChange(Sender: TObject; var Param);
begin
end;

procedure TComponentBinding.DoAdd(Component: TComponent);
begin
end;

procedure TComponentBinding.DoRemove(Component: TComponent);
begin
end;

procedure TComponentBinding.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opInsert then
    Add(AComponent)
  else if Operation = opRemove then
    Remove(AComponent);
end;

function TComponentBinding.GetNotifyComponent(Index: Integer): TComponent;
begin
  Result := TComponent(FComponents[Index]);
end;

function TComponentBinding.GetNotifyCount: Integer;
begin
  Result := FComponents.Count;
end;

{ TComponentBinding.IUnknown }

function TComponentBinding.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TComponentBinding._AddRef: Integer;
begin
  Result := 1;
end;

function TComponentBinding._Release: Integer;
begin
  Result := 1;
end;

{ TComponentBinding.IChangeBinding }

procedure TComponentBinding.Change(Sender: TObject; var Param);
begin
  if csLoading in ComponentState then Exit;
  if not FChanging then
  begin
    FChanging := True;
    try
      DoChange(Sender, Param);
    finally
      FChanging := False;
    end;
  end;
end;

function TComponentBinding.GetInstance: TObject;
begin
  Result := Self;
end;

{ TProviderControl }

constructor TControlProvider.Create;
begin
  inherited Create;
end;

class function TControlProvider.GetProviderName: TProviderName;
begin
  Result := SDefaultProviderName;
end;

function TControlProvider.GetInstanceProviderName: TProviderName;
begin
  Result := GetProviderName;
end;

function TControlProvider.GetInstanceProviderClass: TClass;
begin
  Result := ClassType;
end;

procedure TControlProvider.Init(Control: TControl);
begin
end;

{ TProviderControl }

constructor TProviderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Provider := nil;
end;

procedure TProviderControl.DoMouseHover(HoverObject: TObject);
begin
  if Assigned(FOnMouseHover) then
    FOnMouseHover(Self, HoverObject);
end;

procedure TProviderControl.SetProvider(Value: IControlProvider);
begin
  if Value = nil then
    Value := FindProvider(TControlClass(ClassType));
  if Value <> FProvider then
  begin
    FProvider := Value;
    if FProvider <> nil then
      FProvider.Init(Self);
    Repaint;
  end;
end;

function TProviderControl.GetProviderName: TProviderName;
begin
  if FProvider = nil then
    Result := ''
  else
    Result := Provider.ProviderName;
end;

procedure TProviderControl.SetProviderName(const Value: TProviderName);
begin
  Provider := FindProvider(TControlClass(ClassType), Value);
end;

procedure TProviderControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  DoMouseHover(Self);
end;

procedure TProviderControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  DoMouseHover(nil);
end;

{$IFDEF GDIPLUS}

function TProviderControl.GetGraphics: IGdiGraphics;
begin
  if FGraphics = nil then
    if Parent is TProviderWindow then
      FGraphics := TProviderWindow(Parent).Graphics
    else
      FGraphics := NewGraphics(Canvas.Handle);
  Result := FGraphics;
end;

procedure TProviderControl.WMPaint(var Message: TWMPaint);
begin
  FGraphics := nil;
  try
    inherited;
  finally
    FGraphics := nil;
  end;
end;

{$ENDIF}

{ TImagesProviderControl }

constructor TImagesProviderControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TImagesProviderControl.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TImagesProviderControl.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TImagesProviderControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

procedure TImagesProviderControl.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    ImageListChange(FImages);
  end;
end;

procedure TImagesProviderControl.SetImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

{ TProviderWindow }

constructor TProviderWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Provider := nil;
end;

procedure TProviderWindow.DoMouseHover(HoverObject: TObject);
begin
  if Assigned(FOnMouseHover) then
    FOnMouseHover(Self, HoverObject);
end;

procedure TProviderWindow.SetProvider(Value: IControlProvider);
begin
  if Value = nil then
    Value := FindProvider(TControlClass(ClassType));
  if Value <> FProvider then
  begin
    FProvider := Value;
    if FProvider <> nil then
      FProvider.Init(Self);
    Repaint;
  end;
end;

function TProviderWindow.GetProviderName: TProviderName;
begin
  if FProvider = nil then
    Result := ''
  else
    Result := Provider.ProviderName;
end;

procedure TProviderWindow.SetProviderName(const Value: TProviderName);
begin
  Provider := FindProvider(TControlClass(ClassType), Value);
end;

procedure TProviderWindow.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  DoMouseHover(Self);
end;

procedure TProviderWindow.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  DoMouseHover(nil);
end;

{$IFDEF GDIPLUS}

function TProviderWindow.GetGraphics: IGdiGraphics;
begin
  if FGraphics = nil then
    FGraphics := NewGraphics(Canvas.Handle);
  Result := FGraphics;
end;

procedure TProviderWindow.WMPaint(var Message: TWMPaint);
begin
  FGraphics := nil;
  try
    inherited;
  finally
    FGraphics := nil;
  end;
end;

{$ENDIF}

{ TImagesProviderWindow }

constructor TImagesProviderWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TImagesProviderWindow.Destroy;
begin
  Images := nil;
  FImageChangeLink.Free;
  inherited Destroy;
end;

procedure TImagesProviderWindow.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TImagesProviderWindow.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FImages then
      Images := nil;
end;

procedure TImagesProviderWindow.SetImages(Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
    begin
      FImages.UnRegisterChanges(FImageChangeLink);
      FImages.RemoveFreeNotification(Self);
    end;
    FImages := Value;
    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;
    ImageListChange(FImages);
  end;
end;

procedure TImagesProviderWindow.SetImageIndex(Value: Integer);
begin
  if Value < 0 then Value := -1;
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

{ TProviderManager }

type
  PProviderLink = ^TProviderLink;
  TProviderLink = record
    Next: PProviderLink;
    ControlClass: TControlClass;
    ProviderClass: TControlProviderClass;
    Default: Boolean;
  end;

  TProviderManager = class(TComponent)
  private
    FLink: PProviderLink;
    FProviders: TInterfaceList;
    function GetProvider(Index: Integer): IControlProvider;
    function GetProviderCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(Provider: IControlProvider);
    property Providers[Index: Integer]: IControlProvider read GetProvider;
    property ProviderCount: Integer read GetProviderCount;
  end;

var
  InternalManager: TObject;

function ProviderManager: TProviderManager;
begin
  if InternalManager = nil then
    TProviderManager.Create(Application);
  Result := TProviderManager(InternalManager);
end;

constructor TProviderManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InternalManager := Self;
  FProviders := TInterfaceList.Create;
end;

destructor TProviderManager.Destroy;
var
  A, B: PProviderLink;
begin
  InternalManager := nil;
  A := FLink;
  while A <> nil do
  begin
    B := A.Next;
    Dispose(A);
    A := B;
  end;
  FProviders.Free;
  inherited Destroy;
end;

function TProviderManager.GetProvider(Index: Integer): IControlProvider;
begin
  Result := IControlProvider(FProviders[Index]);
end;

procedure TProviderManager.Add(Provider: IControlProvider);
begin
  Fproviders.Add(Provider);
end;

function TProviderManager.GetProviderCount: Integer;
begin
  Result := FProviders.Count;
end;

procedure FindProviderNames(ControlClass: TControlClass;
  Strings: TStrings);
var
  Link: PProviderLink;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    Link := ProviderManager.FLink;
    while Link <> nil do
      if (Link.ControlClass = ControlClass) or
        ControlClass.InheritsFrom(Link.ControlClass) then
        Strings.Add(Link.ProviderClass.GetProviderName);
  finally
    Strings.EndUpdate;
  end
end;

function FindProvider(ControlClass: TControlClass; const Name: string = ''): IControlProvider;
var
  ProviderClass, DefaultClass: TControlProviderClass;
  Link: PProviderLink;
  M: TProviderManager;
  S: string;
  I: Integer;
begin
  M := ProviderManager;
  ProviderClass := nil;
  DefaultClass := nil;
  Link := M.FLink;
  S := UpperCase(Trim(Name));
  while Link <> nil do
  begin
    if Link.ControlClass = ControlClass then
    begin
      if UpperCase(Link.ProviderClass.GetProviderName) = S then
      begin
        ProviderClass := Link.ProviderClass;
        Break;
      end
      else if Link.Default then
        DefaultClass := Link.ProviderClass;
    end;
    Link := Link.Next;
  end;
  Result := nil;
  if ProviderClass = nil then
    ProviderClass := DefaultClass;
  if ProviderClass <> nil then
  begin
    for I := 0 to M.ProviderCount - 1 do
      if M.Providers[I].ProviderClass = ProviderClass then
      begin
        Result := M.Providers[I];
        Break;
      end;
    if Result = nil then
    begin
      Result := ProviderClass.Create;
      M.Add(Result);
    end;
  end;
end;

procedure RegisterProvider(ProviderClass: TControlProviderClass;
  const ControlClasses: array of TControlClass);

  function NewLink(Index: Integer): PProviderLink;
  begin
    New(Result);
    Result.ProviderClass := ProviderClass;
    Result.ControlClass := ControlClasses[Index];
    Result.Default := False;
    Result.Next := nil;
  end;

var
  Link: PProviderLink;
  M: TProviderManager;
  I: Integer;
begin
  M := ProviderManager;
  for I := Low(ControlClasses) to High(ControlClasses) do
  begin
    Link := M.FLink;
    if Link = nil then
      M.FLink := NewLink(I)
    else
    repeat
      if (Link.ProviderClass = ProviderClass) and (Link.ControlClass = ControlClasses[I]) then
        Break;
      if Link.Next = nil then
      begin
        Link.Next := NewLink(I);
        Break;
      end;
      Link := Link.Next;
    until False;
  end;
end;

procedure RegisterDefaultProvider(ProviderClass: TControlProviderClass;
  ControlClass: TControlClass);

  function NewLink: PProviderLink;
  begin
    New(Result);
    Result.ProviderClass := ProviderClass;
    Result.ControlClass := ControlClass;
    Result.Default := True;
    Result.Next := nil;
  end;

var
  Link: PProviderLink;
  M: TProviderManager;
begin
  M := ProviderManager;
  Link := M.FLink;
  if Link = nil then
    M.FLink := NewLink
  else
  repeat
    if (Link.ProviderClass = ProviderClass) and (Link.ControlClass = ControlClass) then
    begin
      Link.Default := True;
      Break;
    end;
    if Link.Next = nil then
    begin
      Link.Next := NewLink;
      Break;
    end;
    Link := Link.Next;
  until False;
end;

initialization
  InternalManager := nil;
finalization
  InternalManager.Free;
end.
