
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit OleTools;

interface

{$I CODEBOT.INC}

uses
  ActiveX, Classes, SysUtils, Windows, Registry, ComObj, Contnrs, XMLObjects,
  ShlTools, BaseTypes,
  {$IFDEF D6_UP}Variants{$ENDIF};

{ Error info support for stdcall COM methods }

procedure SafeCheck(const Obj; IID: TGUID; Result: HResult);

{ Guid routines }

function CreateGuid: TGUID;
function CreateGuidStr: string;
function GuidStrCheck(const S: string): Boolean;
function GuidToStr(const GUID: TGUID): string;
function StrToGuid(const S: string): TGUID;
function CleanGuidStr(const S: string): string;

{ GetClassFile }

function GetClassFile(const CLSID: TGUID): string;

{ GetInterfaceStrings }

procedure GetProviderStrings(Unknown: IUnknown; Strings: TStrings);
procedure GetInterfaceStrings(Unknown: IUnknown; Strings: TStrings);
function GetInterfaceName(IID: TIID): string;

{ EnumerateVariant  }

type
  TVariantCallback = procedure(const V: OleVariant; Data: Pointer);

procedure EnumerateVariant(const V: OleVariant; Callback: TVariantCallback;
  Data: Pointer);

{ TEnumString class }

type
  TEnumString = class(TInterfacedObject, IEnumString)
  private
    FStrings: TStrings;
    FIndex: Integer;
    procedure SetStrings(Value: TStrings);
  protected
    { IEnumString }
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
  public
    property Strings: TStrings read FStrings write SetStrings;
  end;

{ The TEnumVariant class can be used as part of a collection automation object.
  All collections should support an enumerator interface exposed through a
  propget method named _NewEnum with a dispatch id of -4. }

{ TEnumVariant }

  TEnumVariant = class(TInterfacedObject, IEnumVariant)
  private
    FObjectList: TObjectList;
    FIndex: Integer;
  protected
    { IEnumVariant }
    function Next(celt: LongWord; var rgvar: OleVariant;
      out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  public
    constructor Create(List: TObjectList);
  end;

{ The TAutoStorable class represents an object capable of data storage and
  retrieval through an OleVariant data property }

type
  TAutoIntfObjectClass = class of TAutoIntfObject;

  TAutoStorable = class(TAutoIntfObject)
  protected
    procedure InternalExecute(const Command: OleVariant); virtual;
    function InternalRetrieve(const Format: OleVariant): OleVariant; virtual;
    procedure InternalUpdate(const Data: OleVariant); virtual;
    { IStorable }
    procedure Execute(Command: OleVariant); safecall;
    function  Retrieve(Format: OleVariant): OleVariant; safecall;
    procedure Update(Data: OleVariant); safecall;
  end;

{ The TAutoCollection class can be used as a container for automation objects.
  The item class type and its' interface identifier are passed in the
  constructor. The first parameter of the constructor is the interface
  identifier of the collection itself. In order to support VB's "For Each"
  syntax and the default Item property, the typelibary must set the _NewEnum
  property dispid to -4 and the Item property dipsid to 0. All other
  properties are optional. }

  TAutoCollection = class(TAutoStorable)
  private
    FObjectList: TObjectList;
    FItemClass: TAutoIntfObjectClass;
    FItem: TGUID;
  protected
    property ObjectList: TObjectList read FObjectList;
    function InternalAdd(const Item: OleVariant): OleVariant; virtual;
    procedure InternalDelete(const Item: OleVariant); virtual;
    function InternalGetCount: Integer; virtual;
    function InternalGetItem(const Index: OleVariant): OleVariant; virtual;
    function InternalSearch(const Params: OleVariant): OleVariant; virtual;
    { ICollection }
    function Get__NewEnum: IUnknown; safecall;
    function Add(Item: OleVariant): OleVariant; safecall;
    procedure Delete(Item: OleVariant); safecall;
    function Get_Count: Integer; safecall;
    function Get_Item(Index: OleVariant): OleVariant; safecall;
    { ISearchCollection }
    function Search(Params: OleVariant): OleVariant; safecall;
  public
    constructor Create(Collection: TGUID; ItemClass: TAutoIntfObjectClass;
      Item: TGUID); virtual;
    destructor Destroy; override;
  end;

{ Scripting event object }

  TScriptEvents = class(TObject)
  private
    FEvents: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AttachEvent(const Name: string; Handler: IDispatch);
    procedure DetachEvent(const Name: string; Handler: IDispatch);
    procedure InvokeEvent(const Name: string);
  end;

{ EventSink helper routines }

function ConnectEvent(Instance: IUnknown; EventID: TGUID; Event: IUnknown): Integer;
procedure DisconnectEvent(Instance: IUnknown; EventID: TGUID; Cookie: Integer);

implementation

uses
  StrConst;

var
  InternalDocument: IUnknown;

function InterfacesDocument: IDocument;
begin
  if InternalDocument = nil then
  begin
    Result := CreateDocument;
    Result.LoadFromFile(GetSettingsFile('codebot', 'interfaces.lookup'));
    InternalDocument := Result;
  end
  else
    Result := InternalDocument as IDocument;
end;


procedure SafeCheck(const Obj; IID: TGUID; Result: HResult);
var
  Unk: IUnknown absolute Obj;
  SupportErrorInfo: ISupportErrorInfo;
  ErrorInfo: IErrorInfo;
  Buffer: WideString;
  Description, Source, HelpFile: string;
  HelpContext: Integer;
begin
  if Result <> S_OK then
    if (Unk.QueryInterface(ISupportErrorInfo, SupportErrorInfo) = S_OK) and
      (SupportErrorInfo.InterfaceSupportsErrorInfo(IID) = S_OK) and
      (GetErrorInfo(0, ErrorInfo) = S_OK) then
    begin
      ErrorInfo.GetDescription(Buffer);
      Description := WideCharToString(PWideChar(Buffer));
      SysFreeString(PWideChar(Buffer));
      ErrorInfo.GetSource(Buffer);
      Source := WideCharToString(PWideChar(Buffer));
      SysFreeString(PWideChar(Buffer));
      ErrorInfo.GetHelpFile(Buffer);
      HelpFile := WideCharToString(PWideChar(Buffer));
      SysFreeString(PWideChar(Buffer));
      ErrorInfo.GetHelpContext(HelpContext);
      raise EOleException.Create(Description, Result, Source,
        HelpFile, HelpContext);
    end
    else
      OleError(Result);
end;

const
  NullGuid = '{00000000-0000-0000-0000-000000000000}';

function CreateGuid: TGUID;
begin
  if CoCreateGuid(Result) <> S_OK then
    FillChar(Result, SizeOf(Result), #0);
end;

function CreateGuidStr: string;
var
  G: TGUID;
begin
  if CoCreateGuid(G) = S_OK then
    Result := GuidToStr(G)
  else
    Result := NullGuid;
end;

function GuidStrCheck(const S: string): Boolean;
var
  I: Integer;
begin
  Result := Length(S) = Length(NullGuid);
  if not Result then Exit;
  for I := 1 to Length(S) do
    if NullGuid[I] = '0' then
      if CharInSet(UpCase(S[I]), ['0'..'9', 'A'..'F']) then
        Continue
      else
      begin
        Result := False;
        Break;
      end
    else if NullGuid[I] <> S[I] then
    begin
      Result := False;
      Break;
    end;
end;

function GuidToStr(const GUID: TGUID): string;
var
  OleStr: PWideChar;
begin
  if StringFromIID(GUID, OleStr) = S_OK then
  begin
    Result := OleStr;
    CoTaskMemFree(OleStr);
  end
  else
    Result := NullGuid;
end;

function StrToGuid(const S: string): TGUID;
var
  OleStr: WideString;
begin
  OleStr := S;
  IIDFromString(PWideChar(OleStr), Result);
end;

function GetClassFile(const CLSID: TGUID): string;
var
  Reg: TRegistry;
  S: WideString;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    SetLength(S, 38);
    if (StringFromGUID2(CLSID, PWideChar(S), 39) > 0) and
      (Reg.KeyExists('\CLSID\' + S + '\InprocServer32')) then
    begin
      Reg.OpenKey('\CLSID\' + S + '\InprocServer32', False);
      Result := Reg.ReadString('');
    end;
  finally
    Reg.Free;
  end;
end;

function CleanGuidStr(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '{', '', [rfReplaceAll]);
  Result := StringReplace(Result, '}', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

procedure GuidClear(var Guid: TGUID);
begin
  FillChar(Guid, SizeOf(TGUID), #0);
end;

procedure GetProviderStrings(Unknown: IUnknown; Strings: TStrings);
var
  Obj: IUnknown;
  P: IServiceProvider;
  N: INodes;
  F: IFiler;
  G: TGUID;
  I: Integer;
begin
  Strings.BeginUpdate;
  with Strings do
  try
    Clear;
    if not Supports(Unknown, IServiceProvider, P) then Exit;
    N := InterfacesDocument.Root.Nodes;
    for I := 0 to N.Count - 1 do
    begin
      F := N[I].Attributes.Filer;
      G := StrToGuid(F.ReadString('value'));
      if P.QueryService(G, G, Obj) = S_OK then
        Add(F.ReadString('name'));
    end;
  finally
    EndUpdate;
  end;
end;

procedure GetInterfaceStrings(Unknown: IUnknown; Strings: TStrings);
var
  Obj: IUnknown;
  N: INodes;
  F: IFiler;
  G: TGUID;
  I: Integer;
begin
  Strings.BeginUpdate;
  with Unknown, Strings do
  try
    Clear;
    N := InterfacesDocument.Root.Nodes;
    for I := 0 to N.Count - 1 do
    begin
      F := N[I].Attributes.Filer;
      G := StrToGuid(F.ReadString('value'));
      if QueryInterface(G, Obj) = S_OK then
        Add(F.ReadString('name'));
    end;
  finally
    EndUpdate;
  end;
end;

function GetInterfaceName(IID: TIID): string;
var
  N: INode;
begin
  N := InterfacesDocument.Root.FindNode('entry[@value="'+ GuidToStr(IID) +'"]');
  if N <> nil then
    Result := N.Attributes.NamedAttribute['name'].Value
  else
    Result := '';
end;

procedure EnumerateVariant(const V: OleVariant; Callback: TVariantCallback;
  Data: Pointer);
var
  Unknown: IUnknown;
  EnumVariant: IEnumVariant;
  Variant: OleVariant;
  Dummy: Cardinal;
begin
  if VarType(V) = varDispatch then
  begin
    Unknown := V._NewEnum;
    EnumVariant := Unknown as IEnumVariant;
    while EnumVariant.Next(1, Variant, Dummy) = S_OK do
      Callback(Variant, Data);
  end;
end;

{ TEnumString }

procedure TEnumString.SetStrings(Value: TStrings);
begin
  if Value <> FStrings then
  begin
    FIndex := 0;
    FStrings := Value;
  end;
end;

{ TEnumString.IEnumString }

function TEnumString.Next(celt: Longint;
  out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
begin
  I := 0;
  Result := S_FALSE;
  while (I < celt) and (FIndex < FStrings.Count) do
  begin
    TPointerList(elt)[I] := PWideChar(WideString(FStrings[FIndex]));
    Inc(I);
    Inc(FIndex);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK;
end;

function TEnumString.Skip(celt: Longint): HResult;
begin
  Result := S_FALSE;
  if FStrings <> nil then
    if (FIndex + celt) <= FStrings.Count then
    begin
      Inc(FIndex, celt);
      Result := S_OK;
    end
    else
      FIndex := FStrings.Count;
end;

function TEnumString.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TEnumString.Clone(out enm: IEnumString): HResult;
var
  EnumString: TEnumString;
begin
  EnumString := TEnumString.Create;
  EnumString.Strings := FStrings;
  EnumString.FIndex := FIndex;
  enm := EnumString as IEnumString;
  Result := S_OK;
end;

{ TEnumVariant }

constructor TEnumVariant.Create(List: TObjectList);
begin
  inherited Create;
  FIndex := -1;
  FObjectList := List;
end;

{ TEnumVariant.IEnumVaraint }

function TEnumVariant.Next(celt: LongWord; var rgvar: OleVariant;
  out pceltFetched: LongWord): HResult;
type
  TVariants = array[0..High(Word) div 8] of OleVariant;
var
  Variants: TVariants absolute rgvar;
  Dispatch: IDispatch;
  I: LongWord;
begin
  Result := S_OK;
  for I := 0 to celt - 1 do
  begin
    Inc(FIndex);
    if FIndex > FObjectList.Count - 1 then
    begin
      FIndex := FObjectList.Count - 1;
      Result := S_FALSE;
      Break;
    end;
    if FObjectList[FIndex].GetInterface(IDispatch, Dispatch) then
      Variants[I] := Dispatch
    else
    begin
      Result := S_FALSE;
      Break;
    end;
  end;
   if @pceltFetched <> nil then
    pceltFetched := I;
end;

function TEnumVariant.Skip(celt: LongWord): HResult;
begin
  Result := S_FALSE;
  if FIndex + Integer(celt) > FObjectList.Count - 1 then
    FIndex := FObjectList.Count - 1
  else
  begin
    Inc(FIndex, celt);
    Result := S_OK;
  end;
end;

function TEnumVariant.Reset: HResult;
begin
  FIndex := -1;
  Result := S_OK;
end;

function TEnumVariant.Clone(out Enum: IEnumVariant): HResult;
var
  EnumVariant: TEnumVariant;
begin
  EnumVariant := TEnumVariant.Create(FObjectList);
  EnumVariant.FIndex := FIndex;
  Enum := EnumVariant as IEnumVariant;
  Result := S_OK;
end;

{ TAutoStorable }

procedure TAutoStorable.InternalExecute(const Command: OleVariant);
begin
end;

function TAutoStorable.InternalRetrieve(const Format: OleVariant): OleVariant;
begin
end;

procedure TAutoStorable.InternalUpdate(const Data: OleVariant);
begin
end;

{ TAutoStorable.IStorable }

procedure TAutoStorable.Execute(Command: OleVariant);
begin
  InternalExecute(Command);
end;

function TAutoStorable.Retrieve(Format: OleVariant): OleVariant;
begin
  Result := InternalRetrieve(Format);
end;

procedure TAutoStorable.Update(Data: OleVariant);
begin
  InternalUpdate(Data);
end;

{ TAutoCollection }

constructor TAutoCollection.Create(Collection: TGUID;
  ItemClass: TAutoIntfObjectClass; Item: TGUID);
begin
  FObjectList := TObjectList.Create(False);
  FItemClass := ItemClass;
  FItem := Item;
  { TODO: get typelib  }
  inherited Create(nil, Collection);
end;

destructor TAutoCollection.Destroy;
var
  I: Integer;
begin
  for I := 0 to FObjectList.Count - 1 do
    TAutoCollection(FObjectList[I])._Release;
  FObjectList.Free;
  inherited Destroy;
end;

function TAutoCollection.InternalAdd(const Item: OleVariant): OleVariant;
var
  AutoObject: TAutoIntfObject;
begin
  { TODO: get typelib }
  AutoObject := FItemClass.Create(nil, FItem);
  TAutoCollection(AutoObject)._AddRef;
  FObjectList.Add(AutoObject);
  Result := AutoObject as IDispatch;
end;

procedure TAutoCollection.InternalDelete(const Item: OleVariant);
var
  I: Integer;
begin
  I := Item;
  if I < FObjectList.Count - 1 then
  begin
    TAutoCollection(FObjectList[I])._Release;
    FObjectList.Delete(I);
  end;
end;

function TAutoCollection.InternalGetCount: Integer;
begin
  Result := FObjectList.Count;
end;

function TAutoCollection.InternalGetItem(const Index: OleVariant): OleVariant;
var
  AutoIntfObject: TAutoIntfObject;
  I: Integer;
begin
  I := Index;
  if (I > -1) and (I < FObjectList.Count) then
    AutoIntfObject := TAutoIntfObject(FObjectList[I])
  else
    AutoIntfObject := nil;
  if AutoIntfObject <> nil then
    Result := AutoIntfObject as IDispatch
  else
    VarClear(Result);
end;

function TAutoCollection.InternalSearch(const Params: OleVariant): OleVariant;
begin
  VarClear(Result);
end;

{ TAutoCollection.ICollection }

function TAutoCollection.Get__NewEnum: IUnknown;
begin
  Result := TEnumVariant.Create(FObjectList) as IUnknown;
end;

function TAutoCollection.Add(Item: OleVariant): OleVariant;
begin
  Result := InternalAdd(Item);
end;

procedure TAutoCollection.Delete(Item: OleVariant);
begin
  InternalDelete(Item);
end;

function TAutoCollection.Get_Count: Integer;
begin
  Result := InternalGetCount;
end;

function TAutoCollection.Get_Item(Index: OleVariant): OleVariant;
begin
  Result := InternalGetItem(Index);
end;

{ TAutoCollection.ISearchCollection }

function TAutoCollection.Search(Params: OleVariant): OleVariant;
begin
  Result := InternalSearch(Params);
end;

{ TScriptEvents }

constructor TScriptEvents.Create;
begin
  inherited Create;
  FEvents := TStringList.Create;
end;

destructor TScriptEvents.Destroy;
var
  I: Integer;
begin
  inherited Destroy;
  for I := 0 to FEvents.Count - 1 do
    IUnknown(Pointer(FEvents.Objects[I]))._Release;
  FEvents.Free;
end;

procedure TScriptEvents.AttachEvent(const Name: string; Handler: IDispatch);
begin
  if Name <> '' then
  begin
    Handler._AddRef;
    FEvents.AddObject(Name, TObject(Handler));
  end;
end;

procedure TScriptEvents.DetachEvent(const Name: string; Handler: IDispatch);
var
  I: Integer;
begin
  for I := 0 to FEvents.Count - 1 do
    if (FEvents.Objects[I] = TObject(Handler)) and (FEvents[I] = Name) then
    begin
      IDispatch(Pointer(FEvents.Objects[I]))._Release;
      FEvents.Delete(I);
      Break;
    end;
end;

procedure TScriptEvents.InvokeEvent(const Name: string);
const
  IID_NULL: TGUID = '{00000000-0000-0000-0000-000000000000}';
  DispArgs: array[0..3] of Pointer = (nil, nil, nil, nil);
var
  Dispatch: IDispatch;
  I: Integer;
begin
  for I := 0 to FEvents.Count - 1 do
    if FEvents[I] = Name then
    begin
      Dispatch := IDispatch(Pointer(FEvents.Objects[I]));
      Dispatch.Invoke(0, IID_NULL, LOCALE_SYSTEM_DEFAULT, DISPATCH_METHOD,
        DispArgs, nil, nil, nil);
    end;
end;

{ EventSink helper routines }

function GetConnection(ConnectionPoints: IConnectionPointContainer;
  EventID: TGUID): IConnectionPoint;
begin
  Result := nil;
  if ConnectionPoints <> nil then
    OleCheck(ConnectionPoints.FindConnectionPoint(EventID, Result));
end;

function ConnectEvent(Instance: IUnknown; EventID: TGUID; Event: IUnknown): Integer;
var
  ConnectionPoints: IConnectionPointContainer;
  ConnectionPoint: IConnectionPoint;
begin
  Result := 0;
  if Supports(Instance, IConnectionPointContainer,
    ConnectionPoints) then
  begin
    ConnectionPoint := GetConnection(ConnectionPoints, EventID);
    if (ConnectionPoint <> nil) then
      OleCheck(ConnectionPoint.Advise(Event, Result));
  end;
end;

procedure DisconnectEvent(Instance: IUnknown; EventID: TGUID; Cookie: Integer);
var
  ConnectionPoints: IConnectionPointContainer;
  ConnectionPoint: IConnectionPoint;
begin
  if (Cookie <> 0) and Supports(Instance, IConnectionPointContainer,
    ConnectionPoints) then
  begin
    ConnectionPoint := GetConnection(ConnectionPoints, EventID);
    if (ConnectionPoint <> nil) then
      OleCheck(ConnectionPoint.Unadvise(Cookie));
  end;
end;

{ var
  Document: OleVariant;
  List: OleVariant;
  I: Integer;
begin
  Document := CreateOleObject('MSXML.DOMDocument');
  Document.Load('c:\temp\test.xml');
  List := Document.GetElementsByTagName('person');
  for I := 0 to List.Length - 1 do
    ShowMessage(List.Item(I).Text);
end; }

end.
