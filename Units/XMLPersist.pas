unit XMLPersist;

interface

uses
	SysUtils, Classes, StrTools, XMLParser;

{ TPersistData }

type
	TPersistData = class(TPersistent)
  private
    FDefaults: IDocument;
  	FDocument: IDocument;
    FFileName: string;
    function GetPath(Index: Integer): string;
  public
  	constructor Create(const FileName, Defaults: string);
		destructor Destroy; override;
    property Path[Index: Integer]: string read GetPath;
  end;

{ TEventList }

  IEventListener = interface
		procedure ChangeEvent(Sender: TObject; EventId: Integer = 0);
  end;

  IEventList = interface
  	procedure Change;
  	procedure Subscribe(Listener: IEventListener);
  	procedure Unsubscribe(Listener: IEventListener);
  end;

  TEventList = class(TInterfacedObject, IEventList)
  private
  	FOwner: TObject;
    FListeners: IInterfaceList;
	protected
  	{ IEventList }
  	procedure Change;
  	procedure Subscribe(Listener: IEventListener);
  	procedure Unsubscribe(Listener: IEventListener);
	public
  	constructor Create(AOwner: TObject);
  end;

{ TUndoList }

  TUndoList = class(TPersistent)
  private
  	FChanged: Boolean;
    FChangeEvents: IEventList;
  	FList: TStringArray;
  	FIndex: Integer;
    FCount: Integer;
  	FDocument: IDocument;
    FSuspended: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetState: IDocument;
    function Get(Index: Integer): IDocument;
    procedure Put(Index: Integer; Value: IDocument);
	protected
  	procedure Change;
	public
  	constructor Create(InitialState: IDocument; Levels: Integer = 100);
    procedure Assign(Source: TPersistent); override;
  	procedure Update(State: IDocument);
    procedure Clear(InitialState: IDocument = nil);
    procedure Undo;
    procedure Redo;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
    property ChangeEvents: IEventList read FChangeEvents;
    property Count: Integer read FCount;
    property ItemIndex: Integer read FIndex;
    property Items[Index: Integer]: IDocument read Get write Put; default;
    property State: IDocument read GetState;
    property Suspended: Boolean read FSuspended write FSuspended;
  end;

implementation

uses
	RTLConsts;

{ TPersistData }

constructor TPersistData.Create(const FileName, Defaults: string);
var
	Root: INode;
  S: string;
begin
	FFileName := FileName;
	FDefaults := CreateDocument;
  FDefaults.Text := Defaults;
  FDocument := CreateDocument;
  if FileExists(FFileName) then
	try
  	FDocument.LoadFromFile(FFileName);
  except
	  FDocument := CreateDocument;
  end;
  S := FDefaults.Root.FindNode('//root/text()').Text;
  Root := FDocument.Root;
  if (Root = nil) or (Root.Name <> S) then
  begin
  	Root := FDocument.CreateNode(S);
    FDocument.Root := Root;
	end;
end;

destructor TPersistData.Destroy;
begin
	try
  	FDocument.SaveToFile(FFileName);
  except
	  FDocument := nil;
  end;
  inherited;
end;

function TPersistData.GetPath(Index: Integer): string;
var
	Node: INode;
begin
	Result := '';
	Node := FDefaults.Root.FindNode('//path[@id="' + IntToStr(Index) +  '"]');
  if Node = nil then Exit;
  Result := FDocument.Root.Filer.ReadString(Node.Attributes['key'].Value, Node.Value);
  if Length(Result) > 1 then
  	if (Pos('//', Result) = 0) and (Result[2] <> ':') then
    	Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Result;
end;
{ TEventList }


constructor TEventList.Create(AOwner: TObject);
begin
	FOwner := AOwner;
  FListeners := TInterfaceList.Create;
end;

procedure TEventList.Change;
var
	I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
  	IEventListener(FListeners[I]).ChangeEvent(FOwner);
end;

procedure TEventList.Subscribe(Listener: IEventListener);
begin
	if FListeners.IndexOf(Listener) < 0 then
  	FListeners.Add(Listener);
end;

procedure TEventList.Unsubscribe(Listener: IEventListener);
begin
	FListeners.Remove(Listener);
end;

{ TUndoList }

const
	EmptyDocument = '';

constructor TUndoList.Create(InitialState: IDocument; Levels: Integer = 100);
begin
	inherited Create;
	if Levels < 1 then
  	Levels := 1
  else if Levels > 1000 then
  	Levels := 1000;
  SetLength(FList, Levels);
  FIndex := 0;
  FCount := 1;
  FList[FIndex] := InitialState.Text;
  FChangeEvents := TEventList.Create(Self);
end;

procedure TUndoList.Assign(Source: TPersistent);
var
	Undo: TUndoList absolute Source;
  I: Integer;
begin
	if Suspended then Exit;
	if Source is TUndoList then
  begin
    FCount := Undo.FCount;
  	FIndex := Undo.FIndex;
    FDocument := nil;
  	SetLength(FList, Length(Undo.FList));
    for I := 0 to Length(FList) - 1 do
			FList[I] := Undo.FList[I];
		Change;
  end
  else
  	inherited Assign(Source);
end;

procedure TUndoList.Change;
begin
	FChanged := True;
  FChangeEvents.Change;
end;

procedure TUndoList.Clear(InitialState: IDocument = nil);
var
	I: Integer;
begin
	if FSuspended then Exit;
 	FIndex := 0;
  FCount := 1;
	if InitialState <> nil then
   	FList[FIndex] := InitialState.Text;
 	for I := 1 to Length(FList) - 1 do
  	FList[I] := EmptyDocument;
	Change;
end;

procedure TUndoList.Update(State: IDocument);
var
	I: Integer;
begin
	if FSuspended then Exit;
	if FIndex < Length(FList) - 1 then
  begin
  	Inc(FIndex);
    FCount := FIndex + 1;
    for I := FIndex + 1 to Length(FList) - 1 do
    	FList[I] := EmptyDocument;
  end
  else for I := 0 to Length(FList) - 2 do
		FList[I] := FList[I + 1];
  FList[FIndex] := State.Text;
	Change;
end;

procedure TUndoList.Redo;
begin
  if CanRedo then
  begin
  	Inc(FIndex);
  	Change;
  end;
end;

procedure TUndoList.Undo;
begin
  if CanUndo then
  begin
  	Dec(FIndex);
  	Change;
  end;
end;

function TUndoList.GetState: IDocument;
begin
	Result := nil;
	if FIndex < 0 then Exit;
	if FChanged then
  begin
  	FDocument := CreateDocument;
    FDocument.Text := FList[FIndex];
		FChanged := False;
	end;
  Result := FDocument;
end;

function TUndoList.GetCanRedo: Boolean;
begin
	Result := FCount > FIndex + 1;
  if FSuspended then Result := False;
end;

function TUndoList.GetCanUndo: Boolean;
begin
	Result := FIndex > 0;
  if FSuspended then Result := False;
end;

function TUndoList.Get(Index: Integer): IDocument;
begin
  if (Index < 0) or (Index > FCount - 1) then
    TList.Error(@SListIndexError, Index);
	if Index = FIndex then
		Result := State
	else
  begin
  	Result := CreateDocument;
    Result.Text := FList[Index];
  end;
end;

procedure TUndoList.Put(Index: Integer; Value: IDocument);
begin
	if FSuspended then Exit;
  if (Index < 0) or (Index > FCount - 1) then
    TList.Error(@SListIndexError, Index);
	if Index = FIndex then
		Update(Value)
	else
  	FList[Index] := Value.Text;
end;

end.
