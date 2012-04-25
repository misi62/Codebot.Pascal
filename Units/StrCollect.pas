
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit StrCollect;

interface

{$I CODEBOT.INC}

uses
  QuickString, SysUtils, Classes, Contnrs;

{ TNamedStrings }

type
  TNamedStrings = class(TPersistent)
  private
    FList: TObjectList;
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);
    function GetBody(Index: Integer): string;
    procedure SetBody(Index: Integer; const Value: string);
    function GetCount: Integer;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    { procedure ReadStrings(Reader: TReader);
    procedure WriteStrings(Writer: TWriter); }
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Add(const Item: string; const Body: string);
    procedure Clear;
    function Find(const Item: string; out Body: string): Boolean;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Remove(const Item: string);
    procedure Replace(const Item: string; const Body: string);
    property Item[Index: Integer]: string read GetItem write SetItem;
    property Body[Index: Integer]: string read GetBody write SetBody;
    property Count: Integer read GetCount;
  end;

{ TNamedStringCollector }

  TNamedStringCollector = class(TComponent)
  private
    FNamedStrings: TNamedStrings;
    procedure SetNamedStrings(Value: TNamedStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Find(const Item: string): string; overload;
    function Find(const Item: string; const Param, Value: string): string; overload;
    function Find(const Item: string; const Params: array of string; const Values: array of string): string; overload;
  published
    property Items: TNamedStrings read FNamedStrings write SetNamedStrings;
  end;

implementation

type
  TStringItem = class
    Item: string;
    Body: string;
  end;

constructor TNamedStrings.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TNamedStrings.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TNamedStrings.Assign(Source: TPersistent);
var
  NamedStrings: TNamedStrings absolute Source;
  StringItem: TStringItem;
  I: Integer;
begin
  if Source is TNamedStrings then
  begin
    Clear;
    for I := 0 to NamedStrings.Count - 1 do
    begin
      StringItem := TStringItem.Create;
      StringItem.Item := NamedStrings.Item[I];
      StringItem.Body := NamedStrings.Body[I];
      FList.Add(StringItem);
    end;
  end
  else
    inherited Assign(Source);
end;

function FindItem(List: TObjectList; const Item: string): TStringItem;
var
  StringItem: TStringItem;
  S: string;
  I: Integer;
begin
  Result := nil;
  S := UpperCase(Item);
  for I := 0 to List.Count - 1 do
  begin
    StringItem := TStringItem(List[I]);
    if S = UpperCase(StringItem.Item) then
    begin
      Result := StringItem;
      Break;
    end;
  end;
end;

procedure TNamedStrings.DefineProperties(Filer: TFiler);

  function WriteNamedStrings: Boolean;
  var
    NamedStrings: TNamedStrings;
    I: Integer;
  begin
    NamedStrings := TNamedStrings(Filer.Ancestor);
    if NamedStrings = nil then
      Result := Count > 0
    else if NamedStrings.Count <> Count then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := (NamedStrings.Item[I] <> Item[I]) or
          (NamedStrings.Body[I] <> body[I]);
        if Result then
          Break;
      end
    end;
  end;

begin
  inherited DefineProperties(Filer);
  // Filer.DefineProperty('Strings', ReadStrings, WriteStrings, WriteNamedStrings);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, WriteNamedStrings);
end;

function ReadInteger(Stream: TStream): Integer;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

procedure WriteInteger(Stream: TStream; Value: Integer);
begin
  Stream.WriteBuffer(Value, SizeOf(Value));
end;

function ReadString(Stream: TStream): string;
begin
  SetLength(Result, ReadInteger(Stream));
  Stream.ReadBuffer(Pointer(Result)^, Length(Result) * SizeOf(Char));
end;

procedure WriteString(Stream: TStream; const Value: string);
begin
  WriteInteger(Stream, Length(Value));
  Stream.WriteBuffer(Pointer(Value)^, Length(Value) * SizeOf(Char));
end;

procedure TNamedStrings.ReadData(Stream: TStream);
var
  StringItem: TStringItem;
  I: Integer;
begin
  Clear;
  for I := 0 to ReadInteger(Stream) - 1 do
  begin
    StringItem := TStringItem.Create;
    StringItem.Item := ReadString(Stream);
    StringItem.Body := ReadString(Stream);
    FList.Add(StringItem);
  end;
end;

procedure TNamedStrings.WriteData(Stream: TStream);
var
  StringItem: TStringItem;
  I: Integer;
begin
  WriteInteger(Stream, Count);
  for I := 0 to Count - 1 do
  begin
    StringItem := TStringItem(FList[I]);
    WriteString(Stream, StringItem.Item);
    WriteString(Stream, StringItem.Body);
  end;
end;

{procedure TNamedStrings.ReadStrings(Reader: TReader);
var
  S: string;
  I: Integer;
begin
  Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    S := Reader.ReadString;
    I := Pos(S, ':');
    if I > 0 then
      Add(Copy(S, 1, I - 1), Copy(S, I + 1, Length(S)));
  end;
  Reader.ReadListEnd;
end;

procedure TNamedStrings.WriteStrings(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteString(Item[I] + ':' + Body[I]);
  Writer.WriteListEnd;
end;}

procedure TNamedStrings.Add(const Item: string; const Body: string);
var
  StringItem: TStringItem;
  S: string;
begin
  S := Trim(Item);
  if S = '' then Exit;
  StringItem := FindItem(FList, S);
  if StringItem = nil then
  begin
    StringItem := TStringItem.Create;
    StringItem.Item := S;
    StringItem.Body := Body;
    FList.Add(StringItem);
  end
  else
    StringItem.Body := Body;
end;

procedure TNamedStrings.Clear;
begin
  FList.Clear;
end;

function TNamedStrings.Find(const Item: string; out Body: string): Boolean;
var
  StringItem: TStringItem;
  S: string;
begin
  Result := False;
  Body := '';
  S := Trim(Item);
  if S = '' then Exit;
  StringItem := FindItem(FList, S);
  Result := StringItem <> nil;
  if Result then
    Body := StringItem.Body;
end;

procedure TNamedStrings.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TNamedStrings.Remove(const Item: string);
var
  StringItem: TStringItem;
  S: string;
begin
  S := Trim(Item);
  if S = '' then Exit;
  StringItem := FindItem(FList, S);
  if StringItem <> nil then
    FList.Remove(StringItem);
end;

procedure TNamedStrings.Replace(const Item: string; const Body: string);
var
  StringItem: TStringItem;
  S: string;
begin
  S := Trim(Item);
  if S = '' then Exit;
  StringItem := FindItem(FList, S);
  if StringItem <> nil then
    StringItem.Body := Body;
end;

function TNamedStrings.GetItem(Index: Integer): string;
begin
  Result := TStringItem(FList[Index]).Item;
end;

procedure TNamedStrings.SetItem(Index: Integer; const Value: string);
var
  StringItem: TStringItem;
  S: string;
begin
  S := Trim(Value);
  if S = '' then Exit;
  StringItem := FindItem(FList, S);
  if StringItem = nil then
    TStringItem(FList[Index]).Item := S;
end;

function TNamedStrings.GetBody(Index: Integer): string;
begin
  Result := TStringItem(FList[Index]).Body;
end;

procedure TNamedStrings.SetBody(Index: Integer; const Value: string);
begin
  TStringItem(FList[Index]).Body := Value;
end;

function TNamedStrings.GetCount: Integer;
begin
  Result := FList.Count;
end;


constructor TNamedStringCollector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNamedStrings := TNamedStrings.Create;
end;

destructor TNamedStringCollector.Destroy;
begin
  FNamedStrings.Free;
  inherited Destroy;
end;

function TNamedStringCollector.Find(const Item: string): string;
begin
  FNamedStrings.Find(Item, Result);
end;

function TNamedStringCollector.Find(const Item: string; const Param, Value: string): string;
begin
  if FNamedStrings.Find(Item, Result) then
    Result := QuickReplace(Result, ':' + Param, Value)
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

function TNamedStringCollector.Find(const Item: string; const Params: array of string;
  const Values: array of string): string;
var
  I: Integer;
begin
  if FNamedStrings.Find(Item, Result) then
    for I := Max(Low(Params), Low(Values)) to Min(High(Params), High(Values)) do
      Result := QuickReplace(Result, ':' + Params[I], Values[I]);
      //Result := StringReplace(Result, ':' + Params[I], Values[I], [rfReplaceAll]);
end;

procedure TNamedStringCollector.SetNamedStrings(Value: TNamedStrings);
begin
  FNamedStrings.Assign(Value);
end;

end.
