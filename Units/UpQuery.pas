unit UpQuery;

interface

uses
  Windows, ActiveX, Classes, DB, ADOdb, SysUtils;

{ TUpdateQuery }

type
  TUpdateObject = class;

  TUpdateQuery = class (TADOQuery)
  private
    FDeleteRecords: TADOQuery;
    FUpdateObject: TUpdateObject;
    procedure ClearBuffer;
    procedure InitBuffer;
    procedure FillBuffer;
    procedure ApplyDeleteUpdates;
    function GetSqlIndex(Index: Integer): TStrings;
    procedure SetSqlIndex(Index: Integer; Value: TStrings);
  protected
    procedure InternalDelete; override;
    property UpdateObject: TUpdateObject read FUpdateObject;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyUpdates;
    procedure CancelUpdates;
    procedure CommitUpdates;
  published
    property SqlUpdate: TStrings index 0 read GetSqlIndex write SetSqlIndex;
    property SqlInsert: TStrings index 1 read GetSqlIndex write SetSqlIndex;
    property SqlDelete: TStrings index 2 read GetSqlIndex write SetSqlIndex;
  end;

{ TUpdateObject }

  TUpdateObject = class(TObject)
  private
    FDataSet: TUpdateQuery;
    FQueries: array[TUpdateKind] of TADOQuery;
    FSqlText: array[TUpdateKind] of TStrings;
    function GetQuery(UpdateKind: TUpdateKind): TADOQuery;
    function GetSqlIndex(Index: Integer): TStrings;
    procedure SetSqlIndex(Index: Integer; Value: TStrings);
    function GetSql(UpdateKind: TUpdateKind): TStrings;
    procedure SetSql(UpdateKind: TUpdateKind; Value: TStrings);
  protected
    function GetDataSet: TUpdateQuery; virtual;
    procedure SetDataSet(ADataSet: TUpdateQuery); virtual;
    procedure SqlChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind);  virtual;
    procedure Execute(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);
    property DataSet: TUpdateQuery read GetDataSet write SetDataSet;
    property Query[UpdateKind: TUpdateKind]: TADOQuery read GetQuery;
    property Sql[UpdateKind: TUpdateKind]: TStrings read GetSql write SetSql;
    property SqlModify: TStrings index 0 read GetSqlIndex write SetSqlIndex;
    property SqlInsert: TStrings index 1 read GetSqlIndex write SetSqlIndex;
    property SqlDelete: TStrings index 2 read GetSqlIndex write SetSqlIndex;
  end;

procedure ReadBlob(Query: TADOQuery; const FieldName: string; Strings: TStrings);
function ReadInt(Query: TADOQuery; const FieldName: string): Integer;
function ReadVar(Query: TADOQuery; const FieldName: string): OleVariant;
function ReadBool(Query: TADOQuery; const FieldName: string): Boolean;

implementation

{ TUpdateQuery }

constructor TUpdateQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LockType := ltBatchOptimistic;
  FDeleteRecords := TADOQuery.Create(self);
  FUpdateObject := TUpdateObject.Create;
  FUpdateObject.DataSet := Self;
end;

destructor TUpdateQuery.Destroy;
begin
  FDeleteRecords.Free;
  FUpdateObject.Free;
  inherited Destroy;
end;

procedure TUpdateQuery.ApplyUpdates;
var
  BM: TBookmark;
  F: Boolean;
begin
  DisableControls;
  BM := GetBookmark;
  F := Filtered;
  Filtered := False;
  try
    First;
    while not Eof do
    begin
      if RecordStatus = [rsNew] then
        UpdateObject.Apply(ukInsert) else
      if RecordStatus = [rsModified] then
        UpdateObject.Apply(ukModify);
      Next;
    end;
    if not FDeleteRecords.IsEmpty then
    begin
      ApplyDeleteUpdates;
      ClearBuffer;
    end;
  finally
    Filtered := F;
    GotoBookmark(BM);
    FreeBookmark(BM);
    EnableControls;
  end;
end;

procedure TUpdateQuery.CancelUpdates;
begin
  inherited CancelUpdates;
  ClearBuffer;
end;

procedure TUpdateQuery.InitBuffer;
begin
  if FDeleteRecords.FieldDefs.Count = 0 then
  begin
    FDeleteRecords.Connection := Connection;
    FDeleteRecords.FieldDefs.Assign(FieldDefs);
    TADODataSet(FDeleteRecords).CreateDataSet;
  end;
end;

procedure TUpdateQuery.InternalDelete;
begin
  if not (RecordStatus = [rsNew]) then
    FillBuffer;
  inherited InternalDelete;
end;

procedure TUpdateQuery.ClearBuffer;
begin
  if not FDeleteRecords.IsEmpty then
    while not FDeleteRecords.Bof do
      FDeleteRecords.Delete;
end;

procedure TUpdateQuery.FillBuffer;
var
  I: Integer;
begin
  InitBuffer;
  FDeleteRecords.Append;
  for I:= 0 to Fields.Count - 1 do
    if Fields[I].FieldKind = fkData then
         FDeleteRecords.Fields[I].Assign(Fields[I]);
  FDeleteRecords.Post;
end;

procedure TUpdateQuery.ApplyDeleteUpdates;
var
  Q: TUpdateQuery;
begin
  Q := FUpdateObject.FDataSet;
  FUpdateObject.FDataSet := TUpdateQuery(FDeleteRecords);
  FDeleteRecords.First;
  while not FDeleteRecords.Eof do
  begin
    UpdateObject.Apply(ukDelete);
    FDeleteRecords.Next;
  end;
  FUpdateObject.FDataSet:=Q;
end;

procedure TUpdateQuery.CommitUpdates;
begin
  Requery;
end;

function TUpdateQuery.GetSqlIndex(Index: Integer): TStrings;
begin
  Result := FUpdateObject.GetSqlIndex(Index);
end;

procedure TUpdateQuery.SetSqlIndex(Index: Integer; Value: TStrings);
begin
  FUpdateObject.SetSqlIndex(Index, Value);
end;

{ TUpdateObject }

constructor TUpdateObject.Create;
var
  UpdateKind: TUpdateKind;
begin
  inherited Create;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSqlText[UpdateKind] := TStringList.Create;
    TStringList(FSqlText[UpdateKind]).OnChange := SqlChanged;
  end;
end;

destructor TUpdateObject.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSqlText[UpdateKind].Free;
  inherited Destroy;
end;

procedure TUpdateObject.Execute(UpdateKind: TUpdateKind);
var
  Q: TADOQuery;
  C: TADOConnection;
begin
  Q := Query[UpdateKind];
  C := Q.Connection;
  if C <> nil then
    C.Errors.Clear;
  Q.ExecSQL;
  if (C <> nil) and (C.Errors.Count > 0) then
    DatabaseError(C.Errors[0].Description);
end;

function TUpdateObject.GetQuery(UpdateKind: TUpdateKind): TADOQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TADOQuery.Create(nil);
    FQueries[UpdateKind].Connection := FDataSet.Connection;
    FQueries[UpdateKind].SQL.Assign(FSqlText[UpdateKind]);
  end;
  Result := FQueries[UpdateKind];
end;

function TUpdateObject.GetSql(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSqlText[UpdateKind];
end;

function TUpdateObject.GetSqlIndex(Index: Integer): TStrings;
begin
  Result := FSqlText[TUpdateKind(Index)];
end;

function TUpdateObject.GetDataSet: TUpdateQuery;
begin
  Result := FDataSet;
end;

procedure TUpdateObject.SetDataSet(ADataSet: TUpdateQuery);
begin
  FDataSet := ADataSet;
end;

procedure TUpdateObject.SetSql(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSqlText[UpdateKind].Assign(Value);
end;

procedure TUpdateObject.SetSqlIndex(Index: Integer; Value: TStrings);
begin
  SetSql(TUpdateKind(Index), Value);
end;

procedure TUpdateObject.SqlChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSqlText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
      begin
        FQueries[UpdateKind].Parameters.Clear;
        FQueries[UpdateKind].SQL.Assign(FSqlText[UpdateKind]);
      end;
      Break;
    end;
end;

procedure TUpdateObject.SetParams(UpdateKind: TUpdateKind);
var
  Old: Boolean;
  Param: TParameter;
  Name: string;
  Field: TField;
  Q: TADOQuery;
  I: Integer;
begin
  if FDataSet = nil then Exit;
  Q := Query[UpdateKind];
  for I := 0 to Q.Parameters.Count - 1 do
  begin
    Param := Q.Parameters.Items[I];
    Name := Param.Name;
    Old := CompareText(Copy(Name, 1, 4), 'OLD_') = 0;
    if Old then
      System.Delete(Name, 1, 4);
    Old:= Old and (UpdateKind <> ukDelete);
    Field := FDataSet.FindField(Name);
    if Field = nil then Continue;
    if Old then
    begin
      Param.DataType := Field.DataType;
      Param.Value := Field.OldValue;
    end
    else
    begin
      Param.DataType := Field.DataType;
      Param.Value := Field.Value;
    end;
  end;
end;

procedure TUpdateObject.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  Execute(UpdateKind);
end;

function ReadVar(Query: TADOQuery; const FieldName: string): OleVariant;
begin
  Result := Query[FieldName];
  if TPropVariant(Result).vt = VT_NULL then
    Result := '';
end;

function ReadInt(Query: TADOQuery; const FieldName: string): Integer;
var
  S: string;
begin
  S := ReadVar(Query, FieldName);
  Result := StrToIntDef(S, -1);
end;

procedure ReadBlob(Query: TADOQuery; const FieldName: string; Strings: TStrings);
var
  Stream: TStream;
begin
  Stream := Query.CreateBlobStream(Query.FieldByName(FieldName), bmRead);
  Strings.LoadFromStream(Stream);
  Stream.Free;
end;

function ReadBool(Query: TADOQuery; const FieldName: string): Boolean;
var
  S: string;
begin
  S := UpperCase(ReadVar(Query, FieldName));
  if (S = 'Y') or (S = '1') or (S = 'T') then
    Result := True
  else
    Result := False;
end;

end.
