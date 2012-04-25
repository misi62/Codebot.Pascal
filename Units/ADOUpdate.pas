unit ADOUpdate;

interface

uses
	Windows, ActiveX, Classes, DB, ADOdb, SysUtils;

{ TADOUpdateQuery }

type
  TADOUpdateSQL = class;

  TADOUpdateQuery = class (TADOQuery)
  private
    FDelRecords: TADOQuery;
    FUpdateObject: TADOUpdateSQL;
    procedure SetUpdateObject(Value: TADOUpdateSQL);
    procedure ClearBuffer;
    procedure InitBuffer;
    procedure FillBuffer;
    procedure ApplyDelUpdates;
  protected
    procedure InternalDelete; override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyUpdates;
    procedure CancelUpdates;
    procedure CommitUpdates;
    procedure ReadBlob(const FieldName: string; Strings: TStrings);
    function ReadInt(const FieldName: string): Integer;
    function ReadVar(const FieldName: string): OleVariant;
  published
    property UpdateObject: TADOUpdateSQL read FUpdateObject write SetUpdateObject;
  end;

{ TADOUpdateSQL }

  TADOUpdateSQL = class(TComponent)
  private
    FDataSet: TADOUpdateQuery;
    FQueries: array[TUpdateKind] of TADOQuery;
    FSQLText: array[TUpdateKind] of TStrings;
    function GetQuery(UpdateKind: TUpdateKind): TADOQuery;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    procedure SetSQLIndex(Index: Integer; Value: TStrings);
  protected
    function GetSQL(UpdateKind: TUpdateKind): TStrings; virtual;
    function GetDataSet: TADOUpdateQuery; virtual;
    procedure SetDataSet(ADataSet: TADOUpdateQuery); virtual;
    procedure SQLChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Apply(UpdateKind: TUpdateKind);  virtual;
    procedure ExecSQL(UpdateKind: TUpdateKind);
    procedure SetParams(UpdateKind: TUpdateKind);
    property DataSet: TADOUpdateQuery read GetDataSet write SetDataSet;
    property Query[UpdateKind: TUpdateKind]: TADOQuery read GetQuery;
    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
    property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
    property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
    property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
  end;

implementation

{ TADOUpdateSQL }

constructor TADOUpdateSQL.Create(AOwner: TComponent);
var
  UpdateKind: TUpdateKind;
begin
  inherited Create(AOwner);
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;
end;

destructor TADOUpdateSQL.Destroy;
var
  UpdateKind: TUpdateKind;
begin
  if (FDataSet <> nil) and (FDataSet.UpdateObject = Self) then
    FDataSet.UpdateObject := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
  inherited Destroy;
end;

procedure TADOUpdateSQL.ExecSQL(UpdateKind: TUpdateKind);
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

function TADOUpdateSQL.GetQuery(UpdateKind: TUpdateKind): TADOQuery;
begin
  if not Assigned(FQueries[UpdateKind]) then
  begin
    FQueries[UpdateKind] := TADOQuery.Create(Self);
    FQueries[UpdateKind].Connection := FDataSet.Connection;
    FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
  end;
  Result := FQueries[UpdateKind];
end;

function TADOUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result := FSQLText[UpdateKind];
end;

function TADOUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
  Result := FSQLText[TUpdateKind(Index)];
end;

function TADOUpdateSQL.GetDataSet: TADOUpdateQuery;
begin
  Result := FDataSet;
end;

procedure TADOUpdateSQL.SetDataSet(ADataSet: TADOUpdateQuery);
begin
  FDataSet := ADataSet;
end;

procedure TADOUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TADOUpdateSQL.SetSQLIndex(Index: Integer; Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

procedure TADOUpdateSQL.SQLChanged(Sender: TObject);
var
  UpdateKind: TUpdateKind;
begin
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    if Sender = FSQLText[UpdateKind] then
    begin
      if Assigned(FQueries[UpdateKind]) then
      begin
        FQueries[UpdateKind].Parameters.Clear;
        FQueries[UpdateKind].SQL.Assign(FSQLText[UpdateKind]);
      end;
      Break;
    end;
end;

procedure TADOUpdateSQL.SetParams(UpdateKind: TUpdateKind);
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

procedure TADOUpdateSQL.Apply(UpdateKind: TUpdateKind);
begin
  SetParams(UpdateKind);
  ExecSQL(UpdateKind);
end;

{ TADOUpdateQuery }

procedure TADOUpdateQuery.ApplyUpdates;
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
		if not FDelRecords.IsEmpty then
		begin
      ApplyDelUpdates;
      ClearBuffer;
		end;
  finally
		Filtered := F;
		GotoBookmark(BM);
		FreeBookmark(BM);
		EnableControls;
  end;
end;

procedure TADOUpdateQuery.CancelUpdates;
begin
  inherited CancelUpdates;
  ClearBuffer;
end;

constructor TADOUpdateQuery.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 LockType := ltBatchOptimistic;
 FDelRecords := TADOQuery.Create(self);
end;

procedure TADOUpdateQuery.InitBuffer;
begin
	if FDelRecords.FieldDefs.Count = 0 then
  begin
	  FDelRecords.Connection := Connection;
	  FDelRecords.FieldDefs.Assign(FieldDefs);
	  TADODataSet(FDelRecords).CreateDataSet;
  end;
end;

destructor TADOUpdateQuery.Destroy;
begin
  FDelRecords.Free;
  inherited Destroy;
end;

procedure TADOUpdateQuery.InternalDelete;
begin
 if not (RecordStatus = [rsNew]) then
   FillBuffer;
 inherited InternalDelete;
end;

procedure TADOUpdateQuery.SetUpdateObject(Value: TADOUpdateSQL);
begin
  if Value <> FUpdateObject then
  begin
    if (FUpdateObject <> nil) and (FUpdateObject.DataSet = Self) then
      FUpdateObject.DataSet := nil;
    FUpdateObject := Value;
    if FUpdateObject <> nil then
    begin
      if (FUpdateObject.DataSet <> nil) and (FUpdateObject.DataSet <> Self) then
        FUpdateObject.DataSet.UpdateObject := nil;
      FUpdateObject.DataSet := Self;
    end;
  end;
end;

procedure TADOUpdateQuery.ClearBuffer;
begin
	if not FDelRecords.IsEmpty then
		while not FDelRecords.Bof do
      FDelRecords.Delete;
end;

procedure TADOUpdateQuery.FillBuffer;
var
  I: Integer;
begin
  InitBuffer;
  FDelRecords.Append;
	for I:= 0 to Fields.Count - 1 do
		if Fields[I].FieldKind = fkData then
         FDelRecords.Fields[I].Assign(Fields[I]);
  FDelRecords.Post;
end;

procedure TADOUpdateQuery.ApplyDelUpdates;
var
  Q: TADOUpdateQuery;
begin
  Q := FUpdateObject.FDataSet;
  FUpdateObject.FDataSet := TADOUpdateQuery(FDelRecords);
  FDelRecords.First;
  while not FDelRecords.Eof do
	begin
    UpdateObject.Apply(ukDelete);
    FDelRecords.Next;
	end;
  FUpdateObject.FDataSet:=Q;
end;

procedure TADOUpdateQuery.CommitUpdates;
begin
	Requery;
end;

function TADOUpdateQuery.ReadVar(const FieldName: string): OleVariant;
begin
  Result := Self[FieldName];
  if TPropVariant(Result).vt = VT_NULL then
    Result := '';
end;

procedure TADOUpdateQuery.ReadBlob(const FieldName: string; Strings: TStrings);
var
  Stream: TStream;
begin
  Stream := CreateBlobStream(FieldByName(FieldName), bmRead);
  Strings.LoadFromStream(Stream);
  Stream.Free;
end;

function TADOUpdateQuery.ReadInt(const FieldName: string): Integer;
var
  S: string;
begin
  S := ReadVar(FieldName);
  Result := StrToIntDef(S, -1);
end;

end.