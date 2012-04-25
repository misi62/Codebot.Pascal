unit PhotoCollection;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, GraphTools;

{ TPhotoItem }

type
  TPhotoItem = class(TCollectionItem)
  private
  	FPhoto: IGlassImage;
    FFileName: string;
		procedure SetFileName(const Value: string);
	protected
  	procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
	published
    property Photo: IGlassImage read FPhoto;
    property FileName: string read FFileName write SetFileName;
  end;

{ TPhotoItems }

  TPhotoItems = class(TOwnedCollection)
  private
  	FOnChange: TNotifyEvent;
    function Get(Index: Integer): TPhotoItem;
    procedure Put(Index: Integer; Value: TPhotoItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add(const FileName: string): TPhotoItem; overload;
    function Add(Stream: TStream): TPhotoItem; overload;
    function FindItemID(ID: Integer): TPhotoItem;
    function Insert(Index: Integer): TPhotoItem;
    procedure Swap(A, B: Integer);
    property Items[Index: Integer]: TPhotoItem read Get write Put; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TPhotoItem }

constructor TPhotoItem.Create(Collection: TCollection);
begin
	inherited Create(Collection);
  FPhoto := CreateGlassImage;
end;

procedure TPhotoItem.AssignTo(Dest: TPersistent);
begin
	if Dest is TGraphic then
  	TGraphic(Dest).Assign(FPhoto.Graphic)
	else if Dest is TPicture then
  	TPicture(Dest).Graphic := FPhoto.Graphic
	else
	  inherited AssignTo(Dest);
end;

procedure TPhotoItem.Assign(Source: TPersistent);
var
  EditItem: TPhotoItem absolute Source;
begin
  if Source is TPhotoItem then
  begin
    FPhoto.Graphic.Assign(Source);
    FFileName := TPhotoItem(Source).FileName;
	end
  else if Source is TGraphic then
    FPhoto.Graphic.Assign(Source)
  else if Source is TPicture then
    FPhoto.Graphic.Assign(TPicture(Source).Graphic)
  else
    inherited Assign(Source);
  Changed(False);
end;

procedure TPhotoItem.SetFileName(const Value: string);
var
  Picture: TPicture;
begin
	Picture := TPicture.Create;
  try
  	Picture.LoadFromFile(Value);
    Assign(Picture);
    FFileName := Value;
  finally
  	Picture.Free;
  end;
end;

procedure TPhotoItem.LoadFromStream(Stream: TStream);
begin
	FPhoto.Graphic.SaveToStream(Stream);
  Changed(False);
end;

procedure TPhotoItem.SaveToStream(Stream: TStream);
begin
	FPhoto.Graphic.LoadFromStream(Stream);;
  Changed(False);
end;

{ TPhotoItems }

constructor TPhotoItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TPhotoItem);
end;

function TPhotoItems.Add(const FileName: string): TPhotoItem;
begin
	BeginUpdate;
  try
	  Result := TPhotoItem(inherited Add);
  	Result.FileName := FileName;
	finally
  	EndUpdate;
  end;
end;

function TPhotoItems.Add(Stream: TStream): TPhotoItem;
begin
	BeginUpdate;
  try
	  Result := TPhotoItem(inherited Add);
  	Result.LoadFromStream(Stream);
	finally
  	EndUpdate;
  end;
end;

function TPhotoItems.FindItemID(ID: Integer): TPhotoItem;
begin
  Result := TPhotoItem(inherited FindItemID(ID));
end;

function TPhotoItems.Insert(Index: Integer): TPhotoItem;
begin
  Result := TPhotoItem(GetItem(Index));
end;

procedure TPhotoItems.Swap(A, B: Integer);
var
	Photo: IGlassImage;
	FileName: string;
begin
	Photo := Get(A).FPhoto;
	FileName := Get(A).FFileName;
  Get(A).FPhoto := Get(B).FPhoto;
  Get(A).FFileName := Get(B).FFileName;
  Get(B).FPhoto := Photo;
  Get(B).FFileName := FileName;
  Changed;
end;

procedure TPhotoItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
	if Assigned(FOnChange) then
  	FOnChange(Self);
end;

function TPhotoItems.Get(Index: Integer): TPhotoItem;
begin
  Result := TPhotoItem(GetItem(Index));
end;

procedure TPhotoItems.Put(Index: Integer; Value: TPhotoItem);
begin
  SetItem(Index, Value);
end;

end.
