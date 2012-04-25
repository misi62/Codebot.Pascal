unit ResourceData;

interface

uses
  SysUtils, Classes, ImgList, Forms, ImageListEx, FormTools;

{ TResourceModule }

type
  TResourceModule = class(TDataModule)
    GlassImages: TGlassImageList;
    ControlImages: TGlassImageList;
  end;

{ TImageLink }

	TImageListLink = class(TCustomImageList)
  private
    FImageLink: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FResource: string;
    procedure CheckResource;
    procedure ImageListChange(Sender: TObject);
    procedure SetResource(const Value: string);
	public
		constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  	property Resource: string read FResource write SetResource;
  end;

var
	ResourceModule: TResourceModule;

procedure CreateResourceModule;

implementation

{$R *.dfm}

{ TImageLink }

constructor TImageListLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ShareImages := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TImageListLink.Destroy;
begin
	if FImageLink <> nil then
		FImageLink.UnRegisterChanges(FImageChangeLink);
	FImageChangeLink.Free;	
  inherited Destroy;
end;

procedure TImageListLink.CheckResource;
begin
	//if ResourceModule = nil then
  	//Application.CreateForm(TResourceModule, _ResourceModule);
end;

procedure TImageListLink.ImageListChange(Sender: TObject);
begin
	Handle := (Sender as TCustomImageList).Handle;
end;

procedure TImageListLink.SetResource(const Value: string);
var
	C: TComponent;
begin
	CheckResource;
	if Value <> FResource then
  begin
  	if FImageLink <> nil then
    	FImageLink.UnRegisterChanges(FImageChangeLink);
  	FResource := Value;
    C := ResourceModule.FindComponent(FResource);
    if C is TCustomImageList then
		begin
    	FImageLink := TCustomImageList(C);
    	FImageLink.RegisterChanges(FImageChangeLink);
      Handle := FImageLink.Handle;
    end
    else
    begin
	    FImageLink := nil;
      Handle := 0;
		end;
  end;
end;

procedure CreateResourceModule;
begin
	if ResourceModule = nil then
  	Application.CreateForm(TResourceModule, ResourceModule);
end;

end.
