unit PhotoCtrls;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, ImgList, Graphics, BaseTypes,
  GraphTools,
  ScrollCtrls, Dialogs, ComCtrls, CommCtrl, Forms, ShlTools, ShellAPI, ExtCtrls,
  Jpeg;

type
  TPhotoItem = class(TCollectionItem)
  private
    FAttributes: TStrings;
  	FFileName: string;
  	FChecked: Boolean;
    FSmallImage: TBitmap;
    FLargeImage: TBitmap;
    FWidth: Integer;
    FHeight: Integer;
    procedure SetAttributes(Value: TStrings);
    procedure SetSmallImage(Value: TBitmap);
    function GetChecked: Boolean;
    procedure SetChecked(Value: Boolean);
  public
    constructor Create(Collection: TCollection); override;
  	destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Attributes: TStrings read FAttributes write SetAttributes;
  	property FileName: string read FFileName write FFileName;
  	property Checked: Boolean read GetChecked write SetChecked;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property SmallImage: TBitmap read FSmallImage write SetSmallImage;
    property LargeImage: TBitmap read FLargeImage;
  end;

{ TPhotoItems }

  TPhotoItems = class(TOwnedCollection)
  private
    function Get(Index: Integer): TPhotoItem;
    procedure Put(Index: Integer; Value: TPhotoItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    function Add: TPhotoItem;
    function FindItemID(ID: Integer): TPhotoItem;
    function FindItemFile(const FileName: string): TPhotoItem;
    function Insert(Index: Integer): TPhotoItem;
    property Items[Index: Integer]: TPhotoItem read Get write Put; default;
  end;

{ TPhotoList }

	TPhotoList = class(TScrollList)
  private
  	FBrush: TBitmap;
  	FPhotos: TPhotoItems;
    FThreads: TList;
    FCheckIndex: Integer;
    FCheckDownIndex: Integer;
    procedure PhotoResize(Sender: TObject; const FileName: string;
			Photo: TBitmap; Width, Height: Integer);
    procedure SetPhotos(Value: TPhotoItems);
  protected
    function FindCheckIndex(X, Y: Integer): Integer;
    procedure DrawItem(Index: Integer; var Rect: TRect; State: TDrawState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  	procedure UpdateItems;
	public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
		function AddPhoto(const FileName: string): TPhotoItem;
    procedure AddPhotos(Files: TStrings);
  	property Photos: TPhotoItems read FPhotos write SetPhotos;
    property ItemIndex;
    property TopIndex;
	published
		property Anchors;
    property Align;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
  end;

{ TPhotoView }

type
  TLVBKImage = packed record
    ulFlags: ULONG;
    hbm: HBITMAP;
    pszImage: PAnsiChar;
    cchImageMax: UINT;
    xOffsetPercent: Integer;
    yOffsetPercent: Integer;
  end;
  PLVBKImage = ^TLVBKImage;

const
	WM_CHECK = WM_USER + 20;

  LVM_SETBKIMAGE          = LVM_FIRST + 68;
  LVM_GETBKIMAGE          = LVM_FIRST + 69;
  LVBKIF_SOURCE_NONE      = $00000000;
  LVBKIF_SOURCE_HBITMAP   = $00000001;
  LVBKIF_SOURCE_URL       = $00000002;
  LVBKIF_SOURCE_MASK      = $00000003;
  LVBKIF_STYLE_NORMAL     = $00000000;
  LVBKIF_STYLE_TILE       = $00000010;
  LVBKIF_STYLE_MASK       = $00000010;
  LVBKIF_FLAG_TILEOFFSET  = $00000100;
  LVBKIF_TYPE_WATERMARK   = $10000000;
  LVBKIF_FLAG_ALPHABLEND  = $20000000;

  LVS_EX_DOUBLEBUFFER     = $00010000;
  LVS_EX_HIDELABELS       = $00020000;
  LVS_EX_SINGLEROW        = $00040000;
  LVS_EX_SNAPTOGRID       = $00080000;
  LVS_EX_SIMPLESELECT     = $00100000;

type
  TPhotoView = class(TCustomListView)
  private
	  FPhoto: TAlphaImage;
  	FPhotos: TPhotoItems;
    FThreads: TList;
    FImageList: TImageList;
    FPreviewForm: TCustomForm;
    FPreviewItem: TListItem;
    FPreviewTimer: TTimer;
		procedure PhotoResize(Sender: TObject; const FileName: string;
			Photo: TBitmap; Width, Height: Integer);
    procedure PreviewTimer(Sender: TObject);      
    procedure StartPreview(Item: TListItem);
    procedure ShowPreview;
    procedure HidePreview;
    procedure SetPhotos(Value: TPhotoItems);
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMCheck(var Msg: TMessage); message WM_CHECK;
  protected
    procedure CreateWnd; override;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawItem(Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; override;
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WndProc(var Msg: TMessage); override;
		procedure UpdateItems;
	public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
		function AddPhoto(const FileName: string): TPhotoItem;
    procedure AddPhotos(Files: TStrings);
    procedure Toggle;
    procedure Preview;
    procedure Recycle(Checked: Boolean = False);
  	property Photos: TPhotoItems read FPhotos write SetPhotos;
	published
		property Anchors;
    property Align;
    property Color;
    property Enabled;
    property Visible;
    property OnClick;
    property OnDblClick;
  end;

{ TPhotoResizeThread }

  TPhotoSizes = array of Integer;
  TPhotoResizeEvent = procedure(Sender: TObject; const FileName: string;
  	Photo: TBitmap; Width, Height: Integer) of object;

	TPhotoResizeThread = class(TThread)
  private
  	FFiles: TStrings;
    FSizes: TPhotoSizes;
    FFileName: string;
    FPhoto: TBitmap;
    FMode: TResizeMode;
    FWidth: Integer;
    FHeight: Integer;
    FOnResize: TPhotoResizeEvent;
    procedure DoResize;
  protected
  	procedure Execute; override;
	public
		constructor Create(Files: TStrings; const Sizes: TPhotoSizes);
    property Mode: TResizeMode read FMode write FMode;
    property OnResize: TPhotoResizeEvent read FOnResize write FOnResize;
  end;

function Blank: TAlphaImage;

implementation

{$R photoctrls.res}

const
	DefSmallHeight = 72;
  DefSmallWidth = 120;
  DefViewSmallHeight = 110;
  DefViewLargeHeight = 320;
  CheckOffset = 32;

constructor TPhotoItem.Create(Collection: TCollection);
begin
	FAttributes := TStringList.Create;
	FSmallImage := TBitmap.Create;
	FLargeImage := TBitmap.Create;
	inherited Create(Collection);
end;

destructor TPhotoItem.Destroy;
var
	PhotoView: TPhotoView;
  I: Integer;
begin
  if (Collection is TPhotoItems) and (TPhotoItems(Collection).Owner is TPhotoView) then
  begin
  	I := Index;
    PhotoView := TPhotoView(TPhotoItems(Collection).Owner);
    if I < PhotoView.Items.Count then
    	PhotoView.Items[I].Free;
  end;
	FAttributes.Free;
	FSmallImage.Free;
	FLargeImage.Free;
	inherited Destroy;
end;

function TPhotoItem.GetChecked: Boolean;
var
	PhotoView: TPhotoView;
	I: Integer;
begin
	Result := FChecked;
  if (Collection is TPhotoItems) and (TPhotoItems(Collection).Owner is TPhotoView) then
  begin
  	I := Index;
    PhotoView := TPhotoView(TPhotoItems(Collection).Owner);
    if I < PhotoView.Items.Count then
    	Result := PhotoView.Items[I].Checked;
  end;
end;

procedure TPhotoItem.SetChecked(Value: Boolean);
var
	PhotoView: TPhotoView;
	I: Integer;
begin
	FChecked := Value;
  if (Collection is TPhotoItems) and (TPhotoItems(Collection).Owner is TPhotoView) then
  begin
  	I := Index;
    PhotoView := TPhotoView(TPhotoItems(Collection).Owner);
    if I < PhotoView.Items.Count then
    	PhotoView.Items[I].Checked := FChecked;
  end;
end;

procedure TPhotoItem.Assign(Source: TPersistent);
var
	Item:	TPhotoItem absolute Source;
begin
	if Source is TPhotoItem then
  begin
		FAttributes.Assign(Item.FAttributes);
		FSmallImage.Assign(Item.FSmallImage);
		FLargeImage.Assign(Item.FLargeImage);
  	FFileName := Item.FFileName;
    FWidth  := Item.Width;
    FHeight := Item.Height;
  end
  else inherited Assign(Source);
end;

procedure TPhotoItem.SetAttributes(Value: TStrings);
begin
	FAttributes.Assign(Value);
end;

procedure TPhotoItem.SetSmallImage(Value: TBitmap);
begin
	FSmallImage.Assign(Value);
end;

{ TPhotoItems }

constructor TPhotoItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TPhotoItem);
end;


procedure TPhotoItems.BeginUpdate;
begin
	inherited BeginUpdate;
  if (Owner is TPhotoView) and ([csDestroying] * TPhotoView(Owner).ComponentState = []) then
  	TPhotoView(Owner).Items.BeginUpdate;
end;

procedure TPhotoItems.EndUpdate;
begin
	inherited BeginUpdate;
  if (Owner is TPhotoView) and ([csDestroying] * TPhotoView(Owner).ComponentState = []) then
  	TPhotoView(Owner).Items.EndUpdate;
end;

function TPhotoItems.Add: TPhotoItem;
begin
  Result := TPhotoItem(inherited Add);
  if Owner is TPhotoView then
  	TPhotoView(Owner).Items.Add.Checked := True;
end;

function TPhotoItems.FindItemID(ID: Integer): TPhotoItem;
begin
  Result := TPhotoItem(inherited FindItemID(ID));
end;

function TPhotoItems.FindItemFile(const FileName: string): TPhotoItem;
var
	S: string;
  I: Integer;
begin
	Result := nil;
	S := UpperCase(FileName);
  for I := 0 to Count - 1 do
  	if UpperCase(Get(I).FileName) = S then
    begin
    	Result := Get(I);
      Break;
    end;
end;

function TPhotoItems.Insert(Index: Integer): TPhotoItem;
begin
  Result := TPhotoItem(GetItem(Index));
end;

procedure TPhotoItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if GetOwner is TPhotoList then
  	TPhotoList(GetOwner).UpdateItems
  else if GetOwner is TPhotoView then
  	TPhotoView(GetOwner).UpdateItems;
end;

function TPhotoItems.Get(Index: Integer): TPhotoItem;
begin
  Result := TPhotoItem(GetItem(Index));
end;

procedure TPhotoItems.Put(Index: Integer; Value: TPhotoItem);
begin
  SetItem(Index, Value);
end;

{ TPhotoList }

constructor TPhotoList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True;
  ItemHeight := DefSmallHeight + 16;
	FThreads := TList.Create;
	FPhotos := TPhotoItems.Create(Self);
end;

destructor TPhotoList.Destroy;
var
	Thread: TThread;
	I: Integer;
begin
	for I := 0 to FThreads.Count - 1 do
  begin
  	Thread := TThread(FThreads[I]);
  	Thread.Terminate;
    Thread.WaitFor;
    Thread.Free;
	end;
	FPhotos.Free;
  FBrush.Free;
  inherited Destroy;
end;

procedure TPhotoList.PhotoResize(Sender: TObject; const FileName: string;
	Photo: TBitmap; Width, Height: Integer);
var
	Item: TPhotoItem;
begin
	Item := FPhotos.FindItemFile(FileName);
  if Item = nil then
  	Item := FPhotos.Add;
	Item.FileName := FileName;
  Item.SmallImage.Assign(Photo);
  // Item.Checked := True;
  Invalidate;
end;

function TPhotoList.AddPhoto(const FileName: string): TPhotoItem;
var
  Files: TStrings;
	Sizes: TPhotoSizes;
	Thread: TPhotoResizeThread;
begin
	Result := FPhotos.FindItemFile(FileName);
	if Result = nil then
  begin
		Result := FPhotos.Add;
		Result.FileName := FileName;
    Files := TStringList.Create;
    try
    	Files.Add(FileName);
			SetLength(Sizes, 1);
			Sizes[0] := DefSmallHeight;
			Thread := TPhotoResizeThread.Create(Files, Sizes);
			Thread.OnResize := PhotoResize;
			FThreads.Add(Thread);
			Thread.Resume;
    finally
    	Files.Free;
    end;
	end;
end;

procedure TPhotoList.AddPhotos(Files: TStrings);
var
	Item: TPhotoItem;
	Sizes: TPhotoSizes;
	Thread: TPhotoResizeThread;
  S: string;
  I: Integer;
begin
	FPhotos.BeginUpdate;
  try
		for I := 0 to Files.Count - 1 do
    begin
    	S := Files[I];
			Item := FPhotos.FindItemFile(S);
      if Item = nil then
		  	Item := FPhotos.Add;
			Item.FileName := S;
    end;
  finally
  	FPhotos.EndUpdate;
  end;
  SetLength(Sizes, 1);
  Sizes[0] := DefSmallHeight;
  Thread := TPhotoResizeThread.Create(Files, Sizes);
  Thread.OnResize := PhotoResize;
  FThreads.Add(Thread);
  Thread.Resume;
end;

function TPhotoList.FindCheckIndex(X, Y: Integer): Integer;
const
	CheckSize = 10;
  CheckLeft = (CheckOffset - CheckSize) div 2 - CheckSize;
  CheckRight = CheckLeft + CheckSize * 2;
var
	CheckTop, CheckBottom: Integer;
begin
	Result := -1;
  if (X > CheckLeft) and (X < CheckRight) then
  begin
		CheckTop := Y div ItemHeight * ItemHeight + ItemHeight div 2 - CheckSize;
    CheckBottom := CheckTop  + CheckSize * 2;
    if PtInRect(Rect(CheckLeft, CheckTop, CheckRight, CheckBottom), Point(X, Y)) then
    	Result := TopIndex + Y div ItemHeight;
		if Result > Count - 1 then
    	Result := -1;
  end;
end;

procedure TPhotoList.KeyDown(var Key: Word; Shift: TShiftState);
begin
	inherited KeyDown(Key, Shift);
	if ItemIndex > -1 then
  	case Key of
    	VK_RETURN,
      VK_SPACE:
      	begin
	      	FPhotos[ItemIndex].Checked := not FPhotos[ItemIndex].Checked;
          Invalidate;
				end;
			VK_DELETE:
      	if MessageDlg('Delete this photo from your computer?',
        	mtConfirmation, [mbYes, mbNo], 0) = mrYes then
				begin
					DeleteFile(FPhotos[ItemIndex].FileName);
					FPhotos[ItemIndex].Free;
        end
      	else if FPhotos[ItemIndex].Checked then
      	begin
        	FPhotos[ItemIndex].Checked := False;
          Invalidate;
        end;
		end;
end;

procedure TPhotoList.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
  	FCheckDownIndex := FindCheckIndex(X, Y);
    if FCheckDownIndex > -1 then
    	Invalidate;
  end;
end;

procedure TPhotoList.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    if FCheckDownIndex > FPhotos.Count then
    	FCheckDownIndex := -1;
		if FCheckDownIndex > -1 then
    begin
    	if FCheckDownIndex = FindCheckIndex(X, Y) then
				FPhotos[FCheckDownIndex].Checked := not FPhotos[FCheckDownIndex].Checked;
			FCheckDownIndex := -1;
      Invalidate;
		end;
  end;
end;

procedure TPhotoList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	I: Integer;
begin
	inherited MouseMove(Shift, X, Y);
	I := FindCheckIndex(X, Y);
  if I <> FCheckIndex then
  begin
  	FCheckIndex := I;
    Invalidate;
  end;
end;

procedure TPhotoList.UpdateItems;
begin
	Count := FPhotos.Count;
end;

procedure TPhotoList.SetPhotos(Value: TPhotoItems);
begin
	FPhotos.Assign(Value);
end;

procedure TPhotoList.DrawItem(Index: Integer; var Rect: TRect;
  State: TDrawState);
const
	TextBorder = 8;
var
	DC: HDC;
  B: TGraphic;
  R: TRect;
  CheckState: TDrawState;
begin
  if FBrush = nil then
  begin
  	FBrush := TBitmap.Create;
    FBrush.Width := 2;
    FBrush.Height := 1;
    FBrush.Canvas.Pixels[0, 0] := clBtnShadow;
    FBrush.Canvas.Pixels[1, 0] := clBtnHighlight;
  end;
  Canvas.Font := Font;
	DC := Canvas.Handle;
  if dsFocused in State then
		DrawGradient(DC, Rect, Blend(clWindow, clHighlight, 75), Blend(clWindow, clHighlight), drDown)
  else if dsSelected in State then
		FillRectColor(DC, Rect, Blend(clWindow, clHighlight))
  else if dsHot in State then
		FillRectColor(DC, Rect, Blend(clWindow, clHighlight, 75))
	else
		FillRectColor(DC, Rect, clWindow);
	B := FPhotos[Index].SmallImage;
  if B.Empty then
  	B := Blank;
  R.Left := CheckOffset;
  R.Top := Rect.Top + (HeightOf(Rect) - DefSmallHeight) shr 1;
  R.Right := R.Left + B.Width;
  R.Bottom := R.Top + DefSmallHeight;
	Canvas.Draw(R.Left, R.Top, B);
	InflateRect(R, 1, 1);
	FillRectOutline(DC, R, clHighlight);
  R := Rect;
  R.Right := CheckOffset;
  CheckState := [];
  if Index = FCheckIndex then
  begin
  	Include(CheckState, dsHot);
		if Index = FCheckDownIndex then
  		Include(CheckState, dsPressed);
	end;
	if FPhotos[Index].Checked then
  	Include(CheckState, dsChecked);
  DrawThemeCheckBox(DC, R, CheckState);
  R := Rect;
  if B.Width < DefSmallWidth + 2 then
	  R.Left := DefSmallWidth + TextBorder * 2
	else
  	R.Left := B.Width + TextBorder * 2;
	Dec(R.Right, TextBorder);
  Inc(R.Top, TextBorder);
  {R.Bottom := R.Top + FontHeight(DC);}
  DrawCaption(DC, ExtractFileName(FPhotos[Index].FileName), R, drLeft);
  {Slide(R, drDown, TextBorder);
  DrawCaption(DC, 'Photographer:', R, drLeft);
  Slide(R, drDown);
  DrawCaption(DC, 'Camera Model:', R, drLeft);
  Slide(R, drDown);
  DrawCaption(DC, 'Date Taken:', R, drLeft);
  Slide(R, drDown);
  DrawCaption(DC, 'Size:', R, drLeft);}
  R := Rect;
  R.Top := R.Bottom - 1;
  if FBrush = nil then
  begin
  	FBrush := TBitmap.Create;
    FBrush.Width := 2;
    FBrush.Height := 1;
    FBrush.Canvas.Pixels[0, 0] := clBtnShadow;
    FBrush.Canvas.Pixels[1, 0] := clBtnHighlight;
  end;
	Canvas.Brush.Bitmap := FBrush;
	Canvas.FillRect(R);
end;

(*function AdjustByte(B: Extended): Byte;
var
  I: Integer;
begin
  I := Round(B);
  if I < $00 then
    Result := $00
  else if I > $FF then
    Result := $FF
  else
    Result := I;        
end;

function BmpClone(B: TBitmap): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Assign(B);
end;

type
TBmpData24 = class(TObject)
private
  sl: array of Pointer;
  FWidth, FHeight: integer;
  function GetData(x, y: integer):PRGBTriple;
protected
public
  constructor Create(bmp: TBitmap);
  destructor Destroy; override;
  property Data[x, y: integer]: PRGBTriple read GetData; default;
  property Width: integer read FWidth;
  property Height: integer read FHeight;
end;

constructor TBmpData24.Create(bmp: TBitmap);
var
  i: integer;
begin
  SetLength(sl, bmp.Height);
  for i := 0 to bmp.Height-1 do sl[i] := bmp.ScanLine[i];
  FWidth := bmp.Width;
  FHeight := bmp.Height;
end;

destructor TBmpData24.Destroy;
begin

  inherited;
end;

function TBmpData24.GetData(x, y: integer):PRGBTriple;
begin
  result := sl[y];
  Inc(result, x);
end;

function BicubicFilter(src: TBmpData24; sx, sy: extended): TRGBTriple;
var
  x, y, fx, fy, d:integer;
  r, g, b: extended;
  dx, dy, wx, wy: extended;
begin

  // offset for x0, x1, y0, y1 because of antialias, currently 3 for bicubic
  d := 3;

  x := trunc(sx);
  y := trunc(sy);

  r := 0; g := 0; b := 0;
  for fy := y-1 to y+2 do
    for fx := x-1 to x+2 do
    begin
      dx := abs(fx-sx);
      if dx<1 then
        wx := (dx-1)*(dx*dx-dx-1)
      else
        wx := -(dx-1)*(dx-2)*(dx-2);

      dy := abs(fy-sy);
      if dy<1 then
        wy := (dy-1)*(dy*dy-dy-1)
      else
        wy := -(dy-1)*(dy-2)*(dy-2);

      r := r + src[fx+d,fy+d]^.rgbtRed*wx*wy;
      g := g + src[fx+d,fy+d]^.rgbtGreen*wx*wy;
      b := b + src[fx+d,fy+d]^.rgbtBlue*wx*wy;
    end;

  result.rgbtRed := AdjustByte(r);
  result.rgbtGreen := AdjustByte(g);
  result.rgbtBlue := AdjustByte(b);

end;

function BmpResize(var bmp: TBitmap;
           rwidth, rheight: integer;
                      mode: TResizeMode = rmBilinear):Boolean;
var
  tmp: TBitmap;
  src, dst: TBmpData24;
  wfactor, hfactor, coefx00, coefx01, coefy00, coefy01: extended;
  ix, iy, x, y, fx, fy, w, h, x0, x1, y0, y1: integer;
  xx, yy, r, g, b, r1, r2, g1, g2, b1, b2, wx, wy: extended;

  dxx, dyy, dx, dy: extended;
  dxn: array[1..4] of extended;
  dyn: array[0..3] of extended;
  RX, RY: array[-1..2] of extended;
begin
  result := false;
  if bmp.PixelFormat <> pf24bit then exit;
  w := bmp.Width;
  h := bmp.Height;

  wfactor := w/rwidth;
  hfactor := h/rheight;

  tmp := BmpClone(bmp);
  tmp.Width := rwidth;
  tmp.Height := rheight;

  src := TBmpData24.Create(bmp);
  dst := TBmpData24.Create(tmp);

  for iy := 0 to rheight-1 do
    for ix := 0 to rwidth-1 do
      case mode of
        rmNearest: dst[ix,iy]^ := src[round(wfactor*ix), round(hfactor*iy)]^;
        rmBilinear:
          begin
            xx := wfactor*ix;
            yy := hfactor*iy;

            x0 := trunc(xx); if x0<0 then x0 := 0;
            x1 := x0+1; if x1>w-1 then x1 := x0;
            y0 := trunc(yy); if y0<0 then y0 := 0;
            y1 := y0+1; if y1>h-1 then y1 := y0;

            coefx01 := xx-x0; coefx00 := 1-coefx01;
            coefy01 := yy-y0; coefy00 := 1-coefy01;

            r1 := src[x0, y0]^.rgbtRed*coefx00 + src[x1,y0]^.rgbtRed*coefx01;
            r2 := src[x0, y1]^.rgbtRed*coefx00 + src[x1,y1]^.rgbtRed*coefx01;
            dst[ix,iy]^.rgbtRed := AdjustByte(r1*coefy00 + r2*coefy01);

            g1 := src[x0, y0]^.rgbtGreen*coefx00 + src[x1,y0]^.rgbtGreen*coefx01;
            g2 := src[x0, y1]^.rgbtGreen*coefx00 + src[x1,y1]^.rgbtGreen*coefx01;
            dst[ix,iy]^.rgbtGreen := AdjustByte(g1*coefy00 + g2*coefy01);

            b1 := src[x0, y0]^.rgbtBlue*coefx00 + src[x1,y0]^.rgbtBlue*coefx01;
            b2 := src[x0, y1]^.rgbtBlue*coefx00 + src[x1,y1]^.rgbtBlue*coefx01;
            dst[ix,iy]^.rgbtBlue := AdjustByte(b1*coefy00 + b2*coefy01);
          end;
        rmBicubic:
          begin
            xx := wfactor*ix; x := trunc(xx);
            yy := hfactor*iy; y := trunc(yy);

            r := 0; g := 0; b := 0;
            for fy := y-1 to y+2 do
              for fx := x-1 to x+2 do
              begin
                dx := Abs(xx-fx);
                dy := Abs(yy-fy);

                if dx<1 then
                  wx := (dx-1)*(dx*dx-dx-1)
                else
                  wx := -(dx-1)*(dx-2)*(dx-2);

                if dy<1 then
                  wy := (dy-1)*(dy*dy-dy-1)
                else
                  wy := -(dy-1)*(dy-2)*(dy-2);

                x0 := fx; if (x0<0) or (x0>w-1) then x0 := x;
                y0 := fy; if (y0<0) or (y0>h-1) then y0 := y;

                r := r + src[x0,y0]^.rgbtRed*wx*wy;
                g := g + src[x0,y0]^.rgbtGreen*wx*wy;
                b := b + src[x0,y0]^.rgbtBlue*wx*wy;
              end;

                dst[ix,iy]^.rgbtRed := AdjustByte(r);
                dst[ix,iy]^.rgbtGreen := AdjustByte(g);
                dst[ix,iy]^.rgbtBlue := AdjustByte(b);
          end;
        rmBicubic:
          begin
            xx := wfactor*ix; x := trunc(xx);
            yy := hfactor*iy; y := trunc(yy);

            dxx := xx-x;
            dyy := yy-y;

            dxn[1] := 1-dxx; dxn[1] := dxn[1]*dxn[1]*dxn[1]/6;
            dxn[2] := 2-dxx; dxn[2] := dxn[2]*dxn[2]*dxn[2]/6;
            dxn[3] := 3-dxx; dxn[3] := dxn[3]*dxn[3]*dxn[3]/6;
            dxn[4] := 4-dxx; dxn[4] := dxn[4]*dxn[4]*dxn[4]/6;

            dyn[0] := 0+dyy; dyn[0] := dyn[0]*dyn[0]*dyn[0]/6;
            dyn[1] := 1+dyy; dyn[1] := dyn[1]*dyn[1]*dyn[1]/6;
            dyn[2] := 2+dyy; dyn[2] := dyn[2]*dyn[2]*dyn[2]/6;
            dyn[3] := 3+dyy; dyn[3] := dyn[3]*dyn[3]*dyn[3]/6;

            RX[-1] := dxn[1];
            RX[0]  := dxn[2]-4*dxn[1];
            RX[1]  := dxn[3]-4*dxn[2]+6*dxn[1];
            RX[2]  := dxn[4]-4*dxn[3]+6*dxn[2]-4*dxn[1];

            RY[-1] := dyn[3]-4*dyn[2]+6*dyn[1]-4*dyn[0];
            RY[0]  := dyn[2]-4*dyn[1]+6*dyn[0];
            RY[1]  := dyn[1]-4*dyn[0];
            RY[2]  := dyn[0];

            r := 0; g := 0; b := 0;
            for fy := y-1 to y+2 do
              for fx := x-1 to x+2 do
              begin
                x0 := fx; if (x0<0) or (x0>w-1) then x0 := x;
                y0 := fy; if (y0<0) or (y0>h-1) then y0 := y;

                r := r + src[x0,y0]^.rgbtRed*RX[fx-x]*RY[fy-y];
                g := g + src[x0,y0]^.rgbtGreen*RX[fx-x]*RY[fy-y];
                b := b + src[x0,y0]^.rgbtBlue*RX[fx-x]*RY[fy-y];
              end;

            dst[ix,iy]^.rgbtRed := AdjustByte(r);
            dst[ix,iy]^.rgbtGreen := AdjustByte(g);
            dst[ix,iy]^.rgbtBlue := AdjustByte(b);
          end;
      end;

  src.Free;
  dst.Free;
  bmp.Free;
  bmp := tmp;
  result := true;
end;*)

{ TPhotoResizeThread }

constructor TPhotoResizeThread.Create(Files: TStrings; const Sizes: TPhotoSizes);
begin
	inherited Create(True);
  FFiles := TStringList.Create;
	FFiles.Assign(Files);
  FMode := rmNearest;
  FSizes := Sizes;
end;

procedure TPhotoResizeThread.DoResize;
begin
	if Assigned(FOnResize) then
  	FOnResize(Self, FFileName, FPhoto, FWidth, FHeight);
end;

procedure TPhotoResizeThread.Execute;
var
	Picture: TPicture;
  Scale, Aspect: Single;
	I, J: Integer;
begin
	if not Assigned(FOnResize) then Exit;
	Picture := TPicture.Create;
	FPhoto := TBitmap.Create;
  try
		for I := 0 to FFiles.Count - 1 do
    begin
			FFileName := FFiles[I];
			if FileExists(FFileName) then
			try
				if Terminated then Exit;
				Picture.LoadFromFile(FFileName);
				if (Picture.Graphic.Height < 1) or (Picture.Graphic.Width < 1) then
        	Continue;
		  	for J := Low(FSizes) to High(FSizes) do
				begin
					if Terminated then Exit;
          if FSizes[J] < 1 then Continue;
          FWidth := Picture.Graphic.Width;
          FHeight := Picture.Graphic.Height;
          Scale := FSizes[J] / Picture.Graphic.Height;
          Aspect := Picture.Graphic.Height / Picture.Graphic.Width;
          if Aspect < 0.75 then
            Scale := Scale * (Aspect / 0.75); 
					FPhoto.Assign(Picture.Graphic);
					ResizeBitmap(FPhoto, Round(FPhoto.Width * Scale),
						Round(FPhoto.Height * Scale), FMode);
					Synchronize(DoResize);
				end;
      except
				{ if the image is invalid, then proceed quietly to the next image }
			end;
		end;
	finally
  	FPhoto.Free;
    Picture.Free;
  end;
end;

var
	InternalBlank: TAlphaImage;

function Blank: TAlphaImage;
begin
	if InternalBlank = nil then
	begin
		InternalBlank := TAlphaImage.Create;
		InternalBlank.LoadFromResourceID(601);
	end;
	Result := InternalBlank;
end;

{ TPhotoView }

type
  TPhotoPreviewForm = class(TForm)
  private
    FFrame: TAlphaImage;
    FBitmap: TFastBitmap;
    FFont: HFONT;
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure UpdateItem(PhotoView: TPhotoView; Item: TListItem);
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
  end;

constructor TPhotoPreviewForm.CreateNew(AOwner: TComponent;
  Dummy: Integer = 0);
begin
  inherited CreateNew(AOwner, Dummy);
  FFrame := TAlphaImage.Create;
  FFrame.LoadFromResourceID(605);
  FFont := GetFont('Tahoma', 8);
end;

destructor TPhotoPreviewForm.Destroy;
begin
  FFrame.Free;
  DestroyFastBitmap(FBitmap);
  DeleteObject(FFont);
  inherited Destroy;
end;

procedure UpdateAlphaWindow(Wnd: HWND; Bitmap: TFastBitmap; Opacity: Byte = $FF);
var
	Blend: TBlendFunction;
  Rect: TRect;
	P1, P2: TPoint;
  S: TSize;
  DC: HDC;
begin
	if Bitmap.Height = 0 then Exit;
	SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) or WS_EX_LAYERED);
	GetWindowRect(Wnd, Rect);
	P1.X := Rect.Left;
	P1.Y := Rect.Top;
	SetWindowPos(Wnd, 0, 0, 0, Bitmap.Width, Bitmap.Height,
		SWP_NOACTIVATE or SWP_NOMOVE);
	with Blend do
	begin
		BlendOp := AC_SRC_OVER;
		BlendFlags := 0;
		SourceConstantAlpha := Opacity;
		AlphaFormat := AC_SRC_ALPHA;
	end;
	DC := GetDC(0);
	P2 := Point(0, 0);
	S.cx := Bitmap.Width;
	S.cy := Bitmap.Height;
	UpdateLayeredWindow(Wnd, DC, @P1, @S, Bitmap.DC,
		@P2, 0, @Blend, ULW_ALPHA);
	ReleaseDC(0, DC);
end;


procedure Opaque(const Bitmap: TFastBitmap; const Rect: TRect);
var
	A: PRGBA;
  Row, Col: Integer;
begin
	if (Rect.Left < 0) or (Rect.Top < 0) or (Rect.Right > Bitmap.Width) or
  	(Rect.Bottom > Bitmap.Height) then	Exit;
	for Row := Rect.Top to Rect.Bottom - 1 do
	begin
		A := Bitmap.Bits;
    Inc(A, (Bitmap.Height - Row) * Bitmap.Width);
    Inc(A, Rect.Left);
    for Col := 0 to GraphTools.WidthOf(Rect) - 1 do
    begin
      A.Alpha := $FF;
    	Inc(A);
    end;
	end;
end;

procedure TPhotoPreviewForm.UpdateItem(PhotoView: TPhotoView; Item: TListItem);
var
  PhotoItem: TPhotoItem;
  Font: HFONT;
  B: TBitmap;
	F: TBlendFunction;
  R: TRect;
  P: TPoint;
begin
  if PhotoView = nil then
    Visible := False
  else
  begin
    PhotoItem := PhotoView.Photos[Item.Index];
    B := PhotoItem.LargeImage;
    if B.Width < 100 then
    begin
      Visible := False;
      Exit;
    end;
    Height := B.Height + 82;
    Width := B.Width + 42;
    DestroyFastBitmap(FBitmap);
    FBitmap := CreateFastBitmap(Width, Height, pd32);
    FillChar(F, SizeOf(F), #0);
    F.SourceConstantAlpha := $FF;
		F.AlphaFormat := AC_SRC_ALPHA;
    with FBitmap do
    begin
    	Windows.AlphaBlend(DC, 0, 0, 20, 20, FFrame.Bitmap.DC,
      	0, 0, 20, 20, F);
    	Windows.AlphaBlend(DC, 20, 0, Width - 40, 20, FFrame.Bitmap.DC,
      	20, 0, 60, 20, F);
    	Windows.AlphaBlend(DC, Width - 20, 0, 20, 20, FFrame.Bitmap.DC,
      	80, 0, 20, 20, F);
    	Windows.AlphaBlend(DC, 0, 20, 20, Height - 40, FFrame.Bitmap.DC,
      	0, 20, 20, 20, F);
    	Windows.AlphaBlend(DC, 20, 20, Width - 40, Height - 40, FFrame.Bitmap.DC,
      	20, 20, 60, 20, F);
    	Windows.AlphaBlend(DC, Width - 20, 20, 20, Height - 40, FFrame.Bitmap.DC,
      	80, 20, 20, 20, F);
    	Windows.AlphaBlend(DC, 0, Height - 20, 20, 20, FFrame.Bitmap.DC,
      	0, 80, 20, 20, F);
    	Windows.AlphaBlend(DC, 20, Height - 20, Width - 40, 20, FFrame.Bitmap.DC,
      	20, 80, 60, 20, F);
    	Windows.AlphaBlend(DC, Width - 20, Height - 20, 20, 20, FFrame.Bitmap.DC,
      	80, 80, 20, 20, F);
      BitBlt(DC, 21, 21, B.Width, B.Height, B.Canvas.Handle, 0, 0, SRCCOPY);
      R := Rect(20, 20, B.Width + 22, B.Height + 22);
      FillRectOutline(DC, R, clHighlight);
      R.Top := R.Bottom + 10;
      Font := SelectObject(DC, FFont);
      R.Bottom := R.Top + FontHeight(DC);
      DrawCaption(DC, ExtractFileName(PhotoItem.FileName), R, drCenter);
      Slide(R, drDown);
      DrawCaption(DC, Format('The actual size is %d X %d', [PhotoItem.Width, PhotoItem.Height]), R, drCenter);
      SelectObject(DC, Font);
      R := Rect(20, 20, Width - 20, Height - 15);
      Opaque(FBitmap, R);
    end;
    //
    R := Item.DisplayRect(drIcon);
    P.X := R.Left;
    P.Y := R.Top;
    P := PhotoView.ClientToScreen(P);
    Left := P.X + WidthOf(R) - 15;
    Top := P.Y + ((HeightOf(R) - Height) div 2);
    if Left + Width > Screen.Width then Left := Left - Width - WidthOf(R) + 15;
    if Top + Height > Screen.Height then Top := Screen.Height - Height
    else if Top < 0 then Top := 0;
    UpdateAlphaWindow(Handle, FBitmap);
    Visible := True;
  end;
end;

procedure TPhotoPreviewForm.CMShowingChanged(var Msg: TMessage);
begin
  if Showing then
    SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
      SWP_NOACTIVATE or SWP_SHOWWINDOW)
  else
    inherited;
end;

procedure TPhotoPreviewForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP;
  Params.ExStyle := 0;
  Params.Width := 200;
  Params.Width := 200;
end;

constructor TPhotoView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
	FPhoto := TAlphaImage.Create;
  FPhoto.LoadFromResourceID(602);
  FImageList := TImageList.Create(Self);
  FImageList.Height := 120;
  FImageList.Width := 128 + 32;
	FThreads := TList.Create;
	FPhotos := TPhotoItems.Create(Self);
  FPreviewTimer := TTimer.Create(Self);
  FPreviewTimer.Enabled := False;
  FPreviewTimer.Interval := 250;
  FPreviewTimer.OnTimer := PreviewTimer;
  FPreviewForm := TPhotoPreviewForm.CreateNew(Self);
  DoubleBuffered := True;
  CheckBoxes := True;
  MultiSelect := True;
  LargeImages := FImageList;
  ViewStyle := vsIcon;
  IconOptions.Arrangement := iaTop;
  IconOptions.AutoArrange := True;
end;

destructor TPhotoView.Destroy;
var
	Thread: TThread;
	I: Integer;
begin
	for I := 0 to FThreads.Count - 1 do
  begin
  	Thread := TThread(FThreads[I]);
  	Thread.Terminate;
    Thread.WaitFor;
    Thread.Free;
	end;
	FPhotos.Free;
  FPhoto.Free;
  inherited Destroy;
end;

procedure TPhotoView.CreateWnd;
var
	Bitmap: TBitmap;
  Lib: THandle;
	Image: TLVBKImage;
  A: PRGB;
  B, C: TRGBA;
  X, Y: Integer;
begin
	inherited CreateWnd;
  SendMessage(Handle, LVM_SETEXTENDEDLISTVIEWSTYLE, LVS_EX_DOUBLEBUFFER, LVS_EX_DOUBLEBUFFER);
  FillChar(Image, SizeOf(Image), #0);
  Bitmap := TBitmap.Create;
  if IsLibrary then
    Lib := HInstance
  else
    Lib := MainInstance;
  Bitmap.LoadFromResourceId(Lib, 603);
	B := ColorToRGBA(clActiveCaption);
	C := ColorToRGBA(clWindow);
  for Y := 0 to Bitmap.Height - 1 do
  begin
  	A := Bitmap.ScanLine[Y];
  	for X := 0 to Bitmap.Width - 1 do
    begin
			A.Red :=  Round(B.Red * (($FF - A.Red) / $FF) + C.Red * (A.Red / $FF));
			A.Green :=  Round(B.Green * (($FF - A.Green) / $FF) + C.Green * (A.Green / $FF));
			A.Blue :=  Round(B.Blue * (($FF - A.Blue) / $FF) + C.Blue * (A.Blue / $FF));
    	Inc(A);
    end;
	end;
	Image.ulFlags := LVBKIF_TYPE_WATERMARK;
	Image.hbm := Bitmap.Handle;
  Bitmap.ReleaseHandle;
  Bitmap.Free;
	Image.xOffsetPercent := 100;
	Image.yOffsetPercent := 100;
	SendMessage(Handle, LVM_SETBKIMAGE, 0, Integer(@Image));
end;

procedure TPhotoView.PhotoResize(Sender: TObject; const FileName: string;
	Photo: TBitmap; Width, Height: Integer);
var
	Item: TPhotoItem;
begin
	Item := FPhotos.FindItemFile(FileName);
  if Item = nil then
  	Item := FPhotos.Add;
  Item.FWidth := Width;
  Item.FHeight :=  Height;    
  if Photo.Height <= DefViewSmallHeight then
  begin
	  Item.FileName := FileName;
    Item.SmallImage.Assign(Photo);
    Invalidate;
    // Item.Checked := True;
  end
  else
    Item.LargeImage.Assign(Photo);
end;

function TPhotoView.AddPhoto(const FileName: string): TPhotoItem;
var
  Files: TStrings;
	Sizes: TPhotoSizes;
	Thread: TPhotoResizeThread;
begin
	Result := FPhotos.FindItemFile(FileName);
	if Result = nil then
  begin
		Result := FPhotos.Add;
		Result.FileName := FileName;
  end;
  Files := TStringList.Create;
  try
    Files.Add(FileName);
    SetLength(Sizes, 2);
    Sizes[0] := DefViewSmallHeight;
    Sizes[1] := DefViewLargeHeight;
    Thread := TPhotoResizeThread.Create(Files, Sizes);
    Thread.Mode := rmNearest;
    Thread.OnResize := PhotoResize;
    FThreads.Add(Thread);
    Thread.Resume;
  finally
    Files.Free;
  end;
end;

procedure TPhotoView.AddPhotos(Files: TStrings);
var
	Item: TPhotoItem;
	Sizes: TPhotoSizes;
	Thread: TPhotoResizeThread;
  S: string;
  I: Integer;
begin
	FPhotos.BeginUpdate;
  try
		for I := 0 to Files.Count - 1 do
    begin
    	S := Files[I];
			Item := FPhotos.FindItemFile(S);
      if Item = nil then
		  	Item := FPhotos.Add;
			Item.FileName := S;
    end;
  finally
  	FPhotos.EndUpdate;
  end;
  SetLength(Sizes, 2);
  Sizes[0] := DefViewSmallHeight;
  Sizes[1] := DefViewLargeHeight;
  Thread := TPhotoResizeThread.Create(Files, Sizes);
	Thread.Mode := rmNearest;
  Thread.OnResize := PhotoResize;
  FThreads.Add(Thread);
  Thread.Resume;
end;

function TPhotoView.IsCustomDrawn(Target: TCustomDrawTarget;
	Stage: TCustomDrawStage): Boolean;
begin
	Result := True;
end;

function TPhotoView.CustomDrawItem(Item: TListItem; State: TCustomDrawState;
	Stage: TCustomDrawStage): Boolean;
var
  DC: HDC;
  DrawRect: TRect;
	R: TRect;
	PhotoItem: TPhotoItem;
  G: TGraphic;
  S: string;
  Size: TSize;
  I: Integer;
begin
	Canvas.Font := Font;
	DC := Canvas.Handle;
  Dec(DrawRect.Right, 12);
	DrawRect := Item.DisplayRect(drIcon);
  DrawRect.Left := DrawRect.Right - (128 + 48);
	if ThemePainter.Enabled then
  begin
		Dec(DrawRect.Right, 12);
    OffsetRect(DrawRect, -5,0);
  end
  else
		Inc(DrawRect.Left, 16);
	R := DrawRect;
	//Dec(R.Bottom, Round(FontHeight(DC) * 1.5));
	if [cdsSelected, cdsFocused] * State <> [] then
	begin
		if not ThemePainter.Enabled then
			FillRectColor(DC, R, clBtnFace);
		if cdsFocused in State then
			DrawThemeButton(DC, R, [])
		else
			DrawThemeThinButton(DC, R, [dsHot]);
	end;
	{else
		FillRectColor(DC, R, clWindow);}
	PhotoItem := nil;
	I := Item.Index;
	if I < FPhotos.Count then
		PhotoItem := FPhotos[I];
	if (PhotoItem <> nil) and (not PhotoItem.SmallImage.Empty) then
		G := PhotoItem.SmallImage
	else
		G := FPhoto;
	R.Left := R.Left + (WidthOf(R) - G.Width) div 2;
	R.Top := R.Top + (HeightOf(R) - G.Height) div 2;
	Canvas.Draw(R.Left, R.Top, G);
	if G <> FPhoto then
	begin
		R.Right := R.Left + G.Width;
		R.Bottom := R.Top + G.Height;
		InflateRect(R, 1, 1);
		FillRectOutline(DC, R, clHighlight);
	end;
  if PhotoItem <> nil then
  begin
  	R := DrawRect;
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Round(FontHeight(DC) * 1.5);
		//R.Top := R.Bottom - Round(FontHeight(DC) * 1.5);
    InflateRect(R, -16, 0);
    S := ExtractFileName(PhotoItem.FileName);
    Size := CalcCaptionSize(DC, S);
    R.Top := R.Top + (HeightOf(R) - Size.cy) div 2;
    R.Bottom := R.Top + Size.cy + 1;
    if Size.cx < WidthOf(R) then
    	R.Left := R.Left + (WidthOf(R) - Size.cx) div 2;
		R.Right := R.Left + Size.cx;
    if Item.Selected then
    begin
    	InflateRect(R, 3, 0);
	    FillRectColor(DC, R, clHighlight);
    	InflateRect(R, -3, 0);
      SetTextColor(DC, ColorToRGB(clHighlightText));
    end
    else
      SetTextColor(DC, ColorToRGB(clWindowText));
    DrawCaption(DC, ExtractFileName(PhotoItem.FileName), R, drCenter);
		InflateRect(R, 3, 0);
		if Focused and Item.Focused then
    	DrawFocusRect(DC, R);
  end;
  Result := True;
end;

procedure TPhotoView.UpdateItems;
begin
 //
end;

procedure TPhotoView.SetPhotos(Value: TPhotoItems);
begin
  FPhotos.Assign(Value);
end;

procedure TPhotoView.Recycle(Checked: Boolean = False);

	function Check(Item: TListItem): Boolean;
  begin
  	if Checked then
    	Result := not Item.Checked
		else
    	Result := Item.Selected;
  end;

var
	Form: TCustomForm;
  Wnd: HWND;
  Strings: TStrings;
  S: string;
  I: Integer;
begin
	Form := GetParentForm(Self);
  if Form <> nil then
    Wnd := Form.Handle
  else
    Wnd := 0;
	Strings := TStringList.Create;
  try
		with Items do
			for I := 0 to Count - 1 do
				if Check(Item[I]) then
					Strings.Add(FPhotos[I].FileName);
		S := Strings.CommaText;
		S := StringReplace(S, Strings.QuoteChar, '', [rfReplaceAll]);
		if FileOperation(Wnd, S, '', foDelete, [ooAllowUndo, ooFileOnly, ooSimpleProgress]) then
    try
    	FPhotos.BeginUpdate;
			for I := Items.Count - 1 downto 0 do
				if Check(Items[I]) then
        	FPhotos[I].Free;
    finally
    	FPhotos.EndUpdate;
    end;
  finally
  	Strings.Free;
      EnableWindow(Form.Handle, True);
  end;
end;

procedure TPhotoView.Preview;
var
	Item: TListItem;
	PhotoItem: TPhotoItem;
  I: Integer;
  S: string;
begin
	Item := Selected;
  if Item = nil then Exit;
	I := Item.Index;
	if I < FPhotos.Count then
		PhotoItem := FPhotos[I]
	else
  	PhotoItem := nil;
	if PhotoItem <> nil then
  begin
  	S := 'shimgvw.dll,ImageView_Fullscreen ' + PhotoItem.FileName;
		ShellExecute(Handle, nil, 'rundll32.exe', PChar(S), nil, SW_SHOW);
	end;
end;

procedure TPhotoView.Toggle;
var
	Item: TListItem;
	I: Integer;
begin
	Item := Selected;
  if Item <> nil then
  	for I := 0 to Items.Count - 1 do
    	if (Item <> Items[I]) and (Items[I].Selected) then
				Items[I].Checked := Item.Checked;
end;

procedure TPhotoView.DblClick;
begin
	inherited DblClick;
  Preview;
end;

procedure TPhotoView.KeyDown(var Key: Word; Shift: TShiftState);
var
	Item: TListItem;
  I, J: Integer;
begin
	inherited KeyDown(Key, Shift);
	Item := Selected;
	if Item <> nil then
  	case Key of
    	VK_RETURN:
	      Preview;
    	VK_SPACE:
      	begin
        	Item.Selected := True;
        	J := 0;
          for I := 0 to Items.Count - 1 do
          	if Items[I].Selected then
            	Inc(J);
					if J  < 2 then Exit;
					Item.Checked := not Item.Checked;
        	PostMessage(Handle, WM_CHECK, 0, 0);
				end;
			VK_DELETE:
				Recycle;
		end;
end;

procedure TPhotoView.PreviewTimer(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  ShowPreview;
end;

procedure TPhotoView.StartPreview(Item: TListItem);
begin
  if Item <> FPreviewItem then
  begin
    FPreviewTimer.Enabled := False;
    FPreviewTimer.Enabled := True;
    FPreviewItem := Item;
    HidePreview;
  end
  else if FPreviewForm.Visible then
    FPreviewTimer.Enabled := False
  else
  begin
    FPreviewTimer.Enabled := False;
    FPreviewTimer.Enabled := True;
  end;
end;

procedure TPhotoView.ShowPreview;
begin
  if FPreviewItem <> nil then
    TPhotoPreviewForm(FPreviewForm).UpdateItem(Self, FPreviewItem)
  else
    HidePreview;
end;

procedure TPhotoView.HidePreview;
begin
  FPreviewTimer.Enabled := False;
  TPhotoPreviewForm(FPreviewForm).UpdateItem(nil, nil);
end;

procedure TPhotoView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
begin
  inherited MouseMove(Shift, X, Y);
  Item := GetItemAt(X, Y);
  if Item <> nil then
    StartPreview(Item)
  else
    HidePreview;
end;

procedure TPhotoView.WndProc(var Msg: TMessage);
begin
	if Msg.Msg = WM_ERASEBKGND then
  	DefaultHandler(Msg)
  else inherited WndProc(Msg);
end;

procedure TPhotoView.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  HidePreview;
end;

procedure TPhotoView.WMCheck(var Msg: TMessage);
begin
	Toggle;
end;

initialization
	InternalBlank := nil;
finalization
	InternalBlank.Free;
  InternalBlank := nil;
end.
