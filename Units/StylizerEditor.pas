unit StylizerEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DesignCtrls, Grip, SuplCtrls, StdCtrls, BtnCtrls, BtnEdit,
  ComCtrls, ExtDlgs, Buttons, DialogsEx, ShlCtrls, FileTools, ResourceData,
  ImgList, ImageListEx, BannerCtrls, ExtCtrls, ShellAPI, DesignConfig, XMLParser,
  AdvancedStyles, FormTools;

type
  TStylizerEditorForm = class(TGripForm)
    OKButton: TButton;
    CancelButton: TButton;
    HorizontalBar: THorizontalBar;
    OpenPictureDialog: TOpenPictureDialog;
    Banner: TBanner;
    BannerImages: TGlassImageList;
    DesignerFrame: TFramedWindow;
    TabsFrame: TFramedWindow;
    Pages: TBannerBook;
    Label12: TLabel;
    BackgroundColorButton: TColorGridButton;
    BackgroundBrowseButton: TButton;
    BackgroundView: TShellView;
    BackgroundEditButton: TImageButton;
    BackgroundClearButton: TImageButton;
    BackgroundBox: TComboBox;
    BackgroundItemLabel: TLabel;
    GroupBox3: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    FontItemBox: TComboBox;
    FontNameBox: TComboBox;
    FontSizeEdit: TEdit;
    FontSizeSpinner: TUpDown;
    FontColorButton: TColorGridButton;
    FontBoldButton: TImageButton;
    FontItalicButton: TImageButton;
    FontUnderlineButton: TImageButton;
    GroupBox4: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    PlacementBox: TComboBox;
    StyleBox: TComboBox;
    GridSizeEdit: TEdit;
    GridSizeSpinner: TUpDown;
    ListBox1: TListBox;
    AdvancedButton: TImageButton;
    procedure FormCreate(Sender: TObject);
    procedure BackgroundColorButtonChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BackgroundViewDefaultAction(Sender: TObject; Node: TShellNode;
      var AllowAction: Boolean);
    procedure BackgroundViewIncludeItem(Sender: TObject; Node: TShellNode;
      var AllowAction: Boolean);
    procedure BackgroundClearButtonClick(Sender: TObject);
    procedure BackgroundEditButtonClick(Sender: TObject);
    procedure BannerSelectItem(Sender: TObject);
    procedure BackgroundBoxChange(Sender: TObject);
    procedure FontItemBoxChange(Sender: TObject);
    procedure FontNameBoxChange(Sender: TObject);
    procedure FontSizeSpinnerChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure FontBoldButtonClick(Sender: TObject);
    procedure FontItalicButtonClick(Sender: TObject);
    procedure FontUnderlineButtonClick(Sender: TObject);
    procedure FontColorButtonChange(Sender: TObject);
    procedure FontSizeEditExit(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure StyleBoxChange(Sender: TObject);
    procedure PlacementBoxChange(Sender: TObject);
    procedure GridSizeSpinnerChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure GridSizeEditExit(Sender: TObject);
    procedure AdvancedButtonClick(Sender: TObject);
  private
    FStylizer: TDesignStylizer;
    FDesigner: TDesignSurface;
    FActiveFont: TFont;
    procedure SetStylizer(Value: TDesignStylizer);
    procedure InsertControl(Sender: TObject; X, Y: Integer;
      var Inserted: Boolean);
	public
  	property Stylizer: TDesignStylizer read FStylizer write SetStylizer;
  end;

function EditStylizer(Stylizer: TDesignStylizer): Boolean;

implementation

{$R *.dfm}

function EditStylizer(Stylizer: TDesignStylizer): Boolean;
var
	Form: TStylizerEditorForm;
begin
	Form := TStylizerEditorForm.Create(Application);
	try
   //	Form.Stylizer := Stylizer;
  	Result := Form.ShowModal = mrOk;
    //if Result then
    	//Stylizer.Assign(Form.Stylizer);
  finally
  	Form.Free;
  end;
end;

{ TStylizerEditorForm }

procedure TStylizerEditorForm.SetStylizer(Value: TDesignStylizer);
begin
  FStylizer.Assign(Value);
end;

procedure TStylizerEditorForm.FormCreate(Sender: TObject);
var
	Document: IDocument;
begin
	FDesigner := TDesignSurface.Create(Self);
  FDesigner.Parent := DesignerFrame;
	FDesigner.Align := alClient;
	FStylizer := TDesignStylizer.Create(Self);
  FStylizer.Designer := FDesigner;
  FStylizer.Designing := Banner.ItemIndex = 0;
	FStylizer.OnQueryInsert := InsertControl;
  FontNameBox.Items := Screen.Fonts;
  BackgroundView.Explore(Configuration.Path[CDesignerBackgroundImages]); //'C:\temp\dev\document');
  BackgroundClearButton.Style := bsTransparent;
  BackgroundEditButton.Style := bsTransparent;
	BackgroundClearButton.Enabled := (Stylizer.Background.Graphic <> nil) and
  	(not Stylizer.Background.Graphic.Empty);
	BackgroundEditButton.Enabled := BackgroundClearButton.Enabled;
  FontItemBoxChange(FontItemBox);
  if FileExists('layout.xml') then
  begin
  	Document := CreateDocument;
    Document.LoadFromFile('layout.xml');
		FStylizer.UndoList.Clear(Document);
		FStylizer.RestoreState;
	end;
end;

procedure TStylizerEditorForm.InsertControl(Sender: TObject; X, Y: Integer;
	var Inserted: Boolean);
var
	Kind: TDesignControl;
	Control: TControl;
  Surface: IDesignSurface;
begin
	if ListBox1.ItemIndex < 0 then Exit;
  Kind := TDesignControl(ListBox1.ItemIndex);
  ListBox1.ItemIndex := -1;
	if Supports(Sender, IDesignSurface, Surface) then
  begin
  	Control := Surface.AddControl(Kind);
    Control.Left := X;
    Control.Top := Y;
    Inserted := True;
	  FStylizer.LayoutDesigner(FDesigner);
  end;
end;

procedure TStylizerEditorForm.BackgroundColorButtonChange(Sender: TObject);
begin
 	if BackgroundBox.ItemIndex = 0 then
		FStylizer.BandColor := BackgroundColorButton.ActiveColor
  else
		FStylizer.Color := BackgroundColorButton.ActiveColor;
  FStylizer.Update;
end;

procedure TStylizerEditorForm.Button1Click(Sender: TObject);
begin
	{if OpenPictureDialog.Execute then
		FStylizer.Background.LoadFromFile(OpenPictureDialog.FileName);}
	//FStylizer.LayoutDesigner(FDesigner);
  ShowMessage(IntToStr(FDesigner.Controls[0].Height));
end;

procedure TStylizerEditorForm.BackgroundViewDefaultAction(Sender: TObject;
  Node: TShellNode; var AllowAction: Boolean);
begin
	AllowAction := False;
  if FileExists(Node.Path) then
  begin
		if BackgroundBox.ItemIndex = 0 then
    begin
	  	Stylizer.BandFileName := Node.Path;
	    Stylizer.RestoreState;
		end
    else
    begin
	  	Stylizer.BackgroundFileName := Node.Path;
      Stylizer.Update;
		end;
		BackgroundClearButton.Enabled := True;
    BackgroundEditButton.Enabled := True;
	end;
end;

procedure TStylizerEditorForm.BackgroundViewIncludeItem(Sender: TObject;
  Node: TShellNode; var AllowAction: Boolean);
var
	S: string;
begin
	if Node.ShellFolder = nil then
  begin
	  S := ExtractFileExt(Node.Name);
  	AllowAction := Pos(UpperCase(S), UpperCase(GraphicFileMask(TGraphic))) > 0;
	end
  else
  	AllowAction := False;
end;

procedure TStylizerEditorForm.BackgroundClearButtonClick(Sender: TObject);
begin
	if BackgroundBox.ItemIndex = 0 then
		FStylizer.BandFileName := ''
	else
		FStylizer.BackgroundFileName := '';
  FStylizer.RestoreState;
  BackgroundBox.SetFocus;
	BackgroundClearButton.Enabled := False;
	BackgroundEditButton.Enabled := False;
end;

procedure TStylizerEditorForm.BackgroundEditButtonClick(Sender: TObject);
begin
	if FileExists(Stylizer.BackgroundFileName) then
	  ShellExecute(Handle, 'open', PChar(Stylizer.BackgroundFileName), nil, nil, SW_SHOW);
end;

procedure TStylizerEditorForm.BannerSelectItem(Sender: TObject);
begin
	FStylizer.Designing := Banner.ItemIndex = 0;
end;

procedure TStylizerEditorForm.BackgroundBoxChange(Sender: TObject);
var
	Picture: TPicture;
begin
	if BackgroundBox.ItemIndex = 0 then
  begin
		BackgroundView.Explore(Configuration.Path[CDesignerBandImages]);
  	Picture := Stylizer.Band;
	  BackgroundColorButton.ActiveColor := Stylizer.BandColor;
	end
	else
  begin
		BackgroundView.Explore(Configuration.Path[CDesignerBackgroundImages]);
  	Picture := Stylizer.Background;
	  BackgroundColorButton.ActiveColor := Stylizer.Color;
	end;
	BackgroundClearButton.Enabled := (Picture.Graphic <> nil) and
  	(not Picture.Graphic.Empty);
	BackgroundEditButton.Enabled := BackgroundClearButton.Enabled;
end;

procedure TStylizerEditorForm.FontItemBoxChange(Sender: TObject);
var
	Font: TFont;
begin
  FActiveFont := nil;
	case FontItemBox.ItemIndex of
  	0: Font := FStylizer.BandFont;
    1: Font := FStylizer.CaptionFont;
	else
  	Font := FStylizer.ControlFont
	end;
  FontNameBox.ItemIndex := FontNameBox.Items.IndexOf(Font.Name);
  FontNameBox.ItemIndex := FontNameBox.Items.IndexOf(Font.Name);
	FontSizeSpinner.Position := Font.Size;
  FontBoldButton.Down := fsBold in Font.Style;
  FontItalicButton.Down := fsItalic in Font.Style;
  FontUnderlineButton.Down := fsUnderline in Font.Style;
  FontColorButton.ActiveColor := Font.Color;
  StyleBox.ItemIndex := Ord(Stylizer.Style);
  case FStylizer.CaptionPosition of
  	cpLeft: PlacementBox.ItemIndex := 1;
    cpTop: PlacementBox.ItemIndex := 0;
    cpHidden: PlacementBox.ItemIndex := 2;
	end;
  GridSizeSpinner.Position := FStylizer.GridSize;
  FActiveFont := Font;
end;

procedure TStylizerEditorForm.FontNameBoxChange(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
  FActiveFont.Name := FontNameBox.Text;
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.FontSizeSpinnerChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
	AllowChange := True;
  if FActiveFont = nil then Exit;
  FActiveFont.Size := NewValue;
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.FontBoldButtonClick(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	if FontBoldButton.Down then
		FActiveFont.Style := FActiveFont.Style + [fsBold]
	else
		FActiveFont.Style := FActiveFont.Style - [fsBold];
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.FontItalicButtonClick(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	if FontItalicButton.Down then
		FActiveFont.Style := FActiveFont.Style + [fsItalic]
	else
		FActiveFont.Style := FActiveFont.Style - [fsItalic];
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.FontUnderlineButtonClick(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	if FontUnderlineButton.Down then
		FActiveFont.Style := FActiveFont.Style + [fsUnderline]
	else
		FActiveFont.Style := FActiveFont.Style - [fsUnderline];
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.FontColorButtonChange(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	FActiveFont.Color := FontColorButton.ActiveColor;
  FStylizer.Refresh;
end;

procedure TStylizerEditorForm.FontSizeEditExit(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	FontSizeSpinner.Position := StrToIntDef(FontSizeEdit.Text, FontSizeSpinner.Position);
  FActiveFont.Size := FontSizeSpinner.Position;
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.OKButtonClick(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	FStylizer.UndoList.State.SaveToFile('layout.xml');
end;

procedure TStylizerEditorForm.StyleBoxChange(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	FStylizer.Style := TDesignStyle(StyleBox.ItemIndex);
end;

procedure TStylizerEditorForm.PlacementBoxChange(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	case PlacementBox.ItemIndex of
  	0: FStylizer.CaptionPosition := cpTop;
    1: FStylizer.CaptionPosition := cpLeft;
    2: FStylizer.CaptionPosition := cpHidden;
	end;
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.GridSizeSpinnerChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
	AllowChange := True;
  if FActiveFont = nil then Exit;
  FStylizer.GridSize := NewValue;
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.GridSizeEditExit(Sender: TObject);
begin
  if FActiveFont = nil then Exit;
	GridSizeSpinner.Position := StrToIntDef(GridSizeEdit.Text, GridSizeSpinner.Position);
  FStylizer.GridSize := GridSizeSpinner.Position;
  FStylizer.RestoreState;
end;

procedure TStylizerEditorForm.AdvancedButtonClick(Sender: TObject);
begin
	with TAdvancedStylesForm.Create(Self) do
  try
  	Stylizer := Self.FStylizer;
  	ShowModal;
  finally
  	Free;
  end;
end;

end.
