unit AppExplorerFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, InspectEditors, FormTools, ScrollCtrls, InspectCtrls, InspectorEditorsFrm,
  ComCtrls, PaneCtrls, BtnEdit, ClassTools, TypInfo, SuplCtrls, ExtCtrls,
  BannerCtrls, Grip, ImgList, ImageListEx, Dialogs, DialogsEx, BtnCtrls,
  BlendTools, GraphTools, Menus, WinShot, StdActns, XMLParser, SearchCtrls,
  ResourceData, FlowCtrls, WebCtrls;

type
  TAppExplorerForm = class(TGripForm)
    ComponentFrame: TFramedWindow;
    PathLabel: TLabel;
    UnitLabel: TLabel;
    TreeImages: TGlassImageList;
    DefaultImage: TImageList;
    WinFrame: TFramedWindow;
    PropertiesBox: TCaptionBox;
    Panel1: TPanel;
    HorizontalBar2: THorizontalBar;
    PropTypeLabel: TLabel;
    PropertyLabel: TLabel;
    Inspector: TInspector;
    PaneControl: TPaneControl;
    Splitter: TSplitter;
    TreeSheet: TPaneSheet;
    ComponentTree: TTreeView;
    HorizontalBar3: THorizontalBar;
    DockImages: TGlassImageList;
    BackButton: TImageButton;
    ForwardButton: TImageButton;
    ComponentLabel: TLabel;
    ResourceSheet: TPaneSheet;
    PackageSheet: TPaneSheet;
    HorizontalBar1: THorizontalBar;
    HorizontalBar4: THorizontalBar;
    ResourceText: TMemo;
    Panel2: TPanel;
    PreviewBox: TCaptionBox;
    PreviewSplitter: TSplitter;
    PreviewBar: THorizontalBar;
    ParentLabel: TLabel;
    InsertTimer: TTimer;
    RemoveTimer: TTimer;
    SearchBar: TSearchBar;
    TopSite: TFlowSite;
    FlowBar: TFlowBar;
    CenterSite: TFlowSite;
    RightSite: TFlowSite;
    BottomSite: TFlowSite;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure ForwardButtonClick(Sender: TObject);
    procedure ComponentTreeChange(Sender: TObject; Node: TTreeNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComponentTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ParentLabelMouseEnter(Sender: TObject);
    procedure ParentLabelMouseLeave(Sender: TObject);
    procedure ParentLabelClick(Sender: TObject);
    procedure PreviewBoxClose(Sender: TObject);
    procedure PreviewButtonClick(Sender: TObject);
    procedure InsertTimerTimer(Sender: TObject);
    procedure RemoveTimerTimer(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchBarFindEditChange(Sender: TObject);
    procedure SearchBarNextButtonClick(Sender: TObject);
    procedure SearchBarPriorButtonClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FlowBarButtonClick(Sender: TObject; Button: Integer);
    procedure SearchBarFindButtonClick(Sender: TObject);
    procedure PaneControlPaneChange(Sender: TObject; OldPane,
      NewPane: TPaneSheet);
  private
    FPropertyInspector: TPropertyInspector;
    FWinCapture: TWinCapture;
    FNodeList: TList;
    FComponentList: TList;
		FInsertParent: TTreeNode;
		FInsertComponent: TComponent;
    FDeleteTime: TDateTime;
    FPackages: IDocument;
    FStyleSheet: IDocument;
    FStyleFile: string;
    FRoot: INode;
    FBrowser: TDocBrowser;
    FPackageLoaded: Boolean;
    FComponentLoaded: Boolean;
    FLastPackage: string;
    FSearchText: string;
    procedure BuildNodes(Nodes: TTreeNodes; Parent: TTreeNode; C: TComponent);
    procedure BuildApplicationTree(TreeView: TTreeView);
    procedure PropertyEdit(Sender: TObject);
    procedure PropertyChange(Sender: TObject);
    procedure InstanceChange;
    procedure WMEnable(var Msg: TWMEnable); message WM_ENABLE;
	protected
  	procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

procedure ExploreApplication;

implementation

{$R *.dfm}

type
	TExplorerDispatch = class(TAutoDispatch)
  private
  	FExplorer: TAppExplorerForm;
  protected
  	function OnMethod(MethodIndex: Integer; const Args: TArgList): OleVariant; override;
	public
  	constructor Create; override;
    property Explorer: TAppExplorerForm read FExplorer write FExplorer;
  end;

constructor TExplorerDispatch.Create;
begin
	inherited Create;
  //Methods.Add('all');
end;

function TExplorerDispatch.OnMethod(MethodIndex: Integer; const Args: TArgList): OleVariant;
begin
end;

var
	AppExplorerForm: TAppExplorerForm;

procedure ExploreApplication;
begin
	if ResourceModule = nil then
  	Application.CreateForm(TResourceModule, ResourceModule);
	if AppExplorerForm = nil then
  	Application.CreateForm(TAppExplorerForm, AppExplorerForm);
	AppExplorerForm.Show;
	AppExplorerForm.BringToFront;
end;

type
	TClassImage = record
  	ClassType: TClass;
    Image: Integer;
	end;

const
	ClassImages: array[0..31] of TClassImage = (
  	(ClassType: TMenuItem; Image: 19),
  	(ClassType: TMenu; Image: 19),
  	(ClassType: TBasicAction; Image: 0),
  	(ClassType: TDataModule; Image: 1),
  	(ClassType: TCustomForm; Image: 1),
  	(ClassType: TScrollingWinControl; Image: 2),
  	(ClassType: TCustomFrame; Image: 2),
  	(ClassType: TControlScrollBar; Image: 3),
  	(ClassType: TScrollBar; Image: 4),
  	(ClassType: TCustomStatusBar; Image: 5),
  	(ClassType: TCustomTabControl; Image: 6),
  	(ClassType: TCustomGroupBox; Image: 7),
  	(ClassType: TCustomListBox; Image: 8),
  	(ClassType: TCustomComboBox; Image: 8),
  	(ClassType: TProgressBar; Image: 9),
  	(ClassType: TCustomCheckBox; Image: 10),
  	(ClassType: TRadioButton; Image: 11),
  	(ClassType: TButtonControl; Image: 12),
  	(ClassType: TCustomRichEdit; Image: 14),
  	(ClassType: TCustomMemo; Image: 15),
  	(ClassType: TCustomPanel; Image: 19),
  	(ClassType: TCustomEdit; Image: 16),
  	(ClassType: TCustomControl; Image: 18),
  	(ClassType: TWinControl; Image: 17),
  	(ClassType: TCustomControl; Image: 17),
  	(ClassType: TCustomLabel; Image: 20),
  	(ClassType: TImage; Image: 21),
  	(ClassType: TGraphicControl; Image: 22),
  	(ClassType: TCustomImageList; Image: 23),
  	(ClassType: TCommonDialog; Image: 24),
  	(ClassType: TControl; Image: 25),
  	(ClassType: TComponent; Image: 26)
  );

procedure EnumPackageProc(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);

	function CheckName: string;
  begin
  	if Name = '' then
    	Result := ExtractFileName(ParamStr(0))
		else
    	Result := Name;
  end;

var
	Node: INode absolute Param;
begin
	case NameType of
	  ntContainsUnit: Node.Nodes.Add('unit').Attributes.Filer.WriteString('name', Name);
  	ntRequiresPackage: Node.Nodes.Add('package').Attributes.Filer.WriteString('name', Name);
    ntDcpBpiName: Node.Attributes.Filer.WriteString('name', CheckName);
	end;
end;

function EnumModulesProc(HInstance: Integer; Data: Pointer): Boolean;
var
	Node: INode absolute Data;
  Child: INode;
  Flags: Integer;
begin
	Child := Node.Nodes.Add('module');
	GetPackageInfo(HInstance, Pointer(Child), Flags, EnumPackageProc);
  Result := True;
end;

procedure TAppExplorerForm.FormCreate(Sender: TObject);
begin
	DesktopFont := True;
  FWinCapture := TWinCapture.Create(Self);
  FWinCapture.Parent := PreviewBox;
  FWinCapture.Align := alClient;
	FPropertyInspector := TPropertyInspector.Create;
  FPropertyInspector.OnChange := PropertyChange;
  FPropertyInspector.OnEdit := PropertyEdit;
  BackButton.Width := 48;
  ForwardButton.Width := 48;
  FPackages := CreateDocument;
  FRoot := FPackages.CreateNode('modules');
  FPackages.Root := FRoot;
  EnumModules(EnumModulesProc, Pointer(FRoot));
  FNodeList := TList.Create;
  FComponentList := TList.Create;
  BuildApplicationTree(ComponentTree);
  FBrowser := TDocBrowser.Create(Self);
  TControl(FBrowser).Parent := PackageSheet;
  FBrowser.Align := alClient;
  FBrowser.Navigate('about:blank');
  FBrowser.Options := [doScrollbars, doSelection];
end;

procedure TAppExplorerForm.FormDestroy(Sender: TObject);
var
	I: Integer;
begin
	for I := 0 to (FNodeList.Count - 1) div 2 do
		TComponent(FNodeList[I * 2 + 1]).RemoveFreeNotification(Self);
	FNodeList.Free;
  FComponentList.Free;
	FPropertyInspector.Free;
end;


function FormatName(C: TComponent): string;
begin
	Result := C.Name;
  if Result = '' then
  	Result := C.ClassName
	else
  	Result := Result + ': ' + C.ClassName;
end;

procedure TAppExplorerForm.BuildNodes(Nodes: TTreeNodes; Parent: TTreeNode; C: TComponent);
var
	I: Integer;
begin
	FInsertParent := nil;
	FInsertComponent := nil;
	if C = Self then
	begin
		Parent.Free;
		Exit;
	end;
  if FComponentList.IndexOf(C.ClassType) < 0 then
  	FComponentList.Add(C.ClassType);
	FNodeList.Add(Parent);
	FNodeList.Add(C);
	Parent.Data := C;
	C.FreeNotification(Self);
	for I := Low(ClassImages) to High(ClassImages) do
		if C is ClassImages[I].ClassType then
		begin
			Parent.SelectedIndex := ClassImages[I].Image;
			Parent.ImageIndex := ClassImages[I].Image;
			Break;
		end;
	for I := 0 to C.ComponentCount - 1 do
		BuildNodes(Nodes, Nodes.AddChild(Parent, FormatName(C.Components[I])), C.Components[I]);
end;


procedure TAppExplorerForm.BuildApplicationTree(TreeView: TTreeView);
var
	Nodes: TTreeNodes;
  C: TClass;
	N: INode;
  I: Integer;
begin
	Nodes := TreeView.Items;
  Nodes.BeginUpdate;
  try
  	Nodes.Clear;
		BuildNodes(Nodes, Nodes.Add(nil, FormatName(Application)), Application);
  finally
  	Nodes.EndUpdate;
  end;
	for I := 0 to FComponentList.Count - 1 do
  begin
  	C := TClass(FComponentList[I]);
    N := FRoot.FindNode('//unit[@name = "' + GetTypeData(C.ClassInfo).UnitName +  '"]');
		N.Nodes.Add('component').Attributes.Filer.WriteString('name', C.ClassName);
  end;
end;

procedure TAppExplorerForm.PropertyChange(Sender: TObject);
var
	PropInfo: PPropInfo;
  TypeData: PTypeData;
begin
	PropInfo := FPropertyInspector.PropInfo;
  if (PropInfo <> nil) and (Inspector.Selected <> nil) then
	  PropertyLabel.Caption := Inspector.Selected.Name + ': ' + Inspector.Selected.Text
	else
  	PropertyLabel.Caption := '';
  if PropInfo <> nil then
  begin
    TypeData := GetTypeData(PropInfo.PropType^);
    case PropInfo.PropType^^.Kind of
     	tkClass:
      	begin
		    	PropTypeLabel.Caption := TypeData.ClassType.ClassName + ' defined in ' + TypeData.UnitName + '.pas';
				end;
		else
	    PropTypeLabel.Caption := PropInfo.PropType^^.Name + ' derived from ' + TypeKindNames[PropInfo.PropType^^.Kind];
		end;
  end
  else
  	PropTypeLabel.Caption := '';
end;

procedure TAppExplorerForm.InstanceChange;

	procedure BuildUnitInfo(const UnitName: string);
  var
  	Node: INode;
  begin
  	Node := FRoot.FindNode('//unit[@name = "' + UnitName +  '"]');
    if Node = nil then
    begin
      UnitLabel.Caption := 'Defined in ' + UnitName;
      Exit;
    end;
    Node := Node.Parent;
		UnitLabel.Caption := 'Defined in ' + UnitName + ' contained in ' + Node.Attributes['name'].Value;
    if FLastPackage = Node.Attributes['name'].Value then Exit;
    FLastPackage := Node.Attributes['name'].Value;
    if PackageSheet.Visible  and (FStylesheet <> nil) then
    try
	    FPackageLoaded := True;
    	FBrowser.Load(FPackages.Transform(FStyleSheet));
		except
	    FBrowser.Navigate('about:blank');
      FStyleSheet := nil;
      raise;
		end;
  end;

var
	O: TObject;
  P: TComponent;
	C: TClass;
  S: string;
  I: Integer;
begin
	O := FPropertyInspector.Instance;
	if O <> nil then
  begin
  	P := nil;
  	if (O is TComponent) and (TComponent(O).Name <> '') then
    	S := TComponent(O).Name + ': '
		else
    	S := '';
		S :=  S + O.ClassName + ' ($' + IntToHex(Integer(O), 8) + ')';
    if (O is TControl) and (TControl(O).Parent <> nil) then
    begin
			S := S + ' parented to ';
      P := TControl(O).Parent;
		end
    else if (O is TComponent) and (TComponent(O).Owner <> nil) then
		begin
			S := S + ' owned by ';
      P := TComponent(O).Owner;
		end;
		ComponentLabel.Caption := S;
    if P <> nil then
		begin
			if P.Name <> '' then
      	ParentLabel.Caption := P.Name
			else
      	ParentLabel.Caption := P.ClassName;
			ParentLabel.Left := ComponentLabel.Left + ComponentLabel.Width;
    end
    else
    	ParentLabel.Caption := '';
    ParentLabel.Tag := Integer(P);
		C := O.ClassType;
		S := '';
		repeat
			S := S + C.ClassName + '.';
			C := C.ClassParent;
	  until C = TObject;
		S := S + C.ClassName;
    PathLabel.Caption := S;
    FPackageLoaded := False;
    BuildUnitInfo(GetTypeData(O.ClassInfo).UnitName);
    FComponentLoaded := False;
		if ResourceSheet.Visible then
    begin
	    FComponentLoaded := True;
      if (O is TComponent) then
	    	ResourceText.Text := ComponentToString(TComponent(O))
			else
	    	ResourceText.Text := '';
		end;
	  if (ComponentTree.Selected = nil) or (ComponentTree.Selected.Data <>
    	O) then
		begin
    	ComponentTree.OnChange := nil;
      I := FNodeList.IndexOf(O);
      if I > 0 then
				ComponentTree.Selected := FNodeList[I - 1];
      ComponentTree.OnChange := ComponentTreeChange;
    end;
	end
  else
  begin
		ParentLabel.Caption := '';
  	ComponentLabel.Caption := '';
    PathLabel.Caption := '';
		UnitLabel.Caption := '';
		ComponentTree.OnChange := nil;
		ComponentTree.Selected := nil;
		ComponentTree.OnChange := ComponentTreeChange;
  end;
	PropertyChange(FPropertyInspector);
  if not PreviewBox.Visible then
		FWinCapture.Target := nil
  else
		FWinCapture.Target := O;
  ForwardButton.Repaint;
  BackButton.Repaint;
end;

procedure TAppExplorerForm.BackButtonClick(Sender: TObject);
begin
	BackButton.Enabled := FPropertyInspector.GoBack;
	ForwardButton.Enabled := True;
	InstanceChange;
end;

procedure TAppExplorerForm.ForwardButtonClick(Sender: TObject);
begin
	BackButton.Enabled := True;
	ForwardButton.Enabled := FPropertyInspector.GoForward;
	InstanceChange;
end;

procedure TAppExplorerForm.PropertyEdit(Sender: TObject);
begin
	ForwardButton.Enabled := False;
  BackButton.Enabled := True;
	InstanceChange;
end;

procedure TAppExplorerForm.ComponentTreeChange(Sender: TObject; Node: TTreeNode);
var
	CanEnable: Boolean;
begin
	if ComponentTree.Selected <> nil then
  begin
    if ComponentTree.Selected.Data = Pointer(FPropertyInspector.Instance) then Exit;
    FPropertyInspector.Instance := TObject(ComponentTree.Selected.Data);
    CanEnable := Inspector.InspectObject <> nil;
    if Inspector.InspectObject = nil then
	  	Inspector.InspectObject := FPropertyInspector;
		BackButton.Enabled := CanEnable;
    ForwardButton.Enabled := False;
    InstanceChange;
  end;
end;

procedure TAppExplorerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caFree;
  AppExplorerForm := nil;
end;

procedure TAppExplorerForm.ComponentTreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
	DC: HDC;
	A: TRect;
begin
	DC := ComponentTree.Canvas.Handle;
	A := Node.DisplayRect(True);
	Dec(A.Left, 20);
  A.Right := 3000;
	FillRectColor(DC, A, ComponentTree.Color);
  ImageListDraw(TreeImages, Sender.Canvas, A.Left, A.Top, Node.ImageIndex);
  A := Node.DisplayRect(True);
  with CalculateCaptionSize(DC, Node.Text) do
	  A.Right := A.Left + cx + cy div 2;
	OffsetRect(A, -HeightOf(A) div 4 + 2, 0);
  A.Right := A.Right + 2;
  if Node = ComponentTree.Selected then
  	DrawStyleRect(DC, A, not (cdsSelected in State));
  SetTextColor(DC, ColorToRGB(clWindowText));
	OffsetRect(A, HeightOf(A) div 4, 0);
  DrawCaption(DC, Node.Text, A, drLeft);
	A := Node.DisplayRect(True);
	Dec(A.Left, 20);
  A.Right := 3000;
	SelectClipRect(DC, A, RGN_DIFF);
 	DefaultDraw := True;
end;

procedure TAppExplorerForm.ParentLabelMouseEnter(Sender: TObject);
begin
	ParentLabel.Font.Color := clBlue;
	ParentLabel.Font.Style := [fsBold, fsUnderline];
end;

procedure TAppExplorerForm.ParentLabelMouseLeave(Sender: TObject);
begin
	ParentLabel.Font.Color := clWindowText;
	ParentLabel.Font.Style := [fsBold];
end;

procedure TAppExplorerForm.ParentLabelClick(Sender: TObject);
begin
	if ParentLabel.Tag <> 0 then
  begin
  	FPropertyInspector.Instance := TObject(ParentLabel.Tag);
    InstanceChange;
	end;
end;

procedure TAppExplorerForm.PreviewBoxClose(Sender: TObject);
begin
  PreviewBar.Visible := False;
	PreviewSplitter.Visible := False;
end;

procedure TAppExplorerForm.PreviewButtonClick(Sender: TObject);
begin
	PreviewBox.Visible := True;
  PreviewSplitter.Top := PreviewBox.Top - 10;
end;

procedure TAppExplorerForm.Notification(AComponent: TComponent;
  Operation: TOperation);

  function IsSelfOwned(C: TComponent): Boolean;
  begin
  	if C = Self then
    	Result := True
		else if C = nil then
    	Result := False
		else
    	Result := IsSelfOwned(C.Owner);
  end;

var
	I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if AComponent.Owner = nil then Exit;
  if AComponent is THintAction then Exit;
  case Operation of
  	opRemove:
    	begin
      	AComponent.RemoveFreeNotification(Self);
      	if IsSelfOwned(AComponent) then Exit;
        //DebugStrings.Insert(0, FormatName(AComponent));
      	I := FNodeList.IndexOf(AComponent);
        if I < 1 then Exit;
				ComponentTree.OnChange := nil;
        try
					TObject(FNodeList[I - 1]).Free;
					FNodeList.Delete(I - 1);
					FNodeList.Delete(I - 1);
					FPropertyInspector.Remove(AComponent);
          if FPropertyInspector.Instance = nil then
          begin
          	ComponentTree.Selected := nil;
          	InstanceChange;
          	BackButton.Enabled := False;
          	ForwardButton.Enabled := False;
            RemoveTimer.Enabled := False;
          end
          else
          begin
  	        FDeleteTime := Now;
	          RemoveTimer.Enabled := True;
          end;
        finally
					ComponentTree.OnChange := ComponentTreeChange;
        end;
      end;
    opInsert:
    	begin
      	if IsSelfOwned(AComponent) then Exit;
        I := FNodeList.IndexOf(AComponent.Owner);
        if I < 1 then Exit;
        FInsertParent := TTreeNode(FNodeList[I - 1]);
        FInsertComponent := AComponent;
        InsertTimer.Enabled := True;
      end;
	end;
end;

procedure TAppExplorerForm.WMEnable(var Msg: TWMEnable);
begin
	inherited;
  if not Msg.Enabled then
  	EnableWindow(Handle, True)
end;

procedure TAppExplorerForm.InsertTimerTimer(Sender: TObject);
var
	Nodes: TTreeNodes;
begin
	InsertTimer.Enabled := False;
  if (FInsertParent = nil) or (FInsertComponent = nil) then Exit;
	if FInsertComponent.Owner = nil then Exit;
	Nodes := ComponentTree.Items;
	Nodes.BeginUpdate;
	try
		BuildNodes(Nodes, Nodes.AddChild(FInsertParent, FormatName(FInsertComponent)),
    	FInsertComponent);
	finally
		Nodes.EndUpdate;
	end;
end;

procedure TAppExplorerForm.RemoveTimerTimer(Sender: TObject);
const
	ExpireTime = (1 / (3600 * 50));
begin
	if Now - FDeleteTime < ExpireTime then Exit;
	RemoveTimer.Enabled := False;
	if FPropertyInspector.Instance <> nil then
		InstanceChange;
end;

procedure TAppExplorerForm.SearchButtonClick(Sender: TObject);
begin
	SearchBar.Visible := True;
  SearchBar.FindEdit.SetFocus;
end;

function ComponentMatch(const S: string; C: TComponent): Boolean;
begin
  Result := False;
	if (C = nil) or (S = '') then Exit;
	Result := (Pos(S, UpperCase(C.Name)) > 0) or (Pos(S, UpperCase(C.ClassName)) > 0) or
  	((C is TControl) and (Pos(S, UpperCase(TAppExplorerForm(C).Caption)) > 0));
end;

procedure TAppExplorerForm.SearchBarFindEditChange(Sender: TObject);
var
	I, J: Integer;
begin
	FSearchText := Trim(UpperCase(SearchBar.FindEdit.Text));
  if FSearchText = '' then
  begin
  	SearchBar.NextButton.Enabled := False;
    SearchBar.PriorButton.Enabled := False;
    SearchBar.FindEdit.Color := clWindow;
    Exit;
  end;
  if ComponentTree.Selected = nil then Exit;
  I := FNodeList.IndexOf(ComponentTree.Selected.Data);
  if I < 1 then Exit;
  if ComponentMatch(FSearchText, FNodeList[I]) then
	begin
		SearchBar.NextButton.Enabled := True;
		SearchBar.PriorButton.Enabled := True;
		SearchBar.FindEdit.Color := clWindow;
    Exit;
	end;
	J := I + 2;
  while J < FNodeList.Count - 1 do
  begin
		if ComponentMatch(FSearchText, TComponent(FNodeList[J])) then
    begin
  		SearchBar.NextButton.Enabled := True;
			SearchBar.PriorButton.Enabled := True;
	    SearchBar.FindEdit.Color := clWindow;
      ComponentTree.Selected := TTreeNode(FNodeList[J - 1]);
			Exit;
    end;
  	Inc(J, 2);
  end;
	J := 1;
  while J < I do
  begin
		if ComponentMatch(FSearchText, TComponent(FNodeList[J])) then
    begin
  		SearchBar.NextButton.Enabled := True;
			SearchBar.PriorButton.Enabled := True;
	    SearchBar.FindEdit.Color := clWindow;
      ComponentTree.Selected := TTreeNode(FNodeList[J - 1]);
			Exit;
    end;
  	Inc(J, 2);
  end;
	SearchBar.NextButton.Enabled := False;
	SearchBar.PriorButton.Enabled := False;
	SearchBar.FindEdit.Color := $00A0A0FF;
end;

procedure TAppExplorerForm.SearchBarNextButtonClick(Sender: TObject);
var
	I, J: Integer;
begin
  if (FSearchText = '') or (ComponentTree.Selected = nil) then Exit;
  I := FNodeList.IndexOf(ComponentTree.Selected.Data);
  if I < 1 then Exit;
	J := I + 2;
  while J < FNodeList.Count do
  begin
		if ComponentMatch(FSearchText, TComponent(FNodeList[J])) then
    begin
      ComponentTree.Selected := TTreeNode(FNodeList[J - 1]);
			Exit;
    end;
  	Inc(J, 2);
  end;
	J := 1;
  while J < I do
  begin
		if ComponentMatch(FSearchText, TComponent(FNodeList[J])) then
    begin
      ComponentTree.Selected := TTreeNode(FNodeList[J - 1]);
			Exit;
    end;
  	Inc(J, 2);
  end;
end;

procedure TAppExplorerForm.SearchBarPriorButtonClick(Sender: TObject);
var
	I, J: Integer;
begin
  if (FSearchText = '') or (ComponentTree.Selected = nil) then Exit;
  I := FNodeList.IndexOf(ComponentTree.Selected.Data);
  if I < 1 then Exit;
	J := I - 2;
  while J > 1 do
  begin
		if ComponentMatch(FSearchText, TComponent(FNodeList[J])) then
    begin
      ComponentTree.Selected := TTreeNode(FNodeList[J - 1]);
			Exit;
    end;
  	Dec(J, 2);
  end;
	J := FNodeList.Count - 1;
  while J > I do
  begin
		if ComponentMatch(FSearchText, TComponent(FNodeList[J])) then
    begin
      ComponentTree.Selected := TTreeNode(FNodeList[J - 1]);
			Exit;
    end;
  	Dec(J, 2);
  end;
end;

procedure TAppExplorerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
	if (Key = Ord('F')) and (ssCtrl in Shift) then
  begin
  end;
end;

procedure TAppExplorerForm.FlowBarButtonClick(Sender: TObject;
  Button: Integer);
begin
	case Button of
  	0:
    	begin
				PreviewBox.Visible := True;
        PreviewBox.Top := 0;
				PreviewSplitter.Visible := True;
        PreviewSplitter.Top := 0;
        PreviewBar.Visible := True;
        PreviewBar.Top := 0;
      end;
  	1:
    	begin
				SearchBar.Visible := True;
	  		SearchBar.FindEdit.SetFocus;
      end;
		5:
    	if OpenDialog.Execute then
    	begin
      	FStyleSheet := CreateDocument;
        try
        	FStyleFile := OpenDialog.FileName;
        	FStyleSheet.LoadFromFile(FStyleFile);
					if PackageSheet.Visible then
          begin
		    		FBrowser.Load(FPackages.Transform(FStyleSheet));
				    FPackageLoaded := True;
					end;
				except
			    FBrowser.Navigate('about:blank');
	    	  FStyleSheet := nil;
					raise;
   		  end;
			end;
		6:
			if PackageSheet.Visible and (FileExists(FStyleFile)) then
    	try
      	FStyleSheet := CreateDocument;
				FStyleSheet.LoadFromFile(FStyleFile);
				FBrowser.Load(FPackages.Transform(FStyleSheet));
			except
				FBrowser.Navigate('about:blank');
				FStyleSheet := nil;
        raise;
      end;
	end;
end;

procedure TAppExplorerForm.SearchBarFindButtonClick(Sender: TObject);
begin
	SearchBar.Visible := False;
end;

procedure TAppExplorerForm.PaneControlPaneChange(Sender: TObject; OldPane,
  NewPane: TPaneSheet);
begin
	if NewPane = ResourceSheet then
  	if not FComponentLoaded then
    begin
    	FComponentLoaded := True;
      if FPropertyInspector.Instance is TComponent then
	    	ResourceText.Text := ComponentToString(TComponent(FPropertyInspector.Instance))
			else
	    	ResourceText.Text := '';
		end
    else
  else if NewPane = PackageSheet then
  	if not FPackageLoaded then
	    if FStylesheet <> nil then
  	  try
	  	  FPackageLoaded := True;
    		FBrowser.Load(FPackages.Transform(FStyleSheet));
			except
		    FBrowser.Navigate('about:blank');
    	  FStyleSheet := nil;
      	raise;
			end;
end;

end.
