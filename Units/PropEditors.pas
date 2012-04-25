
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit PropEditors;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Forms, StdCtrls, ComCtrls, Menus,
  {$IFDEF D6_UP}DesignIntf, DesignWindows, DesignEditors, VCLEditors,
  {$IFDEF D6_UP}DesignMenus,{$ENDIF}{$ELSE}DsgnIntf, {$ENDIF}
  OpenTools, StrEdit, Graphics, Controls, PaneCtrls, ImgList,
  Balloon, BalloonHintFrm, FolderCtrls, FolderBarsFrm, ImageListFrm,
  InspectCtrls, InspectorEditorsFrm, TransparentImageFrm,
  BannerCtrls, BannerBookFrm, WinTools, FormTools, ShlCtrls, TypInfo;

{ TDefaultStringsProperty }

type
  TDefaultStringsProperty = class(TStringListProperty)
  end;

{ TBalloonHintEditor }

  TBalloonHintEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TBalloonHintWizard }

  TBalloonHintWizard = class(TMenuWizard)
  public
    procedure Execute; override;
    function GetMenuText: string; override;
  end;

{ TCustomImageIndexPropertyEditor}

  TCustomImageIndexPropertyEditor = class(TIntegerProperty{$IFDEF D6_UP},
    ICustomPropertyListDrawing{$ENDIF})
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;
     { TCustomImageIndexPropertyEditor.ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); {$IFNDEF D6_UP}override;{$ENDIF}
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); {$IFNDEF D6_UP}override;{$ENDIF}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF D6_UP}override;{$ENDIF}
  end;

{ TFolderBarsProperty }

  TFolderBarsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TFolderBarsEditor }

  TFolderBarsEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TReadOnlyImageListEditor }

  TReadOnlyImageListEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TInspectorEditorsProperty }

  TInspectorEditorsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TInspectorEditorsEditor }

  TInspectorEditorsEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TPaneSheetEditor }

  TPaneSheetEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: {$IFDEF D6_UP}IMenuItem{$ELSE}TMenuItem{$ENDIF}); override;
  end;

{ TTransparentImageProperty }

  TTransparentImageProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TTransparentImageEditor }

  TTransparentImageEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TBannerBookProperty }

  TBannerBookProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TBannerBookEditor }

  TBannerBookEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  StrTools;

{ TBalloonHintEditor }

procedure TBalloonHintEditor.Edit;
begin
  if Component is TBalloonHint then
    EditBalloonHint(Component as TBalloonHint);
end;

procedure TBalloonHintEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TBalloonHintEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Balloon Hint Editor...';
end;

function TBalloonHintEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBalloonHintWizard }

procedure TBalloonHintWizard.Execute;
var
  S: string;
begin
  S := EditBalloonHint;
  if S <> '' then AddText(S);
end;

function TBalloonHintWizard.GetMenuText: string;
begin
  Result := 'Balloon Hint Editor...';
end;

{ TCustomImageIndexPropertyEditor}

function TCustomImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TCustomImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  ImageList: TCustomImageList;
  I: Integer;
begin
  ImageList := GetImageListAt(0);
  if Assigned(ImageList) then
    for I := 0 to ImageList.Count-1 do
      Proc(IntToStr(I));
end;

function TCustomImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  Component: TPersistent;
  CollectionItem: TCollectionItem absolute Component;
  PropInfo: PPropInfo;
begin
  Result := nil;
  Component := GetComponent(Index);
  if (Component is TCollectionItem) then
    Component := CollectionItem.Collection.Owner;
  if Component = nil then Exit;
  PropInfo := TypInfo.GetPropInfo(Component.ClassInfo, 'Images');
  if PropInfo = nil then Exit;
  Result := TCustomImageList(GetObjectProp(Component, PropInfo, TCustomImageList));
end;

{ TCustomImageIndexPropertyEditor.ICustomPropertyListDrawing }

procedure TCustomImageIndexPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImageList: TCustomImageList;
  I: Integer;
begin
  ImageList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  I := ARect.Left + 2;
  if Assigned(ImageList) then begin
    ImageList.Draw(ACanvas, I, ARect.Top + 2, StrToInt(Value));
    Inc(I, ImageList.Width);
  end;
  ACanvas.TextOut(I + 3, ARect.Top + 1, Value);
end;

procedure TCustomImageIndexPropertyEditor.ListMeasureHeight(const Value:
string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImageList: TCustomImageList;
begin
  ImageList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImageList) and(ImageList.Height + 4 > AHeight) then
    AHeight := ImageList.Height + 4;
end;

procedure TCustomImageIndexPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImageList: TCustomImageList;
begin
  ImageList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImageList) then
    Inc(AWidth, ImageList.Width);
end;

{ TFolderBarsProperty }

function TFolderBarsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TFolderBarsProperty.Edit;
var
  Bars: TFolderBars;
begin
  Bars := TFolderBars(GetOrdValue);
  if Bars <> nil then
    if EditFolderBars(Bars) then
      Designer.Modified;
end;

{ TFolderBarsEditor }

procedure TFolderBarsEditor.Edit;
var
  Prop: TObject;
begin
  Prop := GetObjectProp(Component, 'Folders', TFolderBars);
  if Prop <> nil then
    if EditFolderBars(Prop as TFolderBars) then
      Designer.Modified;
end;

procedure TFolderBarsEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TFolderBarsEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Folder Bars Designer...';
end;

function TFolderBarsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TReadOnlyImageListEditor }

procedure TReadOnlyImageListEditor.Edit;
begin
  if Component is TCustomImageList then
    ViewImageList(Component as TCustomImageList);
end;

procedure TReadOnlyImageListEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TReadOnlyImageListEditor.GetVerb(Index: Integer): string;
begin
  Result := 'View Images...';
end;

function TReadOnlyImageListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TInspectorEditorsProperty }

function TInspectorEditorsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TInspectorEditorsProperty.Edit;
var
  InspectorEditors: TInspectorEditors;
begin
  InspectorEditors := TInspectorEditors(GetOrdValue);
  if InspectorEditors <> nil then
    with TInspectorEditorsForm.Create(nil) do
    try
      Editors := InspectorEditors;
      if ShowModal = mrOK then
      begin
        SetOrdValue(Integer(Editors));
        Designer.Modified;
      end;
    finally
      Free;
    end;
end;

{ TInspectorEditorsEditor }

procedure TInspectorEditorsEditor.Edit;
begin
  if EditInspector(TInspector(Component)) then
  begin
    AddUnit(Component.Owner as TCustomForm, 'InspectEditors');
    Designer.Modified;
  end;
end;

procedure TInspectorEditorsEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TInspectorEditorsEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Editors Designer...';
end;

function TInspectorEditorsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TPaneSheetEditor }

procedure TPaneSheetEditor.ExecuteVerb(Index: Integer);
var
  PaneControl: TPaneControl;
  PaneSheet: TPaneSheet;
begin
  if Component is TPaneControl then
    PaneControl := Component as TPaneControl
  else
    PaneControl := TControl(Component).Parent as TPaneControl;
  case Index of
    0:
      begin
        PaneSheet := PaneControl.AddPane;
        PaneControl.ActivePane := PaneSheet;
        PaneSheet.Name := Designer.UniqueName(TPaneSheet.ClassName);
      end;
    1:
      with PaneControl, ActivePane do
        if Index < PaneCount - 1 then
          ActivePane := Panes[Index + 1]
        else
          ActivePane := Panes[0];
    2:
      with PaneControl, ActivePane do
        if Index > 0 then
          ActivePane := Panes[Index - 1]
        else
          ActivePane := Panes[PaneCount - 1];
    3: Component.Free;
  end;
end;

function TPaneSheetEditor.GetVerb(Index: Integer): string;
const
  MenuItems: array[0..3] of PChar = ('New Pane', 'Next Pane', 'Previous Pane',
    'Remove Pane');
begin
  Result := MenuItems[Index];
end;

function TPaneSheetEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

procedure TPaneSheetEditor.PrepareItem(Index: Integer; const AItem: {$IFDEF D6_UP}IMenuItem{$ELSE}TMenuItem{$ENDIF});
var
  PaneControl: TPaneControl;
begin
  if Component is TPaneControl then
    PaneControl := Component as TPaneControl
  else
    PaneControl := TControl(Component).Parent as TPaneControl;
  case Index of
    1, 2: AItem.Enabled := PaneControl.PaneCount > 1;
    3: AItem.Enabled := Component is TPaneSheet;
  end;
end;

{ TTransparentImageProperty }

procedure TTransparentImageProperty.Edit;
var
  Component: TPersistent;
begin
  Component := GetComponent(0);
  if Component is TTransparentImage then
    if EditTransparentImage(Component as TTransparentImage) then
      Designer.Modified;
end;

function TTransparentImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TTransparentImageEditor }

procedure TTransparentImageEditor.Edit;
begin
  if Component is TTransparentImage then
    if EditTransparentImage(Component as TTransparentImage) then
      Designer.Modified;
end;

procedure TTransparentImageEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TTransparentImageEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Transparent Images Editor...';
end;

function TTransparentImageEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBannerBookProperty }

procedure TBannerBookProperty.Edit;
var
  BannerBook: TPersistent;
begin
  BannerBook := GetComponent(0);
  if (BannerBook is TBannerBook) and EditBannerBook(TBannerBook(BannerBook)) then
    Designer.Modified;
end;

function TBannerBookProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TBannerBookEditor }

procedure TBannerBookEditor.Edit;
begin
  ExecuteVerb(0);
end;

procedure TBannerBookEditor.ExecuteVerb(Index: Integer);
var
  Pages: TStrings;
  PageIndex: Integer;
  NextPage: Integer;
begin
  Pages := TStrings(GetObjectProp(Component, 'Pages', TStrings));
  PageIndex := GetOrdProp(Component, 'PageIndex');
  if Pages = nil then Exit;
  if Pages.Count = 0 then Exit;
  NextPage := PageIndex;
  case Index of
    0:
      begin
        Inc(NextPage);
        if NextPage > Pages.Count - 1 then
          NextPage := 0;
      end;
    1:
      begin
        Dec(NextPage);
        if NextPage < 0 then
          NextPage := Pages.Count - 1;
      end;
  end;
  if NextPage <> PageIndex then
  begin
    SetOrdProp(Component, 'PageIndex', NextPage);
    Designer.Modified;
  end;
end;

function TBannerBookEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'Next Page';
    1: Result := 'Previous Page';
  end;
end;

function TBannerBookEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure RegisterTransparentUnitsImplementation(AOwner: TComponent);
begin
  AddUnit(AOwner, 'PngTools');
end;

procedure RegisterInspectorUnitsImplementation(AOwner: TComponent);
begin
  AddUnit(AOwner, 'InspectEditors');
end;

procedure RegisterShellUnitsImplementation(AOwner: TComponent);
begin
  AddUnit(AOwner, 'ShellTools');
end;

const
  DM_PASTESELECTION = WM_USER + $0A;
  DM_REMOVESELECTION = DM_PASTESELECTION + 1;

type
  TPageNotifier = class(TInterfacedObject, IDesignNotification)
  private
    FDesigner: IDesigner;
    FSelection: IDesignerSelections;
    FPageClass: TClass;
    FPage: TObject;
    FPages: TBannerBook;
    FMoving: Boolean;
    FWindow: TUtilityWindow;
    procedure DMPasteSelection(var Msg: TMessage); message DM_PASTESELECTION;
    procedure DMRemoveSelection(var Msg: TMessage); message DM_REMOVESELECTION;
    { IDesignNotification }
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
  public
    constructor Create(PageClass: TClass);
    destructor Destroy; override;
  end;

{ TPageNotifier }

constructor TPageNotifier.Create(PageClass: TClass);
begin
  inherited Create;
  FPageClass := PageClass;
  FWindow := TUtilityWindow.Create(Self);
end;

destructor TPageNotifier.Destroy;
begin
  FDesigner := nil;
  FSelection := nil;
  FWindow.Free;
  inherited Destroy;
end;

procedure TPageNotifier.DMPasteSelection(var Msg: TMessage);
begin
  if FPages.Page = nil then Exit;
  FDesigner.CutSelection;
  FDesigner.SelectComponent(TComponent(FPages.Page));
  FDesigner.PasteSelection;
  FDesigner.Modified;
  FDesigner := nil;
  FMoving := False;
end;

procedure TPageNotifier.DMRemoveSelection(var Msg: TMessage);
begin
  if FPage is TControl then
    FDesigner.SelectComponent(TControl(FPage).Parent)
  else if FPage is TComponent then
    FDesigner.SelectComponent(TComponent(FPage).Owner);
  FDesigner.Modified;
  FDesigner := nil;
end;

{ TPageNotifier.IDesignNotification }

procedure TPageNotifier.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
end;

procedure TPageNotifier.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
end;

procedure TPageNotifier.ItemsModified(const ADesigner: IDesigner);
begin
end;

procedure TPageNotifier.SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
var
  P: TPersistent;
  I: Integer;
begin
  if FMoving then Exit;
  for I := 0 to ASelection.Count - 1 do
  begin
    P := ASelection.Items[I];
    if P is FPageClass then
    begin
      FDesigner := ADesigner;
      FPage := P;
      PostMessage(FWindow.Handle, DM_REMOVESELECTION, 0, 0);
      Break;
    end
    else if (P is TControl) and (TControl(P).Parent is TBannerBook) then
    begin
      FMoving := True;
      FDesigner := ADesigner;
      FPages := TBannerBook(TControl(P).Parent);
      PostMessage(FWindow.Handle, DM_PASTESELECTION, 0, 0);
      Break;
    end;
  end;
end;

procedure TPageNotifier.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
begin
end;

procedure TPageNotifier.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
end;

var
  PageNotifier: TPageNotifier;

procedure InternalRegisterPages(PageClass: TClass);
begin
  if PageNotifier = nil then
  begin
    PageNotifier := TPageNotifier.Create(PageClass);
    RegisterDesignNotificationProc(PageNotifier);
    if PageClass.InheritsFrom(TPersistent) then
      RegisterClass(TPersistentClass(PageClass));
    if PageClass.InheritsFrom(TComponent) then
      RegisterNoIcon([TComponentClass(PageClass)]);
  end;
end;

initialization
  @RegisterPages := @InternalRegisterPages;
  @RegisterTransparentUnits := @RegisterTransparentUnitsImplementation;
  @RegisterInspectorUnits := @RegisterInspectorUnitsImplementation;
  @RegisterShellUnits := @RegisterShellUnitsImplementation;
finalization
  if PageNotifier <> nil then
    UnregisterDesignNotificationProc(PageNotifier);
end.
