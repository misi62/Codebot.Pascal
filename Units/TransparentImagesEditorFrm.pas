unit TransparentImagesEditorFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GraphTools, StdCtrls, FormTools, 
  ExtDlgs, SuplCtrls, GridCtrls, ImgListEx, Grip,
  ProviderTools, DropMgr, ClipBrd, ClassTools, ImgList;

type
  TTransparentImagesEditorForm = class(TGripForm)
    OpenButton: TButton;
    DeleteButton: TButton;
    ResetButton: TButton;
    OkButton: TButton;
    CancelButton: TButton;
    OpenDialog: TOpenPictureDialog;
    HelpLabel: TLabel;
    SaveButton: TButton;
    SaveDialog: TSaveDialog;
    Frame: TFramedWindow;
    Bar: THorizontalBar;
    CopyButton: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenClick(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CopyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FImages: TTransparentImageList;
    FDragOrder: Boolean;
    FDragIndex: Integer;
    FGrid: TImageListGrid;
    procedure ListBox1Selection(Sender: TObject; Col, Row: Integer;
      var Allow: Boolean);
    procedure ButtonsChange;
    procedure DropFiles(Sender: TObject; Files: TStrings);
  end;

function EditTransparentImages(Images: TTransparentImageList): Boolean;

implementation

{$R *.dfm}

function EditTransparentImages(Images: TTransparentImageList): Boolean;
begin
  with TTransparentImagesEditorForm.Create(Application) do
  try
    FImages.Assign(Images);
    SaveButton.Enabled := FImages.Count > 0;
    DeleteButton.Enabled := FImages.Count > 0;
    ResetButton.Enabled := FImages.Count > 0;
    OkButton.Visible := True;
    Result := ShowModal = mrOk;
    if Result then
    	Images.Assign(FImages);
  finally
    Free;
  end;
end;

procedure TTransparentImagesEditorForm.FormDestroy(Sender: TObject);
begin
  FImages.Free;
end;

procedure TTransparentImagesEditorForm.FormCreate(Sender: TObject);
begin
  FImages := TGlassImageList.Create(Self);
  FGrid := TImageListGrid.Create(Self);
  with FGrid do
  begin
    Parent := Frame;
    BorderStyle := bsNone;
    Align := alClient;
    Images := FImages;
    FGrid.OnMouseDown := ListBox1MouseDown;
    FGrid.OnMouseUp := ListBox1MouseUp;
    FGrid.OnSelection := ListBox1Selection;
    FGrid.DoubleBuffered := True;
  end;
end;

procedure TTransparentImagesEditorForm.ListBox1Selection(Sender: TObject; Col, Row: Integer;
  var Allow: Boolean);
var
  I: Integer;
begin
  if not Allow then
    Exit;
  if not FDragOrder then
    Exit;
  I := Col + Row * FGrid.ColCount;
  if FDragIndex = -1 then
    FDragIndex := I
  else if FDragIndex <> I then
  begin
    FImages.Images.Move(FDragIndex, I);
    FDragIndex := I;
  end;
end;

procedure TTransparentImagesEditorForm.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  C: TGridCoord;
begin
  if (Button = mbLeft) and (ssShift in Shift) then
  begin
    FDragOrder := True;
    C := FGrid.CoordFromPoint(X, Y);
    FDragIndex := C.X + C.Y * FGrid.ColCount;
    if FDragIndex > FImages.Count - 1 then
      FDragIndex := -1;
  end;
end;

procedure TTransparentImagesEditorForm.ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    FDragOrder := False;
end;

procedure TTransparentImagesEditorForm.ButtonsChange;
var
  B: Boolean;
begin
  B := FImages.Count > 0;
  SaveButton.Enabled := B;
  DeleteButton.Enabled := B;
  ResetButton.Enabled := B;
  CopyButton.Enabled := B;
end;

procedure TTransparentImagesEditorForm.OpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    FImages.Images.Add(OpenDialog.Files);
  ButtonsChange;
end;

procedure TTransparentImagesEditorForm.SaveClick(Sender: TObject);
begin
  if (FImages.Count > 0) and SaveDialog.Execute then
    FImages.Images.SaveToFile(SaveDialog.FileName);
end;

procedure TTransparentImagesEditorForm.ResetClick(Sender: TObject);
begin
  FImages.Images.Clear;
  ButtonsCHange;
end;

procedure TTransparentImagesEditorForm.DeleteClick(Sender: TObject);
var
  C: TGridCoord;
  I: Integer;
begin
  I := FGrid.Selection.X + FGrid.Selection.Y * FGrid.ColCount;
  if (I > -1) and (I < FImages.Count) then
  begin
    FImages.Images.Remove(I);
    I := FGrid.Selection.X + FGrid.Selection.Y * FGrid.ColCount;
    if (FImages.Count > 0) and (I > FImages.Count - 1) then
    begin
      I := FImages.Count - 1;
      C.X := I mod FGrid.ColCount;
      C.Y := I div FGrid.ColCount;
      FGrid.Selection := C;
    end;
  end;
  ButtonsChange;
end;

procedure TTransparentImagesEditorForm.DropFiles(Sender: TObject; Files: TStrings);
var
  I: Integer;
begin
  for I := Files.Count - 1 downto 0 do
    if UpperCase(ExtractFileExt(Files[I])) <> '.PNG' then
      Files.Delete(I);
  if Files.Count > 0 then
  begin
    FImages.Images.Add(Files);
    ButtonsChange;
  end;
end;

procedure TTransparentImagesEditorForm.FormShow(Sender: TObject);
begin
  if Tag = 0 then
  begin
    DropManager.RegisterControl(FGrid, DropFiles);
    Tag := 1;
  end;
end;

procedure TTransparentImagesEditorForm.CopyButtonClick(Sender: TObject);
begin
  ClipBoard.AsText := ComponentToString(FImages);
end;

procedure TTransparentImagesEditorForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

end.
