
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit FolderBarsFrm;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, BtnEdit, FolderCtrls, ImgList, GraphTools, FormTools,
  ShlCtrls, BaseTypes, ProviderTools;

{ TFolderBarsForm }

type
  TFolderBarsForm = class(TForm)
    GroupBox: TGroupBox;
    CaptionEdit: TEdit;
    CaptionLabel: TLabel;
    OKButtopn: TButton;
    CancelButton: TButton;
    NewItemButton: TButton;
    RemoveButton: TButton;
    IndexLabel: TLabel;
    IndexEdit: TEdit;
    IndexSpin: TUpDown;
    VisibleBox: TCheckBox;
    EnabledBox: TCheckBox;
    SelectedIndexLabel: TLabel;
    ImageIndexLabel: TLabel;
    NewBarButton: TButton;
    ListBox: TListBox;
    SelectedIndexEdit: TImageListEdit;
    ImageIndexEdit: TImageListEdit;
    procedure FormCreate(Sender: TObject);
    procedure NewItemButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure IndexSpinChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure NewBarButtonClick(Sender: TObject);
    procedure CaptionEditChange(Sender: TObject);
    procedure EnabledBoxClick(Sender: TObject);
    procedure VisibleBoxClick(Sender: TObject);
    procedure ImageIndexSpinChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure SelectedIndexSpinChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure ImageIndexEditChange(Sender: TObject);
    procedure SelectedIndexEditChange(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure ListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBoxMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  private
  	FCurrent: TObject;
    FFolderBars: TFolderBars;
    FFolderImages: TCustomImageList;
    FItemImages: TCustomImageList;
    procedure SetFolderBars(Value: TFolderBars);
    function GetCurrentFolder: TFolderBar;
    function GetCurrentItem: TFolderItem;
    procedure RecreateList;
  public
    property FolderBars: TFolderBars read FFolderBars write SetFolderBars;
    property CurrentFolder: TFolderBar read GetCurrentFolder;
    property CurrentItem: TFolderItem read GetCurrentItem;
  end;

function EditFolderBars(ABars: TFolderBars): Boolean;

implementation

{$R *.DFM}

{ TFolderBarsForm }

procedure TFolderBarsForm.FormCreate(Sender: TObject);
begin
  ClientHeight := CancelButton.Top + 32;
  ClientWidth := ListBox.Left + CancelButton.Left + CancelButton.Width;
  FFolderBars := TFolderBars.Create(Self);
end;

procedure TFolderBarsForm.RecreateList;
var
	FolderBar: TFolderBar;
	Item: TFolderItem;
  I, J: Integer;
begin
    with ListBox.Items do
    begin
      BeginUpdate;
      Clear;
      for I := 0 to FFolderBars.Count - 1 do
      begin
      	FolderBar := FFolderBars[I];
        FCurrent := FolderBar;
        AddObject(FolderBar.Caption, FolderBar);
        for J := 0 to FFolderBars.Items[I].Items.Count - 1 do
        begin
	        Item := FFolderBars.Items[I].Items[J];
	        FCurrent := Item;
          AddObject(Item.Caption, Item);
        end;
      end;
      EndUpdate;
    end;
end;

procedure TFolderBarsForm.SetFolderBars(Value: TFolderBars);
begin
  if Value <> nil then
  begin
  	if Value.Control <> nil then
    begin
    	FFolderImages := TCustomImageList(
      	Value.Control.Perform(CM_FOLDERIMAGES, 0, 0));
  		FItemImages := TCustomImageList(
      	Value.Control.Perform(CM_ITEMIMAGES, 0, 0));
    end;
    FFolderBars.Assign(Value);
    RecreateList;

			if ListBox.Items.Count > 0 then
      begin
      	ListBox.ItemIndex := 0;
    		ListBox.OnClick(ListBox);
			end;
  end;
end;

procedure TFolderBarsForm.IndexSpinChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
var
	FolderBar: TFolderBar;
  Item: TFolderItem;
begin
	AllowChange := False;
  if CurrentFolder <> nil then
  begin
  	FolderBar := CurrentFolder;
    AllowChange := (NewValue > -1) and (NewValue < FolderBar.Collection.Count);
    if AllowChange then
    begin
			FolderBar.Index := NewValue;
			RecreateList;
	    with ListBox, Items do
      begin
    		ItemIndex := -1;
        ItemIndex := IndexOfObject(FolderBar);
			end;
		end;
  end
  else if CurrentItem <> nil then
  begin
  	Item := CurrentItem;
    AllowChange := (NewValue > -1) and (NewValue < Item.Collection.Count);
    if AllowChange then
    begin
			Item.Index := NewValue;
			RecreateList;
	    with ListBox, Items do
      begin
    		ItemIndex := -1;
        ItemIndex := IndexOfObject(Item);
			end;
		end;
  end;
end;

function EditFolderBars(ABars: TFolderBars): Boolean;
begin
	with TFolderBarsForm.Create(Application) do
	try
  	FolderBars := ABars;
    Result := ShowModal = mrOK;
    if Result then
      ABars.Assign(FolderBars);
  finally
  	Free;
  end;
end;


procedure TFolderBarsForm.ListBoxClick(Sender: TObject);
var
	FolderBar: TFolderBar;
  Item: TFolderItem;
begin
	IndexSpin.OnChangingEx := nil;
  IndexEdit.OnChange := nil;
  ImageIndexEdit.OnChange := nil;
  SelectedIndexEdit.OnChange := nil;
	if CurrentFolder <> nil then
  begin
  	FolderBar := CurrentFolder;
    GroupBox.Caption := 'Folder Bar:';
    CaptionEdit.Text := FolderBar.Caption;
    EnabledBox.Checked := True;
    EnabledBox.Enabled := False;
    VisibleBox.Checked := FolderBar.Visible;
    ImageIndexEdit.Images := FFolderImages;
    ImageIndexEdit.ImageIndex := FolderBar.ImageIndex;
    SelectedIndexLabel.Enabled := True;
    SelectedIndexEdit.Enabled := True;
    SelectedIndexEdit.Images := FFolderImages;
    SelectedIndexEdit.ImageIndex := FolderBar.SelectedIndex;
    IndexSpin.Position := FolderBar.Index;
    RemoveButton.Enabled := True;
    IndexSpin.OnChangingEx := IndexSpinChangingEx;
  end
  else if CurrentItem <> nil then
  begin
  	Item := CurrentItem;
    GroupBox.Caption := 'Folder Item:';
    CaptionEdit.Text := Item.Caption;
    EnabledBox.Checked := Item.Enabled;
    EnabledBox.Enabled := True;
    VisibleBox.Checked := Item.Visible;
    ImageIndexEdit.Images := FItemImages;
    ImageIndexEdit.ImageIndex := Item.ImageIndex;
    IndexSpin.Position := Item.Index;
    SelectedIndexEdit.Images := nil;
    SelectedIndexEdit.ImageIndex := -1;
    SelectedIndexLabel.Enabled := False;
    SelectedIndexEdit.Enabled := False;
    RemoveButton.Enabled := True;
    IndexSpin.OnChangingEx := IndexSpinChangingEx;
  end
  else
  begin
    IndexSpin.OnChangingEx := nil;
    CaptionEdit.Text := '';
    EnabledBox.Checked := True;
    EnabledBox.Enabled := True;
    VisibleBox.Checked := True;
    ImageIndexEdit.Images := nil;
    ImageIndexEdit.ImageIndex := -1;
    SelectedIndexLabel.Enabled := True;
    SelectedIndexEdit.Enabled := True;
    SelectedIndexEdit.Images := nil;
    SelectedIndexEdit.ImageIndex := -1;
    NewItemButton.Enabled := False;
    RemoveButton.Enabled := False;
    IndexSpin.Position := 0;
  end;
  IndexSpin.OnChangingEx := IndexSpinChangingEx;
  ImageIndexEdit.OnChange := ImageIndexEditChange;
  SelectedIndexEdit.OnChange := SelectedIndexEditChange;
end;

procedure TFolderBarsForm.NewBarButtonClick(Sender: TObject);
var
	FolderBar: TFolderBar;
begin
	FolderBar := FFolderBars.Add;
  FolderBar.Caption := 'New Folder Bar';
  FCurrent := FolderBar;
  ListBox.Items.AddObject(FolderBar.Caption, FolderBar);
  ListBox.ItemIndex := ListBox.Items.Count - 1;
  ListBox.OnClick(ListBox);
end;

procedure TFolderBarsForm.NewItemButtonClick(Sender: TObject);
var
	FolderBar: TFolderBar;
  Item: TFolderItem;
  I: Integer;
begin
	if CurrentFolder <> nil then
  	FolderBar := CurrentFolder
	else if CurrentItem <> nil then
  	FolderBar := TFolderItems(CurrentItem.Collection).Folder
	else
  	Exit;
	Item := FolderBar.Items.Add;
  Item.Caption := 'New Folder Item';
  with ListBox, Items do
  begin
		I := ItemIndex + 1;
	  while (I < Count) and (Objects[I] is TFolderItem) do
    	Inc(I);
	  FCurrent := Item;
		if I = Count then
  		AddObject(Item.Caption, Item)
		else
    	InsertObject(I, Item.Caption, Item);
	  ItemIndex := I;
	  OnClick(ListBox);
	end;
end;

procedure TFolderBarsForm.RemoveButtonClick(Sender: TObject);
var
	FolderBar: TFolderBar;
  Item: TFolderItem;
	I: Integer;
begin
  if CurrentFolder <> nil then
	begin
  	FolderBar := CurrentFolder;
    with ListBox, Items do
    begin
	  	I := ItemIndex;
	    Delete(I);
			while (I < Count) and (Objects[I] is TFolderItem) do
		    Delete(I);
	    if I = Count then
      	Dec(I);
			if I > -1 then
      	ItemIndex := I;
		end;
		FolderBar.Free;
  end
  else if CurrentItem <> nil then
  begin
  	Item := CurrentItem;
    with ListBox, Items do
    begin
	  	I := ItemIndex;
	    Delete(I);
	    if I = Count then
      	Dec(I);
			if I > -1 then
      	ItemIndex := I;
		end;
		Item.Free;
  end;
	ListBox.OnClick(ListBox);
  RemoveButton.Enabled := (CurrentFolder <> nil) or (CurrentItem <> nil);
end;

procedure TFolderBarsForm.CaptionEditChange(Sender: TObject);
begin
	if CurrentFolder <> nil then
  begin
  	CurrentFolder.Caption := CaptionEdit.Text;
    ListBox.Repaint;
	end
  else if CurrentItem <> nil then
  begin
  	CurrentItem.Caption := CaptionEdit.Text;
    ListBox.Repaint;
  end;
end;

procedure TFolderBarsForm.EnabledBoxClick(Sender: TObject);
begin
  if CurrentItem <> nil then
  begin
    CurrentItem.Enabled := EnabledBox.Checked;
		ListBox.Repaint;
	end;
end;

procedure TFolderBarsForm.VisibleBoxClick(Sender: TObject);
begin
  if CurrentFolder <> nil then
    CurrentFolder.Visible := VisibleBox.Checked
  else if CurrentItem <> nil then
    CurrentItem.Visible := VisibleBox.Checked;
end;

procedure TFolderBarsForm.ImageIndexSpinChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  if CurrentFolder <> nil then
    CurrentFolder.ImageIndex := NewValue
  else if CurrentItem <> nil then
    CurrentItem.ImageIndex := NewValue;
	ListBox.Repaint;
  AllowChange := True;
end;

procedure TFolderBarsForm.SelectedIndexSpinChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  if CurrentFolder <> nil then
	begin
    CurrentFolder.SelectedIndex := NewValue;
    ListBox.Repaint;
	end;
  AllowChange := True;
end;

procedure TFolderBarsForm.ImageIndexEditChange(Sender: TObject);
begin
  if CurrentFolder <> nil then
    CurrentFolder.ImageIndex := ImageIndexEdit.ImageIndex
  else if CurrentItem <> nil then
    CurrentItem.ImageIndex := ImageIndexEdit.ImageIndex;
	ListBox.Repaint;
end;

procedure TFolderBarsForm.SelectedIndexEditChange(Sender: TObject);
begin
  if CurrentFolder <> nil then
  begin
    CurrentFolder.SelectedIndex := SelectedIndexEdit.ImageIndex;
    ImageIndexEdit.ImageIndex := SelectedIndexEdit.ImageIndex;
	end;
	ListBox.Repaint;
end;

function TFolderBarsForm.GetCurrentFolder: TFolderBar;
begin
	with ListBox, Items do
		if (ItemIndex > -1) and (Objects[ItemIndex] is TFolderBar) then
    	Result := TFolderBar(Objects[ItemIndex])
		else
    	Result := nil;
end;

function TFolderBarsForm.GetCurrentItem: TFolderItem;
begin
	with ListBox, Items do
		if (ItemIndex > -1) and (Objects[ItemIndex] is TFolderItem) then
    	Result := TFolderItem(Objects[ItemIndex])
		else
    	Result := nil;
end;

procedure TFolderBarsForm.ListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Images: TCustomImageList;
  Hot: Boolean;
  DC: HDC;
  R: TRect;
  S: string;
  I: Integer;
begin
	with ListBox, Items do
  begin
    Hot := odSelected in State;
  	DC := Canvas.Handle;
    R := Rect;
    if Hot then
    begin
	    SetTextColor(DC, ColorToRGB(clHighlightText));
			FillRectColor(DC, Rect, clHighlight);
    end
    else
    begin
	    SetTextColor(DC, ColorToRGB(clWindowText));
			FillRectColor(DC, Rect, clWindow);
    end;
		Inc(Rect.Left, 5);
  	if Objects[Index] is TFolderBar then
    begin
    	Images := FFolderImages;
      S := TFolderBar(Objects[Index]).Caption;
      if Hot then
	      I := TFolderBar(Objects[Index]).SelectedIndex
			else
	      I := TFolderBar(Objects[Index]).ImageIndex;
		end
		else
    begin
    	Images := FItemImages;
      S := TFolderItem(Objects[Index]).Caption;
			I := TFolderItem(Objects[Index]).ImageIndex;
			Inc(Rect.Left, 10);
		end;
		if Images <> nil then
    begin
    	if I > -1 then
        ImageListDraw(Images, Canvas, Rect.Left, Rect.Top + 5, I);
			Inc(Rect.Left, Images.Width + 5);
    end;
    DrawCaption(DC, S, Rect, drLeft);
    SelectClipRect(DC, R, RGN_DIFF);
	end;
end;

procedure TFolderBarsForm.ListBoxMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
var
  Images: TCustomImageList;
begin
	with ListBox, Items do
  begin
  	if FCurrent is TFolderBar then
    	Images := FFolderImages
		else
    	Images := FItemImages;
  end;
		if Images <> nil then
     Height := Images.Height + 10
		else
    	Height := ListBox.Canvas.TextHeight('Wg')
end;

end.

