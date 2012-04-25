
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit NamedStringsEditorFrm;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, StdCtrls, Grip,
  SuplCtrls, StrCollect;

type
  TNamedStringsEditorForm = class(TGripForm)
    BodyMemo: TMemo;
    ItemListBox: TListBox;
    OkButton: TButton;
    CancelButton: TButton;
    AddButton: TButton;
    RemoveButton: TButton;
    ItemEdit: TEdit;
    ItemLabel: TLabel;
    BodyLabel: TLabel;
    HorizontalBar: THorizontalBar;
    MoveUpButton: TButton;
    MoveDownButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ItemListBoxClick(Sender: TObject);
    procedure BodyMemoExit(Sender: TObject);
    procedure ItemEditChange(Sender: TObject);
    procedure MoveButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
  private
    FNamedStrings: TNamedStrings;
    FNewCounter: Integer;
    procedure SetNamedStrings(Value: TNamedStrings);
  public
    property NamedStrings: TNamedStrings read FNamedStrings write SetNamedStrings;
  end;

function EditNamedStrings(NamedStrings: TNamedStrings): Boolean;

implementation

{$R *.dfm}

function EditNamedStrings(NamedStrings: TNamedStrings): Boolean;
var
  F: TNamedStringsEditorForm;
begin
  F := TNamedStringsEditorForm.Create(Application);
  try
    F.NamedStrings := NamedStrings;
    Result := F.ShowModal = mrOk;
    if Result then
      NamedStrings.Assign(F.FNamedStrings);
  finally
    F.Free;
  end;
end;

{ TNamedStringsForm }

procedure TNamedStringsEditorForm.SetNamedStrings(Value: TNamedStrings);
var
  I: Integer;
begin
  FNamedStrings.Assign(Value);;
  ItemListBox.Items.BeginUpdate;
  try
    for I := 0 to FNamedStrings.Count - 1 do
      ItemListBox.Items.Add(FNamedStrings.Item[I])
  finally
    ItemListBox.Items.EndUpdate;
  end;
  if FNamedStrings.Count > 0 then
    ItemListBox.ItemIndex := 0;
  ItemListBox.OnClick(ItemListBox);
end;

procedure TNamedStringsEditorForm.FormCreate(Sender: TObject);
begin
  FNamedStrings := TNamedStrings.Create;
  Sizeable := False;
end;

procedure TNamedStringsEditorForm.FormDestroy(Sender: TObject);
begin
  FNamedStrings.Free;
end;

procedure TNamedStringsEditorForm.ItemListBoxClick(Sender: TObject);
var
  I: Integer;
begin
  I := ItemListBox.ItemIndex;
  ItemEdit.OnChange := nil;
  if I > -1 then
  begin
    ItemEdit.Text := FNamedStrings.Item[I];
    BodyMemo.Text := FNamedStrings.Body[I];
  end
  else
  begin
    ItemEdit.Text := '';
    BodyMemo.Text := '';
  end;
  ItemEdit.ReadOnly := FNamedStrings.Count = 0;
  BodyMemo.ReadOnly := FNamedStrings.Count = 0;
  RemoveButton.Enabled := FNamedStrings.Count > 0;
  MoveUpButton.Enabled := FNamedStrings.Count > 0;
  MoveDownButton.Enabled := FNamedStrings.Count > 0;
  ItemEdit.OnChange := ItemEditChange;
end;

procedure TNamedStringsEditorForm.BodyMemoExit(Sender: TObject);
var
  I: Integer;
begin
  I := ItemListBox.ItemIndex;
  if I > -1 then
    FNamedStrings.Body[I] := BodyMemo.Text;
end;

procedure TNamedStringsEditorForm.ItemEditChange(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  I := ItemListBox.ItemIndex;
  if I > -1 then
  begin
    FNamedStrings.Item[I] := ItemEdit.Text;
    ItemListBox.Items[I] := FNamedStrings.Item[I];
  end;
end;

procedure TNamedStringsEditorForm.MoveButtonClick(Sender: TObject);
var
  I, N: Integer;
  S: string;
begin
  I := ItemListBox.ItemIndex;
  if I > -1 then
  begin
    N := I + (Sender as TComponent).Tag;
    if (N < 0) or (N = FNamedStrings.Count) then Exit;
    FNamedStrings.Move(I, N);
    ItemListBox.Items.Move(I, N);
    ItemListBox.ItemIndex := N;
  end;
end;

procedure TNamedStringsEditorForm.RemoveButtonClick(Sender: TObject);
var
  I: Integer;
begin
  I := ItemListBox.ItemIndex;
  if I > -1 then
  begin
    FNamedStrings.Remove(FNamedStrings.Item[I]);
    ItemListBox.Items.Delete(I);
    if I = FNamedStrings.Count then
      Dec(I);
    ItemListBox.ItemIndex := I;
    ItemListBox.OnClick(ItemListBox);
  end;
end;

procedure TNamedStringsEditorForm.AddButtonClick(Sender: TObject);
var
  N, S: string;
begin
  repeat
    Inc(FNewCounter);
    N := 'new item ' + IntToStr(FNewCounter);
  until not FNamedStrings.Find(N, S);
  FNamedStrings.Add(N, '');
  ItemListBox.Items.Add(N);
  ItemListBox.ItemIndex := FNamedStrings.Count - 1;
  ItemListBox.OnClick(ItemListBox);
  ItemEdit.SetFocus;
  ItemEdit.SelectAll;
end;

end.
