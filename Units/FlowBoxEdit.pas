unit FlowBoxEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grip, FlowBox, ImgList, ImgListEx, ProviderTools,
  BtnCtrls, SuplCtrls, ThemeCtrls;

type
  TFieldsEditorForm = class(TGripForm)
    HorizontalBar: THorizontalBar;
    SourceList: TListBox;
    DestList: TListBox;
    AllSourceButton: TImageButton;
    SomeSourceButton: TImageButton;
    SomeDestButton: TImageButton;
    AllDestButton: TImageButton;
    CancelButton: TButton;
    OkayButton: TButton;
    Images: TGlassImageList;
    SourceLabel: TLabel;
    DestLabel: TLabel;
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AllSourceButtonClick(Sender: TObject);
    procedure SomeSourceButtonClick(Sender: TObject);
    procedure SomeDestButtonClick(Sender: TObject);
    procedure AllDestButtonClick(Sender: TObject);
  private
    FFlowBox: TFlowBox;
    FMultiSelect: IMultiSelect;
    procedure SetFlowBox(Value: TFlowBox);
  public
    property FlowBox: TFlowBox read FFlowBox write SetFlowBox;
  end;

function EditFlowBox(Box: TFlowBox): Boolean;

implementation

{$R *.dfm}

function EditFlowBox(Box: TFlowBox): Boolean;
var
  F: TFieldsEditorForm;
begin
  F := TFieldsEditorForm.Create(Application);
  try
    F.FlowBox := Box;
    Result := F.ShowModal = mrOk;
  finally
    F.Free;
  end;
end;

procedure TFieldsEditorForm.FormResize(Sender: TObject);
var
  Y: Integer;
begin
  SourceList.Width := (ClientWidth - 72) div 2;
  DestList.Width := SourceList.Width ;
  DestList.Left := SourceList.Width + 64;
  DestLabel.Left := DestList.Left;
  Y := SourceList.Top + (SourceList.Height) div 2;
  AllSourceButton.Top := Y - AllSourceButton.Height * 2 - 12;
  AllSourceButton.Left := SourceList.Width + 20;
  SomeSourceButton.Top := Y - SomeSourceButton.Height - 4;
  SomeSourceButton.Left := AllSourceButton.Left;
  SomeDestButton.Top := Y + 4;
  SomeDestButton.Left := AllSourceButton.Left;
  AllDestButton.Top := Y + AllDestButton.Height + 12;
  AllDestButton.Left := AllSourceButton.Left;
end;

procedure TFieldsEditorForm.SetFlowBox(Value: TFlowBox);
var
  G: TFlowGroup;
  S: string;
  I: Integer;
begin
  FFlowBox := Value;
  for I := 0 to FFlowBox.Groups.Count - 1 do
  begin
    G := FFlowBox.Groups[I];
    S := Trim(G.Name);
    if S = '' then S := '(unnamed group)';
    if G.Visible then
      DestList.Items.AddObject(S, G)
    else
      SourceList.Items.AddObject(S, G);
  end;
  FMultiSelect := ThemeMultiSelect(SourceList, DestList) as IMultiSelect;
  (FMultiSelect as IDragControl).AllowDrag(DestList, True);
end;

procedure TFieldsEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  G: TFlowGroup;
  I, J: Integer;
begin
  if (FFlowBox <> nil) and (ModalResult = mrOk) then
  begin
    FFlowBox.Groups.BeginUpdate;
    try
      J := 0;
      for I := 0 to DestList.Count - 1 do
      begin
        G := TFlowGroup(DestList.Items.Objects[I]);
        G.Visible := True;
        G.Index := J;
        Inc(J);
      end;
      for I := 0 to SourceList.Count - 1 do
      begin
        G := TFlowGroup(SourceList.Items.Objects[I]);
        G.Visible := False;
        G.Index := J;
        Inc(J);
      end;
    finally
      FFlowBox.Groups.EndUpdate;
    end;
  end;
  FMultiSelect := nil;
end;

procedure TFieldsEditorForm.AllSourceButtonClick(Sender: TObject);
begin
  FMultiSelect.MoveSource(True);
end;

procedure TFieldsEditorForm.SomeSourceButtonClick(Sender: TObject);
begin
  FMultiSelect.MoveSource(False);
end;

procedure TFieldsEditorForm.SomeDestButtonClick(Sender: TObject);
begin
  FMultiSelect.MoveDest(False);
end;

procedure TFieldsEditorForm.AllDestButtonClick(Sender: TObject);
begin
  FMultiSelect.MoveDest(True);
end;

end.
