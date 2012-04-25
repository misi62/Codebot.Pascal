
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit ComponentBindingFrm;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ProviderTools;

type
  TComponentBindingForm = class(TForm)
    SourceList: TListBox;
    DestList: TListBox;
    AddButton: TButton;
    AddAllButton: TButton;
    RemoveAllButton: TButton;
    RemoveButton: TButton;
    CancelButton: TButton;
    OKButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure AddButtonClick(Sender: TObject);
    procedure AddAllButtonClick(Sender: TObject);
    procedure RemoveAllButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

procedure EditBindings(Binding: TComponentBinding);

implementation

{$R *.dfm}

procedure EditBindings(Binding: TComponentBinding);
var
  Form: TComponentBindingForm;
  Source, Dest: TStringList;
  O: TComponent;
  I: Integer;
begin
  Form := TComponentBindingForm.Create(Application);
  try
    O := Binding.Owner;
    Source := TStringList.Create;
    Dest := TStringList.Create;
    try
      Source.Sorted := True;
      Dest.Sorted := True;
      Source.BeginUpdate;
      Dest.BeginUpdate;
      for I := 0 to O.ComponentCount - 1 do
        if Binding.Accept(O.Components[I]) then
          if Binding.Contains(O.Components[I]) then
            Dest.Add(O.Components[I].Name)
          else
            Source.Add(O.Components[I].Name);
      Dest.EndUpdate;
      Source.EndUpdate;
      Form.DestList.Items := Dest;
      Form.SourceList.Items := Source;
    finally
      Dest.Free;
      Source.Free;
    end;
    I := Pos('Binding', Binding.ClassName);
    if I > 0 then
      Form.Caption := Copy(Binding.ClassName, 2, I - 2) + ' ' + Form.Caption;
    if Form.ShowModal = mrOK then
    begin
      Binding.Clear;
      Binding.BeginUpdate;
      try
        for I := 0 to Form.DestList.Items.Count - 1 do
          Binding.Add(O.FindComponent(Form.DestList.Items[I]));
      finally
        Binding.EndUpdate;
      end;
    end;
  finally
    Form.Free;
  end;
end;

procedure TComponentBindingForm.AddButtonClick(Sender: TObject);
var
  S: TStringList;
  I: Integer;
begin
  S := TStringList.Create;
  S.Sorted := True;
  S.Assign(DestList.Items);
  SourceList.Items.BeginUpdate;
  DestList.Items.BeginUpdate;
  S.BeginUpdate;
  for I := 0 to SourceList.Items.Count - 1 do
    if SourceList.Selected[I] then
      S.Add(SourceList.Items[I]);
  S.EndUpdate;
  DestList.Items := S;
  for I := SourceList.Items.Count - 1 downto 0 do
    if SourceList.Selected[I] then
      SourceList.Items.Delete(I);
  DestList.Items.EndUpdate;
  SourceList.Items.EndUpdate;
  S.Free;
end;

procedure TComponentBindingForm.AddAllButtonClick(Sender: TObject);
var
  S: TStringList;
  I: Integer;
begin
  S := TStringList.Create;
  S.Sorted := True;
  S.Assign(DestList.Items);
  SourceList.Items.BeginUpdate;
  DestList.Items.BeginUpdate;
  S.BeginUpdate;
  for I := 0 to SourceList.Items.Count - 1 do
    S.Add(SourceList.Items[I]);
  S.EndUpdate;
  DestList.Items := S;
  SourceList.Items.Clear;
  DestList.Items.EndUpdate;
  SourceList.Items.EndUpdate;
  S.Free;
end;

procedure TComponentBindingForm.RemoveAllButtonClick(Sender: TObject);
var
  S: TStringList;
  I: Integer;
begin
  S := TStringList.Create;
  S.Sorted := True;
  S.Assign(SourceList.Items);
  SourceList.Items.BeginUpdate;
  DestList.Items.BeginUpdate;
  S.BeginUpdate;
  for I := 0 to DestList.Items.Count - 1 do
    S.Add(DestList.Items[I]);
  S.EndUpdate;
  SourceList.Items := S;
  DestList.Items.Clear;
  DestList.Items.EndUpdate;
  SourceList.Items.EndUpdate;
  S.Free;
end;

procedure TComponentBindingForm.RemoveButtonClick(Sender: TObject);
var
  S: TStringList;
  I: Integer;
begin
  S := TStringList.Create;
  S.Sorted := True;
  S.Assign(SourceList.Items);
  SourceList.Items.BeginUpdate;
  DestList.Items.BeginUpdate;
  S.BeginUpdate;
  for I := 0 to DestList.Items.Count - 1 do
    if DestList.Selected[I] then
      S.Add(DestList.Items[I]);
  S.EndUpdate;
  SourceList.Items := S;
  for I := DestList.Items.Count - 1 downto 0 do
    if DestList.Selected[I] then
      DestList.Items.Delete(I);
  DestList.Items.EndUpdate;
  SourceList.Items.EndUpdate;
  S.Free;
end;

procedure TComponentBindingForm.FormCreate(Sender: TObject);
begin
  DesktopFont := True;
  ClientWidth := CancelButton.Left + CancelButton.Width + 8;
  ClientHeight := CancelButton.Top + CancelButton.Height + 8;
end;

end.
