unit DebugNotes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TDebugForm = class(TForm)
    Memo: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

procedure DebugNote(const S: string);

implementation

{$R *.dfm}

var
  DebugForm: TDebugForm;
  Destroyed: Boolean;
  Entered: Boolean;

procedure DebugNote(const S: string);
var
  F: TCustomForm;
begin
  if Entered then Exit;
  try
    Entered := True;
    if Destroyed then Exit;
    if Application.MainForm = nil then Exit;
    if DebugForm = nil then
      Application.CreateForm(TDebugForm, DebugForm);
    if not DebugForm.Visible then
    begin
      F := Screen.ActiveCustomForm;
      DebugForm.Show;
      if F <> nil then
        F.Show;
    end;
    if S = '' then
      DebugForm.Memo.Lines.Clear
    else
      DebugForm.Memo.Lines.Add(S);
    SendMessage(DebugForm.Memo.Handle, EM_LINESCROLL, 0, High(Word));
  finally
    Entered := False;
  end;
end;

procedure TDebugForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TDebugForm.FormDestroy(Sender: TObject);
begin
  Destroyed := True;
end;

procedure TDebugForm.FormCreate(Sender: TObject);
begin
  ClientWidth := Memo.Width + Memo.Left * 2;
  ClientHeight := Memo.Height + Memo.Top * 2;
  Memo.Anchors := [akLeft, akTop, akRight, akBottom];
end;

end.
