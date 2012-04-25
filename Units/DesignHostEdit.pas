unit DesignHostEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PngImage, ExtCtrls, Grip, SuplCtrls, DesignCtrls;

type
  TDesignHostEditorForm = class(TGripForm)
    Image1: TImage;
    CaptionEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    HelpMemo: TMemo;
    Label4: TLabel;
    Label5: TLabel;
    OkButton: TButton;
    CancelButton: TButton;
    KindLabel: TLabel;
    DataBindingLabel: TLabel;
    HorizontalBar1: THorizontalBar;
    HorizontalBar2: THorizontalBar;
  private
    FHost: TDesignHost;
    procedure SetHost(Value: TDesignHost);
  public
    property Host: TDesignHost read FHost write SetHost;
  end;

function EditDesignHost(Host: TDesignHost): Boolean;

implementation

{$R *.dfm}

function EditDesignHost(Host: TDesignHost): Boolean;
var
  Form: TDesignHostEditorForm;
begin
  Form := TDesignHostEditorForm.Create(Application);
  try
    Form.Host := Host;
    Result := Form.ShowModal = mrOk;
    if Result then
    begin
      Host.Caption := Form.CaptionEdit.Text;
      Host.Hint := Form.HelpMemo.Text;
    end;
  finally
    Form.Free;
  end;
end;

{ TDesignHostEditorForm }

procedure TDesignHostEditorForm.SetHost(Value: TDesignHost);
begin
  FHost := Value;
  KindLabel.Caption := ElementToStr(FHost.ControlKind, True);
  DataBindingLabel.Caption := FHost.DataBinding;
  if DataBindingLabel.Caption = '' then
    DataBindingLabel.Caption := SNoBinding;
  CaptionEdit.Text := FHost.Caption;
  HelpMemo.Text := FHost.Hint;
end;

initialization
  DefaultHostEditorProc := EditDesignHost;
end.
