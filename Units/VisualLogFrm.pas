unit VisualLogFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShlTools, FormTools, ShlCtrls, ProviderTools;

type
  TVisualLogFrame = class(TFrame)
    LogFilesLabel: TLabel;
    BackButton: TButton;
    LogList: TShellView;
  private
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

{ TVisualLogFrame }

constructor TVisualLogFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := BackButton.Left + BackButton.Width;
  Height := BackButton.Top + BackButton.Height;
  LogList.Anchors := [akLeft, akTop, akRight, akBottom];
  BackButton.Anchors := [akRight, akBottom];
  BackButton.Anchors := [akRight, akBottom];
end;

procedure TVisualLogFrame.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  FillRect(Msg.DC, ClientRect, COLOR_BTNFACE + 1);
  Msg.Result := 1;
end;

end.
