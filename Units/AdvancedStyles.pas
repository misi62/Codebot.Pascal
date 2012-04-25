unit AdvancedStyles;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DesignCtrls, BtnCtrls;

type
  TAdvancedStylesForm = class(TForm)
    OffsetGroupBox: TGroupBox;
    XEdit: TEdit;
    XSpinner: TUpDown;
    Label15: TLabel;
    Label1: TLabel;
    YEdit: TEdit;
    YSpinner: TUpDown;
    LeftButton: TImageButton;
    CenterButton: TImageButton;
    RightButton: TImageButton;
    procedure FormCreate(Sender: TObject);
    procedure XSpinnerChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure YSpinnerChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure XEditExit(Sender: TObject);
    procedure YEditExit(Sender: TObject);
    procedure BandAlignmentClick(Sender: TObject);
  private
    FStylizer: TDesignStylizer;
    procedure SetStylizer(Value: TDesignStylizer);
  public
    property Stylizer: TDesignStylizer read FStylizer write SetStylizer;
  end;

implementation

{$R *.dfm}

procedure TAdvancedStylesForm.FormCreate(Sender: TObject);
begin
  DesktopFont := True;
  ClientHeight := OffsetGroupBox.Top * 2 + OffsetGroupBox.Height;
  ClientWidth := OffsetGroupBox.Left * 2 + OffsetGroupBox.Width;
end;

procedure TAdvancedStylesForm.SetStylizer(Value: TDesignStylizer);
begin
  FStylizer := Value;
  if FStylizer <> nil then
  begin
    XSpinner.Position := FStylizer.BandOffsetX;
    YSpinner.Position := FStylizer.BandOffsetY;
    case  FStylizer.BandAlignment of
      taLeftJustify: LeftButton.Down :=  True;
      taCenter: CenterButton.Down :=  True;
      taRightJustify: RightButton.Down :=  True;
    end;
  end;
end;

procedure TAdvancedStylesForm.XSpinnerChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  AllowChange := True;
  if FStylizer = nil then Exit;
  FStylizer.BandOffsetX := NewValue;
  FStylizer.RestoreState;
end;

procedure TAdvancedStylesForm.YSpinnerChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  AllowChange := True;
  if FStylizer = nil then Exit;
  FStylizer.BandOffsetY := NewValue;
  FStylizer.RestoreState;
end;

procedure TAdvancedStylesForm.XEditExit(Sender: TObject);
begin
  if FStylizer = nil then Exit;
  XSpinner.Position := StrToIntDef(XEdit.Text, XSpinner.Position);
  FStylizer.BandOffsetX := XSpinner.Position;
  FStylizer.Refresh;
end;

procedure TAdvancedStylesForm.YEditExit(Sender: TObject);
begin
  if FStylizer = nil then Exit;
  YSpinner.Position := StrToIntDef(YEdit.Text, YSpinner.Position);
  FStylizer.BandOffsetY := YSpinner.Position;
  FStylizer.Refresh;
end;

procedure TAdvancedStylesForm.BandAlignmentClick(Sender: TObject);
begin
  if FStylizer = nil then Exit;
  LeftButton.Down := LeftButton = Sender;
  CenterButton.Down := CenterButton = Sender;
  RightButton.Down := RightButton = Sender;
  FStylizer.BandAlignment := TAlignment(TComponent(Sender).Tag);
  FStylizer.Refresh;
end;

end.
