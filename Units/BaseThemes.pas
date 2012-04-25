
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BaseThemes;

interface

{$I CODEBOT.INC}

uses
  Classes;

type
  TThemeComponent = class(TComponent)
  protected
    procedure RemoveEvents; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

procedure ClearTheme(Owner: TComponent; Theme: TThemeComponent);
var
  C: TComponent;
  I: Integer;
begin
  for I := Owner.ComponentCount - 1 downto 0 do
  begin
    C := Owner.Components[I];
    if C = Theme then Continue;
    if C is TThemeComponent then
    begin
      TThemeComponent(C).RemoveEvents;
      C.Free;
    end;
  end;
end;

constructor TThemeComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ClearTheme(AOwner, Self);
end;

destructor TThemeComponent.Destroy;
begin
  RemoveEvents;
  inherited Destroy;
end;

procedure TThemeComponent.RemoveEvents;
begin
end;

end.
