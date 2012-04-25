
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit CodebotDesign;

interface

{$I CODEBOT.INC}

uses
  Classes, ProviderTools, ComponentBindingFrm,
  { design time units }
  DesignEditors;

type
  TBindingEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

procedure TBindingEditor.Edit;
begin
  if Component is TComponentBinding then
    EditBindings(Component as TComponentBinding);
end;

procedure TBindingEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TBindingEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Bindings...';
end;

function TBindingEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
