
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit CodebotShellReg;

interface

{$I CODEBOT.INC}

{$IFDEF D12}
  {$R D12BOTSHELL.DCR}
{$ENDIF}

uses
  Classes, ProviderTools, ShlCtrls, DsgnBinding,
  { design time units }
  DesignIntf;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Codebot Shell', [
    TShellBubbles, TShellEdit, TShellPathEditBar, TShellTree, TShellView,
    TSmallShellImages, TLargeShellImages, TShellBinding]);
  RegisterComponentEditor(TComponentBinding, TBindingEditor);
end;

end.
