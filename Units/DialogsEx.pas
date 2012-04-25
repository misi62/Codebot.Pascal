
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit DialogsEx;

{$I CODEBOT.INC}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SuplCtrls, Consts, ExtCtrls, FormTools, GraphTools;

type
  TMessageForm = class(TForm)
    MessageBar: THorizontalBar;
    B4: TButton;
    B3: TButton;
    B2: TButton;
    B1: TButton;
    MessageLabel: TLabel;
    MessageIcon: TImage;
    InputEdit: TEdit;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
	private
  	Ratio: Double;
  end;

type
	TMsgDlgExType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom,
		mtCritical, mtCancel, mtHelp, mtSecurity, mtFind, mtSave, mtInstall,
		mtNetwork, mtPrinter, mtFavorite, mtSchedule, mtInput, mtTrash);

var
  MaxInputLength: Integer;

function MessageDlg(const Caption, Msg: string; DlgType: TMsgDlgExType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
function MessageDlg(const Msg: string; DlgType: TMsgDlgExType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;
procedure ShowMessage(const Msg: string);
function InputQuery(const ACaption, APrompt: string; DlgType: TMsgDlgExType; var Value: string): Boolean; overload;
function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean; overload;
procedure InputValidate(Control: TWinControl; Condition: Boolean; const Msg: string);

procedure UseDialogsEx(Value: Boolean);

implementation

{$R *.dfm}
{$R dlgicon.res}

procedure TMessageForm.FormCreate(Sender: TObject);
begin
  MessageBar.UseBackground := True
end;

const
  ButtonCaptions: array[TMsgDlgBtn] of string = (
    '&Yes', '&No', '&OK', '&Cancel', '&Abort',
    '&Retry', '&Ignore', '&All', 'N&o to All', 'Y&es to All',
    '&Help'{$IFDEF D11_UP}, '&Close'{$ENDIF});
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0{$IFDEF D11_UP}, mrClose{$ENDIF});
	IconNames: array[TMsgDlgExType] of string = ('Warning', 'Error', 'Information',
  	'Confirmation', 'Application', 'Critical', 'Cancel', 'Help',
		'Security', 'Find', 'Save', 'Install', 'Network', 'Printer',
		'Favourite', 'Schedule', 'Input', 'Trash');

type
	TApplicationException = class(TObject)
	public
		procedure HandleError(Sender: TObject; E: Exception);
  end;

procedure TApplicationException.HandleError(Sender: TObject; E: Exception);
begin
	MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure CustomHandleError(const Title, Msg: string);
begin
	MessageDlg(Title, Msg, mtError, [mbOK], 0);
end;

var
  Handler: TApplicationException;

procedure UseDialogsEx(Value: Boolean);
begin
	if Value then
  begin
    if Handler = nil then
    	Handler := TApplicationException.Create;
	  Application.OnException := Handler.HandleError;
    ErrorBoxProc := CustomHandleError;
  end
  else if not Value then
  begin
    ErrorBoxProc := nil;
	  Application.OnException := nil;
    FreeAndNil(Handler);
  end;
end;

function ConvertDlgType(DlgType: TMsgDlgExType): TMsgDlgType;
begin
	Result := Dialogs.mtCustom;
  case DlgType of
    mtWarning: Result := Dialogs.mtWarning;
    mtError: Result := Dialogs.mtError;
    mtInformation: Result := Dialogs.mtInformation;
    mtConfirmation: Result := Dialogs.mtConfirmation;
  end;
end;

function MessageDlg(const Caption, Msg: string; DlgType: TMsgDlgExType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; overload;

  procedure ResizeForm(Form: TMessageForm);
	const
		DialogOffset = 98;
	  MinWidth = 174;
	  MaxWidth = 500;
	var
		DC: HDC;
	  F: HFont;
		Strings: TStrings;
	  Size: TSize;
	  X, Y: Integer;
    B: TMsgDlgBtn;
	  S: string;
	begin
		DC := GetDC(Form.Handle);
	  F := SelectObject(DC, Form.Font.Handle);
		Strings := TStringList.Create;
	  try
		 	Strings.Text := Form.MessageLabel.Caption;
			Size := CalcCaptionSize(DC, ' ');
			X := Size.cx;
	    for Y := 0 to Strings.Count - 1 do
	    begin
	    	S := Strings[Y] + ' ';
			  Size := CalcCaptionSize(DC, S);
	      if Size.cx > X then
					X := Size.cx;
	    end;
	  finally
	  	Strings.Free;
	  end;
		X := X + Round(DialogOffset * Form.Ratio);
    Y := 8;
    for B := Low(B) to High(B) do
      if B in Buttons then
        Inc(Y, 75 + 8);
    Y := Round(Y * Form.Ratio);
	  if X < (MinWidth * Form.Ratio) then
	  	X := Round(MinWidth * Form.Ratio);
		if X > MaxWidth then
	  	X := MaxWidth;
    if Y > X then
      X := Y;
    Form.ClientWidth := X;
		X := Form.MessageLabel.Width;
		Y := CalcMemoHeight(DC, ' ' + Form.MessageLabel.Caption, X);
	  if Y > Size.cy then
	  	Size.cy := Y - Size.cy;
	  Form.ClientHeight := Size.cy + Round(DialogOffset * Form.Ratio);
	  SelectObject(DC, F);
	  ReleaseDC(Form.Handle, DC);
    with Form do
	    if not B2.Visible then
  	  	B1.Left := (ClientWidth - B1.Width) shr 1;
	end;

var
	Form: TMessageForm;
  Button: TButton;
  Bitmap: TAlphaImage;
  B: TMsgDlgBtn;
  I: Integer;
begin
	if Handler = nil then
  begin
  	Result := Dialogs.MessageDlg(Msg, ConvertDlgType(DlgType), Buttons, HelpCtx);
    Exit;
  end;
	if Buttons = [] then
  begin
  	Result := mrNone;
    Exit;
  end;
	Form := TMessageForm.Create(Application);
  try
  	Form.DesktopFont := true;
		Form.Caption := Caption;
    Form.MessageLabel.Caption := Msg;
    I := 0;
    for B := Low(B) to High(B) do
    	if B in Buttons then
      	Inc(I);
		if I > 4 then
    	I := 4;
    for B := Low(B) to High(B) do
    	if B in Buttons then
      begin
      	case	I of
        	1: Button := Form.B1;
          2: Button := Form.B2;
          3: Button := Form.B3;
          4: Button := Form.B4;
				else
        	Button := nil;
        end;
        Button.Visible := True;
        Button.Caption := ButtonCaptions[B];
        Button.ModalResult := ModalResults[B];
       	Dec(I);
        if I = 0 then Break;
      end;
		Form.Ratio := AdjustFontBorders(Form, Form.B1);
    with Form do
    begin
			MessageLabel.Anchors := [akLeft, akTop, akRight, akBottom];
      MessageBar.Anchors := [akLeft, akTop, akRight, akBottom];
      B1.Anchors := [akRight, akBottom];
      B2.Anchors := [akRight, akBottom];
      B3.Anchors := [akRight, akBottom];
      B4.Anchors := [akRight, akBottom];
    end;
    Bitmap := TAlphaImage.Create;
    try
    	Bitmap.LoadFromResourceName( 'Dlg' + IconNames[DlgType]);
      Form.MessageIcon.Picture.Assign(Bitmap);
    finally
    	Bitmap.Free;
    end;
    ResizeForm(Form);
		Result := Form.ShowModal;
  finally
		Form.Free;
  end;
end;

function MessageDlg(const Msg: string; DlgType: TMsgDlgExType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
const
  Captions: array[TMsgDlgExType] of string = ('Warning', 'Error',
    'Information', 'Confirm', '', 'Critical', 'Confirm', 'Help',
		'Security', 'Find', 'Save', 'Install', 'Network', 'Printer',
		'Favourite', 'Schedule', 'Input', 'Trash');
var
	S: string;
begin
 	if DlgType = mtCustom then
   	S := Application.Title
  else
  	S := Captions[DlgType];
  Result := MessageDlg(S, Msg, DlgType, Buttons, HelpCtx);
end;

procedure ShowMessage(const Msg: string);
begin
	MessageDlg(Msg, mtInformation, [mbOk], 0);
end;

function InputQuery(const ACaption, APrompt: string; DlgType: TMsgDlgExType; var Value: string): Boolean; overload;

  procedure ResizeForm(Form: TMessageForm);
	const
		DialogOffset = 98;
	  MinWidth = 300;
	  MaxWidth = 500;
	var
		DC: HDC;
	  F: HFont;
		Strings: TStrings;
	  Size: TSize;
	  X, Y: Integer;
	  S: string;
	begin
		DC := GetDC(Form.Handle);
	  F := SelectObject(DC, Form.Font.Handle);
		Strings := TStringList.Create;
	  try
		 	Strings.Text := Form.MessageLabel.Caption;
			Size := CalcCaptionSize(DC, ' ');
			X := Size.cx;
	    for Y := 0 to Strings.Count - 1 do
	    begin
	    	S := Strings[Y] + ' ';
			  Size := CalcCaptionSize(DC, S);
	      if Size.cx > X then
					X := Size.cx;
	    end;
	  finally
	  	Strings.Free;
	  end;
		X := Size.cx + Round(DialogOffset * Form.Ratio);
	  if X < (MinWidth * Form.Ratio) then
	  	X := Round(MinWidth * Form.Ratio);
		if X > MaxWidth then
	  	Form.ClientWidth := MaxWidth
	  else
	  	Form.ClientWidth := X;
    Form.MessageBar.Width := Form.ClientWidth;
		X := Form.MessageLabel.Width;
		Y := CalcMemoHeight(DC, ' ' + Form.MessageLabel.Caption, X);
	  if Y > Size.cy then
	  	Size.cy := Y - Size.cy;
	  Form.ClientHeight := Size.cy + Round(DialogOffset * Form.Ratio);
	  SelectObject(DC, F);
	  ReleaseDC(Form.Handle, DC);
    with Form do
	    if not B2.Visible then
  	  	B1.Left := (ClientWidth - B1.Width) shr 1;
		Form.MessageBar.Height := Round(Form.B1.Top - Form.Ratio * 8);
    Form.InputEdit.Width := Form.B1.Left + Form.B1.Width - Form.InputEdit.Left;
    Form.InputEdit.Top := Round(Form.InputEdit.Top - Form.Ratio * 8);
    Form.MessageLabel.Top := Round(Form.MessageLabel.Top - Form.Ratio * 8);
	end;

var
	Form: TMessageForm;
  Bitmap: TAlphaImage;
begin
	Form := TMessageForm.Create(Application);
  with Form do
  try
  	DesktopFont := True;
  	Caption := ACaption;
    MessageLabel.Caption := APrompt;
    InputEdit.Text := Value;
    Bitmap := TAlphaImage.Create;
    try
    	Bitmap.LoadFromResourceName('Dlg' + IconNames[DlgType]);
      MessageIcon.Picture.Assign(Bitmap);
    finally
    	Bitmap.Free;
    end;
    InputEdit.Visible := True;
  	B1.ModalResult := mrCancel;
  	B1.Caption := ButtonCaptions[mbCancel];
		B1.Visible := True;
		B2.ModalResult := mrOk;
		B2.Caption := ButtonCaptions[mbOk];
		B2.Visible := True;
    B2.Default := True;
		Ratio := AdjustFontBorders(Form, B1);
		B1.Anchors := [akRight, akBottom];
		B2.Anchors := [akRight, akBottom];
    ResizeForm(Form);
    Form.InputEdit.MaxLength := MaxInputLength;
		Result := ShowModal = mrOk;
		if Result then
    begin
			Value := Trim(InputEdit.Text);
      Result := Value <> '';
    end;
  finally
  	Free;
  end;
end;

function InputQuery(const ACaption, APrompt: string; var Value: string): Boolean;
begin
	Result := InputQuery(ACaption, APrompt, mtInput, Value);
end;

procedure InputValidate(Control: TWinControl; Condition: Boolean; const Msg: string);
begin
  if Control.Enabled and (not Condition) then
  begin
    MessageDlg('Input Error', Msg, mtError, [mbOk], 0);
    Control.SetFocus;
    Abort;
  end;
end;

procedure TMessageForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
  	ModalResult := mrCancel
	else if Key = VK_RETURN then
  	if B4.Default then B4.Click
  	else if B3.Default then B3.Click
  	else if B2.Default then B2.Click
  	else if B1.Default then B1.Click;
end;

initialization
	Handler := nil;
finalization
	Handler.Free;
end.
