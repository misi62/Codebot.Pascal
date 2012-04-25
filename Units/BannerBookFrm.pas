unit BannerBookFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BannerCtrls;

type
  TBannerBookForm = class(TForm)
    Pages: TMemo;
    OkButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
	end;

function EditBannerBook(BannerBook: TBannerBook): Boolean;

implementation

{$R *.dfm}

function EditBannerBook(BannerBook: TBannerBook): Boolean;
var
	Form: TBannerBookForm;
  S: string;
	I: Integer;
begin
	Form := TBannerBookForm.Create(Application);
  try
  	for I := 0 to BannerBook.Pages.Count - 1 do
    begin
			S := Trim(BannerBook.Pages[I]);
      if S <> '' then
      	Form.Pages.Lines.Add(S);
		end;
    Result := Form.ShowModal = mrOk;
    if Result then
    	BannerBook.Pages := Form.Pages.Lines;
  finally
  	Form.Free;
  end;
end;

procedure TBannerBookForm.FormCreate(Sender: TObject);
begin
	ClientHeight := Pages.Top + CancelButton.Top + CancelButton.Height;
	ClientWidth := Pages.Left + CancelButton.Left + CancelButton.Width;
end;

end.
