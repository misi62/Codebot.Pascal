unit PhotoDlgs;

interface

uses
	Windows, Messages, SysUtils, Classes, Controls, Dialogs, Forms;

{ TPhotoCaptureDialog }

type
	TPhotoCaptureDialog = class(TCommonDialog)
  private
    FAuthor: string;
    FDirectory: string;
  	FPhotos: TStrings;
  public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property Photos: TStrings read FPhotos;
	published
    property Author: string read FAuthor write FAuthor;
    property Directory: string read FDirectory write FDirectory;
  end;

procedure InitCodeLib;
procedure FinishCodeLib;

implementation

{ Interface Module }

var
	codelib: HMODULE;

{ Imports }

  CodePhotoCapture: function(Wnd: HWND; Directory: PChar; PhotoBuffer: PChar; BufferSize: Integer): Boolean; stdcall;

procedure InitCodeLib;
begin
  if codelib = 0 then
  begin
    codelib := LoadLibrary('codelib.dll');
    if codelib <> 0 then
    begin
      @CodePhotoCapture := GetProcAddress(codelib, 'CodePhotoCapture');
    end;
  end;
end;

procedure FinishCodeLib;
begin
  if codelib <> 0 then
  begin
    @CodePhotoCapture := nil;
    FreeLibrary(codelib);
    codelib := 0;
  end;
end;

{ TPhotoCaptureDialog }


constructor TPhotoCaptureDialog.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FPhotos := TStringList.Create;
end;

destructor TPhotoCaptureDialog.Destroy;
begin
	FPhotos.Free;
  inherited Destroy;
end;

function TPhotoCaptureDialog.Execute: Boolean;
const
  BufferSize = 1024 * 5;
var
  Buffer: PChar;
begin
  FPhotos.Clear;
  if @CodePhotoCapture <> nil then
  begin
    GetMem(Buffer, BufferSize);
    try
      Result := CodePhotoCapture(Application.MainForm.Handle, PChar(Directory), Buffer, BufferSize);
      if Result then
        FPhotos.Text := StrPas(Buffer);
    finally
      FreeMem(Buffer);
    end;
  end
  else
    Result := False;
	{if Form = nil then
		Form := TCameraForm.Create(Application);
	Form.HandleNeeded;
	Form.Author := Author;
	Form.Directory := Directory;
	Result := Form.ShowModal = mrOk;
	if Result then
		FPhotos.Assign(Form.Photos)
	else
		FPhotos.Clear;}
end;

end.
