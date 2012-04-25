
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Version;

{$I CODEBOT.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, DialogsEx,
  FileTools;

type
  TVersionForm = class(TForm)
  protected
    function CanAbout: Boolean; virtual;
    procedure WMSysCommand(var Message: TMessage); message WM_SYSCOMMAND;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TVersionForm }

const
  ID_ABOUT  = Word(-1);

constructor TVersionForm.Create(AOwner: TComponent);
var
  SysMenu: HMENU;
begin
  inherited Create(AOwner);
  if CanAbout then
  begin
    SysMenu := GetSystemMenu(Handle, False);
    AppendMenu(SysMenu, MF_SEPARATOR, Word(-1), '');
    AppendMenu(SysMenu, MF_BYPOSITION, ID_ABOUT, 'About...');
  end;
end;

function TVersionForm.CanAbout: Boolean;
begin
  Result := Screen.FormCount = 1;
end;

function FileLastModified(const FileName: string; const Format: string = 'mm/dd/yyyy'): string;
var
  FileH: THandle;
  LocalFT: TFileTime;
  DosFT: DWORD;
  LastAccessedTime: TDateTime;
  FindData: TWin32FindData;
begin
  Result := '';
  FileH := FindFirstFile(PChar(FileName), FindData) ;
  if FileH <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(FileH);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
     FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFT);
     FileTimeToDosDateTime(LocalFT, LongRec(DosFT).Hi, LongRec(DosFT).Lo);
     LastAccessedTime := FileDateToDateTime(DosFT);
     Result := FormatDateTime(Format, LastAccessedTime);
    end;
  end;
end;

procedure TVersionForm.WMSysCommand(var Message: TMessage);
var
  V: TVersionInformation;
  Product, Version: string;
begin
  if Message.WParam = ID_ABOUT then
  begin
    V := TVersionInformation.Create;
    try
      V.FileName := ParamStr(0);
      Product := V.ProductName;
      Version := IntToStr(V.MajorVersion) + '.' + IntToStr(V.MinorVersion) + '.' +
        IntToStr(V.MajorBuild) + '.' + IntToStr(V.MinorBuild);
      MessageDlg('About', 'Product: ' + Product + #13#10 +
        'Version: ' + Version + #13#10#13#10 +
        'Last modified on ' + FileLastModified(V.FileName), mtInstall, [mbOk], 0);
    finally
      V.Free;
    end;
  end;
  inherited;
end;

end.
