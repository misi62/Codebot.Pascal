
(********************************************************)
(*                                                      *)
(*  Codebot Platform Specific Implementation            *)
(*                                                      *)
(*  Requires ZipMaster Library                          *)
(*                                                      *)
(********************************************************)

unit ZipImpl;

interface

uses
  SysUtils, Classes, FileTools, StrTools, BaseTypes, ZipMstr;

implementation

{$R 'res\zmres.res'}
{$R 'res\dzsfxus.res'}

{ TZipFile }

type
  TZipFile = class(TInterfacedObject, IZipFile)
  private
    FZipMaster: TZipMaster;
    FLoaded: Boolean;
    procedure Load;
    procedure ZipMessage(Sender: TObject; ErrCode: Integer;
      Message: string);
  protected
    procedure Add(const Files: string; Recurse: TRecurseMode = rmFile); overload;
    procedure Add(Files: TStrings; Recurse: TRecurseMode = rmFile); overload;
    procedure CopyList(Strings: TStrings);
    procedure Extract(const Directory, Files: string;
      ExpandDirs: Boolean = True); overload;
    procedure Extract(const Directory: string; Files: TStrings;
      ExpandDirs: Boolean = True); overload;
    procedure Remove(const Files: string); overload;
    procedure Remove(Files: TStrings); overload;
    procedure Rename(const Files: string); overload;
    procedure Rename(Files: TStrings); overload;
    procedure RenamePath(const OldPath, NewPath: string);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetCount: Integer;
    function GetItem(Index: Integer): string;
    procedure SetItem(Index: Integer; const Value: string);
    function GetTempDir: string;
    procedure SetTempDir(Value: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRenameArray = array of ZipRenameRec;

const
  SplitChar = ',';

{ TZipFile }

constructor TZipFile.Create;
begin
  inherited Create;
  FZipMaster := TZipMaster.Create(nil);
  FZipMaster.AddOptions := [AddDirNames];
  FZipMaster.HowToDelete := htdFinal;
  FZipMaster.Unattended := True;
  FZipMaster.OnMessage := ZipMessage;
end;

destructor TZipFile.Destroy;
begin
  FZipMaster.Dll_Load := False;
  FZipMaster.Free;
  inherited Destroy;
end;

procedure TZipFile.Load;
begin
  if not FLoaded then
    FZipMaster.Dll_Load := True;
  FLoaded := True;
end;

procedure TZipFile.ZipMessage(Sender: TObject; ErrCode: Integer;
  Message: string);
begin
  if ErrCode <> 0 then
    raise EZipException.Create(Message);
end;

{ TZipFile.IZipFile }

procedure TZipFile.Add(const Files: string; Recurse: TRecurseMode = rmFile);
var
  S: TStrings;
begin
  Load;
  S := TStringList.Create;
  try
    S.Text := Files;
    Add(S, Recurse);
  finally
    S.Free;
  end;
end;


procedure TZipFile.Add(Files: TStrings; Recurse: TRecurseMode = rmFile);
const
  RecurseChars: array[TRecurseMode] of Char = (#0, '|', '>');
var
  C: Char;
  I: Integer;
begin
  Load;
  C := RecurseChars[Recurse];
  if Recurse <> rmFile then
  begin
    for I := 0 to Files.Count - 1 do
      Files[I] := C + Files[I];
  end;
  FZipMaster.FSpecArgs.Assign(Files);
  FZipMaster.Add;
end;

procedure TZipFile.CopyList(Strings: TStrings);
var
  I: Integer;
begin
  Load;
  Strings.BeginUpdate;
  try
    Strings.Clear;
    for I := 0 to GetCount - 1 do
      Strings.Add(GetItem(I));
  finally
    Strings.EndUpdate;
  end;
end;

procedure TZipFile.Extract(const Directory, Files: string;
  ExpandDirs: Boolean = True);
var
  S: TStrings;
begin
  Load;
  S := TStringList.Create;
  try
    S.Text := Files;
    Extract(Directory, S, ExpandDirs);
  finally
    S.Free;
  end;
end;

procedure TZipFile.Extract(const Directory: string; Files: TStrings;
  ExpandDirs: Boolean = True);
begin
  Load;
  FZipMaster.ExtrBaseDir := Directory;
  if ExpandDirs then
    FZipMaster.ExtrOptions := [ExtrOverwrite, ExtrDirNames]
  else
    FZipMaster.ExtrOptions := [ExtrOverwrite];
  FZipMaster.FSpecArgs.Assign(Files);
  FZipMaster.Extract;
end;

procedure TZipFile.Remove(const Files: string);
var
  S: TStrings;
begin
  Load;
  S := TStringList.Create;
  try
    S.Text := Files;
    Remove(S);
  finally
    S.Free;
  end;
end;

procedure TZipFile.Remove(Files: TStrings);
begin
  Load;
  FZipMaster.FSpecArgs.Assign(Files);
  FZipMaster.Delete;
end;

procedure TZipFile.Rename(const Files: string);
var
  S: TStrings;
begin
  Load;
  S := TStringList.Create;
  try
    S.Text := Files;
    Rename(S);
  finally
    S.Free;
  end;
end;

procedure TZipFile.Rename(Files: TStrings);
var
  Counter: Integer;
  Names: TRenameArray;
  List: TList;
  S: TStringArray;
  I: Integer;
begin
  Load;
  Counter := 0;
  S := nil;
  for I := 0 to Files.Count - 1 do
    if Length(Split(Files[I], SplitChar)) = 2 then
      Inc(Counter);
  if Counter = 0 then Exit;
  SetLength(Names, Counter);
  List := TList.Create;
  try
    Counter := 0;
    for I := 0 to Files.Count - 1 do
    begin
      S := Split(Files[I], SplitChar);
      if Length(S) <> 2 then Continue;
      Names[Counter].Source := S[0];
      Names[Counter].Dest := S[1];
      List.Add(@Names[Counter]);
      Inc(Counter);
    end;
    FZipMaster.Rename(List, 0);
  finally
    List.Free;
  end;
end;

procedure TZipFile.RenamePath(const OldPath, NewPath: string);
var
  A, B: TStrings;
  S: string;
  I: Integer;
begin
  Load;
  A := TStringList.Create;
  B := TStringList.Create;
  try
    S := UpperCase(Trim(OldPath));
    I := Pos(':\', S);
    if I > 0 then
      if Length(S) < 4 then
        S := ''
       else
        S := Copy(S, I + 2, Length(S));
    CopyList(A);
    for I := 0 to A.Count - 1 do
      if Pos(S, UpperCase(A[I])) = 1 then
        B.Add(A[I] + ',' + NewPath + Copy(A[I], Length(S) + 1, Length(A[I])));
     Rename(B);
  finally
    A.Free;
    B.Free;
  end;
end;

function TZipFile.GetFileName: string;
begin
  Load;
  Result := FZipMaster.ZipFileName;
end;

procedure TZipFile.SetFileName(const Value: string);
begin
  Load;
  FZipMaster.ZipFileName := Value;
end;

function TZipFile.GetCount: Integer;
begin
  Load;
  Result := FZipMaster.Count;
end;

function TZipFile.GetItem(Index: Integer): string;
begin
  Load;
  Result := FZipMaster.DirEntry[Index].FileName;
end;

procedure TZipFile.SetItem(Index: Integer; const Value: string);
var
  List: TList;
  R: ZipRenameRec;
begin
  Load;
  List := TList.Create;
  try
    R.Source := GetItem(Index);
    R.Dest := Value;
    R.DateTime := 0;
    List.Add(@R);
    FZipMaster.Rename(List, 0);
  finally
    List.Free;
  end;
end;

function TZipFile.GetTempDir: string;
begin
  Result := FZipMaster.TempDir;
end;

procedure TZipFile.SetTempDir(Value: string);
begin
  FZipMaster.TempDir := Value;
end;

function ZipFileCreate: IZipFile;
begin
  Result := TZipFile.Create;
end;

initialization
  CreateZipFile := ZipFileCreate;
end.

