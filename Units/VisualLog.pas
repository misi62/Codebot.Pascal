
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit VisualLog;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, ComCtrls, ShellAPI, VisualLogFrm,
  Forms, FileTools, ShlTools, ShlCtrls, StrTools;

{ TVisualLog }

type
  TVisualLog = class(TObject)
  private
    FListView: TListView;
    FRootDir: string;
    FPrefix: string;
    FFileName: string;
    FFrame: TVisualLogFrame;
    procedure ListViewDoubleClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure LogListIncludeItem(Sender: TObject;
      Node: TShellNode; var AllowAction: Boolean);
    procedure Help;
  public
    constructor Create(ListView: TListView; const RootDir: string);
    procedure Close;
    procedure AlignFrame;
    procedure Add(const Msg: string; Silent: Boolean = False); overload;
    procedure Add(const Strings: TStrings); overload;
    procedure Reset(const Title, Prefix: string);
    procedure Refresh(const Title, Prefix: string);
    procedure Hide;
  end;

implementation

uses Controls;

{ TVisualLog }

var
  FrameCount: Integer;

constructor TVisualLog.Create(ListView: TListView; const RootDir: string);
var
  S: string;
begin
  inherited Create;
  if RootDir = '' then
    S := ExtractFilePath(ParamStr(0))
  else
    S := RootDir;
  CreateDir(ExcludeTrailingPathDelimiter(S));
  FListView := ListView;
  if FListView <> nil then
    FListView.OnDblClick := ListViewDoubleClick;
  FRootDir := IncludeTrailingPathDelimiter(S);
  Help;
  if not FindSwitch('/hide') then
  begin
    FFrame := TVisualLogFrame.Create(FListView.Owner);
    FFrame.Visible := False;
    Inc(FrameCount);
    FFrame.Name := FFrame.Name + IntToStr(FrameCount);
    FFrame.LogList.OnIncludeItem := LogListIncludeItem;
    FFrame.BackButton.OnClick := BackButtonClick;
    AlignFrame;
  end;
end;

procedure TVisualLog.Close;
begin
  FFrame.Free;
end;

procedure TVisualLog.AlignFrame;
begin
  if FFrame <> nil then
  begin
    FFrame.Parent := FListView.Parent;
    FFrame.BoundsRect := FListView.BoundsRect;
    FFrame.Anchors := FListView.Anchors;
    FFrame.LogList.Explore(FRootDir);
  end;
end;

procedure TVisualLog.ListViewDoubleClick(Sender: TObject);
begin
  if FFrame <> nil then
  begin
    FFrame.Visible := True;
    FListView.Visible := False;
    AlignFrame;
    SendMessage(FFrame.LogList.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;
  {if (FListView.Items.Count > 0) and FileExists(FFileName) then
    ShellExecute(0, 'open', PChar(FFileName), nil, nil, SW_SHOW);}
end;

procedure TVisualLog.BackButtonClick(Sender: TObject);
begin
  if FFrame <> nil then
  begin
    FListView.Visible := True;
    FFrame.Visible := False;
  end;
end;


procedure TVisualLog.LogListIncludeItem(Sender: TObject;
  Node: TShellNode; var AllowAction: Boolean);
var
  S: string;
begin
  S := Node.Name;
  AllowAction := (UpperCase(ExtractFileExt(S)) = '.LOG') and (
    UpperCase(Copy(S, 1, Length(FPrefix))) = UpperCase(FPrefix));
end;

procedure TVisualLog.Add(const Msg: string; Silent: Boolean = False);
var
  Item: TListItem;
  F: TextFile;
begin
  if (FListView <> nil) and (not Silent) then
  begin
    Item := FListView.Items.Add;
    Item.Caption := FormatDateTime('hh:mm:ss',  Now);
    Item.SubItems.Add(Msg);
    SendMessage(FListView.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;
  if not FileExists(FFileName) then Exit;
  AssignFile(F, FFileName);
  try
    Append(F);
    WriteLn(F, FormatDateTime('mm/dd/yyyy hh:nn:ssAM/PM', Now) + ' ' + Msg);
  finally
    CloseFile(F);
  end;
end;

procedure TVisualLog.Add(const Strings: TStrings);
var
  Item: TListItem;
  F: TextFile;
  I: Integer;
begin
  if not FileExists(FFileName) then Exit;
  AssignFile(F, FFileName);
  try
    Append(F);
    for I := 0 to Strings.Count - 1 do
      WriteLn(F, Strings[I]);
  finally
    CloseFile(F);
  end;
end;

procedure TVisualLog.Help;
begin
  if FListView <> nil then
  begin
    FListView.Items.Clear;
    with FListView.Items.Add do
    begin
      Caption := 'Usage:';
      SubItems.Add('Double click here to browse log files');
    end;
  end;
end;

procedure Debug(const S: string);
begin
//  FileWriteLn('c:\debug.log', S);
end;

procedure TVisualLog.Reset(const Title, Prefix: string);
var
  F: TextFile;
begin
  Help;
  FPrefix := Prefix;
  FFileName := FRootDir + Prefix + FormatDateTime('yyyymmdd.hhnnss', Now) + '.log';
  AssignFile(F, FFileName);
  try
    Rewrite(F);
    WriteLn(F, 'Log for: ' + Title);
    WriteLn(F, 'Version: ' + GetFileVersion);
    WriteLn(F, 'Created on: ' + FormatDateTime('mmmm d yyyy hh:nn:ssAM/PM', Now));
    WriteLn(F, ' ');
  finally
    CloseFile(F);
  end;
  if FFrame <> nil then
  begin
    FFrame.LogFilesLabel.Caption := 'List of ' + Title + ' log files:';
    try
    FFrame.LogList.HandleNeeded;
    FFrame.LogList.Explore(FRootDir);
    SendMessage(FFrame.LogList.Handle, WM_VSCROLL, SB_BOTTOM, 0);
    except
    end;
  end;
end;

procedure TVisualLog.Refresh(const Title, Prefix: string);
begin
  FPrefix := Prefix;
  if FFrame <> nil then
  begin
    FFrame.LogFilesLabel.Caption := 'List of ' + LowerCase(Title) + ' log files:';
    FFrame.LogList.Refresh;
    SendMessage(FFrame.LogList.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;
end;

procedure TVisualLog.Hide;
begin
  if FFrame <> nil then
    FFrame.Visible := False;
  if FListView <> nil then
    FListView.Visible := False;
end;

end.
