(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.05 Open Source Released 2009                   *)
(*                                                      *)
(********************************************************)
unit FileTools;

interface

{$I CODEBOT.INC}

uses
  Windows, ShellAPI, SysUtils, Classes, Registry;

{ CreateAssociation procedure }

procedure CreateAssociation(const Name, Extension: string; Icon: Integer = 0);

{ GetStandardApplication function }

const
  saNotepad = 0;
  saPaint = 1;
  saCalculator = 2;
  saCDPlayer = 3;
  saMediaPlayer = 4;

function GetStandardApplication(Application: Integer): string;

{ GetTargetFileName }

function GetTargetFileName: string;

{ GetLocalFileName }

function GetLocalFileName(const FileName: string): string;

{ GetTempPath }

function GetTempPath: string;

{ GetTempFileName function }

function GetTempFileName(const Path: string = ''): string;

function FileTempName(const Path: string = ''): string;

{ ChangeFileName function }

function ChangeFileName(const FilePath: string; FileName: string): string;

{ GetShortFileName function }

function GetShortFileName(const FileName: string): string;

{ GetSystemPath function }

function GetSystemPath: string;

{ GetWindowsPath function }

function GetWindowsPath: string;

{ GetConsolePath function }

function GetConsolePath: string;

{ GetFileList procedure }

procedure GetFileList(const Directory: string; Strings: TStrings;
  Wildcards: string = '*.*');

{ GetFileSize function }

function GetFileSize(const FileName: string): Cardinal;

{$IFNDEF D6_UP}
function IncludeTrailingPathDelimiter(const S: string): string;
{$ENDIF}

{ TTempStream }

type
	TTempStream = class(TStream)
  private
  	FStream: TStream;
    FFileName: string;
  protected
		{$IFDEF D6_UP}
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); override;
    {$ENDIF}
    procedure SetSize(NewSize: Longint); override;
  public
  	constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
		{$IFDEF D6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ENDIF}
  end;

{ IZipFile }

	EZipException = class(Exception);
	TRecurseMode = (rmFile, rmDirectory, rmRecurse);

	IZipFile = interface(IUnknown)
	  ['{85854574-28ED-4B86-A62D-FC006E0DA49C}']
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
		property FileName: string read GetFileName write SetFileName;
  	property Count: Integer read GetCount;
    property Item[Index: Integer]: string read GetItem write SetItem; default;
    property TempDir: string read GetTempDir write SetTempDir;
  end;

var
	CreateZipFile: function: IZipFile = nil;

{ TDrive  }

type
  TDriveKind = (dkUnknown, dkRemovable, dkFixed, dkRemote, dkCDRom, dkRAMDisk);

  TDrive = class(TObject)
  private
    FName: string;
    FVolume: string;
    FSerialNumber: Cardinal;
    FFileSystem: string;
    FFreeSpace: Cardinal;
    FSize: Cardinal;
    FKind: TDriveKind;
    FEjected: Boolean;
  public
    property Name: string read FName;
    property Volume: string read FVolume;
    property SerialNumber: Cardinal read FSerialNumber;
    property FileSystem: string read FFileSystem;
    property FreeSpace: Cardinal read FFreeSpace;
    property Size: Cardinal read FSize;
    property Kind: TDriveKind read FKind;
    property Ejected: Boolean read FEjected;
  end;

{ TDrives }

  TDrives = class
  private
    FDrives: TList;
    FRootDrive: TDrive;
    function GetDrive(Index: Integer): TDrive;
    function GetDriveCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Drive[Index: Integer]: TDrive read GetDrive; default;
    property DriveCount: Integer read GetDriveCount;
    property RootDrive: TDrive read FRootDrive;
  end;

function GetDriveSerialNumber: Cardinal;

{ TSearchThread }

type
  TSearchThread = class(TThread)
  private
    FAborted: Boolean;
    FData: Pointer;
    FEvent: TThreadMethod;
    FFieldAddress: Pointer;
    procedure CallTerminate;
    procedure CheckFieldAddress;
  protected
    procedure DoTerminate; override;
    procedure SyncEvent(const Event: TThreadMethod);
  public
    constructor Create(FieldAddress: Pointer = nil);
    procedure Abort;
    procedure Search;
    property Data: Pointer read FData write FData;
    property Aborted: Boolean read FAborted;
  end;

{ TFileSearch }

  TFindFileEvent = procedure(Sender: TObject; const FileName: string; Size: Integer) of object;
  TFindDirectoryEvent = procedure(Sender: TObject; const Directory: string) of object;

  TFileSearch = class(TSearchThread)
  private
    FFileName: string;
    FFileSize: Integer;
    FDirectory: string;
    FPath: string;
    FRecurse: Boolean;
    FWildcards: string;
    FOnFindDirectory: TFindDirectoryEvent;
    FOnFindFile: TFindFileEvent;
    procedure SetPath(const Value: string);
  protected
    procedure CallFindDirectory;
    procedure CallFindFile;
    procedure Execute; override;
    procedure FindDirectory; virtual;
    procedure FindFile; virtual;
    property Directory: string read FDirectory;
    property FileName: string read FFileName;
  public
    property Path: string read FPath write SetPath;
    property Recurse: Boolean read FRecurse write FRecurse;
    property Wildcards: string read FWildcards write FWildcards;
    property OnFindDirectory: TFindDirectoryEvent read FOnFindDirectory
      write FOnFindDirectory;
    property OnFindFile: TFindFileEvent read FOnFindFile write FOnFindFile;
  end;

{ TFileList }

  TFileItem = record
    Name: string;
    Created: TDateTime;
    Modified: TDateTime;
    Size: Cardinal;
  end;
  PFileItem = ^TFileItem;

  TFileField = (ffName, ffCreated, ffModified, ffSize);

  TFileList = class(TObject)
  private
    FList: TList;
    FPath: string;
    FWildcard: string;
    FField: TFileField;
    FDescending: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TFileItem;
    procedure SetPath(const Value: string);
    procedure SetWildcard(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh;
    procedure Clear;
    procedure Sort(Field: TFileField; Descending: Boolean = False);
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TFileItem read GetItem; default;
    property Path: string read FPath write SetPath;
    property Wildcard: string read FWildcard write SetWildcard;
  end;

function IsFolderEmpty(const Folder: string): Boolean;

{ TModuleItem }

type
  TModuleResource = class;

  TModuleItem = class(TPersistent)
  private
    FOwner: TModuleResource;
    FHandle: THandle;
    FID: Integer;
    FName: string;
    function GetHandle: THandle;
  protected
    function CreateHandle: THandle; virtual; abstract;
    procedure DestroyHandle; virtual; abstract;
    function GetResName: PChar;
    class function GetResType: PChar; virtual; abstract;
    function LoadData(var Data: Pointer): Integer;
  public
    constructor Create(AOwner: TModuleResource; ResName: PChar);
    destructor Destroy; override;
    procedure SaveToFile(const Filename: string);
    procedure SaveToStream(Stream: TStream);
    property Handle: THandle read GetHandle;
    property ID: Integer read FID write FID;
    property Name: string read FName;
    property Owner: TModuleResource read FOwner;
  end;

  TModuleItemClass = class of TModuleItem;

{ TBitmapModuleItem }

  TBitmapModuleItem = class(TModuleItem)
  protected
    function CreateHandle: THandle; override;
    procedure DestroyHandle; override;
    class function GetResType: PChar; override;
  end;

{ TIconModuleItem }

  TIconModuleItem = class(TModuleItem)
  protected
    function CreateHandle: THandle; override;
    procedure DestroyHandle; override;
    class function GetResType: PChar; override;
  end;

{ TModuleResource }

  TModuleResource = class(TPersistent)
  private
    FItemClass: TModuleItemClass;
    FList: TList;
    FHandle: THandle;
    function GetCount: Integer;
    function GetItem(Index: Integer): TModuleItem;
    function AddItem(ResName: PChar): TModuleItem;
  public
    constructor Create(const Module: string; ItemClass: TModuleItemClass); overload;
    constructor Create(const Module: HMODULE; ItemClass: TModuleItemClass); overload;
    destructor Destroy; override;
    procedure Realize;
    property Count: Integer read GetCount;
    property Handle: THandle read FHandle;
    property Item[index: Integer]: TModuleItem read GetItem; default;
  end;

(* TVersionInformation class

   Because these DLLs are shared components, they reside in the Windows "system"
   directory. That is, they should be installed into the directory indicated by
   the return value from GetSystemDirectory. Installing in this directory, like
   other shared system components, must be done carefully to avoid overwriting
   newer versions of the DLL that other applications previously installed may be
   relying upon. To retrieve the version information for the installed files, use
   the GetFileVersionInfo APIs provided by the Windows version API. You should
   only install a new DLL if its version information, give by the dwFileVersionMS
   and dwFileVersionLS fields of the VS_FIXEDFILEINFO structure are larger than
   the same version information of the already installed DLL.

 The VS_VERSION_INFO structure is the root structure that contains all other
 file-version information structures.

 VS_VERSION_INFO {
     WORD  wLength;
     WORD  wValueLength;
     WORD  wType;
     WCHAR szKey[];
     WORD  Padding1[];
     VS_FIXEDFILEINFO Value;
     WORD  Padding2[];
     WORD  Children[];
 }; *)

  TLangAndCodePage = record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
  PLangAndCodePage = ^TLangAndCodePage;

  TFixedFileInfo = packed record
    dwSignature: DWORD;
    dwStrucVersion: DWORD;
    dwFileVersionMS: DWORD;
    dwFileVersionLS: DWORD;
    dwProductVersionMS: DWORD;
    dwProductVersionLS: DWORD;
    dwFileFlagsMask: DWORD;
    dwFileFlags: DWORD;
    dwFileOS: DWORD;
    dwFileType: DWORD;
    dwFileSubtype: DWORD;
    dwFileDateMS: DWORD;
    dwFileDateLS: DWORD;
  end;
  PFixedFileInfo = ^TFixedFileInfo;

  TVersionInfo = packed record
    wLength: WORD;
    wValueLength: WORD;
    wType: WORD;
    szKey: array [0..13] of Char;
    Value: TFixedFileInfo;
  end;
  PVersionInfo = ^TVersionInfo;

  TVersionInformation = class(TObject)
  private
    FFileName: string;
    FVersionInfoSize: Integer;
    FVersionInfo: Pointer;
    FFileInfo: PFixedFileInfo;
    FUseSysDir: Boolean;
    procedure SetFileName(const Value: string);
    function GetMajorVersion: Integer;
    function GetMinorVersion: Integer;
    function GetMajorBuild: Integer;
    function GetMinorBuild: Integer;
    function GetVersionItem(Index: Integer): string;
  protected
    procedure Clear;
    property VerionInfo: Pointer read FVersionInfo;
  public
    destructor Destroy; override;
    property UseSysDir: Boolean read FUseSysDir write FUseSysDir;
    property MajorVersion: Integer read GetMajorVersion;
    property MinorVersion: Integer read GetMinorVersion;
    property MajorBuild: Integer read GetMajorBuild;
    property MinorBuild: Integer read GetMinorBuild;
    property FileName: string read FFileName write SetFileName;
    property CompanyName: string index 0 read GetVersionItem;
    property FileDescription: string index 1 read GetVersionItem;
    property FileVersion: string index 2 read GetVersionItem;
    property InternalName: string index 3 read GetVersionItem;
    property LegalCopyright: string index 4 read GetVersionItem;
    property OriginalFilename: string index 5 read GetVersionItem;
    property ProductName: string index 6 read GetVersionItem;
    property ProductVersion: string index 7 read GetVersionItem;
  end;

function GetFileVersion(const FileName: string = ''): string;
function GetFileVersionAlpha(const FileName: string = ''): string;

{ The mother of all random number generators }

function MRandom: Double; overload;
function MRandom(Low, High: Integer): Integer; overload;
procedure MRandSeed(Seed: Integer);

{ The TMD5Digest record is the type of results of
  the MD5 hashsum evaluation functions. The contents
  of a record may be used as four 32-bit integer values
  or as an array of 16 bytes }

type
  TMD5Digest = record
    case Integer of
      0: (A, B, C, D: LongInt);
      1: (V: array [0..15] of Byte);
  end;
  PMD5Digest = ^TMD5Digest;

{ The MD5String function evaluates the MD5 hashsum for
  a string. The S parameter specifies a string to
  evaluate hashsum }

function MD5String(const S: string): TMD5Digest;

{ The MD5File function evaluates the MD5 hashsum for
  a file. The FileName parameter specifies the name
  of a file to evaluate hashsum }

function MD5File(const FileName: string): TMD5Digest;

{ The MD5Stream function evaluates the MD5 hashsum for
  a stream. The Stream parameters specifies the
  TStream descendant class object to evaluate hashsum }

function MD5Stream(const Stream: TStream): TMD5Digest;

{ The MD5Buffer function evaluates the MD5 hashsum for
  any memory buffer. The Buffer parameters specifies a
  buffer to evaluate hashsum. The Size parameter specifies
  the size (in bytes) of a buffer }

function MD5Buffer(const Buffer; Size: Integer): TMD5Digest;

{ The MD5DigestToStr function converts the result of
  a hashsum evaluation function into a string of
  hexadecimal digits }

function MD5DigestToStr(const Digest: TMD5Digest): string;

{ The MD5DigestCompare function compares two
  TMD5Digest record variables. This function returns
  TRUE if parameters are equal or FALSE otherwise }

function MD5DigestCompare(const Digest1, Digest2: TMD5Digest): Boolean;

{ US Secure Hash Algorithm 1 (SHA1) based on the RFC 3174 }
{ US Secure Hash Algorithm 256 (SHA256) based on the RFC 3174 }

{function SHA1String(const S: string): string;
function SHA1Buffer(const Buffer; Size: Integer): string;
function SHA1Stream(const Stream: TStream): string;

function SHAToHex(const Digest: string): string;


function SHA256String(const S: string): string;
function SHA256Buffer(const Buffer; Size: Integer): string;
function SHA256Stream(const Stream: TStream): string;}

{ Hash Message Authentication Code is a type of message authentication code
  calculated using a specific algorithm involving a cryptographic hash function
  in combination with a secret key. Our hash function is SHA1 }

{procedure HMAC(const Data; DataLen: Integer; const Key; KeyLen: Integer;
  out Digest: SHA1Digest); overload;
function HMAC(const Data, Key: string): string; overload;}

{ Base64 refers is a specific MIME content transfer encoding. It also a
  generic term for an encoding scheme that encodes binary data  }

function CalcEncodedSize(Size: Cardinal): Cardinal;
function CalcDecodedSize(const Buffer; Size: Cardinal): Cardinal;

function Base64Encode(const Text: string): string; overload;
function Base64Decode(const Text: string): string; overload;

function Base64Encode(const Buffer; Size: Cardinal): string; overload;
procedure Base64Encode(const Buffer; Size: Cardinal; var Output); overload;

{.$DEFINE Quick64}
{$IFNDEF Quick64}
  {$DEFINE Check64}
{$ENDIF}

{$IFDEF Quick64}
procedure Base64Decode(const Buffer; Size: Cardinal; var Output); overload;
{$ENDIF}
{$IFDEF Check64}
function Base64Decode(const Buffer; Size: Cardinal; var Output): Boolean; overload;
{$ENDIF}

procedure Base64Encode(const Text: PChar; var Output: PChar); overload;
procedure Base64Decode(const Text: PChar; var Output: PChar); overload;

procedure Base64Encode(const Text: string; var Output: string); overload;
procedure Base64Decode(const Text: string; var Output: string); overload;

{ Network time routines }

function TimeZoneBias: Integer;
function TimeZone: string;
function NetworkTime(const Time: TDateTime): string;

implementation

uses
  StrConst;

{$IFNDEF D6_UP}
function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if Result <> '' then
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
end;
{$ENDIF}

procedure CreateAssociation(const Name, Extension: string; Icon: Integer = 0);
var
  Reg: TRegistry;
  S: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.OpenKey('.' + Extension, True);
    Reg.WriteString('', Extension + '_autofile');
    Reg.CloseKey;
    Reg.OpenKey(Extension + '_autofile', True);
    Reg.WriteString('', Name);
    Reg.CloseKey;
    Reg.OpenKey(Extension + '_autofile\shell\open\command', True);
    S := ParamStr(0);
    Reg.WriteString('', S + ' "%1"');
    Reg.CloseKey;
    if Icon <> 0 then
    begin
      Reg.OpenKey(Extension + '_autofile\DefaultIcon', True);
      Reg.WriteString('', S + ',' + IntToStr(Icon));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function GetStandardApplication(Application: Integer): string;
const
  Applications: array[saNotepad..saMediaPlayer] of PChar =
    ('NOTEPAD.EXE', 'PBRUSH.EXE', 'CALC.EXE', 'CDPLAYER.EXE', 'MPLAYER.EXE');
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  Result := '';
  if (Application >= saNotepad) or (Application <= saMediaPlayer) then
  begin
    GetWindowsDirectory(Buffer, MAX_PATH);
    Result := Format('%s\%s', [StrPas(Buffer),
      StrPas(Applications[Application])]);
    if not FileExists(Result) then
      Result := '';
  end;
end;

function GetTargetFileName: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if IsLibrary then
  begin
    GetModuleFileName(HInstance, Buffer, SizeOf(Buffer));
    Result := Buffer;
  end
  else
    Result := ParamStr(0);
end;

function GetLocalFileName(const FileName: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result);
  Result := Result + FileName;
end;

function GetTempPath: string;
begin
  SetLength(Result, MAX_PATH);
  Windows.GetTempPath(MAX_PATH, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetTempFileName(const Path: string = ''): string;
var
  TempPath: string;
begin
  if Path = '' then
		TempPath := GetTempPath
	else
		TempPath := IncludeTrailingPathDelimiter(Path);
  SetLength(Result, MAX_PATH);
  Windows.GetTempFileName(PChar(TempPath), '~TM', 0, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
  DeleteFile(Result);
end;

function FileTempName(const Path: string = ''): string;
begin
  Result := GetTempFileName;
end;

function ChangeFileName(const FilePath: string; FileName: string): string;
var
  Path: string;
  S: string;
begin
  Path := FilePath;
  S := ExtractFileName(Path);
  SetLength(Path, Length(Path) - Length(S));
  Result := Path + FileName;
end;

function GetShortFileName(const FileName: string): string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if GetShortPathName(PChar(FileName), Buffer, MAX_PATH) > 0 then
    Result := Buffer
  else
    RaiseLastWin32Error;
end;

function GetSystemPath: string;
begin
  SetLength(Result, MAX_PATH);
  GetSystemDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetWindowsPath: string;
begin
  SetLength(Result, MAX_PATH);
  GetWindowsDirectory(PChar(Result), MAX_PATH);
  SetLength(Result, StrLen(PChar(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

function GetConsolePath: string;
const
  Consoles: array[Boolean] of string = ('command.com', 'cmd.exe');
var
  Info: TOSVersionInfo;
  S: string;
begin
  Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(Info);
  S := Consoles[Info.dwPlatformId = VER_PLATFORM_WIN32_NT];
  if FileExists(GetSystemPath + S) then
    Result := GetSystemPath + S
  else if FileExists(GetWindowsPath + S) then
    Result := GetWindowsPath + S
  else
    Result := '';
end;

procedure GetFileList(const Directory: string; Strings: TStrings;
  Wildcards: string = '*.*');
var
  PriorSorted: Boolean;
  SearchRec: TSearchRec;
  SearchResult: Integer;
  Cards: TStrings;
  S: string;
  I: Integer;
begin
  Strings.Clear;
  S := Directory;
  if S = '' then Exit;
  S := IncludeTrailingPathDelimiter(S);
  Cards := TStringList.Create;
  try
    Cards.Text := StringReplace(WildCards, ';', #13#10, [rfReplaceAll]);
    for I := 0 to Cards.Count - 1 do
    begin
      SearchResult := FindFirst(S + Cards[I], faAnyFile and
        (not faDirectory), SearchRec);
      while SearchResult = 0 do
      begin
        Strings.AddObject(S + SearchRec.Name, TObject(SearchRec.Size));
        SearchResult := FindNext(SearchRec);
      end;
      FindClose(SearchRec);
    end;
  finally
    Cards.Free;
  end;
  if Strings is TStringList then
    with Strings as TStringList do
    begin
      PriorSorted := Sorted;
      Sorted := True;
      Sorted := PriorSorted;
    end;
end;

function GetFileSize(const FileName: string): Cardinal;
var
  SearchRec: TSearchRec;
  SearchResult: Integer;
begin
  SearchResult := FindFirst(FileName, faAnyFile and (not faDirectory), SearchRec);
  if SearchResult = 0 then
    Result := SearchRec.Size
  else
    Result := 0;
  FindClose(SearchRec);
end;

{ TTempStream }

constructor TTempStream.Create;
begin
	inherited Create;
  FFileName := GetTempFileName;
  FStream := TFileStream.Create(FFileName, fmCreate);
end;

destructor TTempStream.Destroy;
begin
	FStream.Free;
  if FileExists(FFileName) then
	  DeleteFile(FFileName);
  inherited Destroy;
end;

function TTempStream.Read(var Buffer; Count: Integer): Longint;
begin
	Result := FStream.Read(Buffer, Count);
end;


procedure TTempStream.SetSize(NewSize: Integer);
begin
  SetSize(Int64(NewSize));
end;

{$IFDEF D6_UP}
procedure TTempStream.SetSize(const NewSize: Int64);
const
	MemSize = 1024 * 1024 * 10;
var
	A, B, P: Int64;
  Data: Pointer;
begin
	A := NewSize;
  B := FStream.Size;
	P := FStream.Position;
  if A > B then
  begin
  	FStream.Seek(0, soFromEnd);
    GetMem(Data, MemSize);
    try
    	FillChar(Data^, MemSize, #0);
      A := A - B;
      while True do
	      if A > MemSize then
  	    begin
					FStream.Write(Data^, MemSize);
      	  Dec(A, MemSize);
				end
      	else
      	begin
					FStream.Write(Data^, A);
        	Break;
      	end;
    finally
    	FreeMem(Data);
    end;
    FStream.Position := P;
  end
  else
	  FStream.Size := NewSize;
end;

function TTempStream.GetSize: Int64;
begin
	Result := FStream.Size;
end;

function TTempStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
	Result := FStream.Seek(Offset, Origin);
end;
{$ENDIF}
function TTempStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
	Result := FStream.Seek(Offset, Origin);
end;

function TTempStream.Write(const Buffer; Count: Integer): Longint;
begin
	Result := FStream.Write(Buffer, Count);
end;

{ TDrives }

constructor TDrives.Create;

  procedure GetDriveInfo(Drive: PChar; var FreeSpace, Size: Cardinal);
  var
    BytesPerSector: Cardinal;
    SectorsPerCluster: Cardinal;
    NumberOfFreeClusters: Cardinal;
    TotalNumberOfClusters: Cardinal;
  begin
    FreeSpace := 0;
    Size := 0;
    if GetDiskFreeSpace(Drive, SectorsPerCluster, BytesPerSector,
      NumberOfFreeClusters, TotalNumberOfClusters) then
    begin
      FreeSpace := BytesPerSector * SectorsPerCluster * NumberOfFreeClusters;
      Size := BytesPerSector * SectorsPerCluster * TotalNumberOfClusters;
    end;
  end;

const
  MAX_DRIVESTRINGS = MAX_PATH * 24;
var
  LastMode: Cardinal;
  Drive: TDrive;
  Dummy: Cardinal;
  S: string;
  P, Pos: PChar;
begin
  FDrives := TList.Create;
  LastMode := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
  try
    Setlength(S, MAX_DRIVESTRINGS);
    GetLogicalDriveStrings(MAX_DRIVESTRINGS, PChar(S));
    P := PChar(S);
    Pos := P;
    repeat
      while Pos^ <> #0 do
        Inc(Pos);
      Drive := TDrive.Create;
      with Drive do
      begin
        FName := P;
        SetLength(FVolume, MAX_PATH);
        SetLength(FFileSystem, MAX_PATH);
        GetVolumeInformation(P, PChar(FVolume), MAX_PATH, @FSerialNumber, Dummy,
          Dummy, PChar(FFileSystem), MAX_PATH);
        SetLength(FVolume, StrLen(PChar(FVolume)));
        SetLength(FFileSystem,StrLen(PChar(FFileSystem)));
        case GetDriveType(P) of
          DRIVE_REMOVABLE: FKind := dkRemovable;
          DRIVE_FIXED: FKind := dkFixed;
          DRIVE_REMOTE: FKind := dkRemote;
          DRIVE_CDROM: FKind := dkCDRom;
          DRIVE_RAMDISK: FKind := dkRAMDisk;
        else
          FKind := dkUnknown;
        end;
        GetDriveInfo(P, FFreeSpace, FSize);
        if UpperCase(FName) = Copy(GetWindowsPath, 1, Length(FName)) then
          FRootDrive := Drive;
      end;
      FDrives.Add(Drive);
      Inc(Pos);
      P := Pos;
    until P^ = #0;
  finally
    SetErrorMode(LastMode);
  end;
end;

destructor TDrives.Destroy;
var
  I: Integer;
begin
  for I := 0 to FDrives.Count - 1 do
    TObject(FDrives[I]).Free;
  FDrives.Free;
  inherited Destroy;
end;

function TDrives.GetDrive(Index: Integer): TDrive;
begin
  Result := TDrive(FDrives[Index]);
end;

function TDrives.GetDriveCount: Integer;
begin
  Result := FDrives.Count;
end;

function GetDriveSerialNumber: Cardinal;
begin
  with TDrives.Create do
  try
    Result := RootDrive.SerialNumber;
  finally
    Free;
  end;
end;

{ TSearchThread }

constructor TSearchThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TSearchThread.Abort;
begin
  FAborted := True;
  Terminate;
  if (FFieldAddress <> nil) and (TObject(FFieldAddress^) = Self) then
    TObject(FFieldAddress^) := nil;
end;

procedure TSearchThread.CallTerminate;
begin
  OnTerminate(Self);
  if FFieldAddress <> nil then
    TObject(FFieldAddress^) := nil;
end;

procedure TSearchThread.CheckFieldAddress;
begin
  if (FFieldAddress = nil) or (TObject(FFieldAddress^) = Self) then
    FEvent
  else
    Abort;
end;

procedure TSearchThread.DoTerminate;
begin
  if Assigned(OnTerminate) then
    SyncEvent(CallTerminate);
end;

procedure TSearchThread.Search;
begin
  Resume;
end;

procedure TSearchThread.SyncEvent(const Event: TThreadMethod);
begin
  FEvent := Event;
  Synchronize(CheckFieldAddress);
  FEvent := nil;
end;

{ TFileSearch }

procedure TFileSearch.CallFindDirectory;
begin
  FOnFindDirectory(Self, FDirectory);
end;

procedure TFileSearch.CallFindFile;
begin
  FOnFindFile(Self, FFileName, FFileSize);
end;

procedure TFileSearch.FindDirectory;
begin
  if (not Terminated) and Assigned(FOnFindDirectory) then
    SyncEvent(CallFindDirectory);
end;

procedure TFileSearch.FindFile;
begin
  if (not Terminated) and Assigned(FOnFindFile) then
    SyncEvent(CallFindFile);
end;

procedure TFileSearch.Execute;
var
  WildcardStrings: TStrings;

  procedure Search(const Dir: string);
  var
    SearchRec: TSearchRec;
    SearchResult: Integer;
    Directories: TStrings;
    I: Integer;
  begin
    Directories := TStringList.Create;
    try
      FDirectory := Dir;
      SyncEvent(FindDirectory);
      for I := 0 to WildcardStrings.Count - 1 do
        if not Terminated then
        begin
          SearchResult := FindFirst(Dir + WildcardStrings[I], faAnyFile and (not faDirectory), SearchRec);
          while (not Terminated) and (SearchResult = 0) do
          begin
            FFileName := Dir + SearchRec.Name;
            FFileSize := SearchRec.Size;
            SyncEvent(FindFile);
            SearchResult := FindNext(SearchRec);
          end;
          FindClose(SearchRec);
        end;
      if FRecurse then
      begin
        SearchResult := FindFirst(Dir + '*.*', faDirectory, SearchRec);
        while SearchResult = 0 do
        begin
          if SearchRec.Name[Length(SearchRec.Name)] <> '.' then
            Directories.Add(Dir + SearchRec.Name + '\');
          SearchResult := FindNext(SearchRec);
        end;
        FindClose(SearchRec);
      end;
      for I := 0 to Directories.Count - 1 do
        if not Terminated then
          Search(Directories[I]);
    finally
      Directories.Free;
    end;
  end;

var
  Wildcard: string;
  StartPos: PChar;
  P: PChar;
begin
  WildcardStrings := TStringList.Create;
  try
    if FWildcards <> '' then
    begin
      P := PChar(FWildcards);
      while P^ <> #0 do
      begin
        StartPos := P;
        while (P^ <> #0) and (P^ <> ';') do
          Inc(P);
        SetString(Wildcard, StartPos, P - StartPos);
        WildcardStrings.Add(Wildcard);
        if P^ = ';' then
          Inc(P);
      end;
    end
    else
      WildCardStrings.Add('*.*');
    if not Terminated then
      Search(FPath);
  finally
    WildcardStrings.Free;
  end;
end;

procedure TFileSearch.SetPath(const Value: string);
begin
  FPath := Value;
  if FPath <> '' then
    FPath := IncludeTrailingPathDelimiter(FPath);
end;


{ TFileList }

constructor TFileList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TFileList.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function FileTimeToDTime(FTime: TFileTime): TDateTime;
var
  LocalFTime: TFileTime;
  STime: TSystemTime;
begin
  FileTimeToLocalFileTime(FTime, LocalFTime);
  FileTimeToSystemTime(LocalFTime, STime);
  Result := SystemTimeToDateTime(STime);
end;

procedure TFileList.Refresh;
var
  SearchResult: Integer;
  SearchRec: TSearchRec;
  FileItem: PFileItem;
  P, W: string;
begin
  Clear;
  if FPath = '' then
    P := GetCurrentDir
  else
    P := FPath;
  P := IncludeTrailingPathDelimiter(P);
  if FWildcard = '' then
    W := P + '*.*'
  else
    W := P + FWildcard;
  SearchResult := FindFirst(W, faAnyFile and (not faDirectory), SearchRec);
  try
    while SearchResult = 0 do
    begin
      New(FileItem);
      FileItem.Name := SearchRec.Name;
      FileItem.Created := FileTimeToDTime(SearchRec.FindData.ftCreationTime);
      FileItem.Modified := FileTimeToDTime(SearchRec.FindData.ftLastAccessTime);
      FileItem.Size := SearchRec.Size;
      FList.Add(FileItem);
      SearchResult := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
  Sort(FField, FDescending);
end;

procedure TFileList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    Dispose(FList[I]);
  FList.Clear;
end;

function FileSortNameAsc(A, B: PFileItem): Integer;
begin
  if A.Name > B.Name then
    Result := 1
  else if A.Name < B.Name then
    Result := -1
  else
    Result := 0;
end;

function FileSortNameDesc(A, B: PFileItem): Integer;
begin
  if A.Name > B.Name then
    Result := -1
  else if A.Name < B.Name then
    Result := 1
  else
    Result := 0;
end;

function FileSortCreatedAsc(A, B: PFileItem): Integer;
begin
  if A.Created > B.Created then
    Result := 1
  else if A.Created < B.Created then
    Result := -1
  else
    Result := 0;
end;

function FileSortCreatedDesc(A, B: PFileItem): Integer;
begin
  if A.Created > B.Created then
    Result := -1
  else if A.Created < B.Created then
    Result := 1
  else
    Result := 0;
end;

function FileSortModifiedAsc(A, B: PFileItem): Integer;
begin
  if A.Modified > B.Modified then
    Result := 1
  else if A.Modified < B.Modified then
    Result := -1
  else
    Result := 0;
end;

function FileSortModifiedDesc(A, B: PFileItem): Integer;
begin
  if A.Modified > B.Modified then
    Result := -1
  else if A.Modified < B.Modified then
    Result := 1
  else
    Result := 0;
end;

function FileSortSizeAsc(A, B: PFileItem): Integer;
begin
  if A.Size > B.Size then
    Result := 1
  else if A.Size < B.Size then
    Result := -1
  else
    Result := 0;
end;

function FileSortSizeDesc(A, B: PFileItem): Integer;
begin
  if A.Size > B.Size then
    Result := -1
  else if A.Size < B.Size then
    Result := 1
  else
    Result := 0;
end;

procedure TFileList.Sort(Field: TFileField; Descending: Boolean = False);
var
  P: Pointer;
begin
  FField := Field;
  FDescending := Descending;
  case Field of
    ffName:
      if Descending then P := @FileSortNameDesc else P := @FileSortNameAsc;
    ffCreated:
      if Descending then P := @FileSortCreatedDesc else P := @FileSortCreatedAsc;
    ffModified:
      if Descending then P := @FileSortModifiedDesc else P := @FileSortModifiedAsc;
    ffSize:
      if Descending then P := @FileSortSizeDesc else P := @FileSortSizeAsc;
  else
    P := @FileSortNameAsc;
  end;
  FList.Sort(P);
end;

function TFileList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TFileList.GetItem(Index: Integer): TFileItem;
begin
  Result := PFileItem(FList[Index])^;
end;

procedure TFileList.SetPath(const Value: string);
begin
  FPath := Trim(Value);
  if FPath <> '' then
    FPath := IncludeTrailingPathDelimiter(FPath);
end;

procedure TFileList.SetWildcard(const Value: string);
begin
  FWildcard := Value;
end;

function IsFolderEmpty(const Folder: string): Boolean;
var
  SearchRec: TSearchRec;
	S: string;
  I: Integer;
begin
  S := IncludeTrailingPathDelimiter(Folder) + '*.*';
  I := FindFirst(S, faAnyFile, SearchRec);
  Result := True;
	while I = 0 do
  begin
		if SearchRec.Name[1] <> '.' then
    begin
    	Result := False;
      Break;
    end;
		I := FindNext(SearchRec);
  end;
	FindClose(SearchRec);
end;

{ TModuleItem }

constructor TModuleItem.Create(AOwner: TModuleResource; ResName: PChar);
begin
  inherited Create;
  FOwner := AOwner;
  if LongRec(ResName).Hi = 0 then
    FID := Integer(ResName)
  else
    FName := ResName;
end;

destructor TModuleItem.Destroy;
begin
  if FHandle <> 0 then
    DestroyHandle;
  inherited Destroy;
end;

function TModuleItem.GetHandle: THandle;
begin
  if FHandle = 0 then
    FHandle := CreateHandle;
  Result := FHandle;
end;

function TModuleItem.GetResName: PChar;
begin
  if FName <> '' then
    Result := PChar(FName)
  else
    Result := MakeIntResource(FID);
end;

function TModuleItem.LoadData(var Data: Pointer): Integer;
var
  Module: THandle;
  ResInfo: THandle;
begin
  Module := FOWner.Handle;
  ResInfo := FindResource(Module, GetResName, GetResType);
  Data := LockResource(ResInfo);
  Result := SizeofResource(Module, ResInfo);
end;

procedure TModuleItem.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TModuleItem.SaveToStream(Stream: TStream);
var
  Data: Pointer;
  DataSize: Integer;
begin
  DataSize := LoadData(Data);
  Stream.Write(PChar(Data)^, DataSize);
end;

{ TBitmapModuleItem }

function TBitmapModuleItem.CreateHandle: THandle;
begin
  Result := LoadBitmap(Owner.Handle, GetResName);
end;

procedure TBitmapModuleItem.DestroyHandle;
begin
  DeleteObject(Handle);
end;

class function TBitmapModuleItem.GetResType: PChar;
begin
  Result := RT_BITMAP;
end;

{ TIconModuleItem }

function TIconModuleItem.CreateHandle: THandle;
begin
  Result := LoadIcon(Owner.Handle, GetResName);
end;

procedure TIconModuleItem.DestroyHandle;
begin
  DeleteObject(Handle);
end;

class function TIconModuleItem.GetResType: PChar;
begin
  Result := RT_ICON;
end;

{ TModuleResource }

constructor TModuleResource.Create(const Module: HMODULE; ItemClass: TModuleItemClass);
begin
  inherited Create;
  FHandle := Module;
  if FHandle = 0 then
    raise Exception.Create('invalid module');
  FItemClass := ItemClass;
end;

constructor TModuleResource.Create(const Module: string; ItemClass: TModuleItemClass);
begin
  Create(LoadLibrary(PChar(Module)), ItemClass)
end;

destructor TModuleResource.Destroy;
var
  I: Integer;
begin
  if FList <> nil then
  begin
    for I := FList.Count - 1 downto 0 do
      TObject(FList[I]).Free;
    FList.Free;
  end;
  if FHandle <> 0 then
    FreeLibrary(FHandle); // change to detect creation type
  inherited Destroy;
end;

function TModuleResource.AddItem(ResName: PChar): TModuleItem;
begin
  Result := FItemClass.Create(Self, ResName);
  with Result do
    if Handle <> 0 then
    begin
      DestroyHandle;
      FHandle := 0;
      FList.Add(Result);
    end
    else
    begin
      Free;
      Result := nil;
    end;
end;

function EnumResourceProc(Module: THandle; ResType: Integer; ResName: PChar;
  ModuleResource: TModuleResource): Boolean; stdcall;
{var
  ModuleItem: TModuleItem;}
begin
  ModuleResource.AddItem(ResName);
  Result := True;
end;

procedure TModuleResource.Realize;
begin
  if FList = nil then
  begin
    FList := TList.Create;
    EnumResourceNames(FHandle, FItemClass.GetResType, @EnumResourceProc, Integer(Self));
  end;
end;

function EnumCountProc(Module: THandle; ResType: Integer; ResName: PChar;
  Count: PInteger): Boolean; stdcall;
begin
  Inc(Count^);
  Result := True;
end;

function TModuleResource.GetCount: Integer;
var
  I: Integer;
begin
  if FList <> nil then
    Result := FList.Count
  else
  begin
    I := 0;
    EnumResourceNames(FHandle, FItemClass.GetResType, @EnumCountProc, Integer(@I));
    Result := I;
  end;
end;

function TModuleResource.GetItem(Index: Integer): TModuleItem;
begin
  Realize;
  Result := TModuleItem(FList[Index]);
end;

{ TVersionInformation }

const
  VER_STRINGCOUNT = 8;
  VersionNames: array [0..VER_STRINGCOUNT - 1] of string =
    ('CompanyName',
     'FileDescription',
     'FileVersion',
     'InternalName',
     'LegalCopyright',
     'OriginalFilename',
     'ProductName',
     'ProductVersion');

destructor TVersionInformation.Destroy;
begin
  Clear;
end;

procedure TVersionInformation.Clear;
begin
  FFileInfo := nil;
  if Assigned(FVersionInfo) then
  begin
    FreeMem(FVersionInfo);
    FVersionInfo := nil;
    FVersionInfoSize := 0;
  end;
end;

procedure TVersionInformation.SetFileName(const Value: string);
var
  Handle: THandle;
  SysDir: PChar;
  Len: Cardinal;
begin
  Clear;
  SysDir := StrAlloc(MAX_PATH);
  try
    FFileName := Value;
    GetSystemDirectory(SysDir, MAX_PATH);
    if FUseSysDir then
      FFileName := StrPas(SysDir) + '\' + Value;
    FVersionInfoSize := GetFileVersionInfoSize(PChar(FFileName), Handle);
    if FVersionInfoSize <> 0 then
    begin
      GetMem(FVersionInfo, FVersionInfoSize);
      GetFileVersionInfo(PChar(FFileName), Handle, FVersionInfoSize, FVersionInfo);
      VerQueryValue(FVersionInfo, '\', Pointer(FFileInfo), Len);
    end;
  finally
    StrDispose(SysDir);
  end;
end;

function TVersionInformation.GetMajorVersion: Integer;
begin
  Result := 0;
  if FFileInfo <> nil then
    Result := LongRec(FFileInfo.dwFileVersionMS).Hi;
end;

function TVersionInformation.GetMinorVersion: Integer;
begin
  Result := 0;
  if FFileInfo <> nil then
    Result := LongRec(FFileInfo.dwFileVersionMS).Lo;
end;

function TVersionInformation.GetMajorBuild: Integer;
begin
  Result := 0;
  if FFileInfo <> nil then
    Result := LongRec(FFileInfo.dwProductVersionLS).Hi;
end;

function TVersionInformation.GetMinorBuild: Integer;
begin
  Result := 0;
  if FFileInfo <> nil then
    Result := LongRec(FFileInfo.dwProductVersionLS).Lo;
end;

function TVersionInformation.GetVersionItem(Index: Integer): string;
var
  Buffer: Pointer;
  BufferSize: Cardinal;
  CodePage: PLangAndCodePage absolute Buffer;
  S: string;
begin
  Result := '';
  if (FVersionInfo <> nil) and (VerQueryValue(FVersionInfo,
    '\VarFileInfo\Translation', Buffer, BufferSize)) then
  begin
    S := Format('\StringFileInfo\%.4x%.4x\%s', [CodePage.wLanguage,
      CodePage.wCodePage, VersionNames[Index]]);
    if VerQueryValue(FVersionInfo, PChar(S), Buffer, BufferSize) then
      Result := PChar(Buffer);
  end;
end;

function GetFileVersion(const FileName: string = ''): string;
var
  V: TVersionInformation;
begin
  V := TVersionInformation.Create;
  try
    if FileName = '' then
      V.FileName := ParamStr(0)
    else
      V.FileName := FileName;
    Result := IntToStr(V.MajorVersion) + '.' + IntToStr(V.MinorVersion) + '.' +
      IntToStr(V.MajorBuild) + '.' + IntToStr(V.MinorBuild);
  finally
    V.Free;
  end;
end;

function GetFileVersionAlpha(const FileName: string = ''): string;
var
  V: TVersionInformation;
begin
  V := TVersionInformation.Create;
  try
    if FileName = '' then
      V.FileName := ParamStr(0)
    else
      V.FileName := FileName;
    Result := IntToStr(V.MajorVersion) + '.' + IntToStr(V.MinorVersion);
    Result := Result + Chr(Ord('a') + V.MajorBuild);
  finally
    V.Free;
  end;
end;

{ Random number generator }

var
  M0: Integer = 0;
  M1: Integer = 0;
  M2: Integer = 0;
  M3: Integer = 0;
  MC: Integer = 0;
  MF0: Integer = 5115;
  MF1: Integer = 1776;
  MF2: Integer = 1492;
  MF3: Integer = 2111111111;
  F2M32: Integer = $2F800000;
  EXTEND: Comp = 0;

function MRandom: Double;
asm
        PUSH    EDI;
        MOV     EAX, MF3;
        MUL     M3;
        MOV     ECX, EAX;
        MOV     EAX, M2;
        MOV     EDI, EDX;
        MOV     M3, EAX;
        MUL     MF2;
        ADD     ECX, EAX;
        MOV     EAX, M1;
        ADC     EDI, EDX;
        MOV     M2, EAX;
        MUL     MF1;
        ADD     ECX, EAX;
        MOV     EAX, M0;
        ADC     EDI, EDX;
        MOV     M1, EAX;
        MUL     MF0;
        ADD     EAX, ECX;
        ADC     EDX, EDI;
        ADD     EAX, MC;
        ADC     EDX, 0;
        MOV     M0, EAX;
        MOV     MC, EDX;
        LEA     EDI, EXTEND;
        MOV     [EDI], EAX;
        FILD    EXTEND;
        POP     EDI;
        FMUL    F2M32;
end;

function MRandom(Low, High: Integer): Integer;
begin
  Result := Low + Trunc(MRandom * (High - Low));
end;

procedure MRandSeed(Seed: Integer);
asm
        PUSH    EDI;
        CMP     EAX, 1;
        SBB     EAX, 0;
        XOR     ECX, ECX;
@R80:   MOV     EDX, EAX;
        SHL     EAX, 13;
        XOR     EDX, EAX;
        MOV     EAX, EDX;
        SHR     EDX, 17;
        XOR     EAX, EDX;
        MOV     EDX, EAX;
        SHL     EDX, 5;
        XOR     EAX, EDX;
        MOV     M0[ECX * 4], EAX;
        INC     ECX;
        CMP     ECX, 5;
        JB      @R80;
        MOV     EDI, 19;
@R90:   CALL    MRandom;
        FSTP    ST(0);
        DEC     EDI;
        JNZ     @R90;
        POP     EDI;
end;

type
  UINT4 = LongWord;

  PArray4UINT4 = ^TArray4UINT4;
  TArray4UINT4 = array [0..3] of UINT4;
  PArray2UINT4 = ^TArray2UINT4;
  TArray2UINT4 = array [0..1] of UINT4;
  PArray16Byte = ^TArray16Byte;
  TArray16Byte = array [0..15] of Byte;
  PArray64Byte = ^TArray64Byte;
  TArray64Byte = array [0..63] of Byte;

  PByteArray = ^TByteArray;
  TByteArray = array [0..0] of Byte;

  PUINT4Array = ^TUINT4Array;
  TUINT4Array = array [0..0] of UINT4;

  PMD5Context = ^TMD5Context;
  TMD5Context = record
    state: TArray4UINT4;
    count: TArray2UINT4;
    buffer: TArray64Byte;
  end;

const
  S11 = 7;
  S12 = 12;
  S13 = 17;
  S14 = 22;
  S21 = 5;
  S22 = 9;
  S23 = 14;
  S24 = 20;
  S31 = 4;
  S32 = 11;
  S33 = 16;
  S34 = 23;
  S41 = 6;
  S42 = 10;
  S43 = 15;
  S44 = 21;

var
 Padding : TArray64Byte = (
  $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

function _F(X, Y, Z: UINT4): UINT4;
begin
	Result := (((x) and (y)) or ((not x) and (z)));
end;

function _G(X, Y, Z: UINT4): UINT4;
begin
	Result := (((x) and (z)) or ((y) and (not z)));
end;

function _H(X, Y, Z: UINT4): UINT4;
begin
	Result := X xor Y xor Z;
end;

function _I(X, Y, Z: UINT4): UINT4;
begin
	Result := Y xor (X or (not Z));
end;

function RotateLeft(X, N: UINT4): UINT4;
begin
	Result := (X shl N) or (X shr (32 - N));
end;

procedure FF(var A: UINT4; B, C, D, X, S, AC: UINT4);
begin
  A := A + _F(B, C, D) + X + AC;
  A := RotateLeft (A, S);
  A := A + B;
end;

procedure GG(var a: UINT4; b, c, d, x, s, ac: UINT4);
begin
 a := a + _G(b, c, d) + x + ac;
 a := RotateLeft(a, s);
 a := a + b;
end;

procedure HH(var a: UINT4; b, c, d, x, s, ac: UINT4);
begin
 a := a + _H(b, c, d) + x + ac;
 a := RotateLeft(a, s);
 a := a + b;
end;

procedure II(var a: UINT4; b, c, d, x, s, ac: UINT4);
begin
 a := a + _I(b, c, d) + x + ac;
 a := RotateLeft(a, s);
 a := a + b;
end;

procedure MD5Encode(Output: PByteArray; Input: PUINT4Array; Len: LongWord);
var
 i, j: LongWord;
begin
	j := 0;
	i := 0;
	while j < Len do
	begin
  output[j] := Byte(input[i] and $FF);
  output[j+1] := Byte((input[i] shr 8) and $FF);
  output[j+2] := Byte((input[i] shr 16) and $FF);
  output[j+3] := Byte((input[i] shr 24) and $FF);
  Inc(j, 4);
  Inc(i);
 end;
end;

procedure MD5Decode(Output: PUINT4Array; Input: PByteArray; Len: LongWord);
var
  I, J: LongWord;
begin
  J := 0;
  I := 0;
  while J < Len do
  begin
    Output[I] := UINT4(Input[J]) or (UINT4(Input[J + 1]) shl 8) or
      (UINT4(Input[J + 2]) shl 16) or ( UINT4(Input[J+3]) shl 24);
    Inc(J, 4);
    Inc(I);
  end;
end;

procedure MD5_memcpy(Output: PByteArray; Input: PByteArray; Len: LongWord);
begin
  Move(Input^, Output^, Len);
end;

procedure MD5_memset(Output: PByteArray; Value: Integer; Len: LongWord);
begin
  FillChar(Output^, Len, Byte(Value));
end;

procedure MD5Transform(State: PArray4UINT4; Buffer: PArray64Byte);
var
 A, B, C, D: UINT4;
 X : array[0..15] of UINT4;
begin
	a := State[0]; b := State[1]; c := State[2]; d := State[3];
	MD5Decode(PUINT4Array(@x), PByteArray(Buffer), 64);
	FF(A, B, C, D, X[ 0], S11, $D76AA478);
	FF(D, A, B, C, X[ 1], S12, $E8C7B756);
	FF(C, D, A, B, X[ 2], S13, $242070DB);
	FF(B, C, D, A, X[ 3], S14, $C1BDCEEE);
	FF(A, B, C, D, X[ 4], S11, $F57C0FAF);
	FF(D, A, B, C, X[ 5], S12, $4787C62A);
	FF(C, D, A, B, X[ 6], S13, $A8304613);
	FF(B, C, D, A, X[ 7], S14, $FD469501);
	FF(A, B, C, D, X[ 8], S11, $698098D8);
	FF(D, A, B, C, X[ 9], S12, $8B44F7AF);
	FF(C, D, A, B, X[10], S13, $FFFF5BB1);
	FF(B, C, D, A, X[11], S14, $895CD7BE);
	FF(A, B, C, D, X[12], S11, $6B901122);
	FF(D, A, B, C, X[13], S12, $FD987193);
	FF(C, D, A, B, X[14], S13, $A679438E);
	FF(B, C, D, A, X[15], S14, $49B40821);
	GG(A, B, C, D, X[ 1], S21, $F61E2562);
	GG(D, A, B, C, X[ 6], S22, $C040B340);
	GG(C, D, A, B, X[11], S23, $265E5A51);
	GG(B, C, D, A, X[ 0], S24, $E9B6C7AA);
	GG(A, B, C, D, X[ 5], S21, $D62F105D);
	GG(D, A, B, C, X[10], S22, $02441453);
	GG(C, D, A, B, X[15], S23, $D8A1E681);
	GG(B, C, D, A, X[ 4], S24, $E7D3FBC8);
	GG(A, B, C, D, X[ 9], S21, $21E1CDE6);
	GG(D, A, B, C, X[14], S22, $C33707D6);
	GG(C, D, A, B, X[ 3], S23, $F4D50D87);
	GG(B, C, D, A, X[ 8], S24, $455A14ED);
	GG(A, B, C, D, X[13], S21, $A9E3E905);
	GG(D, A, B, C, X[ 2], S22, $FCEFA3F8);
	GG(C, D, A, B, X[ 7], S23, $676F02D9);
	GG(B, C, D, A, X[12], S24, $8D2A4C8A);
	HH(A, B, C, D, X[ 5], S31, $FFFA3942);
	HH(D, A, B, C, X[ 8], S32, $8771F681);
	HH(C, D, A, B, X[11], S33, $6D9D6122);
	HH(B, C, D, A, X[14], S34, $FDE5380C);
	HH(A, B, C, D, X[ 1], S31, $A4BEEA44);
	HH(D, A, B, C, X[ 4], S32, $4BDECFA9);
	HH(C, D, A, B, X[ 7], S33, $F6BB4B60);
	HH(B, C, D, A, X[10], S34, $BEBFBC70);
	HH(A, B, C, D, X[13], S31, $289B7EC6);
	HH(D, A, B, C, X[ 0], S32, $EAA127FA);
	HH(C, D, A, B, X[ 3], S33, $D4EF3085);
	HH(B, C, D, A, X[ 6], S34, $04881D05);
	HH(A, B, C, D, X[ 9], S31, $D9D4D039);
	HH(D, A, B, C, X[12], S32, $E6DB99E5);
	HH(C, D, A, B, X[15], S33, $1FA27CF8);
	HH(B, C, D, A, X[ 2], S34, $C4AC5665);
	II(A, B, C, D, X[ 0], S41, $F4292244);
	II(D, A, B, C, X[ 7], S42, $432AFF97);
	II(C, D, A, B, X[14], S43, $AB9423A7);
	II(B, C, D, A, X[ 5], S44, $FC93A039);
	II(A, B, C, D, X[12], S41, $655B59C3);
	II(D, A, B, C, X[ 3], S42, $8F0CCC92);
	II(C, D, A, B, X[10], S43, $FFEFF47D);
	II(B, C, D, A, X[ 1], S44, $85845DD1);
	II(A, B, C, D, X[ 8], S41, $6FA87E4F);
	II(D, A, B, C, X[15], S42, $FE2CE6E0);
	II(C, D, A, B, X[ 6], S43, $A3014314);
	II(B, C, D, A, X[13], S44, $4E0811A1);
	II(A, B, C, D, X[ 4], S41, $F7537E82);
	II(D, A, B, C, X[11], S42, $BD3AF235);
	II(C, D, A, B, X[ 2], S43, $2AD7D2BB);
	II(B, C, D, A, X[ 9], S44, $EB86D391);
	Inc(State[0], A);
	Inc(State[1], B);
	Inc(State[2], C);
	Inc(State[3], D);
	MD5_memset (PByteArray(@x), 0, SizeOf (x));
end;

procedure MD5Init(var Context: TMD5Context);
begin
	FillChar(Context, SizeOf(Context), 0);
	Context.state[0] := $67452301;
	Context.state[1] := $EFCDAB89;
	Context.state[2] := $98BADCFE;
	Context.state[3] := $10325476;
end;

procedure MD5Update(var Context: TMD5Context; Input: PByteArray; InputLen: LongWord);
var
	i, index, partLen: LongWord;
begin
	index := LongWord( (context.count[0] shr 3) and $3F);
	Inc(Context.count[0], UINT4(InputLen) shl 3);
	if Context.count[0] < UINT4(InputLen) shl 3 then Inc(Context.count[1]);
	Inc(Context.count[1], UINT4(InputLen) shr 29);
	partLen := 64 - index;
	if inputLen >= partLen then
  begin
		MD5_memcpy(PByteArray(@Context.buffer[index]), Input, PartLen);
		MD5Transform(@Context.state, @Context.buffer);
		i := partLen;
		while i + 63 < inputLen do
	  begin
			MD5Transform(@Context.state, PArray64Byte(@Input[i]));
			Inc(i, 64);
		end;
		index := 0;
	end
	else
  	i := 0;
	MD5_memcpy(PByteArray(@Context.buffer[index]), PByteArray(@Input[i]), inputLen - i);
end;

procedure MD5Final(var Digest: TMD5Digest; var Context: TMD5Context);
var
	bits: array [0..7] of Byte;
	index, padLen: LongWord;
begin
	MD5Encode(PByteArray(@bits), PUINT4Array(@Context.count), 8);
	index := LongWord( (Context.count[0] shr 3) and $3F);
	if index < 56 then padLen := 56 - index else padLen := 120 - index;
	MD5Update(Context, PByteArray(@PADDING), padLen);
	MD5Update(Context, PByteArray(@Bits), 8);
	MD5Encode(PByteArray(@Digest), PUINT4Array(@Context.state), 16);
	MD5_memset(PByteArray(@Context), 0, SizeOf(Context));
end;

function MD5DigestToStr(const Digest: TMD5Digest): string;
var
 I: Integer;
begin
	Result :=  '';
	for I := Low(Digest.V) to High(Digest.V) do
		Result := Result + IntToHex(Digest.V[I], 2);
end;

function MD5String(const S: string): TMD5Digest;
begin
	Result := MD5Buffer(PChar(S)^, Length(S));
end;

function MD5File(const FileName: string): TMD5Digest;
var
 F: TFileStream;
begin
	F := TFileStream.Create(FileName, fmOpenRead);
	try
		Result := MD5Stream(F);
  finally
		F.Free;
	end;
end;

function MD5Stream(const Stream: TStream): TMD5Digest;
var
  Context: TMD5Context;
  Buffer: array[0..4095] of Byte;
  Size: Integer;
  ReadBytes : Integer;
  TotalBytes : Integer;
  SavePos: Integer;
begin
	MD5Init(Context);
	Size := Stream.Size;
	SavePos := Stream.Position;
	TotalBytes := 0;
	try
		Stream.Seek(0, soFromBeginning);
		repeat
			ReadBytes := Stream.Read(Buffer, SizeOf(Buffer));
			Inc(TotalBytes, ReadBytes);
			MD5Update(Context, @Buffer, ReadBytes);
		until (ReadBytes = 0) or (TotalBytes = Size);
	finally
		Stream.Seek(SavePos, soFromBeginning);
	end;
	MD5Final(Result, Context);
end;

function MD5Buffer(const Buffer; Size: Integer): TMD5Digest;
var
	Context: TMD5Context;
begin
	MD5Init(Context);
	MD5Update(Context, PByteArray(@Buffer), Size);
	MD5Final(Result, Context);
end;

function MD5DigestCompare(const Digest1, Digest2: TMD5Digest): Boolean;
begin
	Result := False;
	if Digest1.A <> Digest2.A then Exit;
	if Digest1.B <> Digest2.B then Exit;
	if Digest1.C <> Digest2.C then Exit;
	if Digest1.D <> Digest2.D then Exit;
	Result := True;
end;

{ SHA1, SHA256 rountines }

const
  SHA1HashSize = 20;
  SHA256HashSize = 256;

type
  SHA1Digest = array[0..SHA1HashSize - 1] of Char;
  SHA256Digest = array[0..SHA256HashSize - 1] of Char;

const
  shaSuccess = 0;
  shaNull = 1;
  shaInputTooLong = 2;
  shaStateError = 3;

type
  SHA1Context = record
    TempHash: array[0..SHA1HashSize div 4 - 1] of LongWord;
    Lo: LongWord;
    Hi: LongWord;
    MsgBlockIndex: LongInt;
    MsgBlock: array[0..63] of Byte;
    Computed: Integer;
    Corrupted: Integer;
  end;

  SHA256Context = record
    TempHash: array[0..SHA256HashSize div 4 - 1] of LongWord;
    Lo: LongWord;
    Hi: LongWord;
    MsgBlockIndex: LongInt;
    MsgBlock: array[0..63] of Byte;
    Computed: Integer;
    Corrupted: Integer;
  end;


{function SHA1Reset(var Context: SHA1Context): Integer; forward;
function SHA1Input(var Context: SHA1Context; Msg: PChar; Length: Cardinal): Integer; forward;
function SHA1Result(var Context: SHA1Context; var Digest: SHA1Digest): Integer; forward;

function SHA256Reset(var Context: SHA256Context): Integer; forward;
function SHA256Input(var Context: SHA1Context; Msg: PChar; Length: Cardinal): Integer; forward;
function SHA256Result(var Context: SHA1Context; var Digest: SHA1Digest): Integer; forward;


extern int SHA256FinalBits(SHA256Context *, const uint8_t bits,
                           unsigned int bitcount);
extern int SHA256Result(SHA256Context *,
                        uint8_t Message_Digest[SHA256HashSize]);


function SHA1Shift(const Bits, W: LongWord): LongWord;
begin
  Result := (W shl Bits) or (W shr (32 - (Bits)));
end;

procedure SHA1ProcessMsgBlock(var Context: SHA1Context);
const
  K: array[0..3] of LongWord = ($5A827999, $6ED9EBA1, $8F1BBCDC, $CA62C1D6);
var
  Temp: LongWord;
  W: array[0..79] of LongWord;
  A, B, C, D, E: LongWord;
  I: Integer;
begin
  for I := 0 to 15 do
    W[I] := Context.MsgBlock[I * 4] shl 24
      or Context.MsgBlock[I * 4 + 1] shl 16
      or Context.MsgBlock[I * 4 + 2] shl 8
      or Context.MsgBlock[I * 4 + 3];
  for I := 16 to 79 do
    W[I] := SHA1Shift(1, W[I - 3] xor W[I - 8] xor W[I - 14] xor W[I - 16]);
  A := Context.TempHash[0];
  B := Context.TempHash[1];
  C := Context.TempHash[2];
  D := Context.TempHash[3];
  E := Context.TempHash[4];
  for I := 0 to 19 do
  begin
    Temp := SHA1Shift(5, A) + ((B and C) or((not B) and D)) + E + W[I] + K[0];
    E := D;
    D := C;
    C := SHA1Shift(30, B);
    B := A;
    A := Temp;
  end;
  for I := 20 to 39 do
  begin
    Temp := SHA1Shift(5, A) +(B xor C xor D) + E + W[I] + K[1];
    E := D;
    D := C;
    C := SHA1Shift(30, B);
    B := A;
    A := Temp;
  end;
  for I := 40 to 59 do
  begin
    Temp := SHA1Shift(5, A) + ((B and C) or (B and D) or (C and D)) + E + W[I] + K[2];
    E := D;
    D := C;
    C := SHA1Shift(30, B);
    B := A;
    A := Temp;
  end;
  for I := 60 to 79 do
  begin
    Temp := SHA1Shift(5, A) +(B xor C xor D) + E + W[I] + K[3];
    E := D;
    D := C;
    C := SHA1Shift(30, B);
    B := A;
    A := Temp;
  end;
  Inc(Context.TempHash[0], A);
  Inc(Context.TempHash[1], B);
  Inc(Context.TempHash[2], C);
  Inc(Context.TempHash[3], D);
  Inc(Context.TempHash[4], E);
  Context.MsgBlockIndex := 0;
end;

procedure SHA1PadMessage(var Context: SHA1Context);
begin
  if Context.MsgBlockIndex > 55 then
  begin
    Context.MsgBlock[Context.MsgBlockIndex] := $80;
    Inc(Context.MsgBlockIndex);
    while(Context.MsgBlockIndex < 64) do
    begin
      Context.MsgBlock[Context.MsgBlockIndex] := 0;
      Inc(Context.MsgBlockIndex);
    end;
    SHA1ProcessMsgBlock(Context);
    while(Context.MsgBlockIndex < 56) do
    begin
      Context.MsgBlock[Context.MsgBlockIndex] := 0;
      Inc(Context.MsgBlockIndex);
    end;
  end
  else
  begin
    Context.MsgBlock[Context.MsgBlockIndex] := $80;
    Inc(Context.MsgBlockIndex);
    while(Context.MsgBlockIndex < 56) do
    begin
      Context.MsgBlock[Context.MsgBlockIndex] := 0;
      Inc(Context.MsgBlockIndex);
    end;
  end;
  Context.MsgBlock[56] := Context.Hi shr 24;
  Context.MsgBlock[57] := Context.Hi shr 16;
  Context.MsgBlock[58] := Context.Hi shr 8;
  Context.MsgBlock[59] := Context.Hi;
  Context.MsgBlock[60] := Context.Lo shr 24;
  Context.MsgBlock[61] := Context.Lo shr 16;
  Context.MsgBlock[62] := Context.Lo shr 8;
  Context.MsgBlock[63] := Context.Lo;
  SHA1ProcessMsgBlock(Context);
end;

function SHA1Reset(var Context: SHA1Context): Integer;
begin
  if @Context = nil then
  begin
    Result := shaNull;
    Exit;
  end;
  FillChar(Context, SizeOf(Context), #0);
  Context.TempHash[0] := $67452301;
  Context.TempHash[1] := $EFCDAB89;
  Context.TempHash[2] := $98BADCFE;
  Context.TempHash[3] := $10325476;
  Context.TempHash[4] := $C3D2E1F0;
  Result := shaSuccess;
end;

function SHA1Input(var Context: SHA1Context; Msg: PChar; Length: Cardinal): Integer;
begin
  if Length = 0 then
  begin
    Result := shaSuccess;
    Exit;
  end;
  if Msg = nil then
  begin
    Result := shaNull;
    Exit;
  end;
  if Context.Computed <> 0 then
  begin
    Context.Corrupted := shaStateError;
    Result := shaStateError;
    Exit;
  end;
  if Context.Corrupted <> 0 then
  begin
    Result := Context.Corrupted;
    Exit;
  end;
  while (Length > 0) and (Context.Corrupted = 0) do
  begin
    Context.MsgBlock[Context.MsgBlockIndex] := Ord(Msg^) and $FF;
    Inc(Context.MsgBlockIndex);
    Inc(Context.Lo, 8);
    if Context.Lo = 0 then
    begin
      Inc(Context.Hi);
      if Context.Hi = 0 then
        Context.Corrupted := 1;
    end;
    if Context.MsgBlockIndex = 64 then
    SHA1ProcessMsgBlock(Context);
    Inc(Msg);
    Dec(Length);
  end;
  Result := shaSuccess;
end;

function SHA1Result(var Context: SHA1Context; var Digest: SHA1Digest): Integer;
var
  I: Integer;
begin
  if Context.Corrupted <> 0 then
  begin
    Result := Context.Corrupted;
    Exit;
  end;
  if Context.Computed = 0 then
  begin
    SHA1PadMessage(Context);
    FillChar(Context.MsgBlock, SizeOf(Context.MsgBlock), #0);
    Context.Lo := 0;
    Context.Hi := 0;
    Context.Computed := 1;
  end;
  for I := 0 to SHA1HashSize-1 do
    Digest[I] := Chr(Context.TempHash[I shr 2] shr (8 *(3 -(LongWord(I) and $03))));
  Result := shaSuccess;
end;

function SHA1String(const s: string): string;
var
  Context: SHA1Context;
  Digest : SHA1Digest;
begin
  SHA1Reset(Context);
  SHA1Input(Context, PChar(S), Length(S));
  SHA1Result(Context, Digest);
  SetLength(Result, SizeOf(Digest));
  Move(Digest, PChar(Result)^, SizeOf(Digest));
end;

function SHA1Buffer(const Buffer; Size: Integer): string;
var
  Context: SHA1Context;
  Digest: SHA1Digest;
begin
  SHA1Reset(Context);
  SHA1Input(Context, PChar(Buffer), Size);
  SHA1Result(Context, Digest);
  SetLength(Result, SizeOf(Digest));
  Move(Digest, PChar(Result)^, SizeOf(Digest));
end;

function SHA1Stream(const Stream: TStream): string;
const
  BufferSize = 4096;
var
  Context: SHA1Context;
  Digest: SHA1Digest;
  Buffer: PChar;
  I: Integer;
begin
  SHA1Reset(Context);
  Stream.Position := 0;
  GetMem(Buffer, BufferSize);
  try
    repeat
      I := Stream.Read(Buffer^, BufferSize);
      if I > 0 then
        SHA1Input(Context, Buffer, I);
    until I < BufferSize;
  finally
    FreeMem(Buffer);
  end;
  SHA1Result(Context, Digest);
  SetLength(Result, SizeOf(Digest));
  Move(Digest, PChar(Result)^, SizeOf(Digest));
end;

function SHAToHex(const Digest: string): string;
var
  I: Integer;
begin
  Result := '';
  for I:= 1 to Length(Digest) do
    Result := Result + IntToHex(Ord(Digest[I]), 2);
  Result := LowerCase(Result);
end;}

{ HMAC routines }

{procedure HMAC(const Data; DataLen: Integer; const Key; KeyLen: Integer; out Digest: SHA1Digest);
var
  A, B: array[0..64] of Byte;
  Context: SHA1Context;
  I: Integer;
begin
  FillChar(A, SizeOf(A), #0);
  FillChar(B, SizeOf(B), #0);
  if KeyLen > 64 then
  begin
    SHA1Reset(Context);
    SHA1Input(Context, PChar(@Key), KeyLen);
    SHA1Result(Context, Digest);
    Move(Digest, A, SizeOf(Digest));
    Move(Digest, B, SizeOf(Digest));
  end
  else
  begin
    Move(Key, A, KeyLen);
    Move(Key, B, KeyLen);
  end;
  for I:= 0 to 63 do
  begin
    A[I] := A[I] xor $36;
    B[I] := B[I] xor $5C;
  end;
  SHA1Reset(Context);
  SHA1Input(Context, PChar(@A[0]), 64);
  SHA1Input(Context, PChar(@Data), DataLen);
  SHA1Result(Context, Digest);
  SHA1Reset(Context);
  SHA1Input(Context, PChar(@B[0]), 64);
  SHA1Input(Context, Digest, SHA1HashSize);
  SHA1Result(Context, Digest);
end;

function HMAC(const Data, Key: string): string;
var
  Digest: SHA1Digest;
begin
  HMAC(PChar(Data)^, Length(Data), PChar(Key)^, Length(Key), Digest);
  SetLength(Result, SizeOf(Digest));
  Move(Digest, PChar(Result)^, SizeOf(Digest));
end;}

{ Base64 routines }

const
  Base64Codec: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Base64Filler = '=';

function CalcEncodedSize(Size: Cardinal): Cardinal;
begin
  Result := (Size div 3) shl 2;
  if ((Size mod 3) > 0)
  then Inc(Result, 4);
end;

function CalcDecodedSize(const Buffer; Size: Cardinal): Cardinal;
type
  BA = array of Byte;
begin
  Result := 0;
  if Size = 0 then
    Exit;
  if Size mod 4 <> 0 then
    Exit;
  Result := Size div 4 * 3;
  if (BA(Buffer)[Size - 2] = Ord(Base64Filler))
  then Dec(Result, 2)
  else if BA(Buffer)[Size - 1] = Ord(Base64Filler) then Dec(Result);
end;

function Base64Encode(const Text: string): string; overload;
begin
  Base64Encode(Text, Result);
end;

function Base64Decode(const Text: string): string; overload;
begin
  Base64Decode(Text, Result);
end;

function Base64Encode(const Buffer; Size: Cardinal): string;
var
  OutSize: Cardinal;
  PIn, POut: Pointer;
begin
  OutSize := CalcEncodedSize(Size);
  SetLength(Result, OutSize);
  PIn := @Buffer;
  POut := Pointer(Result);
  Base64Encode(PIn, Size, POut);
end;

procedure Base64Encode(const Buffer; Size: Cardinal; var Output);
var
  ByThrees, LeftOver: Cardinal;
asm
  MOV  ESI, [EAX]
  MOV  EDI, [ECX]
  MOV  EAX, EBX
  MOV  ECX, $03
  XOR  EDX, EDX
  DIV  ECX
  MOV  ByThrees, EAX
  MOV  LeftOver, EDX
  LEA  ECX, Base64Codec[0]
  XOR  EAX, EAX
  XOR  EBX, EBX
  XOR  EDX, EDX
  CMP  ByThrees, 0
  JZ   @@LeftOver
  @@LoopStart:
    LODSW
    MOV  BL, AL
    SHR  BL, 2
    MOV  DL, BYTE PTR [ECX + EBX]
    MOV  BH, AH
    AND  BH, $0F
    ROL  AX, 4
    AND  AX, $3F
    MOV  DH, BYTE PTR [ECX + EAX]
    MOV  AX, DX
    STOSW
    LODSB
    MOV  BL, AL
    SHR  BX, 6
    MOV  DL, BYTE PTR [ECX + EBX]
    AND  AL, $3F
    XOR  AH, AH
    MOV  DH, BYTE PTR [ECX + EAX]
    MOV  AX, DX
    STOSW
    DEC  ByThrees
  JNZ  @@LoopStart
  @@LeftOver:
  CMP  LeftOver, 0
  JZ   @@Done
  XOR  EAX, EAX
  XOR  EBX, EBX
  XOR  EDX, EDX
  LODSB
  SHL  AX, 6
  MOV  BL, AH
  MOV  DL, BYTE PTR [ECX + EBX]
  DEC  LeftOver
  JZ   @@SaveOne
  SHL  AX, 2
  AND  AH, $03
  LODSB
  SHL  AX, 4
  MOV  BL, AH
  MOV  DH, BYTE PTR [ECX + EBX]
  SHL  EDX, 16
  SHR  AL, 2
  MOV  BL, AL
  MOV  DL, BYTE PTR [ECX + EBX]
  MOV  DH, Base64Filler
  JMP  @@WriteLast4
  @@SaveOne:
  SHR  AL, 2
  MOV  BL, AL
  MOV  DH, BYTE PTR [ECX + EBX]
  SHL  EDX, 16
  MOV  DH, Base64Filler
  MOV  DL, Base64Filler
  @@WriteLast4:
    MOV  EAX, EDX
    ROR EAX, 16
    STOSD
  @@Done:
end;

{$IFDEF Quick64}
procedure Base64Decode(const Buffer; Size: Cardinal; var Output);
{$ENDIF}
{$IFDEF Check64}
function Base64Decode(const Buffer; Size: Cardinal; var Output): Boolean;
{$ENDIF}
const
  {$IFDEF Quick64}
    Base64Codec: array[0..127] of Byte =
  {$ENDIF}
  {$IFDEF Check64}
    Base64Codec: array[0..255] of Byte =
  {$ENDIF}
  (
    $FF, $FF, $FF, $FF, $FF, {005>} $FF, $FF, $FF, $FF, $FF, // 000..009
    $FF, $FF, $FF, $FF, $FF, {015>} $FF, $FF, $FF, $FF, $FF, // 010..019
    $FF, $FF, $FF, $FF, $FF, {025>} $FF, $FF, $FF, $FF, $FF, // 020..029
    $FF, $FF, $FF, $FF, $FF, {035>} $FF, $FF, $FF, $FF, $FF, // 030..039
    $FF, $FF, $FF, $3E, $FF, {045>} $FF, $FF, $3F, $34, $35, // 040..049
    $36, $37, $38, $39, $3A, {055>} $3B, $3C, $3D, $FF, $FF, // 050..059
    $FF, $FF, $FF, $FF, $FF, {065>} $00, $01, $02, $03, $04, // 060..069
    $05, $06, $07, $08, $09, {075>} $0A, $0B, $0C, $0D, $0E, // 070..079
    $0F, $10, $11, $12, $13, {085>} $14, $15, $16, $17, $18, // 080..089
    $19, $FF, $FF, $FF, $FF, {095>} $FF, $FF, $1A, $1B, $1C, // 090..099
    $1D, $1E, $1F, $20, $21, {105>} $22, $23, $24, $25, $26, // 100..109
    $27, $28, $29, $2A, $2B, {115>} $2C, $2D, $2E, $2F, $30, // 110..119
    $31, $32, $33, $FF, $FF, {125>} $FF, $FF, $FF            // 120..127

    {$IFDEF Check64}
                               {125>}              , $FF, $FF, // 128..129
      $FF, $FF, $FF, $FF, $FF, {135>} $FF, $FF, $FF, $FF, $FF, // 130..139
      $FF, $FF, $FF, $FF, $FF, {145>} $FF, $FF, $FF, $FF, $FF, // 140..149
      $FF, $FF, $FF, $FF, $FF, {155>} $FF, $FF, $FF, $FF, $FF, // 150..159
      $FF, $FF, $FF, $FF, $FF, {165>} $FF, $FF, $FF, $FF, $FF, // 160..169
      $FF, $FF, $FF, $FF, $FF, {175>} $FF, $FF, $FF, $FF, $FF, // 170..179
      $FF, $FF, $FF, $FF, $FF, {185>} $FF, $FF, $FF, $FF, $FF, // 180..189
      $FF, $FF, $FF, $FF, $FF, {195>} $FF, $FF, $FF, $FF, $FF, // 190..199
      $FF, $FF, $FF, $FF, $FF, {205>} $FF, $FF, $FF, $FF, $FF, // 200..209
      $FF, $FF, $FF, $FF, $FF, {215>} $FF, $FF, $FF, $FF, $FF, // 210..219
      $FF, $FF, $FF, $FF, $FF, {225>} $FF, $FF, $FF, $FF, $FF, // 220..229
      $FF, $FF, $FF, $FF, $FF, {235>} $FF, $FF, $FF, $FF, $FF, // 230..239
      $FF, $FF, $FF, $FF, $FF, {245>} $FF, $FF, $FF, $FF, $FF, // 240..249
      $FF, $FF, $FF, $FF, $FF, {255>} $FF                      // 250..255
    {$ENDIF}
  );
asm
  PUSH EBX
  MOV  ESI, [EAX]
  MOV  EDI, [ECX]
  {$IFDEF Check64}
    MOV  EAX, Size
    AND  EAX, $03
    CMP  EAX, $00
    JZ   @@DecodeStart
    JMP  @@ErrorDone
    @@DecodeStart:
  {$ENDIF}
  MOV  EAX, Size
  SHR  EAX, 2
  JZ   @@Done
  LEA  ECX, Base64Codec[0]
  XOR  EBX, EBX
  DEC  EAX
  JZ   @@LeftOver
  PUSH EBP
  MOV  EBP, EAX
  @@LoopStart:
    LODSD
    MOV  EDX, EAX
    MOV  BL, DL
    MOV  AH, BYTE PTR [ECX + EBX]
    {$IFDEF Check64}
      CMP  AH, $FF
      JZ   @@ErrorDoneAndPopEBP
    {$ENDIF}
    MOV  BL, DH
    MOV  AL, BYTE PTR [ECX + EBX]
    {$IFDEF Check64}
      CMP  AL, $FF
      JZ   @@ErrorDoneAndPopEBP
    {$ENDIF}
    SHL  AL, 2
    ROR  AX, 6
    STOSB
    SHR  AX, 12
    SHR  EDX, 16
    MOV  BL, DL
    MOV  AH, BYTE PTR [ECX + EBX]
    {$IFDEF Check64}
      CMP  AH, $FF
      JZ   @@ErrorDoneAndPopEBP
    {$ENDIF}
    SHL  AH, 2
    ROL  AX, 4
    MOV  BL, DH
    MOV  BL, BYTE PTR [ECX + EBX]
    {$IFDEF Check64}
      CMP  BL, $FF
      JZ   @@ErrorDoneAndPopEBP
    {$ENDIF}
    OR   AH, BL
    STOSW
    DEC  EBP
  JNZ  @@LoopStart
  POP  EBP
  @@LEFTOVER:
  LODSD
  MOV  EDX, EAX
  MOV  BL, DL
  MOV  AH, BYTE PTR [ECX + EBX]
  {$IFDEF Check64}
    CMP  AH, $FF
    JZ   @@ErrorDone
  {$ENDIF}
  MOV  BL, DH
  MOV  AL, BYTE PTR [ECX + EBX]
  {$IFDEF Check64}
    CMP  AL, $FF
    JZ   @@ErrorDone
  {$ENDIF}
  SHL  AL, 2
  ROR  AX, 6
  STOSB
  SHR  EDX, 16
  CMP  DL, Base64Filler
  JZ   @@SuccessDone
  SHR  AX, 12
  MOV  BL, DL
  MOV  AH, BYTE PTR [ECX + EBX]
  {$IFDEF Check64}
    CMP  AH, $FF
    JZ   @@ErrorDone
  {$ENDIF}
  SHL  AH, 2
  ROL  AX, 4
  STOSB
  CMP  DH, Base64Filler
  JZ   @@SuccessDone
  MOV  BL, DH
  MOV  BL, BYTE PTR [ECX + EBX]
  {$IFDEF Check64}
    CMP  BL, $FF
    JZ   @@ErrorDone
  {$ENDIF}
  OR   AH, BL
  MOV  AL, AH
  STOSB
  @@SuccessDone:
  {$IFDEF Check64}
    MOV  Result, $01
    JMP  @@Done
    @@ErrorDoneAndPopEBP:
    POP  EBP
    @@ErrorDone:
    MOV  Result, $00
  {$ENDIF}
  @@Done:
  POP  EBX
end;

procedure Base64Encode(const Text: PChar; var Output: PChar);
var
  Size, OutSize: Cardinal;
begin
  Size := Length(Text);
  OutSize := CalcEncodedSize(Size);
  Output := StrAlloc(Succ(OutSize));
  Output[OutSize] := #0;
  Base64Encode(Text, Size, Output);
end;

procedure Base64Encode(const Text: string; var Output: string);
var
  Size, OutSize: Cardinal;
  PIn, POut: Pointer;
begin
  Size := Length(Text);
  OutSize := CalcEncodedSize(Size);
  SetLength(Output, OutSize);
  PIn := @Text[1];
  POut := @Output[1];
  Base64Encode(PIn, Size, POut);
end;

procedure Base64Decode(const Text: PChar; var Output: PChar);
var
  Size, OutSize: Cardinal;
begin
  Size := Length(Text);
  OutSize := CalcDecodedSize(Text, Size);
  Output := StrAlloc(Succ(OutSize));
  Output[OutSize] := #0;
  {$IFDEF Quick64}
    Base64Decode(Text, Size, Output);
  {$ENDIF}
  {$IFDEF Check64}
    if not Base64Decode(Text, Size, Output) then
      Output[0] := #0;
  {$ENDIF}
end;

procedure Base64Decode(const Text: string; var Output: string);
var
  Size, OutSize: Cardinal;
  PIn, POut: Pointer;
begin
  Size := Length(Text);
  PIn := @Text[1];
  OutSize := CalcDecodedSize(PIn, Size);
  SetLength(Output, OutSize);
  FillChar(Output[1], OutSize, '.');
  POut := @Output[1];
  {$IFDEF Quick64}
    Base64Decode(PIn, Size, POut);
  {$ENDIF}
  {$IFDEF Check64}
    if not Base64Decode(PIn, Size, POut) then
      SetLength(Output, 0);
  {$ENDIF}
end;

{ Network time routines }

function TimeZoneBias: Integer;
var
  ZoneInfo: TTimeZoneInformation;
  Bias: Integer;
begin
  case GetTimeZoneInformation(ZoneInfo) of
    2: Bias := ZoneInfo.Bias + ZoneInfo.DaylightBias;
    1: Bias := ZoneInfo.Bias + ZoneInfo.StandardBias;
  else
    Bias := ZoneInfo.Bias;
  end;
  Result := Bias * -1;
end;

function TimeZone: string;
var
  Bias: Integer;
  H, M: Integer;
begin
  Bias := TimeZoneBias;
  if Bias >= 0 then
    Result := '+'
  else
    Result := '-';
  Bias := Abs(Bias);
  H := Bias div 60;
  M := Bias mod 60;
  Result := Result + Format('%.2d%.2d', [H, M]);
end;

function NetworkTime(const Time: TDateTime): string;
const
  DayNames: array[1..7] of string =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MonthNames: array[0..6, 1..12] of string = (
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
    ('jan', 'fv', 'mar', 'avr', 'mai', 'jun',
     'jul', 'ao', 'sep', 'oct', 'nov', 'dc'),
    ('jan', 'fev', 'mar', 'avr', 'mai', 'jun',
     'jul', 'aou', 'sep', 'oct', 'nov', 'dec'),
    ('Jan', 'Feb', 'Mar', 'Apr', 'Mai', 'Jun',
     'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'),
    ('Jan', 'Feb', 'Mr', 'Apr', 'Mai', 'Jun',
     'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'),
    ('Led', 'no', 'Be', 'Dub', 'Kv', 'en',
     'ec', 'Srp', 'Z', 'j', 'Lis', 'Pro'));
var
  Year, Month, Day: word;
begin
  DecodeDate(Time, Year, Month, Day);
  Result := Format('%s, %d %s %s %s', [DayNames[DayOfWeek(Time)], Day,
    MonthNames[1, Month], FormatDateTime('yyyy hh":"nn":"ss', Time), TimeZone]);
end;

end.
