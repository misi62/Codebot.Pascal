
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Base64;

interface

{$I CODEBOT.INC}
{$O+}

uses
  SysUtils;

{.DEFINE Quick64}

{$IFNDEF Quick64}
  {$DEFINE Check64}
{$ENDIF}

function Base64Encode(const Text: string): string; overload;
function Base64Decode(const Text: string): string; overload;

function CalcEncodedSize(Size: Cardinal): Cardinal;
function CalcDecodedSize(const Buffer; Size: Cardinal): Cardinal;

procedure Base64Encode(const Buffer; Size: Cardinal; var Output); overload;

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


implementation

const
  Base64Codec: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Base64Filler = '=';

function Base64Encode(const Text: string): string; overload;
begin
  Base64Encode(Text, Result);
end;

function Base64Decode(const Text: string): string; overload;
begin
  Base64Decode(Text, Result);
end;

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

end.