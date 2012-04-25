
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: February 2010                                 *)
(*                                                      *)
(********************************************************)

unit QuickString;

interface

type
  StringArray = array of string;

function QuickPos(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): Integer; overload;
function QuickPos(const SubStr, SearchStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
function QuickPosCount(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): Integer;
function QuickReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
function QuickReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
function QuickTrim(const S: string): string;
function QuickEquals(const S: string; const Strings: array of string): Boolean;
function QuickIndex(const S: string; const Strings: array of string): Integer;
function QuickSplit(const S: string; Separator: Char): StringArray;

implementation

type
  IntArray = array of Integer;

function QuickPosBuffer(SubStr, SearchStr: PChar; SubStrLen, SearchStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SearchStrLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(SearchStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(SubStr[I]);
    Lookup[B] := SubStrLen - I;
  end;
  Last := SubStr[SubStrLen];
  I := SubStrLen;
  while I <= SearchStrLen do
  begin
    Current := SearchStr[I];
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if SubStr[K] <> SearchStr[J + K] then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function QuickPosBufferI(SubStr, SearchStr: PChar; SubStrLen, SearchStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SearchStrLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(SearchStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(UpCase(SubStr[I]));
    Lookup[B] := SubStrLen - I;
  end;
  Last := UpCase(SubStr[SubStrLen]);
  I := SubStrLen;
  while I <= SearchStrLen do
  begin
    Current := UpCase(SearchStr[I]);
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if UpCase(SubStr[K]) <> UpCase(SearchStr[J + K]) then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function QuickTrim(const S: string): string;
var
  I, L, R: Integer;
begin
  L := 1;
  for I := 1 to Length(S) do
  begin
    L := I;
    if S[I] > ' ' then
      Break;
  end;
  R := Length(S);
  for I := Length(S) downto L do
  begin
    R := I;
    if S[I] > ' ' then
      Break;
  end;
  Result := Copy(S, L, R - L + 1);
end;

function QuickPos(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): Integer;
begin
  if IgnoreCase then
    Result := QuickPosBufferI(PChar(SubStr), PChar(SearchStr), Length(SubStr), Length(SearchStr))
  else
    Result := QuickPosBuffer(PChar(SubStr), PChar(SearchStr), Length(SubStr), Length(SearchStr));
end;

function QuickPos(const SubStr, SearchStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer;
var
  P: PChar;
  I: Integer;
begin
  P := PChar(SearchStr);
  I := Length(SearchStr);
  if (Start < 1) or (Start > I) then
  begin
    Result := 0;
    Exit;
  end;
  Dec(Start);
  Inc(P, Start);
  Dec(I, Start);
  if IgnoreCase then
    Result := QuickPosBufferI(PChar(SubStr), P, Length(SubStr), I)
  else
    Result := QuickPosBuffer(PChar(SubStr), P, Length(SubStr), I);
  if Result > 0 then
    Inc(Result, Start);
end;

function QuickPosCount(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): Integer;
var
  Start, Index: Integer;
begin
  Result := 0;
  Start := 1;
  repeat
    Index := QuickPos(SubStr, SearchStr, Start, IgnoreCase);
    if Index > 0 then
    begin
      Inc(Result);
      Start := Index + 1;
    end;
  until Index = 0;
end;

function QuickPosIndex(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): IntArray;
var
  Start, Index: Integer;
begin
  SetLength(Result, QuickPosCount(SubStr, SearchStr, IgnoreCase));
  Start := 1;
  Index := 0;
  while Index < Length(Result) do
  begin
    Start := QuickPos(SubStr, SearchStr, Start, IgnoreCase);
    Result[Index] := Start;
    Inc(Start);
    Inc(Index);
  end;
end;

function QuickReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  PosIndex: IntArray;
  Diff: Integer;
  I, J, K, L: Integer;
begin
  PosIndex := QuickPosIndex(OldPattern, S, IgnoreCase);
  if Length(PosIndex) = 0 then
  begin
    Result := S;
    Exit;
  end;
  Diff := Length(NewPattern) - Length(OldPattern);
  I := Length(S) + Diff * Length(PosIndex);
  SetLength(Result, I);
  I := 0;
  J := 1;
  K := 1;
  while K <= Length(S) do
  begin
    if K = PosIndex[I] then
    begin
      if I < High(PosIndex) then
        Inc(I);
      Inc(K, Length(OldPattern));
      for L := 1 to Length(NewPattern) do
      begin
        Result[J] := NewPattern[L];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := S[K];
      Inc(J);
      Inc(K);
    end;
  end;
end;

function QuickReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  I: Integer;
begin
  I := QuickPos(OldPattern, S, IgnoreCase);
  if I > 0 then
    Result := Copy(S, 1, I - 1) + NewPattern + Copy(S, I + Length(OldPattern), Length(S))
  else
    Result := S;
end;

function QuickEquals(const S: string; const Strings: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Strings) to High(Strings) do
    if S = Strings[I] then
    begin
      Result := True;
      Break;
    end;
end;

function QuickIndex(const S: string; const Strings: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Strings) to High(Strings) do
    if S = Strings[I] then
    begin
      Result := I;
      Break;
    end;
end;

function QuickSplit(const S: string; Separator: Char): StringArray;
var
  Splits: IntArray;
  Pos: Integer;
  I: Integer;
begin
  Result := nil;
  Splits := nil;
  if Length(S) < 1 then
    Exit;
  if QuickPos(Separator, S) < 1 then
  begin
    SetLength(Result, 1);
    Result[0] := S;
    Exit;
  end;
  Splits := QuickPosIndex(Separator, S);
  SetLength(Result, Length(Splits) + 1);
  Pos := 1;
  for I := Low(Splits) to High(Splits) do
  begin
    Result[I] := Copy(S, Pos, Splits[I] - Pos);
    Pos := Splits[I] + 1;
  end;
  Result[Length(Splits)] := Copy(S, Pos, Length(S));
end;

{function QuickPosBuffer(SubStr, SearchStr: PChar; SubStrLen, SearchStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SearchStrLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(SearchStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(SubStr[I]);
    Lookup[B] := SubStrLen - I;
  end;
  Last := SubStr[SubStrLen];
  I := SubStrLen;
  while I <= SearchStrLen do
  begin
    Current := SearchStr[I];
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if SubStr[K] <> SearchStr[J + K] then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function QuickPosBufferI(SubStr, SearchStr: PChar; SubStrLen, SearchStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SearchStrLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(SearchStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(UpCase(SubStr[I]));
    Lookup[B] := SubStrLen - I;
  end;
  Last := UpCase(SubStr[SubStrLen]);
  I := SubStrLen;
  while I <= SearchStrLen do
  begin
    Current := UpCase(SearchStr[I]);
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if UpCase(SubStr[K]) <> UpCase(SearchStr[J + K]) then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function QuickPos(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): Integer; overload;
begin
  if IgnoreCase then
    Result := QuickPosBufferI(PChar(SubStr), PChar(SearchStr), Length(SubStr), Length(SearchStr))
  else
    Result := QuickPosBuffer(PChar(SubStr), PChar(SearchStr), Length(SubStr), Length(SearchStr));
end;

function QuickPos(const SubStr, SearchStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
var
  P: PChar;
  I: Integer;
begin
  P := PChar(SearchStr);
  I := Length(SearchStr);
  if (Start < 1) or (Start > I) then
  begin
    Result := 0;
    Exit;
  end;
  Dec(Start);
  Inc(P, Start);
  Dec(I, Start);
  if IgnoreCase then
    Result := QuickPosBufferI(PChar(SubStr), P, Length(SubStr), I)
  else
    Result := QuickPosBuffer(PChar(SubStr), P, Length(SubStr), I);
  if Result > 0 then
    Inc(Result, Start);
end;

function QuickPosCount(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): Integer;
var
  Start, Index: Integer;
begin
  Result := 0;
  Start := 1;
  repeat
    Index := QuickPos(SubStr, SearchStr, Start, IgnoreCase);
    if Index > 0 then
    begin
      Inc(Result);
      Start := Index + 1;
    end;
  until Index = 0;
end;

type
  TPosIndex = array of Integer;

function QuickPosIndex(const SubStr, SearchStr: string; IgnoreCase: Boolean = False): TPosIndex;
var
  Start, Index: Integer;
begin
  SetLength(Result, QuickPosCount(SubStr, SearchStr, IgnoreCase));
  Start := 1;
  Index := 0;
  while Index < Length(Result) do
  begin
    Start := QuickPos(SubStr, SearchStr, Start, IgnoreCase);
    Result[Index] := Start;
    Inc(Start);
    Inc(Index);
  end;
end;

function QuickReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  PosIndex: TPosIndex;
  Diff: Integer;
  I, J, K, L: Integer;
begin
  PosIndex := QuickPosIndex(OldPattern, S, IgnoreCase);
  if Length(PosIndex) = 0 then
  begin
    Result := S;
    Exit;
  end;
  Diff := Length(NewPattern) - Length(OldPattern);
  I := Diff * Length(PosIndex);
  J :=  Length(S);
  K := J + I;
  SetLength(Result, K);
  I := 0;
  J := 0;
  K := 1;
  while K <= Length(S) do
  begin
    Inc(J);
    if K = PosIndex[I] then
    begin
      for L := 1 to Length(NewPattern) do
      begin
        Result[J] :=  NewPattern[L];
        Inc(J);
      end;
      Inc(K, Length(OldPattern));
      Dec(J);
      Inc(I);
    end
    else
    begin
      Result[J] :=  S[K];
      Inc(K);
    end;
  end;
end;}

end.
