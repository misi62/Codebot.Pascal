
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2007                   *)
(*                                                      *)
(********************************************************)

unit SendKey;

interface

{$I CODEBOT.INC}

uses
	SysUtils, Windows, Messages;

function TranslateKey(VKey: Word): string;

(* function SendKeys(SendKeysString: PChar; Wait: Boolean): Boolean;

Takes a PChar as its first parameter and a boolean as its second, like so:

	SendKeys('KeyString', Wait);
	SendKeys('abc123{left}{left}{left}def{end}456{left 6}ghi{end}789', True);

where KeyString is a string of key names and modifiers that you want
to send to the current input focus and Wait is a boolean variable or value
that indicates whether SendKeys should wait for each key message to be
processed before proceeding.  See the table below for more information.

SendKeys supports the Visual Basic SendKeys syntax, as documented below.

Supported modifiers:

  + = Shift
  ^ = Control
  & = Alt

Surround sequences of characters or key names with parentheses in order to
modify them as a group.  For example, '+abc' shifts only 'a', while '+(abc)' shifts
all three characters.

Supported special characters

  ~ = Enter
  ( = Begin modifier group (see above)
  ) = End modifier group (see above)
  { = Begin key name text (see below)
  } = End key name text (see below)

Supported characters:

  Any character that can be typed is supported.  Surround the modifier keys
  listed above with braces in order to send as normal text.

Supported key names (surround these with braces):

  BKSP, BS, BACKSPACE
  BREAK
  CAPSLOCK
  CLEAR
  DEL
  DELETE
  DOWN
  END
  ENTER
  ESC
  ESCAPE
  F1
  F2
  F3
  F4
  F5
  F6
  F7
  F8
  F9
  F10
  F11
  F12
  F13
  F14
  F15
  F16
  HELP
  HOME
  INS
  LEFT
  NUMLOCK
  PGDN
  PGUP
  PRTSC
  RIGHT
  SCROLLLOCK
  TAB
  UP

Follow the keyname with a space and a number to send the specified key a
given number of times (e.g., {left 6}). *)

procedure DisableKeys;
procedure EnableKeys;
function KeysSending: Boolean;

function SendKeys(SendKeysString: PChar; Wait: Boolean): Boolean;

implementation

{ SendKeys procedure

	Converts a string of characters and key names to keyboard events and
	passes them to Windows. }


type
  THKeys = array[0..Pred(MaxLongInt)] of Byte;

  WBytes = array[0..pred(SizeOf(Word))] of Byte;

  TSendKey = record
    Name: ShortString;
    VKey: Byte;
  end;

{ Array of keys that SendKeys recognizes.

  if you add to this list, you must be sure to keep it sorted alphabetically
  by Name because a binary search routine is used to scan it. }

const
  VK_SPACE = Ord(' ');
  VK_0 = Ord('0');
  VK_1 = Ord('1');
  VK_2 = Ord('2');
  VK_3 = Ord('3');
  VK_4 = Ord('4');
  VK_5 = Ord('5');
  VK_6 = Ord('6');
  VK_7 = Ord('7');
  VK_8 = Ord('8');
  VK_9 = Ord('9');
  VK_A = Ord('A');
  VK_B = Ord('B');
  VK_C = Ord('C');
  VK_D = Ord('D');
  VK_E = Ord('E');
  VK_F = Ord('F');
  VK_G = Ord('G');
  VK_H = Ord('H');
  VK_I = Ord('I');
  VK_J = Ord('J');
  VK_K = Ord('K');
  VK_L = Ord('L');
  VK_M = Ord('M');
  VK_N = Ord('N');
  VK_O = Ord('O');
  VK_P = Ord('P');
  VK_Q = Ord('Q');
  VK_R = Ord('R');
  VK_S = Ord('S');
  VK_T = Ord('T');
  VK_U = Ord('U');
  VK_V = Ord('V');
  VK_W = Ord('W');
  VK_X = Ord('X');
  VK_Y = Ord('Y');
  VK_Z = Ord('Z');
  VK_BACKQUOTE = $C0;
  VK_MINUS = $BD;
  VK_EQUALS = $BB;
  VK_LBRACKET = $DB;
  VK_RBRACKET = $DD;
  VK_BACKSLASH = $DC;
  VK_SEMICOLON = $BA;
  VK_QUOTE = $DE;
  VK_COMMA = $BC;
  VK_PERIOD = $BE;
  VK_FORWARDSLASH = $BF;

  NormalKeyRecord: array[0..48] of TSendKey = (
		(Name: '~'; VKey: VK_RETURN),
		(Name: Chr(VK_SPACE); VKey: VK_SPACE),
		(Name: Chr(VK_0); VKey: VK_0),
		(Name: Chr(VK_1); VKey: VK_1),
		(Name: Chr(VK_2); VKey: VK_2),
		(Name: Chr(VK_3); VKey: VK_3),
		(Name: Chr(VK_4); VKey: VK_4),
		(Name: Chr(VK_5); VKey: VK_5),
		(Name: Chr(VK_6); VKey: VK_6),
		(Name: Chr(VK_7); VKey: VK_7),
		(Name: Chr(VK_8); VKey: VK_8),
		(Name: Chr(VK_9); VKey: VK_9),
		(Name: Chr(VK_A); VKey: VK_A),
		(Name: Chr(VK_B); VKey: VK_B),
		(Name: Chr(VK_C); VKey: VK_C),
		(Name: Chr(VK_D); VKey: VK_D),
		(Name: Chr(VK_E); VKey: VK_E),
		(Name: Chr(VK_F); VKey: VK_F),
		(Name: Chr(VK_G); VKey: VK_G),
		(Name: Chr(VK_H); VKey: VK_H),
		(Name: Chr(VK_I); VKey: VK_I),
		(Name: Chr(VK_J); VKey: VK_J),
		(Name: Chr(VK_K); VKey: VK_K),
		(Name: Chr(VK_L); VKey: VK_L),
		(Name: Chr(VK_M); VKey: VK_M),
		(Name: Chr(VK_N); VKey: VK_N),
		(Name: Chr(VK_O); VKey: VK_O),
		(Name: Chr(VK_P); VKey: VK_P),
		(Name: Chr(VK_Q); VKey: VK_Q),
		(Name: Chr(VK_R); VKey: VK_R),
		(Name: Chr(VK_S); VKey: VK_S),
		(Name: Chr(VK_T); VKey: VK_T),
		(Name: Chr(VK_U); VKey: VK_U),
		(Name: Chr(VK_V); VKey: VK_V),
		(Name: Chr(VK_W); VKey: VK_W),
		(Name: Chr(VK_X); VKey: VK_X),
		(Name: Chr(VK_Y); VKey: VK_Y),
		(Name: Chr(VK_Z); VKey: VK_Z),
		(Name: Chr(VK_BACKQUOTE); VKey: VK_BACKQUOTE),
		(Name: Chr(VK_MINUS); VKey: VK_MINUS),
		(Name: Chr(VK_EQUALS); VKey: VK_EQUALS),
		(Name: Chr(VK_LBRACKET); VKey: VK_LBRACKET),
		(Name: Chr(VK_RBRACKET); VKey: VK_RBRACKET),
		(Name: Chr(VK_BACKSLASH); VKey: VK_BACKSLASH),
		(Name: Chr(VK_SEMICOLON); VKey: VK_SEMICOLON),
		(Name: Chr(VK_QUOTE); VKey: VK_QUOTE),
		(Name: Chr(VK_COMMA); VKey: VK_COMMA),
		(Name: Chr(VK_PERIOD); VKey: VK_PERIOD),
		(Name: Chr(VK_FORWARDSLASH); VKey: VK_FORWARDSLASH));

  SpecialKeyRecord: array[0..41] of TSendKey = (
   (Name:'BACK'; VKey: VK_BACK),
   (Name:'BKSPACE'; VKey: VK_BACK),
   (Name:'BS'; VKey: VK_BACK),
   (Name:'BREAK'; VKey: VK_CANCEL),
   (Name:'CAPSLOCK'; VKey: VK_CAPITAL),
   (Name:'CLEAR'; VKey: VK_CLEAR),
   (Name:'DEL'; VKey: VK_DELETE),
   (Name:'DELETE'; VKey: VK_DELETE),
   (Name:'DOWN'; VKey: VK_DOWN),
   (Name:'END'; VKey: VK_END),
   (Name:'ENTER'; VKey: VK_RETURN),
   (Name:'ESC'; VKey: VK_ESCAPE),
   (Name:'ESCAPE'; VKey: VK_ESCAPE),
   (Name:'F1'; VKey: VK_F1),
   (Name:'F10'; VKey: VK_F10),
   (Name:'F11'; VKey: VK_F11),
   (Name:'F12'; VKey: VK_F12),
   (Name:'F13'; VKey: VK_F13),
   (Name:'F14'; VKey: VK_F14),
   (Name:'F15'; VKey: VK_F15),
   (Name:'F16'; VKey: VK_F16),
   (Name:'F2'; VKey: VK_F2),
   (Name:'F3'; VKey: VK_F3),
   (Name:'F4'; VKey: VK_F4),
   (Name:'F5'; VKey: VK_F5),
   (Name:'F6'; VKey: VK_F6),
   (Name:'F7'; VKey: VK_F7),
   (Name:'F8'; VKey: VK_F8),
   (Name:'F9'; VKey: VK_F9),
   (Name:'HELP'; VKey: VK_HELP),
   (Name:'HOME'; VKey: VK_HOME),
   (Name:'INS'; VKey: VK_INSERT),
   (Name:'LEFT'; VKey: VK_LEFT),
   (Name:'NUMLOCK'; VKey: VK_NUMLOCK),
   (Name:'PAUSE'; VKey: VK_PAUSE),
   (Name:'PGDN'; VKey: VK_NEXT),
   (Name:'PGUP'; VKey: VK_PRIOR),
   (Name:'PRTSC'; VKey: VK_PRINT),
   (Name:'RIGHT'; VKey: VK_RIGHT),
   (Name:'SCROLLLOCK'; VKey: VK_SCROLL),
   (Name:'TAB'; VKey: VK_TAB),
   (Name:'UP'; VKey: VK_UP));

function TranslateKey(VKey: Word): string;
var
  KeyState: TKeyBoardState;
  Modifiers: string;
	I: Integer;
begin
	Modifiers := '';
  GetKeyboardState(KeyState);
  if KeyState[VK_SHIFT] and $80 <> 0 then
		Modifiers := '+';
  if KeyState[VK_CONTROL] and $80 <> 0 then
		Modifiers := Modifiers + '^';
  if KeyState[VK_MENU] and $80 <> 0 then
		Modifiers := Modifiers + '&';
	Result := '';
	for I := Low(NormalKeyRecord) to High(NormalKeyRecord) do
  	if NormalKeyRecord[I].VKey = VKey then
    begin
			Result := Modifiers + LowerCase(NormalKeyRecord[I].Name);
      Exit;
    end;
	for I := Low(SpecialKeyRecord) to High(SpecialKeyRecord) do
  	if SpecialKeyRecord[I].VKey = VKey then
    begin
			Result := Modifiers + '{' + SpecialKeyRecord[I].Name + '}';
      Exit;
    end;
end;

const
  ExtendedVKeys: set of Byte = [
		VK_UP,
		VK_DOWN,
		VK_LEFT,
		VK_RIGHT,
		VK_HOME,
		VK_END,
		VK_PRIOR,
		VK_NEXT,
		VK_INSERT,
		VK_BACK,
		VK_DELETE];

const
  INVALIDKEY = $FFFF;
  VKKEYSCANSHIFTON = $01;
  VKKEYSCANCTRLON = $02;
  VKKEYSCANALTON = $04;

var
	SendEnabled: Boolean = True;
  SendRunning: Boolean = False;

procedure DisableKeys;
begin
	SendEnabled := False;
end;

procedure EnableKeys;
begin
	SendEnabled := True;
end;

function KeysSending: Boolean;
begin
	Result := SendRunning;
end;

function SendKeys(SendKeysString: PChar; Wait: Boolean): Boolean;
var
  UsingParens, ShiftDown, ControlDown, AltDown, FoundClose: Boolean;
  PosSpace: Byte;
  I, L: Integer;
  NumTimes, MKey: Word;
  KeyString: string[20];

	function BitSet(BitTable, BitMask: Byte): Boolean;
	begin
	  Result := ByteBool(BitTable and BitMask);
	end;

	procedure SetBit(var BitTable: Byte; BitMask: Byte);
	begin
	  BitTable := BitTable or Bitmask;
	end;

	procedure KeyboardEvent(VKey, ScanCode: Byte; Flags: Longint);
	var
	  KeyboardMsg: TMsg;
	begin
  	if not SendEnabled then Exit;
		keybd_event(VKey, ScanCode, Flags, 0);
	  if Wait then
	  	while PeekMessage(KeyboardMsg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do
		  begin
		    TranslateMessage(KeyboardMsg);
		    DispatchMessage(KeyboardMsg);
		  end;
	end;

	procedure SendKeyDown(VKey: Byte; NumTimes: Word; GenUpMsg: Boolean);
	var
	  Count: Word;
	  ScanCode: Byte;
	  NumState: Boolean;
	  KeyBoardState: TKeyboardState;
	begin
  	if not SendEnabled then Exit;
	  if (VKey = VK_NUMLOCK) then
	  begin
	    NumState := ByteBool(GetKeyState(VK_NUMLOCK) and 1);
	    GetKeyBoardState(KeyBoardState);
	    if NumState then
	    	KeyBoardState[VK_NUMLOCK] := (KeyBoardState[VK_NUMLOCK] and not 1)
	    else
      	KeyBoardState[VK_NUMLOCK] := (KeyBoardState[VK_NUMLOCK] or 1);
	    SetKeyBoardState(KeyBoardState);
	    Exit;
	  end;
	  ScanCode := Lo(MapVirtualKey(VKey,0));
    for Count := 1 to NumTimes do
	    if VKey in ExtendedVKeys then
      begin
	      KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY);
	      if(GenUpMsg) then
	        KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP)
	    end
      else
      begin
	      KeyboardEvent(VKey, ScanCode, 0);
				if GenUpMsg then
        	KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP);
	    end;
	end;

	procedure SendKeyUp(VKey: Byte);
	var
	  ScanCode: Byte;
	begin
  	if not SendEnabled then Exit;
	  ScanCode := Lo(MapVirtualKey(VKey, 0));
	  if VKey in ExtendedVKeys then
	    KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY and KEYEVENTF_KEYUP)
	  else
      KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP);
	end;

	procedure SendKey(MKey: Word; NumTimes: Word; GenDownMsg: Boolean);
	begin
  	if not SendEnabled then Exit;
	  if (BitSet(Hi(MKey), VKKEYSCANSHIFTON)) then SendKeyDown(VK_SHIFT, 1, False);
	  if (BitSet(Hi(MKey), VKKEYSCANCTRLON)) then SendKeyDown(VK_CONTROL, 1, False);
	  if (BitSet(Hi(MKey), VKKEYSCANALTON)) then SendKeyDown(VK_MENU, 1, False);
	  SendKeyDown(Lo(MKey), NumTimes, GenDownMsg);
	  if (BitSet(Hi(MKey), VKKEYSCANSHIFTON)) then SendKeyUp(VK_SHIFT);
	  if (BitSet(Hi(MKey), VKKEYSCANCTRLON)) then SendKeyUp(VK_CONTROL);
	  if (BitSet(Hi(MKey), VKKEYSCANALTON)) then SendKeyUp(VK_MENU);
	end;

	{ Implements a simple binary search to locate special key name strings }

	function StringToVKey(KeyString: ShortString): Word;
  var
    L, H, I, C: Integer;
  begin
    Result := INVALIDKEY;
  	if not SendEnabled then Exit;
    L := Low(SpecialKeyRecord);
    H := High(SpecialKeyRecord);
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := AnsiCompareText(SpecialKeyRecord[I].Name, KeyString);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
  	      Result := SpecialKeyRecord[I].VKey;
          Break;
        end;
      end;
    end;
  end;

	procedure PopUpShiftKeys;
	begin
  	if not SendEnabled then Exit;
	  if not UsingParens then
    begin
	    if ShiftDown then SendKeyUp(VK_SHIFT);
	    if ControlDown then SendKeyUp(VK_CONTROL);
	    if AltDown then SendKeyUp(VK_MENU);
	    ShiftDown := False;
	    ControlDown := False;
	    AltDown := False;
	  end;
	end;

begin
  Result := False;
	if not SendEnabled then
  	Exit;
  SendRunning := True;
  try
	  UsingParens := False;
	  ShiftDown := False;
	  ControlDown := False;
	  AltDown := False;
	  I := 0;
	  L := StrLen(SendKeysString);
	  if L = 0 then
	    Exit;
	  while I < L do
	  begin
	  	if not SendEnabled then Exit;
	    case SendKeysString[I] of
	      '(':
	        begin
	            UsingParens := True;
	            Inc(I);
	          end;
	      ')':
	        begin
	            UsingParens := False;
	            PopUpShiftKeys;
	            Inc(I);
	          end;
	      '&':
	        begin
	             AltDown := True;
	             SendKeyDown(VK_MENU,1,False);
	             Inc(I);
	          end;
	      '+':
	         begin
	             ShiftDown := True;
	             SendKeyDown(VK_SHIFT,1,False);
	             Inc(I);
	           end;
	      '^':
	        begin
	          ControlDown := True;
	          SendKeyDown(VK_CONTROL,1,False);
	          Inc(I);
	        end;
	      '{':
	        begin
	          NumTimes := 1;
	          if SendKeysString[Succ(I)]= '{' then
	          begin
	            MKey := VK_LBRACKET;
	            SetBit(Wbytes(MKey)[1], VKKEYSCANSHIFTON);
	            SendKey(MKey, 1, True);
	            PopUpShiftKeys;
	            Inc(I, 3);
	            Continue;
	          end;
	          KeyString := '';
	          FoundClose := False;
	          while I <= L do
	          begin
	            Inc(I);
	            if(SendKeysString[I]='}') then
	            begin
	              FoundClose := True;
	              Inc(I);
	              Break;
	            end;
	            KeyString := KeyString+Upcase(SendKeysString[I]);
	          end;
	          if not FoundClose then
	            Exit;
	          if SendKeysString[I] = '}' then
	          begin
	            MKey := VK_RBRACKET;
	            SetBit(Wbytes(MKey)[1],VKKEYSCANSHIFTON);
	            SendKey(MKey,1,True);
	            PopUpShiftKeys;
	            Inc(I);
	            Continue;
	          end;
	          PosSpace := Pos(' ', KeyString);
	          if(PosSpace<>0) then begin
	             NumTimes := StrToInt(Copy(KeyString, Succ(PosSpace), Length(KeyString) - PosSpace));
	             KeyString := Copy(KeyString, 1, Pred(PosSpace));
	          end;
	          if Length(KeyString) = 1 then
	            MKey := vkKeyScanA(KeyString[1])
	          else
	            MKey := StringToVKey(KeyString);
	          if MKey = VK_PAUSE then
	            Sleep(250)
	          else if MKey <> INVALIDKEY then
	          begin
	            SendKey(MKey, NumTimes, True);
	            PopUpShiftKeys;
	            Continue;
	          end;
	        end;
	      '~':
	        begin
	          SendKeyDown(VK_RETURN, 1, True);
	          PopUpShiftKeys;
	          Inc(I);
	        end;
	    else
	      MKey := vkKeyScan(SendKeysString[I]);
	      if MKey <> INVALIDKEY then
	      begin
	       SendKey(MKey, 1, True);
	       PopUpShiftKeys;
	      end;
	      Inc(I);
	    end;
	  	if not SendEnabled then Exit;
		end;
	  Result := True;
	  PopUpShiftKeys;
  finally
	  SendRunning := False;
  end;
end;

end.
