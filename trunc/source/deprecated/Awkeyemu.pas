(***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Async Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1991-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{*                   AWKEYEMU.PAS 4.06                   *}
{*********************************************************}
{* Deprecated keyboard emulator                          *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$F+,X+}

unit AwKeyEmu;
  {-Abstract keyboard emulator and all derived emulators}

interface

uses
  WinTypes,
  WinProcs,
  SysUtils,
  AwIniDb,
  AwEmu,                                                          
  OoMisc;

function kbInitKeyEmulator(var P : PKeyEmulator; KeyFile: PChar) : Integer;
function kbNumEmuTypes(P : PKeyEmulator) : Integer;
function kbLoadKeyEmuIndex(var P: PKeyEmulator) : Integer;
function kbSetKeyEmuType(var P: PKeyEmulator; EmuType: PChar): Integer;
function kbAddKeyEmuRecord(var P: PKeyEmulator; var Rec): Integer;
function kbDelKeyEmuRecord(var P: PKeyEmulator; Key: PChar): Integer;
function kbUpdKeyEmuRecord(var P: PKeyEmulator): Integer;
function kbLoadKeyEmuMap(var P: PKeyEmulator): Integer;
procedure kbProcessKey(P : Pointer; Key: Word; var Command : TKeyEmuCommandLo); 
procedure kbDoneKeyEmulator(var P : PKeyEmulator);

implementation

function kbInitKeyEmulator(var P : PKeyEmulator; KeyFile: PChar) : Integer;
  {-Initialize a keyboard emulator}
var
  I    : Word;
  ErrorCode : Integer;
  Temp : array[0..3] of char;
  Key : array[0..6] of char;
label
  ExitPoint;
begin
  {initialize the KeyEmulator and allocate memory}
  P := AllocMem(SizeOf(TKeyEmulator));

  {initialize the default fields}
  with P^ do begin
    kbKeyFileName := KeyFile;
    kbKeyName[0] := #0;
    kbKEyNameList[0] := #0;
    kbProcessAll := False;
    kbProcessExt := False;
    kbKeyDataBase := nil;
  end;

  {initialize the Keyboard INI Database}
  iInitIniDatabase(P^.kbKeyDataBase, KeyFile);

  with P^ do begin
    ErrorCode := iAddIniDBStringField(kbKeyDataBase, ApdKeyIndexName, ApdKeyMapNameLen, True); {!!.03}
    if (ErrorCode < ecOK) then
      goto ExitPoint;

    StrPCopy(Key, 'Key');
    for I := 1 to ApdMaxKeyMaps do begin                               {!!.03}              
      ErrorCode := iAddIniDBStringField(kbKeyDataBase, StrCat(Key, Long2StrZ(Temp, I)),
                                        KeyMappingLen, False);
      StrPCopy(Key, 'Key');

      if (Errorcode < ecOK) then
        goto ExitPoint;
    end;

    ErrorCode := iPrepareIniDatabase(kbKeyDataBase, nil);
  end;

ExitPoint:
  kbInitKeyEmulator := ErrorCode;
  if (Errorcode < ecOK) then
    kbDoneKeyEmulator(P);
end;

function kbNumEmuTypes(P : PKeyEmulator) : Integer;
  {-Return the number of emulator types in a database }
begin
  kbNumEmuTypes := iNumIniRecs(P^.kbKeyDataBase);
end;

function kbLoadKeyEmuIndex(var P: PKeyEmulator) : Integer;
  {-Load the keyboard index into Buf }
var
  Buffer : PChar;
  BufferSize : Integer;
  ErrorCode : Integer;
  MoveSize : Integer;
label
  ExitPoint;
begin
  ErrorCode := iAllocIniIndexRec(P^.kbKeyDataBase, Buffer, BufferSize);
  if (ErrorCode < ecOK) then begin
    kbLoadKeyEmuIndex := ErrorCode;
    Exit;
  end;

  ErrorCode := iLoadIniIndex(P^.kbKeyDataBase, Buffer, BufferSize);
  if (ErrorCode < ecOK) then
    goto ExitPoint;

  {see if the buffer is larger than the record size}
  if BufferSize > (ApdKeyIndexMaxLen-1) then                           {!!.03}
    MoveSize := (ApdKeyIndexMaxLen-1)                                  {!!.03}
  else
    MoveSize := BufferSize;

  FillChar(P^.kbKeyNameList, ApdKeyIndexMaxLen+1, #0);                 {!!.03}
  Move(Buffer^, P^.kbKeyNameList, MoveSize);

 ExitPoint:
  kbLoadKeyEmuIndex := ErrorCode;
  iDeAllocIniIndexRec(P^.kbKeyDataBase, Buffer, BufferSize);
end;

function kbAddKeyEmuRecord(var P: PKeyEmulator; var Rec): Integer;
begin
  kbAddKeyEmuRecord := iAddIniRecord(P^.kbKeyDataBase, Rec);
end;

function kbDelKeyEmuRecord(var P: PKeyEmulator; Key: PChar): Integer;
begin
  kbDelKeyEmuRecord := iDelIniRecord(P^.kbKeyDataBase, Key);
end;

function kbUpdKeyEmuRecord(var P: PKeyEmulator): Integer;
var
  XFerRec : TKeyMapXFerRec;
  I : Word;
  MapString : String[ApdKeyMapNameLen];                                {!!.03}
begin
  FillChar(XFerRec, SizeOf(TKeyMapXFerRec), 0);
  for I := 1 to ApdMaxKeyMaps do begin                                 {!!.03}
    with P^ do begin
      if (kbKeyMap[I].KeyCode <> 0) and (kbKeyMap[I].Mapping <> '') then begin
        MapString := Copy(IntToStr(kbKeyMap[I].KeyCode)+','+
                          IntToStr(kbKeyMap[I].ShiftState)+','+
                          kbKeyMap[I].Mapping, 1, SizeOf(TKeyMapping));
        StrPCopy(XFerRec.Keys[I], MapString);
      end;
    end;
  end;
  XFerRec.Name := P^.kbKeyName;
  kbUpdKeyEmuRecord := iUpdIniRecord(P^.kbKeyDataBase, P^.kbKeyName, XFerRec);
end;

function kbSetKeyEmuType(var P: PKeyEmulator; EmuType: PChar): Integer;
begin
  StrCopy(P^.kbKeyName, EmuType);
  kbSetKeyEmuType := ecOK;
end;

function kbLoadKeyEmuMap(var P: PKeyEmulator): Integer;
var
  I : Integer;
  ErrorCode : Integer;
  XFerRec : TKeyMapXFerRec;

  procedure ConvertMapping(Value: String; Index: Word);
  var
    X    : Word;
  begin
    { Set the default values }
    P^.kbKeyMap[Index].KeyCode    := 0;
    P^.kbKeyMap[Index].ShiftState := 0;
    P^.kbKeyMap[Index].Mapping    := '';

    X := Pos(',', Value);
    if X > 0 then begin
      P^.kbKeyMap[Index].KeyCode := StrToInt(Copy(Value, 1, X-1));
      Value := Copy(Value, X+1, Length(Value));
      X := Pos(',', Value);
      if X > 1 then
        P^.kbKeyMap[Index].ShiftState := StrToInt(Copy(Value, 1, X-1));
      if X > 0 then
        Value := Copy(Value, X+1, Length(Value));
      P^.kbKeyMap[Index].Mapping := Value;
    end;
  end;

begin
  FillChar(P^.kbKeyMap, SizeOf(TKeyMapName), 0);
  ErrorCode := iGetIniRecord(P^.kbKeyDataBase, P^.kbKeyName, XFerRec);
  if (ErrorCode = ecOk) then begin
    for I := 1 to ApdMaxKeyMaps do begin                               {!!.03}
      ConvertMapping(StrPas(XFerRec.Keys[I]), I);
    end;
  end;
  kbLoadKeyEmuMap := ErrorCode;
end;

procedure kbProcessKey(P: Pointer; Key: Word; var Command: TKeyEmuCommandLo);
var
  I : Integer;
begin
  Command.KeyMapped := False;
  Command.Extended  := False;
  if not (Key in [VK_SHIFT..VK_MENU, VK_CAPITAL, VK_NUMLOCK]) then begin
    with Command do begin
      {set the KeyCode to match the current key}
      KeyCode := Key;

      {set the shift key mapping for current key state}
      ShiftMap := (Ord((Ord(KeyboardState[VK_CONTROL]) shr 1) = 0) * ksControl) +
                  (Ord((Ord(KeyboardState[VK_MENU])    shr 1) = 0) * ksAlt)     +
                  (Ord((Ord(KeyboardState[VK_SHIFT])   shr 1) = 0) * ksShift);

      {set the toggle key mapping for current key state}
      ToggleMap:= (Ord(not (Ord(KeyboardState[VK_CAPITAL])))       * tsCapital) +
                  (Ord(not (Ord(KeyboardState[VK_NUMLOCK])))       * tsNumlock) +
                  (Ord(not (Ord(KeyboardState[$91])))              * tsScroll);

      {is this considered an extended key}
      if Key in [VK_F1..VK_F12, VK_PRIOR..VK_DOWN] then
        Extended := True;
    end;

    with PKeyEmulator(P)^ do begin
      {do we want to process the extended keys}
      if Command.Extended and (not kbProcessExt) then
        Exit;
      {do we want to process ALL keys (0..1, A..Z)}
      if (Command.KeyCode in [$30..$39, $41..$5A]) and (not kbProcessAll) then
        Exit;
      {check keylist for matching keys}
      for I := 1 to ApdMaxKeyMaps do begin                             {!!.03}
        if kbKeyMap[I].KeyCode = Key then begin
          if kbKeyMap[I].ShiftState = Command.ShiftMap then begin
            Command.KeyMapped := True;
            Command.Value := kbKeyMap[I].Mapping;
          end;
        end;
      end;

    end;
  end;
end;

procedure kbDoneKeyEmulator(var P : PKeyEmulator);
  {-Destroy the emulator}
begin
  iDoneIniDatabase(P^.kbKeyDataBase);
  FreeMem(P, SizeOf(TKeyEmulator));
end;

end.
