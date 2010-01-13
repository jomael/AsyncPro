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
{*                    AWEMU.PAS 4.06                     *}
{*********************************************************}
{* Deprecated terminal emulation                         *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$F+}

unit AwEmu;
  {-Abstract emulator and all derived emulators}

interface

uses
  WinTypes,
  WinProcs,
  SysUtils,
  OoMisc;

type
  TEmuCommandLo = record
    Ch      : Char;                 {Character}
    Cmd     : Byte;                 {Command}
    X, Y    : Byte;                 {Coordinates}
    FColor  : Byte;                 {Foreground color}
    BColor  : Byte;                 {Background color}
    ExtAttr : Byte;                 {Extended attributes}
    EmuType : Byte;                 {Emulator Type}
    {BCB cannot handle a small string in a variant record}
    case Byte of
      1 : (Other  : array[1..MaxOther] of Byte);  {Other data}
    {$IFDEF AProBCB}
      2 : (OtherStrLen : Byte;
           OtherStr  : array[1..10] of Char);  {Other data}
    {$ELSE}
      2 : (OtherStr : String[10]);
    {$ENDIF}
  end;

  {Emulator callback function type}
  TProcessCharProcLo = procedure(PAnsiEmulator: Pointer; C: Char;
                                 var Command: TEmuCommandLo);

  TKeyEmuCommandLo = record
    KeyCode       : Word;
    ShiftMap      : Word;
    ToggleMap     : Word;
    Extended      : Bool;
    KeyMapped     : Bool;
    KeyboardState : TKeyBoardState;
    Value         : string[KeyMappingLen];
  end;

  TProcessKeyProcLo = procedure(PKeyEmulator: Pointer; Key: Word;
                                var Command: TKeyEmuCommandLo);

function aeInitAnsiEmulator(var P : PAnsiEmulator) : Bool;
procedure aeDoneAnsiEmulator(var P : PAnsiEmulator);
procedure aeProcessAnsiChar(P : Pointer; C : Char;
                            var Command : TEmuCommandLo);

procedure aeOptionsOn(P : PAnsiEmulator; Options : Word);
procedure aeOptionsOff(P : PAnsiEmulator; Options : Word);
function  aeOptionsAreOn(P : PAnsiEmulator; Options : Word) : Bool;

const
  {Default colors}
  aweDefForeground = emWhite;
  aweDefBackground = emBlack;

implementation

const
  {Special parser characters for ANSI escape sequences}
  Escape = #27;
  LeftBracket = #91;
  Semicolon = #59;
  FormFeed = #12;

  {Bit settings for emuAttr}
  attrBlink      = $01;
  attrInverse    = $02;
  attrIntense    = $04;
  attrInvisible  = $08;
  attrUnderline  = $10;

  {Emulator types}
  etNone      = 0;
  etANSI      = 1;
  etVT52      = 2;
  etVT100     = 3;
  etANSIBBS   = 4;

  procedure aeOptionsOn(P : PAnsiEmulator; Options : Word);
    {-Enable options}
  begin
    with P^ do
      if Options and teMapVT100 <> 0 then
        emuType := etVT100
      else
        emuType := etANSIBBS;
  end;

  procedure aeOptionsOff(P : PAnsiEmulator; Options : Word);
    {-Disable options}
  begin
    with P^ do
      if Options and teMapVT100 <> 0 then
        emuType := etANSIBBS
      else
        emuType := etVT100;
  end;

  function  aeOptionsAreOn(P : PAnsiEmulator; Options : Word) : Bool;
    {-Return True if all bits in Options are currently set}
  begin
    with P^ do
      if Options and teMapVT100 <> 0 then
        aeOptionsAreOn := emuType = etVT100
      else
        aeOptionsAreOn := False;
  end;

  function aeInitAnsiEmulator(var P : PAnsiEmulator) : Bool;
    {-Initialize an ansi emulator}
  begin
    P := AllocMem(SizeOf(TAnsiEmulator));
    aeInitAnsiEmulator := True;
    with P^ do begin
      emuFlags := 0;
      emuFirst := True;
      emuAttr  := 0;
      emuIndex := 0;
      emuParamIndex := 1;
      FillChar(emuParamStr, SizeOf(emuParamStr), 0);
      emuParserState := GotNone;
    end;
  end;

  procedure aeDoneAnsiEmulator(var P : PAnsiEmulator);
    {-Destroy the emulator}
  begin
    FreeMem(P, SizeOf(TAnsiEmulator));
  end;

  procedure aePutQueue(P : PAnsiEmulator; C : Char);
    {-Save characters}
  begin
    with P^ do begin
      if emuIndex < MaxQueue then begin
        Inc(emuIndex);
        emuQueue[emuIndex] := C;
      end;
    end;
  end;

  procedure aeInitParser(P : PAnsiEmulator);
    {-Reset parser state}
  begin
    with P^ do begin
      emuParamIndex := 1;
      FillChar(emuParamStr, SizeOf(emuParamStr), 0);
      emuParserState := GotNone;
      emuIndex := 0;
    end;
  end;

  procedure aeBuildParam(P : PAnsiEmulator; C : Char);
    {-Build a param string}
  begin
    with P^ do
      emuParamStr[emuParamIndex] := emuParamStr[emuParamIndex] + C;
  end;

  procedure aeConvertParams(P : PAnsiEmulator; C : Char);
    {-Convert param strings to integers}
  var
    I, Code : Integer;
  begin
    with P^ do begin
      for I := 1 to MaxParams do begin
        Val(emuParamStr[I], emuParamInt[I], Code);
        if Code <> 0 then
          emuParamInt[I] := 1;

        {-handle default parameters for default modes}
        if (Length(emuParamStr[1]) = 0) then begin
          if (C in ['J', 'K']) then
            if (emuType = etANSI) or (emuType = etVT100) or
               (emuType = etANSIBBS) then
              emuParamInt[1] := 0
            else
              emuParamInt[1] := 2
          {-if no paramater for the SGR state, then set it to 0}
          else if (C = 'm') then
            emuParamInt[1] := 0;
        end;
      end;
    end;
  end;

  procedure ProcessANSIEmulation(P : PAnsiEmulator; var Command: TEmuCommandLo);
  var
    I : Byte;
  begin
    with P^, Command do begin
      case Ch of
        'A' :
          begin
           {Cursor up}
            Cmd := eCUU; {eUp}
            Y := emuParamInt[1];
          end;
        'B' :
          begin
            {Cursor down}
            Cmd := eCUD; {eDown, eVPR}
            Y := emuParamInt[1];
          end;
        'a' ,
        'C' :
          begin
            {Cursor right}
            Cmd := eCUF; {eRight, eHPR}
            X := emuParamInt[1];
          end;
        'D' :
          begin
            {Cursor left}
            cmd := eCUB; {eLeft}
            X := emuParamInt[1];
          end;
        'E' :
          begin
            {Cursor next line}
            Cmd := eCNL;
            Y := emuParamInt[1];
          end;
        'F' :
          begin
            {Cursor preceding line}
            Cmd := eCPL;
            Y := emuParamInt[1];
          end;
        'G', '`' :
          begin
            {Cursor horizontal absolute}
            Cmd := eCHA; {eHPA}
            X := emuParamInt[1];
          end;
        'H' :
          begin
            {Cursor position}
            Cmd := eCUP;{eGotoXY, eHVP}
            X   := emuParamInt[2];
            Y   := emuParamInt[1];
          end;
        'I' :
          begin
            {Cursor horizontal tabulation}
            Cmd := eCHT;
            X := emuParamInt[1];
          end;
        'J' :
          begin
            {Erase in Display}
            Cmd := eED;
            X := emuParamInt[1];
            Y := 0;
          end;
       'K' :
         begin
           {Erase in line}
           X := emuParamInt[1];
           Y := 1;
           Cmd := eEL;
         end;
        'L' :
          begin
            {Insert line}
            Cmd := eIL;
            Y := emuParamInt[1];
          end;
        'M' :
          begin
            {Delete line}
            Cmd := eDL;
            Y := emuParamInt[1];
          end;
        'N' :
          begin
            {Erase in field}
            Cmd := eEF;
            X := emuParamInt[1];
          end;
        'O' :
          begin
            {Erase in area}
            Cmd := eEA;
            X := emuParamInt[1];
          end;
        'P' :
          begin
            {Delete character}
            Cmd := eDCH;
            X := emuParamInt[1];
          end;
        'Q' :
          begin
            {selected editing extent mode}
            Cmd := eSEM;
            X := emuParamInt[1];
          end;
        'R' :
          begin
            {cursor position report}
            Cmd := eCPR;
            X := emuParamInt[2];
            Y := emuParamInt[1];
          end;
        'S' :
          begin
            {Scroll up}
            Cmd := eSU;
            Y := emuParamInt[1];
          end;
        'T' :
          begin
            {Scroll down}
            Cmd := eSD;
            Y := emuParamInt[1];
          end;
        'U' :
          begin
            {Next page}
            Cmd := eNP;
            Y := emuParamInt[1];
          end;
        'V' :
          begin
            {Preceding page}
            Cmd := ePP;
            Y := emuParamInt[1];
          end;
        'W' :
          begin
            {Cursor tabulation control}
            Cmd := eCTC;
            X := emuParamInt[1];
          end;
        'X' :
          begin
            {Erase character}
            Cmd := eECH;
            X := emuParamInt[1];
          end;
        'Y' :
          begin
            {Cursor vertical tabulation}
            Cmd := eCVT;
            Y := emuParamInt[1];
          end;
        'Z' :
          begin
            {Cursor backward tabulation}
            Cmd := eCBT;
            X := emuParamInt[1];
          end;
        'b' :
          begin
            {Repeat}
            Cmd := eREP;
            X := emuParamInt[1];
          end;
        'c' :
          begin
            {Device attributes}
            Cmd := eDA;
            X := emuParamInt[1];
          end;
        'd' :
          begin
            {Vertical position absolute}
            Cmd := eVPA;
            Y := emuParamInt[1];
          end;
        'e' :
          begin
            {Vertical position relative}
            Cmd := eVPR;
            Y := emuParamInt[1];
          end;
        'f' :
          begin
            {Horizontal and vertical position}
            Cmd := eHVP;{eGotoXY, eCUP}
            X   := emuParamInt[2];
            Y   := emuParamInt[1];
          end;
        'g' :
          begin
            {Tabulation clear}
            Cmd := eTBC;
            X := emuParamInt[1];
          end;
        'h' :
          begin
            {Set Mode}
            Cmd := eSM; {eSetMode}
            X   := emuParamInt[1];
          end;
        'i' :
          begin
            {Media Copy}
            Cmd := eMC;
            X := emuParamInt[1];
          end;
        'l' :
          begin
            {Reset mode}
            Cmd := eRM;
            X := emuParamInt[1];
          end;
        'm' :
          begin
            {Select graphics rendition}
            Cmd := eSGR;
            for I := 1 to emuParamIndex do begin
              {Process the color command}
              case emuParamInt[I] of
                0  :
                  begin
                    FColor := emWhite;
                    BColor := emBlack;
                    emuAttr := 0;
                    ExtAttr := 0;
                  end;
                1  : SetByteFlag(emuAttr, eattrIntense);
                4  : SetByteFlag(emuAttr, eattrUnderline);
                5  : SetByteFlag(emuAttr, eattrBlink);
                7  : SetByteFlag(emuAttr, eattrInverse);
                8  : SetByteFlag(emuAttr, eattrInvisible);

                22 : ClearByteFlag(emuAttr, eattrIntense);
                24 : ClearByteFlag(emuAttr, eattrUnderline);
                25 : ClearByteFlag(emuAttr, eattrBlink);
                27 : ClearByteFlag(emuAttr, eattrInverse);
                28 : ClearByteFlag(emuAttr, eattrInvisible);

                30 : FColor := emBlack;      {Black foreground}
                31 : FColor := emRed;        {Red foreground}
                32 : FColor := emGreen;      {Green foreground}
                33 : FColor := emYellow;     {Yellow forground}
                34 : FColor := emBlue;       {Blue foreground}
                35 : FColor := emMagenta;    {Magenta foreground}
                36 : FColor := emCyan;       {Cyan foreground}
                37 : FColor := emWhite;      {White foreground}
                39 : FColor := aweDefForeGround;{Default foreground}

                40 : BColor := emBlack;      {Black background}
                41 : BColor := emRed;        {Red background}
                42 : BColor := emGreen;      {Green background}
                43 : BColor := emYellow;     {Yellow background}
                44 : BColor := emBlue;       {Blue background}
                45 : BColor := emMagenta;    {Magenta background}
                46 : BColor := emCyan;       {Cyan background}
                47 : BColor := emWhite;      {White background}
                49 : BColor := aweDefBackGround;{Default background}
              end;
            end;
            if ByteFlagIsSet(emuAttr, eattrIntense) and (FColor<8) then
              FColor := FColor+8;
            ExtAttr := emuAttr;
          end;
        'n' :
          begin
            {Device Status Report}
            Cmd := eDSR;
            X := emuParamInt[1];
          end;
        'o' :
          begin
            {Define area qualification}
            Cmd := eDAQ;
            X := emuParamInt[1];
          end;
        's' : Cmd := eSaveCursorPos;
        'u' : Cmd := eRestoreCursorPos;
        '@' :
          begin
            {Insert character}
            Cmd := eICH;
            X := emuParamInt[1];
          end;
      end;
    end;
  end;

  procedure ProcessVT100Emulation(P : PAnsiEmulator; var Command: TEmuCommandLo); 
  var
    I : Byte;
  begin
    with P^, Command do begin
     case Ch of
       'A' :
         begin
           {Cursor up}
           Cmd := eCUU; {eUp}
           Y := emuParamInt[1];
         end;
       'B' :
         begin
           {Cursor down}
           Cmd := eCUD; {eDown}
           Y := emuParamInt[1];
         end;
       'C' :
         begin
           {Cursor right}
           Cmd := eCUF; {eRight}
           X := emuParamInt[1];
         end;
       'D' :
         begin
           {Cursor left}
           cmd := eCUB; {eLeft}
           X := emuParamInt[1];
         end;
       'H' :
         begin
           {cursor position}
           Cmd := eCUP;{eGotoXY, eHVP}
           X   := emuParamInt[2];
           Y   := emuParamInt[1];
         end;
       'I' :
         begin
            X := 0;
           case emuParamInt[1] of
             2  : Cmd := eKAM; {Keyboard action mode}
             3  : Cmd := eCRM; {control representation mode}
             4  : Cmd := eIRM; {Inseration-Replacement mode}
             20 : Cmd := eLNM; {Line feed / new line mode}
           end;
         end;
       'J' :
         begin
           {Erase in display}
           X := emuParamInt[1];
           Y := 1;
           Cmd := eED;
         end;
       'K' :
         begin
           {Erase in line}
           X := emuParamInt[1];
           Y := 0;
           Cmd := eEL;
         end;
       'L' :
         begin
           {Insert line}
           Cmd := eIL;
           Y := emuParamInt[1];
         end;
       'M' :
         begin
           {Delete line}
           Cmd := eDL;
           Y := emuParamInt[1];
         end;
       'R' :
         begin
           {Cursor position report}
           Cmd := eCPR;
           X := emuParamInt[2];
           Y := emuParamInt[1];
         end;
       'c' :
         begin
           {Device attributes}
           case emuParamInt[1] of
             0 :
               begin
                 Cmd := eDA;
                 X := 0;
                 Y := 1;
               end;
           end;
         end;
       'f' :
         begin
           {Horizontal-Vertical position}
           Cmd := eHVP;{eGotoXY, eCUP}
           X   := emuParamInt[2];
           Y   := emuParamInt[1];
         end;
       'h' :
         begin
           X := 1;
           case emuParamInt[1] of
             2  : Cmd := eKAM; {Keyboard action mode}
             3  : Cmd := eCRM; {control representation mode}
             4  : Cmd := eIRM; {Insertation-replacement mode}
             20 : Cmd := eLNM; {Line feed / New line mode}
           end;
         end;
       'i' :
         begin
           {Media copy}
           Cmd := eMC;
           X := emuParamInt[1];
         end;
       'm' :
         begin
           {Select graphics rendition}
           Cmd := eSGR;
           emuAttr := 0;
           for I := 1 to emuParamIndex do begin
             {Process the color command}
             case emuParamInt[I] of
               0  :
                 begin
                   FColor := emWhite;
                   BColor := emBlack;
                   emuAttr := 0;
                   ExtAttr := 0;
                 end;
               1  : SetByteFlag(emuAttr, eattrIntense);
               4  : SetByteFlag(emuAttr, eattrUnderline);
               5  : SetByteFlag(emuAttr, eattrBlink);
               7  : SetByteFlag(emuAttr, eattrInverse);
               8  : SetByteFlag(emuAttr, eattrInvisible);

               22 : ClearByteFlag(emuAttr, eattrIntense);
               24 : ClearByteFlag(emuAttr, eattrUnderline);
               25 : ClearByteFlag(emuAttr, eattrBlink);
               27 : ClearByteFlag(emuAttr, eattrInverse);
               28 : ClearByteFlag(emuAttr, eattrInvisible);

               30 : FColor := emBlack;      {Black foreground}
               31 : FColor := emRed;        {Red foreground}
               32 : FColor := emGreen;      {Green foreground}
               33 : FColor := emYellow;     {Yellow forground}
               34 : FColor := emBlue;       {Blue foreground}
               35 : FColor := emMagenta;    {Magenta foreground}
               36 : FColor := emCyan;       {Cyan foreground}
               37 : FColor := emWhite;      {White foreground}
               39 : FColor := aweDefForeGround;{Default foreground}

               40 : BColor := emBlack;      {Black background}
               41 : BColor := emRed;        {Red background}
               42 : BColor := emGreen;      {Green background}
               43 : BColor := emYellow;     {Yellow background}
               44 : BColor := emBlue;       {Blue background}
               45 : BColor := emMagenta;    {Magenta background}
               46 : BColor := emCyan;       {Cyan background}
               47 : BColor := emWhite;      {White background}
               49 : BColor := aweDefBackGround;{Default background}
             end;
           end;
           if ByteFlagIsSet(emuAttr, eattrIntense) and (FColor<8) then
             FColor := FColor+8;
           ExtAttr := emuAttr;
         end;
       'n' :
         begin
          {Device status report}
           Cmd := eDSR;
           X := emuParamInt[1];
         end;
       'q' :
         begin
          {LEDs -- not supported}
           Cmd := eNone;
         end;                                                       
       'r' :
         begin
           {Dec private/set top/bottom margins}
           Cmd := eDECSTBM;
           X   := emuParamInt[1];
           Y   := emuParamInt[2];
         end;
      end;
    end;
  end;

  procedure ProcessVT52Emulation(P : PAnsiEmulator; var Command: TEmuCommandLo); 
  begin
    with P^, Command do begin
      case Ch of
       'A' :
         begin
           {Cursor up}
           Cmd := eCUU; {eUp}
           Y := 1;
         end;
       'B' :
         begin
           {Cursor down}
           Cmd := eCUD; {eDown}
           Y := 1;
         end;
       'C' :
         begin
           {Cursor right}
           Cmd := eCUF; {eRight}
           X := 1;
         end;
       'D' :
         begin
           {Cursor left}
           cmd := eCUB; {eLeft}
           X := 1;
         end;
       'F' :
         begin
           {NOT SUPPORTED}
           { Select special graphics character set }
         end;
       'G' :
         begin
           {NOT SUPPORTED}
         end;
       'H' :
         begin
           {Cursor position}
           Cmd := eCUP;{eGotoXY}
           X   := 1;
           Y   := 1;
         end;
       'I' :
         begin
           {NOT SUPPORTED}
         end;
       'J' :
         begin
           {Erase in display}
           X := 2;
           Y := 0;
           Cmd := eED;
         end;
       'K' :
         begin
           {Erase in line}
           X := 2;
           Y := 0;
           Cmd := eEL;
         end;
       'Z' : Cmd := eDA;  {Device Attributes}
       'c' : Cmd := eRIS; {Reset to initial state}
       '=' :
         begin
           {NOT SUPPORTED}
         end;
       '>' :
         begin
           {NOT SUPPORTED}
         end;
       '<' :
         begin
           {NOT SUPPORTED}
         end;
      end;
    end;
  end;

  procedure aeMakeCommand(P : PAnsiEmulator; C : Char; var Command : TEmuCommandLo);
    {-Make a command record}
  begin
    with P^ do begin
      aeConvertParams(P, C);
      Command.emuType := P^.emuType;
      with Command do begin
        Ch := C;
        Cmd := eChar;
        case emuType of
          etANSI,
          etANSIBBS : ProcessANSIEmulation(P, Command);
          etVT100   : ProcessVT100Emulation(P, Command);
          etVT52    : ProcessVT52Emulation(P, Command);
        end;
      end;
    end;
  end;

  procedure aeProcessAnsiChar(P : Pointer; C : Char;
                              var Command : TEmuCommandLo);       
  const
    EqualSign = #61;
    QuestionMark = #63;

    procedure ErrorCondition;
      {-Set error state and reset parser}
    begin
      with PAnsiEmulator(P)^ do begin
        Command.Cmd := eError;
        if emuIndex > MaxOther then
          emuIndex := MaxOther;
        Move(emuQueue, Command.Other, emuIndex);
        aeInitParser(P);
      end;
    end;

  begin
    with PAnsiEmulator(P)^ do begin
      {-saftey check if no emulator type selected}
      if emuType = etNone then begin
        Command.Ch := C;
        Command.Cmd := eChar;
        exit;
      end;
      if emuFirst then begin
        {Handle startups that don't init attributes}
        emuFirst := False;
        Command.FColor := aweDefForeGround;
        Command.BColor := aweDefBackGround;
        Command.ExtAttr:= 0;
        emuAttr        := 0;
      end;

      aePutQueue(P, C);
      with Command do begin
        Ch := C;
        Cmd := eNone;
      end;

      {Evaluate parser state}

      case emuParserState of
        GotNone :
          begin
            case C of
              Escape : emuParserState := GotEscape;
              FormFeed : begin
                           Command.Cmd := eClearScreen;
                           Command.X   := 2;
                         end;
              #9 : Command.Cmd := eHT;
              #14 : Command.Cmd := eNone;
              #15 : Command.Cmd := eNone;
              else begin
                Command.Cmd := eChar;
                emuIndex := 0;
              end;
            end;
          end;
        GotEscape :
          if (C = LeftBracket) and (emuType <> etVT52) then
            emuParserState := GotControlSeqIntro
          else if (C = '(') and (emuType = etVT100) then
            emuParserState := GotLeftBrace
          else if (C = ')') and (emuType = etVT100) then
            emuParserState := GotRightBrace
          else begin
            Command.Cmd := eChar;
            case emuType of
              etANSI, etANSIBBS :
                case C of
                  'D' :
                    begin
                      Command.Cmd := eIND;
                      Command.Y := 1;
                    end;
                  'E' : Command.Cmd := eNEL;
                  'F' : Command.Cmd := eSSA;
                  'G' : Command.Cmd := eESA;
                  'H' : Command.Cmd := eHTS;
                  'I' : Command.Cmd := eHTJ;
                  'J' : Command.Cmd := eVTS;
                  'K' : Command.Cmd := ePLD;
                  'L' : Command.Cmd := ePLU;
                  'M' : Command.Cmd := eRI;
                  'N' : Command.Cmd := eSS2;
                  'O' : Command.Cmd := eSS3;
                  'P' : Command.Cmd := eDCS;
                  'Q' : Command.Cmd := ePU1;
                  'R' : Command.Cmd := ePU2;
                  'S' : Command.Cmd := eSTS;
                  'T' : Command.Cmd := eCCH;
                  'U' : Command.Cmd := eMW;
                  'V' : Command.Cmd := eSPA;
                  'W' : Command.Cmd := eEPA;
                  'a' : Command.Cmd := eINT;
                  'b' : Command.Cmd := eEMI;
                  'c' : Command.Cmd := eRIS;
                  '^' : Command.Cmd := ePM;
                  '\' : Command.Cmd := eST;
                end;
              etVT100 :
                case C of
                  'c' : Command.Cmd := eRIS;
                  'D' :
                    begin
                      Command.Cmd := eIND;
                      Command.Y := 1;
                    end;
                  'E' : Command.Cmd := eNEL;
                  'H' : Command.Cmd := eHTS;
                  'M' : Command.Cmd := eRI;
                  'Z' :
                    begin
                      Command.Cmd := eDA;
                      Command.X := 0;
                      Command.Y := 1;
                    end;
                  '7' : Command.Cmd := eSaveCursorPos;
                  '8' : Command.Cmd := eRestoreCursorPos;
                  '=' : Command.Cmd := eNone;
                  '>' : Command.Cmd := eNone;
                end;
              etVT52  :
                begin
                  if ((C >= 'A') and (C <= 'D')) or ((C >= 'F') and (C <= 'K')) or
                     (C = 'Z') or (C = '=') or (C = '<') or (C = '>') or (C = 'c') then
                    aeMakeCommand(P, C, Command);
                end;
            end;
            aeInitParser(P);
          end;
        GotSemiColon,
        GotParam,
        GotControlSeqIntro :
          begin
            case emuType of
              etAnsi,
              etANSIBBS,
              etVT100 :
                begin
                  case C of
                    #48..#57 :
                      begin
                        aeBuildParam(P, C);
                        emuParserState := GotParam;
                      end;
                    EqualSign : ;
                    QuestionMark :
                      begin
                        emuParserState := GotQuestionMark;
                        Inc(emuParamIndex);
                        if emuParamIndex > MaxParams then
                          ErrorCondition;
                      end;
                    SemiColon :
                      begin
                        emuParserState := GotSemicolon;
                        Inc(emuParamIndex);
                        if emuParamIndex > MaxParams then
                          ErrorCondition;
                      end
                    else begin
                      aeMakeCommand(P, C, Command);
                      aeInitParser(P);
                    end;
                  end;
                end;
            end;
          end;
        GotQuestionParam,
        GotQuestionMark :
          begin
            case emuType of
              etVT100 :
                begin
                  case C of
                    #49..#57 :
                      begin
                        aeBuildParam(P, C);
                        emuParserState := GotQuestionParam;
                      end;
                    'h',
                    'l' :
                      begin
                        Command.Cmd := eNone;
                        aeInitParser(P);
                      end;
                    else begin
                      Command.Cmd := eChar;
                      aeInitParser(P);
                    end;
                  end;
                end;
              else begin
                Command.Cmd := eChar;
                aeInitParser(P);
              end;
            end;
          end;
        GotLeftBrace :
          begin
            if emuType = etVT100 then begin
              Command.Cmd := eNone;
              case C of {NOT SUPPORTED}
                'A' : ;
                'B' : ;  { This is for character set information }
                '0' : ;
                '1' : ;
                '2' : ;
                else Command.Cmd := eChar;
              end;
              aeInitParser(P);
            end;
          end;
        GotRightBrace :
          begin
            if emuType = etVT100 then begin
              Command.Cmd := eNone;
              case C of {NOT SUPPORTED}
                'A' : ;
                'B' : ;  { This is for character set information }
                '0' : ;
                '1' : ;
                '2' : ;
                else Command.Cmd := eChar;
              end;
              aeInitParser(P);
            end;
          end;
        GotSpace :
          begin
            {NOT SUPPORTED}
          end;
        GotCommand :
          begin
            {NOT SUPPORTED}
          end;
      end;
    end;
  end;

end.
