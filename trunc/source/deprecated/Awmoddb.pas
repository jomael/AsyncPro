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
{*                   AWMODDB.PAS 4.06                    *}
{*********************************************************}
{* Deprecated modem database (awmodem.ini) support       *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$X+,V-,B-,I-}

unit AwModDB;
  {-Modem database management}

interface

uses
  SysUtils,
  WinTypes,
  WinProcs,
  OoMisc,
  AwIniDB;

{ key strings }
const
  mkName         = 'Name';           { Modem name             }
  mkInitCmd      = 'InitCmd';        { Initialization command }
  mkDialCmd      = 'DialCmd';        { Dial command           }
  mkDialTerm     = 'DialTerm';       { Dial terminator        }
  mkDialCancel   = 'DialCancel';     { Dial cancel            }
  mkHangupCmd    = 'HangupCmd';      { Hangup command         }
  mkConfig       = 'ConfigCmd';      { Configuration command  }
  mkAnswerCmd    = 'AnswerCmd';      { Answer command         }
  mkOkMsg        = 'OkMsg';          { OK response            }
  mkConnectMsg   = 'ConnectMsg';     { CONNECT response       }
  mkBusyMsg      = 'BusyMsg';        { BUSY response          }
  mkVoiceMsg     = 'VoiceMsg';       { VOICE response         }
  mkNoCarrier    = 'NoCarrierMsg';   { NO CARRIER response    }
  mkNoDialTone   = 'NoDialToneMsg';  { NO DIALTONE response   }
  mkErrorStr     = 'ErrorMsg';       { ERROR response         }
  mkRing         = 'RingMsg';        { RING response          }
  mkErrorTags    = 'ErrorCheckTags'; { Error correction tags  }
  mkCompressTags = 'CompressTags';   { Data compression tags  }
  mkLockDTE      = 'LockDTE';        { Lock DTE rate          }
  mkDefBaud      = 'DefaultBaud';    { Default baud rate      }

function mInitModemDatabase(var ModemDB : PModemDatabase; FName : PChar) : Integer;
  {-Initialize a modem database }

procedure mDoneModemDatabase(var ModemDB : PModemDatabase);
  {-Destroy a modem database }

function mAddModem(ModemDB : PModemDatabase; var Modem : TModemData) : Integer;
  {-Add a modem to the database }

function mUpdModem(ModemDB : PModemDatabase; ModemName : PChar; var Modem : TModemData) : Integer;

  {-Update a modem's record in the database }

function mDelModem(ModemDB : PModemDatabase; ModemName : PChar) : Integer;
  {-Delete a modem from the database }

function mGetModem(ModemDB : PModemDatabase; ModemName : PChar; var Modem : TModemData) : Integer;
  {-Get a modem from the database }

function mAllocModemIndexRec(     ModemDB : PModemDatabase;
                              var IRec    : PChar;
                              var BufSize : Integer) : Integer;
  {-Allocate a buffer for the modem index record }

procedure mDeallocModemIndexRec(    ModemDB : PModemDatabase;
                                var IRec    : PChar;
                                    BufSize : Integer );
  {-Deallocate a modem index record }

function mLoadModemIndex(ModemDB : PModemDatabase; Buf : PChar; BufSize : Integer) : Integer;
  {-Load the modem index into Buf }

function mNumModems(ModemDB : PModemDatabase) : Integer;
  {-Return the number of modems in a database }

function mWriteToIni(ModemDB : PModemDatabase; var Rec : TModemData; Section, IniFile : PChar) : Integer;
  {-Write the modem to a user-specified .INI file }

function mReadFromIni(ModemDB : PModemDatabase; var Rec : TModemData; Section, IniFile : PChar) : Integer;
  {-Read the modem from a user-specified .INI file }

const
  DefBaudRate   : LongInt    = 2400;

  ModemDefaults : TModemXFer =
    ( Data            : (
        Name          : '';
        InitCmd       : 'ATZ^M';
        DialCmd       : 'ATD';
        DialTerm      : '^M';
        DialCancel    : '^M';
        HangupCmd     : '+++~~~ATH0^M';
        ConfigCmd     : 'ATE1Q0X1V1^M';
        AnswerCmd     : 'ATA^M';
        OkMsg         : 'OK';
        ConnectMsg    : 'CONNECT';
        BusyMsg       : 'BUSY';
        VoiceMsg      : 'VOICE';
        NoCarrierMsg  : 'NO CARRIER';
        NoDialToneMsg : 'NO DIAL';
        ErrorMsg      : 'ERROR';
        RingMsg       : 'RING'
      );
      Errors          : '';
      Compress        : '';
      LockDTE         : 'TRUE';
      DefBaud         : '2400'
    );

implementation

type
  TModemIniDef =
    record
      Name  : array[0..MaxNameLen] of Char;
      Len   : Cardinal;
    end;

  const
    ModemIniFields : array[1..19] of TModemIniDef =
      ( ( Name : mkInitCmd     ; Len : ApdCmdLen     ),                {!!.03}
        ( Name : mkDialCmd     ; Len : ApdCmdLen     ),                {!!.03}
        ( Name : mkDialTerm    ; Len : ApdCmdLen     ),                {!!.03}
        ( Name : mkDialCancel  ; Len : ApdCmdLen     ),                {!!.03}
        ( Name : mkHangupCmd   ; Len : ApdCmdLen     ),                {!!.03}
        ( Name : mkConfig      ; Len : ApdConfigLen  ),                {!!.03}
        ( Name : mkAnswerCmd   ; Len : ApdCmdLen     ),                {!!.03}
        ( Name : mkOkMsg       ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkConnectMsg  ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkBusyMsg     ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkVoiceMsg    ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkNoCarrier   ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkNoDialTone  ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkErrorStr    ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkRing        ; Len : ApdRspLen     ),                {!!.03}
        ( Name : mkErrorTags   ; Len : ApdTagProfLen ),                {!!.03}
        ( Name : mkCompressTags; Len : ApdTagProfLen ),                {!!.03}
        ( Name : mkLockDTE     ; Len : ApdBoolLen    ),                {!!.03}
        ( Name : mkDefBaud     ; Len : ApdBaudLen    ) );              {!!.03}

  function mInitModemDatabase(var ModemDB : PModemDatabase; FName : PChar) : Integer;
    {-Initialize a modem database }
  var
    Res : Integer;
    I   : Integer;

  label
    ExitPoint;

  begin
    ModemDB := AllocMem(SizeOf(TModemDatabase));

    iInitIniDatabase(ModemDB^.DB, FName);

    with ModemDB^ do begin
      Res := iAddIniDBStringField(DB, mkName, ApdModemNameLen, True);  {!!.03}
      if (Res < ecOK) then
        goto ExitPoint;

      for I := 1 to 19 do begin
        Res := iAddIniDBStringField(DB, ModemIniFields[I].Name, ModemIniFields[I].Len, False);
        if (Res < ecOK) then
          goto ExitPoint;
      end;

      Res := iPrepareIniDatabase(DB, @ModemDefaults);
    end;

ExitPoint:
    mInitModemDatabase := Res;
    if (Res < ecOK) then
      mDoneModemDatabase(ModemDB);
  end;

  procedure mDoneModemDatabase(var ModemDB : PModemDatabase);
    {-Destroy a modem database }
  begin
    iDoneIniDatabase(ModemDB^.DB);
    FreeMem(ModemDB, SizeOf(TModemDatabase));
    ModemDB := nil;
  end;

  procedure UnpackTags(Data : PChar; var Tags : TTagArrayZ; var NumTags : Cardinal);
    {-Unpack a set of tags from a profile string.  A tag profile string is
      a string, stored in an INI database, that contains one or more sub-
      strings, delimited by TagSepChar }
  var
    I        : Cardinal;
    Len      : Cardinal;
    TagBegin : PChar;
    TagEnd   : PChar;

  begin
    if (Data[0] = #0) then begin
      NumTags := 0;
      Exit;
    end;

    I        := 1;
    TagBegin := Data;
    TagEnd   := Data;

    while (TagEnd <> nil) and (I <= ApdMaxTags) do begin               {!!.03}
      TagEnd := StrScan(TagBegin, ApdTagSepChar);                      {!!.03}        
      if (TagEnd <> nil) then begin
        { tag in middle of list }
        Len := MinWord(TagEnd - TagBegin, ApdTagLen);                  {!!.03}
        StrMove(Tags[I], TagBegin, Len)
      end else begin
        { tag at end of list }
        StrMove(Tags[I], TagBegin, StrLen(TagBegin));
      end;

      TagBegin := TagEnd + 1;
      Inc(I);
    end;

    NumTags := I - 1;
  end;

  procedure PackTags(NumTags : Cardinal; var Tags : TTagArrayZ; Dest : PChar);
    {-Pack a tag array into a profile string.  A tag profile string is
      a string, stored in an INI database, that contains one or more sub-
      strings, delimited by TagSepChar }
  var
    I : Cardinal;

  begin
    Dest[0] := #0;
    if (NumTags > 0) then begin
      for I := 1 to Pred(NumTags) do begin
        StrCat(Dest, Tags[I]);
        StrCat(Dest, ApdTagSepChar);                                   {!!.03}
      end;
      StrCat(Dest, Tags[NumTags]);
    end;
  end;

  function BoolFromStr(St : PChar) : Bool;
    {-Return a Bool based on St }
  var
    Temp : TBoolStrZ;

  begin
    if (StrLen(St) > ApdBoolLen) then                                  {!!.03}
      BoolFromStr := False
    else begin
      StrCopy(Temp, St);
      StrUpper(Temp);
      BoolFromStr := (StrComp(Temp, 'TRUE') = 0) or (StrComp(Temp, 'YES') = 0) or
                     (StrComp(Temp, 'ON') = 0) or (StrComp(Temp, '1') = 0);
    end;
  end;

  function StrFromBool(Dest : PChar; B : Bool) : PChar;
    {-Return a string based on a Bool }
  begin
    if B then
      StrCopy(Dest, 'TRUE')
    else
      StrCopy(Dest, 'FALSE');
    StrFromBool := Dest;
  end;

  procedure mTagsToXfer(var XFer : TModemXFer; var Modem : TModemData);
    {-Convert tag profile strings into a tag array }
  begin
    XFer.Data := Modem.Data;
    PackTags(Modem.NumErrors, Modem.Errors, XFer.Errors);
    PackTags(Modem.NumComps, Modem.Compression, XFer.Compress);
    StrFromBool(XFer.LockDTE, Modem.LockDTE);
    Long2StrZ(XFer.DefBaud, Modem.DefBaud);
  end;

  procedure mXferToTags(var XFer : TModemXFer; var Modem : TModemData);
    {-Convert tag array into tag profile strings }
  begin
    Modem.Data := XFer.Data;
    UnpackTags(XFer.Errors, Modem.Errors, Modem.NumErrors);
    UnpackTags(XFer.Compress, Modem.Compression, Modem.NumComps);
    Modem.LockDTE := BoolFromStr(XFer.LockDTE);
    if not Str2LongZ(XFer.DefBaud, Modem.DefBaud) then
      Modem.DefBaud := DefBaudRate;
  end;

  function mAddModem(ModemDB : PModemDatabase; var Modem : TModemData) : Integer;
    {-Add a modem to the database }
  var
    XFer : TModemXFer;

  begin
    with ModemDB^ do begin
      mTagsToXFer(XFer, Modem);

      mAddModem := iAddIniRecord(DB, XFer);
    end;
  end;

  function mUpdModem(ModemDB : PModemDatabase; ModemName : PChar; var Modem : TModemData) : Integer;
    {-Update a modem's record in the database }
  var
    XFer : TModemXFer;

  begin
    with ModemDB^ do begin
      mTagsToXfer(XFer, Modem);

      mUpdModem := iUpdIniRecord(DB, ModemName, XFer);
    end;
  end;

  function mDelModem(ModemDB : PModemDatabase; ModemName : PChar) : Integer;
    {-Delete a modem from the database }
  begin
    mDelModem := iDelIniRecord(ModemDB^.DB, ModemName);
  end;

  function mAllocModemIndexRec(     ModemDB : PModemDatabase;
                                var IRec    : PChar;
                                var BufSize : Integer) : Integer;
    {-Allocate a buffer for the modem index record }
  begin
    mAllocModemIndexRec := iAllocIniIndexRec(ModemDB^.DB, IRec, BufSize);
  end;

  procedure mDeallocModemIndexRec(    ModemDB : PModemDatabase;
                                  var IRec    : PChar;
                                      BufSize : Integer );
    {-Deallocate a modem index record }
  begin
    iDeallocIniIndexRec(ModemDB^.DB, IRec, BufSize);
  end;

  function mLoadModemIndex(ModemDB : PModemDatabase; Buf : PChar; BufSize : Integer) : Integer;
    {-Load the modem index into Buf }
  begin
    mLoadModemIndex := iLoadIniIndex(ModemDB^.DB, Buf, BufSize);
  end;

  function mNumModems(ModemDB : PModemDatabase) : Integer;
    {-Return the number of modems in a database }
  begin
    mNumModems := iNumIniRecs(ModemDB^.DB);
  end;

  function mGetModem(ModemDB : PModemDatabase; ModemName : PChar; var Modem : TModemData) : Integer;
    {-Get a modem from the database }
  var
    XFer : TModemXFer;
    Code : Integer;

  begin
    FillChar(Modem, SizeOf(Modem), 0);
    Code := iGetIniRecord(ModemDB^.DB, ModemName, XFer);
    if (Code = ecOK) then
      mXferToTags(Xfer, Modem);
    mGetModem := Code;
  end;

  function mWriteToIni(ModemDB : PModemDatabase; var Rec : TModemData; Section, IniFile : PChar) : Integer;
    {-Write the modem to a user-specified .INI file }
  var
    XFer : TModemXFer;

  begin
    mTagsToXfer(Xfer, Rec);
    mWriteToIni := iWriteToIni(ModemDB^.DB, XFer, Section, IniFile);
  end;

  function mReadFromIni(ModemDB : PModemDatabase; var Rec : TModemData; Section, IniFile : PChar) : Integer;
    {-Read the modem from a user-specified .INI file }
  var
    Code : Integer;
    XFer : TModemXFer;

  begin
    Code := iReadFromIni(ModemDB^.DB, XFer, Section, IniFile);
    if (Code = ecOK) then
      mXferToTags(Xfer, Rec);
    mReadFromIni := Code;
  end;

end.
