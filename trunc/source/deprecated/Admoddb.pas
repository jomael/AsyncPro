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
{*                   ADMODDB.PAS 4.06                    *}
{*********************************************************}
{* Deprecated modem database component                   *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdModDB;
  {-Delphi modem database component}

interface

uses
  SysUtils,
  Classes,
  Messages,
  WinTypes,
  WinProcs,
  Forms,
  Controls,
  OoMisc,
  AwModDB,
  AdExcept,
  AdIniDB;

const
  admdDefModemDBName = 'AWMODEM.INI';
  admdDefSortedIndex = False;

type
  {string types for modem/modem database component data}
  TModemName     = String[ApdModemNameLen];                            {!!.03}
  TCmdString     = String[ApdCmdLen];                                  {!!.03}                                         
  TRspString     = String[ApdRspLen];                                  {!!.03}
  TTagString     = String[ApdTagLen];                                  {!!.03}
  TTagProfString = String[ApdTagProfLen];                              {!!.03}
  TConfigString  = String[ApdConfigLen];                               {!!.03}
  TBoolStr       = String[ApdBoolLen];                                 {!!.03}       
  TBaudStr       = String[ApdBaudLen];                                 {!!.03}

  {array of response strings}
  PTagArray = ^TTagArray;
  TTagArray = array[1..ApdMaxTags] of TTagString;                      {!!.03}

  {modem data}
  PModemInfo = ^TModemInfo;
  TModemInfo = record
    Name          : TModemName;
    InitCmd       : TCmdString;
    DialCmd       : TCmdString;
    DialTerm      : TCmdString;
    DialCancel    : TCmdString;
    HangupCmd     : TCmdString;
    ConfigCmd     : TConfigString;
    AnswerCmd     : TCmdString;
    OkMsg         : TRspString;
    ConnectMsg    : TRspString;
    BusyMsg       : TRspString;
    VoiceMsg      : TRspString;
    NoCarrierMsg  : TRspString;
    NoDialToneMsg : TRspString;
    ErrorMsg      : TRspString;
    RingMsg       : TRspString;

    NumErrors     : Word;
    Errors        : TTagArray;
    NumComps      : Word;
    Compression   : TTagArray;
    LockDTE       : Boolean;
    DefBaud       : LongInt;
  end;

type
  TApdCustomModemDBase = class(TApdBaseComponent)
  protected {private}
    {.Z+}
    DB           : PModemDatabase;      {record passed to DLL calls}
    Strs         : TStringList;         {for returning lists of modems}
    Scratch      : TModemData;          {scratch record for passing into DLL}
    Changed      : Boolean;             {TRUE if database data has changed}
    FOpen        : Boolean;             {TRUE if database has been opened}
    FSortedIndex : Boolean;
    FFileName    : String;              {file name, used mostly at design time}

    {property read/write}
    procedure SetFileName(const NewName : String);
      {-Update the database's file name.  Reopen under the new name}
    procedure SetSortedIndex(const NewSorted : Boolean);
      {-Set the Sorted property of the record list}
    procedure SetOpen(const OpenIt : Boolean);
      {-Open or close the database}
    function GetRecordList : TStrings;
      {-Returns the names of each modem in the database}
    function GetNumModems : Integer;
      {-Returns the number of modems in the database}

    {utility}
    procedure AssureOpen;
      {-Make sure that the database is open, otherwise raise an exception}
    {.Z-}

  protected
    property FileName : String
      read FFileName write SetFileName;

  public
    {.Z+}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {.Z-}

    procedure AddModem(const Modem : TModemInfo);
      {-Add a modem to the database}
    procedure UpdModem(const ModemName : TModemName; const Modem : TModemInfo);
      {-Update a modem's record in the database}
    procedure DelModem(const ModemName : TModemName);
      {-Delete a modem from the database}
    procedure GetModem(const ModemName : TModemName; var Modem : TModemInfo);
      {-Get a modem from the database}
    procedure WriteToIni(const Rec : TModemInfo; const Section, IniFile : String);
      {-Write the modem to a user-specified .INI file}
    procedure ReadFromIni(var Rec : TModemInfo; const Section, IniFile : String);
      {-Read the modem from a user-specified .INI file}

    property NumModems : Integer
      read GetNumModems;
    property Modems : TStrings
      read GetRecordList;
    property Open : Boolean
      read FOpen write SetOpen;
    property SortedIndex : Boolean
      read FSortedIndex write SetSortedIndex default admdDefSortedIndex;
  end;

  TApdModemDBase = class(TApdCustomModemDBase)
  published
    property FileName;
    property SortedIndex;
  end;

{.Z+}
procedure StrsToPChars(const StrRec : TModemInfo; var PCharRec : TModemData);
  {-Convert a record with string variables to a record with PChars}
procedure PCharsToStrs(const PCharRec : TModemData; var StrRec : TModemInfo);
  {-Convert a record with PChar variables to a record with strings}
{.Z-}

implementation

{TApdCustomModemDBase}

  procedure TApdCustomModemDBase.SetFileName(const NewName : String);
    {-Update the database's file name.  Reopen under the new name}
  begin
    if (TrimRight(NewName) <> '') then
      FFileName := NewName
    else
      Exit;

    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      Exit;

    {reopen the database under the new name}
    if Open then
      Open := True;
  end;

  procedure TApdCustomModemDBase.SetSortedIndex(const NewSorted : Boolean);
    {-Set the Sorted property of the record list}
  begin
    if (NewSorted = FSortedIndex) then
      Exit;

    FSortedIndex := NewSorted;
    if csDesigning in ComponentState then
      Exit;

    Strs.Sorted := FSortedIndex;
    Changed     := True;
  end;

  procedure TApdCustomModemDBase.SetOpen(const OpenIt : Boolean);
    {-Open or close the database}
  var
    Temp : array[0..255] of Char;

    procedure FreeElement;
    begin
      mDoneModemDatabase(DB);
      DB := nil;
    end;

  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      Exit;

    if OpenIt then begin
      if FOpen then
        FreeElement;
      CheckException(Self, mInitModemDatabase(DB, StrPCopy(Temp, FFileName)));
    end else
      if FOpen then
        FreeElement;

    FOpen := OpenIt;
  end;

  function TApdCustomModemDBase.GetRecordList : TStrings;
    {-Returns the names of each modem in the database}
  var
    IndexRec : PChar;
    Temp     : PChar;
    BufSize  : Integer;

  begin
    AssureOpen;

    if Changed then begin
      Strs.Clear;

      if (NumModems > 0) then begin
        {allocate memory for the index}
        CheckException(Self, mAllocModemIndexRec(DB, IndexRec, BufSize));

        {load the buffer with the strings}
        CheckException(Self, mLoadModemIndex(DB, IndexRec, BufSize));

        {put the strings into the list}
        Temp := IndexRec;
        while (Temp^ <> #0) do begin
          Strs.Add(StrPas(Temp));
          Inc(Temp, StrLen(Temp) + 1);
        end;

        {deallocate the index}
        mDeallocModemIndexRec(DB, IndexRec, BufSize);
      end;

      Changed := False;
    end;

    Result := Strs;
  end;

  function TApdCustomModemDBase.GetNumModems : Integer;
    {-Returns the number of modems in the database}
  begin
    AssureOpen;

    GetNumModems := mNumModems(DB);
  end;

  procedure TApdCustomModemDBase.AssureOpen;
    {-Make sure that the database is open, otherwise raise an exception}
  begin
    if not Open then
      Open := True;
  end;

  constructor TApdCustomModemDBase.Create(AOwner : TComponent);
  begin
    inherited Create(AOwner);

    DB           := nil;
    Changed      := True;
    FOpen        := False;
    FSortedIndex := admdDefSortedIndex;
    Strs         := nil;
    FFileName    := admdDefModemDBName;

    {create the object for returning record names}
    if not (csDesigning in ComponentState) then
      Strs := TStringList.Create;
  end;

  destructor TApdCustomModemDBase.Destroy;

  begin
    if Assigned(DB) then
      mDoneModemDatabase(DB);
    if Assigned(Strs) then
      Strs.Free;

    inherited Destroy;
  end;

  procedure TApdCustomModemDBase.AddModem(const Modem : TModemInfo);
    {-Add a modem to the database}
  begin
    AssureOpen;

    StrsToPChars(Modem, Scratch);
    CheckException(Self, mAddModem(DB, Scratch));
    Changed := True;
  end;

  procedure TApdCustomModemDBase.UpdModem(const ModemName : TModemName; const Modem : TModemInfo);
    {-Update a modem's record in the database}
  var
    Temp : array[0..255] of Char;

  begin
    AssureOpen;

    StrsToPChars(Modem, Scratch);
    CheckException(Self, mUpdModem(DB, StrPCopy(Temp, ModemName), Scratch));
    Changed := True;
  end;

  procedure TApdCustomModemDBase.DelModem(const ModemName : TModemName);
    {-Delete a modem from the database}
  var
    Temp : array[0..255] of Char;

  begin
    AssureOpen;

    CheckException(Self, mDelModem(DB, StrPCopy(Temp, ModemName)));
    Changed := True;
  end;

  procedure TApdCustomModemDBase.GetModem(const ModemName : TModemName; var Modem : TModemInfo);
    {-Get a modem from the database}
  var
    Temp : array[0..255] of Char;

  begin
    AssureOpen;

    CheckException(Self, mGetModem(DB, StrPCopy(Temp, ModemName), Scratch));
    PCharsToStrs(Scratch, Modem);
  end;

  procedure TApdCustomModemDBase.WriteToIni(const Rec : TModemInfo; const Section, IniFile : String);
    {-Write the modem to a user-specified .INI file}
  var
    TempSec : array[0..255] of Char;
    TempIni : array[0..255] of Char;

  begin
    AssureOpen;

    StrsToPChars(Rec, Scratch);
    CheckException(Self, mWriteToIni(DB, Scratch, StrPCopy(TempSec, Section), StrPCopy(TempIni, IniFile)));
  end;

  procedure TApdCustomModemDBase.ReadFromIni(var Rec : TModemInfo; const Section, IniFile : String);
    {-Read the modem from a user-specified .INI file}
  var
    TempSec : array[0..255] of Char;
    TempIni : array[0..255] of Char;

  begin
    AssureOpen;

    CheckException(Self, mReadFromIni(DB, Scratch, StrPCopy(TempSec, Section), StrPCopy(TempIni, IniFile)));
    PCharsToStrs(Scratch, Rec);
  end;

  procedure StrsToPChars(const StrRec : TModemInfo; var PCharRec : TModemData);
    {-Convert a record with string variables to a record with PChars}
  var
    I : Word;

  begin
    FillChar(PCharRec, SizeOf(PCharRec), 0);

    StrPCopy(PCharRec.Data.Name, StrRec.Name);
    StrPCopy(PCharRec.Data.InitCmd, StrRec.InitCmd);
    StrPCopy(PCharRec.Data.DialCmd, StrRec.DialCmd);
    StrPCopy(PCharRec.Data.DialTerm, StrRec.DialTerm);
    StrPCopy(PCharRec.Data.DialCancel, StrRec.DialCancel);
    StrPCopy(PCharRec.Data.HangupCmd, StrRec.HangupCmd);
    StrPCopy(PCharRec.Data.ConfigCmd, StrRec.ConfigCmd);
    StrPCopy(PCharRec.Data.AnswerCmd, StrRec.AnswerCmd);
    StrPCopy(PCharRec.Data.OkMsg, StrRec.OkMsg);
    StrPCopy(PCharRec.Data.ConnectMsg, StrRec.ConnectMsg);
    StrPCopy(PCharRec.Data.BusyMsg, StrRec.BusyMsg);
    StrPCopy(PCharRec.Data.VoiceMsg, StrRec.VoiceMsg);
    StrPCopy(PCharRec.Data.NoCarrierMsg, StrRec.NoCarrierMsg);
    StrPCopy(PCharRec.Data.NoDialToneMsg, StrRec.NoDialToneMsg);
    StrPCopy(PCharRec.Data.ErrorMsg, StrRec.ErrorMsg);
    StrPCopy(PCharRec.Data.RingMsg, StrRec.RingMsg);

    for I := 1 to StrRec.NumErrors do
      StrPCopy(PCharRec.Errors[I], StrRec.Errors[I]);
    PCharRec.NumErrors := StrRec.NumErrors;

    for I := 1 to StrRec.NumComps do
      StrPCopy(PCharRec.Compression[I], StrRec.Compression[I]);
    PCharRec.NumComps := StrRec.NumComps;

    PCharRec.LockDTE := StrRec.LockDTE;
    PCharRec.DefBaud := StrRec.DefBaud;
  end;

  procedure PCharsToStrs(const PCharRec : TModemData; var StrRec : TModemInfo);
    {-Convert a record with PChar variables to a record with strings}
  var
    I : Word;

  begin
    FillChar(StrRec, SizeOf(StrRec), 0);

    StrRec.Name          := StrPas(PCharRec.Data.Name);
    StrRec.InitCmd       := StrPas(PCharRec.Data.InitCmd);
    StrRec.DialCmd       := StrPas(PCharRec.Data.DialCmd);
    StrRec.DialTerm      := StrPas(PCharRec.Data.DialTerm);
    StrRec.DialCancel    := StrPas(PCharRec.Data.DialCancel);
    StrRec.HangupCmd     := StrPas(PCharRec.Data.HangupCmd);
    StrRec.ConfigCmd     := StrPas(PCharRec.Data.ConfigCmd);
    StrRec.AnswerCmd     := StrPas(PCharRec.Data.AnswerCmd);
    StrRec.OkMsg         := StrPas(PCharRec.Data.OkMsg);
    StrRec.ConnectMsg    := StrPas(PCharRec.Data.ConnectMsg);
    StrRec.BusyMsg       := StrPas(PCharRec.Data.BusyMsg);
    StrRec.VoiceMsg      := StrPas(PCharRec.Data.VoiceMsg);
    StrRec.NoCarrierMsg  := StrPas(PCharRec.Data.NoCarrierMsg);
    StrRec.NoDialToneMsg := StrPas(PCharRec.Data.NoDialToneMsg);
    StrRec.ErrorMsg      := StrPas(PCharRec.Data.ErrorMsg);
    StrRec.RingMsg       := StrPas(PCharRec.Data.RingMsg);

    for I := 1 to PCharRec.NumErrors do
      StrRec.Errors[I] := StrPas(PCharRec.Errors[I]);
    StrRec.NumErrors := PCharRec.NumErrors;

    for I := 1 to PCharRec.NumComps do
      StrRec.Compression[I] := StrPas(PCharRec.Compression[I]);
    StrRec.NumComps := PCharRec.NumComps;

    StrRec.LockDTE := PCharRec.LockDTE;
    StrRec.DefBaud := PCharRec.DefBaud;
  end;

end.
