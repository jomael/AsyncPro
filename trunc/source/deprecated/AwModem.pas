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
{*                   AWMODEM.PAS 4.06                    *}
{*********************************************************}
{* Deprecated low-level modem support                    *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$X+,V-,B-,I-}

unit AwModem;
  {-Modem access}

interface

{.$DEFINE DispatchDebug}

uses
  WinTypes,
  WinProcs,
  Messages,
  SysUtils,
  OoMisc,
  AwModDB,
  AwUser;

{modem state machine states}
const
  msNone         = 0;
  msCmdWaitRsp   = 1;
  msDialing      = 2;
  msWaitNumber   = 3;
  msWaitTerm     = 4;
  msAnswering    = 5;
  msWaitRing     = 6;
  msWaitFeatures = 7;

const
  NumResponses  = 8;
  RspOK         = 1;
  RspConnect    = 2;
  RspBusy       = 3;
  RspVoice      = 4;
  RspNoCarrier  = 5;
  RspNoDialTone = 6;
  RspError      = 7;
  RspRing       = 8;

const
  TentBaudLen   = 13;  {Length of tentative baud rate}
  CmdSepChar    = '|'; {Character to separate multiple commands}

  cpOK      = 1;
  cpError   = 2;
  cpTimeout = 3;

const
  awmDefDialTimeout     = 60;     {Default seconds for dial timeout}
  awmDefAnswerTimeout   = 60;     {Default seconds for answer timeout}
  awmDefDelayFactor     = 2;      {Default ticks for inter-cmd delay}
  awmDefCmdTimeout      = 182;    {Default ticks for command timeout (10 secs)}
  awmDefDTRDropHold     = 8;      {Default ticks for DTR low during hangup}
  awmDefModemCharDelay  = 0;      {Default ticks between each outgoing cmd char}
  awmDefTildeDelay      = 9;      {Default ticks to delay for ~s in cmd strings}
  awmDefRingWaitTimeout = 182;    {Default ticks before auto answer resets}
  awmDefFeatureWait     = 9;      {Default number of ticks to wait for features}
  awmDefBaudWait        = 36;     {Default number of ticks to wait for a BPS rate}
  TickSeconds        = 18;     {Default number of ticks in a second}

type
  PModemRegisterList = ^TModemRegisterList;
  TModemRegisterList = record
    mrHWindow : TApdHwnd;
    mrNotify  : TApdNotifyProc;
    mrDeleted : Bool;
    mrNext    : PModemRegisterList;
  end;

  PModemResponse = ^TModemResponse;
  TModemResponse = record
    Response   : PChar;
    TriggerIdx : Cardinal;                                           
    Enabled    : Bool;
  end;

  PModemResponseArray = ^TModemResponseArray;
  TModemResponseArray = array[1..NumResponses] of TModemResponse;

  TModemSendCmd = array[0..ApdCmdLen] of Char;                         {!!.03}
  TTentBaudSt   = array[0..TentBaudLen] of Char;

  PModemRec = ^TModemRec;
  TModemRec = record
    {modem comands}
    InitCmd            : PChar;
    DialCmd            : PChar;
    DialTerm           : PChar;
    DialCancel         : PChar;
    HangupCmd          : PChar;
    ConfigCmd          : PChar;
    AnswerCmd          : PChar;

    Responses          : TModemResponseArray;

    NumErrors          : Cardinal;
    ErrorTags          : array[1..ApdMaxTags] of TModemResponse;       {!!.03}
    NumComps           : Cardinal;
    CompressTags       : array[1..ApdMaxTags] of TModemResponse;       {!!.03}

    {last message dispatched to clients}
    LastMessage        : Cardinal;

    {timeout/delay values}
    DialTimeout        : Cardinal;
    AnswerTimeout      : Cardinal;
    DelayFactor        : Cardinal;
    CmdTimeout         : Cardinal;
    DTRDropHold        : Cardinal;
    ModemCharDelay     : Cardinal;
    TildeDelay         : Cardinal;
    RingWaitTimeout    : Cardinal;
    FeatureWait        : Cardinal;
    BaudWait           : Cardinal;

    {communications}
    Port               : TApdBaseDispatcher; 
    LockDTE            : Bool;
    RegisterHead       : PModemRegisterList;

    {state machine}
    ModemStarted       : Bool;
    ModemState         : Integer;
    TimeoutIdx         : Cardinal;                                  

    {dialing/answering}
    TentativeLineIdx   : Cardinal;
    ErrorCorrection    : Bool;
    DataCompression    : Bool;
    TentativeLineSpeed : TTentBaudSt;
    ConnectSpeed       : LongInt;
    Countdown          : Cardinal;

    {auto answering}
    RingCnt            : Cardinal;
    RingWait           : Cardinal;

    {data for DELPHI}
    UserData           : LongInt;
    DelphiComponent    : Bool;

    {last string info}
    LastString         : array[0..255] of Char;
    LastStringLen      : Cardinal;
    BlankPending       : Boolean;                  
  end;

function mInitModem(var Modem : PModemRec; H : TApdBaseDispatcher; var Data : TModemData) : Integer;
  {-Initialize a modem }

function mInitModemDelphi(var Modem : PModemRec; H : TApdBaseDispatcher; var Data : TModemData) : Integer;
  {-Initialize a modem }

procedure mDoneModem(var Modem : PModemRec);
  {-Destroy a modem }

function mGetComHandle(var Modem : PModemRec) : TApdBaseDispatcher;
  {-Return the handle of a modem's port }

procedure mSetModemDialTimeout(Modem : PModemRec; Secs : Cardinal);
  {-Set the number of seconds before a dial attempt times out }

function mGetModemDialTimeout(Modem : PModemRec) : Cardinal;
  {-Get the number of seconds the modem will wait before aborting a dial }

procedure mSetModemAnswerTimeout(Modem : PModemRec; Secs : Cardinal);
  {-Set the number of seconds before an answer attempt times out }

procedure mSetModemDelayFactor(Modem : PModemRec; Ticks : Cardinal);
  {-Set the number of ticks to wait between commands sent to the modem }

procedure mSetModemCmdTimeout(Modem : PModemRec; Ticks : Cardinal);
  {-Set the number of ticks to wait for a modem response }

procedure mSetModemDTRDropHold(Modem : PModemRec; Ticks : Cardinal);
  {-Set the number of ticks to hold DTR low during hangup }

procedure mSetModemCharDelay(Modem : PModemRec; Ticks : Cardinal);
  {-Set the number of ticks to wait between each command character sent }

procedure mSetTildeDelay(Modem : PModemRec; Ticks : Cardinal);
  {-Set the number of ticks to wait when a '~' is encountered in a command }

procedure mSetRingWaitTimeout(Modem : PModemRec; Ticks : Cardinal);
  {-Set the number of ticks to wait before mAutoAnswerModem resets }

function mStartModem(Modem : PModemRec) : Integer;
  {-Have the modem start processing messages }

procedure mStopModem(Modem : PModemRec);
  {-Have the modem stop processing messages }

function mPutModemCommand(Modem : PModemRec; Cmd : PChar) : Integer;
  {-Send a command to the modem, dispatching an error code }

function mRegisterModemHandler(Modem : PModemRec; HWindow : TApdHwnd; Notify : TApdNotifyProc) : Integer;
  {-Add a window/notification procedure to the modem's notify list }

function mDeregisterModemHandler(Modem : PModemRec; HWindow : TApdHwnd; Notify : TApdNotifyProc) : Integer;
  {-Remove a window/notification procedure from the modem's notify list }

function mInitializeModem(Modem : PModemRec) : Integer;
  {-Send the initialization string to the modem }

function mConfigureModem(Modem : PModemRec) : Integer;
  {-Send the configuration strings to the modem }

function mDialModem(Modem : PModemRec; Number : PChar) : Integer;
  {-Dial the modem }

function mIsAttemptingConnect(Modem : PModemRec) : Bool;
  {-Return TRUE if the modem is attempting to establish a connection }

function mExtendConnectAttempt(Modem : PModemRec; DeltaSecs : Integer) : Integer;
  {-Extend the amount of time the modem waits for a CONNECT result }

function mModemStarted(Modem : PModemRec) : Bool;
  {-Return TRUE if StartModem has been called }

function mCancelDialAnswer(Modem : PModemRec) : Integer;
  {-Cancel the dial or answer in progress }

function mGetConnectSpeed(Modem : PModemRec) : LongInt;
  {-Get the actual speed of the connection }

function mHangupModem(Modem : PModemRec) : Integer;
  {-Hangup the modem }

function mAnswerModem(Modem : PModemRec) : Integer;
  {-Answer the modem }

function mAutoAnswerModem(Modem : PModemRec; Rings : Cardinal) : Integer;
  {-Answer the modem after Rings rings }

function mWaitOnFeatures(Modem : PModemRec) : Integer;
  {-Wait until all modem features have been received }

function mAllFeatureWaitOver(Modem : PModemRec) : Bool;
  {-Return TRUE if all modem features have been received and processed }

function mWaitOnResponse(Modem : PModemRec) : Integer;
  {-Wait until the modem finishes processing the last command }

function mGetLastMessage(Modem : PModemRec) : Cardinal;
  {-Return the last message dispatched to modem clients }

implementation

  { non-public modem routines }

  function StrNewCheck(var NewSt : PChar; SrcStr : PChar) : Bool;
    {-Allocate a new string on the heap, checking for available memory }
  var
    Len : Cardinal;

  begin
    Len := StrLen(SrcStr);
    NewSt := AllocMem(Len + 1);

    StrCopy(NewSt, SrcStr);
    StrNewCheck := True;
  end;

  procedure StrDisposeCheck(var St : PChar);
  begin
    if (St <> nil) then
      FreeMem(St, StrLen(St) + 1);
  end;

  procedure DoneModemDynamic(Modem : PModemRec);
    {-Dispose of all dynamic modem data }
  var
    I : Cardinal;
    T : PModemRegisterList;
    N : PModemRegisterList;

  begin
    with Modem^ do begin
      StrDisposeCheck(InitCmd);
      StrDisposeCheck(DialCmd);
      StrDisposeCheck(DialTerm);
      StrDisposeCheck(DialCancel);
      StrDisposeCheck(HangupCmd);
      StrDisposeCheck(ConfigCmd);
      StrDisposeCheck(AnswerCmd);

      for I := 1 to NumResponses do
        StrDisposeCheck(Responses[I].Response);
      for I := 1 to NumErrors do
        StrDisposeCheck(ErrorTags[I].Response);
      for I := 1 to NumComps do
        StrDisposeCheck(CompressTags[I].Response);

      T := RegisterHead;
      while (T <> nil) do begin
        N := T^.mrNext;
        FreeMem(T, SizeOf(TModemRegisterList));
        T := N;
      end;
    end;
  end;

type
  ModemResponseSet = Set of Byte;

const
  RspWaitSet  = [RspOK, RspError];
  DialWaitSet = [RspConnect, RspBusy, RspVoice, RspNoCarrier, RspNoDialTone, RspError];

  function EnableResponses(Modem : PModemRec; Responses : ModemResponseSet) : Integer;
    {-Enable one or more modem responses }
  var
    Code : Integer;
    I    : Cardinal;
    J    : Cardinal;

  begin
    for I := 1 to NumResponses do
      if I in Responses then
        with Modem^ do
          if (not Responses[I].Enabled) and
             (Responses[I].Response <> nil) then begin
            Code := Port.AddDataTrigger(Responses[I].Response, True);
            if (Code < ecOK) then begin
              EnableResponses := Code;
              for J := Pred(I) downto 1 do begin
                Port.RemoveTrigger(Responses[I].TriggerIdx);
                Responses[I].TriggerIdx := 0;
                Responses[I].Enabled    := False;
              end;
              Exit;
            end;
            Responses[I].TriggerIdx := Code;
            Responses[I].Enabled    := True;
          end;
    EnableResponses := ecOK;
  end;

  function DisableResponses(Modem : PModemRec; Responses : ModemResponseSet) : Integer;
    {-Disable one or more modem responses }
  var
    RetCode : Integer;
    I       : Cardinal;

  begin
    RetCode := ecOK;
    for I := 1 to NumResponses do
      if I in Responses then
        with Modem^ do
          if Responses[I].Enabled then begin
            Port.RemoveTrigger(Responses[I].TriggerIdx);

            Responses[I].TriggerIdx := 0;
            Responses[I].Enabled    := False;
          end;
    DisableResponses := RetCode;
  end;

  function EnableFeatureTags(Modem : PModemRec) : Integer;
  var
    Code : Integer;
    I    : Cardinal;
    J    : Cardinal;

  begin
    EnableFeatureTags := ecOK;

    with Modem^ do begin
      for I := 1 to NumErrors do
        if not ErrorTags[I].Enabled then begin
          Code := Port.AddDataTrigger(ErrorTags[I].Response, False);
          if (Code < ecOK) then begin
            EnableFeatureTags := Code;
            for J := Pred(I) downto 1 do begin
              Port.RemoveTrigger(ErrorTags[I].TriggerIdx);
              ErrorTags[I].TriggerIdx := 0;
              ErrorTags[I].Enabled    := False;
            end;

            Exit;
          end;
          ErrorTags[I].TriggerIdx := Code;
          ErrorTags[I].Enabled    := True;
        end;

      for I := 1 to NumComps do
        if not CompressTags[I].Enabled then begin
          Code := Port.AddDataTrigger(CompressTags[I].Response, False);
          if (Code < ecOK) then begin
            EnableFeatureTags := Code;
            for J := Pred(I) downto 1 do begin
              Port.RemoveTrigger(CompressTags[I].TriggerIdx);
              CompressTags[I].TriggerIdx := 0;
              CompressTags[I].Enabled    := False;
            end;
            Exit;
          end;
          CompressTags[I].TriggerIdx := Code;
          CompressTags[I].Enabled    := True;
        end;
    end;
  end;

  function DisableErrorCorrectionTags(Modem : PModemRec) : Integer;
  var
    RetCode : Integer;
    I       : Cardinal;

  begin
    RetCode := ecOK;

    with Modem^ do
      for I := 1 to NumErrors do
        if ErrorTags[I].Enabled then begin
          Port.RemoveTrigger(ErrorTags[I].TriggerIdx);

          ErrorTags[I].TriggerIdx := 0;
          ErrorTags[I].Enabled    := False;
        end;
    DisableErrorCorrectionTags := RetCode;
  end;

  procedure DisableDataCompressionTags(Modem : PModemRec);
  var
    I       : Cardinal;
  begin
    with Modem^ do
      for I := 1 to NumComps do
        if CompressTags[I].Enabled then begin
          Port.RemoveTrigger(CompressTags[I].TriggerIdx);

          CompressTags[I].TriggerIdx := 0;
          CompressTags[I].Enabled    := False;
        end;
  end;

  function DisableFeatureTags(Modem : PModemRec) : Integer;
  var
    RetCode : Integer;
  begin
    RetCode := DisableErrorCorrectionTags(Modem);
    DisableDataCompressionTags(Modem);
    DisableFeatureTags := RetCode;
  end;

  function ModemPutXlatStr(Modem : PModemRec; Str : PChar) : Integer;
    {-Send a string to the modem, XLATing control chars }
  var
    I     : Cardinal;
    Len   : Cardinal;
    Code  : Integer;
    Delay : Bool;

  begin
    ModemPutXlatStr := ecOK;
    Len := StrLen(Str);
    if (Len = 0) then
      Exit;

    with Modem^ do begin
      Dec(Len);
      I := 0;
      while (I <= Len) do begin
        Code  := ecOK;
        Delay := True;
        case Str[I] of
          '^':
            if (I <> Len) and (UpCase(Str[I+1]) in ['@'..'_']) then begin
              Inc(I);
              Code := Port.PutChar(Char(Ord(UpCase(Str[I])) - Ord('A') + 1));
            end else
              Code := Port.PutChar(Str[I]);
          '~':
            begin
              DelayTicks(TildeDelay, True);
              Delay := False;
            end;
          else
            Code := Port.PutChar(Str[I]);
        end;

        if (Code < ecOK) then begin
          ModemPutXlatStr := Code;
          Exit;
        end;

        Inc(I);
        if Delay and (I <> Len) then
          if (ModemCharDelay <> 0) then
            DelayTicks(ModemCharDelay, True);
      end;
    end;
  end;

  {$IFDEF DispatchDebug}
  function TriggerTypeStr(Msg : Cardinal) : String;
    {-Return a string based on the message number in Msg }
  begin
    case Msg of
      apw_TriggerAvail : TriggerTypeStr := 'data available';
      apw_TriggerData  : TriggerTypeStr := 'data string received';
      apw_TriggerTimer : TriggerTypeStr := 'timer trigger';
      apw_TriggerStatus: TriggerTypeStr := 'change in line status';
      else
        TriggerTypeStr := 'unknown message';
    end;
  end;

  function ModemStateStr(State : Integer) : String;
    {-Return a string based on a modem state }
  begin
    case State of
      msNone        : ModemStateStr := 'doing nothing';
      msCmdWaitRsp  : ModemStateStr := 'waiting for a command response';
      msDialing     : ModemStateStr := 'dialing';
      msWaitNumber  : ModemStateStr := 'waiting for a baud rate from the connect string';
      msWaitTerm    : ModemStateStr := 'waiting for a carriage return to finish the baud rate';
      msAnswering   : ModemStateStr := 'answering the phone';
      msWaitRing    : ModemStateStr := 'waiting for the phone to RING';
      msWaitFeatures: ModemStateStr := 'waiting for a data compression/error correction tag';
      else
        ModemStateStr := 'doing something, but I don''t know what';
    end;
  end;

  function ModemMessageStr(Msg : Cardinal) : String;
    {-Return a string based on a modem message }
  begin
    case Msg of
      apw_ModemOK            : ModemMessageStr := 'OK result';
      apw_ModemConnect       : ModemMessageStr := 'Connect result';
      apw_ModemBusy          : ModemMessageStr := 'Busy result';
      apw_ModemVoice         : ModemMessageStr := 'Voice result';
      apw_ModemNoCarrier     : ModemMessageStr := 'No carrier result';
      apw_ModemNoDialTone    : ModemMessageStr := 'No dialtone result';
      apw_ModemError         : ModemMessageStr := 'Error result';
      apw_GotLineSpeed       : ModemMessageStr := 'Line speed received';
      apw_GotErrCorrection   : ModemMessageStr := 'Error correction tag received';
      apw_GotDataCompression : ModemMessageStr := 'Data compression tag received';
      apw_CmdTimeout         : ModemMessageStr := 'Timed out waiting for command response';
      apw_DialTimeout        : ModemMessageStr := 'Timed out waiting for carrier on dial attempt';
      apw_AnswerTimeout      : ModemMessageStr := 'Timed out waiting for carrier on answer attempt';
      apw_DialCount          : ModemMessageStr := 'Countdown timer for dial';
      apw_AnswerCount        : ModemMessageStr := 'Countdown timer for answer';
      apw_ModemRing          : ModemMessageStr := 'RING result';
      apw_ModemIsConnected   : ModemMessageStr := 'Modem has established a connection (definitive)';
      apw_ConnectFailed      : ModemMessageStr := 'Connection attempt failed (definitive)';
      apw_CommandProcessed   : ModemMessageStr := 'Modem command has been processed (definitive)';
      else
        ModemMessageStr := 'Unknown message';
    end;
  end;
  {$ENDIF}

  procedure ModemDispatcher(Msg, wParam : Cardinal; lParam : LongInt); far;
    {-Receive data from the comm dispatcher and turn it into something
      meaningful. }
  var
    Modem     : PModemRec;
    OnChar    : Integer;      {pPeekChar level}
    NumChars  : Integer;
    TrigIdx   : Cardinal absolute wParam;

    function InitializeCall : Bool;
      {-Initialize the modem dispatcher variables }
    var
      ComData : TApdBaseDispatcher;

    begin
      { the comm port handle is in the high Cardinal of lParam }

      { check that the handle is valid and that a modem is using it }
      ComData := TApdBaseDispatcher(PortList[LH(lParam).H]);
      if (ComData = nil) or (ComData.DataPointers[dpModem] = nil) then
        InitializeCall := False
      else begin
        InitializeCall := True;
        Modem := PModemRec(ComData.DataPointers[dpModem]);
        if (Msg = apw_TriggerAvail) then begin
          OnChar   := 1;
          NumChars := wParam;

        end;

      end;
    end;

    function CheckModemState(State : Integer) : Bool;
      {-See if the modem is doing anything }
    begin
      if (State = msNone) then begin
        CheckModemState := False;
        {$IFDEF DispatchDebug}
        DebugOut('Dispatcher exiting: nothing to do!');
        DebugOutTagInt('TrigIdx = ', TrigIdx);
        {$ENDIF}
      end else begin
        CheckModemState := True;
        {$IFDEF DispatchDebug}
        DebugOut('Modem is ' + ModemStateStr(State));
        {$ENDIF}
      end;
    end;

    procedure DispatchToModemClients(DispatchMsg, wParam : Cardinal; lParam : LongInt);
      {-Dispatch a message to all modem clients }
    var
      CurRegRec  : PModemRegisterList;
      Next       : PModemRegisterList;
      Prev       : PModemRegisterList;
      AnyDeleted : Bool;

    begin
      with Modem^ do begin
        {$IFDEF DispatchDebug}
        DebugOutTag('Dispatching message to clients: ', ModemMessageStr(DispatchMsg));
        {$ENDIF}

        LastMessage := DispatchMsg;
        AnyDeleted  := False;
        CurRegRec   := RegisterHead;

        while (CurRegRec <> nil) do begin
          Next := CurRegRec^.mrNext;

          if not CurRegRec^.mrDeleted then begin
            with CurRegRec^ do
              if (@mrNotify <> nil) then
                if not DelphiComponent then
                  mrNotify(DispatchMsg, wParam, lParam)
                else
                  mrNotify(DispatchMsg, wParam, LongInt(Modem))
              else if (mrHWindow <> 0) then
                SendMessage(mrHWindow, DispatchMsg, wParam, lParam);
          end else
            AnyDeleted := True;

          CurRegRec := Next;
        end;

        if AnyDeleted then begin
          repeat
            if RegisterHead^.mrDeleted then begin
              CurRegRec    := RegisterHead;
              RegisterHead := RegisterHead^.mrNext;
              FreeMem(CurRegRec, SizeOf(TModemRegisterList));
            end;
          until (RegisterHead = nil) or not RegisterHead^.mrDeleted;

          if (RegisterHead <> nil) then begin
            Prev      := RegisterHead;
            CurRegRec := RegisterHead^.mrNext;
            while (CurRegRec <> nil) do begin
              if CurRegRec^.mrDeleted then begin
                Prev^.mrNext := CurRegRec^.mrNext;
                FreeMem(CurRegRec, SizeOf(TModemRegisterList));
                CurRegRec := Prev;
              end;

              Prev      := CurRegRec;
              CurRegRec := CurRegRec^.mrNext;
            end;
          end;
        end;
      end;
    end;

    procedure DispatchToModemClientsNoParm(DispatchMsg : Cardinal);
    begin
      DispatchToModemClients(DispatchMsg, 0, 0);
    end;

    function FindResponseCode : Integer;
      {-Find a response code by its trigger index }
    var
      I : Integer;

    begin
      for I := 1 to NumResponses do
        if (Modem^.Responses[I].TriggerIdx = TrigIdx) then begin
          FindResponseCode := I;
          Exit;
        end;
      FindResponseCode := 0;
    end;

    procedure ShutdownResponseWait;
      {-Remove all triggers associated with the msCmdWaitRsp state }
    begin
      with Modem^ do begin
        {$IFDEF DispatchDebug}
        DebugOut('Shutting down wait for command response');
        {$ENDIF}

        DisableResponses(Modem, RspWaitSet);
        Port.RemoveTrigger(TimeoutIdx);

        ModemState := msNone;
        TimeoutIdx := 0;
      end;
    end;

    procedure HandleCommandResponse;
      {-Wait for an OK or ERROR response from the modem }
    var
      ResponseCode : Integer;

    begin
      with Modem^ do
        case Msg of
          apw_TriggerTimer:
            if (TrigIdx = TimeoutIdx) then begin

              { we haven't received an OK or an ERROR yet, so }
              { give up trying to find one and send an error  }
              { message to our clients.                       }

              DispatchToModemClientsNoParm(apw_CmdTimeout);
              ShutdownResponseWait;
              DispatchToModemClients(apw_CommandProcessed, cpTimeout, 0);
            end; { if }

          apw_TriggerData:
            begin

              { we've receive an OK or an error response }
              { find out which and act accordingly       }

              ResponseCode := FindResponseCode;

              case ResponseCode of
                RspOK:
                  begin
                    { send an OK message to clients }
                    DispatchToModemClientsNoParm(apw_ModemOK);
                    ShutdownResponseWait;
                    DispatchToModemClients(apw_CommandProcessed, cpOK, 0);
                  end; { RspOK }
                RspError:
                  begin
                    { send an ERROR message to clients }
                    DispatchToModemClientsNoParm(apw_ModemError);
                    ShutdownResponseWait;
                    DispatchToModemClients(apw_CommandProcessed, cpError, 0);
                  end; { RspError }
              end; { case ResponseCode }
            end; { apw_TriggerData }
        end; { case Msg }
    end; { procedure HandleCommandResponse }

    procedure AbortConnectAttempt(SendCancel : Bool);
      {-Remove all triggers associated with the connection attempt and
        optionally send the cancel command to the modem }
    {var
      Code : Integer;}

    begin
      with Modem^ do begin
        {$IFDEF DispatchDebug}
        DebugOut('Aborting connect attempt');
        {$ENDIF}

        if SendCancel then begin
          DelayTicks(DelayFactor * 2, True);
          mCancelDialAnswer(Modem);
        end;

        DisableResponses(Modem, DialWaitSet);
        DisableErrorCorrectionTags(Modem);
        DisableDataCompressionTags(Modem);
        Port.RemoveTrigger(TimeoutIdx);

        ModemState   := msNone;
        ConnectSpeed := 0;
        TimeoutIdx   := 0;
        DispatchToModemClientsNoParm(apw_ConnectFailed);
      end;
    end;

    procedure EstablishConnection;
      {-Let our clients know that we've received a connection and
        start waiting for the baud rate and any remaining feature
        tags }
    var
      Code : Integer;

    begin
      with Modem^ do begin
        {$IFDEF DispatchDebug}
        DebugOut('Establishing a connection');
        {$ENDIF}

        Code := DisableResponses(Modem, DialWaitSet);
        if (Code < ecOK) then begin
          ConnectSpeed := 0;
          ModemState   := msNone;
          {Port.aGotError(Code);}
          DispatchToModemClientsNoParm(apw_ConnectFailed);
          Exit;
        end;

        Port.RemoveTrigger(TimeoutIdx);

        { start waiting for baud rate }
        ModemState := msWaitNumber;
        FillChar(TentativeLineSpeed, SizeOf(TentativeLineSpeed), 0);
        TentativeLineIdx := 0;

        { establish a timeout for baud rate }
        Code := Port.AddTimerTrigger;
        if (Code < ecOK) then begin
          {Port.aGotError(Code);}
          ConnectSpeed := 0;
          ModemState   := msNone;
          TimeoutIdx   := 0;
          DispatchToModemClientsNoParm(apw_ConnectFailed);
        end else begin
          TimeoutIdx := Code;
          Code := Port.SetTimerTrigger(TimeoutIdx, BaudWait, True);
          if (Code < ecOK) then begin
            {Port.aGotError(Code);}
            ConnectSpeed := 0;
            ModemState   := msNone;
            DispatchToModemClientsNoParm(apw_ConnectFailed);
          end else
            DispatchToModemClientsNoParm(apw_ModemConnect);
        end;
      end;
    end;

    procedure CheckModemFeaturesPrim;
      {-Cycle through all error correction tags and data compression tags
        to see if we've received one.  If so, set the appropriate modem flag
        and continue waiting for the connect response }
    var
      Code : Integer;

      function IsErrorCorrectionTag : Bool;
        {-Return TRUE if we've received an error correction tag }
      var
        I : Cardinal;

      begin
        with Modem^ do
          for I := 1 to NumErrors do
            if (ErrorTags[I].TriggerIdx = TrigIdx) then begin
              IsErrorCorrectionTag := True;
              Exit;
            end;
        IsErrorCorrectionTag := False;
      end;

      function IsDataCompressionTag : Bool;
        {-Return TRUE if we've received a data compression tag }
      var
        I : Cardinal;

      begin
        with Modem^ do
          for I := 1 to NumComps do
            if (CompressTags[I].TriggerIdx = TrigIdx) then begin
              IsDataCompressionTag := True;
              Exit;
            end;
        IsDataCompressionTag := False;
      end;

    begin
      if IsErrorCorrectionTag then begin
        {$IFDEF DispatchDebug}
        DebugOut('Got an error correction tag');
        {$ENDIF}

        Code := DisableErrorCorrectionTags(Modem);
        if (Code < ecOK) then begin
          {Modem^.Port.aGotError( Code);}
          Exit;
        end;
        DispatchToModemClientsNoParm(apw_GotErrCorrection);
        Modem^.ErrorCorrection := True;
      end else if IsDataCompressionTag then begin
        {$IFDEF DispatchDebug}
        DebugOut('Got a data compression tag');
        {$ENDIF}

        DisableDataCompressionTags(Modem);
        DispatchToModemClientsNoParm(apw_GotDataCompression);
        Modem^.DataCompression := True;
      end;
    end;

    procedure CheckModemFeatures;
    begin
      with Modem^ do
        case Msg of
          apw_TriggerData:
            begin
              CheckModemFeaturesPrim;
              if ErrorCorrection and DataCompression then begin
                {$IFDEF DispatchDebug}
                DebugOut('All modem features received');
                {$ENDIF}
                Port.RemoveTrigger(TimeoutIdx);
                ModemState   := msNone;
                DispatchToModemClientsNoParm(apw_ModemIsConnected)
              end; { if }
            end; { apw_TriggerData }

          apw_TriggerTimer:
            if (TrigIdx = TimeoutIdx) then begin
              {$IFDEF DispatchDebug}
              DebugOut('Giving up waiting for features');
              {$ENDIF}

              DisableFeatureTags(Modem);
              Port.RemoveTrigger(TimeoutIdx);
              ModemState   := msNone;
              DispatchToModemClientsNoParm(apw_ModemIsConnected);
            end; { apw_TriggerTimer }
        end; { case }
    end;

    procedure HandleConnectionAttemptResponse;
      {-The modem is attempting to connect, be it via an answer or
        a dial.  This routine processes the responses that can
        be returned during a connect attempt }
    var
      ResponseCode : Integer;
      Code         : Integer;
      WasDialing   : Bool;

    begin
      with Modem^ do
        case Msg of
          apw_TriggerTimer:
            if (TrigIdx = TimeoutIdx) then begin

              { decrement the countdown value.  If it's zero, then    }
              { the dial attempt has failed, otherwise, notify our    }
              { clients that they should update their status displays }

              Dec(CountDown);
              if (CountDown = 0) then begin
                WasDialing := (ModemState = msDialing);
                AbortConnectAttempt(True);
                if WasDialing then
                  DispatchToModemClientsNoParm(apw_DialTimeout)
                else
                  DispatchToModemClientsNoParm(apw_AnswerTimeout);
              end else begin
                { reset the timer trigger }
                Code := Port.SetTimerTrigger(TimeoutIdx, TickSeconds, True);
                if (Code < ecOK) then begin
                  AbortConnectAttempt(True);
                  Exit;
                end;

                if (ModemState = msDialing) then
                  DispatchToModemClients(apw_DialCount, CountDown, 0)
                else
                  DispatchToModemClients(apw_AnswerCount, CountDown, 0);
              end; { else }
            end; { apw_TriggerTimer }

          apw_TriggerData:
            begin

              { we've received some kind of a message from the }
              { modem.  find out which and act accordingly     }

              ResponseCode := FindResponseCode;
              if (ResponseCode <> 0) then

                { we've received a response, not an error correction }
                { or data compression tag.                           }

                case ResponseCode of
                  RspConnect: EstablishConnection;

                  RspBusy:
                    begin
                      { the modem we're trying to call is busy.      }
                      { abort the connect attempt and notify clients }
                      AbortConnectAttempt(True);
                      DispatchToModemClientsNoParm(apw_ModemBusy);
                    end; { RspBusy }

                  RspVoice:
                    begin
                      AbortConnectAttempt(False);
                      DispatchToModemClientsNoParm(apw_ModemVoice);
                    end; { RspVoice }

                  RspNoCarrier:
                    begin
                      AbortConnectAttempt(False);
                      DispatchToModemClientsNoParm(apw_ModemNoCarrier);
                    end; { RspNoCarrier }

                  RspNoDialTone:
                    begin
                      AbortConnectAttempt(False);
                      DispatchToModemClientsNoParm(apw_ModemNoDialTone);
                    end; { RspNoDialTone }

                  RspError:
                    begin
                      AbortConnectAttempt(False);
                      DispatchToModemClientsNoParm(apw_ModemError);
                    end; { RspError }
                end { case ResponseCode }
              else
                CheckModemFeaturesPrim;
            end; { apw_TriggerData }
        end; { case Msg }
    end; { procedure }

    function ModemGetChar(var Ch : Char) : Integer;
      {-Get a character from the modem }
    begin
      with Modem^ do begin
        ModemGetChar := Port.GetChar(Ch);

        if (ModemState <> msNone) then
          if (Ch = #10) or (Ch = #13) then
            BlankPending := True
          else begin
            if BlankPending then begin
              FillChar(LastString, SizeOf(LastString), 0);
              LastStringLen := 0;
              BlankPending := False;                                
            end;
            if (LastStringLen < Pred(SizeOf(LastString))) then begin
              LastString[LastStringLen] := Ch;
              Inc(LastStringLen);
            end;
          end;
        {$IFDEF DispatchDebug}
        DebugOut('Got char from modem: ' + Ch);
        {$ENDIF}

        Inc(OnChar);
      end;
    end;

    procedure ShutDownSpeedWait;
      {-Stop waiting for speed indicators from the modem }
    {var
      Code : Integer;}

    begin
      with Modem^ do begin
        {$IFDEF DispatchDebug}
        DebugOut('Shutting down DTE rate wait');
        {$ENDIF}
        Port.RemoveTrigger(TimeoutIdx);
        ModemState := msNone;
      end;
    end;

    procedure UpdateLineSpeed;
      {-Change the line speed, if required, to the received baud rate }
    {var
      Code : Integer;}

    begin
      with Modem^ do
        if not LockDTE then begin
          {$IFDEF DispatchDebug}
          DebugOut('Updating line speed');
          {$ENDIF}
          Port.SetLine(ConnectSpeed, DontChangeParity,
                           DontChangeDataBits, DontChangeStopBits);
        end;
    end;

    procedure EnterFeatureWait;
      {-Enter the msWaitFeatures state }
    var
      Code : Integer;

    begin
      with Modem^ do begin
        {$IFDEF DispatchDebug}
        DebugOut('Enabling feature wait');
        {$ENDIF}
        ModemState := msNone;

        Code := Port.AddTimerTrigger;
        if (Code < ecOK) then begin
          DispatchToModemClientsNoParm(apw_ConnectFailed);
          Exit;
        end;

        TimeoutIdx := Code;

        Code := Port.SetTimerTrigger(TimeoutIdx, FeatureWait, True);
        if (Code < ecOK) then begin
          Port.RemoveTrigger(TimeoutIdx);
          DispatchToModemClientsNoParm(apw_ConnectFailed);
          Exit;
        end;

        ModemState := msWaitFeatures;
      end;
    end;

    procedure GetLineSpeedStart;
      {-Get the beginning of the line speed from the modem }
    var
      Ch       : Char;
      IsNumber : Bool;
      Code     : Integer;

    begin
      with Modem^ do
        case Msg of
          apw_TriggerTimer:
            if (TrigIdx = TimeoutIdx) then begin
              { we haven't received a baud rate, assume 300 baud }
              ShutdownSpeedWait;
              ConnectSpeed := 300;
              UpdateLineSpeed;

              DispatchToModemClients(apw_GotLineSpeed, 0, ConnectSpeed);

              { wait half a second for any feature tags that may yet come in }
              EnterFeatureWait;
            end; { if }

          apw_TriggerData:
            { if we haven't received all feature tags yet, check for them }
            if not (ErrorCorrection and DataCompression) then
              CheckModemFeaturesPrim;

          apw_TriggerAvail:
            begin
              repeat
                { try to find the beginning of the baud rate }
                Code := ModemGetChar(Ch);
                if (Code < ecOK) then begin
                  ShutDownSpeedWait;
                  {Port.aGotError(Code);}
                  DispatchToModemClientsNoParm(apw_ConnectFailed);
                  Exit;
                end; { if }

                {$IFDEF DispatchDebug}
                DebugOut('Waiting for #, got ' + Ch);
                {$ENDIF}

                IsNumber := (Ch >= '0') and (Ch <= '9');
              until (OnChar > NumChars) or IsNumber;

              if IsNumber then begin
                TentativeLineIdx      := 1;
                TentativeLineSpeed[0] := Ch;

                ModemState := msWaitTerm;
              end else
                Exit;

              ModemState := msWaitTerm;
            end; { apw_TriggerAvail }
        end; { case }
    end; { procedure }

    procedure GetLineSpeedEnd;
      {-Get the rest of the line speed from the modem }
    var
      Code     : Integer;
      Ch       : Char;
      IsNumber : Bool;

    begin
      with Modem^ do
        case Msg of
          apw_TriggerTimer:
            if (TrigIdx = TimeoutIdx) then begin
              { we haven't received a baud rate, assume 300 baud }
              ShutdownSpeedWait;
              ConnectSpeed := 300;
              UpdateLineSpeed;

              { wait half a second for any feature tags that may yet come in }
              EnterFeatureWait;
            end; { if }

          apw_TriggerData:
            { if we haven't received all feature tags yet, check for them }
            if not (ErrorCorrection and DataCompression) then
              CheckModemFeaturesPrim;

          apw_TriggerAvail:
            begin
              repeat
                Code := ModemGetChar(Ch);
                if (Code < ecOK) then begin
                  ShutdownSpeedWait;
                  {Port.aGotError(Code);}
                  DispatchToModemClientsNoParm(apw_ConnectFailed);
                  Exit;
                end;

                {$IFDEF DispatchDebug}
                DebugOut('Waiting for terminator, got ' + Ch);
                {$ENDIF}

                TentativeLineSpeed[TentativeLineIdx] := Ch;
                Inc(TentativeLineIdx);

                if (TentativeLineIdx < TentBaudLen) then
                  IsNumber := (Ch >= '0') and (Ch <= '9')
                else
                  IsNumber := True;
              until (OnChar > NumChars) or not IsNumber;

              { if the last character was not a number, then we got the rate }
              if not IsNumber then begin
                TentativeLineSpeed[TentativeLineIdx-1] := #0;

                {$IFDEF DispatchDebug}
                DebugOutTagSt('Got line speed: ', TentativeLineSpeed);
                {$ENDIF}

                { can't fail except for memory overwrite }
                Str2LongZ(TentativeLineSpeed, ConnectSpeed);
                UpdateLineSpeed;
                ShutdownSpeedWait;
                DispatchToModemClients(apw_GotLineSpeed, 0, ConnectSpeed);

                EnterFeatureWait;
              end;
            end; { apw_TriggerAvail }
        end; { case }
    end;

    function ShutDownRingWait : Bool;
    var
      Code : Integer;

    begin
      ShutDownRingWait := True;

      with Modem^ do begin
        {$IFDEF DispatchDebug}
        DebugOut('Disabling wait-for-ring');
        {$ENDIF}
        ModemState := msNone;
        Code := DisableResponses(Modem, [RspRing]);
        if (Code < ecOK) then begin
          {Port.aGotError(Code);}
          ShutDownRingWait := False;
        end;

        Port.RemoveTrigger(TimeoutIdx);
      end;
    end;

    procedure GotRing;
      {-Got a ring response during an auto answer }
    var
      Code : Integer;

    begin
      with Modem^ do
        case Msg of
          apw_TriggerData:
            if (TrigIdx = Responses[RspRing].TriggerIdx) then begin
              DispatchToModemClientsNoParm(apw_ModemRing);

              {$IFDEF DispatchDebug}
              DebugOut('Got a RING');
              {$ENDIF}
              Dec(RingWait);
              if (RingWait = 0) then begin
                {$IFDEF DispatchDebug}
                DebugOut('Got the LAST ring, starting answer');
                {$ENDIF}

                if ShutDownRingWait then begin
                  {$IFDEF DispatchDebug}
                  DebugOut('Starting answer');
                  {$ENDIF}

                  {Code := }mAnswerModem(Modem);
                  {if (Code < ecOK) then
                    Port.aGotError(Code);}
                end; { if }
              end else begin
                Code := Port.SetTimerTrigger(TimeoutIdx, RingWaitTimeout, True);
                if (Code < ecOK) then begin
                  ShutDownRingWait;
                end; { if }
              end; { else }
            end; { apw_TriggerData }

          apw_TriggerTimer:
            if wParam = TimeoutIdx then begin
              {$IFDEF DispatchDebug}
              DebugOut('Cancelling auto answer');
              {$ENDIF}
              RingWait := RingCnt;
            end; { apw_TriggerTimer }
        end; { case }
    end; { procedure }

    procedure SwallowData;
      {-Swallow incoming characters on an apw_TriggerAvail }
    var
      Code : Integer;
      Ch   : Char;

    begin
      with Modem^ do begin
        if (Msg <> apw_TriggerAvail) or
           (ModemState = msWaitNumber) or (ModemState = msWaitTerm) then
          Exit;

        {$IFDEF DispatchDebug}
        DebugOut('Swallowing extraneous characters');
        {$ENDIF}

        while (OnChar <= NumChars) do begin
          Code := ModemGetChar(Ch);
          if (Code < ecOK) then begin
            {Port.aGotError(Code);}
            Exit;
          end;
        end;
      end;
    end;

  begin
    {$IFDEF DispatchDebug}
    DebugOut('Entered dispatcher: ' + TriggerTypeStr(Msg));
    {$ENDIF}

    { if the com handle is invalid or there is no modem attached to }
    { the port sending this message, then exit                      }
    if not InitializeCall then begin
      {$IFDEF DispatchDebug}
      DebugOut('Dispatcher exiting: invalid com port or modem');
      {$ENDIF}
      Exit;
    end;

    with Modem^ do begin
      { make sure the modem is actually doing something before proceeding }
      if not CheckModemState(ModemState) then
        Exit;

      case ModemState of
        msCmdWaitRsp  : HandleCommandResponse;
        msDialing,
        msAnswering   : HandleConnectionAttemptResponse;
        msWaitNumber  : GetLineSpeedStart;
        msWaitTerm    : GetLineSpeedEnd;
        msWaitRing    : GotRing;
        msWaitFeatures: CheckModemFeatures;
      end;

      if (Msg = apw_TriggerAvail) and (ModemState = msWaitTerm) and (OnChar <= NumChars) then
        GetLineSpeedEnd;

      SwallowData;
    end;
  end;

{******************************************************************************}

  function mInitModem(var Modem : PModemRec; H : TApdBaseDispatcher; var Data : TModemData) : Integer;
    {-Initialize a modem }
  var
    I    : Cardinal;
    Code : Integer;

  begin
    Modem := AllocMem(SizeOf(TModemRec));

    { initialize pointers in structure so we can do a mass dispose later }
    FillChar(Modem^, SizeOf(Modem^), 0);

    with Modem^ do begin
      if not StrNewCheck(InitCmd, Data.Data.InitCmd) or
         not StrNewCheck(DialCmd, Data.Data.DialCmd) or
         not StrNewCheck(DialTerm, Data.Data.DialTerm) or
         not StrNewCheck(DialCancel, Data.Data.DialCancel) or
         not StrNewCheck(HangupCmd, Data.Data.HangupCmd) or
         not StrNewCheck(ConfigCmd, Data.Data.ConfigCmd) or
         not StrNewCheck(AnswerCmd, Data.Data.AnswerCmd) or
         not StrNewCheck(Responses[RspOk].Response, Data.Data.OkMsg) or
         not StrNewCheck(Responses[RspConnect].Response, Data.Data.ConnectMsg) or
         not StrNewCheck(Responses[RspBusy].Response, Data.Data.BusyMsg) or
         not StrNewCheck(Responses[RspVoice].Response, Data.Data.VoiceMsg) or
         not StrNewCheck(Responses[RspNoCarrier].Response, Data.Data.NoCarrierMsg) or
         not StrNewCheck(Responses[RspNoDialTone].Response, Data.Data.NoDialToneMsg) or
         not StrNewCheck(Responses[RspError].Response, Data.Data.ErrorMsg) or
         not StrNewCheck(Responses[RspRing].Response, Data.Data.RingMsg) then begin
        DoneModemDynamic(Modem);
        mInitModem := ecOutOfMemory;
        Exit;
      end;

      NumErrors := Data.NumErrors;
      for I := 1 to NumErrors do
        if not StrNewCheck(ErrorTags[I].Response, Data.Errors[I]) then begin
          DoneModemDynamic(Modem);
          mInitModem := ecOutOfMemory;
          Exit;
        end;

      NumComps := Data.NumComps;
      for I := 1 to NumComps do
        if not StrNewCheck(CompressTags[I].Response, Data.Compression[I]) then begin
          DoneModemDynamic(Modem);
          mInitModem := ecOutOfMemory;
          Exit;
        end;

      Code := H.SetDataPointer(Modem, dpModem);                    
      if (Code < ecOK) then begin
        DoneModemDynamic(Modem);
        mInitModem := Code;
        Exit;
      end;

      DialTimeout     := awmDefDialTimeout;
      AnswerTimeout   := awmDefAnswerTimeout;
      DelayFactor     := awmDefDelayFactor;
      CmdTimeout      := awmDefCmdTimeout;
      DTRDropHold     := awmDefDTRDropHold;
      ModemCharDelay  := awmDefModemCharDelay;
      TildeDelay      := awmDefTildeDelay;
      RingWaitTimeout := awmDefRingWaitTimeout;
      FeatureWait     := awmDefFeatureWait;
      BaudWait        := awmDefBaudWait;
      LockDTE         := Data.LockDTE;
      ModemStarted    := False;
      ModemState      := msNone;
      LastMessage     := wm_Null;
      Port            := H;
      UserData        := 0;
      DelphiComponent := False;
    end;

    mInitModem := ecOK;
  end;

  function mInitModemDelphi(var Modem : PModemRec; H : TApdBaseDispatcher; var Data : TModemData) : Integer;
    {-Initialize a modem}
  var
    Code : Integer;

  begin
    Code := mInitModem(Modem, H, Data);
    if (Code = ecOK) then
      Modem^.DelphiComponent := True;
    mInitModemDelphi := Code;
  end;

  procedure mDoneModem(var Modem : PModemRec);
    {-Destroy a modem }
  begin
    mStopModem(Modem);
    DoneModemDynamic(Modem);
    FreeMem(Modem, SizeOf(TModemRec));
    Modem := nil;                         
  end;

  function mGetComHandle(var Modem : PModemRec) : TApdBaseDispatcher;
    {-Return the handle of a modem's port }
  begin
    mGetComHandle := Modem^.Port;
  end;

  procedure mSetModemDialTimeout(Modem : PModemRec; Secs : Cardinal);
    {-Set the number of seconds before a dial attempt times out }
  begin
    Modem^.DialTimeout := Secs;
  end;

  function mGetModemDialTimeout(Modem : PModemRec) : Cardinal;
    {-Get the number of seconds the modem will wait before aborting a dial }
  begin
    mGetModemDialTimeout := Modem^.DialTimeout;
  end;

  procedure mSetModemAnswerTimeout(Modem : PModemRec; Secs : Cardinal);
    {-Set the number of seconds before an answer attempt times out }
  begin
    Modem^.AnswerTimeout := Secs;
  end;

  procedure mSetModemDelayFactor(Modem : PModemRec; Ticks : Cardinal);
    {-Set the number of ticks to wait between commands sent to the modem }
  begin
    Modem^.DelayFactor := Ticks;
  end;

  procedure mSetModemCmdTimeout(Modem : PModemRec; Ticks : Cardinal);
    {-Set the number of ticks to wait for a modem response }
  begin
    Modem^.CmdTimeout := Ticks;
  end;

  procedure mSetModemDTRDropHold(Modem : PModemRec; Ticks : Cardinal);
    {-Set the number of ticks to hold DTR low during hangup }
  begin
    Modem^.DTRDropHold := Ticks;
  end;

  procedure mSetModemCharDelay(Modem : PModemRec; Ticks : Cardinal);
    {-Set the number of ticks to wait between each command character sent }
  begin
    Modem^.ModemCharDelay := Ticks;
  end;

  procedure mSetTildeDelay(Modem : PModemRec; Ticks : Cardinal);
    {-Set the number of ticks to wait when a '~' is encountered in a command }
  begin
    Modem^.TildeDelay := Ticks;
  end;

  procedure mSetRingWaitTimeout(Modem : PModemRec; Ticks : Cardinal);
    {-Set the number of ticks to wait before mAutoAnswerModem resets }
  begin
    Modem^.RingWaitTimeout := Ticks;
  end;

  function mStartModem(Modem : PModemRec): Integer;
    {-Have the modem start processing messages }
  begin
    with Modem^ do begin
      if ModemStarted then begin
        mStartModem := ecModemBusy;
        Exit;
      end;

      Port.RegisterProcTriggerHandler(ModemDispatcher);
      Port.ChangeLengthTrigger(1);
      ModemStarted := True;
      mStartModem  := ecOK;
    end;
  end;

  procedure mStopModem(Modem : PModemRec);
    {-Have the modem stop processing messages }
  begin
    with Modem^ do begin
      if not ModemStarted then
        Exit;

      if mIsAttemptingConnect(Modem) then
        mCancelDialAnswer(Modem)
      else if (ModemState <> msNone) then begin
        DisableResponses(Modem, RspWaitSet);
        Port.RemoveTrigger(TimeoutIdx);
      end;

      Port.DeregisterProcTriggerHandler(ModemDispatcher);
      ModemStarted := False;
      ModemState   := msNone;
    end;
  end;

  function mPutModemCommand(Modem : PModemRec; Cmd : PChar) : Integer;
    {-Send a command to the modem, dispatching an error code }
  var
    Code : Integer;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mPutModemCommand := ecModemNotStarted;
        Exit;
      end;

      if (ModemState <> msNone) then begin
        mPutModemCommand := ecModemBusy;
        Exit;
      end;

      {initialize the last string}
      FillChar(LastString, SizeOf(LastString), 0);
      LastStringLen := 0;
      BlankPending := False;                                

      { enable response codes }
      Code := EnableResponses(Modem, RspWaitSet);
      if (Code < ecOK) then begin
        mPutModemCommand := Code;
        Exit;
      end;

      { add the timeout trigger }
      Code := Port.AddTimerTrigger;
      if (Code < ecOK) then begin
        DisableResponses(Modem, RspWaitSet);
        mPutModemCommand := Code;
        Exit;
      end;
      TimeoutIdx := Code;

      { send the command to the modem }
      Code := ModemPutXlatStr(Modem, Cmd);
      if (Code < ecOK) then begin
        DisableResponses(Modem, RspWaitSet);
        Port.RemoveTrigger(TimeoutIdx);
        mPutModemCommand := Code;
        Exit;
      end;

      { enable the timeout trigger }
      Code := Port.SetTimerTrigger(TimeoutIdx, CmdTimeout, True);
      if (Code < ecOK) then begin
        DisableResponses(Modem, RspWaitSet);
        Port.RemoveTrigger(TimeoutIdx);
        mPutModemCommand := Code;
        Exit;
      end;

      { tell the dispatcher we're waiting for a modem response }
      ModemState := msCmdWaitRsp;

      mPutModemCommand := ecOK;
    end;
  end;

  function mRegisterModemHandler(Modem : PModemRec; HWindow : TApdHwnd; Notify : TApdNotifyProc) : Integer;
    {-Add a window/notification procedure to the modem's notify list }
  var
    NewRegRec : PModemRegisterList;

  begin
    with Modem^ do begin
      NewRegRec := AllocMem(SizeOf(TModemRegisterList));

      with NewRegRec^ do begin
        mrHWindow := HWindow;
        mrNotify  := Notify;

        if (RegisterHead = nil) then
          RegisterHead := NewRegRec
        else begin
          NewRegRec^.mrNext := RegisterHead;
          RegisterHead      := NewRegRec;
        end;
      end;
    end;

    mRegisterModemHandler := ecOK;
  end;

  function mDeregisterModemHandler(Modem : PModemRec; HWindow : TApdHwnd; Notify : TApdNotifyProc) : Integer;
    {-Remove a window/notification procedure from the modem's notify list }
  var
    OnReg   : PModemRegisterList;

    function Matches : Bool;
    begin
      if (HWindow <> 0) then
        Matches := OnReg^.mrHWindow = HWindow
      else
        Matches := @OnReg^.mrNotify = @Notify;
    end;

  begin
    mDeregisterModemHandler := ecOK;

    with Modem^ do begin
      OnReg   := RegisterHead;

      while (OnReg <> nil) and not Matches do
        OnReg   := OnReg^.mrNext;

      if (OnReg <> nil) then
        OnReg^.mrDeleted := True;
    end;
  end;

  function mInitializeModem(Modem : PModemRec) : Integer;
    {-Send the initialization string to the modem }
  begin
    mInitializeModem := mPutModemCommand(Modem, Modem^.InitCmd);
  end;

  function mConfigureModem(Modem : PModemRec) : Integer;
    {-Send the configuration strings to the modem }
  var
    Code   : Integer;
    CmdPtr : PChar;
    SepPtr : PChar;
    OutCmd : TCmdStringZ;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mConfigureModem := ecModemNotStarted;
        Exit;
      end;

      if (ModemState <> msNone) then begin
        mConfigureModem := ecModemBusy;
        Exit;
      end;

      CmdPtr := ConfigCmd;
      while (CmdPtr <> nil) and (CmdPtr[0] <> #0) do begin
        SepPtr := StrScan(CmdPtr, CmdSepChar);
        if (SepPtr = nil) then begin
          StrCopy(OutCmd, CmdPtr);
          CmdPtr := nil;
        end else begin
          StrLCopy(OutCmd, CmdPtr, SepPtr - CmdPtr);
          CmdPtr := SepPtr + 1;
        end;

        if (OutCmd[0] <> #0) then begin
          Code := mPutModemCommand(Modem, OutCmd);
          if (Code < ecOK) then begin
            mConfigureModem := Code;
            Exit;
          end;

          Code := mWaitOnResponse(Modem);
          if (Code < ecOK) then begin
            mConfigureModem := Code;
            Exit;
          end;

          DelayTicks(DelayFactor, True);
        end;
      end;
    end;

    mConfigureModem := ecOK;
  end;

  function mDialModem(Modem : PModemRec; Number : PChar) : Integer;
    {-Dial the modem }
  var
    Code : Integer;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mDialModem := ecModemNotStarted;
        Exit;
      end;

      if (ModemState <> msNone) then begin
        mDialModem := ecModemBusy;
        Exit;
      end;

      if (Number[0] = #0) then begin
        mDialModem := ecOK;
        Exit;
      end;

      {initialize the last string}
      FillChar(LastString, SizeOf(LastString), 0);
      LastStringLen := 0;
      BlankPending := False;                               

      { initialize dialing variables }
      ErrorCorrection := False;
      DataCompression := False;
      ConnectSpeed    := 0;

      { enable dial response codes }
      Code := EnableResponses(Modem, DialWaitSet);
      if (Code < ecOK) then begin
        mDialModem := Code;
        Exit;
      end;

      Code := EnableFeatureTags(Modem);
      if (Code < ecOK) then begin
        DisableResponses(Modem, DialWaitSet);
        mDialModem := Code;
        Exit;
      end;

      { add the timeout trigger }
      Code := Port.AddTimerTrigger;
      if (Code < ecOK) then begin
        DisableResponses(Modem, DialWaitSet);
        DisableFeatureTags(Modem);
        mDialModem := Code;
        Exit;
      end;
      TimeoutIdx := Code;

      { send the dial command }
      Code := ModemPutXlatStr(Modem, DialCmd);
      if (Code = ecOK) then begin
        Code := ModemPutXlatStr(Modem, Number);
        if (Code = ecOK) then
          Code := ModemPutXlatStr(Modem, DialTerm);
      end;
      if (Code < ecOK) then begin
        DisableResponses(Modem, DialWaitSet);
        DisableFeatureTags(Modem);
        Port.RemoveTrigger(TimeoutIdx);
        mDialModem := Code;
        Exit;
      end;

      { enable the timeout trigger }
      Code := Port.SetTimerTrigger(TimeoutIdx, TickSeconds, True);
      if (Code < ecOK) then begin
        DisableResponses(Modem, DialWaitSet);
        DisableFeatureTags(Modem);
        Port.RemoveTrigger(TimeoutIdx);
        mDialModem := Code;
        Exit;
      end;

      ModemState := msDialing;
      CountDown  := DialTimeout;

      mDialModem := ecOK;
    end;
  end;

  function mIsAttemptingConnect(Modem : PModemRec) : Bool;
    {-Return TRUE if the modem is attempting to establish a connection }
  begin
    with Modem^ do
      mIsAttemptingConnect := ModemStarted and (ModemState in
        [msDialing, msAnswering, msWaitNumber, msWaitTerm, msWaitFeatures]);
  end;

  function mExtendConnectAttempt(Modem : PModemRec; DeltaSecs : Integer) : Integer;
    {-Extend the amount of time the modem waits for a CONNECT result }
  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mExtendConnectAttempt := ecModemNotStarted;
        Exit;
      end;

      if not mIsAttemptingConnect(Modem) then begin
        mExtendConnectAttempt := ecModemNotDialing;
        Exit;
      end;

      mExtendConnectAttempt := ecOK;
      if (ModemState = msAnswering) or ((ModemState = msDialing) and (DeltaSecs > 0)) then
        Inc(Modem^.CountDown, DeltaSecs);
    end;
  end;

  function mModemStarted(Modem : PModemRec) : Bool;
    {-Return TRUE if StartModem has been called }
  begin
    mModemStarted := Modem^.ModemStarted;
  end;

  function mCancelDialAnswer(Modem : PModemRec) : Integer;
    {-Cancel the dial in progress }
  begin
    mCancelDialAnswer := ecOK;

    with Modem^ do begin
      if not ModemStarted or
         ((ModemState <> msDialing) and
          (ModemState <> msWaitNumber) and
          (ModemState <> msWaitTerm) and
          (ModemState <> msAnswering) and
          (ModemState <> msWaitRing))
          then begin
        mCancelDialAnswer := ecModemNotDialing;
        Exit;
      end;

      if (ModemState <> msWaitRing) then begin
        mCancelDialAnswer := ModemPutXlatStr(Modem, DialCancel);
        DelayTicks(DelayFactor, False);
        DisableResponses(Modem, DialWaitSet);
        Port.RemoveTrigger(TimeoutIdx);
      end else begin
        Port.RemoveTrigger(TimeoutIdx);
        TimeoutIdx := 0;
        DisableResponses(Modem, [RspRing]);
      end;

      ModemState   := msNone;
      ConnectSpeed := 0;
    end;
  end;

  function mGetConnectSpeed(Modem : PModemRec) : LongInt;
    {-Get the actual speed of the connection }
  begin
    mGetConnectSpeed := Modem^.ConnectSpeed;
  end;

  function mHangupModem(Modem : PModemRec) : Integer;
  var
    Code : Integer;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mHangupModem := ecModemNotStarted;
        Exit;
      end;

      if (ModemState <> msNone) then begin
        mHangupModem := ecModemBusy;
        Exit;
      end;

      ConnectSpeed := 0;

      if (HangupCmd[0] = #0) or (StrIComp(HangupCmd, 'DTR') = 0) then begin
        Code := Port.SetDtr(False);
        if (Code < ecOK) then begin
          mHangupModem := Code;
          Exit;
        end;

        DelayTicks(DTRDropHold, True);

        Code := Port.SetDtr(True);
      end else
        Code := mPutModemCommand(Modem, HangupCmd);

      mHangupModem := Code;
    end;
  end;

  function mAnswerModem(Modem : PModemRec) : Integer;
    {-Answer the modem }
  var
    Code : Integer;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mAnswerModem := ecModemNotStarted;
        Exit;
      end;

      if (ModemState <> msNone) then begin
        mAnswerModem := ecModemBusy;
        Exit;
      end;

      {initialize the last string}
      FillChar(LastString, SizeOf(LastString), 0);
      LastStringLen := 0;
      BlankPending := False;                               

      ErrorCorrection := False;
      DataCompression := False;
      ConnectSpeed    := 0;

      { enable answer/dial response codes }
      Code := EnableResponses(Modem, DialWaitSet);
      if (Code < ecOK) then begin
        mAnswerModem := Code;
        Exit;
      end;

      Code := EnableFeatureTags(Modem);
      if (Code < ecOK) then begin
        mAnswerModem := Code;
        DisableResponses(Modem, DialWaitSet);
        Exit;
      end;

      { add the timeout trigger }
      Code := Port.AddTimerTrigger;
      if (Code < ecOK) then begin
        DisableResponses(Modem, DialWaitSet);
        DisableFeatureTags(Modem);
        mAnswerModem := Code;
        Exit;
      end;

      TimeoutIdx := Code;

      { send the answer command }
      Code := ModemPutXlatStr(Modem, AnswerCmd);
      if (Code < ecOK) then begin
        DisableResponses(Modem, DialWaitSet);
        DisableFeatureTags(Modem);
        Port.RemoveTrigger(TimeoutIdx);
        mAnswerModem := Code;
        Exit;
      end;

      { enable the timeout trigger }
      Code := Port.SetTimerTrigger(TimeoutIdx, TickSeconds, True);
      if (Code < ecOK) then begin
        DisableResponses(Modem, DialWaitSet);
        DisableFeatureTags(Modem);
        Port.RemoveTrigger(TimeoutIdx);
        mAnswerModem := Code;
        Exit;
      end;

      ModemState := msAnswering;
      CountDown  := AnswerTimeout;

      mAnswerModem := ecOK;
    end;
  end;

  function mAutoAnswerModem(Modem : PModemRec; Rings : Cardinal) : Integer;
    {-Answer the modem after Rings rings }
  var
    Code : Integer;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mAutoAnswerModem := ecModemNotStarted;
        Exit;
      end;

      if (ModemState <> msNone) then begin
        mAutoAnswerModem := ecModemBusy;
        Exit;
      end;

      if (Rings = 0) then begin
        mAutoAnswerModem := ecBadArgument;
        Exit;
      end;

      RingWait := Rings;
      RingCnt  := Rings;

      Code := EnableResponses(Modem, [RspRing]);
      if (Code < ecOK) then begin
        mAutoAnswerModem := Code;
        Exit;
      end;

      { set, but don't enable, a timeout trigger }
      Code := Port.AddTimerTrigger;
      if (Code < ecOK) then begin
        DisableResponses(Modem, [RspRing]);
        mAutoAnswerModem := Code;
        Exit;
      end;

      TimeoutIdx := Code;
      ModemState := msWaitRing;
      mAutoAnswerModem := ecOK;
    end;
  end;

  function mAllFeatureWaitOver(Modem : PModemRec) : Bool;
    {-Return TRUE if all modem features have been received and processed }
  begin
    with Modem^ do
      case ModemState of
        msDialing,
        msAnswering   : mAllFeatureWaitOver := ErrorCorrection and DataCompression;
        msWaitFeatures: mAllFeatureWaitOver := False;
        else
          mAllFeatureWaitOver := True;
      end;
  end;

  function mWaitOnFeatures(Modem : PModemRec) : Integer;
    {-Wait until all modem features have been received }
  var
    Msg : TMsg;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mWaitOnFeatures := ecModemNotStarted;
        Exit;
      end;

      mWaitOnFeatures := ecOK;
      while not mAllFeatureWaitOver(Modem) do
        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
          if (Msg.Message = wm_Quit) then begin
            PostQuitMessage(Msg.wParam);
            mWaitOnFeatures := ecGotQuitMsg;
            Exit;
          end else begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
    end;
  end;

  function mWaitOnResponse(Modem : PModemRec) : Integer;
    {-Wait until the modem finishes processing the last command }
  var
    Msg : TMsg;

  begin
    with Modem^ do begin
      if not ModemStarted then begin
        mWaitOnResponse := ecModemNotStarted;
        Exit;
      end;

      mWaitOnResponse := ecOK;
      while (ModemState <> msNone) do
        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
          if (Msg.Message = wm_Quit) then begin
            PostQuitMessage(Msg.wParam);
            mWaitOnResponse := ecGotQuitMsg;
            Exit;
          end else begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
    end;
  end;

  function mGetLastMessage(Modem : PModemRec) : Cardinal;
    {-Return the last message dispatched to modem clients }
  begin
    mGetLastMessage := Modem^.LastMessage;
  end;

end.
