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
{*                   ADMODEM.PAS 4.06                    *}
{*********************************************************}
{* Deprecated modem component                            *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-,I+}

unit AdModem;
  {-Delphi modem component}

interface

uses
  WinProcs,
  WinTypes,
  SysUtils,
  Classes,
  Messages,
  Forms,
  Controls,
  OoMisc,
  AwModem,
  AdExcept,
  AdPort,
  AdModDB;

const
  {defaults}
  admoDefAutoStart = True;

type
  TModemStatus = (msModemOK, msModemConnect, msModemBusy, msModemVoice,
                  msModemNoCarrier, msModemNoDialTone, msModemError,
                  msGotLineSpeed, msGotErrCorrection, msGotDataCompression,
                  msCmdTimeout, msDialTimeout, msAnswerTimeout, msDialCount,
                  msAnswerCount, msModemRing, msModemIsConnected, msConnectFailed,
                  msCommandProcessed);

  TLineSpeedEvent = procedure(M : TObject; Speed : LongInt) of object;
  TConnectCountEvent = procedure(M : TObject; Remaining : Word) of object;
  TModemEvent = procedure(M : TObject; WhatHappened : TModemStatus; Data : LongInt) of object;

  {.Z+}
  PPChar = ^PChar;

  TTagSet = class(TPersistent)
  protected {private}
    FStrings : array[1..ApdMaxTags] of TTagString;                     {!!.03}

    function GetStr(Index : Integer) : TTagString;
    procedure SetStr(Index : Integer; const Tag : TTagString);

  public
    constructor Create;

    property Strings[Index : Integer] : TTagString
      read GetStr write SetStr;

  published
    property String1 : TTagString
      read FStrings[1] write FStrings[1];
    property String2 : TTagString
      read FStrings[2] write FStrings[2];
    property String3 : TTagString
      read FStrings[3] write FStrings[3];
    property String4 : TTagString
      read FStrings[4] write FStrings[4];
    property String5 : TTagString
      read FStrings[5] write FStrings[5];
  end;
  {.Z-}

  TApdCustomModem = class(TApdBaseComponent)
  protected {private}
    {.Z+}
    ModemRec             : PModemRec;           {record passed to API calls}
    FComPort             : TApdCustomComPort;   {port modem is attached to}
    Data                 : TModemInfo;          {data about modem}

    FDialTimeout         : Cardinal;
    FAnswerTimeout       : Cardinal;
    FDelayFactor         : Cardinal;
    FCmdTimeout          : Cardinal;
    FDTRDropHold         : Cardinal;
    FCharDelay           : Cardinal;
    FTildeDelay          : Cardinal;
    FRingWaitTimeout     : Cardinal;
    FAutoStart           : Boolean;
    FStarted             : Boolean;
    FDataCompressionTags : TTagSet;
    FErrorCorrectionTags : TTagSet;
    FModemOk             : TNotifyEvent;
    FModemConnect        : TNotifyEvent;
    FModemBusy           : TNotifyEvent;
    FModemVoice          : TNotifyEvent;
    FModemNoCarrier      : TNotifyEvent;
    FModemNoDialTone     : TNotifyEvent;
    FModemError          : TNotifyEvent;
    FGotLineSpeed        : TLineSpeedEvent;
    FGotErrCorrection    : TNotifyEvent;
    FGotDataCompression  : TNotifyEvent;
    FCmdTimedOut         : TNotifyEvent;
    FDialTimedOut        : TNotifyEvent;
    FAnswerTimedOut      : TNotifyEvent;
    FDialCount           : TConnectCountEvent;
    FAnswerCount         : TConnectCountEvent;
    FOnByeBye            : TNotifyEvent;
    FModemIsConnected    : TNotifyEvent;
    FConnectFailed       : TNotifyEvent;
    FCommandProcessed    : TModemEvent;
    FModemRing           : TNotifyEvent;
    FModemEvent          : TModemEvent;

    procedure SetComPort(const NewPort : TApdCustomComPort);
      {-Change the modem's com port}
    procedure SetModemStrPrim(const NewData : ShortString; var Data : ShortString; RecData : PPChar);
      {-Set a modem's string field}
    procedure SetInitCmd(Cmd : TCmdString);
      {-Set the modem's InitCmd field}
    procedure SetDialCmd(Cmd : TCmdString);
      {-Set the modem's DialCmd field}
    procedure SetDialTerm(Cmd : TCmdString);
      {-Set the modem's DialTerm field}
    procedure SetDialCancel(Cmd : TCmdString);
      {-Set the modem's DialCancel field}
    procedure SetHangupCmd(Cmd : TCmdString);
      {-Set the modem's HangupCmd field}
    procedure SetConfigCmd(Cmd : TConfigString);
      {-Set the modem's ConfigCmd field}
    procedure SetAnswerCmd(Cmd : TCmdString);
      {-Set the modem's AnswerCmd field}
    procedure SetOkMsg(Rsp : TRspString);
      {-Set the modem's OkMsg field}
    procedure SetConnectMsg(Rsp : TRspString);
      {-Set the modem's ConnectMsg field}
    procedure SetBusyMsg(Rsp : TRspString);
      {-Set the modem's BusyMsg field}
    procedure SetVoiceMsg(Rsp : TRspString);
      {-Set the modem's VoiceMsg field}
    procedure SetNoCarrierMsg(Rsp : TRspString);
      {-Set the modem's NoCarrierMsg field}
    procedure SetNoDialToneMsg(Rsp : TRspString);
      {-Set the modem's NoDialToneMsg field}
    procedure SetErrorMsg(Rsp : TRspString);
      {-Set the modem's ErrorMsg field}
    procedure SetRingMsg(Rsp : TRspString);
      {-Set the modem's RingMsg field}
    procedure SetDataCompressionTags(const Tags : TTagSet);
      {-Set modem data compression tags}
    procedure SetErrorCorrectionTags(const Tags : TTagSet);
      {-Set modem error correction tags}
    procedure SetLockDTE(Lock : Boolean);
      {-Set whether the modem should lock the port rate or not}
    procedure SetDialTimeout(Secs : Cardinal);
      {-Set the number of seconds before a dial attempt times out}
    procedure SetAnswerTimeout(Secs : Cardinal);
      {-Set the number of seconds before an answer attempt times out}
    procedure SetDelayFactor(Ticks : Cardinal);
      {-Set the number of ticks to wait between commands sent to the modem}
    procedure SetCmdTimeout(Ticks : Cardinal);
      {-Set the number of ticks to wait for a modem response}
    procedure SetDTRDropHold(Ticks : Cardinal);
      {-Set the number of ticks to hold DTR low during hangup}
    procedure SetCharDelay(Ticks : Cardinal);
      {-Set the number of ticks to wait between each command character sent}
    procedure SetTildeDelay(Ticks : Cardinal);
      {-Set the number of ticks to wait when a '~' is encountered in a command}
    procedure SetRingWaitTimeout(Ticks : Cardinal);
      {-Set the number of ticks to wait before AutoAnswer resets}
    procedure SetStarted(Start : Boolean);
      {-Start or stop the modem}
    procedure SetModemInfo(const Info : TModemInfo);
      {-Set all fields for a modem}
    function GetLastString : String;
      {-Return the last string the modem received}   

    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure CreateModemRecord;
      {-Create the record passed to the API functions}
    procedure DestroyModemRecord;
      {-Destroy the record passed to the API functions}
    procedure AssureStarted;
      {-Make sure the modem has been started}
    procedure ModemPortClose(CP : TObject; Opening : Boolean);
      {-Called when the port the modem is attached to is closed}

  protected
    {event methods}
    procedure ModemOk; virtual;
    procedure ModemConnect; virtual;
    procedure ModemBusy; virtual;
    procedure ModemVoice; virtual;
    procedure ModemNoCarrier; virtual;
    procedure ModemNoDialTone; virtual;
    procedure ModemError; virtual;
    procedure GotLineSpeed(Speed : LongInt); virtual;
    procedure GotErrCorrection; virtual;
    procedure GotDataCompression; virtual;
    procedure CmdTimedOut; virtual;
    procedure DialTimedOut; virtual;
    procedure AnswerTimedOut; virtual;
    procedure DialCount(Remaining : Cardinal); virtual;
    procedure AnswerCount(Remaining : Cardinal); virtual;
    procedure ModemIsConnected; virtual;
    procedure ConnectFailed; virtual;
    procedure ModemRing; virtual;

    procedure CommandProcessed(const WhatHappened : TModemStatus); virtual;
    procedure ModemEvent(const WhatHappened : TModemStatus; const Data : LongInt);

  public
    {creation/destruction}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {.Z-}

    property Started : Boolean
      read FStarted write SetStarted;
    property ModemInfo : TModemInfo
      read Data write SetModemInfo;

    {com port this is linked to}
    property ComPort : TApdCustomComPort
      read FComPort write SetComPort;

    {set this property TRUE to set Start := True automatically}
    property AutoStart : Boolean
      read FAutoStart write FAutoStart default admoDefAutoStart;

    {commands, timeouts, etc.}
    property InitCmd : TCmdString
      read Data.InitCmd write SetInitCmd;
    property DialCmd : TCmdString
      read Data.DialCmd write SetDialCmd;
    property DialTerm : TCmdString
      read Data.DialTerm write SetDialTerm;
    property DialCancel : TCmdString
      read Data.DialCancel write SetDialCancel;
    property HangupCmd : TCmdString
      read Data.HangupCmd write SetHangupCmd;
    property ConfigCmd : TConfigString
      read Data.ConfigCmd write SetConfigCmd;
    property AnswerCmd : TCmdString
      read Data.AnswerCmd write SetAnswerCmd;
    property OkMsg : TRspString
      read Data.OkMsg write SetOkMsg;
    property ConnectMsg : TRspString
      read Data.ConnectMsg write SetConnectMsg;
    property BusyMsg : TRspString
      read Data.BusyMsg write SetBusyMsg;
    property VoiceMsg : TRspString
      read Data.VoiceMsg write SetVoiceMsg;
    property NoCarrierMsg : TRspString
      read Data.NoCarrierMsg write SetNoCarrierMsg;
    property NoDialToneMsg : TRspString
      read Data.NoDialToneMsg write SetNoDialToneMsg;
    property ErrorMsg : TRspString
      read Data.ErrorMsg write SetErrorMsg;
    property RingMsg : TRspString
      read Data.RingMsg write SetRingMsg;
    property DataCompressionTags : TTagSet
      read FDataCompressionTags write SetDataCompressionTags;
    property ErrorCorrectionTags : TTagSet
      read FErrorCorrectionTags write SetErrorCorrectionTags;
    property LockDTE : Boolean
      read Data.LockDTE write SetLockDTE;
    property DialTimeout : Cardinal
      read FDialTimeout write SetDialTimeout default awmDefDialTimeout;
    property AnswerTimeout : Cardinal
      read FAnswerTimeout write SetAnswerTimeout default awmDefAnswerTimeout;
    property DelayFactor : Cardinal
      read FDelayFactor write SetDelayFactor default awmDefDelayFactor;
    property CmdTimeout : Cardinal
      read FCmdTimeout write SetCmdTimeout default awmDefCmdTimeout;
    property DTRDropHold : Cardinal
      read FDTRDropHold write SetDTRDropHold default awmDefDTRDropHold;
    property CharDelay : Cardinal
      read FCharDelay write SetCharDelay default awmDefModemCharDelay;
    property TildeDelay : Cardinal
      read FTildeDelay write SetTildeDelay default awmDefTildeDelay;
    property RingWaitTimeout : Cardinal
      read FRingWaitTimeout write SetRingWaitTimeout default awmDefRingWaitTimeout;
    property LastString : String
      read GetLastString;                        

    {events}
    property OnModemOk : TNotifyEvent
      read FModemOk write FModemOk;
    property OnModemConnect : TNotifyEvent
      read FModemConnect write FModemConnect;
    property OnModemBusy : TNotifyEvent
      read FModemBusy write FModemBusy;
    property OnModemVoice : TNotifyEvent
      read FModemVoice write FModemVoice;
    property OnModemNoCarrier : TNotifyEvent
      read FModemNoCarrier write FModemNoCarrier;
    property OnModemNoDialTone : TNotifyEvent
      read FModemNoDialTone write FModemNoDialTone;
    property OnModemError : TNotifyEvent
      read FModemError write FModemError;
    property OnGotLineSpeed : TLineSpeedEvent
      read FGotLineSpeed write FGotLineSpeed;
    property OnGotErrCorrection : TNotifyEvent
      read FGotErrCorrection write FGotErrCorrection;
    property OnGotDataCompression : TNotifyEvent
      read FGotDataCompression write FGotDataCompression;
    property OnCmdTimedOut : TNotifyEvent
      read FCmdTimedOut write FCmdTimedOut;
    property OnDialTimedOut : TNotifyEvent
      read FDialTimedOut write FDialTimedOut;
    property OnAnswerTimedOut : TNotifyEvent
      read FAnswerTimedOut write FAnswerTimedOut;
    property OnDialCount : TConnectCountEvent
      read FDialCount write FDialCount;
    property OnAnswerCount : TConnectCountEvent
      read FAnswerCount write FAnswerCount;
    property OnByeBye : TNotifyEvent
      read FOnByeBye write FOnByeBye;
    property OnCommandProcessed : TModemEvent
      read FCommandProcessed write FCommandProcessed;
    property OnModemIsConnected : TNotifyEvent
      read FModemIsConnected write FModemIsConnected;
    property OnConnectFailed : TNotifyEvent
      read FConnectFailed write FConnectFailed;
    property OnModemRing : TNotifyEvent
      read FModemRing write FModemRing;
    property OnModemEvent : TModemEvent
      read FModemEvent write FModemEvent;

    procedure PutCommand(const Cmd : String);
      {-Put a command to the modem and handle the response}
    procedure Initialize;
      {-Send the initialization string to the modem}
    procedure Configure;
      {-Send the configuration strings to the modem}
    procedure Dial(const Number : String);
      {-Dial the modem}
    function IsAttemptingConnect : Boolean;
      {-Return TRUE if the modem is attempting to establish a connection}
    procedure ExtendConnectAttempt(const DeltaSecs : Integer);
      {-Extend the amount of time the modem waits for a CONNECT result}
    procedure CancelDialAnswer;
      {-Cancel the dial/answer in progress}
    function GetConnectSpeed : LongInt;
      {-Get the actual speed of the connection}
    procedure Hangup;
      {-Hangup the modem}
    procedure Answer;
      {-Answer the modem}
    procedure AutoAnswer(const Rings : Cardinal);
      {-Answer the modem after Rings rings}
    function FeatureWaitOver : Boolean;
      {-Return TRUE if all modem features have been received and processed}
    procedure WaitOnFeatures;
      {-Wait until all modem features have been received}
    procedure WaitOnResponse;
      {-Wait until the modem finishes processing the last command}
  end;

  TApdModem = class(TApdCustomModem)
  published
    {com port this is linked to}
    property ComPort;

    {commands, timeouts, etc.}
    property InitCmd;
    property DialCmd;
    property DialTerm;
    property DialCancel;
    property HangupCmd;
    property ConfigCmd;
    property AnswerCmd;
    property OkMsg;
    property ConnectMsg;
    property BusyMsg;
    property VoiceMsg;
    property NoCarrierMsg;
    property NoDialToneMsg;
    property ErrorMsg;
    property RingMsg;
    property DataCompressionTags;
    property ErrorCorrectionTags;
    property LockDTE;
    property DialTimeout;
    property AnswerTimeout;
    property DelayFactor;
    property CmdTimeout;
    property DTRDropHold;
    property CharDelay;
    property TildeDelay;
    property RingWaitTimeout;

    {events}
    property OnModemOk;
    property OnModemConnect;
    property OnModemBusy;
    property OnModemVoice;
    property OnModemNoCarrier;
    property OnModemNoDialTone;
    property OnModemError;
    property OnGotLineSpeed;
    property OnGotErrCorrection;
    property OnGotDataCompression;
    property OnCmdTimedOut;
    property OnDialTimedOut;
    property OnAnswerTimedOut;
    property OnDialCount;
    property OnAnswerCount;
    property OnCommandProcessed;
    property OnModemIsConnected;
    property OnConnectFailed;
    property OnModemRing;
    property OnModemEvent;
  end;

const
  DefModemData : TModemInfo =
    ( Name          : '';
      InitCmd       : 'ATZ^M';
      DialCmd       : 'ATDT';
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
      NoDialToneMsg : 'NO DIALTONE';
      ErrorMsg      : 'ERROR';
      RingMsg       : 'RING';
      NumErrors     : 0;
      Errors        : ('', '', '', '', '');
      NumComps      : 0;
      Compression   : ('', '', '', '', '');
      LockDTE       : True;
      DefBaud       : 19200
    );

implementation



{TTagSet}

  function TTagSet.GetStr(Index : Integer) : TTagString;
  begin
    Result := FStrings[Index];
  end;

  procedure TTagSet.SetStr(Index : Integer; const Tag : TTagString);
  begin
    FStrings[Index] := Tag;
  end;

  constructor TTagSet.Create;
  begin
    inherited Create;

    FillChar(FStrings, SizeOf(FStrings), 0);
  end;

{TApdCustomModem}

  procedure ModemHandler(Msg, wParam : Cardinal; lParam : LongInt); far;
  var
    Modem : TApdCustomModem;

  begin
    Modem := TApdCustomModem(PModemRec(lParam)^.UserData);
    with Modem do
      {take OnModemEvent into account}
      case Msg of
        apw_ModemOk           :
          begin
            ModemOK;
            ModemEvent(msModemOK, 0);
          end;
        apw_ModemConnect      :
          begin
            ModemConnect;
            ModemEvent(msModemConnect, 0);
          end;
        apw_ModemBusy         :
          begin
            ModemBusy;
            ModemEvent(msModemBusy, 0);
          end;
        apw_ModemVoice        :
          begin
            ModemVoice;
            ModemEvent(msModemVoice, 0);
          end;
        apw_ModemNoCarrier    :
          begin
            ModemNoCarrier;
            ModemEvent(msModemNoCarrier, 0);
          end;
        apw_ModemNoDialTone   :
          begin
            ModemNoDialTone;
            ModemEvent(msModemNoDialTone, 0);
          end;
        apw_ModemError        :
          begin
            ModemError;
            ModemEvent(msModemError, 0);
          end;
        apw_GotLineSpeed      :
          begin
            GotLineSpeed(mGetConnectSpeed(PModemRec(lParam)));
            ModemEvent(msGotLineSpeed, mGetConnectSpeed(PModemRec(lParam)));
          end;
        apw_GotErrCorrection  :
          begin
            GotErrCorrection;
            ModemEvent(msGotErrCorrection, 0);
          end;
        apw_GotDataCompression:
          begin
            GotDataCompression;
            ModemEvent(msGotDataCompression, 0);
          end;
        apw_CmdTimeout        :
          begin
            CmdTimedOut;
            ModemEvent(msCmdTimeOut, 0);
          end;
        apw_DialTimeout       :
          begin
            DialTimedOut;
            ModemEvent(msDialTimeOut, 0);
          end;
        apw_AnswerTimeout     :
          begin
            AnswerTimedOut;
            ModemEvent(msAnswerTimeOut, 0);
          end;
        apw_DialCount         :
          begin
            DialCount(wParam);
            ModemEvent(msDialCount, wParam);
          end;
        apw_AnswerCount       :
          begin
            AnswerCount(wParam);
            ModemEvent(msAnswerCount, wParam);
          end;
        apw_ModemIsConnected  :
          begin
            ModemIsConnected;
            ModemEvent(msModemIsConnected, 0);
          end;
        apw_ConnectFailed     :
          begin
            ConnectFailed;
            ModemEvent(msConnectFailed, 0);
          end;
        apw_ModemRing         :
          begin
            ModemRing;
            ModemEvent(msModemRing, 0);
          end;
        apw_CommandProcessed  :
          case wParam of
            cpTimeout:
              begin
                CommandProcessed(msCmdTimeOut);
                ModemEvent(msCommandProcessed, Ord(msCmdTimeOut));
              end;
            cpOK     :
              begin
                CommandProcessed(msModemOK);
                ModemEvent(msCommandProcessed, Ord(msModemOK));
              end;

            cpError  :
              begin
                CommandProcessed(msModemError);
                ModemEvent(msCommandProcessed, Ord(msModemError));
              end;                                                     
          end;
      end;
  end;

  procedure TApdCustomModem.SetComPort(const NewPort : TApdCustomComPort);
    {-Change the modem's com port}
  var
    WasStarted : Boolean;

  begin
    if (NewPort = FComPort) then
      Exit;

    WasStarted := Started;
    Started    := False;
    FComPort := NewPort;
    Started  := WasStarted;
  end;

  procedure TApdCustomModem.SetModemStrPrim(const NewData : ShortString; var Data : ShortString; RecData : PPChar);
    {-Set a modem's string field}
  var
    NewStr : PChar;

  begin
    if Assigned(ModemRec) then begin
      if (ModemRec^.ModemState <> msNone) then
        raise EModemBusy.Create(ecModemBusy, False);

      NewStr := AllocMem(Length(NewData) + 1);
      StrPCopy(NewStr, NewData);

      FreeMem(RecData^, StrLen(RecData^) + 1);
      RecData^ := NewStr;
    end;
    Data := NewData;
  end;

  procedure TApdCustomModem.SetInitCmd(Cmd : TCmdString);
    {-Set the modem's InitCmd field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Cmd, Data.InitCmd, @ModemRec^.InitCmd)
    else
      SetModemStrPrim(Cmd, Data.InitCmd, nil);
  end;

  procedure TApdCustomModem.SetDialCmd(Cmd : TCmdString);
    {-Set the modem's DialCmd field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Cmd, Data.DialCmd, @ModemRec^.DialCmd)
    else
      SetModemStrPrim(Cmd, Data.DialCmd, nil);
  end;

  procedure TApdCustomModem.SetDialTerm(Cmd : TCmdString);
    {-Set the modem's DialTerm field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Cmd, Data.DialTerm, @ModemRec^.DialTerm)
    else
      SetModemStrPrim(Cmd, Data.DialTerm, nil);
  end;

  procedure TApdCustomModem.SetDialCancel(Cmd : TCmdString);
    {-Set the modem's DialCancel field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Cmd, Data.DialCancel, @ModemRec^.DialCancel)
    else
      SetModemStrPrim(Cmd, Data.DialCancel, nil);
  end;

  procedure TApdCustomModem.SetHangupCmd(Cmd : TCmdString);
    {-Set the modem's HangupCmd field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Cmd, Data.HangupCmd, @ModemRec^.HangupCmd)
    else
      SetModemStrPrim(Cmd, Data.HangupCmd, nil);
  end;

  procedure TApdCustomModem.SetConfigCmd(Cmd : TConfigString);
    {-Set the modem's ConfigCmd field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Cmd, Data.ConfigCmd, @ModemRec^.ConfigCmd)
    else
      SetModemStrPrim(Cmd, Data.ConfigCmd, nil);
  end;

  procedure TApdCustomModem.SetAnswerCmd(Cmd : TCmdString);
    {-Set the modem's AnswerCmd field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Cmd, Data.AnswerCmd, @ModemRec^.AnswerCmd)
    else
      SetModemStrPrim(Cmd, Data.AnswerCmd, nil);
  end;

  procedure TApdCustomModem.SetOkMsg(Rsp : TRspString);
    {-Set the modem's OkMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.OkMsg, @ModemRec^.Responses[RspOK].Response)
    else
      SetModemStrPrim(Rsp, Data.OkMsg, nil);
  end;

  procedure TApdCustomModem.SetConnectMsg(Rsp : TRspString);
    {-Set the modem's ConnectMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.ConnectMsg, @ModemRec^.Responses[RspConnect].Response)
    else
      SetModemStrPrim(Rsp, Data.ConnectMsg, nil);
  end;

  procedure TApdCustomModem.SetBusyMsg(Rsp : TRspString);
    {-Set the modem's BusyMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.BusyMsg, @ModemRec^.Responses[RspBusy].Response)
    else
      SetModemStrPrim(Rsp, Data.BusyMsg, nil);
  end;

  procedure TApdCustomModem.SetVoiceMsg(Rsp : TRspString);
    {-Set the modem's VoiceMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.VoiceMsg, @ModemRec^.Responses[RspVoice].Response)
    else
      SetModemStrPrim(Rsp, Data.VoiceMsg, nil);
  end;

  procedure TApdCustomModem.SetNoCarrierMsg(Rsp : TRspString);
    {-Set the modem's NoCarrierMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.NoCarrierMsg, @ModemRec^.Responses[RspNoCarrier].Response)
    else
      SetModemStrPrim(Rsp, Data.NoCarrierMsg, nil);
  end;

  procedure TApdCustomModem.SetNoDialToneMsg(Rsp : TRspString);
    {-Set the modem's NoDialToneMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.NoDialToneMsg, @ModemRec^.Responses[RspNoDialTone].Response)
    else
      SetModemStrPrim(Rsp, Data.NoDialToneMsg, nil);
  end;

  procedure TApdCustomModem.SetErrorMsg(Rsp : TRspString);
    {-Set the modem's ErrorMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.ErrorMsg, @ModemRec^.Responses[RspError].Response)
    else
      SetModemStrPrim(Rsp, Data.ErrorMsg, nil);
  end;

  procedure TApdCustomModem.SetRingMsg(Rsp : TRspString);
    {-Set the modem's RingMsg field}
  begin
    if Assigned(ModemRec) then
      SetModemStrPrim(Rsp, Data.RingMsg, @ModemRec^.Responses[RspRing].Response)
    else
      SetModemStrPrim(Rsp, Data.RingMsg, nil);
  end;

  procedure TApdCustomModem.SetDataCompressionTags(const Tags : TTagSet);
    {-Set modem data compression tags}
  var
    I : Cardinal;

  begin
    if Assigned(ModemRec) and (ModemRec^.ModemState <> msNone) then
      raise EModemBusy.Create(ecModemBusy, False);
    for I := 1 to ApdMaxTags do                                        {!!.03}
      FDataCompressionTags.Strings[I] := Tags.Strings[I];
  end;

  procedure TApdCustomModem.SetErrorCorrectionTags(const Tags : TTagSet);
    {-Set modem error correction tags}
  var
    I : Cardinal;

  begin
    if Assigned(ModemRec) and (ModemRec^.ModemState <> msNone) then
      raise EModemBusy.Create(ecModemBusy, False);
    for I := 1 to ApdMaxTags do                                        {!!.03}
      FErrorCorrectionTags.Strings[I] := Tags.Strings[I];
  end;

  procedure TApdCustomModem.SetStarted(Start : Boolean);
    {-Start or stop the modem}
  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) or (Start = Started) then
      Exit;

    if not Assigned(FComPort) then
      raise EPortNotAssigned.Create(ecPortNotAssigned, False);

    if (FComPort.DeviceLayer = dlWinSock) then
      raise ECannotUseWithWinSock.Create(ecCannotUseWithWinSock, False); 

    if (Start = False) then begin
      DestroyModemRecord;
      FComPort.DeregisterUserCallback(ModemPortClose);
    end else begin
      if Assigned(ModemRec) then
        DestroyModemRecord;
      CreateModemRecord;

      {register the global modem handler}
      try
        CheckException(Self, mRegisterModemHandler(ModemRec, 0, ModemHandler));
      except
        DestroyModemRecord;
        raise;
      end;

      {turn the modem on}
      try
        CheckException(Self, mStartModem(ModemRec)); 
      except
        DestroyModemRecord;
        raise;
      end;

      FComPort.RegisterUserCallback(ModemPortClose);
    end;
    FStarted := Start;
  end;

  procedure TApdCustomModem.SetModemInfo(const Info : TModemInfo);
    {-Set all fields for a modem}
  begin
    SetInitCmd(Info.InitCmd);
    SetDialCmd(Info.DialCmd);
    SetDialTerm(Info.DialTerm);
    SetDialCancel(Info.DialCancel);
    SetHangupCmd(Info.HangupCmd);
    SetConfigCmd(Info.ConfigCmd);
    SetAnswerCmd(Info.AnswerCmd);
    SetOkMsg(Info.OkMsg);
    SetConnectMsg(Info.ConnectMsg);
    SetBusyMsg(Info.BusyMsg);
    SetVoiceMsg(Info.VoiceMsg);
    SetNoCarrierMsg(Info.NoCarrierMsg);
    SetNoDialToneMsg(Info.NoDialToneMsg);
    SetErrorMsg(Info.ErrorMsg);
    SetRingMsg(Info.RingMsg);
    SetLockDTE(Info.LockDTE);
    Data.DefBaud := Info.DefBaud;
    Data.Compression := Info.Compression;
    Data.NumComps := Info.NumComps;
    Data.Errors := Info.Errors;
    Data.NumErrors := Info.NumErrors;
    Data.Name := Info.Name;                                           
  end;

  function TApdCustomModem.GetLastString : String;
    {-Return the last string the modem received}
  begin
    AssureStarted;
    Result := StrPas(ModemRec^.LastString);
  end;

  procedure TApdCustomModem.SetLockDTE(Lock : Boolean);
    {-Set whether the modem should lock the port rate or not}
  begin
    if Assigned(ModemRec) then
      ModemRec^.LockDTE := Lock;
    Data.LockDTE := Lock;
  end;

  procedure TApdCustomModem.SetDialTimeout(Secs : Cardinal);
    {-Set the number of seconds before a dial attempt times out}
  begin
    if Assigned(ModemRec) then
      mSetModemDialTimeout(ModemRec, Secs);
    FDialTimeout := Secs;
  end;

  procedure TApdCustomModem.SetAnswerTimeout(Secs : Cardinal);
    {-Set the number of seconds before an answer attempt times out}
  begin
    if Assigned(ModemRec) then
      mSetModemAnswerTimeout(ModemRec, Secs);
    FAnswerTimeout := Secs;
  end;

  procedure TApdCustomModem.SetDelayFactor(Ticks : Cardinal);
    {-Set the number of ticks to wait between commands sent to the modem}
  begin
    if Assigned(ModemRec) then
      mSetModemDelayFactor(ModemRec, Ticks);
    FDelayFactor := Ticks;
  end;

  procedure TApdCustomModem.SetCmdTimeout(Ticks : Cardinal);
    {-Set the number of ticks to wait for a modem response}
  begin
    if Assigned(ModemRec) then
      mSetModemCmdTimeout(ModemRec, Ticks);
    FCmdTimeout := Ticks;
  end;

  procedure TApdCustomModem.SetDTRDropHold(Ticks : Cardinal);
    {-Set the number of ticks to hold DTR low during hangup}
  begin
    if Assigned(ModemRec) then
      mSetModemDTRDropHold(ModemRec, Ticks);
    FDTRDropHold := Ticks;
  end;

  procedure TApdCustomModem.SetCharDelay(Ticks : Cardinal);
    {-Set the number of ticks to wait between each command character sent}
  begin
    if Assigned(ModemRec) then
      mSetModemCharDelay(ModemRec, Ticks);
    FCharDelay := Ticks;
  end;

  procedure TApdCustomModem.SetTildeDelay(Ticks : Cardinal);
    {-Set the number of ticks to wait when a '~' is encountered in a command}
  begin
    if Assigned(ModemRec) then
      mSetTildeDelay(ModemRec, Ticks);
    FTildeDelay := Ticks;
  end;

  procedure TApdCustomModem.SetRingWaitTimeout(Ticks : Cardinal);
    {-Set the number of ticks to wait before AutoAnswer resets}
  begin
    if Assigned(ModemRec) then
      mSetRingWaitTimeout(ModemRec, Ticks);
    FRingWaitTimeout := Ticks;
  end;

  procedure TApdCustomModem.Notification(AComponent : TComponent; Operation : TOperation);
  begin
    inherited Notification(AComponent, Operation);

    if (Operation = opRemove) then begin
      {see if our com port is going away}
      if (AComponent = FComPort) then
        FComPort := nil;
    end else if (Operation = opInsert) then begin
      {check for a com port being installed}
      if not Assigned(FComPort) and (AComponent is TApdCustomComPort) then
        ComPort := TApdCustomComPort(AComponent);
    end;
  end;

  procedure TApdCustomModem.CreateModemRecord;
    {-Create the record passed to the API functions}
  var
    I         : Cardinal;
    OnTag     : Cardinal;
    PCharData : TModemData;

  begin
    if Assigned(ModemRec) then
      DestroyModemRecord;
    Data.DefBaud := ComPort.Baud;

    {copy error correction feature tags into info structure}
    FillChar(Data.Errors, SizeOf(TTagArray), 0);
    OnTag := 0;
    for I := 1 to ApdMaxTags do                                        {!!.03}
      if (ErrorCorrectionTags.Strings[I] <> '') then begin
        Inc(OnTag);
        Data.Errors[OnTag] := ErrorCorrectionTags.Strings[I];
      end;
    Data.NumErrors := OnTag;

    {copy data compression feature tags into info structure}
    FillChar(Data.Compression, SizeOf(TTagArray), 0);
    OnTag := 0;
    for I := 1 to ApdMaxTags do                                        {!!.03}
      if (DataCompressionTags.Strings[I] <> '') then begin
        Inc(OnTag);
        Data.Compression[OnTag] := DataCompressionTags.Strings[I];
      end;
    Data.NumComps := OnTag;

    StrsToPChars(Data, PCharData);

    {create modem record}
    CheckException(Self, mInitModemDelphi(ModemRec, FComPort.Dispatcher, PCharData));
    ModemRec^.UserData := LongInt(Self);

    {set modem options}
    DialTimeout     := FDialTimeout;
    AnswerTimeout   := FAnswerTimeout;
    DelayFactor     := FDelayFactor;
    CmdTimeout      := FCmdTimeout;
    DTRDropHold     := FDTRDropHold;
    CharDelay       := FCharDelay;
    TildeDelay      := FTildeDelay;
    RingWaitTimeout := FRingWaitTimeout;
  end;

  procedure TApdCustomModem.DestroyModemRecord;
    {-Destroy the record passed to the API functions}
  begin
    if Assigned(ModemRec) then begin
      mDoneModem(ModemRec);
      ModemRec := nil;
    end;
  end;

  procedure TApdCustomModem.AssureStarted;
    {-Make sure the modem has been started}
  begin
    if not Assigned(FComPort) then
      raise EPortNotAssigned.Create(ecPortNotAssigned, False);
    if FComPort.Dispatcher = nil then
      raise ECommNotOpen.Create(ecCommNotOpen, False);
    if not AutoStart then begin
      if not Started then
        raise EModemNotStarted.Create(ecModemNotStarted, False);
    end else
      Started := True;
  end;

  procedure TApdCustomModem.ModemPortClose(CP : TObject; Opening : Boolean);
    {-Called when the port the modem is attached to is closed}
  begin
    if not Opening then
      Started := False;
  end;

  procedure TApdCustomModem.ModemOk;
  begin
    if Assigned(FModemOk) then
      FModemOk(Self);
  end;

  procedure TApdCustomModem.ModemConnect;
  begin
    if Assigned(FModemConnect) then
      FModemConnect(Self);
  end;

  procedure TApdCustomModem.ModemBusy;
  begin
    if Assigned(FModemBusy) then
      FModemBusy(Self);
  end;

  procedure TApdCustomModem.ModemVoice;
  begin
    if Assigned(FModemVoice) then
      FModemVoice(Self);
  end;

  procedure TApdCustomModem.ModemNoCarrier;
  begin
    if Assigned(FModemNoCarrier) then
      FModemNoCarrier(Self);
  end;

  procedure TApdCustomModem.ModemNoDialTone;
  begin
    if Assigned(FModemNoDialTone) then
      FModemNoDialTone(Self);
  end;

  procedure TApdCustomModem.ModemError;
  begin
    if Assigned(FModemError) then
      FModemError(Self);
  end;

  procedure TApdCustomModem.GotLineSpeed(Speed : LongInt);
  begin
    if Assigned(FGotLineSpeed) then
      FGotLineSpeed(Self, Speed);
  end;

  procedure TApdCustomModem.GotErrCorrection;
  begin
    if Assigned(FGotErrCorrection) then
      FGotErrCorrection(Self);
  end;

  procedure TApdCustomModem.GotDataCompression;
  begin
    if Assigned(FGotDataCompression) then
      FGotDataCompression(Self);
  end;

  procedure TApdCustomModem.CmdTimedOut;
  begin
    if Assigned(FCmdTimedOut) then
      FCmdTimedOut(Self);
  end;

  procedure TApdCustomModem.DialTimedOut;
  begin
    if Assigned(FDialTimedOut) then
      FDialTimedOut(Self);
  end;

  procedure TApdCustomModem.AnswerTimedOut;
  begin
    if Assigned(FAnswerTimedOut) then
      FAnswerTimedOut(Self);
  end;

  procedure TApdCustomModem.DialCount(Remaining : Cardinal);
  begin
    if Assigned(FDialCount) then
      FDialCount(Self, Remaining);
  end;

  procedure TApdCustomModem.AnswerCount(Remaining : Cardinal);
  begin
    if Assigned(FAnswerCount) then
      FAnswerCount(Self, Remaining);
  end;

  procedure TApdCustomModem.ModemIsConnected;
  begin
    if Assigned(FModemIsConnected) then
      FModemIsConnected(Self);
  end;

  procedure TApdCustomModem.ConnectFailed;
  begin
    if Assigned(FConnectFailed) then
      FConnectFailed(Self);
  end;

  procedure TApdCustomModem.ModemRing;
  begin
    if Assigned(FModemRing) then
      FModemRing(Self);
  end;

  procedure TApdCustomModem.CommandProcessed(const WhatHappened : TModemStatus);
  begin
    if Assigned(FCommandProcessed) then
      FCommandProcessed(Self, WhatHappened, 0);
  end;

  procedure TApdCustomModem.ModemEvent(const WhatHappened : TModemStatus; const Data : LongInt);
  begin
    if Assigned(FModemEvent) then
      FModemEvent(Self, WhatHappened, Data);
  end;

  constructor TApdCustomModem.Create(AOwner : TComponent);
  var
    I : Cardinal;

  begin
    inherited Create(AOwner);

    ModemRec             := nil;
    FComPort             := nil;
    Data                 := DefModemData;
    FDataCompressionTags := TTagSet.Create;
    FErrorCorrectionTags := TTagSet.Create;

    {search our owner for a com port}
    if Assigned(AOwner) and (AOwner.ComponentCount > 0) then
      for I := 0 to Pred(AOwner.ComponentCount) do
        if AOwner.Components[I] is TApdCustomComPort then begin
          FComPort := TApdCustomComPort(AOwner.Components[I]);
          Break;
        end;

    {set properties to defaults}
    FDialTimeout        := awmDefDialTimeout;
    FAnswerTimeout      := awmDefAnswerTimeout;
    FDelayFactor        := awmDefDelayFactor;
    FCmdTimeout         := awmDefCmdTimeout;
    FDTRDropHold        := awmDefDTRDropHold;
    FCharDelay          := awmDefModemCharDelay;
    FTildeDelay         := awmDefTildeDelay;
    FRingWaitTimeout    := awmDefRingWaitTimeout;
    FAutoStart          := admoDefAutoStart;
    FStarted            := False;

    {set event properties to default}
    FModemOk            := nil;
    FModemConnect       := nil;
    FModemBusy          := nil;
    FModemVoice         := nil;
    FModemNoCarrier     := nil;
    FModemNoDialTone    := nil;
    FModemError         := nil;
    FGotLineSpeed       := nil;
    FGotErrCorrection   := nil;
    FGotDataCompression := nil;
    FCmdTimedOut        := nil;
    FDialTimedOut       := nil;
    FAnswerTimedOut     := nil;
    FDialCount          := nil;
    FAnswerCount        := nil;
    FOnByeBye           := nil;
    FCommandProcessed   := nil;
    FModemIsConnected   := nil;
    FConnectFailed      := nil;
    FModemRing          := nil;
    FModemEvent         := nil;
  end;

  destructor TApdCustomModem.Destroy;
  begin
    if Assigned(FOnByeBye) then
      FOnByeBye(Self);

    FDataCompressionTags.Free;
    FErrorCorrectionTags.Free;

    Started := False;

    inherited Destroy;
  end;

  procedure TApdCustomModem.PutCommand(const Cmd : String);
    {-Put a command to the modem and handle the response}
  var
    TempStr : array[0..255] of Char;

  begin
    AssureStarted;
    CheckException(Self, mPutModemCommand(ModemRec, StrPCopy(TempStr, Cmd)));
  end;

  procedure TApdCustomModem.Initialize;
    {-Send the initialization string to the modem}
  begin
    AssureStarted;
    CheckException(Self, mInitializeModem(ModemRec));
  end;

  procedure TApdCustomModem.Configure;
    {-Send the configuration strings to the modem}
  begin
    AssureStarted;
    CheckException(Self, mConfigureModem(ModemRec));
  end;

  procedure TApdCustomModem.Dial(const Number : String);
    {-Dial the modem}
  var
    Temp : array[0..255] of Char;

  begin
    AssureStarted;
    CheckException(Self, mDialModem(ModemRec, StrPCopy(Temp, Number)));
  end;

  function TApdCustomModem.IsAttemptingConnect : Boolean;
    {-Return TRUE if the modem is attempting to establish a connection}
  begin
    AssureStarted;
    IsAttemptingConnect := mIsAttemptingConnect(ModemRec);
  end;

  procedure TApdCustomModem.ExtendConnectAttempt(const DeltaSecs : Integer);
    {-Extend the amount of time the modem waits for a CONNECT result}
  begin
    AssureStarted;
    CheckException(Self, mExtendConnectAttempt(ModemRec, DeltaSecs));
  end;

  procedure TApdCustomModem.CancelDialAnswer;
    {-Cancel the dial/answer in progress}
  begin
    AssureStarted;
    CheckException(Self, mCancelDialAnswer(ModemRec));
  end;

  function TApdCustomModem.GetConnectSpeed : LongInt;
    {-Get the actual speed of the connection}
  begin
    AssureStarted;
    GetConnectSpeed := mGetConnectSpeed(ModemRec);
  end;

  procedure TApdCustomModem.Hangup;
    {-Hangup the modem}
  begin
    AssureStarted;
    CheckException(Self, mHangupModem(ModemRec));
  end;

  procedure TApdCustomModem.Answer;
    {-Answer the modem}
  begin
    AssureStarted;
    CheckException(self, mAnswerModem(ModemRec));
  end;

  procedure TApdCustomModem.AutoAnswer(const Rings : Cardinal);
    {-Answer the modem after Rings rings}
  begin
    AssureStarted;
    CheckException(Self, mAutoAnswerModem(ModemRec, Rings));
  end;

  function TApdCustomModem.FeatureWaitOver : Boolean;
    {-Return TRUE if all modem features have been received and processed}
  begin
    AssureStarted;
    FeatureWaitOver := mAllFeatureWaitOver(ModemRec);
  end;

  procedure TApdCustomModem.WaitOnFeatures;
    {-Wait until all modem features have been received}
  begin
    AssureStarted;
    CheckException(Self, mWaitOnFeatures(ModemRec));
  end;

  procedure TApdCustomModem.WaitOnResponse;
    {-Wait until the modem finishes processing the last command}
  begin
    AssureStarted;
    CheckException(Self, mWaitOnResponse(ModemRec));
  end;

end.
