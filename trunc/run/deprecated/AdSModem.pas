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
{*                   ADSMODEM.PAS 5.01                   *}
{*********************************************************}
{* Deprecated TApdSModem component, simple modem control *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I ..\..\includes\AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-,I+}
{$IFDEF WIN32}
{$H+}
{$ENDIF}

unit AdSModem;
  {-Async Professional for Delphi -- Simplified Modem Component}

interface

uses
  {-----RTL}
  Windows,
  SysUtils,
  Classes,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  {$IFDEF Win32}
  Registry,
  {$IFNDEF UseAwWin32}
  LnsWin32,
  {$ELSE}
  AwWin32,
  {$ENDIF}
  {$ELSE}
  AwComm,
  {$ENDIF}
  IniFiles,
  OoMisc,
  AwUser,
  AdSelCom,
  AdExcept,
  AdPort,
  AdSMdm1;

const
  { Constants for status messages in APW.STR }
  smMsgBase                   = 4200;

  smMsgReady                  = 4201;
  smMsgInitialize             = 4202;
  smMsgInitializeTimeout      = 4203;
  smMsgAutoAnswerBackground   = 4204;
  smMsgAutoAnswerWait         = 4205;
  smMsgAnswerWait             = 4206;
  smMsgDialWait               = 4207;
  smMsgDialCycle              = 4208;
  smMsgNoDialtone             = 4209;                            
  smMsgConnectWait            = 4210;
  smMsgConnected              = 4211;
  smMsgHangup                 = 4212;
  smMsgCancel                 = 4213;

  adsmDefAnswerCmd          = 'ATA^M';
  adsmDefDialCmd            = 'ATDT';
  adsmDefDialCancelCmd      = '^M';
  adsmDefDialTerminatorCmd  = '^M';
  adsmDefHangupCmd          = 'DTR';
  adsmDefInitializeCmd      = 'ATE1Q0X4V1^M';

  adsmDefBusyMsg            = 'BUSY';
  adsmDefConnectMsg         = 'CONNECT';
  adsmDefDataCompressionMsg = '';
  adsmDefErrorCorrectionMsg = '';
  adsmDefErrorMsg           = 'ERROR';
  adsmDefNoCarrierMsg       = 'NO CARRIER';
  adsmDefNoDialToneMsg      = 'NO DIALTONE';                     
  adsmDefOkMsg              = 'OK';
  adsmDefRingMsg            = 'RING';

  adsmDefAnswerTimeout      = 60;     {seconds}
  adsmDefCmdTimeout         = 50;     {ticks}
  adsmDefConnectInfoTimeout = 9;      {ticks}
  adsmDefDialTimeout        = 60;     {seconds}
  adsmDefRingWaitTimeout    = 182;    {ticks}

  adsmDefDTRDropHoldDelay   = 8;      {ticks}
  adsmDefInterCharDelay     = 0;      {ticks}
  adsmDefInterCmdDelay      = 2;      {ticks}
  adsmDefTildeDelay         = 9;      {ticks}

  adsmDefModemIniName       = 'AWMODEM.INI';
  adsmDefaultModemTag       = '<default modem>';

  adsmDefExtendTime         = 60;     {seconds}
  adsmDefRetryWaitTime      = 60;     {seconds}
  adsmDefUpdateStatusTime   = 1;      {seconds}

  adsmDefAutoRetryDial      = True;
  adsmDefForceHangup        = False;
  adsmDefMaxDialAttempts    = 10;
  adsmDefShowStatus         = True;

const
  {Special flags used outside of the state machine or for status info}
  smsWaitForInProgress  = $0001;
  smsResponseReceived   = $0002;
  smsDataTransmitted    = $0004;
  smsBatchInProgress    = $0008;

  {Maximum size of connect message buffer}
  MaxBufferSize = 4096;
type
  {Simple modem machine states}
  TApdSModemStates = (
    smsUnknown,                     {hasn't been or couldn't be initialized}
    smsReady,                       {idle and ready}

    smsInitialize,                  {starting initialize process}
    smsInitializeTimeout,           {timeout waiting for initialize response}

    smsAutoAnswerBackground,        {autoanswer mode -- no rings received}
    smsAutoAnswerWait,              {autoanswer mode -- waiting for Nth ring}
    smsAnswerWait,                  {answering call -- waiting for connect}
    smsDialWait,                    {dialing call -- waiting for connect}
    smsDialCycle,                   {time to retry a dial attempt (cycletime=0)}
    smsNoDialTone,                  {no dialtone detected}        
    smsConnectWait,                 {connect in progress -- waiting for optional data}
    smsConnected,                   {done with connect process}

    smsHangup,                      {starting hangup process}

    smsCancel,                      {starting cancel process}
    smsAbort                        {cancel current operation}
  );

  {for internal use only -- subject to change}
  TApdSModemStateData = record
    TimeRemaining : Cardinal;
    String1       : String;
    String2       : String;
    Cardinal1     : Cardinal;
    Cardinal2     : Cardinal;
    Byte1         : Byte;
    Byte2         : Byte;
  end;

  TApdSModemStatusInfo = class(TApdBaseComponent)
  protected {private}
    msiModemState       : TApdSModemStates;
    msiModemStateData   : TApdSModemStateData;
    msiSendRecvData     : TStringList;
    msiSendRecvDataCopy : TStringList;

    {These procedures are used by the modem component to set the status info}
    procedure AddSendRecvData(Msg : String);
      {Add the message to the Send/Receive string list}
    procedure SetInfoAnswerWait(TimeRemaining : Cardinal);
      {Set the specific parameters describing the Answer progress}
    procedure SetInfoAutoAnswerWait(RingToAnswer : Byte;
                                    RingsReceived : Byte);
      {Set the specific parameters describing the AutoAnswer progress}
    procedure SetInfoConnected(ErrorCorrection : String;
                               DataCompression : String;
                               ConnectSpeed : Cardinal);
      {Set the specific parameters describing the Connection progress}
    procedure SetInfoDialCycle(NextPhoneNumber : String;
                               NextAttemptNumber : Cardinal;
                               MaxAttempts : Cardinal;
                               TimeRemaining : Cardinal);
      {Set the specific parameters describing the Dial Retry/Cycle progress}
    procedure SetInfoDialWait(PhoneNumber : String;
                              AttemptNumber : Cardinal;
                              MaxAttempts : Cardinal;
                              TimeRemaining : Cardinal);
      {Set the specific parameters describing the Dial progress}

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    function  GetModemState : TApdSModemStates;
      {Get the current state of the modem component (e.g., smsDialWait)}
    function  GetModemStateMsg : String;
      {Get the description of the current modem state (e.g., 'Dialing')}
    function  GetSendRecvMessages : TStringList;
      {Get a list of the available send/receive messages}
    procedure GetInfoAnswerWait(var TimeRemaining : Cardinal);
      {Get the specific status info when GetModemState = smsAnswerWait}
    procedure GetInfoAutoAnswerWait(var RingToAnswer : Byte;
                                    var RingsReceived : Byte);
      {Get the specific status info when GetModemState = smsAutoAnswerWait}
    procedure GetInfoConnected(var ErrorCorrection : String;
                               var DataCompression : String;
                               var ConnectSpeed : Cardinal);
      {Get the specific status info when GetModemState = smsConnected}
    procedure GetInfoDialCycle(var NextPhoneNumber : String;
                               var NextAttemptNumber : Cardinal;
                               var MaxAttempts : Cardinal;
                               var TimeRemaining : Cardinal);
      {Get the specific status info when GetModemState = smsDialCycle}
    procedure GetInfoDialWait(var PhoneNumber : String;
                              var AttemptNumber : Cardinal;
                              var MaxAttempts : Cardinal;
                              var TimeRemaining : Cardinal);
      {Get the specific status info when GetModemState = smsDialWait}
  end;

  TApdSModemEvent = procedure(ModemInstance : TObject;
                              ModemStatus   : TApdSModemStatusInfo) of object;

  TApdSModemStatusDisplay = class;

  TApdCustomSModem = class(TApdBaseComponent)
  protected {private}
    msModemStatusTrigger  : Cardinal;
    msUpdateStatusTrigger : Cardinal;
    msTimeoutTrigger      : Cardinal;
    msForceUpdateStatus   : Boolean;
    msCRLFIndex           : Byte;
    msModemResponse       : String;
    msConnectResponses    : String;                              
    msRingReceiveCount    : Byte;
    msStatusVisible       : Boolean;

    FComPort              : TApdCustomComPort;
    FPhoneNumber          : String;
    FStarted              : Boolean;
    FAllowYielding        : Boolean;

    FAnswerCmd            : String;
    FDialCmd              : String;
    FDialCancelCmd        : String;
    FDialTerminatorCmd    : String;
    FHangupCmd            : String;
    FInitializeCmd        : String;

    FBusyMsg              : String;
    FConnectMsg           : String;
    FDataCompressionMsg   : String;
    FErrorCorrectionMsg   : String;
    FErrorMsg             : String;
    FNoCarrierMsg         : String;
    FNoDialToneMsg        : String;
    FOkMsg                : String;
    FRingMsg              : String;

    FAnswerTimeout        : Cardinal;
    FCmdTimeout           : Cardinal;
    FConnectInfoTimeout   : Cardinal;
    FDialTimeout          : Cardinal;
    FRingWaitTimeout      : Cardinal;

    FDTRDropHoldDelay     : Cardinal;
    FInterCharDelay       : Cardinal;
    FInterCmdDelay        : Cardinal;
    FTildeDelay           : Cardinal;

    FAnswerOnRing         : Byte;
    FAutoRetryDial        : Boolean;
    FConnectSpeed         : Cardinal;
    FDialAttempt          : Cardinal;
    FExtendTime           : Cardinal;
    FForceHangup          : Boolean;
    FLockDTE              : Boolean;
    FMaxDialAttempts      : Cardinal;
    FPreferredPortSpeed   : Cardinal;
    FRetryWaitTime        : Cardinal;
    FShowStatus           : Boolean;
    FStatusDisplay        : TApdSModemStatusDisplay;
    FStatusInfo           : TApdSModemStatusInfo;
    FUpdateStatusTime     : Cardinal;

    FModemState           : TApdSModemStates;
    FModemStateFlags      : Cardinal;
    FConnectionStatus     : TApdSModemEvent;

    procedure SimpleModemStateMachine(Msg, wParam : Cardinal;
                                 lParam : Longint);                 

    procedure DisplayWaitForResult(Responses : AnsiString; Index : Cardinal);
    procedure HostPortClose(CP : TObject; Opening : Boolean);
    function  ParseMultiLineCommand(var Remainder : String) : String;
    procedure SetAllDefaults; virtual;
    procedure SetComPort(const NewPort : TApdCustomComPort);
    procedure SetStarted(const StartIt : Boolean);
    procedure VerifyStarted; virtual;

    procedure RefreshModemStatus; virtual;
    procedure SetModemState(ModemState : TApdSModemStates); virtual;

    function  msResponseMatches(StringToMatch : String) : Boolean;
    function  msResponseIsBusy : Boolean;
    function  msResponseIsConnect : Boolean;
    function  msResponseIsError : Boolean;
    function  msResponseIsNoCarrier : Boolean;
    function  msResponseIsNoDialTone : Boolean;
    function  msResponseIsOk : Boolean;
    function  msResponseIsRing : Boolean;
    procedure msCheckForResponseTags;

    procedure msPrepareForResponse(TimeoutTicks : Cardinal);
    procedure msPrepareForCmdResponse;
    procedure msPrepareForDialResponse;
    procedure msPrepareForAnswerResponse;
    procedure msPrepareForAutoAnswerResponse;
    procedure msPrepareForConnectInfoResponse;

  protected

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent : TComponent;
                           Operation : TOperation); override;

    property ComPort : TApdCustomComPort
      read FComPort write SetComPort;
    property PhoneNumber : String
      read FPhoneNumber write FPhoneNumber;
    property Started : Boolean
      read FStarted write SetStarted;
    property AllowYielding : Boolean
      read FAllowYielding write FAllowYielding default False;

    property AnswerCmd : String
      read FAnswerCmd write FAnswerCmd;
    property DialCmd : String
      read FDialCmd write FDialCmd;
    property DialCancelCmd : String
      read FDialCancelCmd write FDialCancelCmd;
    property DialTerminatorCmd : String
      read FDialTerminatorCmd write FDialTerminatorCmd;
    property HangupCmd : String
      read FHangupCmd write FHangupCmd;
    property InitializeCmd : String
      read FInitializeCmd write FInitializeCmd;

    property BusyMsg : String
      read FBusyMsg write FBusyMsg;
    property ConnectMsg : String
      read FConnectMsg write FConnectMsg;
    property DataCompressionMsg : String
      read FDataCompressionMsg write FDataCompressionMsg;
    property ErrorCorrectionMsg : String
      read FErrorCorrectionMsg write FErrorCorrectionMsg;
    property ErrorMsg : String
      read FErrorMsg write FErrorMsg;
    property NoCarrierMsg : String
      read FNoCarrierMsg write FNoCarrierMsg;
    property NoDialToneMsg : String
      read FNoDialToneMsg write FNoDialToneMsg;
    property OkMsg : String
      read FOkMsg write FOkMsg;
    property RingMsg : String
      read FRingMsg write FRingMsg;

    property AnswerTimeout : Cardinal
      read FAnswerTimeout write FAnswerTimeout default adsmDefAnswerTimeout;
    property CmdTimeout : Cardinal
      read FCmdTimeout write FCmdTimeout default adsmDefCmdTimeout;
    property ConnectInfoTimeout : Cardinal
      read FConnectInfoTimeout write FConnectInfoTimeout
      default adsmDefConnectInfoTimeout;
    property DialTimeout : Cardinal
      read FDialTimeout write FDialTimeout default adsmDefDialTimeout;
    property RingWaitTimeout : Cardinal
      read FRingWaitTimeout write FRingWaitTimeout
      default adsmDefRingWaitTimeout;

    property DTRDropHoldDelay : Cardinal
      read FDTRDropHoldDelay write FDTRDropHoldDelay
      default adsmDefDTRDropHoldDelay;
    property InterCharDelay : Cardinal
      read FInterCharDelay write FInterCharDelay default adsmDefInterCharDelay;
    property InterCmdDelay : Cardinal
      read FInterCmdDelay write FInterCmdDelay default adsmDefInterCmdDelay;
    property TildeDelay : Cardinal
      read FTildeDelay write FTildeDelay default adsmDefTildeDelay;

    property AutoRetryDial : Boolean
      read FAutoRetryDial write FAutoRetryDial default adsmDefAutoRetryDial;
    property ConnectSpeed : Cardinal
      read FConnectSpeed;
    property DialAttempt : Cardinal
      read FDialAttempt;
    property ExtendTime : Cardinal
      read FExtendTime write FExtendTime;
    property ForceHangup : Boolean
      read FForceHangup write FForceHangup default adsmDefForceHangup;
    property LockDTE : Boolean
      read FLockDTE write FLockDTE;
    property MaxDialAttempts : Cardinal
      read FMaxDialAttempts write FMaxDialAttempts;
    property PreferredPortSpeed : Cardinal
      read FPreferredPortSpeed write FPreferredPortSpeed;
    property RetryWaitTime : Cardinal
      read FRetryWaitTime write FRetryWaitTime;
    property ShowStatus : Boolean
      read FShowStatus write FShowStatus default adsmDefShowStatus;

    property ModemState : TApdSModemStates
      read FModemState write SetModemState;
    property ModemStateFlags : Cardinal
      read FModemStateFlags write FModemStateFlags;
    property OnConnectionStatus : TApdSModemEvent
      read FConnectionStatus write FConnectionStatus;

    procedure Answer;
      {Answer the modem immediately -- not documented}
    procedure AutoAnswer(RingCount : Byte);
      {Tell modem to auto answer on Nth ring}
    procedure Cancel;
      {Cancel current modem operation (e.g., dialing)}
    procedure Dial(AutoRetry : Boolean);
      {Attempt connection}
    procedure ExtendConnectTime(ExtraSeconds : Integer);
      {Extend the timer for the current connect attempt}
    procedure Hangup;
      {Break modem connection}
    procedure Initialize;
      {Return modem to known state}
    procedure PutStringModem(ModemString : String);
      {Send the string to the modem}
    procedure Redial;
      {Cause an immediate redial}
  end;

  TApdSModem = class(TApdCustomSModem)
  protected
		FModemIniName       : string;
    FModemName          : string;

  public
    constructor Create(AOwner : TComponent); override;
    procedure ModifyInitCommand;
    function SelectModem: Boolean;
    procedure SetAllDefaults; override;
		procedure SetModemIniName(NewIniFileName : string); virtual;
		procedure SetModemName(ModemName : string); virtual;

  published
    property ComPort;
		property ModemIniName : string
			read FModemIniName write SetModemIniName;
		property ModemName : string
      read FModemName write SetModemName;
    property PhoneNumber;
    property ShowStatus;

    property OnConnectionStatus;
  end;

  TApdSModemStatusDisplay = class(TForm)
    msGroupBox1: TGroupBox;
    msAction: TLabel;
    msActionInfo: TLabel;
    msInfo1: TLabel;
    msInfo2: TLabel;
    msInfo3: TLabel;
    msInfo4: TLabel;
    msValue1: TLabel;
    msValue2: TLabel;
    msValue3: TLabel;
    msValue4: TLabel;
    msGroupBox2: TGroupBox;
    msMessages: TMemo;
    msButtonExtend: TButton;
    msButtonCycle: TButton;
    msButtonCancel: TButton;
    procedure msButtonExtendClick(Sender: TObject);
    procedure msButtonCycleClick(Sender: TObject);
    procedure msButtonCancelClick(Sender: TObject);
    procedure msEraseAllValues;
  private
    { Private declarations }
    msdCurrentRow : Integer;                                        
  protected
    procedure UpdateDisplay(ModemStatus : TApdSModemStatusInfo);
  public
    { Public declarations }
    SModem : TApdCustomSModem;
  end;

procedure GetModemDatabaseEntries(ModemIniName : string;
            var ModemList : TStringList; AtDesignTime : Boolean);

const
  {character used to delimit responses when combined in one string}
  MultiResponseSeparator  : AnsiChar = '|';
  MultiCmdSeparator       : AnsiChar = '|';
  SModemDataPtrID         = 5;

implementation

{Status display form}
{$R *.DFM}

const
  IgnoreCase              = True;

{- TApdSModem State Machine -------------------------------}

procedure TApdCustomSModem.SimpleModemStateMachine(Msg, wParam : Cardinal;
                                 lParam : Longint);
  {- Handle data flow to and from the simple modem component}
const
  msAvailTrigger = $FFFF;   {simulate a handle for an avail trigger}

var
  TriggerID : Cardinal absolute wParam;
  Finished : Boolean;

  function CollectResponse : Boolean;
    {- Check for text responses}
  var
    c        : AnsiChar;

  begin
    {Collect characters until CR/LF}
    Finished := False;
    CollectResponse := False;
    while ComPort.CharReady and not Finished do begin
      c := ComPort.getChar;
      msModemResponse := msModemResponse + Char(c);
      if ComPort.CheckForString(msCRLFIndex, c, #13#10, True) then begin
        msModemResponse := Trim(msModemResponse);
        if (msModemResponse <> '') then begin
          {Got a text response}
          Finished := True;
          CollectResponse := True;
          {Add completed message to connect response buffer}
          if Length(msConnectResponses) + Length(msModemResponse) < 255 then
            msConnectResponses := msConnectResponses + msModemResponse;

          {All error responses are aborts}
          if msResponseIsError then
            SetModemState(smsAbort);
        end;
      end;
    end;
  end;

begin
  {Find which modem instance we are working with}
  {Dispatcher := TApdBaseDispatcher(PortList[LH(lParam).H]);
  with Dispatcher do begin
    Dispatcher.GetDataPointer(Pointer(SModem), SModemDataPtrID);}

    with {SModem, }FComPort do begin
      {If it's a TriggerAvail message, then force the TriggerID to msAvailTrigger}
      if (Msg = APW_TRIGGERAVAIL) then
        TriggerID := msAvailTrigger;

      {Exit immediately on triggers that aren't ours}
      if (TriggerID <> msAvailTrigger) and
         (TriggerID <> msTimeoutTrigger) and
         (TriggerID <> msModemStatusTrigger) and
         (TriggerID <> msUpdateStatusTrigger) then
        Exit;

      {Exit when in connected state unless the DCD status trigger fired}
      if (FModemState = smsConnected) and
         (TriggerID <> msModemStatusTrigger) then
        Exit;

      {Process until we encounter a wait condition}
      repeat
        {Show status periodically}
        if (TriggerID = msUpdateStatusTrigger) or msForceUpdateStatus then begin
          msForceUpdateStatus := False;
          RefreshModemStatus;
          SetTimerTrigger(msUpdateStatusTrigger,
                           Secs2Ticks(FUpdateStatusTime), True);

          {If it was just our timer for a status update, we're done}
          if (TriggerID = msUpdateStatusTrigger) then
            Exit;
        end;

        {Preprocess pending modem responses while in certain states}
        case FModemState of
          smsAutoAnswerBackground,
          smsAutoAnswerWait,
          smsAnswerWait,
          smsDialWait,
          smsConnectWait :
            begin
              if (TriggerID = msAvailTrigger) and (not CollectResponse) then
                {Entire message not ready, exit until more received}
                Exit;
            end;
        end;

        {Process the current modem state}
        case FModemState of
          smsReady,
          smsUnknown :
            {Nothing to do...exit}
            Exit;

          smsAutoAnswerBackground,
          smsAutoAnswerWait :
            begin
              {If message received was RingMsg then we need to increment
               the ring received count.  If RingRecvCount = RingToAnswerOn,
               then we need to clear the RingWaitTimeout timer, send AnswerCmd,
               set a connect timer, prepare for a response, and change to
               AnswerWait state.  If not enough rings have been received, then
               we need to set a timer for RingWaitTimeout and we are done.
              }
              if (FModemState = smsAutoAnswerBackground) then begin
                {Show status when first ring is detected}
                if (TriggerID = msAvailTrigger) then
                  SetModemState(smsAutoAnswerWait);
                Exit;
              end;                                                  
              if (TriggerID = msAvailTrigger) then begin
                if msResponseIsRing then begin
                  inc(msRingReceiveCount);
                  if (msRingReceiveCount = FAnswerOnRing) then begin
                    msPrepareForAnswerResponse;
                    PutStringModem(FAnswerCmd);
                    SetModemState(smsAnswerWait);
                  end else begin
                    {Not enough rings received yet; set timer for next ring}
                    Dispatcher.SetTimerTrigger(msTimeoutTrigger,
                                     FRingWaitTimeout, True);
                  end;
                end else
                  msPrepareForAutoAnswerResponse;
              end else if (TriggerID = msTimeoutTrigger) then begin
                {Didn't receive next ring within specified time, so reset count}
                {Log the fact that we received rings but didn't answer}
                msRingReceiveCount := 0;
                {Modem state stays as AutoAnswerWait}
              end;
            end;

          smsAnswerWait :
            begin
              {We're in the process of answering and waiting for ConnectMsg
               to be received.  Other possible messages to watch for include
               NoCarrier.
              }
              if (TriggerID = msAvailTrigger) then begin
                if msResponseIsConnect then begin
                  {Set a timer to watch for optional data (e.g., data comp.)}
                  Dispatcher.SetTimerTrigger(msTimeoutTrigger,
                                   FConnectInfoTimeout, True);
                  SetModemState(smsConnectWait);
                end else if msResponseIsNoCarrier then begin
                  {Didn't get a connect}
                  SetModemState(smsReady);
                end else begin
                  {Got something else we didn't expect, ignore it}
                end;
              end else if (TriggerID = msTimeoutTrigger) then begin
                {Didn't get a connect}
                SetModemState(smsReady);
              end;
            end;

          smsDialWait :
            begin
              {We're in the process of dialing a call.  We could arrive here
               if we receive any of the following -- NoCarrierMsg,
               NoDialToneMsg, ConnectMsg, or timeout while waiting for data.
              }
              if (TriggerID = msAvailTrigger) then begin
                if msResponseIsConnect then begin
                  {Set a timer to watch for optional data (e.g., data comp.)}
                  msPrepareForConnectInfoResponse;
                  SetModemState(smsConnectWait);
                end else if msResponseIsNoCarrier or
                            msResponseIsBusy then begin
                  {If we're in auto-retry mode, set a timer for the retry delay}
                  if AutoRetryDial then begin
                    Dispatcher.SetTimerTrigger(msTimeoutTrigger,
                                     Secs2Ticks(FRetryWaitTime), True);
                    SetModemState(smsDialCycle);
                    Exit;
                  end else begin
                    SetModemState(smsReady);
                  end;
                end else if msResponseIsNoDialTone then begin
                  FStatusDisplay.UpdateDisplay(FStatusInfo);
                  Dispatcher.SetTimerTrigger(msTimeoutTrigger, 0, False);
                  SetModemState(smsNoDialTone);
                  { one-time status, return to smsReady }
                  SetModemState(smsReady);
                end else
                  {It wasn't a response we were looking for}
                  msPrepareForDialResponse;

              end else if (TriggerID = msTimeoutTrigger) then begin
                {Didn't get a connect, so send the cancel command (and cycle?)}
                PutStringModem(FDialCancelCmd);
                if AutoRetryDial and
                  (FDialAttempt < FMaxDialAttempts) then begin         {!!.01}
                  Dispatcher.SetTimerTrigger(msTimeoutTrigger,
                                   Secs2Ticks(FRetryWaitTime), True);
                  SetModemState(smsDialCycle);
                  Exit;
                end else
                  SetModemState(smsReady);
              end;
            end;

          smsDialCycle :
            begin
              if (TriggerID = msTimeoutTrigger) then begin
                PutStringModem(FDialCmd + ' ' + FPhoneNumber + FDialTerminatorCmd);
                inc(FDialAttempt);
                msPrepareForDialResponse;
                SetModemState(smsDialWait);
              end;
              Exit;                                                    {!!.01}
            end;

          smsConnectWait :
            begin
              {We received a connect message already; now we're waiting for
               optional connect data (e.g., data compression messages) or
               a timeout if no other connect information is sent.}
              if (TriggerID = msAvailTrigger) then begin
                {Got optional connect info or other end sent data very quickly}
                {ProcessConnectInfo(SModem);}
                {Prepare for response but don't set a new timer}
                msModemResponse := '';
                msCRLFIndex := 0;
              end;
              {Now we are connected}
              SetModemState(smsConnected);
            end;

          smsConnected :
            begin
              {We have either just finished waiting for connect information
               and are now considered totally connected and ready for the user
               to proceed (we will also install a status trigger on DCD to
               watch for a line disconnect), or we have entered the state
               machine at this point because the status trigger on DCD fired.
              }
              if (msModemStatusTrigger = 0) then begin
                {Just finished connecting...}
                msModemStatusTrigger := AddStatusTrigger(stModem);
                SetStatusTrigger(msModemStatusTrigger, msDCDDelta, True);
                Exit;
              end else if (TriggerID = msModemStatusTrigger) and
                not(FComPort.DCD) then begin
                {We just lost DCD}
                SetModemState(smsHangup);
                if (msModemStatusTrigger <> 0) then begin
                  RemoveTrigger(msModemStatusTrigger);
                  msModemStatusTrigger := 0;
                end;
                SetModemState(smsReady);
              end;
            end;

        end;
      until {Finished}False;
    end;
  {end;}
end;

{- Dual-purpose code (prop. editor & dialog code) ---------}

procedure GetModemDatabaseEntries(ModemIniName : string;
            var ModemList : TStringList; AtDesignTime : Boolean);
  {Read the list of modems stored in the modem database (INI file)}
var
  IniName       : string;
  ModemIni      : TIniFile;
  {$IFDEF Win32}
  SearchSource  : TRegIniFile;
  {$ELSE}
  SearchSource  : TIniFile;
	{$ENDIF}

	procedure FindModemDatabase(var IniFileName : string);
	var
		TmpRegPath: string;
	begin
		{ Check if the specified modem database exists...if not we can't
			select anything except the default modem. The algorithm for finding
			AWMODEM.INI (or another named modem database) is different for
			run-time and design-time. At run-time we check the WINDOWS directory,
			the paramstr(0) directory, and the current directory. At design-time
			we check the WINDOWS directory, the current directory, and the compiler
			search path (obtained from DELPHI.INI for Delphi 1 and from the
			registry for other versions).
		}
		IniFileName := '';
		TmpRegPath := '';
		if ModemIniName <> '' then begin
			{we have a filename, now check if it already contains a path}
			if ExtractFilePath(ModemIniName) <> '' then begin
				{already have a path, so check if the file exists}
				if FileExists(ModemIniName) then
					IniFileName := ModemIniName;
			end else begin
				{first check the windows directory}
				ModemIni := TIniFile.Create(ModemIniName);
				if ModemIni.ReadInteger('Defaults','_Entries', 0) <> 0 then
					IniFileName := ModemIniName;
				ModemIni.Free;

				if IniFileName = '' then begin
					{not found in windows directory, so continue the search}
					if AtDesignTime then begin
						{design-time search}
						{next check the compiler search path}
						{$IFDEF Win32}
						{$IFDEF Ver90}
						TmpRegPath := 'Software\Borland\Delphi\2.0';
						{$ELSE}
						{$IFDEF Ver100}
						TmpRegPath := 'Software\Borland\Delphi\3.0';
						{$ELSE}
						{$IFDEF Ver120}
						TmpRegPath := 'Software\Borland\Delphi\4.0';
						{$ELSE}
						{$IFDEF Ver130}
						TmpRegPath := 'Software\Borland\Delphi\5.0';
						{$ELSE}
						{$IFDEF Ver93}
						TmpRegPath := 'Software\Borland\C++Builder\1.0';
						{$ELSE}
						{$IFDEF Ver110}
						TmpRegPath := 'Software\Borland\C++Builder\3.0';
						{$ELSE}
						{$IFDEF Ver125}
						TmpRegPath := 'Software\Borland\C++Builder\4.0';
						{$ELSE}
						{$IFDEF Ver130}
							{$IFDEF BCB}
							TmpRegPath := 'Software\Borland\C++Builder\5.0';
							{$ENDIF}
						{$ENDIF}
						{$ENDIF}
						{$ENDIF}
						{$ENDIF}
						{$ENDIF}
						{$ENDIF}
						{$ENDIF}
						{$ENDIF}
						SearchSource := TRegIniFile.Create(TmpRegPath);
						{$ELSE}
						SearchSource := TIniFile.Create('DELPHI.INI');
						{$ENDIF}
						IniFileName :=
							FileSearch(ModemIniName,
												 SearchSource.ReadString('Library', 'SearchPath', ''));
						SearchSource.Free;

						{if we still have a path, it was in the current directory, so
						 expand the path}
						if (IniFileName = ModemIniName) then
							IniFileName := ExpandFileName(ModemIniName);
					end else begin
						{run-time search}
						{check the paramstr(0) directory, then the current directory}
						if FileExists(ExtractFilePath(ParamStr(0))+ModemIniName) then
							IniFileName := ModemIniName
						else if FileExists(ModemIniName) then
							IniFileName := ExpandFileName(ModemIniName);
					end;
				end;
			end;
		end;
	end;

begin
	if not assigned(ModemList) then
		Exit;

	{Create the string list of modem names}
	ModemList.Clear;
	ModemList.Sorted := False;  {keep the names in same order as INI file}

	{If we don't have an ini source, we can't need to do the following}
	FindModemDatabase(IniName);
	if IniName <> '' then begin
		{open the database and read the number of entries}
		ModemIni := TIniFile.Create(string(IniName));
		ModemIni.ReadSection('Index', ModemList);
		ModemIni.Free;
	end;

	{add the "default" modem to the list}
	ModemList.Insert(0, adsmDefaultModemTag);
end;

{- TApdCustomSModem ---------------------------------------}

constructor TApdCustomSModem.Create(AOwner : TComponent);
var
  i : Integer;
begin
  inherited Create(AOwner);

  SetModemState(smsUnknown);
  SetAllDefaults;

  {Search our owner for a com port}
  if Assigned(AOwner) and (AOwner.ComponentCount > 0) then
    for I := 0 to Pred(AOwner.ComponentCount) do
      if AOwner.Components[I] is TApdCustomComPort then begin
        FComPort := TApdCustomComPort(AOwner.Components[I]);
        Break;
      end;

  {Create a modem status display and link this modem instance to it}
  FStatusDisplay := TApdSModemStatusDisplay.Create(nil);
  with FStatusDisplay do begin
    Visible := False;
    SModem := Self;
    msEraseAllValues;
  end;

  {Create a status info class}
  FStatusInfo := TApdSModemStatusInfo.Create(Self);

end;

destructor TApdCustomSModem.Destroy;
begin
  FStatusDisplay.Free;
  FStatusDisplay := nil;
  FStatusInfo.Free;
  FStatusInfo := nil;

  inherited Destroy;
end;

procedure TApdCustomSModem.Notification(AComponent : TComponent;
                                        Operation : TOperation);
  {Link/unlink comport when dropped or removed from form}
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then begin
    {See if our com port is going away}
    if (AComponent = FComPort) then
      FComPort := nil;
  end else if (Operation = opInsert) then begin
    {Check for a com port being installed}
    if not Assigned(FComPort) and (AComponent is TApdCustomComPort) then
      ComPort := TApdCustomComPort(AComponent);
  end;
end;

function TApdCustomSModem.ParseMultiLineCommand(var Remainder : String) : String;
  {Return the first part of a multi-line modem command from the string and
   update the remainder with the first part removed}
var
  DelimPos : Integer;

begin
  DelimPos := pos(Char(MultiCmdSeparator), Remainder);
  if DelimPos > 0 then begin
    Result := copy(Remainder, 1, pred(DelimPos));
    Remainder := copy(Remainder, succ(DelimPos), length(Remainder));
  end else begin
    Result := Remainder;
    Remainder := '';
  end;
end;

procedure TApdCustomSModem.SetStarted(const StartIt : Boolean);
  {Attempt to start (or stop) modem component}
begin
  if (csDesigning in ComponentState) or
     (csLoading in ComponentState) or
     (StartIt = FStarted) then
    Exit;

  {Validate comport}
  if not Assigned(FComPort) then
    raise EPortNotAssigned.Create(ecPortNotAssigned, False);

  if (FComPort.DeviceLayer = dlWinSock) then
    raise ECannotUseWithWinSock.Create(ecCannotUseWithWinSock, False); 

  with FComPort do
    if StartIt then begin
      {Open the port if it isn't already}
      Open := True;

      {Register our trigger handler procedure}
      Dispatcher.RegisterEventTriggerHandler(SimpleModemStateMachine);

      {Register our data pointer to the comport record}
      Dispatcher.SetDataPointer(Self, SModemDataPtrID);

      {Install callback for port open/close}
      RegisterUserCallback(HostPortClose);

      {Set the best port speed for the modem}
      Baud := FPreferredPortSpeed;

      {Force the trigger length to 1}
      Dispatcher.ChangeLengthTrigger(1);

      {Set a timer for the status update}
      msUpdateStatusTrigger := AddTimerTrigger;
      Dispatcher.SetTimerTrigger(msUpdateStatusTrigger,
                       Secs2Ticks(FUpdateStatusTime), True);

      {Define a timer to use for general timeouts}
      msTimeoutTrigger := AddTimerTrigger;
    end else begin
      {Unhook our trigger handler procedure}
      Dispatcher.DeregisterEventTriggerHandler(SimpleModemStateMachine);

      {Set the comport record data trigger to nil}
      Dispatcher.SetDataPointer(nil, SModemDataPtrID);

      {Remove port open/close callback}
      DeregisterUserCallback(HostPortClose);

      {Change the modem status}
      SetModemState(smsUnknown);

      {Deallocate triggers}
      if (msUpdateStatusTrigger <> 0) then begin
        RemoveTrigger(msUpdateStatusTrigger);
        msUpdateStatusTrigger := 0;
      end;
      if (msTimeoutTrigger <> 0) then begin
        RemoveTrigger(msTimeoutTrigger);
        msTimeoutTrigger := 0;
      end;
      if (msModemStatusTrigger <> 0) then begin
        RemoveTrigger(msModemStatusTrigger);
        msModemStatusTrigger := 0;
      end;
    end;

  FStarted := StartIt;
end;

procedure TApdCustomSModem.VerifyStarted;
  {Make sure modem is connected to valid comport and ready for use}
begin
  if not Assigned(FComPort) then
    raise EPortNotAssigned.Create(ecPortNotAssigned, False);

  if not Started then begin
    {attempt to start once}
    Started := True;
    if not Started then
      {if still not started...}
      raise EModemNotStarted.Create(ecModemNotStarted, False);
  end;

  {Check for reentrancy; should apply when AllowYielding = True}
  if (FModemStateFlags and smsWaitForInProgress) <> 0 then
    raise EModemBusy.Create(ecModemBusy, False);
end;

procedure TApdCustomSModem.HostPortClose(CP : TObject; Opening : Boolean);
  {Called when the host port for the modem is opened or closed}
begin
  if not Opening then
{    Started := False; }
end;

procedure TApdCustomSModem.SetComPort(const NewPort : TApdCustomComPort);
begin
  if NewPort <> FComPort then begin
    FComPort := NewPort;
  end;
end;

procedure TApdCustomSModem.SetAllDefaults;
begin
  FAnswerCmd := adsmDefAnswerCmd;
  FDialCmd := adsmDefDialCmd;
  FDialCancelCmd := adsmDefDialCancelCmd;
  FDialTerminatorCmd := adsmDefDialTerminatorCmd;
  FHangupCmd := adsmDefHangupCmd;
  FInitializeCmd := adsmDefInitializeCmd;

  FBusyMsg := adsmDefBusyMsg;
  FConnectMsg := adsmDefConnectMsg;
  FDataCompressionMsg := adsmDefDataCompressionMsg;
  FErrorCorrectionMsg := adsmDefErrorCorrectionMsg;
  FErrorMsg := adsmDefErrorMsg;
  FNoCarrierMsg := adsmDefNoCarrierMsg;
  FNoDialToneMsg := adsmDefNoDialToneMsg;
  FOkMsg := adsmDefOkMsg;
  FRingMsg := adsmDefRingMsg;

  FAnswerTimeout := adsmDefAnswerTimeout;
  FCmdTimeout := adsmDefCmdTimeout;
  FConnectInfoTimeout := adsmDefConnectInfoTimeout;
  FDialTimeout := adsmDefDialTimeout;
  FRingWaitTimeout := adsmDefRingWaitTimeout;

  FDTRDropHoldDelay := adsmDefDTRDropHoldDelay;
  FInterCharDelay := adsmDefInterCharDelay;
  FInterCmdDelay := adsmDefInterCmdDelay;
  FTildeDelay := adsmDefTildeDelay;

  FExtendTime := adsmDefExtendTime;
  FRetryWaitTime := adsmDefRetryWaitTime;
  FUpdateStatusTime := adsmDefUpdateStatusTime;

  FAutoRetryDial := adsmDefAutoRetryDial;
  FForceHangup := adsmDefForceHangup;
  FMaxDialAttempts := adsmDefMaxDialAttempts;
  FShowStatus := adsmDefShowStatus;
end;

procedure TApdCustomSModem.RefreshModemStatus;
  {Refresh the modem status dialog and call the user event (if hooked)}
var
  TimeRemaining : Longint;

begin
  {Update the modem info class}
  with FStatusInfo, FComPort do begin
    if FModemState = smsUnknown then
      Exit;
    msiModemState := FModemState;
    case FModemState of
      smsDialWait :
        begin
          Dispatcher.TimerTicksRemaining(msTimeoutTrigger, TimeRemaining);
          SetInfoDialWait(FPhoneNumber, FDialAttempt, FMaxDialAttempts,
                          Ticks2Secs(TimeRemaining));
        end;
      smsDialCycle :
        begin
          Dispatcher.TimerTicksRemaining(msTimeoutTrigger, TimeRemaining);
          SetInfoDialCycle(FPhoneNumber, FDialAttempt+1, FMaxDialAttempts,{!!.01}
                           Ticks2Secs(TimeRemaining));
        end;
      smsAnswerWait :
        begin
          Dispatcher.TimerTicksRemaining(msTimeoutTrigger, TimeRemaining);
          SetInfoAnswerWait(Ticks2Secs(TimeRemaining));
        end;
      smsAutoAnswerWait :
        begin
          SetInfoAutoAnswerWait(FAnswerOnRing, msRingReceiveCount);
        end;
    end;

    {Show/Hide the status display if appropriate}
    if FShowStatus and
      Assigned(FStatusDisplay) then begin
      case FModemState of
        smsReady, smsAutoAnswerBackground, smsConnected:
          if msStatusVisible and
             ((FModemStateFlags and smsBatchInProgress) = 0) then begin
            msStatusVisible := False;
            FStatusDisplay.Hide;
          end;
      else
        begin
          if not msStatusVisible then begin
            msStatusVisible := True;
            FStatusDisplay.Show;
            FStatusDisplay.Repaint;
          end;
          FStatusDisplay.UpdateDisplay(FStatusInfo);
        end;
      end;
    end;

    {Call the user's status hook}
    if Assigned(FConnectionStatus) then
      FConnectionStatus(Self, FStatusInfo);
  end;
end;

procedure TApdCustomSModem.SetModemState(ModemState : TApdSModemStates);
  {Update the modem state variable and call any procedures hooked on status}
begin
  if ModemState <> FModemState then begin
    FModemState := ModemState;

    if (FModemState = smsReady) or
       (FModemState = smsUnknown) then
      {Disable status trigger}
      FComPort.Dispatcher.SetTimerTrigger(msUpdateStatusTrigger,
                       Secs2Ticks(FUpdateStatusTime), False)
    else
      {Enable status trigger}
      FComPort.Dispatcher.SetTimerTrigger(msUpdateStatusTrigger,
                       Secs2Ticks(FUpdateStatusTime), True);

    if (FModemState = smsDialWait) or
       (FModemState = smsDialCycle) and
       (not FStatusDisplay.msButtonCycle.Enabled) then begin
      FStatusDisplay.msButtonCycle.Enabled := True;
      FStatusDisplay.Repaint;
    end else if (FStatusDisplay.msButtonCycle.Enabled) then begin
      FStatusDisplay.msButtonCycle.Enabled := False;
      FStatusDisplay.Repaint;
    end;

    if (FModemState <> smsUnknown) then
      RefreshModemStatus;
  end;
end;

function TApdCustomSModem.msResponseMatches(StringToMatch : String) : Boolean;
  {Check if the latest modem response contains string to match}
begin
  if (pos(StringToMatch, msModemResponse) > 0) then begin
    Result := True;
    FStatusInfo.AddSendRecvData('[recv] ' + StringToMatch);
  end else
    Result := False;
end;

function TApdCustomSModem.msResponseIsBusy : Boolean;
  {Check if the latest modem response contains BusyMsg}
begin
  Result := msResponseMatches(FBusyMsg);
end;

procedure TApdCustomSModem.msCheckForResponseTags;
  {Check if the connect responses contain ErrorCompression, DataCompression
   or ConnectSpeed tags}
var
  S,
  TempStr,
  ErrorCorrection,
  DataCompression : String;
  CommaPos : Integer;
begin
    S := FErrorCorrectionMsg + ',';
    while Length(S) > 0 do begin
      CommaPos := Pos(',', S);
      if CommaPos = 0 then CommaPos := Length(S);
      TempStr := Copy(S, 1, CommaPos-1);
      if Pos(TempStr, S) > 0 then begin
        If Length(ErrorCorrection) > 0 then ErrorCorrection := ErrorCorrection + ',';
        ErrorCorrection := ErrorCorrection + TempStr;
      end;
       S := Copy(S, CommaPos + 1, Length(S));
    end;

    S := FDataCompressionMsg + ',';
    while Length(S) > 0 do begin
      CommaPos := Pos(',', S);
      if CommaPos = 0 then CommaPos := Length(S);
      TempStr := Copy(S, 1, CommaPos-1);
      if Pos(TempStr, S) > 0 then begin
        If Length(DataCompression) > 0 then DataCompression := DataCompression + ',';
        DataCompression := DataCompression + TempStr;
      end;
       S := Copy(S, CommaPos + 1, Length(S));
    end;

    if Length(msModemResponse) > Length(FConnectMsg) then begin
      TempStr := Copy(msModemResponse, Length(FConnectMsg) + 1, Length(msModemResponse));
      S := msModemResponse;
      FConnectSpeed := StrToIntDef(msModemResponse, 0);
    end;
    FStatusInfo.SetInfoConnected(ErrorCorrection, DataCompression, FConnectSpeed);
end;

function TApdCustomSModem.msResponseIsConnect : Boolean;
  {Check if the latest modem response contains ConnectMsg}
begin
  Result := msResponseMatches(FConnectMsg);
  if Result then begin
    {check for error correction, data compression tags in connect responses}
    msCheckForResponseTags;                                        
  end;
end;

function TApdCustomSModem.msResponseIsError : Boolean;
  {Check if the latest modem response contains ErrorMsg}
begin
  Result := msResponseMatches(FErrorMsg);
end;

function TApdCustomSModem.msResponseIsNoCarrier : Boolean;
  {Check if the latest modem response contains NoCarrierMsg}
begin
  Result := msResponseMatches(FNoCarrierMsg);
end;

function TApdCustomSModem.msResponseIsNoDialTone : Boolean;
  {Check if the latest modem response contains NoDialToneMsg}
begin
  Result := msResponseMatches(FNoDialToneMsg);
end;

function TApdCustomSModem.msResponseIsOk : Boolean;
  {Check if the latest modem response contains OkMsg}
begin
  Result := msResponseMatches(FOkMsg);
end;

function TApdCustomSModem.msResponseIsRing : Boolean;
  {Check if the latest modem response contains RingMsg}
begin
  Result := msResponseMatches(FRingMsg);
end;


procedure TApdCustomSModem.msPrepareForResponse(TimeoutTicks : Cardinal);
  {Reset a response timer and initialize the response string}
begin
  msModemResponse := '';
  msCRLFIndex := 0;
  FComPort.SetTimerTrigger(msTimeoutTrigger, TimeoutTicks, True);
end;

procedure TApdCustomSModem.msPrepareForCmdResponse;
begin
  msPrepareForResponse(FCmdTimeout);
end;

procedure TApdCustomSModem.msPrepareForDialResponse;
begin
  msPrepareForResponse(Secs2Ticks(FDialTimeout));
end;

procedure TApdCustomSModem.msPrepareForAnswerResponse;
begin
  msPrepareForResponse(Secs2Ticks(FAnswerTimeout));
end;

procedure TApdCustomSModem.msPrepareForAutoAnswerResponse;
begin
  msPrepareForResponse(FRingWaitTimeout);
end;

procedure TApdCustomSModem.msPrepareForConnectInfoResponse;
begin
  msPrepareForResponse(FConnectInfoTimeout);
end;

procedure TApdCustomSModem.DisplayWaitForResult(Responses : AnsiString; Index : Cardinal);
var
  Rsp : String;
  i   : Integer;                                                     
  j   : Cardinal;

begin
  Rsp := '';
  i := 1;
  j := 1;
  while (i < Length(Responses)) and
        (j < Index) do begin
    if (Responses[i] = MultiResponseSeparator) then
      inc(j);
    inc(i);
  end;
  if (j = Index) then begin
    while (i < Length(Responses)) and
          (Responses[i] <> MultiResponseSeparator) do begin
      Rsp := Rsp + Char(Responses[i]);
      inc(i);
    end;
  end;
  FStatusInfo.AddSendRecvData('[recv] ' + Rsp);
  FStatusDisplay.UpdateDisplay(FStatusInfo);
end;

procedure TApdCustomSModem.Answer;
  {Answer the modem immediately -- not documented for general use}
begin
  if (FModemStateFlags and smsWaitForInProgress) <> 0 then
    {modem is already waiting for data; prevent reentrancy}
    raise EModemBusy.Create(ecModemBusy, False);

  {make sure modem is ready}
  VerifyStarted;

  FComPort.FlushInBuffer;
  FStatusDisplay.msEraseAllValues;

  {If already dialing, then we have an implied Cancel; if already
   connected, then we have an implied Hangup}
  FModemStateFlags := FModemStateFlags or smsBatchInProgress;
  Hangup;

  {If Initialize failed, exit}
  FModemStateFlags := FModemStateFlags and not smsBatchInProgress;
  if (FModemState <> smsReady) then
    Exit;

  {Send the answer command and let the state machine handle the response}
  msPrepareForAnswerResponse;
  PutStringModem(FAnswerCmd);
  SetModemState(smsAnswerWait);
end;

procedure TApdCustomSModem.AutoAnswer(RingCount : Byte);
  {Tell modem to auto answer on Nth ring}
begin
  if (FModemStateFlags and smsWaitForInProgress) <> 0 then
    {modem is already waiting for data; prevent reentrancy}
    raise EModemBusy.Create(ecModemBusy, False);

  {If already dialing, then we have an implied Cancel; if already
   connected, then we have an implied Hangup}
  FModemStateFlags := FModemStateFlags or smsBatchInProgress;
  Hangup;
  Initialize;

  FModemStateFlags := FModemStateFlags and not smsBatchInProgress;
  if (FModemState <> smsReady) then
    Exit;

  msPrepareForCmdResponse;
  SetModemState(smsAutoAnswerBackground);
  msRingReceiveCount := 0;
  FAnswerOnRing := RingCount;
end;

procedure TApdCustomSModem.Cancel;
  {Cancel current modem operation (e.g., dialing)}
var
  ExpectedResponses : AnsiString;

begin
  if (FModemStateFlags and smsWaitForInProgress) <> 0 then
    {modem is already waiting for data; prevent reentrancy}
    raise EModemBusy.Create(ecModemBusy, False);

  {make sure modem is ready}
  VerifyStarted;

  with FComPort do begin
    {If connected, then Cancel implies Hangup}
    If (msModemStatusTrigger <> 0) then
      Hangup
    else begin
      {Cancel any active timers}
      Dispatcher.SetTimerTrigger(msTimeoutTrigger, 0, False);

      if (FModemState = smsDialWait) or
         (FModemState = smsAnswerWait) then begin
        {Need to cancel the dial or answer in progress}
        FModemStateFlags := FModemStateFlags or smsWaitForInProgress;
        SetModemState(smsCancel);
        ExpectedResponses := AnsiString(OkMsg + #13#10 + Char(MultiResponseSeparator) +
                                        ErrorMsg + #13#10 + Char(MultiResponseSeparator) +
                                        NoCarrierMsg + #13#10 + Char(MultiResponseSeparator) +
                                        FDialCancelCmd);
        {Send the cancel command}
        {$IFDEF Win32}
        PrepareWait;
        {$ENDIF}
        PutStringModem(FDialCancelCmd);
        {Wait for a response from the modem or a timeout}
        WaitForMultiString(ExpectedResponses, CmdTimeout, FAllowYielding,
                           IgnoreCase, MultiResponseSeparator);
        FModemStateFlags := FModemStateFlags and not smsWaitForInProgress;
      end;

      FComPort.FlushInBuffer;
      SetModemState(smsReady);
    end;
  end;
end;

procedure TApdCustomSModem.Dial(AutoRetry : Boolean);
  {Attempt connection}
begin
  if (FModemStateFlags and smsWaitForInProgress) <> 0 then
    {modem is already waiting for data; prevent reentrancy}
    raise EModemBusy.Create(ecModemBusy, False);

  if (FPhoneNumber <> '') then begin
    FStatusDisplay.msEraseAllValues;

    FDialAttempt := 1;
    FAutoRetryDial := AutoRetry;

    {If already dialing, then we have an implied Cancel; if already
     connected, then we have an implied Hangup}
    FModemStateFlags := FModemStateFlags or smsBatchInProgress;
    Hangup;
    Initialize;

    {If Initialize failed, exit}
    FModemStateFlags := FModemStateFlags and not smsBatchInProgress;
    if (FModemState <> smsReady) then
      Exit;

    msPrepareForDialResponse;
    SetModemState(smsDialWait);
    PutStringModem(FDialCmd + ' ' + FPhoneNumber + FDialTerminatorCmd);
    {flow continues in the state machine}
  end;
end;

procedure TApdCustomSModem.ExtendConnectTime(ExtraSeconds : Integer);
  {Extend the timer for the current connect attempt}
begin
  with FComPort do
    Dispatcher.ExtendTimer(msTimeoutTrigger, ExtraSeconds);
end;

procedure TApdCustomSModem.Hangup;
  {Break modem connection if connected.  If not connected, then Hangup
   implies a Cancel of the current operation (unless.}
var
  ExpectedResponses : AnsiString;

begin
  if (FModemStateFlags and smsWaitForInProgress) <> 0 then
    {modem is already waiting for data; prevent reentrancy}
    raise EModemBusy.Create(ecModemBusy, False);

  {make sure modem is ready}
  VerifyStarted;

  with FComPort do begin
    if (FModemState = smsConnected) or
       (FForceHangup) then begin
      FModemStateFlags := FModemStateFlags or smsWaitForInProgress;
      SetModemState(smsHangup);
      if (FHangupCmd = 'DTR') then begin
        {Do a hardware hangup}
        DTR := False;
        DelayTicks(FDTRDropHoldDelay, AllowYielding);
        DTR := True;
      end else begin
        {Do a software hangup}
        ExpectedResponses := AnsiString(OkMsg + #13#10 + Char(MultiResponseSeparator) +
                                        ErrorMsg + #13#10 + Char(MultiResponseSeparator) +
                                        NoCarrierMsg + #13#10);
        {$IFDEF Win32}
        PrepareWait;
        {$ENDIF}
        PutStringModem(FHangupCmd);
        {wait for a response from the modem or a timeout}
        WaitForMultiString(ExpectedResponses, CmdTimeout, FAllowYielding,
                           IgnoreCase, MultiResponseSeparator);
      end;

      if (msModemStatusTrigger <> 0) then
        RemoveTrigger(msModemStatusTrigger);
      msModemStatusTrigger := 0;

      FModemStateFlags := FModemStateFlags and not smsWaitForInProgress;
      SetModemState(smsReady);
    end else
      {Cancel implied}
      Cancel;
  end;
end;

procedure TApdCustomSModem.Initialize;
  {return modem to known state}
const
  {init results from WaitForMultiString}
  irTimeout = 0;
  irOk      = 1;
  irError   = 2;

var
  InitResult        : Integer;
  InitCmdRemainder  : String;
  InitCmdPart       : String;
  ExpectedResponses : AnsiString;

begin
  if (FModemStateFlags and smsWaitForInProgress) <> 0 then
    {modem is already waiting for data; prevent reentrancy}
    raise EModemBusy.Create(ecModemBusy, False);

  {make sure modem is ready}
  VerifyStarted;

  {update the modem state}
  SetModemState(smsInitialize);
  FModemStateFlags := FModemStateFlags or smsWaitForInProgress;

  InitResult := irOk;
  ExpectedResponses := AnsiString(OkMsg + #13#10+ Char(MultiResponseSeparator) + ErrorMsg + #13#10);
  InitCmdRemainder := InitializeCmd;
  InitCmdPart := ParseMultiLineCommand(InitCmdRemainder);
  with FComPort do begin
    while (InitCmdPart <> '') and (InitResult = irOk) do begin
      {$IFDEF Win32}
      PrepareWait;
      {$ENDIF}
      {send the initialize command}
      PutStringModem(InitCmdPart);

      {Wait for a response from the modem or a timeout}
      InitResult := WaitForMultiString(ExpectedResponses, CmdTimeout,
                                       FAllowYielding, IgnoreCase,
                                       MultiResponseSeparator);
      {Display the returned value}
      DisplayWaitForResult(ExpectedResponses, InitResult);

      {Parse next part of command}
      InitCmdPart := ParseMultiLineCommand(InitCmdRemainder);

      {Wait for intercommand delay}
      if (FInterCmdDelay > 0) then
        DelayTicks(FInterCmdDelay, AllowYielding);
    end;
  end;

  FModemStateFlags := FModemStateFlags and not smsWaitForInProgress;
  if InitResult = irTimeout then begin
    SetModemState(smsInitializeTimeout);
    raise EModemNotResponding.Create(ecModemNotResponding, False);
  end else if InitResult = irError then begin
    SetModemState(smsAbort);
    raise EModemRejectedCommand.Create(ecModemRejectedCommand, False);
  end else
    SetModemState(smsReady);

  {flush any remaining characters from the input buffer (e.g., <cr><lf>)}
  FComPort.FlushInBuffer;
end;

procedure TApdCustomSModem.PutStringModem(ModemString : String);
  {send string to modem, translating special codes and adding interchar delay}
var
  i   : Integer;
  len : Integer;
  Str : String;

begin
  with FComPort do begin
    Str := '';
    i := 0;
    len := length(ModemString);
    while i < len do begin
      inc(i);
      case ModemString[i] of
        '^' :
          begin
            {embedded control character in string? ('a'..'z' or 'A'..'Z'?)}
            if (i <> len) and
               CharInSet(chr(ord(ModemString[i+1]) and $5F), ['A'..'Z']) then begin
              inc(i);   {skip over '^' and send control character}
              Dispatcher.PutChar(AnsiChar((ord(ModemString[i]) and $5F) - ord('A') + 1));
            end else begin
              {just write the character '^'}
              Dispatcher.PutChar(AnsiChar(ModemString[i]));
              Str := Str + ModemString[i];
            end;
          end;
        '~' :
          begin
            {delay for TildeDelay ticks}
            DelayTicks(TildeDelay, AllowYielding);
          end;
      else
        {normal character}
        Dispatcher.PutChar(AnsiChar(ModemString[i]));
        Str := Str + ModemString[i];
      end;

      {allow for interchar delay}
      if (InterCharDelay <> 0) and
         (i <> len) then
        DelayTicks(InterCharDelay, AllowYielding);
    end;

    FStatusInfo.AddSendRecvData('[send] '+ Str);
  end;
end;

procedure TApdCustomSModem.Redial;
  {Cause an immediate redial}
begin
  with FComPort do
    if (FModemState = smsDialCycle) then
      {Set the timer to expire in 1 tick}
      Dispatcher.SetTimerTrigger(msTimeoutTrigger, 1, True)
    else
      Dial(FAutoRetryDial);
end;

{- TApdSModem ---------------------------------------------}

constructor TApdSModem.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  ModemIniName := adsmDefModemIniName;
  ModemName := adsmDefaultModemTag;
end;

procedure TApdSModem.ModifyInitCommand;
var
  WStartPos : Integer;
  WEndPos   : Integer;
  NewCmd    : String;

begin
  {We are actually using the 'config' command from the modem database, so
   throw away the &W at the end, and make it the 'initialize' command}
  NewCmd := FInitializeCmd;
  WStartPos := pos('&W', AnsiUpperCase(NewCmd));
  while (WStartPos <> 0) do begin
    {There's at least one more &W in the string to remove}
    WEndPos := succ(WStartPos);
    {Look for a numeric value following &W and remove it also}
    if (NewCmd[succ(WEndPos)] >= '0') or
       (NewCmd[succ(WEndPos)] <= '9') then
      inc(WEndPos);
    Delete(NewCmd, WStartPos, WEndPos-WStartPos);
    WStartPos := pos('&W', AnsiUpperCase(NewCmd));
  end;

  {Change the definition of InitializeCmd}
  FInitializeCmd := NewCmd;
end;

function TApdSModem.SelectModem: Boolean;
	{Populate a list with modem names from database and display a select dialog}
var
	ModemList : TStringList;
	PortList  : TStringList;
	i         : Cardinal;
	SelDialog : TAdSModemSelection;
	ValResult : Integer;
	NewComNum : Integer;
begin
	ModemList := TStringList.Create;
	PortList := TStringList.Create;

	GetModemDatabaseEntries(ModemIniName, ModemList, False);
	{build the port list}
	for i := 1 to MaxComHandles do
		if IsPortAvailable(i) then
			PortList.Add(ComName(i));
	SelDialog := TAdSModemSelection.Create(nil);
	SelDialog.ModemList.Items.Assign(ModemList);
	SelDialog.ModemList.Text := string(FModemName);
	SelDialog.PortList.Items.Assign(PortList);
	if FComPort.ComNumber <> 0 then
		SelDialog.PortList.Text := ComName(FComPort.ComNumber)
	else
		SelDialog.PortList.Text := '<unknown>';

	Result := False;
	if (SelDialog.ShowModal = mrOk) then begin
		{unpack values}
		try
			Result := True;
			ModemName := SelDialog.ModemList.Text;
			Val(copy(SelDialog.PortList.Text,4,4), NewComNum, ValResult);
			if ValResult = 0 then begin
				FComPort.ComNumber := NewComNum;
				Started := True;
			end;
		except
			raise;
		end;
	end;

	SelDialog.Free;
	ModemList.Free;
	PortList.Free;
end;

procedure TApdSModem.SetAllDefaults;
begin
  inherited SetAllDefaults;
  ModifyInitCommand;
end;

procedure TApdSModem.SetModemIniName(NewIniFileName : string);
begin
  FModemIniName := NewIniFileName;
end;

procedure TApdSModem.SetModemName(ModemName : string);
	{Read and set the parameters for ModemName from modem INI database}
var
	IniFile : TIniFile;
begin
	if FModemName <> ModemName then
	begin
		FModemName := ModemName;

		if ModemName = adsmDefaultModemTag then
			SetAllDefaults
		else
		begin
			{next read the parameters for this modem from the ini database}
			IniFile := TIniFile.Create(ModemIniName);

			with IniFile do begin
				FAnswerCmd := ReadString(ModemName,'AnswerCmd',adsmDefAnswerCmd);
				FDialCmd := ReadString(ModemName,'DialCmd',adsmDefDialCmd);
				FDialCancelCmd := ReadString(ModemName,'DialCancel',adsmDefDialCancelCmd);
				FDialTerminatorCmd := ReadString(ModemName,'DialTerm',adsmDefDialTerminatorCmd);
				FHangupCmd := ReadString(ModemName,'HangupCmd',adsmDefHangupCmd);
				FInitializeCmd := ReadString(ModemName,'ConfigCmd',adsmDefInitializeCmd);
				ModifyInitCommand;

				FBusyMsg := ReadString(ModemName,'BusyMsg',adsmDefBusyMsg);
				FConnectMsg := ReadString(ModemName,'ConnectMsg',adsmDefConnectMsg);
				FDataCompressionMsg := ReadString(ModemName,'CompressTags',adsmDefDataCompressionMsg);
				FErrorCorrectionMsg := ReadString(ModemName,'ErrorCheckTags',adsmDefErrorCorrectionMsg);
				FErrorMsg := ReadString(ModemName,'ErrorMsg',adsmDefErrorMsg);
				FNoCarrierMsg := ReadString(ModemName,'NoCarrierMsg',adsmDefNoCarrierMsg);
				FNoDialToneMsg := ReadString(ModemName,'NoDialToneMsg',adsmDefNoDialToneMsg);
				FOkMsg := ReadString(ModemName,'OkMsg',adsmDefOkMsg);
				FRingMsg := ReadString(ModemName,'RingMsg',adsmDefRingMsg);

				FPreferredPortSpeed := ReadInteger(ModemName, 'DefaultBaud', 9600);
				FLockDTE := ReadBool(ModemName, 'LockDTE', (PreferredPortSpeed >= 9600));
			end;
		end;
	end;
end;

{- Modem status display -----------------------------------}

procedure TApdSModemStatusDisplay.msEraseAllValues;
var
  i : Integer;

begin
  msAction.Caption := '';
  msActionInfo.Caption := '';
  msInfo1.Caption := '';
  msInfo2.Caption := '';
  msInfo3.Caption := '';
  msInfo4.Caption := '';
  msValue1.Caption := '';
  msValue2.Caption := '';
  msValue3.Caption := '';
  msValue4.Caption := '';
  for i := 0 to pred(msMessages.Lines.Count) do
    msMessages.Lines[i] := '';
  msdCurrentRow := 0;
end;

procedure TApdSModemStatusDisplay.msButtonExtendClick(Sender: TObject);
begin
  if Assigned(SModem) then
    SModem.ExtendConnectTime(Secs2Ticks(SModem.FExtendTime));
end;

procedure TApdSModemStatusDisplay.msButtonCycleClick(Sender: TObject);
begin
   if Assigned(SModem) then
     SModem.Redial;
end;

procedure TApdSModemStatusDisplay.msButtonCancelClick(Sender: TObject);
begin
  if Assigned(SModem) then
    SModem.Cancel;
end;

procedure TApdSModemStatusDisplay.UpdateDisplay(ModemStatus : TApdSModemStatusInfo);
var
  i : Integer;

begin
  with ModemStatus, msiModemStateData do begin
    {Clear old general status info}

    {Update the general status info}
    msAction.Caption := GetModemStateMsg;
    case GetModemState of
      smsReady,
      smsInitialize,
      smsInitializeTimeout,
      smsConnectWait,
      smsHangup,
      smsCancel :
        begin
          msInfo1.Caption  := '';
          msValue1.Caption := '';
          msInfo2.Caption  := '';
          msValue2.Caption := '';
          msInfo3.Caption  := '';
          msValue3.Caption := '';
          msInfo4.Caption  := '';
          msValue4.Caption := '';
        end;
      smsAutoAnswerWait :
        begin
        end;
      smsAnswerWait :
        begin
          msInfo1.Caption  := 'Time Remaining';
          msValue1.Caption := IntToStr(TimeRemaining);
          msInfo2.Caption  := '';
          msValue2.Caption := '';
          msInfo3.Caption  := '';
          msValue3.Caption := '';
          msInfo4.Caption  := '';
          msValue4.Caption := '';
        end;
      smsDialWait :
        begin
          msInfo1.Caption  := 'Phone Number';
          msValue1.Caption := String1;
          msInfo2.Caption  := 'Dial Attempt';
          msValue2.Caption := IntToStr(Cardinal1);
          msInfo3.Caption  := 'Max Attempts';
          msValue3.Caption := IntToStr(Cardinal2);
          msInfo4.Caption  := 'Time Remaining';
          msValue4.Caption := IntToStr(TimeRemaining);
        end;
      smsDialCycle :
        begin
          msInfo1.Caption  := 'Next Number To Dial';
          msValue1.Caption := String1;
          msInfo2.Caption  := 'Next Dial Attempt';
          msValue2.Caption := IntToStr(succ(Cardinal1));
          msInfo3.Caption  := 'Max Attempts';
          msValue3.Caption := IntToStr(Cardinal2);
          msInfo4.Caption  := 'Time Remaining';
          msValue4.Caption := IntToStr(TimeRemaining);
        end;
      smsConnected :
        begin
        end;
    end;

    {Update the scrolling message portion of the display (the memo)}
    with msMessages do begin
      while (msiSendRecvData.Count > 0) do begin
        if msdCurrentRow = pred(Lines.Count) then begin
          {Scroll off the least recently used message}
          for i := 1 to pred(Lines.Count) do
             Lines[pred(i)] := Lines[i];
        end else
          Inc(msdCurrentRow);

        Lines[msdCurrentRow] := msiSendRecvData.Strings[0];
        msiSendRecvData.Delete(0);
      end;
    end;

    case GetModemState of
      smsInitialize,
      smsHangup,
      smsCancel :
        {Force a repaint when we know the next code executed doesn't yield}
        Repaint;
    end;
  end;
end;

{- Modem status class -------------------------------------}

constructor TApdSModemStatusInfo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  msiSendRecvData := TStringList.Create;
  msiSendRecvDataCopy := TStringList.Create;
end;

destructor TApdSModemStatusInfo.Destroy;
begin
  inherited Destroy;

  msiSendRecvData.Free;
  msiSendRecvDataCopy.Free;
end;

procedure TApdSModemStatusInfo.AddSendRecvData(Msg : String);
  {Add the message to the Send/Receive string list}
begin
  msiSendRecvData.Add(Msg);
end;

procedure TApdSModemStatusInfo.SetInfoAnswerWait(TimeRemaining : Cardinal);
  {Set the specific parameters describing the Answer progress}
begin
  msiModemStateData.TimeRemaining := TimeRemaining;
end;

procedure TApdSModemStatusInfo.SetInfoAutoAnswerWait(RingToAnswer : Byte;
                                                     RingsReceived : Byte);
  {Set the specific parameters describing the AutoAnswer progress}
begin
  msiModemStateData.Byte1 := RingToAnswer;
  msiModemStateData.Byte2 := RingsReceived;
end;

procedure TApdSModemStatusInfo.SetInfoConnected(ErrorCorrection : String;
                                                DataCompression : String;
                                                ConnectSpeed : Cardinal);
  {Set the specific parameters describing the Connection progress}
begin
  msiModemStateData.String1 := ErrorCorrection;
  msiModemStateData.String2 := DataCompression;
  msiModemStateData.Cardinal1 := ConnectSpeed;
end;

procedure TApdSModemStatusInfo.SetInfoDialCycle(NextPhoneNumber : String;
                                                NextAttemptNumber : Cardinal;
                                                MaxAttempts : Cardinal;
                                                TimeRemaining : Cardinal);
  {Set the specific parameters describing the Dial Retry/Cycle progress}
begin
  msiModemStateData.String1 := NextPhoneNumber;
  msiModemStateData.Cardinal1 := NextAttemptNumber;
  msiModemStateData.Cardinal2 := MaxAttempts;
  msiModemStateData.TimeRemaining := TimeRemaining;
end;

procedure TApdSModemStatusInfo.SetInfoDialWait(PhoneNumber : String;
                                               AttemptNumber : Cardinal;
                                               MaxAttempts : Cardinal;
                                               TimeRemaining : Cardinal);
  {Set the specific parameters describing the Dial progress}
begin
  msiModemStateData.String1 := PhoneNumber;
  msiModemStateData.Cardinal1 := AttemptNumber;
  msiModemStateData.Cardinal2 := MaxAttempts;
  msiModemStateData.TimeRemaining := TimeRemaining;
end;

function TApdSModemStatusInfo.GetModemState : TApdSModemStates;
  {Get the current state of the modem component (e.g., smsDialWait)}
begin
  Result := msiModemState;
end;

function TApdSModemStatusInfo.GetModemStateMsg : String;
  {Get the description of the current modem state (e.g., 'Dialing')}
begin
  Result := AproLoadStr(smMsgBase + ord(msiModemState));              
end;

function TApdSModemStatusInfo.GetSendRecvMessages : TStringList;
  {Get a list of the available send/receive messages}
begin
  msiSendRecvDataCopy.Assign(msiSendRecvData);
  Result := msiSendRecvDataCopy;
end;

procedure TApdSModemStatusInfo.GetInfoAnswerWait(var TimeRemaining : Cardinal);
  {Get the specific status info when GetModemState = smsAnswerWait}
begin
  if msiModemState <> smsAnswerWait then
    raise EModemStatusMismatch.Create(ecModemStatusMismatch, False);

  TimeRemaining := msiModemStateData.TimeRemaining;
end;

procedure TApdSModemStatusInfo.GetInfoAutoAnswerWait(var RingToAnswer : Byte;
                                                     var RingsReceived : Byte);
  {Get the specific status info when GetModemState = smsAutoAnswerWait}
begin
  if msiModemState <> smsAutoAnswerWait then                         
    raise EModemStatusMismatch.Create(ecModemStatusMismatch, False);

  RingToAnswer := msiModemStateData.Byte1;
  RingsReceived := msiModemStateData.Byte2;
end;

procedure TApdSModemStatusInfo.GetInfoConnected(var ErrorCorrection : String;
                                                var DataCompression : String;
                                                var ConnectSpeed : Cardinal);
  {Get the specific status info when GetModemState = smsConnectWait}
begin
  if msiModemState <> smsConnected then
    raise EModemStatusMismatch.Create(ecModemStatusMismatch, False);

  ErrorCorrection := msiModemStateData.String1;
  DataCompression := msiModemStateData.String2;
  ConnectSpeed := msiModemStateData.Cardinal1;
end;

procedure TApdSModemStatusInfo.GetInfoDialCycle(var NextPhoneNumber : String;
                                                var NextAttemptNumber : Cardinal;
                                                var MaxAttempts : Cardinal;
                                                var TimeRemaining : Cardinal);
  {Get the specific status info when GetModemState = smsDialCycle}
begin
  if msiModemState <> smsDialCycle then
    raise EModemStatusMismatch.Create(ecModemStatusMismatch, False);

  NextPhoneNumber := msiModemStateData.String1;
  NextAttemptNumber := msiModemStateData.Cardinal1;
  MaxAttempts := msiModemStateData.Cardinal2;
  TimeRemaining := msiModemStateData.TimeRemaining;
end;

procedure TApdSModemStatusInfo.GetInfoDialWait(var PhoneNumber : String;
                                               var AttemptNumber : Cardinal;
                                               var MaxAttempts : Cardinal;
                                               var TimeRemaining : Cardinal);
  {Get the specific status info when GetModemState = smsDialWait}
begin
  if msiModemState <> smsDialWait then
    raise EModemStatusMismatch.Create(ecModemStatusMismatch, False);

  PhoneNumber := msiModemStateData.String1;
  AttemptNumber := msiModemStateData.Cardinal1;
  MaxAttempts := msiModemStateData.Cardinal2;
  TimeRemaining := msiModemStateData.TimeRemaining;
end;

{----------------------------------------------------------}

end.
