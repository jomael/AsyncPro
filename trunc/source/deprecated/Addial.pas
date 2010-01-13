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
{*                    ADDIAL.PAS 4.06                    *}
{*********************************************************}
{* Deprecated modem dialer dialog                        *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdDial;
  {-Dialer window}

interface

uses
  SysUtils,
  WinTypes,
  WinProcs,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Buttons,
  OoMisc,
  AwModem,
  AdExcept,
  AdPort,
  AdModem;

type
  {TApdModemDialer options}
  TDialerOption  = (mdAbortOnVoice, mdAbortOnBusy, mdAbortOnNoCarrier,
                    mdAbortOnNoDialtone, mdAbortOnError);
  TDialerOptions = Set of TDialerOption;

  TDialResult = (drNone, drDialing, drConnected, drAborted);

const
  {Prompt/label base used to translate ddXxx values to string IDs}
  DialerDialogBase  = 4300;

  ddCycling         = 4301;   {dialer is cycling}
  ddRetryWaiting    = 4302;   {waiting to retry}
  ddRetryWaitOver   = 4303;   {wait for retry over, about to dial}
  ddDialing         = 4304;   {dialing and counting down}
  ddModemConnect    = 4305;   {connection established}
  ddModemConnectAt  = 4306;   {connection established at known baud rate}
  ddModemVoice      = 4307;   {remote answered with voice}
  ddModemError      = 4308;   {modem returned an ERROR}
  ddModemNoCarrier  = 4309;   {modem returned NO CARRIER}
  ddModemBusy       = 4310;   {modem received busy signal}
  ddModemNoDialTone = 4311;   {modem did not find dial tone}
  ddDialTimedOut    = 4312;   {dial attempt timed out}

  {TApdModemDialer settings}
  addDefDialerOptions = [mdAbortOnNoDialtone, mdAbortOnError];

  {TApdDialerDialog settings}
  NumStatusLines  = 13;
  addDefMaxDialTries = 10;
  addDefRetrySecs    = 60;

type
  {component for dialing phone numbers}
  TApdCustomModemDialer = class(TApdBaseComponent)
  protected {private}
    {.Z+}
    FModem                 : TApdCustomModem;    {modem to use for dialing}
    FPhoneNumber           : String;             {phone number to dial}
    FMaxDialTries          : Integer;            {max # of dial attempts}
    FRetrySecs             : Integer;            {seconds between dial retries}
    FDialerOptions         : TDialerOptions;     {options for dialing}
    FDialResult            : TDialResult;        {result of dial attempt}
    FDialing               : Boolean;            {TRUE if dialer active}
    FRetryStart            : TNotifyEvent;       {before dial retry starts}
    FRetryCount            : TConnectCountEvent; {when retry counter ticks}
    FRetryEnd              : TNotifyEvent;       {when dial retry is finished}
    FCycleDial             : TNotifyEvent;       {when dial attempt cycled}
    FDialStart             : TNotifyEvent;       {at beginning of dial}
    FConnect               : TNotifyEvent;       {when connection established}
    FDialCount             : TConnectCountEvent; {when dial timer ticks}
    FGotLineSpeed          : TLineSpeedEvent;    {when connect speed detrmnd}
    FBusy                  : TNotifyEvent;       {when modem returns busy}
    FVoice                 : TNotifyEvent;       {when modem returns voice}
    FError                 : TNotifyEvent;       {when modem returns error}
    FNoCarrier             : TNotifyEvent;       {when modem returns ncarrier}
    FNoDialTone            : TNotifyEvent;       {when modem returns ndialtn}
    FDialTimedOut          : TNotifyEvent;       {when dial attempt times out}
    FConnectionEstablished : TNotifyEvent;       {when connection is established}
    FTooManyTries          : TNotifyEvent;       {when too many dials tried}
    FRetrying              : Boolean;            {TRUE if retrying dial attempt}

    {dialing data}
    ComPort                : TApdCustomComPort;  {communications port}
    DialTry                : Integer;            {current dial try}
    DialIdx                : Integer;            {index of dial start timer trig}

    {retry data}
    Cycling                : Boolean;            {TRUE if cycling dial attempt}
    RetryIdx               : Integer;            {index of retry count trigger}
    RetryWait              : Integer;            {# secs left until retry}

    {saved event handlers}
    SaveTriggerTimer       : TTriggerTimerEvent;
    SaveModemConnect       : TNotifyEvent;
    SaveDialCount          : TConnectCountEvent;
    SaveGotLineSpeed       : TLineSpeedEvent;
    SaveModemBusy          : TNotifyEvent;
    SaveModemVoice         : TNotifyEvent;
    SaveModemError         : TNotifyEvent;
    SaveModemNoCarrier     : TNotifyEvent;
    SaveModemNoDialTone    : TNotifyEvent;
    SaveDialTimedOut       : TNotifyEvent;
    SaveOnByeBye           : TNotifyEvent;
    SaveModemIsConnected   : TNotifyEvent;

    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    procedure PrepareDialing;
      {-Start a timer to trigger the beginning of a dial attempt}
    procedure RetryDial;
      {-Retry the last dial attempt, if possible}
    procedure RetryDone;
      {-Cleanup after a retry countdown}

    {dialer event handlers}
    procedure DialerTriggerTimer(Sender : TObject; TriggerHandle : Word);
    procedure DialerModemConnect(Sender : TObject);
    procedure DialerDialCount(Sender : TObject; Remaining : Word);
    procedure DialerGotLineSpeed(Sender : TObject; Speed : LongInt);
    procedure DialerModemBusy(Sender : TObject);
    procedure DialerModemVoice(Sender : TObject);
    procedure DialerModemError(Sender : TObject);
    procedure DialerModemNoCarrier(Sender : TObject);
    procedure DialerModemNoDialTone(Sender : TObject);
    procedure DialerDialTimedOut(Sender : TObject);
    procedure DialerModemByeBye(Sender : TObject);
    procedure DialerModemIsConnected(Sender : TObject);

    procedure RetryStart; virtual;
    procedure RetryCount; virtual;
    procedure RetryEnd; virtual;
    procedure CycleDial; virtual;
    procedure DialStart; virtual;
    procedure GotConnect; virtual;
    procedure DialCount(Cnt : Cardinal); virtual;
    procedure GotLineSpeed(Speed : LongInt); virtual;
    procedure GotBusy; virtual;
    procedure GotVoice; virtual;
    procedure GotError; virtual;
    procedure GotNoCarrier; virtual;
    procedure GotNoDialTone; virtual;
    procedure DialTimedOut; virtual;
    procedure ConnectionEstablished; virtual;
    procedure TooManyTries; virtual;

    procedure SinkHooks;
      {-Replace existing ComPort/Modem event handlers with dialer handlers}
    procedure RemoveHooks;
      {-Remove dialer event handlers and replace with originals}
    procedure DialingFinished;
      {-Dialing done: shut down dial attempt}
    procedure ShutDownOnDestroy;
      {-Clean up during the middle of a dial attempt}

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    {.Z-}

    {dialing data}
    property Modem : TApdCustomModem
      read FModem write FModem;
    property PhoneNumber : String
      read FPhoneNumber write FPhoneNumber;
    property MaxDialTries : Integer
      read FMaxDialTries write FMaxDialTries default addDefMaxDialTries;
    property RetrySecs : Integer
      read FRetrySecs write FRetrySecs default addDefRetrySecs;
    property DialerOptions : TDialerOptions
      read FDialerOptions write FDialerOptions;
    property DialResult : TDialResult
      read FDialResult;
    property Dialing : Boolean
      read FDialing;
    property Retrying : Boolean
      read FRetrying;

    {events}
    property OnRetryStart : TNotifyEvent
      read FRetryStart write FRetryStart;
    property OnRetryCount : TConnectCountEvent
      read FRetryCount write FRetryCount;
    property OnRetryEnd : TNotifyEvent
      read FRetryEnd write FRetryEnd;
    property OnCycleDial : TNotifyEvent
      read FCycleDial write FCycleDial;
    property OnDialStart : TNotifyEvent
      read FDialStart write FDialStart;
    property OnConnect : TNotifyEvent
      read FConnect write FConnect;
    property OnDialCount : TConnectCountEvent
      read FDialCount write FDialCount;
    property OnGotLineSpeed : TLineSpeedEvent
      read FGotLineSpeed write FGotLineSpeed;
    property OnBusy : TNotifyEvent
      read FBusy write FBusy;
    property OnVoice : TNotifyEvent
      read FVoice write FVoice;
    property OnError : TNotifyEvent
      read FError write FError;
    property OnNoCarrier : TNotifyEvent
      read FNoCarrier write FNoCarrier;
    property OnNoDialTone : TNotifyEvent
      read FNoDialTone write FNoDialTone;
    property OnDialTimedOut : TNotifyEvent
      read FDialTimedOut write FDialTimedOut;
    property OnConnectionEstablished : TNotifyEvent
      read FConnectionEstablished write FConnectionEstablished;
    property OnTooManyTries : TNotifyEvent
      read FTooManyTries write FTooManyTries;

    {methods}
    procedure Dial;
    procedure Cycle;
    procedure Extend(const ByHowMuch : Cardinal);
    procedure Abort;
  end;

  TApdModemDialer = class(TApdCustomModemDialer)
    {dialing data}
    property Modem;
    property PhoneNumber;
    property MaxDialTries;
    property RetrySecs;
    property DialerOptions;

    {events}
    property OnRetryStart;
    property OnRetryCount;
    property OnRetryEnd;
    property OnCycleDial;
    property OnDialStart;
    property OnConnect;
    property OnDialCount;
    property OnGotLineSpeed;
    property OnBusy;
    property OnVoice;
    property OnError;
    property OnNoCarrier;
    property OnNoDialTone;
    property OnConnectionEstablished;
    property OnTooManyTries;
  end;

  {.Z+}
  TDialerForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    CycleBtn: TBitBtn;
    ExtendBtn: TBitBtn;
    AbortBtn: TBitBtn;
    procedure CycleBtnClick(Sender: TObject);
    procedure ExtendBtnClick(Sender: TObject);
    procedure AbortBtnClick(Sender: TObject);

  public
    Labels   : array[1..NumStatusLines] of TLabel;
    OnLabel  : Cardinal;
    Dialer   : TApdCustomModemDialer;
    SaveTrig : TTriggerTimerEvent;
    DialTrig : Integer;

    constructor Create(AOwner : TComponent); override;

    procedure AddStatusString(const St : String);
      {-Put a status string in the display}
    procedure AddResString(const ResID : Cardinal);
      {-Load a status string from a resource and display it}
    procedure DoDial(const ADialer : TApdCustomModemDialer);
      {-Begin dialing}

    {dialer event handlers}
    procedure FrmRetryStart(Sender : TObject);
    procedure FrmRetryCount(M : TObject; Remaining : Word);
    procedure FrmRetryEnd(Sender : TObject);
    procedure FrmDialStart(Sender : TObject);
    procedure FrmConnect(Sender : TObject);
    procedure FrmDialCount(M : TObject; Remaining : Word);
    procedure FrmGotLineSpeed(M : TObject; Speed : LongInt);
    procedure FrmBusy(Sender : TObject);
    procedure FrmVoice(Sender : TObject);
    procedure FrmError(Sender : TObject);
    procedure FrmNoCarrier(Sender : TObject);
    procedure FrmNoDialTone(Sender : TObject);
    procedure FrmDialTimedOut(Sender : TObject);
    procedure FrmTooManyTries(Sender : TObject);
    procedure FrmConnectionEstablished(Sender : TObject);
    procedure FrmTriggerTimer(CP : TObject; TriggerHandle : Word);
  end;
  {.Z-}

  TApdDialerDialog = class(TCommonDialog)
  protected {private}
    {.Z+}
    FModem         : TApdCustomModem;
    FPhoneNumber   : String;
    FMaxDialTries  : Integer;
    FRetrySecs     : Integer;
    FDialerOptions : TDialerOptions;
    FCaption       : TCaption;

    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    function GetVersion : string;
    procedure SetVersion(const Value : string);                 

  public
    constructor Create(AOwner : TComponent); override;
    {.Z-}

    function Execute : Boolean; {$IFDEF VERSION3} override; {$ENDIF}  

  published
    property Version : string                                   
      read GetVersion
      write SetVersion
      stored False;
    {dialing data}
    property Modem : TApdCustomModem
      read FModem write FModem;
    property PhoneNumber : String
      read FPhoneNumber write FPhoneNumber;
    property MaxDialTries : Integer
      read FMaxDialTries write FMaxDialTries default addDefMaxDialTries;
    property RetrySecs : Integer
      read FRetrySecs write FRetrySecs default addDefRetrySecs;
    property DialerOptions : TDialerOptions
      read FDialerOptions write FDialerOptions;
    property Caption : TCaption
      read FCaption write FCaption;                                     
  end;

implementation

{$R *.DFM}

{TApdCustomModemDialer}

  procedure TApdCustomModemDialer.Notification(AComponent : TComponent; Operation : TOperation);
  begin
    inherited Notification(AComponent, Operation);

    if (Operation = opRemove) then begin
      {see if our modem is going away}
      if (AComponent = FModem) then begin
        FModem  := nil;
        ComPort := nil;
      end;
    end else if (Operation = opInsert) then begin
      {check for modem being installed}
      if not Assigned(FModem) and (AComponent is TApdCustomModem) then
        Modem := TApdCustomModem(AComponent);
    end;
  end;

  procedure TApdCustomModemDialer.PrepareDialing;
    {-Start a timer to trigger the beginning of a dial attempt}
  begin
    DialIdx := ComPort.AddTimerTrigger;
    ComPort.SetTimerTrigger(DialIdx, 4, True);
  end;

  procedure TApdCustomModemDialer.RetryDial;
    {-Retry the last dial attempt, if possible}
  begin
    Inc(DialTry);
    if (DialTry >= MaxDialTries) then begin
      DialingFinished;
      FDialResult := drAborted;
      TooManyTries;
      Exit;
    end;

    {let user know we're starting a retry}
    RetryStart;

    {start the countdown}
    RetryWait := RetrySecs;
    RetryIdx  := ComPort.AddTimerTrigger;
    ComPort.SetTimerTrigger(RetryIdx, TickSeconds, True);
    FRetrying := True;
  end;

  procedure TApdCustomModemDialer.RetryDone;
    {-Cleanup after a retry countdown}
  begin
    ComPort.RemoveTrigger(RetryIdx);
    RetryIdx := 0;

    {let the user know the retry wait is over}
    RetryEnd;

    {start another dial attempt}
    PrepareDialing;
    FRetrying := False;
  end;

  procedure TApdCustomModemDialer.DialerTriggerTimer(Sender : TObject; TriggerHandle : Word);
  begin
    if (TriggerHandle = RetryIdx) then begin
      if Retrying then begin
        Dec(RetryWait);
        if (RetryWait = 0) then
          RetryDone
        else begin
          {tell user to update retry count display}
          RetryCount;

          {reset the countdown timer}
          ComPort.SetTimerTrigger(RetryIdx, TickSeconds, True);
        end;
      end;
    end else if (TriggerHandle = DialIdx) then begin
      {start dialing}
      ComPort.RemoveTrigger(DialIdx);
      DialIdx := 0;
      Modem.Dial(PhoneNumber);

      {let the user know}
      DialStart;
    end;

    if Assigned(SaveTriggerTimer) then
      SaveTriggerTimer(Sender, TriggerHandle);
  end;

  procedure TApdCustomModemDialer.DialerModemConnect(Sender : TObject);
  begin
    GotConnect;
    if Assigned(SaveModemConnect) then
      SaveModemConnect(Sender);
  end;

  procedure TApdCustomModemDialer.DialerDialCount(Sender : TObject; Remaining : Word);
  begin
    DialCount(Remaining);
    if Assigned(SaveDialCount) then
      SaveDialCount(Sender, Remaining);
  end;

  procedure TApdCustomModemDialer.DialerGotLineSpeed(Sender : TObject; Speed : LongInt);
  begin
    GotLineSpeed(Speed);
    if Assigned(SaveGotLineSpeed) then
      SaveGotLineSpeed(Sender, Speed);
  end;

  procedure TApdCustomModemDialer.DialerModemBusy(Sender : TObject);
  begin
    if Assigned(SaveModemBusy) then
      SaveModemBusy(Sender);

    if mdAbortOnBusy in DialerOptions then begin
      FDialResult := drAborted;
      GotBusy;
      DialingFinished;
    end else begin
      GotBusy;
      RetryDial;
    end;
  end;

  procedure TApdCustomModemDialer.DialerModemVoice(Sender : TObject);
  begin
    if Assigned(SaveModemVoice) then
      SaveModemVoice(Sender);

    if mdAbortOnVoice in DialerOptions then begin
      FDialResult := drAborted;
      GotVoice;
      DialingFinished;
    end else begin
      GotVoice;
      RetryDial;
    end;
  end;

  procedure TApdCustomModemDialer.DialerModemError(Sender : TObject);
  begin
    if Assigned(SaveModemError) then
      SaveModemError(Sender);

    if mdAbortOnError in DialerOptions then begin
      FDialResult := drAborted;
      GotError;
      DialingFinished;
    end else begin
      GotError;
      RetryDial;
    end;
  end;

  procedure TApdCustomModemDialer.DialerModemNoCarrier(Sender : TObject);
  begin
    if Assigned(SaveModemNoCarrier) then
      SaveModemNoCarrier(Sender);

    if mdAbortOnNoCarrier in DialerOptions then begin
      FDialResult := drAborted;
      GotNoCarrier;
      DialingFinished;
    end else begin
      GotNoCarrier;
      RetryDial;
    end;
  end;

  procedure TApdCustomModemDialer.DialerModemNoDialTone(Sender : TObject);
  begin
    if Assigned(SaveModemNoDialTone) then
      SaveModemNoDialTone(Sender);

    if mdAbortOnNoDialTone in DialerOptions then begin
      FDialResult := drAborted;
      GotNoDialTone;
      DialingFinished;
    end else begin
      GotNoDialTone;
      RetryDial;
    end;
  end;

  procedure TApdCustomModemDialer.DialerDialTimedOut(Sender : TObject);
  begin
    if Assigned(SaveDialTimedOut) then
      SaveDialTimedOut(Sender);

    DialTimedOut;
    RetryDial;
  end;

  procedure TApdCustomModemDialer.DialerModemByeBye(Sender : TObject);
  begin
    ShutDownOnDestroy;

    if Assigned(SaveOnByeBye) then
      SaveOnByeBye(Sender);
  end;

  procedure TApdCustomModemDialer.DialerModemIsConnected(Sender : TObject);
  begin
    if Assigned(SaveModemIsConnected) then
      SaveModemIsConnected(Sender);

    {all done!}
    FDialResult := drConnected;
    DialingFinished;

    ConnectionEstablished;
  end;

  procedure TApdCustomModemDialer.RetryStart;
  begin
    if Assigned(FRetryStart) then
      FRetryStart(Self);
  end;

  procedure TApdCustomModemDialer.RetryCount;
  begin
    if Assigned(FRetryCount) then
      FRetryCount(Self, RetryWait);
  end;

  procedure TApdCustomModemDialer.RetryEnd;
  begin
    if Assigned(FRetryEnd) then
      FRetryEnd(Self);
  end;

  procedure TApdCustomModemDialer.CycleDial;
  begin
    if Assigned(FCycleDial) then
      FCycleDial(Self);
  end;

  procedure TApdCustomModemDialer.DialStart;
  begin
    if Assigned(FDialStart) then
      FDialStart(Self);
  end;

  procedure TApdCustomModemDialer.GotConnect;
  begin
    if Assigned(FConnect) then
      FConnect(Self);
  end;

  procedure TApdCustomModemDialer.DialCount(Cnt : Cardinal);
  begin
    if Assigned(FDialCount) then
      FDialCount(Self, Cnt);
  end;

  procedure TApdCustomModemDialer.GotLineSpeed;
  begin
    if Assigned(FGotLineSpeed) then
      FGotLineSpeed(Self, Speed);
  end;

  procedure TApdCustomModemDialer.GotBusy;
  begin
    if Assigned(FBusy) then
      FBusy(Self);
  end;

  procedure TApdCustomModemDialer.GotVoice;
  begin
    if Assigned(FVoice) then
      FVoice(Self);
  end;

  procedure TApdCustomModemDialer.GotError;
  begin
    if Assigned(FError) then
      FError(Self);
  end;

  procedure TApdCustomModemDialer.GotNoCarrier;
  begin
    if Assigned(FNoCarrier) then
      FNoCarrier(Self);
  end;

  procedure TApdCustomModemDialer.GotNoDialTone;
  begin
    if Assigned(FNoDialTone) then
      FNoDialTone(Self);
  end;

  procedure TApdCustomModemDialer.DialTimedOut;
  begin
    if Assigned(FDialTimedOut) then
      FDialTimedOut(Self);
  end;

  procedure TApdCustomModemDialer.ConnectionEstablished;
  begin
    if Assigned(FConnectionEstablished) then
      FConnectionEstablished(Self);
  end;

  procedure TApdCustomModemDialer.TooManyTries;
  begin
    if Assigned(FTooManyTries) then
      FTooManyTries(Self);
  end;

  procedure TApdCustomModemDialer.SinkHooks;
    {-Replace existing ComPort/Modem event handlers with dialer handlers}
  begin
    {save old event handlers}
    SaveTriggerTimer     := ComPort.OnTriggerTimer;
    SaveModemConnect     := Modem.OnModemConnect;
    SaveDialCount        := Modem.OnDialCount;
    SaveGotLineSpeed     := Modem.OnGotLineSpeed;
    SaveModemBusy        := Modem.OnModemBusy;
    SaveModemVoice       := Modem.OnModemVoice;
    SaveModemError       := Modem.OnModemError;
    SaveModemNoCarrier   := Modem.OnModemNoCarrier;
    SaveModemNoDialTone  := Modem.OnModemNoDialTone;
    SaveDialTimedOut     := Modem.OnDialTimedOut;
    SaveOnByeBye         := Modem.OnByeBye;
    SaveModemIsConnected := Modem.OnModemIsConnected;

    {install our event handlers}
    ComPort.OnTriggerTimer   := DialerTriggerTimer;
    Modem.OnModemConnect     := DialerModemConnect;
    Modem.OnDialCount        := DialerDialCount;
    Modem.OnGotLineSpeed     := DialerGotLineSpeed;
    Modem.OnModemBusy        := DialerModemBusy;
    Modem.OnModemVoice       := DialerModemVoice;
    Modem.OnModemError       := DialerModemError;
    Modem.OnModemNoCarrier   := DialerModemNoCarrier;
    Modem.OnModemNoDialTone  := DialerModemNoDialTone;
    Modem.OnDialTimedOut     := DialerDialTimedOut;
    Modem.OnByeBye           := DialerModemByeBye;
    Modem.OnModemIsConnected := DialerModemIsConnected;
  end;

  procedure TApdCustomModemDialer.RemoveHooks;
    {-Remove dialer event handlers and replace with originals}
  begin
    Modem.OnModemConnect     := SaveModemConnect;
    Modem.OnDialCount        := SaveDialCount;
    Modem.OnGotLineSpeed     := SaveGotLineSpeed;
    Modem.OnModemBusy        := SaveModemBusy;
    Modem.OnModemVoice       := SaveModemVoice;
    Modem.OnModemError       := SaveModemError;
    Modem.OnModemNoCarrier   := SaveModemNoCarrier;
    Modem.OnModemNoDialTone  := SaveModemNoDialTone;
    Modem.OnDialTimedOut     := SaveDialTimedOut;
    Modem.OnByeBye           := SaveOnByeBye;
    Modem.OnModemIsConnected := SaveModemIsConnected;
    ComPort.OnTriggerTimer   := SaveTriggerTimer;
  end;

  procedure TApdCustomModemDialer.DialingFinished;
    {-Dialing done: shut down dial attempt}
  begin
    FDialing  := False;
    FRetrying := False;
    RemoveHooks;
  end;

  procedure TApdCustomModemDialer.ShutDownOnDestroy;
    {-Clean up during the middle of a dial attempt}
  begin
    if Dialing and Assigned(FModem) and (Modem.ComPort <> nil) then begin
      if Retrying then
        ComPort.RemoveTrigger(RetryIdx)
      else if Modem.IsAttemptingConnect then
        Modem.CancelDialAnswer;

      DialingFinished;
    end;
  end;

  constructor TApdCustomModemDialer.Create(AOwner : TComponent);
  var
    I : Cardinal;
  begin
    inherited Create(AOwner);

    {set default property values}
    FModem                 := nil;
    FPhoneNumber           := '';
    FMaxDialTries          := addDefMaxDialTries;
    FRetrySecs             := addDefRetrySecs ;
    FDialerOptions         := addDefDialerOptions;
    FDialResult            := drNone;
    FDialing               := False;
    FRetrying              := False;
    FRetryStart            := nil;
    FRetryCount            := nil;
    FRetryEnd              := nil;
    FCycleDial             := nil;
    FDialStart             := nil;
    FConnect               := nil;
    FDialCount             := nil;
    FGotLineSpeed          := nil;
    FBusy                  := nil;
    FVoice                 := nil;
    FError                 := nil;
    FNoCarrier             := nil;
    FNoDialTone            := nil;
    FDialTimedOut          := nil;
    FConnectionEstablished := nil;
    FTooManyTries          := nil;

    ComPort                := nil;
    DialTry                := 0;
    DialIdx                := 0;

    Cycling                := False;
    RetryIdx               := 0;
    RetryWait              := 0;

    SaveTriggerTimer       := nil;
    SaveModemConnect       := nil;
    SaveDialCount          := nil;
    SaveGotLineSpeed       := nil;
    SaveModemBusy          := nil;
    SaveModemVoice         := nil;
    SaveModemError         := nil;
    SaveModemNoCarrier     := nil;
    SaveModemNoDialTone    := nil;
    SaveDialTimedOut       := nil;

    {search our owner for a modem}
    if Assigned(AOwner) and (AOwner.ComponentCount > 0) then
      for I := 0 to Pred(AOwner.ComponentCount) do
        if AOwner.Components[I] is TApdCustomModem then begin
          FModem := TApdCustomModem(AOwner.Components[I]);
          Break;
        end;
  end;

  destructor TApdCustomModemDialer.Destroy;
  begin
    ShutDownOnDestroy;

    inherited Destroy;
  end;

  procedure TApdCustomModemDialer.Dial;
  begin
    if Dialing then
      raise EAlreadyDialing.Create(ecAlreadyDialing, False);
    if not Assigned(FModem) then
      raise EModemNotAssigned.Create(ecModemNotAssigned, False);
    if (Modem.ComPort = nil) then
      raise EPortNotAssigned.Create(ecPortNotAssigned, False);

    ComPort := Modem.ComPort;

    {let the user know what we're up to...}
    FDialResult := drDialing;
    SinkHooks;

    DialTry  := 0;
    FDialing := True;
    PrepareDialing;
  end;

  procedure TApdCustomModemDialer.Cycle;
  begin
    if not Dialing then
      raise ENotDialing.Create(ecNotDialing, False);

    {prevent reentrancy}
    if Cycling then
      Exit;
    Cycling := True;

    {if we're retrying, stop}
    if Retrying then begin
      RetryDone;
      Cycling := False;
    end else begin
      {cancel the dial attempt in progress}
      Modem.CancelDialAnswer;

      Inc(DialTry);
      if (DialTry > MaxDialTries) then begin
        FDialResult := drAborted;
        DialingFinished;
        TooManyTries;
        Exit;
      end;

      {let the user know what we're up to}
      CycleDial;

      {delay a bit so as to avoid freaking the modem out}
      DelayTicks(4, False);

      PrepareDialing;
      Cycling := False;
    end;
  end;

  procedure TApdCustomModemDialer.Extend(const ByHowMuch : Cardinal);
  begin
    if not Dialing then
      raise ENotDialing.Create(ecNotDialing, False);

    try
      Modem.ExtendConnectAttempt(ByHowMuch);
    except
      on EModemNotDialing do begin
      end else
        raise;
    end;
  end;

  procedure TApdCustomModemDialer.Abort;
  begin
    if not Dialing then
      raise ENotDialing.Create(ecNotDialing, False);

    if Modem.IsAttemptingConnect then begin
      Modem.CancelDialAnswer;
      FDialResult := drAborted;
    end else if Retrying then begin
      ComPort.RemoveTrigger(RetryIdx);
      RetryIdx := 0;
    end;                                                          

    DialingFinished;
  end;

{TDialerForm}

  constructor TDialerForm.Create(AOwner : TComponent);
  begin
    inherited Create(AOwner);

    Labels[1]  := Label1;
    Labels[2]  := Label2;
    Labels[3]  := Label3;
    Labels[4]  := Label4;
    Labels[5]  := Label5;
    Labels[6]  := Label6;
    Labels[7]  := Label7;
    Labels[8]  := Label8;
    Labels[9]  := Label9;
    Labels[10] := Label10;
    Labels[11] := Label11;
    Labels[12] := Label12;
    Labels[13] := Label13;

    SaveTrig := nil;
    DialTrig := 0;
    Dialer   := nil;
  end;

  procedure TDialerForm.CycleBtnClick(Sender: TObject);
  begin
    if not Dialer.Retrying then
      AddResString(ddCycling);

    try
      Dialer.Cycle;
    finally
    end;
  end;

  procedure TDialerForm.ExtendBtnClick(Sender: TObject);
  begin
    try
      Dialer.Extend(5);
    finally
    end;
  end;

  procedure TDialerForm.AbortBtnClick(Sender: TObject);
  begin
    try
      Dialer.Abort;
    finally
      ModalResult := mrAbort;
    end;
  end;

  procedure TDialerForm.AddStatusString(const St : String);
    {-Put a status string in the display}
  var
    I : Integer;

  begin
    Inc(OnLabel);
    if (OnLabel > NumStatusLines) then begin
      for I := 2 to NumStatusLines do
        Labels[I-1].Caption := Labels[I].Caption;
      Dec(OnLabel);
    end;

    Labels[OnLabel].Caption := St;
  end;

  procedure TDialerForm.AddResString(const ResID : Cardinal);
    {-Load a status string from a resource and display it}
  begin
    AddStatusString(AproLoadStr(ResID)); 
  end;

  procedure TDialerForm.DoDial(const ADialer : TApdCustomModemDialer);
    {-Begin dialing}
  var
    I : Cardinal;

  begin
    OnLabel := 0;
    for I := 1 to NumStatusLines do
      Labels[I].Caption := '';

    Dialer := ADialer;

    {set up a timer trigger to start the dial}
    SaveTrig := Dialer.Modem.ComPort.OnTriggerTimer;
    DialTrig := Dialer.Modem.ComPort.AddTimerTrigger;
    Dialer.Modem.ComPort.OnTriggerTimer := FrmTriggerTimer;
    Dialer.Modem.ComPort.SetTimerTrigger(DialTrig, 4, True);

    with Dialer do begin
      OnRetryStart            := FrmRetryStart;
      OnRetryCount            := FrmRetryCount;
      OnRetryEnd              := FrmRetryEnd;
      OnDialStart             := FrmDialStart;
      OnConnect               := FrmConnect;
      OnDialCount             := FrmDialCount;
      OnGotLineSpeed          := FrmGotLineSpeed;
      OnBusy                  := FrmBusy;
      OnVoice                 := FrmVoice;
      OnError                 := FrmError;
      OnNoCarrier             := FrmNoCarrier;
      OnNoDialTone            := FrmNoDialTone;
      OnDialTimedOut          := FrmDialTimedOut;
      OnTooManyTries          := FrmTooManyTries;
      OnConnectionEstablished := FrmConnectionEstablished;

      ShowModal;

      OnRetryStart            := nil;
      OnRetryCount            := nil;
      OnRetryEnd              := nil;
      OnDialStart             := nil;
      OnConnect               := nil;
      OnDialCount             := nil;
      OnGotLineSpeed          := nil;
      OnBusy                  := nil;
      OnVoice                 := nil;
      OnError                 := nil;
      OnNoCarrier             := nil;
      OnNoDialTone            := nil;
      OnConnectionEstablished := nil;
      OnTooManyTries          := nil;
    end;
  end;

  procedure TDialerForm.FrmRetryStart(Sender : TObject);
  begin
    AddStatusString(AproLoadStr(ddRetryWaiting) + ' ' +
      IntToStr(Dialer.RetrySecs));
  end;

  procedure TDialerForm.FrmRetryCount(M : TObject; Remaining : Word);
  begin
    Labels[OnLabel].Caption := AproLoadStr(ddRetryWaiting) + ' ' +
      IntToStr(Remaining);
  end;

  procedure TDialerForm.FrmRetryEnd(Sender : TObject);
  begin
    Labels[OnLabel].Caption := AproLoadStr(ddRetryWaitOver);         
  end;

  procedure TDialerForm.FrmDialStart(Sender : TObject);
  begin
    AddStatusString(AproLoadStr(ddDialing) + ' ' +                   
      IntToStr(Dialer.Modem.DialTimeout));
  end;

  procedure TDialerForm.FrmConnect(Sender : TObject);
  begin
    AddResString(ddModemConnect);
  end;

  procedure TDialerForm.FrmDialCount(M : TObject; Remaining : Word);
  begin
    Labels[OnLabel].Caption := AproLoadStr(ddDialing) + ' ' +        
      IntToStr(Remaining);
  end;

  procedure TDialerForm.FrmGotLineSpeed(M : TObject; Speed : LongInt);
  begin
    AddStatusString(format(AproLoadStr(ddModemConnectAt),
      [Speed]));                       
  end;

  procedure TDialerForm.FrmBusy(Sender : TObject);
  begin
    if (Dialer.DialResult = drAborted) then
      ModalResult := mrAbort
    else
      AddResString(ddModemBusy);
  end;

  procedure TDialerForm.FrmVoice(Sender : TObject);
  begin
    if (Dialer.DialResult = drAborted) then
      ModalResult := mrAbort
    else
      AddResString(ddModemVoice);
  end;

  procedure TDialerForm.FrmError(Sender : TObject);
  begin
    if (Dialer.DialResult = drAborted) then
      ModalResult := mrAbort
    else
      AddResString(ddModemError);
  end;

  procedure TDialerForm.FrmNoCarrier(Sender : TObject);
  begin
    if (Dialer.DialResult = drAborted) then
      ModalResult := mrAbort
    else
      AddResString(ddModemNoCarrier);
  end;

  procedure TDialerForm.FrmNoDialTone(Sender : TObject);
  begin
    if (Dialer.DialResult = drAborted) then
      ModalResult := mrAbort
    else
      AddResString(ddModemNoDialTone);
  end;

  procedure TDialerForm.FrmDialTimedOut(Sender : Tobject);
  begin
    Labels[OnLabel].Caption := AproLoadStr(ddDialTimedOut);         
  end;

  procedure TDialerForm.FrmTooManyTries(Sender : TObject);
  begin
    ModalResult := mrAbort;
  end;

  procedure TDialerForm.FrmConnectionEstablished(Sender : TObject);
  begin
    ModalResult := mrOK;
    PostMessage(Handle,wm_Close,0,0);
  end;

  procedure TDialerForm.FrmTriggerTimer(CP : TObject; TriggerHandle : Word);
  begin
    if (TriggerHandle = DialTrig) then begin
      Dialer.Modem.ComPort.OnTriggerTimer := SaveTrig;
      Dialer.Modem.ComPort.RemoveTrigger(DialTrig);
      SaveTrig := nil;
      DialTrig := 0;
      Dialer.Dial;
    end else if Assigned(SaveTrig) then
      SaveTrig(CP, TriggerHandle);
  end;

{TApdDialerDialog}

  procedure TApdDialerDialog.Notification(AComponent : TComponent; Operation : TOperation);
  begin
    inherited Notification(AComponent, Operation);

    if (Operation = opRemove) then begin
      {see if our modem is going away}
      if (AComponent = FModem) then
        FModem  := nil;
    end else if (Operation = opInsert) then begin
      {check for modem being installed}
      if not Assigned(FModem) and (AComponent is TApdCustomModem) then
        Modem := TApdCustomModem(AComponent);
    end;
  end;

  constructor TApdDialerDialog.Create(AOwner : TComponent);
  var
    I : Integer;

  begin
    inherited Create(AOwner);

    {set default property values}
    FModem         := nil;
    FPhoneNumber   := '';
    FMaxDialTries  := addDefMaxDialTries;
    FRetrySecs     := addDefRetrySecs;
    FDialerOptions := addDefDialerOptions;
    FCaption       := 'Dial Progress';

    {search our owner for a modem}
    if Assigned(AOwner) and (AOwner.ComponentCount > 0) then
      for I := 0 to Pred(AOwner.ComponentCount) do
        if AOwner.Components[I] is TApdCustomModem then begin
          FModem := TApdCustomModem(AOwner.Components[I]);
          Break;
        end;
  end;

  function TApdDialerDialog.Execute : Boolean;
  var
    Dialer : TApdCustomModemDialer;
    Frm    : TDialerForm;

  begin
    {check for valid property values}
    if not Assigned(FModem) then
      raise EModemNotAssigned.Create(ecModemNotAssigned, False);
    if (Modem.ComPort = nil) then
      raise EPortNotAssigned.Create(ecPortNotAssigned, False);

    {create form}
    Frm       := TDialerForm.Create(Self);
    Frm.Ctl3D := Ctl3D;
    Frm.Caption := FCaption;                                       

    try
      {create the dialer}
      Dialer               := TApdCustomModemDialer.Create(Self);
      Dialer.Modem         := Modem;
      Dialer.PhoneNumber   := PhoneNumber;
      Dialer.MaxDialTries  := MaxDialTries;
      Dialer.RetrySecs     := RetrySecs;
      Dialer.DialerOptions := DialerOptions;
    except
      Frm.Free;
      raise;
    end;

    {dial the modem}
    Frm.DoDial(Dialer);
    Frm.Free;

    Result := (Dialer.DialResult = drConnected);

    Dialer.Free;
  end;

  function TApdDialerDialog.GetVersion : string;
  begin
    Result := ApVersionStr;
  end;

  procedure TApdDialerDialog.SetVersion(const Value : string);
  begin
  end;

end.
