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
{*                    AWCOMM.PAS 4.06                    *}
{*********************************************************}
{* Deprecated 16-bit device layer                        *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$X+,F+,K+}

unit AwComm;
  {-Device layer for standard Windows COMM.DRV}

interface

uses
  WinTypes,
  WinProcs,
  SysUtils,
  Messages,
  OoMisc,
  awUser;

type
  TApdCommDispatcher = class(TApdBaseDispatcher)
    private
      MsrShadow : PByte;
      UseMSRShadow : Boolean;
    protected
      function CloseCom : Integer; override;
      function EscapeComFunction(Func : Integer) : LongInt; override;
      function FlushCom(Queue : Integer) : Integer; override;
      function GetComError(var Stat : TComStat) : Integer; override;
      function GetComEventMask(EvtMask : Integer) : Word; override;
      function GetComState(var DCB: TDCB): Integer; override;
      function OpenCom(ComName: PChar; InQueue, OutQueue : Word) : Integer; override;
      function ProcessCommunications : Integer; override;
      function SetComEventMask(EvtMask : Word) : PWord; override;
      function SetComState(var DCB : TDCB) : Integer; override;
      procedure StartDispatcher; override;
      procedure StopDispatcher; override;
      function ReadCom(Buf : PChar; Size: Integer) : Integer; override;
      function WriteCom(Buf : PChar; Size: Integer) : Integer; override;
      procedure SetMsrShadow(OnOff : Boolean); override;
      function SetupCom(InSize, OutSize : Integer) : Boolean; override;

      procedure Dispatcher(Msg : Cardinal;
                         wParam : Cardinal; lParam : LongInt){ : Cardinal};
  end;

  {$IFDEF EnableTapi16}
  TApdTAPI16Dispatcher = class(TApdCommDispatcher)
    protected
      function OpenCom(ComName: PChar; InQueue, OutQueue : Cardinal) : Integer; override;
      function CloseCom : Integer; override;
    public
      constructor Create(Owner : TObject; InCid : Integer);
  end;
  {$ENDIF}

implementation

const
  {Modem status absolute state masks}
  CtsMask  = $10;
  DsrMask  = $20;
  DCDMask  = $40;
  RingMask = $80;

  {Avoid linking in WIN31}
  ev_CTSS   = $0400;    {CTS state}
  ev_DSRS   = $0800;    {DSR state}
  ev_RLSDS  = $1000;    {RLSD state}
  ev_RingTe = $2000;    {Ring trailing edge indicator}

  {From COMM.DRV (MicroSoft says they'll never change it...)}
  MsrShadowOfs = 35;

  {Flags to mask off if using MSRShadow register}
  AbsEventFlags = EV_CTSS + EV_DSRS + EV_RLSDS + EV_Ring;

  {For isolating line status changes}
  ev_LineChange = ev_Err + ev_Break;

  {For isolating modem status changes}
  ev_ModemChange = ev_CTS + ev_DSR + ev_RLSD + ev_Ring + ev_RingTe;

  {Default level for comm notifications}
  DefCNLevel = 10;

{Merging modem status bits and Flags}

  procedure MergeMsWithFlags(MS : Byte; var Flags : Word);
    {-Merge modem status bits with Flags}
  begin
    {Remove absolute event flags from Flags}
    Flags := Flags and not AbsEventFlags;

    {Add in absolute event flags as required}
    if MS and DsrMask = DsrMask then
      Flags := Flags or EV_DSRS;
    if MS and CtsMask = CtsMask then
      Flags := Flags or EV_CTSS;
    if MS and DCDMask = DCDMask then
      Flags := Flags or EV_RLSDS;
    if MS and RingMask = RingMask then
      Flags := Flags or EV_RING;
  end;

{Route application APIs to standard Windows APIs}

  procedure TApdCommDispatcher.SetMsrShadow(OnOff : Boolean);
    {-Enable/disable use of msr shadow register}
  begin
    UseMsrShadow := OnOff;
  end;

  function TApdCommDispatcher.SetupCom(InSize, OutSize : Integer) : Boolean;
  begin
    Result := True;
  end;

  function TApdCommDispatcher.CloseCom: Integer;
    {-Route to standard Windows API}
  begin
    Result := CloseComm(CidEx);
  end;

  function TApdCommDispatcher.EscapeComFunction(Func: Integer): LongInt;
    {-Route to standard Windows API}
  begin
    Result := EscapeCommFunction(CidEx, Func);
  end;

  function TApdCommDispatcher.FlushCom(Queue: Integer): Integer;
    {-Route to standard Windows API}
  begin
    Result := FlushComm(CidEx, Queue);
  end;

  function TApdCommDispatcher.GetComError(var Stat: TComStat): Integer;
    {-Route to standard Windows API}
  begin
    Result := GetCommError(CidEx, Stat);
  end;

  function TApdCommDispatcher.GetComEventMask(EvtMask: Integer): Word;
    {-Route to standard Windows API}
  var
    {Index : Integer;}
    MS : Word;
    Flags : Word;
  begin
    Flags := GetCommEventMask(CidEx, EvtMask);

    {Set absolute bits from shadow register}
    if UseMsrShadow then begin
      MS := MsrShadow^;
      MergeMsWithFlags(MS, Flags);
    end;
    Result := Flags;
  end;

  function TApdCommDispatcher.GetComState(var DCB: TDCB): Integer;
    {-Route to standard Windows API}
  begin
    Result := GetCommState(CidEx, DCB);
  end;

  function TApdCommDispatcher.OpenCom(ComName: PChar; InQueue, OutQueue: Word): Integer;
    {-Route to standard Windows API}
  begin
    CidEx := OpenComm(ComName, InQueue, OutQueue);
    Result := CidEx;
  end;

  function TApdCommDispatcher.ProcessCommunications : Integer;
    {-Call the dispatcher once to process incoming data (but not triggers)}
  const
    NotifyBits : array[Boolean] of Longint = (0, cn_Receive);
  begin
    Result := 0;
    Dispatcher(0, 1, NotifyBits[EventWord^ and ev_RxChar <> 0]);
  end;

  function TApdCommDispatcher.ReadCom(Buf: PChar; Size: Integer): Integer;
    {-Route to standard Windows API}
  begin
    Result := ReadComm(CidEx, Buf, Size);
  end;

  function TApdCommDispatcher.SetComEventMask(EvtMask: Word): PWord;
    {-Route to standard Windows API}
  var
    {Index : Integer;}
    P : PWord;
  begin
    P := SetCommEventMask(CidEx, EvtMask);

    MsrShadow := PByte(P);
    Inc(LongInt(MsrShadow), MsrShadowOfs);

    Result := P;
  end;

  function TApdCommDispatcher.SetComState(var DCB: TDCB): Integer;
    {-Route to standard Windows API}
  begin
    Result := SetCommState(DCB);
  end;

  function TApdCommDispatcher.WriteCom(Buf: PChar; Size: Integer): Integer;
    {-Route to standard Windows API}
  begin
    Result := WriteComm(CidEx, Buf, Size);
  end;

  procedure TApdCommDispatcher.Dispatcher(Msg : Cardinal;
                       wParam : Cardinal; lParam : LongInt){ : Cardinal};
    {-Dispatch COMM functions}
  var
    I, J : Cardinal;
    Len : Integer;
    BytesToRead : Cardinal;
    FreeSpace : Cardinal;
    BeginFree : Cardinal;
    EndFree : Cardinal;
    Start, Stop : Cardinal;
    ReceiveData : Boolean;
    LineError : Boolean;
    ModemEvent : Boolean;

  begin
    if ClosePending then
      Exit;

    if not EventBusy or (wParam <> 0) then begin
      if Flags and poUseEventWord <> 0 then begin

        {Set local condition flags}
        ReceiveData :=
          (EventWord^ and ev_RxChar <> 0);

        {Check for line and modem events}
        LineError := EventWord^ and ev_LineChange <> 0;
        ModemEvent := EventWord^ and ev_ModemChange <> 0;
      end else begin
        {No event Cardinal, call GetCommError to check for incoming data...}
        LastError := GetComError(ComStatus);
        ReceiveData := ComStatus.cbInQue > 0;

        {...and always check for line/modem events}
        LineError := True;
        ModemEvent := true;
      end;

      {Check for received data event}
      if ReceiveData then begin

        {Clear the received char event}
        GetComEventMask(ev_RxChar);

        if not ExtractData then
          {Check for missed data in que, shouldn't happen}
          RefreshStatus;
      end;

      {Check for line errors}
      if LineError then begin
        {Get and clear event}
        J := GetComEventMask(ev_Err) and ev_Err;

        {Get and clear error (updates LastError field)}
        RefreshStatus;

        if DLoggingOn and (LastLineErr <> J) then
          AddDispatchEntry(dtDispatch,
                            dstLineStatus, J, nil, 0);
        LastLineErr := J;                                           
      end;

      {Check for modem status changes}
      if ModemEvent then begin
        {Get and clear the event}
        J := GetComEventMask(ev_ModemChange);

        if FlagIsSet(Flags, $01{poUseMSRShadow}) then
          {Update ModemStatus from MSRShadow register}
          ModemStatus := MsrShadow^ and $00FF
        else
          {Map to EV_ flags to modem status register bits}
          MapEventsToMS(J);

        if DLoggingOn and (LastModemStatus <> ModemStatus) then
          AddDispatchEntry(dtDispatch, dstModemStatus,
                            ModemStatus, @J, 2);
        LastModemStatus := ModemStatus;
      end;

      {Check all triggers}
      if not EventBusy and (wParam = 0) then begin
        GlobalStatHit := False;                                     
        while CheckTriggers and not ClosePending do ;

        {Allow status triggers to hit again}
        if GlobalStatHit then                                     
          ResetStatusHits;
      end;

    end else
      {Attempt at re-entrancy}
      if DLoggingOn then
        AddDispatchEntry(dtError, dstNone, 0, nil, 0);
    {Check for pending close}
    if ClosePending then
      DonePortPrim;
  end;

  function CommTimer(H : TApdHwnd; Msg : Cardinal; wParam : Cardinal;
                      lParam : LongInt) : Cardinal; export;
    {-Dispatch COMM functions}
  var
    Device : TApdCommDispatcher;

    function FindHandle(TimerID : Integer) : TApdCommDispatcher;
      {-Return handle of Cid}
    var
      I : Integer;
    begin
      for I := 0 to pred(PortList.Count) do begin
        Result := TApdCommDispatcher(PortList[i]);
        if (Result <> nil) then
          if (Result.TimerID = TimerID) then
            Exit;
      end;                                                          
      Result := nil;
    end;

  begin
    CommTimer := 0;
    Device := FindHandle(wParam);
    if Device <> nil then
      Device.Dispatcher(0, 0, lParam);
  end;

  procedure TApdCommDispatcher.StartDispatcher;
  begin
    {See if we're already active}
    if DispActive then
      raise Exception.Create('Dispatcher already started');

    DispActive := True;

    {Start dispatcher in appropriate mode}
    TimerID := SetTimer(0, 1, TimerFreq, @CommTimer);
    if TimerID = 0 then
      raise Exception.Create('Resource not available');
    CreateDispatcherWindow;
    if not EnableCommNotification(CidEx, DispatcherWindow,
                                    DefCNLevel, -1) then
      raise Exception.Create('Resource not available');
  end;

  procedure TApdCommDispatcher.StopDispatcher;
  begin
    if not DispActive then
      Exit;                                                       

    KillTimer(0, TimerID);
    EnableCommNotification(CidEx, 0, -1 , -1);
    DestroyWindow(DispatcherWindow);
    DispActive := False;
  end;

  {$IFDEF EnableTapi16}
  constructor TApdTAPI16Dispatcher.Create(Owner : TObject; InCid : Integer);
  begin
    CidEx := InCid;                                               
    inherited Create(Owner);
  end;

  function TApdTAPI16Dispatcher.OpenCom(ComName: PChar; InQueue, OutQueue : Cardinal) : Integer;
  begin
    if CidEx <> 0 then
      Result := CidEx                                            
    else
      Result := ecCommNotOpen;
  end;

  function TApdTAPI16Dispatcher.CloseCom : Integer;
  begin
    Result := CloseComm(CidEx);
    CidEx := 0;                                                     
  end;
  {$ENDIF}

  function DispatcherWndFunc(hWindow : TApdHwnd; Msg, wParam : Cardinal;
                             lParam : Longint) : Longint;
                             {$IFDEF Win32}
                             stdcall; export;
                             {$ELSE}
                             export;
                             {$ENDIF}
    {-Window function for wm_CommNotify or cw_ApdSocketMessage messages}
  var
    I : Integer;
  begin
    Result := 0;
    {Pass wm_CommNotify messages to the dispatcher}
    if Msg = wm_CommNotify then begin
      for I := 0 to pred(PortList.Count) do
        if (i < PortList.Count) and (PortList[i] <> nil) then       
          with TApdCommDispatcher(PortList[i]) do
            if (CidEx = wParam) then begin
              Dispatcher(Msg, 0, lParam);
              break;
            end;
    end else
      Result := DefWindowProc(hWindow, Msg, wParam, lParam);
  end;

  procedure RegisterDispatcherClass;
  const
    Registered : Boolean = False;
  var
    XClass: TWndClass;
  begin
    if Registered then
      Exit;
    Registered := True;

    with XClass do begin
      Style         := 0;
      lpfnWndProc   := @DispatcherWndFunc;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      {$IFDEF VERSION3}
      hInstance     := System.MainInstance;
      {$ELSE}
      hInstance     := System.hInstance;
      {$ENDIF}
      hIcon         := 0;
      hCursor       := 0;
      hbrBackground := 0;
      lpszMenuName  := nil;
      lpszClassName := DispatcherClassName;
    end;
    WinProcs.RegisterClass(XClass);
  end;

begin
  {if not (csDesigning in ComponentState) then}
    RegisterDispatcherClass;
end.
