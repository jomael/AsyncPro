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
{*                   AWFOSSIL.PAS 4.06                   *}
{*********************************************************}
{* Deprecated Fossil device layer/dispatcher             *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$X+,F+}

unit AwFossil;
  {-Device layer for standard FOSSIL driver under Windows}

interface

uses
  WinTypes,
  WinProcs,
  SysUtils,
  OoMisc,
  awUser,
  awComm;

type
  TApdFossilDispatcher = class(TApdCommDispatcher)
    protected
      function CloseCom : Integer;                                          override;
      function EscapeComFunction(Func : Integer) : LongInt;                 override;
      function FlushCom(Queue : Integer) : Integer;                         override;
      function GetComError(var Stat : TComStat) : Integer;                  override;
      function GetComEventMask(EvtMask : Integer) : Word;                   override;
      function GetComState(var DCB: TDCB): Integer;                         override;
      function OpenCom(ComName: PChar; InQueue, OutQueue : Word) : Integer; override;
      function SetComEventMask(EvtMask : Word) : PWord;                     override;
      function SetComState(var DCB : TDCB) : Integer;                       override;
      function ReadCom(Buf : PChar; Size: Integer) : Integer;               override;
      function WriteCom(Buf : PChar; Size: Integer) : Integer;              override;
      procedure SetMsrShadow(OnOff : Boolean);                              override;
      function SetupCom(InSize, OutSize : Integer) : Boolean;               override;
  end;

implementation

var
  {Timer callback address}
  TimerAddr   : TFarProc;

  {Misc}
  ExitSave : Pointer;

const
  FossilTimerActive : Word = 0;

type
  {Information we need to store about each open TApdFossilDispatcher.AP port}
  TFossilPortInfo = record
    PortID     : Word;                  {Port ID, 0=COM1, 1=COM2, etc}
    EventWord  : Word;                  {Event word used by caller}
    EventFlags : Word;                  {Events we're checking for}
    Filler     : array[1..31] of Byte;  {Fill to MSR shadow locations}
    MSRShadow  : Byte;                  {MSR shadow register}
  end;

  TRegisters = record
    case Integer of
      0: (AX, BX, CX, DX, BP, SI, DI, DS, ES, Flags: Word);
      1: (AL, AH, BL, BH, CL, CH, DL, DH: Byte);
  end;

  {Holds driver information from TApdFossilDispatcher.AP GetDriverInfo call}
  PDriverInfo = ^TDriverInfo;
  TDriverInfo = record
    diSize      : Word;
    diSpec      : Byte;
    diRev       : Byte;
    diID        : Pointer;
    diInSize    : Word;
    diInFree    : Word;
    diOutSize   : Word;
    diOutFree   : Word;
    diSWidth    : Byte;
    diSHeight   : Byte;
    diBaudMask  : Byte;
  end;

  DPMIRegisters = record
    drDI : LongInt;
    drSI : LongInt;
    drBP : LongInt;
    drReserved : LongInt;
    drBX : LongInt;
    drDX : LongInt;
    drCX : LongInt;
    drAX : LongInt;
    drFlags : Word;
    drES : Word;
    drDS : Word;
    drFS : Word;
    drGS : Word;
    drIP : Word;
    drCS : Word;
    drSP : Word;
    drSS : Word;
  end;

  {DOS memory pointer}
  TDosMemRec = record
    Sele, Segm : Word;
  end;

var
  {Needed by most calls so make global}
  Regs : TRegisters;

  {Info needed by each open TApdFossilDispatcher.AP port}
  FossilPorts : array[1..MaxComHandles] of TFossilPortInfo;

  {DOS memory blocks for TDriverInfo and I/O}
  InfoBlock : array[1..MaxComHandles] of TDosMemRec;
  IOBlock   : array[1..MaxComHandles] of TDosMemRec;

  {Keep track of open Cids}
  ValidCids : array[1..MaxComHandles] of Integer;

  {Timer identifier}
  FossilTimerID : Word;

const
  {Misc constants}
  FossilSignature = $1954;       {Signature returned by TApdFossilDispatcher.AP driver}
  TFossilRecAPFreq : Word = 100;        {Timer resolution}

{DPMI specifics}

  function SimulateRealModeInt(IntNo : Byte;
                               var Regs : DPMIRegisters) : Word; Assembler;
  asm
    xor     bx,bx
    mov     bl,IntNo
    xor     cx,cx       {StackWords = 0}
    les     di,Regs
    mov     ax,0300h
    int     31h
    jc      @@ExitPoint
    xor     ax,ax
  @@ExitPoint:
  end;

{Managing Cids}

  function AssignCid : Integer;
    {-Return next available CID, -1 if none left}
  var
    I : Word;
  begin
    for I := 1 to MaxComHandles do
      if ValidCids[I] = -1 then begin
        ValidCids[I] := I;
        AssignCid := I;
        Exit;
      end;
    AssignCid := -1;
  end;

  procedure RemoveCid(Cid : Integer);
    {-Mark Cid as available}
  begin
    if (Cid > 0) and (Cid <= MaxComHandles) then
      ValidCids[Cid] := -1;
  end;

{Utility routines}

  procedure FossilIntr(var Regs : TRegisters);
    {-Virtualized int}
  var
    DRegs : DPMIRegisters;
  begin
    FillChar(DRegs, SizeOf(DRegs), 0);
    with DRegs do begin
      drAX := Regs.AX;
      drBX := Regs.BX;
      drCX := Regs.CX;
      drDX := Regs.DX;
      drES := Regs.ES;
      drDI := Regs.DI;
      if SimulateRealModeInt($14, DRegs) = 0 then ;
      Regs.AX := drAX;
    end;
  end;

  function UpdateModemStatus(Cid : Word) : Byte;
    {-Return MSR from TApdFossilDispatcher.AP}
  begin
    with Regs do begin
      AH := $03;
      DX := FossilPorts[Cid].PortID;
      FossilIntr(Regs);
      UpdateModemStatus := AL;
    end;
  end;

  procedure UpdateDriverInfo(Cid : Integer; var Info : TDriverInfo);
    {-Return current driver information from the TApdFossilDispatcher.AP driver}
  begin
    with Regs do begin
      AH := $1B;
      CX := SizeOf(Info);
      DX := FossilPorts[Cid].PortID;
      Regs.ES := InfoBlock[Cid].Segm;
      Regs.DI := 0;
      FillChar(Mem[InfoBlock[Cid].Sele:0], SizeOf(Info), 0);
      FossilIntr(Regs);
      Move(Mem[InfoBlock[Cid].Sele:0], Info, SizeOf(Info));
    end;
  end;

  function FossilTimer(H : TApdHwnd; Msg : Word; wParam : Word; lParam : LongInt) : Word; export;
    {-Check status, update event word}
  var
    I : Word;
    Info : TDriverInfo;
  begin
    for I := 1 to MaxComHandles do begin
      if ValidCids[I] <> -1 then begin
        with FossilPorts[I] do begin
          {Update info}
          MSRShadow := UpdateModemStatus(I);
          UpdateDriverInfo(I, Info);

          {Set rcv/transmit bits in EventWord}
          with Info do begin
            if diInFree < diInSize then
              EventWord := EventWord or EV_RXCHAR;
            if diOutFree = diOutSize then
              EventWord := EventWord or EV_TXEMPTY;
          end;
        end;
      end;
    end;
  end;

  function TApdFossilDispatcher.CloseCom: Integer;
    {-Close TApdFossilDispatcher.AP port and possibly clear timer}
  var
    I : Word;
  begin
    Result := ecOK;

    with Regs do begin
      {Deinit the TApdFossilDispatcher.AP}
      AH := $05;
      DX := FossilPorts[CidEx].PortID;
      FossilIntr(Regs);
    end;

    {Clear our event word}
    FossilPorts[CidEx].EventWord := 0;

    {Release buffers}
    if LongInt(InfoBlock[CidEx]) <> 0 then
      GlobalDosFree(InfoBlock[CidEx].Sele);
    if LongInt(IOBlock[CidEx]) <> 0 then
      GlobalDosFree(IOBlock[CidEx].Sele);

    {See if we should release the timer}
    Dec(FossilTimerActive);
    if FossilTimerActive = 0 then
      KillTimer(0, FossilTimerID);

    RemoveCid(CidEx);
  end;

  function TApdFossilDispatcher.EscapeComFunction(Func: Integer): LongInt;
    {-Set/clear modem signals, xon/xoff status as requested}
  begin
    Result := ecOK;
    with Regs do begin
      case Func of
        SetXoff :
          AX := $1002;
        SetXon  :
          AX := $1000;
        WinTypes.SetDtr  :
          AX := $0601;
        WinTypes.ClrDtr  :
          AX := $0600;
        WinTypes.SetRts,
        WinTypes.ClrRts,
        ResetDev :
          begin
            Result := ecNotSupported;
            Exit;
          end;
      end;
      DX := FossilPorts[CidEx].PortID;
      FossilIntr(Regs);
    end;
  end;

  function TApdFossilDispatcher.FlushCom(Queue: Integer): Integer;
    {-Flush input or output buffer}
  begin
    Result := ecOK;
    with Regs do begin
      DX := FossilPorts[CidEx].PortID;
      if Queue = 0 then
        AX := $0900
      else
        AX := $0A00;
      FossilIntr(Regs);
    end;
  end;

  function TApdFossilDispatcher.GetComError(var Stat: TComStat): Integer;
    {-Make TApdFossilDispatcher.AP call to get status info}
  var
    Info : TDriverInfo;
  begin
    Result := 0;
    UpdateDriverInfo(CidEx, Info);
    with Info, Stat do begin
      cbInQue  := diInSize  - diInFree;
      cbOutQue := diOutSize - diOutFree;

      {Handle X00 bug}
      if cbOutQue = 1 then
        cbOutQue := 0;
    end;
  end;

  function TApdFossilDispatcher.GetComEventMask(EvtMask: Integer): Word;
    {-Return current event mask and clear requested event}
  var
    W : Word;
  begin
    {Clear requested bits in event word}
    with FossilPorts[CidEx] do begin
      W := EventWord;
      EventWord := EventWord xor EvtMask;
    end;
    Result := W;
  end;

  function TApdFossilDispatcher.GetComState(var DCB: TDCB): Integer;
    {-Fill DCB with what info we can get}
  var
    Info : TDriverInfo;
  begin
    with DCB, FossilPorts[CidEx] do begin
      UpdateDriverInfo(CidEx, Info);
      FillChar(DCB, SizeOf(DCB), 0);
      Id := CidEx;
      with Info do
        case (diBaudMask shr 5) of
          $02  : BaudRate := 300;
          $03  : BaudRate := 600;
          $04  : BaudRate := 1200;
          $05  : BaudRate := 2400;
          $06  : BaudRate := 4800;
          $07  : BaudRate := 9600;
          $00  : BaudRate := 19200;
          $01  : BaudRate := 38400;
          else   BaudRate := 0;
        end;
      ByteSize := 8;
      Parity := NoParity;
      StopBits := 1;
    end;
    Result := 0;
  end;

  function ComNumber(ComName : PChar) : Integer;
    {-Return a TApdFossilDispatcher.AP port number for ComName, e.g. COM1 = 0}
  var
    P : PChar;
    I : Integer;
    Code : Integer;
  begin
    ComNumber := -1;
    if StrLIComp('COM', ComName, 3) <> 0 then
      Exit;
    P := ComName+3;
    Val(P, I, Code);
    if Code = 0 then
      ComNumber := I-1;
  end;

  function TApdFossilDispatcher.OpenCom(ComName : PChar; InQueue, OutQueue : Word): Integer;
    {-Start TApdFossilDispatcher.AP driver}
  begin
    with Regs do begin
      AH :=$04;
      BX := 0;
      DX := ComNumber(ComName);
      if (Integer(DX) < 0) or (DX > 7) then begin
        Result := ecHardware;
        Exit;
      end;
      FossilIntr(Regs);
      if AX = FossilSignature then begin
        {Return next CID}
        CidEx := AssignCid;
        if CidEx > 0 then
          with FossilPorts[CidEx] do begin
            PortID := ComNumber(ComName);
            EventWord := 0;
            EventFlags := 0;
          end;

        LongInt(IOBlock[CidEx]) := 0;

        {Allocate TDriverInfo block}
        LongInt(InfoBlock[CidEx]) := GlobalDosAlloc(SizeOf(TDriverInfo));
        if LongInt(InfoBlock[CidEx]) = 0 then begin
          CloseCom;
          Result := ecOutOfMemory;
          Exit;
        end;

        {Allocate I/O buffer}
        LongInt(IOBlock[CidEx]) := GlobalDosAlloc(DispatchBufferSize);
        if LongInt(IOBlock[CidEx]) = 0 then begin
          CloseCom;
          Result := ecOutOfMemory;
          Exit;
        end;

        Result := CidEx;
      end else begin
        Result := ecHardware;
      end;
    end;
  end;

  function TApdFossilDispatcher.ReadCom(Buf: PChar; Size: Integer): Integer;
    {-Read size bytes from TApdFossilDispatcher.AP driver}
  var
    I : Word;
    Info : TDriverInfo;
    Limit : Word;
  begin
    with Regs do begin
      {Get actual buffer count}
      UpdateDriverInfo(CidEx, Info);
      Limit := Info.diInSize - Info.diInFree;
      if Limit > Size then
        Limit := Size;
      if Limit > DispatchBufferSize then
        Limit := DispatchBufferSize;

      {Read a block of data}
      AH := $18;
      DX := FossilPorts[CidEx].PortID;
      ES := IOBlock[CidEx].Segm;
      DI := 0;
      CX := Limit;
      FossilIntr(Regs);
      if AX > 0 then begin
        Move(Mem[IOBlock[CidEx].Sele:0], Buf^, AX);
        Result := AX;
      end else
        Result := 0;
    end;
  end;

  function TApdFossilDispatcher.SetComEventMask(EvtMask: Word): PWord;
    {-Start a timer to simulate COMM.DRV event word processing}
  begin
    if FossilTimerActive = 0 then
      FossilTimerID := SetTimer(0, 1, TFossilRecAPFreq, @FossilTimer);
    Inc(FossilTimerActive);

    if FossilTimerID = 0 then begin
      Result := nil;
    end else begin
      Result := @FossilPorts[CidEx].EventWord;
      FossilPorts[CidEx].EventFlags := EvtMask;
    end;
  end;

  function BaudMask(Baud : LongInt; var Mask : Byte) : Boolean;
    {-Convert Baud to Mask, return False if invalid Baud}
  begin
    BaudMask := True;
    case Baud div 10 of
      30   : Mask := $02;
      60   : Mask := $03;
      120  : Mask := $04;
      240  : Mask := $05;
      480  : Mask := $06;
      960  : Mask := $07;
      1920 : Mask := $00;
      3840 : Mask := $01;
      else begin
        Mask := 0;
        BaudMask := False;
      end;
    end;
  end;

  function TApdFossilDispatcher.SetComState(var DCB: TDCB): Integer;
    {-Set line parameters and/or flow control}
  var
    BaudCode,
    ParityCode,
    DataCode,
    StopCode : Byte;
    SaveAX : Word;
  begin
    Result := ecOK;

    with DCB, Regs do begin
      {Set baud rate}
      AH := $00;
      if not BaudMask(BaudRate, BaudCode) then begin
        Result := ecBaudRate;
        Exit;
      end;

      {Set parity code}
      case Parity of
        NoParity : ParityCode := 0;
        OddParity : ParityCode := 1;
        EvenParity : ParityCode := 3;
        else begin
          Result := ecNotSupported;
          Exit;
        end;
      end;

      {Set databit and stopbit codes}
      if Stopbits < 1 then
        Stopbits := 1
      else if Stopbits > 2 then
        Stopbits := 2;
      StopCode := StopBits - 1;
      DataCode := ByteSize - 5;

      {Assemble the option byte and try to set the options}
      AL := (BaudCode shl 5) + (ParityCode shl 3) +
            (StopCode shl 2) + DataCode;
      DX := FossilPorts[ID].PortID;
      SaveAX := AX;
      FossilIntr(Regs);
      if (AX = SaveAX) or (AX = 0) then begin
        Result := ecHardware;
        Exit;
      end;

      {First make sure all flow control is off}
      AH := $0F;
      AL := $00;
      DX := FossilPorts[ID].PortID;
      FossilIntr(Regs);

      {Set standard TApdFossilDispatcher.AP hdw flow control for any hdw flow requests}
      if ((DCB.Flags and dcb_OutxCtsFlow) = dcb_OutxCtsFlow) or
         ((DCB.Flags and dcb_RtsFlow) = dcb_RtsFlow) or
         ((DCB.Flags and dcb_OutxDsrFlow) = dcb_OutxDsrFlow) or
         ((DCB.Flags and dcb_DtrFlow) = dcb_DtrFlow) then begin
        AH := $0F;
        AL := $02;
        DX := FossilPorts[ID].PortID;
        FossilIntr(Regs);
      end;

      {Set standard TApdFossilDispatcher.AP sfw flow control for any sfw flow requests}
      if ((DCB.Flags and dcb_OutX) = dcb_OutX) or
         ((DCB.Flags and dcb_InX) = dcb_InX) then begin
        AH := $0F;
        AL := $09;
        DX := FossilPorts[ID].PortID;
        FossilIntr(Regs);
      end;
    end;
  end;

  function TApdFossilDispatcher.WriteCom(Buf: PChar; Size: Integer): Integer;
    {-Call TApdFossilDispatcher.AP character output}
  begin
    {Call TApdFossilDispatcher.AP to send a char}
    with Regs do begin
      if Size > DispatchBufferSize then begin
        Result := ecBadArgument;
        Exit;
      end;

      {Move data to IOBlock}
      Move(Buf^, Mem[IOBlock[CidEx].Sele:0], Size);

      {Send it to the TApdFossilDispatcher.AP driver}
      AH := $19;
      CX := Size;
      ES := IOBlock[CidEx].Segm;
      DI := 0;
      DX := FossilPorts[CidEx].PortID;
      FossilIntr(Regs);

      if AX > 0 then
        if AX <> Size then
          Result := -AX
        else
          Result := AX
      else
        Result := 0;
    end;
  end;

  procedure TApdFossilDispatcher.SetMsrShadow(OnOff : Boolean);
  begin
    {Nothing to do for TApdFossilDispatcher.AP}
  end;

  function TApdFossilDispatcher.SetupCom(InSize, OutSize : Integer) : Boolean;
  begin
    {Nothing to do for TApdFossilDispatcher.AP}
  end;

begin
  {Inits}
  FillChar(ValidCids, SizeOf(ValidCids), $FF);
end.
