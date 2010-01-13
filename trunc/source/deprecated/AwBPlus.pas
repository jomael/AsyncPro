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
{*                   AWBPLUS.PAS 4.06                    *}
{*********************************************************}
{* Deprecated CompuServe B+ protocol (CompuServe doesn't *}
{* support this anymore                                  *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$V-,I-,B-,F+,A-,X+}

{$IFDEF Win32}
{$J+}
{$ENDIF}

unit AwBPlus;
  {-Provides CIS B+ remote (not host) protocol}

interface

uses
  WinTypes,
  WinProcs,
  Messages,
  SysUtils,
  OoMisc,
  AwUser,
  AwTPcl,
  AwAbsPcl,
  AdPort; 

const
  {For estimating protocol transfer times}
  BPlusTurnDelay = 0;        {Millisecond turnaround delay}
  BPlusOverHead  = 20;       {Default overhead for each data subpacket}

{Constructors/destructors}
function bpInit(var P : PProtocolData; H : TApdCustomComPort;
                Options : Cardinal) : Integer;
procedure bpDone(var P : PProtocolData);

function bpReinit(P : PProtocolData) : Integer;
procedure bpDonePart(P : PProtocolData);

{Terminal mode methods}
function bpProcessENQ(P : PProtocolData) : Integer;
function bpProcessESCI(P : PProtocolData; X, Y : Byte) : Integer;
function bpPrepareProcessDLE(P : PProtocolData;
                             var ATimerIndex : Cardinal) : Integer;
function bpProcessDLE(P : PProtocolData; IsData : Bool;
                      var Ready, Start, Upload : Bool) : Integer;

{Control}
procedure bpPrepareReceive(P : PProtocolData);
procedure bpReceive(Msg, wParam : Cardinal;
                   lParam : LongInt);
procedure bpPrepareTransmit(P : PProtocolData);
procedure bpTransmit(Msg, wParam : Cardinal;
                    lParam : LongInt);


implementation

const
  {Default ParamsRecord values}
  DefDR : Byte = 1;   {Can handle Download Resume}
  DefBS : Byte = 16;  {Default to 128 * DefBS (2048) byte packets}
  DefWS = 1;          {Can handle send ahead}
  DefWR = 2;          {Can receive up to 2 packets ahead}
  DefCM = 1;          {Can handle CRC blockchecking}
  DefDQ = 1;          {Can handle special quoting including non-quoted NUL}
  DefUR = 0;          {Can NOT handle Upload Recovery (not supported by CIS)}
  DefFI = 1;          {Can handle File Info packet}
  DefXP = 0;          {FTP/GIF does not use TransportLayer}

  aDataTrigger = 0;

  function IncSequence(Value : Integer) : Integer;
    {-Increment a Sequence Number var}
  begin
    IncSequence := (Succ(Value) mod 10);
  end;

  procedure SendByte(P : PProtocolData; C : Char);
  begin
    with P^ do
      aHC.PutChar(C);
  end;

  function IncSA(P : PProtocolData; Value : Integer) : Integer;
  begin
    with P^ do
      if Value = bSAMax then
        IncSA := 1
      else
        IncSA := Value + 1;
  end;

  procedure UpdateBlockCheck(P : PProtocolData; CurByte : Byte);
    {-Update the CRC/bChecksum to reflect the new byte}

    function UpdCrc(CurByte : Byte; CurCrc : Cardinal) : Cardinal;
      {-Due to an oddity in the CIS handling of CRC's, we use this special
        version of UpdateCrc rather than the one in APMISC.  This function
        requires the CRC lookup table in APMISC.}
    begin
      UpdCrc := CrcTable[((CurCrc shr 8) xor CurByte) and $FF] xor
                (CurCrc shl 8);
    end;

  begin
    with P^ do begin
      if aCheckType = bcCrc16 then
        bChecksum := UpdCRC(CurByte,bChecksum)
      else begin
        bChecksum := bChecksum shl 1;
        if bChecksum > 255 then
          bChecksum := (bChecksum and $FF) + 1;
        bChecksum := bChecksum + CurByte;
        if bChecksum > 255 then
          bChecksum := (bChecksum and $FF) + 1;
      end;
    end;
  end;

  procedure SendQuotedByte(P : PProtocolData; C : Char);
    {-Quote and transmit I}
  var
    B : Byte absolute C;
  begin
    with P^ do begin
      if bQuoteTable[B] <> #0 then begin
        SendByte(P, cDLE);
        SendByte(P, bQuoteTable[B]);
      end else
        SendByte(P, Chr(B));
    end;
  end;

  procedure bpSendAck(P : PProtocolData);
    {-Send Ack}
  begin
    with P^ do begin
      SendByte(P, cDLE);
      SendByte(P, Chr(bSeqNum + Ord('0')));
    end;
  end;

  procedure bpSendNAK(P : PProtocolData);
    {-Send Nak}
  begin
    SendByte(P, cNAK);
  end;

  procedure bpSendData(P : PProtocolData; BNum : Integer);
  var
    I : Integer;
  begin
    with P^ do begin
      with bSBuffer[BNum] do begin
        if bBPlusMode and (aCheckType = bcCrc16) then
          bChecksum := $FFFF
        else
          bChecksum := 0;

        SendByte(P, cDLE);
        SendByte(P, 'B');

        SendByte(P, Chr(Seq+Ord('0')));
        UpdateBlockCheck(P, Byte(Seq+Ord('0')));

        SendByte(P, PType);
        UpdateBlockCheck(P, Byte(PType));

        for I := 1 to Num do begin
          SendQuotedByte(P, Buf^[I]);
          UpdateBlockCheck(P, Byte(Buf^[I]));
        end;

        SendByte(P, cETX);
        UpdateBlockCheck(P, Byte(cETX));

        if bBPlusMode and (aCheckType = bcCrc16) then
          SendQuotedByte(P, Char(Hi(bChecksum)));
        SendQuotedByte(P, Char(Lo(bChecksum)));
      end;
    end;
  end;

  procedure bpSendPacket(P : PProtocolData; APacketType : Char; Size : Integer);
    {-Send a packet of data}
  begin
    with P^ do begin
      bSeqNum := IncSequence(bSeqNum);
      with bSBuffer[bNext2Fill] do begin
        Seq   := bSeqNum;
        Num   := Size;
        PType := APacketType;
      end;
      bpSendData(P, bNext2Fill);
      bNext2Fill := IncSA(P, bNext2Fill);
      Inc(bSAWaiting);
    end;
  end;

  procedure bpSendFailure(P : PProtocolData; Reason : String);
    {-Send a failure packet}
  begin
    with P^ do begin
      bNext2ACK := 1;
      bNext2Fill := 1;
      bSAWaiting := 0;
      bAborting := True;

      with bSBuffer[1] do
        Move(Reason[1], Buf^[1], Length(Reason));

      bpSendPacket(P, 'F', Length(Reason));
    end;
  end;

  procedure bpGetCharQuoted(P : PProtocolData; var C : Char);
    {-Return a character that was transmitted bQuoted}
  label
    Quote;
  begin
    with P^ do begin
      bQuoted := False;

      if bQuotePending then
        goto Quote;
      aHC.ValidDispatcher.GetChar(C);
      if C <> cDLE then
        Exit;

    Quote:
      bQuoted := True;
      if aHC.CharReady then begin
        bQuotePending := False;
        aHC.ValidDispatcher.GetChar(C);
        if C < #$60 then
          C := Char(Ord(C) and $1F)
        else
          C := Char((Ord(C) and $1F) or $80);
      end else
        bQuotePending := True;
    end;
  end;

  function bpCollectPacket(P : PProtocolData) : Bool;
    {-Collect a packet}
  var
    C : Char;
    Finished : Bool;
  begin
    bpCollectPacket := False;

    with P^ do begin
      repeat
        {Reset char timer each time a new character is received}
        aHC.SetTimerTrigger(bCurTimer, aHandshakeWait, True);

        {Process current packet collection state}
        case bPacketState of
          psGetDLE :
            begin
              aHC.ValidDispatcher.GetChar(C);
              case C of
                cDLE : bPacketState := psGetB;
                cENQ : bPacketState := psSendAck;
              end;
            end;

          psGetB :
            begin
              aHC.ValidDispatcher.GetChar(C);
              case C of
                'B'  : bPacketState := psGetSeq;
                ';'  : bPacketState := psGetDLE;
                cEnq : bPacketState := psSendAck;
                else   bPacketState := psGetDLE;
              end;
            end;

          psGetSeq :
            begin
              {Reset timer to discount time spent verifying CRCs}
              if bResumeFlag then
                NewTimer(aTimer, 1);

              aHC.ValidDispatcher.GetChar(C);
              case C of
                cEnq : bPacketState := psSendAck
                else begin
                  if aCheckType = bcCrc16 then
                    bChecksum := $FFFF
                  else
                    bChecksum := 0;
                  UpdateBlockCheck(P, Byte(C));
                  bPacketNum := Ord(C)-Ord('0');
                  bPacketState := psGetType;
                end;
              end;
            end;

          psGetType :
            begin
              bpGetCharQuoted(P, C);
              if bQuotePending then
                Exit;
              UpdateBlockCheck(P, Byte(C));
              bLastType := C;
              bIdx := 1;
              bPacketState := psGetData;
            end;

          psGetData :
            {Stay here while data available...}
            while aHC.CharReady do begin
              bpGetCharQuoted(P, C);
              if bQuotePending then
                Exit;
              UpdateBlockCheck(P, Byte(C));
              if (C = cETX) and not bQuoted then begin
                bPacketState := psGetCheck1;
                Exit;
              end else begin
                bRBuffer^[bIdx] := C;
                Inc(bIdx);
              end;
            end;

          psGetCheck1 :
            begin
              bpGetCharQuoted(P, C);
              if bQuotePending then
                Exit;
              if aCheckType = bcCrc16 then begin
                UpdateBlockCheck(P, Byte(C));
                bPacketState := psGetCheck2;
              end else begin
                bNewChk := Byte(C);
                bPacketState := psCheckCheck;
              end;
            end;

          psGetCheck2 :
            begin
              bpGetCharQuoted(P, C);
              if bQuotePending then
                Exit;
              UpdateBlockCheck(P, Byte(C));
              bNewChk := 0;
              bPacketState := psCheckCheck;
            end;

          psCheckCheck :
            begin
              if bNewChk <> bChecksum then begin
                {bChecksum/CRC error}
                aProtocolStatus := psBlockCheckError;
                aForceStatus := True;
                bPacketState := psError;
              end else if bLastType = 'F' then
                {Always accept failure packet}
                bPacketState := psSuccess
              else if bPacketNum = bSeqNum then begin
                {Dupe packet}
                aProtocolStatus := psDuplicateBlock;
                aForceStatus := True;
                bPacketState := psSendAck;
              end else if bPacketNum <> bNextSeq then begin
                {Out-of-sequence error...}
                if bPacketNum <> IncSequence(bNextSeq) then begin
                  {...and not a possible SA packet after error}
                  apProtocolError(P, ecSequenceError);
                  aForceStatus := True;
                  bPacketState := psGetDLE;
                end else
                  bPacketState := psGetDLE;
              end else begin
                aProtocolStatus := psOk;
                aForceStatus := True;
                bPacketState := psSuccess;
              end;
            end;

          psError :
            begin
              Inc(aTotalErrors);
              Inc(aBlockErrors);
              if (aBlockErrors > BPErrorMax) then begin
                bpCollectPacket := True;
                Exit;
              end;
              if not bNAKSent or not bBPlusMode then begin
                bNAKSent := True;
                bpSendNAK(P);
              end;
              bPacketState := psGetDLE;
            end;

          psSuccess :
            begin
              if not bAborting then
                bSeqNum := bPacketNum;
              bResumeFlag := False;
              bRSize := bIdx;
              bpCollectPacket := True;
              Exit;
            end;

          psSendAck :
            begin
              if not bAborting then
                bpSendAck(P);
              bPacketState := psGetDLE;
            end;
        end;

        {Stay or exit?}
        case bPacketState of
          psCheckCheck, psSuccess, psError, psSendAck :
            Finished := False;
          else
            Finished := True;
        end;
      until Finished;
    end;
  end;

  function bpCollectAck(P : PProtocolData) : Bool;
    {-Collect acknowledgement to last packet}
  var
    I : Cardinal;
    SAIdx : Integer;
    C : Char;
    Finished : Bool;
  begin
    with P^ do begin
      bpCollectAck := False;

      repeat
        {Restart the character timer with each character}
        if aHC.CharReady and not bAborting then
          NewTimer(aReplyTimer, aHandshakeWait);

        if TimerExpired(aReplyTimer) and (bAckState <> acSendENQ) then
          bAckState := acTimeout;

        case bAckState of
          acGetDLE :
            if aHC.CharReady then begin
              aHC.ValidDispatcher.GetChar(C);
              case C of
                cDLE : bAckState := acGetNum;           {Potential ACK}
                cNAK : bAckState := acSendENQ;          {Packet error}
                cETX : bAckState := acSendNAK;          {Sequence problem}
              end;
            end;

          acGetNum :
            if aHC.CharReady then begin
              aHC.ValidDispatcher.GetChar(C);
              bSaveC := C;
              case C of
                '0'..'9' : bAckState := acHaveAck;     {Have ACK, check it}
                'B'      : if bAborting then
                             bAckState := acSkipPacket1
                           else
                             bAckState := acGetPacket;
                cNak     : bAckState := acSendEnq;
                ';'      : begin
                             aProtocolStatus := psWaitAck;
                             aForceStatus := True;
                             bAckState := acGetDLE;
                           end;
                else       bAckState := acGetDLE;
              end;
            end;

          acGetPacket :
            begin
              {Prepare to collect packet}
              bBPlusState := rbCollectPacket;
              aBlockErrors := 0;
              bPacketState := psGetSeq;
              bAckState := acCollectPacket;
            end;

          acCollectPacket :
            if aHC.CharReady then begin
              if bpCollectPacket(P) then begin
                {Got a complete packet -- finished here}
                if aProtocolStatus = psOK then begin
                  aBlockErrors := 0;
                  bpCollectAck := True;
                end else
                  {Error getting packet, retry}
                  bAckState := acGetDLE;
              end;
            end else if TimerExpired(aReplyTimer) then
              bAckState := acGetDLE;

          acSkipPacket1 :
            if aHC.CharReady then begin
              aHC.ValidDispatcher.GetChar(C);
              if C = cETX then begin
                bpGetCharQuoted(P, C);
                if bQuotePending then
                  bAckState := acSkipPacket2
                else if aCheckType = bcCrc16 then
                  bAckState := acSkipPacket3
                else
                  bAckState := acGetDLE;
              end;
            end;

          acSkipPacket2 : {Collect 2nd byte of 1st check byte}
            if aHC.CharReady then begin
              bpGetCharQuoted(P, C);
              bAckState := acSkipPacket3;
            end;

          acSkipPacket3 : {Collect 2nd check byte}
            if aHC.CharReady then begin
              bpGetCharQuoted(P, C);
              if bQuotePending then
                bAckState := acSkipPacket4
              else
                bAckState := acGetDLE;
            end;

          acSkipPacket4 : {Collect 2nd byte of 2st check byte}
            if aHC.CharReady then begin
              bpGetCharQuoted(P, C);
              bAckState := acGetDLE;
            end;

          acHaveACK :
             begin
              bPacketNum := Byte(bSaveC) - Byte('0');
              if Integer(bSBuffer[bNext2ACK].Seq) = bPacketNum then begin 
                {Expected ACK}
                aLastBlockSize := bSBuffer[bNext2ACK].Num;
                bNext2ACK := IncSA(P, bNext2ACK);
                Dec(bSAWaiting);
                if bSAErrors > 0 then
                  Dec(bSAErrors);
                bpCollectACK := True;
                bAckState := acGetDLE;
                Exit;
              end else if (Integer(bSBuffer[IncSA(P, bNext2ACK)].Seq) = bPacketNum) and
                          (bSAWaiting = 2) then begin
                {Missed ACK}
                apProtocolError(P, ecSequenceError);
                Dec(bSAWaiting, 2);

                {Inc twice to skip the miss}
                bNext2ACK := IncSA(P, bNext2ACK);
                bNext2ACK := IncSA(P, bNext2ACK);
                if bSAErrors > 0 then
                  Dec(bSAErrors);
                bpCollectACK := True;
                bAckState := acGetDLE;
                Exit;
              end else if Integer(bSBuffer[bNext2ACK].Seq) = IncSequence(bPacketNum) then begin
                if bSentENQ then
                  {Remote missed first packet}
                  bAckState := acSendData
                else
                  {Duplicate ACK}
                  bAckState := acGetDLE;
              end else begin
                if bAborting then
                  bAckState := acGetDLE
                else
                  bAckState := acTimeout;
              end;
              bSentENQ := False;
            end;

          acTimeout :
            begin
              apProtocolError(P, ecTimeout);
              aForceStatus := True;
              bAckState := acSendENQ;
              NewTimer(aReplyTimer, aHandshakeWait);
            end;

          acSendNAK :
            begin
              Inc(aBlockErrors);
              Inc(aTotalErrors);
              if (aBlockErrors > BPErrorMax) or bAborting then begin
                bpCollectAck := True;
                Exit;
              end;
              bpSendNAK(P);
              bAckState := acGetDLE;
            end;

          acResync1 :
            if aHC.CharReady then begin
              aHC.ValidDispatcher.GetChar(C);
              if C = cDLE then
                bAckState := acResync2
            end;

          acResync2 :
            if aHC.CharReady then begin
              aHC.ValidDispatcher.GetChar(C);
              case C of
                'B' : if bAborting then
                        bAckState := acSkipPacket1
                      else
                        bAckState := acGetPacket;
                '0'..'9' : bAckState := acResync3;
              end;
            end;

          acResync3 :
            if aHC.CharReady then begin
              aHC.ValidDispatcher.GetChar(C);
              if C = cDLE then
                bAckState := acResync4
            end;

          acResync4 :
            if aHC.CharReady then begin
              aHC.ValidDispatcher.GetChar(C);
              case C of
                'B' : if bAborting then
                        bAckState := acSkipPacket1
                      else
                        bAckState := acGetPacket;
                '0'..'9' : bAckState := acHaveAck;
              end;
            end;

          acSendENQ :
            begin
              Inc(aBlockErrors);
              Inc(aTotalErrors);
              if (aBlockErrors > BPErrorMax) or bAborting then begin
                apProtocolError(P, ecTooManyErrors);
                bpCollectACK := True;
                Exit;
              end;

              SendByte(P, cENQ);
              SendByte(P, cENQ);
              bAckState := acResync1;
              bSentENQ := True;
            end;

          acSendData :
            begin
              Inc(bSAErrors, 3);
              if bSAErrors >= 12 then
                {If too many SA errors, cease SendAhead}
                bSAMax := 1;

              {Flush all pending packets to send}
              SAIdx := bNext2ACK;
              for I := 1 to bSAWaiting do begin
                bpSendData(P, SAIdx);
                SAIdx := IncSA(P, SAIdx);
              end;
              bSentENQ := False;
              bAckState := acGetDLE;
            end;

          acFailed :
            begin
              bpCollectAck := True;
              bAckState := acGetDLE;
            end;
        end;

        {Stay or exit}
        case bAckState of
          acGetPacket,
          acHaveAck,
          acTimeout,
          acSendNak,
          acSendEnq,
          acSendData,
          acFailed : Finished := False;
          else       Finished := True;
        end;
      until Finished;
    end;
  end;

  procedure bpPrepareWriting(P : PProtocolData);
    {-Opens a file to receive, handles resume/overwrite request}
  label
    ExitPoint;
  var
    {$IFDEF Win32}
    Res    : DWORD;
    {$ELSE}
    Res    : Integer;
    {$ENDIF}                                                       
    OvrW   : Bool;
    ET     : EventTimer;
    I      : Integer;
    F      : LongInt;
    S      : string[fsPathname];
    Dir    : string[fsDirectory];
    Name   : string[fsName];
  begin
    with P^ do begin
      {Allocate a file buffer}
      aFileBuffer := AllocMem(FileBufferSize);

      {Inits}
      bResumeFlag := False;
      aFileOpen := False;
      OvrW := False;

      {Does the file exist already?}
      Assign(aWorkFile, aPathName);
      Reset(aWorkFile, 1);
      Res := IOResult;

      {Exit on errors other than FileNotFound}
      if (Res <> 0) and (Res <> 2) then begin
        apProtocolError(P, Res);
        goto ExitPoint;
      end;

      {If file exists process potential resume}
      if Res = 0 then begin
        {$IFDEF Win32}
        aWriteFailOpt := Cardinal(SendMessageTimeout(
                              aHWindow, apw_ProtocolResume,
                              aWriteFailOpt, LongInt(P),
                              SMTO_ABORTIFHUNG + SMTO_BLOCK,
                              1000, Res));
        {$ELSE}
        aWriteFailOpt := Cardinal(SendMessage(aHWindow, apw_ProtocolResume,
                                         aWriteFailOpt, LongInt(P)));
        {$ENDIF}
        case aWriteFailOpt of
          wfcWriteFail :
            begin
              aProtocolStatus := psCantWriteFile;
              aProtocolError := psCantWriteFile;
              aForceStatus := True;
              goto ExitPoint;
            end;
          wfcWriteResume :
            bResumeFlag := True;
          wfcWriteNone,
          wfcWriteRename :
            aProtocolStatus := psFileRenamed;
          wfcWriteAnyway :
            OvrW := True;
        end;
      end;

      if bResumeFlag then begin
        {Calculate CRC on existing file's contents}
        aProtocolStatus := psTryResume;
        apShowStatus(P, 0);
        NewTimer(ET, 1);
        F := FileSize(aWorkFile);

        with bSBuffer[bNext2Fill] do begin
          Seek(aWorkFile, 0);
          bChecksum := $FFFF;
          repeat
            BlockRead(aWorkFile,Buf^[1], 512, Res);
            for I := 1 to (Res) do
              UpdateBlockCheck(P, Byte(Buf^[I]));
            if ElapsedTimeInSecs(ET) >= 10 then begin
              {Send WACK so host knows we're busy}
              NewTimer(ET, 1);
              SendByte(P, cDLE);
              SendByte(P, ';');
              aProtocolStatus := psTryResume;
              apShowStatus(P, 0);
            end;
          until (Res = 0) or (IOResult <> 0);

          {Send the host a "Tr" packet with our info}
          FillChar(Buf^, SizeOf(Buf^), 0);
          Buf^[1] := 'r';

          {Send filesize and CRC}
          S := IntToStr(F) + ' ' + IntToStr(bChecksum) + ' ';
          Move(S[1], Buf^[2], Length(S));
          bpSendPacket(P, 'T', Length(S)+1);

          {Ack will get collected by next state in ProcessDLE}

          {Assume resuming....}
          aProtocolStatus := psHostResume;
          apShowStatus(P, 0);
          aFileOfs := F;
          aBytesTransferred := F;
          aBytesRemaining := aSrcFileLen - aBytesTransferred;
          aInitFilePos := F;
          aStartOfs := F;
          aLastOfs := F;
          aEndOfs := aStartOfs + FileBufferSize;

          Seek(aWorkFile, F);
          aSaveError := IoResult;
          if aSaveError <> 0 then begin
            apProtocolError(P, aSaveError);
            goto ExitPoint;
          end;
          aFileOpen := True;
          Exit;
        end;
      end else begin
        Close(aWorkFile);
        if IOResult = 0 then ;

        {Change the file name if needed}
        if (Res = 0) and not bResumeFlag and not OvrW then begin
          S := StrPas(aPathName);
          Dir := ExtractFilePath(S);
          Name := ExtractFileName(S);
          Name[1] := '$';
          S := Dir + Name;
          StrPCopy(aPathName, S);
        end;

        {Give status a chance to show that the file was renamed}
        apShowStatus(P, 0);

        {Ok to rewrite file now}
        Assign(aWorkFile, aPathname);
        Rewrite(aWorkFile, 1);
        Res := IOResult;
        if Res <> 0 then begin
          apProtocolError(P, Res);
          goto ExitPoint;
        end;

        {Acknowledge the T packet}
        bpSendAck(P);

        {Initialized the buffer management vars}
        aInitFilePos := 0;
        aBytesTransferred := 0;
        aBytesRemaining := 0;
        aFileOfs := 0;
        aStartOfs := 0;
        aLastOfs := 0;
        aEndOfs := aStartOfs + FileBufferSize;
        aFileOpen := True;
        Exit;
      end;

ExitPoint:
      Close(aWorkFile);
      if IOResult <> 0 then ;
    end;
  end;

  procedure bpInitData(P : PProtocolData);
    {-Allocates and initializes a protocol control block with options}
  begin
    with P^ do begin
      aCurProtocol := BPlus;
      aCheckType := bcChecksum1;
      if aActCPS = 0 then
        DefBS := 8
      else case aActCPS of
        0..30   : DefBS := 1;
        31..120 : DefBS := 4;
        else      DefBS := 16;
      end;

      aFinishWait := BPDefFinishWait;
      aHandshakeWait := BPTimeoutMax;
      bQuotePending := False;
      bSentENQ := False;
      aTurnDelay := BPlusTurnDelay;
      aOverhead := BPlusOverHead;
      apResetReadWriteHooks(P);
      apPrepareWriting := bpPrepareWriting;
    end;
  end;

  function bpInit(var P : PProtocolData; H : TApdCustomComPort;
                  Options : Cardinal) : Integer;
    {-Allocates and initializes a protocol control block with options}
  var
    I : Cardinal;
  begin
    {Check for adequate output buffer size}
    if H.OutBuffUsed + H.OutBuffFree < 1024 then begin
      bpInit := ecOutputBufferTooSmall;
      Exit;
    end;

    {Allocate the protocol data record}
    if apInitProtocolData(P, H, Options) <> ecOK then begin
      bpInit := ecOutOfMemory;
      Exit;
    end;

    with P^ do begin
      aCurProtocol := BPlus;
      aCheckType := bcChecksum1;
      case aActCPS of
        0..30   : DefBS := 1;
        31..120 : DefBS := 4;
        else      DefBS := 16;
      end;

      bpInitData(P);

      {Allocate buffers}
      bRBuffer := AllocMem(BPBufferMax);
      I := 1;
      while (I <= BPSendAheadMax) do begin
        bSBuffer[I].Buf := Allocmem(BPBufferMax);
        Inc(I);
      end;
      bpInit := ecOK;
    end;
  end;

  function bpReinit(P : PProtocolData) : Integer;
    {-Allocates and initializes a protocol control block with options}
  var
    I : Cardinal;
  begin
    with P^ do begin
      aCurProtocol := BPlus;
      aCheckType := bcChecksum1;
      case aActCPS of
        0..30   : DefBS := 1;
        31..120 : DefBS := 4;
        else      DefBS := 16;
      end;

      bpInitData(P);

      {Allocate buffers}
      bRBuffer := AllocMem(BPBufferMax);
      I := 1;
      while (I <= BPSendAheadMax) do begin
        bSBuffer[I].Buf := AllocMem(BPBufferMax);
        Inc(I);
      end;
      bpReinit := ecOK;
    end;
  end;

  procedure bpDonePart(P : PProtocolData);
    {-Destroy the protocol object}
  var
    I : Cardinal;
  begin
    with P^ do begin
      for I := 1 to BPSendAheadMax do
        FreeMem(bSBuffer[I].Buf, BPBufferMax);
      FreeMem(bRBuffer, BPBufferMax);
    end;
  end;

  procedure bpDone(var P : PProtocolData);
    {-Destroy the protocol object}
  begin
    bpDonePart(P);
    apDoneProtocol(P);
  end;

  procedure bpUpdateQuoteTable(P : PProtocolData; QS : TQuoteArray);
    {-Update our bQuoteTable to match the QS quotearray}
  var
    I,J,K : Integer;
    B,C : Byte;
  begin
    with P^ do begin

      K := 0;
      C := $40;

      for I := 0 to 7 do begin
        if I = 4 then begin
          K := 128;
          C := $60;
        end;
        B := QS[I];

        for J := 0 to 7 do begin
          if (B and $80) <> 0 then
            bQuoteTable[K] := Char(C);
          B := B shl 1;
          Inc(C);
          Inc(K);
        end;
      end;
    end;
  end;

  procedure bpInitVars(P : PProtocolData);
    {-Init vars that need resetting each time a DLE is seen}
  begin
    with P^ do begin
      bNext2ACK  := 1;
      bNext2Fill := 1;
      bSAWaiting := 0;
      bSAMax     := 1;
      bAbortCount:= 0;
      aTotalErrors := 0;
      bResumeFlag := False;
    end;
  end;

  procedure bpResetProtocol(P : PProtocolData);
    {-Init important session-dependant protocol vars}
  begin
    with P^ do begin
      bSeqNum := 0;
      bSAMax := 1;
      bSAErrors := 0;
      aBlockLen := 512;
      bAbortCount := 0;
      bBPlusMode := False;
      aCheckType := bcChecksum1;
      FillChar(bQuoteTable, SizeOf(bQuoteTable), 0);
      FillChar(bOurParams, SizeOf(bOurParams), 0);
      bOurParams.BlkSize := 4;
      bOurParams.QuoteSet := DQDefault;
      bpUpdateQuoteTable(P, DQDefault);
    end;
  end;

  procedure bpSendTransportParams(P : PProtocolData);
    {-Send our params, collect ack}
  begin
    with P^ do begin
      {Some inits}
      bOurParams.QuoteSet := DQDefault;
      FillChar(bRBuffer^[bRSize+1], SizeOf(bRBuffer^)-bRSize, 0);

      {Save the host's params}
      Move(bRBuffer^[1], bHostParams.WinSend, 4);
      Move(bRBuffer^[7], bHostParams.QuoteSet, 11);

      {Send '+' packet under FULL quoting}
      bQSP := (bRSize >= 14);
      bpUpdateQuoteTable(P, DQFull);

      {Fill outgoing buffer}
      with bSBuffer[bNext2Fill] do begin
        Buf^[1] := Char(DefWS);
        Buf^[2] := Char(DefWR);
        Buf^[3] := Char(DefBS);
        Buf^[4] := Char(DefCM);
        Buf^[5] := Char(DefDQ);
        Buf^[6] := Char(DefXP);
        Move(bOurParams.QuoteSet, Buf^[7], 8);
        Buf^[15] := Char(DefDR);
        Buf^[16] := Char(DefUR);
        Buf^[17] := Char(DefFI);
      end;

      {Send the transport packet}
      bpSendPacket(P, '+', 17);
    end;
  end;

  procedure bpProcessTransportParams(P : PProtocolData);
    {-Process received "+" packet, send our params}
  begin
    with P^ do begin
      {Make a minimal set of parameters to work from}
      if bHostParams.WinSend < DefWR then
        bOurParams.WinSend := bHostParams.WinSend
      else
        bOurParams.WinSend := DefWR;

      {If > 0, we can use all windows}
      if bOurParams.WinSend <> 0 then
        bSAMax := BPSendAheadMax;

      if bHostParams.WinRecv < DefWS then
        bOurParams.WinRecv := bHostParams.WinRecv
      else
        bOurParams.WinRecv := DefWS;

      if bHostParams.BlkSize < DefBS then
        bOurParams.BlkSize := bHostParams.BlkSize
      else
        bOurParams.BlkSize := DefBS;

      if bOurParams.BlkSize = 0 then
        bOurParams.BlkSize := 4;
      aBlockLen := (bOurParams.BlkSize * 128);

      if bHostParams.ChkType < DefCM then
        bOurParams.ChkType := bHostParams.ChkType
      else
        bOurParams.ChkType := DefCM;

      {If = 1, we need CRC blockchecking}
      if bOurParams.ChkType > 0 then
        aCheckType := bcCrc16;

      if bHostParams.DROpt < DefDR then
        bOurParams.DROpt := bHostParams.DROpt
      else
        bOurParams.DROpt := DefDR;

      bOurParams.UROpt := DefUR;

      if bHostParams.FIOpt < DefFI then
        bOurParams.FIOpt := bHostParams.FIOpt
      else
        bOurParams.FIOpt := DefFI;

      FillChar(bQuoteTable, SizeOf(bQuoteTable), 0);
      bpUpdateQuoteTable(P, bOurParams.QuoteSet);
      if bQSP then
        bpUpdateQuoteTable(P, bHostParams.QuoteSet);
      bBPlusMode := True;
    end;
  end;

  procedure bpProcessFileTransferParams(P : PProtocolData);
    {-Extract Tranfer parameters}
  var
    I : Integer;                                                    
  begin
    with P^ do begin
      {Note bDirection}
      case bRBuffer^[1] of
        'D' : bDirection := dDownload;
        'U' : bDirection := dUpload;
        else begin
          bpSendFailure(P, 'NUnimplemented Transfer Function');
          apProtocolError(P, ecProtocolError);
        end;
      end;

      {Start timer now...}
      NewTimer(aTimer, 1);

      {Verify file type}
      if (bRBuffer^[2] <> 'A') and (bRBuffer^[2] <> 'B') then begin
        bpSendFailure(P, 'NUnimplemented File Type');
        apProtocolError(P, ecProtocolError);
      end;

      {Retrieve pathname}
      I := 2;
      while (bRBuffer^[I] <> #0) and
            (I < bRSize-1) and
            (I < SizeOf(TPathCharArray)) do begin
        Inc(I);
        if aUpcaseFileNames then
          aPathName[I-3] := Upcase(bRBuffer^[I]);
      end;
      aPathname[I-2] := #0;

      case bDirection of
        dUpload :
          begin
            apLogFile(P, lfTransmitStart);

            {Prepare to read file}
            apPrepareReading(P);
            if aProtocolError <> ecOK then begin
              {Send failure, ProcessDLE will collect ACK}
              bpSendFailure(P, 'AFile Error');
              apLogFile(P, lfTransmitFail);
              Exit;
            end;
            aFileOfs := 0;
          end;

        dDownLoad :
          begin
            apLogFile(P, lfReceiveStart);
            if not apAcceptFile(P, aPathname) then begin
              aProtocolStatus := psFileRejected;
              aForceStatus := True;

              {Send failure packet, ProcessDLE will collect ACK}
              bpSendFailure(P, 'AFile rejected');
              Exit;
            end;

            {Prepare to write file}
            apPrepareWriting(P);
            if (aProtocolStatus = psCantWriteFile) or
               (aProtocolError <> ecOK) then begin
              {Send failure packet, ProcessDLE will collect ACK}
              bpSendFailure(P, 'AAborted by user');
              apLogFile(P, lfReceiveFail);
              Exit;
            end;
          end;
      end;
    end;
  end;

  function bpProcessENQ(P : PProtocolData) : Integer;
    {-Called when the terminal handler receives an <ENQ>}
  begin
    with P^ do begin
      if aCurProtocol <> BPlus then
        bpProcessENQ := ecBadProtocolFunction
      else begin
        bAborting := False;
        bpResetProtocol(P);
        bpProcessENQ := aHC.ValidDispatcher.PutString(cDLE+'++'+cDLE+'0');
      end;
    end;
  end;

  function bpProcessESCI(P : PProtocolData; X, Y : Byte) : Integer;
    {-Called by terminal handler when <ESC><'I'> seen at port}
  var
    S : String;
    T : String[5];
    I : Integer;
  begin
    with P^ do begin
      if aCurProtocol <> BPlus then begin
        bpProcessESCI := ecBadProtocolFunction;
        Exit;
      end;

      S := ESCIResponse;

      {Make sure tailer is in place for later}
      if Pos(',+',S) = 0 then
        S := S + ',+';

      {If 'SSxx' part of string, insert screen size values}
      I := Pos('SSxx',S);
      if I > 0 then begin
        S[I+2] := Chr(Y+31);
        S[I+3] := Chr(X+31);
      end;

      {Build the string's bChecksum and append it to the string}
      X := 0;
      for I := 1 to Length(S) do
        Inc(X, Ord(S[I]));
      Str(X, T);
      S := S + T;

      {Send the response}
      bpProcessESCI := aHC.ValidDispatcher.PutString(S+^M);
    end;
  end;

  function bpPrepareProcessDLE(P : PProtocolData;
                               var ATimerIndex : Cardinal) : Integer;
    {-Start <DLE> processing, returns timer index}
  begin
    with P^ do begin
      if aCurProtocol <> BPlus then begin
        bpPrepareProcessDLE := ecBadProtocolFunction;
        Exit;
      end;

      aProtocolStatus := psOK;
      aProtocolError := ecOK;
      bpPrepareProcessDLE := ecOK;
      bTermState := tpsWaitB;
      bTimerIndex := aHC.AddTimerTrigger;
      ATimerIndex := bTimerIndex;
      if bTimerIndex > 0 then begin
        aHC.SetTimerTrigger(bTimerIndex, aHandshakeWait, True);
        bCurTimer := bTimerIndex;
      end else
        bpPrepareProcessDLE := ATimerIndex;
    end;
  end;

  function bpProcessDLE(P : PProtocolData; IsData : Bool;
                        var Ready, Start, Upload : Bool) : Integer;
    {-Collects packets from terminal mode, return Ready True when complete}
  var
    Finished : Bool;
    Complete : Bool;
    C : Char;

  {$IFDEF VER90}
    {$DEFINE VER93}
  {$ENDIF}                                                  
  {$IFDEF VER93}
  procedure CheckTermState;
  begin
    case P^.bTermState of
    tpsError : Finished := False;
    else       if not Finished then
                 Finished := not P^.aHC.CharReady;
    end;
  end;
  {$ENDIF}

  begin
    with P^ do begin
      if aCurProtocol <> BPlus then begin
        bpProcessDLE := ecBadProtocolFunction;
        Exit;
      end;

      bpProcessDLE := ecOK;
      Start := False;
      Ready := False;

      {If data not ready this must be a timeout, set error state}
      if not IsData then
        bTermState := tpsError;

      {Process characters and timeouts}
      Finished := False;
      Complete := False;
      repeat
        case bTermState of
          tpsWaitB :
            begin
              aHC.ValidDispatcher.GetChar(C);
              case C of
                cDLE : {ignore} ;
                'B' : begin
                        aHC.SetTimerTrigger(bTimerIndex, aHandshakeWait, True);
                        bTermState := tpsWaitSeq;
                      end;
                else  bTermState := tpsError;
              end;
            end;

          tpsWaitSeq :
            begin
              {Get sequence byte...}
              aHC.ValidDispatcher.GetChar(C);
              if aCheckType = bcCrc16 then
                bChecksum := $FFFF
              else
                bChecksum := 0;
              UpdateBlockCheck(P, Byte(C));
              bTermState := tpsWaitType;
              bPacketNum := Ord(C)-Ord('0');
            end;

          tpsWaitType :
            begin
              aHC.ValidDispatcher.GetChar(C);
              case C of
                '+' :
                  begin
                    {Prepare to collect + packet}
                    bTermState := tpsCollectPlus;
                    bPacketState := psGetData;
                    bpInitVars(P);
                    bNextSeq := IncSequence(bSeqNum);
                    bIdx := 1;
                    UpdateBlockCheck(P, Byte('+'));
                  end;
                'T' :
                  begin
                    {Prepare to collect T packet}
                    bTermState := tpsCollectT;
                    bPacketState := psGetData;
                    bNextSeq := IncSequence(bSeqNum);
                    bIdx := 1;
                    UpdateBlockCheck(P, Byte('T'));
                  end;
                else
                  bTermState := tpsError;
              end;
            end;

          tpsCollectPlus :
            {Collect and process + packet, send our options}
            if bpCollectPacket(P) then begin
              {Got host options, send ours, prepare to wait for ACK}
              bpSendTransportParams(P);
              bTermState := tpsCollectAckPlus;
              bAckState := acGetDLE;
            end;

          tpsCollectAckPlus :
            if bpCollectAck(P) then begin
              {Got the ack from our params, now compare host's and ours}
              bpProcessTransportParams(P);
              Complete := True;
            end;

          tpsCollectT :
            if bpCollectPacket(P) then begin
              bpProcessFileTransferParams(P);
              if aProtocolError = ecOK then begin
                Complete := True;
                Start := True;
                Upload := bDirection = dUpload;
              end else begin
                Complete := True;
                bAckState := acGetDLE;
                bTermState := tpsCollectAckT;
              end;
            end;

          tpsCollectAckT :
            if bpCollectAck(P) then begin
              {Finished collecting failure ack, we're done}
              Complete := True;
              if aProtocolStatus = psHostResume then begin
                Start := True;
                Upload := bDirection = dUpload;
              end else
                bpProcessDLE := aProtocolError;
            end;

          tpsError :
            begin
              {Timeout getting char or unknown packet type}
              aHC.RemoveTrigger(bTimerIndex);
              Complete := True;
            end;
        end;

        {$IFDEF Ver93}
        CheckTermState;
        {$ELSE}
        {Stay in state machine if more data available}
        case bTermState of
          tpsError : Finished := False;
          else       if not Finished then
                       Finished := not aHC.CharReady;
        end;
        {$ENDIF}
      until Finished or Complete;

      if Complete then begin
        bTimerIndex := aTimeoutTrigger;
        Ready := True;
      end;
    end;
  end;

  {$IFDEF VER90}
    {$UNDEF VER93}
  {$ENDIF}            

  procedure bpHandleResumeFail(P : PProtocolData);
    {-Resume bFailed, rewrite the file}
  var
    Result : Cardinal;
    S      : string[fsPathname];
    Dir    : string[fsDirectory];
    Name   : string[fsName];
  begin
    with P^ do begin
      Close(aWorkFile);

      {If we default to Rename, rename the file}
      if aWriteFailOpt = wfcWriteRename then begin
        S := StrPas(aPathName);
        Dir := ExtractFilePath(S);
        Name := ExtractFileName(S);
        Name[1] := '$';
        S := Dir + Name;
        StrPCopy(aPathName, S);
        Assign(aWorkFile, aPathName);
        aProtocolStatus := psFileRenamed;
      end;

      {Otherwise just overwrite}
      Rewrite(aWorkFile, 1);
      Result := IoResult;
      if Result <> 0 then begin
        aFileOpen := False;
        apProtocolError(P, Result);
        apShowStatus(P, 0);
        bpSendFailure(P, 'CCannot create file');
        Exit;
      end;

      {Set status vars}
      aBytesTransferred := 0;
      aProtocolStatus := psResumeBad;
      apShowStatus(P, 0);
      bResumeFlag := False;
      bpSendAck(P);
      NewTimer(aTimer, 1);
      aFileOfs := 0;
      aInitFilePos := 0;
    end;
  end;

  procedure bpPrepareTransmit(P : PProtocolData);
    {-Prepare for a B+ transmit}
  begin
    with P^ do begin
      {Inits}
      bBPlusState := tbInitial;
      aProtocolError := ecOK;
      aSaveStatus := psOK;
      aSaveError := ecOK;

      {Reset status but make sure filesize doesn't get changed}
      Inc(aInProgress);
      apResetStatus(P);
      Dec(aInProgress);

      {bpCollectPacket should now use aTimeoutTrigger for timer}
      bCurTimer := aTimeoutTrigger;
    end;
  end;

  procedure bpTransmit(Msg, wParam : Cardinal;
                      lParam : LongInt);
    {-Perform one increment of a protocol transmit}
  var
    TriggerID   : Cardinal absolute wParam;
    P           : PProtocolData;
    Finished    : Bool;
    StatusTicks : LongInt;
    Dispatcher      : TApdBaseDispatcher;
  begin
    {Get the protocol pointer from data pointer 1}
    Dispatcher := TApdBaseDispatcher(PortList[LH(lParam).H]);
    with Dispatcher do
      GetDataPointer(Pointer(P), 1);

    with P^ do begin
      {$IFDEF Win32}
      EnterCriticalSection(aProtSection);

      {Exit if protocol was cancelled while waiting for crit section}
      if bBPlusState = tbDone then begin
        LeaveCriticalSection(aProtSection);
        Exit;
      end;
      {$ENDIF}

      {Force TriggerID for TriggerAvail messages}
      if Msg = apw_TriggerAvail then
        TriggerID := aDataTrigger;

      repeat

        {Restore last status}
        aProtocolStatus := aSaveStatus;

        case aSaveStatus of
          psCancelRequested,
          psFileRejected : ;
          else begin
            if Msg = apw_ProtocolCancel then begin
              if bBPlusState = tbWaitErrorAck then
                bBPlusState := tbCleanup
              else begin
                {Send failure packet}
                bpSendFailure(P, 'AAborted by user');
                aProtocolStatus := psCancelRequested;
                aForceStatus := True;
                bBPlusState := tbError;
              end;
              aForceStatus := True;
            end else if Integer(TriggerID) = aNoCarrierTrigger then begin 
              bBPlusState := tbCleanup;
              aProtocolStatus := psAbortNoCarrier;
            end;
          end;
        end;

        {Show status at requested intervals and after significant events}
        if aForceStatus or (Integer(TriggerID) = aStatusTrigger) then begin
          if aSaveError <> ecOK then
            aProtocolError := aSaveError;
          aElapsedTicks := ElapsedTime(aTimer);
          if Dispatcher.TimerTicksRemaining(aStatusTrigger,
                                  StatusTicks) <> 0 then
            StatusTicks := 0;
          if StatusTicks <= 0 then begin                            
            apShowStatus(P, 0);
            Dispatcher.SetTimerTrigger(aStatusTrigger, aStatusInterval, True);
            aForceStatus := False;
          end;
          if Integer(TriggerID) = aStatusTrigger then begin          
            {$IFDEF Win32}
            LeaveCriticalSection(P^.aProtSection);
            {$ENDIF}
            Exit;
          end;
        end;

        {Main state processor}
        case bBPlusState of
          tbInitial :
            begin
              apShowFirstStatus(P);
              Dispatcher.SetTimerTrigger(aTimeoutTrigger, aHandshakeWait, True);
              bBPlusState := tbGetBlock;
              aLastBlock := False;
            end;

          tbGetBlock :
            if aLastBlock then
              bBPlusState := tbEndOfFile
            else with bSBuffer[bNext2Fill] do begin
              if FlagIsSet(aFlags, apBP2KTransmit) then
                Num := aBlockLen
              else
                Num := 1024;
              aLastBlock := apReadProtocolBlock(P, PDataBlock(Buf)^, Num);
              if aProtocolError = ecOK then begin
                Inc(aFileOfs, Num);
                bBPlusState := tbWaitFreeSpace;
                Dispatcher.SetTimerTrigger(aTimeoutTrigger, aTransTimeout, True);
                Dispatcher.SetStatusTrigger(aOutBuffFreeTrigger, (Num*2)+10, True);
              end else begin
                bpSendFailure(P, 'EFile read failure');
                bBPlusState := tbError;
              end;
            end;

          tbWaitFreeSpace :
            if Integer(TriggerID) = aOutBuffFreeTrigger then
              bBPlusState := tbSendData
            else if Integer(TriggerID) = aTimeoutTrigger then begin 
              bpSendFailure(P, 'ETimeout waiting for output buffer space');
              bBPlusState := tbError;
            end else if TriggerID = aDataTrigger then begin
              if bpCollectAck(P) then begin
                if aProtocolError <> ecOK then begin
                  aForceStatus := True;
                  bpSendFailure(P, 'EToo many errors');
                  bBPlusState := tbError;
                end;
              end;
            end;

          tbSendData :
            with bSBuffer[bNext2Fill] do begin
              bpSendPacket(P, 'N', Num);
              aForceStatus := True;

              {if bSAWaiting = 1 then begin}
              Dispatcher.SetTimerTrigger(aTimeoutTrigger,
                               aHandshakeWait, True);
              bAckState := acGetDLE;
              {end;}
              bBPlusState := tbCheckAck;
            end;

          tbCheckAck :
            if (bSAWaiting < bSAMax) and not Dispatcher.CharReady then
              bBPlusState := tbGetBlock
            else if bpCollectAck(P) then begin
              if aProtocolError = ecOK then begin
                bBPlusState := tbGetBlock;
                Inc(aBytesTransferred, aLastBlockSize);
                Dec(aBytesRemaining, aLastBlockSize);
              end else begin
                aForceStatus := True;
                bpSendFailure(P, 'EToo many errors');
                bBPlusState := tbError;
              end;
            end;

          tbEndOfFile :
            begin
              {Send TransferComplete packet}
              with bSBuffer[bNext2Fill] do begin
                Buf^[1] := 'C';
                bpSendPacket(P, 'T', 1);
                Dispatcher.SetTimerTrigger(aTimeoutTrigger, aHandshakeWait, True);
                bBPlusState := tbEofAck;
              end;
              {if bSAWaiting = 0 then begin}
              Dispatcher.SetTimerTrigger(aTimeoutTrigger,
                               aHandshakeWait, True);
              bAckState := acGetDLE;
              {end;}
            end;

          tbEofAck :
            if bpCollectAck(P) then
              if aProtocolError = ecOK then begin
                bBPlusState := tbCleanup;
                aForceStatus := True;
              end else begin
                bpSendFailure(P, 'EToo many errors');
                aForceStatus := True;
                bBPlusState := tbError;
              end;

          tbError :
            begin
              {Save failure status}
              aSaveError := aProtocolError;

              {Start waiting for acknowledgment (failure packet already sent)}
              bBPlusState := tbWaitErrorAck;
              bAckState := acGetDLE;
              Dispatcher.SetTimerTrigger(aTimeoutTrigger, aFinishWait, True);
            end;

          tbWaitErrorAck :
            if bpCollectAck(P) then begin
              aProtocolError := aSaveError;
              aForceStatus := True;
              bBPlusState := tbCleanup;
            end;

          tbCleanup :
            begin
              apFinishReading(P);

              {Log file}
              if aProtocolError = ecOK then
                apLogFile(P, lfTransmitOK)
              else
                apLogFile(P, lfTransmitFail);

              apShowLastStatus(P);
              Dispatcher.FlushInBuffer;
              bBPlusState := tbDone;
              apSignalFinish(P);
            end;
        end;

        {Stay in state machine or exit}
        case bBPlusState of
          {Stay only if data ready}
          tbCheckAck,
          tbWaitErrorAck,
          tbEofAck        : Finished := not Dispatcher.CharReady;

          {Stay because there is more work to do}
          tbInitial,
          tbGetBlock,
          tbSendData,
          tbEndOfFile,
          tbError,
          tbCleanup       : Finished := False;

          {Exit, waiting for new trigger}
          tbWaitFreeSpace : Finished := not Dispatcher.CharReady;

          {Done state, always exit}
          tbDone          : Finished := True;
          else              Finished := True;
        end;

        {Store aProtocolStatus}
        aSaveStatus := aProtocolStatus;

        {If staying is state machine force data ready}
        TriggerID := aDataTrigger;
      until Finished;
    end;

    {$IFDEF Win32}
    LeaveCriticalSection(P^.aProtSection);
    {$ENDIF}
  end;

  procedure bpPrepareReceive(P : PProtocolData);
    {-Prepare to receive BPlus parts}
  begin
    with P^ do begin
      {Init the state machine}
      bBPlusState := rbInitial;
      aProtocolError := ecOK;
      aSaveStatus := psOK;
      aSaveError := ecOK;

      {bpCollectPacket should now use aTimeoutTrigger for timer}
      bCurTimer := aTimeoutTrigger;
    end;
  end;

  procedure bpReceive(Msg, wParam : Cardinal;
                     lParam : LongInt);
    {-Perform one increment of a protocol receive}
  var
    TriggerID   : Cardinal absolute wParam;
    P           : PProtocolData;
    Finished    : Bool;
    C           : Char;
    I           : Integer;
    SaveSize    : LongInt;
    S           : String;
    StatusTicks : LongInt;
    Dispatcher      : TApdBaseDispatcher;
  begin

    {Get the protocol pointer from data pointer 1}
    Dispatcher := TApdBaseDispatcher(PortList[LH(lParam).H]);
    with Dispatcher do
      GetDataPointer(Pointer(P), 1);

    with P^ do begin
      {$IFDEF Win32}
      EnterCriticalSection(aProtSection);

      {Exit if protocol was cancelled while waiting for crit section}
      if bBPlusState = rbDone then begin
        LeaveCriticalSection(aProtSection);
        Exit;
      end;
      {$ENDIF}

      {Force TriggerID for TriggerAvail messages}
      if Msg = apw_TriggerAvail then
        TriggerID := aDataTrigger;

      repeat

        {Nothing to do if state is rbDone}
        if bBPlusState = rbDone then begin
          {$IFDEF Win32}
          LeaveCriticalSection(aProtSection);
          {$ENDIF}
          Exit;
        end;

        {Restore last status}
        aProtocolStatus := aSaveStatus;

        case aProtocolStatus of
          psCancelRequested,
          psFileRejected : ;
          else begin
            if Msg = apw_ProtocolCancel then begin
              if bBPlusState = rbWaitErrorAck then
                bBPlusState := rbCleanup
              else begin
                {Send failure packet}
                bpSendFailure(P, 'AAborted by user');
                aProtocolStatus := psCancelRequested;
                bBPlusState := rbError;
              end;
              aForceStatus := True;
            end else if Integer(TriggerID) = aNoCarrierTrigger then begin 
              bBPlusState := tbCleanup;
              aProtocolStatus := psAbortNoCarrier;
            end;
          end;
        end;

        {Show status at requested intervals and after significant events}
        if aForceStatus or (Integer(TriggerID) = aStatusTrigger) then begin 
          if aSaveError <> ecOK then
            aProtocolError := aSaveError;
          if Dispatcher.TimerTicksRemaining(aStatusTrigger,
                                  StatusTicks) <> 0 then
            StatusTicks := 0;
          if StatusTicks <= 0 then begin                              
            apShowStatus(P, 0);
            Dispatcher.SetTimerTrigger(aStatusTrigger, aStatusInterval, True);
            aForceStatus := False;
          end;
          if Integer(TriggerID) = aStatusTrigger then begin         
            {$IFDEF Win32}
            LeaveCriticalSection(aProtSection);
            {$ENDIF}
            Exit;
          end;
        end;

        {Main state processor}
        case bBPlusState of
          rbInitial :
            begin
              {apResetStatus(P);}
              aBlockNum := 0;
              aElapsedTicks := 0;
              aBlockErrors := 0;
              aTotalErrors := 0;
              apShowFirstStatus(P);

              {Start waiting for first packet}
              Dispatcher.SetTimerTrigger(aTimeoutTrigger, aHandshakeWait, True);
              aSaveError := ecOK;
              bBPlusState := rbGetDLE;
            end;

          rbGetDLE :
            if TriggerID = aDataTrigger then begin
              Dispatcher.GetChar(C);
              if C = cDLE then
                bBPlusState := rbGetB
            end else if Integer(TriggerID) = aTimeoutTrigger then  
              bBPlusState := rbSendEnq;

          rbGetB :
            if TriggerID = aDataTrigger then begin
              Dispatcher.GetChar(C);
              if C = 'B' then begin
                bBPlusState := rbCollectPacket;
                bNAKSent := False;
                bNextSeq := IncSequence(bSeqNum);
                aBlockErrors := 0;
                bPacketState := psGetSeq;
              end else
                bBPlusState := rbSendEnq;
            end else if Integer(TriggerID) = aTimeoutTrigger then  
              bBPlusState := rbSendEnq;

          rbCollectPacket :
            if TriggerID = aDataTrigger then begin
              if bpCollectPacket(P) then begin
                {Got a complete packet -- process it}
                if aProtocolError = ecOK then begin
                  aBlockErrors := 0;
                  bBPlusState := rbProcessPacket;
                  aForceStatus := True;
                end else begin
                  {Too many errors, let rbSendEnq handle}
                  bBPlusState := rbSendEnq;
                  Dispatcher.SetTimerTrigger(aTimeoutTrigger, aHandshakeWait, True);
                end;
              end;
            end else if Integer(TriggerID) = aTimeoutTrigger then begin
              {Timeout error, let rbSendEnq handle}
              bBPlusState := rbSendEnq;
              Dispatcher.SetTimerTrigger(aTimeoutTrigger, aHandshakeWait, True);
            end;

          rbProcessPacket :
            begin
              aForceStatus := True;
              case bLastType of
                'N':  {Next data packet, write it to file}
                  begin
                    {Call the write method to write this block}
                    bFailed := apWriteProtocolBlock(P,
                               PDataBlock(bRBuffer)^, bRSize-1);

                    {Process result}
                    if bFailed then begin
                      bpSendFailure(P, 'EWrite failure');
                      aForceStatus := True;
                      bBPlusState := rbError;
                    end else begin
                      Inc(aFileOfs, bRSize-1);
                      Dec(aBytesRemaining, bRSize-1);
                      Inc(aBytesTransferred, bRSize-1);
                      aElapsedTicks := ElapsedTime(aTimer);
                      bpSendAck(P);

                      {Prepare to get next packet}
                      bBPlusState := rbGetDLE;
                      NewTimer(aReplyTimer, aHandshakeWait);
                    end;
                  end;

                'T':     {Transfer control packet, process per second byte}
                  begin
                    case bRBuffer^[1] of
                      'C':   {Transfer Complete packet}
                        begin
                          apFinishWriting(P);
                          bpSendAck(P);
                          bBPlusState := rbCleanup;
                        end;

                      'I':   {Transfer Info packet, we only use FileSize field here}
                        begin
                          bpSendAck(P);
                          I := 4;
                          S := '';
                          while (I <= bRSize-1) and
                                (bRBuffer^[I] >= '0') and
                                (bRBuffer^[I] <= '9') do begin
                            S := S + bRBuffer^[I];
                            Inc(I);
                          end;
                          Val(S, aSrcFileLen, I);
                          if I <> 0 then
                            aSrcFileLen := 0;
                          aBytesRemaining :=
                            aSrcFileLen - aBytesTransferred;

                          {Get next packet}
                          bBPlusState := rbGetDLE;
                        end;

                      'f':   {Host bFailed Resume, rewrite the file}
                        begin
                          bpHandleResumeFail(P);
                          bBPlusState := rbGetDLE;
                        end;

                      else begin
                          {Unknown T packet type}
                          apProtocolError(P, ecProtocolError);
                          bpSendFailure(P, 'NInvalid T Packet');
                          bBPlusState := rbError;
                        end;
                    end;
                  end;

                'F':    {Failure packet, exit immediately}
                  begin
                    aProtocolStatus := psCancelRequested;
                    aForceStatus := True;
                    bpSendAck(P);
                    bBPlusState := rbCleanup;
                  end;
               else begin
                    {Unsupported packet type, exit immediately}
                    apProtocolError(P, ecProtocolError);
                    bpSendFailure(P, 'NUnknown packet type');
                    bBPlusState := rbError;
                  end;
              end;
            end;

          rbSendEnq :
            begin
              aProtocolStatus := psTimeout;
              Inc(aBlockErrors);
              Inc(aTotalErrors);
              if aBlockErrors > BPErrorMax then begin
                apProtocolError(P, ecTimeout);
                bpSendFailure(P, 'ATimeout');
                bBPlusState := rbError;
              end else
                bBPlusState := rbGetDLE;
            end;

          rbError :
            begin
              {Save failure status}
              aSaveError := aProtocolError;

              {Start waiting for acknowledgment (failure packet already sent)}
              bBPlusState := rbWaitErrorAck;
              bAckState := acGetDLE;
              NewTimer(aReplyTimer, aFinishWait);
            end;

          rbWaitErrorAck :
            if bpCollectAck(P) then begin
              aProtocolError := aSaveError;
              aForceStatus := True;
              bBPlusState := rbCleanup;
            end;

         rbCleanup :
            begin
              {Close file}
              SaveSize := aSrcFileLen;
              apFinishWriting(P);
              aSrcFileLen := SaveSize;

              {Log receive status}
              if aProtocolError <> ecOK then
                apLogFile(P, lfReceiveFail)
              else
                apLogFile(P, lfReceiveOK);

              apShowLastStatus(P);
              Dispatcher.FlushInBuffer;
              bBPlusState := rbDone;
              apSignalFinish(P);
            end;
        end;

        {Stay in state machine or exit}
        case bBPlusState of
          {Stay in state machine of more data ready}
          rbGetDLE,
          rbGetB,
          rbWaitErrorAck,
          rbCollectPacket      : Finished := not Dispatcher.CharReady;

          {Stay in state machine}
          rbFinished,
          rbCleanup,
          rbProcessPacket,
          rbSendEnq,
          rbError              : Finished := False;

          {Exit state machine to wait for trigger}
          rbInitial            : Finished := True;

          {Done state}
          rbDone               : Finished := True;
          else                   Finished := True;
        end;

        {Store aProtocolStatus}
        aSaveStatus := aProtocolStatus;

        {if TriggerID = 0 then}
        {  Finished := True;  }

        {If staying in state machine force data ready}
        TriggerID := aDataTrigger;
      until Finished;
    end;

    {$IFDEF Win32}
    LeaveCriticalSection(P^.aProtSection);
    {$ENDIF}
  end;

end.
