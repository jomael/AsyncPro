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
{*                   APROREG.PAS 4.06                    *}
{*********************************************************}
{* APRO component registration                           *}
{*********************************************************}


{Global defines potentially affecting this unit}
{$I ..\includes\AWDEFINE.INC}

unit APROReg;

interface

uses
  DesignIntf,
  DesignEditors;

{ Constant declarations for IDE palette tab names }

const
  AsyncProfessional_TabBaseName = 'AsyncPro';   // SBD
  APROTabName          = AsyncProfessional_TabBaseName + ' General';
  APROFaxTabName       = AsyncProfessional_TabBaseName + ' Fax';
  APROTelephonyTabName = AsyncProfessional_TabBaseName + ' Telephony';
  APROStateTabName     = AsyncProfessional_TabBaseName + ' State Machine';

procedure Register;

implementation

{$R APROREG.DCR}

uses
  { RTL/VCL Units }
  Classes, SysUtils, Windows,

  { Standard Units }
  AdPort,      {TApdComPort}

// SBD! Add the following units as we upgrade them.

{$IFNDEF UNICODE}
  AdProtcl,    {TApdProtocol}
  AdPStat,     {TApdProtocolStatus}
{$ENDIF}
  AdStatLt,    {TApdSLController, TApdStatusLight}
{$IFNDEF UNICODE}
  AdWnPort,    {TApdWinsockPort}
{$ENDIF}
  AdSelCom,    {port selection dialog}
{$IFNDEF UNICODE}
  AdPacket,    {TApdDataPacket}
{$ENDIF}
  AdPropEd,    {design-time property editors}
{$IFNDEF UNICODE}
  AdPager,     {TApdTapPager, TApdSNPPPager, TApdSMSPager}
  AdPgr,       {TApdPager (replaces TApdTAPPager and TApdSNPPPager)}     {!!.04}
  AdGSM,       {TApdGSMPhone}
  AdFtp,       {TApdFTPClient}
  AdScript,    {TApdScript}
  AdTrmEmu,    {TAdVT100Emulator, TAdTTYEmulator}
  AdRas,       {TApdRasDialer}
  AdRStat,     {TApdRasStatus}
  AdMdm,       {TAdModem}
{$ENDIF}

{$IFNDEF UNICODE}
  {State Machine Units}
  AdStMach,    {TApdStateMachine, TApdState}
  AdStDS,      {Additional data sources}                                 {!!.04}
  AdStSt,      {Additional States}                                       {!!.04}
  AdStProt,    {Protocol States}                                         {!!.04}
  AdStFax,     {Fax States}                                              {!!.04}
  AdStSapi,    {SAPI States}                                             {!!.04}
{$ENDIF}

{$IFNDEF UNICODE}
  { Fax Units }
  AdFaxCvt,    {TApdFaxConverter, TApdFaxUnpacker}
  AdFView,     {TApdFaxViewer}
  AdFax,       {TApdSendFax, TApdReceiveFax}
  AdFStat,     {TApdFaxStatus}
  AdFaxPrn,    {TApdFaxPrinter}
  AdFPStat,    {TApdFaxPrinterStatus}
  AdFaxCtl,    {TApdFaxDriverInterface}
  AdFaxSrv,    {TApdFaxServer, TApdFaxServerManager, TApdFaxClient}
{$ENDIF}

  { Telephony Units }
  AdTapi,      {TApdTapiDevice}
  AdTStat     {TApdTapiStatus}
{$IFNDEF UNICODE}
  ,AdPEditT    {select device dialog}
  ,AdSapiEn    {TApdSapiEngine}
  ,AdSapiPh    {TApdSapiPhone}
  ,AdVoIP      {IP Telephony components}
{$ENDIF}
   ;

procedure Register;
begin
  { Register standard components }
  RegisterComponents(APROTabName,
                     [TApdComPort,
{$IFNDEF UNICODE}
                      TApdWinsockPort,
                      TApdRasDialer,
                      TApdRasStatus,
                      TApdFtpClient,
                      TApdFtpLog,
                      TApdDataPacket,
                      TApdScript,
                      {TApdStateMachine,} { installed to APRO State Machine tab below }
                      {TApdState,}
                      TAdModem,
                      TAdModemStatus,
{$ENDIF}
                      TApdSLController,
                      TApdStatusLight
{$IFNDEF UNICODE}
                      ,TApdProtocol
                      ,TApdProtocolLog
                      ,TApdProtocolStatus
                      ,TApdPager
                      ,TApdTAPPager
                      ,TApdSNPPPager
                      ,TApdGSMPhone
                      ,TApdPagerLog
                      ,TAdTerminal
                      ,TAdTTYEmulator
                      ,TAdVT100Emulator
{$ENDIF}
                        ]);
  { Register Telephony components }
  RegisterComponents(APROTelephonyTabName,
                     [TApdTapiDevice,
                      TApdTapiStatus,
                      TApdTapiLog
{$IFNDEF UNICODE}
                      ,TApdSapiEngine
                      ,TApdSapiPhone
                      ,TApdVoIP
{$ENDIF}
                        ]);

  { Register Fax Components }
{$IFNDEF UNICODE}
  RegisterComponents(APROFaxTabName,
                     [TApdFaxConverter,
                      TApdFaxUnpacker,
                      TApdFaxViewer,
                      TApdReceiveFax,
                      TApdSendFax,
                      TApdFaxStatus,
                      TApdFaxLog,
                      TApdFaxPrinter,
                      TApdFaxPrinterStatus,
                      TApdFaxPrinterLog,
                      TApdFaxDriverInterface,
                      TApdFaxServer,
                      TApdFaxServerManager,
                      TApdFaxClient]);
{$ENDIF}

  { Register State Machine Components }                                  {!!.04}
{$IFNDEF UNICODE}
  RegisterComponents (APROStateTabName,                                  {!!.04}
                      [TApdStateMachine,                                 {!!.04}
                       TApdStateComPortSource,                           {!!.04}
                       TApdStateGenericSource,                           {!!.04}
                       TApdState,                                        {!!.04}
                       TApdActionState,                                  {!!.04}
                       TApdSendFileState,                                {!!.04}
                       TApdReceiveFileState,                             {!!.04}
                       TApdSendFaxState,                                 {!!.04}
                       TApdReceiveFaxState,                              {!!.04}
                       TApdSapiSpeakState]);                             {!!.04}
{$ENDIF}
end;

end.
