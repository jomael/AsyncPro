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
{*                   APDEPREG.PAS 4.06                   *}
{*********************************************************}
{* Deprecated component registration unit, add to your   *}
{* own package to install                                *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F+}

unit APDepReg;

interface

uses
{$IFDEF Delphi6}
  DesignIntf,
  DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}

{ Constant declarations for IDE palette tab names }

const
  APRODepTabName = 'APRO Deprecated';

procedure Register;

implementation

{.$R APDEPREG.DCR}

uses
  { RTL/VCL Units }
  Classes, SysUtils,
  AdDataB,
  AdDBFld,
  AdDial,
  AdFldLst,
  AdGetNum,
  AdIniDB,
  AdKeyEd,
  AdModDB,
  AdModem,
  AdPBEdit,
  AdPBook,
  AdPEntry,
  AdSelNum,
  ADSMDM1,
  AdSModem,
  AdTerm,
  AwBPlus,
  AwEmu,
  AwIniDB,
  AwKeyEmu,
  AwModDB,
  AwModem,
  AwTerm;

procedure Register;
begin
  { Register deprecated components }
  RegisterComponents(APRODepTabName,
                     [TApdTerminal,
                      TApdKeyBoardEmulator,
                      TApdEmulator,
                      TApdPhonebook,
                      TApdPhonebookEditor,
                      TApdPhoneNumberSelector,
                      TApdModem,
                      TApdModemDialer,
                      TApdDialerDialog,
                      TApdSModem,
                      TApdModemDBase,
                      TApdIniDBase
                      ]);

end;

end.
