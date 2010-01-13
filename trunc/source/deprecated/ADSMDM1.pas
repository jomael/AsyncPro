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
{*                   ADSMDM1.PAS 4.06                    *}
{*********************************************************}
{* Deprecated modem selection dialog for the TApdSModem  *}
{*********************************************************}

unit AdSMdm1;

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
  ExtCtrls;

type
  TAdSModemSelection = class(TForm)
    ModemList: TComboBox;
    PortList: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Bevel1: TBevel;

  private
    { Private declarations }

  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  end;

implementation

{$R *.DFM}

constructor TAdSModemSelection.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
end;

destructor TAdSModemSelection.Destroy;
begin
  inherited Destroy;
end;

end.
