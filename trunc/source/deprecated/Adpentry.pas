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
{*                   ADPENTRY.PAS 4.06                   *}
{*********************************************************}
{* Deprecated phone book entry dialog                    *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdPEntry;
  {-Data entry for phonebook entries}

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
  AdExcept,
  AdPBook;

const
  { Resource string IDs for prompts -- strings are in APW.STR }
  peMustEnterName   = 4401; {to inform user that required data missing     }
  peMustEnterNumber = 4402; {"  "      "    "    "        "    "           }
  peNameExists      = 4403; {to inform used phone entry name already exists}

type
  TPhoneEntryForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    NameEdit: TEdit;
    NumberEdit: TEdit;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    procedure OkBtnClick(Sender: TObject);

  protected
    EditData  : PPhonebookEntry;
    Phonebook : TApdPhonebook;
    OrigName  : String[NameLen];

  public
    constructor CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook; var Data : TPhonebookEntry);
  end;

implementation

{$R *.DFM}

  constructor TPhoneEntryForm.CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook; var Data : TPhonebookEntry);
  begin
    inherited Create(AOwner);

    EditData  := @Data;
    Phonebook := PB;

    OrigName        := EditData^.Name;
    NameEdit.Text   := EditData^.Name;
    NumberEdit.Text := EditData^.Number;
  end;

  procedure TPhoneEntryForm.OkBtnClick(Sender: TObject);
  begin
    ModalResult := mrNone;

    {make sure both fields contain data}
    if (NameEdit.Text = '') then begin
      NameEdit.SetFocus;
      raise Exception.Create(AproLoadStr(peMustEnterName));          
    end else if (NumberEdit.Text = '') then begin
      NumberEdit.SetFocus;
      raise Exception.Create(AproLoadStr(peMustEnterNumber));        

    {make sure that an entry with this name does not already exist}
    end else if (UpperCase(OrigName) <> UpperCase(NameEdit.Text)) and
                (Phonebook.KeyExists(NameEdit.Text)) then begin
      NameEdit.SetFocus;
      raise Exception.Create(AproLoadStr(peNameExists));
    end;

    EditData^.Name   := NameEdit.Text;
    EditData^.Number := NumberEdit.Text;

    ModalResult := mrOK;
  end;

end.
