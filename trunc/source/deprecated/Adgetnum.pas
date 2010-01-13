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
{*                   ADGETNUM.PAS 4.06                   *}
{*********************************************************}
{* Deprecated phone book dialog to retrieve numbers      *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F+,T-,V-,B+}

unit AdGetNum;
  {-For getting phone numbers from the user or a phonebook}

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
  AdPBook,
  AdSelNum,
  AdExcept;

type
  PShortString = ^ShortString;

  {.Z+}
  {form for entering or choosing a phone number}
  TGetNumberForm = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    PhoneCombo: TComboBox;
    SelectBtn: TBitBtn;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    procedure OkBtnClick(Sender: TObject);
    procedure SelectBtnClick(Sender: TObject);

  protected
    Phonebook : TApdPhonebook;
    EditNum   : PShortString;

  public
    constructor CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook; var EditSt : ShortString);
  end;
  {.Z-}

  {component for selecting or choosing a phone number}
  TApdPhoneNumberSelector = class(TCommonDialog)
  protected {private}
    {.Z+}
    FPhonebook : TApdPhonebook;
    FSelNum    : String;

    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    function GetVersion : string;
    procedure SetVersion(const Value : string);

  public
    constructor Create(AOwner : TComponent); override;
    {.Z-}
    function Execute : Boolean; {$IFDEF VERSION3} override; {$ENDIF} 

    property SelectedNumber : String
      read FSelNum;

  published
    property Version : string                                 
      read GetVersion
      write SetVersion
      stored False;
    property Phonebook : TApdPhonebook
      read FPhonebook write FPhonebook;
  end;

const
  {maximum number of entries in phone number history list}
  NumberHistLen = 20;

var
  {phone number history list strings}
  NumberHistory : TStringList;

implementation

{$R *.DFM}

{TGetNumberForm}

  constructor TGetNumberForm.CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook; var EditSt : ShortString);
  begin
    inherited Create(AOwner);

    PhoneCombo.Text  := EditSt;
    PhoneCombo.Items := NumberHistory;
    EditNum          := @EditSt;
    Phonebook        := PB;

    if (Phonebook.NumRecs = 0) then
      SelectBtn.Enabled := False;
  end;

  procedure TGetNumberForm.OkBtnClick(Sender: TObject);
  var
    I : Integer;

  begin
    EditNum^ := PhoneCombo.Text;
    if (EditNum^ <> '') then begin
      {if the list is full, delete the last item}
      if (NumberHistory.Count = NumberHistLen) then
        NumberHistory.Delete(NumberHistLen - 1);

      {see if the phone number is already in the history list}
      I := NumberHistory.IndexOf(EditNum^);
      if (I <> -1) then
        NumberHistory.Delete(I);

      {insert the phone number at the top}
      NumberHistory.Insert(0, EditNum^);
    end;
  end;

  procedure TGetNumberForm.SelectBtnClick(Sender: TObject);
  var
    Frm : TNumberSelectForm;

  begin
    Frm := TNumberSelectForm.CreatePBook(Self, Phonebook);
    Frm.Ctl3D := Ctl3D;
    if (Frm.ShowModal = mrOK) then
      PhoneCombo.Text := Frm.SelectedNumber;
    Frm.Free;
  end;

{TApdPhoneNumberSelector}

  procedure TApdPhoneNumberSelector.Notification(AComponent : TComponent; Operation : TOperation);
  begin
    inherited Notification(AComponent, Operation);

    if (Operation = opRemove) then begin
      {see if our phonebook is going away}
      if (AComponent = FPhonebook) then
        FPhonebook := nil;
    end else if (Operation = opInsert) then begin
      {check for phonebook being installed}
      if not Assigned(FPhonebook) and (AComponent is TApdPhonebook) then
        Phonebook := TApdPhonebook(AComponent);
    end;
  end;

  constructor TApdPhoneNumberSelector.Create(AOwner : TComponent);
  var
    I : Word;

  begin
    inherited Create(AOwner);

    FPhonebook := nil;
    FSelNum    := '';

    {search our owner for a phonebook}
    if Assigned(AOwner) and (AOwner.ComponentCount > 0) then
      for I := 0 to Pred(AOwner.ComponentCount) do
        if AOwner.Components[I] is TApdPhonebook then begin
          FPhonebook := TApdPhonebook(AOwner.Components[I]);
          Break;
        end;
  end;

  function TApdPhoneNumberSelector.Execute : Boolean;
  var
    Frm     : TGetNumberForm;
    WasOpen : Boolean;
    Edit    : String[PhoneNumLen];

  begin
    {make sure we have a valid phonebook}
    if not Assigned(FPhonebook) then
      raise EPhonebookNotAssigned.Create(ecPhonebookNotAssigned, False);

    Result := False;
    Edit   := '';

    {save the open state of the database and open it, if necessary}
    WasOpen := Phonebook.Open;
    if not WasOpen then
      Phonebook.Open := True;

    {execute the phonebook editing form}
    Frm := TGetNumberForm.CreatePBook(Self, Phonebook, Edit);
    Frm.Ctl3D := Ctl3D;
    if (Frm.ShowModal = mrOK) then begin
      FSelNum := TrimRight(Edit);                                    
      Result  := (FSelNum <> '');
    end;
    Frm.Free;

    {restore the open state of the database}
    if not WasOpen then
      Phonebook.Open := False;
  end;

  function TApdPhoneNumberSelector.GetVersion : string;
  begin
    Result := ApVersionStr;
  end;

  procedure TApdPhoneNumberSelector.SetVersion(const Value : string);
  begin
  end;

  procedure AdGetNumExit; {$IFNDEF Win32} far; {$ENDIF}
  begin
    NumberHistory.Free;
  end;

initialization
  NumberHistory := TStringList.Create;

{$IFNDEF Win32}
  AddExitProc(AdGetNumExit);
{$ELSE}
finalization
  AdGetNumExit;
{$ENDIF}

end.
