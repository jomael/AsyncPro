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
{*                   ADDBFLD.PAS 4.06                    *}
{*********************************************************}
{* Deprecated TDBFieldList property editor               *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdDBFld;
  {-Field information form for TDBFieldList property editor}

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
  ExtCtrls,
  StdCtrls,
  Buttons,
  ooMisc,
  AdDataB;

type
  {.Z+}
  TFrmFieldInfo = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    StrTypeCheckBox: TCheckBox;
    FieldNameEdit: TEdit;
    MaxLenEdit: TEdit;
    procedure OkBtnClick(Sender: TObject);
    procedure StrTypeCheckBoxClick(Sender: TObject);

  protected
    EditData : TDBFieldInfo;
    RecSize  : Word;
    DBFields : TDBFieldList;

  public
    procedure Initialize(FieldInfo : TDBFieldInfo; Size : Word; Fields : TDBFieldList);
      {-Initialize dialog fields with values from FieldInfo}
    procedure AcceptValues;
      {-Place values from dialog fields into EditData}
  end;
  {.Z-}

implementation

{$R *.DFM}

procedure TFrmFieldInfo.Initialize(FieldInfo : TDBFieldInfo; Size : Word; Fields : TDBFieldList);
  {-Initialize dialog fields with values from FieldInfo}
begin
  StrTypeCheckBox.Checked := FieldInfo.IsStr;

  {if this isn't a string field, make the maximum length field unavailable}
  if not FieldInfo.IsStr then begin
    MaxLenEdit.Text    := '';
    MaxLenEdit.Visible := False;
    Label2.Visible     := False;

  {otherwise, set the initial value in the maximum length field}
  end else
    MaxLenEdit.Text := IntToStr(FieldInfo.Len);

  {initialize the values of the other controls}
  FieldNameEdit.Text := FieldInfo.Name;
  EditData           := FieldInfo;
  RecSize            := Size;
  DBFields           := Fields;
end;

procedure TFrmFieldInfo.AcceptValues;
  {-Place values from dialog fields into EditData}
var
  E : Integer;

begin
  EditData.Name  := Trim(FieldNameEdit.Text);
  EditData.IsStr := StrTypeCheckBox.Checked;
  if EditData.IsStr then
    Val(Trim(MaxLenEdit.Text), EditData.Len, E);
end;

procedure TFrmFieldInfo.OkBtnClick(Sender: TObject);
var
  E : Integer;
  V : LongInt;
  I : Word;
  S : String;

begin
  ModalResult := mrNone;

  {make sure the field name isn't blank}
  if (Trim(FieldNameEdit.Text) = '') then begin
    FieldNameEdit.SetFocus;
    raise Exception.Create('You must enter a field name');
  end;

  {make sure this isn't a duplicate field name}
  S := UpperCase(Trim(FieldNameEdit.Text));
  if (DBFields.Count <> 0) then
    for I := 0 to Pred(DBFields.Count) do
      if (UpperCase(TDBFieldInfo(DBFields.Items[I]).Name) = S) and (DBFields.Items[I] <> EditData) then begin
        FieldNameEdit.SetFocus;
        raise Exception.Create('A field with that name already exists');
      end;

  {is this a string type field?}
  if StrTypeCheckBox.Checked then begin
    {validate the field length}
    if (Length(Trim(MaxLenEdit.Text)) = 0) then begin
      MaxLenEdit.SetFocus;
      raise Exception.Create('You must enter a field length for string fields');
    end;

    Val(Trim(MaxLenEdit.Text), V, E);
    if (E <> 0) then begin
      MaxLenEdit.SetFocus;
      raise Exception.Create('Field length must be a number');
    end;

    if (V = 0) or (V > 255) then begin
      MaxLenEdit.SetFocus;
      raise Exception.Create('Field length must be between 1 and 255 characters');
    end;

    if ((V + RecSize) > $FFF0) then begin
      MaxLenEdit.SetFocus;
      raise Exception.Create('Total record size cannot be greater than 65520 bytes');
    end;
  end;

  ModalResult := mrOK;
  AcceptValues;
end;

procedure TFrmFieldInfo.StrTypeCheckBoxClick(Sender: TObject);
begin
  {toggle the availability of the maximum length field}
  {when the string type check box's value changes}
  if StrTypeCheckBox.Checked then begin
    MaxLenEdit.Text    := '20';
    MaxLenEdit.Visible := True;
    Label2.Visible     := True;
  end else begin
    MaxLenEdit.Text    := '';
    MaxLenEdit.Visible := False;
    Label2.Visible     := False;
  end;
end;

end.
