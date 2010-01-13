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
{*                   ADFLDLST.PAS 4.06                   *}
{*********************************************************}
{* Deprecated generic INI database field editor          *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F+}

unit AdFldLst;

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
  AdDBFld,
  AdDataB,
  ExtCtrls;

type
  {.Z+}
  TFrmDBFieldList = class(TForm)
    FieldList: TListBox;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    UpBtn: TSpeedButton;
    DownBtn: TSpeedButton;
    HelpBtn: TBitBtn;
    AddBtn: TBitBtn;
    EditBtn: TBitBtn;
    DeleteBtn: TBitBtn;
    Bevel1: TBevel;
    procedure AddBtnClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FieldListDblClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure DownBtnClick(Sender: TObject);

  protected
    EditFields : TDBFieldList;
    DBFields   : TDBFieldList;

  public
    constructor CreateFields(AOwner : TComponent; Fields : TDBFieldList);
    destructor Destroy; override;

    procedure EditPrim;
      {-Change the values for a particular database field}
    function RecSize : Word;
      {-Return the size of a database record}
    procedure SwapItems(I, J : Word);
      {-Swap the positions of two database items}
  end;
  {.Z-}

implementation

{$R *.DFM}

constructor TFrmDBFieldList.CreateFields(AOwner : TComponent; Fields : TDBFieldList);
var
  I : Word;

begin
  inherited Create(AOwner);

  {make a copy of the passed in fields}
  EditFields := TDBFieldList.Create;
  if (Fields.Count > 0) then
    for I := 0 to Pred(Fields.Count) do begin
      EditFields.Add(TDBFieldInfo.Copy(TDBFieldInfo(Fields.Items[I])));
      FieldList.Items.Add(TDBFieldInfo(EditFields.Items[I]).Name);
    end;
  DBFields := Fields;
end;

destructor TFrmDBFieldList.Destroy;
var
  I : Word;

begin
  {destroy all fields in the edit list}
  if (EditFields.Count > 0) then
    for I := 0 to Pred(EditFields.Count) do
      TDBFieldInfo(EditFields.Items[I]).Free;
  EditFields.Free;

  inherited Destroy;
end;

procedure TFrmDBFieldList.EditPrim;
  {-Change the values for a particular database field}
var
  FR  : TFrmFieldInfo;
  Fld : TDBFieldInfo;
  IX, Temp  : Integer;

begin
  { Delphi 4 Listbox bug workaround }
  Temp := FieldList.ItemIndex;
  if (Temp = 0) and (FieldList.Items.Count = 0) then
    Temp := -1;
  {is an item in the list selected?}
  if (Temp <> -1) then begin                                        
    {get the field to edit}
    Fld := TDBFieldInfo(EditFields.Items[FieldList.ItemIndex]);

    {execute the edit form}
    FR := TFrmFieldInfo.Create(Application);
    FR.Initialize(Fld, RecSize, EditFields);

    {did the user accept the new values?}
    if (FR.ShowModal = mrOK) then begin
      {update the field's information}
      IX := FieldList.ItemIndex;
      FieldList.Items[FieldList.ItemIndex] := Fld.Name;
      FieldList.ItemIndex := IX;
    end;
    FR.Free;

  {if not, respond politely}
  end else
    MessageBeep(0);
end;

function TFrmDBFieldList.RecSize : Word;
  {-Return the size of a database record}
var
  I   : Word;
  Fld : TDBFieldInfo;

begin
  {no size yet}
  Result := 0;
  if (EditFields.Count > 0) then
    {loop through each field and add its size to Result}
    for I := 0 to Pred(EditFields.Count) do begin
      Fld := TDBFieldInfo(EditFields.Items[I]);
      {if this is a string field, simply add the string length}
      {and an extra byte for the length byte to the size}
      if Fld.IsStr then
        Inc(Result, Fld.Len + 1)

      {otherwise, the field is an integer}
      else
        Inc(Result, SizeOf(Integer));
    end;
end;

procedure TFrmDBFieldList.SwapItems(I, J : Word);
  {-Swap the positions of two database items}
begin
  EditFields.Exchange(I, J);
  FieldList.Items.Exchange(I, J);
end;

procedure TFrmDBFieldList.AddBtnClick(Sender: TObject);
var
  FR  : TFrmFieldInfo;
  Fld : TDBFieldInfo;

begin
  {create form for editing field information}
  Fld := TDBFieldInfo.CreateString('FieldName', 20);
  FR  := TFrmFieldInfo.Create(Application);
  FR.Initialize(Fld, RecSize, EditFields);

  if (FR.ShowModal = mrOK) then begin
    {add the new field information to the list}
    EditFields.Add(Fld);
    FieldList.Items.Add(Fld.Name);
  end else
    Fld.Free;
  FR.Free;
end;

procedure TFrmDBFieldList.EditBtnClick(Sender: TObject);
begin
  EditPrim;
end;

procedure TFrmDBFieldList.DeleteBtnClick(Sender: TObject);
var
  Temp : Integer;
begin
  { Delphi 4 Listbox bug workaround }
  Temp := FieldList.ItemIndex;
  if (Temp = 0) and (FieldList.Items.Count = 0) then
    Temp := -1;
  {is an item in the list selected?}
  if (Temp <> -1) then begin
    EditFields.Delete(FieldList.ItemIndex);
    FieldList.Items.Delete(FieldList.ItemIndex);

  {if not, respond politely}
  end else
    MessageBeep(0);
end;

procedure TFrmDBFieldList.OkBtnClick(Sender: TObject);
var
  I    : Word;
  Strs : Boolean;

begin
  {if the user is accepting this field list, there should be at least one
  {string type field of MaxIndexLen characters or less}
  Strs := False;
  if (EditFields.Count > 0) then begin
    for I := 0 to Pred(EditFields.Count) do
      if TDBFieldInfo(EditFields.Items[I]).IsStr and (TDBFieldInfo(EditFields.Items[I]).Len <= MaxIndexLen) then begin
        Strs := True;
        Break;
      end;
  end;
  if not Strs then begin
    ModalResult := mrNone;
    raise Exception.Create('You should create at least one string field of ' +
                            IntToStr(MaxIndexLen) + ' characters or less');
  end;

  {delete old fields}
  if (DBFields.Count > 0) then
    for I := 0 to Pred(DBFields.Count) do
      TDBFieldInfo(DBFields.Items[I]).Free;
  DBFields.Clear;

  {make a copy of the fields}
  if (EditFields.Count > 0) then
    for I := 0 to Pred(EditFields.Count) do
      DBFields.Add(TDBFieldInfo.Copy(TDBFieldInfo(EditFields.Items[I])));

  ModalResult := mrOK;
end;

procedure TFrmDBFieldList.FieldListDblClick(Sender: TObject);
begin
  EditPrim;
end;

procedure TFrmDBFieldList.UpBtnClick(Sender: TObject);
var
  IX : Integer;

begin
  {if there's more than one item and an item is selected in}
  {the listbox, move the selected item up one position in}
  {the list}
  if (FieldList.Items.Count < 2) or (FieldList.ItemIndex = -1) or (FieldList.ItemIndex = 0) then
    MessageBeep(0)
  else begin
    IX := FieldList.ItemIndex;
    SwapItems(IX, Pred(IX));
    FieldList.ItemIndex := Pred(IX);
  end;
end;

procedure TFrmDBFieldList.DownBtnClick(Sender: TObject);
var
  IX : Integer;

begin
  {if there's more than one item and an item is selected in}
  {the listbox, move the selected item down one position in}
  {the list}
  if (FieldList.Items.Count < 2) or (FieldList.ItemIndex = -1) or (FieldList.ItemIndex = Pred(FieldList.Items.Count)) then
    MessageBeep(0)
  else begin
    IX := FieldList.ItemIndex;
    SwapItems(IX, Succ(IX));
    FieldList.ItemIndex := Succ(IX);
  end;
end;

end.
