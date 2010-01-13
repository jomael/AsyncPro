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
{*                   ADPBEDIT.PAS 4.06                   *}
{*********************************************************}
{* Deprecated phone book editor                          *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdPBEdit;
  {-Phone book editor}

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
  AdPBook,
  ExtCtrls,
  Grids,
  AdPEntry,
  AdExcept,
  OoMisc;

const
  { Constants for strings }
  peDeleteQuery = 4101;

type
  {form for editing phonebooks}
  {.Z+}
  TPhonebookForm = class(TForm)
    AddBtn: TBitBtn;
    ChangeBtn: TBitBtn;
    RemoveBtn: TBitBtn;
    HelpBtn: TBitBtn;
    OkBtn: TBitBtn;
    ListGrid: TDrawGrid;
    ListHeader: THeader;
    procedure ListGridDrawCell( Sender : TObject ; Col, Row : Longint;
                                Rect : TRect; State : TGridDrawState );
    procedure FormCreate(Sender: TObject);
    procedure HeaderSized(Sender: TObject; ASection, AWidth: Integer);
    procedure AddBtnClick(Sender: TObject);
    procedure ChangeBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure ListGridDblClick(Sender: TObject);

  protected
    LeftEdge  : Integer;                                             
    Items     : TList;
    Phonebook : TApdPhonebook;

    procedure ResizeGridColumns;
      {-Change the widths of the grid columns to match the header sections}
    procedure EditPrim;
      {-Edit the currently selected record}

  public
    constructor CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook);
    destructor Destroy; override;
  end;
  {.Z-}

  {component for editing phonebooks}
  TApdPhonebookEditor = class(TCommonDialog)
  protected {private}
    {.Z+}
    FPhonebook : TApdPhonebook;

    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    function GetVersion : string;
    procedure SetVersion(const Value : string);                   

  public
    constructor Create(AOwner : TComponent); override;
    {.Z-}
    function Execute: Boolean; {$IFDEF VERSION3} override; {$ENDIF}   

  published
    property Version : string                                     
      read GetVersion
      write SetVersion
      stored False;
    property Phonebook : TApdPhonebook
      read FPhonebook write FPhonebook;
  end;

implementation

{$R *.DFM}

{TPhonebookForm}

  constructor TPhonebookForm.CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook);
  begin
    Phonebook := PB;
    Items     := TList.Create;

    inherited Create(AOwner);
  end;

  destructor TPhonebookForm.Destroy;
  var
    I : Cardinal;

  begin
    if (Items.Count > 0) then
      for I := 0 to Pred(Items.Count) do
        FreeMem(Items.Items[I], SizeOf(TPhonebookEntry));
    Items.Free;

    inherited Destroy;
  end;

  procedure TPhonebookForm.ListGridDrawCell( Sender : TObject; Col, Row : Longint;
                                             Rect : TRect; State: TGridDrawState );
  var
    Entry : PPhonebookEntry;
    C     : array[0..255] of Char;

  begin
    if (Row < Items.Count) then begin
      Entry := PPhonebookEntry(Items.Items[Row]);

      if (Col = 0) then
        StrPCopy(C, Entry^.Name)
      else
        StrPCopy(C, Entry^.Number);

      ExtTextOut(ListGrid.Canvas.Handle, Rect.Left + 2, Rect.Top + 2, 0,
                 @Rect, C, StrLen(C), nil);
    end;
  end;

  procedure TPhonebookForm.ResizeGridColumns;
    {-Change the widths of the grid columns to match the header sections}
  begin
    ListGrid.ColWidths[0] := ListHeader.SectionWidth[0] - LeftEdge;
    ListGrid.ColWidths[1] := ListGrid.Width - ListGrid.ColWidths[0];
  end;

  procedure TPhonebookForm.EditPrim;
    {-Edit the currently selected record}
  var
    OnItem : Cardinal;
    Entry  : PPhonebookEntry;
    Frm    : TPhoneEntryForm;
    Old    : TPhonebookEntry;
    Name   : String[NameLen];

  begin
    if (Items.Count = 0) then begin
      MessageBeep(0);
      Exit;
    end;

    OnItem := ListGrid.Row;
    Entry  := Items.Items[OnItem];
    Name   := Entry^.Name;
    Old    := Entry^;

    Frm       := TPhoneEntryForm.CreatePBook(Self, Phonebook, Old);
    Frm.Ctl3D := Ctl3D;
    if (Frm.ShowModal = mrOK) then begin
      Entry^ := Old;
      Phonebook.UpdRecord(Name, Entry^);
      ListGrid.Invalidate;
    end;
    Frm.Free;
  end;

  procedure TPhonebookForm.FormCreate(Sender: TObject);
  var
    Entry : PPhonebookEntry;
    I     : Cardinal;

  begin
    LeftEdge := (ListGrid.Width - ListGrid.ClientWidth) div 2;
    ListHeader.SectionWidth[0] := ListHeader.Width div 2;
    ListHeader.SectionWidth[1] := ListHeader.Width - ListHeader.SectionWidth[0];
    ResizeGridColumns;

    {load the list with entry data}
    for I := 1 to Phonebook.NumRecs do begin
      GetMem(Entry, SizeOf(TPhonebookEntry));
      Phonebook.GetRecord(Phonebook.RecordList[I-1], Entry^);
      Items.Add(Entry);
    end;

    ListGrid.RowCount := Phonebook.NumRecs;
  end;

  procedure TPhonebookForm.HeaderSized(Sender: TObject; ASection, AWidth: Integer);
  begin
    ResizeGridColumns;
  end;

  procedure TPhonebookForm.AddBtnClick(Sender: TObject);
  var
    Frm      : TPhoneEntryForm;
    NewEntry : PPhonebookEntry;

  begin
    GetMem(NewEntry, SizeOf(TPhonebookEntry));
    NewEntry^.Name   := '';
    NewEntry^.Number := '';

    Frm := TPhoneEntryForm.CreatePBook(Self, Phonebook, NewEntry^);
    Frm.Ctl3D := Ctl3D;
    if (Frm.ShowModal = mrOK) then begin
      Phonebook.AddRecord(NewEntry^);
      ListGrid.RowCount := Phonebook.NumRecs;
      Items.Add(NewEntry);
      if (Items.Count = 1) then
        ListGrid.Invalidate;
      ListGrid.Row := Pred(Items.Count);
    end else
      FreeMem(NewEntry, SizeOf(TPhonebookEntry));
    Frm.Free;
  end;

  procedure TPhonebookForm.ChangeBtnClick(Sender: TObject);
  begin
    EditPrim;
  end;

  procedure TPhonebookForm.RemoveBtnClick(Sender: TObject);
  begin
    if (Items.Count = 0) then begin
      MessageBeep(0);
      Exit;
    end;


    if (MessageDlg(AproLoadStr(peDeleteQuery),                       
                   mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
      Exit;

    Phonebook.DelRecord(PPhonebookEntry(Items.Items[ListGrid.Row])^.Name);
    Items.Delete(ListGrid.Row);
    ListGrid.RowCount := Pred(ListGrid.RowCount);
  end;

  procedure TPhonebookForm.ListGridDblClick(Sender: TObject);
  begin
    EditPrim;
  end;

{TApdPhonebookEditor}

  procedure TApdPhonebookEditor.Notification(AComponent : TComponent; Operation : TOperation);
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

  constructor TApdPhonebookEditor.Create(AOwner : TComponent);
  var
    I : Cardinal;
  begin
    inherited Create(AOwner);

    FPhonebook := nil;
    {search our owner for a phonebook}
    if Assigned(AOwner) and (AOwner.ComponentCount > 0) then
      for I := 0 to Pred(AOwner.ComponentCount) do
        if AOwner.Components[I] is TApdPhonebook then begin
          FPhonebook := TApdPhonebook(AOwner.Components[I]);
          Break;
        end;
  end;

  function TApdPhonebookEditor.Execute: Boolean;
  var
    Frm     : TPhonebookForm;
    WasOpen : Boolean;

  begin
    {make sure we have a valid phonebook}
    if not Assigned(FPhonebook) then
      raise EPhonebookNotAssigned.Create(ecPhonebookNotAssigned, False);

    {save the open state of the database and open it, if necessary}
    WasOpen := Phonebook.Open;
    if not WasOpen then
      Phonebook.Open := True;

    {execute the phonebook editing form}
    Frm := TPhonebookForm.CreatePBook(Self, Phonebook);
    Frm.Ctl3D := Ctl3D;
    Frm.ShowModal;
    Frm.Free;
    {restore the open state of the database}
    Execute := WasOpen;
    if not WasOpen then
      Phonebook.Open := False;
  end;

  function TApdPhonebookEditor.GetVersion : string;
  begin
    Result := ApVersionStr;
  end;

  procedure TApdPhonebookEditor.SetVersion(const Value : string);
  begin
  end;

end.
