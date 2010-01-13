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
{*                   ADSELNUM.PAS 4.06                   *}
{*********************************************************}
{* Deprecated phone number selection dialog              *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdSelNum;
  {-For selecting phone numbers from a list}

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
  ExtCtrls,
  Grids,
  AdPBook;

type
  TNumberSelectForm = class(TForm)
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    ListGrid: TDrawGrid;
    ListHeader: THeader;
    procedure ListGridDrawCell( Sender : TObject ; Col, Row : Longint;
                                Rect : TRect; State : TGridDrawState );
    procedure ListHeaderSized(Sender: TObject; ASection, AWidth: Integer);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ListGridDblClick(Sender: TObject);

  protected
    LeftEdge  : Word;
    Items     : TList;
    Phonebook : TApdPhonebook;
    FSelNum   : String;
    FSelName  : String;

    procedure ResizeGridColumns;
      {-Change the widths of the grid columns to match the header sections}

  public
    constructor CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook);
    destructor Destroy; override;

    property SelectedNumber : String
      read FSelNum;
    property SelectedName : String
      read FSelName;
  end;

implementation

{$R *.DFM}

  procedure TNumberSelectForm.ResizeGridColumns;
    {-Change the widths of the grid columns to match the header sections}
  begin
    ListGrid.ColWidths[0] := ListHeader.SectionWidth[0] - LeftEdge;
    ListGrid.ColWidths[1] := ListGrid.Width - ListGrid.ColWidths[0];
  end;

  constructor TNumberSelectForm.CreatePBook(const AOwner : TComponent; const PB : TApdPhonebook);
  begin
    Phonebook := PB;
    Items     := TList.Create;
    FSelNum   := '';
    FSelName  := '';

    inherited Create(AOwner);
  end;

  destructor TNumberSelectForm.Destroy;
  var
    I : Word;

  begin
    if (Items.Count > 0) then
      for I := 0 to Pred(Items.Count) do
        FreeMem(Items.Items[I], SizeOf(TPhonebookEntry));
    Items.Free;

    inherited Destroy;
  end;

  procedure TNumberSelectForm.ListGridDrawCell( Sender : TObject; Col, Row : Longint;
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

  procedure TNumberSelectForm.ListHeaderSized(Sender: TObject; ASection, AWidth: Integer);
  begin
    ResizeGridColumns;
  end;

  procedure TNumberSelectForm.FormCreate(Sender: TObject);
  var
    Entry : PPhonebookEntry;
    I     : Word;

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

  procedure TNumberSelectForm.OkBtnClick(Sender : TObject);
  begin
    FSelNum     := PPhonebookEntry(Items.Items[ListGrid.Row])^.Number;
    FSelName    := PPhonebookEntry(Items.Items[ListGrid.Row])^.Name;
    ModalResult := mrOK;
  end;

  procedure TNumberSelectForm.ListGridDblClick(Sender : TObject);
  begin
    OkBtnClick(Sender);
  end;

end.
