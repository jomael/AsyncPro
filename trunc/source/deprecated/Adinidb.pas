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
{*                   ADINIDB.PAS 4.06                    *}
{*********************************************************}
{* Deprecated INI database component                     *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdIniDB;
  {-Delphi INI database component}

interface

uses
  SysUtils,
  Classes,
  Messages,
  WinTypes,
  WinProcs,
  Forms,
  Controls,
  OoMisc,
  AwIniDB,
  AdExcept,
  AdDataB,
  AdFldLst;

const
  DefDBName      = 'DATABASE.INI';
  DefSortedIndex = False;

{$IFDEF Win32}
{$IFOPT H-}
{$DEFINE HOff}
{$ENDIF}
{$ENDIF}

type

  {a database key string}
  TDBKeyStr = String[MaxIndexLen];

  {INI database component}
  TApdCustomIniDBase = class(TApdBaseComponent)
  protected {private}
    {.Z+}
    DB            : PIniDatabase;       {record passed to DLL calls}
    FRecordList   : TStringList;        {for returning record lists}
    FFieldList    : TDBFieldList;       {list of database fields}
    Scratch       : Pointer;            {scratch record for passing into DLL}
    Changed       : Boolean;            {TRUE if database data has changed}
    FOpen         : Boolean;            {TRUE if database has been opened}
    FFileName     : String;             {file name, used mostly at design time}
    FIndexedField : TDBIndexedField;    {name of field used for index}
    FSortedIndex  : Boolean;            {TRUE if RecordList should be sorted}
    CustComponent : Boolean;

    {Property read/write methods}
    procedure SetFileName(const NewName : String);
      {-Update the database's file name.  If open, reopen with the new name}
    procedure SetOpen(const OpenIt : Boolean);
      {-Open or close the database}
    procedure SetFieldList(const Fields : TDBFieldList);
      {-Set the database's list of fields}
    procedure SetIndexedField(const S : TDBIndexedField);
      {-Set the name of the field used to index the database}
    procedure SetSortedIndex(const NewSorted : Boolean);
      {-Set the Sorted property of the record list}
    function GetRecordList : TStrings;
      {-Returns the key strings for each record in the database}
    function GetNumRecs : Integer;
      {-Returns the number of records in the database}

    {utility}
    procedure AssureOpen;
      {-Make sure that the database is open, otherwise raise an exception}
    procedure ClearFieldList;
      {-Remove all fields in the field list}

    {streaming}
    procedure ReadFields(Reader : TReader);
      {-Reads the database field list from a stream}
    procedure WriteFields(Writer : TWriter);
      {-Writes the database field list to a stream}
    procedure DefineProperties(Filer : TFiler); override;
      {-Define methods for reading and writing field list}

  protected
    property FileName : String
      read FFileName write SetFileName;
    property SortedIndex : Boolean
      read FSortedIndex write SetSortedIndex default DefSortedIndex;

  public
    {Creation/destruction}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    {.Z-}
    function KeyExists(const Key : TDBKeyStr) : Boolean;
      {-Return TRUE if an entry with an index of 'Name' exists}
    procedure AddRecord(var Rec);
      {-Add a record to the database}
    procedure UpdRecord(const Key : TDBKeyStr; var Rec);
      {-Update a record in the database}
    procedure DefaultIndexed;
      {-Default the FIndexedField property to the first indexable field in the list}
    procedure DelRecord(const Key : TDBKeyStr);
      {-Remove a record from the database}
    procedure GetRecord(const Key : TDBKeyStr; var Rec);
      {-Get a record from the database}
    procedure WriteToIni(var Rec; const Section, IniFile : String);
      {-Write the record to a user-specified .INI file}
    procedure ReadFromIni(var Rec; const Section, IniFile : String);
      {-Read the record from a user-specified .INI file}

    property FieldList : TDBFieldList
      read FFieldList write SetFieldList;
    property IndexedField : TDBIndexedField
      read FIndexedField write SetIndexedField;
    property RecordList : TStrings
      read GetRecordList;
    property NumRecs : Integer
      read GetNumRecs;
    property Open : Boolean
      read FOpen write SetOpen;
  end;

  TApdIniDBase = class(TApdCustomIniDBase)
  published
    property FileName;
    property FieldList;
    property IndexedField;
    property SortedIndex;
  end;

implementation

{TApdCustomIniDBase}

  procedure TApdCustomIniDBase.SetFileName(const NewName : String);
    {-Update the database's file name.  If open, reopen with the new name}
  begin
    if (TrimRight(NewName) <> '') then
      FFileName := TrimRight(NewName)                                
    else
      Exit;

    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      Exit;

    {reopen the database, if necessary}
    if Open then
      Open := True;
  end;

  procedure TApdCustomIniDBase.SetOpen(const OpenIt : Boolean);
    {-Open or close the database}
  var
    I       : Cardinal;
    Fld     : TDBFieldInfo;
    Idx     : TDBIndexedField;
    Temp    : array[0..255] of Char;
    FldName : array[0..255] of Char;

    procedure FreeElement;
    begin
      if Assigned(Scratch) then begin
        FreeMem(Scratch, DB^.RecordSize);
        Scratch := nil;
      end;

      iDoneIniDatabase(DB);
      DB := nil;
    end;

  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      Exit;

    if OpenIt then begin
      if FOpen then
        FreeElement;

      iInitIniDatabase(DB, StrPCopy(Temp, FFileName));

      try
        {add fields to the database}
        Idx := UpperCase(FIndexedField);
        for I := 0 to Pred(FieldList.Count) do begin
          Fld := FieldList.Items[I];
          StrPCopy(FldName, Fld.Name);

          if Fld.IsStr then
            CheckException(Self, iAddIniDBStringField(DB, FldName, Fld.Len,
                          UpperCase(Fld.Name) = Idx))
          else
            CheckException(Self, iAddIniDBIntField(DB, FldName));
        end;

        {open the database}
        CheckException(Self, iPrepareIniDatabase(DB, nil));

        {get scratch memory for passing in and out of the DLL}
        GetMem(Scratch, DB^.RecordSize);

        Changed := True;
      except
        FreeElement;
        raise;
      end;

    end else
      if FOpen then
        FreeElement;

    FOpen := OpenIt;
  end;

  procedure TApdCustomIniDBase.SetFieldList(const Fields : TDBFieldList);
    {-Set the database's list of fields}
  var
    I, J    : Cardinal;
    Fld     : TDBFieldInfo;
    WasOpen : Boolean;

  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      Exit;

    {make sure that there is at least one field}
    if (Fields.Count = 0) then
      raise EBadFieldList.Create(ecBadFieldList, False);

    {make sure at least one field is indexable}
    for I := 0 to Pred(Fields.Count) do begin
      Fld := Fields.Items[I];

      {if there is an indexable field, start using the new field list}
      if Fld.IsStr and (Fld.Len <= MaxIndexLen) then begin
        WasOpen := Open;
        Open := False;
        ClearFieldList;
        {copy the field list}
        for J := 0 to Pred(Fields.Count) do
          FieldList.Add(TDBFieldInfo.Copy(TDBFieldInfo(Fields.Items[J])));
        DefaultIndexed;
        Open := WasOpen;
        Exit;
      end;
    end;

    {no indexable fields...raise an exception}
    raise EBadFieldList.Create(ecBadFieldList, False);
  end;

  procedure TApdCustomIniDBase.SetIndexedField(const S : TDBIndexedField);
    {-Set the name of the field used to index the database}
  var
    I   : Word;
    Fld : TDBFieldInfo;
    F   : String;

  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then begin
      FIndexedField := S;
      Exit;
    end;

    F := UpperCase(S);

    {see if this field can, in fact, be indexed}
    for I := 0 to Pred(FieldList.Count) do begin
      Fld := TDBFieldInfo(FieldList.Items[I]);
      if (UpperCase(Fld.Name) = F) and Fld.IsStr and (Fld.Len <= MaxIndexLen) then begin
        FIndexedField := S;

        {reopen the database, if necessary}
        if Open then
          Open := True;

        Exit;
      end;
    end;

    {raise an exception if the field name passed for the index was bad}
    raise EBadFieldForIndex.Create(ecBadFieldForIndex, False);
  end;

  procedure TApdCustomIniDBase.SetSortedIndex(const NewSorted : Boolean);
    {-Set the Sorted property of the record list}
  begin
    if (NewSorted = FSortedIndex) then
      Exit;

    FSortedIndex := NewSorted;
    if csDesigning in ComponentState then
      Exit;

    FRecordList.Sorted := FSortedIndex;
    Changed            := True;
  end;

  function TApdCustomIniDBase.GetRecordList : TStrings;
    {-Returns the key strings for each record in the database}
  var
    IndexRec : PChar;
    Temp     : PChar;
    BufSize  : Integer;

  begin
    AssureOpen;

    if Changed then begin
      FRecordList.Clear;

      if (NumRecs > 0) then begin
        {allocate memory for the index}
        CheckException(Self, iAllocIniIndexRec(DB, IndexRec, BufSize));

        {load the buffer with the strings}
        CheckException(Self, iLoadIniIndex(DB, IndexRec, BufSize));

        {put the strings into the list}
        Temp := IndexRec;
        while (Temp^ <> #0) do begin
          FRecordList.Add(StrPas(Temp));
          Inc(Temp, StrLen(Temp) + 1);
        end;

        {deallocate the index}
        iDeallocIniIndexRec(DB, IndexRec, BufSize);
      end;

      Changed := False;
    end;

    Result := FRecordList;
  end;

  function TApdCustomIniDBase.GetNumRecs : Integer;
    {-Returns the number of records in the database}
  begin
    AssureOpen;
    Result := iNumIniRecs(DB);
  end;

  procedure TApdCustomIniDBase.AssureOpen;
    {-Make sure that the database is open, otherwise raise an exception}
  begin
    if not Open then
      Open := True;
  end;

  procedure TApdCustomIniDBase.ClearFieldList;
    {-Remove all fields in the field list}
  var
    I : Word;

  begin
    if (FieldList.Count > 0) then begin
      for I := 0 to Pred(FieldList.Count) do
        TDBFieldInfo(FieldList.Items[I]).Free;
      FieldList.Clear;
    end;
  end;

  procedure TApdCustomIniDBase.DefaultIndexed;
    {-Default the FIndexedField property to the first indexable field in the list}
  var
    I : Word;

  begin
    for I := 0 to Pred(FieldList.Count) do
      with TDBFieldInfo(FieldList.Items[I]) do
        if IsStr then begin
          IndexedField := Name;
          Break;
        end;
  end;

  procedure TApdCustomIniDBase.ReadFields(Reader : TReader);
    {-Reads the database field list from a stream}
  var
    Fld : TDBFieldInfo;

  begin
    {remove all existing fields, if any}
    ClearFieldList;

    Reader.ReadListBegin;
    while not Reader.EndOfList do begin
      Fld := TDBFieldInfo.Create;

      Fld.Len   := Reader.ReadInteger;
      Fld.IsStr := Reader.ReadBoolean;
      Fld.Name  := Reader.ReadString;

      FieldList.Add(Fld);
    end;
    Reader.ReadListEnd;
  end;

  procedure TApdCustomIniDBase.WriteFields(Writer : TWriter);
    {-Writes the database field list to a stream}
  var
    I   : Integer;
    Fld : TDBFieldInfo;

  begin
    Writer.WriteListBegin;
    for I := 0 to Pred(FieldList.Count) do begin
      Fld := FieldList.Items[I];
      Writer.WriteInteger(Fld.Len);
      Writer.WriteBoolean(Fld.IsStr);
      Writer.WriteString(Fld.Name);
    end;
    Writer.WriteListEnd;
  end;

  procedure TApdCustomIniDBase.DefineProperties(Filer : TFiler);
    {-Define methods for reading and writing field list}
  begin
    inherited DefineProperties(Filer);

    if not CustComponent then
      Filer.DefineProperty('FakeProperty', ReadFields, WriteFields, True);
  end;

  constructor TApdCustomIniDBase.Create(AOwner : TComponent);
    {-Create an INI database}
  var
    DefFld : TDBFieldInfo;
  begin
    CustComponent := False;

    inherited Create(AOwner);

    DB           := nil;
    Scratch      := nil;
    Changed      := True;
    FOpen        := False;
    FSortedIndex := DefSortedIndex;
    FRecordList  := nil;
    FFileName    := DefDBName;

    FFieldList := TDBFieldList.Create;

    if not (csDesigning in ComponentState) then
      {create the object for returning record names}
      FRecordList := TStringList.Create;

    {create default values}
    FIndexedField := 'Default';

    {create a default field}
    DefFld := TDBFieldInfo.CreateString('Default', 20);
    FieldList.Add(DefFld);
  end;

  destructor TApdCustomIniDBase.Destroy;
    {-Destroy an INI database}
  begin
    Open := False;

    if Assigned(Scratch) then
      FreeMem(Scratch, DB^.RecordSize);
    if Assigned(FRecordList) then
      FRecordList.Free;

    ClearFieldList;
    FieldList.Free;

    inherited Destroy;
  end;

  function TApdCustomIniDBase.KeyExists(const Key : TDBKeyStr) : Boolean;
    {-Return TRUE if an entry with an index of 'Name' exists}
  var
    P : array[0..MaxIndexLen] of Char;

  begin
    AssureOpen;

    StrPCopy(P, Key);
    Result := iKeyExists(DB, P);
  end;

  procedure TApdCustomIniDBase.AddRecord(var Rec);
    {-Add a record to the database}
  begin
    AssureOpen;

    FieldList.StrsToPChars(Rec, Scratch^);
    CheckException(Self, iAddIniRecord(DB, Scratch^));

    Changed := True;
  end;

  procedure TApdCustomIniDBase.UpdRecord(const Key : TDBKeyStr; var Rec);
    {-Update a record in the database}
  var
    Temp : array[0..MaxIndexLen] of Char;

  begin
    AssureOpen;

    FieldList.StrsToPChars(Rec, Scratch^);
    CheckException(Self, iUpdIniRecord(DB, StrPCopy(Temp, Key), Scratch^));

    Changed := True;
  end;

  procedure TApdCustomIniDBase.DelRecord(const Key : TDBKeyStr);
    {-Remove a record from the database}
  var
    Temp : array[0..MaxIndexLen] of Char;

  begin
    AssureOpen;

    CheckException(Self, iDelIniRecord(DB, StrPCopy(Temp, Key)));
    Changed := True;
  end;

  procedure TApdCustomIniDBase.GetRecord(const Key : TDBKeyStr; var Rec);
    {-Get a record from the database}
  var
    Temp : array[0..MaxIndexLen] of Char;

  begin
    AssureOpen;

    CheckException(Self, iGetIniRecord(DB, StrPCopy(Temp, Key), Scratch^));
    FieldList.PCharsToStrs(Scratch^, Rec);
  end;

  procedure TApdCustomIniDBase.WriteToIni(var Rec; const Section, IniFile : String);
    {-Write the record to a user-specified .INI file}
  var
    TempSec : array[0..255] of Char;
    TempIni : array[0..255] of Char;

  begin
    AssureOpen;

    FieldList.StrsToPChars(Rec, Scratch^);
    CheckException(Self, iWriteToIni(DB, Scratch^, StrPCopy(TempSec, Section), StrPCopy(TempIni, IniFile)));
  end;

  procedure TApdCustomIniDBase.ReadFromIni(var Rec; const Section, IniFile : String);
    {-Read the record from a user-specified .INI file}
  var
    TempSec : array[0..255] of Char;
    TempIni : array[0..255] of Char;

  begin
    AssureOpen;

    CheckException(Self, iReadFromIni(DB, Scratch^, StrPCopy(TempSec, Section), StrPCopy(TempIni, IniFile)));
    FieldList.PCharsToStrs(Scratch^, Rec);
  end;

end.
