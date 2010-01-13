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
{*                   ADDATAB.PAS 4.06                    *}
{*********************************************************}
{* Deprecated INI database support                       *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit ADDataB;
  {-INI/Database concepts}

interface

uses
  SysUtils,
  Classes,
  OoMisc;

type
  {.Z+}
  {information about one field in an INI database}
  TDBFieldInfo = class(TObject)
  public
    Len   : Word;
    IsStr : Boolean;
    Name  : String;

    constructor CreateString(const FldName : String; const FldLen : Word);
    constructor CreateInt(const FldName : String);
    constructor Copy(const FI : TDBFieldInfo);
  end;

  {a list of TDBFieldInfos}
  TDBFieldList = class(TList)
  public
      {-Ensure all field list items are freed}
    procedure StrsToPChars(var StrRec, PCharRec);
      {-Convert a record with string variables to a record with PChars}
    procedure PCharsToStrs(var PCharRec, StrRec);
      {-Convert a record with PChar variables to a record with strings}
  end;
  {.Z-}

  {an indexed field}
  TDBIndexedField = String[MaxNameLen];

implementation

{TDBFieldInfo}

  constructor TDBFieldInfo.CreateString(const FldName : String; const FldLen : Word);
  begin
    Len   := FldLen;
    IsStr := True;
    Name  := FldName;
  end;

  constructor TDBFieldInfo.CreateInt(const FldName : String);
  begin
    Len   := SizeOf(Integer);
    IsStr := False;
    Name  := FldName;
  end;

  constructor TDBFieldInfo.Copy(const FI : TDBFieldInfo);
  begin
    Len   := FI.Len;
    IsStr := FI.IsStr;
    Name  := FI.Name;
  end;

{TDBFieldList}

  procedure TDBFieldList.StrsToPChars(var StrRec, PCharRec);
    {-Convert a record with string variables to a record with PChars}
  var
    StrData   : Pointer;
    PCharData : Pointer;
    Fld       : TDBFieldInfo;
    I         : Word;
    Len       : Word;

  begin
    StrData   := @StrRec;
    PCharData := @PCharRec;

    if (Count > 0) then
      {loop through each field}
      for I := 0 to Pred(Count) do begin
        {get a pointer to the field data}
        Fld := TDBFieldInfo(Items[I]);

        {if this is an integer field, just copy it into the destination rec}
        if not Fld.IsStr then begin
          Integer(PCharData^) := Integer(StrData^);

          StrData   := AddWordToPtr(StrData, SizeOf(Integer));
          PCharData := AddWordToPtr(PCharData, SizeOf(Integer));

        {otherwise, convert the pascal style string into a PChar}
        end else begin
          StrPCopy(PChar(PCharData), ShortString(StrData^));
          Len := Succ(Fld.Len);

          StrData   := AddWordToPtr(StrData, Len);
          PCharData := AddWordToPtr(PCharData, Len);
        end;
      end;
  end;

  procedure TDBFieldList.PCharsToStrs(var PCharRec, StrRec);
    {-Convert a record with PChar variables to a record with strings}
  var
    StrData   : Pointer;
    PCharData : Pointer;
    Fld       : TDBFieldInfo;
    I         : Word;
    Len       : Word;

  begin
    StrData   := @StrRec;
    PCharData := @PCharRec;

    if (Count > 0) then
      {loop through each field}
      for I := 0 to Pred(Count) do begin
        {get a pointer to the field data}
        Fld := TDBFieldInfo(Items[I]);

        {if this is an integer field, just copy it into the destination rec}
        if not Fld.IsStr then begin
          Integer(StrData^) := Integer(PCharData^);

          StrData   := AddWordToPtr(StrData, SizeOf(Integer));
          PCharData := AddWordToPtr(PCharData, SizeOf(Integer));

        {otherwise, convert the PChar into a pascal style string}
        end else begin
          ShortString(StrData^) := StrPas(PChar(PCharData));
          Len := Succ(Fld.Len);

          StrData   := AddWordToPtr(StrData, Len);
          PCharData := AddWordToPtr(PCharData, Len);
        end;
      end;
  end;

end.
