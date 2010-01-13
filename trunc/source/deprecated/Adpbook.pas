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
{*                   ADPBOOK.PAS 4.06                    *}
{*********************************************************}
{* Deprecated phone book database                        *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$G+,X+,F-,V-,P-,T-,B-}

unit AdPBook;
  {-Ini database descendant for storing phonebooks}

interface

uses
  SysUtils,
  WinTypes,
  WinProcs,
  Classes,
  ooMisc,
  AdDataB,
  AdIniDB;

const
  {length of name and phone number fields for phonebook}
  NameLen     = 21;
  PhoneNumLen = 21;

type
  PPhonebookEntry = ^TPhonebookEntry;
  TPhonebookEntry = packed record
    Name   : String[NameLen];
    Number : String[PhoneNumLen];
  end;

  TApdPhonebook = class(TApdCustomIniDBase)
  public
    constructor Create(AOwner : TComponent); override;

  protected
    {.Z+}
    procedure DefineProperties(Filer : TFiler); override;
    {.Z-}

  published
    property FileName;
  end;

implementation

  procedure TApdPhonebook.DefineProperties(Filer : TFiler);
  begin
    CustComponent := True;

    inherited DefineProperties(Filer);
  end;

  constructor TApdPhonebook.Create(AOwner : TComponent);
  var
    FL : TDBFieldList;
  begin
    inherited Create(AOwner);

    {create the field list}
    FL := TDBFieldList.Create;
    FL.Add(TDBFieldInfo.CreateString('Name', NameLen));
    FL.Add(TDBFieldInfo.CreateString('Number', PhoneNumLen));

    {set the field list}
    FieldList := FL;
    FL.Free;

    IndexedField := 'Name';

  end;

end.
