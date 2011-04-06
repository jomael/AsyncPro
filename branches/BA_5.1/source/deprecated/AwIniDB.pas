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
{*                   AWINIDB.PAS 4.06                    *}
{*********************************************************}
{* Deprecated INI file database                          *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I ..\..\includes\AWDEFINE.INC}

{Options required for this unit}
{$X+,V-,B-,I-}

unit AwIniDB;
	{-INI file database management }

interface

uses
	SysUtils,
	Windows,
	Classes,
	OoMisc;

procedure iInitIniDatabase(var Ini : PIniDatabase; FName : PAnsiChar);
	{-Initialize an .INI file database }

procedure iDoneIniDatabase(var Ini : PIniDatabase);
	{-Destroy an .INI file database }

function iAddIniDBStringField(Ini       : PIniDatabase;
															FieldName : PAnsiChar; MaxLen : Cardinal;
															Index     : Bool) : Integer;
	{-Add a string field to the .INI file database }

function iAddIniDBIntField(Ini : PIniDatabase; FieldName : PAnsiChar) : Integer;
	{-Add an integer field to the .INI file database }

function iPrepareIniDatabase(Ini : PIniDatabase; Defaults : Pointer) : Integer;
	{-Prepare the databse for reading/writing }

function iChangeIniDefaults(Ini : PIniDatabase; var DefaultRec) : Integer;
	{-Change the default values for record fields }

function iKeyExists(Ini : PIniDatabase; Key : PAnsiChar) : Bool;
	{-Return TRUE if an entry with an index of 'Name' exists }

function iAddIniRecord(Ini : PIniDatabase; var Rec) : Integer;
	{-Add a record to the database }

function iUpdIniRecord(Ini : PIniDatabase; Key : PAnsiChar; var Rec) : Integer;
	{-Update a record in the database }

function iDelIniRecord(Ini : PIniDatabase; Key : PAnsiChar) : Integer;
	{-Remove a record from the database }

function iGetIniRecord(Ini : PIniDatabase; Key : PAnsiChar; var Rec) : Integer;
	{-Get a record from the database }

function iGetIniIndexRecSize(Ini : PIniDatabase) : Integer;
	{-Get the size of the .INI index buffer }

function iAllocIniIndexRec(     Ini     : PIniDatabase;
														var IRec    : PAnsiChar;
														var BufSize : Integer) : Integer;
	{-Allocate a buffer for the record index }

procedure iDeallocIniIndexRec(     Ini     : PIniDatabase;
															 var IRec    : PAnsiChar;
																	 BufSize : Integer );
	{-Deallocate an index buffer }

function iLoadIniIndex(Ini : PIniDatabase; Buf : PAnsiChar; BufSize : Integer) : Integer;
	{-Load the INI index into Buf }

function iNumIniRecs(Ini : PIniDatabase) : Integer;
	{-Return the number of records in an INI database }

function iWriteToIni(Ini : PIniDatabase; var Rec; Section, IniFile : PAnsiChar) : Integer;
	{-Write the record to a user-specified .INI file }

function iReadFromIni(Ini : PIniDatabase; var Rec; Section, IniFile : PAnsiChar) : Integer;
  {-Read the record from a user-specified .INI file }

implementation

	procedure iInitIniDatabase(var Ini : PIniDatabase; FName : PAnsiChar);
		{-Initialize a .INI file database }
	begin

		Ini := AllocMem(SizeOf(TIniDatabase));

		Ini^.FName := AllocMem(StrLen(FName) + 1);

		StrCopy(Ini^.FName, FName);

		with Ini^ do begin
			DictionaryHead := nil;
			DictionaryTail := nil;
			NumRecords     := 0;
			RecordSize     := 0;
			DefaultRecord  := nil;
			Prepared       := False;
		end;

	end;

	procedure iDoneIniDatabase(var Ini : PIniDatabase);
		{-Destroy an .INI file database }
	var
		Temp : PIniDatabaseKey;

	begin
		with Ini^ do begin
			FreeMem(FName, StrLen(FName) + 1);
			while (DictionaryHead <> nil) do begin
				Temp := DictionaryHead^.Next;
				FreeMem(DictionaryHead^.KeyName, StrLen(DictionaryHead^.KeyName) + 1);
				FreeMem(DictionaryHead, SizeOf(TIniDatabaseKey));
				DictionaryHead := Temp;
			end;

			FreeMem(DefaultRecord, RecordSize);
		end;

		FreeMem(Ini, SizeOf(TIniDatabase));
	end;

	function iAddIniKeyPrim(Ini       : PIniDatabase;
													AKeyName  : PAnsiChar;
													AStrType  : Bool;
													AIndex    : Bool;
													ADataSize : Cardinal) : Integer;
		{-Add an .INI key with these attributes to the dictionary }
	var
		NewKey : PIniDatabaseKey;
	begin
		if ((DWORD(Ini^.RecordSize) + ADataSize) > $FFF0) or
				(AIndex and (Pred(ADataSize) > MaxIndexLen)) then begin
			iAddIniKeyPrim := ecDataTooLarge;
			Exit;
		end;

		if (StrLen(AKeyName) > MaxNameLen) then begin
			iAddIniKeyPrim := ecKeyTooLong;
			Exit;
		end;

		NewKey := AllocMem(SizeOf(TIniDatabaseKey));

		NewKey^.KeyName := AllocMem(StrLen(AKeyName) + 1);

		StrCopy(NewKey^.KeyName, AKeyName);

		with Ini^, NewKey^ do begin
			DataSize := ADataSize;
			StrType  := AStrType;
			Index    := AIndex;
			Next     := nil;

			if (DictionaryHead = nil) then begin
				DictionaryHead := NewKey;
				DictionaryTail := NewKey;
			end else begin
				DictionaryTail^.Next := NewKey;
				DictionaryTail       := NewKey;
			end;

			Inc(RecordSize, DataSize);
		end;

		iAddIniKeyPrim := ecOK;
	end;

	function iAddIniDBStringField(  Ini       : PIniDatabase;
                                  FieldName : PAnsiChar; MaxLen : Cardinal;
                                  Index     : Bool) : Integer;
    {-Add a string field to the .INI file database }
  begin
    iAddIniDBStringField := iAddIniKeyPrim(Ini, FieldName, True, Index, MaxLen + 1);
  end;

  function iAddIniDBIntField(Ini : PIniDatabase; FieldName : PAnsiChar) : Integer;
    {-Add an integer field to the .INI file database }
  begin
    iAddIniDBIntField := iAddIniKeyPrim(Ini, FieldName, False, False, SizeOf(Integer));
  end;

  function IniIndexKey(Ini : PIniDatabase) : PIniDatabaseKey;
    {-Return a pointer to the indexed key }
  var
    CurItem : PIniDatabaseKey;

  begin
    with Ini^ do begin
      CurItem := DictionaryHead;
      while (CurItem <> nil) do begin
        if CurItem^.Index then begin
          IniIndexKey := CurItem;
          Exit;
        end;
        CurItem := CurItem^.Next;
      end;
      IniIndexKey := nil;
    end;
  end;

	function iPrepareIniDatabase(Ini : PIniDatabase; Defaults : Pointer) : Integer;
		{-Prepare the databse for reading/writing }
	var
		CurItem   : PIniDatabaseKey;
		TempRec   : Pointer;
		Code      : Integer;
		TempStr   : array [0..255] of AnsiChar;
		TempFName : array [0..255] of AnsiChar;
		Existed   : Bool;

	begin
		with Ini^ do begin
			{ if there are no items defined, it's an error }
			if (DictionaryHead = nil) then
			begin
				iPrepareIniDatabase := ecNoFieldsDefined;
				Exit;
			end;

			if (IniIndexKey(Ini) = nil) then
			begin
				iPrepareIniDatabase := ecNoIndexKey;
				Exit;
			end;

			{ allocate the default data record }
			DefaultRecord := AllocMem(RecordSize);

			Existed := ExistFileZ(FName);
			if not Existed then
			begin
				JustFileNameZ(TempFName, FName);

				{is filename unqualified?}
				if (TempFName = FName) then
				begin
					GetWindowsDirectory(@TempFName[1], 255);
					AddBackslashZ(TempFName, TempFName);
					SysUtils.StrCat(TempFName, FName);
					Existed := ExistFileZ(TempFName);
				end;
			end;

			{ if the .INI file doesn't exist, create a default one }
			if not Existed then
			begin
				{ create the index section }
				if not WritePrivateProfileStringA( dbIndex,
																					dbBogus,
																					dbBogus,
																					FName ) then
				begin
					iPrepareIniDatabase := ecIniWrite;
					FreeMem(DefaultRecord, RecordSize);
					Exit;
				end;

				if not WritePrivateProfileStringA( dbIndex,
																					dbBogus,
																					nil,
																					FName ) then
				begin
					iPrepareIniDatabase := ecIniWrite;
					FreeMem(DefaultRecord, RecordSize);
					Exit;
				end;

				{ create the defaults section }
				if not WritePrivateProfileStringA( dbDefaults,
																					dbNumEntries,
																					Long2StrZ(TempStr, NumRecords),
																					FName ) then
				begin
					iPrepareIniDatabase := ecIniWrite;
					FreeMem(DefaultRecord, RecordSize);
					Exit;
				end;

				if (Defaults <> nil) then
				begin
					Prepared := True;
					Code := iChangeIniDefaults(Ini, Defaults^);
					if (Code < ecOK) then
					begin
						Prepared := False;
						iPrepareIniDatabase := Code;
						FreeMem(DefaultRecord, RecordSize);
						Exit;
					end;
				end;

				NumRecords := 0;
			end else
			begin
				{ load the number of database entries }
				NumRecords := GetPrivateProfileIntA( dbDefaults,
																						dbNumEntries,
																						0, FName );

				{ load the default record }
				TempRec := DefaultRecord;
				CurItem := DictionaryHead;
				while (CurItem <> nil) do
				begin
					if not CurItem^.Index then
						if CurItem^.StrType then
							GetPrivateProfileStringA(  dbDefaults,
																				CurItem^.KeyName,
																				'', PAnsiChar(TempRec),
																				CurItem^.DataSize,
																				FName )
						else
							Integer(TempRec^) := GetPrivateProfileIntA(  dbDefaults,
																													CurItem^.KeyName,
																													0, FName );

					TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
					CurItem := CurItem^.Next;
				end;
			end;

			Prepared := True;
		end;

		iPrepareIniDatabase := ecOK;
	end;

	function iChangeIniDefaults(Ini : PIniDatabase; var DefaultRec) : Integer;
		{-Change the default values for record fields }
	var
		CurItem : PIniDatabaseKey;
		TempRec : Pointer;
		TempStr : array[0..5] of AnsiChar;

	begin
		with Ini^ do begin
			{ if there are no items defined, it's an error }
			if (DictionaryHead = nil) then begin
				iChangeIniDefaults := ecNoFieldsDefined;
				Exit;
			end;

			if not Prepared then begin
				iChangeIniDefaults := ecDatabaseNotPrepared;
				Exit;
			end;

			Move(DefaultRec, DefaultRecord^, RecordSize);

			TempRec := DefaultRecord;
			CurItem := DictionaryHead;
			while (CurItem <> nil) do begin
				if not CurItem^.Index then
					if CurItem^.StrType then begin
						if not WritePrivateProfileStringA( dbDefaults,
																							CurItem^.KeyName,
																							PAnsiChar(TempRec),
																							FName ) then begin
							iChangeIniDefaults := ecIniWrite;
							Exit;
						end
					end else begin
						if not WritePrivateProfileStringA( dbDefaults,
																							CurItem^.KeyName,
																							Long2StrZ(TempStr, Integer(TempRec^)),
																							FName ) then begin
							iChangeIniDefaults := ecIniWrite;
							Exit;
						end;
					end;

				TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
				CurItem := CurItem^.Next;
			end;
		end;

		iChangeIniDefaults := ecOK;
	end;

	function iKeyExists(Ini : PIniDatabase; Key : PAnsiChar) : Bool;
		{-Return TRUE if an entry with an index of 'Name' exists }
	var
		Temp : array[0..5] of AnsiChar;
	begin
		if not Ini^.Prepared then begin
			iKeyExists := False;
			Exit;
    end;

		GetPrivateProfileStringA(dbIndex, Key, '', Temp, SizeOf(Temp), Ini^.FName);
    iKeyExists := (StrComp(Temp, NonValue) = 0);
  end;

	function iGetIniDataString(Ini : PIniDatabase; var Rec; Key : PIniDatabaseKey) : PAnsiChar;
    {-Get a string from an INI data record }
  var
    CurItem : PIniDatabaseKey;
    TempRec : Pointer;

  begin
    with Ini^ do begin
      CurItem := DictionaryHead;
      TempRec := @Rec;
      while (CurItem <> nil) and (CurItem <> Key) do begin
        TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
        CurItem := CurItem^.Next;
      end;
      if (CurItem = Key) then
        iGetIniDataString := PAnsiChar(TempRec)
      else
        iGetIniDataString := nil;
    end;
  end;

  function iUpdateIniRecCount(Ini : PIniDatabase) : Integer;
    {-Update the NumEntries field in the .INI file }
  var
    Temp : array[0..5] of AnsiChar;

  begin
    with Ini^ do begin
      Long2StrZ(Temp, NumRecords);
			if not WritePrivateProfileStringA(dbDefaults, dbNumEntries, Temp, FName) then
        iUpdateIniRecCount := ecIniWrite
      else
        iUpdateIniRecCount := ecOK;
    end;
  end;

	function iPutIniString(Ini : PIniDatabase; Name, Key, Str : PAnsiChar) : Bool;
    {-Put a string to the .INI file }
  var
		TempStr : array[0..Length(NonValue)] of AnsiCHar;

	begin
		{ if the string is intentionally left blank, exit }
		if (Str = nil) or (Str[0] = #0) then begin
			GetPrivateProfileStringA(Name, Key, '', TempStr, SizeOf(TempStr), Ini^.FName);
			if (StrComp(TempStr, NonValue) = 0) then begin
				iPutIniString := True;
				Exit;
			end;
		end;

		{ if the string <> '', write it out }
		if (Str <> nil) and (Str[0] <> #0) then
			iPutIniString := WritePrivateProfileStringA(Name, Key, Str, Ini^.FName)
		else
			{ if the string = '', delete its database entry }
			iPutIniString := WritePrivateProfileStringA(Name, Key, nil, Ini^.FName);
	end;

	function iSaveIniRecord(Ini : PIniDatabase; SecName : PAnsiChar; var Rec) : Integer;
		{-Save an INI record to the database }
	var
		CurItem : PIniDatabaseKey;
		TempRec : Pointer;
		TempStr : PAnsiChar;
		Temp    : array[0..5] of AnsiChar;

	begin
		with Ini^ do begin
			{ if there are no items defined, it's an error }
			if (DictionaryHead = nil) then begin
				iSaveIniRecord := ecNoFieldsDefined;
				Exit;
			end;

			if not Prepared then begin
				iSaveIniRecord := ecDatabaseNotPrepared;
				Exit;
			end;

			CurItem := DictionaryHead;
			TempRec := @Rec;
			while (CurItem <> nil) do begin
				if not CurItem^.Index then begin
					if CurItem^.StrType then
						TempStr := PAnsiChar(TempRec)
					else
						TempStr := Long2StrZ(Temp, Integer(TempRec^));

					if not iPutIniString(Ini, SecName, CurItem^.KeyName, TempStr) then begin
						iSaveIniRecord := ecIniWrite;
						Exit;
					end;
				end;

				TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
				CurItem := CurItem^.Next;
			end;
		end;

		iSaveIniRecord := ecOK;
	end;


	function iAddIniRecord(Ini : PIniDatabase; var Rec) : Integer;
		{-Add a record to the database }
	var
		IndexKey  : PIniDatabaseKey;
		IndexName : PAnsiChar;
		Code      : Integer;

	begin
		with Ini^ do begin
			{ if there are no items defined, it's an error }
			if (DictionaryHead = nil) then begin
				iAddIniRecord := ecNoFieldsDefined;
				Exit;
			end;

			if not Prepared then begin
				iAddIniRecord := ecDatabaseNotPrepared;
				Exit;
			end;

			if (NumRecords = MaxDBRecs) then begin
				iAddIniRecord := ecDatabaseFull;
				Exit;
			end;

			IndexKey  := IniIndexKey(Ini);
			IndexName := iGetIniDataString(Ini, Rec, IndexKey);

			if iKeyExists(Ini, IndexName) then begin
				iAddIniRecord := ecRecordExists;
				Exit;
			end;

			{ add the entry to the index }
			if not WritePrivateProfileStringA(dbIndex, IndexName, NonValue, FName) then begin
				iAddIniRecord := ecIniWrite;
				Exit;
			end;

			Code := iSaveIniRecord(Ini, IndexName, Rec);
			if (Code < ecOK) then begin
				iAddIniRecord := Code;
				Exit;
			end;

			Inc(Ini^.NumRecords);
			iAddIniRecord := iUpdateIniRecCount(Ini);
		end;
	end;

	function iUpdIniRecord(Ini : PIniDatabase; Key : PAnsiChar; var Rec) : Integer;
		{-Update a record in the database }
	var
		IndexKey  : PIniDatabaseKey;
		IndexName : PAnsiChar;
		Code      : Integer;

	begin
		with Ini^ do begin
			{ if there are no items defined, it's an error }
			if (DictionaryHead = nil) then begin
				iUpdIniRecord := ecNoFieldsDefined;
				Exit;
			end;

			if not Prepared then begin
				iUpdIniRecord := ecDatabaseNotPrepared;
				Exit;
			end;

			IndexKey  := IniIndexKey(Ini);
			IndexName := iGetIniDataString(Ini, Rec, IndexKey);

			if not iKeyExists(Ini, Key) then begin
				iUpdIniRecord := ecRecordNotFound;
				Exit;
			end;

			if (StrIComp(Key, IndexName) <> 0) then begin
				{ if the name has changed, first delete the old entry }
				Code := iDelIniRecord(Ini, Key);
				if (Code < ecOK) then begin
					iUpdIniRecord := Code;
					Exit;
				end;

				{ add a new entry }
				iUpdIniRecord := iAddIniRecord(Ini, Rec);
			end else
				iUpdIniRecord := iSaveIniRecord(Ini, Key, Rec);
		end;
	end;

	function iDelIniRecord(Ini : PIniDatabase; Key : PAnsiChar) : Integer;
    {-Remove a record from the database }
  begin
    with Ini^ do begin
      { if there are no items defined, it's an error }
      if (DictionaryHead = nil) then begin
        iDelIniRecord := ecNoFieldsDefined;
        Exit;
      end;

      if not Prepared then begin
        iDelIniRecord := ecDatabaseNotPrepared;
        Exit;
      end;

      if not iKeyExists(Ini, Key) then begin
        iDelIniRecord := ecRecordNotFound;
        Exit;
      end;

      iDelIniRecord := ecIniWrite;

      { delete the index entry }
			if not WritePrivateProfileStringA(dbIndex, Key, nil, FName) then
        Exit;

      { delete the record }
			if not WritePrivateProfileStringA(Key, nil, nil, FName) then
        Exit;

      { update the record count }
      Dec(NumRecords);
      iDelIniRecord := iUpdateIniRecCount(Ini);
    end;
  end;

	function iGetIniRecord(Ini : PIniDatabase; Key : PAnsiChar; var Rec) : Integer;
		{-Get a record from the database }
	var
		TempRec : Pointer;
		DefRec  : Pointer;
		CurItem : PIniDatabaseKey;

	begin
		with Ini^ do begin
			{ if there are no items defined, it's an error }
			if (DictionaryHead = nil) then begin
				iGetIniRecord := ecNoFieldsDefined;
				Exit;
			end;

			if not Prepared then begin
				iGetIniRecord := ecDatabaseNotPrepared;
				Exit;
			end;

			if not iKeyExists(Ini, Key) then begin
				iGetIniRecord := ecRecordNotFound;
				Exit;
			end;

			TempRec := @Rec;
			DefRec  := DefaultRecord;

			CurItem := DictionaryHead;
			while (CurItem <> nil) do begin
				if CurItem^.StrType then
					if CurItem^.Index then
						StrCopy(PAnsiChar(TempRec), Key)
					else begin
						GetPrivateProfileStringA(  Key,
																			CurItem^.KeyName,
																			PAnsiChar(DefRec),
																			PAnsiChar(TempRec),
																			CurItem^.DataSize,
																			FName );

						if (StrComp(PChar(TempRec), NonValue) = 0) then
							PChar(TempRec)[0] := #0;
					end
				else
					Integer(TempRec^) := GetPrivateProfileIntA(  Key,
																											CurItem^.KeyName,
																											Integer(DefRec^),
																											FName );

				TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
				DefRec  := AddWordToPtr(DefRec, CurItem^.DataSize);
				CurItem := CurItem^.Next;
			end;

			iGetIniRecord := ecOK;
		end;
	end;

  function iGetIniIndexRecSize(Ini : PIniDatabase) : Integer;
    {-Get the size of the .INI index buffer }
  begin
    iGetIniIndexRecSize := (Ini^.NumRecords * MaxIndexLen) + 1;
  end;

  function iAllocIniIndexRec(     Ini     : PIniDatabase;
                              var IRec    : PAnsiChar;
                              var BufSize : Integer) : Integer;
    {-Allocate a buffer for the record index }
  begin
    with Ini^ do begin
      BufSize := iGetIniIndexRecSize(Ini);
      if (BufSize = 0) then begin
        iAllocIniIndexRec := ecDatabaseEmpty;
        Exit;
      end;

      IRec := AllocMem(BufSize);
      iAllocIniIndexRec := ecOK;
    end;
  end;

  procedure iDeallocIniIndexRec(     Ini     : PIniDatabase;
																 var IRec    : PAnsiChar;
                                     BufSize : Integer );
    {-Deallocate an index buffer }
  begin
    FreeMem(IRec, BufSize);
  end;

	function iLoadIniIndex(Ini : PIniDatabase; Buf : PAnsiChar; BufSize : Integer) : Integer;
		{-Load the INI index into Buf }
	begin
		if (Ini^.NumRecords = 0) then begin
			iLoadIniIndex := ecDatabaseEmpty;
			Exit;
		end;

		if Ini^.Prepared then
			if (GetPrivateProfileStringA(dbIndex, nil, nil, Buf, BufSize, Ini^.FName) = 0) then
				iLoadIniIndex := ecIniRead
			else
				iLoadIniIndex := ecOK
		else
			iLoadIniIndex := ecDatabaseNotPrepared;
	end;

  function iNumIniRecs(Ini : PIniDatabase) : Integer;
    {-Return the number of records in an INI database }
  begin
    iNumIniRecs := Ini^.NumRecords;
  end;

  function iWriteToIni(Ini : PIniDatabase; var Rec; Section, IniFile : PAnsiChar) : Integer;
    {-Write the record to a user-specified .INI file }
  var
    CurItem : PIniDatabaseKey;
    TempRec : Pointer;
		TempStr : PAnsiChar;
    Temp    : array[0..5] of AnsiChar;

  begin
    with Ini^ do begin
      { if there are no items defined, it's an error }
      if (DictionaryHead = nil) then begin
        iWriteToIni := ecNoFieldsDefined;
        Exit;
      end;

      if not Prepared then begin
        iWriteToIni := ecDatabaseNotPrepared;
        Exit;
      end;

      CurItem := DictionaryHead;
      TempRec := @Rec;
      while (CurItem <> nil) do begin
        if CurItem^.StrType then
          TempStr := PAnsiChar(TempRec)
        else
          TempStr := Long2StrZ(Temp, Integer(TempRec^));

				if not WritePrivateProfileStringA(Section, CurItem^.KeyName, TempStr, IniFile) then begin
					iWriteToIni := ecIniWrite;
          Exit;
        end;

        TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
        CurItem := CurItem^.Next;
      end;
    end;

		iWriteToIni := ecOK;
	end;

	function iReadFromIni(Ini : PIniDatabase; var Rec; Section, IniFile : PAnsiChar) : Integer;
		{-Read the record from a user-specified .INI file }
	var
		TempRec : Pointer;
		DefRec  : Pointer;
		CurItem : PIniDatabaseKey;

	begin
		with Ini^ do begin
			{ if there are no items defined, it's an error }
			if (DictionaryHead = nil) then begin
				iReadFromIni := ecNoFieldsDefined;
				Exit;
			end;

			if not Prepared then begin
				iReadFromIni := ecDatabaseNotPrepared;
				Exit;
			end;

			FillChar(Rec, RecordSize, 0);
			TempRec := @Rec;
			DefRec  := DefaultRecord;
			CurItem := DictionaryHead;
			while (CurItem <> nil) do begin
				if CurItem^.StrType then
					GetPrivateProfileStringA(Section,
																	CurItem^.KeyName,
																	PAnsiChar(DefRec),
																	PAnsiChar(TempRec),
																	CurItem^.DataSize,
																	IniFile)
				else
					Integer(TempRec^) := GetPrivateProfileIntA(Section,
																										CurItem^.KeyName,
																										Integer(DefRec^),
																										IniFile);

				TempRec := AddWordToPtr(TempRec, CurItem^.DataSize);
				DefRec  := AddWordToPtr(DefRec, CurItem^.DataSize);
				CurItem := CurItem^.Next;
			end;
		end;

		iReadFromIni := ecOK;
	end;

end.
