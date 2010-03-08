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
 *    Sulaiman Mah
 *    Sean B. Durkin
 *    Sebastian Zierer
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{*                   ADSELCOM.PAS 5.00                   *}
{*********************************************************}
{* Port selection dialog, IsPortAvailable method         *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I ..\..\includes\AWDEFINE.INC}


unit AdSelCom;
	{-Com port selection dialog}
interface

uses
  Windows,
  SysUtils,
  Classes,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  OoMisc,
  AwUser,
{$IFNDEF UseAwWin32}
  LnsWin32;
{$ELSE}
  AwWin32;
{$ENDIF}

type
  TComSelectForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    OkBtn: TBitBtn;
    AbortBtn: TBitBtn;
    Bevel1: TBevel;
    PortsComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
  private
  public
    function SelectedCom : String;
    function SelectedComNum : Word;
  end;

function IsPortAvailable(ComNum : Cardinal) : Boolean;

const
  {True to create a dispatcher to validate the port; false to open the
   port using direct API calls}
  UseDispatcherForAvail : Boolean = True;
  {True to return True even if the port is in use; False to return False
   if the port is in use}
  ShowPortsInUse : Boolean = True;                                  
implementation

{$R *.DFM}

function IsPortAvailable( ComNum : Cardinal) : Boolean;

var
  ComName : string;
  Res : Integer;
  DeviceLayer : TApdBaseDispatcher;
  CC: TCommConfig;
  Len: Cardinal;
begin
ComName := Format( '\\.\COM%d', [ComNum]);
DeviceLayer := nil;
try
  if ComNum = 0 then
      result := False
    else if UseDispatcherForAvail then
      begin
      DeviceLayer  := TApdWin32Dispatcher.Create(nil);
      if ShowPortsInUse then
          result := DeviceLayer.CheckPort( ComName)
        else
          begin
          Res := DeviceLayer.OpenCom( ComName, 64, 64);
          result := (Res >= 0) or (ShowPortsInUse and
                      (GetLastError = DWORD( ecAccessDenied)));
          if Res >= 0 then
            DeviceLayer.CloseCom
          end;
      end
    else
      begin
      if ShowPortsInUse then  //SZ: optimize this one - otherwise bluetooth devices may request confirmation
          begin
          Len := SizeOf(CC);
          FillChar(CC, Len, 0);
          CC.dwSize := Len;
          result := GetDefaultCommConfig( PChar( ComName), CC, Len)
          end
        else
          begin
          Res := CreateFile(
                   PChar( ComName),
                   GENERIC_READ or GENERIC_WRITE,
                   0,
                   nil,
                   OPEN_EXISTING,
                   FILE_ATTRIBUTE_NORMAL or
                   FILE_FLAG_OVERLAPPED,
                   0);
          result := (Res >= 0) or (ShowPortsInUse and
                      (GetLastError = DWORD( ecAccessDenied)));
          if Res >= 0 then
            CloseHandle( Res)
          end
      end
finally
  if UseDispatcherForAvail then
    DeviceLayer.Free
end end;


procedure TComSelectForm.FormCreate( Sender: TObject);
var
  I : Integer;
  S : string;
begin
  for I := 1 to MaxComHandles do
    if IsPortAvailable(I) then begin
      S := Format('COM%d', [I]);
      PortsComboBox.Items.Add(S);
    end;
  PortsComboBox.ItemIndex := 0;
end;

function TComSelectForm.SelectedCom : String;
begin
  Result := PortsComboBox.Items[PortsComboBox.ItemIndex];
end;

function TComSelectForm.SelectedComNum : Word;
var
  S : String;
begin
  S := PortsComboBox.Items[PortsComboBox.ItemIndex];
  S := Copy(S, 4, 255);
  Result := StrToInt(S);
end;

end.
