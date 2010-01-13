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
{*                   ADKEYED.PAS 4.06                    *}
{*********************************************************}
{* Deprecated keyboard emulator editor dialog            *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

unit AdKeyEd;
  {-Keyboard Emulator File Editor Dialog-}

interface

uses
  WinTypes,
  WinProcs,
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Forms,
  Graphics,
  Dialogs,
  Buttons,
  Tabs,
  ExtCtrls,
  OoMisc,
  AwKeyEmu,
  AdExcept,
  AdTerm;

const
  MaxKeyTableRows = 7;
  MaxKeyTableCols = 23;

type
  PKeyTableItem = ^KeyTableItem;
  KeyTableItem = record
    Number   : Word;
    Location : TRect;
    State    : TButtonState;
    KeyCode  : Word;
    Caption  : PChar;
  end;

  KeyTable = record
    TotalKeys : Word;
    Table : array[0..(MaxKeyTableCols*MaxKeyTableRows)-1] of KeyTableItem;
  end;

  TOnKeyClickEvent = procedure(Sender: TObject; Code: Word; Text: String) of Object;

  TKeyLayoutMap = class(TCustomControl)
  private
    KTable      : KeyTable;
    Captured    : PKeyTableItem;
    FOnKeyClick : TOnKeyClickEvent;
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
    procedure Populate;
    property OnKeyClick : TOnKeyClickEvent
      read FOnKeyClick write FOnKeyClick;
  end;

  TKMEditor = class(TForm)
    KeyboardPanel      : TPanel;
    EmuTypesTab        : TTabSet;
    MappingsGroupBox   : TGroupBox;
    NormalLabel        : TLabel;
    CtrlKeyLabel       : TLabel;
    ShiftKeyLabel      : TLabel;
    CtrlShitLabel      : TLabel;
    CtrlAltLabel       : TLabel;
    CtrlAltShitLabel   : TLabel;
    NormalEditBox      : TEdit;
    CtrlKeyEditBox     : TEdit;
    ShiftKeyEditBox    : TEdit;
    CtrlShiftEditBox   : TEdit;
    CtrlAltEditBox     : TEdit;
    CtrlAltShiftEditBox: TEdit;
    OkButton           : TBitBtn;
    CancelButton       : TBitBtn;
    NewTypeButton      : TButton;
    LoadFileButton     : TButton;
    SaveFileButton     : TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure KeyboardKeyClick(Sender: TObject; Code: Word; Text: String);
    procedure EmuTypesTabChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure NormalEditBoxExit(Sender: TObject);
    procedure CtrlKeyEditBoxExit(Sender: TObject);
    procedure ShiftKeyEditBoxExit(Sender: TObject);
    procedure CtrlShiftEditBoxExit(Sender: TObject);
    procedure CtrlAltEditBoxExit(Sender: TObject);
    procedure CtrlAltShiftEditBoxExit(Sender: TObject);
    procedure LoadFileButtonClick(Sender: TObject);
    procedure SaveFileButtonClick(Sender: TObject);
    procedure NewTypeButtonClick(Sender: TObject);
  private
    { Private declarations }
    KeyLayout   : TKeyLayoutMap;
    FFileName   : string;
    FKeyEmuData : Pointer;
    FLastKeyCode: Word;
    FFileModified : Boolean;

    procedure SetFileName(Value: String);
    procedure UpdateKeymap(Code: Word; ShiftState: Byte; Map: String);
  public
    { Public declarations }
    property FileName : string
      read FFileName write SetFileName;
  end;

var
  KMEditor: TKMEditor;

implementation

{$R *.DFM}

type
  LayoutRec = record
    KeyCode: Word;
    Name   : PChar;
  end;

const
  { KeyCodes with $00 are disabled}
  Keyboard101Layout : array[0..100] of LayoutRec = (
    (KeyCode: $1B; Name: 'Esc' ), (KeyCode: $70; Name: 'F1'  ),
    (KeyCode: $71; Name: 'F2'  ), (KeyCode: $72; Name: 'F3'  ),
    (KeyCode: $73; Name: 'F4'  ), (KeyCode: $74; Name: 'F5'  ),
    (KeyCode: $75; Name: 'F6'  ), (KeyCode: $76; Name: 'F7'  ),
    (KeyCode: $77; Name: 'F8'  ), (KeyCode: $78; Name: 'F9'  ),
    (KeyCode: $79; Name: 'F10' ), (KeyCode: $7A; Name: 'F11' ),
    (KeyCode: $7B; Name: 'F12' ), (KeyCode: $2C; Name: 'Psn' ),
    (KeyCode: $00; Name: 'ScLk'), (KeyCode: $00; Name: 'Paus'),
    (KeyCode: $00; Name: '`'   ), (KeyCode: $31; Name: '1'   ),
    (KeyCode: $32; Name: '2'   ), (KeyCode: $33; Name: '3'   ),
    (KeyCode: $34; Name: '4'   ), (KeyCode: $35; Name: '5'   ),
    (KeyCode: $36; Name: '6'   ), (KeyCode: $37; Name: '7'   ),
    (KeyCode: $38; Name: '8'   ), (KeyCode: $39; Name: '9'   ),
    (KeyCode: $30; Name: '0'   ), (KeyCode: $00; Name: '-'   ),
    (KeyCode: $00; Name: '='   ), (KeyCode: $08; Name: 'Back'),
    (KeyCode: $2D; Name: 'Ins' ), (KeyCode: $24; Name: 'Home'),
    (KeyCode: $21; Name: 'PgUp'), (KeyCode: $00; Name: 'Num' ),
    (KeyCode: $6F; Name: '/'   ), (KeyCode: $6A; Name: '*'   ),
    (KeyCode: $6C; Name: '-'   ), (KeyCode: $09; Name: 'Tab' ),
    (KeyCode: $00; Name: 'Q'   ), (KeyCode: $00; Name: 'W'   ),
    (KeyCode: $00; Name: 'E'   ), (KeyCode: $00; Name: 'R'   ),
    (KeyCode: $00; Name: 'T'   ), (KeyCode: $00; Name: 'Y'   ),
    (KeyCode: $00; Name: 'U'   ), (KeyCode: $00; Name: 'I'   ),
    (KeyCode: $00; Name: 'O'   ), (KeyCode: $00; Name: 'P'   ),
    (KeyCode: $00; Name: '['   ), (KeyCode: $00; Name: ']'   ),
    (KeyCode: $00; Name: '\'   ), (KeyCode: $2E; Name: 'Del' ),
    (KeyCode: $23; Name: 'End' ), (KeyCode: $22; Name: 'PgDn'),
    (KeyCode: $67; Name: '7'   ), (KeyCode: $68; Name: '8'   ),
    (KeyCode: $69; Name: '9'   ), (KeyCode: $6B; Name: '+'   ),
    (KeyCode: $00; Name: 'Caps'), (KeyCode: $00; Name: 'A'   ),
    (KeyCode: $00; Name: 'S'   ), (KeyCode: $00; Name: 'D'   ),
    (KeyCode: $00; Name: 'F'   ), (KeyCode: $00; Name: 'G'   ),
    (KeyCode: $00; Name: 'H'   ), (KeyCode: $00; Name: 'J'   ),
    (KeyCode: $00; Name: 'K'   ), (KeyCode: $00; Name: 'L'   ),
    (KeyCode: $00; Name: ';'   ), (KeyCode: $00; Name: ''''  ),
    (KeyCode: $00; Name: 'Enter'),(KeyCode: $64; Name: '4'   ),
    (KeyCode: $65; Name: '5'   ), (KeyCode: $66; Name: '6'   ),
    (KeyCode: $00; Name: 'Shift'),(KeyCode: $00; Name: 'Z'   ),
    (KeyCode: $00; Name: 'X'   ), (KeyCode: $00; Name: 'C'   ),
    (KeyCode: $00; Name: 'V'   ), (KeyCode: $00; Name: 'B'   ),
    (KeyCode: $00; Name: 'N'   ), (KeyCode: $00; Name: 'M'   ),
    (KeyCode: $00; Name: ','   ), (KeyCode: $00; Name: '.'   ),
    (KeyCode: $00; Name: '/'   ), (KeyCode: $00; Name: 'Shift'),
    (KeyCode: $26; Name: #24   ), (KeyCode: $61; Name: '1'   ),
    (KeyCode: $62; Name: '2'   ), (KeyCode: $63; Name: '3'   ),
    (KeyCode: $00; Name: 'Ent' ), (KeyCode: $00; Name: 'Ctrl'),
    (KeyCode: $00; Name: 'Alt' ), (KeyCode: $00; Name: 'Space'),
    (KeyCode: $00; Name: 'Alt' ), (KeyCode: $00; Name: 'Ctrl'),
    (KeyCode: $25; Name: #27   ), (KeyCode: $28; Name: #25   ),
    (KeyCode: $27; Name: #26   ), (KeyCode: $60; Name: '0'   ),
    (KeyCode: $6E; Name: '.'   ));

constructor TKeyLayoutMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csClickEvents, csCaptureMouse];
  Parent := TWinControl(AOwner);
  Captured := nil;
  KTable.TotalKeys := 0;
end;

procedure TKeyLayoutMap.Paint;
var
  R : Byte;

  procedure DrawKeyText(Caption: PChar; Bounds: TRect; State: TButtonState);
  begin
    with Canvas do begin
      Font.Name := 'Terminal';
      Font.Size := 6;
      Brush.Style := bsClear;
      if State = bsDisabled then begin
        OffSetRect(Bounds, 1, 1);
        Font.Color := clWhite;
        DrawText(Handle, Caption, StrLen(Caption), Bounds,
                 DT_CENTER or DT_VCENTER or DT_SINGLELINE);
        OffSetRect(Bounds, -1, -1);
        Font.Color := clDkGray;
        DrawText(Handle, Caption, StrLen(Caption), Bounds,
                 DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      end else begin
        Font.Color := clBlack;
        if State = bsDown then
          OffSetRect(Bounds, 1, 1);
        DrawText(Handle, Caption, StrLen(Caption), Bounds,
                 DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      end;
    end;
  end;

begin
  with Canvas do begin
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;

    for R := 0 to KTable.TotalKeys-1 do begin
      with KTable.Table[R] do begin
        DrawButtonFace(Canvas, Location, 1, bsAutoDetect, False,
                      (State = bsDown), False);
        DrawKeyText(Caption, Location, State);
      end;
    end;
  end;
end;

procedure TKeyLayoutMap.MouseDown(Button: TMouseButton; Shift: TShiftState;
                              X, Y: Integer);
var
  MousePoint : TPoint;
  I : Word;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) then begin
    MousePoint := Point(X, Y);
    for I := 0 to KTable.TotalKeys-1 do begin
      with KTable.Table[I] do begin
        if PtInRect(Location, MousePoint) then begin
          if (not (State = bsDown)) and (State <> bsDisabled) then begin
            State := bsDown;
            InvalidateRect(Handle, @Location, False);
            Captured := @KTable.Table[I];
          end;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TKeyLayoutMap.MouseUp(Button: TMouseButton; Shift: TShiftState;
                            X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Captured = nil then
    Exit;
  if (Button = mbLeft) then begin
    if PtInRect(Captured^.Location, Point(X, Y)) then begin
      with Captured^ do begin
        if State = bsDown then begin
          State := bsUp;
          InvalidateRect(Handle, @Location, False);
          if Assigned(FOnKeyClick) then
            FOnKeyClick(Self, KeyCode, StrPas(Caption));
         end;
      end;
    end;
    Captured := nil;
  end;
end;

procedure TKeyLayoutMap.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if Captured <> nil then begin
    with Captured^ do begin
      if not (PtInRect(Location, Point(X, Y))) and (State = bsDown) then begin
        State := bsUp;
        InvalidateRect(Handle, @Location, False);
      end;
      if (PtInRect(Location, Point(X, Y))) and (State = bsUp) then begin
        State := bsDown;
        InvalidateRect(Handle, @Location, False);
      end;
    end;
  end;
end;

procedure TKeyLayoutMap.Populate;
var
  I : Integer;
  xSBWidth, xSBHeight : Integer;
begin
  xSBWidth := (ClientWidth div MaxKeyTableCols);
  xSBHeight:= (ClientHeight div MaxKeyTableRows);

  with KTable do begin
    for I := 0 to 15 do begin
      Table[I].Number := I;
      Inc(TotalKeys, 1);
      with Table[I].Location do begin
        Top    := 0;
        Bottom := Top + xSBHeight;
        case I of
          0     : Left := 0;
          1..4  : Left := ((I+1)*xSBWidth);
          5..8  : Left := ((I+1)*xSBWidth)+(xSBWidth div 2);
          9..12 : Left := ((I+2)*xSBWidth);
          13..15: Left := ((I+2)*xSBWidth)+(xSBWidth div 2);
        end;
        Right  := Left + xSBWidth;
      end;
    end;

    for I := 0 to 20 do begin
      Table[I+16].Number := I+16;
      Inc(TotalKeys, 1);
      with Table[I+16].Location do begin
        Top    := 2*xSBHeight;
        Bottom := Top + xSBHeight;
        Right := 0;
        case I of
          0..12 : Left  := I*xSBWidth;
          13    : begin
                    Left  := I*xSBWidth;
                    Right := xSBWidth;
                  end;
          14..16: Left  := (I+1)*xSBWidth+(xSBWidth div 2);
          17..20: Left  := (I+2)*xSBWidth;
        end;
        Right := Right + Left + xSBWidth;
      end;
    end;

    for I := 0 to 20 do begin
      Table[I+37].Number := I+37;
      Inc(TotalKeys, 1);
      with Table[I+37].Location do begin
        Top    := 3*xSBHeight;
        Bottom := Top + xSBHeight;
        Right := 0;
        case I of
          0     : begin
                    Left  := I*xSBWidth;
                    Right := (xSBWidth div 2);
                  end;
          1..12 : Left  := I*xSBWidth+(xSBWidth div 2);
          13    : begin
                    Left  := I*xSBWidth+(xSBWidth div 2);
                    Right := (xSBWidth div 2)+1;
                  end;
          14..16: Left  := (I+1)*xSBWidth+(xSBWidth div 2);
          17..19: Left  := (I+2)*xSBWidth;
          20    : begin
                    Bottom := Bottom+xSBHeight;
                    Left  := (I+2)*xSBWidth;
                  end;
        end;
        Right := Right + Left + xSBWidth;
      end;
    end;

    for I := 0 to 15 do begin
      Table[I+58].Number := I+58;
      Inc(TotalKeys, 1);
      with Table[I+58].Location do begin
        Top    := 4*xSBHeight;
        Bottom := Top + xSBHeight;
        Right := 0;
        case I of
          0     : begin
                    Left  := I*xSBWidth;
                    Right := (xSBWidth*3) div 4;
                  end;
          1..11 : Left := (I*xSBWidth)+((xSBWidth*3) div 4);
          12    : begin
                    Left  := (I*xSBWidth)+((xSBWidth*3) div 4);
                    Right := ((xSBWidth*4) div 3)-1;
                  end;
          13..15: Left  := (I+6)*xSBWidth;
        end;
        Right := Right + Left + xSBWidth;
      end;
    end;

    for I := 0 to 16 do begin
      Table[I+74].Number := I+74;
      Inc(TotalKeys, 1);
      with Table[I+74].Location do begin
        Top    := 5*xSBHeight;
        Bottom := Top + xSBHeight;
        Right := 0;
        case I of
          0     : begin
                    Left  := I*xSBWidth;
                    Right := (xSBWidth div 2) + ((xSBWidth*3) div 4);
                  end;
          1..10 : Left := (I*xSBWidth)+(xSBWidth div 2)+((xSBWidth*3) div 4);
          11    : begin
                    Left := (I*xSBWidth)+(xSBWidth div 2)+((xSBWidth*3) div 4);
                    Right := xSBWidth+((xSBWidth*4) div 5);
                  end;
          12    : Left  := (I+4)*xSBWidth+(xSBWidth div 2);
          13..15: Left  := (I+6)*xSBWidth;
          16    : begin
                    Bottom := Bottom+xSBHeight;
                    Left  := (I+6)*xSBWidth;
                  end;
        end;
        Right := Right + Left + xSBWidth;
      end;
    end;

    for I := 0 to 9 do begin
      Table[I+91].Number := I+91;
      Inc(TotalKeys, 1);
      with Table[I+91].Location do begin
        Top    := 6*xSBHeight;
        Bottom := Top + xSBHeight;
        Right := 0;
        case I of
          0     : begin
                    Left  := I*xSBWidth;
                    Right := (xSBWidth div 2);
                  end;
          1     : begin
                    Left  := ((I+1)*xSBWidth)+(xSBWidth div 2);
                    Right := (xSBWidth div 2)+1;
                  end;
          2     : begin
                    Left  := ((I+2)*xSBWidth);
                    Right := xSBWidth*6;
                  end;
          3     : begin
                    Left := ((I+8)*xSBWidth);
                    Right := (xSBWidth div 2)+1;
                  end;
          4     : begin
                    Left := ((I+9)*xSBWidth)+(xSBWidth div 2)+1;
                    Right := (xSBWidth div 2);
                  end;
          5..7  : Left  := (I+10)*xSBWidth+(xSBWidth div 2);
          8     : begin
                    Left  := (I+11)*xSBWidth;
                    Right := xSBWidth;
                  end;
          9     : Left  := (I+12)*xSBWidth;
        end;
        Right := Right + Left + xSBWidth;
      end;
    end;
    for I := 0 to TotalKeys do begin
      Table[I].KeyCode := KeyBoard101Layout[I].KeyCode;
      Table[I].Caption := KeyBoard101Layout[I].Name;
      if KeyBoard101Layout[I].KeyCode <> $00 then
        Table[I].State := bsUp
      else
        Table[I].State := bsDisabled;
    end;
  end;
end;

{---------------}

procedure TKMEditor.FormCreate(Sender: TObject);
begin
  FKeyEmuData := nil;
  FLastKeyCode := 0;
  FFileModified := False;
  KeyLayout := TKeyLayoutMap.Create(KeyboardPanel);
  KeyLayout.SetBounds(KeyboardPanel.Left+2, KeyboardPanel.Top+2,
                      KeyboardPanel.Width,  KeyboardPanel.Height);
  KeyLayout.Populate;
  KeyLayout.OnKeyClick := KeyboardKeyClick;
end;

procedure TKMEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FKeyEmuData <> nil then
    kbDoneKeyEmulator(PKeyEmulator(FKeyEmuData));
  KeyLayout.Free;
end;

procedure TKMEditor.SetFileName(Value: String);
var
  TempFileName : Array[00..255] of char;
  TempIndex    : PChar;
begin
  if Value <> FFileName then begin
    FFileName := Value;
    if FKeyEmuData <> nil then begin
      kbDoneKeyEmulator(PKeyEmulator(FKeyEmuData));
      FKeyEmuData := nil;
      FFileModified := False;
    end;
    if kbInitKeyEmulator(PKeyEmulator(FKeyEmuData),
                         StrPCopy(TempFileName, FFileName)) = ecOK then begin
      EmuTypesTab.Tabs.Clear;
      if kbNumEmuTypes(PKeyEmulator(FKeyEmuData)) > 0 then begin
        if CheckException(Self,
              kbLoadKeyEmuIndex(PKeyEmulator(FKeyEmuData))) = ecOK then begin
          TempIndex := PKeyEmulator(FKeyEmuData)^.kbKeyNameList;
          while (TempIndex^ <> #0) do begin
            EmuTypesTab.Tabs.Add(StrPas(TempIndex));
            Inc(TempIndex, StrLen(TempIndex) + 1);
          end;
          EmuTypesTab.TabIndex := 0;
        end;
      end;
    end;
  end;
end;

function CvtCodeToStr(Value: String): String;
var
  J : Byte;
begin
  Result := '';
  for J := 1 to Length(Value) do begin
    Case Value[J] of
      #27 : Result := Result + '<ESC>';
     else
      Result := Result + Value[J];
    end;
  end;
end;

function CvtStrToCode(Value: String): String;
var
  J : Byte;
begin
  J:= Pos('<ESC>', Value);
  while J <> 0 do begin
    if J > 1 then
      Value := Copy(Value, 1, J-1)+#27+Copy(Value, J+5, Length(Value))
    else
      Value := #27+Copy(Value, J+5, Length(Value));
    J:= Pos('<ESC>', Value);
  end;
  Result := Value;
end;

procedure TKMEditor.UpdateKeymap(Code: Word; ShiftState: Byte; Map: String);
var
  EmptyKey : Word;
  Updated : Boolean;
  I : Word;
begin
  if FKeyEmuData = nil then
    Exit;
  FFileModified := True;
  with PKeyEmulator(FKeyEmuData)^ do begin
    Updated := False;
    EmptyKey := 0;
    for I := 1 to ApdMaxKeyMaps do begin                               {!!.03}
      if (Code = kbKeyMap[I].KeyCode) and (ShiftState = kbKeyMap[I].ShiftState)
      then begin
        kbKeyMap[I].Mapping := Map;
        Updated := True;
        Break;
      end else begin
        if (EmptyKey = 0) and (kbKeyMap[I].Mapping = '') then
          EmptyKey := I;
      end;
    end;
    if not Updated then begin
      if EmptyKey > 0 then begin
        kbKeyMap[EmptyKey].Mapping := Map;
        kbKeyMap[EmptyKey].KeyCode := Code;
        kbKeyMap[EmptyKey].ShiftState := ShiftState;
      end;
    end;
  end;
end;

procedure TKMEditor.KeyboardKeyClick(Sender: TObject; Code: Word; Text: String);
var
  I : Word;
begin
  if FKeyEmuData = nil then
    Exit;

  { Check for modifications of previous contents}
  NormalEditBoxExit(Self);
  CtrlKeyEditBoxExit(Self);
  CtrlAltEditBoxExit(Self);
  ShiftKeyEditBoxExit(Self);
  CtrlShiftEditBoxExit(Self);
  CtrlAltShiftEditBoxExit(Self);
                                                          
  MappingsGroupBox.Caption := 'Mappings - ['+Text+']';

  FLastKeyCode := Code;

  NormalEditBox.Text := '';
  CtrlKeyEditBox.Text := '';
  CtrlAltEditBox.Text := '';
  ShiftKeyEditBox.Text := '';
  CtrlShiftEditBox.Text := '';
  CtrlAltShiftEditBox.Text := '';

  with PKeyEmulator(FKeyEmuData)^ do begin
    for I := 1 to ApdMaxKeyMaps do begin                               {!!.03}
      with kbKeyMap[I] do begin
        if Code = KeyCode then begin
          case ShiftState of
            $0E : NormalEditBox.Text := CvtCodeToStr(Mapping);
            $0C : CtrlKeyEditBox.Text := CvtCodeToStr(Mapping);
            $08 : CtrlAltEditBox.Text := CvtCodeToStr(Mapping);
            $06 : ShiftKeyEditBox.Text := CvtCodeToStr(Mapping);
            $04 : CtrlShiftEditBox.Text := CvtCodeToStr(Mapping);
            $00 : CtrlAltShiftEditBox.Text := CvtCodeToStr(Mapping);
          end;
        end;
      end;
    end;
  end;

  NormalEditBox.Modified := False;
  CtrlKeyEditBox.Modified := False;
  CtrlAltEditBox.Modified := False;
  ShiftKeyEditBox.Modified := False;
  CtrlShiftEditBox.Modified := False;
  CtrlAltShiftEditBox.Modified := False;
end;

procedure TKMEditor.EmuTypesTabChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  EmuName : TKeyMapName;
  MsgResult : Word;
begin
  KeyboardKeyClick(Self, 0, '');
  if FFileModified and (FLastKeyCode <> 0) then begin
      MsgResult := MessageDlg('The current Key Map has been modified.  Save?',
                            mtWarning, mbYesNoCancel, 0);
    case MsgResult of
      mrYes : SaveFileButtonClick(Self);
      mrCancel : begin
                   AllowChange := False;
                   Exit;
                 end;
      mrNo : FFileModified := False;
    end;
  end;
  kbSetKeyEmuType(PKeyEmulator(FKeyEmuData), StrPCopy(EmuName, EmuTypesTab.Tabs[NewTab]));
  kbLoadKeyEmuMap(PKeyEmulator(FKeyEmuData));
end;

procedure TKMEditor.NormalEditBoxExit(Sender: TObject);
begin
  if NormalEditBox.Modified then begin
    UpdateKeyMap(FLastKeyCode, $0E, CvtStrToCode(NormalEditBox.Text));
    NormalEditBox.Modified := False;
  end;
end;

procedure TKMEditor.CtrlKeyEditBoxExit(Sender: TObject);
begin
  if CtrlKeyEditBox.Modified then begin
    UpdateKeyMap(FLastKeyCode, $0C, CvtStrToCode(CtrlKeyEditBox.Text));
    CtrlKeyEditBox.Modified := False;
  end;
end;

procedure TKMEditor.ShiftKeyEditBoxExit(Sender: TObject);
begin
  if ShiftKeyEditBox.Modified then begin
    UpdateKeyMap(FLastKeyCode, $06, CvtStrToCode(ShiftKeyEditBox.Text));
    ShiftKeyEditBox.Modified := False;
  end;
end;

procedure TKMEditor.CtrlShiftEditBoxExit(Sender: TObject);
begin
  if CtrlShiftEditBox.Modified then begin
    UpdateKeyMap(FLastKeyCode, $04, CvtStrToCode(CtrlShiftEditBox.Text));
    CtrlShiftEditBox.Modified := False;
  end;
end;

procedure TKMEditor.CtrlAltEditBoxExit(Sender: TObject);
begin
  if CtrlAltEditBox.Modified then begin
    UpdateKeyMap(FLastKeyCode, $08, CvtStrToCode(CtrlAltEditBox.Text));
    CtrlAltEditBox.Modified := False;
  end;
end;

procedure TKMEditor.CtrlAltShiftEditBoxExit(Sender: TObject);
begin
  if CtrlAltShiftEditBox.Modified then begin
    UpdateKeyMap(FLastKeyCode, $00, CvtStrToCode(CtrlAltShiftEditBox.Text));
    CtrlAltShiftEditBox.Modified := False;
  end;
end;

procedure TKMEditor.LoadFileButtonClick(Sender: TObject);
var
  FileDialog : TOpenDialog;
  MsgResult : Word;
begin
  if FFileModified and (FLastKeyCode <> 0) then begin
    MsgResult := MessageDlg('The current Key Map has been modified.  Save?',
                            mtWarning, mbYesNoCancel, 0);
    case MsgResult of
      mrYes : SaveFileButtonClick(Self);
      mrCancel : Exit;
    end;
  end;
  FileDialog := TOpenDialog.Create(Application);
  with FileDialog do begin
    Filter := 'Keyboard DBase Files (*.INI)|*.INI|All Files (*.*)|*.*';
    Options := [ofPathMustExist, ofShowHelp];
    Title := 'Select File';
    try
      if Execute then
        SetFileName(FileName);
    finally
      Free;
    end;
  end;
end;

procedure TKMEditor.SaveFileButtonClick(Sender: TObject);
begin
  if (FKeyEmuData <> nil) and FFileModified then
    if kbUpdKeyEmuRecord(PKeyEmulator(FKeyEmuData)) = ecOK then
      FFileModified := False
end;

procedure TKMEditor.NewTypeButtonClick(Sender: TObject);
var
  NewMapName : AnsiString;
  XFerRec : TKeyMapXFerRec;
begin
  if (FKeyEmuData <> nil) then begin
    NewMapName := '';
    if InputQuery('New Key Map Name', 'Name', NewMapName) then begin
      FillChar(XFerRec, SizeOf(XFerRec), 0);
      StrPCopy(XFerRec.Name, NewMapName);
      if (kbAddKeyEmuRecord(PKeyEmulator(FKeyEmuData), XFerRec) = ecOK) then
        EmuTypesTab.Tabs.Add(NewMapName);
    end;
  end;
end;

end.


