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
{*                    ADTERM.PAS 4.06                    *}
{*********************************************************}
{* Deprecated terminal (TApdTerminal)                    *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$I+,G+,X+,F+}
{$C MOVEABLE,DEMANDLOAD,DISCARDABLE}

unit AdTerm;
  {-Delphi terminal window component}

interface

uses
  WinTypes,
  WinProcs,
  SysUtils,
  Classes,
  Messages,
  Controls,
  StdCtrls,
  Forms,
  Graphics,
  Dialogs,
  OoMisc,
  AwUser,
  AwEmu,
  AwKeyEmu,
  AwTerm,
  AdExcept,
  AdPort,
  AdProtcl;

const
  {ANSI color constants}
  emBlack       = 0;
  emRed         = 1;
  emGreen       = 2;
  emYellow      = 3;
  emBlue        = 4;
  emMagenta     = 5;
  emCyan        = 6;
  emWhite       = 7;
  emBlackBold   = 8;
  emRedBold     = 9;
  emGreenBold   = 10;
  emYellowBold  = 11;
  emBlueBold    = 12;
  emMagentaBold = 13;
  emCyanBold    = 14;
  emWhiteBold   = 15;

const
  {Emulator commands}
  eNone            = 0;       {No command, ignore this char}
  eChar            = 1;       {No command, process the char}
  eGotoXY          = 2; {X}   {Absolute goto cursor position call}
  eUp              = 3; {X}   {Cursor up}
  eDown            = 4; {X}   {Cursor down}
  eRight           = 5; {X}   {Cursor right}
  eLeft            = 6; {X}   {Cursor left}
  eClearBelow      = 7; {R}   {Clear screen below cursor}
  eClearAbove      = 8; {R}   {Clear screen above cursor}
  eClearScreen     = 9; {R}   {Clear entire screen}
  eClearEndofLine  = 10;{R}   {Clear from cursor to end of line}
  eClearStartOfLine= 11;{R}   {Clear from cursor to the start of line}
  eClearLine       = 12;{R}   {Clear entire line that cursor is on}
  eSetMode         = 13;{X}   {Set video mode}
  eSetBackground   = 14;      {Set background attribute}
  eSetForeground   = 15;      {Set foreground attribute}
  eSetAttribute    = 16;{X}   {Set video attribute (foreground and background)}
  eSaveCursorPos   = 17;      {Save cursor position}
  eRestoreCursorPos= 18;      {Restore cursor position}
  eDeviceStatusReport = 19;{X}{Report device status or cursor position}
  eString          = 20;      {Pascal style string}
  eHT              = 21;      {Horizontal Tab Character}
  eError           = 255;     {indicates a parser error}

type
  {For setting auto-scrollbar behavior}
  TAutoScroll = (asNone, asHorizontal, asVertical, asBoth);
  TIntegralSize = (isNone, isWidth, isHeight, isBoth);

  {Emulator types}
  TEmulatorType = (etNone, etANSI, etVT52, etVT100, etANSIBBS);

  {Capture mode}
  TCaptureMode = (cmOff, cmOn, cmAppend);

  {Status event handler}
  TTerminalStatusEvent = procedure(CP : TObject;
                                   Row, Col : Byte;
                                   BufRow, BufCol : Word) of object;

  TTerminalErrorEvent = procedure(CP : TObject;
                                  ErrorCode: Word) of object;

  TTerminalCursorPosReportEvent = procedure(CP : TObject;
                                            XPos, YPos: Integer) of object;

  TTermAttrLine = array[0..MaxCols-1] of Byte;

const
  {Default emulator property values}
  {$IFNDEF AProBCB}
  adtDefEmuData     = nil;
  adtDefComPort     = nil;
  adtDefEmulator    = nil;
  adtDefTermSave    = nil;
  adtDefControlStyles = [csClickEvents, csSetCaption, csFramed, csDoubleClicks]; 
  {$ENDIF}
  adtDefEmulatorType = etAnsi;

  adtDefWidth       = 200;
  adtDefHeight      = 200;
  adtDefTermTabStop = True; {Was "DefTabStop"}                   
  adtDefParentColor = False;
  adtDefParentFont  = False;
  adtDefStyle       = 0;
  adtDefScrollBars  = ssNone;
  adtDefAutoScroll  = asNone;
  adtDefIntegralSize = isBoth;
  adtDefScrollback  = False;
  adtDefActive      = True;
  adtDefColor       = clBlack;
  adtDefFontColor   = clSilver;
  adtDefRows        = 200;
  adtDefColumns     = 80;
  adtDefPageHeight  = 25;
  adtDefDisplayRows = 15;
  adtDefDisplayColumns = 40;
  adtDefCapture     = cmOff;
  adtDefCaptureFile = 'APD.CAP';
  adtDefFontName    = 'Terminal';
  adtDefWantTabs    = True;
  adtDefBPlusTriggers = False;
  adtDefBlinkTime     = 550;
  adtDefPersistentMark = True;
  adtDefHalfDuplex     = False;

const
  DefKeyEmuEnabled         = False;
  DefKeyEmuProcessAll      = False;
  DefKeyEmuProcessExtended = False;
  DefKeyEmuFilename        = 'AWKEYBRD.INI';

type
  {record type used to describe a command and its parameters}
  TEmuCommand = record
    Ch      : Char;                 {Character}
    Cmd     : Byte;                 {Command}
    X, Y    : Byte;                 {Coordinates}
    FColor  : Byte;                 {Foreground color}
    BColor  : Byte;                 {Background color}
    ExtAttr : Byte;                 {Extended attributes}
    EmuType : Byte;                 {Emulator Type}
    {BCB cannot handle a small string in a variant record}
    case Byte of
      1 : (Other  : array[1..MaxOther] of Byte);  {Other data}
    {$IFDEF AProBCB}
      2 : (OtherStrLen : Byte;
           OtherStr  : array[1..10] of Char);  {Other data}
    {$ELSE}
      2 : (OtherStr : String[10]);
    {$ENDIF}
  end;

  {Emulator callback function type}
  TProcessCharProc = procedure(PAnsiEmulator: Pointer; C: Char;
                               var Command: TEmuCommand);

  TKeyEmuCommand = record
    KeyCode       : Word;
    ShiftMap      : Word;
    ToggleMap     : Word;
    Extended      : Bool;
    KeyMapped     : Bool;
    KeyboardState : TKeyBoardState;
    Value         : string[KeyMappingLen];
  end;

  TProcessKeyProc = procedure(PKeyEmulator: Pointer; Key: Word;
                              var Command: TKeyEmuCommand);

  TApdCustomKeyboardEmulator = class(TApdBaseComponent)
  protected {private}
    FKeyEmuInUse           : Boolean;
    FKeyEmuData            : Pointer;
    FKeyEmuProc            : TProcessKeyProc;
    FEnabled               : Boolean;
    FKeyEmuType            : String;
    FKeyEmuFileName        : String;
    FKeyEmuProcessAllKeys  : Boolean;
    FKeyEmuProcessExtended : Boolean;
    FKeyEmuTypeList        : TStringList;
  protected
    procedure SetEnabled(const Value: Boolean);
    procedure SetFileName(const Value: String);
    procedure SetKeyEmuData(const NewKeyEmu : Pointer);
    procedure SetKeyEmuProc(const NewKeyProc : TProcessKeyProc);
    procedure SetKeyEmuType(const Value: String);
    procedure SetProcessAll(const Value: Boolean);
    procedure SetProcessExt(const Value: Boolean);
    function GetProcessExt: Boolean;
    function GetProcessAll: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property KeyEmuData : Pointer
      read FKeyEmuData write SetKeyEmuData;
    property KeyEmuProc : TProcessKeyProc
      read FKeyEmuProc write SetKeyEmuProc;
    property KeyEmuInUse : Boolean
      read FKeyEmuInUse write FKeyEmuInUse;

    property Enabled : Boolean
      read FEnabled write SetEnabled;
    property MapFileName : String
      read FKeyEmuFileName write SetFileName;
    property EmulatorType : String
      read FKeyEmuType write SetKeyEmuType;
    property ProcessAllKeys : Boolean
      read GetProcessAll write SetProcessAll;
    property ProcessExtended : Boolean
      read GetProcessExt write SetProcessExt;
    property KeyEmuTypeList : TStringList
      read FKeyEmuTypeList;
  end;

  TApdKeyboardEmulator = class(TApdCustomKeyboardEmulator)
  published
    property Enabled;
    property MapFileName;
    property EmulatorType;
    property ProcessAllKeys;
    property ProcessExtended;
  end;

  {Emulator process char event}
  TProcessCharEvent = procedure(CP : TObject;
                                C : Char;
                                var Command : TEmuCommand) of object;

  {The base emulator component}
  TApdCustomEmulator = class(TApdBaseComponent)
  protected {private}
    {.Z+}
    FEmuInUse      : Boolean;
    FEmuData       : Pointer;
    FEmuProc       : TProcessCharProc;
    FEmulatorType  : TEmulatorType;
    FOnProcessChar : TProcessCharEvent;

  protected
    procedure SetEmuData(NewEmu : Pointer);
    procedure SetEmuProc(NewProc : TProcessCharProc);
    procedure SetEmulatorType(const NewEmulatorType : TEmulatorType);
    procedure Loaded; override;
    procedure ProcessChar(C : Char; var Command : TEmuCommand); virtual;

  public
    constructor Create(AOwner : TComponent); override;
      {-Create a TApdEmulator component}
    destructor Destroy; override;
    {.Z-}

    property EmulatorType : TEmulatorType
      read FEmulatorType write SetEmulatorType default adtDefEmulatorType;
    property OnProcessChar : TProcessCharEvent
      read FOnProcessChar write FOnProcessChar;
    property EmuData : Pointer
      read FEmuData write SetEmuData;
    property EmuProc : TProcessCharProc
      read FEmuProc write SetEmuProc;
    property EmuInUse : Boolean
      read FEmuInUse write FEmuInUse;
  end;

  {The emulator component}
  TApdEmulator = class(TApdCustomEmulator)
  published
    property EmulatorType;
    property OnProcessChar;
  end;

  {The base terminal window component}
  TApdCustomTerminal = class(TApdBaseWinControl)
  protected {private}
    {.Z+}
    {Misc}
    TermSave      : Pointer;
    Created       : Boolean;
    ActivePending : Boolean;
    Force         : Boolean;
    WasFullWidth  : Boolean;
    PixelWidth    : Word;
    InRecreate    : Boolean;
    TermBuff      : TTermBuff;

    {Property fields}
    FComPort      : TApdCustomComPort;
    FStyle        : LongInt;
    FScrollBars   : TScrollStyle;
    FAutoScroll   : TAutoScroll;
    FIntegralSize : TIntegralSize;
    FScrollBack   : Boolean;
    FActive       : Boolean;
    FEmulator     : TApdCustomEmulator;
    FRows         : Word;
    FColumns      : Word;
    FPageHeight   : Word;
    FDisplayRows  : Word;
    FDisplayColumns : Word;
    FCaptureFile  : ShortString;
    FCapture      : TCaptureMode;
    FKeyBoardEmu  : TApdKeyboardEmulator;
    FOnTerminalStatus  : TTerminalStatusEvent;
    FOnTerminalError   : TTerminalErrorEvent;
    FOnCursorPosReport : TTerminalCursorPosReportEvent;
    FBlinkTime    : Word;
    FPersistentMark    : Boolean;
    FHalfDuplex   : Boolean;

    {Colors}
    ColorMap      : TAnsiColorMap;

    {Private procedures}
    procedure StuffDesignData;
    procedure SetComPort(const NewComPort : TApdCustomComPort);
    procedure SetEmulator(const NewEmulator : TApdCustomEmulator);
    procedure SetKeyboardEmu(const NewKeyEmulator : TApdKeyboardEmulator);
    procedure SetName(const Value: TComponentName); override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure ResetTermBuffer;

    {Font procedures}
    procedure PassFont;
    procedure CMFontChanged(var Message : TMessage); message CM_FONTCHANGED;

    {Color procedures}
    function ColorIndex(const RGB : TColor) : Integer;
    function CurrentColor(const Back : Boolean) : Integer;
    procedure GetColorMap;
    procedure SetColorMap;
    procedure PassColors;
    procedure CMColorChanged(var Message : TMessage); message CM_COLORCHANGED;

    {Open/close messages}
    procedure PortOpen(var Message : TMessage); message APW_PORTOPEN;
    procedure PortClose(var Message : TMessage); message APW_PORTCLOSE;

    {Terminal activation}
    function StartTerminalPrim(UseExcept : Boolean) : Integer;
    procedure StartTerminal; dynamic;
    function StopTerminalPrim(UseExcept : Boolean) : Integer;
    procedure StopTerminal; dynamic;

  protected
    {Property methods}
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure SetScrollBars(const NewScroll : TScrollStyle);
    procedure SetAutoScroll(const NewScroll : TAutoScroll);
    procedure SetIntegralSize(const NewInt : TIntegralSize);
    procedure SetScrollback(const NewScroll : Boolean);
    procedure SetActive(const NewActive : Boolean);
    procedure SetRows(const NewRows : Word);
    procedure SetColumns(const NewColumns : Word);
    procedure SetPageHeight(const NewPageHeight : Word);
    function GetDisplayRows : Word;
    procedure SetDisplayRows(const NewRows : Word);
    function GetDisplayColumns : Word;
    procedure SetDisplayColumns(const NewColumns : Word);
    function GetWantTabs : Boolean;
    procedure SetWantTabs(const NewTabs : Boolean);
    function GetHeight : Integer;
    procedure SetHeight(const NewHeight : Integer);
    function GetWidth : Integer;
    procedure SetWidth(const NewWidth : Integer);
    function getCharWidth : Byte; {change to lower case g for BCB}
    function getCharHeight : Byte; {change to lower case g for BCB}
    procedure SetCapture(const NewCapture : TCaptureMode);
    procedure SetCaptureFile(const NewFile : ShortString);
    procedure SetBlinkTime(const NewTime : Word);
    procedure SetPersistentMark(const NewMark : Boolean);
    procedure SetHalfDuplex(const NewDuplex : Boolean);
    procedure SetTermLine(Index: Word; NewLine: String);
    function  GetTermLine(Index: Word): String;
    {AttrLines property removed for BCB}
    {$IFNDEF AProBCB}
    procedure SetTermAttrLine(Index: Word; NewLine: TTermAttrLine);
    function  GetTermAttrLine(Index: Word): TTermAttrLine;
    {$ENDIF}
    {Event methods}
    procedure TerminalStatus(Row, Col : Byte; BufRow, BufCol : Word); virtual;
    procedure TerminalError(ErrorCode : Word); virtual;
    procedure CursorPosReport(XPos, YPos : Integer); virtual;

    {Message methods}
    procedure apwTermStatus(var Message : TMessage); message APW_TERMSTATUS;
    procedure apwTermError(var Message : TMessage);
      message APW_TERMERROR;
    procedure apwCursorPosReport(var Message : TMessage);
      message APW_CURSORPOSREPORT;

    {Other protected methods}
    procedure PassBuffer;
    procedure GetTermState;
    procedure SetTermState;
    procedure RecreateWnd;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent : TComponent;
                           Operation: TOperation); override;
    procedure Loaded; override;

   public
    {Creation/destruction}
    constructor Create(AOwner : TComponent); override;
      {-Create a TApdTerminal component}
    {.Z-}

    {Other public properties}
    procedure ClearWindow;
      {-Clear the visible window by performing a page-down operation}
    procedure ClearBuffer;
      {-Clear the entire buffer}
    procedure StuffChar(const C : Char);
      {-Add the character C to the current location in the terminal window}
    procedure StuffString(const S : String);
      {-Add the string S to the current location in the terminal window}
    procedure ForcePaint;
      {-Update the terminal display, required after StuffChar or StuffString}
    procedure CopyToClipboard;
      {-Copy the marked block to the clipboard}
    procedure SetColors(const FC, BC : Byte);
      {-Set the foreground and background colors for new data}
    function  ClientLine(Value: Word): Word;
      {-Return the actual buffer line # from the client line #}
    {Added for BCB since the AttrLines property was removed.}
    procedure SetTermAttrLineEx(Index: Word; NewLine: TTermAttrLine);
    procedure GetTermAttrLineEx(Index: Word; var Line : TTermAttrLine);

    property CharWidth : Byte
      read getCharWidth; {Changed to lower case 'g' for BCB}
    property CharHeight : Byte
      read getCharHeight; {Changed to lower case 'g' for BCB}  
    property Lines[Index: Word]: String
      read GetTermLine write SetTermLine;
    {AttrLines property removed for BCB}
    {$IFNDEF AProBCB}
    property AttrLines[Index: Word]: TTermAttrLine
      read GetTermAttrLine write SetTermAttrLine;
    {$ENDIF}

  public {published}
    property ComPort : TApdCustomComPort
      read FComPort write SetComPort;
    property Active : Boolean
      read FActive write SetActive default adtDefActive;
    property ScrollBars : TScrollStyle
      read FScrollBars write SetScrollBars default adtDefScrollBars;
    property AutoScroll : TAutoScroll
      read FAutoScroll write SetAutoScroll default adtDefAutoScroll;
    property IntegralSize : TIntegralSize
      read FIntegralSize write SetIntegralSize default adtDefIntegralSize;
    property Scrollback : Boolean
      read FScrollback write SetScrollback default adtDefScrollback;
    property Emulator : TApdCustomEmulator
      read FEmulator write SetEmulator;

    property Rows : Word
      read FRows write SetRows default adtDefRows;
    property Columns : Word
      read FColumns write SetColumns default adtDefColumns;
    property PageHeight : Word
      read FPageHeight write SetPageHeight default adtDefPageHeight;
    property DisplayRows : Word
      read GetDisplayRows write SetDisplayRows default adtDefDisplayRows;
    property DisplayColumns : Word
      read GetDisplayColumns write SetDisplayColumns default adtDefDisplayColumns;
    property CaptureFile : ShortString
      read FCaptureFile write SetCaptureFile;
    property Capture : TCaptureMode
      read FCapture write SetCapture default adtDefCapture;
    property WantTabs : Boolean
      read GetWantTabs write SetWantTabs default adtDefWantTabs;
    property BlinkTime : Word
      read FBlinkTime write SetBlinkTime;
    property PersistentMark : Boolean
      read FPersistentMark write SetPersistentMark;
    property HalfDuplex : Boolean
      read FHalfDuplex write SetHalfDuplex;
    property KeyBoardEmu : TApdKeyboardEmulator
      read FKeyBoardEmu write SetKeyboardEmu;

    {.Z+}
    {Override these so we can get to SetWidth/SetHeight}
    property Height : Integer
      read GetHeight write SetHeight;
    property Width : Integer
      read GetWidth write SetWidth;
    {.Z-}

    {Events}
    property OnTerminalStatus : TTerminalStatusEvent
      read FOnTerminalStatus write FOnTerminalStatus;
    property OnTerminalError : TTerminalErrorEvent
      read FOnTerminalError write FOnTerminalError;
    property OnCursorPosReport : TTerminalCursorPosReportEvent
      read FOnCursorPosReport write FOnCursorposReport;
  end;

  {The terminal window component}
  TApdTerminal = class(TApdCustomTerminal)
  published
    {Publish the terminal window properties}
    property ComPort;
    property Active;
    property ScrollBars;
    property AutoScroll;
    property IntegralSize;
    property Scrollback;
    property Emulator;
    property Rows;
    property Columns;
    property PageHeight;
    property DisplayRows;
    property DisplayColumns;
    property CaptureFile;
    property Capture;
    property WantTabs;
    property Height;
    property Width;
    property BlinkTime;
    property PersistentMark;
    property HalfDuplex;
    property KeyBoardEmu;
    property OnTerminalStatus;
    property OnTerminalError;
    property OnCursorPosReport;

    {Published inherited properties}
    property Align;
    property Color default adtDefColor;
    property Ctl3D;
    property Cursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property Visible;

    {Published inherited events}
    property OnClick;
    property OnDblClick;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  {B+ Terminal}
(*  TApdCustomBPTerminal = class(TApdCustomTerminal)
  protected {private}
    {.Z+}
    TimerIndex    : Cardinal;         {B+ timer trigger handle}
    EnqTrig       : Cardinal;         {ENQ trigger handle}
    DLETrig       : Cardinal;         {DLE trigger handle}
    EscITrig      : Cardinal;         {<ESC>I trigger handle}
    Started       : Boolean;          {True if B+ started}
    ProcessingDLE : Boolean;          {True when processing a DLE sequence}
    Protocol      : TApdProtocol;     {B+ protocol component}
    FBPlusTriggers: Boolean;          {Sets triggers on/off}
    FOnBPlusStart : TNotifyEvent;     {Event handler for B+ starts}

  protected
    procedure SetBPlusTriggers(const OnOff : Boolean);
    procedure CreateWnd; override;
    procedure apwTriggerTimer(var Msg : TMessage); message APW_TRIGGERTIMER;
    procedure apwTriggerLength(var Msg : TMessage); message APW_TRIGGERAVAIL;
    procedure apwTriggerData(var Msg : TMessage); message APW_TRIGGERDATA;
    procedure apwBPlusStart(var Msg : TMessage); message APW_TERMBPLUSSTART;
    function FoundDLE : Boolean;
    procedure Notification(AComponent : TComponent;
                           Operation: TOperation); override;

  public
    constructor Create(AOwner : TComponent); override;
      {-Create a TApdBPTerminal}
    {.Z-}

    property OnBPlusStart : TNotifyEvent
      read FOnBPlusStart write FOnBPlusStart;
    property BPlusTriggers : Boolean
      read FBPlusTriggers write SetBPlusTriggers default adtDefBPlusTriggers;
  end;

  {B+ Terminal}
  TApdBPTerminal = class(TApdCustomBPTerminal)
  published
    property ComPort;
    property Active;
    property ScrollBars;
    property AutoScroll;
    property IntegralSize;
    property Scrollback;
    property Emulator;
    property Rows;
    property Columns;
    property PageHeight;
    property DisplayRows;
    property DisplayColumns;
    property CaptureFile;
    property Capture;
    property Align;
    property Color default adtDefColor;
    property Ctl3D;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property Visible;
    property WantTabs;
    property BlinkTime;
    property PersistentMark;
    property HalfDuplex;
    property KeyBoardEmu;
    property OnClick;
    property OnDblClick;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnBPlusStart;
    property OnTerminalError;
    property OnTerminalStatus;
    property OnCursorPosReport;
  end;*)

implementation


{Private types}
type
  TTermFontData = record
    Height : Byte;
    Width  : Byte;
    JIC    : Word;
  end;

{Global emulator hook}

  procedure EmulatorHook(P : PAnsiEmulator; C : Char;
                         var Command : TEmuCommand); far;
    {-Process this character}
  begin
    with TApdCustomEmulator(P) do begin
      if Assigned(FOnProcessChar) then
        ProcessChar(C, Command)
      else if @FEmuProc <> nil then
        TProcessCharProcLo(FEmuProc)(EmuData, C, TEmuCommandLo(Command))
      else begin
        Command.Ch := C;
        Command.Cmd := eChar;
      end;
    end;
  end;

  procedure KeyboardHook(P : PKeyEmulator; Key : Word;
                         var Command : TKeyEmuCommand); far;
    {-Process this key}
  begin
    with TApdCustomKeyboardEmulator(P) do begin
      if @FKeyEmuProc <> nil then
        TProcessKeyProcLo(FKeyEmuProc)(KeyEmuData, Key, TKeyEmuCommandLo(Command))
      else begin
        FillChar(Command, SizeOf(Command), #0);
        Command.KeyCode := Key;
        Command.Extended := False;
      end;
    end;
  end;

{-ApdCustomKeyboard-}

constructor TApdCustomKeyboardEmulator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeyEmuInUse           := False;
  FKeyEmuData            := nil;
  FKeyEmuProc            := nil;
  FEnabled               := DefKeyEmuEnabled;
  FKeyEmuType            := '';
  FKeyEmuFilename        := DefKeyEmuFileName;
  FKeyEmuProcessAllKeys  := DefKeyEmuProcessAll;
  FKeyEmuProcessExtended := DefKeyEmuProcessExtended;
  FKeyEmuTypeList        := TStringList.Create;
end;

destructor TApdCustomKeyboardEmulator.Destroy;
begin
  Enabled := False;
  FKeyEmuTypeList.Free;
  inherited Destroy;
end;

procedure TApdCustomKeyboardEmulator.SetEnabled(const Value: Boolean);
var
  TempFileName  : array[0..255] of char;
  TempIndex     : PChar;
begin
  if Value <> FEnabled then begin
    if Value then begin
      if CheckException(Self, kbInitKeyEmulator(PKeyEmulator(FKeyEmuData),
                              StrPCopy(TempFileName, FKeyEmuFileName))) = ecOK then begin
        @FKeyEmuProc := @kbProcessKey;
        FEnabled := Value;
        FKeyEmuTypeList.Clear;
        if kbNumEmuTypes(PKeyEmulator(FKeyEmuData)) > 1 then begin
          if CheckException(Self,
                kbLoadKeyEmuIndex(PKeyEmulator(FKeyEmuData))) = ecOK then begin
            TempIndex := PKeyEmulator(FKeyEmuData)^.kbKeyNameList;
            while (TempIndex^ <> #0) do begin
              FKeyEmuTypeList.Add(StrPas(TempIndex));
              Inc(TempIndex, StrLen(TempIndex) + 1);
            end;
            EmulatorType := FKeyEmuTypeList[0];
          end;
        end;
      end;
    end else begin
      if FKeyEmuData <> nil then
        kbDoneKeyEmulator(PKeyEmulator(FKeyEmuData));
      FKeyEmuData := nil;
      FKeyEmuProc := nil;
      FEnabled := Value;
      FKeyEmuTypeList.Clear;
      EmulatorType := '';
    end;
  end;
end;

procedure TApdCustomKeyboardEmulator.SetKeyEmuType(const Value: String);
var
  Temp : TKeyMapName;
begin
  if Value <> FKeyEmuType then begin
    FKeyEmuType := Value;
    if FKeyEmuData <> nil then
      if not (csDesigning in ComponentState) then begin
        kbSetKeyEmuType(PKeyEmulator(FKeyEmuData), StrPCopy(Temp, Value));
        CheckException(Self, kbLoadKeyEmuMap(PKeyEmulator(FKeyEmuData)));
      end;
  end;
end;

procedure TApdCustomKeyboardEmulator.SetFileName(const Value: String);
begin
  if FKeyEmuFileName <> Value then begin
    FKeyEmuFileName := Value;
    if FEnabled then begin
      Enabled := False;
      Enabled := True;
    end;
  end;
end;

procedure TApdCustomKeyboardEmulator.SetKeyEmuData(const NewKeyEmu : Pointer);
  {-Set new Emu data}
begin
  if NewKeyEmu <> FKeyEmuData then begin
    FKeyEmuData := NewKeyEmu;
  end;
end;

procedure TApdCustomKeyboardEmulator.SetKeyEmuProc(const NewKeyProc : TProcessKeyProc);
  {-Set new EmuProc}
begin
  if @NewKeyProc <> @FKeyEmuProc then begin
    FKeyEmuProc := NewKeyProc;
  end;
end;

procedure TApdCustomKeyboardEmulator.SetProcessAll(const Value: Boolean);
begin
  if Value <> FKeyEmuProcessAllKeys then begin
    FKeyEmuProcessAllKeys := Value;
    if FKeyEmuData <> nil then
      PKeyEmulator(FKeyEmuData)^.kbProcessAll := Bool(Value);
  end;
end;

function TApdCustomKeyBoardEmulator.GetProcessAll: Boolean;
begin
  Result := FKeyEmuProcessAllKeys;
end;

procedure TApdCustomKeyboardEmulator.SetProcessExt(const Value: Boolean);
begin
  if Value <> FKeyEmuProcessExtended then begin
    FKeyEmuProcessExtended := Value;
    if FKeyEmuData <> nil then
      PKeyEmulator(FKeyEmuData)^.kbProcessExt := Bool(Value);
  end;
end;

function TApdCustomKeyboardEmulator.GetProcessExt: Boolean;
begin
  Result := FKeyEmuProcessExtended;
end;

{TApdEmulator}

  constructor TApdCustomEmulator.Create(AOwner : TComponent);
    {-Create and initialize the emulator component}
  begin
    inherited Create(AOwner);

    {Inits}
    {$IFDEF AproBCB}
    FEmuData := nil;
    {$ELSE}
    FEmuData := adtDefEmuData;
    {$ENDIF}
    FEmuProc := nil;
    FEmulatorType := adtDefEmulatorType;
    FEmuInUse := False;
  end;

  destructor TApdCustomEmulator.Destroy;
  begin
    EmulatorType := etNone;
    inherited Destroy;
  end;

  procedure TApdCustomEmulator.SetEmulatorType(
                            const NewEmulatorType : TEmulatorType);
    {-Set a new emulator type}
  begin
    if NewEmulatorType <> FEmulatorType then begin
      {Get rid of old emulator}
      case FEmulatorType of
        etNone :
          ;
        etANSI..etANSIBBS :
          if EmuData <> nil then begin
            aeDoneAnsiEmulator(PAnsiEmulator(FEmuData));
            FEmuData := nil;
            FEmuProc := nil;
          end;
      end;

      {Create new emulator}
      FEmulatorType := NewEmulatorType;
      case FEmulatorType of
        etNone :
          EmuData := nil;
        etANSI..etANSIBBS :
          begin
            aeInitAnsiEmulator(PAnsiEmulator(FEmuData));
            PAnsiEmulator(FEmuData)^.emuType := Ord(NewEmulatorType);
            @FEmuProc := @aeProcessAnsiChar;
          end;
      end;
    end;
  end;

  procedure TApdCustomEmulator.SetEmuData(NewEmu : Pointer);
    {-Set new Emu data}
  begin
    if NewEmu <> FEmuData then begin
      FEmuData := NewEmu;
      EmulatorType := etNone;
    end;
  end;

  procedure TApdCustomEmulator.SetEmuProc(NewProc : TProcessCharProc);
    {-Set new EmuProc}
  begin
    if @NewProc <> @FEmuProc then begin
      FEmuProc := NewProc;
      EmulatorType := etNone;
    end;
  end;

  procedure TApdCustomEmulator.Loaded;
    {-Force setting of ANSI emulation}
  begin
    inherited Loaded;

    if FEmulatorType = etAnsi then begin
      FEmulatorType := etNone;
      EmulatorType := etAnsi;
    end;
  end;

  procedure TApdCustomEmulator.ProcessChar(C : Char; var Command : TEmuCommand);
    {-Call event handler}
  begin
    FOnProcessChar(Self, C, Command)
  end;

{TApdTerminal}

  constructor TApdCustomTerminal.Create(AOwner : TComponent);
    {-Create and initialize the terminal component}
  begin
    inherited Create(AOwner);

    {Inits}
    Force := False;
    Created := False;
    WasFullWidth := False;
    Width := adtDefWidth;
    Height := adtDefHeight;
    {$IFDEF AProBCB}                                              
    ControlStyle := [csClickEvents, csSetCaption, csFramed, csDoubleClicks];
    {$ELSE}
    ControlStyle := adtDefControlStyles;
    {$ENDIF}
    TabStop := adtDefTermTabStop;
    ParentColor := adtDefParentColor;
    ParentFont := adtDefParentFont;
    ActivePending := False;
    InRecreate := False;
    FStyle := adtDefStyle;
    FActive := adtDefActive;
    Color := adtDefColor;
    FScrollBars := adtDefScrollBars;
    FScrollback := adtDefScrollback;
    FIntegralSize := adtDefIntegralSize;
    FRows := adtDefRows;
    FColumns := adtDefColumns;
    FPageHeight := adtDefPageHeight;
    FDisplayRows := adtDefDisplayRows;
    FDisplayColumns := adtDefDisplayColumns;
    FCapture := adtDefCapture;
    FCaptureFile := adtDefCaptureFile;
    FKeyboardEmu := nil;
    {$IFDEF AProBCB}
    FComPort := nil;
    FEmulator := nil;
    TermSave := nil;
    {$ELSE}
    FComPort := adtDefComPort;
    FEmulator := adtDefEmulator;
    TermSave := adtDefTermSave;
    {$ENDIF}
    Font.Color := adtDefFontColor;
    Font.Name := adtDefFontName;
    Font.Size := 9;
    Font.Pitch := fpFixed;
    Font.Style := [];
    FBlinkTime := adtDefBlinkTime;
    FPersistentMark := adtDefPersistentMark;
    FHalfDuplex := adtDefHalfDuplex;
  end;

  procedure TApdCustomTerminal.CreateWnd;
    {-Search for an existing TComPort and assign if found}
  var
    OurForm    : TForm;
    OrigHeight : Word;
    OrigWidth  : Word;

    function FindComPort : TApdCustomComPort;
      {-Search for an existing TComPort}
    var
      I : Integer;
    begin
      Result := nil;
      for I := 0 to OurForm.ComponentCount-1 do begin
        if OurForm.Components[I] is TApdCustomComPort then begin
          Result := TApdCustomComPort(OurForm.Components[I]);
          Break;
        end;
      end;
    end;

    function FindEmulator : TApdCustomEmulator;
      {-Search for an existing TApdEmulator}
    var
      I : Integer;
    begin
      Result := nil;
      for I := 0 to OurForm.ComponentCount-1 do begin
        if OurForm.Components[I] is TApdCustomEmulator then begin
          Result := TApdCustomEmulator(OurForm.Components[I]);
          if Result.EmuInUse then
            Result := nil
          else
            Break;
        end;
      end;
    end;


    function FindKeyboard : TApdKeyboardEmulator;
      {-Search for an existing TApdEmulator}
    var
      I : Integer;
    begin
      Result := nil;
      for I := 0 to OurForm.ComponentCount-1 do begin
        if OurForm.Components[I] is TApdKeyboardEmulator then begin
          Result := TApdKeyboardEmulator(OurForm.Components[I]);
          if Result.KeyEmuInUse then
            Result := nil
          else
            Break;
        end;
      end;
    end;


  begin
    {Save height/width before creating window because the creation}
    {process will change them to the AWTERM defaults.             }
    OrigHeight := Height;
    OrigWidth  := Width;

    {Create the window element}
    inherited CreateWnd;
    Created := True;

    {Find the parent form}
    OurForm := TForm(GetParentForm(Self));

    {Search for comport and assign if found}
    if not Assigned(FComPort) then
      ComPort := FindComPort;

    {Search for emulator and assign if found}
    if not Assigned(FEmulator) then
      Emulator := FindEmulator;


    {Search for keyboard and assign if found}
    if not Assigned(FKeyboardEmu) then
      KeyBoardEmu := FindKeyboard;

    {Set the terminal window's font and color}
    GetColorMap;
    PassColors;
    PassFont;

    {Force the initial size}
    Force := True;
    SetHeight(OrigHeight);
    SetWidth(OrigWidth);
    SetDisplayRows(FDisplayRows);
    SetDisplayColumns(FDisplayColumns);
    Force := False;

    {Set options}
    SetWantTabs(adtDefWantTabs);

    if csDesigning in ComponentState then
      {Stuff with design data}
      StuffDesignData
    else
      {Clear buffer to initial colors}
      ClearBuffer;
  end;

  procedure TApdCustomTerminal.CreateParams(var Params : TCreateParams);
    {-Setup our window creation parameters}
  const
    ScrollBits : array[TScrollStyle] of DWORD =
                 (0, ws_HScroll, ws_VScroll, ws_HScroll + ws_VScroll);
    AutoBits : array[TAutoScroll] of DWORD =
               (0, tws_AutoHScroll, tws_AutoVScroll,
               tws_AutoHScroll + tws_AutoVScroll);
    IntegralBits : array[TIntegralSize] of DWORD =                    
                   (0, tws_IntWidth, tws_IntHeight,
                   tws_IntWidth + tws_IntHeight);
  begin
    inherited CreateParams(Params);

    if csDesigning in ComponentState then begin
      RegisterTerminalWindowClass(True);
      CreateSubClass(Params, TerminalClassNameDesign);
    end else begin
      RegisterTerminalWindowClass(False);
      CreateSubClass(Params, TerminalClassName);
    end;
    Params.Style := Params.Style or
                    AutoBits[FAutoScroll] or
                    ScrollBits[FScrollBars] or
                    IntegralBits[FIntegralSize];
  end;

  procedure TApdCustomTerminal.StuffDesignData;
    {-Stuff terminal with design data, assumes we're in design mode}
  var
    I : Word;
  begin
    ClearBuffer;
    for I := 1 to DisplayRows do begin
      StuffString(Name + ' - line ' + IntToStr(I));
      if I <> DisplayRows then
        StuffString(^M^J);
    end;
    Repaint;
  end;

  procedure TApdCustomTerminal.SetComPort(const NewComPort : TApdCustomComPort);
    {-Set a new comport, set or release handle}
  var
    OK : Boolean;
  begin
    if NewComPort <> FComPort then begin
      {Reregister from old comport}
      if Assigned(FComPort) then
        ComPort.DeregisterUser(Handle);

      FComPort := NewComPort;

      {Okay to try to activate?}
      if Assigned(FComPort) then begin
        ComPort.RegisterUser(Handle);
        OK := ComPort.Open or
              (ComPort.AutoOpen and not (csDesigning in ComponentState))
      end else
        OK := False;

      if OK then begin
        if Active or ActivePending then begin
          {Stop the current terminal, in case it is active...}
          StopTerminalPrim(False);
          {...and start the new one}
          StartTerminal;
          ActivePending := False;
        end else begin
          StopTerminalPrim(False);
          ActivePending := True;
        end;
      end else
        CheckException(Self, SendMessage(Handle, APW_TERMRELCOM, 0, 0));
    end;
  end;

  procedure TApdCustomTerminal.ClearWindow;
    {-Clear the window}
  begin
    CheckException(Self, SendMessage(Handle, APW_TERMCLEAR, 0, 0));
  end;

  procedure TApdCustomTerminal.ClearBuffer;
    {-Clear the entire buffer}
  begin
    CheckException(Self, SendMessage(Handle, APW_TERMCLEAR, 1, 0));
  end;

  procedure TApdCustomTerminal.StuffChar(const C : Char);
    {-Add C to the current location in the terminal window}
  begin
    CheckException(Self, SendMessage(Handle, APW_TERMSTUFF, 1, Longint(@C)));
  end;

  procedure TApdCustomTerminal.StuffString(const S : String);
    {-Add S to the current location in the terminal window}
  begin
    CheckException(Self, SendMessage(Handle, APW_TERMSTUFF,
                                     Length(S), Longint(@S[1])));
  end;

  procedure TApdCustomTerminal.ForcePaint;
    {-Force a screen update}
  begin
    CheckException(Self, SendMessage(Handle, APW_TERMPAINT, 0, 0));
  end;

  procedure TApdCustomTerminal.CopyToClipboard;
    {-Copy marked block to clipboard}
  begin
    CheckException(Self, SendMessage(Handle, WM_COPY, 0, 0));
  end;

  procedure TApdCustomTerminal.SetColors(const FC, BC : Byte);
    {-Set colors}
  begin
    CheckException(Self,
      SendMessage(Handle, APW_TERMCOLORS, FC, BC));
  end;

  function TApdCustomTerminal.StartTerminalPrim(UseExcept : Boolean) : Integer;
    {-Start terminal}
  begin
    {Try to start the terminal}
    if Assigned(FComPort) then begin
      if FComPort.Dispatcher <> nil then begin
        Result := SendMessage(Handle, APW_TERMSETCOM, FComPort.Dispatcher.Handle, 0);
        if Result = ecOk then
          Result := SendMessage(Handle, APW_TERMSTART, 1, 0);
      end else
        Result := ecBadHandle;
    end else
      Result := ecPortNotAssigned;

    {Return value or generate exception}
    if UseExcept then
      StartTerminalPrim := CheckException(Self, Result)
    else
      StartTerminalPrim := Result;
  end;

  procedure TApdCustomTerminal.StartTerminal;
    {-Start terminal, assume ok to do so}
  begin
    StartTerminalPrim(True);
  end;

  function TApdCustomTerminal.StopTerminalPrim(UseExcept : Boolean) : Integer;
    {-Stop the terminal}
  begin
    if Assigned(FComPort) then
      if FComPort.Dispatcher <> nil then
        Result := SendMessage(Handle, APW_TERMSTOP, 0, 0)
      else
        Result := ecBadHandle
    else
      Result := ecPortNotAssigned;

    if UseExcept then
      StopTerminalPrim := CheckException(Self, Result)
    else
      StopTerminalPrim := Result;
  end;

  procedure TApdCustomTerminal.StopTerminal;
    {-Stop the terminal, assume ok to do so}
  begin
    CheckException(Self, SendMessage(Handle, APW_TERMSTOP, 0, 0));
  end;

  procedure TApdCustomTerminal.SetEmulator(
                               const NewEmulator : TApdCustomEmulator);
    {-Attach the emulator to the terminal}
  begin
    if FEmulator <> NewEmulator then begin
      if FEmulator <> nil then
        FEmulator.EmuInUse := False;
      FEmulator := NewEmulator;
      if (FEmulator <> nil) and (not FEmulator.EmuInUse) then begin
        SendMessage(Handle, APW_TERMSETEMUPROC, 0, LongInt(@EmulatorHook));
        SendMessage(Handle, APW_TERMSETEMUPTR, 0, LongInt(FEmulator));
        FEmulator.EmuInUse := True;
      end else begin
        SendMessage(Handle, APW_TERMSETEMUPROC, 0, 0);
        SendMessage(Handle, APW_TERMSETEMUPTR, 0, 0);
        FEmulator := nil;
      end;
    end;
  end;

  procedure TApdCustomTerminal.SetKeyboardEmu(
                               const NewKeyEmulator : TApdKeyboardEmulator);
    {-Attach the emulator to the terminal}
  begin
    if FKeyBoardEmu <> NewKeyEmulator then begin
      if FKeyboardEmu <> nil then
        FKeyboardEmu.KeyEmuInUse := False;
      FKeyboardEmu := NewKeyEmulator;
      if (FKeyboardEmu <> nil) and (not FKeyboardEmu.KeyEmuInUse) then begin
        SendMessage(Handle, APW_TERMSETKEYEMUPROC, 0, LongInt(@KeyboardHook));
        SendMessage(Handle, APW_TERMSETKEYEMUPTR, 0, LongInt(FKeyboardEmu));
        FKeyboardEmu.KeyEmuInUse := True;
      end else begin
        SendMessage(Handle, APW_TERMSETKEYEMUPROC, 0, 0);
        SendMessage(Handle, APW_TERMSETKEYEMUPTR, 0, 0);
        FKeyboardEmu := nil;
      end;
    end;
  end;

  procedure TApdCustomTerminal.SetName(const Value: TComponentName);
    {-If in design mode, stuff new data}
  var
    OldName : TComponentName;
  begin
    OldName := Name;
    inherited SetName(Value);

    if (csDesigning in ComponentState) and (OldName <> '') then
      StuffDesignData;
  end;

  procedure TApdCustomTerminal.WMSize(var Message: TWMSize);
    {-Update design data on resize}
  begin
    inherited;
    if (DisplayRows < 1) then
      DisplayRows := 1;
    if csDesigning in ComponentState then
      StuffDesignData;
  end;

  procedure TApdCustomTerminal.ResetTermBuffer;
  begin
    SendMessage(Handle, APW_TERMGETBUFFPTR, 0, LongInt(@TermBuff));
  end;

  procedure TApdCustomTerminal.PassFont;
    {-Tell the terminal window about the font change}
  var
    FontHandle : HFont;
  begin
    FontHandle := SendMessage(Handle, WM_SETFONT, Font.Handle, Longint(True));
    if FontHandle = 0 then begin
      {Terminal window rejected the font, switch back to default font}
      Font.Name := adtDefFontName;
      Font.Pitch := fpFixed;
      Font.Style := [];
      Font.Size  := 9;
    end;
  end;

  procedure TApdCustomTerminal.CMFontChanged(var Message : TMessage);
    {-Tell the terminal window about the font change}
  begin
    inherited;

    if Created then begin
      {Pass the font change along to the terminal window}
      PassFont;

      {See if we need a terminal size change}
      if csLoading in ComponentState then begin
        SetDisplayRows(FDisplayRows);
        SetDisplayColumns(FDisplayColumns);
      end;

      {Update the colors in case the font color changed}
      PassColors;

      {If designing, update the terminal window contents to show new color}
      if csDesigning in ComponentState then
        StuffDesignData;
    end;
  end;

  procedure TApdCustomTerminal.GetColorMap;
    {-Return the color map}
  begin
    SendMessage(Handle, APW_TERMCOLORMAP, gscGetMap, LongInt(@ColorMap));
  end;

  procedure TApdCustomTerminal.SetColorMap;
    {-Set a new color map}
  begin
    SendMessage(Handle, APW_TERMCOLORMAP, gscSetMap, LongInt(@ColorMap));
  end;

  procedure TApdCustomTerminal.PassColors;
    {-Pass colors to terminal window}
  var
    FC, BC : Integer;
    RGB : LongInt;
  begin
    {Move background color from terminal component to terminal window}
    RGB := ColorToRGB(Color);
    BC := ColorIndex(RGB);
    if BC = -1 then begin
      {Color not in map, stuff it in current background color slot}
      BC := CurrentColor(True);
      ColorMap[BC] := RGB;
      SetColorMap;
    end;

    {Move foreground color from font to terminal window}
    RGB := ColorToRGB(Font.Color);
    FC := ColorIndex(RGB);
    if FC = -1 then begin
      {Color not in map, stuff it in the current foreground color slot}
      FC := CurrentColor(False);
      ColorMap[FC] := RGB;
      SetColorMap;
    end;

    {Set new foreground/background colors (also sets highlight colors)}
    SetColors(FC, BC);
    ResetTermBuffer;
  end;

  procedure TApdCustomTerminal.CMColorChanged(var Message : TMessage);
    {-Tell terminal window that color changed}
  begin
    if Created then begin
      {Pass new colors to the terminal window}
      PassColors;

      {If designing, update the terminal window contents to show new color}
      if csDesigning in ComponentState then
        StuffDesignData;
    end;
    inherited;
  end;

  procedure TApdCustomTerminal.PortOpen(var Message : TMessage);
    {-Port was just opened, inform window procedure}
  begin
    CheckException(Self,
      SendMessage(Handle, APW_TERMSETCOM, ComPort.Dispatcher.handle, 0));
    if ActivePending then
      SetActive(True);
  end;

  procedure TApdCustomTerminal.PortClose(var Message : TMessage);
    {-Port was just closed, inform the window procedure}
  begin
    CheckException(Self, SendMessage(Handle, APW_TERMRELCOM, 0, 0));
    ActivePending := Active;
    Active := False;
  end;

  function TApdCustomTerminal.ColorIndex(const RGB : TColor) : Integer;
    {-Return an ANSI color index for RGB, -1 if not found}
  var
    I : Word;
  begin
    for I := emBlack to emWhiteBold do begin
      if LongInt(ColorMap[I]) = RGB then begin                       
        Result := I;
        Exit;
      end;
    end;
    Result := -1;
  end;

  function TApdCustomTerminal.CurrentColor(const Back : Boolean) : Integer;
    {-Return current foreground or background color index}
  var
    Colors : Longint;
  begin
    Colors := SendMessage(Handle, APW_TERMCOLORMAP, gscGetColors, 0);
    if Back then
      Result := LH(Colors).H
    else
      Result := LH(Colors).L
  end;

  procedure TApdCustomTerminal.SetIntegralSize(const NewInt : TIntegralSize);
    {-Adjust the window style long}
  begin
    if FIntegralSize <> NewInt then begin
      FIntegralSize := NewInt;
      RecreateWnd;
    end;
  end;

  procedure TApdCustomTerminal.SetScrollback(const NewScroll : Boolean);
    {-Set the scrollback mode}
  begin
    if FScrollback <> NewScroll then begin
      FScrollback := NewScroll;
      if FScrollback then begin
        WasFullWidth := DisplayColumns = Columns;
        PixelWidth := Width;
      end else if WasFullWidth then begin
        Force := True;
        if (IntegralSize = isNone) or (IntegralSize = isHeight) then
          Width := PixelWidth
        else
          DisplayColumns := FColumns;
        Force := False;
      end;
      SendMessage(Handle, APW_TERMTOGGLESCROLL, 0, 0);
    end;
  end;

  procedure TApdCustomTerminal.GetTermState;
    {-Note the current terminal state}
  begin
    TermSave := Pointer(SendMessage(Handle, APW_TERMSAVE, 0, 0));
  end;

  procedure TApdCustomTerminal.SetTermState;
    {-Restore the current terminal state}
  begin
    if TermSave <> nil then
      SendMessage(Handle, APW_TERMSAVE, 1, LongInt(TermSave));
  end;

  procedure TApdCustomTerminal.RecreateWnd;
    {-Recreate the window element}
  var
    WasActive : Boolean;
  begin
    if not (csLoading in ComponentState) then begin
      WasActive := Active;
      if Active then
        StopTerminalPrim(False);
      GetTermState;
      InRecreate := True;
      inherited RecreateWnd;
      InRecreate := False;
      SetTermState;
      if WasActive then
        StartTerminalPrim(False);
    end;
  end;

  procedure TApdCustomTerminal.WndProc(var Message: TMessage);
  begin
    case Message.Msg of
      WM_ERASEBKGND :
        ;
      WM_DESTROY :
        begin
          {Deregister ourselves from the port}
          if Assigned(FComPort) then
            ComPort.DeregisterUser(Handle);

          {Close capture file it one is open}
          if (FCapture = cmOn) or (FCapture = cmAppend) then
            SetCapture(cmOff);

          {The emulator is now free for another terminal window}
          if Assigned(FEmulator) then
            FEmulator.EmuInUse := False;

          if Assigned(FKeyboardEmu) then
            FKeyboardEmu.KeyEmuInUse := False;

        end;
      WM_MOUSEACTIVATE :
        if not (csDesigning in ComponentState) then
          inherited WndProc(Message);
      else
        inherited WndProc(Message);
    end;
  end;

  procedure TApdCustomTerminal.SetScrollBars(const NewScroll : TScrollStyle);
    {-Set the new scroll behavior}
  const
    ScrollBits : array[TScrollStyle] of LongInt =
                 (0, ws_HScroll, ws_VScroll, ws_HScroll or ws_VScroll);
  begin
    if NewScroll <> FScrollBars then begin
      {Update scrollbar bits}
      FScrollBars := NewScroll;

      {Deselect autoscroll for the specified scrollbars}
      if (NewScroll = ssBoth) or (NewScroll = ssHorizontal) then begin
        case FAutoScroll of
          asBoth       : FAutoScroll := asVertical;
          asHorizontal : FAutoScroll := asNone;
        end;
      end;
      if (NewScroll = ssBoth) or (NewScroll = ssVertical) then begin
        case FAutoScroll of
          asBoth     : FAutoScroll := asHorizontal;
          asVertical : FAutoScroll := asNone;
        end;
      end;

      {Recreate window element to show new scrollbar options}
      RecreateWnd;
    end;
  end;

  procedure TApdCustomTerminal.SetAutoScroll(const NewScroll : TAutoScroll);
    {-Set the new autoscroll behavior}
  begin
    if NewScroll <> FAutoScroll then begin
      FAutoScroll := NewScroll;

      {Recreate window element to show new scrollbar options}
      RecreateWnd;
    end;
  end;

  procedure TApdCustomTerminal.SetActive(const NewActive : Boolean);
    {-Start or stop terminal events}
  var
    Res : Integer;
  begin
    if (FActive <> NewActive) or ActivePending then begin
      if not (csDesigning in ComponentState) then begin
        if Assigned(FComPort) then begin
          {All okay, activate/deactivate the terminal window}
          if NewActive then begin
            Res := StartTerminalPrim(False);
            if Res = ecOk then
              ActivePending := False
            else begin
              ActivePending := True;
              FComPort.ForcePortOpen;
              Exit;
            end;
            FActive := True;
          end else begin
            {Deactivating terminal window}
            StopTerminalPrim(False);
            FActive := False;
          end;
        end else begin
          if NewActive then
            {Can't set active because port isn't assigned}
            CheckException(Self, ecPortNotAssigned)
          else
            {Not assigned but ok to set to False}
            FActive := False;
        end;
      end else
        {Designing, just set the property}
        FActive := NewActive;
    end;
  end;

  procedure TApdCustomTerminal.SetRows(const NewRows : Word);
    {-Set a new number of rows}
  begin
    if FRows <> NewRows then begin
      if NewRows < PageHeight then
        FRows := PageHeight
      else
        FRows := NewRows;
      if Longint(Rows) * Columns > 65535 then
        FColumns := 65535 div Rows;
      PassBuffer;
    end;
  end;

  procedure TApdCustomTerminal.SetColumns(const NewColumns : Word);
    {-Set a new number of columns}
  begin
    if FColumns <> NewColumns then begin
      if NewColumns < FDisplayColumns then                        
        FColumns := FDisplayColumns
      else FColumns := NewColumns;
      if Longint(Rows) * Columns > 65535 then
        FRows := 65535 div Columns;
      PassBuffer;
    end;
  end;

  procedure TApdCustomTerminal.SetPageHeight(const NewPageHeight : Word);
    {-Set a new page height}
  begin
    if FPageHeight <> NewPageHeight then begin
      if NewPageHeight > Rows then
        FPageHeight := Rows
      else                                                         
        FPageHeight := NewPageHeight;
      PassBuffer;
    end;
  end;

  function TApdCustomTerminal.GetDisplayRows : Word;
    {-Return number of display rows}
  begin
    FDisplayRows := ClientHeight div CharHeight;
    Result := FDisplayRows;
  end;

  procedure TApdCustomTerminal.SetDisplayRows(const NewRows : Word);
    {-Set a new display height; always updates, even if no change}
  begin
    if (IntegralSize = isBoth) or (IntegralSize = isHeight) then begin
      if NewRows > PageHeight then
        FDisplayRows := PageHeight
      else
        FDisplayRows := NewRows;
      SendMessage(Handle, APW_TERMFORCESIZE, FDisplayColumns, FDisplayRows);
      ResetTermBuffer;
    end;
  end;

  function TApdCustomTerminal.GetDisplayColumns : Word;
    {-Return the number of display columns}
  begin
    FDisplayColumns := ClientWidth div CharWidth;
    Result := FDisplayColumns;
  end;

  procedure TApdCustomTerminal.SetDisplayColumns(const NewColumns : Word);
    {-Set a new display width; always updates, even if no change}
  begin
    if (IntegralSize = isBoth) or (IntegralSize = isWidth) then begin
      if NewColumns > Columns then
        FDisplayColumns := Columns
      else
        FDisplayColumns := NewColumns;
      SendMessage(Handle, APW_TERMFORCESIZE, FDisplayColumns, FDisplayRows);
    end;
  end;

  function TApdCustomTerminal.GetWantTabs : Boolean;
    {-Return WantTabs option}
  begin
    Result := (GetWindowLong(Handle, gwl_Style) and tws_WantTab) <> 0;
  end;

  procedure TApdCustomTerminal.SetWantTabs(const NewTabs : Boolean);
    {-Set new WantTabs option}
  var
    Style : Longint;
  begin
    Style := GetWindowLong(Handle, gwl_Style);
    if NewTabs then
      SetWindowLong(Handle, gwl_Style, Style or tws_WantTab)
    else
      SetWindowLong(Handle, gwl_Style, Style and not tws_WantTab);
  end;

  function TApdCustomTerminal.GetHeight : Integer;
    {-Get the height in pixels}
  begin
    Result := inherited Height;
  end;

  procedure TApdCustomTerminal.SetHeight(const NewHeight : Integer);
    {-Set height in pixels if sbPixels}
  begin
    if (IntegralSize = isNone) or
       (IntegralSize = isWidth) or
       (csLoading in ComponentState) then
      {It's valid to change Height...}
      if (NewHeight <> Height) or Force then
        {...and it really needs to be changed}
        inherited Height := NewHeight;
  end;

  function TApdCustomTerminal.GetWidth : Integer;
    {-Get the width in pixels}
  begin
    Result := inherited Width;
  end;

  procedure TApdCustomTerminal.SetWidth(const NewWidth : Integer);
    {-Set width in pixels if sbPixels}
  begin
    if (IntegralSize = isNone) or
       (IntegralSize = isHeight) or
       (csLoading in ComponentState) then
      {It's valid to change width...}
      if (NewWidth <> Width) or Force then
        {...and it really needs to be changed}
        inherited Width := NewWidth;
  end;

  procedure TApdCustomTerminal.SetBlinkTime(const NewTime : Word);
    {-Set the time interval for blinking characters}
  begin
    if FBlinkTime <> NewTime then begin
      FBlinkTime := NewTime;
      SendMessage(Handle, apw_TermBlinkTimeChange, NewTime, 0);
    end;
  end;

  procedure TApdCustomTerminal.SetPersistentMark(const NewMark: Boolean);
    {-Set the the terminal marking to be either persistent no not}
  begin
    if FPersistentMark <> NewMark then begin
      FPersistentMark := NewMark;
      SendMessage(Handle, apw_TermPersistentMarkChange, Ord(NewMark), 0);
    end;
  end;

  procedure TApdCustomTerminal.SetHalfDuplex(const NewDuplex: Boolean);
    {-Set the the terminal half/full duplex mode}
  begin
    if FHalfDuplex <> NewDuplex then begin
      FHalfDuplex := NewDuplex;
      SendMessage(Handle, apw_TermSetHalfDuplex, Ord(NewDuplex), 0);
    end;
  end;

  function TApdCustomTerminal.ClientLine(Value: Word): Word;
  begin
    if Value = 0 then
      Value := 1;
    if Value > FDisplayRows then
      raise EBadArgument.Create(ecBadArgument, False)
    else
      Result := SendMessage(Handle, APW_TERMGETCLIENTLINE, 0, 0) + Value;
  end;

  procedure TApdCustomTerminal.SetTermLine(Index: Word; NewLine: String);
  var
    TempLine : array[0..MaxCols-1] of char;
    TempAttr : TTermAttrLine;
    TempXAttr: TTermAttrLine;
    LineNum  : Word;
    TempColor: Byte;
  begin
    if Index = 0 then
      Index := 1;
    if (Longint(Index) * Columns > 65535) or
       (Index > Rows) or
       (Index < 1) then                                          
      raise EBadArgument.Create(ecBadArgument, False)
    else if TermBuff.Data <> nil then begin
      LineNum := Columns*(Index-1);
      while Length(NewLine) < Columns do
        NewLine := NewLine + ' ';
      StrPCopy(TempLine, NewLine);

      {-Set the color to the first color in the line}
      {Changed from property to GetTermAttrLineEx() for BCB.}
      {$IFNDEF AProBCB}
      TempAttr := AttrLines[Index];
      {$ELSE}
      GetTermAttrLineEx(Index, TempAttr);
      {$ENDIF}
      TempColor := TempAttr[0];
      if TempColor = 0 then
        TempColor := 7;       {white on black}
      FillChar(TempAttr, SizeOf(TempAttr), TempColor);

      {Turn off all extended attributes}
      FillChar(TempXAttr, SizeOf(TempXAttr), 0);

      {replace the old data with the new}
      Move(TempLine, TermBuff.Data^[LineNum], Columns);
      Move(TempAttr, TermBuff.Attr^[LineNum], Columns);
      Move(TempXAttr, TermBuff.XAttr^[LineNum], Columns);

      {-Redisplay the terminal}
      InvalidateRect(Handle, nil, False);
      UpdateWindow(Handle);
    end;
  end;

  function TApdCustomTerminal.GetTermLine(Index: Word): String;
  var
    TempLine : array[0..MaxCols] of char;
  begin
    if (Longint(Index) * Columns > 65535) or
       (Index > Rows) or
       (Index < 1) then                                          
      raise EBadArgument.Create(ecBadArgument, False)
    else if TermBuff.Data <> nil then begin
      FillChar(TempLine, SizeOf(TempLine), #0);
      Move(TermBuff.Data^[Columns*(Index-1)], TempLine, Columns);
      Result := StrPas(TempLine);
    end;
  end;

  {$IFNDEF AProBCB}
  procedure TApdCustomTerminal.SetTermAttrLine(Index: Word; NewLine: TTermAttrLine);
  var
    TempXAttr: TTermAttrLine;
    LineNum  : Word;
  begin
    if Index = 0 then
      Index := 1;
    if (Longint(Index) * Columns > 65535) or
       (Index > Rows) or
       (Index < 1) then                                          
      raise EBadArgument.Create(ecBadArgument, False)
    else if TermBuff.Attr <> nil then begin
      LineNum := Columns*(Index-1);

      {Turn off all extended attributes}
      FillChar(TempXAttr, SizeOf(TempXAttr), 0);

      {replace the old data with the new}
      Move(NewLine, TermBuff.Attr^[LineNum], Columns);
      Move(TempXAttr, TermBuff.XAttr^[LineNum], Columns);

      {-Redisplay the terminal}
      InvalidateRect(Handle, nil, False);
      UpdateWindow(Handle);
    end;
  end;

  function TApdCustomTerminal.GetTermAttrLine(Index: Word): TTermAttrLine;
  var
    TempLine : TTermAttrLine;
  begin
    if (Longint(Index) * Columns > 65535) or
       (Index > Rows) or
       (Index < 1) then                                           
      raise EBadArgument.Create(ecBadArgument, False)
    else if TermBuff.Attr <> nil then begin
      Move(TermBuff.Attr^[Columns*(Index-1)], TempLine, Columns);
      Result := TempLine;
    end;
  end;
  {$ENDIF}

  procedure TApdCustomTerminal.SetTermAttrLineEx(Index: Word; NewLine: TTermAttrLine);
  var
    TempXAttr: TTermAttrLine;
    LineNum  : Word;
  begin
    if Index = 0 then
      Index := 1;
    if (Longint(Index) * Columns > 65535) or
       (Index > Rows) or
       (Index < 1) then                                            
      raise EBadArgument.Create(ecBadArgument, False)
    else if TermBuff.Attr <> nil then begin
      LineNum := Columns*(Index-1);

      {Turn off all extended attributes}
      FillChar(TempXAttr, SizeOf(TempXAttr), 0);

      {replace the old data with the new}
      Move(NewLine, TermBuff.Attr^[LineNum], Columns);
      Move(TempXAttr, TermBuff.XAttr^[LineNum], Columns);

      {-Redisplay the terminal}
      InvalidateRect(Handle, nil, False);
      UpdateWindow(Handle);
    end;
  end;

  procedure TApdCustomTerminal.GetTermAttrLineEx(Index: Word; var Line : TTermAttrLine);
  begin
    if (Longint(Index) * Columns > 65535) or
       (Index > Rows) or
       (Index < 1) then                                         
      raise EBadArgument.Create(ecBadArgument, False)
    else if TermBuff.Attr <> nil then begin
      Move(TermBuff.Attr^[Columns*(Index-1)], Line, Columns);
    end;
  end;

  function TApdCustomTerminal.getCharWidth : Byte;
    {-Return the current character width, in pixels}
  var
    FD : TTermFontData;
  begin
    Longint(FD) := SendMessage(Handle, APW_TERMFONTSIZE, 0, 0);
    Result := FD.Width;
  end;

  function TApdCustomTerminal.getCharHeight : Byte;
    {-Return the current character height, in pixels}
  var
    FD : TTermFontData;
  begin
    Longint(FD) := SendMessage(Handle, APW_TERMFONTSIZE, 0, 0);
    Result := FD.Height;
  end;

  procedure TApdCustomTerminal.SetCapture(const NewCapture : TCaptureMode);
    {-Turn capturing on/off}
  var
    CapAppend : Boolean;
    Enable    : Boolean;
    P : array[0..255] of Char;
  begin
    if FCapture <> NewCapture then begin
      FCapture := NewCapture;
      if not (csDesigning in ComponentState) and not InRecreate then begin
        StrPCopy(P, FCaptureFile);
        Enable := (NewCapture = cmOn) or (NewCapture = cmAppend);
        CapAppend := NewCapture = cmAppend;
        CheckException(Self, SendMessage(Handle, APW_TERMCAPTURE,
                             (Ord(CapAppend) shl 8) or Ord(Enable),
                             LongInt(@P)));
      end;
    end;
  end;

  procedure TApdCustomTerminal.SetCaptureFile(const NewFile : ShortString);
    {-Set new capture file name}
  var
    OldCapture : TCaptureMode;
  begin
    if CompareText(FCaptureFile, NewFile) <> 0 then begin
      if (FCapture = cmOn) or (FCapture = cmAppend) then begin
        OldCapture := FCapture;
        SetCapture(cmOff);
        FCaptureFile := NewFile;
        SetCapture(OldCapture);
      end else
        FCaptureFile := NewFile;
    end;
  end;

  procedure TApdCustomTerminal.TerminalStatus(Row, Col : Byte;
                                           BufRow, BufCol : Word);
    {-Call the user's event handler}
  begin
    if Assigned(FOnTerminalStatus) then
      FOnTerminalStatus(Self, Row, Col, BufRow, BufCol);
  end;

  procedure TApdCustomTerminal.apwTermStatus(var Message : TMessage);
    {-Receives apw_TermStatus message from terminal}
  begin
    with Message do
      {$IFDEF WIN32}
      TerminalStatus(Lo(wParamLo), Hi(wParamLo), lParamLo, lParamHi); 
      {$ELSE}
      TerminalStatus(wParamLo, wParamHi, lParamLo, lParamHi);
      {$ENDIF}
  end;

  procedure TApdCustomTerminal.TerminalError(ErrorCode: Word);
    {-Call the user's event handler}
  begin
    if Assigned(FOnTerminalError) then
      FOnTerminalError(Self, ErrorCode);
  end;

  procedure TApdCustomTerminal.apwTermError(var Message : TMessage);
    {-Receives apw_TermError message from terminal}
  begin
    with Message do begin
      TerminalError(wParamLo);
      if lParam = 0 then
        Capture := cmOff;
    end;                                                             
  end;

  procedure TApdCustomTerminal.CursorPosReport(XPos, YPos : Integer);
    {-Call the user's event handler}
  begin
    if Assigned(FOnCursorPosReport) then
      FOnCursorPosReport(Self, XPos, YPos);
  end;

  procedure TApdCustomTerminal.apwCursorPosReport(var Message : TMessage);
    {-Receives apw_CursorPosReport message from terminal}
  begin
    with Message do
      CursorPosReport(wParamLo, lParamLo);
  end;

  procedure TApdCustomTerminal.PassBuffer;
    {-Pass the buffer sizes to the terminal window}
  var
    L : Longint;
  begin
    if Longint(Rows) * Columns > 65535 then
      raise EBufferTooBig.Create(ecBufferTooBig, False);

    {Pass the values to the terminal window}
    LH(L).L := Rows;
    LH(L).H := Columns;
    CheckException(Self, SendMessage(Handle, APW_TERMBUFFER, PageHeight, L));
    ResetTermBuffer;
  end;

  procedure TApdCustomTerminal.Notification(AComponent : TComponent;
                                            Operation : TOperation);
    {-Handles TComPort inserts and removes}
  begin
    inherited Notification(AComponent, Operation);

    if Operation = opRemove then begin
      {Check for owned components going away}
      if AComponent = FComPort then begin
        SetActive(False);
        ComPort := nil;
      end;
      if AComponent = FEmulator then begin
        FEmulator.EmuInUse := False;
        FEmulator := nil;
      end;
      if AComponent = FKeyboardEmu then begin
        FKeyboardEmu.KeyEmuInUse := False;
        FKEyboardEmu := nil;
      end;

    end else if Operation = opInsert then begin
      {Check for owned components being installed}
      if (FComPort = nil) and
         (AComponent is TApdCustomComPort) and
         not Assigned(FComPort) then begin
          ComPort := TApdCustomComPort(AComponent);
      end;
      if (FEmulator = nil) and (AComponent is TApdCustomEmulator) then
        if (csDesigning in ComponentState) and
        (not TApdCustomEmulator(AComponent).EmuInUse) then
          Emulator := TApdCustomEmulator(AComponent);

      if (FKeyboardEmu = nil) and  (AComponent is TApdCustomKeyboardEmulator) then
        if (csDesigning in ComponentState) and
        (not TApdKeyboardEmulator(AComponent).KeyEmuInUse) then
          KeyboardEmu := TApdKeyboardEmulator(AComponent);
    end;
  end;

  procedure TApdCustomTerminal.Loaded;
    {-Handle initialization-after-load issues}
  begin
    inherited Loaded;

    {Try to open terminal if Active or ActivePending}
    if Active then
      ActivePending := True;
    if ActivePending then
      SetActive(True);

    {Clear the component name stuff out of the buffer}
    if not (csDesigning in ComponentState) then
      ClearBuffer;
  end;

(*  constructor TApdCustomBPTerminal.Create(AOwner : TComponent);
    {-Create BPTerminal and initialize B+ fields}
  begin
    inherited Create(AOwner);

    {Init B+ fields}
    Started := False;
    ProcessingDLE := False;
    Protocol := nil;
    FBPlusTriggers := adtDefBPlusTriggers;
  end;

  procedure TApdCustomBPTerminal.SetBPlusTriggers(const OnOff : Boolean);
    {-Set the B+ triggers on or off}
  begin
    if OnOff <> FBPlusTriggers then begin

      if not Assigned(FComPort) then
        Exit;

      FBPlusTriggers := OnOff;
      if OnOff then begin
        EnqTrig := ComPort.AddDataTrigger(cEnq, False);
        DLETrig := ComPort.AddDataTrigger(cDLE, False);
        EscITrig := ComPort.AddDataTrigger(cEsc+'I', False);
      end else begin
        ComPort.RemoveTrigger(EnqTrig);
        ComPort.RemoveTrigger(DLETrig);
        ComPort.RemoveTrigger(EscITrig);
      end;
    end;
  end;

  procedure TApdCustomBPTerminal.CreateWnd;
    {-Search for an existing TProtocol and assign if found}
  var
    OurForm : TForm;

    function FindProtocol : TApdProtocol;
      {-Search for an existing protocol}
    var
      I : Integer;
    begin
      Result := nil;
      for I := 0 to OurForm.ComponentCount-1 do begin
        if OurForm.Components[I] is TApdProtocol then begin
          Result := TApdProtocol(OurForm.Components[I]);
          Break;
        end;
      end;
    end;

  begin
    inherited CreateWnd;

    {Find the parent form}
    OurForm := TForm(GetParentForm(Self));

    {Search for protocol and assign if found}
    if not Assigned(Protocol) then
      Protocol := FindProtocol;
  end;

  procedure TApdCustomBPTerminal.apwTriggerLength(var Msg : TMessage);
    {-Needed for processing DLEs that arrive the terminal has control}
  var
    Ready : Boolean;
    Start : Boolean;
    Upload : Boolean;
    Finished : Boolean;
  begin
    if ProcessingDLE then begin
      {Check pointers}
      if not Assigned(Protocol) or not Assigned(FComPort) then begin
        DefaultHandler(Msg);
        Exit;
      end;

      {Call ProcessDLE for the current DLE and any new DLEs}
      repeat
        {Assume we'll check just once more}
        Finished := True;

        {Process the DLE stream}
        try
          Protocol.ProcessDLE(True, Ready, Start, Upload);
          if Ready then begin
            {Time to start protocol}
            if Start then begin
              {Starting the protocol, remove B+ triggers}
              SetBPlusTriggers(False);
              ProcessingDLE := False;

              {Start the protocol}
              SendMessage(Handle, APW_TERMBPLUSSTART, 0, 0);
              if Upload then
                Protocol.StartTransmit
              else
                Protocol.StartReceive;
            end else begin
              {Not time to start yet, see if we should restore the trigger}
              if FoundDLE then begin
                {Loop back to ProcessDLE}
                Finished := False;
                ProcessingDLE := True;
                Protocol.PrepareProcessDLE(TimerIndex);
              end else begin
                {No DLE, set trigger and exit}
                Finished := True;
                ProcessingDLE := False;
                DLETrig := ComPort.AddDataTrigger(cDLE, False);
              end;
            end;
          end;
        except
          on EAPDException do begin
            ProcessingDLE := False;
            ComPort.FlushInBuffer;
            ComPort.AddDataTrigger(cDLE, False);
            Finished := True;
          end;
        end;
      until Finished;
    end else
      DefaultHandler(Msg);
  end;

  procedure TApdCustomBPTerminal.apwTriggerTimer(var Msg : TMessage);
    {-Needed for processing DLEs}
  var
    Ready : Boolean;
    Start : Boolean;
    Upload : Boolean;
  begin
    if ProcessingDLE and (Cardinal(Msg.wParam) = TimerIndex) then    
      if not Assigned(Protocol) then
        DefaultHandler(Msg)
      else
        Protocol.ProcessDLE(False, Ready, Start, Upload)
    else
      DefaultHandler(Msg);
  end;

  procedure TApdCustomBPTerminal.apwTriggerData(var Msg : TMessage);
    {-Got a B+ special character or string, process it}
  begin
    if Cardinal(Msg.wParam) = EnqTrig then begin                    
      Protocol.ProcessEnq;
      if FoundDLE then begin
        Protocol.PrepareProcessDLE(TimerIndex);
        ProcessingDLE := True;
      end;
    end else if Cardinal(Msg.wParam) = EscITrig then begin          
      Protocol.ProcessESCI(80, 25);
    end else if Cardinal(Msg.wParam) = DLETrig then begin
      {Prepare to handle the DLE on avail trigger}
      ProcessingDLE := True;
      Protocol.PrepareProcessDLE(TimerIndex);
      ComPort.RemoveTrigger(DLETrig);
    end else
      DefaultHandler(Msg);
  end;

  procedure TApdCustomBPTerminal.apwBPlusStart(var Msg : TMessage);
    {-Call the user's event handler}
  begin
    if Assigned(FOnBPlusStart) then
      FOnBPlusStart(Self);
  end;

  function TApdCustomBPTerminal.FoundDLE : Boolean;
    {-Return True if a DLE is waiting in the input buffer}
  var
    I : Word;
    C : Char;
  begin
    FoundDLE := True;

    {Do nothing if no comport}
    if not Assigned(FComPort) then
      Exit;

    if ComPort.CharReady then begin
      I := 1;
      while (I <= ComPort.InBuffUsed) do begin
        C := ComPort.PeekChar(I);
        if C = cDLE then
          Exit;
        Inc(I);
      end;
    end;
    FoundDLE := False;
  end;

  procedure TApdCustomBPTerminal.Notification(AComponent : TComponent;
                                              Operation: TOperation);
    {-Handle Protocol inserts/removes}
  begin
    inherited Notification(AComponent, Operation);

    if Operation = opRemove then begin
      if AComponent = Protocol then
        Protocol := nil;
    end else if Operation = opInsert then begin
      if (Protocol = nil) and (AComponent is TApdProtocol) then
        Protocol := TApdProtocol(AComponent);
    end;
  end;
*)
end.




