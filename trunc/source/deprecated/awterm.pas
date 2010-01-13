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
{*                    AWTERM.PAS 4.06                    *}
{*********************************************************}
{* Deprecated terminal emulator support                  *}
{*********************************************************}

{Global defines potentially affecting this unit}
{$I AWDEFINE.INC}

{Options required for this unit}
{$X+,B+,F-,I-,Q-}

{$IFDEF Win32}
{$J+}
{$ENDIF}

unit AwTerm;
  {-Terminal window}

interface

uses
  WinTypes,
  WinProcs,
  Messages,
  SysUtils,
  OoMisc,
  AwUser,
  AwEmu;

{$DEFINE IncludeTerm}

type
  {Windows message record}
  {$IFDEF Win32}
  wMsg = record
    hWindow : TApdHwnd;
    Message : UINT;
    case Integer of
      0: (wParam : WPARAM; lParam : LPARAM);
      1: (wParamLo, wParamHi : Word; lParamLo, lParamHi : Word);
  end;
  {$ELSE}
  wMsg = record
    hWindow : TApdHwnd;
    Message : Word;
    case Integer of
      0: (wParam : Word; lParam : LongInt);
      1: (wParamLo, wParamHi : Byte; lParamLo, lParamHi : Word);
  end;
  {$ENDIF}

{.$I AWTERM.PA0}   {Interfaced routines}
type
  {For holding/changing the ANSI color map}
  TAnsiColorMap = array[emBlack..emWhiteBold] of TColorRef;

const
  {Maximum number of columns for terminal window}
  MaxCols = 133;

  {Map ANSI color constants into RGB values}
  ColorValues : TAnsiColorMap = (
    $00000000,    {black,       emBlack}
    $00000080,    {red,         emRed}
    $00008000,    {green,       emGreen}
    $00008080,    {yellow,      emYellow}
    $00800000,    {blue,        emBlue}
    $00800080,    {magenta,     emMagenta}
    $00808000,    {cyan,        emCyan}
    $00C0C0C0,    {gray,        emWhite}
    $00808080,    {dark gray,   emBlackBold}
    $000000FF,    {dark red,    emRedBold}
    $0000FF00,    {dark green,  emGreenBold}
    $0000FFFF,    {brown,       emYellowBold}
    $00FF0000,    {dark blue,   emBlueBold}
    $00FF00FF,    {pink,        emMagentaBold}
    $00FFFF00,    {dark mag,    emCyanBold}
    $00FFFFFF);   {white,       emWhiteBold}

  {For getting/setting color maps}
  gscSetMap = 0;
  gscGetMap = 1;
  gscGetColors = 2;

type
  {Screen buffer}
  PScreenBuffer = ^TScreenBuffer;
  TScreenBuffer = array[0..65520] of Char;

  {Screen attribute buffer}
  PAttrBuffer = ^TAttrBuffer;
  TAttrBuffer = array[0..65520] of Byte;

  {Temporary Screen Attribute buffer for entended attributes}
  PAttrBufferB = ^TAttrBufferB;
  TAttrBufferB = array[0..65520] of Byte;

  {Extended screen attribute buffer}
  PExtAttrBuffer = ^TExtAttrBuffer;
  TExtAttrBuffer = array[0..65520] of Byte;

  {Terminal window tab stop settings}
  PHorizontalTabStop = ^THorizontalTabStop;
  THorizontalTabStop = array[1..25] of Byte;

  PVerticalTabStop   = ^TVerticalTabStop;
  TVerticalTabStop   = array[1..12] of Byte;

  {Capture buffer}
  PCharBuffer = ^TCharBuffer;
  TCharBuffer = array[1..65521] of Char;

{$IFDEF IncludeTerm}
type
  {For storing the default proc}
  TWndProc = function (Wnd: TApdHwnd; Msg, wParam: Word; lParam: LongInt): LongInt;

  TBuffer = class
    {Buffer information}
    bScreenBuffer: PScreenBuffer; {Pointer to screen data}
    bAttrBuffer  : PAttrBuffer;   {Pointer to attribute data}
    bAttrBufferB : PAttrBufferB;  {Pointer to attribute data}
    bExtAttrBuffer:PExtAttrBuffer;{Pointer to extended attribute data}
    bHorizTabStop: PHorizontalTabStop;{Pointer to horizontal tab data}
    bVertiTabStop: PVerticalTabStop;{Pointer to vertical tab data}
    bHeight      : Word;          {Buffer height}
    bWidth       : Word;          {Buffer width}
    bCharHeight  : Word;          {Height of one character of current font}
    bCharWidth   : Word;          {Width of one character of current font}
    bScrollback  : Bool;          {True when in scrollback mode}
    bJunk1       : Bool;          {Keep word alignment}
    bPageHeight  : Word;          {Height of page}
    bBufferSize  : Word;          {Total buffer size}
    bBufferLimit : Word;          {Number of characters -1 line}
    bX           : Word;          {Current buffer location of caret}
    bY           : Word;          {Current buffer location of caret}
    bMaxY        : Word;          {Y highwater}
    bXPos        : Integer;       {X coord of upper left of client}
    bYPos        : Integer;       {Y coord of upper left of client}
    bCom         : TApdBaseDispatcher;       {Copy of TTerminal's TComHandle}
    bInMargins   : Bool;          {Caret in margin area}

    {Save/restore client/buffer info when in scrollback}
    bSaveX       : Word;          {Saved X}
    bSaveY       : Word;          {Saved Y}
    bSaveXPos    : Integer;       {Saved XPos}
    bSaveYPos    : Integer;       {Saved YPos}
    bWasFull     : Boolean;       {Saved fullscreen flag}

    {Color information, current and original}
    bfColor      : Byte;          {Current foreground color}
    bbColor      : Byte;          {Current background color}
    bfColorOrg   : Byte;          {Original foreground color}
    bbColorOrg   : Byte;          {Original background color}
    bColors      : TAnsiColorMap; {ANSI color map}
    bExtAttr     : Byte;          {Extended ANSI attribute byte}
    bBlinkReset  : Bool;          {blinking reset state}

    {Client area information (visible portion of Buffer)}
    cHeight      : Word;          {Char height of client area}
    cWidth       : Word;          {Char width of client area}
    cSizeX       : Word;          {Pixel width of client area}
    cSizeY       : Word;          {Pixel height of client area}
    cSaveX       : Word;          {Pixel width before scrollback}
    cSaveY       : Word;          {Pixel height before scrollback}
    cCheckX      : Word;          {width at start of scrollback}
    cCheckY      : Word;          {height at start of scrollback}
    cLastHeight  : Word;          {Previous height, checked during wmSize}
    cLastWidth   : Word;          {Previous width, check during wmSize}
    cMarginTop   : Word;          {Top margin of page/client}
    cMarginBottom: Word;          {Bottom margin of page/client}

    {Emulator}
    bEmulator    : Pointer;       {Emulator pointer}
    bEmuProc     : TProcessCharProcLo; {Callback to emulator}
    bEC          : TEmuCommandLo; {Results of last emulator call} 
    bSaveXLoc    : Word;          {X from last eSaveCursorPosition}
    bSaveYLoc    : Word;          {Y from last eSaveCursorPosition}
    bSaveFlag    : Bool;          {Are we currently in a previos save state}
    bSaveAttr    : Byte;          {attribute form last eSaveCursorPosition}
    bSaveAttrEx  : Byte;          {extended attribute form last eSaveCursorPosition}

    {Painting information}
    bWnd         : hWnd;          {Window handle of owning Terminal}
    bNeedVScroll : Integer;       {Pending vertical scroll value}
    bNeedHScroll : Integer;       {Pending horizontal scroll value}
    bRedrawRect  : TRect;         {Union of all invalid rectangles}
    bFocused     : Bool;          {The current window has focus}

    {Marking information}
    bMarking     : Bool;          {In the process of marking}
    bMarked      : Bool;          {A marked block exists}
    bOffScreen   : Bool;          {True if doing offscreen scrolling}
    bMarkAnchorX : Word;          {Anchor for current block}
    bMarkAnchorY : Word;          {Anchor for current block}
    bMarkStartX  : Word;          {Start column of marked block}
    bMarkStartY  : Word;          {Start row of marked block}
    bMarkEndX    : Word;          {End column of marked block}
    bMarkEndY    : Word;          {End row of marked block}
    bScrollTimer : Word;          {Timer used for off-screen scrolling}
    bMarkColorB  : Byte;          {Highlight background color}
    bMarkColorF  : Byte;          {Highlight foreground color}

    {Capturing}
    bCapIndex    : Word;          {Index into capture buffer}
    bCapBuffer   : PCharBuffer;   {Capture buffer}
    bCaptureName : array[0..255] of Char; {Name of capture file}
    bCaptureFile : File;          {File of captured data}
    bCapture     : Bool;          {True if capturing}

    {Semaphore for save structure}
    bFinal       : Word;          {Notes end of data for copying}

    constructor Create(AWnd : TApdHwnd; Height, Width : Word);
    destructor Destroy; override;
    procedure bDeallocBuffers;
    function bNewBuffer(Rows, Cols, PageHeight : Word) : Integer;
    procedure bSetColors(FC, BC : Word);
    procedure bSetHighlightColors(FC, BC : Word);
    procedure bFlushCapture;
    procedure bAddToCapture(C : Char);
    function bSetCapture(Enable, Append : Bool; FName : PChar) : Integer;
    procedure bSetScrollMode(Scrollback : Bool);
    procedure bInvalidateChar(X, Y : Word);
    procedure bPostStatusMsg;
    procedure bUpdateFont(Height, Width : Word);
    procedure bUpdateBuffer;
    procedure bWriteChar(C : Char);
    procedure bClearScreen;
    procedure bProcessChar(C : Char);
    procedure bMoveCaret;
    function bCurLineLength : Word;
    function bNeedPaint : Bool;
    procedure bForcePaint;
    procedure bUpdateScrollThumb(Vert : Bool);
    procedure bScroll(X, Y : Integer);
    procedure bGotoTop(Vert : Boolean);
    procedure bGotoBottom(Vert : Boolean);
    function bConvertToRow(I : Integer) : Word;
    function bConvertToCol(I : Integer) : Word;
    procedure bFixMarking(NewX, NewY : Word);
    procedure bResetMarking;
    procedure bUpdateMarks(RawX, RawY : Integer);
    procedure bSortTabBuffer(var TabBuffer; Size : Byte);
    function bGetNextTabStop(CurrentPos, Count : Byte;
                             var TabBuffer; Size : Byte): Byte;
    function bGetPrevTabStop(CurrentPos, Count : Byte;
                             var TabBuffer; Size : Byte): Byte;
    procedure bSetHorizontalTabStop(Column : Byte);
    procedure bClearHorizontalTabStop(Column : Byte);
    procedure bSetVerticalTabStop(Row : Byte);
    procedure bClearVerticalTabStop(Row : Byte);
  end;

  TTerminal = class
    tWnd        : hWnd;           {Window handle for terminal window}
    aBuffer     : TBuffer;        {Buffer object, screen contents}
    tFont       : THandle;        {Handle to selected font}
    tCom        : TApdBaseDispatcher; {Handle to com port}
    tCharHeight : Word;           {Height of current font}
    tCharWidth  : Word;           {Width of current font}
    tScrollBack : Bool;           {True when in tScrollBack mode}
    tDataReady  : Bool;           {True if data arrived during tScrollBack}
    tHasVScroll : Bool;           {True if vertical scrollbar present}
    tHasHScroll : Bool;           {True if horizontal scrollbar present}
    tFocused    : Bool;           {True when window has the focus}
    tMinimized  : Bool;           {True when window is minimized}
    tPersistentMarking : Bool;    {True if persistant marking in on}
    tHalfDuplex : Bool;           {True when in full duplex mode}
    tDefWndProc : TFarProc;       {The default window proc}
    tFinal      : Word;           {Notes end of data for copying}
    HadVScroll  : Bool;           {Preserve VScroll state through scroll-back}
    vMin,vMax   : Integer;        {Saved VScroll ranges}
    TriggerHandlerInstalled : Boolean;                                         

    {Keyboard Emulator}
    tKeyEmuPtr   : Pointer;       {Emulator pointer}
    tKeyEmuProc  : TProcessKeyProcLo; {Callback to emulator}
    tKC          : TKeyEmuCommandLo; {Results of last keyboard emulator call}

    constructor Create(AWnd : TApdHwnd);
    destructor Destroy; override;
    procedure tBlink;
    procedure tSetInitialSizes;
    function tGetWindowStyle : LongInt;
    function tWantTab : Bool;
    function tIntHeight : Bool;
    function tIntWidth : Bool;
    function tAutoVScroll : Bool;
    function tAutoHScroll : Bool;
    procedure tMakeCaret;
    procedure tCalcClientRowCol;
    procedure tAdjustIntSize;
    procedure tAdjustMaxSize;
    procedure tAdjustClient;
    function tFullWidth : Bool;
    function tFullHeight : Bool;
    procedure tCheckVScroll;
    procedure tCheckHScroll;
    procedure tResizeClientRect;
    procedure tGetMousePos(var X, Y : Integer);
    procedure tStartTerminal(var Msg : wMsg);
    procedure tStopTerminal(var Msg : wMsg);
    procedure tSetEmulatorPtr(var Msg : wMsg);
    procedure tSetEmulatorProc(var Msg : wMsg);
    procedure tSetKeyEmulatorPtr(var Msg : wMsg);
    procedure tSetKeyEmulatorProc(var Msg : wMsg);
    procedure tSetComHandle(var Msg : wMsg);
    procedure tReleaseComHandle(var Msg : wMsg);
    procedure tHandleData(var Msg : wMsg);
    procedure tClearWindow(var Msg : wMsg);
    procedure tClearBuffer(var Msg : wMsg);
    procedure tToggleScrollback(var Msg : wMsg);
    procedure tStuffData(var Msg : wMsg);
    procedure tForcePaint(var Msg : wMsg);
    procedure tSetWndProc(wParam : Word; lParam : Longint);
    function tSaveRestore(var Msg : wMsg) : Longint;
    function tGetSetColorMap(var Msg : wMsg) : LongInt;
    procedure tForceSize(var Msg : wMsg);
    function tGetFontSize(var Msg : wMsg) : Longint;
    procedure tBlinkTimeChange(var Msg : wMsg);
    procedure tPersistentMarkChange(var Msg : wMsg);
    procedure tSetHalfDuplex(var Msg: wMsg);
    procedure tGetBuffPtr(var Msg: wMsg);
    function tGetFirstClientLine(var Msg: wMsg): Longint;
    procedure tTextOutColor(DC : HDC; X, Y : Word;
                            Line : PChar; Attr : PChar;
                            Len : Word; MarkStart : Word; MarkEnd : Word);
    procedure tPaint(PaintDC: HDC; var PaintInfo: TPaintStruct);
    procedure wmPaint(var Msg : wMsg);
    procedure wmKeydown(var Msg : wMsg);
    procedure wmMouseActivate(var Msg : wMsg);
    procedure wmSize(var Msg : wMsg);
    procedure wmGetMinMaxInfo(var Msg : wMsg);
    function wmSetFont(Font : THandle) : LongInt;
    procedure wmChar(var Msg : wMsg);
    procedure wmSetFocus(var Msg : wMsg);
    procedure wmKillFocus(var Msg : wMsg);
    function wmGetDlgCode(var Msg : wMsg) : LongInt;
    procedure wmVScroll(var Msg : wMsg);
    procedure wmHScroll(var Msg : wMsg);
    procedure wmEraseBkGnd(var Msg : wMsg);

    procedure wmLButtonDown(var Msg : wMsg);
    procedure wmLButtonUp(var Msg : wMsg);
    procedure wmMouseMove(var Msg : wMsg);
    procedure wmTimer(var Msg : wMsg);
    procedure wmCopy(var Msg : wMsg);
  end;

  {For saving/restoring terminal data}
  PTermSave = ^TTermSave;
  TTermSave = record
    ScreenBuffer : Pointer;
    AttrBuffer   : Pointer;
    RestBuffer   : Pointer;
    TermBuffer   : Pointer;
    AttrBufferB  : Pointer;
    ExtAttrBuffer: Pointer;
    HorizTabStop : Pointer;
    VertiTabStop : Pointer;
  end;

{$ENDIF}
{$UNDEF IncludeTerm}

  PTermBuff = ^TTermBuff;
  TTermBuff = record
    Data : PScreenBuffer;
    Attr : PAttrBuffer;
    XAttr: PExtAttrBuffer;
  end;

procedure RegisterTerminalWindowClass(Designing : Boolean);




implementation

type
  PMinMaxInfo = ^TMinMaxInfo;
  TMinMaxInfo = record
    ptReserved     : TPoint;
    ptMaxSize      : TPoint;
    ptMaxPosition  : TPoint;
    ptMinTrackSize : TPoint;
    ptMaxTrackSize : TPoint;
  end;

const
  tmScrollTimer = 1;
  tmBlinkTimer  = 2;
  DefBlinkTime  = 550;

  {Default buffer size}
  DefRows = 200;
  DefCols = 80;
  DefPageHeight = 25;

  {Default font, OemFixedFont}
  DefFont = Oem_Fixed_Font;

  {Capture buffer constants}
  DefCaptureName = 'CAPTURE.CAP';
  MaxCaptureSize = 8192;

  {Tab stop constants}
  DefTabStop     = 8;

  {moved here for access to the emulator types in the terminal window}
  {Emulator types}
  etNone      = 0;
  etANSI      = 1;
  etVT52      = 2;
  etVT100     = 3;
  etANSIBBS   = 4;


{General purpose routines}

  function GetTerminalPtr(HW : TApdHwnd) : TTerminal;
    {-Extract the terminal window object pointer from the window long}
  begin
    GetTerminalPtr := TTerminal(GetWindowLong(HW, gwl_Terminal));
  end;

{TBuffer}

  constructor TBuffer.Create(AWnd : TApdHwnd; Height, Width : Word);
    {-Allocate pages and init fields}
  begin
    inherited Create;
    {Load the default color map}
    Move(ColorValues, bColors, SizeOf(bColors));

    {Set the startup colors}
    bSetColors(aweDefForeground, aweDefBackground);

    {Allocate buffers}
    bScreenBuffer := nil;
    bAttrBuffer := nil;
    bAttrBufferB := nil;
    bExtAttrBuffer := nil;
    bHorizTabStop := nil;
    bVertiTabStop := nil;

    bBufferSize := 0;

    if bNewBuffer(Height, Width, DefPageHeight) <> ecOK then
      raise Exception.Create('Buffer allocation failed');

    {Init fields}
    bWnd := AWnd;
    bCapture := False;
    bCom := nil;
    bExtAttr := 0;
    bBlinkReset := False;
    bInMargins := True;
    bSaveFlag := False;

    {No marking}
    bResetMarking;
    bSetHighlightColors(aweDefBackground, aweDefForeground);

    {Start off with a null emulator}
    bEmulator := nil;
    @bEmuProc := nil;
  end;

  destructor TBuffer.Destroy;
    {-Clean up}
  begin
    if bCapture then
      bSetCapture(False, False, '');
    bDeallocBuffers;
  end;

  procedure TBuffer.bDeallocBuffers;
    {-Release pages}
  begin
    if bScreenBuffer <> nil then
      FreeMem(bScreenBuffer, bBufferSize);
    if bAttrBuffer <> nil then
      FreeMem(bAttrBuffer, bBufferSize);
    if bAttrBufferB <> nil then
      FreeMem(bAttrBufferB, bBufferSize);
    if bExtAttrBuffer <> nil then
      FreeMem(bExtAttrBuffer, bBufferSize);
    if bHorizTabStop <> nil then
      FreeMem(bHorizTabStop, SizeOf(THorizontalTabStop));
    if bVertiTabStop <> nil then
      FreeMem(bVertiTabStop, SizeOf(TVerticalTabStop));
  end;

  function TBuffer.bNewBuffer(Rows, Cols, PageHeight : Word) : Integer;
    {-Free the old buffer, allocate a new one}
  var
    Attr : Byte;
    Loop : Byte;
  begin
    {Check for maximum column size}
    if Cols > MaxCols then begin
      bNewBuffer := ecBadArgument;
      Exit;
    end;

    {Check for to few Rows}
    if (PageHeight > Rows) then begin
      bNewBuffer := ecBadArgument;
      Exit;
    end;

    {Check new size}
    if LongInt(Rows) * Cols > 65535 then begin
      bNewBuffer := ecBadArgument;
      Exit;
    end;

    {Get rid of old buffers, if any}
    bDeallocBuffers;

    {calculate new buffer size}
    bBufferSize := Rows * Cols;                                      

    {Allocate new buffers}
    bScreenBuffer := AllocMem(bBuffersize);
    bAttrBuffer := AllocMem(bBufferSize);
    bAttrBufferB := AllocMem(bBufferSize);
    bExtAttrBuffer := AllocMem(bBufferSize);
    bHorizTabStop := AllocMem(SizeOf(THorizontalTabStop));
    bVertiTabStop := AllocMem(SizeOf(TVerticalTabStop));

    {Okay}
    bNewBuffer := ecOK;

    {Init buffer fields}
    bWidth := Cols;
    bHeight := Rows;
    bX := 0;
    bY := 0;
    bMaxY := PageHeight-1;
    bPageHeight := PageHeight;

    {Initialize screen and attribute buffers}
    FillChar(bScreenBuffer^, bBufferSize, ' ');
    FillChar(bAttrBuffer^, bBufferSize, 0);
    Attr := (bbColor shl 4) or bbColor;
    FillChar(bAttrBuffer^, bWidth*bPageHeight, Attr);

    FillChar(bAttrBufferB^, bBufferSize, 0);
    FillChar(bExtAttrBuffer^, bBufferSize, 0);
    FillChar(bAttrBufferB^, bWidth*bPageHeight, Attr);
    FillChar(bHorizTabStop^, SizeOf(THorizontalTabStop), 0);

    {initialize the tab stop buffer to ever Nth column}
    for Loop := 1 to (bWidth div DefTabStop) do
      bSetHorizontalTabStop(Loop*DefTabStop);

    FillChar(bVertiTabStop^, SizeOf(TVerticalTabStop), 0);

    {Set total number of chars -1 line}
    bBufferLimit :=  LongInt(bWidth)*(bHeight-1);

    {No update needed right now}
    bNeedVScroll := 0;
    bNeedHScroll := 0;
    FillChar(bRedrawRect, SizeOf(bRedrawRect), 0);

    {Initial client area is upper left quadrant of buffer}
    bXPos := 0;
    bYPos := 0;
    cLastHeight := 0;
    cLastWidth := 0;
    cMarginBottom := bPageHeight;
  end;

  procedure TBuffer.bSetColors(FC, BC : Word);
    {-Set default colors}
  begin
    bfColorOrg := FC;
    bbColorOrg := BC;
    bfColor := FC;
    bbColor := BC;
    bSetHighlightColors(BC, FC);
  end;

  procedure TBuffer.bSetHighlightColors(FC, BC : Word);
  begin
    bMarkColorF := FC;
    bMarkColorB := BC;
  end;

  procedure TBuffer.bFlushCapture;
    {-Flush capture file, turn off capture on error}
  var
    BW : Cardinal;
    Res : Integer;
  begin
    if bCapIndex >= 1 then begin                                    
      BlockWrite(bCaptureFile, bCapBuffer^, bCapIndex, BW);
      if BW <> bCapIndex then
        Res := ecDiskFull
      else
        Res := IoResult;
      if Res <> ecOK then begin
        FreeMem(bCapBuffer, MaxCaptureSize);
        Close(bCaptureFile);
        bCapture := False;                                         
        if IoResult <> ecOK then ;
        SendMessage(bWnd, apw_TermError, Word(-Res), 0);
      end;
      bCapIndex := 0;
    end;
  end;

  procedure TBuffer.bAddToCapture(C : Char);
    {-Add C to capture file, turn off capture on error}
  begin
    if bCapture then begin
      Inc(bCapIndex);
      bCapBuffer^[bCapIndex] := C;
      if bCapIndex = MaxCaptureSize then
        bFlushCapture;
    end;
  end;

  function TBuffer.bSetCapture(Enable, Append : Bool; FName : PChar) : Integer;
    {-Turn capturing on/off}
  var
    Res : Word;
  begin
    if Enable and not bCapture then begin
      {Allocate a capture buffer}
      bCapBuffer := AllocMem(MaxCaptureSize);

      {Get file name}
      if FName[0] = #0 then
        StrCopy(bCaptureName, DefCaptureName)
      else
        StrCopy(bCaptureName, FName);
      {Open the file...}
      Assign(bCaptureFile, bCaptureName);
      if Append then begin
        {Appending, get file size, seek to end}
        Reset(bCaptureFile, 1);
        Res := IoResult;
        case Res of
          0 : begin
                Seek(bCaptureFile, FileSize(bCaptureFile));
                Res := IoResult;
              end;
          2 : begin
                Rewrite(bCaptureFile, 1);
                Res := IoResult;
              end;
        end;
        if Res <> 0 then begin
          Close(bCaptureFile);
          bCapture := False;                                    
          if IoResult <> 0 then ;
        end;
      end else begin
        {Not appending, open new file}
        Rewrite(bCaptureFile, 1);
        Res := IoResult;
      end;

      {If capture started okay, init fields}
      if Res = ecOK then begin
        bCapture := True;
        bCapIndex := 0;
      end;
    end else if bCapture then begin
      {Ending capture, close file and release buffer}
      bFlushCapture;
      Close(bCaptureFile);
      Res := IoResult;
      FreeMem(bCapBuffer, MaxCaptureSize);
      bCapture := False;
    end else
      Res := 0;
    bSetCapture := -Res;
  end;

  procedure TBuffer.bSetScrollMode(Scrollback : Bool);
    {-Set flag in buffer for normal or scrollback}
  begin
    bScrollback := Scrollback;
  end;

  procedure TBuffer.bInvalidateChar(X, Y : Word);
    {-Invalidate client rectangle containing Buffer location X, Y}
  var
    Rect : TRect;
  begin
    {Convert buffer(X,Y) to client area coordinates}
    with Rect do begin
      Left   := (X-bXPos) * bCharWidth;
      Top    := (Y-bYPos) * bCharHeight;
      Right  := Left + bCharWidth;
      Bottom := Top + bCharHeight;

      if (Bottom <= cSizeY) then
        {Merge this invalid area with existing update rectangle}
        UnionRect(bRedrawRect, bRedrawRect, Rect)
      else
        {Character is outside of client area, just move caret}
        bMoveCaret;
    end;
  end;

  procedure TBuffer.bPostStatusMsg;
    {-Send a status message to the window}
  var
    Row, Col, Left : Word;
    Top : LongInt;
  begin
    if bScrollback then begin
      {Row/Col always zero when in scrollback mode}
      Row := 0;
      Col := 0;
    end else begin
      {Set Row/Col to current cursor position}
      Col := bX+1;
      Row := bY-bYPos+1;
    end;

    {Coordinate of top/left visible corner}
    Left := bXPos+1;
    Top := bYPos+1;
    PostMessage(bWnd, apw_TermStatus,
                (Col shl 8) or Row,
                (LongInt(Left) shl 16) or Top);
  end;

  procedure TBuffer.bUpdateFont(Height, Width : Word);
    {-Set new char width and height values}
  begin
    bCharHeight := Height;
    bCharWidth  := Width;
    cHeight := cSizeY div bCharHeight;
    cWidth := cSizeX div bCharWidth;
    cMarginBottom := bPageHeight;
    cMarginTop := 1;
  end;

  procedure TBuffer.bUpdateBuffer;
    {-Adjust buffer contents or bYPos,bXPos for new bX,bY value}
  var
    Diff : Integer;
    Max  : Word;
    TempBuf        : array[0..MaxCols-1] of byte;
    TempAttrBuf    : array[0..MaxCols-1] of byte;
    TempAttrBufB   : array[0..MaxCols-1] of byte;
    TempExtAttrBuf : array[0..MaxCols-1] of byte;
    Limit : Word;
    MoveSize : Word;
    MoveFrom : Word;
    MoveTo : Word;
  begin
    {Adjust highwater mark}
    if (bY > bMaxY) and (bY <> bHeight) then begin
      bMaxY := bY;
      if bScrollback then begin
        {Adjust scroll range}
        if bMaxY >= cHeight then
          Max := (bMaxY-cHeight)+1
        else
          Max := bMaxY;
        SetScrollRange(bWnd, sb_Vert, 1, Max, False);
      end;
    end;

    MoveSize := bBufferLimit-((bPageHeight-cMarginBottom)*bWidth);
    Limit := bY+(bPageHeight-cMarginBottom);

    if Limit = bHeight then begin
      {Buffer is full, move data and attributes up one line, clear last line}
      Move(bScreenBuffer^[bWidth], bScreenBuffer^[0], MoveSize);
      Move(bAttrBuffer^[bWidth], bAttrBuffer^[0], MoveSize);
      Move(bAttrBufferB^[bWidth], bAttrBufferB^[0], MoveSize);
      Move(bExtAttrBuffer^[bWidth], bExtAttrBuffer^[0], MoveSize);

      FillChar(bScreenBuffer^[MoveSize], bWidth, ' ');
      FillChar(bAttrBuffer^[MoveSize], bWidth, (bbColor shl 4) or bfColor);
      FillChar(bAttrBufferB^[MoveSize], bWidth, (bbColor shl 4) or bfColor);
      FillChar(bExtAttrBuffer^[MoveSize], bWidth, bExtAttr);

      {One line free again, need a scroll}
      Dec(bY);
      Inc(bNeedVScroll, -BCharHeight);
    end else if bY >= (bYPos+cMarginBottom) then begin
      if bInMargins then begin
        {Client area should be scrolled up}
        Diff := bY-(bYPos+cMarginBottom-1);
        Inc(bNeedVScroll, -(Diff*bCharHeight));
        Inc(bYPos, Diff);
        if cMarginBottom < cHeight then begin
          MoveFrom := (bYPos+cMarginBottom)*bWidth;
          MoveTo := (bYPos+cMarginBottom-1)*bWidth;
          MoveSize := bBufferLimit-MoveFrom;

          {Step 1 - Move bottom-fixed area of client window down}
          Move(bScreenBuffer^[MoveTo], bScreenBuffer^[MoveFrom], MoveSize);
          Move(bAttrBuffer^[MoveTo], bAttrBuffer^[MoveFrom], MoveSize);
          Move(bAttrBufferB^[MoveTo], bAttrBufferB^[MoveFrom], MoveSize);
          Move(bExtAttrBuffer^[MoveTo], bExtAttrBuffer^[MoveFrom], MoveSize);

          {Clear first line of bottom-fixed area of client window}
          FillChar(bScreenBuffer^[MoveTo], bWidth, ' ');
          FillChar(bAttrBuffer^[MoveTo], bWidth, ' ');
          FillChar(bAttrBufferB^[MoveTo], bWidth, ' ');
          FillChar(bExtAttrBuffer^[MoveTo], bWidth, ' ');
        end;

        if cMarginTop > 1 then begin
          {Save top line of scrolling window (temp)}
          MoveTo := (((bYPos-1)+cMarginTop)-1)*bWidth;

          Move(bScreenBuffer^[MoveTo], TempBuf[0], bWidth);
          Move(bAttrBuffer^[MoveTo], TempAttrBuf[0], bWidth);
          Move(bAttrBufferB^[MoveTo], TempAttrBufB[0], bWidth);
          Move(bExtAttrBuffer^[MoveTo], TempExtAttrBuf[0], bWidth);

          {Move top-fixed area of client window down}
          MoveTo := (bYPos-1)*bWidth;
          MoveFrom := (bYPos)*bWidth;
          MoveSize := ((((bYPos-1)+cMarginTop)-1)*bWidth)-(MoveTo);

          Move(bScreenBuffer^[MoveTo], bScreenBuffer^[MoveFrom], MoveSize);
          Move(bAttrBuffer^[MoveTo], bAttrBuffer^[MoveFrom], MoveSize);
          Move(bAttrBufferB^[MoveTo], bAttrBufferB^[MoveFrom], MoveSize);
          Move(bExtAttrBuffer^[MoveTo], bExtAttrBuffer^[MoveFrom], MoveSize);

          {restore saved line to top line of client}
          Move(TempBuf[0], bScreenBuffer^[MoveTo], bWidth);
          Move(TempAttrBuf[0], bAttrBuffer^[MoveTo], bWidth);
          Move(TempAttrBufB[0], bAttrBufferB^[MoveTo], bWidth);
          Move(TempExtAttrBuf[0], bExtAttrBuffer^[MoveTo], bWidth);
        end;
      end else if (bY >= (bYPos+cHeight)) then
        Dec(bY);
    end else
      {No scrolling necessary, just set new caret position}
      bMoveCaret;

    {Make sure cursor remains visible}
    if bY >= (bYPos+cHeight) then begin
      {Client area should be scrolled up}
      Diff := bY-(bYPos+cHeight-1);
      Inc(bNeedVScroll, -(Diff*bCharHeight));
      Inc(bYPos, Diff);
    end else if bY < bYPos then begin
      {Client area should be scrolled down}
      Diff := bYPos-bY;
      Inc(bNeedVScroll, Diff*bCharHeight);
      Inc(bYPos, -Diff);
    end;
  end;

  procedure TBuffer.bWriteChar(C : Char);
    {-Write C}
  var
    BuffPos : Word;

    procedure NewLine;
      {-Advance one line}
    var
      FillPos : Word;
    begin
      {Advance buffer to next line}
      Inc(bY);

      {Update buffer for new bY}
      bUpdateBuffer;

      {If scrolled up, fill current line attributes with current background}

      if ((bNeedVScroll < 0) and ((bY+1) >= (bYPos+cMarginBottom))) then begin
        FillPos := bY*bWidth;
        FillChar(bAttrBuffer^[FillPos], bWidth, (bbColor shl 4) or bfColor);
        FillChar(bAttrBufferB^[FillPos], bWidth, (bbColor shl 4) or bfColor);
        FillChar(bExtAttrBuffer^[FillPos], bWidth, bExtAttr);
      end;
    end;

  begin
    {margin check}
    if ((((bY-bYPos)+1) >= cMarginTop) and
        (((bY-bYPos)+1) <= cMarginBottom)) then
       bInMargins := True
     else
       bInMargins := False;

    {Insert character}
    case C of
      cCR :
        begin
          bX := 0;
          bMoveCaret;
        end;
      cLF :
        NewLine;
      cBS :
        if bX <> 0 then begin
          Dec(bX);
          bScreenBuffer^[bY*bWidth+bX] := ' ';
          bInvalidateChar(bX, bY);
        end;
      else
        begin
          BuffPos := bY*bWidth+bX;
          bScreenBuffer^[BuffPos] := C;
          bAttrBuffer^[BuffPos] := (bbColor shl 4) or bfColor;
          bAttrBufferB^[BuffPos] := (bbColor shl 4) or bfColor;
          bExtAttrBuffer^[BuffPos] := bExtAttr;

          if ByteFlagIsSet(bExtAttr, eattrBlink) and bBlinkReset then begin
            bAttrBuffer^[BuffPos] := ((bAttrBuffer^[BuffPos] shr 4) shl 4) or
                                     (bAttrBuffer^[buffPos] shr 4);
          end;

          bInvalidateChar(bX, bY);
          if bX >= (bWidth-1) then begin
            bX := 0;
            NewLine;
          end else
            Inc(bX);
        end;
    end;

    {Add to capture file}
    bAddToCapture(C);
  end;

  procedure TBuffer.bClearScreen;
    {-Simulate clear screen by bringing bottom of physical screen to top}
  var
    I : Word;
    BuffPos : Word;
    FillSize : Word;
  begin
    bX := 0;
    if (bYPos + (bPageHeight*2)) < bHeight then
      {Still in virgin part of buffer}
      Inc(bYPos, bPageHeight)
    else begin
      {Issue enough newlines to get the current page to scroll off}
      bY := bHeight-1;
      for I := 1 to bPageHeight do
        bWriteChar(cLF);
      bYPos := bHeight-bPageHeight;
    end;
    bY := bYPos;
    BuffPos := bY*bWidth;
    FillSize := bWidth*bPageHeight;
    FillChar(bScreenBuffer^[BuffPos], FillSize, ' ');
    FillChar(bAttrBuffer^[BuffPos], FillSize, (bbColor shl 4) or bfColor);
    FillChar(bAttrBufferB^[BuffPos], FillSize,(bbColor shl 4) or bfColor);
    FillChar(bExtAttrBuffer^[BuffPos], FillSize, bExtAttr);

    InvalidateRect(bWnd, nil, False);
    UpdateWindow(bWnd);
  end;

  procedure TBuffer.bSortTabBuffer(var TabBuffer; Size: Byte);
  var
    DoneSort : Bool;
    Loop : Byte;
    Exch : Byte;
  begin
    repeat
      DoneSort := True;
      for Loop := 1 to Size-1 do begin
        if TByteBuffer(TabBuffer)[Loop] =
           TByteBuffer(TabBuffer)[Loop+1] then
             TByteBuffer(TabBuffer)[Loop] := 0;
        if TByteBuffer(TabBuffer)[Loop] >
           TByteBuffer(TabBuffer)[Loop+1] then begin
          Exch := TByteBuffer(TabBuffer)[Loop];
          TByteBuffer(TabBuffer)[Loop] := TByteBuffer(TabBuffer)[Loop+1];
          TByteBuffer(TabBuffer)[Loop+1] := Exch;
          DoneSort := False;
        end;
      end;
    until DoneSort;
  end;

  function TBuffer.bGetNextTabStop(CurrentPos, Count : Byte;
                                   var TabBuffer; Size : Byte): Byte;
  var
    TabLoop : Byte;
  begin
    bGetNextTabStop := CurrentPos;
    for TabLoop := 1 to Size do begin
      if (TByteBuffer(TabBuffer)[TabLoop] > CurrentPos) and
         (TByteBuffer(TabBuffer)[TabLoop] < bWidth)  then begin
        bGetNextTabStop := TByteBuffer(TabBuffer)[TabLoop];
        Dec(Count, 1);
        if Count = 0 then
          exit;
      end;
    end;
  end;

  function TBuffer.bGetPrevTabStop(CurrentPos, Count : Byte;
                                   var TabBuffer; Size : Byte): Byte;
  var
    TabLoop : Byte;
  begin
    bGetPrevTabStop := CurrentPos;
    for TabLoop := Size downto 1 do begin
      if TByteBuffer(TabBuffer)[TabLoop] < CurrentPos then begin
        if TByteBuffer(TabBuffer)[TabLoop] > 0 then
          bGetPrevTabStop := TByteBuffer(TabBuffer)[TabLoop];
        Dec(Count, 1);
        if Count = 0 then
          exit;
      end;
    end;
  end;

  procedure TBuffer.bSetHorizontalTabStop(Column : Byte);
  begin
    {-see if room in tab stop buffer}
    if bHorizTabStop^[1] = 0 then begin
      {-put new tab stop in buffer}
      bHorizTabStop^[1] := Column;

      {-sort the tab buffer to a logical order}
      bSortTabBuffer(bHorizTabStop^, SizeOf(THorizontalTabStop));
    end;
  end;

  procedure TBuffer.bClearHorizontalTabStop(Column : Byte);
  var
    TabLoop : Byte;
    FoundTab : Boolean;
  begin
    FoundTab := False;
    for TabLoop := 1 to SizeOf(THorizontalTabStop) do begin
      if bHorizTabStop^[TabLoop] = Column then begin
        FoundTab := True;
        bHorizTabStop^[TabLoop] := 0;
      end;
    end;
    {-sort the tab buffer to a logical order}
    if FoundTab then
      bSortTabBuffer(bHorizTabStop^, SizeOf(THorizontalTabStop));
  end;

  procedure TBuffer.bSetVerticalTabStop(Row : Byte);
  begin
    if bVertiTabStop^[1] = 0 then begin
      {-put new tab stop in buffer}
      bVertiTabStop^[1] := Row;

      {-sort the tab buffer to a logical order}
      bSortTabBuffer(bVertiTabStop^, SizeOf(TVerticalTabStop));
    end;
  end;

  procedure TBuffer.bClearVerticalTabStop(Row : Byte);
  var
    TabLoop : Byte;
    FoundTab : Boolean;
  begin
    FoundTab := False;
    for TabLoop := 1 to SizeOf(TVerticalTabStop) do begin
      if bVertiTabStop^[TabLoop] = Row then begin
        FoundTab := True;
        bVertiTabStop^[TabLoop] := 0;
      end;
    end;
    {-sort the tab buffer to a logical order}
    if FoundTab then
      bSortTabBuffer(bVertiTabStop^, SizeOf(TVerticalTabStop));
  end;

  procedure TBuffer.bProcessChar(C : Char);
    {-Show C to emulator, process results}
  var
    TempBColor : Byte;
    Start, Limit : Word;
    I : Word;
    MoveSize : Word;
    UpdateRect : TRect;

    procedure GetChangedRect(StartChange, EndChange: Word; var Dest : TRect);
    var
      ClientTop   : Word;
      SRow, LRow  : Word;
    begin
      { The clients top position in the buffer }
      ClientTop := (bYPos * bWidth);

      { Get the first character row }
      SRow := (StartChange-ClientTop) div bWidth;

      { Get the last character row }
      LRow := ((EndChange-ClientTop) div bWidth) + 1;

      { calculate a new TRect structure for the screen }
      Dest.Top    := SRow*bCharHeight;
      Dest.Left   := 0;
      Dest.Bottom := LRow*bCharHeight;
      Dest.Right  := bWidth*bCharWidth;                  
    end;

    procedure ClearPart(Start, Limit : Word);
      {-Clear part of the buffer and redraw}
    var
      FillSize  : Word;
      ClearRect : TRect;
    begin
      FillSize := Limit-Start;
      FillChar(bScreenBuffer^[Start], FillSize, ' ');
      FillChar(bAttrBuffer^[Start], FillSize, (bbColor shl 4) or bfColor);
      FillChar(bAttrBufferB^[Start], FillSize, (bbColor shl 4) or bfColor);
      FillChar(bExtAttrBuffer^[Start], FillSize, 0);

      GetChangedRect(Start, Limit, ClearRect); 
      InvalidateRect(bWnd, @ClearRect, False);

      UpdateWindow(bWnd);
    end;

    procedure ReportCursorPosition;
      {-Output CPR sequence with cursor position (no error checking)}
    var
      cpX : String[3];
      cpY : String[3];
      RCP : String[10];
    begin
      {convert the values to strings}
      Str(bX+1, cpX);
      Str((bY-bYPos)+1, cpY);

      {create the ANSI sequence}
      RCP := #27'['+cpY+';'+cpX+'R';
      bCom.PutBlock(RCP[1], Length(RCP));
    end;

    procedure ReportDeviceAttributes(TermType : Byte);
      {-Output DA sequence specifing the VT terminal type}
    const
      RDA100 : array[0..7] of char = #27'[?1;0c';
      RDA52  : array[0..3] of char = #27'/Z';
    begin
      Case TermType of
        0 : bCom.PutBlock(RDA52, 3);
        1 : bCom.PutBlock(RDA100, 7);
      end;
    end;

  begin
    {If emulator was attached, call it}
    if (bEmulator <> nil) and (@bEmuProc <> nil) then begin
      bEmuProc(bEmulator, C, bEC);
    end else begin
      bEC.Cmd := eChar;
      bEC.Ch := C;
    end;

    {Process emulator results}
    with bEC do begin
      case Cmd of
        eNone : {etNONE, etVT52, etANSI, etVT100}
          {nothing to do} ;
        eHT  :  {etNONE, etVT52, etANSI, etVT100}
          begin
            bX := bGetNextTabStop(bX, 1, bHorizTabStop^,
                                 SizeOf(THorizontalTabStop));
            if bX > bWidth then
              bX := bWidth;

            {Update the caret position}
            bMoveCaret;
          end;
        eChar : {etNONE, etVT52, etANSI, etVT100}
          if (Ch <> #0) then
            bWriteChar(Ch);
        eSGR : {etANSI, etVT100}
          if (FColor in [emBlack..emWhiteBold]) and
             (BColor in [emBlack..emWhiteBold]) then begin          
            bfColorOrg := FColor;
            bbColorOrg := BColor;
            bExtAttr := ExtAttr;
            if ByteFlagIsSet(ExtAttr, eattrInverse) then begin
              bfColor := BColor;
              bbColor := FColor;
            end else begin
              bfColor := FColor;
              bbColor := BColor;
            end;
          end;
        eString :
          {AProBCB version only has the Byte version of this array.}
          {$IFDEF AProBCB}
          for I := 1 to OtherStrLen do
          	  bWriteChar(Char(OtherStr[I]));
          {$ELSE}
          for I := 1 to Length(OtherStr) do
            bWriteChar(OtherStr[I]);
          {$ENDIF}
        eCUP : {eGotoXY, eHVP} {etVT52, etANSI, etVT100}
          begin
            if Integer(X) > 0 then
              bX := bXPos + (X-1);
            if Integer(Y) > 0 then
              bY := bYPos + (Y-1);

            {Limit checks...}
            if bX >= bWidth then
              bX := bWidth-1
            else if Integer(bX) < 0 then
              bX := 0;
            if bY >= bHeight then
              bY := bHeight-1
            else if integer(bY) < 0 then
              bY := 0;

            {Update the caret position}
            bMoveCaret;

            {margin check}
            if (((bY-bYPos)+1) >= cMarginTop) and
               (((bY-bYPos)+1) <= cMarginBottom) then
              bInMargins := True
            else
              bInMargins := False;
          end;
        eCUU : {eUp, eRI} {etVT52, etANSI, etVT100}
          begin
            {margin check}
            if ((((bY-bYPos)+1) >= cMarginTop) and
               (((bY-bYPos)+1) <= cMarginBottom))
               or
               {will cursor 'pass through' margin}
               ((((bY-bYPos)+1-Y) < cMarginTop) and
               (((bY-bYPos)+1) > cMarginTop)) then
              bInMargins := True
            else
              bInMargins := False;

            if Y > bY then
              Y := bY;
            Dec(bY, Y);

            {check if in a scrolling area and limit top}
            if bInMargins and (((bY-bYPos)+1) <= cMarginTop) then
              bY := (bYPos+cMarginTop)-1

            {limit check for top of client}
            else if integer(bY) < bYPos then
              bY := bYPos;

            {Update the caret position}
            bMoveCaret;
          end;
        eCUD : {eDown, eHPR, eIND} {etVT52, etANSI, etVT100}
          begin
            {margin check}
            if ((((bY-bYPos)+1) >= cMarginTop) and
               (((bY-bYPos)+1) <= cMarginBottom))
               or
               {will cursor 'pass through' margin}
               ((((bY-bYPos)+1) < cMarginBottom) and
               (((bY-bYPos)+1+Y) > cMarginBottom)) then
              bInMargins := True
            else
              bInMargins := False;

            if bY+Y < bHeight then begin
              Inc(bY, Y);

              {check if in a scrolling area and limit bottom}
              if bInMargins and (((bY-bYPos)+1) > cMarginBottom) then
                bY := (bYPos+cMarginBottom)-1
              {limit check for bottom of client}
              else if bY >= (bYPos+cHeight) then
                bY := (bYPos+cHeight)-1;

              {Update the caret position}
              bMoveCaret;
            end;
          end;
        eCUF : {eRight} {etVT52, etANSI, etVT100}
          if bX+X < bWidth then begin
            Inc(bX, X);
            {Update the caret position}
            bMoveCaret;
          end;
        eCUB : {eLeft} {etVT52, etANSI, etVT100}
          begin
            if X > bX then
              X := bX;
            Dec(bX, X);
            {Update the caret position}
            bMoveCaret;
          end;
        eClearBelow,
        eClearAbove,
        eClearScreen,
        eED : {etVT52, etANSI, etVT100}
          begin
            TempBColor := bbColor;
            if Y = 1 then begin
              if ByteFlagIsSet(ExtAttr, eAttrInverse) then
                bbColor := bfColor;
            end;
            case X of
              0 : {eClearBelow}
                begin
                  Start := (bY*bWidth)+bX;
                  ClearPart(Start, bBufferSize);
                end;
              1 : {eClearAbove}
                begin
                  Limit := (bY*bWidth)+bX+1;
                  ClearPart(0, Limit);
                end;
              2 : bClearScreen; {eClearScreen}
            end;
            if Y = 1 then
              bbColor := TempBColor;
          end;
        eClearEndOfLine,
        eClearStartOfLine,
        eClearLine,
        eEL : {etVT52, etANSI, etVT100}
          begin
            TempBColor := bbColor;
            if Y = 1 then begin
              if ByteFlagIsSet(ExtAttr, eAttrInverse) then
                bbColor := bfColor;
            end;
            case X of
              0 : {eClearEndOfLine}
                begin
                  Start := (bY*bWidth)+bX;
                  Limit := Start+(bWidth-bX)+1;
                  if (Limit > (bHeight * bWidth)) then
                    Limit := bHeight * bWidth;                    
                  ClearPart(Start, Limit);
                end;
              1 : {eClearStartOfLine}
                begin
                  Start := bY*bWidth;
                  Limit := Start+bX+1;
                  ClearPart(Start, Limit);
                end;
              2 : {eClearLine}
                begin
                  Start := bY*bWidth;
                  Limit := Start+bWidth;
                  ClearPart(Start, Limit);
                end;
            end;
            if Y = 1 then
              bbColor := TempBColor;
          end;
        eSaveCursorPos : {etANSI, etVT100}
          begin
            bSaveFlag := True;
            bSaveXLoc := bX;
            bSaveYLoc := bY;
            bSaveAttr   := (bbColor shl 4) or bfColor;
            bSaveAttrEx :=  bExtAttr;
          end;
        eRestoreCursorPos : {etANSI, etVT100}
          begin
            if bSaveFlag then begin
              bX := bSaveXLoc;
              bY := bSaveYLoc;
              bbColor := bSaveAttr shr 4;
              bfColor := bSaveAttr and $0F;
              bExtAttr := bSaveAttrEx;
              bSaveFlag := False;
            end else begin
              if (bEC.emuType = etANSI) then begin
                bX := bXPos;
                bY := bYPos;
                bbColor := emBlack;
                bfColor := emWhite;
                bExtAttr := 0;
              end;
            end;

            {Update the caret position}
            bMoveCaret;
          end;
        eDSR : {eDeviceStatusReport} {etANSI, etVT100}
          begin
            if X = 6 then
              ReportCursorPosition;
          end;

        eCHA : {eHPA} {etANSI}
          begin
            if Integer(X)-1 >= 0 then
              bX := bXPos + (X-1);

            {Limit checks...}
            if bX >= bWidth then
              bX := bWidth-1
            else if Integer(bX) < 0 then
              bX := 0;

            {Update the caret position}
            bMoveCaret;
          end;
        eCNL : {etANSI}
          begin
            if Y = 0 then
              Y := 1;
            if bY+Y < bHeight then
              Inc(bY, Y);
            bX := bXPos;

            {Update the caret position}
            bMoveCaret;
          end;
        eCPL : {etANSI}
          begin
            if ((((bY-bYPos)+1) >= cMarginTop) and
                (((bY-bYPos)+1) <= cMarginBottom)) then
              bInMargins := True
            else
              bInMargins := False;

            if Y = 0 then
              Y := 1;
            if Y > bY then
              Y := bY;
            Dec(bY, Y);
            bX := bXPos;

            {Update the caret position}
            bMoveCaret;
          end;
        eVPA :{etANSI}
          begin
            if Integer(Y) > 0 then
              bY := bYPos + (Y-1);

            {Limit checks...}
            if bY >= bHeight then
              bY := bHeight-1
            else if integer(bY) < 0 then
              bY := 0;

            {Update the caret position}
            bMoveCaret;
          end;
        eDCH : {etANSI}
          begin
            Start := (bY*bWidth)+bX;
            Limit := Start+X;
            Move(bScreenBuffer^[Limit], bScreenBuffer^[Start],
                     bBufferLimit-(Limit));
            Move(bAttrBuffer^[Limit], bAttrBuffer^[Start],
                     bBufferLimit-(Limit));
            Move(bAttrBufferB^[Limit], bAttrBufferB^[Start],
                     bBufferLimit-(Limit));
            Move(bExtAttrBuffer^[Limit], bExtAttrBuffer^[Start],
                     bBufferLimit-(Limit));

            GetChangedRect(Start, Limit, UpdateRect);  
            InvalidateRect(bWnd, @UpdateRect, False);
            UpdateWindow(bWnd);
          end;
        eDL : {etANSI}
          begin
            if Y = 0 then
              Y := 1;

            if (bYPos-bPageHeight) >= 0 then
              if Y > (bYPos-bPageHeight) then
                Y := (bYPos-bPageHeight);

            Start := bY*bWidth;
            Limit := Start+(Y*bWidth);
            Move(bScreenBuffer^[Limit],  bScreenBuffer^[Start],
                     bBufferLimit-Limit);
            Move(bAttrBuffer^[Limit],    bAttrBuffer^[Start],
                     bBufferLimit-Limit);
            Move(bAttrBufferB^[Limit],   bAttrBufferB^[Start],
                     bBufferLimit-Limit);
            Move(bExtAttrBuffer^[Limit], bExtAttrBuffer^[Start],
                     bBufferLimit-Limit);

            GetChangedRect(Start, Start+(cMarginBottom*bWidth), UpdateRect);
            InvalidateRect(bWnd, @UpdateRect, False);
            UpdateWindow(bWnd);
          end;
        eECH : {etANSI}
          begin
            Start := (bY*bWidth)+bX;
            Limit := Start+X;
            ClearPart(Start, Limit);
          end;
        eICH : {etANSI}
          begin
            Start := (bY*bWidth)+bX;
            Limit := Start+X;
            if Limit > ((bY+1)*bWidth) then
              Limit := ((bY+1)*bWidth);
            if Limit < ((bY+1)*bWidth) then begin
              MoveSize := bWidth-(X+bX);
              Move(bScreenBuffer^[Start], bScreenBuffer^[Limit], MoveSize);
              Move(bAttrBuffer^[Start], bAttrBuffer^[Limit], MoveSize);
              Move(bAttrBufferB^[Start], bAttrBufferB^[Limit], MoveSize);
              Move(bExtAttrBuffer^[Start], bExtAttrBuffer^[Limit], MoveSize);
            end;
            ClearPart(Start, Limit);
          end;
        eIL : {etANSI, etVT100}
          begin
            if ((((bY-bYPos)+1) >= cMarginTop) and
               (((bY-bYPos)+1) <= cMarginBottom)) then
              bInMargins := True
            else
              bInMargins := False;

            Start := bY*bWidth;
            Limit := Start+(Y*bWidth);

            MoveSize := bPageHeight*bWidth;

            if Limit > MoveSize then
              MoveSize := (MoveSize - (Limit-Start))
            else
              Dec(MoveSize, Limit);

            if Limit < bBufferSize then begin                      
              Move(bScreenBuffer^[Start], bScreenBuffer^[Limit], MoveSize);
              Move(bAttrBuffer^[Start], bAttrBuffer^[Limit], MoveSize);
              Move(bAttrBufferB^[Start], bAttrBufferB^[Limit], MoveSize);
              Move(bExtAttrBuffer^[Start], bExtAttrBuffer^[Limit], MoveSize);
            end;                                                   
            ClearPart(Start, Limit);

            GetChangedRect(Start, Start+(cMarginBottom*bWidth), UpdateRect);
            InvalidateRect(bWnd, @UpdateRect, False);
            UpdateWindow(bWnd);
          end;
        eCPR : {etANSI, etVT100}
          SendMessage(bWnd, apw_CursorPosReport, X, Y);
        eDA  : {etANSI, etVT100}
          begin
            Case X of
              0 : ReportDeviceAttributes(Y);
            end;
          end;
        eNEL : {etANSI, etVT100}
          begin
            if ((((bY-bYPos)+1) >= cMarginTop) and
               (((bY-bYPos)+1) <= cMarginBottom)) then
              bInMargins := True
            else
              bInMargins := False;

            if bY+1 < bHeight then
              Inc(bY, 1);
            bX := 0;

            {Update the caret position}
            bMoveCaret;
          end;
         {-tabulation}
        eCBT : {etANSI}
          begin
            bX := bGetPrevTabStop(bX+1, X, bHorizTabStop^,
                                  SizeOf(THorizontalTabStop))-1;
            {Update the caret position}
            bMoveCaret;
           end;
        eCHT : {etANSI}
          begin
            bX := bGetNextTabStop(bX+1, X, bHorizTabStop^,
                                  SizeOf(THorizontalTabStop))-1;

            {Update the caret position}
            bMoveCaret;
          end;
        eCTC : {etANSI}
          begin
            case X of
              0 : bSetHorizontalTabStop(bX+1);
              1 : begin
                    if bY > bPageHeight-1 then
                      Start := bPageHeight
                    else
                      Start := bY+1;
                    bSetVerticalTabStop(Start);
                  end;
              2 : bClearHorizontalTabStop(bX+1);
              3 : begin
                    if bY > bPageHeight-1 then
                      Start := bPageHeight
                    else
                      Start := bY+1;
                    bClearVerticalTabStop(Start);
                  end;
              4,
              5 : begin
                    FillChar(bHorizTabStop^, SizeOf(THorizontalTabStop), 0);
                    for Limit := 1 to (cWidth div DefTabStop) do
                      bSetHorizontalTabStop(Limit*DefTabStop);
                  end;
              6 : FillChar(bVertiTabStop^, SizeOf(TVerticalTabStop), 0);
            end;
          end;
        eCVT : {etANSI}
          begin
            if bY < bPageHeight-1 then begin
              bY := bGetNextTabStop(bY+1, Y, bVertiTabStop^,
                                  SizeOf(TVerticalTabStop))-1;

            {Update the caret position}
            bMoveCaret;
            end;
          end;
        eHTJ : {etANSI}
          begin
            X := (bGetNextTabStop(bX+1, 1, bHorizTabStop^,
                                 SizeOf(THorizontalTabStop))-1)-bX;
            Start := (bY*bWidth)+bX;
            Limit := Start+X;
            if Limit > ((bY+1)*bWidth) then
              Limit := ((bY+1)*bWidth);
            if Limit < ((bY+1)*bWidth) then begin
              MoveSize := bWidth-(X+bX);
              Move(bScreenBuffer^[Start], bScreenBuffer^[Limit], MoveSize);
              Move(bAttrBuffer^[Start], bAttrBuffer^[Limit], MoveSize);
              Move(bAttrBufferB^[Start], bAttrBufferB^[Limit], MoveSize);
              Move(bExtAttrBuffer^[Start], bExtAttrBuffer^[Limit], MoveSize);
            end;
            Inc(bX, X);
            ClearPart(Start, Limit);
          end;
        eHTS : {etANSI, etVT100}
          begin
            bSetHorizontalTabStop(bX+1);
          end;
        eTBC : {etANSI}
          begin
            case X of
              0 : bClearHorizontalTabStop(bX+1);
              1 : begin
                    if bY > bPageHeight-1 then
                      Start := bPageHeight
                    else
                      Start := bY+1;
                    bClearVerticalTabStop(Start);
                  end;
              2,
              3 : begin
                    FillChar(bHorizTabStop^, SizeOf(THorizontalTabStop), 0);
                    for Limit := 1 to (cWidth div DefTabStop) do
                      bSetHorizontalTabStop(Limit*DefTabStop);
                  end;
              4 : FillChar(bVertiTabStop^, SizeOf(TVerticalTabStop), 0);
            end;
          end;
        eVTS : {etANSI}
          begin
            if bY > bPageHeight-1 then
              Start := bPageHeight
            else
              Start := bY+1;
            bSetVerticalTabStop(Start);
          end;
        eDECSTBM : {etVT100}
          begin
            cMarginTop := X;
            cMarginBottom := Y;
            bX := bXPos;
            bY := bYPos;
            bInMargins := (cMarginTop = 1);

            {Update the caret position}
            bMoveCaret;
          end;
      end;
    end;
  end;

  procedure TBuffer.bMoveCaret;
    {-Set new caret position}
  begin
    if bFocused then
      SetCaretPos((bX-bXPos)*bCharWidth, (bY-bYPos)*bCharHeight);
  end;

  function TBuffer.bCurLineLength : Word;
    {-Return the length of the current line}
  var
    I : Word;
    Start, Stop : Word;
  begin
    {Find index of end of current line}
    Stop := (bY*bWidth)+bWidth-1;
    Start := (Stop+1)-bWidth;
    for I := Stop downto Start do
      if bScreenBuffer^[I] <> ' ' then begin
        bCurLineLength := (I-Start)+1;
        Exit;
      end;

    {Empty line if we get here}
    bCurLineLength := 0;
  end;

  function TBuffer.bNeedPaint : Bool;
    {-Return True if the RedrawRect is not empty}
  begin
    bNeedPaint := (bRedrawRect.Top - bRedrawRect.Bottom) <> 0;
  end;

  procedure TBuffer.bForcePaint;
    {-Force paint message}
  var
    Rect : TRect;
  begin
    if bNeedPaint then begin
      InvalidateRect(bWnd, @bRedrawRect, False);
      FillChar(bRedrawRect, Sizeof(bRedrawRect), 0);
    end;

    if (bNeedVScroll <> 0) or (bNeedHScroll <> 0) then begin
      {Scroll the client area}
      with Rect do begin
        GetClientRect(bWnd, Rect);
        if not bScrollBack then begin
          Bottom := bCharHeight*cMarginBottom;
          Top := bCharHeight*(cMarginTop-1);                       
        end;
      end;
      ScrollWindow(bWnd, bNeedHScroll, bNeedVScroll, @Rect, nil);

      {Invalidate horizontal non-integral portion of window}
      if bNeedVScroll < 0 then begin
        GetClientRect(bWnd, Rect);
        if not bScrollBack then
          Rect.Bottom := bCharHeight*cMarginBottom;
        Rect.Top := (Rect.Bottom + bNeedVScroll) - bCharHeight;
        InvalidateRect(bWnd, @Rect, True);
      end;

      {Update scroll thumbs}
      if bNeedVScroll <> 0 then
        bUpdateScrollThumb(True);
      if bNeedHScroll <> 0 then
        bUpdateScrollThumb(False);

      {Reset scroll indicators}
      bNeedVScroll := 0;
      bNeedHScroll := 0;

      {Force an update}
      UpdateWindow(bWnd);
    end;

    {User status}
    bPostStatusMsg;
  end;

  procedure TBuffer.bUpdateScrollThumb(Vert : Bool);
    {-Update scroll thumb}
  var
    Current : Word;
    Check : Integer;
  begin
    if Vert then begin
      {Update vertical scroll bar}
      if bScrollBack then
        Current := bYPos+1
      else begin
        Check := bMaxY-bYPos;
        if bPageHeight >= Check then
          Current := bPageHeight - Check
        else
          Current := Check;
      end;
      SetScrollPos(bWnd, sb_Vert, Current-1, True);
    end else begin
      {Update horizontal scrollbar}
      SetScrollPos(bWnd, sb_Horz, bXPos, True);
    end;
  end;

  procedure TBuffer.bScroll(X, Y : Integer);
    {-Handle scrollback and scroll PageHeight within window}
  begin
    if bScrollBack then begin
      {ScrollBack mode}
      if (Y < 0) and (bYPos > 0) then begin
        {Scroll down}
        Y := Abs(Y);
        if Y > bYPos then
          Y := bYPos;
        Dec(bYPos, Y);
        Inc(bNeedVScroll, bCharHeight*Y);
      end else if (Y > 0) and ((bYPos+cHeight) <= bMaxY) then begin
        {Scroll up}
        if (bYPos+Y) > (bMaxY-cHeight+1) then
          Y := (bMaxY-cHeight)-bYPos+1;
        Inc(bYPos, Y);
        Inc(bNeedVScroll, -bCharHeight*Y);
      end;
    end else if (cHeight < bPageHeight) then begin
      {Handle scrolling bPageHeight lines within smaller window}
      if (Y < 0) and (bMaxY-bYPos+1 < bPageHeight) then begin
        {Scroll down}
        Y := Abs(Y);
        if (Y = bPageHeight) then
          {PgUp request, just goto top}
          Y := bYPos-(bMaxY-(bPageHeight-1));
        if (Y > bYPos) then
          Y := bYPos;
        Dec(bYPos, Y);
        Inc(bNeedVScroll, bCharHeight*Y);
      end else if (Y > 0) and (bYPos+cHeight <= bMaxY) then begin
        {Scroll up}
        if (Y= bPageHeight) then
          {PgDn request, just goto bottom}
          Y := (bMaxY-cHeight)-bYPos+1;
        Inc(bYPos, Y);
        Inc(bNeedVScroll, -bCharHeight*Y);
      end;
    end;

    {Horizontal scrolling is same in either mode}
    if (X < 0) and (bXPos <> 0) then begin
      {Scroll left}
      X := Abs(X);
      if X > bXPos then
        X := bXPos;
      Inc(bNeedHScroll, bCharWidth*X);
      Dec(bXPos, X);
    end else if (X > 0) and (bXPos+cWidth < bWidth) then begin
      {Scroll right}
      if X+bXPos+cWidth > bWidth then
        X := bWidth-(bXPos+cWidth);
      Dec(bNeedHScroll, bCharWidth*X);
      Inc(bXPos, X);
    end;

    {Update scroll thumbs}
    bUpdateScrollThumb(True);
    bUpdateScrollThumb(False);

    {Force a paint message}
    bForcePaint;
  end;

  procedure TBuffer.bGotoTop(Vert : Boolean);
    {-Handle "goto top" requests}
  var
    Y : Integer;
  begin
    if Vert then begin
      {Vertical "top" request}
      if bScrollBack then begin
        {ScrollBack mode}
        bYPos := 0;
        InvalidateRect(bWnd, nil, False);
      end else begin
        {Goto top of window}
        Y := bYPos-(bMaxY-(bPageHeight-1));
        if (Y > bYPos) then
          Y := bYPos;
        Dec(bYPos, Y);
        Inc(bNeedVScroll, bCharHeight*Y);
      end;
    end else begin
      {Horizontal "top" request}
      bXPos := 0;
      InvalidateRect(bWnd, nil, False);
    end;

    {Update scroll thumbs}
    bUpdateScrollThumb(True);
    bUpdateScrollThumb(False);

    {User status}
    bPostStatusMsg;
  end;

  procedure TBuffer.bGotoBottom(Vert : Boolean);
    {-Handle "goto bottom" requests}
  var
    Y : Integer;
  begin
    if Vert then begin
      {Vertical "bottom" request}
      if bScrollBack then begin
        {ScrollBack mode}
        if bMaxY > cHeight then
          bYPos := bMaxY-cHeight
        else
          bYPos := 1;
        InvalidateRect(bWnd, nil, False);
      end else begin
        {Goto bottom of window}
        Y := (bMaxY-cHeight)-bYPos+1;
        Inc(bYPos, Y);
        Inc(bNeedVScroll, -bCharHeight*Y);
      end;
    end else begin
      {Horizontal "bottom" request}
      bXPos := bWidth-cWidth;
      InvalidateRect(bWnd, nil, False);
    end;

    {Update scroll thumbs}
    bUpdateScrollThumb(True);
    bUpdateScrollThumb(False);

    {User status}
    bPostStatusMsg;
  end;

  function TBuffer.bConvertToRow(I : Integer) : Word;
    {-Return the row for this pixel value}
  var
    Row : Word;
  begin
    if I < 0 then
      I := 0;
    if bCharHeight <> 0 then begin
      Row := I div bCharHeight;
      if (I mod bCharHeight = 0) and (I <> 0) then
        Dec(Row);
      Inc(Row, bYPos);
    end else
      Row := 0;
    bConvertToRow := Row;
  end;

  function TBuffer.bConvertToCol(I : Integer) : Word;
    {-Return the col for this pixel value}
  var
    Col : Word;
  begin
    if I < 0 then
      I := 0;
    if bCharWidth <> 0 then begin
      Col := I div bCharWidth;
      if (I mod bCharHeight = 0) and (I <> 0) then
        Dec(Col);
      Inc(Col, bXPos);
    end else
      Col := 0;
    bConvertToCol := Col;
  end;

  procedure TBuffer.bResetMarking;
    {-Get rid of current marking}
  begin
    bMarking := False;
    bMarked := False;
    bOffScreen := False;
    bMarkAnchorX := 0;
    bMarkAnchorY := 0;
    bMarkStartX := 0;
    bMarkStartY := 0;
    bMarkEndX := 0;
    bMarkEndY := 0;
    bScrollTimer := 0;
  end;

  procedure TBuffer.bFixMarking(NewX, NewY : Word);
    {-Set new mark limit}
  begin
    if (NewY > bMarkAnchorY) or
       ((NewY = bMarkAnchorY) and (NewX > bMarkAnchorX)) then begin
      {Note new end}
      bMarkEndX := NewX;
      bMarkEndY := NewY;
      bMarkStartX := bMarkAnchorX;
      bMarkStartY := bMarkAnchorY;
    end else if (NewY < bMarkAnchorY) or
       ((NewY = bMarkAnchorY) and (NewX < bMarkAnchorX)) then begin
      {Note new start}
      bMarkStartX := NewX;
      bMarkStartY := NewY;
      bMarkEndX := bMarkAnchorX;
      bMarkEndY := bMarkAnchorY;
    end;
  end;

  procedure TBuffer.bUpdateMarks(RawX, RawY : Integer);
    {-Update new mouse position}
  var
    NewX, NewY : Word;
  begin
    if bMarking then begin
      {Convert X to columns}
      if RawX < 0 then
        RawX := 0
      else if RawX > cSizeX then
        RawX := cSizeX;
      NewX := bConvertToCol(RawX);

      {Convert Y to columns}
      if RawY < 0 then
        RawY := 0
      else if RawY > cSizeY then
        RawY := cSizeY;
      NewY := bConvertToRow(RawY);

      {Adjust block}
      bFixMarking(NewX, NewY);

      {Don't know what old marks were so repaint entire screen}
      InvalidateRect(bWnd, nil, False);
    end;
  end;

{TTerminal}

  constructor TTerminal.Create(AWnd : TApdHwnd);
    {-Construct TTerminal window}
  begin
    inherited Create;
    {Init the buffer}
    aBuffer := TBuffer.Create(AWnd, DefRows, DefCols);

    {Init some fields}
    tCom := nil;
    tScrollBack := False;
    aBuffer.bSetScrollMode(False);
    tDataReady := False;
    tWnd := AWnd;
    tFocused := False;
    aBuffer.bFocused := tFocused;
    tMinimized := False;
    tDefWndProc := @DefWindowProc;
    tHalfDuplex := False;

    {Start off with a null keyboard emulator}
    tKeyEmuPtr := nil;
    @tKeyEmuProc := nil;

    SetTimer(tWnd, tmBlinkTimer, DefBlinkTime, nil);
  end;

  destructor TTerminal.Destroy;
    {-Deallocate the Buffer}
  begin
    KillTimer(tWnd, tmBlinkTimer);
    if tCom <> nil then
      tCom.DeregisterWndTriggerHandler(tWnd);
    aBuffer.Free;
    aBuffer := nil;
  end;

  procedure TTerminal.tBlink;
  var
    FirstCharPos : Word;
    ScrPos : Integer;
    Changed : Boolean;
    BlinkAttr : Byte;
    BlinkRect : TRect;
    BlinkX, BlinkY : Word;
  begin
    {setup initial values}
    Changed := False;
    FillChar(BlinkRect, SizeOf(BlinkRect), 0);

    {search the entire terminal window for blinking attributes}
    with aBuffer do begin
      FirstCharPos := bYPos*bWidth;
      for ScrPos := FirstCharPos to (FirstCharPos+(cHeight*bWidth)-1)
                    do begin

        if ByteFlagIsSet(bExtAttrBuffer^[ScrPos], eattrBlink) then begin

          {calculate a TRect structure of the changed areas }
          BlinkX := (ScrPos - FirstCharPos);

          if not Changed then begin
            BlinkRect.Top  := (BlinkX div bWidth);
            BlinkRect.Left := (BlinkX - (bWidth*BlinkRect.Bottom));
          end;

          BlinkRect.Bottom := (BlinkX div bWidth);
          BlinkY := (BlinkX - (bWidth * BlinkRect.Bottom));

          if BlinkY < BlinkRect.Left then
            BlinkRect.Left := BlinkY;

          if BlinkY > BlinkRect.Right then
            BlinkRect.Right := BlinkY;




          {if the blinking restore flag is set then restore to origianl colors}
          if bBlinkReset then
            bAttrBuffer^[ScrPos] := bAttrBufferB^[ScrPos]

          {if not restoring then change the foreground color to the background}
          else begin
            BlinkAttr := (bAttrBuffer^[ScrPos] shr 4);
            bAttrBuffer^[ScrPos] := ((BlinkAttr) shl 4) or (BlinkAttr);
          end;
          Changed := True;
        end;
      end;
      bBlinkReset := not bBlinkReset;
    end;
    if Changed then begin
      {change the values in the TRect structure to account for height/width}
      with BlinkRect, aBuffer do begin
        Top    := bCharHeight *  Top;
        Bottom := bCharHeight * (Bottom+1);
        Left   := bCharWidth  *  Left;
        Right  := bCharWidth  * (Right+1);
      end;
      InvalidateRect(tWnd, @BlinkRect, False);
    end;
  end;

  procedure TTerminal.tSetInitialSizes;
  var
    Rect : TRect;
  begin
    {Note the initial client size}
    GetClientRect(tWnd, Rect);
    aBuffer.cSizeY := Rect.Bottom;
    aBuffer.cSizeX := Rect.Right;

    {Set the default font and note its height}
    wmSetFont(0);

    with aBuffer do begin
      {Conditionally add vertical scrollbar}
      if tAutoVScroll then
        tCheckVScroll
      else
        tHasVScroll := False;

      {Conditionally add horizontal scrollbar}
      if tAutoHScroll then
        tCheckHScroll
      else
        tHasHScroll := False;
    end;

    {Create and display caret}
  end;

  function TTerminal.tGetWindowStyle : LongInt;
    {-Return the style window long}
  begin
    tGetWindowStyle := GetWindowLong(tWnd, gwl_Style);
  end;

  function TTerminal.tWantTab : Bool;
    {-Return True if the tws_WantTab style is set}
  begin
    tWantTab := tGetWindowStyle and tws_WantTab <> 0;
  end;

  function TTerminal.tIntHeight : Bool;
    {-Return True if the tws_IntHeight style is set}
  begin
    tIntHeight := tGetWindowStyle and tws_IntHeight <> 0;
  end;

  function TTerminal.tIntWidth : Bool;
    {-Return True if the tws_IntWidth style is set}
  begin
    tIntWidth := tGetWindowStyle and tws_IntWidth <> 0;
  end;

  function TTerminal.tAutoVScroll : Bool;
    {-Return True if auto vertical scrolling is on}
  begin
    tAutoVScroll := tGetWindowStyle and tws_AutoVScroll <> 0;
  end;

  function TTerminal.tAutoHScroll : Bool;
    {-Return True if auto horizontal scrolling is on}
  begin
    tAutoHScroll := tGetWindowStyle and tws_AutoHScroll <> 0;
  end;

  procedure TTerminal.tMakeCaret;
    {-Create and position a caret}
  begin
    {Create a caret}
    CreateCaret(tWnd, 0, tCharWidth, tCharHeight);

    {Set initial caret position}
    aBuffer.bMoveCaret;

    {Show the caret}
    ShowCaret(tWnd);
    InvalidateRect(tWnd, nil, False);
  end;

  procedure TTerminal.tCalcClientRowCol;
    {-Calculate client rows/cols rect from pixel rect}
  begin
    with aBuffer do begin
      if bCharHeight <> 0 then
        cHeight := cSizeY div bCharHeight
      else
        cHeight := 0;
      if bCharWidth <> 0 then
        cWidth := cSizeX div bCharWidth
      else
        cWidth := 0;
    end;
  end;

  function TTerminal.tFullWidth : Bool;
    {-Return True if window is full width}
  begin
    with aBuffer do begin
      if tHasVScroll then
        tFullWidth := (cSizeX + GetSystemMetrics(sm_CXVScroll)) >=
                      (bCharWidth * bWidth)
      else
        tFullWidth := cSizeX >= (bCharWidth * bWidth);
    end;
  end;

  function TTerminal.tFullHeight : Bool;
    {-Return True if window is full height}
  begin
    with aBuffer do begin
      if tHasHScroll then
        tFullHeight := (cSizeY + GetSystemMetrics(sm_CYHScroll)) >=
                       (bCharHeight * bPageHeight)
      else
        tFullHeight := cSizeY >= (bCharHeight * bPageHeight);
    end;
  end;

  procedure TTerminal.tAdjustIntSize;
    {-Adjust window for integral width/height}
  var
    XDiff : Word;
    YDiff : Word;
    Rect : TRect;
  begin
    with aBuffer do begin
      if tIntHeight then
        YDiff := cSizeY mod bCharHeight
      else
        YDiff := 0;
      if tIntWidth then
        XDiff := cSizeX mod bCharWidth
      else
        XDiff := 0;

      if (XDiff <> 0) or (YDiff <> 0) then begin
        GetWindowRect(tWnd, Rect);
        with Rect do
          SetWindowPos(tWnd, 0,
                       0, 0, (Right-Left)-XDiff, (Bottom-Top)-YDiff,
                       swp_NoMove or swp_NoZOrder);
      end;
    end;
  end;

  procedure TTerminal.tAdjustMaxSize;
    {-Adjust window down to maximum size}
  var
    XDiff : Word;
    YDiff : Word;
    MaxSizeY : Word;
    MaxSizeX : Word;
    Rect : TRect;
  begin
    with aBuffer do begin
      MaxSizeY := (bCharHeight * bPageHeight);
      MaxSizeX := (bCharWidth * bWidth);
      if cSizeY > MaxSizeY then
        YDiff := cSizeY - MaxSizeY
      else
        YDiff := 0;
      if cSizeX > MaxSizeX then
        XDiff := cSizeX - MaxSizeX
      else
        XDiff := 0;

      if (XDiff <> 0) or (YDiff <> 0) then begin
        GetWindowRect(tWnd, Rect);
        with Rect do
          SetWindowPos(tWnd, 0,
                       0, 0, (Right-Left)-XDiff, (Bottom-Top)-YDiff,
                       swp_NoMove or swp_NoZOrder);
      end;
    end;
  end;

  procedure TTerminal.tAdjustClient;
    {-Client size may have changed, slide contents around as necessary}
  var
    ActualY : Word;
  begin
    with aBuffer do begin
      {See if height got smaller, slide client up if so}
      if cLastHeight > cHeight then begin
        ActualY := bYPos + bY;
        Inc(bYPos, cLastHeight-cHeight);
        if bYPos > ActualY then
          bYPos := ActualY;
        InvalidateRect(bWnd, nil, False);
      end;

      {See if height got bigger, slide client down if so}
      if cLastHeight < cHeight then begin
        Dec(bYPos, cHeight-cLastHeight);
        if bYPos < 0 then
          bYPos := 0;
        InvalidateRect(bWnd, nil, False);
      end;

      {See if width got bigger, slide client to left if so}
      if cLastWidth < cWidth then begin
        Dec(bXPos, cWidth-cLastWidth);
        if bXPos < 0 then
          bXPos := 0;
        InvalidateRect(bWnd, nil, False);
      end;

      {Note height for next resizing}
      cLastHeight := cHeight;
    end;
  end;

  procedure TTerminal.tCheckVScroll;
    {-Set or remove vertical scrollbar}
  var
    Min, Max : Word;
  begin
    with aBuffer do begin
      tHasVScroll := True;
      if tScrollBack then begin
        {Scrollback mode, set scroll range for contents of buffer}
        Min := 0;
        if bMaxY >= cHeight then
          Max := (bMaxY-cHeight)+1
        else
          Max := bMaxY;
      end else if (cHeight < bPageHeight) then begin
        {Not scrollback but window smaller then page height}
        Min := 0;
        Max := bPageHeight-cHeight;
      end else begin
        {No scrolling}
        Min := 0;
        Max := 0;
        tHasVScroll := False;
      end;

      {Set new scroll range}
      SetScrollRange(bWnd, sb_Vert, Min, Max, True);
      if Max <> 0 then
        {Scrollbar is present, set scroll thumb}
        aBuffer.bUpdateScrollThumb(True);
    end;
  end;

  procedure TTerminal.tCheckHScroll;
    {-Set or remove horizontal scrollbar}
  var
    Min, Max : Word;
  begin
   with aBuffer do begin
      if (cWidth < bWidth) then begin
        {Window width less than page width, use horizontal scrollbar}
        Min := 0;
        Max := bWidth-cWidth;
        tHasHScroll := True;
      end else begin
        {No scrolling}
        Min := 0;
        Max := 0;
        tHasHScroll := False;
      end;

      {Set new scroll range}
      SetScrollRange(bWnd, sb_Horz, Min, Max, True);
      if Max <> 0 then
        aBuffer.bUpdateScrollThumb(False);
    end;
  end;

  procedure TTerminal.tResizeClientRect;
    {-Reduce window size if client is too big}
  var
    XDiff : Integer;
    YDiff : Integer;
    Rect : TRect;
    ForceHeight : Bool;
    ForceWidth : Bool;
  begin
    with aBuffer do begin
      {Check for height adjustment}
      ForceHeight := True;
      if cHeight > bPageHeight then
        cHeight := bPageHeight
      else if cHeight < 1 then
        cHeight := 1
      else
        ForceHeight := False;
      if tIntHeight or ForceHeight then
        YDiff := -(cSizeY - (cHeight * bCharHeight))
      else
        YDiff := 0;

      {Check for width adjustment}
      ForceWidth := True;
      if cWidth > bWidth then
        cWidth := bWidth
      else if cWidth = 0 then
        cWidth := 1
      else
        ForceWidth := False;
      if tIntWidth or ForceWidth then
        XDiff := -(cSizeX - (cWidth * bCharWidth))
      else
        XDiff := 0;

      {Adjust window}
      if (YDiff <> 0) or (XDiff <> 0) then begin
        GetWindowRect(tWnd, Rect);
        with Rect do
          SetWindowPos(tWnd, 0,
                        0, 0,
                        Right-Left+XDiff, Bottom-Top+YDiff,
                        swp_NoMove + swp_NoZOrder);
      end;
    end;
  end;

  procedure TTerminal.tGetMousePos(var X, Y : Integer);
    {-Get window-relative mouse cursor position}
  var
    P : TPoint;
  begin
    GetCursorPos(P);
    ScreenToClient(tWnd, P);
    X := P.X;
    Y := P.Y;
  end;

  procedure TTerminal.tStartTerminal(var Msg : wMsg);
    {-Start dispatcher and set length trigger}
  begin
    if (tCom <> nil) and not TriggerHandlerInstalled then begin
      tCom.RegisterWndTriggerHandler(tWnd);
      TriggerHandlerInstalled := True;                               
      tCom.ChangeLengthTrigger(1);
    end;
  end;

  procedure TTerminal.tStopTerminal(var Msg : wMsg);
    {-Stop dispatcher}
  begin
    if (tCom <> nil) and TriggerHandlerInstalled then begin          
      tCom.DeregisterWndTriggerHandler(tWnd);
      TriggerHandlerInstalled := False;                               
    end;
  end;

  procedure TTerminal.tSetEmulatorPtr(var Msg : wMsg);
    {-Set emulator}
  begin
    aBuffer.bEmulator := Pointer(Msg.lParam);
  end;

  procedure TTerminal.tSetEmulatorProc(var Msg : wMsg);
    {-Set emulator callback}
  begin
    aBuffer.bEmuProc := TProcessCharProcLo(Msg.lParam);          
  end;


  procedure TTerminal.tSetKeyEmulatorPtr(var Msg : wMsg);
    {-Set keyboard emulator}
  begin
    tKeyEmuPtr := Pointer(Msg.lParam);
  end;

  procedure TTerminal.tSetKeyEmulatorProc(var Msg : wMsg);
    {-Set keyboard emulator callback}
  begin
    tKeyEmuProc := TProcessKeyProcLo(Msg.lParam);
  end;

  procedure TTerminal.tSetComHandle(var Msg : wMsg);
    {-Set a new com handle}
  begin
    if (Msg.wParam < PortList.Count) and (PortList[Msg.wParam] <> nil) then
      tCom := TApdBaseDispatcher(PortList[Msg.wParam])
    else
      tCom := nil;                                                  
    aBuffer.bCom := tCom;
  end;

  procedure TTerminal.tReleaseComHandle(var Msg : wMsg);
    {-Stop the terminal window because the com handle is being released}
  begin
    tStopTerminal(Msg);                                            
    tCom := nil;
  end;

  procedure TTerminal.tHandleData(var Msg : wMsg);
    {-Data is available, retrieve and send to terminal buffer}
  var
    I : Word;
    C : Char;
  begin
    if tScrollBack then
      {Don't process data now}
      tDataReady := True
    else begin
      for I := 1 to Msg.wParam do
        if tCom.CharReady then begin
          tCom.GetChar(C);
          aBuffer.bProcessChar(C);
        end;

      {Update the screen}
      aBuffer.bForcePaint;
    end;
  end;

  procedure TTerminal.tTextOutColor(DC : HDC; X, Y : Word;
                                    Line : PChar; Attr : PChar;
                                    Len : Word; MarkStart : Word;
                                    MarkEnd : Word);
  var
    I          : Word;
    NewLen     : Word;
    NewX       : Word;
    ColorRef   : TColorRef;
    Changed    : Boolean;
    Marked     : Boolean;
    LastAttr   : Char;
  begin
    {Handle completely marked line separately}
    if (MarkStart = 0) and (MarkEnd = Len) then begin
      ColorRef := aBuffer.bColors[aBuffer.bMarkColorB];
      SetBkColor(DC, ColorRef);
      ColorRef := aBuffer.bColors[aBuffer.bMarkColorF];
      SetTextColor(DC, ColorRef);
      ExtTextOut(DC, (X*aBuffer.bCharWidth),
                     (Y*aBuffer.bCharHeight),
                     0, nil, Line, Len, nil);
      Exit;
    end;

    {Inits}
    NewX := X;
    NewLen := 0;
    LastAttr := Attr[0];

    {Loop through line, writing at each color change}
    for I := 0 to Len-1 do begin
      {Color change?}
      Marked := False;
      if (MarkEnd <> 0) then begin
        if (I = MarkEnd) and (I <> MarkStart) then begin
          {At end of marked segment or trailing part of line is marked}
          Changed := True;
          Marked := True;
          if I = Len-1 then
            Inc(NewLen);
        end else if (I = MarkStart) and (I <> 0) then
          {Entering marked segment, write out current segment}
          Changed := True
        else if ((I > MarkStart) and (I <= MarkEnd)) then
          {In marked segment}
          Changed := False
        else
          {Not in marked segment, check for attribute color change}
          Changed := (Attr[I] <> LastAttr);
      end else
        {Marking not active, check for attribute color change}
        Changed := (Attr[I] <> LastAttr);

      {If no color change then just increment length}
      if not Changed then
        Inc(NewLen);

      {Write line if changed or done}
      if Changed or (I = Len-1) then with aBuffer do begin
        if Marked then begin
          ColorRef := bColors[bMarkColorB];
          SetBkColor(DC, ColorRef);
          ColorRef := bColors[bMarkColorF];
          SetTextColor(DC, ColorRef);
        end else begin
          ColorRef := bColors[Byte(LastAttr) shr 4];
          SetBkColor(DC, ColorRef);
          ColorRef := bColors[Byte(LastAttr) and $0F];
          SetTextColor(DC, ColorRef);
        end;

        {Write out the previous data in the previous colors}
        ExtTextOut(DC, (NewX*bCharWidth), (Y*bCharHeight),
                   0, nil, Line, NewLen, nil);

        {Start checking from the new current position}
        Inc(Line, NewLen);
        LastAttr := Attr[I];
        Inc(NewX, NewLen);
        NewLen := 1;

        {See if a color change occurred in the last character. If so, }
        {we need a special write for this last character.}
        if (I = Len-1) and Changed then begin
          ColorRef := bColors[Byte(LastAttr) shr 4];
          SetBkColor(DC, ColorRef);
          ColorRef := bColors[Byte(LastAttr) and $0F];
          SetTextColor(DC, ColorRef);
          ExtTextOut(DC, (NewX*bCharWidth), (Y*bCharHeight),
                     0, nil, Line, 1, nil);
        end;
      end;
    end;
  end;

  procedure TTerminal.tPaint(PaintDC: HDC; var PaintInfo: TPaintStruct);
    {-Update the invalidated portions of the client area}
  var
    X, Y, sY  : Word;
    Len       : Word;
    Index     : Word;
    MarkStart : Word;
    MarkEnd   : Word;
    CharRect  : TRect;
    Temp      : PScreenBuffer;
    TempA     : PAttrBuffer;
    ColorRef  : TColorRef;
    C         : Char;
  begin
    with aBuffer do begin

      {Convert to character coordinates}
      with PaintInfo.RcPaint do begin

        {Don't paint if rectangle is empty}
        if Bottom - Top = 0 then
          Exit;

        if bCharWidth <> 0 then begin
          CharRect.Left   := Left div bCharWidth;
          CharRect.Right  := Right div bCharWidth;
        end else begin
          CharRect.Left := 0;
          CharRect.Right := 0;
        end;
        if bCharHeight <> 0 then begin
          CharRect.Top    := Top div bCharHeight;
          CharRect.Bottom := Bottom div bCharHeight;
        end else begin
          CharRect.Top := 0;
          CharRect.Bottom := 0;
        end;

        {Don't paint past the maximum column}
        if CharRect.Right > cWidth then
          CharRect.Right := cWidth;
      end;

      {Select the font}
      if tFont <> 0 then
        SelectObject(PaintDC, tFont);

      {Set the background mode}
      SetBkMode(PaintDC, Opaque);

      {Draw the characters in the invalid rectangle}
      with CharRect do begin
        if (Top = Bottom) and (Left = Right) then begin
          {Special case for updating just one char}
          X := Left+bXPos;
          Y := Top+bYPos;
          Index := Y*bWidth+X;
          C := bScreenBuffer^[Index];
          ColorRef := bColors[(bAttrBuffer^[Index] shr 4)];
          SetBkColor(PaintDC, ColorRef);
          ColorRef := bColors[(bAttrBuffer^[Index] and $0F)];
          SetTextColor(PaintDC, ColorRef);
          ExtTextOut(PaintDC, (bCharWidth*Left), (bCharHeight*Top),
                     0, nil, @C, 1, nil);
        end else begin
          {Loop thru lines}
          for Y := Top to Bottom do begin
            {Get next line from buffer}
            sY := Y+bYPos;
            if sY < bHeight then begin
              Index := (sY*bWidth)+Left+bXPos;
              Len := (Right-Left)+1;

              {Don't read into the next line, which could happen with
               non-integral widths and horizontal scrolling}
              if Left+bXPos+Len >= bWidth then
                Len := bWidth-(Left+bXPos);

              {Calculate marking columns}
              if (bMarking or bMarked) and
                 (sY >= bMarkStartY) and
                 (sY <= bMarkEndY) then begin
                {This line needs to be marked, set start/stop columns}
                if (sY = bMarkStartY) then
                  MarkStart := bMarkStartX
                else
                  MarkStart := 0;
                if (sY = bMarkEndY) then
                  MarkEnd := bMarkEndX
                else
                  MarkEnd := Len-1
              end else begin
                {No marking for this line}
                MarkStart := 0;
                MarkEnd := 0;
              end;

              {Output this line}
              Temp := AddWordToPtr(bScreenBuffer, Index);
              TempA := AddWordToPtr(bAttrBuffer, Index);
              tTextOutColor(PaintDC, Left, Y, PChar(Temp), PChar(TempA),
                            Len, MarkStart, MarkEnd);
            end;
          end;
        end;
      end;

      {Position caret}
      if tFocused then
        aBuffer.bMoveCaret;
    end;
  end;

  procedure TTerminal.tClearWindow(var Msg : wMsg);
    {-Clear the buffer and redraw}
  begin
    if Msg.wParam = 0 then
      aBuffer.bClearScreen
    else if Msg.wParam = 1 then
      tClearBuffer(Msg);
  end;

  procedure TTerminal.tClearBuffer(var Msg : wMsg);
    {-Clear the buffer and redraw}
  var
    ColorAttr : Byte;
  begin
    with aBuffer do begin
      bX := 0;
      bY := 0;
      bXPos := 0;
      bYPos := 0;
      bMaxY := bPageHeight-1;
      FillChar(bScreenBuffer^, bBufferSize, ' ');
      ColorAttr := (bbColorOrg shl 4) or bfColorOrg;
      FillChar(bAttrBuffer^, bBufferSize, ColorAttr);
      FillChar(bAttrBufferB^, bBufferSize, ColorAttr);
      FillChar(bExtAttrBuffer^, bBufferSize, 0);
      bfColor := bfColorOrg;
      bbColor := bbColorOrg;
      InvalidateRect(tWnd, nil, False);
      bPostStatusMsg;
    end;
  end;

  procedure TTerminal.tToggleScrollback(var Msg : wMsg);
    {-Toggle scrollback mode}
  var
    Rect : TRect;
    SameSize : Boolean;
  begin
    with aBuffer do begin
      tScrollBack := not tScrollBack;

      {Notify tBuffer of scroll mode change}
      bSetScrollMode(tScrollback);

      if not tScrollBack then begin
        SameSize := (cCheckX = cSizeX) and (cCheckY = cSizeY);

        {Conditionally remove vertical scrollbar}
        if HadVScroll then begin
          SetScrollRange(tWnd,sb_Vert,vMin,vMax,true);
          aBuffer.bUpdateScrollThumb(True);
        end else                                                  
          if tAutoVScroll then
            if bWasFull then
              SetScrollRange(tWnd, sb_Vert, 0, 0, True)
            else
              tCheckVScroll
          else begin
            tHasVScroll := False;
            SetScrollRange(tWnd, sb_Vert, 0, 0, True);
          end;

        {Conditionally remove horizontal scrollbar}
        if tAutoHScroll then
          if bWasFull then
            SetScrollRange(tWnd, sb_Horz, 0, 0, True)
          else
            tCheckHScroll;

        if SameSize then begin
          {Window size didn't change while in scrollback, restore to}
          {exact pre-scrollback size                                }
          GetWindowRect(tWnd, Rect);
          with Rect do
            SetWindowPos(tWnd, 0,
                         0, 0,
                         (Right-Left)+(Integer(cSaveX)-cSizeX),
                         (Bottom-Top)+(Integer(cSaveY)-cSizeY),
                         swp_NoMove or swp_NoZOrder);

        end;

        {Reset previous client coords}
        bX := bSaveX;
        bY := bSaveY;
        bXPos := bSaveXPos;
        bYPos := bSaveYPos;

        {Redraw}
        InvalidateRect(bWnd, nil, True);
        if tDataReady then begin
          tDataReady := False;
          PostMessage(bWnd, apw_TriggerAvail, DispatchBufferSize, 0); 
        end;

      end else begin
        {Entering scrollback mode, save client info}
        bSaveX := bX;
        bSaveY := bY;
        bSaveXPos := bXPos;
        bSaveYPos := bYPos;
        bWasFull := tFullHeight and tFullWidth;
        cSaveX := cSizeX;
        cSaveY := cSizeY;

        HadVScroll := (GetWindowLong(tWnd,GWL_STYLE) and ws_VScroll) <> 0;
        if HadVScroll then                                                 
          GetScrollRange(tWnd,SB_VERT,vMin,vMax);                         

        {Always use vertical scrollbar}
        tCheckVScroll;

        {Use horizontal scrollbar conditionally}
        if tAutoHScroll then
          tCheckHScroll;

        {Note size of window with new scrollbars}
        cCheckX := cSizeX;
        cCheckY := cSizeY;
      end;

      {And update status}
      bPostStatusMsg;
    end;
  end;

  procedure TTerminal.tStuffData(var Msg : wMsg);
  var
    I : Word;
    P : PChar;
  begin
    with Msg do begin
      if wParam <> 0 then begin
        P := PChar(lParam);
        for I := 0 to Msg.wParam-1 do
          aBuffer.bProcessChar(P[I]);
      end;
    end;
  end;

  procedure TTerminal.tForcePaint(var Msg : wMsg);
  begin
    aBuffer.bForcePaint;
  end;

  procedure TTerminal.tSetWndProc(wParam : Word; lParam : Longint);
  begin
    if Pointer(lParam) <> nil then
      tDefWndProc := TFarProc(lParam)
    else if wParam = 1 then
      tDefWndProc := @DefMDIChildProc
    else
      tDefWndProc := @DefWindowProc;
  end;

  function TTerminal.tSaveRestore(var Msg : wMsg) : LongInt;
    {-Save/restore the terminal window}
  var
    Save : PTermSave;
    RestSize : Cardinal;
    TermSize : Cardinal;

    procedure Cleanup;
    begin
      if Save <> nil then begin
        with Save^, aBuffer do begin
          FreeMem(ScreenBuffer, bBufferSize);
          FreeMem(AttrBuffer, bBufferSize);
          FreeMem(AttrBufferB, bBufferSize);
          FreeMem(ExtAttrBuffer, bBufferSize);
          FreeMem(RestBuffer, RestSize);
          FreeMem(TermBuffer, TermSize);
          FreeMem(HorizTabStop, SizeOf(THorizontalTabStop));
          FreeMem(VertiTabStop, SizeOf(TVerticalTabStop));
        end;
        FreeMem(Save, SizeOf(TTermSave));
      end;
    end;

  begin
    with Msg do begin
      {Calculate general data buffer sizes}
      with aBuffer do begin
        {$IFDEF Win32}
        RestSize := Cardinal(@bFinal) - Cardinal(@bHeight);
        TermSize := Cardinal(@tFinal) - Cardinal(@tFont);
        {$ELSE}
        RestSize := Ofs(bFinal)-Ofs(bHeight);
        TermSize := Ofs(tFinal)-Ofs(tFont);
        {$ENDIF}
      end;

      if wParam = 0 then begin
        {Saving - allocate main save structure}
        Save := AllocMem(SizeOf(TTermSave));

        {Allocate sub-buffers and copy}
        with aBuffer, Save^ do begin
          ScreenBuffer := AllocMem(bBuffersize);
          AttrBuffer := AllocMem(bBufferSize);
          RestBuffer := AllocMem(RestSize);
          AttrBufferB := AllocMem(bBufferSize);
          ExtAttrBuffer := AllocMem(bBufferSize);
          HorizTabStop := AllocMem(SizeOf(THorizontalTabStop));
          VertiTabstop := AllocMem(SizeOf(TVerticalTabStop));
          TermBuffer := AllocMem(TermSize);

          Move(bScreenBuffer^, ScreenBuffer^, bBufferSize);
          Move(bAttrBuffer^, AttrBuffer^, bBufferSize);
          Move(bHeight, RestBuffer^, RestSize);
          Move(tFont, TermBuffer^, TermSize);
          Move(bAttrBufferB^, AttrBufferB^, bBufferSize);
          Move(bExtAttrBuffer^, ExtAttrBuffer^, bBufferSize);
          Move(bHorizTabStop^, HorizTabStop^, SizeOf(THorizontalTabStop));
          Move(bVertiTabStop^, VertiTabStop^, SizeOf(TVerticalTabStop));
          {Capturing?}
          if bCapture then begin
            {Turn it off...}
            bSetCapture(False, False, '');
            {...but note that capture was on so restore will restart}
            bCapture := True;
          end;
        end;

        tSaveRestore := LongInt(Save);
      end else begin
        {Restoring...}
        Save := PTermSave(Msg.lParam);
        with aBuffer, Save^ do begin
          {Get rid of current buffers, switch to saved}
          FreeMem(bScreenBuffer, bBufferSize);
          FreeMem(bAttrBuffer, bBufferSize);
          FreeMem(bAttrBufferB, bBufferSize);
          FreeMem(bExtAttrBuffer, bBufferSize);
          FreeMem(bHorizTabStop, SizeOf(THorizontalTabStop));
          FreeMem(bVertiTabStop, SizeOf(TVerticalTabStop));
          bScreenBuffer := ScreenBuffer;
          bAttrBuffer := AttrBuffer;
          bAttrBufferB := AttrBufferB;
          bExtAttrBuffer := ExtAttrBuffer;
          bHorizTabStop := HorizTabStop;
          bVertiTabStop := VertiTabstop;

          {Switch rest of data}
          Move(RestBuffer^, bHeight, RestSize);
          bWnd := tWnd;
          Move(TermBuffer^, tFont, TermSize);

          {Capturing?}
          if bCapture then begin
            {Turn capture back on with append}
            bCapture := False;
            bSetCapture(True, True, bCaptureName);
          end;

          tSaveRestore := 0;
        end;
      end;
    end;
  end;

  function TTerminal.tGetSetColorMap(var Msg : wMsg) : LongInt;
    {-Get/set color map or get Save/restore the terminal window}
  begin
    with Msg, aBuffer do begin
      tGetSetColorMap := 0;
      case wParam of
        gscSetMap :
          Move(Pointer(lParam)^, bColors, SizeOf(bColors));
        gscGetMap :
          Move(bColors, Pointer(lParam)^, SizeOf(bColors));
        gscGetColors :
          tGetSetColorMap := (longint(bbColor) shl 16) or bfColor;
      end;
    end;
  end;

  procedure TTerminal.tForceSize(var Msg : wMsg);
    {-Force the window to the specified character row/cols}
  var
    Width, Height : Word;
    WX, WY : Word;
    WRect : TRect;
    CRect : TRect;
  begin
    with Msg, aBuffer do begin
      GetWindowRect(tWnd, WRect);
      GetClientRect(tWnd, CRect);
      WX := (WRect.Right - WRect.Left) - (CRect.Right - CRect.Left);
      WY := (WRect.Bottom - WRect.Top) - (CRect.Bottom - CRect.Top);
      Width := (bCharWidth * wParam) + WX;
      Height := (bCharHeight * Word(lParam)) + WY;
      SetWindowPos(tWnd, 0,
                   0, 0, Width, Height,
                   swp_NoMove or swp_NoZOrder);
      cMarginBottom := bPageHeight;
    end;
  end;

  function TTerminal.tGetFontSize(var Msg : wMsg) : Longint;
    {-Return the font size in pixels}
  type
    TFontData = record
      Height : Byte;
      Width  : Byte;
      JIC    : Word;
    end;
  var
    Res : TFontData;
  begin
    with Msg, aBuffer do begin
      Res.Height := bCharHeight;
      Res.Width  := bCharWidth;
    end;
    tGetFontSize := Longint(Res);
  end;

  procedure TTerminal.tBlinkTimeChange(var Msg : wMsg);
  begin
    KillTimer(tWnd, tmBlinkTimer);
    SetTimer(tWnd, tmBlinkTimer, Msg.wParam, nil);
  end;

  procedure TTerminal.tPersistentMarkChange(var Msg : wMsg);
  begin
    tPersistentMarking := Boolean(Msg.wParam);
  end;

  procedure TTerminal.tSetHalfDuplex(var Msg : wMsg);
  begin
    tHalfDuplex := Boolean(Msg.wParam);
  end;

  procedure TTerminal.tGetBuffPtr(var Msg : wMsg);
  begin
    with Msg do begin
      PTermBuff(lParam)^.Data  := aBuffer.bScreenBuffer;
      PTermBuff(lParam)^.Attr  := aBuffer.bAttrBuffer;
      PTermBuff(lParam)^.XAttr := aBuffer.bExtAttrBuffer;
    end;
  end;

  function TTerminal.tGetFirstClientLine(var Msg : wMsg): LongInt;
  begin
    with Msg, aBuffer  do
      tGetFirstClientLine := bYPos;
  end;

  procedure TTerminal.wmPaint(var Msg : wMsg);
    {-Send paint message to tPaint}
  var
    PS : TPaintStruct;
  begin
    BeginPaint(tWnd, PS);
    tPaint(PS.hDC, PS);
    EndPaint(tWnd, PS);
  end;

  procedure TTerminal.wmKeydown(var Msg : wMsg);
    {-Process keyboard scroll commands}
  var
    I : Integer;
  begin
    with aBuffer do begin
      if tScrollBack then begin
        {In scrollback mode, process scroll request}
        case Msg.wParamLo of
          vk_Up    :
            bScroll(0, -1);
          vk_Down  :
            bScroll(0, 1);
          vk_Left  :
            bScroll(-1, 0);
          vk_Right :
            bScroll(1, 0);
          vk_Prior :
            bScroll(0, -cHeight);
          vk_Next  :
            bScroll(0, cHeight);
          vk_Home  :
            bScroll(-bWidth, 0);
          vk_End   :
            bScroll(bCurLineLength-bX, 0);
        end;
      end else if (tKeyEmuPtr <> nil) and (@tKeyEmuProc <> nil) then begin
        GetKeyboardState(tKC.KeyboardState);
        tKeyEmuProc(tKeyEmuPtr, Msg.wParamLo, tKC);

        {handle keys that dont generate a wmChar message}
        if tCom <> nil then
          if tKC.Extended and tKC.KeyMapped then                     
            if tHalfDuplex then begin
              for I := 1 to Length(tKC.Value) do begin
                tCom.PutBlock(tKC.Value[I], 1);
                aBuffer.bProcessChar(tKC.Value[I]);

                {Update the screen}
                aBuffer.bForcePaint;
              end;
            end else
              tCom.PutBlock(tKC.Value[1], Length(tKC.Value));
      end;
    end;
    with Msg do
      DefWindowProc(tWnd, Message, wParam, lParam);
  end;

  procedure TTerminal.wmMouseActivate(var Msg : wMsg);
    {-Set focus on mouse activate}
  begin
    if GetFocus <> tWnd then
      SetFocus(tWnd);
  end;

  procedure TTerminal.wmSize(var Msg: wMsg);
    {-Add/remove scrollbars, handle integral sizing}
  const
    size_Restored = 0;
    size_Minimized = 1;
    size_Maximized = 2;
  begin
    with aBuffer do begin
      {Note size of new client rect}
      cSizeY := Msg.lParamHi;
      cSizeX := Msg.lParamLo;

      {Set minimized flag if we've been minimized}
      if Msg.wParam = size_Minimized then
        tMinimized := False;

      {Make sure window isn't too big}
      tAdjustMaxSize;

      {Ignore scrollbars if full size}
      if tFullWidth and tFullHeight and not tScrollback then begin
        cSizeX := bCharWidth * bWidth;
        cSizeY := bCharHeight * bPageHeight;
      end else if tIntHeight or tIntWidth then
        {Integral sizeing might be required, go do it}
        tAdjustIntSize;

      {Calculate height/width in character rows/columns}
      tCalcClientRowCol;

      {Add/remove scrollbars as required}
      if tAutoVScroll then
        tCheckVScroll;
      if tAutoHScroll then
        tCheckHScroll;

      {Adjust client if this a real restoration}
      if ((Msg.wParam = size_Restored) or (Msg.wParam = size_Maximized)) and
         not tMinimized then
        tAdjustClient;

      {Reset minimize flag}
      tMinimized :=  Msg.wParam = size_Minimized;
    end;
  end;

  procedure TTerminal.wmGetMinMaxInfo(var Msg: wMsg);
    {-Return max window size}
  var
    MinMax : PMinMaxInfo;
    MaxWidth : Word;
    MaxHeight : Word;
    Style : Longint;
    DC : HDC;
    PixelWidth : Integer;
    PixelHeight : Integer;
  begin
    MinMax := PMinMaxInfo(Msg.lParam);
    with MinMax^, aBuffer do begin
      MaxWidth := (bWidth * bCharWidth);
      MaxHeight := (bPageHeight * bCharHeight);

      {Get the window style}
      Style := tGetWindowStyle;

      {Height/width border adjustments}
      if Style and ws_ThickFrame <> 0 then begin
        Inc(MaxWidth, 2*GetSystemMetrics(sm_CXFrame));
        Inc(MaxHeight, 2*GetSystemMetrics(sm_CYFrame));
        if Style and ws_Caption <> 0 then
          Inc(MaxHeight, GetSystemMetrics(sm_CYCaption));
      end else if Style and ws_DlgFrame <> 0 then begin
        Inc(MaxWidth, 2*GetSystemMetrics(sm_CXDlgFrame));
        Inc(MaxHeight, 2*GetSystemMetrics(sm_CYDlgFrame));
        if Style and ws_Caption <> 0 then
          Inc(MaxHeight, GetSystemMetrics(sm_CYCaption))
      end else begin
        Inc(MaxWidth, 2*GetSystemMetrics(sm_CXFrame));
        Inc(MaxHeight, 2*GetSystemMetrics(sm_CYFrame));
      end;

      {Now make sure this still fits within the physical device}
      DC := GetDC(tWnd);
      PixelWidth := GetDeviceCaps(DC, HorzRes);
      PixelHeight := GetDeviceCaps(DC, VertRes);
      if MaxWidth > PixelWidth then
        MaxWidth := PixelWidth;
      if MaxHeight > PixelHeight then
        MaxHeight := PixelHeight;
      ReleaseDC(tWnd, DC);

      {Return the max sizes}
      ptMaxSize.X := MaxWidth;
      ptMaxSize.Y := MaxHeight;
      ptMaxTrackSize.X := MaxWidth;
      ptMaxTrackSize.Y := MaxHeight;
    end;
  end;

  function TTerminal.wmSetFont(Font : THandle) : LongInt;
    {-Validate the new font being requested}
  var
    OldFont : THandle;
    Metrics : TTextMetric;
    DC : hDC;
  begin
    {Check for default font}
    if Font = 0 then
      Font := GetStockObject(DefFont);

    DC := GetWindowDC(tWnd);
    OldFont := SelectObject(DC, Font);
    GetTextMetrics(DC, Metrics);

    with Metrics do begin
      {Don't select variable pitch fonts}
      if not Odd(tmPitchAndFamily) then begin
        {Note char height and width}
        tFont := Font;
        tCharHeight := tmHeight;
        tCharWidth := tmAveCharWidth;
        aBuffer.bUpdateFont(tCharHeight, tCharWidth);

        {Shrink window if necessary}
        tAdjustMaxSize;

        {See if we need scrollbars}
        if tAutoVScroll then
          tCheckVScroll;
        if tAutoHScroll then
          tCheckHScroll;

        {Check for integral sizing}
        tAdjustIntSize;

        {Adjust the client area}
        tAdjustClient;

        {Make a proper size caret}
        if tFocused then
          tMakeCaret;

        {Return the old font handle}
        wmSetFont := OldFont;
      end else begin
        SelectObject(DC, OldFont);

        {Return zero}
        wmSetFont := 0;
      end;
    end;
    ReleaseDC(tWnd, DC);
  end;

  procedure TTerminal.wmChar(var Msg : wMsg);
    {-Transmit the character}
  var
    I : Integer;
  begin
    if tCom <> nil then
      with aBuffer do begin
        if (tKeyEmuPtr <> nil) and (@tKeyEmuProc <> nil) and tKC.KeyMapped then begin
          if tHalfDuplex then begin
            for I := 1 to Length(tKC.Value) do begin
              tCom.PutBlock(tKC.Value[I], 1);
              aBuffer.bProcessChar(tKC.Value[I]);

              {Update the screen}
              aBuffer.bForcePaint;
            end;
          end else
            tCom.PutBlock(tKC.Value[1], Length(tKC.Value));
        end else begin
          tCom.PutBlock(Char(Msg.wParamLo), 1);
          if tHalfDuplex then begin
            aBuffer.bProcessChar(Char(Msg.wParamLo));

            {Update the screen}
            aBuffer.bForcePaint;
          end;
        end;
      end;
  end;

  procedure TTerminal.wmSetFocus(var Msg : wMsg);
    {-Create and position a caret, send the status message}
  begin
    tFocused := True;
    aBuffer.bFocused := tFocused;
    tMakeCaret;
    aBuffer.bPostStatusMsg;
    with Msg do
      CallWindowProc(tDefWndProc, hWindow, Message, wParam, lParam);
   end;

  procedure TTerminal.wmKillFocus(var Msg : wMsg);
    {-Don't need our caret anymore}
  begin
    tFocused := False;
    aBuffer.bFocused := tFocused;
    HideCaret(tWnd);
    DestroyCaret;
  end;

  function TTerminal.wmGetDlgCode(var Msg : wMsg) : LongInt;
    {-Decide how to handle kbd input}
  begin
    if not tWantTab then
      wmGetDlgCode := dlgc_WantAllKeys or
                      dlgc_WantArrows or
                      dlgc_WantChars
    else
      wmGetDlgCode := dlgc_WantAllKeys or
                      dlgc_WantArrows or
                      dlgc_WantChars or
                      dlgc_WantTab;
  end;

  procedure TTerminal.wmVScroll(var Msg : wMsg);
    {-Scroll vertically}
  var
    Y : Integer;
  begin
    with aBuffer, Msg do begin
      {$IFDEF Win32}
      case wParamLo of
      {$ELSE}
      case wParam of
      {$ENDIF}                                                        
        sb_Top :
          bGotoTop(True);
        sb_Bottom :
          bGotoBottom(True);
        sb_LineDown :
          bScroll(0, 1);
        sb_LineUp :
          bScroll(0, -1);
        sb_PageDown :
          bScroll(0, bPageHeight);
        sb_PageUp :
          bScroll(0, -bPageHeight);
        sb_ThumbPosition,
        sb_ThumbTrack :
          begin
            {$IFDEF Win32}
            if tScrollBack then
              Y := wParamHi-bYPos
            else
              Y := wParamHi-(bPageHeight-1)-(bMaxY-bYPos);
            {$ELSE}                                                  
            if tScrollBack then
              Y := lParamLo-bYPos
            else
              Y := lParamLo-(bPageHeight-1)-(bMaxY-bYPos);
            {$ENDIF}                                                  

            bScroll(0, Y);
          end;
      end;
    end;
  end;

  procedure TTerminal.wmHScroll(var Msg : wMsg);
    {-Scroll horizontally}
  var
    X : Integer;
  begin
    with aBuffer, Msg do begin
      case wParam of
        sb_Top :
          bGotoTop(False);
        sb_Bottom :
          bGotoBottom(False);
        sb_LineDown :
          bScroll(1, 0);
        sb_LineUp :
          bScroll(-1, 0);
        sb_PageDown :
          bScroll(bPageHeight, 0);
        sb_PageUp :
          bScroll(-bPageHeight, 0);
        sb_ThumbPosition,
        sb_ThumbTrack :
          begin
            X := lParamLo-(bXPos+1);
            bScroll(X, 0);
          end;
      end;
    end;
  end;

  procedure TTerminal.wmEraseBkGnd(var Msg : wMsg);
  var
    Rect : TRect;
    Brush : HBrush;
  begin
    with aBuffer do begin
      with Rect do begin
        Top    := 0;
        Bottom := cSizeY;
        Left   := 0;
        Right  := cSizeX;
        Brush := CreateSolidBrush(bColors[bbColor]);
        FillRect(HDC(Msg.wParam), Rect, Brush);
        DeleteObject(Brush);
      end;
    end;
  end;

  procedure TTerminal.wmLButtonDown(var Msg : wMsg);
    {-Start marking for clipboard}
  begin
    with aBuffer do begin
      if (GetKeyState(vk_Shift) < 0) then begin
        {Shift is pressed, use old anchor}
        bMarking := True;
        bMarked := False;
        bUpdateMarks(LH(Msg.lParam).L, LH(Msg.lParam).H);
      end else begin
        bResetMarking;
        bMarking := True;
        bMarkAnchorX := bConvertToCol(LH(Msg.lParam).L);
        bMarkAnchorY := bConvertToRow(LH(Msg.lParam).H);
        bMarkStartX := bMarkAnchorX;
        bMarkStartY := bMarkAnchorY;
        bMarkEndX := bMarkStartX;
        bMarkEndY := bMarkStartY;
        if not tPersistentMarking then
          bUpdateMarks(LH(Msg.lParam).L, LH(Msg.lParam).H);
      end;

      {Set up for scrolling marking}
      bScrollTimer := SetTimer(tWnd, tmScrollTimer, 50, nil);
      if bScrollTimer <> 0 then
        SetCapture(tWnd);
    end;
  end;

  procedure TTerminal.wmLButtonUp(var Msg : wMsg);
    {-Finished marking for clipboard}
  begin
    with aBuffer do begin
      {Finished with mouse/scroll stuff}
      ReleaseCapture;
      if bScrollTimer <> 0 then
        KillTimer(tWnd, tmScrollTimer);

      {Say not marking any more}
      bOffScreen := False;
      bMarking := False;
      bMarked := True;
    end;
  end;

  procedure TTerminal.wmMouseMove(var Msg : wMsg);
    {-Update new mouse position}
  var
    RawX, RawY : Integer;
  begin
    with aBuffer do begin
      if bMarking then begin
        {Extract mouse coordinates}
        RawX := LH(Msg.lParam).L;
        RawY := LH(Msg.lParam).H;

        {If outside of window, schedule a scroll, else just do it}
        if (RawY < 0) or (RawY > cSizeY) then
          bOffScreen := True
        else begin
          bOffScreen := False;
          bUpdateMarks(RawX, RawY);
        end;
      end;
    end;
  end;

  procedure TTerminal.wmTimer(var Msg : wMsg);
    {-Perform a timed scroll}
  var
    RawX, RawY : Integer;
    Increment  : Integer;
  begin
    with aBuffer do begin
      if bOffScreen then begin
        tGetMousePos(RawX, RawY);
        if (RawY < 0) or (RawY > cSizeY) then begin
          if (RawY < 0) then begin
            if (RawY < -20) then
              Increment := -3
            else
              Increment := -1;
            bScroll(0, Increment);
            RawY := 0;
          end else begin
            if (RawY - cSizeY) > 20 then
              Increment := 3
            else
              Increment := 1;
            bScroll(0, Increment);
            RawY := cSizeY;
          end;
          bUpdateMarks(RawX, RawY);
        end;
      end;
    end;
  end;

  procedure TTerminal.wmCopy(var Msg : wMsg);
    {-Copy the marked block to the clipboard}
  const
    CRLF : array[1..2] of Char = ^M^J;
  var
    Size     : LongInt;
    I, Lines : Word;
    H        : THandle;
    P, LineP : PChar;
  begin
    with aBuffer do begin
      if not bMarked then
        Exit;

      {Calculate size of marked block}
      Lines := bMarkEndY - bMarkStartY;
      if Lines = 0 then
        {Single line}
        Size :=  (bMarkEndX - bMarkStartX) + 2
      else begin
        {Multiple lines: first, middle, last}
        Size := (bWidth - bMarkStartX) + 2;
        Inc(Size, (Lines-1)*bWidth);
        Inc(Size, (Lines-1)*2);
        Inc(Size, bMarkEndX+2);
      end;
      Inc(Size);

      {Allocate global memory block}
      H := GlobalAlloc(GHND, Size);
      if H = 0 then begin
        SendMessage(bWnd, apw_TermError, Word(ecOutOfMemory), 1);    
        Exit;
      end;

      {Copy selected text to global memory block}
      P := GlobalLock(H);
      for I := bMarkStartY to bMarkEndY do begin
        if I = bMarkStartY then begin
          {First line special case}
          LineP := AddWordToPtr(bScreenBuffer, (I*bWidth)+bMarkStartX);
          Size := (bWidth-bMarkStartX);
        end else begin
          {Not first line}
          LineP := AddWordToPtr(bScreenBuffer, (I*bWidth));
          Size := bWidth;
        end;

        {Last line special case}
        if I = bMarkEndY then
          Dec(Size, bWidth-bMarkEndX);

        {Move it}
        Move(LineP^, P^, Size);

        Inc(P, Size);
        Move(CRLF, P^, 2);
        Inc(P, 2);
      end;
      P^ := #0;
      GlobalUnlock(H);

      {Give the handle to the clipboard}
      OpenClipboard(tWnd);
      EmptyClipboard;
      SetClipboardData(cf_Text, H);
      CloseClipboard;
    end;
  end;

{Terminal control window function}

  {$IFDEF Win32}
  function tTerminalWndFunc(hWindow : TApdHwnd; Msg : UINT;
                            wParam : WPARAM;
                            lParam : LPARAM) : LRESULT; stdcall export;
  {$ELSE}
  function tTerminalWndFunc(hWindow : TApdHwnd; Msg, wParam : Word;
                            lParam : LongInt) : LongInt; export;
  {$ENDIF}
    {-Window function for TerminalWindow}
  var
    PCreate : PCreateStruct absolute lParam;
    PT  : TTerminal;
    WM  : wMsg;

    function DefWndFunc : LongInt;
    begin
      DefWndFunc :=
        CallWindowProc(PT.tDefWndProc, hWindow, Msg, wParam, lParam);
    end;

  begin
    tTerminalWndFunc := 0;

    {Get a pointer to our object}
    if Msg <> wm_Create then begin
      PT := GetTerminalPtr(hWindow);
      if PT = nil then begin
        tTerminalWndFunc := DefWindowProc(hWindow, Msg, wParam, lParam);
        Exit;
      end;
    end;

    {Set up wMsg variable}
    WM.hWindow := hWindow;
    WM.Message := Msg;
    WM.wParam  := wParam;
    WM.lParam  := lParam;

    case Msg of
      {Dispatcher messages}
      apw_TriggerAvail :
        PT.tHandleData(WM);

      {OOTERM messages}
      apw_TermStart :
        PT.tStartTerminal(WM);
      apw_TermStop :
        PT.tStopTerminal(WM);
      apw_TermSetCom :
        PT.tSetComHandle(WM);
      apw_TermRelCom :
        PT.tReleaseComHandle(WM);
      apw_TermSetEmuPtr :
        PT.tSetEmulatorPtr(WM);
      apw_TermSetEmuProc :
        PT.tSetEmulatorProc(WM);
      apw_TermSetKeyEmuPtr :
        PT.tSetKeyEmulatorPtr(WM);
      apw_TermSetKeyEmuProc :
        PT.tSetKeyEmulatorProc(WM);
      apw_TermClear :
        PT.tClearWindow(WM);
      apw_TermBuffer :
        tTerminalWndFunc :=
          PT.aBuffer.bNewBuffer(LH(lParam).L, LH(lParam).H, wParam);
      apw_TermColors :
        PT.aBuffer.bSetColors(wParam, lParam);
      apw_TermColorsH :
        PT.aBuffer.bSetHighlightColors(wParam, lParam);
      apw_TermToggleScroll :
        PT.tToggleScrollback(WM);
      apw_TermStuff :
        PT.tStuffData(WM);
      apw_TermPaint :
        PT.tForcePaint(WM);
      apw_TermCapture :
        tTerminalWndFunc := PT.aBuffer.bSetCapture(Boolean(Lo(wParam)),
                                                     Boolean(Hi(wParam)),
                                                     PChar(lParam));
      apw_TermSetWndProc :
        PT.tSetWndProc(wParam, lParam);
      apw_TermSave :
        tTerminalWndFunc := PT.tSaveRestore(WM);
      apw_TermColorMap :
        tTerminalWndFunc := PT.tGetSetColorMap(WM);
      apw_TermForceSize :
        PT.tForceSize(WM);
      apw_TermFontSize :
        tTerminalWndFunc := PT.tGetFontSize(WM);
      apw_TermBlinkTimeChange :
        PT.tBlinkTimeChange(WM);
      apw_TermPersistentMarkChange :
        PT.tPersistentMarkChange(WM);
      apw_TermSetHalfDuplex :
        PT.tSetHalfDuplex(WM);
      apw_TermGetBuffPtr :
        PT.tGetBuffPtr(WM);
      apw_TermGetClientLine :
        tTerminalWndFunc := PT.tGetFirstClientLine(WM);

      {Windows messages}
      wm_Destroy :
        begin
          PT.aBuffer.bSetCapture(False, False, '');
          PT.tReleaseComHandle(WM);
        end;
      wm_Create:
        with PCreate^ do begin
          SetWindowLong(hWindow, gwl_Terminal, 0);
          PT := TTerminal.Create(hWindow);
          SetWindowLong(hWindow, gwl_Terminal, LongInt(PT));
          if PT <> nil then
            PT.tSetInitialSizes;
        end;
      wm_GetDlgCode :
        tTerminalWndFunc := PT.wmGetDlgCode(WM);
      wm_SetFocus :
        PT.wmSetFocus(WM);
      wm_KillFocus :
        PT.wmKillFocus(WM);
      wm_Enable :
        begin
          InvalidateRect(hWindow, nil, False);
          tTerminalWndFunc := DefWndFunc;
        end;
      wm_NCDestroy :
        begin
          PT.Free;
          SetWindowLong(hWindow, gwl_Terminal, LongInt(nil));
        end;
      wm_Paint :
        PT.wmPaint(WM);
      wm_EraseBkGnd :
        begin
          PT.wmEraseBkGnd(WM);
          tTerminalWndFunc := 1;
        end;
      wm_MouseActivate :
        begin
          tTerminalWndFunc := DefWndFunc;
          PT.wmMouseActivate(WM);
        end;
      wm_LButtonDown :
        PT.wmLButtonDown(WM);
      wm_LButtonUp :
        PT.wmLButtonUp(WM);
      wm_MouseMove :
        PT.wmMouseMove(WM);
      wm_Timer :
        begin
          PT.tBlink;
          PT.wmTimer(WM);
        end;
      wm_Copy :
        PT.wmCopy(WM);
      wm_KeyDown :
        PT.wmKeyDown(WM);
      wm_Char :
        PT.wmChar(WM);
      wm_HScroll :
        PT.wmHScroll(WM);
      wm_VScroll :
        PT.wmVScroll(WM);
      wm_Size :
        PT.wmSize(WM);
      wm_GetMinMaxInfo :
        PT.wmGetMinMaxInfo(WM);
      wm_SetFont :
        tTerminalWndFunc := PT.wmSetFont(wParam);
      wm_GetFont :
        tTerminalWndFunc := PT.tFont;
      else
        tTerminalWndFunc := DefWndFunc;
    end;
  end;

  procedure RegisterTerminalWindowClass(Designing : Boolean);
  const
    Registered : array[Boolean] of Boolean = (False, False);
  var
    XClass: TWndClass;
  begin
    if Registered[Designing] then
      Exit;
    Registered[Designing] := True;

    with XClass do begin
      Style         := cs_HRedraw or cs_VRedraw or cs_DblClks;
      lpfnWndProc   := @tTerminalWndFunc;
      cbClsExtra    := 0;
      cbWndExtra    := SizeOf(Pointer);
      {$IFDEF VERSION3}
      if ModuleIsLib and not ModuleIsPackage then
        hInstance   := SysInit.hInstance
      else
        hInstance   := System.MainInstance;
      hIcon         := LoadIcon(hInstance, 'DEFICON');
      {$ELSE}
      hInstance     := System.hInstance;
      hIcon         := LoadIcon(System.hInstance, 'DEFICON');
      {$ENDIF}
      hCursor       := LoadCursor(0, idc_Arrow);
      hbrBackground := 0;
      lpszMenuName  := nil;
      if Designing then
        lpszClassName := TerminalClassNameDesign
      else
        lpszClassName := TerminalClassName;
    end;
    WinProcs.RegisterClass(XClass);
  end;

end.
