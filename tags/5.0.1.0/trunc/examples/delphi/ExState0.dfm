�
 TFORM1 0w  TPF0TForm1Form1Left� Top� Width� Height�CaptionExStateColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	PixelsPerInch`
TextHeight TApdStateMachineApdStateMachine1LeftTopWidth� HeightICaptionCallerID State MachineComPortApdComPort1
StartState	ApdState1TerminalState	ApdState4ColorclWhiteTabOrder  	TApdState	ApdState1LeftTopWidthTHeight2ActiveColorclYellowCaptionStart state (ATZ)
ConditionsDefaultErrorDefaultNextStartStringOK	EndString^M
PacketSize Timeout 	NextState	ApdState2	ErrorCode 
IgnoreCaseConnectoid.ColorclBlueConnectoid.WidthConnectoid.Font.CharsetDEFAULT_CHARSETConnectoid.Font.ColorclWindowTextConnectoid.Font.Height�Connectoid.Font.NameMS Sans SerifConnectoid.Font.Style Connectoid.Caption ColorclBlueFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style   
GlyphCells InactiveColorclWhiteOutputOnActivateATZ  	TApdState	ApdState3LeftTop� Width� Height2ActiveColorclYellowCaptionWait for CID and RING
ConditionsDefaultErrorDefaultNextStartStringRING
PacketSize Timeout 	ErrorCode 
IgnoreCaseConnectoid.Caption
ConnectoidConnectoid.ColorclBlueConnectoid.WidthConnectoid.Font.CharsetDEFAULT_CHARSETConnectoid.Font.ColorclWindowTextConnectoid.Font.Height�Connectoid.Font.NameMS Sans SerifConnectoid.Font.Style Caption
ConnectoidColorclBlueFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style  DefaultErrorDefaultNextStartStringDATE:	EndString^M
PacketSize Timeout 	NextState	ApdState3	ErrorCode 
IgnoreCaseConnectoid.Caption
ConnectoidConnectoid.ColorclBlueConnectoid.WidthConnectoid.Font.CharsetDEFAULT_CHARSETConnectoid.Font.ColorclWindowTextConnectoid.Font.Height�Connectoid.Font.NameMS Sans SerifConnectoid.Font.Style Caption
ConnectoidColorclBlueFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style  DefaultErrorDefaultNextStartStringNMBR:	EndString^M
PacketSize Timeout 	NextState	ApdState3	ErrorCode 
IgnoreCaseConnectoid.Caption
ConnectoidConnectoid.ColorclBlueConnectoid.WidthConnectoid.Font.CharsetDEFAULT_CHARSETConnectoid.Font.ColorclWindowTextConnectoid.Font.Height�Connectoid.Font.NameMS Sans SerifConnectoid.Font.Style Caption
ConnectoidColorclBlueFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style  DefaultErrorDefaultNextStartStringNAME:	EndString^M
PacketSize Timeout 	NextState	ApdState3	ErrorCode 
IgnoreCaseConnectoid.Caption
ConnectoidConnectoid.ColorclBlueConnectoid.WidthConnectoid.Font.CharsetDEFAULT_CHARSETConnectoid.Font.ColorclWindowTextConnectoid.Font.Height�Connectoid.Font.NameMS Sans SerifConnectoid.Font.Style Caption
ConnectoidColorclBlueFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style   
GlyphCells InactiveColorclWhiteOnStateFinishApdState3StateFinish  	TApdState	ApdState4Left!Top WidthKHeight2ActiveColorclYellowCaptionAnswer (ATA)
ConditionsDefaultErrorDefaultNextStartStringCONNECT
PacketSize Timeout 	ErrorCode 
IgnoreCaseConnectoid.Caption
ConnectoidConnectoid.ColorclBlueConnectoid.WidthConnectoid.Font.CharsetDEFAULT_CHARSETConnectoid.Font.ColorclWindowTextConnectoid.Font.Height�Connectoid.Font.NameMS Sans SerifConnectoid.Font.Style Caption
ConnectoidColorclBlueFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style   
GlyphCells InactiveColorclWhiteOutputOnActivateATA  	TApdState	ApdState2Left!Top`WidthKHeight2ActiveColorclLimeCaptionInit for CID
ConditionsDefaultErrorDefaultNextStartStringOK
PacketSize Timeout 	NextState	ApdState3	ErrorCode 
IgnoreCaseConnectoid.ColorclBlueConnectoid.WidthConnectoid.Font.CharsetDEFAULT_CHARSETConnectoid.Font.ColorclWindowTextConnectoid.Font.Height�Connectoid.Font.NameMS Sans SerifConnectoid.Font.Style Connectoid.Caption ColorclBlueFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style   
GlyphCells InactiveColorclWhiteOutputOnActivate	AT#CID=1   TButtonButton1Left+TopXWidthKHeightCaptionAnswerTabOrderOnClickButton1Click  TApdComPortApdComPort1	TraceNameAPRO.TRCLogNameAPRO.LOGLeftTop    