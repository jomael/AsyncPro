�
 TFORM1 0�  TPF0TForm1Form1Left� Top� Width�HeightCaptionExSapiPh
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTopWidth�HeightAutoSizeCaptiongThis demonstration shows using the TApdSapiPhone component in a more complicated telephony application.WordWrap	  TLabelLabel2Left
Top(Width�Height!AutoSizeCaption�When the program answers the phone, it will ask the user for their phone number, a date and a time.  These values are listed in the grid below.WordWrap	  TGaugeGauge1Left�ToppWidthHeightqKindgkVerticalBarShowText	ForeColorclNavy	BackColor	clBtnFaceMaxValue��  Progress   TLabelLogLeftTop@WidthHeightCaptionLog  TButton	btnAnswerLeftTopPWidthKHeightCaptionAnswerTabOrder OnClickbtnAnswerClick  TStringGridStringGrid1LeftToppWidth�Height� ColCount	FixedCols RowCount	FixedRows TabOrder	ColWidths� j_%   TMemoMemo1LeftTopPWidth�Height� 
ScrollBars
ssVerticalTabOrderWordWrap  TApdSapiEngineApdSapiEngine1CharSetcsText	Dictation
SRAutoGain 
TTSOptions OnInterferenceApdSapiEngine1InterferenceOnPhraseFinishApdSapiEngine1PhraseFinish	OnSRErrorApdSapiEngine1SRErrorOnSRWarningApdSapiEngine1SRWarning	OnSSErrorApdSapiEngine1SSErrorOnSSWarningApdSapiEngine1SSWarning	OnVUMeterApdSapiEngine1VUMeterLeft� TopP  TApdSapiPhoneApdSapiPhone1NoAnswerMax NoAnswerTime 	NumDigits Options 
SapiEngineApdSapiEngine1OnAskForDateFinishApdSapiPhone1AskForDateFinishOnAskForPhoneNumberFinish$ApdSapiPhone1AskForPhoneNumberFinishOnAskForTimeFinishApdSapiPhone1AskForTimeFinishOnAskForYesNoFinishApdSapiPhone1AskForYesNoFinishOnTapiDisconnectApdSapiPhone1TapiDisconnectComPortApdComPort1ShowTapiDevices		ShowPorts	EnableVoice	OnTapiConnectApdSapiPhone1TapiConnectLeftXTopP  TApdComPortApdComPort1	TraceNameAPRO.TRCLogNameAPRO.LOGLeftxTopP   