�
 TFORM1 0  TPF0TForm1Form1Left� Top� Width�Height�CaptionExSapiAs
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OnCreate
FormCreate	OnDestroyFormDestroyPixelsPerInch`
TextHeight TLabelLabel1LeftTopWidth�Height!AutoSizeCaptionXThis example program shows how to use the AskFor methods of the TApdSapiPhone component.WordWrap	  TLabellblDateLeft� Top=WidthHeightCaptionNone  TLabellblExtensionLeft� Top]WidthHeightCaptionNone  TLabel	lblPlanetLeft� Top}WidthHeightCaptionNone  TLabellblColorLeft� Top� WidthHeightCaptionNone  TLabellblPhoneNumberLeft� Top� WidthHeightCaptionNone  TLabellblSpellingLeft� Top� WidthHeightCaptionNone  TLabellblTimeLeft� Top� WidthHeightCaptionNone  TLabellblYesNoLeft� TopWidthHeightCaptionNone  TLabelLabel2Left� Top=WidthHeightCaptionDate:  TLabelLabel3Left� Top]Width1HeightCaption
Extension:  TLabelLabel4Left� Top}Width!HeightCaptionPlanet:  TLabelLabel5Left� Top� WidthHeightCaptionColor:  TLabelLabel6Left� Top� WidthJHeightCaptionPhone Number:  TLabelLabel7Left� Top� Width(HeightCaption	Spelling:  TLabelLabel8Left� Top� WidthHeightCaptionTime:  TLabelLabel9Left� TopWidth(HeightCaptionYes/No:  TLabelLabel10LeftTop@Width#HeightCaptionReplies  TShapeShape1Left� Top� WidthHeightBrush.Color	clBtnFaceShapestCircle  TButtonbtnDateLeftTop8Width� HeightCaptionAsk For DateTabOrder OnClickbtnDateClick  TButtonbtnExtensionLeftTopXWidth� HeightCaptionAsk for ExtensionTabOrderOnClickbtnExtensionClick  TButton	btnPlanetLeftTopxWidth� HeightCaptionAsk for PlanetTabOrderOnClickbtnPlanetClick  TButtonbtnColorLeftTop� Width� HeightCaptionAsk for ColorTabOrderOnClickbtnColorClick  TButtonbtnPhoneNumberLeftTop� Width� HeightCaptionAsk for Phone NumberTabOrderOnClickbtnPhoneNumberClick  TButtonbtnSpellingLeftTop� Width� HeightCaptionAsk for SpellingTabOrderOnClickbtnSpellingClick  TButtonbtnTimeLeftTop� Width� HeightCaptionAsk for TimeTabOrderOnClickbtnTimeClick  TButtonbtnYesNoLeftTopWidth� HeightCaptionAsk for Yes/NoTabOrderOnClickbtnYesNoClick  TMemoMemo1LeftTopPWidth�Height� Lines.StringsMemo1 
ScrollBars
ssVerticalTabOrder  TApdSapiEngineApdSapiEngine1CharSetcsText	Dictation
SRAutoGain 
TTSOptions OnInterferenceApdSapiEngine1InterferenceOnPhraseFinishApdSapiEngine1PhraseFinish	OnSRErrorApdSapiEngine1SRErrorOnSRWarningApdSapiEngine1SRWarning	OnSSErrorApdSapiEngine1SSErrorOnSSWarningApdSapiEngine1SSWarningLeftTop@  TApdSapiPhoneApdSapiPhone1NoAnswerMax NoAnswerTime 	NumDigits Options 
SapiEngineApdSapiEngine1OnAskForDateFinishApdSapiPhone1AskForDateFinishOnAskForExtensionFinish"ApdSapiPhone1AskForExtensionFinishOnAskForListFinishApdSapiPhone1AskForListFinishOnAskForPhoneNumberFinish$ApdSapiPhone1AskForPhoneNumberFinishOnAskForSpellingFinish!ApdSapiPhone1AskForSpellingFinishOnAskForTimeFinishApdSapiPhone1AskForTimeFinishOnAskForYesNoFinishApdSapiPhone1AskForYesNoFinishShowTapiDevices		ShowPorts	EnableVoiceLeft8Top@   