�
 TFORM1 0  TPF0TForm1Form1Left� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsDialogCaption(Simple Fax on Demand (Same-Call Receive)ClientHeight� ClientWidth7
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style PositionpoScreenCenterOnCreate
FormCreatePixelsPerInch`
TextHeight TButtonButton1Left'Top� Width|HeightCaptionSelect A TAPI DeviceTabOrder OnClickButton1Click  TButtonAnswerButtonLeft� Top� WidthLHeightCaptionAnswer CallsTabOrderOnClickAnswerButtonClick  	TGroupBox	GroupBox1LeftTopWidth&HeightwCaptionStatusTabOrder TLabelLabel1LeftTopWidthHeight	AlignmenttaCenterAutoSizeCaptionFax on Demand Current State  TLabelLabel2LeftTopDWidthHeight	AlignmenttaCenterAutoSizeCaptionWave Device Status  TEditEdit1LeftTop#Width	HeightColorclSilverReadOnly	TabOrder Text	StateIdle  TEditEdit2LeftTopRWidth	HeightColorclSilverReadOnly	TabOrderTextWave Device Idle...   TApdTapiDeviceApdTapiDevice1ComPortApdComPort1AnswerOnRingShowTapiDevices		ShowPorts	EnableVoice	OnTapiConnectApdTapiDevice1TapiConnect
OnTapiDTMFApdTapiDevice1TapiDTMFOnTapiWaveNotifyApdTapiDevice1TapiWaveNotifyLeft  TApdFaxStatusApdFaxStatus1PositionpoScreenCenterCtl3D	VisibleFaxApdReceiveFax1Caption
Fax StatusLeft8  TApdComPortApdComPort1InSize  OutSize  AutoOpenHWFlowOptions	hwfUseRTShwfRequireCTS 
BufferFull�BufferResume3TracingtlOn	TraceSize }	TraceName	VOICE.TRCLoggingtlOnLogSize �  LogName	VOICE.LOG  TApdReceiveFaxApdReceiveFax1	StationID0ComPortApdComPort1StatusDisplayApdFaxStatus1FaxFile
fax001.apf
FaxFileExtAPFOneFax	DestinationDirc:\LeftTFakeProperty   