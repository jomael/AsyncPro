�
 TFORM1 0�	  TPF0TForm1Form1Left� Top� BorderStylebsSingleCaptionRcvFaxClientHeight~ClientWidth.
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style PositionpoScreenCenterPixelsPerInch`
TextHeight TPanelPanel1LeftTopWidthHeight� TabOrder  TLabelLabel1Left� TopWidthXHeightCaption&Receive directory:  TLabelLabel2Left� TopGWidth|HeightCaption&Modem initialization string:  TRadioGroup
rfFaxClassLeft
TopWidth� HeightXCaption
Fax &class	ItemIndex Items.Stringsauto detectclass 1class 2	class 2.0 TabOrder OnClickrfFaxClassClick  TRadioGrouprfNameStyleLeft
TopsWidth� Height6Caption&Name style	ItemIndex Items.Stringscount (faxnnnn.apf)month/day (mmddnnnn.apf) TabOrderOnClickrfNameStyleClick  TEditrfDirectoryLeft� Top!WidthHeightTabOrderOnChangerfDirectoryChange  TEditrfModemInitLeft� TopXWidthHeightTabOrderOnChangerfModemInitChange   TPanelPanel2LeftTop� WidthHeight� CaptionPanel2TabOrder TLabelLabel3LeftTopWidthOHeightCaptionReceived faxes:  TListBoxrfReceiveListLeftTopWidthHeighto
ItemHeightTabOrder    TButtonrfReceiveFaxesLeft	TopXWidthYHeight!Caption&Receive faxesTabOrderOnClickrfReceiveFaxesClick  TButtonrfExitLeft� TopXWidthYHeight!CaptionE&xitTabOrderOnClickrfExitClick  TButtonrfSelectPortLefthTopXWidthYHeight!CaptionSelect &portTabOrderOnClickrfSelectPortClick  TApdComPortApdComPort1InSize  OutSize  AutoOpenHWFlowOptions	hwfUseRTShwfRequireCTS 
BufferFull�BufferResume3TracingtlOn	TraceName
RCVFAX.TRCLoggingtlOnLogName
RCVFAX.LOGTapiModetmOnLeft�TopV  TApdFaxStatusApdFaxStatus1PositionpoScreenCenterCtl3D	VisibleFaxApdReceiveFax1Caption
Fax StatusLeft�TopU  
TApdFaxLog
ApdFaxLog1FaxHistoryName
APDFAX.HISFaxApdReceiveFax1Left�TopU  TApdReceiveFaxApdReceiveFax1	StationIDAPro RcvFaxComPortApdComPort1StatusDisplayApdFaxStatus1FaxLog
ApdFaxLog1
FaxFileExtAPFOnFaxLogApdReceiveFax1FaxLog
OnFaxErrorApdReceiveFax1FaxErrorOnFaxFinishApdReceiveFax1FaxFinishConstantStatus	Left�TopVFakeProperty  TApdTapiDeviceApdTapiDevice1ComPortApdComPort1ShowTapiDevices		ShowPorts	EnableVoiceOnTapiPortOpenApdTapiDevice1TapiPortOpenOnTapiPortCloseApdTapiDevice1TapiPortCloseLeftkTopV   