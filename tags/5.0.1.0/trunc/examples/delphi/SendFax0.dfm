�
 TSFMAIN 0�  TPF0TsfMainsfMainLeft� Top� BorderStylebsSingleCaptionSendFaxClientHeight�ClientWidth
Font.ColorclBlackFont.Height�	Font.NameArial
Font.Style PositionpoScreenCenterOnShowFormShowPixelsPerInch`
TextHeight TLabelLabel7LeftTop� WidthHHeightCaptionFaxes to send:  TButtonSendFaxLeftTop�WidthIHeight!Caption&Send faxesTabOrderOnClickSendFaxClick  TButtonsfAddLeft(Top�WidthIHeight!Caption&AddDefault	TabOrder OnClick
sfAddClick  TButtonsfExitLeft� Top�WidthIHeight!CaptionE&xitTabOrderOnClicksfExitClick  TPanelPanel1LeftTopWidth	Height� TabOrder TLabelLabel1Left� TophWidth|HeightCaption&Modem initialization string:  TLabelLabel2Left� Top� Width:HeightCaptionFax &header:  TLabelLabel3Left� TopWidth0HeightCaptionStation &ID:  TLabelLabel4Left� Top8Width3HeightCaption&Dial prefix:  TLabelLabel5LeftTopxWidth@HeightCaptionDial a&ttempts:  TLabelLabel6LeftTop� Width5HeightCaptionRetry &wait:  TRadioGroup
sfFaxClassLeftTopWidth� HeightYCaption
Fax &class	ItemIndex Items.Stringsauto detectclass 1class 2	class 2.0 TabOrder OnClicksfFaxClassClick  TEditsfModemInitLeft� TopxWidthAHeightTabOrderOnChangesfModemInitChange  TEditsfHeaderLeft� Top� WidthAHeightTabOrderText"Fax sent by $I using APro    $D $TOnChangesfHeaderChange  TEditsfStationIDLeft� TopWidth� Height	MaxLengthTabOrderTextAPro SENDFAXOnChangesfStationIDChange  TEditsfDialPrefixLeft� TopHWidthyHeightTabOrderOnChangesfDialPrefixChange  TEditsfDialAttemptsLeftpTopxWidth)HeightTabOrderText3OnChangesfDialAttemptsChange  TEditsfRetryWaitLeftpTop� Width)HeightTabOrderText60OnChangesfRetryWaitChange  	TCheckBoxEnhTextLeftTop� WidthHeightCaption0Use Enhanced Text for Fax Headers and Cover PageTabOrder  TButton
HdrFontBtnLeftWTop� WidthKHeightCaptionHeader FontTabOrderOnClickHdrFontBtnClick  TButton
CvrFontBtnLeft�Top� WidthKHeightCaption
Cover FontTabOrder	OnClickCvrFontBtnClick   TButtonsfModifyLeftxTop�WidthIHeight!Caption&ModifyTabOrderOnClicksfModifyClick  TButtonsfDeleteLeft�Top�WidthIHeight!Caption&DeleteTabOrderOnClicksfDeleteClick  TListBoxsfFaxListBoxLeftTopWidth	HeightiTabStop
Font.ColorclBlackFont.Height�	Font.NameCourier New
Font.Style 
ItemHeight
ParentFontTabOrder  TButtonsfSelectComPortLeftXTop�WidthIHeight!CaptionSelect &portTabOrderOnClicksfSelectComPortClick  TApdComPortApdComPort1InSize  OutSize  AutoOpenHWFlowOptions	hwfUseRTShwfRequireCTS 
BufferFull�BufferResume3TracingtlOn	TraceSize }	TraceNameSENDFAX.TRCLoggingtlOnLogNameSENDFAX.LOGTapiModetmOnLeft�Top  TApdFaxStatusApdFaxStatus1PositionpoScreenCenterCtl3D	VisibleFaxApdSendFax1Caption
Fax StatusLeft�Top.  TApdSendFaxApdSendFax1FaxClass	fcUnknownComPortApdComPort1StatusDisplayApdFaxStatus1FaxLog
ApdFaxLog1
FaxFileExtAPFOnFaxLogApdSendFax1FaxLogOnFaxFinishApdSendFax1FaxFinishEnhTextEnabledEnhHeaderFont.ColorclWindowTextEnhHeaderFont.Height�EnhHeaderFont.NameArialEnhHeaderFont.Style EnhFont.ColorclWindowTextEnhFont.Height�EnhFont.NameCourier NewEnhFont.Style 	OnFaxNextApdSendFax1FaxNextLeft�TopFakeProperty  
TApdFaxLog
ApdFaxLog1FaxHistoryName
APDFAX.HISFaxApdSendFax1Left�Top.  TApdTapiDeviceApdTapiDevice1ComPortApdComPort1ShowTapiDevices		ShowPorts	EnableVoiceOnTapiPortOpenApdTapiDevice1TapiPortOpenOnTapiPortCloseApdTapiDevice1TapiPortCloseLeft�TopO  TFontDialogFontDialog1
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style MinFontSize MaxFontSize Options	fdEffectsfdForceFontExist Left�TopO   