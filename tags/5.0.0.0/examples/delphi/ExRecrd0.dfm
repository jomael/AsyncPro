�
 TFORM1 0�  TPF0TForm1Form1Left� Top� WidthZHeightyCaption(Async Professional Advanced TAPI Example
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style PositionpoScreenCenterOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1Left	Top
WidthWHeight	AlignmenttaRightJustifyAutoSizeCaption	Caller ID  TLabelLabel2Left	Top&WidthWHeight	AlignmenttaRightJustifyAutoSizeCaptionCaller ID Name  TLabelLabel5LeftTop@Width� HeightCaption$Maximum Message Length (In Seconds):  TLabelLabel3LeftTop� Width� HeightCaption!Messages (double-click to play) :  TEditCallerIDLeftcTopWidth� HeightColorclSilverReadOnly	TabOrder TextCallerID  TEditCallerIDNameLeftcTop#Width� HeightColorclSilverReadOnly	TabOrderTextCallerIDName  TButtonButton1LeftTop� Width|HeightCaptionSelect A TAPI DeviceTabOrderOnClickButton1Click  TButtonAnswerButtonLeft� Top� WidthHHeightCaptionAnswer CallsTabOrderOnClickAnswerButtonClick  TButton
CancelCallLeft� Top� WidthHHeightCaptionCancel CallEnabledTabOrderOnClickCancelCallClick  	TGroupBox	GroupBox1LeftTopxWidthAHeight/Caption Status TabOrder TLabelLabel4LeftTopWidth'HeightAutoSizeCaptionWaiting For Call   TEditMaxLengthEditLeft� Top<WidthiHeightTabOrderText30OnExitMaxLengthEditExit  TListBoxCallsListBoxLeftTop� WidthAHeightq
ItemHeight	PopupMenu
PopupMenu1TabOrder
OnDblClickCallsListBoxDblClick	OnKeyDownCallsListBoxKeyDown  	TCheckBoxMonitorLeftTopXWidthHeightCaption$Monitor Recording Through Sound CardState	cbCheckedTabOrderOnClickMonitorClick  TApdComPortApdComPort1AutoOpen	TraceNameAPRO.TRCLogNameAPRO.LOGTapiModetmOnLeft  TApdTapiDeviceApdTapiDevice1ComPortApdComPort1AnswerOnRingShowTapiDevices		ShowPortsEnableVoiceOnTapiConnectApdTapiDevice1TapiConnectOnTapiCallerIDApdTapiDevice1TapiCallerIDOnTapiWaveNotifyApdTapiDevice1TapiWaveNotify  
TPopupMenu
PopupMenu1OnPopupPopupMenu1PopupTop 	TMenuItemPlay1Caption&PlayOnClickCallsListBoxDblClick  	TMenuItemDelete1Caption&DeleteOnClickDelete1Click    