�
 TFORM1 0@  TPF0TForm1Form1Left� Top� BorderStylebsSingleCaptionFtpDemo: Ftp ClientClientHeightjClientWidth�
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style Menu	MainMenu1PositionpoScreenCenter
OnActivateFormActivatePixelsPerInch`
TextHeight TTabbedNotebookTabbedNotebook1Left Top Width�Height� AlignalTopTabFont.CharsetDEFAULT_CHARSETTabFont.Color	clBtnTextTabFont.Height�TabFont.NameMS Sans SerifTabFont.Style TabOrder  TTabPage LeftTopCaption&Login Information TLabelLabel7LeftTopWidthHeightCaptionServer  TLabelLabel2LeftTop>Width3HeightCaption	User name  TLabelLabel3LeftTop^Width.HeightCaptionPassword  TEdit
ServerEditLeftPTopWidth� HeightTabOrder   TEditUserNameEditLeftPTop:Width� HeightTabOrder  TEditPasswordEditLeftPTopZWidth� HeightTabOrder  TButtonLoginBtnLeftpTopWidthIHeightCaptionLoginTabOrderOnClickLogin1Click  TButton	LogoutBtnLeftpTop.WidthIHeightCaptionLogoutTabOrderOnClickLogout1Click  TButtonExitBtnLeftpTop^WidthIHeightCaption&ExitTabOrderOnClick
Exit1Click   TTabPage LeftTopCaption&Transfer Options TLabelLabel1Left ToptWidthEHeightCaptionTimeout (ticks)  TLabelLabel6Left8TopZWidth.HeightCaption
Restart at  TRadioGroupReceiveMode1Left� TopWidth� HeightACaption Receive Mode ColumnsCtl3D		ItemIndexItems.StringsAppendReplaceRestart ParentCtl3DTabOrder   TRadioGroup	SendMode1LeftTopWidth� HeightACaption Send Mode Columns	ItemIndexItems.StringsAppendReplaceUniqueRestart TabOrder  TEditTimeoutEditLeftnToppWidth9HeightTabOrder  TEditRestartEditLeftnTopVWidth9HeightTabOrder  TRadioGroup	FileType1LeftXTopWidthQHeightACaption File Type 	ItemIndex Items.StringsAsciiBinary TabOrder  TButton
ReceiveBtnLeft� Top`WidthKHeightCaptionReceiveTabOrderOnClickRecieve1Click  TButtonSendBtnLeftTop`WidthKHeightCaptionSendTabOrderOnClick
Send1Click   TTabPage LeftTopCaptionServer Replies TMemo	ReplyMemoLeft Top Width�Height� AlignalClient
ScrollBars
ssVerticalTabOrder     	TGroupBox	GroupBox2Left Top� Width�Height� AlignalClientCaption Requested Information TabOrder TMemoInfoMemoLeftTopWidth�Height� AlignalClient
ScrollBars
ssVerticalTabOrder    	TMainMenu	MainMenu1Left Top0 	TMenuItemFile1Caption&File 	TMenuItemLogin1CaptionLog&inOnClickLogin1Click  	TMenuItemLogout1CaptionLog&outOnClickLogout1Click  	TMenuItemN2Caption-  	TMenuItemSend1Caption&Send...OnClick
Send1Click  	TMenuItemRecieve1Caption&Receive...OnClickRecieve1Click  	TMenuItemRename1Caption
Re&name...OnClickRename1Click  	TMenuItemDelete1Caption
&Delete...OnClickDelete1Click  	TMenuItemN1Caption-  	TMenuItemExit1Caption&ExitOnClick
Exit1Click   	TMenuItem
Directory1Caption
&Directory 	TMenuItemList1CaptionList 	TMenuItem	FullList1CaptionFull...OnClickFullList1Click  	TMenuItem
NamesList1CaptionNames...OnClickNamesList1Click   	TMenuItemChange1Caption	Change...OnClickChange1Click  	TMenuItem
CreateDir1Caption	Create...OnClickCreateDir1Click  	TMenuItemRename2Caption	Rename...OnClickRename1Click  	TMenuItemN3Caption-  	TMenuItemDelete2Caption	Delete...OnClickDelete1Click   	TMenuItemMisc1Caption&Misc 	TMenuItemHelp1Caption&Help...OnClick
Help1Click  	TMenuItemStatus1CaptionServer &status...OnClickStatus1Click  	TMenuItemSendFtp1CaptionSend &Ftp command...OnClickSendFtp1Click  	TMenuItemN4Caption-  	TMenuItemLog1Caption&Log dialogOnClick	Log1Click  	TMenuItem
Clearmemo1Caption&Clear memoOnClickClearBtnClick    TApdFtpClientApdFtpClient1ConnectTimeout FileTypeftAsciiFtpLog
ApdFtpLog1Password
1234567890PassiveModeServerAddressftp.turbopower.comTransferTimeoutUserName	anonymous
OnFtpErrorApdFtpClient1FtpError
OnFtpReplyApdFtpClient1FtpReplyOnFtpStatusApdFtpClient1FtpStatusLogNameAPRO.LOG	TraceNameAPRO.TRCWsPortftpLeft@Top0  
TApdFtpLog
ApdFtpLog1EnabledFtpHistoryNameAPROFTP.HIS	FtpClientApdFtpClient1LeftTop0   