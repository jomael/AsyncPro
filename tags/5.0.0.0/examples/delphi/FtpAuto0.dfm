�
 TFORM1 0�  TPF0TForm1Form1Left� Top� WidthAHeightNCaptionFtpAuto
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OnCreate
FormCreate	OnDestroyFormDestroyPixelsPerInch`
TextHeight 	TGroupBox	GroupBox1LeftTopWidthHeight� Caption Login/Logout TabOrder  TLabelLabel1LeftTopWidthHeightCaptionURL  TLabelLabel2LeftTop8Width0HeightCaptionUsername  TLabelLabel3LeftTopXWidth.HeightCaptionPassword  TButtonbtnLoginLeft`Top� WidthKHeightCaptionLoginTabOrder OnClickbtnLoginClick  TButton	btnLogoutLeft� Top� WidthKHeightCaptionLogoutTabOrderOnClickbtnLogoutClick  TEditedtURLLeftXTopWidth� HeightTabOrder  TEditedtUsernameLeftXTop8Width� HeightTabOrder  TEditedtPasswordLeftXTopXWidth� HeightTabOrder   	TGroupBox	GroupBox2Left TopWidthHeight!Caption Remote Directory TabOrder TLabelLabel5LeftTop� WidthGHeightCaptionLocal Directory  TListBoxlbxFilesLeftTopWidth� Height� 
ItemHeightTabOrder 
OnDblClicklbxFilesDblClickOnMouseDownlbxFilesMouseDown  TEditedtLocalDirLeftTop Width� HeightTabOrderTextc:\temp   TMemoMemo1LeftTop� WidthHeightqLines.Strings.This example demonstrates how to automate the .process of downloading files from an FTP host. 2Each time you double-click on a directory name to 9change the current working directory, all its files will 0be transferred to the specified local directory.  TabOrder  TApdFtpClientApdFtpClient1ConnectTimeout FileTypeftBinaryPassword
1234567890PassiveModeServerAddressftp.turbopower.comTransferTimeoutDUserName	anonymous
OnFtpErrorApdFtpClient1FtpErrorOnFtpStatusApdFtpClient1FtpStatusLoggingtlOnLogName
FtpLog.txtTracingtlOn	TraceName
FtpTrc.txtWsPortftpLeft(Top�    