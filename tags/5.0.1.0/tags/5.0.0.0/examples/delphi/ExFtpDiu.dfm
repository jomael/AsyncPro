�
 TFORM1 0�  TPF0TForm1Form1Left�TopSWidth�HeightUCaptionExFtpDir: FTP Directory ExampleColor	clBtnFace
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTopWidthOHeightCaptionFTP Server URL  TLabelLabel2LeftTop$Width5HeightCaption	User Name  TLabelLabel3LeftTop<Width.HeightCaptionPassword  TLabelLabel4LeftToppWidthOHeightCaptionCurrent DirectoryWordWrap	  TLabelLabel5LeftuTop_Width� HeightCaption (double click on item to select)  TButtonbtnLoginLeft0TopWidthKHeightCaptionLog&inTabOrderOnClickbtnLoginClick  TButton	btnLogoutLeft0Top0WidthKHeightCaptionLog&outTabOrderTabStopOnClickbtnLogoutClick  TListBoxlbxCurrentDirLeft`ToppWidth� Height� TabStop
ItemHeightTabOrder
OnDblClicklbxCurrentDirDblClickOnMouseDownlbxCurrentDirMouseDown  TEdit	edtServerLefthTopWidth� HeightTabOrder   TEditedtUserLefthTop Width� HeightTabOrder  TEditedtPasswordLefthTop8Width� HeightTabOrder  TButtonbtnCloseLeft0TopWidthKHeightCaption&CloseTabOrderTabStopOnClickbtnCloseClick  TApdFtpClientApdFtpClient1ConnectTimeout FileTypeftBinaryPassiveModeTransferTimeoutD
OnFtpErrorApdFtpClient1FtpErrorOnFtpStatusApdFtpClient1FtpStatusLogNameAPRO.LOG	TraceNameAPRO.TRCWsPortftp	OnWsErrorApdFtpClient1WsErrorLeftPToph   