�
 TFORM1 0�	  TPF0TForm1Form1Left� Top� WidthHeight�CaptionExSMSMessage
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style PixelsPerInch`
TextHeight TLabelLabel1Left� Top8Width^HeightCaptionDestination Address  TLabelLabel2Left� TophWidth8HeightCaption
Time Stamp  TLabelLabel3Left� Top� WidthXHeightCaptionStatus of Message  TLabelLabel4Left� Top� WidthYHeightCaptionThe Message itself  TLabelLabel5Left Top Width� HeightCaptionMessage Store (List Messages)  TLabelLabel6Left� TopWidthHeightCaptionIndex  TLabelLabel7LeftpTopXWidth� HeightCaptionButtons below for highlighted  TLabelLabel8LeftXTophWidth� HeightCaption"message in TreeView or right click  TLabelLabel9LeftpTopxWidth� HeightCaptionthe message for pop-up menu  TButton
btnConnectLeft(TopWidthKHeightCaptionConnectTabOrder OnClickbtnConnectClick  TEditEdit1Left� TopHWidthyHeightTabOrder  TEditEdit2Left� TopxWidthyHeightReadOnly	TabOrder  TEditEdit3Left� Top� WidthyHeightReadOnly	TabOrder  TMemoMemo1Left� Top� Width� Height� Lines.StringsMemo1 TabOrder  TButtonButton1Left�Top� WidthaHeightCaptionSend A Message TabOrderOnClick
Send1Click  TAdTerminalAdTerminal1Left Top� Width� Height� CaptureFileAPROTERM.CAPComPortApdComPort1
ScrollbackColorclBlack
Font.ColorclSilverFont.Height�	Font.NameTerminal
Font.Style ParentColor
ParentFontTabOrder  TButtonDelete1Left�Top� WidthaHeightCaptionDelete a messageTabOrderOnClickDelete1Click  TEditedtIndexLeft� TopWidthHeightTabOrder  	TTreeView	TreeView1Left TopWidth� Height� IndentTabOrder	
OnDblClickTreeView1DblClick	OnKeyDownTreeView1KeyDown  TButtonbtnAddMessageLeft�TopWidthKHeightCaptionAdd MessageTabOrder
OnClickbtnAddMessageClick  TApdGSMPhoneApdGSMPhone1ComPortApdComPort1QuickConnectNotifyOnNewMessageOnMessageListApdGSMPhone1MessageListOnGSMCompleteApdGSMPhone1GSMCompleteLeft�Top(  TApdComPortApdComPort1Baud � 	TraceNameAPRO.TRCLoggingtlOnLogNameAPRO.LOGLeft�Top  
TPopupMenu
PopupMenu1LeftXTopP 	TMenuItemSend1Caption&SendOnClick
Send1Click  	TMenuItemDelete2Caption&DeleteOnClickDelete1Click    