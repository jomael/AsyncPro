object DeviceSelectionForm: TDeviceSelectionForm
  Left = 228
  Top = 159
  ActiveControl = dsfOkBitBtn
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Device Selection'
  ClientHeight = 75
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object dsfComboBox: TComboBox
    Left = 8
    Top = 8
    Width = 209
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object dsfOkBitBtn: TBitBtn
    Left = 56
    Top = 40
    Width = 75
    Height = 27
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = dsfOkBitBtnClick
    NumGlyphs = 2
  end
  object dsfCancelBitBtn: TBitBtn
    Left = 140
    Top = 40
    Width = 75
    Height = 27
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = dsfCancelBitBtnClick
    NumGlyphs = 2
  end
end
