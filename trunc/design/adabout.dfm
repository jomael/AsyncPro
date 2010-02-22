object ApdAboutForm: TApdAboutForm
  Left = 428
  Top = 372
  BorderStyle = bsDialog
  Caption = 'About TurboPower Async Professional'
  ClientHeight = 312
  ClientWidth = 396
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 6
    Top = 265
    Width = 387
    Height = 17
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 150
    Top = 8
    Width = 134
    Height = 16
    Caption = 'Async Professional'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 150
    Top = 25
    Width = 190
    Height = 13
    Caption = 'Version 5.00 (auto updated in OnCreate)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label11: TLabel
    Left = 7
    Top = 273
    Width = 253
    Height = 13
    Caption = 'Copyright (c) 1991-xxxx, TurboPower AsycPro authors'
  end
  object Label12: TLabel
    Left = 7
    Top = 289
    Width = 96
    Height = 13
    Caption = 'All Rights Reserved.'
  end
  object Label3: TLabel
    Left = 150
    Top = 48
    Width = 93
    Height = 13
    Caption = 'Released under the'
  end
  object Label9: TLabel
    Left = 248
    Top = 47
    Width = 122
    Height = 13
    Caption = 'Mozilla Public License 1.1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label9Click
    OnMouseDown = Label5MouseUp
    OnMouseUp = Label5MouseUp
  end
  object Label10: TLabel
    Left = 150
    Top = 80
    Width = 201
    Height = 13
    Caption = 'Async Profession can be downloaded from'
  end
  object Label13: TLabel
    Left = 160
    Top = 95
    Width = 182
    Height = 13
    Caption = 'http://sourceforge.net/projects/tpapro'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label13Click
    OnMouseDown = Label5MouseUp
    OnMouseUp = Label5MouseUp
  end
  object Label14: TLabel
    Left = 150
    Top = 119
    Width = 169
    Height = 13
    Caption = 'Support information is available from'
  end
  object Label15: TLabel
    Left = 160
    Top = 134
    Width = 182
    Height = 13
    Caption = 'http://sourceforge.net/projects/tpapro'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label13Click
    OnMouseDown = Label5MouseUp
    OnMouseUp = Label5MouseUp
  end
  object Label4: TLabel
    Left = 150
    Top = 158
    Width = 197
    Height = 13
    Caption = 'Visit the Async Professional home page at'
  end
  object Label6: TLabel
    Left = 160
    Top = 173
    Width = 210
    Height = 13
    Caption = 'https://sourceforge.net/projects/tpapro/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label6Click
    OnMouseDown = Label5MouseUp
    OnMouseUp = Label5MouseUp
  end
  object Label5: TLabel
    Left = 150
    Top = 200
    Width = 111
    Height = 13
    Caption = 'Online help newsgroup:'
  end
  object Label7: TLabel
    Left = 160
    Top = 215
    Width = 224
    Height = 13
    Caption = 'http://sourceforge.net/forum/?group_id=71007'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label7Click
    OnMouseDown = Label5MouseUp
    OnMouseUp = Label5MouseUp
  end
  object Panel1: TPanel
    Left = 6
    Top = 6
    Width = 139
    Height = 251
    BevelOuter = bvLowered
    TabOrder = 0
  end
  object Button1: TButton
    Left = 298
    Top = 277
    Width = 88
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = Button1Click
  end
end
