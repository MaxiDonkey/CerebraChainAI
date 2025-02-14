object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Promises and Thought Chains'
  ClientHeight = 672
  ClientWidth = 1096
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    1096
    672)
  TextHeight = 21
  object Button1: TButton
    Left = 8
    Top = 144
    Width = 161
    Height = 25
    Caption = 'I like fruits'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 184
    Width = 1073
    Height = 480
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Left = 192
    Top = 144
    Width = 161
    Height = 25
    Caption = 'Create document'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo2: TMemo
    Left = 8
    Top = 8
    Width = 1073
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Do works of art belong to humanity or to their creator?')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Button3: TButton
    Left = 376
    Top = 144
    Width = 161
    Height = 25
    Caption = 'Clear'
    TabOrder = 4
    OnClick = Button3Click
  end
end
