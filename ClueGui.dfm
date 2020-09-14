object ClueForm: TClueForm
  Left = 0
  Top = 0
  Caption = 'Clue helper'
  ClientHeight = 336
  ClientWidth = 578
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 16
  object buttonPanel: TPanel
    Left = 0
    Top = 269
    Width = 578
    Height = 67
    Align = alBottom
    TabOrder = 0
    OnResize = buttonPanelResize
    object progressLabel: TLabel
      Left = 120
      Top = 8
      Width = 80
      Height = 16
      Caption = 'progressLabel'
    end
    object Btn_Close: TButton
      Left = 280
      Top = 1
      Width = 297
      Height = 65
      Align = alRight
      Caption = 'Close'
      TabOrder = 0
      OnClick = Btn_CloseClick
    end
    object Btn_Update: TButton
      Left = 1
      Top = 1
      Width = 280
      Height = 65
      Align = alLeft
      Caption = 'Convert && Move'
      TabOrder = 1
      OnClick = Btn_UpdateClick
    end
    object progressConvertMove: TProgressBar
      Left = 24
      Top = 8
      Width = 150
      Height = 17
      TabOrder = 2
      Visible = False
    end
  end
  object mainPanel: TPanel
    Left = 0
    Top = 0
    Width = 578
    Height = 269
    Align = alClient
    ParentShowHint = False
    ShowHint = False
    TabOrder = 1
    ExplicitTop = 2
    DesignSize = (
      578
      269)
    object basefolderLabel: TLabel
      Left = 24
      Top = 41
      Width = 93
      Height = 16
      Caption = 'Base destination'
    end
    object cluefolderLabel: TLabel
      Left = 24
      Top = 11
      Width = 75
      Height = 16
      Caption = 'Clue-S folder'
    end
    object patternLabel: TLabel
      Left = 24
      Top = 70
      Width = 100
      Height = 16
      Caption = 'Subfolder pattern'
    end
    object startatlabel: TLabel
      Left = 24
      Top = 105
      Width = 43
      Height = 16
      Caption = 'Start at'
    end
    object domainLabel: TLabel
      Tag = 5
      Left = 24
      Top = 132
      Width = 72
      Height = 16
      Caption = 'Ilwis domain'
    end
    object georefLabel: TLabel
      Tag = 4
      Left = 24
      Top = 162
      Width = 67
      Height = 16
      Caption = 'Ilwis georef'
    end
    object recentLabel: TLabel
      Left = 24
      Top = 234
      Width = 119
      Height = 16
      Caption = 'Previous conversions'
    end
    object patternEdit: TEdit
      Left = 152
      Top = 70
      Width = 409
      Height = 21
      Hint = 'SubfolderPattern'
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      TabOrder = 0
      Text = 'scen_00000'
      OnExit = patternEditExit
    end
    object startatEdit: TEdit
      Left = 152
      Top = 105
      Width = 409
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      TabOrder = 1
      Text = '1'
      OnExit = startatEditExit
    end
    object cluefolderEdit: TButtonedEdit
      Left = 152
      Top = 11
      Width = 409
      Height = 24
      Hint = 'CluesOutputFolder'
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Images = arrowImages
      RightButton.DropDownMenu = cluefolderListMenu
      RightButton.HotImageIndex = 0
      RightButton.ImageIndex = 0
      RightButton.PressedImageIndex = 0
      RightButton.Visible = True
      TabOrder = 2
      TextHint = 'Select the folder where clues resides'
    end
    object basefolderEdit: TButtonedEdit
      Left = 152
      Top = 41
      Width = 409
      Height = 24
      Hint = 'BaseDestinationFolder'
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Images = arrowImages
      RightButton.DropDownMenu = processedFolderListMenu
      RightButton.HotImageIndex = 0
      RightButton.ImageIndex = 0
      RightButton.PressedImageIndex = 0
      RightButton.Visible = True
      TabOrder = 3
      TextHint = 'Select the root folder to store the processed scenarios'
      OnExit = patternEditExit
    end
    object ilwisDomainBEdit: TButtonedEdit
      Left = 152
      Top = 132
      Width = 409
      Height = 24
      Hint = 'IlwisDomain'
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Enabled = False
      Images = arrowImages
      RightButton.DropDownMenu = domainsMenu
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      TabOrder = 4
      TextHint = 'Select an ILWIS value domain object'
    end
    object ilwisGeorefBEdit: TButtonedEdit
      Left = 152
      Top = 162
      Width = 409
      Height = 24
      Hint = 'IlwisGeoref'
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Enabled = False
      Images = arrowImages
      RightButton.DropDownMenu = georefsMenu
      RightButton.ImageIndex = 0
      RightButton.Visible = True
      TabOrder = 5
      TextHint = 'Select an ILWIS georeference object'
    end
    object styleChooser: TComboBox
      Left = 416
      Top = 192
      Width = 145
      Height = 24
      Anchors = [akRight]
      TabOrder = 6
      Text = 'styleChooser'
      OnChange = changeStyleClick
    end
    object exploreButton: TButton
      Left = 504
      Top = 231
      Width = 57
      Height = 25
      Hint = 'Open an explorer window in the ILWIS output folder'
      Anchors = [akTop, akRight]
      Caption = 'Open'
      TabOrder = 7
      OnClick = exploreButtonClick
    end
    object historyCombobox: TComboBox
      Left = 152
      Top = 231
      Width = 346
      Height = 24
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 8
    end
    object overwriteCheckbox: TCheckBox
      Left = 24
      Top = 196
      Width = 176
      Height = 17
      Caption = 'Overwrite existing folder'
      TabOrder = 9
    end
  end
  object arrowImages: TImageList
    Left = 320
    Top = 184
    Bitmap = {
      494C010104000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D9ED
      F45DF8F9FA070000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DAEDF55D74DB
      F5FF91A5B27DC1CBD14CF8F9FA0800000000FAFAFA05C3DDE773E4E4E41BF1F1
      F10EFEFEFE010000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000087D7EEFF72DC
      F6FF879EAD868EAFBFAA7AB1C8F677C1D5FF61D4ECFF4ECFEAFF66CDE7FFBCC2
      C450DADADA25F9F9F90600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000808080000000000000000000808080000000000000000000808080000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000F8F8F8070000000000000000000000000000
      000000000000000000000000000000000000000000009DD3E7FF76DDF7FF73DC
      F6FF428BA5FF4FA2BAFF5CB8CFFF5DC0D9FF51CFEAFF4BCCE9FFE2C7A4FFA4C7
      D4A4C7C7C738E0E0E01FFEFEFE01000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C0C0C0008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000808080000000000000000000000000000000
      00000000000000000000F8F8F807000000FF505050AF00000000000000000000
      000000000000000000000000000000000000000000009BE8FAFF80E1F8FF79DE
      F7FF438CA6FF4DA2BBFF59B7D0FF5AC0DAFF53CEEAFF4FCDEAFFF5FCFEFFA9CE
      DB9ED7D7D728E0E0E01FF0F0F00FFDFDFD020000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00080808000000000000000000000000000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F8F8F807000000FF000000FF000000FF505050AF000000000000
      00000000000000000000000000000000000000000000A5ECFCFF8CE7FAFF83E3
      F8FF3F8AA6FF50A1BAFF5FB8CFFF62C3DCFF5ED0EAFF5BD0ECFFF6FCFEFFB1D8
      E396E9E9E916F1F1F10EF9F9F906FEFEFE010000000000000000000000000000
      000000000000000000000000FF000000FF000000000000000000000000000080
      00000080000000000000C0C0C000808080000000000000000000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F8F8F807000000FF171717E800000000D4D4D42B000000FF505050AF0000
      00000000000000000000000000000000000000000000AEF0FDFF99ECFBFF8FE8
      FAFF99CDE1FF53A0B7FF64B9CFFF6BC6DDFF67CDE5FF69D6EEFF71D9EFFFC1E3
      ED7CF9F9F906FEFEFE0100000000000000000000000000000000808000008080
      0000000000000000000000FFFF000000FF0000000000000000000000000000FF
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000000000000000000000000000F8F8
      F807000000FF171717E8000000000000000000000000D4D4D42B000000FF5050
      50AF0000000000000000000000000000000000000000B6F3FEFFA5F1FDFF9CED
      FCFFAAD9ECFF57A0B9FF6BBCD1FF74CADFFF72D2E8FF78DAF0FF8ED7ECFFFEFE
      FE01000000000000000000000000000000000000000000000000FFFF00008080
      0000000000000000000000000000000000000000000000000000000000000080
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F8F8F8070000
      00FF171717E80000000000000000000000000000000000000000D4D4D42B0000
      00FF505050AF00000000000000000000000000000000BAF5FFFFAEF5FEFFA9F2
      FEFFADDBEEFF5BA4BAFF73BFD3FF7DCEE2FF7DD5EAFF93E1F3FF87D7EBFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F8F8F807000000FF1717
      17E800000000000000000000000000000000000000000000000000000000D4D4
      D42B000000FF505050AF000000000000000000000000BAF5FFFFB2F7FFFFB1F6
      FFFFAFDDEFFF5FA5BCFF78C1D5FF85D1E5FF88D9EDFFA4E7F5FF87D9ECFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF00000000000000000000000000000000000000FF00FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008080800000000000C2C2C23D171717E80000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D4D4D42B0C0C0CF3000000000000000000000000BAF5FFFFB2F7FFFFB3F7
      FFFFB2DFF1FF63A7BDFF7FC4D7FF8CD5E6FF91DDEFFFB3EBF8FF86DCEEFF0000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF000000800000000000000000000000FF00FF00FF00FF00800080000000
      0000000000000000000000000000000000000000000000000000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BAF5FFFFB2F7FFFFB3F7
      FFFFB5E1F2FF6AAEC2FF86CADCFF92D9E9FF98E1F1FFB4EBF9FF85DEEFFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF00000000000000000000000000000000000000FF00FF00000000000000
      00000000000000000000000000000000000000000000000080000000FF000000
      8000000000000000000000000000000000000000000000000000000000000000
      0000000080008080800080808000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BAF5FFFFB2F7FFFFB4F7
      FFFFB7E3F4FF7ABCCFFF8ED2E2FF9DE0F0FFA3E7F7FFB4ECF8FF85E0F0FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000080000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BAF5FFFFB2F7FFFFE8FD
      FFFF9ACFE0FF8CCEDEFFA7EAF8FFADECF9FFB1ECF9FFBCEFF9FF84E1F1FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BAF5FFFFBFF7FEFF96CC
      DDFF98E4F1FFADECF7FFCCF4FCFFD8F7FCFFC3F2FAFF9FEAF6FFB6EEF7960000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B7F0FCFFBBE9F4FFA1EA
      F5FAB3EEF7ABCDF4FA66F4FCFE15000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFE7FFFFFFFFFFFFFFC107FFFFFFFF
      FFFFC003F7CF0000FEFF8001DFF3FFFEFC7F8000BFF9DFFEF83F80007CE4DFFE
      F11F8003CCE6FFFEE38F800FCFE6FFFEC7C7801FFFFEFFFE8FE3801FF7BEFFFE
      9FF3801F631FDFFEFFFF801FF7BD8FF0FFFF801FFFFBDFF9FFFF801FEFF7FFFB
      FFFF801FF83FFFFFFFFF81FFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object cluefolderListMenu: TPopupMenu
    AutoHotkeys = maManual
    Images = arrowImages
    Left = 344
    Top = 8
  end
  object mainEvents: TApplicationEvents
    OnActivate = mainEventsActivate
    Left = 384
    Top = 184
  end
  object processedFolderListMenu: TPopupMenu
    AutoHotkeys = maManual
    Images = arrowImages
    Left = 448
    Top = 40
  end
  object domainsMenu: TPopupMenu
    AutoHotkeys = maManual
    Images = arrowImages
    Left = 176
    Top = 128
  end
  object georefsMenu: TPopupMenu
    AutoHotkeys = maManual
    Images = arrowImages
    Left = 240
    Top = 160
  end
end
