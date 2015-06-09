object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Ustawienia'
  ClientHeight = 544
  ClientWidth = 568
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 256
    Top = 288
    Width = 31
    Height = 13
    Caption = 'Label3'
  end
  object PageControl: TPageControl
    Left = 0
    Top = 0
    Width = 568
    Height = 544
    ActivePage = tsMap
    Align = alClient
    MultiLine = True
    TabOrder = 0
    object tsMap: TTabSheet
      Caption = 'Mapy'
      object iShowMap: TImage
        Left = 130
        Top = 3
        Width = 420
        Height = 426
        AutoSize = True
      end
      object Label15: TLabel
        Left = 3
        Top = 376
        Width = 72
        Height = 13
        Caption = 'Mapa startowa'
      end
      object lbMaps: TListBox
        Left = 3
        Top = -1
        Width = 121
        Height = 262
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbMapsClick
      end
      object bEditMap: TButton
        Tag = 1
        Left = 3
        Top = 267
        Width = 75
        Height = 25
        Caption = 'Edytuj'
        TabOrder = 1
        OnClick = bMapButtons
      end
      object bNewMap: TButton
        Tag = 2
        Left = 3
        Top = 298
        Width = 75
        Height = 25
        Caption = 'Nowa Mapa'
        TabOrder = 2
        OnClick = bMapButtons
      end
      object cbStartMap: TComboBox
        Left = 3
        Top = 395
        Width = 108
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 3
      end
      object bDeleteMap: TButton
        Tag = 3
        Left = 3
        Top = 329
        Width = 75
        Height = 25
        Caption = 'Usu'#324
        TabOrder = 4
        OnClick = bMapButtons
      end
      object Button1: TButton
        Left = 3
        Top = 440
        Width = 75
        Height = 25
        Caption = 'Uruchom'
        TabOrder = 5
        OnClick = Button1Click
      end
    end
    object txTex: TTabSheet
      Caption = 'Textury'
      ImageIndex = 1
      object Image3: TImage
        Left = 228
        Top = 184
        Width = 329
        Height = 329
        Center = True
        Proportional = True
        Stretch = True
      end
      object Label5: TLabel
        Left = 135
        Top = 65
        Width = 85
        Height = 13
        Caption = 'Szybko'#347#263' animacji'
      end
      object lbTextures: TListBox
        Left = 3
        Top = 3
        Width = 126
        Height = 366
        ItemHeight = 13
        TabOrder = 0
        OnClick = lbTexturesClick
      end
      object bAddTex: TButton
        Tag = 1
        Left = 135
        Top = 163
        Width = 75
        Height = 25
        Caption = 'dodaj'
        TabOrder = 1
        OnClick = bAddTexClick
      end
      object bDeleteTex: TButton
        Tag = 1
        Left = 0
        Top = 402
        Width = 75
        Height = 25
        Caption = 'usun'
        TabOrder = 2
        OnClick = bDeleteTexClick
      end
      object cbTextureAnim: TCheckBox
        Left = 135
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Animuje'
        TabOrder = 3
      end
      object cbTextureLoop: TCheckBox
        Left = 135
        Top = 29
        Width = 97
        Height = 17
        Caption = 'Zap'#281'tl animacj'#281
        TabOrder = 4
      end
      object cbTextureCircle: TCheckBox
        Left = 135
        Top = 42
        Width = 97
        Height = 17
        Caption = 'Animacja ko'#322'owa'
        TabOrder = 5
      end
      object leTextureName: TLabeledEdit
        Left = 135
        Top = 128
        Width = 121
        Height = 21
        EditLabel.Width = 75
        EditLabel.Height = 13
        EditLabel.Caption = 'Nazwa tekstury'
        TabOrder = 6
      end
      object bAddTex2: TButton
        Tag = 4
        Left = 135
        Top = 194
        Width = 75
        Height = 25
        Caption = 'Dodaj 2'
        TabOrder = 7
        OnClick = bAddTexClick
      end
      object fseTextureSpeed: TFloatSpinEdit
        Left = 135
        Top = 84
        Width = 121
        Height = 22
        Increment = 0.100000001490116100
        MaxValue = 0
        MinValue = 0
        TabOrder = 8
        Precision = 3
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Obiekty'
      ImageIndex = 2
      object iObjTex: TImage
        Left = 319
        Top = 51
        Width = 238
        Height = 238
        Transparent = True
      end
      object bAddObject: TButton
        Tag = 1
        Left = 0
        Top = 363
        Width = 124
        Height = 25
        Caption = 'dodaj'
        TabOrder = 0
        OnClick = bObjectClick
      end
      object bDeleteOnject: TButton
        Tag = 3
        Left = 0
        Top = 335
        Width = 124
        Height = 25
        Caption = 'Usu'#324
        TabOrder = 1
        OnClick = bObjectClick
      end
      object ObjectsEditor: TClassPropertyEditor
        Left = 130
        Top = 27
        Width = 183
        Height = 486
        TabOrder = 2
        OnEditChange = ObjectsEditorEditChange
        OnNeedList = ObjectsEditorNeedList
      end
      object cbObjectType: TComboBox
        Tag = 4
        Left = 3
        Top = 0
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 3
        Text = 'Obiekty'
        OnChange = bObjectClick
        Items.Strings = (
          'Obiekty'
          'Potwory'
          'Pociski'
          'Bronie')
      end
      object bActObject: TButton
        Tag = 2
        Left = 0
        Top = 391
        Width = 124
        Height = 25
        Caption = 'Aktualizuj'
        TabOrder = 4
        OnClick = bObjectClick
      end
      object ObjectsList: TListView
        Tag = 5
        Left = 0
        Top = 27
        Width = 124
        Height = 302
        Columns = <>
        RowSelect = True
        TabOrder = 5
        ViewStyle = vsList
        OnClick = bObjectClick
      end
      object Button3: TButton
        Left = 3
        Top = 480
        Width = 75
        Height = 25
        Caption = 'zapisz obiekt'
        TabOrder = 6
        OnClick = Button3Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Inne'
      ImageIndex = 4
      object Label1: TLabel
        Left = 19
        Top = 3
        Width = 38
        Height = 13
        Caption = 'Czcionki'
      end
      object Label4: TLabel
        Left = 163
        Top = 3
        Width = 35
        Height = 13
        Caption = 'D'#378'wi'#281'ki'
      end
      object lbFonts: TListBox
        Left = 3
        Top = 19
        Width = 121
        Height = 214
        ItemHeight = 13
        TabOrder = 0
      end
      object lbSounds: TListBox
        Left = 147
        Top = 19
        Width = 121
        Height = 214
        ItemHeight = 13
        TabOrder = 1
      end
      object Button8: TButton
        Tag = 2
        Left = 19
        Top = 239
        Width = 75
        Height = 25
        Caption = 'Dodaj'
        TabOrder = 2
        OnClick = bAddTexClick
      end
      object Button9: TButton
        Tag = 2
        Left = 19
        Top = 270
        Width = 75
        Height = 25
        Caption = 'Usu'#324
        TabOrder = 3
        OnClick = bDeleteTexClick
      end
      object Button10: TButton
        Tag = 3
        Left = 163
        Top = 239
        Width = 75
        Height = 25
        Caption = 'Dodaj'
        TabOrder = 4
        OnClick = bAddTexClick
      end
      object Button11: TButton
        Tag = 3
        Left = 163
        Top = 270
        Width = 75
        Height = 25
        Caption = 'Usu'#324
        TabOrder = 5
        OnClick = bDeleteTexClick
      end
    end
    object Sk: TTabSheet
      Caption = 'Skrypty'
      ImageIndex = 4
      object Label2: TLabel
        Left = 95
        Top = 21
        Width = 32
        Height = 13
        Caption = 'Nazwa'
      end
      object seScriptEditor: TSynEdit
        Left = 95
        Top = 40
        Width = 462
        Height = 454
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        PopupMenu = PopupMenu1
        TabOrder = 0
        Gutter.DigitCount = 3
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.LeftOffset = 0
        Gutter.ShowLineNumbers = True
        Gutter.Width = 15
        Gutter.ZeroStart = True
        Highlighter = SynPasSyn1
        RightEdge = 44
      end
      object lbScripts: TListBox
        Tag = 3
        Left = 3
        Top = 16
        Width = 86
        Height = 225
        ItemHeight = 13
        TabOrder = 1
        OnClick = ScriptBtnClick
      end
      object bScriptAdd: TButton
        Left = 3
        Top = 247
        Width = 86
        Height = 25
        Caption = 'Dodaj'
        TabOrder = 2
        OnClick = ScriptBtnClick
      end
      object bScriptDelete: TButton
        Tag = 2
        Left = 3
        Top = 309
        Width = 86
        Height = 25
        Caption = 'Usu'#324
        TabOrder = 3
        OnClick = ScriptBtnClick
      end
      object bScriptAct: TButton
        Tag = 1
        Left = 3
        Top = 278
        Width = 86
        Height = 25
        Caption = 'Aktualizuj'
        TabOrder = 4
        OnClick = ScriptBtnClick
      end
      object eScriptName: TEdit
        Left = 133
        Top = 13
        Width = 121
        Height = 21
        TabOrder = 5
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Opcje'
      ImageIndex = 5
      object Label7: TLabel
        Left = 27
        Top = 77
        Width = 71
        Height = 13
        Caption = 'Czcionka menu'
      end
      object Label9: TLabel
        Left = 227
        Top = 13
        Width = 65
        Height = 13
        Caption = 'Muzyka menu'
      end
      object leGameName: TLabeledEdit
        Left = 19
        Top = 32
        Width = 121
        Height = 21
        EditLabel.Width = 51
        EditLabel.Height = 13
        EditLabel.Caption = 'Nazwa gry'
        TabOrder = 0
      end
      object cbMainFontName: TComboBox
        Left = 19
        Top = 96
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
      end
      object CheckListBox1: TCheckListBox
        Left = 219
        Top = 32
        Width = 121
        Height = 209
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Przedmioty'
      ImageIndex = 6
      object Label6: TLabel
        Left = 282
        Top = 13
        Width = 113
        Height = 13
        Caption = 'W'#322'a'#347'ciwo'#347#263'i przedmiotu'
      end
      object lbItems: TListBox
        Tag = 4
        Left = 3
        Top = 3
        Width = 121
        Height = 350
        ItemHeight = 13
        TabOrder = 0
        OnClick = bItemClick
      end
      object bItemAdd: TButton
        Tag = 1
        Left = 3
        Top = 352
        Width = 75
        Height = 25
        Caption = 'Dodaj'
        TabOrder = 1
        OnClick = bItemClick
      end
      object bItemAct: TButton
        Tag = 2
        Left = 3
        Top = 383
        Width = 75
        Height = 25
        Caption = 'Aktualizuj'
        TabOrder = 2
        OnClick = bItemClick
      end
      object mItemProp: TMemo
        Left = 282
        Top = 32
        Width = 263
        Height = 273
        Lines.Strings = (
          '[OnUse]')
        TabOrder = 3
      end
      object bItemDelete: TButton
        Tag = 3
        Left = 3
        Top = 414
        Width = 75
        Height = 25
        Caption = 'Usu'#324
        TabOrder = 4
        OnClick = bItemClick
      end
      object ItemEditor: TClassPropertyEditor
        Left = 130
        Top = 13
        Width = 146
        Height = 444
        TabOrder = 5
        OnNeedList = ObjectsEditorNeedList
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'All(*.bmp;*.tga;*.jpg)|*.bmp;*.tga;*.jpg'
    Options = [ofHideReadOnly, ofNoChangeDir, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofNoNetworkButton, ofEnableSizing, ofDontAddToRecent]
    Left = 536
  end
  object MainMenu1: TMainMenu
    Left = 496
    object Plik1: TMenuItem
      Caption = 'Plik'
      object Nowa: TMenuItem
        Caption = 'Nowa'
        OnClick = MainMenuClick
      end
      object Zapisz1: TMenuItem
        Tag = 1
        Caption = 'Zapisz'
        OnClick = MainMenuClick
      end
      object Zapiszjako1: TMenuItem
        Tag = 2
        Caption = 'Zapisz jako'
        OnClick = MainMenuClick
      end
      object Otwrz1: TMenuItem
        Tag = 3
        Caption = 'Otw'#243'rz'
        OnClick = MainMenuClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Zakocz1: TMenuItem
        Tag = 4
        Caption = 'Zako'#324'cz'
        OnClick = MainMenuClick
      end
    end
  end
  object OpenMapa: TOpenDialog
    Left = 376
  end
  object SaveMapa: TSaveDialog
    Left = 400
  end
  object SynPasSyn1: TSynPasSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    Left = 464
  end
  object PopupMenu1: TPopupMenu
    Left = 440
    object Dodajwydarzenie1: TMenuItem
      Caption = 'Dodaj wydarzenie'
      OnClick = AddPieceScript
    end
    object N2: TMenuItem
      Caption = '-'
      OnClick = AddPieceScript
    end
    object Wyczy1: TMenuItem
      Tag = 1
      Caption = 'Wyczy'#347#263
      OnClick = AddPieceScript
    end
  end
end
