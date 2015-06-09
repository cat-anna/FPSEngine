object MapEditor: TMapEditor
  Left = 0
  Top = 0
  Caption = 'Edytor'
  ClientHeight = 475
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnMouseWheelDown = MapViewMouseWheelDown
  OnMouseWheelUp = MapViewMouseWheelUp
  OnShow = FormShow
  DesignSize = (
    624
    475)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 436
    ActivePage = TabSheet1
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'Mapa'
      ImageIndex = 1
      OnShow = TabSheet2Show
      DesignSize = (
        616
        408)
      object MapView: TDXDraw
        Left = 3
        Top = 1
        Width = 406
        Height = 404
        AutoInitialize = True
        AutoSize = False
        Color = clBlack
        Display.FixedBitCount = False
        Display.FixedRatio = True
        Display.FixedSize = True
        Options = [doAllowReboot, doWaitVBlank, doDirectX7Mode, doHardware, doSelectDriver]
        SurfaceHeight = 401
        SurfaceWidth = 401
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        Traces = <>
        OnMouseDown = Image1MouseDown
        OnMouseMove = Image1MouseMove
        OnMouseUp = Image1MouseUp
      end
      object PageControl2: TPageControl
        Left = 415
        Top = 0
        Width = 198
        Height = 405
        ActivePage = TabSheet5
        Anchors = [akRight, akBottom]
        MultiLine = True
        TabOrder = 1
        TabStop = False
        object TabSheet3: TTabSheet
          Caption = #346'ciany/sufity/pod'#322'ogi/drzwi'
          object Label1: TLabel
            Left = 3
            Top = 208
            Width = 42
            Height = 13
            Caption = 'Tekstura'
          end
          object Image2: TImage
            Left = 0
            Top = 227
            Width = 190
            Height = 132
            Align = alBottom
            Center = True
            Proportional = True
            Stretch = True
            ExplicitWidth = 161
          end
          object SpeedButton1: TSpeedButton
            Left = 3
            Top = 0
            Width = 49
            Height = 22
            AllowAllUp = True
            GroupIndex = 1
            Caption = #347'ciany'
            OnClick = SpeedButton1Click
          end
          object SpeedButton2: TSpeedButton
            Tag = 1
            Left = 106
            Top = 0
            Width = 52
            Height = 22
            AllowAllUp = True
            GroupIndex = 1
            Caption = 'pod'#322'ogi'
            OnClick = SpeedButton1Click
          end
          object SpeedButton3: TSpeedButton
            Tag = 2
            Left = 54
            Top = 0
            Width = 52
            Height = 22
            AllowAllUp = True
            GroupIndex = 1
            Caption = 'sufity'
            OnClick = SpeedButton1Click
          end
          object SpeedButton9: TSpeedButton
            Tag = 6
            Left = 3
            Top = 23
            Width = 49
            Height = 22
            AllowAllUp = True
            GroupIndex = 1
            Caption = 'Drzwi'
            OnClick = SpeedButton1Click
          end
          object gbDoorDir: TGroupBox
            Left = 3
            Top = 51
            Width = 155
            Height = 142
            Caption = 'Kierunek drzwi'
            TabOrder = 0
            Visible = False
            object Label6: TLabel
              Left = 15
              Top = 64
              Width = 47
              Height = 26
              Caption = 'Szybko'#347#263' drzwi'
              WordWrap = True
            end
            object Label7: TLabel
              Left = 2
              Top = 96
              Width = 150
              Height = 13
              Caption = 'Czas przez jaki dzwi s'#261' otwarte'
            end
            object RadioButton7: TRadioButton
              Tag = 3
              Left = 15
              Top = 19
              Width = 17
              Height = 17
              TabOrder = 0
              OnClick = rbDoorDirChange
            end
            object RadioButton8: TRadioButton
              Tag = 1
              Left = 3
              Top = 30
              Width = 22
              Height = 17
              TabOrder = 1
              OnClick = rbDoorDirChange
            end
            object RadioButton9: TRadioButton
              Tag = 4
              Left = 15
              Top = 42
              Width = 30
              Height = 17
              TabOrder = 2
              OnClick = rbDoorDirChange
            end
            object RadioButton10: TRadioButton
              Tag = 2
              Left = 29
              Top = 30
              Width = 17
              Height = 17
              TabOrder = 3
              OnClick = rbDoorDirChange
            end
            object RadioButton11: TRadioButton
              Tag = 6
              Left = 52
              Top = 41
              Width = 60
              Height = 17
              Caption = 'W d'#243#322
              TabOrder = 4
              OnClick = rbDoorDirChange
            end
            object RadioButton12: TRadioButton
              Tag = 5
              Left = 52
              Top = 18
              Width = 55
              Height = 17
              Caption = 'W g'#243'r'#281
              Checked = True
              TabOrder = 5
              TabStop = True
              OnClick = rbDoorDirChange
            end
            object fseDoorSpeed: TFloatSpinEdit
              Left = 68
              Top = 63
              Width = 54
              Height = 22
              Increment = 0.100000001490116100
              MaxValue = 0
              MinValue = 0
              TabOrder = 6
              Value = 0.100000000000000000
              Precision = 3
            end
            object seDoorOpenTime: TSpinEdit
              Left = 35
              Top = 112
              Width = 70
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 7
              Value = 0
            end
          end
          object cbTexture: TComboBox
            Left = 51
            Top = 200
            Width = 107
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            OnChange = cbTextureChange
            OnDropDown = cbTextureDropDown
          end
        end
        object TabSheet4: TTabSheet
          Tag = 3
          Caption = 'Obiekty'
          ImageIndex = 1
          OnShow = SpeedButton1Click
          object lNewItem: TLabel
            Left = 3
            Top = 3
            Width = 79
            Height = 13
            Caption = 'Tworzony obiekt'
          end
          object cbNewObject: TComboBox
            Left = 3
            Top = 22
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnDropDown = cbNewObjectDropDown
          end
        end
        object Inne: TTabSheet
          Caption = 'Inne'
          ImageIndex = 2
          object SpeedButton6: TSpeedButton
            Tag = 4
            Left = 3
            Top = 3
            Width = 84
            Height = 22
            AllowAllUp = True
            GroupIndex = 1
            Caption = 'Punkt startowy'
            OnClick = SpeedButton1Click
          end
          object GroupBox2: TGroupBox
            Left = 0
            Top = 278
            Width = 190
            Height = 81
            Align = alBottom
            Caption = 'Przesu'#324' ca'#322#261' map'#281' w kierunku:'
            TabOrder = 0
            object SpeedButton5: TSpeedButton
              Tag = 4
              Left = 79
              Top = 36
              Width = 23
              Height = 22
              Caption = '>'
              Flat = True
              OnClick = MoveMapBtn
            end
            object SpeedButton7: TSpeedButton
              Tag = 1
              Left = 56
              Top = 16
              Width = 23
              Height = 22
              Caption = '/\'
              Flat = True
              OnClick = MoveMapBtn
            end
            object SpeedButton8: TSpeedButton
              Tag = 2
              Left = 56
              Top = 56
              Width = 23
              Height = 22
              Caption = '\/'
              Flat = True
              OnClick = MoveMapBtn
            end
            object SpeedButton4: TSpeedButton
              Tag = 3
              Left = 34
              Top = 36
              Width = 23
              Height = 22
              Caption = '<'
              Flat = True
              OnClick = MoveMapBtn
            end
          end
        end
        object TabSheet5: TTabSheet
          Tag = 5
          Caption = 'Wydarzenia'
          ImageIndex = 3
          OnShow = SpeedButton1Click
          object Label3: TLabel
            Left = 11
            Top = 3
            Width = 89
            Height = 13
            Caption = 'Spos'#243'b wywo'#322'ania'
          end
          object Label4: TLabel
            Left = 11
            Top = 49
            Width = 57
            Height = 13
            Caption = 'Wydarzenie'
          end
          object cbEvSource: TComboBox
            Tag = 1
            Left = 3
            Top = 22
            Width = 94
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnClick = cbEventClick
          end
          object cbEvent: TComboBox
            Tag = 2
            Left = 3
            Top = 68
            Width = 94
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 1
            OnClick = cbEventClick
          end
          object leTelDest: TLabeledEdit
            Tag = 3
            Left = 3
            Top = 120
            Width = 155
            Height = 21
            EditLabel.Width = 156
            EditLabel.Height = 13
            EditLabel.Caption = 'Cel teleportu(dwuklik by zmieni'#263')'
            Enabled = False
            ReadOnly = True
            TabOrder = 2
            OnDblClick = cbEventClick
          end
          object leScriptName: TLabeledEdit
            Tag = 4
            Left = 3
            Top = 160
            Width = 121
            Height = 21
            EditLabel.Width = 71
            EditLabel.Height = 13
            EditLabel.Caption = 'Nazwa skryptu'
            Enabled = False
            TabOrder = 3
            OnDblClick = cbEventClick
          end
          object lseEventId: TLabeledSpinEdit
            Tag = 5
            Left = 3
            Top = 216
            Width = 121
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 4
            Value = 0
            OnChange = cbEventClick
            EditLabel.Width = 93
            EditLabel.Height = 13
            EditLabel.Caption = 'Id tego wydarzenia'
          end
        end
        object TabSheet6: TTabSheet
          Tag = 7
          Caption = 'Przedmioty'
          ImageIndex = 4
          OnShow = SpeedButton1Click
          object Label8: TLabel
            Left = 8
            Top = 8
            Width = 47
            Height = 13
            Caption = 'Przedmiot'
          end
          object cbNewItem: TComboBox
            Left = 3
            Top = 27
            Width = 145
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
            OnDropDown = cbNewItemDropDown
          end
          object lseItemCount: TLabeledSpinEdit
            Left = 3
            Top = 80
            Width = 121
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 1
            Value = 1
            EditLabel.Width = 22
            EditLabel.Height = 13
            EditLabel.Caption = 'Ilo'#347#263
          end
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Opcje'
      ImageIndex = 1
      OnShow = TabSheet1Show
      object Label2: TLabel
        Left = 16
        Top = 105
        Width = 75
        Height = 13
        Caption = 'Zasi'#281'g widzenia'
      end
      object Label5: TLabel
        Left = 3
        Top = 333
        Width = 121
        Height = 13
        Caption = 'Skasuj z mapy wszystkie:'
      end
      object Label9: TLabel
        Left = 192
        Top = 16
        Width = 84
        Height = 13
        Caption = 'Playlista tej mapy'
      end
      object leMapName: TLabeledEdit
        Left = 15
        Top = 32
        Width = 121
        Height = 21
        EditLabel.Width = 61
        EditLabel.Height = 13
        EditLabel.Caption = 'Nazwa mapy'
        TabOrder = 0
        OnChange = leMapNameChange
      end
      object fseFD: TFloatSpinEdit
        Left = 16
        Top = 124
        Width = 64
        Height = 22
        Increment = 0.039999999105930330
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Precision = 4
        OnChange = fseFDChange
      end
      object cbFog: TCheckBox
        Left = 16
        Top = 82
        Width = 64
        Height = 17
        Caption = 'Mg'#322'a'
        TabOrder = 2
        OnClick = fseFDChange
      end
      object pFogColor: TPanel
        Left = 16
        Top = 152
        Width = 121
        Height = 57
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Caption = 'Kolor mg'#322'y'
        TabOrder = 3
        OnClick = fseFDChange
      end
      object Button2: TButton
        Left = 15
        Top = 379
        Width = 75
        Height = 25
        Caption = 'Wyczy'#347#263' map'#281
        TabOrder = 4
        OnClick = Button2Click
      end
      object cbDeleteFormMap: TComboBox
        Left = 16
        Top = 352
        Width = 113
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 5
        Text = 'Wszystko'
        Items.Strings = (
          'Wszystko'
          #346'ciany'
          'Sufity'
          'Pod'#322'ogi'
          'Obiekty'
          'Wydarzenia'
          'Drzwi')
      end
      object CheckListBox1: TCheckListBox
        Left = 176
        Top = 35
        Width = 129
        Height = 262
        ItemHeight = 13
        TabOrder = 6
      end
    end
  end
  object Button1: TButton
    Left = 542
    Top = 445
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button3: TButton
    Left = 7
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Testuj map'#281
    TabOrder = 2
  end
  object Button4: TButton
    Left = 88
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Zapisz'
    TabOrder = 3
    OnClick = Button4Click
  end
  object ColorDialog1: TColorDialog
    Color = clGray
    Left = 544
  end
  object DXImageList1: TDXImageList
    DXDraw = MapView
    Items.ColorTable = {
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
      0000000000000000000000000000000000000000000000000000000000000000}
    Items = <
      item
        Name = 'player'
        PatternHeight = 0
        PatternWidth = 0
        Picture.Data = {
          045444494228000000070000000700000001000100000000001C000000000000
          0000000000000000000000000000000000FFFFFF00FF040000EF006800C7006C
          008300620000007200FF000000FE000000}
        SystemMemory = False
        Transparent = True
        TransparentColor = clWhite
      end>
    Left = 512
  end
end
