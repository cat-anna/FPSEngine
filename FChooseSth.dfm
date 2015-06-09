object ChooseForm: TChooseForm
  Left = 0
  Top = 0
  Caption = 'Wybierz'
  ClientHeight = 421
  ClientWidth = 394
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
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  DesignSize = (
    394
    421)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 394
    Height = 385
    ActivePage = TabSheet1
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    ExplicitWidth = 454
    object TabSheet1: TTabSheet
      Caption = 'Cel teleportacji'
      ExplicitLeft = 8
      ExplicitTop = 28
      object Label1: TLabel
        Left = 3
        Top = 3
        Width = 68
        Height = 13
        Caption = 'Wybierz map'#281
      end
      object DXDraw: TDXDraw
        Left = 0
        Top = 23
        Width = 386
        Height = 334
        AutoInitialize = True
        AutoSize = True
        Color = clBlack
        Display.FixedBitCount = False
        Display.FixedRatio = True
        Display.FixedSize = True
        Options = [doAllowReboot, doWaitVBlank, doCenter, do3D, doDirectX7Mode, doHardware, doSelectDriver]
        SurfaceHeight = 334
        SurfaceWidth = 386
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 0
        Traces = <>
        OnMouseMove = DXDraw1MouseMove
        OnMouseUp = DXDrawMouseUp
        ExplicitTop = 27
      end
      object cbTeleMap: TComboBox
        Left = 88
        Top = 0
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = cbTeleMapChange
      end
    end
  end
  object Button1: TButton
    Left = 311
    Top = 391
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 371
  end
  object Button2: TButton
    Left = 230
    Top = 391
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Anuluj'
    ModalResult = 2
    TabOrder = 2
    ExplicitLeft = 290
  end
end
