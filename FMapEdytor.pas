unit FMapEdytor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Buttons, ExtCtrls, FPSEngine, LabeledSpinEdit,
  ComCtrls, FloatSpinEdit, DXDraws, DIB, CheckLst;

type
  TEditingMode = (emWall, emFloor, emCeiling, emObject, emStartPoint, emEvent,
                  emDoor, emItem);
  TData = class
  public
    Typ: TEditingMode;
    DrawPosition: TDrawPosition;
    Collision, Horizontal, Selected: bool;
    Position: T3Dpoint;
    ItemName, TextureName: string;
    sx, sy, DoorDir, DoorOpenTime, ItemCount: integer;
    EventData: TEventData;
    EventSource: TFPSEventSource;
    EventType: TFPSEventType;
    DoorSpeed: Single;
  end;
  PData = ^TData;

  TMapEditor = class(TForm)
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    leMapName: TLabeledEdit;
    fseFD: TFloatSpinEdit;
    cbFog: TCheckBox;
    pFogColor: TPanel;
    MapView: TDXDraw;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    ColorDialog1: TColorDialog;
    DXImageList1: TDXImageList;
    Button2: TButton;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    Label1: TLabel;
    Image2: TImage;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TabSheet4: TTabSheet;
    lNewItem: TLabel;
    cbNewObject: TComboBox;
    Inne: TTabSheet;
    SpeedButton6: TSpeedButton;
    GroupBox2: TGroupBox;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    TabSheet5: TTabSheet;
    cbEvSource: TComboBox;
    cbEvent: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    leTelDest: TLabeledEdit;
    leScriptName: TLabeledEdit;
    Label5: TLabel;
    cbDeleteFormMap: TComboBox;
    SpeedButton9: TSpeedButton;
    gbDoorDir: TGroupBox;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    RadioButton9: TRadioButton;
    RadioButton10: TRadioButton;
    RadioButton11: TRadioButton;
    RadioButton12: TRadioButton;
    fseDoorSpeed: TFloatSpinEdit;
    Label6: TLabel;
    Label7: TLabel;
    seDoorOpenTime: TSpinEdit;
    cbTexture: TComboBox;
    TabSheet6: TTabSheet;
    cbNewItem: TComboBox;
    Label8: TLabel;
    lseItemCount: TLabeledSpinEdit;
    CheckListBox1: TCheckListBox;
    Label9: TLabel;
    lseEventId: TLabeledSpinEdit;
    procedure TabSheet1Show(Sender: TObject);
    procedure cbNewItemDropDown(Sender: TObject);
    procedure cbTextureChange(Sender: TObject);
    procedure cbTextureDropDown(Sender: TObject);
    procedure rbDoorDirChange(Sender: TObject);
    procedure cbEventClick(Sender: TObject);
    procedure MoveMapBtn(Sender: TObject);
    procedure cbNewObjectDropDown(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure MapViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MapViewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormHide(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure fseFDChange(Sender: TObject);
    procedure leMapNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState;
      mX, mY: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
  private
    IEnded: bool;
    EventData: TEventData;
    DoorDirection: byte;
    procedure DrawOnMap(Data: TData; Typ: TEditingMode; sX, sY: integer;
      Selected: bool = false; Horizontal: bool = false; Deleting: bool = false;
      Focused: bool = false);
  public
    Tryb: TEditingMode;
    Clicked, NewHorizontal: bool;
    Selected: TData;
    Mapa: TList;

    FPSMap: TFPSMap;

    MapSnapShot: TDIB;

    function GetObjectAt(ax, ay: integer; rObj: PData): bool;
    procedure IPaint;
    procedure RefreshMap;
    procedure SetMap(aMap: TFPSMap);
  end;

const
  girdSize = 10;
  girdZoom = 1 / girdSize;
  ObjectSize = 2;

  WallHeight = 1.5;

  FloorColor = $00FFFFC8;
  CeilingColor = $009FCFFF;

  EMText: array[TEditingMode] of string = ('Œciana', 'Pod³oga', 'Sufit', 'Obiekt',
     'StartPoint', 'Wydarzenie', 'Drzwi', 'Przedmiot');
  ETText = 'Teleportuj'#13#10'Uruchom skrypt';
  ESTest = 'Kolizja z graczem'#13#10'"U¿ycie"';

  AngleFacor = 360 / 255;

implementation

uses Utils, math, FMainForm, jpeg, Types, FChooseSth;

{$R *.dfm}

function SortData(Item1, Item2: Pointer): integer;
var D1, D2: TData;
  function EM2Int(E: TEditingMode): integer;
  begin
    case E of
      emWall: EM2Int := 2;
      emFloor: EM2Int := 1;
      emCeiling: EM2Int := 1;
      emObject: EM2Int := 3;
      emStartPoint: EM2Int := 0;
      emEvent: EM2Int := 5;
      emDoor: EM2Int := 4;
      emItem: EM2Int := 3;
      else EM2Int := -1;
    end;
  end;
begin
  D1 := TData(Item1);
  D2 := TData(Item2);

  Result := EM2Int(d1.typ) - EM2Int(D2.typ);
  if Result = 0 then
    Result := (D1.sx + D1.sy) - (D2.sx + D2.sy);
end;

procedure TMapEditor.leMapNameChange(Sender: TObject);
begin
  FPSMap.MapName := leMapName.Text;
end;

procedure TMapEditor.MapViewMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Tryb <> emStartPoint then exit;
  FPSMap.StartPoint.Y := FPSMap.StartPoint.Y + 10;
  if FPSMap.StartPoint.Y > 360 then
    FPSMap.StartPoint.Y := FPSMap.StartPoint.Y - 360;
  IPaint;
end;

procedure TMapEditor.MapViewMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Tryb <> emStartPoint then exit;  
  FPSMap.StartPoint.Y := FPSMap.StartPoint.Y - 10;
  if FPSMap.StartPoint.Y < 0 then
    FPSMap.StartPoint.Y := FPSMap.StartPoint.Y + 360;
  IPaint;
end;

procedure TMapEditor.MoveMapBtn(Sender: TObject);
const MoveArray: array[1..4] of TPoint = ((X: 0; Y: -1;), (X: 0; Y: 1;),
                                          (X: -1; Y: 0;), (X: 1; Y: 0;));
var
  Offset: TPoint;
  i: Integer;
begin
  Offset := MoveArray[TControl(Sender).Tag];
  FPSMap.StartPoint := Sum3DPoints(FPSMap.StartPoint, Make3DPoint(Offset.X, 0, Offset.Y));
  for i := 0 to Mapa.Count - 1 do
  with TData(Mapa[i]) do
  begin
    sx := sx + Offset.X * girdSize;
    sy := sy + Offset.Y * girdSize;
    Position := Sum3DPoints(Position, Make3DPoint(-Offset.X, 0, -Offset.Y));
    with DrawPosition do
    begin
      V1 := Sum3DPoints(V1, Make3DPoint(-Offset.X, 0, -Offset.Y));
      V2 := Sum3DPoints(V2, Make3DPoint(-Offset.X, 0, -Offset.Y));
      V3 := Sum3DPoints(V3, Make3DPoint(-Offset.X, 0, -Offset.Y));
      V4 := Sum3DPoints(V4, Make3DPoint(-Offset.X, 0, -Offset.Y));
    end;
  end;
  IPaint;
end;

procedure TMapEditor.rbDoorDirChange(Sender: TObject);
begin
  DoorDirection := TControl(Sender).Tag;
end;

procedure TMapEditor.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ax, ay: integer;
    Data: TData;
begin
  clicked := Button = mbLeft;

  if Tryb in [emWall, emFloor, emCeiling] then
  begin
    ax := (x div girdSize) * girdSize;
    ay := (y div girdSize) * girdSize;
  end else
  begin
    ax := x;
    ay := y;
  end;

  if GetObjectAt(aX, aY, @data) then
  begin  
    if ssCtrl in Shift then
    begin
      Mapa.Remove(Data);
      Data.Free;
    end else
    begin
      if Selected <> nil then Selected.Selected := false;
      Selected := Data;
      Data.Selected := true;
    end;
    IPaint;
  end;

  if(Clicked)and(Tryb = emEvent)then
    EventData.EventArea.LeftTop := Make3DPoint(-X / girdSize, 0, -Y / girdSize);
end;

procedure TMapEditor.Image1MouseMove(Sender: TObject; Shift: TShiftState; mX,  mY: Integer);
var dx, dy: integer;
    Data: TData;
    px, py: integer;
    Skip: bool;
    R:TRect;
begin
  if not IEnded then exit;
  IEnded := false;
  Data := nil;

  MapView.PasteImage(MapSnapShot, 0, 0);
  Application.ProcessMessages;
  dx := (mx div girdSize) * girdSize;
  dy := (my div girdSize) * girdSize;

  if Tryb in [emObject, emStartPoint] then
  begin
    px := mX;
    py := mY;
  end else
  begin
    px := (mX div girdSize) * girdSize;
    py := (mY div girdSize) * girdSize;
  end;   

  if(py = 0)or(px = 0)then
  begin
    IEnded := true;
    exit;
  end;

  Skip := GetObjectAt(px, py, @Data);

  if Data <> nil then
  with Data do
  begin
    DrawOnMap(Data, Typ, sx, sy, Selected, Horizontal, ssCtrl in Shift, true);
    MapView.Flip;
    IEnded := true;
    exit;
  end else
  if ssCtrl in Shift then
  begin
    IEnded := true;
    exit;
  end;

  case Tryb of
    emWall, emFloor, emCeiling, emObject, emDoor, emItem:
    begin
      DrawOnMap(nil, Tryb, px, py, false, NewHorizontal, ssCtrl in Shift);
      MapView.Flip;
    end;
    emStartPoint:
    begin
      MapView.Surface.Canvas.Release;
      with DXImageList1.Items.Find('player') do
        DrawRotate(MapView.Surface, round(mx - Width / 2), round(my - Height / 2), Width, Height, 0,
        0.5, 0.5, FPSMap.StartPoint.Y / AngleFacor);
    end;
    emEvent:;
  end;
  
  if(not clicked)or(ssCtrl in Shift)then
  begin
    IEnded := true;
    exit;
  end;

  if not Skip then
  case Tryb of
    emWall: //dodaj œcianê
    begin 
      Data := TData.Create;
      Mapa.Add(Data);
      with Data do
      begin
        sx := px;
        sy := py;
        Horizontal := NewHorizontal;
        Typ := Tryb;
        Collision := true;
        TextureName := cbTexture.Text;
        DrawPosition := MakeDrawPosition(-(dx * girdZoom), 0, -(dy * girdZoom));
        with DrawPosition do
        begin
          V3.Y := V3.Y + WallHeight;
          if NewHorizontal then
          begin
            V1.Z := V1.Z - 1;
            V4 := Sum3DPoints(V4, Make3DPoint(0, WallHeight, -1));
          end else
          begin
            V1.X := V1.X - 1;
            V4 := Sum3DPoints(V4, Make3DPoint(-1, WallHeight, 0));
          end;
        end;
      end;
    end;
    emFloor: //dodaj pod³ogê
    begin     
      Data := TData.Create;
      Mapa.Add(Data);
      with Data do
      begin
        sx := px;
        sy := py;
        typ := Tryb;
        Collision := false;
        TextureName := cbTexture.Text;
        DrawPosition := MakeDrawPosition(-(dx * girdZoom)-1, 0, -(dy * girdZoom)-1);
        with DrawPosition do
        begin
          V1 := Sum3DPoints(V1, Make3DPoint(1, 0, 1));
          V2.Z := V2.Z + 1;
          V4.X := V4.X + 1;
        end;
      end;
    end;
    emCeiling: //dodaj sufit
    begin   
      Data := TData.Create;
      Mapa.Add(Data);
      with Data do
      begin
        sx := px;
        sy := py;
        typ := Tryb;
        Collision := false;
        TextureName := cbTexture.Text;
        DrawPosition := MakeDrawPosition(-(dx * girdZoom)-1, WallHeight, -(dy * girdZoom)-1);
        with DrawPosition do
        begin
          V1 := Sum3DPoints(V1, Make3DPoint(1, 0, 1));
          V2.Z := V2.Z + 1;
          V4.X := V4.X + 1;
        end;
      end;
    end;
    emObject: //dodaj objekt
    begin
      if cbNewObject.ItemIndex = -1 then exit;
      Data := TData.Create;
      Mapa.Add(Data);
      with Data do
      begin
        sx := px;
        sy := py;
        typ := Tryb;
        Position.X := -px * girdZoom;
        Position.Z := -py * girdZoom;
        ItemName := cbNewObject.Text;
      end;
    end;
    emStartPoint:
    begin
      FPSMap.StartPoint.X := -px * girdZoom;
      FPSMap.StartPoint.Z := -py * girdZoom;
    end;
    emEvent:
    with MapView.Surface.Canvas, MapView do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      R := Rect(Round(-EventData.EventArea.LeftTop.X * girdSize), Round(-EventData.EventArea.LeftTop.Z * girdSize), mX, mY);
      Rectangle(R);
      Release;
      Flip;
      IEnded := true;
      Exit;
    end;
    emDoor:
    begin
      Data := TData.Create;
      Mapa.Add(Data);
      with Data do
      begin
        sx := px;
        sy := py;
        Horizontal := NewHorizontal;
        Typ := Tryb;
        Collision := true;
        TextureName := cbTexture.Text;
        DoorDir := DoorDirection;
        DoorSpeed := fseDoorSpeed.Value;             
        DrawPosition := MakeDrawPosition(-(dx * girdZoom), 0, -(dy * girdZoom));
        DoorOpenTime := seDoorOpenTime.Value;
        with DrawPosition do
        begin
          V3.Y := V3.Y + WallHeight;
          if NewHorizontal then
          begin
            V1.Z := V1.Z - 1;
            V4 := Sum3DPoints(V4, Make3DPoint(0, WallHeight, -1));
          end else
          begin
            V1.X := V1.X - 1;
            V4 := Sum3DPoints(V4, Make3DPoint(-1, WallHeight, 0));
          end;
        end;
      end;
    end;
    emItem:
    begin
      if cbNewItem.ItemIndex = -1 then exit;
      Data := TData.Create;
      Mapa.Add(Data);
      with Data do
      begin
        sx := px;
        sy := py;
        typ := Tryb;
        Position.X := -px * girdZoom;
        Position.Z := -py * girdZoom;
        ItemName := cbNewItem.Text;
        ItemCount := lseItemCount.Value;
      end;
    end;
  end;
  Mapa.Sort(SortData);
  IPaint;
  IEnded := true;
end;

procedure TMapEditor.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var AData: TData;
begin
  if Button = mbRight then NewHorizontal := not NewHorizontal;
  if Button = mbLeft then
  begin
     Image1MouseMove(nil, shift, X, Y);
     clicked := false;

     if Tryb = emEvent then
     begin
       if(cbEvSource.ItemIndex = -1)or(cbEvent.ItemIndex = -1) then
       begin
         IPaint;
         exit;
       end;
       EventData.EventArea.Size := GetVector(Make3DPoint(-X / girdSize, 0, -Y / girdSize), EventData.EventArea.LeftTop);
       AData := TData.Create;
       Mapa.Add(AData);
       AData.Typ := emEvent;
       AData.EventData := EventData;
       AData.EventSource := TFPSEventSource(cbEvSource.ItemIndex);
       AData.EventType := TFPSEventType(cbEvent.ItemIndex);
       IPaint;
     end;
  end;
end;

procedure TMapEditor.DrawOnMap(Data: TData; Typ: TEditingMode; sX, sY: integer;
  Selected: bool = false; Horizontal: bool = false; Deleting: bool = false;
  Focused: bool = false);
var BColor: TColor;
begin
  with MapView.Surface.Canvas do
  begin
    Brush.Style := bsSolid;
    case typ of
      emWall:
      begin
        if Deleting then Brush.Color := clRed
        else if Focused then Brush.Color := clLime           
        else if Selected then Brush.Color := clGreen
        else Brush.Color := clWhite;
        Pen.Color := clBlack;
        if Horizontal then Rectangle(sx - 2, sy, sx + 2, sy + girdSize + 1)
        else Rectangle(sx, sy - 2,sx + girdSize + 1, sy + 2);
      end;
      emFloor:
      begin
        if Deleting then Brush.Color := clRed
        else if Focused then Brush.Color := clLime
        else if Selected then Brush.Color := clGreen
        else Brush.Color := FloorColor;
        Pen.Color := Brush.Color;
        Rectangle(sx + 1, sy + 1, sx + girdSize div 2, sy + girdSize);
      end;
      emCeiling:
      begin
        if Deleting then Brush.Color := clRed
        else if Focused then Brush.Color := clLime
        else if Selected then Brush.Color := clGreen
        else Brush.Color := CeilingColor;
        Pen.Color := Brush.Color;
        Rectangle(sx + girdSize div 2, sy + 1, sx + girdSize, sy + girdSize);
      end;
      emObject:
      begin
        if Deleting then Brush.Color := clRed
        else if Focused then Brush.Color := clLime
        else if Selected then Brush.Color := clGreen
        else Brush.Color := clFuchsia;
        Pen.Color := Brush.Color;
        Rectangle(sx - ObjectSize, sy - ObjectSize, sx + ObjectSize, sy + ObjectSize);
      end;
      emEvent:
      begin
        if Data = nil then exit;
        if Deleting then BColor := clRed
        else if Focused then BColor := clLime
        else if Selected then BColor := clGreen
        else BColor := clBlue;
        with Data.EventData.EventArea do
        MapView.Surface.FillRectAlpha(Rect(Round(-LeftTop.X * girdSize),
            Round(-LeftTop.Z * girdSize),  Round(-(LeftTop.X + Size.X) * girdSize),
            Round(-(LeftTop.Z + Size.Z) * girdSize)), BColor, 50);
      end;
      emDoor:
      begin
        if Deleting then Brush.Color := clRed
        else if Focused then Brush.Color := clLime
        else if Selected then Brush.Color := clGreen
        else Brush.Color := clWhite;
        Pen.Color := clBlack;
        if Horizontal then Ellipse(sx - 2, sy, sx + 2, sy + girdSize + 1)
        else Ellipse(sx, sy - 2,sx + girdSize + 1, sy + 2);
      end;
      emItem:
      begin
        if Deleting then Brush.Color := clRed
        else if Focused then Brush.Color := clLime
        else if Selected then Brush.Color := clGreen
        else Brush.Color := clTeal;
        Pen.Color := Brush.Color;
        Rectangle(sx - ObjectSize, sy - ObjectSize, sx + ObjectSize, sy + ObjectSize);
      end;
    end;
    Release;
  end;
end;

procedure TMapEditor.IPaint;
var i: integer;
begin
  MapView.Surface.Fill(MapView.Surface.ColorMatch(clWhite));
  with MapView.Surface.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Pen.Color := clSilver;
    FillRect(MapView.ClientRect);
    for i := 0 to MapView.ClientWidth div girdSize do
    begin
      MoveTo(i * girdSize, 0);
      LineTo(i * girdSize, MapView.ClientHeight);
    end;

    for i := 0 to MapView.ClientHeight div girdSize do
    begin
      MoveTo(0, i * girdSize);
      LineTo(MapView.ClientWidth, i * girdSize);
    end;;

    for I := 0 to Mapa.Count - 1 do
    with TData(Mapa[i]) do
      DrawOnMap(TData(Mapa[i]), Typ, sx, sy, Selected, Horizontal);

    Brush.Color := clBlack;
    Pen.Color := clBlack;
    Release;
  end;

  with DXImageList1.Items.Find('player') do
    DrawRotate(MapView.Surface, round(-FPSMap.StartPoint.X / girdZoom - Width / 2),
       round(-FPSMap.StartPoint.Z / girdZoom - Height / 2), Width + 1, Height + 1, 0,
       0.5, 0.5, FPSMap.StartPoint.Y / AngleFacor);

  MapView.Flip;  
  MapView.GrabImage(0, 0, MapView.ClientWidth, MapView.ClientHeight, MapSnapShot);
  Application.ProcessMessages;
end;

procedure TMapEditor.SpeedButton1Click(Sender: TObject);
begin
  Tryb := TEditingMode((Sender as TControl).Tag);
  gbDoorDir.Visible := Tryb = emDoor;
end;

procedure TMapEditor.TabSheet1Show(Sender: TObject);
var i: integer;
begin
  CheckListBox1.Items.Clear;
  for i := 0 to MainForm.lbSounds.Items.Count - 1 do
    CheckListBox1.Items.Add(GetFileNameNoExt(MainForm.lbSounds.Items[i]));



//  CheckListBox1.
end;

procedure TMapEditor.TabSheet2Show(Sender: TObject);
begin
  IPaint;
end;

procedure TMapEditor.Button1Click(Sender: TObject);
begin
  hide;
end;

procedure TMapEditor.Button2Click(Sender: TObject);
var i: integer;
    ToDel: set of TEditingMode;
const DelArray: array[0..6] of set of TEditingMode = ([emWall, emFloor, emCeiling, emObject, emEvent, emDoor], [emWall], [emFloor], [emCeiling], [emObject], [emEvent], [emDoor]);
begin
  if cbDeleteFormMap.ItemIndex = -1 then exit;
  if MessageDlg('Jesteœ pewien??', mtConfirmation, mbYesNo, 0) <> mrYes then exit;

  ToDel := DelArray[cbDeleteFormMap.ItemIndex];

  i := 0;
  while i < Mapa.Count do
  begin
    if TData(Mapa[i]).Typ in ToDel then
    begin
      TData(Mapa[i]).Free;
      Mapa.Delete(i);
    end else
      Inc(i);
  end;

  FPSMap.Owner.Update;
  IPaint;
end;

procedure TMapEditor.Button4Click(Sender: TObject);
begin
  RefreshMap;
  FPSMap.Owner.Update;
end;

procedure TMapEditor.cbEventClick(Sender: TObject);
var T: integer;
begin
  T := TControl(Sender).Tag;
  case T of
    1:begin//Ÿród³o
      end;
    2:begin//wydarzenie
        leTelDest.Enabled := cbEvent.ItemIndex = 0;
        leScriptName.Enabled := cbEvent.ItemIndex = 1;
      end;
    3:begin//zmieñ cel teleportu
        with TChooseForm.Create(Self) do
        begin
          PrepereTeleportDestSelect(DXImageList1.Items.Find('player'));
          if ShowModal = mrOk then
          begin
            EventData.DPoint := TeleportDest;
            EventData.DString := DString;
            leTelDest.Text := EventData.DString + Format('(x: %f; y:%f)', [EventData.DPoint.X, EventData.DPoint.Z]);
          end;
          Free;
        end;
      end;
    4:begin//leCsriptName.OnClick;

      end;
    5:begin
        try
          lseEventId.Value := abs(lseEventId.Value);
        except
          lseEventId.Value := 0;
        end;
        EventData.Id := lseEventId.Value;
      end;
  end;
end;

procedure TMapEditor.cbNewItemDropDown(Sender: TObject);
var i: integer;
begin
  cbNewItem.Items.Clear;
  for i := 0 to FPSMap.Owner.StaticObjects.Count - 1 do
    if TFPSBasicSprite(FPSMap.Owner.StaticObjects[i]) is TFPSBasicItem then
      cbNewItem.Items.Add(TFPSBasicItem(FPSMap.Owner.StaticObjects[i]).Name);
end;

procedure TMapEditor.cbNewObjectDropDown(Sender: TObject);
var i: integer;
begin
  cbNewObject.Items.Clear;
  for i := 0 to FPSMap.Owner.StaticObjects.Count - 1 do
    cbNewObject.Items.Add(TFPSBasicSprite(FPSMap.Owner.StaticObjects[i]).Name);
end;

procedure TMapEditor.cbTextureChange(Sender: TObject);
begin
  LoadTexture(cbTexture.Text, FPSMap.Owner, Image2);
end;

procedure TMapEditor.cbTextureDropDown(Sender: TObject);
var i: integer;
begin
  cbTexture.Items.Clear;
  for i := 0 to FPSMap.Owner.Textures.Count - 1 do
    cbTexture.Items.Add(ChangeFileExt(FPSMap.Owner.Textures[i], ''));
end;

procedure TMapEditor.FormCreate(Sender: TObject);
begin
  Mapa := TList.Create;
  MapView.Initialize;
  MapSnapShot := TDIB.Create;
  IEnded := true;
  cbEvSource.Items.Text := ESTest;
  cbEvent.Items.Text := ETText;
  DoorDirection := 5;
end;

procedure TMapEditor.FormDestroy(Sender: TObject);
var i: integer;
begin
  MapView.Finalize;
  MapSnapShot.Free;
  for i := 0 to Mapa.Count - 1 do TData(Mapa[i]).Free;
  Mapa.Clear;
  Mapa.Free;
end;

procedure TMapEditor.FormHide(Sender: TObject);
begin
  Button4.Click;
end;

procedure TMapEditor.FormShow(Sender: TObject);
begin
  leMapName.Text := FPSMap.MapName;
  PageControl1.TabIndex := 0;
  fseFD.OnChange := nil;
  cbFog.OnClick := nil;
  pFogColor.OnClick := nil;
  fseFD.Value := FPSMap.SeeRange;
  cbFog.Checked := FPSMap.Fog;
  pFogColor.Color := RGB(Round(FPSMap.FogColor[0] * 255), Round(FPSMap.FogColor[1] * 255), Round(FPSMap.FogColor[2] * 255));
  fseFD.OnChange := fseFDChange;
  cbFog.OnClick := fseFDChange;
  pFogColor.OnClick := fseFDChange;
end;

procedure TMapEditor.fseFDChange(Sender: TObject);
begin
  if Sender is TPanel then
  begin
    ColorDialog1.Color := pFogColor.Color;
    if ColorDialog1.Execute then pFogColor.Color := ColorDialog1.Color;
  end;
  fseFD.Value := Abs(fseFD.Value);
  FPSMap.Fog := cbFog.Checked;
  FPSMap.SeeRange := fseFD.Value;
  FPSMap.FogColor[0] := GetRValue(pFogColor.Color) / 255;
  FPSMap.FogColor[1] := GetGValue(pFogColor.Color) / 255;
  FPSMap.FogColor[2] := GetBValue(pFogColor.Color) / 255;
  FPSMap.FogColor[3] := 1.0;
  pFogColor.Font.Color := RGB(255 - GetRValue(pFogColor.Color), 255 - GetGValue(pFogColor.Color), 255 - GetBValue(pFogColor.Color));
end;

function TMapEditor.GetObjectAt(ax, ay: integer; rObj: PData): bool;
var i: integer;
begin
  Result := false;
  for i := 0 to Mapa.Count - 1 do
  with TData(Mapa[i]) do
  if Tryb = Typ then  
  case Tryb of
    emWall, emFloor, emCeiling:
    if(sx = ax)and(sy = ay)and(typ = Tryb)then
    begin
      if(tryb = emWall)and(NewHorizontal <> Horizontal) then Continue;      
      Result := true;
      rObj^ := TData(Mapa[i]);
      Exit;
    end;
    emObject, emItem:
    if IsRectCollision(Rect(sx - ObjectSize, sy - ObjectSize, sx + ObjectSize, sy + ObjectSize), Make3DPoint(aX, 0, aY)) then
    begin
      Result := true;
      rObj^ := TData(Mapa[i]);
      Exit;
    end;
    emStartPoint: ;
    emEvent:
    with TData(Mapa[i]).EventData.EventArea do
    if IsRectCollision(Rect(Round(-LeftTop.X * girdSize),
          Round(-LeftTop.Z * girdSize),  Round(-(LeftTop.X + Size.X) * girdSize),
          Round(-(LeftTop.Z + Size.Z) * girdSize)), Make3DPoint(aX, 0, aY)) then
    begin
      Result := true;
      rObj^ := TData(Mapa[i]);
      Exit;
    end;
  end;
end;

procedure TMapEditor.RefreshMap();
var
  i: integer;
  FPSBO: TFPSBasicSprite;
  FPSBD: TFPSBasicDoor;
begin
  with FPSMap do
  begin
    Clear;
    for i := 0 to CheckListBox1.Items.Count - 1 do
      if CheckListBox1.Checked[i] then
        FPSMap.PlayList.Add(CheckListBox1.Items[i]);
    for i := 0 to Mapa.Count - 1 do
    with TData(Mapa[i]) do
    begin
      case Typ of
        emWall, emFloor, emCeiling:
        begin
          FPSBO := TFPSBasicSprite.Create(nil, FPSMap);
          FPSBO.DrawPosition := DrawPosition;
          FPSBO.TextureName := TextureName;
          FPSBO.Collision := Collision;
          FPSMap.Objects.Add(FPSBO);
        end;
        emObject: FPSMap.CreateObject(ItemName, Position);
        emEvent: FPSMap.CrerateEventField(EventSource, EventType, EventData);
        emDoor:
        begin
          FPSBD := TFPSBasicDoor.Create(nil, FPSMap);
          FPSBD.DrawPosition := DrawPosition;
          FPSBD.TextureName := TextureName;
          FPSBD.Collision := true;
          FPSBD.OpeningDirection := DoorDir;
          FPSBD.Speed := DoorSpeed;
          FPSBD.OpenTime := DoorOpenTime;
          FPSMap.Objects.Add(FPSBD);
        end;
        emItem: FPSMap.CreateItem(ItemName, Position, ItemCount);
      end;
    end;
  end;
end;

procedure TMapEditor.SetMap(aMap: TFPSMap);
var Data: TData;
  i: Integer;
begin
  FPSMap := aMap; 
  for i := 0 to FPSMap.Objects.Count - 1 do
  begin
    Data := TData.Create;
    Mapa.Add(Data);
    if TFPSBasicSprite(FPSMap.Objects[i]).ClassNameIs(TFPSBasicSprite.ClassName) then
    begin
      with TFPSBasicSprite(FPSMap.Objects[i]) do
      begin
        Data.DrawPosition := DrawPosition;
        Data.TextureName := TextureName;
        Data.Collision := Collision;
      end;
      with Data, Data.DrawPosition do
      begin
        sx := Round(-(V3.X / girdZoom / girdSize) * girdSize);
        sy := Round(-(V2.Z / girdZoom / girdSize) * girdSize);
        if((V1.Y = WallHeight)and(V2.Y = V1.Y)and(V3.Y = V1.Y)and(V4.Y = V1.Y))then
          typ := emCeiling   //sufit
        else if(V1.Y = 0)and(V2.Y = 0)and(V3.Y = 0)and(V4.Y = 0)then
          typ := emFloor     //pod³oga
        else typ := emWall;  //œciana
        if typ <> emWall then sx := sx - girdSize;
        Horizontal := V1.Z <> V2.Z;
        Selected := false;
      end;
      Continue;
    end;
    if TFPSBasicSprite(FPSMap.Objects[i]) is TFPSBasicItem then
    with TFPSBasicItem(FPSMap.Objects[i]) do
    begin
      Data.Position := Position;
      Data.ItemName := Name;
      Data.sx := Round(-(Position.X / girdZoom / girdSize) * girdSize);
      Data.sy := Round(-(Position.Z / girdZoom / girdSize) * girdSize);
      Data.Selected := false;
      Data.Typ := emItem;
      Data.ItemCount := ItemsCount;
      Continue;
    end;
    if TFPSBasicSprite(FPSMap.Objects[i]) is TFPSBasicObject then
    with TFPSBasicObject(FPSMap.Objects[i]) do
    begin
      Data.Position := Position;
      Data.ItemName := Name;
      Data.sx := Round(-(Position.X / girdZoom / girdSize) * girdSize);
      Data.sy := Round(-(Position.Z / girdZoom / girdSize) * girdSize);
      Data.Selected := false;
      Data.Typ := emObject;
      Continue;
    end;
    if TFPSBasicSprite(FPSMap.Objects[i]) is TFPSBasicEventField then
    with TFPSBasicEventField(FPSMap.Objects[i]) do
    begin
      Data.EventData := EventData;
      Data.EventSource := EventSource;
      Data.EventType := EventType;
      Data.Typ := emEvent;
      Data.Selected := false;
      Continue;
    end;
    if TFPSBasicSprite(FPSMap.Objects[i]) is TFPSBasicDoor then
    with TFPSBasicDoor(FPSMap.Objects[i]) do
    begin
      Data.Typ := emDoor;
      Data.Selected := false;
      Data.DoorDir := DoorDirection;
      Data.DoorSpeed := Speed;
      Data.DoorOpenTime := OpenTime;
      Data.DrawPosition := DrawPosition;
      Data.TextureName := TextureName;
      Data.Collision := true;
      Data.sx := Round(-(DrawPosition.V3.X / girdZoom / girdSize) * girdSize);
      Data.sy := Round(-(DrawPosition.V2.Z / girdZoom / girdSize) * girdSize);
      Data.Horizontal := DrawPosition.V1.Z <> DrawPosition.V2.Z;
      Continue;
    end;
    ShowMessage('zuo');
  end;
  Mapa.Sort(SortData);
  IPaint;  
end;

end.
