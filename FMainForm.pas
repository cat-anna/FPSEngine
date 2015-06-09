unit FMainForm;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, strutils,
  Dialogs, ComCtrls, FPSEngine, StdCtrls, Spin, FloatSpinEdit, ExtCtrls, Buttons,
  ExtDlgs, LabeledSpinEdit, Windows, Menus, FMapEdytor, ClassPropertyEditor,
  SynEditHighlighter, SynHighlighterPas, shellapi,
  SynEdit, CheckLst;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    tsMap: TTabSheet;
    txTex: TTabSheet;
    lbTextures: TListBox;
    bAddTex: TButton;
    bDeleteTex: TButton;
    Image3: TImage;
    TabSheet1: TTabSheet;
    iObjTex: TImage;
    bAddObject: TButton;
    OpenDialog1: TOpenDialog;
    TabSheet2: TTabSheet;
    MainMenu1: TMainMenu;
    Plik1: TMenuItem;
    Zapisz1: TMenuItem;
    Otwrz1: TMenuItem;
    N1: TMenuItem;
    Zakocz1: TMenuItem;
    lbMaps: TListBox;
    bEditMap: TButton;
    iShowMap: TImage;
    Nowa: TMenuItem;
    bNewMap: TButton;
    OpenMapa: TOpenDialog;
    SaveMapa: TSaveDialog;
    lbFonts: TListBox;
    Label1: TLabel;
    lbSounds: TListBox;
    Label4: TLabel;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Label15: TLabel;
    cbStartMap: TComboBox;
    bDeleteMap: TButton;
    Button1: TButton;
    bDeleteOnject: TButton;
    ObjectsEditor: TClassPropertyEditor;
    cbObjectType: TComboBox;
    bActObject: TButton;
    ObjectsList: TListView;
    Sk: TTabSheet;
    seScriptEditor: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    lbScripts: TListBox;
    bScriptAdd: TButton;
    bScriptDelete: TButton;
    bScriptAct: TButton;
    Label2: TLabel;
    eScriptName: TEdit;
    PopupMenu1: TPopupMenu;
    Dodajwydarzenie1: TMenuItem;
    N2: TMenuItem;
    Wyczy1: TMenuItem;
    TabSheet3: TTabSheet;
    leGameName: TLabeledEdit;
    Zapiszjako1: TMenuItem;
    Button3: TButton;
    cbTextureAnim: TCheckBox;
    cbTextureLoop: TCheckBox;
    cbTextureCircle: TCheckBox;
    Label3: TLabel;
    Label5: TLabel;
    leTextureName: TLabeledEdit;
    bAddTex2: TButton;
    fseTextureSpeed: TFloatSpinEdit;
    TabSheet4: TTabSheet;
    lbItems: TListBox;
    bItemAdd: TButton;
    bItemAct: TButton;
    mItemProp: TMemo;
    bItemDelete: TButton;
    Label6: TLabel;
    ItemEditor: TClassPropertyEditor;
    cbMainFontName: TComboBox;
    Label7: TLabel;
    CheckListBox1: TCheckListBox;
    Label9: TLabel;
    procedure bItemClick(Sender: TObject);
    procedure ObjectsEditorNeedList(Sender: TObject; PropName: string;
      List: TStrings);
    procedure Button3Click(Sender: TObject);
    procedure AddPieceScript(Sender: TObject);
    procedure ScriptBtnClick(Sender: TObject);
    procedure ObjectsEditorEditChange(Sender: TObject; Field: TField);
    procedure Button1Click(Sender: TObject);
    procedure MainMenuClick(Sender: TObject);
    procedure lbMapsClick(Sender: TObject);
    procedure bMapButtons(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bObjectClick(Sender: TObject);
    procedure bAddTexClick(Sender: TObject);
    procedure lbTexturesClick(Sender: TObject);
    procedure bDeleteTexClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    Engine: TThreadEngine;
    MapData: TFPSData;

    MapFileName: string;
    Saved: bool;

    MapEditors: TList;

    procedure RefreshMapData;
    procedure LoadData(FN: String);
    procedure DataUpdate;
    procedure Clean;
  end;

var
  MainForm: TMainForm;

procedure LoadTexture(Name: String; MapData: TFPSData; Dest: TImage);

implementation

uses FPSUtils, math, InI, jpeg;

const
  ClassArray: array[0..3]of TClass = (TFPSBasicObject, TFPSBasicMonster, TFPSBasicBullet, TFPSBasicWeapon);

{$R *.dfm}

procedure LoadTexture(Name: String; MapData: TFPSData; Dest: TImage);
var
  MStr, FStr: TMemoryStream;
  jpg: TJPEGImage;
  Size: integer;
  Typ: byte;
begin
  if Name = '' then exit;
  Name := ChangeFileExt(Name, '.ftf');
  MStr := TMemoryStream.Create;
  FStr := TMemoryStream.Create;
  MapData.GetFile(mdftTexture, Name, MStr);
  Dest.Picture.Bitmap.SetSize(0, 0);
  MStr.Seek(SizeOf(TFileHeader), soBeginning);
  LoadStringFromStream(MStr);
  MStr.Seek(3 + SizeOf(Single) + SizeOf(Word), soCurrent);
  MStr.Read(Typ, SizeOf(Typ));
  MStr.Read(Size, SizeOf(Size));
  FStr.CopyFrom(MStr, Size);
  FStr.Position := 0;
  case Typ of
    1: Dest.Picture.Bitmap.LoadFromStream(FStr);
    2:begin
        jpg := TJPEGImage.Create;
        jpg.LoadFromStream(FStr);
        Dest.Picture.Assign(jpg);
        jpg.Free;
      end;
    3: LoadTGA(FStr, Dest.Picture.Bitmap)
    else ShowMessage('Nie obsjugujê tego formatu!!!');
  end;
  Dest.Refresh;
  MStr.Free;
  FStr.Free;
end;

procedure TMainForm.MainMenuClick(Sender: TObject);
begin
  case TControl(Sender).Tag of
    0:begin  //nowa
        if(not Saved)and(MessageDlg('Mapa nie jest zapisana'#13#10'Czy chcesz kontynuowaæ?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes)then exit;
        MapFileName := 'noname.zip';
        SaveMapa.InitialDir := ExtractFilePath(Application.ExeName);
        if SaveMapa.Execute then
        begin
          MapData.Clear;
          MapFileName := SaveMapa.FileName;
          MapData.CreateNew(MapFileName);
        end;
      end;
    1:begin //zapisz
        DataUpdate;
      end;
    2:begin //zapisz jako
        SaveMapa.InitialDir := ExtractFilePath(Application.ExeName);
        if not FileExists(MapFileName) then
        if SaveMapa.Execute then MapFileName := SaveMapa.FileName;
        MapData.SaveDataAs(MapFileName);
      end;
    3:begin //otwórz
        if(not Saved)and(MessageDlg('Mapa nie jest zapisana'#13#10'Czy chcesz kontynuowaæ?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes)then exit;
        if not OpenMapa.Execute then exit;
        MapData.Clear;
        MapData.LoadData(OpenMapa.FileName);
    end;
    4:Application.Terminate;
  end;   
end;

procedure TMainForm.ObjectsEditorEditChange(Sender: TObject; Field: TField);
begin
  if Field.PropName = 'TextureName' then
  begin
    LoadTexture(Field.ComboBox.Text, MapData, iObjTex);
  end;
end;

procedure TMainForm.ObjectsEditorNeedList(Sender: TObject; PropName: string;
  List: TStrings);
var i: integer;
begin
  List.Clear;
  if CompareStr(PropName, 'TextureName') = 0 then
    for i := 0 to MapData.Textures.Count - 1 do
      List.Add(ChangeFileExt(MapData.Textures[i], ''))
  else if CompareStr(PropName, 'ScriptName') = 0 then
    for i := 0 to MapData.Scripts.Count - 1 do
      List.Add(ChangeFileExt(MapData.Scripts[i], ''));
end;

procedure TMainForm.lbMapsClick(Sender: TObject);
begin
  if lbMaps.ItemIndex = -1 then exit;
  with TMapEditor(MapEditors[lbMaps.ItemIndex]) do
  begin
    iShowMap.Picture.Bitmap.SetSize(MapSnapShot.Width, MapSnapShot.Height);
    iShowMap.Picture.Bitmap.Canvas.CopyRect(
        Rect(0, 0, MapSnapShot.Width, MapSnapShot.Height),
        MapSnapShot.Canvas,
        Rect(0, 0, MapSnapShot.Width, MapSnapShot.Height));
  end;
end;

procedure TMainForm.lbTexturesClick(Sender: TObject);
begin
  if lbTextures.ItemIndex = -1 then exit;
  LoadTexture(lbTextures.Items[lbTextures.ItemIndex], MapData, Image3);
end;

procedure TMainForm.AddPieceScript(Sender: TObject);
begin
  case TControl(Sender).Tag of
    0:with seScriptEditor.Lines do //dodaj wydarzenie
      begin
        Add('function Event(Event: TFPSBasicEvent; Id: integer): integer;');
        Add('begin');
        Add('  Result := 0;');
        Add('  if Event = be then');
        Add('  begin');
        Add('');
        Add('    Result := 1;');
        Add('  end;');
        Add('end;');
      end;
    1:with seScriptEditor.Lines do //czyœæ
      begin
        Clear;
        Add('uses Game;');
        Add('');
      end;
  end;
end;

procedure TMainForm.bAddTexClick(Sender: TObject);
var i, size: integer;
    s: string;
    Value: Single;
    MStr, FStr: TMemoryStream;
    MDFT: TMapDataFileType;
    Header: TFileHeader;
    b: byte;
    w: word;
begin
  case TButton(Sender).Tag of
    1, 4:begin
        MDFT := mdftTexture;
        OpenDialog1.Filter := 'Tekstury(*.bmp;*.tga;*.jpg)|*.bmp;*.tga;*.jpg';
      end;
    2:begin
        MDFT := mdftFont;
        OpenDialog1.Filter := 'Czcionki(*.ttf)|*.ttf';
      end;
    3:begin
        MDFT := mdftSound;
        OpenDialog1.Filter := 'Znane typy|*.mp3;*.wav;*.ogg;*.it;*.xm;*.mod';
      end;
    else exit;
  end;
  if not OpenDialog1.Execute then exit;
  MStr := TMemoryStream.Create;
{  with TStringList.Create do
  begin
    Assign(OpenDialog1.Files);
    Sort;
    OpenDialog1.Files.Text := Text;
    Free;
    ShowMessage(OpenDialog1.Files.Text);
  end;     }
  if TButton(Sender).Tag = 1 then
  begin
    if leTextureName.Text = '' then
      leTextureName.Text := ChangeFileExt(ExtractFileName(OpenDialog1.Files[0]), '');

    FillChar(Header, SizeOf(Header), 0);
    MStr.Write(Header, SizeOf(Header));
    SaveStringToStream([leTextureName.Text], MStr);
    SaveBoolToStream([cbTextureAnim.Checked, cbTextureLoop.Checked, cbTextureCircle.Checked], MStr);
    Value := fseTextureSpeed.Value;
    MStr.Write(Value, SizeOf(Value));
    w := OpenDialog1.Files.Count;
    MStr.Write(w, SizeOf(w));
    FStr := TMemoryStream.Create;
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      FStr.LoadFromFile(OpenDialog1.Files[i]);
      s := ExtractFileExt(OpenDialog1.Files[i]);
      if CompareStr(s, '.bmp') = 0 then b := 1
      else if CompareStr(s, '.jpg') = 0 then b := 2
      else if CompareStr(s, '.tga') = 0 then b := 3;
      MStr.Write(b, SizeOf(b));
      Size := FStr.Size;
      MStr.Write(Size, SizeOf(Size));
      FStr.SaveToStream(MStr);
    end;
    FStr.Free;
    MapData.AddFile(MDFT, leTextureName.Text + '.ftf', MStr);
  end else
  if TButton(Sender).Tag = 4 then
  begin
    FillChar(Header, SizeOf(Header), 0);
    FStr := TMemoryStream.Create;
    for i := 0 to OpenDialog1.Files.Count - 1 do
    begin
      MStr.Clear;
      MStr.Write(Header, SizeOf(Header));
      SaveStringToStream([ChangeFileExt(ExtractFileName(OpenDialog1.Files[i]), '')], MStr);
      SaveBoolToStream([false, false, false], MStr);
      Value := 0;
      MStr.Write(Value, SizeOf(Value));
      w := 1;
      MStr.Write(w, SizeOf(w));
      FStr.LoadFromFile(OpenDialog1.Files[i]);
      s := ExtractFileExt(OpenDialog1.Files[i]);
      if CompareStr(s, '.bmp') = 0 then b := 1
      else if CompareStr(s, '.jpg') = 0 then b := 2
      else if CompareStr(s, '.tga') = 0 then b := 3;
      MStr.Write(b, SizeOf(b));
      Size := FStr.Size;
      MStr.Write(Size, SizeOf(Size));
      FStr.SaveToStream(MStr);
      MapData.AddFile(MDFT, ChangeFileExt(ExtractFileName(OpenDialog1.Files[i]), '.ftf'), MStr);
    end;
    FStr.Free;
  end else
  for i := 0 to OpenDialog1.Files.Count - 1 do
  begin
    s := ExtractFileName(OpenDialog1.Files[i]);
    MStr.Clear;
    MStr.LoadFromFile(OpenDialog1.Files[i]);
    MapData.AddFile(MDFT, OpenDialog1.Files[i], MStr);
  end;
  MStr.Free;
  RefreshMapData;
end;

procedure TMainForm.bDeleteTexClick(Sender: TObject);
var
  MDFT: TMapDataFileType;
  lb: TListBox;
begin
  case TButton(Sender).Tag of
    1:begin
        MDFT := mdftTexture;
        lb := lbTextures;
      end;
    2:begin
        MDFT := mdftFont;
        lb := lbFonts;
      end;
    3:begin
        MDFT := mdftSound;
        lb := lbSounds;
      end;
    else exit;
  end;

  if lb.ItemIndex <> -1 then MapData.DeleteFile(MDFT, lb.Items[lb.ItemIndex]);
  RefreshMapData;
end;

procedure TMainForm.bItemClick(Sender: TObject);
var Obj: TFPSBasicItem;
begin
  case TControl(Sender).Tag of
    1:begin//dodaj
        Obj := TFPSBasicItem(ItemEditor.CreateSelectedClass);
        if Obj = nil then exit;
        MapData.StaticObjects.Add(Obj);
        with Obj do
        begin
          ProperyData := mItemProp.Lines.Text;
        end;
        DataUpdate;
        RefreshMapData;
      end;
    2:if lbItems.ItemIndex <> -1 then//aktualizuj
        with TFPSBasicItem(lbItems.Items.Objects[lbItems.ItemIndex]) do
        begin
          ItemEditor.UpdateObject(TFPSBasicItem(lbItems.Items.Objects[lbItems.ItemIndex]));
          ProperyData := mItemProp.Lines.Text;
          DataUpdate;
          RefreshMapData;
        end;
    3:if lbItems.ItemIndex <> -1 then//usuñ
      begin
        TFPSBasicItem(lbItems.Items.Objects[lbItems.ItemIndex]).Free;
        MapData.StaticObjects.Remove(TFPSBasicItem(lbItems.Items.Objects[lbItems.ItemIndex]));
        DataUpdate;
        RefreshMapData;
      end;
    4:if lbItems.ItemIndex <> -1 then//listbox.click;
        with TFPSBasicItem(lbItems.Items.Objects[lbItems.ItemIndex]) do
        begin
          mItemProp.Lines.Text := ProperyData;
          ItemEditor.SelectObject(TFPSBasicItem(lbItems.Items.Objects[lbItems.ItemIndex]));
        end;
  end;
end;

procedure TMainForm.bMapButtons(Sender: TObject);
var MN: string;
    ME: TMapEditor;
begin
  case TButton(Sender).Tag of
    1:begin //editmap
        if lbMaps.ItemIndex = -1 then lbMaps.ItemIndex := 0;
        TMapEditor(MapEditors[lbMaps.ItemIndex]).Show;
      end;
    2:begin//addmap
        Mn := 'NowaMapa';
        if not InputQuery('Edytor', 'Podaj nazwê mapy', Mn) then exit;

        ME := TMapEditor.Create(Self);
        MapEditors.Add(ME);
        lbMaps.Items.Add(MN);
        with ME do
        begin
          FPSMap := TFPSMap.Create(MapData);
          FPSMap.MapName := MN;
          IPaint;
        end;
        RefreshMapData;
      end;
    3:if(lbMaps.ItemIndex > -1)and(MessageDlg('Czy jesteœ pewien??', mtConfirmation, mbYesNo, 0) = mrYes)then
      begin//deletemap
        TFPSMap(MapData.Maps[lbMaps.ItemIndex]).Free;
        MapData.Maps.Delete(lbMaps.ItemIndex);
        TMapEditor(MapEditors[lbMaps.ItemIndex]).Free;
        MapEditors.Delete(lbMaps.ItemIndex);
        DataUpdate;
        RefreshMapData;
      end;
  end;
end;

procedure TMainForm.bObjectClick(Sender: TObject);
var Obj: TFPSBasicSprite;
begin
  case TWinControl(Sender).Tag of
    1:begin//add
        Obj := TFPSBasicSprite(ObjectsEditor.CreateSelectedClass);
        if Obj = nil then exit;
        MapData.StaticObjects.Add(Obj);
        DataUpdate;
        RefreshMapData;
      end;
    2:begin//aktualizuj
        ObjectsEditor.UpdateObject;
        DataUpdate;
      end;
    3:begin//usuñ
        if ObjectsList.ItemIndex = -1 then exit;
        MapData.StaticObjects.Remove(ObjectsList.Items[ObjectsList.ItemIndex].Data);
        TFPSBasicSprite(ObjectsList.Items[ObjectsList.ItemIndex].Data).Free;
        RefreshMapData;
        DataUpdate;
      end;
    4:begin
        ObjectsEditor.SelectClass(ClassArray[cbObjectType.ItemIndex]);//zmiana typu
        RefreshMapData;
      end;
    5:begin//zmiana obiektu
        if ObjectsList.ItemIndex = -1 then exit;
        ObjectsEditor.SelectObject(TObject(ObjectsList.Items[ObjectsList.ItemIndex].Data));
      end;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin   
 { if(Engine = nil)or(Engine.Ended)then
  begin
    DataUpdate;
    Engine := TThreadEngine.Create(true);
    Engine.Engine.LoadData(MapFileName);
    Engine.Resume;
  end;    }
  ShellExecuteA(handle, 'open', 'fps.exe', pchar(MapFileName), nil, SW_SHOW);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  ObjectsEditor.SaveClassToFile(ExtractFilePath(Application.ExeName) + 'Objects.ini');
end;

procedure TMainForm.ScriptBtnClick(Sender: TObject);
var
  i: integer;
  MStr: TMemoryStream;
begin
  case TControl(Sender).Tag of
    0:if not MapData.Scripts.Find(eScriptName.Text, i) then//dodaj
      begin
        MStr := TMemoryStream.Create;
        seScriptEditor.Lines.SaveToStream(MStr);
        MapData.AddFile(mdftScript, eScriptName.Text + '.pas', MStr);
        MStr.Free;
      end;
    1:if MapData.Scripts.Find(eScriptName.Text + '.pas', i) then //aktualizuj
      begin
        MStr := TMemoryStream.Create;
        seScriptEditor.Lines.SaveToStream(MStr);
        MapData.AddFile(mdftScript, eScriptName.Text + '.pas', MStr);
        MStr.Free;
      end;
    2:if lbScripts.ItemIndex > -1 then//usuñ
      begin
        MapData.DeleteFile(mdftScript, lbScripts.Items[lbScripts.ItemIndex]);
        MapData.Scripts.Delete(MapData.Scripts.IndexOf(lbScripts.Items[lbScripts.ItemIndex]));
      end;
    3:if lbScripts.ItemIndex > -1 then //select
      begin
        MStr := TMemoryStream.Create;
        MapData.GetFile(mdftScript, lbScripts.Items[lbScripts.ItemIndex], MStr);
        eScriptName.Text := ChangeFileExt(ExtractFileName(lbScripts.Items[lbScripts.ItemIndex]), '');
        MStr.Position := 0;
        seScriptEditor.Lines.LoadFromStream(MStr);
        MStr.Free;
      end;
  end;
  RefreshMapData;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Engine := nil;
  MapEditors := TList.Create;
  MapData := TFPSData.Create;
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  Saved := true;
 	if (ParamCount > 0)and(Pos('debug', ParamStr(1)) > 0) then DebugMode := true;
  ObjectsEditor.AssignStringToList('TextureName');
  ObjectsEditor.AssignStringToList('ScriptName');
  ItemEditor.SelectClass(TFPSBasicItem);
  ItemEditor.AssignStringToList('TextureName');
  ItemEditor.AssignStringToList('ScriptName');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if(Engine <> nil)and not Engine.Ended then Engine.DoEnd;
  Clean;
  MapData.Free;
  MapEditors.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FileExists('Mapa.zip') then LoadData('Mapa.zip')
  else MapData.CreateNew('Mapa.zip');
  PageControl.TabIndex := 0;
end;

procedure TMainForm.RefreshMapData;
var i: Integer;
begin
  lbTextures.Items.Assign(MapData.Textures);
  lbFonts.Items.Assign(MapData.Fonts);
  lbSounds.Items.Assign(MapData.Sounds);
  lbScripts.Items.Assign(MapData.Scripts);
  lbMaps.Items.Clear;
  for i := 0 to MapEditors.Count - 1 do
    lbMaps.Items.Add(TMapEditor(MapEditors[i]).FPSMap.MapName);

  ObjectsList.Items.Clear;
  for i := 0 to MapData.StaticObjects.Count - 1 do
  if(TFPSBasicSprite(MapData.StaticObjects[i]).ClassType = ClassArray[cbObjectType.ItemIndex])or
    (TFPSBasicSprite(MapData.StaticObjects[i]).ClassParent = ClassArray[cbObjectType.ItemIndex]) then
  with ObjectsList.Items.Add do
  begin
    Caption := TFPSBasicSprite(MapData.StaticObjects[i]).Name;
    Data := TFPSBasicSprite(MapData.StaticObjects[i]);
  end;

  cbStartMap.Items.Assign(lbMaps.Items);
  cbStartMap.ItemIndex := cbStartMap.Items.IndexOf(MapData.StartMap);

  i := cbStartMap.ItemIndex;
  cbStartMap.Items.Assign(lbMaps.Items);
  cbStartMap.ItemIndex := i;

  lbItems.Items.Clear;
  for i := 0 to MapData.StaticObjects.Count - 1 do
    if TFPSBasicSprite(MapData.StaticObjects[i]) is TFPSBasicItem then
      lbItems.Items.AddObject(TFPSBasicItem(MapData.StaticObjects[i]).Name, TFPSBasicItem(MapData.StaticObjects[i]));

  CheckListBox1.Items.Clear;
  for i := 0 to lbSounds.Items.Count - 1 do
    CheckListBox1.Items.Add(GetFileNameNoExt(lbSounds.Items[i]));

  for i := 0 to MapData.MenuMusic.Count - 1 do
    CheckListBox1.Checked[CheckListBox1.Items.IndexOf(MapData.MenuMusic[i])] := true;

  cbMainFontName.Items.Clear;
  for i := 0 to lbFonts.Items.Count - 1 do
    cbMainFontName.Items.Add(GetFileNameNoExt(lbFonts.Items[i]));
  cbMainFontName.ItemIndex := cbMainFontName.Items.IndexOf(MapData.MenuFont);
end;

procedure TMainForm.DataUpdate;
var i: integer;
begin
  MapData.MenuMusic.Clear;
  for i := 0 to CheckListBox1.Items.Count - 1 do
    if CheckListBox1.Checked[i] then
      MapData.MenuMusic.Add(CheckListBox1.Items[i]);

  MapData.GameName := leGameName.Text;
  MapData.StartMap := cbStartMap.Text;
  MapData.MenuFont := cbMainFontName.Text;
  MapData.Update;
end;

procedure TMainForm.LoadData(FN: String);
var i: integer;
    ME: TMapEditor;
begin
  Clean;
  MapFileName := FN;
  MapData.LoadData(FN);
  for i := 0 to MapData.Maps.Count - 1 do
  begin
    ME := TMapEditor.Create(Self);
    MapEditors.Add(ME);
    ME.SetMap(TFPSMap(MapData.Maps[i]));
  end;
  cbStartMap.Items.Assign(lbMaps.Items);
  cbStartMap.ItemIndex := cbStartMap.Items.IndexOf(MapData.StartMap);
  leGameName.Text := MapData.GameName;
  RefreshMapData;
end;

procedure TMainForm.Clean;
var i: Integer;
begin
  for i := 0 to MapEditors.Count - 1 do TMapEditor(MapEditors[i]).Free;
  MapEditors.Clear;
end;

end.
