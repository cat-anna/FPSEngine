unit FPSEngine;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ZipForge, uPSCompiler, uPSRuntime,
  dglOpengl;

const
  CollisionRadius = 0.2;

type
  T3DPoint = record
    X: Single;
    Y: Single;
    Z: Single;
  end;
  TDrawPosition = record
    V1: T3Dpoint;
    V2: T3Dpoint;
    V3: T3Dpoint;
    V4: T3Dpoint;
  end;
  TFontData = record
    Name: string;
    Base: Cardinal;
    Handle: Cardinal;
    Id: Integer;
  end;
  TArea = record
    LeftTop: T3Dpoint;
    Size: T3Dpoint;
  end;
  TEventData = record
    DString: string;
    DPoint: T3Dpoint;
    EventArea: TArea;
    Id: word;
  end;
  TTexture = record
    Name: string;
    FramesCount: Word;
    Frames: array of Cardinal;
    Animated: Boolean;
    AnimLooped: Boolean;
    AnimCircled: Boolean;
    AnimForward: Boolean;
    AnimPos: Single;
    AnimSpeed: Single;
  end;
  TFileHeader = array[0..4] of char;

  TMapDataFileType = (mdftTexture, mdftFont, mdftSound, mdftScript, mdftMap);
  TFPSEventType = (etTeleport, etScript);
  TFPSEventSource = (esCollision, esOnUse);
  TFPSBasicEvent = (beCreate, beDead, beAnimationLoop, beCollision, beUse);
  TFPSEventSet = set of TFPSBasicEvent;
  TFPSItemProprtyType = (iptUse, iptEquip, iptUnEquip);
  TLogMsgType = (lmtNone, lmtError, lmtInfo);

  TFPSEngine = class;
  TFPSMap = class;
  TFPSBasicSprite = class;

  TAudioSystem = class
  protected
    FSounds: TList;
    FHandle: HWND;
    FTimerHandle, FPlayListPos: integer;
    FPlayList: TStringList;
    FPlayRandom: bool;
    FEnabled: bool;
    procedure SetEnabled(Value: bool);
    procedure WndProc(var Msg: TMessage);
    procedure PlayListPlay;
  public
    procedure Play(SndName: string);
    procedure Clear;
    procedure AddSound(FileName: string; Data: TStream = nil);
    procedure SetPlayList(List: TStringList; PlayRandom: bool = true);
    procedure StopAll;
    constructor Create;
    destructor Destroy; override;
    property Enabled: bool read FEnabled write SetEnabled;
  end;

  TScriptEngine = class
  protected
    FCompiler: TPSPascalCompiler;
    FExec: TPSExec;
    FClassImporter: TPSRuntimeClassImporter;
    Scripts: TList;
    function GetByName(Name: string): Pointer;
    procedure LoadExec(se: TPSExec; Code: string);
    function OnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
  public
    SelfPointer: TObject;

    destructor Destroy; override;
    constructor Create;
    function AddScript(ScriptName, Code: String; Messages: PString = nil): Boolean;
    procedure RunScript(ScriptName: String);
    function RunFunction(ScriptName, FunctionName: string; Params: array of Variant): Variant;
    function CheckScript(Code: String; Messages: TStrings): boolean;
    procedure Clear;
    property Compiler: TPSPascalCompiler read FCompiler;     
  end;

  TFPSBasicSprite = class(TPersistent)
  protected
    FDestroyable, FCollision, FDeaded, FVisible: boolean;
    FScriptName, FScriptFunction, FName, FTextureName: string;
    FOwnerEngine: TFPSEngine;
    FOwnerMap: TFPSMap;
    FTexture: TTexture;
    ScriptEvents: TFPSEventSet;
    procedure DoDraw; virtual;
    procedure DoMove; virtual;
  public
    DrawPosition: TDrawPosition;
    procedure RunScript(Event: TFPSBasicEvent; Id: integer = 0); virtual;
    procedure Dead;
    procedure Hit(Power: Single); virtual;
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); virtual;
    destructor Destroy(); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); virtual;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); virtual;
    procedure Assign(Item: TFPSBasicSprite); reintroduce; virtual;
    property OwnerEngine: TFPSEngine read FOwnerEngine write FOwnerEngine;
    property OwnerMap: TFPSMap read FOwnerMap write FOwnerMap;
    property Deaded: Boolean read FDeaded;
  published
    property Destroyable: Boolean read FDestroyable write FDestroyable;
    property Collision: Boolean read FCollision write FCollision;
    property ScriptName: string read FScriptName write FScriptName;
    property ScriptFunction: string read FScriptFunction write FScriptFunction;
    property Name: string read FName write FName;
    property TextureName: string read FTextureName write FTextureName;
  end;

  TFPSBasicObject = class(TFPSBasicSprite)
  protected
    FWidth, FHeight, FHitPoints: Single;
    procedure SetYPos(Value: Single);
    function GetYPos: Single;
  public
    Position: T3Dpoint;
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    destructor Destroy(); override;
    procedure Hit(Power: Single); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
  published
    property Visible: boolean read FVisible write FVisible;
    property Width: Single read FWidth write FWidth;
    property Height: Single read FHeight write FHeight;
    property YPosition: Single read GetYPos write SetYPos;
    property HitPoints: Single read FHitPoints write FHitPoints;
  end;

  TFPSBasicBullet = class(TFPSBasicSprite)
  protected
    FBasicSpeed: Single;
    FSpeedMultiplier: byte;
    FLifeTime, CreateTime: Cardinal;
    Position, Speed: T3Dpoint;
    procedure DoDraw; override;
    procedure DoMove; override;
  public
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
  published
    property BasicSpeed: Single read FBasicSpeed write FBasicSpeed;
    property SpeedMultiplier: byte read FSpeedMultiplier write FSpeedMultiplier;
    property LifeTime: Cardinal read FLifeTime write FLifeTime;
  end;

  TFPSBasicWeapon = class(TFPSBasicSprite)
  protected
    FBulletName, FShotSound: string;
    FCanShot: boolean;
  public
    procedure RunScript(Event: TFPSBasicEvent; Id: integer = 0); override;
    procedure Shot; virtual;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
    property CanShot: boolean read FCanShot;
  published
    property BulletName: string read FBulletName write FBulletName;
    property ShotSound: string read FShotSound write FShotSound;
  end;

  TFPSBasicMonster = class(TFPSBasicObject)
  protected
    FAttack, FSpeed, FSeeRange: Single;
    FAtackSpeed, LastAttack: Cardinal;
    procedure DoMove; override;
  public
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
  published
    property Attack: Single read FAttack write FAttack;
    property Speed: Single read FSpeed write FSpeed;
    property SeeRange: Single read FSeeRange write FSeeRange;
    property AtackSpeed: Cardinal read FAtackSpeed write FAtackSpeed;
  end;

  TFPSBasicKillZone = class(TFPSBasicObject)
  protected
    FRange, FDmgPerSek: Single;
    Emitter: TFPSBasicSprite;
    procedure DoMove; override;
    procedure DoDraw; override;
  public
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
  published
    property Range: Single read FRange write FRange;
    property DmgPerSek: Single read FDmgPerSek write FDmgPerSek;
  end;

  TFPSBasicEventField = class(TFPSBasicSprite)
  protected
    procedure DoMove; override;
  public
    EventSource: TFPSEventSource;
    EventType: TFPSEventType;
    EventData: TEventData;
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
  published
  end;

  TFPSBasicDoor = class(TFPSBasicSprite)
  protected
    FInMove: boolean;
    FMoveDirection, FOpeningDirection, FMovePhase: byte;
    FOpenTime: Word;
    FOpenedTime: Cardinal;
    FSpeed: Single;
    FStartPos: T3DPoint;
    procedure DoMove; override;
  public
    procedure RunScript(Event: TFPSBasicEvent; Id: integer = 0); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
  published
    property OpeningDirection: byte read FOpeningDirection write FOpeningDirection;
    property Speed: Single read FSpeed write FSpeed;
    property OpenTime: Word read FOpenTime write FOpenTime;
  end;

  TFPSBasicItem = class(TFPSBasicObject)
  protected
    FItemsCount: Word;
    FPropertyData: string;
  public
    procedure RunScript(Event: TFPSBasicEvent; Id: integer = 0); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
    procedure LoadFromStream(Stream: TStream; LoadAll: bool = false); override;
    procedure Assign(Item: TFPSBasicSprite); override;
    procedure DoItemProperty(WhichProperty: TFPSItemProprtyType); virtual;
  published
    property ItemsCount: Word read FItemsCount write FItemsCount;
    property ProperyData: string read FPropertyData write FPropertyData;
  end;

  TFPSData = class
  protected
    Packer: TZipForge;
    Opened: bool;
    SLArray: array[TMapDataFileType] of TStringList;

    procedure SaveStaticObjects(MStr: TMemoryStream);
    procedure LoadStaticObjects(MStr: TMemoryStream);
    function GetStaticObject(ObjName: string): TFPSBasicSprite;
  public
    Maps, StaticObjects: TList;
    StartMap, DataFileName, GameName, MenuFont: string;
    Textures, Fonts, Sounds, Scripts, MenuMusic: TStringList;
    OwnerEngine: TFPSEngine;

    constructor Create;
    destructor Destroy; override;
    procedure CreateNew(FName: string);
    procedure LoadData(FName: string);
    procedure SaveDataAs(FName: string);
    procedure Update;
    procedure AddFile(FileType: TMapDataFileType; FileName: string; MStr: TMemoryStream);
    procedure DeleteFile(FileType: TMapDataFileType; FileName: string);
    procedure GetFile(FileType: TMapDataFileType; FileName: string; MStr: TMemoryStream);
    procedure Clear;
    function GetStartMap: TFPSMap;
  end;

  TFPSMap = class
    MapName, FontName: String;
    Objects: TList;
    Fog: bool;
    FogColor: array[0..3]of Single;
    SeeRange: Single;
    StartPoint: T3Dpoint;
    StartDirection: Single;
    Owner: TFPSData;
    PlayList: TStringList;

    constructor Create(aOwner: TFPSData; Data: TStream = nil);
    destructor Destroy; override;
    procedure LoadMap(MStr: TStream);
    procedure SaveMap(MStr: TStream);
    procedure CreateObject(ObjName: String; ObjPos: T3Dpoint);
    procedure CreateKillZone(Range, DmgPerSek: Single; Creator: TFPSBasicObject);
    procedure CrerateEventField(EventSource: TFPSEventSource; EventType: TFPSEventType; EventData: TEventData);
    procedure CreateItem(ItemName: String; ItemPos: T3Dpoint; ItemCount: integer);
    procedure DestroyObject(Index: integer);
    procedure Clear();
  end;

  TFPSPlayer = class(TPersistent)
  protected
    FHitPoints, FDirection: Single;
    FPosition: T3DPoint;
  public
    ActiveWeapon: TFPSBasicWeapon;      
    procedure Hit(Power: Single);
  published
    property HitPoints: Single read FHitPoints write FHitPoints;
    property Direction: Single read FDirection write FDirection;
    property Position: T3DPoint read FPosition write FPosition;
  end;

  TFPSEngine = class(TPersistent)
  protected
    h_DC: HDC;
    h_RC: HGLRC;
    FHandle: HWND;
    Keys: Array[0..255] of Boolean;
    KeysCount: array[0..255] of word;
    FPSCount, FontsCount, FScreenSize, FGameFont: Integer;
    FPSText, FMainMenuFont, FDataFile: string;
    FLastFrameTime, FFrameTime: Cardinal;
    FInitialised, FInMenu, FEnded, FDoFullScreen, FDataLoaded, FExtWindow: bool;
    FWindow: TForm;
    FDeadList, FTextures, FBackpack: TList;
    FScriptEngine: TScriptEngine;
    FAudioSystem: TAudioSystem;
    Fonts: array of TFontData;
    HeadMovement, HeadMovAngle, Tilt: Single;
    UseKey, ShotKey: byte;

    procedure DoMove;
    procedure RemoveDead;
    procedure DoDraw;
    procedure ProcessKeys;
    procedure ReObjectsPositions;
    procedure ReObjectPos(Obj: TFPSBasicObject);
    procedure TeleportPlayer(MapName: String; Position: T3DPoint; NewDirection: Single);
    procedure SetPlayerPosition(NewPosition: T3DPoint; NewDirection: Single);
    procedure WResize(Sender: TObject);
    procedure WKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WClose(Sender: TObject; var Action: TCloseAction);
    procedure SetGraphicMode;
    procedure SaveConfiguration;
    procedure DrawTexture(Sprite: TFPSBasicSprite);
    procedure PutInBackpack(Item: TFPSBasicItem);
    function GetFromBackpack(ItemName: String; var Item: TFPSBasicItem): bool;
    procedure ParseItemProp(PropOwner, PropName, PropValue: string; PropMult: integer = 1);
    function GetFont(FontName: string): TFontData;
    procedure WndProc(var Message: TMessage);
    procedure DoWork;
    function GetTexture(TexName: string): TTexture;
    function ShowMenu(Names: TStringList; StartIndex: Integer): Integer;
    procedure MainMenu;
    procedure OptionsMenu;
    procedure LoadModuleMenu;
  public
    ActiveMap: TFPSMap;
    MapData: TFPSData;
    Player: TFPSPlayer;       

    procedure SetActiveMap(NewActive: TFPSMap);
    procedure Clear;
    procedure StartGame;
    procedure DoEnd;
    
    procedure PlaySound(SndName: string);
    function RunEventScript(ScriptName, FncName: string; SelfPointer: TFPSBasicSprite; Event: TFPSBasicEvent; Id: integer = 0): integer;

    constructor Create;
    destructor Destroy; override;
    procedure Initialise(ExtWindow: TForm = nil);
    procedure Finalize;
    procedure Run(RunInLoop: bool = true);
    function LoadData(FileName: string): bool;

    property Ended: bool read FEnded;
    property Initialised: bool read FInitialised;
  end;

  TThreadEngine = class(TThread)
  protected
    procedure Execute; override;
    function GetEnded: bool;
  public
    Engine: TFPSEngine;
    procedure BeforeDestruction; override;
    procedure AfterConstruction; override;
    procedure DoEnd;
    property Ended: bool read GetEnded;
  end;

  TFPSBasicClass = class of TFPSBasicSprite;

  TFPSClassList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AClass: TFPSBasicClass); overload;
    procedure Add(AClass: TFPSBasicClass; AName: ShortString); overload;
    function FindByName(AName: string): TFPSBasicClass;
  end;

procedure AddLog(Msg: string; MsgType: TLogMsgType = lmtError); overload;
procedure AddLog(Text: string; Varibles: array of const; MsgType: TLogMsgType = lmtError); overload;

var
  FPSClassList: TFPSClassList;
  DebugMode: boolean = false;

const
  ModuleExt = '.zip';

implementation

uses Controls, Utils, InI, GLUtils, FPSPreObj, mmsystem, graphics,
     uPSR_std, uPSC_std, uPSI_FPSEngine, Bass, math, dialogs, TypInfo, IniFiles;

type
  PTexture = ^TTexture;

const
  TIMER_FPS = 1;
  TIMER_DRAW = 2;
  MouseSpeed: Integer = 7;
  TurnSpeed: Single = 7;  
  FPS = 25;
  BoolPolStr: array[Boolean]of string  = ('wy³¹czony', 'w³¹czony');
  ConfigFileName = 'Config.ini';
  ScreenSizeTable: array[0..3] of TPoint = ((x: 640; y:480), (x:800; y: 600), (x:1024; y:768), (x:1280; y:1024));

  MainMenuOptions = 'Wczytaj modu³'#13#10'Nowa gra'#13#10'Wczytaj grê'#13#10'Opcje'#13#10'Wyjœcie';
  OptionsMenuOptions = 'Ustawienia klawiszy'#13#10'Rozdzielczoœæ'#13#10'Pe³ny ekran'#13#10'DŸwiêki'#13#10'Powrót';

  ArchiveDirs: array[TMapDataFileType] of string = ('Textures\', 'Other\',
     'Sounds\', 'Scripts\', 'Maps\');

  ItemSections: array[TFPSItemProprtyType] of string = ('OnUse', 'OnEquip', 'OnEquip');

  LogMsgHeader: array[TLogMsgType] of string = ('', 'B³¹d: ', 'Info: ');

procedure TFPSEngine.Initialise(ExtWindow: TForm);
begin
  Player := TFPSPlayer.Create;
  Player.ActiveWeapon := TFPSShotGun1.Create(Self, nil);
  FExtWindow := ExtWindow = nil;
  if ExtWindow = nil then FWindow := TForm.Create(nil)
  else FWindow := ExtWindow;
  with FWindow do
  begin
    Caption := 'Inicjalizacja...';
    Position := poScreenCenter;
    Width := 400;
    Height := 0;
    Show;
    OnKeyDown := WKeyDown;
    OnKeyUp := WKeyUp;
    OnClose := WClose;
    BorderStyle := bsSingle;
  end;

  FHandle := Classes.AllocateHWnd(WndProc);
  SetTimer(FHandle, TIMER_FPS, 1000, nil);
  
  InitOpenGL;
  h_DC := GetDC(FWindow.Handle);
  h_RC := CreateRenderingContext(h_DC, [opDoubleBuffered], 32, 24, 0, 0, 0, 0);
  ActivateRenderingContext(h_DC, h_RC);

  glClearColor(0.0, 0.0, 0.0, 0.0); 	   // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glClearDepth(1.0);                       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do
  glEnable(GL_TEXTURE_2D);           // Enable Texture Mapping
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

  //kana³ alfa
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_GREATER, 0.4);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);

  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);

  SetGraphicMode;
  FWindow.OnResize := WResize;
  WResize(nil);

  FInitialised := true;
end;

procedure TFPSEngine.Run(RunInLoop: bool = true);
var ATime: Cardinal;
begin
  if not FInitialised then exit;     

  MainMenu;

  Player.HitPoints := 100;
  ShowCursor(false);
  SetCursorPos(300, 300);
  FLastFrameTime := GetTickCount;
  if not RunInLoop then
  begin
    SetTimer(FHandle, TIMER_DRAW, Trunc(1000 / FPS), nil);
    Exit;
  end
  else
  while not FEnded do
  begin
    ATime := GetTickCount;
    if ATime - FLastFrameTime > FFrameTime then
    begin
      FLastFrameTime := ATime;
      DoWork;
    end;
    Sleep(10);
    Application.ProcessMessages
  end;
  ShowCursor(true);
  Finalize;
  DoEnd;
end;

procedure TFPSEngine.Finalize;
begin
  Clear;
  ShowCursor(true);
  KillTimer(FHandle, TIMER_FPS);
  KillTimer(FHandle, TIMER_DRAW);
  Classes.DeallocateHWnd(FHandle);

  Player.ActiveWeapon.Free;
  Player.Free;
  DeactivateRenderingContext; 
  wglDeleteContext(h_RC);
  ReleaseDC(FWindow.Handle, h_DC);
  if not FExtWindow then FWindow.Free;
  FInitialised := false;
end;

procedure TFPSEngine.MainMenu;
var
  MenuOptions: TStringList;
  Selected: integer;
begin
  FAudioSystem.SetPlayList(MapData.MenuMusic);
  FInMenu := true;
  MenuOptions := TStringList.Create;
  MenuOptions.Text := MainMenuOptions;
  Selected := 0;
  while not FEnded do
  begin
    FWindow.Caption := 'FPSEdytor';
    Selected := ShowMenu(MenuOptions, Selected);
    case Selected of
     -1: DoEnd;
      0: LoadModuleMenu;//wczytaj modu³
      1: if FDataLoaded then //nowa gra
         begin
           StartGame;
           Break;
         end;
      2: ;//wczytaj grê
      3: OptionsMenu;//opcje
      4: DoEnd; //wyjœcie
    end;
  end;   
  MenuOptions.Free;
  FInMenu := false;
end;

procedure TFPSEngine.OptionsMenu;
var
  MenuOptions: TStringList;
  Selected: integer;
begin
  MenuOptions := TStringList.Create;
  MenuOptions.Text := OptionsMenuOptions;
  Selected := 0;
  while not FEnded do
  begin
    MenuOptions[3] := 'DŸwiêki - ' + BoolPolStr[FAudioSystem.Enabled];
    MenuOptions[2] := 'Pe³ny ekran - ' + BoolPolStr[FDoFullScreen];
    MenuOptions[1] := 'Rozdzielczoœæ - ' + Format('%dx%d', [ ScreenSizeTable[FScreenSize].X, ScreenSizeTable[FScreenSize].Y]);
    Selected := ShowMenu(MenuOptions, Selected);
    case Selected of
     -1: Break;//escape
      0: ; // klawisze
      1:begin //rozdzielczoœæ
          Inc(FScreenSize);
          if FScreenSize = Length(ScreenSizeTable) then FScreenSize := 0;
          SetGraphicMode;
        end;
      2:begin //pe³ny ekran
          FDoFullScreen := not FDoFullScreen;
          SetGraphicMode;
        end;
      3:begin
          FAudioSystem.Enabled := not FAudioSystem.Enabled;
        end;
      4: Break; //powrót
    end;
  end;
  MenuOptions.Free;
  SaveConfiguration;
end;

procedure TFPSEngine.LoadModuleMenu;
var
  MenuOptions: TStringList;
  Selected: integer;
  sr: TSearchRec;
begin
  MenuOptions := TStringList.Create;
  Selected := 0;
  while not FEnded do
  begin
    MenuOptions.Clear;
    if FindFirst(ExtractFilePath(Application.ExeName) + '*' + ModuleExt, faAnyFile, sr) = 0 then
    begin
      MenuOptions.Add(GetFileNameNoExt(sr.Name));
      while findNext(sr) = 0 do MenuOptions.Add(GetFileNameNoExt(sr.Name));
    end;
    MenuOptions.Add('Powrót');
    Selected := ShowMenu(MenuOptions, Selected);
    if(Selected = -1)or(Selected = MenuOptions.Count - 1)then Break
    else
    begin
      LoadData(MenuOptions[Selected] + ModuleExt);
      Break;
    end;
  end;
  MenuOptions.Free;
end;

function TFPSEngine.ShowMenu(Names: TStringList; StartIndex: Integer): Integer;
var
  HalfWidth, LastResult: integer;
  Buffer: TBitmap;
  procedure MenuRedraw;
  var i: integer;
  begin
    Buffer.Canvas.FillRect(Buffer.Canvas.ClipRect);
    for i := 0 to Names.Count - 1 do
    begin
      if i = Result then Buffer.Canvas.Font.Color := clGray
      else Buffer.Canvas.Font.Color := clWhite;
      Buffer.Canvas.TextOut(HalfWidth - Buffer.Canvas.TextWidth(Names[i]) div 2,
         Round(Buffer.Canvas.TextHeight(Names[i]) * 1.5 * i) + 80, Names[i]);
    end;
    FWindow.Canvas.Draw(0, 0, Buffer);
  end;
begin
  HalfWidth := FWindow.Width div 2;
  Buffer := TBitmap.Create;
  Result := StartIndex;
  LastResult := -1;
  with Buffer, Buffer.Canvas do
  begin
    SetSize(FWindow.Width, FWindow.Height);
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    Font.Size := 25;
    Font.Name := FMainMenuFont;
    KeysCount[vk_up] := 0;
    KeysCount[vk_Down] := Result;
    KeysCount[vk_return] := 0;
    KeysCount[vk_Escape] := 0;
    while not FEnded do
    begin
      Sleep(50);
      Application.ProcessMessages;
      if KeysCount[vk_return] > 0 then
      begin
        Buffer.Free;
        Exit;
      end;
      if KeysCount[vk_Escape] > 0 then
      begin
        Result := -1;
        Buffer.Free;
        Exit;
      end;
      Result := KeysCount[vk_Down] - KeysCount[vk_up];
      if Result < 0 then
      begin
        KeysCount[vk_up] := 0;
        KeysCount[vk_Down] := 0;
        Result := 0;
      end;
      MenuRedraw;
      if Result >= Names.Count then
      begin
        KeysCount[vk_up] := 0;
        KeysCount[vk_Down] := Names.Count - 1;
        Result := Names.Count - 1;
      end;
      if Result <> LastResult then
      begin
        MenuRedraw;
        LastResult := Result;
        Application.ProcessMessages;
        Continue;
      end;
    end;
  end;
  Buffer.Free;
end;

constructor TFPSEngine.Create;
begin
  inherited;
  Randomize;
  FInitialised := false;
  FInMenu := false;
  FEnded := false;
  FDataLoaded := false;
  FDataFile := '';
  FMainMenuFont := 'Arial';
  FDeadList := TList.Create;
  FTextures := TList.Create;
  FBackpack := TList.Create;
  MapData := TFPSData.Create;
  MapData.OwnerEngine := Self;
  ActiveMap := nil;
  FAudioSystem := TAudioSystem.Create;
  FScriptEngine := TScriptEngine.Create;
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + ConfigFileName) do
  begin
    FScreenSize := ReadInteger('Graphic', 'ScreenSize', 1);
    FDoFullScreen := ReadBool('Graphic', 'FullScreen', false);
    UseKey := ReadInteger('Keys', 'Use', VK_SHIFT);
    ShotKey := ReadInteger('Keys', 'Shot', VK_SPACE);
    FAudioSystem.Enabled := ReadBool('Sound', 'Enabled', true);
    Free;
  end;
end;

destructor TFPSEngine.Destroy;
begin
  if FInitialised then Finalize;
  MapData.Free;
  FAudioSystem.Free;
  FScriptEngine.Free;
  FDeadList.Free;
  FTextures.Free;
  inherited;
end;

procedure TFPSEngine.DoDraw;
var i: Integer;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity;

  glEnable(GL_LIGHTING);
  glDisable(GL_TEXTURE_2D);

  glColor4f(1.0, 1.0, 1.0, 1.0);
   //napisy
  glTranslatef(0.0, 0.0, -0.1);
  glScalef(0.003, 0.003, 0.003);
  glPrint(Format('HP:  %d', [Round(Player.HitPoints)]), -18.0, 11.2, 0.0, Fonts[FGameFont].Base);
  glPrint(FPSText, -18.0, -13.5, 0.0, Fonts[FGameFont].Base);

  glEnable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  
   //narysuj broñ
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
 
  if Player.ActiveWeapon <> nil then Player.ActiveWeapon.DoDraw;
  
  WResize(nil);

  glRotatef(Tilt, 1, 0, 0);
  glRotatef(Player.Direction, 0, 1, 0);
  glTranslatef(-Player.Position.X, -0.5 + HeadMovement, -Player.Position.Z);

  for i := 0 to ActiveMap.Objects.Count - 1 do
    TFPSBasicSprite(ActiveMap.Objects[i]).DoDraw;

  glFlush();
  SwapBuffers(wglGetCurrentDC);
end;

procedure TFPSEngine.DoEnd;
begin
  FEnded := true;
end;

procedure TFPSEngine.ProcessKeys;
var
  NewPosition: T3Dpoint;
  j: integer;
  MousePos: TPoint;
  procedure MovePlayer;
  var i: integer;
      CanMove: bool;
  begin
    CanMove := true;
    i := 0;
    while i < ActiveMap.Objects.Count do
    begin
      with TFPSBasicSprite(ActiveMap.Objects[i]) do
      if Is2DCollision(DrawPosition, NewPosition)and(Collision or (TFPSBasicSprite(ActiveMap.Objects[i]) is TFPSBasicItem))then
      begin
        CanMove := false;
        RunScript(beCollision);
      end;
      Inc(i);
    end;

    if CanMove then
    begin
      Player.Position := NewPosition;
      HeadMovAngle := HeadMovAngle + 60;
      HeadMovement := 0.01 * sin(HeadMovAngle*pi/180);
      ReObjectsPositions;
    end;
  end;
begin
  if Keys[87] then   //  VK_UP   //ruch do przodu
  begin
    NewPosition := AddTo3DPoint(Player.Position,
      sin(Player.Direction*pi/180)*FFrameTime/300, 0,
      -cos(Player.Direction*pi/180)*FFrameTime/300);
    MovePlayer;
  end;
  if Keys[83] then  //  VK_DOWN   //ruch w ty³
  begin
    NewPosition := AddTo3DPoint(Player.Position,
      -sin(Player.Direction*pi/180)*FFrameTime/300, 0,
       cos(Player.Direction*pi/180)*FFrameTime/300);
    MovePlayer;
  end;
  if Keys[65] then //ruch w lewo
  begin
    NewPosition := AddTo3DPoint(Player.Position,
      -sin((Player.Direction+90)*pi/180)*FFrameTime/450, 0,
       cos((Player.Direction+90)*pi/180)*FFrameTime/450);
    MovePlayer;
  end;
  if Keys[68] then //ruch w prawo
  begin
    NewPosition := AddTo3DPoint(Player.Position,
      sin((Player.Direction+90)*pi/180)*FFrameTime/450, 0,
      -cos((Player.Direction+90)*pi/180)*FFrameTime/450);
    MovePlayer;
  end;

  if(Keys[ShotKey])and(Player.ActiveWeapon <> nil)then
  begin
    Player.ActiveWeapon.Shot;
  end;

  if Keys[UseKey] then
  begin
    for j := 0 to ActiveMap.Objects.Count - 1 do
    with TFPSBasicSprite(ActiveMap.Objects[j]) do
    if IsCollision(DrawPosition, Player.Position, 2)then
      RunScript(beUse);
  end;

  if GetForegroundWindow = FWindow.Handle then
  begin
    GetCursorPos(MousePos);
    SetCursorPos(300, 300);
    Player.Direction := Player.Direction + (MousePos.x - 300)/50 * MouseSpeed;
    Tilt := Tilt - (300 - MousePos.y)/50 * MouseSpeed;
    if Tilt > 60 then Tilt := 60;
    if Tilt < -60 then Tilt := -60;
  end;

  if Keys[VK_ESCAPE] then FWindow.Close;
end;

procedure TFPSEngine.SetGraphicMode;
begin  
  with FWindow do
  begin
    Width := ScreenSizeTable[FScreenSize].X;
    Height := ScreenSizeTable[FScreenSize].Y;
    Left := Screen.Monitors[0].Width div 2 - Width div 2;
    Top := Screen.Monitors[0].Height div 2 - Height div 2;
  end;
end;

procedure TFPSEngine.SaveConfiguration;
begin
  with TIniFile.Create(ExtractFilePath(Application.ExeName) + ConfigFileName) do
  begin
    WriteInteger('Graphic', 'ScreenSize', FScreenSize);
    WriteBool('Graphic', 'FullScreen', FDoFullScreen);
    WriteBool('Sound', 'Enabled', FAudioSystem.Enabled);
    WriteInteger('Keys', 'Use', UseKey);
    WriteInteger('Keys', 'Shot', ShotKey);
    Free;
  end;
end;

procedure TFPSEngine.WResize(Sender: TObject);
begin
  glViewport(0, 0, FWindow.Width, FWindow.Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity();                   // Reset View
  gluPerspective(45.0, FWindow.Width/FWindow.Height, 0.1, 100.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity();                   // Reset View
end;

procedure TFPSEngine.WClose(Sender: TObject; var Action: TCloseAction);
begin
  DoEnd;
end;

procedure TFPSEngine.WKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not keys[Key] then KeysCount[Key] := KeysCount[Key] + 1;
  keys[Key] := true;
end;

procedure TFPSEngine.WKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  keys[Key] := false;
//  KeysCount[Key] := KeysCount[Key] - 1;
end;

procedure TFPSEngine.WndProc(var Message: TMessage);
begin
  with Message do
  case Msg of
    WM_TIMER:
      case WParam of
        TIMER_FPS:
          begin
            FPSText := Format('FPS: %d  FrameFime: %f', [FPSCount, FFrameTime]);
            FPSCount := 0;
          end;
        TIMER_DRAW: DoWork;
      end;
    else
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  end;
end;

procedure TFPSEngine.ReObjectsPositions;
var i: Integer;
begin
  for i := 0 to ActiveMap.Objects.Count - 1 do
    if TFPSBasicSprite(ActiveMap.Objects[i]) is TFPSBasicObject then
      ReObjectPos(TFPSBasicObject(ActiveMap.Objects[i]));
end;

procedure TFPSEngine.ReObjectPos(Obj: TFPSBasicObject);
var PosProp, PosPropPower: single;
    NX, NZ, r: single;
begin
  with Obj, Obj.DrawPosition do
  begin
    PosProp := ((Position.X - Player.Position.X) / (Position.Z - Player.Position.Z));
    PosPropPower := Sqr(PosProp);
    r := Width / 2;

    NX := r / Sqrt(1 + PosPropPower);
    NZ := r * PosProp * Power(1 + PosPropPower, -0.5);

    V1.X := Position.X - NX;
    V4.X := V1.X;
    V1.Z := Position.Z + NZ;
    V4.Z := V1.Z;

    V2.X := Position.X + NX;
    V3.X := V2.X;
    V2.Z := Position.Z - NZ;
    V3.Z := V2.Z;
  end;
end;

procedure TFPSEngine.DrawTexture(Sprite: TFPSBasicSprite);
begin
  with Sprite, Sprite.FTexture do
  begin
    if Animated then
    begin
      if AnimForward then
      begin //animacja idzie do przodu
        if(Trunc(AnimPos + AnimSpeed) >= FramesCount)then
        begin
          if AnimCircled then
          begin
            AnimPos := FramesCount - 0.001;
            AnimForward := false;
          end else
          begin
            if AnimLooped then AnimPos := 0
            else
            begin
              Animated := false;     
              RunScript(beAnimationLoop);
            end;
          end;
        end else AnimPos := AnimPos + AnimSpeed;
      end else
      begin  //animacja idzie do ty³u
        if(AnimPos - AnimSpeed <= 0) then
        begin
          AnimPos := 0;
          AnimForward := true;
        end else AnimPos := AnimPos - AnimSpeed;
      end;
    end;

    with DrawPosition do
    begin
      glBindTexture(GL_TEXTURE_2D, Frames[Trunc(AnimPos)]);
      glBegin(GL_QUADS);
        glTexCoord2f(0, 0);  glVertex3fv(@V1);
        glTexCoord2f(1, 0);  glVertex3fv(@V2);
        glTexCoord2f(1, 1);  glVertex3fv(@V3);
        glTexCoord2f(0, 1);  glVertex3fv(@V4);
      glEnd();
    end;
  end;
end;

function TFPSEngine.GetTexture(TexName: string): TTexture;
var
  i: Integer;
begin
  for i := 0 to FTextures.Count - 1 do
  if CompareStr(PTexture(FTextures[i])^.Name, TexName) = 0 then
  begin
    Result := PTexture(FTextures[i])^;
    exit;
  end;
end;

function TFPSEngine.LoadData(FileName: string): bool;
var S, Code: String;
    i, j, Size: integer;
    MStr, FStr: TMemoryStream;
    Tex: PTexture;
    Header: TFileHeader;
    b: byte;
begin
  Result := false;
  if not FInitialised then exit;  
  if CompareStr(ExtractFileExt(FileName), ModuleExt) <> 0 then Exit;
  FDataFile := FileName;
  with FWindow.Canvas do
  begin
    FWindow.Caption := '£adowanie...';
    Brush.Color := clBlack;
    Brush.Style := bsSolid;
    Font.Name := FMainMenuFont;
    Font.Color := clWhite;
    Font.Size := 20;
    FillRect(ClipRect);
    TextOut(70, 50, 'Wczytywanie modu³u...');
  end;
  Clear;
  MapData.LoadData(FileName);
  MStr := TMemoryStream.Create;
//dŸwiêki       
  for i := 0 to MapData.Sounds.Count - 1 do
  begin
    Application.ProcessMessages;
    FWindow.Canvas.TextOut(100, 100, Format('£adujê dŸwiêki... %d/%d', [i + 1, MapData.Sounds.Count]));
    MapData.GetFile(mdftSound, MapData.Sounds[i], MStr);
    FAudioSystem.AddSound(MapData.Sounds[i], MStr);
  end;
  FAudioSystem.SetPlayList(MapData.MenuMusic);
//tekstury
  FStr := TMemoryStream.Create;
  for i := 0 to MapData.Textures.Count - 1 do
  begin
    Application.ProcessMessages;
    S := MapData.Textures[i];
    FWindow.Canvas.TextOut(100, 150, Format('£adujê tekstury... %d/%d', [i+1, MapData.Textures.Count]));
    MapData.GetFile(mdftTexture, S, MStr);
    New(Tex);
    FTextures.Add(Tex);
    MStr.Read(Header, SizeOf(Header));
    with Tex^ do
    begin
      Name := LoadStringFromStream(MStr);
      Animated := LoadBoolFromStream(MStr);
      AnimLooped := LoadBoolFromStream(MStr);
      AnimCircled := LoadBoolFromStream(MStr);
      MStr.Read(AnimSpeed, SizeOf(AnimSpeed));
      MStr.Read(FramesCount, SizeOf(FramesCount));
      AnimPos := 0;
      AnimForward := true;
    end;
    SetLength(Tex^.Frames, Tex^.FramesCount);
    for j := 0 to Tex^.FramesCount - 1 do
    begin
      FStr.Clear;
      MStr.Read(b, SizeOf(b));
      MStr.Read(Size, SizeOf(Size));
      FStr.CopyFrom(MStr, Size);
      FStr.Position := 0;
      case b of
        1: LoadBMPTexture(FStr, '', Tex^.Frames[j]);
        2: LoadJPGTexture(FStr, '', Tex^.Frames[j]);
        3: LoadTGATexture(FStr, '', Tex^.Frames[j]);
        else ShowMessage('B³¹d przy ³adowaniu tekstury!!!');
      end;
    end;
  end;
  FStr.Free;
//czcionki
  FontsCount := MapData.Fonts.Count;
  SetLength(Fonts, FontsCount);
  for i := 0 to FontsCount - 1 do
  begin
    Fonts[i].Id := i;
    Application.ProcessMessages;
    Fonts[i].Name := GetFileNameNoExt(MapData.Fonts[i]);
    FWindow.Canvas.TextOut(100, 200, Format('£adujê czcionki... %d/%d', [i + 1, FontsCount]));
    MapData.GetFile(mdftFont, MapData.Fonts[i], MStr);
    Fonts[i].Handle := AddFontMemResourceEx(MStr.Memory, MStr.Size, nil, @j);
    if Fonts[i].Handle = 0 then RaiseLastOSError;
    Fonts[i].Base := GLUtils.CreateFont(Fonts[i].Name, 0.5, h_DC);
  end;
//skrypty
  for i := 0 to MapData.Scripts.Count - 1 do
  begin
    Application.ProcessMessages;
    MStr.Clear;
    MapData.GetFile(mdftScript, MapData.Scripts[i], MStr);
    SetLength(Code, MStr.Size);
    MStr.Read(PChar(Code)^, length(Code));
    FWindow.Canvas.TextOut(100, 250, Format('Kompilujê skrypty... %d/%d', [i + 1, MapData.Scripts.Count]));
    if not FScriptEngine.AddScript(ChangeFileExt(MapData.Scripts[i], ''), Code, @S) then
    begin
      Beep;
      ShowMessage('B³¹d kompilacji skryptu: ' + MapData.Scripts[i] + #13#10 + S);
    end;
  end;
//ró¿ne opcje
{  if MapData.MenuFont > -1 then MainMenuFont := Fonts[MapData.MenuFont].Name
  else MainMenuFont := 'Arial';}
  FMainMenuFont := MapData.MenuFont;
  FWindow.Caption := 'Modu³: ' + MapData.GameName;
//rozprowadzanie tekstur
  for i := 0 to MapData.Maps.Count - 1 do
    for j := 0 to TFPSMap(MapData.Maps[i]).Objects.Count - 1 do
    with TFPSBasicSprite(TFPSMap(MapData.Maps[i]).Objects[j]) do
      FTexture := GetTexture(TextureName);
  Player.ActiveWeapon.FTexture := GetTexture(Player.ActiveWeapon.TextureName);

  MStr.Free;
  FDataLoaded := true;
  Result := true;
end;

procedure TFPSEngine.SetActiveMap(NewActive: TFPSMap);
begin
  ActiveMap := NewActive;

  if ActiveMap.Fog then
  begin
    glEnable(GL_FOG);
    glFogi(GL_FOG_MODE, GL_LINEAR);
    glHint(GL_FOG_HINT,GL_DONT_CARE);                 // Kvalita mlhy
    glFogf(GL_FOG_START, 1.0);                         // Zaèátek mlhy - v hloubce (osa z)
    glFogf(GL_FOG_END, ActiveMap.SeeRange);
    glFogfv(GL_FOG_COLOR, @ActiveMap.FogColor);
    glFogf(GL_FOG_DENSITY, 0.1);
  end else glDisable(GL_FOG);
  FAudioSystem.SetPlayList(ActiveMap.PlayList);

  FGameFont := GetFont(ActiveMap.FontName).Id;

  FWindow.Caption := 'Modu³: ' + MapData.GameName + ' - Mapa: ' + ActiveMap.MapName;
end;

procedure TFPSEngine.Clear;
var i, j: integer;
begin
  FAudioSystem.Clear;
  FScriptEngine.Clear;
  for i := 0 to FontsCount - 1 do
  begin
    glDeleteLists(Fonts[i].Base, 256);
    RemoveFontMemResourceEx(Fonts[i].Handle);
  end;
  for i := 0 to FTextures.Count - 1 do
  with PTexture(FTextures[i])^ do
  begin
    for j := 0 to FramesCount - 1 do
      glDeleteTextures(0, Frames[j]);      
    Dispose(PTexture(FTextures[i]));
  end;
  FTextures.Clear;
  for i := 0 to FBackpack.Count - 1 do
  begin
    TFPSBasicSprite(FBackpack[i]).Free;
  end;
  FBackpack.Clear;
  FDeadList.Clear;
  MapData.Clear;
  FDataLoaded := false;  
end;

procedure TFPSEngine.StartGame;
begin
  if not FInitialised then exit;
  SetActiveMap(MapData.GetStartMap);
  SetPlayerPosition(ActiveMap.StartPoint, ActiveMap.StartDirection);
end;

procedure TFPSEngine.DoMove;
var i: Integer;
begin
  i := 0;
  while i < ActiveMap.Objects.Count do
  begin
    TFPSBasicSprite(ActiveMap.Objects[i]).DoMove;
    Inc(i);
  end;
end;

procedure TFPSEngine.DoWork;
var ATime: Cardinal;
begin
  ATime := GetTickCount;
  FLastFrameTime := ATime;
  FFrameTime := ATime - FLastFrameTime;
  Inc(FPSCount);
  RemoveDead;
  ProcessKeys;   
  DoMove;
  DoDraw;
end;

procedure TFPSEngine.RemoveDead;
begin
  while FDeadList.Count > 0 do
  begin
    ActiveMap.DestroyObject(ActiveMap.Objects.IndexOf(FDeadList[0]));
    FDeadList.Delete(0);
  end;
end;

procedure TFPSEngine.TeleportPlayer(MapName: String; Position: T3DPoint; NewDirection: Single);
var i: integer;
begin
  for i := 0 to MapData.Maps.Count - 1 do
  if CompareText(TFPSMap(MapData.Maps[i]).MapName, MapName) = 0 then
  begin
    SetActiveMap(TFPSMap(MapData.Maps[i]));
    SetPlayerPosition(Position, NewDirection);
    Exit;
  end;
end;

procedure TFPSEngine.SetPlayerPosition(NewPosition: T3DPoint; NewDirection: Single);
begin
  Player.FPosition := NewPosition;
  Player.FPosition.Y := 0.5;
  Player.Direction := NewDirection;
  ReObjectsPositions;
end;

procedure TFPSEngine.PutInBackpack(Item: TFPSBasicItem);
//var i: integer;
begin
{  for i := 0 to FBackpack.Count - 1 do
    if CompareStr(TFPSBasicItem(FBackpack[i]).Name, Item.Name) = 0 then
    with TFPSBasicItem(FBackpack[i]) do
    begin
      ItemsCount := ItemsCount + Item.ItemsCount;
      Item.Dead;
      Exit;
    end;     }
  Item.DoItemProperty(iptUse);
  Item.Dead;
//  ActiveMap.Objects.Remove(Item);
//  FBackpack.Add(Item);
end;

function TFPSEngine.GetFont(FontName: string): TFontData;
var i: integer;
begin
  for i := 0 to FontsCount - 1 do
    if CompareStr(ActiveMap.FontName, Fonts[i].Name) = 0 then
    begin
      Result := Fonts[i];
      Break;
    end;
end;

function TFPSEngine.GetFromBackpack(ItemName: String; var Item: TFPSBasicItem): bool;
var i: integer;
begin
  Result := false;
  Item := nil;
  for i := 0 to FBackpack.Count - 1 do
    if CompareStr(TFPSBasicItem(FBackpack[i]).Name, ItemName) = 0 then
    begin
      Result := true;
      Item := TFPSBasicItem(FBackpack[i]);
      Exit;
    end;
end;

procedure TFPSEngine.ParseItemProp(PropOwner, PropName, PropValue: string; PropMult: integer = 1);
var Obj: TPersistent;
begin
  if CompareStr(PropOwner, 'Player') = 0 then Obj := Player
  else
  begin
    AddLog('Nieznany obiekt "%s"', [PropOwner]);
    Exit;
  end;
{  TTypeKind = (tkUnknown, , tkChar, tkEnumeration, ,
    tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
    tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray);  }  
  try
    if IsPublishedProp(Obj, PropName) then
    begin
      case PropType(Obj, PropName) of
        tkInteger: SetInt64Prop(Obj, PropName,
                      GetInt64Prop(Obj, PropName) + StrToInt(PropValue) * PropMult);
        tkFloat: SetFloatProp(Obj, PropName,
                      GetFloatProp(Obj, PropName) + StrToFloat(PropValue) * PropMult);
        else
          AddLog('W³aœciwoœæ %s.%s ma nieobs³ugiwany typ', [PropOwner, PropName]);
      end;
    end else
    begin
      AddLog('W³aœciwoœæ: %s.%s nie istnieje', [PropOwner, PropName]);
    end;
  except
    AddLog('Wyst¹pi³ b³¹d podczas analizy sk³adniowej W³aœciwoœci przedmiotu(%s.%s=%s)', [PropOwner, PropName, PropValue]);
  end;
end;

procedure TFPSEngine.PlaySound(SndName: string);
begin
  FAudioSystem.Play(SndName);
end;

function TFPSEngine.RunEventScript(ScriptName, FncName: string;
  SelfPointer: TFPSBasicSprite; Event: TFPSBasicEvent; Id: integer): integer;
begin
  FScriptEngine.SelfPointer := SelfPointer;
  Result := FScriptEngine.RunFunction(ScriptName, FncName, [Event, Id]);
end;

{   TFPSMap   }

procedure TFPSMap.LoadMap(MStr: TStream);
var
  i, Count: integer;
  Header: TFileHeader;
  ItemClass: TFPSBasicClass;
  FPSBO: TFPSBasicSprite;
  N: String;
begin
  if MStr = nil then exit;
  
  Clear;
  MStr.Position := 0;
//naglowek, nazwa, start
  MStr.Read(Header, SizeOf(Header));
  MapName := LoadStringFromStream(MStr);
  MStr.Read(StartPoint, SizeOf(StartPoint));
  MStr.Read(StartDirection, SizeOf(StartDirection));
//mg³a
  Fog := LoadBoolFromStream(MStr);
  MStr.Read(FogColor, SizeOf(FogColor));
  MStr.Read(SeeRange, SizeOf(SeeRange));
//pusta przestrzeñ do 500 bajtu
  MStr.Seek(500, soFromBeginning);
//obiekty wszelakie
  MStr.Read(Count, SizeOf(Count));
  for i := 0 to Count - 1 do
  begin
    N := LoadStringFromStream(MStr);
    ItemClass := FPSClassList.FindByName(N);
    if ItemClass = nil then
    begin
      AddLog('Nieznany obiekt: %s', [N]);
      Break;
    end;
    FPSBO := ItemClass.Create(Owner.OwnerEngine, Self);
    FPSBO.LoadFromStream(MStr);
    Objects.Add(FPSBO);
  end;
  PlayList.Text := LoadStringFromStream(MStr);
end;

procedure TFPSMap.SaveMap(MStr: TStream);
var
  i, Count: integer;
  Header: array[0..4] of char;
begin
  FillChar(Header, 5, 0);
//naglowek, nazwa
  MStr.Write(Header, SizeOf(Header));
  SaveStringToStream([MapName], MStr);
  MStr.Write(StartPoint, SizeOf(StartPoint));
  MStr.Write(StartDirection, SizeOf(StartDirection));
//mg³a
  SaveBoolToStream([Fog], MStr);
  MStr.Write(FogColor, SizeOf(FogColor));
  MStr.Write(SeeRange, SizeOf(SeeRange));
//pusta przestrzeñ do 500 bajtu
  MStr.Seek(500, soFromBeginning);
//obiekty wszelakie
  Count := Objects.Count;
  MStr.Write(Count, SizeOf(Count));
  for i := 0 to Count - 1 do
    TFPSBasicSprite(Objects[i]).SaveToStream(MStr);
  SaveStringToStream([PlayList.Text], MStr);
end;

procedure TFPSMap.Clear();
var
  i: Integer;
begin
  for i := 0 to Objects.Count - 1 do
    TFPSBasicSprite(Objects[i]).Free;
  Objects.Clear;
  PlayList.Clear;
end;

constructor TFPSMap.Create(aOwner: TFPSData; Data: TStream = nil);
begin
  Objects := TList.Create;
  PlayList := TStringList.Create;
  Owner := aOwner;
  Owner.Maps.Add(Self);   
  StartPoint := Make3DPoint(0, 0, 0);
  FogColor[0] := 0;
  FogColor[1] := 0;
  FogColor[2] := 0;
  FogColor[3] := 0;
  Fog := false;
  LoadMap(Data);
end;

destructor TFPSMap.Destroy;
begin
  Clear;
  Objects.Free;
  PlayList.Free;
  inherited;
end;

procedure TFPSMap.CreateObject(ObjName: String; ObjPos: T3Dpoint);
var
  Obj, StaticObj: TFPSBasicSprite;
  ObjectClass: TFPSBasicClass;
begin
  StaticObj := Owner.GetStaticObject(ObjName);
  if StaticObj = nil then
  begin
    AddLog('Statyczny obiekt "%s" nie istnieje', [ObjName]);
    exit;
  end;
  ObjectClass := FPSClassList.FindByName(StaticObj.ClassName);
  Obj := ObjectClass.Create(Owner.OwnerEngine, Self);
  TFPSBasicObject(Obj).Position := ObjPos;
  Obj.Assign(StaticObj);
  Objects.Add(Obj);
  if(Obj is TFPSBasicObject)and(Owner.OwnerEngine <> nil) then
    Owner.OwnerEngine.ReObjectPos(TFPSBasicObject(Obj));
end;

procedure TFPSMap.DestroyObject(Index: integer);
begin
  TFPSBasicObject(Objects[Index]).Free;
  Objects.Delete(Index);
end;

procedure TFPSMap.CreateKillZone(Range, DmgPerSek: Single; Creator: TFPSBasicObject);
var
  Obj: TFPSBasicKillZone;
begin
  Obj := TFPSBasicKillZone.Create(Owner.OwnerEngine, Self);
  Obj.Range := Range;
  Obj.DmgPerSek := DmgPerSek;
  Obj.Emitter := Creator;
  Obj.Position := Creator.Position;
  Objects.Add(Obj);
end;

procedure TFPSMap.CrerateEventField(EventSource: TFPSEventSource; EventType: TFPSEventType; EventData: TEventData);
var Obj: TFPSBasicEventField;
begin
  Obj := TFPSBasicEventField.Create(Owner.OwnerEngine, Self);
  Obj.EventSource := EventSource;
  Obj.EventType := EventType;
  Obj.EventData := EventData;
  Objects.Add(Obj);
end;

procedure TFPSMap.CreateItem(ItemName: String; ItemPos: T3Dpoint; ItemCount: integer);
var
  Obj: TFPSBasicItem;
  StaticItem: TFPSBasicSprite;
  ObjectClass: TFPSBasicClass;
begin
  StaticItem := Owner.GetStaticObject(ItemName);
  if(StaticItem = nil)or not (StaticItem is TFPSBasicItem) then
  begin
    AddLog('Statyczny przedmiot "%s" nie istnieje', [ItemName]);
    exit;
  end;
  ObjectClass := FPSClassList.FindByName(StaticItem.ClassName);
  Obj := (ObjectClass.Create(Owner.OwnerEngine, Self) as TFPSBasicItem);
  if Obj = nil then
  begin
    AddLog('Wyst¹pi³ b³¹d przy tworzeniu obiektu "%s"', [ItemName]);
    exit;
  end;
  Obj.Position := ItemPos;
  Obj.Assign(StaticItem);
  Obj.ItemsCount := ItemCount;
  Objects.Add(Obj);
  if(Obj is TFPSBasicObject)and(Owner.OwnerEngine <> nil) then
    Owner.OwnerEngine.ReObjectPos(TFPSBasicObject(Obj));
end;

{   TFPSData   }

procedure TFPSData.CreateNew(FName: string);
begin
  if Opened then Clear;
  Opened := true;
  DataFileName := FName;
  if FileExists(FName) then Windows.DeleteFile(PChar(FName));
  Packer.FileName := DataFileName;
  Packer.OpenArchive;
end;

procedure TFPSData.LoadData(FName: string);
var
  MStr: TMemoryStream;
  SIni: TIni;
  ai: TZFArchiveItem;
  procedure FindFiles(Mask: string; List: TStringList);
  begin
    if Packer.FindFirst(Mask , ai, ZFNormalFileAttr) then
    begin
      List.Add(ai.FileName);
      while Packer.FindNext(ai) do List.Add(ai.FileName);
    end;
    List.Sort;
  end;
begin
  Clear;
  DataFileName := FName;
  Packer.FileName := DataFileName;
  Packer.OpenArchive;
  MStr := TMemoryStream.Create;
  Packer.ExtractToStream('Data.ini', MStr);
  SIni := TIni.Create(MStr);

//opcje
  StartMap := SIni.ReadString('Options', 'StartMap', '');
  GameName := SIni.ReadString('Options', 'GameName', 'Noname');
  MenuFont := SIni.ReadString('Options', 'MenuFont', '');
//£adowanie list plików
  FindFiles(ArchiveDirs[mdftTexture] + '*.ftf', Textures);
  FindFiles(ArchiveDirs[mdftFont] + '*.ttf', Fonts);
  FindFiles(ArchiveDirs[mdftSound] + '*.mp3', Sounds);
  FindFiles(ArchiveDirs[mdftSound] + '*.wav', Sounds);
  FindFiles(ArchiveDirs[mdftSound] + '*.xm', Sounds);
  FindFiles(ArchiveDirs[mdftSound] + '*.it', Sounds);
  FindFiles(ArchiveDirs[mdftSound] + '*.ogg', Sounds);
  FindFiles(ArchiveDirs[mdftSound] + '*.mod', Sounds);
  FindFiles(ArchiveDirs[mdftScript] + '*.pas', Scripts);
//statyczne obiekty
  if Packer.FindFirst('Objects.dat', ai) then
  begin
    MStr.Clear;
    Packer.ExtractToStream('Objects.dat', MStr);
    LoadStaticObjects(MStr);
  end;
//mapy
  if Packer.FindFirst(ArchiveDirs[mdftMap] + '*.map', ai, ZFNormalFileAttr) then
  begin
    MStr.Clear;
    Packer.ExtractToStream(ArchiveDirs[mdftMap] + ai.FileName, MStr);
    TFPSMap.Create(Self, MStr);
    while Packer.FindNext(ai) do
    begin
      MStr.Clear;
      Packer.ExtractToStream(ArchiveDirs[mdftMap] + ai.FileName, MStr);
      TFPSMap.Create(Self, MStr);
    end;
  end;
//PlayLisy
  SIni.ReadListSection('MenuMusic', MenuMusic);

  SIni.Free;
  MStr.Free;
  Opened := true;
end;

procedure TFPSData.SaveDataAs(FName: string);
//var
//  NewFile: TZipForge;
begin
  if DataFileName = FName then
  begin
    Update;
    exit;
  end;  
//  NewFile := TZipForge.Create(nil);
//  NewFile.Free;
end;

procedure TFPSData.Update;
var
  MStr: TMemoryStream;
  SIni: TIni;
  i: Integer;
begin
  if not Opened then exit;

  MStr := TMemoryStream.Create;
  SIni := TIni.Create('');

//Opcje gry
  SIni.WriteString('Options', 'StartMap', StartMap);
  SIni.WriteString('Options', 'GameName', GameName);
  SIni.WriteString('Options', 'MenuFont', MenuFont);
//mapy
  for i := 0 to Maps.Count - 1 do
  begin
    MStr.Clear;
    TFPSMap(Maps[i]).SaveMap(MStr);
    Packer.AddFromStream('Maps\' + TFPSMap(Maps[i]).MapName + '.map', MStr);
  end;
//PlayLisy
  SIni.WriteListSection('MenuMusic', MenuMusic);
//statyczne obiekty
  MStr.Clear;
  SaveStaticObjects(MStr);
  Packer.AddFromStream('Objects.dat', MStr);
//zapis do data.ini
  MStr.Clear;
  SIni.SaveToStream(MStr);
  Packer.AddFromStream('Data.ini', MStr);
  SIni.Free;
  MStr.Free;
end;

procedure TFPSData.AddFile(FileType: TMapDataFileType; FileName: string; MStr: TMemoryStream);
var
  Path: string;
  SL: TStringList;
begin
  if not Opened then exit;
  SL := SLArray[FileType];
  Path := ArchiveDirs[FileType];
  if SL.IndexOf(ExtractFileName(FileName)) = -1 then
    SL.Add(ExtractFileName(FileName));
  MStr.Position := 0;
  Packer.AddFromStream(Path + ExtractFileName(FileName), MStr);
  Update;
end;

procedure TFPSData.DeleteFile(FileType: TMapDataFileType; FileName: string);
var
  Path: string;
  i: integer;
  SL: TStringList;
begin
  if not Opened then exit;
  SL := SLArray[FileType];
  i := SL.IndexOf(FileName);
  SL.Delete(i);
  Path := ArchiveDirs[FileType];
  Packer.DeleteFiles(Path + FileName);
  Update;
end;

procedure TFPSData.GetFile(FileType: TMapDataFileType; FileName: string; MStr: TMemoryStream);
var Path: string;
begin
  if not Opened then exit;
  Path := ArchiveDirs[FileType];
  MStr.Clear;
  Packer.ExtractToStream(Path + FileName, MStr);
  MStr.Position := 0;
end;

procedure TFPSData.SaveStaticObjects(MStr: TMemoryStream);
var
  Header: TFileHeader;
  i, Count: integer;
begin
  FillChar(Header, 5, 1);
  MStr.Clear;
  MStr.Write(Header, SizeOf(Header));
  Count := StaticObjects.Count;
  MStr.Write(Count, SizeOf(Count));

  for i := 0 to Count - 1 do
    TFPSBasicSprite(StaticObjects[i]).SaveToStream(MStr, true);
end;

procedure TFPSData.LoadStaticObjects(MStr: TMemoryStream);
var
  Header: TFileHeader;
  i, Count: integer;
  N: string;
  ItemClass: TFPSBasicClass;
  FPSBO: TFPSBasicSprite;
begin
  MStr.Position := 0;
  if MStr.Read(Header, SizeOf(Header)) = 0 then exit;
  MStr.Read(Count, SizeOf(Count));

  for i := 0 to Count - 1 do
  begin
    N := LoadStringFromStream(MStr);
    ItemClass := FPSClassList.FindByName(N);
    if ItemClass = nil then
    begin
      AddLog('Nieznany obiekt: %s', [N]);
      Break;
    end;
    FPSBO := ItemClass.Create(OwnerEngine, nil);
    FPSBO.LoadFromStream(MStr, true);
    StaticObjects.Add(FPSBO);
  end;
end;

constructor TFPSData.Create;
begin
  Opened := false;
  Maps := TList.Create;
  StaticObjects := TList.Create;
  Textures := TStringList.Create;
  Fonts := TStringList.Create;
  Sounds := TStringList.Create;
  Scripts := TStringList.Create;
  MenuMusic := TStringList.Create;
  SLArray[mdftTexture] := Textures; 
  SLArray[mdftFont] := Fonts;
  SLArray[mdftSound] := Sounds;
  SLArray[mdftScript] := Scripts;
  Packer := TZipForge.Create(Application);
  StartMap := '';
end;

destructor TFPSData.Destroy;
begin
  Clear;
  Textures.Free;
  Fonts.Free;
  Sounds.Free;
  Maps.Free;
  Scripts.Free;
  MenuMusic.Free;
  StaticObjects.Free;
//  Packer.Free;
  inherited;
end;

procedure TFPSData.Clear;
var i: Integer;
begin
  Textures.Clear;
  Fonts.Clear;
  Sounds.Clear;
  Scripts.Clear;
  MenuMusic.Clear;
  for i := 0 to Maps.Count - 1 do TFPSMap(Maps[i]).Free;
  Maps.Clear;
  for i := 0 to StaticObjects.Count - 1 do TFPSBasicSprite(StaticObjects[i]).Free;
  StaticObjects.Clear;
  Packer.CloseArchive;
  Packer.FileName := '';
end;

function TFPSData.GetStartMap: TFPSMap;
var i: Integer;
begin
  for i := 0 to Maps.Count - 1 do
    if CompareStr(StartMap, TFPSMap(Maps[i]).MapName) = 0 then
    begin
      Result := TFPSMap(Maps[i]);
      Exit;
    end;
  Result := TFPSMap(Maps[0]);
end;

function TFPSData.GetStaticObject(ObjName: string): TFPSBasicSprite;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to StaticObjects.Count - 1 do
  if CompareStr(TFPSBasicSprite(StaticObjects[i]).Name, ObjName) = 0 then
  begin
    Result := TFPSBasicSprite(StaticObjects[i]);
    exit;
  end;
end;

{   TFPSBasicSprite   }

procedure TFPSBasicSprite.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  SaveStringToStream([Self.ClassName, FTextureName], Stream);
  Stream.Write(DrawPosition, SizeOf(DrawPosition));
  SaveBoolToStream([Collision], Stream);
end;

procedure TFPSBasicSprite.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin
  FTextureName := LoadStringFromStream(Stream);
  Stream.Read(DrawPosition, SizeOf(DrawPosition));  
  Collision := LoadBoolFromStream(Stream);
end;

procedure TFPSBasicSprite.Assign(Item: TFPSBasicSprite);
begin
  if Item = nil then exit;  
  FTextureName := Item.FTextureName;
  Collision := Item.Collision;
  FScriptName := Item.FScriptName;
  FScriptFunction := Item.FScriptFunction;
  Name := Item.Name;
  if OwnerEngine <> nil then FTexture := OwnerEngine.GetTexture(TextureName);
  
  RunScript(beCreate);  
end;

constructor TFPSBasicSprite.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  OwnerEngine := aEngine;
  OwnerMap := aOwner;
  Collision := false;
  FVisible := true;
  FScriptName := '';
  FScriptFunction := '';
  Name := '';
  Destroyable := false;
  fDeaded := false;
  ScriptEvents := [beCreate, beDead, beAnimationLoop, beCollision, beUse];
end;

destructor TFPSBasicSprite.Destroy();
begin
  inherited;
end;

procedure TFPSBasicSprite.RunScript(Event: TFPSBasicEvent; Id: integer = 0);
begin
  if(OwnerEngine = nil)or(ScriptName = '')or(ScriptFunction = '')then exit;
  if Event in ScriptEvents then
  begin
    if OwnerEngine.RunEventScript(FScriptName, FScriptFunction, Self, Event, Id) = 0 then
      Exclude(ScriptEvents, Event);
  end;
end;

procedure TFPSBasicSprite.Dead;
begin
  FDeaded := true;
  RunScript(beDead);
  OwnerEngine.FDeadList.Add(Self);
end;

procedure TFPSBasicSprite.Hit;
begin
end;

procedure TFPSBasicSprite.DoDraw;
begin
  if FVisible then OwnerEngine.DrawTexture(Self);
end;

procedure TFPSBasicSprite.DoMove;
begin
end;

{   TFPSBasicObject   }

procedure TFPSBasicObject.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  SaveStringToStream([Self.ClassName, Name], Stream);
  if not SaveAll then
    Stream.Write(Position, SizeOf(Position))
  else
  begin
    SaveStringToStream([FTextureName, FScriptName, FScriptFunction], Stream);
    SaveBoolToStream([FVisible, FDestroyable, FCollision], Stream);
    Stream.Write(Position.Y, SizeOf(Position.Y));
    Stream.Write(FWidth, SizeOf(FWidth));
    Stream.Write(FHeight, SizeOf(FHeight));
    Stream.Write(FHitPoints, SizeOf(FHitPoints));    
  end;
end;

procedure TFPSBasicObject.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin   
  Name := LoadStringFromStream(Stream);
  if not LoadAll then
  begin
    Stream.Read(Position, SizeOf(Position));
    Assign(OwnerMap.Owner.GetStaticObject(Name));
    Position.Y := 0;
  end
  else
  begin
    FTextureName := LoadStringFromStream(Stream);
    FScriptName := LoadStringFromStream(Stream);
    FScriptFunction := LoadStringFromStream(Stream);
    FVisible := LoadBoolFromStream(Stream);
    FDestroyable := LoadBoolFromStream(Stream);
    FCollision := LoadBoolFromStream(Stream);
    Stream.Read(Position.Y, SizeOf(Position.Y));
    Stream.Read(FWidth, SizeOf(FWidth));
    Stream.Read(FHeight, SizeOf(FHeight));
    Stream.Read(FHitPoints, SizeOf(FHitPoints));
  end;
end;

procedure TFPSBasicObject.Assign(Item: TFPSBasicSprite);
var Obj: TFPSBasicObject;
begin
  inherited Assign(Item);
  if Item is TFPSBasicObject then
  begin
    Obj := TFPSBasicObject(Item);
    Destroyable := Obj.Destroyable;
    Visible := Obj.Visible;
    Width := Obj.Width;
    Height := Obj.Height;
    FHitPoints := Obj.HitPoints;
    Position := Sum3DPoints(Obj.Position, Position);
    with DrawPosition do
    begin
      V1.Y := Position.Y;
      V2.Y := Position.Y;
      V3.Y := Position.Y + Height;
      V4.Y := Position.Y + Height;
    end;       
  end;
end;

constructor TFPSBasicObject.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  FCollision := false;
  FDestroyable := false;
  Position := Make3DPoint(0, 0, 0);
end;

procedure TFPSBasicObject.SetYPos(Value: Single);
begin
  Position.Y := Value;
end;

function TFPSBasicObject.GetYPos: Single;
begin
  Result := Position.Y;
end;

destructor TFPSBasicObject.Destroy();
begin
  inherited;
end;

procedure TFPSBasicObject.Hit(Power: Single);
begin
  if not Destroyable then exit;
  FHitPoints := FHitPoints - Power;
  if FHitPoints <= 0 then
    Dead;  
end;

{   TFPSBasicBullet   }

procedure TFPSBasicBullet.DoDraw;
begin
  //nic
end;

procedure TFPSBasicBullet.DoMove;
var i, j: Integer;
  Obj: TFPSBasicSprite;
begin
  if GetTickCount - CreateTime > LifeTime then
  begin
    Dead;
    Exit;
  end;
  for i := 1 to SpeedMultiplier do
  begin
    Position := Sum3DPoints(Position, Speed);
    j := 0;
    while j < OwnerMap.Objects.Count do
    begin
      Obj := TFPSBasicSprite(OwnerMap.Objects[j]);
      if Obj is TFPSBasicObject then
      if(Obj.Collision)and(Is2DCollision(Obj.DrawPosition, Position))then
      begin
        Dead;
        Obj.Hit(50);
        Exit;
      end;
      Inc(j);
    end;
  end;
end;

procedure TFPSBasicBullet.SaveToStream(Stream: TStream; SaveAll: bool = false);
var LifeTimeLeft: Cardinal;
begin
  SaveStringToStream([Self.ClassName, Name], Stream);
  if not SaveAll then
  begin
    Stream.Write(Position, SizeOf(Position));
    LifeTimeLeft := LifeTime - (GetTickCount - CreateTime);
    Stream.Write(LifeTimeLeft, SizeOf(LifeTimeLeft));
  end
  else
  begin
    Stream.Write(BasicSpeed, SizeOf(BasicSpeed));
    Stream.Write(SpeedMultiplier, SizeOf(SpeedMultiplier));
    Stream.Write(LifeTime, SizeOf(LifeTime));
    SaveStringToStream([FScriptName, FScriptFunction], Stream);
  end;
end;

procedure TFPSBasicBullet.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin
  Name := LoadStringFromStream(Stream);
  if not LoadAll then
  begin
    Stream.Read(Position, SizeOf(Position));
    Assign(OwnerMap.Owner.GetStaticObject(Name));
    Stream.Read(FLifeTime, SizeOf(FLifeTime));
  end
  else
  begin
    Stream.Read(FBasicSpeed, SizeOf(FBasicSpeed));
    Stream.Read(FSpeedMultiplier, SizeOf(FSpeedMultiplier));
    Stream.Read(FLifeTime, SizeOf(FLifeTime));
    FScriptName := LoadStringFromStream(Stream);
    FScriptFunction := LoadStringFromStream(Stream);
  end;
end;

procedure TFPSBasicBullet.Assign(Item: TFPSBasicSprite);
var Obj: TFPSBasicBullet;
begin
  if Item is TFPSBasicBullet then
  begin
    Obj := TFPSBasicBullet(Item);
    BasicSpeed := Obj.BasicSpeed;
    SpeedMultiplier := Obj.SpeedMultiplier;
    LifeTime := Obj.LifeTime;
    CreateTime := GetTickCount;
    Position := OwnerEngine.Player.Position;
    Speed.X := BasicSpeed *  sin(OwnerEngine.Player.Direction * pi / 180) * OwnerEngine.FFrameTime * 500;
    Speed.Z := BasicSpeed * -cos(OwnerEngine.Player.Direction * pi / 180) * OwnerEngine.FFrameTime * 500;
  end;
  inherited;
end;

{   TFPSBasicWeapon   }

procedure TFPSBasicWeapon.RunScript(Event: TFPSBasicEvent; Id: integer = 0);
begin
  if Event = beAnimationLoop then
  begin
    FCanShot := true;
    FTexture.AnimPos := 0;
  end;
  inherited;
end;

procedure TFPSBasicWeapon.Shot;
begin
  if not CanShot then exit;
  FCanShot := false;
  FTexture.AnimPos := 0;
  FTexture.Animated := true;
  OwnerEngine.ActiveMap.CreateObject(BulletName, OwnerEngine.Player.Position);
  OwnerEngine.PlaySound(ShotSound);
end;

procedure TFPSBasicWeapon.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  SaveStringToStream([Self.ClassName, Name], Stream);
  if not SaveAll then
  begin
    //nic
  end
  else
  begin
    SaveStringToStream([BulletName, FTextureName, ShotSound, FScriptName,
                        FScriptFunction], Stream);
  end;
end;

procedure TFPSBasicWeapon.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin
  Name := LoadStringFromStream(Stream);
  if not LoadAll then
  begin
    //nic
  end
  else
  begin
    BulletName := LoadStringFromStream(Stream);
    FTextureName := LoadStringFromStream(Stream);
    FShotSound := LoadStringFromStream(Stream);
    FScriptName := LoadStringFromStream(Stream);
    FScriptFunction := LoadStringFromStream(Stream);
  end;
end;

procedure TFPSBasicWeapon.Assign(Item: TFPSBasicSprite);
var Obj: TFPSBasicWeapon;
begin
  if Item is TFPSBasicWeapon then
  begin
    Obj := TFPSBasicWeapon(Item);
    BulletName := Obj.BulletName;
    ShotSound := Obj.ShotSound;
    FCanShot := true;
  end;
  inherited;
end; 

{   TFPSBasicMonster   }

procedure TFPSBasicMonster.DoMove;
var i: Integer;
    Collisioned: bool;
    Alfa: GLfloat;
    NewPosition: T3Dpoint;
    Obj: TFPSBasicSprite;
begin
//atak
  if IsCollision(DrawPosition, OwnerEngine.Player.Position, 3) then
  begin
    if GetTickCount - LastAttack >= AtackSpeed then
    begin
      LastAttack := GetTickCount;
      OwnerEngine.Player.HitPoints := OwnerEngine.Player.HitPoints - Attack;
    end;
  end else
//ruch
  if Distance2d(Position, OwnerEngine.Player.Position) <= SeeRange then
  begin
    Alfa := ArcSin((OwnerEngine.Player.Position.Z - Position.Z) / Distance2d(OwnerEngine.Player.Position, Position));
    Alfa := Alfa + Pi / 2;    
    NewPosition := Sum3DPoints(Position, Make3DPoint(-Sin(Alfa) * Speed, 0, -Cos(Alfa) * Speed));
    i := 0;
    Collisioned := false;

    while(i < OwnerMap.Objects.Count)and(not Collisioned) do
    begin
      Obj := TFPSBasicSprite(OwnerMap.Objects[i]);
      Inc(i);
      if Obj = Self then Continue;
      if(Obj.Collision)and(IsCollision(Obj.DrawPosition, Position))then
        Collisioned := true;
    end;
    if not Collisioned then
    begin
      Position := NewPosition;
      OwnerEngine.ReObjectPos(Self);
    end;
  end;
end;

procedure TFPSBasicMonster.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  inherited;
  if SaveAll then
  begin
    Stream.Write(FAttack, SizeOf(FAttack));
    Stream.Write(FSpeed, SizeOf(FSpeed));
    Stream.Write(FAtackSpeed, SizeOf(FAtackSpeed));
    Stream.Write(FSeeRange, SizeOf(FSeeRange));
  end;
end;

procedure TFPSBasicMonster.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin
  inherited;
  if LoadAll then
  begin
    Stream.Read(FAttack, SizeOf(FAttack));
    Stream.Read(FSpeed, SizeOf(FSpeed));
    Stream.Read(FAtackSpeed, SizeOf(FAtackSpeed));
    Stream.Read(FSeeRange, SizeOf(FSeeRange));
  end;
end;

procedure TFPSBasicMonster.Assign(Item: TFPSBasicSprite);
var Obj: TFPSBasicMonster;
begin
  inherited;
  if Item is TFPSBasicMonster then
  begin
    Obj := TFPSBasicMonster(Item);
    FAttack := Obj.Attack;
    FSpeed := Obj.Speed;
    FAtackSpeed := Obj.AtackSpeed;
    FSeeRange := Obj.SeeRange;
    LastAttack := GetTickCount;
  end;
end;

{   TFPSBasicKillZone   }

procedure TFPSBasicKillZone.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  inherited;
  if SaveAll then
  begin
    Stream.Write(FRange, SizeOf(FRange));
    Stream.Write(FDmgPerSek, SizeOf(FDmgPerSek));
  end;
end;

procedure TFPSBasicKillZone.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin
  inherited;
  if LoadAll then
  begin
    Stream.Read(FRange, SizeOf(FRange));
    Stream.Read(FDmgPerSek, SizeOf(FDmgPerSek));    
  end;
end;

procedure TFPSBasicKillZone.Assign(Item: TFPSBasicSprite);
var Obj: TFPSBasicKillZone;
begin
  inherited;
  if Item is TFPSBasicKillZone then
  begin
    Obj := TFPSBasicKillZone(Item);
    FRange := Obj.Range;
    FDmgPerSek := Obj.FDmgPerSek;
  end;
end;

constructor TFPSBasicKillZone.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Emitter := nil;
end;

procedure TFPSBasicKillZone.DoMove;
var
  i: integer;
  Obj: TFPSBasicObject;
  Dst: single;
begin
  for i := 0 to OwnerMap.Objects.Count - 1 do
  if TFPSBasicSprite(OwnerMap.Objects[i]) is TFPSBasicObject then
  begin
    Obj := TFPSBasicObject(OwnerMap.Objects[i]);
    if(Obj.Deaded)or(not Obj.Destroyable) then Continue;
    Dst := Distance2d(Position, Obj.Position);
    if Dst < Range then
      Obj.Hit((FDmgPerSek / 20) * Dst / (Range - Dst));
  end;

  Dst := Distance2d(Self.Position, OwnerEngine.Player.Position);
  if Dst < Range then
    OwnerEngine.Player.Hit((FDmgPerSek / 20) * (Dst / (Range - Dst)));

  if(Emitter <> nil)and(Emitter.Deaded)then Dead;
end;

procedure TFPSBasicKillZone.DoDraw;
begin
end;

{   TFPSBasicEventField   }

procedure TFPSBasicEventField.DoMove;
begin
  if(EventSource = esCollision)and
    IsAreaCollision(EventData.EventArea, OwnerEngine.Player.Position) then
  begin
    case EventType of
      etTeleport: OwnerEngine.TeleportPlayer(EventData.DString, EventData.DPoint, EventData.DPoint.Y);
      etScript: RunScript(beCollision, EventData.Id);
    end;
  end;
end;

constructor TFPSBasicEventField.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  FVisible := false;
end;

procedure TFPSBasicEventField.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  SaveStringToStream([Self.ClassName], Stream);
  Stream.Write(Byte(EventSource), SizeOf(Byte));
  Stream.Write(Byte(EventType), SizeOf(Byte));
  Stream.Write(EventData.Id, SizeOf(EventData.Id));
  Stream.Write(EventData.EventArea, SizeOf(EventData.EventArea));
  case EventType of
    etTeleport:
    begin
      SaveStringToStream([EventData.DString], Stream);
      Stream.Write(EventData.DPoint, SizeOf(EventData.DPoint))
    end;
    etScript: SaveStringToStream([EventData.DString], Stream);
  end;
end;

procedure TFPSBasicEventField.LoadFromStream(Stream: TStream; LoadAll: bool = false);
var b: Byte;
begin
  Stream.Read(b, SizeOf(b));
  EventSource := tfpsEventSource(b);
  Stream.Read(b, SizeOf(b));
  EventType := tfpsEventType(b);
  Stream.Read(EventData.Id, SizeOf(EventData.Id));
  Stream.Read(EventData.EventArea, SizeOf(EventData.EventArea));
  case EventType of
    etTeleport:
    begin
      EventData.DString := LoadStringFromStream(Stream);
      Stream.Read(EventData.DPoint, SizeOf(EventData.DPoint));
    end;
    etScript: EventData.DString := LoadStringFromStream(Stream);
  end;
end;

procedure TFPSBasicEventField.Assign(Item: TFPSBasicSprite);
begin
  inherited;
end;

{   TFPSBasicDoor   }

procedure TFPSBasicDoor.DoMove;
var
  DPos: T3DPoint;
  dV: Single;
begin
  if FInMove then
  begin
    case FMoveDirection of
      1: DrawPosition := AddToDrawPosition(DrawPosition,  Speed, 0, 0);//lewo
      2: DrawPosition := AddToDrawPosition(DrawPosition, -Speed, 0, 0);//prawo
      3: DrawPosition := AddToDrawPosition(DrawPosition, 0, 0,  Speed);//góra
      4: DrawPosition := AddToDrawPosition(DrawPosition, 0, 0, -Speed);//dó³
      5: DrawPosition := AddToDrawPosition(DrawPosition, 0,  Speed, 0);//do nieba
      6: DrawPosition := AddToDrawPosition(DrawPosition, 0, -Speed, 0);//do ziemi
    end;
  end;
  DPos := GetVector(DrawPosition.V1, FStartPos);
  case FMoveDirection of
    1, 2: dV := DPos.X;
    3, 4: dV := DPos.Z;
    5, 6: dV := DPos.Y;
    else exit;
  end;
  case FMovePhase of
    1:if Abs(dV) >= 1.5 then
      begin
        FMovePhase := 2;
        if Odd(FMoveDirection) then Inc(FMoveDirection)
        else Dec(FMoveDirection);
        FOpenedTime := GetTickCount;
        FInMove := false;
      end;
    2:if GetTickCount - FOpenedTime > FOpenTime then
      begin
        if FOpenTime = 0 then
        begin
          FMovePhase := 4;
          exit;
        end;
        FMovePhase := 3;
        FInMove := true;
      end;
    3:if Abs(dV) < Speed then
      begin
        FMovePhase := 4;
        FMoveDirection := 0;
        FInMove := false;
      end;
  end; 
end;

procedure TFPSBasicDoor.RunScript(Event: TFPSBasicEvent; Id: integer = 0);
begin
  inherited;
  if Event = beUse then
  begin
    FInMove := true;
    FMovePhase := 1;
    FMoveDirection := FOpeningDirection;
  end;
end;

procedure TFPSBasicDoor.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  SaveStringToStream([Self.ClassName], Stream);
  Stream.Write(DrawPosition, SizeOf(DrawPosition));
  SaveStringToStream([FTextureName], Stream);
  Stream.Write(FSpeed, SizeOf(FSpeed));
  Stream.Write(FOpeningDirection, SizeOf(FOpeningDirection));
  Stream.Write(FOpenTime, SizeOf(FOpenTime));
end;

procedure TFPSBasicDoor.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin
  Stream.Read(DrawPosition, SizeOf(DrawPosition));
  FTextureName := LoadStringFromStream(Stream);
  Stream.Read(FSpeed, SizeOf(FSpeed));
  Stream.Read(FOpeningDirection, SizeOf(FOpeningDirection));
  Stream.Read(FOpenTime, SizeOf(FOpenTime));
  FCollision := true;
  FStartPos := DrawPosition.V1;
end;

procedure TFPSBasicDoor.Assign(Item: TFPSBasicSprite);
begin
  inherited;
end;

{   TFPSBasicItem   }

procedure TFPSBasicItem.RunScript(Event: TFPSBasicEvent; Id: integer = 0);
begin
  if Event = beCollision then
  begin
    OwnerEngine.PutInBackpack(Self);
  end else inherited;
end;

procedure TFPSBasicItem.DoItemProperty(WhichProperty: TFPSItemProprtyType);
var
  SIni: TIni;
  Str: TStringList;
  i, PropMultiplier: integer;
  PropName, PropOwner, PropValue: string;
begin
  SIni := TIni.Create('');
  SIni.SetText(FPropertyData);
  Str := TStringList.Create;
  SIni.ReadSection(ItemSections[WhichProperty], Str);
  if WhichProperty = iptUnEquip then PropMultiplier := -1
  else PropMultiplier := 1;

  for i := 0 to Str.Count - 1 do
  begin
    PropName := Str[i];
    PropValue := SIni.ReadString(ItemSections[WhichProperty], PropName, '');
    if Pos('.', PropName) > 0 then
    begin
      PropOwner := Copy(PropName, 0, Pos('.', PropName) - 1);
      Delete(PropName, 1, Pos('.', PropName));
    end else PropOwner := '';
    OwnerEngine.ParseItemProp(PropOwner, PropName, PropValue, PropMultiplier);
  end;

  SIni.Free;
  Str.Free;
end;

procedure TFPSBasicItem.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  inherited;
  if SaveAll then
  begin
    SaveStringToStream([FPropertyData], Stream);
  end else
  begin
    Stream.Write(FItemsCount, SizeOf(FItemsCount));
  end;
end;

procedure TFPSBasicItem.LoadFromStream(Stream: TStream; LoadAll: bool = false);
begin
  inherited;
  if LoadAll then
  begin
    FPropertyData := LoadStringFromStream(Stream);
  end else
  begin
    Stream.Read(FItemsCount, SizeOf(FItemsCount));
  end;     
end;

procedure TFPSBasicItem.Assign(Item: TFPSBasicSprite);
var Obj: TFPSBasicItem;
begin
  if Item is TFPSBasicItem then
  begin
    Obj := TFPSBasicItem(Item);
    FItemsCount := Obj.ItemsCount;
    FPropertyData := Obj.ProperyData;
  end;
  inherited;
end;

{ TFPSPlayer }

procedure TFPSPlayer.Hit(Power: Single);
begin
  HitPoints := HitPoints - Power;
end;

{   TScriptEngine   }

type
  PScript = ^TScript;
  TScript = record
    Name: String;
    Code: String;
  end;

procedure CreateObject(Name: string; Creator: TFPSBasicSprite);
begin
  Creator.OwnerMap.CreateObject(Name, TFPSBasicObject(Creator).Position);
end;

procedure CreateKillZone(Range, DmgPerSek: Single; Creator: TFPSBasicObject);
begin
  Creator.OwnerMap.CreateKillZone(Range, DmgPerSek, Creator);
end;

procedure TeleportPlayer(MapName: string; pX, pZ, Direction: Single; Sender: TFPSBasicSprite);
begin
  Sender.OwnerEngine.TeleportPlayer(MapName, Make3DPoint(pX, 0, pZ), Direction);
end;

function TScriptEngine.AddScript(ScriptName, Code: String; Messages: PString = nil): Boolean;
var
  Script: PScript;
  ScriptCode: string;
  i: Longint;
begin
  Result := false;
  if(FCompiler.Compile(Code))and(FCompiler.GetOutput(ScriptCode))then
  begin
    New(Script);
    Script^.Name := ScriptName;
    Script^.Code := ScriptCode;
    Scripts.Add(Script);
    Result := true;
  end;
  if Messages <> nil then
  begin
    Messages^ := '';
    for i := 0 to FCompiler.MsgCount - 1 do
      Messages^ := Messages^ + #13#10 + Compiler.Msg[i].MessageToString;
  end;
end;

procedure TScriptEngine.LoadExec(se: TPSExec; Code: string);
var S: string;
begin
  RegisterClassLibraryRuntime(FExec, FClassImporter);
  se.RegisterDelphiFunction(@CreateObject, 'CreateObject', cdRegister);
  se.RegisterDelphiFunction(@CreateKillZone, 'CreateKillZone', cdRegister);
  se.RegisterDelphiFunction(@TeleportPlayer, 'TeleportPlayer', cdRegister);

  Se.LoadData(Code);

  SetVariantToClass(se.GetVarNo(se.GetVar('Self')), SelfPointer);
  S := SelfPointer.ClassName;
  SetVariantToClass(se.GetVarNo(se.GetVar('Self_' + S[5] + S[10])), SelfPointer);
end;

function TScriptEngine.OnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
begin
  Result := false;
  if CompareText('system', Name) = 0 then
  begin
    SIRegister_Std(Sender);
    Result := true;
  end else
  if CompareText('game', Name) = 0 then
  begin
    SIRegister_FPSEngine(Sender);

    AddImportedClassVariable(Sender, 'Self', 'TFPSBasicSprite');
    AddImportedClassVariable(Sender, 'Self_BO', 'TFPSBasicObject');
    AddImportedClassVariable(Sender, 'Self_BB', 'TFPSBasicBullet');
    AddImportedClassVariable(Sender, 'Self_BW', 'TFPSBasicWeapon');
    AddImportedClassVariable(Sender, 'Self_BM', 'TFPSBasicMonster');

    Sender.AddDelphiFunction('procedure CreateObject(Name: string; Creator: TFPSBasicSprite);');
    Sender.AddDelphiFunction('procedure CreateKillZone(Range, DmgPerSek: Single; Creator: TFPSBasicObject);');
    Sender.AddDelphiFunction('procedure TeleportPlayer(MapName: string; pX, pZ, Direction: Single; Sender: TFPSBasicSprite);');
    Result := true;
  end;
end;

procedure TScriptEngine.RunScript(ScriptName: String);
var Script: PScript;
begin
  Script := GetByName(ScriptName);
  if Script = nil then exit;
  LoadExec(FExec, Script^.Code);
  FExec.RunScript;
end;

function TScriptEngine.RunFunction(ScriptName, FunctionName: string; Params: array of Variant): Variant;
var
  Script: PScript;
  ProcNo: Cardinal;
begin
  Script := GetByName(ScriptName);
  Result := 0;
  if Script = nil then exit;
  LoadExec(FExec, Script^.Code);
  ProcNo := FExec.GetProc(FunctionName);
  if ProcNo = InvalidVal then
    AddLog('Funkcja %s nie istnieje w skrypcie %s', [FunctionName, ScriptName])
  else
    Result := FExec.RunProcP(Params, ProcNo);
end;

function TScriptEngine.CheckScript(Code: String; Messages: TStrings): boolean;
var i: integer;
begin
  Result := FCompiler.Compile(Code);
  if Messages <> nil then
  begin
    for i := 0 to FCompiler.MsgCount - 1 do
      Messages.Add(Compiler.Msg[i].MessageToString);
  end;
end;

procedure TScriptEngine.Clear;
var i: Integer;
begin
  for i := 0 to Scripts.Count - 1 do
    Dispose(PScript(Scripts[i]));
  Scripts.Clear;
end;

function IntOnUses(Sender: TPSPascalCompiler; const Name: string): Boolean;
begin
  Result := TScriptEngine(Sender.ID).OnUses(Sender, Name);
end;

constructor TScriptEngine.Create;
begin
  inherited;
  Scripts := TList.Create;
  FCompiler := TPSPascalCompiler.Create;
  FCompiler.ID := Self;
  FCompiler.OnUses := IntOnUses;
  FCompiler.AllowNoBegin := true;
  FCompiler.AllowUnit := true;
  FCompiler.AllowNoEnd := true;
  FExec := TPSExec.Create;
  FExec.Id := Self;
  FClassImporter := TPSRuntimeClassImporter.Create;
  RIRegister_FPSEngine(FClassImporter);
end;

destructor TScriptEngine.Destroy;
begin
  Clear;
  Scripts.Free;
  FCompiler.Free;
  FExec.Free;
  FClassImporter.Free;
  inherited;
end;

function TScriptEngine.GetByName(Name: string): Pointer;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Scripts.Count - 1 do
  if PScript(Scripts[i])^.Name = Name then
  begin
    Result := Scripts[i];
    Exit;
  end;
end;

{   TAudioSystem   }

type
  TSoundType = (stStream, stMusic);
  PSound = ^TSound;
  TSound = record
    Handle: Cardinal;
    MS: TMemoryStream;
    Name: string;
    Typ: TSoundType;
  end;

procedure TAudioSystem.Play(SndName: string);
var i: integer;
begin
  if not FEnabled then exit;  
  for i := 0 to FSounds.Count - 1 do
  if CompareStr(PSound(FSounds[i])^.Name, SndName) = 0 then
  begin
    BASS_ChannelPlay(PSound(FSounds[i])^.Handle, true);
    Exit;
  end;
  AddLog('DŸwiêk "%s" nie istnieje', [SndName])
end;

procedure TAudioSystem.PlayListPlay;
var
  i, SndLen: integer;
  SndTime: single;
  Aud: PSound;
  SndName: string;
begin
  if(not FEnabled)or(FPlayList.Count = 0)then exit;
  
  if FPlayListPos = FPlayList.Count then FPlayListPos := 0;
  Randomize;
  if FPlayRandom then FPlayListPos := Random(FPlayList.Count);
  SndName := FPlayList[FPlayListPos];

  Aud := nil;
  for i := 0 to FSounds.Count - 1 do
  if CompareStr(PSound(FSounds[i])^.Name, SndName) = 0 then
  begin
    Aud := PSound(FSounds[i]);
    Break;
  end;
  Inc(FPlayListPos);
  if Aud = nil then Exit;

  SndLen := BASS_ChannelGetLength(Aud^.Handle, BASS_POS_BYTE);
  SndTime := BASS_ChannelBytes2Seconds(Aud^.Handle, SndLen);
  BASS_ChannelPlay(Aud^.Handle, true);
  SndTime := (SndTime * 1000) + 1000;
  FTimerHandle := SetTimer(FHandle, 1, Trunc(SndTime), nil);
end;

procedure TAudioSystem.SetEnabled(Value: bool);     
begin
  FEnabled := Value;
  if not FEnabled then
    StopAll
  else
    PlayListPlay;
end;

procedure TAudioSystem.SetPlayList(List: TStringList; PlayRandom: bool);
begin
  if List.Equals(FPlayList) then exit;
  StopAll;
  if(List = nil)or((List <> nil)and(List.Count = 0))then
  begin
    KillTimer(FHandle, FTimerHandle);
    Exit;
  end;
  FPlayListPos := 0;
  FPlayRandom := PlayRandom; 
  FPlayList.Assign(List);
  PlayListPlay;
end;

procedure TAudioSystem.StopAll;
var i: Integer;
begin
  for i := 0 to FSounds.Count - 1 do
    BASS_ChannelPause(PSound(FSounds[i])^.Handle)
end;

procedure TAudioSystem.WndProc(var Msg: TMessage);
begin
  with Msg do
    if Msg = WM_TIMER then
      if WParam = FTimerHandle then
      begin //zmiana muzyki
        KillTimer(FHandle, FTimerHandle);
        PlayListPlay;
      end
    else
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TAudioSystem.AddSound(FileName: string; Data: TStream);
var
  Aud: PSound;
  MStr: TMemoryStream;
  S: String;
begin
  MStr := TMemoryStream.Create;
  
  if Data <> nil then
  begin
    Data.Position := 0;
    MStr.CopyFrom(Data, Data.Size);
  end else
  if FileExists(FileName) then
  begin
    MStr.LoadFromFile(FileName);
  end else
  begin
    MStr.Free;
    AddLog('Nie mogê za³adowaæ dŸwiêku %s', [FileName]);
    Exit;
  end;
  MStr.Position := 0;            

  S := ExtractFileExt(FileName);
  New(Aud);
  FSounds.Add(Aud);
  Aud^.Name := GetFileNameNoExt(FileName);
  Aud^.MS := TMemoryStream.Create;
  MStr.SaveToStream(Aud^.MS);
  Aud^.MS.Position := 0;
  if CompareExtensions(S, ['MP3', 'MP2', 'MP1', 'WAV', 'OGG']) then
  begin
    Aud^.Handle := BASS_StreamCreateFile(true, Aud^.MS.Memory, 0, Aud^.MS.Size, BASS_STREAM_PRESCAN);
    Aud^.Typ := stStream;
  end else
  if CompareExtensions(S, ['MO3', 'IT', 'XM', 'S3M', 'MTM', 'MOD', 'UMX']) then
  begin
    Aud^.Handle := BASS_MusicLoad(true, Aud^.MS.Memory, 0, Aud^.MS.Size, BASS_MUSIC_PRESCAN, 0);
    Aud^.Typ := stMusic;
  end
  else addLog('Nieznany typ pliku %s', [S]);
  MStr.Free;
end;

procedure TAudioSystem.Clear;
var i: Integer;
begin
  for i := 0 to FSounds.Count - 1 do
  with PSound(FSounds[i])^ do
  begin
    case Typ of
      stStream: BASS_StreamFree(Handle);
      stMusic: BASS_MusicFree(Handle);
    end;
    MS.Free;
    Dispose(PSound(FSounds[i]));
  end;
  FSounds.Clear;
  FPlayList.Clear;
  KillTimer(FHandle, FTimerHandle);
end;

constructor TAudioSystem.Create;
begin
  inherited;
  FEnabled := true;
  if not BASS_Init(1, 44100, 0, Application.Handle, nil) then
    raise Exception.Create('Nie mogê zainicjowaæ bibljoteki Bass!')
  else BASS_Start;

  FPlayList := TStringList.Create;
  FSounds := TList.Create;
  FHandle := Classes.AllocateHWnd(WndProc)
end;

destructor TAudioSystem.Destroy;
begin
  Clear;
  Classes.DeallocateHWnd(FHandle);
  FPlayList.Free;
  FSounds.Free;
  BASS_Stop;
  BASS_Free;
  inherited;
end;

{ TThreadEngine }

procedure TThreadEngine.AfterConstruction;
begin
  inherited;
  Engine := TFPSEngine.Create;
end;

procedure TThreadEngine.BeforeDestruction;
begin
  inherited;
  Engine.Free;
end;

procedure TThreadEngine.DoEnd;
begin
  Engine.DoEnd;
end;

procedure TThreadEngine.Execute;
begin
  inherited;
  if not Engine.Initialised then Engine.Initialise;
  
  Engine.Run; 
  FreeOnTerminate := true;
  Terminate;
end;

function TThreadEngine.GetEnded: bool;
begin
  Result := Engine.Ended;
end;

{   TFPSClassList   }

type
  PFPSClassListItem = ^TFPSClassListItem;
  TFPSClassListItem = record
    ItemClass: TFPSBasicClass;
    ItemName: ShortString;
  end;

procedure TFPSClassList.Add(AClass: TFPSBasicClass; AName: ShortString);
var NewRec: PFPSClassListItem;
begin
  New(NewRec);
  NewRec^.ItemClass := AClass;
  NewRec^.ItemName := AName;
  inherited Add(NewRec);
end;

constructor TFPSClassList.Create;
begin
  inherited;
  Add(TFPSBasicSprite);
  Add(TFPSBasicObject);
  Add(TFPSBasicBullet);
  Add(TFPSBasicWeapon);
  Add(TFPSBasicMonster);
  Add(TFPSBasicKillZone);
  Add(TFPSBasicEventField);
  Add(TFPSBasicDoor);
  Add(TFPSBasicItem);
end;

destructor TFPSClassList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Dispose(PFPSClassListItem(Items[I]));
  inherited;
end;

procedure TFPSClassList.Add(AClass: TFPSBasicClass);
begin
  Add(AClass, AClass.ClassName);
end;

function TFPSClassList.FindByName(AName: string): TFPSBasicClass;
var I: Integer;
begin
  for I := Count - 1 downto 0 do
    with PFPSClassListItem(Items[I])^ do
      if CompareStr(ItemName, AName) = 0 then
      begin
        Result := ItemClass;
        Exit;
      end;
  Result := nil;
end;

var
  NewSessionAdded: bool = false;

procedure AddLog(Msg: string; MsgType: TLogMsgType = lmtError);
begin
  with TStringList.Create do
  begin
    if FileExists(ExtractFilePath(Application.ExeName) + 'log.txt') then LoadFromFile(ExtractFilePath(Application.ExeName) + 'log.txt');
    if not NewSessionAdded then
    begin
      Add('-----Nowa sesja-----');
      NewSessionAdded := true;
    end;
    Add(LogMsgHeader[MsgType] + Msg);
    SaveToFile(ExtractFilePath(Application.ExeName) + 'log.txt');
    Free;
  end;
end;

procedure AddLog(Text: string; Varibles: array of const; MsgType: TLogMsgType = lmtError);
begin
  AddLog(Format(Text, Varibles), MsgType)
end;

initialization
  FPSClassList := TFPSClassList.Create;

finalization
  FPSClassList.Free;

end.
