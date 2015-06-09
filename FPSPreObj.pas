unit FPSPreObj;

interface

uses Windows, Classes, FPSEngine, Dialogs, SysUtils, FPSUtils;

type
  TFPSPlant1 = class(TFPSBasicObject)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

  TFPSPlant2 = class(TFPSBasicObject)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

  TFPSMonster1 = class(TFPSBasicMonster)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

  TFPSShotGun1 = class(TFPSBasicWeapon)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

  TFPSShotGunBullet1 = class(TFPSBasicBullet)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

  TFPSDeadMonster1 = class(TFPSBasicObject)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

  TFPSBarrel1 = class(TFPSBasicObject)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

  TFPSBarrelExplosion1 = class(TFPSBasicObject)
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;

{  TFPS1 = class(TFPSBasic)
    procedure RunScript(Event: TFPSBasicEvent); override;
    constructor Create(aEngine: TFPSEngine; aOwner: TFPSMap); override;
    procedure SaveToStream(Stream: TStream; SaveAll: bool = false); override;
  end;}

procedure RegisterObjects(Data: TFPSData);
procedure RegisterClasses(ClassList: TFPSClassList);

implementation

procedure RegisterClasses(ClassList: TFPSClassList);
begin
  FPSClassList.Add(TFPSPlant1);
  FPSClassList.Add(TFPSPlant2);
  FPSClassList.Add(TFPSMonster1);
  FPSClassList.Add(TFPSShotGun1);
  FPSClassList.Add(TFPSShotGunBullet1);
  FPSClassList.Add(TFPSDeadMonster1);
  FPSClassList.Add(TFPSBarrel1);
  FPSClassList.Add(TFPSBarrelExplosion1);
//  FPSClassList.Add();
//  FPSClassList.Add();
//  FPSClassList.Add();
end;

procedure RegisterObjects(Data: TFPSData);
begin
  Data.StaticObjects.Add(TFPSPlant1.Create(nil, nil));
  Data.StaticObjects.Add(TFPSPlant2.Create(nil, nil));
  Data.StaticObjects.Add(TFPSMonster1.Create(nil, nil));
  Data.StaticObjects.Add(TFPSShotGun1.Create(nil, nil));
  Data.StaticObjects.Add(TFPSShotGunBullet1.Create(nil, nil));
  Data.StaticObjects.Add(TFPSDeadMonster1.Create(nil, nil));
  Data.StaticObjects.Add(TFPSBarrel1.Create(nil, nil));
  Data.StaticObjects.Add(TFPSBarrelExplosion1.Create(nil, nil));
end;


{  TFPS  }
{
procedure .RunScript(Event: TFPSBasicEvent);
begin

end;

constructor .Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
end;

procedure .SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if not SaveAll then inherited;
end;
}

{  TFPS  }
       {
procedure TFPSBarrelExplosion1.RunScript(Event: TFPSBasicEvent);
begin
  if Event = beAnimationLoop then Dead;
end;             }

constructor TFPSBarrelExplosion1.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Name := 'BarrelExp';
  {Collision := false;
  Destroyable := false;
  Height := 0.6;
  Height := 0.6;
  TextureIndex := 31;
  TextureCount := 6;
  Animated := true;
  AnimationSpeed := 0.4;
  AnimationPos := TextureIndex;    }
end;

procedure TFPSBarrelExplosion1.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if SaveAll then exit;
  inherited;
end;

{  TFPSBarrel1  }
        {
procedure TFPSBarrel1.RunScript(Event: TFPSBasicEvent);
begin
  if Event = beDead then
    OwnerMap.CreateObject('BarrelExp', Position);
end;    }

constructor TFPSBarrel1.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Collision := true;
  Destroyable := true;
  Width := 0.6;
  Height := 0.6;
 { TextureIndex := 37;
  TextureCount := 2;
  Name := 'Barrel1';
  Position.Y := 0;
  AnimationSpeed := 0.05;   }
end;

procedure TFPSBarrel1.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if SaveAll then exit;
  inherited;
end;

{  TFPSDeadMonster1  }
          {
procedure TFPSDeadMonster1.RunScript(Event: TFPSBasicEvent);
begin
{  if Event = beAnimationLoop then
  begin
    Animated := false;
    AnimationPos := TextureCount + TextureIndex - 1;
  end; }  {
end;        }

constructor TFPSDeadMonster1.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Destroyable := false;
  Collision := false;
  Position.Y := 0;
{  TextureIndex := 23;
  TextureCount := 8;
  Animated := true;
  AnimationSpeed := 0.4;
  Width := 0.6;
  Height := 0.6;
  Name := 'Monster1Dead';
  AnimationPos := TextureIndex;    }
end;

procedure TFPSDeadMonster1.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if SaveAll then exit;
  inherited;
end;

{  TFPS  }


constructor TFPSShotGunBullet1.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Position := Make3DPoint(0, 0, 0);
  Speed := Make3DPoint(0, 0, 0);
  SpeedMultiplier := 5;
  BasicSpeed := 0.3;
  LifeTime := 1500;
  CreateTime := GetTickCount;
  Name := 'Bullet1';
end;

procedure TFPSShotGunBullet1.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if SaveAll then exit;
  inherited;
end;

{  TFPSShotGun1  }

constructor TFPSShotGun1.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  with DrawPosition do
  begin
    V1 := Make3DPoint(-0.8, -1, 0);
    V2 := Make3DPoint(0.2, -1, 0);
    V3 := Make3DPoint(0.2, 0, 0);
    V4 := Make3DPoint(-0.8, 0, 0);
  end;
  BulletName := 'Bullet1';
  Name := 'ShotGun';
  ShotSound := 'shotgun';
  TextureName := 'ShotGun1';
  FCanShot := true;
  FVisible := true;
end;

procedure TFPSShotGun1.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if SaveAll then exit;
  inherited;
end;

{  TFPSMonster1  }
         {
procedure TFPSMonster1.RunScript(Event: TFPSBasicEvent);
begin
  if Event = beDead then
  begin
    OwnerMap.CreateObject('Monster1Dead', Position);
  end;
end;     }

constructor TFPSMonster1.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Speed := 1/20;
  AtackSpeed := 2000;
  Attack := 5;
  {TextureIndex := 19;
  TextureCount := 4;
  AnimationSpeed := 0.2;
  Visible := true;
  Animated := true;
  Destroyable := true;
  Collision := true;
  Width := 0.6;
  Height := 0.6;
  Position.Y := 0.1;
  Name := 'Monster1';
  AnimationPos := TextureIndex;   }
  SeeRange := 10;
end;

procedure TFPSMonster1.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if not SaveAll then inherited;
end;

{  TFPSPlant2  }

constructor TFPSPlant2.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Name := 'Plant2';
  Collision := true;
  Destroyable := true;
{  TextureIndex := 7;
  Animated := false;
  Height := 0.4;
  Width := 0.4;
  Position.Y := 0;
  AnimationPos := TextureIndex;}
end;

procedure TFPSPlant2.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if SaveAll then exit;
  inherited;
end;

{  TFPSPlant1  }

constructor TFPSPlant1.Create(aEngine: TFPSEngine; aOwner: TFPSMap);
begin
  inherited;
  Name := 'Plant1';
  Collision := true;
  Destroyable := false;
{  TextureIndex := 6;
  Animated := false;
  Height := 0.6;
  Width := 0.6;
  Position.Y := 0;
  AnimationPos := TextureIndex; }
end;

procedure TFPSPlant1.SaveToStream(Stream: TStream; SaveAll: bool = false);
begin
  if SaveAll then exit;
  inherited;
end;

end.
