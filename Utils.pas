unit Utils;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, strutils, FPSEngine,
  math, OpenGL;

function MakeDrawPosition(cX, cY, cZ: Single): TDrawPosition;
function AddToDrawPosition(DP: TDrawPosition; cX, cY, cZ: Single): TDrawPosition;
function Make3DPoint(cX, cY, cZ: Single): T3DPoint;
function AddTo3DPoint(P: T3DPoint; cX, cY, cZ: Single): T3DPoint;
function Sum3DPoints(Point1, Point2: T3DPoint): T3DPoint;
function Distance2d(Point1, Point2: T3DPoint): Single;
function GetVector(Point1, Point2: T3DPoint): T3DPoint;

function IsCollision(W: TDrawPosition; P: T3Dpoint; CR: Single = CollisionRadius): Bool;
function Is2DCollision(W: TDrawPosition; P: T3Dpoint; CR: Single = CollisionRadius): Bool;
function IsAreaCollision(Area: TArea; P: T3Dpoint; CR: Single = CollisionRadius): Bool;
function IsRectCollision(Rect: TRect; P: T3DPoint): bool;

function LoadTGA(Stream: TStream; DestBitmap: TBitmap): Boolean;

function GetFileNameNoExt(name: string): string;
function ReversePos(substr, s: string; caseSensitive: boolean = false): integer;

procedure SaveStringToStream(Strings: array of string; Stream: TStream);
function LoadStringFromStream(Stream: TStream): string;
procedure SaveBoolToStream(Values: array of bool; Stream: TStream);
function LoadBoolFromStream(Stream: TStream): bool;

function CompareExtensions(FExt: string; Exts: array of string): bool;

implementation

uses GLUtils;

function CompareExtensions(FExt: string; Exts: array of string): bool;
var i: Integer;
begin
  Result := false;
  if FExt[1] = '.' then delete(FExt, 1, 1);
  for i := 0 to Length(Exts) - 1 do
    if CompareStr(LowerCase(FExt), LowerCase(Exts[i])) = 0 then
    begin
      Result := true;
      Exit;
    end;
end;

procedure SaveBoolToStream(Values: array of bool; Stream: TStream);
var i, j: byte;
  B: bool;
begin
  for j := 0 to Length(Values) - 1 do
  begin
    B := Values[j];
    i := IfThen(B, $31, $30);
    Stream.Write(i, SizeOf(i));
  end;
end;

function LoadBoolFromStream(Stream: TStream): bool;
var i: byte;
begin
  Stream.Read(i, SizeOf(i));
  Result := i = $31;
end;

procedure SaveStringToStream(Strings: array of string; Stream: TStream);
var i, j: byte;
  S: string;
begin
  for j := 0 to Length(Strings) - 1 do
  begin
    S := Strings[j];
    i := Length(S);
    Stream.Write(i, SizeOf(i));
    Stream.Write(PChar(S)^, i);
  end;
end;

function LoadStringFromStream(Stream: TStream): string;
var i: byte;
begin
  Stream.Read(i, SizeOf(i));
  SetLength(Result, i);
  Stream.Read(PChar(Result)^, i);
end;

function ReversePos(substr, s: string; caseSensitive: boolean = false): integer;
var i, j: integer;
begin
  result := 0;
  if caseSensitive then
  begin
    substr := lowerCase(substr);
    s := lowerCase(s);
  end;

  j := length(substr);
  for i := length(s) downto 1 do
  begin
    if s[i] = substr[j] then
    begin
      j := j - 1;
      if j = 0 then
      begin
        result := i;
        exit;
      end else continue;
    end else j := length(substr);
  end;
end;

function GetFileNameNoExt(name: string): string;
var pos: integer;
begin
  if name = '..' then
  begin
    result := name;
    exit;
  end;
  pos := reversePos('.', name, false);
  if pos <> 0 then result := copy(name, 1, pos - 1)
  else result := name;
end;

function GetVector(Point1, Point2: T3DPoint): T3DPoint;
begin
  with Result do
  begin
    X := Point1.X - Point2.X;
    Y := Point1.Y - Point2.Y;
    Z := Point1.Z - Point2.Z;
  end;
end;

function Distance2d(Point1, Point2: T3DPoint): GLfloat;
begin
  Result := Sqrt(Sqr(Point1.X - Point2.X) + Sqr(Point1.Z - Point2.Z));
end;

function Sum3DPoints(Point1, Point2: T3DPoint): T3DPoint;
begin
  with Result do
  begin
    X := Point2.X + Point1.X;
    Y := Point2.Y + Point1.Y;
    Z := Point2.Z + Point1.Z;
  end;
end;

function Make3DPoint(cX, cY, cZ: GLfloat): T3Dpoint;
begin
  with Result do
  begin
    X := cX;
    Y := cy;
    Z := cZ;
  end;
end;

function AddTo3DPoint(P: T3DPoint; cX, cY, cZ: Single): T3DPoint;
begin
  with Result do
  begin
    X := P.X + cX;
    Y := P.Y + cy;
    Z := P.Z + cZ;
  end;
end;

function MakeDrawPosition(cX, cY, cZ: Single): TDrawPosition;
begin
  with Result do
  begin
    V1 := Make3DPoint(cX, cY, cZ);
    V2 := V1;
    V3 := V1;
    V4 := V1;
  end;
end;

function AddToDrawPosition(DP: TDrawPosition; cX, cY, cZ: Single): TDrawPosition;
begin
  with Result do
  begin
    V1 := AddTo3DPoint(DP.V1, cX, cY, cZ);
    V2 := AddTo3DPoint(DP.V2, cX, cY, cZ);
    V3 := AddTo3DPoint(DP.V3, cX, cY, cZ);
    V4 := AddTo3DPoint(DP.V4, cX, cY, cZ);
  end;
end;

function Is2DCollision(W: TDrawPosition; P: T3Dpoint; CR: Single = CollisionRadius): Bool;
begin
  with W do
  Result := (((V1.X + CR > P.X)and(V3.X - CR < P.X))or
             ((V3.X + CR > P.X)and(V1.X - CR < P.X)))and
            (((V1.Z + CR > P.Z)and(V3.Z - CR < P.Z))or
             ((V3.Z + CR > P.Z)and(V1.Z - CR < P.Z)));
end;

function IsCollision(W: TDrawPosition; P: T3Dpoint; CR: Single = CollisionRadius): Bool;
begin
  with W do
  Result := (((V1.X + CR > P.X)and(V3.X - CR < P.X))or
             ((V3.X + CR > P.X)and(V1.X - CR < P.X)))and
            (((V1.Z + CR > P.Z)and(V3.Z - CR < P.Z))or
             ((V3.Z + CR > P.Z)and(V1.Z - CR < P.Z)))and  
            (((V1.Y + CR > P.Y)and(V3.Y - CR < P.Y))or
             ((V3.Y + CR > P.Y)and(V1.Y - CR < P.Y)));
end;

function IsAreaCollision(Area: TArea; P: T3Dpoint; CR: Single = CollisionRadius): Bool;
begin
  with Area do
   Result := (((LeftTop.X + CR > P.X)and(LeftTop.X + Size.X - CR < P.X))or
    ((LeftTop.X + Size.X + CR > P.X)and(LeftTop.X - CR < P.X)))and
    (((LeftTop.Z + CR > P.Z)and(LeftTop.Z + Size.Z - CR < P.Z))or
    ((LeftTop.Z + Size.Z + CR > P.Z)and(LeftTop.Z - CR < P.Z)));
end;

function IsRectCollision(Rect: TRect; P: T3DPoint): bool;
begin
  Result := (P.X >= Rect.Left)and(P.X <= Rect.Right)and
            (P.Z >= Rect.Top)and(P.Z <= Rect.Bottom);
end;

type
  TTGAHeader = packed record   // Header type for TGA images
    FileType     : Byte;
    ColorMapType : Byte;
    ImageType    : Byte;
    ColorMapSpec : Array[0..4] of Byte;
    OrigX  : Array [0..1] of Byte;
    OrigY  : Array [0..1] of Byte;
    Width  : Array [0..1] of Byte;
    Height : Array [0..1] of Byte;
    BPP    : Byte;
    ImageInfo : Byte;
  end;

function LoadTGA(Stream: TStream; DestBitmap: TBitmap): Boolean;
var
  TGAHeader : TTGAHeader;
  image     : Pointer;    {or PRGBTRIPLE}
  CompImage : Pointer;
  Width, Height : Integer;
  ColorDepth    : Integer;
  ImageSize     : Integer;
  BufferIndex : Integer;
  currentByte : Integer;
  CurrentPixel : Integer;
  I, j: Integer;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
  ByteArr: Pointer;
begin
  Stream.Position := 0;
  Stream.Read(TGAHeader, SizeOf(TGAHeader));
// Only support 24, 32 bit images
  if (TGAHeader.ImageType <> 2) AND    { TGA_RGB }
     (TGAHeader.ImageType <> 10) then  { Compressed RGB }
  begin
    MessageBox(0, PChar('Loading Error. Only 24 and 32bit TGA supported.'), PChar('TGA File Error'), MB_OK);
    Result := false;
    Exit;
  end;

  if TGAHeader.ColorMapType <> 0 then  // Don't support colormapped files
  begin
    Result := False;
    MessageBox(0, PChar('Loading Error. Colormapped TGA files not supported.'), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
  Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
  ColorDepth := TGAHeader.BPP;
  ImageSize  := Width * Height * (ColorDepth div 8);

  if ColorDepth < 24 then
  begin
    Result := False;
    MessageBox(0, PChar('Loading Error. Only 24 and 32 bit TGA files supported.'), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  GetMem(Image, ImageSize);

  if TGAHeader.ImageType = 2 then   // Standard 24, 32 bit TGA file
  begin
    Stream.Read(image^, ImageSize);
    // TGAs are stored BGR and not RGB, so swap the R and B bytes.
    // 32 bit TGA files have alpha channel and gets loaded differently
    if TGAHeader.BPP = 24 then
    begin
      for I :=0 to Width * Height - 1 do
      begin
        Front := Pointer(Integer(Image) + I*3);
        Back := Pointer(Integer(Image) + I*3 + 2);
        Temp := Front^;
        Front^ := Back^;
        Back^ := Temp;
      end;
    end else
    for I :=0 to Width * Height - 1 do
    begin
      Front := Pointer(Integer(Image) + I*4);
      Back := Pointer(Integer(Image) + I*4 + 2);
      Temp := Front^;
      Front^ := Back^;
      Back^ := Temp;
    end;
  end;

  // Compressed 24, 32 bit TGA files
  if TGAHeader.ImageType = 10 then
  begin
    ColorDepth := ColorDepth div 8;
    CurrentByte := 0;
    CurrentPixel := 0;
    BufferIndex := 0;

    GetMem(CompImage, Stream.Size - sizeOf(TGAHeader));

    Stream.Read(CompImage^, Stream.Size - sizeOf(TGAHeader));
// Extract pixel information from compressed data
    repeat
      Front := Pointer(Integer(CompImage) + BufferIndex);
      Inc(BufferIndex);
      if Front^ < 128 then
      begin
        For I := 0 to Front^ do
        begin
          CopySwapPixel(Pointer(Integer(CompImage)+BufferIndex+I*ColorDepth), Pointer(Integer(image)+CurrentByte));
          CurrentByte := CurrentByte + ColorDepth;
          inc(CurrentPixel);
        end;
        BufferIndex :=BufferIndex + (Front^+1)*ColorDepth
      end
      else
      begin
        For I := 0 to Front^ -128 do
        begin
          CopySwapPixel(Pointer(Integer(CompImage)+BufferIndex), Pointer(Integer(image)+CurrentByte));
          CurrentByte := CurrentByte + ColorDepth;
          inc(CurrentPixel);
        end;
        BufferIndex := BufferIndex + ColorDepth
      end;
    until CurrentPixel >= Width*Height;
    FreeMem(CompImage);
  end;

  DestBitmap.SetSize(Width, Height);

  if TGAHeader.BPP = 24 then
  begin
    DestBitmap.PixelFormat := pf24bit
  end else
  begin
    currentByte := Height * Width * 4;
    DestBitmap.PixelFormat := pf32bit;
    for i := 0 to Height - 1 do
    begin
      ByteArr := DestBitmap.ScanLine[i];
      for j := Width - 1 downto 0 do
      begin
        Front := Pointer(Integer(Image) + currentByte);
        Back  := Pointer(Integer(ByteArr) + j * 4+ 2);
        Back^ := Front^;

        Front := Pointer(Integer(Image) + currentByte + 1);
        Back  := Pointer(Integer(ByteArr) + j * 4 + 1);
        Back^ := Front^;

        Front := Pointer(Integer(Image) + currentByte + 2);
        Back  := Pointer(Integer(ByteArr) + j * 4 );
        Back^ := Front^;

        Front := Pointer(Integer(Image) + currentByte + 3);
        Back  := Pointer(Integer(ByteArr) + j * 4 + 3);
        Back^ := Front^;
        dec(currentByte, 4)
      end;
   end;
  end;
  Result := TRUE;
  FreeMem(Image);
end;

end.
