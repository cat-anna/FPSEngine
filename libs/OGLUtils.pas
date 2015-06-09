unit OGLUtils;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Glut, OpenGL, mmsystem, JPEG;

type
  TFontSize= record
    fBoxX, fBoxY : single;
  end;
  TRGB=array[0..2]of byte;
  TPoint2D=record
    x, y:Single;
  end;
  TPoint3D=record
    x, y, z:Single;
  end;
 TImageData = array of byte;

//----------------SETUPY, ITP.-------------------
procedure SetupPixelFormat(DC: HDC);
procedure SwapRGB(data : Pointer; Size : Integer);
//---------------OŒWIETLENIE, MGLA, ITP.---------
procedure Light(obj:GLUquadricObj; lightnr:cardinal; Ambient, Diffuse, Specular, Position, MSpecular:array of GLfloat; Visible:bool);
procedure Fog;
//----------------SCIANY, ITP.--------------------
procedure RysujPokoj(width, height, longZ: GLfloat; Tex: GLuint);
procedure RysujSciane(width, height: GLfloat; Tex: GLuint);
//----------------OBIEKTY-------------------------
procedure glDrawTorus(BigRadius,SmallRadius:glFloat;nHorizontal,nVertical:word);
procedure glDrawSpring(rBegin,rEnd,rSpring,Height:glFloat;nVitkov,nHorizontal,nVertical:word);
//----------------TEKST---------------------------
procedure OutText(Litera:PChar);
procedure PreOutText(DC:HDC; FontHandle:THandle);

procedure CreateFont(FontName:string; DC:HDC);
procedure Write3DText(pText: string; pX, pY, pZ: single; pCenter: boolean; tex:GLuint);
//----------------TEKSTURY------------------------
procedure LoadTexture( filename: PChar;  var texture: GLuint);
function LoadBMP(FileName: PChar; var biFH: BITMAPINFOHEADER):TImageData;

function ProLoadTexture(Filename: String; var Texture : GLuint; LoadFromRes : Boolean): Boolean;
function LoadTGATexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
function LoadJPGTexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
function LoadBMPTexture(Filename: String; var Texture : GLuint; LoadFromResource : Boolean) : Boolean;
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;
//----------------EXTERNAL------------------------
procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;
procedure glInterleavedArrays(format: GLenum; stride: GLsizei; const pointer); stdcall; external OpenGL32;
procedure glDrawElements(mode: GLenum; count: GLsizei; atype: GLenum; const indices); stdcall; external OpenGL32;
function gluBuild2DMipmaps(Target: GLenum; Components, Width, Height: GLint; Format, atype: GLenum; Data: Pointer): GLint; stdcall; external glu32;
//------------------------------------------------

var
  FontSizes:array[0..1023]of TFontSize;
  FontList:integer;

const
   SpringWind:Byte=0;
   SpringSkelet:boolean=false;
   SpringAngle:glFloat=0;
   TorusSkelet:boolean=false;
   TorusAngle:glFloat=360;
   GLF_START_LIST=1000;

implementation
//-------------------------------------------------------------------
//-------------------------------------------------------------------
//-------------------------------------------------------------------
//-------------------------------------------------------------------
//-------------------------------------------------------------------
//-------------------------------------------------------------------
//-------------------------------------------------------------------
procedure CreateFont(FontName:string; DC:HDC);
var
  lFont : TFont;
begin
  lFont:= TFont.Create();
  lFont.Name:=FontName;
  SelectObject(DC, lFont.Handle);
  FontList:= glGenLists(256);
  wglUseFontOutlines(DC, 0, 256, FontList, 0.0, 0.2, WGL_FONT_POLYGONS, @FontSizes);
  lFont.Free();
end;
//-------------------------------------------------------------------
procedure Write3DText(pText: string; pX, pY, pZ: single; pCenter: boolean; tex:GLuint);
var
  i  : integer;
  lX : single;
begin
  if (pText = '') then Exit;
  lX:= pX;
  if pCenter then
  begin
    for i:= 1 to Length(pText) do lX:= lX - FontSizes[Ord(pText[i])].fBoxX;
  end;
  glPushMatrix();
    glTranslatef(lX, pY, pZ);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, tex);
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
      glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glListBase(FontList);
      glCallLists(Length(pText), GL_UNSIGNED_BYTE, @pText[1]);
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
  glPopMatrix();
end;
//-------------------------------------------------------------------
procedure SwapRGB(data : Pointer; Size : Integer);
asm
  mov ebx, eax
  mov ecx, size
@@loop :
  mov al,[ebx+0]
  mov ah,[ebx+2]
  mov [ebx+2],al
  mov [ebx+0],ah
  add ebx,3
  dec ecx
  jnz @@loop
end;
//-------------------------------------------------------------------
function CreateTexture(Width, Height, Format : Word; pData : Pointer) : Integer;
var
  Texture : GLuint;
begin
  glGenTextures(1, Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); { only first two can be used }
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }
  if Format = GL_RGBA then
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
  else
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
  result := Texture;
end;
//-------------------------------------------------------------------
function LoadBMPTexture(Filename: String; var Texture : GLuint; LoadFromResource : Boolean) : Boolean;
var
  FileHeader: BITMAPFILEHEADER;
  InfoHeader: BITMAPINFOHEADER;
  Palette: array of RGBQUAD;
  BitmapFile: THandle;
  BitmapLength: LongWord;
  PaletteLength: LongWord;
  ReadBytes: LongWord;
  Width, Height : Integer;
  pData : Pointer;
  ResStream : TResourceStream;
begin
  result :=FALSE;
  if LoadFromResource then // Load from resource
  begin
    try
      ResStream := TResourceStream.Create(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'BMP');
      ResStream.ReadBuffer(FileHeader, SizeOf(FileHeader));  // FileHeader
      ResStream.ReadBuffer(InfoHeader, SizeOf(InfoHeader));  // InfoHeader
      PaletteLength := InfoHeader.biClrUsed;
      SetLength(Palette, PaletteLength);
      ResStream.ReadBuffer(Palette, PaletteLength);
      Width := InfoHeader.biWidth;
      Height := InfoHeader.biHeight;
      BitmapLength := InfoHeader.biSizeImage;
      if BitmapLength = 0 then
        BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;
      GetMem(pData, BitmapLength);
      ResStream.ReadBuffer(pData^, BitmapLength);            // Bitmap Data
      ResStream.Free;
    except on
      EResNotFound do
      begin
        MessageBox(0, PChar('File not found in resource - ' + Filename), PChar('BMP Texture'), MB_OK);
        Exit;
      end
      else
      begin
        MessageBox(0, PChar('Unable to read from resource - ' + Filename), PChar('BMP Unit'), MB_OK);
        Exit;
      end;
    end;
  end
  else
  begin   // Load image from file
    BitmapFile := CreateFile(PChar(Filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if (BitmapFile = INVALID_HANDLE_VALUE) then begin
      MessageBox(0, PChar('Error opening ' + Filename), PChar('BMP Unit'), MB_OK);
      Exit;
    end;
    ReadFile(BitmapFile, FileHeader, SizeOf(FileHeader), ReadBytes, nil);
    ReadFile(BitmapFile, InfoHeader, SizeOf(InfoHeader), ReadBytes, nil);
    PaletteLength := InfoHeader.biClrUsed;
    SetLength(Palette, PaletteLength);
    ReadFile(BitmapFile, Palette, PaletteLength, ReadBytes, nil);
    if (ReadBytes <> PaletteLength) then begin
      MessageBox(0, PChar('Error reading palette'), PChar('BMP Unit'), MB_OK);
      Exit;
    end;
    Width  := InfoHeader.biWidth;
    Height := InfoHeader.biHeight;
    BitmapLength := InfoHeader.biSizeImage;
    if BitmapLength = 0 then
      BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;
    GetMem(pData, BitmapLength);
    ReadFile(BitmapFile, pData^, BitmapLength, ReadBytes, nil);
    if (ReadBytes <> BitmapLength) then begin
      MessageBox(0, PChar('Error reading bitmap data'), PChar('BMP Unit'), MB_OK);
      Exit;
    end;
    CloseHandle(BitmapFile);
  end;
  SwapRGB(pData, Width*Height);
  Texture :=CreateTexture(Width, Height, GL_RGB, pData);
  FreeMem(pData);
  result :=TRUE;
end;
//-------------------------------------------------------------------
function LoadJPGTexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  BMP : TBitmap;
  JPG : TJPEGImage;
  C : LongWord;
  Line : ^LongWord;
  ResStream : TResourceStream;      // used for loading from resource
begin
  result :=FALSE;
  JPG:=TJPEGImage.Create;
  if LoadFromResource then // Load from resource
  begin
    try
      ResStream := TResourceStream.Create(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'JPEG');
      JPG.LoadFromStream(ResStream);
      ResStream.Free;
    except on
      EResNotFound do
      begin
        MessageBox(0, PChar('File not found in resource - ' + Filename), PChar('JPG Texture'), MB_OK);
        Exit;
      end
      else
      begin
        MessageBox(0, PChar('Couldn''t load JPG Resource - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
        Exit;
      end;
    end;
  end
  else
  begin
    try
      JPG.LoadFromFile(Filename);
    except
      MessageBox(0, PChar('Couldn''t load JPG - "'+ Filename +'"'), PChar('BMP Unit'), MB_OK);
      Exit;
    end;
  end;
  BMP:=TBitmap.Create;
  BMP.pixelformat:=pf32bit;
  BMP.width:=JPG.width;
  BMP.height:=JPG.height;
  BMP.canvas.draw(0,0,JPG);        // Copy the JPEG onto the Bitmap
  Width :=BMP.Width;
  Height :=BMP.Height;
  SetLength(Data, Width*Height);
  For H:=0 to Height-1 do
  Begin
    Line :=BMP.scanline[Height-H-1];   // flip JPEG
    For W:=0 to Width-1 do
    Begin
      c:=Line^ and $FFFFFF; // Need to do a color swap
      Data[W+(H*Width)] :=(((c and $FF) shl 16)+(c shr 16)+(c and $FF00)) or $FF000000;  // 4 channel.
      inc(Line);
    End;
  End;
  BMP.free;
  JPG.free;
  Texture :=CreateTexture(Width, Height, GL_RGBA, addr(Data[0]));
  result :=TRUE;
end;
//-------------------------------------------------------------------
function LoadTGATexture(Filename: String; var Texture: GLuint; LoadFromResource : Boolean): Boolean;
var
  TGAHeader : packed record   // Header type for TGA images
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
  TGAFile   : File;
  bytesRead : Integer;
  image     : Pointer;    {or PRGBTRIPLE}
  Width, Height : Integer;
  ColorDepth    : Integer;
  ImageSize     : Integer;
  I : Integer;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
  ResStream : TResourceStream;      // used for loading from resource
begin
  result :=FALSE;
  GetMem(Image, 0);
  if LoadFromResource then // Load from resource
  begin
    try
      ResStream := TResourceStream.Create(hInstance, PChar(copy(Filename, 1, Pos('.', Filename)-1)), 'TGA');
      ResStream.ReadBuffer(TGAHeader, SizeOf(TGAHeader));  // FileHeader
      result :=TRUE;
    except on
      EResNotFound do
      begin
        MessageBox(0, PChar('File not found in resource - ' + Filename), PChar('TGA Texture'), MB_OK);
        Exit;
      end
      else
      begin
        MessageBox(0, PChar('Unable to read from resource - ' + Filename), PChar('BMP Unit'), MB_OK);
        Exit;
      end;
    end;
  end
  else
  begin
    if FileExists(Filename) then
    begin
      AssignFile(TGAFile, Filename);
      Reset(TGAFile, 1);
      BlockRead(TGAFile, TGAHeader, SizeOf(TGAHeader));
      result :=TRUE;
    end
    else
    begin
      MessageBox(0, PChar('File not found  - ' + Filename), PChar('TGA Texture'), MB_OK);
      Exit;
    end;
  end;
  if Result = TRUE then
  begin
    Result :=FALSE;
    if (TGAHeader.ImageType <> 2) then  { TGA_RGB }
    begin
      Result := False;
      CloseFile(tgaFile);
      MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Compressed TGA files not supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;
    if TGAHeader.ColorMapType <> 0 then
    begin
      Result := False;
      CloseFile(TGAFile);
      MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Colormapped TGA files not supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;
    Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
    Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
    ColorDepth := TGAHeader.BPP;
    ImageSize  := Width*Height*(ColorDepth div 8);

    if ColorDepth < 24 then
    begin
      Result := False;
      CloseFile(TGAFile);
      MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 24 bit TGA files supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;
    GetMem(Image, ImageSize);
    if LoadFromResource then // Load from resource
    begin
      try
        ResStream.ReadBuffer(Image^, ImageSize);
        ResStream.Free;
      except
        MessageBox(0, PChar('Unable to read from resource - ' + Filename), PChar('BMP Unit'), MB_OK);
        Exit;
      end;
    end
    else         // Read in the image from file
    begin
      BlockRead(TGAFile, image^, ImageSize, bytesRead);
      if bytesRead <> ImageSize then
      begin
        Result := False;
        CloseFile(TGAFile);
        MessageBox(0, PChar('Couldn''t read file "'+ Filename +'".'), PChar('TGA File Error'), MB_OK);
        Exit;
      end;
    end;
  end;
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
    Texture :=CreateTexture(Width, Height, GL_RGB, Image);
  end
  else
  begin
    for I :=0 to Width * Height - 1 do
    begin
      Front := Pointer(Integer(Image) + I*4);
      Back := Pointer(Integer(Image) + I*4 + 2);
      Temp := Front^;
      Front^ := Back^;
      Back^ := Temp;
    end;
    Texture :=CreateTexture(Width, Height, GL_RGBA, Image);
  end;
  Result :=TRUE;
  FreeMem(Image);
end;
//-------------------------------------------------------------------
function ProLoadTexture(Filename: String; var Texture : GLuint; LoadFromRes : Boolean) : Boolean;
begin
  if copy(Uppercase(filename), length(filename)-3, 4) = '.BMP' then
    LoadBMPTexture(Filename, Texture, LoadFromRes);
  if copy(Uppercase(filename), length(filename)-3, 4) = '.JPG' then
    LoadJPGTexture(Filename, Texture, LoadFromRes);
  if copy(Uppercase(filename), length(filename)-3, 4) = '.TGA' then
    LoadTGATexture(Filename, Texture, LoadFromRes);
end;
//-------------------------------------------------------------------
function LoadBMP(FileName: PChar; var biFH: BITMAPINFOHEADER):TImageData;
var
    bFH: BITMAPFILEHEADER;
    fileHandle: integer;
    i: Cardinal;
    tempRGB: byte;
const BMPTYPE = 19778;
begin
    if not FileExists(Filename) then //jezeli plik nie istnieje
    begin
      Result := nil;
      exit;
    end;
    fileHandle := FileOpen(FileName, fmOpenRead);
    FileRead (fileHandle, bFH, sizeof(BITMAPFILEHEADER) );
    if bFH.bfType <> BMPTYPE then
    begin
        FileClose(fileHandle);
        Result := nil;
        exit;
    end;
    FileRead(fileHandle, biFH, sizeof(BITMAPINFOHEADER) );
    FileSeek(fileHandle, bFH.bfOffBits, 0);
    SetLength(result, biFH.biSizeImage);
    for i:=0 to biFH.biSizeImage-1 do
        FileRead(fileHandle, result[i], 1);
    i := 0;
    while (i < biFH.biSizeImage) do
    begin
        tempRGB := result[i];
        result[i] := result[i + 2];
        result[i + 2] := tempRGB;
        Inc(i, 3);
    end;
    FileClose(fileHandle);
end;
//-------------------------------------------------------------------
procedure LoadTexture( filename: PChar;  var texture: GLuint);
var
    image: BITMAPINFOHEADER;
    buffor: TImageData;
begin
    buffor := LoadBMP(filename, image);
    if (Assigned(buffor)) then
    begin
        glGenTextures( 1, texture );
        glBindTexture( GL_TEXTURE_2D, texture );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexImage2D( GL_TEXTURE_2D, 0, 4, image.biWidth, image.biHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, buffor );
    end
    else
        MessageBox( HWND(nil), PAnsiChar('Nie udalo siê za³adowaæ tekstury o nazwie: ' + filename + #10#13 +'Upewnij siê czy podana jej lokalizacja jest prawid³owa' ), 'Informacja', MB_OK);
end;
//-------------------------------------------------------------------
procedure PreOutText(DC:HDC; FontHandle:THandle);
var hOldFont : HFont;
    agmf : Array [0..255] of TGLYPHMETRICSFLOAT;
begin
  hOldFont := SelectObject(DC, FontHandle);
  wglUseFontOutlines(DC, 0, 255, GLF_START_LIST, 0.0, 0.15,
               WGL_FONT_POLYGONS, @agmf);
  DeleteObject(SelectObject(DC, hOldFont));
end;
//-------------------------------------------------------------------
procedure OutText(Litera : PChar);
begin
  glListBase(GLF_START_LIST);
  glCallLists(Length (Litera), GL_UNSIGNED_BYTE, Litera);
end;
//-------------------------------------------------------------------
procedure glDrawTorus(BigRadius,SmallRadius:glFloat;nHorizontal,nVertical:word);
var x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:glFloat;
    rTorus:glFloat;
    HorAngle,VertAngle,dHorAngle,dVertAngle:glFloat;
    Hor,Vert:word;
begin
  rTorus:=(BigRadius-SmallRadius)/2;
  dHorAngle:=TorusAngle/nHorizontal;
  HorAngle:=0;
  dVertAngle:=360/nVertical;
  for Hor:=1 to nHorizontal do
  begin
    VertAngle:=0;
    for Vert:=1 to nVertical do
    begin
       x1:=(SmallRadius+rTorus+rTorus*cos(VertAngle*pi/180))*cos(HorAngle*pi/180);
       y1:=(SmallRadius+rTorus+rTorus*cos(VertAngle*pi/180))*sin(HorAngle*pi/180);
       z1:=rTorus*sin(VertAngle*pi/180);
       x2:=(SmallRadius+rTorus+rTorus*cos((VertAngle+dVertAngle)*pi/180))*cos(HorAngle*pi/180);
       y2:=(SmallRadius+rTorus+rTorus*cos((VertAngle+dVertAngle)*pi/180))*sin(HorAngle*pi/180);
       z2:=rTorus*sin((VertAngle+dVertAngle)*pi/180);
       x3:=(SmallRadius+rTorus+rTorus*cos((VertAngle+dVertAngle)*pi/180))*cos((HorAngle+dHorAngle)*pi/180);
       y3:=(SmallRadius+rTorus+rTorus*cos((VertAngle+dVertAngle)*pi/180))*sin((HorAngle+dHorAngle)*pi/180);
       z3:=rTorus*sin((VertAngle+dVertAngle)*pi/180);
       x4:=(SmallRadius+rTorus+rTorus*cos(VertAngle*pi/180))*cos((HorAngle+dHorAngle)*pi/180);
       y4:=(SmallRadius+rTorus+rTorus*cos(VertAngle*pi/180))*sin((HorAngle+dHorAngle)*pi/180);
       z4:=rTorus*sin(VertAngle*pi/180);
       if TorusSkelet then
       begin
         glBegin(gl_lines);
           glVertex3f(x1,y1,z1);
           glVertex3f(x2,y2,z2);
           glVertex3f(x2,y2,z2);
           glVertex3f(x3,y3,z3);
           glVertex3f(x3,y3,z3);
           glVertex3f(x4,y4,z4);
           glVertex3f(x4,y4,z4);
           glVertex3f(x1,y1,z1);
         glEnd;
      end
      else
      begin
        glBegin(gl_polygon);
          glNormal3f(x1,y1,z1);
          glVertex3f(x1,y1,z1);
          glVertex3f(x2,y2,z2);
          glVertex3f(x3,y3,z3);
          glVertex3f(x4,y4,z4);
        glEnd;
      end;
      VertAngle:=VertAngle+dVertAngle;
    end;
    HorAngle:=HorAngle+dHorAngle;
  end;
end;
//-------------------------------------------------------------------
procedure glDrawSpring(rBegin,rEnd,rSpring,Height:glFloat;nVitkov,nHorizontal,nVertical:word);
var x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:glFloat;
    Radius,dRadius,dHeight,HeightNew:glFloat;
    HorAngle,VertAngle,dHorAngle,dVertAngle:glFloat;
    Hor,Vert:word;
begin
  if (nVitkov+SpringAngle/360)*2*rSpring>height then Height:=(nVitkov+SpringAngle/360)*2*rSpring;
    dHorAngle:=(nVitkov*360+SpringAngle)/nHorizontal;
    if SpringWind<>0 then dHorAngle:=-dHorAngle;
    HorAngle:=0;
    dVertAngle:=360/nVertical;
    HeightNew:=0;
    dHeight:=Height/nHorizontal;
    Radius:=rBegin;
    dRadius:=(rEnd-rBegin)/nHorizontal;
    for Hor:=1 to nHorizontal do
    begin
      VertAngle:=0;
      for Vert:=1 to nVertical do
      begin
        x1:=(Radius+rSpring+rSpring*cos(VertAngle*pi/180))*cos(HorAngle*pi/180);
        y1:=(Radius+rSpring+rSpring*cos(VertAngle*pi/180))*sin(HorAngle*pi/180);
        z1:=rSpring*sin(VertAngle*pi/180)+HeightNew;
        x2:=(Radius+rSpring+rSpring*cos((VertAngle+dVertAngle)*pi/180))*cos(HorAngle*pi/180);
        y2:=(Radius+rSpring+rSpring*cos((VertAngle+dVertAngle)*pi/180))*sin(HorAngle*pi/180);
        z2:=rSpring*sin((VertAngle+dVertAngle)*pi/180)+HeightNew;
        x3:=(Radius+dRadius+rSpring+rSpring*cos((VertAngle+dVertAngle)*pi/180))*cos((HorAngle+dHorAngle)*pi/180);
        y3:=(Radius+dRadius+rSpring+rSpring*cos((VertAngle+dVertAngle)*pi/180))*sin((HorAngle+dHorAngle)*pi/180);
        z3:=rSpring*sin((VertAngle+dVertAngle)*pi/180)+HeightNew+dHeight;
        x4:=(Radius+dRadius+rSpring+rSpring*cos(VertAngle*pi/180))*cos((HorAngle+dHorAngle)*pi/180);
        y4:=(Radius+dRadius+rSpring+rSpring*cos(VertAngle*pi/180))*sin((HorAngle+dHorAngle)*pi/180);
        z4:=rSpring*sin(VertAngle*pi/180)+HeightNew+dHeight;
        if SpringSkelet then
        begin
          glBegin(gl_lines);
            glVertex3f(x1,y1,z1);
            glVertex3f(x2,y2,z2);
            glVertex3f(x2,y2,z2);
            glVertex3f(x3,y3,z3);
            glVertex3f(x3,y3,z3);
            glVertex3f(x4,y4,z4);
            glVertex3f(x4,y4,z4);
            glVertex3f(x1,y1,z1);
          glEnd;
        end
        else
        begin
          glBegin(gl_polygon);
            glNormal3f(x1,y1,z1);
            glVertex3f(x1,y1,z1);
            glVertex3f(x2,y2,z2);
            glVertex3f(x3,y3,z3);
            glVertex3f(x4,y4,z4);
          glEnd;
        end;
        VertAngle:=VertAngle+dVertAngle;
      end;
      HorAngle:=HorAngle+dHorAngle;
      HeightNew:=HeightNew+dHeight;
      Radius:=Radius+dRadius;
    end;
end;
//-------------------------------------------------------------------
procedure RysujSciane(width, height: GLfloat; Tex: GLuint);
var
    i, j: integer;
    dx, dy, krotnoscX, krotnoscY: GLfloat;
const nx = 10; ny = 10;
begin
    dx := width / nx;
    dy := height / ny;

    krotnoscX := 1.0 / nx;
    krotnoscY := 1.0 / ny;

    glColor3f(0.2, 0.2, 0.2);
    glBindTexture(GL_TEXTURE_2D, tex);
    glBegin(GL_QUADS);
    for j:=0 to ny-1 do
        for i:=0 to nx-1 do
        begin
          glTexCoord2f( krotnoscX*i, krotnoscY*j );
          glVertex3f( -width/2.0 + i*dx, -height/2.0 +dy*j, 0.0 );

          glTexCoord2f( krotnoscX*(i + 1), krotnoscY*j );
          glVertex3f( -width/2.0 + (i+1)*dx, -height/2.0 +dy*j, 0.0 );

          glTexCoord2f( krotnoscX*(i + 1), krotnoscY*(j + 1) );
          glVertex3f( -width/2.0 + (i+1)*dx, -height/2.0 +dy*(j+1), 0.0 );

          glTexCoord2f( krotnoscX*i, krotnoscY*(j + 1) );
          glVertex3f( -width/2.0 + i*dx, -height/2.0 +dy*(j+1), 0.0 );
        end;
    glEnd();
end;
//-------------------------------------------------------------------
procedure RysujPokoj(width, height, longZ: GLfloat; Tex: GLuint);
begin
    //tylna œciana
    glPushMatrix();
        glNormal3f(0.0, 0.0, 1.0);
        glTranslatef(0.0, 0.0, -longZ/2.0);
        RysujSciane(width, height, tex);
    glPopMatrix();

    //lewa
    glPushMatrix();
        glNormal3f(-1.0, 0.0, 0.0);
        glTranslatef(-width/2.0, 0.0, 0.0);
        glRotatef(90.0, 0.0, 1.0, 0.0);
        RysujSciane(width, height, tex);
    glPopMatrix();

    //prawa
    glPushMatrix();
        glNormal3f(1.0, 0.0, 0.0);
        glTranslatef(width/2.0, 0.0, 0.0);
        glRotatef(-90.0, 0.0, 1.0, 0.0);
        RysujSciane(width, height, tex);
    glPopMatrix();

    //dol
    glPushMatrix();
        glNormal3f(0.0, -1.0, 0.0);
        glTranslatef(0.0, -height/2.0, 0.0);
        glRotatef(90.0, 1.0, 0.0, 0.0);
        RysujSciane(width, width, tex);
    glPopMatrix();

    //gora
    glPushMatrix();
        glNormal3f(0.0, 1.0, 0.0);
        glTranslatef(0.0, height/2.0, 0.0);
        glRotatef(90.0, 1.0, 0.0, 0.0);
        RysujSciane(width, width, tex);
    glPopMatrix();
end;
//-------------------------------------------------------------------
procedure Fog;
var r, g, b: GLfloat;
begin
  glFogi(GL_FOG_MODE, GL_LINEAR);
  r := 0.5;
  g := 0.5;
  b := 0.5;
  glFogfv(GL_FOG_COLOR, @b);
  glFogfv(GL_FOG_COLOR, @g);
  glFogfv(GL_FOG_COLOR, @r);
  glFogf(GL_FOG_DENSITY, 0.0);
  glFogf(GL_FOG_START, 0);
  glFogf(GL_FOG_end, 10);
end;
//-------------------------------------------------------------------
procedure Light(obj:GLUquadricObj; lightnr:cardinal; Ambient, Diffuse, Specular, Position, MSpecular:array of GLfloat; Visible:bool);
begin
  glLightfv(lightnr, GL_AMBIENT, @Ambient);
  glLightfv(lightnr, GL_DIFFUSE, @Diffuse);
  glLightfv(lightnr, GL_SPECULAR, @Specular);
  glLightfv(lightnr, GL_POSITION, @Position);

  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @MSpecular);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, 50.0);
  glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, 1.0);

  if visible then
  begin
    gluQuadricTexture(obj, GL_FALSE);
    glPushMatrix();
        glColor3f(specular[0], specular[1], specular[2]);
        glTranslatef(Position[0], Position[1], Position[2]);
        gluSphere(obj, 5.0, 20, 20);
    glPopMatrix();
    glColor3f(0.5, 0.5, 0.5);
  end;
end;
//-------------------------------------------------------------------
procedure SetupPixelFormat(DC: HDC);
const
   pfd: TPIXELFORMATDESCRIPTOR = (
        nSize: sizeof(TPIXELFORMATDESCRIPTOR);
        nVersion: 1;
        dwFlags: PFD_SUPPORT_OPENGL or
                 PFD_DRAW_TO_WINDOW or
                 PFD_DOUBLEBUFFER;
        iPixelType: PFD_TYPE_RGBA;
        cColorBits: 24;
        cRedBits: 0; cRedShift: 0;
        cGreenBits: 0;  cGreenShift: 0;
        cBlueBits: 0; cBlueShift: 0;
        cAlphaBits: 0;  cAlphaShift: 0;
        cAccumBits: 0;
        cAccumRedBits: 0;
        cAccumGreenBits: 0;
        cAccumBlueBits: 0;
        cAccumAlphaBits: 0;
        cDepthBits: 32;
        cStencilBits: 0;
        cAuxBuffers: 0;
        iLayerType: PFD_MAIN_PLANE;
        bReserved: 0;
        dwLayerMask: 0;
        dwVisibleMask: 0;
        dwDamageMask: 0;
   );
var pixelFormatIndex:integer;
begin
    pixelFormatIndex := ChoosePixelFormat(DC, @pfd);
    SetPixelFormat(DC, pixelFormatIndex, @pfd);
end;
//-------------------------------------------------------------------
end.
