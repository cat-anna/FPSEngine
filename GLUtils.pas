unit GLUtils;

interface

uses
  Windows, OpenGL, Graphics, Classes, SysUtils, jpeg;

procedure glDeleteTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
function gluBuild2DMipmaps(Target: GLenum; Components, Width, Height: GLint; Format, atype: GLenum; Data: Pointer): GLint; stdcall; external glu32;
procedure glGenTextures(n: GLsizei; var textures: GLuint); stdcall; external opengl32;
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall; external opengl32;

function LoadTexture(Stream: TMemoryStream; Filename: String; var Texture: GLuint): Boolean;
function LoadTGATexture(Stream: TMemoryStream; Filename: String; var Texture: GLuint): Boolean;
function LoadJPGTexture(Stream: TMemoryStream; Filename: String; var Texture: GLuint): Boolean;
function LoadBMPTexture(Stream: TMemoryStream; Filename: String; var Texture : GLuint) : Boolean;

procedure CopySwapPixel(const Source, Destination : Pointer);

function CreateFont(Name: string; FontDeep: single; DC: HDC): GLuint;
procedure glPrint(Text: string; PX, PY, PZ: GLfloat; FontBase: GLuint);

procedure SetupPixelFormat(DC:HDC);

implementation

function CreateFont(Name: string; FontDeep: single; DC: HDC): GLuint;
var Font: HFONT;
    gmf: array[0..255] of GLYPHMETRICSFLOAT;
begin
    Result := glGenLists(256);
    Font := Windows.CreateFont(-24,                                  // Výška
                       0,                                  // Šíøka
                       0,                                  // Úhel escapement
                       0,                                  // Úhel orientace
                       FW_BOLD,                            // Tuènost
                       0,                                  // Kurzíva
                       0,                                  // Podtržení
                       0,                                  // Pøeškrtnutí
                       ANSI_CHARSET,                       // Znaková sada
                       OUT_TT_PRECIS,                      // Pøesnost výstupu (TrueType)
                       CLIP_DEFAULT_PRECIS,                // Pøesnost oøezání
                       ANTIALIASED_QUALITY,                // Výstupní kvalita
                       FF_DONTCARE or DEFAULT_PITCH,       // Rodina a pitch
                       Pchar(Name));              // Jméno fontu
    SelectObject(dc, Font);
    wglUseFontOutlines(dc,                                // Vybere DC
                       0,                                  // Poèáteèní znak
                       255,                                // Koncový znak
                       Result,                               // Adresa prvního znaku
                       0,                                  // Hranatost
                       FontDeep,                              // Hloubka v ose z
                       WGL_FONT_POLYGONS,                  // Polygony ne drátìný model
                       @gmf);                              // Adresa bufferu pro uložení informací

end;

procedure glPrint(Text: string; PX, PY, PZ: GLfloat; FontBase: GLuint);
begin
  if text = '' then exit;

  glPushMatrix;
  glTranslate(PX, PY, PZ);
  glPushAttrib(GL_LIST_BIT);
  glListBase(FontBase);                                       // Nastaví první display list na base
  glCallLists(length(text),GL_UNSIGNED_BYTE,Pchar(text)); // Vykreslí display listy
  glPopAttrib;
  glPopMatrix;
end;

procedure SetupPixelFormat(DC: HDC);
const
   pfd:TPIXELFORMATDESCRIPTOR = (
        nSize:sizeof(TPIXELFORMATDESCRIPTOR);	// size
        nVersion:1;			// version
        dwFlags:PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW or
                PFD_DOUBLEBUFFER;	// support double-buffering
        iPixelType:PFD_TYPE_RGBA;	// color type
        cColorBits:32;			// preferred color depth
        cRedBits:0; cRedShift:0;	// color bits (ignored)
        cGreenBits:0;  cGreenShift:0;
        cBlueBits:0; cBlueShift:0;
        cAlphaBits:0;  cAlphaShift:0;   // no alpha buffer
        cAccumBits: 0;
        cAccumRedBits: 0;  		// no accumulation buffer,
        cAccumGreenBits: 0;     	// accum bits (ignored)
        cAccumBlueBits: 0;
        cAccumAlphaBits: 0;
        cDepthBits:16;			// depth buffer
        cStencilBits:0;			// no stencil buffer
        cAuxBuffers:0;			// no auxiliary buffers
        iLayerType:PFD_MAIN_PLANE;  	// main layer
   bReserved: 0;
   dwLayerMask: 0;
   dwVisibleMask: 0;
   dwDamageMask: 0;                    // no layer, visible, damage masks
   );
var pixelFormat:integer;
begin
  pixelFormat := ChoosePixelFormat(DC, @pfd);
  SetPixelFormat(DC, pixelFormat, @pfd);
end;

{  Swap bitmap format from BGR to RGB }
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

procedure CopySwapPixel(const Source, Destination : Pointer);
asm
  push ebx
  mov bl,[eax+0]
  mov bh,[eax+1]
  mov [edx+2],bl
  mov [edx+1],bh
  mov bl,[eax+2]
  mov bh,[eax+3]
  mov [edx+0],bl
  mov [edx+3],bh
  pop ebx
end;

function CreateTexture(Width, Height, Format : Word; pData : Pointer) : GLuint;
begin
  glGenTextures(1, result);
  glBindTexture(GL_TEXTURE_2D, result);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}
//  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, G_DECAL);  {Texture does NOT blend with object background}

  { Select a filtering type. BiLinear filtering produces very good results with little performance impact
    GL_NEAREST               - Basic texture (grainy looking texture)
    GL_LINEAR                - BiLinear filtering
    GL_LINEAR_MIPMAP_NEAREST - Basic mipmapped texture
    GL_LINEAR_MIPMAP_LINEAR  - BiLinear Mipmapped texture
  }  

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); { only first two can be used }
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }

  if Format = GL_RGBA then
    gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, pData)
  else
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, Width, Height, GL_RGB, GL_UNSIGNED_BYTE, pData);
//  glTexImage2D(GL_TEXTURE_2D, 0, 3, Width, Height, 0, GL_RGB, GL_UNSIGNED_BYTE, pData);  // Use when not wanting mipmaps to be built by openGL
end;

function LoadBMPTexture(Stream: TMemoryStream; Filename: String; var Texture : GLuint) : Boolean;
var
  FileHeader: BITMAPFILEHEADER;
  InfoHeader: BITMAPINFOHEADER;
  Palette: array of Windows.RGBQUAD;
  BitmapLength: LongWord;
  PaletteLength: LongWord;
  Width, Height : Integer;
  pData : Pointer;

  MStr : TMemoryStream;
begin
  MStr := TMemoryStream.Create;
  Result := false;

  try
    if Stream = nil then
    begin
      if FileExists(Filename) then MStr.LoadFromFile(Filename)
      else
      begin
        MessageBox(0, PChar('File not found  - ' + Filename), PChar('BMP Texture'), MB_OK);
        MStr.Free;
        Exit;
      end;
    end else MStr.LoadFromStream(Stream);
  except
    MessageBox(0, PChar('Couldn''t load BMP  - ' + Filename), PChar('BMP Texture'), MB_OK);
  end;

  try
    MStr.ReadBuffer(FileHeader, SizeOf(FileHeader));  // FileHeader
    MStr.ReadBuffer(InfoHeader, SizeOf(InfoHeader));  // InfoHeader
    PaletteLength := InfoHeader.biClrUsed;
    SetLength(Palette, PaletteLength);
    MStr.ReadBuffer(Palette, PaletteLength);          // Palette

    Width := InfoHeader.biWidth;
    Height := InfoHeader.biHeight;

    BitmapLength := InfoHeader.biSizeImage;
    if BitmapLength = 0 then
      BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;

    GetMem(pData, BitmapLength);
    MStr.ReadBuffer(pData^, BitmapLength);            // Bitmap Data
    MStr.Free;
  except
      MessageBox(0, PChar('Unable to read from resource - ' + Filename), PChar('BMP Unit'), MB_OK);
      MStr.Free;
      Exit;
  end;

  // Bitmaps are stored BGR adn not RGB, so swap the R and B bytes.
  SwapRGB(pData, Width * Height);

  Texture := CreateTexture(Width, Height, GL_RGB, pData);
  FreeMem(pData);
  result := TRUE;
end;

function LoadJPGTexture(Stream: TMemoryStream; Filename: String; var Texture: GLuint): Boolean;
var
  Data : Array of LongWord;
  W, Width : Integer;
  H, Height : Integer;
  C : LongWord;
  Line : ^LongWord;
  jpg: TJPEGImage;
  Bmp: TBitmap;
begin
  result := FALSE;
  jpg := TJPEGImage.Create;
  try
    if Stream = nil then
    begin
      if FileExists(Filename) then jpg.LoadFromFile(Filename)
      else
      begin
        MessageBox(0, PChar('File not found  - ' + Filename), PChar('JPG Texture'), MB_OK);
        Exit;
      end;
    end else jpg.LoadFromStream(Stream);
  except
    MessageBox(0, PChar('Couldn''t load JPG - "'+ Filename +'"'), PChar('JPG Unit'), MB_OK);
    Exit;
  end;

  // Create Bitmap
  Bmp := TBitmap.Create;
  Width := jpg.Width;
  Height := jpg.Height;
  SetLength(Data, Width * Height);
  Bmp.SetSize(Width, Height);
  Bmp.Canvas.Draw(0, 0, jpg);
  jpg.Free;

  For H := 0 to Height - 1 do
  Begin
    Line := Bmp.scanline[Height - H - 1];   // flip JPEG
    For W:=0 to Width - 1 do
    Begin
      c := Line^ and $FFFFFF; // Need to do a color swap
      Data[W+(H*Width)] :=(((c and $FF) shl 16)+(c shr 16)+(c and $FF00)) or $FF000000;  // 4 channel.
      inc(Line);
    End;
  End;  
  Bmp.Free;

  Texture := CreateTexture(Width, Height, GL_RGBA, addr(Data[0]));
  result := TRUE;
end;

type
  TTGAHeader =  packed record   // Header type for TGA images
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

function LoadTGATexture(Stream: TMemoryStream; Filename: String; var Texture: GLuint): Boolean;
var
  TGAHeader: TTGAHeader;
  image     : Pointer;    {or PRGBTRIPLE}
  CompImage : Pointer;
  Width, Height : Integer;
  ColorDepth    : Integer;
  ImageSize     : Integer;
  BufferIndex : Integer;
  currentByte : Integer;
  CurrentPixel : Integer;
  I : Integer;
  Front: ^Byte;
  Back: ^Byte;
  Temp: Byte;
  MStr: TMemoryStream;
begin
  MStr := TMemoryStream.Create;

  if Stream = nil then
  begin
    if FileExists(Filename) then MStr.LoadFromFile(Filename)
    else
    begin
      MessageBox(0, PChar('File not found  - ' + Filename), PChar('TGA Texture'), MB_OK);
      MStr.Free;
      Result := FALSE;
      Exit;
    end;
  end else MStr.LoadFromStream(Stream);

  MStr.ReadBuffer(TGAHeader, SizeOf(TGAHeader));

    // Only support 24, 32 bit images
  if (TGAHeader.ImageType <> 2) AND    { TGA_RGB }
     (TGAHeader.ImageType <> 10) then  { Compressed RGB }
  begin
    Result := False;
    MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 24 and 32bit TGA supported.'), PChar('TGA File Error'), MB_OK);
    MStr.Free;
    Exit;
  end;

    // Don't support colormapped files
  if TGAHeader.ColorMapType <> 0 then
  begin
    Result := False;
    MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Colormapped TGA files not supported.'), PChar('TGA File Error'), MB_OK);
    MStr.Free;
    Exit;
  end;

    // Get the width, height, and color depth
  Width := TGAHeader.Width[0] + TGAHeader.Width[1] * 256;
  Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
  ColorDepth := TGAHeader.BPP;
  ImageSize := Width*Height * (ColorDepth div 8);

  if ColorDepth < 24 then
  begin
    Result := False;
    MStr.Free;
    MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 24 and 32 bit TGA files supported.'), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  GetMem(Image, ImageSize);

  if TGAHeader.ImageType = 2 then   // Standard 24, 32 bit TGA file
  try
    MStr.Read(image^, ImageSize);
    MStr.Free;
  except
    MessageBox(0, PChar('Unable to read from resource - ' + Filename), PChar('BMP Unit'), MB_OK);
    Result := False;
    Exit;
  end;

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
    Texture := CreateTexture(Width, Height, GL_RGB, Image);
  end else
  begin
    for I :=0 to Width * Height - 1 do
    begin
      Front := Pointer(Integer(Image) + I*4);
      Back := Pointer(Integer(Image) + I*4 + 2);
      Temp := Front^;
      Front^ := Back^;
      Back^ := Temp;
    end;
    Texture := CreateTexture(Width, Height, GL_RGBA, Image);
  end;

  // Compressed 24, 32 bit TGA files
  if TGAHeader.ImageType = 10 then
  begin
    ColorDepth := ColorDepth DIV 8;
    CurrentByte :=0;
    CurrentPixel :=0;
    BufferIndex :=0;

    GetMem(CompImage, MStr.Size - sizeOf(TGAHeader));
    try
      MStr.Read(CompImage^, MStr.Size - sizeOf(TGAHeader));
      MStr.Free;
    except
      MessageBox(0, PChar('Unable to read from resource - ' + Filename), PChar('BMP Unit'), MB_OK);
      Result := False;
      Exit;
    end;

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
        BufferIndex := BufferIndex + (Front^+1)*ColorDepth
      end else
      begin
        For I := 0 to Front^ -128 do
        begin
          CopySwapPixel(Pointer(Integer(CompImage)+BufferIndex), Pointer(Integer(image)+CurrentByte));
          CurrentByte := CurrentByte + ColorDepth;
          inc(CurrentPixel);
        end;
        BufferIndex :=BufferIndex + ColorDepth
      end;
    until CurrentPixel >= Width*Height;

    if ColorDepth = 3 then
      Texture := CreateTexture(Width, Height, GL_RGB, Image)
    else
       Texture := CreateTexture(Width, Height, GL_RGBA, Image);
  end;

  Result := TRUE;
  FreeMem(Image);
end;

function LoadTexture(Stream: TMemoryStream; Filename: String; var Texture : GLuint) : Boolean;
begin
  Result := false;
  if copy(Uppercase(filename), length(filename)-3, 4) = '.BMP' then
    Result := LoadBMPTexture(Stream, Filename, Texture);
  if copy(Uppercase(filename), length(filename)-3, 4) = '.JPG' then
    Result := LoadJPGTexture(Stream, Filename, Texture);
  if copy(Uppercase(filename), length(filename)-3, 4) = '.TGA' then
    Result := LoadTGATexture(Stream, Filename, Texture);
end;

end.
