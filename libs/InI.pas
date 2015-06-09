unit InI;

interface

uses IniFiles, SysUtils, Classes, dialogs;

type
  TIniFile = IniFiles.TIniFile;

  TIni = class(TMemIniFile)
  public
    constructor Create(const TextOrFName: string); overload;
    constructor Create(Stream: TStream); overload;
    procedure LoadFromStram(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure SetText(Text: String);
    procedure GetText(var Text: String);
    procedure ReadListSection(Section: string; List: TStrings);
    procedure WriteListSection(Section: string; List: TStrings);
  end;

implementation

procedure TIni.SetText(Text: String);
var List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := Text;
    SetStrings(List);
  finally
    List.Free;
  end;
end;

procedure TIni.WriteListSection(Section: string; List: TStrings);
var i: integer;
begin
  EraseSection(Section);
  for i := 0 to List.Count - 1 do
  begin
    WriteString(Section, IntToStr(i), List[i]);
  end;
end;

constructor TIni.Create(Stream: TStream);
begin
  inherited Create('');
  LoadFromStram(Stream);
end;

constructor TIni.Create(const TextOrFName: string);
begin
  if FileExists(TextOrFName) then
    inherited Create(FileName)
  else
  begin
    inherited Create('');
    SetText(TextOrFName);
  end;
end;

procedure TIni.GetText(var Text: String);
var List: TStringList;
begin
  List := TStringList.Create;
  try
    GetStrings(List);
    Text := List.Text;
  finally
    List.Free;
  end;
end;

procedure TIni.LoadFromStram(Stream: TStream);
var List: TStringList;
begin
  if Stream = nil then exit;
  List := TStringList.Create;
  try
    Stream.Position := 0;
    List.LoadFromStream(Stream);
    SetStrings(List);
  finally
    List.Free;
  end;
end;

procedure TIni.ReadListSection(Section: string; List: TStrings);
var i: integer;
begin
  List.Clear;
  i := 0;
  while ValueExists(Section, IntToStr(i)) do
  begin
    List.Add(ReadString(Section, IntToStr(i), ''));
    Inc(i);
  end;
end;

procedure TIni.SaveToStream(Stream: TStream);
var
  Str: TStringList;
begin
  Str := TStringList.Create;
  GetStrings(Str);
  Stream.Position := 0;
  Str.SaveToStream(Stream);
  Str.Free;
end;

end.
