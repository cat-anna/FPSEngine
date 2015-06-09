program FPS;

uses
  Forms,
  FPSEngine,
  SysUtils;

type
  TForm1 = class(TForm);

{$R *.res}
{$R FPSForm.dfm}

var
  Engine: TFPSEngine;
  i: integer;
  Form: TForm1;

begin
  Application.Initialize;
  Application.ShowMainForm := false;
  Application.CreateForm(TForm1, Form);
  Engine := TFPSEngine.Create;
  Engine.Initialise(Form);

  for i := 0 to ParamCount do
  begin
    if FileExists(ParamStr(i)) and (ExtractFileExt(ParamStr(i)) = ModuleExt) then
      Engine.LoadData(ParamStr(i));     
  end;
  Engine.Run(false);
  Application.Run;
  Engine.Free; 
end.
