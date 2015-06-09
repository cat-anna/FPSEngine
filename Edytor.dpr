program Edytor;

uses
  Forms,
  FMainForm in 'FMainForm.pas' {MainForm},
  FPSEngine in 'FPSEngine.pas',
  Utils in 'Utils.pas',
  GLUtils in 'GLUtils.pas',
  FMapEdytor in 'FMapEdytor.pas' {MapEditor},
  FPSPreObj in 'FPSPreObj.pas',
  FChooseSth in 'FChooseSth.pas' {ChooseForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
//  ReportMemoryLeaksOnShutdown := true;
end.
