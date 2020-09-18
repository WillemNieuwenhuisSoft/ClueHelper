program ClueHelper;

uses
  Vcl.Forms,
  ClueGui in 'ClueGui.pas' {ClueForm},
  AscFile in 'AscFile.pas',
  IlwisMap in 'IlwisMap.pas',
  ClueTypes in 'ClueTypes.pas',
  AscToIlwis in 'AscToIlwis.pas',
  CluesConfig in 'CluesConfig.pas',
  MoveScenarioFiles in 'MoveScenarioFiles.pas',
  ImageConvertor in 'ImageConvertor.pas',
  Vcl.Themes,
  Vcl.Styles,
  About in 'About.pas' {aboutForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TClueForm, ClueForm);
  Application.CreateForm(TaboutForm, aboutForm);
  Application.Run;
end.
