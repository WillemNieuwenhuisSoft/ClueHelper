program ClueS;

uses
  Vcl.Forms,
  ClueGui in 'ClueGui.pas' {ClueForm},
  AscFile in 'AscFile.pas',
  IlwisMap in 'IlwisMap.pas',
  ClueTypes in 'ClueTypes.pas',
  AscToIlwis in 'AscToIlwis.pas',
  CluesConfig in 'CluesConfig.pas',
  SelectFolder in 'SelectFolder.pas' {selectFolderForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TClueForm, ClueForm);
  Application.CreateForm(TselectFolderForm, selectFolderForm);
  Application.Run;
end.
