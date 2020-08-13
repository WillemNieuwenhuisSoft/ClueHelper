unit ClueGui;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.AppAnalytics, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList,
  Vcl.Menus;

type
  TClueForm = class(TForm)
    buttonPanel: TPanel;
    Btn_Close: TButton;
    Btn_Update: TButton;
    mainPanel: TPanel;
    basefolderLabel: TLabel;
    basefolderEdit: TEdit;
    basefolderBrowse: TButton;
    cluefolderLabel: TLabel;
    cluefolderBrowse: TButton;
    cluefolderEdit: TEdit;
    patternLabel: TLabel;
    patternEdit: TEdit;
    startatlabel: TLabel;
    startatEdit: TEdit;
    exampleEdit: TEdit;
    domainLabel: TLabel;
    domainBrowseButton: TButton;
    georefLabel: TLabel;
    georefEdit: TEdit;
    domainEdit: TEdit;
    georefBrowseButton: TButton;
    clueFolderButEdit: TButtonedEdit;
    arrowImages: TImageList;
    folderListMenu: TPopupMenu;
    folder11: TMenuItem;
    folder21: TMenuItem;
    procedure Btn_CloseClick(Sender: TObject);
    procedure Btn_UpdateClick(Sender: TObject);
    procedure buttonPanelResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure selectFolder(Sender: TObject);
    procedure clueFolderButEditRightButtonClick(Sender: TObject);
  private
    { Private declarations }

    procedure saveConfig;
    procedure saveSpecialTEdit(parent: TWinControl);

  public
    { Public declarations }
  end;

var
  ClueForm: TClueForm;

implementation

{$R *.dfm}

uses
    FileCtrl, AscFile, IlwisMap, CluesConfig;

procedure TClueForm.Btn_CloseClick(Sender: TObject);
begin
    saveConfig;
    Close;
end;

procedure TClueForm.Btn_UpdateClick(Sender: TObject);
begin
    saveConfig;
// TODO: start the file convert and copy
end;

procedure TClueForm.buttonPanelResize(Sender: TObject);
begin
    Btn_Close.Width := buttonPanel.Width div 2;
    Btn_Update.Width := buttonPanel.Width div 2;
end;

procedure TClueForm.clueFolderButEditRightButtonClick(Sender: TObject);
begin
    ShowMessage('hhh');
end;

procedure TClueForm.FormCreate(Sender: TObject);
var
    configname : string;
begin
    cluefolderBrowse.Tag := NativeInt(cluefolderEdit);
    basefolderBrowse.Tag := NativeInt(basefolderEdit);
    configname := extractFilePath(Application.ExeName) + 'clues.config';
    config := TCluesConfig.Create(configname);
end;

procedure TClueForm.saveConfig;
begin
    saveSpecialTEdit(self);
end;

procedure TClueForm.saveSpecialTEdit(parent : TWinControl);
var
    i : integer;
    ctrl : TControl;
begin
    for i := 0 to parent.ControlCount - 1 do begin
        ctrl := parent.controls[i];
        if ctrl is TEdit then begin
            // hint is "misused" to hold name of configuration property in each edit
            // showhint must be turned off!
            if not ctrl.Hint.IsEmpty then
                config.item[ctrl.Hint] := (ctrl as TEdit).Text;
        end;
        if ctrl is Tpanel then
            saveSpecialTEdit(ctrl as TPanel);

    end;
end;

procedure TClueForm.selectFolder(Sender: TObject);
var
    curPath, newPath : string;
    te : TEdit;
    tb : TButton;
    dirs : TArray<string>;
    dir : string;
begin
    if sender is TButton then begin
        tb := sender as TButton;
        if tb.Tag <> 0 then begin
            te := TEdit(tb.tag);
            curPath := ExpandFileName(te.Text);
            if SelectDirectory('Select a folder', curPath, newPath) then
                te.Text := ExtractFileName(newPath);
        end;
    end;
end;

end.
