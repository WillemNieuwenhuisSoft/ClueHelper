unit ClueGui;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.AppAnalytics, System.Actions, Vcl.ActnList, System.ImageList, Vcl.ImgList,
  Vcl.Menus, Vcl.AppEvnts, Vcl.Mask, Vcl.ComCtrls, System.Win.TaskbarCore,
  Vcl.Taskbar, ShlObj, imageconvertor, cluetypes;

type
  TClueForm = class(TForm)
    buttonPanel: TPanel;
    Btn_Close: TButton;
    Btn_Update: TButton;
    mainPanel: TPanel;
    basefolderLabel: TLabel;
    cluefolderLabel: TLabel;
    patternLabel: TLabel;
    patternEdit: TEdit;
    startatlabel: TLabel;
    startatEdit: TEdit;
    exampleEdit: TEdit;
    domainLabel: TLabel;
    georefLabel: TLabel;
    cluefolderEdit: TButtonedEdit;
    arrowImages: TImageList;
    cluefolderListMenu: TPopupMenu;
    mainEvents: TApplicationEvents;
    processedFolderListMenu: TPopupMenu;
    basefolderEdit: TButtonedEdit;
    domainBEdit: TButtonedEdit;
    domainsMenu: TPopupMenu;
    georefsMenu: TPopupMenu;
    georefBEdit: TButtonedEdit;
    progressConvertMove: TProgressBar;
    styleChooser: TComboBox;
    procedure Btn_CloseClick(Sender: TObject);
    procedure Btn_UpdateClick(Sender: TObject);
    procedure buttonPanelResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mainEventsActivate(Sender: TObject);
    procedure changeStyleClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    TaskBarNative: ITaskBarList3;
    _thread : TImageConvertor;
    _thread_handle : THandle;

    procedure HandleThreadResult(var Message: TUMWorkerDone); message UM_WORKERDONE;
    procedure HandleThreadProgress(var Message: TUMWorkerProgress); message UM_WORKERPROGRESS;

    procedure saveConfig;
    procedure saveSpecialTEdit(parent: TWinControl);
    procedure populateFromConfig(parent : TWinControl);

    procedure clueFolderMenuClick(Sender: TObject);
    procedure processedFolderMenuClick(Sender: TObject);
    procedure updateFolderEdit(edit : TObject; folder : string);
    procedure domainsMenuClick(Sender: TObject);
    procedure georefsMenuClick(Sender: TObject);

  public
    { Public declarations }
  end;

var
  ClueForm: TClueForm;

implementation

{$R *.dfm}

uses
    Registry, ShellApi, ComObj,
    themes, styles, system.types, ioutils,
    CluesConfig;

procedure TClueForm.Btn_CloseClick(Sender: TObject);
begin
    saveConfig;
    Close;
end;

procedure TClueForm.Btn_UpdateClick(Sender: TObject);
begin
    saveConfig;

    Btn_Close.Enabled := false;

    try
        progressConvertMove.Visible := true;
        // start thread
//        _thread := TImageConvertor.Create(self.Handle);
//        _thread_handle := _thread.Handle;
    // TODO: start the file convert and copy

//        for i := 0 to 99 do begin
//            Application.ProcessMessages;
//            progressConvertMove.Position := i;
//            sleep(25);
//            TaskBarNative.SetProgressValue(Handle, i, 100);
//        end;
//        progressConvertMove.Position := 100;
//        Application.ProcessMessages;
//        sleep(200);
    finally
        progressConvertMove.Visible := false;
        Btn_Close.Enabled := true;
        Btn_Close.SetFocus;
    end;
end;

procedure TClueForm.changeStyleClick(Sender: TObject);
begin
    if styleChooser.ItemIndex >= 0 then begin
        TStyleManager.TrySetStyle(styleChooser.Items[styleChooser.ItemIndex]);
    end;
end;

procedure TClueForm.buttonPanelResize(Sender: TObject);
begin
    Btn_Close.Width := buttonPanel.Width div 2;
    Btn_Update.Width := buttonPanel.Width div 2;
end;

procedure TClueForm.FormResize(Sender: TObject);
begin
    // prepare size of progressbar on top of move button
    progressConvertMove.Left := Btn_Update.Left + 2;
    progressConvertMove.Width := Btn_Update.Width - 4;
    progressConvertMove.Top := Btn_Update.Top + 2;
    progressConvertMove.Height := Btn_Update.Height - 4;
end;


procedure TClueForm.FormCreate(Sender: TObject);
var
    configname : string;
    styles : TArray<String>;
    i : integer;
begin
    // theme related stuff
    styles := TStylemanager.StyleNames;
    for i := 0 to length(styles) - 1 do
        styleChooser.AddItem(styles[i], nil);
    styleChooser.ItemIndex := 2;    // 'Emerald Light Slate'

    TStyleManager.TrySetStyle(styleChooser.Items[2]);
    TaskBarNative := CreateComObject(CLSID_TaskbarList) as ITaskBarList3;
    TaskBarNative.SetProgressState(Handle, ord(TTaskBarProgressState.Normal));

    // Actual application stuff
    configname := extractFilePath(Application.ExeName) + 'clues.config';
    config := TCluesConfig.Create(configname);

    populateFromConfig(self);

end;

// repopulate the folder lists and the files lists to account for external changes
procedure TClueForm.mainEventsActivate(Sender: TObject);
var
    i : integer;
    item : TMenuItem;
    path : string;
    folders : TStringDynArray;
    files : TStringDynArray;
begin
    folders := TDirectory.getDirectories(extractFilePath(Application.ExeName));
    clueFolderListMenu.Items.Clear;
    processedFolderListMenu.Items.Clear;
    for i := 0 to length(folders) - 1 do begin
        item := TMenuItem.Create(cluefolderListMenu);
        item.Caption := extractFileName(folders[i]);
        item.OnClick := clueFolderMenuClick;
        item.SubMenuImages := arrowImages;
        item.ImageIndex := 1;
        clueFolderListMenu.Items.Add(item);

        item := TMenuItem.Create(processedFolderListMenu);
        item.Caption := extractFileName(folders[i]);
        item.OnClick := ProcessedFolderMenuClick;
        item.SubMenuImages := arrowImages;
        item.ImageIndex := 1;
        processedFolderListMenu.items.Add(item);
    end;

    path := ExpandFileName(cluefolderEdit.Text);
    // get ilwis domain files
    domainsMenu.Items.Clear;
    domainBEdit.Color := clWebAliceBlue;
    if DirectoryExists(path) then begin
        files := TDirectory.GetFiles(path, '*.dom');
        for i := 0 to length(files) - 1 do begin
            item := TMenuItem.Create(domainsMenu);
            item.Caption := extractFileName(files[i]);
            item.OnClick := domainsMenuClick;
            item.SubMenuImages := arrowImages;
            item.ImageIndex := 2;
            domainsMenu.Items.Add(item);
        end;
        domainBEdit.Enabled := (length(files) > 0);
        if length(files) > 0 then
            domainBEdit.Color := clWindow;
    end;
    // get ilwis georef files
    georefsMenu.Items.Clear;
    georefBEdit.Color := clWebAliceBlue;
    if DirectoryExists(path) then begin
        files := TDirectory.GetFiles(path, '*.grf');
        for i := 0 to length(files) - 1 do begin
            item := TMenuItem.Create(georefsMenu);
            item.Caption := extractFileName(files[i]);
            item.OnClick := georefsMenuClick;
            item.SubMenuImages := arrowImages;
            item.ImageIndex := 3;
            georefsMenu.Items.Add(item);
        end;
        georefBEdit.Enabled := (length(files) > 0);
        if length(files) > 0 then
            georefBEdit.Color := clWindow;
    end;
end;

procedure TClueForm.clueFolderMenuClick(Sender: TObject);
var
    folder : string;
begin
    if not (Sender is TMenuItem) then
        exit;

    folder := (Sender as TMenuItem).Caption;
    updateFolderEdit(clueFolderEdit, folder);
    mainEventsActivate(cluefolderEdit);
end;

procedure TClueForm.processedFolderMenuClick(Sender: TObject);
var
    folder : string;
begin
    if not (Sender is TMenuItem) then
        exit;

    folder := (Sender as TMenuItem).Caption;
    updateFolderEdit(baseFolderEdit, folder);
end;

procedure TClueForm.domainsMenuClick(Sender: TObject);
var
    diskitem : string;
begin
    if not (Sender is TMenuItem) then
        exit;

    diskitem := (Sender as TMenuItem).Caption;
    updateFolderEdit(domainBEdit, diskitem);
end;

procedure TClueForm.georefsMenuClick(Sender: TObject);
var
    diskitem : string;
begin
    if not (Sender is TMenuItem) then
        exit;

    diskitem := (Sender as TMenuItem).Caption;
    updateFolderEdit(georefBEdit, diskitem);
end;

procedure TClueForm.HandleThreadProgress(var Message: TUMWorkerProgress);
begin
    progressConvertMove.Position := Message.progress;
end;

procedure TClueForm.HandleThreadResult(var Message: TUMWorkerDone);
begin
    Btn_Update.Enabled := true;
    progressConvertMove.Visible := false;
end;

procedure TClueForm.updateFolderEdit(edit : TObject; folder : string);
var
    item : TButtonedEdit;
begin
    if not (edit is TButtonedEdit) then
        exit;

    item := edit as TButtonedEdit;
    item.Text := folder;
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
        if ctrl is TButtonedEdit then begin
            // hint is "misused" to hold name of configuration property in each edit
            // showhint must be turned off!
            if not ctrl.Hint.IsEmpty then
                config.item[ctrl.Hint] := (ctrl as TButtonedEdit).Text;
        end;
        if ctrl is Tpanel then
            saveSpecialTEdit(ctrl as TPanel);

    end;
end;

procedure TClueForm.populateFromConfig(parent : TWinControl);
var
    i : integer;
    ctrl : TControl;
begin
    for i := 0 to parent.ControlCount - 1 do begin
        ctrl := parent.controls[i];
        if ctrl is TButtonedEdit then begin
            // hint is "misused" to hold name of configuration property in each edit
            // showhint must be turned off!
            if not ctrl.Hint.IsEmpty then begin
                if not config.item[ctrl.Hint].isEmpty then
                    (ctrl as TButtonedEdit).Text := config.item[ctrl.Hint];
            end;
        end;
        if ctrl is Tpanel then
            populateFromConfig(ctrl as TPanel);

    end;
end;

end.
