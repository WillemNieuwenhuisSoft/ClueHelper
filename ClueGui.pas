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
    domainLabel: TLabel;
    georefLabel: TLabel;
    cluefolderEdit: TButtonedEdit;
    arrowImages: TImageList;
    cluefolderListMenu: TPopupMenu;
    mainEvents: TApplicationEvents;
    processedFolderListMenu: TPopupMenu;
    basefolderEdit: TButtonedEdit;
    ilwisDomainBEdit: TButtonedEdit;
    domainsMenu: TPopupMenu;
    georefsMenu: TPopupMenu;
    ilwisGeorefBEdit: TButtonedEdit;
    styleChooser: TComboBox;
    progressConvertMove: TProgressBar;
    progressLabel: TLabel;
    exploreButton: TButton;
    historyCombobox: TComboBox;
    recentLabel: TLabel;
    overwriteCheckbox: TCheckBox;
    aboutButton: TButton;
    procedure Btn_CloseClick(Sender: TObject);
    procedure Btn_UpdateClick(Sender: TObject);
    procedure buttonPanelResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mainEventsActivate(Sender: TObject);
    procedure changeStyleClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure exploreButtonClick(Sender: TObject);
    procedure startatEditExit(Sender: TObject);
    procedure patternEditExit(Sender: TObject);
    procedure aboutButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    TaskBarNative: ITaskBarList3;
    _ic : TImageConvertor;

    procedure HandleThreadResult(var Message: TUMWorkerDone); message UM_WORKERDONE;
    procedure HandleThreadProgress(var Message: TUMWorkerProgress); message UM_WORKERPROGRESS;

    procedure saveConfig;
    procedure populateFromConfig;

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
    CluesConfig, about, TlHelp32;


function ProcessCount(const ExeName: String): Integer;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle:= CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize:= SizeOf(FProcessEntry32);
  ContinueLoop:= Process32First(FSnapshotHandle, FProcessEntry32);
  Result:= 0;
  while Integer(ContinueLoop) <> 0 do begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(ExeName)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(ExeName))) then Inc(Result);
    ContinueLoop:= Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

procedure TClueForm.aboutButtonClick(Sender: TObject);
var
    about : TaboutForm;
begin
    about := TaboutForm.Create(self);
    about.Show;
end;

procedure TClueForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    saveConfig;
end;


procedure TClueForm.Btn_CloseClick(Sender: TObject);
begin
    Close;
end;

procedure TClueForm.Btn_UpdateClick(Sender: TObject);
begin
    saveConfig;

    Btn_Close.Enabled := false;
    progressConvertMove.Visible := true;

    // start thread
    _ic := TImageConvertor.Create(self.Handle);

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

    // Now adjust the progress label
    progressLabel.Top := 0;
    progressLabel.Left := 0;
    progressLabel.Width := progressConvertMove.ClientWidth;
    progressLabel.Height := progressConvertMove.ClientHeight;

end;

procedure TClueForm.FormCreate(Sender: TObject);
var
    configname : string;
    styles : TArray<String>;
    i : integer;
    szpos : string;
    parts : TStringList;
    clueHandle : THandle;
begin
    // theme related stuff
    styles := TStylemanager.StyleNames;
    for i := 0 to length(styles) - 1 do
        styleChooser.AddItem(styles[i], nil);
    styleChooser.ItemIndex := 2;    // 'Emerald Light Slate'

    TStyleManager.TrySetStyle(styleChooser.Items[2]);
    TaskBarNative := CreateComObject(CLSID_TaskbarList) as ITaskBarList3;
    TaskBarNative.SetProgressState(Handle, ord(TTaskBarProgressState.Normal));

    // setup of progress bar
    progressLabel.Parent := progressConvertMove;
    progressLabel.AutoSize := false;
    progressLabel.Transparent := true;
    progressLabel.Top := 0;
    progressLabel.Left := 0;
    progressLabel.Width := progressConvertMove.ClientWidth;
    progressLabel.Height := progressConvertMove.ClientHeight;
    progressLabel.Alignment := taCenter;
    progressLabel.Layout := tlCenter;

    // Actual application stuff
    configname := extractFilePath(Application.ExeName) + 'clues.config';
    config := TCluesConfig.Create(configname);

    parts := TStringList.Create;
    parts.Delimiter := ',';
    szpos := config.item['WindowPosition'];
    if Length(szpos) > 0 then
    begin
        parts.CommaText := szpos;
        if parts.Count = 2 then begin
            self.Top := StrToIntDef(parts[0], self.Top);
            self.Left := StrToIntDef(parts[1], self.Left);
        end;

    end;
    szpos := config.item['WindowSize'];
    if Length(szpos) > 0 then
    begin
        parts.CommaText := szpos;
        if parts.Count = 2 then begin
            self.Width := StrToIntDef(parts[0], self.Width);
            self.Height := StrToIntDef(parts[1], self.Height);
        end;

    end;
    parts.Free;

    populateFromConfig;

    historyCombobox.Items.Clear;
    for i := config.history.Count - 1 downto 0 do
        historyCombobox.Items.Add(config.history[i]); // add in reverse order
    historyCombobox.ItemIndex := 0;

    if ProcessCount('clues.exe') > 0 then
    begin
        clueHandle := FindWindow(nil, 'Dyna-CLUE');
        if clueHandle > 0 then
            SetForegroundWindow(clueHandle);
    end
    else
        if FileExists(ExpandFileName(clueFolderEdit.Text) + '\clues.exe') then
            ShellExecute(Handle, 'open', PChar(ExpandFileName(clueFolderEdit.Text) + '\clues.exe'), nil, PChar(ExpandFileName(clueFolderEdit.Text)), SW_SHOWNORMAL) ;
end;

// repopulate the folder lists and the files lists to account for external changes
procedure TClueForm.mainEventsActivate(Sender: TObject);

    procedure addMenuItem(where : TPopupMenu; fname : string; eventHandler : TNotifyEvent);
    var
        item : TMenuItem;
    begin
        item := TMenuItem.Create(where);
        item.Caption := extractFileName(fname);
        item.OnClick := eventHandler;
        item.SubMenuImages := arrowImages;
        item.ImageIndex := 1;
        where.Items.Add(item);
    end;
var
    i : integer;
    path : string;
    folders : TStringDynArray;
    files : TStringDynArray;
begin
    folders := TDirectory.getDirectories(extractFilePath(Application.ExeName));
    clueFolderListMenu.Items.Clear;
    processedFolderListMenu.Items.Clear;
    for i := 0 to length(folders) - 1 do begin
        addMenuItem(cluefolderListMenu, ExtractFileName(folders[i]), clueFolderMenuClick);
        addMenuItem(processedFolderListMenu, ExtractFileName(folders[i]), ProcessedFolderMenuClick);
    end;
    addMenuItem(processedFolderListMenu, '<new>', ProcessedFolderMenuClick);

    path := ExpandFileName(cluefolderEdit.Text);
    // get ilwis domain files
    domainsMenu.Items.Clear;
    ilwisDomainBEdit.Color := clWebAliceBlue;
    if DirectoryExists(path) then begin
        files := TDirectory.GetFiles(path, '*.dom');
        for i := 0 to length(files) - 1 do
            addMenuItem(domainsMenu, extractFileName(files[i]), domainsMenuClick);

        ilwisDomainBEdit.Enabled := (length(files) > 0);
        if length(files) > 0 then
            ilwisDomainBEdit.Color := clWindow;
    end;
    // get ilwis georef files
    georefsMenu.Items.Clear;
    ilwisGeorefBEdit.Color := clWebAliceBlue;
    if DirectoryExists(path) then begin
        files := TDirectory.GetFiles(path, '*.grf');
        for i := 0 to length(files) - 1 do
            addMenuItem(georefsMenu, extractFileName(files[i]), georefsMenuClick);

        ilwisGeorefBEdit.Enabled := (length(files) > 0);
        if length(files) > 0 then
            ilwisGeorefBEdit.Color := clWindow;
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
    updateFolderEdit(ilwisDomainBEdit, diskitem);
end;

procedure TClueForm.exploreButtonClick(Sender: TObject);
begin
    if historyCombobox.ItemIndex >= 0 then
        ShellExecute(Handle, 'open', PChar(historyCombobox.Items[historyCombobox.ItemIndex]), nil, nil, SW_SHOWNORMAL);
end;

procedure TClueForm.georefsMenuClick(Sender: TObject);
var
    diskitem : string;
begin
    if not (Sender is TMenuItem) then
        exit;

    diskitem := (Sender as TMenuItem).Caption;
    updateFolderEdit(ilwisGeorefBEdit, diskitem);
end;

procedure TClueForm.HandleThreadProgress(var Message: TUMWorkerProgress);
var
    progress : integer;
begin
    progressConvertMove.Max := Message.total;
    progressConvertMove.Position := Message.progress;
    progress := Message.progress * 100 div Message.total;
    progressLabel.Caption := Format('%d %%', [progress]);
    TaskBarNative.SetProgressValue(Handle, Message.progress, Message.total);
end;

procedure TClueForm.HandleThreadResult(var Message: TUMWorkerDone);
begin
    progressConvertMove.Visible := false;
    TaskBarNative.SetProgressValue(Handle, 0, 100);
    historyCombobox.Items.Insert(0, ExpandFileName(config.getScenarioFolder));
    historyCombobox.ItemIndex := 0;
    exploreButton.Enabled := historyCombobox.Items.Count > 0;
    Btn_Close.Enabled := true;
    Btn_Close.SetFocus;

    // update config
    config.nextFolder;
    populateFromConfig;

    if Message.Result = 2 then ShowMessage('Files successfully moved and converted');
    if Message.Result = 1 then ShowMessage('Error moving and converting files');
    if Message.Result = 10 then ShowMessage('Could not create folder in destination');
    if Message.Result = 20 then ShowMessage('Missing files in source folder; did you run the simulation?');

end;

procedure TClueForm.updateFolderEdit(edit : TObject; folder : string);
var
    item : TButtonedEdit;
    newfolder : string;
begin
    if not (edit is TButtonedEdit) then
        exit;

    item := edit as TButtonedEdit;

    if folder = '<new>' then begin
        if Length(item.Text) > 0 then
            newfolder := ExpandFileName(item.Text);
            if not DirectoryExists(newfolder) then begin
                if CreateDir(newfolder) then begin
                    folder := ExtractFileName(newfolder);
                    mainEventsActivate(self);   // add the new folder to the menus
                end;
            end;
    end;

    item.Text := folder;
end;

procedure TClueForm.saveConfig;
begin
    // save conversion configuration
    config.item['CluesOutputFolder'] := cluefolderEdit.Text;
    config.item['BaseDestinationFolder'] := basefolderEdit.Text;
    config.item['SubfolderPattern'] := patternEdit.Text;
    config.item['IlwisGeoref'] := ilwisGeorefBEdit.Text;
    config.item['IlwisDomain'] := ilwisDomainBEdit.Text;
    if overwriteCheckbox.Checked then
        config.item['OverwriteFolder'] := 'True'
    else
        config.item['OverwriteFolder'] := 'False';

    // save gui stuff
    config.item['WindowPosition'] := Format('%d,%d', [self.Top, self.Left]);
    config.item['WindowSize'] := Format('%d,%d', [self.Width, self.Height]);
    if styleChooser.ItemIndex >= 0 then
        config.item['Theme'] := styleChooser.Items[styleChooser.ItemIndex]
    else
        config.item['Theme'] := styleChooser.Items[2];

end;

procedure TClueForm.startatEditExit(Sender: TObject);
var
    num :integer;
begin
    if length(startatEdit.Text) > 0 then begin
        num := StrToIntDef(startatEdit.Text, -1);
        if num > 0 then
            config.startNum := num
        else
            ShowMessage('Please enter a valid number');
    end;
end;

procedure TClueForm.patternEditExit(Sender: TObject);
var
    i : integer;
begin
    if length(basefolderEdit.Text) > 0 then
        config.item['BaseDestinationFolder'] := basefolderEdit.Text;
    if length(patternEdit.Text) > 0 then
    begin
        config.pattern := patternEdit.Text;
        historyCombobox.Items.Clear;
        for i := config.history.Count - 1 downto 0 do
            historyCombobox.Items.Add(config.history[i]);   // add in reverse order
        historyCombobox.ItemIndex := 0;
        startatEdit.Text := config.getStartNumAsString;
    end;

end;

procedure TClueForm.populateFromConfig;
var
    i, index : integer;
    item : string;
begin
    item := config.item['CluesOutputFolder'];
    if (length(item) > 0) and DirectoryExists(item) then
    begin
        cluefolderEdit.Text := item;
    end;
    item := config.item['BaseDestinationFolder'];
    if (length(item) > 0) and DirectoryExists(item) then basefolderEdit.Text := item;
    item := config.item['SubfolderPattern'];
    if length(item) > 0 then patternEdit.Text := item;
    if config.startNum > 0 then
        startatEdit.Text := config.getStartNumAsString;

    item := config.item['IlwisGeoref'];
    if (length(item) > 0) and (length(config.sourceFolder) > 0) then
        if FileExists(ExpandFileName(config.sourceFolder + '\' + item)) then
            ilwisGeorefBEdit.Text := item;
    item := config.item['IlwisDomain'];
    if (length(item) > 0) and (length(config.sourceFolder) > 0) then
        if FileExists(ExpandFileName(config.sourceFolder + '\' + item)) then
            ilwisDomainBEdit.Text := item;

    item := config.item['OverwriteFolder'];
    if length(item) > 0 then overwriteCheckbox.Checked := item = 'True';

    item := config.item['Theme'];
    index := styleChooser.Items.IndexOf(item);
    if index >= 0 then
        styleChooser.ItemIndex := index
    else
        styleChooser.ItemIndex := 2;    // fallback

    changeStyleClick(self);
end;

end.
