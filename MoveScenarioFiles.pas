unit MoveScenarioFiles;

interface

type
    TMoveScenarioFiles = class

//	static Logger logger = Logger.getLogger(MoveScenarioFiles.class.getName());
//	private CluesConfig config;
	private
        function createFolder(newFolder : string) : boolean;

	public
        constructor Create;

        procedure moveScenarioFiles;
        procedure copyServiceObjects;

    end;

implementation

uses
    classes, system.types, Sysutils, ioutils, inifiles, regularexpressions, windows,
    cluesconfig;

type

	TScenarioFileFilter = class
        _regex : TRegEx;
        _match : TMatch;
    public
        constructor Create;

        function accept(filename : string): boolean;
    end;

    constructor TScenarioFileFilter.Create;
    var
        name_part : string;
    begin
        _regex := TRegEx.Create('cov_all[.]\\d+|prob\\d+_.*[.]\\d+');
    end;

    function TScenarioFileFilter.accept(filename : string): boolean;
    begin
        _match := _regex.Match(filename);
        accept := _match.Success;
    end;



{ TMoveScenarioFiles }

constructor TMoveScenarioFiles.Create;
begin
    //
end;

function TMoveScenarioFiles.createFolder(newFolder: string): boolean;
begin
    if DirectoryExists(newFolder) then
//			logger.info("Folder '" + newFolder + "' already exist, content (if any) will be replaced");
    else begin
        try
            TDirectory.CreateDirectory(newFolder);
        except
//    		logger.severe("Unable to create base folder '" + newFolder + "', quitting program");
    		createFolder := false;
        end;
		createFolder := true;

    end;
end;

procedure TMoveScenarioFiles.moveScenarioFiles;
var
    sourceFolder : string;
    targetFolder : string;
    newfile : string;
    filter : TScenarioFileFilter;
    done,
    i : integer;
    files : TStringDynArray;
begin
    // retrieve the source and destination folders
    sourceFolder := config.item['CluesOutputFolder'];
    targetFolder := config.getScenarioFolder;

    // destination does not exist (at least should not; is checked at startup or change of destination root)
    // so create the new destination
    if not createFolder(targetFolder) then
        // Unable to create destination folder; moving files NOT initiated
        exit;

    // get all the files from the source
    // and only move them if they are actual datafiles
    files := TDirectory.GetFiles(sourceFolder);
    filter := TScenarioFileFilter.Create;
    done := 0;
//		logger.info(source + " --> " + dest);
    for i := 0 to length(files) - 1 do begin
        if filter.accept(files[i]) then begin
            // move the file
            newfile := ChangeFilePath(ChangeFileExt(files[i], '.asc'), targetFolder);
            try
                TDirectory.Move(files[i], newfile);
                inc(done);
            except
//					logger.warning(file + " NOT moved to " + newFile);
            end;
        end;
    end;
//		logger.info("Moving files complete; " + Integer.toString(done) + " files moved");


end;


procedure TMoveScenarioFiles.copyServiceObjects;
var
    filesToCopy : TStringList;
    i : integer;
    entry : string;
    dom : string;
    rpr : string;
    grf : string;
    domIni : TiniFile;
    rprIni : TIniFile;
    grfIni : TiniFile;
begin
    filesToCopy := TStringList.Create;
    // Domain and related
    dom := ChangeFilePath(config.item['IlwisDomain'], config.sourceFolder);
    filesToCopy.add(dom);

    domIni := TIniFile.Create(dom);
    entry := domIni.ReadString('TableStore', 'Data', '');       // cannot be empty
    filesToCopy.Add(ChangeFilePath(entry, config.sourceFolder));
    entry := domIni.ReadString('Domain', 'Representation', '');  // maybe empty
    if length(ExtractFileExt(entry)) > 0 then begin
        rpr := ChangeFilePath(entry, config.sourceFolder);
        filesToCopy.Add(rpr);
        rprIni := TIniFile.Create(rpr);
        entry := rprIni.ReadString('TableStore', 'Data', '');   // maybe empty
        if length(entry) > 0  then
            filesToCopy.Add(ChangeFilePath(entry, config.sourceFolder));
    end;
    domIni.Free;
    rprIni.Free;

    // Georef and related
    grf := ChangeFilePath(config.item['IlwisGeoref'], config.sourceFolder);
    filesToCopy.Add(grf);

    grfIni := TIniFile.Create(grf);
    entry := grfIni.ReadString('TableStore', 'Data', '');       // cannot be empty
    filesToCopy.Add(ChangeFilePath(entry, config.sourceFolder));
    entry := grfIni.ReadString('GeoRef', 'CoordSystem', '');    // maybe empty
    if length(entry) > 0  then
        filesToCopy.Add(ChangeFilePath(entry, config.sourceFolder));
    entry := grfIni.ReadString('GeoRefCTP', 'Reference Map', ''); // maybe empty
    if length(entry) > 0  then
        filesToCopy.Add(ChangeFilePath(entry, config.sourceFolder));

    // Now copy
    for i := 0 to filesToCopy.Count - 1 do begin
        if FileExists(filesToCopy[i]) then
            TFile.Copy(filesToCopy[i], ChangeFilePath(filesToCopy[i], config.getScenarioFolder));

    end;
end;

end.
