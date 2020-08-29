unit CluesConfig;

interface

uses System.Generics.Collections;

type
    TProperties = (CluesOutputFolder = 1, BaseDestinationFolder = 2, SubfolderPattern = 3, IlwisGeoref = 4, IlwisDomain = 5, Invalid = 99);

    TCluesConfig = class
        _config : TDictionary<string, string>;

        _name : string;         // filename of the config file
        _sourcePath : string;   // folder where clue stores the output (usually the folder where the exe can be found)
        _basePath : string;     // target folder for processed data
        _folderPattern : string;

        _startNum : integer;
        _valid,
        _changed: Boolean;

    private

        function getDestPath: string;
        function getLastDir(path: string): integer;
        function getPattern(pattern: string; start: integer): string;
        function getStartNum: string;
        procedure nextFolder;
        procedure save;
        procedure load;
        procedure setFolderPattern(folderPattern: string);
        procedure setStartNum(startNum: string);
        function getProperty(key : string) : string;
        procedure setProperty(key: string; const Value: string);

    public
        constructor Create(configname : string);
        destructor Destroy; override;

        function getScenarioFolder : string;

        property configname : string read _name write _name;
        property sourceFolder : string read _sourcePath write _sourcePath;
//        property basePath : string read _basePath write _basePath;
//        property folderPattern : string read _folderPattern write _folderPattern;
        property startNum : string read getStartNum write setStartNum;
        property isValid : boolean read _valid;
        property item[key : string] : string read getProperty write setProperty;
    end;

    var
        config : TCluesConfig;

implementation

uses
    typinfo, classes, sysutils, ioutils, system.types, json.types, json.readers, json.writers, regularexpressions, math;

type
    // scenario folders look like: <name-part>_<number-part>
    TScenarioFolderFilter = class
        _pattern : string;

        _regex : TRegEx;
        _match : TMatch;
        _value : integer;
    public
        constructor Create(folderPattern : string);

        function accept(foldername : string) : boolean;

        property value : integer read _value;
    end;

var
    lookup : TDictionary<string, TProperties>;

    constructor TScenarioFolderFilter.Create(folderPattern : string);
    var
        name_part : string;
    begin
        // first determine the <name-part> and use that for the following matches
        // this allows for multiple scenario setups in the same folder
        _regex := TRegEx.Create('([a-zA-Z_]*)_([0-9]{0,9})');
        _match := _regex.Match(folderPattern);

        // reset the regex to use the pattern based on the user provided name-part
        if _match.Success then begin
            name_part := _match.Groups.Item[1].Value;
            _pattern := '(' + name_part + '_)([0-9]{0,9})';
            _regex := TRegEx.Create(_pattern);
        end;
    end;

    function TScenarioFolderFilter.accept(foldername : string): boolean;
    begin
        _value := -1;
        _match := _regex.Match(foldername);
        accept := _match.Success;
        if _match.Success then
            _value := StrToInt(_match.Groups.Item[2].Value);
    end;

    //--------------------- TCluesConfig --------------------
	constructor TCluesConfig.Create(configname : string);
    begin
        _config := TDictionary<string, string>.Create;
        _name := configname;

        // apply some defaults, possibly overruled by the load
        _folderPattern := 'scen_00001';
        _startNum := 1;
        _valid := false;

        load;
        _changed := false;
    end;

	function TCluesConfig.getScenarioFolder: string;
    begin
        Result := ExpandFileName(getPattern(_folderPattern, _startNum));
    end;

	destructor TCluesConfig.Destroy;
    begin
        save;
        _config.Free;

        inherited;
    end;

    function TCluesConfig.getDestPath : string;
    begin
        getDestPath := ExpandFileName(getPattern(_folderPattern, _startNum));
    end;

    function TCluesConfig.getProperty(key : string) : string;
    begin
        if _config.ContainsKey(key) then
            getProperty := _config[key]
        else
            getProperty := '';
    end;

    procedure TCluesConfig.setProperty(key: string; const Value: string);
    var
        k : TProperties;
    begin
        // don't allow setting of empty values
        if length(Value) = 0 then
            exit;

        k := lookup[key];
        case k of
            CluesOutputFolder       : _sourcePath := Value;
            BaseDestinationFolder   : _basePath := Value;
            SubfolderPattern        : _folderPattern := Value;
        end;

        if _config.ContainsKey(key) then
            if _config[key] <> value then begin
                _config[key] := Value;
                _changed := true;
            end;
    end;

    function TCluesConfig.getStartNum : string;
    begin
		getStartNum := intToStr(_startNum);
    end;

    procedure TCluesConfig.load;
    var
        reader : TJsonTextReader;
        str_read : TStreamReader;
        stream : TFileStream;
        key : string;
        value : string;
        isPair : boolean;
        curDir : string;
    begin
        if FileExists(_name) then begin
            stream := TFileStream.Create(_name, fmOpenRead);
            str_read := TStreamReader.Create(stream);
            reader := TJsonTextReader.Create(str_read);
            try
                // very simple configuration reader, should be adapted to the actual structure.
                while reader.read do begin
                    case reader.TokenType of
                        TJsonToken.startobject: begin
                        end;
                        TJsonToken.PropertyName :  begin
                            isPair := true;
                            key := reader.Value.AsString;
                        end;
                        TJsonToken.String : begin
                            value := reader.Value.AsString;
                            _config.Add(key, value);
                            isPair := false;
                        end;
                        TJsonToken.EndObject: begin
                        end;
                    end;

                end
            finally
                reader.Close;
                str_read.Free;
                stream.Free;
            end;
            curDir := GetCurrentDir;
            if _config.ContainsKey('CluesOutputFolder') then
                _sourcePath := ExpandFileName(_config.Items['CluesOutputFolder']);
            if _config.ContainsKey('BaseDestinationFolder') then begin
                _basePath := ExpandFileName(_config.Items['BaseDestinationFolder']);
                if DirectoryExists(_basePath) then
                    _startNum := getLastDir(_basePath);
            end;
            if _config.ContainsKey('SubfolderPattern') then
                _folderPattern := _config.items['SubfolderPattern'];
        end;
        _valid := DirectoryExists(_basePath);
    end;

    procedure TCluesConfig.setStartNum(startNum : string);
    var
        value : integer;
    begin
		try
			value := StrToInt(startNum);
		except
            on EConvertError do value := 0;
        end;

		_startNum := value;
     end;

	procedure TCluesConfig.setFolderPattern(folderPattern : string);
    begin
		_folderPattern := folderPattern;
		_startNum := getLastDir(_basePath) + 1;

    end;

    function TCluesConfig.getPattern(pattern : string; start : integer) : string;
    var
        regex : TRegEx;
        match : TMatch;
        namePattern, numPattern : string;
        len : integer;
        fmtstr : string;
    begin
        regex := TRegEx.Create('([a-zA-Z_]*)([0]{0,9})');
        match := regex.Match(pattern);

        namePattern := '';
        len := 5;
        if match.Success then begin
            namePattern := match.Groups.Item[1].Value;
            numPattern := match.Groups.Item[2].Value;
            if length(pattern) > 0 then
                len := length(pattern);
        end;

        if start > 0 then begin
            fmtstr := '%0' + format('%d', [len]) + 'd';
            namePattern := namePattern + Format(fmtstr, [start]);
        end
        else
            namePattern := namePattern + '00123456789'.substring(0, len);

        getPattern := namePattern;
    end;

    // Get the highest scenarionumber from the folder names matching the folder pattern
    // The path should exist!
	function TCluesConfig.getLastDir(path : string) : integer;
    var
        filter : TScenarioFolderFilter;
        max, value : integer;
        i : integer;
        folders : TStringDynArray;
    begin
		if length(_folderPattern) = 0 then
			getLastDir := 1;

        folders := TDirectory.getDirectories(_basePath); //, TSearchOption.soTopDirectoryOnly);
		filter := TScenarioFolderFilter.Create(_folderPattern);
        max := 0;
        for i := 0 to length(folders) - 1 do begin
            if filter.accept(folders[i]) then begin
                value := filter.value;
                if value > max then
                    max := value;
            end;
        end;

        getLastDir := max;
    end;

    procedure TCluesConfig.save;
    var
        writer : TJsonTextWriter;
        stream : TFileStream;
        key : string;
    begin
        if not _changed then
            exit;

        stream := TFileStream.Create(_name, fmOpenWrite or fmCreate);
        writer := TJsonTextWriter.Create(stream);
        try
            writer.WriteStartObject;
            writer.WritePropertyName('Configuration');
            writer.WriteStartObject;
            for key in _config.keys do begin
                writer.WritePropertyName(key);
                writer.WriteValue(_config.items[key]);
            end;
            writer.WriteEndObject;

            writer.WriteEndObject;
        finally
            writer.Close;
            stream.Free;
        end;


	 {	config.put("CluesOutputFolder", Filename.getRelative(sourcePath, curDir));
		config.put("SubfolderPattern", folderPattern);
		config.setDoPersist(true); // after a call to put the entire config is saved
		config.put("BaseDestinationFolder", Filename.getRelative(basePath, curDir));
		config.setDoPersist(false);
	}
    end;

	procedure TCluesConfig.nextFolder;
    begin
		inc(_startNum);
    end;


initialization
    // allowed properties
    lookup := TDictionary<string, TProperties>.Create;
    lookup.Add('CluesOutputFolder', CluesOutputFolder);
    lookup.Add('BaseDestinationFolder', BaseDestinationFolder);
    lookup.Add('SubfolderPattern', SubfolderPattern);
    lookup.Add('IlwisGeoref', IlwisGeoref);
    lookup.Add('IlwisDomain', IlwisDomain);

finalization
    config.Free;
    lookup.Free;

end.
