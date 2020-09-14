unit CluesConfig;

interface

uses Classes, Generics.Collections;

type
    TCluesConfig = class
        _config : TDictionary<string, string>;
        _history : TStringList;

        _name : string;         // filename of the config file
        _sourcePath : string;   // folder where clue stores the output (usually the folder where the exe can be found)
        _basePath : string;     // target folder for processed data
        _folderPattern : string;

        _startNum : integer;
        _valid,
        _changed: Boolean;

    private

        function getLastDir: integer;
        function getPattern(pattern: string; start: integer): string;
        procedure save;
        procedure load;
        procedure setFolderPattern(folderPattern: string);
        procedure setStartNum(startNum: string);
        function getProperty(key : string) : string;
        procedure setProperty(key: string; const Value: string);
        function configKeys: TStrings;
        function getSourcePath: string;
        function getPatternProp: string;

    public
        constructor Create(configname : string);
        destructor Destroy; override;

        function getScenarioFolder : string;
        function getStartNumAsString: string;

        property configname : string read _name write _name;
        property sourceFolder : string read getSourcePath;
        property startNum : integer read _startNum write _startNum;
        procedure nextFolder;
        property isValid : boolean read _valid;
        property item[key : string] : string read getProperty write setProperty;
        property keys : TStrings read configKeys;
        property history : TStringList read _history;
        property pattern : string read getPatternProp write setFolderPattern;
    end;

    var
        config : TCluesConfig;

implementation

uses
    typinfo, sysutils, ioutils, system.types, json.types, json.readers, json.writers, regularexpressions, math;

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

    function TCluesConfig.configKeys: TStrings;
    var
        key : string;
        ts : TStringList;
    begin
        ts := TStringList.Create;
        for key in _config.Keys do
            ts.Add(key);

        result := ts;
    end;

    constructor TCluesConfig.Create(configname : string);
    begin
        _config := TDictionary<string, string>.Create;
        _history := TStringList.Create;
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
        Result := IncludeTrailingPathDelimiter(_basePath) + getPattern(_folderPattern, _startNum);
    end;

	function TCluesConfig.getSourcePath: string;
    begin
        result := ExpandFileName(_config.Items['CluesOutputFolder']);
    end;

    destructor TCluesConfig.Destroy;
    begin
        save;
        _config.Free;

        inherited;
    end;

    function TCluesConfig.getProperty(key : string) : string;
    begin
        if _config.ContainsKey(key) then
            getProperty := _config[key]
        else
            getProperty := '';
    end;

    procedure TCluesConfig.setProperty(key: string; const Value: string);
    begin
        // don't allow setting of empty values
        if length(Value) = 0 then
            exit;

        if key = 'CluesOutputFolder' then _sourcePath := Value;
        if key = 'BaseDestinationFolder' then _basePath := Value;
        if key = 'SubfolderPattern' then _folderPattern := Value;

        if _config.ContainsKey(key) then begin
            if _config[key] <> value then begin
                _config[key] := Value;
                _changed := true;
            end;
        end
        else begin
            _config.Add(key, Value);
            _changed := true;
        end;
    end;

    function TCluesConfig.getStartNumAsString : string;
    begin
		getStartNumAsString := intToStr(_startNum);
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
                    _startNum := getLastDir + 1;
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
        item['SubfolderPattern'] := _folderPattern;
		_startNum := getLastDir + 1;            // recalc startnum, update folder list.
    end;

    function TCluesConfig.getPattern(pattern : string; start : integer) : string;
    var
        regex : TRegEx;
        match : TMatch;
        namePattern, numPattern : string;
        len : integer;
    begin
        regex := TRegEx.Create('([a-zA-Z_]*)([0]{0,9})');
        match := regex.Match(pattern);

        namePattern := '';
        len := 5;
        if match.Success then begin
            namePattern := match.Groups.Item[1].Value;
            numPattern := match.Groups.Item[2].Value;
            if length(pattern) > 0 then
                len := length(numPattern);
        end;

        if start > 0 then
            namePattern := namePattern + Format('%.*d', [len, start])
        else
            namePattern := namePattern + '00123456789'.substring(0, len);

        getPattern := namePattern;
    end;

    function TCluesConfig.getPatternProp: string;
    begin
        getPatternProp := _folderPattern;
    end;

// Get the highest scenarionumber from the folder names matching the folder pattern
    // The path should exist!
	function TCluesConfig.getLastDir : integer;
    var
        filter : TScenarioFolderFilter;
        max, value : integer;
        i : integer;
        folders : TStringDynArray;
    begin
		if length(_folderPattern) = 0 then
			getLastDir := 1;

        if _history.Count > 0 then _history.Clear;

        folders := TDirectory.getDirectories(_basePath);
		filter := TScenarioFolderFilter.Create(_folderPattern);
        max := 0;
        for i := 0 to length(folders) - 1 do begin
            if filter.accept(folders[i]) then begin
                value := filter.value;
                _history.Add(ExpandFileName(folders[i]));
                if value > max then
                    max := value;
            end;
        end;
        _history.Sort;     // A to Z

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
        writer.Formatting := TJsonFormatting.Indented;
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

    end;

	procedure TCluesConfig.nextFolder;
    begin
		inc(_startNum);
    end;

initialization

finalization
    config.Free;

end.
