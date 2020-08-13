unit AscFile;

interface

uses Classes, ClueTypes;

type
    TStats = record
        isValid : boolean;
        low : integer;
        high : integer;

        constructor Create(valid : boolean);
    end;

    TAscFile = class
        private
            _name : string;
            _rows : integer;
            _cols : integer;
            _left_X : double;
            _bottom_Y : double;
            _isCorner : boolean;
            _cellSize : double;
            _unknown : integer;

            _buffer : TMatrix<integer>;

            _countMetaData : integer;
            _last : integer;
            _c_pos : integer;
            _cache : TVector<integer>;
            _stats : TStats;

        function getStats: TStats;

    protected
        function nextMetaField(var ascfile : TextFile) : string;
        procedure loadMetadata(var ascfile : TextFile);
        function next(var ascfile : TextFile) : integer;

    public
        constructor Create;
        function isCorner : boolean;
        function line(row : integer) : TVector<integer>;
        function isNodata(row, col : integer) : boolean;
        function map : TMatrix<integer>;
        procedure load;

        property filename : string read _name write _name;
        property rows : integer read _rows;
        property cols : integer read _cols;
        property bottomY : double read _bottom_Y;
        property leftX : double read _left_X;
        property pixelIsArea : boolean read isCorner;
        property cellsize : double read _cellSize;
        property nodata : integer read _unknown;
        property stats : TStats read getStats;
    end;

implementation

uses sysutils, vcl.dialogs, math;

constructor TAscFile.Create;
begin
    _buffer := nil;
    _last := 0;
    _c_pos := 0;
    _unknown  := -MaxInt;
    _stats := TStats.Create(false);

end;

function TAscFile.getStats: TStats;
var
    r, c,
    l, h, v : integer;
begin
    if _buffer = nil then
        exit;

    l := +maxint;
    h := -maxint;
    for r := 0 to _rows - 1 do begin
         for c := 0 to _cols - 1 do begin
            v := _buffer[r][c];
            l := Math.min(l, v);
            h := Math.max(h, v);
         end;
    end;

    _stats.isValid := true;
    _stats.low := l;
    _stats.high := h;

    getStats := _stats;
end;

function TAscFile.isCorner : boolean;
begin
    result := not _isCorner;
end;

function TAscFile.isNodata(row, col: integer): boolean;
begin
    if _buffer = nil then
        isNodata := true;

    isNoData := _buffer[row][col] = _unknown;
end;

function TAscFile.Line(row : integer) : TVector<integer>;
begin
    if (row >= 0) and (row < rows) then
        result := _buffer[row];

    result := nil;
end;

function TAscFile.map : TMatrix<integer>;
begin
    if _buffer = nil then
        load();
    result := _buffer;
end;

function TAscFile.nextMetaField(var ascfile : TextFile) : string;
var line : string;
    parts : TStringList;
begin
    line := '';
    if not Eof(ascfile) then begin
        Readln(ascfile, line);
    end;

    parts := TStringList.Create;
    parts.CommaText := line;
    _isCorner := parts[0] = 'center';

    result := '';
    if parts.Count = 2 then
        result := parts[1];

    parts.Free;

end;

procedure TAscFile.loadMetadata(var ascfile : TextFile);
var
    unk : string;
    l : integer;
begin
    _cols := StrToInt(nextMetaField(ascfile));
    _rows := StrToInt(nextMetaField(ascfile));
    _left_X := StrToFloat(nextMetaField(ascfile));
    _bottom_Y := StrToFloat(nextMetaField(ascfile));
    _cellSize := StrToFloat(nextMetaField(ascfile));

    // now try optional stuff
    unk := nextMetaField(ascfile);
    _countMetaData := 5;
    if length(unk) > 0 then begin
        _countMetaData := 6;
        _unknown := StrToInt(unk);
    end;

    reset(ascfile);
    for l := 0 to _countMetaData - 1 do
        readln(ascfile);
	// now we are at the start of the data section

end;

function TAscFile.next(var ascfile : TextFile) : integer;
var
    parts : TStringList;
    line : string;
    v : integer;
begin
    if _c_pos >= _last then begin
        readln(ascfile, line);
        parts := TStringList.Create;
        parts.CommaText := line;
        if parts.Count > length(_cache) then
            // assumption: only happens the first time as cache is still unallocated
            // assumption 2: all lines contain the same number of values (except maybe the last)
            SetLength(_cache, parts.Count);
        _c_pos := 0;
        _last := parts.Count;
        for v := 0 to parts.Count - 1 do
            _cache[v] := StrToInt(parts[v]);
    end;

    result :=  _cache[_c_pos];
    inc(_c_pos);

end;

procedure TAscFile.load;
var ascfile : TextFile;
    row, col : integer;
begin
    try
        Assign(ascfile, self._name);
        Reset(ascfile);
        try
            loadMetadata(ascfile);
            SetLength(_buffer, _rows, _cols);
            for row := 0 to _rows - 1 do
                for col := 0 to _cols - 1 do
                    _buffer[row][col] := next(ascfile);

        except
            on EConvertError do
              ShowMessage('Integer number expected: does the ASC file contain floats?');
        end;
    finally
        Close(ascfile);
    end;

end;


{ TStats }

constructor TStats.Create(valid : boolean);
begin
    isValid := valid;
end;

end.
