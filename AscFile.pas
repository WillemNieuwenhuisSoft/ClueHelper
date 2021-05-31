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
            _stats: TStats;
            _parts: TStringList;
            function getStats: TStats;

    protected
        function ReadLine(var Stream: TBufferedFileStream; var line: string): boolean;
        function nextMetaField(var ascfile : TBufferedFileStream) : string;
        procedure loadMetadata(var ascfile : TBufferedFileStream);
        function nextRow(var ascfile : TBufferedFileStream) : TVector<integer>;
    public
        constructor Create;
        destructor Destroy; override;
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

uses
    sysutils, vcl.dialogs, math;

constructor TAscFile.Create;
begin
    _buffer := nil;
    _last := 0;
    _c_pos := 0;
    _unknown  := -MaxInt;
    _stats := TStats.Create(false);
    _parts := TStringList.Create;

end;

destructor TAscFile.Destroy;
begin
    _parts.Free;

    inherited;
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

function TAscFile.ReadLine(var Stream: TBufferedFileStream; var line: string): boolean;
var
    RawLine: UTF8String;
    ch: AnsiChar;
begin
    result := false;
    ch := #0;
    while (Stream.Read(ch, 1) = 1) and (ch <> #13) do
    begin
        result := true;
        RawLine := RawLine + UTF8String(ch);
    end;
    line := string(RawLine);
    if ch = #13 then
    begin
        result := true;
        if (Stream.Read(ch, 1) = 1) and (ch <> #10) then
            Stream.Seek(-1, soCurrent) // unread it if not LF character.
    end
end;

function TAscFile.nextMetaField(var ascfile : TBufferedFileStream) : string;
var
    line : string;
    position : int64;
    num : double;
begin
    position := ascfile.Position;

    line := '';
    ReadLine(ascfile, line);

    _parts.CommaText := line;
    _isCorner := _parts[0] = 'center';

    result := '';
    if _parts.Count = 2 then
        result := _parts[1]
    else begin
        if TryStrToFloat(_parts[0], num) then
            ascfile.Position := position;   // reset file pointer if not metadata field
    end;
end;

procedure TAscFile.loadMetadata(var ascfile : TBufferedFileStream);
var
    unk : string;
    format : TFormatSettings;
begin
    format.DecimalSeparator := '.';
    format.ThousandSeparator := ',';
    _cols := StrToInt(nextMetaField(ascfile));
    _rows := StrToInt(nextMetaField(ascfile));
    _left_X := StrToFloat(nextMetaField(ascfile), format);
    _bottom_Y := StrToFloat(nextMetaField(ascfile), format);
    _cellSize := StrToFloat(nextMetaField(ascfile), format);

    // now try optional stuff
    unk := nextMetaField(ascfile);
    _countMetaData := 5;
    if length(unk) > 0 then begin
        _countMetaData := 6;
        _unknown := StrToInt(unk);
    end;

end;

function TAscFile.nextRow(var ascfile: TBufferedFileStream): TVector<integer>;
var
    RawLine: UTF8String;
    ch: AnsiChar;
    values : TVector<integer>;
    strval : string;
    col : integer;
    eof : boolean;
begin
    setLength(values, _cols);
    ch := #0;
    col := 0;
    eof := false;
    while (not eof) and (col < _cols) do begin
        eof := ascfile.Read(ch, 1) = 0;

        // skip white space
        while (not eof) and ( (ch = #13) or (ch = #10) or (ch = ' ') or (ch = #9)) do
            eof := ascfile.Read(ch, 1) = 0;

        // either EOF or ch is non-whitespace
        while (not eof) and (ch <> #13) and (ch <> #10) and (ch <> ' ') and (ch <> #9) do
        begin
            RawLine := RawLine + UTF8String(ch);
            eof := ascfile.Read(ch, 1) = 0;
        end;

        // either EOF of ch is whitespace
        // convert found string to value
        strval := string(RawLine);
        RawLine := '';
        values[col] := StrToIntDef(strval, _unknown);

        inc(col);

    end;

    result := values;
end;


procedure TAscFile.load;
var
    row : integer;
    ascfile : TBufferedFileStream;
begin
    ascfile := TBufferedFileStream.Create(_name, fmOpenRead);

    try
        try
            loadMetadata(ascfile);
            SetLength(_buffer, _rows);
            for row := 0 to _rows - 1 do
                _buffer[row] := nextRow(ascfile);

        except
            on EConvertError do begin
              ShowMessage('Integer number expected: does the ASC file contain floats?');
            end;
        end;
    finally
        ascfile.Free;
    end;

end;


{ TStats }

constructor TStats.Create(valid : boolean);
begin
    isValid := valid;
end;

end.
