unit IlwisMap;

interface

uses sysutils, classes, ClueTypes;

const
    bUNDEF : byte = 0;
    iUNDEF : integer = -MaxInt;

type
    TIlwisMap<T> = class
	    _name : string;
        _georef : string;
        _domain : string;

        _rows : integer;
        _cols : integer;

        _buffer : TMatrix<T>;

    private
        data : TFileStream;
        writer : TBinaryWriter;

        procedure storeElem(value : T); overload;
        procedure storeElem(value : byte); overload;
        procedure storeElem(value : integer); overload;

    public
        constructor Create;
        function map : TMatrix<T>;
        function pixel(row: integer; col : integer) : T;
        function line(row : integer) : TVector<T>;
        procedure setMap(values : TMatrix<T>);
        procedure store;
        procedure storeMetadata;

        function getNodata(v : T): T; overload;
        function getNodata(v : byte): byte; overload;
        function getNodata(v : integer): integer; overload;

        property filename : string read _name write _name;
        property rows : integer read _rows write _rows;
        property cols : integer read _cols write _cols;
        property georef : string read _georef write _georef;
        property domain : string read _domain write _domain;
//        property nodata : T read getNodata;

    end;

    TIlwisByteMap = TIlwisMap<byte>;

implementation

uses typinfo, inifiles;

{ TIlwisByteMap }

constructor TIlwisMap<T>.Create;
begin
    _buffer := nil;
end;

function TIlwisMap<T>.map: TMatrix<T>;
begin
    if _buffer = nil then
        ;  // do some loading ?

    map := _buffer;
end;

function TIlwisMap<T>.pixel(row: integer; col : integer) : T;
begin
    pixel := _buffer[row][col];
end;

function TIlwisMap<T>.getNodata(V : T): T;
//var
//    info    : PTypeInfo;
begin
//    info := System.TypeInfo(T);
//    case info^.Kind of
//        tkChar : getNodata := bUNDEF;
//        tkInteger : getNodata := iUNDEF;
//    end;
end;

function TIlwisMap<T>.getNodata(V : byte): byte;
begin
    getNodata := bUNDEF;
end;

function TIlwisMap<T>.getNodata(v : integer): integer;
begin
    getNodata := iUNDEF;
end;

function TIlwisMap<T>.line(row: integer): TVector<T>;
begin
    line := _buffer[row];
end;

procedure TIlwisMap<T>.setMap(values: TMatrix<T>);
begin
    _buffer := values;
end;

procedure TIlwisMap<T>.store;
var
    datafile : string;
    row, col : integer;
begin
    datafile := ChangeFileExt(_name, '.mp#');

    data := TFileStream.Create(datafile, fmOpenWrite or fmCreate);
    writer := TBinaryWriter.Create(data);
    try
        for row := 0 to _rows - 1 do
            for col := 0 to _cols - 1 do
                storeElem(_buffer[row][col]);
        writer.Close;
    finally
        writer.Free;
        data.Free;
    end;

    storeMetadata;

end;

procedure TIlwisMap<T>.storeElem(value: T);
begin
    // empty on purpose
end;

procedure TIlwisMap<T>.storeElem(value: byte);
begin
    writer.write(value);
end;

procedure TIlwisMap<T>.storeElem(value: integer);
begin
    writer.Write(value);
end;

procedure TIlwisMap<T>.storeMetadata;
var
    mprfile : TIniFile;
    mprname : string;
    datafile : string;
    grfname : string;
    domname : string;
    info    : PTypeInfo;
    typeName : string;
begin
    mprname := ChangeFileExt(_name, '.mpr');
    datafile := ChangeFileExt(_name, '.mp#');
    grfname := ChangeFileExt(_georef, '.grf');
    domname := ChangeFileExt(_domain, '.dom');
    mprfile := TIniFile.Create(mprname);
    mprfile.WriteString('BaseMap', 'Domain', ExtractFileName(domname));
    mprfile.WriteString('BaseMap', 'Type', 'Map');
    mprfile.WriteString('Ilwis', 'Type', 'BaseMap');
    mprfile.WriteString('Map', 'Type', 'MapStore');
    mprfile.WriteString('Map', 'GeoRef', ExtractFileName(grfName));
    mprfile.WriteString('Map', 'Size', format('%-4d %-4d', [rows, cols]));
    info := System.TypeInfo(T);
    typeName := string(info^.Name);
    if typeName = 'Integer' then
        typeName := 'Int';
    mprfile.WriteString('MapStore', 'Type', typeName);
    mprfile.WriteString('MapStore', 'Structure', 'Line');
    mprfile.WriteString('MapStore', 'Data', ExtractFileName(datafile));
    mprfile.Free;

end;

end.
