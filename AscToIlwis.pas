unit AscToIlwis;

interface

type
    TAscToIlwis = class
        _ascname : string;
        _ilwisname : string;

    public
        procedure convert;

        property ascName : string read _ascname write _ascname;
        property ilwisName : string read _ilwisname write _ilwisname;
    end;

implementation

uses
    AscFile, IlwisMap, ClueTypes, CluesConfig;

    procedure TAscToIlwis.convert;
    var
        asc : TAscFile;
        ilwis : TIlwisMap<byte>;
        ascbuf : TMatrix<integer>;
        ilwbuf : TMatrix<byte>;
        row , col : integer;
    begin
		asc := TAscFile.Create;
        asc.filename := _ascname;
		ilwis := TIlwisMap<byte>.Create;
        ilwis.filename := _ilwisname;

		ascbuf := asc.map;  // forces load of data;
        SetLength(ilwbuf, asc.rows, asc.cols);
        for row := 0 to asc.rows - 1 do
            for col := 0 to asc.cols - 1 do
                if asc.isNodata(row, col) then
                    ilwbuf[row][col] := ilwis.getNodata(0)
                else
                    ilwbuf[row][col] := byte( ascbuf[row][col]) + 1;

		ilwis.setMap(ilwbuf);
		ilwis.cols := asc.cols;
		ilwis.rows := asc.rows;
        // Domain and GeoRef need to be supplied by user
        // This metadata cannot be determined from the AscFile
		ilwis.georef := config.item['IlwisGeoref'];
		ilwis.domain := config.item['IlwisDomain'];

		ilwis.store();
    end;

end.
