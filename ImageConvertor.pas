unit ImageConvertor;

interface

uses
    classes;

type
    TImageConvertor = class(TThread)
        _owner : THandle;

    protected
        procedure Execute; override;

    public
        constructor Create(window : THandle);

        procedure convertAll;
    end;

implementation

uses system.types, sysutils, ioutils, windows,
     cluetypes, AscToIlwis, CluesConfig, movescenariofiles;

{ TImageConvertor }

procedure TImageConvertor.convertAll;
begin
    Execute;
end;

constructor TImageConvertor.Create(window: THandle);
begin
    inherited Create;

    _owner := window;
end;

procedure TImageConvertor.Execute;
var
    mv : TMoveScenarioFiles;
    a2i : TAscToIlwis;
    i : integer;
    folder : string;
    files : TStringDynArray;
    status : integer;
begin
    inherited;

    try
        status := 1 ; // not moved and not converted
        // move generated files
        mv := TMoveScenarioFiles.Create;
        if not mv.moveScenarioFiles then begin
            status := mv.reason;    // error code
            exit;
        end;

        // copy ilwis service files
        mv.copyServiceObjects;

        // convert the move generated files
        a2i := TAscToIlwis.Create;
        folder := config.getScenarioFolder;
        files := TDirectory.GetFiles(folder);
        for i := 0 to length(files) - 1 do begin
            if ExtractFileExt(files[i]) = '.asc' then begin

                PostMessage(_owner, UM_WORKERPROGRESS, length(files), i + 1);
                a2i.ascName := files[i];
                a2i.ilwisName := ChangeFileExt(files[i], '.mpr');
                a2i.convert;
            end;
        end;
        status := 2; // files moved and converted.
    finally
        PostMessage(_owner, UM_WORKERDONE, 0, status);

        mv.Free;
    end;

end;


end.
