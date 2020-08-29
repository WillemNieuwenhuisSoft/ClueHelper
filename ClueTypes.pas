unit ClueTypes;

interface

uses windows, winapi.messages;

const
    UM_WORKERPROGRESS = WM_APP + 1;
    UM_WORKERDONE = WM_APP + 2;

type
    TVector<T> = array of T;
    TMatrix<T> = array of TVector<T>;


type
    TUMWorkerDone = packed record
        Msg: Cardinal;
        ThreadHandle: Integer;
        unused: Integer;
        Result: LRESULT;
    end;

    TUMWorkerProgress = packed record
        Msg: Cardinal;
        ThreadHandle: Integer;
        progress: Integer;
        Result: LRESULT;
    end;


implementation

end.
