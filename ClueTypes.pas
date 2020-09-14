unit ClueTypes;

interface

uses
    windows, winapi.messages,
    Vcl.ComCtrls;

const
    UM_WORKERPROGRESS = WM_APP + 1;
    UM_WORKERDONE = WM_APP + 2;

type
    TVector<T> = array of T;
    TMatrix<T> = array of TVector<T>;


type
    TUMWorkerDone = record
        Msg: Cardinal;
        ThreadHandle: Integer;
        unused: Integer;
        Result: LRESULT;
    end;

    TUMWorkerProgress = record
        Msg: Cardinal;
        ThreadHandle: integer;
        total: integer;
        progress: LRESULT;
    end;

    TProgressBarWithText = class(TProgressBar)
    private
        FProgressText: string;
        procedure setProgressText(const Value: string);
    protected
        procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

//    public
//        constructor Create(owner : TComponent);
    published
        property ProgressText: string read FProgressText write setProgressText;
    end;

implementation

procedure TProgressBarWithText.setProgressText(const Value: string);
begin
    FProgressText := Value;
    Invalidate;
end;

procedure TProgressBarWithText.WMPaint(var Message: TWMPaint);
var
    DC: HDC;
    prevfont: HGDIOBJ;
    prevbkmode: Integer;
    R: TRect;
begin
    inherited;

    if ProgressText <> '' then
    begin
        R := ClientRect;
        DC := GetWindowDC(Handle);
        prevbkmode := SetBkMode(DC, TRANSPARENT);
        prevfont := SelectObject(DC, Font.Handle);
        DrawText(DC, PChar(ProgressText), Length(ProgressText), R,
          DT_SINGLELINE or DT_CENTER or DT_VCENTER);
        SelectObject(DC, prevfont);
        SetBkMode(DC, prevbkmode);
        ReleaseDC(Handle, DC);
    end;
end;

end.
