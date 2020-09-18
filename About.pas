unit About;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.jpeg, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TaboutForm = class(TForm)
    aboutImage: TImage;
    aboutMemo: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure closeAbout(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  aboutForm: TaboutForm;

implementation

{$R *.dfm}

procedure TaboutForm.closeAbout(Sender: TObject);
begin
    Close;
end;

procedure TaboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action  := caFree;
end;

end.
