program drover;

uses
  Vcl.Forms,
  Main in 'Main.pas' {frmMain},
  Options in '..\Options.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Drover';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
