program Project1;

uses
  FastMM4,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uCustomer in 'uCustomer.pas';

{$R *.res}
const
  AppName = 'Synopse SQLite3 Demo';
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Form1.Caption:= AppName;
  Form1.Label4.Caption:= AppName;
  Application.Title:= AppName;
  Application.Run;
end.
