program Project1;

uses
  FastMM4,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uCustomer in 'uCustomer.pas';
  
const
  AppName = 'Synopse SQLite3 Demo';

{$R *.res}
begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);  
  Form1.Caption:= AppName;
  Form1.Label4.Caption:= AppName;
  Application.Run;
end.
