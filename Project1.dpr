// JCL_DEBUG_EXPERT_GENERATEJDBG ON
// JCL_DEBUG_EXPERT_DELETEMAPFILE ON
// JCL_DEBUG_EXPERT_INSERTJDBG ON
program Project1;

uses
  {$IFDEF SQLITEDEMO_USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uCustomer in 'uCustomer.pas',
  uQueryHistory in 'uQueryHistory.pas';

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
