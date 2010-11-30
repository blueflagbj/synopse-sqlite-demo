unit uCustomer;

interface

uses
  SynCommons,
  SQLite3Commons;

type
  TTasks = class;

  TPerson = class(TSQLRecord)
  private
    fName, fSurname: RawUTF8;
  published
    property Name: RawUTF8 read fName write fName;
    property Surname: RawUTF8 read fSurname write fSurname;
  end;

  TCustomer = class(TPerson)
  private
    fTasks: TTasks;
  published
    property Name;
    property Surname;
    property Tasks: TTasks read fTasks;
  end;

  TTaskPriority = (tpLow, tpNormal, tpHigh);
  TTask = class(TSQLRecord)
  private
    fPriority: TTaskPriority;
    fText: RawUTF8;
  published
    property Priority: TTaskPriority read fPriority write fPriority;
    property Text: RawUTF8 read fText write fText;
  end;

  TTasks = class(TSQLRecordMany)
  published
    property Source: TRecordReference read fSource;
    property Dest: TRecordReference read fDest;
  end;

function CreateSampleModel: TSQLModel;


implementation

function CreateSampleModel: TSQLModel;
begin
  result := TSQLModel.Create([TCustomer, TTask, TTasks]);
end;


end.
