unit uQueryHistory;

interface
uses
  SynCommons,
  SQLite3Commons;

type
  TQueryHistory = class(TSQLRecord)
  private
    fSQL: RawUTF8;
    fLastUsed: TDateTime;
    fData: TSQLTable;
  published
    property SQL: RawUTF8 read fSQL write fSQL;
    property LastUsed: TDateTime read fLastUsed write fLastUsed;
  public
    procedure FillHistory(const aClient: TSQLRest);
  protected
    destructor Destroy(); override;
  end;

implementation
uses SysUtils;

procedure TQueryHistory.FillHistory(const aClient: TSQLRest);
begin
  fData:= aClient.MultiFieldValues(RecordClass, '');
  fData.SortFields(fData.FieldIndex('LastUsed'), false, nil, sftDateTime);
  FillPrepare(fData);
end;


destructor TQueryHistory.Destroy();
begin
  FreeAndNil(fData);
  inherited;
end;

end.
