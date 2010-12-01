(*
  Simple Almost-Real-Life example of SynopseSQLite3 Framework.
  Written by Michal Gajek, http://migajek.com/

  The demo doesn't depend on any 3rd party libraries nor components,
  however it is highly recommended to work with "SQLITEDEMO_USE_FASTMM4" condition ON,
  together with EnableMemoryLeaks in FastMM in order to keep the code leak-free.

  ToDO:
    [ ] in each ListBox / ComboBox "Object", instead of ID, keep RecordRef ?
*)
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, CheckLst, Grids,
  
  SynCommons, SQLite3Commons, SQLite3, SQLite3UI,

  uCustomer;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lbCustomers: TListBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblName: TLabel;
    lblSurname: TLabel;
    btnAddCustomer: TButton;
    lbTasks: TListBox;
    btnNewTask: TButton;
    cbCustomers: TComboBox;
    lbCustomerTasks: TListBox;
    Label3: TLabel;
    gbEditTask: TGroupBox;
    cbTaskPriority: TComboBox;
    CheckListBox1: TCheckListBox;
    TabSheet3: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    TabSheet4: TTabSheet;
    dgTable: TDrawGrid;
    edtQuery: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure btnAddCustomerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbCustomersClick(Sender: TObject);
    procedure btnNewTaskClick(Sender: TObject);
    procedure cbCustomersClick(Sender: TObject);
    procedure lbTasksClick(Sender: TObject);
    procedure cbTaskPriorityChange(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure edtQueryKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function LoadCustomer(const ACustomerID: integer): TCustomer;
    procedure DisplayCustomerInfo(const ACustomer: TCustomer); overload;
    procedure DisplayCustomerInfo(const ACustomerID: integer); overload;
    procedure FillCustomersList(const AList: TStrings; const AClear: boolean = true);

    function LoadTask(const ATaskID: integer): TTask;
    procedure FillTasksList(const AList: TStrings; ATasks: TTask);
    procedure LoadTasksForCustomer(const ACustomer: TCustomer; const AList: TStrings);

    procedure LoadQueryHistory();
  public
    { Public declarations }
    Database: TSQLRestClientDB;
    Model: TSQLModel;
  end;

var
  Form1: TForm1;

implementation
uses ShellApi, uQueryHistory;
{$R *.dfm}

// loads customer data into "Details" box. Pass nil in order to clear the box.
procedure TForm1.DisplayCustomerInfo(const ACustomer: TCustomer);
begin
  if ACustomer <> nil then
    begin
      lblName.Caption:= ACustomer.Name;
      lblSurname.Caption:= ACustomer.Surname;
      LoadTasksForCustomer(ACustomer, lbCustomerTasks.Items);
    end
  else
    begin
      lblName.Caption:= '';
      lblSurname.Caption:= '';
    end;
end;

procedure TForm1.DisplayCustomerInfo(const ACustomerID: integer);
var
  cust: TCustomer;
begin
  cust:= LoadCustomer(ACustomerID);
  try
    DisplayCustomerInfo(cust);
  finally
    FreeAndNil(cust);
  end;
end;

function TForm1.LoadCustomer(const ACustomerID: integer): TCustomer;
begin
   result:= TCustomer.Create(Database, ACustomerID);
end;

// loads a list of customers to a AList
procedure TForm1.FillCustomersList(const AList: TStrings; const AClear: boolean = true);
var
 data: TSQLTable;
 cust: TCustomer;
begin
  // load all the customers
  data:= Database.MultiFieldValues(TCustomer, '');

  try
    AList.BeginUpdate();
    if AClear then
      AList.Clear();
    cust:= TCustomer.Create();
    cust.FillPrepare(data);
    while cust.FillOne do
      AList.AddObject(Format('%s, %s', [cust.Surname, cust.Name]), Pointer(cust.ID)); // we keep integer ID as "Data" object

  finally
    AList.EndUpdate();
    FreeAndNil(data);
    FreeAndNil(cust);
  end;

end;

procedure TForm1.FillTasksList(const AList: TStrings; ATasks: TTask);
var
  freeAfter: boolean;
  data: TSQLTable;
begin
  freeAfter:= ATasks = nil;
  if freeAfter then
    begin
      ATasks:= TTask.Create();
      data:= Database.MultiFieldValues(TTask, '');
      ATasks.FillPrepare(data);
    end;

  try
    AList.BeginUpdate();
    AList.Clear();
    while ATasks.FillOne do
      AList.AddObject(Format('%s', [ATasks.Text]), Pointer(ATasks.ID));
  finally
    AList.EndUpdate();
    if freeAfter then
      begin
        FreeAndNil(ATasks);
        FreeAndNil(data);
      end;
  end;

end;

procedure TForm1.LoadTasksForCustomer(const ACustomer: TCustomer; const AList: TStrings);
var
 task: TTask;
begin
  ACustomer.Tasks.FillMany(Database, ACustomer.ID);
  AList.BeginUpdate();
  AList.Clear();
  try
    while ACustomer.Tasks.FillOne do
      begin
        task:= TTask(Database.Retrieve(ACustomer.Tasks.Dest));
        AList.AddObject(Format('%s', [task.Text]), Pointer(task.id));
        FreeAndNil(task);
      end;
  finally
    AList.EndUpdate();
  end;
end;

function TForm1.LoadTask(const ATaskID: integer): TTask;
begin
  result:= TTask.Create(Database, ATaskID);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
Model:= CreateSampleModel;
Database:= TSQLRestClientDB.Create(Model, CreateSampleModel, ChangeFileExt(Application.ExeName,'.db3'), TSQLRestServerDB);
Database.Server.CreateMissingTables(0);

// clear all the fields.
DisplayCustomerInfo(nil);
FillCustomersList(lbCustomers.Items);
cbCustomers.AddItem('<Any customer>', nil);
cbCustomers.ItemIndex:= 0;
FillCustomersList(cbCustomers.Items, false);
cbCustomersClick(nil);

LoadQueryHistory();
end;

procedure TForm1.btnAddCustomerClick(Sender: TObject);
var
  cust: TCustomer;
begin

  cust:= TCustomer.Create;
  try
    cust.Name:= InputBox('Name', 'Customer name', 'John');
    cust.Surname:= InputBox('Surname', 'Customer surname', 'Doe');
    if (cust.Name <> '') and (cust.Surname <> '') then
      Database.Add(cust, true);
  finally
    FreeAndNil(cust);
    FillCustomersList(lbCustomers.Items);
  end;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Database.Free;
  Model.Free;
end;

procedure TForm1.lbCustomersClick(Sender: TObject);
var
  cust: TCustomer;
begin
  if lbCustomers.ItemIndex <> -1 then
    begin
      // since we store record ID as object, now we can load it
      if lbCustomers.Items.Objects[lbCustomers.ItemIndex] <> nil then
        begin
          cust:= LoadCustomer( Integer(lbCustomers.Items.Objects[lbCustomers.ItemIndex]) );
          DisplayCustomerInfo(cust);
          FreeAndNil(cust);
        end
    end
  else
    DisplayCustomerInfo(nil);

end;

procedure TForm1.btnNewTaskClick(Sender: TObject);
var
  task: TTask;
begin

  task:= TTask.Create();
  try
    task.Text:= InputBox('Text', 'Task description', '');
    task.Priority:= tpNormal;
    if (task.Text <> '') then
      Database.Add(task, true);
  finally
    FreeAndNil(task);
  end;

end;

procedure TForm1.cbCustomersClick(Sender: TObject);
var
  cust: TCustomer;
begin
gbEditTask.Visible:= false;
  if cbCustomers.ItemIndex <> -1 then
    begin
      // since we store record ID as object, now we can load it
      if cbCustomers.Items.Objects[cbCustomers.ItemIndex] <> nil then
        begin
          cust:= LoadCustomer( Integer(cbCustomers.Items.Objects[cbCustomers.ItemIndex]) );
          LoadTasksForCustomer(cust, lbTasks.Items);
          FreeAndNil(cust);
        end
      else
        FillTasksList(lbTasks.Items, nil);
    end
  else
    FillTasksList(lbTasks.Items, nil);
end;

procedure TForm1.lbTasksClick(Sender: TObject);
var
  task: TTask;
  cust: TCustomer;
  clientsIds: TIntegerDynArray;
  i, j: integer;
begin
  gbEditTask.Visible:= lbTasks.ItemIndex <> -1;
  if not gbEditTask.Visible then
    exit;

  gbEditTask.Visible:= lbTasks.Items.Objects[lbTasks.ItemIndex] <> nil;
  if not gbEditTask.Visible then
    exit;

  task:= LoadTask(integer(lbTasks.Items.Objects[lbTasks.ItemIndex]));
  cbTaskPriority.ItemIndex:= Ord(task.Priority);
  cbTaskPriority.Tag:= task.ID;

  FillCustomersList(CheckListBox1.Items, true);
  CheckListBox1.Tag:= task.ID;
  // load list of customers assigned to the given task
  cust:= TCustomer.Create();
  try
    cust.Tasks.SourceGet(Database, task.ID, clientsIds);
    for i:= low(clientsIds) to high(clientsIds) do
      begin

        // find the client on the list (by ID)
        for j:= 0 to CheckListBox1.Count -1 do
          if Integer(CheckListBox1.Items.Objects[j]) = clientsIds[i] then
              CheckListBox1.Checked[j]:= true;
      end;
  finally
    cust.Free();
    FreeAndNil(task);
  end;
end;

procedure TForm1.cbTaskPriorityChange(Sender: TObject);
var
 task: TTask;
begin
  if cbTaskPriority.Tag > 0 then
    begin
      task:= LoadTask(cbTaskPriority.Tag);
      if task <> nil then
        begin
          task.Priority:= TTaskPriority(cbTaskPriority.ItemIndex);
          Database.Update(task);
          task.Free;
        end;
    end
end;

procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
var
 task: TTask;
 cust: TCustomer;
begin
  // first, load the Task based on ID (stored in TAG property)
  if (sender as TComponent).Tag > 0 then
    begin
      task:= LoadTask((sender as TComponent).Tag);
      if task <> nil then
        begin

          // now load the customer from the list
          if CheckListBox1.ItemIndex > - 1 then
            begin
              cust:= LoadCustomer(Integer(CheckListBox1.Items.Objects[CheckListBox1.ItemIndex]));
              if cust <> nil then
                begin
                  if CheckListBox1.Checked[CheckListBox1.ItemIndex] then
                    cust.Tasks.ManyAdd(Database, cust.ID, task.ID, true)
                  else
                    cust.Tasks.ManyDelete(Database, cust.ID, task.ID);
                    
                  FreeAndNil(cust);
                end;
            end;
          FreeAndNil(task);
        end;
    end

end;

procedure TForm1.Label7Click(Sender: TObject);
begin
ShellExecute(0, 'open', 'http://code.google.com/p/synopse-sqlite-demo/', '', '',  SW_SHOWNORMAL);
end;

procedure TForm1.edtQueryKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 data: TSQLTable;
 hist: TQueryHistory;
begin
if Key = VK_RETURN then
  begin
    Key:= 0;

    // we don't have to worry for freeing the data nor the "previously" created instance of TSQLTableToGrid
    // as the Framework takes care of everything.
    data:= Database.ExecuteList([TCustomer, TTask, TTasks], edtQuery.Text);
    TSQLTableToGrid.Create(dgTable, data, Database);

    hist:= TQueryHistory.Create(Database, 'SQL = "%"', [edtQuery.Text]);
    try
      if hist.ID = 0 then
        hist.SQL:= edtQuery.Text;
      hist.LastUsed:= Now();
      if hist.ID > 0 then
        Database.Update(hist)
      else
        Database.Add(hist, true);
    finally
      FreeAndNil(hist);
      LoadQueryHistory();
    end;
    
  end;
end;

procedure TForm1.LoadQueryHistory();
var
 hist: TQueryHistory;
begin
  hist:= TQueryHistory.Create();
  edtQuery.Items.BeginUpdate();
  edtQuery.Items.Clear();
  try
    hist.FillHistory(Database);
    while hist.FillOne do
      edtQuery.AddItem(hist.SQL, nil);
  finally
    edtQuery.Items.EndUpdate();
    FreeAndNil(hist);
  end;
end;

end.
