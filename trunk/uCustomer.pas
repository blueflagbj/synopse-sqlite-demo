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


  TUserRole = class(TSQLRecord)
  private
    fRoleName: RawUTF8;
  public
    // create standard roles: admin & user
    class procedure CreateStandardRoles(const ADatabase: TSQLRest);
  published
    property RoleName: RawUTF8 read fRoleName write fRoleName;
  end;

  TUserRoles = class(TSQLRecordMany)
  private
    fValidUntil: TTimeLog;
  public
    function GetRole(const ADatabase: TSQLRest): TUserRole;
  published
    property ValidUntil: TTimeLog read fValidUntil write fValidUntil;
    property Dest: TRecordReference read fDest;
    property Source: TRecordReference read fSource;
  end;

  // user of our system
  TUser = class(TPerson)
  private
    fRoles: TUserRoles;
    fLogin, fPassword: RawUTF8;
  public
    function HasRole(const AClient: TSQLRest; const ARoleName: RawUTF8; const ARoleID: integer = -1): boolean; overload;
    function HasRole(const AClient: TSQLRest; const ARoleName: RawUTF8; const ARoleID: integer; var ARowID: integer): boolean; overload;
  published
    property Roles: TUserRoles read fRoles write fRoles;
    property Name;
    property Surname;
    property Login: RawUTF8 read fLogin write fLogin;
    property Password: RawUTF8 read fPassword write fPassword; // set MD5 here by SetPassword?
  end;
function CreateSampleModel: TSQLModel;


implementation
uses SysUtils, uQueryHistory;

function CreateSampleModel: TSQLModel;
begin
  result := TSQLModel.Create([TCustomer, TTask, TTasks, TQueryHistory, TUser, TUserRole, TUserRoles]);
end;


class procedure TUserRole.CreateStandardRoles(const ADatabase: TSQLRest);
const
  names: array [0..1] of string = ( 'user', 'admin' );
var
  i: integer;
  role: TUserRole;
begin
  for i:= low(names) to high(names) do
    begin
      role:= TUserRole.Create(ADatabase, 'RoleName = "%"', [names[i]]);
      try
        // if the role isn't present yet
        if role.ID = 0 then
          begin
            role.RoleName:= names[i];
            ADatabase.Add(role, true);
          end
      finally
        FreeAndNil(role);
      end;
    end;
end;

function TUserRoles.GetRole(const ADatabase: TSQLRest): TUserRole;
begin
  result:= TUserRole(ADatabase.Retrieve(fDest));
end;


function TUser.HasRole(const AClient: TSQLRest; const ARoleName: RawUTF8; const ARoleID: integer = -1): boolean;
var
  dummy: integer;
begin
  result:= HasRole(AClient, ARoleName, ARoleID, dummy);
end;

function TUser.HasRole(const AClient: TSQLRest; const ARoleName: RawUTF8; const ARoleID: integer; var ARowID: integer): boolean;
var
  role: TUserRole;
begin
  result:= false;
  ARowID:= -1;
  fRoles.FillMany(AClient, fID);

  while fRoles.FillOne do
    begin
      role:= fRoles.GetRole(AClient);
      if role <> nil then
        try
          if ((ARoleID = -1) and (role.RoleName = ARoleName)) or ((ARoleID > -1) and (role.ID = ARoleID) ) then
            begin
              result:= true;
              ARowID:= fRoles.ID;
              break;
            end;
        finally
          FreeAndNil(role);
        end;
    end;
end;

end.
