(*
  Duall Sistemas, Object Persistence SQLdb Broker

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dSQLdbBroker;

{$i dopf.inc}

interface

uses
  dOPF, Classes, SysUtils, SQLdb, DB;

type

  { TdSQLdbConnectionDef }

  TdSQLdbConnectionDef = class(TdConnectionDef)
  private
    FConn: TSQLConnector;
  protected
    function GetConnection: TObject; override;
    function GetTransaction: TObject; override;
    function GetConnected: Boolean; override;
    function GetDatabase: string; override;
    function GetDriver: string; override;
    function GetHost: string; override;
    function GetPassword: string; override;
    function GetPort: Integer; override;
    function GetUser: string; override;
    procedure SetConnected(const AValue: Boolean); override;
    procedure SetDatabase(const AValue: string); override;
    procedure SetDriver(const AValue: string); override;
    procedure SetHost(const AValue: string); override;
    procedure SetPassword(const AValue: string); override;
    procedure SetPort(const AValue: Integer); override;
    procedure SetUser(const AValue: string); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Connect: TdConnectionDef; override;
    function Disconnect: TdConnectionDef; override;
    function StartTransaction: TdConnectionDef; override;
    function Commit: TdConnectionDef; override;
    function CommitRetaining: TdConnectionDef; override;
    function Rollback: TdConnectionDef; override;
    function RollbackRetaining: TdConnectionDef; override;
    function InTransaction: Boolean; override;
  end;

  { TdSQLdbQueryDef }

  TdSQLdbQueryDef = class(TdQueryDef)
  private
    FConn: TdConnection;
    FQuery: TSQLQuery;
  protected
    function GetActive: Boolean; override;
    function GetBOF: Boolean; override;
    function GetConnection: TdConnection; override;
    function GetDataSet: TDataSet; override;
    function GetDataSource: TDataSource; override;
    function GetEOF: Boolean; override;
    function GetFieldDefs: TFieldDefs; override;
    function GetFields: TFields; override;
    function GetModified: Boolean; override;
    function GetParams: TParams; override;
    function GetPosition: Int64; override;
    function GetSQL: TStrings; override;
    function GetState: TDataSetState; override;
    procedure SetActive(const AValue: Boolean); override;
    procedure SetConnection(AValue: TdConnection); override;
    procedure SetDataSource(AValue: TDataSource); override;
    procedure SetPosition(const AValue: Int64); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ApplyUpdates: TdQueryDef; override;
    function CancelUpdates: TdQueryDef; override;
    function Apply: TdQueryDef; override;
    function ApplyRetaining: TdQueryDef; override;
    function Undo: TdQueryDef; override;
    function UndoRetaining: TdQueryDef; override;
    function Append: TdQueryDef; override;
    function Insert: TdQueryDef; override;
    function Edit: TdQueryDef; override;
    function Cancel: TdQueryDef; override;
    function Delete: TdQueryDef; override;
    function Open: TdQueryDef; override;
    function Close: TdQueryDef; override;
    function Refresh: TdQueryDef; override;
    function First: TdQueryDef; override;
    function Prior: TdQueryDef; override;
    function Next: TdQueryDef; override;
    function Last: TdQueryDef; override;
    function Post: TdQueryDef; override;
    function Execute: TdQueryDef; override;
    function RowsAffected: Int64; override;
    function Locate(const AKeyFields: string;const AKeyValues: Variant;
      const AOptions: TLocateOptions = []): Boolean; override;
    function Param(const AName: string): TParam; override;
    function Field(const AName: string): TField; override;
    function FieldDef(const AName: string): TFieldDef; override;
    function Count: Int64; override;
    function GetBookmark: TBookmark; override;
    procedure GotoBookmark(ABookmark: TBookmark); override;
  end;

  { TdSQLdbBroker }

  TdSQLdbBroker = class(TdBroker)
  public
    class function GetConnectionDefClass: TdConnectionDefClass; override;
    class function GetQueryDefClass: TdQueryDefClass; override;
  end;

implementation

{ TdSQLdbConnectionDef }

constructor TdSQLdbConnectionDef.Create;
begin
  inherited Create;
  FConn := TSQLConnector.Create(nil);
  FConn.Transaction := TSQLTransaction.Create(FConn);
end;

destructor TdSQLdbConnectionDef.Destroy;
begin
  FreeAndNil(FConn);
  inherited Destroy;
end;

function TdSQLdbConnectionDef.Connect: TdConnectionDef;
begin
  Result := Self;
  FConn.Connected := True;
end;

function TdSQLdbConnectionDef.Disconnect: TdConnectionDef;
begin
  Result := Self;
  FConn.Connected := False;
end;

function TdSQLdbConnectionDef.StartTransaction: TdConnectionDef;
begin
  Result := Self;
  FConn.Transaction.StartTransaction;
end;

function TdSQLdbConnectionDef.Commit: TdConnectionDef;
begin
  Result := Self;
  FConn.Transaction.Commit;
end;

function TdSQLdbConnectionDef.CommitRetaining: TdConnectionDef;
begin
  Result := Self;
  FConn.Transaction.CommitRetaining;
end;

function TdSQLdbConnectionDef.Rollback: TdConnectionDef;
begin
  Result := Self;
  FConn.Transaction.Rollback;
end;

function TdSQLdbConnectionDef.RollbackRetaining: TdConnectionDef;
begin
  Result := Self;
  FConn.Transaction.RollbackRetaining;
end;

function TdSQLdbConnectionDef.InTransaction: Boolean;
begin
  Result := FConn.Transaction.Active;
end;

function TdSQLdbConnectionDef.GetConnection: TObject;
begin
  Result := FConn;
end;

function TdSQLdbConnectionDef.GetTransaction: TObject;
begin
  Result := FConn.Transaction;
end;

function TdSQLdbConnectionDef.GetConnected: Boolean;
begin
  Result := FConn.Connected;
end;

function TdSQLdbConnectionDef.GetDatabase: string;
begin
  Result := FConn.DatabaseName;
end;

function TdSQLdbConnectionDef.GetDriver: string;
begin
  Result := FConn.ConnectorType;
end;

function TdSQLdbConnectionDef.GetHost: string;
begin
  Result := FConn.HostName;
end;

function TdSQLdbConnectionDef.GetPassword: string;
begin
  Result := FConn.Password;
end;

function TdSQLdbConnectionDef.GetPort: Integer;
begin
  Result := StrToIntDef(FConn.Params.Values['port'], 0);
end;

function TdSQLdbConnectionDef.GetUser: string;
begin
  Result := FConn.UserName;
end;

procedure TdSQLdbConnectionDef.SetConnected(const AValue: Boolean);
begin
  FConn.Connected := AValue;
end;

procedure TdSQLdbConnectionDef.SetDatabase(const AValue: string);
begin
  FConn.DatabaseName := AValue;
end;

procedure TdSQLdbConnectionDef.SetDriver(const AValue: string);
begin
  FConn.ConnectorType := AValue;
end;

procedure TdSQLdbConnectionDef.SetHost(const AValue: string);
begin
  FConn.HostName := AValue;
end;

procedure TdSQLdbConnectionDef.SetPassword(const AValue: string);
begin
  FConn.Password := AValue;
end;

procedure TdSQLdbConnectionDef.SetPort(const AValue: Integer);
begin
  FConn.Params.Values['port'] := IntToStr(AValue);
end;

procedure TdSQLdbConnectionDef.SetUser(const AValue: string);
begin
  FConn.UserName := AValue;
end;

{ TdSQLdbQueryDef }

constructor TdSQLdbQueryDef.Create;
begin
  inherited Create;
  FQuery := TSQLQuery.Create(nil);
end;

destructor TdSQLdbQueryDef.Destroy;
begin
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function TdSQLdbQueryDef.GetActive: Boolean;
begin
  Result := FQuery.Active;
end;

function TdSQLdbQueryDef.GetBOF: Boolean;
begin
  Result := FQuery.BOF;
end;

function TdSQLdbQueryDef.GetConnection: TdConnection;
begin
  Result := FConn;
end;

function TdSQLdbQueryDef.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TdSQLdbQueryDef.GetDataSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TdSQLdbQueryDef.GetEOF: Boolean;
begin
  Result := FQuery.EOF;
end;

function TdSQLdbQueryDef.GetFieldDefs: TFieldDefs;
begin
  Result := FQuery.FieldDefs;
end;

function TdSQLdbQueryDef.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TdSQLdbQueryDef.GetModified: Boolean;
begin
  Result := FQuery.Modified;
end;

function TdSQLdbQueryDef.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TdSQLdbQueryDef.GetPosition: Int64;
begin
  Result := FQuery.RecNo;
end;

function TdSQLdbQueryDef.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TdSQLdbQueryDef.GetState: TDataSetState;
begin
  Result := FQuery.State;
end;

procedure TdSQLdbQueryDef.SetActive(const AValue: Boolean);
begin
  FQuery.Active := AValue;
end;

procedure TdSQLdbQueryDef.SetConnection(AValue: TdConnection);
begin
  FConn := AValue;
  if Assigned(FConn) and Assigned(FConn.Def) and
    Assigned(FConn.Def.Connection) then
  begin
    FQuery.DataBase := TSQLConnector(FConn.Def.Connection);
    FQuery.Transaction := TSQLTransaction(FConn.Def.Transaction);
  end
  else
    FQuery.DataBase := nil;
end;

procedure TdSQLdbQueryDef.SetDataSource(AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

procedure TdSQLdbQueryDef.SetPosition(const AValue: Int64);
begin
  FQuery.RecNo := AValue;
end;

function TdSQLdbQueryDef.ApplyUpdates: TdQueryDef;
begin
  Result := Self;
  FQuery.ApplyUpdates(0);
end;

function TdSQLdbQueryDef.CancelUpdates: TdQueryDef;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TdSQLdbQueryDef.Apply: TdQueryDef;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  try
    if FQuery.Modified then
      FQuery.ApplyUpdates(0);
    VTrans.Commit;
  except
    VTrans.Rollback;
    raise;
  end;
end;

function TdSQLdbQueryDef.ApplyRetaining: TdQueryDef;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  try
    if FQuery.Modified then
      FQuery.ApplyUpdates(0);
    VTrans.CommitRetaining;
  except
    VTrans.RollbackRetaining;
    raise;
  end;
end;

function TdSQLdbQueryDef.Undo: TdQueryDef;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  if FQuery.Modified then
    FQuery.CancelUpdates;
  VTrans.Rollback;
end;

function TdSQLdbQueryDef.UndoRetaining: TdQueryDef;
var
  VTrans: TSQLTransaction;
begin
  Result := Self;
  VTrans := TSQLConnector(FQuery.DataBase).Transaction;
  if not VTrans.Active then
    Exit;
  if FQuery.Modified then
    FQuery.CancelUpdates;
  VTrans.RollbackRetaining;
end;

function TdSQLdbQueryDef.Append: TdQueryDef;
begin
  Result := Self;
  FQuery.Append;
end;

function TdSQLdbQueryDef.Insert: TdQueryDef;
begin
  Result := Self;
  FQuery.Insert;
end;

function TdSQLdbQueryDef.Edit: TdQueryDef;
begin
  Result := Self;
  FQuery.Edit;
end;

function TdSQLdbQueryDef.Cancel: TdQueryDef;
begin
  Result := Self;
  FQuery.Cancel;
end;

function TdSQLdbQueryDef.Delete: TdQueryDef;
begin
  Result := Self;
  FQuery.Delete;
end;

function TdSQLdbQueryDef.Open: TdQueryDef;
begin
  Result := Self;
  FQuery.Open;
end;

function TdSQLdbQueryDef.Close: TdQueryDef;
begin
  Result := Self;
  FQuery.Close;
end;

function TdSQLdbQueryDef.Refresh: TdQueryDef;
begin
  Result := Self;
  FQuery.Refresh;
end;

function TdSQLdbQueryDef.First: TdQueryDef;
begin
  Result := Self;
  FQuery.First;
end;

function TdSQLdbQueryDef.Prior: TdQueryDef;
begin
  Result := Self;
  FQuery.Prior;
end;

function TdSQLdbQueryDef.Next: TdQueryDef;
begin
  Result := Self;
  FQuery.Next;
end;

function TdSQLdbQueryDef.Last: TdQueryDef;
begin
  Result := Self;
  FQuery.Last;
end;

function TdSQLdbQueryDef.Post: TdQueryDef;
begin
  Result := Self;
  FQuery.Post;
end;

function TdSQLdbQueryDef.Execute: TdQueryDef;
begin
  Result := Self;
  FQuery.ExecSQL;
end;

function TdSQLdbQueryDef.RowsAffected: Int64;
begin
  Result := FQuery.RowsAffected;
end;

function TdSQLdbQueryDef.Locate(const AKeyFields: string;
  const AKeyValues: Variant; const AOptions: TLocateOptions): Boolean;
begin
  Result := FQuery.Locate(AKeyFields, AKeyValues, AOptions);
end;

function TdSQLdbQueryDef.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AName);
end;

function TdSQLdbQueryDef.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AName);
end;

function TdSQLdbQueryDef.FieldDef(const AName: string): TFieldDef;
begin
  Result := FQuery.FieldDefs.Find(AName);
end;

function TdSQLdbQueryDef.Count: Int64;
begin
  Result := FQuery.RecordCount;
end;

function TdSQLdbQueryDef.GetBookmark: TBookmark;
begin
  Result := FQuery.GetBookmark;
end;

procedure TdSQLdbQueryDef.GotoBookmark(ABookmark: TBookmark);
begin
  FQuery.GotoBookmark(ABookmark);
end;

{ TdSQLdbBroker }

class function TdSQLdbBroker.GetConnectionDefClass: TdConnectionDefClass;
begin
  Result := TdSQLdbConnectionDef;
end;

class function TdSQLdbBroker.GetQueryDefClass: TdQueryDefClass;
begin
  Result := TdSQLdbQueryDef;
end;

end.

