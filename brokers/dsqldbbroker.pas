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

  { TdSQLdbConnectionBroker }

  TdSQLdbConnectionBroker = class(TdConnectionBroker)
  private
    FCon: TSQLConnector;
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
    function Connect: TdConnectionBroker; override;
    function Disconnect: TdConnectionBroker; override;
    function StartTransaction: TdConnectionBroker; override;
    function Commit: TdConnectionBroker; override;
    function CommitRetaining: TdConnectionBroker; override;
    function Rollback: TdConnectionBroker; override;
    function RollbackRetaining: TdConnectionBroker; override;
    function InTransaction: Boolean; override;
  end;

  { TdSQLdbQueryBroker }

  TdSQLdbQueryBroker = class(TdQueryBroker)
  private
    FCon: TSQLConnector;
    FQuery: TSQLQuery;
  protected
    function GetActive: Boolean; override;
    function GetBOF: Boolean; override;
    function GetConnection: TObject; override;
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
    procedure SetConnection(AValue: TObject); override;
    procedure SetDataSource(AValue: TDataSource); override;
    procedure SetPosition(const AValue: Int64); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ApplyUpdates: TdQueryBroker; override;
    function CancelUpdates: TdQueryBroker; override;
    function Apply: TdQueryBroker; override;
    function ApplyRetaining: TdQueryBroker; override;
    function Undo: TdQueryBroker; override;
    function UndoRetaining: TdQueryBroker; override;
    function Append: TdQueryBroker; override;
    function Insert: TdQueryBroker; override;
    function Edit: TdQueryBroker; override;
    function Cancel: TdQueryBroker; override;
    function Delete: TdQueryBroker; override;
    function Open: TdQueryBroker; override;
    function Close: TdQueryBroker; override;
    function Refresh: TdQueryBroker; override;
    function First: TdQueryBroker; override;
    function Prior: TdQueryBroker; override;
    function Next: TdQueryBroker; override;
    function Last: TdQueryBroker; override;
    function Post: TdQueryBroker; override;
    function Execute: TdQueryBroker; override;
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

implementation

{ TdSQLdbConnectionBroker }

constructor TdSQLdbConnectionBroker.Create;
begin
  inherited Create;
  FCon := TSQLConnector.Create(nil);
  FCon.Transaction := TSQLTransaction.Create(FCon);
end;

destructor TdSQLdbConnectionBroker.Destroy;
begin
  FreeAndNil(FCon);
  inherited Destroy;
end;

function TdSQLdbConnectionBroker.Connect: TdConnectionBroker;
begin
  Result := Self;
  FCon.Connected := True;
end;

function TdSQLdbConnectionBroker.Disconnect: TdConnectionBroker;
begin
  Result := Self;
  FCon.Connected := False;
end;

function TdSQLdbConnectionBroker.StartTransaction: TdConnectionBroker;
begin
  Result := Self;
  FCon.Transaction.StartTransaction;
end;

function TdSQLdbConnectionBroker.Commit: TdConnectionBroker;
begin
  Result := Self;
  FCon.Transaction.Commit;
end;

function TdSQLdbConnectionBroker.CommitRetaining: TdConnectionBroker;
begin
  Result := Self;
  FCon.Transaction.CommitRetaining;
end;

function TdSQLdbConnectionBroker.Rollback: TdConnectionBroker;
begin
  Result := nil;
  FCon.Transaction.Rollback;
end;

function TdSQLdbConnectionBroker.RollbackRetaining: TdConnectionBroker;
begin
  Result := Self;
  FCon.Transaction.RollbackRetaining;
end;

function TdSQLdbConnectionBroker.InTransaction: Boolean;
begin
  Result := FCon.Transaction.Active;
end;

function TdSQLdbConnectionBroker.GetConnection: TObject;
begin
  Result := FCon;
end;

function TdSQLdbConnectionBroker.GetTransaction: TObject;
begin
  Result := FCon.Transaction;
end;

function TdSQLdbConnectionBroker.GetConnected: Boolean;
begin
  Result := FCon.Connected;
end;

function TdSQLdbConnectionBroker.GetDatabase: string;
begin
  Result := FCon.DatabaseName;
end;

function TdSQLdbConnectionBroker.GetDriver: string;
begin
  Result := FCon.ConnectorType;
end;

function TdSQLdbConnectionBroker.GetHost: string;
begin
  Result := FCon.HostName;
end;

function TdSQLdbConnectionBroker.GetPassword: string;
begin
  Result := FCon.Password;
end;

function TdSQLdbConnectionBroker.GetPort: Integer;
begin
  Result := StrToIntDef(FCon.Params.Values['port'], 0);
end;

function TdSQLdbConnectionBroker.GetUser: string;
begin
  Result := FCon.UserName;
end;

procedure TdSQLdbConnectionBroker.SetConnected(const AValue: Boolean);
begin
  FCon.Connected := AValue;
end;

procedure TdSQLdbConnectionBroker.SetDatabase(const AValue: string);
begin
  FCon.DatabaseName := AValue;
end;

procedure TdSQLdbConnectionBroker.SetDriver(const AValue: string);
begin
  FCon.ConnectorType := AValue;
end;

procedure TdSQLdbConnectionBroker.SetHost(const AValue: string);
begin
  FCon.HostName := AValue;
end;

procedure TdSQLdbConnectionBroker.SetPassword(const AValue: string);
begin
  FCon.Password := AValue;
end;

procedure TdSQLdbConnectionBroker.SetPort(const AValue: Integer);
begin
  FCon.Params.Values['port'] := IntToStr(AValue);
end;

procedure TdSQLdbConnectionBroker.SetUser(const AValue: string);
begin
  FCon.UserName := AValue;
end;

{ TdSQLdbQueryBroker }

constructor TdSQLdbQueryBroker.Create;
begin
  inherited Create;
  FQuery := TSQLQuery.Create(nil);
end;

destructor TdSQLdbQueryBroker.Destroy;
begin
  FreeAndNil(FQuery);
  inherited Destroy;
end;

function TdSQLdbQueryBroker.GetActive: Boolean;
begin
  Result := FQuery.Active;
end;

function TdSQLdbQueryBroker.GetBOF: Boolean;
begin
  Result := FQuery.BOF;
end;

function TdSQLdbQueryBroker.GetConnection: TObject;
begin
  Result := FCon;
end;

function TdSQLdbQueryBroker.GetDataSet: TDataSet;
begin
  Result := FQuery;
end;

function TdSQLdbQueryBroker.GetDataSource: TDataSource;
begin
  Result := FQuery.DataSource;
end;

function TdSQLdbQueryBroker.GetEOF: Boolean;
begin
  Result := FQuery.EOF;
end;

function TdSQLdbQueryBroker.GetFieldDefs: TFieldDefs;
begin
  Result := FQuery.FieldDefs;
end;

function TdSQLdbQueryBroker.GetFields: TFields;
begin
  Result := FQuery.Fields;
end;

function TdSQLdbQueryBroker.GetModified: Boolean;
begin
  Result := FQuery.Modified;
end;

function TdSQLdbQueryBroker.GetParams: TParams;
begin
  Result := FQuery.Params;
end;

function TdSQLdbQueryBroker.GetPosition: Int64;
begin
  Result := FQuery.RecNo;
end;

function TdSQLdbQueryBroker.GetSQL: TStrings;
begin
  Result := FQuery.SQL;
end;

function TdSQLdbQueryBroker.GetState: TDataSetState;
begin
  Result := FQuery.State;
end;

procedure TdSQLdbQueryBroker.SetActive(const AValue: Boolean);
begin
  FQuery.Active := AValue;
end;

procedure TdSQLdbQueryBroker.SetConnection(AValue: TObject);
begin
  if Assigned(AValue) and (AValue is TSQLConnector) then
  begin
    FCon := AValue as TSQLConnector;
    FQuery.DataBase := FCon;
    FQuery.Transaction := FCon.Transaction;
  end
  else
  begin
    FCon := nil;
    FQuery.Transaction := nil;
    FQuery.DataBase := nil;
  end;
end;

procedure TdSQLdbQueryBroker.SetDataSource(AValue: TDataSource);
begin
  FQuery.DataSource := AValue;
end;

procedure TdSQLdbQueryBroker.SetPosition(const AValue: Int64);
begin
  FQuery.RecNo := AValue;
end;

function TdSQLdbQueryBroker.ApplyUpdates: TdQueryBroker;
begin
  Result := Self;
  FQuery.ApplyUpdates(0);
end;

function TdSQLdbQueryBroker.CancelUpdates: TdQueryBroker;
begin
  Result := Self;
  FQuery.CancelUpdates;
end;

function TdSQLdbQueryBroker.Apply: TdQueryBroker;
begin
  Result := Self;
  if Assigned(FCon) and FCon.Transaction.Active then
    try
      if FQuery.Modified then
        FQuery.ApplyUpdates(0);
      FCon.Transaction.Commit;
    except
      FCon.Transaction.Rollback;
      raise;
    end;
end;

function TdSQLdbQueryBroker.ApplyRetaining: TdQueryBroker;
begin
  Result := Self;
  if Assigned(FCon) and FCon.Transaction.Active then
  try
    if FQuery.Modified then
      FQuery.ApplyUpdates(0);
    FCon.Transaction.CommitRetaining;
  except
    FCon.Transaction.RollbackRetaining;
    raise;
  end;
end;

function TdSQLdbQueryBroker.Undo: TdQueryBroker;
begin
  Result := Self;
  if Assigned(FCon) and FCon.Transaction.Active then
  begin
    if FQuery.Modified then
      FQuery.CancelUpdates;
    FCon.Transaction.Rollback;
  end;
end;

function TdSQLdbQueryBroker.UndoRetaining: TdQueryBroker;
begin
  Result := Self;
  if Assigned(FCon) and FCon.Transaction.Active then
  begin
    if FQuery.Modified then
      FQuery.CancelUpdates;
    FCon.Transaction.RollbackRetaining;
  end;
end;

function TdSQLdbQueryBroker.Append: TdQueryBroker;
begin
  Result := Self;
  FQuery.Append;
end;

function TdSQLdbQueryBroker.Insert: TdQueryBroker;
begin
  Result := Self;
  FQuery.Insert;
end;

function TdSQLdbQueryBroker.Edit: TdQueryBroker;
begin
  Result := Self;
  FQuery.Edit;
end;

function TdSQLdbQueryBroker.Cancel: TdQueryBroker;
begin
  Result := Self;
  FQuery.Cancel;
end;

function TdSQLdbQueryBroker.Delete: TdQueryBroker;
begin
  Result := Self;
  FQuery.Delete;
end;

function TdSQLdbQueryBroker.Open: TdQueryBroker;
begin
  Result := Self;
  FQuery.Open;
end;

function TdSQLdbQueryBroker.Close: TdQueryBroker;
begin
  Result := Self;
  FQuery.Close;
end;

function TdSQLdbQueryBroker.Refresh: TdQueryBroker;
begin
  Result := Self;
  FQuery.Refresh;
end;

function TdSQLdbQueryBroker.First: TdQueryBroker;
begin
  Result := Self;
  FQuery.First;
end;

function TdSQLdbQueryBroker.Prior: TdQueryBroker;
begin
  Result := Self;
  FQuery.Prior;
end;

function TdSQLdbQueryBroker.Next: TdQueryBroker;
begin
  Result := Self;
  FQuery.Next;
end;

function TdSQLdbQueryBroker.Last: TdQueryBroker;
begin
  Result := Self;
  FQuery.Last;
end;

function TdSQLdbQueryBroker.Post: TdQueryBroker;
begin
  Result := Self;
  FQuery.Post;
end;

function TdSQLdbQueryBroker.Execute: TdQueryBroker;
begin
  Result := Self;
  FQuery.ExecSQL;
end;

function TdSQLdbQueryBroker.RowsAffected: Int64;
begin
  Result := FQuery.RowsAffected;
end;

function TdSQLdbQueryBroker.Locate(const AKeyFields: string;
  const AKeyValues: Variant; const AOptions: TLocateOptions): Boolean;
begin
  Result := FQuery.Locate(AKeyFields, AKeyValues, AOptions);
end;

function TdSQLdbQueryBroker.Param(const AName: string): TParam;
begin
  Result := FQuery.Params.ParamByName(AName);
end;

function TdSQLdbQueryBroker.Field(const AName: string): TField;
begin
  Result := FQuery.Fields.FieldByName(AName);
end;

function TdSQLdbQueryBroker.FieldDef(const AName: string): TFieldDef;
begin
  Result := FQuery.FieldDefs.Find(AName);
end;

function TdSQLdbQueryBroker.Count: Int64;
begin
  Result := FQuery.RecordCount;
end;

function TdSQLdbQueryBroker.GetBookmark: TBookmark;
begin
  Result := FQuery.GetBookmark;
end;

procedure TdSQLdbQueryBroker.GotoBookmark(ABookmark: TBookmark);
begin
  FQuery.GotoBookmark(ABookmark);
end;

end.

