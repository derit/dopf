(*
  Duall Sistemas, Object Persistence Classes

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dOPF;

{$i dopf.inc}

interface

uses
  Classes, SysUtils, DB, TypInfo;

type
  EdNotImplemented = class(Exception);

  EdConnection = class(Exception);

  EdQuery = class(Exception);

  TdLogType = (ltTransaction, ltSQL, ltCustom);

  TdLogFilter = set of TdLogType;

  TdLogEvent = procedure(const AType: TdLogType; const AMsg: string) of object;

  TdBrokerClass = class of TdBroker;

  { TdLogger }

  TdLogger = class(TObject)
  private
    FActive: Boolean;
    FFileName: TFileName;
    FFilter: TdLogFilter;
    FOnLog: TdLogEvent;
    FStream: TFileStream;
    procedure SetActive(AValue: Boolean);
    procedure SetFileName(AValue: TFileName);
  protected
    property Stream: TFileStream read FStream write FStream;
  public
    constructor Create(const AFileName: TFileName); overload; virtual;
    destructor Destroy; override;
    procedure Log(const AType: TdLogType; AMsg: string);
    property Active: Boolean read FActive write SetActive;
    property Filter: TdLogFilter read FFilter write FFilter;
    property FileName: TFileName read FFileName write SetFileName;
    property OnLog: TdLogEvent read FOnLog write FOnLog;
  end;

  { TdConnectionDef }

  TdConnectionDef = class(TObject)
  protected
    function GetConnection: TObject; virtual;
    function GetTransaction: TObject; virtual;
    function GetConnected: Boolean; virtual;
    function GetDatabase: string; virtual;
    function GetDriver: string; virtual;
    function GetHost: string; virtual;
    function GetPassword: string; virtual;
    function GetPort: Integer; virtual;
    function GetUser: string; virtual;
    procedure SetConnected({%H-}const AValue: Boolean); virtual;
    procedure SetDatabase({%H-}const AValue: string); virtual;
    procedure SetDriver({%H-}const AValue: string); virtual;
    procedure SetHost({%H-}const AValue: string); virtual;
    procedure SetPassword({%H-}const AValue: string); virtual;
    procedure SetPort({%H-}const AValue: Integer); virtual;
    procedure SetUser({%H-}const AValue: string); virtual;
  public
    constructor Create; virtual;
    function Connect: TdConnectionDef; virtual;
    function Disconnect: TdConnectionDef; virtual;
    function StartTransaction: TdConnectionDef; virtual;
    function Commit: TdConnectionDef; virtual;
    function CommitRetaining: TdConnectionDef; virtual;
    function Rollback: TdConnectionDef; virtual;
    function RollbackRetaining: TdConnectionDef; virtual;
    function InTransaction: Boolean; virtual;
    property Connection: TObject read GetConnection;
    property Transaction: TObject read GetTransaction;
    property Connected: Boolean read GetConnected write SetConnected;
  published
    property Driver: string read GetDriver write SetDriver;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
  end;

  TdConnectionDefClass = class of TdConnectionDef;

  { TdConnection }

  TdConnection = class(TComponent)
  private
    FBrokerClass: TdBrokerClass;
    FDef: TdConnectionDef;
    FLogger: TdLogger;
    function GetConnected: Boolean;
    function GetDatabase: string;
    function GetDriver: string;
    function GetHost: string;
    function GetPassword: string;
    function GetPort: Integer;
    function GetUser: string;
    procedure SetConnected(AValue: Boolean);
    procedure SetDatabase(AValue: string);
    procedure SetDriver(AValue: string);
    procedure SetHost(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetPort(AValue: Integer);
    procedure SetUser(AValue: string);
  protected
    procedure CheckDef;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent;
      ABrokerClass: TdBrokerClass); overload; virtual;
    destructor Destroy; override;
    function GetBrokerClass: TdBrokerClass;
    procedure SetDef(ABrokerClass: TdBrokerClass);
    function Connect: TdConnection;
    function Disconnect: TdConnection;
    function StartTransaction: TdConnection;
    function Commit: TdConnection;
    function CommitRetaining: TdConnection;
    function Rollback: TdConnection;
    function RollbackRetaining: TdConnection;
    function InTransaction: Boolean;
    property Connected: Boolean read GetConnected write SetConnected;
    property Def: TdConnectionDef read FDef write FDef;
    property Logger: TdLogger read FLogger;
  published
    property Driver: string read GetDriver write SetDriver;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
  end;

  { TdQueryDef }

  TdQueryDef = class(TObject)
  protected
    function GetActive: Boolean; virtual;
    function GetBOF: Boolean; virtual;
    function GetConnection: TdConnection; virtual;
    function GetDataSet: TDataSet; virtual;
    function GetDataSource: TDataSource; virtual;
    function GetEOF: Boolean; virtual;
    function GetFieldDefs: TFieldDefs; virtual;
    function GetFields: TFields; virtual;
    function GetModified: Boolean; virtual;
    function GetParams: TParams; virtual;
    function GetPosition: Int64; virtual;
    function GetSQL: TStrings; virtual;
    function GetState: TDataSetState; virtual;
    procedure SetActive({%H-}const AValue: Boolean); virtual;
    procedure SetConnection({%H-}AValue: TdConnection); virtual;
    procedure SetDataSource({%H-}AValue: TDataSource); virtual;
    procedure SetPosition({%H-}const AValue: Int64); virtual;
  public
    constructor Create; virtual;
    function ApplyUpdates: TdQueryDef; virtual;
    function CancelUpdates: TdQueryDef; virtual;
    function Apply: TdQueryDef; virtual;
    function ApplyRetaining: TdQueryDef; virtual;
    function Undo: TdQueryDef; virtual;
    function UndoRetaining: TdQueryDef; virtual;
    function Append: TdQueryDef; virtual;
    function Insert: TdQueryDef; virtual;
    function Edit: TdQueryDef; virtual;
    function Cancel: TdQueryDef; virtual;
    function Delete: TdQueryDef; virtual;
    function Open: TdQueryDef; virtual;
    function Close: TdQueryDef; virtual;
    function Refresh: TdQueryDef; virtual;
    function First: TdQueryDef; virtual;
    function Prior: TdQueryDef; virtual;
    function Next: TdQueryDef; virtual;
    function Last: TdQueryDef; virtual;
    function Post: TdQueryDef; virtual;
    function Execute: TdQueryDef; virtual;
    function RowsAffected: Int64; virtual;
    function Locate({%H-}const AKeyFields: string;{%H-}const AKeyValues: Variant;
      {%H-}const AOptions: TLocateOptions = []): Boolean; virtual;
    function Param({%H-}const AName: string): TParam; virtual;
    function Field({%H-}const AName: string): TField; virtual;
    function FieldDef({%H-}const AName: string): TFieldDef; virtual;
    function Count: Int64; virtual;
    function GetBookmark: TBookmark; virtual;
    procedure GotoBookmark({%H-}ABookmark: TBookmark); virtual;
    property BOF: Boolean read GetBOF;
    property EOF: Boolean read GetEOF;
    property Bookmark: TBookmark read GetBookmark write GotoBookmark;
    property Modified: Boolean read GetModified;
    property Position: Int64 read GetPosition write SetPosition;
  published
    property Active: Boolean read GetActive write SetActive;
    property SQL: TStrings read GetSQL;
    property Fields: TFields read GetFields;
    property Params: TParams read GetParams;
    property State: TDataSetState read GetState;
    property DataSet: TDataSet read GetDataSet;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Connection: TdConnection read GetConnection write SetConnection;
  end;

  TdQueryDefClass = class of TdQueryDef;

  { TdQuery }

  TdQuery = class(TComponent)
  private
    FBrokerClass: TdBrokerClass;
    FDef: TdQueryDef;
    function GetActive: Boolean;
    function GetBOF: Boolean;
    function GetConnection: TdConnection;
    function GetDataSet: TDataSet;
    function GetDataSource: TDataSource;
    function GetEOF: Boolean;
    function GetFieldDefs: TFieldDefs;
    function GetFields: TFields;
    function GetModified: Boolean;
    function GetParams: TParams;
    function GetPosition: Int64;
    function GetSQL: TStrings;
    function GetState: TDataSetState;
    procedure SetActive(const AValue: Boolean);
    procedure SetConnection(AValue: TdConnection);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetPosition(const AValue: Int64);
  protected
    procedure CheckDef;
    function GetParameterizedSQL: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetDef(ABrokerClass: TdBrokerClass);
    function GetFields(AObject: TObject): TdQuery;
    function GetParams(AObject: TObject): TdQuery;
    function SetFields(AObject: TObject): TdQuery;
    function SetParams(AObject: TObject): TdQuery;
    function ApplyUpdates: TdQuery;
    function CancelUpdates: TdQuery;
    function Apply: TdQuery;
    function ApplyRetaining: TdQuery;
    function Undo: TdQuery;
    function UndoRetaining: TdQuery;
    function Append: TdQuery;
    function Insert: TdQuery;
    function Edit: TdQuery;
    function Cancel: TdQuery;
    function Delete: TdQuery;
    function Open: TdQuery;
    function Close: TdQuery;
    function Refresh: TdQuery;
    function First: TdQuery;
    function Prior: TdQuery;
    function Next: TdQuery;
    function Last: TdQuery;
    function Post: TdQuery;
    function Execute: TdQuery;
    function RowsAffected: Int64;
    function Locate(const AKeyFields: string; const AKeyValues: Variant;
      const AOptions: TLocateOptions = []): Boolean;
    function Param(const AName: string): TParam;
    function Field(const AName: string): TField;
    function FieldDef(const AName: string): TFieldDef;
    function Count: Int64;
    function GetBookmark: TBookmark;
    procedure GotoBookmark(ABookmark: TBookmark);
    property Def: TdQueryDef read FDef write FDef;
    property SQL: TStrings read GetSQL;
    property Fields: TFields read GetFields;
    property Params: TParams read GetParams;
    property BOF: Boolean read GetBOF;
    property EOF: Boolean read GetEOF;
    property Bookmark: TBookmark read GetBookmark write GotoBookmark;
    property Modified: Boolean read GetModified;
    property State: TDataSetState read GetState;
    property Active: Boolean read GetActive write SetActive;
    property DataSet: TDataSet read GetDataSet;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Connection: TdConnection read GetConnection write SetConnection;
    property Position: Int64 read GetPosition write SetPosition;
  end;

  { TdBroker }

  TdBroker = class(TObject)
  public
    class function GetConnectionDefClass: TdConnectionDefClass; virtual;
    class function GetQueryDefClass: TdQueryDefClass; virtual;
  end;

implementation

procedure NotImplementedError;
begin
  raise EdNotImplemented.Create('Not implemended.');
end;

{ TdLogger }

constructor TdLogger.Create(const AFileName: TFileName);
begin
  inherited Create;
  SetFileName(AFileName);
  FFilter := [ltTransaction, ltSQL, ltCustom];
end;

destructor TdLogger.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TdLogger.SetFileName(AValue: TFileName);
var
  F: TFileName;
begin
  if FActive and (Trim(AValue) <> '') and (AValue <> FFileName) then
  begin
    FFileName := AValue;
    FreeAndNil(FStream);
    F := ChangeFileExt(FFileName, '_' + FormatDateTime('yyyymmdd', Date) +
      ExtractFileExt(FFileName));
    if FileExists(F) then
    begin
      FStream := TFileStream.Create(F, fmOpenReadWrite);
      FStream.Seek(FStream.Size, soBeginning);
    end
    else
      FStream := TFileStream.Create(F, fmCreate);
  end;
end;

procedure TdLogger.SetActive(AValue: Boolean);
begin
  if AValue <> FActive then
  begin
    FActive := AValue;
    if not FActive then
      FreeAndNil(FStream);
  end;
end;

procedure TdLogger.Log(const AType: TdLogType; AMsg: string);
var
  T: string;
begin
  if FActive and (AType in FFilter) then
  begin
    WriteStr(T, AType);
    Delete(T, 1, 2);
    AMsg := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' - ' + T +
      ': ' + AMsg;
    if Assigned(FStream) then
    begin
      FStream.Write(AMsg[1], Length(AMsg));
      FStream.Write(LineEnding[1], Length(LineEnding));
    end;
    if Assigned(FOnLog) then
      FOnLog(AType, AMsg);
  end;
end;

{ TdConnectionDef }

constructor TdConnectionDef.Create;
begin
  inherited Create;
end;

function TdConnectionDef.Connect: TdConnectionDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.Disconnect: TdConnectionDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.StartTransaction: TdConnectionDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.Commit: TdConnectionDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.CommitRetaining: TdConnectionDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.Rollback: TdConnectionDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.RollbackRetaining: TdConnectionDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.InTransaction: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdConnectionDef.GetConnection: TObject;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.GetTransaction: TObject;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionDef.GetConnected: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdConnectionDef.GetDatabase: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionDef.GetDriver: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionDef.GetHost: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionDef.GetPassword: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionDef.GetPort: Integer;
begin
  Result := 0;
  NotImplementedError;
end;

function TdConnectionDef.GetUser: string;
begin
  Result := '';
  NotImplementedError;
end;

procedure TdConnectionDef.SetConnected(const AValue: Boolean);
begin
  NotImplementedError;
end;

procedure TdConnectionDef.SetDatabase(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionDef.SetDriver(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionDef.SetHost(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionDef.SetPassword(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionDef.SetPort(const AValue: Integer);
begin
  NotImplementedError;
end;

procedure TdConnectionDef.SetUser(const AValue: string);
begin
  NotImplementedError;
end;

{ TdConnection }

constructor TdConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogger := TdLogger.Create('');
end;

constructor TdConnection.Create(AOwner: TComponent;
  ABrokerClass: TdBrokerClass);
begin
  Create(AOwner);
  SetDef(ABrokerClass);
end;

destructor TdConnection.Destroy;
begin
  FreeAndNil(FDef);
  FLogger.Free;
  inherited Destroy;
end;

function TdConnection.GetBrokerClass: TdBrokerClass;
begin
  Result := FBrokerClass;
end;

procedure TdConnection.SetDef(ABrokerClass: TdBrokerClass);
begin
  if not Assigned(ABrokerClass) then
    raise EdConnection.Create('Broker class must not be nil.');
  FreeAndNil(FDef);
  FBrokerClass := ABrokerClass;
  FDef := FBrokerClass.GetConnectionDefClass.Create;
end;

procedure TdConnection.CheckDef;
begin
  if not Assigned(FDef) then
    raise EdConnection.Create('Definition is not set.');
end;

function TdConnection.GetConnected: Boolean;
begin
  CheckDef;
  Result := FDef.GetConnected;
end;

function TdConnection.GetDatabase: string;
begin
  CheckDef;
  Result := FDef.GetDatabase;
end;

function TdConnection.GetDriver: string;
begin
  CheckDef;
  Result := FDef.GetDriver;
end;

function TdConnection.GetHost: string;
begin
  CheckDef;
  Result := FDef.GetHost;
end;

function TdConnection.GetPassword: string;
begin
  CheckDef;
  Result := FDef.GetPassword;
end;

function TdConnection.GetPort: Integer;
begin
  CheckDef;
  Result := FDef.GetPort;
end;

function TdConnection.GetUser: string;
begin
  CheckDef;
  Result := FDef.GetUser;
end;

procedure TdConnection.SetConnected(AValue: Boolean);
begin
  CheckDef;
  FDef.SetConnected(AValue);
end;

procedure TdConnection.SetDatabase(AValue: string);
begin
  CheckDef;
  FDef.SetDatabase(AValue);
end;

procedure TdConnection.SetDriver(AValue: string);
begin
  CheckDef;
  FDef.SetDriver(AValue);
end;

procedure TdConnection.SetHost(AValue: string);
begin
  CheckDef;
  FDef.SetHost(AValue);
end;

procedure TdConnection.SetPassword(AValue: string);
begin
  CheckDef;
  FDef.SetPassword(AValue);
end;

procedure TdConnection.SetPort(AValue: Integer);
begin
  CheckDef;
  FDef.SetPort(AValue);
end;

procedure TdConnection.SetUser(AValue: string);
begin
  CheckDef;
  FDef.SetUser(AValue);
end;

function TdConnection.Connect: TdConnection;
begin
  Result := Self;
  CheckDef;
  FDef.Connect;
end;

function TdConnection.Disconnect: TdConnection;
begin
  Result := Self;
  CheckDef;
  FDef.Disconnect;
end;

function TdConnection.StartTransaction: TdConnection;
begin
  Result := Self;
  CheckDef;
  FDef.StartTransaction;
end;

function TdConnection.Commit: TdConnection;
begin
  Result := Self;
  CheckDef;
  FLogger.Log(ltTransaction, 'Trying Connection.Commit');
  FDef.Commit;
end;

function TdConnection.CommitRetaining: TdConnection;
begin
  Result := Self;
  CheckDef;
  FLogger.Log(ltTransaction, 'Trying Connection.CommitRetaining');
  FDef.CommitRetaining;
end;

function TdConnection.Rollback: TdConnection;
begin
  Result := Self;
  CheckDef;
  FLogger.Log(ltTransaction, 'Trying Connection.Rollback');
  FDef.Rollback;
end;

function TdConnection.RollbackRetaining: TdConnection;
begin
  Result := Self;
  CheckDef;
  FLogger.Log(ltTransaction, 'Trying Connection.RollbackRetaining');
  FDef.RollbackRetaining;
end;

function TdConnection.InTransaction: Boolean;
begin
  CheckDef;
  Result := FDef.InTransaction;
end;

{ TdQueryDef }

constructor TdQueryDef.Create;
begin
  inherited Create;
end;

function TdQueryDef.ApplyUpdates: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.CancelUpdates: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Apply: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.ApplyRetaining: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Undo: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.UndoRetaining: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Append: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Insert: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Edit: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Cancel: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Delete: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Open: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Close: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Refresh: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.First: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Prior: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Next: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Last: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Post: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Execute: TdQueryDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.RowsAffected: Int64;
begin
  Result := 0;
  NotImplementedError;
end;

function TdQueryDef.Locate(const AKeyFields: string; const AKeyValues: Variant;
  const AOptions: TLocateOptions): Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryDef.Param(const AName: string): TParam;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Field(const AName: string): TField;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.FieldDef(const AName: string): TFieldDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.Count: Int64;
begin
  Result := 0;
  NotImplementedError;
end;

function TdQueryDef.GetBookmark: TBookmark;
begin
  Result := nil;
  NotImplementedError;
end;

procedure TdQueryDef.GotoBookmark(ABookmark: TBookmark);
begin
  NotImplementedError;
end;

function TdQueryDef.GetActive: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryDef.GetBOF: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryDef.GetConnection: TdConnection;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.GetDataSet: TDataSet;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.GetDataSource: TDataSource;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.GetEOF: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryDef.GetFieldDefs: TFieldDefs;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.GetFields: TFields;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.GetModified: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryDef.GetParams: TParams;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.GetPosition: Int64;
begin
  Result := 0;
  NotImplementedError;
end;

function TdQueryDef.GetSQL: TStrings;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryDef.GetState: TDataSetState;
begin
  Result := dsInactive;
  NotImplementedError;
end;

procedure TdQueryDef.SetActive(const AValue: Boolean);
begin
  NotImplementedError;
end;

procedure TdQueryDef.SetConnection(AValue: TdConnection);
begin
  NotImplementedError;
end;

procedure TdQueryDef.SetDataSource(AValue: TDataSource);
begin
  NotImplementedError;
end;

procedure TdQueryDef.SetPosition(const AValue: Int64);
begin
  NotImplementedError;
end;

{ TdQuery }

constructor TdQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TdConnection then
  begin
    SetDef(TdConnection(AOwner).GetBrokerClass);
    SetConnection(TdConnection(AOwner));
  end;
end;

destructor TdQuery.Destroy;
begin
  FreeAndNil(FDef);
  inherited Destroy;
end;

procedure TdQuery.SetDef(ABrokerClass: TdBrokerClass);
begin
  if not Assigned(ABrokerClass) then
    raise EdQuery.Create('Broker class must not be nil.');
  FreeAndNil(FDef);
  FBrokerClass := ABrokerClass;
  FDef := FBrokerClass.GetQueryDefClass.Create;
end;

function TdQuery.GetFields(AObject: TObject): TdQuery;
var
  I: Integer;
  F: TField;
  PI: PPropInfo;
begin
  Result := Self;
  Connection.Logger.Log(ltCustom, 'Trying Query.GetFields');
  for I := 0 to Pred(Fields.Count) do
  begin
    F := Fields[I];
    PI := GetPropInfo(PTypeInfo(AObject.ClassInfo), F.FieldName);
    if not Assigned(PI) then
      Continue;
    case F.DataType of
      ftFixedWideChar, ftWideString, ftFixedChar,
        ftString: SetStrProp(AObject, PI, F.AsString);
      ftSmallInt, ftInteger, ftAutoInc,
        ftWord: SetOrdProp(AObject, PI, F.AsInteger);
      ftLargeInt: SetInt64Prop(AObject, PI, F.AsLargeInt);
      ftFloat: SetFloatProp(AObject, PI, F.AsFloat);
      ftBoolean: SetOrdProp(AObject, PI, Ord(F.AsBoolean));
      ftDate, ftTime, ftDateTime: SetFloatProp(AObject, PI, F.AsDateTime);
    end;
  end;
end;

function TdQuery.GetParams(AObject: TObject): TdQuery;
var
  I: Integer;
  P: TParam;
  PI: PPropInfo;
begin
  Result := Self;
  Connection.Logger.Log(ltCustom, 'Trying Query.GetParams');
  for I := 0 to Pred(Params.Count) do
  begin
    P := Params[I];
    PI := GetPropInfo(PTypeInfo(AObject.ClassInfo), P.Name);
    if not Assigned(PI) then
      Continue;
    case P.DataType of
      ftFixedWideChar, ftWideString, ftFixedChar,
        ftString: SetStrProp(AObject, PI, P.AsString);
      ftSmallInt, ftInteger, ftAutoInc,
        ftWord: SetOrdProp(AObject, PI, P.AsInteger);
      ftLargeInt: SetInt64Prop(AObject, PI, P.AsLargeInt);
      ftFloat: SetFloatProp(AObject, PI, P.AsFloat);
      ftBoolean: SetOrdProp(AObject, PI, Ord(P.AsBoolean));
      ftDate, ftTime, ftDateTime: SetFloatProp(AObject, PI, P.AsDateTime);
    end;
  end;
end;

function TdQuery.SetFields(AObject: TObject): TdQuery;
var
  C, I: Integer;
  F: TField;
  PI: PPropInfo;
  PL: PPropList = nil;
begin
  Result := Self;
  Connection.Logger.Log(ltCustom, 'Trying Query.SetFields');
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      for I := 0 to Pred(C) do
      begin
        PI := PL^[I];
        F := Fields.FindField(PI^.Name);
        if not Assigned(F) then
          Continue;
        case PI^.PropType^.Kind of
          tkAString: F.AsString := GetStrProp(AObject, PI);
          tkChar: PChar(F.AsString)^ := Char(GetOrdProp(AObject, PI));
          tkInteger: F.AsInteger := GetOrdProp(AObject, PI);
          tkInt64, tkQWord: F.AsLargeInt := GetInt64Prop(AObject, PI);
          tkBool: F.AsBoolean := GetOrdProp(AObject, PI) <> 0;
          tkFloat: F.AsFloat := GetFloatProp(AObject, PI);
          tkEnumeration: F.AsString := GetEnumProp(AObject, PI);
          tkSet: F.AsString := GetSetProp(AObject, PI, False);
        end;
      end;
    finally
      FreeMem(PL);
    end;
end;

function TdQuery.SetParams(AObject: TObject): TdQuery;
var
  C, I: Integer;
  P: TParam;
  PI: PPropInfo;
  PL: PPropList = nil;
begin
  Result := Self;
  Connection.Logger.Log(ltCustom, 'Trying Query.SetParams');
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      for I := 0 to Pred(C) do
      begin
        PI := PL^[I];
        P := Params.FindParam(PI^.Name);
        if not Assigned(P) then
          Continue;
        case PI^.PropType^.Kind of
          tkAString: P.AsString := GetStrProp(AObject, PI);
          tkChar: PChar(P.AsString)^ := Char(GetOrdProp(AObject, PI));
          tkInteger: P.AsInteger := GetOrdProp(AObject, PI);
          tkInt64, tkQWord: P.AsLargeInt := GetInt64Prop(AObject, PI);
          tkBool: P.AsBoolean := GetOrdProp(AObject, PI) <> 0;
          tkFloat: P.AsFloat := GetFloatProp(AObject, PI);
          tkEnumeration: P.AsString := GetEnumProp(AObject, PI);
          tkSet: P.AsString := GetSetProp(AObject, PI, False);
        end;
      end;
    finally
      FreeMem(PL);
    end;
end;

procedure TdQuery.CheckDef;
begin
  if not Assigned(FDef) then
    raise EdQuery.Create('Definition is not set.');
end;

function TdQuery.GetParameterizedSQL: string;
var
  V: string;
  I: Integer;
  P: TParam;
begin
  Result := Trim(SQL.Text);
  for I := 0 to Pred(Params.Count) do
  begin
    P := Params[I];
    V := P.AsString;
    case P.DataType of
      ftString, ftDate, ftTime, ftDateTime, ftMemo, ftFixedChar, ftGuid:
        V := QuotedStr(V);
      ftCurrency: V := FloatToStr(P.AsFloat);
    end;
    Result := StringReplace(Result, ':' + P.Name, V, [rfIgnoreCase, rfReplaceAll]);
  end;
end;

function TdQuery.GetActive: Boolean;
begin
  CheckDef;
  Result := FDef.GetActive;
end;

function TdQuery.GetBOF: Boolean;
begin
  CheckDef;
  Result := FDef.GetBOF;
end;

function TdQuery.GetConnection: TdConnection;
begin
  CheckDef;
  Result := FDef.GetConnection;
end;

function TdQuery.GetDataSet: TDataSet;
begin
  CheckDef;
  Result := FDef.GetDataSet;
end;

function TdQuery.GetDataSource: TDataSource;
begin
  CheckDef;
  Result := FDef.GetDataSource;
end;

function TdQuery.GetEOF: Boolean;
begin
  CheckDef;
  Result := FDef.GetEOF;
end;

function TdQuery.GetFieldDefs: TFieldDefs;
begin
  CheckDef;
  Result := FDef.GetFieldDefs;
end;

function TdQuery.GetFields: TFields;
begin
  CheckDef;
  Result := FDef.GetFields;
end;

function TdQuery.GetModified: Boolean;
begin
  CheckDef;
  Result := FDef.GetModified;
end;

function TdQuery.GetParams: TParams;
begin
  CheckDef;
  Result := FDef.GetParams;
end;

function TdQuery.GetPosition: Int64;
begin
  CheckDef;
  Result := FDef.GetPosition;
end;

function TdQuery.GetSQL: TStrings;
begin
  CheckDef;
  Result := FDef.GetSQL;
end;

function TdQuery.GetState: TDataSetState;
begin
  CheckDef;
  Result := FDef.GetState;
end;

procedure TdQuery.SetActive(const AValue: Boolean);
begin
  CheckDef;
  FDef.SetActive(AValue);
end;

procedure TdQuery.SetConnection(AValue: TdConnection);
begin
  CheckDef;
  FDef.SetConnection(AValue);
end;

procedure TdQuery.SetDataSource(AValue: TDataSource);
begin
  CheckDef;
  FDef.SetDataSource(AValue);
end;

procedure TdQuery.SetPosition(const AValue: Int64);
begin
  CheckDef;
  FDef.SetPosition(AValue);
end;

function TdQuery.ApplyUpdates: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltCustom, 'Trying Query.ApplyUpdates');
  FDef.ApplyUpdates;
end;

function TdQuery.CancelUpdates: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltCustom, 'Trying Query.CancelUpdates');
  FDef.CancelUpdates;
end;

function TdQuery.Apply: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltCustom, 'Trying Query.Apply');
  FDef.Apply;
end;

function TdQuery.ApplyRetaining: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltCustom, 'Trying Query.ApplyRetaining');
  FDef.ApplyRetaining;
end;

function TdQuery.Undo: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltCustom, 'Trying Query.Undo');
  FDef.Undo;
end;

function TdQuery.UndoRetaining: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltCustom, 'Trying Query.UndoRetaining');
  FDef.UndoRetaining;
end;

function TdQuery.Append: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Append;
end;

function TdQuery.Insert: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Insert;
end;

function TdQuery.Edit: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Edit;
end;

function TdQuery.Cancel: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Cancel;
end;

function TdQuery.Delete: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Delete;
end;

function TdQuery.Open: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltSQL, Trim(SQL.Text));
  FDef.Open;
end;

function TdQuery.Close: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Close;
end;

function TdQuery.Refresh: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Refresh;
end;

function TdQuery.First: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.First;
end;

function TdQuery.Prior: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Prior;
end;

function TdQuery.Next: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Next;
end;

function TdQuery.Last: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Last;
end;

function TdQuery.Post: TdQuery;
begin
  Result := Self;
  CheckDef;
  FDef.Post;
end;

function TdQuery.Execute: TdQuery;
begin
  Result := Self;
  CheckDef;
  Connection.Logger.Log(ltSQL, GetParameterizedSQL);
  FDef.Execute;
end;

function TdQuery.RowsAffected: Int64;
begin
  CheckDef;
  Result := FDef.RowsAffected;
end;

function TdQuery.Locate(const AKeyFields: string; const AKeyValues: Variant;
  const AOptions: TLocateOptions): Boolean;
begin
  CheckDef;
  Result := FDef.Locate(AKeyFields, AKeyValues, AOptions);
end;

function TdQuery.Param(const AName: string): TParam;
begin
  CheckDef;
  Result := FDef.Param(AName);
end;

function TdQuery.Field(const AName: string): TField;
begin
  CheckDef;
  Result := FDef.Field(AName);
end;

function TdQuery.FieldDef(const AName: string): TFieldDef;
begin
  CheckDef;
  Result := FDef.FieldDef(AName);
end;

function TdQuery.Count: Int64;
begin
  CheckDef;
  Result := FDef.Count;
end;

function TdQuery.GetBookmark: TBookmark;
begin
  CheckDef;
  Result := FDef.GetBookmark;
end;

procedure TdQuery.GotoBookmark(ABookmark: TBookmark);
begin
  CheckDef;
  FDef.GotoBookmark(ABookmark);
end;

{ TdBroker }

class function TdBroker.GetConnectionDefClass: TdConnectionDefClass;
begin
  Result := nil;
  NotImplementedError;
end;

class function TdBroker.GetQueryDefClass: TdQueryDefClass;
begin
  Result := nil;
  NotImplementedError;
end;

end.

