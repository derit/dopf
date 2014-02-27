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
  dUtils, Classes, SysUtils, DB;

type
  EdNotImplemented = class(Exception);

  EdConnection = class(Exception);

  EdQuery = class(Exception);

  TdLogType = (ltTransaction, ltSQL, ltCustom);

  TdLogFilter = set of TdLogType;

  TdLogEvent = procedure(const AType: TdLogType; const AMsg: string) of object;

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

  { TdConnectionBroker }

  TdConnectionBroker = class(TObject)
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
    function Connect: TdConnectionBroker; virtual;
    function Disconnect: TdConnectionBroker; virtual;
    function StartTransaction: TdConnectionBroker; virtual;
    function Commit: TdConnectionBroker; virtual;
    function CommitRetaining: TdConnectionBroker; virtual;
    function Rollback: TdConnectionBroker; virtual;
    function RollbackRetaining: TdConnectionBroker; virtual;
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

  { TdConnection }

  generic TdConnection<T1, T2> = class(TComponent)
  private
    FBroker: T1;
    FLogger: T2;
    function GetConnected: Boolean;
    function GetDatabase: string;
    function GetDriver: string;
    function GetHost: string;
    function GetPassword: string;
    function GetPort: Integer;
    function GetUser: string;
    procedure SetConnected(const AValue: Boolean);
    procedure SetDatabase(const AValue: string);
    procedure SetDriver(const AValue: string);
    procedure SetHost(const AValue: string);
    procedure SetPassword(const AValue: string);
    procedure SetPort(const AValue: Integer);
    procedure SetUser(const AValue: string);
  protected
    procedure CheckBrokerClass; virtual;
    procedure CheckBroker; virtual;
    procedure CheckLoggerClass; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect: T1;
    function Disconnect: T1;
    function StartTransaction: T1;
    function Commit: T1;
    function CommitRetaining: T1;
    function Rollback: T1;
    function RollbackRetaining: T1;
    function InTransaction: Boolean;
    property Broker: T1 read FBroker write FBroker;
    property Logger: T2 read FLogger write FLogger;
    property Connected: Boolean read GetConnected write SetConnected;
  published
    property Driver: string read GetDriver write SetDriver;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write SetPort;
  end;

  { TdQueryBroker }

  TdQueryBroker = class(TObject)
  protected
    function GetActive: Boolean; virtual;
    function GetBOF: Boolean; virtual;
    function GetConnection: TObject; virtual;
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
    procedure SetConnection({%H-}AValue: TObject); virtual;
    procedure SetDataSource({%H-}AValue: TDataSource); virtual;
    procedure SetPosition({%H-}const AValue: Int64); virtual;
  public
    constructor Create; virtual;
    function ApplyUpdates: TdQueryBroker; virtual;
    function CancelUpdates: TdQueryBroker; virtual;
    function Apply: TdQueryBroker; virtual;
    function ApplyRetaining: TdQueryBroker; virtual;
    function Undo: TdQueryBroker; virtual;
    function UndoRetaining: TdQueryBroker; virtual;
    function Append: TdQueryBroker; virtual;
    function Insert: TdQueryBroker; virtual;
    function Edit: TdQueryBroker; virtual;
    function Cancel: TdQueryBroker; virtual;
    function Delete: TdQueryBroker; virtual;
    function Open: TdQueryBroker; virtual;
    function Close: TdQueryBroker; virtual;
    function Refresh: TdQueryBroker; virtual;
    function First: TdQueryBroker; virtual;
    function Prior: TdQueryBroker; virtual;
    function Next: TdQueryBroker; virtual;
    function Last: TdQueryBroker; virtual;
    function Post: TdQueryBroker; virtual;
    function Execute: TdQueryBroker; virtual;
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
    property Connection: TObject read GetConnection write SetConnection;
  end;

  { TdQuery }

  generic TdQuery<T1, T2> = class(TComponent)
  private
    FBroker: T1;
    FConnection: T2;
    function GetActive: Boolean;
    function GetBOF: Boolean;
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
    procedure SetDataSource(AValue: TDataSource);
    procedure SetPosition(const AValue: Int64);
  protected
    procedure CheckConnection; virtual;
    procedure CheckBrokerClass; virtual;
    procedure CheckBroker; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplyUpdates: T1;
    function CancelUpdates: T1;
    function Apply: T1;
    function ApplyRetaining: T1;
    function Undo: T1;
    function UndoRetaining: T1;
    function Append: T1;
    function Insert: T1;
    function Edit: T1;
    function Cancel: T1;
    function Delete: T1;
    function Open: T1;
    function Close: T1;
    function Refresh: T1;
    function First: T1;
    function Prior: T1;
    function Next: T1;
    function Last: T1;
    function Post: T1;
    function Execute: T1;
    function RowsAffected: Int64;
    function Locate({%H-}const AKeyFields: string;{%H-}const AKeyValues: Variant;
      {%H-}const AOptions: TLocateOptions = []): Boolean;
    function Param({%H-}const AName: string): TParam;
    function Field({%H-}const AName: string): TField;
    function FieldDef({%H-}const AName: string): TFieldDef;
    function Count: Int64;
    function GetBookmark: TBookmark;
    procedure GotoBookmark({%H-}ABookmark: TBookmark);
    property Connection: T2 read FConnection write FConnection;
    property Broker: T1 read FBroker write FBroker;
    property SQL: TStrings read GetSQL;
    property Fields: TFields read GetFields;
    property FieldDefs: TFieldDefs read GetFieldDefs;
    property Params: TParams read GetParams;
    property BOF: Boolean read GetBOF;
    property EOF: Boolean read GetEOF;
    property Bookmark: TBookmark read GetBookmark write GotoBookmark;
    property Modified: Boolean read GetModified;
    property State: TDataSetState read GetState;
    property Active: Boolean read GetActive write SetActive;
    property DataSet: TDataSet read GetDataSet;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Position: Int64 read GetPosition write SetPosition;
  end;

  { TdEntityQuery }

  generic TdEntityQuery<T1, T2, T3> = class(specialize TdQuery<T1, T2>)
  private
    FEntity: T3;
  protected
    function CreateEntity: T3; virtual;
    procedure FreeEntity; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFields: T1;
    function SetFields: T1;
    function GetParams: T1;
    function SetParams: T1;
    property Entity: T3 read FEntity write FEntity;
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

{ TdConnectionBroker }

constructor TdConnectionBroker.Create;
begin
  inherited Create;
end;

function TdConnectionBroker.Connect: TdConnectionBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.Disconnect: TdConnectionBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.StartTransaction: TdConnectionBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.Commit: TdConnectionBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.CommitRetaining: TdConnectionBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.Rollback: TdConnectionBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.RollbackRetaining: TdConnectionBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.InTransaction: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdConnectionBroker.GetConnection: TObject;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.GetTransaction: TObject;
begin
  Result := nil;
  NotImplementedError;
end;

function TdConnectionBroker.GetConnected: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdConnectionBroker.GetDatabase: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionBroker.GetDriver: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionBroker.GetHost: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionBroker.GetPassword: string;
begin
  Result := '';
  NotImplementedError;
end;

function TdConnectionBroker.GetPort: Integer;
begin
  Result := 0;
  NotImplementedError;
end;

function TdConnectionBroker.GetUser: string;
begin
  Result := '';
  NotImplementedError;
end;

procedure TdConnectionBroker.SetConnected(const AValue: Boolean);
begin
  NotImplementedError;
end;

procedure TdConnectionBroker.SetDatabase(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionBroker.SetDriver(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionBroker.SetHost(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionBroker.SetPassword(const AValue: string);
begin
  NotImplementedError;
end;

procedure TdConnectionBroker.SetPort(const AValue: Integer);
begin
  NotImplementedError;
end;

procedure TdConnectionBroker.SetUser(const AValue: string);
begin
  NotImplementedError;
end;

{ TdConnection }

constructor TdConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CheckBrokerClass;
  CheckLoggerClass;
  FBroker := T1.Create;
  FLogger := T2.Create('');
end;

destructor TdConnection.Destroy;
begin
  FBroker.Free;
  FLogger.Free;
  inherited Destroy;
end;

procedure TdConnection.CheckBrokerClass;
begin
  if not T1.InheritsFrom(TdConnectionBroker) then
    raise EdConnection.CreateFmt('Invalid broker class: "%s".', [T1.ClassName]);
end;

procedure TdConnection.CheckBroker;
begin
  if FBroker = nil then
    raise EdConnection.Create('Broker not assigned.');
end;

procedure TdConnection.CheckLoggerClass;
begin
  if not T2.InheritsFrom(TdLogger) then
    raise EdConnection.CreateFmt('Invalid logger class: "%s".', [T2.ClassName]);
end;

function TdConnection.GetConnected: Boolean;
begin
  CheckBroker;
  Result := FBroker.Connected;
end;

function TdConnection.GetDatabase: string;
begin
  CheckBroker;
  Result := FBroker.Database;
end;

function TdConnection.GetDriver: string;
begin
  CheckBroker;
  Result := FBroker.Driver;
end;

function TdConnection.GetHost: string;
begin
  CheckBroker;
  Result := FBroker.Host;
end;

function TdConnection.GetPassword: string;
begin
  CheckBroker;
  Result := FBroker.Password;
end;

function TdConnection.GetPort: Integer;
begin
  CheckBroker;
  Result := FBroker.Port;
end;

function TdConnection.GetUser: string;
begin
  CheckBroker;
  Result := FBroker.User;
end;

procedure TdConnection.SetConnected(const AValue: Boolean);
begin
  CheckBroker;
  FBroker.Connected := AValue;
end;

procedure TdConnection.SetDatabase(const AValue: string);
begin
  CheckBroker;
  FBroker.Database := AValue;
end;

procedure TdConnection.SetDriver(const AValue: string);
begin
  CheckBroker;
  FBroker.Driver := AValue;
end;

procedure TdConnection.SetHost(const AValue: string);
begin
  CheckBroker;
  FBroker.Host := AValue;
end;

procedure TdConnection.SetPassword(const AValue: string);
begin
  CheckBroker;
  FBroker.Password := AValue;
end;

procedure TdConnection.SetPort(const AValue: Integer);
begin
  CheckBroker;
  FBroker.Port := AValue;
end;

procedure TdConnection.SetUser(const AValue: string);
begin
  CheckBroker;
  FBroker.User := AValue;
end;

function TdConnection.Connect: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Connect;
end;

function TdConnection.Disconnect: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Disconnect;
end;

function TdConnection.StartTransaction: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.StartTransaction;
end;

function TdConnection.Commit: T1;
begin
  Result := FBroker;
  CheckBroker;
  FLogger.Log(ltTransaction, 'Trying Connection.Commit');
  FBroker.Commit;
end;

function TdConnection.CommitRetaining: T1;
begin
  Result := FBroker;
  CheckBroker;
  FLogger.Log(ltTransaction, 'Trying Connection.CommitRetaining');
  FBroker.CommitRetaining;
end;

function TdConnection.Rollback: T1;
begin
  Result := FBroker;
  CheckBroker;
  FLogger.Log(ltTransaction, 'Trying Connection.Rollback');
  FBroker.Rollback;
end;

function TdConnection.RollbackRetaining: T1;
begin
  Result := FBroker;
  CheckBroker;
  FLogger.Log(ltTransaction, 'Trying Connection.RollbackRetaining');
  FBroker.RollbackRetaining;
end;

function TdConnection.InTransaction: Boolean;
begin
  CheckBroker;
  Result := FBroker.InTransaction;
end;

{ TdQueryBroker }

constructor TdQueryBroker.Create;
begin
  inherited Create;
end;

function TdQueryBroker.ApplyUpdates: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.CancelUpdates: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Apply: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.ApplyRetaining: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Undo: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.UndoRetaining: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Append: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Insert: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Edit: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Cancel: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Delete: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Open: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Close: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Refresh: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.First: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Prior: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Next: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Last: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Post: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Execute: TdQueryBroker;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.RowsAffected: Int64;
begin
  Result := 0;
  NotImplementedError;
end;

function TdQueryBroker.Locate(const AKeyFields: string;
  const AKeyValues: Variant; const AOptions: TLocateOptions): Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryBroker.Param(const AName: string): TParam;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Field(const AName: string): TField;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.FieldDef(const AName: string): TFieldDef;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.Count: Int64;
begin
  Result := 0;
  NotImplementedError;
end;

function TdQueryBroker.GetBookmark: TBookmark;
begin
  Result := nil;
  NotImplementedError;
end;

procedure TdQueryBroker.GotoBookmark(ABookmark: TBookmark);
begin
  NotImplementedError;
end;

function TdQueryBroker.GetConnection: TObject;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.GetActive: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryBroker.GetBOF: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryBroker.GetDataSet: TDataSet;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.GetDataSource: TDataSource;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.GetEOF: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryBroker.GetFieldDefs: TFieldDefs;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.GetFields: TFields;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.GetModified: Boolean;
begin
  Result := False;
  NotImplementedError;
end;

function TdQueryBroker.GetParams: TParams;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.GetPosition: Int64;
begin
  Result := 0;
  NotImplementedError;
end;

function TdQueryBroker.GetSQL: TStrings;
begin
  Result := nil;
  NotImplementedError;
end;

function TdQueryBroker.GetState: TDataSetState;
begin
  Result := dsInactive;
  NotImplementedError;
end;

procedure TdQueryBroker.SetActive(const AValue: Boolean);
begin
  NotImplementedError;
end;

procedure TdQueryBroker.SetConnection(AValue: TObject);
begin
  NotImplementedError;
end;

procedure TdQueryBroker.SetDataSource(AValue: TDataSource);
begin
  NotImplementedError;
end;

procedure TdQueryBroker.SetPosition(const AValue: Int64);
begin
  NotImplementedError;
end;

{ TdQuery }

constructor TdQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CheckBrokerClass;
  FBroker := T1.Create;
  FConnection := T2(AOwner);
  if Assigned(AOwner) then
    FBroker.Connection := FConnection.Broker.Connection;
end;

destructor TdQuery.Destroy;
begin
  FBroker.Free;
  inherited Destroy;
end;

procedure TdQuery.CheckBrokerClass;
begin
  if not T1.InheritsFrom(TdQueryBroker) then
    raise EdQuery.CreateFmt('Invalid broker class: "%s".', [T1.ClassName]);
end;

procedure TdQuery.CheckBroker;
begin
  if FBroker = nil then
    raise EdQuery.Create('Broker not assigned.');
end;

procedure TdQuery.CheckConnection;
begin
  if FConnection = nil then
    raise EdQuery.Create('Connection not assigned.');
end;

function TdQuery.GetActive: Boolean;
begin
  CheckBroker;
  Result := FBroker.Active;
end;

function TdQuery.GetBOF: Boolean;
begin
  CheckBroker;
  Result := FBroker.BOF;
end;

function TdQuery.GetDataSet: TDataSet;
begin
  CheckBroker;
  Result := FBroker.DataSet;
end;

function TdQuery.GetDataSource: TDataSource;
begin
  CheckBroker;
  Result := FBroker.DataSource;
end;

function TdQuery.GetEOF: Boolean;
begin
  CheckBroker;
  Result := FBroker.EOF;
end;

function TdQuery.GetFieldDefs: TFieldDefs;
begin
  CheckBroker;
  Result := FBroker.FieldDefs;
end;

function TdQuery.GetFields: TFields;
begin
  CheckBroker;
  Result := FBroker.Fields;
end;

function TdQuery.GetModified: Boolean;
begin
  CheckBroker;
  Result := FBroker.Modified;
end;

function TdQuery.GetParams: TParams;
begin
  CheckBroker;
  Result := FBroker.Params;
end;

function TdQuery.GetPosition: Int64;
begin
  CheckBroker;
  Result := FBroker.Position;
end;

function TdQuery.GetSQL: TStrings;
begin
  CheckBroker;
  Result := FBroker.SQL;
end;

function TdQuery.GetState: TDataSetState;
begin
  CheckBroker;
  Result := FBroker.State;
end;

procedure TdQuery.SetActive(const AValue: Boolean);
begin
  CheckBroker;
  FBroker.Active := AValue;
end;

procedure TdQuery.SetDataSource(AValue: TDataSource);
begin
  CheckBroker;
  FBroker.DataSource := AValue;
end;

procedure TdQuery.SetPosition(const AValue: Int64);
begin
  CheckBroker;
  FBroker.Position := AValue;
end;

function TdQuery.ApplyUpdates: T1;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  Connection.Logger.Log(ltCustom, 'Trying Query.ApplyUpdates');
  FBroker.ApplyUpdates;
end;

function TdQuery.CancelUpdates: T1;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  Connection.Logger.Log(ltCustom, 'Trying Query.CancelUpdates');
  FBroker.CancelUpdates;
end;

function TdQuery.Apply: T1;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  Connection.Logger.Log(ltCustom, 'Trying Query.Apply');
  FBroker.Apply;
end;

function TdQuery.ApplyRetaining: T1;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  Connection.Logger.Log(ltCustom, 'Trying Query.ApplyRetaining');
  FBroker.ApplyRetaining;
end;

function TdQuery.Undo: T1;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  Connection.Logger.Log(ltCustom, 'Trying Query.Undo');
  FBroker.Undo;
end;

function TdQuery.UndoRetaining: T1;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  Connection.Logger.Log(ltCustom, 'Trying Query.UndoRetaining');
  FBroker.UndoRetaining;
end;

function TdQuery.Append: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Append;
end;

function TdQuery.Insert: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Insert;
end;

function TdQuery.Edit: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Edit;
end;

function TdQuery.Cancel: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Cancel;
end;

function TdQuery.Delete: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Delete;
end;

function TdQuery.Open: T1;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  Connection.Logger.Log(ltSQL, Trim(SQL.Text));
  FBroker.Open;
end;

function TdQuery.Close: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Close;
end;

function TdQuery.Refresh: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Refresh;
end;

function TdQuery.First: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.First;
end;

function TdQuery.Prior: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Prior;
end;

function TdQuery.Next: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Next;
end;

function TdQuery.Last: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Last;
end;

function TdQuery.Post: T1;
begin
  Result := FBroker;
  CheckBroker;
  FBroker.Post;
end;

function TdQuery.Execute: T1;
var
  S: string;
begin
  Result := FBroker;
  CheckBroker;
  CheckConnection;
  S := Trim(SQL.Text);
  dParameterizeSQL(S, Params);
  Connection.Logger.Log(ltSQL, S);
  FBroker.Execute;
end;

function TdQuery.RowsAffected: Int64;
begin
  CheckBroker;
  Result := FBroker.RowsAffected;
end;

function TdQuery.Locate(const AKeyFields: string; const AKeyValues: Variant;
  const AOptions: TLocateOptions): Boolean;
begin
  CheckBroker;
  Result := FBroker.Locate(AKeyFields, AKeyValues, AOptions);
end;

function TdQuery.Param(const AName: string): TParam;
begin
  CheckBroker;
  Result := FBroker.Param(AName);
end;

function TdQuery.Field(const AName: string): TField;
begin
  CheckBroker;
  Result := FBroker.Field(AName);
end;

function TdQuery.FieldDef(const AName: string): TFieldDef;
begin
  CheckBroker;
  Result := FBroker.FieldDef(AName);
end;

function TdQuery.Count: Int64;
begin
  CheckBroker;
  Result := FBroker.Count;
end;

function TdQuery.GetBookmark: TBookmark;
begin
  CheckBroker;
  Result := FBroker.Bookmark;
end;

procedure TdQuery.GotoBookmark(ABookmark: TBookmark);
begin
  CheckBroker;
  FBroker.GotoBookmark(ABookmark);
end;

{ TdEntityQuery }

constructor TdEntityQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEntity := CreateEntity;
end;

destructor TdEntityQuery.Destroy;
begin
  FreeEntity;
  inherited Destroy;
end;

function TdEntityQuery.CreateEntity: T3;
begin
  Result := T3.Create;
end;

procedure TdEntityQuery.FreeEntity;
begin
  FreeAndNil(FEntity);
end;

function TdEntityQuery.GetFields: T1;
begin
  Result := Broker;
  Connection.Logger.Log(ltCustom, 'Trying Query.GetFields');
  dGetFields(FEntity, Fields);
end;

function TdEntityQuery.SetFields: T1;
begin
  Result := Broker;
  Connection.Logger.Log(ltCustom, 'Trying Query.SetFields');
  dSetFields(FEntity, Fields);
end;

function TdEntityQuery.GetParams: T1;
begin
  Result := Broker;
  Connection.Logger.Log(ltCustom, 'Trying Query.GetParams');
  dGetParams(FEntity, Params);
end;

function TdEntityQuery.SetParams: T1;
begin
  Result := Broker;
  Connection.Logger.Log(ltCustom, 'Trying Query.SetParams');
  dSetParams(FEntity, Params);
end;

end.

