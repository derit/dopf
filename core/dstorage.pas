(*
  Duall Sistemas, Object Storage Classes

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dStorage;

{$i dopf.inc}

interface

uses
  dClasses, dUtils, dSqlBuilder, SysUtils, TypInfo, FGL;

type
  EdStorage = class(EdException);

  { TdGStorage }

  generic TdGStorage<T1, T2, T3> = class(TdComponent)
  private type
    TTable = specialize TdGTable<T3>;
    TSelectBuilder = specialize TdGSelectBuilder<TTable>;
    TInsertBuilder = specialize TdGInsertBuilder<TTable>;
    TUpdateBuilder = specialize TdGUpdateBuilder<TTable>;
    TDeleteBuilder = specialize TdGDeleteBuilder<TTable>;
  private
    FConnection: T1;
    FQuery: T2;
    FTableName: string;
  public type
    TEntities = specialize TFPGObjectList<T3>;
  protected
    procedure CheckEntity({%H-}AEntity: T3);
    procedure CheckEntities({%H-}AEntities: TEntities);
    function InternalFind({%H-}ATable: TTable; {%H-}AEntity: T3;
      const ACondition: string): Boolean;
    procedure PopulateEntities({%H-}AEntities: TEntities); virtual;
    procedure SetSql(const ASql: string); virtual;
    procedure SetParams({%H-}AEntity: T3); virtual;
    procedure GetFields({%H-}AEntity: T3); virtual;
    property Query: T2 read FQuery;
  public
    constructor Create(AConnection: T1;
      const ATableName: string); reintroduce; virtual;
    procedure Conditions({%H-}AEntity: T3; out AParams: string); overload;
    procedure Conditions({%H-}AEntity: T3; out AParams: string;
      const AIgnoredParams: array of string); overload;
    function Get(AEntity: T3): Boolean;
    function Find(AEntity: T3; const ACondition: string): Boolean; overload;
    function Find(AEntity: T3; AEntities: TEntities;
      const ACondition: string): Boolean; overload;
    function List(AEntity: T3; AEntities: TEntities;
      const ASql: string = ''): Boolean;
    function List(AEntities: TEntities; const ASql: string = ''): Boolean;
    procedure Add(AEntity: T3;
      {%H-}const AIgnorePrimaryKeys: Boolean = True); virtual;
    procedure Modify(AEntity: T3;
      {%H-}const AIgnorePrimaryKeys: Boolean = True); virtual;
    procedure Remove(AEntity: T3;
      {%H-}const AIgnoreProperties: Boolean = True); virtual;
    procedure Empty; virtual;
    procedure Apply; virtual;
    procedure Discard; virtual;
    property Connection: T1 read FConnection;
  end;

implementation

{ TdGStorage }

constructor TdGStorage.Create(AConnection: T1; const ATableName: string);
begin
  inherited Create(AConnection);
  FConnection := AConnection;
  FQuery := T2.Create(FConnection);
  FTableName := ATableName;
end;

procedure TdGStorage.CheckEntity(AEntity: T3);
begin
  if AEntity = nil then
    raise EdStorage.Create('Entity must not be nil.');
  if T3 = TObject then
    raise EdStorage.Create('Entity must be TObject directly.');
end;

procedure TdGStorage.CheckEntities(AEntities: TEntities);
begin
  if AEntities = nil then
    raise EdStorage.Create('Entities must not be nil.');
end;

procedure TdGStorage.Empty;
begin
  SetSql('delete from ' + FTableName);
  FQuery.Execute;
end;

function TdGStorage.InternalFind(ATable: TTable; AEntity: T3;
  const ACondition: string): Boolean;
var
  FS: string = '';
begin
  TSelectBuilder.MakeFields(ATable, FS, True);
  SetSql('select ' + FS + ' from ' + FTableName);
  if ACondition <> '' then
    FQuery.SQL.Add('where ' + ACondition);
  SetParams(AEntity);
  FQuery.Open;
  Result := FQuery.Count > 0;
  if Result then
    GetFields(AEntity);
end;

procedure TdGStorage.PopulateEntities(AEntities: TEntities);
var
  E: T3;
begin
  FQuery.First;
  while not FQuery.EOF do
  begin
    E := T3.Create;
    GetFields(E);
    AEntities.Add(E);
    FQuery.Next;
  end;
end;

procedure TdGStorage.SetSql(const ASql: string);
begin
  FQuery.Close;
  FQuery.SQL.Text := ASql;
end;

procedure TdGStorage.SetParams(AEntity: T3);
begin
  dUtils.dSetParams(AEntity, FQuery.Params);
end;

procedure TdGStorage.GetFields(AEntity: T3);
begin
  dUtils.dGetFields(AEntity, FQuery.Fields);
end;

procedure TdGStorage.Conditions(AEntity: T3; out AParams: string);
var
  C, I: Integer;
  N: ShortString;
  PL: PPropList = nil;
begin
  C := GetPropList(PTypeInfo(AEntity.ClassInfo), PL);
  AParams := '';
  if Assigned(PL) then
    try
      for I := 0 to Pred(C) do
      begin
        N := PL^[I]^.Name;
        AParams += N + ' = :' + N + ' and ';
      end;
      SetLength(AParams, Length(AParams) - 5);
      StrLower(PChar(AParams));
    finally
      FreeMem(PL);
    end;
end;

procedure TdGStorage.Conditions(AEntity: T3; out AParams: string;
  const AIgnoredParams: array of string);
var
  B: Boolean;
  N: ShortString;
  C, I, J: Integer;
  PL: PPropList = nil;
begin
  C := GetPropList(PTypeInfo(AEntity.ClassInfo), PL);
  AParams := '';
  if Assigned(PL) then
    try
      for I := 0 to Pred(C) do
      begin
        N := PL^[I]^.Name;
        B := False;
        for J := Low(AIgnoredParams) to High(AIgnoredParams) do
          if ShortCompareText(N, AIgnoredParams[J]) = 0 then
          begin
            B := True;
            Break;
          end;
        if B then
          Continue;
        AParams += N + ' = :' + N + ' and ';
      end;
      SetLength(AParams, Length(AParams) - 5);
      StrLower(PChar(AParams));
    finally
      FreeMem(PL);
    end;
end;

function TdGStorage.Get(AEntity: T3): Boolean;
var
  T: TTable;
  PS: string = '';
begin
  CheckEntity(AEntity);
  T := TTable.Create;
  try
    TDeleteBuilder.MakeParams(T, PS, True);
    Result := InternalFind(T, AEntity, PS);
  finally
    T.Free;
  end;
end;

function TdGStorage.Find(AEntity: T3; const ACondition: string): Boolean;
var
  T: TTable;
begin
  CheckEntity(AEntity);
  T := TTable.Create;
  try
    Result := InternalFind(T, AEntity, ACondition);
  finally
    T.Free;
  end;
end;

{$NOTES OFF}
function TdGStorage.Find(AEntity: T3; AEntities: TEntities;
  const ACondition: string): Boolean;
var
  T: TTable;
begin
  CheckEntity(AEntity);
  CheckEntities(AEntities);
  T := TTable.Create;
  try
    Result := InternalFind(T, AEntity, ACondition);
    if Result then
      PopulateEntities(AEntities);
  finally
    T.Free;
  end;
end;
{$NOTES ON}

{$NOTES OFF}
function TdGStorage.List(AEntity: T3; AEntities: TEntities;
  const ASql: string): Boolean;
var
  T: TTable;
  FS: string = '';
begin
  CheckEntity(AEntity);
  CheckEntities(AEntities);
  T := TTable.Create;
  try
    if ASql = '' then
    begin
      TSelectBuilder.MakeFields(T, FS, True);
      SetSql('select ' + FS + ' from ' + FTableName);
    end
    else
      SetSql(ASql);
    SetParams(AEntity);
    FQuery.Open;
    Result := FQuery.Count > 0;
    if Result then
      GetFields(AEntity);
    if Result then
      PopulateEntities(AEntities);
  finally
    T.Free;
  end;
end;
{$NOTES ON}

{$NOTES OFF}
function TdGStorage.List(AEntities: TEntities; const ASql: string): Boolean;
var
  T: TTable;
  FS: string = '';
begin
  CheckEntities(AEntities);
  T := TTable.Create;
  try
    if ASql = '' then
    begin
      TSelectBuilder.MakeFields(T, FS, True);
      SetSql('select ' + FS + ' from ' + FTableName);
    end
    else
      SetSql(ASql);
    FQuery.Open;
    Result := FQuery.Count > 0;
    if Result then
      PopulateEntities(AEntities);
  finally
    T.Free;
  end;
end;
{$NOTES ON}

{$NOTES OFF}
procedure TdGStorage.Add(AEntity: T3; const AIgnorePrimaryKeys: Boolean);
var
  S: string = '';
  B: TInsertBuilder;
begin
  CheckEntity(AEntity);
  B := TInsertBuilder.Create(nil);
  try
    B.Table.Name := FTableName;
    B.Build(S, AIgnorePrimaryKeys);
    SetSql(S);
    SetParams(AEntity);
    FQuery.Execute;
  finally
    B.Free;
  end;
end;
{$NOTES ON}

{$NOTES OFF}
procedure TdGStorage.Modify(AEntity: T3; const AIgnorePrimaryKeys: Boolean);
var
  S: string = '';
  B: TUpdateBuilder;
begin
  CheckEntity(AEntity);
  B := TUpdateBuilder.Create(nil);
  try
    B.Table.Name := FTableName;
    B.Build(S, AIgnorePrimaryKeys);
    SetSql(S);
    SetParams(AEntity);
    FQuery.Execute;
  finally
    B.Free;
  end;
end;
{$NOTES ON}

{$NOTES OFF}
procedure TdGStorage.Remove(AEntity: T3; const AIgnoreProperties: Boolean);
var
  S: string = '';
  B: TDeleteBuilder;
begin
  CheckEntity(AEntity);
  B := TDeleteBuilder.Create(nil);
  try
    B.Table.Name := FTableName;
    B.Build(S, AIgnoreProperties);
    SetSql(S);
    SetParams(AEntity);
    FQuery.Execute;
  finally
    B.Free;
  end;
end;
{$NOTES ON}

procedure TdGStorage.Apply;
begin
  FQuery.Apply;
end;

procedure TdGStorage.Discard;
begin
  FQuery.Undo;
end;

end.

