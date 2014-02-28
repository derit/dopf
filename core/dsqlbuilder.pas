(*
  Duall Sistemas, SQL Builder Classes

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dSqlBuilder;

{$i dopf.inc}

interface

uses
  dClasses, Classes, SysUtils, TypInfo;

type
  EdTable = class(EdException);

  { TdGTable }

  generic TdGTable<T> = class(TdObject)
  private
    FPrimaryKeys: TStrings;
    FPropCount: Integer;
    FPropList: PPropList;
    FName: string;
    procedure SetName(const AValue: string);
  protected
    property PropCount: Integer read FPropCount;
    property PropList: PPropList read FPropList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Name: string read FName write SetName;
    property PrimaryKeys: TStrings read FPrimaryKeys;
  end;

  { TdSqlBuilder }

  TdSqlBuilder = class(TdComponent)
  public
    procedure Build(out ASql: string;
      const ACondition: Boolean = True); virtual; abstract;
  end;

  { TdGSqlBuilder }

  generic TdGSqlBuilder<T> = class(TdSqlBuilder)
  private
    FTable: T;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Table: T read FTable write FTable;
  end;

  { TdGSelectBuilder }

  generic TdGSelectBuilder<T> = class(specialize TdGSqlBuilder<T>)
  public
    procedure MakeFields(out AFields: string;
      const AUseWildcard: Boolean); virtual;
    procedure Build(out ASql: string;
      const AUseWildcard: Boolean = True); override;
  end;

  { TdGInsertBuilder }

  generic TdGInsertBuilder<T> = class(specialize TdGSqlBuilder<T>)
  public
    procedure MakeFields(out AFields, AParams: string;
      const AIgnorePrimaryKeys: Boolean); virtual;
    procedure Build(out ASql: string;
      const AIgnorePrimaryKeys: Boolean = True); override;
  end;

  { TdGUpdateBuilder }

  generic TdGUpdateBuilder<T> = class(specialize TdGSqlBuilder<T>)
  public
    procedure MakeFields(out AFields, AParams: string;
      const AIgnorePrimaryKeys: Boolean); virtual;
    procedure Build(out ASql: string;
      const AIgnorePrimaryKeys: Boolean = True); override;
  end;

  { TdGDeleteBuilder }

  generic TdGDeleteBuilder<T> = class(specialize TdGSqlBuilder<T>)
  public
    procedure MakeParams(out AParams: string;
      const AIgnoreProperties: Boolean); virtual;
    procedure Build(out ASql: string;
      const AIgnoreProperties: Boolean = True); override;
  end;

var
  dDefaultPrimaryKeyName: ShortString = 'id';

implementation

{ TdGTable }

constructor TdGTable.Create;
begin
  inherited Create;
  FPropCount := GetPropList(PTypeInfo(T.ClassInfo), FPropList);
  FPrimaryKeys := TStringList.Create;
  FPrimaryKeys.Add(dDefaultPrimaryKeyName);
end;

destructor TdGTable.Destroy;
begin
  if Assigned(FPropList) then
    FreeMem(FPropList);
  FPrimaryKeys.Free;
  inherited Destroy;
end;

procedure TdGTable.SetName(const AValue: string);
begin
  if Trim(AValue) = '' then
    raise EdTable.Create('Name must not be empty.');
  StrLower(PChar(AValue));
  if AValue <> FName then
    FName := AValue;
end;

{ TdGSqlBuilder }

constructor TdGSqlBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTable := T.Create;
end;

destructor TdGSqlBuilder.Destroy;
begin
  FTable.Free;
  inherited Destroy;
end;

{ TdGSelectBuilder }

procedure TdGSelectBuilder.MakeFields(out AFields: string;
  const AUseWildcard: Boolean);
var
  N: string;
  I: Integer;
begin
  if AUseWildcard then
  begin
    AFields := '*';
    Exit;
  end;
  if (FTable = nil) or (not Assigned(FTable.PropList)) then
    Exit;
  for I := 0 to Pred(FTable.PropCount) do
  begin
    N := FTable.PropList^[I]^.Name;
    N += ', ';
    AFields += N;
  end;
  SetLength(AFields, Length(AFields) - 2);
  StrLower(PChar(AFields));
end;

procedure TdGSelectBuilder.Build(out ASql: string; const AUseWildcard: Boolean);
var
  FS: string;
begin
  MakeFields(FS, AUseWildcard);
  ASql := 'select ' + FS + ' from ' + FTable.Name;
end;

{ TdGInsertBuilder }

procedure TdGInsertBuilder.MakeFields(out AFields, AParams: string;
  const AIgnorePrimaryKeys: Boolean);
var
  N: string;
  I: Integer;
begin
  AFields := '';
  AParams := '';
  if (FTable = nil) or (not Assigned(FTable.PropList)) then
    Exit;
  for I := 0 to Pred(FTable.PropCount) do
  begin
    N := FTable.PropList^[I]^.Name;
    if AIgnorePrimaryKeys and (FTable.PrimaryKeys.IndexOf(N) > -1) then
      Continue;
    N += ', ';
    AFields += N;
    AParams += ':' + N;
  end;
  SetLength(AFields, Length(AFields) - 2);
  SetLength(AParams, Length(AParams) - 2);
  StrLower(PChar(AFields));
  StrLower(PChar(AParams));
end;

procedure TdGInsertBuilder.Build(out ASql: string;
  const AIgnorePrimaryKeys: Boolean);
var
  FS, PS: string;
begin
  MakeFields(FS, PS, AIgnorePrimaryKeys);
  ASql := 'insert into ' + FTable.Name + ' (' + FS + ') ' + 'values (' + PS + ')';
end;

{ TdGUpdateBuilder }

procedure TdGUpdateBuilder.MakeFields(out AFields, AParams: string;
  const AIgnorePrimaryKeys: Boolean);
var
  N, P: string;
  I, X: Integer;
begin
  AFields := '';
  AParams := '';
  if (FTable = nil) or (not Assigned(FTable.PropList)) then
    Exit;
  for I := 0 to Pred(FTable.PropCount) do
  begin
    N := FTable.PropList^[I]^.Name;
    X := FTable.PrimaryKeys.IndexOf(N);
    if X > -1 then
    begin
      P := FTable.PrimaryKeys[X];
      AParams += P + ' = :' + P + ' and ';
      if AIgnorePrimaryKeys then
        Continue;
    end;
    AFields += N + ' = :' + N + ', ';
  end;
  SetLength(AFields, Length(AFields) - 2);
  StrLower(PChar(AFields));
  SetLength(AParams, Length(AParams) - 5);
  StrLower(PChar(AParams));
end;

procedure TdGUpdateBuilder.Build(out ASql: string;
  const AIgnorePrimaryKeys: Boolean);
var
  FS, PS: string;
begin
  MakeFields(FS, PS, AIgnorePrimaryKeys);
  ASQL := 'update ' + FTable.Name + ' set ' + FS + ' where ' + PS;
end;

{ TdGDeleteBuilder }

procedure TdGDeleteBuilder.MakeParams(out AParams: string;
  const AIgnoreProperties: Boolean);
var
  N, P: string;
  I, X: Integer;
begin
  AParams := '';
  if (FTable = nil) or (not Assigned(FTable.PropList)) then
    Exit;
  for I := 0 to Pred(FTable.PropCount) do
  begin
    N := FTable.PropList^[I]^.Name;
    X := FTable.PrimaryKeys.IndexOf(N);
    if X > -1 then
    begin
      P := FTable.PrimaryKeys[X];
      AParams += P + ' = :' + P + ' and ';
    end
    else
      if not AIgnoreProperties then
        AParams += N + ' = :' + N + ' and ';
  end;
  SetLength(AParams, Length(AParams) - 5);
  StrLower(PChar(AParams));
end;

procedure TdGDeleteBuilder.Build(out ASql: string;
  const AIgnoreProperties: Boolean);
var
  PS: string;
begin
  MakeParams(PS, AIgnoreProperties);
  ASQL := 'delete from ' + FTable.Name + ' where ' + PS;
end;

end.

