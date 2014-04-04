(*
  Duall Sistemas, Utilities Unit

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit dUtils;

{$i dopf.inc}

interface

uses
  dClasses, DB, SysUtils, TypInfo;

procedure dParameterizeSQL(var ASQL: string; AParams: TParams);
procedure dGetFields(AObject: TObject; AFields: TFields);
procedure dSetFields(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AFields: TFields); overload;
procedure dSetFields(AObject: TObject; AFields: TFields); overload;
procedure dGetParams(AObject: TObject; AParams: TParams);
procedure dSetParams(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AParams: TParams); overload;
procedure dSetParams(AObject: TObject; AParams: TParams); overload;

implementation

procedure dParameterizeSQL(var ASQL: string; AParams: TParams);
var
  V: string;
  P: TParam;
  I: Integer;
begin
  if not Assigned(AParams) then
    raise EdException.Create('AParams must not be nil.');
  for I := 0 to Pred(AParams.Count) do
  begin
    P := AParams[I];
    case P.DataType of
      ftString, ftDate, ftTime, ftDateTime, ftMemo, ftFixedChar, ftGuid:
        V := QuotedStr(P.AsString);
      ftFloat, ftCurrency, ftBCD:
        begin
          V := FloatToStr(P.AsFloat);
          V := StringReplace(V, ',', '.', []);
        end
    else
      V := P.AsString;
    end;
    { TODO: use exactly replace instead of StringReplace. }
    ASQL := StringReplace(ASQL, ':' + P.Name, V, [rfIgnoreCase]);
  end;
end;

procedure dGetFields(AObject: TObject; AFields: TFields);
var
  I: Integer;
  F: TField;
  PI: PPropInfo;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  if not Assigned(AFields) then
    raise EdException.Create('AFields must not be nil.');
  for I := 0 to Pred(AFields.Count) do
  begin
    F := AFields[I];
    PI := GetPropInfo(PTypeInfo(AObject.ClassInfo), F.FieldName);
    if not Assigned(PI) then
      Continue;
    case F.DataType of
      ftFixedWideChar, ftWideString, ftFixedChar, ftString, ftMemo, ftGuid,
        ftWideMemo: SetStrProp(AObject, PI, F.AsString);
      ftSmallInt, ftInteger, ftAutoInc,
        ftWord: SetOrdProp(AObject, PI, F.AsInteger);
      ftLargeInt: SetInt64Prop(AObject, PI, F.AsLargeInt);
      ftFloat, ftCurrency, ftBCD: SetFloatProp(AObject, PI, F.AsFloat);
      ftBoolean: SetOrdProp(AObject, PI, Ord(F.AsBoolean));
      ftDate, ftTime, ftDateTime, ftTimeStamp:
        SetFloatProp(AObject, PI, F.AsDateTime);
      ftVariant: SetVariantProp(AObject, PI, F.AsVariant);
    end;
  end;
end;

procedure dSetFields(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AFields: TFields);
var
  F: TField;
  I: Integer;
  PI: PPropInfo;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  if APropCount < 1 then
    raise EdException.CreateFmt(
      'APropCount must be greater than zero. Probably, you need to publish ' +
      'the properties in the "%s" class.', [AObject.ClassName]);
  if not Assigned(APropList) then
    raise EdException.Create('APropList must not be nil.');
  if not Assigned(AFields) then
    raise EdException.Create('AFields must not be nil.');
  for I := 0 to Pred(APropCount) do
  begin
    PI := APropList^[I];
    F := AFields.FindField(PI^.Name);
    if not Assigned(F) then
      Continue;
    case PI^.PropType^.Kind of
      tkAString: F.AsString := GetStrProp(AObject, PI);
      tkChar: PChar(F.AsString)^ := Char(GetOrdProp(AObject, PI));
      tkInteger: F.AsInteger := GetOrdProp(AObject, PI);
      tkInt64, tkQWord: F.AsLargeInt := GetInt64Prop(AObject, PI);
      tkBool: F.AsBoolean := GetOrdProp(AObject, PI) <> 0;
      tkFloat:
        case PI^.PropType^.Name of
          'TDate', 'TTime', 'TDateTime':
            F.AsDateTime := GetFloatProp(AObject, PI);
        else
          F.AsFloat := GetFloatProp(AObject, PI);
        end;
      tkEnumeration: F.AsString := GetEnumProp(AObject, PI);
      tkSet: F.AsString := GetSetProp(AObject, PI, False);
    end;
  end;
end;

procedure dSetFields(AObject: TObject; AFields: TFields);
var
  C: Integer;
  PL: PPropList = nil;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      dUtils.dSetFields(PL, C, AObject, AFields);
    finally
      FreeMem(PL);
    end;
end;

procedure dGetParams(AObject: TObject; AParams: TParams);
var
  I: Integer;
  P: TParam;
  PI: PPropInfo;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  if not Assigned(AParams) then
    raise EdException.Create('AParams must not be nil.');
  for I := 0 to Pred(AParams.Count) do
  begin
    P := AParams[I];
    PI := GetPropInfo(PTypeInfo(AObject.ClassInfo), P.Name);
    if not Assigned(PI) then
      Continue;
    case P.DataType of
      ftFixedWideChar, ftWideString, ftFixedChar, ftString, ftMemo, ftGuid,
        ftWideMemo: SetStrProp(AObject, PI, P.AsString);
      ftSmallInt, ftInteger, ftAutoInc,
        ftWord: SetOrdProp(AObject, PI, P.AsInteger);
      ftLargeInt: SetInt64Prop(AObject, PI, P.AsLargeInt);
      ftFloat, ftCurrency, ftBCD: SetFloatProp(AObject, PI, P.AsFloat);
      ftBoolean: SetOrdProp(AObject, PI, Ord(P.AsBoolean));
      ftDate, ftTime, ftDateTime, ftTimeStamp:
        SetFloatProp(AObject, PI, P.AsDateTime);
      ftVariant: SetVariantProp(AObject, PI, P.Value);
    end;
  end;
end;

procedure dSetParams(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AParams: TParams);
var
  P: TParam;
  I: Integer;
  PI: PPropInfo;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  if APropCount < 1 then
    raise EdException.CreateFmt(
      'APropCount must be greater than zero. Probably, you need to publish ' +
      'the properties in the "%s" class.', [AObject.ClassName]);
  if not Assigned(APropList) then
    raise EdException.Create('APropList must not be nil.');
  if not Assigned(AParams) then
    raise EdException.Create('AParams must not be nil.');
  for I := 0 to Pred(APropCount) do
  begin
    PI := APropList^[I];
    P := AParams.FindParam(PI^.Name);
    if not Assigned(P) then
      Continue;
    case PI^.PropType^.Kind of
      tkAString: P.AsString := GetStrProp(AObject, PI);
      tkChar: PChar(P.AsString)^ := Char(GetOrdProp(AObject, PI));
      tkInteger: P.AsInteger := GetOrdProp(AObject, PI);
      tkInt64, tkQWord: P.AsLargeInt := GetInt64Prop(AObject, PI);
      tkBool: P.AsBoolean := GetOrdProp(AObject, PI) <> 0;
      tkFloat:
        case PI^.PropType^.Name of
          'TDate': P.AsDate := Trunc(GetFloatProp(AObject, PI));
          'TTime': P.AsTime := Frac(GetFloatProp(AObject, PI));
          'TDateTime': P.AsDateTime := GetFloatProp(AObject, PI);
        else
          P.AsFloat := GetFloatProp(AObject, PI);
        end;
      tkEnumeration: P.AsString := GetEnumProp(AObject, PI);
      tkSet: P.AsString := GetSetProp(AObject, PI, False);
    end;
  end;
end;

procedure dSetParams(AObject: TObject; AParams: TParams);
var
  C: Integer;
  PL: PPropList = nil;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      dUtils.dSetParams(PL, C, AObject, AParams);
    finally
      FreeMem(PL);
    end;
end;

end.

