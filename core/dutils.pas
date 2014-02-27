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
  DB, SysUtils, TypInfo;

procedure GetParameterizeSQL(var ASQL: string; AParams: TParams);
procedure GetFields(AObject: TObject; AFields: TFields);
procedure GetParams(AObject: TObject; AParams: TParams);
procedure SetFields(AObject: TObject; AFields: TFields);
procedure SetParams(AObject: TObject; AParams: TParams);

implementation

procedure GetParameterizeSQL(var ASQL: string; AParams: TParams);
var
  V: string;
  I: Integer;
  P: TParam;
begin
  for I := 0 to Pred(AParams.Count) do
  begin
    P := AParams[I];
    V := P.AsString;
    case P.DataType of
      ftString, ftDate, ftTime, ftDateTime, ftMemo, ftFixedChar, ftGuid:
        V := QuotedStr(V);
      ftCurrency: V := FloatToStr(P.AsFloat);
    end;
    ASQL := StringReplace(ASQL, ':' + P.Name, V, [rfIgnoreCase, rfReplaceAll]);
  end;
end;

procedure GetFields(AObject: TObject; AFields: TFields);
var
  I: Integer;
  F: TField;
  PI: PPropInfo;
begin
  for I := 0 to Pred(AFields.Count) do
  begin
    F := AFields[I];
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

procedure GetParams(AObject: TObject; AParams: TParams);
var
  I: Integer;
  P: TParam;
  PI: PPropInfo;
begin
  for I := 0 to Pred(AParams.Count) do
  begin
    P := AParams[I];
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

procedure SetFields(AObject: TObject; AFields: TFields);
var
  C, I: Integer;
  F: TField;
  PI: PPropInfo;
  PL: PPropList = nil;
begin
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      for I := 0 to Pred(C) do
      begin
        PI := PL^[I];
        F := AFields.FindField(PI^.Name);
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

procedure SetParams(AObject: TObject; AParams: TParams);
var
  C, I: Integer;
  P: TParam;
  PI: PPropInfo;
  PL: PPropList = nil;
begin
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      for I := 0 to Pred(C) do
      begin
        PI := PL^[I];
        P := AParams.FindParam(PI^.Name);
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

end.

