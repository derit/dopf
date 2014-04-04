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

const
  dNullParam: string = 'null';
  dNullStr: string = '';
  dNullInt: Integer = 0;
  dNullInt64: Int64 = 0;
  dNullFloat: Double = 0;
  dNullBoolean: Boolean = False;
  dNullDate: TDate = 0;
  dNullTime: TTime = 0;
  dNullDateTime: TDateTime = 0;

procedure dParameterizeSQL(var ASql: string; AParams: TParams;
  const ANulls: Boolean = False);
procedure dGetFields(AObject: TObject; AFields: TFields;
  const ANulls: Boolean = False);
procedure dSetFields(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AFields: TFields; const ANulls: Boolean = False); overload;
procedure dSetFields(AObject: TObject; AFields: TFields;
  const ANulls: Boolean = False); overload;
procedure dGetParams(AObject: TObject; AParams: TParams;
  const ANulls: Boolean = False);
procedure dSetParams(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AParams: TParams; const ANulls: Boolean = False); overload;
procedure dSetParams(AObject: TObject; AParams: TParams;
  const ANulls: Boolean = False); overload;

implementation

procedure dParameterizeSQL(var ASql: string; AParams: TParams;
  const ANulls: Boolean);
var
  V: string;
  P: TParam;
  I: Integer;

  procedure Replace;
  begin
    { TODO: use exactly replace instead of StringReplace. }
    ASql := StringReplace(ASql, ':' + P.Name, V, [rfIgnoreCase]);
  end;

begin
  if not Assigned(AParams) then
    raise EdException.Create('AParams must not be nil.');
  if ANulls then
    for I := 0 to Pred(AParams.Count) do
    begin
      P := AParams[I];
      case P.DataType of
        ftString, ftDate, ftTime, ftDateTime, ftMemo, ftFixedChar, ftGuid:
          if P.IsNull then
            V := dNullParam
          else
            V := QuotedStr(P.AsString);
        ftFloat, ftCurrency, ftBCD:
          if P.IsNull then
            V := dNullParam
          else
          begin
            V := FloatToStr(P.AsFloat);
            V := StringReplace(V, ',', '.', []);
          end;
      else
        if P.IsNull then
          V := dNullParam
        else
          V := P.AsString;
      end;
      Replace;
    end
  else
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
      Replace;
    end;
end;

procedure dGetFields(AObject: TObject; AFields: TFields; const ANulls: Boolean);
var
  I: Integer;
  F: TField;
  PI: PPropInfo;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  if not Assigned(AFields) then
    raise EdException.Create('AFields must not be nil.');
  if ANulls then
    for I := 0 to Pred(AFields.Count) do
    begin
      F := AFields[I];
      PI := GetPropInfo(PTypeInfo(AObject.ClassInfo), F.FieldName);
      if not Assigned(PI) then
        Continue;
      case F.DataType of
        ftFixedWideChar, ftWideString, ftFixedChar, ftString, ftMemo,
          ftGuid, ftWideMemo:
          if F.IsNull then
            SetStrProp(AObject, PI, dNullStr)
          else
            SetStrProp(AObject, PI, F.AsString);
        ftSmallInt, ftInteger, ftAutoInc, ftWord:
          if F.IsNull then
            SetOrdProp(AObject, PI, dNullInt)
          else
            SetOrdProp(AObject, PI, F.AsInteger);
        ftLargeInt:
          if F.IsNull then
            SetInt64Prop(AObject, PI, dNullInt64)
          else
            SetInt64Prop(AObject, PI, F.AsLargeInt);
        ftFloat, ftCurrency, ftBCD:
          if F.IsNull then
            SetFloatProp(AObject, PI, dNullFloat)
          else
            SetFloatProp(AObject, PI, F.AsFloat);
        ftBoolean:
          if F.IsNull then
            SetOrdProp(AObject, PI, Ord(dNullBoolean))
          else
            SetOrdProp(AObject, PI, Ord(F.AsBoolean));
        ftDate:
          if F.IsNull then
            SetFloatProp(AObject, PI, dNullDate)
          else
            SetFloatProp(AObject, PI, Trunc(F.AsDateTime));
        ftTime:
          if F.IsNull then
            SetFloatProp(AObject, PI, dNullTime)
          else
            SetFloatProp(AObject, PI, Frac(F.AsDateTime));
        ftDateTime, ftTimeStamp:
          if F.IsNull then
            SetFloatProp(AObject, PI, dNullDateTime)
          else
            SetFloatProp(AObject, PI, F.AsDateTime);
        ftVariant: SetVariantProp(AObject, PI, F.AsVariant);
      end;
    end
  else
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
        ftDate: SetFloatProp(AObject, PI, Trunc(F.AsDateTime));
        ftTime: SetFloatProp(AObject, PI, Frac(F.AsDateTime));
        ftDateTime, ftTimeStamp: SetFloatProp(AObject, PI, F.AsDateTime);
        ftVariant: SetVariantProp(AObject, PI, F.AsVariant);
      end;
    end;
end;

procedure dSetFields(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AFields: TFields; const ANulls: Boolean);
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
  if ANulls then
    for I := 0 to Pred(APropCount) do
    begin
      PI := APropList^[I];
      F := AFields.FindField(PI^.Name);
      if not Assigned(F) then
        Continue;
      case PI^.PropType^.Kind of
        tkAString:
          begin
            F.AsString := GetStrProp(AObject, PI);
            if F.AsString = dNullStr then
              F.Clear;
          end;
        tkChar:
          begin
            PChar(F.AsString)^ := Char(GetOrdProp(AObject, PI));
            if F.AsString = dNullStr then
              F.Clear;
          end;
        tkInteger:
          begin
            F.AsInteger := GetOrdProp(AObject, PI);
            if F.AsInteger = dNullInt then
              F.Clear;
          end;
        tkInt64, tkQWord:
          begin
            F.AsLargeInt := GetInt64Prop(AObject, PI);
            if F.AsLargeInt = dNullInt64 then
              F.Clear;
          end;
        tkBool:
          begin
            F.AsBoolean := GetOrdProp(AObject, PI) <> 0;
            if F.AsBoolean = dNullBoolean then
              F.Clear;
          end;
        tkFloat:
          case PI^.PropType^.Name of
            'TDate':
              begin
                F.AsDateTime := Trunc(GetFloatProp(AObject, PI));
                if F.AsDateTime = dNullDate then
                  F.Clear;
              end;
            'TTime':
              begin
                F.AsDateTime := Frac(GetFloatProp(AObject, PI));
                if F.AsDateTime = dNullTime then
                  F.Clear;
              end;
            'TDateTime':
              begin
                F.AsDateTime := GetFloatProp(AObject, PI);
                if F.AsDateTime = dNullDateTime then
                  F.Clear;
              end
          else
            F.AsFloat := GetFloatProp(AObject, PI);
            if F.AsFloat = dNullFloat then
              F.Clear;
          end;
        tkEnumeration:
          begin
            F.AsString := GetEnumProp(AObject, PI);
            if F.AsString = dNullStr then
              F.Clear;
          end;
        tkSet:
          begin
            F.AsString := GetSetProp(AObject, PI, False);
            if F.AsString = dNullStr then
              F.Clear;
          end;
      end;
    end
  else
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
            'TDate': F.AsDateTime := Trunc(GetFloatProp(AObject, PI));
            'TTime': F.AsDateTime := Frac(GetFloatProp(AObject, PI));
            'TDateTime': F.AsDateTime := GetFloatProp(AObject, PI);
          else
            F.AsFloat := GetFloatProp(AObject, PI);
          end;
        tkEnumeration: F.AsString := GetEnumProp(AObject, PI);
        tkSet: F.AsString := GetSetProp(AObject, PI, False);
      end;
    end;
end;

procedure dSetFields(AObject: TObject; AFields: TFields; const ANulls: Boolean);
var
  C: Integer;
  PL: PPropList = nil;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      dUtils.dSetFields(PL, C, AObject, AFields, ANulls);
    finally
      FreeMem(PL);
    end;
end;

procedure dGetParams(AObject: TObject; AParams: TParams; const ANulls: Boolean);
var
  I: Integer;
  P: TParam;
  PI: PPropInfo;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  if not Assigned(AParams) then
    raise EdException.Create('AParams must not be nil.');
  if ANulls then
    for I := 0 to Pred(AParams.Count) do
    begin
      P := AParams[I];
      PI := GetPropInfo(PTypeInfo(AObject.ClassInfo), P.Name);
      if not Assigned(PI) then
        Continue;
      case P.DataType of
        ftFixedWideChar, ftWideString, ftFixedChar, ftString, ftMemo, ftGuid,
          ftWideMemo:
          if P.IsNull then
            SetStrProp(AObject, PI, dNullStr)
          else
            SetStrProp(AObject, PI, P.AsString);
        ftSmallInt, ftInteger, ftAutoInc, ftWord:
          if P.IsNull then
            SetOrdProp(AObject, PI, dNullInt)
          else
            SetOrdProp(AObject, PI, P.AsInteger);
        ftLargeInt:
          if P.IsNull then
            SetInt64Prop(AObject, PI, dNullInt64)
          else
            SetInt64Prop(AObject, PI, P.AsLargeInt);
        ftFloat, ftCurrency, ftBCD:
          if P.IsNull then
            SetFloatProp(AObject, PI, dNullFloat)
          else
            SetFloatProp(AObject, PI, P.AsFloat);
        ftBoolean:
          if P.IsNull then
            SetOrdProp(AObject, PI, Ord(dNullBoolean))
          else
            SetOrdProp(AObject, PI, Ord(P.AsBoolean));
        ftDate:
          if P.IsNull then
            SetFloatProp(AObject, PI, dNullDate)
          else
            SetFloatProp(AObject, PI, P.AsDate);
        ftTime:
          if P.IsNull then
            SetFloatProp(AObject, PI, dNullTime)
          else
            SetFloatProp(AObject, PI, P.AsDateTime);
        ftDateTime, ftTimeStamp:
          if P.IsNull then
            SetFloatProp(AObject, PI, dNullDateTime)
          else
            SetFloatProp(AObject, PI, P.AsDateTime);
        ftVariant: SetVariantProp(AObject, PI, P.Value);
      end;
    end
  else
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
        ftDate: SetFloatProp(AObject, PI, P.AsDate);
        ftTime: SetFloatProp(AObject, PI, P.AsDateTime);
        ftDateTime, ftTimeStamp: SetFloatProp(AObject, PI, P.AsDateTime);
        ftVariant: SetVariantProp(AObject, PI, P.Value);
      end;
    end;
end;

procedure dSetParams(APropList: PPropList; const APropCount: Integer;
  AObject: TObject; AParams: TParams; const ANulls: Boolean);
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
  if ANulls then
    for I := 0 to Pred(APropCount) do
    begin
      PI := APropList^[I];
      P := AParams.FindParam(PI^.Name);
      if not Assigned(P) then
        Continue;
      case PI^.PropType^.Kind of
        tkAString:
          begin
            P.AsString := GetStrProp(AObject, PI);
            if P.AsString = dNullStr then
              P.Clear;
          end;
        tkChar:
          begin
            PChar(P.AsString)^ := Char(GetOrdProp(AObject, PI));
            if P.AsString = dNullStr then
              P.Clear;
          end;
        tkInteger:
          begin
            P.AsInteger := GetOrdProp(AObject, PI);
            if P.AsInteger = dNullInt then
              P.Clear;
          end;
        tkInt64, tkQWord:
          begin
            P.AsLargeInt := GetInt64Prop(AObject, PI);
            if P.AsLargeInt = dNullInt64 then
              P.Clear;
          end;
        tkBool:
          begin
            P.AsBoolean := GetOrdProp(AObject, PI) <> 0;
            if P.AsBoolean = dNullBoolean then
              P.Clear;
          end;
        tkFloat:
          case PI^.PropType^.Name of
            'TDate':
              begin
                P.AsDate := Trunc(GetFloatProp(AObject, PI));
                if P.AsDate = dNullDate then
                  P.Clear;
              end;
            'TTime':
              begin
                P.AsTime := Frac(GetFloatProp(AObject, PI));
                if P.AsTime = dNullTime then
                  P.Clear;
              end;
            'TDateTime':
              begin
                P.AsDateTime := GetFloatProp(AObject, PI);
                if P.AsDateTime = dNullDateTime then
                  P.Clear;
              end
          else
            P.AsFloat := GetFloatProp(AObject, PI);
            if P.AsFloat = dNullFloat then
              P.Clear;
          end;
        tkEnumeration:
          begin
            P.AsString := GetEnumProp(AObject, PI);
            if P.AsString = dNullStr then
              P.Clear;
          end;
        tkSet:
          begin
            P.AsString := GetSetProp(AObject, PI, False);
            if P.AsString = dNullStr then
              P.Clear;
          end;
      end;
    end
  else
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

procedure dSetParams(AObject: TObject; AParams: TParams; const ANulls: Boolean);
var
  C: Integer;
  PL: PPropList = nil;
begin
  if not Assigned(AObject) then
    raise EdException.Create('AObject must not be nil.');
  C := GetPropList(PTypeInfo(AObject.ClassInfo), PL);
  if Assigned(PL) then
    try
      dUtils.dSetParams(PL, C, AObject, AParams, ANulls);
    finally
      FreeMem(PL);
    end;
end;

end.

