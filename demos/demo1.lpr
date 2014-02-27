program demo1;

{$mode objfpc}{$H+}

uses
{$IFDEF DEBUG}
  heaptrc,
{$ENDIF}
  dOPF, dUtils, dSQLdbBroker, person, sysutils, pqconnection;

type
  Tcon = specialize TdConnection<TdSQLdbConnectionBroker, TdLogger>;

  Tqry = specialize TdQuery<TdSQLdbQueryBroker, Tcon>;

var
  i: Integer;
  b, e: TDateTime;
  con: Tcon;
  qry: Tqry;
  per: TPerson;
begin
  con := Tcon.Create(nil);
  qry := Tqry.Create(con);
  per := TPerson.Create;
  try
    con.Logger.Active := True;
    con.Logger.Filter := [ltSQL];
    con.Logger.FileName := 'OUTPUT.LOG';
    con.Driver := 'postgresql';
    con.Host := '127.0.0.1';
    con.Database := 'postgres';
    con.User := 'postgres';
    con.Password := 'postgres';
    con.Connect;

    qry.SQL.Text := 'delete from person';
    qry.Execute.Apply;

    qry.SQL.Text := 'insert into person (id, name) values (:id, :name)';
    per.Id := 1;
    per.Name := 'Silvio';
    dUtils.SetParams(per, qry.Params);
    qry.Execute;
    per.Id := 2;
    per.Name := 'Waldir';
    dUtils.SetParams(per, qry.Params);
    qry.Execute.Apply;

    qry.SQL.Text := 'select id, name from person';
    qry.Open.First;
    while not qry.EOF do
    begin
      dUtils.GetFields(per, qry.Fields);
      WriteLn('Record: ', per.id, ', ', per.Name);
      qry.Next;
    end;
    qry.Open.Apply;

    qry.Open.First;
    b := Now;
    for i := 1 to 1000000 do
    begin
      per.Id := 0;
      per.Name := '';
      dUtils.GetFields(per, qry.Fields);
    end;
    e := Now;
    WriteLn('Performance: ', FormatDateTime('hh:nn:ss.zzz', e - b));

    ReadLn;
  finally
    per.Free;
    con.Free;
  end;
{$IFDEF DEBUG}
  DeleteFile('HEAP.TXT');
  SetHeapTraceOutput('HEAP.TXT');
  Sleep(1000);
{$ENDIF}
end.

