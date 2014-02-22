program demo1;

{$mode objfpc}{$H+}

uses
  dOPF, dSQLdbBroker, person, sysutils, pqconnection;

var
  i: Integer;
  b, e: TDateTime;
  con: TdConnection;
  qry: TdQuery;
  per: TPerson;
begin
  per := TPerson.Create;
  try
    con := TdConnection.Create(nil, TdSQLdbBroker);
    con.Logger.Active := True;
    con.Logger.Filter := [ltSQL];
    con.Logger.FileName := 'OUTPUT.LOG';
    con.Driver := 'postgresql';
    con.Host := '127.0.0.1';
    con.Database := 'postgres';
    con.User := 'postgres';
    con.Password := 'postgres';
    con.Connect;
    qry := TdQuery.Create(con);

    qry.SQL.Text := 'delete from person';
    qry.Execute.Apply;

    qry.SQL.Text := 'insert into person (id, name) values (:id, :name)';
    per.Id := 1;
    per.Name := 'Silvio';
    qry.SetParams(per).Execute;
    per.Id := 2;
    per.Name := 'Waldir';
    qry.SetParams(per).Execute.Apply;

    qry.SQL.Text := 'select id, name from person';
    qry.Open.GetFields(per).Apply;
    WriteLn('Record: ', per.id, ', ', per.Name);

    qry.Open.First;
    b := Now;
    for i := 1 to 1000000 do
    begin
      per.Id := 0;
      per.Name := '';
      qry.GetFields(per);
    end;
    e := Now;
    WriteLn('Performance: ', FormatDateTime('hh:nn:ss.zzz', e - b));

    ReadLn;
  finally
    per.Free;
    con.Free;
  end;
end.

