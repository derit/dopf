program demo1;

{$mode objfpc}{$H+}

uses
{$IFDEF DEBUG}
  heaptrc,
{$ENDIF}
  dOPF, dUtils, dSQLdbBroker, person, sysutils, pqconnection;

type
  Tcon = class(TdSQLdbConnector)
  end;

  Tqry = class(specialize TdSQLdbEntity<Tcon, TPerson>)
  end;

var
  i: Integer;
  b, e: TDateTime;
  con: Tcon;
  qry: Tqry;
begin
  con := Tcon.Create(nil);
  qry := Tqry.Create(con);
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
    qry.Entity.Id := 1;
    qry.Entity.Name := 'Silvio';
    qry.SetParams.Execute;
    qry.Entity.Id := 2;
    qry.Entity.Name := 'Waldir';
    qry.SetParams.Execute.Apply;

    qry.SQL.Text := 'select id, name from person';
    qry.Open.First;
    while not qry.EOF do
    begin
      qry.GetFields;
      WriteLn('Record: ', qry.Entity.id, ', ', qry.Entity.Name);
      qry.Next;
    end;
    qry.Open.Apply;

    qry.Open.First;
    b := Now;
    for i := 1 to 1000000 do
    begin
      qry.Entity.Id := 0;
      qry.Entity.Name := '';
      qry.GetFields;
    end;
    e := Now;
    WriteLn('Performance: ', FormatDateTime('hh:nn:ss.zzz', e - b));

    ReadLn;
  finally
    con.Free;
  end;
{$IFDEF DEBUG}
  DeleteFile('HEAP.TXT');
  SetHeapTraceOutput('HEAP.TXT');
  Sleep(1000);
{$ENDIF}
end.

