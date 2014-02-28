program demo1;

{$mode objfpc}{$H+}

uses
  dStorage, dSQLdbBroker, dbutils, person, sysutils;

type
  Tstorage = specialize TdGStorage<TdSQLdbConnector, TdSQLdbQuery, TPerson>;

var
  i, per: TPerson;
  pers: Tstorage.TEntities;
  storage: Tstorage;
begin
  storage := Tstorage.Create(dbutils.con, 'person');
  pers := Tstorage.TEntities.Create;
  per := TPerson.Create;
  try
    WriteLn('Empty table');
    storage.Empty;
    storage.Apply;
    WriteLn('Done.');

    WriteLn('Add Silvio Clécio');
    per.Name := 'Silvio Clécio';
    storage.Add(per);
    WriteLn('Done.');

    WriteLn('Add Anonymous');
    per.Id := 1000;
    per.Name := 'Anonymous';
    storage.Add(per, False);
    WriteLn('Done.');

    WriteLn('Add Waldir');
    per.Id := 1001;
    per.Name := 'Waldir';
    storage.Add(per, False);
    WriteLn('Done.');

    WriteLn('Add João Morais');
    per.Name := 'João Morais';
    storage.Add(per);
    WriteLn('Done.');

    WriteLn('Add Sven Barth');
    per.Name := 'Sven Barth';
    storage.Add(per);
    WriteLn('Done.');

    WriteLn('Modify name of Waldir to Waldir Paim');
    per.Id := 1001;
    per.Name := 'Waldir Paim';
    storage.Modify(per);
    WriteLn('Done.');

    WriteLn('Remove Anonymous');
    per.Id := 1000;
    storage.Remove(per);
    WriteLn('Done.');

    WriteLn('Get Waldir Paim');
    per.Id := 1001;
    storage.Get(per);
    WriteLn(per.Id, ', ', per.Name);
    WriteLn('Done.');

    WriteLn('Find Silvio Clécio by name');
    per.Name := 'Silvio Clécio';
    storage.Find(per, 'name = :name');
    WriteLn(per.Id, ', ', per.Name);
    WriteLn('Done.');

    WriteLn('Search for names containing "a"');
    per.Name := '%a%';
    storage.Find(per, pers, 'name like (:name)');
    for i in pers do
      WriteLn(i.Id, ', ', i.Name);
    pers.Clear;
    WriteLn('Done.');

    WriteLn('List all');
    storage.List(pers);
    for i in pers do
      WriteLn(i.Id, ', ', i.Name);
    pers.Clear;
    WriteLn('Done.');

    WriteLn('Search for names containing "a" (order by id DESC)');
    per.Name := '%a%';
    storage.List(per, pers, 'select * from person where name like (:name) order by id desc');
    for i in pers do
      WriteLn(i.Id, ', ', i.Name);
    pers.Clear;
    WriteLn('Done.');

    storage.Apply;
  finally
    per.Free;
    pers.Free;
    storage.Free;
  end;

  ReadLn;
end.

