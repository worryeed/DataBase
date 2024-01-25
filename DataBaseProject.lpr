program DataBaseProject;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
   {$ENDIF} {$IFDEF HASAMIGA}
  athreads,
   {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, DataBase, CustomComponentsUnit, WrapperUnit, DisposerUnit,
  ComponentsPositionAndSizeCalculatorUnit, NameUnit, InputFieldsWrapperUnit,
  FindFormUnit, LinesFinderUnit;

{ you can add units after this }

{$R *.res}

var
  Table: TWrappedTable;
  params: TParamsArray;
  ResPos: TPositionSizeResultArray;

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TDataBaseForm, DataBaseForm);

  ResPos := ComponentsPositionAndSizeCalculator.GetTablePositionSize(DataBaseForm, 10);

  Table := TWrappedTable.Create(ResPos[0], ResPos[1], ResPos[2], ResPos[3], 'Таблица результатов');
  setLength(params, 4);  params[0] := 'id'; params[1] := 'Участок'; params[2] := 'Кандидат'; params[3] := 'Количество голосов';
  DataBaseForm.AddComponent(Table, params);

  Table := TWrappedTable.Create(ResPos[0], ResPos[1], ResPos[2], ResPos[3], 'Таблица участковых комиссий');
  setLength(params, 5);
  params[0] := 'id'; params[1] := 'Адрес'; params[2] := 'Количество зарегистрированных жителей'; params[3] := 'Телефон'; params[4] := 'Размер поселения';
  DataBaseForm.AddComponent(Table, params);

  Table := TWrappedTable.Create(ResPos[0], ResPos[1], ResPos[2], ResPos[3], 'Таблица кандидатов');
  setLength(params, 5);
  params[0] := 'id'; params[1] := 'ФИО'; params[2] := 'Годовой доход'; params[3] := 'Пол'; params[4] := 'Возраст';
  DataBaseForm.AddComponent(Table, params);

  DataBaseForm.ConnectTables('Таблица результатов', 1, 'Таблица участковых комиссий', 'Таблица результатов и Таблица участковых комиссий');
  DataBaseForm.ConnectTables('Таблица результатов', 2, 'Таблица кандидатов', 'Таблица результатов и Таблица кандидатов');

  //Application.CreateForm(TFindForm, FindForm);
  Application.Run;
end.
