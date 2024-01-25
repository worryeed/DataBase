unit DataBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, CustomComponentsUnit, DisposerUnit, WrapperUnit,
  ComponentsPositionAndSizeCalculatorUnit, NameUnit, InputFieldsWrapperUnit,
  FindFormUnit, LinesFinderUnit;

type

  TConnect = class(IName)
  private
    _name: string;
  public
    NameSourceTable: string;
    NameSecondTable: string;
    Column: integer;
    property Name: string read _name write _name;
    constructor Create(newNameSourceTable: string; newColumn: integer; newNameSecondTable, newName: string);
  end;

  TTableArray = array of TWrappedTable;
  TComponentArray = array of TWrapper;
  TConnectArray = array of TConnect;
  TParamsArray = array of string;
  TInpuFieldArray = array of TInputFieldsWrapper;
  TLabelArray = array of TWrappedLabel;

  TFind = class
  private
  public
    class function FindTable(ArrTable: TTableArray; NameTable: string): TWrappedTable; static;
    class function FindComponent(ArrComponent: TComponentArray; NameComponent: string): TWrapper; static;
    class function FindConnect(ArrConnect: TConnectArray; NameSourceTable: string): TConnect; static;
    class function FindConnectByName(ArrConnect: TConnectArray; Name: string): TConnect; static;
  end;

  { TDataBaseForm }

  TDataBaseForm = class(TForm)
    LeftTableButton: TButton;
    AddButton: TButton;
    DeleteButton: TButton;
    ChangeButton: TButton;
    MainMenu: TMainMenu;
    MenuItemChange: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemLoad: TMenuItem;
    RightTableButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure ChangeButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LeftTableButtonClick(Sender: TObject);
    procedure MenuItemFindClick(Sender: TObject);
    procedure MenuItemLoadClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure RightTableButtonClick(Sender: TObject);

    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonFindClick(Sender: TObject);
  private
    _saveFilePath: string;
    _arrTable: TTableArray;
    _arrComponent: TComponentArray;
    _arrConnect: TConnectArray;
    _currentTable: TWrappedTable;
    _currentIndexTable: integer;
    _arrInputFields: TInpuFieldArray;
    _arrLabels: TLabelArray;
    _subForm: TFindForm;
    _saveArr: TLinesArray;
    procedure HideAll;
    function AddToArray(var ComponentArray: TComponentArray; Component: TWrapper): boolean;
    function AddToArray(var ComponentArray: TTableArray; Component: TWrappedTable): boolean;overload;
    function AddToArray(var ComponentArray: TConnectArray; Component: TConnect): boolean;overload;
    procedure UpdateComboBoxes;
    procedure UpdateButtons;
    procedure UpdateInputFields;
    procedure UpdateSizes;
    procedure UpdateSizeButtons;
    procedure UpdateSubForm;
    procedure UpdateTabs;
    procedure ShowCurrentTable;
    procedure DeleteFromArray(var ComponentArray: TComponentArray; ComponentName: string);
    procedure SaveTableToArr;
    procedure LoadTableFromArr;
  public
    function AddComponent(Component: TWrapper): boolean;
    function AddComponent(Table: TWrappedTable; params: TParamsArray): boolean;overload;
    function ConnectTables(NameSourceTable: string; column: integer; NameSecondTable, newName: string): boolean;
  end;

var
  DataBaseForm: TDataBaseForm;

implementation

{$R *.lfm}

{ TFind }

{$region TFind}

class function TFind.FindTable(ArrTable: TTableArray; NameTable: string): TWrappedTable;
var
  i: integer;
begin
  FindTable := nil;
  for i := 0 to length(ArrTable) - 1 do
    if (ArrTable[i].Name = NameTable) then FindTable := ArrTable[i];
end;

class function TFind.FindComponent(ArrComponent: TComponentArray; NameComponent: string): TWrapper;
var
  i: integer;
begin
  FindComponent := nil;
  for i := 0 to length(ArrComponent) - 1 do
    if (ArrComponent[i] <> nil) and (ArrComponent[i].Name = NameComponent) then FindComponent := ArrComponent[i];
end;

class function TFind.FindConnect(ArrConnect: TConnectArray; NameSourceTable: string): TConnect;
var
  i: integer;
begin
  FindConnect := nil;
  for i := 0 to length(ArrConnect) - 1 do
    if (ArrConnect[i].NameSourceTable = NameSourceTable) then
      FindConnect := ArrConnect[i];
end;

class function TFind.FindConnectByName(ArrConnect: TConnectArray; Name: string): TConnect;
var
  i: integer;
begin
  FindConnectByName := nil;
  for i := 0 to length(ArrConnect) - 1 do
    if (ArrConnect[i].Name = Name) then
      FindConnectByName := ArrConnect[i];
end;

{$endregion}

{ TDataBaseForm }

{$region Update}

procedure TDataBaseForm.UpdateTabs;
var i: integer;
begin
  LeftTableButton.TabOrder := 0;
  RightTableButton.TabOrder := 1;
  for i := 0 to length(_arrInputFields) - 1 do
    _arrInputFields[i].TabOrder := i + 2;
  AddButton.TabOrder := length(_arrInputFields) + 2;
  ChangeButton.TabOrder := length(_arrInputFields) + 3;
  DeleteButton.TabOrder := length(_arrInputFields) + 4;
end;

procedure TDataBaseForm.UpdateSubForm;
var
  ResPosSize: TPositionSizeResultArray;
begin
  if (_subForm <> nil) then
  begin
    ResPosSize := ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(self, round(self.Width * 0.2), 0, 4.3, 1);
    _subForm.Width := ResPosSize[2];
    _subForm.Top := self.Top + ResPosSize[1];
    _subForm.Left := self.Left + ResPosSize[0];
    _subForm.UpdateBorder;
  end;
end;

procedure TDataBaseForm.UpdateSizeButtons;
var
  ResPosSize: TPositionSizeResultArray;
begin
  ResPosSize := ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(self, 10, 460, 1.6666, 2);
  LeftTableButton.Left := ResPosSize[0];
  RightTableButton.Left := ResPosSize[1];
  LeftTableButton.Top := ResPosSize[2];
  RightTableButton.Top := ResPosSize[2];
  LeftTableButton.Height := ResPosSize[Length(ResPosSize) - 1];
  RightTableButton.Height := ResPosSize[Length(ResPosSize) - 1];
  LeftTableButton.Width := ResPosSize[Length(ResPosSize) - 2];
  RightTableButton.Width := ResPosSize[Length(ResPosSize) - 2];
  if (ResPosSize[length(ResPosSize) - 1] - 15 > 15) then
  begin
    LeftTableButton.Font.Height := ResPosSize[Length(ResPosSize) - 1] - 15;
    RightTableButton.Font.Height := ResPosSize[Length(ResPosSize) - 1] - 15;
  end
  else
  begin
    LeftTableButton.Font.Height := 15;
    RightTableButton.Font.Height := 15;
  end;
  ResPosSize := ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(self, 10, 70, 87.9166, 3);
  AddButton.Left := ResPosSize[0];
  ChangeButton.Left := ResPosSize[1];
  DeleteButton.Left := ResPosSize[2];
  AddButton.Top := ResPosSize[3];
  ChangeButton.Top := ResPosSize[3];
  DeleteButton.Top := ResPosSize[3];
  AddButton.Height := ResPosSize[Length(ResPosSize) - 1];
  ChangeButton.Height := ResPosSize[Length(ResPosSize) - 1];
  DeleteButton.Height := ResPosSize[Length(ResPosSize) - 1];
  AddButton.Width := ResPosSize[Length(ResPosSize) - 2];
  ChangeButton.Width := ResPosSize[Length(ResPosSize) - 2];
  DeleteButton.Width := ResPosSize[Length(ResPosSize) - 2];
  if (ResPosSize[length(ResPosSize) - 1] - 15 > 15) then
  begin
    AddButton.Font.Height := ResPosSize[Length(ResPosSize) - 1] - 15;
    ChangeButton.Font.Height := ResPosSize[Length(ResPosSize) - 1] - 15;
    DeleteButton.Font.Height := ResPosSize[Length(ResPosSize) - 1] - 15;
  end
  else if (ResPosSize[length(ResPosSize) - 1] - 15 <= 15) then
  begin
    AddButton.Font.Height := 15;
    ChangeButton.Font.Height := 15;
    DeleteButton.Font.Height := 15;
  end;
end;

procedure TDataBaseForm.UpdateSizes;
var
  i: integer;
  ResPosSize: TPositionSizeResultArray;

begin
  if (_currentTable <> nil) then
  begin
    ResPosSize := ComponentsPositionAndSizeCalculator.GetTablePositionSize(DataBaseForm, 10);
    for i := 0 to length(_arrTable) - 1 do
    begin
      _arrTable[i].X := ResPosSize[0];
      _arrTable[i].Y := ResPosSize[1];
      _arrTable[i].Width := ResPosSize[2];
      _arrTable[i].Height := ResPosSize[3];
    end;
    if (_currentTable <> nil) then ResPosSize := ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(self, 10, 30, 80, _currentTable.CountColumn - 1);
    for i := 0 to length(_arrInputFields) - 1 do
    begin
      _arrInputFields[i].X := ResPosSize[i];
      _arrInputFields[i].Y := ResPosSize[length(ResPosSize) - 3];
      _arrInputFields[i].Width := ResPosSize[length(ResPosSize) - 2];
      (_arrInputFields[i] as TInputFieldsWrapper).Height := ResPosSize[length(ResPosSize) - 1];
    end;
    if (_currentTable <> nil) then ResPosSize := ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(self, 10, 30, 74.5, _currentTable.CountColumn - 1);
    for i := 0 to length(_arrLabels) - 1 do
    begin
      _arrLabels[i].X := ResPosSize[i];
      _arrLabels[i].Y := ResPosSize[length(ResPosSize) - 3];
      _arrLabels[i].Width := ResPosSize[length(ResPosSize) - 2];
      if (ResPosSize[length(ResPosSize) - 1] - 15 > 15) then _arrLabels[i].Height := ResPosSize[length(ResPosSize) - 1]-15
      else _arrLabels[i].Height := 15;
    end;
    UpdateSizeButtons;
  end;
end;

procedure TDataBaseForm.UpdateComboBoxes;
var
  i, j: integer;
  SecondTable: TWrappedTable;
  ComboBox: TWrappedComboBox;
begin
  if (_currentTable <> nil) and (_arrInputFields <> nil) then
    for i := 0 to length(_arrConnect) - 1 do
      if (_arrConnect[i].NameSourceTable=_currentTable.Name) then
      begin
        SecondTable := TFind.FindTable(_arrTable, _arrConnect[i].NameSecondTable);
        if (SecondTable <> nil) then
        begin
          ComboBox := (_arrInputFields[_arrConnect[i].Column-1] as TWrappedComboBox);
          ComboBox.Clear;
          for j := 1 to SecondTable.CountRow-1 do
            ComboBox.Add(SecondTable.Cells[0, j]);
        end;
      end;
end;

procedure TDataBaseForm.UpdateInputFields;
var
  i: integer;
  ResPosSize: TPositionSizeResultArray;
  InputFields: TInpuFieldArray;
  Labels: TLabelArray;

begin
  if (_arrInputFields <> nil) then
    for i := 0 to length(_arrInputFields) - 1 do
      if (_arrInputFields[i] <> nil) then
      begin
        _arrInputFields[i].Hide;
        _arrLabels[i].Hide;
      end;

  if (_currentTable <> nil) then
  begin
    setLength(InputFields, _currentTable.CountColumn - 1);
    setLength(Labels, _currentTable.CountColumn - 1);
    ResPosSize := ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(self, 10, 30, 81, _currentTable.CountColumn - 1);
    for i := 0 to length(_arrConnect) - 1 do
    begin
      if (_arrConnect[i].NameSourceTable = _currentTable.Name) and (TFind.FindComponent(_arrComponent, _currentTable.Name + IntToStr(_arrConnect[i].Column) + 'ComboBox') = nil) then
      begin
        InputFields[_arrConnect[i].Column - 1] := TWrappedComboBox.Create(
            ResPosSize[_arrConnect[i].Column - 1],
            ResPosSize[_currentTable.CountColumn-1],
            ResPosSize[length(ResPosSize)-2],
            ResPosSize[length(ResPosSize)-1],
            _currentTable.Name + IntToStr(_arrConnect[i].Column) + 'ComboBox');
      end
      else if (_arrConnect[i].NameSourceTable = _currentTable.Name) then
      begin
        InputFields[_arrConnect[i].Column - 1] := (TFind.FindComponent(_arrComponent, _currentTable.Name + IntToStr(_arrConnect[i].Column) + 'ComboBox') as TInputFieldsWrapper);
      end;
    end;
    for i := 0 to length(InputFields) - 1 do
    begin
      if (InputFields[i] = nil) and (TFind.FindComponent(_arrComponent, _currentTable.Name + IntToStr(i + 1) + 'Edit') = nil) then
      begin
        InputFields[i] := TWrappedEdit.Create(
            ResPosSize[i],
            ResPosSize[_currentTable.CountColumn-1],
            ResPosSize[length(ResPosSize)-2],
            ResPosSize[length(ResPosSize)-1],
            _currentTable.Name + IntToStr(i + 1) + 'Edit');
      end
      else if (InputFields[i] = nil) then
      begin
        InputFields[i] := (TFind.FindComponent(_arrComponent, _currentTable.Name + IntToStr(i + 1) + 'Edit') as TInputFieldsWrapper);
      end;
    end;
    ResPosSize := ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(self, 10, 30, 77, _currentTable.CountColumn - 1);
    for i := 0 to length(Labels) - 1 do
    begin
      if (Labels[i] = nil) and (TFind.FindComponent(_arrComponent, _currentTable.Name + _currentTable.Cells[i+1, 0] + 'Label') = nil) then
      begin
        Labels[i] := TWrappedLabel.Create(
          ResPosSize[i],
          ResPosSize[_currentTable.CountColumn-1],
          ResPosSize[_currentTable.CountColumn],
          30,
          _currentTable.Name + _currentTable.Cells[i+1, 0] + 'Label');
        Labels[i].Caption:=_currentTable.Cells[i+1, 0];
      end
      else if (Labels[i] = nil) then
      begin
        Labels[i] := (TFind.FindComponent(_arrComponent, _currentTable.Name + _currentTable.Cells[i+1, 0] + 'Label') as TWrappedLabel);
      end;
    end;
    _arrLabels := Labels;
    _arrInputFields := InputFields;
    for i := 0 to length(_arrInputFields)-1 do
    begin
      _arrInputFields[i].Show;
      _arrLabels[i].Show;
      AddComponent(_arrInputFields[i]);
      AddComponent(_arrLabels[i]);
    end;
  end;
end;

procedure TDataBaseForm.UpdateButtons;
begin
  LeftTableButton.Enabled := False;
  RightTableButton.Enabled := True;
  if (Length(_arrTable) > 0) then
  begin
    _currentTable := _arrTable[0];
    _currentIndexTable := 0;
    _currentTable.Show;
  end
  else
    RightTableButton.Enabled := False;
end;

{$endregion}

{$region AddToArray}

function TDataBaseForm.AddToArray(var ComponentArray: TComponentArray; Component: TWrapper): boolean;
begin
  AddToArray := False;
  if (TFind.FindComponent(ComponentArray, Component.Name) = nil) then
  begin
    setLength(ComponentArray, length(ComponentArray) + 1);
    ComponentArray[length(ComponentArray) - 1] := Component;
    AddToArray := True;
  end;
end;

function TDataBaseForm.AddToArray(var ComponentArray: TTableArray; Component: TWrappedTable): boolean;
begin
  AddToArray := False;
  if (TFind.FindTable(ComponentArray, Component.Name) = nil) then
  begin
    setLength(ComponentArray, length(ComponentArray) + 1);
    ComponentArray[length(ComponentArray) - 1] := Component;
    AddToArray := True;
  end;
end;

function TDataBaseForm.AddToArray(var ComponentArray: TConnectArray; Component: TConnect): boolean;
begin
  AddToArray := False;
  if (TFind.FindConnect(ComponentArray, Component.Name) = nil) then
  begin
    setLength(ComponentArray, length(ComponentArray) + 1);
    ComponentArray[length(ComponentArray) - 1] := Component;
    AddToArray := True;
  end;
end;

{$endregion}

procedure TDataBaseForm.DeleteFromArray(var ComponentArray: TComponentArray; ComponentName: string);
var
  obj: TObject;
begin
  obj := TFind.FindComponent(ComponentArray, ComponentName);
  FreeAndNil(obj);
end;

procedure TDataBaseForm.HideAll;
var
  i: integer;
begin
  if (length(_arrTable) <> 0) then
    for i := 0 to length(_arrTable) - 1 do
      _arrTable[i].Hide;
end;

procedure TDataBaseForm.ShowCurrentTable;
begin
  HideAll;
  _currentTable.Show;
end;

function TDataBaseForm.ConnectTables(NameSourceTable: string; column: integer; NameSecondTable, newName: string): boolean;
var
  Connect: TConnect;
begin
  Connect := TConnect.Create(NameSourceTable, column, NameSecondTable, newName);
  if (Connect <> nil) then
  begin
    AddToArray(_arrConnect, Connect);
    ConnectTables := True;
    UpdateInputFields;
    UpdateTabs;
  end
  else
    ConnectTables := False;
end;

procedure TDataBaseForm.SaveTableToArr;
var i: integer;
begin
  if (_currentTable <> nil) then
  begin
    if (_saveArr <> nil) then for i := 0 to length(_saveArr) - 1 do _saveArr[i].Destroy;
    SetLength(_saveArr, _currentTable.CountRow-1);
    for i := 0 to length(_saveArr) - 1 do
    begin
      _saveArr[i] := TStringList.Create;
      _saveArr[i].Assign(_currentTable.Rows[i+1]);
    end;
    _saveArr := _saveArr;
  end;
  _saveArr := _saveArr;
end;

procedure TDataBaseForm.LoadTableFromArr;
var i: integer;
  Id: string;
begin
  if (_saveArr <> nil) then
  begin
    if (_currentTable.Row <> 0) then
    begin
      Id := _currentTable.Rows[_currentTable.Row][0];
      for i := 1 to _currentTable.CountRow - 1 do
        if (_currentTable.Rows[i][0] = Id) then _currentTable.Row := i;
    end;
    _currentTable.CountRow := length(_saveArr) + 1;
    for i := 0 to length(_saveArr) - 1 do
    begin
      _currentTable.Rows[i + 1].Destroy;
      _currentTable.Rows[i + 1].Assign(_saveArr[i]);
    end;
    if (_currentTable.Row <> 0) then
      for i := 1 to _currentTable.CountRow - 1 do
        if (_currentTable.Rows[i][0] = Id) then _currentTable.Row := i;
  end;
end;

{$region AddToForm}

function TDataBaseForm.AddComponent(Table: TWrappedTable; params: TParamsArray): boolean;
var
  i: integer;
  ex: boolean;
begin
  ex := False;
  Table.CountColumn := length(params);
  Table.CountRow := 1;
  for i := 0 to length(params) - 1 do
    Table[i, 0] := params[i];
  Table.Hide;
  ex := (ex or (not AddComponent(Table)));
  if (not AddToArray(_arrTable, Table)) then
  begin
    Table.Destroy;
    ex := True;
  end;
  AddComponent := not ex;
  if (length(_arrTable) = 1) then
  begin
    UpdateButtons;
    UpdateInputFields;
    UpdateTabs;
  end;
end;

function TDataBaseForm.AddComponent(Component: TWrapper): boolean;
var
  Disposer: TDisposer;
  ex: boolean;
begin
  if (Component = nil) then AddComponent := False
  else
  begin
    Disposer := TDisposer.Create;
    if (Disposer = nil) then ex := True;
    ex := (TFind.FindComponent(_arrComponent, Component.Name) <> nil);
    if (not ex) then
    begin
      AddComponent := Disposer.Place(Component, self);
      AddToArray(_arrComponent, Component);
    end
    else
      AddComponent := False;
    Disposer.Destroy;
  end;
end;

{$endregion}

{$region Form}

procedure TDataBaseForm.FormCreate(Sender: TObject);
var
  TempStr: string;
  i: integer;
begin
  _saveFilePath := '';
  TempStr := '';
  i := 1;
  while (i <= length(Application.ExeName)) do
  begin
    if (Application.ExeName[i] = '\') then _saveFilePath := TempStr;
    TempStr += Application.ExeName[i];
    i += 1;
  end;
  _saveFilePath := _saveFilePath + '\Saves\';
  UpdateSizeButtons;
  _subForm := TFindForm.Create(Self);
  _subForm.ButtonClose.OnClick := @ButtonCloseClick;
  _subForm.ButtonFind.OnClick := @ButtonFindClick;
end;

procedure TDataBaseForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  for i := 0 to length(_arrComponent)-1 do if (_arrComponent[i] <> nil) then _arrComponent[i].Destroy;
  for i := 0 to length(_arrConnect)-1 do _arrConnect[i].Destroy;
  _subForm.Destroy;
  if (_saveArr <> nil) then for i := 0 to length(_saveArr) - 1 do _saveArr[i].Destroy;
  inherited;
end;

procedure TDataBaseForm.FormResize(Sender: TObject);
begin
  UpdateSizes;
  UpdateSubForm;
end;

procedure TDataBaseForm.FormConstrainedResize(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize);
begin
  MinWidth := 620;
  MinHeight := 360;
  MaxWidth := Monitor.Width;
  MaxHeight:= Monitor.Height;
  UpdateSizes;
  UpdateSubForm;
end;

{$endregion}

{$region Left/Right}

procedure TDataBaseForm.LeftTableButtonClick(Sender: TObject);
begin
  LoadTableFromArr;
  _currentIndexTable := _currentIndexTable - 1;
  _currentTable := _arrTable[_currentIndexTable];
  if (_currentIndexTable = 0) then LeftTableButton.Enabled := False
  else
    LeftTableButton.Enabled := True;
  if (_currentIndexTable = length(_arrTable) - 1) then RightTableButton.Enabled := False
  else
    RightTableButton.Enabled := True;
  ShowCurrentTable;
  UpdateInputFields;
  UpdateComboBoxes;
  UpdateSizes;
  UpdateTabs;
  _subForm.UpdateComboBox(_currentTable);
  SaveTableToArr;
end;

procedure TDataBaseForm.RightTableButtonClick(Sender: TObject);
begin
  LoadTableFromArr;
  _currentIndexTable := _currentIndexTable + 1;
  _currentTable := _arrTable[_currentIndexTable];
  if (_currentIndexTable = 0) then LeftTableButton.Enabled := False
  else
    LeftTableButton.Enabled := True;
  if (_currentIndexTable = length(_arrTable) - 1) then RightTableButton.Enabled := False
  else
    RightTableButton.Enabled := True;
  ShowCurrentTable;
  UpdateInputFields;
  UpdateComboBoxes;
  UpdateSizes;
  UpdateTabs;
  _subForm.UpdateComboBox(_currentTable);
  SaveTableToArr;
end;

{$endregion}

{$region Add/Del/Ch}

procedure TDataBaseForm.AddButtonClick(Sender: TObject);
var
  i, MaxIndex: integer;
  ex: boolean;
begin
  ex := false;
  if (_arrInputFields = nil) then ex := true;
  if (not ex) then
    for i := 0 to length(_arrInputFields) - 1 do
      if (_arrInputFields[i].Caption = '') then ex := True;
  if (not ex) then
  begin
    if (_subForm.Visible) then LoadTableFromArr;
    _currentTable.CountRow := _currentTable.CountRow + 1;
    if (_currentTable.CountRow - 1 = 1) then
      _currentTable.Cells[0, _currentTable.CountRow - 1] := '1'
    else
    begin
      MaxIndex := 0;
      for i := 1 to _currentTable.CountRow-2 do if (MaxIndex < StrToInt(_currentTable.Rows[i][0])) then MaxIndex := StrToInt(_currentTable.Rows[i][0]);
      _currentTable.Cells[0, _currentTable.CountRow - 1] := IntToStr(MaxIndex + 1);
    end;
    for i := 0 to length(_arrInputFields) - 1 do
    begin
      _currentTable.Cells[i + 1, _currentTable.CountRow - 1] := _arrInputFields[i].Caption;
      _arrInputFields[i].Caption := '';
    end;
    _currentTable.Row := _currentTable.CountRow;
    SaveTableToArr;
  end;
end;

procedure TDataBaseForm.DeleteButtonClick(Sender: TObject);
begin
  if (_currentTable <> nil) and (_currentTable.Row <> 0) then
  begin
    if (_subForm.Visible) then LoadTableFromArr;
    _currentTable.DeleteRow(_currentTable.Row);
    _currentTable.Row := _currentTable.CountRow;
    SaveTableToArr;
  end;
end;

procedure TDataBaseForm.ChangeButtonClick(Sender: TObject);
var
  i: integer;
  ex: boolean;
begin
  ex := false;
  if (_arrInputFields = nil) then ex := true;
  if (not ex) then
  begin
    for i := 0 to length(_arrInputFields) - 1 do
      if (_arrInputFields[i].Caption = '') then ex := True;
  end;
  if (_currentTable <> nil) and (_currentTable.Row <> 0) and (not ex) then
  begin
    if (_subForm.Visible) then LoadTableFromArr;
    for i := 0 to length(_arrInputFields) - 1 do
    begin
      _currentTable.Cells[i + 1, _currentTable.Row] := _arrInputFields[i].Caption;
      _arrInputFields[i].Caption := '';
    end;
    SaveTableToArr;
  end;
end;

{$endregion}

{$region Menu}

procedure TDataBaseForm.MenuItemSaveClick(Sender: TObject);
var
  i: integer;
begin
  CreateDir(_saveFilePath);
  for i := 0 to length(_arrTable)-1 do
    _arrTable[i].Save(_saveFilePath+_arrTable[i].Name+'.xml');
end;

procedure TDataBaseForm.MenuItemLoadClick(Sender: TObject);
var
  i: integer;
begin
  if (directoryExists(_saveFilePath)) then
    for i := 0 to length(_arrTable)-1 do
      if FileExists(_saveFilePath+_arrTable[i].Name+'.xml') then
        _arrTable[i].Load(_saveFilePath+_arrTable[i].Name+'.xml');
  UpdateComboBoxes;
  SaveTableToArr;
end;

procedure TDataBaseForm.MenuItemFindClick(Sender: TObject);
begin
  if (_subForm <> nil) and (not _subForm.Visible) then
  begin
    UpdateSubForm;
    SaveTableToArr;
    _subForm.UpdateComboBox(_currentTable);
    _subForm.show;
  end;
end;

{$endregion}

{ TFindForm }

{$region FindForm}

procedure TDataBaseForm.ButtonCloseClick(Sender: TObject);
begin
  _subForm.Hide;
  LoadTableFromArr;
end;

procedure TDataBaseForm.ButtonFindClick(Sender: TObject);
var
  Finder: TLinesFinder;
  resArr: TLinesArray;
  i: integer;
begin
  if (_subForm.EditFind.Text <> '') then
  begin
    LoadTableFromArr;
    Finder := TLinesFinder.Create(_subForm.CheckBoxFullWord.Checked, _subForm.CheckBoxCaseSensitive.Checked);
    if (_subForm.ComboBoxColumns.ItemIndex = 0) then
      resArr := Finder.FindLine(_currentTable, _subForm.EditFind.Text)
    else
      resArr := Finder.FindLine(_currentTable, _subForm.ComboBoxColumns.ItemIndex - 1, _subForm.EditFind.Text);
    Finder.Destroy;
    if (resArr <> nil) then
    begin
      _currentTable.CountRow := length(resArr) + 1;
      for i := 1 to length(resArr) do
      begin
        _currentTable.Rows[i].Assign(resArr[i - 1]);
        resArr[i - 1].Destroy;
      end;
    end
    else _currentTable.CountRow := 1;
  end
  else LoadTableFromArr;

end;

{$endregion}

{ TConnect }

constructor TConnect.Create(newNameSourceTable: string; newColumn: integer; newNameSecondTable, newName: string);
begin
  self.NameSourceTable := newNameSourceTable;
  self.NameSecondTable := newNameSecondTable;
  self.Column := newColumn;
  self.Name := newName;
end;

end.
