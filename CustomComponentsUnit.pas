unit CustomComponentsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, Forms, Controls, StdCtrls, WrapperUnit, InputFieldsWrapperUnit, XMLRead, DOM, Dialogs, Math, Variants, Graphics;

type
  TSortDirection = (Up, Down, None);
  TWrappedTable = class(TWrapper)
  private
    _sortDirection: TSortDirection;
    function GetCountColumn: integer;
    procedure SetCountColumn(newCountColumn: integer);
    function GetCountRow: integer;
    procedure SetCountRow(newCountRow: integer);
    function GetCell(i, j: integer): string;
    procedure SetCell(i, j: integer; value: string);
    function GetRow: integer;
    procedure SetRow(newRow: integer);

    function GetRows(i: integer): TStrings;
    procedure SetRows(i: integer; value: TStrings);

    procedure SortHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure Sort(Table : TStringGrid; const aCol : Integer; aCompare : TStringListSortCompare = nil);
  public
    constructor Create(newX, newY, newWidth, newHeight: integer; newName: string);
    property CountColumn: integer read GetCountColumn write SetCountColumn;
    property CountRow: integer read GetCountRow write SetCountRow;
    property Cells[i, j: integer]: string read GetCell write SetCell;default;
    property Rows[i: integer]: TStrings read GetRows write SetRows;
    property Row: integer read GetRow write SetRow;
    procedure DeleteRow(Index: integer);
    procedure Save(Path: string);
    procedure Load(Path: string);
  end;

  TWrappedEdit = class(TInputFieldsWrapper)
  private
  public
    constructor Create(newX, newY, newWidth, newHeight: integer; newName: string);
  end;

  TWrappedComboBox = class(TInputFieldsWrapper)
  private
    function GetHeight: integer;
    procedure SetHeight(newHeight: integer);
  public
    constructor Create(newX, newY, newWidth, newHeight: integer; newName: string);
    procedure Clear;
    procedure Add(Value: string);
    property Height: integer read GetHeight write SetHeight;
  end;

  TWrappedLabel = class(TWrapper)
  private
    function GetCaprion: string;
    procedure SetCaprion(newCaption: string);
    function GetHeight: integer;
    procedure SetHeight(newHeight: integer);
  public
    constructor Create(newX, newY, newWidth, newHeight: integer; newName: string);
    property Caption: string read GetCaprion write SetCaprion;
    property Height: integer read GetHeight write SetHeight;
  end;

implementation

function FindNode(AParent: TDOMNode; AName: string): TDOMNode;forward;

function CompareDown(List: TStringList; Index1, Index2: integer): integer;forward;

function CompareUp(List: TStringList; Index1, Index2: integer): integer;forward;

{$region Table}

constructor TWrappedTable.Create(newX, newY, newWidth, newHeight: integer; newName: string);
var
  table: TStringGrid;
begin
  table := TStringGrid.Create(nil);
  table.AutoFillColumns:=true;
  table.DefaultRowHeight:=30;
  table.OnHeaderClick:=@SortHeaderClick;
  table.Flat:=True;
  _currentItem := table;
  _sortDirection := None;
  inherited Create(newX, newY, newWidth, newHeight, newName);
end;

procedure TWrappedTable.DeleteRow(Index: integer);
begin
  (_currentItem as TStringGrid).DeleteRow(Index);
end;

function TWrappedTable.GetCountColumn: integer;
begin
  GetCountColumn := (_currentItem as TStringGrid).ColCount;
end;

procedure TWrappedTable.SetCountColumn(newCountColumn: integer);
begin
 (_currentItem as TStringGrid).ColCount := newCountColumn;
end;

function TWrappedTable.GetCountRow: integer;
begin
  GetCountRow := (_currentItem as TStringGrid).RowCount;
end;
procedure TWrappedTable.SetCountRow(newCountRow: integer);
begin
  (_currentItem as TStringGrid).RowCount := newCountRow;
end;

function TWrappedTable.GetCell(i, j: integer): string;
begin
  GetCell := (_currentItem as TStringGrid).Cells[i, j];
end;

procedure TWrappedTable.SetCell(i, j: integer; value: string);
begin
  (_currentItem as TStringGrid).Cells[i, j] := value;
end;

function TWrappedTable.GetRow: integer;
begin
  GetRow := (_currentItem as TStringGrid).Row;
end;

procedure TWrappedTable.SetRow(newRow: integer);
begin
  (_currentItem as TStringGrid).Row := newRow;
end;

function TWrappedTable.GetRows(i: integer): TStrings;
begin
  GetRows := (_currentItem as TStringGrid).Rows[i];
end;

procedure TWrappedTable.SetRows(i: integer; value: TStrings);
begin
  (_currentItem as TStringGrid).Rows[i] := value;
end;

procedure TWrappedTable.Save(Path: string);
begin
  (_currentItem as TStringGrid).SaveToFile(Path);
end;

procedure TWrappedTable.Sort(Table : TStringGrid; const aCol : Integer; aCompare : TStringListSortCompare = nil);
var
  SlSort, SlRow : TStringList;
  i: Integer;
begin
  SlSort := TStringList.Create;
  for i := 1 to Table.RowCount - 1 do
  begin
    SlRow := TStringList.Create;
    SlRow.Assign(Table.Rows[i]);
    SlSort.AddObject(Table.Cells[aCol, i], SlRow);
  end;
  if Assigned(aCompare) then SlSort.CustomSort(aCompare)
  else SlSort.Sort;
  for i := 1 to Table.RowCount - 1 do
  begin
    SlRow := (SlSort.Objects[i-1] as TStringList);
    Table.Rows[i].Assign(SlRow);
    SlRow.Destroy;
  end;
  SlSort.Destroy;
end;

procedure TWrappedTable.SortHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if ((self.CountRow > 1) and (IsColumn)) then
  begin
    if (_sortDirection = Down) then _sortDirection := Up
    else _sortDirection := Down;
    if (_sortDirection = Down) then Sort((_currentItem as TStringGrid), Index, @CompareDown)
    else if (_sortDirection = Up) then Sort((_currentItem as TStringGrid), Index, @CompareUp);
  end;
end;

procedure TWrappedTable.Load(Path: string);
var
  ChildNode, AttrNode: TDOMNode;
  Doc: TXMLDocument;
  rowstr, str: string;
  MaxRow, CountCol, i: integer;
 begin
  MaxRow := 0;
  CountCol := 0;
  ReadXMLFile(Doc, Path);
  ChildNode:= FindNode(Doc.DocumentElement, 'cells');
  if Assigned(ChildNode) then
  begin
    AttrNode:= ChildNode.Attributes.GetNamedItem('cellcount');
    str := AttrNode.NodeValue;
    CountCol := strtoint(str);
  end;
  for i := 1 to CountCol do
  begin
    ChildNode:= FindNode(Doc.DocumentElement, 'cell'+inttostr(i));
    if Assigned(ChildNode) then
    begin
      AttrNode:= ChildNode.Attributes.GetNamedItem('row');
      rowstr := AttrNode.NodeValue;
      if (strtoint(rowstr) > MaxRow) then MaxRow := strtoint(rowstr);
    end;
  end;
  Self.CountRow := MaxRow+1;
  (_currentItem as TStringGrid).LoadFromFile(Path);
  Doc.Destroy;
end;

{$endregion}

{$region Edit}

constructor TWrappedEdit.Create(newX, newY, newWidth, newHeight: integer; newName: string);
var Edit: TEdit;
begin
  Edit := TEdit.Create(nil);
  _currentItem := Edit;
  inherited Create(newX, newY, newWidth, newHeight, newName);
end;

{$endregion}

{$region ComboBox}

constructor TWrappedComboBox.Create(newX, newY, newWidth, newHeight: integer; newName: string);
var ComboBox: TComboBox;
begin
  ComboBox := TComboBox.Create(nil);
  ComboBox.ReadOnly := True;
  _currentItem := ComboBox;
  inherited Create(newX, newY, newWidth, newHeight, newName);
end;

procedure TWrappedComboBox.Clear;
begin
  (_currentItem as TComboBox).Items.Clear;
end;

procedure TWrappedComboBox.Add(Value: string);
begin
  (_currentItem as TComboBox).Items.Add(Value);
end;

procedure TWrappedComboBox.SetHeight(newHeight: integer);
begin
  (_currentItem as TComboBox).Font.Height:=newHeight;
end;

function TWrappedComboBox.GetHeight: integer;
begin
  GetHeight := (_currentItem as TComboBox).Font.Height;
end;

{$endregion}

{$region Label}

constructor TWrappedLabel.Create(newX, newY, newWidth, newHeight: integer; newName: string);
var
  lbl: TLabel;
begin
  lbl := TLabel.Create(nil);
  _currentItem := lbl;
  inherited Create(newX, newY, newWidth, newHeight, newName);
end;

function TWrappedLabel.GetCaprion: string;
begin
 GetCaprion := (_currentItem as TLabel).Caption;
end;

procedure TWrappedLabel.SetCaprion(newCaption: string);
begin
 (_currentItem as TLabel).Caption:=newCaption;
end;

procedure TWrappedLabel.SetHeight(newHeight: integer);
begin
  (_currentItem as TLabel).Font.Height:=newHeight;
  (_currentItem as TLabel).Height:=newHeight;
end;

function TWrappedLabel.GetHeight: integer;
begin
  GetHeight := (_currentItem as TLabel).Font.Height;
end;

{$endregion}

function CompareDown(List: TStringList; Index1, Index2: integer): integer;
var
  Value1, Value2: real;
  Code1, Code2: integer;
  str1, str2: string;
begin
  str1 := List[Index1];
  str2 := List[Index2];
  Val(str1, Value1, Code1);
  Val(str2, Value2, Code2);
  if (Code1 = 0) and (Code2 = 0) then
  begin
    if (value1 = value2) then CompareDown := 0
    else if (Value1 > Value2) then CompareDown := 1
    else CompareDown := -1;
  end
  else
  begin
    if (str1 = str2) then CompareDown := 0
    else if (str1 > str2) then CompareDown := 1
    else CompareDown := -1;
  end;
end;

function CompareUp(List: TStringList; Index1, Index2: integer): integer;
var
  Value1, Value2: real;
  Code1, Code2: integer;
  str1, str2: string;
begin
  str1 := List[Index1];
  str2 := List[Index2];
  Val(str1, Value1, Code1);
  Val(str2, Value2, Code2);
  if (Code1 = 0) and (Code2 = 0) then
  begin
    if (value1 = value2) then CompareUp := 0
    else if (Value1 > Value2) then CompareUp := -1
    else CompareUp := 1;
  end
  else
  begin
    if (str1 = str2) then CompareUp := 0
    else if (str1 > str2) then CompareUp := -1
    else CompareUp := 1;
  end;
end;

function FindNode(AParent: TDOMNode; AName: string): TDOMNode;
var
  node: TDOMNode;
  ex: boolean;
begin
  FindNode := AParent.FindNode(AName);
  ex := false;
  if FindNode = nil then
  begin
    node := AParent.FirstChild;
    while (node <> nil) and (not ex) do
    begin
      FindNode := FindNode(node, AName);
      if Result <> nil then
        ex := true;
      node := node.NextSibling;
    end;
  end;
end;


end.















