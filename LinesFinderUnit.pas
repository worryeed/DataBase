unit LinesFinderUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Grids, CustomComponentsUnit, dialogs;

type
  TLinesArray = array of TStringList;
  TLinesFinder = class
    private
      procedure AddItem(var A: TLinesArray; Value: TStrings);
    public
      FullWord: boolean;
      CaseSensitive: boolean;
      function FindLine(Table: TWrappedTable; Column: integer; Search: string): TLinesArray;
      function FindLine(Table: TWrappedTable; Search: string): TLinesArray;overload;
      constructor Create;
      constructor Create(newFullWord, newCaseSensitive: boolean);
  end;

implementation

constructor TLinesFinder.Create;
begin
  FullWord := false;
  CaseSensitive := false;
end;

constructor TLinesFinder.Create(newFullWord, newCaseSensitive: boolean);
begin
  FullWord := newFullWord;
  CaseSensitive := newCaseSensitive;
end;

function TLinesFinder.FindLine(Table: TWrappedTable; Column: integer; Search: string): TLinesArray;
var
  i: integer;
  resArr: TLinesArray;
  Source, Substr: string;
begin
  resArr := nil;
  if (Column >= 0) and (Column < Table.CountColumn) then
    for i := 1 to Table.CountRow-1 do
    begin
      if (CaseSensitive) then
      begin
        Source := Table.Rows[i][Column];
        Substr := Search;
      end
      else
      begin
        Source := AnsiLowerCase(Table.Rows[i][Column]);
        Substr := AnsiLowerCase(Search);
      end;
      if (FullWord) then
      begin
        if (Source = Substr) then
          AddItem(resArr, Table.Rows[i]);
      end
      else
      begin
        if (Pos(Substr, Source) <> 0) then
          AddItem(resArr, Table.Rows[i]);
      end;
    end;
  FindLine := resArr;
end;

function TLinesFinder.FindLine(Table: TWrappedTable; Search: string): TLinesArray;
var
  i, j: integer;
  resArr: TLinesArray;
  Added: boolean;
  Source, Substr: string;
begin
  resArr := nil;
  for i := 1 to Table.CountRow - 1 do
  begin
    Added := False;
    for j := 0 to Table.CountColumn - 1 do
    begin
      if (CaseSensitive) then
      begin
        Source := Table.Rows[i][j];
        Substr := Search;
      end
      else
      begin
        Source := AnsiLowerCase(Table.Rows[i][j]);
        Substr := AnsiLowerCase(Search);
      end;
      if (FullWord) then
      begin
        if (Source = Substr) and (not Added) then
        begin
          AddItem(resArr, Table.Rows[i]);
          Added := True;
        end;
      end
      else
      begin
        if (Pos(Substr, Source) <> 0) and (not Added) then
        begin
          AddItem(resArr, Table.Rows[i]);
          Added := True;
        end;
      end;
    end;
  end;
  FindLine := resArr;
end;

procedure TLinesFinder.AddItem(var A: TLinesArray; Value: TStrings);
begin
  setLength(A, length(A)+1);
  A[length(A)-1] := TStringList.Create;
  A[length(A)-1].Assign(Value);
end;

end.

