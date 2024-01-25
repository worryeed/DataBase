unit ComponentsPositionAndSizeCalculatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TPositionSizeResultArray = array of integer;

  ComponentsPositionAndSizeCalculator = class
    public
    class function GetTablePositionSize(form: TForm; Padding: integer): TPositionSizeResultArray;static;
    class function GetComponentsPositionSize(form: TForm; Padding, Space: integer; HeightPrecent: real; Count: integer): TPositionSizeResultArray;static;
  end;

implementation

class function ComponentsPositionAndSizeCalculator.GetTablePositionSize(form: TForm; Padding: integer): TPositionSizeResultArray;
var
  Wdth, Hght: integer;
  ResultArr: TPositionSizeResultArray;

begin
  if (form <> nil) then
  begin
    setLength(ResultArr, 4);
    Wdth := form.Width - Padding * 2;
    Hght := round(form.Height * 0.63);
    ResultArr[0] := Padding;
    ResultArr[1] := round(form.Height * 0.1);
    ResultArr[2] := Wdth;
    ResultArr[3] := Hght;
    GetTablePositionSize := ResultArr;
  end
  else GetTablePositionSize := nil;
end;

class function ComponentsPositionAndSizeCalculator.GetComponentsPositionSize(form: TForm; Padding, Space: integer; HeightPrecent: real; Count: integer): TPositionSizeResultArray;
var
  Wdth, Hght, i: integer;
  ResultArr: TPositionSizeResultArray;

begin
  if (form <> nil) then
  begin
    setLength(ResultArr, Count+3);
    Wdth := ((form.Width - Padding*2) - (Space * (count-1))) div count;
    Hght := round(form.Height * (6.25/100));
    for i := 0 to Count-1 do
      ResultArr[i] := (i*Wdth)+(i*Space)+Padding;
    ResultArr[Count] := round(form.Height * (HeightPrecent/100));
    ResultArr[Count+1] := Wdth;
    ResultArr[Count+2] := Hght;
    GetComponentsPositionSize := ResultArr;
  end
  else GetComponentsPositionSize := nil;
end;

end.

