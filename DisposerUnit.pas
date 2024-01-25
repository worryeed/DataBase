unit DisposerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, WrapperUnit;

type
  TDisposer = class
  private
  public
    function Place(item: TWrapper; form: TForm): boolean;
  end;

implementation

function TDisposer.Place(Item: TWrapper; Form: TForm): boolean;
begin
  if (Form <> nil) and (Item <> nil) and ((Item.X >= 0) and (Item.X+Item.Width <= Form.Width)) and ((Item.Y >= 0) and (Item.Y+Item.Height <= Form.Height)) then
  begin
    Place := True;
    Item.Parent := Form;
  end
  else
    Place := false;
end;

end.
