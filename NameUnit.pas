unit NameUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  {$interfaces corba}
  IName = interface
    property Name: string;
  end;
implementation

end.

