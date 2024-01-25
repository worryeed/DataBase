unit WrapperUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, NameUnit, dialogs;

{$interfaces corba}
type
  IWrapper = interface(IName)
    property X: integer;
    property Y: integer;
    property Height: integer;
    property Width: integer;
    property Parent: TWinControl;
    property Caption: string;
    procedure Hide;
    procedure Show;
  end;

type
  TWrapper = class(IWrapper)
  private
    function GetX: integer;
    procedure SetX(newX: integer);
    function GetY: integer;
    procedure SetY(newY: integer);
    function GetWidth: integer;
    procedure SetWidth(newWidth: integer);
    function GetHeight: integer;
    procedure SetHeight(newHeight: integer);
    function GetParent: TWinControl;
    procedure SetParent(newParent: TWinControl);
    function GetCaption: string;
    procedure SetCaption(newCaption: string);
  protected
    _name: string;
    _currentItem: TControl;
  public
    property X: integer read getX write setX;
    property Y: integer read getY write setY;
    property Width: integer read getWidth write setWidth;
    property Height: integer read getHeight write setHeight;
    property Parent: TWinControl read GetParent write SetParent;
    property Name: string read _name write _name;
    property Caption: string read GetCaption write SetCaption;
    procedure Hide;
    procedure Show;
    constructor Create(newX, newY, newWidth, newHeight: integer; newName: string);
    destructor Destroy; override;
  end;

implementation

procedure TWrapper.Hide;
begin
  _currentItem.Hide;
end;

procedure TWrapper.Show;
begin
  _currentItem.Show;
end;

procedure TWrapper.SetParent(newParent: TWinControl);
begin
  _currentItem.Parent := newParent;
end;

function TWrapper.GetParent: TWinControl;
begin
  GetParent := _currentItem.Parent;
end;

procedure TWrapper.SetX(newX: integer);
begin
  _currentItem.Left := newX;
end;

function TWrapper.GetX: integer;
begin
  GetX := _currentItem.Left;
end;

procedure TWrapper.SetY(newY: integer);
begin
  _currentItem.Top := newY;
end;

function TWrapper.GetY: integer;
begin
  GetY := _currentItem.Top;
end;

procedure TWrapper.SetWidth(newWidth: integer);
begin
  _currentItem.Width := newWidth;
end;

function TWrapper.GetWidth: integer;
begin
  GetWidth := _currentItem.Width;
end;

procedure TWrapper.SetHeight(newHeight: integer);
begin
  _currentItem.Height := newHeight;
end;

function TWrapper.GetHeight: integer;
begin
  GetHeight := _currentItem.Height;
end;

procedure TWrapper.SetCaption(newCaption: string);
begin
  _currentItem.Caption := newCaption;
end;

function TWrapper.GetCaption: string;
begin
  GetCaption := _currentItem.Caption;
end;

constructor TWrapper.Create(newX, newY, newWidth, newHeight: integer; newName: string);
begin
  X := newX;
  Y := newY;
  Width := newWidth;
  Height := newHeight;
  Name := newName;
end;

destructor TWrapper.Destroy;
begin
  _currentItem.Destroy;
  inherited Destroy;
end;

end.
