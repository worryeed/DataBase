unit InputFieldsWrapperUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, WrapperUnit, Controls;

type
  TInputFieldsWrapper = class(TWrapper)
  private
    function GetHeight: integer;
    procedure SetHeight(newHeight: integer);
    function GetTabOrder: integer;
    procedure SetTabOrder(newTabOrder: integer);
  public
    property Height: integer read GetHeight write SetHeight;
    property TabOrder: integer read GetTabOrder write SetTabOrder;
  end;

implementation

procedure TInputFieldsWrapper.SetHeight(newHeight: integer);
begin
  _currentItem.Font.Height:=newHeight-8;
  _currentItem.Height:=newHeight;
end;

function TInputFieldsWrapper.GetHeight: integer;
begin
  GetHeight := _currentItem.Font.Height;
end;

procedure TInputFieldsWrapper.SetTabOrder(newTabOrder: integer);
begin
  (_currentItem as TWinControl).TabOrder := newTabOrder;
end;

function TInputFieldsWrapper.GetTabOrder: integer;
begin
  GetTabOrder := (_currentItem as TWinControl).TabOrder;
end;

end.
