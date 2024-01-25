unit FindFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, CustomComponentsUnit;

type

  { TFindForm }

  TFindForm = class(TForm)
    CheckBoxFullWord: TCheckBox;
    CheckBoxCaseSensitive: TCheckBox;
    ComboBoxColumns: TComboBox;
    EditFind: TEdit;
    ButtonFind: TSpeedButton;
    ButtonClose: TSpeedButton;
    ImageBorder: TImage;
    procedure FormCreate(Sender: TObject);
    procedure UpdateComboBox(Table: TWrappedTable);
    procedure UpdateBorder;
  private
  public
  end;

var
  FindForm: TFindForm;

implementation

{$R *.lfm}

{ TFindForm }

procedure TFindForm.UpdateComboBox(Table: TWrappedTable);
var i: integer;
begin
  ComboBoxColumns.Items.Clear;
  ComboBoxColumns.Items.Add('Во всей таблице');
  ComboBoxColumns.ItemIndex:=0;
  for i := 0 to Table.CountColumn - 1 do
  begin
    ComboBoxColumns.Items.Add(Table.Cells[i, 0]);
  end;
end;

procedure TFindForm.UpdateBorder;
begin
  ImageBorder.Canvas.Rectangle(0, 0, self.Width, self.Height);
end;

procedure TFindForm.FormCreate(Sender: TObject);
begin
  ImageBorder.SendToBack;
  ImageBorder.Visible:=True;
  ImageBorder.Canvas.Brush.Color := clMenu;
  ImageBorder.Canvas.Pen.Color := clWindowFrame;
  UpdateBorder;
end;

end.

