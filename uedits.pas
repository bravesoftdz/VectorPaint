unit UEdits;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, ExtCtrls, StdCtrls, Spin, Dialogs;

type
  PointerArray = array of Pointer;

type
  TMainEdit = Class
    PropPointer: PointerArray;
    function    GetControlTop(APanel: TWinControl): Integer;
    constructor Create(APanel: TWinControl; APProp: PointerArray); virtual;
  end;

  TMainEditClass = Class of TMainEdit;

  PW_TSE_PenWidth = Class(TMainEdit)
  strict private
    procedure SpinEdit1_OnChange(Sender: TObject);
  public
    constructor Create(APanel: TWinControl; APProp: PointerArray); override;
  end;

  PS_TCB_PenStyle = Class(TMainEdit)
  private
    procedure ComboBox1_DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
    procedure ComboBox1_OnChange(Sender: TObject);
  public
    constructor Create(APanel: TWinControl; APProp: PointerArray); override;
  end;

  BS_TCB_BrushStyle = Class(TMainEdit)
  private
    procedure ComboBox2_DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
    procedure ComboBox2_OnChange(Sender: TObject);
  public
    constructor Create(APanel: TWinControl; APProp: PointerArray); override;
  end;

type
  TControl_Edit = Class
  type
    TObjArray = array of TObject;
    TClassEditArray = array of TMainEditClass;
    PointerToMethod = procedure of object;
  var
    Edit_Array: TObjArray;
    Class_Edit_Array: TClassEditArray;
    MAIN_Invalidate: PointerToMethod;

    procedure Free_All_Edit;
    procedure Add_Edit(ACEdit: TObject);
    procedure Register_Edit(CLEdit: TMainEditClass);
    function  CompareName(AName: String): TMainEditClass;
  end;

var
  ControlEdits: TControl_Edit;

implementation

constructor TMainEdit.Create(APanel: TWinControl; APProp: PointerArray);
begin
  PropPointer := APProp;
  ControlEdits.Add_Edit(Self);
end;

function TMainEdit.GetControlTop(APanel: TWinControl): Integer;
begin
  with APanel do
    if ControlCount = 0 then
      Result := 0
    else begin
      Result := Controls[ControlCount - 1].Top +
      Controls[ControlCount - 1].Height;
    end;
end;

procedure PW_TSE_PenWidth.SpinEdit1_OnChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(PropPointer) do
    PInteger(PropPointer[I])^ := TSpinEdit(Sender).Value;
  ControlEdits.MAIN_Invalidate;
end;

procedure PS_TCB_PenStyle.ComboBox1_OnChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(PropPointer) do
    PInteger(PropPointer[I])^ := TComboBox(Sender).ItemIndex;
  ControlEdits.MAIN_Invalidate;
end;

procedure PS_TCB_PenStyle.ComboBox1_DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  HCenter: Integer;
begin
  HCenter := aRect.Top + (aRect.Bottom - aRect.Top) shr 1;
  with TComboBox(Control).Canvas do begin
    Brush.Color := clWhite;
    FillRect(aRect);

    Pen.Style := TPenStyle(Index);
    Pen.Width := 1;
    MoveTo(aRect.Left, HCenter);
    LineTo(aRect.Right, HCenter);
  end;
end;

procedure BS_TCB_BrushStyle.ComboBox2_OnChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to High(PropPointer) do
    PInteger(PropPointer[I])^ := TComboBox(Sender).ItemIndex;
  ControlEdits.MAIN_Invalidate;
end;

procedure BS_TCB_BrushStyle.ComboBox2_DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control).Canvas do begin
    Brush.Color := clWhite;
    FillRect(aRect);

    Brush.Style := TBrushStyle(Index);
    Brush.Color := clBlack;
    FillRect(aRect);
  end;
end;

//------------------------- Конструкторы Edit ----------------------------------

constructor PW_TSE_PenWidth.Create(APanel: TWinControl; APProp: PointerArray);
var
  SpinEdit1: TSpinEdit;
begin
  inherited;
  SpinEdit1 := TSpinEdit.Create(APanel);

  with SpinEdit1 do begin
    SetBounds(8, GetControlTop(APanel) + 8, 112 , 21);
    OnChange := @SpinEdit1_OnChange;
    Value := PInteger(PropPointer[0])^;
    MinValue := 1;
    Hint := 'Выберите ширину линий из выпадающего списка';
    ShowHint := True;
    Parent := APanel;
  end;
end;

constructor PS_TCB_PenStyle.Create(APanel: TWinControl; APProp: PointerArray);
var
  ComboBox1: TComboBox;
begin
  inherited;
  ComboBox1 := TComboBox.Create(APanel);

  with ComboBox1 do begin
    SetBounds(8, GetControlTop(APanel) + 8, 112 , 21);
    OnDrawItem := @ComboBox1_DrawItem;
    OnChange := @ComboBox1_OnChange;
    Style := csOwnerDrawFixed;
    ReadOnly := True;
    Items.DelimitedText := '"","","","","",""';
    ItemIndex := PInteger(PropPointer[0])^;
    Hint := 'Выберите стиль линий из выпадающего списка';
    ShowHint := True;
    Parent := APanel;
  end;
end;

constructor BS_TCB_BrushStyle.Create(APanel: TWinControl; APProp: PointerArray);
var
  ComboBox2: TComboBox;
begin
  inherited;
  ComboBox2 := TComboBox.Create(APanel);

  with ComboBox2 do begin
    SetBounds(8, GetControlTop(APanel) + 8, 112 , 21);
    OnDrawItem := @ComboBox2_DrawItem;
    OnChange := @ComboBox2_OnChange;
    Style := csOwnerDrawFixed;
    ReadOnly := True;
    Items.DelimitedText := '"","","","","","","",""';
    ItemIndex := PInteger(PropPointer[0])^;
    Hint := 'Выберите стиль заливки из выпадающего списка';
    ShowHint := True;
    Parent := APanel;
  end;
end;

//------------------------- Работа С Массивами ClassEdit и Edit ----------------

procedure TControl_Edit.Free_All_Edit;
var
  I: Integer;
begin
  for I := 0 to High(Edit_Array) do
    Edit_Array[I].Free;
  SetLength(Edit_Array, 0);
end;

procedure TControl_Edit.Add_Edit(ACEdit: TObject);
begin
  SetLength(Edit_Array, Length(Edit_Array) + 1);
  Edit_Array[High(Edit_Array)] := ACEdit;
end;

procedure TControl_Edit.Register_Edit(CLEdit: TMainEditClass);
begin
  SetLength(Class_Edit_Array, Length(Class_Edit_Array) + 1);
  Class_Edit_Array[High(Class_Edit_Array)] := CLEdit;
end;

function  TControl_Edit.CompareName(AName: String): TMainEditClass;
var
  I: Integer;
  SignStr: String;
begin
  SignStr := Copy(AName, 1, Pos('_', AName));
  for I := 0 to High(Class_Edit_Array) do
    if Pos(SignStr, Class_Edit_Array[I].ClassName) = 1 then begin
      Result := Class_Edit_Array[I];
      exit;
    end;
  Result := nil;
end;

initialization

  ControlEdits := TControl_Edit.Create;
  With ControlEdits do begin
    Register_Edit(PW_TSE_PenWidth);
    Register_Edit(PS_TCB_PenStyle);
    Register_Edit(BS_TCB_BrushStyle);
  end;

end.

