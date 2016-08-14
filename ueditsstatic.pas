unit UEditsStatic;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Graphics, ExtCtrls, StdCtrls, Spin, Dialogs, sysutils;

type
  PointerArray = array of Pointer;

type
  TMainEditNewStatic = Class
    Prop__Pointer: PointerArray;
    class function Tagis: Integer; virtual; abstract;
    constructor Create(AObject: TControl; APProp: PointerArray); virtual;
  end;

  TMainEditStaticClass = Class of TMainEditNewStatic;

  PC_PenCol = Class(TMainEditNewStatic)
  type
    OnMouseDown = procedure(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer) of Object;
  strict private
    Pen: TShape;
    PointerToMethod: OnMouseDown;
    procedure PenColorMouseDown(Sender: TObject;  Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);  virtual;
    class function Tagis: Integer; override;

    constructor Create(AObject: TControl; APProp: PointerArray); override;
    destructor  Destroy; override;
  end;

  BC_BrushCol = Class(PC_PenCol)
    class function Tagis: Integer; override;
  end;

type
  TControl_EditNew = Class
  type
    TObjArray = array of TObject;
    TClassEditNewArray = array of TMainEditStaticClass;
    PointerToMethod = procedure of object;
  var
    EditNew_Array: TObjArray;
    Class_EditNew_Array: TClassEditNewArray;
    MAIN_Invalidate: PointerToMethod;

    Class_Object_Array: array of TControl;

    procedure Free_All_Edit;
    procedure Add_Edit(ACEdit: TObject);
    procedure Register_Edit(CLEdit: TMainEditStaticClass);
    function  CompareName(AName: String): TMainEditStaticClass;
    procedure Register_Controls(CLEdit: TControl);
    function  CompareName_Controls(TMENC: TMainEditStaticClass): TControl;
  end;

  var
    ControlEditsNew: TControl_EditNew;

implementation

constructor TMainEditNewStatic.Create(AObject: TControl; APProp: PointerArray);
begin
  Prop__Pointer := APProp;
  ControlEditsNew.Add_Edit(Self);
end;

constructor PC_PenCol.Create(AObject: TControl; APProp: PointerArray);
begin
  inherited;
  Pen := AObject as TShape;
  PointerToMethod := Pen.OnMouseDown;
  Pen.OnMouseDown := @PenColorMouseDown;
end;

procedure PC_PenCol.PenColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  I: integer;
begin
  PointerToMethod(Pen, Button,Shift,X, Y);
  for I := 0 to High(Prop__Pointer) do
    PColor(Prop__Pointer[I])^ := Pen.Brush.Color;

  ControlEditsNew.MAIN_Invalidate;
end;

destructor PC_PenCol.Destroy;
begin
  Pen.OnMouseDown := PointerToMethod;
end;

class function PC_PenCol.Tagis: Integer;
begin
  Result := 1;
end;

class function BC_BrushCol.Tagis: Integer;
begin
  Result := 2;
end;

//------------------------- Работа С Массивами ClassEditNew и EditNew ----------

procedure TControl_EditNew.Free_All_Edit;
var
  I: Integer;
begin
  for I := 0 to High(EditNew_Array) do
    EditNew_Array[I].Free;
  SetLength(EditNew_Array, 0);
end;

procedure TControl_EditNew.Add_Edit(ACEdit: TObject);
begin
  SetLength(EditNew_Array, Length(EditNew_Array) + 1);
  EditNew_Array[High(EditNew_Array)] := ACEdit;
end;

procedure TControl_EditNew.Register_Edit(CLEdit: TMainEditStaticClass);
begin
  SetLength(Class_EditNew_Array, Length(Class_EditNew_Array) + 1);
  Class_EditNew_Array[High(Class_EditNew_Array)] := CLEdit;
end;

function  TControl_EditNew.CompareName(AName: String): TMainEditStaticClass;
var
  I: Integer;
  SignStr: String;
begin
  SignStr := Copy(AName, 1, Pos('_', AName));
  for I := 0 to High(Class_EditNew_Array) do
    if Pos(SignStr, Class_EditNew_Array[I].ClassName) = 1 then begin
      Result := Class_EditNew_Array[I];
      exit;
    end;
  Result := nil;
end;

procedure TControl_EditNew.Register_Controls(CLEdit: TControl);
begin
  SetLength(Class_Object_Array, Length(Class_Object_Array) + 1);
  Class_Object_Array[High(Class_Object_Array)] := CLEdit;
end;

function  TControl_EditNew.CompareName_Controls(TMENC: TMainEditStaticClass): TControl;
var
  I: Integer;
begin
  for I := 0 to High(Class_Object_Array) do
    if TMENC.Tagis = Class_Object_Array[I].Tag then begin
      Result := Class_Object_Array[I];
      exit;
    end;
  Result := nil;
end;

initialization

  ControlEditsNew := TControl_EditNew.Create;
  With ControlEditsNew do begin
    Register_Edit(PC_PenCol);
    Register_Edit(BC_BrushCol);
  end;

end.
