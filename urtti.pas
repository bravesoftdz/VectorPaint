unit URTTI;

{$mode objfpc}{$H+}

interface

uses
  TypInfo, UEdits, Controls, Dialogs, UFigures, Classes, UEditsStatic, sysutils;

type
  TObjectArray = array of TObject;


procedure EqualClassArray(AFigure1, AFigure2: TFigure);
procedure EqualClassProperties(AClass1, AClass2: TObject);
procedure GetComponentProperties(Instance: TObjectArray; Panel: TWinControl); overload;
procedure GetComponentProperties(Instance: TObject; Panel: TWinControl); overload;
function CopyObject(AObject: TObject): TObject;
function CopyFigure(AFigure: TFigure): TFigure;

implementation

function CopyFigure(AFigure: TFigure): TFigure;
begin
  Result := TFigure(AFigure.ClassType.Create);
  EqualClassProperties(Result, AFigure);
  EqualClassArray(Result, AFigure);
end;

function CopyObject(AObject: TObject): TObject;
begin
  Result := AObject.ClassType.Create;
  EqualClassProperties(Result, AObject);
end;

procedure EqualClassArray(AFigure1, AFigure2: TFigure);
var
  I: Integer;
begin
  SetLength(AFigure1.PointsArray, Length(AFigure2.PointsArray));
  for I := 0 to High(AFigure1.PointsArray) do
    AFigure1.PointsArray[I] := AFigure2.PointsArray[I];
end;

procedure EqualClassProperties(AClass1, AClass2: TObject);
var
  I, Count: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  Count := GetTypeData(AClass1.ClassInfo)^.PropCount;
  if Count > 0 then begin
    GetMem(PropList, SizeOf(PPropInfo) * Count);
    try
      GetPropInfos(AClass1.ClassInfo, PropList);
      for I := 0 to Count - 1 do begin
        PropInfo := PropList^[I];
        if PropInfo^.PropType^.Kind <> tkDynArray then
          SetPropValue(AClass1, PropInfo^.Name, GetPropValue(AClass2,
            PropInfo^.Name));
      end;
    finally
      FreeMem(PropList, SizeOf(Pointer) * Count);
    end;
  end;
end;


procedure GetComponentProperties(Instance: TObjectArray; Panel: TWinControl);
type
  TPropListRec = record
    Count: Integer;
    PropList: PPropList;
  end;

  TPointerRec = record
    Kur: PointerArray;
    Name: String;
  end;

var
  PropList_Array: array of TPropListRec;

  procedure FreeAllMem;
  var
    I: Integer;
  begin
    for I := 0 to High(PropList_Array) do
      FreeMem(PropList_Array[I].PropList);
  end;

var
  NewEdit: TMainEditClass;
  NewEditNew: TMainEditStaticClass;
  MUFinal: array of TPointerRec;
  CurrentPointerArray: TPointerRec;
  Height: Integer;
  Flag: Boolean;
  I, Count, J, L: Integer;
begin
  ControlEdits.Free_All_Edit;
  ControlEditsNew.Free_All_Edit;

  Height := High(Instance);

  if Height = -1 then exit;

  for I := 0 to Height do begin
    Count := GetTypeData(Instance[I].ClassInfo)^.PropCount;
    if Count = 0 then begin
      FreeAllMem;
      Exit;
    end;
    SetLength(PropList_Array, Length(PropList_Array) + 1);
    GetMem(PropList_Array[High(PropList_Array)].PropList, SizeOf(PPropInfo) * Count);
    GetPropInfos(Instance[I].ClassInfo, PropList_Array[High(PropList_Array)].PropList);
    PropList_Array[High(PropList_Array)].Count := Count;
  end;

  SetLength(CurrentPointerArray.Kur, Height + 1);

  for L := 0 to PropList_Array[0].Count -1 do begin

    for I := 0 to Height do begin
      Flag := False;
      for J := 0 to PropList_Array[I].Count -1 do begin
        if PropList_Array[0].PropList^[L]^.Name = PropList_Array[I].PropList^[J]^.Name  then begin
          CurrentPointerArray.Kur[I] :=
            Pointer(Instance[I]) + PtrUInt(PropList_Array[I].PropList^[J]^.SetProc);
          CurrentPointerArray.Name := PropList_Array[I].PropList^[J]^.Name;
          Flag := True;
          break;
        end;
      end;
      if not(Flag) then break;
          //Имеется недоработка в коде ДОПИШИ!!!  УСТРАНЕНА, ОПТИМИЗИРОВАТЬ !!!
    end;
    if Flag then begin
      SetLength(MUFinal, Length(MUFinal) + 1);
      SetLength(MUFinal[High(MUFinal)].Kur, Height + 1);
      MUFinal[High(MUFinal)].Name := CurrentPointerArray.Name;

      for J := 0 to Height do begin
        MUFinal[High(MUFinal)].Kur[J] := CurrentPointerArray.Kur[J];
      end;
    end;
  end;

  for I := 0 to High(MUFinal) do begin
    NewEdit := ControlEdits.CompareName(MUFinal[I].Name);
    if NewEdit = nil then continue;
    NewEdit.Create(Panel, MUFinal[I].Kur);
  end;

  for I := 0 to High(MUFinal) do begin
    NewEditNew := ControlEditsNew.CompareName(MUFinal[I].Name);
    if NewEditNew = nil then continue;
    NewEditNew.Create(
    ControlEditsNew.CompareName_Controls(NewEditNew), MUFinal[I].Kur);
  end;

  FreeAllMem;
end;

procedure GetComponentProperties(Instance: TObject; Panel: TWinControl); overload;
var
  ArrayOneFigure: array[0..0] of TObject;
begin
  if Instance = nil then exit;
  ArrayOneFigure[0] := Instance;
  GetComponentProperties(ArrayOneFigure, Panel);
end;

end.

