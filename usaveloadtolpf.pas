unit USaveLoadToLPF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UVisualTools, typinfo, UTransformation, UFigures, Dialogs,
  UGraph, UExceptions;

procedure Save(Put: AnsiString);
procedure Open(Loa: AnsiString);

implementation

procedure SaveFigure(ADest: TStrings; AFigure: TFigure);
var
  PropInfo: PPropInfo;
  PropList: PPropList;
  PropCount, K, I: Integer;
  S: AnsiString;
begin
  ADest.Append('    figure = ' + AFigure.ClassName);
  PropCount := GetTypeData(AFigure.ClassInfo)^.PropCount;
  if PropCount > 0 then begin
    GetMem(PropList, SizeOf(PPropInfo) * PropCount);
    try
      GetPropInfos(AFigure.ClassInfo, PropList);
      for I := 0 to PropCount - 1 do begin
        PropInfo := PropList^[I];
        S := '      ' + PropInfo^.Name + ' = ';
        if PropInfo^.Name <> 'PointsArr' then
          S += String(GetPropValue(AFigure, PropInfo^.Name))
        else begin
          for K := 0 to High(AFigure.PointsArr) do
            S += Format('|%g/%g|,',
              [AFigure.PointsArr[K].X, AFigure.PointsArr[K].Y]);
          Delete(S, Length(S), 1);
        end;
        ADest.Append(S);
      end;
    finally
      FreeMem(PropList, SizeOf(Pointer) * PropCount);
    end;
    ADest.Append('    end');
  end;
end;

procedure SaveArea(ADest: TStrings; ATrans: TTransform);
var
  PropInfo: PPropInfo;
  PropList: PPropList;
  I, Count: Integer;
begin
  Count := GetTypeData(ATrans.ClassInfo)^.PropCount;
  if Count > 0 then begin
    GetMem(PropList, SizeOf(PPropInfo) * Count);
    try
      GetPropInfos(ATrans.ClassInfo, PropList);
      for I := 0 to Count - 1 do begin
        PropInfo := PropList^[I];
        ADest.Append('  ' + PropInfo^.Name + ' = ' + String(GetPropValue(ATrans,
          PropInfo^.Name)));
      end;
    finally
      FreeMem(PropList, SizeOf(Pointer) * Count);
    end;
  end;
end;

procedure Save(Put: AnsiString);
var
  J: Integer;
  ListIng: TStringList;
begin
  ListIng := TStringList.Create;
  try
    with ListIng do begin
      Append('Vector Editor by LifePack');
      SaveArea(ListIng, Trans);
      for J := 0 to High(MAIN_FIGURE_ARRAY) do
        SaveFigure(ListIng, MAIN_FIGURE_ARRAY[J]);
      ChangeFileExt(Put, 'lpf');
      SaveToFile(Put);
    end;
  finally
    ListIng.Free;
  end;
end;

function OpenFigureArray(STR: AnsiString): TDoublePointArray;
var
  List: TStringList;
  I, PosDot: Integer;
begin
  List := TStringList.Create;

  List.Delimiter := ',';
  List.QuoteChar := '|';
  List.DelimitedText := STR;

  if List.Count = 0 then raise Exception.Create('Отсутствуют координаты фигуры');

  SetLength(Result, List.Count);

  for I := 0 to List.Count-1 do begin
    PosDot := Pos('/', List[I]);
    if PosDot = 0 then
      raise Exception.Create('Отсутствует символ "/" между координатами на сттроке');
    if not(TryStrToFloat(Copy(List[I], 1 , PosDot - 1), Result[I].X)) then
      raise Exception.Create('Невозможно привести координату X в вещественное число');
    if not(TryStrToFloat(Copy(List[I], PosDot + 1 , Length(List[I]) - PosDot), Result[I].Y)) then
      raise Exception.Create('Невозможно привести координату Y в вещественное число');
  end;
end;

procedure Open(Loa: AnsiString);
var
  ListIng: TStringList;
  NewFigureArray: TFigureArray;
  NewFigureClass: TFigureClass;
  I, ClassIndex, J: Integer;
  PropertyName: AnsiString;

  NotFoundPropertyN, NotFoundPropertyV: TStringList;
  FigureCount: Integer;

begin
  ListIng := TStringList.Create;
  NotFoundPropertyN := TStringList.Create;
  NotFoundPropertyV := TStringList.Create;
  try
    With ListIng do begin
      LoadFromFile(Loa);
      if Strings[0] <> 'Vector Editor by LifePack' then exit;
      try
        Text := StringReplace(Text,' ','',[rfReplaceAll]);

        for I := 1 to IndexOfName('figure') - 1 do
          SetPropValue(Trans, Names[I], ValueFromIndex[I]);

        if NotFoundPropertyN.Capacity <> 0 then
          raise EErrorList.CreateList(NotFoundPropertyN);

        if NotFoundPropertyV.Capacity <> 0 then
          raise EErrorList.CreateList(NotFoundPropertyV);

        ClassIndex := IndexOfName('figure');
        While ClassIndex > 0 do begin
          SetLength(NewFigureArray, Length(NewFigureArray) + 1);
          NewFigureClass := ControlFigures.CompareName(ValueFromIndex[ClassIndex]);
          NewFigureArray[High(NewFigureArray)] := NewFigureClass.Create;
          ListIng[ClassIndex] := 'No matter what kind of line';
          J := ClassIndex + 1;
          While Strings[J] <> 'end' do begin
            PropertyName := Names[J];
            if PropertyName <> 'PointsArr' then
              SetPropValue(NewFigureArray[High(NewFigureArray)], PropertyName, ValueFromIndex[J])
            else
              NewFigureArray[High(NewFigureArray)].PointsArr := OpenFigureArray(ValueFromIndex[J]);
            Inc(J);
          end;
          ClassIndex := IndexOfName('figure');
        end;


        MAIN_FIGURE_ARRAY := NewFigureArray;

        Trans.RefreshScrollBar;
        Trans.RefreshSpinEdit;
        ControlTools.MAIN_Invalidate;
      except
        on E: EErrorList do E.ShowDialogList;
      end;

    end;
  finally
    NotFoundPropertyN.Free;
    NotFoundPropertyV.Free;
    ListIng.Free;
  end;
end;

end.

