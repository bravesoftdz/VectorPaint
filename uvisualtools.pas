unit UVisualTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, ExtCtrls, Dialogs,
  UFigures, UNotVisualTools, UTransformation, URTTI, sysutils, UGraph;

type
  TTools = class
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); virtual;
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); virtual;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); virtual;
    class function  GetGlyph: string; virtual; abstract;
    class function  GetHint: string; virtual; abstract;
    class procedure Proper; virtual; abstract;
  end;

  TToolsClass = class of TTools;

  TTwoPointFig_Tools = class(TTools)
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
  end;

  { TTwoPointFigFilling_Tools }

  TTwoPointFigFilling_Tools = class(TTwoPointFig_Tools)
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
  end;

  TArrayFig_Tools = class(TTools)

  end;

  TLine_Tools = class(TTwoPointFig_Tools)
    class function  GetHint: string; override;
    class function  GetGlyph: string; override;
    class procedure Proper; override;
  end;

  TRectangle_Tools = class(TTwoPointFigFilling_Tools)
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TRoundRect_Tools = class(TTwoPointFigFilling_Tools)
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TEllipse_Tools = class(TTwoPointFigFilling_Tools)
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TPencil_Tools = class(TArrayFig_Tools)
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TPolyLine_Tools = class(TArrayFig_Tools)
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TSelect_Tools = class(TTools)
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TSelectNodes_Tools = class(TTools)
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TZoom_Tools = class(TTools)
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

  TRelocate_Tools = class(TTools)
    class procedure MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class procedure MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
      Canva: TCanvas); override;
    class function  GetGlyph: string; override;
    class function  GetHint: string; override;
    class procedure Proper; override;
  end;

type
  TControl_Tools = class
  type
    PointerToMethod = procedure of object;
    TClassToolsArray = array of TToolsClass;
  var
  protected
    _Want_To_Finish: Boolean;
  public
    Class_Tools_Array: TClassToolsArray;
    MAIN_Invalidate: PointerToMethod;
    Main_Panel: TPanel;

    property Want_To_Finish: boolean read _Want_To_Finish write _Want_To_Finish default True;
    procedure Register_Tools(CLTools: TToolsClass);
    procedure Re_Draw_Canvas(Canva: TCanvas);
    procedure Set_Selected_On_Draw;
  end;

var
  MAIN_FIGURE_ARRAY: TFigureArray;

  GlobalPenColor: ^TColor;
  GlobalBrushColor: ^TColor;
  GlobalBackgroundColor: ^TColor;

  FakeFigure: TFigure;

  ControlTools: TControl_Tools;

implementation

var
  OneNotVisualTools: TNotVisible;

//------------------------- Class TTools ---------------------------------------

class procedure TTools.MouseDown(Shift: TShiftState; MousePoint: TDoublePoint;
  Canva: TCanvas);
begin
  SetLength(MAIN_FIGURE_ARRAY, Length(MAIN_FIGURE_ARRAY) + 1);
  MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)] := CopyFigure(FakeFigure);
  MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].PC_PenCol := GlobalPenColor^;

  MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].AddPoint(MousePoint, True);
end;

class procedure TTools.MouseMove(Shift: TShiftState; MousePoint: TDoublePoint;
  Canva: TCanvas);
begin
end;

class procedure TTools.MouseUp(Shift: TShiftState; MousePoint: TDoublePoint;
  Canva: TCanvas);
begin
end;

//------------------------- Class TTwoPointFigFilling_Tools --------------------

class procedure TTwoPointFigFilling_Tools.MouseDown(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  inherited;
  TTwoPointFigFilling(MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)]).BC_BrushCol :=
    GlobalBrushColor^;
end;

//------------------------- Сlass TTwoPointFig_Tools ---------------------------

class procedure TTwoPointFig_Tools.MouseDown(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  inherited;
  MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].AddPoint(MousePoint, True);

end;

class procedure TTwoPointFig_Tools.MouseUp(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].AddPoint(MousePoint, False);
  ControlTools.Want_To_Finish := True;
  ControlTools.MAIN_Invalidate;
end;

class procedure TTwoPointFig_Tools.MouseMove(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  if not (ssLeft in Shift) then exit;
  Trans.ExtendArea(MousePoint);
  Trans.RefreshScrollBar;
  MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].AddPoint(MousePoint, False);
  ControlTools.MAIN_Invalidate;
end;

//------------------------- Сlass TLine_Tools ----------------------------------

class function TLine_Tools.GetGlyph: string;
begin
  Result := 'Icons/OneLine.bmp';
end;

class function TLine_Tools.GetHint: string;
begin
  Result := 'Отрезок';
end;

class procedure TLine_Tools.Proper;
begin
  FakeFigure.Free;
  FakeFigure := TLine.Create;
end;

//------------------------- Сlass TRectangle_Tools -----------------------------

class function TRectangle_Tools.GetGlyph: string;
begin
  Result := 'Icons/Rectangle.bmp';
end;

class function TRectangle_Tools.GetHint: string;
begin
  Result := 'Прямоугольник';
end;

class procedure TRectangle_Tools.Proper;
begin
  FakeFigure.Free;
  FakeFigure := TRectangle.Create;
end;

//------------------------- Сlass TRoundRect_Tools -----------------------------

class function TRoundRect_Tools.GetGlyph: string;
begin
  Result := 'Icons/RoundRect.bmp';
end;

class function TRoundRect_Tools.GetHint: string;
begin
  Result := 'Прямоугольник с закругленными углами';
end;

class procedure TRoundRect_Tools.Proper;
begin
  FakeFigure.Free;
  FakeFigure := TRoundRect.Create;
end;

//------------------------- Сlass TEllipse_Tools -------------------------------

class function TEllipse_Tools.GetGlyph: string;
begin
  Result := 'Icons/Ellipse.bmp';
end;

class function TEllipse_Tools.GetHint: string;
begin
  Result := 'Эллипс';
end;

class procedure TEllipse_Tools.Proper;
begin
  FakeFigure.Free;
  FakeFigure := TEllipse.Create;
end;

//------------------------- TPencil_Tools --------------------------------------

class procedure TPencil_Tools.MouseUp(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  ControlTools.Want_To_Finish := True;
end;

class procedure TPencil_Tools.MouseMove(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  if ssLeft in Shift then begin
    MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].AddPoint(MousePoint, True);
    Trans.ExtendArea(MousePoint);
    Trans.RefreshScrollBar;
    ControlTools.MAIN_Invalidate;
  end;
end;

class function TPencil_Tools.GetGlyph: string;
begin
  Result := 'Icons/Pencil.bmp';
end;

class function TPencil_Tools.GetHint: string;
begin
  Result := 'Карандаш';
end;

class procedure TPencil_Tools.Proper;
begin
  FakeFigure.Free;
  FakeFigure := TPencil.Create;
end;

//------------------------- TPolyLine_Tools ------------------------------------

class procedure TPolyLine_Tools.MouseDown(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  if ssRight in Shift then
    ControlTools.Want_To_Finish := True
  else
  begin
    if (Length(MAIN_FIGURE_ARRAY) = 0) or
       (ControlTools.Want_To_Finish) then
    begin
      inherited;
      ControlTools.Want_To_Finish := False;
    end;
    MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].AddPoint(MousePoint, True);
    ControlTools.MAIN_Invalidate;
  end;
end;

class procedure TPolyLine_Tools.MouseUp(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin

end;

class procedure TPolyLine_Tools.MouseMove(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  if Length(MAIN_FIGURE_ARRAY) = 0 then
    exit;
  if ControlTools.Want_To_Finish then
    exit;
  MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY)].AddPoint(MousePoint, False);
  Trans.ExtendArea(MousePoint);
  Trans.RefreshScrollBar;
  ControlTools.MAIN_Invalidate;
end;

class function TPolyLine_Tools.GetGlyph: string;
begin
  Result := 'Icons/PolyLine.bmp';
end;

class function TPolyLine_Tools.GetHint: string;
begin
  Result := 'Прямая по точкам';
end;

class procedure TPolyLine_Tools.Proper;
begin
  FakeFigure.Free;
  FakeFigure := TPolyLine.Create;
end;

//------------------------- TSelect_Tools --------------------------------------

class procedure TSelect_Tools.MouseDown(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
var
  I: Integer;

  FigureList: TObjectArray;
begin
  With OneNotVisualTools do begin
    SelObj := -2;
    With MousePoint do begin
      DynMPoint.X := X;
      DynMPoint.Y := Y;

      DynMRect.Left := X;
      DynMRect.Top := Y;
      DynMRect.Right := X + 1;
      DynMRect.Bottom := Y + 1;
    end;

    for I := High(MAIN_FIGURE_ARRAY) downto 0 do
      if (MAIN_FIGURE_ARRAY[I].SelectedFigure = SelectedDrawFigure) and
        MAIN_FIGURE_ARRAY[I].MouseOverFigure(DynMRect) then
        SelObj := -1;
  end;
  OneNotVisualTools.DynMRect.BottomRight := OneNotVisualTools.DynMRect.TopLeft;
end;

class procedure TSelect_Tools.MouseUp(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
var
  I: Integer;
  FigureList: TObjectArray;
begin
  if OneNotVisualTools.SelObj = -1 then exit;
  ControlTools.Set_Selected_On_Draw;
  for I := High(MAIN_FIGURE_ARRAY) downto 0 do
    if MAIN_FIGURE_ARRAY[I].MouseOverFigure(OneNotVisualTools.DynMRect) then
    begin
      MAIN_FIGURE_ARRAY[I].SelectedFigure := SelectedDrawFigure;
      SetLength(FigureList, Length(FigureList) + 1);
      FigureList[high(FigureList)] := MAIN_FIGURE_ARRAY[I];
    end;
  ControlTools.Main_Panel.DestroyComponents;
  GetComponentProperties(FigureList, ControlTools.Main_Panel);
  OneNotVisualTools.DynMRect.BottomRight := OneNotVisualTools.DynMRect.TopLeft;
  ControlTools.MAIN_Invalidate;
end;

class procedure TSelect_Tools.MouseMove(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
var
  I: Integer;
begin
  if not (ssLeft in Shift) then exit;
  if OneNotVisualTools.SelObj = -2 then
  begin
    OneNotVisualTools.DynMRect.Right := MousePoint.X;
    OneNotVisualTools.DynMRect.Bottom := MousePoint.Y;
    ControlTools.MAIN_Invalidate;
  end
  else
  begin
    OneNotVisualTools.DynMPoint.X := MousePoint.X - OneNotVisualTools.DynMPoint.X;
    OneNotVisualTools.DynMPoint.Y := MousePoint.Y - OneNotVisualTools.DynMPoint.Y;
    for I := 0 to High(MAIN_FIGURE_ARRAY) do
    begin
      if MAIN_FIGURE_ARRAY[I].SelectedFigure = SelectedDrawFigure then
        MAIN_FIGURE_ARRAY[I].MoveFigure(OneNotVisualTools.DynMPoint);
    end;
    OneNotVisualTools.DynMPoint.X := MousePoint.X;
    OneNotVisualTools.DynMPoint.Y := MousePoint.Y;
    ControlTools.MAIN_Invalidate;
  end;
end;

class function TSelect_Tools.GetGlyph: string;
begin
  Result := 'Icons/Select.bmp';
end;

class function TSelect_Tools.GetHint: string;
begin
  Result := 'Выбрать фигуру';
end;

class procedure TSelect_Tools.Proper;
begin
  FakeFigure := nil;
  OneNotVisualTools.Free;
  OneNotVisualTools := TSelect.Create;
end;

//------------------------- TSelectNodes_Tools ---------------------------------

class procedure TSelectNodes_Tools.MouseDown(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
var
  I: Integer;
begin
  OneNotVisualTools.SelObj := -2;
  OneNotVisualTools.DynMPoint := MousePoint;
  ControlTools.Set_Selected_On_Draw;
  for I := High(MAIN_FIGURE_ARRAY) downto 0 do
  begin
    OneNotVisualTools.SelNode := MAIN_FIGURE_ARRAY[I].MouseOverFigureNode(
      DoubleRect(MousePoint.X - 1, MousePoint.Y - 1, MousePoint.X + 1, MousePoint.Y + 1));
    if MAIN_FIGURE_ARRAY[I].MouseOverFigure(
      DoubleRect(MousePoint.X - 1, MousePoint.Y - 1, MousePoint.X + 1, MousePoint.Y + 1)) then
      MAIN_FIGURE_ARRAY[I].SelectedFigure := SelectedNodeDrawFigure;
      if OneNotVisualTools.SelNode > -1 then
      begin
        MAIN_FIGURE_ARRAY[I].SelectedFigure := SelectedNodeDrawFigure;
        OneNotVisualTools.SelObj := I;
        exit;
      end;
  end;
  ControlTools.MAIN_Invalidate;
end;

class procedure TSelectNodes_Tools.MouseUp(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
end;

class procedure TSelectNodes_Tools.MouseMove(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  if not (ssLeft in Shift) then
    exit;
  if OneNotVisualTools.SelObj <= -1 then
    exit;
  OneNotVisualTools.DynMPoint.X := MousePoint.X - OneNotVisualTools.DynMPoint.X;
  OneNotVisualTools.DynMPoint.Y := MousePoint.Y - OneNotVisualTools.DynMPoint.Y;

  MAIN_FIGURE_ARRAY[OneNotVisualTools.SelObj].MoveFigureNode(OneNotVisualTools.DynMPoint,
    OneNotVisualTools.SelNode);
  OneNotVisualTools.DynMPoint := MousePoint;
  ControlTools.MAIN_Invalidate;
end;

class function TSelectNodes_Tools.GetGlyph: string;
begin
  Result := 'Icons/SelectNodes.bmp';
end;

class function TSelectNodes_Tools.GetHint: string;
begin
  Result := 'Выбрать вершину фигуры';
end;

class procedure TSelectNodes_Tools.Proper;
begin
  FakeFigure := nil;
  OneNotVisualTools.Free;
  OneNotVisualTools := TSelect.Create;
end;

//------------------------- Сlass TZoom_Tools ----------------------------------

class procedure TZoom_Tools.MouseDown(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  OneNotVisualTools.DynMRect.TopLeft := MousePoint;
end;

class procedure TZoom_Tools.MouseUp(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
var
  MyRect: TRect;
begin
  MyRect := Trans.W2S(MaxRect(MousePoint, OneNotVisualTools.DynMRect.TopLeft));
  With Trans do begin
    Scaling(W2S(MaxRect(MousePoint, OneNotVisualTools.DynMRect.TopLeft)));
    Centering(W2S(MaxRect(MousePoint, OneNotVisualTools.DynMRect.TopLeft)));
    RefreshScrollBar;
  end;
  OneNotVisualTools.DynMRect.TopLeft := MousePoint;
  OneNotVisualTools.DynMRect.BottomRight := MousePoint;
  ControlTools.MAIN_Invalidate;
end;

class procedure TZoom_Tools.MouseMove(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  if not(ssLeft in Shift) then exit;
  OneNotVisualTools.DynMRect.BottomRight := MousePoint;
  ControlTools.MAIN_Invalidate;
end;

class function TZoom_Tools.GetGlyph: string;
begin
  Result := 'Icons/Zoom.bmp';
end;

class function TZoom_Tools.GetHint: string;
begin
  Result := 'Увеличить изображение';
end;

class procedure TZoom_Tools.Proper;
begin
  FakeFigure := nil;
  OneNotVisualTools.Free;
  OneNotVisualTools := TZoom.Create;
end;

//------------------------- Сlass TRelocate_Tools ------------------------------

class procedure TRelocate_Tools.MouseDown(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  OneNotVisualTools.DynMPoint := MousePoint;
end;

class procedure TRelocate_Tools.MouseUp(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
end;

class procedure TRelocate_Tools.MouseMove(Shift: TShiftState;
  MousePoint: TDoublePoint; Canva: TCanvas);
begin
  if not (ssLeft in Shift) then exit;
  Trans.OffsetX := Trans.OffsetX +
            Round(OneNotVisualTools.DynMPoint.X * Trans.Scale - MousePoint.X * Trans.Scale);
  Trans.OffsetY := Trans.OffsetY +
            Round(OneNotVisualTools.DynMPoint.Y* Trans.Scale - MousePoint.Y * Trans.Scale);
  Trans.RefreshScrollBar;
  ControlTools.MAIN_Invalidate;
end;

class function TRelocate_Tools.GetGlyph: string;
begin
  Result := 'Icons/Relocate.bmp';
end;

class function TRelocate_Tools.GetHint: string;
begin
  Result := 'Переместить холст';
end;

class procedure TRelocate_Tools.Proper;
begin
  FakeFigure := nil;
  OneNotVisualTools.Free;
  OneNotVisualTools := TRelocate.Create;
end;

//------------------------- Работа С Массивами ClassеTools ---------------------

procedure TControl_Tools.Register_Tools(CLTools: TToolsClass);
begin
  SetLength(Class_Tools_Array, Length(Class_Tools_Array) + 1);
  Class_Tools_Array[High(Class_Tools_Array)] := CLTools;
end;

procedure TControl_Tools.Re_Draw_Canvas(Canva: TCanvas);
var
  I: Integer;
begin
  With Canva do begin
    Brush.Style := bsSolid;
    Brush.Color := GlobalBackgroundColor^;
    Clear;
  end;
  for I := 0 to High(MAIN_FIGURE_ARRAY) do
  begin
    case MAIN_FIGURE_ARRAY[I].SelectedFigure of
      SelectedDrawFigure: MAIN_FIGURE_ARRAY[I].SelectedDraw(Canva);
      SelectedNodeDrawFigure: MAIN_FIGURE_ARRAY[I].SelectedNodeDraw(Canva);
      DrawFigure: MAIN_FIGURE_ARRAY[I].Draw(Canva);
    end;
  end;
  OneNotVisualTools.DynDraw(Canva);
end;

procedure TControl_Tools.Set_Selected_On_Draw;
var
  I: Integer;
begin
  for I := 0 to High(MAIN_FIGURE_ARRAY) do
    MAIN_FIGURE_ARRAY[I].SelectedFigure := DrawFigure;
end;

initialization

  ControlTools := TControl_Tools.Create;
  with ControlTools do
  begin
    Register_Tools(TLine_Tools);
    Register_Tools(TRectangle_Tools);
    Register_Tools(TEllipse_Tools);
    Register_Tools(TPencil_Tools);
    Register_Tools(TPolyLine_Tools);
    Register_Tools(TSelect_Tools);
    Register_Tools(TSelectNodes_Tools);
    Register_Tools(TZoom_Tools);
    Register_Tools(TRelocate_Tools);
    Register_Tools(TRoundRect_Tools);
  end;
  OneNotVisualTools := TNotVisible.Create;

end.
