unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Windows, Graphics, UTransformation, UGraph;

type
  RSelectedType = (DrawFigure,
                   SelectedDrawFigure,
                   SelectedNodeDrawFigure,
                   NotDrawFigure);

type
{$M+}
  TFigure = Class
  public
    PointsArray: TDoublePointArray;
  protected
    _PenColor: TColor;
    _PenWidth: Integer;
    _PenStyle: Integer;
    _Selected: RSelectedType;
    procedure SetDefaultCanvas(Canva: TCanvas);
  published
    property  PointsArr: TDoublePointArray read PointsArray write PointsArray;
    property  PC_PenCol: TColor read _PenColor write _PenColor;
    property  PW_PenWid: Integer read _PenWidth write _PenWidth;
    property  PS_PenSty: Integer read _PenStyle write _PenStyle;

    property  SelectedFigure: RSelectedType read _Selected write _Selected default DrawFigure;
  public
    // Добавить или изменить вершину
    procedure   AddPoint(MousePoint: TDoublePoint; Next: Boolean); virtual;
    // Проверка на пренадлежность фигуры или вершины
    function    MouseOverFigure(MousePoint: TDoubleRect): Boolean; virtual; abstract;
    function    MouseOverFigureNode(MousePoint: TDoubleRect): Integer; virtual;
    // Перемещение фигуры или вершины
    procedure   MoveFigure(MousePoint: TDoublePoint); virtual;
    procedure   MoveFigureNode(MousePoint: TDoublePoint; INode: Integer); virtual;
    // Виды прорисовок
    procedure   SelectedDraw(Canva: TCanvas); virtual;
    procedure   SelectedNodeDraw(Canva: TCanvas); virtual;
    procedure   Draw(Canva: TCanvas); virtual;
  end;
{$M-}

   TFigureClass = Class of TFigure;

type
  TTwoPointFig = Class(TFigure)
  public

  end;

type
  TTwoPointFigFilling = Class(TTwoPointFig)
  protected
    _BrushStyle: Integer;
    _BrushColor: TColor;
  published
    property  BC_BrushCol: TColor read _BrushColor write _BrushColor;
    property  BS_BrushSty: Integer read _BrushStyle write _BrushStyle;
  public
    procedure Draw(Canva: TCanvas); override;
  end;

  TArrayFig = Class(TFigure)

  end;

type
  TPencil = Class(TArrayFig)
  public
    procedure Draw(Canva: TCanvas); override;
    function  MouseOverFigure(MousePoint: TDoubleRect): Boolean; override;
    procedure SelectedDraw(Canva: TCanvas); override;
  end;

type
  TPolyLine = Class(TPencil)

  end;

type
  TLine = Class(TTwoPointFig)
  public
    procedure Draw(Canva: TCanvas); override;
    function  MouseOverFigure(MousePoint: TDoubleRect): Boolean; override;
  end;

  TRectangle = Class(TTwoPointFigFilling)
  public
    procedure Draw(Canva: TCanvas); override;
    function  MouseOverFigure(MousePoint: TDoubleRect): Boolean; override;
  end;

  TEllipse = Class(TTwoPointFigFilling)
  public
    procedure Draw(Canva: TCanvas); override;
    function  MouseOverFigure(MousePoint: TDoubleRect): Boolean; override;
  end;

  TRoundRect = Class(TTwoPointFigFilling)
  protected
    _Width_Ell, _Height_Ell: Integer;
  published
    property PW_WidthEll: Integer read _Width_Ell write _Width_Ell;
    property PW_HeightEll: Integer read _Height_Ell write _Height_Ell;
  public
    procedure Draw(Canva: TCanvas); override;
    function  MouseOverFigure(MousePoint: TDoubleRect): Boolean; override;
  end;

  TControl_Figure = Class
  type
    TFigureClassArray = array of TFigureClass;
  var
    Class_Figure_Array: TFigureClassArray;
    procedure Register_Figures(CLFigure: TFigureClass);
    function  CompareName(AName: String): TFigureClass;
  end;

type
  TFigureArray = array of TFigure;

var
  ControlFigures: TControl_Figure;

implementation

//------------------------- Class TFigure --------------------------------------

procedure TFigure.SetDefaultCanvas(Canva: TCanvas);
begin
  With Canva do begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
 end;
end;

procedure TFigure.Draw(Canva: TCanvas);
begin
  With Canva.Pen do begin
    Color := _PenColor;
    Width := _PenWidth;
    Style := TPenStyle(_PenStyle);
  end;
end;

procedure TFigure.SelectedNodeDraw(Canva: TCanvas);
var
  I: Integer;
begin
  Draw(Canva);
  SetDefaultCanvas(Canva);
  for I := 0 to High(PointsArray) do begin
    Canva.Rectangle(ExpandRect(Trans.W2S(PointsArray[I]), 3));
  end;
end;

procedure TFigure.SelectedDraw(Canva: TCanvas);
begin
  SelectedNodeDraw(Canva);
end;

function TFigure.MouseOverFigureNode(MousePoint: TDoubleRect): Integer;
var
  I: Integer;
  RectangleRgn: HRGN;
begin
  Result := -1;
  for I := 0 to High(PointsArray) do begin
    RectangleRgn := CreateRectRgnIndirect(ExpandRect(PointsArray[I], 3 / Trans.Scale));
    if RectInRegion(RectangleRgn, MousePoint) then Result := I;
    DeleteObject(RectangleRgn);
  end;
end;

procedure TFigure.MoveFigureNode(MousePoint: TDoublePoint; INode: Integer);
begin
  PointsArray[INode] += MousePoint;
end;

procedure TFigure.MoveFigure(MousePoint: TDoublePoint);
var
  I: Integer;
begin
  for I := 0 to High(PointsArray) do
    PointsArray[I] += MousePoint;
end;

procedure TFigure.AddPoint(MousePoint: TDoublePoint; Next: Boolean);
begin
  if Next then
    SetLength(PointsArray, Length(PointsArray) + 1);
  PointsArray[High(PointsArray)] := MousePoint;
end;

//------------------------- TTwoPointFigFilling --------------------------------

procedure TTwoPointFigFilling.Draw(Canva: TCanvas);
begin
  Inherited;
  With Canva.Brush do begin
    Color := _BrushColor;
    Style := TBrushStyle(_BrushStyle);
  end;
end;

//------------------------- Class TPencil --------------------------------------

procedure TPencil.Draw(Canva: TCanvas);
begin
  Inherited;
  Canva.PolyLine(Trans.W2S(PointsArray));
end;

function TPencil.MouseOverFigure(MousePoint: TDoubleRect): Boolean;
var
  I: Integer;
  PencilRgn: HRGN;
  LinePos: array of TPoint;
begin
  Result := False;
  SetLength(LinePos, 2 * Length(PointsArray) - 1);
  for I := 0 to High(PointsArray) do
    LinePos[I] := PointsArray[I];
  for I := High(PointsArray) downto 0 do
    LinePos[2 * Length(PointsArray) - 1 - I] := ExpandPoint(PointsArray[I], 1);
  PencilRgn:=CreatePolygonRgn(LinePos[0], Length(LinePos), WINDING);
  if RectInRegion(PencilRgn, MousePoint) then Result := True;
  DeleteObject(PencilRgn);
end;

procedure TPencil.SelectedDraw(Canva: TCanvas);
begin
  Draw(Canva);
  SetDefaultCanvas(Canva);
  Canva.Rectangle(ExpandRect(Trans.W2S(PointsArray[0]), 3));
  Canva.Rectangle(ExpandRect(Trans.W2S(PointsArray[High(PointsArray)]), 3));
end;

//------------------------- Class TLine ----------------------------------------

procedure TLine.Draw(Canva: TCanvas);
begin
  Inherited;
  Canva.Line(Trans.W2S(PointsArray[0]), Trans.W2S(PointsArray[1]));
end;

function TLine.MouseOverFigure(MousePoint: TDoubleRect): Boolean;
var
  LinePos: Array[0..3] of TPoint;
  LineRgn: HRGN;
begin
  Result := False;
  LinePos[0] := ExpandPoint(PointsArray[0], 1);
  LinePos[1] := PointsArray[0];
  LinePos[2] := PointsArray[1];
  LinePos[3] := ExpandPoint(PointsArray[1], 1);
  LineRgn := CreatePolygonRgn(LinePos[0], 4, WINDING);
  if RectInRegion(LineRgn, MousePoint) then Result := True;
  DeleteObject(LineRgn);
end;

//------------------------- Class TRectangle -----------------------------------

procedure TRectangle.Draw(Canva: TCanvas);
begin
  Inherited;
  Canva.Rectangle(Trans.W2S(DoubleRect(PointsArray[0], PointsArray[1])));
end;

function TRectangle.MouseOverFigure(MousePoint: TDoubleRect): Boolean;
var
  RectangleRgn: HRGN;
begin
  Result := False;
  RectangleRgn := CreateRectRgnIndirect(DoubleRect(PointsArray[0], PointsArray[1]));
  if RectInRegion(RectangleRgn, MousePoint) then Result := True;
  DeleteObject(RectangleRgn);
end;

//------------------------- Class TRoundRect -----------------------------------

procedure TRoundRect.Draw(Canva: TCanvas);
begin
  Inherited;
  Canva.RoundRect(Trans.W2S(DoubleRect(PointsArray[0], PointsArray[1])),
                  Round(_Width_Ell * Trans.Scale),
                  Round(_Height_Ell * Trans.Scale));
end;

function TRoundRect.MouseOverFigure(MousePoint: TDoubleRect): Boolean;
var
  RoundRectRgn: HRGN;
begin
  Result := False;
  RoundRectRgn := CreateRoundRectRgn(
                  Round(PointsArray[0].X),
                  Round(PointsArray[0].Y),
                  Round(PointsArray[1].X),
                  Round(PointsArray[1].Y),
                  _Width_Ell,
                  _Height_Ell);
  if RectInRegion(RoundRectRgn, MousePoint) then Result := True;
  DeleteObject(RoundRectRgn);
end;

//------------------------- Class TEllipse -------------------------------------

procedure TEllipse.Draw(Canva: TCanvas);
begin
  Inherited;
  Canva.Ellipse(Trans.W2S(DoubleRect(PointsArray[0], PointsArray[1])));
end;

function TEllipse.MouseOverFigure(MousePoint: TDoubleRect): Boolean;
var
  EllipseRgn: HRGN;
begin
  Result := False;
  EllipseRgn := CreateEllipticRgnIndirect(DoubleRect(PointsArray[0], PointsArray[1]));
  if RectInRegion(EllipseRgn, MousePoint) then Result := True;
  DeleteObject(EllipseRgn);
end;

//------------------------- Работа С Массивами ClassFigure ---------------------

procedure TControl_Figure.Register_Figures(CLFigure: TFigureClass);
begin
  SetLength(Class_Figure_Array, Length(Class_Figure_Array) + 1);
  Class_Figure_Array[High(Class_Figure_Array)] := CLFigure;
end;

function  TControl_Figure.CompareName(AName: String): TFigureClass;
var
  I: Integer;
begin
  for I := 0 to High(Class_Figure_Array) do
    if AName = Class_Figure_Array[I].ClassName then begin
      Result := Class_Figure_Array[I];
      exit;
    end;
  Result := nil;
end;

initialization

  ControlFigures := TControl_Figure.Create;
  with ControlFigures do
  begin
    Register_Figures(TEllipse);
    Register_Figures(TRectangle);
    Register_Figures(TLine);
    Register_Figures(TPencil);
    Register_Figures(TPolyLine);
    Register_Figures(TRoundRect);
  end;

end.

