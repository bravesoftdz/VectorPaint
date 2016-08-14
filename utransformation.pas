unit UTransformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, UGraph, StdCtrls, Spin;

type
  TTransform = Class
  protected
    _Offset: TPoint;
    _Scale: Double;
    _ScrollBarX,
    _ScrollBarY: TScrollBar;
    _FilledArea: TDoubleRect;
    _FSEScale: TFloatSpinEdit;
    procedure SetScale(AScale: Double);
    procedure SetOffset(AOffset: TPoint);
  public
    property FilledArea: TDoubleRect read _FilledArea write _FilledArea;

    property Offset: TPoint read _Offset write SetOffset;

    property ScrollBarX: TScrollBar read _ScrollBarX write _ScrollBarX;
    property ScrollBarY: TScrollBar read _ScrollBarX write _ScrollBarY;

    property FSEScale: TFloatSpinEdit read _FSEScale write _FSEScale;

    procedure ScrollIncrease(Coordinate: TPoint);
    procedure ScrollReduction(Coordinate: TPoint);
    procedure RefreshScrollBar;
    procedure RefreshSpinEdit;
    procedure ExtendArea(ADPoint: TDoublePoint);
    procedure Centering(ARect: TRect);
    procedure Scaling(ARect: Trect);
    function  S2W(APoint: TPoint): TDoublePoint;
    function  W2S(ADPoint: TDoublePoint): TPoint; overload;
    function  W2S(ADPointArray: TDoublePointArray): TPointArray; overload;
    function  W2S(ADRect: TDoubleRect): TRect; overload;
    function  W2S(ADRectArray: TDoubleRectArray): TRectArray; overload;
  published
    property Scale: Double read _Scale write SetScale;
    property OffsetX: Integer read _Offset.X write _Offset.X;
    property OffsetY: Integer read _Offset.Y write _Offset.Y;
    property FilledAreaLeft: Double read _FilledArea.Left write _FilledArea.Left;
    property FilledAreaTop: Double read _FilledArea.Top write _FilledArea.Top;
    property FilledAreaRight: Double read _FilledArea.Right write _FilledArea.Right;
    property FilledAreaBottom: Double read _FilledArea.Bottom write _FilledArea.Bottom;
  end;

var
  Trans: TTransform;

implementation

procedure TTransform.SetScale(AScale: Double);
begin
  if (AScale > _FSEScale.MinValue) and
     (AScale < _FSEScale.MaxValue) then
  begin
    _FSEScale.Value := AScale;
    _Scale := AScale;
  end;
end;

procedure TTransform.SetOffset(AOffset: TPoint);
begin
  _Offset := AOffset;
end;

procedure TTransform.RefreshScrollBar;
var
  SB: TDoublePoint;
begin
  SB := DoublePoint(_ScrollBarX.Width - 1, _ScrollBarY.Height - 1);
  _FilledArea := MaxRect(_FilledArea, _Offset / _Scale);
  _FilledArea := MaxRect(_FilledArea, (_Offset + SB) / _Scale);

  With _ScrollBarY do begin
    Min := Round(_FilledArea.Top * _Scale);
    Max := Round(_FilledArea.Bottom * _Scale);
    Position := _Offset.Y;
  end;
  With _ScrollBarX do begin
    Min := Round(_FilledArea.Left * _Scale);
    Max := Round(_FilledArea.Right * _Scale);
    Position := _Offset.X;
  end;
end;

procedure TTransform.RefreshSpinEdit;
begin
  _FSEScale.Value := _Scale;
end;

function TTransform.S2W(APoint: TPoint): TDoublePoint;
begin
  Result := (APoint + _OffSet) / _Scale;
end;

function TTransform.W2S(ADPoint: TDoublePoint): TPoint;
begin
  Result := Round(ADPoint * _Scale - _Offset);
end;

function TTransform.W2S(ADPointArray: TDoublePointArray): TPointArray;
var
  I: Integer;
begin
  SetLength(Result, Length(ADPointArray));
  for I := 0 to High(ADPointArray) do
    Result[I] := W2S(ADPointArray[I]);
end;

function TTransform.W2S(ADRect: TDoubleRect): TRect;
begin
  Result.TopLeft := W2S(ADRect.TopLeft);
  Result.BottomRight := W2S(ADRect.BottomRight);
end;

function TTransform.W2S(ADRectArray: TDoubleRectArray): TRectArray;
var
  I: Integer;
begin
  SetLength(Result, Length(ADRectArray));
  for I := 0 to High(ADRectArray) do
    Result[I] := W2S(ADRectArray[I]);
end;

procedure TTransform.ExtendArea(ADPoint: TDoublePoint);
begin
  _FilledArea := MaxRect(_FilledArea, ADPoint);
end;

procedure TTransform.Centering(ARect: TRect);
begin
  with ARect do begin
    if ((Right - Left) or (Bottom - Top)) = 0 then exit;
    _OffSet.X := _OffSet.X - (_ScrollBarX.Width div 2 - Left -
                (Right - Left) div 2);
    _OffSet.Y := _OffSet.Y - (_ScrollBarY.Height div 2 - Top -
                (Bottom - Top) div 2);
  end;
end;

procedure TTransform.Scaling(ARect: Trect);
begin
  with ARect do begin
    if ((Right - Left) or (Bottom - Top)) = 0 then exit;
    if (Right - Left) > (Bottom - Top) then
      Scale := _Scale * _ScrollBarX.Width / (Right - Left)
    else
      Scale := _Scale * _ScrollBarY.Height / (Bottom - Top);
  end;
end;

procedure TTransform.ScrollIncrease(Coordinate: TPoint);
begin
  if (_Scale + 0.1 > _FSEScale.MinValue) and
     (_Scale + 0.1 < _FSEScale.MaxValue) then
  begin
    _Offset :=  -(Coordinate - S2W(Coordinate) * (_Scale + 0.1));
    Scale := _Scale + 0.1;
    RefreshScrollBar;
  end;
end;

procedure TTransform.ScrollReduction(Coordinate: TPoint);
begin
  if (_Scale - 0.1 > _FSEScale.MinValue) and
     (_Scale - 0.1 < _FSEScale.MaxValue) then
  begin
    _Offset :=  -(Coordinate - S2W(Coordinate) * (_Scale - 0.1));
    Scale := _Scale - 0.1;
    RefreshScrollBar;
  end;
end;

initialization

  Trans := TTransform.Create;

end.
