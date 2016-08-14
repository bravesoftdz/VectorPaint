unit UGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, Math;
type
  TDoublePoint = record
    X, Y: Double;
  end;

  TDoubleRect = record
    Case Integer of
      0: (Left, Top, Right, Bottom: Double);
      1: (TopLeft, BottomRight: TDoublePoint);
  end;

  PDoublePoint = ^TDoublePoint;
  PDoubleRect = ^TDoubleRect;

  TPointArray = array of TPoint;
  TDoublePointArray = array of TDoublePoint;
  TRectArray = array of TRect;
  TDoubleRectArray = array of TDoubleRect;

  PPointArray = ^TPointArray;
  PDoublePointArray = ^TDoublePointArray;
  PRectArray = ^TRectArray;
  PDoubleRectArray = ^TDoubleRectArray;

operator + (Addend1, Addend2: TDoublePoint): TDoublePoint;
operator + (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
operator + (Addend1: TDoublePoint; Addend2: TPoint): TDoublePoint;
operator + (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
operator + (Addend1, Addend2: TPoint): TPoint;

operator + (Addend1, Addend2: TDoubleRect): TDoubleRect;
operator + (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
operator + (Addend1: TDoubleRect; Addend2: TRect): TDoubleRect;
operator + (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
operator + (Addend1, Addend2: TRect): TRect;

operator - (Addend1, Addend2: TDoublePoint): TDoublePoint;
operator - (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
operator - (Addend1: TDoublePoint; Addend2: TPoint): TDoublePoint;
operator - (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
operator - (Addend1, Addend2: TPoint): TPoint;

operator - (Addend1, Addend2: TDoubleRect): TDoubleRect;
operator - (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
operator - (Addend1: TDoubleRect; Addend2: TRect): TDoubleRect;
operator - (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
operator - (Addend1, Addend2: TRect): TRect;

operator * (Addend1, Addend2: TDoublePoint): TDoublePoint;
operator * (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
operator * (Addend1: TDoublePoint; Addend2: TPoint): TDoublePoint;
operator * (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
operator * (Addend1, Addend2: TPoint): TPoint;

operator * (Addend1, Addend2: TDoubleRect): TDoubleRect;
operator * (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
operator * (Addend1: TDoubleRect; Addend2: TRect): TDoubleRect;
operator * (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
operator * (Addend1, Addend2: TRect): TRect;

operator / (Addend1, Addend2: TDoublePoint): TDoublePoint;
operator / (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
operator / (Addend1: TDoublePoint; Addend2: TPoint): TDoublePoint;
operator / (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
operator / (Addend1, Addend2: TPoint): TPoint;

operator / (Addend1, Addend2: TDoubleRect): TDoubleRect;
operator / (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
operator / (Addend1: TDoubleRect; Addend2: TRect): TDoubleRect;
operator / (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
operator / (Addend1, Addend2: TRect): TRect;

function Round(Rounded: TDoublePoint): TPoint; overload;
function Trunc(Trunced: TDoublePoint): TPoint; overload;
function Round(Rounded: TDoubleRect): TRect; overload;
function Trunc(Trunced: TDoubleRect): TRect; overload;

function ExpandRect(Explanded: TDoublePoint; Offset: Double): TDoubleRect; overload;
function ExpandPoint(Explanded: TDoublePoint; Offset: Double): TDoublePoint; overload;

function DoublePoint(AX, AY: Double): TDoublePoint;
function DoubleRect(AX, AY, BX, BY: Double): TDoubleRect; overload;
function DoubleRect(PointStart, PointEnd: TDoublePoint): TDoubleRect; overload;

function MaxRect(APoint1, APoint2: TDoublePoint): TDoubleRect;
function MaxRect(ARect: TDoubleRect; APoint: TDoublePoint): TDoubleRect;
function MaxRect(APoint: TDoublePoint; ARect: TDoubleRect): TDoubleRect;

operator = (Compare1, Compare2: TDoublePoint): Boolean;
operator = (Compare1, Compare2: TDoubleRect): Boolean;
operator = (Compare1, Compare2: TPoint): Boolean;
operator = (Compare1, Compare2: TRect): Boolean;

operator := (Value: TDoublePoint): TPoint;
operator := (Value: TPoint): TDoublePoint;
operator := (Value: TDoubleRect): TRect;
operator := (Value: TRect): TDoubleRect;

operator + (Value: TDoublePoint): TPoint;
operator + (Value: TPoint): TDoublePoint;
operator + (Value: TDoubleRect): TRect;
operator + (Value: TRect): TDoubleRect;

operator - (Value: TDoublePoint): TPoint;
operator - (Value: TPoint): TDoublePoint;
operator - (Value: TDoubleRect): TRect;
operator - (Value: TRect): TDoubleRect;

implementation

// functions

function ExpandRect(Explanded: TDoublePoint; Offset: Double): TDoubleRect;
begin
  Result.TopLeft := Explanded - Offset;
  Result.BottomRight := Explanded + Offset;
end;

function ExpandPoint(Explanded: TDoublePoint; Offset: Double): TDoublePoint;
begin
  Result.X := Explanded.X + Offset;
  Result.Y := Explanded.Y - Offset;
end;

function DoublePoint(AX, AY: Double): TDoublePoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function DoubleRect(AX, AY, BX, BY: Double): TDoubleRect;
begin
  Result.TopLeft := DoublePoint(AX, AY);
  Result.BottomRight := DoublePoint(BX, BY);
end;

function DoubleRect(PointStart, PointEnd: TDoublePoint): TDoubleRect;
begin
  Result.TopLeft := PointStart;
  Result.BottomRight := PointEnd;
end;

function Round(Rounded: TDoublePoint): TPoint;
begin
  Result.X := Round(Rounded.X);
  Result.Y := Round(Rounded.Y);
end;

function Trunc(Trunced: TDoublePoint): TPoint;
begin
  Result.X := Trunc(Trunced.X);
  Result.Y := Trunc(Trunced.Y);
end;

function Round(Rounded: TDoubleRect): TRect;
begin
  Result.TopLeft := Round(Rounded.TopLeft);
  Result.BottomRight := Round(Rounded.BottomRight);
end;

function Trunc(Trunced: TDoubleRect): TRect;
begin
  Result.TopLeft := Trunc(Trunced.TopLeft);
  Result.BottomRight := Trunc(Trunced.BottomRight);
end;

function MaxRect(APoint1, APoint2: TDoublePoint): TDoubleRect;
begin
  Result.Left := Min(APoint1.X, APoint2.X);
  Result.Top := Min(APoint1.Y, APoint2.Y);
  Result.Right := Max(APoint1.X, APoint2.X);
  Result.Bottom := Max(APoint1.Y, APoint2.Y);
end;

function MaxRect(ARect: TDoubleRect; APoint: TDoublePoint): TDoubleRect;
begin
  Result.Left := Min(ARect.Left, APoint.X);
  Result.Top := Min(ARect.Top, APoint.Y);
  Result.Right := Max(ARect.Right, APoint.X);
  Result.Bottom := Max(ARect.Bottom, APoint.Y);
end;

function MaxRect(APoint: TDoublePoint; ARect: TDoubleRect): TDoubleRect;
begin
  Result.Left := Min(ARect.Left, APoint.X);
  Result.Top := Min(ARect.Top, APoint.Y);
  Result.Right := Max(ARect.Right, APoint.X);
  Result.Bottom := Max(ARect.Bottom, APoint.Y);
end;

// operators

operator + (Addend1, Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X + Addend2.X;
  Result.Y := Addend1.Y + Addend2.Y;
end;

operator + (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
begin
  Result.X := Addend1.X + Addend2;
  Result.Y := Addend1.Y + Addend2;
end;

operator + (Addend1: TDoublePoint; Addend2 : TPoint): TDoublePoint;
begin
  Result.X := Addend1.X + Addend2.X;
  Result.Y := Addend1.Y + Addend2.Y;
end;

operator + (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X + Addend2.X;
  Result.Y := Addend1.Y + Addend2.Y;
end;

operator + (Addend1, Addend2: TPoint): TPoint;
begin
  Result.X := Addend1.X + Addend2.X;
  Result.Y := Addend1.Y + Addend2.Y;
end;

operator + (Addend1, Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft + Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight + Addend2.BottomRight;
end;

operator + (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft + Addend2;
  Result.BottomRight := Addend1.BottomRight + Addend2;
end;

operator + (Addend1: TDoubleRect; Addend2 : TRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft + Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight + Addend2.BottomRight;
end;

operator + (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft + Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight + Addend2.BottomRight;
end;

operator + (Addend1, Addend2: TRect): TRect;
begin
  Result.TopLeft := Addend1.TopLeft + Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight + Addend2.BottomRight;
end;


operator - (Addend1, Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X - Addend2.X;
  Result.Y := Addend1.Y - Addend2.Y;
end;

operator - (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
begin
  Result.X := Addend1.X - Addend2;
  Result.Y := Addend1.Y - Addend2;
end;

operator - (Addend1: TDoublePoint; Addend2 : TPoint): TDoublePoint;
begin
  Result.X := Addend1.X - Addend2.X;
  Result.Y := Addend1.Y - Addend2.Y;
end;

operator - (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X - Addend2.X;
  Result.Y := Addend1.Y - Addend2.Y;
end;

operator - (Addend1, Addend2: TPoint): TPoint;
begin
  Result.X := Addend1.X - Addend2.X;
  Result.Y := Addend1.Y - Addend2.Y;
end;

operator - (Addend1, Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft - Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight - Addend2.BottomRight;
end;

operator - (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft - Addend2;
  Result.BottomRight := Addend1.BottomRight - Addend2;
end;

operator - (Addend1: TDoubleRect; Addend2 : TRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft - Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight - Addend2.BottomRight;
end;

operator - (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft - Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight - Addend2.BottomRight;
end;

operator - (Addend1, Addend2: TRect): TRect;
begin
  Result.TopLeft := Addend1.TopLeft - Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight - Addend2.BottomRight;
end;


operator * (Addend1, Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X * Addend2.X;
  Result.Y := Addend1.Y * Addend2.Y;
end;

operator * (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
begin
  Result.X := Addend1.X * Addend2;
  Result.Y := Addend1.Y * Addend2;
end;

operator * (Addend1: TDoublePoint; Addend2: TPoint): TDoublePoint;
begin
  Result.X := Addend1.X * Addend2.X;
  Result.Y := Addend1.Y * Addend2.Y;
end;

operator * (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X * Addend2.X;
  Result.Y := Addend1.Y * Addend2.Y;
end;

operator * (Addend1, Addend2: TPoint): TPoint;
begin
  Result.X := Addend1.X * Addend2.X;
  Result.Y := Addend1.Y * Addend2.Y;
end;

operator * (Addend1, Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft * Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight * Addend2.BottomRight;
end;

operator * (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft * Addend2;
  Result.BottomRight := Addend1.BottomRight * Addend2;
end;

operator * (Addend1: TDoubleRect; Addend2 : TRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft * Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight * Addend2.BottomRight;
end;

operator * (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft * Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight * Addend2.BottomRight;
end;

operator * (Addend1, Addend2: TRect): TRect;
begin
  Result.TopLeft := Addend1.TopLeft * Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight * Addend2.BottomRight;
end;


operator / (Addend1, Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X / Addend2.X;
  Result.Y := Addend1.Y / Addend2.Y;
end;

operator / (Addend1: TDoublePoint; Addend2: Double): TDoublePoint;
begin
  Result.X := Addend1.X / Addend2;
  Result.Y := Addend1.Y / Addend2;
end;

operator / (Addend1: TDoublePoint; Addend2: TPoint): TDoublePoint;
begin
  Result.X := Addend1.X / Addend2.X;
  Result.Y := Addend1.Y / Addend2.Y;
end;

operator / (Addend1: TPoint; Addend2: TDoublePoint): TDoublePoint;
begin
  Result.X := Addend1.X / Addend2.X;
  Result.Y := Addend1.Y / Addend2.Y;
end;

operator / (Addend1, Addend2: TPoint): TPoint;
begin
  Result.X := Round(Addend1.X / Addend2.X);
  Result.Y := Round(Addend1.Y / Addend2.Y);
end;

operator / (Addend1, Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft / Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight / Addend2.BottomRight;
end;

operator / (Addend1: TDoubleRect; Addend2: Double): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft / Addend2;
  Result.BottomRight := Addend1.BottomRight / Addend2;
end;

operator / (Addend1: TDoubleRect; Addend2 : TRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft / Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight / Addend2.BottomRight;
end;

operator / (Addend1: TRect; Addend2: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Addend1.TopLeft / Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight / Addend2.BottomRight;
end;

operator / (Addend1, Addend2: TRect): TRect;
begin
  Result.TopLeft := Addend1.TopLeft / Addend2.TopLeft;
  Result.BottomRight := Addend1.BottomRight / Addend2.BottomRight;
end;


operator = (Compare1, Compare2 : TDoublePoint): Boolean;
begin
  Result := (Compare1.X = Compare2.X) and (Compare1.Y = Compare2.Y);
end;

operator = (Compare1, Compare2 : TDoubleRect): Boolean;
begin
  Result := (Compare1.TopLeft = Compare2.TopLeft) and
            (Compare1.BottomRight = Compare2.BottomRight);
end;

operator = (Compare1, Compare2: TPoint): Boolean;
begin
  Result := (Compare1.X = Compare2.X) and (Compare1.Y = Compare2.Y);
end;

operator = (Compare1, Compare2: TRect): Boolean;
begin
  Result := (Compare1.TopLeft = Compare2.TopLeft) and
            (Compare1.BottomRight = Compare2.BottomRight);
end;


operator := (Value: TDoublePoint): TPoint;
begin
  Result.X := Round(Value.X);
  Result.Y := Round(Value.Y);
end;

operator := (Value: TPoint): TDoublePoint;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

operator := (Value: TDoubleRect): TRect;
begin
  Result.TopLeft := Value.TopLeft;
  Result.BottomRight := Value.BottomRight;
end;

operator := (Value: TRect): TDoubleRect;
begin
  Result.TopLeft := Value.TopLeft;
  Result.BottomRight := Value.BottomRight;
end;


operator - (Value: TDoublePoint): TPoint;
begin
  Result := Value * -1;
end;

operator - (Value: TPoint): TDoublePoint;
begin
  Result := Value * -1;
end;

operator - (Value: TDoubleRect): TRect;
begin
  Result := Value * -1;
end;

operator - (Value: TRect): TDoubleRect;
begin
  Result := Value * -1;
end;


operator + (Value: TDoublePoint): TPoint;
begin
  Result := Value;
end;

operator + (Value: TPoint): TDoublePoint;
begin
  Result := Value;
end;

operator + (Value: TDoubleRect): TRect;
begin
  Result := Value;
end;

operator + (Value: TRect): TDoubleRect;
begin
  Result := Value;
end;

end.

