unit UNotVisualTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UTransformation, UGraph;

type
  TNotVisible = Class
  protected
    _SelObj: Integer;
    _SelNode: Integer;
  public
    DynMRect: TDoubleRect;
    DynMPoint: TDoublePoint;
    property SelObj: Integer read _SelObj write _SelObj;
    property SelNode: Integer read _SelNode write _SelNode;
    procedure DynDraw(Canva: TCanvas);
  end;

  TSelect = Class(TNotVisible)
  end;

  TZoom = Class(TNotVisible)
  end;

  TRelocate = Class(TNotVisible)
  end;

implementation

procedure TNotVisible.DynDraw(Canva: TCanvas);
begin
  With Canva do begin
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psDot;
    Brush.Style := bsClear;
    Brush.Color := clWhite;
    Rectangle(Trans.W2S(DynMRect));
  end;
end;

end.

