{
@abstract(Geometry stuff)

Distance functions and other geometry related routines.

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3geom;
{$I g3config.inc}

interface

uses
  g3types;

  function BoxDistance(const Point: TVector2f; const Min: TVector2f; const Max: TVector2f): Single;

implementation

uses
  g3common,
  g3vector;

function BoxDistance(const Point: TVector2f; const Min: TVector2f; const Max: TVector2f): Single;
var
  dx, dy, x, y: Single;
begin
  dx := (Max.x - Min.x)*0.5;
  dy := (Max.y - Min.y)*0.5;
  x := abs(Point.x - (Max.x + Min.x)*0.5) - dx;
  y := abs(Point.y - (Max.y + Min.y)*0.5) - dy;
  Result := VectorMagnitude(Vec2f(MaxS(0, x), MaxS(0, y))) + MinS(0, MaxS(x, y));
end;

end.
