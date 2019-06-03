{
@abstract(Vector math stuff)

Vector, matrix and quaternion math types and routines.

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3vector;
{$I g3config.inc}

interface

uses
  g3types;

type
  // Base float type
  TG3Float = Single;

  // 3x3 matrix
  TMatrix3f = array[0..2, 0..2] of TG3Float;
  // 4x4 matrix
  TMatrix4f = array[0..3, 0..3] of TG3Float;

  // Line or segment intersection test result
  TIntersectResult = (irIntersect, irCoincident, irParallel, irOutOfSegment);

  function VectorNormalize(const V: TVector2f): TVector2f; overload;
  procedure VectorNormalize(out Result: TVector2f; const V: TVector2f); overload;
  procedure VectorNormalize(out Result: TVector2f; const V: TVector2f; len: Single); overload;
  function VectorNormalize(const V: TVector3f): TVector3f; overload;
  function VectorAdd(const V1, V2: TVector2f): TVector2f; overload;
  procedure VectorAdd(out Result: TVector2f; const V1, V2: TVector2f); overload;
  function VectorAdd(const V1, V2: TVector3f): TVector3f; overload;
  procedure VectorAdd(out Result: TVector3f; const V1, V2: TVector3f); overload;
  function VectorAdd(const V1, V2: TVector4f): TVector4f; overload;
  procedure VectorAdd(out Result: TVector4f; const V1, V2: TVector4f); overload;
  function VectorSub(const V1, V2: TVector2f): TVector2f; overload;
  procedure VectorSub(out Result: TVector2f; const V1, V2: TVector2f); overload;
  function VectorSub(const V1, V2: TVector3f): TVector3f; overload;
  procedure VectorSub(out Result: TVector3f; const V1, V2: TVector3f); overload;
  function VectorSub(const V1, V2: TVector4f): TVector4f; overload;
  procedure VectorSub(out Result: TVector4f; const V1, V2: TVector4f); overload;
  function VectorScale(const V: TVector2f; const Factor: Single): TVector2f; overload;
  procedure VectorScale(out Result: TVector2f; const V: TVector2f; const Factor: Single); overload;
  function VectorScale(const V: TVector3f; const Factor: Single): TVector3f; overload;
  procedure VectorScale(out Result: TVector3f; const V: TVector3f; const Factor: Single); overload;
  function VectorScale(const V: TVector4f; const Factor: Single): TVector4f; overload;
  procedure VectorScale(out Result: TVector4f; const V: TVector4f; const Factor: Single); overload;
  function VectorMagnitude(const V: TVector2f): Single; overload;
  function VectorMagnitude(const V: TVector3f): Single; overload;
  function VectorMagnitudeSq(const V: TVector2f): Single; overload;
  function VectorMagnitudeSq(const V: TVector3f): Single; overload;
  function LineIntersect(const AP1, AP2, BP1, BP2: TVector2f; out Hit: TVector2f): TIntersectResult;
  function RayIntersect(const AP1, ADir, BP1, BDir: TVector2f; out Hit: TVector2f): TIntersectResult;
  function SegmentIntersect(const AP1, AP2, BP1, BP2: TVector2f; out Hit: TVector2f): TIntersectResult;
  // Signed area of ABC triangle > 0 if points specified in CCW order and < 0 otherwise
  function SignedAreaX2(const A, B, C: TVector2f): Single; overload;
  // Signed area of a triangle with edges AB and AC > 0 if points specified in CCW order and < 0 otherwise
  function SignedAreaX2(const AB, AC: TVector2f): Single; overload;

  function VectorDot(const V1, V2: TVector2f): Single; overload;
  function VectorDot(const V1, V2: TVector3f): Single; overload;
  function VectorCross(const V1, V2: TVector3f): TVector3f; overload;
  function VectorReflect(const V, N: TVector2f): TVector2f; overload;
  function VectorReflect(const V, N: TVector3f): TVector3f; overload;
  function GetNearestPointIndex(const Points: P2DPointArray; Count: Integer; const Point: TVector2f): Integer;

implementation

function VectorNormalize(const V: TVector2f): TVector2f; overload;
var
  Sq: Single;
  Zero: Integer;
begin
  Sq := Sqrt(sqr(V.X) + sqr(V.Y));
  Zero := Ord(Sq = 0);
  Sq := (1 - Zero) / (Sq + Zero);
  Result.X := Sq * V.X;
  Result.Y := Sq * V.Y;
end;

function VectorNormalize(const V: TVector3f): TVector3f; overload;
var
  Sq: Single;
  Zero: Integer;
begin
  Sq := Sqrt(sqr(V.X) + sqr(V.Y) + sqr(V.Z));
  Zero := Ord(Sq = 0);
  Sq := (1 - Zero) / (Sq + Zero);
  Result.X := Sq * V.X;
  Result.Y := Sq * V.Y;
  Result.Z := Sq * V.Z;
end;

procedure VectorNormalize(out Result: TVector2f; const V: TVector2f); overload;
var
  Sq: Single;
  Zero: Integer;
begin
  Sq := Sqrt(sqr(V.X) + sqr(V.Y));       // TODO: switch to invsqrt()
  Zero := Ord(Sq = 0);
  Sq := (1 - Zero) / (Sq + Zero);
  Result.X := Sq * V.X;
  Result.Y := Sq * V.Y;
end;

procedure VectorNormalize(out Result: TVector2f; const V: TVector2f; len: Single); overload;
var
  Sq: Single;
  Zero: Integer;
begin
  Sq := Sqrt(sqr(V.X) + sqr(V.Y));       // TODO: switch to invsqrt()
  Zero := Ord(Sq = 0);
  Sq := len * (1 - Zero) / (Sq + Zero);
  Result.X := Sq * V.X;
  Result.Y := Sq * V.Y;
end;

function VectorAdd(const V1, V2: TVector2f): TVector2f; overload;
begin
  Result.X := V1.X + V2.X;
  Result.Y := V1.Y + V2.Y;
end;

procedure VectorAdd(out Result: TVector2f; const V1, V2: TVector2f); overload;
begin
  Result.X := V1.X + V2.X;
  Result.Y := V1.Y + V2.Y;
end;

function VectorAdd(const V1, V2: TVector3f): TVector3f; overload;
begin
  with Result do begin
    X := V1.X + V2.X;
    Y := V1.Y + V2.Y;
    Z := V1.Z + V2.Z;
  end;
end;

procedure VectorAdd(out Result: TVector3f; const V1, V2: TVector3f); overload;
begin
  with Result do begin
    X := V1.X + V2.X;
    Y := V1.Y + V2.Y;
    Z := V1.Z + V2.Z;
  end;
end;

function VectorAdd(const V1, V2: TVector4f): TVector4f; overload;
begin
  with Result do begin
    X := V1.X + V2.X;
    Y := V1.Y + V2.Y;
    Z := V1.Z + V2.Z;
    W := V1.W + V2.W;
  end;
end;

procedure VectorAdd(out Result: TVector4f; const V1, V2: TVector4f); overload;
begin
  with Result do begin
    X := V1.X + V2.X;
    Y := V1.Y + V2.Y;
    Z := V1.Z + V2.Z;
    W := V1.W + V2.W;
  end;
end;

function VectorSub(const V1, V2: TVector2f): TVector2f; overload;
begin
  Result.X := V1.X - V2.X;
  Result.Y := V1.Y - V2.Y;
end;

procedure VectorSub(out Result: TVector2f; const V1, V2: TVector2f); overload;
begin
  Result.X := V1.X - V2.X;
  Result.Y := V1.Y - V2.Y;
end;

function VectorSub(const V1, V2: TVector3f): TVector3f; overload;
begin
  with Result do begin
    X := V1.X - V2.X;
    Y := V1.Y - V2.Y;
    Z := V1.Z - V2.Z;
  end;
end;

procedure VectorSub(out Result: TVector3f; const V1, V2: TVector3f); overload;
begin
  with Result do begin
    X := V1.X - V2.X;
    Y := V1.Y - V2.Y;
    Z := V1.Z - V2.Z;
  end;
end;

function VectorSub(const V1, V2: TVector4f): TVector4f; overload;
begin
  with Result do begin
    X := V1.X - V2.X;
    Y := V1.Y - V2.Y;
    Z := V1.Z - V2.Z;
    W := V1.W - V2.W;
  end;
end;

procedure VectorSub(out Result: TVector4f; const V1, V2: TVector4f); overload;
begin
  with Result do begin
    X := V1.X - V2.X;
    Y := V1.Y - V2.Y;
    Z := V1.Z - V2.Z;
    W := V1.W - V2.W;
  end;
end;

function VectorScale(const V: TVector2f; const Factor: Single): TVector2f; overload;
begin
  Result.X := V.X * Factor;
  Result.Y := V.Y * Factor;
end;

procedure VectorScale(out Result: TVector2f; const V: TVector2f; const Factor: Single); overload;
begin
  Result.X := V.X * Factor;
  Result.Y := V.Y * Factor;
end;

function VectorScale(const V: TVector3f; const Factor: Single): TVector3f; overload;
begin
  Result.X := V.X * Factor;
  Result.Y := V.Y * Factor;
  Result.Z := V.Z * Factor;
end;

procedure VectorScale(out Result: TVector3f; const V: TVector3f; const Factor: Single); overload;
begin
  Result.X := V.X * Factor;
  Result.Y := V.Y * Factor;
  Result.Z := V.Z * Factor;
end;

function VectorScale(const V: TVector4f; const Factor: Single): TVector4f; overload;
begin
  Result.X := V.X * Factor;
  Result.Y := V.Y * Factor;
  Result.Z := V.Z * Factor;
  Result.W := V.W * Factor;
end;

procedure VectorScale(out Result: TVector4f; const V: TVector4f; const Factor: Single); overload;
begin
  Result.X := V.X * Factor;
  Result.Y := V.Y * Factor;
  Result.Z := V.Z * Factor;
  Result.W := V.W * Factor;
end;

function VectorMagnitude(const V: TVector2f): Single; overload;
begin
  Result := Sqrt(Sqr(V.X) + Sqr(V.Y));
end;

function VectorMagnitude(const V: TVector3f): Single; overload;
begin
  Result := Sqrt(Sqr(V.X) + Sqr(V.Y) + Sqr(V.Z));
end;

function VectorMagnitudeSq(const V: TVector2f): Single; overload;
begin
  Result := Sqr(V.X) + Sqr(V.Y);
end;

function VectorMagnitudeSq(const V: TVector3f): Single; overload;
begin
  Result := Sqr(V.X) + Sqr(V.Y) + Sqr(V.Z);
end;

const
  EPSILON = 0.00001;

function LineIntersect(const AP1, AP2, BP1, BP2: TVector2f; out Hit: TVector2f): TIntersectResult;
begin
  Result := RayIntersect(AP1, Vec2f(AP2.x - AP1.x, AP2.y - AP1.y), BP1, Vec2f(BP2.x - BP1.x, BP2.y - BP1.y), Hit);
end;

function RayIntersect(const AP1, ADir, BP1, BDir: TVector2f; out Hit: TVector2f): TIntersectResult;
var
  Denominator, NumA, NumB: Single;
  Ua: Single;
begin
  Denominator := BDir.y * ADir.x - BDir.x * ADir.y;
  NumA := BDir.x * (AP1.y - BP1.y) - BDir.y * (AP1.x - BP1.x);
  NumB := ADir.x * (AP1.y - BP1.y) - ADir.y * (AP1.x - BP1.x);
  if (Abs(Denominator) < EPSILON) then
  begin
    if (Abs(NumA) < EPSILON) and (Abs(NumB) < EPSILON) then
      Result := irCoincident
    else
      Result := irParallel;
  end else begin
    Denominator := 1 / Denominator;
    ua := NumA * Denominator;
    Hit.X := AP1.x + ua * ADir.x;
    Hit.Y := AP1.y + ua * ADir.y;
    Result := irIntersect;
  end;
end;

function SegmentIntersect(const AP1, AP2, BP1, BP2: TVector2f; out Hit: TVector2f): TIntersectResult;
var
  Denominator, NumA, NumB: Single;
  a, Ub: Single;
begin
  Denominator := (BP2.y - BP1.y) * (AP2.x - AP1.x) - (BP2.x - BP1.x) * (AP2.y - AP1.y);
  NumA := (BP2.x - BP1.x) * (AP1.y - BP1.y) - (BP2.y - BP1.y) * (AP1.x - BP1.x);
  NumB := (AP2.x - AP1.x) * (AP1.y - BP1.y) - (AP2.y - AP1.y) * (AP1.x - BP1.x);

  if (Abs(Denominator) < EPSILON) then
  begin
    if (Abs(NumA) < EPSILON) and (Abs(NumB) < EPSILON) then
      Result := irCoincident
    else
      Result := irParallel;
  end else begin
    Denominator := 1 / Denominator;
    a := NumA * Denominator;
    ub := NumB * Denominator;

    if (a >= 0.0) and (a <= 1.0) and (ub >= 0.0) and (ub <= 1.0) then
    begin
      Hit.X := AP1.x + a * (AP2.x - AP1.x);
      Hit.Y := AP1.y + a * (AP2.y - AP1.y);
      Result := irIntersect;
    end else
      Result := irOutOfSegment;
  end;
end;

function SignedAreaX2(const A, B, C: TVector2f): Single; overload;
begin
  Result := (B.x - A.x) * (C.y - A.y) - (B.y - A.y) * (C.x - A.x);
end;

function SignedAreaX2(const AB, AC: TVector2f): Single; overload;
begin
  Result := AB.x * AC.y - AB.y * AC.x;
end;

function VectorDot(const V1, V2: TVector2f): Single; overload;
begin
  Result := V1.X*V2.X + V1.Y*V2.Y;
end;

function VectorDot(const V1, V2: TVector3f): Single; overload;
begin
  Result := V1.X*V2.X + V1.Y*V2.Y + V1.Z*V2.Z;
end;

function VectorCross(const V1, V2: TVector3f): TVector3f; overload;
begin
  Result.X := V1.Y*V2.Z - V1.Z*V2.Y;
  Result.Y := V1.Z*V2.X - V1.X*V2.Z;
  Result.Z := V1.X*V2.Y - V1.Y*V2.X;
end;

function VectorReflect(const V, N: TVector2f): TVector2f; overload;
// N - reflecting surface's normal
var d : Single;
begin
  d := -VectorDot(V, N) * 2;
  Result.X := (d * N.X) + V.X;
  Result.Y := (d * N.Y) + V.Y;
end;

function VectorReflect(const V, N: TVector3f): TVector3f; overload;
// N - reflecting surface's normal
var d : Single;
begin
  d := -VectorDot(V, N) * 2;
  Result.X := (d * N.X) + V.X;
  Result.Y := (d * N.Y) + V.Y;
  Result.Z := (d * N.Z) + V.Z;
end;

function GetNearestPointIndex(const Points: P2DPointArray; Count: Integer; const Point: TVector2f): Integer;
var
  i: Integer;
  dist, maxDist: Single;
  P: ^TVector2f;
begin
  Result := 0;
  P := @Points^[0];
  maxDist := VectorMagnitudeSq(VectorSub(P^, Point));
  for i := 1 to Count - 1 do
  begin
    Inc(P);
    Dist := VectorMagnitudeSq(VectorSub(P^, Point));
    if Dist < maxDist then
    begin
      Result := i;
      maxDist := Dist;
    end;
  end;
end;

end.
