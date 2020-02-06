{
@abstract(Spline calculation unit)

The unit contains spline calculation types and routines

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3spline;
{$I g3config.inc}

interface

uses
  g3types;

type
  TSingleBuffer = array[0..MaxInt div SizeOf(Single) - 1] of Single;
  PSingleBuffer = ^TSingleBuffer;

  TSplineCoeff = record
    A, B, C, D: Single;
  end;

  TSplineCoeffs = array[0..MaxInt div SizeOf(TSplineCoeff) - 1] of TSplineCoeff;
  PSplineCoeffs = ^TSplineCoeffs;

  TSpline = class(TObject)
  private
    FCoeffsDirty: Boolean;
    FCoeff: PSplineCoeffs;
    FStride: Integer;
    FSamples: PSingleBuffer;
    FSampleCount: Integer;
    procedure SetSampleCount(Count: Integer);
    procedure CalcCoeffs(StartIndex, EndIndex: Integer);
    function GetInterpolated(Param: Single; Channel: Integer): Single;
    function GetInterpolatedTangent(Param: Single; Channel: Integer): Single;
    function GetSegmentCount(): Integer;
  public
    constructor Create(Stride: Integer);
    destructor Destroy(); override;
    procedure SetSamples(Count: Integer; ASamples: PSingleBuffer);
    procedure FindExtremums(Segment, Channel: Integer; out R1, R2: Single);
    procedure FindInflection(Segment, Channel: Integer; out R: Single);
    property Samples: PSingleBuffer read FSamples;
    property SampleCount: Integer read FSampleCount write SetSampleCount;
    property SegmentCount: Integer read GetSegmentCount;
  end;

  TSpline1D = class;

  TSplineIterator = class(TObject)
  private
    FSpline: TSpline;
    t, maxT: Single;
  end;

  TIterStep1D = class(TSplineIterator)
  private
    FStep: Single;
  public
    constructor Create(Spline: TSpline1D; Step: Single);
    function GetEnumerator(): TIterStep1D;
    function MoveNext(): Boolean;
    function GetCurrent(): TVector2f;
    property Current: TVector2f read GetCurrent;
  end;

  TSpline1D = class(TSpline)
  private
    function GetSample(Index: Integer): Single;
    procedure SetSample(Index: Integer; AValue: Single);
    function GetInterpolated(Param: Single): Single;
    function GetInterpolatedTangent(Param: Single): Single;
  public
    constructor Create();
    function GetEnumerator(): TIterStep1D;
    function WithStep(Step: Single): TIterStep1D;
    property Sample[Index: Integer]: Single read GetSample write SetSample;
    property Value[T: Single]: Single read GetInterpolated; default;
    property Tangent[T: Single]: Single read GetInterpolatedTangent;
  end;

  TSpline2D = class;

  TIterStep2D = class(TSplineIterator)
  private
    FStep: Single;
    FLast: TVector2f;
    FEndMarker: Integer;
  public
    constructor Create(Spline: TSpline2D; Step: Single);
    function GetEnumerator(): TIterStep2D;
    function MoveNext(): Boolean;
    function GetCurrent(): TVector2f;
    property Current: TVector2f read GetCurrent;
  end;

  TSpline2D = class(TSpline)
  private
    function GetSample(Index: Integer): TVector2f;
    procedure SetSample(Index: Integer; AValue: TVector2f);
    function GetInterpolated(Param: Single): TVector2f;
    function GetInterpolatedTangent(Param: Single): TVector2f;
  public
    constructor Create();
    function GetEnumerator(): TIterStep2D;
    function WithStep(Step: Single): TIterStep2D;
    function GetNearestParamApprox(const Point: TVector2f; subdiv: integer): Single;
    function GetNearestSegment(const Point: TVector2f): Integer;
    function GetNearestParamApproxOpt(const Point: TVector2f; subdiv: integer): Single;
    // Axis aligned bounding box of the spline (x and y coordinates are used)
    function GetAABB(): TAABB;
    property Sample[Index: Integer]: TVector2f read GetSample write SetSample;
    property Value[T: Single]: TVector2f read GetInterpolated; default;
    property Tangent[T: Single]: TVector2f read GetInterpolatedTangent;
    property AABB: TAABB read GetAABB;
  end;

implementation

uses
  g3common,
  g3vector,
  g3geom,
  g3log,
  sysutils;

type
  TParamArray = array[0..7] of Single;

{ TSpline }

function TSpline.GetInterpolated(Param: Single; Channel: Integer): Single;
var
  Segment: Integer;
  T, T2: Single;
begin
  Assert(Param <= FSampleCount);
  Assert(Channel < FStride);
  if FCoeffsDirty then
    CalcCoeffs(0, FSampleCount - 2);
  Segment := MinI(FSampleCount - 2, trunc(Param));
  T := Param - Segment;
  Segment := Segment * FStride + Channel;
  T2 := Sqr(T);
  Result := FCoeff[Segment].A * T2 * T + FCoeff[Segment].B * T2 + FCoeff[Segment].C * T + FCoeff[Segment].D;
end;

function TSpline.GetInterpolatedTangent(Param: Single; Channel: Integer): Single;
var
  Segment: Integer;
  T: Single;
begin
  Assert(Param <= FSampleCount);
  Assert(Channel < FStride);
  if FCoeffsDirty then
    CalcCoeffs(0, FSampleCount - 2);
  Segment := MinI(FSampleCount - 2, trunc(Param));
  T := Param - Segment;
  Segment := Segment * FStride + Channel;
  Result := 3 * FCoeff[Segment].A * T * T + 2 * FCoeff[Segment].B * T + FCoeff[Segment].C;
end;

procedure TSpline.SetSampleCount(Count: Integer);
begin
  if FSampleCount = Count then Exit;
  FSampleCount := Count;
  ReallocMem(FSamples, FSampleCount * SizeOf(Single) * FStride);
  ReallocMem(FCoeff, (FSampleCount - 1) * SizeOf(TSplineCoeff) * FStride);
  FCoeffsDirty := true;
end;

procedure TSpline.CalcCoeffs(StartIndex, EndIndex: Integer);
var
  i0, i1, i2, i3, j: Integer;
begin
  Assert((StartIndex >= 0) and (StartIndex < FSampleCount - 1), 'Invalid start index');
  Assert((EndIndex >= 0) and (EndIndex < FSampleCount - 1) and (StartIndex <= EndIndex), 'Invalid end index');
  if FSampleCount < 2 then Exit;
  for i1 := StartIndex to EndIndex do
  begin
    for j := 0 to FStride - 1 do
    begin
      i0 := MaxI(0, i1 - 1) * FStride + j;
      i2 := MinI(FSampleCount - 1, i1 + 1) * FStride + j;
      i3 := MinI(FSampleCount - 1, i1 + 2) * FStride + j;
      FCoeff^[i1 * FStride + j].A := 0.5 * (-FSamples^[i0] + 3 * FSamples^[i1 * FStride + j] - 3 * FSamples^[i2] + FSamples^[i3]);
      FCoeff^[i1 * FStride + j].B := FSamples^[i0] - 2.5 * FSamples^[i1 * FStride + j] + 2 * FSamples^[i2] - 0.5 * FSamples^[i3];
      FCoeff^[i1 * FStride + j].C := 0.5 * (-FSamples^[i0] + FSamples^[i2]);
      FCoeff^[i1 * FStride + j].D := FSamples^[i1 * FStride + j];
    end;
  end;
  FCoeffsDirty := false;
end;

function TSpline.GetSegmentCount(): Integer;
begin
  Result := SampleCount - 1;
end;

constructor TSpline.Create(Stride: Integer);
begin
  FStride := Stride;
  FCoeffsDirty := true;
end;

destructor TSpline.Destroy();
begin
  if Assigned(FSamples) then
    FreeMem(FSamples);
  if Assigned(FCoeff) then
    FreeMem(FCoeff);
  inherited;
end;

procedure TSpline.SetSamples(Count: Integer; ASamples: PSingleBuffer);
begin
  if FSampleCount = Count then
    exit;
  FSampleCount := Count;
  FSamples := ASamples;
  if Assigned(FCoeff) then
    FreeMem(FCoeff);
  if FSampleCount > 0 then
    GetMem(FCoeff, (FSampleCount - 1) * SizeOf(TSplineCoeff) * FStride)
  else
    FCoeff := nil;
  FCoeffsDirty := true;
end;

procedure findRoots(const K: TSplineCoeff; var Result: TParamArray; var Count: Integer);
var
  A, B, C, D: Single;
begin
  A := 3 * K.A;
  B := 2 * K.B;
  C := K.C;
  if A = 0 then
  begin
    if B <> 0 then
    begin
      Result[Count] := -C / B;
      Count := Count + Ord((Result[Count] > 0) and (Result[Count] < 1));
    end;
    exit;
  end;
  D := B * B - 4 * A * C;
  if D > 0 then
  begin
    Result[Count] := (-B - sqrt(D)) / (2 * A);
    Count := Count + Ord((Result[Count] > 0) and (Result[Count] < 1));
    Result[Count] := (-B + sqrt(D)) / (2 * A);
    Count := Count + Ord((Result[Count] > 0) and (Result[Count] < 1));
  end else if D = 0 then
  begin
    Result[Count] := -B / (2 * A);
    Count := Count + Ord((Result[Count] > 0) and (Result[Count] < 1));
  end;
end;

procedure TSpline.FindExtremums(Segment, Channel: Integer; out R1, R2: Single);
var
  Result: TParamArray;
  Count: Integer;
begin
  if FCoeffsDirty then
    CalcCoeffs(0, FSampleCount - 2);
  Count := 0;
  findRoots(FCoeff^[Segment * FStride + Channel], Result, Count);
  R1 := Ord(Count > 0) * Result[0] - Ord(Count <= 0);
  R2 := Ord(Count > 1) * Result[1] - Ord(Count <= 1);
end;

procedure TSpline.FindInflection(Segment, Channel: Integer; out R: Single);
begin
  if FCoeffsDirty then
    CalcCoeffs(0, FSampleCount - 2);
  if FCoeff^[Segment * FStride + Channel].A = 0 then
    R := -1
  else
    R := -FCoeff^[Segment * FStride + Channel].B / (3 * FCoeff^[Segment * FStride + Channel].A);
end;

{ TSpline1D }

function TSpline1D.GetSample(Index: Integer): Single;
begin
  Assert(Index < FSampleCount);
  Result := FSamples^[Index];
end;

procedure TSpline1D.SetSample(Index: Integer; AValue: Single);
begin
  Assert(Index < FSampleCount);
  FSamples^[Index] := AValue;
  FCoeffsDirty := true;
end;

function TSpline1D.GetInterpolated(Param: Single): Single;
begin
  Result := inherited GetInterpolated(Param, 0);
end;

function TSpline1D.GetInterpolatedTangent(Param: Single): Single;
begin
  Result := inherited GetInterpolatedTangent(Param, 0);
end;

constructor TSpline1D.Create();
begin
  inherited Create(1);
end;

function TSpline1D.GetEnumerator(): TIterStep1D;
begin
  Result := TIterStep1D.Create(Self, 0.1);
end;

function TSpline1D.WithStep(Step: Single): TIterStep1D;
begin
  Result := TIterStep1D.Create(Self, Step);
end;

{ TSpline2D }

function TSpline2D.GetSample(Index: Integer): TVector2f;
begin
  Assert(Index < FSampleCount);
  Result := PVector2f(@FSamples^[Index * FStride])^;
end;

procedure TSpline2D.SetSample(Index: Integer; AValue: TVector2f);
begin
  Assert(Index < FSampleCount);
  FSamples^[Index * FStride] := AValue.x;
  FSamples^[Index * FStride + 1] := AValue.y;
  FCoeffsDirty := true;
end;

function TSpline2D.GetInterpolated(Param: Single): TVector2f;
begin
  Result.x := inherited GetInterpolated(Param, 0);
  Result.y := inherited GetInterpolated(Param, 1);
end;

function TSpline2D.GetInterpolatedTangent(Param: Single): TVector2f;
begin
  Result.x := inherited GetInterpolatedTangent(Param, 0);
  Result.y := inherited GetInterpolatedTangent(Param, 1);
end;

constructor TSpline2D.Create();
begin
  inherited Create(2);
end;

function TSpline2D.GetEnumerator(): TIterStep2D;
begin
  Result := TIterStep2D.Create(Self, 0.1);
end;

function TSpline2D.WithStep(Step: Single): TIterStep2D;
begin
  Result := TIterStep2D.Create(Self, Step);
end;

function TSpline2D.GetNearestParamApprox(const Point: TVector2f; subdiv: integer): Single;
var
  t, d, Nearest, step, maxT: Single;
  i: Integer;
begin
  Assert(FStride > 1, 'Stride must be > 1 for 2D nearest point search');
  Result := 0;
  Nearest := VectorMagnitudeSq(Vec2f(Point.x - FSamples^[0], Point.y - FSamples^[1]));
  step := 1 / subdiv;
  t := step;
  maxT := FSampleCount - 1;
  i := 0;
  while t < maxT do
  begin
    d := VectorMagnitudeSq(VectorSub(Point, GetInterpolated(t)));
    if d < Nearest then
    begin
      Result := t;
      Nearest := d;
    end;
    t := t + step;
    inc(i);
  end;
//  Debug('Nearest point search steps: %d', [i]);
end;

function checkPointDist(const v: TVector2f; var Nearest: Single): Boolean;
var
  d: Single;
begin
  d := VectorMagnitudeSq(v);
  Result := d < Nearest;
  if Result then
    Nearest := d;
end;

procedure UpdateBB(const V: Single; var Min: Single; var Max: Single);
begin
  Min := MinS(Min, V);
  Max := MaxS(Max, V);
end;

function TSpline2D.GetNearestSegment(const Point: TVector2f): Integer;
var
  i: Integer;
  nearest, d2: Single;
begin
  Result := 0;
  nearest := VectorMagnitudeSq(Vec2f(Point.x - FSamples^[0], Point.y - FSamples^[1]));
  for i := 1 to FSampleCount - 2 do
  begin
    d2 := VectorMagnitudeSq(Vec2f(Point.x - FSamples^[i * FStride], Point.y - FSamples^[i * FStride + 1]));
    if d2 < nearest then
    begin
      nearest := d2;
      Result := i;
    end;
  end;
end;

{ TSplineIterator }

function TSpline2D.GetNearestParamApproxOpt(const Point: TVector2f; subdiv: integer): Single;
var
  t, t2, Nearest: Single;
  step: Single;
  xy, xy2, txy, dxy, dxy2: TVector2f;
  Min, Max: TVector2f;
  Ex: TParamArray;
  ExNum, ExCount: Integer;
  segI, i: Integer;
  iterations: Integer;

  procedure SortEx();
  var
    n1, n2: Integer;
    bv: Single;
    s: string;
  begin
    for n1 := 1 to ExCount-1 do
    begin
      n2 := n1 - 1;
      bv := Ex[n1];
      while (n2 >= 0) do
      begin
        if not (bv < Ex[n2]) then Break;
        Ex[n2 + 1] := Ex[n2];
        Dec(n2);
      end;
      if n1 <> n2 + 1 then
        Ex[n2 + 1] := bv;
    end;
    s := '';
  end;

begin
  Assert(FStride > 1, 'Stride must be > 1 for 2D nearest point search');
  if FCoeffsDirty then
    CalcCoeffs(0, FSampleCount - 2);
  segI := GetNearestSegment(Point);
  Result := segI;
  Nearest := VectorMagnitudeSq(Vec2f(Point.x - FSamples^[segI * FStride], Point.y - FSamples^[segI * FStride + 1]));
  iterations := 0;
  step := 1 / subdiv;
  for i := 0 to FSampleCount - 2 do
  begin
    t := segI;
    Min.X := MinS(FSamples^[segI * FStride], FSamples^[(segI + 1) * FStride]);
    Min.Y := MinS(FSamples^[segI * FStride + 1], FSamples^[(segI + 1) * FStride + 1]);
    Max.X := MaxS(FSamples^[segI * FStride], FSamples^[(segI + 1) * FStride]);
    Max.Y := MaxS(FSamples^[segI * FStride + 1], FSamples^[(segI + 1) * FStride + 1]);
    ExCount := 0;
    findRoots(FCoeff^[segI * FStride], Ex, ExCount);
    if ExCount > 0 then
      UpdateBB(GetInterpolated(segI + Ex[0]).x, Min.X, Max.X);
    if ExCount > 1 then
      UpdateBB(GetInterpolated(segI + Ex[1]).x, Min.X, Max.X);
    ExNum := ExCount;
    findRoots(FCoeff^[segI * FStride + 1], Ex, ExCount);
    if ExCount > ExNum then
      UpdateBB(GetInterpolated(segI + Ex[ExNum]).y, Min.Y, Max.Y);
    if ExCount > ExNum + 1 then
      UpdateBB(GetInterpolated(segI + Ex[ExNum + 1]).y, Min.Y, Max.Y);
    //Debug('= BB #%d: [%3.5F, %3.5F - %3.5F, %3.5F]', [segI, Min.X, Min.Y, Max.X, Max.Y]);

    if Sqr(MaxS(0, BoxDistance(Point, Min, Max))) > Nearest then
    begin
//      Debug('= Skipping segment #%d: [%3.5F, %3.5F - %3.5F, %3.5F] for (%3.5F, %3.5F), dist: %3.5F, min: %3.5F', [segI, Min.X, Min.Y, Max.X, Max.Y, Point.x, Point.y, BoxDistance(Point, Min, Max), Nearest]);
      segI := segI + 1;
      if segI > FSampleCount - 2 then
        segI := 0;
      continue;
    end;

    FindInflection(segI, 0, Ex[ExCount]);
    ExCount := ExCount + Ord((Ex[ExCount] > 0) and (Ex[ExCount] < 1));
    FindInflection(segI, 1, Ex[ExCount]);
    ExCount := ExCount + Ord((Ex[ExCount] > 0) and (Ex[ExCount] < 1));
    SortEx();
    Ex[ExCount] := 1;
    ExNum := 0;
    while ExNum < ExCount + 1 do
    begin
      txy := GetInterpolatedTangent(t);
      xy := GetInterpolated(t);
      VectorSub(dxy, Point, xy);
      // if curve is directed from P at the beginning of a span
      if VectorDot(txy, dxy) < 0 then
      begin
        t2 := segI + Ex[ExNum];
        xy2 := GetInterpolated(t2);
        // if P and span ending point are on the same side of tangent line at span beginning point that span can be skipped (span beginning is checked)
        if VectorDot(txy, dxy) * SignedAreaX2(VectorSub(xy2, xy), txy) <= 0 then
        begin
          if checkPointDist(dxy, Nearest) then
            Result := t;
//          Debug('=== skipping by %3.5F, t: %3.5F, span: [%3.5F..%3.5F]', [(segI + Ex[ExNum]) - t, t, t, segI + Ex[ExNum]]);
          t := t2;
        end;
        inc(iterations);
      end else    // if curve is directed towards P at the beginning of a span
      begin
        t2 := segI + Ex[ExNum];
        xy2 := GetInterpolated(t2);
        VectorSub(dxy2, Point, xy2);
        // if at span ending the curve is still directed towards P
        if VectorDot(GetInterpolatedTangent(t2), dxy2) >= 0 then
            // if P and span ending point are on the same side of tangent line at span beginning point that span can be skipped (span ending is checked)
          if VectorDot(txy, dxy) * SignedAreaX2(VectorSub(xy2, xy), txy) <= 0 then
          begin
//            Debug('==- skipping by %3.5F, t: %3.5F, span: [%3.5F..%3.5F]', [t2 - t, t, t, t2]);
            if checkPointDist(dxy2, Nearest) then
              Result := t2;
            t := t2;
          end;
        inc(iterations);
      end;

      t := t + step;
      xy := GetInterpolated(t);
      while t < segI + Ex[ExNum] do
      begin
        VectorSub(dxy, Point, xy);
        if checkPointDist(dxy, Nearest) then
          Result := t;
        t := t + step;
        xy := GetInterpolated(t);
        inc(iterations);
      end;
      t := segI + Ex[ExNum];

      Inc(ExNum);
    end;
    segI := segI + 1;
    if segI > FSampleCount - 2 then
      segI := 0;
  end;
//  Debug('Nearest point search steps: %d', [iterations]);
end;

function TSpline2D.GetAABB(): TAABB;
var
  Ex: TParamArray;
  segI: Integer;
  ExNum, ExCount: Integer;
begin
  if FCoeffsDirty then
    CalcCoeffs(0, FSampleCount - 2);
  Result.P1.X := MinS(FSamples^[0], FSamples^[FStride]);
  Result.P1.Y := MinS(FSamples^[1], FSamples^[FStride + 1]);
  Result.P2.X := MaxS(FSamples^[0], FSamples^[FStride]);
  Result.P2.Y := MaxS(FSamples^[1], FSamples^[FStride + 1]);
  ExCount := 0;
  findRoots(FCoeff^[0], Ex, ExCount);
  if ExCount > 0 then
    UpdateBB(GetInterpolated(Ex[0]).x, Result.P1.X, Result.P2.X);
  if ExCount > 1 then
    UpdateBB(GetInterpolated(Ex[1]).x, Result.P1.X, Result.P2.X);
  ExNum := ExCount;
  findRoots(FCoeff^[1], Ex, ExCount);
  if ExCount > ExNum then
    UpdateBB(GetInterpolated(Ex[ExNum]).y, Result.P1.Y, Result.P2.Y);
  if ExCount > ExNum + 1 then
    UpdateBB(GetInterpolated(Ex[ExNum + 1]).y, Result.P1.Y, Result.P2.Y);
  for segI := 1 to FSampleCount - 2 do
  begin
    UpdateBB(FSamples^[segI * FStride], Result.P1.x, Result.P2.x);
    UpdateBB(FSamples^[segI * FStride + 1], Result.P1.y, Result.P2.y);
    UpdateBB(FSamples^[(segI + 1) * FStride], Result.P1.x, Result.P2.x);
    UpdateBB(FSamples^[(segI + 1) * FStride + 1], Result.P1.y, Result.P2.y);
    ExCount := 0;
    findRoots(FCoeff^[segI * FStride], Ex, ExCount);
    if ExCount > 0 then
      UpdateBB(GetInterpolated(segI + Ex[0]).x, Result.P1.X, Result.P2.X);
    if ExCount > 1 then
      UpdateBB(GetInterpolated(segI + Ex[1]).x, Result.P1.X, Result.P2.X);
    ExNum := ExCount;
    findRoots(FCoeff^[segI * FStride + 1], Ex, ExCount);
    if ExCount > ExNum then
      UpdateBB(GetInterpolated(segI + Ex[ExNum]).y, Result.P1.Y, Result.P2.Y);
    if ExCount > ExNum + 1 then
      UpdateBB(GetInterpolated(segI + Ex[ExNum + 1]).y, Result.P1.Y, Result.P2.Y);
  end;
end;

{ TIterStep1D }

constructor TIterStep1D.Create(Spline: TSpline1D; Step: Single);
begin
  FSpline := Spline;
  FStep := Step;
  maxT := Spline.SampleCount - 1 + FStep * 0.01;
  t := -FStep;
end;

function TIterStep1D.GetEnumerator(): TIterStep1D;
begin
  Result := Self;
end;

function TIterStep1D.MoveNext(): Boolean;
begin
  t := t + FStep;
  Result := t < maxT;
  if not Result then
    t := maxT - FStep * 0.01;
end;

function TIterStep1D.GetCurrent(): TVector2f;
begin
  Result := Vec2f(t, TSpline1D(FSpline)[t]);
end;

{ TIterStep2D }

constructor TIterStep2D.Create(Spline: TSpline2D; Step: Single);
begin
  FSpline := Spline;
  FStep := Step;
  maxT := Spline.SampleCount - 1;
  t := 0;
  FLast := Spline.Sample[0];
  FEndMarker := Ord(Spline.SampleCount < 2);
end;

function TIterStep2D.GetEnumerator(): TIterStep2D;
begin
  Result := Self;
end;

function TIterStep2D.MoveNext(): Boolean;
var
  dir: TVector2f;
  d: Single;
begin
  Result := FEndMarker <= 1;
  FLast := TSpline2D(FSpline)[t];
  dir := TSpline2D(FSpline).Tangent[t];
  d := sqrt(sqr(dir.x) + sqr(dir.y));
  t := t + fstep / d;
  FEndMarker := FEndMarker + Ord(t >= maxT);
  if FEndMarker > 0 then
    t := maxT;
end;

function TIterStep2D.GetCurrent(): TVector2f;
begin
  Result := FLast;
end;

end.
