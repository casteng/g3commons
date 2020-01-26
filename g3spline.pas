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

  TSplineKoeff = record
    A, B, C, D: Single;
  end;

  TSplineKoeffs = array[0..MaxInt div SizeOf(TSplineKoeff) - 1] of TSplineKoeff;
  PSplineKoeffs = ^TSplineKoeffs;

  TSpline = class(TObject)
  private
    FStride: Integer;
    Koeff: PSplineKoeffs;
    FSamples: PSingleBuffer;
    FSampleCount: Integer;
    procedure SetSampleCount(Count: Integer);
    procedure CalcKoeffs(StartIndex, EndIndex: Integer);
    function GetInterpolated(Param: Single; Channel: Integer): Single;
    function GetInterpolatedTangent(Param: Single; Channel: Integer): Single;
    function GetSegmentCount(): Integer;
  public
    constructor Create(Stride: Integer);
    destructor Destroy(); override;
    procedure SetSamples(Count: Integer; ASamples: PSingleBuffer);

    procedure findRoot1(Segment, Channel: Integer; out R1, R2: Single);
    procedure findRoot2(Segment, Channel: Integer; out R: Single);

    property Samples: PSingleBuffer read FSamples;
    property SampleCount: Integer read FSampleCount write SetSampleCount;
    property SegmentCount: Integer read GetSegmentCount;
  end;

  TSpline1D = class(TSpline)
  private
    function GetSample(Index: Integer): Single;
    procedure SetSample(Index: Integer; AValue: Single);
    function GetInterpolated(Param: Single): Single;
    function GetInterpolatedTangent(Param: Single): Single;
  public
    constructor Create();
    property Sample[Index: Integer]: Single read GetSample write SetSample;
    property Value[T: Single]: Single read GetInterpolated; default;
    property Tangent[T: Single]: Single read GetInterpolatedTangent;
  end;

  TSpline2D = class(TSpline)
  private
    function GetSample(Index: Integer): TVector2f;
    procedure SetSample(Index: Integer; AValue: TVector2f);
    function GetInterpolated(Param: Single): TVector2f;
    function GetInterpolatedTangent(Param: Single): TVector2f;
  public
    constructor Create();
    function GetNearestParamApprox(const Point: TVector2f; subdiv: integer): Single;
    function GetNearestSegment(const Point: TVector2f): Integer;
    function GetNearestParamApproxOpt(const Point: TVector2f; subdiv: integer): Single;
    property Sample[Index: Integer]: TVector2f read GetSample write SetSample;
    property Value[T: Single]: TVector2f read GetInterpolated; default;
    property Tangent[T: Single]: TVector2f read GetInterpolatedTangent;
  end;

  procedure CalcCatmullRom1D(PointsCount, Resolution: Integer; ControlPoints, Curve: PSingleBuffer; CurveStride: Integer = 1);
  procedure CalcCatmullRom2D(PointsCount, Resolution: Integer; ControlPoints, Curve: P2DPointArray);
  procedure CalcCatmullRomND(PointsCount, Resolution, N: Integer; ControlPoints, FinalCurve: Pointer; ControlStride, CurveStride: Integer);

implementation

uses
  g3common,
  g3vector,
  g3geom,
  g3log,
  sysutils;

type
  TParamArray = array[0..6] of Single;

procedure CalcCatmullRom1D(PointsCount, Resolution: Integer; ControlPoints, Curve: PSingleBuffer; CurveStride: Integer = 1);
var
  i, j, CI: Integer;
  Ap, Bp, Cp, Dp: Single;
  OverR, T: Single;
begin
  CI := 0;
  OverR := 1 / Resolution;
  ControlPoints^[0] := ControlPoints^[1];
  ControlPoints^[PointsCount] := ControlPoints^[PointsCount - 1];
  for i := 1 to PointsCount - 1 do
  begin
//Coeffs
    Ap :=  -ControlPoints^[i-1] + 3*ControlPoints^[i] - 3*ControlPoints^[i+1] + ControlPoints^[i+2];
    Bp := 2*ControlPoints^[i-1] - 5*ControlPoints^[i] + 4*ControlPoints^[i+1] - ControlPoints^[i+2];
    Cp :=  -ControlPoints^[i-1] + ControlPoints^[i+1];
    Dp := 2*ControlPoints^[i];
//Calc
    Curve^[CI] := ControlPoints^[i];
    Inc(CI, CurveStride);
    T := OverR;
    for j := 1 to Resolution - 1 do
    begin
      Curve^[CI] := ((Ap * T * T * T) + (Bp * T * T) + (Cp * T) + Dp) * 0.5;  { Calc x value }
      T := T + OverR;
      Inc(CI, CurveStride);
    end;
  end;
//  Curve^[CI] := CtrlPt^[PointsCount];
end;

procedure CalcCatmullRom2D(PointsCount, Resolution: Integer; ControlPoints, Curve: P2DPointArray);
var
  i0, i1, i2, i3, j, CI: Integer;
  Ap, Bp, Cp, Dp: record X, Y: Single end;
  OverR, T, T2, T3: Single;
begin
  Assert(PointsCount > 1);
  Assert(Resolution > 0);
  Assert(Assigned(ControlPoints) and Assigned(Curve));
  CI := 0;
  //ControlPoints^[0] := ControlPoints^[1];
  //ControlPoints^[PointsCount + 1] := ControlPoints^[PointsCount];
  OverR := 1 / Resolution;
  for i1 := 0 to PointsCount - 2 do
  begin
    // TODO: optimize
    i0 := MaxI(0, i1 - 1);
    i2 := MinI(PointsCount - 1, i1 + 1);
    i3 := MinI(PointsCount - 1, i1 + 2);
    Ap.X :=  -ControlPoints^[i0].X + 3*ControlPoints^[i1].X - 3*ControlPoints^[i2].X + ControlPoints^[i3].X;
    Bp.X := 2*ControlPoints^[i0].X - 5*ControlPoints^[i1].X + 4*ControlPoints^[i2].X - ControlPoints^[i3].X;
    Cp.X :=  -ControlPoints^[i0].X + ControlPoints^[i2].X;
    Dp.X := 2*ControlPoints^[i1].X;
    Ap.Y :=  -ControlPoints^[i0].Y + 3*ControlPoints^[i1].Y - 3*ControlPoints^[i2].Y + ControlPoints^[i3].Y;
    Bp.Y := 2*ControlPoints^[i0].Y - 5*ControlPoints^[i1].Y + 4*ControlPoints^[i2].Y - ControlPoints^[i3].Y;
    Cp.Y :=  -ControlPoints^[i0].Y + ControlPoints^[i2].Y;
    Dp.Y := 2*ControlPoints^[i1].Y;
//Calc
    Curve^[CI].X := Dp.X * 0.5;  { Calc x value }
    Curve^[CI].Y := Dp.Y * 0.5;  { Calc y value }
    Inc(CI);
    T := OverR;
    for j := 1 to Resolution - 1 do
    begin
      T2 := T * T;
      T3 := T2 * T;
      Curve^[CI].X := ((Ap.X * T3) + (Bp.X * T2) + (Cp.X * T) + Dp.X) * 0.5;  { Calc x value }
      Curve^[CI].Y := ((Ap.Y * T3) + (Bp.Y * T2) + (Cp.Y * T) + Dp.Y) * 0.5;  { Calc y value }
      T := T + OverR;
      Inc(CI);
    end;
  end;
  Curve^[CI] := ControlPoints^[PointsCount - 1];
end;

procedure CalcCatmullRomND(PointsCount, Resolution, N: Integer; ControlPoints, FinalCurve: Pointer; ControlStride, CurveStride: Integer);
var
  i, j, k, CI: Integer;
  Ap, Bp, Cp, Dp: array of Single;
  OverR, T, T2, T3: Single;
  CtrlPt, Curve: PSingleBuffer;
begin
  CtrlPt := ControlPoints;
  Curve := FinalCurve;
  CI := 0;
  for j := 0 to N - 1 do begin
    CtrlPt^[0 + j] := CtrlPt^[1 * ControlStride + j];
    CtrlPt^[(PointsCount + 1) * ControlStride + j] := CtrlPt^[PointsCount * ControlStride + j];
  end;
  OverR := 1 / Resolution;
  SetLength(Ap, N);
  SetLength(Bp, N);
  SetLength(Cp, N);
  SetLength(Dp, N);

  for i := 1 to PointsCount - 1 do begin
//Coeffs
    for j := 0 to N - 1 do begin
      Ap[j] :=    -CtrlPt^[(i - 1) * ControlStride + j] + 3 * CtrlPt^[i * ControlStride + j] - 3 * CtrlPt^[(i + 1) * ControlStride + j] + CtrlPt^[(i + 2) * ControlStride + j];
      Bp[j] := 2 * CtrlPt^[(i - 1) * ControlStride + j] - 5 * CtrlPt^[i * ControlStride + j] + 4 * CtrlPt^[(i + 1) * ControlStride + j] - CtrlPt^[(i + 2) * ControlStride + j];
      Cp[j] :=    -CtrlPt^[(i - 1) * ControlStride + j] + CtrlPt^[(i + 1) * ControlStride + j];
      Dp[j] := 2 * CtrlPt^[i * ControlStride + j];
      Curve^[CI * CurveStride + j] := Dp[j] * 0.5;  { Calc x value }
    end;
//Calc
    Inc(CI);
    T := OverR;
    for k := 1 to Resolution - 1 do
    begin
      T2 := T * T;
      T3 := T2 * T;
      for j := 0 to N - 1 do
        Curve^[CI * CurveStride + j] := ((Ap[j] * T3) + (Bp[j] * T2) + (Cp[j] * T) + Dp[j]) * 0.5;
      { Calc x value }
      T := T + OverR;
      Inc(CI);
    end;
  end;
  for j := 0 to N - 1 do
    Curve^[CI * CurveStride + j] := CtrlPt^[PointsCount * ControlStride + j];
end;

{ TSpline }

function TSpline.GetInterpolated(Param: Single; Channel: Integer): Single;
var
  Segment: Integer;
  T, T2: Single;
begin
  Assert(Param <= FSampleCount);
  Assert(Channel < FStride);
  Segment := MinI(FSampleCount-2, trunc(Param));
  T := Param - Segment;
  Segment := Segment*FStride + Channel;
  T2 := Sqr(T);
  Result := Koeff[Segment].A * T2 * T + Koeff[Segment].B * T2 + Koeff[Segment].C * T + Koeff[Segment].D;
end;

function TSpline.GetInterpolatedTangent(Param: Single; Channel: Integer): Single;
var
  Segment: Integer;
  T: Single;
begin
  Assert(Param <= FSampleCount);
  Assert(Channel < FStride);
  Segment := MinI(FSampleCount-2, trunc(Param));
  T := Param - Segment;
  Segment := Segment*FStride + Channel;
  Result := 3*Koeff[Segment].A * T * T + 2*Koeff[Segment].B * T + Koeff[Segment].C;
end;

procedure TSpline.SetSampleCount(Count: Integer);
begin
  if FSampleCount = Count then Exit;
  FSampleCount := Count;
  ReallocMem(FSamples, FSampleCount * SizeOf(Single) * FStride);
  ReallocMem(Koeff, (FSampleCount-1) * SizeOf(TSplineKoeff) * FStride);
end;

procedure TSpline.CalcKoeffs(StartIndex, EndIndex: Integer);
var
  i0, i1, i2, i3, j: Integer;
begin
  Assert((StartIndex >= 0) and (StartIndex < FSampleCount - 1), 'Invalid start index');
  Assert((EndIndex >= 0) and (EndIndex < FSampleCount - 1) and (StartIndex <= EndIndex), 'Invalid end index');
  if FSampleCount < 2 then Exit;
  for i1 := StartIndex to EndIndex do
  begin
    for j := 0 to FStride-1 do
    begin
      i0 := MaxI(0, i1 - 1)*FStride+j;
      i2 := MinI(FSampleCount - 1, i1 + 1)*FStride+j;
      i3 := MinI(FSampleCount - 1, i1 + 2)*FStride+j;
      Koeff^[i1*FStride+j].A := 0.5 * (-FSamples^[i0] + 3 * FSamples^[i1 * FStride + j] - 3 * FSamples^[i2] + FSamples^[i3]);
      Koeff^[i1*FStride+j].B := FSamples^[i0] - 2.5 * FSamples^[i1*FStride+j] + 2 * FSamples^[i2] - 0.5 * FSamples^[i3];
      Koeff^[i1*FStride+j].C := 0.5 * (-FSamples^[i0] + FSamples^[i2]);
      Koeff^[i1*FStride+j].D := FSamples^[i1*FStride+j];
    end;
  end;
end;

function TSpline.GetSegmentCount(): Integer;
begin
  Result := SampleCount-1;
end;

constructor TSpline.Create(Stride: Integer);
begin
  FStride := Stride;
end;

destructor TSpline.Destroy();
begin
  if Assigned(FSamples) then
    FreeMem(FSamples);
  if Assigned(Koeff) then
    FreeMem(Koeff);
  inherited;
end;

procedure TSpline.SetSamples(Count: Integer; ASamples: PSingleBuffer);
begin
  if FSampleCount = Count then
    exit;
  FSampleCount := Count;
  FSamples := ASamples;
  if Assigned(Koeff) then
    FreeMem(Koeff);
  if FSampleCount > 0 then
    GetMem(Koeff, (FSampleCount - 1) * SizeOf(TSplineKoeff) * FStride);
  CalcKoeffs(0, FSampleCount - 2);
end;

procedure findRoots(const K: TSplineKoeff; var Result: TParamArray; var Count: Integer);
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
  D := B*B - 4 * A * C;
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

procedure TSpline.findRoot1(Segment, Channel: Integer; out R1, R2: Single);
var
  Result: TParamArray;
  Count: Integer;
begin
  Count := 0;
  findRoots(Koeff^[Segment*FStride + Channel], Result, Count);
  R1 := Ord(Count > 0) * Result[0] - Ord(Count <= 0);
  R2 := Ord(Count > 1) * Result[1] - Ord(Count <= 1);
end;

procedure TSpline.findRoot2(Segment, Channel: Integer; out R: Single);
begin
  if Koeff^[Segment * FStride + Channel].A = 0 then
    R := -1
  else
    R := -Koeff^[Segment * FStride + Channel].B / (3 * Koeff^[Segment * FStride + Channel].A);
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
  CalcKoeffs(0, FSampleCount - 2);
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

{ TSpline2D }

function TSpline2D.GetSample(Index: Integer): TVector2f;
begin
  Assert(Index < FSampleCount);
  Result := PVector2f(@FSamples^[Index*FStride])^;
end;

procedure TSpline2D.SetSample(Index: Integer; AValue: TVector2f);
begin
  Assert(Index < FSampleCount);
  FSamples^[Index*FStride] := AValue.x;
  FSamples^[Index*FStride+1] := AValue.y;
  CalcKoeffs(0, FSampleCount - 2);
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
  maxT := FSampleCount- 1;
  i := 0;
  while t < maxT do
  begin
    d := VectorMagnitudeSq(VectorSub(Point, GetInterpolated(t)));
    if d < Nearest then
    begin
      Result := t;
      Nearest := d;
    end;
    t := t+ step;
    inc(i);
  end;
  Debug('Nearest point search steps: %d', [i]);
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
  for i := 1 to FSampleCount-2 do
  begin
    d2 := VectorMagnitudeSq(Vec2f(Point.x - FSamples^[i*FStride], Point.y - FSamples^[i*FStride+1]));
    if d2 < nearest then
    begin
      nearest := d2;
      Result := i;
    end;
  end;
  Debug('= Nearest #%d', [Result]);
end;

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
    for n1 := 0 to ExCount-1 do
    begin
      Assert((n1 = 0) or (Ex[n1] >= Ex[n1 - 1]), 'Sort fail');
      s := Format('%S__%3.5F', [s, Ex[n1]]);
    end;
    Debug('=== Spans: 0__%s__1, count: %d', [s, ExCount+1]);
  end;

begin
  Assert(FStride > 1, 'Stride must be > 1 for 2D nearest point search');
  segI := GetNearestSegment(Point);
  Result := segI;
  Nearest := VectorMagnitudeSq(Vec2f(Point.x - FSamples^[segI*FStride], Point.y - FSamples^[segI*FStride+1]));
  iterations := 0;
  step := 1 / subdiv;
  for i := 0 to FSampleCount-2 do
  begin
    t := segI;
    Min.X := MinS(FSamples^[segI*FStride],   FSamples^[(segI+1)*FStride]);
    Min.Y := MinS(FSamples^[segI*FStride+1], FSamples^[(segI+1)*FStride+1]);
    Max.X := MaxS(FSamples^[segI*FStride],   FSamples^[(segI+1)*FStride]);
    Max.Y := MaxS(FSamples^[segI*FStride+1], FSamples^[(segI+1)*FStride+1]);
    ExCount := 0;
    findRoots(Koeff^[segI*FStride], Ex, ExCount);
    if ExCount > 0 then
      UpdateBB(GetInterpolated(segI + Ex[0]).x, Min.X, Max.X);
    if ExCount > 1 then
      UpdateBB(GetInterpolated(segI + Ex[1]).x, Min.X, Max.X);
    ExNum := ExCount;
    findRoots(Koeff^[segI*FStride+1], Ex, ExCount);
    if ExCount > ExNum then
      UpdateBB(GetInterpolated(segI + Ex[ExNum]).y, Min.Y, Max.Y);
    if ExCount > ExNum + 1 then
      UpdateBB(GetInterpolated(segI + Ex[ExNum+1]).y, Min.Y, Max.Y);
    //Debug('= BB #%d: [%3.5F, %3.5F - %3.5F, %3.5F]', [segI, Min.X, Min.Y, Max.X, Max.Y]);

    if Sqr(MaxS(0, BoxDistance(Point, Min, Max))) > Nearest then
    begin
      Debug('= Skipping segment #%d: [%3.5F, %3.5F - %3.5F, %3.5F] for (%3.5F, %3.5F), dist: %3.5F, min: %3.5F', [segI, Min.X, Min.Y, Max.X, Max.Y, Point.x, Point.y, BoxDistance(Point, Min, Max), Nearest]);
      segI := segI + 1;
      if segI > FSampleCount-2 then
        segI := 0;
      continue;
    end;

    findRoot2(segI, 0, Ex[ExCount]);
    ExCount := ExCount + Ord((Ex[ExCount] > 0) and (Ex[ExCount] < 1));
    findRoot2(segI, 1, Ex[ExCount]);
    ExCount := ExCount + Ord((Ex[ExCount] > 0) and (Ex[ExCount] < 1));
    SortEx();
    Ex[ExCount] := 1;
    ExNum := 0;
    while ExNum < ExCount+1 do
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
          Debug('=== skipping by %3.5F, t: %3.5F, span: [%3.5F..%3.5F]', [(segI + Ex[ExNum]) - t, t, t, segI + Ex[ExNum]]);
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
            Debug('==- skipping by %3.5F, t: %3.5F, span: [%3.5F..%3.5F]', [t2 - t, t, t, t2]);
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
    if segI > FSampleCount-2 then
      segI := 0;
  end;
  Debug('Nearest point search steps: %d', [iterations]);
end;

end.
