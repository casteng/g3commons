{
@abstract(Common routines unit)

The unit contains common routines

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3common;
{$I g3config.inc}

interface

uses
  {!}g3types;

  // Returns max of the two arguments
  function MaxI(V1, V2: Integer): Integer; {$I inline.inc}
  // Returns min of the two arguments
  function MinI(V1, V2: Integer): Integer; {$I inline.inc}
  // Clamp the value to range
  function ClampI(V, Min, Max: Integer): Integer; //{$I inline.inc}
  // Returns max of the two arguments. Doesn't take in account NaNs etc.
  function MaxS(V1, V2: Single): Single; {$I inline.inc}
  // Returns min of the two arguments. Doesn't take in account NaNs etc.
  function MinS(V1, V2: Single): Single; {$I inline.inc}
  // Clamps value to the specified bounds. Doesn't take in account NaNs etc.
  function ClampS(V, Min, Max: Single): Single; {$I inline.inc}
  // Returns sign of the value
  function Sign(Value: Single): Single; overload; {$I inline.inc}
  // Returns sign of the value
  function Sign(Value: Integer): Integer; overload; {$I inline.inc}
  // Swaps two integer values
  procedure SwapI(var a, b: Integer); {$I inline.inc}
  // Returns true if the value is a power of two
  function IsPowerOf2(const value: Integer): Boolean; {$I inline.inc}
  // Returns power of 2 value which is greater or equal to x
  function NextPowerOf2(const x: Integer): Integer; {$I inline.inc}
  { Returns 1/Sqrt(x). May be fast if assembler optimizations are on.
    If FLOAT_IEEE is defined it's also fast, but returned value may differ from expected value by at most 0.175%
    and for 0 value 19817753709685768200 is returned. }
  function InvSqrt(x: Single): Single; {$I inline.inc}
  // Returns the smallest value that is greater than or equal to X and is integer
  function Ceil(const X: Single): Integer; {$I inline.inc}
  // Returns the largest value that is less than or equal to X and is integer
  function Floor(const X: Single): Integer; {$I inline.inc}
  {$IFDEF FLOAT_IEEE}
  // Returns True if v1 equals to v2 with relative accuracy specified in Units in the Last Place by MAX_ULPS
  function FloatEquals(const v1: Double; const v2: Double): Boolean; overload; {$I inline.inc}
  // Returns True if v1 equals to v2 with relative accuracy specified in Units in the Last Place by MAX_ULPS
  function FloatEquals(v1: Single; v2: Single): Boolean; overload; {$I inline.inc}
  {$ENDIF}

  // Returns base pointer shifted by offset
  function PtrOffs(Base: Pointer; Offset: PtrInt): Pointer; {$I inline.inc}
  // Returns pointer as a number
  function PtrToInt(P: Pointer): PtrUInt; {$I inline.inc}

  // Returns positions of ch within the given string starting from Start or -1 if not found
  function CharPos(const ch: AnsiChar; const s: AnsiString; const Start: Integer ): Integer;
  // Returns extension part of file name
  function GetFileExt(const FileName: string): string;

  // Converts file path relative to given file path to absolute path
  function GetPathRelativeToFile(const FileName, RelativeFileName: string): string;

type
  { @Abstract(Pseudo-random numbers generator)
    Generates a sequence of pseudo-random numbers.
    }
  TRandomGenerator = class
  protected
    // Seeds for sequences
    RandomSeed: array of Longword;
    // Chain values for sequences
    RandomChain: array of Longword;
    // Current sequence
    FCurrentSequence: Cardinal;
    // Number of sequences
    procedure SetMaxSequence(AMaxSequence: Integer);
    procedure SetCurrentSequence(const Value: Cardinal);
  public
    constructor Create;
    // Initializes the current sequence with the specified chain value and the specified seed
    procedure InitSequence(Chain, Seed: Longword);
    // Generate a raw random number. Fastest method.
    function GenerateRaw: Longword; virtual;
    // Generate a floating point random number within the given range
    function Rnd(Range: Single): Single;
    // Generate a floating point random number within the range [-<b>Range..Range</b>]
    function RndSymm(Range: Single): Single;
    // Generate an integer random number within the range [0..<b>Range</b>-1]
    function RndI(Range: Integer): Integer;
    // Current sequence
    property CurrentSequence: Cardinal read FCurrentSequence write SetCurrentSequence;
  end;

  { @Abstract(Reference-counted container of temporary objects and memory buffers )
    Create an instance with @Link(CreateRefcountedContainer). The container can be used to accumulate temporary objects and buffers.
    When no more references points to the container it destroys itself and all accumulated objects and buffers.
    Usage:
    with CreateRefcountedContainer do begin
      obj := TSomeObject.Create();
      Managed.AddObject(obj);
    end;
    The container and all added objects will be destroyed after the current routine execution (but not after "with" statement end). }
  IRefcountedContainer = interface
    // Adds an object instance
    function AddObject(Obj: TObject): TObject;
    // Adds a memory buffer
    function AddPointer(Ptr: Pointer): Pointer;
    // Adds an array of object instances
    procedure AddObjects(Objs: array of TObject);
    // Adds an array of memory buffers
    procedure AddPointers(Ptrs: array of Pointer);
    // Returns self for use within "with" statement
    function GetContainer(): IRefcountedContainer;
    // Returns self for use within "with" statement
    property Managed: IRefcountedContainer read GetContainer;
  end;

  // Back buffer data structure
  TBackBufferData = packed record
    OrigData: Pointer;
    Size: Cardinal;
    Data: Pointer;
  end;
  TBackBufferArray = packed array[0..$FFFFFF] of TBackBufferData;

  { @Abstract(Deferred write helper for user entity classes)
    May be used to support deferred write of properties in non performace critical classes.
    Use WriteXXX methods in a property setter to remember a write operation.
    Use Flush() method to apply remembered write operations.
    Not thread safe. There is on much sense to make this class thread safe as properties will be written from one thread.
  }
  TBackBuffer = class
  private
    FBuffer: Pointer;
    {$IFOPT C+}FSize: Integer;{$ENDIF}
    FData: ^TBackBufferArray;
    FCount: Integer;
    function GetIndex(Dest: Pointer; Size: Integer): Integer;
  public
    // Initialize internall storage for Count properties with Size total size
    constructor Create(Count: Integer; Size: Integer);
    destructor Destroy(); override;
    // Writes Value into internal storage as a value of a property which is located at Dest and has the specified size
    procedure WriteProperty(Dest: Pointer; Size: Integer; const Value);
    // Writes all values from internal storage into properties
    procedure Flush();
  end;

  // Create an instance of reference counted container
  function CreateRefcountedContainer: IRefcountedContainer;

implementation

uses SysUtils;

function MaxI(V1, V2: Integer): Integer; {$I inline.inc}
begin
  Result := V1 * Ord(V1 >= V2) + V2 * Ord(V1 < V2);
end;

function MinI(V1, V2: Integer): Integer; {$I inline.inc}
begin
  Result := V1 * Ord(V1 <= V2) + V2 * Ord(V1 > V2);
end;

function ClampI(V, Min, Max: Integer): Integer; //{$I inline.inc}
begin
  Result := V + Ord(V < Min) * (Min - V) - Ord(V > Max) * (V - Max);
end;

function MaxS(V1, V2: Single): Single;
begin
  if V1 > V2 then Result := V1 else Result := V2;
end;

function MinS(V1, V2: Single): Single;
begin
  if V1 < V2 then Result := V1 else Result := V2;
end;

function ClampS(V, Min, Max: Single): Single; {$I inline.inc}
begin
  Result := MinS(MaxS(V, Min), Max);
end;

function Sign(Value: Single): Single; {$I inline.inc}
begin
  Result := Ord(Value > 0) - Ord(Value < 0);
end;

function Sign(Value: Integer): Integer; {$I inline.inc}
begin
  Result := Ord(Value > 0) - Ord(Value < 0);
end;

procedure SwapI(var a, b: Integer); {$I inline.inc}
begin
  a := a xor b;
  b := b xor a;
  a := a xor b;
end;

function IsPowerOf2(const value: Integer): Boolean; {$I inline.inc}
begin
  Result := value and (value-1) = 0;
end;

function NextPowerOf2(const x: Integer): Integer; {$I inline.inc}
begin
  Result := x-1;
  Result := Result or Result shr 1;
  Result := Result or Result shr 2;
  Result := Result or Result shr 4;
  Result := Result or Result shr 8;
  Result := Result or Result shr 16;
  {$IF SizeOf(Integer) > 4}
  Result := Result or Result shr 32;
  {$IFEND}
  Inc(Result);
end;

function InvSqrt(x: Single): Single; {$I inline.inc}
{$IFDEF FLOAT_IEEE}
const
  THREE_HALFS = 1.5;
var
  yi: Integer absolute Result;
begin
  Result := x;
  yi := $5f3759df - ( yi shr 1 );
  Result := Result * (THREE_HALFS - (x * 0.5 * Result * Result));
{$ELSE}
begin
  Result := 1 / Sqrt(x);
{$ENDIF}
end;

function Ceil(const X: Single): Integer; {$I inline.inc}
begin
  Result := Integer(Trunc(X));
  Result := Result + Ord(Frac(X) > 0);
end;

function Floor(const X: Single): Integer; {$I inline.inc}
begin
  Result := Integer(Trunc(X));
  Result := Result - Ord(Frac(X) < 0);
end;

{$IFDEF FLOAT_IEEE}
function FloatEquals(const v1: Double; const v2: Double): Boolean; overload; {$I inline.inc}
var
  d1: Int64 absolute v1;
  d2: Int64 absolute v2;
begin
  if (d1 and SIGN_BIT_DOUBLE) <> (d2 and SIGN_BIT_DOUBLE) then
    Result := v1 = v2
  else
    Result := Abs(d1 - d2) <= MAX_ULPS;
end;

function FloatEquals(v1: Single; v2: Single): Boolean; overload; {$I inline.inc}
begin
  if (Integer((@v1)^) and SIGN_BIT_SINGLE) <> (Integer((@v2)^) and SIGN_BIT_SINGLE) then
    Result := v1 = v2
  else
    Result := Abs(Integer((@v1)^) - Integer((@v2)^)) <= MAX_ULPS;
end;
{$ENDIF}

function PtrOffs(Base: Pointer; Offset: PtrInt): Pointer; {$I inline.inc}
begin
  Result := Base;
  Inc(PByte(Result), Offset);
end;

function PtrToInt(P: Pointer): PtrUInt; {$I inline.inc}
begin
  Result := PtrUInt(P);
end;

function CharPos(const ch: AnsiChar; const s: AnsiString; const Start: Integer): Integer;
begin       // TODO: optimize
  Result := Pos(ch, Copy(s, Start, Length(s)));
  if Result >= STRING_INDEX_BASE then
    Result := Result + Start
  else
    Result := -1;
end;

function GetFileExt(const FileName: string): string;
var i, ind: Integer;
begin
  ind := -1;
  for i := 1 to Length(FileName) do
  begin
    if FileName[i] = '.' then
      ind := i
    else if FileName[i] = '\' then
      ind := -1;
  end;

  if ind = -1 then
    Result := ''
  else
    Result := Copy(FileName, ind+1, Length(FileName));
end;

function GetPathRelativeToFile(const FileName, RelativeFileName: string): string;
begin
  Result := ExtractFilePath(FileName) + RelativeFileName;
end;

const
  // Minimum capacity of reference counted container
  MinRefCContainerLength = 8;

type
  TRefcountedContainer = class(TLiteInterfacedObject, IRefcountedContainer)
  private
    ObjList: array of TObject;
    PtrList: array of Pointer;
    ObjCount, PtrCount: Integer;
  public
    destructor Destroy; override;

    function AddObject(Obj: TObject): TObject;
    function AddPointer(Ptr: Pointer): Pointer;
    procedure AddObjects(Objs: array of TObject);
    procedure AddPointers(Ptrs: array of Pointer);
    function GetContainer(): IRefcountedContainer;
  end;

{ TRandomGenerator }

constructor TRandomGenerator.Create;
begin
  SetMaxSequence(8);
  CurrentSequence := 0;
  InitSequence(1, 1);
end;

procedure TRandomGenerator.InitSequence(Chain, Seed: Longword);
begin
  RandomChain[FCurrentSequence] := Chain;
  RandomSeed [FCurrentSequence] := Seed;
end;

function TRandomGenerator.GenerateRaw: Longword;
begin
{$Q-}
  RandomSeed[FCurrentSequence] := 97781173 * RandomSeed[FCurrentSequence] + RandomChain[FCurrentSequence];
  Result := RandomSeed[FCurrentSequence];
end;

function TRandomGenerator.Rnd(Range: Single): Single;
const RandomNorm = 1/$FFFFFFFF;
begin
  Result := GenerateRaw * RandomNorm * Range;
end;

function TRandomGenerator.RndSymm(Range: Single): Single;
begin
  Result := Rnd(2*Range) - Range;
end;

function TRandomGenerator.RndI(Range: Integer): Integer;
begin
  Result := Round(Rnd(MaxI(0, Range-1)));
end;

procedure TRandomGenerator.SetMaxSequence(AMaxSequence: Integer);
begin
  SetLength(RandomSeed, AMaxSequence);
  SetLength(RandomChain, AMaxSequence);
end;

procedure TRandomGenerator.SetCurrentSequence(const Value: Cardinal);
begin
  FCurrentSequence := Value;
  if Integer(Value) > High(RandomSeed) then
  begin
    SetMaxSequence(Value+1);
  end;
end;

{ TRefcountedContainer }

destructor TRefcountedContainer.Destroy;
var i: Integer;
begin
  for i := ObjCount-1 downto 0 do if Assigned(ObjList[i]) then
  begin
    try
      FreeAndNil(ObjList[i]);
    except
      // TODO: log
    end;
  end;
  for i := PtrCount-1 downto 0 do if Assigned(PtrList[i]) then
  begin
    FreeMem(PtrList[i]);
  end;
  ObjList := nil;
  PtrList := nil;
  inherited;
end;

function TRefcountedContainer.AddObject(Obj: TObject): TObject;
begin
  Inc(ObjCount);
  if ObjCount > Length(ObjList) then
  begin
    SetLength(ObjList, MaxI(MinRefCContainerLength, Length(ObjList) * 2));
  end;
  ObjList[ObjCount-1] := Obj;
  Result := Obj;
end;

function TRefcountedContainer.AddPointer(Ptr: Pointer): Pointer;
begin
  Inc(PtrCount);
  if PtrCount > Length(PtrList) then
  begin
    SetLength(PtrList, MaxI(MinRefCContainerLength, Length(PtrList) * 2));
  end;
  PtrList[PtrCount-1] := Ptr;
  Result := Ptr;
end;

procedure TRefcountedContainer.AddObjects(Objs: array of TObject);
var i: Integer;
begin
  for i := Low(Objs) to High(Objs) do AddObject(Objs[i]);
end;

procedure TRefcountedContainer.AddPointers(Ptrs: array of Pointer);
var i: Integer;
begin
  for i := Low(Ptrs) to High(Ptrs) do AddPointer(Ptrs[i]);
end;

function TRefcountedContainer.GetContainer: IRefcountedContainer;
begin
  Result := Self;
end;

function CreateRefcountedContainer: IRefcountedContainer;
begin
  Result := TRefcountedContainer.Create;
end;

{ TBackBuffer }

function TBackBuffer.GetIndex(Dest: Pointer; Size: Integer): Integer;
begin
  Result := 0;
  while Result < FCount do
  begin
    if Assigned(FData^[Result].OrigData) then
    begin
      if FData^[Result].OrigData = Dest then
        Exit
    end else begin
      if Result > 0 then
        FData^[Result].Data := PtrOffs(FData^[Result-1].Data, FData^[Result-1].Size);
      {$IFOPT C+} Assert(PtrToInt(FData^[Result].Data) + Cardinal(Size) <= PtrToInt(FBuffer) + Cardinal(FSize), 'TBackBuffer.WriteProperty: Buffer is too small'); {$ENDIF}
      FData^[Result].OrigData := Dest;
      FData^[Result].Size := Size;
      Exit;
    end;
    Inc(Result);
  end;
  Assert(False, 'TBackBuffer.WriteProperty: No backbuffer slots left');
end;

constructor TBackBuffer.Create(Count, Size: Integer);
begin
  Assert((Count > 0) and (Size > 0));
  FCount := Count;
  GetMem(FBuffer, Size);
  {$IFOPT C+} FSize := Size; {$ENDIF}
  GetMem(FData, Count * SizeOf(TBackBufferData));
  FillChar(FData^, Count * SizeOf(TBackBufferData), 0);
  FData^[0].Data := FBuffer;
end;

destructor TBackBuffer.Destroy;
begin
  FreeMem(FData);
  FreeMem(FBuffer);
  inherited;
end;

procedure TBackBuffer.WriteProperty(Dest: Pointer; Size: Integer; const Value);
var
  Index: Integer;
begin
  Index := GetIndex(Dest, Size);
  Move(Value, FData^[Index].Data^, Size);
end;

procedure TBackBuffer.Flush;
var
  i: Integer;
begin
  for i := 0 to FCount-1 do
    if Assigned(FData^[i].OrigData) then
      Move(FData^[i].Data^, FData^[i].OrigData^, FData^[i].Size);
end;

end.
