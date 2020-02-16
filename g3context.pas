{
@abstract(Application context)

Context aims to manage and provide dependencies on demand such as singleton instances

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3context;
{$I g3config.inc}

interface

uses
  g3property,
  g3types;

type
{$MESSAGE 'Instantiating TClassObjectMap interface'}
    {$DEFINE _HashMapTypeNullable}
  _HashMapKeyType = TClass;
  _HashMapValueType = TObject;
    {$I tpl_coll_hashmap.inc}
  // Maps class to instance
  TClassObjectMap = _GenHashMap;

  // Class to store application and subsystem's parameters
  TConfig = class
  private
    Data: TProperties;
    function GetValue(const Name: TPropertyName): string;
    procedure SetValue(const Name: TPropertyName; const Value: string);
  public
    constructor Create;
    destructor Destroy(); override;

    procedure AddProperties(Properties: TProperties);
    procedure Remove(const Name: TPropertyName);
    function GetString(const Name: TPropertyName; const Def: string = ''): string;
    function GetAnsiString(const Name: TPropertyName; const Def: AnsiString = ''): AnsiString;
    function GetFlag(const Name: TPropertyName; const Def: Boolean = false): Boolean;
    function GetInt(const Name: TPropertyName; const Def: Integer = 0): Integer;
    function GetInt64(const Name: TPropertyName; const Def: Int64 = 0): Int64;
    function GetFloat(const Name: TPropertyName; const Def: Single = 0.0): Single;
    function GetPointer(const Name: TPropertyName; const Def: Pointer = nil): Pointer;

    procedure SetFlag(const Name: TPropertyName; Value: Boolean);
    procedure SetInt(const Name: TPropertyName; Value: Integer);
    procedure SetInt64(const Name: TPropertyName; Value: Int64);
    procedure SetFloat(const Name: TPropertyName; Value: Single);
    procedure SetPointer(const Name: TPropertyName; Value: Pointer);
    property ValuesStr[const Name: TPropertyName]: string read GetValue write SetValue; default;
  end;

{ Returns an singleton instance of the given class previously stored or nil if no such instance exists }
  function GetSingleton(AClass: TClass): TObject;
{ Adds a singleton instance to storage for the given class. If AClass is nil class of the instance will be used.
  Returns True if there was no instance for the class in storage. }
  function AddSingleton(Instance: TObject; AClass: TClass = nil): Boolean;
{ Removes a singleton instance from storage for the given class. Returns True if there was such instance. }
  function RemoveSingleton(AClass: TClass): Boolean;
{ Logs error message for situation when singleton instance was already created }
  procedure LogSingletonExists(Cls: TClass);

implementation

uses
  g3log, g3common;

  {$MESSAGE 'Instantiating TClassObjectMap'}
  {$I tpl_coll_hashmap.inc}

const
  LOGTAG = 'context';

var
  ClassObjectMap: TClassObjectMap;

procedure LogSingletonExists(Cls: TClass);
begin
  g3log.DoLog(LOGTAG, '%s singleton was already created', llError, [Cls.ClassName()]);
end;

function GetSingleton(AClass: TClass): TObject;
begin
  Result := ClassObjectMap[AClass];
end;

function AddSingleton(Instance: TObject; AClass: TClass = nil): Boolean;
begin
  Result := ClassObjectMap.PutValue(AClass, Instance);
end;

function RemoveSingleton(AClass: TClass): Boolean;
begin
  Result := ClassObjectMap.Remove(AClass);
end;

{ TConfig }

function TConfig.GetValue(const Name: TPropertyName): string;
var
  Value: PPropertyValue;
begin
  Result := '';
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Value^.AsUnicodeString
end;

procedure TConfig.SetValue(const Name: TPropertyName; const Value: string);
begin
  Data.AddString(Name, Value);
end;

constructor TConfig.Create;
begin
  Data := TProperties.Create();
  if not g3context.AddSingleton(Self, TConfig) then
    LogSingletonExists(TConfig);
end;

destructor TConfig.Destroy;
begin
  Data.Free();
  Data := nil;
  inherited;
end;

procedure TConfig.AddProperties(Properties: TProperties);
begin
  Data.AddAll(Properties);
end;

procedure TConfig.Remove(const Name: TPropertyName);
begin
  g3log.DoLog(LOGTAG, 'Not implemented', llInfo, []);
end;

function TConfig.GetString(const Name: TPropertyName; const Def: string = ''): string;
var
  Value: PPropertyValue;
begin
  Result := Def;
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Value^.AsUnicodeString;
end;

function TConfig.GetAnsiString(const Name: TPropertyName; const Def: AnsiString = ''): AnsiString;
var
  Value: PPropertyValue;
begin
  Result := Def;
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Value^.AsAnsiString;
end;

function TConfig.GetFlag(const Name: TPropertyName; const Def: Boolean = false): Boolean;
var
  Value: PPropertyValue;
begin
  Result := Def;
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Value^.AsBoolean
end;

function TConfig.GetInt(const Name: TPropertyName; const Def: Integer = 0): Integer;
var
  Value: PPropertyValue;
begin
  Result := Def;
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Value^.AsInteger
end;

function TConfig.GetInt64(const Name: TPropertyName; const Def: Int64 = 0): Int64;
var
  Value: PPropertyValue;
begin
  Result := Def;
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Value^.AsInt64
end;

function TConfig.GetFloat(const Name: TPropertyName; const Def: Single = 0.0): Single;
var
  Value: PPropertyValue;
begin
  Result := Def;
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Value^.AsSingle
end;

function TConfig.GetPointer(const Name: TPropertyName; const Def: Pointer = nil): Pointer;
var
  Value: PPropertyValue;
begin
  Result := Def;
  Value := Data.Value[Name];
  if Assigned(Value) then
    Result := Pointer(Value^.AsInt64)
end;

procedure TConfig.SetFlag(const Name: TPropertyName; Value: Boolean);
begin
  Data.AddBoolean(Name, Value);
end;

procedure TConfig.SetInt(const Name: TPropertyName; Value: Integer);
begin
  Data.AddInt(Name, Value);
end;

procedure TConfig.SetInt64(const Name: TPropertyName; Value: Int64);
begin
  Data.AddInt64(Name, Value);
end;

procedure TConfig.SetPointer(const Name: TPropertyName; Value: Pointer);
begin
  Data.AddInt64(Name, PtrToInt(Value));
end;

procedure TConfig.SetFloat(const Name: TPropertyName; Value: Single);
begin
  Data.AddSingle(Name, Value);
end;

initialization
  ClassObjectMap := TClassObjectMap.Create();
finalization
  ClassObjectMap.Free();
end.
