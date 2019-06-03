{
@abstract(Properties store class)

Entity properties, collection of properties

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3property;
{$I g3ge.inc}

interface

uses
  g3types, {!}Template, g3IO;

type
  // Type for property names
  TPropertyName = AnsiString;

   // Possible property types
  TPropertyType = (
    // Boolean value
    ptBoolean,
    // 32-bit integer value
    ptInteger,
    // 64-bit integer value
    ptInt64,
    // Single-precision floating-point value
    ptSingle,
    // Double-precision floating-point value
    ptDouble,
    // Short string value
    ptShortString,
    // Ansi string value
    ptAnsiString,
    // Unicode string value
    ptString,
    // Enumerated value
    ptEnumeration,
    // Set of numbers [0..31]
    ptSet,
    // Pointer value
    ptPointer,
    // A link to an object
    ptObjectLink,
    // Bynary data
    ptBinary,
    // Object value
    ptObject,
    // Class value
    ptClass
  );

  { Type for serializable binary data.
    Published properties of descendant types will be included during serialization and deserialization.
    Actual type of such property should be always equal to its declared type.
    During destruction of an entity destructors of all published properties of descendant types will be called. }
  TBinaryData = class
  private
    // Indicates whether this instance is bound to an entity or must be destroyed by containing TProperties instance
    Bound: Boolean;
  public
    // Init instance with data from the given instance
    procedure Assign(AData: TBinaryData); virtual; abstract;
    // Reads binary data from input stream and returns True if success.
    function Read(IStream: TInputStream): Boolean; virtual; abstract;
    // Writes binary data to output stream and returns True if success.
    function Write(OStream: TOutputStream): Boolean; virtual; abstract;
    // Returns pointer to data
    function GetData(): Pointer; virtual; abstract;
  end;
  // Serializable binary data metaclass
  CBinaryData = class of TBinaryData;

  // Dynamic array if byte based binary data implementation
  TDynamicArray = class(TBinaryData)
  public
    Data: array of Byte;
    destructor Destroy(); override;
    procedure Assign(AData: TBinaryData); override;
    function Read(IStream: TInputStream): Boolean; override;
    function Write(OStream: TOutputStream): Boolean; override;
    function GetData(): Pointer; override;
  end;

  // Pointer based binary data implementation
  TPointerData = class(TBinaryData)
  public
    Data: Pointer;
    Size: Integer;
    destructor Destroy(); override;
    procedure Assign(AData: TBinaryData); override;
    function Read(IStream: TInputStream): Boolean; override;
    function Write(OStream: TOutputStream): Boolean; override;
    function GetData(): Pointer; override;
    procedure Allocate(ASize: Integer);
  end;

  // Text based binary data implementation
  TTextData = class(TBinaryData)
  public
    Data: AnsiString;
    destructor Destroy(); override;
    procedure Assign(AData: TBinaryData); override;
    function Read(IStream: TInputStream): Boolean; override;
    function Write(OStream: TOutputStream): Boolean; override;
    function GetData(): Pointer; override;
    function GetText(): AnsiString;
  end;

  PPropertyValue = ^TPropertyValue;
  // Data structure representing a property value
  TPropertyValue = packed record
    AsUnicodeString: UnicodeString;
    AsAnsiString: AnsiString;
    // Property value as various type
    case t: TPropertyType of
      ptBoolean: (AsBoolean: Boolean);
      ptInteger, ptEnumeration, ptSet: (AsInteger: Integer);
      ptInt64: (AsInt64: Int64);
      ptSingle: (AsSingle: Single);
      ptDouble: (AsDouble: Double);
      ptShortString: (AsShortString: ShortString);
      ptObject: (AsObject: TObject);
      ptClass: (AsClass: TClass);
      ptPointer: (AsPointer: Pointer);
      ptObjectLink: (Linked: TObject; LinkedClass: CAbstractEntity);
      ptBinary: (AsData: TBinaryData; BinDataClass: CBinaryData);
      //, ptMethod, ptVariant, ptInterface: ();
  end;

  PProperty = ^TProperty;
  // Data structure representing a property description
  TProperty = packed record
    // Property name
    Name: TPropertyName;
    // Property type information
    TypeId: TPropertyType;
  end;

  _VectorValueType = TProperty;
  _PVectorValueType = PProperty;
  {$MESSAGE 'Instantiating TPropertyList interface'}
  {$I tpl_coll_vector.inc}
  // Property list
  TPropertyList = class(_GenVector)
  private
    function IndexOfName(const Name: TPropertyName): Integer; {$I inline.inc}
  end;

  { @Abstract(Property collection)    }
  TProperties = class
  protected
    FProperties: TPropertyList;
    FValues: array of TPropertyValue;
    function GetIndex(const Name: TPropertyName): Integer;
    function GetPropByIndex(Index: Integer): PProperty;
    function GetValueByIndex(Index: Integer): PPropertyValue;
    function GetProperty(const Name: TPropertyName): TProperty;
    procedure SetProperty(const Name: TPropertyName; const Prop: TProperty);
    function GetValue(const Name: TPropertyName): PPropertyValue;
    procedure SetValue(const Name: TPropertyName; const Value: PPropertyValue);
    function GetCount: Integer;
  public
    // Creates an empty property collection
    constructor Create(); overload;
    // Destroys the property collection
    destructor Destroy; override;

    // Add a property or reset an existing one
    function AddProp(const Name: TPropertyName; TypeId: TPropertyType): PPropertyValue;

    procedure AddString(const Name: TPropertyName; const Value: string);
    procedure AddAnsiString(const Name: TPropertyName; const Value: AnsiString);
    procedure AddInt(const Name: TPropertyName; const Value: Integer);
    procedure AddInt64(const Name: TPropertyName; const Value: Int64);
    procedure AddSingle(const Name: TPropertyName; const Value: Single);

    // Property definitions
    property Prop[const Name: TPropertyName]: TProperty read GetProperty write SetProperty;
    // Property values
    property Value[const Name: TPropertyName]: PPropertyValue read GetValue write SetValue; default;
    // Property definition by index
    property PropByIndex[Index: Integer]: PProperty read GetPropByIndex;
    // Number of properties
    property Count: Integer read GetCount;
  end;

  // Abstract class responsible for properties serialization / deserialization
  TPropertyFilerBase = class
  public
    { Reads arbitrary list of property definitions and values from input stream and returns it.
      Returns nil if error occured.
      May raise EUnsupportedOperation if filer format doesn't support storage of property definitions. }
    function ReadArbitrary(IStream: TInputStream): TProperties; virtual; abstract;
    // Reads and fills values of specified in Properties list of properties from input stream and returns True if success.
    function Read(IStream: TInputStream; Properties: TProperties): Boolean; virtual; abstract;
    // Writes properties to output stream and returns True if success.
    function Write(OStream: TOutputStream; Properties: TProperties): Boolean; virtual; abstract;
  end;

  { Simple property filer implementation.
    Doesn't store property definitions but only values and therefore doesn't support read of arbitrary list of properties. }
  TSimplePropertyFiler = class(TPropertyFilerBase)
  public
    function ReadArbitrary(IStream: TInputStream): TProperties; override;
    function Read(IStream: TInputStream; Properties: TProperties): Boolean; override;
    function Write(OStream: TOutputStream; Properties: TProperties): Boolean; override;
  end;

  ECEPropertyError = class(EError);

  // Builds list of property definitions for the given class using RTTI
  function GetClassProperties(AClass: TClass): TProperties;
  // Builds list of property definitions for the given class using RTTI and fills values in the given object instance
  function GetClassPropertiesAndValues(AClass: TClass; AObj: TObject): TProperties;
  // Sets property values of the specified instance from the Properties
  procedure SetClassPropertiesAndValues(AObj: TObject; Properties: TProperties);

implementation

uses TypInfo, g3rtti;

const
  SIMPLE_PROPERTIES_BEGIN_SIGNATURE: TSignature = (Bytes: (Ord('S'), Ord('P'), Ord('_'), Ord('B')));
  SIMPLE_PROPERTIES_END_SIGNATURE: TSignature = (Bytes: (Ord('S'), Ord('P'), Ord('_'), Ord('E')));

function _VectorEquals(const v1, v2: TProperty): Boolean; {$I inline.inc}
begin
  Result := v1.Name = v2.Name;
end;

{ TDynamicArray }

destructor TDynamicArray.Destroy;
begin
  SetLength(Data, 0);
  Data := nil;
  inherited;
end;

procedure TDynamicArray.Assign(AData: TBinaryData);
begin
  if AData <> nil then Data := TDynamicArray(AData).Data;
end;

function TDynamicArray.Read(IStream: TInputStream): Boolean;
var DataSize: Integer;
begin
  Result := False;
  if not IStream.ReadCheck(DataSize, SizeOf(DataSize)) then Exit;
  SetLength(Data, DataSize);
  if DataSize > 0 then
    if not IStream.ReadCheck(Data[0], Length(Data)) then Exit;
  Result := True;
end;

function TDynamicArray.Write(OStream: TOutputStream): Boolean;
var DataSize: Integer;
begin
  Result := False;
  DataSize := Length(Data);
  if not OStream.WriteCheck(DataSize, SizeOf(DataSize)) then Exit;
  if DataSize > 0 then
    if not OStream.WriteCheck(Data[0], DataSize) then Exit;
  Result := True;
end;

function TDynamicArray.GetData: Pointer;
begin
  if Data <> nil then
    Result := @Data[0]
  else
    Result := nil;
end;

{ TTextData }

destructor TTextData.Destroy;
begin
  SetLength(Data, 0);
  inherited;
end;

procedure TTextData.Assign(AData: TBinaryData);
begin
  if not (AData is TTextData) then
    raise EInvalidArgument.Create('TTextData.Assign: Invalid type');
  Data := Copy(TTextData(AData).GetText(), STRING_INDEX_BASE, Length(TTextData(AData).GetText()));
end;

function TTextData.Read(IStream: TInputStream): Boolean;
begin
  Result := g3IO.ReadAnsiString(IStream, Data);
end;

function TTextData.Write(OStream: TOutputStream): Boolean;
begin
  Result := g3IO.WriteAnsiString(OStream, Data);
end;

function TTextData.GetData: Pointer;
begin
  if Data <> '' then
    Result := @Data[STRING_INDEX_BASE]
  else
    Result := nil;
end;

function TTextData.GetText: AnsiString;
begin
 Result := Data;
end;

{ TPointerData }

destructor TPointerData.Destroy;
begin
  if Assigned(Data) then FreeMem(Data, Size);
  Data := nil;
  inherited;
end;

procedure TPointerData.Assign(AData: TBinaryData);
begin
  if AData <> nil then
  begin
    Allocate(TPointerData(AData).Size);
    Move(TPointerData(AData).Data^, Data^, Size);
  end;
end;

function TPointerData.Read(IStream: TInputStream): Boolean;
begin
  Result := False;
  if not IStream.ReadCheck(Size, SizeOf(Size)) then Exit;
  Allocate(Size);
  if Size > 0 then
    if not IStream.ReadCheck(Data^, Size) then Exit;
  Result := True;
end;

function TPointerData.Write(OStream: TOutputStream): Boolean;
begin
  Result := False;
  if not OStream.WriteCheck(Size, SizeOf(Size)) then Exit;
  if Size > 0 then
    if not OStream.WriteCheck(Data^, Size) then Exit;
  Result := True;
end;

function TPointerData.GetData: Pointer;
begin
  Result := Data;
end;

procedure TPointerData.Allocate(ASize: Integer);
begin
  if Assigned(Data) then FreeMem(Data, Size);
  Data := nil;
  Size := ASize;
  if Size > 0 then
    GetMem(Data, Size);
end;

{ TPropertyList }

function TPropertyList.IndexOfName(const Name: TPropertyName): Integer;
begin
  Result := FSize;
  while (Result >= 0) and (FValues[Result].Name <> Name) do Dec(Result);
end;

{$MESSAGE 'Instantiating TPropertyList'}
{$I tpl_coll_vector.inc}

{ TProperties }

function TProperties.GetIndex(const Name: TPropertyName): Integer;
begin
  Result := FProperties.Size-1;
  while (Result >= 0) and (FProperties[Result].Name <> Name) do Dec(Result);
end;

function TProperties.GetPropByIndex(Index: Integer): PProperty;
begin
  Result := FProperties.GetPtr(Index); //TODO: handle non existing name
end;

function TProperties.GEtValueByIndex(Index: Integer): PPropertyValue;
begin
  Result := @FValues[Index]; //TODO: handle non existing name
end;

function TProperties.GetProperty(const Name: TPropertyName): TProperty;
begin
  Result := GetPropByIndex(GetIndex(Name))^;
end;

procedure TProperties.SetProperty(const Name: TPropertyName; const Prop: TProperty);
begin

end;

function TProperties.GetValue(const Name: TPropertyName): PPropertyValue;
begin
  Result := GetValueByIndex(GetIndex(Name));
end;

procedure TProperties.SetValue(const Name: TPropertyName; const Value: PPropertyValue);
begin

end;

function TProperties.GetCount: Integer;
begin
  Result := FProperties.GetSize();
end;

constructor TProperties.Create;
begin
  FProperties := TPropertyList.Create();
end;

destructor TProperties.Destroy;
var i: Integer;
begin
  for i := 0 to FProperties.Size-1 do
    if (FProperties[i].TypeId = ptBinary) and Assigned(FValues[i].AsData) and not FValues[i].AsData.Bound then
      FValues[i].AsData.Free();
  FProperties.Free();
  FProperties := nil;
  inherited;
end;

function TProperties.AddProp(const Name: TPropertyName; TypeId: TPropertyType): PPropertyValue;
var
  Index: Integer;
begin
  Index := GetIndex(Name);
  if Index = -1 then begin
    Index := FProperties.Size;
    FProperties.Size := Index + 1;
    SetLength(FValues, FProperties.Size);
  end else
    Finalize(FValues[Index]);               // reset existing values
  Result := @FValues[Index];
  FProperties.ValuesPtr[Index].Name := Name;
  FProperties.ValuesPtr[Index].TypeId := TypeId;
end;

procedure TProperties.AddString(const Name: TPropertyName; const Value: string);
begin
  AddProp(Name, ptString)^.AsUnicodeString := Value;
end;

procedure TProperties.AddAnsiString(const Name: TPropertyName; const Value: AnsiString);
begin
  AddProp(Name, ptAnsiString)^.AsAnsiString := Value;
end;

procedure TProperties.AddInt(const Name: TPropertyName; const Value: Integer);
begin
  AddProp(Name, ptInteger)^.AsInteger := Value;
end;

procedure TProperties.AddInt64(const Name: TPropertyName; const Value: Int64);
begin
  AddProp(Name, ptInt64)^.AsInt64 := Value;
end;

procedure TProperties.AddSingle(const Name: TPropertyName; const Value: Single);
begin
  AddProp(Name, ptSingle)^.AsSingle := Value;
end;

{ TPropertyFiler }

function TSimplePropertyFiler.ReadArbitrary(IStream: TInputStream): TProperties;
begin
  Result := nil;
  raise EUnsupportedOperation.Create('Arbitrary properties deserialization not supported');
end;

function TSimplePropertyFiler.Read(IStream: TInputStream; Properties: TProperties): Boolean;
var
  i: Integer;
  Sign: TSignature;
  Prop: PProperty;
  Value: PPropertyValue;
begin
  if Properties = nil then raise EInvalidArgument.Create('Properties argument is nil');
  Result := false;

  if not IStream.ReadCheck(Sign.DWord, SizeOf(SIMPLE_PROPERTIES_BEGIN_SIGNATURE.DWord)) then Exit;
  if Sign.DWord <> SIMPLE_PROPERTIES_BEGIN_SIGNATURE.DWord then Exit;

  for i := 0 to Properties.Count-1 do
  begin
    Prop := Properties.GetPropByIndex(i);
    Value := Properties.GetValueByIndex(i);
    case Prop^.TypeId of
      ptBoolean, ptInteger,
      ptEnumeration, ptSet: if not IStream.ReadCheck(Value^.AsInteger, SizeOf(Value^.AsInteger)) then Exit;
      ptInt64: if not IStream.ReadCheck(Value^.AsInt64, SizeOf(Value^.AsInt64)) then Exit;
      ptSingle: if not IStream.ReadCheck(Value^.AsSingle, SizeOf(Value^.AsSingle)) then Exit;
      ptDouble: if not IStream.ReadCheck(Value^.AsDouble, SizeOf(Value^.AsDouble)) then Exit;
      ptShortString: if not g3IO.ReadShortString(IStream, Value^.AsShortString) then Exit;
      ptAnsiString: if not g3IO.ReadAnsiString(IStream, Value^.AsAnsiString) then Exit;
      ptString: if not g3IO.ReadUnicodeString(IStream, Value^.AsUnicodeString) then Exit;
      ptBinary: begin
        if not Assigned(Value^.AsData) or Value^.AsData.Bound then
          Value^.AsData := Value^.BinDataClass.Create();
        if not Value^.AsData.Read(IStream) then Exit;
      end;
      ptObjectLink: if not g3IO.ReadAnsiString(IStream, Value^.AsAnsiString) then Exit;
      else Assert(False, 'Invalid property type: ' + TypInfo.GetEnumName(TypeInfo(TPropertyType), Ord(Prop.TypeId)));
    end;
  end;

  if not IStream.ReadCheck(Sign.DWord, SizeOf(SIMPLE_PROPERTIES_END_SIGNATURE.DWord)) then Exit;
  if Sign.DWord <> SIMPLE_PROPERTIES_END_SIGNATURE.DWord then Exit;

  Result := True;
end;

function TSimplePropertyFiler.Write(OStream: TOutputStream; Properties: TProperties): Boolean;

var
  i: Integer;
  Prop: PProperty;
  Value: PPropertyValue;
begin
  if Properties = nil then raise EInvalidArgument.Create('Properties argument is nil');
  Result := False;
  if not OStream.WriteCheck(SIMPLE_PROPERTIES_BEGIN_SIGNATURE.DWord, SizeOf(SIMPLE_PROPERTIES_BEGIN_SIGNATURE.DWord)) then Exit;
  for i := 0 to Properties.Count-1 do
  begin
    Prop := Properties.GetPropByIndex(i);
    Value := Properties.GetValueByIndex(i);
    case Prop^.TypeId of
      ptBoolean, ptInteger,
      ptEnumeration, ptSet: if not OStream.WriteCheck(Value^.AsInteger, SizeOf(Value^.AsInteger)) then Exit;
      ptInt64: if not OStream.WriteCheck(Value^.AsInt64, SizeOf(Value^.AsInt64)) then Exit;
      ptSingle: if not OStream.WriteCheck(Value^.AsSingle, SizeOf(Value^.AsSingle)) then Exit;
      ptDouble: if not OStream.WriteCheck(Value^.AsDouble, SizeOf(Value^.AsDouble)) then Exit;
      ptShortString: if not g3IO.WriteShortString(OStream, Value^.AsShortString) then Exit;
      ptAnsiString: if not g3IO.WriteAnsiString(OStream, Value^.AsAnsiString) then Exit;
      ptString: if not g3IO.WriteUnicodeString(OStream, Value^.AsUnicodeString) then Exit;
      ptBinary: if not Value^.AsData.Write(OStream) then Exit;
      ptObjectLink: if not g3IO.WriteAnsiString(OStream, Value^.AsAnsiString) then Exit;
      else Assert(False, 'Invalid property type: ' + TypInfo.GetEnumName(TypeInfo(TPropertyType), Ord(Prop.TypeId)));
    end;
  end;
  if not OStream.WriteCheck(SIMPLE_PROPERTIES_END_SIGNATURE.DWord, SizeOf(SIMPLE_PROPERTIES_END_SIGNATURE.DWord)) then Exit;
  Result := True;
end;

function GetClassProperties(AClass: TClass): TProperties;
begin
  Result := GetClassPropertiesAndValues(AClass, nil);
end;

function GetClassPropertiesAndValues(AClass: TClass; AObj: TObject): TProperties;
var
  PropInfos: PPropList;
  PropInfo: PPropInfo;
  Count, i: Integer;
  Value: PPropertyValue;
  OClass: TClass;
begin
  Assert((AObj = nil) or (AObj.ClassType = AClass));
  Result := TProperties.Create();
  Count := g3rtti.GetClassPropList(AClass, PropInfos);

  try
    for i := 0 to Count - 1 do
    begin
      PropInfo := PropInfos^[i];
      WriteLn('Prop: ', PropInfo^.Name, ', type: ', TypInfo.GetEnumName(TypeInfo(TTypeKind), Ord(PropInfo^.PropType^.Kind)), ', type name: ', PropInfo^.PropType^.Name);
      case PropInfo^.PropType^.Kind of
        {$IF Declared(tkBool)}
        tkBool,
        {$IFEND}
        tkInteger:
        if PropInfo^.PropType^.Name = 'Boolean' then
        begin
          Value := Result.AddProp(PropInfo^.Name, ptBoolean);
          if Assigned(AObj) then
            Value^.AsBoolean := TypInfo.GetOrdProp(AObj, PropInfo) = Ord(True);
        end else begin
          Value := Result.AddProp(PropInfo^.Name, ptInteger);
          if Assigned(AObj) then
            Value^.AsInteger := TypInfo.GetOrdProp(AObj, PropInfo);
        end;
        tkInt64: begin
          Value := Result.AddProp(PropInfo^.Name, ptInt64);
          if Assigned(AObj) then
            Value^.AsInt64 := TypInfo.GetInt64Prop(AObj, PropInfo);
        end;
        tkFloat:
          if PropInfo^.PropType^.Name = 'Single' then
          begin
            Value := Result.AddProp(PropInfo^.Name, ptSingle);
            if Assigned(AObj) then
              Value^.AsSingle := TypInfo.GetFloatProp(AObj, PropInfo);
          end
          else if PropInfo^.PropType^.Name = 'Double' then
          begin
            Value := Result.AddProp(PropInfo^.Name, ptDouble);
            if Assigned(AObj) then
              Value^.AsDouble := TypInfo.GetFloatProp(AObj, PropInfo);
          end else
            raise ECEPropertyError.Create('Unsupported property type: ' + string(PropInfo^.PropType^.Name));
        tkEnumeration: begin
          Value := Result.AddProp(PropInfo^.Name, ptEnumeration);
          if Assigned(AObj) then
            Value^.AsInteger := TypInfo.GetOrdProp(AObj, PropInfo);
        end;
        tkSet: begin
          Value := Result.AddProp(PropInfo^.Name, ptSet);
          if Assigned(AObj) then
            Value^.AsInteger := TypInfo.GetOrdProp(AObj, PropInfo);
        end;
        {$IF Declared(tkAString)}tkAString, {$IFEND}
        tkLString: begin
          if PropInfo^.PropType^.Name = 'UTF8String' then
          begin
            Value := Result.AddProp(PropInfo^.Name, ptString);
            if Assigned(AObj) then
              Value^.AsUnicodeString := UnicodeString(GetAnsiStrProp(AObj, PropInfo));
          end else begin
            Value := Result.AddProp(PropInfo^.Name, ptAnsiString);
            if Assigned(AObj) then
              Value^.AsAnsiString := GetAnsiStrProp(AObj, PropInfo);
          end;
        end;
        {$IF Declared(tkUString)}tkUString: begin
          Value := Result.AddProp(PropInfo^.Name, ptString);
          if Assigned(AObj) then
            Value^.AsUnicodeString := UnicodeString(TypInfo.GetUnicodeStrProp(AObj, PropInfo));
        end;
        {$IFEND}
        tkString: begin
          if PropInfo^.PropType^.Name = 'ShortString' then
          begin
            Value := Result.AddProp(PropInfo^.Name, ptShortString);
            if Assigned(AObj) then
              Value^.AsShortString := ShortString(TypInfo.GetStrProp(AObj, PropInfo));
          end else begin
            {$IFDEF UNICODE}
              Value := Result.AddProp(PropInfo^.Name, ptString);
              if Assigned(AObj) then
                Value^.AsUnicodeString := UnicodeString(TypInfo.GetUnicodeStrProp(AObj, PropInfo));
            {$ELSE}
              Value := Result.AddProp(PropInfo^.Name, ptAnsiString);
              if Assigned(AObj) then
                Value^.AsAnsiString := GetAnsiStrProp(AObj, PropInfo);
            {$ENDIF}
          end;
        end;
        tkWString: begin
          Value := Result.AddProp(PropInfo^.Name, ptString);
          if Assigned(AObj) then
            Value^.AsUnicodeString := TypInfo.GetWideStrProp(AObj, PropInfo);
        end;

        tkClass: begin
          OClass := g3rtti.GetObjectPropClass(AClass, PropInfo);
          if OClass.InheritsFrom(TBinaryData) then                    // Binary data case
          begin
            Value := Result.AddProp(PropInfo^.Name, ptBinary);
            Value^.BinDataClass := CBinaryData(OClass);
            if Assigned(AObj) then
            begin
              Value^.AsData := Value^.BinDataClass.Create();
              Value^.AsData.Assign(TBinaryData(TypInfo.GetObjectProp(AObj, PropInfo)));
            end;
          end else if OClass.InheritsFrom(TAbstractEntity) then       // Object link case
          begin
            Value := Result.AddProp(PropInfo^.Name, ptObjectLink);
            Value^.LinkedClass := CAbstractEntity(OClass);
            if Assigned(AObj) then
            begin
              Value^.Linked := TypInfo.GetObjectProp(AObj, PropInfo);
              Value^.AsAnsiString := TAbstractEntity(Value^.Linked).GetFullName();
            end;
          end else
            raise ECEPropertyError.Create('Property of unsupported class: ' + string(OClass.ClassName));
        end;
        else
          raise ECEPropertyError.Create('Unsupported property type: ' + string(PropInfo^.PropType^.Name));
      end;
    end;
  finally
    FreeMem(PropInfos);
  end;
end;


{$WARNINGS OFF}

procedure SetClassPropertiesAndValues(AObj: TObject; Properties: TProperties);

var

  i: Integer;

  Prop: TProperty;

  Value: TPropertyValue;

begin

  for i := 0 to Properties.Count-1 do

  begin

    Prop := Properties.PropByIndex[i]^;

    Value := Properties.GetValueByIndex(i)^;

    case Prop.TypeId of

      ptBoolean, ptInteger,
      ptEnumeration, ptSet: TypInfo.SetOrdProp(AObj, Prop.Name, Value.AsInteger);
      ptInt64: TypInfo.SetInt64Prop(AObj, Prop.Name, Value.AsInt64);
      ptSingle: TypInfo.SetFloatProp(AObj, Prop.Name, Value.AsSingle);
      ptDouble: TypInfo.SetFloatProp(AObj, Prop.Name, Value.AsDouble);

      ptShortString: TypInfo.SetStrProp(AObj, Prop.Name, Value.AsShortString);
      ptAnsiString: TypInfo.SetStrProp(AObj, Prop.Name, Value.AsAnsiString);
      ptString: {$IFDEF UNICODE}
        TypInfo.SetUnicodeStrProp(AObj, Prop.Name, Value.AsUnicodeString);
        {$ELSE}
        TypInfo.SetStrProp(AObj, Prop.Name, Value.AsAnsiString);
        {$ENDIF}

      ptPointer: ;
      ptObjectLink: begin
        if ((Value.LinkedClass <> nil) and (Value.Linked is Value.LinkedClass)) or (Value.Linked is TAbstractEntity) then
          TypInfo.SetObjectProp(AObj, Prop.Name, Value.Linked)
        else
          if AObj is TAbstractEntity then
            TAbstractEntity(AObj).SetObjectLink(Prop.Name, Value.AsAnsiString)
          else
            raise ECEPropertyError.Create('Class doesn''t support object links: ' + string(AObj.ClassName));
      end;
      ptBinary: begin
        TypInfo.SetObjectProp(AObj, Prop.Name, Value.AsData);
        Value.AsData.Bound := True;
      end;
      ptObject: ;
      ptClass: ;
      else
        raise ECEPropertyError.Create('Unsupported property type: ' + TypInfo.GetEnumName(TypeInfo(TPropertyType), Ord(Prop.TypeId)));
    end;
  end;
end;

{$WARNINGS ON}

end.
