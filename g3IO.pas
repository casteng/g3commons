{
@abstract(Base input/output unit)

The unit contains common input/output classes

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3IO;
{$I g3ge.inc}

interface

uses g3types;

const
  // A substring to separate URL type part in a resource URL
  URL_TYPE_SEPARATOR = '://';
  // File protocol (default)
  PROTOCOL_FILE = 'file';
  // Protocol to query assets which location is platform dependent
  PROTOCOL_ASSET = 'asset';

type
  { @Abstract(Abstract binary stream) }
  TG3Stream = class
  protected
    FPosition: Int64;
    FClosed: Boolean;
  public
    // Calls Close() before destruction
    destructor Destroy(); override;
    // Closes this stream and releases any system resources associated with the stream
    procedure Close(); virtual; abstract;
    // Current position within the stream in bytes
    property Position: Int64 read FPosition;
  end;

  { @Abstract(Abstract input binary stream) }
  TInputStream = class(TG3Stream)
  protected
    // Returns stream size or -1 if not applicable/supported
    function GetSize(): Int64; virtual; abstract;
  public
    // Reads up to Count of bytes from this stream to Buffer, moves current position forward for number of bytes read and returns that number
    function Read(var Buffer; const Count: Cardinal): Cardinal; virtual; abstract;
    // Reads Count bytes from this stream to Buffer, moves current position forward for the number of bytes read and returns True if all the Count bytes were successfully read
    function ReadCheck(var Buffer; const Count: Cardinal): Boolean;
    property Size: Int64 read GetSize;
  end;

  { @Abstract(Abstract output binary stream) }
  TOutputStream = class(TG3Stream)
  public
    // Writes up to Count bytes from Buffer to this stream, moves current position forward for the number of bytes written and returns that number
    function Write(const Buffer; const Count: Cardinal): Cardinal; virtual; abstract;
    // Writes Count bytes from Buffer to this stream, moves current position forward for the number of bytes written and returns True if all the Count bytes were successfully written
    function WriteCheck(const Buffer; const Count: Cardinal): Boolean;
  end;

  // File usage mode
  TFileUsage = (fuRead, fuWrite, fuReadWrite, fuAppend);

  TFileShare = (smAllowAll, smAllowRead, smExclusive);

  TG3File = class
  private
    FFileName: string;
    FFileSize: Int64;
    FPosition: Int64;
    F: file;
    FClosed: Boolean;
  public
    constructor Create(const AFileName: string; const Usage: TFileUsage = fuReadWrite; const ShareMode: TFileShare = smAllowAll);
    function Seek(const NewPos: Int64): Boolean;
    // Closes file
    procedure Close;
    destructor Destroy; override;

    function Read(var Buffer; const Count: Cardinal): Cardinal;
    function Write(const Buffer; const Count: Cardinal): Cardinal;

    // Associated file name
    property Filename: string read FFileName;
  end;

  { @Abstract(File input stream)
    Provides input stream implementation for binary files }
  TFileInputStream = class(TInputStream)
  private
    FFile: TG3File;
  protected
    function GetSize(): Int64; override;
  public
    // Creates a file stream associating it with file with the given file name
    constructor Create(const AFileName: string);
    // Frees FFile
    destructor Destroy(); override;
    // Closes file
    procedure Close; override;
    function Read(var Buffer; const Count: Cardinal): Cardinal; override;
  end;

  { @Abstract(File output stream)
  Provides output stream implementation for binary files }
  TFileOutputStream = class(TOutputStream)
  private
    FFile: TG3File;
  public
    // Creates a file stream associating it with file with the given file name
    constructor Create(const AFileName: string; const ShareMode: TFileShare = smAllowAll);
    // Frees FFile
    destructor Destroy(); override;
    // Closes file
    procedure Close; override;
    function Write(const Buffer; const Count: Cardinal): Cardinal; override;
  end;

  EIOError = class(EError)
  public
    Code: Integer;
    constructor Create(ACode: Integer; AMsg: string);
  end;

  // Variables of this type are data type identifiers i.e. bitmap, .obj model, etc
  TDataTypeID = TSignature;

  function ReadShortString(InS: TInputStream; out Str: ShortString): Boolean;
  function WriteShortString(OutS: TOutputStream; const Str: ShortString): Boolean;
  function ReadAnsiString(InS: TInputStream; out Str: AnsiString): Boolean;
  function WriteAnsiString(OutS: TOutputStream; const Str: AnsiString): Boolean;
  function ReadUnicodeString(InS: TInputStream; out Str: UnicodeString): Boolean;
  function WriteUnicodeString(OutS: TOutputStream; const Str: UnicodeString): Boolean;

  function GetFileModifiedTime(const FileName: string): TDateTime;

  // Returns data type ID based on file extension
  function GetDataTypeFromExt(const ext: string): TDataTypeID;
  // Returns protocol part of URL
  function GetProtocolFromUrl(const URL: string): AnsiString;
  // Returns data type ID based on URL (extension part)
  function GetDataTypeIDFromUrl(const URL: string): TDataTypeID;
  // Returns path part of URL
  function GetPathFromURL(const URL: string): string;
  // Returns modification time of a resource by URL or 0 zero if not found or modification time is unsupported
  function GetResourceModificationTime(const URL: string): TDateTime;
  (* Returns input stream for a resource specified by URL or nil if resource not found.
     Currently only local files and assets supported. *)
  function GetResourceInputStream(const URL: string): TInputStream;

implementation

uses
  SysUtils, g3common, g3context, g3log;

  function ReadShortString(InS: TInputStream; out Str: ShortString): Boolean;
  var l: Byte;
  begin
    Result := InS.ReadCheck(l, SizeOf(l));
    if Result then
    begin
      SetLength(Str, l);
      if l > 0 then Result := InS.ReadCheck(Str[1], l);
    end;
  end;

  function WriteShortString(OutS: TOutputStream; const Str: ShortString): Boolean;
  begin
    Result := OutS.WriteCheck(Str[0], Length(Str)+1);
  end;

  function ReadAnsiString(InS: TInputStream; out Str: AnsiString): Boolean;
  var l: Cardinal;
  begin
    Result := InS.ReadCheck(l, SizeOf(l));
    if Result then
    begin
      SetLength(Str, l);
      if l > 0 then Result := InS.ReadCheck(Pointer(Str)^, l * SizeOf(AnsiChar));
    end;
  end;

  function WriteAnsiString(OutS: TOutputStream; const Str: AnsiString): Boolean;
  var l: Cardinal;
  begin
    l := Length(Str);
    Result := OutS.WriteCheck(l, SizeOf(l));
    if Result and (l > 0) then
      Result := OutS.WriteCheck(Pointer(Str)^, l * SizeOf(AnsiChar));
  end;

  function ReadUnicodeString(InS: TInputStream; out Str: UnicodeString): Boolean;
  var
    l: Cardinal;
    UTF8: UTF8String;
  begin
    Str := '';
    Result := InS.ReadCheck(l, SizeOf(l));
    if Result and (l > 0) then
    begin
      SetLength(UTF8, l);
      Result := InS.ReadCheck(Pointer(UTF8)^, l * SizeOf(AnsiChar));
      {$IFDEF UNICODE_STRING}
      Str := UTF8ToUnicodeString(UTF8);
      {$ELSE}
        {$IFDEF FPC}
        Str := UTF8Decode(UTF8);

        {$ELSE}
        Str := UTF8Decode(UTF8);

        {$ENDIF}
      {$ENDIF}
    end;
  end;

  function WriteUnicodeString(OutS: TOutputStream; const Str: UnicodeString): Boolean;
  var
    l: Cardinal;
    UTF8: UTF8String;
  begin
      UTF8 := UTF8Encode(Str);
      l := Length(UTF8);
      Result := OutS.WriteCheck(l, SizeOf(l));
      if Result and (l > 0) then
        Result := OutS.WriteCheck(Pointer(UTF8)^, l * SizeOf(AnsiChar));
  end;

function GetFileModifiedTime(const FileName: string): TDateTime;
var sr: TSearchRec;
begin
  Result := 0;
  if SysUtils.FindFirst(FileName, faDirectory, sr) = 0 then
  begin
    {$IFDEF HAS_FILE_TIMESTAMP}
      Result := sr.TimeStamp;
    {$ELSE}
      Result := SysUtils.FileDateToDateTime(sr.Time);
    {$ENDIF}
  end;
  SysUtils.FindClose(sr);
end;

function GetDataTypeFromExt(const ext: string): TDataTypeID;
var i: Integer;
begin
  Result.Bytes[0] := Ord('.');
  for i := 1 to High(Result.Bytes) do
    if i <= Length(ext) then
      Result.Bytes[i] := Ord(UpperCase(Copy(ext, i, 1))[1])
    else
      Result.Bytes[i] := Ord(' ');
end;

function GetProtocolFromUrl(const URL: string): AnsiString;
var Ind: Integer;
begin
  Ind := Pos(URL_TYPE_SEPARATOR, URL);
  if Ind >= STRING_INDEX_BASE then
    Result := AnsiString(Copy(URL, STRING_INDEX_BASE, Ind-1))
  else
    Result := '';
end;

function GetDataTypeIDFromUrl(const URL: string): TDataTypeID;
begin
  Result := GetDataTypeFromExt(GetFileExt(URL));
end;

function GetPathFromURL(const URL: string): string;
var Ind: Integer;
begin
  Ind := Pos(URL_TYPE_SEPARATOR, URL);
  if Ind >= STRING_INDEX_BASE then
    Result := Copy(URL, Ind + 3, Length(URL))
  else
    Result := URL;
end;

function IsLocalFileProtocol(const Protocol: AnsiString): Boolean;
begin
  Result := (Protocol = '') or (Protocol = PROTOCOL_FILE) or (Protocol = PROTOCOL_ASSET);
end;

function JoinPaths(const Path1, Path2: string): string;
begin
  Result := IncludeTrailingPathDelimiter(Path1) + ExcludeTrailingPathDelimiter(Path2);
end;

function ResolveFilenameFromUrl(const URL: string): string;
var
  Protocol: AnsiString;
  AssetsPath: string;
  Config: TConfig;
begin
  Protocol := GetProtocolFromUrl(URL);
  if IsLocalFileProtocol(Protocol) then
  begin
    Result := GetPathFromURL(URL);
    if Protocol = PROTOCOL_ASSET then
    begin
      Config := g3context.GetSingleton(TConfig) as TConfig;
      AssetsPath := Config['Path.Asset'];
      Result := GetPathRelativeToFile(ParamStr(0), JoinPaths(AssetsPath, Result));
    end;
  end else
    Raise EIOError.CreateFmt('Unknown protocol in URL: %s', [Protocol]);
end;

function GetResourceModificationTime(const URL: string): TDateTime;
var FileName: string;
begin
  FileName := ResolveFilenameFromUrl(URL);
  if FileExists(FileName) then
    Result := GetFileModifiedTime(FileName)
  else
    Result := 0;
end;

function GetResourceInputStream(const URL: string): TInputStream;
var
  FileName: string;
begin
  FileName := ResolveFilenameFromUrl(URL);
  if FileExists(FileName) then
    Result := TFileInputStream.Create(FileName)
  else begin
    g3log.DoLog('io', 'Resource not found by URL: %s', llWarning, [URL]);
    Result := nil;
  end;
end;

{ TG3Stream }

destructor TG3Stream.Destroy;
begin
  Close();
  inherited;
end;

{ TInputStream }

function TInputStream.ReadCheck(var Buffer; const Count: Cardinal): Boolean;
begin
  Result := Read(Buffer, Count) = Count;
end;

{ TOutputStream }

function TOutputStream.WriteCheck(const Buffer; const Count: Cardinal): Boolean;
begin
  Result := Write(Buffer, Count) = Count;
end;

{ TG3File }

constructor TG3File.Create(const AFileName: string; const Usage: TFileUsage = fuReadWrite; const ShareMode: TFileShare = smAllowAll);
var OldFileMode: Byte;
begin
  OldFileMode := FileMode;
  case ShareMode of
    smAllowAll: FileMode := 0;
    smAllowRead: FileMode := fmShareDenyWrite;
    smExclusive: FileMode := fmShareExclusive;
  end;
  FFileName := ExpandFileName(AFileName);
  AssignFile(F, FFileName);
  case Usage of
    fuRead: begin
      FileMode := FileMode or fmOpenRead;
      Reset(F, 1);
    end;
    fuReadWrite: begin
      FileMode := FileMode or fmOpenReadWrite;
      {$I-}
      Reset(F, 1);
      {$I+}
      if (IOResult <> 0) and not FileExists(FFileName) then Rewrite(F, 1);
    end;
    fuWrite: Rewrite(F, 1);
    fuAppend: if FileExists(FFileName) then
    begin
      FileMode := FileMode or fmOpenReadWrite;
      Reset(F, 1);
      FFileSize := FileSize(F);
      Seek(FFileSize);
    end else Rewrite(F, 1);
  end;

  FFileSize := FileSize(F);

  FileMode := OldFileMode;
end;

destructor TG3File.Destroy;
begin
  Close();
  inherited;
end;

function TG3File.Read(var Buffer; const Count: Cardinal): Cardinal;
begin
  BlockRead(F, Buffer, Count, Result);
  if Result > 0 then FPosition := FPosition + Result;
end;

function TG3File.Write(const Buffer; const Count: Cardinal): Cardinal;
begin
  BlockWrite(F, Buffer, Count, Result);
  if Result > 0 then FPosition := FPosition + Result;
  FFileSize := FPosition;
end;

procedure TG3File.Close;
begin
  if FClosed then Exit;
{$I-}
  CloseFile(F);
  FClosed := IOResult = 0;
end;

function TG3File.Seek(const NewPos: Int64): Boolean;
begin
{$I-}
  System.Seek(F, NewPos);
  Result := IOResult = 0;
  if Result then FPosition := NewPos;
end;

{ EIOError }

constructor EIOError.Create(ACode: Integer; AMsg: string);
begin
  inherited Create(AMsg);
  Code := ACode;
end;

{ TFileInputStream }

function TFileInputStream.GetSize(): Int64;
begin
  Result := FFile.FFileSize;
end;

constructor TFileInputStream.Create(const AFileName: string);
begin
  FFile := TG3File.Create(AFileName, fuRead);
end;

destructor TFileInputStream.Destroy;
begin
  FreeAndNil(FFile);
  inherited;
end;

procedure TFileInputStream.Close;
begin
  if Assigned(FFile) then FFile.Close();
end;

function TFileInputStream.Read(var Buffer; const Count: Cardinal): Cardinal;
begin
  Result := FFile.Read(Buffer, Count);
end;

{ TFileOutputStream }

constructor TFileOutputStream.Create(const AFileName: string; const ShareMode: TFileShare);
begin
  FFile := TG3File.Create(AFileName, fuWrite, ShareMode);
end;

destructor TFileOutputStream.Destroy;
begin
  FreeAndNil(FFile);
  inherited;
end;

procedure TFileOutputStream.Close;
begin
  if Assigned(FFile) then FFile.Close();
end;

function TFileOutputStream.Write(const Buffer; const Count: Cardinal): Cardinal;
begin
  Result := FFile.Write(Buffer, Count);
end;

end.
