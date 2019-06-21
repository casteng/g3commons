{
@abstract(Logger)
The unit contains main logger interface and several default appenders

@author(George Bakhtadze (avagames@gmail.com))
}

unit g3log;
{$I g3config.inc}

interface

uses
  g3types;

type
  // Log level prefix string type
  TLogPrefix = string;
  // Log level class
  TLogLevel = (llVerbose, llDebug, llInfo, llWarning, llError, llFatalError);
  // Tag used to filter log messages on subsystem basis
  TLogTag = AnsiString;

type
  // Method pointer which formats
  TLogFormatDelegate = function(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel): string of object;

  // Log appender metaclass
  CAppender = class of TAppender;
  // Abstract log appender
  TAppender = class(TObject)
  public
    // Format pattern for log timestamp. If empty no timestamps will be appended.
    TimeFormat: string;
    { Default appender constructor.
      Creates the appender of the specified log levels, initializes TimeFormat and Formatter to default values
      and registers the new appender in log system. }
    constructor Create(Level: TLogLevel);
  protected
    // Should be overridden to actually append log
    procedure AppendLog(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel); virtual; abstract;
  private
    FFormatter: TLogFormatDelegate;
    FLogLevel: TLogLevel;
    function GetPreparedStr(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel): string;
    procedure SetLevel(Level: TLogLevel);
  public
    // Set of levels which to include in the log
    property LogLevel: TLogLevel read FLogLevel write SetLevel;
    // String formatter delegate. It's recommended for descendant classes to use it.
    property Formatter: TLogFormatDelegate read FFormatter write FFormatter;
  end;

  { Logging interface which may be used to set tag within a scope and automatically remove it.
    Intended to be created with WithLog() function
  }
  ILog = {$IFNDEF NOLOGGING} interface {$ELSE} class public {$ENDIF}     // To avoid unnecessary stack frames w/o logging
    // Set current tag
    procedure SetTag(const Tag: TLogTag);
    // Returns True if logging with current tag and the specified level is enabled
    function IsEnabled(level: TLogLevel): Boolean;
    // Calls all registered appenders to log the verbose message
    procedure Verbose(const Str: string); overload;
    // Calls all registered appenders to log the debug message
    procedure Debug(const Str: string); overload;
    // Calls all registered appenders to log the info
    procedure Info(const Str: string); overload;
    // Calls all registered appenders to log the warning
    procedure Warning(const Str: string); overload;
    // Calls all registered appenders to log the error
    procedure Error(const Str: string); overload;
    // Calls all registered appenders to log the fatal error
    procedure Fatal(const Str: string); overload;
    // Calls all registered appenders to log the formatted verbose message
    procedure Verbose(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted debug message
    procedure Debug(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted info
    procedure Info(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted warning
    procedure Warning(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted error
    procedure Error(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted fatal error
    procedure Fatal(const Str: string; const Args: array of const); overload;
  end;

  // Filters by tag and calls all registered appenders to log the string with source location information
  procedure DoLog(const Tag: TLogTag; const Str: string; const CodeLoc: TCodeLocation; Level: TLogLevel); overload;
  // Filters by tag and calls all registered appenders to log the string with source location information
  procedure DoLog(const Tag: TLogTag; const Str: string; Level: TLogLevel; const Args: array of const); overload;
  // Calls all registered appenders to log the verbose message
  procedure Verbose(const Str: string); overload;
  // Calls all registered appenders to log the debug message
  procedure Debug(const Str: string); overload;
  // Calls all registered appenders to log the info
  procedure Info(const Str: string); overload;
  // Calls all registered appenders to log the warning
  procedure Warning(const Str: string); overload;
  // Calls all registered appenders to log the error
  procedure Error(const Str: string); overload;
  // Calls all registered appenders to log the fatal error
  procedure Fatal(const Str: string); overload;
  // Calls all registered appenders to log the formatted verbose message
  procedure Verbose(const Str: string; const Args: array of const); overload;
  // Calls all registered appenders to log the formatted debug message
  procedure Debug(const Str: string; const Args: array of const); overload;
  // Calls all registered appenders to log the formatted info
  procedure Info(const Str: string; const Args: array of const); overload;
  // Calls all registered appenders to log the formatted warning
  procedure Warning(const Str: string; const Args: array of const); overload;
  // Calls all registered appenders to log the formatted error
  procedure Error(const Str: string; const Args: array of const); overload;
  // Calls all registered appenders to log the formatted fatal error
  procedure Fatal(const Str: string; const Args: array of const); overload;

  // Set log tag for current thread
  procedure SetTag(const Tag: TLogTag);

  { Creates an instance of TLog and returns ILog reference.
    Usage:
    var
      Log: ILog;
    begin
      Log := WithTag('someTag');
      Log.Info('All calls to logging routines till the end of the scope from current thread will be with "someTag" tag.');
      g3Log.Debug('Direct calls too');
      ...
    end;
  }
  function WithTag(const Tag: TLogTag): ILog; overload;

  // Formatted version of WithTag()
  function WithTag(const Tag: string; const Args: array of const): ILog; overload;

  // Logging with tags starting with TagStart will be sent only for level greater than MinLevel
  procedure SetFiltering(const TagStart: TLogTag; MinLevel: TLogLevel);
  // Remove all log filters
  procedure ResetFiltering();

  // Prints to log the specified stack trace which can be obtained by some of BaseDebug unit routines
  procedure LogStackTrace(const StackTrace: TBaseStackTrace);

  { A special function-argument. Should be called ONLY as Assert() argument.
    Allows to log source file name and line number at calling location.
    Doesn't require any debug information to be included in binary module.
    The only requirement is inclusion of assertions code.
    Tested in Delphi 7+ and FPC 2.4.2+.

    Suggested usage:

    Assert(_Log(lkInfo), 'Log message');

    This call will log the message with source filename and Line number
    Always returns False. }
  function _Log(Level: TLogLevel): Boolean; overload;
  function _Log(): Boolean; overload;

  // Adds an appender to list of registered appenders. All registered appenders will be destroyed on shutdown.
  procedure AddAppender(Appender: TAppender);
  // Removes an appender from list of registered appenders. Doesn't destroy the appender.
  procedure RemoveAppender(Appender: TAppender);
  // Returns a registered appender of the specified class
  function FindAppender(AppenderClass: CAppender): TAppender;

  { Initializes default appenders:
    TConsoleAppender if current application is a console application
    TWinDebugAppender for Delphi applications running under debugger in Windows OS
  }
  procedure AddDefaultAppenders();

  // Removes all appenders added by AddDefaultAppenders() if any
  procedure RemoveDefaultAppenders();

type
  // Appends log messages to a system console. Application should be a console application.
  TSysConsoleAppender = class(TAppender)
  protected
    // Prints the log string to a system console
    procedure AppendLog(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel); override;
  end;

  // Use OutputsDebugString() for loging. Works only in Windows.
  TWinDebugAppender = class(TAppender)
  protected
    // Prints the log string with OutputsDebugString()
    procedure AppendLog(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel); override;
  end;

  // Appends log messages to a file.
  TFileAppender = class(TAppender)
  public
    // Creates the appender with the specified file name and log levels
    constructor Create(const Filename: string; ALevel: TLogLevel);
  protected
    // Appends file with the log string
    procedure AppendLog(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel); override;
  private
    LogFile: Text;
  end;


implementation

uses
  {$IFDEF MULTITHREADLOG}
    g3Concurrent,
    {$IFDEF UNIX}
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WINDOWS}{$IFDEF DELPHI}
    {$IFDEF NAMESPACED_UNITS} Winapi.Windows, {$ELSE} Windows, {$ENDIF}
  {$ENDIF}{$ENDIF}
  SysUtils;

type
  TLogFilter = record
    TagStart: TLogTag;
    MinLevel: TLogLevel;
  end;
  TlogFilters = array [0..$FFFF] of TLogFilter;
  PLogFilters = ^TlogFilters;

const
  // Default level prefixes
  Prefix: array[TLogLevel] of string = (' (v) ', ' (d) ', ' (i) ', '(W) ', '(E) ', '(!) ');
  // Level names
  LEVEL_NAME: array[TLogLevel] of string = ('verbose', 'debug', 'info', 'warning', 'error', 'fatal');
  LOG_TAG = TLogTag('logger');

{$IFNDEF NOLOGGING}
{$IFDEF MULTITHREADLOG}threadvar{$ELSE}var{$ENDIF}
  CurrentTag: TLogTag;
var
  Filter: PLogFilters;
  FilterSize, FilterCapacity: Integer;
{$ENDIF}

type
  TLog = class(TLiteInterfacedObject, ILog)
  private
    OldTag: TLogTag;
  public
    constructor Create(const Tag: TLogTag);
    destructor Destroy; override;

    procedure SetTag(const Tag: TLogTag);
    function IsEnabled(Level: TLogLevel): Boolean;

    // Calls all registered appenders to log the verbose message
    procedure Verbose(const Str: string); overload;
    // Calls all registered appenders to log the debug message
    procedure Debug(const Str: string); overload;
    // Calls all registered appenders to log the info
    procedure Info(const Str: string); overload;
    // Calls all registered appenders to log the warning
    procedure Warning(const Str: string); overload;
    // Calls all registered appenders to log the error
    procedure Error(const Str: string); overload;
    // Calls all registered appenders to log the fatal error
    procedure Fatal(const Str: string); overload;
    // Calls all registered appenders to log the formatted verbose message
    procedure Verbose(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted debug message
    procedure Debug(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted info
    procedure Info(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted warning
    procedure Warning(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted error
    procedure Error(const Str: string; const Args: array of const); overload;
    // Calls all registered appenders to log the formatted fatal error
    procedure Fatal(const Str: string; const Args: array of const); overload;
  end;

{ TAppender }

constructor TAppender.Create(Level: TLogLevel);
begin
  TimeFormat := 'yyyy"-"mm"-"dd hh":"nn":"ss"."zzz';
  LogLevel  := Level;
  FFormatter := GetPreparedStr;
  AddAppender(Self);
  DoLog(LOG_TAG, 'Appender of class %s initialized', llInfo, [ClassName]);
end;

function TAppender.GetPreparedStr(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel): string;
begin
  Result := FormatDateTime(TimeFormat, Time) + Prefix[Level] + Tag + '|' + Str;
  if (CodeLoc <> nil) then
    Result := Concat(Result, ' --- ', CodeLocToStr(CodeLoc^));
end;

procedure TAppender.SetLevel(Level: TLogLevel);
begin
  FLogLevel := Level;
end;

{ Logger }

var
  FAppenders: array of TAppender;
  {$IFDEF MULTITHREADLOG}
    Mutex: TCEMutex;
  {$ENDIF}

procedure Lock(); {$I inline.inc}
begin
  {$IFDEF MULTITHREADLOG}
    MutexEnter(Mutex);
  {$ENDIF}
end;

procedure UnLock(); {$I inline.inc}
begin
  {$IFDEF MULTITHREADLOG}
    MutexLeave(Mutex);
  {$ENDIF}
end;

const EmptyCodeLoc: TCodeLocation = (Address: nil; SourceFilename: ''; UnitName: ''; ProcedureName: ''; LineNumber: -1);

function PassFilter(const Tag: TLogTag; Level: TLogLevel): Boolean;                // TODO: optimize
var
  i: Integer;
  TagStart, TagUpper: TLogTag;
begin
  Result := false;
  TagUpper := UpperCase(Tag);
  for i := FilterSize - 1 downto 0 do
  begin
    TagStart := Filter^[i].TagStart;
    if Copy(TagUpper, STRING_INDEX_BASE, Length(TagStart)) = TagStart then
      if Level < Filter^[i].MinLevel then
        Exit;
  end;
  Result := true;
end;

procedure DoLog(const Tag: TLogTag; const Str: string; const CodeLoc: TCodeLocation; Level: TLogLevel); overload;
{$IFNDEF NOLOGGING} var i: Integer; Time: TDateTime; SrcLocPtr: PCodeLocation; {$ENDIF}
begin
  {$IFNDEF NOLOGGING}
  Lock();
  try
    if not PassFilter(Tag, Level) then
      Exit;
    if CodeLoc.LineNumber = -1 then
      SrcLocPtr := nil
    else
      SrcLocPtr := @CodeLoc;

    Time := Now;

    for i := 0 to High(FAppenders) do
      if Level >= FAppenders[i].LogLevel then
        FAppenders[i].AppendLog(Time, Tag, Str, SrcLocPtr, Level);
  finally
    UnLock();
  end;
  {$ENDIF}
end;

procedure DoLog(const Tag: TLogTag; const Str: string; Level: TLogLevel; const Args: array of const); overload;
begin
  {$IFNDEF NOLOGGING} DoLog(Tag, Format(Str, Args), EmptyCodeLoc, Level);{$ENDIF}
end;

procedure Verbose(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llVerbose); {$ENDIF}
end;

procedure Debug(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llDebug); {$ENDIF}
end;

procedure Info(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llInfo); {$ENDIF}
end;

procedure Warning(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llWarning); {$ENDIF}
end;

procedure Error(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llError); {$ENDIF}
end;

procedure Fatal(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llFatalError); {$ENDIF}
end;

procedure Verbose(const Str: string; const Args: array of const); overload;
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llVerbose);{$ENDIF}
end;

procedure Debug(const Str: string; const Args: array of const); overload;
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llDebug);{$ENDIF}
end;

procedure Info(const Str: string; const Args: array of const); overload;
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llInfo);{$ENDIF}
end;

procedure Warning(const Str: string; const Args: array of const); overload;
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llWarning);{$ENDIF}
end;

procedure Error(const Str: string; const Args: array of const); overload;
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llError);{$ENDIF}
end;

procedure Fatal(const Str: string; const Args: array of const); overload;
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llFatalError);{$ENDIF}
end;

procedure SetTag(const Tag: TLogTag);
begin
  {$IFNDEF NOLOGGING} CurrentTag := Tag;{$ENDIF}
end;

function WithTag(const Tag: TLogTag): ILog; overload;
begin
  Result := TLog.Create(Tag);
end;

function WithTag(const Tag: string; const Args: array of const): ILog; overload;
begin
  Result := TLog.Create(TLogTag(Format(Tag, Args)));
end;

function GetFilterIndex(const TagStartUpper: TLogTag): Integer;
begin
  Result := FilterSize-1;
  while (Result >= 0) and (Filter^[Result].TagStart <> TagStartUpper) do
    Dec(Result);
end;

procedure SetFiltering(const TagStart: TLogTag; MinLevel: TLogLevel);
var
  TagStartUpper: TLogTag;
  Index: Integer;
begin
  Lock();
  try
    TagStartUpper := UpperCase(TagStart);
    Index := GetFilterIndex(TagStartUpper);
    if Index < 0 then
    begin
      Index := FilterSize;
      Inc(FilterSize);
      if FilterCapacity < FilterSize then
      begin
        FilterCapacity := FilterSize + 4;
        ReallocMem(Filter, SizeOf(TLogFilter) * FilterCapacity);
      end;
      Filter^[FilterSize - 1].TagStart := TagStartUpper;
      Filter^[FilterSize - 1].MinLevel := MinLevel;
      DoLog(LOG_TAG, 'Added filter for tag "%s" with level %s. Filter size: %d', llInfo, [TagStart, LEVEL_NAME[MinLevel], FilterSize]);
    end else if MinLevel = llVerbose then begin            // Remove entry
      Filter^[Index] := Filter^[FilterSize - 1];
      Dec(FilterSize);
      DoLog(LOG_TAG, 'Removed filter for tag "%s". Filter size: %d', llInfo, [TagStart, FilterSize]);
    end else begin
      Filter^[FilterSize - 1].MinLevel := MinLevel;
      DoLog(LOG_TAG, 'Replaced filter for tag "%s" with level %s. Filter size: %d', llInfo, [TagStart, LEVEL_NAME[MinLevel], FilterSize]);
    end;
  finally
    UnLock();
  end;
end;

procedure ResetFiltering();
begin
  Lock();
  try
    FreeAndNil(Filter);
    FilterSize := 0;
    FilterCapacity := 0;
  finally
    UnLock();
  end;
end;

procedure LogStackTrace(const StackTrace: TBaseStackTrace);
var i: Integer;
begin
  for i := 0 to High(StackTrace) do
    Info(' --- ' + IntToStr(i) + '. ' + CodeLocToStr(StackTrace[i]));
end;

var
  AssertLogLevel: TLogLevel;

{$IFDEF FPC}
  procedure LogAssert(const Message, Filename: ShortString; LineNumber: LongInt; ErrorAddr: Pointer);
{$ELSE}
  procedure LogAssert(const Message, Filename: string; LineNumber: Integer; ErrorAddr: Pointer);
{$ENDIF}
var CodeLocation: TCodeLocation;
begin
  AssertRestore();

  CodeLocation := GetCodeLoc(Filename, '', '', LineNumber, ErrorAddr);

  DoLog(CurrentTag, Message, CodeLocation, AssertLogLevel);
end;

function _Log(Level: TLogLevel): Boolean; overload;
begin
  if AssertHook(@LogAssert) then begin
    AssertLogLevel := Level;
    Result := False;
  end else
    Result := True;  // Prevent assertion error if hook failed
end;

function _Log(): Boolean; overload;
begin
  Result := _Log(LLInfo);
end;

// Returns index of the appender or -1 if not found
function IndexOfAppender(Appender: TAppender): Integer;
begin
  Result := High(FAppenders);
  while (Result >= 0) and (FAppenders[Result] <> Appender) do Dec(Result);
end;

procedure AddAppender(Appender: TAppender);
begin
  if not Assigned(Appender) then Exit;
  if IndexOfAppender(Appender) >= 0 then begin
    DoLog(LOG_TAG, 'Duplicate appender of class "%s', llWarning, [Appender.ClassName]);
    Exit;
  end;
  Lock();
  try
    SetLength(FAppenders, Length(FAppenders)+1);
    // Set default formatter
    if @Appender.Formatter = nil then
      Appender.Formatter := Appender.GetPreparedStr;
    FAppenders[High(FAppenders)] := Appender;
  finally
    Unlock();
  end;
end;

procedure RemoveAppender(Appender: TAppender);
var i: Integer;
begin
  i := IndexOfAppender(Appender);
  // if found, replace it with last and resize array
  if i >= 0 then begin
    Lock();
    try
      FAppenders[i] := FAppenders[High(FAppenders)];
      SetLength(FAppenders, Length(FAppenders)-1);
    finally
      Unlock();
    end;
  end;
end;

function FindAppender(AppenderClass: CAppender): TAppender;
var i: Integer;
begin
  i := High(FAppenders);
  while (i >= 0) and (FAppenders[i].ClassType <> AppenderClass) do Dec(i);

  if i >= 0 then
    Result := FAppenders[i]
  else
    Result := nil;
end;

{$WARN SYMBOL_PLATFORM OFF}
procedure AddDefaultAppenders();
begin
  {$IFDEF WINDOWS}{$IFDEF DELPHI}
    if DebugHook > 0 then
      TWinDebugAppender.Create(llVerbose);
  {$ENDIF}{$ENDIF}

  if IsConsole then begin
    if Length(FAppenders) = 0 then
      TSysConsoleAppender.Create(llDebug)
    else
      TSysConsoleAppender.Create(llWarning)
  end;
end;

procedure RemoveDefaultAppenders();
begin
  if IsConsole then
    RemoveAppender(FindAppender(TSysConsoleAppender));

  {$IFDEF WINDOWS}{$IFDEF DELPHI}
    if DebugHook > 0 then
      RemoveAppender(FindAppender(TWinDebugAppender));
  {$ENDIF}{$ENDIF}
end;

procedure DestroyAppenders();
var i: Integer;
begin
  Lock();
  try
    for i := 0 to High(FAppenders) do begin
      FAppenders[i].Free;
    end;
    SetLength(FAppenders, 0);
  finally
    Unlock();
  end;
end;

{ TConsoleAppender }

procedure TSysConsoleAppender.AppendLog(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel);
begin
  if IsConsole then
  begin
    Writeln(Formatter(Time, Tag, Str, CodeLoc, Level));
    Flush(Output);
  end;
end;

{ TWinDebugAppender }

procedure TWinDebugAppender.AppendLog(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel);
begin
  {$IFDEF WINDOWS}{$IFDEF DELPHI}
    if DebugHook > 0 then
      {$IFDEF UNICODE}
        OutputDebugString(PWideChar(Formatter(Time, Tag, Str, CodeLoc, Level)));
      {$ELSE}
        OutputDebugStringA(PAnsiChar(Formatter(Time, Tag, Str, CodeLoc, Level)));
      {$ENDIF}
  {$ENDIF}{$ENDIF}
end;

{ TFileAppender }

constructor TFileAppender.Create(const Filename: string; ALevel: TLogLevel);
begin
  if (Pos(':', Filename) > 0) or (Pos('/', Filename) = 1) then
    AssignFile(LogFile, Filename)
  else
    AssignFile(LogFile, ExtractFilePath(ParamStr(0)) + Filename);

  {$I-}
  Rewrite(LogFile);
  CloseFile(LogFile);
  //if IOResult <> 0 then LogLevels := [];

  inherited Create(ALevel);
end;

procedure TFileAppender.AppendLog(const Time: TDateTime; const Tag: TLogTag; const Str: string; CodeLoc: PCodeLocation; Level: TLogLevel);
begin
  {$I-}
  Append(LogFile);
  if IOResult <> 0 then Exit;
  WriteLn(LogFile, Formatter(Time, Tag, Str, CodeLoc, Level));
  Flush(LogFile);
  CloseFile(LogFile);
end;

{ TLog }

constructor TLog.Create(const Tag: TLogTag);
begin
  OldTag := CurrentTag;
  CurrentTag := Tag;
end;

destructor TLog.Destroy;
begin
  CurrentTag := OldTag;
  inherited;
end;

procedure TLog.SetTag(const Tag: TLogTag);
begin
  CurrentTag := Tag;
end;

function TLog.IsEnabled(Level: TLogLevel): Boolean;
begin
  Result := true;
end;

procedure TLog.Verbose(const Str: string; const Args: array of const);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llVerbose); {$ENDIF}
end;

procedure TLog.Verbose(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llVerbose); {$ENDIF}
end;

procedure TLog.Debug(const Str: string; const Args: array of const);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llDebug); {$ENDIF}
end;

procedure TLog.Debug(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llDebug); {$ENDIF}
end;

procedure TLog.Info(const Str: string; const Args: array of const);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llInfo); {$ENDIF}
end;

procedure TLog.Info(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llInfo); {$ENDIF}
end;

procedure TLog.Warning(const Str: string; const Args: array of const);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llWarning); {$ENDIF}
end;

procedure TLog.Warning(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llWarning); {$ENDIF}
end;

procedure TLog.Error(const Str: string; const Args: array of const);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llError); {$ENDIF}
end;

procedure TLog.Error(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llError); {$ENDIF}
end;

procedure TLog.Fatal(const Str: string; const Args: array of const);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Format(Str, Args), EmptyCodeLoc, llFatalError); {$ENDIF}
end;

procedure TLog.Fatal(const Str: string);
begin
  {$IFNDEF NOLOGGING} DoLog(CurrentTag, Str, EmptyCodeLoc, llFatalError); {$ENDIF}
end;

initialization
  {$IFDEF MULTITHREADLOG}
    MutexCreate(Mutex);
  {$ENDIF}
  ResetFiltering();
  AddDefaultAppenders();
finalization
  DoLog(LOG_TAG, 'Log session shutdown', llInfo, []);
  DestroyAppenders();
  ResetFiltering();
  {$IFDEF MULTITHREADLOG}
    MutexDelete(Mutex)
  {$ENDIF}
end.

