{
@abstract(Concurrency utilities)

The unit contains concurrency and synchronization related routines. Platform specific.

@author(George Bakhtadze (avagames@gmail.com))
}

unit g3concurrent;
{$I g3config.inc}

interface

uses
  {$IFDEF WINDOWS}
    Windows,
    {$IFDEF NAMESPACED_UNITS} System.SyncObjs, {$ELSE} SyncObjs {$ENDIF}
  {$ENDIF}
  {$IFDEF UNIX}
    {!}cthreads
  {$ENDIF}
  ;

  {$IFDEF FPC}
type
  TMutex = TRTLCriticalSection;
  {$ENDIF}
  {$IFDEF DELPHI}
type
  TMutex = TCriticalSection;
  {$ENDIF}

  // Thread-safe increment of the value
  function AtomicIncrement(var Addend: LongInt): LongInt;
  // Thread-safe decrement of the value
  function AtomicDecrement(var Addend: LongInt): LongInt;
  // Store Source in Target and returns the old value of Target in a thread-safe way
  function AtomicExchange(var Target: LongInt; Source: LongInt): LongInt;
  // Thread-safe add and exchange of values
  function AtomicAddExchange(var Target: LongInt; Source: LongInt): LongInt;
  // Exchanges Target with NewValue if Target and Comparand are equal. It returns the old value of Target.
  function AtomicCompareExchange(var Target: LongInt; NewValue: LongInt; Comparand: LongInt): LongInt;

  procedure MutexCreate(out Mutex: TMutex);
  procedure MutexDelete(var Mutex: TMutex);
  procedure MutexEnter(var Mutex: TMutex);
  function MutexTryEnter(var Mutex: TMutex): Boolean;
  procedure MutexLeave(var Mutex: TMutex);

implementation

{$IFDEF FPC}

function AtomicIncrement(var Addend: LongInt): LongInt;
begin
  Result := InterlockedIncrement(Addend);
end;

function AtomicDecrement(var Addend: LongInt): LongInt;
begin
  Result := InterlockedDecrement(Addend);
end;

function AtomicExchange(var Target: LongInt; Source: LongInt): LongInt;
begin
  Result := InterLockedExchange(Target, Source);
end;

function AtomicAddExchange(var Target: LongInt; Source: LongInt): LongInt;
begin
  Result := InterLockedExchangeAdd(Target, Source);
end;

function AtomicCompareExchange(var Target: LongInt; NewValue: LongInt; Comparand: LongInt): LongInt;
begin
  Result := InterlockedCompareExchange(Target, NewValue, Comparand);
end;

procedure MutexCreate(out Mutex: TMutex);
begin
  InitCriticalSection(Mutex);
end;

procedure MutexDelete(var Mutex: TMutex);
begin
  DoneCriticalsection(Mutex);
end;

procedure MutexEnter(var Mutex: TMutex);
begin
  EnterCriticalsection(Mutex);
end;

function MutexTryEnter(var Mutex: TMutex): Boolean;
begin
  Result := TryEnterCriticalsection(Mutex) <> 0;
end;

procedure MutexLeave(var Mutex: TMutex);
begin
  LeaveCriticalsection(Mutex);
end;

{$ELSE}{$IFDEF WINDOWS}

function AtomicIncrement(var Addend: LongInt): LongInt;
begin
  Result := Windows.InterlockedIncrement(Addend);
end;

function AtomicDecrement(var Addend: LongInt): LongInt;
begin
  Result := Windows.InterlockedDecrement(Addend);
end;

function AtomicExchange(var Target: LongInt; Source: LongInt): LongInt;
begin
  Result := Windows.InterLockedExchange(Target, Source);
end;

function AtomicAddExchange(var Target: LongInt; Source: LongInt): LongInt;
begin
  Result := Windows.InterlockedExchangeAdd(Target, Source);
end;

function AtomicCompareExchange(var Target: LongInt; NewValue: LongInt; Comparand: LongInt): LongInt;
begin
  Result := Windows.InterlockedCompareExchange(Target, NewValue, Comparand);
end;

procedure MutexCreate(out Mutex: TMutex);
begin
  Mutex := TMutex.Create();
end;

procedure MutexDelete(var Mutex: TMutex);
begin
  if Assigned(Mutex) then
    Mutex.Free();
  Mutex := nil;
end;

procedure MutexEnter(var Mutex: TMutex);
begin
  Mutex.Enter();
end;

function MutexTryEnter(var Mutex: TMutex): Boolean;
begin
  Result := Mutex.TryEnter();
end;

procedure MutexLeave(var Mutex: TMutex);
begin
  Mutex.Leave();
end;

{$ENDIF}{$ENDIF}

end.
