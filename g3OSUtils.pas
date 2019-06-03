{
@abstract(Operating system utilities)

The unit contains operating system specific routines. Platform specific.

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3OSUtils;
{$I g3ge.inc}

interface

  // Obtains mouse cursor position relative to screen and fills X and Y with the position
  procedure GetMouseCursorPos(out X, Y: Integer);
  // Sets mouse cursor position relative to screen
  procedure SetMouseCursorPos(X, Y: Integer);
  // Adjust mouse cursor visibility counter. The cursor will be visible if the counter >= 0. Initial value of the counter is zero.
  function AdjustMouseCursorVisibility(Show: Boolean): Integer;
  // Returns current time in milliseconds (precision is up to 1/20 of second)
  function GetCurrentMs(): Int64;
  // Returns value of high-frequency counter
  function GetPerformanceCounter(): Int64;
  // Returns frequency of high-frequency counter
  function GetPerformanceFrequency(): Int64;

implementation

{$IFDEF WINDOWS}
uses
    Windows;
{$ENDIF}
{$IFDEF UNIX}
uses
  unix;
{$ENDIF}

var
  PerformanceFrequency: Int64;

function GetPerformanceFrequency(): Int64;
begin
  Result := PerformanceFrequency;
end;

{$IFDEF UNIX}

procedure GetMouseCursorPos(out X, Y: Integer);
begin

end;

procedure SetMouseCursorPos(X, Y: Integer);
begin

end;

function AdjustMouseCursorVisibility(Show: Boolean): Integer;
begin

end;
{    tp: timespec;
begin
    clock_gettime(CLOCK_MONOTONIC, @tp);
    Result := (Int64(tp.tv_sec) * Int64(1000000)) + (tp.tv_nsec div 1000);}
function GetCurrentMs(): Int64;
var
  tm: TimeVal;
begin
  fpGetTimeOfDay(@tm, nil);
  Result := tm.tv_sec * Int64(1000) + tm.tv_usec div 1000;
end;

function GetPerformanceCounter: Int64;
begin
  Result := GetCurrentMs();
end;

procedure ObtainPerformanceFrequency;
begin
  PerformanceFrequency := 1000;
end;

{$ENDIF}

{$IFDEF WINDOWS}

function GetCurrentMs: Int64;
begin
  Result := Windows.GetTickCount();
end;

procedure GetMouseCursorPos(out X, Y: Integer);
var
  Pnt: TPoint;
begin
  Windows.GetCursorPos(Pnt);
  X := Pnt.X; Y := Pnt.Y;
end;

procedure SetMouseCursorPos(X, Y: Integer);
begin
  Windows.SetCursorPos(X, Y);
end;

function AdjustMouseCursorVisibility(Show: Boolean): Integer;
begin
  Result := Windows.ShowCursor(Show);
end;

procedure ObtainPerformanceFrequency;
begin
  if not Windows.QueryPerformanceFrequency(PerformanceFrequency) then
    PerformanceFrequency := 0;
end;

function GetPerformanceCounter: Int64;
begin
  Windows.QueryPerformanceCounter(Result);
end;

{$ENDIF}

initialization
//  ObtainPerformanceFrequency();
finalization
end.
