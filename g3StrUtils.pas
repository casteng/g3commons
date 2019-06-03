{
@abstract(String utilities)

The unit contains string and text specific routines.

The contents of this file are subject to the license defined in the file licence.txt.
}

unit g3StrUtils;
{$I g3config.inc}

interface

type
  TAnsiStringArray = array of AnsiString;
//  TUnicodeStringArray = array of UnicodeString;
  TStringArray = array of String;

  { Splits s around occurences of Delim. Res contains splitted strings. Returns number of parts.
    The parts in the array are in the order in which they occur in original string. }
  function Split(const Str, Delim: string; out Res: TStringArray; EmptyOK: Boolean): Integer; overload;
  // Returns True if Str starts with Prefix
  function StartsWith(const Str: string; const Prefix: string): Boolean;  overload;
{ Joins arrays of string into one string separating the strings by the given separator.
  First strings which are empty do not separated. E.g. JSA(['', 's1', 's2'], ',') => 's1,s2' }
  function JoinStrArray(Strings: TAnsiStringArray; Separator: AnsiString): AnsiString;
  {$IFDEF UNICODE}
  // Ansi string version of Split()
  function Split(const Str, Delim: AnsiString; out Res: TAnsiStringArray; EmptyOK: Boolean): Integer; overload;
  // Returns True if Str starts with Prefix
  function StartsWith(const Str: AnsiString; const Prefix: AnsiString): Boolean;  overload;
  {$ENDIF}


implementation

uses
  g3types;

function Split(const Str, Delim: string; out Res: TStringArray; EmptyOK: Boolean): Integer; overload;
var i: Integer; s: string;
begin
  Result := 1;
  s := Str;
  Res := nil;
  while s <> '' do begin
    i := Pos(Delim, s);
    if i > 0 then begin
      if (i > 1) or EmptyOK then begin
        Inc(Result);
        if Length(Res) < Result then SetLength(Res, Result);
        Res[Result-2] := Copy(s, 1, i-1);
      end;
      s := Copy(s, i + Length(Delim), Length(s));
    end else Break;
  end;

  if Length(Res) < Result then SetLength(Res, Result);
  if EmptyOK or (s <> '') then
    Res[Result-1] := s
  else
    Dec(Result);
  if Length(Res) <> Result then SetLength(Res, Result);
end;

function StartsWith(const Str: string; const Prefix: string): Boolean;  overload;
begin
  Result := Copy(Str, STRING_INDEX_BASE, Length(Prefix)) = Prefix;
end;

function JoinStrArray(Strings: TAnsiStringArray; Separator: AnsiString): AnsiString;
var
  i: Integer;
  SepLen: Integer;
  Len, Index: Integer;
begin
  Len := 0;
  SepLen := Length(Separator);
  for i := 0 to High(Strings) do
  begin
    if Len > 0 then
      Inc(Len, SepLen);
    Inc(Len, Length(Strings[i]));
  end;
  SetLength(Result, Len);
  Index := STRING_INDEX_BASE;
  for i := 0 to High(Strings) do
  begin
    if (Index > STRING_INDEX_BASE) and (SepLen > 0) then
    begin
      Move(Separator[STRING_INDEX_BASE], Result[Index], SepLen);
      Inc(Index, SepLen);
    end;
    Len := Length(Strings[i]);
    if Len > 0 then
    begin
      Move(Strings[i][STRING_INDEX_BASE], Result[Index], Len);
      Inc(Index, Len);
    end;
  end;
  Assert(Index - STRING_INDEX_BASE = Length(Result));
end;

{$IFDEF UNICODE}
function Split(const Str, Delim: AnsiString; out Res: TAnsiStringArray; EmptyOK: Boolean): Integer; overload;
var i: Integer; s: AnsiString;
begin
  Result := 1;
  s := Str;
  Res := nil;
  while s <> '' do begin
    i := Pos(Delim, s);
    if i > 0 then begin
      if (i > 1) or EmptyOK then begin
        Inc(Result);
        if Length(Res) < Result then SetLength(Res, Result);
        Res[Result-2] := Copy(s, 1, i-1);
      end;
      s := Copy(s, i + Length(Delim), Length(s));
    end else Break;
  end;

  if Length(Res) < Result then SetLength(Res, Result);
  if EmptyOK or (s <> '') then
    Res[Result-1] := s
  else
    Dec(Result);
  if Length(Res) <> Result then SetLength(Res, Result);
end;

function StartsWith(const Str: AnsiString; const Prefix: AnsiString): Boolean;  overload;
begin
  Result := Copy(Str, STRING_INDEX_BASE, Length(Prefix)) = Prefix;
end;

{$ENDIF}

end.
