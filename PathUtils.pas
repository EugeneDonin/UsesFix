unit PathUtils;

interface
uses SysUtils, Classes, StrUtils;

function AbsolutePathToRelative(const CurrentDir, Path: string): string;
function RelativePathToAbsolute(const CurrentDir, Path: string): string;

implementation

procedure SplitPath(const Path: string; out Parts: TStringList);
begin
  Parts.Clear;
  Parts.StrictDelimiter := True;
  Parts.Delimiter := '\';
  Parts.DelimitedText := Path;
end;

function MergePath(Parts: TStringList; PatDelimiter: Char = '\'): string;
begin
  Parts.StrictDelimiter := True;
  Parts.Delimiter := PatDelimiter;
  Result := Parts.DelimitedText;
end;

(*------------------------------------------------------------------------------
  Converts Path to relative path according to CurrentDir
  For ezmple, for

  CurrentDir = 'C:\Temp\Dir1\Dir2'
  Path = 'C:\Temp\Dir3\Dir4'

  result will be

  '..\..\Dir3\Dir4'

  @param CurrentDir
  @param Path
  @return relative path
------------------------------------------------------------------------------*)
function AbsolutePathToRelative(const CurrentDir, Path: string): string;
var
  CurrentDirParts: TStringList;
  PathParts: TStringList;
begin
  Result := Path;
  if Length(Path) = 0 then
    Exit;

  CurrentDirParts := TStringList.Create;
  try
    SplitPath(CurrentDir, CurrentDirParts);
    PathParts := TStringList.Create;
    try
      SplitPath(Path, PathParts);
      if not ((PathParts.Count > 0) and (CurrentDirParts.Count > 0) and
               AnsiSameText(PathParts[0], CurrentDirParts[0])) then
        Exit;

      while ((PathParts.Count > 0) and (CurrentDirParts.Count > 0) and
               AnsiSameText(PathParts[0], CurrentDirParts[0])) do begin
        PathParts.Delete(0);
        CurrentDirParts.Delete(0);
      end;
      Result := DupeString('..\', CurrentDirParts.Count) + MergePath(PathParts);
    finally
      FreeAndNil(PathParts);
    end;
  finally
    FreeAndNil(CurrentDirParts);
  end;
end;

(*------------------------------------------------------------------------------
  Converts relative Path to absolute according to CurrentDir
  For ezmple, for

  CurrentDir = 'C:\Temp\Dir1\Dir2'
  Path = '..\..\Dir3\Dir4'

  result will be

  'C:\Temp\Dir3\Dir4'

  @param CurrentDir
  @param Path
  @return absolute path
------------------------------------------------------------------------------*)
function RelativePathToAbsolute(const CurrentDir, Path: string): string;
var
  CurrentDirParts: TStringList;
  PathParts: TStringList;
begin
  Result := Path;

  if Length(Path) = 0 then
    Exit;

  CurrentDirParts := TStringList.Create;
  try
    SplitPath(CurrentDir, CurrentDirParts);
    if Path[1] = '\' then begin
      Result := CurrentDirParts[0] + Path;
    end
    else if (Length(Path) > 3) and (Copy(Path, 1, 3) = '..\') then begin
      PathParts := TStringList.Create;
      try
      SplitPath(Path, PathParts);
        while (PathParts.Count > 0) and (CurrentDirParts.Count > 0) and
              (PathParts[0] = '..') do begin
          PathParts.Delete(0);
          CurrentDirParts.Delete(CurrentDirParts.Count - 1);
        end;
        Result := MergePath(CurrentDirParts) + '\' +  MergePath(PathParts);
      finally
        FreeAndNil(PathParts);
      end;
    end;
  finally
    FreeAndNil(CurrentDirParts);
  end;
end;



end.
