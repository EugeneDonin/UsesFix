unit SrcUtils;

interface
uses SysUtils, Classes;

function RemoveComments(Lines: TStrings): Boolean; overload;
function RemoveComments(const FileName: string): Boolean; overload;

type
  MultiLineComment = record
    Opened, Closed: string;
  end;
const
  LineComments: array[0..0] of string =
    ('//');
  MultiLineComments: array[0..1] of MultiLineComment =
    (
      (Opened: '{'; Closed: '}'),
      (Opened: '(*'; Closed: '*)')
    );

implementation

function RemoveLineComment(var Line: string): Boolean;
var
  CommentPos, QuotePos: Integer;
  LineComment: string;
begin
  Result := False;
  for LineComment in LineComments do begin
    CommentPos := Pos(LineComment, Line);
    if CommentPos > 0 then begin
      QuotePos := Pos('''', Line);
      if (QuotePos > 0) and (QuotePos < CommentPos) then begin
      { TODO : Quotes }
      end
      else begin
        Result := True;
        Line := Copy(Line, 1, CommentPos - 1);
      end;
      Break;
    end;
  end;
end;

function RemoveComments(Lines: TStrings): Boolean;
var
  i: Integer;
  Line: string;
begin
  Result := False;
  i:=0;
  while i < Lines.Count do begin
    Line := Lines[i];
    if RemoveLineComment(Line) then begin
      Lines[i] := Line;
      Result := True;
    end;
    Inc(i);
  end;
end;

function RemoveComments(const FileName: string): Boolean; overload;
var
  Lines: TStrings;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    Result := RemoveComments(Lines);
    Lines.SaveToFile(FileName);
  finally
    FreeAndNil(Lines);
  end;
end;

end.
