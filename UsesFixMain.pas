unit UsesFixMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TUsesFixMainForm = class(TForm)
    LabeledEditDprFileName: TLabeledEdit;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    ButtonFix: TButton;
    LabelSearchPath: TLabel;
    MemoSearchPath: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonFixClick(Sender: TObject);
  private
    FCurrentDir: string;
    procedure ReadConfiguration;
    procedure SaveConfiguration;
  public
    { Public declarations }
  end;

var
  UsesFixMainForm: TUsesFixMainForm;

implementation

uses IniFiles, StrUtils, PathUtils, SrcUtils;
{$R *.dfm}

procedure TUsesFixMainForm.ButtonFixClick(Sender: TObject);
begin
{$IFDEF DEBUG}
  ShowMessage('DEBUG defined');
{$ENDIF}
{$IFDEF NDEBUG}
  ShowMessage('NDEBUG defined');
{$ENDIF}
Exit;
  RemoveComments('Test.pas');
  Exit;
  ShowMessage(RelativePathToAbsolute(FCurrentDir, MemoSearchPath.Lines[0]));
  ShowMessage(AbsolutePathToRelative(FCurrentDir, MemoSearchPath.Lines[1]));
end;

procedure TUsesFixMainForm.FormCreate(Sender: TObject);
begin
  FCurrentDir := ExcludeTrailingPathDelimiter(GetCurrentDir);
  ReadConfiguration;
end;

procedure TUsesFixMainForm.FormDestroy(Sender: TObject);
begin
  SaveConfiguration;
end;

procedure TUsesFixMainForm.ReadConfiguration;
var
  IniFile: TIniFile;
  PathList: TStringList;
begin
  IniFile := TIniFile.Create(FCurrentDir + '\UsesFix.ini');
  try
    LabeledEditDprFileName.Text := IniFile.ReadString('Default', 'DprFileName', '');
    PathList := TStringList.Create;
    try
      PathList.CommaText := IniFile.ReadString('Default', 'SearchPath', '');
      MemoSearchPath.Lines.Clear;
      MemoSearchPath.Lines.AddStrings(PathList);
    finally
      FreeAndNil(PathList);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TUsesFixMainForm.SaveConfiguration;
var
  IniFile: TIniFile;
  PathList: TStringList;
begin
  IniFile := TIniFile.Create(FCurrentDir + '\UsesFix.ini');
  try
    IniFile.WriteString('Default', 'DprFileName', LabeledEditDprFileName.Text);
    PathList := TStringList.Create;
    try
      PathList.AddStrings(MemoSearchPath.Lines);
      IniFile.WriteString('Default', 'SearchPath', PathList.CommaText);
    finally
      FreeAndNil(PathList);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
