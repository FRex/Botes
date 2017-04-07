unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, ExtCtrls;

type

  { TForm1 }


  TForm1 = class(TForm)
    AwesomeBar: TEdit;
    MainMenu1: TMainMenu;
    Suggestions: TMemo;
    MenuItemFileSave: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    TextEditor: TSynEdit;
    procedure AwesomeBarChange(Sender: TObject);
    procedure AwesomeBarEnter(Sender: TObject);
    procedure AwesomeBarExit(Sender: TObject);
    procedure AwesomeBarKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemFileSaveClick(Sender: TObject);
    procedure TextEditorKeyPress(Sender: TObject; var Key: char);
  private
    FAllLines, FDiscardedLines, FAllTags: TStringList;
    procedure CollectAllTags;
    procedure FilterTextByAwesomeBar;
    procedure UpdateSuggestions;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function IsTagLine(const line: string): boolean;
begin
  Result := line.StartsWith('#');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FAllLines := TStringList.Create;
  FDiscardedLines := TStringList.Create;
  FAllTags := TStringList.Create;

  try
    FAllLines.LoadFromFile('allnotes.txt');
  except
    on EFOpenError do
      MessageDlg('File not found', 'Failed to open file allnotes.txt.',
        mtError, [mbOK], 0);
  end;

  FAllTags.Sorted := True;
  FAllTags.Duplicates := dupIgnore;
  CollectAllTags;

  TextEditor.Lines.Assign(FAllLines);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FAllLines.Free;
  FDiscardedLines.Free;
  FAllTags.Free;
end;

procedure TForm1.MenuItemFileSaveClick(Sender: TObject);
begin
  FAllLines.Assign(FDiscardedLines);
  FAllLines.AddStrings(TextEditor.Lines);
  FAllLines.SaveToFile('allnotes.txt');
  CollectAllTags;
  TextEditor.MarkTextAsSaved;
end;

procedure TForm1.TextEditorKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 27 then
    AwesomeBar.SetFocus;
end;

procedure TForm1.AwesomeBarChange(Sender: TObject);
begin
  UpdateSuggestions;
  if AwesomeBar.Text = '' then
    TextEditor.Lines.Assign(FAllLines)
  else
    FilterTextByAwesomeBar;
end;

procedure TForm1.AwesomeBarEnter(Sender: TObject);
begin
  Suggestions.Visible := True;
  UpdateSuggestions;
end;

procedure TForm1.AwesomeBarExit(Sender: TObject);
begin
  Suggestions.Visible := False;
end;

procedure TForm1.AwesomeBarKeyPress(Sender: TObject; var Key: char);
begin
  if (Ord(Key) = 27) or (Ord(Key) = 13) then
    TextEditor.SetFocus;
end;

procedure TForm1.CollectAllTags;
var
  str: string;
  split: TStringList;
begin
  FAllTags.Clear;
  split := TStringList.Create;
  try
    split.Delimiter := ' ';
    for str in FAllLines do
      if IsTagLine(str) then
      begin
        split.Clear;
        split.DelimitedText := str.Replace('#', '');
        FAllTags.AddStrings(split);
      end;
  finally
    split.Free;
  end;
end;

procedure TForm1.FilterTextByAwesomeBar;
var
  str: string;
  inseg: boolean;
  tmp: TStringList;
begin
  try
    tmp := TStringList.Create;
    TextEditor.Lines.Clear;
    FDiscardedLines.Clear;
    inseg := False;
    for str in FAllLines do
    begin
      if IsTagLine(str) then
        inseg := (str + ' ').Contains('#' + AwesomeBar.Text + ' ');

      if inseg then
        tmp.Add(str)
      else
        FDiscardedLines.Add(str);

    end; //for each line
    TextEditor.Lines.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

procedure TForm1.UpdateSuggestions;
var
  str: string;
  tmp: TStringList;
  bmap: TBitmap;
  cy: integer;
begin
  try
    bmap := TBitmap.Create;
    tmp := TStringList.Create;
    for str in FAllTags do
      if str.StartsWith(AwesomeBar.Text) then
        tmp.Add(str);

    if tmp.Count = 0 then
      tmp.Assign(FAllTags);

    tmp.Delimiter := #10;
    Suggestions.Text := tmp.DelimitedText;
    bmap.Canvas.Font := Suggestions.Font;
    cy := bmap.Canvas.TextHeight(Suggestions.Lines[0]);
    Suggestions.Height := Suggestions.Lines.Count * cy + cy div 2;
  finally
    tmp.Free;
    bmap.Free;
  end;
end;

end.
