unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, SynEditMarkupHighAll,
  SynEditMiscClasses, SynEditMarkupSpecialLine;

type

  { TForm1 }


  TForm1 = class(TForm)
    AwesomeBar: TEdit;
    TextQuery: TEdit;
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemFileOpenClick(Sender: TObject);
    procedure MenuItemFileSaveClick(Sender: TObject);
    procedure TextEditorKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TextEditorKeyPress(Sender: TObject; var Key: char);
    procedure TextEditorSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure TextQueryChange(Sender: TObject);
    procedure TextQueryKeyPress(Sender: TObject; var Key: char);
  private
    FAllLines, FDiscardedLines, FAllTags: TStringList;
    procedure CollectAllTags;
    procedure FilterTextByAwesomeBar;
    procedure UpdateSuggestions;
    procedure SaveNotes;
    function QueryUnsavedChanges: TModalResult;
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
var
  markup: TSynEditMarkupHighlightAllCaret;
begin
  FAllLines := TStringList.Create;
  FAllLines.LineBreak := #10;
  FDiscardedLines := TStringList.Create;
  FDiscardedLines.LineBreak := #10;
  FAllTags := TStringList.Create;
  FAllLines.LineBreak := #10;

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

  markup := TSynEditMarkupHighlightAllCaret(
    TextEditor.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  markup.MarkupInfo.FrameColor := clSilver;
  markup.MarkupInfo.Background := clGray;
  markup.WaitTime := 100;
  markup.Trim := True;
  markup.FullWord := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FAllLines.Free;
  FDiscardedLines.Free;
  FAllTags.Free;
end;

procedure TForm1.MenuItemFileOpenClick(Sender: TObject);
begin
  ShowMessage('NOT IMPLEMENTED YET');
end;

procedure TForm1.MenuItemFileSaveClick(Sender: TObject);
begin
  SaveNotes;
end;

procedure TForm1.TextEditorKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Ord(Key) = 70) and (Shift = [ssCtrl]) then
    TextQuery.SetFocus;
end;

procedure TForm1.TextEditorKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 27 then
    AwesomeBar.SetFocus;
end;

procedure TForm1.TextEditorSpecialLineMarkup(Sender: TObject;
  Line: integer; var Special: boolean; Markup: TSynSelectedColor);
begin
  if IsTagLine(TextEditor.Lines[Line - 1]) then
  begin
    Special := True;
    Markup.Background := clMoneyGreen;
    Markup.Foreground := clBlack;
  end;
end;

procedure TForm1.TextQueryChange(Sender: TObject);
var
  i, spot: integer;
begin
  //TODO: search from caret down at first
  for i := 0 to TextEditor.Lines.Count do
  begin
    spot := Pos(UpperCase(TextQuery.Text), UpperCase(TextEditor.Lines[i]));
    if spot <> 0 then
    begin
      TextEditor.CaretX := spot + Length(TextQuery.Text);
      TextEditor.CaretY := i + 1;
      TextEditor.BlockBegin := TPoint.Create(spot, i + 1);
      TextEditor.BlockEnd := TPoint.Create(spot + Length(TextQuery.Text), i + 1);
      Exit;
    end;
  end; //for line
end;

procedure TForm1.TextQueryKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 27 then
    TextEditor.SetFocus;

  if Ord(Key) = 13 then
  begin
    //TODO: find next
  end;
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
var
  ans: TModalResult;
begin
  if TextEditor.Modified then
  begin
    ans := QueryUnsavedChanges;
    if ans = mrCancel then
      Exit;

    if ans = mrYes then
      SaveNotes;
  end;

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

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  ans: TModalResult;
begin
  if not TextEditor.Modified then
  begin
    CanClose := True;
    Exit;
  end;

  ans := QueryUnsavedChanges;
  CanClose := (ans <> mrCancel);
  if ans = mrYes then
    SaveNotes;
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

procedure TForm1.SaveNotes;
begin
  FAllLines.Assign(FDiscardedLines);
  FAllLines.AddStrings(TextEditor.Lines);
  FAllLines.SaveToFile('allnotes.txt');
  CollectAllTags;
  TextEditor.MarkTextAsSaved;
  TextEditor.Modified := False;
end;

function TForm1.QueryUnsavedChanges: TModalResult;
const
  title = 'Unsaved changes';
  message = 'There are unsaved changes. Save them?';
begin
  Result := MessageDlg(title, message, mtConfirmation, mbYesNoCancel, 0);
end;

end.
