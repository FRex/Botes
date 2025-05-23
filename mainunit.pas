unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SynEdit, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, ComCtrls, ActnList, UniqueInstance, SynEditMiscClasses,
  SynEditKeyCmds, LCLType;

const
  UndoCloseTabHistoryMaxSize = 100;

type

  { TForm1 }


  TForm1 = class(TForm)
    UndoCloseTabAction: TAction;
    BeginFindAction: TAction;
    Suggestions: TListBox;
    StatusBar: TStatusBar;
    TextQueryStatusLabel: TLabel;
    TextQueryPanel: TPanel;
    SaveNotesAction: TAction;
    MoveTabForwardAction: TAction;
    MoveTabBackwardAction: TAction;
    PrevTabAction: TAction;
    NextTabAction: TAction;
    NewTabAction: TAction;
    CloseTabAction: TAction;
    MainTabsActionList: TActionList;
    AwesomeBar: TEdit;
    MainTabs: TTabControl;
    TextQuery: TEdit;
    TextEditor: TSynEdit;
    DeselectSuggestionsTimer: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure AwesomeBarChange(Sender: TObject);
    procedure AwesomeBarEnter(Sender: TObject);
    procedure AwesomeBarKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure BeginFindActionExecute(Sender: TObject);
    procedure CloseTabActionExecute(Sender: TObject);
    procedure DeselectSuggestionsTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MainTabsChange(Sender: TObject);
    procedure MainTabsChanging(Sender: TObject; var AllowChange: boolean);
    procedure MoveTabBackwardActionExecute(Sender: TObject);
    procedure MoveTabForwardActionExecute(Sender: TObject);
    procedure NewTabActionExecute(Sender: TObject);
    procedure NextTabActionExecute(Sender: TObject);
    procedure PrevTabActionExecute(Sender: TObject);
    procedure SaveNotesActionExecute(Sender: TObject);
    procedure StatusBarDblClick(Sender: TObject);
    procedure SuggestionsKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure TextEditorCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure TextEditorEnter(Sender: TObject);
    procedure TextEditorExit(Sender: TObject);
    procedure TextEditorKeyPress(Sender: TObject; var Key: char);
    procedure TextEditorSpecialLineMarkup(Sender: TObject; Line: integer;
      var Special: boolean; Markup: TSynSelectedColor);
    procedure TextQueryChange(Sender: TObject);
    procedure TextQueryEnter(Sender: TObject);
    procedure TextQueryExit(Sender: TObject);
    procedure TextQueryKeyPress(Sender: TObject; var Key: char);
    procedure UndoCloseTabActionExecute(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: integer; const Parameters: array of string);
    procedure SelectOrAddTab(const query: string);
    procedure OpenShortcutFile(const fname: string);
  private
    FAllLines, FDiscardedLines, FAllTags, FTabCaretsAndViews: TStringList;
    FFoundTextPoints: array of TPoint; //xy of caret for each found text
    FAllTagCount, FSelectedTagCount: integer;
    FUndoCloseTabHistory: array [0..UndoCloseTabHistoryMaxSize] of string;
    FUndoCloseTabHistoryUsed: integer;
    FBaseTitle: string;

    procedure CollectAllTags;
    procedure FilterTextByAwesomeBar;
    procedure UpdateSuggestions;
    function SaveNotes: boolean;
    function QueryUnsavedChanges: TModalResult;
    procedure RefreshFoundPoints;
    procedure MoveCursorToNextFind;
    procedure SaveCaretAndView(tabindex: integer);
    procedure LoadCaretAndView(tabindex: integer);
    procedure SortTodoListAroundCurrentLine;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  StrUtils, dateutils, Math, LCLIntf, SynEditMarkupHighAll, SysUtils,
  SynHighlighterAny, FileUtil, SynGutterLineNumber;

const
  LineCountPanel = 0;
  TagCountPanel = 1;
  QueryTimePanel = 2;
  OldSizePanel = 3;
  NotesPathSizePanel = 4;

  { TForm1 }

function IsTagLine(const line: string): boolean;
var
  i: integer;
begin
  //a tag line must start with # and be longer than 2
  if (not line.StartsWith('#')) or (Length(line) < 2) then
    Exit(False);

  //a tagline must not have hashes inside words
  for i := 2 to Length(line) do
    if (line[i] = '#') and (line[i - 1] <> ' ') then
      Exit(False);

  //a tagline must not contain words without hash in front
  for i := 2 to Length(line) do
    if (line[i] <> '#') and (line[i] <> ' ') and (line[i - 1] = ' ') then
      Exit(False);

  Exit(True);
end;

function TagLineWithTag(const line, tag: string): boolean;
begin
  Result := IsTagLine(line) and (line + ' ').Contains('#' + tag + ' ');
end;

function LoadQueryFromShortcutFile(const fpath: string): string;
const
  prefix = 'Query ';
var
  Lines: TStringList;
  line: string;
begin
  try
    Lines := TStringList.Create;
    Lines.LoadFromFile(fpath);
    for line in Lines do
      if line.StartsWith(prefix) then
        Exit(line.Substring(Length(prefix)));
  finally
    Lines.Free;
  end;
  Result := '';
end;

function GetFileNearExe(const fname: string): string;
begin
  Result := ExtractFilePath(Application.ExeName) + fname;
end;

function PrettyPrintFileSize(size: int64): string;
begin
  if size < 0 then
    Exit('Error');

  if size < intpower(1024.0, 1) then
    Exit(IntToStr(size) + ' Bytes');

  if size < intpower(1024.0, 2) then
    Exit(FloatToStr(RoundTo(size / intpower(1024.0, 1), -3)) + ' KiB');

  if size < intpower(1024.0, 3) then
    Exit(FloatToStr(RoundTo(size / intpower(1024.0, 2), -3)) + ' MiB');

  if size < intpower(1024.0, 4) then
    Exit(FloatToStr(RoundTo(size / intpower(1024.0, 3), -3)) + ' GiB');

  Result := FloatToStr(RoundTo(size / intpower(1024.0, 4), -3)) + ' TiB';
end;

function GetOldDirSize: string;
var
  oname, fname: string;
  list: TStringList;
  fsize, tsize: int64;
begin
  oname := GetFileNearExe('old');
  if not DirectoryExists(oname) then
    Exit('Old: no directory.');

  list := FindAllFiles(oname);
  tsize := 0;
  for fname in list do
  begin
    fsize := FileSize(fname);
    if fsize > 0 then
      tsize += fsize;
  end;

  Result := 'Old: ' + IntToStr(list.Count) + ' files, ' + PrettyPrintFileSize(tsize);
  list.Free;
end;

function GetWordTouchingCursor(const line: string; idx: integer): string;
var
  tmp: string;
  start, ending: integer;
begin
  tmp := ' ' + line + ' ';
  start := idx + 1;
  ending := idx + 1;
  while tmp[start - 1] <> ' ' do
    start := start - 1;

  while tmp[ending] <> ' ' do
    ending := ending + 1;

  Result := Copy(tmp, start, ending - start);
end;

procedure EnsureNotesExist(const notefile: string);
var
  initialnotefile: string;
  content: TStringList;
begin
  if FileExists(notefile) then
    Exit;

  initialnotefile := GetFileNearExe('botes-initial-allnotes.txt');
  if FileExists(initialnotefile) then
  begin
    CopyFile(initialnotefile, notefile);
    Exit;
  end;

  content := TStringList.Create;
  try
    content.LineBreak := #10;

    // below was auto generated
    content.Add('#demo');
    content.Add('Hello, controlls are:');
    content.Add('Ctrl + S to save changes.');
    content.Add(
      'Esc - switch between this text area and the bar at the top that now says ''demo''.');
    content.Add('Ctrl + Page Up/Down - move between tabs.');
    content.Add('Ctrl + Shift + Page Up/Down - move the current tab around on the tab bar.');
    content.Add('Ctrl + N - open a new tab.');
    content.Add('Ctrl + T - open a new tab.');
    content.Add('Ctrl + Shift + T - undo closing last tab.');
    content.Add('Ctrl + W - close current tab.');
    content.Add(
      'Ctrl + D - (for TODO lists), toggle between [ ] and [x] if current line starts with either.');
    content.Add(
      'Ctrl + Shift + D - add a [ ] at the start of the line (for TODO lists) if not already present.');
    content.Add(
      'Ctrl + O - open URL or hashtag that''s selected or (if nothing''s selected) under cursor');
    content.Add('');
    content.Add('Ctrl + F when in main text area will open a simple search');
    content.Add('');
    content.Add('Ctrl + K - sorts a TODO list (putting done at the end), try:');
    content.Add('[ ] item 1');
    content.Add('[x] item 2');
    content.Add('[x] item 3');
    content.Add('[ ] item 4');
    content.Add('[x] item 5');
    content.Add('');
    content.Add('Double click on status bar to open the directory Botes is in.');
    content.Add('');
    content.Add('Create old directory there to get a duplicate of every saved file.');
    content.Add('See the included README.md for more details about this feature.');
    content.Add('');
    content.Add(
      'Create a file with a single line (case sensitive) in the form of ''Query YOURTAG''');
    content.Add(
      'and pass it as first and only argument when starting Botes to create a shortcut');
    content.Add(
      'to open/select a tab with ''YOURTAG'' in it. You can further use a unique extension');
    content.Add(
      '(for example .bs, Botes shortcut) and associate it with Botes to create a file');
    content.Add(
      'that when opened with default app from explorer or command line will open Botes.');
    content.Add('');
    content.Add(
      'There can only be one instance of Botes running, if you attempt to open another');
    content.Add(
      'it will instead focus the already opened one. This feature works properly with');
    content.Add(
      'the above shortcut file feature as well (it''ll open or focus a tab specified in');
    content.Add('the file you attempted to open in the already existing instance).');
    content.Add('');
    content.Add(
      'Check https://github.com/FRex/Botes for the latest version and more information.');
    content.Add('');
    content.Add('When the suggestions list box is visible pressing down will go into it');
    content.Add('and pressing up when having first item selected in it or escape at');
    content.Add('any time will go back to editing the query edit box.');
    content.Add('');
    content.Add(
      'Pressing enter will select the item from list box, query it and go to text area.');
    content.Add('Pressing backspace will go back to query edit box and put in it');
    content.Add('the item minus the last character.');
    content.Add('');
    content.Add('');

    content.SaveToFile(notefile);
  finally
    content.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  caretmarkup: TSynEditMarkupHighlightAllCaret;
  matchmarkup: TSynEditMarkupHighlightAll;
  i: integer;
  allnotespath: string;
  notespanelstr: string;
begin
  WindowState := wsMaximized;

  FBaseTitle := Caption;
  FAllLines := TStringList.Create;
  FAllLines.LineBreak := #10;
  FDiscardedLines := TStringList.Create;
  FDiscardedLines.LineBreak := #10;
  FAllTags := TStringList.Create;
  FAllLines.LineBreak := #10;
  FTabCaretsAndViews := TStringList.Create;
  FTabCaretsAndViews.LineBreak := #10;
  MainTabs.Tabs.LineBreak := #10;
  MainTabs.Options := MainTabs.Options + [nboDoChangeOnSetIndex];
  StatusBar.Panels[OldSizePanel].Text := GetOldDirSize;
  FUndoCloseTabHistoryUsed := 0;
  allnotespath := GetFileNearExe('allnotes.txt');
  EnsureNotesExist(allnotespath);

  try

    FAllLines.LoadFromFile(allnotespath);
    notespanelstr := allnotespath + ' - ' + PrettyPrintFileSize(FileSize(allnotespath));
    StatusBar.Panels[NotesPathSizePanel].Text := notespanelstr;
    if TextEditor.Gutter.Parts.Part[1] is TSynGutterLineNumber then
      (TextEditor.Gutter.Parts.Part[1] as TSynGutterLineNumber).DigitCount :=
        Length(IntToStr(FAllLines.Count));

  except
    on EFOpenError do
      MessageDlg('File not found', Format('Failed to open file: %s', [allnotespath]),
        mtError, [mbOK], 0);
  end;

  FAllTags.Sorted := True;
  FAllTags.Duplicates := dupIgnore;
  CollectAllTags;

  TextEditor.Lines.Assign(FAllLines);

  caretmarkup := TSynEditMarkupHighlightAllCaret(
    TextEditor.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  caretmarkup.MarkupInfo.Background := clTeal;
  caretmarkup.WaitTime := 100;
  caretmarkup.Trim := True;
  caretmarkup.FullWord := True;

  matchmarkup := TSynEditMarkupHighlightAll(
    TextEditor.MarkupByClass[TSynEditMarkupHighlightAll]);
  matchmarkup.Enabled := False;

  try
    FTabCaretsAndViews.LoadFromFile(GetFileNearExe('tabcaretsandviews.txt'));
  except
    //ignore this error, since this is a file added in later version of the program
    on EFOpenError do ;
  end;

  try
    MainTabs.Tabs.LoadFromFile(GetFileNearExe('tabs.txt'));
    for i := 0 to MainTabs.Tabs.Count - 1 do
      if (length(MainTabs.Tabs[i]) > 0) and (MainTabs.Tabs[i][1] = '@') then
      begin
        MainTabs.Tabs[i] := Copy(MainTabs.Tabs[i], 2, length(MainTabs.Tabs[i]));
        MainTabs.TabIndex := i;
      end;

    if ParamCount > 0 then
      OpenShortcutFile(ParamStr(1));
  except
    //ignore EFOpenError - we start with one empty tab set in designer so its ok
    on EFOpenError do ;
  end;

  //make sure we have entry for each tab, if there were more tabs than line numbers
  while FTabCaretsAndViews.Count < MainTabs.Tabs.Count do
    FTabCaretsAndViews.Add('');

  //set first tab awesome bar
  MainTabsChange(MainTabs);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FAllLines.Free;
  FDiscardedLines.Free;
  FAllTags.Free;
  FTabCaretsAndViews.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //set focus to text if there is a tag query or to awesome bar if there isnt
  if Length(MainTabs.Tabs[MainTabs.TabIndex]) = 0 then
    AwesomeBar.SetFocus
  else
    TextEditor.SetFocus;
end;

procedure TForm1.MainTabsChange(Sender: TObject);
begin
  AwesomeBar.Text := MainTabs.Tabs[MainTabs.TabIndex];
  LoadCaretAndView(MainTabs.TabIndex);
end;

procedure TForm1.MainTabsChanging(Sender: TObject; var AllowChange: boolean);
var
  ans: TModalResult;
begin
  if TextEditor.Modified then
  begin
    ans := QueryUnsavedChanges;
    if ans = mrCancel then
      AllowChange := False;

    if ans = mrYes then
      AllowChange := SaveNotes;

    if ans = mrNo then
      TextEditor.Modified := False;

  end; //if
end;

procedure TForm1.MoveTabBackwardActionExecute(Sender: TObject);
var
  onchanging: TTabChangingEvent;
begin
  if MainTabs.TabIndex = 0 then
    Exit;

  MainTabs.Tabs.Exchange(MainTabs.TabIndex, MainTabs.TabIndex - 1);
  FTabCaretsAndViews.Exchange(MainTabs.TabIndex, MainTabs.TabIndex - 1);
  onchanging := MainTabs.OnChanging;
  MainTabs.OnChanging := nil;
  MainTabs.TabIndex := MainTabs.TabIndex - 1;
  MainTabs.OnChanging := onchanging;
end;

procedure TForm1.MoveTabForwardActionExecute(Sender: TObject);
var
  onchanging: TTabChangingEvent;
begin
  if (MainTabs.TabIndex + 1) = MainTabs.Tabs.Count then
    Exit;

  MainTabs.Tabs.Exchange(MainTabs.TabIndex, MainTabs.TabIndex + 1);
  FTabCaretsAndViews.Exchange(MainTabs.TabIndex, MainTabs.TabIndex + 1);
  onchanging := MainTabs.OnChanging;
  MainTabs.OnChanging := nil;
  MainTabs.TabIndex := MainTabs.TabIndex + 1;
  MainTabs.OnChanging := onchanging;
end;

procedure TForm1.NewTabActionExecute(Sender: TObject);
begin
  SaveCaretAndView(MainTabs.TabIndex);
  MainTabs.Tabs.Append('');
  FTabCaretsAndViews.Append('');
  MainTabs.TabIndex := MainTabs.Tabs.Count - 1;
  AwesomeBar.SetFocus;
end;

procedure TForm1.NextTabActionExecute(Sender: TObject);
var
  tab: integer;
begin
  tab := MainTabs.TabIndex + 1;
  if tab = MainTabs.Tabs.Count then
    tab := 0;

  SaveCaretAndView(MainTabs.TabIndex);
  MainTabs.TabIndex := tab;
  LoadCaretAndView(tab);
end;

procedure TForm1.PrevTabActionExecute(Sender: TObject);
var
  tab: integer;
begin
  tab := MainTabs.TabIndex;
  if tab = 0 then
    tab := MainTabs.Tabs.Count;

  SaveCaretAndView(MainTabs.TabIndex);
  MainTabs.TabIndex := tab - 1;
  LoadCaretAndView(tab - 1);
end;

procedure TForm1.SaveNotesActionExecute(Sender: TObject);
begin
  SaveNotes;
end;

procedure TForm1.StatusBarDblClick(Sender: TObject);
begin
  OpenDocument(GetFileNearExe(''));
end;

procedure TForm1.SuggestionsKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  str: string;
  ans: TModalResult;
begin
  if Key = VK_ESCAPE then
  begin
    if TextEditor.Modified then
    begin
      ans := QueryUnsavedChanges;
      if ans = mrYes then
      begin
        if SaveNotes then
          AwesomeBar.SetFocus;
      end;
      if ans = mrNo then
        AwesomeBar.SetFocus;
    end
    else
      AwesomeBar.SetFocus;
  end;

  if (Key = VK_RETURN) and (Suggestions.ItemIndex <> -1) then
  begin
    AwesomeBar.Text := Suggestions.Items[Suggestions.ItemIndex];
    TextEditor.SetFocus;
  end;

  if (Key = VK_UP) and (Suggestions.ItemIndex = 0) then
  begin
    AwesomeBar.SetFocus;
    AwesomeBar.SelStart := Length(AwesomeBar.Text);
    AwesomeBar.SelLength := 0;
    DeselectSuggestionsTimer.Enabled := True;
  end;

  if (Key = VK_BACK) and (Suggestions.ItemIndex <> -1) then
  begin
    str := Suggestions.Items[Suggestions.ItemIndex];
    AwesomeBar.Text := str.Substring(0, Length(str) - 1);
    AwesomeBar.SetFocus;
    AwesomeBar.SelStart := Length(AwesomeBar.Text);
    AwesomeBar.SelLength := 0;
    DeselectSuggestionsTimer.Enabled := True;
  end;
end;

procedure TForm1.TextEditorCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
var
  mark, word, line, url, tmp: string;
  a, b, oldCaretXY, oldBlockBegin, oldBlockEnd: TPoint;
  lineno: longint;
begin
  if Command = (ecUserDefinedFirst + 0) then
  begin
    a.Create(1, TextEditor.CaretY);
    b.Create(4, TextEditor.CaretY);

    //save original text to not keep querying it and set the [x]/[ ] etc. more than once
    mark := TextEditor.TextBetweenPoints[a, b];

    //use TextBetweenPoints so this change is in editing history and can be ctrl-z'd
    if mark = '[ ]' then
      TextEditor.TextBetweenPoints[a, b] := '[x]';

    if mark = '[x]' then
      TextEditor.TextBetweenPoints[a, b] := '[ ]';
  end;

  if Command = (ecUserDefinedFirst + 1) then
  begin
    url := '';

    //if anything is selected try to open it, otherwise open 'word' near cursor
    if TextEditor.SelText <> '' then
      url := TextEditor.SelText
    else
    begin
      line := TextEditor.Lines[TextEditor.CaretY - 1];
      if (Length(line) + 1) >= TextEditor.CaretX then
      begin
        word := GetWordTouchingCursor(line, TextEditor.CaretX);

        //looking for substring :/ catches absolute Windows paths (with forward
        //slashes) and URLs, looking for starting / catches absolute Linux paths
        //starting with hashtag is to catch links to other notes within the app
        if (Pos(':/', word) <> 0) or word.StartsWith('/') or word.StartsWith('#') then
          url := word;
      end;
    end;

    if url.StartsWith('#') then
      SelectOrAddTab(Copy(url, 2, Length(url) + 100))
    else if url <> '' then
      OpenUrl(url);

  end; //if command is user defined first + 1

  if Command = (ecUserDefinedFirst + 2) then
    SortTodoListAroundCurrentLine;


  if Command = (ecUserDefinedFirst + 3) then
  begin
    oldCaretXY := TextEditor.CaretXY;
    oldBlockBegin := TextEditor.BlockBegin;
    oldBlockEnd := TextEditor.BlockEnd;

    for lineno := TextEditor.CharIndexToRowCol(TextEditor.SelStart).Y
      to TextEditor.CharIndexToRowCol(TextEditor.SelEnd).Y do
    begin
      a.Create(1, lineno);
      b.Create(4, lineno);
      tmp := TextEditor.TextBetweenPoints[a, b];
      if (tmp <> '[ ]') and (tmp <> '[x]') and (TextEditor.Lines[lineno - 1] <> '') then
        TextEditor.TextBetweenPoints[a, b] := '[ ] ' + tmp;
    end;

    TextEditor.CaretXY := oldCaretXY;
    TextEditor.BlockBegin := oldBlockBegin;
    TextEditor.BlockEnd := oldBlockEnd;
  end;

end;

function IsTodoLine(line: string): boolean;
begin
  Exit((line.Substring(0, 3) = '[ ]') or (line.Substring(0, 3) = '[x]'));
end;

procedure TForm1.SortTodoListAroundCurrentLine;
var
  firstTodo, lastTodo, cur: integer;
  completed, pending: TStringList;
  line: string;
begin
  if not IsTodoLine(TextEditor.Lines[TextEditor.CaretY - 1]) then
    Exit;

  for cur := TextEditor.CaretY - 1 downto 0 do
  begin
    if not IsTodoLine(TextEditor.Lines[cur]) then
      break;
    firstTodo := cur;
  end;

  for cur := TextEditor.CaretY - 1 to TextEditor.Lines.Count - 1 do
  begin
    if not IsTodoLine(TextEditor.Lines[cur]) then
      break;
    lastTodo := cur;
  end;

  completed := TStringList.Create;
  pending := TStringList.Create;

  for cur := firstTodo to lastTodo do
    if TextEditor.Lines[cur][2] = 'x' then
      completed.Append(TextEditor.Lines[cur])
    else
      pending.Append(TextEditor.Lines[cur]);

  TextEditor.BeginUpdate;
  for cur := firstTodo to lastTodo do
  begin
    if (cur - firstTodo) < pending.Count then
      line := pending[cur - firstTodo]
    else
      line := completed[cur - firstTodo - pending.Count];

    TextEditor.TextBetweenPoints[TPoint.Create(0, cur + 1),
      TPoint.Create(maxLongint, cur + 1)] := line;
  end;
  TextEditor.EndUpdate;

  completed.Free;
  pending.Free;
end;

procedure TForm1.TextEditorEnter(Sender: TObject);
begin
  Suggestions.Visible := False;
  TextEditor.MarkupByClass[TSynEditMarkupHighlightAllCaret].Enabled := True;
end;

procedure TForm1.TextEditorExit(Sender: TObject);
begin
  TextEditor.MarkupByClass[TSynEditMarkupHighlightAllCaret].Enabled := False;
end;

procedure TForm1.TextEditorKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 27 then
    AwesomeBar.SetFocus;
end;

procedure TForm1.TextEditorSpecialLineMarkup(Sender: TObject;
  Line: integer; var Special: boolean; Markup: TSynSelectedColor);
var
  notempty: boolean;
begin
  if IsTagLine(TextEditor.Lines[Line - 1]) then
  begin
    Special := True;
    if TagLineWithTag(TextEditor.Lines[Line - 1], AwesomeBar.Text) then
    begin
      Markup.Background := clMoneyGreen;
      Markup.Foreground := clBlack;
    end
    else
    begin
      Markup.Background := clMaroon;
      Markup.Foreground := clCream;
    end;
  end; //if IsTagLine(linestr)

  //special case for an UNSAVEABLE edit: highlight first line if it's not a tag
  //line except if it's empty AND the only line in the editor which can be saved
  //because it's a special case when deleting all notes in a given tag
  notempty := not ((TextEditor.Lines.Count = 1) and (TextEditor.Lines[0] = ''));
  if (Line = 1) and (not IsTagLine(TextEditor.Lines[0])) and notempty then
  begin
    Special := True;
    Markup.Background := clMaroon;
    Markup.Foreground := clCream;
  end;
end;

procedure TForm1.TextQueryChange(Sender: TObject);
var
  matchmarkup: TSynEditMarkupHighlightAll;
begin
  RefreshFoundPoints;
  matchmarkup := TSynEditMarkupHighlightAll(
    TextEditor.MarkupByClass[TSynEditMarkupHighlightAll]);
  matchmarkup.SearchString := TextQuery.Text;
  TextQueryStatusLabel.Caption :=
    Format('Find: %d results', [Length(FFoundTextPoints)]);
end;

procedure TForm1.TextQueryEnter(Sender: TObject);
begin
  Suggestions.Visible := False;
  RefreshFoundPoints;
  MoveCursorToNextFind;
  TextEditor.MarkupByClass[TSynEditMarkupHighlightAll].Enabled := True;
end;

procedure TForm1.TextQueryExit(Sender: TObject);
begin
  TextQueryPanel.Hide;
  TextEditor.MarkupByClass[TSynEditMarkupHighlightAll].Enabled := False;
end;

procedure TForm1.TextQueryKeyPress(Sender: TObject; var Key: char);
begin
  if Ord(Key) = 27 then
    TextEditor.SetFocus;

  if Ord(Key) = 13 then
    MoveCursorToNextFind;
end;

procedure TForm1.UndoCloseTabActionExecute(Sender: TObject);
begin
  //FUndoCloseTabHistoryUsed < 0 should NEVER happen but just to be safe
  if FUndoCloseTabHistoryUsed <= 0 then
    Exit;

  //pop element from history and open a tab with it
  FUndoCloseTabHistoryUsed := FUndoCloseTabHistoryUsed - 1;
  MainTabs.Tabs.Append(FUndoCloseTabHistory[FUndoCloseTabHistoryUsed]);
  MainTabs.TabIndex := MainTabs.Tabs.Count - 1;

  //deselect and move cursor to end of string
  if AwesomeBar.Focused then
  begin
    AwesomeBar.SelLength := 0;
    AwesomeBar.SelStart := Length(AwesomeBar.Text);
  end;
end;

procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject;
  ParamCount: integer; const Parameters: array of string);
begin

  if ParamCount > 0 then
    OpenShortcutFile(Parameters[0]);

  Application.Restore;
  Application.BringToFront;
end;

procedure TForm1.SelectOrAddTab(const query: string);
var
  i: integer;
begin
  if query = '' then
    Exit;

  //backwards to select rightmost tab if there are duplicates
  for i := (MainTabs.Tabs.Count - 1) downto 0 do
  begin
    if MainTabs.Tabs[i] = query then
    begin
      MainTabs.TabIndex := i;
      Exit;
    end;
  end; //for

  //if above loop didn't find and select a tab we must add it
  MainTabs.Tabs.Append(query);
  MainTabs.TabIndex := MainTabs.Tabs.Count - 1;
end;

procedure TForm1.OpenShortcutFile(const fname: string);
var
  shortcutquery: string;
begin
  try
    shortcutquery := LoadQueryFromShortcutFile(fname);
    SelectOrAddTab(shortcutquery);
  except
    on EFOpenError do
      MessageDlg('File not found', Format('Failed to open shortcut file: %s', [fname]),
        mtError, [mbOK], 0);
  end;
end;

function PrettyPrintMilliSeconds(totalms: int64): string;
var
  s, ms: integer;
begin
  s := totalms div 1000;
  ms := totalms mod 1000;
  Result := Format('%d.%.3ds', [s, ms]);
end;

procedure TForm1.AwesomeBarChange(Sender: TObject);
var
  starttime: TDateTime;
  tmp: string;
  alllines: boolean;
begin
  starttime := Now;
  MainTabs.Tabs[MainTabs.TabIndex] := AwesomeBar.Text;
  UpdateSuggestions;
  alllines := False;
  if AwesomeBar.Text = '' then
  begin
    FDiscardedLines.Clear;
    TextEditor.Lines.Assign(FAllLines);
    alllines := True;
    Caption := FBaseTitle;
  end
  else
  begin
    Caption := AwesomeBar.Text + ' - ' + FBaseTitle;
    Application.Title := Caption;
    FilterTextByAwesomeBar;
  end;

  tmp := Format('Lines: %d/%d', [TextEditor.Lines.Count, FAllLines.Count]);
  StatusBar.Panels[LineCountPanel].Text := tmp;

  if alllines then
  begin
    StatusBar.Panels[TagCountPanel].Text := 'Tags: all';
  end
  else
  begin
    tmp := Format('Tags: %d/%d', [FSelectedTagCount, FAllTagCount]);
    StatusBar.Panels[TagCountPanel].Text := tmp;
  end;

  tmp := 'Time: ' + PrettyPrintMilliSeconds(MilliSecondsBetween(Now, starttime));
  StatusBar.Panels[QueryTimePanel].Text := tmp;
end;

procedure TForm1.AwesomeBarEnter(Sender: TObject);
var
  ans: TModalResult;
begin
  if TextEditor.Modified then
  begin
    ans := QueryUnsavedChanges;
    if ans = mrCancel then
    begin
      TextEditor.SetFocus;
      Exit;
    end;

    if ans = mrYes then
      SaveNotes;
  end;

  UpdateSuggestions;
  Suggestions.Visible := True;
  Suggestions.ItemIndex := -1;
end;

procedure TForm1.AwesomeBarKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) or (Key = VK_RETURN) then
    TextEditor.SetFocus;

  if Key = VK_DOWN then
  begin
    if Suggestions.Items.Count > 0 then
      Suggestions.ItemIndex := 0;

    Suggestions.SetFocus;
  end;
end;

procedure TForm1.BeginFindActionExecute(Sender: TObject);
begin
  TextQueryPanel.Show;
  TextQuery.SetFocus;
end;

procedure TForm1.CloseTabActionExecute(Sender: TObject);
var
  ans: TModalResult;
begin
  //check for modification first
  if TextEditor.Modified then
  begin
    ans := QueryUnsavedChanges;
    if ans = mrCancel then
      Exit;

    if ans = mrYes then
      SaveNotes;

    if ans = mrNo then
      TextEditor.Modified := False;
  end; //if

  //only close if unmodified or cancel wasn't picked above

  //add to history first if we have space
  if FUndoCloseTabHistoryUsed < UndoCloseTabHistoryMaxSize then
  begin
    FUndoCloseTabHistory[FUndoCloseTabHistoryUsed] := MainTabs.Tabs[MainTabs.TabIndex];
    FUndoCloseTabHistoryUsed := FUndoCloseTabHistoryUsed + 1;
  end;

  MainTabs.Tabs.Delete(MainTabs.TabIndex);
  if MainTabs.Tabs.Count = 0 then
  begin
    MainTabs.Tabs.Append('');
    MainTabsChange(MainTabs);
  end;
  FilterTextByAwesomeBar;

  LoadCaretAndView(MainTabs.TabIndex);

end;

procedure TForm1.DeselectSuggestionsTimerTimer(Sender: TObject);
begin
  Suggestions.ItemIndex := -1;
  DeselectSuggestionsTimer.Enabled := False;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  strs.Assign(MainTabs.Tabs);
  strs[MainTabs.TabIndex] := '@' + strs[MainTabs.TabIndex];
  strs.SaveToFile(GetFileNearExe('tabs.txt'));
  strs.Free;
  SaveCaretAndView(MainTabs.TabIndex);
  FTabCaretsAndViews.SaveToFile(GetFileNearExe('tabcaretsandviews.txt'));
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
  if ans = mrYes then
    CanClose := SaveNotes;

  if ans = mrNo then
    CanClose := True;

  if ans = mrCancel then
    CanClose := False;
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
    FAllTagCount := 0;
    FSelectedTagCount := 0;
    for str in FAllLines do
    begin
      if IsTagLine(str) then
      begin
        inseg := TagLineWithTag(str, AwesomeBar.Text);
        FAllTagCount += 1;
        if inseg then
          FSelectedTagCount += 1;
      end;

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
begin
  try
    tmp := TStringList.Create;
    for str in FAllTags do
      if str.StartsWith(AwesomeBar.Text) then
        tmp.Add(str);

    if tmp.Count = 0 then
      tmp.Assign(FAllTags);

    Suggestions.Items.Assign(tmp);
  finally
    tmp.Free;
  end;
end;

procedure SaveToOld(list: TStrings);
var
  timestamp, dname, fname: string;
begin
  dname := GetFileNearExe('old');
  if not DirectoryExists(dname) then
    Exit;

  DateTimeToString(timestamp, 'yyyy-mm-dd-hh-nn', Now);
  fname := 'allnotes-' + timestamp + '.txt';
  list.SaveToFile(dname + DirectorySeparator + fname);

end;

function TForm1.SaveNotes: boolean;
var
  allnotespath: string;
  notespanelstr: string;
begin
  //fix for below check - since 'no lines' = 'one empty line' kinda
  if (TextEditor.Lines.Count = 1) and (TextEditor.Lines[0] = '') then
    TextEditor.Lines.Clear;

  //first line of non empty text area must be a tag line due to how the file
  //format and app itself is built technically and conceptually
  //if first line isn't so then quit and don't touch any files nor mark content saved
  //note: even if line count is 0 we gotta save to handle the
  //case of someone deleting all notes in a specific tag
  if TextEditor.Lines.Count <> 0 then
    if not IsTagLine(TextEditor.Lines[0]) then
    begin
      MessageDlg('Error', 'First line of text must be a tagline.', mtError, [mbOK], 0);
      Exit(False);
    end;

  allnotespath := GetFileNearExe('allnotes.txt');
  FAllLines.Assign(FDiscardedLines);
  FAllLines.AddStrings(TextEditor.Lines);
  FAllLines.SaveToFile(allnotespath);
  SaveToOld(FAllLines);
  StatusBar.Panels[OldSizePanel].Text := GetOldDirSize;
  notespanelstr := allnotespath + ' - ' + PrettyPrintFileSize(FileSize(allnotespath));
  StatusBar.Panels[NotesPathSizePanel].Text := notespanelstr;
  CollectAllTags;
  TextEditor.MarkTextAsSaved;
  TextEditor.Modified := False;
  SaveCaretAndView(MainTabs.TabIndex);
  FTabCaretsAndViews.SaveToFile(GetFileNearExe('tabcaretsandviews.txt'));
  Exit(True);
end;

function TForm1.QueryUnsavedChanges: TModalResult;
const
  title = 'Unsaved changes';
  message = 'There are unsaved changes. Save them?';
begin
  if FileExists(GetFileNearExe('cfg/always-save-unchanged')) then Exit(mrYes);
  Result := MessageDlg(title, message, mtConfirmation, mbYesNoCancel, 0);
end;

procedure TForm1.RefreshFoundPoints;
var
  i, spot, x, y: integer;
begin
  SetLength(FFoundTextPoints, 0);
  for i := 0 to TextEditor.Lines.Count do
  begin
    spot := 0;
    while True do
    begin
      spot := PosEx(UpperCase(TextQuery.Text), UpperCase(TextEditor.Lines[i]), spot + 1);
      if spot <> 0 then
      begin
        x := spot + Length(TextQuery.Text);
        y := i + 1;
        SetLength(FFoundTextPoints, Length(FFoundTextPoints) + 1);
        FFoundTextPoints[High(FFoundTextPoints)].SetLocation(x, y);
      end
      else
        Break;
    end; //while true
  end; //for line
end;

procedure TForm1.MoveCursorToNextFind;
var
  pt, c: TPoint;
  i: integer;
begin
  c := TextEditor.CaretXY;
  for i := 0 to High(FFoundTextPoints) do
  begin
    pt := FFoundTextPoints[i];
    if (c.y < pt.y) or ((c.y = pt.y) and (c.x < pt.x)) then
    begin
      TextEditor.CaretXY := pt;
      TextQueryStatusLabel.Caption :=
        Format('Find: %d/%d', [i + 1, Length(FFoundTextPoints)]);
      Exit;
    end; //if
  end; //for

  if Length(FFoundTextPoints) <> 0 then
  begin
    TextEditor.CaretXY := FFoundTextPoints[0];
    TextQueryStatusLabel.Caption := Format('Find: 1/%d', [Length(FFoundTextPoints)]);
  end
  else
    TextQueryStatusLabel.Caption := 'No Results';
end;

procedure TForm1.SaveCaretAndView(tabindex: integer);
var
  parts: TStringList;
begin
  try
    parts := TStringList.Create;
    parts.Delimiter := ' ';
    parts.Append(IntToStr(TextEditor.CaretY));
    parts.Append(IntToStr(TextEditor.CaretX));
    parts.Append(IntToStr(TextEditor.TopLine));
    parts.Append(IntToStr(TextEditor.LeftChar));
    FTabCaretsAndViews[tabindex] := parts.DelimitedText;
  finally
    parts.Free;
  end;
end;

procedure TForm1.LoadCaretAndView(tabindex: integer);
var
  parts: TStringList;
  y, x, t, l: integer;
begin
  try
    parts := TStringList.Create;
    try
      parts.Delimiter := ' ';
      parts.DelimitedText := FTabCaretsAndViews[tabindex];
      if parts.Count <> 4 then
        Exit;

      //parse to vars to not partly set in case later int doesnt parse
      y := StrToInt(parts[0]);
      x := StrToInt(parts[1]);
      t := StrToInt(parts[2]);
      l := StrToInt(parts[3]);

      TextEditor.CaretY := y;
      TextEditor.CaretX := x;
      TextEditor.TopLine := t;
      TextEditor.LeftChar := l;
    except
      on EConvertError do ;
    end;
  finally
    parts.Free;
  end;
end;

end.
