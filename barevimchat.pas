unit BarevIMChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, StdCtrls, Graphics, Contnrs, LazUTF8,
  RichMemo, RichMemoUtils, BarevIMTypes;

type
  { TChatTabManager - manages chat tabs and history }
  TChatTabManager = class
  private
    FPageControl: TPageControl;
    FChatHistory: TFPHashList;
    
    function FindTabByCaption(const Caption: string): TTabSheet;
  public
    constructor Create(APageControl: TPageControl);
    destructor Destroy; override;
    
    { Tab management }
    function GetOrCreateTab(const BuddyNick, BuddyJID: string): TTabSheet;
    procedure CloseTab(TabSheet: TTabSheet);
    procedure CloseAllTabs;
    function GetTabRichMemo(TabSheet: TTabSheet): TRichMemo;
    
    { Message display }
    procedure AddMessageToTab(TabSheet: TTabSheet; const Nick, MessageText: string; 
      Incoming: Boolean);
    
    { History management }
    procedure SaveMessageToHistory(const BuddyJID, Nick, MessageText: string; 
      Incoming: Boolean);
    procedure LoadHistoryToTab(TabSheet: TTabSheet; const BuddyJID: string);
    procedure ClearAllHistory;
    
    property PageControl: TPageControl read FPageControl;
  end;

implementation

{ TChatTabManager }

constructor TChatTabManager.Create(APageControl: TPageControl);
begin
  inherited Create;
  FPageControl := APageControl;
  FChatHistory := TFPHashList.Create;
end;

destructor TChatTabManager.Destroy;
begin
  ClearAllHistory;
  FChatHistory.Free;
  inherited Destroy;
end;

function TChatTabManager.FindTabByCaption(const Caption: string): TTabSheet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FPageControl.PageCount - 1 do
  begin
    if FPageControl.Pages[I].Caption = Caption then
    begin
      Result := FPageControl.Pages[I];
      Exit;
    end;
  end;
end;

function TChatTabManager.GetOrCreateTab(const BuddyNick, BuddyJID: string): TTabSheet;
var
  TabSheet: TTabSheet;
  RichMemo: TRichMemo;
begin
  Result := FindTabByCaption(BuddyNick);
  if Assigned(Result) then
    Exit;

  TabSheet := TTabSheet.Create(FPageControl);
  TabSheet.PageControl := FPageControl;
  TabSheet.Caption := BuddyNick;
  TabSheet.Hint := '';

  RichMemo := TRichMemo.Create(TabSheet);
  RichMemo.Parent := TabSheet;
  RichMemo.Align := alClient;
  RichMemo.ReadOnly := True;
  RichMemo.ScrollBars := ssAutoBoth;

  LoadHistoryToTab(TabSheet, BuddyJID);
  
  Result := TabSheet;
end;

procedure TChatTabManager.CloseTab(TabSheet: TTabSheet);
begin
  if Assigned(TabSheet) then
  begin
    TabSheet.Hint := '';
    TabSheet.Free;
  end;
end;

procedure TChatTabManager.CloseAllTabs;
begin
  while FPageControl.PageCount > 0 do
    FPageControl.Pages[0].Free;
end;

function TChatTabManager.GetTabRichMemo(TabSheet: TTabSheet): TRichMemo;
begin
  Result := nil;
  if Assigned(TabSheet) and (TabSheet.ControlCount > 0) and
     (TabSheet.Controls[0] is TRichMemo) then
  begin
    Result := TRichMemo(TabSheet.Controls[0]);
  end;
end;

procedure TChatTabManager.AddMessageToTab(
  TabSheet: TTabSheet;
  const Nick, MessageText: string;
  Incoming: Boolean
);
var
  RichMemo: TRichMemo;
  LastSender: string;
  HeaderStart, HeaderLen: Integer;
  TimeStamp, Symbol, FirstLine: string;
  NickColor: TColor;
  ShowHeader: Boolean;
begin
  RichMemo := GetTabRichMemo(TabSheet);
  if not Assigned(RichMemo) then Exit;

  LastSender := TabSheet.Hint;
  ShowHeader := (LastSender <> Nick);

  TimeStamp := '[' + TimeToStr(Now) + ']';
  Symbol := GetMessageSymbol(Incoming);
  NickColor := GetMessageColor(Incoming);

  RichMemo.Lines.BeginUpdate;
  try
    if ShowHeader then
    begin
      FirstLine := Symbol + ' ' + Nick + ' ' + TimeStamp;

      HeaderStart := UTF8Length(RichMemo.Text);
      HeaderLen := UTF8Length(FirstLine);

      if HeaderStart > 0 then
      begin
        HeaderLen := HeaderLen + 1;
      end;

      RichMemo.Lines.Add(FirstLine);
      RichMemo.Lines.Add(MessageText);

      RichMemo.SetRangeParams(
        HeaderStart,
        HeaderLen,
        [tmm_Color, tmm_Styles],
        '',
        0,
        NickColor,
        [fsBold],
        []
      );

      TabSheet.Hint := Nick;
    end
    else
    begin
      RichMemo.Lines.Add(MessageText);
    end;
  finally
    RichMemo.Lines.EndUpdate;
  end;

  RichMemo.SelStart := RichMemo.GetTextLen;
  RichMemo.SelLength := 0;
end;

procedure TChatTabManager.SaveMessageToHistory(const BuddyJID, Nick, 
  MessageText: string; Incoming: Boolean);
var
  History: TStringList;
  Symbol, TimeStamp, FirstLine: string;
  LastLine: string;
  ShowHeader: Boolean;
begin
  History := TStringList(FChatHistory.Find(BuddyJID));
  if not Assigned(History) then
  begin
    History := TStringList.Create;
    FChatHistory.Add(BuddyJID, History);
  end;

  ShowHeader := True;
  if History.Count > 0 then
  begin
    LastLine := History[History.Count - 1];
    if (Length(LastLine) > 0) and (LastLine[1] <> SYMBOL_INCOMING[1]) and 
       (LastLine[1] <> SYMBOL_OUTGOING[1]) and (History.Count > 1) then
      LastLine := History[History.Count - 2];
    
    Symbol := GetMessageSymbol(Incoming);

    if (Length(LastLine) > 0) and (LastLine[1] = Symbol[1]) and 
       (Pos(Nick, LastLine) > 0) then
      ShowHeader := False;
  end;
  
  TimeStamp := '[' + TimeToStr(Now) + ']';
  
  if ShowHeader then
  begin
    Symbol := GetMessageSymbol(Incoming);
    FirstLine := Symbol + ' ' + Nick + ' ' + TimeStamp;
    History.Add(FirstLine);
  end;
  
  History.Add(MessageText);
end;

procedure TChatTabManager.LoadHistoryToTab(TabSheet: TTabSheet; const BuddyJID: string);
var
  History: TStringList;
  RichMemo: TRichMemo;
  I: Integer;
begin
  RichMemo := GetTabRichMemo(TabSheet);
  if not Assigned(RichMemo) then
    Exit;
  
  RichMemo.Clear;
  
  History := TStringList(FChatHistory.Find(BuddyJID));
  if Assigned(History) then
  begin
    for I := 0 to History.Count - 1 do
      RichMemo.Lines.Add(History[I]);
    
    if RichMemo.Lines.Count > 0 then
    begin
      RichMemo.SelStart := Length(RichMemo.Text);
      RichMemo.SelLength := 0;
    end;
  end;
end;

procedure TChatTabManager.ClearAllHistory;
var
  I: Integer;
  History: TStringList;
begin
  for I := 0 to FChatHistory.Count - 1 do
  begin
    History := TStringList(FChatHistory.Items[I]);
    if Assigned(History) then
      History.Free;
  end;
  FChatHistory.Clear;
end;

end.
