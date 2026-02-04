unit BarevIMForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, LCLType, LCLIntf, Barev, BarevTypes,
  BarevIMTypes, BarevIMConfig, BarevIMChat, BarevIMContacts, BarevIMAddBuddyDlg,
  BarevIMAvatar, BarevIMLoginDlg;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonDisconnect: TButton;
    ButtonSend: TButton;
    ButtonAddBuddy: TButton;
    ComboBoxStatus: TComboBox;
    MemoMessage: TMemo;
    ImageAvatar: TImage;
    LabelUserInfo: TLabel;
    LabelBuddies: TLabel;
    ListBoxBuddies: TListBox;
    MemoLog: TMemo;
    OpenDialogAvatar: TOpenDialog;
    PageControlChats: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    PopupMenuAvatar: TPopupMenu;
    MenuItemSetAvatar: TMenuItem;
    MenuItemClearAvatar: TMenuItem;
    Splitter1: TSplitter;
    Timer1: TTimer;
    PopupMenuBuddy: TPopupMenu;
    MenuItemOpenChat: TMenuItem;
    MenuItemSeparator0: TMenuItem;
    MenuItemRemoveBuddy: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemRequestAvatar: TMenuItem;
    
    procedure ButtonAddBuddyClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ComboBoxStatusChange(Sender: TObject);
    procedure MenuItemOpenChatClick(Sender: TObject);
    procedure MenuItemRemoveBuddyClick(Sender: TObject);
    procedure MenuItemRequestAvatarClick(Sender: TObject);
    procedure MenuItemSetAvatarClick(Sender: TObject);
    procedure MenuItemClearAvatarClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure MemoMessageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageAvatarClick(Sender: TObject);
    procedure ListBoxBuddiesDblClick(Sender: TObject);
    procedure ListBoxBuddiesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ListBoxBuddiesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PageControlChatsCloseTabClicked(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FClient: TBarevClient;
    FConnected: Boolean;
    FCurrentBuddy: TBarevBuddy;
    FFirstShow: Boolean;
    FRightClickIndex: Integer;

    FConfig: TBarevConfig;
    FChatManager: TChatTabManager;
    FContactManager: TContactManager;
    FAvatarManager: TAvatarManager;

    procedure OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure OnTypingNotification(Buddy: TBarevBuddy; IsTyping: Boolean);
    procedure OnLog(const LogLevel, Message: string);

    procedure OnContactLog(const Message: string);
    procedure OnAvatarLog(const Message: string);

    function DoConnect(const ANick, AIPv6: string; APort: Integer): Boolean;
    procedure DoDisconnect;
    procedure ShowLoginDialog;

    procedure UpdateBuddyList;
    procedure UpdateMyAvatarDisplay;
    procedure UpdateUserInfoLabel;
    procedure SetConnectedState(Connected: Boolean);
    procedure LogMessage(const Message: string);
    procedure DrawDefaultAvatar;
    function GetBuddyNickFromListIndex(Index: Integer): string;
    procedure UpdateTabCaption(const BuddyNick: string; Status: TBuddyStatus; IsTyping: Boolean);
    function FindTabByBuddyNick(const BuddyNick: string): TTabSheet;
    procedure OpenChatWithBuddy(Index: Integer);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.DrawDefaultAvatar;
var
  Bmp: TBitmap;
  CenterX, CenterY: Integer;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := 48;
    Bmp.Height := 48;
    
    Bmp.Canvas.Brush.Color := clSilver;
    Bmp.Canvas.FillRect(0, 0, 48, 48);
    
    CenterX := 24;
    CenterY := 24;
    
    Bmp.Canvas.Brush.Color := clGray;
    Bmp.Canvas.Pen.Color := clGray;
    Bmp.Canvas.Ellipse(CenterX - 8, 8, CenterX + 8, 24);
    Bmp.Canvas.Ellipse(CenterX - 16, 26, CenterX + 16, 58);
    
    Bmp.Canvas.Brush.Style := bsClear;
    Bmp.Canvas.Pen.Color := clDkGray;
    Bmp.Canvas.Pen.Width := 2;
    Bmp.Canvas.Rectangle(0, 0, 48, 48);
    
    ImageAvatar.Picture.Bitmap.Assign(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  ConfigPath: string;
begin
  FConnected := False;
  FClient := nil;
  FCurrentBuddy := nil;
  FFirstShow := True;
  FRightClickIndex := -1;

  ConfigPath := DetermineConfigFilePath;
  FConfig := TBarevConfig.Create(ConfigPath);
  FChatManager := TChatTabManager.Create(PageControlChats);
  FContactManager := nil;
  FAvatarManager := nil;

  Caption := 'barev IM';
  Position := poScreenCenter;

  OpenDialogAvatar.Title := 'Select Avatar Image';
  OpenDialogAvatar.Filter := 'Image Files|*.png;*.jpg;*.jpeg;*.gif;*.bmp|All Files|*.*';

  FConfig.Load;

  Left := FConfig.WindowLeft;
  Top := FConfig.WindowTop;
  Width := FConfig.WindowWidth;
  Height := FConfig.WindowHeight;
  
  MemoLog.ReadOnly := True;
  
  ListBoxBuddies.ShowHint := True;

  DrawDefaultAvatar;

  SetConnectedState(False);
  
  LogMessage('Configuration file: ' + FConfig.ConfigFile);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if FFirstShow then
  begin
    FFirstShow := False;
    Application.ProcessMessages;

    if FConfig.AutoConnect and FConfig.IsValidForAutoConnect then
    begin
      LogMessage('Auto-connecting...');
      if not DoConnect(FConfig.Nick, FConfig.IPv6, FConfig.Port) then
      begin
        LogMessage('Auto-connect failed, showing login dialog');
        ShowLoginDialog;
      end;
    end
    else
    begin
      ShowLoginDialog;
    end;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if FConnected then
    DoDisconnect;

  FConfig.WindowLeft := Left;
  FConfig.WindowTop := Top;
  FConfig.WindowWidth := Width;
  FConfig.WindowHeight := Height;
  FConfig.Save;

  FAvatarManager.Free;
  FContactManager.Free;
  FChatManager.Free;
  FConfig.Free;
end;

procedure TFormMain.ShowLoginDialog;
var
  Nick, IPv6: string;
  Port: Integer;
  AutoConnect: Boolean;
begin
  Nick := FConfig.Nick;
  IPv6 := FConfig.IPv6;
  Port := FConfig.Port;
  AutoConnect := FConfig.AutoConnect;
  
  if TLoginDialog.Execute(Nick, IPv6, Port, AutoConnect) then
  begin
    FConfig.Nick := Nick;
    FConfig.IPv6 := IPv6;
    FConfig.Port := Port;
    FConfig.AutoConnect := AutoConnect;
    FConfig.Save;

    if not DoConnect(Nick, IPv6, Port) then
    begin
      if MessageDlg('Connection Failed', 
        'Failed to connect. Try again?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        ShowLoginDialog
      else
        Application.Terminate;
    end;
  end
  else
  begin
    Application.Terminate;
  end;
end;

function TFormMain.DoConnect(const ANick, AIPv6: string; APort: Integer): Boolean;
begin
  Result := False;
  
  try
    FClient := TBarevClient.Create(ANick, AIPv6, APort);
    FClient.OnMessageReceived := @OnMessageReceived;
    FClient.OnBuddyStatus := @OnBuddyStatus;
    FClient.OnTypingNotification := @OnTypingNotification;
    FClient.OnLog := @OnLog;
    
    if FClient.Start then
    begin
      SetConnectedState(True);
      LogMessage('=== Connected as ' + FClient.MyJID + ' on port ' + IntToStr(APort) + ' ===');

      FContactManager := TContactManager.Create(FClient, FConfig.GetContactsFilePath);
      FContactManager.OnLog := @OnContactLog;
      FContactManager.LoadContacts;

      FAvatarManager := TAvatarManager.Create(FClient);
      FAvatarManager.OnLog := @OnAvatarLog;

      if (FConfig.AvatarPath <> '') and FileExists(FConfig.AvatarPath) then
      begin
        if FAvatarManager.SetMyAvatar(FConfig.AvatarPath) then
          LogMessage('Loaded saved avatar: ' + FConfig.AvatarPath);
      end;
      
      UpdateBuddyList;
      UpdateMyAvatarDisplay;
      UpdateUserInfoLabel;
      
      Result := True;
    end
    else
    begin
      ShowMessage('Failed to start Barev client. Port ' + IntToStr(APort) + ' may be in use.');
      FClient.Free;
      FClient := nil;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Error starting client: ' + E.Message);
      if Assigned(FClient) then
      begin
        FClient.Free;
        FClient := nil;
      end;
    end;
  end;
end;

procedure TFormMain.DoDisconnect;
begin
  if Assigned(FContactManager) then
  begin
    FContactManager.SaveContacts;
    FreeAndNil(FContactManager);
  end;
  
  FreeAndNil(FAvatarManager);

  if Assigned(FClient) then
  begin
    FClient.Stop;
    FreeAndNil(FClient);
  end;
  
  FCurrentBuddy := nil;
  SetConnectedState(False);
  
  ListBoxBuddies.Clear;
  FChatManager.CloseAllTabs;
  FChatManager.ClearAllHistory;
  
  DrawDefaultAvatar;
  UpdateUserInfoLabel;
  
  LogMessage('=== Disconnected ===');
end;

procedure TFormMain.ButtonDisconnectClick(Sender: TObject);
begin
  if FConnected then
  begin
    DoDisconnect;
    ShowLoginDialog;
  end;
end;

procedure TFormMain.SetConnectedState(Connected: Boolean);
begin
  FConnected := Connected;
  
  if Connected then
  begin
    ButtonDisconnect.Enabled := True;
    ButtonAddBuddy.Enabled := True;
    ComboBoxStatus.Enabled := True;
    ComboBoxStatus.ItemIndex := 0;
    Timer1.Enabled := True;
  end
  else
  begin
    ButtonDisconnect.Enabled := False;
    ButtonAddBuddy.Enabled := False;
    ButtonSend.Enabled := False;
    MemoMessage.Enabled := False;
    ComboBoxStatus.Enabled := False;
    ComboBoxStatus.ItemIndex := -1;
    Timer1.Enabled := False;
  end;
end;

procedure TFormMain.UpdateUserInfoLabel;
begin
  if FConnected and Assigned(FClient) then
    LabelUserInfo.Caption := FClient.MyJID
  else
    LabelUserInfo.Caption := 'Not connected';
end;

procedure TFormMain.LogMessage(const Message: string);
begin
  MemoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + Message);
end;

procedure TFormMain.OnContactLog(const Message: string);
begin
  LogMessage(Message);
end;

procedure TFormMain.OnAvatarLog(const Message: string);
begin
  LogMessage('[Avatar] ' + Message);
end;

procedure TFormMain.ButtonAddBuddyClick(Sender: TObject);
var
  BuddyNick, BuddyIPv6: string;
  BuddyPort: Integer;
begin
  if not FConnected then
    Exit;
  
  if TAddBuddyDialog.Execute(BuddyNick, BuddyIPv6, BuddyPort) then
  begin
    try
      FContactManager.AddBuddy(BuddyNick, BuddyIPv6, BuddyPort);
      FContactManager.SaveContacts;
      UpdateBuddyList;
    except
      on E: Exception do
        ShowMessage('Error adding buddy: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.ComboBoxStatusChange(Sender: TObject);
var
  NewStatus: TBuddyStatus;
begin
  if not FConnected or not Assigned(FClient) then
    Exit;
  
  case ComboBoxStatus.ItemIndex of
    0: NewStatus := bsAvailable;
    1: NewStatus := bsAway;
    2: NewStatus := bsExtendedAway;
    3: NewStatus := bsDoNotDisturb;
  else
    NewStatus := bsAvailable;
  end;
  
  if FClient.SendPresence(NewStatus, '') then
    LogMessage('Status changed to: ' + StatusToDisplayString(NewStatus))
  else
    LogMessage('Failed to change status');
end;

function TFormMain.GetBuddyNickFromListIndex(Index: Integer): string;
var
  ItemText: string;
  SpacePos: Integer;
begin
  Result := '';
  if (Index < 0) or (Index >= ListBoxBuddies.Items.Count) then
    Exit;
  
  ItemText := ListBoxBuddies.Items[Index];
  SpacePos := Pos(' ', ItemText);
  if SpacePos > 0 then
    Result := Copy(ItemText, SpacePos + 1, Length(ItemText))
  else
    Result := ItemText;
end;

procedure TFormMain.OpenChatWithBuddy(Index: Integer);
var
  BuddyNick: string;
  Buddy: TBarevBuddy;
  TabSheet: TTabSheet;
begin
  if Index < 0 then
    Exit;
  
  BuddyNick := GetBuddyNickFromListIndex(Index);
  Buddy := FContactManager.FindBuddyByNick(BuddyNick);
  
  if Assigned(Buddy) then
  begin
    FCurrentBuddy := Buddy;
    
    TabSheet := FChatManager.GetOrCreateTab(Buddy.Nick, Buddy.JID);
    UpdateTabCaption(Buddy.Nick, Buddy.Status, False);
    PageControlChats.ActivePage := TabSheet;
    
    LogMessage('Opened chat with: ' + FCurrentBuddy.Nick);
    
    try
      FClient.ConnectToBuddy(FCurrentBuddy.JID);
      
      ButtonSend.Enabled := True;
      MemoMessage.Enabled := True;
      MemoMessage.SetFocus;
      
      LogMessage('Connecting to: ' + FCurrentBuddy.Nick);
    except
      on E: Exception do
        ShowMessage('Error connecting to buddy: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.MenuItemOpenChatClick(Sender: TObject);
begin
  if not FConnected then
    Exit;
  
  if FRightClickIndex < 0 then
    Exit;
  
  OpenChatWithBuddy(FRightClickIndex);
end;

procedure TFormMain.MenuItemRemoveBuddyClick(Sender: TObject);
var
  BuddyNick: string;
  Buddy: TBarevBuddy;
  TabSheet: TTabSheet;
  I: Integer;
begin
  if not FConnected then
    Exit;
  
  if FRightClickIndex < 0 then
  begin
    ShowMessage('Please select a buddy to remove');
    Exit;
  end;
  
  BuddyNick := GetBuddyNickFromListIndex(FRightClickIndex);
  
  if MessageDlg('Remove Buddy',
                'Are you sure you want to remove ' + BuddyNick + '?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;
  
  Buddy := FContactManager.FindBuddyByNick(BuddyNick);
  if not Assigned(Buddy) then
  begin
    ShowMessage('Could not find buddy');
    Exit;
  end;
  
  try
    if Assigned(FCurrentBuddy) and (FCurrentBuddy.JID = Buddy.JID) then
    begin
      FCurrentBuddy := nil;
      ButtonSend.Enabled := False;
      MemoMessage.Enabled := False;
    end;
    
    for I := 0 to PageControlChats.PageCount - 1 do
    begin
      if Pos(BuddyNick, PageControlChats.Pages[I].Caption) > 0 then
      begin
        TabSheet := PageControlChats.Pages[I];
        FChatManager.CloseTab(TabSheet);
        Break;
      end;
    end;
    
    if FContactManager.RemoveBuddy(Buddy.JID) then
    begin
      FContactManager.SaveContacts;
      UpdateBuddyList;
      LogMessage('Removed buddy: ' + BuddyNick);
    end
    else
      ShowMessage('Failed to remove buddy');
  except
    on E: Exception do
      ShowMessage('Error removing buddy: ' + E.Message);
  end;
end;

procedure TFormMain.MenuItemRequestAvatarClick(Sender: TObject);
var
  BuddyNick: string;
  Buddy: TBarevBuddy;
begin
  if not FConnected then
    Exit;
  
  if FRightClickIndex < 0 then
  begin
    ShowMessage('Please select a buddy');
    Exit;
  end;
  
  BuddyNick := GetBuddyNickFromListIndex(FRightClickIndex);
  Buddy := FContactManager.FindBuddyByNick(BuddyNick);
  
  if Assigned(Buddy) then
    FAvatarManager.RequestBuddyAvatar(Buddy.JID);
end;

procedure TFormMain.MenuItemSetAvatarClick(Sender: TObject);
begin
  if not FConnected then
    Exit;
  
  if OpenDialogAvatar.Execute then
  begin
    if FAvatarManager.SetMyAvatar(OpenDialogAvatar.FileName) then
    begin
      FConfig.AvatarPath := OpenDialogAvatar.FileName;
      UpdateMyAvatarDisplay;
    end;
  end;
end;

procedure TFormMain.MenuItemClearAvatarClick(Sender: TObject);
begin
  if not FConnected then
    Exit;
  
  FAvatarManager.ClearMyAvatar;
  FConfig.AvatarPath := '';
  DrawDefaultAvatar;
end;

procedure TFormMain.ImageAvatarClick(Sender: TObject);
var
  Pt: TPoint;
begin
  if FConnected then
  begin
    Pt := ImageAvatar.ClientToScreen(Point(0, ImageAvatar.Height));
    PopupMenuAvatar.PopUp(Pt.X, Pt.Y);
  end;
end;

procedure TFormMain.ButtonSendClick(Sender: TObject);
var
  MessageText: string;
  TabSheet: TTabSheet;
begin
  if not Assigned(FCurrentBuddy) then
    Exit;
  
  MessageText := Trim(MemoMessage.Text);
  if MessageText = '' then
    Exit;
  
  try
    FClient.SendMessage(FCurrentBuddy.JID, MessageText);
    
    TabSheet := FChatManager.GetOrCreateTab(FCurrentBuddy.Nick, FCurrentBuddy.JID);
    UpdateTabCaption(FCurrentBuddy.Nick, FCurrentBuddy.Status, False);
    FChatManager.AddMessageToTab(TabSheet, FConfig.Nick, MessageText, False);
    FChatManager.SaveMessageToHistory(FCurrentBuddy.JID, FConfig.Nick, MessageText, False);
    
    MemoMessage.Clear;
    MemoMessage.SetFocus;
  except
    on E: Exception do
      ShowMessage('Error sending message: ' + E.Message);
  end;
end;

procedure TFormMain.MemoMessageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = 13) and (ssCtrl in Shift) then
  begin
    Key := 0;
    ButtonSendClick(Sender);
  end;
end;

procedure TFormMain.ListBoxBuddiesContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Index: Integer;
begin
  Index := ListBoxBuddies.ItemAtPos(MousePos, True);
  if Index >= 0 then
  begin
    FRightClickIndex := Index;
    ListBoxBuddies.ItemIndex := Index;
    Handled := False;
  end
  else
  begin
    FRightClickIndex := -1;
    Handled := True;
  end;
end;

procedure TFormMain.ListBoxBuddiesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Index: Integer;
  BuddyNick: string;
  Buddy: TBarevBuddy;
begin
  Index := ListBoxBuddies.ItemAtPos(Point(X, Y), True);
  if Index >= 0 then
  begin
    BuddyNick := GetBuddyNickFromListIndex(Index);
    if Assigned(FContactManager) then
    begin
      Buddy := FContactManager.FindBuddyByNick(BuddyNick);
      if Assigned(Buddy) then
        ListBoxBuddies.Hint := BuddyNick + ' - ' + StatusToDisplayString(Buddy.Status)
      else
        ListBoxBuddies.Hint := BuddyNick;
    end
    else
      ListBoxBuddies.Hint := BuddyNick;
  end
  else
    ListBoxBuddies.Hint := '';
end;

procedure TFormMain.ListBoxBuddiesDblClick(Sender: TObject);
begin
  OpenChatWithBuddy(ListBoxBuddies.ItemIndex);
end;

function TFormMain.FindTabByBuddyNick(const BuddyNick: string): TTabSheet;
var
  I: Integer;
  TabCaption: string;
begin
  Result := nil;
  for I := 0 to PageControlChats.PageCount - 1 do
  begin
    TabCaption := PageControlChats.Pages[I].Caption;
    if (TabCaption = BuddyNick) or
       (Pos(StatusToIcon(bsOffline) + ' ' + BuddyNick, TabCaption) > 0) or
       (Pos(StatusToIcon(bsAvailable) + ' ' + BuddyNick, TabCaption) > 0) or
       (Pos(StatusToIcon(bsAway) + ' ' + BuddyNick, TabCaption) > 0) or
       (Pos(StatusToIcon(bsExtendedAway) + ' ' + BuddyNick, TabCaption) > 0) or
       (Pos(StatusToIcon(bsDoNotDisturb) + ' ' + BuddyNick, TabCaption) > 0) or
       (Pos(BuddyNick + ' (typing...)', TabCaption) > 0) then
    begin
      Result := PageControlChats.Pages[I];
      Exit;
    end;
  end;
end;

procedure TFormMain.UpdateTabCaption(const BuddyNick: string; Status: TBuddyStatus; IsTyping: Boolean);
var
  TabSheet: TTabSheet;
  NewCaption: string;
begin
  TabSheet := FindTabByBuddyNick(BuddyNick);
  if not Assigned(TabSheet) then
    Exit;
  
  if IsTyping then
    NewCaption := StatusToIcon(Status) + ' ' + BuddyNick + ' (typing...)'
  else
    NewCaption := StatusToIcon(Status) + ' ' + BuddyNick;
  
  TabSheet.Caption := NewCaption;
end;

procedure TFormMain.PageControlChatsCloseTabClicked(Sender: TObject);
var
  TabSheet: TTabSheet;
  TabCaption: string;
  I: Integer;
  Buddy: TBarevBuddy;
  BuddyCount: Integer;
begin
  if PageControlChats.ActivePage <> nil then
  begin
    TabSheet := PageControlChats.ActivePage;
    TabCaption := TabSheet.Caption;
    
    if Assigned(FCurrentBuddy) then
    begin
      BuddyCount := FContactManager.GetBuddyCount;
      for I := 0 to BuddyCount - 1 do
      begin
        Buddy := FContactManager.GetBuddyByIndex(I);
        if Pos(Buddy.Nick, TabCaption) > 0 then
        begin
          if FCurrentBuddy.JID = Buddy.JID then
          begin
            FCurrentBuddy := nil;
            ButtonSend.Enabled := False;
            MemoMessage.Enabled := False;
          end;
          Break;
        end;
      end;
    end;
    
    FChatManager.CloseTab(TabSheet);
    LogMessage('Closed chat tab: ' + TabCaption);
  end;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
begin
  if Assigned(FClient) then
  begin
    try
      FClient.Process;
    except
      on E: Exception do
        LogMessage('Process error: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
var
  TabSheet: TTabSheet;
begin
  FChatManager.SaveMessageToHistory(Buddy.JID, Buddy.Nick, MessageText, True);
  
  TabSheet := FChatManager.GetOrCreateTab(Buddy.Nick, Buddy.JID);
  UpdateTabCaption(Buddy.Nick, Buddy.Status, False);
  FChatManager.AddMessageToTab(TabSheet, Buddy.Nick, MessageText, True);
  
  if not Assigned(FCurrentBuddy) or (FCurrentBuddy.JID <> Buddy.JID) then
    LogMessage('Message from ' + Buddy.Nick);
end;

procedure TFormMain.OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
var
  TabSheet: TTabSheet;
begin
  UpdateBuddyList;
  
  TabSheet := FindTabByBuddyNick(Buddy.Nick);
  if Assigned(TabSheet) then
    UpdateTabCaption(Buddy.Nick, NewStatus, False);
  
  if NewStatus <> bsOffline then
    LogMessage('Buddy ' + Buddy.Nick + ': ' + StatusToDisplayString(NewStatus))
  else
    LogMessage('Buddy offline: ' + Buddy.Nick);
end;

procedure TFormMain.OnTypingNotification(Buddy: TBarevBuddy; IsTyping: Boolean);
begin
  UpdateTabCaption(Buddy.Nick, Buddy.Status, IsTyping);
end;

procedure TFormMain.OnLog(const LogLevel, Message: string);
begin
  MemoLog.Lines.Add('[' + LogLevel + '] ' + Message);
end;

procedure TFormMain.UpdateBuddyList;
var
  I: Integer;
  Buddy: TBarevBuddy;
  BuddyCount: Integer;
  DisplayText: string;
begin
  ListBoxBuddies.Clear;
  
  if not Assigned(FContactManager) then
  begin
    LabelBuddies.Caption := 'Buddies (0)';
    Exit;
  end;
  
  BuddyCount := FContactManager.GetBuddyCount;
  LabelBuddies.Caption := 'Buddies (' + IntToStr(BuddyCount) + ')';
  
  for I := 0 to BuddyCount - 1 do
  begin
    Buddy := FContactManager.GetBuddyByIndex(I);
    DisplayText := StatusToIcon(Buddy.Status) + ' ' + Buddy.Nick;
    ListBoxBuddies.Items.Add(DisplayText);
  end;
end;

procedure TFormMain.UpdateMyAvatarDisplay;
var
  AvatarPath: string;
begin
  if not Assigned(FAvatarManager) then
    Exit;
  
  AvatarPath := FAvatarManager.GetMyAvatarPath;
  if (AvatarPath <> '') and FileExists(AvatarPath) then
  begin
    try
      ImageAvatar.Picture.LoadFromFile(AvatarPath);
    except
      DrawDefaultAvatar;
    end;
  end
  else
    DrawDefaultAvatar;
end;

end.
