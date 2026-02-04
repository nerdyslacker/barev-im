unit BarevIMForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, LCLType, LCLIntf, Barev, BarevTypes,
  BarevIMTypes, BarevIMChat, BarevIMAddBuddyDlg, BarevIMLoginDlg;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonDisconnect: TButton;
    ButtonToggleLogs: TButton;
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
    procedure ButtonToggleLogsClick(Sender: TObject);
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
    procedure ListBoxBuddiesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBoxBuddiesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ListBoxBuddiesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PageControlChatsChange(Sender: TObject);
    procedure PageControlChatsCloseTabClicked(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FClient: TBarevClient;
    FConnected: Boolean;
    FCurrentBuddy: TBarevBuddy;
    FFirstShow: Boolean;
    FRightClickIndex: Integer;
    FDefaultAvatar: TBitmap;
    FConfigPath: string;
    
    FWindowLeft: Integer;
    FWindowTop: Integer;
    FWindowWidth: Integer;
    FWindowHeight: Integer;

    FChatManager: TChatTabManager;

    procedure OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure OnTypingNotification(Buddy: TBarevBuddy; IsTyping: Boolean);
    procedure OnLog(const LogLevel, Message: string);

    function DoConnect(const ANick, AIPv6: string; APort: Integer): Boolean;
    procedure DoDisconnect;
    procedure ShowLoginDialog;
    
    procedure LoadWindowSettings;
    procedure SaveWindowSettings;

    procedure UpdateBuddyList;
    procedure UpdateMyAvatarDisplay;
    procedure UpdateUserInfoLabel;
    procedure SetConnectedState(Connected: Boolean);
    procedure LogMessage(const Message: string);
    procedure CreateDefaultAvatar;
    function GetBuddyNickFromListIndex(Index: Integer): string;
    procedure UpdateTabCaption(const BuddyNick: string; Status: TBuddyStatus; IsTyping: Boolean);
    function FindTabByBuddyNick(const BuddyNick: string): TTabSheet;
    procedure OpenChatWithBuddy(Index: Integer);
    function GetBuddyAvatar(Buddy: TBarevBuddy): TBitmap;
    function FindBuddyByNick(const Nick: string): TBarevBuddy;
  public
  end;

var
  FormMain: TFormMain;

implementation

uses
  IniFiles;

{$R *.lfm}

const
  BUDDY_ITEM_HEIGHT = 40;
  AVATAR_SIZE = 32;

function DetermineConfigFilePath: string;
var
  I: Integer;
  ConfigParam: string;
begin
  ConfigParam := '';

  for I := 1 to ParamCount do
  begin
    if Pos('--config=', ParamStr(I)) = 1 then
    begin
      ConfigParam := Copy(ParamStr(I), 10, Length(ParamStr(I)));
      Break;
    end;
  end;
  
  if ConfigParam <> '' then
  begin
    Result := ConfigParam;
    ForceDirectories(ExtractFileDir(Result));
  end
  else
  begin
    Result := GetUserDir + '.barev' + PathDelim + 'barev.ini';
    if not DirectoryExists(GetUserDir + '.barev') then
      ForceDirectories(GetUserDir + '.barev');
  end;
end;

{ TFormMain }

procedure TFormMain.CreateDefaultAvatar;
var
  CenterX, CenterY: Integer;
begin
  FDefaultAvatar := TBitmap.Create;
  FDefaultAvatar.Width := AVATAR_SIZE;
  FDefaultAvatar.Height := AVATAR_SIZE;
  
  FDefaultAvatar.Canvas.Brush.Color := clSilver;
  FDefaultAvatar.Canvas.FillRect(0, 0, AVATAR_SIZE, AVATAR_SIZE);
  
  CenterX := AVATAR_SIZE div 2;
  CenterY := AVATAR_SIZE div 2;
  
  FDefaultAvatar.Canvas.Brush.Color := clGray;
  FDefaultAvatar.Canvas.Pen.Color := clGray;
  FDefaultAvatar.Canvas.Ellipse(CenterX - 5, 4, CenterX + 5, 14);
  FDefaultAvatar.Canvas.Ellipse(CenterX - 10, 16, CenterX + 10, 38);
  
  FDefaultAvatar.Canvas.Brush.Style := bsClear;
  FDefaultAvatar.Canvas.Pen.Color := clDkGray;
  FDefaultAvatar.Canvas.Pen.Width := 1;
  FDefaultAvatar.Canvas.Rectangle(0, 0, AVATAR_SIZE, AVATAR_SIZE);
end;

function TFormMain.GetBuddyAvatar(Buddy: TBarevBuddy): TBitmap;
var
  TempPic: TPicture;
begin
  Result := FDefaultAvatar;
  
  if not Assigned(Buddy) then
    Exit;
  
  if (Buddy.AvatarPath <> '') and FileExists(Buddy.AvatarPath) then
  begin
    TempPic := TPicture.Create;
    try
      try
        TempPic.LoadFromFile(Buddy.AvatarPath);
        Result := TBitmap.Create;
        Result.Width := AVATAR_SIZE;
        Result.Height := AVATAR_SIZE;
        Result.Canvas.StretchDraw(Rect(0, 0, AVATAR_SIZE, AVATAR_SIZE), TempPic.Graphic);
      except
        Result := FDefaultAvatar;
      end;
    finally
      TempPic.Free;
    end;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FConnected := False;
  FClient := nil;
  FCurrentBuddy := nil;
  FFirstShow := True;
  FRightClickIndex := -1;
  
  FWindowLeft := 100;
  FWindowTop := 100;
  FWindowWidth := 800;
  FWindowHeight := 600;

  CreateDefaultAvatar;
  
  FConfigPath := DetermineConfigFilePath;

  FChatManager := TChatTabManager.Create(PageControlChats);

  Caption := 'barev IM';
  Position := poScreenCenter;

  OpenDialogAvatar.Title := 'Select Avatar Image';
  OpenDialogAvatar.Filter := 'Image Files|*.png;*.jpg;*.jpeg;*.gif;*.bmp|All Files|*.*';

  LoadWindowSettings;

  Left := FWindowLeft;
  Top := FWindowTop;
  Width := FWindowWidth;
  Height := FWindowHeight;
  
  MemoLog.ReadOnly := True;
  MemoLog.Visible := False;
  ButtonToggleLogs.Caption := 'Show Logs';
  
  ListBoxBuddies.ShowHint := True;
  ListBoxBuddies.Style := lbOwnerDrawFixed;
  ListBoxBuddies.ItemHeight := BUDDY_ITEM_HEIGHT;

  ImageAvatar.Picture.Bitmap.Assign(FDefaultAvatar);

  SetConnectedState(False);
  
  LogMessage('Configuration file: ' + FConfigPath);
end;

procedure TFormMain.LoadWindowSettings;
var
  Ini: TIniFile;
begin
  if not FileExists(FConfigPath) then
    Exit;
    
  Ini := TIniFile.Create(FConfigPath);
  try
    FWindowLeft := Ini.ReadInteger('Window', 'Left', 100);
    FWindowTop := Ini.ReadInteger('Window', 'Top', 100);
    FWindowWidth := Ini.ReadInteger('Window', 'Width', 800);
    FWindowHeight := Ini.ReadInteger('Window', 'Height', 600);
  finally
    Ini.Free;
  end;
end;

procedure TFormMain.SaveWindowSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FConfigPath);
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);
  finally
    Ini.Free;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  Nick, IPv6: string;
  Port: Integer;
  AutoConnect: Boolean;
  Ini: TIniFile;
begin
  if FFirstShow then
  begin
    FFirstShow := False;
    Application.ProcessMessages;
    
    Nick := '';
    IPv6 := '';
    Port := 5299;
    AutoConnect := False;
    
    if FileExists(FConfigPath) then
    begin
      Ini := TIniFile.Create(FConfigPath);
      try
        Nick := Ini.ReadString('User', 'Nick', '');
        IPv6 := Ini.ReadString('User', 'IPv6', '');
        Port := Ini.ReadInteger('User', 'Port', 5299);
        AutoConnect := Ini.ReadBool('Window', 'AutoConnect', False);
      finally
        Ini.Free;
      end;
    end;
    
    if AutoConnect and (Nick <> '') and (IPv6 <> '') then
    begin
      LogMessage('Auto-connecting...');
      if not DoConnect(Nick, IPv6, Port) then
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

  SaveWindowSettings;

  FDefaultAvatar.Free;
  FChatManager.Free;
end;

procedure TFormMain.ShowLoginDialog;
var
  Nick, IPv6: string;
  Port: Integer;
  AutoConnect: Boolean;
  Ini: TIniFile;
begin
  Nick := '';
  IPv6 := '';
  Port := 5299;
  AutoConnect := False;
  
  if FileExists(FConfigPath) then
  begin
    Ini := TIniFile.Create(FConfigPath);
    try
      Nick := Ini.ReadString('User', 'Nick', '');
      IPv6 := Ini.ReadString('User', 'IPv6', '');
      Port := Ini.ReadInteger('User', 'Port', 5299);
      AutoConnect := Ini.ReadBool('Window', 'AutoConnect', False);
    finally
      Ini.Free;
    end;
  end;
  
  if TLoginDialog.Execute(Nick, IPv6, Port, AutoConnect) then
  begin
    Ini := TIniFile.Create(FConfigPath);
    try
      Ini.WriteString('User', 'Nick', Nick);
      Ini.WriteString('User', 'IPv6', IPv6);
      Ini.WriteInteger('User', 'Port', Port);
      Ini.WriteBool('Window', 'AutoConnect', AutoConnect);
    finally
      Ini.Free;
    end;

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

      FClient.LoadConfig(FConfigPath);
      
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
  if Assigned(FClient) then
  begin
    FClient.SaveConfig;
    FClient.Stop;
    FreeAndNil(FClient);
  end;
  
  FCurrentBuddy := nil;
  SetConnectedState(False);
  
  ListBoxBuddies.Clear;
  FChatManager.CloseAllTabs;
  FChatManager.ClearAllHistory;
  
  ImageAvatar.Picture.Bitmap.Assign(FDefaultAvatar);
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

procedure TFormMain.ButtonToggleLogsClick(Sender: TObject);
begin
  MemoLog.Visible := not MemoLog.Visible;
  if MemoLog.Visible then
    ButtonToggleLogs.Caption := 'Hide Logs'
  else
    ButtonToggleLogs.Caption := 'Show Logs';
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
      FClient.AddBuddy(BuddyNick, BuddyIPv6, BuddyPort);
      FClient.SaveConfig;
      UpdateBuddyList;
      LogMessage('Added buddy: ' + BuddyNick);
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
  Buddy: TBarevBuddy;
begin
  Result := '';
  if not Assigned(FClient) then
    Exit;
  if (Index < 0) or (Index >= FClient.GetBuddyCount) then
    Exit;
  
  Buddy := FClient.GetBuddyByIndex(Index);
  if Assigned(Buddy) then
    Result := Buddy.Nick;
end;

function TFormMain.FindBuddyByNick(const Nick: string): TBarevBuddy;
var
  I: Integer;
  Buddy: TBarevBuddy;
begin
  Result := nil;
  if not Assigned(FClient) then
    Exit;
    
  for I := 0 to FClient.GetBuddyCount - 1 do
  begin
    Buddy := FClient.GetBuddyByIndex(I);
    if Assigned(Buddy) and (Buddy.Nick = Nick) then
    begin
      Result := Buddy;
      Exit;
    end;
  end;
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
  Buddy := FindBuddyByNick(BuddyNick);
  
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
  
  Buddy := FindBuddyByNick(BuddyNick);
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
    
    if FClient.RemoveBuddy(Buddy.JID) then
    begin
      FClient.SaveConfig;
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
  Buddy := FindBuddyByNick(BuddyNick);
  
  if Assigned(Buddy) then
  begin
    FClient.RequestBuddyAvatar(Buddy.JID);
    LogMessage('Requested avatar from ' + BuddyNick);
  end;
end;

procedure TFormMain.MenuItemSetAvatarClick(Sender: TObject);
begin
  if not FConnected then
    Exit;
  
  if OpenDialogAvatar.Execute then
  begin
    if FClient.LoadMyAvatar(OpenDialogAvatar.FileName) then
    begin
      FClient.SaveConfig;
      UpdateMyAvatarDisplay;
      LogMessage('Avatar set: ' + OpenDialogAvatar.FileName);
    end;
  end;
end;

procedure TFormMain.MenuItemClearAvatarClick(Sender: TObject);
begin
  if not FConnected then
    Exit;
  
  FClient.ClearMyAvatar;
  FClient.SaveConfig;
  ImageAvatar.Picture.Bitmap.Assign(FDefaultAvatar);
  LogMessage('Avatar cleared');
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
    FChatManager.AddMessageToTab(TabSheet, FClient.Nick, MessageText, False);
    FChatManager.SaveMessageToHistory(FCurrentBuddy.JID, FClient.Nick, MessageText, False);
    
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

procedure TFormMain.ListBoxBuddiesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  Buddy: TBarevBuddy;
  StatusIcon: string;
  TextX, TextY, AvatarX, AvatarY: Integer;
  AvatarRect: TRect;
  TempBmp: TBitmap;
  TempPic: TPicture;
begin
  if not Assigned(FClient) then
    Exit;
  if (Index < 0) or (Index >= FClient.GetBuddyCount) then
    Exit;
    
  Buddy := FClient.GetBuddyByIndex(Index);
  if not Assigned(Buddy) then
    Exit;

  with ListBoxBuddies.Canvas do
  begin
    if odSelected in State then
    begin
      Brush.Color := clHighlight;
      Font.Color := clHighlightText;
    end
    else
    begin
      Brush.Color := clWindow;
      Font.Color := clWindowText;
    end;
    
    FillRect(ARect);
    
    StatusIcon := StatusToIcon(Buddy.Status);
    TextX := ARect.Left + 4;
    TextY := ARect.Top + (ARect.Height - TextHeight(StatusIcon)) div 2;
    TextOut(TextX, TextY, StatusIcon + ' ' + Buddy.Nick);
    
    AvatarX := ARect.Right - AVATAR_SIZE - 4;
    AvatarY := ARect.Top + (ARect.Height - AVATAR_SIZE) div 2;
    AvatarRect := Rect(AvatarX, AvatarY, AvatarX + AVATAR_SIZE, AvatarY + AVATAR_SIZE);
    
    if (Buddy.AvatarPath <> '') and FileExists(Buddy.AvatarPath) then
    begin
      TempPic := TPicture.Create;
      try
        try
          TempPic.LoadFromFile(Buddy.AvatarPath);
          StretchDraw(AvatarRect, TempPic.Graphic);
        except
          StretchDraw(AvatarRect, FDefaultAvatar);
        end;
      finally
        TempPic.Free;
      end;
    end
    else
      StretchDraw(AvatarRect, FDefaultAvatar);
    
    Pen.Color := clGray;
    Brush.Style := bsClear;
    Rectangle(AvatarRect);
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
  Buddy: TBarevBuddy;
begin
  Index := ListBoxBuddies.ItemAtPos(Point(X, Y), True);
  if Index >= 0 then
  begin
    Buddy := FClient.GetBuddyByIndex(Index);
    if Assigned(Buddy) then
      ListBoxBuddies.Hint := Buddy.Nick + ' - ' + StatusToDisplayString(Buddy.Status)
    else
      ListBoxBuddies.Hint := '';
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

procedure TFormMain.PageControlChatsChange(Sender: TObject);
var
  TabSheet: TTabSheet;
  TabCaption: string;
  I: Integer;
  Buddy: TBarevBuddy;
begin
  TabSheet := PageControlChats.ActivePage;
  if not Assigned(TabSheet) then
  begin
    FCurrentBuddy := nil;
    Exit;
  end;
  
  TabCaption := TabSheet.Caption;
  
  if Assigned(FClient) then
  begin
    for I := 0 to FClient.GetBuddyCount - 1 do
    begin
      Buddy := FClient.GetBuddyByIndex(I);
      if Assigned(Buddy) and (Pos(Buddy.Nick, TabCaption) > 0) then
      begin
        FCurrentBuddy := Buddy;
        ButtonSend.Enabled := True;
        MemoMessage.Enabled := True;
        Exit;
      end;
    end;
  end;
  
  FCurrentBuddy := nil;
end;

procedure TFormMain.PageControlChatsCloseTabClicked(Sender: TObject);
var
  TabSheet: TTabSheet;
  TabCaption: string;
  I: Integer;
  Buddy: TBarevBuddy;
begin
  if PageControlChats.ActivePage <> nil then
  begin
    TabSheet := PageControlChats.ActivePage;
    TabCaption := TabSheet.Caption;
    
    if Assigned(FCurrentBuddy) and Assigned(FClient) then
    begin
      for I := 0 to FClient.GetBuddyCount - 1 do
      begin
        Buddy := FClient.GetBuddyByIndex(I);
        if Assigned(Buddy) and (Pos(Buddy.Nick, TabCaption) > 0) then
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
  BuddyCount: Integer;
begin
  ListBoxBuddies.Items.BeginUpdate;
  try
    ListBoxBuddies.Clear;
    
    if not Assigned(FClient) then
    begin
      LabelBuddies.Caption := 'Buddies (0)';
      Exit;
    end;
    
    BuddyCount := FClient.GetBuddyCount;
    LabelBuddies.Caption := 'Buddies (' + IntToStr(BuddyCount) + ')';
    
    for I := 0 to BuddyCount - 1 do
      ListBoxBuddies.Items.Add(IntToStr(I));
  finally
    ListBoxBuddies.Items.EndUpdate;
  end;
end;

procedure TFormMain.UpdateMyAvatarDisplay;
var
  AvatarPath: string;
begin
  if not Assigned(FClient) or not Assigned(FClient.AvatarManager) then
    Exit;
  
  AvatarPath := FClient.AvatarManager.MyAvatarPath;
  if (AvatarPath <> '') and FileExists(AvatarPath) then
  begin
    try
      ImageAvatar.Picture.LoadFromFile(AvatarPath);
    except
      ImageAvatar.Picture.Bitmap.Assign(FDefaultAvatar);
    end;
  end
  else
    ImageAvatar.Picture.Bitmap.Assign(FDefaultAvatar);
end;

end.
