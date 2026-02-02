unit BarevIMForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, Barev, BarevTypes,
  BarevIMTypes, BarevIMConfig, BarevIMChat, BarevIMContacts;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonConnect: TButton;
    ButtonSend: TButton;
    ButtonAddBuddy: TButton;
    EditMessage: TEdit;
    EditMyNick: TEdit;
    EditMyIPv6: TEdit;
    EditMyPort: TEdit;
    LabelMyNick: TLabel;
    LabelMyIPv6: TLabel;
    LabelMyPort: TLabel;
    LabelBuddies: TLabel;
    ListBoxBuddies: TListBox;
    MemoLog: TMemo;
    PageControlChats: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Timer1: TTimer;
    PopupMenuBuddy: TPopupMenu;
    MenuItemRemoveBuddy: TMenuItem;
    
    procedure ButtonAddBuddyClick(Sender: TObject);
    procedure MenuItemRemoveBuddyClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure EditMessageKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxBuddiesDblClick(Sender: TObject);
    procedure PageControlChatsCloseTabClicked(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FClient: TBarevClient;
    FConnected: Boolean;
    FCurrentBuddy: TBarevBuddy;

    FConfig: TBarevConfig;
    FChatManager: TChatTabManager;
    FContactManager: TContactManager;

    procedure OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure OnLog(const LogLevel, Message: string);

    procedure OnContactLog(const Message: string);

    procedure UpdateBuddyList;
    procedure ApplyConfigToUI;
    procedure ApplyUIToConfig;
    procedure SetConnectedState(Connected: Boolean);
    procedure LogMessage(const Message: string);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  ConfigPath: string;
begin
  FConnected := False;
  FClient := nil;
  FCurrentBuddy := nil;

  ConfigPath := DetermineConfigFilePath;
  FConfig := TBarevConfig.Create(ConfigPath);
  FChatManager := TChatTabManager.Create(PageControlChats);
  FContactManager := nil;

  Caption := 'barev IM';
  Position := poScreenCenter;
  Width := 800;
  Height := 600;

  FConfig.Load;
  ApplyConfigToUI;
  
  MemoLog.ReadOnly := True;
  SetConnectedState(False);
  
  LogMessage('Configuration file: ' + FConfig.ConfigFile);

  if FConfig.IsValidForAutoConnect then
  begin
    LogMessage('Auto-connecting...');
    Application.ProcessMessages;
    Sleep(100);
    ButtonConnectClick(nil);
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FClient) then
  begin
    if Assigned(FContactManager) then
      FContactManager.SaveContacts;
    FClient.Stop;
    FClient.Free;
  end;

  ApplyUIToConfig;
  FConfig.Save;

  FContactManager.Free;
  FChatManager.Free;
  FConfig.Free;
end;

procedure TFormMain.ApplyConfigToUI;
begin
  EditMyNick.Text := FConfig.Nick;
  EditMyIPv6.Text := FConfig.IPv6;
  EditMyPort.Text := IntToStr(FConfig.Port);
  
  Left := FConfig.WindowLeft;
  Top := FConfig.WindowTop;
  Width := FConfig.WindowWidth;
  Height := FConfig.WindowHeight;
end;

procedure TFormMain.ApplyUIToConfig;
var
  Port: Integer;
begin
  FConfig.Nick := EditMyNick.Text;
  FConfig.IPv6 := EditMyIPv6.Text;
  
  if TryStrToInt(Trim(EditMyPort.Text), Port) then
    FConfig.Port := Port
  else
    FConfig.Port := DEFAULT_PORT;
  
  FConfig.WindowLeft := Left;
  FConfig.WindowTop := Top;
  FConfig.WindowWidth := Width;
  FConfig.WindowHeight := Height;
end;

procedure TFormMain.SetConnectedState(Connected: Boolean);
begin
  FConnected := Connected;
  
  if Connected then
  begin
    ButtonConnect.Caption := 'Disconnect';
    EditMyNick.Enabled := False;
    EditMyIPv6.Enabled := False;
    EditMyPort.Enabled := False;
    ButtonAddBuddy.Enabled := True;
    Timer1.Enabled := True;
  end
  else
  begin
    ButtonConnect.Caption := 'Connect';
    EditMyNick.Enabled := True;
    EditMyIPv6.Enabled := True;
    EditMyPort.Enabled := True;
    ButtonAddBuddy.Enabled := False;
    ButtonSend.Enabled := False;
    EditMessage.Enabled := False;
    Timer1.Enabled := False;
  end;
end;

procedure TFormMain.LogMessage(const Message: string);
begin
  MemoLog.Lines.Add(Message);
end;

procedure TFormMain.OnContactLog(const Message: string);
begin
  LogMessage(Message);
end;

procedure TFormMain.ButtonConnectClick(Sender: TObject);
var
  Port: Integer;
begin
  if not FConnected then
  begin
    if Trim(EditMyNick.Text) = '' then
    begin
      ShowMessage('Please enter your nickname');
      EditMyNick.SetFocus;
      Exit;
    end;
    
    if Trim(EditMyIPv6.Text) = '' then
    begin
      ShowMessage('Please enter your IPv6 address');
      EditMyIPv6.SetFocus;
      Exit;
    end;
    
    if not TryStrToInt(Trim(EditMyPort.Text), Port) then
    begin
      ShowMessage('Invalid port number');
      EditMyPort.SetFocus;
      Exit;
    end;
    
    if (Port < 1) or (Port > 65535) then
    begin
      ShowMessage('Port must be between 1 and 65535');
      EditMyPort.SetFocus;
      Exit;
    end;
    
    try
      FClient := TBarevClient.Create(EditMyNick.Text, EditMyIPv6.Text, Port);
      FClient.OnMessageReceived := @OnMessageReceived;
      FClient.OnBuddyStatus := @OnBuddyStatus;
      FClient.OnLog := @OnLog;
      
      if FClient.Start then
      begin
        SetConnectedState(True);
        LogMessage('=== Connected on port ' + IntToStr(Port) + ' ===');

        FContactManager := TContactManager.Create(FClient, FConfig.GetContactsFilePath);
        FContactManager.OnLog := @OnContactLog;
        FContactManager.LoadContacts;
        
        UpdateBuddyList;
      end
      else
      begin
        ShowMessage('Failed to start Barev client. Port ' + IntToStr(Port) + ' may be in use.');
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
  end
  else
  begin
    if Assigned(FContactManager) then
    begin
      FContactManager.SaveContacts;
      FContactManager.Free;
      FContactManager := nil;
    end;
    
    if Assigned(FClient) then
    begin
      FClient.Stop;
      FClient.Free;
      FClient := nil;
    end;
    
    FCurrentBuddy := nil;
    SetConnectedState(False);
    
    ListBoxBuddies.Clear;
    FChatManager.CloseAllTabs;
    FChatManager.ClearAllHistory;
    MemoLog.Clear;
    
    LogMessage('=== Disconnected ===');
  end;
end;

procedure TFormMain.ButtonAddBuddyClick(Sender: TObject);
var
  BuddyNick, BuddyIPv6, BuddyPortStr: string;
  BuddyPort: Integer;
begin
  if not FConnected then
    Exit;
  
  BuddyNick := '';
  BuddyIPv6 := '';
  BuddyPortStr := IntToStr(DEFAULT_PORT);
  
  if InputQuery('Add Buddy', 'Enter buddy nickname:', BuddyNick) then
  begin
    if Trim(BuddyNick) = '' then
      Exit;
    
    if InputQuery('Add Buddy', 'Enter buddy IPv6 address:', BuddyIPv6) then
    begin
      if Trim(BuddyIPv6) = '' then
        Exit;
      
      if InputQuery('Add Buddy', 'Enter buddy port (default ' + IntToStr(DEFAULT_PORT) + '):', BuddyPortStr) then
      begin
        if not TryStrToInt(Trim(BuddyPortStr), BuddyPort) then
          BuddyPort := DEFAULT_PORT;
        
        if (BuddyPort < 1) or (BuddyPort > 65535) then
          BuddyPort := DEFAULT_PORT;
        
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
  end;
end;

procedure TFormMain.MenuItemRemoveBuddyClick(Sender: TObject);
var
  Index: Integer;
  BuddyNick: string;
  Buddy: TBarevBuddy;
  TabSheet: TTabSheet;
  I: Integer;
begin
  if not FConnected then
    Exit;
  
  Index := ListBoxBuddies.ItemIndex;
  if Index < 0 then
  begin
    ShowMessage('Please select a buddy to remove');
    Exit;
  end;
  
  BuddyNick := Copy(ListBoxBuddies.Items[Index], 1, Pos(' ', ListBoxBuddies.Items[Index]) - 1);
  
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
      EditMessage.Enabled := False;
    end;

    for I := 0 to PageControlChats.PageCount - 1 do
    begin
      if PageControlChats.Pages[I].Caption = BuddyNick then
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

procedure TFormMain.ButtonSendClick(Sender: TObject);
var
  MessageText: string;
  TabSheet: TTabSheet;
begin
  if not Assigned(FCurrentBuddy) then
    Exit;
  
  MessageText := Trim(EditMessage.Text);
  if MessageText = '' then
    Exit;
  
  try
    FClient.SendMessage(FCurrentBuddy.JID, MessageText);

    TabSheet := FChatManager.GetOrCreateTab(FCurrentBuddy.Nick, FCurrentBuddy.JID);
    FChatManager.AddMessageToTab(TabSheet, EditMyNick.Text, MessageText, False);
    FChatManager.SaveMessageToHistory(FCurrentBuddy.JID, EditMyNick.Text, MessageText, False);
    
    EditMessage.Clear;
    EditMessage.SetFocus;
  except
    on E: Exception do
      ShowMessage('Error sending message: ' + E.Message);
  end;
end;

procedure TFormMain.EditMessageKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ButtonSendClick(Sender);
  end;
end;

procedure TFormMain.ListBoxBuddiesDblClick(Sender: TObject);
var
  Index: Integer;
  BuddyNick: string;
  Buddy: TBarevBuddy;
  TabSheet: TTabSheet;
begin
  Index := ListBoxBuddies.ItemIndex;
  if Index < 0 then
    Exit;
  
  BuddyNick := Copy(ListBoxBuddies.Items[Index], 1, Pos(' ', ListBoxBuddies.Items[Index]) - 1);
  Buddy := FContactManager.FindBuddyByNick(BuddyNick);
  
  if Assigned(Buddy) then
  begin
    FCurrentBuddy := Buddy;

    TabSheet := FChatManager.GetOrCreateTab(Buddy.Nick, Buddy.JID);
    PageControlChats.ActivePage := TabSheet;
    
    LogMessage('Opened chat with: ' + FCurrentBuddy.Nick);
    
    try
      FClient.ConnectToBuddy(FCurrentBuddy.JID);
      
      ButtonSend.Enabled := True;
      EditMessage.Enabled := True;
      EditMessage.SetFocus;
      
      LogMessage('Connecting to: ' + FCurrentBuddy.Nick);
    except
      on E: Exception do
        ShowMessage('Error connecting to buddy: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.PageControlChatsCloseTabClicked(Sender: TObject);
var
  TabSheet: TTabSheet;
  TabCaption: string;
begin
  if PageControlChats.ActivePage <> nil then
  begin
    TabSheet := PageControlChats.ActivePage;
    TabCaption := TabSheet.Caption;

    if Assigned(FCurrentBuddy) and (TabCaption = FCurrentBuddy.Nick) then
    begin
      FCurrentBuddy := nil;
      ButtonSend.Enabled := False;
      EditMessage.Enabled := False;
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
  FChatManager.AddMessageToTab(TabSheet, Buddy.Nick, MessageText, True);

  if not Assigned(FCurrentBuddy) or (FCurrentBuddy.JID <> Buddy.JID) then
    LogMessage('Message from ' + Buddy.Nick);
end;

procedure TFormMain.OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
begin
  UpdateBuddyList;
  
  if NewStatus <> bsOffline then
    LogMessage('Buddy online: ' + Buddy.Nick)
  else
    LogMessage('Buddy offline: ' + Buddy.Nick);
end;

procedure TFormMain.OnLog(const LogLevel, Message: string);
begin
  MemoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + LogLevel + ': ' + Message);
end;

procedure TFormMain.UpdateBuddyList;
var
  I: Integer;
  Status: string;
  Buddy: TBarevBuddy;
  BuddyCount: Integer;
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
    if Buddy.Status <> bsOffline then
      Status := '[Online]'
    else
      Status := '[Offline]';
    
    ListBoxBuddies.Items.Add(Buddy.Nick + ' ' + Status);
  end;
end;

end.
