unit BarevIMForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  Menus, LCLType, Barev, BarevTypes, BarevIMTypes, BarevIMConfig, BarevIMChat,
  BarevIMContacts, BarevIMAddBuddyDlg, BarevIMFileTransfer, BarevIMAvatar;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonConnect: TButton;
    ButtonSend: TButton;
    ButtonAddBuddy: TButton;
    ButtonSendFile: TButton;
    MemoMessage: TMemo;
    EditMyNick: TEdit;
    EditMyIPv6: TEdit;
    EditMyPort: TEdit;
    ImageAvatar: TImage;
    LabelMyNick: TLabel;
    LabelMyIPv6: TLabel;
    LabelMyPort: TLabel;
    LabelBuddies: TLabel;
    ListBoxBuddies: TListBox;
    MemoLog: TMemo;
    OpenDialogFile: TOpenDialog;
    OpenDialogAvatar: TOpenDialog;
    SaveDialogFile: TSaveDialog;
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
    MenuItemRemoveBuddy: TMenuItem;
    MenuItemSeparator1: TMenuItem;
    MenuItemRequestAvatar: TMenuItem;
    
    procedure ButtonAddBuddyClick(Sender: TObject);
    procedure ButtonSendFileClick(Sender: TObject);
    procedure MenuItemRemoveBuddyClick(Sender: TObject);
    procedure MenuItemRequestAvatarClick(Sender: TObject);
    procedure MenuItemSetAvatarClick(Sender: TObject);
    procedure MenuItemClearAvatarClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure MemoMessageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageAvatarClick(Sender: TObject);
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
    FFileTransferManager: TFileTransferManager;
    FAvatarManager: TAvatarManager;

    procedure OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure OnTypingNotification(Buddy: TBarevBuddy; IsTyping: Boolean);
    procedure OnLog(const LogLevel, Message: string);

    procedure OnFTOffer(Buddy: TBarevBuddy; const Sid, FileName: string; FileSize: Int64);
    procedure OnFTProgress(Buddy: TBarevBuddy; const Sid: string; BytesDone, BytesTotal: Int64);
    procedure OnFTComplete(Buddy: TBarevBuddy; const Sid, LocalPath: string);
    procedure OnFTError(Buddy: TBarevBuddy; const Sid, ErrMsg: string);

    procedure OnContactLog(const Message: string);
    procedure OnFileTransferLog(const Message: string);
    procedure OnAvatarLog(const Message: string);

    procedure UpdateBuddyList;
    procedure UpdateMyAvatarDisplay;
    procedure ApplyConfigToUI;
    procedure ApplyUIToConfig;
    procedure SetConnectedState(Connected: Boolean);
    procedure LogMessage(const Message: string);
    procedure ShowFileOffer(const BuddyNick, FileName: string; FileSize: Int64; const Sid: string);
    procedure DrawDefaultAvatar;
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
  CenterX, CenterY, Radius: Integer;
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

  ConfigPath := DetermineConfigFilePath;
  FConfig := TBarevConfig.Create(ConfigPath);
  FChatManager := TChatTabManager.Create(PageControlChats);
  FContactManager := nil;
  FFileTransferManager := nil;
  FAvatarManager := nil;

  Caption := 'barev IM';
  Position := poScreenCenter;
  Width := 800;
  Height := 600;

  DrawDefaultAvatar;

  OpenDialogFile.Title := 'Select File to Send';
  OpenDialogFile.Filter := 'All Files|*.*';
  
  OpenDialogAvatar.Title := 'Select Avatar Image';
  OpenDialogAvatar.Filter := 'Image Files|*.png;*.jpg;*.jpeg;*.gif;*.bmp|All Files|*.*';
  
  SaveDialogFile.Title := 'Save File As';

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

  FAvatarManager.Free;
  FFileTransferManager.Free;
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
    ButtonSendFile.Enabled := False;
    MemoMessage.Enabled := False;
    Timer1.Enabled := False;
  end;
end;

procedure TFormMain.LogMessage(const Message: string);
begin
  MemoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + Message);
end;

procedure TFormMain.OnContactLog(const Message: string);
begin
  LogMessage(Message);
end;

procedure TFormMain.OnFileTransferLog(const Message: string);
begin
  LogMessage('[FT] ' + Message);
end;

procedure TFormMain.OnAvatarLog(const Message: string);
begin
  LogMessage('[Avatar] ' + Message);
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
      FClient.OnTypingNotification := @OnTypingNotification;
      FClient.OnLog := @OnLog;
      
      if FClient.Start then
      begin
        SetConnectedState(True);
        LogMessage('=== Connected as ' + FClient.MyJID + ' on port ' + IntToStr(Port) + ' ===');

        FContactManager := TContactManager.Create(FClient, FConfig.GetContactsFilePath);
        FContactManager.OnLog := @OnContactLog;
        FContactManager.LoadContacts;

        FFileTransferManager := TFileTransferManager.Create(FClient);
        FFileTransferManager.OnLog := @OnFileTransferLog;

        if Assigned(FClient.FileTransfer) then
        begin
          FClient.FileTransfer.OnFileOffer := @OnFTOffer;
          FClient.FileTransfer.OnProgress := @OnFTProgress;
          FClient.FileTransfer.OnComplete := @OnFTComplete;
          FClient.FileTransfer.OnError := @OnFTError;
        end;

        FAvatarManager := TAvatarManager.Create(FClient);
        FAvatarManager.OnLog := @OnAvatarLog;
        
        UpdateBuddyList;
        UpdateMyAvatarDisplay;
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
      FreeAndNil(FContactManager);
    end;
    
    FreeAndNil(FAvatarManager);
    FreeAndNil(FFileTransferManager);
    
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
    MemoLog.Clear;

    ImageAvatar.Picture.Clear;
    
    LogMessage('=== Disconnected ===');
  end;
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

procedure TFormMain.ButtonSendFileClick(Sender: TObject);
var
  Sid: string;
begin
  if not FConnected or not Assigned(FCurrentBuddy) then
  begin
    ShowMessage('Please select a buddy first');
    Exit;
  end;
  
  if OpenDialogFile.Execute then
  begin
    Sid := FFileTransferManager.SendFile(FCurrentBuddy.JID, OpenDialogFile.FileName);
    if Sid <> '' then
    begin
      FChatManager.AddMessageToTab(
        FChatManager.GetOrCreateTab(FCurrentBuddy.Nick, FCurrentBuddy.JID),
        EditMyNick.Text,
        '📤 Sending file: ' + ExtractFileName(OpenDialogFile.FileName),
        False
      );
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
      ButtonSendFile.Enabled := False;
      MemoMessage.Enabled := False;
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

procedure TFormMain.MenuItemRequestAvatarClick(Sender: TObject);
var
  Index: Integer;
  BuddyNick: string;
  Buddy: TBarevBuddy;
begin
  if not FConnected then
    Exit;
  
  Index := ListBoxBuddies.ItemIndex;
  if Index < 0 then
  begin
    ShowMessage('Please select a buddy');
    Exit;
  end;
  
  BuddyNick := Copy(ListBoxBuddies.Items[Index], 1, Pos(' ', ListBoxBuddies.Items[Index]) - 1);
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
      UpdateMyAvatarDisplay;
  end;
end;

procedure TFormMain.MenuItemClearAvatarClick(Sender: TObject);
begin
  if not FConnected then
    Exit;
  
  FAvatarManager.ClearMyAvatar;
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
    FChatManager.AddMessageToTab(TabSheet, EditMyNick.Text, MessageText, False);
    FChatManager.SaveMessageToHistory(FCurrentBuddy.JID, EditMyNick.Text, MessageText, False);
    
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
      ButtonSendFile.Enabled := True;
      MemoMessage.Enabled := True;
      MemoMessage.SetFocus;
      
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
      ButtonSendFile.Enabled := False;
      MemoMessage.Enabled := False;
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

procedure TFormMain.OnTypingNotification(Buddy: TBarevBuddy; IsTyping: Boolean);
var
  TabSheet: TTabSheet;
begin
  TabSheet := FChatManager.GetOrCreateTab(Buddy.Nick, Buddy.JID);
  
  if IsTyping then
    TabSheet.Caption := Buddy.Nick + ' (typing...)'
  else
    TabSheet.Caption := Buddy.Nick;
end;

procedure TFormMain.OnLog(const LogLevel, Message: string);
begin
  MemoLog.Lines.Add('[' + LogLevel + '] ' + Message);
end;

procedure TFormMain.OnFTOffer(Buddy: TBarevBuddy; const Sid, FileName: string; FileSize: Int64);
begin
  FFileTransferManager.AddPendingOffer(Sid, FileName, FileSize, Buddy.Nick, Buddy.JID);

  ShowFileOffer(Buddy.Nick, FileName, FileSize, Sid);
end;

procedure TFormMain.OnFTProgress(Buddy: TBarevBuddy; const Sid: string; BytesDone, BytesTotal: Int64);
begin
  if (BytesDone mod (1024 * 1024)) < 1024 then // Log every ~1MB
    LogMessage('Transfer ' + Sid + ': ' + IntToStr(BytesDone div 1024) + 'KB / ' + 
               IntToStr(BytesTotal div 1024) + 'KB');
end;

procedure TFormMain.OnFTComplete(Buddy: TBarevBuddy; const Sid, LocalPath: string);
var
  TabSheet: TTabSheet;
begin
  LogMessage('File transfer complete: ' + LocalPath);

  TabSheet := FChatManager.GetOrCreateTab(Buddy.Nick, Buddy.JID);
  FChatManager.AddMessageToTab(TabSheet, EditMyNick.Text, 
    '📥 File received: ' + ExtractFileName(LocalPath), True);
  
  ShowMessage('File received from ' + Buddy.Nick + ':' + LineEnding + LocalPath);
end;

procedure TFormMain.OnFTError(Buddy: TBarevBuddy; const Sid, ErrMsg: string);
begin
  LogMessage('File transfer error (' + Sid + '): ' + ErrMsg);
  FFileTransferManager.RemovePendingOffer(Sid);
end;

procedure TFormMain.ShowFileOffer(const BuddyNick, FileName: string; FileSize: Int64; const Sid: string);
var
  SizeStr: string;
  Response: Integer;
begin
  if FileSize < 1024 then
    SizeStr := IntToStr(FileSize) + ' bytes'
  else if FileSize < 1024 * 1024 then
    SizeStr := Format('%.1f KB', [FileSize / 1024])
  else
    SizeStr := Format('%.1f MB', [FileSize / (1024 * 1024)]);
  
  Response := MessageDlg('Incoming File',
    BuddyNick + ' wants to send you a file:' + LineEnding + LineEnding +
    'File: ' + FileName + LineEnding +
    'Size: ' + SizeStr + LineEnding + LineEnding +
    'Accept this file?',
    mtConfirmation, [mbYes, mbNo], 0);
  
  if Response = mrYes then
  begin
    SaveDialogFile.FileName := FileName;
    if SaveDialogFile.Execute then
      FFileTransferManager.AcceptFile(Sid, SaveDialogFile.FileName)
    else
      FFileTransferManager.RejectFile(Sid);
  end
  else
    FFileTransferManager.RejectFile(Sid);
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
