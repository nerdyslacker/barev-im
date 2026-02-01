unit BarevIMForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, Barev, BarevTypes, IniFiles, Contnrs;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonConnect: TButton;
    ButtonSend: TButton;
    ButtonAddBuddy: TButton;
    ButtonRemoveBuddy: TButton;
    EditMessage: TEdit;
    EditMyNick: TEdit;
    EditMyIPv6: TEdit;
    EditMyPort: TEdit;
    LabelMyNick: TLabel;
    LabelMyIPv6: TLabel;
    LabelMyPort: TLabel;
    LabelBuddies: TLabel;
    LabelChat: TLabel;
    ListBoxBuddies: TListBox;
    MemoChat: TMemo;
    MemoLog: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Splitter1: TSplitter;
    TabSheetChat: TTabSheet;
    TabSheetLog: TTabSheet;
    Timer1: TTimer;
    procedure ButtonAddBuddyClick(Sender: TObject);
    procedure ButtonRemoveBuddyClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure EditMessageKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxBuddiesDblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FClient: TBarevClient;
    FConnected: Boolean;
    FCurrentBuddy: TBarevBuddy;
    FConfigFile: string;
    FChatHistory: TFPHashList;
    procedure OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure OnLog(const LogLevel, Message: string);
    procedure UpdateBuddyList;
    procedure AddChatMessage(const Nick, Message: string; Incoming: Boolean);
    procedure LoadChatHistory(const BuddyJID: string);
    procedure SaveChatMessage(const BuddyJID, Nick, Message: string; Incoming: Boolean);
    procedure ClearAllChatHistory;
    procedure LoadConfig;
    procedure SaveConfig;
    procedure LoadBuddies;
    procedure SaveBuddies;
  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
  ConfigParam: string;
begin
  FConnected := False;
  FClient := nil;
  FCurrentBuddy := nil;
  FChatHistory := TFPHashList.Create;

  ConfigParam := '';
  for I := 1 to ParamCount do
  begin
    if (Pos('--config=', ParamStr(I)) = 1) then
    begin
      ConfigParam := Copy(ParamStr(I), 10, Length(ParamStr(I)));
      Break;
    end;
  end;

  if ConfigParam <> '' then
  begin
    FConfigFile := ConfigParam;
    ForceDirectories(ExtractFileDir(FConfigFile));
  end
  else
  begin
    FConfigFile := GetAppConfigDir(False) + 'barev.ini';
    ForceDirectories(GetAppConfigDir(False));
  end;
  
  Caption := 'barev IM';
  Position := poScreenCenter;
  Width := 800;
  Height := 600;

  EditMyPort.Text := '5299';

  LoadConfig;
  
  MemoChat.ReadOnly := True;
  MemoLog.ReadOnly := True;
  
  ButtonSend.Enabled := False;
  EditMessage.Enabled := False;
  ButtonRemoveBuddy.Enabled := False;
  
  MemoLog.Lines.Add('Configuration file: ' + FConfigFile);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FClient) then
  begin
    SaveBuddies;
    FClient.Stop;
    FClient.Free;
  end;

  ClearAllChatHistory;
  FChatHistory.Free;
  
  SaveConfig;
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
      Exit;
    end;
    
    if Trim(EditMyIPv6.Text) = '' then
    begin
      ShowMessage('Please enter your IPv6 address');
      Exit;
    end;

    if not TryStrToInt(Trim(EditMyPort.Text), Port) then
    begin
      ShowMessage('Invalid port number');
      Exit;
    end;
    
    if (Port < 1) or (Port > 65535) then
    begin
      ShowMessage('Port must be between 1 and 65535');
      Exit;
    end;
    
    try
      FClient := TBarevClient.Create(EditMyNick.Text, EditMyIPv6.Text, Port);
      FClient.OnMessageReceived := @OnMessageReceived;
      FClient.OnBuddyStatus := @OnBuddyStatus;
      FClient.OnLog := @OnLog;
      
      if FClient.Start then
      begin
        FConnected := True;
        ButtonConnect.Caption := 'Disconnect';
        EditMyNick.Enabled := False;
        EditMyIPv6.Enabled := False;
        EditMyPort.Enabled := False;
        ButtonAddBuddy.Enabled := True;
        ButtonRemoveBuddy.Enabled := True;
        Timer1.Enabled := True;
        
        MemoLog.Lines.Add('=== Connected on port ' + IntToStr(Port) + ' ===');

        LoadBuddies;
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
    if Assigned(FClient) then
    begin
      SaveBuddies;
      FClient.Stop;
      FClient.Free;
      FClient := nil;
    end;
    
    FConnected := False;
    FCurrentBuddy := nil;
    ButtonConnect.Caption := 'Connect';
    EditMyNick.Enabled := True;
    EditMyIPv6.Enabled := True;
    EditMyPort.Enabled := True;
    ButtonAddBuddy.Enabled := False;
    ButtonRemoveBuddy.Enabled := False;
    ButtonSend.Enabled := False;
    EditMessage.Enabled := False;
    Timer1.Enabled := False;
    
    ListBoxBuddies.Clear;

    ClearAllChatHistory;
    MemoChat.Clear;
    MemoLog.Clear;
    LabelChat.Caption := 'Chat';
    
    MemoLog.Lines.Add('=== Disconnected ===');
  end;
end;

procedure TFormMain.ButtonAddBuddyClick(Sender: TObject);
var
  BuddyNick, BuddyIPv6, BuddyPortStr: string;
  BuddyPort: Integer;
  Buddy: TBarevBuddy;
begin
  if not FConnected then
    Exit;
    
  BuddyNick := '';
  BuddyIPv6 := '';
  BuddyPortStr := '5299';
  
  if InputQuery('Add Buddy', 'Enter buddy nickname:', BuddyNick) then
  begin
    if Trim(BuddyNick) = '' then
      Exit;
      
    if InputQuery('Add Buddy', 'Enter buddy IPv6 address:', BuddyIPv6) then
    begin
      if Trim(BuddyIPv6) = '' then
        Exit;
      
      if InputQuery('Add Buddy', 'Enter buddy port (default 5299):', BuddyPortStr) then
      begin
        if not TryStrToInt(Trim(BuddyPortStr), BuddyPort) then
          BuddyPort := 5299;
          
        if (BuddyPort < 1) or (BuddyPort > 65535) then
          BuddyPort := 5299;
        
        try
          Buddy := FClient.AddBuddy(BuddyNick, BuddyIPv6, BuddyPort);
          UpdateBuddyList;
          SaveBuddies;
          MemoLog.Lines.Add('Added buddy: ' + BuddyNick + ' (' + BuddyIPv6 + ':' + IntToStr(BuddyPort) + ')');
        except
          on E: Exception do
            ShowMessage('Error adding buddy: ' + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TFormMain.ButtonRemoveBuddyClick(Sender: TObject);
var
  Index: Integer;
  BuddyNick: string;
  I: Integer;
  Buddy: TBarevBuddy;
  BuddyJID: string;
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

  BuddyJID := '';
  for I := 0 to FClient.GetBuddyCount - 1 do
  begin
    Buddy := FClient.GetBuddyByIndex(I);
    if Buddy.Nick = BuddyNick then
    begin
      BuddyJID := Buddy.JID;
      Break;
    end;
  end;
  
  if BuddyJID = '' then
  begin
    ShowMessage('Could not find buddy');
    Exit;
  end;
  
  try
    if Assigned(FCurrentBuddy) and (FCurrentBuddy.JID = BuddyJID) then
    begin
      FCurrentBuddy := nil;
      MemoChat.Clear;
      LabelChat.Caption := 'Chat';
      ButtonSend.Enabled := False;
      EditMessage.Enabled := False;
    end;

    if FClient.RemoveBuddy(BuddyJID) then
    begin
      UpdateBuddyList;
      SaveBuddies;
      MemoLog.Lines.Add('Removed buddy: ' + BuddyNick);
    end
    else
    begin
      ShowMessage('Failed to remove buddy');
    end;
  except
    on E: Exception do
      ShowMessage('Error removing buddy: ' + E.Message);
  end;
end;

procedure TFormMain.ButtonSendClick(Sender: TObject);
var
  MessageText: string;
begin
  if not Assigned(FCurrentBuddy) then
    Exit;
    
  MessageText := Trim(EditMessage.Text);
  if MessageText = '' then
    Exit;
    
  try
    FClient.SendMessage(FCurrentBuddy.JID, MessageText);

    SaveChatMessage(FCurrentBuddy.JID, EditMyNick.Text, MessageText, False);

    AddChatMessage(EditMyNick.Text, MessageText, False);
    
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
  JID: string;
  I: Integer;
  Buddy: TBarevBuddy;
begin
  Index := ListBoxBuddies.ItemIndex;
  if Index < 0 then
    Exit;

  JID := Copy(ListBoxBuddies.Items[Index], 1, Pos(' ', ListBoxBuddies.Items[Index]) - 1);

  for I := 0 to FClient.GetBuddyCount - 1 do
  begin
    Buddy := FClient.GetBuddyByIndex(I);
    if Buddy.Nick = JID then
    begin
      if not Assigned(FCurrentBuddy) or (FCurrentBuddy.JID <> Buddy.JID) then
      begin
        FCurrentBuddy := Buddy;

        LoadChatHistory(FCurrentBuddy.JID);
        
        LabelChat.Caption := 'Chat with: ' + FCurrentBuddy.Nick;
        MemoLog.Lines.Add('Switched to chat with: ' + FCurrentBuddy.Nick);
      end;

      try
        FClient.ConnectToBuddy(FCurrentBuddy.JID);
        
        ButtonSend.Enabled := True;
        EditMessage.Enabled := True;
        EditMessage.SetFocus;
        
        MemoLog.Lines.Add('Connecting to: ' + FCurrentBuddy.Nick);
      except
        on E: Exception do
          ShowMessage('Error connecting to buddy: ' + E.Message);
      end;
      
      Break;
    end;
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
        MemoLog.Lines.Add('Process error: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
begin
  SaveChatMessage(Buddy.JID, Buddy.Nick, MessageText, True);

  if Assigned(FCurrentBuddy) and (FCurrentBuddy.JID = Buddy.JID) then
  begin
    AddChatMessage(Buddy.Nick, MessageText, True);
  end
  else
  begin
    MemoLog.Lines.Add('Message from ' + Buddy.Nick + ' (not in active chat)');
  end;
end;

procedure TFormMain.OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
begin
  UpdateBuddyList;
  if NewStatus <> bsOffline then
    MemoLog.Lines.Add('Buddy online: ' + Buddy.Nick)
  else
    MemoLog.Lines.Add('Buddy offline: ' + Buddy.Nick);
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
begin
  ListBoxBuddies.Clear;
  
  if not Assigned(FClient) then
    Exit;
    
  for I := 0 to FClient.GetBuddyCount - 1 do
  begin
    Buddy := FClient.GetBuddyByIndex(I);
    if Buddy.Status <> bsOffline then
      Status := '[Online]'
    else
      Status := '[Offline]';
      
    ListBoxBuddies.Items.Add(Buddy.Nick + ' ' + Status);
  end;
end;

procedure TFormMain.AddChatMessage(const Nick, Message: string; Incoming: Boolean);
var
  Prefix: string;
begin
  if Incoming then
    Prefix := '>> '
  else
    Prefix := '<< ';
    
  MemoChat.Lines.Add('[' + TimeToStr(Now) + '] ' + Prefix + Nick + ': ' + Message);

  MemoChat.SelStart := Length(MemoChat.Text);
  MemoChat.SelLength := 0;
end;

procedure TFormMain.LoadConfig;
var
  Ini: TIniFile;
begin
  if not FileExists(FConfigFile) then
  begin
    EditMyNick.Text := 'mynick';
    EditMyIPv6.Text := '201:af82:9f2f:7809::1';
    EditMyPort.Text := '5299';
    Exit;
  end;
  
  try
    Ini := TIniFile.Create(FConfigFile);
    try
      EditMyNick.Text := Ini.ReadString('User', 'Nick', 'mynick');
      EditMyIPv6.Text := Ini.ReadString('User', 'IPv6', '201:af82:9f2f:7809::1');
      EditMyPort.Text := IntToStr(Ini.ReadInteger('User', 'Port', 5299));

      Left := Ini.ReadInteger('Window', 'Left', Left);
      Top := Ini.ReadInteger('Window', 'Top', Top);
      Width := Ini.ReadInteger('Window', 'Width', 800);
      Height := Ini.ReadInteger('Window', 'Height', 600);
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Error loading config: ' + E.Message);
  end;
end;

procedure TFormMain.SaveConfig;
var
  Ini: TIniFile;
  Port: Integer;
begin
  try
    Ini := TIniFile.Create(FConfigFile);
    try
      Ini.WriteString('User', 'Nick', EditMyNick.Text);
      Ini.WriteString('User', 'IPv6', EditMyIPv6.Text);

      if TryStrToInt(Trim(EditMyPort.Text), Port) then
        Ini.WriteInteger('User', 'Port', Port)
      else
        Ini.WriteInteger('User', 'Port', 5299);

      Ini.WriteInteger('Window', 'Left', Left);
      Ini.WriteInteger('Window', 'Top', Top);
      Ini.WriteInteger('Window', 'Width', Width);
      Ini.WriteInteger('Window', 'Height', Height);
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Error saving config: ' + E.Message);
  end;
end;

procedure TFormMain.LoadBuddies;
var
  Ini: TIniFile;
  Sections: TStringList;
  I: Integer;
  Nick, IPv6: string;
  Port: Integer;
begin
  if not Assigned(FClient) then
    Exit;
    
  if not FileExists(FConfigFile) then
    Exit;
    
  try
    Ini := TIniFile.Create(FConfigFile);
    Sections := TStringList.Create;
    try
      Ini.ReadSections(Sections);
      
      for I := 0 to Sections.Count - 1 do
      begin
        if Pos('Buddy_', Sections[I]) = 1 then
        begin
          Nick := Ini.ReadString(Sections[I], 'Nick', '');
          IPv6 := Ini.ReadString(Sections[I], 'IPv6', '');
          Port := Ini.ReadInteger(Sections[I], 'Port', 5299);
          
          if (Nick <> '') and (IPv6 <> '') then
          begin
            try
              FClient.AddBuddy(Nick, IPv6, Port);
              MemoLog.Lines.Add('Loaded buddy: ' + Nick + '@' + IPv6);
            except
              on E: Exception do
                MemoLog.Lines.Add('Error loading buddy ' + Nick + ': ' + E.Message);
            end;
          end;
        end;
      end;
    finally
      Sections.Free;
      Ini.Free;
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Error loading buddies: ' + E.Message);
  end;
end;

procedure TFormMain.SaveBuddies;
var
  Ini: TIniFile;
  I: Integer;
  Buddy: TBarevBuddy;
  SectionName: string;
  Sections: TStringList;
  J: Integer;
begin
  if not Assigned(FClient) then
    Exit;
    
  try
    Ini := TIniFile.Create(FConfigFile);
    Sections := TStringList.Create;
    try
      Ini.ReadSections(Sections);
      for J := 0 to Sections.Count - 1 do
      begin
        if Pos('Buddy_', Sections[J]) = 1 then
          Ini.EraseSection(Sections[J]);
      end;

      for I := 0 to FClient.GetBuddyCount - 1 do
      begin
        Buddy := FClient.GetBuddyByIndex(I);
        SectionName := 'Buddy_' + IntToStr(I);
        
        Ini.WriteString(SectionName, 'Nick', Buddy.Nick);
        Ini.WriteString(SectionName, 'IPv6', Buddy.IPv6Address);
        Ini.WriteInteger(SectionName, 'Port', Buddy.Port);
      end;
    finally
      Sections.Free;
      Ini.Free;
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Error saving buddies: ' + E.Message);
  end;
end;

procedure TFormMain.SaveChatMessage(const BuddyJID, Nick, Message: string; Incoming: Boolean);
var
  History: TStringList;
  Prefix: string;
  HistoryLine: string;
begin
  History := TStringList(FChatHistory.Find(BuddyJID));
  if not Assigned(History) then
  begin
    History := TStringList.Create;
    FChatHistory.Add(BuddyJID, History);
  end;

  if Incoming then
    Prefix := '>> '
  else
    Prefix := '<< ';
    
  HistoryLine := '[' + TimeToStr(Now) + '] ' + Prefix + Nick + ': ' + Message;
  History.Add(HistoryLine);
end;

procedure TFormMain.LoadChatHistory(const BuddyJID: string);
var
  History: TStringList;
  I: Integer;
begin
  MemoChat.Clear;

  History := TStringList(FChatHistory.Find(BuddyJID));
  if Assigned(History) then
  begin
    for I := 0 to History.Count - 1 do
    begin
      MemoChat.Lines.Add(History[I]);
    end;

    if MemoChat.Lines.Count > 0 then
    begin
      MemoChat.SelStart := Length(MemoChat.Text);
      MemoChat.SelLength := 0;
    end;
  end;
end;

procedure TFormMain.ClearAllChatHistory;
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
