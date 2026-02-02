unit BarevIMContacts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Barev, BarevTypes;

type
  TLogEvent = procedure(const Message: string) of object;

  { TContactManager - manages buddy list operations }
  TContactManager = class
  private
    FClient: TBarevClient;
    FContactsFile: string;
    FOnLog: TLogEvent;
  public
    constructor Create(AClient: TBarevClient; const AContactsFile: string);
    
    function AddBuddy(const Nick, IPv6: string; Port: Integer): TBarevBuddy;
    function RemoveBuddy(const BuddyJID: string): Boolean;
    function FindBuddyByNick(const Nick: string): TBarevBuddy;
    function FindBuddyByJID(const JID: string): TBarevBuddy;
    
    procedure LoadContacts;
    procedure SaveContacts;
    procedure Log(const Message: string);
    
    function GetBuddyCount: Integer;
    function GetBuddyByIndex(Index: Integer): TBarevBuddy;
    
    property Client: TBarevClient read FClient write FClient;
    property ContactsFile: string read FContactsFile write FContactsFile;
    property OnLog: TLogEvent read FOnLog write FOnLog;
  end;

implementation

{ TContactManager }

constructor TContactManager.Create(AClient: TBarevClient; const AContactsFile: string);
begin
  inherited Create;
  FClient := AClient;
  FContactsFile := AContactsFile;
  FOnLog := nil;
end;

procedure TContactManager.Log(const Message: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Message);
end;

function TContactManager.AddBuddy(const Nick, IPv6: string; Port: Integer): TBarevBuddy;
begin
  Result := nil;
  if not Assigned(FClient) then
    Exit;
  
  Result := FClient.AddBuddy(Nick, IPv6, Port);
  Log('Added buddy: ' + Nick + ' (' + IPv6 + ':' + IntToStr(Port) + ')');
end;

function TContactManager.RemoveBuddy(const BuddyJID: string): Boolean;
begin
  Result := False;
  if not Assigned(FClient) then
    Exit;
  
  Result := FClient.RemoveBuddy(BuddyJID);
end;

function TContactManager.FindBuddyByNick(const Nick: string): TBarevBuddy;
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
    if Buddy.Nick = Nick then
    begin
      Result := Buddy;
      Exit;
    end;
  end;
end;

function TContactManager.FindBuddyByJID(const JID: string): TBarevBuddy;
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
    if Buddy.JID = JID then
    begin
      Result := Buddy;
      Exit;
    end;
  end;
end;

procedure TContactManager.LoadContacts;
begin
  if not Assigned(FClient) then
    Exit;
  
  if not FileExists(FContactsFile) then
  begin
    Log('No contacts file found at: ' + FContactsFile);
    Exit;
  end;
  
  if FClient.LoadContactsFromFile(FContactsFile) then
    Log('Loaded contacts from: ' + FContactsFile)
  else
    Log('Failed to load contacts');
end;

procedure TContactManager.SaveContacts;
begin
  if not Assigned(FClient) then
    Exit;
  
  if FClient.SaveContactsToFile(FContactsFile) then
    Log('Saved contacts to: ' + FContactsFile)
  else
    Log('Failed to save contacts');
end;

function TContactManager.GetBuddyCount: Integer;
begin
  if Assigned(FClient) then
    Result := FClient.GetBuddyCount
  else
    Result := 0;
end;

function TContactManager.GetBuddyByIndex(Index: Integer): TBarevBuddy;
begin
  if Assigned(FClient) then
    Result := FClient.GetBuddyByIndex(Index)
  else
    Result := nil;
end;

end.
