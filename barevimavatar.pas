unit BarevIMAvatar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Barev, BarevTypes;

type
  TLogEvent = procedure(const Message: string) of object;
  TAvatarChangedEvent = procedure(const BuddyJID: string) of object;

  { TAvatarManager - manages avatar operations }
  TAvatarManager = class
  private
    FClient: TBarevClient;
    FOnLog: TLogEvent;
    FOnAvatarChanged: TAvatarChangedEvent;
    
    procedure Log(const Message: string);
  public
    constructor Create(AClient: TBarevClient);
    
    { My avatar operations }
    function SetMyAvatar(const FilePath: string): Boolean;
    procedure ClearMyAvatar;
    function GetMyAvatarHash: string;
    function GetMyAvatarPath: string;
    
    { Buddy avatar operations }
    function RequestBuddyAvatar(const BuddyJID: string): Boolean;

    property Client: TBarevClient read FClient write FClient;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnAvatarChanged: TAvatarChangedEvent
      read FOnAvatarChanged write FOnAvatarChanged;
  end;

implementation

{ TAvatarManager }

constructor TAvatarManager.Create(AClient: TBarevClient);
begin
  inherited Create;
  FClient := AClient;
  FOnLog := nil;
  FOnAvatarChanged := nil;
end;

procedure TAvatarManager.Log(const Message: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Message);
end;

function TAvatarManager.SetMyAvatar(const FilePath: string): Boolean;
begin
  Result := False;
  
  if not Assigned(FClient) then
  begin
    Log('Error: Client not connected');
    Exit;
  end;
  
  if not FileExists(FilePath) then
  begin
    Log('Error: Avatar file not found: ' + FilePath);
    Exit;
  end;
  
  Result := FClient.LoadMyAvatar(FilePath);
  if Result then
  begin
    Log('Avatar set: ' + FilePath);
    Log('Avatar hash: ' + FClient.GetMyAvatarHash);
    FClient.SendPresence;
  end
  else
    Log('Failed to set avatar');
end;

procedure TAvatarManager.ClearMyAvatar;
begin
  if not Assigned(FClient) then
    Exit;
  
  FClient.ClearMyAvatar;
  FClient.SendPresence;
  Log('Avatar cleared');
end;

function TAvatarManager.GetMyAvatarHash: string;
begin
  if Assigned(FClient) then
    Result := FClient.GetMyAvatarHash
  else
    Result := '';
end;

function TAvatarManager.GetMyAvatarPath: string;
begin
  if Assigned(FClient) and Assigned(FClient.AvatarManager) then
    Result := FClient.AvatarManager.MyAvatarPath
  else
    Result := '';
end;

function TAvatarManager.RequestBuddyAvatar(const BuddyJID: string): Boolean;
begin
  Result := False;
  
  if not Assigned(FClient) then
  begin
    Log('Error: Client not connected');
    Exit;
  end;
  
  Result := FClient.RequestBuddyAvatar(BuddyJID);
  if Result then
    Log('Avatar request sent to: ' + BuddyJID)
  else
    Log('Failed to request avatar from: ' + BuddyJID);
end;

end.
