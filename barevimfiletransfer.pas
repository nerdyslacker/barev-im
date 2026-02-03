unit BarevIMFileTransfer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, StdCtrls, ExtCtrls, Dialogs,
  Barev, BarevTypes, BarevIMTypes;

type
  TLogEvent = procedure(const Message: string) of object;
  TFileReceivedEvent = procedure(const BuddyNick, FilePath: string) of object;
  TTransferProgressEvent = procedure(const Sid: string; BytesDone, BytesTotal: Int64) of object;

  { TPendingFileOffer - stores incoming file offer info }
  TPendingFileOffer = record
    Sid: string;
    FileName: string;
    FileSize: Int64;
    BuddyNick: string;
    BuddyJID: string;
  end;
  PPendingFileOffer = ^TPendingFileOffer;

  { TFileTransferManager - manages file transfers }
  TFileTransferManager = class
  private
    FClient: TBarevClient;
    FPendingOffers: TList;
    FOnLog: TLogEvent;
    FOnFileReceived: TFileReceivedEvent;
    FOnTransferProgress: TTransferProgressEvent;
    
    procedure Log(const Message: string);
    procedure ClearPendingOffers;
  public
    constructor Create(AClient: TBarevClient);
    destructor Destroy; override;
    
    { File transfer operations }
    function SendFile(const BuddyJID, FilePath: string): string;
    function AcceptFile(const Sid, SavePath: string): Boolean;
    procedure RejectFile(const Sid: string);
    
    { Pending offers management }
    procedure AddPendingOffer(const Sid, FileName: string; FileSize: Int64; 
      const BuddyNick, BuddyJID: string);
    function GetPendingOffer(const Sid: string): PPendingFileOffer;
    function GetPendingOfferCount: Integer;
    function GetPendingOfferByIndex(Index: Integer): PPendingFileOffer;
    procedure RemovePendingOffer(const Sid: string);
    
    property Client: TBarevClient read FClient write FClient;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property OnFileReceived: TFileReceivedEvent
      read FOnFileReceived write FOnFileReceived;
    property OnTransferProgress: TTransferProgressEvent
      read FOnTransferProgress write FOnTransferProgress;
  end;

implementation

{ TFileTransferManager }

constructor TFileTransferManager.Create(AClient: TBarevClient);
begin
  inherited Create;
  FClient := AClient;
  FPendingOffers := TList.Create;
  FOnLog := nil;
  FOnFileReceived := nil;
  FOnTransferProgress := nil;
end;

destructor TFileTransferManager.Destroy;
begin
  ClearPendingOffers;
  FPendingOffers.Free;
  inherited Destroy;
end;

procedure TFileTransferManager.Log(const Message: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Message);
end;

procedure TFileTransferManager.ClearPendingOffers;
var
  I: Integer;
  Offer: PPendingFileOffer;
begin
  for I := 0 to FPendingOffers.Count - 1 do
  begin
    Offer := PPendingFileOffer(FPendingOffers[I]);
    Dispose(Offer);
  end;
  FPendingOffers.Clear;
end;

function TFileTransferManager.SendFile(const BuddyJID, FilePath: string): string;
var
  Buddy: TBarevBuddy;
begin
  Result := '';
  
  if not Assigned(FClient) then
  begin
    Log('Error: Client not connected');
    Exit;
  end;
  
  if not Assigned(FClient.FileTransfer) then
  begin
    Log('Error: File transfer not available');
    Exit;
  end;
  
  if not FileExists(FilePath) then
  begin
    Log('Error: File not found: ' + FilePath);
    Exit;
  end;
  
  Buddy := FClient.GetBuddy(BuddyJID);
  if not Assigned(Buddy) then
    Buddy := FClient.FindBuddyByJID(BuddyJID);
  
  if not Assigned(Buddy) then
  begin
    Log('Error: Buddy not found: ' + BuddyJID);
    Exit;
  end;
  
  Result := FClient.FileTransfer.OfferFile(Buddy, FilePath);
  if Result <> '' then
    Log('File offer sent: ' + ExtractFileName(FilePath) + ' (SID: ' + Result + ')')
  else
    Log('Failed to send file offer');
end;

function TFileTransferManager.AcceptFile(const Sid, SavePath: string): Boolean;
begin
  Result := False;
  
  if not Assigned(FClient) or not Assigned(FClient.FileTransfer) then
  begin
    Log('Error: File transfer not available');
    Exit;
  end;
  
  Result := FClient.FileTransfer.AcceptOffer(Sid, SavePath);
  if Result then
  begin
    Log('Accepting file transfer: ' + Sid);
    RemovePendingOffer(Sid);
  end
  else
    Log('Failed to accept file transfer: ' + Sid);
end;

procedure TFileTransferManager.RejectFile(const Sid: string);
begin
  if not Assigned(FClient) or not Assigned(FClient.FileTransfer) then
  begin
    Log('Error: File transfer not available');
    Exit;
  end;
  
  FClient.FileTransfer.RejectOffer(Sid);
  RemovePendingOffer(Sid);
  Log('Rejected file transfer: ' + Sid);
end;

procedure TFileTransferManager.AddPendingOffer(const Sid, FileName: string; 
  FileSize: Int64; const BuddyNick, BuddyJID: string);
var
  Offer: PPendingFileOffer;
begin
  New(Offer);
  Offer^.Sid := Sid;
  Offer^.FileName := FileName;
  Offer^.FileSize := FileSize;
  Offer^.BuddyNick := BuddyNick;
  Offer^.BuddyJID := BuddyJID;
  FPendingOffers.Add(Offer);
end;

function TFileTransferManager.GetPendingOffer(const Sid: string): PPendingFileOffer;
var
  I: Integer;
  Offer: PPendingFileOffer;
begin
  Result := nil;
  for I := 0 to FPendingOffers.Count - 1 do
  begin
    Offer := PPendingFileOffer(FPendingOffers[I]);
    if Offer^.Sid = Sid then
    begin
      Result := Offer;
      Exit;
    end;
  end;
end;

function TFileTransferManager.GetPendingOfferCount: Integer;
begin
  Result := FPendingOffers.Count;
end;

function TFileTransferManager.GetPendingOfferByIndex(Index: Integer): PPendingFileOffer;
begin
  if (Index >= 0) and (Index < FPendingOffers.Count) then
    Result := PPendingFileOffer(FPendingOffers[Index])
  else
    Result := nil;
end;

procedure TFileTransferManager.RemovePendingOffer(const Sid: string);
var
  I: Integer;
  Offer: PPendingFileOffer;
begin
  for I := FPendingOffers.Count - 1 downto 0 do
  begin
    Offer := PPendingFileOffer(FPendingOffers[I]);
    if Offer^.Sid = Sid then
    begin
      Dispose(Offer);
      FPendingOffers.Delete(I);
      Exit;
    end;
  end;
end;

end.
