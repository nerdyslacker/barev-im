unit BarevIMTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BarevTypes;

const
  DEFAULT_PORT = 5299;
  DEFAULT_NICK = 'mynick';
  DEFAULT_IPV6 = '201:af82:9f2f:7809::1';

  COLOR_INCOMING = clRed;
  COLOR_OUTGOING = clSkyBlue;

  SYMBOL_INCOMING = '↓';
  SYMBOL_OUTGOING = '↑';

type
  { TChatMessage - represents a single chat message }
  TChatMessage = record
    Nick: string;
    Message: string;
    Incoming: Boolean;
    Timestamp: TDateTime;
  end;
  
  { PChatMessage - pointer to chat message for lists }
  PChatMessage = ^TChatMessage;

function CreateChatMessage(const ANick, AMessage: string; AIncoming: Boolean): TChatMessage;
function GetMessageSymbol(Incoming: Boolean): string;
function GetMessageColor(Incoming: Boolean): TColor;

{ Status display helpers }
function StatusToDisplayString(Status: TBuddyStatus): string;
function StatusToIcon(Status: TBuddyStatus): string;

implementation

function CreateChatMessage(const ANick, AMessage: string; AIncoming: Boolean): TChatMessage;
begin
  Result.Nick := ANick;
  Result.Message := AMessage;
  Result.Incoming := AIncoming;
  Result.Timestamp := Now;
end;

function GetMessageSymbol(Incoming: Boolean): string;
begin
  if Incoming then
    Result := SYMBOL_INCOMING
  else
    Result := SYMBOL_OUTGOING;
end;

function GetMessageColor(Incoming: Boolean): TColor;
begin
  if Incoming then
    Result := COLOR_INCOMING
  else
    Result := COLOR_OUTGOING;
end;

function StatusToDisplayString(Status: TBuddyStatus): string;
begin
  case Status of
    bsOffline: Result := 'Offline';
    bsAvailable: Result := 'Available';
    bsAway: Result := 'Away';
    bsExtendedAway: Result := 'Extended Away';
    bsDoNotDisturb: Result := 'Do Not Disturb';
  else
    Result := 'Unknown';
  end;
end;

function StatusToIcon(Status: TBuddyStatus): string;
begin
  case Status of
    bsOffline: Result := '⚫';
    bsAvailable: Result := '🟢';
    bsAway: Result := '🟡';
    bsExtendedAway: Result := '🟠';
    bsDoNotDisturb: Result := '⛔';
  else
    Result := '⚪';
  end;
end;

end.
