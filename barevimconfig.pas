unit BarevIMConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, BarevIMTypes;

type
  { TBarevConfig - manages application configuration }
  TBarevConfig = class
  private
    FConfigFile: string;
    FNick: string;
    FIPv6: string;
    FPort: Integer;
    FWindowLeft: Integer;
    FWindowTop: Integer;
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FAvatarPath: string;
    FAutoConnect: Boolean;
  public
    constructor Create(const AConfigFile: string);
    
    procedure Load;
    procedure Save;
    
    function IsValidForAutoConnect: Boolean;
    function GetContactsFilePath: string;
    
    property ConfigFile: string read FConfigFile;
    property Nick: string read FNick write FNick;
    property IPv6: string read FIPv6 write FIPv6;
    property Port: Integer read FPort write FPort;
    property WindowLeft: Integer read FWindowLeft write FWindowLeft;
    property WindowTop: Integer read FWindowTop write FWindowTop;
    property WindowWidth: Integer read FWindowWidth write FWindowWidth;
    property WindowHeight: Integer read FWindowHeight write FWindowHeight;
    property AvatarPath: string read FAvatarPath write FAvatarPath;
    property AutoConnect: Boolean read FAutoConnect write FAutoConnect;
  end;

{ Helper function to determine config file path }
function DetermineConfigFilePath: string;

implementation

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
    Result := GetAppConfigDir(False) + 'barev.ini';
    ForceDirectories(GetAppConfigDir(False));
  end;
end;

{ TBarevConfig }

constructor TBarevConfig.Create(const AConfigFile: string);
begin
  inherited Create;
  FConfigFile := AConfigFile;

  FNick := DEFAULT_NICK;
  FIPv6 := DEFAULT_IPV6;
  FPort := DEFAULT_PORT;
  FWindowLeft := 100;
  FWindowTop := 100;
  FWindowWidth := 800;
  FWindowHeight := 600;
  FAvatarPath := '';
  FAutoConnect := False;
end;

procedure TBarevConfig.Load;
var
  Ini: TIniFile;
begin
  if not FileExists(FConfigFile) then
    Exit;
  
  Ini := TIniFile.Create(FConfigFile);
  try
    FNick := Ini.ReadString('User', 'Nick', DEFAULT_NICK);
    FIPv6 := Ini.ReadString('User', 'IPv6', DEFAULT_IPV6);
    FPort := Ini.ReadInteger('User', 'Port', DEFAULT_PORT);
    FAvatarPath := Ini.ReadString('User', 'AvatarPath', '');
    FAutoConnect := Ini.ReadBool('User', 'AutoConnect', False);
    
    FWindowLeft := Ini.ReadInteger('Window', 'Left', FWindowLeft);
    FWindowTop := Ini.ReadInteger('Window', 'Top', FWindowTop);
    FWindowWidth := Ini.ReadInteger('Window', 'Width', 800);
    FWindowHeight := Ini.ReadInteger('Window', 'Height', 600);
  finally
    Ini.Free;
  end;
end;

procedure TBarevConfig.Save;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    Ini.WriteString('User', 'Nick', FNick);
    Ini.WriteString('User', 'IPv6', FIPv6);
    Ini.WriteInteger('User', 'Port', FPort);
    Ini.WriteString('User', 'AvatarPath', FAvatarPath);
    Ini.WriteBool('User', 'AutoConnect', FAutoConnect);
    
    Ini.WriteInteger('Window', 'Left', FWindowLeft);
    Ini.WriteInteger('Window', 'Top', FWindowTop);
    Ini.WriteInteger('Window', 'Width', FWindowWidth);
    Ini.WriteInteger('Window', 'Height', FWindowHeight);
  finally
    Ini.Free;
  end;
end;

function TBarevConfig.IsValidForAutoConnect: Boolean;
begin
  Result := (Trim(FNick) <> '') and
            (Trim(FNick) <> DEFAULT_NICK) and
            (Trim(FIPv6) <> '') and
            (Trim(FIPv6) <> DEFAULT_IPV6) and
            (FPort > 0) and (FPort <= 65535);
end;

function TBarevConfig.GetContactsFilePath: string;
begin
  Result := ExtractFileDir(FConfigFile) + PathDelim + 'contacts.txt';
end;

end.
