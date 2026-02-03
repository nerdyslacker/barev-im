unit BarevIMAddBuddyDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  BarevIMTypes;

type

  { TAddBuddyDialog }

  TAddBuddyDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    EditNick: TEdit;
    EditIPv6: TEdit;
    EditPort: TEdit;
    LabelNick: TLabel;
    LabelIPv6: TLabel;
    LabelPort: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
    class function Execute(out ANick, AIPv6: string; out APort: Integer): Boolean;
  end;

var
  AddBuddyDialog: TAddBuddyDialog;

implementation

{$R *.lfm}

{ TAddBuddyDialog }

procedure TAddBuddyDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Port: Integer;
begin
  CanClose := True;
  
  if ModalResult = mrOK then
  begin
    if Trim(EditNick.Text) = '' then
    begin
      ShowMessage('Please enter a nickname');
      EditNick.SetFocus;
      CanClose := False;
      Exit;
    end;
    
    if Trim(EditIPv6.Text) = '' then
    begin
      ShowMessage('Please enter an IPv6 address');
      EditIPv6.SetFocus;
      CanClose := False;
      Exit;
    end;
    
    if not TryStrToInt(Trim(EditPort.Text), Port) then
    begin
      ShowMessage('Invalid port number');
      EditPort.SetFocus;
      CanClose := False;
      Exit;
    end;
    
    if (Port < 1) or (Port > 65535) then
    begin
      ShowMessage('Port must be between 1 and 65535');
      EditPort.SetFocus;
      CanClose := False;
      Exit;
    end;
  end;
end;

class function TAddBuddyDialog.Execute(out ANick, AIPv6: string; out APort: Integer): Boolean;
var
  Dlg: TAddBuddyDialog;
begin
  Result := False;
  Dlg := TAddBuddyDialog.Create(nil);
  try
    Dlg.EditPort.Text := IntToStr(DEFAULT_PORT);
    if Dlg.ShowModal = mrOK then
    begin
      ANick := Trim(Dlg.EditNick.Text);
      AIPv6 := Trim(Dlg.EditIPv6.Text);
      APort := StrToInt(Trim(Dlg.EditPort.Text));
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

end.
