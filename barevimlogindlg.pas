unit BarevIMLoginDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, BarevIMTypes;

type

  { TLoginDialog }

  TLoginDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckBoxAutoConnect: TCheckBox;
    EditNick: TEdit;
    EditIPv6: TEdit;
    EditPort: TEdit;
    LabelNick: TLabel;
    LabelIPv6: TLabel;
    LabelPort: TLabel;
    LabelTitle: TLabel;
    PanelTop: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
    class function Execute(var ANick, AIPv6: string; var APort: Integer; 
      var AAutoConnect: Boolean): Boolean;
  end;

var
  LoginDialog: TLoginDialog;

implementation

{$R *.lfm}

{ TLoginDialog }

procedure TLoginDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Port: Integer;
begin
  CanClose := True;
  
  if ModalResult = mrOK then
  begin
    if Trim(EditNick.Text) = '' then
    begin
      ShowMessage('Please enter your nickname');
      EditNick.SetFocus;
      CanClose := False;
      Exit;
    end;
    
    if Trim(EditIPv6.Text) = '' then
    begin
      ShowMessage('Please enter your IPv6 address');
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

class function TLoginDialog.Execute(var ANick, AIPv6: string; var APort: Integer;
  var AAutoConnect: Boolean): Boolean;
var
  Dlg: TLoginDialog;
begin
  Result := False;
  Dlg := TLoginDialog.Create(nil);
  try
    if ANick <> '' then
      Dlg.EditNick.Text := ANick;
    if AIPv6 <> '' then
      Dlg.EditIPv6.Text := AIPv6;
    if APort > 0 then
      Dlg.EditPort.Text := IntToStr(APort)
    else
      Dlg.EditPort.Text := IntToStr(DEFAULT_PORT);
    Dlg.CheckBoxAutoConnect.Checked := AAutoConnect;
    
    if Dlg.ShowModal = mrOK then
    begin
      ANick := Trim(Dlg.EditNick.Text);
      AIPv6 := Trim(Dlg.EditIPv6.Text);
      APort := StrToInt(Trim(Dlg.EditPort.Text));
      AAutoConnect := Dlg.CheckBoxAutoConnect.Checked;
      Result := True;
    end;
  finally
    Dlg.Free;
  end;
end;

end.
