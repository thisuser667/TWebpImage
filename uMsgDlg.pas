unit uMsgDlg;

{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}


interface

uses
  {$IFNDEF FPC and IFDEF WINDOWS}
  {$DEFINE NOFPCW}
  System.Classes, System.SysUtils, Vcl.Forms, Vcl.Dialogs, Vcl.Controls, Vcl.ExtCtrls;
  {$ELSE}
  Classes, SysUtils, Forms, Dialogs, Controls, ExtCtrls;
  {$ENDIF}

type
  TMsgDlgEx = class
  private
    FMsgFrm: TForm;
    FParent: TForm;
    FTimer: TTimer;
    FDefModRes: TModalResult;
    procedure DoOnTimer(Sender: TObject);
  public
    constructor Create(const AMsg: string; ADlgType: TMsgDlgType; AButtons:
      TMsgDlgButtons; AParent: TForm; ADefModRes: TModalResult;
      AInterval: Cardinal);
    destructor Destroy; override;
    function ShowDialog: TModalResult;
  end;

implementation

constructor TMsgDlgEx.Create(const AMsg: string; ADlgType: TMsgDlgType;
  AButtons: TMsgDlgButtons; AParent: TForm; ADefModRes: TModalResult;
  AInterval: Cardinal);
begin
  FMsgFrm := CreateMessageDialog(AMsg, ADlgType, AButtons);
  FDefModRes := ADefModRes;
  FParent := AParent;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := AInterval;
  FTimer.OnTimer := {$IFNDEF NOFPCW}@{$ENDIF}DoOnTimer;
end;

destructor TMsgDlgEx.Destroy;
begin
  FTimer.Enabled := False;
  FTimer.Free;
  FMsgFrm.FormStyle:= fsNormal;
  FMsgFrm.Hide;
  FMsgFrm.Free;
  inherited Destroy;
end;

function TMsgDlgEx.ShowDialog: TModalResult;
begin
  FMsgFrm.FormStyle := {$IFNDEF NOFPCW}fsSystemStayOnTop{$ELSE}fsStayOnTop{$ENDIF};
  if FParent <> nil then
  begin
     FMsgFrm.Position := poDefaultSizeOnly;
     FMsgFrm.Left := FParent.Left + (FParent.Width - FMsgFrm.Width) div 2;
     FMsgFrm.Top := FParent.Top + (FParent.Height - FMsgFrm.Height) div 2;
   end
   else
     FMsgFrm.Position := {$IFNDEF NOFPCW}poWorkAreaCenter{$ELSE}poScreenCenter{$ENDIF};
   FTimer.Enabled := True;
   Result := FMsgFrm.ShowModal;
end;

procedure TMsgDlgEx.DoOnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  FMsgFrm.ModalResult := FDefModRes;
end;


end.

