unit MovieImageAuxUnit;

{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}


interface

uses
  {$IFNDEF FPC and IFDEF WINDOWS}
  {$DEFINE NOFPCW}
  Winapi.Windows, System.Classes, System.SysUtils, System.AnsiStrings, Vcl.ExtCtrls,
    Vcl.Forms, Vcl.Dialog;
  {$ELSE}
  LclIntf, LMessages, Dialogs, LclType, Classes, SysUtils, Controls, Forms, StdCtrls,
    ExtCtrls, FpImage, FpReadBmp, FpReadJpeg, FpReadPng, FpReadTiff, FpReadXpm;
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

  TGraphicType = record
    Offset, Len: Cardinal;
    Signature, Extension: PAnsiChar;
  end;

  TFontData = record
    Height: Integer;
    Weight: Integer;
    Width: Integer;
    Italic: Integer;
  end;
  PFontData = ^TFontData;

function GetImageExtensionBySignature(Stream: TMemoryStream): AnsiString; overload;
function GetImageExtensionBySignature(FileName: String): AnsiString; overload;
function ArrayToAnsiString(const a: array of AnsiChar;
  len: Integer = 0; start: Integer = 0): AnsiString;
function GetBitsValue(sourcevalue, posbyte,
  numbytes: Byte): Byte;
procedure GetMonoFonts;
{$IFNDEF NOFPCW}
function GetFPImageReaderForFileExtension(const FileExt: string): TFPCustomImageReaderClass;
{$ENDIF}

var
  MonoFonts: TStringList;

implementation

const
  GraphicTypes: array[1..7] of TGraphicType = (
      (Offset: 0; Len: 2;  Signature: 'BM'; Extension: '.bmp'),  // BMP
      (Offset: 6; Len: 4; Signature: 'JFIF'; Extension: '.jpg'),// JPG
      (Offset: 6; Len: 5; Signature: 'Exif'; Extension: '.jpg'),// JPG
      (Offset: 1; Len: 3; Signature: 'PNG'; Extension: '.png'), // PNG
      (Offset: 0; Len: 3; Signature: 'GIF'; Extension: '.gif'), // GIF
      (Offset: 8; Len: 4; Signature: 'WEBP'; Extension: '.webp'), // WEBP
      (Offset: 0; Len: 3; Signature: 'II*'; Extension: '.tiff')  // TIFF
    );

function GetImageExtensionBySignature(Stream: TMemoryStream): AnsiString;
var
  I: Integer;
begin
  Result:= '';
  for I := Low(GraphicTypes) to High(GraphicTypes) do
  with GraphicTypes[I] do
  begin
    if CompareMem(Pointer(Cardinal(Stream.Memory) + Offset),
      Signature, Len) then
    begin
      {$IFNDEF NOFPCW}
      Result:= StrPas(Extension);
      {$ELSE}
      Result:= System.AnsiStrings.StrPas(Extension);
      {$ENDIF}
      exit;
    end;
  end;
end;

function GetImageExtensionBySignature(FileName: String): AnsiString;
var
  f: TMemoryStream;
begin
  f:= TMemoryStream.Create;
  try
    f.LoadFromFile(FileName);
    Result:= GetImageExtensionBySignature(f);
  finally
    f.Free;
  end;
end;

function ArrayToAnsiString(const a: array of AnsiChar;
  len: Integer = 0; start: Integer = 0): AnsiString;
var
  i: Integer;
begin
  if len = 0 then
  begin
    len:= Length(a);
    for i:= start to High(a) do
      if a[i] = #0 then
      begin
        len:= i;
        break;
      end;
  end;
  if len > 0 then
    SetString(Result, PAnsiChar(@a[start]), len)
  else
    Result := '';
end;

function GetBitsValue(sourcevalue, posbyte,
  numbytes: Byte): Byte;
begin
  Result:= Byte(sourcevalue shl posbyte);
  Result:= Byte(Result shr (8 - numbytes));
end;

function MonoCallback(var f: TEnumLogFontEx; var Metric: TNewTextMetricEx;
    FontType: Longint; Data:LParam): LongInt; stdcall;
var
  I: Integer;
  p: PFontData;
  b: Byte;
  tp: Boolean;
begin
  b:= f.elfLogFont.lfPitchAndFamily;
  tp:= true;
  if FIXED_PITCH and b = 0 then
    tp:= b AND $F0 = FF_MODERN;
  if tp then
  begin
    I:= 0;
    while I < MonoFonts.Count do
    begin
      if CompareText(MonoFonts[I], f.elfLogFont.lfFaceName) = 0 then
        with PFontData(IntPtr(MonoFonts.Objects[I]))^ do
          if (Height = f.elfLogFont.lfHeight) and (Width = f.elfLogFont.lfWidth)
            and (Weight = f.elfLogFont.lfWeight) and
            (Italic = f.elfLogFont.lfItalic) then
            break;
      Inc(I);
    end;
    if I = MonoFonts.Count then
    begin
      New(p);
      p^.Height:= f.elfLogFont.lfHeight;
      p^.Width:= f.elfLogFont.lfWidth;
      p^.Weight:= f.elfLogFont.lfWeight;
      p^.Italic:= f.elfLogFont.lfItalic;
      MonoFonts.AddObject(f.elfLogFont.lfFaceName, TObject(IntPtr(p)));
    end;
  end;
  Result:= 1;
end;

procedure GetMonoFonts;
var
  f: TLOGFONT;
  dc: HDC;
begin
  f.lfCharSet:= ANSI_CHARSET;
  f.lfFaceName[0]:= #0;
  f.lfPitchAndFamily:= 0;
  dc:= GetDC(0);
  try
    EnumFontFamiliesEx(dc, {$IFNDEF NOFPCW}@{$ENDIF}f, @MonoCallBack, 0, 0);
  finally
    ReleaseDC(0, dc);
  end;
  ReleaseDC(0, dc);
end;

procedure FreeMonoFonts;
var
  I: Integer;
begin
  for I:= 0 to MonoFonts.Count -1 do
    Dispose(PFontData(IntPtr(MonoFonts.Objects[I])));
  MonoFonts.Free;
end;

{$IFNDEF NOFPCW}
function GetFPImageReaderForFileExtension(const FileExt: string): TFPCustomImageReaderClass;
begin
  if FileExt='.bmp' then
    Result:=TFPReaderBMP;
  if (FileExt='.jpg') or (FileExt='.jpeg') then
    Result:=TFPReaderJPEG;
  if FileExt='.png' then
    Result:=TFPReaderPNG;
  if FileExt='.xpm' then
    Result:=TFPReaderXPM;
  if FileExt='.tiff' then
    Result:=TFPReaderTiff;
end;
{$ENDIF}

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


initialization
  MonoFonts:= TStringList.Create;

finalization
  FreeMonoFonts;

end.

