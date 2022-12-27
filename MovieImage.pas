unit MovieImage;

{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}


// Made by thisuser667, except gif decoding.

interface

uses
  // Define DOPLAINTEXT when you want to implement gif plain text extensions.
  // (https://entropymine.wordpress.com/2018/06/23/gif-plain-text-extensions/)
  // That means almost never, but in FPC, if defined, I couldn't achieve
  // transparency.
  {$IFNDEF FPC and IFDEF WINDOWS}
  {$DEFINE NOFPCW}
  {$DEFINE DOPLAINTEXT}
  WinApi.Windows, Winapi.Messages, Winapi.WinCodec, System.SysUtils,
    System.AnsiStrings, Vcl.Graphics, System.Classes, System.Math,
    Vcl.ExtCtrls, System.Contnrs, Vcl.Forms, Vcl.Dialogs,
    System.Types, Vcl.Controls, System.UiTypes,
  {$ELSE}
  {$DEFINE DOPLAINTEXT}
  LclType, LMessages, LclIntF, Types, Controls, IntFGraphics,
    SysUtils, Graphics, Classes, Math, ExtCtrls, GraphType,
    Contnrs, Forms, Dialogs, FpImage,
  {$ENDIF}
  MovieImageAuxUnit, libwebp124, uMsgDlg;

type
  TBGrTriple = record
    rgbRed, rgbGreen, rgbBlue: Byte;
  end;
  PBGrTriple = ^TBGrTriple;

  TBGrTripleArray = array of TBGrTriple;
  PBGrTripleArray = ^TBGrTripleArray;

  TRGBQuadArray = array of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;
  TRgbTripleArray = array of TRGBTriple;
  PRgbTripleArray = ^TRGBTripleArray;

  TMovieImageBase = class;

  {$IFDEF NOFPCW}
  PWpQuad = ^TWpQuad;
  TWpQuad = record
    rgbRed: Byte;
    rgbGreen: Byte;
    rgbBlue: Byte;
    rgbReserved: Byte;
  end;
  TWpQuadArray = array of TWpQuad;
  PWpQuadArray = ^TWpQuadArray;
  {$ENDIF}

  TDisposalMethod = (dmUndefined, dmNotDisposal, dmBackgroundColor,
    dmRestorePrev);

  TPlainTextBlock = record   // 0x01 plain text extension identifier
    CellSizeX, CellSizeY: Byte;
    ForeColorIndex, BackColorIndex: Byte; // index to color table
    TextData: array of AnsiChar;
    FontName: array[0..LF_FACESIZE - 1] of Char;
    FontSize: Integer;
  end;
  PPlainTextBlock = ^TPlainTextBlock;

  TMovieFrame = class
  protected
    {BlendedBits: TBytes; // bytes that are reserved for each loop
      // there is not very much time gained for only blend them
      // once.
      // have been enough.}
    Disposal: TDisposalMethod;
    FrameWidth, FrameHeight, FrameLeft, FrameTop, Delay: Word;
    DecodedBits: TBytes;
    TextBlock: PPLainTextBlock;
  public
    //Blended: Boolean;  // maybe length(blendedbits) > 0 would
    constructor Create;
    destructor Destroy; override;
  end;

  TGifFrame = class(TMovieFrame)
  protected
    Interlaced: Boolean;
    {$IFDEF NOFPCW}
    LocalColorTable: TBgrTripleArray;
    {$ELSE}
    LocalColorTable: TRGbTripleArray;
    {$ENDIF}
    //TableSorted: Boolean;  it was only useful with less than 256 colors
    UserInput: Boolean;
    TransparentColorIndex: SmallInt; // -1 if not transparent index flag
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TWebpFrame = class(TMovieFrame)
  protected
    AlphaCompressed: Byte;  // 0 = not Alpha, 1 = Alpha not compresed;
    Blending: Boolean;  // this frame has to be blended instead of
      // just drawed over the previous frame. (just transparent pixels)
    Filtering: Byte;  // just for encoding
    Lossless: Boolean;  // not very important, just to check that
      // if it's lossless it can't have alpha.
    PreProcessing: Boolean;  // should have been implemented.
      // read about it in the webp container specification.
  end;

  TMovieFrameList = class(TObjectList)  // container of the frames.
  private
    MyBase: TMovieImageBase;
    function GetFrame(Index: Integer): TMovieFrame;
    procedure SetFrame(Index: Integer; Value: TMovieFrame);
  public
    constructor Create(Parent: TMovieImageBase);
    procedure AssignFrame(SourceFrame: TMovieFrame;
      var DestFrame: TMovieFrame);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    property Frame[Index: Integer]: TMovieFrame read GetFrame
      write SetFrame; default;
  end;

  TBeforeDrawingFrameEvent = procedure(Sender: TObject) of
    object;
  TAfterFrameDrawnEvent = procedure(Sender: TObject) of
    object;

  TAnimationSpeed = 1..1000;

  { TImageBase }

  TImageBase = class({$IFDEF NOFPCW}TWicImage{$ELSE}Graphics.TBitmap{$ENDIF})
  private
    FCanvas: TCanvas;
    FCanvasRect: TRect;
    FImageWidth, FImageHeight: Integer;
    procedure SetCanvas(Value: TCanvas);
    function GetImagewidth: Integer;
    function GetImageHeight: Integer;
  public
    constructor Create(NewCanvas: TCanvas = nil); virtual;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer); virtual;
    procedure LoadFromFile(const FileName: string); {$IFDEF NOFPCW}override;
      {$ELSE} virtual;{$ENDIF}
    property Canvas: TCanvas read FCanvas write SetCanvas;
    property CanvasRect: TRect read FCanvasRect write FCanvasRect;
    property ImageWidth: Integer read GetImageWidth;
    property ImageHeight: Integer read GetImageHeight;
  end;

  { TMovieImageBase }

  TMovieImageBase = class(TImageBase)
  private
    FLoopIndex: Integer;  // infinite loop can be overridden
      // with InfiniteLoopAnyWays, despite almost all of the webp
      // files have intinite looping just the same.
    FAnimateOnGetCanvas: Boolean;  // internal processing.
    FAnimationSpeed: TAnimationSpeed; // 50 = normal speed div 2
    FCurrentBits: TBytes;  // the canvas bits in each iteration
    FFileName: string;
    FNeedCompleteRedraw: Boolean;  // as blending changes
      // as the loop progresses, if one frame is drawn
      // from the user app, all the previous frames have to be
      // rendered.
    FOnBeforeDrawingFrame: TBeforeDrawingFrameEvent;
    FOnAfterFrameDrawn: TAfterFrameDrawnEvent;
    FOnLoop: TNotifyEvent;
    FFrameIndex: Integer;
    FTimer: TTimer;
    FCurrentFrame: Integer; // as said.
    FExternalDrawing: Boolean;  // if true doesn't process draw
    FOnNextFrame: TNotifyEvent;
    {$IFNDEF NOFPCW}
    FInfo: BITMAPINFO;
    FFileHeader: BITMAPFILEHEADER;
    {$ENDIF}
    FMinimum: Word;
    procedure DrawDecodedBytes(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer; BlendedBits: TBytes);
    function GetAnimating: Boolean;
    function GetPaused: Boolean;
    procedure AnimateTimer(Sender: TObject); // internal ontimer event
  protected
    function DecodeFrame(EncodedStream: TStream;
      pfIndex: Integer): Boolean; virtual; abstract;
      // called while loading or at the end of the loading.
      // As implemented, it just relies on high level dll
      // procedures that return bytes almost read to be drawn.
    {function GetHeight: Integer; override;
    function GetWidth: Integer; override;}
  public
    Frames: TMovieFrameList;
    LoopCount: Word;
    class var SourceAnimated: Byte;
    constructor Create(NewCanvas: TCanvas = nil); override;
    destructor Destroy; override;
    class function CanBeAnimated(FName: string = ''): Boolean;
      virtual; abstract;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer); override; abstract;
    procedure Animate;
    procedure StopAnimation;
    procedure LoadFromFile(const Filename: string); override;
    {$WARNINGS OFF}
    procedure LoadFromStream(Stream: TMemoryStream); virtual; abstract;
    {$WARNINGS ON}
    procedure PauseAnimation;
    procedure RestoreAnimation;
    function LoopDurationinMs: Cardinal; // sum of all the delays
    class function ValidGifFile(Stream: TStream): Boolean;
    property AnimationSpeed: TAnimationSpeed read FAnimationSpeed
      write FAnimationSpeed default 100;
    property Animating: Boolean read GetAnimating;
    property CurrentFrame: Integer read FCurrentFrame write
      FCurrentFrame;
    property ExternalDrawing: Boolean write FExternalDrawing;
      // it's not interesting to get this value, but to set it.
    property FileName: string read FFileName;
    property LoopIndex: Integer read FLoopIndex write FLoopIndex;
    property IsPaused: Boolean read GetPaused;
    property FrameIndex: Integer read FFrameIndex;
    property NeedCompleteRedraw: Boolean read FNeedCompleteRedraw
      write FNeedCompleteRedraw;
    property OnBeforeDrawingFrame: TBeforeDrawingFrameEvent read
      FOnBeforeDrawingFrame write FOnBeforeDrawingFrame;
    property OnAfterFrameDrawn: TAfterFrameDrawnEvent read
      FOnAfterFrameDrawn write FOnAfterFrameDrawn;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnNextFrame: TNotifyEvent read FONNextFrame write
      FOnNextFrame;
  end;

  TNoMovieBaseImage = class(TMovieImageBase)

  end;

  TMovieGifImage = class(TMovieImageBase)
  private
    FBackgroundIndex: Byte; // index to global color table
    FPlainTextFontName: string;
    //FColorResolution: Byte; used only for encoding
    // GlobalTableSorted: was only useful with less than 256 colors
    FPixelRatio: Byte; // non-square pixels, apply correction in drawing
    FBackgroundColor: Byte;
    procedure SetPlainTextFontName(Value: string);
  protected
    function DecodeFrame(EncodedStream: TStream;
      pfIndex: Integer): Boolean; override;
    {$IFDEF NOFPCW}
    procedure DrawBackground(ARect: TRect;
      table: TBgrTripleArray);
    {$ELSE}
    procedure DrawBackground(ARect: TRect;
      table: TRGbTripleArray);
    {$ENDIF}
  public
    constructor Create(NewCanvas: TCanvas = nil); override;
    class function CanBeAnimated(Fname: string = ''): Boolean; override;
    procedure LoadFromStream(Stream: TMemoryStream); override;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer); override;
    property PlainTextFontName: string read FPlainTextFontName
      write SetPlainTextFontName;
  end;

  TMovieWebpImage = class(TMovieImageBase)
  protected
    HasExifData: Boolean;  // not implemented
      // chunks without implementation can be just ignored
    HasFramesWithAlpha: Boolean;
    HasICCP: Boolean; // should be implemented in a more professional
      // implementation.
    HasXMPData: Boolean; // not implemented
    // All the next overrides, necessary because TGraphic has
    {$IFDEF NOFPCW}
    FBackgroundColor: TWpQuad;
    {$ELSE}
    FBackgroundColor: TRGbQuad;  // background color inside the file
    {$ENDIF}
    function DecodeFrame(EncodedStream: TStream;
      pfIndex: Integer): Boolean; override;
    // them as abstract.
    procedure DrawBackground(ARect: TRect);
    {$IFDEF NOFPCW}
    procedure SetBackgroundColor(Value: TWpQuad);
    {$ELSE}
    procedure SetBackgroundColor(Value: TRGbQuad);
    {$ENDIF}
  public
    constructor Create(NewCanvas: TCanvas = nil); override;
    class function CanBeAnimated(Fname: string = ''): Boolean; override;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer); override;
    procedure LoadFromStream(Stream: TMemoryStream); override;
    {$IFDEF NOFPCW}
    property BackgroundColor: TWpQuad read FBackgroundColor
      write SetBackgroundColor;
    {$ELSE}
    property BackgroundColor: TRgbQuad read FBackgroundColor
      write SetBackgroundColor;
    {$ENDIF}
  end;

var
  ErrorOnBadFrame: Boolean = true;
  InfiniteLoopAnyWays: Boolean = false;
  AnimateOnLoaded: Boolean = true;
  {$IFDEF NOFPCW}
  GlobalTable: TBGrTripleArray;
  {$ELSE}
  GlobalTAble: TRGBTripleArray;
  {$ENDIF}

implementation

const
  sWebpImage = 'Webp Image File';

constructor TMovieImageBase.Create(NewCanvas: TCanvas = nil);
begin
  inherited Create;
  SourceAnimated:= 0;
  FAnimationSpeed:= 100;
  SetLength(FCurrentBits, 0);
  FTimer:= nil;
  Frames:= TMovieFrameList.Create(Self);
  FImageWidth:= 0;
  FImageHeight:= 0;
  FFileName:= '';
  FNeedCompleteRedraw:= false;
  SetCanvas(NewCanvas);
  {$IFNDEF NOFPCW}
  FillChar(FInfo, sizeof(BITMAPINFOHEADER), 0);
  with FInfo do
  begin
    bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    bmiHeader.biPlanes := 1;
    bmiHeader.biCompression := BI_RGB;
  end;
  with FFileHeader do
  begin
  	bfType := $4D42;
  	bfOffBits := sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER);
  	bfReserved1 := 0;
  	bfReserved2 := 0;
  end;
  {$ENDIF}
end;

destructor TMovieImageBase.Destroy;
begin
  FreeAndNil(FTimer);
  Frames.Free;
  inherited;
end;

const
  USER_TIMER_MINIMUM = $0000000A;
  GIFFIXEDMINIMUM = $64; // when all delays in file are <= 1 (10 ms)
  {$IFDEF NOFPCW}UOI_TIMERPROC_EXCEPTION_SUPPRESSION = 7;{$ENDIF}

procedure TMovieImageBase.Animate;
var
  {$IFDEF NOFPCW}h: THandle;{$ENDIF}
  b: LONGBOOL;
begin
  if (LoopIndex > 0) or (Frames.Count < 2) or (FTimer <> nil) or
    (Sourceanimated < 2) then
    exit;
  if FCanvas = nil then
  begin
    FAnimateOnGetCanvas:= true;
    exit;
  end;
  {$IFDEF NOFPCW}
  h:= GetCurrentProcess;
  if h <> 0 then
  begin
    b:= false;
    if not SetUserObjectInformationW(h,
      UOI_TIMERPROC_EXCEPTION_SUPPRESSION, @b, sizeof(LONGBOOL)) then
      exit;    // in settimer api webpage this is recommended.
               // it can be removed or implemented but
               // without exit on error if desired.
  end;
  {$ENDIF}
  FCurrentFrame:= 0;
  if Ftimer <> nil then
    FTimer.Free;
  FTimer:= TTimer.Create(nil);
  FTimer.Interval:= USER_TIMER_MINIMUM;
  FTimer.Enabled:= true;
  FTimer.OnTimer:= {$IFNDEF NOFPCW}@{$ENDIF}AnimateTimer;
  FLoopIndex:= 1;
end;

procedure TMovieImageBase.AnimateTimer(Sender: TObject);
var
  Delay: Integer;
  MsgDlgEx: TMsgDlgEx;
  {$IFDEF NOFPCW}
  Wnd: HWND;
  stext: string;
  {$ENDIF}
begin
  try
    {$IFDEF NOFPCW}
    Wnd := GetActiveWindow;
    if IsWindow(Wnd) then
    begin
      SetLength(stext, MAX_PATH);
      SetLength(stext, GetWindowtext(Wnd, PChar(stext), MAX_PATH));
      if stext = 'Confirm' then
      exit;
    end;
    {$ENDIF}
    FTimer.Enabled:= false;
    FExternalDrawing:= true;
    FNeedCompleteRedraw:= FCurrentFrame = 0;
    if IsRectEmpty(FCanvasRect) then
      FCanvasRect:= FCanvas.ClipRect;
    DrawFrame(FCanvas, FCanvasRect, FCurrentFrame);
    FNeedCompleteRedraw:= true; // needcompleteredraw always
      // true if calling drawframe externally
    //Delay:= Frames[FCurrentFrame].Delay;
    Delay:= Max(FMinimum, Min(10000, Frames.Frame[FCurrentFrame].Delay));
    if FrameIndex < Frames.Count -1 then
    begin
      if (Self is TMovieGifImage) and
        TGifFrame(Frames.Frame[FCurrentFrame]).UserInput then
      begin
        if Frames.Frame[FCurrentFrame].Delay = 0 then
        begin
          {$IFNDEF NOFPCW}
          MessageDlg('Warning', 'Press ENTER or ESCAPE to continue',
            mtConfirmation, [mbOk], '');
          {$ELSE}
          MessageDlg('Press ENTER or ESCAPE to continue',
            mtConfirmation, [mbOk], 0);
          {$ENDIF}
        end
        else
        begin
          {CloseTimer:= FTimer;
          TimerId := SetTimer(0, 0, 2000, @CloseMessageBox);
          Application.MessageBox(
            PChar('Warning'),
            PChar('Press ENTER or ESCAPE to continue'),
            MB_OK);
          KillTimer(0, Timerid);
         // exit;}
          MsgDlgEx:= TMsgDlgEx.Create(
            'Pulsa Intro o Escape para continuar',
            mtConfirmation, [mbOk], Application.MainForm, mrOk, Delay);
          try
            msgDlgEx.ShowDialog;
          finally
            MsgDlgEx.Free;
          end;
        end;
        FTimer.Interval:= 10;
      end
      else
        FTimer.Interval:= Round(Delay / FAnimationSpeed * 100);
    end;
    FTimer.Enabled:= true;
    if FCurrentFrame < Frames.Count -1 then
    begin
      Inc(FCurrentFrame);
      if Assigned(FOnNextFrame) then
        FOnNextFrame(Sender);
    end
    else if (LoopCount = 0) or (FLoopIndex < LoopCount) or
      InfiniteLoopAnyways then
    begin
      FCurrentFrame:= 0;
      Inc(FLoopIndex);
      if Assigned(FOnLoop) then
        FOnloop(Self);
    end
    else
    begin
      FTimer.Enabled:= false;
      FLoopIndex:= 0;
      FreeAndNil(FTimer);
    end;
  except
    floopindex:= 0;
  end;
end;

class function TMovieImageBase.ValidGifFile(Stream: TStream): Boolean;
var
  gif: array[0..2] of Byte;
  iver: Boolean;
begin
  Result:= false;
  Stream.Read(gif, 3);
  if ((gif[0] <> $47) and (gif[0] <> $50)) or (gif[1] <> $49) or
    (gif[2] <> $46) then
    exit;
  iver:= Boolean(gif[0] = $47);
  Stream.Read(gif, 3);
  if iver then
  begin
    if not ((gif[0] = $38) and (gif[1] = $39) and
      (gif[2] = $61)) and
      not ((gif[0] = $38) and (gif[1] = $37) and
      (gif[2] = $61)) then
      exit;
  end
  else if not ((gif[0] = $39) and (gif[1] = $39) and
    (gif[2] = $61)) then
    exit;
  Result:= true;
end;

procedure TMovieImageBase.DrawDecodedBytes(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer; BlendedBits: TBytes);
var
  {$IFDEF NOFPCW}
  curbitmap, wicbitmap: IWicBitmap;
  canvasstride: Integer;
  pixelFormat: WICPixelFormatGUID;
  {$ELSE}
  f: TLazIntfImage;
  r: TRawImage;
  {$ENDIF}
begin
  with Frames.Frame[pfIndex] do
  begin
    {$IFDEF NOFPCW}
    if Self is TMovieGifImage then
    begin
      canvasstride:= ((((FImageWidth * 24) + 23) and not 23)
          shr 3);
      pixelformat:= GUID_WICPixelFormat24bppRGB;
    end
    else
    begin
      canvasstride:= ((((FImageWidth * 32) + 31) and not 31)
          shr 3);
      pixelformat:= GUID_WICPixelFormat32bppRGBA;
    end;
    if ImagingFactory.CreateBitmapFromMemory(FImageWidth, FImageHeight,
      pixelformat, canvasstride, Length(BlendedBits), @BlendedBits[0],
      wicbitmap) = S_OK then
    begin
      curbitmap:= Self.Handle;
      try
        Self.Handle:= wicbitmap;
        Self.ImageFormat:= wifJpeg;
        //Self.SaveToFile('g:\poya.jpg');
        ACanvas.StretchDraw(ARect, Self);
      finally
        if curbitmap <> nil then
        begin
          Self.Handle:= curbitmap;
          wicbitmap:= nil;
        end;
      end;
    end;
    {$ELSE}
    r.Init;
    if Self is TMovieGifImage then
      r.Description.Init_BPP24_B8G8R8_BIO_TTB(FImageWidth, FImageHeight)
    else
      r.Description.Init_BPP32_B8G8R8A8_BIO_TTB(FImageWidth, FImageHeight);
    r.CreateData(true);
    r.Data:= @BlendedBits[0];
    r.DataSize:= Length(BlendedBits);
    f:= TLazIntFImage.Create(r, false);
    Graphics.TBitmap(Self).LoadFromIntfImage(f);
    ACanvas.StretchDraw(ARect, Graphics.TBitmap(Self));
    f.Free;
    {$ENDIF}
  end;
end;

function TMovieImageBase.GetAnimating: Boolean;
begin
  Result:= LoopIndex > 0;
end;

function TMovieImageBase.GetPaused: Boolean;
begin
  Result:= (fTimer <> nil) and not FTimer.Enabled;
end;

procedure TMovieImageBase.LoadFromFile(const Filename: string);
var
  Stream: TMemoryStream;
  fourcc: array[0..3] of AnsiChar;
  filesize: Integer;
  bwebp: Boolean;
begin
  Frames.Clear;
  Stream:= TMemoryStream.Create;
  bwebp:= true;
  try
    Stream.LoadFromFile(FileName);
    if Self is TMovieGifImage then
    begin
      if not ValidGifFile(Stream) then
        raise Exception.Create('Not a valid gif file')
      else
      begin
        Stream.Position:= 0;
        FFileName:= Filename;
        TMovieGifImage(Self).LoadFromStream(Stream);
      end;
    end
    else
    begin
      bwebp:= false;
      Stream.Read(fourcc, 4);
      if ArrayToAnsiString(fourcc) <> 'RIFF' then
        exit;
      Stream.Read(filesize, 4);
      if Stream.Size < filesize + 8 then
        exit;
      Stream.Read(fourcc, 4);
      if ArrayToAnsiString(fourcc) <> 'WEBP' then
        exit;
      bwebp:= true;
    end;
  finally
    if not bwebp then
      raise Exception.Create('Not a valid webp file')
    else if Self is TMovieWebpImage then
    begin
      Stream.Position:= 0;
      FFileName:= Filename;
      TMovieWebpImage(Self).LoadFromStream(Stream);
    end;
    Stream.free;
  end;
end;

function TMovieImageBase.LoopDurationinMs: Cardinal;
var
  I: Integer;
begin
  Result:= 0;
  if (Frames.Count < 2) or (SourceAnimated < 2) then
    exit;
  for I:= 0 to Frames.Count -1 do
    Inc(Result, Max(FMinimum, Frames.Frame[I].Delay));
end;

procedure TMovieImageBase.PauseAnimation;
begin
  if (FTimer <> nil) and Ftimer.Enabled then
    FTimer.Enabled:= false;
end;

procedure TMovieImageBase.RestoreAnimation;
begin
  if (FTimer <> nil) and not Ftimer.Enabled then
    FTimer.Enabled:= true;
end;

procedure TMovieImageBase.StopAnimation;
begin
  if (FTimer = nil) or not FTimer.Enabled then
    exit;
  FloopIndex:= 0;
  FreeAndNil(FTimer);
end;

{ TMovieFrameList }

procedure TMovieFrameList.AssignFrame(SourceFrame: TMovieFrame;
  var DestFrame: TMovieFrame);
begin
  if MyBase is TMovieWebpImage then
  with TWebpFrame(DestFrame) do
  begin
    AlphaCompressed:= TWebpFrame(SourceFrame).AlphaCompressed;
    Blending:= TWebpFrame(Sourceframe).Blending;
    Filtering:= TWebpFrame(SourceFrame).Filtering;
    Lossless:= TWebpFrame(SourceFrame).Lossless;
    PreProcessing:= TWebpFrame(SourceFrame).PreProcessing;
  end
  else
  with TGifFrame(DestFrame) do
  begin
    Interlaced:= TGifFrame(SourceFrame).Interlaced;
    LocalColorTable:= TGifFrame(SourceFrame).LocalColorTable;
    TransparentColorIndex:= TGifFrame(SourceFrame).TransparentColorIndex;
    UserInput:= TGifFrame(SourceFrame).UserInput;
  end;
  with DestFrame do
  begin
    DecodedBits:= Copy(Sourceframe.DecodedBits, 0, High(Integer));
    Disposal:= SourceFrame.Disposal;
    FrameWidth:= SourceFrame.FrameWidth;
    FrameHeight:= SourceFrame.FrameHeight;
    FrameLeft:= SourceFrame.FrameLeft;
    FrameTop:= SourceFrame.FrameTop;
    Delay:= Sourceframe.Delay;
  end;
end;

procedure TMovieFrameList.Clear;
var
  I: Integer;
begin
  for I:= Count -1 downto 0 do
    Delete(I);
end;

constructor TMovieFrameList.Create(Parent: TMovieImageBase);
begin
  inherited Create(false);
  Self.MyBase:= Parent;
end;

procedure TMovieFrameList.Delete(Index: Integer);
begin
  if MyBase is TMovieGifImage then
    TGifFrame(Items[Index]).Free
  else
    TWebpFrame(Items[Index]).Free;
  inherited;
end;

destructor TMovieFrameList.Destroy;
begin
  Clear;
  inherited;
end;

function TMovieFrameList.GetFrame(Index: Integer): TMovieFrame;
begin
  Result:= TMovieFrame(Items[Index]);
end;

procedure TMovieFrameList.SetFrame(Index: Integer;
  Value: TMovieFrame);
begin
  AssignFrame(TMovieFrame(Items[Index]), Value);
end;

class function TMovieGifImage.CanBeAnimated(Fname: string = ''): Boolean;
const
  TrailerByte: Byte = $3B;
var
  packedfields, sep: Byte;
  colortablesize: Word;
  m: TmemoryStream;
  minlzwsize: Byte;
  extensiontype: Byte;
  blocklength: Byte;
  fillbytes: Integer;
  imagecount: Byte;

  function BlockRight(blength: Integer = -1): Boolean;
  var
    setendblock: Boolean;
  begin
    setendblock:= blength = -1;
    if setendblock then
      blength:= blocklength;
    Result:= true;
    fillbytes:= m.Position + blength;
    if m.Size - fillbytes <= 0 then  // corruption
      Result:= false;
  end;

  procedure ProcessBlock;
  begin
    repeat
      m.Read(blocklength, 1);
      if blocklength > 0 then
      begin
        if BlockRight then
          m.Seek(blocklength, soCurrent)
        else
          sep:= $00;
      end;
    until (blocklength = 0) or (sep = $00);
  end;

begin
  Result:= SourceAnimated = 2;
  if Result then
    exit;
  if not FileExists(Fname) then
    exit;
  m:= TmemoryStream.Create;   // I better not reuse the load stream method because
                              // I want this to be a class method.
  try
    m.LoadFromFile(FName);
    imagecount:= 0;
    if not ValidGifFile(m) then
      exit;
    m.Seek($A, soBeginning);
    m.Read(packedfields, 1);
    m.Seek(2, soCurrent);
    if GetBitsValue(packedfields, 0, 1) = 1 then
    begin
      colortablesize:= Round(Power(2, GetBitsValue(packedfields, 5, 3) + 1) * 3);
      if not BlockRight(colortablesize) then
        exit;
      m.Seek(colortablesize, soCurrent);
    end;
    repeat
      m.Read(sep, 1);
      if sep = $2C then
      begin
        Inc(imagecount);
        if imagecount = 2 then
          exit;
        m.Seek(8, soCurrent);
        m.Read(packedfields, 1);
        if GetBitsValue(packedfields, 0, 1) = 1 then
        begin
          colortablesize:= Round(Power(2, GetBitsValue(packedfields, 5,3) + 1) * 3);
          if not BlockRight(colortablesize) then
            sep:= $00
          else
            m.Seek(colortablesize, soCurrent);
        end;
        m.Read(minlzwsize, 1);
        ProcessBlock;
      end
      else if sep = $21 then
      begin
        m.Read(extensiontype, 1);
        case extensiontype of
          $F9: // control extension
          begin
            m.Read(blocklength, 1);
            if blocklength = 4 then
              m.Seek(5, soCurrent)
            else
              m.Seek(-1, socurrent);
          end;
          $FE: ProcessBlock;
          $01: // plain text extension
          begin
            m.Read(blocklength, 1);
            if blocklength <> 12 then
            begin
              m.Seek(-1, soCurrent);
              continue;
            end;
            m.Seek(blocklength, soCurrent);
            ProcessBlock;
          end;
          $FF: // application extension
          begin
            m.Read(blocklength, 1);
            if blocklength <> 11 then
            begin
              m.Seek(-1, soCurrent);
              continue;
            end;
            if BlockRight then
            begin
              m.Seek(blocklength, soCurrent);
              ProcessBlock;
            end
            else
              sep:= $00;
          end;
        end;
      end;
    until ((sep <> $2C) and (sep <> $21));
  finally
    m.free;
  end;
end;

procedure ReadCheck(Stream: TStream; var Buffer; Size: LongInt);
var
  ReadSize		: integer;
begin
  ReadSize := Stream.Read(Buffer, Size);
  if ReadSize <> Size then
    raise Exception.Create('Corruption');
end;

constructor TMovieGifImage.Create(NewCanvas: TCanvas = nil);
begin
  inherited Create(NewCanvas);
  {$IFNDEF NOFPCW}
  FInfo.bmiHeader.biBitCount := 24;
  {$ENDIF}
  FBackgroundColor:= 0;
  SourceAnimated:= 2;
  FPlainTextFontName:= '';
end;

// gif decoding source algorithm:
// http://www.tolderlund.eu/delphi/gifimaged2010b.zip

const
  GIFCodeBits		= 12;			// Max number of bits per GIF token code
  GIFCodeMax		= (1 SHL GIFCodeBits)-1;// Max GIF token code
  						// 12 bits = 4095
  StackSize		= (2 SHL GIFCodeBits);	// Size of decompression stack
  TableSize		= (1 SHL GIFCodeBits);	// Size of decompression table

function TMovieGifImage.DecodeFrame(EncodedStream: TStream;
  pfIndex: Integer): Boolean;
var
  table0		: array[0..TableSize-1] of integer;
  table1		: array[0..TableSize-1] of integer;
  firstcode, oldcode	: integer;
  buf			: array[0..257] of BYTE;

  Dest			: PByte;
  v			,
  xpos, ypos, pass	: integer;

  stack			: array[0..StackSize-1] of integer;
  Source			: ^integer;
  BitsPerCode		: integer;		// number of CodeTableBits/code
  InitialBitsPerCode	: BYTE;

  MaxCode		: integer;		// maximum code, given BitsPerCode
  MaxCodeSize		: integer;
  ClearCode		: integer;		// Special code to signal "Clear table"
  EOFCode		: integer;		// Special code to signal EOF
  step			: integer;
  i			: integer;
  StartBit		,			// Index of bit buffer start
  LastBit		,			// Index of last bit in buffer
  LastByte		: integer;		// Index of last byte in buffer
  get_done		,
  return_clear		,
  ZeroBlock		: boolean;
  ClearValue		: BYTE;
{$ifdef DEBUG_DECOMPRESSPERFORMANCE}
  TimeStartDecompress	,
  TimeStopDecompress	: DWORD;
{$endif}

  function nextCode(BitsPerCode: integer): integer;
  const
    masks: array[0..15] of integer =
      ($0000, $0001, $0003, $0007,
       $000f, $001f, $003f, $007f,
       $00ff, $01ff, $03ff, $07ff,
       $0fff, $1fff, $3fff, $7fff);
  var
    StartIndex, EndIndex		: integer;
    ret			: integer;
    EndBit		: integer;
    count		: BYTE;
  begin
    if (return_clear) then
    begin
      return_clear := False;
      Result := ClearCode;
      exit;
    end;

    EndBit := StartBit + BitsPerCode;

    if (EndBit >= LastBit) then
    begin
      if (get_done) then
      begin
        {if (StartBit >= LastBit) then
          Warning(gsWarning, sDecodeTooFewBits);}
        Result := -1;
        exit;
      end;
      buf[0] := buf[LastByte-2];
      buf[1] := buf[LastByte-1];

      if (EncodedStream.Read(count, 1) <> 1) then
      begin
        Result := -1;
        exit;
      end;
      if (count = 0) then
      begin
        ZeroBlock := True;
        get_done := TRUE;
      end else
      begin
        // Handle premature end of file
        if (EncodedStream.Size - EncodedStream.Position < Count) then
        begin
          //Warning(gsWarning, sOutOfData);
          // Not enough data left - Just read as much as we can get
          Count := EncodedStream.Size - EncodedStream.Position;
        end;
        if (Count <> 0) then
          ReadCheck(EncodedStream, Buf[2], Count);
      end;

      LastByte := 2 + count;
      StartBit := (StartBit - LastBit) + 16;
      LastBit := LastByte * 8;

      EndBit := StartBit + BitsPerCode;
    end;

    EndIndex := EndBit DIV 8;
    StartIndex := StartBit DIV 8;

    ASSERT(StartIndex <= high(buf), 'StartIndex too large');
    if (StartIndex = EndIndex) then
      ret := buf[StartIndex]
    else
      if (StartIndex + 1 = EndIndex) then
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8)
      else
        ret := buf[StartIndex] OR (buf[StartIndex+1] SHL 8) OR (buf[StartIndex+2] SHL 16);

    ret := (ret SHR (StartBit AND $0007)) AND masks[BitsPerCode];

    Inc(StartBit, BitsPerCode);

    Result := ret;
  end;

  function NextLZW: integer;
  var
    code, incode	: integer;
    i			: integer;
    b			: BYTE;
  begin
    code := nextCode(BitsPerCode);
    while (code >= 0) do
    begin
      if (code = ClearCode) then
      begin
        ASSERT(ClearCode < TableSize, 'ClearCode too large');
        for i := 0 to ClearCode-1 do
        begin
          table0[i] := 0;
          table1[i] := i;
        end;
        for i := ClearCode to TableSize-1 do
        begin
          table0[i] := 0;
          table1[i] := 0;
        end;
        BitsPerCode := InitialBitsPerCode+1;
        MaxCodeSize := 2 * ClearCode;
        MaxCode := ClearCode + 2;
        Source := @stack;
        repeat
          firstcode := nextCode(BitsPerCode);
          oldcode := firstcode;
        until (firstcode <> ClearCode);

        Result := firstcode;
        exit;
      end;
      if (code = EOFCode) then
      begin
        Result := -2;
        if (ZeroBlock) then
          exit;
        // Eat rest of data blocks
        if (EncodedStream.Read(b, 1) <> 1) then
          exit;
        while (b <> 0) do
        begin
          EncodedStream.Seek(b, soFromCurrent);
          if (EncodedStream.Read(b, 1) <> 1) then
            exit;
        end;
        exit;
      end;

      incode := code;

      if (code >= MaxCode) then
      begin
        Source^ := firstcode;
        Inc(Source);
        code := oldcode;
      end;

      ASSERT(Code < TableSize, 'Code too large');
      while (code >= ClearCode) do
      begin
        Source^ := table1[code];
        Inc(Source);
        if (code = table0[code]) then
          raise Exception.Create('Decode circular');
          //Error(sDecodeCircular);
        code := table0[code];
        ASSERT(Code < TableSize, 'Code too large');
      end;

      firstcode := table1[code];
      Source^ := firstcode;
      Inc(Source);

      code := MaxCode;
      if (code <= GIFCodeMax) then
      begin
        table0[code] := oldcode;
        table1[code] := firstcode;
        Inc(MaxCode);
        if ((MaxCode >= MaxCodeSize) and (MaxCodeSize <= GIFCodeMax)) then
        begin
          MaxCodeSize := MaxCodeSize * 2;
          Inc(BitsPerCode);
        end;
      end;

      oldcode := incode;

      if (longInt(Source) > longInt(@stack)) then
      begin
        Dec(Source);
        Result := Source^;
        exit;
      end
    end;
    Result := code;
  end;

  function readLZW: integer;
  begin
    if (longInt(Source) > longInt(@stack)) then
    begin
      Dec(Source);
      Result := Source^;
    end else
      Result := NextLZW;
  end;

begin
  Result:= false;
  // Clear image data in case decompress doesn't complete
  with TGifFrame(Frames.Frame[pfIndex]) do
  begin
    EncodedStream.Position:= 0;
    if TransparentColorIndex > -1 then
      // Clear to transparent color
      ClearValue := TGifFrame(Frames.Frame[pfIndex]).TransparentColorIndex
    else
      // Clear to first color
      ClearValue := 0;
    SetLength(DecodedBits, FrameWidth * FrameHeight);
    FillChar((@DecodedBits[0])^, FrameWidth * FrameHeight, ClearValue);
    (*
    ** Read initial code size in bits from EncodedStream
    *)
    if EncodedStream.Read(InitialBitsPerCode, 1) <> 1 then
      exit;
  // 2006.07.29 ->
    if InitialBitsPerCode > 8 then
      InitialBitsPerCode := 8;
  // 2006.07.29 <-
    (*
    **  Initialize the Compression routines
    *)
    BitsPerCode := InitialBitsPerCode + 1;
    ClearCode := 1 SHL InitialBitsPerCode;
    EOFCode := ClearCode + 1;
    MaxCodeSize := 2 * ClearCode;
    MaxCode := ClearCode + 2;

    StartBit := 0;
    LastBit := 0;
    LastByte := 2;

    ZeroBlock := False;
    get_done := False;
    return_clear := TRUE;

    Source := @stack;

    try
      if (Interlaced) then
      begin
        ypos := 0;
        pass := 0;
        step := 8;

        for i := 0 to FImageHeight-1 do
        begin
          Dest:= @DecodedBits[FImageWidth * ypos];
          for xpos := 0 to FImageWidth-1 do
          begin
            v := readLZW;
            if (v < 0) then
              exit;
            Dest^ := Byte(v);
            Inc(Dest);
          end;
          Inc(ypos, step);
          if (ypos >= FImageHeight) then
            repeat
              if (pass > 0) then
                step := step DIV 2;
              Inc(pass);
              ypos := step DIV 2;
            until (ypos < FImageHeight);
        end;
      end else
      begin
        Dest := @DecodedBits[0];
        for ypos := 0 to (FImageheight * FImagewidth)-1 do
        begin
          v := readLZW;
          if (v < 0) then
            exit;
          Dest^ := Byte(v);
          Inc(Dest);
        end;
      end;
    finally
      if (readLZW >= 0) then
        ;
  //      raise GIFException.Create('Too much input data, ignoring extra...');
    end;
  end;
end;

{$IFDEF NOFPCW}
procedure TMovieGifImage.DrawBackground(ARect: TRect;
  table: TBgrTripleArray);
{$ELSE}
procedure TMovieGifImage.DrawBackground(ARect: TRect;
  table: TRGbTripleArray);
{$endif}
var
  Row, Col: Integer;
  stride: Integer;
  {$IFDEF NOFPCW}
  P: PBgrTriple;
  {$ELSE}
  P: PRGBTriple;
  {$ENDIF}
begin
  stride:= ((((FImageWidth * 24) + 23) and not 23) shr 3);
  SetLength(FCurrentBits, stride * FImageHeight);
  if Frames.Count > 1 then
  begin
    for Row:= ARect.Top to ARect.Height - 1 do
    begin
      P:= @FCurrentBits[stride * Row + (ARect.Left * 3)];
      Col:= ARect.Width;
      while Col > ARect.Left do
      begin
        P^:= table[FBackgroundColor];
        Inc(P);
        Dec(Col);
      end;
    end;
  end;
end;

function Rect(Left, Top, Right, Bottom: Integer):
  {$IFDEF NOFPCW}System.Types.TRect{$ELSE}Types.TRect{$ENDIF};
begin
  {$IFDEF NOFPCW}
  Result:= System.Types.Rect(Left, Top, Right, Bottom);
  {$ELSE}
  Result:= Types.Rect(Left, Top, Right, Bottom);
  {$ENDIF}
end;

procedure TMovieGifImage.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer);
var
  PreviousBits: TBytes;
  Row, Col, canvasstride, framestride, textstride,
   minx, maxx: Integer;
  P2: PByte;
  I: Integer;
  {$IFDEF NOFPCW}
  colortable: TBgrTripleArray;
  P: PBGrTriple;
  {$ELSE}
  colortable: TRgbTripleArray;
  P: PRGBTriple;
  {$ENDIF}
  bm: Tbitmap;
  stext: AnsiString;
  completedx, cellx, celly, cellsizex, cellsizey: Integer;
  scalex, scaley: Single;
  transb, transg, transr: Byte;
  x, y, slen: Integer;
  BlendedBits: Tbytes;
begin
  if (pfIndex < 0) or (pfIndex > Frames.Count - 1) then
    exit;
  FFrameIndex:= pfIndex;
  if Assigned(FOnBeforeDrawingFrame) then
    FOnBeforeDrawingFrame(Self);
  with TGifFrame(Frames.Frame[pfIndex]) do
  begin
    if Length(LocalColorTable) > 0 then
      colortable:= LocalColorTable
    else
      colortable:= GlobalTable;
    if (Length(FCurrentBits) = 0) or (Acanvas <> FCanvas) or
      (pfIndex = 0) or FNeedCompleteRedraw then
    begin
      DrawBackground(Rect(0, 0, FImageWidth, FImageHeight),
        colortable);
      if ACanvas <> FCanvas then
        FCanvas:= ACanvas;
    end;
    if FNeedCompleteRedraw and (Frames.Count > 1) then
    begin
      FExternalDrawing:= true;
      FNeedCompleteRedraw:= false;
      for I:= 0 to pfIndex - 1 do
        DrawFrame(Acanvas, ARect, I);
      FNeedCompleteRedraw:= true;
    end;
    if (Length(DecodedBits) = 0) and ((TextBlock = nil) or
      (Length(GlobalTable) = 0))  then
      exit;
    try
      if Disposal = dmRestorePrev then
        PreviousBits:= Copy(FCurrentBits, 0, High(Integer))
      else
        SetLength(previousbits, 0);
      if Length(DecodedBits) = 0 then
      begin
        if Length(previousbits) > 0 then
        begin
          BlendedBits:= Copy(PreviousBits, 0, High(Integer));
          DrawDecodedBytes(ACanvas, ARect, pfIndex, BlendedBits);
          SetLength(BlendedBits, 0);
          Finalize(BlendedBits);
        end;
        stext:= ArrayToAnsiString(TextBlock^.TextData);
        bm:= TBitmap.Create;
        try
          with bm do
          begin
            scalex:= ARect.Width / FImageWidth;
            scaley:= ARect.Height / FImageHeight;
            completedx:= 0;
            cellx:= Round(FrameLeft * scalex) + ARect.Left;
            celly:= Round(FrameTop * scaley) + Arect.Top;
            cellsizex:= Round(TextBlock^.CellSizeX * scalex);
            cellsizey:= Round(TextBlock^.CellSizey * scaley);
            if TransparentColorIndex > -1 then
            begin
              {$IFDEF NOFPCW}
              transb:= GlobalTable[TransparentColorIndex].rgbBlue;
              transg:= GlobalTable[TransparentColorIndex].rgbGreen;
              transr:= GlobalTable[TransparentColorIndex].rgbRed;
              {$ELSE}
              transb:= GlobalTable[TransparentColorIndex].rgbtBlue;
              transg:= GlobalTable[TransparentColorIndex].rgbtGreen;
              transr:= GlobalTable[TransparentColorIndex].rgbtRed;
              {$ENDIF}
            end
            else
            begin
              transb:= 0; transg:= 0; transr:= 0;
            end;
            ACanvas.Font.Name:= MonoFonts[Random(MonoFonts.Count)];
            ACanvas.Font.Height:= Round(-(TextBlock^.CellSizeY - 1) *
              scaley);
            try
              slen:= Length(stext);
              for I:= 1 to slen do
              begin
                SetSize(ACanvas.TextWidth(string(stext[I])),
                  ACanvas.TextHeight(string(stext[I])));
                with bm.Canvas do
                begin
                  Font.Name:= ACanvas.Font.Name;
                  Font.Height:= Acanvas.Font.Height;
                  {$IFDEF NOFPCW}
                  with GlobalTable[TextBlock^.ForeColorIndex] do
                    Font.Color:= RGB(rgbRed, rgbGreen, rgbBlue);
                  with GlobalTable[TextBlock^.BackColorIndex] do
                    Brush.Color:= RGB(rgbRed, rgbGreen, rgbBlue);
                  {$ELSE}
                  with GlobalTable[TextBlock^.ForeColorIndex] do
                    Font.Color:= (rgbtRed or (rgbtGreen shl 8) or (rgbtBlue shl 16));
                  with GlobalTable[TextBlock^.BackColorIndex] do
                    Brush.Color:= (rgbtRed or (rgbtGreen shl 8) or (rgbtBlue shl 16));
                  {$ENDIF}
                  Brush.Style:= bsSolid;
                  {$IFDEF NOFPCW}
                  SetBkMode(Handle, Winapi.Windows.TRANSPARENT);
                  {$ELSE}
                  SetBkMode(Handle, LclType.TRANSPARENT);
                  {$ENDIF}
                  FillRect(ClipRect);
                  TextOut(0, 0, string(stext[I]));
                  {$IFDEF NOFPCW}  // this doesn't work in FPC, at least in Windows.
                  if (TransparentColorIndex > -1) and
                    (TextBlock^.BackColorIndex = TransparentColorIndex) then
                  begin
                    bm.Transparent:= true;
                    bm.TransparentColor:= RGB(transr, transg, transb);
                    bm.TransparentMode:= tmFixed;
                  end;
                  {$ENDIF}
                  ACanvas.StretchDraw(Rect(cellx, celly,
                    cellx + cellsizex, celly + cellsizey), bm);
                end;
                Inc(cellx, cellsizeX);
                Inc(completedx, TextBlock^.CellSizeX);
                if completedx >= FrameWidth then
                begin
                  completedx:= 0;
                  Inc(celly, cellsizey);
                  cellx:= Round(FrameLeft * scalex) + Arect.Left;
                end;
              end;
            except
            end;
          end;
        finally
          bm.Free;
        end;
        exit;
      end
      else
      begin
        if (pfIndex < Frames.Count -1) and
          (frames.Frame[pfIndex + 1].Disposal =
          dmRestorePrev) then
          PreviousBits:= Copy(FCurrentBits, 0,
            High(Integer));
        canvasstride:= ((((FImageWidth * 24) + 23) and not 23)
            shr 3);
        minx:= Max(0, FrameLeft);
        maxx:= Min(FrameLeft + FrameWidth, FImageWidth);
        framestride:= FrameWidth;
        for Row:= Max(0, FrameTop) to Min(FImageHeight,
          FrameTop + FrameHeight) -1 do
        begin
          Col := maxx;
          P:= @FCurrentBits[canvasstride * Row + FrameLeft * 3];
          P2:= @DecodedBits[framestride * (Row - FrameTop)];
          while Col > minx do
          begin
            if P2^ <> TransparentColorIndex then
              P^:= colortable[P2^];
            Inc(p);
            Inc(p2);
            Dec(Col);
          end;
        end;
        if Length(BlendedBits) <> Length(FCurrentBits) then
          SetLength(BlendedBits, Length(FCurrentBits));
        BlendedBits:= Copy(FCurrentBits, 0, High(Integer));
        DrawDecodedBytes(ACanvas, ARect, pfIndex, BlendedBits);
        SetLength(BlendedBits, 0);
        Finalize(BlendedBits);
      end;
    finally
      if Frames.count > 1 then
        if (Disposal = dmRestorePrev) and
          (Length(PreviousBits) <> 0) then
          FCurrentBits:= Copy(PreviousBits, 0, High(Integer))
        else if Disposal = dmBackgroundColor then
          DrawBackground(Rect(0, 0, FImageWidth,
            FImageHeight), colortable);
      if Assigned(FOnAfterFrameDrawn) then
        FOnAfterFrameDrawn(Self);
    end;
  end;
end;

procedure TMovieGifImage.LoadFromStream(Stream: TMemoryStream);
const
  TrailerByte: Byte = $3B;
var
  packedfields, sep: Byte;
  colortablesize: Word;
  w: Word;
  extensiontype: Byte;
  blocklength: Byte;
  bt: Byte;
  fillbytes: Integer;
  frame: TGifFrame;
  EncodedStream: TMemoryStream;
  AppIdentifier: array[0..7] of AnsiChar;
  AppSubIdentifier: array[0..2] of AnsiChar;
  table: TBgrTripleArray;
  delaysset: Integer;

  function ReadCheck(var Buffer; Size: LongInt): Boolean;
  var
    ReadSize: integer;
  begin
    ReadSize := Stream.Read(Buffer, Size);
    Result:= ReadSize = Size;
  end;

  function BlockRight(blength: Integer = -1): Boolean;
  var
    setendblock: Boolean;
  begin
    setendblock:= blength = -1;
    if setendblock then
      blength:= blocklength;
    Result:= true;
    fillbytes:= Stream.Position + blength;
    if Stream.Size - fillbytes <= 0 then
      Result:= false;
  end;

  function ProcessBlock: Boolean;
  begin
    Result:= false;
    repeat
      if not ReadCheck(blocklength, 1) then
        exit;
      if (blocklength > 0) and (sep <> 0) then
      begin
        Result:= BlockRight;
        if Result then
        begin
          if sep = $2C then
          begin
            EncodedStream.Write(blocklength, 1);
            EncodedStream.CopyFrom(Stream, blocklength);
            //Stream.Seek(blocklength, socurrent);
            continue;
          end
          {$IFDEF DOPLAINTEXT}
          else if (sep = $21) and (extensiontype = $01) then
          begin
            SetLength(frame.TextBlock^.TextData, blocklength);
            Result:= ReadCheck((@frame.TextBlock^.TextData[0])^,
              blocklength);
            if not Result then
              break;
            continue;
          end
          {$ENDIF}
          else if (sep = $21) and (extensiontype = $FF) and
            (AppIdentifier = 'NETSCAPE') then
          begin
            Result:= ReadCheck(bt, 1);
            if bt = 1 then
            begin
              Result:= ReadCheck(w, 2);
              if not Result then
                break;
              if w = 0 then
                LoopCount:= 0
              else
                LoopCount:= w + 1;
            end;
            continue;
          end;
          Stream.Seek(blocklength, soCurrent)
        end;
      end;
    until Result and ((blocklength = 0) or (sep = $00));
  end;

{$IFNDEF NOFPCW}
var
  I: Integer;
{$ENDIF}

begin
  SourceAnimated:= 1;
  EncodedStream:= TMemoryStream.Create;
  delaysset:= 0;
  try
    if not ValidGifFile(Stream) then
      exit;
    sep:= 1;
    if not ReadCheck(w, 2) then
      raise Exception.Create('Bad file');
    FImageWidth:= w;
    if not ReadCheck(w, 2) then
      raise Exception.Create('Bad file');
    FImageHeight:= w;
    if not ReadCheck(packedfields, 1) then
      raise Exception.Create('Bad file');
    if not ReadCheck(bt, 1) then
      raise Exception.Create('Bad file');
    FBackgroundIndex:= bt;
    if not ReadCheck(bt, 1) then
      raise Exception.Create('Bad file');
    FPixelRatio:= bt; // if not 0, = (+ 15) / 64
    if GetBitsValue(packedfields, 0, 1) = 1 then
    begin
      colortablesize:= Round(Power(2, GetBitsValue(packedfields, 5, 3) + 1) * 3);
      {$IFDEF NOFPCW}
      SetLength(GlobalTable, colortablesize div 3);
      if not ReadCheck((@GlobalTable[0])^, colortablesize) then
        raise Exception.Create('Bad file');
      {$ELSE}
      SetLength(table, colortablesize div 3);
      if not ReadCheck((@table[0])^, colortablesize) then
        raise Exception.Create('Bad file');
      SetLength(GlobalTable, Length(table));
      for I:= 0 to High(table) do
      begin
        GlobalTable[I].rgbtGreen:= table[I].rgbGreen;
        GlobalTable[I].rgbtBlue:= table[I].rgbBlue;
        GlobalTable[I].rgbtRed:= table[I].rgbRed;
      end;
      {$ENDIF}
    end
    else
      FBackgroundIndex:= 0;
    frame:= nil;
    FMinimum:= USER_TIMER_MINIMUM;
    LoopCount:= 1;
    repeat
      if not ReadCheck(sep, 1) then
        if Frames.Count = 0 then
          raise Exception.Create('Bad file')
        else
          break;
      if sep = $2C then
      begin
        if frame = nil then
          frame:= TGifFrame.Create;
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameLeft:= w;
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameTop:= w;
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameWidth:= w;
        if not ReadCheck(w, 2) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.FrameHeight:= w;
        if not ReadCheck(packedfields, 1) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        frame.Interlaced:= Boolean(GetBitsValue(packedfields, 1, 1));
        if GetBitsValue(packedfields, 0, 1) = 1 then
        begin
          colortablesize:= Round(Power(2, GetBitsValue(packedfields, 5,3) + 1) * 3);
          if BlockRight(colortablesize) then
          begin
            SetLength(table, colortablesize div 3);
            if not ReadCheck((@table[0])^, colortablesize) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            SetLength(frame.LocalColorTable, Length(table));
            {$IFDEF NOFPCW}
            Move((@table[0])^, (@frame.LocalColorTable[0])^, colortablesize);
            {$ELSE}
            for I:= 0 to High(table) do
            begin
              frame.LocalColorTable[I].rgbtGreen:= table[I].rgbGreen;
              frame.LocalColorTable[I].rgbtBlue:= table[I].rgbBlue;
              frame.LocalColorTable[I].rgbtRed:= table[I].rgbRed;
            end;
            {$ENDIF}
          end
          else if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        end;
        EncodedStream.Clear;
        if Stream.Size < Stream.Position + 1 then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        EncodedStream.CopyFrom(Stream, 1);
        if not ProcessBlock then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        if EncodedStream.Size > 1 then
        begin
          Frames.Add(frame);
          DecodeFrame(EncodedStream, Frames.Count - 1);
          frame:= nil;
        end
        else if Frames.Count = 0 then
          raise Exception.Create('Bad file')
        else
          break;
      end
      else if sep = $21 then
      begin
        if not ReadCheck(extensiontype, 1) then
          if Frames.Count = 0 then
            raise Exception.Create('Bad file')
          else
            break;
        case extensiontype of
          $F9: // graphic control extension
          begin
            frame:= TGifFrame.Create;
            if not ReadCheck(blocklength, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if blocklength = 4 then
            begin
              if not ReadCheck(bt, 1) then
                if Frames.Count = 0 then
                  raise Exception.Create('Bad file')
                else
                  break;
              frame.Disposal:= TDisposalMethod(
                GetBitsValue(bt, 3, 3));
              frame.UserInput:= Boolean(
                GetBitsValue(bt, 6, 1));
              if GetBitsValue(bt, 7, 1) = 1 then
                frame.TransparentColorIndex:= -2
                  // waiting for transparent index
              else
                frame.TransparentColorIndex:= -1;
              if not ReadCheck(w, 2) then
                if Frames.Count = 0 then
                  raise Exception.Create('Bad file')
                else
                  break;

              frame.Delay:= w * 10;
              if w > 0 then
                Inc(delaysset);
              if not ReadCheck(bt, 1) then
                if Frames.Count = 0 then
                  raise Exception.Create('Bad file')
                else
                  break;
              if frame.TransparentColorIndex = -2 then
                frame.TransparentColorIndex:= bt;
              Stream.Seek(1, soCurrent);
            end
            else if Frames.Count = 0 then
              raise Exception.Create('Bad file')
            else
              break;
          end;
          $FE:
          begin
            if not ProcessBlock then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
          end;
          $01: // plain text extension
          begin
            if not ReadCheck(blocklength, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if blocklength <> 12 then
            begin
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            end;
            {$IFNDEF DOPLAINTEXT}
            if frame <> nil then
            begin
              FreeAndNil(frame);
              Stream.Seek(blocklength, soCurrent);
            end;
            {$ELSE}
            if frame = nil then
              frame:= TGifFrame.Create;
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameLeft:= w;
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameTop:= w;
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameWidth:= w;
            if not ReadCheck(w, 2) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.FrameHeight:= w;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            New(frame.TextBlock);
            frame.TextBlock^.CellSizeX:= bt;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.TextBlock^.CellSizeY:= bt;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.TextBlock^.ForeColorIndex:= bt;
            if not ReadCheck(bt, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            frame.TextBlock^.BackColorIndex:= bt;
            SetLength(frame.TextBlock^.TextData, 0);
            {$ENDIF}
            if not ProcessBlock then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            {$IFDEF DOPLAINTEXT}
            if Length(frame.TextBlock^.TextData) > 0 then
            begin
              frame.TextBlock^.FontName[0]:= #0;
              frame.TextBlock^.FontSize:= 0;
              Frames.Add(frame);
              SetLength(frame.DecodedBits, 0);
              frame:= nil;
            end;
            {$ENDIF}
          end;
          $FF: // application extension
          begin
            if not ReadCheck(blocklength, 1) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if blocklength <> 11 then
            begin
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            end;
            if not ReadCheck(AppIdentifier, 8) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if not ReadCheck(AppSubIdentifier, 3) then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
            if not ProcessBlock then
              if Frames.Count = 0 then
                raise Exception.Create('Bad file')
              else
                break;
          end;
        end;
      end;
    until ((sep <> $2C) and (sep <> $21));
  finally
    if frame <> nil then
      FreeAndNil(frame);
    if delaysset / Frames.count * 100 < 10 then
      FMinimum:= GIFFIXEDMINIMUM;
    EncodedStream.Free;
    SourceAnimated:= Byte(Frames.Count > 1) + 1;
    if (MonoFonts.Count = 0) then // and (Length(FTextblocks) > 0) then
    begin
      GetMonoFonts;
      if (FPlainTextFontName = '') and (Monofonts.Count > 0) then
        FPlaintextFontName:= MonoFonts[Random(MonoFonts.Count)];
    end;
  end;
end;

procedure TMovieGifImage.SetPlainTextFontName(Value: string);
var
  smess: string;
begin
  if CompareText(PlaintextFontName, Value) <> 0 then
  begin
    if (Monofonts = nil) or (MonoFonts.Count = 0) or (MonoFonts.IndexOf(Value) = -1) then
    begin
      smess:= 'The font for plain text has to be a monospace font';
      if (Monofonts = nil) or (MonoFonts.Count = 0) then
        smess:= smess + ' and there is no such font installed in your system';
      {$IFNDEF NOFPCW}
      MessageDlg('Error', smess, mtConfirmation, [mbOk], '');
      {$ELSE}
      MessageDlg(smess, mtError, [mbOk], 0);
      {$ENDIF}
      exit;
    end;
    PlainTextFontName:= Value;
  end;
end;

constructor TMovieWebpImage.Create(NewCanvas: TCanvas);
var
  cl: TColorRef;
begin
  inherited;
  {$IFNDEF NOFPCW}
  FInfo.bmiHeader.biBitCount := 32;
  {$ENDIF}
  cl:= GetSysColor(COLOR_BACKGROUND);
  FBackgroundColor.rgbBlue:= Byte(cl);
  FBackgroundColor.rgbGreen:= Byte(cl shr 8);
  FBackgroundColor.rgbRed:= Byte(cl shr 16);
  FBackgroundColor.rgbReserved:= Byte(cl shr 24);
end;

function TMovieWebpImage.DecodeFrame(EncodedStream: TStream;
  pfIndex: Integer): Boolean;
var
  config: TWebPDecoderConfig;
  bufsize: Integer;
begin
  Result:= false;
  if WebPInitDecoderConfig(@config) <> 0 then
  with TWebpFrame(Frames.Frame[pfIndex]) do
  begin
    if WEBPGetFeatures(TMemoryStream(EncodedStream).Memory,
      EncodedStream.Size, @config.input) = VP8_STATUS_OK then
    begin
      if FrameHeight <> config.input.height then
        FrameHeight:= config.input.height;
      if FrameWidth <> config.input.width then
        FrameWidth:= config.input.width;
      {$IFNDEF NOFPCW}
      config.output.colorspace := MODE_bgrATr; //MODE_bgrATr;
      {$ELSE}
      config.output.colorspace := MODE_rgbATr; //MODE_bgrATr;
      {$ENDIF}
      config.output.RGBA.RGBA.stride:=
        ((((config.input.width * 32) + 31) and not 31)
        shr 3);
      bufsize:= config.output.RGBA.RGBA.stride * config.input.height;
      SetLength(DecodedBits, bufsize);
      config.output.RGBA.RGBA.rgba:= @DecodedBits[0];
      config.output.RGBA.RGBA.size := bufsize;
      config.output.is_external_memory := 1;
      config.options.no_fancy_upsampling:= 1;
      config.options.dithering_strength:= 0;
      try
        Result:= WebpDecode(TMemoryStream(EncodedStream).Memory,
          EncodedStream.Size, @config)
          = VP8_STATUS_OK;
      except
        Result:= false;
      end;
    end;
  end;
end;

class function TMovieWebpImage.CanBeAnimated(Fname: string): Boolean;
var
  Stream: TFileStream;
  fourcc: array[0..3] of AnsiChar;
  chunksize: Cardinal;
  icount: Integer;
begin
  Result:= SourceAnimated = 2;
  if Result then
    exit;
  Result:= false;
  if not FileExists(Fname) then
    exit;
  Stream:= TFileStream.Create(FName, fmOpenRead or
    fmShareDenyWrite);
  try
    Stream.Seek(8, soBeginning);
    Stream.Read(fourcc, 4);
    icount:= 0;
    if ArrayToAnsiString(fourcc) <> 'WEBP' then
      exit;
    Stream.Read(fourcc, 4);
    if ArrayToAnsiString(fourcc) <> 'VP8X' then
      exit;
    repeat
      Stream.Read(chunksize, 4);
      if chunksize mod 2 <> 0 then
        Inc(chunksize);
      if (fourcc = 'VP8 ') or (fourcc = 'VP8L') then
      begin
        if chunksize = 0 then
          exit;
        Inc(icount);
        if icount = 2 then
        begin
          Result:= true;
          exit;
        end;
      end;
      if fourcc = 'ANMF' then
        Stream.Seek(16, soCurrent)
      else
        Stream.Seek(chunksize, soCurrent);
      if Stream.Position < Stream.Size - 4 then
        Stream.Read(fourcc, 4);
    until Stream.Position >= Stream.Size - 4;
  finally
    Stream.Free;
  end;
end;

procedure TMovieWebpImage.DrawBackground(ARect: TRect);
var
  Row, Col: Integer;
  stride: Integer;
  {$IFDEF NOFPCW}
  P: PWpQuad;
  {$ELSE}
  P: PRgbQuad;
  {$ENDIF}
begin
  stride:= ((((FImageWidth * 32) + 31) and not 31)
      shr 3);
  SetLength(FCurrentBits, stride * FImageHeight);
  for Row:= ARect.Top to ARect.Height - 1 do
  begin
    P:= @FCurrentBits[stride * Row + (ARect.Left * 4)];
    Col:= ARect.Width;
    while Col > ARect.Left do
    begin
      P^:= FBackgroundColor;
      Inc(P);
      Dec(Col);
    end;
  end;
end;

procedure TMovieWebpImage.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer);
var
  Row, Col, canvasstride, framestride, minx, maxx: Integer;
  {$IFDEF NOFPCW}
  P, P2: PWpQuad;
  {$ELSE}
  P, P2: PRgbQuad;
  {$ENDIF}
  I: Integer;
  BlendedBits: TBytes;
begin
  if (pfIndex < 0) or (pfIndex > Frames.Count - 1) then
    exit;
  FFrameIndex:= pfIndex;
  if Assigned(FOnBeforeDrawingFrame) then
    FOnBeforeDrawingFrame(Self);
  if (Length(FCurrentBits) = 0) or (Acanvas <> FCanvas) or
    FNeedCompleteRedraw then
  begin
    if Frames.Count > 1 then
      DrawBackground(Rect(0, 0, FImageWidth, FImageHeight));
    if ACanvas <> FCanvas then
      FCanvas:= ACanvas;
  end;
  if FNeedCompleteRedraw and (Frames.Count > 1) then
  begin
    FExternalDrawing:= true;
    FNeedCompleteRedraw:= false;
    for I:= 0 to pfIndex - 1 do
      DrawFrame(Acanvas, ARect, I);
    FNeedCompleteRedraw:= true;
  end;
  with TWebpFrame(Frames.Frame[pfIndex]) do
  begin
    if Length(DecodedBits) = 0 then
      exit;
    canvasstride:= ((((FImageWidth * 32) + 31) and not 31)
        shr 3);
    framestride:= ((((FrameWidth * 32) + 31) and not 31)
        shr 3);
    if Frames.Count > 1 then
    begin
      minx:= Max(0, FrameLeft);
      maxx:= Min(FrameLeft + FrameWidth, FImageWidth);
      for Row:= Max(0, FrameTop) to Min(FImageHeight,
        FrameTop + FrameHeight) -1 do
      begin
        Col := maxx;
        P:= @FCurrentBits[canvasstride * Row + (FrameLeft * 4)];
        P2:= @DecodedBits[framestride * (Row - FrameTop)];
        while Col > minx do
        begin
          if Blending and (p2^.rgbReserved <> 255) then
          begin
            // this premultiplication formula can be verified
            // in the webp container specification page.
            // probably there are faster methods.
            p^.rgbReserved := p^.rgbReserved + p2^.rgbReserved *
              (1 - p^.rgbReserved div 255);
            if p^.rgbReserved = 0 then
            begin
              p^.rgbBlue:= 0;
              p^.rgbGreen:= 0;
              p^.rgbRed:= 0;
            end
            else
            begin
              p^.rgbBlue:= (p^.rgbBlue * p^.rgbReserved +
                p2^.rgbBlue * p2^.rgbReserved * (1 - p^.rgbReserved
                div 255)) div p^.rgbReserved;
              p^.rgbGreen:= (p^.rgbGreen * p^.rgbReserved +
                p2^.rgbGreen * p2^.rgbReserved * (1 - p^.rgbReserved
                div 255)) div p^.rgbReserved;
              p^.rgbRed:= (p^.rgbRed * p^.rgbReserved +
                p2^.rgbRed * p2^.rgbReserved * (1 - p^.rgbReserved
                div 255)) div p^.rgbReserved;
            end;
          end
          else
            Cardinal(p^):= Cardinal(p2^);
          Inc(p);
          Inc(p2);
          Dec(Col);
        end;
      end;
      if Length(BlendedBits) <> Length(FCurrentBits) then
        SetLength(BlendedBits, Length(FCurrentBits));
      BlendedBits:= Copy(FCurrentBits, 0, High(Integer));
    end
    else
      BlendedBits:= Copy(DecodedBits, 0, High(Integer));
    try
      DrawDecodedBytes(ACanvas, ARect, pfIndex, BlendedBits);
      if (Disposal = dmBackgroundColor) and (Frames.Count > 1) then
        DrawBackground(Rect(FrameLeft, FrameTop, FrameLeft +
          FrameWidth, FrameTop + FrameHeight));
      if Assigned(FOnAfterFrameDrawn) then
        FOnAfterFrameDrawn(Self);
    finally
      SetLength(BlendedBits, 0);
      Finalize(BlendedBits);
    end;
  end;
end;

procedure TMovieWebpImage.LoadFromStream(Stream: TmemoryStream);
var
  Frame: TWebpFrame;
  fourcc: array[0..3] of AnsiChar;
  chunksize, filesize: Cardinal;
  webinfobyte: Byte;
  imageheader, unknownfourcc: Boolean;
  b1, b2, b3: Byte;
  bcolor: TRGBquad;
  EncodedStream: TMemoryStream;
begin
  HasICCP:= false;
  HasFramesWithAlpha:= false;
  HasExifData:= false;
  HasXMPData:= false;
  Frames.Clear;
  FImageWidth:= 0;
  FCurrentFrame:= 0;
  FLoopIndex:= 0;
  FAnimateOnGetCanvas:= false;
  Sourceanimated:= 1;
  FExternalDrawing:= false;
  FImageHeight:= 0;
  Frame:= nil;
  Encodedstream:= TMemoryStream.Create;
  try
    Stream.Read(fourcc, 4);
    if ArrayToAnsiString(fourcc) <> 'RIFF' then
      exit;
    Stream.Read(filesize, 4);
    if Stream.Size < filesize + 8 then  // corruption
      exit;
    Stream.Read(fourcc, 4);
    if ArrayToAnsiString(fourcc) <> 'WEBP' then
      exit;
    Stream.Read(fourcc, 4);
    imageheader:= false;
    repeat
      unknownfourcc:= true;
      Stream.Read(chunksize, 4);
      if chunksize mod 2 <> 0 then
        Inc(chunksize);
      if Stream.Position + chunksize > Stream.Size then  // corruption
        exit;
      if fourcc = 'VP8X' then
      begin
        if chunksize = 0 then
          exit;
        unknownfourcc:= false;
        Stream.Read(webinfobyte, 1);
        HasICCP:= GetBitsValue(webinfobyte, 2, 1) = 1;
        HasFramesWithAlpha:= GetBitsValue(webinfobyte, 3, 1) = 1;
        HasExifData:= GetBitsValue(webinfobyte, 4, 1) = 1;
        HasXMPData:= GetBitsValue(webinfobyte, 5, 1) = 1;
        SourceAnimated:= GetBitsValue(webinfobyte, 6, 1) + 1;
        Stream.Seek(3, soCurrent);
        if Stream.Position + chunksize > Stream.Size then  // corruption
          exit;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        FImageWidth:= 1 + ((b3 shl 16) or (b2 shl 8) or b1);
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        FImageHeight:= 1 + ((b3 shl 16) or (b2 shl 8) or b1);
      end
      else if (fourcc = 'VP8 ') or (fourcc = 'VP8L') then
      begin
        if chunksize = 0 then
          exit;
        unknownfourcc:= false;
        if Frame = nil then // NO VP8X
        begin
          if not imageheader then
            imageheader:= true;
          Frame:= TWebpFrame.Create;
        end;
        Frame.Lossless:= fourcc = 'VP8L';
        if Frame.Lossless and (EncodedStream.Size > 0) then
          // alpha y vp8l can't go together
        begin
          if ErrorOnBadFrame then
          begin
            Frames.Clear;
            exit;
          end;
          Frame.Free;
          Frame:= Nil;
        end;
        if imageheader and (Frame <> nil) then
        begin
          Encodedstream.Write(fourcc, 4);
          Encodedstream.Write(chunksize, 4);
          Encodedstream.CopyFrom(Stream, chunksize);
          Frames.Add(Frame);
          if not DecodeFrame(Encodedstream, Frames.Count - 1) then
          begin
            if ErrorOnBadFrame then
            begin
              Frames.Clear;
              exit;
            end;
            FreeAndNil(Frame);
          end;
          imageheader:= false;
        end
        else
          Stream.Seek(chunksize, soCurrent);
      end
      else if fourcc = 'ANIM' then
      begin
        {$IFDEF NOFPCW}
        Stream.Read(bcolor, 4);
        FBackgroundColor.rgbBlue:= bcolor.rgbRed;
        FBackgroundColor.rgbGreen:= bcolor.rgbGreen;
        FBackgroundColor.rgbRed:= bcolor.rgbBlue;
        FBackgroundColor.rgbReserved:= bcolor.rgbReserved;
        {$ELSE}
        Stream.Read(FBackgroundColor, 4);
        {$ENDIF}
        unknownfourcc:= false;
        Stream.Read(LoopCount, 2);
        Stream.Seek(chunksize - 6, soCurrent);
      end
      else if fourcc = 'ANMF' then
      begin
        // specification says if not FSourceanimated
        // this shouldn't be present, but it doesn't say
        // ignore it. Besides I implement autouseranimation so
        // I keep using it even if not FSourceAnimated.
        unknownfourcc:= false;
        imageheader:= true;
        Frame:= TWebpFrame.Create;
        Frame.AlphaCompressed:= 0;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameLeft:= ((b3 shl 16) or (b2 shl 8) or b1) * 2;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameTop:= ((b3 shl 16) or (b2 shl 8) or b1) * 2;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameWidth:= ((b3 shl 16) or (b2 shl 8) or b1) + 1;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.FrameHeight:= ((b3 shl 16) or (b2 shl 8) or b1) + 1;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        Frame.Delay:= ((b3 shl 16) or (b2 shl 8) or b1);
        Encodedstream.Clear;
        SetLength(Frame.DecodedBits, 0);
        Stream.Read(webinfobyte, 1);
        Frame.Blending:= GetBitsValue(webinfobyte, 6, 1) = 0;
        Frame.Disposal:= TDisposalMethod(GetBitsValue(webinfobyte, 7,
          1) + 1);
      end
      else if imageheader and (fourcc <> 'ALPH') and
        (fourcc <> 'VP8 ') and (fourcc <> 'VP8X') then
      begin
        imageheader:= false;
        FreeAndNil(Frame);
      end
      else if imageheader and (Frame <> nil) and
         (fourcc = 'ALPH') then
      begin
        unknownfourcc:= false;
        Encodedstream.Write(fourcc, 4);
        Encodedstream.Write(chunksize, 4);
        Stream.Read(webinfobyte, 1);
        Stream.Seek(-1, soCurrent);
        EncodedStream.CopyFrom(Stream, chunksize);
        //webinfobyte:= Frame.ImageBits[8];
        Frame.PreProcessing:= Boolean(GetBitsValue(webinfobyte, 2, 2));
        Frame.Filtering:= GetBitsValue(webinfobyte, 4, 2);
        Frame.AlphaCompressed:= GetBitsValue(webinfobyte, 6, 2) + 1;
        if chunksize + Stream.Position > Stream.Size then
          exit;
      end;
      if unknownfourcc then
        Stream.Seek(chunksize, soCurrent);
      if Stream.Position < Stream.Size - 4 then
        Stream.Read(fourcc, 4);
    until Stream.Position >= Stream.Size - 4;
  finally
    if Frames.Count > 0 then
    begin
      if (SourceAnimated = 2) and (Frames.Count < 2) then
        SourceAnimated:= 1;
      if AnimateOnLoaded and (Frames.Count > 1) and
        (SourceAnimated = 2) then
        Animate;
      if FImageWidth = 0 then
        FImageWidth:= Frames.Frame[0].FrameWidth;
      if FImageHeight = 0 then
        FImageHeight:= Frames.Frame[0].FrameHeight;
      if Frames.Count = 1 then
      begin
        Frames.Frame[0].FrameLeft:= 0;
        Frames.Frame[0].FrameTop:= 0;
      end;
    end;
  end;
end;

{$IFDEF NOFPCW}
procedure TMovieWebpImage.SetBackgroundColor(Value: TWpQuad);
{$ELSE}
procedure TMovieWebpImage.SetBackgroundColor(Value: TRgbQuad);
{$ENDIF}
begin
  if Cardinal(Value) <> Cardinal(FBackgroundColor) then
  begin
    FBackgroundColor:= Value;
    DrawBackground(Rect(0, 0, FImageWidth, FImageHeight));
  end;
end;

{ TGifFrame }

constructor TGifFrame.Create;
begin
  inherited;
  SEtLength(LocalColorTable, 0);
end;

destructor TGifFrame.Destroy;
begin
  SEtLength(LocalColorTable, 0);
  Finalize(LocalColorTable);
  inherited;
end;

{ TMovieFrame }

constructor TMovieFrame.Create;
begin
  inherited;
  TextBlock:= nil;
  SetLength(DecodedBits, 0);
end;

destructor TMovieFrame.Destroy;
begin
  SetLength(DecodedBits, 0);
  Finalize(DecodedBits);
  if TextBlock <> nil then
    Dispose(TextBlock);
  inherited;
end;

{ TIMageBase }

constructor TImageBase.Create(NewCanvas: TCanvas = nil);
begin
  inherited Create;
  //FWic:= nil;
  FCanvas:= NewCanvas;
  if (FCanvas <> nil) and (FCanvas.Handle <> 0) then
    FCanvasRect:= FCanvas.ClipRect;
end;

procedure TImageBase.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer);
begin
  if Self is TMovieImageBase then
    TMovieImageBase(Self).DrawFrame(ACanvas, ARect, pfIndex)
  else
    ACanvas.StretchDraw(Arect, Self);
end;

procedure TImageBase.LoadFromFile(const FileName: string);
var
  FileExt: string;
{$IFNDEF NOFPCW}
  f: TLazIntfImage;
  Reader: TFPCustomImageReader;
{$ENDIF}
begin
  if not FileExists(FileName) then
    exit;
  FileExt:= string(GetImageExtensionBySignature(FileName));
  if ((FileExt = '.GIF') and (Self is TMovieGifImage)) or
    ((FileExt = '.WEBP') and (Self is TMovieWebpImage)) then
    TMovieImageBase(Self).LoadFromFile(FileName)
  else
  begin
    {$IFDEF NOFPCW}
    inherited;
    {$ELSE}
    f:= TLazIntfImage.Create(0, 0);
    Reader := GetFPImageReaderForFileExtension(FileExt).Create;
    try
      f.DataDescription:= GetDescriptionFromDevice(0);
      f.LoadFromFile(FileName, Reader);
      LoadFromIntfImage(f);
    finally
      f.Free;
      Reader.Free;
    end;
    {$ENDIF}
  end;
end;

procedure TImageBase.SetCanvas(Value: TCanvas);
begin
  if (Value <> nil) and ((FCanvas = nil) or (FCanvas.Handle <>
    Value.Handle)) then
  begin
    FCanvas:= Value;
    FCanvasRect:= FCanvas.ClipRect;
  end;
end;

function TImageBase.GetImagewidth: Integer;
begin
  if (Self is TMovieWebpImage) or
    ((Self is TMovieGifImage) and
    (TMovieGifImage(self).FPixelRatio = 0)) then
    Result:= FImageWidth
  else if (Self is TMovieGifImage) and
    (TMovieGifImage(self).FPixelRatio <> 0) then
    Result:= Round(FImageWidth * ((
      TMovieGifImage(self).FPixelRatio + 15) / 64))
  else
    Result:= Width;
end;

function TImageBase.GetImageHeight: Integer;
begin
  if Self is TMovieImageBase then
    Result:= FImageHeight
  else
    Result:= Height;
end;

initialization
  SetLength(GlobalTAble, 0);
  Randomize;

finalization
  SetLength(GlobalTable, 0);
  Finalize(GlobalTable);

end.
