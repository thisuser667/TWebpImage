unit WebpImage;

interface

// Done by the user of the web page in which it is originally
  // published.
// Licensing: Do what you want with it.
// This is NOT something very pro, but it's a good start to do something pro,
// which, as I insist, you are completely free to do it.
// It is to be used with libwebp.dll 1.2.4
// In the future, the first thing that has to be checked is
// the value of the libwebp.pas const "WEBP_DECODER_ABI_VERSION"
// in decode.h of the source code.
// I could check that previous webp dll versions couldn't open
// some more recent files.
// Writing webp new files, extracting frames, etc. to other formats are
// things that if not done by someone else, I will do maybe.

uses Winapi.Windows, System.SysUtils, libwebp124, {ExtendedImageUtils,}
  VCl.Graphics, System.Classes,
  System.Math, Vcl.ExtCtrls, Winapi.Wincodec;

{$WARN SYMBOL_PLATFORM OFF} // Disable platform warnings. This library is only supported on Windows.

type
  TWebpFrame = record  // each image
    AlphaCompressed: Byte;  // 0 = not Alpha, 1 = Alpha not compresed;
      // 2 = Alpha compressed; = compressed or not is not important.
    Blending: Boolean;  // this frame has to be blended instead of
      // just drawed over the previous frame. (just transparent pixels)
    DecodedBits: TBytes;  // as said.
    Disposal: Boolean;  // once drawn the frame, redraw the
      // background if true
    Filtering: Byte;  // just for encoding
    FrameWidth, FrameHeight, FrameLeft, FrameTop, Delay: Integer;
      // as said. delay in miliseconds.
    ImageBits: TBytes;  // previously to decoding, once decoded
      // they can be removed.
    Lossless: Boolean;  // not very important, just to check that
      // if it's lossless it can't have alpha.
    PreProcessing: Boolean;  // should have been implemented.
      // read about it in the webp container specification.
    BlendedBits: TBytes; // bytes that are reserved for each loop
      // there is not very much time gained for only blend them
      // once.
    Blended: Boolean;  // maybe length(blendedbits) > 0 would
      // have been enough.
  end;
  PWebpFrame = ^TWebpFrame;

  TWebpFrameList = class(TList)  // container of the frames.
  private
    function GetFrame(Index: Integer): TWebpFrame;
    procedure SetFrame(Index: Integer; Value: TWebpFrame);
  public
    procedure AddFrame(framedata: TWebpFrame);
    procedure AssignFrame(SourceFrame: TWebpFrame;
      var DestFrame: TWebpFrame);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    property Frame[Index: Integer]: TWebpFrame read GetFrame
      write SetFrame;
  end;

type
  TBeforeDrawingFrameEvent = procedure(Sender: TObject) of
    object;
  TAfterFrameDrawnEvent = procedure(Sender: TObject) of
    object;
  TNextFrameEvent = procedure(Sender: TObject;
    FrameIndex: Integer) of object;

  TAnimationSpeed = 0..1000;

  TWebpImage = class(TGraphic)
  private
    FLoopIndex: Integer;  // infinite loop can be overridden
      // with InfiniteLoopAnyWays, despite almost all of the webp
      // files have intinite looping just the same.
    FAltBackgroundColor: TRgbQuad;  // tcolor hasn't alpha
      // replace the background color in the file if
      // freplacebackgroundcolor is true
    FSourceAnimated: Boolean;  // what the file says about it.
    FAnimateOnGetCanvas: Boolean;  // internal processing.
    FAnimationSpeed: TAnimationSpeed; // 50 = normal speed div 2
    FBackgroundColor: TRGbQuad;  // background color inside the file
    FCurrentBits: TBytes;  // the canvas bits in each iteration
    FFileName: string;
    FNeedCompleteRedraw: Boolean;  // as blending changes
      // as the loop progresses, if one frame is drawn
      // from the user app, all the previous frames have to be
      // rendered.
    FOnBeforeDrawingFrame: TBeforeDrawingFrameEvent;
    FOnAfterFrameDrawn: TAfterFrameDrawnEvent;
    FOnLoop: TNotifyEvent;
    FReplaceBackgroundColor: Boolean;
    FTimer: TTimer;
    FCurrentFrame: Integer; // as said.
    FCanvas: TCanvas; // got in draw or drawframe.
    FCanvasRect: TRect; // idem but only on draw
    FExternalDrawing: Boolean;  // if true doesn't process draw
    FHeight: Integer; // canvas (global) height, is not previewed that
      // height or width can be widened or enlarged if
      // some frame has coordenates outside this rectangle.
      // just that part of the frame is not drawn.
    FOnNextFrame: TNextFrameEvent;
    FWidth: Integer;
    function GetAnimating: Boolean;
    function GetPaused: Boolean;
    procedure SetAltBackgroundColor(Value: TRGbQuad);
    procedure SetReplaceBackgroundColor(Value: Boolean);
    procedure AnimateTimer(Sender: TObject); // internal ontimer event
  protected
    HasExifData: Boolean;  // not implemented
      // chunks without implementation can be just ignored
    HasFramesWithAlpha: Boolean;  // just because it's in the header.
    HasICCP: Boolean; // should be implemented in a more professional
      // implementation.
    HasXMPData: Boolean; // not implemented
    // All the next overrides, necessary because TGraphic has
    // them as abstract.
    procedure AssignTo(Dest: TPersistent); override;
    function DecodeFrame(FrameIndex: Integer): Boolean; virtual;
      // called while loading or at the end of the loading.
      // As implemented, it just relies on high level dll
      // procedures that return bytes almost read to be drawn.
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetPalette: HPALETTE; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure SetPalette(Value: HPalette); override;
    procedure WriteData(Stream: TStream); override;
  public
    var Frames: TWebpFrameList;
    LoopCount: Word;
    UserAnimation: Word; // overrides Fsourceanimated.
      // 0 = not overridden. > 0 = global delay when
      // fsourcenimated is false and frame delay is 0
      // and frames.count is greater than 1.
    constructor Create; override;
    destructor Destroy; override;
    procedure Animate;
    procedure ClearData; // cleaning
    procedure DrawBackground(ARect: TRect);
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      pfIndex: Integer);
    class function CanBeAnimated(Filename: string): Boolean;
    procedure LoadFromFile(const Filename: string); override;
      // overridden just to get ffilename value
    procedure LoadFromStream(Stream: TStream); override;
    procedure PauseAnimation;
    procedure RestoreAnimation;
    procedure SaveToStream(Stream: TStream); override;
    function LoopDurationinMs: Cardinal; // sum of all the delays
    property AnimationSpeed: TAnimationSpeed read FAnimationSpeed
      write FAnimationSpeed;
    property AltBackgroundColor: TRGbQuad read
      FAltBackgroundColor write SetAltBackgroundColor;
    property Animating: Boolean read GetAnimating;
    property CurrentFrame: Integer read FCurrentFrame write
      FCurrentFrame;
    property ExternalDrawing: Boolean write FExternalDrawing;
      // it's not interesting to get this value, but to set it.
    property FileName: string read FFileName;
    property LoopIndex: Integer read FLoopIndex;
    property IsPaused: Boolean read GetPaused;
    property OnBeforeDrawingFrame: TBeforeDrawingFrameEvent read
      FOnBeforeDrawingFrame write FOnBeforeDrawingFrame;
    property OnAfterFrameDrawn: TAfterFrameDrawnEvent read
      FOnAfterFrameDrawn write FOnAfterFrameDrawn;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnNextFrame: TNextFrameEvent read FONNextFrame write
      FOnNextFrame;
    property ReplaceBackgroundColor: Boolean read
      FReplaceBackgroundColor write SetReplaceBackgroundColor;
    property SourceAnimated: Boolean read FSourceAnimated;
  end;

var
  CF_WEBP: Word = 0;
  DecodeWhileLoading: Boolean = true;
  ErrorOnBadFrame: Boolean = true;
  InfiniteLoopAnyWays: Boolean = false;
  AnimateOnLoaded: Boolean = true;

procedure ClearFrame(frame: PWebpFrame);

implementation

const
  sWebpImage = 'Webp Image File';

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

procedure ClearFrame(frame: PWebpFrame);
begin
  with frame^ do
  begin
    SetLength(ImageBits, 0);
    Finalize(ImageBits);
    SetLength(DecodedBits, 0);
    Finalize(DecodedBits);
    SetLength(BlendedBits, 0);
    Finalize(BlendedBits);
  end;
  Dispose(frame);
end;

constructor TWebpImage.Create;
var
  cl: TColorRef;
begin
  inherited;
  SetLength(FCurrentBits, 0);
  Ftimer:= nil;
  Frames:= TWebpFrameList.Create;
  cl:= GetSysColor(COLOR_BACKGROUND);
  FAltBackgroundColor.rgbBlue:= Byte(cl);
  FAltBackgroundColor.rgbGreen:= Byte(cl shr 8);
  FAltBackgroundColor.rgbRed:= Byte(cl shr 16);
  FAltBackgroundColor.rgbReserved:= Byte(cl shr 24);
  FWidth:= 0;
  FHeight:= 0;
  FFileName:= '';
  UserAnimation:= 0;
  FReplaceBackgroundColor:= false;
  FNeedCompleteRedraw:= true;
end;

destructor TWebpImage.Destroy;
begin
  FreeAndNil(FTimer);
  Frames.Free;
  inherited;
end;

const
  USER_TIMER_MINIMUM = $0000000A;
  UOI_TIMERPROC_EXCEPTION_SUPPRESSION = 7;

procedure TWebpImage.Animate;
var
  h: THandle;
  b: LONGBOOL;
begin
  if (LoopIndex > 0) or (Frames.Count < 2) or (not FSourceAnimated and
    (UserAnimation = 0)) or (FTimer <> nil) then
    exit;
  if FCanvas = nil then
  begin
    FAnimateOnGetCanvas:= true;
    exit;
  end;
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
  FCurrentFrame:= 0;
  FTimer:= TTimer.Create(nil);
  FTimer.Interval:= USER_TIMER_MINIMUM;
  FTimer.Enabled:= true;
  FTimer.OnTimer:= AnimateTimer;
  FLoopIndex:= 1;
end;

procedure TWebpImage.AnimateTimer(Sender: TObject);
var
  Delay: Integer;
begin
  FExternalDrawing:= true;
  FNeedCompleteRedraw:= false;
  DrawFrame(FCanvas, FCanvasRect, FCurrentFrame);
  FNeedCompleteRedraw:= true; // needcompleteredraw always
    // true if calling drawframe externally
  //Delay:= Frames[FCurrentFrame].Delay;
  if FSourceAnimated or (Frames.Frame[FCurrentFrame].Delay > 0)  then
    Delay:= Frames.Frame[FCurrentFrame].Delay
  else if UserAnimation > 0 then
    Delay:= UserAnimation
  else
    Delay:= 10;
  FTimer.Interval:= Round(Delay / FAnimationSpeed * 100);
  if FCurrentFrame < Frames.Count -1 then
  begin
    Inc(FCurrentFrame);
    if Assigned(FOnNextFrame) then
      FOnNextFrame(Sender, FCurrentFrame);
  end
  else if (LoopCount = 0) or (FLoopIndex < LoopCount) then
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
end;

procedure TWebpImage.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  // this need to be redone.
  if Dest is TWebpImage then
  begin
    with TWebpImage(Dest) do
    begin
      HasICCP:= Self.HasICCP;
      HasFramesWithAlpha:= Self.HasFramesWithAlpha;
      HasExifData:= Self.HasExifData;
      HasXMPData:= Self.HasXmpData;
      Width:= FWidth;
      Height:= FHeight;
      Frames.Clear;
      for I:= 0 to Self.Frames.Count -1 do
        Frames.AddFrame(Self.Frames.Frame[I]); // this is incomplete.
    end;
  end
  else
    inherited AssignTo(Dest);
end;

class function TWebpImage.CanBeAnimated(Filename: string): Boolean;
var
  Stream: TFileStream;
  fourcc: array[0..3] of AnsiChar;
  chunksize: Cardinal;
  icount: Integer;
begin
  Result:= false;
  if not FileExists(Filename) then
    exit;
  Stream:= TFileStream.Create(FileName, fmOpenRead or
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

procedure TWebpImage.ClearData;
begin
  Frames.Clear;
end;

function TWebpImage.DecodeFrame(FrameIndex: Integer): Boolean;
var
  config: TWebPDecoderConfig;
  bufsize: Integer;
begin
  Result:= false;
  if WebPInitDecoderConfig(@config) <> 0 then
  with PWebpFrame(Frames.Items[FrameIndex])^ do
  begin
    if WEBPGetFeatures(@ImageBits[0], Length(ImageBits),
      @config.input) = VP8_STATUS_OK then
    begin
      if FrameHeight <> config.input.height then
        FrameHeight:= config.input.height;
      if FrameWidth <> config.input.width then
        FrameWidth:= config.input.width;
      config.output.colorspace := MODE_bgrATr;
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
        Result:= WebpDecode(@ImageBits[0], Length(ImageBits), @config)
          = VP8_STATUS_OK;
        if Result then
        begin
          SetLength(ImageBits, 0);
          Finalize(ImageBits);
        end;
      except
        Result:= false;
      end;
    end;
  end;
end;

procedure TWebpImage.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if (Frames.Count > 1) and ((Length(FCurrentBits) = 0) or
    (Acanvas <> FCanvas) or (FCanvasRect <> ARect)) then
    DrawBackground(Rect(0, 0, FWidth, FHeight));
  if (ACanvas <> FCanvas) or (FCanvasRect <> ARect) then
  begin
    if ACanvas <> FCanvas then
      FCanvas:= ACanvas;
    if FCanvasRect <> ARect then
      FCanvasRect:= ARect;
    if (Frames.Count > 1) and (AnimateOnLoaded or FAnimateOnGetCanvas)
      and (FLoopIndex = 0) then
    begin
      FAnimationSpeed:= 100;
      if FanimateOnGetCanvas then
        FAnimateOnGetCanvas:= false;
      Animate;
      exit;
    end;
  end;
  if not FexternalDrawing or (Frames.Count = 1) then
  begin
    FNeedCompleteRedraw:= false;
    DrawFrame(Acanvas, ARect, FCurrentFrame);
    FNeedCompleteRedraw:= true;
  end;
end;

procedure TWebpImage.DrawBackground(ARect: TRect);
var
  Row, Col: Integer;
  stride: Integer;
  P: PRgbQuad;
  backcl: TRgbQuad;
begin
  if FReplaceBackGroundColor then
    backcl:= FAltBackgroundColor
  else
    backcl:= FBackgroundColor;
  stride:= ((((FWidth * 32) + 31) and not 31)
      shr 3);
  SetLength(FCurrentBits, stride * FHeight);
  for Row:= ARect.Top to ARect.Height - 1 do
  begin
    P:= @FCurrentBits[stride * Row + (ARect.Left * 4)];
    Col:= ARect.Width;
    while Col > ARect.Left do
    begin
      P^:= backcl;
      Inc(P);
      Dec(Col);
    end;
  end;
end;

procedure TWebpImage.DrawFrame(ACanvas: TCanvas; ARect: TRect;
  pfIndex: Integer);
var
  Row, Col, canvasstride, framestride, minx, maxx: Integer;
  P, P2: PRgbquad;
  wic: TWicImage;  // remarkable better stretching quality than
                   // stretchdibits. Fast enough for me,
                   // maybe with Direct2D can be even faster.
  wicbitmap: IWicBitmap;
  I: Integer;
begin
  if (pfIndex < 0) or (pfIndex > Frames.Count - 1) then
    exit;
  if Assigned(FOnBeforeDrawingFrame) then
    FOnBeforeDrawingFrame(Self);
  if (Length(FCurrentBits) = 0) or (Acanvas <> FCanvas) or
    FNeedCompleteRedraw then
  begin
    if Frames.Count > 1 then
      DrawBackground(Rect(0, 0, FWidth, FHeight));
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
  with PWebpFrame(Frames.Items[pfIndex])^ do
  begin
    if Length(DecodedBits) = 0 then
      exit;
    canvasstride:= ((((FWidth * 32) + 31) and not 31)
        shr 3);
    framestride:= ((((FrameWidth * 32) + 31) and not 31)
        shr 3);
    if Frames.Count > 1 then
    begin
      if not blended then
      begin
        minx:= Max(0, FrameLeft);
        maxx:= Min(FrameLeft + FrameWidth, FWidth);
        for Row:= Max(0, FrameTop) to Min(FHeight,
          FrameTop + FrameHeight) -1 do
        begin
          Col := maxx;
          P:= @FCurrentBits[canvasstride * Row + (FrameLeft * 4)];
          P2:= @DecodedBits[framestride * (Row - FrameTop)];
          while Col > minx do
          begin
            if Blending and (p2.rgbReserved <> 255) then
            begin
              // this premultiplication formula can be verified
              // in the webp container specification page.
              // probably there are faster methods.
              p.rgbReserved := P.rgbReserved + p2.rgbReserved *
                (1 - P.rgbReserved div 255);
              if p.rgbReserved = 0 then
              begin
                p.rgbBlue:= 0;
                p.rgbGreen:= 0;
                p.rgbRed:= 0;
              end
              else
              begin
                p.rgbBlue:= (P.rgbBlue * P.rgbReserved +
                  p2.rgbBlue * p2.rgbReserved * (1 - P.rgbReserved
                  div 255)) div p.rgbReserved;
                p.rgbGreen:= (P.rgbGreen * P.rgbReserved +
                  p2.rgbGreen * p2.rgbReserved * (1 - P.rgbReserved
                  div 255)) div p.rgbReserved;
                p.rgbRed:= (P.rgbRed * P.rgbReserved +
                  p2.rgbRed * p2.rgbReserved * (1 - P.rgbReserved
                  div 255)) div p.rgbReserved;
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
        blended:= true;
      end;
    end
    else
      BlendedBits:= Copy(DecodedBits, 0, High(Integer));
    wic:= TWicImage.Create;
    try
      if wic.ImagingFactory.CreateBitmapFromMemory(FWidth, FHeight,
        GUID_WICPixelFormat32bppBGRA, canvasstride, Length(BlendedBits),
        @BlendedBits[0], wicbitmap) = S_OK then
      begin
        wic.Handle:= wicbitmap;
        ACanvas.StretchDraw(ARect, wic);
      end;
    finally
      wic.free;
    end;
    if Disposal and (Frames.Count > 1) then
      DrawBackground(Rect(FrameLeft, FrameTop, FrameLeft +
        FrameWidth, FrameTop + FrameHeight));
    if Assigned(FOnAfterFrameDrawn) then
      FOnAfterFrameDrawn(Self);
  end;
end;

function TWebpImage.Equals(Graphic: TGraphic): Boolean;
begin
  Result := Graphic = Self;
end;

function TWebpImage.GetEmpty: Boolean;
begin
  Result:= Frames.Count = 0;
end;

function TWebpImage.GetHeight: Integer;
begin
  Result:= FHeight;
end;

function TWebpImage.GetPalette: HPALETTE;
begin
  Result:= 0;
end;

function TWebpImage.GetWidth: Integer;
begin
  Result:= FWidth;
end;

function TWebpImage.GetAnimating: Boolean;
begin
  Result:= LoopIndex > 0;
end;

function TWebpImage.GetPaused: Boolean;
begin
  Result:= (fTimer <> nil) and not FTimer.Enabled;
end;

procedure TWebpImage.LoadFromFile(const Filename: string);
begin
  FFilename:= FileName;
  inherited;
end;

procedure TWebpImage.LoadFromStream(Stream: TStream);
var
  P: PWebpFrame;
  fourcc: array[0..3] of AnsiChar;
  chunksize, filesize: Cardinal;
  I: INteger;
  webinfobyte: Byte;
  imageheader, unknownfourcc: Boolean;
  b1, b2, b3: Byte;
begin
  HasICCP:= false;
  HasFramesWithAlpha:= false;
  HasExifData:= false;
  HasXMPData:= false;
  Frames.Clear;
  FWidth:= 0;
  FCanvas:= nil;
  FCurrentFrame:= 0;
  FLoopIndex:= 0;
  FAnimateOnGetCanvas:= false;
  FSourceanimated:= false;
  FExternalDrawing:= false;
  FHeight:= 0;
  P:= nil;
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
        FSourceAnimated:= GetBitsValue(webinfobyte, 6, 1) = 1;
        Stream.Seek(3, soCurrent);
        if Stream.Position + chunksize > Stream.Size then  // corruption
          exit;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        FWidth:= 1 + ((b3 shl 16) or (b2 shl 8) or b1);
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        FHeight:= 1 + ((b3 shl 16) or (b2 shl 8) or b1);
      end
      else if (fourcc = 'VP8 ') or (fourcc = 'VP8L') then
      begin
        if chunksize = 0 then
          exit;
        unknownfourcc:= false;
        if P = nil then // NO VP8X
        begin
          if not imageheader then
            imageheader:= true;
          New(P);
        end;
        P.Lossless:= fourcc = 'VP8L';
        if P.Lossless and (Length(P.ImageBits) > 0) then
          // alpha y vp8l can't go together
        begin
          if ErrorOnBadFrame then
          begin
            Frames.Clear;
            exit;
          end;
          ClearFrame(P);
          P:= Nil;
        end;
        if imageheader and (P <> nil) then
        begin
          I:= Length(P.ImageBits);
          SetLength(P.ImageBits,
            I + Integer(chunksize) + 8);
          Move(fourcc, (@P.ImageBits[I])^, 4);
          Move(chunksize, (@P.ImageBits[I + 4])^, 4);
          Stream.Read((@P.ImageBits[I + 8])^, chunksize);
          Frames.Add(P);
          if DecodeWhileLoading and
            not DecodeFrame(Frames.Count - 1) then
          begin
            if ErrorOnBadFrame then
            begin
              Frames.Clear;
              exit;
            end;
            ClearFrame(P);
            P:= Nil;
          end;
          imageheader:= false;
        end
        else
          Stream.Seek(chunksize, soCurrent);
      end
      else if fourcc = 'ANIM' then
      begin
        Stream.Read(FBackgroundColor, 4);
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
        New(P);
        SetLength(P.BlendedBits, FWidth * FHeight * 4);
        P.Blended:= false;
        P.AlphaCompressed:= 0;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        P.FrameLeft:= ((b3 shl 16) or (b2 shl 8) or b1) * 2;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        P.FrameTop:= ((b3 shl 16) or (b2 shl 8) or b1) * 2;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        P.FrameWidth:= ((b3 shl 16) or (b2 shl 8) or b1) + 1;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        P.FrameHeight:= ((b3 shl 16) or (b2 shl 8) or b1) + 1;
        Stream.Read(b1, 1);
        Stream.Read(b2, 1);
        Stream.Read(b3, 1);
        P.Delay:= ((b3 shl 16) or (b2 shl 8) or b1);
        SetLength(P.ImageBits, 0);
        SetLength(P.DecodedBits, 0);
        Stream.Read(webinfobyte, 1);
        P.Blending:= GetBitsValue(webinfobyte, 6, 1) = 0;
        P.Disposal:= Boolean(GetBitsValue(webinfobyte, 7, 1));
      end
      else if imageheader and (fourcc <> 'ALPH') and
        (fourcc <> 'VP8 ') and (fourcc <> 'VP8X') then
      begin
        imageheader:= false;
        Dispose(P);
        P:= Nil;
      end
      else if imageheader and (P <> nil) and
         (fourcc = 'ALPH') then
      begin
        unknownfourcc:= false;
        SetLength(P.ImageBits, chunksize + 8);
        Move(fourcc, (@P.ImageBits[0])^, 4);
        Move(chunksize, (@P.ImageBits[4])^, 4);
        Stream.Read((@P.ImageBits[8])^, chunksize);
        webinfobyte:= P.ImageBits[8];
        P.PreProcessing:= Boolean(GetBitsValue(webinfobyte, 2, 2));
        P.Filtering:= GetBitsValue(webinfobyte, 4, 2);
        P.AlphaCompressed:= GetBitsValue(webinfobyte, 6, 2) + 1;
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
      if not DecodeWhileLoading then
        for I:= Frames.Count -1 downto 0 do
          if not DecodeFrame(I) then
          begin
            if ErrorOnBadFrame then
            begin
              Frames.Clear;
              break;
            end
            else
            begin
              ClearFrame(PWebpFrame(Frames.Items[I]));
              Frames.Delete(I);
            end;
          end;
      if FSourceAnimated and (Frames.Count < 2) then
        FSourceAnimated:= false;
      if InfiniteLoopAnyways and (LoopCount <> 0) then
        LoopCount:= 0;
      if AnimateOnLoaded and (Frames.Count > 1) and
        (FSourceAnimated or (UserAnimation > 0)) then
      begin
        FAnimationSpeed:= 100;
        Animate;
      end;
      if FWidth = 0 then
        FWidth:= Frames.Frame[0].FrameWidth;
      if FHeight = 0 then
        FHeight:= Frames.Frame[0].FrameHeight;
      if Frames.Count = 1 then
      begin
        PWebpFrame(Frames.Items[0]).FrameLeft:= 0;
        PWebpFrame(Frames.Items[0]).FrameTop:= 0;
      end;
    end;
  end;
end;

function TWebpImage.LoopDurationinMs: Cardinal;
var
  I: Integer;
begin
  Result:= 0;
  if Frames.Count < 2 then
    exit;
  if not FSourceAnimated and (UserAnimation > 0) then
    Result:= UserAnimation * Frames.Count
  else if FSourceanimated then
    for I:= 0 to Frames.Count -1 do
      Inc(Result, Frames.Frame[I].Delay);
end;

procedure TWebpImage.PauseAnimation;
begin
  if (FTimer <> nil) and Ftimer.Enabled then
    FTimer.Enabled:= false;
end;

procedure TWebpImage.RestoreAnimation;
begin
  if (FTimer <> nil) and not Ftimer.Enabled then
    FTimer.Enabled:= true;
end;

procedure TWebpImage.SaveToStream(Stream: TStream);
begin
  inherited;
end;

procedure TWebpImage.SetAltBackgroundColor(Value: TRGBQuad);
begin
  if Cardinal(Value) <>
    Cardinal(FAltBackgroundColor) then
  begin
    FAltBackgroundColor:= Value;
    if FReplaceBackgroundColor and
      (Cardinal(FAltBackgroundColor) <>
      Cardinal(FbackgroundColor)) then
      DrawBackground(Rect(0, 0, FWidth, FHeight));
  end;
end;

procedure TWebpImage.SetHeight(Value: Integer);
begin
  inherited;
end;

procedure TWebpImage.SetPalette(Value: HPalette);
begin
  inherited;  // I ignore the palette, sorry.
end;

procedure TWebpImage.SetReplaceBackgroundColor(Value: Boolean);
begin
  if Value <> FReplaceBackgroundColor then
  begin
    FReplaceBackgroundColor:= Value;
    if Cardinal(FBackgroundColor) <>
      Cardinal(FAltBackgroundColor) then
      DrawBackground(Rect(0, 0, FWidth, FHeight));
  end;
end;

procedure TWebpImage.SetWidth(Value: Integer);
begin
  inherited;
end;

procedure TWebpImage.WriteData(Stream: TStream);
begin
  inherited;

end;

{ TWebpFrameList }

procedure TWebpFrameList.AddFrame(framedata: TWebpFrame);
var
  p: PWebpFrame;
begin
  New(p);
  AssignFrame(framedata, p^);
end;

procedure TWebpFrameList.AssignFrame(SourceFrame: TWebpFrame;
  var DestFrame: TWebpFrame);
begin
  with DestFrame do
  begin
    AlphaCompressed:= SourceFrame.AlphaCompressed;
    Blending:= Sourceframe.Blending;
    DecodedBits:= Copy(Sourceframe.DecodedBits, 0, High(Integer));
    Disposal:= SourceFrame.Disposal;
    Filtering:= SourceFrame.Filtering;
    FrameWidth:= SourceFrame.FrameWidth;
    FrameHeight:= SourceFrame.FrameHeight;
    FrameLeft:= SourceFrame.FrameLeft;
    FrameTop:= SourceFrame.FrameTop;
    Delay:= Sourceframe.Delay;
    ImageBits:= Copy(SourceFrame.ImageBits, 0, High(Integer));
    Lossless:= SourceFrame.Lossless;
    PreProcessing:= SourceFrame.PreProcessing;
    BlendedBits:= Copy(SourceFrame.BlendedBits, 0, hIgh(Integer));
    Blended:= SourceFrame.Blended;
  end;
end;

procedure TWebpFrameList.Clear;
var
  I: Integer;
begin
  for I:= Count -1 downto 0 do
    Delete(I);
end;

procedure TWebpFrameList.Delete(Index: Integer);
begin
  ClearFrame(PWebpFrame(Items[Index]));
  inherited;
end;

destructor TWebpFrameList.Destroy;
begin
  Clear;
  inherited;
end;

function TWebpFrameList.GetFrame(Index: Integer): TWebpFrame;
begin
  Result:= PWebpFrame(Items[Index])^;
end;

procedure TWebpFrameList.SetFrame(Index: Integer; Value: TWebpFrame);
begin
  AssignFrame(PWebpFrame(Items[Index])^, Value);
end;

initialization
  TPicture.RegisterFileFormat('WEBP', sWebpImage,
    TWebpImage);  // Do not localize
  CF_WEBP := RegisterClipboardFormat(PChar(sWebpImage));
  TPicture.RegisterClipboardFormat(CF_WEBP, TWebpImage);

finalization
  TPicture.UnregisterGraphicClass(TWebpImage);

end.
