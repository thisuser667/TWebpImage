unit AnimatedBox;

{$IFDEF FPC}
{$mode objfpc}
{$H+}
{$ENDIF}


interface

uses
   /// WARNING: THIS CAN'T BE INSTALLED AS A PACKAGE OR PALETTE COMPONENT
  ///  IN LAZARUS. IN LAZARUS JUST CREATE IT AT RUN TIME.

  {$IFNDEF FPC and IFDEF WINDOWS}
  {$DEFINE NOFPCW}
  WinApi.Windows, Winapi.Messages, Winapi.WinCodec, System.SysUtils,
    System.AnsiStrings, Vcl.Graphics, System.Classes, System.Math,
    Vcl.ExtCtrls, System.Contnrs, Vcl.Forms, Vcl.Controls,
    System.Types,
  {$ELSE}
  LclType, LMessages, LclIntF, SysUtils, Graphics, Classes, Math,
  Controls, Contnrs, Forms, Types,
  {$ENDIF}
  MovieImage, MovieImageAuxUnit;

type
  TOtherImageKind = (ikNone, ikWic, ikGif, ikWebp);

  { TAnimatedBox }

  TAnimatedBox = class(TCustomControl)
  private
    FAnimationSpeed: TAnimationSpeed;
    FOnLoop: TNotifyEvent;
    FOnNextFrame: TNotifyEvent;
    FFileName: string;
    FKind: TOtherImageKind;
    FBorderStyle: TBorderStyle;
    FProportional, FAutoSize, FCenter, FStretch: Boolean;
    FImageRect: TRect;
    FOnPaint: TNotifyEvent;
    FResizing: Boolean;
    FScale: Single;
    FImage: TImageBase;
    FNotPaint: Boolean;
    FImageValid: Boolean;
    FUpdatingControl: Boolean;
    FZoomFactor: Single;
    procedure SetOnNextFrameEvent(Event: TNotifyEvent);
    procedure SetOnLoop(Event: TNotifyEvent);
    function GetOnNextFrameEvent: TNotifyEvent;
    function GetOnLoop: TNotifyEvent;
    procedure SetAnimationSpeed(Value: TAnimationSpeed);
    function GetAnimationSpeed: TAnimationSpeed;
    function GetAnimating: Boolean;
    function GetCurrentFrameIndex: Integer;
    procedure SetImageRect(Value: TRect);
    function GetFrames: TMovieFrameList;
    procedure SetProportional(Value: Boolean);
    procedure SetCenter(Value: Boolean);
    procedure SetScale(Value: Single);
    procedure SetStretch(Value: Boolean);
    procedure SetImageAutoSize(Value: Boolean);
    procedure SetZoomFactor(Value: Single);
    {$IFDEF NOFPCW}
    procedure SetBorderStyle(Value: TBorderStyle);
    {$ENDIF}
    procedure UpdateControl;
    //procedure WMSIZE(var Message: TWmSize);
    //{$IFDEF NOFPCW}
    //procedure WMSIZE(var Message: TWmSize); message WM_SIZE;
    //{$ELSE}
    //procedure WMSIZE(var Message: TLMSize); message LM_SIZE;
    //{$ENDIF}
   protected
    procedure Paint; override;
    {$IFNDEF NOFPCW}
    procedure SetBorderStyle(Value: TBorderStyle); override;
    procedure WndProc(var Message: TLMessage); override;
    {$ELSE}
    procedure WndProc(var Message: TMessage); override;
    {$ENDIF}
   public
    function CanBeAnimated(FileName: string = ''): Boolean;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      DrawFrameIndex: Integer);
    function IsGifOrWebp: Boolean;
    procedure SetPausedState(Value: Boolean);
    function GetImageCount: Integer;
    function ImageWidth: Integer;
    function ImageHeight: Integer;
    procedure LoadFromFile(Filename: string);
    function LoopDuration: Integer;
    constructor Create(AOwner: TComponent); override;
    procedure StartAnimation;
    procedure StopAnimation;
    procedure ZoomIn;
    procedure ZoomOut;
    destructor Destroy; override;
    property Animating: Boolean read GetAnimating;
    property Canvas;
    property FrameIndex: Integer read GetCurrentFrameIndex;
    property ImageKind: TOtherImageKind read FKind;
    property Imagerect: TRect read FImageRect write SetImagerect;
    property Frames: TMovieFrameList read GetFrames;
    property Scale: Single read FScale write SetScale;
  published
    property AnimationSpeed: TAnimationSpeed read GetAnimationSpeed
      write SetAnimationSpeed default 100;
    property Kind: TOtherImageKind read FKind;
    property OnNextFrame: TNotifyEvent read
      GetOnNextFrameEvent write SetOnNextFrameEvent;
    property OnLoop: TNotifyEvent read GetOnLoop write SetOnLoop;
    property Proportional: Boolean read FProportional write SetProportional;
    property AutoSize: Boolean read FAutoSize write SetImageAutoSize;
    property Center: Boolean read FCenter write SetCenter;
    property Stretch: Boolean read FStretch write SetStretch;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property ZoomFactor: Single read FZoomFactor write SetZoomFactor;
    property Align;
    property Anchors;
    {$IFDEF NOFPCW}
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property Ctl3D;
    property Padding;
    property ParentBiDiMode;
    property ParentCtl3D;
    property Touch;
    property StyleElements;
    property StyleName;
    property OnCanResize;
    property OnGesture;
    property OnMouseActivate;
    {$ENDIF}
    property BorderStyle: TBorderStyle read FBorderStyle
      write SetBorderStyle default bsSingle;
    property Constraints;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Color nodefault;
    property Font;
    property ParentBackground default False;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

// the next three declarations are tested only in Windows.
// I haven't found another way to implement High DPI in Lazarus
// but I am not very interested in Lazarus
{$IFNDEF NOFPCW and $IFDEF WINDOWS}
type
    DEVICE_SCALE_FACTOR = (
    DEVICE_SCALE_FACTOR_INVALID = 0,
    SCALE_100_PERCENT = 100,
    SCALE_120_PERCENT = 120,
    SCALE_125_PERCENT = 125,
    SCALE_140_PERCENT = 140,
    SCALE_150_PERCENT = 150,
    SCALE_160_PERCENT = 160,
    SCALE_175_PERCENT = 175,
    SCALE_180_PERCENT = 180,
    SCALE_200_PERCENT = 200,
    SCALE_225_PERCENT = 225,
    SCALE_250_PERCENT = 250,
    SCALE_300_PERCENT = 300,
    SCALE_350_PERCENT = 350,
    SCALE_400_PERCENT = 400,
    SCALE_450_PERCENT = 450,
    SCALE_500_PERCENT = 500);
  TDeviceScaleFactor = DEVICE_SCALE_FACTOR;
  PDeviceScaleFactor = ^TDeviceScaleFactor;

function GetScaleFactorForMonitor(hMon: HMONITOR; var pScale: TDeviceScaleFactor): HRESULT;
  stdcall; external 'shcore.dll' Name 'GetScaleFactorForMonitor';
function GetPPi: Integer;
function GetPixelsFactor: single;
{$ENDIF}

procedure Register;

implementation

{ TAnimatedBox }

{$IFNDEF NOFPCW and $IFDEF WINDOWS}
function GetPPi: Integer;
var
  sres: HRESULT;
  d: TDeviceScaleFactor;
begin
  Result := 96;
  sres := GetScaleFactorForMonitor(Screen.PrimaryMonitor.Handle, d);
  if sres = S_Ok then
    Result := integer(d);
end;

function GetPixelsFactor: single;
begin
  Result := GetPPi / 100;
end;
{$ENDIF}


procedure Register;
begin
  RegisterComponents('Samples', [TAnimatedBox]);
end;

constructor TAnimatedBox.Create(AOwner: TComponent);
begin
  inherited;
  FProportional:= false;
  FStretch:= false;
  FCenter:= false;
  FResizing:= false;
  FAnimationSpeed:= 100;
  FNotPaint:= false;
  FAutoSize:= false;
  ImageRect:= BoundsRect;
  FImage:= TImageBase.Create;
  FImageValid:= false;
  FZoomFactor:= 0.1;
end;

destructor TAnimatedBox.Destroy;
begin
  FImageValid:= false;
  FreeAndNil(FImage);
  inherited;
end;

procedure TAnimatedBox.DrawFrame(ACanvas: TCanvas; ARect: TRect;
      DrawFrameIndex: Integer);
begin
  if FImageValid then
  begin
    with ACanvas do
    begin
      Brush.Color:= Self.Color;
      Brush.Style:= bsSolid;
      FillRect(Cliprect);
    end;
    if FNotPaint then
      FNotPaint:= false;
    FImage.DrawFrame(ACanvas, ARect, DrawFrameIndex);
    FNotPaint:= IsGifOrWebp and (TMovieImageBase(FImage).Frames.
      Count > 1);
  end;
end;

function TAnimatedBox.GetAnimating: Boolean;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).Animating
  else
    Result:= false;
end;

function TAnimatedBox.GetAnimationSpeed: TAnimationSpeed;
begin
  if (not FImageValid) or not IsGifOrWebp then
    Result:= FAnimationSpeed
  else
    Result:= TMovieImageBase(FImage).AnimationSpeed;
end;

function TAnimatedBox.GetCurrentFrameIndex: Integer;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).FrameIndex
  else
    Result:= 0;
end;

function TAnimatedBox.GetFrames: TMovieFrameList;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).Frames
  else
    Result:= nil;
end;

function TAnimatedBox.GetImageCount: Integer;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).Frames.Count
  else
    Result:= 1;
end;

function TAnimatedBox.GetOnLoop: TNotifyEvent;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).OnLoop
  else
    Result:= nil;
end;

function TAnimatedBox.GetOnNextFrameEvent: TNotifyEvent;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).OnNextFrame
  else
    Result:= nil;
end;

function TAnimatedBox.ImageHeight: Integer;
begin
  Result:= 0;
  if FImageValid then
    if IsGifOrWebp then
      Result:= TMovieImageBase(FImage).ImageHeight
    else
      Result:= FImage.Height;
end;

function TAnimatedBox.ImageWidth: Integer;
begin
  Result:= 0;
  if FImageValid then
    if IsGifOrWebp then
      Result:= TMovieImageBase(FImage).ImageWidth
    else
      Result:= FImage.Width;
end;

function TAnimatedBox.IsGifOrWebp: Boolean;
begin
  Result:= (FImage is TMovieGifImage) or (FImage is TMovieWebpImage);
end;

function TAnimatedBox.CanBeAnimated(FileName: string = ''): Boolean;
begin
  if FileExists(FileName) then
    Result:= TMovieImageBase.CanBeAnimated(FileName)
  else if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).CanBeAnimated
  else
    Result:= false;
end;

procedure AdjustImage(var w, h: Integer; destw, desth: Integer;
  respectmin: Boolean = false);
var
  rt: Single;
begin
  if (h = 0) or (w = 0) then
    exit;
  rt:= w / h;
  try
    if w >= h then
    begin
      h:= Min(Round(destw * h / w), desth);
      if respectmin and (h * rt > w) then
        h:= Round(w / rt);
      w:= Round(h * rt);
    end
    else
    begin
      w:= Min(Round(desth * w / h), destw);
      if respectmin and (w / rt > h) then
        w:= Round(h * rt);
      h:= Round(w / rt);
    end;
  except
  end;
end;

procedure TAnimatedBox.LoadFromFile(Filename: string);
var
  sext: string;
begin
  if not FileExists(FileName) then
    raise Exception.Create('The file does not exist');
  sext:= string(GetImageExtensionBySignature(FileName));
  if Animating then
    StopAnimation;
  if FImageValid then
  begin
    FImage.Free;
    FImageValid:= false;
  end;
  try
    if sext = '.gif' then
      FImage:= TMovieGifImage.Create(Canvas)
    else if sext = '.webp' then
      FImage:= TMovieWebpImage.Create(Canvas)
    else
      FImage:= TImageBase.Create(Canvas);
    if FImage <> nil then
    begin
      try
        FImage.LoadFromFile(FileName);
        FScale:= 1;
        FFileName:= FileName;
        FImageValid:= true;
        FNotPaint:= IsGifOrWebp and (TMovieImageBase(FImage).Frames.
          Count > 1);
        FUpdatingControl:= false;
        UpdateControl;
        if CanbeAnimated and AnimateOnloaded then
          StartAnimation;
      except
        FImage:= nil;
        FFileName:= '';
      end;
    end;
  except
  end;
end;

function TAnimatedBox.LoopDuration: Integer;
begin
  if IsGifOrWebp then
    Result:= TMovieImageBase(FImage).LoopDurationinMs
  else
    Result:= 0;
end;

procedure TAnimatedBox.Paint;
begin
  with Canvas do
  begin
    Brush.Color:= Self.Color;
    Brush.Style:= bsSolid;
    FillRect(Cliprect);
    if FNotPaint then
      exit;
    if FImageValid then
    begin
      FImage.Canvas:= Canvas;
      DrawFrame(Canvas, ImageRect, FrameIndex);
    end;
    if Assigned(FOnPaint) then
      FOnPaint(Self);
  end;
end;

procedure TAnimatedBox.SetAnimationSpeed(Value: TAnimationSpeed);
begin
  if Value <> FAnimationSpeed then
  begin
    FAnimationSpeed:= Value;
    if (csDesigning in ComponentState) or
      not CanbeAnimated then
      exit;
    if FImage is TMovieImageBase then
      TMovieImageBase(FImage).AnimationSpeed:= FAnimationSpeed;
  end;
end;

procedure TAnimatedBox.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    RecreateWnd{$IFNDEF NOFPCW}(Self){$ENDIF};
  end;
end;

procedure TAnimatedBox.SetImageAutoSize(Value: Boolean);
begin
  if Value <> FAutoSize then
  begin
    FAutoSize:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

procedure TAnimatedBox.SetImageRect(Value: TRect);
begin
  if FImageValid and (FImageRect <> Value) then
  begin
    FImageRect:= Value;
    FImage.CanvasRect:= FImageRect;
    FScale:= FImageRect.Width / FImage.ImageWidth;
    if FUpdatingControl then
      exit;
    if IsGifOrWebp then
    begin
      TMovieImageBase(FImage).NeedCompleteRedraw:= true;
      if CanbeAnimated then
      begin
        if Animating then
          StopAnimation;
        StartAnimation;
        exit;
      end;
    end;
    Refresh;
  end;
end;

procedure TAnimatedBox.SetCenter(Value: Boolean);
begin
  if Value <> FCenter then
  begin
    FCenter:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

procedure TAnimatedBox.SetOnLoop(Event: TNotifyEvent);
begin
  if not (IsGifOrWebp) then
    exit;
  with TMovieImageBase(FImage) do
    FOnLoop:= Event;
end;

procedure TAnimatedBox.SetOnNextFrameEvent(Event: TNotifyEvent);
begin
  if not (IsGifOrWebp) then
    exit;
  with TMovieImageBase(FImage) do
    FOnNextFrame:= Event;
end;

procedure TAnimatedBox.SetPausedState(Value: Boolean);
begin

end;

procedure TAnimatedBox.SetProportional(Value: Boolean);
begin
  if Value <> Fproportional then
  begin
    Fproportional:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

{$IFNDEF NOFPCW}
function RectF(Left, Top, Right, Bottom: single): TRectF; overload;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function RectF(Rc: TRect): TREctF; overload;
begin
  Result:= RectF(Rc.Left, Rc.Top, Rc.Right, Rc.Bottom);
end;

function PointFRound(pt: Types.TPointF): Types.TPoint;
begin
  Result.X := Round(pt.X);
  Result.Y := Round(pt.Y);
end;

{$ENDIF}

function RectFRound(rc: {$IFDEF NOFPCW}System.{$ENDIF}Types.
  TRectF): TRect;
begin
  {$IFNDEF NOFPCW}
  Result.TopLeft := PointFRound(rc.TopLeft);
  Result.BottomRight := PointFRound(rc.BottomRight);
  {$ELSE}
  Result:= rc.Round;
  {$ENDIF}
end;

procedure TAnimatedBox.SetScale(Value: Single);
begin
  if (Value <> FScale) and FImageValid then
  begin
    FScale:= Value;
    ImageRect:= RectFRound({$IFDEF NOFPCW}System.Types.{$ENDIF}
      RectF(0, 0, FImage.ImageWidth * Value,
      FImage.ImageHeight * Value));
    //Refresh;
  end;
end;

procedure TAnimatedBox.SetStretch(Value: Boolean);
begin
  if Value <> FStretch then
  begin
    FStretch:= Value;
    if not FUpdatingControl then
      UpdateControl;
  end;
end;

procedure TAnimatedBox.SetZoomFactor(Value: Single);
begin
  if (Value > 0.05) and (Value <> FZoomFactor) then
    FZoomFactor:= Value;
end;

procedure TAnimatedBox.StartAnimation;
begin
  if not (IsGifOrWebp) or (Frames.Count < 2) or not CanbeAnimated or
    (csDesigning in ComponentState) then
    exit;
  TMovieImageBase(FImage).LoopIndex:= 0;
  TMovieImageBase(FImage).Animate;
end;

procedure TAnimatedBox.StopAnimation;
begin
  if not (IsGifOrWebp) then
    exit;
  with TMovieImageBase(FImage) do
  begin
    if not CanbeAnimated or (csDesigning in ComponentState) then
      exit;
    StopAnimation;
  end;
end;

procedure TAnimatedBox.UpdateControl;
var
  w, h: Integer;
  rc: TRectF;
  animated: Boolean;
begin
  if FUpdatingControl then
    exit;
  FUpdatingControl:= true;
  FResizing:= true;
  try
    animated:= Animating;
    if animated then
      TMovieImageBase(FImage).PauseAnimation;
    if FAutosize then
    begin
      if FImageValid then
      begin
        Align:= alNone;
        Width:= ImageWidth;
        Height:= ImageHeight;
        rc:= RectF(0, 0, Width, Height);
      end;
    end
    else
    begin
      if FImageValid then
      begin
        w:= ImageWidth;
        h:= ImageHeight;
        if FProportional then
          AdjustImage(w, h, Width, Height)
        else if FStretch then
        begin
          w:= Width;
          h:= Height;
        end;
        rc:= RectF(0, 0, w, h);
        if FCenter then
          rc.Offset((Width - w) / 2, (Height - h) / 2);
      end;
    end;
    if FImageValid then
      ImageRect:= RectFRound(rc);
    Refresh;
    if animated then
      TMovieImageBase(FImage).RestoreAnimation;
  finally
    FResizing:= false;
    FUpdatingControl:= false;
  end;
end;

{$IFDEF NOFPCW}
procedure TAnimatedBox.WndProc(var Message: TMessage);
{$ELSE}
procedure TAnimatedBox.WndProc(var Message: TLMessage);
{$ENDIF}
var
  wp: PWindowPos;
begin
  inherited;
  {$IFDEF NOFPCW}
  if Message.Msg = WM_WINDOWPOSCHANGED then
  {$ELSE}
  if Message.Msg = LM_WINDOWPOSCHANGED then
  {$ENDIF}
  begin
    wp:= Pointer(Message.LParam);
    if wp = nil then
      exit;
    if wp^.hwnd = Self.Handle then
    begin
      if FResizing then
        exit;
      if FImageValid and IsGifOrWebp then
        TMovieImageBase(FImage).NeedCompleteRedraw:= true;
      FUpdatingControl:= false;
      UpdateControl;
    end;
  end;
end;

procedure TAnimatedBox.ZoomIn;
begin
  SetScale(FScale + FZoomFactor);
end;

procedure TAnimatedBox.ZoomOut;
begin
  SetScale(Max(FZoomFactor, FScale - FZoomFactor));
end;

end.
