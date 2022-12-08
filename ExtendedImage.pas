{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{ Adapted by thisuser667                                }
{*******************************************************}
unit ExtendedImage;

interface

uses Winapi.Windows, Vcl.ExtCtrls, Vcl.Imaging.GIFImg, WebpImage,
  System.SysUtils, Vcl.Graphics, System.Types, System.Rtti,
  System.Classes, Vcl.Controls, System.Math;

type
  TExtendedGifImage = class;

  TExtendedRenderer = class(Vcl.imaging.Gifimg.TGifRenderer)
  protected
    Image: TExtendedGifImage;
    procedure DoNextFrame; override;
    procedure Loop; override;
  public
    constructor Create(AImage: TGIFImage); override;
  end;

  TExtendedGifImage = class(Vcl.imaging.Gifimg.TGIFImage)
  private
    FRenderer: TExtendedRenderer;
    FOnNextFrame: TNotifyEvent;
    FFrameIndex: Integer;
  protected
    function CreateRenderer: TCustomGIFRenderer; override;
    property Renderer: TExtendedRenderer read FRenderer;
  public
    property FrameIndex: Integer read FFrameIndex write FFrameIndex;
    property OnNextFrame: TNotifyEvent read FOnNextFrame
      write FOnNextFrame;
  end;

  TGifFrame = class(Vcl.Imaging.GifImg.TGifFrame)
  protected
  end;

  TExtendedImageKind = (ikNone, ikWic, ikGif, ikWebp);

  TExtendedImage = class(TImage)
  private
    FAnimationSpeed: TAnimationSpeed;
    FOnLoop: TNotifyEvent;
    FOnNextFrame: TNotifyEvent;
    FFileName: string;
    FAnimating: Boolean;
    FKind: TExtendedImageKind;
    procedure SetOnNextFrameEvent(Event: TNotifyEvent);
    procedure SetOnLoop(Event: TNotifyEvent);
    function GetOnNextFrameEvent: TNotifyEvent;
    function GetOnLoop: TNotifyEvent;
    procedure SetAnimationSpeed(Value: TAnimationSpeed);
    function GetAnimationSpeed: TAnimationSpeed;
    function GetAnimating: Boolean;
    function GetCurrentFrameIndex: Integer;
    procedure SetAnimating(Value: Boolean);
  public
    function RectImageSize: TSize;
    function RectImage: TRect;
    function CanBeAnimated: Boolean;
    procedure DrawFrame(ACanvas: TCanvas; ARect: TRect;
      DrawFrameIndex: Integer);
    function GetImageCount: Integer;
    function GetGif: TExtendedGifImage;
    function GetWebp: TWebpImage;
    function ImageWidth: Integer;
    function ImageHeight: Integer;
    procedure LoadFromFile(Filename: string);
    function LoopDuration: Integer;
    constructor Create(AOwner: TComponent); override;
    procedure StartAnimation;
    procedure StopAnimation;
    property Animating: Boolean read GetAnimating write
      SetAnimating;
    property FrameINdex: Integer read GetCurrentFrameIndex;
    property ImageKind: TExtendedImageKind read FKind;
  published
    property AnimationSpeed: TAnimationSpeed read GetAnimationSpeed
      write SetAnimationSpeed;
    property OnNextFrame: TNotifyEvent read
      GetOnNextFrameEvent write SetOnNextFrameEvent;
    property OnLoop: TNotifyEvent read GetOnLoop write SetOnLoop;
  end;

var
  AnimateOnLoad: Boolean;

procedure Register;

implementation

uses Vcl.Imaging.GIFConsts;

{var
  gifregistered: Boolean;}

{ TAnimatedImage }

procedure Register;
begin
  RegisterComponents('Samples', [TExtendedImage]);
end;

procedure AdjustImage(var w, h: Integer; destw, desth: Integer);
var
  rt: Single;
begin
  if (h = 0) or (w = 0) then
    exit;
  rt:= w / h;
  try
    if w >= h then
    begin
      h:= Round(Min(destw * h / w, desth));
      w:= Round(h * rt);
    end
    else
    begin
      w:= Round(Min(desth * w / h, destw));
      h:= Round(w / rt);
    end;
  except
  end;
end;

function TExtendedImage.RectImage: TRect;
var
  sz: TSize;
  w, h: Integer;
begin
  sz:= RectImageSize;
  Result:= Rect(0, 0, sz.cx, sz.cy);
  if Center then
  begin
    if Align = alClient then
    begin
      w:= Parent.Width;
      h:= Parent.Height;
    end
    else if (Align = alLeft) or (Align = alRight) then
    begin
      w:= Width;
      h:= Parent.Height;
    end
    else if (Align = alTop) or (Align = alBottom) then
    begin
      w:= Parent.Width;
      h:= Height;
    end
    else
    begin
      w:= Width;
      h:= Height;
    end;
    Result.Offset((w - sz.cx) div 2, (h - sz.cy) div 2);
  end;
end;

function TExtendedImage.RectImageSize: TSize;
var
  w, h, w2, h2: Integer;
begin
  if AutoSize then
    Result:= TSize.Create(ImageWidth, ImageHeight)
  else
  begin
    if Align = alClient then
    begin
      w:= Parent.Width;
      h:= Parent.Height;
    end
    else if (Align = alLeft) or (Align = alRight) then
    begin
      w:= Width;
      h:= Parent.Height;
    end
    else if (Align = alTop) or (Align = alBottom) then
    begin
      w:= Parent.Width;
      h:= Height;
    end
    else
    begin
      w:= Width;
      h:= Height;
    end;
    if Proportional then
    begin
      w2:= ImageWidth;
      h2:= ImageHeight;
      AdjustImage(w2, h2, w, h);
      Result:= TSize.Create(w2, h2);
    end
    else if not Stretch then
      Result:= TSize.Create(Min(w, ImageWidth),
        Min(h, ImageHeight))
    else
      Result:= TSize.Create(w, h);
  end;
end;

function TExtendedImage.CanBeAnimated: Boolean;
begin
  if FKind = ikGif then
    Result:= GetGif.Images.Count > 1
  else if FKind = ikWebp then
    Result:= TWebpImage.CanBeAnimated(FFilename)
  else
    Result:= false;
end;

constructor TExtendedImage.Create(AOwner: TComponent);
begin
  inherited;
  FAnimating:= false;
  FOnLoop:= nil;
  FKind:= ikNone;
  FOnNextFrame:= nil;
  FFileName:= '';
  FAnimationSpeed:= 100;
end;

procedure TExtendedImage.DrawFrame(ACanvas: TCanvas;
  ARect: TRect; DrawFrameIndex: Integer);
var
  Bitmap: TBitmap;
  Gf: TExtendedGifImage;
  Gr: TGifRenderer;
  Index: Integer;
begin
  if (FFileName = '') or (DrawFrameIndex < 0) or
    (DrawFrameIndex > GetImageCount - 1) then
    exit;
  if FKind = ikGif then
  begin
    Gf:= GetGif;
    Bitmap:= TBitmap.Create;
    try
      Gr:= TGifRenderer.Create(Gf);
      try
        Bitmap.SetSize(Gf.Width, Gf.Height);
        for Index:= 0 to Gf.Images.Count -1 do
        begin
          if Gf.Images[Index].Empty then
            continue;
          Gr.Draw(Bitmap.Canvas, Bitmap.Canvas.ClipRect);
          if Index = DrawFrameIndex then
          begin
            ACanvas.StretchDraw(ARect, Bitmap);
            exit;
          end;
          Gr.NextFrame;
        end;
      finally
        Gr.Free;
      end;
    finally
      Bitmap.Free;
    end;
  end
  else if FKind = ikWebp then
    GetWebp.DrawFrame(ACanvas, ARect, DrawFrameIndex);
end;

function TExtendedImage.GetCurrentFrameIndex: Integer;
begin
  Result:= 0;
  if (csDesigning in ComponentState) or
    not CanBeAnimated or not Animating then
    exit;
  if FKind = ikGif then
    Result:= GetGif.FrameIndex
  else if FKind = ikWebp then
    Result:= GetWebp.CurrentFrame;
end;

function TExtendedImage.GetGif: TExtendedGIFImage;
begin
  if FKind = ikGif then
    Result:= TExtendedGifImage(Picture.Graphic)
  else
    Result:= nil;
end;

function TExtendedImage.GetImageCount: Integer;
begin
  if FKind = ikGif then
    Result:= GetGif.Images.Count
  else if FKind = ikWebp then
    Result:= GetWebp.Frames.Count
  else
    Result:= 0;
end;

function TExtendedImage.GetOnLoop: TNotifyEvent;
begin
  if not (csDesigning in ComponentState) and Animating then
  begin
    if FKind = ikGif then
      FOnLoop:= GetGif.OnLoop
    else if FKind = ikWebp then
      FOnLoop:= Getwebp.OnLoop;
  end;
  Result:= FOnLoop;
end;

function TExtendedImage.GetOnNextFrameEvent: TNotifyEvent;
begin
  if not (csDesigning in ComponentState) and Animating then
  begin
    if FKind = ikGif then
      FOnNextFrame:= GetGif.OnNextFrame
    else if FKind = ikWebp then
      FOnNextFrame:= GetWebp.OnNextFrame;
  end;
  Result:= FOnNextFrame;
end;

function TExtendedImage.GetAnimating: Boolean;
begin
  if not (csDesigning in ComponentState) and
    CanbeAnimated then
  begin
    if FKind = ikGif then
      FAnimating:= GetGif.Animate
    else if FKind = ikWebp then
      FAnimating:= GetWebp.Animating;
  end;
  Result:= FAnimating;
end;

function TExtendedImage.GetAnimationSpeed: TAnimationSpeed;
begin
  Result:= 100;
  if (csDesigning in ComponentState) or
    not CanbeAnimated or not Animating then
    exit;
  if FKind = ikGif then
    FAnimationSpeed:= GetGif.AnimationSpeed
  else if FKind = ikWebp then
    FanimationSpeed:= GetWebp.AnimationSpeed;
  Result:= FAnimationSpeed;
end;

function TExtendedImage.GetWebp: TWebpImage;
begin
  if FKind = ikWebp then
    Result:= TWebpImage(Picture.Graphic)
  else
    Result:= nil;
end;

function TExtendedImage.ImageHeight: Integer;
begin
  if FFileName = '' then
    Result:= 0
  else
    Result:= Picture.Height;
end;

function TExtendedImage.ImageWidth: Integer;
begin
  if FFileName = '' then
    Result:= 0
  else
    Result:= Picture.Width;
end;

procedure TExtendedImage.LoadFromFile(Filename: string);
var
  sext: string;
  wic: TWicImage;
  signature: array[0..$C] of AnsiChar;
  f: TFileStream;
  fk: TExtendedImageKind;
begin
  Picture.Graphic:= nil;
  if FileExists(FileName) then
  begin
    try
      fk:= ikNone;
      f:= TfileStream.Create(FileName, fmOpenRead);
      try
        f.Read(signature, $C);
        if Pos('WEBP', signature + 8) = 1 then
          fk:= ikWebp
        else if Pos('GIF', signature) = 1 then
          fk:= ikGif;
      except
      end;
      f.Free;
      sext:= ExtractFileExt(FileName);
      if fk = ikNone then
      begin
        wic:= TWicImage.Create;
        try
          wic.LoadFromFile(FileName);
          Picture.Assign(wic);
          FKind:= ikWic;
        finally
          wic.Free;
        end;
      end
      else
      begin
        WebpImage.AnimateOnLoaded:= AnimateOnLoad;
        Picture.LoadFromFile(FileName);
      end;
      FFileName:= FileName;
      if Picture.Graphic.ClassName = 'TGIFImage' then
      begin
        FKind:= ikGif;
        with TExtendedGifImage(Picture.Graphic) do
        begin
          OnLoop:= FOnLoop;
          AnimationSpeed:= FAnimationSpeed;
          OnNextFrame:= FOnNextFrame;
        end;
      end
      else if Picture.Graphic is TWebpImage then
      begin
        FKind:= ikWebp;
        with TWebpImage(Picture.Graphic) do
        begin
          OnLoop:= FOnLoop;
          AnimationSpeed:= FAnimationSpeed;
          OnNextFrame:= FOnNextFrame;
        end;
      end;
      if (FKind > ikWic) and CanBeAnimated then
        StartAnimation;
    except
    end;
  end;
end;

function TExtendedImage.LoopDuration: Integer;
var
  I: Integer;
  gf: TExtendedGifImage;
begin
  Result:= 0;
  if not CanbeAnimated then
    exit;
  if FKind = ikGif then
  begin
    gf:= GetGif;
    for I:= 0 to gf.Images.Count -1 do
      Result:= Result + TGifFrame(gf.Images.Frames[I]).GCE.Delay * 10;
  end
  else if FKind = ikWebp then
    Result:= GetWebp.LoopDurationinMs;
end;

procedure TExtendedImage.SetOnLoop(Event: TNotifyEvent);
begin
  FOnLoop:= Event;
  if not (csDesigning in ComponentState) and CanBeAnimated then
    if FKind = ikGif then
      GetGif.OnLoop:= Event
    else if FKind = ikWebp then
      Getwebp.OnLoop:= Event;
end;

procedure TExtendedImage.SetOnNextFrameEvent(Event: TNotifyEvent);
begin
  FOnNextFrame:= Event;
  if not (csDesigning in ComponentState) and CanBeAnimated then
    if FKind = ikGif then
      GetGif.OnNextFrame:= Event
    else if FKind = ikWebp then
      Getwebp.OnNextFrame:= Event;
end;

procedure TExtendedImage.SetAnimating(Value: Boolean);
begin
  if FAnimating <> Value then
  begin
    if (csDesigning in ComponentState) or
      not CanbeAnimated then
    begin
      Fanimating:= false;
      exit;
    end;
    FAnimating:= Value;
    if FAnimating then
      StartAnimation
    else
      StopAnimation;
  end;
end;

procedure TExtendedImage.SetAnimationSpeed(Value: TAnimationSpeed);
var
  gf: TExtendedGifImage;
  wb: TWebpImage;
begin
  if Value <> FAnimationSpeed then
  begin
    FAnimationSpeed:= Value;
    if (csDesigning in ComponentState) or
      not CanbeAnimated then
      exit;
    if FKind = ikGif then
    begin
      gf:= GetGif;
      if gf.Animate then
      begin
        gf.Animate:= false;
        gf.AnimationSpeed:= Value;
        gf.Animate:= true;
      end;
    end
    else if FKind = ikWebp then
    begin
      wb:= GetWebp;
      if wb.Animating then
        wb.AnimationSpeed:= Value;
    end;
  end;
end;

procedure TExtendedImage.StartAnimation;
begin
  if not CanbeAnimated or (csDesigning in ComponentState) then
    exit;
  if FKind = ikGif then
    GetGif.Animate:= true
  else
    GetWebp.Animate;
end;

procedure TExtendedImage.StopAnimation;
begin
  if not CanbeAnimated or (csDesigning in ComponentState) then
    exit;
  if FKind = ikGif then
    GetGif.Animate:= false
  else
    GetWebp.PauseAnimation;
end;

{ TExtendedGifImage }

function TExtendedGifImage.CreateRenderer: TCustomGIFRenderer;
begin
  Result := TExtendedRenderer.Create(Self);
  Result.Speed := AnimationSpeed;
  Result.Transparent := Transparent;
  Result.BackgroundColor := EffectiveBackgroundColor;
  Result.Animate := Animate;
  TExtendedRenderer(Result).Image:= Self;
end;

{ TExtendedRenderer }

constructor TExtendedRenderer.Create(AImage: TGIFImage);
begin
  inherited;
  Self.Image:= TExtendedGifImage(Image);
end;

procedure TExtendedRenderer.DoNextFrame;
begin
  inherited;
  Image.FrameIndex:= FrameIndex;
  if Assigned(Image.OnNextFrame) then
    Image.OnNextFrame(Image);
end;

procedure TExtendedRenderer.Loop;
begin
  inherited;
  if Assigned(Image.OnLoop) then
    Image.OnLoop(Image);
end;

initialization
  AnimateOnLoad:= true;

end.
