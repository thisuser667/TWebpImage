# TWebpImage
Descendant of delphi class TGraphic for webp and gif graphic file formats and others. Includes animation. Definitive and last implementation.

Implementation of a new graphic class for Delphi's TGraphic class, for the webp graphic format (libwebp.dll 1.2.4). As far as I know, nothing similar exists yet. It can be a valid starting point to do something more professional.

It doesn't include the libwebp.dll library. You can download it precompiled in https://developers.google.com/speed/webp/download. If you don't know how to, just google for the library itself.

MovieImage.pas is the base class for loading many picture formats and animating Gif and Webp images. Gif and Webp are almost entirely proppietary implementations, except decoding images, in Gif the decoding can be found at: http://www.tolderlund.eu/delphi/gifimaged2010b.zip. And for Webp the decoding uses Libwebp124.pas.

Simple use: 

AnimatedImage.LoadFromFile  in any jpeg,png, etc., gif, and webp image files.

AnimatedImage.StartAnimation if gif or webp file

AnimatedImage.StopAnimation idem

You can also paint in AnimatedImage anytime you want in the OnPaint event, unlike TImage in which it has to be an Tbitmap in the graphic.

