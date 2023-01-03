# TWebpImage
Descendant of delphi class TGraphic for webp and gif graphic file formats and others. Includes animation. Definitive and last implementation.

Implementation of a new graphic class for Delphi's TGraphic class, for the webp graphic format (libwebp.dll 1.2.4). As far as I know, nothing similar exists yet. It can be a valid starting point to do something more professional.

It doesn't include the libwebp.dll library. You can download it precompiled in https://developers.google.com/speed/webp/download. If you don't know how to, just google for the library itself.

MovieImage.pas is the base class for loading many picture formats and animating Gif and Webp images. Gif and Webp are almost entirely proppietary implementations, except decoding images, in Gif the decoding can be found at: http://www.tolderlund.eu/delphi/gifimaged2010b.zip. And for Webp the decoding uses Libwebp124.pas.

This last mentioned file, Libwebp124.pas is valid for Delphi as well as for Lazarus, but the directive "delayed" in extern declarations is valid only for Delphi, so any component that uses MovieImage only can be installed in Delphi, in Lazarus must be created at run time. It is included such component in AnimatedImage.pas.

Simple use: 

AnimatedImage.LoadFromFile  in any jpeg,png, etc., gif, and webp image files.

AnimatedImage.StartAnimation
AnimatedImage.DrawFrame, etc.

You can also paint in AnimatedImage anytime you want in the OnPaint event, unlike TImage in which it has to be an Tbitmap in the graphic.

