# TWebpImage
Descendant of delphi class TGraphic for webp graphic file format. Includes animation.

Implementation of a new graphic class for Delphi's TGraphic class, for the webp graphic format (libwebp.dll 1.2.4). As far as I know, nothing similar exists yet. It can be a valid starting point to do something more professional.

It doesn't include the libwebp.dll library. You can download it precompiled in https://developers.google.com/speed/webp/download. If you don't know how to, just google for the library itself.

WebpImage is for Windows, LazWebpImage for FPC.

All the extern declarations in libweb124.pas are now declared as 'delayed'. This is because of the new file, ExtendedImage, a descendant of TImage that is a kind of intersection of GifImage, WebpImage and finally, because TPngImage and TJpegImage fail with some files, instead of use them, just use another TWicImage temporary file with the static formats. Without the 'delayed', trying to install ExtendedImage in a package, returns "Entry point not found". ExtendedImage is only for Windows.
