unit ThemesEx;

interface

{$MINENUMSIZE 4}

uses
  Windows;

//------------------------------------------------------------------------
//  BufferedPaintInit() - Initialize the Buffered Paint API.
//                        Should be called prior to BeginBufferedPaint,
//                        and should have a matching BufferedPaintUnInit.
//------------------------------------------------------------------------
var
  BufferedPaintInit: function: HRESULT; stdcall;

//------------------------------------------------------------------------
//  BufferedPaintUnInit() - Uninitialize the Buffered Paint API.
//                          Should be called once for each call to BufferedPaintInit,
//                          when calls to BeginBufferedPaint are no longer needed.
//------------------------------------------------------------------------
var
  BufferedPaintUnInit: function: HRESULT; stdcall;

//------------------------------------------------------------------------
//  BeginBufferedPaint() - Begins a buffered paint operation.
//
//    hdcTarget          - Target DC on which the buffer will be painted
//    rcTarget           - Rectangle specifying the area of the target DC to paint to
//    dwFormat           - Format of the buffer (see BP_BUFFERFORMAT)
//    pPaintParams       - Paint operation parameters (see BP_PAINTPARAMS)
//    phBufferedPaint    - Pointer to receive handle to new buffered paint context
//------------------------------------------------------------------------
type
  HPAINTBUFFER = THandle;      // handle to a buffered paint context

  BP_BUFFERFORMAT = (
    BPBF_COMPATIBLEBITMAP,    // Compatible bitmap
    BPBF_DIB,                 // Device-independent bitmap
    BPBF_TOPDOWNDIB,          // Top-down device-independent bitmap
    BPBF_TOPDOWNMONODIB,
    BPBF_COMPOSITED = BPBF_TOPDOWNMONODIB
  );
  TBPBufferFormat = BP_BUFFERFORMAT;

  BP_ANIMATIONSTYLE = (
    BPAS_NONE,                // No animation
    BPAS_LINEAR,              // Linear fade animation
    BPAS_CUBIC,               // Cubic fade animation
    BPAS_SINE                 // Sinusoid fade animation
  );
  TBPAnimationStyle = BP_ANIMATIONSTYLE;

  BP_ANIMATIONPARAMS = packed record
    cbSize: DWORD;
    dwFlags: DWORD;
    style: TBPAnimationStyle;
    dwDuration: DWORD;
  end;
  TBPAnimationParams = BP_ANIMATIONPARAMS;
  PBPAnimationParams = ^TBPAnimationParams;

const
  BPPF_ERASE               = $0001; // Empty the buffer during BeginBufferedPaint()
  BPPF_NOCLIP              = $0002; // Don't apply the target DC's clip region to the double buffer
  BPPF_NONCLIENT           = $0004; // Using a non-client DC

type
  BP_PAINTPARAMS = packed record
    cbSize: DWORD;
    dwFlags: DWORD; // BPPF_ flags
    prcExclude: PRect;
    pBlendFunction: PBlendFunction;
  end;
  TBPPaintParams = BP_PAINTPARAMS;
  PBPPaintParams = ^TBPPaintParams;

var
  BeginBufferedPaint: function(hdcTarget: HDC; const rcTarget: TRect;
    dwFormat: TBPBufferFormat; pPaintParams: PBPPaintParams;
    out hdc: HDC): HPAINTBUFFER; stdcall;

//------------------------------------------------------------------------
//  EndBufferedPaint() - Ends a buffered paint operation.
//
//    hBufferedPaint   - handle to buffered paint context
//    fUpdateTarget    - update target DC
//---------------------------------------------s---------------------------
  EndBufferedPaint: function(hBufferedPaint: HPAINTBUFFER; fUpdateTarget: BOOL): HRESULT; stdcall;

//------------------------------------------------------------------------
//  GetBufferedPaintTargetRect() - Returns the target rectangle specified during BeginBufferedPaint
//
//    hBufferedPaint             - handle to buffered paint context
//    prc                        - pointer to receive target rectangle
//------------------------------------------------------------------------
  GetBufferedPaintTargetRect: function(hBufferedPaint: HPAINTBUFFER; out Rect: TRect): HRESULT; stdcall;

//------------------------------------------------------------------------
//  GetBufferedPaintTargetDC() - Returns the target DC specified during BeginBufferedPaint
//
//    hBufferedPaint           - handle to buffered paint context
//------------------------------------------------------------------------
  GetBufferedPaintTargetDC: function(hBufferedPaint: HPAINTBUFFER): HDC; stdcall;

//------------------------------------------------------------------------
//  GetBufferedPaintDC() - Returns the same paint DC returned by BeginBufferedPaint
//
//    hBufferedPaint     - handle to buffered paint context
//------------------------------------------------------------------------
  GetBufferedPaintDC: function(hBufferedPaint: HPAINTBUFFER): HDC; stdcall;

//------------------------------------------------------------------------
//  GetBufferedPaintBits() - Obtains a pointer to the buffer bitmap, if the buffer is a DIB
//
//    hBufferedPaint       - handle to buffered paint context
//    ppbBuffer            - pointer to receive pointer to buffer bitmap pixels
//    pcxRow               - pointer to receive width of buffer bitmap, in pixels;
//                           this value may not necessarily be equal to the buffer width
//------------------------------------------------------------------------
  GetBufferedPaintBits: function(hBufferedPaint: HPAINTBUFFER; out pbBuffer: PRGBQuad; out cxRow: Integer): HRESULT; stdcall;

//------------------------------------------------------------------------
//  BufferedPaintClear() - Clears given rectangle to ARGB = {0, 0, 0, 0}
//
//    hBufferedPaint     - handle to buffered paint context
//    prc                - rectangle to clear; NULL specifies entire buffer
//------------------------------------------------------------------------
  BufferedPaintClear: function(hBufferedPaint: HPAINTBUFFER; prc: PRect = nil): HRESULT; stdcall;

//------------------------------------------------------------------------
//  BufferedPaintSetAlpha() - Set alpha to given value in given rectangle
//
//    hBufferedPaint        - handle to buffered paint context
//    prc                   - rectangle to set alpha in; NULL specifies entire buffer
//    alpha                 - alpha value to set in the given rectangle
//------------------------------------------------------------------------
  BufferedPaintSetAlpha: function(hBufferedPaint: HPAINTBUFFER; prc: PRect = nil; alpha: Byte = $FF): HRESULT; stdcall;

//------------------------------------------------------------------------
//  BufferedPaintStopAllAnimations() - Stop all buffer animations for the given window
//
//    hwnd                           - window on which to stop all animations
//------------------------------------------------------------------------
  BufferedPaintStopAllAnimations: function(hwnd: HWND): HRESULT; stdcall;

type
  HANIMATIONBUFFER = THandle;  // handle to a buffered paint animation

var
  BeginBufferedAnimation: function(hwnd: HWND; hdcTarget: HDC; const rcTarget: TRect;
    dwFormat: TBPBufferFormat; pPaintParams: PBPPaintParams; const animationParams: TBPAnimationParams;
    out hdcFrom: HDC; out hdcTo: HDC): HANIMATIONBUFFER; stdcall;

  EndBufferedAnimation: function(hbpAnimation: HANIMATIONBUFFER; fUpdateTarget: BOOL): HRESULT; stdcall;

  BufferedPaintRenderAnimation: function(hwnd: HWND; hdcTarget: HDC): BOOL; stdcall;
    
//----------------------------------------------------------------------------
// Tells if the DWM is running, and composition effects are possible for this 
// process (themes are active). 
// Roughly equivalent to "DwmIsCompositionEnabled() && IsAppthemed()"
//----------------------------------------------------------------------------
  IsCompositionActive: function: BOOL; stdcall;

//------------------------------------------------------------------------
//  GetThemeTransitionDuration()
//                      - Gets the duration for the specified transition
//
//  hTheme              - theme data handle
//  iPartId             - part number 
//  iStateIdFrom        - starting state number of part
//  iStateIdTo          - ending state number of part
//  iPropId             - property id 
//  pdwDuration         - receives the transition duration
//------------------------------------------------------------------------
type
  HTHEME = THandle;

var
  GetThemeTransitionDuration: function(hTheme: HTHEME; iPartId, iStateIdFrom,
    iStateIdTo, iPropId: Integer; out dwDuration: DWORD): HRESULT; stdcall;

function LoadThemesEx: Boolean;
procedure UnloadThemesEx;
function IsThemesExLoaded: Boolean;

implementation

const
  UxThemeLib = 'UxTheme.dll';

var
  ThemeLib: HMODULE;

function LoadThemesEx: Boolean;
begin
  if ThemeLib = 0 then
    ThemeLib := LoadLibrary(UxThemeLib);
  if ThemeLib <> 0 then
  begin
    @BufferedPaintInit := GetProcAddress(ThemeLib, 'BufferedPaintInit');
    @BufferedPaintUnInit := GetProcAddress(ThemeLib, 'BufferedPaintUnInit');
    @BeginBufferedPaint := GetProcAddress(ThemeLib, 'BeginBufferedPaint');
    @EndBufferedPaint := GetProcAddress(ThemeLib, 'EndBufferedPaint');
    @GetBufferedPaintTargetRect := GetProcAddress(ThemeLib, 'GetBufferedPaintTargetRect');
    @GetBufferedPaintTargetDC := GetProcAddress(ThemeLib, 'GetBufferedPaintTargetDC');
    @GetBufferedPaintDC := GetProcAddress(ThemeLib, 'GetBufferedPaintDC');
    @GetBufferedPaintBits := GetProcAddress(ThemeLib, 'GetBufferedPaintBits');
    @BufferedPaintClear := GetProcAddress(ThemeLib, 'BufferedPaintClear');
    @BufferedPaintSetAlpha := GetProcAddress(ThemeLib, 'BufferedPaintSetAlpha');
    @BufferedPaintStopAllAnimations := GetProcAddress(ThemeLib, 'BufferedPaintStopAllAnimations');
    @BeginBufferedAnimation := GetProcAddress(ThemeLib, 'BeginBufferedAnimation');
    @EndBufferedAnimation := GetProcAddress(ThemeLib, 'EndBufferedAnimation');
    @BufferedPaintRenderAnimation := GetProcAddress(ThemeLib, 'BufferedPaintRenderAnimation');
    @IsCompositionActive := GetProcAddress(ThemeLib, 'IsCompositionActive');
    @GetThemeTransitionDuration := GetProcAddress(ThemeLib, 'GetThemeTransitionDuration');
  end;
  Result := (ThemeLib <> 0) and (@BufferedPaintInit <> nil);
end;

procedure UnloadThemesEx;
begin
  @BufferedPaintInit := nil;
  @BufferedPaintUnInit := nil;
  @BeginBufferedPaint := nil;
  @EndBufferedPaint := nil;
  @GetBufferedPaintTargetRect := nil;
  @GetBufferedPaintTargetDC := nil;
  @GetBufferedPaintDC := nil;
  @GetBufferedPaintBits := nil;
  @BufferedPaintClear := nil;
  @BufferedPaintSetAlpha := nil;
  @BufferedPaintStopAllAnimations := nil;
  @BeginBufferedAnimation := nil;
  @EndBufferedAnimation := nil;
  @BufferedPaintRenderAnimation := nil;
  @IsCompositionActive := nil;
  @GetThemeTransitionDuration := nil;
  if ThemeLib <> 0 then
    FreeLibrary(ThemeLib);
end;

function IsThemesExLoaded: Boolean;
begin
  Result := (ThemeLib <> 0) and (@BufferedPaintInit <> nil);
end;

end.
