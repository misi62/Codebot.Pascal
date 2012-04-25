
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit GraphTools;

interface

{$I CODEBOT.INC}

{$IFNDEF D10_UP}
	{$R MANIFEST.RES}
{$ENDIF}

uses
  Windows, Messages, ActiveX, SysUtils, Classes, Graphics, Math,
  GraphThemes, BaseTypes, GdiPlus, GdiIntf, MathTools, SysTools, ImageTools;

type
  TDirection = BaseTypes.TDirection;
  TDirections = BaseTypes.TDirections;
  TCorner = (cnTopLeft, cnTopRight, cnBottomRight, cnBottomLeft);
  TCorners = set of TCorner;

type
  TRectBounds = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

function RectBounds(Left, Top, Width, Height: Integer): TRectBounds; overload;
function RectBounds(const Rect: TRect): TRectBounds; overload;

type
	TRGB = BaseTypes.TRGB;
	PRGB = BaseTypes.PRGB;

	TRGBA = BaseTypes.TRGBA;
	PRGBA = BaseTypes.PRGBA;

	TRGBAFloat = BaseTypes.TRGBAFloat;
	PRGBAFloat = BaseTypes.PRGBAFloat;

function RGBColor(R, G, B: Byte): TColor;
function RGBAToColor(RGBA: TRGBA): TColor;
function HSLToColor(const HSL: THSL): TColor;
function HueToColor(Hue: Single): TColor;
function RGBAFToColor(RGBAF: TRGBAFloat): TColor;
function ColorToRGBA(Color: TColor): TRGBA;
function ColorToHSL(Color: TColor): THSL;
function ColorToRGBAF(Color: TColor): TRGBAFloat;

{ Color controls }

procedure DrawHueLinear(DC: HDC; Rect: TRect);
procedure DrawHueRadial(DC: HDC; Rect: TRect);
procedure DrawHueRing(DC: HDC; X, Y, InnerRadius, OuterRadius: Integer);
procedure DrawInverseRing(DC: HDC; X, Y, InnerRadius, OuterRadius: Integer);
procedure DrawSaturationBox(DC: HDC; Rect: TRect; Hue: Single);
procedure DrawDesaturationBox(DC: HDC; Rect: TRect; Hue: Single);
procedure DrawAlphaBox(DC: HDC; Rect: TRect; RGBA: TRGBA; CheckSize: Integer = 10);
procedure DrawAlphaColor(DC: HDC; Rect: TRect; RGBA: TRGBA; CheckSize: Integer = 10);

const
  NormalGraphicOpacity = $C0;
  DisabledGraphicOpacity = $7F;
  HotGraphicOpacity = $FF;

type
  TAlphaImage = class(TImageBitmap)
  private
    FDisabled: TAlphaImage;
    function GetDisabled: TAlphaImage;
    procedure SetDisabled(Value: TAlphaImage);
  public
    destructor Destroy; override;
    procedure BeginDraw;
    procedure EndDraw;
    procedure Saturate(Color: TColor);
    procedure Screen(Color: TColor);
    procedure Colorize(Color: TColor);
    procedure Fade(D: TDirection);
    procedure Grayscale;
    property Disabled: TAlphaImage read GetDisabled write SetDisabled;
  end;

{ The LayerBlit procedure represents paint program like layer blending
	see also http://www.pegtop.net/delphi/articles/blendmodes/ }

type
	TBlitMode = (bmNormal, bmAverage, bmScreen, bmMultiply, bmLighten, bmDarken,
  	bmDifference);

procedure LayerBlit(Dest: HDC; X, Y, Width, Height: Integer; Src: HDC;
	SrcX, SrcY: Integer; Mode: TBlitMode); overload;
procedure LayerBlit(Dest: HDC; X, Y: Integer; Mask: TFastBitmap;
	Mode: TBlitMode); overload;

type
	TResizeMode = (rmBicubic, rmBilinear, rmNearest);

procedure ResizeBitmap(var Bitmap: TBitmap; NewWidth, NewHeight: Integer; Mode: TResizeMode);

procedure RotateBitmap90DegreesCounterClockwise(var ABitmap: TBitmap);
procedure RotateBitmap90DegreesClockwise(var ABitmap: TBitmap);
procedure RotateBitmap180Degrees(var ABitmap: TBitmap);

procedure ColorPick(Enabled: Boolean);

const
  ILD_PRESSED = $0080;
	CN_COLORPICK = WM_USER + $FA0;

{ New color constants defined by Internet Explorer }

const
  clAliceBlue = TColor($FFF8F0);
  clAntiqueWhite = TColor($D7EBFA);
  clAquamarine = TColor($D4FF7F);
  clAzure = TColor($FFFFF0);
  clBeige = TColor($DCF5F5);
  clBisque = TColor($C4E4FF);
  clBlanchedAlmond = TColor($CDEBFF);
  clBlueViolet = TColor($E22B8A);
  clBrown = TColor($2A2AA5);
  clBurlywood = TColor($87B8DE);
  clCadetBlue = TColor($A09E5F);
  clChartreuse = TColor($00FF7F);
  clChocolate = TColor($1E69D2);
  clCoral = TColor($507FFF);
  clCornFlowerBlue = TColor($ED9564);
  clCornSilk = TColor($DCF8FF);
  clCrimson = TColor($3C14DC);
  clCyan = TColor($FFFF00);
  clDarkBlue = TColor($8B0000);
  clDarkCyan = TColor($8B8B00);
  clDarkGoldenrod = TColor($0B86B8);
  clDarkGray = TColor($A9A9A9);
  clDarkGreen = TColor($006400);
  clDarkKhaki = TColor($6BB7BD);
  clDarkMagenta = TColor($8B008B);
  clDarkOliveGreen = TColor($2F6B55);
  clDarkOrange = TColor($008CFF);
  clDarkOrchid = TColor($CC3299);
  clDarkRed = TColor($00008B);
  clDarkSalmon = TColor($7A96E9);
  clDarkseaGreen = TColor($8BBC8F);
  clDarkslateBlue = TColor($8B3D48);
  clDarkSlateGray = TColor($4F4F2F);
  clDarkTurquoise = TColor($D1CE00);
  clDarkViolet = TColor($D30094);
  clDeepPink = TColor($9314FF);
  clDeepSkyBlue = TColor($FFBF00);
  clDimGray = TColor($696969);
  clDodgerBlue = TColor($FF901E);
  clFireBrick = TColor($2222B2);
  clFloralWhite = TColor($F0FAFF);
  clForestGreen = TColor($228B22);
  clGainsboro = TColor($DCDCDC);
  clGhostWhite = TColor($FFF8F8);
  clGold = TColor($00D7FF);
  clGoldenrod = TColor($20A5DA);
  clGreenYellow = TColor($2FFFAD);
  clHoneydew = TColor($F0FFF0);
  clHotPink = TColor($B469FF);
  clIndianRed = TColor($5C5CCD);
  clIndigo = TColor($82004B);
  clIvory = TColor($F0FFFF);
  clKhaki = TColor($8CE6F0);
  clLavender = TColor($FAE6E6);
  clLavenderBlush = TColor($F5F0FF);
  clLawnGreen = TColor($00FC7C);
  clLemonChiffon = TColor($CDFAFF);
  clLightBlue = TColor($E6D8AD);
  clLightCoral = TColor($8080F0);
  clLightCyan = TColor($FFFFE0);
  clLightGoldenrodYellow = TColor($D2FAFA);
  clLightGreen = TColor($90EE90);
  clLightGray = TColor($D3D3D3);
  clLightPink = TColor($C1B6FF);
  clLightSalmon = TColor($7AA0FF);
  clLightSeaGreen = TColor($AAB220);
  clLightSkyBlue = TColor($FACE87);
  clLightSlategray = TColor($998877);
  clLightSteelBlue = TColor($DEC4B0);
  clLightYellow = TColor($E0FFFF);
  clLimeGreen = TColor($32CD32);
  clLinen = TColor($E6F0FA);
  clMagenta = TColor($FF00FF);
  clMediumAquamarine = TColor($AACD66);
  clMediumBlue = TColor($CD0000);
  clMediumOrchid = TColor($D355BA);
  clMediumPurple = TColor($DB7093);
  clMediumSeaGreen = TColor($71B33C);
  clMediumSlateBlue = TColor($EE687B);
  clMediumSpringGreen = TColor($9AFA00);
  clMediumTurquoise = TColor($CCD148);
  clMediumVioletRed = TColor($8515C7);
  clMidnightBlue = TColor($701919);
  clMintCream = TColor($FAFFF5);
  clMistyRose = TColor($E1E4FF);
  clMoccasin = TColor($B5E4FF);
  clNavajoWhite = TColor($ADDEFF);
  clOldLace = TColor($E6F5FD);
  clOliveDrab = TColor($238E6B);
  clOrange = TColor($00A5FF);
  clOrangeRed = TColor($0045FF);
  clOrchid = TColor($D670DA);
  clPaleGoldenrod = TColor($AAE8EE);
  clPaleGreen = TColor($98FB98);
  clPaleTurquoise = TColor($EEEEAF);
  clPalevioletRed = TColor($9370DB);
  clPapayawhip = TColor($D5EFFF);
  clPeachPuff = TColor($B9DAFF);
  clPeru = TColor($3F85CD);
  clPink = TColor($CBC0FF);
  clPlum = TColor($DDA0DD);
  clPowderBlue = TColor($E6E0B0);
  clRosyBrown = TColor($8F8FBC);
  clRoyalBlue = TColor($E16941);
  clSaddleBrown = TColor($13458B);
  clSalmon = TColor($7280FA);
  clSandyBrown = TColor($60A4F4);
  clSeaGreen = TColor($578B2E);
  clSeaShell = TColor($EEF5FF);
  clSienna = TColor($2D52A0);
  clSkyBlue = TColor($EBCE87);
  clSlateBlue = TColor($CD5A6A);
  clSlateGray = TColor($908070);
  clSnow = TColor($FAFAFF);
  clSpringGreen = TColor($7FFF00);
  clSteelBlue = TColor($B48246);
  clTan = TColor($8CB4D2);
  clThistle = TColor($D8BFD8);
  clTomato = TColor($4763FF);
  clTurquoise = TColor($D0E040);
  clViolet = TColor($EE82EE);
  clWheat = TColor($B3DEF5);
  clWhiteSmoke = TColor($F5F5F5);
  clYellowGreen = TColor($32CD9A);

  EnabledColors: array[Boolean] of TColor = (clBtnFace, clWindow);

function clThemeBorder: TColor;
function clThemeBkgnd: TColor;
function clSelected: TColor;
function clSelectedBorder: TColor;
function clSelectedBkgnd: TColor;
function clColumn: TColor;

{ Drawing and image states  }

type
  TDrawStateItem = BaseTypes.TDrawStateItem;
  TDrawState = BaseTypes.TDrawState;

  TImageState = (isNormal, isDisabled, isHot, isPressed);

function DirectionToAlignment(Direction: TDirection): TAlignment;
function AlignmentToDirection(Alignment: TAlignment): TDirection;

{ Context functions used to translate between pixel coordinates and device space
  coordinates }

function InitializeDevice(DC: HDC): HDC;
procedure FinalizeDevice(DC: HDC);

{ Device space coordinate translation routines }

function IntToDevice(Value: Integer; Angle: Integer = 0): Double;
function PointToDevice(const Point: TPoint): TFloatPoint; overload;
function PointToDevice(X, Y: Integer): TFloatPoint; overload;
function RectToDevice(const Rect: TRect): TFloatRect; overload;
function RectToDevice(ALeft, ATop, ARight, ABottom: Integer): TFloatRect; overload;
function PolygonToDevice(const Polygon: TPolygon): TFloatPolygon;
function DeviceToInt(const Value: Double; Angle: Integer = 0): Integer;
function DeviceToPoint(const Point: TFloatPoint): TPoint; overload;
function DeviceToPoint(const X, Y: Double): TPoint; overload;
function DeviceToRect(const Rect: TFloatRect): TRect; overload;
function DeviceToRect(const ALeft, ATop, ARight, ABottom: Double): TRect; overload;
function DeviceToPolygon(const Polygon: TFloatPolygon): TPolygon;

{ Decive compatible rect querying and transofrmation routines }

procedure OffsetRect(var Rect: TFloatRect; const X, Y: Double); overload;
procedure InflateRect(var Rect: TFloatRect; const X, Y: Double); overload;
function HeightOf(const Rect: TFloatRect): Double; overload;
function WidthOf(const Rect: TFloatRect): Double; overload;
procedure Slide(var Rect: TFloatRect; Direction: TDirection = drDown;
  Distance: Double = 0); overload;
function Chamfer(const Rect: TFloatRect; const Size: Double;
  Corners: TCorners): TFloatPolygon; overload;

procedure OffsetRect(var Rect: TRect; X, Y: Integer); overload;
procedure InflateRect(var Rect: TRect; X, Y: Integer); overload;
function CenterRect(const Rect: TRect; Size: Integer): TRect;
function HeightOf(const Rect: TRect): Integer; overload;
function WidthOf(const Rect: TRect): Integer; overload;
function MoveRect(const Rect: TRect; X, Y: Integer): TRect;
procedure Slide(var Rect: TRect; Direction: TDirection = drDown;
  Distance: Integer = 0); overload;

function GetTextAscent: Double;
function GetTextDescent: Double;
function GetTextBaseline: Double;

{ Color manipulation functions }

function Blend(ForeColor: TColor; BackColor: TColor; Percent: Byte = 50): TColor;
function Delta(Color: TColor; Value: Integer): TColor;
function Scale(Color: TColor; Value: Integer): TColor;

{ The TCalcRectEvent datatype is a generic method pointer that delegates
  the resizing of the Rect parameter to external objects }

type
  TNotifyIndexEvent = procedure(Sender: TObject; Index: Integer) of object;
  TDrawRectEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect) of object;
  TDrawStateEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect;
    State: TDrawState) of object;
  TDrawIndexEvent = procedure(Sender: TObject; Canvas: TCanvas; Index: Integer;
    Rect: TRect; State: TDrawState) of object;
  TMeasureIndexEvent = procedure(Sender: TObject; Canvas: TCanvas; Index: Integer;
    out Size: Integer) of object;
  TDrawIndexDefaultEvent = procedure(Sender: TObject; Canvas: TCanvas; Index: Integer;
    Rect: TRect; State: TDrawState; var DefaultDraw: Boolean) of object;
  TDrawStage = (dsPreProcess, dsProcessing, dsPostProcess);
  TDrawStageEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect;
    Stage: TDrawStage) of object;
  TCalcRectEvent = procedure(Sender: TObject; var Rect: TRect) of object;

function GetBorder: Integer;
function GetTextColor(Background: TColor): TColor;

{ These get object functions all return dynamically allocated GDI objects. It is
  the caller's responsibility to manage the lifetime of these objects. Summary
  of the function are as follows:

    The GetBitmap function returns an eight pixel by eight pixel TBitmap object
    with alternating colors specified by the ForeColor and BackColor parameters }

type
	TBitmapBrush = (bbChecker, bbHalflight);

function GetBitmap(ForeColor: TColor; BackColor: TColor): TBitmap; overload;
function GetBitmap(Resource: Integer): TBitmap; overload;
function GetBrush(Bitmap: TBitmap): HBRUSH; overload;
function GetBrush(Bitmap: TFastBitmap): HBRUSH; overload;
function GetBrush(Bitmap: TBitmapBrush): HBRUSH; overload;
function GetBrush(C1, C2: TColor; Size: Integer = 1): HBRUSH; overload;
function GetBrush(Color: TColor; Style: TBrushStyle = bsSolid): HBRUSH; overload;
function GetPen(Color: TColor; Width: Integer = 0; Square: Boolean = False): HPEN; overload;
function GetPen(Color: TColor; Style: TPenStyle; Width: Integer = 1): HPEN; overload;
function GetRegion(const Polygon: TPolygon): HRGN; overload;
function GetRegion(DC: HDC): HRGN; overload;
function GetFont(const Name: string; Size: Integer; Italic: Boolean = False;
  Underlined: Boolean = False; Weight: Integer = 0;  Charset: TFontCharset = 0): HFONT; overload;
function GetFontDirect(Font: HFONT; Style: TFontStyles): HFONT; overload;
function GetFont(DC: HDC; Style: TFontStyles): HFONT; overload;
function GetFont(DC: HDC; Style: TFontStyles; Size: Integer): HFONT; overload;
function SelectFontStyle(DC: HDC; Style: TFontStyles): HFONT; overload;
function SelectFontStyle(DC: HDC; Style: TFontStyles; Size: Integer): HFONT; overload;
function GetFontBold(const Name: string; Size: Integer): HFONT;

{ FillRect routines }

procedure FillRectChecker(DC: HDC; const Rect: TRect; C1, C2: TColor; CheckerSize: Integer = 10); overload;
procedure FillRectChecker(DC: HDC; const Rect: TRect; CheckerSize: Integer = 10); overload;
procedure FillRectColor(DC: HDC; const Rect: TRect; Color: TColor);
procedure FillRectColorAlpha(DC: HDC; Rect: TRect; Color: TColor; Alpha: Byte);
procedure FillRectColorPixel(DC: HDC; Rect: TRect; Pixel: TFastBitmap);
procedure FillRectOutline(DC: HDC; const Rect: TRect; Color: TColor; Style: TPenStyle = psSolid);
procedure FillRectFrame(DC: HDC; const Rect: TRect);
procedure FillRoundRect(DC: HDC; const Rect: TRect; Radius: Integer); overload;
procedure FillRoundRect(DC: HDC; const Rect: TRect; Radius: Integer;
  Pen, Brush: TColor); overload;
procedure FillRoundRect(DC: HDC; const Rect: TRect; Radius: Integer;
  Pen: TColor); overload;
procedure FillRoundRectFancy(DC: HDC; const Rect: TRect; Radius: Integer;
  GradStart, GradEnd: TColor; GradDir: TDirection; Pen, Brush: TColor;
  PenStyle: TPenStyle; BrushStyle: TBrushStyle);

{ The SwapFont proceure }

procedure SwapFont(A, B: TFont);

{ The OverwriteObject helper routine selects and deletes GDI objects from a
  device context in a single call }

procedure OverwriteObject(DC: HDC; Obj: HGDIOBJ);

{ The SelectRectPath procedure selects a clipping rectangle into a device
  context use the mode passed in the Mode parameter }

procedure SelectClipRect(DC: HDC; const Rect: TRect; Mode: Integer);

{ The FontRatio procedure Calcs the ratio of screen fonts to design fonts }

procedure FontRatio(var X, Y: Double); overload;
procedure FontRatio(Font: HFont; var X, Y: Double); overload;

{ Non-themed drawing routines }

{ Helper draw format flags }

const
  DR_FORMAT = DT_SINGLELINE or DT_END_ELLIPSIS;
  DR_LEFT = DR_FORMAT or DT_VCENTER or DT_LEFT;
  DR_TOP = DR_FORMAT or DT_TOP or DT_LEFT;
  DR_RIGHT = DR_FORMAT or DT_VCENTER or DT_RIGHT;
  DR_BOTTOM = DR_FORMAT or DT_BOTTOM or DT_LEFT;
  DR_CENTER = DR_FORMAT or DT_VCENTER or DT_CENTER;
  DR_FILL = DT_SINGLELINE or DT_VCENTER or DT_CENTER or DT_NOCLIP;
  DR_WRAP = DT_TOP or DT_WORDBREAK or DT_END_ELLIPSIS or DT_LEFT;
  DR_WRAP_CENTER = DT_VCENTER or DT_CENTER or DT_WORDBREAK or DT_END_ELLIPSIS;

{ Element sizes }

  NodeSize = 16;

{ The DrawArrow procedure draws a small triangle centered in a rectangle
  pointing in the direction specified by the Direction parameter }

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
  ForeColor, BackColor: TColor; Size: Integer = 0; Enabled: Boolean = True); overload;

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
	Color: TColor = clWindowFrame; Enabled: Boolean = True); overload;

{ The DrawBox procedure }

procedure DrawBox(DC: HDC; Color: TColor; var Rect: TRect);

{ The DrawCheckBox procedure }

procedure DrawCheckBox(DC: HDC; Rect: TRect; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);

{ The DrawCheckText procedure }

procedure DrawCheckText(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Checked: Boolean; ForeColor, BackColor: TColor;
  Flat: Boolean = True);

{ The DrawSortArrow procedure draws an etched arrow similar to the ones
  found in an explorer in detail view }

procedure DrawSortArrow(DC: HDC; Rect: TRect; Direction: TDirection);

{ The DrawSeparator procedure }

procedure DrawSeparator(DC: HDC; Rect: TRect; Color: TColor; Horizontal: Boolean = True);

{ The DrawStyleRect procedure }

procedure DrawRoundRect(DC: HDC; Rect: TRect; Color: TColor; Hollow: Boolean = False);
procedure DrawStyleRoundText(DC: HDC; Rect: TRect; const Text: string; Color: TColor; Hollow: Boolean = False);
procedure DrawStyleOutline(DC: HDC; Rect: TRect; Light: Boolean = False);
procedure DrawStyleRect(DC: HDC; Rect: TRect; Light: Boolean = False);
procedure DrawMenuHighlightRect(DC: HDC; Rect: TRect);
procedure DrawFancyMenuHighlightRect(DC: HDC; Rect: TRect);
procedure DrawStyleRoundRect(DC: HDC; Rect: TRect; Light: Boolean = False; Hollow: Boolean = False);
procedure DrawStyleRectOutline(DC: HDC; Rect: TRect; Light: Boolean = False);
procedure DrawStyleRoundRectText(DC: HDC; Rect: TRect; const Text: string; Light: Boolean = False; Hollow: Boolean = False);
procedure FillRectState(DC: HDC; Rect: TRect; State: TDrawState; Background: TColor = clWindow);

procedure DrawRectState(DC: HDC; Rect: TRect; Color: TColor; State: TDrawState);
procedure DrawTextState(DC: HDC; const Text: string; Rect: TRect; Color: TColor; State: TDrawState);

{ The CalcCaptionRect function returns the Calcd rectangle which exactly
  bounds the text given in the Caption parameter. See also DrawCaption }

const
  Directions: array[TDirection] of Integer = (DR_LEFT or DT_VCENTER, DR_TOP,
    DR_RIGHT or DT_VCENTER, DR_BOTTOM, DR_CENTER, DR_FILL, DR_WRAP, DR_WRAP or DT_CENTER); //);

function CalcCaptionRect(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection): TRect;

{ The CalcCaptionSize function }

function CalcCaptionSize(DC: HDC; const Text: string): TSize;

{ The FontHeight function }

function FontHeight(DC: HDC): Integer; overload;
function FontHeight(Font: TFont): Integer; overload;

function FontWidth(DC: HDC; const S: string): Integer; overload;
function FontWidth(Font: TFont; const S: string): Integer; overload;

function FontSize(DC: HDC; const S: string): TSize; overload;
function FontSize(Font: TFont; const S: string): TSize; overload;

{ The CalcMemoHeight function }

function CalcMemoHeight(DC: HDC; const Text: string; Width: Integer): Integer;

{ The DrawCaption procedure draws a vertically centered line of text truncated
  to fit in a rectangle, Directioned according to its Direction parameter }

procedure DrawCaption(DC: HDC;  const Caption: string; Rect: TRect;
  Direction: TDirection; Enabled: Boolean = True; HidePrefix: Boolean = False);

{ The DrawEditText procedure draws a text vertically centered as if it was
  inside an edit control. The correct left/right offest of Rect should be 3 }

procedure DrawEditText(DC: HDC; const Text: string; const Rect: TRect; Direction: TDirection);

{ Draws a focus rect correcting for text color }

procedure DrawFocus(DC: HDC; Rect: TRect; BorderX: Integer = 0; BorderY: Integer = 0);

{ The DrawClose procedure draws a close symbol in a device context centered
  about the Rect parameter	}

procedure DrawClose(DC: HDC; Rect: TRect; Color: TColor = clWindowFrame);

{ The DrawDivider procedure draws a beveled line in a device context }

type
  TDrawDividerKind = (ddHorz, ddVert);

procedure DrawDivider(DC: HDC; Rect: TRect; Kind: TDrawDividerKind);

{ The DrawEllipsis procedure }

procedure DrawEllipsis(DC: HDC; Rect: TRect; Enabled: Boolean = True);

{ The DrawFrame procedure draws border inside a rectangle using the following
  states:

    dfFocus:   A thin single pixel line simulating a window border.

    dfFramed:  A framed border two pixels thick that simulates the look of a
               three dimensional button.
    dfHover:   A thin single pixel line border that appears as a control border
               when drawn against an application workspace background.
    dfRaised:  A thin single pixel line border that has a three dimensional
               appearance of being raised.
    dfFlat:    No border drawn at all. Provided as a convience to handle a
               request to draw nothing.
    dfLowered: A thin single line border that has the three dimensional
               appearance of being sunken.
    dfPressed: A thin single pixel inverted border for items that appear as
               controls when drawn against an application workspace background.
    dfSunken:  The sunken border of a client edge window with a three
               dimensional look.
    dfPushed:  An inverted framed border two pixels thick that simluates the
               look of a Pushed button. }

type
  TDrawFrameState = (dfFocus, dfFramed, dfHover, dfRaised, dfFlat,
  	dfLowered, dfSunken, dfPressed, dfPushed);

procedure DrawFrame(DC: HDC; Rect: TRect; State: TDrawFrameState);

{ The DrawGradient procedure fills the Rect parameter with a series of colored
  bands that range in gradient value from StartColor to EndColor. The number of
  color bands is specified by the Colors parameter, and the direction of the
  gradiant fill is determined by the Direction parameter  }

procedure DrawGradient(DC: HDC; Rect: TRect; StartColor, EndColor: TColor;
  Direction: TDirection);

procedure DrawGradientSplit(DC: HDC; Rect: TRect; C1, C2, C3, C4: TColor;
  Direction: TDirection);

{ Draws a nice gradient information box }

procedure DrawInfoBox(DC: HDC; const Rect: TRect);

{ The DrawGrip procedure draws a size grip image }

procedure DrawGrip(DC: HDC; Rect: TRect; Clipped: Boolean = True);

{ The DrawNode procedure draws a visual representation of a tree node. The
  default SolidColor parameter paints the node using system colors }

procedure DrawNode(DC: HDC; Rect: TRect; Expanded: Boolean; SolidColor: TColor = cl3DDkShadow);

{ The DrawToolGrip procedure draws a grip on dockable windows }

procedure DrawToolGrip(DC: HDC; const Rect: TRect);

{ The DrawPolygon procedure is a simple wrapper around the Polygon GDI function }

procedure DrawPolygon(DC: HDC; const P: TPolygon); overload;
procedure DrawPolygon(DC: HDC; const P: TFloatPolygon); overload;

{ The DrawAngledText procedure }

procedure DrawAngledText(DC: HDC; const S: string; const Angle: Double;
  const Point: TPoint);

{ The DrawRect procedure }

procedure DrawRect(DC: HDC; const Rect: TRect; Color: TColor);

{ The DrawRectOutline procedure }

procedure DrawRectOutline(DC: HDC; const Rect: TRect; Color: TColor);

{ The DrawRectEdge procedure }

procedure DrawRectEdge(DC: HDC; const Rect: TRect; Color: TColor; Direction: TDirection);

{ The DrawBorder procedure create a standard themed 3D sunken window border }

procedure DrawBorder(DC: HDC; const Rect: TRect);

{ The DrawDashedRect procedure }

procedure DrawDashedRect(DC: HDC; const Rect: TRect; Pen, Brush: TColor);

{ The DrawSlantRect procedure }

procedure DrawSlantRect(DC: HDC; const Rect: TRect; const A, B: TPoint);

{ The DrawSlantText procedure }

procedure DrawSlantText(DC: HDC; const S: string; const A, B: TPoint);

{ The DrawBevelRect procedure }

procedure DrawBevelRect(DC: HDC; Rect: TRect; Bevel: Integer; Color: TColor);

{ The DrawBubble procedure }

procedure DrawBubble(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; ForeColor, BackColor: TColor);

{ The DrawCapsule procedures }

type
  TCapsuleProc = procedure(DC: HDC; Rect: TRect; ForeColor, BackColor: TColor;
    Data: Pointer);

{ The DrawCapsuleAdvanced procedure }

procedure DrawCapsuleAdvanced(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer;
  CapsuleProc: TCapsuleProc; ForeColor, BackColor: TColor; Data: Pointer);

{ The DrawCapsuleText procedure }

procedure DrawCapsuleText(DC: HDC; const Caption, Body: string; Rect: TRect;
  CaptionDirection, BodyDirection: TDirection; Bevel: Integer; Width: Integer;
  ForeColor, BackColor: TColor);


{ The DrawCapsuleCheckBox procedure }

procedure DrawCapsuleCheckBox(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);

{ GDI element plus drawing routines }

{$IFDEF GDIPLUS}
procedure DrawToolbar(DC: HDC; Rect: TRect);
procedure DrawMidbar(DC: HDC; const Rect: TRect);
procedure DrawStatusbar(DC: HDC; const Rect: TRect);
procedure DrawButtonContainer(DC: HDC; const Rect: TRect; Radius: Single);
{$ENDIF}

{ Glyphs }

procedure BlitInvert(DC: HDC; const Rect: TRect);
procedure BlitAnd(Dest: HDC; const Rect: TRect; Source: HDC);
procedure BlitOr(Dest: HDC; const Rect: TRect; Source: HDC);

type
	TGlyphKind = (gkClose, gkPin, gkPinPushed, gkChevronUp, gkChevronDown,
  	gkArrowLeft, gkArrowRight, gkArrowUp, gkArrowDown,
    gkArrowLeftDisabled, gkArrowRightDisabled, gkArrowUpDisabled,
    gkArrowDownDisabled, gkEllipse, gkQuestion, gkVSpin, gkHSpin);

function GlyphFind(Kind: TGlyphKind): TBitmap;
procedure GlyphBlendBlt(DC: HDC; Glyph: TGlyphKind; X, Y: Integer; Color: TColor); overload;
procedure GlyphBlendBlt(DC: HDC; Glyph: TBitmap; X, Y: Integer; Color: TColor); overload;
procedure GlyphDraw(DC: HDC; const Rect: TRect; Glyph: TGlyphKind; Color: TColor); overload;
procedure GlyphFrame(DC: HDC; Rect: TRect; Glyph: TGlyphKind; State: TDrawState; Color: TColor);

{ These are all elements which can be themed. }

type
  TThemedElement = (
    teButton,
    teClock,
    teComboBox,
    teEdit,
    teExplorerBar,
    teHeader,
    teListView,
    teMenu,
    tePage,
    teProgress,
    teRebar,
    teScrollBar,
    teSpin,
    teStartPanel,
    teStatus,
    teTab,
    teTaskBand,
    teTaskBar,
    teToolBar,
    teToolTip,
    teTrackBar,
    teTrayNotify,
    teTreeView,
    teWindow
  );

{ The root part of each element is sometimes used for special painting and does
	not belong to a certain state. }

{ Button }

  TThemedButton = (
    tbButtonDontCare,
    tbButtonRoot,
    tbPushButtonNormal, tbPushButtonHot, tbPushButtonPressed, tbPushButtonDisabled, tbPushButtonDefaulted,
    tbRadioButtonUncheckedNormal, tbRadioButtonUncheckedHot, tbRadioButtonUncheckedPressed, tbRadioButtonUncheckedDisabled,
    tbRadioButtonCheckedNormal, tbRadioButtonCheckedHot, tbRadioButtonCheckedPressed, tbRadioButtonCheckedDisabled,
    tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled,
    tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot, tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled,
    tbCheckBoxMixedNormal, tbCheckBoxMixedHot, tbCheckBoxMixedPressed, tbCheckBoxMixedDisabled,
    tbGroupBoxNormal, tbGroupBoxDisabled,
    tbUserButton
  );

{ Clock }

  TThemedClock = (
    tcClockDontCare,
    tcClockRoot,
    tcTimeNormal
  );

{ ComboBox }

  TThemedComboBox = (
    tcComboBoxDontCare,
    tcComboBoxRoot,
    tcDropDownButtonNormal, tcDropDownButtonHot, tcDropDownButtonPressed, tcDropDownButtonDisabled,
    { new with vista }
    tcBackground,
    tcTransparentBackgroundNormal, tcTransparentBackgroundHot, tcTransparentBackgroundDisabled, tcTransparentBackgroundFocused,
    tcBorderNormal, tcBorderHot, tcBorderFocused, tcBorderDisabled,
    tcReadOnlyNormal, tcReadOnlyHot, tcReadOnlyPressed, tcReadOnlyDisabled,
    tcDropDownButtonRightNormal, tcDropDownButtonRightHot, tcDropDownButtonRightPressed, tcDropDownButtonRightDisabled,
    tcDropDownButtonLeftNormal, tcDropDownButtonLeftHot, tcDropDownButtonLeftPressed, tcDropDownButtonLeftDisabled,
    tcCueBannerNormal, tcCueBannerHot, tcCueBannerPressed, tcCueBannerDisabled
  );

{ Edit }

  TThemedEdit = (
    teEditDontCare,
    teEditRoot,
    teEditTextNormal, teEditTextHot, teEditTextSelected, teEditTextDisabled, teEditTextFocused, teEditTextReadOnly, teEditTextAssist,
    teEditCaret,
    { new with vista }
    teBackgroundNormal, teBackgroundHot, teBackgroundDisabled, teBackgroundFocused, teBackgroundReadOnly, teBackgroundAssist,
    tePassword,
    teBackgroundWithBorderNormal, teBackgroundWithBorderHot, teBackgroundWithBorderDisabled, teBackgroundWithBorderFocused,
    teBorderNoScrollNormal, teBorderNoScrollHot, teBorderNoScrollFocused, teBorderNoScrollDisabled,
    teBorderHScrollNormal, teBorderHScrollHot, teBorderHScrollFocused, teBorderHScrollDisabled,
    teBorderVScrollNormal, teBorderVScrollHot, teBorderVScrollFocused, teBorderVScrollDisabled,
    teBorderHVScrollNormal, teBorderHVScrollHot, teBorderHVScrollFocused, teBorderHVScrollDisabled
  );

{ ExplorerBar }

  TThemedExplorerBar = (
    tebExplorerBarDontCare,
    tebExplorerBarRoot,
    tebHeaderBackgroundNormal, tebHeaderBackgroundHot, tebHeaderBackgroundPressed,
    tebHeaderCloseNormal, tebHeaderCloseHot, tebHeaderClosePressed,
    tebHeaderPinNormal, tebHeaderPinHot, tebHeaderPinPressed,
    tebHeaderPinSelectedNormal, tebHeaderPinSelectedHot, tebHeaderPinSelectedPressed,
    tebIEBarMenuNormal, tebIEBarMenuHot, tebIEBarMenuPressed,
    tebNormalGroupBackground,
    tebNormalGroupCollapseNormal, tebNormalGroupCollapseHot, tebNormalGroupCollapsePressed,
    tebNormalGroupExpandNormal, tebNormalGroupExpandHot, tebNormalGroupExpandPressed,
    tebNormalGroupHead,
    tebSpecialGroupBackground,
    tebSpecialGroupCollapseSpecial, tebSpecialGroupCollapseHot, tebSpecialGroupCollapsePressed,
    tebSpecialGroupExpandSpecial, tebSpecialGroupExpandHot, tebSpecialGroupExpandPressed,
    tebSpecialGroupHead
  );

{ Header }

  TThemedHeader = (
    thHeaderDontCare,
    thHeaderRoot,
    thHeaderItemNormal, thHeaderItemHot, thHeaderItemPressed,
    thHeaderItemLeftNormal, thHeaderItemLeftHot, thHeaderItemLeftPressed,
    thHeaderItemRightNormal, thHeaderItemRightHot, thHeaderItemRightPressed,
    thHeaderSortArrowSortedUp, thHeaderSortArrowSortedDown
  );

{ ListView }

  TThemedListview = (
    tlListviewDontCare,
    tlListviewRoot,
    tlListItemNormal, tlListItemHot, tlListItemSelected, tlListItemDisabled, tlListItemSelectedNotFocus,
    tlListGroup,
    tlListDetail,
    tlListSortDetail,
    tlEmptyText
  );

{ Menu }

  TThemedMenu = (
    tmMenuDontCare,
    tmMenuRoot,
    tmMenuItemNormal, tmMenuItemSelected, tmMenuItemDemoted,
    tmMenuDropDown,
    tmMenuBarItem,
    tmMenuBarDropDown,
    tmChevron,
    tmSeparator
  );

{ Page }

  TThemedPage = (
    tpPageDontCare,
    tpPageRoot,
    tpUpNormal, tpUpHot, tpUpPressed, tpUpDisabled,
    tpDownNormal, tpDownHot, tpDownPressed, tpDownDisabled,
    tpUpHorzNormal, tpUpHorzHot, tpUpHorzPressed, tpUpHorzDisabled,
    tpDownHorzNormal, tpDownHorzHot, tpDownHorzPressed, tpDownHorzDisabled
  );

{ Progress }

  TThemedProgress = (
    tpProgressDontCare,
    tpProgressRoot,
    tpBar,
    tpBarVert,
    tpChunk,
    tpChunkVert
  );

{ Rebar }

  TThemedRebar = (
    trRebarDontCare,
    trRebarRoot,
    trGripper,
    trGripperVert,
    trBandNormal, trBandHot, trBandPressed, trBandDisabled, trBandChecked, trBandHotChecked,
    trChevronNormal, trChevronHot, trChevronPressed, trChevronDisabled,
    trChevronVertNormal, trChevronVertHot, trChevronVertPressed, trChevronVertDisabled
  );

{ ScrollBar }

  TThemedScrollBar = (
    tsScrollBarDontCare,
    tsScrollBarRoot,
    tsArrowBtnUpNormal, tsArrowBtnUpHot, tsArrowBtnUpPressed, tsArrowBtnUpDisabled,
    tsArrowBtnDownNormal, tsArrowBtnDownHot, tsArrowBtnDownPressed, tsArrowBtnDownDisabled,
    tsArrowBtnLeftNormal, tsArrowBtnLeftHot, tsArrowBtnLeftPressed, tsArrowBtnLeftDisabled,
    tsArrowBtnRightNormal, tsArrowBtnRightHot, tsArrowBtnRightPressed, tsArrowBtnRightDisabled,
    tsThumbBtnHorzNormal, tsThumbBtnHorzHot, tsThumbBtnHorzPressed, tsThumbBtnHorzDisabled,
    tsThumbBtnVertNormal, tsThumbBtnVertHot, tsThumbBtnVertPressed, tsThumbBtnVertDisabled,
    tsLowerTrackHorzNormal, tsLowerTrackHorzHot, tsLowerTrackHorzPressed, tsLowerTrackHorzDisabled,
    tsUpperTrackHorzNormal, tsUpperTrackHorzHot, tsUpperTrackHorzPressed, tsUpperTrackHorzDisabled,
    tsLowerTrackVertNormal, tsLowerTrackVertHot, tsLowerTrackVertPressed, tsLowerTrackVertDisabled,
    tsUpperTrackVertNormal, tsUpperTrackVertHot, tsUpperTrackVertPressed, tsUpperTrackVertDisabled,
    tsGripperHorzNormal, tsGripperHorzHot, tsGripperHorzPressed, tsGripperHorzDisabled,
    tsGripperVertNormal, tsGripperVertHot, tsGripperVertPressed, tsGripperVertDisabled,
    tsSizeBoxRightAlign, tsSizeBoxLeftAlign
  );

{ Spin }

  TThemedSpin = (
    tsSpinDontCare,
    tsSpinRoot,
    tsUpNormal, tsUpHot, tsUpPressed, tsUpDisabled,
    tsDownNormal, tsDownHot, tsDownPressed, tsDownDisabled,
    tsUpHorzNormal, tsUpHorzHot, tsUpHorzPressed, tsUpHorzDisabled,
    tsDownHorzNormal, tsDownHorzHot, tsDownHorzPressed, tsDownHorzDisabled
  );

{ StartPanel }

  TThemedStartPanel = (
    tspStartPanelDontCare,
    tspStartPanelRoot,
    tspUserPane,
    tspMorePrograms,
    tspMoreProgramsArrowNormal, tspMoreProgramsArrowHot, tspMoreProgramsArrowPressed,
    tspProgList,
    tspProgListSeparator,
    tspPlacesList,
    tspPlacesListSeparator,
    tspLogOff,
    tspLogOffButtonsNormal, tspLogOffButtonsHot, tspLogOffButtonsPressed,
    tspUserPicture,
    tspPreview
  );

{ Status }

  TThemedStatus = (
    tsStatusDontCare,
    tsStatusRoot,
    tsPane,
    tsGripperPane,
    tsGripper
  );

{ Tab }

  TThemedTab = (
    ttTabDontCare,
    ttTabRoot,
    ttTabItemNormal, ttTabItemHot, ttTabItemSelected, ttTabItemDisabled, ttTabItemFocused,
    ttTabItemLeftEdgeNormal, ttTabItemLeftEdgeHot, ttTabItemLeftEdgeSelected, ttTabItemLeftEdgeDisabled, ttTabItemLeftEdgeFocused,
    ttTabItemRightEdgeNormal, ttTabItemRightEdgeHot, ttTabItemRightEdgeSelected, ttTabItemRightEdgeDisabled, ttTabItemRightEdgeFocused,
    ttTabItemBothEdgeNormal, ttTabItemBothEdgeHot, ttTabItemBothEdgeSelected, ttTabItemBothEdgeDisabled, ttTabItemBothEdgeFocused,
    ttTopTabItemNormal, ttTopTabItemHot, ttTopTabItemSelected, ttTopTabItemDisabled, ttTopTabItemFocused,
    ttTopTabItemLeftEdgeNormal, ttTopTabItemLeftEdgeHot, ttTopTabItemLeftEdgeSelected, ttTopTabItemLeftEdgeDisabled, ttTopTabItemLeftEdgeFocused,
    ttTopTabItemRightEdgeNormal, ttTopTabItemRightEdgeHot, ttTopTabItemRightEdgeSelected, ttTopTabItemRightEdgeDisabled, ttTopTabItemRightEdgeFocused,
    ttTopTabItemBothEdgeNormal, ttTopTabItemBothEdgeHot, ttTopTabItemBothEdgeSelected, ttTopTabItemBothEdgeDisabled, ttTopTabItemBothEdgeFocused,
    ttPane,
    ttBody
  );

{ TaskBand }

  TThemedTaskBand = (
    ttbTaskBandDontCare,
    ttbTaskBandRoot,
    ttbGroupCount,
    ttbFlashButton,
    ttpFlashButtonGroupMenu
  );

{ TaskBar }

  TThemedTaskBar = (
    ttTaskBarDontCare,
    ttTaskBarRoot,
    ttbTimeNormal
  );

{ ToolBar }

  TThemedToolBar = (
    ttbToolBarDontCare,
    ttbToolBarRoot,
    ttbButtonNormal, ttbButtonHot, ttbButtonPressed, ttbButtonDisabled, ttbButtonChecked, ttbButtonCheckedHot,
    ttbDropDownButtonNormal, ttbDropDownButtonHot, ttbDropDownButtonPressed, ttbDropDownButtonDisabled, ttbDropDownButtonChecked, ttbDropDownButtonCheckedHot,
    ttbSplitButtonNormal, ttbSplitButtonHot, ttbSplitButtonPressed, ttbSplitButtonDisabled, ttbSplitButtonChecked, ttbSplitButtonCheckedHot,
    ttbSplitButtonDropDownNormal, ttbSplitButtonDropDownHot, ttbSplitButtonDropDownPressed, ttbSplitButtonDropDownDisabled, ttbSplitButtonDropDownChecked, ttbSplitButtonDropDownCheckedHot,
    ttbSeparatorNormal, ttbSeparatorHot, ttbSeparatorPressed, ttbSeparatorDisabled, ttbSeparatorChecked, ttbSeparatorCheckedHot,
    ttbSeparatorVertNormal, ttbSeparatorVertHot, ttbSeparatorVertPressed, ttbSeparatorVertDisabled, ttbSeparatorVertChecked, ttbSeparatorVertCheckedHot
  );

{ ToolTip }

  TThemedToolTip = (
    tttToolTipDontCare,
    tttToolTipRoot,
    tttStandardNormal, tttStandardLink,
    tttStandardTitleNormal, tttStandardTitleLink,
    tttBaloonNormal, tttBaloonLink,
    tttBaloonTitleNormal, tttBaloonTitleLink,
    tttCloseNormal, tttCloseHot, tttClosePressed
  );

{ TrackBar }

  TThemedTrackBar = (
    ttbTrackBarDontCare,
    ttbTrackBarRoot,
    ttbTrack,
    ttbTrackVert,
    ttbThumbNormal, ttbThumbHot, ttbThumbPressed, ttbThumbFocused, ttbThumbDisabled,
    ttbThumbBottomNormal, ttbThumbBottomHot, ttbThumbBottomPressed, ttbThumbBottomFocused, ttbThumbBottomDisabled,
    ttbThumbTopNormal, ttbThumbTopHot, ttbThumbTopPressed, ttbThumbTopFocused, ttbThumbTopDisabled,
    ttbThumbVertNormal, ttbThumbVertHot, ttbThumbVertPressed, ttbThumbVertFocused, ttbThumbVertDisabled,
    ttbThumbLeftNormal, ttbThumbLeftHot, ttbThumbLeftPressed, ttbThumbLeftFocused, ttbThumbLeftDisabled,
    ttbThumbRightNormal, ttbThumbRightHot, ttbThumbRightPressed, ttbThumbRightFocused, ttbThumbRightDisabled,
    ttbThumbTics,
    ttbThumbTicsVert
  );

{ TrayNotify }

  TThemedTrayNotify = (
    ttnTrayNotifyDontCare,
    ttnTrayNotifyRoot,
    ttnBackground,
    ttnAnimBackground
  );

{ TreeView }

  TThemedTreeView = (
    ttTreeViewDontCare,
    ttTreeViewRoot,
    ttItemNormal, ttItemHot, ttItemSelected, ttItemDisabled, ttItemSelectedNotFocus,
    ttGlyphClosed, ttGlyphOpened,
    ttBranch
  );

{ Window }

  TThemedWindow = (
    twWindowDontCare,
    twWindowRoot,
    twCaptionActive, twCaptionInactive, twCaptionDisabled,
    twSmallCaptionActive, twSmallCaptionInactive, twSmallCaptionDisabled,
    twMinCaptionActive, twMinCaptionInactive, twMinCaptionDisabled,
    twSmallMinCaptionActive, twSmallMinCaptionInactive, twSmallMinCaptionDisabled,
    twMaxCaptionActive, twMaxCaptionInactive, twMaxCaptionDisabled,
    twSmallMaxCaptionActive, twSmallMaxCaptionInactive, twSmallMaxCaptionDisabled,

    twFrameLeftActive, twFrameLeftInactive,
    twFrameRightActive, twFrameRightInactive,
    twFrameBottomActive, twFrameBottomInactive,
    twSmallFrameLeftActive, twSmallFrameLeftInactive,
    twSmallFrameRightActive, twSmallFrameRightInactive,
    twSmallFrameBottomActive, twSmallFrameBottomInactive,

    twSysButtonNormal, twSysButtonHot, twSysButtonPushed, twSysButtonDisabled,
    twMDISysButtonNormal, twMDISysButtonHot, twMDISysButtonPushed, twMDISysButtonDisabled,
    twMinButtonNormal, twMinButtonHot, twMinButtonPushed, twMinButtonDisabled,
    twMDIMinButtonNormal, twMDIMinButtonHot, twMDIMinButtonPushed, twMDIMinButtonDisabled,
    twMaxButtonNormal, twMaxButtonHot, twMaxButtonPushed, twMaxButtonDisabled,
    twCloseButtonNormal, twCloseButtonHot, twCloseButtonPushed, twCloseButtonDisabled,
    twSmallCloseButtonNormal, twSmallCloseButtonHot, twSmallCloseButtonPushed, twSmallCloseButtonDisabled,
    twMDICloseButtonNormal, twMDICloseButtonHot, twMDICloseButtonPushed, twMDICloseButtonDisabled,
    twRestoreButtonNormal, twRestoreButtonHot, twRestoreButtonPushed, twRestoreButtonDisabled,
    twMDIRestoreButtonNormal, twMDIRestoreButtonHot, twMDIRestoreButtonPushed, twMDIRestoreButtonDisabled,
    twHelpButtonNormal, twHelpButtonHot, twHelpButtonPushed, twHelpButtonDisabled,
    twMDIHelpButtonNormal, twMDIHelpButtonHot, twMDIHelpButtonPushed, twMDIHelpButtonDisabled,

    twHorzScrollNormal, twHorzScrollHot, twHorzScrollPushed, twHorzScrollDisabled,
    twHorzThumbNormal, twHorzThumbHot, twHorzThumbPushed, twHorzThumbDisabled,
    twVertScrollNormal, twVertScrollHot, twVertScrollPushed, twVertScrollDisabled,
    twVertThumbNormal, twVertThumbHot, twVertThumbPushed, twVertThumbDisabled,

    twDialog,
    twCaptionSizingTemplate,
    twSmallCaptionSizingTemplate,
    twFrameLeftSizingTemplate,
    twSmallFrameLeftSizingTemplate,
    twFrameRightSizingTemplate,
    twSmallFrameRightSizingTemplate,
    twFrameBottomSizingTemplate,
    twSmallFrameBottomSizingTemplate
  );

  TThemeData = array[TThemedElement] of HTHEME;

  PThemedDetails = ^TThemedDetails;
  TThemedDetails = record
    Element: TThemedElement;
    Part: Integer;
    State: Integer;
  end;

	TThemeSize = THEMESIZE;

const
  tsMin = TS_MIN;
  tsTrue = TS_TRUE;
  tsDraw = TS_DRAW;

  ThemeDataNames: array[TThemedElement] of PWideChar = (
    'button',      // teButton
    'clock',       // teClock
    'combobox',    // teComboBox
    'edit',        // teEdit
    'explorerbar', // teExplorerBar
    'header',      // teHeader
    'listview',    // teListView
    'menu',        // teMenu
    'page',        // tePage
    'progress',    // teProgress
    'rebar',       // teRebar
    'scrollbar',   // teScrollBar
    'spin',        // teSpin
    'startpanel',  // teStartPanel
    'status',      // teStatus
    'tab',         // teTab
    'taskband',    // teTaskBand
    'taskbar',     // teTaskBar
    'toolbar',     // teToolBar
    'tooltip',     // teToolTip
    'trackbar',    // teTrackBar
    'traynotify',  // teTrayNotify
    'TreeView',    // teTreeView
    'window'       // teWindow
  );


{ TThemePainter  is a small foot print class to provide the user with pure
	Windows XP theme related abilities like	painting elements and text or
  retrieving certain info. }

type
  TThemePainter = class(TObject)
  private
    FAvailable: Boolean;
    FControlsEnabled: Boolean;
    FWindow: TUtilityWindow;
    FThemeData: TThemeData;
    FOnThemeChange: TNotifyEvent;
    function GetTheme(Element: TThemedElement): HTHEME;
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
  protected
    procedure DoOnThemeChange; virtual;
    procedure UnloadThemeData;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDetails(Widget: TThemedButton): TThemedDetails; overload;
    function GetDetails(Widget: TThemedClock): TThemedDetails; overload;
    function GetDetails(Widget: TThemedComboBox): TThemedDetails; overload;
    function GetDetails(Widget: TThemedEdit): TThemedDetails; overload;
    function GetDetails(Widget: TThemedExplorerBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedHeader): TThemedDetails; overload;
    function GetDetails(Widget: TThemedListView): TThemedDetails; overload;
    function GetDetails(Widget: TThemedMenu): TThemedDetails; overload;
    function GetDetails(Widget: TThemedPage): TThemedDetails; overload;
    function GetDetails(Widget: TThemedProgress): TThemedDetails; overload;
    function GetDetails(Widget: TThemedRebar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedScrollBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedSpin): TThemedDetails; overload;
    function GetDetails(Widget: TThemedStartPanel): TThemedDetails; overload;
    function GetDetails(Widget: TThemedStatus): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTab): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTaskBand): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTaskBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedToolBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedToolTip): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTrackBar): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTrayNotify): TThemedDetails; overload;
    function GetDetails(Widget: TThemedTreeView): TThemedDetails; overload;
    function GetDetails(Widget: TThemedWindow): TThemedDetails; overload;
    function ColorToRGB(Color: TColor; Details: PThemedDetails = nil): COLORREF;
    function ContentRect(DC: HDC; const Details: TThemedDetails; BoundingRect: TRect): TRect;
    procedure DrawEdge(DC: HDC; const Details: TThemedDetails; const R: TRect; Edge, Flags: Cardinal;
      ContentRect: PRect = nil);
    procedure DrawElement(DC: HDC; const Details: TThemedDetails; const R: TRect; ClipRect: PRect = nil);
    procedure DrawIcon(DC: HDC; const Details: TThemedDetails; const R: TRect; himl: HIMAGELIST; Index: Integer);
    procedure DrawParentBackground(Window: HWND; Target: HDC; Details: PThemedDetails; OnlyIfTransparent: Boolean;
      Bounds: PRect = nil);
    procedure DrawText(DC: HDC; const Details: TThemedDetails;
    	const S: WideString; R: TRect; Flags: Cardinal);
    function HasTransparentParts(Details: TThemedDetails): Boolean;
    function PartSize(const Details: TThemedDetails; var Rect: TRect; ThemeSize: TThemeSize = tsDraw): TSize; overload;
    function PartSize(const Details: TThemedDetails; ThemeSize: TThemeSize = tsDraw): TSize; overload;
    procedure UpdateThemes;
    property Theme[Element: TThemedElement]: HTHEME read GetTheme;
    property Available: Boolean read FAvailable;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OnThemeChange: TNotifyEvent read FOnThemeChange write FOnThemeChange;
  end;

function ThemePainter: TThemePainter;

function GetThemeBorder: Integer;

{ Themed drawing routines }

function CalcEditHeight(DC: HDC): Integer; overload;
function CalcEditHeight(Font: TFont): Integer; overload;
function CalcComboButtonRect(DC: HDC; Rect: TRect; State: TDrawState): TRect;

procedure DrawThemeComboBox(DC: HDC; Rect: TRect; State: TDrawState);
procedure DrawThemeComboButton(DC: HDC; Rect: TRect; State: TDrawState);

procedure DrawThemeBorder(DC: HDC; const Rect: TRect; State: TDrawState;
	Thickness: Integer = -1; X: Integer = 0; Y: Integer = 0);
procedure DrawThemeBorderInflate(DC: HDC; var Rect: TRect; State: TDrawState);
procedure DrawThemeEdit(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeButton(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeButtonFocus(DC: HDC; const Rect: TRect);
procedure DrawThemeThinButton(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeHeader(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeNode(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeCheckBox(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeClose(DC: HDC; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame);
procedure DrawThemeToolClose(DC: HDC; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame);
procedure DrawThemePin(DC: HDC; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame);
procedure DrawThemeArrow(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState;
  Color: TColor = clWindowFrame; Adjust: Integer = 0);
procedure DrawThemeScroll(DC: HDC; const Rect: TRect; State: TDrawState);
// procedure DrawThemeScroll(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState);
procedure DrawFlatScroll(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState);
procedure DrawThemeGrip(DC: HDC; const Rect: TRect);
procedure	DrawThemeGripper(DC: HDC; Rect: TRect; Color: TColor; Horizontal: Boolean = True);
procedure DrawThemeGroupBox(DC: HDC; const Text: string; const Rect: TRect; State: TDrawState);
procedure DrawThemeExpandableBox(DC: HDC; const Text: string; const Rect: TRect;	State: TDrawState);
procedure DrawThemeHorzThumb(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeHorzSplit(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeVertThumb(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeVertSplit(DC: HDC; const Rect: TRect; State: TDrawState);
procedure DrawThemeCloseBox(DC: HDC; const Text: string; const Rect: TRect; State: TDrawState);
procedure DrawThemeStatus(DC: HDC; const Text: string; const Rect: TRect);
procedure DrawThemeBar(DC: HDC; Rect: TRect; Background: TColor);
procedure DrawThemeSeperator(DC: HDC; Rect: TRect; Background: TColor;
	Themed: Boolean = True; Transparent: Boolean = False; Flat: Boolean = False); overload;
procedure DrawThemeSeperator(DC: HDC; const Rect: TRect; ForeGround,
	Background: TColor); overload;
procedure DrawThemeDesigner(DC: HDC; const Rect: TRect; GripSize: Integer;
	FixedWidth, FixedHeight: Boolean; State: TDrawState);

{ Alpha blending suport routines }

function CreateBlendSection(DC: HDC; Width: Integer; Height: Integer): HBITMAP;

function BlendImages(Source1, Source2, Dest: HBITMAP; Weight: Byte): Boolean;

procedure DrawBlend(Dest: HDC; DestX, DestY, DestWidth,
  DestHeight: Integer; Source: HDC; SourceX, SourceY, SourceWidth,
  SourceHeight: Integer; SourceWeight: Byte);

{ Resource conversion routines }

function HexToStream(const HexData: string): TStream;
function StreamToHex(Stream: TStream): string;
procedure LoadGraphicResource(Graphic: TGraphic; Ident: Integer);

type
  TCaptionPosition = (cpLeft, cpTop, cpRight, cpBottom, cpHidden);

  IIgnoreResize = interface
    ['{991310E3-945F-44F7-AED1-C0747048D9EE}']
  end;

  IIgnoreMargin = interface
    ['{E393ED7A-1D47-41E7-9E04-9D0F1083FCCF}']
  end;

{ Glass image routines }

	IGlassImage = interface
  	['{98ADB4B7-83BB-4B77-A8F5-F2A5B102981B}']
    function GetDC: HDC;
    function GetGraphic: TGraphic;
    function GetPixelDepth: TPixelDepth;
    function GetScanline(Row: Integer): Pointer;
    function GetStride: Integer;
    property DC: HDC read GetDC;
		property Graphic: TGraphic read GetGraphic;
    property PixelDepth: TPixelDepth read GetPixelDepth;
    property Scanline[Row: Integer]: Pointer read GetScanline;
    property Stride: Integer read GetStride;
  end;

	INativeDraw = interface(IGlassImage)
		['{381653CF-19EA-43F7-83E3-CDED1D5554BF}']
    procedure DrawAlpha(const Rect: TRect; DC: HDC; X, Y: Integer;
      Opacity: Byte = $FF; Grayscale: Boolean = False);
    procedure DrawState(const Rect: TRect; DC: HDC; X, Y: Integer;
      State: TDrawState = []);
    procedure DrawColor(const Rect: TRect; DC: HDC; X, Y: Integer;
      Color: TColor; Opacity: Byte = $FF);
	end;

function CreateGlassImage(Stream: TStream = nil): IGlassImage;

type
  IIndexedImages = interface
    ['{234EC9C4-2B47-4556-B22D-E71C4B262213}']
    function GetSize: Integer;
    procedure DrawImage(Canvas: TCanvas; X, Y, Index: Integer;
      State: TDrawState = []);
    property Size: Integer read GetSize;
  end;

implementation

{$R GLYPHS.RES}

uses
	Consts;

function RectBounds(Left, Top, Width, Height: Integer): TRectBounds;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Width := Width;
  Result.Height := Height;
end;

function RectBounds(const Rect: TRect): TRectBounds;
begin
  Result.Left := Rect.Left;
  Result.Top := Rect.Top;
  Result.Width := Rect.Right - Rect.Left;
  Result.Height := Rect.Bottom - Rect.Top;
end;

function RGBColor(R, G, B: Byte): TColor;
var
  RGBA: TRGBA absolute Result;
begin
  RGBA.Red := B;
  RGBA.Green:= G;
  RGBA.Blue := R;
	RGBA.Alpha := 0;
end;

function RGBAToColor(RGBA: TRGBA): TColor;
begin
	RGBA.Alpha := RGBA.Blue;
  RGBA.Blue := RGBA.Red;
  RGBA.Red := RGBA.Alpha;
  RGBA.Alpha := 0;
  Result := TColor(RGBA);
end;

function HSLToColor(const HSL: THSL): TColor;
begin
  Result := RGBAToColor(HSLToRGBA(HSL));
end;

function HueToColor(Hue: Single): TColor;
begin
  Result := RGBAToColor(HSLToRGBA(HSL(Hue, 1, 0.5)));
end;

function RGBAFToColor(RGBAF: TRGBAFloat): TColor;
var
  RGBA: TRGBA;
begin
  RGBA.Red := Round(RGBAF.Red * $FF);
  RGBA.Green := Round(RGBAF.Green * $FF);
  RGBA.Blue := Round(RGBAF.Blue * $FF);
  Result := RGBAToColor(RGBA);
end;

function ColorToRGBA(Color: TColor): TRGBA;
begin
  Result := TRGBA(ColorToRGB(Color));
	Result.Alpha := Result.Blue;
  Result.Blue := Result.Red;
  Result.Red := Result.Alpha;
  Result.Alpha := 0;
end;

function ColorToHSL(Color: TColor): THSL;
begin
  Result := RGBAToHSL(ColorToRGBA(Color));
end;

function ColorToRGBAF(Color: TColor): TRGBAFloat;
var
  RGBA: TRGBA;
begin
  RGBA := ColorToRGBA(Color);
  Result.Red := RGBA.Red / $FF;
  Result.Green := RGBA.Green / $FF;
  Result.Blue := RGBA.Blue / $FF;
  Result.Alpha := 0;
end;

procedure DrawHueLinear(DC: HDC; Rect: TRect);
var
  P: HPen;
  C1, C2: TRGBA;
  W, I, J: Integer;
begin
  W := WidthOf(Rect);
  I := 0;
  C1.Alpha := 0;
  C2 := C1;
  P := SelectObject(DC, GetPen(0));
  for J := 1 to W do
  begin
    C1 := HSLtoRGBA(HSL(I / W, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(DC, I, Rect.Top, nil);
    LineTo(DC, I, Rect.Bottom);
    Inc(I);
  end;
  OverwriteObject(DC, P);
end;

procedure DrawHueRadial(DC: HDC; Rect: TRect);
var
  P: HPen;
  C1, C2: TRGBA;
  W, H, D, X, Y, I, J: Integer;
begin
  W := WidthOf(Rect);
  H := HeightOf(Rect);
  D := W + W + H + H;
  X := Rect.Left + W div 2;
  Y := Rect.Top + H div 2;
  I := 0;
  C1.Alpha := 0;
  C2 := C1;
  P := SelectObject(DC, GetPen(0));
  for J := 1 to W do
  begin
    C1 := HSLtoRGBA(HSL(I / D, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(DC, I, Rect.Top, nil);
    LineTo(DC, X, Y);
    Inc(I);
  end;
  for J := 1 to H do
  begin
    C1 := HSLtoRGBA(HSL(I / D, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(DC, Rect.Right - 1, Rect.Top + J, nil);
    LineTo(DC, X, Y);
    Inc(I);
  end;
  for J := 1 to W do
  begin
    C1 := HSLtoRGBA(HSL(I / D, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(DC, Rect.Right - J, Rect.Bottom - 1, nil);
    LineTo(DC, X, Y);
    Inc(I);
  end;
  for J := 1 to H do
  begin
    C1 := HSLtoRGBA(HSL(I / D, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(DC, Rect.Left, Rect.Bottom - J, nil);
    LineTo(DC, X, Y);
    Inc(I);
  end;
  OverwriteObject(DC, P);
end;

procedure DrawHueRing(DC: HDC; X, Y, InnerRadius, OuterRadius: Integer);
var
  B: TFastBitmap;
  P: HPen;
  R: PRGBA;
  D: Single;
  C1, C2: TRGBA;
  C, I, J: Integer;
begin
  B := CreateFastBitmap(OuterRadius * 2 + 2, OuterRadius * 2 + 2, pd32);
  C := B.Width div 2;
  C1.Alpha := 0;
  C2 := C1;
  P := SelectObject(B.DC, GetPen(0));
  for I := 0 to B.Width - 1 do
  begin
    C1 := HSLtoRGBA(HSL(VertexAngle(Vertex(I - C, -C)) / 360, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(B.DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(B.DC, I, 0, nil);
    LineTo(B.DC, C, C);
  end;
  for I := 0 to B.Height - 1 do
  begin
    C1 := HSLtoRGBA(HSL(VertexAngle(Vertex(C, I - C)) / 360, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(B.DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(B.DC, B.Width - 1, I, nil);
    LineTo(B.DC, C, C);
  end;
  for I := 0 to B.Width - 1 do
  begin
    C1 := HSLtoRGBA(HSL(VertexAngle(Vertex(C - I, C)) / 360, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(B.DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(B.DC, B.Width - I - 1, B.Height - 1, nil);
    LineTo(B.DC, C, C);
  end;
  for I := 0 to B.Height - 1 do
  begin
    C1 := HSLtoRGBA(HSL(VertexAngle(Vertex(-C, C - I)) / 360, 1, 0.5));
    if Longword(C1) <> Longword(C2) then
      OverwriteObject(B.DC, GetPen(RGBAToColor(C1)));
    C2 := C1;
    MoveToEx(B.DC, 0, B.Height - I - 1, nil);
    LineTo(B.DC, C, C);
  end;
  OverwriteObject(B.DC, P);
  R := B.Bits;
  for I := 0 to B.Width - 1 do
    for J := 0 to B.Height - 1 do
    begin
      D := Sqrt((I - C) * (I - C) + (J - C) * (J - C));
      if D < InnerRadius - 1 then
        PLongword(R)^ := 0
      else if D < InnerRadius then
      begin
        D := InnerRadius - D;
        D := 1 - D;
        R.Red := Round(D * R.Red);
        R.Green := Round(D * R.Green);
        R.Blue := Round(D * R.Blue);
        R.Alpha := Round(D * $FF);
      end
      else if D < OuterRadius then
        R.Alpha := $FF
      else if D < OuterRadius + 1 then
      begin
        D := (D - OuterRadius);
        D := 1 - D;
        R.Red := Round(D * R.Red);
        R.Green := Round(D * R.Green);
        R.Blue := Round(D * R.Blue);
        R.Alpha := Round(D * $FF);
      end
      else
        PLongword(R)^ := 0;
      Inc(R);
    end;
  BaseTypes.AlphaDraw(DC, X - C, Y - C, B);
  DestroyFastBitmap(B);
end;

procedure DrawInverseRing(DC: HDC; X, Y, InnerRadius, OuterRadius: Integer);
var
  B: TFastBitmap;
  P: PRGBA;
  C: Integer;
  D: Single;
  I, J: Integer;
begin
  B := CreateFastBitmap(OuterRadius * 2 + 2, OuterRadius * 2 + 2, pd32);
  C := B.Width div 2;
  BitBlt(B.DC, 0, 0, B.Width, B.Height, DC, X - C, Y - C, SRCCOPY);
  P := B.Bits;
  for I := 0 to B.Width - 1 do
    for J := 0 to B.Height - 1 do
    begin
      D := Sqrt((I - C) * (I - C) + (J - C) * (J - C));
      if D < InnerRadius - 1 then
        D := 0
      else if D < InnerRadius then
      begin
        D := InnerRadius - D;
        D := 1 - D;
      end
      else if D < OuterRadius then
        D := 1
      else if D < OuterRadius + 1 then
      begin
        D := (D - OuterRadius);
        D := 1 - D;
      end
      else
        D := 0;
      if D > 0 then
      begin
        P.Red := $FF - P.Red;
        P.Green :=  $FF - P.Green;
        P.Blue :=  $FF - P.Blue;
      end;
      if D = 0 then
        PLongword(P)^ := 0
      else if D = 1 then
        P.Alpha := $FF
      else
      begin
        { D := Sin(D * (Pi / 2)); More natural? }
        P.Red := Round(D * P.Red);
        P.Green := Round(D * P.Green);
        P.Blue := Round(D * P.Blue);
        P.Alpha := Round(D * $FF);
      end;
      Inc(P);
    end;
  BaseTypes.AlphaDraw(DC, X - C, Y - C, B);
  DestroyFastBitmap(B);
end;

procedure DrawSaturationBox(DC: HDC; Rect: TRect; Hue: Single);
var
  B: TFastBitmap;
  P: PRGBA;
  X, Y, W, H: Integer;
begin
  W := WidthOf(Rect);
  H := HeightOf(Rect);
  if (W < 1) or (H < 1) then Exit;
  B := CreateFastBitmap(W, H, pd32);
  P := B.Bits;
  for Y := 1 to H do
    for X := 1 to W do
    begin
      P^ := HSLtoRGBA(HSL(Hue, X / W, 1 - Y / H));
      Inc(P);
    end;
  BaseTypes.AlphaDraw(DC, Rect.Left, Rect.Top, B);
  DestroyFastBitmap(B);
end;

procedure DrawDesaturationBox(DC: HDC; Rect: TRect; Hue: Single);
var
  B: TFastBitmap;
  //C, D: TRGBA;
  P: PRGBA;
  X, Y, W, H: Integer;
  A1: Single;
  HSL: THSL;
begin
  W := WidthOf(Rect);
  H := HeightOf(Rect);
  if (W < 1) or (H < 1) then Exit;
  B := CreateFastBitmap(W, H, pd32);
  P := B.Bits;
  for Y := 1 to H do
    for X := 1 to W do
    begin
      HSL.Hue := Hue;
      HSL.Saturation := X / W;
      A1 := Y / H;
      HSL.Lightness := (A1 * (1 - HSL.Saturation) + A1) / 2;
      P^ := HSLtoRGBA(HSL);
      Inc(P);
    end;
  BaseTypes.AlphaDraw(DC, Rect.Left, Rect.Top, B);
  DestroyFastBitmap(B);
end;

procedure DrawAlphaBox(DC: HDC; Rect: TRect; RGBA: TRGBA; CheckSize: Integer = 10);
var
  B: TFastBitmap;
  C: HBRUSH;
  R: TRGBA;
  P: PRGBA;
  V: Single;
  X, Y, W, H: Integer;
begin
  W := WidthOf(Rect);
  H := HeightOf(Rect);
  if (W < 1) or (H < 1) then Exit;
  C := GetBrush(clWhite, clSilver, CheckSize);
  FillRect(DC, Rect, C);
  DeleteObject(C);
  B := CreateFastBitmap(W, H, pd32);
  P := B.Bits;
  for Y := 1 to H do
    for X := 1 to W do
    begin
      V := X / W;
      R := RGBA;
      R.Red := Round(V * R.Red);
      R.Green := Round(V * R.Green);
      R.Blue := Round(V * R.Blue);
      R.Alpha := Round(V * $FF);
      P^ := R;
      Inc(P);
    end;
  BaseTypes.AlphaDraw(DC, Rect.Left, Rect.Top, B);
  DestroyFastBitmap(B);
end;

procedure DrawAlphaColor(DC: HDC; Rect: TRect; RGBA: TRGBA; CheckSize: Integer = 10);
var
  B: HBRUSH;
  P: TFastBitmap;
begin
  B := GetBrush(clWhite, clSilver, CheckSize);
  FillRect(DC, Rect, B);
  DeleteObject(B);
  P := CreatePixel(RGBA);
  AlphaDrawRect(DC, Rect, P, GetRect(1, 1));
  DestroyFastBitmap(P);
end;

{ TAlphaImage }

destructor TAlphaImage.Destroy;
begin
  FDisabled.Free;
  inherited Destroy;
end;

function TAlphaImage.GetDisabled: TAlphaImage;
begin
  Result := FDisabled;
  if Empty then Exit;
  if Result = nil then
  begin
    FDisabled := TAlphaImage.Create;
    FDisabled.Assign(Self);
    FDisabled.Grayscale;
    FDisabled.Opacity := DisabledGraphicOpacity;
    Result := FDisabled
  end;
end;

procedure TAlphaImage.SetDisabled(Value: TAlphaImage);
begin
  if Value = nil then
  begin
    FDisabled.Free;
    FDisabled := nil;
  end
  else
  begin
    if FDisabled = nil then
      FDisabled := TAlphaImage.Create;
    FDisabled.Assign(Value);
  end;
end;

{procedure TAlphaImage.BorderBlit(DC: HDC; const Rect: TRect; Border: Integer);
begin
  BorderBlit(DC, Rect, Classes.Rect(Border, Border, Border, Border));
end;}

const
  MaskDrawColor: TRGBA = (Blue: 1; Green: 2; Red: 3; Alpha: 0);
  MaskDrawBlank: TRGBA = (Blue: 0; Green: 0; Red: 0; Alpha: 0);

procedure TAlphaImage.BeginDraw;
begin
  if Empty then Exit;
  HandleNeeded(False);
  FillRectColor(Bitmap.DC, Bounds, RGBAToColor(MaskDrawColor));
end;

procedure TAlphaImage.EndDraw;
var
  B: TFastBitmap;
  RGBA: PRGBA;
  I: Integer;
begin
  if Empty then Exit;
  HandleNeeded(False);
  B := Bitmap;
  RGBA := B.Bits;
  for I := 0 to B.Width * B.Height - 1 do
  begin
    if DWORD(RGBA^) = DWORD(MaskDrawColor) then
      RGBA^ := MaskDrawBlank
    else
      RGBA.Alpha := $FF;
    Inc(RGBA);
  end;
end;

procedure TAlphaImage.Grayscale;
begin
  ImageGrayscale(Self);
end;

procedure TAlphaImage.Saturate(Color: TColor);
begin
  ImageSaturate(Self, Color);
end;

procedure TAlphaImage.Screen(Color: TColor);
begin
  ImageScreen(Self, Color);
end;

procedure TAlphaImage.Colorize(Color: TColor);
begin
  ImageColorize(Self, Color);
end;

procedure TAlphaImage.Fade(D: TDirection);
begin
  ImageFade(Self, D);
end;

{ LayerBlit }

procedure LayerBlit(Dest: HDC; X, Y, Width, Height: Integer; Src: HDC;
	SrcX, SrcY: Integer; Mode: TBlitMode);
var
	A, B: TFastBitmap;
  Col, Row: Integer;
  BitsA, BitsB: PRGB;
begin
	if Mode = bmNormal then
  begin
  	BitBlt(Dest, X, Y, Width, Height, Src, SrcX, SrcY, SRCCOPY);
    Exit;
	end;
	A := CreateFastBitmap(Width, Height);
  B := CreateFastBitmap(Width, Height);
  try
  	BitBlt(A.DC, 0, 0, Width, Height, Dest, X, Y, SRCCOPY);
  	BitBlt(B.DC, 0, 0, Width, Height, Src, SrcX, SrcY, SRCCOPY);
    BitsA := A.Bits;
    BitsB := B.Bits;
    case Mode of
    	bmAverage:
				for Col := 0 to Width - 1 do
		    	for Row := 0 to Height - 1 do
		      begin
		      	BitsA.Blue := (BitsA.Blue + BitsB.Blue) shr 1;
		      	BitsA.Green := (BitsA.Green + BitsB.Green) shr 1;
		      	BitsA.Red := (BitsA.Red + BitsB.Red) shr 1;
						Inc(BitsA);
						Inc(BitsB);
		      end;
			bmScreen:
				for Col := 0 to Width - 1 do
		    	for Row := 0 to Height - 1 do
		      begin
		      	BitsA.Blue := $FF - (($FF - BitsA.Blue) * ($FF - BitsB.Blue) shr 8);
		      	BitsA.Green := $FF - (($FF - BitsA.Green) * ($FF - BitsB.Green) shr 8);
		      	BitsA.Red := $FF - (($FF - BitsA.Red) * ($FF - BitsB.Red) shr 8);
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmMultiply:
				for Col := 0 to Width - 1 do
		    	for Row := 0 to Height - 1 do
		      begin
		      	BitsA.Blue := (BitsA.Blue * BitsB.Blue) shr 8;
		      	BitsA.Green := (BitsA.Green * BitsB.Green) shr 8;
		      	BitsA.Red := (BitsA.Red * BitsB.Red) shr 8;
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmLighten:
				for Col := 0 to Width - 1 do
		    	for Row := 0 to Height - 1 do
		      begin
		      	if BitsB.Blue > BitsA.Blue then BitsA.Blue := BitsB.Blue;
		      	if BitsB.Green > BitsA.Green then BitsA.Green := BitsB.Green;
		      	if BitsB.Red > BitsA.Red then BitsA.Red := BitsB.Red;
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmDarken:
				for Col := 0 to Width - 1 do
		    	for Row := 0 to Height - 1 do
		      begin
		      	if BitsB.Blue < BitsA.Blue then BitsA.Blue := BitsB.Blue;
		      	if BitsB.Green < BitsA.Green then BitsA.Green := BitsB.Green;
		      	if BitsB.Red < BitsA.Red then BitsA.Red := BitsB.Red;
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmDifference:
				for Col := 0 to Width - 1 do
		    	for Row := 0 to Height - 1 do
		      begin
		      	BitsA.Blue := $FF - Abs($FF - BitsA.Blue - BitsB.Blue);
		      	BitsA.Green := $FF - Abs($FF - BitsA.Green - BitsB.Green);
		      	BitsA.Red := $FF - Abs($FF - BitsA.Red - BitsB.Red);
						Inc(BitsA);
						Inc(BitsB);
		      end;
		end;
  	BitBlt(Dest, X, Y, Width, Height, A.DC, 0, 0, SRCCOPY);
  finally
  	DestroyFastBitmap(B);
  	DestroyFastBitmap(A);
  end;
end;

procedure LayerBlit(Dest: HDC; X, Y: Integer; Mask: TFastBitmap;
	Mode: TBlitMode);
var
	A, B: TFastBitmap;
  Col, Row: Integer;
  BitsA, BitsB: PRGB;
begin
	if Mode = bmNormal then
  begin
  	BitBlt(Dest, X, Y, Mask.Width, Mask.Height, Mask.DC, 0, 0, SRCCOPY);
    Exit;
	end;
	A := CreateFastBitmap(Mask.Width, Mask.Height);
  try
  	BitBlt(A.DC, 0, 0, Mask.Width, Mask.Height, Dest, X, Y, SRCCOPY);
    BitsA := A.Bits;
	  B := Mask;
    BitsB := B.Bits;
    case	Mode of
    	bmAverage:
				for Col := 0 to Mask.Width - 1 do
		    	for Row := 0 to Mask.Height - 1 do
		      begin
		      	BitsA.Blue := (BitsA.Blue + BitsB.Blue) shr 1;
		      	BitsA.Green := (BitsA.Green + BitsB.Green) shr 1;
		      	BitsA.Red := (BitsA.Red + BitsB.Red) shr 1;
						Inc(BitsA);
						Inc(BitsB);
		      end;
			bmScreen:
				for Col := 0 to Mask.Width - 1 do
		    	for Row := 0 to Mask.Height - 1 do
		      begin
		      	BitsA.Blue := $FF - (($FF - BitsA.Blue) * ($FF - BitsB.Blue) shr 8);
		      	BitsA.Green := $FF - (($FF - BitsA.Green) * ($FF - BitsB.Green) shr 8);
		      	BitsA.Red := $FF - (($FF - BitsA.Red) * ($FF - BitsB.Red) shr 8);
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmMultiply:
				for Col := 0 to Mask.Width - 1 do
		    	for Row := 0 to Mask.Height - 1 do
		      begin
		      	BitsA.Blue := (BitsA.Blue * BitsB.Blue) shr 8;
		      	BitsA.Green := (BitsA.Green * BitsB.Green) shr 8;
		      	BitsA.Red := (BitsA.Red * BitsB.Red) shr 8;
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmLighten:
				for Col := 0 to Mask.Width - 1 do
		    	for Row := 0 to Mask.Height - 1 do
		      begin
		      	if BitsB.Blue > BitsA.Blue then BitsA.Blue := BitsB.Blue;
		      	if BitsB.Green > BitsA.Green then BitsA.Green := BitsB.Green;
		      	if BitsB.Red > BitsA.Red then BitsA.Red := BitsB.Red;
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmDarken:
				for Col := 0 to Mask.Width - 1 do
		    	for Row := 0 to Mask.Height - 1 do
		      begin
		      	if BitsB.Blue < BitsA.Blue then BitsA.Blue := BitsB.Blue;
		      	if BitsB.Green < BitsA.Green then BitsA.Green := BitsB.Green;
		      	if BitsB.Red < BitsA.Red then BitsA.Red := BitsB.Red;
						Inc(BitsA);
						Inc(BitsB);
		      end;
    	bmDifference:
				for Col := 0 to Mask.Width - 1 do
		    	for Row := 0 to Mask.Height - 1 do
		      begin
		      	BitsA.Blue := $FF - Abs($FF - BitsA.Blue - BitsB.Blue);
		      	BitsA.Green := $FF - Abs($FF - BitsA.Green - BitsB.Green);
		      	BitsA.Red := $FF - Abs($FF - BitsA.Red - BitsB.Red);
						Inc(BitsA);
						Inc(BitsB);
		      end;
		end;
  	BitBlt(Dest, X, Y, Mask.Width, Mask.Height, A.DC, 0, 0, SRCCOPY);
  finally
  	DestroyFastBitmap(A);
  end;
end;

const
	MaxPixelCount = 32768;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..MaxPixelCount - 1] of TRGBTriple;

function SinC(X: Double): Double;
begin
  if Abs(X) < 1 then
    Result := 1 - 2 * X * X + X * X * Abs(X)
  else if (Abs(X) >= 1) and (Abs(X) < 2) then
    Result := 4 - 8 * Abs(X) + 5 * X * X - X * X * Abs(X)
  else
  	Result := 0;
end;

procedure Bicubic(I1, I2, I3, I4: TRGBTriple; var New: TRGBTriple; U: Double);
var
  I: Integer;
begin
  I := Trunc(I1.rgbtRed * SinC(U + 1) + I2.rgbtRed
    * SinC(U) + I3.rgbtRed * SinC(U - 1) + I4.rgbtRed * SinC(U - 2));
  if I > 255 then
  	I := 255
	else if I < 0 then
  	I := 0;
  New.rgbtRed := Byte(I);
  I := Trunc(I1.rgbtGreen * SinC(U + 1) + I2.rgbtGreen
    * SinC(U) + I3.rgbtGreen * SinC(U - 1) + I4.rgbtGreen * SinC(U - 2));
  if I > 255 then
  	I := 255
	else if I < 0 then
  	I := 0;
  New.rgbtGreen := Byte(I);
  I := Trunc(I1.rgbtBlue * SinC(U + 1) + I2.rgbtBlue
    * SinC(U) + I3.rgbtBlue * SinC(U - 1) + I4.rgbtBlue * SinC(U - 2));
  if I > 255 then
  	I := 255
	else if I < 0 then
  	I := 0;
  New.rgbtBlue := Byte(I);
end;

procedure ResizeBicubic(Src: TBitmap; var Dest: TBitmap;
  DestWidth, DestHeight, SrcWidth, SrcHeight: Integer);
var
  ScaleH, ScaleW: Double;
  F1, F2, F3, F4, FNew: TRGBTriple;
  Temp1, Temp2, Temp3, Temp4, TempDst: PRGBTripleArraY;
  X, Y, U, V: Double;
  X1, X2, X3, X4, Y1, Y2, Y3, Y4, I, J, TempRGB: Integer;
begin
  Dest := TBitmap.Create;
  Dest.PixelFormat := pf24Bit;
  Dest.Width := DestWidth;
  Dest.Height := DestHeight;
  Src.PixelFormat := pf24Bit;
  Src.Width := SrcWidth;
  Src.Height := SrcHeight;
  ScaleH := DestHeight / SrcHeight;
  ScaleW := DestWidth / SrcWidth;
  for I := 0 to DestHeight - 1 do
  begin
    X := I / ScaleH;
    X2 := Trunc(X);
    X1 := X2 - 1;
    X3 := X2 + 1;
    X4 := X2 + 2;
    if X1 < 0 then X1 := 0;
    if X3 > SrcHeight - 1 then X3 := SrcHeight - 1;
    if X4 > SrcHeight - 1 then X4 := SrcHeight - 1;
    Temp1 := Src.ScanLine[X1];
    Temp2 := Src.ScanLine[X2];
    Temp3 := Src.ScanLine[X3];
    Temp4 := Src.ScanLine[X4];
    TempDst := Dest.ScanLine[I];
    V := X - X2;
    for J := 0 to DestWidth - 1 do
    begin
      Y := J / ScaleW;
      Y2 := Trunc(Y);
      Y1 := Y2 - 1;
      Y3 := Y2 + 1;
      Y4 := Y2 + 2;
      if Y1 < 0 then Y1 := 0;
      if Y3 > SrcWidth - 1 then Y3 := SrcWidth - 1;
      if Y4 > SrcWidth - 1 then Y4 := SrcWidth - 1;
      U := Y - Y2;
      Bicubic(Temp1^[Y1], Temp1^[Y2], Temp1^[Y3], Temp1^[Y4], F1, U);
      Bicubic(Temp2^[Y1], Temp2^[Y2], Temp2^[Y3], Temp2^[Y4], F2, U);
      Bicubic(Temp3^[Y1], Temp3^[Y2], Temp3^[Y3], Temp3^[Y4], F3, U);
      Bicubic(Temp4^[Y1], Temp4^[Y2], Temp4^[Y3], Temp4^[Y4], F4, U);
      Bicubic(F1, F2, F3, F4, FNew, V);
      TempDst^[J] := FNew;
    end;
  end;
end;

procedUre ResizeBilinear(Src: TBitmap; var Dest: TBitmap;
  DestWidth, DestHeight, SrcWidth, SrcHeight: Integer);
var
  ScaleH, ScaleW: Double;
  F1, F2, F3, F4, F12, F34, FNew: TRGBTriple;
  Temp1, Temp2, TempDst: PRGBTripleArraY;
  X, Y: Double;
  X1, X2, Y1, Y2, I, J: Integer;
begin
  Dest := TBitmap.Create;
  Dest.PixelFormat := pf24Bit;
  Dest.Width := DestWidth;
  Dest.Height := DestHeight;
  Src.PixelFormat := pf24Bit;
  Src.Width := SrcWidth;
  Src.Height := SrcHeight;
  ScaleH := DestHeight / SrcHeight;
  ScaleW := DestWidth / SrcWidth;
  for I := 0 to DestHeight - 1 do
  begin
    X := I / ScaleH;
    X1 := Trunc(X);
    X2 := X1 + 1;
    if X2 > SrcHeight - 1 then X2 := SrcHeight - 1;
    Temp1 := Src.ScanLine[X1];
    Temp2 := Src.ScanLine[X2];
    TempDst := Dest.ScanLine[I];
    for J := 0 to DestWidth - 1 do
    begin
      Y := J / ScaleW;
      Y1 := Trunc(Y);
      Y2 := Y1 + 1;
      if Y2 > SrcWidth - 1 then Y2 := SrcWidth - 1;
      F1 := Temp1^[Y1];
      F2 := Temp1^[Y2];
      F3 := Temp2^[Y1];
      F4 := Temp2^[Y2];
      F12.rgbtRed := Trunc(F1.rgbtRed + (Y - Y1) * (F2.rgbtRed - F1.rgbtRed));
      F12.rgbtGreen := Trunc(F1.rgbtGreen + (Y - Y1) * (F2.rgbtGreen - F1.rgbtGreen));
      F12.rgbtBlue := Trunc(F1.rgbtBlue + (Y - Y1) * (F2.rgbtBlue - F1.rgbtBlue));
      F34.rgbtRed := Trunc(F3.rgbtRed + (Y - Y1) * (F4.rgbtRed - F3.rgbtRed));
      F34.rgbtGreen := Trunc(F3.rgbtGreen + (Y - Y1) * (F4.rgbtGreen - F3.rgbtGreen));
      F34.rgbtBlue := Trunc(F3.rgbtBlue + (Y - Y1) * (F4.rgbtBlue - F3.rgbtBlue));
      FNew.rgbtRed := Trunc(F12.rgbtRed + (X - X1) * (F34.rgbtRed - F12.rgbtRed));
      FNew.rgbtGreen := Trunc(F12.rgbtGreen + (X - X1) * (F34.rgbtGreen - F12.rgbtGreen));
      FNew.rgbtBlue := Trunc(F12.rgbtBlue + (X - X1) * (F34.rgbtBlue - F12.rgbtBlue));
      TempDst^[J] := FNew;
    end;
  end;
end;

procedUre ResizeNearest(Src: TBitmap; var Dest: TBitmap;
  DestWidth, DestHeight, SrcWidth, SrcHeight: Integer);
var
  ScaleH, ScaleW: Double;
  FNew: TRGBTriple;
  TempSrc, TempDst: PRGBTripleArraY;
  X, Y, I, J: Integer;
begin
  Dest := TBitmap.Create;
  Dest.PixelFormat := pf24Bit;
  Dest.Width := DestWidth;
  Dest.Height := DestHeight;
  Src.PixelFormat := pf24Bit;
  Src.Width := SrcWidth;
  Src.Height := SrcHeight;
  ScaleH := DestHeight / SrcHeight;
  ScaleW := DestWidth / SrcWidth;
  for I := 0 to DestHeight - 1 do
  begin
    X := Round(I / ScaleH);
    if X > SrcHeight - 1 then X := SrcHeight - 1;
    TempSrc := Src.ScanLine[X];
    TempDst := Dest.ScanLine[I];
    for J := 0 to DestWidth - 1 do
    begin
      Y := Round(J / ScaleW);
      if Y > SrcWidth - 1 then Y := SrcWidth - 1;
      FNew := TempSrc^[Y];
      TempDst^[J] := FNew;
    end;
  end;
end;

procedure ResizeBitmap(var Bitmap: TBitmap; NewWidth, NewHeight: Integer; Mode: TResizeMode);
var
	Dest: TBitmap;
  W, H: Integer;
begin
	Dest := nil;
  W := Bitmap.Width;
  H := Bitmap.Height;
  if (W > 0) and (H > 0) then
  try
		case Mode of
	  	rmBicubic: ResizeBicubic(Bitmap, Dest, NewWidth, NewHeight, W, H);
	    rmBilinear: ResizeBilinear(Bitmap, Dest, NewWidth, NewHeight, W, H);
	    rmNearest: ResizeNearest(Bitmap, Dest, NewWidth, NewHeight, W, H);
		end;
  finally
    Bitmap.Free;
    Bitmap := Dest;
  end;
end;

procedure RotateBitmap90DegreesCounterClockwise(var ABitmap: TBitmap);
const
  BitsPerByte = 8;
var
  PbmpInfoR: PBitmapInfoHeader;
  bmpBuffer, bmpBufferR: PByte;
  MemoryStream, MemoryStreamR: TMemoryStream;
  PbmpBuffer, PbmpBufferR: PByte;
  BytesPerPixel, PixelsPerByte: Longint;
  BytesPerScanLine, BytesPerScanLineR: Longint;
  PaddingBytes: Longint;
  BitmapOffset: Longint;
  BitCount: Longint;
  WholeBytes, ExtraPixels: Longint;
  SignificantBytes, SignificantBytesR: Longint;
  ColumnBytes: Longint;
  AtLeastEightBitColor: Boolean;
  T: Longint;

procedure NonIntegralByteRotate;
var
  X, Y: Longint;
  I: Longint;
  MaskBits, CurrentBits: Byte;
  FirstMask, LastMask: Byte;
  PFirstScanLine: PByte;
  FirstIndex, CurrentBitIndex: Longint;
  ShiftRightAmount, ShiftRightStart: Longint;
begin
  Inc(PbmpBuffer, BytesPerScanLine * Pred(PbmpInfoR^.biHeight));
  PFirstScanLine := bmpBufferR;
  FirstIndex := BitsPerByte - BitCount;
  LastMask := 1 shl BitCount - 1;
  FirstMask := LastMask shl FirstIndex;
  CurrentBits := FirstMask;
  CurrentBitIndex := FirstIndex;
  ShiftRightStart := BitCount * (PixelsPerByte - 1);
  for Y := 1 to PbmpInfoR^.biHeight do begin
    PbmpBufferR := PFirstScanLine;
    for X := 1 to WholeBytes do begin
      MaskBits := FirstMask;
      ShiftRightAmount := ShiftRightStart;
      for I := 1 to PixelsPerByte do begin
        PbmpBufferR^ := PbmpBufferR^ and not CurrentBits or
                        PbmpBuffer^ and MaskBits shr
                        ShiftRightAmount shl CurrentBitIndex;
        MaskBits := MaskBits shr BitCount;
          Inc(PbmpBufferR, BytesPerScanLineR);
          Dec(ShiftRightAmount, BitCount);
      end;
        Inc(PbmpBuffer);
    end;
    if ExtraPixels <> 0 then begin
      MaskBits := FirstMask;
      ShiftRightAmount := ShiftRightStart;
      for I := 1 to ExtraPixels do begin
        PbmpBufferR^ := PbmpBufferR^ and not CurrentBits or
                        PbmpBuffer^ and MaskBits shr
                        ShiftRightAmount shl CurrentBitIndex;

        MaskBits := MaskBits shr BitCount;
          Inc(PbmpBufferR, BytesPerScanLineR);
        Dec(ShiftRightAmount, BitCount);
      end;
        Inc(PbmpBuffer);
    end;
      Inc(PbmpBuffer, PaddingBytes);

    if CurrentBits = LastMask then begin
      CurrentBits := FirstMask;
      CurrentBitIndex := FirstIndex;
        Inc(PFirstScanLine);
    end
    else begin
      CurrentBits := CurrentBits shr BitCount;
      Dec(CurrentBitIndex, BitCount);
    end;
  end;
end;

procedure IntegralByteRotate;
var
  X, Y: Longint;
begin
  Inc(PbmpBufferR, SignificantBytesR - BytesPerPixel);
  for Y := 1 to PbmpInfoR^.biHeight do begin
    for X := 1 to PbmpInfoR^.biWidth do begin
        Move(PbmpBuffer^, PbmpBufferR^, BytesPerPixel);
        Inc(PbmpBuffer, BytesPerPixel);
        Inc(PbmpBufferR, BytesPerScanLineR);
    end;
      Inc(PbmpBuffer, PaddingBytes);
      Dec(PbmpBufferR, ColumnBytes + BytesPerPixel);
  end;
end;

begin
  MemoryStream := TMemoryStream.Create;
  ABitmap.SaveToStream(MemoryStream);
  ABitmap.Free;
  bmpBuffer := MemoryStream.Memory;
  BitmapOffset := PBitmapFileHeader(bmpBuffer)^.bfOffBits;
    Inc(bmpBuffer, SizeOf(TBitmapFileHeader));
  PbmpInfoR := PBitmapInfoHeader(bmpBuffer);
  bmpBuffer := MemoryStream.Memory;
    Inc(bmpBuffer, BitmapOffset);
  PbmpBuffer := bmpBuffer;
  with PbmpInfoR^ do begin
    BitCount := biBitCount;
    BytesPerScanLine := (((biWidth * BitCount) + 31) div 32) * SizeOf(DWORD);
    BytesPerScanLineR := (((biHeight * BitCount) + 31) div 32) * SizeOf(DWORD);
    AtLeastEightBitColor := BitCount >= BitsPerByte;
    if AtLeastEightBitColor then begin
      BytesPerPixel := biBitCount shr 3;
      SignificantBytes := biWidth * BitCount shr 3;
      SignificantBytesR := biHeight * BitCount shr 3;
      PaddingBytes := BytesPerScanLine - SignificantBytes;
      ColumnBytes := BytesPerScanLineR * biWidth;
    end
    else begin
      PixelsPerByte := SizeOf(Byte) * BitsPerByte div BitCount;
      WholeBytes := biWidth div PixelsPerByte;
      ExtraPixels := biWidth mod PixelsPerByte;
      PaddingBytes := BytesPerScanLine - WholeBytes;
      if ExtraPixels <> 0 then Dec(PaddingBytes);
    end;
    MemoryStreamR := TMemoryStream.Create;
    MemoryStreamR.SetSize(BitmapOffset + BytesPerScanLineR * biWidth);
  end;
  MemoryStream.Seek(0, soFromBeginning);
  MemoryStreamR.CopyFrom(MemoryStream, BitmapOffset);
  bmpBufferR := MemoryStreamR.Memory;
    Inc(bmpBufferR, BitmapOffset);
  PbmpBufferR := bmpBufferR;
  if AtLeastEightBitColor then IntegralByteRotate else NonIntegralByteRotate;
  MemoryStream.Free;
  PbmpBufferR := MemoryStreamR.Memory;
    Inc(PbmpBufferR, SizeOf(TBitmapFileHeader));
  PbmpInfoR := PBitmapInfoHeader(PbmpBufferR);
  with PbmpInfoR^ do begin
    T := biHeight;
    biHeight := biWidth;
    biWidth := T;
    biSizeImage := 0;
  end;
  ABitmap := TBitmap.Create;
  MemoryStreamR.Seek(0, soFromBeginning);
  ABitmap.LoadFromStream(MemoryStreamR);
  MemoryStreamR.Free;
end;

procedure RotateBitmap90DegreesClockwise(var ABitmap: TBitmap);
begin
  RotateBitmap90DegreesCounterClockwise(ABitmap);
  RotateBitmap180Degrees(ABitmap);
end;

procedure RotateBitmap180Degrees(var ABitmap: TBitmap);
var
  RotatedBitmap: TBitmap;
begin
  RotatedBitmap := TBitmap.Create;
  with RotatedBitmap do
  begin
    Width := ABitmap.Width;
    Height := ABitmap.Height;
    Canvas.StretchDraw(Rect(ABitmap.Width - 1, ABitmap.Height - 1, -1, -1),
      ABitmap);
  end;
  ABitmap.Free;
  ABitmap := RotatedBitmap;
end;

var
  InternalHighlight: Integer = -1;
	InternalThemeBorder: Integer = -1;

function clThemeBorder: TColor;
var
	Bitmap: TBitmap;
begin
  if not ThemePainter.Enabled then
  begin
		Result := clHighlight;
		Exit;
	end;
  if ColorToRGB(InternalHighlight) <> ColorToRGB(clHighlight) then
  begin
    InternalHighlight := ColorToRGB(clHighlight);
    InternalThemeBorder := -1;
  end;
	Result := TColor(InternalThemeBorder);
  if InternalThemeBorder <> -1 then Exit;
  Bitmap := TBitmap.Create;
  try
  	Bitmap.Height := 10;
    Bitmap.Width := 10;
    DrawThemeBorder(Bitmap.Canvas.Handle, Rect(0, 0, 10, 10), []);
    Result := Bitmap.Canvas.Pixels[5, 0];
  finally
  	Bitmap.Free;
  end;
  InternalThemeBorder := Integer(Result);
end;

function clThemeBkgnd: TColor;
var
  C: TRGBA;
begin
  if not ThemePainter.Enabled then
  begin
		Result := Blend(clHighlight, clWindow, 60);
		Exit;
	end;
  Result := clThemeBorder;
  C := ColorToRGBA(Result);
  if (Abs(C.Blue - C.Red) < $20) and (Abs(C.Red - C.Green) < $20) and
    (Abs(C.Green - C.Blue) < $20) then
    Result := clHighlight;
end;

function clSelected: TColor;
begin
	Result := Blend(clWindow, clHighlight, 75)
end;

function clSelectedBorder: TColor;
begin
	Result := Blend(clWindow, clHighlight, 50)
end;

function clSelectedBkgnd: TColor;
begin
	Result := Blend(clWindow, clHighlight, 75)
end;

function clColumn: TColor;
begin
	Result := Blend(clWindow, clWindowText, 90);
end;

function DirectionToAlignment(Direction: TDirection): TAlignment;
begin
  Result := taLeftJustify;
  case Direction of
    drLeft, drUp, drDown: Result := taLeftJustify;
    drCenter: Result := taCenter;
    drRight: Result := taRightJustify;
  end;
end;

function AlignmentToDirection(Alignment: TAlignment): TDirection;
begin
  Result := drLeft;
  case Alignment of
    taLeftJustify: Result := drLeft;
    taCenter: Result := drRight;
    taRightJustify: Result := drCenter;
  end;
end;

var
  DeviceContext: HDC;

function InitializeDevice(DC: HDC): HDC;
begin
  Result := DeviceContext;
  DeviceContext := DC;
end;

procedure FinalizeDevice(DC: HDC);
begin
  DeviceContext := DC;
end;

function IntToDevice(Value: Integer; Angle: Integer = 0): Double;
var
  Ratio: Double;
begin
  case Angle of
    0:
      begin
        Value := Value + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
        Ratio := 1 / GetDeviceCaps(DeviceContext, LOGPIXELSX);
      end;
    90:
      begin
        Value := Value + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
        Ratio := 1 / GetDeviceCaps(DeviceContext, LOGPIXELSY);
      end;
  else
    Ratio := 0;
  end;
  Result := Value * Ratio;
end;

function PointToDevice(const Point: TPoint): TFloatPoint;
begin
  Result.x := (Point.x + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.y := (Point.y + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function PointToDevice(X, Y: Integer): TFloatPoint;
begin
  Result.x := (X + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.y := (Y + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function RectToDevice(const Rect: TRect): TFloatRect;
begin
  Result.Left := (Rect.Left + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Top := (Rect.Top + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
  Result.Right := (Rect.Right + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Bottom := (Rect.Bottom + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function RectToDevice(ALeft, ATop, ARight, ABottom: Integer): TFloatRect;
begin
  Result.Left := (ALeft + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Top := (ATop + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
  Result.Right := (ARight + GetDeviceCaps(DeviceContext, PHYSICALOFFSETX)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSX);
  Result.Bottom := (ABottom + GetDeviceCaps(DeviceContext, PHYSICALOFFSETY)) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function PolygonToDevice(const Polygon: TPolygon): TFloatPolygon;
var
  I: Integer;
begin
  SetLength(Result, Length(Polygon));
  for I := Low(Polygon) to High(Polygon) do
    Result[I] := PointToDevice(Polygon[I]);
end;

function DeviceToInt(const Value: Double; Angle: Integer = 0): Integer;
var
  Delta: Integer;
  Ratio: Double;
begin
  case Angle of
    0:
      begin
        Delta := GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
        Ratio := GetDeviceCaps(DeviceContext, LOGPIXELSX);
      end;
    90:
      begin
        Delta := GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
        Ratio := GetDeviceCaps(DeviceContext, LOGPIXELSY);
      end;
  else
    Delta := 0;
    Ratio := 0;
  end;
  Result := Trunc(Value * Ratio) - Delta;
end;

function DeviceToPoint(const Point: TFloatPoint): TPoint;
begin
  Result.x := Trunc(Point.x * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.y := Trunc(Point.y * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToPoint(const X, Y: Double): TPoint;
begin
  Result.x := Trunc(X * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.y := Trunc(Y * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToRect(const Rect: TFloatRect): TRect;
begin
  Result.Left := Trunc(Rect.Left * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Top := Trunc(Rect.Top * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
  Result.Right := Trunc(Rect.Right * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Bottom := Trunc(Rect.Bottom * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToRect(const ALeft, ATop, ARight, ABottom: Double): TRect;
begin
  Result.Left := Trunc(ALeft * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Top := Trunc(ATop * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
  Result.Right := Trunc(ARight * GetDeviceCaps(DeviceContext, LOGPIXELSX)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETX);
  Result.Bottom := Trunc(ABottom * GetDeviceCaps(DeviceContext, LOGPIXELSY)) -
    GetDeviceCaps(DeviceContext, PHYSICALOFFSETY);
end;

function DeviceToPolygon(const Polygon: TFloatPolygon): TPolygon;
var
  I: Integer;
begin
  SetLength(Result, Length(Polygon));
  for I := Low(Polygon) to High(Polygon) do
    Result[I] := DeviceToPoint(Polygon[I]);
end;

procedure OffsetRect(var Rect: TFloatRect; const X, Y: Double);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure InflateRect(var Rect: TFloatRect; const X, Y: Double);
begin
  Rect.Left := Rect.Left - X;
  Rect.Top := Rect.Top - Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

function HeightOf(const Rect: TFloatRect): Double;
begin
  Result := Rect.Bottom - Rect.Bottom;
end;

function WidthOf(const Rect: TFloatRect): Double;
begin
  Result := Rect.Right - Rect.Left;
end;

procedure Slide(var Rect: TFloatRect; Direction: TDirection = drDown;
  Distance: Double = 0);
begin
  case Direction of
    drLeft: OffsetRect(Rect, -WidthOf(Rect) - Distance, 0);
    drUp: OffsetRect(Rect, 0, -HeightOf(Rect) - Distance);
    drRight: OffsetRect(Rect, WidthOf(Rect) + Distance, 0);
    drDown: OffsetRect(Rect, 0, HeightOf(Rect) + Distance);
  end;
end;

function Chamfer(const Rect: TFloatRect; const Size: Double;
  Corners: TCorners): TFloatPolygon;
var
  Corner: TCorner;
  I: Integer;
begin
  I := 0;
  for Corner := Low(TCorner) to High(TCorner) do
    if Corner in Corners then
      Inc(I);
  SetLength(Result, 4 + I);
  I := 0;
  for Corner := Low(TCorner) to High(TCorner) do
  begin
    case Corner of
      cnTopLeft:
        begin
          Result[I] := Rect.TopLeft;
          if Corner in Corners then
          begin
            Result[I].Y := Rect.Top + Size;
            Inc(I);
            Result[I].X := Rect.Left + Size;
            Result[I].Y := Rect.Top;
          end;
        end;
      cnTopRight:
        begin
          Result[I].X := Rect.Right;
          Result[I].Y := Rect.Top;
          if Corner in Corners then
          begin
            Result[I].X := Rect.Right - Size;
            Inc(I);
            Result[I].X := Rect.Right;
            Result[I].Y := Rect.Top + Size;
          end;
        end;
      cnBottomRight:
        begin
          Result[I] := Rect.BottomRight;
          if Corner in Corners then
          begin
            Result[I].Y := Rect.Bottom - Size;
            Inc(I);
            Result[I].X := Rect.Right - Size;
            Result[I].Y := Rect.Bottom;
          end;
        end;
      cnBottomLeft:
        begin
          Result[I].X := Rect.Left;
          Result[I].Y := Rect.Bottom;
          if Corner in Corners then
          begin
            Result[I].X := Rect.Left + Size;
            Inc(I);
            Result[I].X := Rect.Left;
            Result[I].Y := Rect.Bottom - Size;
          end;
        end;
    end;
    Inc(I);
  end;
end;

function GetRegion(const Polygon: TPolygon): HRGN;
begin
  Result := 0;
  if Length(Polygon) > 2 then
    Result := CreatePolygonRgn(Pointer(Polygon)^, Length(Polygon), WINDING);
end;

procedure OffsetRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left + X;
  Rect.Top := Rect.Top + Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

procedure InflateRect(var Rect: TRect; X, Y: Integer);
begin
  Rect.Left := Rect.Left - X;
  Rect.Top := Rect.Top - Y;
  Rect.Right := Rect.Right + X;
  Rect.Bottom := Rect.Bottom + Y;
end;

function HeightOf(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function WidthOf(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function CenterRect(const Rect: TRect; Size: Integer): TRect;
begin
  Result.Left := Rect.Left + (WidthOf(Rect) - Size) shr 1;
  Result.Top := Rect.Top + (HeightOf(Rect) - Size) shr 1;
  Result.Right := Result.Left + Size;
  Result.Bottom := Result.Top + Size;
end;

function MoveRect(const Rect: TRect; X, Y: Integer): TRect;
begin
  Result := Rect;
  with Result do
  begin
    Right := Right - Left + X;
    Bottom := Bottom - Top + Y;
    Left := X;
    Top := Y;
  end;
end;

procedure Slide(var Rect: TRect; Direction: TDirection = drDown;
  Distance: Integer = 0);
begin
  case Direction of
    drLeft: OffsetRect(Rect, -WidthOf(Rect) - Distance, 0);
    drUp: OffsetRect(Rect, 0, -HeightOf(Rect) - Distance);
    drRight: OffsetRect(Rect, WidthOf(Rect) + Distance, 0);
    drDown: OffsetRect(Rect, 0, HeightOf(Rect) + Distance);
  end;
end;

function GetTextAscent: Double;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DeviceContext, TextMetric);
  Result := (TextMetric.tmHeight - TextMetric.tmDescent) /
    GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function GetTextDescent: Double;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DeviceContext, TextMetric);
  Result := TextMetric.tmDescent / GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

function GetTextBaseline: Double;
var
  TextMetric: TTextMetric;
begin
  GetTextMetrics(DeviceContext, TextMetric);
  with TextMetric do
    Result := (tmHeight + tmExternalLeading) /
      GetDeviceCaps(DeviceContext, LOGPIXELSY);
end;

{ Color manipulation functions }

type
  TQuad = array[0..3] of Byte;

function QuadRange(Value: Integer): Byte;
begin
  if Value > 255 then
    Result := 255
  else if Value < 0 then
    Result := 0
  else
    Result := Value;
end;

function Blend(ForeColor: TColor; BackColor: TColor; Percent: Byte = 50): TColor;
var
  RGB: TQuad absolute Result;
  F: TQuad;
  B: TQuad;
begin
  F := TQuad(ColorToRGB(ForeColor));
  B := TQuad(ColorToRGB(BackColor));
  RGB[0] := (F[0] * Percent div 100) + (B[0] * (100 - Percent) div 100);
  RGB[1] := (F[1] * Percent div 100) + (B[1] * (100 - Percent) div 100);
  RGB[2] := (F[2] * Percent div 100) + (B[2] * (100 - Percent) div 100);
  RGB[3] := 0;
end;

function Delta(Color: TColor; Value: Integer): TColor;
var
  RGB: TRGBQuad absolute Result;
begin
  RGB := TRGBQuad(ColorToRGB(Color));
  with RGB do
  begin
     rgbBlue := QuadRange(rgbBlue + Value);
     rgbGreen := QuadRange(rgbGreen + Value);
     rgbRed := QuadRange(rgbRed + Value);
  end;
end;

function Scale(Color: TColor; Value: Integer): TColor;
var
  RGB: TRGBQuad absolute Result;
  Factor: Double;
begin
  RGB := TRGBQuad(ColorToRGB(Color));
  Factor := Value / 100;
  with RGB do
  begin
     rgbBlue := QuadRange(Trunc(rgbBlue * Factor));
     rgbGreen := QuadRange(Trunc(rgbGreen * Factor));
     rgbRed := QuadRange(Trunc(rgbRed * Factor));
  end;
end;

function GetBorder: Integer;
begin
	if ThemePainter.Enabled then
  	Result := 1
  else
  	Result := GetSystemMetrics(SM_CXEDGE);
end;

function AverageColor(Color: TColor): Byte;
var
	RGB: TRGBQuad absolute Color;
begin
  Color := ColorToRGB(Color);
  Result := (RGB.rgbBlue + RGB.rgbGreen + RGB.rgbRed) div 3;
end;

function GetTextColor(Background: TColor): TColor;
var
  L, H: TColor;
begin
  if AverageColor(clWindow) > AverageColor(clWindowFrame) then
  begin
	  L := ColorToRGB(clWindowFrame);
  	H := ColorToRGB(clWindow);
	end
  else
  begin
	  H := ColorToRGB(clWindowFrame);
  	L := ColorToRGB(clWindow);
	end;
  if AverageColor(Background) > 128 then
  	Result := L
	else
  	Result := H;
end;

{ Get object functions }

function GetBitmap(ForeColor: TColor; BackColor: TColor): TBitmap;
var
  PixelColors: array[Boolean] of TColor;
  Col: Integer;
  Row: Integer;
begin
  PixelColors[False] := ForeColor;
  PixelColors[True] := BackColor;
  Result := TBitmap.Create;
  with Result do
  begin
    Height := 8;
    Width := 8;
    for Col := 0 to Width - 1 do
      for Row := 0 to Height - 1 do
        Canvas.Pixels[Col, Row] := PixelColors[Odd(Col + Row)];
    HandleType := bmDDB;
  end;
end;

function GetBitmap(Resource: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.LoadFromResourceID(hInstance, Resource);
  except
    Result.Free;
    raise;
  end;
end;

function GetBrush(Bitmap: TBitmap): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_PATTERN;
  LogBrush.lbColor := 0;
  LogBrush.lbHatch := Bitmap.Handle;
  Result := CreateBrushIndirect(LogBrush);
end;

function GetBrush(Bitmap: TFastBitmap): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_PATTERN;
  LogBrush.lbColor := 0;
  LogBrush.lbHatch := Bitmap.Handle;
  Result := CreateBrushIndirect(LogBrush);
end;

function GetBrush(Color: TColor; Style: TBrushStyle = bsSolid): HBRUSH;
var
  LogBrush: TLogBrush;
begin
  Result := 0;
  LogBrush.lbStyle := BS_HATCHED;
  LogBrush.lbColor := ColorToRGB(Color);
  with LogBrush do
    case Style of
      bsSolid: Result := CreateSolidBrush(lbColor);
      bsClear: lbStyle := BS_HOLLOW;
    else
      lbHatch := Ord(Style) - Ord(bsHorizontal);
    end;
  if Result = 0 then
    Result := CreateBrushIndirect(LogBrush);
end;

function GetBrush(Bitmap: TBitmapBrush): HBRUSH; overload;
const
	ImageOffset = 20;
var
	B: TBitmap;
begin
	case Bitmap of
  	bbChecker:
    	begin
			  B := TBitmap.Create;
			  B.Height :=  ImageOffset;
			  B.Width := ImageOffset;
			  with B.Canvas do
			  begin
			  	Brush.Color := clWhite;
			  	FillRect(Rect(0, 0, ImageOffset, ImageOffset));
					Brush.Color := clSilver;
			  	FillRect(Rect(0, ImageOffset div 2, ImageOffset div 2, ImageOffset));
					FillRect(Rect(ImageOffset div 2, 0, ImageOffset, ImageOffset div 2));
				end;
			end;
	else
		B := GetBitmap(clBtnFace, clBtnHighlight);
	end;
  Result := GetBrush(B);
  B.Free;
end;

function GetBrush(C1, C2: TColor; Size: Integer = 1): HBRUSH; overload;
var
  B: TFastBitmap;
  R: TRect;
begin
  Result := 0;
  if Size < 1 then Exit;
  B := CreateFastBitmap(Size * 2, Size * 2);
  R := GetRect(0, 0, B.Width, B.Height);
  FillRectColor(B.DC, R, C2);
  R.Right := R.Right shr 1;
  R.Bottom := R.Right;
  FillRectColor(B.DC, R, C1);
  OffsetRect(R, Size, Size);
  FillRectColor(B.DC, R, C1);
  Result := GetBrush(B);
  DestroyFastBitmap(B);
end;

function GetPen(Color: TColor; Width: Integer = 0; Square: Boolean = False): HPEN;
var
  LogBrush: TLogBrush;
begin
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := ColorToRGB(Color);
  if Square then
    Result := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_SQUARE or PS_JOIN_BEVEL, Width,
      LogBrush, 0, nil)
  else
    Result := CreatePen(PS_SOLID, Width, LogBrush.lbColor)
end;

function GetPen(Color: TColor; Style: TPenStyle; Width: Integer = 1): HPEN; overload;
const
  PenStyles: array[psSolid..psInsideFrame] of Cardinal =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME);
begin
  if Style > psInsideFrame then
  begin
    Result := 0;
    Exit;
  end;
  Result := CreatePen(PenStyles[Style], Width, ColorToRGB(Color));
end;

function GetRegion(DC: HDC): HRGN;
var
  Wnd: HWND;
  Rect: TRect;
begin
  Wnd := WindowFromDC(DC);
  if Wnd <> 0 then
  begin
    GetWindowRect(Wnd, Rect);
    with Rect do
      Result := CreateRectRgn(Left, Top, Right, Bottom);
    GetClipRgn(DC, Result);
  end
  else
    Result := 0;
end;

function GetFont(const Name: string; Size: Integer; Italic: Boolean = False;
  Underlined: Boolean = False; Weight: Integer = 0;  Charset: TFontCharset = 0): HFONT;
const
  BoolBytes: array[Boolean] of Byte = (0, 1);
var
  LogFont: TLogFont;
  DC: HDC;
  S: string;
begin
  FillChar(LogFont, SizeOf(TLogFont), #0);
  DC := GetDC(0);
  LogFont.lfHeight := -MulDiv(Size, GetDeviceCaps(DC, LOGPIXELSY), 72);
  ReleaseDC(0, DC);
  LogFont.lfItalic := BoolBytes[Italic];
  LogFont.lfUnderline := BoolBytes[Underlined];
  LogFont.lfWeight := Weight;
  LogFont.lfCharSet := Charset;
  LogFont.lfQuality := CLEARTYPE_QUALITY or PROOF_QUALITY;
  LogFont.lfOutPrecision := OUT_DEVICE_PRECIS;
  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  S := Name;
  if Length(S) > 31 then
    SetLength(S, 31);
  StrPCopy(LogFont.lfFaceName, PChar(S));
  Result := CreateFontIndirect(LogFont);
end;

function GetFontDirect(Font: HFONT; Style: TFontStyles): HFONT;
const
  BoolBytes: array[Boolean] of Byte = (0, 1);
var
  LogFont: TLogFont;
begin
  GetObject(Font, SizeOf(LogFont), @LogFont);
  LogFont.lfItalic := BoolBytes[fsItalic in Style];
  LogFont.lfUnderline := BoolBytes[fsUnderline in Style];
  if fsBold in Style then
    LogFont.lfWeight := FW_BOLD
  else
    LogFont.lfWeight := FW_NORMAL;
  LogFont.lfQuality := CLEARTYPE_QUALITY or PROOF_QUALITY;
  Result := CreateFontIndirect(LogFont);
end;

function GetFont(DC: HDC; Style: TFontStyles): HFONT;
begin
  Result := GetFontDirect(GetCurrentObject(DC, OBJ_FONT), Style);
end;

function GetFont(DC: HDC; Style: TFontStyles; Size: Integer): HFONT; overload;
const
  BoolBytes: array[Boolean] of Byte = (0, 1);
var
  LogFont: TLogFont;
begin
  Result := GetCurrentObject(DC, OBJ_FONT);
  GetObject(Result, SizeOf(LogFont), @LogFont);
  LogFont.lfItalic := BoolBytes[fsItalic in Style];
  LogFont.lfUnderline := BoolBytes[fsUnderline in Style];
  if fsBold in Style then
    LogFont.lfWeight := FW_BOLD
  else
    LogFont.lfWeight := FW_NORMAL;
  LogFont.lfHeight := Size;
  LogFont.lfQuality := CLEARTYPE_QUALITY or PROOF_QUALITY;
  LogFont.lfOutPrecision := OUT_DEVICE_PRECIS;
  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  Result := CreateFontIndirect(LogFont);
end;

function SelectFontStyle(DC: HDC; Style: TFontStyles): HFONT;
begin
  Result := SelectObject(DC, GetFont(DC, Style));
end;

function SelectFontStyle(DC: HDC; Style: TFontStyles; Size: Integer): HFONT;
begin
  Result := SelectObject(DC, GetFont(DC, Style, Size));
end;

function GetFontBold(const Name: string; Size: Integer): HFONT;
begin
  Result := GetFont(Name, Size, False, False, FW_BOLD);
end;


procedure FillRectChecker(DC: HDC; const Rect: TRect; C1, C2: TColor; CheckerSize: Integer = 10);
var
  B: HBRUSH;
begin
  B := GetBrush(C1, C2, CheckerSize);
  FillRect(DC, Rect, B);
  DeleteObject(B);
end;

procedure FillRectChecker(DC: HDC; const Rect: TRect; CheckerSize: Integer = 10);
begin
  FillRectChecker(DC, Rect, clWhite, clSilver, CheckerSize);
end;

procedure FillRectColor(DC: HDC; const Rect: TRect; Color: TColor);
var
	B: HBRUSH;
begin
	B:= GetBrush(Color);
  FillRect(DC, Rect, B);
  DeleteObject(B);
end;

procedure FillRectColorAlpha(DC: HDC; Rect: TRect; Color: TColor; Alpha: Byte);
var
  B: TFastBitmap;
  R: Single;
  C: TRGBA;
	F: TBlendFunction;
begin
  B := CreateFastBitmap(1, 1, pd32);
  R := Alpha / $FF;
  C := ColorToRGBA(Color);
  C.Red := Round(C.Red * R);
  C.Green := Round(C.Green * R);
  C.Blue := Round(C.Blue * R);
  C.Alpha := Alpha;
  PRGBA(B.Bits)^ := C;
  FillChar(F, SizeOf(F), #0);
  F.SourceConstantAlpha := $FF;
  F.AlphaFormat := AC_SRC_ALPHA;
  AlphaBlend(DC, Rect.Left, Rect.Top, WidthOf(Rect), HeightOf(Rect),
    B.DC, 0, 0, 1, 1, F);
  DestroyFastBitmap(B);
end;

procedure FillRectColorPixel(DC: HDC; Rect: TRect; Pixel: TFastBitmap);
var
	F: TBlendFunction;
begin
  FillChar(F, SizeOf(F), #0);
  F.SourceConstantAlpha := $FF;
  F.AlphaFormat := AC_SRC_ALPHA;
  AlphaBlend(DC, Rect.Left, Rect.Top, WidthOf(Rect), HeightOf(Rect),
    Pixel.DC, 0, 0, 1, 1, F);
end;

procedure FillRectOutline(DC: HDC; const Rect: TRect; Color: TColor; Style: TPenStyle = psSolid);
var
	P: HPEN;
begin
  SetBkMode(DC, TRANSPARENT);
	P := SelectObject(DC, GetPen(Color, Style));
  with Rect do
  begin
  	MoveToEx(DC, Left, Top, nil);
		LineTo(DC, Right - 1, Top);
		LineTo(DC, Right - 1, Bottom - 1);
		LineTo(DC, Left, Bottom - 1);
		LineTo(DC, Left, Top);
  end;
  OverwriteObject(DC, P);
end;

procedure FillRectFrame(DC: HDC; const Rect: TRect);
begin
  FillRectColor(DC, Rect, clWindow);
  if ThemePainter.Enabled then
    FillRectOutline(DC, Rect, clThemeBorder)
  else
    DrawFrame(DC, Rect, dfSunken);
end;

procedure FillRoundRect(DC: HDC; const Rect: TRect; Radius: Integer);
begin
  RoundRect(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Radius, Radius);
end;

procedure FillRoundRect(DC: HDC; const Rect: TRect; Radius: Integer;
  Pen, Brush: TColor); overload;
var
  B: HBRUSH;
  P: HPEN;
begin
  B := SelectObject(DC, GetBrush(Brush));
  P := SelectObject(DC, GetPen(Pen));
  RoundRect(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Radius, Radius);
  OverwriteObject(DC, B);
  OverwriteObject(DC, P);
end;

procedure FillRoundRect(DC: HDC; const Rect: TRect; Radius: Integer;
  Pen: TColor); overload;
var
  B: HBRUSH;
  P: HPEN;
begin
  B := SelectObject(DC, GetStockObject(NULL_BRUSH));
  P := SelectObject(DC, GetPen(Pen));
  RoundRect(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Radius, Radius);
  SelectObject(DC, B);
  OverwriteObject(DC, P);
end;

procedure FillRoundRectFancy(DC: HDC; const Rect: TRect; Radius: Integer;
  GradStart, GradEnd: TColor; GradDir: TDirection; Pen, Brush: TColor;
  PenStyle: TPenStyle; BrushStyle: TBrushStyle);
var
  B: HBRUSH;
  P: HPEN;
  M: DWORD;
begin
  BeginPath(DC);
  FillRoundRect(DC, Rect, Radius, Pen, Brush);
  EndPath(DC);
  SelectClipPath(DC, RGN_COPY);
  DrawGradient(DC, Rect, GradStart, GradEnd, GradDir);
  SelectClipRgn(DC, 0);
  P := SelectObject(DC, GetPen(Pen, PenStyle));
  B := SelectObject(DC, GetBrush(Brush, BrushStyle));
  M := SetBkMode(DC, TRANSPARENT);
  FillRoundRect(DC, Rect, Radius);
  OverwriteObject(DC, B);
  OverwriteObject(DC, P);
  SetBkMode(DC, M);
end;

{ Scratch object management }

var
  ScratchFont: TFont;
  ScratchBitmap: TBitmap;

procedure SwapFont(A, B: TFont);
begin
  if ScratchFont = nil then
  	ScratchFont := TFont.Create;
  ScratchFont.Assign(A);
  A.Assign(B);
  B.Assign(ScratchFont);
end;

procedure ColorMaskBlt(DC: HDC; const Rect, Mask: TRect; ForeColor, BackColor: TColor);
const
  Rop: array[Boolean] of Cardinal = (SRCCOPY, SRCAND);
var
  MemDC: HDC;
  A, B: TRect;
  Brush: HBrush;
  W, H: Integer;
begin
  if IsRectEmpty(Rect) or IsRectEmpty(Mask) then Exit;
  MemDC := ScratchBitmap.Canvas.Handle;
  BitBlt(MemDC, 0, 0, WidthOf(Mask), HeightOf(Mask), 0, 0, 0, DSTINVERT);
  A := Mask;
  OffsetRect(A, WidthOf(Mask), 0);
  Brush := CreateSolidBrush(ColorToRGB(ForeColor));
  FillRect(MemDC, A, Brush);
  DeleteObject(Brush);
  BitBlt(MemDC, A.Left, A.Top, WidthOf(Mask), HeightOf(Mask), MemDC, 0, 0,
    SRCAND);
  BitBlt(MemDC, 0, 0, WidthOf(Mask), HeightOf(Mask), 0, 0, 0, DSTINVERT);
  B := A;
  OffsetRect(B, WidthOf(Mask), 0);
  Brush := CreateSolidBrush(ColorToRGB(BackColor));
  FillRect(MemDC, B, Brush);
  DeleteObject(Brush);
  BitBlt(MemDC, B.Left, B.Top, WidthOf(Mask), HeightOf(Mask), MemDC, 0, 0,
    SRCAND);
  BitBlt(MemDC, A.Left, A.Top, WidthOf(Mask), HeightOf(Mask), MemDC, B.Left,
    B.Top, SRCPAINT);
  B := Mask;
  W := WidthOf(Rect);
  if WidthOf(B) > W then
  begin
    Inc(A.Left, (WidthOf(B) - W) div 2 + 1);
    B.Right := B.Left + W;
  end;
  H := HeightOf(Rect);
  if HeightOf(B) > H then
  begin
    Inc(A.Top, (HeightOf(B) - H) div 2 + 1);
    B.Bottom := B.Top + H;
  end;
  BitBlt(DC, Rect.Left + W div 2 - WidthOf(B) div 2, Rect.Top +
    H div 2 - HeightOf(B) div 2, WidthOf(B), HeightOf(B),
    MemDC, A.Left, A.Top, Rop[BackColor = $FFFFFF]);
end;

procedure FillColorMaskBlt(DC: HDC; const Rect, Mask: TRect;
  ForeColor, BackColor: TColor);
var
  MemDC: HDC;
  Brush: HBRUSH;
begin
  MemDC := ScratchBitmap.Canvas.Handle;
  Brush := SelectObject(MemDC, GetStockObject(BLACK_BRUSH));
  with Mask do
  begin
    FloodFill(MemDC, Left + WidthOf(Mask) div 2, Top + HeightOf(Mask) div 2,
      GetSysColor(COLOR_BTNFACE));
    SelectObject(MemDC, GetStockObject(WHITE_BRUSH));
    FloodFill(MemDC, Left + 1, Top + 1, 0);
    FloodFill(MemDC, Left, Top, $FFFFFF);
  end;
  SelectObject(DC, Brush);
  ColorMaskBlt(DC, Rect, Mask, ForeColor, BackColor);
end;

{ Utility procedures }

procedure OverwriteObject(DC: HDC; Obj: HGDIOBJ);
begin
  DeleteObject(SelectObject(DC, Obj));
end;

procedure SelectClipRect(DC: HDC; const Rect: TRect; Mode: Integer);
var
  Region: HRGN;
begin
  with Rect do
    Region := CreateRectRgn(Left, Top, Right, Bottom);
  ExtSelectClipRgn(DC, Region, Mode);
  DeleteObject(Region);
end;

procedure FontRatio(var X, Y: Double);
var
	LogFont: TLogFont;
  Font: HFont;
begin
	X := 1;
	Y := 1;
  // !
  Exit;
	if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0) then
  begin
		Font := CreateFontIndirect(LogFont);
    FontRatio(Font, X, Y);
    DeleteObject(Font)
	end;
end;

procedure FontRatio(Font: HFont; var X, Y: Double);
const
	Characters = ' !@#$%^&*()_0123456789ABCDEFGHIJLLMNOPQRSTUVWTYZabcdefghijklmnopqrstuvwxyz';
  DesignX = 448;
  DesignY = 39;
var
	DC: HDC;
  Size: TSize;
  S: string;
begin
  DC := GetDC(0);
  Font := SelectObject(DC, Font);
  S := Characters;
	Size := CalcCaptionSize(DC, S);
	X := Size.cx;
  S := S + #13#10 + S + #13#10 + S;
	Y := CalcMemoHeight(DC, S, Round(X) + 1);
	SelectObject(DC, Font);
  ReleaseDC(0, DC);
  X := X / DesignX;
  Y := Y / DesignY;
end;

{ Draw routines }

type
  TPaintBitmapProc = procedure (DC: HDC; Width, Height: Integer;
    Data: Pointer);

procedure PaintBitmap(Width, Height: Integer; Data: Pointer;
  Proc: TFarProc);
var
  DC: HDC;
  Bitmap: HBITMAP;
  PriorBitmap: HBITMAP;
begin
  DC := GetDC(0);
  Bitmap := CreateCompatibleBitmap(DC, Width, Height);
  ReleaseDC(0, DC);
  DC := CreateCompatibleDC(0);
  PriorBitmap := SelectObject(DC, Bitmap);
  try
    if Proc <> nil then
      TPaintBitmapProc(Proc)(DC, Width, Height, Data);
  finally
    SelectObject(DC, PriorBitmap);
    DeleteDC(DC);
    DeleteObject(Bitmap);
  end;
end;

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
  ForeColor, BackColor: TColor; Size: Integer = 0; Enabled: Boolean = True);
var
  MemDC: HDC;
  Mask: TRect;
  Style: Cardinal;
  Brush: HBRUSH;
begin
  with ScratchBitmap do
  begin
    MemDC := Canvas.Handle;
    BitBlt(MemDC, 0, 0, Width, Height, 0, 0, 0, WHITENESS);
  end;
  if Size > 0 then
    Mask := GetRect(0, 0, Size, Size)
  else
    Mask := GetRect(0, 0, GetSystemMetrics(SM_CXVSCROLL),
      GetSystemMetrics(SM_CYVSCROLL));
  case Direction of
    drLeft: Style := DFCS_SCROLLLEFT;
    drUp: Style := DFCS_SCROLLUP;
    drRight: Style := DFCS_SCROLLRIGHT;
  else
    Style := DFCS_SCROLLDOWN;
  end;
  if Enabled then
  begin
    Style := Style or DFCS_MONO or DFCS_FLAT;
    DrawFrameControl(MemDC, Mask, DFC_SCROLL, Style);
    FillColorMaskBlt(DC, Rect, Mask, ForeColor, BackColor);
  end
  else
  begin
    ExcludeClipRect(DC, Rect.Right, Rect.Top, Rect.Right + 2, Rect.Bottom);
    OffsetRect(Rect, -1, 0);
    Style := Style or DFCS_INACTIVE or DFCS_FLAT;
    DrawFrameControl(MemDC, Mask, DFC_SCROLL, Style);
    Brush := SelectObject(MemDC, GetSysColorBrush(COLOR_BTNFACE));
    FloodFill(MemDC, 0, 0, GetSysColor(COLOR_BTNFACE));
    OverwriteObject(MemDC, Brush);
    BitBlt(DC, Rect.Left + WidthOf(Rect) div 2 - WidthOf(Mask) div 2, Rect.Top +
      HeightOf(Rect) div 2 - HeightOf(Mask) div 2, WidthOf(Mask), HeightOf(Mask),
      MemDC, 0, 0, SRCCOPY);
  end;
end;

procedure DrawBox(DC: HDC; Color: TColor; var Rect: TRect);
var
  Pen: HPEN;
  Point: TPoint;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @Point);
    LineTo(DC, Right - 1, Top);
    LineTo(DC, Right - 1, Bottom - 1);
    LineTo(DC, Left, Bottom - 1);
    LineTo(DC, Left, Top);
    MoveToEx(DC, Point.x, Point.y, nil);
  end;
  InflateRect(Rect, -1, -1);
  OverwriteObject(DC, Pen);
end;

procedure DrawCheckBox(DC: HDC; Rect: TRect; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);
const
  CheckedState: array[Boolean] of Cardinal = (0, DFCS_CHECKED);
var
  Mask: TRect;
  MemDC: HDC;
  I: Integer;
begin
	I := Round(GetDeviceCaps(DC, LOGPIXELSX) / 96 * 13);
  // Mask := GetRect(0, 0, 13, 13);
  Mask := GetRect(0, 0, I, I);
  if Flat then
  begin
    MemDC := ScratchBitmap.Canvas.Handle;
    DrawFrameControl(MemDC, Mask, DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_MONO or
      CheckedState[Checked]);
    ColorMaskBlt(DC, Rect, Mask, ForeColor, BackColor);
  end
  else
  begin
    OffsetRect(Mask, Rect.Left + WidthOf(Rect) div 2 - WidthOf(Mask) div 2,
      Rect.Top + HeightOf(Rect) div 2 - HeightOf(Mask) div 2);
    DrawFrameControl(DC, Mask, DFC_BUTTON, DFCS_BUTTONCHECK or
      CheckedState[Checked]);
  end;
end;

procedure DrawCheckText(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Checked: Boolean; ForeColor, BackColor: TColor;
  Flat: Boolean = True);
var
  PriorColor: COLORREF;
begin
  PriorColor := SetTextColor(DC, ColorToRGB(ForeColor));
  OffsetRect(Rect, 0, -1);
  case Direction of
    drLeft:
      begin
        DrawCaption(DC, Text, Rect, drLeft);
        SetTextColor(DC, PriorColor);
        Inc(Rect.Left, CalcCaptionSize(DC, Text).cx + 5);
        Rect.Right := Rect.Left + 13;
      end;
    drRight:
      begin
        Inc(Rect.Left, 18);
        DrawCaption(DC, Text, Rect, drLeft);
        Dec(Rect.Left, 18);
        SetTextColor(DC, PriorColor);
        Rect.Right := Rect.Left + 13;
      end;
  end;
  DrawCheckBox(DC, Rect, Checked, ForeColor, BackColor, Flat);
end;

procedure DrawSortArrow(DC: HDC; Rect: TRect; Direction: TDirection);
var
  Point: TPoint;
  X, Y: Integer;
  Pen: HPEN;
begin
  with Rect do
  begin
    Pen := SelectObject(DC, CreatePen(PS_SOLID, 0,
      GetSysColor(COLOR_BTNHIGHLIGHT)));
    Point := Rect.TopLeft;
    X := Left + (Right - Left) div 2 + 3;
    Y := Top + (Bottom - Top) div 2 + 3;
    case Direction of
      drUp:
        begin
         MoveToEx(DC, X, Y, @Point);
         LineTo(DC, X + 7, Y);
         LineTo(DC, X + 4, Y - 6);
         LineTo(DC, X + 4, Y - 7);
       end;
      drDown:
        begin
         MoveToEx(DC, X + 4, Y, @Point);
         LineTo(DC, X + 7, Y - 6);
        end;
    end;
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW)));
    case Direction of
      drUp:
        begin
          MoveToEx(DC, X + 3, Y - 6, nil);
          LineTo(DC, X, Y);
          MoveToEx(DC, Point.X, Point.Y, nil);
        end;
      drDown:
        begin
          LineTo(DC, X, Y - 6);
          LineTo(DC, X + 3, Y);
          LineTo(DC, X + 3, Y + 1);
        end;
    end;
    OverwriteObject(DC, Pen);
  end;
end;

procedure DrawSeparator(DC: HDC; Rect: TRect; Color: TColor; Horizontal: Boolean = True);
begin
	if Horizontal then
  begin
		Rect.Left := Rect.Left + (WidthOf(Rect) shr 1);
	  Rect.Right := Rect.Left + 1;
  end
  else
  begin
		Rect.Top := Rect.Top + (HeightOf(Rect) shr 1);
	  Rect.Bottom := Rect.Top + 1;
  end;
  FillRectColor(DC, Rect, Blend(Color, clBlack, 75));
	if Horizontal then
	  Slide(Rect, drRight)
	else
  	Slide(Rect);
  FillRectColor(DC, Rect, Blend(Color, clWhite));
end;

procedure DrawRoundRect(DC: HDC; Rect: TRect; Color: TColor; Hollow: Boolean = False);
const
  Radius = 14;
var
  P: HPEN;
  B: HBRUSH;
begin
  P := GetPen(Blend(Color, 0, 66));
  if Hollow then
    B := GetStockObject(HOLLOW_BRUSH)
  else
    B := GetBrush(Color);
  P := SelectObject(DC, P);
  B := SelectObject(DC, B);
  RoundRect(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Radius, Radius);
  OverwriteObject(DC, P);
  if Hollow then
    SelectObject(DC, B)
  else
    OverwriteObject(DC, B);
end;

procedure DrawStyleRoundText(DC: HDC; Rect: TRect; const Text: string; Color: TColor; Hollow: Boolean = False);
const
  HPad = 6;
  VPad = 4;
var
  Size: TSize;
begin
  Size := CalcCaptionSize(DC, Text);
  Rect.Left := Rect.Left + Round(WidthOf(Rect) / 2 - Size.cx / 2) - VPad;
  Rect.Right := Rect.Left + Size.cx + VPad * 2;
  Rect.Top := Rect.Top + Round(HeightOf(Rect) / 2 - Size.cy / 2) - HPad;
  Rect.Bottom := Rect.Top + Size.cy + HPad * 2;
  DrawRoundRect(DC, Rect, Color, Hollow);
  DrawCaption(DC, Text, Rect, drCenter);
end;

procedure DrawStyleOutline(DC: HDC; Rect: TRect; Light: Boolean = False);
begin
	if Light then
    DrawRectOutline(DC, Rect, clHighlight) // Blend(clHighlight, clWindow, 66))
  else
    DrawRectOutline(DC, Rect, clHighlight);
end;

procedure DrawStyleRect(DC: HDC; Rect: TRect; Light: Boolean = False);
begin
	if Light then
  begin
   	FillRectColor(DC, Rect, Blend(clHighlight, clWindow, 66));
		InflateRect(Rect, -1, -1);
   	FillRectColor(DC, Rect, Blend(clHighlight, clWindow, 15));
	end
  else
  begin
   	FillRectColor(DC, Rect, clHighlight);
		InflateRect(Rect, -1, -1);
   	FillRectColor(DC, Rect, Blend(clHighlight, clWindow, 50));
	end;
end;

procedure DrawMenuHighlightRect(DC: HDC; Rect: TRect);
begin
  DrawRectOutline(DC, Rect, clMenuHighlight);
  InflateRect(Rect, -1, -1);
  FillRectColor(DC, Rect, Blend(clMenuHighlight, clMenu, 33));
end;

procedure DrawFancyMenuHighlightRect(DC: HDC; Rect: TRect);
begin
  FillRoundRectFancy(DC, Rect, 2, clMenu,
    Blend(clMenu, clMenuHighlight, 90), drDown, Blend(clMenu, clMenuHighlight, 60),
    clMenu, psSolid, bsClear);
end;

procedure DrawStyleRoundRect(DC: HDC; Rect: TRect; Light: Boolean = False; Hollow: Boolean = False);
const
  Radius = 14;
var
  P: HPEN;
  B: HBRUSH;
begin
  B := 0;
  if Hollow then
    B := GetStockObject(HOLLOW_BRUSH);
	if Light then
  begin
    P := GetPen(Blend(clHighlight, clWindow, 66));
    if not Hollow then
      B := GetBrush(Blend(clHighlight, clWindow, 20));
	end
  else
  begin
    P := GetPen(clHighlight);
    if not Hollow then
      B := GetBrush(Blend(clHighlight, clWindow, 33));
	end;
  P := SelectObject(DC, P);
  B := SelectObject(DC, B);
  RoundRect(DC, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Radius, Radius);
  OverwriteObject(DC, P);
  if Hollow then
    SelectObject(DC, B)
  else
    OverwriteObject(DC, B);
end;

procedure DrawStyleRectOutline(DC: HDC; Rect: TRect; Light: Boolean = False);
begin
	if Light then
    FillRectOutline(DC, Rect, Blend(clHighlight, clWindow, 33))
  else
    FillRectOutline(DC, Rect, Blend(clHighlight, clWindow, 66));
end;

procedure DrawStyleRoundRectText(DC: HDC; Rect: TRect; const Text: string; Light: Boolean = False; Hollow: Boolean = False);
const
  HPad = 6;
  VPad = 4;
var
  Size: TSize;
begin
  Size := CalcCaptionSize(DC, Text);
  Rect.Left := Rect.Left + Round(WidthOf(Rect) / 2 - Size.cx / 2) - VPad;
  Rect.Right := Rect.Left + Size.cx + VPad * 2;
  Rect.Top := Rect.Top + Round(HeightOf(Rect) / 2 - Size.cy / 2) - HPad;
  Rect.Bottom := Rect.Top + Size.cy + HPad * 2;
  DrawStyleRoundRect(DC, Rect, Light, Hollow);
  DrawCaption(DC, Text, Rect, drCenter);
end;

procedure FillRectState(DC: HDC; Rect: TRect; State: TDrawState; Background: TColor = clWindow);
begin
  if dsSelected in State then
  begin
    if dsFocused in State then
      if dsPressed in State then
       	FillRectColor(DC, Rect, clHighlight)
      else
        DrawStyleRect(DC, Rect, False)
    else
    begin
      FillRect(DC, Rect, Background);
      DrawStyleRectOutline(DC, Rect, False);
    end;
  end
  else if dsHot in State then
    DrawStyleRect(DC, Rect, True)
  else
    FillRect(DC, Rect, Background);
end;

procedure DrawRectState(DC: HDC; Rect: TRect; Color: TColor; State: TDrawState);
begin
  if dsSelected in State then
    DrawStyleRect(DC, Rect, not (dsFocused in State))
  else
  begin
    FillRectColor(DC, Rect, Color);
    if dsHot in State then
      DrawStyleRectOutline(DC, Rect, not (dsFocused in State));
  end;
end;

procedure DrawTextState(DC: HDC; const Text: string; Rect: TRect; Color: TColor; State: TDrawState);
begin
  DrawRectState(Dc, Rect, Color, State);
  InflateRect(Rect, -4, 0);
  DrawCaption(DC, Text, Rect, drLeft);
end;

function CalcCaptionRect(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection): TRect;
var
  Size: TSize;
begin
  FillChar(Size, SizeOf(Size), #0);
  if Text = '' then
	  GetTextExtentPoint32(DC, ' ', 1, Size)
  else
	  GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
  Result := Rect;
  with Result do
  begin
    { if Right - Left < Size.cx then
      Size.cx := Right - Left; }
    Top := Top + (Bottom - Top - Size.cy) div 2;
    Bottom := Top + Size.cy;
    case Direction of
      drLeft:
        Right := Rect.Left + Size.cx;
      drCenter:
        begin
          Left := (Rect.Right - Rect.Left - Size.cx) div 2;
          Right := Left + Size.cx;
        end;
      drRight:
        Left := Rect.Right - Size.cx;
      drFill:
      	begin
        	Left := Left + WidthOf(Rect) div 2 - Size.cx div 2;
          Right := Left + Size.cx;
        end;
    end;
  end;
end;

function CalcCaptionSize(DC: HDC; const Text: string): TSize;
begin
  FillChar(Result, SizeOf(TSize), #0);
  GetTextExtentPoint32(DC, PChar(Text), Length(Text), Result);
end;

function FontHeight(DC: HDC): Integer;
begin
  Result := CalcCaptionSize(DC, 'Wg').cy;
end;

function FontHeight(Font: TFont): Integer; 
var
  DC: HDC;
  F: HFONT;
begin
  DC := GetDC(0);
  F := SelectObject(DC, Font.Handle);
  Result := FontHeight(DC);
  SelectObject(DC, F);
  ReleaseDC(0, DC);
end;

function FontWidth(DC: HDC; const S: string): Integer;
begin
  Result := CalcCaptionSize(DC, S).cx
end;

function FontWidth(Font: TFont; const S: string): Integer;
var
  DC: HDC;
  F: HFONT;
begin
  DC := GetDC(0);
  F := SelectObject(DC, Font.Handle);
  Result := FontWidth(DC, S);
  SelectObject(DC, F);
  ReleaseDC(0, DC);
end;

function FontSize(DC: HDC; const S: string): TSize; overload;
begin
  Result := CalcCaptionSize(DC, S);
end;

function FontSize(Font: TFont; const S: string): TSize; overload;
var
  DC: HDC;
  F: HFONT;
begin
  DC := GetDC(0);
  F := SelectObject(DC, Font.Handle);
  Result := CalcCaptionSize(DC, S);
  SelectObject(DC, F);
  ReleaseDC(0, DC);
end;

function CalcMemoHeight(DC: HDC; const Text: string; Width: Integer): Integer;
var
  Rect: TRect;
begin
  Rect := GetRect(0, 0, Width, 0);
  Result := DrawText(DC, PChar(Text), -1, Rect, DT_LEFT or DT_TOP or
    DT_WORDBREAK or DT_CALCRECT);
end;


procedure DrawCaption(DC: HDC;  const Caption: string; Rect: TRect;  Direction: TDirection; Enabled: Boolean = True; HidePrefix: Boolean = False);
const
	PrefixFlags: array[Boolean] of Integer = (0, DT_HIDEPREFIX);
  TabSpace = 10;
var
  DrawRect: TRect;
  PriorMode: Integer;
  PriorColor: COLORREF;
begin
  DrawRect := Rect;
  PriorMode := SetBkMode(DC, TRANSPARENT);
  if Enabled then
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction] or PrefixFlags[HidePrefix])
  else
  begin
    OffsetRect(DrawRect, 1, 1);
    PriorColor := SetTextColor(DC, GetSysColor(COLOR_BTNHIGHLIGHT));
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction] or PrefixFlags[HidePrefix]);
    OffsetRect(DrawRect, -1, -1);
    SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));
    DrawText(DC, PChar(Caption), -1, DrawRect, Directions[Direction] or PrefixFlags[HidePrefix]);
    SetTextColor(DC, PriorColor);
  end;
  SetBkMode(DC, PriorMode);
end;

procedure DrawEditText(DC: HDC; const Text: string; const Rect: TRect; Direction: TDirection);
var
  PriorMode: Integer;
  PriorColor: COLORREF;
  DrawRect: TRect;
  F: Cardinal;
begin
  if Direction in [drLeft, drCenter, drRight] then
  begin
    DrawRect := Rect;
    PriorMode := SetBkMode(DC, TRANSPARENT);
    PriorColor := SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT));
    Inc(DrawRect.Top, 3);
    F := DT_SINGLELINE or DT_TOP or DT_NOCLIP;
    if Direction = drLeft then
      F := F or DT_LEFT
    else if Direction = drCenter then
      F := F or DT_CENTER
    else
      F := F or DT_RIGHT;
    DrawText(DC, PChar(Text), -1, DrawRect, F);
    SetTextColor(DC, PriorColor);
    SetBkMode(DC, PriorMode);
  end;
end;

procedure DrawFocus(DC: HDC; Rect: TRect; BorderX: Integer = 0; BorderY: Integer = 0);
var
  PriorColor: COLORREF;
begin
	InflateRect(Rect, BorderX, BorderY);
  PriorColor := SetTextColor(DC, 0);
  DrawFocusRect(DC, Rect);
  SetTextColor(DC, PriorColor);
end;

procedure DrawArrow(DC: HDC; Rect: TRect; Direction: TDirection;
	Color: TColor = clWindowFrame; Enabled: Boolean = True);
var
	Glyph: TGlyphKind;
begin
	case Direction of
  	drLeft: Glyph := gkArrowLeft;
    drRight:Glyph := gkArrowRight;
  	drUp: Glyph := gkArrowUp;
  	drDown: Glyph := gkArrowDown;
  else
  	Exit;
  end;
  if not Enabled then
    Inc(Glyph, 4);
	GlyphDraw(DC, Rect, Glyph, Color);
end;

procedure DrawClose(DC: HDC; Rect: TRect; Color: TColor = clWindowFrame);
begin
	GlyphDraw(DC, Rect, gkClose, Color);
end;

procedure DrawDivider(DC: HDC; Rect: TRect; Kind: TDrawDividerKind);
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @PriorPoint);
    PriorPen := SelectObject(DC, CreatePen(PS_SOLID, 0,
      GetSysColor(COLOR_BTNSHADOW)));
    if Kind = ddVert then
    begin
      LineTo(DC, Right, Top);
      MoveToEx(DC, Left, Top + 1, nil);
    end
    else
    begin
      LineTo(DC, Left, Bottom);
      MoveToEx(DC, Left + 1, Top, nil);
    end;
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNHILIGHT)));
    if Kind = ddVert then
      LineTo(DC, Right, Top + 1)
    else
      LineTo(DC, Left + 1, Bottom);
    OverwriteObject(DC, PriorPen);
    with PriorPoint do
      MoveToEx(DC, X, Y, nil);
  end;
end;

procedure DrawEllipsis(DC: HDC; Rect: TRect; Enabled: Boolean = True);

  procedure DrawDots(Dot: TRect; Color: TColor);
  var
    I: Integer;
  begin
    with Rect do
      OffsetRect(Dot, Left + (WidthOf(Rect) - 10) div 2, Top + HeightOf(Rect) - 6);
    for I := 0 to 2 do
    begin
      FillRect(DC, Dot, Color + 1);
      OffsetRect(Dot, 4, 0);
    end;
  end;

begin
  FillRect(DC, Rect, COLOR_BTNFACE + 1);
  if Enabled then
    DrawDots(GetRect(0, 0, 2, 2), COLOR_WINDOWTEXT)
  else
  begin
    DrawDots(GetRect(1, 1, 3, 3), COLOR_BTNHIGHLIGHT);
    DrawDots(GetRect(0, 0, 2, 2), COLOR_BTNSHADOW);
  end;
end;

procedure DrawFrame(DC: HDC; Rect: TRect; State: TDrawFrameState);
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  if State = dfFlat then
    Exit;
  with Rect do
  begin
    Dec(Right);
    Dec(Bottom);
    MoveToEx(DC, Left, Top, @PriorPoint);
    PriorPen := SelectObject(DC, CreatePen(PS_SOLID, 0,
      GetSysColor(COLOR_BTNFACE)));
    case State of
      dfFocus:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_WINDOWFRAME)));
          LineTo(DC, Right, Top);
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Top);
        end;
      dfFramed:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Left, Top);
          InflateRect(Rect, -1, -1);
          MoveToEx(DC, Left, Top, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Left, Top);
        end;
     dfHover:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNFACE)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNFACE)));
          LineTo(DC, Left, Top);
        end;
     dfRaised:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Left, Top);
        end;
     dfLowered:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
        end;
     dfPressed:
        begin
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNFACE)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Left, Top);
        end;
     dfSunken:
       begin
          MoveToEx(DC, Left, Bottom, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
          InflateRect(Rect, -1, -1);
          MoveToEx(DC, Left, Bottom, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Left, Top);
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
       end;
     dfPushed:
        begin
          InflateRect(Rect, -1, -1);
          MoveToEx(DC, Left, Top, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DLIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_BTNSHADOW)));
          LineTo(DC, Left, Top);
          InflateRect(Rect, 1, 1);
          MoveToEx(DC, Left, Top, nil);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Right, Top);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DHILIGHT)));
          LineTo(DC, Right, Bottom);
          LineTo(DC, Left, Bottom);
          LineTo(DC, Left, Bottom - 1);
          OverwriteObject(DC, CreatePen(PS_SOLID, 0,
            GetSysColor(COLOR_3DDKSHADOW)));
          LineTo(DC, Left, Top);
        end;
    end;
    OverwriteObject(DC, PriorPen);
    with PriorPoint do
      MoveToEx(DC, x, y, nil);
  end;
end;

procedure DrawGradient(DC: HDC; Rect: TRect; StartColor, EndColor: TColor;
  Direction: TDirection);
var
  Colors: Integer;
  Delta: Integer;
  StartRGB: array[0..2] of Byte;
  DeltaRGB: array[0..2] of Integer;
  ColorBand: TRect;
  Brush: HBrush;
  I: Integer;
begin
  if IsRectEmpty(Rect) then Exit;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  if StartColor = EndColor then
  begin
    FillRectColor(DC, Rect, StartColor);
    Exit;
  end;
  case Direction of
    drDown, drRight:
      begin
        StartRGB[0] := GetRValue(StartColor);
        StartRGB[1] := GetGValue(StartColor);
        StartRGB[2] := GetBValue(StartColor);
        DeltaRGB[0] := GetRValue(EndColor) - StartRGB[0];
        DeltaRGB[1] := GetGValue(EndColor) - StartRGB[1];
        DeltaRGB[2] := GetBValue(EndColor) - StartRGB[2];
      end;
    drUp, drLeft:
      begin
        StartRGB[0] := GetRValue(EndColor);
        StartRGB[1] := GetGValue(EndColor);
        StartRGB[2] := GetBValue(EndColor);
        DeltaRGB[0] := GetRValue(StartColor) - StartRGB[0];
        DeltaRGB[1] := GetGValue(StartColor) - StartRGB[1];
        DeltaRGB[2] := GetBValue(StartColor) - StartRGB[2];
      end;
  end;
  ColorBand := Rect;
  Colors := $FF;
  if Direction in [drDown, drUp] then
  begin
    Colors := Max(2, Min(Colors, HeightOf(Rect)));
    Delta := HeightOf(Rect) div Colors;
  end
  else
  begin
    Colors := Max(2, Min(Colors, WidthOf(Rect)));
    Delta := WidthOf(Rect) div Colors;
  end;
  if Delta > 0 then
    for I := 0 to Colors - 1 do
    begin
      case Direction of
        drDown, drUp:
          begin
            ColorBand.Top := Rect.Top + I * Delta;
            ColorBand.Bottom := ColorBand.Top + Delta;
          end;
        drRight, drLeft:
          begin
            ColorBand.Left := Rect.Left + I * Delta;
            ColorBand.Right := ColorBand.Left + Delta;
          end;
      end;
      Brush := CreateSolidBrush(RGB(
      StartRGB[0] + MulDiv(I, DeltaRGB[0], Colors - 1),
      StartRGB[1] + MulDiv(I, DeltaRGB[1], Colors - 1),
      StartRGB[2] + MulDiv(I, DeltaRGB[2], Colors - 1)));
      FillRect(DC, ColorBand, Brush);
      DeleteObject(Brush);
    end;
  if Direction in [drDown, drUp] then
    Delta := HeightOf(Rect) mod Colors
  else
    Delta := WidthOf(Rect) mod Colors;
  if Delta > 0 then
  begin
    case Direction of
      drDown, drUp:
        begin
          ColorBand.Top := Rect.Bottom - Delta;
          ColorBand.Bottom := ColorBand.Top + Delta;
        end;
      drRight, drLeft:
        begin
          ColorBand.Left := Rect.Right - Delta;
          ColorBand.Right := ColorBand.Left + Delta;
        end;
    end;
    case Direction of
      drDown, drRight:
          Brush := CreateSolidBrush(EndColor);
        else
          Brush := CreateSolidBrush(StartColor);
    end;
    FillRect(DC, ColorBand, Brush);
    DeleteObject(Brush);
  end;
end;

procedure DrawGradientSplit(DC: HDC; Rect: TRect; C1, C2, C3, C4: TColor;
  Direction: TDirection);
var
  R: TRect;
begin
  case Direction of
    drLeft:
      begin
        R := Rect;
        R.Right := R.Left + WidthOf(Rect) div 2;
        DrawGradient(DC, R, C3, C4, Direction);
        R.Left := R.Right;
        R.Right := Rect.Right;
        DrawGradient(DC, R, C1, C2, Direction);
      end;
    drRight:
      begin
        R := Rect;
        R.Right := R.Left + WidthOf(Rect) div 2;
        DrawGradient(DC, R, C1, C2, Direction);
        R.Left := R.Right;
        R.Right := rect.Right;
        DrawGradient(DC, R, C3, C4, Direction);
      end;
    drUp:
      begin
        R := Rect;
        R.Bottom := R.Top + HeightOf(Rect) div 2;
        DrawGradient(DC, R, C3, C4, Direction);
        R.Top := R.Bottom;
        R.Bottom := Rect.Bottom;
        DrawGradient(DC, R, C1, C2, Direction);
      end;
    drDown:
      begin
        R := Rect;
        R.Bottom := R.Top + HeightOf(Rect) div 2;
        DrawGradient(DC, R, C1, C2, Direction);
        R.Top := R.Bottom;
        R.Bottom := Rect.Bottom;
        DrawGradient(DC, R, C3, C4, Direction);
      end;
  end;
end;

procedure DrawInfoBox(DC: HDC; const Rect: TRect);
begin
  FillRoundRectFancy(DC, Rect, 20, $BFF1FF, clWindow, drDown,
    clBtnShadow, $BFF1FF, psSolid, bsFDiagonal);
end;

type
  TDrawGripData = record
    DC: HDC;
    Rect: TRect;
    Clipped: Boolean;
  end;
  PDrawGripData = ^TDrawGripData;

procedure DrawGripProc(DC: HDC; Width, Height: Integer; Data: PDrawGripData);
const
  RasterOps: array[Boolean] of DWORD = (SRCCOPY, SRCAND);
var
  PriorBrush: HBRUSH;
  PriorPen: HPEN;
  P: TPolygon;
  I: Integer;
begin
  PriorBrush := SelectObject(DC, GetStockObject(WHITE_BRUSH));
  PriorPen := SelectObject(DC, GetStockObject(WHITE_PEN));
  if Data.Clipped then
  begin
    BitBlt(DC, 0, 0, Width, Height, 0, 0, 0, BLACKNESS);
    SetLength(P, 3);
    P[0].X:= 0;
    P[0].X:= Height;
    P[1].X:= Width;
    P[1].Y := 0;
    P[2].X:= Width;
    P[2].Y := Height;
    DrawPolygon(DC, P);
    with Data.Rect do
      BitBlt(Data.DC, Right - Width, Bottom - Height, Width, Height, DC, 0, 0,
        SRCPAINT);
    BitBlt(DC, 0, 0, Width, Height, 0, 0, 0, WHITENESS);
  end
  else
    FillRect(DC, GetRect(0, 0, Width, Height), COLOR_BTNFACE + 1);
  SelectObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNHIGHLIGHT)));
  for I := 0 to (Width div 4) - 1 do
  begin
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNHIGHLIGHT)));
    MoveToEx(DC, Width - (I * 4) - 5, Height, nil);
    LineTo(DC, Width, Height - (I * 4) - 5);
    OverwriteObject(DC, CreatePen(PS_SOLID, 2, GetSysColor(COLOR_BTNSHADOW)));
    MoveToEx(DC, Width - (I * 4) - 3, Height, nil);
    LineTo(DC, Width, Height - (I * 4) - 3);
    OverwriteObject(DC, CreatePen(PS_SOLID, 0, GetSysColor(COLOR_BTNFACE)));
    MoveToEx(DC, Width - (I * 4) - 2, Height, nil);
    LineTo(DC, Width, Height - (I * 4) - 2);
  end;
  OverwriteObject(DC, PriorPen);
  SelectObject(DC, PriorBrush);
  with Data.Rect do
    BitBlt(Data.DC, Right - Width, Bottom - Height, Width, Height, DC, 0, 0,
      RasterOps[Data.Clipped]);
end;

procedure DrawGrip(DC: HDC; Rect: TRect; Clipped: Boolean = True);
var
  Data: TDrawGripData;
begin
  Data.DC := DC;
  Data.Rect := Rect;
  Data.Clipped := Clipped;
  PaintBitmap(GetSystemMetrics(SM_CXHSCROLL), GetSystemMetrics(SM_CXVSCROLL),
    @Data, @DrawGripProc);
end;

procedure DrawNode(DC: HDC; Rect: TRect; Expanded: Boolean; SolidColor: TColor = cl3DDkShadow);
var
  PriorPen: HPEN;
  PriorPoint: TPoint;
begin
  with Rect do
  begin
    Left := Left + (Right - Left - 9) div 2;
    Rect := GetRect(Left, Top + (Bottom - Top - 9) div 2, Left + 8, 0);
  end;
  Rect.Bottom := Rect.Top + 8;
  PriorPen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(SolidColor)));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @PriorPoint);
    LineTo(DC, Right, Top);
    LineTo(DC, Right, Bottom);
    LineTo(DC, Left, Bottom);
    LineTo(DC, Left, Top);
    if SolidColor <> cl3DDkShadow then
      OverwriteObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(SolidColor)))
    else
      OverwriteObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(clWindowText)));
    MoveToEx(DC, Left + 2, Top + 4, nil);
    LineTo(DC, Right - 1, Top + 4);
    if not Expanded then
    begin
      MoveToEx(DC, Left + 4, Top + 2, nil);
      LineTo(DC, Left + 4, Bottom - 1);
    end;
  end;
  with PriorPoint do
    MoveToEx(DC, x, y, nil);
  OverwriteObject(DC, PriorPen);
end;

procedure DrawToolGrip(DC: HDC; const Rect: TRect);
const
  GripSize = 2;
var
  R: TRect;
begin
  R := Rect;
  if HeightOf(Rect) < WidthOf(Rect) then
  begin
    R.Left := Rect.Left + GripSize;
    R.Right := R.Left + GripSize;
    R.Top := R.Top + (HeightOf(Rect) - (GripSize + 1)) div 2;
    R.Bottom := R.Top + GripSize;
    while R.Right + GripSize * 2 < Rect.Right + 1 do
    begin
      OffsetRect(R, 1, 1);
      FillRect(DC, R, COLOR_BTNHIGHLIGHT + 1);
      OffsetRect(R, -1, -1);
      FillRect(DC, R, COLOR_BTNSHADOW + 1);
      OffsetRect(R, GripSize * 2, 0);
    end;
  end
  else
  begin
    R.Left := R.Left + (WidthOf(Rect) - (GripSize + 1)) div 2;
    R.Right := R.Left + GripSize;
    R.Top := Rect.Top + GripSize;
    R.Bottom := R.Top + GripSize;
    while R.Bottom + GripSize * 2 < Rect.Bottom + 1 do
    begin
      OffsetRect(R, 1, 1);
      FillRect(DC, R, COLOR_BTNHIGHLIGHT + 1);
      OffsetRect(R, -1, -1);
      FillRect(DC, R, COLOR_BTNSHADOW + 1);
      OffsetRect(R, 0, GripSize * 2);
    end;
  end;
end;

procedure DrawPolygon(DC: HDC; const P: TFloatPolygon);
var
  PriorDC: HDC;
begin
  PriorDC := InitializeDevice(DC);
  DrawPolygon(DC, DeviceToPolygon(P));
  FinalizeDevice(PriorDC);
end;

procedure DrawPolygon(DC: HDC; const P: TPolygon);
begin
  Polygon(DC, Pointer(P)^, Length(P));
end;

procedure DrawAngledText(DC: HDC; const S: string; const Angle: Double;
  const Point: TPoint);
var
  Font: HFONT;
  LogFont: TLogFont;
  PriorMode: Integer;
begin
  Font := GetCurrentObject(DC, OBJ_FONT);
  GetObject(Font, SizeOf(TLogFont), @LogFont);
  LogFont.lfEscapement := -Round(Angle * 10);
  LogFont.lfQuality := CLEARTYPE_QUALITY or PROOF_QUALITY;
  SelectObject(DC, CreateFontIndirect(LogFont));
  PriorMode := SetBkMode(DC, TRANSPARENT);
  TextOut(DC, Point.X, Point.Y, PChar(S), Length(S));
  SetBkMode(DC, PriorMode);
  OverwriteObject(DC, Font);
end;

procedure DrawRect(DC: HDC; const Rect: TRect; Color: TColor);
var
  Brush: HBRUSH;
begin
  Brush := CreateSolidBrush(ColorToRGB(Color));
  FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
end;

procedure DrawRectOutline(DC: HDC; const Rect: TRect; Color: TColor);
var
  Pen: HBRUSH;
  Point: TPoint;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  with Rect do
  begin
    MoveToEx(DC, Left, Top, @Point);
    LineTo(DC, Right - 1, Top);
    LineTo(DC, Right - 1, Bottom - 1);
    LineTo(DC, Left, Bottom - 1);
    LineTo(DC, Left, Top);
    MoveToEx(DC, Point.x, Point.y, nil);
  end;
  DeleteObject(SelectObject(DC, Pen));
end;

procedure DrawRectEdge(DC: HDC; const Rect: TRect; Color: TColor; Direction: TDirection);
var
  P: HPEN;
begin
  if Direction in [drLeft..drDown] then
  begin
    P := SelectObject(DC, GetPen(Color));
    with Rect do
      case Direction of
        drLeft:
          begin
            MoveTo(DC, Left, Top);
            LineTo(DC, Left, Bottom);
          end;
        drUp:
          begin
            MoveTo(DC, Left, Top);
            LineTo(DC, Right - 1, Top);
          end;
        drRight:
          begin
            MoveTo(DC, Right - 1, Top);
            LineTo(DC, Right - 1, Bottom);
          end;
        drDown:
          begin
            MoveTo(DC, Left, Bottom);
            LineTo(DC, Right - 1, Bottom);
          end;
    end;
    OverwriteObject(DC, P);
  end;
end;

procedure DrawBorder(DC: HDC; const Rect: TRect);
begin
  if ThemePainter.Enabled then
    DrawRectOutline(DC, Rect, clThemeBorder)
  else
    DrawFrame(DC, Rect, dfSunken);
end;

procedure DrawDashedRect(DC: HDC; const Rect: TRect; Pen, Brush: TColor);
var
	P: HPEN;
  B: HBRUSH;
begin
	P := SelectObject(DC, GetPen(Pen, psDot));
	B := SelectObject(DC, GetBrush(Brush));
  with Rect do
	  Rectangle(DC, Left, Top, Right, Bottom);
	OverwriteObject(DC, P);
	OverwriteObject(DC, B);
end;

procedure DrawSlantRect(DC: HDC; const Rect: TRect; const A, B: TPoint);
var
  Rectangle: TRectangle;
begin
  Rectangle := Rotate(Rect, A, B);
  Polygon(DC, Rectangle, 4);
end;

procedure DrawSlantText(DC: HDC; const S: string; const A, B: TPoint);
var
  SlantAngle: Double;
  Font: HFONT;
  LogFont: TLogFont;
  Rectangle: TRectangle;
  PriorMode: Integer;
begin
  SlantAngle := Angle(A, B);
  Font := GetCurrentObject(DC, OBJ_FONT);
  GetObject(Font, SizeOf(TLogFont), @LogFont);
  LogFont.lfEscapement := -Round(SlantAngle * 10);
  LogFont.lfQuality := CLEARTYPE_QUALITY or PROOF_QUALITY;
  SelectObject(DC, CreateFontIndirect(LogFont));
  with CalcCaptionSize(DC, S) do
    Rectangle := Rotate(GetRect(0, 0, cx, cy), A, B);
  PriorMode := SetBkMode(DC, TRANSPARENT);
  TextOut(DC, Rectangle[2].x, Rectangle[2].Y, PChar(S), Length(S));
  SetBkMode(DC, PriorMode);
  OverwriteObject(DC, Font);
end;

procedure DrawBevelRect(DC: HDC; Rect: TRect; Bevel: Integer; Color: TColor);
var
  Pen: HPEN;
  Point: TPoint;
begin
	{ BeginPath(DC); }
  Pen := SelectObject(DC, GetPen(Color));
  with Rect do
  begin
    MoveToEx(DC, Left, Top + Bevel, @Point);
    LineTo(DC, Left + Bevel, Top);
    LineTo(DC, Right - Bevel - 1, Top);
    LineTo(DC, Right - 1, Top + Bevel);
    LineTo(DC, Right - 1, Bottom - Bevel - 1);
    LineTo(DC, Right - Bevel - 1, Bottom - 1);
    LineTo(DC, Left + Bevel, Bottom - 1);
    LineTo(DC, Left, Bottom - Bevel - 1);
    LineTo(DC, Left, Top + Bevel);
    MoveToEx(DC, Point.x, Point.y, nil);
  end;
  OverwriteObject(DC, Pen);
	{ EndPath(DC);
    if Solid then
	    StrokeAndFillPath(DC)
		else
	    StrokePath(DC); }
end;

{ DrawBubble }

procedure DrawBubble(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; ForeColor, BackColor: TColor);
var
  BubbleArea: array[0..7] of TPoint;
  Pen: HPEN;
  Brush: HBRUSH;
  Rgn: HRGN;
begin
  with Rect do
  begin
    BubbleArea[0].X := Left;
    BubbleArea[0].Y := Top + Bevel;
    BubbleArea[1].X := Left + Bevel;
    BubbleArea[1].Y := Top;
    BubbleArea[2].X := Right - Bevel - 1;
    BubbleArea[2].Y := Top;
    BubbleArea[3].X := Right - 1;
    BubbleArea[3].Y := Top + Bevel;
    BubbleArea[4].X := Right - 1;
    BubbleArea[4].Y := Bottom - Bevel - 1;
    BubbleArea[5].X := Right - Bevel - 1;
    BubbleArea[5].Y := Bottom - 1;
    BubbleArea[6].X := Left + Bevel;
    BubbleArea[6].Y := Bottom - 1;
    BubbleArea[7].X := Left;
    BubbleArea[7].Y := Bottom - Bevel - 1;
  end;
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(ForeColor)));
  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(BackColor)));
  Polygon(DC, BubbleArea, 8);
  OverwriteObject(DC, Brush);
  OverwriteObject(DC, Pen);
  InflateRect(Rect, -2, 0);
  DrawCaption(DC, Caption, Rect, Direction);
  Rgn := CreatePolygonRgn(BubbleArea, 8, WINDING);
  OffsetRgn(Rgn, 1, 1);
  ExtSelectClipRgn(DC, Rgn, RGN_DIFF);
  DeleteObject(Rgn);
end;

{ DrawCapsuleAdvanced }

procedure DrawCapsuleAdvanced(DC: HDC; const Caption: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer;
  CapsuleProc: TCapsuleProc; ForeColor, BackColor: TColor; Data: Pointer);
var
  CapsuleArea: array[0..7] of TPoint;
  CaptionArea, BodyArea: array[0..5] of TPoint;

  procedure BuildAreas;
  begin
    with Rect do
    begin
      CapsuleArea[0].X := Left;
      CapsuleArea[0].Y := Top + Bevel;
      CapsuleArea[1].X := Left + Bevel;
      CapsuleArea[1].Y := Top;
      CapsuleArea[2].X := Right - Bevel - 1;
      CapsuleArea[2].Y := Top;
      CapsuleArea[3].X := Right - 1;
      CapsuleArea[3].Y := Top + Bevel;
      CapsuleArea[4].X := Right - 1;
      CapsuleArea[4].Y := Bottom - Bevel - 1;
      CapsuleArea[5].X := Right - Bevel - 1;
      CapsuleArea[5].Y := Bottom - 1;
      CapsuleArea[6].X := Left + Bevel;
      CapsuleArea[6].Y := Bottom - 1;
      CapsuleArea[7].X := Left;
      CapsuleArea[7].Y := Bottom - Bevel - 1;
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[1] := CapsuleArea[1];
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[2].X := CaptionArea[0].X + Width;
      CaptionArea[2].Y := CapsuleArea[1].Y;
      CaptionArea[3].X := CapsuleArea[7].X + Width;
      CaptionArea[3].Y := CapsuleArea[6].Y;
      CaptionArea[4] := CapsuleArea[6];
      CaptionArea[5] := CapsuleArea[7];
      BodyArea[0] := CaptionArea[2];
      BodyArea[1] := CapsuleArea[2];
      BodyArea[2] := CapsuleArea[3];
      BodyArea[3] := CapsuleArea[4];
      BodyArea[4] := CapsuleArea[5];
      BodyArea[5] := CaptionArea[3];
    end;
  end;

var
  Pen: HPEN;
  Brush: HBRUSH;
  DrawRect: TRect;
  Rgn: HRGN;
begin
  BuildAreas;
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(ForeColor)));
  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(BackColor)));
  Polygon(DC, CaptionArea, 6);
  Polygon(DC, BodyArea, 6);
  OverwriteObject(DC, Brush);
  OverwriteObject(DC, Pen);
  DrawRect := Rect;
  DrawRect.Right := DrawRect.Left + Width;
  InflateRect(DrawRect, -2, 0);
  DrawCaption(DC, Caption, DrawRect, Direction);
  DrawRect := Rect;
  DrawRect.Left := DrawRect.Left + Width;
  if @CapsuleProc <> nil then
    CapsuleProc(DC, DrawRect, ForeColor, BackColor, Data);
  Rgn := CreatePolygonRgn(CapsuleArea, 8, WINDING);
  OffsetRgn(Rgn, 1, 1);
  ExtSelectClipRgn(DC, Rgn, RGN_DIFF);
  DeleteObject(Rgn);
end;

{ DrawCapsuleText }

type
  TDrawCapsuleTextParams = record
    Text: string;
    Direction: TDirection;
  end;
  PDrawCapsuleTextParams = ^TDrawCapsuleTextParams;

procedure DrawCapsuleTextProc(DC: HDC; Rect: TRect; ForeColor, BackColor: TColor;
  Data: Pointer);
var
  Params: PDrawCapsuleTextParams absolute Data;
begin
  InflateRect(Rect, -4, 0);
  if FieldCount(Params.Text, '|') = 2 then
  begin
    DrawCaption(DC, FieldValue(Params.Text, '|', 1), Rect, drRight);
    DrawCaption(DC, FieldValue(Params.Text, '|', 0), Rect, drLeft);
  end
  else
    DrawCaption(DC, Params.Text, Rect, Params.Direction);
end;

procedure DrawCapsuleText(DC: HDC; const Caption, Body: string; Rect: TRect;
  CaptionDirection, BodyDirection: TDirection; Bevel: Integer; Width: Integer;
  ForeColor, BackColor: TColor);
var
  Params: TDrawCapsuleTextParams;
begin
  Params.Text := Body;
  Params.Direction := BodyDirection;
  DrawCapsuleAdvanced(DC, Caption, Rect, CaptionDirection, Bevel, Width,
    @DrawCapsuleTextProc, ForeColor, BackColor, @Params);
end;

procedure DrawCapsuleCheckBox(DC: HDC; const Text: string; Rect: TRect;
  Direction: TDirection; Bevel: Integer; Width: Integer; Checked: Boolean;
  ForeColor, BackColor: TColor; Flat: Boolean = True);
var
  CapsuleArea: array[0..7] of TPoint;
  CaptionArea: array[0..5] of TPoint;

  procedure BuildAreas;
  begin
    with Rect do
    begin
      CapsuleArea[0].X := Left;
      CapsuleArea[0].Y := Top + Bevel;
      CapsuleArea[1].X := Left + Bevel;
      CapsuleArea[1].Y := Top;
      CapsuleArea[2].X := Right - Bevel - 1;
      CapsuleArea[2].Y := Top;
      CapsuleArea[3].X := Right - 1;
      CapsuleArea[3].Y := Top + Bevel;
      CapsuleArea[4].X := Right - 1;
      CapsuleArea[4].Y := Bottom - Bevel - 1;
      CapsuleArea[5].X := Right - Bevel - 1;
      CapsuleArea[5].Y := Bottom - 1;
      CapsuleArea[6].X := Left + Bevel;
      CapsuleArea[6].Y := Bottom - 1;
      CapsuleArea[7].X := Left;
      CapsuleArea[7].Y := Bottom - Bevel - 1;
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[1] := CapsuleArea[1];
      CaptionArea[0] := CapsuleArea[0];
      CaptionArea[2].X := CaptionArea[0].X + Width;
      CaptionArea[2].Y := CapsuleArea[1].Y;
      CaptionArea[3].X := CapsuleArea[7].X + Width;
      CaptionArea[3].Y := CapsuleArea[6].Y;
      CaptionArea[4] := CapsuleArea[6];
      CaptionArea[5] := CapsuleArea[7];
    end;
  end;

var
  Pen: HPEN;
  Brush: HBRUSH;
  DrawRect: TRect;
  Rgn: HRGN;
begin
  BuildAreas;
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 0, ColorToRGB(ForeColor)));
  Brush := SelectObject(DC, CreateSolidBrush(ColorToRGB(BackColor)));
  Polygon(DC, CapsuleArea, 8);
  Polygon(DC, CaptionArea, 6);
  OverwriteObject(DC, Brush);
  OverwriteObject(DC, Pen);
  DrawRect := Rect;
  DrawRect.Right := DrawRect.Left + Width;
  InflateRect(DrawRect, -2, 0);
  DrawCaption(DC, Text, DrawRect, Direction);
  DrawRect := Rect;
  DrawRect.Left := DrawRect.Left + Width;
  DrawCheckBox(DC, DrawRect, Checked, ForeColor, BackColor, Flat);
  Rgn := CreatePolygonRgn(CapsuleArea, 8, WINDING);
  OffsetRgn(Rgn, 1, 1);
  ExtSelectClipRgn(DC, Rgn, RGN_DIFF);
  DeleteObject(Rgn);
end;

{$IFDEF GDIPLUS}
procedure DrawToolbar(DC: HDC; Rect: TRect);
var
  F: TFastBitmap;
  G: IGdiGraphics;
  R: TRectI;
  B: IGdiBrush;
begin
  F := CreateFastBitmap(1, HeightOf(Rect), pd32);
  R := NewRectI(-1, -1, 2, F.Height + 2);
  if not IsFastBitmap(F) then Exit;
  try
    G := NewGraphics(F.DC);
    B := NewLinearGradientBrush(R, 90, NewColor(clBtnFace), NewColor(Blend(clWindow, clBtnFace, 75)));
    G.FillRectangle(B, R);
    R.Y := R.Height - 4;
    R.Height := 5;
    B := NewLinearGradientBrush(R, 90, NewColor(clBtnShadow), NewColor(clBtnFace));
    G.FillRectangle(B, R);
    BaseTypes.AlphaDraw(DC, Rect, F);
  finally
    DestroyFastBitmap(F);
  end;
end;

procedure DrawMidbar(DC: HDC; const Rect: TRect);
var
  F: TFastBitmap;
  G: IGdiGraphics;
  R: TRectI;
  B: IGdiBrush;
begin
  F := CreateFastBitmap(1, HeightOf(Rect), pd32);
  R := NewRectI(-1, -1, 2, F.Height);
  if not IsFastBitmap(F) then Exit;
  try
    G := NewGraphics(F.DC);
    B := NewLinearGradientBrush(R, 90, NewColor(clBtnFace), NewColor(Blend(clWindow, clBtnFace, 75)));
    G.FillRectangle(B, R);
    R.Y := R.Height - 5;
    R.Height := 5;
    B := NewLinearGradientBrush(NewRectF(R), 90, [NewColor(clBtnShadow), NewColor(clBtnFace), NewColor(clBtnFace)], [0.0, 0.5, 1.0]);
    G.FillRectangle(B, R);
    R := NewRectI(-1, -1, 2, F.Height + 2);
    R.Height := 1;
    B := NewSolidBrush(NewColor(clBtnShadow));
    G.FillRectangle(B, R);
    BaseTypes.AlphaDraw(DC, Rect, F);
  finally
    DestroyFastBitmap(F);
  end;
end;

procedure DrawStatusbar(DC: HDC; const Rect: TRect);
var
  F: TFastBitmap;
  G: IGdiGraphics;
  R: TRectI;
  B: IGdiBrush;
begin
  F := CreateFastBitmap(1, HeightOf(Rect), pd32);
  R := NewRectI(-1, -1, 2, F.Height + 2);
  if not IsFastBitmap(F) then Exit;
  try
    G := NewGraphics(F.DC);
    B := NewLinearGradientBrush(R, 270, NewColor(clBtnFace), NewColor(Blend(clWindow, clBtnFace, 75)));
    G.FillRectangle(B, R);
    R.Height := 3;
    B := NewLinearGradientBrush(R, 270, NewColor(Blend(clBtnShadow, clBtnFace)), NewColor(clBtnFace));
    G.FillRectangle(B, R);
    BaseTypes.AlphaDraw(DC, Rect, F);
  finally
    DestroyFastBitmap(F);
  end;
end;

procedure DrawButtonContainer(DC: HDC; const Rect: TRect; Radius: Single);
var
  F: TFastBitmap;
  R: TRectF;
  G: IGdiGraphics;
  P: IGdiGraphicsPath;
  B: IGdiBrush;
begin
  F := CreateFastBitmap(Rect, pd32);
  if not IsFastBitmap(F) then
    Exit;
  R := NewRectF(WidthOf(Rect), HeightOf(Rect));
  InflateRectF(R, -1, -1);
  G := NewGraphics(F.DC);
  G.Clear(0);
  P := NewRoundedRect(R, Radius);
  B := NewLinearGradientBrush(R, 90, NewColor(Blend(clBtnShadow, clBtnFace, 33)),
    NewColor(Blend(clWindow, clBtnFace, 33)));
  G.FillPath(B, P);
  P.Widen(NewPen(NewColor(0)), nil);
  R.Height := R.Height + 1;
  B := NewLinearGradientBrush(R, 90, NewColor(clBtnShadow), NewColor(clWindow));
  G.FillPath(B, P);
  AlphaDraw(DC, Rect, F);
  DestroyFastBitmap(F);
end;
{$ENDIF}

function ImageState(State: TDrawState): TImageState;
begin
  Result := isNormal;
  if dsDisabled in State then
    Result := isDisabled
  else if dsHot in State then
    if dsPressed in State then
      Result := isPressed
    else
      Result := isHot;
end;

procedure BlitInvert(DC: HDC; const Rect: TRect);
begin
	with Rect do
		BitBlt(DC, Left, Top, Right - Left, Bottom - Top, 0, 0, 0, DSTINVERT);
end;

procedure BlitAnd(Dest: HDC; const Rect: TRect; Source: HDC);
begin
	with Rect do
		BitBlt(Dest, Left, Top, Right - Left, Bottom - Top, Source, 0, 0, SRCAND);
end;

procedure BlitOr(Dest: HDC; const Rect: TRect; Source: HDC);
begin
	with Rect do
		BitBlt(Dest, Left, Top, Right - Left, Bottom - Top, Source, 0, 0, SRCPAINT);
end;

var
	InternalGlyphs: array[TGlyphKind] of TBitmap;

const
	GlyphBase = 1001;
  GlyphDim = 12;
  GlyphHalf = GlyphDim div 2;
  GlyphBits = GlyphDim * GlyphDim;

function GlyphFind(Kind: TGlyphKind): TBitmap;
begin
  if InternalGlyphs[Kind] = nil then
	  InternalGlyphs[Kind] := GetBitmap(GlyphBase + Ord(Kind));
  Result := InternalGlyphs[Kind];
end;

procedure GlyphBlendBlt(DC: HDC; Glyph: TGlyphKind; X, Y: Integer; Color: TColor);
begin
  GlyphBlendBlt(DC, GlyphFind(Glyph), X, Y, Color);
end;

procedure GlyphBlendBlt(DC: HDC; Glyph: TBitmap; X, Y: Integer; Color: TColor);
var
  RGB: TRGBQuad absolute Color;
  S: TRGBTriple absolute Color;
  T: TRGBTriple;
  A, B: TFastBitmap;
  C, D: PRGBTriple;
  I: Integer;
begin
  Color := ColorToRGB(Color);
  I := S.rgbtBlue;
  S.rgbtBlue := S.rgbtRed;
  S.rgbtRed := I;
  T.rgbtBlue := S.rgbtBlue div 3;
  T.rgbtGreen := S.rgbtGreen div 3;
  T.rgbtRed := S.rgbtRed div 3;
  A := CreateFastBitmap(Glyph.Width, Glyph.Height);
  B := CreateFastBitmap(Glyph.Width, Glyph.Height);
  try
    BitBlt(A.DC, 0, 0, Glyph.Width, Glyph.Height, DC, X, Y, SRCCOPY);
    BitBlt(B.DC, 0, 0, Glyph.Width, Glyph.Height, Glyph.Canvas.Handle, 0, 0, SRCCOPY);
    C := A.Bits;
    D := B.Bits;
    for I := 0 to Glyph.Width * Glyph.Height - 1 do
    begin
      case D.rgbtBlue of
        0: C^ := S;
        128:
          begin
            C.rgbtBlue := (C.rgbtBlue + S.rgbtBlue) shr 1;
            C.rgbtGreen := (C.rgbtGreen + S.rgbtGreen) shr 1;
            C.rgbtRed := (C.rgbtRed + S.rgbtRed) shr 1;
          end;
        192:
          begin
            C.rgbtBlue := (C.rgbtBlue div 3) shl 1 + T.rgbtBlue;
            C.rgbtGreen := (C.rgbtGreen div 3) shl 1 + T.rgbtGreen;
            C.rgbtRed := (C.rgbtRed div 3) shl 1  + T.rgbtRed;
          end;
      end;
      Inc(C);
      Inc(D);
    end;
    BitBlt(DC, X, Y, Glyph.Width, Glyph.Height, A.DC, 0, 0, SRCCOPY);
  finally
    DestroyFastBitmap(A);
    DestroyFastBitmap(B);
  end;
end;

procedure GlyphDraw(DC: HDC; const Rect: TRect; Glyph: TGlyphKind; Color: TColor);
var
	H, W: Integer;
begin
  H := Rect.Bottom - Rect.Top;
  W := Rect.Right - Rect.Left;
  with Rect do
		GlyphBlendBlt(DC, Glyph, Left + (W - GlyphDim) div 2,
      Top + (H - GlyphDim) div 2, Color);
end;

procedure GlyphFrame(DC: HDC; Rect: TRect; Glyph: TGlyphKind; State: TDrawState;
  Color: TColor);
var
	X, Y, H, W: Integer;
  Pen: HPEN;
  Brush: HBRUSH;
begin
	X := GlyphDim;
  Y := GlyphDim;
  H := Rect.Bottom - Rect.Top;
  W := Rect.Right - Rect.Left;
  with Rect do
  begin
		Left := Left + (W - X) div 2;
    Top := Top + (H - Y) div 2;
    Right := Left + X + 1;
    Bottom := Top + Y;
 	end;
  if dsPressed in State then
  	OffsetRect(Rect, 1, 1);
	GlyphBlendBlt(DC, Glyph, Rect.Left, Rect.Top, Color);
  if dsFlat in State then Exit;
	InflateRect(Rect, 2, 2);
  if dsPressed in State then
  begin
  	OffsetRect(Rect, -1, -1);
    Inc(Rect.Right);
    Inc(Rect.Bottom);
    if dsThin in State then
	  	DrawFrame(DC, Rect, dfLowered)
    else
      DrawFrame(DC, Rect, dfSunken);

  end
  else if not (dsThin in State) then
  begin
    Pen := SelectObject(DC, CreatePen(PS_SOLID, 2, ColorToRGB(Color)));
    Brush := SelectObject(DC, GetStockObject(HOLLOW_BRUSH));
    with Rect do
    	Rectangle(DC, Left, Top, Right, Bottom);
		OverwriteObject(DC, Pen);
    SelectObject(DC, Brush);
  end;
end;

var
  InternalThemePainter: TThemePainter;

function ThemePainter: TThemePainter;
begin
  if InternalThemePainter = nil then
    InternalThemePainter := TThemePainter.Create;
  Result := InternalThemePainter;
end;

constructor TThemePainter.Create;
begin
	FWindow := TUtilityWindow.Create(Self);
  FAvailable := ThemesLoaded;
  UpdateThemes;
end;

destructor TThemePainter.Destroy;
begin
  UnloadThemeData;
	FWindow.Free;
  inherited;
end;

function TThemePainter.GetTheme(Element: TThemedElement): HTHEME;
begin
  if FAvailable and (FThemeData[Element] = 0) then
    FThemeData[Element] := OpenThemeData(FWindow.Handle, ThemeDataNames[Element]);
  Result := FThemeData[Element];
end;

function TThemePainter.GetEnabled: Boolean;
begin
  Result := FAvailable and FControlsEnabled;
end;

function EnumThemesChildProc(Wnd: HWND; lParam: Integer): BOOL; stdcall;
begin
  SendMessage(Wnd, WM_THEMECHANGED, 0, 0);
  if IsWindowVisible(Wnd) then
  begin
    ShowWindow(Wnd, SW_HIDE);
    ShowWindow(Wnd, SW_SHOW);
  end;
  Result := True;
end;

function IsProcessWindow(Wnd: HWND): Boolean;
var
  Process: THandle;
begin
  Result := IsWindow(Wnd);
  if Result then
  begin
    GetWindowThreadProcessId(Wnd, @Process);
    Result := Process = GetCurrentProcessID;
  end;
end;

function EnumThemesProc(Wnd: HWND; lParam: Integer): BOOL; stdcall;
begin
  if IsProcessWindow(Wnd) then
  begin
    EnumChildWindows(Wnd, @EnumThemesChildProc, 0);
    InvalidateRect(Wnd, nil, True);
  end;
  Result := True;
end;

procedure TThemePainter.SetEnabled(Value: Boolean);
const
  Flags: array[Boolean] of DWORD = (
    STAP_ALLOW_NONCLIENT, STAP_ALLOW_NONCLIENT or STAP_ALLOW_CONTROLS or STAP_ALLOW_WEBCONTENT);
begin
  if not FAvailable then Exit;
  if Value = Enabled then Exit;
  SetThemeAppProperties(Flags[Value]);
  UpdateThemes;
  EnumWindows(@EnumThemesProc, 0);
end;

procedure TThemePainter.WMThemeChanged(var Message: TMessage);
begin
  UpdateThemes;
	DoOnThemeChange;
end;

procedure TThemePainter.DoOnThemeChange;
begin
	InternalThemeBorder := -1;
  if Assigned(FOnThemeChange) then
    FOnThemeChange(Self);
end;

procedure TThemePainter.UnloadThemeData;
var
  Entry: TThemedElement;
begin
  if (FWindow.Handle = 0) or IsWindow(FWindow.Handle) then
  begin
    for Entry := Low(TThemeData) to High(TThemeData) do
      if FThemeData[Entry] <> 0 then
      begin
        CloseThemeData(FThemeData[Entry]);
        FThemeData[Entry] := 0;
      end;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedButton): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teButton;
  with Result do
  begin
    case Widget of
      tbPushButtonNormal..tbPushButtonDefaulted:
        begin
          Part := BP_PUSHBUTTON;
          Base := Ord(tbPushButtonNormal);
        end;
      tbRadioButtonUncheckedNormal..tbRadioButtonCheckedDisabled:
        begin
          Part := BP_RADIOBUTTON;
          Base := Ord(tbRadioButtonUncheckedNormal);
        end;
      tbCheckBoxUncheckedNormal..tbCheckBoxMixedDisabled:
        begin
          Part := BP_CHECKBOX;
          Base := Ord(tbCheckBoxUncheckedNormal);
        end;
      tbGroupBoxNormal..tbGroupBoxDisabled:
        begin
          Part := BP_GROUPBOX;
          Base := Ord(tbGroupBoxNormal);
        end;
      tbUserButton:
        begin
          Part := BP_USERBUTTON;
          Base := Ord(tbUserButton);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedClock): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teClock;
  with Result do
  begin
    case Widget of
      tcTimeNormal:
        begin
          Part := CLP_TIME;
          Base := Ord(tcTimeNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedComboBox): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teComboBox;
  with Result do
  begin
    case Widget of
      tcDropDownButtonNormal..tcDropDownButtonDisabled:
        begin
          Part := CP_DROPDOWNBUTTON;
          Base := Ord(tcDropDownButtonNormal);
        end;
      tcBackground:
        begin
          Part := CP_BACKGROUND;
          Base:= Ord(tcBackground);
        end;
      tcTransparentBackgroundNormal..tcTransparentBackgroundFocused:
        begin
          Part := CP_TRANSPARENTBACKGROUND;
          Base:= Ord(tcTransparentBackgroundNormal);
        end;
      tcBorderNormal..tcBorderDisabled:
        begin
          Part := CP_BORDER;
          Base:= Ord(tcBorderNormal);
        end;
      tcReadOnlyNormal..tcReadOnlyDisabled:
        begin
          Part := CP_READONLY;
          Base:= Ord(tcReadOnlyNormal);
        end;
      tcDropDownButtonRightNormal..tcDropDownButtonRightDisabled:
        begin
          Part := CP_DROPDOWNBUTTONRIGHT;
          Base:= Ord(tcDropDownButtonRightNormal);
        end;
      tcDropDownButtonLeftNormal..tcDropDownButtonLeftDisabled:
        begin
          Part := CP_DROPDOWNBUTTONLEFT;
          Base:= Ord(tcDropDownButtonLeftNormal);
        end;
      tcCueBannerNormal..tcCueBannerDisabled:
        begin
          Part := CP_CUEBANNER;
          Base:= Ord(tcCueBannerNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedEdit): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teEdit;
  with Result do
  begin
    case Widget of
      teEditTextNormal..teEditTextAssist:
        begin
          Part := EP_EDITTEXT;
          Base := Ord(teEditTextNormal);
        end;
      teEditCaret:
        begin
          Part := EP_CARET;
          Base := Ord(teEditCaret);
        end;
      teBackgroundNormal..teBackgroundAssist:
        begin
          Part := EP_BACKGROUND;
          Base := Ord(teBackgroundNormal);
        end;
      tePassword:
        begin
          Part := EP_PASSWORD;
          Base := Ord(tePassword);
        end;
      teBackgroundWithBorderNormal..teBackgroundWithBorderFocused:
        begin
          Part := EP_BACKGROUNDWITHBORDER;
          Base := Ord(teBackgroundWithBorderNormal);
        end;
      teBorderNoScrollNormal..teBorderNoScrollDisabled:
        begin
          Part := EP_EDITBORDER_NOSCROLL;
          Base := Ord(teBorderNoScrollNormal);
        end;
      teBorderHScrollNormal..teBorderHScrollDisabled:
        begin
          Part := EP_EDITBORDER_HSCROLL;
          Base := Ord(teBorderHScrollNormal);
        end;
      teBorderVScrollNormal..teBorderVScrollDisabled:
        begin
          Part := EP_EDITBORDER_VSCROLL;
          Base := Ord(teBorderVScrollNormal);
        end;
      teBorderHVScrollNormal..teBorderHVScrollDisabled:
        begin
          Part := EP_EDITBORDER_HVSCROLL;
          Base := Ord(teBorderHVScrollNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedExplorerBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teExplorerBar;
  with Result do
  begin
    case Widget of
      tebHeaderBackgroundNormal..tebHeaderBackgroundPressed:
        begin
          Part := EBP_HEADERBACKGROUND;
          Base := Ord(tebHeaderBackgroundNormal);
        end;
      tebHeaderCloseNormal..tebHeaderClosePressed:
        begin
          Part := EBP_HEADERCLOSE;
          Base := Ord(tebHeaderCloseNormal);
        end;
      tebHeaderPinNormal..tebHeaderPinSelectedPressed:
        begin
          Part := EBP_HEADERPIN;
          Base := Ord(tebHeaderPinSelectedNormal);
        end;
      tebIEBarMenuNormal..tebIEBarMenuPressed:
        begin
          Part := EBP_IEBARMENU;
          Base := Ord(tebIEBarMenuNormal);
        end;
      tebNormalGroupBackground:
        begin
          Part := EBP_NORMALGROUPBACKGROUND;
          Base := Ord(tebNormalGroupBackground);
        end;
      tebNormalGroupCollapseNormal..tebNormalGroupCollapsePressed:
        begin
          Part := EBP_NORMALGROUPCOLLAPSE;
          Base := Ord(tebNormalGroupCollapseNormal);
        end;
      tebNormalGroupExpandNormal..tebNormalGroupExpandPressed:
        begin
          Part := EBP_NORMALGROUPEXPAND;
          Base := Ord(tebNormalGroupExpandNormal);
        end;
      tebNormalGroupHead:
        begin
          Part := EBP_NORMALGROUPHEAD;
          Base := Ord(tebNormalGroupHead);
        end;
      tebSpecialGroupBackground:
        begin
          Part := EBP_SPECIALGROUPBACKGROUND;
          Base := Ord(tebSpecialGroupBackground);
        end;
      tebSpecialGroupCollapseSpecial..tebSpecialGroupCollapsePressed:
        begin
          Part := EBP_SPECIALGROUPCOLLAPSE;
          Base := Ord(tebSpecialGroupCollapseSpecial);
        end;
      tebSpecialGroupExpandSpecial..tebSpecialGroupExpandPressed:
        begin
          Part := EBP_SPECIALGROUPEXPAND;
          Base := Ord(tebSpecialGroupExpandSpecial);
        end;
      tebSpecialGroupHead:
        begin
          Part := EBP_SPECIALGROUPHEAD;
          Base := Ord(tebSpecialGroupHead);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedHeader): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teHeader;
  with Result do
  begin
    case Widget of
      thHeaderItemNormal..thHeaderItemPressed:
        begin
          Part := HP_HEADERITEM;
          Base := Ord(thHeaderItemNormal);
        end;
      thHeaderItemLeftNormal..thHeaderItemLeftPressed:
        begin
          Part := HP_HEADERITEMLEFT;
          Base := Ord(thHeaderItemLeftNormal);
        end;
      thHeaderItemRightNormal..thHeaderItemRightPressed:
        begin
          Part := HP_HEADERITEMRIGHT;
          Base := Ord(thHeaderItemRightNormal);
        end;
      thHeaderSortArrowSortedUp..thHeaderSortArrowSortedDown:
        begin
          Part := HP_HEADERSORTARROW;
          Base := Ord(thHeaderSortArrowSortedUp);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedListview): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teListView;
  with Result do
  begin
    case Widget of
      tlListItemNormal..tlListItemSelectedNotFocus:
        begin
          Part := LVP_LISTITEM;
          Base := Ord(tlListItemNormal);
        end;
      tlListGroup:
        begin
          Part := LVP_LISTGROUP;
          Base := Ord(tlListGroup);
        end;
      tlListDetail:
        begin
          Part := LVP_LISTDETAIL;
          Base := Ord(tlListDetail);
        end;
      tlListSortDetail:
        begin
          Part := LVP_LISTSORTEDDETAIL;
          Base := Ord(tlListSortDetail);
        end;
      tlEmptyText:
        begin
          Part := LVP_EMPTYTEXT;
          Base := Ord(tlEmptyText);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedMenu): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teMenu;
  with Result do
  begin
    case Widget of
      tmMenuItemNormal..tmMenuItemDemoted:
        begin
          Part := MP_MENUITEM;
          Base := Ord(tmMenuItemNormal);
        end;
      tmMenuDropDown:
        begin
          Part := MP_MENUDROPDOWN;
          Base := Ord(tmMenuDropDown);
        end;
      tmMenuBarItem:
        begin
          Part := MP_MENUBARITEM;
          Base := Ord(tmMenuBarItem);
        end;
      tmMenuBarDropDown:
        begin
          Part := MP_MENUBARDROPDOWN;
          Base := Ord(tmMenuBarDropDown);
        end;
      tmChevron:
        begin
          Part := MP_CHEVRON;
          Base := Ord(tmChevron);
        end;
      tmSeparator:
        begin
          Part := MP_SEPARATOR;
          Base := Ord(tmSeparator);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedPage): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := tePage;
  with Result do
  begin
    case Widget of
      tpUpNormal..tpUpDisabled:
        begin
          Part := PGRP_UP;
          Base := Ord(tpUpNormal);
        end;
      tpDownNormal..tpDownDisabled:
        begin
          Part := PGRP_DOWN;
          Base := Ord(tpDownNormal);
        end;
      tpUpHorzNormal..tpUpHorzDisabled:
        begin
          Part := PGRP_UPHORZ;
          Base := Ord(tpUpHorzNormal);
        end;
      tpDownHorzNormal..tpDownHorzDisabled:
        begin
          Part := PGRP_DOWNHORZ;
          Base := Ord(tpDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedProgress): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teProgress;
  with Result do
  begin
    case Widget of
      tpBar:
        begin
          Part := PP_BAR;
          Base := Ord(tpBar);
        end;
      tpBarVert:
        begin
          Part := PP_BARVERT;
          Base := Ord(tpBarVert);
        end;
      tpChunk:
        begin
          Part := PP_CHUNK;
          Base := Ord(tpChunk);
        end;
      tpChunkVert:
        begin
          Part := PP_CHUNKVERT;
          Base := Ord(tpChunkVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedRebar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teRebar;
  with Result do
  begin
    case Widget of
      trGripper:
        begin
          Part := RP_GRIPPER;
          Base := Ord(trGripper);
        end;
      trGripperVert:
        begin
          Part := RP_GRIPPERVERT;
          Base := Ord(trGripperVert);
        end;
      trBandNormal..trBandHotChecked:
        begin
          Part := RP_BAND;
          Base := Ord(trBandNormal);
        end;
      trChevronNormal..trChevronDisabled:
        begin
          Part := RP_CHEVRON;
          Base := Ord(trChevronNormal);
        end;
      trChevronVertNormal..trChevronVertDisabled:
        begin
          Part := RP_CHEVRONVERT;
          Base := Ord(trChevronVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedScrollBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teScrollBar;
  with Result do
  begin
    case Widget of
      tsArrowBtnUpNormal..tsArrowBtnRightDisabled:
        begin
          Part := SBP_ARROWBTN;
          Base := Ord(tsArrowBtnUpNormal);
        end;
      tsThumbBtnHorzNormal..tsThumbBtnHorzDisabled:
        begin
          Part := SBP_THUMBBTNHORZ;
          Base := Ord(tsThumbBtnHorzNormal);
        end;
      tsThumbBtnVertNormal..tsThumbBtnVertDisabled:
        begin
          Part := SBP_THUMBBTNVERT;
          Base := Ord(tsThumbBtnVertNormal);
        end;
      tsLowerTrackHorzNormal..tsLowerTrackHorzDisabled:
        begin
          Part := SBP_LOWERTRACKHORZ;
          Base := Ord(tsLowerTrackHorzNormal);
        end;
      tsUpperTrackHorzNormal..tsUpperTrackHorzDisabled:
        begin
          Part := SBP_UPPERTRACKHORZ;
          Base := Ord(tsUpperTrackHorzNormal);
        end;
      tsLowerTrackVertNormal..tsLowerTrackVertDisabled:
        begin
          Part := SBP_LOWERTRACKVERT;
          Base := Ord(tsLowerTrackVertNormal);
        end;
      tsUpperTrackVertNormal..tsUpperTrackVertDisabled:
        begin
          Part := SBP_UPPERTRACKVERT;
          Base := Ord(tsUpperTrackVertNormal);
        end;
      tsGripperHorzNormal..tsGripperHorzDisabled:
        begin
          Part := SBP_GRIPPERHORZ;
          Base := Ord(tsGripperHorzNormal);
        end;
      tsGripperVertNormal..tsGripperVertDisabled:
        begin
          Part := SBP_GRIPPERVERT;
          Base := Ord(tsGripperVertNormal);
        end;
      tsSizeBoxRightAlign..tsSizeBoxLeftAlign:
        begin
          Part := SBP_SIZEBOX;
          Base := Ord(tsSizeBoxRightAlign);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedSpin): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teSpin;
  with Result do
  begin
    case Widget of
      tsUpNormal..tsUpDisabled:
        begin
          Part := SPNP_UP;
          Base := Ord(tsUpNormal);
        end;
      tsDownNormal..tsDownDisabled:
        begin
          Part := SPNP_DOWN;
          Base := Ord(tsDownNormal);
        end;
      tsUpHorzNormal..tsUpHorzDisabled:
        begin
          Part := SPNP_UPHORZ;
          Base := Ord(tsUpHorzNormal);
        end;
      tsDownHorzNormal..tsDownHorzDisabled:
        begin
          Part := SPNP_DOWNHORZ;
          Base := Ord(tsDownHorzNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedStartPanel): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teStartPanel;
  with Result do
  begin
    case Widget of
      tspUserPane:
        begin
          Part := SPP_USERPANE;
          Base := Ord(tspUserPane);
        end;
      tspMorePrograms:
        begin
          Part := SPP_MOREPROGRAMS;
          Base := Ord(tspMorePrograms);
        end;
      tspMoreProgramsArrowNormal..tspMoreProgramsArrowPressed:
        begin
          Part := SPP_MOREPROGRAMSARROW;
          Base := Ord(tspMoreProgramsArrowNormal);
        end;
      tspProgList:
        begin
          Part := SPP_PROGLIST;
          Base := Ord(tspProgList);
        end;
      tspProgListSeparator:
        begin
          Part := SPP_PROGLISTSEPARATOR;
          Base := Ord(tspProgListSeparator);
        end;
      tspPlacesList:
        begin
          Part := SPP_PLACESLIST;
          Base := Ord(tspPlacesList);
        end;
      tspPlacesListSeparator:
        begin
          Part := SPP_PLACESLISTSEPARATOR;
          Base := Ord(tspPlacesListSeparator);
        end;
      tspLogOff:
        begin
          Part := SPP_LOGOFF;
          Base := Ord(tspLogOff);
        end;
      tspLogOffButtonsNormal..tspLogOffButtonsPressed:
        begin
          Part := SPP_LOGOFFBUTTONS;
          Base := Ord(tspLogOffButtonsNormal);
        end;
      tspUserPicture:
        begin
          Part := SPP_USERPICTURE;
          Base := Ord(tspUserPicture);
        end;
      tspPreview:
        begin
          Part := SPP_PREVIEW;
          Base := Ord(tspPreview);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedStatus): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teStatus;
  with Result do
  begin
    case Widget of
      tsPane:
        begin
          Part := SP_PANE;
          Base := Ord(tsPane);
        end;
      tsGripperPane:
        begin
          Part := SP_GRIPPERPANE;
          Base := Ord(tsGripperPane);
        end;
      tsGripper:
        begin
          Part := SP_GRIPPER;
          Base := Ord(tsGripper);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTab): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTab;
  with Result do
  begin
    case Widget of
      ttTabItemNormal..ttTabItemFocused:
        begin
          Part := TABP_TABITEM;
          Base := Ord(ttTabItemNormal);
        end;
      ttTabItemLeftEdgeNormal..ttTabItemLeftEdgeFocused:
        begin
          Part := TABP_TABITEMLEFTEDGE;
          Base := Ord(ttTabItemLeftEdgeNormal);
        end;
      ttTabItemRightEdgeNormal..ttTabItemRightEdgeFocused:
        begin
          Part := TABP_TABITEMRIGHTEDGE;
          Base := Ord(ttTabItemRightEdgeNormal);
        end;
      ttTabItemBothEdgeNormal..ttTabItemBothEdgeFocused:
        begin
          Part := TABP_TABITEMBOTHEDGE;
          Base := Ord(ttTabItemBothEdgeNormal);
        end;
      ttTopTabItemNormal..ttTopTabItemFocused:
        begin
          Part := TABP_TOPTABITEM;
          Base := Ord(ttTopTabItemNormal);
        end;
      ttTopTabItemLeftEdgeNormal..ttTopTabItemLeftEdgeFocused:
        begin
          Part := TABP_TOPTABITEMLEFTEDGE;
          Base := Ord(ttTopTabItemLeftEdgeNormal);
        end;
      ttTopTabItemRightEdgeNormal..ttTopTabItemRightEdgeFocused:
        begin
          Part := TABP_TOPTABITEMRIGHTEDGE;
          Base := Ord(ttTopTabItemRightEdgeNormal);
        end;
      ttTopTabItemBothEdgeNormal..ttTopTabItemBothEdgeFocused:
        begin
          Part := TABP_TOPTABITEMBOTHEDGE;
          Base := Ord(ttTopTabItemBothEdgeNormal);
        end;
      ttPane:
        begin
          Part := TABP_PANE;
          Base := Ord(ttPane);
        end;
      ttBody:
        begin
          Part := TABP_BODY;
          Base := Ord(ttBody);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTaskBand): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTaskBand;
  with Result do
  begin
    case Widget of
      ttbGroupCount:
        begin
          Part := TDP_GROUPCOUNT;
          Base := Ord(ttbGroupCount);
        end;
      ttbFlashButton:
        begin
          Part := TDP_FLASHBUTTON;
          Base := Ord(ttbFlashButton);
        end;
      ttpFlashButtonGroupMenu:
        begin
          Part := TDP_FLASHBUTTONGROUPMENU;
          Base := Ord(ttpFlashButtonGroupMenu);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTaskBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTaskBar;
  with Result do
  begin
    case Widget of
      ttbTimeNormal:
        begin
          Part := CLP_TIME;
          Base := Ord(ttbTimeNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedToolBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teToolBar;
  with Result do
  begin
    case Widget of
      ttbButtonNormal..ttbButtonCheckedHot:
        begin
          Part := TP_BUTTON;
          Base := Ord(ttbButtonNormal);
        end;
      ttbDropDownButtonNormal..ttbDropDownButtonCheckedHot:
        begin
          Part := TP_DROPDOWNBUTTON;
          Base := Ord(ttbDropDownButtonNormal);
        end;
      ttbSplitButtonNormal..ttbSplitButtonCheckedHot:
        begin
          Part := TP_SPLITBUTTON;
          Base := Ord(ttbSplitButtonNormal);
        end;
      ttbSplitButtonDropDownNormal..ttbSplitButtonDropDownCheckedHot:
        begin
          Part := TP_SPLITBUTTONDROPDOWN;
          Base := Ord(ttbSplitButtonDropDownNormal);
        end;
      ttbSeparatorNormal..ttbSeparatorCheckedHot:
        begin
          Part := TP_SEPARATOR;
          Base := Ord(ttbSeparatorNormal);
        end;
      ttbSeparatorVertNormal..ttbSeparatorVertCheckedHot:
        begin
          Part := TP_SEPARATORVERT;
          Base := Ord(ttbSeparatorVertNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedToolTip): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teToolTip;
  with Result do
  begin
    case Widget of
      tttStandardNormal..tttStandardLink:
        begin
          Part := TTP_STANDARD;
          Base := Ord(tttStandardNormal);
        end;
      tttStandardTitleNormal..tttStandardTitleLink:
        begin
          Part := TTP_STANDARDTITLE;
          Base := Ord(tttStandardTitleNormal);
        end;
      tttBaloonNormal..tttBaloonLink:
        begin
          Part := TTP_BALLOON;
          Base := Ord(tttBaloonNormal);
        end;
      tttBaloonTitleNormal..tttBaloonTitleLink:
        begin
          Part := TTP_BALLOONTITLE;
          Base := Ord(tttBaloonTitleNormal);
        end;
      tttCloseNormal..tttClosePressed:
        begin
          Part := TTP_CLOSE;
          Base := Ord(tttCloseNormal);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTrackBar): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTrackBar;
  with Result do
  begin
    case Widget of
      ttbTrack:
        begin
          Part := TKP_TRACK;
          Base := Ord(ttbTrack);
        end;
      ttbTrackVert:
        begin
          Part := TKP_TRACKVERT;
          Base := Ord(ttbTrackVert);
        end;
      ttbThumbNormal..ttbThumbDisabled:
        begin
          Part := TKP_THUMB;
          Base := Ord(ttbThumbNormal);
        end;
      ttbThumbBottomNormal..ttbThumbBottomDisabled:
        begin
          Part := TKP_THUMBBOTTOM;
          Base := Ord(ttbThumbBottomNormal);
        end;
      ttbThumbTopNormal..ttbThumbTopDisabled:
        begin
          Part := TKP_THUMBTOP;
          Base := Ord(ttbThumbTopNormal);
        end;
      ttbThumbVertNormal..ttbThumbVertDisabled:
        begin
          Part := TKP_THUMBVERT;
          Base := Ord(ttbThumbVertNormal);
        end;
      ttbThumbLeftNormal..ttbThumbLeftDisabled:
        begin
          Part := TKP_THUMBLEFT;
          Base := Ord(ttbThumbLeftNormal);
        end;
      ttbThumbRightNormal..ttbThumbRightDisabled:
        begin
          Part := TKP_THUMBRIGHT;
          Base := Ord(ttbThumbRightNormal);
        end;
      ttbThumbTics:
        begin
          Part := TKP_TICS;
          Base := Ord(ttbThumbTics);
        end;
      ttbThumbTicsVert:
        begin
          Part := TKP_TICSVERT;
          Base := Ord(ttbThumbTicsVert);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTrayNotify): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTrayNotify;
  with Result do
  begin
    case Widget of
      ttnBackground:
        begin
          Part := TNP_BACKGROUND;
          Base := Ord(ttnBackground);
        end;
      ttnAnimBackground:
        begin
          Part := TNP_ANIMBACKGROUND;
          Base := Ord(ttnAnimBackground);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedTreeView): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teTreeView;
  with Result do
  begin
    case Widget of
      ttItemNormal..ttItemSelectedNotFocus:
        begin
          Part := TVP_TREEITEM;
          Base := Ord(ttItemNormal);
        end;
      ttGlyphClosed..ttGlyphOpened:
        begin
          Part := TVP_GLYPH;
          Base := Ord(ttGlyphClosed);
        end;
      ttBranch:
        begin
          Part := TVP_BRANCH;
          Base := Ord(ttBranch);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.GetDetails(Widget: TThemedWindow): TThemedDetails;
var
  Base: Integer;
begin
  Result.Element := teWindow;
  with Result do
  begin
    case Widget of
      twCaptionActive..twCaptionDisabled:
        begin
          Part := WP_CAPTION;
          Base := Ord(twCaptionActive);
        end;
      twSmallCaptionActive..twSmallCaptionDisabled:
        begin
          Part := WP_SMALLCAPTION;
          Base := Ord(twSmallCaptionActive);
        end;
      twMinCaptionActive..twMinCaptionDisabled:
        begin
          Part := WP_MINCAPTION;
          Base := Ord(twMinCaptionActive);
        end;
      twSmallMinCaptionActive..twSmallMinCaptionDisabled:
        begin
          Part := WP_SMALLMINCAPTION;
          Base := Ord(twSmallMinCaptionActive);
        end;
      twMaxCaptionActive..twMaxCaptionDisabled:
        begin
          Part := WP_MAXCAPTION;
          Base := Ord(twMaxCaptionActive);
        end;
      twSmallMaxCaptionActive..twSmallMaxCaptionDisabled:
        begin
          Part := WP_SMALLMAXCAPTION;
          Base := Ord(twSmallMaxCaptionActive);
        end;
      twFrameLeftActive..twFrameLeftInactive:
        begin
          Part := WP_FRAMELEFT;
          Base := Ord(twFrameLeftActive);
        end;
      twFrameRightActive..twFrameRightInactive:
        begin
          Part := WP_FRAMERIGHT;
          Base := Ord(twFrameRightActive);
        end;
      twFrameBottomActive..twFrameBottomInactive:
        begin
          Part := WP_FRAMEBOTTOM;
          Base := Ord(twFrameBottomActive);
        end;
      twSmallFrameLeftActive..twSmallFrameLeftInactive:
        begin
          Part := WP_SMALLFRAMELEFT;
          Base := Ord(twSmallFrameLeftActive);
        end;
      twSmallFrameRightActive..twSmallFrameRightInactive:
        begin
          Part := WP_SMALLFRAMERIGHT;
          Base := Ord(twSmallFrameRightActive);
        end;
      twSmallFrameBottomActive..twSmallFrameBottomInactive:
        begin
          Part := WP_SMALLFRAMEBOTTOM;
          Base := Ord(twSmallFrameBottomActive);
        end;
      twSysButtonNormal..twSysButtonDisabled:
        begin
          Part := WP_SYSBUTTON;
          Base := Ord(twSysButtonNormal);
        end;
      twMDISysButtonNormal..twMDISysButtonDisabled:
        begin
          Part := WP_MDISYSBUTTON;
          Base := Ord(twMDISysButtonNormal);
        end;
      twMinButtonNormal..twMinButtonDisabled:
        begin
          Part := WP_MINBUTTON;
          Base := Ord(twMinButtonNormal);
        end;
      twMDIMinButtonNormal..twMDIMinButtonDisabled:
        begin
          Part := WP_MDIMINBUTTON;
          Base := Ord(twMDIMinButtonNormal);
        end;
      twMaxButtonNormal..twMaxButtonDisabled:
        begin
          Part := WP_MAXBUTTON;
          Base := Ord(twMaxButtonNormal);
        end;
      twCloseButtonNormal..twCloseButtonDisabled:
        begin
          Part := WP_CLOSEBUTTON;
          Base := Ord(twCloseButtonNormal);
        end;
      twSmallCloseButtonNormal..twSmallCloseButtonDisabled:
        begin
          Part := WP_SMALLCLOSEBUTTON;
          Base := Ord(twSmallCloseButtonNormal);
        end;
      twMDICloseButtonNormal..twMDICloseButtonDisabled:
        begin
          Part := WP_MDICLOSEBUTTON;
          Base := Ord(twMDICloseButtonNormal);
        end;
      twRestoreButtonNormal..twRestoreButtonDisabled:
        begin
          Part := WP_RESTOREBUTTON;
          Base := Ord(twRestoreButtonNormal);
        end;
      twMDIRestoreButtonNormal..twMDIRestoreButtonDisabled:
        begin
          Part := WP_MDIRESTOREBUTTON;
          Base := Ord(twMDIRestoreButtonNormal);
        end;
      twHelpButtonNormal..twHelpButtonDisabled:
        begin
          Part := WP_HELPBUTTON;
          Base := Ord(twHelpButtonNormal);
        end;
      twMDIHelpButtonNormal..twMDIHelpButtonDisabled:
        begin
          Part := WP_MDIHELPBUTTON;
          Base := Ord(twMDIHelpButtonNormal);
        end;
      twHorzScrollNormal..twHorzScrollDisabled:
        begin
          Part := WP_HORZSCROLL;
          Base := Ord(twHorzScrollNormal);
        end;
      twHorzThumbNormal..twHorzThumbDisabled:
        begin
          Part := WP_HORZTHUMB;
          Base := Ord(twHorzThumbNormal);
        end;
      twVertScrollNormal..twVertScrollDisabled:
        begin
          Part := WP_VERTSCROLL;
          Base := Ord(twVertScrollNormal);
        end;
      twVertThumbNormal..twVertThumbDisabled:
        begin
          Part := WP_VERTTHUMB;
          Base := Ord(twVertThumbNormal);
        end;
      twDialog:
        begin
          Part := WP_DIALOG;
          Base := Ord(twDialog);
        end;
      twCaptionSizingTemplate:
        begin
          Part := WP_CAPTIONSIZINGTEMPLATE;
          Base := Ord(twCaptionSizingTemplate);
        end;
      twSmallCaptionSizingTemplate:
        begin
          Part := WP_SMALLCAPTIONSIZINGTEMPLATE;
          Base := Ord(twSmallCaptionSizingTemplate);
        end;
      twFrameLeftSizingTemplate:
        begin
          Part := WP_FRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twFrameLeftSizingTemplate);
        end;
      twSmallFrameLeftSizingTemplate:
        begin
          Part := WP_SMALLFRAMELEFTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameLeftSizingTemplate);
        end;
      twFrameRightSizingTemplate:
        begin
          Part := WP_FRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twFrameRightSizingTemplate);
        end;
      twSmallFrameRightSizingTemplate:
        begin
          Part := WP_SMALLFRAMERIGHTSIZINGTEMPLATE;
          Base := Ord(twSmallFrameRightSizingTemplate);
        end;
      twFrameBottomSizingTemplate:
        begin
          Part := WP_FRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twFrameBottomSizingTemplate);
        end;
      twSmallFrameBottomSizingTemplate:
        begin
          Part := WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE;
          Base := Ord(twSmallFrameBottomSizingTemplate);
        end;
    else
      Part := 0;
      Base := 0;
    end;
    State := Ord(Widget) - Base + 1;
  end;
end;

function TThemePainter.ColorToRGB(Color: TColor; Details: PThemedDetails = nil): COLORREF;
begin
  if (Color and $80000000 = 0) or (Details = nil) then
    Result := Color
  else
    Result := GetThemeSysColor(Theme[Details.Element], Color and not $80000000);
end;

function TThemePainter.ContentRect(DC: HDC; const Details: TThemedDetails; BoundingRect: TRect): TRect;
begin
  with Details do
    GetThemeBackgroundContentRect(Theme[Element], DC, Part, State, BoundingRect, @Result);
end;

procedure TThemePainter.DrawEdge(DC: HDC; const Details: TThemedDetails; const R: TRect; Edge, Flags: Cardinal;
  ContentRect: PRect = nil);
begin
  with Details do
    DrawThemeEdge(Theme[Element], DC, Part, State, R, Edge, Flags, ContentRect);
end;

procedure TThemePainter.DrawElement(DC: HDC; const Details: TThemedDetails; const R: TRect; ClipRect: PRect = nil);
begin
  with Details do
    DrawThemeBackground(Theme[Element], DC, Part, State, R, ClipRect);
end;

procedure TThemePainter.DrawIcon(DC: HDC; const Details: TThemedDetails; const R: TRect; himl: HIMAGELIST;
  Index: Integer);
begin
  with Details do
    DrawThemeIcon(Theme[Element], DC, Part, State, R, himl, Index);
end;

procedure TThemePainter.DrawParentBackground(Window: HWND; Target: HDC; Details: PThemedDetails;
  OnlyIfTransparent: Boolean; Bounds: PRect = nil);
var
  DoDraw: Boolean;
begin
  if OnlyIfTransparent and Assigned(Details) then
  begin
    with Details^ do
      DoDraw := IsThemeBackgroundPartiallyTransparent(Theme[Element], Part, State);
  end
  else
    DoDraw := True;
  if DoDraw then
    DrawThemeParentBackground(Window, Target, Bounds);
end;

procedure TThemePainter.DrawText(DC: HDC; const Details: TThemedDetails;
	const S: WideString; R: TRect; Flags: Cardinal);
begin
  with Details do
    DrawThemeText(Theme[Element], DC, Part, State, PWideChar(S), Length(S), Flags, 0, R);
end;

function TThemePainter.HasTransparentParts(Details: TThemedDetails): Boolean;
begin
  with Details do
    Result := IsThemeBackgroundPartiallyTransparent(Theme[Element], Part, State);
end;

function TThemePainter.PartSize(const Details: TThemedDetails; var Rect: TRect; ThemeSize: TThemeSize = tsDraw): TSize;
begin
  with Details do
  	GetThemePartSize(Theme[Element], 0, Part, State, @Rect, ThemeSize, Result);
end;

function TThemePainter.PartSize(const Details: TThemedDetails; ThemeSize: TThemeSize = tsDraw): TSize;
begin
  with Details do
  	GetThemePartSize(Theme[Element], 0, Part, State, nil, ThemeSize, Result); // tsDraw
end;

procedure TThemePainter.UpdateThemes;
begin
  if FAvailable then
    UnloadThemeData;
  FAvailable := ThemesLoaded;
  if FAvailable then
    FControlsEnabled := GetThemeAppProperties and STAP_ALLOW_CONTROLS = STAP_ALLOW_CONTROLS
  else
    FControlsEnabled := False;
end;

function GetThemeBorder: Integer;
const
	Borders: array[Boolean] of Integer = (2, 2);
begin
	Result := Borders[ThemePainter.Enabled];
end;

function CalcEditHeight(DC: HDC): Integer;
begin
  Result := FontHeight(DC) + 8;
end;

function CalcEditHeight(Font: TFont): Integer;
begin
  Result := FontHeight(Font) + 8;
end;

procedure DrawThemeComboBox(DC: HDC; Rect: TRect; State: TDrawState);
var
  Theme: TThemedComboBox;
begin
  if ThemePainter.Enabled then
  begin
    Theme := tcBorderNormal;
    if dsDisabled in State then
      Theme := tcBorderDisabled
    else if dsHot in State then
        Theme := tcBorderHot
    else if dsFocused in State then
      Theme := tcBorderFocused;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    DrawFrame(DC, Rect, dfSunken);
    InflateRect(Rect, -2, -2);
    FilLRectColor(DC, Rect, clWindow);
  end;
end;

function CalcComboButtonRect(DC: HDC; Rect: TRect; State: TDrawState): TRect;
var
  D: TThemedDetails;
  I: Integer;
begin
  Result := Rect;
  if ThemePainter.Enabled then
  begin
    if IsVistaOrLater then
      D := ThemePainter.GetDetails(tcDropDownButtonRightNormal)
    else
      D := ThemePainter.GetDetails(tcDropDownButtonNormal);
    Result := Rect;
    GetThemeMetric(ThemePainter.Theme[D.Element], 0, D.Part, D.State, TMT_BORDERSIZE, I);
    InflateRect(Result, -I, -I);
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL);
  end
  else
  begin
    InflateRect(Result, -2, -2);
    Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL);
  end;
end;

procedure DrawThemeComboButton(DC: HDC; Rect: TRect; State: TDrawState);
var
  Theme: TThemedComboBox;
  FrameState: Cardinal;
begin
  if ThemePainter.Enabled then
  begin
    if IsVistaOrLater then
      Theme := tcDropDownButtonRightNormal
    else
      Theme := tcDropDownButtonNormal;
    if dsDisabled in State then
      Inc(Theme, 3)
    else if dsPressed in State then
      Inc(Theme, 2)
    else if dsHot in State then
      Inc(Theme);
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    FrameState := DFCS_SCROLLDOWN;
    if dsPressed in State then
	    FrameState := FrameState or DFCS_PUSHED or DFCS_FLAT;
    DrawFrameControl(DC, Rect, DFC_SCROLL, FrameState);
  end;
  if dsCustom in State then
  begin
    InflateRect(Rect, -3, -3);
    StretchBlt(DC, Rect.Left, Rect.Top, WidthOf(Rect), HeightOf(Rect), DC,
      Rect.Left, Rect.Top, 1, HeightOf(Rect), SRCCOPY);
  end;
end;

procedure DrawThemeBorder(DC: HDC; const Rect: TRect; State: TDrawState;
	Thickness: Integer = -1; X: Integer = 0; Y: Integer = 0);
var
  A, B: TRect;
  E: TThemedEdit;
begin
	A := Rect;
  B := Rect;
  OffsetRect(B, X, Y);
  if Thickness < 0 then
  	Thickness := GetThemeBorder;
  InflateRect(B, -Thickness, -Thickness);
  SelectClipRect(DC, B, RGN_DIFF);
  with ThemePainter do
    if Enabled then
    begin
      E := teBorderNoScrollNormal;
      if dsDisabled in State then
        E := teBorderNoScrollDisabled
      else if dsHot in State then
        E := teBorderNoScrollHot
      else if dsSelected in State then
        E := teBorderNoScrollFocused;
      DrawElement(DC, GetDetails(E), A);
    end
    else
    begin
      B := Rect;
      InflateRect(B, -2, -2);
      FillRectColor(DC, B, clWindow);
      if dsThin in State then
        DrawFrame(DC, Rect, dfLowered)
      else
        DrawFrame(DC, Rect, dfSunken);
    end;
  SelectClipRgn(DC, 0);
end;

procedure DrawThemeBorderInflate(DC: HDC; var Rect: TRect; State: TDrawState);
begin
  DrawThemeBorder(DC, Rect, State);
  InflateRect(Rect, -1, -1);
  if not ThemePainter.Enabled then
    InflateRect(Rect, -1, -1);
end;

procedure DrawThemeEdit(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedEdit;
  R: TRect;
begin
  if ThemePainter.Enabled then
  begin
    if dsDisabled in State then
      Theme := teBorderVScrollDisabled
    else if dsHot in State then
      Theme := teBorderVScrollHot
    else if dsFocused in State then
      Theme := teBorderVScrollFocused
    else
      Theme := teBorderVScrollNormal;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    DrawFrame(DC, Rect, dfSunken);
    R := Rect;
    InflateRect(R, -2, -2);
    FillRectColor(DC, R, clWindow);
  end;
end;

procedure DrawThemeButton(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedButton;
  R: TRect;
begin
	if dsFlat in State then
		DrawThemeThinButton(DC, Rect, State)
  else if ThemePainter.Enabled then
  begin
    Theme := tbPushButtonNormal;
    if dsDisabled in State then
      Theme := tbPushButtonDisabled
    else if dsHot in State then
      if dsPressed in State then
        Theme := tbPushButtonPressed
      else
        Theme := tbPushButtonHot
    else if dsPressed in State then
      Theme := tbPushButtonHot
    else if dsFocused in State then
      Theme := tbPushButtonDefaulted;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    R := Rect;
    if dsFocused in State then
    begin
	    DrawFrame(DC, R, dfFocus);
      InflateRect(R, -1, -1);
    end;
    if (dsHot in State) and (dsPressed in State) then
	    DrawFrame(DC, R, dfPressed)
    else
	    DrawFrame(DC, R, dfFramed);
  end;
end;

procedure DrawThemeButtonFocus(DC: HDC; const Rect: TRect);
begin
  if ThemePainter.Enabled then
    DrawFocus(DC, Rect, -3, -3)
  else
    DrawFocus(DC, Rect, -4, -4);
end;

procedure DrawThemeThinButton(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedToolBar;
begin
  if ThemePainter.Enabled then
  begin
    Theme := ttbButtonNormal;
    if dsDisabled in State then
      Theme := ttbButtonDisabled
    else if dsHot in State then
      if dsPressed in State then
        Theme := ttbButtonPressed
      else
        Theme := ttbButtonHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
		if dsDisabled in State then Exit;
    if dsHot in State then
      if dsPressed in State then
  	    DrawFrame(DC, Rect, dfLowered)
      else
	      DrawFrame(DC, Rect, dfRaised);
  end;
end;

procedure DrawThemeHeader(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedHeader;
  B: HBRUSH;
begin
  if ThemePainter.Enabled then
  begin
    Theme := thHeaderItemNormal;
    if dsDisabled in State then
      Theme := thHeaderRoot
    else if dsHot in State then
      if dsPressed in State then
        Theme := thHeaderItemPressed
      else if dsHot in State then
        Theme := thHeaderItemHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    if dsDisabled in State then
	    B := GetBrush(clBtnFace)
    else if dsHot in State then
      if dsPressed in State then
	      B := GetBrush(clBtnFace, clBtnShadow)
      else
	      B := GetBrush(clBtnFace, clBtnHighlight)
    else if dsSelected in State then
      B := GetBrush(clBtnFace, Blend(clBtnHighlight, clBtnFace))
		else
      B := GetBrush(clBtnFace);
  	FillRect(DC, Rect, B);
    DeleteObject(B);
    if dsDisabled in State then
	    DrawFrame(DC, Rect, dfRaised)
    else if dsHot in State then
      if dsPressed in State then
	      DrawFrame(DC, Rect, dfLowered)
      else
	      DrawFrame(DC, Rect, dfRaised)
		else
	    DrawFrame(DC, Rect, dfRaised);
  end;
end;

procedure DrawThemeNode(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedTreeView;
begin
  if ThemePainter.Enabled then
  begin
    if dsExpanded in State then
      Theme := ttGlyphOpened
    else
      Theme:= ttGlyphClosed;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
    DrawNode(DC, Rect, dsExpanded in State);
end;

procedure DrawThemeGroupBox(DC: HDC; const Text: string; const Rect: TRect;
	State: TDrawState);
var
  GroupRect: TRect;
  CaptionRect: TRect;
  NodeRect: TRect;
  ClipRect: TRect;
	Details: TThemedDetails;
begin
  GroupRect := Rect;
  CaptionRect := Rect;
  NodeRect := Rect;
  NodeRect.Right := NodeRect.Left + NodeSize;
  NodeRect.Bottom := NodeRect.Top + NodeSize;
  with CalcCaptionSize(DC, Text), CaptionRect  do
  begin
    Inc(Top);
    Left := Left + 2;
    Bottom := Top + cY;
    Right := Left + cX + 4;
    OffsetRect(CaptionRect, NodeSize + 2, 0);
  end;
  if HeightOf(CaptionRect) > NodeSize then
    OffsetRect(NodeRect, 0, (HeightOf(CaptionRect) - NodeSize) div 2);
  ClipRect := CaptionRect;
  with ClipRect do
  begin
    Left := Rect.Left;
    ExcludeClipRect(DC, Left, Top, Right, Bottom);
  end;
  GroupRect.Top := GroupRect.Top + HeightOf(CaptionRect) div 2 + 1;
  GroupRect.Left := GroupRect.Left + NodeSize div 2;
  if not (dsExpanded in State) then
	  with GroupRect do
    begin
  	  ExcludeClipRect(DC, Right - 4, Top, Right, Bottom);
  	  ExcludeClipRect(DC, ClipRect.Left, Top + 2, Right, Bottom);
    end;
  if ThemePainter.Enabled then
	  with ThemePainter do
    begin
      if dsDisabled in State then
        Details := GetDetails(tbGroupBoxNormal)
      else
        Details := GetDetails(tbGroupBoxDisabled);
	    DrawElement(DC, Details, GroupRect);
      SelectClipRgn(DC, 0);
      DrawText(DC, Details, Text, CaptionRect, DT_CENTER or DT_SINGLELINE);
    end
  else
  begin
    DrawFrame(DC, GroupRect, dfLowered);
    InflateRect(GroupRect, -1, -1);
    DrawFrame(DC, GroupRect, dfRaised);
    SelectClipRgn(DC, 0);
    DrawCaption(DC, Text, CaptionRect, drCenter);
    FilLRect(DC, NodeRect, COLOR_BTNFACE + 1);
    if dsPressed in State then
	    DrawFrame(DC, NodeRect, dfLowered)
    else
	    DrawFrame(DC, NodeRect, dfRaised);
    DrawNode(DC, NodeRect, dsExpanded in State);
  end;
end;

procedure DrawThemeExpandableBox(DC: HDC; const Text: string; const Rect: TRect;
	State: TDrawState);
var
  GroupRect: TRect;
  CaptionRect: TRect;
  NodeRect: TRect;
  ClipRect: TRect;
	Details: TThemedDetails;
begin
  GroupRect := Rect;
  CaptionRect := Rect;
  NodeRect := Rect;
  NodeRect.Right := NodeRect.Left + NodeSize;
  NodeRect.Bottom := NodeRect.Top + NodeSize;
  with CalcCaptionSize(DC, Text), CaptionRect  do
  begin
    Inc(Top);
    Left := Left + 2;
    Bottom := Top + cY;
    Right := Left + cX + 4;
    OffsetRect(CaptionRect, NodeSize + 2, 0);
  end;
  if HeightOf(CaptionRect) > NodeSize then
    OffsetRect(NodeRect, 0, (HeightOf(CaptionRect) - NodeSize) div 2);
  ClipRect := CaptionRect;
  with ClipRect do
  begin
    Left := Rect.Left;
    ExcludeClipRect(DC, Left, Top, Right, Bottom);
  end;
  GroupRect.Top := GroupRect.Top + HeightOf(CaptionRect) div 2 + 1;
  GroupRect.Left := GroupRect.Left + NodeSize div 2;
  if not (dsExpanded in State) then
	  with GroupRect do
    begin
  	  ExcludeClipRect(DC, Right - 4, Top, Right, Bottom);
  	  ExcludeClipRect(DC, ClipRect.Left, Top + 2, Right, Bottom);
    end;
  if ThemePainter.Enabled then
	  with ThemePainter do
    begin
      if dsDisabled in State then
        Details := GetDetails(tbGroupBoxNormal)
      else
        Details := GetDetails(tbGroupBoxDisabled);
	    DrawElement(DC, Details, GroupRect);
      SelectClipRgn(DC, 0);
      DrawText(DC, Details, Text, CaptionRect, DT_CENTER or DT_SINGLELINE);
      if dsPressed in State then
			  Details := GetDetails(ttbButtonPressed);
      {else
			  Details := GetDetails(ttbButtonHot);}
      DrawElement(DC, Details, NodeRect);
      if dsExpanded in State then
			  Details := GetDetails(ttGlyphOpened)
      else
			  Details := GetDetails(ttGlyphClosed);
      DrawElement(DC, Details, NodeRect);
    end
  else
  begin
    DrawFrame(DC, GroupRect, dfLowered);
    InflateRect(GroupRect, -1, -1);
    DrawFrame(DC, GroupRect, dfRaised);
    SelectClipRgn(DC, 0);
    DrawCaption(DC, Text, CaptionRect, drCenter);
    FilLRect(DC, NodeRect, COLOR_BTNFACE + 1);
    if dsPressed in State then
	    DrawFrame(DC, NodeRect, dfLowered)
    else
	    DrawFrame(DC, NodeRect, dfRaised);
    DrawNode(DC, NodeRect, dsExpanded in State);
  end;
end;

procedure DrawThemeHorzThumb(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedTrackBar;
begin
  if ThemePainter.Enabled then
  begin
    Theme := ttbThumbNormal;
    if dsDisabled in State then
      Theme := ttbThumbDisabled
    else if dsPressed in State then
      Theme := ttbThumbPressed
    else if dsHot in State then
      Theme := ttbThumbHot
    else if dsFocused in State then
      Theme := ttbThumbFocused;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    FillRectColor(DC, Rect, clBtnFace);
		DrawFrame(DC, Rect, dfFramed);
  end;
end;

procedure DrawThemeVertThumb(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedTrackBar;
begin
  if ThemePainter.Enabled then
  begin
    Theme := ttbThumbVertNormal;
    if dsDisabled in State then
      Theme := ttbThumbVertDisabled
    else if dsPressed in State then
      Theme := ttbThumbVertPressed
    else if dsHot in State then
      Theme := ttbThumbVertHot
    else if dsFocused in State then
      Theme := ttbThumbVertFocused;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    FillRectColor(DC, Rect, clBtnFace);
		DrawFrame(DC, Rect, dfFramed);
  end;
end;


procedure DrawThemeHorzSplit(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedToolBar;
  R: TRect;
begin
  if ThemePainter.Enabled then
  begin
		R := Rect;
    Dec(R.Left, 2);
    Inc(R.Right, 2);
    Theme := ttbSeparatorNormal;
    if dsDisabled in State then
      Theme := ttbSeparatorDisabled
    else if dsPressed in State then
      Theme := ttbSeparatorPressed
    else if dsHot in State then
      Theme := ttbSeparatorHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
  end
  else
		DrawFrame(DC, Rect, dfLowered);
end;

procedure DrawThemeVertSplit(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedToolBar;
  R: TRect;
begin
  if ThemePainter.Enabled then
  begin
		R := Rect;
    Dec(R.Top, 2);
    Inc(R.Bottom, 2);
    Theme := ttbSeparatorVertNormal;
    if dsDisabled in State then
      Theme := ttbSeparatorVertDisabled
    else if dsPressed in State then
      Theme := ttbSeparatorVertPressed
    else if dsHot in State then
      Theme := ttbSeparatorVertHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
  end
  else
		DrawDivider(DC, Rect, ddVert);
end;

procedure DrawThemeCheckBox(DC: HDC; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedButton;
begin
  if ThemePainter.Enabled then
  begin
    Theme := tbCheckBoxUncheckedNormal;
    if dsDisabled in State then
      Theme := tbCheckBoxUncheckedDisabled
    else if dsPressed in State then
      Theme := tbCheckBoxUncheckedPressed
    else if dsHot in State then
      Theme := tbCheckBoxUncheckedHot;
		if dsChecked in State then
    	Inc(Theme, 4);
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
		DrawCheckBox(DC, Rect, dsChecked in State, clWindow, clWindow, False);
end;

procedure DrawThemeClose(DC: HDC; const Rect: TRect; State: TDrawState; Color: TColor = clWindowFrame);
var
  Theme: TThemedExplorerBar;
begin
	if ThemePainter.Enabled then
  begin
		if dsDisabled in State then
    	Theme := tebHeaderCloseNormal
		else if dsPressed in State then
    	Theme := tebHeaderClosePressed
		else if dsHot in State then
    	Theme := tebHeaderCloseHot
		else
    	Theme := tebHeaderCloseNormal;
		ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  	GlyphFrame(DC, Rect, gkClose, State, Color);
end;

procedure DrawThemeToolClose(DC: HDC; const Rect: TRect; State: TDrawState; Color: TColor = clWindowFrame);
var
  Theme: TThemedToolTip;
begin
	if ThemePainter.Enabled then
  begin
		if dsDisabled in State then
    	Theme := tttCloseNormal
		else if dsPressed in State then
    	Theme := tttClosePressed
		else if dsHot in State then
    	Theme := tttCloseHot
		else
    	Theme := tttCloseNormal;
		ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  	GlyphFrame(DC, Rect, gkClose, State, Color);
end;

procedure DrawThemePin(DC: HDC; const Rect: TRect; State: TDrawState; Color: TColor = clWindowFrame);
var
  Theme: TThemedExplorerBar;
begin
	if ThemePainter.Enabled then
  begin
		if dsDisabled in State then
    	Theme := tebHeaderPinSelectedNormal
		else if dsPressed in State then
    	Theme := tebHeaderPinSelectedPressed
		else if dsHot in State then
    	Theme := tebHeaderPinSelectedHot
		else
    	Theme := tebHeaderPinNormal;
		ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else if dsPressed in State then
  	GlyphFrame(DC, Rect, gkPinPushed, State, Color)
  else
  	GlyphFrame(DC, Rect, gkPin, State, Color);
end;

procedure DrawThemeArrow(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState;
 Color: TColor = clWindowFrame; Adjust: Integer = 0);
const
	GlyphSize = 17;
var
	Theme: TThemedScrollBar;
	R: TRect;
  I: Integer;
begin
  if ThemePainter.Enabled then
  begin
  	case Direction of
      drLeft: Theme := tsArrowBtnLeftNormal;
			drUp: Theme := tsArrowBtnUpNormal;
      drRight: Theme := tsArrowBtnRightNormal;
      drDown: Theme := tsArrowBtnDownNormal;
    else
    	Exit;
		end;
    if dsDisabled in State then
			Inc(Theme, 3)
    else if dsPressed in State then
			Inc(Theme, 2)
    else if dsHot in State then
			Inc(Theme, 1);
    R := Rect;
    I := (WidthOf(Rect) - GlyphSize) div 2;
    Inc(R.Left, I); //  + Adjust
    I := (HeightOf(Rect) - GlyphSize) div 2;
    Inc(R.Top, I + Adjust);
    R.Right := R.Left + GlyphSize;
    R.Bottom := R.Top + GlyphSize;
  	ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
  end
  else
		DrawArrow(DC, Rect, Direction, Color, not (dsDisabled in State));
end;

procedure DrawScroll(DC: HDC; Theme: TThemedScrollBar; Rect: TRect);
var
 W, H, X, Y: Integer;
 A: TFastBitmap;
begin
  W := WidthOf(Rect);
  H := HeightOf(Rect);
  if (W < 1) or (H < 1) then Exit;
  X := Rect.Left;
  Y := Rect.Top;
  OffsetRect(Rect, -X, -Y);
  A := CreateFastBitmap(W, H);
  ThemePainter.DrawElement(A.DC, ThemePainter.GetDetails(Theme), Rect);
  StretchBlt(A.DC, 3, 3, W - 6, H - 6, A.DC, 3, 3, 1, H - 6, SRCCOPY);
  BitBlt(DC, X, Y, W, H, A.DC, 0, 0, SRCCOPY);
  DestroyFastBitmap(A);
end;

procedure DrawThemeScroll(DC: HDC; const Rect: TRect; State: TDrawState);
var
	Theme: TThemedScrollBar;
  FrameState: Cardinal;
  // R: TRect;
begin
  if ThemePainter.Enabled then
  begin
    Theme := tsThumbBtnVertNormal;
    if dsDisabled in State then
      Theme := tsThumbBtnVertDisabled
    else if dsPressed in State then
      Theme := tsThumbBtnVertPressed
    else if dsHot in State then
      Theme := tsThumbBtnVertHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), Rect);
  end
  else
  begin
    FrameState := DFCS_SCROLLDOWN;
    if dsPressed in State then
	    FrameState := FrameState or DFCS_FLAT;
    DrawFrameControl(DC, Rect, DFC_SCROLL, FrameState);
  end;

(*  if ThemePainter.Enabled then
  begin
    R := Rect;
  	Theme := tsArrowBtnRightNormal;
    if dsDisabled in State then
			Inc(Theme, 3)
    else if dsPressed in State then
			Inc(Theme, 2)
    else if dsHot in State then
			Inc(Theme, 1);
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
    StretchBlt(DC, R.Left + 3, R.Top + 3, WidthOf(R) - 6, HeightOf(R) - 6, DC,
      R.Left + 3, R.Top + 3, 2, HeightOf(R) - 6, SRCCOPY);
  end
  else
  begin
		FrameState := 0;
  	{case Direction of
      drLeft: FrameState := DFCS_SCROLLLEFT;
			drUp: FrameState := DFCS_SCROLLUP;
      drRight: FrameState := DFCS_SCROLLRIGHT;
      drDown: FrameState := DFCS_SCROLLDOWN;
      drCenter: FrameState := DFCS_BUTTONPUSH;
		end;}
    if dsThin in State then
			FrameState := FrameState or DFCS_FLAT;
    if dsDisabled in State then
			FrameState := FrameState or DFCS_INACTIVE
    else if dsPressed in State then
			FrameState := FrameState or DFCS_PUSHED or DFCS_FLAT;
    //if Direction = drCenter then
	    DrawFrameControl(DC, Rect, DFC_BUTTON, FrameState)
    //else
	    //DrawFrameControl(DC, Rect, DFC_SCROLL, FrameState);
  end;*)
end;

procedure DrawFlatScroll(DC: HDC; Direction: TDirection; const Rect: TRect; State: TDrawState);
var
  FrameState: Cardinal;
begin
  FrameState := 0;
	case Direction of
    drLeft: FrameState := DFCS_SCROLLLEFT;
    drUp: FrameState := DFCS_SCROLLUP;
    drRight: FrameState := DFCS_SCROLLRIGHT;
    drDown: FrameState := DFCS_SCROLLDOWN;
    drCenter: FrameState := DFCS_BUTTONPUSH;
  end;
  if dsThin in State then
    FrameState := FrameState or DFCS_FLAT;
  if dsDisabled in State then
	  FrameState := FrameState or DFCS_INACTIVE
  else if dsPressed in State then
	  FrameState := FrameState or DFCS_PUSHED or DFCS_FLAT;
  if Direction = drCenter then
    DrawFrameControl(DC, Rect, DFC_BUTTON, FrameState)
  else
    DrawFrameControl(DC, Rect, DFC_SCROLL, FrameState);
end;

procedure DrawThemeGrip(DC: HDC; const Rect: TRect);
var
  DrawRect: TRect;
begin
  DrawRect := Rect;
  DrawRect.Left := DrawRect.Right - GetSystemMetrics(SM_CXVSCROLL);
  with ThemePainter do
    if Enabled then
      DrawElement(DC, GetDetails(tsGripper), DrawRect)
    else
      DrawGrip(DC, DrawRect);
end;

procedure	DrawThemeGripper(DC: HDC; Rect: TRect; Color: TColor; Horizontal: Boolean = True);
var
	Details: TThemedRebar;
begin
	if Horizontal then
  	Details := trGripper
	else
  	Details := trGripperVert;
	with ThemePainter do
  	if Enabled then
			DrawElement(DC, GetDetails(Details), Rect)
		else
    begin
      if Horizontal then
      	OffsetRect(Rect, 2, 0)
			else
      	OffsetRect(Rect, 0, 2);
    	InflateRect(Rect, -4, -4);
			DrawSeparator(DC, Rect, Color, Horizontal);
      if Horizontal then
      	OffsetRect(Rect, -3, 0)
			else
      	OffsetRect(Rect, 0, -3);
			DrawSeparator(DC, Rect, Color, Horizontal);
		end;
end;

procedure DrawThemeCloseBox(DC: HDC; const Text: string; const Rect: TRect; State: TDrawState);
var
  Theme: TThemedExplorerBar;
  R: TRect;
begin
	R := Rect;
	with ThemePainter do
	  if ThemePainter.Enabled then
	    DrawElement(DC, GetDetails(teEditTextNormal), R)
    else
    begin
    	DrawFrame(DC, R, dfSunken);
			InflateRect(R, -2, -2);
      FillRect(DC, R, COLOR_WINDOW + 1);
    end;
	R := Rect;
	InflateRect(R, -2, -2);
  R.Bottom := Rect.Top + 29;
	with ThemePainter do
    if Enabled then
    begin
      DrawElement(DC, GetDetails(trRebarRoot), R);
    end
    else
    begin
      DrawFrame(DC, R, dfRaised);
      Dec(R.Bottom);
      FillRect(DC, R, COLOR_BTNFACE + 1);
      Inc(R.Bottom);
    end;
	Inc(R.Left, 5);
  DrawCaption(DC, Text, R, drLeft);
  R.Left := R.Right - HeightOf(R);
  if ThemePainter.Enabled then
  begin
    Theme := tebHeaderCloseNormal;
    if dsPressed in State then
      Theme := tebHeaderClosePressed
    else if dsHot in State then
	    Theme := tebHeaderCloseHot;
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(Theme), R);
    Slide(R, drLeft);
    ThemePainter.DrawElement(DC, ThemePainter.GetDetails(tebHeaderPinNormal), R);
	end
  else
  begin
    DrawClose(DC, R);
    Slide(R, drLeft);
		GlyphDraw(DC, R, gkPin, clWindowFrame);
  end;
end;

procedure DrawThemeStatus(DC: HDC; const Text: string; const Rect: TRect);
var
	R: TRect;
begin
 	with ThemePainter do
	  if Enabled then
		begin
    	R := Rect;
  		DrawElement(DC, GetDetails(tsStatusRoot), R);
	    R.Left := R.Right - HeightOf(R);
	  	DrawElement(DC, GetDetails(tsGripper), R);
	  end
    else
    begin
  		FillRect(DC, Rect, COLOR_BTNFACE + 1);
			DrawGrip(DC, Rect, False);
      DrawDivider(DC, Rect, ddVert);
    end;
	R := Rect;
  R.Right := R.Right - HeightOf(Rect);
	Inc(R.Left, HeightOf(R) div 2);
	DrawCaption(DC,Text, R, drLeft)
end;

procedure DrawThemeSeperator(DC: HDC; const Rect: TRect; ForeGround, Background: TColor);
var
	Brush: HBrush;
  Pen: HPen;
  R: TRect;
begin
	Brush := GetBrush(Background);
  FillRect(DC, Rect, Brush);
  DeleteObject(Brush);
  Pen := SelectObject(DC, GetPen(ForeGround));
  MoveToEx(DC, Rect.Left, Rect.Bottom - 1, nil);
  LineTo(DC, Rect.Right, Rect.Bottom -1);
  OverwriteObject(DC, Pen);
  R := Rect;
  R.Top := R.Bottom - 1;
  R.Right := R.Left + Widthof(Rect) div 2;
  DrawGradient(DC, R, Background, ForeGround, drRight);
  R.Left := R.Right + 1;
  R.Right := Rect.Right;
  DrawGradient(DC, R, Background, ForeGround, drLeft);
end;

procedure DrawThemeBar(DC: HDC; Rect: TRect; Background: TColor);
var
  R: TRect;
  Pen: HPen;
begin
  Pen := SelectObject(DC, GetPen(clActiveCaption));
  MoveToEx(DC, Rect.Left, Rect.Bottom - 1, nil);
  LineTo(DC, Rect.Right, Rect.Bottom -1);
  OverwriteObject(DC, Pen);
  R := Rect;
  R.Top := R.Bottom - 1;
  R.Right := WidthOf(Rect) div 2;
  DrawGradient(DC, R, Background, clActiveCaption, drRight);
  R.Left := R.Right + 1;
  R.Right := Rect.Right;
  DrawGradient(DC, R, Background, clActiveCaption, drLeft);
end;

procedure DrawThemeSeperator(DC: HDC; Rect: TRect; Background: TColor;
	Themed: Boolean = True; Transparent: Boolean = False; Flat: Boolean = False);
var
  Pen: HPen;
begin
	if not Transparent then
  	FillRectColor(DC, Rect, Background);
  Themed := Themed or ThemePainter.Enabled;
  Flat := Flat and ThemePainter.Enabled;
  if Flat then
  begin
		Rect.Top := Rect.Bottom - 1;
    FillRectColor(DC, Rect, clThemeBorder);
  end
  else if Themed then
    DrawThemeBar(DC, Rect, Background)
	  {Pen := SelectObject(DC, GetPen(clActiveCaption));
    //Pen := SelectObject(DC, GetPen(clThemeBorder));
 		MoveToEx(DC, Rect.Left, Rect.Bottom - 1, nil);
	  LineTo(DC, Rect.Right, Rect.Bottom -1);
  	OverwriteObject(DC, Pen);
	  R := Rect;
  	R.Top := R.Bottom - 1;
	  R.Right := WidthOf(Rect) div 2;
  	DrawGradient(DC, R, Background, clActiveCaption, drRight); //
	  R.Left := R.Right + 1;
  	R.Right := Rect.Right;
	  DrawGradient(DC, R, Background, clActiveCaption, drLeft); //}
  else
  begin
	  Pen := SelectObject(DC, GetPen(clBtnShadow));
 		MoveToEx(DC, Rect.Left, Rect.Bottom - 2, nil);
	  LineTo(DC, Rect.Right, Rect.Bottom - 2);
  	OverwriteObject(DC, GetPen(clBtnHighlight));
	 	MoveToEx(DC, Rect.Left, Rect.Bottom - 1, nil);
  	LineTo(DC, Rect.Right, Rect.Bottom - 1);
	  OverwriteObject(DC, Pen);
	end;
end;

procedure DrawThemeDesigner(DC: HDC; const Rect: TRect; GripSize: Integer;
	FixedWidth, FixedHeight: Boolean; State: TDrawState);
var
	A, B: TRect;
	C: TColor;
  X, Y: Integer;
begin
  A := Rect;
  if dsFocused in State then
		if ThemePainter.Enabled then
    begin
    	C := clInactiveCaption;
			DrawThemeBorder(DC, Rect, [])
		end
		else
    begin
      C := cl3DDkShadow;
  		DrawRectOutline(DC, A, cl3DDkShadow);
		end
	else
  	C := clGray;
	InflateRect(A, -1, -1);
	for X := 0 to 2 do
  begin
		if (X = 0) and FixedHeight then Continue;
		A := Rect;
		case X of
			0:
      	begin
        	A.Left := (WidthOf(Rect) - GripSize) shr 1;
					A.Right := A.Left + GripSize;
        end;
      1: A.Right := GripSize;
      2: A.Left := A.Right - GripSize;
    end;
    for Y := 0 to 2 do
    begin
			if (Y > 0) and FixedHeight then Continue;
	   	if (X = 0) and (Y = 0) then Continue;
     	B := A;
			case Y of
        0:
 	      	begin
   	    		B.Top := (HeightOf(Rect) - GripSize) shr 1;
						B.Bottom := B.Top + GripSize;
       	  end;
       	1: B.Bottom := GripSize;
        2: B.Top := A.Bottom - GripSize;
 	    end;
			if ThemePainter.Enabled and (dsFocused in State) then
				ThemePainter.DrawElement(DC, ThemePainter.GetDetails(tebHeaderBackgroundNormal), B)
			else
				FillRectColor(DC, B, C);
		end;
	end;
end;

{procedure DrawThemeNode(DC: HDC; const Rect: TRect; State: TDrawState);
begin
	DrawNode(DC, Rect);
end;}

function CreateBlendSection(DC: HDC; Width: Integer; Height: Integer): HBITMAP;
var
  BitmapInfo: TBitmapInfo;
  Bits: Pointer;
begin
  FillChar(BitmapInfo, SizeOf(TBitmapInfo), #0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := 24;
    biCompression := BI_RGB;
  end;
  Result := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
end;

function BlendImages(Source1, Source2, Dest: HBITMAP; Weight: Byte): Boolean;

  function BitmapsCompatible(const B1, B2: Windows.TBitmap): Boolean;
  begin
    Result := (B1.bmBitsPixel = B2.bmBitsPixel) and (B1.bmPlanes = B2.bmPlanes) and
     (B1.bmWidth = B2.bmWidth) and (B1.bmHeight = B2.bmHeight);
  end;

type
  TRGBTripleArray = array[0..0] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;
var
  B1, B2, D: Windows.TBitmap;
  RGBSrc1, RGBSrc2, RGBDest: PRGBTripleArray;
  WidthBytes: Cardinal;
  Compliment: Byte;
  X, Y: Integer;
begin
  Result := False;
  if not ((GetObject(Source1, SizeOf(Windows.TBitmap), @B1) <> 0) and
    (GetObject(Source2, SizeOf(Windows.TBitmap), @B2) <> 0) and
    (GetObject(Dest, SizeOf(Windows.TBitmap), @D) <> 0) and BitmapsCompatible(B1, B2) and
    BitmapsCompatible(B1, D) and (B1.bmBitsPixel = 24) and (B1.bmPlanes = 1) and
    (B1.bmBits <> nil) and (B2.bmBits <> nil) and (D.bmBits <> nil)) then
    Exit;
  Compliment := 255 - Weight;
  WidthBytes := D.bmWidthBytes;
  RGBSrc1 := B1.bmBits;
  RGBSrc2 := B2.bmBits;
  RGBDest := D.bmBits;
  for Y := 0 to D.bmHeight - 1 do
  begin
    for X := 0 to D.bmWidth - 1 do
    begin
      RGBDest^[X].rgbtRed := Byte(((Cardinal(RGBSrc1^[X].rgbtRed) * Weight) +
        (Cardinal(RGBSrc2^[X].rgbtRed) * Compliment)) shr 8);
      RGBDest^[X].rgbtGreen := Byte(((Cardinal(RGBSrc1^[X].rgbtGreen) * Weight) +
        (Cardinal(RGBSrc2^[X].rgbtGreen) * Compliment)) shr 8);
      RGBDest^[X].rgbtBlue := Byte(((Cardinal(RGBSrc1^[X].rgbtBlue) * Weight) +
        (Cardinal(RGBSrc2^[X].rgbtBlue) * Compliment)) shr 8);
    end;
    RGBSrc1 := PRGBTripleArray(Cardinal(RGBSrc1) + WidthBytes);
    RGBSrc2 := PRGBTripleArray(Cardinal(RGBSrc2)+ WidthBytes);
    RGBDest := PRGBTripleArray(Cardinal(RGBDest) + WidthBytes);
  end;
  Result := True;
end;

procedure DrawBlend(Dest: HDC; DestX, DestY, DestWidth,
  DestHeight: Integer; Source: HDC; SourceX, SourceY, SourceWidth,
  SourceHeight: Integer; SourceWeight: Byte);
var
  S1, S2, D1: HDC;
  B1, B2, B3: HBITMAP;
begin
  B1 := CreateBlendSection(Dest, DestWidth, DestHeight);
  B2 := CreateBlendSection(Dest, DestWidth, DestHeight);
  B3 := CreateBlendSection(Dest, DestWidth, DestHeight);
  S1 := CreateCompatibleDC(Dest);
  S2 := CreateCompatibleDC(Dest);
  D1 := CreateCompatibleDC(Dest);
  SelectObject(S1, B1);
  SelectObject(S2, B2);
  SelectObject(D1, B3);
  SetStretchBltMode(S1, COLORONCOLOR);
  SetStretchBltMode(S2, COLORONCOLOR);
  StretchBlt(S1, 0, 0, DestWidth, DestHeight, Source, SourceX,
    SourceY, SourceWidth, SourceHeight, SRCCOPY);
  StretchBlt(S2, 0, 0, DestWidth, DestHeight,Dest, DestX, DestY,
    DestWidth, DestHeight, SRCCOPY);
  BlendImages(B1, B2, B3, SourceWeight);
  DeleteDC(S1);
  DeleteDC(S2);
  DeleteObject(B1);
  DeleteObject(B2);
  BitBlt(Dest, DestX, DestY, DestWidth, DestHeight, D1, 0, 0, SRCCOPY);
  DeleteDC(D1);
  DeleteObject(B3);
end;

function HexToStream(const HexData: string): TStream;
var
  P: Pointer;
  I: Integer;
begin
  I := Length(HexData) div 2;
  GetMem(P, I);
  try
    HexToBin(PChar(HexData), PChar(P), I);
    Result := TMemoryStream.Create;
    Result.Read(P^, I);
  finally
    FreeMem(P);
  end;
end;

function StreamToHex(Stream: TStream): string;
var
  PriorPosition: Integer;
  P: Pointer;
  I: Integer;
begin
  PriorPosition := Stream.Position;
  Stream.Seek(0, 0);
  I := Stream.Size;
  GetMem(P, I);
  try
    Stream.Write(P^, I);
    SetLength(Result, I * 2);
    BinToHex(PChar(P), PChar(Result), I);
  finally
    Stream.Seek(PriorPosition, 0);
    FreeMem(P);
  end;
end;

procedure LoadGraphic(Graphic: TGraphic; const HexData: string);
var
  Stream: TStream;
begin
  Stream := HexToStream(HexData);
  try
    Graphic.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure LoadGraphicResource(Graphic: TGraphic; Ident: Integer);
var
  Format: PChar;
  Stream: TStream;
begin
  Format := nil;
  if Graphic.ClassType = TBitmap then
    Format := RT_BITMAP
  else if Graphic.ClassType = TIcon then
    Format := RT_ICON
  else if Graphic.ClassType = TMetaFile then
    Format := RT_RCDATA;
  Stream := TResourceStream.CreateFromID(HInstance, Ident, Format);
  try
    Graphic.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ Glass image routines }

type
	TNativeGlassImage = class(TInterfacedObject, IUnknown, IGlassImage, INativeDraw)
  private
  	FImage: TAlphaImage;
    FGrayscale: TAlphaImage;
    procedure Change(Sender: TObject);
    procedure Draw(DestDC, SrcDC: HDC; const Rect: TRect; X, Y: Integer;
      Opacity: Byte);
  protected
  	{ IGlassImage }
    function GetDC: HDC;
    function GetGraphic: TGraphic;
    function GetPixelDepth: TPixelDepth;
    function GetScanline(Row: Integer): Pointer;
    function GetStride: Integer;
  	{ INativeDraw }
    procedure DrawAlpha(const Rect: TRect; DC: HDC; X, Y: Integer;
      Opacity: Byte = $FF; Grayscale: Boolean = False);
    procedure DrawState(const Rect: TRect; DC: HDC; X, Y: Integer;
      State: TDrawState = []);
    procedure DrawColor(const Rect: TRect; DC: HDC; X, Y: Integer;
      Color: TColor; Opacity: Byte = $FF);
	public
  	constructor Create;
    destructor Destroy; override;
  end;

{ TNativeGlassImage }

constructor TNativeGlassImage.Create;
begin
	inherited Create;
  FImage := TAlphaImage.Create;
  FImage.PixelDepth := pd32;
  FImage.OnChange := Change;
end;

destructor TNativeGlassImage.Destroy;
begin
	FImage.Free;
  FGrayscale.Free;
	inherited Destroy;
end;

procedure TNativeGlassImage.Change(Sender: TObject);
begin
	FreeAndNil(FGrayscale);
end;

{ TNativeGlassImage.IGlassImage }

function TNativeGlassImage.GetDC: HDC;
begin
  Result := FImage.Canvas.Handle;
end;

function TNativeGlassImage.GetGraphic: TGraphic;
begin
	Result := FImage;
end;

function TNativeGlassImage.GetPixelDepth: TPixelDepth;
begin
	Result := FImage.PixelDepth;
end;

function TNativeGlassImage.GetScanline(Row: Integer): Pointer;
begin
	Result := FImage.Scanline[Row];
end;

function TNativeGlassImage.GetStride: Integer;
begin
	FImage.HandleNeeded;
  Result := FImage.Stride;
end;

{ TNativeGlassImage.INativeDraw }

procedure TNativeGlassImage.Draw(DestDC, SrcDC: HDC; const Rect: TRect; X, Y: Integer;
  Opacity: Byte);
var
	Func: TBlendFunction;
begin
  FillChar(Func, SizeOf(Func), #0);
	Func.SourceConstantAlpha := Opacity;
  if FImage.PixelDepth = pd32 then
		Func.AlphaFormat := AC_SRC_ALPHA;
	AlphaBlend(DestDC, X, Y, WidthOf(Rect), HeightOf(Rect), SrcDC, Rect.Left,
  	Rect.Top, WidthOf(Rect), HeightOf(Rect), Func);
end;

procedure TNativeGlassImage.DrawAlpha(const Rect: TRect; DC: HDC; X, Y: Integer;
  Opacity: Byte = $FF; Grayscale: Boolean = False);
var
  SrcDC: HDC;
begin
  if FImage.Empty then Exit;
	if Grayscale  then
  begin
  	if FGrayscale = nil then
    begin
    	FGrayscale := TAlphaImage.Create;
      FGrayscale.Assign(FImage);
      FGrayscale.Grayscale;
    end;
    SrcDC := FGrayscale.Canvas.Handle;
	end
  else
  	SrcDC := FImage.Canvas.Handle;
  Draw(DC, SrcDC, Rect, X, Y, Opacity);
end;

procedure TNativeGlassImage.DrawState(const Rect: TRect; DC: HDC; X, Y: Integer;
  State: TDrawState = []);
begin
  case ImageState(State) of
    isNormal:
      begin
        DrawAlpha(Rect, DC, X, Y);
      end;
    isDisabled:
      begin
        DrawAlpha(Rect, DC, X, Y, $80, True);
        DrawColor(Rect, DC, X, Y, clWhite, $30);
      end;
    isHot:
      begin
        DrawAlpha(Rect, DC, X, Y);
        {DrawAlpha(Rect, DC, X, Y);
        DrawColor(Rect, DC, X, Y, clWhite, $30);}
      end;
    isPressed:
      begin
        DrawAlpha(Rect, DC, X, Y);
        DrawColor(Rect, DC, X, Y, clBlack, $30);
      end;
  end;
end;

procedure TNativeGlassImage.DrawColor(const Rect: TRect; DC: HDC; X, Y: Integer;
  Color: TColor; Opacity: Byte = $FF);
var
  B: TFastBitmap;
  RGBA: PRGBA;
  C: TRGBA;
  A: Single;
  R: TRect;
  I: Integer;
begin
  if (WidthOf(Rect) < 1) or (WidthOf(Rect) > FImage.Width) or
    (HeightOf(Rect) < 1) or (HeightOf(Rect) > FImage.Height) or
    (FImage.PixelDepth <> pd32) then
    Exit;
  B := CreateFastBitmap(WidthOf(Rect), HeightOf(Rect), pd32);
  Draw(B.DC, FImage.Canvas.Handle, Rect, 0, 0, $FF);
  RGBA := B.Bits;
  C := ColorToRGBA(Color);
  for I := 0 to B.Width * B.Height - 1 do
  begin
    if RGBA.Alpha = 0 then
    begin
      Inc(RGBA);
      Continue;
    end;
    if RGBA.Alpha = $FF then
    begin
      RGBA.Red := C.Red;
      RGBA.Green := C.Green;
      RGBA.Blue := C.Blue;
      Inc(RGBA);
      Continue;
    end;
    A := RGBA.Alpha / $FF;
    RGBA.Red := Round(C.Red * A);
    RGBA.Green := Round(C.Green * A);
    RGBA.Blue := Round(C.Blue * A);
    Inc(RGBA);
  end;
  R := Rect;
  OffsetRect(R, -R.Left, -R.Top);
  Draw(DC, B.DC, R, X, Y, Opacity);
  DestroyFastBitmap(B);
end;

{ CreateGlassImages }

function CreateGlassImage(Stream: TStream = nil): IGlassImage;
begin
	Result := TNativeGlassImage.Create as IGlassImage;
  if Stream <> nil then
    Result.Graphic.LoadFromStream(Stream);
end;

{ GlassImageBlit

procedure GlassImageBlit(Source: IGlassImage; const Rect: TRect; DC: HDC;
	X, Y: Integer; Opacity: Byte = $FF; Grayscale: Boolean = False);
var
	NativeDraw: INativeDraw;
begin
	if Supports(Source, INativeDraw, NativeDraw) then
  	NativeDraw.Draw(Rect, DC, X, Y, Opacity, Grayscale);
end; }

{procedure GlassBlit(Source: IGlassImage; const Rect: TRect; DC: HDC; X, Y: Integer;
	Opacity: Byte = $FF; Grayscale: Boolean = False);
var
  W, H, Row, Col, Size, I: Integer;
  Bitmap: TFastBitmap;
  Area: TRect;
  A, B: PRGBTriple;
  C: TRGBTriple;
  Alpha: PByte;
  Blend: Byte;
  OpacityBlend, SourceBlend, DestBlend: Single;
begin
  if not Source.Initialize then Exit;
  W := Source.Graphic.Width;
  H := Source.Graphic.Height;
 	Area := Rect;
	with Area do
	begin
		if Left < 0 then Left := 0;
    if Top < 0 then Top := 0;
    if Right > W then Right := W;
    if Bottom > H then Bottom := H;
    if (Bottom - Top < 1) or (Right - Left < 1) then Exit;
	end;
	Bitmap := CreateFastBitmap(Area.Right - Area.Left, Area.Bottom - Area.Top);
 	try
		BitBlt(Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, DC, X, Y, SRCCOPY);
    OpacityBlend := Opacity / High(Byte);
    Size := Bitmap.Width * SizeOf(TRGBTriple) + Bitmap.Width mod 4;
    I := 0;
    for Row := Area.Bottom - 1 downto Area.Top do
    begin
      Inc(I);
			A := Bitmap.Bits;
      Inc(PByte(A), (I - 1) * Size);
    	B := Source.Scanline[Row];
      Inc(B, Area.Left);
      Alpha := Pointer(Source.AlphaScanline[Row]);
      Inc(Alpha, Area.Left);
      for Col := Area.Left to Area.Right - 1 do
      begin
      	if OpacityBlend = 0 then
        begin
    	    Inc(A);
  	      Inc(B);
	        Inc(Alpha);
        	Continue;
        end;
	      if OpacityBlend < 1 then
  	    	Blend := Round(Alpha^ * OpacityBlend)
				else
        	Blend := Alpha^;
				C := B^;
        if Grayscale then
        begin
  	      C.rgbtRed := Round(0.3 * c.rgbtRed + 0.6 * c.rgbtGreen + 0.1 * c.rgbtBlue);
          C.rgbtBlue := C.rgbtRed;
          C.rgbtGreen := c.rgbtRed;
        end;
				if Blend = High(Byte) then
	        A^ := C
				else if Blend > 0 then
        begin
        	SourceBlend := Blend / High(Byte);
          DestBlend := 1 - SourceBlend;
	        A.rgbtBlue := Round(C.rgbtBlue * SourceBlend + A.rgbtBlue * DestBlend);
	        A.rgbtGreen := Round(C.rgbtGreen * SourceBlend + A.rgbtGreen * DestBlend);
	        A.rgbtRed := Round(C.rgbtRed * SourceBlend + A.rgbtRed * DestBlend);
        end;
        Inc(A);
        Inc(B);
        Inc(Alpha);
			end;
		end;
		BitBlt(DC, X, Y, Bitmap.Width, Bitmap.Height, Bitmap.DC, 0, 0, SRCCOPY);
	finally
		DestroyFastBitmap(Bitmap);
	end;
end; }

procedure InitializeGlyphs;
var
	I: TGlyphKind;
begin
	for I := Low(InternalGlyphs) to High(InternalGlyphs) do
  	InternalGlyphs[I] := nil;
end;

procedure FinalizeGlyphs;
var
	I: TGlyphKind;
begin
	for I := Low(InternalGlyphs) to High(InternalGlyphs) do
  	InternalGlyphs[I].Free;
end;

{$IFNDEF LITE}
type
	TColorPicker = class
  public
  	constructor Create;
    destructor Destroy; override;
    procedure Pick(Msg: Cardinal; const HookStruct: TMouseHookStruct;
    var Remove: Boolean);
  end;

var
	ColorPicker: TObject;

procedure ColorPick(Enabled: Boolean);
begin
	if Enabled and (ColorPicker = nil) then
  	ColorPicker := TColorPicker.Create
	else if (not Enabled) and (ColorPicker <> nil) then
  	FreeAndNil(ColorPicker);
end;

{ TColorPicker }

constructor TColorPicker.Create;
begin
	inherited Create;
  HookMouse(Pick);
end;

destructor TColorPicker.Destroy;
begin
  UnhookMouse(Pick);
  inherited Destroy;
end;

function IsShiftDown: Boolean;
var
  KeyState: TKeyBoardState;
begin
  GetKeyboardState(KeyState);
  Result := KeyState[VK_SHIFT] and $80 <> 0;
end;

procedure TColorPicker.Pick(Msg: Cardinal;
  const HookStruct: TMouseHookStruct; var Remove: Boolean);
var
	DC: HDC;
  C: TColorRef;
begin
  if (Msg = WM_LBUTTONDOWN) and IsShiftDown then
  begin
  	DC := GetDC(0);
    C := GetPixel(DC, HookStruct.pt.X, HookStruct.pt.Y);
    ReleaseDC(0, DC);
    SendMessage(GetFocus, CN_COLORPICK, C, 0);
    Remove := True;
  end;
end;
{$ENDIF}

procedure PatchINT3;
var
  NOP: Byte;
  BytesWritten: DWORD;
  NtDll: THandle;
  P: Pointer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;
  NtDll := GetModuleHandle('NTDLL.DLL');
  if NtDll = 0 then
    Exit;
  P := GetProcAddress(NtDll, 'DbgBreakPoint');
  if P = nil then
    Exit;
  try
    if Char(P^) <> #$CC then
      Exit;
    NOP := $90;
    if WriteProcessMemory(GetCurrentProcess, P, @NOP, 1, BytesWritten) and
      (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, P, 1);
  except
    on EAccessViolation do ;
    else raise;
  end;
end;

initialization
  PatchINT3;
	InternalThemePainter := nil;
  ScratchFont := nil;
  ScratchBitmap := TBitmap.Create;
  with ScratchBitmap do
  begin
    Width := 150;
    Height := 50;
  end;
  InitializeGlyphs;
  TPicture.RegisterFileFormat('PNG', 'PNG Image Files', TAlphaImage);
	TPicture.RegisterFileFormat('GIF', 'GIF Image Files', TAlphaImage);
	TPicture.RegisterFileFormat('JPG', 'JPEG Image Files', TAlphaImage);
	TPicture.RegisterFileFormat('TIFF', 'TIFF Image Files', TAlphaImage);
finalization
	TPicture.UnregisterGraphicClass(TAlphaImage);
	FinalizeGlyphs;
  ScratchBitmap.Free;
  ScratchFont.Free;
  InternalThemePainter.Free;
  {$IFNDEF LITE}
  ColorPicker.Free;
  {$ENDIF}
end.
