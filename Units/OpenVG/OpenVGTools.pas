unit OpenVGTools;

interface

uses
  Windows, OpenVG;

type
  Float = VGfloat;
  TByteArray = array of Byte;

  TFloatArray = array of Float;
  TFloatArrays = array of TFloatArray;

  TVec2 = record
    case Integer of
    0: (X, Y: Float);
    1: (S, T: Float);
    2: (V: array[0..1] of Float);
  end;
  PVec2 = ^TVec2;

  TVec2Array = array of TVec2;
  TVec2Arrays = array of TVec2Array;

  TVec3 = record
    case Integer of
    0: (X, Y, Z: Float);
    1: (R, G, B: Float);
    2: (Red, Green, Blue: Float);
    3: (Heading, Pitch, Roll: Float);
    4: (Hue, Saturation, Lightness: Float);
    5: (V: array[0..2] of Float);
  end;
  PVec3 = ^TVec3;

  TVec3Array = array of TVec3;
  TVec3Arrays = array of TVec3Array;

  TRGB = TVec3;
  PRGB = ^TRGB;

  THSL = TVec3;
  PHSL = ^THSL;

  TVec4 = record
    case Integer of
    0: (X, Y, Z, W: Float);
    1: (R, G, B, A: Float);
    2: (Red, Green, Blue, Alpha: Float);
    3: (XY: TVec2);
    4: (XYZ: TVec3);
    5: (Left, Top, Right, Bottom: Float);
    6: (V: array[0..3] of Float);
  end;
  PVec4 = ^TVec4;
  TVec4Array = array of TVec4;

  TRGBA = TVec4;
  PRGBA = PVec4;
  TVecColor = TVec4;
  TVecColors = TVec4Array;

  TVecRect = record
    X, Y, Width, Height: Float;
  end;
  TVecPoint = TVec2;
  TVecPoints = TVec2Array;
  TVecSize = TVec2;

  TVecPattern = TFloatArray;
  TArcKind = (akOpen, akChord, akPie);
  TVecCap = (cpButt, cpRound, cpSquare);
  TVecFillRule = (frEven, frOdd);
  TVecJoin = (jnMiter, jnRound, jnBevel);
  TVecSpread = (spPad, spRepeat, spReflect);

function Vec2(X: Float): TVec2; overload;
function Vec2(const X, Y: Float): TVec2; overload;
function Vec2(const V: TVec3): TVec2; overload;
function Vec2(const V: TVec4): TVec2; overload;
function Vec3(X: Float): TVec3; overload;
function Vec3(const X, Y, Z: Float): TVec3; overload;
function Vec3(const V: TVec4): TVec3; overload;
function Vec4(X: Float): TVec4; overload;
function Vec4(const X, Y, Z, W: Float): TVec4; overload;
function Vec(const X, Y: Float): TVec2; overload;
function Vec(const X, Y, Z: Float): TVec3; overload;
function Vec(const X, Y, Z, W: Float): TVec4; overload;
function Vec(const V: TVec2; Z: Float): TVec3; overload;
function Vec(const V: TVec2; Z, W: Float): TVec4; overload;
function Vec(const V: TVec3; W: Float): TVec4; overload;
function VecAdd(const A, B: TVec2): TVec2; overload;
function VecSubtract(const A, B: TVec2): TVec2; overload;
function VecMultiply(const A, B: TVec2): TVec2; overload;
function VecMultiply(const A: TVec2; B: Float): TVec2; overload;
function VecDivide(const A, B: TVec2): TVec2; overload;
function VecNormalize(const V: TVec2): TVec2; overload;
function VecAngle(X1, Y1, X2, Y2, X3, Y3: Float): Float; overload;
function VecAngle(const A, B, C: TVec2): Float; overload;
function VecDistance(const A, B: TVec2): Float;
function VecAverage(const A, B: TVec2): TVec2;
function VecExtend(const A, B: TVec2; Distance: Float): TVec2;
function VecAdd(const A, B: TVec3): TVec3; overload;
function VecSubtract(const A, B: TVec3): TVec3; overload;
function VecMultiply(const A, B: TVec3): TVec3; overload;
function VecDivide(const A, B: TVec3): TVec3; overload;
function VecNormalize(const V: TVec3): TVec3; overload;
function VecRotate(const V: TVec2; const Angle: Float): TVec2;
function VecRect(X, Y, Width, Height: Float): TVecRect;
function VecArray(const Values: array of Float): TFloatArray;
function VecColors(const Colors: array of TVecColor): TVecColors;

const
  vgClearColor: TVecColor = (R: $00 / $FF; G: $00 / $FF; B: $00 / $FF; A: 0.0);
  vgAliceBlue: TVecColor = (R: $F0 / $FF; G: $F8 / $FF; B: $FF / $FF; A: 1.0);
  vgAntiqueWhite: TVecColor = (R: $FA / $FF; G: $EB / $FF; B: $D7 / $FF; A: 1.0);
  vgAqua: TVecColor = (R: $00 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  vgAquamarine: TVecColor = (R: $7F / $FF; G: $FF / $FF; B: $D4 / $FF; A: 1.0);
  vgAzure: TVecColor = (R: $F0 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  vgBeige: TVecColor = (R: $F5 / $FF; G: $F5 / $FF; B: $DC / $FF; A: 1.0);
  vgBisque: TVecColor = (R: $FF / $FF; G: $E4 / $FF; B: $C4 / $FF; A: 1.0);
  vgBlack: TVecColor = (R: $00 / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  vgBlanchedAlmond: TVecColor = (R: $FF / $FF; G: $EB / $FF; B: $CD / $FF; A: 1.0);
  vgBlue: TVecColor = (R: $00 / $FF; G: $00 / $FF; B: $FF / $FF; A: 1.0);
  vgBlueViolet: TVecColor = (R: $8A / $FF; G: $2B / $FF; B: $E2 / $FF; A: 1.0);
  vgBrown: TVecColor = (R: $A5 / $FF; G: $2A / $FF; B: $2A / $FF; A: 1.0);
  vgBurlyWood: TVecColor = (R: $DE / $FF; G: $B8 / $FF; B: $87 / $FF; A: 1.0);
  vgCadetBlue: TVecColor = (R: $5F / $FF; G: $9E / $FF; B: $A0 / $FF; A: 1.0);
  vgChartreuse: TVecColor = (R: $7F / $FF; G: $FF / $FF; B: $00 / $FF; A: 1.0);
  vgChocolate: TVecColor = (R: $D2 / $FF; G: $69 / $FF; B: $1E / $FF; A: 1.0);
  vgCoral: TVecColor = (R: $FF / $FF; G: $7F / $FF; B: $50 / $FF; A: 1.0);
  vgCornflowerBlue: TVecColor = (R: $64 / $FF; G: $95 / $FF; B: $ED / $FF; A: 1.0);
  vgCornsilk: TVecColor = (R: $FF / $FF; G: $F8 / $FF; B: $DC / $FF; A: 1.0);
  vgCrimson: TVecColor = (R: $DC / $FF; G: $14 / $FF; B: $3C / $FF; A: 1.0);
  vgCyan: TVecColor = (R: $00 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  vgDarkBlue: TVecColor = (R: $00 / $FF; G: $00 / $FF; B: $8B / $FF; A: 1.0);
  vgDarkCyan: TVecColor = (R: $00 / $FF; G: $8B / $FF; B: $8B / $FF; A: 1.0);
  vgDarkGoldenrod: TVecColor = (R: $B8 / $FF; G: $86 / $FF; B: $0B / $FF; A: 1.0);
  vgDarkGray: TVecColor = (R: $A9 / $FF; G: $A9 / $FF; B: $A9 / $FF; A: 1.0);
  vgDarkGreen: TVecColor = (R: $00 / $FF; G: $64 / $FF; B: $00 / $FF; A: 1.0);
  vgDarkKhaki: TVecColor = (R: $BD / $FF; G: $B7 / $FF; B: $6B / $FF; A: 1.0);
  vgDarkMagenta: TVecColor = (R: $8B / $FF; G: $00 / $FF; B: $8B / $FF; A: 1.0);
  vgDarkOliveGreen: TVecColor = (R: $55 / $FF; G: $6B / $FF; B: $2F / $FF; A: 1.0);
  vgDarkOrange: TVecColor = (R: $FF / $FF; G: $8C / $FF; B: $00 / $FF; A: 1.0);
  vgDarkOrchid: TVecColor = (R: $99 / $FF; G: $32 / $FF; B: $CC / $FF; A: 1.0);
  vgDarkRed: TVecColor = (R: $8B / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  vgDarkSalmon: TVecColor = (R: $E9 / $FF; G: $96 / $FF; B: $7A / $FF; A: 1.0);
  vgDarkSeaGreen: TVecColor = (R: $8F / $FF; G: $BC / $FF; B: $8B / $FF; A: 1.0);
  vgDarkSlateBlue: TVecColor = (R: $48 / $FF; G: $3D / $FF; B: $8B / $FF; A: 1.0);
  vgDarkSlateGray: TVecColor = (R: $2F / $FF; G: $4F / $FF; B: $4F / $FF; A: 1.0);
  vgDarkTurquoise: TVecColor = (R: $00 / $FF; G: $CE / $FF; B: $D1 / $FF; A: 1.0);
  vgDarkViolet: TVecColor = (R: $94 / $FF; G: $00 / $FF; B: $D3 / $FF; A: 1.0);
  vgDeepPink: TVecColor = (R: $FF / $FF; G: $14 / $FF; B: $93 / $FF; A: 1.0);
  vgDeepSkyBlue: TVecColor = (R: $00 / $FF; G: $BF / $FF; B: $FF / $FF; A: 1.0);
  vgDimGray: TVecColor = (R: $69 / $FF; G: $69 / $FF; B: $69 / $FF; A: 1.0);
  vgDodgerBlue: TVecColor = (R: $1E / $FF; G: $90 / $FF; B: $FF / $FF; A: 1.0);
  vgFirebrick: TVecColor = (R: $B2 / $FF; G: $22 / $FF; B: $22 / $FF; A: 1.0);
  vgFloralWhite: TVecColor = (R: $FF / $FF; G: $FA / $FF; B: $F0 / $FF; A: 1.0);
  vgForestGreen: TVecColor = (R: $22 / $FF; G: $8B / $FF; B: $22 / $FF; A: 1.0);
  vgFuchsia: TVecColor = (R: $FF / $FF; G: $00 / $FF; B: $FF / $FF; A: 1.0);
  vgGainsboro: TVecColor = (R: $DC / $FF; G: $DC / $FF; B: $DC / $FF; A: 1.0);
  vgGhostWhite: TVecColor = (R: $F8 / $FF; G: $F8 / $FF; B: $FF / $FF; A: 1.0);
  vgGold: TVecColor = (R: $FF / $FF; G: $D7 / $FF; B: $00 / $FF; A: 1.0);
  vgGoldenrod: TVecColor = (R: $DA / $FF; G: $A5 / $FF; B: $20 / $FF; A: 1.0);
  vgGray: TVecColor = (R: $80 / $FF; G: $80 / $FF; B: $80 / $FF; A: 1.0);
  vgGreen: TVecColor = (R: $00 / $FF; G: $80 / $FF; B: $00 / $FF; A: 1.0);
  vgGreenYellow: TVecColor = (R: $AD / $FF; G: $FF / $FF; B: $2F / $FF; A: 1.0);
  vgHoneydew: TVecColor = (R: $F0 / $FF; G: $FF / $FF; B: $F0 / $FF; A: 1.0);
  vgHotPink: TVecColor = (R: $FF / $FF; G: $69 / $FF; B: $B4 / $FF; A: 1.0);
  vgIndianRed: TVecColor = (R: $CD / $FF; G: $5C / $FF; B: $5C / $FF; A: 1.0);
  vgIndigo: TVecColor = (R: $4B / $FF; G: $00 / $FF; B: $82 / $FF; A: 1.0);
  vgIvory: TVecColor = (R: $FF / $FF; G: $FF / $FF; B: $F0 / $FF; A: 1.0);
  vgKhaki: TVecColor = (R: $F0 / $FF; G: $E6 / $FF; B: $8C / $FF; A: 1.0);
  vgLavender: TVecColor = (R: $E6 / $FF; G: $E6 / $FF; B: $FA / $FF; A: 1.0);
  vgLavenderBlush: TVecColor = (R: $FF / $FF; G: $F0 / $FF; B: $F5 / $FF; A: 1.0);
  vgLawnGreen: TVecColor = (R: $7C / $FF; G: $FC / $FF; B: $00 / $FF; A: 1.0);
  vgLemonChiffon: TVecColor = (R: $FF / $FF; G: $FA / $FF; B: $CD / $FF; A: 1.0);
  vgLightBlue: TVecColor = (R: $AD / $FF; G: $D8 / $FF; B: $E6 / $FF; A: 1.0);
  vgLightCoral: TVecColor = (R: $F0 / $FF; G: $80 / $FF; B: $80 / $FF; A: 1.0);
  vgLightCyan: TVecColor = (R: $E0 / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  vgLightGoldenrodYellow: TVecColor = (R: $FA / $FF; G: $FA / $FF; B: $D2 / $FF; A: 1.0);
  vgLightGray: TVecColor = (R: $D3 / $FF; G: $D3 / $FF; B: $D3 / $FF; A: 1.0);
  vgLightGreen: TVecColor = (R: $90 / $FF; G: $EE / $FF; B: $90 / $FF; A: 1.0);
  vgLightPink: TVecColor = (R: $FF / $FF; G: $B6 / $FF; B: $C1 / $FF; A: 1.0);
  vgLightSalmon: TVecColor = (R: $FF / $FF; G: $A0 / $FF; B: $7A / $FF; A: 1.0);
  vgLightSeaGreen: TVecColor = (R: $20 / $FF; G: $B2 / $FF; B: $AA / $FF; A: 1.0);
  vgLightSkyBlue: TVecColor = (R: $87 / $FF; G: $CE / $FF; B: $FA / $FF; A: 1.0);
  vgLightSlateGray: TVecColor = (R: $77 / $FF; G: $88 / $FF; B: $99 / $FF; A: 1.0);
  vgLightSteelBlue: TVecColor = (R: $B0 / $FF; G: $C4 / $FF; B: $DE / $FF; A: 1.0);
  vgLightYellow: TVecColor = (R: $FF / $FF; G: $FF / $FF; B: $E0 / $FF; A: 1.0);
  vgLime: TVecColor = (R: $00 / $FF; G: $FF / $FF; B: $00 / $FF; A: 1.0);
  vgLimeGreen: TVecColor = (R: $32 / $FF; G: $CD / $FF; B: $32 / $FF; A: 1.0);
  vgLinen: TVecColor = (R: $FA / $FF; G: $F0 / $FF; B: $E6 / $FF; A: 1.0);
  vgMagenta: TVecColor = (R: $FF / $FF; G: $00 / $FF; B: $FF / $FF; A: 1.0);
  vgMaroon: TVecColor = (R: $80 / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  vgMediumAquamarine: TVecColor = (R: $66 / $FF; G: $CD / $FF; B: $AA / $FF; A: 1.0);
  vgMediumBlue: TVecColor = (R: $00 / $FF; G: $00 / $FF; B: $CD / $FF; A: 1.0);
  vgMediumOrchid: TVecColor = (R: $BA / $FF; G: $55 / $FF; B: $D3 / $FF; A: 1.0);
  vgMediumPurple: TVecColor = (R: $93 / $FF; G: $70 / $FF; B: $DB / $FF; A: 1.0);
  vgMediumSeaGreen: TVecColor = (R: $3C / $FF; G: $B3 / $FF; B: $71 / $FF; A: 1.0);
  vgMediumSlateBlue: TVecColor = (R: $7B / $FF; G: $68 / $FF; B: $EE / $FF; A: 1.0);
  vgMediumSpringGreen: TVecColor = (R: $00 / $FF; G: $FA / $FF; B: $9A / $FF; A: 1.0);
  vgMediumTurquoise: TVecColor = (R: $48 / $FF; G: $D1 / $FF; B: $CC / $FF; A: 1.0);
  vgMediumVioletRed: TVecColor = (R: $C7 / $FF; G: $15 / $FF; B: $85 / $FF; A: 1.0);
  vgMidnightBlue: TVecColor = (R: $19 / $FF; G: $19 / $FF; B: $70 / $FF; A: 1.0);
  vgMintCream: TVecColor = (R: $F5 / $FF; G: $FF / $FF; B: $FA / $FF; A: 1.0);
  vgMistyRose: TVecColor = (R: $FF / $FF; G: $E4 / $FF; B: $E1 / $FF; A: 1.0);
  vgMoccasin: TVecColor = (R: $FF / $FF; G: $E4 / $FF; B: $B5 / $FF; A: 1.0);
  vgNavajoWhite: TVecColor = (R: $FF / $FF; G: $DE / $FF; B: $AD / $FF; A: 1.0);
  vgNavy: TVecColor = (R: $00 / $FF; G: $00 / $FF; B: $80 / $FF; A: 1.0);
  vgOldLace: TVecColor = (R: $FD / $FF; G: $F5 / $FF; B: $E6 / $FF; A: 1.0);
  vgOlive: TVecColor = (R: $80 / $FF; G: $80 / $FF; B: $00 / $FF; A: 1.0);
  vgOliveDrab: TVecColor = (R: $6B / $FF; G: $8E / $FF; B: $23 / $FF; A: 1.0);
  vgOrange: TVecColor = (R: $FF / $FF; G: $A5 / $FF; B: $00 / $FF; A: 1.0);
  vgOrangeRed: TVecColor = (R: $FF / $FF; G: $45 / $FF; B: $00 / $FF; A: 1.0);
  vgOrchid: TVecColor = (R: $DA / $FF; G: $70 / $FF; B: $D6 / $FF; A: 1.0);
  vgPaleGoldenrod: TVecColor = (R: $EE / $FF; G: $E8 / $FF; B: $AA / $FF; A: 1.0);
  vgPaleGreen: TVecColor = (R: $98 / $FF; G: $FB / $FF; B: $98 / $FF; A: 1.0);
  vgPaleTurquoise: TVecColor = (R: $AF / $FF; G: $EE / $FF; B: $EE / $FF; A: 1.0);
  vgPaleVioletRed: TVecColor = (R: $DB / $FF; G: $70 / $FF; B: $93 / $FF; A: 1.0);
  vgPapayaWhip: TVecColor = (R: $FF / $FF; G: $EF / $FF; B: $D5 / $FF; A: 1.0);
  vgPeachPuff: TVecColor = (R: $FF / $FF; G: $DA / $FF; B: $B9 / $FF; A: 1.0);
  vgPeru: TVecColor = (R: $CD / $FF; G: $85 / $FF; B: $3F / $FF; A: 1.0);
  vgPink: TVecColor = (R: $FF / $FF; G: $C0 / $FF; B: $CB / $FF; A: 1.0);
  vgPlum: TVecColor = (R: $DD / $FF; G: $A0 / $FF; B: $DD / $FF; A: 1.0);
  vgPowderBlue: TVecColor = (R: $B0 / $FF; G: $E0 / $FF; B: $E6 / $FF; A: 1.0);
  vgPurple: TVecColor = (R: $80 / $FF; G: $00 / $FF; B: $80 / $FF; A: 1.0);
  vgRed: TVecColor = (R: $FF / $FF; G: $00 / $FF; B: $00 / $FF; A: 1.0);
  vgRosyBrown: TVecColor = (R: $BC / $FF; G: $8F / $FF; B: $8F / $FF; A: 1.0);
  vgRoyalBlue: TVecColor = (R: $41 / $FF; G: $69 / $FF; B: $E1 / $FF; A: 1.0);
  vgSaddleBrown: TVecColor = (R: $8B / $FF; G: $45 / $FF; B: $13 / $FF; A: 1.0);
  vgSalmon: TVecColor = (R: $FA / $FF; G: $80 / $FF; B: $72 / $FF; A: 1.0);
  vgSandyBrown: TVecColor = (R: $F4 / $FF; G: $A4 / $FF; B: $60 / $FF; A: 1.0);
  vgSeaGreen: TVecColor = (R: $2E / $FF; G: $8B / $FF; B: $57 / $FF; A: 1.0);
  vgSeaShell: TVecColor = (R: $FF / $FF; G: $F5 / $FF; B: $EE / $FF; A: 1.0);
  vgSienna: TVecColor = (R: $A0 / $FF; G: $52 / $FF; B: $2D / $FF; A: 1.0);
  vgSilver: TVecColor = (R: $C0 / $FF; G: $C0 / $FF; B: $C0 / $FF; A: 1.0);
  vgSkyBlue: TVecColor = (R: $87 / $FF; G: $CE / $FF; B: $EB / $FF; A: 1.0);
  vgSlateBlue: TVecColor = (R: $6A / $FF; G: $5A / $FF; B: $CD / $FF; A: 1.0);
  vgSlateGray: TVecColor = (R: $70 / $FF; G: $80 / $FF; B: $90 / $FF; A: 1.0);
  vgSnow: TVecColor = (R: $FF / $FF; G: $FA / $FF; B: $FA / $FF; A: 1.0);
  vgSpringGreen: TVecColor = (R: $00 / $FF; G: $FF / $FF; B: $7F / $FF; A: 1.0);
  vgSteelBlue: TVecColor = (R: $46 / $FF; G: $82 / $FF; B: $B4 / $FF; A: 1.0);
  vgTan: TVecColor = (R: $D2 / $FF; G: $B4 / $FF; B: $8C / $FF; A: 1.0);
  vgTeal: TVecColor = (R: $00 / $FF; G: $80 / $FF; B: $80 / $FF; A: 1.0);
  vgThistle: TVecColor = (R: $D8 / $FF; G: $BF / $FF; B: $D8 / $FF; A: 1.0);
  vgTomato: TVecColor = (R: $FF / $FF; G: $63 / $FF; B: $47 / $FF; A: 1.0);
  vgTransparent: TVecColor = (R: $FF / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  vgTurquoise: TVecColor = (R: $40 / $FF; G: $E0 / $FF; B: $D0 / $FF; A: 1.0);
  vgViolet: TVecColor = (R: $EE / $FF; G: $82 / $FF; B: $EE / $FF; A: 1.0);
  vgWheat: TVecColor = (R: $F5 / $FF; G: $DE / $FF; B: $B3 / $FF; A: 1.0);
  vgWhite: TVecColor = (R: $FF / $FF; G: $FF / $FF; B: $FF / $FF; A: 1.0);
  vgWhiteSmoke: TVecColor = (R: $F5 / $FF; G: $F5 / $FF; B: $F5 / $FF; A: 1.0);
  vgYellow: TVecColor = (R: $FF / $FF; G: $FF / $FF; B: $00 / $FF; A: 1.0);
  vgYellowGreen: TVecColor = (R: $9A / $FF; G: $CD / $FF; B: $32 / $FF; A: 1.0);

function NameToColor(const Name: string): TVecColor;

type
  TPixelDepth = (pd24 = 24, pd32 = 32);

  TFastBitmap = record
    DC: HDC;
    Handle: HBITMAP;
    Bits: Pointer;
    Width: Integer;
    Height: Integer;
    Depth: TPixelDepth;
  end;
  PFastBitmap = ^TFastBitmap;
  TFastBitmaps = array of TFastBitmap;

function CreateFastBitmap(Width, Height: Integer; Depth: TPixelDepth = pd24): TFastBitmap;
procedure DestroyFastBitmap(var Bitmap: TFastBitmap);
function IsFastBitmap(const Bitmap: TFastBitmap): Boolean;
procedure AlphaDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap; Opacity: Byte = $FF);

{ Vector Graphics Extensions }

function vguOpenSurface(Width, Height: VGuint): Boolean;
procedure vguCloseSurface;
procedure vguResizeSurface(Width, Height: VGuint);
procedure vguClearSurface(const Color: TVecColor);
procedure vguDrawSurface(DC: HDC; X, Y: VGint);
procedure vguCopySurface(var Bitmap: TFastBitmap);

function vguNewPaint: VGPaint;
procedure vguDeletePaint(Paint: VGPaint);
procedure vguSolidPaint(Paint: VGPaint; const Color: TVecColor);
procedure vguLinearPaint(Paint: VGPaint; const A, B: TVec2; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
procedure vguRadialPaint(Paint: VGPaint; const Center, Focus: TVec2; Radius: Float; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
function vguNewPath: VGPath;
procedure vguDeletePath(Path: VGPath);
procedure vguOpenPath(Path: VGPaint);
procedure vguClosePath(Path: VGPath);
procedure vguClearPath(Path: VGPath);
procedure vguMoveTo(Path: VGPaint; const V: TVec2);
procedure vguLineTo(Path: VGPaint; const V: TVec2);
procedure vguCurveTo(Path: VGPaint; const A, B, C: TVec2);

procedure vguStrokeWidth(Width: Float);
procedure vguStrokePattern(Pattern: array of Float);
procedure vguStrokeCap(Cap: TVecCap);
procedure vguStrokeJoin(Join: TVecJoin);
procedure vguStrokeMiterLimit(MiterLimit: Float);

procedure vguFillPath(Paint: VGPaint; Path: VGPath);
procedure vguStrokePath(Paint: VGPaint; Path: VGPath);

{ IVecObject }

type
  IVecObject = interface
    ['{BCE8B894-8A4E-4114-B126-BEBF5A378288}']
  end;

{ IVecHandle }

  IVecHandle = interface(IVecObject)
    ['{19BCA026-502E-4298-9091-397F675BB735}']
    function GetHandle: VGHandle;
    property Handle: VGHandle read GetHandle;
  end;

{ IVecPaint }

  IVecPaint = interface(IVecHandle)
    ['{5C16A0BE-29CC-490C-BEB6-87A22164039D}']
    function GetCap: TVecCap;
    procedure SetCap(Value: TVecCap);
    function GetFillRule: TVecFillRule;
    procedure SetFillRule(Value: TVecFillRule);
    function GetJoin: TVecJoin;
    procedure SetJoin(Value: TVecJoin);
    function GetMiterLimit: Float;
    procedure SetMiterLimit(Value: Float);
    function GetPattern: TVecPattern;
    procedure SetPattern(const Value: TVecPattern);
    function GetWidth: Float;
    procedure SetWidth(Value: Float);
    procedure Solid(Color: TVecColor);
    procedure Linear(const A, B: TVec2; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
    procedure Radial(const Center, Focus: TVec2; Radius: Float; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
    procedure Conic(Stops: TFloatArray; Colors: TVecColors);
    property Cap: TVecCap read GetCap write SetCap;
    property FillRule: TVecFillRule read GetFillRule write SetFillRule;
    property Join: TVecJoin read GetJoin write SetJoin;
    property MiterLimit: Float read GetMiterLimit write SetMiterLimit;
    property Pattern: TVecPattern read GetPattern write SetPattern;
    property Width: Float read GetWidth write SetWidth;
  end;

{ IVecPath }

  IVecPath = interface(IVecHandle)
    ['{11905C57-F689-4682-B710-C802944F8A1D}']
    procedure Open;
    procedure Close;
    procedure Clear;
    procedure Append(Path: IVecPath);
    procedure AppendData(const Commands: TByteArray; const Data: TVecPoints);
    procedure Arc(X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
    procedure Ellipse(CX, CY, Width, Height: Float); overload;
    procedure Ellipse(const Center: TVecPoint; const Size: TVecSize); overload;
    procedure Rectangle(X, Y, Width, Height: Float); overload;
    procedure Rectangle(const Rect: TVecRect); overload;
    procedure RoundRectangle(X, Y, Width, Height, ArcWidth, ArcHeight: Float); overload;
    procedure RoundRectangle(const Rect: TVecRect; Radius: TVecSize); overload;
    procedure Polygon(const V: TVecPoints);
    procedure MoveTo(X, Y: Float); overload;
    procedure MoveTo(const V: TVecPoint); overload;
    procedure LineTo(X, Y: Float); overload;
    procedure LineTo(V: TVecPoint); overload;
    procedure CurveTo(X1, Y1, X2, Y2, X3, Y3: Float); overload;
    procedure CurveTo(const A, B, C: TVecPoint); overload;
  end;

{ IVecSurface }

  IVecSurface = interface(IVecObject)
    ['{F8B3C793-4BB8-48CF-9A4B-663FBADDA277}']
    procedure Resize(Width, Height: Integer);
    procedure Clear(const Color: TVecColor);
    procedure FillArc(Paint: IVecPaint; X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
    procedure StrokeArc(Paint: IVecPaint; X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
    procedure FillEllipse(Paint: IVecPaint; CX, CY, Width, Height: Float); overload;
    procedure StrokeEllipse(Paint: IVecPaint; CX, CY, Width, Height: Float); overload;
    procedure FillEllipse(Paint: IVecPaint; const Center: TVecPoint; const Size: TVecSize); overload;
    procedure StrokeEllipse(Paint: IVecPaint; const Center: TVecPoint; const Size: TVecSize); overload;
    procedure FillRectangle(Paint: IVecPaint; X, Y, Width, Height: Float); overload;
    procedure StrokeRectangle(Paint: IVecPaint; X, Y, Width, Height: Float); overload;
    procedure FillRectangle(Paint: IVecPaint; const Rect: TVecRect); overload;
    procedure StrokeRectangle(Paint: IVecPaint; const Rect: TVecRect); overload;
    procedure FillRoundRectangle(Paint: IVecPaint; X, Y, Width, Height, ArcWidth, ArcHeight: Float); overload;
    procedure StrokeRoundRectangle(Paint: IVecPaint; X, Y, Width, Height, ArcWidth, ArcHeight: Float); overload;
    procedure FillRoundRectangle(Paint: IVecPaint; const Rect: TVecRect; Radius: TVecSize); overload;
    procedure StrokeRoundRectangle(Paint: IVecPaint; const Rect: TVecRect; Radius: TVecSize); overload;
    procedure FillPolygon(Paint: IVecPaint; const V: TVecPoints);
    procedure StrokePolygon(Paint: IVecPaint; const V: TVecPoints);
    procedure FillPath(Paint: IVecPaint; Path: IVecPath);
    procedure StrokePath(Paint: IVecPaint; Path: IVecPath);
  end;

{ Instance routines }

function VecPaint: IVecPaint;
function VecPath: IVecPath;
function VecSurface: IVecSurface;

implementation

{ Vec routines }

function Vec2(X: Float): TVec2;
begin
  Result.X := X; Result.Y := X;
end;

function Vec2(const X, Y: Float): TVec2;
begin
  Result.X := X; Result.Y := Y;
end;

function Vec2(const V: TVec3): TVec2;
begin
  Result.X := V.X; Result.Y := V.Y;
end;

function Vec2(const V: TVec4): TVec2;
begin
  Result.X := V.X; Result.Y := V.Y;
end;

function Vec3(X: Float): TVec3;
begin
  Result.X := X; Result.Y := X; Result.Z := X;
end;

function Vec3(const X, Y, Z: Float): TVec3;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function Vec3(const V: TVec4): TVec3;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := V.Z;
end;

function Vec4(X: Float): TVec4;
begin
  Result.X := X; Result.Y := X; Result.Z := X; Result.W := X;
end;

function Vec4(const X, Y, Z, W: Float): TVec4;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z; Result.W := W;
end;

function Vec(const X, Y: Float): TVec2;
begin
  Result.X := X; Result.Y := Y;
end;

function Vec(const X, Y, Z: Float): TVec3;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function Vec(const X, Y, Z, W: Float): TVec4;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z; Result.W := W;
end;

function Vec(const V: TVec2; Z: Float): TVec3;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := Z;
end;

function Vec(const V: TVec2; Z, W: Float): TVec4;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := Z; Result.W := W;
end;

function Vec(const V: TVec3; W: Float): TVec4;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := V.Z; Result.W := W;
end;

function ArcTan2(const Y, X: Extended): Extended;
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;

function ArcCos(const X: Extended): Extended;
begin
  Result := ArcTan2(Sqrt(1 - X * X), X);
end;

function Tan(const X: Extended): Extended;
asm
        FLD    X
        FPTAN
        FSTP   ST(0)
        FWAIT
end;

function VecAdd(const A, B: TVec2): TVec2;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function VecSubtract(const A, B: TVec2): TVec2;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function VecMultiply(const A: TVec2; B: Float): TVec2;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
end;

function VecMultiply(const A, B: TVec2): TVec2;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

function VecDivide(const A, B: TVec2): TVec2;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

function VecNormalize(const V: TVec2): TVec2;
var
  Ratio: Float;
begin
  Ratio := Sqrt(V.X * V.X + V.Y * V.Y);
  if Ratio > 0 then
  begin
    Ratio := 1 / Ratio;
    Result.X := V.X * Ratio;
    Result.Y := V.Y * Ratio;
  end
  else
    Result := Vec2(0);
end;

function VecAngle(X1, Y1, X2, Y2, X3, Y3: Float): Float; overload;
var
  D, T: Float;
begin
  X1 := X1 - X2;
  X3 := X3 - X2;
  Y1 := Y1 - Y2;
  Y3 := Y3 - Y2;
  D := (X1 * X1 + Y1 * Y1) * (X3 * X3 + Y3 * Y3);
  if D = 0 then
    Result := 0
  else
  begin
    T :=(X1 * X3 + Y1 * Y3) / Sqrt(D);
    if T = 1.0 then
      Result := 0
    else if T = -1.0 then
      Result := Pi
    else
      Result := ArcCos(T);
  end;
end;

function VecAngle(const A, B, C: TVec2): Float; overload;
begin
  Result := VecAngle(A.X, A.Y, B.X, B.Y, C.X, C.Y);
end;

function VecDistance(const A, B: TVec2): Float;
var
  X, Y: Float;
begin
  X := A.X - B.X;
  Y := A.Y - B.Y;
  Result := Sqrt(X * X + Y * Y);
end;

function VecAverage(const A, B: TVec2): TVec2;
begin
  Result.X := (A.X + B.X) / 2;
  Result.Y := (A.Y + B.Y) / 2;
end;

function VecExtend(const A, B: TVec2; Distance: Float): TVec2;
var
  C: TVec2;
begin
  C := VecSubtract(B, A);
  C := VecNormalize(C);
  C.X := C.X * Distance;
  C.Y := C.Y * Distance;
  Result := VecAdd(B, C);
end;

function VecAdd(const A, B: TVec3): TVec3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

function VecSubtract(const A, B: TVec3): TVec3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

function VecMultiply(const A, B: TVec3): TVec3;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
end;

function VecDivide(const A, B: TVec3): TVec3;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
  Result.Z := A.Z / B.Z;
end;

function VecNormalize(const V: TVec3): TVec3;
var
  Ratio: Float;
begin
  Ratio := Sqrt(V.X * V.X + V.Y * V.Y + V.Z * V.Z);
  if Ratio > 0 then
  begin
    Ratio := 1 / Ratio;
    Result.X := V.X * Ratio;
    Result.Y := V.Y * Ratio;
    Result.X := V.Z * Ratio;
  end
  else
    Result := Vec3(0);
end;

function VecRotate(const V: TVec2; const Angle: Float): TVec2;
var
  Radians: Float;
begin
  Radians :=   Angle * (PI / 180);
  Result.X := (V.X * Cos(Radians)) - (V.Y * Sin(Radians));
  Result.Y := (V.X * Sin(Radians)) + (V.Y * Cos(Radians));
end;

function VecRect(X, Y, Width, Height: Float): TVecRect;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := Width;
  Result.Height := Height;
end;

function VecArray(const Values: array of Float): TFloatArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Values));
  for I := Low(Values) to High(Values) do
    Result[I] := Values[I];
end;

function VecColors(const Colors: array of TVecColor): TVecColors;
var
  I: Integer;
begin
  SetLength(Result, Length(Colors));
  for I := Low(Colors) to High(Colors) do
    Result[I] := Colors[I];
end;

{ Vector Graphics Extensions }

var
  SurfaceWidth, SurfaceHeight: VGint;

function vguOpenSurface(Width, Height: VGuint): Boolean;
begin
  Result := LoadOpenVG and (vgInitContextAM(Width, Width, VG_FALSE) = VG_TRUE);
  if Result then
  begin
    SurfaceWidth := Width;
    SurfaceHeight := Height;
    vgSeti(VG_RENDERING_QUALITY, VG_RENDERING_QUALITY_BETTER);
  end;
end;

procedure vguCloseSurface;
begin
  vgDestroyContextAM;
  SurfaceWidth := 0;
  SurfaceHeight := 0;
  UnloadOpenVG;
end;

procedure vguResizeSurface(Width, Height: VGuint);
begin
  vgResizeSurfaceAM(Width, Height);
  SurfaceWidth := Width;
  SurfaceHeight := Height
end;

procedure vguClearSurface(const Color: TVecColor);
begin
  vgSetfv(VG_CLEAR_COLOR, 4, @Color);
  vgClear(0, 0, SurfaceWidth, SurfaceHeight);
end;

procedure vguDrawSurface(DC: HDC; X, Y: VGint);
var
  MemDC: HDC;
  Bitmap: HBITMAP;
  Bitmapinfo: TBitmapInfo;
  Bits: Pointer;
  Func: TBlendFunction;
begin
  MemDC := CreateCompatibleDC(0);
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := SurfaceWidth;
    biHeight := SurfaceHeight;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
  end;
  Bitmap := CreateDibSection(MemDC, BitmapInfo, DIB_RGB_COLORS, Bits,  0, 0);
  Bitmap := SelectObject(MemDC, Bitmap);
  Move(vgGetSurfacePixelsAM^, Bits^, SurfaceWidth * SurfaceHeight * 4);
  Func.BlendOp := 0;
  Func.BlendFlags := 0;
  Func.SourceConstantAlpha := $FF;
  Func.AlphaFormat := AC_SRC_ALPHA;
  AlphaBlend(DC, X, Y, SurfaceWidth, SurfaceHeight, MemDC, 0, 0, SurfaceWidth, SurfaceHeight, Func);
  Bitmap := SelectObject(MemDC, Bitmap);
  DeleteObject(Bitmap);
  DeleteDC(MemDC);
end;

procedure vguCopySurface(var Bitmap: TFastBitmap);
begin
  if (Bitmap.Width <> SurfaceWidth) or (Bitmap.Height <> SurfaceHeight) then
    DestroyFastBitmap(Bitmap);
  if not IsFastBitmap(Bitmap) then
    Bitmap := CreateFastBitmap(SurfaceWidth, SurfaceHeight, pd32);
  Move(vgGetSurfacePixelsAM^, Bitmap.Bits^, SurfaceWidth * SurfaceHeight * 4);
end;

function vguNewPaint: VGPaint;
begin
  Result := vgCreatePaint;
end;

procedure vguDeletePaint(Paint: VGPaint);
begin
  if @vgDestroyPaint <> nil then
    vgDestroyPaint(Paint);
end;

procedure vguSolidPaint(Paint: VGPaint; const Color: TVecColor);
begin
  vgSetParameteri(Paint, VG_PAINT_TYPE, VG_PAINT_TYPE_COLOR);
  vgSetParameterfv(Paint, VG_PAINT_COLOR, 4, @Color);
end;

procedure vguLinearPaint(Paint: VGPaint; const A, B: TVec2; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
const
  Spreads: array[TVecSpread] of VGint = (VG_COLOR_RAMP_SPREAD_PAD, VG_COLOR_RAMP_SPREAD_REPEAT, VG_COLOR_RAMP_SPREAD_REFLECT);
var
  Gradient: array of Float;
  Line: array[0..1] of TVec2;
  I, J: Integer;
begin
  if (Length(Stops) < 2) or (Length(Stops) <> Length(Colors)) then
    Exit;
  SetLength(Gradient, Length(Stops) * 5);
  for I := Low(Stops) to High(Stops) do
  begin
    J := I * 5;
    Gradient[J] := Stops[I];
    Gradient[J + 1] := Colors[I].R;
    Gradient[J + 2] := Colors[I].G;
    Gradient[J + 3] := Colors[I].B;
    Gradient[J + 4] := Colors[I].A;
  end;
  vgSetParameteri(Paint, VG_PAINT_TYPE, VG_PAINT_TYPE_LINEAR_GRADIENT);
  vgSetParameterfv(Paint, VG_PAINT_COLOR_RAMP_STOPS, Length(Gradient), @Gradient[0]);
  Line[0] := A;
  Line[1] := B;
  vgSetParameterfv(Paint, VG_PAINT_LINEAR_GRADIENT, 4, @Line);
  vgSetParameteri(Paint, VG_PAINT_COLOR_RAMP_SPREAD_MODE, Spreads[Spread]);
end;

procedure vguRadialPaint(Paint: VGPaint; const Center, Focus: TVec2; Radius: Float; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
const
  Spreads: array[TVecSpread] of VGint = (VG_COLOR_RAMP_SPREAD_PAD, VG_COLOR_RAMP_SPREAD_REPEAT, VG_COLOR_RAMP_SPREAD_REFLECT);
var
  Gradient: array of Float;
  Line: array[0..5] of Float;
  I, J: Integer;
begin
  if (Length(Stops) < 2) or (Length(Stops) <> Length(Colors)) then
    Exit;
  SetLength(Gradient, Length(Stops) * 5);
  for I := Low(Stops) to High(Stops) do
  begin
    J := I * 5;
    Gradient[J] := Stops[I];
    Gradient[J + 1] := Colors[I].R;
    Gradient[J + 2] := Colors[I].G;
    Gradient[J + 3] := Colors[I].B;
    Gradient[J + 4] := Colors[I].A;
  end;
  vgSetParameteri(Paint, VG_PAINT_TYPE, VG_PAINT_TYPE_RADIAL_GRADIENT);
  vgSetParameterfv(Paint, VG_PAINT_COLOR_RAMP_STOPS, Length(Gradient), @Gradient[0]);
  Line[0] := Center.X;
  Line[1] := Center.Y;
  Line[2] := Focus.X;
  Line[3] := Focus.Y;
  Line[4] := Radius;
  vgSetParameterfv(Paint, VG_PAINT_RADIAL_GRADIENT, 5, @Line);
  vgSetParameteri(Paint, VG_PAINT_COLOR_RAMP_SPREAD_MODE, Spreads[Spread]);
end;

function vguNewPath: VGPath;
begin
  Result := vgCreatePath(VG_PATH_FORMAT_STANDARD, VG_PATH_DATATYPE_F,
    1, 0, 0, 0, VG_PATH_CAPABILITY_ALL);
end;

procedure vguDeletePath(Path: VGPath);
begin
  if @vgDestroyPath <> nil then
    vgDestroyPath(Path);
end;

procedure vguClearPath(Path: VGPath);
begin
  vgClearPath(Path, VG_PATH_CAPABILITY_ALL);
end;

procedure vguOpenPath(Path: VGPath);
begin
  vgClearPath(Path, VG_PATH_CAPABILITY_ALL);
end;

procedure vguClosePath(Path: VGPaint);
var
  Command: VGubyte;
  Dummy: Float;
begin
  Command := VG_CLOSE_PATH;
  Dummy := 0;
  vgAppendPathData(Path, 1, @Command, @Dummy);
end;

procedure vguMoveTo(Path: VGPaint; const V: TVec2);
var
  Command: VGubyte;
begin
  Command := VG_MOVE_TO_ABS;
  vgAppendPathData(Path, 1, @Command, @V);
end;

procedure vguLineTo(Path: VGPaint; const V: TVec2);
var
  Command: VGubyte;
begin
  Command := VG_LINE_TO_ABS;
  vgAppendPathData(Path, 1, @Command, @V);
end;

procedure vguCurveTo(Path: VGPaint; const A, B, C: TVec2);
var
  Command: VGubyte;
  Data: array [0..2] of TVec2;
begin
  Command := VG_CUBIC_TO_ABS;
  Data[0] := A;
  Data[1] := B;
  Data[2] := C;
  vgAppendPathData(Path, 1, @Command, @Data[0]);
end;

procedure vguStrokeWidth(Width: Float);
begin
  vgSetf(VG_STROKE_LINE_WIDTH, Width);
end;

procedure vguStrokePattern(Pattern: array of Float);
var
  I: Float;
begin
  if Length(Pattern) < 2 then
  begin
    I := 1;
    vgSetfv(VG_STROKE_DASH_PATTERN, 1, @I);
  end
  else
    vgSetfv(VG_STROKE_DASH_PATTERN, Length(Pattern), @Pattern[0]);
end;

procedure vguStrokeCap(Cap: TVecCap);
const
  Caps: array[TVecCap] of VGint = (VG_CAP_BUTT, VG_CAP_ROUND, VG_CAP_SQUARE);
begin
  vgSeti(VG_STROKE_CAP_STYLE, Caps[Cap]);
end;

procedure vguStrokeJoin(Join: TVecJoin);
const
  Joins: array[TVecJoin] of VGint = (VG_JOIN_MITER, VG_JOIN_ROUND, VG_JOIN_BEVEL);
begin
  vgSeti(VG_STROKE_JOIN_STYLE, Joins[Join]);
end;

procedure vguStrokeMiterLimit(MiterLimit: Float);
begin
  vgSetf(VG_STROKE_MITER_LIMIT, MiterLimit);
end;

procedure vguFillPath(Paint: VGPaint; Path: VGPath);
begin
  vgSetPaint(Paint, VG_FILL_PATH);
  vgDrawPath(Path, VG_FILL_PATH);
end;

procedure vguStrokePath(Paint: VGPaint; Path: VGPath);
begin
  vgSetPaint(Paint, VG_STROKE_PATH);
  vgDrawPath(Path, VG_STROKE_PATH);
end;

function UpperCase(const S: string): string;
var
  Ch: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'a') and (Ch <= 'z') then Dec(Ch, 32);
    Dest^ := Ch;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

function NameToColor(const Name: string): TVecColor;
var
  S: string;
begin
  S :=  UpperCase(Name);
  if S = 'CLEARCOLOR' then
    Result := vgClearColor
  else if S = 'ALICEBLUE' then
    Result := vgAliceBlue
  else if S = 'ANTIQUEWHITE' then
    Result := vgAntiqueWhite
  else if S = 'AQUA' then
    Result := vgAqua
  else if S = 'AQUAMARINE' then
    Result := vgAquamarine
  else if S = 'AZURE' then
    Result := vgAzure
  else if S = 'BEIGE' then
    Result := vgBeige
  else if S = 'BISQUE' then
    Result := vgBisque
  else if S = 'BLACK' then
    Result := vgBlack
  else if S = 'BLANCHEDALMOND' then
    Result := vgBlanchedAlmond
  else if S = 'BLUE' then
    Result := vgBlue
  else if S = 'BLUEVIOLET' then
    Result := vgBlueViolet
  else if S = 'BROWN' then
    Result := vgBrown
  else if S = 'BURLYWOOD' then
    Result := vgBurlyWood
  else if S = 'CADETBLUE' then
    Result := vgCadetBlue
  else if S = 'CHARTREUSE' then
    Result := vgChartreuse
  else if S = 'CHOCOLATE' then
    Result := vgChocolate
  else if S = 'CORAL' then
    Result := vgCoral
  else if S = 'CORNFLOWERBLUE' then
    Result := vgCornflowerBlue
  else if S = 'CORNSILK' then
    Result := vgCornsilk
  else if S = 'CRIMSON' then
    Result := vgCrimson
  else if S = 'CYAN' then
    Result := vgCyan
  else if S = 'DARKBLUE' then
    Result := vgDarkBlue
  else if S = 'DARKCYAN' then
    Result := vgDarkCyan
  else if S = 'DARKGOLDENROD' then
    Result := vgDarkGoldenrod
  else if S = 'DARKGRAY' then
    Result := vgDarkGray
  else if S = 'DARKGREEN' then
    Result := vgDarkGreen
  else if S = 'DARKKHAKI' then
    Result := vgDarkKhaki
  else if S = 'DARKMAGENTA' then
    Result := vgDarkMagenta
  else if S = 'DARKOLIVEGREEN' then
    Result := vgDarkOliveGreen
  else if S = 'DARKORANGE' then
    Result := vgDarkOrange
  else if S = 'DARKORCHID' then
    Result := vgDarkOrchid
  else if S = 'DARKRED' then
    Result := vgDarkRed
  else if S = 'DARKSALMON' then
    Result := vgDarkSalmon
  else if S = 'DARKSEAGREEN' then
    Result := vgDarkSeaGreen
  else if S = 'DARKSLATEBLUE' then
    Result := vgDarkSlateBlue
  else if S = 'DARKSLATEGRAY' then
    Result := vgDarkSlateGray
  else if S = 'DARKTURQUOISE' then
    Result := vgDarkTurquoise
  else if S = 'DARKVIOLET' then
    Result := vgDarkViolet
  else if S = 'DEEPPINK' then
    Result := vgDeepPink
  else if S = 'DEEPSKYBLUE' then
    Result := vgDeepSkyBlue
  else if S = 'DIMGRAY' then
    Result := vgDimGray
  else if S = 'DODGERBLUE' then
    Result := vgDodgerBlue
  else if S = 'FIREBRICK' then
    Result := vgFirebrick
  else if S = 'FLORALWHITE' then
    Result := vgFloralWhite
  else if S = 'FORESTGREEN' then
    Result := vgForestGreen
  else if S = 'FUCHSIA' then
    Result := vgFuchsia
  else if S = 'GAINSBORO' then
    Result := vgGainsboro
  else if S = 'GHOSTWHITE' then
    Result := vgGhostWhite
  else if S = 'GOLD' then
    Result := vgGold
  else if S = 'GOLDENROD' then
    Result := vgGoldenrod
  else if S = 'GRAY' then
    Result := vgGray
  else if S = 'GREEN' then
    Result := vgGreen
  else if S = 'GREENYELLOW' then
    Result := vgGreenYellow
  else if S = 'HONEYDEW' then
    Result := vgHoneydew
  else if S = 'HOTPINK' then
    Result := vgHotPink
  else if S = 'INDIANRED' then
    Result := vgIndianRed
  else if S = 'INDIGO' then
    Result := vgIndigo
  else if S = 'IVORY' then
    Result := vgIvory
  else if S = 'KHAKI' then
    Result := vgKhaki
  else if S = 'LAVENDER' then
    Result := vgLavender
  else if S = 'LAVENDERBLUSH' then
    Result := vgLavenderBlush
  else if S = 'LAWNGREEN' then
    Result := vgLawnGreen
  else if S = 'LEMONCHIFFON' then
    Result := vgLemonChiffon
  else if S = 'LIGHTBLUE' then
    Result := vgLightBlue
  else if S = 'LIGHTCORAL' then
    Result := vgLightCoral
  else if S = 'LIGHTCYAN' then
    Result := vgLightCyan
  else if S = 'LIGHTGOLDENRODYELLOW' then
    Result := vgLightGoldenrodYellow
  else if S = 'LIGHTGRAY' then
    Result := vgLightGray
  else if S = 'LIGHTGREEN' then
    Result := vgLightGreen
  else if S = 'LIGHTPINK' then
    Result := vgLightPink
  else if S = 'LIGHTSALMON' then
    Result := vgLightSalmon
  else if S = 'LIGHTSEAGREEN' then
    Result := vgLightSeaGreen
  else if S = 'LIGHTSKYBLUE' then
    Result := vgLightSkyBlue
  else if S = 'LIGHTSLATEGRAY' then
    Result := vgLightSlateGray
  else if S = 'LIGHTSTEELBLUE' then
    Result := vgLightSteelBlue
  else if S = 'LIGHTYELLOW' then
    Result := vgLightYellow
  else if S = 'LIME' then
    Result := vgLime
  else if S = 'LIMEGREEN' then
    Result := vgLimeGreen
  else if S = 'LINEN' then
    Result := vgLinen
  else if S = 'MAGENTA' then
    Result := vgMagenta
  else if S = 'MAROON' then
    Result := vgMaroon
  else if S = 'MEDIUMAQUAMARINE' then
    Result := vgMediumAquamarine
  else if S = 'MEDIUMBLUE' then
    Result := vgMediumBlue
  else if S = 'MEDIUMORCHID' then
    Result := vgMediumOrchid
  else if S = 'MEDIUMPURPLE' then
    Result := vgMediumPurple
  else if S = 'MEDIUMSEAGREEN' then
    Result := vgMediumSeaGreen
  else if S = 'MEDIUMSLATEBLUE' then
    Result := vgMediumSlateBlue
  else if S = 'MEDIUMSPRINGGREEN' then
    Result := vgMediumSpringGreen
  else if S = 'MEDIUMTURQUOISE' then
    Result := vgMediumTurquoise
  else if S = 'MEDIUMVIOLETRED' then
    Result := vgMediumVioletRed
  else if S = 'MIDNIGHTBLUE' then
    Result := vgMidnightBlue
  else if S = 'MINTCREAM' then
    Result := vgMintCream
  else if S = 'MISTYROSE' then
    Result := vgMistyRose
  else if S = 'MOCCASIN' then
    Result := vgMoccasin
  else if S = 'NAVAJOWHITE' then
    Result := vgNavajoWhite
  else if S = 'NAVY' then
    Result := vgNavy
  else if S = 'OLDLACE' then
    Result := vgOldLace
  else if S = 'OLIVE' then
    Result := vgOlive
  else if S = 'OLIVEDRAB' then
    Result := vgOliveDrab
  else if S = 'ORANGE' then
    Result := vgOrange
  else if S = 'ORANGERED' then
    Result := vgOrangeRed
  else if S = 'ORCHID' then
    Result := vgOrchid
  else if S = 'PALEGOLDENROD' then
    Result := vgPaleGoldenrod
  else if S = 'PALEGREEN' then
    Result := vgPaleGreen
  else if S = 'PALETURQUOISE' then
    Result := vgPaleTurquoise
  else if S = 'PALEVIOLETRED' then
    Result := vgPaleVioletRed
  else if S = 'PAPAYAWHIP' then
    Result := vgPapayaWhip
  else if S = 'PEACHPUFF' then
    Result := vgPeachPuff
  else if S = 'PERU' then
    Result := vgPeru
  else if S = 'PINK' then
    Result := vgPink
  else if S = 'PLUM' then
    Result := vgPlum
  else if S = 'POWDERBLUE' then
    Result := vgPowderBlue
  else if S = 'PURPLE' then
    Result := vgPurple
  else if S = 'RED' then
    Result := vgRed
  else if S = 'ROSYBROWN' then
    Result := vgRosyBrown
  else if S = 'ROYALBLUE' then
    Result := vgRoyalBlue
  else if S = 'SADDLEBROWN' then
    Result := vgSaddleBrown
  else if S = 'SALMON' then
    Result := vgSalmon
  else if S = 'SANDYBROWN' then
    Result := vgSandyBrown
  else if S = 'SEAGREEN' then
    Result := vgSeaGreen
  else if S = 'SEASHELL' then
    Result := vgSeaShell
  else if S = 'SIENNA' then
    Result := vgSienna
  else if S = 'SILVER' then
    Result := vgSilver
  else if S = 'SKYBLUE' then
    Result := vgSkyBlue
  else if S = 'SLATEBLUE' then
    Result := vgSlateBlue
  else if S = 'SLATEGRAY' then
    Result := vgSlateGray
  else if S = 'SNOW' then
    Result := vgSnow
  else if S = 'SPRINGGREEN' then
    Result := vgSpringGreen
  else if S = 'STEELBLUE' then
    Result := vgSteelBlue
  else if S = 'TAN' then
    Result := vgTan
  else if S = 'TEAL' then
    Result := vgTeal
  else if S = 'THISTLE' then
    Result := vgThistle
  else if S = 'TOMATO' then
    Result := vgTomato
  else if S = 'TRANSPARENT' then
    Result := vgTransparent
  else if S = 'TURQUOISE' then
    Result := vgTurquoise
  else if S = 'VIOLET' then
    Result := vgViolet
  else if S = 'WHEAT' then
    Result := vgWheat
  else if S = 'WHITE' then
    Result := vgWhite
  else if S = 'WHITESMOKE' then
    Result := vgWhiteSmoke
  else if S = 'YELLOW' then
    Result := vgYellow
  else if S = 'YELLOWGREEN' then
    Result := vgYellowGreen
  else
    Result := vgBlack;
end;

function CreateFastBitmap(Width, Height: Integer; Depth: TPixelDepth = pd24): TFastBitmap;
var
  BitmapInfo: TBitmapinfo;
begin
  if (Width < 1) or (Height < 1) then
  begin
    FillChar(Result, SizeOf(Result), #0);
    Exit;
  end;
  Result.DC := CreateCompatibleDC(0);
  FillChar(BitmapInfo, SizeOf(BitmapInfo), #0);
  with BitmapInfo.bmiHeader do
  begin
    biSize := SizeOf(BitmapInfo.bmiHeader);
    biWidth := Width;
    biHeight := Height;
    biPlanes := 1;
    biBitCount := Ord(Depth);
    biCompression := BI_RGB;
  end;
  with Result do
    Handle := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
  Result.Width := Width;
  Result.Height := Height;
  Result.Depth := Depth;
  with Result do
    SelectObject(DC, Handle);
end;

procedure DestroyFastBitmap(var Bitmap: TFastBitmap);
begin
  if Bitmap.DC <> 0 then
  begin
    DeleteDC(Bitmap.DC);
    DeleteObject(Bitmap.Handle);
    FillChar(Bitmap, SizeOf(Bitmap), #0);
  end;
end;

function IsFastBitmap(const Bitmap: TFastBitmap): Boolean;
begin
  Result := Bitmap.DC <> 0;
end;

procedure AlphaDraw(DC: HDC; X, Y: Integer; const Bitmap: TFastBitmap; Opacity: Byte = $FF);
var
  Func: TBlendFunction;
begin
  if IsFastBitmap(Bitmap) and (Bitmap.Depth = pd32) then
  begin
    Func.BlendOp := 0;
    Func.BlendFlags := 0;
    Func.SourceConstantAlpha := Opacity;
    Func.AlphaFormat := AC_SRC_ALPHA;
    AlphaBlend(DC, X, Y, Bitmap.Width,
      Bitmap.Height, Bitmap.DC, 0, 0, Bitmap.Width, Bitmap.Height, Func);
  end;
end;

{ Vector Objects }

type
  TVecPaint = class(TInterfacedObject, IVecObject, IVecHandle, IVecPaint)
  private
    FHandle: VGhandle;
    FCap: TVecCap;
    FFillRule: TVecFillRule;
    FJoin: TVecJoin;
    FMiterLimit: Float;
    FPattern: TVecPattern;
    FWidth: Float;
  public
    constructor Create;
    destructor Destroy; override;
    { IVecHandle }
    function GetHandle: VGHandle;
    { IVecPaint }
    function GetCap: TVecCap;
    procedure SetCap(Value: TVecCap);
    function GetFillRule: TVecFillRule;
    procedure SetFillRule(Value: TVecFillRule);
    function GetJoin: TVecJoin;
    procedure SetJoin(Value: TVecJoin);
    function GetMiterLimit: Float;
    procedure SetMiterLimit(Value: Float);
    function GetPattern: TVecPattern;
    procedure SetPattern(const Value: TVecPattern);
    function GetWidth: Float;
    procedure SetWidth(Value: Float);
    procedure Solid(Color: TVecColor);
    procedure Linear(const A, B: TVec2; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
    procedure Radial(const A, B: TVec2; Radius: Float; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
    procedure Conic(Stops: TFloatArray; Colors: TVecColors);
  end;

{ TVecPath }

  TVecPath = class(TInterfacedObject, IVecObject, IVecHandle, IVecPath)
  private
    FHandle: VGhandle;
  public
    constructor Create;
    destructor Destroy; override;
    { IVecHandle }
    function GetHandle: VGHandle;
    { IVecPaint }
    procedure Open;
    procedure Close;
    procedure Clear;
    procedure Append(Path: IVecPath);
    procedure AppendData(const Commands: TByteArray; const Data: TVecPoints);
    procedure Arc(X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
    procedure Ellipse(CX, CY, Width, Height: Float); overload;
    procedure Ellipse(const Center: TVecPoint; const Size: TVecSize); overload;
    procedure Rectangle(X, Y, Width, Height: Float); overload;
    procedure Rectangle(const Rect: TVecRect); overload;
    procedure RoundRectangle(X, Y, Width, Height, ArcWidth, ArcHeight: Float); overload;
    procedure RoundRectangle(const Rect: TVecRect; Radius: TVecSize); overload;
    procedure Polygon(const V: TVecPoints);
    procedure MoveTo(X, Y: Float); overload;
    procedure MoveTo(const V: TVecPoint); overload;
    procedure LineTo(X, Y: Float); overload;
    procedure LineTo(V: TVecPoint); overload;
    procedure CurveTo(X1, Y1, X2, Y2, X3, Y3: Float); overload;
    procedure CurveTo(const A, B, C: TVecPoint); overload;
  end;

{ TVecSurface }

  TVecSurface = class(TInterfacedObject, IVecObject, IVecSurface)
    { IVecSurface }
    procedure Resize(Width, Height: Integer);
    procedure Clear(const Color: TVecColor);
    procedure FillArc(Paint: IVecPaint; X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
    procedure StrokeArc(Paint: IVecPaint; X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
    procedure FillEllipse(Paint: IVecPaint; CX, CY, Width, Height: Float); overload;
    procedure StrokeEllipse(Paint: IVecPaint; CX, CY, Width, Height: Float); overload;
    procedure FillEllipse(Paint: IVecPaint; const Center: TVecPoint; const Size: TVecSize); overload;
    procedure StrokeEllipse(Paint: IVecPaint; const Center: TVecPoint; const Size: TVecSize); overload;
    procedure FillRectangle(Paint: IVecPaint; X, Y, Width, Height: Float); overload;
    procedure StrokeRectangle(Paint: IVecPaint; X, Y, Width, Height: Float); overload;
    procedure FillRectangle(Paint: IVecPaint; const Rect: TVecRect); overload;
    procedure StrokeRectangle(Paint: IVecPaint; const Rect: TVecRect); overload;
    procedure FillRoundRectangle(Paint: IVecPaint; X, Y, Width, Height, ArcWidth, ArcHeight: Float); overload;
    procedure StrokeRoundRectangle(Paint: IVecPaint; X, Y, Width, Height, ArcWidth, ArcHeight: Float); overload;
    procedure FillRoundRectangle(Paint: IVecPaint; const Rect: TVecRect; Radius: TVecSize); overload;
    procedure StrokeRoundRectangle(Paint: IVecPaint; const Rect: TVecRect; Radius: TVecSize); overload;
    procedure FillPolygon(Paint: IVecPaint; const V: TVecPoints);
    procedure StrokePolygon(Paint: IVecPaint; const V: TVecPoints);
    procedure FillPath(Paint: IVecPaint; Path: IVecPath);
    procedure StrokePath(Paint: IVecPaint; Path: IVecPath);
  end;

{ TVecPaint }

constructor TVecPaint.Create;
begin
  inherited Create;
  FHandle := vguNewPaint;
  FMiterLimit := vgGetf(VG_STROKE_MITER_LIMIT);
  FWidth := 1;
end;

destructor TVecPaint.Destroy;
begin
  vguDeletePaint(FHandle);
  inherited Destroy;
end;

{ TVecPaint.IVecHandle }

function TVecPaint.GetHandle: VGHandle;
begin
  Result := FHandle;
end;

{ TVecPaint.IVecPaint }

function TVecPaint.GetCap: TVecCap;
begin
  Result := FCap;
end;

procedure TVecPaint.SetCap(Value: TVecCap);
begin
  FCap := Value;
end;

function TVecPaint.GetFillRule: TVecFillRule;
begin
  Result := FFillRule;
end;

procedure TVecPaint.SetFillRule(Value: TVecFillRule);
begin
  FFillRule := Value;
end;

function TVecPaint.GetJoin: TVecJoin;
begin
  Result := FJoin;
end;

procedure TVecPaint.SetJoin(Value: TVecJoin);
begin
  FJoin := Value;
end;

function TVecPaint.GetMiterLimit: Float;
begin
  Result := FMiterLimit;
end;

procedure TVecPaint.SetMiterLimit(Value: Float);
begin
  FMiterLimit := Value;
end;

function TVecPaint.GetPattern: TVecPattern;
begin
  Result := FPattern;
end;

procedure TVecPaint.SetPattern(const Value: TVecPattern);
begin
  FPattern := Value;
end;

function TVecPaint.GetWidth: Float;
begin
  Result := FWidth;
end;

procedure TVecPaint.SetWidth(Value: Float);
begin
  FWidth := Value;
end;

procedure TVecPaint.Solid(Color: TVecColor);
begin
  vguSolidPaint(FHandle, Color);
end;

procedure TVecPaint.Linear(const A, B: TVec2; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
begin
  vguLinearPaint(FHandle, A, B, Stops, Colors, Spread);
end;

procedure TVecPaint.Radial(const A, B: TVec2; Radius: Float; const Stops: TFloatArray; const Colors: TVecColors; Spread: TVecSpread = spPad);
begin
  vguRadialPaint(FHandle, A, B, Radius, Stops, Colors, Spread);
end;

procedure TVecPaint.Conic(Stops: TFloatArray; Colors: TVecColors);
begin
end;

{ TVecPath }

constructor TVecPath.Create;
begin
  inherited Create;
  FHandle := vguNewPath;
end;

destructor TVecPath.Destroy;
begin
  vguDeletePath(FHandle);
  inherited Destroy;
end;

{ TVecPath.IVecHandle }

function TVecPath.GetHandle: VGHandle;
begin
  Result := FHandle;
end;

{ TVecPath.IVecPaint }

procedure TVecPath.Open;
begin
  vguClearPath(FHandle);
end;

procedure TVecPath.Close;
begin
end;

procedure TVecPath.Clear;
begin
  vguClearPath(FHandle);
end;

procedure TVecPath.Append(Path: IVecPath);
begin
  vgAppendPath(FHandle, Path.Handle);
end;

procedure TVecPath.AppendData(const Commands: TByteArray; const Data: TVecPoints);
begin
  vgAppendPathData(FHandle, Length(Commands), @Commands[0], @Data[0]);
end;

procedure TVecPath.Arc(X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
const
  ArcKinds: array[TArcKind] of VGint = (VGU_ARC_OPEN, VGU_ARC_CHORD, VGU_ARC_PIE);
begin
  vguArc(FHandle, X, Y, Width, Height, StartAngle, AngleExtent, ArcKinds[Kind]);
end;

procedure TVecPath.Ellipse(CX, CY, Width, Height: Float);
begin
  vguEllipse(FHandle, CX, CY, Width, Height);
end;

procedure TVecPath.Ellipse(const Center: TVecPoint; const Size: TVecSize);
begin
  vguEllipse(FHandle, Center.X, Center.Y, Size.X, Size.Y);
end;

procedure TVecPath.Rectangle(X, Y, Width, Height: Float);
begin
  vguRect(FHandle, X, Y, Width, Height);
end;

procedure TVecPath.Rectangle(const Rect: TVecRect);
begin
  with Rect do
    vguRect(FHandle, X, Y, Width, Height);
end;

procedure TVecPath.RoundRectangle(X, Y, Width, Height, ArcWidth, ArcHeight: Float);
begin
  vguRoundRect(FHandle, X, Y, Width, Height, ArcWidth, ArcHeight);
end;

procedure TVecPath.RoundRectangle(const Rect: TVecRect; Radius: TVecSize);
begin
  with Rect do
    vguRoundRect(FHandle, X, Y, Width, Height, Radius.X, Radius.Y);
end;

procedure TVecPath.Polygon(const V: TVecPoints);
var
  Commands: TByteArray;
  I: Integer;
begin
  if Length(V) < 1 then Exit;
  SetLength(Commands, Length(V));
  for I := Low(Commands) to High(Commands) do
    Commands[I] := VG_LINE_TO;
  Commands[0] := VG_MOVE_TO;
  vgAppendPathData(FHandle, Length(V), @Commands[0], @V[0]);
end;

procedure TVecPath.MoveTo(X, Y: Float);
begin
  MoveTo(Vec(X, Y));
end;

procedure TVecPath.MoveTo(const V: TVecPoint);
var
  C: VGbyte;
begin
  C := VG_MOVE_TO;
  vgAppendPathData(FHandle, 1, @C, @V);
end;

procedure TVecPath.LineTo(X, Y: Float);
begin
  LineTo(Vec(X, Y));
end;

procedure TVecPath.LineTo(V: TVecPoint);
var
  C: VGbyte;
begin
  C := VG_LINE_TO;
  vgAppendPathData(FHandle, 1, @C, @V);
end;

procedure TVecPath.CurveTo(X1, Y1, X2, Y2, X3, Y3: Float);
begin
  CurveTo(Vec(X1, Y1), Vec(X2, Y2), Vec(X3, Y3));
end;

procedure TVecPath.CurveTo(const A, B, C: TVecPoint);
var
  Command: VGbyte;
  D: TVecPoints;
begin
  Command := VG_CUBIC_TO;
  SetLength(D, 3);
  D[0] := A;
  D[1] := B;
  D[2] := C;
  vgAppendPathData(FHandle, 1, @Command, @D[0]);
end;

{ TVecSurface.IVecSurface }

procedure TVecSurface.Resize(Width, Height: Integer);
begin
  if @vgGetError = nil then
    vguOpenSurface(Width, Height)
  else
    vguResizeSurface(Width, Height);
end;

procedure TVecSurface.Clear(const Color: TVecColor);
begin
  vguClearSurface(Color);
end;

procedure TVecSurface.FillArc(Paint: IVecPaint; X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Arc(X, Y, Width, Height, StartAngle, AngleExtent, Kind);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokeArc(Paint: IVecPaint; X, Y, Width, Height, StartAngle, AngleExtent: Float; Kind: TArcKind);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Arc(X, Y, Width, Height, StartAngle, AngleExtent, Kind);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillEllipse(Paint: IVecPaint; CX, CY, Width, Height: Float);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Ellipse(CX, CY, Width, Height);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokeEllipse(Paint: IVecPaint; CX, CY, Width, Height: Float);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Ellipse(CX, CY, Width, Height);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillEllipse(Paint: IVecPaint; const Center: TVecPoint; const Size: TVecSize);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Ellipse(Center, Size);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokeEllipse(Paint: IVecPaint; const Center: TVecPoint; const Size: TVecSize);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Ellipse(Center, Size);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillRectangle(Paint: IVecPaint; X, Y, Width, Height: Float);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Rectangle(X, Y, Width, Height);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokeRectangle(Paint: IVecPaint; X, Y, Width, Height: Float);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Rectangle(X, Y, Width, Height);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillRectangle(Paint: IVecPaint; const Rect: TVecRect);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Rectangle(Rect);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokeRectangle(Paint: IVecPaint; const Rect: TVecRect);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Rectangle(Rect);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillRoundRectangle(Paint: IVecPaint; X, Y, Width, Height, ArcWidth, ArcHeight: Float);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.RoundRectangle(X, Y, Width, Height, ArcWidth, ArcHeight);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokeRoundRectangle(Paint: IVecPaint; X, Y, Width, Height, ArcWidth, ArcHeight: Float);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.RoundRectangle(X, Y, Width, Height, ArcWidth, ArcHeight);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillRoundRectangle(Paint: IVecPaint; const Rect: TVecRect; Radius: TVecSize);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.RoundRectangle(Rect, Radius);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokeRoundRectangle(Paint: IVecPaint; const Rect: TVecRect; Radius: TVecSize);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.RoundRectangle(Rect, Radius);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillPolygon(Paint: IVecPaint; const V: TVecPoints);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Polygon(V);
  FillPath(Paint, Path);
end;

procedure TVecSurface.StrokePolygon(Paint: IVecPaint; const V: TVecPoints);
var
  Path: IVecPath;
begin
  Path := VecPath;
  Path.Polygon(V);
  StrokePath(Paint, Path);
end;

procedure TVecSurface.FillPath(Paint: IVecPaint; Path: IVecPath);
begin
  vgSetPaint(Paint.Handle, VG_FILL_PATH);
  vgDrawPath(Path.Handle, VG_FILL_PATH);
end;

procedure TVecSurface.StrokePath(Paint: IVecPaint; Path: IVecPath);
begin
  vguStrokeWidth(Paint.Width);
  vguStrokePattern(Paint.Pattern);
  vguStrokeCap(Paint.Cap);
  vguStrokeJoin(Paint.Join);
  vguStrokeMiterLimit(Paint.MiterLimit);
  vgSetPaint(Paint.Handle, VG_STROKE_PATH);
  vgDrawPath(Path.Handle, VG_STROKE_PATH);
end;

{ Instance routines }

function VecPaint: IVecPaint;
begin
  Result := TVecPaint.Create;
end;

function VecPath: IVecPath;
begin
  Result := TVecPath.Create;
end;

function VecSurface: IVecSurface;
begin
  Result := TVecSurface.Create;
end;

end.
