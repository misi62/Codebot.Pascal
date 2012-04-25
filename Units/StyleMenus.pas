
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit StyleMenus;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Menus, ImgList,
  BaseTypes, GraphTools, FormTools, ProviderTools;

type
  TExcludeMenuEvent = procedure(Sender: TObject; Menu: TMenu; var Exclude: Boolean) of object;

  TMenuStylizer = class(TComponent)
  private
    FBorderColor: TColor;
    FEnabled: Boolean;
    FMarginColor: TColor;
    FMarginWidth: Integer;
    FSelectColor: TColor;
    FSelectFill: TColor;
    FSeparatorBlend: Boolean;
    FSeparatorColor: TColor;
    FShadow: Boolean;
    FOnExcludeMenu: TExcludeMenuEvent;
    procedure SetEnabled(Value: Boolean);
    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);
    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
  protected
    procedure Loaded; override;
    function ExcludeMenu(Menu: TMenu): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;
  published
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property MarginColor: TColor read FMarginColor write FMarginColor;
    property MarginWidth: Integer read FMarginWidth write FMarginWidth;
    property SelectColor: TColor read FSelectColor write FSelectColor;
    property SelectFill: TColor read FSelectFill write FSelectFill;
    property SeparatorBlend: Boolean read FSeparatorBlend write FSeparatorBlend;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor;
    property Shadow: Boolean read FShadow write FShadow;
    property OnExcludeMenu: TExcludeMenuEvent read FOnExcludeMenu write FOnExcludeMenu;
  end;

implementation

{ TMenuStylizer }

const
  RootMargin = 5;
  ItemMargin = 24;

constructor TMenuStylizer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBorderColor := clMenuBar;
  FEnabled := True;
  FMarginColor := clBtnFace;
  FMarginWidth := ItemMargin;
  FSelectColor := Blend(clHighlight, clWindow, 75);
  FSelectFill := Blend(clHighlight, clWindow, 25);
  FSeparatorBlend := True;
  FSeparatorColor := Blend(clThemeBkgnd, clMenu);
  FShadow := True;
  Update;
end;

destructor TMenuStylizer.Destroy;
begin
  if (Owner <> nil) and (not (csDestroying in Owner.ComponentState)) then
    Enabled := False;
  inherited Destroy;
end;

procedure TMenuStylizer.Loaded;
begin
  inherited Loaded;
  Update;
end;

function TMenuStylizer.ExcludeMenu(Menu: TMenu): Boolean;
begin
  Result := False;
  if Assigned(FOnExcludeMenu) then
    FOnExcludeMenu(Self, Menu, Result);
end;

procedure TMenuStylizer.Update;

  procedure RecurseUpdate(Comp: TComponent);
  var
    Menu: TMenu;
    Item: TMenuItem;
    A: TMenuMeasureItemEvent;
    B: TMenuMeasureItemEvent;
    C: Integer absolute A;
    D: Integer absolute B;
    I: Integer;
  begin
    for I := 0 to Comp.ComponentCount - 1 do
    begin
      if Comp.Components[I] is TMenu then
        Menu := Comp.Components[I] as TMenu
      else
        Menu := nil;
      if Menu <> nil then
      begin
        if ExcludeMenu(Menu) then
          Continue
        else
          Menu.OwnerDraw := Enabled;
      end
      else if Comp.Components[I] is TMenuItem then
      begin
        Item := Comp.Components[I] as TMenuItem;
        if Enabled then
        begin
          A := Item.OnMeasureItem;
          B := MeasureItem;
          if C <> D  then
          begin
            Item.Caption := Item.Caption + ' ';
            Item.Caption := Trim(Item.Caption);
          end;
          Item.OnAdvancedDrawItem := AdvancedDrawItem;
          Item.OnMeasureItem := MeasureItem;
        end
        else
        begin
          Item.OnAdvancedDrawItem := AdvancedDrawItem;
          Item.OnMeasureItem := MeasureItem;
        end;
      end;
      RecurseUpdate(Comp.Components[I]);
    end;
  end;

begin
  RecurseUpdate(Owner);
end;

procedure TMenuStylizer.AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState);
const
  ShadowWidth = 3;
var
  Item: TMenuItem absolute Sender;
  Font: TFont;
  F: HFONT;
  M: TMenu;
  DC: HDC;
  Root: Boolean;
  R, C, Clip: TRect;
  S: TSize;
begin
  DC := ACanvas.Handle;
  R := ARect;
  Root := (Item.Parent = nil) or (Item.Parent.Parent = nil);
  if Item.GetParentMenu is TPopupMenu then Root := False;
  if Item.Default then
  begin
    Font := TFont.Create;
    Font.Assign(ACanvas.Font);
    Font.Style := Font.Style + [fsBold];
    F := SelectObject(DC, Font.Handle);
  end
  else
  begin
    Font := nil;
    F := 0;
  end;
  SetTextColor(DC, ColorToRGB(clWindowText));
  if Root then
  begin
    Clip := ARect;
    Slide(Clip);
    SelectClipRect(DC, Clip, RGN_DIFF);
    if ThemePainter.Enabled then
      FillRectColor(DC, ARect, clMenuBar)
    else
      FillRectColor(DC, ARect, clMenu);
    Dec(ARect.Right, ShadowWidth);
    if odSelected in State then
    begin
      Inc(ARect.Top);
      Inc(ARect.Bottom, RootMargin);
      FillRectColor(DC, ARect, FBorderColor);
      InflateRect(ARect, -1, -1);
      FillRectColor(DC, ARect, FMarginColor);
      InflateRect(ARect, 1, 1);
      Dec(ARect.Top);
      Dec(ARect.Bottom, RootMargin);
    end
    else if odHotLight in State then
    begin
      Inc(ARect.Top);
      Inc(ARect.Left);
      DrawStyleRect(DC, ARect, False);
      Dec(ARect.Top);
      Dec(ARect.Left);
    end;
    DrawCaption(DC, Item.Caption, ARect, drCenter, Item.Enabled, odNoAccel in State);
    if FShadow and (odSelected in State) then
    begin
      Slide(ARect, drRight);
      ARect.Right := ARect.Left + ShadowWidth;
      Inc(ARect.Top, ShadowWidth);
      FillRectColor(DC, ARect, clBtnShadow);
    end;
    SelectClipRgn(DC, 0);
  end
  else
  begin
    FillRectColor(DC, ARect, FMarginColor);
    Inc(ARect.Left, FMarginWidth);
    FillRectColor(DC, ARect, clWindow);
    Inc(ARect.Left, RootMargin);
    if Item.Enabled and (odSelected in State) then
    begin
      DrawStyleRect(DC, R);
      {FillRectColor(DC, R, FSelectColor);
      InflateRect(R, -1, -1);                                      FlowCtrls
      FillRectColor(DC, R, FSelectFill);
      InflateRect(R, 1, 1);}
    end;
    if Item.Caption = '-' then
    begin
      ARect.Bottom := (ARect.Top + ARect.Bottom) shr 1;
      DrawThemeSeperator(DC, ARect, FSeparatorColor, clMenu);
    end
    else
    begin
      M := Item.GetParentMenu;
      if Item.Checked then
      begin
        C := R;
        InflateRect(R, -1, -1);
        //C.Right := ItemMargin - 1; // - 3; //HeightOf(R) - 4;
        R.Right := ItemMargin - 1; // - 3; //HeightOf(R) - 4;
        // DrawThemeThinButton(DC, R, [dsHot, dsPressed]);
        FillRectColorAlpha(DC, C, clHighlight, $24);
        Inc(R.Right, 4);
      end;
      if (M.Images <> nil) and (Item.ImageIndex > -1) then
      begin
        if Item.Enabled and (odSelected in State) and (not Item.Checked) then
          FillRectColor(DC, Classes.Rect(R.Left + 5, R.Top + 5, R.Left + 23, R.Top + 23), Blend(clWindow, clHighlight, 20));
        S := ImageListSize(M.Images);
        if (S.cx > 0) and (S.cy > 0) then
          ImageListDraw(M.Images, ACanvas, R.Left + (22 - S.cx) div 2,
            R.Top + (HeightOf(R) - S.cy) div 2, Item.ImageIndex, Item.Enabled);
      end;
      if M.Images <> nil then
        ARect.Left := ARect.Left + M.Images.Width - 20;
      Inc(ARect.Left, 3);
      DrawCaption(DC, Item.Caption, ARect, drLeft, Item.Enabled, odNoAccel in State);
      if Item.ShortCut <> 0 then
      begin
        Dec(ARect.Right, HeightOf(ARect) shr 1);
        DrawCaption(DC, ShortCutToText(Item.ShortCut), ARect, drRight, Item.Enabled);
      end;
    end;
  end;
  if Item.Default then
  begin
    SelectObject(DC, F);
    Font.Free;
  end;
end;

procedure TMenuStylizer.MeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  Item: TMenuItem absolute Sender;
  Font: TFont;
  F: HFONT;
  M: TMenu;
  Root: Boolean;
  DC: HDC;
  S: TSize;
begin
  Font := nil;
  DC := ACanvas.Handle;
  Root := (Item.Parent = nil) or (Item.Parent.Parent = nil);
  if Item.GetParentMenu is TPopupMenu then Root := False;
  if Item.Default then
  begin
    Font := TFont.Create;
    Font.Assign(ACanvas.Font);
    Font.Style := Font.Style + [fsBold];
    F := SelectObject(DC, Font.Handle);
  end
  else
    F := 0;
  if Item.ShortCut <> 0 then
    S := CalcCaptionSize(DC, Item.Caption + '  ' + ShortCutToText(Item.ShortCut))
  else
    S := CalcCaptionSize(DC, Item.Caption + ' ');
  if Root then
    Width := S.cx - 5
  else
    Width := S.cx + FMarginWidth + ItemMargin - 10;
  if Item.Caption = '-' then
    Height := RootMargin
  else
    Height := S.cy * 2;
  M := Item.GetParentMenu;
  if (not Root) and (M.Images <> nil) then
  begin
    Width := Width + M.Images.Width - 16;
    if Item.Caption <> '-' then
      if Height < M.Images.Height + 4 then
      Height := M.Images.Height + 4;
  end;
  if Item.Default then
  begin
    SelectObject(DC, F);
    Font.Free;
  end;
end;

procedure TMenuStylizer.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    Update;
  end;
end;


end.
