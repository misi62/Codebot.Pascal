
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit AlphaSplash;

interface

{$I CODEBOT.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GraphTools, BlendTools, BaseTypes, ExtCtrls;

type
	TAlphaSplashScreen = class(TComponent)
  private
    FAutoDestroy: Boolean;
    FForm: TForm;
    FVisible: Boolean;
    FFade: Double;
    FDuration: Double;
    FImage: TAlphaMap;
    FTimer: TTimer;
    FVisibleTime: TDateTime;
    FTesting: Boolean;
    FReleasing: Boolean;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure TimerExpired(Sender: TObject);
    function GetClientRect: TRect;
    procedure SetDuration(Value: Double);
    procedure SetFade(Value: Double);
    procedure SetImage(Value: TAlphaMap);
    procedure SetVisible(Value: Boolean);
    procedure SetTest(Value: Boolean);
    function GetTest: Boolean;
	protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadImage(Stream: TStream);
    procedure WriteImage(Stream: TStream);
    procedure Loaded; override;
    procedure DoShow; dynamic;
    procedure DoHide; dynamic;
	public
  	constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawStrings(S: TStrings; const Rect: TRect);
    procedure DrawText(const S: string; const Rect: TRect; Direction: TDirection);
    procedure Update;
  	property ClientRect: TRect read GetClientRect;
	published
  	property AutoDestroy: Boolean read FAutoDestroy write FAutoDestroy;
  	property Image: TAlphaMap read FImage write SetImage;
    property Visible: Boolean read FVisible write SetVisible;
    property Fade: Double read FFade write SetFade;
    property Duration: Double read FDuration write SetDuration;
    property Test: Boolean read GetTest write SetTest;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

implementation

type
	TSplashForm = class(TForm)
  private
    procedure CMShowingChanged(var Msg: TMessage); message CM_SHOWINGCHANGED;
		procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
	protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
	public
  	destructor Destroy; override;
  end;

{ TSplashForm }

destructor TSplashForm.Destroy;
var
	S: TAlphaSplashScreen;
  R: Boolean;
begin
	S := TAlphaSplashScreen(Owner);
  R := (S <> nil) and (S.FReleasing);
  if R then
  	S.FTimer.Enabled := False;
  inherited Destroy;
  if R then
  	S.Free;
end;

procedure TSplashForm.CMShowingChanged(var Msg: TMessage);
begin
	if csDestroying in ComponentState then Exit;
	Left := (Screen.Width - Width) shr 1;
  Top := (Screen.Height - Height) shr 1;
  if Visible then
  begin
		ShowWindow(Handle, SW_SHOWNOACTIVATE);
		TAlphaSplashScreen(Owner).DoShow;
	end
	else
  begin
		ShowWindow(Handle, SW_HIDE);
		TAlphaSplashScreen(Owner).DoHide;
	end;
	Msg.Result := 1;
end;

procedure TSplashForm.WMNCHitTest(var Msg: TWMNCHitTest);
begin
	Msg.Result := HTCLIENT;
end;

procedure TSplashForm.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
	TAlphaSplashScreen(Owner).DoHide;
end;

{ TAlphaSplashScreen }

const
	OneSecond = (1 / 60 / 60 / 24);

constructor TAlphaSplashScreen.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 30;
  FTimer.OnTimer := TimerExpired;
  FImage := TAlphaMap.Create;
  FImage.Canvas.Font := Screen.IconFont;
  FImage.Canvas.Font.Size := 8;
  FForm := TSplashForm.CreateNew(Self);
  FForm.BorderStyle := bsNone;
  FForm.FormStyle := fsStayOnTop;
end;

destructor TAlphaSplashScreen.Destroy;
begin
	FReleasing := False;
	FAutoDestroy := False;
	FTimer.Enabled := False;
	FImage.Free;
	inherited Destroy;
end;

procedure TAlphaSplashScreen.TimerExpired(Sender: TObject);
begin
	if FVisible then
		if (FDuration > 0) and ((Now - FVisibleTime) / OneSecond > FDuration) then
    begin
  		Visible := False;
      if FFade = 0 then
      	FTimer.Enabled := False
      else
      	FTimer.Interval := 30;
		end
    else if (FFade > 0) and ((Now - FVisibleTime) / OneSecond > FFade) then
    begin
			FTimer.Interval := 500;
      if FImage.Opacity < $FF then
      begin
      	FImage.Opacity := $FF;
        Update;
      end;
      if FDuration = 0 then
				FTimer.Enabled := False;
    end
		else if FFade > 0 then
    begin
    	FImage.Opacity := Round($FF * ((Now - FVisibleTime) / OneSecond / FFade));
      Update;
    end
    else
	else if (FFade > 0) and ((Now - FVisibleTime) / OneSecond > FFade) then
  begin
		FTimer.Enabled := False;
    FForm.Visible := False;
  end
  else if FFade > 0 then
  begin
		FImage.Opacity := $FF - Round($FF * ((Now - FVisibleTime) / OneSecond / FFade));
		Update;
  end;
end;

procedure TAlphaSplashScreen.ReadImage(Stream: TStream);
begin
	FImage.LoadFromStream(Stream);
end;

procedure TAlphaSplashScreen.WriteImage(Stream: TStream);
begin
	FImage.SaveToStream(Stream);
end;

procedure TAlphaSplashScreen.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Image', ReadImage, WriteImage, (FImage.Height > 0)
  	and (FImage.Width > 0));
end;

procedure TAlphaSplashScreen.Loaded;
begin
	inherited Loaded;
	if FVisible then
  begin
  	FVisible := False;
    Visible := True;
  end;
end;

procedure TAlphaSplashScreen.DrawStrings(S: TStrings; const Rect: TRect);
var
	DC: HDC;
  C: COLORREF;
  B: HBRUSH;
	R: TRect;
	I: Integer;
begin
	DC := FImage.Canvas.Handle;
  C := GetPixel(DC, Rect.Left, Rect.Top);
  B := CreateSolidBrush(C);
 	FillRect(DC, Rect, B);
  R := Rect;
	R.Bottom := R.Top + FontHeight(DC);
	for I := 0 to S.Count - 1 do
  begin
    DrawText(S[I], R, drLeft);
    Slide(R);
  end;
  FImage.Opaque(Rect);
end;

procedure TAlphaSplashScreen.DrawText(const S: string; const Rect: TRect;
  Direction: TDirection);
var
	DC: HDC;
  C: COLORREF;
  B: HBRUSH;
  R: TRect;
begin
	DC := FImage.Canvas.Handle;
  C := GetPixel(DC, Rect.Left, Rect.Top);
  B := CreateSolidBrush(C);
 	FillRect(DC, Rect, B);
  DeleteObject(B);
  SetBKMode(DC, TRANSPARENT);
  R := Rect;
	Windows.DrawText(DC, PChar(S), -1, R, Directions[Direction]);
  FImage.Opaque(R);
end;

procedure TAlphaSplashScreen.Update;
begin
	UpdateAlphaWindow(FForm.Handle, FImage, FImage.Canvas, FImage.Opacity);
end;

procedure TAlphaSplashScreen.DoShow;
begin
	if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TAlphaSplashScreen.DoHide;
begin
	if Assigned(FOnHide) then FOnHide(Self);
  FVisible := False;
  FForm.Visible := False;
end;

function TAlphaSplashScreen.GetClientRect: TRect;
begin
  Result := FForm.ClientRect;
end;

procedure TAlphaSplashScreen.SetDuration(Value: Double);
begin
	if Value < 0 then	Value := 0;
  if Value <> FDuration then
  begin
  	FDuration := Value;
  end;
end;

procedure TAlphaSplashScreen.SetFade(Value: Double);
begin
	if Value < 0 then	Value := 0;
  if Value <> FFade then
  begin
  	FFade := Value;
  end;
end;

procedure TAlphaSplashScreen.SetImage(Value: TAlphaMap);
begin
  FImage.Assign(Value);
  if FVisible then
		UpdateAlphaWindow(FForm.Handle, FImage, FImage.Canvas);
end;

procedure TAlphaSplashScreen.SetVisible(Value: Boolean);
begin
  if Value <> FVisible then
  begin
  	FVisible := Value;
    if Value and (not FTesting) and (csDesigning in ComponentState) then Exit;
    FTesting := False;
		if (FImage = nil) or (csLoading in ComponentState) then Exit;
    FVisibleTime := Now;
    if (FFade > 0) and (not Value) then
    begin
			FTimer.Interval := 30;
			FTimer.Enabled := True;
    end
    else
    if (FFade = 0) or Value then
    begin
    	if Value and (FFade > 0) then
      begin
      	FImage.Opacity := 0;
      	FTimer.Interval := 30;
      	FTimer.Enabled := True;
			end
			else
      begin
      	FImage.Opacity := $FF;
        if Value and (FDuration > 0) then
        begin
	      	FTimer.Interval := 500;
	      	FTimer.Enabled := True;
        end;
			end;
      if Value then
		    Update;
      if FForm = nil then
      begin
        FForm := TSplashForm.CreateNew(Self);
        FForm.BorderStyle := bsNone;
        FForm.FormStyle := fsStayOnTop;
      end;
	    FForm.Visible := Value;
		end;
  end;
end;

function TAlphaSplashScreen.GetTest: Boolean;
begin
  Result := FForm.Visible;
end;

procedure TAlphaSplashScreen.SetTest(Value: Boolean);
begin
	FTesting := True;
  Visible := Value;
end;

end.
