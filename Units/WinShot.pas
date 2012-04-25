unit WinShot;

interface

uses
	Windows, Messages, Classes, Controls, Graphics, GraphTools, GraphThemes,
  ImgList, Forms, SysUtils, Menus, StyleMenus;

type
	TWinCapture = class(TWinControl)
  private
		FThemeData: TThemeData;
    FTarget: TObject;
    FMenuForm: TCustomForm;
    FMenu: TMenu;
    procedure SetTarget(Value: TObject);
    procedure WMDestroy(var Msg: TWMDestroy); message WM_DESTROY;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
	protected
		procedure CreateWindowHandle(const Params: TCreateParams); override;
  public
    property Target: TObject read FTarget write SetTarget;
	published
  	property Align;
    property Anchors;
    property Enabled;
		property Visible;
  end;

implementation

{ TWinCapture }

procedure TWinCapture.CreateWindowHandle(const Params: TCreateParams);
var
	I: TThemedElement;
begin
  inherited CreateWindowHandle(Params);
	if ThemePainter.Available then
		for I := Low(I) to High(I) do
	    FThemeData[I] := OpenThemeData(Handle, ThemeDataNames[I]);
end;

procedure TWinCapture.WMDestroy(var Msg: TWMDestroy);
var
	I: TThemedElement;
begin
	if ThemePainter.Available then
		for I := Low(I) to High(I) do
    	CloseThemeData(FThemeData[I]);
	inherited;
end;

procedure TWinCapture.WMEraseBkgnd(var Msg: TWMEraseBkgnd);

	procedure DrawGraphic;
  var
  	G: TGraphic;
  	C: TCanvas;
  begin
  	G := TGraphic(FTarget);
    if G.Empty then Exit;
    C := TCanvas.Create;
    try
    	C.Handle := Msg.DC;
			C.Draw((ClientWidth - G.Width) shr 1, (ClientHeight - G.Height) shr 1, G);
		finally
    	C.Free;
    end;
  end;

	procedure DrawImageList;
  var
  	Img: TCustomImageList;
    C: TCanvas;
    X, Y: Integer;
  	I: Integer;
  begin
		Img := TCustomImageList(FTarget);
    X := (ClientWidth - Img.Width * Img.Count) shr 1;
    Y := (ClientHeight - Img.Height) shr 1;
    if Y < 0 then Y := 0;
    C := TCanvas.Create;
    try
    	C.Handle := Msg.DC;
	    for I := 0 to Img.Count - 1 do
      begin
  	  	Img.Draw(C, X, Y, I);
        Inc(X, Img.Width);
			end;
		finally
    	C.Free;
    end;
  end;

	procedure DrawControl;
  var
		W: TWinControl;
		A, B: TRect;
  	P: TPoint;
  begin
	  W := nil;
		if FTarget is TWinControl then
	  	W := TWinControl(FTarget)
	  else if FTarget is TControl then
	  	W := TControl(FTarget).Parent;
		if W <> nil then
	  	W.HandleNeeded;
		if (W <> nil) and W.HandleAllocated then
    begin
    	A := ClientRect;
      B := W.ClientRect;
	    SetWindowOrgEx(Msg.DC, (WidthOf(B) - WidthOf(A)) shr 1, (HeightOf(B) - HeightOf(A)) shr 1, @P);
      SelectClipRect(Msg.DC, B, RGN_DIFF);
		  SendMessage(W.Handle, WM_ERASEBKGND, Msg.DC, 0);
      SelectClipRgn(Msg.DC, 0);
      SelectClipRect(Msg.DC, B, RGN_DIFF);
	  	SendMessage(W.Handle, WM_PAINT, Msg.DC, 0);
	    SetWindowOrgEx(Msg.DC, P.X, P.Y, nil);
    end;
  end;

begin
	FillRectColor(Msg.DC, ClientRect, clAppWorkSpace);
	if FTarget is TGraphic then
  	DrawGraphic
	else if FTarget is TCustomImageList then
  	DrawImageList
  else if FTarget is TControl then
		DrawControl;
	Msg.Result := 1;
end;

procedure TWinCapture.SetTarget(Value: TObject);

	procedure CreateMenu;
  var
  	M: TMenu;
    N: TMenuItem;
		P: TPopupMenu;
    S: TStream;
		F: TCustomForm;
    W: HWND;
    I: Integer;
  begin
    W := Windows.GetFocus;
  	M := TMenu(Value);
		S := TMemoryStream.Create;
    try
    	if GetClass(M.ClassName) = nil then
				RegisterClass(TPersistentClass(M.ClassType));
    	S.WriteComponent(M);
      S.Seek(0, 0);
      M := TMenu(S.ReadComponent(nil));
    finally
			S.Free;
    end;
    F := TCustomForm.CreateNew(Self);
		F.Color := clAppWorkSpace;
		F.BorderStyle := bsNone;
		F.Width := 5000;
		F.Height := 30;
    if (M.Owner <> F) and (M.Owner <> nil) then
    	M.Owner.RemoveComponent(M);
		F.InsertComponent(M);
    if M is TPopupMenu then
    begin
    	P := TPopupMenu(M);
	    M := TMainMenu.Create(F);
  	  M.Items.Add(TMenuItem.Create(M));
    	M.Items[0].Caption := P.Name;
			for I := 0 to P.Items.Count - 1 do
			begin
  			N := P.Items[0];
				P.Items.Remove(N);
				M.Items[0].Add(N);
        P.RemoveComponent(N);
        F.InsertComponent(N);
		  end;
      P.Free;
    end;
    F.Menu := TMainMenu(M);
		Windows.SetParent(F.Handle, Handle);
		SetWindowPos(F.Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOZORDER or
    	SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE);
    FMenuForm := F;
    FMenu := M;
    FMenu.Images := TMainMenu(Value).Images;
    Application.ProcessMessages;
    Windows.SetFocus(W);
    TMenuStylizer.Create(F);
  end;

begin
	if Value is TMenuItem then
  	Value := TMenuItem(Value).GetParentMenu;
	if Value <> FTarget then
  begin
		if (Value <> Self) and (Value <> FMenuForm) and (Value <> FMenu) then
    begin
    	FreeAndNil(FMenu);
    	FreeAndNil(FMenuForm);
    	FTarget := Value;
      if (FTarget is TMainMenu) or (FTarget is TPopupMenu) then
      	CreateMenu;
		end
		else
    begin
    	FreeAndNil(FMenu);
    	FreeAndNil(FMenuForm);
    	FTarget := nil;
		end;
		InvalidateRect(Handle, nil, True);
  end;
end;

end.
