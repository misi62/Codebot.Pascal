
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit CodeReg;

interface

{$I CODEBOT.INC}

uses
  Classes, AutoComplete, Balloon, ImgListEx, BlendTools, FormTools, BannerCtrls,
  SuplCtrls, PaneCtrls, CtrlBox, FlowCtrls, BtnEdit, ScrollCtrls, InspectCtrls,
  InspectEditors, BtnCtrls, FolderCtrls, ShlCtrls, WebCtrls, AlphaSplash,
  DropMgr, MacroTools, CameraCtrls, PhotoCtrls, ProgressCtrls, GridCtrls,
  TabCtrls, LayerCtrls, ListCtrls, CaptionBoxCtrls, FoldingCtrls, SlideCtrls,
  FlowBox, ADOUpdate, ShellDocView, UpQuery, ColorCtrls, SuplDBCtrls,
  { design time units }
  {$IFDEF D6_UP}DesignIntf, DesignWindows, DesignEditors,
  {$IFDEF D8_UP}DesignMenus, {$ENDIF}{$ELSE}DsgnIntf, {$ENDIF}CodeProp;

procedure Register;

implementation

{$R CODEREG.DCR}

procedure Register;
begin
  RegisterComponents('Codebot Components', [TAutoCompletion, TBalloonHint,
    TGlassImageList, TAlphaSplashScreen, TDropFiles, TMacroRecorder]);
  RegisterComponents('Codebot Containers', [TFramedWindow, TBanner, TBannerBook,
    TCaptionBox, THeaderSectionBox, TPaneControl,  TControlBox, TControlBoxButton,
      TFlowSite, TFlowbar, TDrawTabs, TNullPanel, TPaintPanel, TFlowBox]);
  RegisterComponents('Codebot Data', [
    TDBDrawList, TDBReportView]);
  RegisterComponents('Codebot Controls', [
    TBackground, TButtonEdit, TWebBrowserEx, TDocBrowser, TDatePopupEdit, TIntegerEdit, TCashEdit, TListEdit,
    TCheckListEdit, TImageListEdit, TDrawList,
    TInspector, TFolderView, TImageSpeedButton, TImageButton, TCheckListButton,
    TColorGridButton, TBrushButton, TPenButton, TThemeGlyphButton, TSlideBar, TSlideEdit,
    TShadowLabel, THorizontalBar, TPhotoList, TIndeterminateProgress,
    TLayerGrid, TCheckDrawList, TFoldingView, TInfoBox, TContentGrid,
    THuePicker, TSaturationPicker, TAlphaPicker, TPaintPanel]);
  RegisterComponents('Codebot Shell', [
    TShellBubbles, TShellEdit, TShellTree, TShellView, TLargeShellImages, TSmallShellImages]);
  RegisterComponents('ADO', [TADOUpdateQuery, TADOUpdateSQL, TUpdateQuery]);
  RegisterNoIcon([TPaneSheet]);
  RegisterPropertyEditor(TStrings.ClassInfo, nil, '', TDefaultStringsProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageDisabledIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TBannerItem, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TFlowButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TImageSpeedButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImageHotIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImagePressedIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImageDisabledIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TInspectorEditors.ClassInfo, TInspector, 'Editors', TInspectorEditorsProperty);
  RegisterPropertyEditor(TStrings.ClassInfo, TBannerBook, 'Pages', TBannerBookProperty);
  RegisterPropertyEditor(TFolderBars.ClassInfo, TFolderView, 'Folders', TFolderBarsProperty);
  RegisterPropertyEditor(TGlassImages.ClassInfo, nil, '', TTransparentImageProperty);
  RegisterComponentEditor(TFolderView, TFolderBarsEditor);
  RegisterComponentEditor(TBalloonHint, TBalloonHintEditor);
  RegisterComponentEditor(TLargeShellImages, TReadOnlyImageListEditor);
  RegisterComponentEditor(TSmallShellImages, TReadOnlyImageListEditor);
  RegisterComponentEditor(TInspector, TInspectorEditorsEditor);
  RegisterComponentEditor(TPaneControl, TPaneSheetEditor);
  RegisterComponentEditor(TPaneSheet, TPaneSheetEditor);
  RegisterComponentEditor(TGlassImageList, TTransparentImageEditor);
  RegisterComponentEditor(TFlowBar, TFlowBarEditor);
  RegisterComponentEditor(TFlowBox, TFlowBoxEditor);
  RegisterComponentEditor(TDrawTabs, TDrawTabsEditor);
end;

end.
