
(********************************************************)
(*                                                      *)
(*  Private Distribution of Codebot Class Library       *)
(*  Version: 2.00.00                                    *)
(*  Date: March 2009                                    *)
(*                                                      *)
(********************************************************)

unit CodebotReg;

interface

{$I CODEBOT.INC}

uses
	Classes, AutoComplete, Balloon, ImgListEx, BlendTools, FormTools, BannerCtrls,
  SuplCtrls, PaneCtrls, CtrlBox, FlowCtrls, BtnEdit, ScrollCtrls, InspectCtrls,
  InspectEditors, BtnCtrls, FolderCtrls, ShlCtrls, WebCtrls, AlphaSplash,
  DropMgr, CameraCtrls, PhotoCtrls, ProgressCtrls, GridCtrls, // MacroTools, 
  TabCtrls, LayerCtrls, ListCtrls, CaptionBoxCtrls, FoldingCtrls, SlideCtrls,
  FlowBox, ADOUpdate, ShellDocView, UpQuery, ColorCtrls, SuplDBCtrls, StepCtrls,
  LabelCtrls, StrCollect,
  { Design time units }
  {$IFDEF D6_UP}DesignIntf, DesignWindows, DesignEditors,
  {$IFDEF D8_UP}DesignMenus, {$ENDIF}{$ELSE}DsgnIntf, {$ENDIF}CodeProp;

procedure Register;

implementation
procedure Register;
begin
  RegisterComponents('Codebot Components', [TAutoCompletion, TBalloonHint,
  	TGlassImageList, TAlphaSplashScreen, TDropFiles, TNamedStringCollector]); //TMacroRecorder, 
  RegisterComponents('Codebot Containers', [TFramedWindow, TBanner, TBannerBook,
  	TCaptionBox, THeaderSectionBox, TPaneControl,	TControlBox, TControlBoxButton,
    	TFlowSite, TFlowbar, TDrawTabs, TNullPanel, TPaintPanel, TFlowBox]);
  RegisterComponents('Codebot Data', [
  	TDBDrawList, TDBReportView, TADOUpdateQuery, TADOUpdateSQL, TUpdateQuery]);
  RegisterComponents('Codebot Controls', [
  	TBackground, TButtonEdit, TWebBrowserEx, TDocBrowser, TDatePopupEdit, TIntegerEdit, TCashEdit, TListEdit,
    TCheckListEdit, TImageListEdit, TDrawList,
    TInspector, TFolderView, TImageSpeedButton, TImageButton, TCheckListButton,
    TColorGridButton, TBrushButton, TPenButton, TThemeGlyphButton, TSlideBar, TSlideEdit,
    TShadowLabel, THorizontalBar, TPhotoList, TIndeterminateProgress, TSpinProgress,
    TLayerGrid, TCheckDrawList, TFoldingView, TInfoBox, TContentGrid,
    THuePicker, TSaturationPicker, TAlphaPicker, TAnglePicker, TColorSlideEdit,
    TPaintPanel, TStepBubbles, TPaintImage, TWrapLabel]);
  RegisterComponents('Codebot Shell', [
  	TShellBubbles, TShellEdit, TShellTree, TShellView, TShellPathEditBar, TShellBinding,
    TLargeShellImages, TSmallShellImages]);
  RegisterNoIcon([TPaneSheet]);
  RegisterPropertyEditor(TStrings.ClassInfo, nil, '', TDefaultStringsProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomImageButton, 'ImageDisabledIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TBannerItem, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TFlowButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TPaintImage, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TImageSpeedButton, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImageIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImageHotIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImagePressedIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TCustomButtonEdit, 'ImageDisabledIndex', TCustomImageIndexPropertyEditor);
  RegisterPropertyEditor(TInspectorEditors.ClassInfo, TInspector, 'Editors', TInspectorEditorsProperty);
  RegisterPropertyEditor(TStrings.ClassInfo, TBannerBook, 'Pages', TBannerBookProperty);
  RegisterPropertyEditor(TFolderBars.ClassInfo, TFolderView, 'Folders', TFolderBarsProperty);
  RegisterPropertyEditor(TGlassImages.ClassInfo, nil, '', TTransparentImageProperty);
  RegisterPropertyEditor(TNamedStrings.ClassInfo, nil, '', TNamedStringsProperty);
  RegisterComponentEditor(TFolderView, TFolderBarsEditor);
  RegisterComponentEditor(TBalloonHint, TBalloonHintEditor);
  RegisterComponentEditor(TLargeShellImages, TReadOnlyImageListEditor);
  RegisterComponentEditor(TSmallShellImages, TReadOnlyImageListEditor);
  RegisterComponentEditor(TInspector, TInspectorEditorsEditor);
  RegisterComponentEditor(TPaneControl, TPaneSheetEditor);
  RegisterComponentEditor(TPaneSheet, TPaneSheetEditor);
  RegisterComponentEditor(TNamedStringCollector, TNamedStringsEditor);
  RegisterComponentEditor(TGlassImageList, TTransparentImageEditor);
  RegisterComponentEditor(TFlowBar, TFlowBarEditor);
  RegisterComponentEditor(TFlowBox, TFlowBoxEditor);
  RegisterComponentEditor(TDrawTabs, TDrawTabsEditor);
  RegisterComponentEditor(TShellBinding, TBindingEditor);
end;

end.
