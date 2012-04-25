unit DesignConfig;

interface

uses
  SysUtils, Classes, StrTools, XMLPersist;

const
  CDesignerBackgroundImages = 1;
  CDesignerBandImages = 2;
  CDesignerLayouts = 3;
  CDesignerStyles = 4;
  CDesignerControls = 5;

function Configuration: TPersistData;

implementation

var
  InternalConfiguration: TObject;

function Configuration: TPersistData;
begin
  if InternalConfiguration = nil then
    InternalConfiguration := TPersistData.Create('settings.xml',
      FileReadString('defaults.xml'));
  Result := TPersistData(InternalConfiguration);
end;

initialization
  InternalConfiguration := nil;
finalization
  InternalConfiguration.Free;
end.
