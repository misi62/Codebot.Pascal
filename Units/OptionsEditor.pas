unit OptionsEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, BannerCtrls, ImgList, ImageListEx, Grip;

type
  TOptionsForm = class(TGripForm)
    OptionsImages: TGlassImageList;
    Banner1: TBanner;
    Button1: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
