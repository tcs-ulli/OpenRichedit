unit RichEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DOM, ScrollingControl,Graphics;

type
  TRichEdit = class(TScrollingControl)
  private
    FBGColor: TColor;
    FDOM: TDOMDocument;
  protected
    procedure Paint; override;
    procedure DoRender(aCanvas : TCanvas);virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DOM : TDOMDocument read FDOM;
  published
    property DefaultColor : TColor read FBGColor write FBGColor default clWindow;
  end;

implementation

procedure TRichEdit.Paint;
begin
  Canvas.Brush.Color:=FBGColor;
  Canvas.FillRect(ClientRect);
  DoRender(Canvas);
end;

procedure TRichEdit.DoRender(aCanvas: TCanvas);
begin

end;

constructor TRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDOM := TDOMDocument.Create;
  FBGColor:=clWindow;
end;

destructor TRichEdit.Destroy;
begin
  FDOM.Free;
  inherited Destroy;
end;

end.

