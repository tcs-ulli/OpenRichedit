unit RichEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DOM, ScrollingControl,Graphics,DOMLayout;

type
  TRichEdit = class(TScrollingControl)
  private
    FBGColor: TColor;
    FDoc: TLayoutedDocument;
  protected
    procedure Paint; override;
    procedure DoRender(aCanvas : TCanvas);virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Document : TLayoutedDocument read FDoc;
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
  FDoc := TLayoutedDocument.Create;
  FBGColor:=clWindow;
end;

destructor TRichEdit.Destroy;
begin
  FDoc.Free;
  inherited Destroy;
end;

end.

