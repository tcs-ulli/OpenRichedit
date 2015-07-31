unit RichEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, DOM;

type
  TRichEdit = class(TCustomControl)
  private
    FDOM: TDOMDocument;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DOM : TDOMDocument read FDOM;
  end;

implementation

constructor TRichEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDom := TDOMDocument.Create;
end;

destructor TRichEdit.Destroy;
begin
  FDOM.Free;
  inherited Destroy;
end;

end.

