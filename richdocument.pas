unit richdocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,DOM;

  { TCustomRichDocument }

type
  TCustomRichDocument = class(TDOMDocument)
  private
    FFilename: string;
    procedure SetFilename(AValue: string);
  public
    procedure Open;virtual;abstract;
    function AsString : string;virtual;
    property FileName : string read FFilename write SetFilename;
  end;

implementation

{ TCustomRichDocument }

procedure TCustomRichDocument.SetFilename(AValue: string);
begin
  if FFilename=AValue then Exit;
  FFilename:=AValue;
end;

function TCustomRichDocument.AsString: string;
var
  i: Integer;
begin
  Result := '';
end;

end.

