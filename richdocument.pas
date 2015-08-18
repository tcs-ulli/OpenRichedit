unit richdocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,DOM;

  { TCustomRichDocument }

type
  TCustomRichDocument = class(TDOMDocument)
  private
    FBodyNode: TDOMElement;
    FFilename: string;
    procedure SetFilename(AValue: string);
  public
    procedure Open;virtual;abstract;
    function AsString : string;virtual;
    property FileName : string read FFilename write SetFilename;
    property BodyNode : TDOMElement read FBodyNode write FBodyNode;
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
  ChildNode: TDOMNode;
  aNodeName: DOMString;
  cellText: String;
  hyperlink: String;
  subnode: TDOMNode;
  procedure AddToCellText(AText: String);
  begin
    if cellText = ''
       then cellText := AText
       else cellText := cellText + AText;
  end;
  function GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
  var
    i: LongWord;
    Found: Boolean;
  begin
    Result := '';
    if ANode = nil then
      exit;

    Found := false;
    i := 0;
    while not Found and (i < ANode.Attributes.Length) do begin
      if ANode.Attributes.Item[i].NodeName = AAttrName then begin
        Found := true;
        Result := ANode.Attributes.Item[i].NodeValue;
      end;
      inc(i);
    end;
  end;
begin
  Result := '';
  cellText := '';
  hyperlink := '';
  ChildNode := BodyNode.FirstChild;
  while Assigned(childnode) do
  begin
    aNodeName := childNode.NodeName;
    if (aNodeName = 'text:p')
    or (aNodeName = 'p')
    then begin
      // Each 'text:p' node is a paragraph --> we insert a line break after the first paragraph
      if cellText <> '' then
        cellText := cellText + LineEnding;
      subnode := childnode.FirstChild;
      while Assigned(subnode) do
      begin
        aNodename := subnode.NodeName;
        case aNodename of
          '#text' :
            AddToCellText(subnode.TextContent);
          'text:a':     // "hyperlink anchor"
            begin
              hyperlink := GetAttrValue(subnode, 'xlink:href');
              AddToCellText(subnode.TextContent);
            end;
          'text:span':
            AddToCellText(subnode.TextContent);
        end;
        subnode := subnode.NextSibling;
      end;
    end;
    childnode := childnode.NextSibling;
  end;
end;

end.

