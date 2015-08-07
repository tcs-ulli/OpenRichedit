unit DOMLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type
  TLayoutedDocument = class
  public
    procedure ReadFromDOM(aDoc : TDOMDocument);
  end;

  TLayoutNode = class
  public
    constructor Create(aDomNode : TDOMNode);
    procedure Calculate;virtual;abstract;
    property DOMNode : TDOMNode read FNode;
  end;

  TInvisibleNode = class(TLayoutNode)
  public
    procedure Calculate;override;
  end;

implementation

procedure TInvisibleNode.Calculate;
begin
end;

procedure TLayoutedDocument.ReadFromDOM(aDoc: TDOMDocument);
  procedure ProcessNode(Node: TDOMNode);
  var
    cNode: TDOMNode;
  begin
    if Node = nil then Exit; // Stops if reached a leaf



    // Goes to the child node
    cNode := Node.FirstChild;
    // Processes all child nodes
    while cNode <> nil do
    begin
      ProcessNode(cNode);
      cNode := cNode.NextSibling;
    end;
  end;

var
  iNode: TDOMNode;
begin
  iNode := aDoc.DocumentElement.FirstChild;
  while iNode <> nil do
    begin
      ProcessNode(iNode);
      iNode := iNode.NextSibling;
    end;
end;

end.

