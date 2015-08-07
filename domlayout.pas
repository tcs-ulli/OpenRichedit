unit DOMLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, DOM_HTML, FPCanvas;

type
  TLayoutNode = class;
  TLayoutNodeClass = class of TLayoutNode;
  TLayoutedDocument = class
  private
    FLayout: TLayoutNode;
    FCanvas : TFPCustomCanvas;
  public
    procedure ReadFromDOM(aDoc : TDOMDocument);
    property Layout : TLayoutNode read FLayout;
    procedure Clear;
  end;

  TLayoutNode = class(TList)
  private
    FNode: TDOMNode;
    FParent: TLayoutedDocument;
    function GetChilds(Index : Integer): TLayoutNode;
  public
    constructor Create(aDoc : TLayoutedDocument; aDomNode : TDOMNode);
    procedure Calculate;virtual;abstract;
    function FindNodeType(aNode : TDOMNode) : TLayoutNodeClass;
    property DOMNode : TDOMNode read FNode;
    property Parent : TLayoutedDocument read FParent;
    property Childs[Index : Integer] : TLayoutNode read GetChilds;
  end;

  TLayoutInvisibleNode = class(TLayoutNode)
  public
    procedure Calculate;override;
  end;

  TLayoutFont = class(TLayoutInvisibleNode)
  public
    procedure Calculate;override;
  end;

  TLayoutText = class(TLayoutNode)
  public
    procedure Calculate;override;
  end;

implementation

procedure TLayoutText.Calculate;
begin

  DOMNode.TextContent;
end;

procedure TLayoutInvisibleNode.Calculate;
begin
end;

procedure TLayoutFont.Calculate;
begin
  inherited Calculate;
  //Apply font Settings
end;

function TLayoutNode.GetChilds(Index : Integer): TLayoutNode;
begin
  Result := TLayoutNode(Items[Index]);
end;

constructor TLayoutNode.Create(aDoc: TLayoutedDocument; aDomNode: TDOMNode);
var
  cNode: TDOMNode;
begin
  inherited Create;
  FNode := aDomNode;
  FParent := aDoc;
  cNode := aDomNode.FirstChild;
  while Assigned(cNode) do
    begin
      Add(FindNodeType(cNode).Create(FParent,cNode));
      cNode := cNode.NextSibling;
    end;
end;

function TLayoutNode.FindNodeType(aNode: TDOMNode): TLayoutNodeClass;
begin
  Result := TLayoutInvisibleNode;
  if (aNode is THTMLFontElement) or (aNode is THTMLBaseFontElement) then
    Result := TLayoutFont
  else if (aNode is TDOMText) then
    Result := TLayoutText
  else
    begin
      writeln(aNode.ClassName)
    end;
end;

procedure TLayoutedDocument.ReadFromDOM(aDoc: TDOMDocument);
var
  iNode: TDOMNode;
begin
  FLayout.Free;
  FLayout := TLayoutNode.Create(Self,aDoc.DocumentElement);
end;

procedure TLayoutedDocument.Clear;
begin
  FreeAndNil(FLayout);
end;

end.

