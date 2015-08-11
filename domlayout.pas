unit DOMLayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, DOM_HTML, FPCanvas,types;

type
  TLayoutNode = class;
  TLayoutNodeClass = class of TLayoutNode;
  TLayoutedDocument = class
  private
    FLayout: TLayoutNode;
    FWidth: Integer;
    procedure SetWidth(AValue: Integer);
  protected
    LastAddedNode : TLayoutNode;
  public
    constructor Create;
    procedure ReadFromDOM(aDoc : TDOMDocument);
    property Layout : TLayoutNode read FLayout;
    procedure Clear;
    procedure RenderToCanvas(aCanvas: TFPCustomCanvas; ViewPort: TRect);
    property Width : Integer read FWidth write SetWidth;
  end;
  TLayoutNode = class(TList)
  private
    FBoundsRect: TRect;
    FChanged: Boolean;
    FNode: TDOMNode;
    FParent: TLayoutedDocument;
    FStopAt: TPoint;
    FPriorNode : TLayoutNode;
    FCanvas : TFPCustomCanvas;
    function GetBoundsRect: TRect;
    function GetChilds(Index : Integer): TLayoutNode;
    function GetStartAt: TPoint;
    function GetStopAt: TPoint;
  public
    constructor Create(aDoc : TLayoutedDocument; aDomNode : TDOMNode);virtual;
    procedure Calculate(aCanvas: TFPCustomCanvas);virtual;
    function FindNodeType(aNode : TDOMNode) : TLayoutNodeClass;
    property DOMNode : TDOMNode read FNode;
    property Parent : TLayoutedDocument read FParent;
    property Childs[Index : Integer] : TLayoutNode read GetChilds;
    function IsInView(ViewPort : TRect) : Boolean;
    procedure RenderToCanvas(aCanvas: TFPCustomCanvas;ViewPort : TRect);
    procedure Change;
    property Changed : Boolean read FChanged;
    property BoundsRect : TRect read GetBoundsRect;
    property StartAt : TPoint read GetStartAt;
    property StopAt : TPoint read GetStopAt;
  end;

  TLayoutInvisibleNode = class(TLayoutNode)
  public
    procedure Calculate(aCanvas: TFPCustomCanvas);override;
  end;

  TLayoutFont = class(TLayoutInvisibleNode)
  public
    procedure Calculate(aCanvas: TFPCustomCanvas);override;
  end;

  TLayoutText = class(TLayoutNode)
  private
    FLines : TStringList;
  public
    constructor Create(aDoc: TLayoutedDocument; aDomNode: TDOMNode); override;
    destructor Destroy; override;
    procedure Calculate(aCanvas: TFPCustomCanvas);override;
  end;

implementation

constructor TLayoutText.Create(aDoc: TLayoutedDocument; aDomNode: TDOMNode);
begin
  inherited Create(aDoc, aDomNode);
  FLines := TStringList.Create;
end;

destructor TLayoutText.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TLayoutText.Calculate(aCanvas: TFPCustomCanvas);
var
  tmp: string;
  aPos: TPoint;
  aLineI : Integer = 0;
  aLine : string = '';
  PartWidth: Integer;
begin
  tmp := DOMNode.TextContent;
  aPos := StartAt;
  FLines.Clear;
  while pos(' ',tmp)>0 do
    begin
      PartWidth := aCanvas.TextWidth(copy(tmp,0,pos(' ',tmp)-1));
      if PartWidth+aPos.x<FParent.Width then
        begin
          aLine += copy(tmp,0,pos(' ',tmp));
          tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
          aPos.x+=PartWidth;
        end
      else
        begin
          FLines.Add(aLine);
          inc(aLineI);
          aPos.y+=aCanvas.TextHeight(aLine);
          aLine := '';
        end;
    end;
  FLines.Add(aLine+tmp);
end;

procedure TLayoutInvisibleNode.Calculate(aCanvas: TFPCustomCanvas);
begin
  inherited Calculate(aCanvas);
  FChanged:=False;
end;

procedure TLayoutFont.Calculate(aCanvas: TFPCustomCanvas);
begin
  inherited Calculate(aCanvas);
  //Apply font Settings
end;

function TLayoutNode.GetChilds(Index : Integer): TLayoutNode;
begin
  Result := TLayoutNode(Items[Index]);
end;

function TLayoutNode.GetBoundsRect: TRect;
begin
  if FChanged then
    Calculate(FCanvas);
  Result := FBoundsRect;
end;

function TLayoutNode.GetStartAt: TPoint;
begin
  if not Assigned(FPriorNode) then
    Result := Point(0,0)
  else Result := FPriorNode.StartAt;
end;

function TLayoutNode.GetStopAt: TPoint;
begin
  if FChanged then
    Calculate(FCanvas);
  Result := FStopAt;
end;

constructor TLayoutNode.Create(aDoc: TLayoutedDocument; aDomNode: TDOMNode);
var
  cNode: TDOMNode;
begin
  inherited Create;
  FPriorNode := aDoc.LastAddedNode;
  aDoc.LastAddedNode:=Self;
  FNode := aDomNode;
  FParent := aDoc;
  FChanged:=True;
  cNode := aDomNode.FirstChild;
  while Assigned(cNode) do
    begin
      Add(FindNodeType(cNode).Create(FParent,cNode));
      cNode := cNode.NextSibling;
    end;
end;

procedure TLayoutNode.Calculate(aCanvas: TFPCustomCanvas);
var
  i: Integer;
begin
  FCanvas := aCanvas;
  for i := 0 to Count-1 do
    TLayoutNode(Items[i]).Calculate(aCanvas);
end;

function TLayoutNode.FindNodeType(aNode: TDOMNode): TLayoutNodeClass;
begin
  Result := TLayoutInvisibleNode;
  if (aNode.NodeName='font') then
    Result := TLayoutFont
  else if ((aNode.NodeName='p') or (aNode is TDOMText)) then
    Result := TLayoutText
  else
    begin
      writeln(aNode.ClassName+' ('+aNode.NodeName+'='+aNode.NodeValue+')');
    end;
end;

function TLayoutNode.IsInView(ViewPort: TRect): Boolean;
var
  aRect: TRect;
begin
  Result := IntersectRect(aRect,ViewPort,BoundsRect);
end;

procedure TLayoutNode.RenderToCanvas(aCanvas: TFPCustomCanvas; ViewPort: TRect);
var
  i: Integer;
begin
  FCanvas := aCanvas;
  if not IsInView(ViewPort) then exit;
  for i := 0 to Count-1 do
    if TLayoutNode(Items[i]).IsInView(ViewPort) then
      TLayoutNode(Items[i]).RenderToCanvas(aCanvas,ViewPort);
end;

procedure TLayoutNode.Change;
begin
  FChanged:=True;
end;

procedure TLayoutedDocument.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
end;

constructor TLayoutedDocument.Create;
begin
end;

procedure TLayoutedDocument.ReadFromDOM(aDoc: TDOMDocument);
var
  iNode: TDOMNode;
begin
  FLayout.Free;
  LastAddedNode:=nil;
  FLayout := TLayoutNode.Create(Self,aDoc.DocumentElement);
end;

procedure TLayoutedDocument.Clear;
begin
  FreeAndNil(FLayout);
end;

procedure TLayoutedDocument.RenderToCanvas(aCanvas: TFPCustomCanvas;ViewPort : TRect);
var
  i: Integer;
begin
  for i := 0 to FLayout.Count-1 do
    TLayoutNode(FLayout[i]).RenderToCanvas(aCanvas,ViewPort);
end;

end.
