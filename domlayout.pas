{
Line
1   Element 1 Element 2<br>
2   Element 2 Image 1
3   Element 3 Image 1
4   Element 3 Image 1
5   Element 3 Element 3<br>
}

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
    function FindLayoutNode(aDOMNode : TDOMNode) : TLayoutNode;
    property Width : Integer read FWidth write SetWidth;
  end;
  TLayoutDisplay=(ldBlock,   //End with Line Break
                  ldInline); //End without Line Break (In Line)
  TLayoutNode = class(TList)
  private
    FChanged: Boolean;
    FDisplay: TLayoutDisplay;
    FNode: TDOMNode;
    FParent: TLayoutedDocument;
    FStopAt: TPoint;
    FBottomRight : TPoint;
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
    function GetLayoutNode(aDOMNode : TDOMNode) : TLayoutNode;
    property DOMNode : TDOMNode read FNode;
    property Parent : TLayoutedDocument read FParent;
    property Childs[Index : Integer] : TLayoutNode read GetChilds;
    function IsInView(ViewPort : TRect) : Boolean;
    procedure RenderToCanvas(aCanvas: TFPCustomCanvas;ViewPort : TRect);virtual;
    procedure Change;
    property Changed : Boolean read FChanged;
    property BoundsRect : TRect read GetBoundsRect;
    property StartAt : TPoint read GetStartAt;
    property StopAt : TPoint read GetStopAt;
    property Display : TLayoutDisplay read FDisplay;
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
  protected
  public
    constructor Create(aDoc: TLayoutedDocument; aDomNode: TDOMNode); override;
    destructor Destroy; override;
    procedure Calculate(aCanvas: TFPCustomCanvas);override;
    procedure RenderToCanvas(aCanvas: TFPCustomCanvas; ViewPort: TRect);override;
  end;
  TLineObj = class
  private
    FRect: TRect;
  public
    constructor Create(aStartPos,aEndPos : TPoint);
    property Rect : TRect read FRect;
  end;

implementation

constructor TLineObj.Create(aStartPos, aEndPos: TPoint);
begin
  FRect := types.Rect(aStartPos.x,aStartPos.y,aEndPos.x,aEndPos.y);
end;

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
  aPos: TPoint;
  aLineI : Integer = 0;
  aLine : string = '';
  PartWidth: Integer;
  aText: string;
  actPart: String;
  aLineStart: TPoint;

  function GetTextPart : string;
  begin
    if pos(' ',aText)>0 then
      begin
        Result := copy(aText,0,pos(' ',aText)-1);
        aText := copy(aText,pos(' ',aText)+1,length(aText));
      end
    else
      begin
        Result := aText;
        aText := '';
      end;
  end;

begin
  aText := DOMNode.TextContent;
  aPos := StartAt;
  aLineStart := aPos;
  FLines.Clear;
  FBottomRight:=aPos;
  actPart := GetTextPart;
  while actPart<>'' do
    begin
      PartWidth := aCanvas.TextWidth(actPart);
      if PartWidth+aPos.x<FParent.Width then
        begin
          aLine += actPart;
          aPos.x+=PartWidth;
        end
      else
        begin //Line Break
          FLines.AddObject(aLine,TLineObj.Create(aLineStart,Point(aPos.x,aPos.y+aCanvas.TextHeight(aLine))));
          inc(aLineI);
          if aPos.x>FBottomRight.x then
            FBottomRight.x:=aPos.x;
          aPos.y+=aCanvas.TextHeight(aLine);
          FBottomRight.y:=aPos.y;
          aPos.x := 0;
          aLineStart := aPos;
          aLine := actPart;
        end;
      actPart := GetTextPart;
    end;
  if aLine<>'' then
    begin
      FLines.AddObject(aLine,TLineObj.Create(aLineStart,Point(aPos.x,aPos.y+aCanvas.TextHeight(aLine))));
      if aPos.x>FBottomRight.x then
        FBottomRight.x:=aPos.x;
      inc(aLineI);
    end;
  if Display=ldBlock then  //Add Line Break
    begin
      aPos.y+=aCanvas.TextHeight(aLine);
      aPos.x := 0;
      FBottomRight.y:=aPos.y;
    end;
  FStopAt := aPos;
  inherited Calculate(aCanvas);
end;

procedure TLayoutText.RenderToCanvas(aCanvas: TFPCustomCanvas; ViewPort: TRect);
var
  i: Integer;
  aRect: TRect;
begin
  inherited RenderToCanvas(aCanvas,ViewPort);
  for i := 0 to FLines.Count-1 do
    with TLineObj(FLines.Objects[i]) do
      begin
        aRect := Rect;
        aCanvas.TextOut(aRect.Left,aRect.Top,FLines[i]);
      end;
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
  Result := Rect(StartAt.x,StartAt.y,FBottomRight.x,FBottomRight.y);
end;

function TLayoutNode.GetStartAt: TPoint;
begin
  if not Assigned(FPriorNode) then
    Result := Point(0,0)
  else Result := FPriorNode.StopAt;
end;

function TLayoutNode.GetStopAt: TPoint;
begin
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
  FDisplay:=ldBlock;
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
  writeln('Calc:'+DOMNode.NodeName+','+DOMNode.NodeValue+' Start:'+IntToStr(StartAt.x)+','+IntToStr(StartAt.y));
  FCanvas := aCanvas;
  for i := 0 to Count-1 do
    begin
      TLayoutNode(Items[i]).Calculate(aCanvas);
      if TLayoutNode(Items[i]).StopAt.x>FStopAt.x then
        FStopAt.x := TLayoutNode(Items[i]).StopAt.x;
      if TLayoutNode(Items[i]).StopAt.y>FStopAt.y then
        FStopAt.y := TLayoutNode(Items[i]).StopAt.y;
    end;
  writeln('Calc:'+DOMNode.NodeName+','+DOMNode.NodeValue+' Stop:'+IntToStr(FStopAt.x)+','+IntToStr(FStopAt.y));
  FChanged:=False;
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

function TLayoutNode.GetLayoutNode(aDOMNode: TDOMNode): TLayoutNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    begin
      if TLayoutNode(Items[i]).DOMNode=aDOMNode then
        begin
          Result := TLayoutNode(Items[i]);
          break;
        end
      else Result := TLayoutNode(Items[i]).GetLayoutNode(aDOMNode);
      if Assigned(Result) then exit;
    end;
end;

function TLayoutNode.IsInView(ViewPort: TRect): Boolean;
var
  aRect: TRect;
begin
  Result := IntersectRect(aRect,ViewPort,BoundsRect);
  Result := True;
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
  FLayout := TLayoutNode.Create(Self,aDoc.DocumentElement.FindNode('body'));
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

function TLayoutedDocument.FindLayoutNode(aDOMNode: TDOMNode): TLayoutNode;
var
  i: Integer;
begin
  Result := Layout.GetLayoutNode(aDOMNode);
end;

end.
