{
  DOM Layout Klassen, die ein DOM Layout auf einen Canvas rendern können
}

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
  Classes, SysUtils, FPCanvas,types,DOM;

type
  TLayoutNode = class;
  TLayoutDiv = class;
  TLayoutNodeClass = class of TLayoutNode;
  TLayoutedDocument = class;

  {:@abstract(Basic Reader Interface, with this class you can read custom Document types to TLayoutedDocument.)}
  TDocumentreader = class
  public
    {:Read liest eine "Node" aus dem Dokument und zwar die nachfolgende zu PriorNode. TLayoutedDocument liest nur die Nodes die gerade zum Rendern benötigt werden}
    function Read(Document : TLayoutedDocument;PriorNode : TObject) : TLayoutNode;virtual;abstract;
  end;

  TTextDocumentReader = class(TDocumentreader)
  public
    function Read(Document: TLayoutedDocument;PriorNode : TObject): TLayoutNode;override;
  end;

  { TLayoutedDocument }

  TLayoutedDocument = class
  private
    FCanvas: TFPCustomCanvas;
    FLayout: TLayoutDiv;
    FWidth: Integer;
    procedure SetCanvas(AValue: TFPCustomCanvas);
    procedure SetWidth(AValue: Integer);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromReader(aReader : TDocumentreader);
    procedure LoadFromStream(aStream : TStream;aReader : TDocumentreader);
    {This function can be used later to Provide own Classes to an inherited class Readers use it to provide the Real class based on ther suggestions}
    function GetOwnNodeClass(aClass : TLayoutNodeClass) : TLayoutNodeClass;
    property Layout : TLayoutDiv read FLayout write FLayout;
    procedure Clear;
    procedure RenderToCanvas(aCanvas: TFPCustomCanvas; ViewPort: TRect);
    function FindLayoutNode(aDOMNode : TDOMNode) : TLayoutNode;
    property Canvas : TFPCustomCanvas read FCanvas write SetCanvas;
    property Width : Integer read FWidth write SetWidth;
  end;
  TLayoutDisplay=(ldBlock,   //End with Line Break
                  ldInline); //End without Line Break (In Line)

  {:@abstract(Basic Layout Object.)}
  TLayoutNode = class(TList)
  private
    FChanged: Boolean;
    FDisplay: TLayoutDisplay;
    FDiv: TLayoutDiv;
    FNode: TDOMNode;
    FDocument: TLayoutedDocument;
    FStopAt: TPoint;
    FBottomRight : TPoint;
    FPriorNode : TLayoutNode;
    function GetBoundsRect: TRect;
    function GetChilds(Index : Integer): TLayoutNode;
    function GetStartAt: TPoint;
    function GetStopAt: TPoint;
  protected
    FPar : TLayoutNode;
    function GetCanvas : TFPCustomCanvas;
  public
    constructor Create(aDiv: TLayoutDiv; aDomNode: TDOMNode); virtual;
    destructor Destroy; override;
    {:Calculate kann für jede Node selbständig layouten StartAt gibt die Renderstart Koordinaten an
    //Nach dem Calkulieren müssen in StopAt die Koordinaten wo die nächste Node zu Zeichnen anfangen kann stehen.
    //In FBottomRight muss die Untere Rechte Ecke der Node gespeichert sein (StopAt kann eine neue Zeile sein)}
    procedure PrepareCalculate;
    procedure Calculate;virtual;
    {:Gibt den Nodetypen für die entsprecende DOMNode zurück, TLayoutInvisibleNode für eine nicht unterstützte Node}
    function FindNodeType(aNode : TDOMNode) : TLayoutNodeClass;
    {:Findet die LayoutNode zur DOM Node in den Childs (auch Rekursiv)}
    function GetLayoutNode(aDOMNode : TDOMNode) : TLayoutNode;
    property DOMNode : TDOMNode read FNode;
    {:Die nächsthöhere Sektion an der das Element sich orientiert (z.b. um umbrüche zu berechnen)}
    property Section : TLayoutDiv read FDiv;
    {:Nächsthöhere Ebene (Listenvorfahr)}
    property Parent : TLayoutNode read FPar;
    {:Listenelemente}
    property Childs[Index : Integer] : TLayoutNode read GetChilds;
    {:Ist das Element im angegebenen Rechteck enthalten ?}
    function IsInView(ViewPort : TRect) : Boolean;
    {:Ausgabe auf einem (FP)Canvas}
    procedure Render(ViewPort : TRect);virtual;
    {:Markiert das Objekt als geändert und wird beim nächsten versuch auf Layoutdaten zuzugreifen neu berechnet}
    procedure Change;
    {:Ist das Element geändetr ?}
    property Changed : Boolean read FChanged;
    {:Gibt das umgebende Rechteck zurück}
    property BoundsRect : TRect read GetBoundsRect;
    {:Hier ist der oberste Linke Punkt des Objektes}
    property StartAt : TPoint read GetStartAt;
    {:Hier kann das nächste Objekt begonnen werden}
    property StopAt : TPoint read GetStopAt;
    {:Soll das Objekt fliessend oder Blockweise gerendert werden ?}
    property Display : TLayoutDisplay read FDisplay;
    {:Gibt das nächste Objekt nach diesem zurück}
    function GetNext : TLayoutNode;
  end;

  TLayoutInvisibleNode = class(TLayoutNode)
  public
    procedure Calculate;override;
  end;

  {:Divider Objekt es beherbergt andere Objekte. Zeilen werden an seinen Grenzen umgebrochen z.b. Tabellenzellen }

  TLayoutDiv = class(TLayOutNode)
  private
    FWidth: Integer;
    procedure SetWidth(AValue: Integer);
  protected
    LastAddedNode : TLayoutNode;
  public
    property Width : Integer read FWidth write SetWidth;
  end;

  TLayoutFont = class(TLayoutInvisibleNode)
  public
    procedure Calculate;override;
  end;

  {:Text Objekt}

  { TLayoutText }

  TLayoutText = class(TLayoutNode)
  private
    FLines : TStringList;
    FText: string;
    procedure SetText(AValue: string);
  protected
    procedure ClearLines;
  public
    constructor Create(aDiv: TLayoutDiv; aDomNode: TDOMNode); override;
    destructor Destroy; override;
    procedure Calculate;override;
    procedure Render(ViewPort: TRect);override;
    property Text : string read FText write SetText;
  end;
  TLineObj = class
  private
    FRect: TRect;
  public
    constructor Create(aStartPos,aEndPos : TPoint);
    property Rect : TRect read FRect;
  end;

implementation

procedure Debug(s : string);
begin
  writeln(s);
end;

function TTextDocumentReader.Read(Document: TLayoutedDocument;
  PriorNode: TObject): TLayoutNode;
begin

end;

{ TLayoutDiv }

procedure TLayoutDiv.SetWidth(AValue: Integer);
var
  aNode: TLayoutNode;
begin
  if FWidth=AValue then Exit;
  aNode := nil;
  if Count>0 then
    aNode := TLayoutNode(Items[0]);
  while Assigned(aNode) do
    begin
      if aNode.BoundsRect.Right=Self.BoundsRect.Right then
        aNode.Change;
      aNode := aNode.GetNext;
    end;
  FWidth:=AValue;
end;

constructor TLineObj.Create(aStartPos, aEndPos: TPoint);
begin
  FRect := types.Rect(aStartPos.x,aStartPos.y,aEndPos.x,aEndPos.y);
end;

procedure TLayoutText.SetText(AValue: string);
begin
  if FText=AValue then Exit;
  FText:=AValue;
  Change;
end;

procedure TLayoutText.ClearLines;
var
  i: Integer;
begin
  for i := 0 to FLines.Count-1 do
    FLines.Objects[i].Free;
  FLines.Clear;
end;

constructor TLayoutText.Create(aDiv: TLayoutDiv; aDomNode: TDOMNode);
begin
  inherited Create(aDiv, aDomNode);
  FLines := TStringList.Create;
end;

destructor TLayoutText.Destroy;
var
  i: Integer;
begin
  ClearLines;
  FLines.Free;
  inherited Destroy;
end;

procedure TLayoutText.Calculate;
var
  aPos: TPoint;
  aLineI : Integer = 0;
  aLine : string = '';
  PartWidth: Integer;
  aText: string;
  actPart: String;
  aLineStart: TPoint;
  LineBreak : Boolean = False;
  aNext: TLayoutNode;

  function GetTextPart : string;
  begin
    LineBreak:=False;
    if pos(' ',aText)>0 then
      begin
        Result := copy(aText,0,pos(' ',aText));
        aText := copy(aText,pos(' ',aText)+1,length(aText));
      end
    else if pos(#10,aText)>0 then
      begin
        LineBreak:=True;
        Result := StringReplace(copy(aText,0,pos(#10,aText)-1),#13,'',[rfReplaceAll]);
        aText := copy(aText,pos(#10,aText)+1,length(aText));
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
  ClearLines;
  FBottomRight:=aPos;
  actPart := GetTextPart;
  while actPart<>'' do
    begin
      if Assigned(GetCanvas) then
        PartWidth := GetCanvas.TextWidth(actPart)
      else PartWidth:=0;
      if (PartWidth+aPos.x<FDiv.Width) and (not LineBreak) then
        begin
          aLine += actPart;
          aPos.x+=PartWidth;
        end
      else
        begin //Line Break
          if Assigned(GetCanvas) then
            FLines.AddObject(aLine,TLineObj.Create(aLineStart,Point(aPos.x,aPos.y+GetCanvas.TextHeight(aLine))));
          inc(aLineI);
          if aPos.x>FBottomRight.x then
            FBottomRight.x:=aPos.x;
          if Assigned(GetCanvas) then
            aPos.y+=GetCanvas.TextHeight(aLine);
          FBottomRight.y:=aPos.y;
          aPos.x := 0;
          aLineStart := aPos;
          aLine := actPart;
        end;
      actPart := GetTextPart;
    end;
  if aLine<>'' then
    begin
      if Assigned(GetCanvas) then
        FLines.AddObject(aLine,TLineObj.Create(aLineStart,Point(aPos.x,aPos.y+GetCanvas.TextHeight(aLine))));
      if aPos.x>FBottomRight.x then
        FBottomRight.x:=aPos.x;
      inc(aLineI);
    end;
  if Display=ldBlock then  //Add Line Break
    begin
      if Assigned(GetCanvas) then
        aPos.y+=GetCanvas.TextHeight(aLine);
      aPos.x := 0;
      FBottomRight.y:=aPos.y;
    end;
  if (FStopAt.y<>aPos.y) or (FStopAt.x<>aPos.x) then
    begin
      FStopAt := aPos;
      aNext := GetNext;
      if Assigned(aNext) then aNext.Change;
    end;
  inherited Calculate;
end;

procedure TLayoutText.Render(ViewPort: TRect);
var
  i: Integer;
  aRect: TRect;
begin
  inherited Render(ViewPort);
  if not assigned(GetCanvas) then exit;
  for i := 0 to FLines.Count-1 do
    with TLineObj(FLines.Objects[i]) do
      begin
        aRect := Rect;
        GetCanvas.TextOut(aRect.Left,aRect.Top,FLines[i]);
      end;
end;

procedure TLayoutInvisibleNode.Calculate;
begin
  inherited Calculate;
  FChanged:=False;
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

function TLayoutNode.GetBoundsRect: TRect;
begin
  Result := Rect(0,0,0,0);
  if FChanged then
    begin
      PrepareCalculate;
      Calculate;
    end;
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

constructor TLayoutNode.Create(aDiv: TLayoutDiv; aDomNode: TDOMNode);
var
  cNode: TDOMNode;
  nNode: TLayoutNode;
begin
  inherited Create;
  FPar:=nil;
  FPriorNode:=nil;
  if Self is TLayoutDiv then aDiv := TLayoutDiv(Self);
  if Assigned(aDiv) then
    begin
      FPriorNode := aDiv.LastAddedNode;
      aDiv.LastAddedNode:=Self;
    end;
  FDocument := aDiv.FDocument;
  FNode := aDomNode;
  FDiv := aDiv;
  FChanged:=True;
  cNode := aDomNode.FirstChild;
  FDisplay:=ldBlock;
  while Assigned(cNode) do
    begin
      nNode := FindNodeType(cNode).Create(FDiv,cNode);
      Add(nNode);
      nNode.FPar:=Self;
      cNode := cNode.NextSibling;
    end;
end;

destructor TLayoutNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Childs[i].Free;
  inherited Destroy;
end;

procedure TLayoutNode.PrepareCalculate;
begin
  FStopAt:=StartAt;
end;

procedure TLayoutNode.Calculate;
var
  i: Integer;
begin
  if Assigned(FPriorNode) then
    debug('Calc:'+DOMNode.NodeName+','+DOMNode.NodeValue+' Start:'+IntToStr(StartAt.x)+','+IntToStr(StartAt.y)+' Prior:'+FPriorNode.DOMNode.NodeName)
  else
    debug('Calc:'+DOMNode.NodeName+','+DOMNode.NodeValue+' Start:'+IntToStr(StartAt.x)+','+IntToStr(StartAt.y));
  for i := 0 to Count-1 do
    begin
      TLayoutNode(Items[i]).PrepareCalculate;
      TLayoutNode(Items[i]).Calculate;
      if TLayoutNode(Items[i]).StopAt.x>FStopAt.x then FStopAt.x := TLayoutNode(Items[i]).StopAt.x;
      if TLayoutNode(Items[i]).StopAt.y>FStopAt.y then FStopAt.y := TLayoutNode(Items[i]).StopAt.y;
    end;
  debug('Calc:'+DOMNode.NodeName+','+DOMNode.NodeValue+' Stop:'+IntToStr(FStopAt.x)+','+IntToStr(FStopAt.y));
  FChanged:=False;
end;

function TLayoutNode.FindNodeType(aNode: TDOMNode): TLayoutNodeClass;
begin
  Result := TLayoutInvisibleNode;
  if (aNode.NodeName='font') then
    Result := TLayoutFont
  else if ((aNode.NodeName='p')) then
    Result := TLayoutText
  else
    begin
      debug(aNode.ClassName+' ('+aNode.NodeName+'='+aNode.NodeValue+')');
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

procedure TLayoutNode.Render(ViewPort: TRect);
var
  i: Integer;
begin
  if not IsInView(ViewPort) then exit;
  for i := 0 to Count-1 do
    if TLayoutNode(Items[i]).IsInView(ViewPort) then
      TLayoutNode(Items[i]).Render(ViewPort);
end;

function TLayoutNode.GetCanvas: TFPCustomCanvas;
begin
  if Assigned(FDocument) then
    Result := FDocument.Canvas
  else Result := nil;
end;

procedure TLayoutNode.Change;
begin
  FChanged:=True;
  FStopAt.X:=0;
  FStopAt.Y:=0;
end;

function TLayoutNode.GetNext: TLayoutNode;
var
  aIdx: Integer;
begin
  Result := nil;
  if Assigned(FPar) then
    begin
      aIdx := FPar.IndexOf(Self);
      if aIdx+1 < FPar.Count then
        Result := TLayoutNode(FPar.Items[aIdx+1])
      else Result := FPar.GetNext;
    end;
end;

procedure TLayoutedDocument.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  Layout.Width:=FWidth;
end;

function TLayoutedDocument.GetOwnNodeClass(aClass: TLayoutNodeClass
  ): TLayoutNodeClass;
begin
  Result := aClass;
end;

procedure TLayoutedDocument.SetCanvas(AValue: TFPCustomCanvas);
begin
  if FCanvas=AValue then Exit;
  FCanvas:=AValue;
end;

constructor TLayoutedDocument.Create;
begin
end;

destructor TLayoutedDocument.Destroy;
begin
  FLayout.Free;
  inherited Destroy;
end;

procedure TLayoutedDocument.LoadFromReader(aReader: TDocumentreader);
begin
  FLayout.Free;
  aReader.Read(Self,nil);
end;

procedure TLayoutedDocument.LoadFromStream(aStream: TStream;
  aReader: TDocumentreader);
var
  iNode: TDOMNode;
begin
end;

procedure TLayoutedDocument.Clear;
begin
  FreeAndNil(FLayout);
end;

procedure TLayoutedDocument.RenderToCanvas(aCanvas: TFPCustomCanvas;ViewPort : TRect);
var
  i: Integer;
begin
  if not Assigned(FLayout) then exit;
  FCanvas := aCanvas;
  for i := 0 to FLayout.Count-1 do
    TLayoutNode(FLayout[i]).Render(ViewPort);
  debug('Rendering end=============================')
end;

function TLayoutedDocument.FindLayoutNode(aDOMNode: TDOMNode): TLayoutNode;
var
  i: Integer;
begin
  Result := Layout.GetLayoutNode(aDOMNode);
end;

end.
