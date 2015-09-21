unit domdocumentreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, DOM_HTML, DOMLayout;

 type
   TDOMReader = class(TDocumentreader)
   private
     FDOM: TDOMDocument;
   public
     function Read(Document: TLayoutedDocument;PriorNode : TObject): TLayoutNode; override;
     property DOM : TDOMDocument read FDOM write FDOM;
   end;

implementation

function TDOMReader.Read(Document: TLayoutedDocument; PriorNode: TObject
  ): TLayoutNode;
begin
  if PriorNode=nil then
    Result := Document.GetOwnNodeClass(TLayoutDiv).Create(nil,FDOM.DocumentElement.FindNode('body'))
  else
    begin
      NextNode := TDOMNode(PriorNode).FirstChild;
      NextNode := TDOMElement(PriorNode).NextSibling;

    end;
end;

end.

