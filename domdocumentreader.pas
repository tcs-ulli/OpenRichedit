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
     function Read(Document: TLayoutedDocument): Boolean; override;
     property DOM : TDOMDocument read FDOM write FDOM;
   end;

implementation

function TDOMReader.Read(Document: TLayoutedDocument): Boolean;
begin
  Document.Layout := TLayoutDiv.Create(nil,FDOM.DocumentElement.FindNode('body'));
end;

end.

