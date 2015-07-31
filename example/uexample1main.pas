unit uexample1main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,RichEdit,
  DOM;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    RE : TRichEdit;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  DN: TDOMText;
begin
  RE := TRichEdit.Create(Self);
  RE.Parent:=Self;
  RE.Align := alClient;

  DN := TDOMText.Create(RE.DOM);
  RE.DOM.AppendChild(DN);
  DN.AppendData('Test string zum rendern');
end;

end.

