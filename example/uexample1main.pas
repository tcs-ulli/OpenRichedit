unit uexample1main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  RichEdit, DOMLayout, DOM, SAX_HTML, DOM_HTML;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    procedure XML2Tree(tree: TTreeView; XMLDoc: TXMLDocument);
    { private declarations }
  public
    { public declarations }
    RE : TRichEdit;
    doc: THTMLDocument;
  end;

var
  Form1: TForm1;

implementation

uses domdocumentreader;

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  DN: TDOMText;
  Root: TDOMElement;
  aReader: TDOMReader;
begin
  RE := TRichEdit.Create(Self);
  RE.Parent:=Self;
  RE.Align := alClient;

  ReadHTMLFile(doc,'..'+DirectorySeparator+'..'+DirectorySeparator+'test1.html');
  XML2Tree(TreeView1,doc);
  aReader := TDOMReader.Create;
  aReader.DOM := doc;
  RE.Document.LoadFromReader(aReader);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Doc.Free;
end;

procedure TForm1.TreeView1SelectionChanged(Sender: TObject);
var
  aNode: TLayoutNode;
begin
  aNode := RE.Document.FindLayoutNode(TDOMNode(TreeView1.Selected.Data));
  RE.Refresh;
  if not Assigned(aNode) then exit;
  RE.Canvas.Brush.Style:=bsClear;
  RE.Canvas.Pen.Color:=clBlue;
  RE.Canvas.Rectangle(aNode.BoundsRect);
end;

procedure TForm1.XML2Tree(tree: TTreeView; XMLDoc: TXMLDocument);
var
  iNode: TDOMNode;

  procedure ProcessNode(Node: TDOMNode; TreeNode: TTreeNode);
  var
    cNode: TDOMNode;
    s: string;
  begin
    if Node = nil then Exit; // Stops if reached a leaf

    // Adds a node to the tree
    if Node.HasAttributes and (Node.Attributes.Length>0) then
      s := Node.NodeName+'('+Node.Attributes[0].NodeName+'='+Node.Attributes[0].NodeValue+')'
    else
      s := Node.NodeName;
    if Node.NodeValue<>'' then
      s := s+'='+Node.NodeValue;
    TreeNode := tree.Items.AddChildObject(TreeNode, s, Node);

    // Goes to the child node
    cNode := Node.FirstChild;

    // Processes all child nodes
    while cNode <> nil do
    begin
      ProcessNode(cNode, TreeNode);
      cNode := cNode.NextSibling;
    end;
  end;

begin
  iNode := XMLDoc.DocumentElement.FirstChild;
  while iNode <> nil do
  begin
    ProcessNode(iNode, nil); // Recursive
    iNode := iNode.NextSibling;
  end;
end;

end.

