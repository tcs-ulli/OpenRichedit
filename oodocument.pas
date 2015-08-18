unit oodocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils,richdocument, zipper,
  XMLRead, DOM, XMLWrite;

type

  { TODFDocument }

  TODFDocument = class(TCustomRichDocument)
  private
    Content:txmldocument;
    FFilename : string;
    procedure ReadStyles(AStylesNode: TDOMNode);
    procedure ReadAutomaticStyles(AStylesNode: TDOMNode);
    procedure ReadMasterStyles(AStylesNode: TDOMNode);
  public
    procedure Open; override;
    function ExtractFile(aFilename : string;OutputPath : string) : Boolean;
  end;

implementation

{ TODFDocument }

procedure TODFDocument.ReadStyles(AStylesNode: TDOMNode);
begin

end;

procedure TODFDocument.ReadAutomaticStyles(AStylesNode: TDOMNode);
begin

end;

procedure TODFDocument.ReadMasterStyles(AStylesNode: TDOMNode);
begin

end;

procedure TODFDocument.Open;
var
  FilePath: String;
  UnZip: TUnZipper;
  FileList: TStringList;
  StylesNode: TDOMNode;
  tmp: DOMString;
  Doc : TXMLDocument;
  RootNode: TDOMElement;
begin
  FilePath := GetTempDir(false);
  UnZip := TUnZipper.Create;
  FileList := TStringList.Create;
  try
    FileList.Add('styles.xml');
    FileList.Add('content.xml');
    FileList.Add('settings.xml');
    UnZip.OutputPath := FilePath;
    Unzip.UnZipFiles(FileName,FileList);
  finally
    FreeAndNil(FileList);
    FreeAndNil(UnZip);
  end; //try
  try
    Doc := TXMLDocument.Create;
    // process the styles.xml file
    ReadXMLFile(Doc, FilePath+'styles.xml');
    DeleteFile(FilePath+'styles.xml');

    StylesNode := Self.DocumentElement.FindNode('office:styles');
    ReadStyles(StylesNode);

    StylesNode := Self.DocumentElement.FindNode('office:automatic-styles');
    ReadAutomaticStyles(StylesNode);

    StylesNode := Self.DocumentElement.FindNode('office:master-styles');
    ReadMasterStyles(StylesNode);

    Doc.Free;

    //process the content.xml file
    ReadXMLFile(TXMLDocument(Self), FilePath+'content.xml');
    DeleteFile(FilePath+'content.xml');

    StylesNode := Self.DocumentElement.FindNode('office:automatic-styles');
    ReadStyles(StylesNode);

    RootNode := TDOMElement(Self.DocumentElement.FindNode('office:body'));
    if not Assigned(BodyNode) then Exit;

    BodyNode := TDomElement(RootNode.FindNode('office:text'));
    if not Assigned(BodyNode) then Exit;
  finally
  end;
end;

function TODFDocument.ExtractFile(aFilename: string; OutputPath: string
  ): Boolean;
var
  UnZip: TUnZipper;
  FileList: TStringList;
begin
  UnZip := TUnZipper.Create;
  FileList := TStringList.Create;
  try
    FileList.Add(aFilename);
    UnZip.OutputPath := OutputPath;
    Unzip.UnZipFiles(FileName,FileList);
  finally
    FreeAndNil(FileList);
    FreeAndNil(UnZip);
  end; //try
end;

end.

