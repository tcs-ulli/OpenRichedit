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
  bNode: TDOMNode;
  aContent: TXMLDocument;
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

    StylesNode := Doc.DocumentElement.FindNode('office:styles');
    ReadStyles(StylesNode);

    StylesNode := Doc.DocumentElement.FindNode('office:automatic-styles');
    ReadAutomaticStyles(StylesNode);

    StylesNode := Doc.DocumentElement.FindNode('office:master-styles');
    ReadMasterStyles(StylesNode);

    Doc.Free;

    DeleteFile(FilePath+'settings.xml');

    //process the content.xml file
    aContent := Content;
    ReadXMLFile(aContent, FilePath+'content.xml');
    Content := aContent;
    DeleteFile(FilePath+'content.xml');

    StylesNode := Content.DocumentElement.FindNode('office:automatic-styles');
    ReadStyles(StylesNode);

    RootNode := TDOMElement(Content.DocumentElement.FindNode('office:body'));
    if not Assigned(RootNode) then Exit;

    bNode := RootNode.FindNode('office:text');
    BodyNode := bNode as TDOMElement;
    if not Assigned(BodyNode) then
      BodyNode := RootNode;
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
  Result := FileExists(OutputPath+aFilename);
end;

end.

