{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OpenRichedit;

interface

uses
  RichEdit, DOMLayout, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('OpenRichedit', @Register);
end.
