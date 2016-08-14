unit UExceptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UEDialogList;

type
  EErrorList = Class(Exception)
  protected
    _StrList: TStringList;
  public
    constructor CreateList(AStr: TStringList);
    procedure ShowDialogList;
  end;

implementation

constructor EErrorList.CreateList(AStr: TStringList);
begin
  _StrList := AStr;
end;

procedure EErrorList.ShowDialogList;
begin
  ErrorDialogList.ListBox1.Items := _StrList;
  ErrorDialogList.ShowModal;
end;

end.

