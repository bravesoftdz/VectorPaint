unit UEDialogList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TErrorDialogList }

  TErrorDialogList = class(TForm)
    ListBox1: TListBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ErrorDialogList: TErrorDialogList;

implementation

{$R *.lfm}

end.

