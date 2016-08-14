program VectorEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, lazcontrols, UMain, UVisualTools,
  UNotVisualTools, UTransformation, URTTI, UEditsStatic, UFigures,
  UGraph, USaveLoadToLPF, UExceptions, UEDialogList, UEdits, UAbout;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TVectorEditMainForm, VectorEditMainForm);
  Application.CreateForm(TErrorDialogList, ErrorDialogList);
  Application.CreateForm(TAbout, About);
  Application.Run;
end.

