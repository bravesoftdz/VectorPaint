unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls,
  Buttons, ComCtrls, Grids, Spin, UVisualTools, UTransformation,
  URTTI, Math, UFigures, UEdits, UGraph, USaveLoadToLPF, UEditsStatic, UAbout;

{ TVectorEditMainForm }

type
  TVectorEditMainForm = class(TForm)
    Big_Left_Panel: TPanel;
    FSE_ScaleEdit: TFloatSpinEdit;
    Menu_Move_Down: TMenuItem;
    Menu_Cut: TMenuItem;
    Menu_Move_Up: TMenuItem;
    Menu_Delete: TMenuItem;
    Menu_Paste: TMenuItem;
    Menu_Copy: TMenuItem;
    Menu_Edit: TMenuItem;
    TB_Open: TToolButton;
    TB_Save: TToolButton;
    TB_Exit: TToolButton;
    TB_Separator1: TToolButton;
    TB_Separator2: TToolButton;
    TB_Separator3: TToolButton;
    TB_Separator4: TToolButton;
    TB_OriginalScale: TToolButton;
    TB_Separator5: TToolButton;
    ToolBar_Main: TToolBar;
    Tools_Panel: TPanel;
    Edits_Panel: TPanel;

    Big_Bottom_Panel: TPanel;
    Sh_Background_Color: TShape;
    StatusBar_Main: TStatusBar;
    Color_DrawGrid: TDrawGrid;
    SH_Brush_Color: TShape;
    SH_Pen_Color: TShape;
    Right_Scroll_Panel: TPanel;
    SB_Right: TScrollBar;
    Bottom_Scroll_Panel: TPanel;
    SB_Bottom: TScrollBar;
    Central_Part_Panel: TPanel;
    Left_Part_Panel: TPanel;
    ToolBar1: TToolBar;

    TB_Centering: TToolButton;
    TB_Delete: TToolButton;
    TB_Move_Up: TToolButton;
    TB_Move_Down: TToolButton;
    TB_Paste: TToolButton;
    TB_Copy: TToolButton;
    TB_Cut: TToolButton;

    Main_OpenDialog: TOpenDialog;
    Main_ColorDialog: TColorDialog;
    Main_SaveDialog: TSaveDialog;
    Main_ImageList: TImageList;
    Main_Menu: TMainMenu;

    Menu_SaveAs: TMenuItem;
    Menu_Open: TMenuItem;
    Menu_File: TMenuItem;
    Menu_Help: TMenuItem;
    Menu_Exit: TMenuItem;
    Menu_About: TMenuItem;

    Main_PaintBox: TPaintBox;
    procedure Color_DrawGridDblClick(Sender: TObject);
    procedure Color_DrawGridDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure Color_DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormCreate(Sender: TObject);
    procedure FSE_ScaleEditChange(Sender: TObject);

    procedure Menu_AboutClick(Sender: TObject);
    procedure Main_PaintBoxMouseLeave(Sender: TObject);
    procedure Sh_Background_ColorMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure SpeedButtonClick(Sender: TObject);
    procedure CreateControlEx(ParentClass: TWinControl);
    procedure Main_PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Main_PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure Main_PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure Main_PaintBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure Main_PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure Main_PaintBoxPaint(Sender: TObject);
    procedure SB_BottomChange(Sender: TObject);
    procedure SB_RightChange(Sender: TObject);
    procedure SH_ColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure StatusBar_MainResize(Sender: TObject);

    procedure TB_CenteringClick(Sender: TObject);
    procedure TB_ExitClick(Sender: TObject);
    procedure TB_OpenClick(Sender: TObject);
    procedure TB_OriginalScaleClick(Sender: TObject);
    procedure TB_DeleteClick(Sender: TObject);
    procedure TB_Move_UpClick(Sender: TObject);
    procedure TB_Move_DownClick(Sender: TObject);
    procedure TB_PasteClick(Sender: TObject);
    procedure TB_CopyClick(Sender: TObject);
    procedure TB_CutClick(Sender: TObject);
    procedure TB_SaveClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  VectorEditMainForm: TVectorEditMainForm;
  Tools: TToolsClass;
  Clipboard: array of TFigure;
  CellsColors: array[0..24, 0..1] of
  TColor = ((clBlack, clMaroon), (clGreen, clOlive), (clNavy, clPurple),
    (clTeal, clGray), (clSilver, clRed), (clLime, clYellow),
    (clBlue, clFuchsia), (clAqua, clLtGray), (clDkGray, clWhite),
    (clMoneyGreen, clSkyBlue), (clCream, clMedGray), (clNone, clDefault),
    (clScrollBar, clBackground), (clActiveCaption, clInactiveCaption),
    (clMenu, clWindow), (clWindowFrame, clMenuText),
    (clWindowText, clCaptionText), (clActiveBorder, clInactiveBorder),
    (clAppWorkspace, clHighlight), (clHighlightText, clBtnFace),
    (clBtnShadow, clGrayText), (clBtnText, clInactiveCaptionText),
    (clBtnHighlight, cl3DDkShadow), (cl3DLight, clInfoText),
    (clInfoBk, clHotLight));

const
  BRUSH_COLOR = clWhite;
  PEN_COLOR = clBlack;
  BACKGROUND_COLOR = clWhite;

implementation

{$R *.lfm}

{ TVectorEditMainForm }

procedure TVectorEditMainForm.SpeedButtonClick(Sender: TObject);
var
  I: integer;
begin
  Edits_Panel.DestroyComponents;
  Tools := ControlTools.Class_Tools_Array[TSpeedButton(Sender).Tag];
  Tools.Proper;
  GetComponentProperties(FakeFigure, Edits_Panel);
  ControlTools.Set_Selected_On_Draw;
  ControlTools.MAIN_Invalidate;
end;

procedure TVectorEditMainForm.CreateControlEx(ParentClass: TWinControl);
var
  I: integer;
  SpBt_New: TSpeedButton;
begin
  ParentClass.Height := 8 + 40 * Ceil(Length(ControlTools.Class_Tools_Array) / 3);
  for I := 0 to High(ControlTools.Class_Tools_Array) do begin
    SpBt_New := TSpeedButton.Create(ParentClass);
    with SpBt_New do begin
      OnClick := @SpeedButtonClick;
      Parent := ParentClass;
      Tag := I;
      GroupIndex := 1;
      SetBounds((I mod 3) * 40 + 8, (I div 3) * 40 + 8, 32, 32);
      ShowHint := True;
      Hint := ControlTools.Class_Tools_Array[I].GetHint;
      Glyph.LoadFromFile(ControlTools.Class_Tools_Array[I].GetGlyph);
      if I = 5 then begin
        Down := True;
        Tools := ControlTools.Class_Tools_Array[I];
      end;
    end;
  end;
end;

procedure TVectorEditMainForm.Main_PaintBoxPaint(Sender: TObject);
begin
  ControlTools.Re_Draw_Canvas(Main_PaintBox.Canvas);
end;

procedure TVectorEditMainForm.SB_BottomChange(Sender: TObject);
begin
  Trans.OffsetX := SB_Bottom.Position;
  Main_PaintBox.Invalidate;
end;

procedure TVectorEditMainForm.SB_RightChange(Sender: TObject);
begin
  Trans.OffsetY := SB_Right.Position;
  Main_PaintBox.Invalidate;
end;

procedure TVectorEditMainForm.FormCreate(Sender: TObject);
begin
  CreateControlEx(Tools_Panel);

  ControlTools.MAIN_Invalidate := @Main_PaintBox.Invalidate;
  ControlEdits.MAIN_Invalidate := @Main_PaintBox.Invalidate;
  ControlEditsNew.MAIN_Invalidate := @Main_PaintBox.Invalidate;

  Trans.ScrollBarX := SB_Bottom;
  Trans.ScrollBarY := SB_Right;
  Trans.FSEScale := FSE_ScaleEdit;
  Trans.Scale := FSE_ScaleEdit.Value;

  SH_Background_Color.Brush.Color := BACKGROUND_COLOR;
  SH_Brush_Color.Brush.Color := BRUSH_COLOR;
  SH_Pen_Color.Brush.Color := PEN_COLOR ;

  ControlTools.Main_Panel := Edits_Panel;

  ControlEditsNew.Register_Controls(VectorEditMainForm.SH_Pen_Color);
  ControlEditsNew.Register_Controls(VectorEditMainForm.SH_Brush_Color);

  GlobalPenColor := @SH_Pen_Color.Brush.Color;
  GlobalBrushColor := @SH_Brush_Color.Brush.Color;
  GlobalBackgroundColor := @SH_Background_Color.Brush.Color;
end;

procedure TVectorEditMainForm.FSE_ScaleEditChange(Sender: TObject);
begin
  Trans.Scale := FSE_ScaleEdit.Value;
  Trans.RefreshScrollBar;
  Invalidate;
end;

//------------------------- Муню -----------------------------------------------

procedure TVectorEditMainForm.Menu_AboutClick(Sender: TObject);
begin
  About.ShowModal;
end;

//------------------------- Обработка Событий Главного Холста ------------------

procedure TVectorEditMainForm.Main_PaintBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  Tools.MouseUp(Shift, Trans.S2W(Point(X, Y)), Main_PaintBox.Canvas);
end;

procedure TVectorEditMainForm.Main_PaintBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  Tools.MouseDown(Shift, Trans.S2W(Point(X, Y)), Main_PaintBox.Canvas);
end;

procedure TVectorEditMainForm.Main_PaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  W_MouseP: TDoublePoint;
begin
  W_MouseP := Trans.S2W(Point(X, Y));
  with StatusBar_Main.Panels do begin
    Items[1].Text := 'Экранные X: ' + IntToStr(X) +
      ' Y: ' + IntToStr(Y);
    Items[2].Text := 'Мировые X: ' + FloatToStrF(W_MouseP.X, ffFixed, 0, 2) +
      ' Y: ' + FloatToStrF(W_MouseP.Y, ffFixed, 0, 2);
  end;
  Tools.MouseMove(Shift, W_MouseP, Main_PaintBox.Canvas);
end;

procedure TVectorEditMainForm.Main_PaintBoxMouseLeave(Sender: TObject);
begin
  StatusBar_Main.Panels[1].Text := '';
  StatusBar_Main.Panels[2].Text := '';
end;

procedure TVectorEditMainForm.Main_PaintBoxMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  Trans.ScrollReduction(MousePos);
  Invalidate;
end;

procedure TVectorEditMainForm.Main_PaintBoxMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  Trans.ScrollIncrease(MousePos);
  Invalidate;
end;

//------------------------- Обработка Цветов Кисти и Заливки -------------------

procedure TVectorEditMainForm.SH_ColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Main_ColorDialog.Execute then
    TShape(Sender).Brush.Color := Main_ColorDialog.Color;
end;

procedure TVectorEditMainForm.Sh_Background_ColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Main_ColorDialog.Execute then begin
    Sh_Background_Color.Brush.Color := Main_ColorDialog.Color;
    Invalidate;
  end;
end;

//------------------------- Панель Состояний -----------------------------------

procedure TVectorEditMainForm.StatusBar_MainResize(Sender: TObject);
begin
  StatusBar_Main.Panels.Items[0].Width := StatusBar_Main.Width - 350;
  SB_Bottom.PageSize := SB_Bottom.Width;
  SB_Right.PageSize := SB_Right.Height;
end;

//------------------------- Верхнее меню ---------------------------------------

procedure Swap(var AObject1, AObject2: TFigure);
var
  AObject3: TFigure;
begin
  AObject3 := AObject1;
  AObject1 := AObject2;
  AObject2 := AObject3;
end;

procedure TVectorEditMainForm.TB_CenteringClick(Sender: TObject);
begin
  Trans.Scaling(Trans.W2S(Trans.FilledArea));
  Trans.Centering(Trans.W2S(Trans.FilledArea));
  Trans.RefreshScrollBar;
  Invalidate;
end;

procedure TVectorEditMainForm.TB_ExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVectorEditMainForm.TB_OpenClick(Sender: TObject);
begin
  if Main_OpenDialog.Execute then
    Open(Utf8ToAnsi(Main_OpenDialog.FileName));
end;

procedure TVectorEditMainForm.TB_OriginalScaleClick(Sender: TObject);
begin
  Trans.Scale := 1;
  Trans.RefreshScrollBar;
end;

procedure TVectorEditMainForm.TB_DeleteClick(Sender: TObject);
var
  I, J, ArrayLength: integer;
begin
  ArrayLength := 0;
  I := 0;
  while I <= High(MAIN_FIGURE_ARRAY) - ArrayLength do
    if MAIN_FIGURE_ARRAY[I].SelectedFigure = SelectedDrawFigure then
    begin
      MAIN_FIGURE_ARRAY[I].Free;
      Inc(ArrayLength);
      for J := I to High(MAIN_FIGURE_ARRAY) - ArrayLength do
        Swap(MAIN_FIGURE_ARRAY[J], MAIN_FIGURE_ARRAY[J + 1]);
    end
    else
      Inc(I);
  SetLength(MAIN_FIGURE_ARRAY, Length(MAIN_FIGURE_ARRAY) - ArrayLength);
  Invalidate;
end;

procedure TVectorEditMainForm.TB_Move_UpClick(Sender: TObject);
var
  I, J, ArrayLength: integer;
begin
  ArrayLength := 0;
  I := 0;
  while I <= High(MAIN_FIGURE_ARRAY) - ArrayLength do
    if MAIN_FIGURE_ARRAY[I].SelectedFigure = SelectedDrawFigure then begin
      Inc(ArrayLength);
      for J := I to High(MAIN_FIGURE_ARRAY) - 1 do
        Swap(MAIN_FIGURE_ARRAY[J], MAIN_FIGURE_ARRAY[J + 1]);
    end
    else
      Inc(I);

  Invalidate;
end;

procedure TVectorEditMainForm.TB_Move_DownClick(Sender: TObject);
var
  I, J, ArrayLength: integer;
begin
  ArrayLength := 0;
  I := High(MAIN_FIGURE_ARRAY);
  while I >= ArrayLength do
    if MAIN_FIGURE_ARRAY[I].SelectedFigure = SelectedDrawFigure then begin
      Inc(ArrayLength);
      for J := I downto 1 do
        Swap(MAIN_FIGURE_ARRAY[J], MAIN_FIGURE_ARRAY[J - 1]);
    end
    else
      Dec(I);

  Invalidate;
end;

procedure TVectorEditMainForm.TB_PasteClick(Sender: TObject);
var
  I, ArrayLength: integer;
begin
  SetLength(MAIN_FIGURE_ARRAY, Length(MAIN_FIGURE_ARRAY) + Length(Clipboard));
  for I := 0 to High(Clipboard) do
    MAIN_FIGURE_ARRAY[High(MAIN_FIGURE_ARRAY) - I] := CopyFigure(Clipboard[I]);

  Invalidate;
end;

procedure TVectorEditMainForm.TB_CopyClick(Sender: TObject);
var
  I, J: integer;
  NewInst: TFigure;
begin
  for I := 0 to High(Clipboard) do
    Clipboard[I].Free;
  SetLength(Clipboard, 0);
  for I := 0 to High(MAIN_FIGURE_ARRAY) do
    if MAIN_FIGURE_ARRAY[I].SelectedFigure = SelectedDrawFigure then begin
      SetLength(Clipboard, Length(Clipboard) + 1);
      Clipboard[High(Clipboard)] := CopyFigure(MAIN_FIGURE_ARRAY[I]);
    end;
end;

procedure TVectorEditMainForm.TB_CutClick(Sender: TObject);
begin
  TB_CopyClick(Sender);
  TB_DeleteClick(Sender);
end;

procedure TVectorEditMainForm.TB_SaveClick(Sender: TObject);
begin
  if Main_SaveDialog.Execute then
    Save(Utf8ToAnsi(Main_SaveDialog.FileName));
end;

//------------------------- Палитра --------------------------------------------

procedure TVectorEditMainForm.Color_DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
    SH_Pen_Color.Brush.Color := CellsColors[X div 18, Y div 18];

  if Button = mbRight then
    SH_Brush_Color.Brush.Color := CellsColors[X div 18, Y div 18];
end;

procedure TVectorEditMainForm.Color_DrawGridDrawCell(Sender: TObject;
  aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
begin
  with Color_DrawGrid.Canvas do begin
    Brush.Color := CellsColors[aCol, aRow];
    FillRect(aRect);
  end;
end;

procedure TVectorEditMainForm.Color_DrawGridDblClick(Sender: TObject);
begin
  if Main_ColorDialog.Execute then
    with Color_DrawGrid, Color_DrawGrid.Canvas  do begin
      CellsColors[Col, Row] := Main_ColorDialog.Color;
      Brush.Color := Main_ColorDialog.Color;
      FillRect(CellRect(Col, Row));
    end;
end;

initialization

end.
