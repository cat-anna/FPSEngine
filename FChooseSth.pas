unit FChooseSth;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, DXDraws, DIB, FPSEngine;

type
  TChooseFormType = (cftTeleportDest);
  TChooseForm = class(TForm)
    PageControl1: TPageControl;
    Button1: TButton;
    Button2: TButton;
    TabSheet1: TTabSheet;
    DXDraw: TDXDraw;
    Label1: TLabel;
    cbTeleMap: TComboBox;
    procedure FormDestroy(Sender: TObject);
    procedure cbTeleMapChange(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure DXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DXDrawMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FBackground: TDIB;
    FPlayerImg: TPictureCollectionItem;

    procedure TeleRedraw;
  public
    TeleportDest: T3Dpoint;
    DString: string;
    Tryb: TChooseFormType;

    procedure PrepereTeleportDestSelect(PImg: TPictureCollectionItem);
  end;

implementation

uses FMapEdytor, FMainForm;

{$R *.dfm}

procedure TChooseForm.PrepereTeleportDestSelect(PImg: TPictureCollectionItem);
begin
  TabSheet1.Visible := true;
  Tryb := cftTeleportDest;
  FPlayerImg := PImg;
  cbTeleMap.Items.Assign(MainForm.lbMaps.Items);
  cbTeleMap.ItemIndex := 0;
  cbTeleMapChange(nil);
end;

procedure TChooseForm.cbTeleMapChange(Sender: TObject);
begin
  with TMapEditor(MainForm.MapEditors[cbTeleMap.ItemIndex]) do
  begin
    DString := FPSMap.MapName;
    FBackground := MapSnapShot;
  end;
  TeleRedraw;
end;

procedure TChooseForm.DXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  TeleRedraw;
  with FPlayerImg do
     DrawRotate(DXDraw.Surface, round(X - Width / 2), round(Y - Height / 2), Width, Height, 0,
     0.5, 0.5, TeleportDest.Y / AngleFacor);
  DXDraw.Flip;
end;

procedure TChooseForm.DXDrawMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TeleportDest.X := -X * GirdZoom;
  TeleportDest.Z := -Y * GirdZoom;
  TeleRedraw;
end;

procedure TChooseForm.FormCreate(Sender: TObject);
begin
  TabSheet1.Visible := false;
  DXDraw.Initialize;
end;

procedure TChooseForm.FormDestroy(Sender: TObject);
begin
  DXDraw.Finalize;
end;

procedure TChooseForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Tryb <> cftTeleportDest then exit;
  TeleportDest.Y := TeleportDest.Y + 10;
  if TeleportDest.Y > 360 then
    TeleportDest.Y := TeleportDest.Y - 360;
  TeleRedraw;
end;

procedure TChooseForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Tryb <> cftTeleportDest then exit;
  TeleportDest.Y := TeleportDest.Y - 10;
  if TeleportDest.Y < 0 then
    TeleportDest.Y := TeleportDest.Y + 360;
  TeleRedraw;
end;

procedure TChooseForm.TeleRedraw;
begin
  with DXDraw do
  begin
    PasteImage(FBackground, 0, 0);
    with FPlayerImg do
      DrawRotate(DXDraw.Surface, round(-TeleportDest.X * girdSize - Width / 2),
      round(-TeleportDest.Z * girdSize - Height / 2), Width, Height, 0,
      0.5, 0.5, TeleportDest.Y / AngleFacor);
    Flip;
  end;
end;

end.
