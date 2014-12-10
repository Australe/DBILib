// _____________________________________________________________________________
{
  Copyright (C) 1996-2014, All rights reserved, John Vander Reest

  This source is free software; you may redistribute, use and/or modify it under
  the terms of the GNU Lesser General Public License as published by the
  Free Software Foundation; either version 2 of the License, or (at your option)
  any later version.
  You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Version: 2.00
  Author: John Vander Reest

  Change History:
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 20/04/2005 12:15:28 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIToolTips;

interface

uses
  Classes, Controls, Windows, Messages;

type
  TDBICustomTooltip = class(THintWindow)
  private
    FAutoHint: Boolean;
    FHintControl: TWinControl;
    FHintControlWndProc: TFarProc;
    FLeft: Integer;
    FTop: Integer;
    FOwner: TWinControl;
    FOwnerWndProc: TWndMethod;

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysFontChanged(var Message: TMessage); message CM_SYSFONTCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;

    procedure SetLeft(Value: Integer);
    procedure SetTop(Value: Integer);
    procedure SetAutoHint(const Value: Boolean);

  protected
    procedure HintWindowRefresh; virtual;
    procedure HintWindowShow(const WindowVisible: Boolean);
    function HintWindowTopLeft: TPoint;
    function HintWindowVisible: Boolean;
    procedure HintControlWndProc(var Message: TMessage);
    procedure Notification(
      AComponent: TComponent;
      Operation: TOperation); override;
    procedure OwnerWndProc(var Message: TMessage); virtual;
    procedure Paint; override;
    procedure SetHintControl(const Value: TWinControl);

    property AutoHint: Boolean read FAutoHint write SetAutoHint;
    property HintControl: TWinControl read FHintControl write SetHintControl;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;

  end;  { TXiCustomTooltip }


  TDBITooltip = class(TDBICustomTooltip)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;

  published
    property AutoHint;
    property HintControl;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
  end;


  TDBIShapeTooltip = class(TDBICustomTooltip)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;

  public
    procedure ActivateHint(Rect: TRect; const AHint: string); override;

  end;


type
  TDBITooltips = class(TComponent)
  private
    FItems: TStringList;

  protected
    function GetItem(Index: TWinControl): TDBITooltip;
    function GetItems: TStringList;

    property List: TStringList read GetItems;

  public
    destructor Destroy; override;

    property Items[Index: TWinControl]: TDBITooltip read GetItem; default;

  end;


implementation

uses
  Forms, Graphics, WinProcs, SysUtils, Types;


{ TDBITooltips }

destructor TDBITooltips.Destroy;
begin
  FreeAndNil(FItems);

  inherited Destroy;
end;


function TDBITooltips.GetItem(Index: TWinControl): TDBITooltip;
var
  ItemIndex: Integer;

begin
  ItemIndex := List.IndexOf(Index.Name);
  if (ItemIndex < 0) then begin
    ItemIndex := List.AddObject(Index.Name, TDBITooltip.Create(Owner));

    Result := List.Objects[ItemIndex] as TDBITooltip;
    Result.AutoHint := False;
    Result.Font.Color := clRed;
    Result.Font.Size := 10;
    Result.HintControl := Index;
    Result.Left := 15;
    Result.Top := Index.Height-5;
  end;

  Result := List.Objects[ItemIndex] as TDBITooltip;
  Result.Caption := Index.Hint;
end;


function TDBITooltips.GetItems: TStringList;
begin
  if not Assigned(FItems) then begin
    FItems := TStringList.Create;
    FItems.Duplicates := dupIgnore;
    FItems.Sorted := True;
  end;
  Result := FItems;
end;





{ TDBIShapeTooltip }

const
  InflateX: Byte = 16;
  InflateY: Byte = 20;

// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 12:17:18 - Updated code.<br>
}
procedure TDBIShapeTooltip.ActivateHint(Rect: TRect; const AHint: string);
begin
  // Make window bigger to accommodate new non-rectangular shape
  Inc(Rect.Right, InflateX);
  Inc(Rect.Bottom, InflateY);

  // Set modified window size & move it on-screen
  inherited ActivateHint(Rect, AHint);
end;  { ActivateHint }


// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 11:26:43 - Updated code.<br>
}
procedure TDBIShapeTooltip.CreateParams(var Params: TCreateParams);
begin
  // Make window transparent
  Brush.Style := bsClear;

  // Set up normal attributes
  inherited CreateParams(Params);

  // Remove border
  Params.Style := Params.Style and not ws_Border;
end;  { CreateParams }


// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 12:17:43 - Updated code.<br>
}
procedure TDBIShapeTooltip.Paint;
var
  Rect: TRect;
  CCaption: array[0..255] of Char;

begin
  inherited Paint;
  Canvas.Brush.Color := Color;
  Rect := ClientRect;
  Canvas.Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
  OffsetRect(Rect, InflateX div 2, InflateY div 2);
  OffsetRect(Rect, InflateX div 2, InflateY div 2);
  Inc(Rect.Left);
  Canvas.Brush.Style := bsClear;
  StrPCopy(CCaption, Caption);
  DrawText(Canvas.Handle, CCaption, -1, Rect, dt_Left or dt_NoPrefix or dt_WordBreak);
  Canvas.Brush.Style := bsSolid;
end;  { Paint }





{ TDBITooltip }

// _____________________________________________________________________________
{**
  Jvr - 21/12/2007 13:32:08 - Initial code.<br>
}
constructor TDBITooltip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 21/12/2007 13:34:28 - Initial code.<br>

  Here we remove the border created on the windows API-level
  when the window is created and set the control to transparent
}
procedure TDBITooltip.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  if Transparent then begin
    Params.Style := Params.Style and not WS_BORDER;
    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/12/2007 13:54:22 - Initial code.<br>
}
function TDBITooltip.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;  { GetTransparent }


// _____________________________________________________________________________
{**
  Jvr - 21/12/2007 13:32:56 - Initial code.<br>
}
procedure TDBITooltip.SetTransparent(Value: Boolean);
begin
  if Transparent <> Value then
  begin
    if Value then begin
      ControlStyle := ControlStyle - [csOpaque];
    end
    else begin
      ControlStyle := ControlStyle + [csOpaque];
    end;

    Invalidate;
  end;
end;  { SetTransparent }





{ TDBICustomTooltip }

// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 11:44:01 - Initial code.<br>
}
constructor TDBICustomTooltip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOwner := AOwner as TWinControl;
  FOwnerWndProc := FOwner.WindowProc;
  FOwner.WindowProc := OwnerWndProc;

  FAutoHint := True;
  Color := Application.HintColor;
  Visible := False;

  SetHintControl(AOwner as TWinControl);
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 13:51:46 - Initial code.<br>
}
destructor TDBICustomTooltip.Destroy;
begin
  SetHintControl(nil);
  FOwner.WindowProc := FOwnerWndProc;

  inherited Destroy;
end;  { Destroy }


procedure TDBICustomTooltip.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  HintWindowRefresh;

  inherited;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/04/2005 14:40:23 - Initial code.<br>
}
procedure TDBICustomTooltip.CMParentFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  HintWindowRefresh;

  inherited;
end;


// _____________________________________________________________________________
{**
  Jvr - 03/05/2005 13:36:58 - Initial code.<br>
}
procedure TDBICustomTooltip.CMSysFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  HintWindowRefresh;

  inherited;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/04/2005 14:34:52 - Initial code.<br>
}
procedure TDBICustomTooltip.CMVisibleChanged(var Message: TMessage);
begin
  HintWindowRefresh;

  inherited;
end;  { CMVisibleChanged }


// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 16:19:49 - Initial code.<br>
}
procedure TDBICustomTooltip.HintControlWndProc(var Message: TMessage);
begin
  with Message do begin
    Result := CallWindowProc(FHintControlWndProc, FHintControl.Handle, Msg, WParam, LParam);

    case Msg of
      WM_Activate: HintWindowShow((LoWord(WParam) <> WA_INACTIVE) and Visible);
      WM_SetFocus: HintWindowShow(Visible);
      WM_KillFocus: HintWindowShow(False);
    end;  { case }
  end;
end;  { HintControlWndProc }


// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 17:15:23 - Initial code.<br>
}
procedure TDBICustomTooltip.HintWindowRefresh;
var
  HintRect: TRect;

begin
  if HintWindowVisible then begin
//{##JVR
    if Assigned(FHintControl) and FAutoHint then begin
      Caption := FHintControl.Hint;
    end;
(*##JVR//}
    else

      Caption := Format(
        'Left = %d, Top = %d, Width = %d, Height = %d',
        [Left, Top, Width, Height]
        );
//*)
      // How big should it be?
      HintRect := CalcHintRect(Screen.Width, Caption, Self);
//##JVR      HintRect := Rect(0, 0, Width, Height);

      // Where should it go
      if Assigned(FHintControl) then begin
        with FHintControl.ClientToScreen(HintWindowTopLeft)
          do OffsetRect(HintRect, X, Y);
      end
      else begin
        with HintWindowTopLeft
          do OffsetRect(HintRect, X, Y);
      end;

    // Display it
    ActivateHint(HintRect, Caption);
//    MoveWindow(Handle, HintRect.Left, HintRect.Top, HintRect.Right-HintRect.Left, HintRect.Bottom-HintRect.Top, True);
  end
  else begin
    // Keep object, but destroy underlying window
    ReleaseHandle;
  end;
end;  { HintWindowActivate }


// _____________________________________________________________________________
{**
  Jvr - 29/04/2005 13:48:54 - Initial code.<br>
}
procedure TDBICustomTooltip.HintWindowShow(const WindowVisible: Boolean);
const
  Visibility: array[Boolean] of Cardinal = (SWP_HIDEWINDOW, SWP_SHOWWINDOW);

begin
  // Where should it go
  if Assigned(FHintControl) then begin
    with FHintControl.ClientToScreen(HintWindowTopLeft) do
      SetWindowPos(Handle, HWND_TOPMOST, X, Y, Width, Height,
        Visibility[WindowVisible] or SWP_NOACTIVATE);
  end
  else begin
    with HintWindowTopLeft do
      SetWindowPos(Handle, HWND_TOPMOST, X, Y, Width, Height,
        Visibility[WindowVisible] or SWP_NOACTIVATE);
  end;
end;  { HintWindowShow }


// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 17:10:20 - Initial code.<br>
}
function TDBICustomTooltip.HintWindowTopLeft: TPoint;
begin
  Result := Point(FLeft, FTop);
end;  { HintWindowTopLeft }


// _____________________________________________________________________________
{**
  Jvr - 29/04/2005 17:47:12 - Initial code.<br>
}
function TDBICustomTooltip.HintWindowVisible: Boolean;
var
  HintPoint: TPoint;
  WindowOwner: TWinControl;

begin
  if Assigned(FHintControl) then begin
    WindowOwner := Owner as TWinControl;

{##JVR - Check if hint window is within the ParentWindow
    HintPoint := HintWindowTopLeft;
    Inc(HintPoint.X, FHintControl.Left);
    Inc(HintPoint.Y, FHintControl.Top + Height);
}
    // Check if the Hintcontrol is visible (within the parent window)
    HintPoint := Point(FHintControl.Left, FHintControl.Top);

    Result := Visible and PtInRect(
      Rect(0, 0, WindowOwner.ClientWidth, WindowOwner.ClientHeight), HintPoint
      );
  end
  else begin
    Result := Visible;
  end;
end;  { HintWindowVisible }


// _____________________________________________________________________________
{**
  Jvr - 21/04/2005 17:32:46 - Initial code.<br>
}
procedure TDBICustomTooltip.OwnerWndProc(var Message: TMessage);
begin
  with Message do begin
    case Msg of
      WM_Activate: HintWindowShow((LoWord(WParam) <> WA_INACTIVE) and Visible);
      WM_Move: HintWindowRefresh;
      WM_Size: HintWindowShow(HintWindowVisible);
      WM_SetFocus: HintWindowShow(Visible);
      WM_KillFocus: HintWindowShow(False);
    end;  { case }
  end;  { with }

  FOwnerWndProc(Message);
end;  { OwnerWndProc}


// _____________________________________________________________________________
{**
  Jvr - 05/05/2005 17:49:52 - Initial code.<br>
}
procedure TDBICustomTooltip.Notification(
  AComponent: TComponent;
  Operation: TOperation
  );
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FHintControl) then begin
    SetHintControl(nil);
  end;
end;  { Notification }


// _____________________________________________________________________________
{**
  Jvr - 20/04/2005 17:56:37 - Initial code.<br>
}
procedure TDBICustomTooltip.Paint;
var
  Rect: TRect;

begin
  Rect := ClientRect;
  Inc(Rect.Left, 2);
  Inc(Rect.Top, 2);
  Canvas.Font := Font;
  DrawText(Canvas.Handle, PChar(Caption), -1, Rect, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end;  { Paint }


// _____________________________________________________________________________
{**
  Jvr - 03/05/2005 16:52:48 - Initial code.<br>
}
procedure TDBICustomTooltip.SetAutoHint(const Value: Boolean);
begin
  FAutoHint := Value;
  HintWindowRefresh;
end;  { SetAutoHint }


// _____________________________________________________________________________
{**
  Jvr - 21/04/2005 15:38:42 - Initial code.<br>
}
procedure TDBICustomTooltip.SetHintControl(const Value: TWinControl);
begin
  if (Value = FHintControl) then begin
    Exit;
  end;

  if (Value <> nil) then begin
    Value.FreeNotification(Self);
  end;

{##JVR

  // If we have a previous Wndproc object instance, then free it
  if Assigned(FHintControlWndProcInstance) then begin
    FreeObjectInstance(FHintControlWndProcInstance);
    FHintControlWndProcInstance := nil;
  end;

  // If the Prior HintControl was subclassed then restore it's Window procedure
  if Assigned(FHintControlWndProc) and Assigned(FHintControl) then begin
    if FHintControl.HandleAllocated then begin
      SetWindowLong(
        FHintControl.Handle,
        gwl_WndProc,
        LongInt(FHintControlWndProc)
        );
    end;

    FHintControlWndProc := nil;
  end;

  // Subclass the newly assigned Hint Control Windows procedure
  if Assigned(Value) then begin
    FHintControlWndProcInstance := MakeObjectInstance(HintControlWndProc);
    FHintControlWndProc := TFarProc(SetWindowLong(
      Value.Handle,
      gwl_WndProc,
      LongInt(FHintControlWndProcInstance)
      ));
  end;
//}

  FHintControl := Value;
  HintWindowRefresh;
end;  { SetHintControl }


// _____________________________________________________________________________
{**
  Jvr - 29/04/2005 16:55:58 - Initial code.<br>
}
procedure TDBICustomTooltip.SetLeft(Value: Integer);
var
  NewTopLeft: TPoint;

begin
  FLeft := Value;

  // Calculate the new coordinates
  NewTopLeft.X := FLeft;
  NewTopLeft.Y := FTop;

  if Assigned(FHintControl) then begin
    NewTopLeft := FHintControl.ClientToScreen(NewTopLeft);
  end;

  SetBounds(NewTopLeft.X, NewTopLeft.Y, inherited Width, inherited Height);
  ScalingFlags := ScalingFlags + [sfLeft];
end;  { SetLeft }


// _____________________________________________________________________________
{**
  Jvr - 29/04/2005 16:56:04 - Initial code.<br>
}
procedure TDBICustomTooltip.SetTop(Value: Integer);
var
  NewTopLeft: TPoint;

begin
  FTop := Value;

  // Calculate the new coordinates
  NewTopLeft.X := FLeft;
  NewTopLeft.Y := FTop;

  if Assigned(FHintControl) then begin
    NewTopLeft := FHintControl.ClientToScreen(NewTopLeft);
  end;

  SetBounds(NewTopLeft.X, NewTopLeft.Y, inherited Width, inherited Height);
  ScalingFlags := ScalingFlags + [sfTop];
end; { SetTop }



end.
