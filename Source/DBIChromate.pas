unit DBIChromate;

{#omcodecop off : jvr : dbilib}

interface

{$define UserDefinedDialogs}

uses
  Windows, Messages, Classes, SysUtils, Controls, Comctrls, Extctrls, Stdctrls,
  Actions, StdActns, ActnList, Menus, Buttons, CefLib, CefVcl, DBIToolTips;

type
  TDBICustomChromate = class(TChromium)
  private
    FActions: TActionList;

  protected
    function GetActionByName(const ActionName: String): TAction;
    function GetDefaultUrl: String; virtual;

    procedure doOnAddressChange(
      const Browser: ICefBrowser;
      const Frame: ICefFrame;
      const Url: UString
      ); override;
{$ifdef UserDefinedDialogs}
    function doOnJsdialog(
      const Browser: ICefBrowser;
      const OriginUrl: UString;
      const AcceptLang: UString;
      DialogType: TCefJsDialogType;
      const MessageText: UString;
      const DefaultPromptText: UString;
      Callback: ICefJsDialogCallback;
      out SuppressMessage: Boolean
      ): Boolean; override;
{$endif}
    procedure SetDefaultUrl(const Value: String); virtual;

  protected
    procedure CheckStability;

    class procedure RegisterSchemes(const Registrar: ICefSchemeRegistrar);

    property Actions: TActionList read FActions write FActions;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsMainFrame(const Browser: ICefBrowser; const Frame: ICefFrame = nil): Boolean;
    procedure LoadFromFile(const AFileName: TFileName);

    property DefaultUrl: String read GetDefaultUrl write SetDefaultUrl;

  end;


  TDBIChromate = class(TDBICustomChromate)
  public
    class function Build(AOwner: TComponent; AParent: TWinControl): TDBIChromate;
    class function Register: Boolean;

  end;


  TDBIDeveloperToolsClient = class(TComponent)
  private
    FHandle: HWnd;
    FParent: TWinControl;

  protected
    procedure CreateWindowHandle;
    procedure CreateWindowInfo(var WindowInfo: TCefWindowInfo); virtual;
    procedure DestroyWindowHandle;

    function GetChromium: TChromium;
    function GetHandle: HWND;
    function GetParentHandle: HWND;
    function GetVisible: Boolean;
    function GetWindowHandle: HWND;

    procedure SetVisible(const Value: Boolean);

    property Chromium: TChromium read GetChromium;
    property WindowHandle: HWnd read FHandle write FHandle;

  public

    property Parent: TWinControl read FParent write FParent;
    property Handle: HWND read GetHandle;
    property Visible: Boolean read GetVisible write SetVisible;
  end;


type
  TDBIDeveloperTools = class(TWinControl)
  private
    FChromium: TChromium;
    FToolsClient: TDBIDeveloperToolsClient;

    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;

  protected
    procedure AdjustClientRect(var Rect: TRect); override;

    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DestroyWindowHandle; override;

    function GetToolsClient: TDBIDeveloperToolsClient;

  public
    class function Build(AOwner: TComponent; Chromium: TChromium): TDBIDeveloperTools;

    property Chromium: TChromium read FChromium write FChromium;
  end;


type
  TDBIChromiumHelper = class Helper for TCustomChromium
  protected
    function GetHandler: ICefClient;

  public
    property ClientHandler: ICefClient read GetHandler;

  end;


type
  TDBIChromateToolBar = class(TToolBar);
  TDBIChromateToolButtonIndex = (bbBack, bbHome, bbLoad, bbMenu, bbNext, bbReload);

type
  TDBIChromateActionIndex = (
    // Bookmarks
    bmAPI,
    bmArchitecture,
    bmBuilding,
    bmDemo,
    bmDesigner,
    bmExperiment,
    bmFiles,
    bmForum,
    bmGeneral,
    bmJavaScript,
    bmLibrary,
    bmProject,
    bmSource,
    bmTrouble,
    bmTutorial,
    bmWindows,

    // Menu Actions
    aiBack,
    aiClose,
    aiDebugging,
    aiHome,
    aiHookDom,
    aiJavaScript,
    aiLoad,
    aiMenu,
    aiNew,
    aiNext,
    aiPageSource,
    aiPageText,
    aiPrint,
    aiReload,
    aiSave,
    aiToolbar,
    aiZoomIn,
    aiZoomOut,
    aiZoomReset
    );

  TDBIChromateActionRecord = record
    Title: String;
    Shortcut: TShortcut;
    ImageIndex: Integer;
    Data: String;
  end;

  TDBIChromateActionRecords = array[TDBIChromateActionIndex] of TDBIChromateActionRecord;

type
  TDBIChromateBrowserOption = (coDeveloperTools, coToolBar);
  TDBIChromateBrowserOptions = set of TDBIChromateBrowserOption;

type
  TDBIChromateBrowser = class(TComponent)
  private
    FActionItems: array[TDBIChromateActionIndex] of TAction;
    FActionList: TActionList;
    FChromium: TDBIChromate;
    FLoading: Boolean;
    FOptions: TDBIChromateBrowserOptions;
    FParent: TWinControl;
    FPopupMenu: TPopupMenu;
    FSplitter: TSplitter;
    FStatusBar: TDBITooltip;
    FToolBar: TDBIChromateToolBar;
    FToolEdit: TEdit;
    FTools: TDBIDeveloperTools;

    FDemoShowMessage: TAction;
    FToolsFileOpen: TFileOpen;

  protected
    procedure ChromiumAddressChange(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Frame: ICefFrame;
      const Url: UString
      );

    procedure ChromiumBeforeContextMenu(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Frame: ICefFrame;
      const Params: ICefContextMenuParams;
      const Model: ICefMenuModel
      );

    procedure ChromiumBeforeDownload(
      Sender: TObject;
      const Browser: ICefBrowser;
      const DownloadItem: ICefDownloadItem;
      const SuggestedName: UString;
      const Callback: ICefBeforeDownloadCallback
      );

    procedure ChromiumBeforePopup(
      Sender: TObject; const Browser: ICefBrowser;
      const Frame: ICefFrame;
      const TargetUrl: UString;
      const TargetFrameName: UString;
      var PopupFeatures: TCefPopupFeatures;
      var WindowInfo: TCefWindowInfo;
      var Client: ICefClient;
      var Settings: TCefBrowserSettings;
      var NoJavascriptAccess: Boolean;
      out Result: Boolean
      );

    procedure ChromiumBeforeResourceLoad(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Frame: ICefFrame;
      const Request: ICefRequest;
      out Result: Boolean
      );

    procedure ChromiumContextMenuCommand(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Frame: ICefFrame;
      const Params: ICefContextMenuParams;
      CommandId: Integer;
      EventFlags: TCefEventFlags;
      out Result: Boolean
      );

    procedure ChromiumDownloadUpdated(
      Sender: TObject;
      const Browser: ICefBrowser;
      const DownloadItem: ICefDownloadItem;
      const Callback: ICefDownloadItemCallback
      );

    procedure ChromiumKeyEvent(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Event: PCefKeyEvent;
      OSEvent: PMsg;
      out Result: Boolean
      );

    procedure ChromiumLoadEnd(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Frame: ICefFrame;
      HttpStatusCode: Integer
      ); virtual;

    procedure ChromiumLoadStart(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Frame: ICefFrame
      );

    procedure ChromiumProcessMessageReceived(
      Sender: TObject;
      const Browser: ICefBrowser;
      SourceProcess: TCefProcessId;
      const Message: ICefProcessMessage;
      out Result: Boolean
      );

    procedure ChromiumStatusMessage(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Value: UString
      );

    procedure ChromiumTitleChange(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Title: UString
      );


  protected
    function GetAction(
      const ActionName: String;
      const Title: String = '';
      const Shortcut: TShortcut = 0;
      const ImageIndex: Integer = -1;
      OnExecute: TNotifyEvent = nil;
      OnUpdate: TNotifyEvent = nil
      ): TAction;

    function GetActionFileOpen(
      const ActionName: String;
      const Title: String;
      const Shortcut: TShortcut = 0;
      const ImageIndex: Integer = -1;
      OnExecute: TNotifyEvent = nil;
      OnUpdate: TNotifyEvent = nil
      ): TFileOpen;

    function GetActionItem(const Index: TDBIChromateActionIndex): TAction;
    class function GetActionItems: TDBIChromateActionRecords;
    function GetActionList: TActionList;

    function GetActionMethod(
      const Index: TDBIChromateActionIndex;
      const MethodTemplate: String
      ): TNotifyEvent;

  protected
    function GetChromate: TDBIChromate;
    function GetControlsOwner: TComponent;
    function GetMenuOwner: TComponent;
    function GetParent: TWinControl;
    function GetPopupMenu: TPopupMenu;
    function GetPopupMenuBreak: TMenuItem;
    function GetPopupMenuItem(PopupAction: TCustomAction): TMenuItem;
    function GetSplitter: TSplitter;
    function GetStatusBar: TDBITooltip;
    function GetToolBar: TDBIChromateToolBar;
    function GetToolButton(const Index: TDBIChromateToolButtonIndex): TToolButton;
    function GetToolButtonLeft: Integer;
    function GetToolEdit: TEdit;
    function GetTools: TDBIDeveloperTools;
    function GetUrlAddress: String; virtual;
    function GetUrlDefault: String; virtual;

    procedure SendMessageRenderer(const Msg: String);

    procedure SetOptions(const Value: TDBIChromateBrowserOptions);
    procedure SetParent(Value: TWinControl);
    procedure SetStatusMessage(const Value: String);
    function SetupPage: TDBIChromateBrowser;

    property Chromium: TDBIChromate read GetChromate;
    property Splitter: TSplitter read GetSplitter;
    property StatusBar: TDBITooltip read GetStatusBar;
    property ToolButton[const Index: TDBIChromateToolButtonIndex]: TToolButton read GetToolButton;
    property Tools: TDBIDeveloperTools read GetTools;

  published
    procedure ActionAddressKeyPress(Sender: TObject; var Key: Char);
    procedure ActionBackExecute(Sender: TObject);
    procedure ActionBookmarkExecute(Sender: TObject);
    procedure ActionButtonUpdate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionDebuggingExecute(Sender: TObject);
    procedure ActionHomeExecute(Sender: TObject);
    procedure ActionHookDomExecute(Sender: TObject);
    procedure ActionJavaScriptExecute(Sender: TObject);
    procedure ActionLoadExecute(Sender: TObject); virtual;
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionNextExecute(Sender: TObject);
    procedure ActionPageSourceExecute(Sender: TObject);
    procedure ActionPageTextExecute(Sender: TObject);
    procedure ActionMenuExecute(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure ActionReloadUpdate(Sender: TObject);
    procedure ActionResizeExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject); virtual;
    procedure ActionToolbarExecute(Sender: TObject);
    procedure ActionZoomExecute(Sender: TObject);

    procedure DemoShowMessageExecute(Sender: TObject);
    procedure ToolsFileOpenExecute(Sender: TObject);

    property ToolEdit: Tedit read GetToolEdit;
    property ToolsFileOpen: TFileOpen read FToolsFileOpen;

  public
    procedure Console(const Msg: String; Args: array of const);

    procedure Close;
    procedure Home;
    function Page(const Url: String): TDBIChromateBrowser;
    procedure Load; virtual;

    property ActionItem[const Index: TDBIChromateActionIndex]: TAction read GetActionItem;
    property ActionList: TActionList read GetActionList;
    property Options: TDBIChromateBrowserOptions read FOptions write SetOptions;
    property PopupMenu: TPopupMenu read GetPopupMenu;
    property Toolbar: TDBIChromateToolbar read GetToolbar;

  end;
  TDBIChromateBrowserClass = class of TDBIChromateBrowser;

type
  TDBIChromateDesigner = class(TDBIChromateBrowser)
  private
    FLoaded: Boolean;

  protected
    procedure ChromiumLoadEnd(
      Sender: TObject;
      const Browser: ICefBrowser;
      const Frame: ICefFrame;
      HttpStatusCode: Integer
      ); override;

  public
    procedure Load; override;

  published
    procedure ActionLoadExecute(Sender: TObject); override;
    procedure ActionSaveExecute(Sender: TObject); override;

  end;


type
  TDBIChromatePage = class(TTabSheet)
  private
    FBrowser: TDBIChromateBrowser;

  protected
    function GetBrowser: TDBIChromateBrowser;
    function GetBrowserClass: TDBIChromateBrowserClass; virtual;

    procedure DoHide; override;
    procedure DoShow; override;

  public
    class function Build(APageControl: TPageControl): TDBIChromatePage;

    property Browser: TDBIChromateBrowser read GetBrowser;
  end;


type
  TDBIChromateDesignerPage = class(TDBIChromatePage)
  protected
    function GetBrowserClass: TDBIChromateBrowserClass; override;

  end;


implementation

{$define UseFileScheme True}

uses
{$ifdef UseFileScheme}
  CefFileScheme,
{$endif}
  Consts, TypInfo, Graphics, Forms, Dialogs, ToolWin, UITypes, Variants; //##JVR, DBIChromateExtensions;



{ TDBIChromiumHelper }

function TDBIChromiumHelper.GetHandler: ICefClient;
begin
  Result := Self.FHandler;
end;




{ TDBIChromateDesignerPage }

function TDBIChromateDesignerPage.GetBrowserClass: TDBIChromateBrowserClass;
begin
  Result := TDBIChromateDesigner;
end;





{ TDBIChromatePage }

class function TDBIChromatePage.Build(APageControl: TPageControl): TDBIChromatePage;
begin
  Assert(Assigned(APageControl));

  Result := Self.Create(APageControl.Owner);
  Result.PageControl := APageControl;
  Result.Caption := 'New Tab';

  APageControl.ActivePage := Result;
end;


procedure TDBIChromatePage.DoHide;
begin
  Browser.ActionList.State := asSuspended;
end;


procedure TDBIChromatePage.DoShow;
begin
  Browser.ActionList.State := asNormal;
end;


function TDBIChromatePage.GetBrowser: TDBIChromateBrowser;
begin
  if not Assigned(FBrowser) then begin
    FBrowser := GetBrowserClass.Create(Self.Owner);
    FBrowser.SetParent(Self);
    FBrowser.SetupPage;
    FBrowser.Home;
  end;
  Result := FBrowser;
end;


function TDBIChromatePage.GetBrowserClass: TDBIChromateBrowserClass;
begin
  Result := TDBIChromateBrowser;
end;





{ TDBIChromateDesigner }

procedure TDBIChromateDesigner.ActionLoadExecute(Sender: TObject);
begin
//##JVR  Load;
end;


procedure TDBIChromateDesigner.ActionSaveExecute(Sender: TObject);
const
//##JVR  Script = 'alert("Unable to save form");';
  Script =
    'var doc = document.getElementById("designer");' +
    'if (doc) { doc.saveToFile("dummy"); }' +
    'else { alert("Failed to save form"); } ';

begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Browser.MainFrame.ExecuteJavaScript(Script, 'about:blank', 0);
  end;
end;


procedure TDBIChromateDesigner.ChromiumLoadEnd(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Frame: ICefFrame;
  HttpStatusCode: Integer
  );
begin
//  if Chromium.IsMainFrame(Browser, Frame) then begin
//    Sleep(1000);
//    Application.ProcessMessages;
//  end;

  inherited ChromiumLoadEnd(Sender, Browser, Frame, HttpStatusCode);
end;


procedure TDBIChromateDesigner.Load;
const
  Script =
    'var doc = document.getElementById("designer");' +
    'if (doc) { doc.loadFromFile("dummy"); }' +
    'else { alert("Failed to load form"); } ';

begin

  if (Chromium.Browser <> nil) and not FLoaded then begin
    FLoaded := True;
    Chromium.Browser.MainFrame.ExecuteJavaScript(Script, 'about:blank', 0);
  end;

  inherited Load;
end;





{ TDBIChromateBrowser }

procedure TDBIChromateBrowser.ActionAddressKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then begin
    if Chromium.Browser <> nil then begin
      Chromium.Browser.MainFrame.LoadUrl(GetUrlAddress);
      Abort;
    end;
  end;
end;


procedure TDBIChromateBrowser.ActionBackExecute(Sender: TObject);
begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Browser.GoBack;
  end;
end;


procedure TDBIChromateBrowser.ActionBookmarkExecute(Sender: TObject);
const
  Prompt = 'Sorry Sender is not of type "TAction"';

var
  ActionName: String;
  Index: TDBIChromateActionIndex;

begin
  if (Sender is TAction) then begin
    ActionName := TAction(Sender).Name;

    for Index := Low(TDBIChromateActionIndex) to High(TDBIChromateActionIndex) do begin
      if (ActionName = TypInfo.GetEnumName(TypeInfo(TDBIChromateActionIndex), Ord(Index))) then begin
        Page(GetActionItems[Index].Data);

        Break;
      end;
    end;
  end

  else begin
    MessageDlg(Prompt, mtWarning, [mbOK], 0);
  end;
end;


procedure TDBIChromateBrowser.ActionButtonUpdate(Sender: TObject);
var
  Index: TDBIChromateToolButtonIndex;

begin
  if (Sender is TAction) then begin
    Index := TDBIChromateToolButtonIndex(TAction(Sender).ImageIndex);

    ToolButton[Index].Enabled := Chromium.Browser <> nil;
    if ToolButton[Index].Enabled then begin
      case Index of
        bbBack: ToolButton[bbBack].Enabled := Chromium.Browser.CanGoBack;
        bbNext: ToolButton[bbNext].Enabled := Chromium.Browser.CanGoForward;
      else
        ToolButton[bbNext].Enabled := True;
      end;
    end;
  end;
end;


procedure TDBIChromateBrowser.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;


procedure TDBIChromateBrowser.ActionHomeExecute(Sender: TObject);
begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Load(Chromium.DefaultUrl);
  end;
end;


procedure TDBIChromateBrowser.ActionHookDomExecute(Sender: TObject);
begin
  SendMessageRenderer('visitdom');

  Console('%s::visitdom: This is currently NOT working', [Self.ClassName]);
end;


procedure TDBIChromateBrowser.ActionJavaScriptExecute(Sender: TObject);
const
//##JVR  Script = 'alert(''JavaScript execute works!'');';
  Script = 'getInvestor();';

begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Browser.MainFrame.ExecuteJavaScript(Script, 'about:blank', 0);
  end;
end;


procedure TDBIChromateBrowser.ActionLoadExecute(Sender: TObject);
begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Load(GetUrlAddress);
  end;
end;


procedure TDBIChromateBrowser.ActionNewExecute(Sender: TObject);
var
  Browser: TDBIChromateBrowser;

begin
  if (GetParent is TTabSheet) then begin
    Browser :=TDBIChromatePage.Build((GetParent as TTabSheet).PageControl).Browser;

    Browser.ActionList.Images := ActionList.Images;
    Browser.Toolbar.DisabledImages := Toolbar.DisabledImages;
    Browser.Toolbar.Images := Toolbar.Images;
    Browser.Toolbar.Visible := True;
  end;
end;


procedure TDBIChromateBrowser.ActionNextExecute(Sender: TObject);
begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Browser.GoForward;
  end;
end;


procedure TDBIChromateBrowser.ActionPageSourceExecute(Sender: TObject);
begin
  Chromium.Browser.MainFrame.GetSourceProc(
    procedure(const CallbackText: UString)
    var
      Source: UString;

    begin
      Source := CallbackText;
      Source := StringReplace(Source, '<', '&lt;', [rfReplaceAll]);
      Source := StringReplace(Source, '>', '&gt;', [rfReplaceAll]);
      Source := '<html><body>Source:<pre>' + Source + '</pre></body></html>';

      Chromium.Browser.MainFrame.LoadString(Source, '');
    end
    );
end;


procedure TDBIChromateBrowser.ActionPageTextExecute(Sender: TObject);
begin
  Chromium.Browser.MainFrame.GetTextProc(
    procedure(const CallbackText: UString)
    var
      Source: UString;

    begin
      Source := CallbackText;
      Source := StringReplace(Source, '<', '&lt;', [rfReplaceAll]);
      Source := StringReplace(Source, '>', '&gt;', [rfReplaceAll]);
      Source := '<html><body>Text:<pre>' + Source + '</pre></body></html>';

      Chromium.Browser.MainFrame.LoadString(Source, '');
    end
  );
end;


procedure TDBIChromateBrowser.ActionMenuExecute(Sender: TObject);
var
  MenuCoord: TPoint;

begin
  MenuCoord.X := 4;
  MenuCoord.Y := 0;
  if Toolbar.Visible then begin
    MenuCoord.y := Toolbar.Height - MenuCoord.X;
  end;

  MenuCoord := GetParent.ClientToScreen(MenuCoord);
  PopupMenu.Popup(MenuCoord.X, MenuCoord.Y);
end;


procedure TDBIChromateBrowser.ActionPrintExecute(Sender: TObject);
begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Browser.Host.Print;
  end;
end;


procedure TDBIChromateBrowser.ActionReloadExecute(Sender: TObject);
begin
  if (Chromium.Browser <> nil) then begin
    if FLoading then begin
      Chromium.Browser.StopLoad;
    end
    else begin
      Chromium.Browser.Reload;
    end;
  end;
end;


procedure TDBIChromateBrowser.ActionReloadUpdate(Sender: TObject);
begin
  if FLoading then begin
//##JVR    TAction(Sender).Caption := 'X';

  end
  else begin
//##JVR    TAction(Sender).Caption := 'R';

  end;

  ToolButton[bbReload].Enabled := Chromium.Browser <> nil;
end;


procedure TDBIChromateBrowser.ActionResizeExecute(Sender: TObject);
begin
  if Toolbar.Visible then begin
    Windows.LockWindowUpdate(Toolbar.Handle);
    try
      ToolEdit.Visible := False;
      ToolEdit.Width := GetParent.Width - 130;
      ToolEdit.ComponentIndex := 99;
      ToolEdit.Align := alRight;
      ToolEdit.Visible := True;
    finally
      Windows.LockWindowUpdate(0);
    end;

  end;
end;


procedure TDBIChromateBrowser.ActionSaveExecute(Sender: TObject);
const
  Script = 'alert("Save is not implemented for this page!");';

begin
  if (Chromium.Browser <> nil) then begin
    Chromium.Browser.MainFrame.ExecuteJavaScript(Script, 'about:blank', 0);
  end;
end;


procedure TDBIChromateBrowser.ActionToolbarExecute(Sender: TObject);
begin
  if Chromium.Browser <> nil then begin
    Toolbar.Visible := not Toolbar.Visible;
  end;
end;


procedure TDBIChromateBrowser.ActionZoomExecute(Sender: TObject);
var
  ItemIndex: TDBIChromateActionIndex;

begin
  ItemIndex := TDBIChromateActionIndex(
    TypInfo.GetEnumValue(TypeInfo(TDBIChromateActionIndex), (Sender as TAction).Name)
    );

  if Chromium.Browser <> nil then begin
    case ItemIndex of
      aiZoomIn:    Chromium.Browser.Host.ZoomLevel := Chromium.Browser.Host.ZoomLevel + 0.5;
      aiZoomOut:   Chromium.Browser.Host.ZoomLevel := Chromium.Browser.Host.ZoomLevel - 0.5;
      aiZoomReset: Chromium.Browser.Host.ZoomLevel := 0;
    end;
  end;
end;


procedure TDBIChromateBrowser.ChromiumAddressChange(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Frame: ICefFrame;
  const Url: UString
  );
begin
  if Chromium.IsMainFrame(Browser, Frame) then begin
    GetToolEdit.Text := Url;

    Application.ProcessMessages;
  end;
end;


const
  CUSTOMMENUCOMMAND_INSPECTELEMENT = $6E7E05; // = 7241221;

procedure TDBIChromateBrowser.ChromiumBeforeContextMenu(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Frame: ICefFrame;
  const Params: ICefContextMenuParams;
  const Model: ICefMenuModel
  );
begin
  Model.AddItem(CUSTOMMENUCOMMAND_INSPECTELEMENT, 'Inspect Element');
end;


procedure TDBIChromateBrowser.ChromiumBeforeDownload(
  Sender: TObject;
  const Browser: ICefBrowser;
  const DownloadItem: ICefDownloadItem;
  const SuggestedName: UString;
  const Callback: ICefBeforeDownloadCallback
  );
begin
  Callback.Cont(ExtractFilePath(ParamStr(0)) + SuggestedName, True);
end;


procedure TDBIChromateBrowser.ChromiumBeforePopup(
  Sender: TObject; const Browser: ICefBrowser;
  const Frame: ICefFrame;
  const TargetUrl: UString;
  const TargetFrameName: UString;
  var PopupFeatures: TCefPopupFeatures;
  var WindowInfo: TCefWindowInfo;
  var Client: ICefClient;
  var Settings: TCefBrowserSettings;
  var NoJavascriptAccess: Boolean;
  out Result: Boolean
  );
begin
  // prevent popup
  Chromium.Load(TargetUrl);
  Result := True;
end;


procedure TDBIChromateBrowser.ChromiumBeforeResourceLoad(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Frame: ICefFrame;
  const Request: ICefRequest;
  out Result: Boolean
  );
var
  Url: TUrlParts;

begin
  // redirect home to google
  if CefParseUrl(Request.Url, Url) then begin
    if (Url.Host = 'home') then begin
      Url.Host := 'www.google.com';
      Request.Url := CefCreateUrl(Url);
    end;
  end;
end;


procedure TDBIChromateBrowser.ChromiumContextMenuCommand(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Frame: ICefFrame;
  const Params: ICefContextMenuParams;
  CommandId: Integer;
  EventFlags: TCefEventFlags;
  out Result: Boolean
  );
var
  MousePoint: TCefPoint;

begin
  Result := False;
  if (CommandId = CUSTOMMENUCOMMAND_INSPECTELEMENT) then begin
    MousePoint.X := Params.XCoord;
    MousePoint.Y := Params.YCoord;
{##JVR
    Splitter.Visible := True;
    DevTools.Visible := True;
    actDevTool.Checked := True;
    DevTools.CloseDevTools(crm.Browser);
    application.ProcessMessages;
    DevTools.ShowDevTools(crm.Browser,@mousePoint);
//}
    ShowMessage('TDBIChromateBrowser.ChromiumContextMenuCommand("Not implemented Yet!")');
    Result := True;
  end;
end;


procedure TDBIChromateBrowser.ChromiumDownloadUpdated(
  Sender: TObject;
  const Browser: ICefBrowser;
  const DownloadItem: ICefDownloadItem;
  const Callback: ICefDownloadItemCallback
  );
begin
  if DownloadItem.IsInProgress then begin
    SetStatusMessage(IntToStr(DownloadItem.PercentComplete) + '%');
  end
  else begin
    SetStatusMessage('');
  end;
end;


procedure TDBIChromateBrowser.ChromiumKeyEvent(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Event: PCefKeyEvent;
  OSEvent: PMsg;
  out Result: Boolean
  );
begin
  if (Event.Kind = KEYEVENT_RAWKEYDOWN) and (Event.Windows_Key_Code = VK_F5) then begin
    Result := True;
    Chromium.Browser.Reload;
  end;
end;


procedure TDBIChromateBrowser.ChromiumLoadEnd(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Frame: ICefFrame;
  HttpStatusCode: Integer
  );
begin
  if Chromium.IsMainFrame(Browser, Frame) then begin
    ToolButton[bbReload].Enabled := True;

    FLoading := False;
  end
  else if (HttpStatusCode = 0) then begin
    Load;
  end;
end;


procedure TDBIChromateBrowser.ChromiumLoadStart(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Frame: ICefFrame
  );
begin
  ToolButton[bbReload].Enabled := False;

  if Chromium.IsMainFrame(Browser, Frame) then begin
    FLoading := True;
  end;
end;

type
  TDBICustomChromateData = class(TPersistent)
  end;


  TProtectedActionData = class(TObject)
  private
    FMessage: ICefProcessMessage;

  protected
    property Message: ICefProcessMessage read FMessage write FMessage;
  end;


  TProtectedAction = class(TCustomAction)
  protected
    function GetData: TProtectedActionData;
    function GetMessage: ICefProcessMessage;

    procedure SetMessage(Value: ICefProcessMessage);

    property Data: TProtectedActionData read GetData;

  public
    property Message: ICefProcessMessage read GetMessage write SetMessage;
  end;

function TProtectedAction.GetData: TProtectedActionData;
begin
  if not Assigned(FMask) then begin
    FMask := TProtectedActionData.Create;
  end;
  Result := TProtectedActionData(FMask);
end;


function TProtectedAction.GetMessage: ICefProcessMessage;
begin
  Result := Data.Message;
end;


procedure TProtectedAction.SetMessage(Value: ICefProcessMessage);
begin
  Data.Message := Value;
end;



procedure TDBIChromateBrowser.ChromiumProcessMessageReceived(
  Sender: TObject;
  const Browser: ICefBrowser;
  SourceProcess: TCefProcessId;
  const Message: ICefProcessMessage;
  out Result: Boolean
  );
var
  Action: TContainedAction;

begin
  try
    if (Message.Name = 'action') then begin
      Action := Chromium.GetActionByName(Message.ArgumentList.GetString(0));
      if Assigned(Action) then begin
  //##JVR      TProtectedAction(Action).Message := Message;
        Action.Execute;
      end;
    end

    else if (Message.Name = 'mouseover') then begin
      SetStatusMessage(Message.ArgumentList.GetString(0));
      Result := True;
    end
    else begin
      Result := False;
    end;
  except
    on E: Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;


procedure TDBIChromateBrowser.ChromiumStatusMessage(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Value: UString
  );
begin
  SetStatusMessage(Value);
end;


procedure TDBIChromateBrowser.ChromiumTitleChange(
  Sender: TObject;
  const Browser: ICefBrowser;
  const Title: UString
  );
begin
  if Chromium.IsMainFrame(Browser) then begin
    if GetParent is TTabSheet then begin
      TTabSheet(GetParent).Caption := Title;
    end
    else if GetParent is TCustomForm then begin
      TCustomForm(GetParent).Caption := Title;
    end;
  end;
end;


procedure TDBIChromateBrowser.Close;
begin
  if GetParent is TTabSheet then begin
    GetParent.Free;
  end
  else if GetParent is TCustomForm then begin
    TCustomForm(GetParent).Close;
  end;
end;


procedure TDBIChromateBrowser.Console(const Msg: String; Args: array of const);
var
  Script: String;

begin
  if (Chromium.Browser <> nil) then begin
    Script := 'console.log(''' + Format(Msg, Args) + ''');';
    Chromium.Browser.MainFrame.ExecuteJavaScript(Script, 'about:blank', 0);
  end;
end;


procedure TDBIChromateBrowser.ActionDebuggingExecute(Sender: TObject);
begin
  Tools.Visible := not Tools.Visible;
  Splitter.Visible := Tools.Visible;
  Splitter.Top := Chromium.Height;
end;


procedure TDBIChromateBrowser.DemoShowMessageExecute(Sender: TObject);
begin
  ShowMessage('This Message is an action in Delphi called from an Html web page');
end;


function TDBIChromateBrowser.GetAction(
  const ActionName: String;
  const Title: String = '';
  const Shortcut: TShortcut = 0;
  const ImageIndex: Integer = -1;
  OnExecute: TNotifyEvent = nil;
  OnUpdate: TNotifyEvent = nil
  ): TAction;
begin
  Result := TAction.Create(Self);
  Result.Name := ActionName;
  Result.Caption := Title;
  Result.Shortcut := Shortcut;
  Result.ImageIndex := ImageIndex;
  Result.OnExecute := OnExecute;
  Result.OnUpdate := OnUpdate;
  Result.ActionList := FActionList;
end;


function TDBIChromateBrowser.GetActionFileOpen(
  const ActionName: String;
  const Title: String;
  const Shortcut: TShortcut = 0;
  const ImageIndex: Integer = -1;
  OnExecute: TNotifyEvent = nil;
  OnUpdate: TNotifyEvent = nil
  ): TFileOpen;
begin
  Result := TFileOpen.Create(Self);
  Result.Name := ActionName;
  Result.Caption := Title;
  Result.Shortcut := Shortcut;
  Result.ImageIndex := ImageIndex;
  Result.OnAccept := OnExecute;
  Result.OnUpdate := OnUpdate;
  Result.ActionList := FActionList;
end;


function TDBIChromateBrowser.GetActionItem(const Index: TDBIChromateActionIndex): TAction;
begin
  if not Assigned(FActionItems[Index]) then begin
    FActionItems[Index] := TAction.Create(Self);
    FActionItems[Index].ActionList := FActionList;
    FActionItems[Index].Caption := GetActionItems[Index].Title;
    FActionItems[Index].ImageIndex := GetActionItems[Index].ImageIndex;
    FActionItems[Index].Name := TypInfo.GetEnumName(TypeInfo(TDBIChromateActionIndex), Ord(Index));
    FActionItems[Index].Shortcut := GetActionItems[Index].Shortcut;
    FActionItems[Index].OnExecute := GetActionMethod(Index, 'Action%sExecute');
    FActionItems[Index].OnUpdate := nil;
  end;
  Result := FActionItems[Index];
end;


// bmAPI, bmArchitecture, bmBuilding, bmDemo, bmDesigner, bmExperiment, bmFiles, bmForum, bmGeneral, bmJavaScript, bmLibrary, bmProject, bmSource, bmTrouble, bmTutorial,
// aiBack, aiClose, aiDebugging, aiHome, aiHookDom, aiJavascript, aiMenu, aiNew, aiPageSource, aiPageText, aiPrint, aiToolbar, aiZoomIn, aiZoomOut, aiZoomReset
class function TDBIChromateBrowser.GetActionItems: TDBIChromateActionRecords;
const
  CData: TDBIChromateActionRecords = (
    // Bookmarks
    ( Title: 'API Documentation';     Shortcut:         0; ImageIndex: -1; Data: 'http://magpcss.org/ceforum/apidocs3'; ),
    ( Title: 'Architecture';          Shortcut:         0; ImageIndex: -1; Data: 'https://bitbucket.org/chromiumembedded/cef/wiki/Architecture.md'; ),
    ( Title: 'Branches and Building'; Shortcut:         0; ImageIndex: -1; Data: 'https://bitbucket.org/chromiumembedded/cef/wiki/BranchesAndBuilding'; ),
    ( Title: 'Silk Demo Application'; Shortcut:         0; ImageIndex: -1; Data: 'http://localhost:9000/app/'; ),
    ( Title: 'Designer';              Shortcut: {F11} 122; ImageIndex: -1; Data: 'http://localhost:9000/designer/index.html'; ),
    ( Title: 'Trapdoor Embedded Demo';Shortcut: {F6}  117; ImageIndex: -1; Data: 'http://localhost:9000/app/assets/trapdoor/investor/investor.html'; ), //##JVR http://localhost:9000/designer/huntsman.html'; ),
    ( Title: 'File Explorer';         Shortcut:         0; ImageIndex: -1; Data: 'local://c/'; ),
    ( Title: 'Google Groups';         Shortcut:         0; ImageIndex: -1; Data: 'https://groups.google.com/forum/?fromgroups#!forum/delphichromiumembedded'; ),
    ( Title: 'General Usage';         Shortcut:         0; ImageIndex: -1; Data: 'https://bitbucket.org/chromiumembedded/cef/wiki/GeneralUsage'; ),
    ( Title: 'Javascript Integration';Shortcut:         0; ImageIndex: -1; Data: 'https://bitbucket.org/chromiumembedded/cef/wiki/JavaScriptIntegration.md'; ),
    ( Title: 'Chromium Libraries';    Shortcut:         0; ImageIndex: -1; Data: 'https://code.google.com/p/chromium/issues/detail?id=321626'; ),
    ( Title: 'Project Page';          Shortcut:         0; ImageIndex: -1; Data: 'https://bitbucket.org/chromiumembedded/cef'; ),
    ( Title: 'Chromium Source Code';  Shortcut:         0; ImageIndex: -1; Data: 'http://www.chromium.org/developers/how-tos/getting-around-the-chrome-source-code'; ),
    ( Title: 'Troublesome DLLS';      Shortcut:         0; ImageIndex: -1; Data: 'https://code.google.com/p/chromium/codesearch#chromium/src/content/common/sandbox_win.cc&q=ktroublesome&sq=package%3achromium&type=cs&l=43'; ),
    ( Title: 'Tutorial';              Shortcut:         0; ImageIndex: -1; Data: 'https://bitbucket.org/chromiumembedded/cef/wiki/Tutorial'; ),
    ( Title: 'Chrome Windows Build';  Shortcut:         0; ImageIndex: -1; Data: 'https://chromium.googlesource.com/chromium/reference_builds/chrome_win/+/master'; ),

    // Menu Actions
    ( Title: 'Back';                  Shortcut:                0; ImageIndex: Ord(bbBack);   Data: ''; ),
    ( Title: 'Close';                 Shortcut: {Ctrl+F4}  16499; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Developer Tools';       Shortcut: {F12}        123; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Home';                  Shortcut:                0; ImageIndex: Ord(bbHome);   Data: ''; ),
    ( Title: 'Hook Dom';              Shortcut:                0; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Execute JavaScript';    Shortcut: {Ctrl+J}   16458; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Load';                  Shortcut:                0; ImageIndex: Ord(bbLoad);   Data: ''; ),
    ( Title: 'Menu';                  Shortcut: {F10}        121; ImageIndex: Ord(bbMenu);   Data: ''; ),
    ( Title: 'New Tab';               Shortcut: {Ctrl+T}   16468; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Next';                  Shortcut:                0; ImageIndex: Ord(bbNext);   Data: ''; ),
    ( Title: 'Page Source';           Shortcut: {Shift+F11} 8314; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Page Text';             Shortcut:                0; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Print...';              Shortcut: {Ctrl+P}   16464; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Re-load';               Shortcut:                0; ImageIndex: Ord(bbReload); Data: ''; ),
    ( Title: 'Save';                  Shortcut: {Ctrl+S}   16467; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Toolbar';               Shortcut: {Shift+F10} 8313; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Zoom &In';              Shortcut: {Ctrl++}{=}16571; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Zoom &Out';             Shortcut: {Ctrl+-}   16573; ImageIndex: -1;            Data: ''; ),
    ( Title: 'Zoom &Reset';           Shortcut: {Ctrl+0}   16432; ImageIndex: -1;            Data: ''; )

    );
begin
  Result := CData;
end;


function TDBIChromateBrowser.GetActionList: TActionList;
begin
  if not Assigned(FActionList) then begin
    FActionList := TActionList.Create(GetMenuOwner);

    FDemoShowMessage := GetAction('DemoShowMessage', 'Demo Message', 0, -1, DemoShowMessageExecute, nil);
    FToolsFileOpen := GetActionFileOpen('ToolsFileOpen', 'Open...', 0, -1, ToolsFileOpenExecute, nil);
  end;
  Result := FActionList;
end;


function TDBIChromateBrowser.GetActionMethod(
  const Index: TDBIChromateActionIndex;
  const MethodTemplate: String
  ): TNotifyEvent;
var
  Method: TMethod;
  MethodName: String;

begin
  case Index of
    bmAPI..bmWindows:      MethodName := 'Bookmark';
    aiZoomIn..aiZoomReset: MethodName := 'Zoom';
  else
    MethodName := TypInfo.GetEnumName(TypeInfo(TDBIChromateActionIndex), Ord(Index));
    MethodName := Copy(MethodName, 3, 128);
  end;

  Method.Code := MethodAddress(Format(MethodTemplate, [MethodName]));
  if not Assigned(Method.Code) then begin
    raise Exception.CreateFmt('Published method "%s" not found', [MethodName]);
  end;

  Method.Data := Self;
  if not Assigned(Method.Data) then begin
    raise Exception.CreateFmt('Object of type "%s" not instantiated', [ClassName]);
  end;

  Result := TNotifyEvent(Method);
end;


function TDBIChromateBrowser.GetChromate: TDBIChromate;
begin
  if not Assigned(FChromium) then begin
    FChromium := TDBIChromate.Build(Self, GetParent);
    FChromium.Actions := GetActionList;

    FChromium.OnAddressChange := ChromiumAddressChange;
    FChromium.OnBeforeContextMenu := ChromiumBeforeContextMenu;
    FChromium.OnBeforeDownload := ChromiumBeforeDownload;
    FChromium.OnBeforePopup := ChromiumBeforePopup;
    FChromium.OnBeforeResourceLoad := ChromiumBeforeResourceLoad;
    FChromium.OnContextMenuCommand := ChromiumContextMenuCommand;
    FChromium.OnDownloadUpdated := ChromiumDownloadUpdated;
    FChromium.OnKeyEvent := ChromiumKeyEvent;
    FChromium.OnLoadEnd := ChromiumLoadEnd;
    FChromium.OnLoadStart := ChromiumLoadStart;
    FChromium.OnProcessMessageReceived := ChromiumProcessMessageReceived;
    FChromium.OnStatusMessage := ChromiumStatusMessage;
    FChromium.OnTitleChange := ChromiumTitleChange;
  end;
  Result := FChromium;
end;


function TDBIChromateBrowser.GetControlsOwner: TComponent;
begin
  Result := FParent; //##JVR Self; //##JVR GetParent.Owner;
end;


function TDBIChromateBrowser.GetMenuOwner: TComponent;
begin
  Result := FParent;
end;


function TDBIChromateBrowser.GetParent: TWinControl;
begin
  Result := FParent;
end;


function TDBIChromateBrowser.GetPopupMenu: TPopupMenu;
begin
  if not Assigned(FPopupMenu) then begin
    FPopupMenu := TPopupMenu.Create(GetMenuOwner);
    FPopupMenu.Images := Toolbar.Images;

    GetPopupMenuItem(ActionItem[aiNew]);
    GetPopupMenuItem(ToolsFileOpen);
    GetPopupMenuBreak;
    GetPopupMenuItem(ActionItem[aiSave]);
    GetPopupMenuItem(ActionItem[aiClose]);
    GetPopupMenuBreak;
    GetPopupMenuItem(ActionItem[aiLoad]);
    GetPopupMenuItem(ActionItem[aiPrint]);
    GetPopupMenuBreak;

    GetPopupMenuItem(ActionItem[aiDebugging]);
    GetPopupMenuItem(ActionItem[aiToolbar]);
    GetPopupMenuBreak;

    GetPopupMenuItem(ActionItem[aiZoomIn]);
    GetPopupMenuItem(ActionItem[aiZoomOut]);
    GetPopupMenuItem(ActionItem[aiZoomReset]);
    GetPopupMenuBreak;

    GetPopupMenuItem(ActionItem[aiJavascript]);
    GetPopupMenuItem(ActionItem[aiPageSource]);
    GetPopupMenuItem(ActionItem[aiPageText]);
    GetPopupMenuItem(ActionItem[aiHookDom]);
    GetPopupMenuItem(ActionItem[bmFiles]);
    GetPopupMenuBreak;

    GetPopupMenuItem(ActionItem[bmDemo]);
    GetPopupMenuItem(ActionItem[bmDesigner]);
    GetPopupMenuItem(ActionItem[bmExperiment]);
    GetPopupMenuBreak;

    GetPopupMenuItem(ActionItem[bmProject]);
    GetPopupMenuItem(ActionItem[bmTutorial]);
    GetPopupMenuItem(ActionItem[bmBuilding]);
    GetPopupMenuItem(ActionItem[bmGeneral]);
    GetPopupMenuItem(ActionItem[bmJavascript]);
    GetPopupMenuItem(ActionItem[bmArchitecture]);
    GetPopupMenuBreak;
    GetPopupMenuItem(ActionItem[bmAPI]);
    GetPopupMenuItem(ActionItem[bmSource]);
    GetPopupMenuItem(ActionItem[bmLibrary]);
    GetPopupMenuItem(ActionItem[bmWindows]);
    GetPopupMenuItem(ActionItem[bmTrouble]);
    GetPopupMenuItem(ActionItem[bmForum]);
  end;
  Result := FPopupMenu;
end;


function TDBIChromateBrowser.GetPopupMenuBreak: TMenuItem;
begin
  Result := TMenuItem.Create(GetMenuOwner);
  Result.Caption := '-';

  FPopupMenu.Items.Add(Result);
end;


function TDBIChromateBrowser.GetPopupMenuItem(PopupAction: TCustomAction): TMenuItem;
begin
  Result := TMenuItem.Create(GetMenuOwner);
  Result.Action := PopupAction;

  FPopupMenu.Items.Add(Result);
end;


function TDBIChromateBrowser.GetSplitter: TSplitter;
begin
  if not Assigned(FSplitter) then begin
    FSplitter := TSplitter.Create(GetControlsOwner);
    FSplitter.Visible := False;
    FSplitter.Parent := GetParent;
    FSplitter.Align := alBottom;
    FSplitter.Color := clGray;
    FSplitter.Cursor := crSizeNS;
    FSplitter.Height := 5;
  end;
  Result := FSplitter;
end;


function TDBIChromateBrowser.GetStatusBar: TDBITooltip;
begin
  if not Assigned(FStatusBar) then begin
    FStatusBar := TDBITooltip.Create(GetControlsOwner);
    FStatusBar.AutoHint := False;
    FStatusBar.Font.Color := clWindowText;
    FStatusBar.Font.Size := 10;
    FStatusBar.HintControl := GetParent;
    FStatusBar.Left := 0;
    FStatusBar.Top := GetParent.Height;
  end;
  Result := FStatusBar;
end;


function TDBIChromateBrowser.GetToolBar: TDBIChromateToolBar;
begin
  if not Assigned(FToolBar) then begin
    FToolBar := TDBIChromateToolBar.Create(GetControlsOwner);
    FToolBar.Parent := GetParent;
    FToolBar.Align := alTop;
    FToolBar.BorderWidth := 1;
    FToolBar.Color := clBtnface;
    FToolBar.EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
    FToolBar.EdgeInner := esRaised;
    FToolBar.EdgeOuter := esLowered;
    FToolbar.Height := 30;
    FToolBar.ParentColor := False;
    FToolBar.TabOrder := 0;
    FToolBar.Transparent := False;
    FToolBar.OnResize := ActionResizeExecute;
    FToolbar.Visible := coToolBar in Options;
  end;
  Result := FToolBar;
end;


function TDBIChromateBrowser.GetToolButton(const Index: TDBIChromateToolButtonIndex): TToolButton;
var
  ButtonIndex: Integer;
  Bar: TDBIChromateToolBar;
  Value: TToolButton;

begin
  Value := nil;
  Bar := GetToolbar;
  for ButtonIndex := 0 to Bar.ButtonCount-1 do begin
    if (Ord(Index) = Bar.Buttons[ButtonIndex].ImageIndex) then begin
      Value := Bar.Buttons[ButtonIndex];
      Break;
    end;
  end;

  if not Assigned(Value) then begin
    Value := TToolButton.Create(GetControlsOwner);
    Value.Style := tbsButton;
    Value.ImageIndex := Ord(Index);
    Value.Left := GetToolButtonLeft;

    Value.Parent := Bar;
  end;
  Result := Value;
end;


function TDBIChromateBrowser.GetToolButtonLeft: Integer;
var
  Bar: TDBIChromateToolBar;
  ButtonIndex: Integer;

begin
  Result := 0;
  Bar := GetToolBar;

  for ButtonIndex := 0 to Bar.ButtonCount-1 do begin
    Result := Result + Bar.Buttons[ButtonIndex].Left + Bar.Buttons[ButtonIndex].Width;
  end;
end;


function TDBIChromateBrowser.GetToolEdit: TEdit;
begin
  if not Assigned(FToolEdit) then begin
    FToolEdit := TEdit.Create(GetControlsOwner);
    FToolEdit.Align := alRight;
    FToolEdit.BevelInner := bvSpace;
    FToolEdit.BevelKind := bkFlat;
    FToolEdit.BevelOuter := bvLowered;
    FToolEdit.BorderStyle := bsNone;
    FToolEdit.Font.Size := 10;
    FToolEdit.Left := GetToolButtonLeft;
    FToolEdit.TabOrder := 0;
    FToolEdit.Width := 800;
    FToolEdit.OnKeyPress := ActionAddressKeyPress;

    FToolEdit.Parent := GetToolBar;

    ActionResizeExecute(Self);
  end;
  Result := FToolEdit;
end;


function TDBIChromateBrowser.GetTools: TDBIDeveloperTools;
begin
  if not Assigned(FTools) then begin
    FTools := TDBIDeveloperTools.Create(GetControlsOwner);
    FTools.Visible := False;
    FTools.Chromium := Chromium;
    FTools.Parent := GetParent;
    FTools.Height := 300;
    FTools.Align := alBottom;

    GetSplitter;
  end;
  Result := FTools;
end;


function TDBIChromateBrowser.GetUrlAddress: String;
begin
  Result := GetToolEdit.Text;
end;


function TDBIChromateBrowser.GetUrlDefault: String;
var
  Index: Integer;

begin
  Result := '';

  for Index := 1 to ParamCount do begin
    Result := Result + ParamStr(Index);
  end;

  if (Result = '') then begin
    Result := GetUrlAddress;
  end;
end;


procedure TDBIChromateBrowser.Home;
begin
  if (ParamCount > 0) then begin
    Chromium.Load(GetUrlDefault);
  end;
end;


procedure TDBIChromateBrowser.Load;
begin
  //##NOP
end;


function TDBIChromateBrowser.Page(const Url: String): TDBIChromateBrowser;
begin
//##JVR  Result := nil;

//##JVR  if (GetParent is TTabSheet) then begin
    Result := Self;

  if (GetParent is TTabSheet) then begin
    if (Result.GetUrlAddress <> '') then begin
      Result := TDBIChromatePage.Build((GetParent as TTabSheet).PageControl).Browser;
      Result.ActionList.Images := ActionList.Images;
      Result.Toolbar.DisabledImages := Toolbar.DisabledImages;
      Result.Toolbar.Images := Toolbar.Images;
//##JVR      Result.Toolbar.Visible := True;
    end;
  end;

    Result.Toolbar.Visible := True;
    Result.Chromium.Load(Url);
//##JVR  end;
end;


procedure TDBIChromateBrowser.SendMessageRenderer(const Msg: String);
{$ifdef UseEventListeners}
begin
  Chromium.Browser.SendProcessMessage(PID_RENDERER, TCefProcessMessageRef.New(Msg));
end;
{$else}
const
  Prompt = 'To enable EventListeners set the conditional directive {$define UseEventListeners}';
begin
  MessageDlg(Prompt, mtWarning, [mbOK], 0);
end;
{$endif}


procedure TDBIChromateBrowser.SetOptions(const Value: TDBIChromateBrowserOptions);
begin
  FOptions := Value;
end;


procedure TDBIChromateBrowser.SetParent(Value: TWinControl);
begin
  FParent := Value;
end;


procedure TDBIChromateBrowser.SetStatusMessage(const Value: String);
begin
  StatusBar.Caption := Value;

  StatusBar.Visible := Value <> '';
end;


function TDBIChromateBrowser.SetupPage: TDBIChromateBrowser;
begin
  GetActionList;
  ToolBar.Visible := True;

  // Activate Non-Menu Function-Key Buttons
  ActionItem[aiMenu];  {F10}

  ToolButton[bbMenu].DropDownMenu := PopupMenu;
  ToolButton[bbMenu].Enabled := True;
  ToolButton[bbMenu].EnableDropdown := True;

  ToolButton[bbBack].Action := ActionItem[aiBack];
  ToolButton[bbBack].Enabled := False;

  ToolButton[bbNext].Action := ActionItem[aiNext];
  ToolButton[bbNext].Enabled := False;

  ToolButton[bbReload].Action := ActionItem[aiReload];
  ToolButton[bbHome].Action := ActionItem[aiHome];

  GetToolEdit;

  Result := Self;
end;


procedure TDBIChromateBrowser.ToolsFileOpenExecute(Sender: TObject);
const
  Script = 'document.querySelector("#designer").loadFromFile(''activities.demo\\first'');';

begin
//##JVR  ShowMessage('This is a test - Select any file from the file open dialog'#13'Click Open'#13'The designer will open the "first.scala" form!');
  if (Chromium.Browser <> nil) then begin
    Chromium.Browser.MainFrame.ExecuteJavaScript(Script, 'about:blank', 0);
  end;
end;





{ TDBIChromate }

class function TDBIChromate.Build(AOwner: TComponent; AParent: TWinControl): TDBIChromate;
begin
  Result := Self.Create(AOwner);
  Result.Parent := AParent;
  Result.Align := alClient;
end;


class function TDBIChromate.Register: Boolean;
begin
  CefCache := 'cache';
  CefOnRegisterCustomSchemes := RegisterSchemes;
  CefSingleProcess := False;

  Result := CefLoadLibDefault;
  if not Result then begin
    Exit;
  end;

{$ifdef UseFileScheme}
  CefRegisterSchemeHandlerFactory('local', '', TFileScheme);
{$endif}
end;





{ TDBIChromate }

procedure TDBICustomChromate.CheckStability;
const
  MsgStability = 'DEBUG: Check Stability Exception Raised from %s';
var
  Address: Pointer;

begin
  asm
    mov eax, [ebp + 4] // get return address
    mov Address, eax
  end;

  raise Exception.CreateFmt(MsgStability, [ClassName]) at Address;
end;


constructor TDBICustomChromate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if (GetDefaultUrl <> '') then begin
    inherited DefaultUrl := GetDefaultUrl;
  end;
end;


destructor TDBICustomChromate.Destroy;
begin
  //##DEBUG;

  inherited Destroy;
end;


type
  PAction = ^TAction;

procedure TDBICustomChromate.doOnAddressChange(
  const Browser: ICefBrowser;
  const Frame: ICefFrame;
  const Url: UString
  );

  function GetActionName(const url: String; var ActionName: String): Boolean;
  var
    Offset: Integer;

  begin
    Offset := Pos('##', url);
    Result := Offset > 0;
    if result then begin
      ActionName := Copy(url, Offset+2, 128);
    end;
  end;

var
  ActionName: String;

begin
  try
    if Assigned(OnAddressChange) then begin
      OnAddressChange(Self, Browser, Frame, Url);
    end;

    if IsMainFrame(Browser, Frame) then begin
      if GetActionName(url, ActionName) then begin
        GetActionByName(ActionName).Execute;
      end;
    end;
  except
    on E: Exception do
      Application.HandleException(Self);
  end;
end;


{$ifdef UserDefinedDialogs}
{**
  TCefJsDialogType = (JSDIALOGTYPE_ALERT = 0,  JSDIALOGTYPE_CONFIRM, JSDIALOGTYPE_PROMPT);
}
function TDBICustomChromate.doOnJsdialog(
  const Browser: ICefBrowser;
  const OriginUrl: UString;
  const AcceptLang: UString;
  DialogType: TCefJsDialogType;
  const MessageText: UString;
  const DefaultPromptText: UString;
  Callback: ICefJsDialogCallback;
  out SuppressMessage: Boolean
  ): Boolean;
var
  Prompt: String;

begin
  try
    Result := False;
    if Assigned(OnJsdialog) then begin
      OnJsdialog(
        Self,
        Browser,
        OriginUrl,
        AcceptLang,
        DialogType,
        MessageText,
        DefaultPromptText,
        Callback,
        SuppressMessage,
        Result
        );

      Exit;
    end;

    // Custom Dialog
    SuppressMessage := False;

    if Assigned(Browser) then begin
      Prompt := DefaultPromptText;
      Result := True;

      case DialogType of
        JSDIALOGTYPE_ALERT:   Result := MessageDlg(MessageText, mtCustom, [mbOK], 0) = mrOk;
        JSDIALOGTYPE_CONFIRM: Result := MessageDlg(MessageText, mtConfirmation, mbOKCancel, 0) = mrOK;
        JSDIALOGTYPE_PROMPT:  Result := InputQuery(Application.Title, MessageText, Prompt);

      else
        Result := False;
        MessageDlg(MessageText, mtWarning, [mbOK], 0);
  //##JVR      Console.Add(Format('%s[%s]: %s, %s', [OriginUrl, AcceptLang, MessageText, DefaultPromptText]));
      end;

      // Update Result if changed
      if Result then begin
        Callback.Cont(Result, Prompt);
      end
      else begin
        SuppressMessage := True;
      end;
    end;
  except
    on E: Exception do
      Application.HandleException(Self);
  end;
end;
{$endif}

function TDBICustomChromate.GetActionByName(const ActionName: String): TAction;
var
  Index: Integer;
  PBrowserAction: PAction;

begin
  Result := nil;

  // Try the assigned actionlist first
  if Assigned(FActions) then begin
    for Index := 0 to Actions.ActionCount-1 do begin
      if CompareText(Actions[Index].Name, ActionName) = 0 then begin
        Result := Actions[Index] as TAction;
        Break;
      end;
    end;
  end;

  // Otherwise try the form
  if not Assigned(Result) then begin
    if not (Owner is TCustomForm) then begin
      raise Exception.CreateFmt('Owner of %s::%s is not a Form', [Owner.ClassName, Owner.Name]);
    end;

    PBrowserAction := Owner.FieldAddress(ActionName);
    if not Assigned(PBrowserAction) then begin
      raise Exception.CreateFmt('Action "%s" not found', [ActionName]);
    end;

    Result := PBrowserAction^;
  end;
end;


function TDBICustomChromate.GetDefaultUrl: String;
var
  Index: Integer;

begin
  Result := '';

  for Index := 1 to ParamCount do begin
    Result := Result + ParamStr(Index);
  end;

  if (Result = '') then begin
    Result := inherited DefaultUrl;
  end;
end;


function TDBICustomChromate.IsMainFrame(const Browser: ICefBrowser; const Frame: ICefFrame): Boolean;
begin
  Result := (Browser <> nil) and (Browser.Identifier = BrowserId) and ((Frame = nil) or (Frame.IsMain));
end;


procedure TDBICustomChromate.LoadFromFile(const AFileName: TFileName);
begin
  if FileExists(AFileName) then begin
    Load('file:///' + AFileName);
  end;
end;


class procedure TDBICustomChromate.RegisterSchemes(const Registrar: ICefSchemeRegistrar);
begin
  Registrar.AddCustomScheme('local', True, True, False);
end;


procedure TDBICustomChromate.SetDefaultUrl(const Value: String);
begin
  inherited DefaultUrl := Value;
end;





{ TDBIDeveloperTools }

procedure TDBIDeveloperTools.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);

  Windows.SetWindowPos(GetToolsClient.Handle, 0, Rect.Left, Rect.Top, Rect.Width, Rect.Height, SWP_NOZORDER);
end;


class function TDBIDeveloperTools.Build(AOwner: TComponent; Chromium: TChromium): TDBIDeveloperTools;
begin
  Result := Self.Create(AOwner);
  Result.Chromium := Chromium;
end;


procedure TDBIDeveloperTools.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);

  GetToolsClient.CreateWindowHandle;
end;


procedure TDBIDeveloperTools.DestroyWindowHandle;
begin
  if Assigned(FToolsClient) then begin
    if Assigned(FToolsClient.Parent) then begin
      Windows.SetParent(FToolsClient.Handle, 0);
    end;

    Chromium.Browser.Host.CloseDevTools;
    FToolsClient.WindowHandle := 0;

//##JVR    FToolsClient.DestroyWindowHandle;
    FToolsClient.Free;
    FToolsClient := nil;
  end;

  inherited DestroyWindowHandle;
end;


function TDBIDeveloperTools.GetToolsClient: TDBIDeveloperToolsClient;
begin
  if not Assigned(FToolsClient) then begin
    FToolsClient := TDBIDeveloperToolsClient.Create(Chromium);
    FToolsClient.Parent := Self;
  end;
  Result := FToolsClient;
end;


procedure TDBIDeveloperTools.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of Word = (
    SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW,
    SWP_NOSIZE + SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_SHOWWINDOW);
begin
  inherited;

  SetWindowPos(GetToolsClient.Handle, 0, 0, 0, 0, 0, ShowFlags[Showing]);
end;


procedure TDBIDeveloperTools.WMDestroy(var Message: TWMDestroy);
begin
  inherited;

  DestroyWindowHandle;
end;


procedure TDBIDeveloperTools.WMWindowPosChanged(var Message: TWMWindowPosChanged);
const
  ShowFlags: array[Boolean] of Word = (
    {SWP_NOSIZE + }SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_HIDEWINDOW,
    {SWP_NOSIZE + }SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_SHOWWINDOW);
begin
  inherited;

  SetWindowPos(GetToolsClient.Handle, 0, 0, 0, Width, Height, ShowFlags[Showing]);
end;





{ TDBIDeveloperToolsClient }

const
  ToolsWindowName = 'DevTools';

procedure TDBIDeveloperToolsClient.CreateWindowHandle;
var
  WindowInfo: TCefWindowInfo;
  Setting: TCefBrowserSettings;
  InspectElementAt: PCefPoint;

begin
  InspectElementAt := nil;
  CreateWindowInfo(WindowInfo);

  FillChar(Setting, SizeOf(Setting), 0);
  Setting.Size := SizeOf(Setting);

  Chromium.Browser.Host.ShowDevTools(@WindowInfo, Chromium.ClientHandler, @Setting, InspectElementAt);
end;


procedure TDBIDeveloperToolsClient.CreateWindowInfo(var WindowInfo: TCefWindowInfo);
const
  Styles: array[Boolean] of LongWord = (WS_OVERLAPPEDWINDOW, WS_CHILD);

begin
  FillChar(WindowInfo, SizeOf(WindowInfo), 0);

  WindowInfo.Style :=  WS_CLIPCHILDREN or WS_CLIPSIBLINGS or Styles[Parent <> nil];
  WindowInfo.Window_Name := CefString(ToolsWindowName);
  WindowInfo.Parent_Window := GetParentHandle;
  WindowInfo.X := Integer(CW_USEDEFAULT);
  WindowInfo.Y := Integer(CW_USEDEFAULT);

  if Assigned(Parent) then begin
    WindowInfo.Width := Parent.Width;
    WindowInfo.Height := Parent.Height;
  end
  else begin
    WindowInfo.Width := Integer(CW_USEDEFAULT);
    WindowInfo.Height := Integer(CW_USEDEFAULT);
  end;
end;


procedure TDBIDeveloperToolsClient.DestroyWindowHandle;
begin
  if Assigned(Parent) then begin
    Windows.SetParent(Handle, 0);
  end;

  Chromium.Browser.Host.CloseDevTools;
  WindowHandle := 0;
end;


function TDBIDeveloperToolsClient.GetChromium: TChromium;
begin
  Result := Owner as TChromium;
end;


function TDBIDeveloperToolsClient.GetHandle: HWND;
begin
  if (WindowHandle = 0) or not Windows.IsWindow(WindowHandle) then begin
    WindowHandle := GetWindowHandle;
//##JVR    Windows.SetWindowText(WindowHandle, 'John Vander Reest');
  end;
  Result := WindowHandle;
end;


function TDBIDeveloperToolsClient.GetParentHandle: HWND;
begin
  if Assigned(Parent) then begin
    Result := Parent.Handle;
  end
  else begin
    Result := Chromium.Browser.Host.WindowHandle;
  end;
end;


function TDBIDeveloperToolsClient.GetVisible: Boolean;
begin
  Result := Windows.IsWindow(GetWindowHandle);
//##JVR    Windows.IsWindow(WindowHandle) and Windows.IsWindowVisible(WindowHandle);
end;


function TDBIDeveloperToolsClient.GetWindowHandle: HWND;
begin
  if Assigned(Parent) then begin
    Result := Windows.FindWindowEx(Parent.Handle, 0, nil, ToolsWindowName);
  end
  else begin
    Result := Windows.FindWindowEx(0, 0, nil, ToolsWindowName);
  end;
end;


procedure TDBIDeveloperToolsClient.SetVisible(const Value: Boolean);
begin
  if (Value = GetVisible) then begin
    Exit;
  end;

  if Value then begin
    CreateWindowHandle;
  end
  else begin
    DestroyWindowHandle;
  end;
end;



end.
