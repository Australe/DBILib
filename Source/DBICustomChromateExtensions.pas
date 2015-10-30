{#omcodecop off : jvr : google api and javascript compatibility }

{. $define UseEventListeners}

{ : Please Note:
  :
  : Numerous methods and properties in this unit do NOT comply with the Delphi Code Cop
  :
  : This is intentional!
  : The methods and properties are defined with javascript usage in mind and adhere to
  : the javascript coding conventions
}

unit DBICustomChromateExtensions;

interface

uses
  Windows, Messages, SysUtils, Classes, Variants, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  Actions, ActnList, cefvcl, ceflib;

type
  TDBICustomChromateExtensionHandler = class(TCefRTTIExtension)
  protected
    function Execute(
      const name: ustring;
      const obj: ICefv8Value;
      const arguments: TCefv8ValueArray;
      var retval: ICefv8Value;
      var exception: ustring
      ): Boolean; override;

  end;

type
  TDBICustomChromateExtension = class;

  TDBICustomChromateExtensions = class(TStringList)
  protected
    function GetExtension(const Index: String): TDBICustomChromateExtension;

    class procedure Initialize;
    class procedure RegisterExtensions;
    class procedure Release;

  public
    class function Instance: TDBICustomChromateExtensions;

    property Extensions[const Index: String]: TDBICustomChromateExtension read GetExtension; default;

  end;


  TDBICustomChromateExtension = class(TPersistent)
  private
    FName: String;
    FOwner: TObject;

  protected
    class function Build(AOwner: TObject; const AName: String): TDBICustomChromateExtension; virtual;
    procedure Error(const msg: String; Args: array of const); virtual;

    function GetBrowser: ICefBrowser;

    function getname: Variant; virtual;
    procedure setname(value: Variant); virtual;

    function GetExtensionOwner: TObject; virtual;
    procedure SetExtensionOwner(Value: TObject); virtual;

    function GetNameStr: String; virtual;

    property NameStr: String read GetNameStr;

  public
    class procedure Publish(ProcessMessage: ICefProcessMessage);
    class procedure Register(const NameSpace: String);

    property Browser: ICefBrowser read GetBrowser;
    property Owner: TObject read GetExtensionOwner write SetExtensionOwner;

  published
    property name: Variant read getname write setname;

  end;


type
  TDBICustomChromateExtensionItems = class(TStringList)
  private
    FItemIndex: Integer;

  protected
    property ItemIndex: Integer read FItemIndex write FItemIndex;

  end;


type
  TDBICustomChromateExtensionIterator = class(TDBICustomChromateExtension)
  private
    FItems: TDBICustomChromateExtensionItems;

  protected
    function GetItems: TDBICustomChromateExtensionItems;
    function GetNameStr: String; override;
    procedure SetExtensionOwner(Value: TObject); override;

    property Items: TDBICustomChromateExtensionItems read GetItems;

  public
    destructor Destroy; override;

  published
    procedure first;
    function getcount: Variant;
    function getname: Variant; override;
    function getrecno: Variant;
    procedure last;
    procedure new(aname: Variant); virtual;
    procedure next;
    procedure prior;
    procedure setname(value: Variant); override;
    procedure setrecno(value: Variant);

    property count: Variant read getcount;
    property recno: Variant read getrecno write setrecno;
  end;


type
  TDBICustomChromateRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
{$ifdef UseEventListeners}
  private
    FBrowser: ICefBrowser;

  protected
    function GetPath(const Node: ICefDomNode): string;
    procedure DomVisitor(const Document: ICefDomDocument);
    procedure Listener(const Event: ICefDomEvent);

    function OnProcessMessageReceived(
      const browser: ICefBrowser;
      sourceProcess: TCefProcessId;
      const message: ICefProcessMessage
      ): Boolean; override;
{$endif}
    procedure OnWebKitInitialized; override;

  end;


function ChromateExtensions: TDBICustomChromateExtensions;


implementation

uses
  UITYpes, Dialogs;


function ChromateExtensions: TDBICustomChromateExtensions;
begin
  Result := TDBICustomChromateExtensions.Instance;
end;





//(*##JVR

// TMyHandler's Execute will be invoked later. TMyHandler is defined as
type
  TMyHandler = class(TCefv8HandlerOwn)
  protected
    function Execute(
      const name: ustring;
      const obj: ICefv8Value;
      const arguments: TCefv8ValueArray;
      var retval: ICefv8Value;
      var exception: ustring
      ): Boolean; override;

  end;

function TMyHandler.Execute(
  const name: ustring;
  const obj: ICefv8Value;
  const arguments: TCefv8ValueArray;
  var retval: ICefv8Value;
  var exception: ustring
  ): Boolean;
begin
  Result := True;

  ShowMessage('Execute!');
end;



procedure RegisterExtension;
var
  Code: String;

begin
  Code :=

   // Temporary
   'var cef;'+
   'if (!cef)'+
   '  cef = {};'+
   'if (!cef.test)'+
   '  cef.test = {};'+

   '(function() {'+
   '  cef.test.__defineGetter__(''test_param'', function() {'+
   '    native function GetTestParam();'+
   '    return GetTestParam();'+
   '  });'+
   '  cef.test.__defineSetter__(''test_param'', function(b) {'+
   '    native function SetTestParam();'+
   '    if(b) SetTestParam(b);'+
   '  });'+
   '  cef.test.test_object = function() {'+
   '    native function GetTestObject();'+
   '    return GetTestObject();'+
   '  };'+
   '})();';

  CefRegisterExtension('example/v8', Code, TMyHandler.Create as ICefv8Handler);
end;

//*)







{ TDBICustomChromateRenderProcessHandler }

{$ifdef UseEventListeners}
function TDBICustomChromateRenderProcessHandler.GetPath(const Node: ICefDomNode): string;
begin
  Result := '<' + Node.Name + '>';
  if (Node.Parent <> nil) then begin
    Result := GetPath(Node.Parent) + Result;
  end;
end;

procedure TDBICustomChromateRenderProcessHandler.Listener(const Event: ICefDomEvent);
var
  Msg: ICefProcessMessage;

begin
  Msg := TCefProcessMessageRef.New('click');
  Msg.ArgumentList.SetString(0, GetPath(Event.Target));

  FBrowser.SendProcessMessage(PID_BROWSER, Msg);
end;


procedure TDBICustomChromateRenderProcessHandler.DomVisitor(const Document: ICefDomDocument);
begin
  Document.Body.AddEventListenerProc('click', True, Listener);
end;


function TDBICustomChromateRenderProcessHandler.OnProcessMessageReceived(
  const browser: ICefBrowser;
  sourceProcess: TCefProcessId;
  const message: ICefProcessMessage
  ): Boolean;
begin
  ShowMessageFmt('TDBICustomChromateRenderProcessHandler.OnProcessMessageReceived:: %s', [message.Name]);

  Result := Assigned(browser) and (message.Name = 'visitdom');
  if Result then begin
    FBrowser := browser;
    FBrowser.MainFrame.VisitDomProc(DomVisitor);
  end;
end;
{$endif}

procedure TDBICustomChromateRenderProcessHandler.OnWebKitInitialized;
begin
  TDBICustomChromateExtensions.Instance.RegisterExtensions;
end;





{ TDBICustomChromateExtensionIterator }

destructor TDBICustomChromateExtensionIterator.Destroy;
begin
  FreeAndNil(FItems);

  inherited Destroy;
end;


procedure TDBICustomChromateExtensionIterator.first;
begin
  if Assigned(FItems) then begin
    recno := 0;
  end;
end;


function TDBICustomChromateExtensionIterator.getcount: Variant;
begin
  if Assigned(FItems) then begin
    Result := FItems.Count;
  end
  else begin
    Result := -1;
  end;
end;


function TDBICustomChromateExtensionIterator.GetItems: TDBICustomChromateExtensionItems;
begin
  if not Assigned(FItems) then begin
    FItems := TDBICustomChromateExtensionItems.Create;
    FItems.OwnsObjects := True;
    FItems.Duplicates := dupError;
    FItems.Sorted := True;
  end;
  Result := FItems;
end;


function TDBICustomChromateExtensionIterator.getname: Variant;
begin
  if Assigned(FItems) and (Fitems.ItemIndex < FItems.Count) then begin
    Result := (FItems.Objects[FItems.ItemIndex] as TDBICustomChromateExtension).getname;
  end
  else begin
    Result := inherited getname;
  end;
end;


function TDBICustomChromateExtensionIterator.GetNameStr: String;
begin
  if Assigned(FItems) and (Fitems.ItemIndex < FItems.Count) then begin
    Result := (FItems.Objects[FItems.ItemIndex] as TDBICustomChromateExtension).GetNameStr;
  end
  else begin
    Result := inherited GetNameStr;
  end;
end;


function TDBICustomChromateExtensionIterator.getrecno: Variant;
begin
  if Assigned(FItems) then begin
    Result := FItems.ItemIndex;
  end
  else begin
    Result := -1;
  end;
end;


procedure TDBICustomChromateExtensionIterator.last;
begin
  if Assigned(FItems) then begin
    recno := FItems.Count-1;
  end;
end;


procedure TDBICustomChromateExtensionIterator.new(aname: Variant);
var
  Item: TDBICustomChromateExtension;

begin
  Item := Build(FOwner, VarToStr(aname));
  Items.AddObject(Item.NameStr, Item);
end;


procedure TDBICustomChromateExtensionIterator.next;
begin
  if Assigned(FItems) and (FItems.Count > Succ(FItems.ItemIndex)) then begin
    recno := FItems.ItemIndex + 1;
  end;
end;


procedure TDBICustomChromateExtensionIterator.prior;
begin
  if Assigned(FItems) and (0 < Pred(Items.ItemIndex)) then begin
    recno := FItems.ItemIndex - 1;
  end;
end;


procedure TDBICustomChromateExtensionIterator.SetExtensionOwner(Value: TObject);
var
  Index: Integer;

begin
  inherited SetExtensionOwner(Value);

  if Assigned(FItems) and  (FItems.Count > 0) then begin
    for Index := 0 to FItems.Count-1 do begin
      (FItems.Objects[Index] as TDBICustomChromateExtension).SetExtensionOwner(Value);
    end;
  end;
end;


procedure TDBICustomChromateExtensionIterator.setname(value: Variant);
begin
  if Assigned(FItems) and (Fitems.ItemIndex < FItems.Count) then begin
    (FItems.Objects[FItems.ItemIndex] as TDBICustomChromateExtension).setname(value);
  end
  else begin
    inherited setname(value);
  end;
end;


procedure TDBICustomChromateExtensionIterator.setrecno(value: Variant);
begin
  if Assigned(FItems) and (0 <= value) and (value < FItems.Count) then begin
    FItems.ItemIndex := value;
  end;
end;





{ TDBICustomChromateExtension }

class function TDBICustomChromateExtension.Build(AOwner: TObject; const AName: String): TDBICustomChromateExtension;
begin
  Result := Self.Create;
  Result.FOwner := AOwner;
  Result.FName := AName;
end;


procedure TDBICustomChromateExtension.Error(const Msg: String; Args: array of const);
var
  Prompt: String;

begin
  Prompt := FName;
  if (Prompt = '') then begin
    Prompt := ClassName;
  end;
  Prompt := Prompt + Format(' : ' + Msg, Args);
{##JVR
  if (FOwner is TDBICustomChromateBrowser) then begin
    TDBICustomChromateBrowser(FOwner).Console.Add(Prompt);
  end
  else begin
//}
    MessageDlg(Prompt, mtError, [mbOK], 0);
//##JVR  end;
end;


function TDBICustomChromateExtension.getname: Variant;
begin
  Result := '';

  if (FName = '') then begin
    Error('No Name assigned', []);
  end
  else begin
    Result := FName;
  end;
end;


function TDBICustomChromateExtension.GetNameStr: String;
begin
  Result := '';

  if (FName = '') then begin
    Error('No Name assigned', []);
  end
  else begin
    Result := FName;
  end;
end;


class procedure TDBICustomChromateExtension.Publish(ProcessMessage: ICefProcessMessage);
begin
  TCefv8ContextRef.Current.GetBrowser.SendProcessMessage(PID_BROWSER, ProcessMessage);
end;


function TDBICustomChromateExtension.GetBrowser: ICefBrowser;
begin
  Result := TCefv8ContextRef.Current.GetBrowser;
end;


function TDBICustomChromateExtension.GetExtensionOwner: TObject;
begin
  Result := FOwner;

  if not Assigned(FOwner) then begin
    Error('No Owner assigned', []);
  end;
end;


class procedure TDBICustomChromateExtension.Register(const NameSpace: String);
begin
  TDBICustomChromateExtensions.Instance.AddObject(NameSpace, Self.Create);
end;


procedure TDBICustomChromateExtension.setname(value: Variant);
begin
  FName := VarToStr(value);
end;


procedure TDBICustomChromateExtension.SetExtensionOwner(Value: TObject);
begin
  FOwner := Value;
end;




{ TDBICustomChromateExtensions }

var
  _RegisteredExtensions: TDBICustomChromateExtensions = nil;

class function TDBICustomChromateExtensions.Instance: TDBICustomChromateExtensions;
begin
  if not Assigned(_RegisteredExtensions) then begin
    _RegisteredExtensions := TDBICustomChromateExtensions.Create;
    _RegisteredExtensions.OwnsObjects := True;
    _RegisteredExtensions.Duplicates := dupError;
    _RegisteredExtensions.Sorted := True;
  end;
  Result := _RegisteredExtensions;
end;


class procedure TDBICustomChromateExtensions.Initialize;
begin
  CefRenderProcessHandler := TDBICustomChromateRenderProcessHandler.Create;
end;


class procedure TDBICustomChromateExtensions.RegisterExtensions;
var
  Index: Integer;

begin
  if Assigned(_RegisteredExtensions) then begin
    for Index := 0 to Instance.Count-1 do begin
      TDBICustomChromateExtensionHandler.Register(Instance.Strings[Index], Instance.Objects[Index]);
    end;
  end;
end;


class procedure TDBICustomChromateExtensions.Release;
begin
  FreeAndNil(_RegisteredExtensions);
end;


function TDBICustomChromateExtensions.GetExtension(const Index: String): TDBICustomChromateExtension;
var
  ItemIndex: Integer;

begin
  ItemIndex := IndexOf(Index);
  Assert(ItemIndex > -1, Index + ' object not found');

  Result := Objects[ItemIndex] as TDBICustomChromateExtension;
end;



{ TDBICustomChromateExtensionHandler }

function TDBICustomChromateExtensionHandler.Execute(
  const name: ustring;
  const obj: ICefv8Value;
  const arguments: TCefv8ValueArray;
  var retval: ICefv8Value;
  var exception: ustring
  ): Boolean;
begin
  Result := inherited Execute(name, obj, arguments, retval, exception);
end;

initialization
  TDBICustomChromateExtensions.Initialize;

finalization
  TDBICustomChromateExtensions.Release;

end.
