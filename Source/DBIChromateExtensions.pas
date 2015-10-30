unit DBIChromateExtensions;

interface

uses
  Windows, Messages, SysUtils, Classes, Variants, Graphics, Forms, Controls, StdCtrls, ExtCtrls,
  Actions, ActnList, cefvcl, ceflib, DBICustomChromateExtensions;

type
  TChromateAction = class(TDBICustomChromateExtensionIterator)
  public
    class procedure execute(const ActionName: String);
  end;

type
  TChromiumExtension = class(TDBICustomChromateExtensionIterator)
  public
    procedure display(msg: Variant);
    procedure debug(msg: Variant);
  end;

type
  TJvrExtension = class(TDBICustomChromateExtensionIterator)
  public
    procedure point(X, Y: Variant);
  end;

type
  TTestExtension = class(TDBICustomChromateExtension)
    class function hello: string;
  end;




implementation

uses
  Dialogs;

(*##JVR

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





{ TChromateAction }

class procedure TChromateAction.execute(const ActionName: String);
var
  ProcessMessage: ICefProcessMessage;

begin
  ProcessMessage := TCefProcessMessageRef.New('action');
  ProcessMessage.ArgumentList.SetString(0, ActionName);

  Publish(ProcessMessage);
end;





{ TTestExtension }

class function TTestExtension.hello;
var
  Browser: ICefBrowser;

begin
  Browser := TCefv8ContextRef.Current.GetBrowser;

  ShowMessage('Hello from the world of Delphi');
end;





{ TJvrExtension }

procedure TJvrExtension.point(X, Y: Variant);
begin
  ShowMessageFmt('Object Name %s [X = %s] [Y = %s]', [name, VarToStr(X), VarToStr(Y)]);
end;





{ TChromiumExtension }

procedure TChromiumExtension.debug(msg: Variant);
begin
  ShowMessageFmt('%s = %s: %s', [Self.Classname, VarToStr(name), VarToStr(msg)]);
end;

{##JVR
var
  Form: TFormHtmlViewer;

begin
  if Owner is TFormHtmlViewer then begin
    Form := Owner as TFormHtmlViewer;

    Form.Console.Add(Format('%s = %s --> %s', [Self.Classname, NameStr, VarToStr(msg)]));
  end;
end;
//}

procedure TChromiumExtension.display(msg: Variant);
begin
  ShowMessageFmt('%s = %s: %s', [Self.Classname, VarToStr(name), VarToStr(msg)]);
end;


initialization
  TChromateAction.Register('action');
  TChromiumExtension.Register('sample');
  TJvrExtension.Register('delphi');
  TTestExtension.Register('app');

end.
