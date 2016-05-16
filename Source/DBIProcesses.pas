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
  1.0 | 20/05/2010 10:11:01 | Jvr | Initial Release
  1.1 | 15/07/2013 10:11:01 | Jvr | Refactored
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib}

unit DBIProcesses;

interface

uses
  Windows, SysUtils, Classes;

type
  TDBIProcessOutputInfo = class(TPersistent)
  private
    FID: Integer;
    FLine: String;
    FCycles: LongWord;

  public
    property ID: Integer read FID write FID;
    property Line: String read FLine write FLine;
    property Cycles: LongWord read FCycles write FCycles;

  end;
  TDBIProcessOutputInfoClass = class of TDBIProcessOutputInfo;


type
  TDBIProcessEmitKind = (ioNone, ioStdin, ioStdout, ioStderr, ioInfo, ioTrace, ioWarning, ioError, ioDebug);
  TDBIProcessEmit = procedure(Sender: TObject; const Message: String; const EmitKind: TDBIProcessEmitKind) of object;

  TDBIProcessInfoOption = (piCtrlCAbort, piDetachedProcess, piRedirectOutput, piVerbose, piVerifyTargetName);
  TDBIProcessInfoOptions = set of TDBIProcessInfoOption;
  

type
  TDBICustomProcess = class(TComponent)
  private
    FAborted: Boolean;
    FExitCode: Integer;
    FEnvironmentPath: String;
    FOptions: TDBIProcessInfoOptions;
    FOutput: TStrings;
    FProcessInfo: TProcessInformation;
    FProcessAttributes: TSecurityAttributes;
    FStartupInfo: TStartupInfo;
    FTickOffset: LongWord;
    FVisible: Boolean;
    FWaitHandle: THandle;

    FParameters: String;
    FSourceName: String;
    FTargetName: String;

    hRead: THandle;
    hWrite: THandle;
    hAbortRead: THandle;
    hAbortWrite: THandle;

  private
    FOnIdle: TNotifyEvent;
    FOnProcessBegin: TNotifyEvent;
    FOnProcessEnd: TNotifyEvent;

  protected
    function CheckTargetName: String;

    procedure Emit(
      Sender: TObject;
      const Message: String;
      const EmitKind: TDBIProcessEmitKind
      ); virtual;

    function GetCommandLine: String; virtual;
    function GetEnvironmentVariable(const Name: String): String;
    function GetDebugInfo: String;
    function GetOutput: TStrings; virtual;
    function GetParameters: String; virtual;
    function GetSourceName: String; virtual;
    function GetTargetName: String; virtual;

    procedure OutputRelease; virtual;

    procedure ProcessAbort;
    procedure ProcessBegin; virtual;
    procedure ProcessClose;
    procedure ProcessCloseHandles;
    function ProcessCreate: Boolean;
    procedure ProcessEnd; virtual;
    function ProcessExecute: Boolean; virtual;
    function ProcessIdle: Boolean; virtual;
    function ProcessOutput(hRead: THandle; var TextBuffer: String; const Eof: Boolean): Boolean;
    procedure ProcessOutputLine(const Value: String); virtual;
    function ProcessStartupInfo: Boolean;

    procedure RaiseLastOSError;
    procedure RegisterDetachedCallback;

    procedure SetEnvironmentPath(const Value: String);
    procedure SetOptions(const Value: TDBIProcessInfooptions); virtual;
    procedure SetOutput(Value: TStrings);
    procedure SetParameters(const Value: String); virtual;
    procedure SetSourceName(const Value: String); virtual;
    procedure SetTargetName(const Value: String); virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    property Aborted: Boolean read FAborted write FAborted;
    property EnvironmentPath: String read FEnvironmentPath write SetEnvironmentPath;
    property Output: TStrings read GetOutput write SetOutput;
    property ExitCode: Integer read FExitCode;
    property Options: TDBIProcessInfoOptions read FOptions write SetOptions;

  protected
    property CommandLine: String read GetCommandLine;
    property Parameters: String read GetParameters write SetParameters;
    property SourceName: String read GetSourceName write SetSourceName;
    property TargetName: String read GetTargetName write SetTargetName;
    property Visible: Boolean read FVisible write SetVisible default True;

    property OnProcessBegin: TNotifyEvent read FOnProcessBegin write FOnProcessBegin;
    property OnProcessEnd: TNotifyEvent read FOnProcessEnd write FOnProcessEnd;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean; virtual;

    class function Instance: TDBICustomProcess; virtual;
{$ifndef fpc}
    class function ProcessExists(ProgramName: String): Boolean;
{$endif}
    class procedure Release; virtual;

  end;


type
  TDBIShellProcess = class(TDBICustomProcess)
  protected
    procedure SetTargetName(const Value: String); override;

  public
    property Options;
    property Parameters;
    property SourceName;
    property TargetName;
    property Visible;

    property OnProcessBegin;
    property OnProcessEnd;
    property OnIdle;

  end;



type
  TDBIProcessItem = function(Item: TDBIProcessOutputInfo; var Aborted: Boolean): Boolean of object;

  TDBIProcess = class(TDBICustomProcess)
  private
    FDescription: String;
    FOnProcessItem: TDBIProcessItem;

  protected
    function GetOutputClassType: TDBIProcessOutputInfoClass; virtual;
    function GetData: TDBIProcessOutputInfo; virtual;

    procedure ProcessOutputLine(const Value: String); override;
    function ProcessOutputItem(Item: TDBIProcessOutputInfo): Boolean; virtual;

    property Data: TDBIProcessOutputInfo read GetData;
    property Description: String read FDescription write FDescription;

  public
    constructor Create(AOwner: TComponent); override;

    property OutputClassType: TDBIProcessOutputInfoClass read GetOutputClassType;
    property Parameters;
    property TargetName;

    property OnProcessBegin;
    property OnProcessEnd;
    property OnIdle;
    property OnProcessItem: TDBIProcessItem read FOnProcessItem write FOnProcessItem;

  end;


implementation

{$ifndef fpc}
uses
  TlHelp32;
{$endif}

type
  WaitOrTimerCallback = procedure (Instance: TDBIProcess; TimeOrWaitFired: Boolean); stdcall;

const
  WT_EXECUTEDEFAULT      = $00;
  WT_EXECUTEONLYONCE     = $08;
  WT_EXECUTELONGFUNCTION = $10;

function RegisterWaitForSingleObject(
  var phNewWaitObject: THandle;
  hObject: THandle;
  Callback: WaitOrTimerCallback;
  Context: Pointer;
  dwMilliseconds: Cardinal;
  dwFlags: Cardinal
  ): Boolean; stdcall; external 'kernel32.dll';

function UnregisterWait(WaitHandle: THandle): Boolean; stdcall; external 'kernel32.dll';

procedure WaitCallback(Instance: TDBIProcess; TimeOrWaitFired: Boolean); stdcall;
begin
  Instance.ProcessEnd;
end;





{ TDBIProcess }

function TDBIProcess.GetOutputClassType: TDBIProcessOutputInfoClass;
begin
  Result := TDBIProcessOutputInfo;
end;


constructor TDBIProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Options := [piRedirectOutput];
  Visible := False;
end;


function TDBIProcess.GetData: TDBIProcessOutputInfo;
begin
  Result := Output.Objects[Output.Count-1] as TDBIProcessOutputInfo;
end;


procedure TDBIProcess.ProcessOutputLine(const Value: String);
var
  Item: TDBIProcessOutputInfo;

begin
  // Add Output Item Instance
  Item := OutputClassType.Create;
  Item.Line := Value;
  Item.Cycles := GetTickCount - FTickOffset;
  Item.ID := Output.AddObject(String(Value), Item);

  ProcessOutputItem(Item);
end;


function TDBIProcess.ProcessOutputItem(Item: TDBIProcessOutputInfo): Boolean;
begin
  Result := Assigned(FOnProcessItem) and FOnProcessItem(Item, FAborted) and not FAborted;
end;





{ TDBIShellProcess }

procedure TDBIShellProcess.SetTargetName(const Value: String);
begin
  inherited SetTargetName(Value);

  if (SourceName = '') then begin
    inherited SetSourceName(ExtractFilepath(Value));
  end;
end;





{ TDBICustomProcess }

type
  TDBIOutputList = class(TStringList);


function TDBICustomProcess.CheckTargetName: String;
var
  Extension: String;
  Offset: Integer;

begin
  Result := StringReplace(TargetName, '"', '', [rfReplaceAll]);

  // Strip unwanted parameters
  Extension := ExtractFileExt(Result);
  Offset := Pos(' ', Extension);
  if (Offset > 1) then begin
    SetLength(Extension, Offset-1);
    Result := ChangeFileExt(Result, Extension);
  end;

  if (piVerifyTargetName in Options) and (Result <> '') and not FileExists(Result) then begin
    raise Exception.CreateFmt('TargetName "%s" Not Found', [Result]);
  end;
end;


constructor TDBICustomProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FVisible := True;
  FTickOffset := GetTickCount;

  // ProcessAttributes
  FillChar(FProcessAttributes, SizeOf(FProcessAttributes), 0);
  FProcessAttributes.nLength := SizeOf(FProcessAttributes);
  FProcessAttributes.bInheritHandle := True;
  FProcessAttributes.lpSecurityDescriptor := nil;

  // ProcessInfo
  FillChar(FProcessInfo, SizeOf(FProcessInfo), 0);

  // StartupInfo
  FillChar(FStartupInfo, SizeOf(FStartupInfo), 0);
  FStartupInfo.cb := SizeOf(FStartupInfo);
end;


destructor TDBICustomProcess.Destroy;
begin
  OutputRelease;

  // Release Detached Process Wait handle
  if (FWaitHandle <> 0) then begin
    UnregisterWait(FWaitHandle);
    FWaitHandle := 0;
  end;

  inherited Destroy;
end;


procedure TDBICustomProcess.Emit(
  Sender: TObject;
  const Message: String;
  const EmitKind: TDBIProcessEmitKind
  );
begin
  //##NOP
end;


function TDBICustomProcess.Execute: Boolean;
begin
  Result := ProcessExecute;
end;


function TDBICustomProcess.GetDebugInfo: String;
begin
  Result := '';

  if (TargetName <> '') then begin
    Result := Result + 'TargetName: ' + TargetName + sLineBreak;
  end;

  if (SourceName <> '') then begin
    Result := Result + 'SourceName: ' + SourceName + sLineBreak;
  end;

  if (Parameters <> '') then begin
    Result := Result + 'Parameters: ' + Parameters + sLineBreak;
  end;
end;


function TDBICustomProcess.GetCommandLine: String;
begin
  CheckTargetName;

  Result := TargetName + ' ' + Parameters;
end;


function TDBICustomProcess.GetEnvironmentVariable(const Name: String): String;
begin
  SetLength(Result, 8 * 1024);
  SetLength(Result, Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Length(Result)));
end;


function TDBICustomProcess.GetOutput: TStrings;
begin
  if not Assigned(FOutput) then begin
    FOutput := TDBIOutputList.Create;
  end;
  Result := FOutput;
end;


function TDBICustomProcess.GetParameters: String;
begin
  Result := FParameters;
end;


function TDBICustomProcess.GetSourceName: String;
begin
  Result := FSourceName;
end;


function TDBICustomProcess.GetTargetName: String;
begin
  Result := FTargetName;
end;


var
  _ProcessInstance: TDBICustomProcess = nil;

class function TDBICustomProcess.Instance: TDBICustomProcess;
begin
  if not Assigned(_ProcessInstance) then begin
    _ProcessInstance := Self.Create(nil);
  end;
  Result := _ProcessInstance;
end;


procedure TDBICustomProcess.OutputRelease;
begin
  if Assigned(FOutput) and (FOutput is TDBIOutputList) then begin
    FOutput.Free;
  end;
  FOutput := nil;
end;


procedure TDBICustomProcess.ProcessAbort;
begin
  if (piCtrlCAbort in Options) then begin
    GenerateConsoleCtrlEvent(CTRL_C_EVENT, FProcessInfo.dwProcessId);

    if WaitForSingleObject(FProcessInfo.hProcess, 500) = WAIT_TIMEOUT then begin
      TerminateProcess(FProcessInfo.hProcess, Cardinal(1));
    end;
  end
  else begin
    TerminateProcess(FProcessInfo.hProcess, Cardinal(1));
  end;
end;


procedure TDBICustomProcess.ProcessBegin;
begin
  if Assigned(FOnProcessBegin) then begin
    FOnProcessBegin(Self);
  end;
end;


procedure TDBICustomProcess.ProcessClose;
begin
  if not (piDetachedProcess in Options) then begin
    ProcessEnd;
  end;
end;


procedure TDBICustomProcess.ProcessCloseHandles;
  procedure ProcessCloseHandle(AHandle: THandle);
  begin
    if (AHandle <> 0) then begin
      CloseHandle(AHandle);
    end;
  end;

begin
  ProcessCloseHandle(hAbortRead);
  ProcessCloseHandle(hAbortWrite);
  ProcessCloseHandle(hRead);
  ProcessCloseHandle(hWrite);
end;


function TDBICustomProcess.ProcessCreate: Boolean;
const
  ThreadAttributes = nil;

begin
  Result := CreateProcess(
    nil,
    PChar(GetCommandLine),
    @FProcessAttributes,
    ThreadAttributes,
    True,
    CREATE_SUSPENDED,
    nil,
    PChar(SourceName),
    FStartupInfo,
    FProcessInfo
    );
end;


procedure TDBICustomProcess.ProcessEnd;
begin
  try
    if Assigned(FOnProcessEnd) then begin
      FOnProcessEnd(Self);
    end;

  finally
    if (FWaitHandle <> 0) then begin
      UnregisterWait(FWaitHandle);
      FWaitHandle := 0;
    end;

    CloseHandle(FProcessInfo.hProcess);
  end;
end;


function TDBICustomProcess.ProcessExecute: Boolean;
const
  ExitCode_SUCCESS = 0;
  ExitCode_STILL_ACTIVE = 259;

var
  OutputBuffer: String;
  OrgEnvPath: String;

  function Processing: Boolean;
  begin
    Result := not (piDetachedProcess in Options);
    if Result then begin
      Result := WaitForSingleObject(FProcessInfo.hProcess, 80) = WAIT_TIMEOUT;
    end
    else begin
      RegisterDetachedCallback;

      WaitForInputIdle(FProcessInfo.hProcess, Infinite);
    end;
  end;

begin
  FAborted := False;
  FExitCode := -2;
  FTickOffset := GetTickCount;
  OutputBuffer := '';

  Result := ProcessStartupInfo;
  if not Result then begin
    Exit;
  end;

  try
    OrgEnvPath := GetEnvironmentVariable('PATH');
    if (EnvironmentPath <> '') then begin
      SetEnvironmentVariable('PATH', Pointer(EnvironmentPath));
    end;

    try
      Result := ProcessCreate;
      if Result then begin
        ProcessBegin;

        ResumeThread(FProcessInfo.hThread);
        CloseHandle(FProcessInfo.hThread);

        try
          while Processing and not Aborted do begin
            ProcessOutput(hRead, OutputBuffer, False);
            ProcessIdle;
          end;

          // Process remaining characters in buffer
          ProcessOutput(hRead, OutputBuffer, True);

          if Aborted then begin
            ProcessAbort;
          end;

          // Exit-Code
          GetExitCodeProcess(FProcessInfo.hProcess, Cardinal(FExitCode));

        finally
          ProcessClose;
        end;
      end
      else begin
        FExitCode := -1;

        RaiseLastOSError;
      end;

    finally
      if (EnvironmentPath <> '') then begin
        SetEnvironmentVariable('PATH', Pointer(OrgEnvPath));
      end;
    end;
  finally
    ProcessCloseHandles;
  end;

  Result := (ExitCode = ExitCode_SUCCESS) or (ExitCode = ExitCode_STILL_ACTIVE);
end;


{$ifndef fpc}
class function TDBICustomProcess.ProcessExists(ProgramName: String): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;

begin
  Result := False;
  ProgramName := UpperCase(ProgramName);

  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while (Integer(ContinueLoop) <> 0) and not Result do begin
    Result :=
      (ProgramName = UpperCase(ExtractFileName(FProcessEntry32.szExeFile))) or
      (ProgramName = UpperCase(FProcessEntry32.szExeFile));

    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;

  CloseHandle(FSnapshotHandle);
end;
{$endif}


function TDBICustomProcess.ProcessIdle: Boolean;
begin
  Result := Assigned(FOnIdle);

  if Result then begin
    FOnIdle(Self);
  end;
end;


function TDBICustomProcess.ProcessOutput(hRead: THandle; var TextBuffer: String; const Eof: Boolean): Boolean;
var
  OutputBuffer: AnsiString;
  LineBuffer: String;
  PTextBuffer, PStart: PChar;
  BytesInPipe: Cardinal;
  Size: Cardinal;

begin
  BytesInPipe := 0;

  Result := PeekNamedPipe(hRead, nil, 0, nil, @BytesInPipe, nil);
  Result := Result and (BytesInPipe <> 0);

  // Read Pipe into Buffer
  if Result then begin
    SetLength(OutputBuffer, BytesInPipe);
    ReadFile(hRead, OutputBuffer[1], BytesInPipe, Size, nil);
    SetLength(OutputBuffer, Size);
    TextBuffer := TextBuffer + String(OutputBuffer);
  end;

  Result := Length(TextBuffer) > 0;
  if not Result then begin
    Exit;
  end;

  // Split lines on Line-Break boundries
  PTextBuffer := PChar(TextBuffer);
  PStart := PTextBuffer;

  if (PTextBuffer <> nil) then begin
    while (PTextBuffer^ <> #0) do begin
      PStart := PTextBuffer;
      while not CharInSet(PTextBuffer^, [#0, #10, #13]) do begin
        Inc(PTextBuffer);
      end;

      // if we are at a Line-Break then process line
      if (PTextBuffer^ <> #0) or Eof then begin
        SetString(LineBuffer, PStart, PTextBuffer - PStart);

        if PTextBuffer^ = #13 then Inc(PTextBuffer);
        if PTextBuffer^ = #10 then Inc(PTextBuffer);

        Emit(Self, LineBuffer, ioStdout);
        ProcessOutputLine(LineBuffer);
      end;
    end;
  end;

  // Now assign the remaining chars to the TextBuffer
  SetString(TextBuffer, PStart, PTextBuffer - PStart);
end;


procedure TDBICustomProcess.ProcessOutputLine(const Value: String);
begin
  Output.Add(String(Value));
end;


function TDBICustomProcess.ProcessStartupInfo: Boolean;
const
  RedirectOutputFlag: array[Boolean] of Word = (0, STARTF_USESTDHANDLES);
  ShowWindowFlag: array[Boolean] of Word = (STARTF_USESHOWWINDOW, 0);

begin
  Result := True;

  if not Visible then begin
    FStartupInfo.wShowWindow := SW_HIDE;
  end;

  if  (piRedirectOutput in Options) then begin
    // Standard I/O
    Result := CreatePipe(hRead, hWrite, @FProcessAttributes, 0);

    // Error I/O
    Result := Result and CreatePipe(hAbortRead, hAbortWrite, @FProcessAttributes, 0);

    FStartupInfo.hStdInput := hAbortRead;
    FStartupInfo.hStdOutput := hWrite;
    FStartupInfo.hStdError := FStartupInfo.hStdOutput; // redirect
  end;

  FStartupInfo.dwFlags :=
    RedirectOutputFlag[piRedirectOutput in Options] or ShowWindowFlag[Visible];
end;


procedure TDBICustomProcess.RaiseLastOSError;
const
  SysOSError = 'System Error.  Code: %d.' + sLineBreak + '%s';
  SysUnkOSError = 'A call to an OS function failed';

var
  Error: EOSError;
  LastError: Integer;

begin
  LastError := GetLastError;

  if (LastError <> 0) then begin
    Error := EOSError.CreateFmt(GetDebugInfo + SysOSError, [LastError, SysErrorMessage(LastError)]);
  end
  else begin
    Error := EOSError.Create(GetDebugInfo + SysUnkOSError);
  end;

  Error.ErrorCode := LastError;
  raise Error;
end;


procedure TDBICustomProcess.RegisterDetachedCallback;
begin
  RegisterWaitForSingleObject(
    FWaitHandle,                  // New wait object handle
    FProcessInfo.hProcess,        // Process Handle
    WaitCallback,                 // Callback Routine
    Self,                         // Context Data
    INFINITE, WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION or WT_EXECUTEONLYONCE
    );
end;


class procedure TDBICustomProcess.Release;
begin
  _ProcessInstance.Free;
  _ProcessInstance := nil;
end;


procedure TDBICustomProcess.SetEnvironmentPath(const Value: String);
begin
  FEnvironmentPath := Value;
{##JVR
  if (FEnvironmentPath <> '') then begin
    SetEnvironmentVariable('PATH', Pointer(FEnvironmentPath));
  end;
//}
end;


procedure TDBICustomProcess.SetOptions(const Value: TDBIProcessInfooptions);
begin
  FOptions := Value;
end;


procedure TDBICustomProcess.SetOutput(Value: TStrings);
begin
  OutputRelease;

  FOutput := Value;
end;


procedure TDBICustomProcess.SetParameters(const Value: String);
begin
  FParameters := Value;
end;


procedure TDBICustomProcess.SetSourceName(const Value: String);
begin
  FSourceName := Value;
end;


procedure TDBICustomProcess.SetTargetName(const Value: String);
begin
  FTargetName := Value;
end;


procedure TDBICustomProcess.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;



initialization
finalization
  TDBICustomProcess.Release;

end.
