// _____________________________________________________________________________
{
  Copyright (C) 1996-2013, All rights reserved, John Vander Reest

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

{#omcodecop off : jvr : dbilib code}

unit DBIProcesses;

interface

uses
  Windows, SysUtils, Classes;

type
  TDBIProcessOutputInfo = class(TPersistent)
  private
    FID: Integer;
    FLine: AnsiString;
    FCycles: LongWord;

  public
    property ID: Integer read FID write FID;
    property Line: AnsiString read FLine write FLine;
    property Cycles: LongWord read FCycles write FCycles;

  end;
  TDBIProcessOutputInfoClass = class of TDBIProcessOutputInfo;


type
  TDBIProcessInfoOption = (piCtrlCAbort, piDetachedProcess, piVerbose);
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

    FParameters: String;
    FSourceName: String;
    FTargetName: String;

    hRead: THandle;
    hWrite: THandle;
    hAbortRead: THandle;
    hAbortWrite: THandle;

  private
    FOnIdle: TNotifyEvent;

  protected
    function GetCommandLine: String; virtual;
    function GetEnvironmentVariable(const Name: String): String;
    function GetOutput: TStrings; virtual;
    function GetParameters: String; virtual;
    function GetSourceName: String; virtual;
    function GetTargetName: String; virtual;

    procedure OutputRelease; virtual;

    procedure ProcessAbort;
    procedure ProcessCloseHandles;
    function ProcessCreate: Boolean;
    function ProcessIdle: Boolean; virtual;
    function ProcessOutput(hRead: THandle; var TextBuffer: AnsiString; const Eof: Boolean): Boolean;
    procedure ProcessOutputLine(const Value: AnsiString); virtual;
    function ProcessStartupInfo: Boolean;

    procedure SetEnvironmentPath(const Value: String);
    procedure SetOutput(Value: TStrings);
    procedure SetParameters(const Value: String); virtual;
    procedure SetSourceName(const Value: String); virtual;
    procedure SetTargetName(const Value: String); virtual;

    property Aborted: Boolean read FAborted write FAborted;
    property EnvironmentPath: String read FEnvironmentPath write SetEnvironmentPath;
    property Output: TStrings read GetOutput write SetOutput;
    property ExitCode: Integer read FExitCode;
    property Options: TDBIProcessInfoOptions read FOptions write FOptions;

  protected
    property CommandLine: String read GetCommandLine;
    property Parameters: String read GetParameters write SetParameters;
    property SourceName: String read GetSourceName write SetSourceName;
    property TargetName: String read GetTargetName write SetTargetName;

    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute: Boolean; virtual;

    class function Instance: TDBICustomProcess; virtual;
    class procedure Release; virtual;

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

    procedure ProcessOutputLine(const Value: AnsiString); override;
    function ProcessOutputItem(Item: TDBIProcessOutputInfo): Boolean; virtual;

    property Data: TDBIProcessOutputInfo read GetData;
    property Description: String read FDescription write FDescription;

  public
    property OutputClassType: TDBIProcessOutputInfoClass read GetOutputClassType;
    property Parameters;
    property TargetName;

    property OnIdle;
    property OnProcessItem: TDBIProcessItem read FOnProcessItem write FOnProcessItem;

  end;


implementation


{ TDBIProcess }

function TDBIProcess.GetOutputClassType: TDBIProcessOutputInfoClass;
begin
  Result := TDBIProcessOutputInfo;
end;


function TDBIProcess.GetData: TDBIProcessOutputInfo;
begin
  Result := Output.Objects[Output.Count-1] as TDBIProcessOutputInfo;
end;


procedure TDBIProcess.ProcessOutputLine(const Value: AnsiString);
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





{ TDBICustomProcess }

type
  TDBIOutputList = class(TStringList);


constructor TDBICustomProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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

  inherited Destroy;
end;


function TDBICustomProcess.Execute: Boolean;
var
  OutputBuffer: AnsiString;
  OrgEnvPath: String;

  function Processing: Boolean;
  begin
    Result := not (piDetachedProcess in Options);
    if Result then begin
      Result := WaitForSingleObject(FProcessInfo.hProcess, 80) = WAIT_TIMEOUT;
    end
    else begin
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
          CloseHandle(FProcessInfo.hProcess);
        end;
      end
      else begin
        Win32Check(False);
        FExitCode := -1;
      end;

    finally
      if (EnvironmentPath <> '') then begin
        SetEnvironmentVariable('PATH', Pointer(OrgEnvPath));
      end;
    end;
  finally
    ProcessCloseHandles;
  end;

  Result := ExitCode = 0;
end;


function TDBICustomProcess.GetCommandLine: String;
begin
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
    PChar(FSourceName),
    FStartupInfo,
    FProcessInfo
    );
end;


function TDBICustomProcess.ProcessIdle: Boolean;
begin
  Result := Assigned(FOnIdle);

  if Result then begin
    FOnIdle(Self);
  end;
end;


function TDBICustomProcess.ProcessOutput(hRead: THandle; var TextBuffer: AnsiString; const Eof: Boolean): Boolean;
var
  OutputBuffer: AnsiString;
  LineBuffer: AnsiString;
  PTextBuffer, PStart: PAnsiChar;
  BytesInPipe: Cardinal;
  Size: Cardinal;

begin
  BytesInPipe := 0;

  Result := PeekNamedPipe(hRead, nil, 0, nil, @BytesInPipe, nil);
  Result := Result and (BytesInPipe <> 0);
  if not Result then begin
    Exit;
  end;

  // Read Pipe into Buffer
  SetLength(OutputBuffer, BytesInPipe);
  ReadFile(hRead, OutputBuffer[1], BytesInPipe, Size, nil);
  SetLength(OutputBuffer, Size);
  TextBuffer := TextBuffer + OutputBuffer;

  // Split lines on Line-Break boundries
  PTextBuffer := PAnsiChar(TextBuffer);
  PStart := PTextBuffer;

  if (PTextBuffer <> nil) then begin
    while (PTextBuffer^ <> #0) do begin
      PStart := PTextBuffer;
      while not (PTextBuffer^ in [#0, #10, #13]) do begin
        Inc(PTextBuffer);
      end;

      // if we are at a Line-Break then process line
      if (PTextBuffer^ <> #0) or Eof then begin
        SetString(LineBuffer, PStart, PTextBuffer - PStart);
        ProcessOutputLine(LineBuffer);

        if PTextBuffer^ = #13 then Inc(PTextBuffer);
        if PTextBuffer^ = #10 then Inc(PTextBuffer);
      end;
    end;
  end;

  // Now assign the remaining chars to the TextBuffer
  SetString(TextBuffer, PStart, PTextBuffer - PStart);
end;


procedure TDBICustomProcess.ProcessOutputLine(const Value: AnsiString);
begin
  Output.Add(String(Value));
end;


function TDBICustomProcess.ProcessStartupInfo: Boolean;
begin
  // Standard I/O
  Result := CreatePipe(hRead, hWrite, @FProcessAttributes, 0);

  // Error I/O
  Result := Result and CreatePipe(hAbortRead, hAbortWrite, @FProcessAttributes, 0);

  if Result then begin
    FStartupInfo.wShowWindow := SW_HIDE;
    FStartupInfo.hStdInput := hAbortRead;
    FStartupInfo.hStdOutput := hWrite;
    FStartupInfo.hStdError := FStartupInfo.hStdOutput; // redirect
    FStartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  end;
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


initialization
finalization
  TDBICustomProcess.Release;

end.
