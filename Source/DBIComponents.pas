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
  1.0 | 18/04/2008 11:50:10 | Jvr | Initial Release
  ______________________________________________________________________________
}

{#omcodecop off : jvr : dbilib code}

unit DBIComponents;

{$I DBICompilers.inc}

interface

uses
  Classes, SysUtils;

type
  TDBICompoundComponent = class(TComponent)
  private
    FParent: TDBICompoundComponent;
    FChildren: TList;

  protected
    procedure ChangeParent(AParent: TDBICompoundComponent); virtual;
    function GetChild(const Index: Integer): TDBICompoundComponent; virtual;
    function GetChildCount: Integer; virtual;
    function GetChildIndex: Integer;
    function GetChildList: TList;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetCompoundName: String; virtual;
    function GetDisplayName: string; overload; virtual;
    function FindUniqueName: String; virtual;

    procedure InsertChild(AComponent: TDBICompoundComponent);
    procedure ReadState(Reader: TReader); override;
    procedure RemoveChild(AComponent: TDBICompoundComponent);
    procedure SetChildIndex(const Value: Integer);
//##JVR    procedure SetName(const NewName: TComponentName); override;
    procedure SetParent(AParent: TDBICompoundComponent); virtual;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetParentReference(Enable: Boolean);
    procedure SetReference(Enable: Boolean);

    property ChildIndex: Integer read GetChildIndex write SetChildIndex stored False;
    property ChildList: TList read GetChildList;
    property DisplayName: String read GetDisplayName;

  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AParent: TDBICompoundComponent); reintroduce; overload;
    destructor Destroy; override;

    function GetMemberComponent(
      const ACompoundName: String;
      MinClass: TClass = nil
      ): TComponent;

    function GetParentComponent: TComponent; override;
    function GetRootComponent: TComponent; virtual;
    function HasParent: Boolean; override;

    property ChildCount: Integer read GetChildCount;
    property Children[const Index: Integer]: TDBICompoundComponent read GetChild;
    property CompoundName: String read GetCompoundName;
    property Parent: TDBICompoundComponent read FParent write SetParent;

  end;


type
  IPersistenceAdapter = interface
    function LoadFromFile(AFileName: TFileName): TComponent;
    function LoadFromResource(RootAncestor: TClass = nil): TComponent;
    function LoadFromStream(AStream: TStream): TComponent;
    function SaveToFile(AFileName: TFileName): boolean;
    function SaveToStream(AStream: TStream): TStream;
  end;


type
  TDBICustomPersistenceAdapter = class(TDBICompoundComponent, IPersistenceAdapter)
  private
    FRootComponent: TComponent;
    FText: String;

  protected
//##CLASSES    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetFileName: TFileName; virtual;
    procedure SetRootComponent(Value: TComponent); virtual;

    property RootComponent: TComponent read GetRootComponent write SetRootComponent;

  protected
    function GetDisplayName: String; override;
    function GetText: String; virtual;
    function IsTextStored: Boolean;
    procedure SetText(const Value: String); virtual;

    property Text: String read GetText write SetText stored IsTextStored;

  public
    function ToString: String; {$ifdef DELPHIxe2}override;{$else}virtual;{$endif}

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function GetRootComponent: TComponent; override;

    function LoadFromFile(AFileName: TFileName = ''): TComponent; virtual;
    function LoadFromResource(RootAncestor: TClass = nil): TComponent; virtual;
    function LoadFromStream(AStream: TStream): TComponent; virtual;

    function SaveToFile(AFileName: TFileName = ''): Boolean; virtual;
    function SaveToStream(AStream: TStream): TStream; virtual;

  end;

  TDBIPersistenceAdapter = class(TDBICustomPersistenceAdapter)
  end;

  TDBIPersistentComponent = class(TDBICustomPersistenceAdapter)
  end;


type
  TDBICustomIniFilePersistenceAdapter = class(TDBICustomPersistenceAdapter)
  private
    FStrings: TStrings;

  protected
    function GetFileName: TFileName; override;
    function GetStrings: TStrings;

    property Strings: TStrings read GetStrings;

  public
    function ToString: String; override;

    destructor Destroy; override;

    function LoadFromFile(AFileName: TFileName = ''): TComponent; override;
    function SaveToFile(AFileName: TFileName = ''): Boolean; override;

  end;

  TDBIIniFilePersistenceAdapter = class(TDBICustomIniFilePersistenceAdapter)
  end;

implementation

uses
  Consts, RtlConsts, IniFiles, Types, TypInfo, DBITypInfo, DBIUtils;


{ TDBIIniFile }

type
  TDBIIniFile = class(TMemIniFile)
  protected
    function GetSection(const Value: String): TStrings;
    function CreateSection(const ASection: String): TStrings;
    procedure WriteSectionValues(const ASection: String; Strings: TStrings);
  end;


// _____________________________________________________________________________
{**
  Jvr - 18/04/2008 13:24:59 - Initial code.<br />
}
function TDBIIniFile.CreateSection(const ASection: String): TStrings;
const
  DummyKey = '_Dummy42zxc';

begin
  Result := nil;
  try
    WriteString(ASection, DummyKey, DummyKey);
    DeleteKey(ASection, DummyKey);
    Result := GetSection(ASection);
  except
    Result.Free;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 18/04/2008 11:54:36 - Initial code.<br />
}
procedure TDBIIniFile.WriteSectionValues(const ASection: String; Strings: TStrings);
var
  SectionItem: TStrings;

begin
  Assert(Assigned(Strings));
  Strings.BeginUpdate;
  try
    SectionItem := GetSection(ASection);
    if not Assigned(SectionItem) then begin
      SectionItem := CreateSection(ASection);
    end;

    Assert(Assigned(SectionItem));
    SectionItem.Assign(Strings); // XE3: Note, "Assign" also copies "Sorted" property in XE3 (not in 2006).
   finally
    Strings.EndUpdate;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 18/04/2008 12:06:36 - Initial code.<br />
}
function TDBIIniFile.GetSection(const Value: String): TStrings;
var
  Sections: TStrings;
  Index: integer;

begin
  Sections := Local(TStringList.Create).Obj as TStrings;
  ReadSections(Sections);

  Index := Sections.IndexOf(Value);
  if (Index < 0) then begin
    Result := nil;
  end
  else begin
    Result := TStrings(Sections.Objects[Index]);
  end;
end;





{ TDBICustomIniFilePersistenceAdapter }

const
  ParentAttribute = '.parent';
  
destructor TDBICustomIniFilePersistenceAdapter.Destroy;
begin
  FStrings.Free;
  FStrings := nil;

  inherited Destroy;
end;

function TDBICustomIniFilePersistenceAdapter.GetFileName: TFileName;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

function TDBICustomIniFilePersistenceAdapter.GetStrings: TStrings;
begin
  if not Assigned(FStrings) then begin
    FStrings := TStringList.Create;
  end;
  Result := FStrings;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2008 07:43:00 - Initial code.<br />
}
function TDBICustomIniFilePersistenceAdapter.ToString: String;
var
  Index: Integer;

  procedure WriteComponent(Item: TComponent; ItemIndex: Integer = 0);
  begin
    if (Item.Name <> '') then begin
      Strings.Add(Format('[%s = %s]', [Item.Name, Item.ClassName]));
      if (ItemIndex = -1) then begin
        Strings.Add(ParentAttribute + '=-1');
      end;
      TDBIProperties.GetProperties(Item, Strings);
    end;
  end;

begin
  Strings.Clear;

  WriteComponent(RootComponent, -1);
  for Index := 0 to RootComponent.ComponentCount-1 do begin
    WriteComponent(RootComponent.Components[Index]);
  end;

  Result := Strings.Text;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2008 07:43:32 - Initial code.<br />
}
function TDBICustomIniFilePersistenceAdapter.LoadFromFile(AFileName: TFileName): TComponent;
const
  LineBreak = #13#10;
  Quote = #39;

var
  IniFile: TDBIIniFile;
  SectionIndex: Integer;
  SectionNames: TStrings;
  Stream: TMemoryStream;

  procedure WriteFmt(const Data: String; Args: array of const);
  var
    Output: String;
  begin
    Output := Format(Data + LineBreak, Args);
    Stream.Write(PAnsiChar(AnsiString(Output))^, Length(Output));
  end;
  
  procedure WriteProperty(const PropName, PropValue, PropClassName: String);
  var
    PropInfo: PPropInfo;
    Output: String;
  begin
    Output := '';
    PropInfo := GetPropInfo(GetClass(PropClassName), PropName);
    if (PropInfo <> nil) and (PropValue <> '') then begin
      case PropInfo^.PropType^.Kind of
        tkChar, tkWChar, tkString, tkLString, tkWString{$ifdef DELPHIxe2}, tkUString{$endif}:
          Output := Format('    %s = %s%s%s%s', [PropName, Quote, PropValue, Quote, LineBreak]);
      else
        Output := Format('    %s = %s%s', [PropName, PropValue, LineBreak]);
      end;
    end;

    Stream.Write(PAnsiChar(AnsiString(Output))^, Length(Output));
  end;

  function WriteComponent(const AComponentName: String): Integer;
  var
    Section: TStrings;
    InstanceClassName: String;
    KeyIndex: Integer;
    KeyName, KeyValue: String;

  begin
    Result := 0;
    
    Section := IniFile.GetSection(AComponentName);
    if (Section.Count > 0) then begin
      // Check if this component is the Parent
      Result := StrToIntDef(Section.Values[ParentAttribute], 0);

      KeyName := SectionNames.Names[SectionIndex];
      InstanceClassName := SectionNames.Values[KeyName];
      WriteFmt('  object %s: %s', [KeyName, InstanceClassName]);

      for KeyIndex := 0 to Section.Count-1 do begin
        KeyName := Section.Names[KeyIndex];
        KeyValue := Section.Values[KeyName];
        WriteProperty(KeyName, KeyValue, InstanceClassName);
      end;

      if (Result > -1) then begin
        WriteFmt('  end', []);
      end;
    end;
  end;

begin
  Result := nil;

  if (AFileName = '') then begin
    AFilename := GetFileName;
  end;

  if FileExists(AFileName) then begin
    IniFile := Local(TDBIIniFile.Create(AFileName)).Obj as TDBIIniFile;
    SectionNames := Local(TStringList.Create).Obj as TStrings;
    Stream := Local(TMemoryStream.Create).Obj as TMemoryStream;

    IniFile.ReadSections(SectionNames);
    if SectionNames.Count > 0 then begin
      WriteComponent(SectionNames[0]);
      for SectionIndex := 1 to SectionNames.Count-1 do begin
        WriteComponent(SectionNames[SectionIndex]);
      end;
      WriteFmt('end', []);
    end;

{$ifdef UseDebugInfo}
    Stream.Position := 0;
    Stream.SaveToFile(ChangeFileExt(AFileName, '.dfm.txt'));
{$endif}

    Stream.Position := 0;
    Result := LoadFromStream(Stream);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/09/2011 19:16:00 - Initial code.<br />
}
function TDBICustomIniFilePersistenceAdapter.SaveToFile(AFileName: TFileName = ''): Boolean;
var
  Index: Integer;
  IniFile: TDBIIniFile;

  procedure WriteComponent(Item: TComponent; ItemIndex: Integer = 0);
  begin
    Assert(Item.Name <> '');
    
    if (Item.Name <> '') then begin
      Strings.Clear;
      if (ItemIndex = -1) then begin
        Strings.Add(ParentAttribute + '=-1');
      end;
      TDBIProperties.GetProperties(Item, Strings);
      IniFile.WriteSectionValues(Item.Name + '=' + Item.ClassName, Strings);
    end;
  end;

begin
  Result := Assigned(RootComponent);
  if Result then begin
    if (AFileName = '') then begin
      AFilename := GetFileName;
    end;

    IniFile := Local(TDBIIniFile.Create(AFileName)).Obj as TDBIIniFile;
    IniFile.Clear;

    WriteComponent(RootComponent, -1);
    if (RootComponent.ComponentCount > 0) then begin
      for Index := 0 to RootComponent.ComponentCount-1 do begin
        WriteComponent(RootComponent.Components[Index]);
      end;
    end;

    IniFile.UpdateFile;
  end;
end;





{ TDBICustomPersistenceAdapter }

// _____________________________________________________________________________
{**
  Jvr - 01/09/2008 07:43:00 - Initial code.<br />
}
procedure TDBICustomPersistenceAdapter.BeginUpdate;
begin
  //##NOP
end;  { BeginUpdate }


// _____________________________________________________________________________
{**
  Jvr - 01/09/2008 07:43:32 - Initial code.<br />
}
procedure TDBICustomPersistenceAdapter.EndUpdate;
begin
  //##NOP
end;  { EndUpdate }


(*##CLASSES
procedure TDBICustomPersistenceAdapter.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  Index: Integer;
  Component: TComponent;

begin
  for Index := 0 to ComponentCount - 1 do begin
    Component := Components[Index];
    if (Component.Owner = Root) then begin
      Proc(Component);
    end;
  end;
end;
//*)


// _____________________________________________________________________________
{**
  Jvr - 30/11/2010 15:47:20 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.GetDisplayName: string;
begin
  Result := GetText;

  if (Result = '') then begin
    Result := inherited GetDisplayName;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 21/12/2010 11:35:13 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.GetFileName: TFileName;
begin
  Result := ChangeFileExt(ParamStr(0), '.dfm');
end;


// _____________________________________________________________________________
{**
  Jvr - 28/08/2008 10:10:50 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.GetRootComponent: TComponent;
begin
  if Assigned(FRootComponent) then begin
    Result := FRootComponent;
  end
  else begin
    Result := inherited GetRootComponent;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 02/02/2012 14:59:44 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.GetText: String;
begin
  if (FText <> '') then begin
    Result := FText;
  end
  else begin
    Result := Name;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/11/2010 10:47:23 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.IsTextStored: Boolean;
begin
  Result := (FText <> '') and (CompareText(FText, Name) <> 0);
end;


// _____________________________________________________________________________
{**
  Jvr - 28/08/2008 12:06:20 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.LoadFromFile(AFileName: TFileName = ''): TComponent;
begin
  if (AFileName = '') then begin
    AFilename := GetFileName;
  end;

  Result := LoadFromStream(Local(TFileStream.Create(AFileName, fmOpenRead)).Obj as TStream);
end;


// _____________________________________________________________________________
{**
  Jvr - 07/12/2010 09:14:08 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.LoadFromResource(RootAncestor: TClass = nil): TComponent;
begin
  GlobalNameSpace.BeginWrite;
  try
    if not Assigned(RootAncestor) then begin
      RootAncestor := RootComponent.ClassType;
    end;

    Result := RootComponent;

    if not InitInheritedComponent(RootComponent, RootAncestor) then begin
      raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 28/08/2008 12:03:03 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.LoadFromStream(AStream: TStream): TComponent;
var
  MemoryStream: TMemoryStream;
  Reader: TReader;

begin
  MemoryStream := Local(TMemoryStream.Create).Obj as TMemoryStream;
  ObjectTextToBinary(AStream, MemoryStream);
  MemoryStream.Position := 0;
  Reader := Local(TReader.Create(MemoryStream, 4096)).Obj as TReader;

  BeginUpdate;
  try
    Result := Reader.ReadRootComponent(RootComponent);
  finally
    EndUpdate;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 28/08/2008 12:05:00 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.SaveToFile(AFileName: TFileName): Boolean;
begin
  if (AFileName = '') then begin
    AFileName := GetFileName;
  end;

  Result := Assigned(
    SaveToStream(Local(TFileStream.Create(AFileName, fmCreate)).Obj as TStream)
    );
end;


// _____________________________________________________________________________
{**
  Jvr - 28/08/2008 11:51:42 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.SaveToStream(AStream: TStream): TStream;
var
  MemoryStream: TMemoryStream;

begin
  MemoryStream := Local(TMemoryStream.Create).Obj as TMemoryStream;

  BeginUpdate;
  try
    MemoryStream.WriteComponent(RootComponent);
    MemoryStream.Position := 0;
    ObjectBinaryToText(MemoryStream, AStream);
    Result := AStream;
  finally
    EndUpdate;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 30/11/2010 16:02:32 - Initial code.<br />
}
procedure TDBICustomPersistenceAdapter.SetRootComponent(Value: TComponent);
begin
  FRootComponent := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 22/12/2010 12:36:18 - Initial code.<br />
}
procedure TDBICustomPersistenceAdapter.SetText(const Value: String);
begin
  FText := Value;
end;


// _____________________________________________________________________________
{**
  Jvr - 13/09/2011 19:16:00 - Initial code.<br />
}
function TDBICustomPersistenceAdapter.ToString: String;
begin
  Result := (SaveToStream(Local(TStringStream.Create('')).Obj as TStream) as TStringStream).DataString;
end;





{ TDBICompoundComponent }

// _____________________________________________________________________________
{**
  Jvr - 09/09/2010 21:48:50 - Initial code.<br />
}
procedure TDBICompoundComponent.ChangeParent(AParent: TDBICompoundComponent);
begin
  FParent := AParent;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:49:18 - Initial code.<br />
}
constructor TDBICompoundComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ChangeParent(nil);
end;


// _____________________________________________________________________________
{**
  Jvr - 02/09/2010 21:19:44 - Initial code.<br />
}
constructor TDBICompoundComponent.Create(AOwner: TComponent; AParent: TDBICompoundComponent);
begin
  inherited Create(AOwner);

  if Assigned(AParent) then begin
    AParent.InsertChild(Self);
  end;

  Name := FindUniqueName;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:49:42 - Initial code.<br />
}
destructor TDBICompoundComponent.Destroy;
begin
  SetParent(nil);
  FreeAndNil(FChildren);
  
  inherited Destroy;
end;


function TDBICompoundComponent.FindUniqueName: String;
var
  ID: Integer;

begin
  ID := ComponentIndex;

  if (ID < 1) then begin
    ID := 1;
  end;

  repeat
    Result := Copy(Self.ClassName, 2, 255) + IntToStr(ID);

    Inc(ID);
  until (Owner = nil) or (Owner.FindComponent(Result) = nil);
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:11:41 - Initial code.<br />
}
function TDBICompoundComponent.GetChild(const Index: Integer): TDBICompoundComponent;
begin
  Result := nil;
  if (Index >= 0) and (Index < ChildList.Count) then begin
    Result := ChildList[Index];
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:19:35 - Initial code.<br />
}
function TDBICompoundComponent.GetChildCount: Integer;
begin
  Result := ChildList.Count;
end;


// _____________________________________________________________________________
{**
  Jvr - 18/11/2010 22:52:19 - Initial code.<br />
}
function TDBICompoundComponent.GetChildIndex: Integer;
begin
  if (Parent <> nil) then begin
    Result := Parent.ChildList.IndexOf(Self);
  end
  else begin
    Result := -1;
  end;
end;  { GetChildIndex }


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:33:34 - Initial code.<br />
}
function TDBICompoundComponent.GetChildList: TList;
begin
  if (FChildren = nil) then begin
    FChildren := TList.Create;
  end;
  Result := FChildren;

  Assert(Assigned(Result));
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:46:46 - Initial code.<br />
}
procedure TDBICompoundComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  Index: Integer;
  Component: TComponent;

begin
  for Index := 0 to ChildCount - 1 do begin
    Component := Children[Index];
    Proc(Component);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 10:03:44 - Initial code.<br />
}
function TDBICompoundComponent.GetCompoundName: String;
var
  ObjectPath: String;

begin
  Result := Name;
  if (GetOwner is TDBICompoundComponent) then begin
    ObjectPath := TDBICompoundComponent(GetOwner).GetCompoundName;
     if (ObjectPath <> '') then begin
      Result := ObjectPath + '.' + Result;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 02/09/2010 11:03:45 - Initial code.<br />
}
function TDBICompoundComponent.GetDisplayName: string;
begin
  Result := Name;
  if (Result = '') then begin
    Result := ClassName;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 03/02/2011 11:22:52 - Initial code.<br />
}
function TDBICompoundComponent.GetMemberComponent(
  const ACompoundName: String;
  MinClass: TClass = nil
  ): TComponent;

  function GetOwner: TComponent;
  begin
    Result := Owner;
    if not Assigned(Result) then begin
      Result := Self;
    end;
  end;

var
  Item: TComponent;
  ItemName: String;
  ItemIndex: Integer;

begin
  Result := nil;
  for ItemIndex := 0 to GetOwner.ComponentCount-1 do begin
    Item := GetOwner.Components[ItemIndex];
    if (MinClass = nil) or (Item is MinClass) then begin
      ItemName := (Item as TDBICompoundComponent).CompoundName;
      if CompareText(ItemName, ACompoundName) = 0 then begin
        Result := GetOwner.Components[ItemIndex];
        Break;
      end;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:57:38 - Initial code.<br />
}
function TDBICompoundComponent.GetParentComponent: TComponent;
begin
  Result := FParent;
end;


// _____________________________________________________________________________
{**
  Jvr - 08/03/2011 12:16:57 - Initial code.<br />
}
function TDBICompoundComponent.GetRootComponent: TComponent;
  function FindRoot(Item: TDBICompoundComponent): TDBICompoundComponent;
  begin
    if (Item.Parent is TDBICompoundComponent) and Assigned(Item.Parent.Owner) then begin
      Result := FindRoot(Item.Parent);
     end
    else begin
      Result := Item;
    end;
 end;
begin
  Result := FindRoot(Self);
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:56:45 - Initial code.<br />
}
function TDBICompoundComponent.HasParent: Boolean;
begin
  Result := FParent <> nil;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:38:14 - Initial code.<br />
}
procedure TDBICompoundComponent.InsertChild(AComponent: TDBICompoundComponent);
begin
  if (AComponent is TDBICompoundComponent) then begin
    ChildList.Add(AComponent);
    TDBICompoundComponent(AComponent).ChangeParent(Self);
    TDBICompoundComponent(AComponent).SetParentReference(True);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:51:15 - Initial code.<br />
}
procedure TDBICompoundComponent.ReadState(Reader: TReader);
begin
  if (Reader.Parent is TDBICompoundComponent) then begin
    Parent := TDBICompoundComponent(Reader.Parent);
  end;

  inherited ReadState(Reader);
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:41:49 - Initial code.<br />
}
procedure TDBICompoundComponent.RemoveChild(AComponent: TDBICompoundComponent);
begin
  if (AComponent is TDBICompoundComponent) then begin
    TDBICompoundComponent(AComponent).SetParentReference(False);
    ChildList.Remove(AComponent);
    TDBICompoundComponent(AComponent).ChangeParent(nil);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 18/11/2010 22:55:52 - Initial code.<br />
}
resourcestring
  SPageIndexError =
    '%d is an invalid ChildIndex value.  ChildIndex must be between 0 and %d';

procedure TDBICompoundComponent.SetChildIndex(const Value: Integer);
var
  MaxPageIndex: Integer;

begin
  if (Parent <> nil) then begin
    MaxPageIndex := Parent.ChildList.Count - 1;

    if (Value > MaxPageIndex) then begin
      raise EListError.CreateResFmt(@SPageIndexError, [Value, MaxPageIndex]);
    end;
    Parent.ChildList.Move(ChildIndex, Value);
  end;  { if }
end;


// _____________________________________________________________________________
{**
  Jvr - 07/09/2010 21:35:31 - Initial code.<br />

type
  TXiProtectedComponent = class(TComponent);

procedure TXiCompoundComponent.SetName(const NewName: TComponentName);
begin
  if (Name <> NewName) then begin
    if (NewName <> '') and not IsValidIdent(NewName) then begin
      raise EComponentError.CreateResFmt(@SInvalidName, [NewName]);
    end;

    if (Parent <> nil) then begin
      TXiProtectedComponent(Parent).ValidateRename(Self, Name, NewName);
    end
    else begin
      ValidateRename(nil, Name, NewName);
    end;

    SetParentReference(False);
    ChangeName(NewName);
    SetParentReference(True);
  end;
end;
//}

// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:44:54 - Initial code.<br />
}
procedure TDBICompoundComponent.SetParent(AParent: TDBICompoundComponent);
begin
  if (FParent <> AParent) then begin
    if (AParent = Self) then begin
      raise EInvalidOperation.CreateRes(@SControlParentSetToSelf);
    end;

    if (FParent <> nil) then begin
      FParent.RemoveChild(Self);
    end;

    if (AParent <> nil) then begin
      AParent.InsertChild(Self);
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 01/09/2010 09:53:29 - Initial code.<br />
}
procedure TDBICompoundComponent.SetParentComponent(Value: TComponent);
begin
  if (Parent <> Value) and (Value is TDBICompoundComponent) then begin
    SetParent(TDBICompoundComponent(Value));
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/12/2010 19:00:33 - Initial code.<br />
}
procedure TDBICompoundComponent.SetParentReference(Enable: Boolean);
var
  Field: ^TComponent;

begin
  if (Parent <> nil) then begin
    Field := Parent.FieldAddress(Name);
    if Field <> nil then begin
      if Enable then begin
        Field^ := Self;
      end
      else begin
        Field^ := nil;
      end;
    end;
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 07/09/2010 21:38:30 - Initial code.<br />
}
procedure TDBICompoundComponent.SetReference(Enable: Boolean);
var
  Field: ^TComponent;

begin
  if (Owner <> nil) then begin
    Field := Owner.FieldAddress(Name);
    if Field <> nil then begin
      if Enable then begin
        Field^ := Self;
      end
      else begin
        Field^ := nil;
      end;
    end;
  end;
end;



initialization
  Classes.RegisterClass(TDBIPersistentComponent);
  Classes.RegisterClass(TDBIPersistenceAdapter);
  Classes.RegisterClass(TDBIIniFilePersistenceAdapter);

end.