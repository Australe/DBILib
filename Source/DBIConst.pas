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
  1.0 | 02/04/2001 15:07:18 | Jvr | Initial Release
  1.0 | 02/04/2001 15:07:18 | Jvr | Removed Hooked Exceptions
  ______________________________________________________________________________
}

unit DBIConst;

{$I DBICompilers.inc}

interface

// Disable Typed @ operator, Controls the type of pointer returned by @ operator.
{$T-}

uses
  SysUtils, DB;

const
  // Library (Package) Name
  DBILibraryName = 'DBILib';
  DBIComponentPage = 'Data Access';

type
  TDBIObjectClass = class of TObject;

  EDBICustomException = class(Exception)
  protected
    class function GetDebugInfo(const Context: String): String; overload; virtual;
    class function GetDebugInfo(Sender: TObject; const Context: String): String; overload; virtual;
    class function GetDebugInfo(Sender: TDBIObjectClass; const Context: String): String; overload;
    class function GetExceptionInfo(E: Exception): String;
    class function GetUnitName(Sender: TObject): String; overload;
    class function GetUnitName(Sender: TDBIObjectClass): String; overload;

  public
    constructor Create(const Context, Msg: String; Args: array of const); overload;

    constructor Create(Sender: TObject; const Context: String); overload;
    constructor Create(Sender: TObject; const Context, Msg: String; Args: array of const); overload;
    constructor Create(Sender: TObject; E: Exception; const Context, Msg: String; Args: array of const); overload;

    constructor Create(Sender: TDBIObjectClass; const Context, Msg: String; Args: array of const); overload;
    constructor Create(Sender: TDBIObjectClass; E: Exception; const Context, Msg: String; Args: array of const); overload;
  end;

  EDBIException = class(EDBICustomException);

  EDBINotImplementedException = class(EDBIException)
  protected
    class function GetDebugInfo(Sender: TObject; const Context: String): String; overload; override;
  end;


type
  // Metadata modes
  TDBIMetaDataMode = (mdHeaderOnly, mdNew, mdAll);

  // DBI Stream Modes
  TDBIStreamMode = (smMemoryStream, smFileStream, smExternalStream, smLoadFromStream, smLoadFromFile);

  // This enumerated type indicates what is responsible for the lifetime of
  // the created list for all TDBICustomListDataConnection derived classes
  TDBIListDataConnectionListMode = (lmNone, lmInternal, lmExternal);

  // These Options are specific to
  // all TDBICustomListDataConnection derived classes
  TDBIListDataConnectionOption = (
    osStringFieldsDefaultToAnsi, // This option is solely available for Unicode
                                 // purposes.  In more recent versions of Delphi,
                                 // The "String" data type defaults to "WideString"
                                 // as appossed to "AnsiString", which is the
                                 // default for earlier versions of Delphi.
                                 // This causes problems for datasets when
                                 // accessing and / or storing of strings.
                                 // The default for existing applications ussally
                                 // is AnsiString, causing a conflict when binding
                                 // to the dataset TFields.  Use this property to
                                 // force the dataset to default back to AnsiString
                                 // Default = ON

    osErrorOnReadOnlyProperty,   // If 'osErrorOnReadOnlyProperty' is set, then
                                 // a readonly exception is raised when attempting
                                 // to update a readonly property from the dataset.
                                 // If NOT set the dataset doesn't attempt to
                                 // update readonly properties.
                                 // Default = OFF

    osObjectValidation           // If set, use Object properties for field validation
                                 //   and/or
                                 // use the Object validation procedure for validation
                                 //
                                 // Object properties can also be initialised in
                                 // the TObjectListDataset.OnCreateObject event
                                 // The object properties for the validation
                                 // object will also be kept in sync with the
                                 // Dataset field buffers. (The validation
                                 // object properties require the same values
                                 // as in the dataset edit field buffers)
                                 // Default = OFF
{
    osObjectWriteThrough         // Allows setting of fields through the dataset
                                 // to write directly to the actual object
                                 // without any buffering. A consequence of this
                                 // is that any property Setter/Getter methods
                                 // will be called on a field by field basis and
                                 // therefore field validation will automatically
                                 // be applied.
                                 // To keep things consistent the
                                 // 'osObjectValidation' option will be implicitly
                                 // set when 'osObjectWriteThrough' is set.
                                 // The object validation proc will therefore be
                                 // called when posting a record.
                                 // NOTE: When 'osObjectWriteThrough' is set the
                                 // only effect posting has on an object is to
                                 // validate the object and to revert the object
                                 // back to it's original state if the user
                                 // 'Cancels' the edit.
                                 // When inserting a new record (object) into the
                                 // dataset, it is NOT added to the underlying
                                 // list until the record (object) has been
                                 // successfully posted.
                                 // If a new object was inserted and 'Cancelled'
                                 // (via the dataset) then the object will be
                                 // destroyed.
                                 // Default = OFF
}
    );

  TDBIListDataConnectionOptions = set of TDBIListDataConnectionOption;

const
  osDefaultListDataConnectionOptions = [osStringFieldsDefaultToAnsi];

type
  TDBICreateObjectEventType = (coNormal, coValidate);

  TDBIObjectDataEvent = (deEdit, dePost, deCancel);

  TDBIOnCreateObjectEvent = procedure(
    Sender: TObject;
    var DataObject: TObject;
    const ClassTypeName: String;
    EventType: TDBICreateObjectEventType
    ) of object;

type
  TDBIDataFormat = (dfDefault, dfXbase, dfXbasePlus, dfXML, dfCSV, dfJSON);
  TDBIOpenMode = (omOpen, omCreateDataset, omClose);

const
  // Null/Blank Datetime representation
  // See SysUtils: MinDateTime & MaxDateTime
  // NOTE: Use typed constants here otherwise you might get rounding
  //   errors in date comparisons.
{$ifdef DELPHI6}
  C_DBISignifyNullDateTime = -657434.0; //##JVR - this one matches "SysUtils.DateDelta" -693593.0;
  DBIMinDateTime: TDateTime = C_DBISignifyNullDateTime;         { 01/01/0100 12:00:00.000 AM = $FFF5F7E6 }
  DBIMaxDateTime: TDateTime = 2958465.99999;     { 12/31/9999 11:59:59.999 PM = $002D2482 }
{$else}
  DBIMinDateTime = -693593.0;         { 01/01/0001 12:00:00.000 AM = $FFF56AA7 }
  DBISignifyNullDateTime = -693593.0; //-657434.0; { Must be the same as DBIMinDateTime }
  DBIMaxDateTime = 2958465.99999;     { 12/31/9999 11:59:59.999 PM = $002D2482 }
{$endif}

  DBIZeroDateTime = -693594.0;      { 00/00/0000 12:00:00.000 AM = $FFF56AA6 }

type
  PDateTimeRec = ^TDateTimeRec;     { see DB.pas for TDateTimeRec record definition }



implementation

uses
  TypInfo;

{$ifdef fpc}
  {$define DebugExceptions True}
{$endif}


{ EDBINotImplementedException }

class function EDBINotImplementedException.GetDebugInfo(Sender: TObject; const Context: String): String;
begin
  Result := inherited GetDebugInfo(Sender, Context) + 'Not Implemented Yet!';
end;





{ EDBICustomException }

constructor EDBICustomException.Create(const Context, Msg: String; Args: array of const);
begin
  inherited CreateFmt(GetDebugInfo(Context) + Msg, Args);
end;


constructor EDBICustomException.Create(Sender: TObject; const Context: String);
begin
  inherited Create(GetDebugInfo(Sender, Context));
end;


constructor EDBICustomException.Create(Sender: TObject; const Context, Msg: String; Args: array of const);
begin
  inherited CreateFmt(GetDebugInfo(Sender, Context) + Msg, Args);
end;


constructor EDBICustomException.Create(Sender: TObject; E: Exception; const Context, Msg: String; Args: array of const);
begin
  inherited CreateFmt(GetDebugInfo(Sender, Context) + Msg + GetExceptionInfo(E), Args);
end;


constructor EDBICustomException.Create(Sender: TDBIObjectClass; const Context, Msg: String; Args: array of const);
begin
  inherited CreateFmt(GetDebugInfo(Sender, Context) + Msg, Args);
end;


constructor EDBICustomException.Create(Sender: TDBIObjectClass; E: Exception; const Context, Msg: String; Args: array of const);
begin
  inherited CreateFmt(GetDebugInfo(Sender, Context) + Msg + GetExceptionInfo(E), Args);
end;


class function EDBICustomException.GetDebugInfo(const Context: String): String;
begin
{$ifdef DebugExceptions}
  Result := Context + #13;
{$else}
  Result := '';
{$endif}
end;


class function EDBICustomException.GetDebugInfo(Sender: TObject; const Context: String): String;
begin
{$ifdef DebugExceptions}
  if Assigned(Sender) then begin
    Result := String(GetUnitName(Sender)) + '::' + Sender.ClassName + '::' + Context + #13;
  end
  else begin
    Result := Context + #13;
  end;
{$else}
  Result := '';
{$endif}
end;


class function EDBICustomException.GetDebugInfo(Sender: TDBIObjectClass; const Context: String): String;
begin
{$ifdef DebugExceptions}
  if Assigned(Sender) then begin
    Result := String(GetUnitName(Sender)) + '::' + Sender.ClassName + '::' + Context + #13;
  end
  else begin
    Result := Context + #13;
  end;
{$else}
  Result := '';
{$endif}
end;


class function EDBICustomException.GetExceptionInfo(E: Exception): String;
const
  DebugException = #13#13'raised exception %s with message'#13'%s';

begin
{$ifdef DebugExceptions}
  if Assigned(E) then begin
    Result := Format(DebugException, [E.ClassName, E.Message]);
  end;
{$else}
  Result := '';
{$endif}
end;


class function EDBICustomException.GetUnitName(Sender: TObject): String;
begin
  Result := '';
  if Assigned(Sender) then begin
    Result := String(TypInfo.GetTypeData(Sender.ClassInfo)^.UnitName);
  end;
end;


class function EDBICustomException.GetUnitName(Sender: TDBIObjectClass): String;
begin
  Result := '';
  if Assigned(Sender) then begin
    Result := String(TypInfo.GetTypeData(Sender.ClassInfo)^.UnitName);
  end;
end;





end.

