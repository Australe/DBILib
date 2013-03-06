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
  1.0 | 05/10/2003 10:16:11 | Jvr | Initial Release
  ______________________________________________________________________________
}

unit DBIFields;

interface

uses
  DBIConfigs, Classes, SysUtils, Contnrs, TypInfo, DB, DBIUtils;

type
  TDBIPersistentFields = class(TDBIIniConfiguration)
  private
    FFields: TObjectList;

  public
    constructor Create(
      const ASectionName: String = DefaultSectionName;
      CreateOptions: TomConfigOptions = [coLoadFromIniFile, coLoadFromCommandLine];
      const AFileName: String = '';
      const ACommandLine: String = ''
      ); override;

    destructor Destroy; override;

    procedure CopyTo(Fields: TFields);
    procedure CopyProperties(Source: TObject; Destination: TObject);
    function MakeObject(ClassRef: TClass): TObject; override;

  published
    property FieldList: TObjectList read FFields write FFields;

  end;  { TDBIPersistentFields }


  TDBINoSpacesStringField = class(TStringField)
  protected
    procedure SetAsString(const Value: string); override;
  end;  { TDBINoSpacesStringField }


  TDBIFieldMetadata = class(TPersistent)
  private
    FViewName: String;
    FFieldName: String;
    FDomain: String;
    FProperties: TStrings;
    FTag: Integer;

  public
//##JVR    constructor Create;
//##JVR    destructor Destroy; override;

  published
    property ViewName: String read FViewName write FViewName;
    property FieldName: String read FFieldName write FFieldName;
    property Domain: String read FDomain write FDomain;
    property Properties: TStrings read FProperties;
    property Tag: Integer read FTag write FTag;

  end;  { TDBIFieldMetadata }


implementation


// _____________________________________________________________________________
{**
  Jvr - 23/05/2002 12:39:26.<P>
}
constructor TDBIPersistentFields.Create(
  const ASectionName: String;
  CreateOptions: TomConfigOptions;
  const AFileName: String;
  const ACommandLine: String
  );
begin
  FFields := TObjectlist.Create;

  inherited Create;
end;  { Create }


// _____________________________________________________________________________
{**
  Jvr - 23/05/2002 13:46:30.<P>
}
function TDBIPersistentFields.MakeObject(ClassRef: TClass): TObject;
begin
  Result := TFieldClass(ClassRef).Create(nil);
end;  { MakeObject }


// _____________________________________________________________________________
{**
  Jvr - 23/05/2002 12:40:21.<P>
}
destructor TDBIPersistentFields.Destroy;
begin
  FFields.Free;
  FFields := nil;

  inherited Destroy;
end;  { Destroy }


// _____________________________________________________________________________
{**
  Jvr - 23/05/2002 13:07:16.<P>
}
procedure TDBIPersistentFields.CopyTo(Fields: TFields);
var
  Index: Integer;
  Field: TField;
  NewField: TField;

begin
  Assert(Fields <> nil);
  Assert(FFields.Count <> 0);
  Fields.Clear;

  for Index := FFields.Count -1 downto 0 do begin
    Field := FFields[Index] as TField;

    NewField := TFieldClass(Field.ClassType).Create(Fields.Dataset);
    NewField.FieldName := Field.FieldName;
    NewField.Dataset := Fields.Dataset;

    CopyProperties(Field, NewField);
  end;
end;


// _____________________________________________________________________________
{**
  Jvr - 23/05/2002 15:22:21.<P>
}
procedure TDBIPersistentFields.CopyProperties(
  Source: TObject;
  Destination: TObject
  );
var
  PropertyList: TList;
  PropInfo: PPropInfo;
  Index: Integer;
  Proceed: Boolean;
  Data: String;
  Value: Integer;

begin
  PropertyList:= TList.Create;
  try
    DBIGetPropertyList(Source.ClassInfo, PropertyList);
    for Index := 0 to PropertyList.Count - 1 do begin
      PropInfo := PropertyList[Index];

      Proceed :=
        IsPublishedProp(Destination, PropInfo.Name)// Dest has matching property
        and Assigned(PropInfo.SetProc)             // Dest property is writeable
        and Assigned(PropInfo.GetProc);            // Source property is readable

      if not Proceed then begin
        Continue;
      end;

      case PropInfo^.PropType^.Kind of
        tkString,
        tkWstring,
        tkLString:  begin
          Data := GetStrProp(Source, PropInfo.Name);
          SetStrProp(Destination, PropInfo.Name, Data);
        end;

        tkInteger: begin
          Value := GetOrdProp(Source, PropInfo.Name);
          SetOrdProp(Destination, PropInfo.Name, Value);
        end;

        tkInt64: begin
          SetInt64Prop(Destination, PropInfo.Name, GetInt64Prop(Source, PropInfo.Name));
        end;

        tkFloat: begin
          SetFloatProp(Destination, PropInfo.Name, GetFloatProp(Source, PropInfo.Name));
        end;

        tkChar: begin
          SetOrdProp(Destination, PropInfo.Name, GetOrdProp(Source, PropInfo.Name));
        end;

        tkEnumeration: begin
          SetEnumProp(Destination, PropInfo.Name, GetEnumProp(Source, PropInfo.Name));
        end;

        tkSet: begin
          SetSetProp(Destination, PropInfo.Name, GetSetProp(Source, PropInfo.Name));
        end;

        tkClass: begin
(*
          // Write the section/sectionvalues for 'PropInfo.Name'
          ObjectReference := GetObjectProp(Self, PropInfo, TClass(nil));

          if (ObjectReference is TStrings) then begin
            SaveStringsToIniFile(
              ObjectReference as TStrings,
              PropInfo.Name,
              ASectionName,
              AIniFile
              );
          end

          else if (ObjectReference is TObjectList) then begin
            SaveObjectListToIniFile(
              ObjectReference as TObjectList,
              PropInfo.Name,
              ASectionName,
              AIniFile
              );
          end

          else if (ObjectReference is TObject) then begin
            // First write the referred section to the inifile
            ObjectName := GetStrProp(ObjectReference, 'Name');
            if (ObjectName = '') then begin
              Error(EPropertyError,
                'Property "%s.%s" either has no value or does not exist.',
                [ObjectReference.ClassName, 'Name']
                );
            end;
            AIniFile.WriteString(ASectionName, PropInfo.Name, ObjectName);

            // Now write the actual object to the inifile
            SaveObjectToIniFile(ObjectReference, ObjectName, AIniFile);
          end;
*)
        end;  { tkClass }

      end;  { case }
    end;  { for }
  finally
    FreeAndNil(PropertyList)
  end;  { try..finally }
end;




procedure TDBINoSpacesStringField.SetAsString(const Value: string);
begin
  if Pos(' ', Value) > 0 then begin
    DatabaseError('No spaces allowed in field');
  end;

  inherited SetAsString(Value);
end;



initialization
  // Register the Tfield classes
  RegisterClass(TStringField);
  RegisterClass(TBooleanField);
  RegisterClass(TIntegerField);
  RegisterClass(TFloatField);
  RegisterClass(TBlobField);
  RegisterClass(TMemoField);
  RegisterClass(TDBINoSpacesStringField);


  RegisterClass(TDBIFieldMetadata);
end.
