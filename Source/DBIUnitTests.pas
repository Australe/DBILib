{ ______________________________________________________________________________

  <H5>Copyright</H5>
  ObjectMastery Pty Ltd<BR>
  Copyright (C) 2001, All rights reserved.<!--

  Project:       Common
  Files(s):      DBIUnitTests.pas
  Classes:       ...
  Author:        John Vander Reest
  Purpose:       ...

  Notes:         None
  -->
  <P><H5>Change History</H5><CODE>
  ______________________________________________________________________________
  REL | DATE/TIME           | WHO | DETAILS
  1.0 | 06/02/2001 16:30:37 | Jvr | Initial Release
  ______________________________________________________________________________
  </CODE>
}

unit DBIUnitTests;

{#omcodecop off : jvr : native api code}

interface

uses
  Classes, Contnrs, DB, DBClient, DBIStrings, DBIIntfConsts, DBIObjectListDatasets,
{$ifdef omTesting}
  omTestSuites, omTestMastery;
{$else}
  TestFrameWork;

type
  TomTestSuite = TTestCase;
  ETestFailed = ETestFailure;
{$endif}

const
  dbfXDSTest = 'XDSTest.dbf';
  fptXDSTest = 'XDSTest.fpt';

  dbfXDSSave = 'XDSSave.dbf';
  fptXDSSave = 'XDSSave.fpt';
  
  dbfXDSLoad = 'XDSLoad.dbf';
  fptXDSLOad = 'XDSLoad.fpt';
  
  dbfODSTest = 'ODSTest.dbf';
  fptODSTest = 'ODSTest.fpt';

  dbfODSSave = 'ODSSave.dbf';
  fptODSSave = 'ODSSave.fpt';

  dbfXDSMem = 'XDSMem.dbf';
  fptXDSMem = 'XDSMem.fpt';
  
  cdsXDSDbug = 'XDSDbug.cds';
  cdsXDSTest = 'XDSTest.cds';
  cdsXDSSave = 'XDSSave.cds';

  cdsODSTest = 'ODSTest.cds';
  cdsODSSave = 'ODSSave.cds';

  dbfJobQueue = 'JobQueue.dbf';
  dbfInvTran = 'InvTran.dbf';

type
  TDBIClientDataset = class(TClientDataset)
  public
    property DSBase;
    property DSCursor;
  end;


  TDBIObjectListDataset = class(TObjectListDataset)
  public
    property DSBase;
    property DSCursor;
  end;


type
  TDBIUnitTests = class(TomTestSuite)
  private
    function GetParent: TomTestSuite;

  protected
{$ifndef omTesting}
    function GetTestTempDir: String;

    property TestTempDir: String read GetTestTempDir;
{$endif}

  public
    function DataPath(const FileName: String = ''): String;

    procedure ZapOGadTable;
    procedure ZapXGadTable;
    
    property Parent: TomTestSuite read GetParent;

  end;  { TDBIUnitTests }


const
  ftAnsiString = ftString;
  ftUnicodeString = {##JVR} ftString; //##JVR} ftWideString;

{$ifdef Delphi2009}
  ftDefaultString = = {##JVR} ftString; //##JVR} ftWideString;
{$else}
  ftDefaultString = ftString;
{$endif}


type
  PFieldRecord = ^TFieldRecord;
  TFieldRecord = record
    FieldName: String;
    FieldType: TFieldType;
    FieldSize: Integer;
    Precision: Integer;
  end;


  TBookData = class(TPersistent)
  private
    FSequence: Integer;
    FName: TDBIString;
    FAuthor: TDBIString;
    FPurchased: TDateTime;
    FPrice: Double;
    FCurrency: TDBIString;
    FRating: Integer;
    FApproved: Boolean;
    FComments: TDBIString;
    FNotes: TDBIString;
    FDetails: TDBIString;

  public
    class procedure AssertFields(ADataset: TDataset);
    class procedure CreateFieldDefs(ADataset: TDataset);
    class procedure OccupyFields(ADataset: TDataset);

    class procedure XbaseCreateTable(const AFileName: String);

  published
    property Sequence: Integer read FSequence write FSequence;
    property Name: TDBIString read FName write FName;
    property Author: TDBIString read FAuthor write FAuthor;
    property Purchased: TDateTime read FPurchased write FPurchased;
    property Price: Double read FPrice write FPrice;
    property Currency: TDBIString read FCurrency write FCurrency;
    property Rating: Integer read FRating write FRating;
    property Approved: Boolean read FApproved write FApproved;
    property Comments: TDBIString read FComments write FComments;
    property Notes: TDBIString read FNotes write FNotes;
    property Details: TDBIString read FDetails write FDetails;

  end;  { TBookData }


  TBookCategory = class(TPersistent)
  private
    FSequence: Integer;
    FCategoryName: TDBIString;
    FBooks: TObjectList;

  published
    property Sequence: Integer read FSequence write FSequence;
    property CategoryName: TDBIString read FCategoryName write FCategoryName;
    property Books: TObjectList read FBooks write FBooks;

  end;  { TBookCategory }


type
  TBookRecord = record
    Sequence: Integer;
    Name: TDBIString;
    Author: TDBIString;
    Purchased: TDBIString;
    Price: Double;
    Currency: TDBIString;
    Rating: Integer;
    Approved: Boolean;
    Comments: TDBIString;
    Notes: TDBIString;
    Details: TDBIString;
  end;  { TBookRecord }


const
  TBookFields: array[0..10] of TFieldRecord = (
    (FieldName: 'Sequence';  FieldType: ftInteger;  ),
    (FieldName: 'Name';      FieldType: ftString;   FieldSize: 50;),
    (FieldName: 'Author';    FieldType: ftString;   FieldSize: 50;),
    (FieldName: 'Purchased'; FieldType: ftDate;     ),
    (FieldName: 'Price';     FieldType: ftCurrency; Precision: 2;),
    (FieldName: 'Currency';  FieldType: ftString;   FieldSize: 3;),
    (FieldName: 'Rating';    FieldType: ftInteger;  ),
    (FieldName: 'Approved';  FieldType: ftBoolean;  ),
    (FieldName: 'Comments';  FieldType: ftString;   FieldSize: 250{300};),
    (FieldName: 'Notes';     FieldType: ftMemo;     ),
    (FieldName: 'Details';   FieldType: ftMemo;     )
  );


const
  dbfOBooks = 'oBooks.dbf';
  fptOBooks = 'oBooks.fpt';

  dbfXBooks = 'xBooks.dbf';
  fptXBooks = 'xBooks.fpt';

  dbfOBooksUpD8 = 'oBooksUpD8.dbf';
  fptOBooksUpD8 = 'oBooksUpD8.fpt';

  dbfXBooksUpD8 = 'xBooksUpD8.dbf';
  fptXBooksUpD8 = 'xBooksUpD8.fpt';
  cdsXBooksUpD8 = 'xBooksUpD8.cds';

  DateTimeFormat = 'DD/MM/YYYY HH:NN:SS';

  // ___________________________________________________________________________
  {**
    Data for the CREATION of the test dataset(s)
  }
  NewBooks: array[0..4] of TBookRecord = (
    (
      Sequence: 0;
      Name: 'Learn Delphi 2 Database Programming Today!';
      Author: 'Jeff Cogswell';
      Purchased: '21/11/1996 00:00:00';
      Price: 69.95;
      Currency: 'AUD';
      Rating: 1;
      Approved: False;
      Comments: 'Unable to form a valid opinion on this one';
      Notes:    'SQL, OOP, Client/Server and 32-bit Delphi';
      Details:  'Jeff Cogswell, a freelance writer and professional ' +
                'programmer, is a regular contributer to Windows Tech Journal ' +
                'and VB Tech Journal.';
    ), (
      Sequence: 1;
      Name: 'Secrets of Delphi 2';
      Author: 'Ray Lischner';
      Purchased: '02/02/1997 00:00:00';
      Price: 49.95;
      Currency: 'USA';
      Rating: 5;
      Approved: True;
      Comments: 'A terrific book on the advanced subjects of Delphi';
      Notes:    '';
      Details:  '';
    ), (
      Sequence: 2;
      Name: 'Teach Yourself Delphi in 21 Days';
      Author: 'Wozniewics & Shammas';
      Purchased: '04/03/1996 00:00:00';
      Price: 59.95;
      Currency: 'AUD';
      Rating: 1;
      Approved: False;
      Comments: 'Book for beginners';
      Notes:    'In just 21 days, you can become a proficient Delphi ' +
                'programmer! Using easy-to-understand examples and full code ' +
                'listings, you''ll learn how to implement the latest Delphi ' +
                'enhancements and Teach Yourself Delphi in 21 Days.';
      Details:  '';
    ), (
      Sequence: 3;
      Name: 'Hidden Paths of Delphi 3';
      Author: 'Ray Lischner';
      Purchased: '15/08/1997 00:00:00';
      Price: 39.95;
      Currency: 'USA';
      Rating: 5;
      Approved: True;
      Comments: 'A Great book to learn about the internals of the Delphi IDE';
      Notes:    'Hidden Paths of Delphi 3 cuts through the clutter of ' +
                'all-in-one Delphi tomes to present a focused treatment of ' +
                'a vital, yet undocumented, ' +
                'Delphi feature: The open Tools API.';
      Details:  'LEARN TO BUILD DELPHI 3 Wizards.'#13 +
                'A collection of classes for interfacing with Delphi''s IDE, ' +
                'the Open Tools API allows developers to program in ' +
                'a familiar language, namely Object Pascal. The Open Tools API ' +
                'enables Delphi developers to create wizards to automate the ' +
                'creation of new forms and projects, create new items in ' +
                'Delphi''s menu, intercat with Delphi''s code and form editors, ' +
                'and define new file systems for storing and ' +
                'retrieving source, form, and resource files.'#13#13 +
                'ABOUT THE AUTHOR:'#13 +
                'Ray Lischner has written articles for Delphi Informat and ' +
                'Delphi Developer''s Journal. His first book, ' +
                'Secrets of Delphi 2, is widely recognized as the most useful ' +
                'book for advanced Delphi programming.'#13 +
                'Ray teaches computer science at Oregon State University';
    ), (
      Sequence: 4;
      Name: 'Developing Custom Delphi Components';
      Author: 'Ray Konopka';
      Purchased: '21/07/1998 00:00:00';
      Price: 80.00;
      Currency: 'AUD';
      Rating: 3;
      Approved: True;
      Comments: 'A good beginners book to component development';
      Notes:    '';
      Details:  'Ray Konopka is the founder of Raize Software Solutions, ' +
                'Inc., a supplier of Delphi custom components, training, ' +
                'and consulting services.';
    )
  );


const
  // ___________________________________________________________________________
  {**
    Data UPDATE the test dataset(s)
  }
  UpdateBooks: array[0..4] of TBookRecord = (
    (
      Sequence: 0;
      Name: 'Learn Delphi 2 Database Programming Today!';
      Author: 'Jeff Cogswell';
      Purchased: '21/11/1996 00:00:00';
      Price: 69.95;
      Currency: 'AUD';
      Rating: 1;
      Approved: False;
      Comments: 'Unable to form a valid opinion on this one';
      Notes:    'SQL, OOP, Client/Server and 32-bit Delphi';
      Details:  'Jeff Cogswell, a freelance writer and professional ' +
                'programmer, is a regular contributer to Windows Tech Journal ' +
                'and VB Tech Journal.';
    ), (
      Sequence: 1;
      Name: 'Secrets of Delphi 2';
      Author: 'Ray Lischner';
      Purchased: '02/02/1997 00:00:00';
      Price: 99.95;
      Currency: 'AUD';
      Rating: 5;
      Approved: True;
      Comments: 'A terrific book on the advanced subjects of Delphi';
      Notes:    'Secrets of Delphi 2 exposes the mysteries of the ' +
                'undocumented features Delphi programmers must know to create ' +
                'high-quality, state-of-the art components and applications. ' +
                'From writing experts to mastering the internal messages in ' +
                'the Delphi base classes to controlling palette realization.';
      Details:  '';
    ), (
      Sequence: 2;
      Name: 'Teach Yourself Delphi in 21 Days';
      Author: 'Wozniewics & Shammas';
      Purchased: '04/03/1996 00:00:00';
      Price: 59.95;
      Currency: 'AUD';
      Rating: 1;
      Approved: False;
      Comments: 'Book for beginners';
      Notes:    'In just 21 days, you can become a proficient Delphi ' +
                'programmer! Using easy-to-understand examples and full code ' +
                'listings, you''ll learn how to implement the latest Delphi ' +
                'enhancements and Teach Yourself Delphi in 21 Days.';
      Details:  '';
    ), (
      Sequence: 3;
      Name: 'Hidden Paths of Delphi 3';
      Author: 'Ray Lischner';
      Purchased: '15/08/1997 00:00:00';
      Price: 39.95;
      Currency: 'USA';
      Rating: 5;
      Approved: True;
      Comments: 'A Great book to learn about the internals of the Delphi IDE';
      Notes:    'Hidden Paths of Delphi 3 cuts through the clutter of ' +
                'all-in-one Delphi tomes to present a focused treatment of ' +
                'a vital, yet undocumented, ' +
                'Delphi feature: The open Tools API.'#13#13 +
                'LEARN TO BUILD DELPHI 3 Wizards.'#13 +
                'A collection of classes for interfacing with Delphi''s IDE, ' +
                'the Open Tools API allows developers to program in ' +
                'a familiar language, namely Object Pascal. The Open Tools API ' +
                'enables Delphi developers to create wizards to automate the ' +
                'creation of new forms and projects, create new items in ' +
                'Delphi''s menu, intercat with Delphi''s code and form editors, ' +
                'and define new file systems for storing and ' +
                'retrieving source, form, and resource files.';
      Details:  'ABOUT THE AUTHOR:'#13 +
                'Ray Lischner has written articles for Delphi Informat and ' +
                'Delphi Developer''s Journal. His first book, ' +
                'Secrets of Delphi 2, is widely recognized as the most useful ' +
                'book for advanced Delphi programming.'#13 +
                'Ray teaches computer science at Oregon State University';
    ), (
      Sequence: 4;
      Name: 'Developing Custom Delphi Components';
      Author: 'Ray Konopka';
      Purchased: '21/07/1998 00:00:00';
      Price: 80.00;
      Currency: 'AUD';
      Rating: 4;
      Approved: True;
      Comments: 'A good beginners book for component development. Further-more';{ +
                'if possible it would be a good idea to purchase a more ' +
                'up-to-date version of the book. I believe that the latest ' +
                'version is for Delphi 3, but it may be difficult to get as ' +
                'it is already out of print.';}
      Notes:    'Developing Custom Delpi';
      Details:  '';
    )
  );

type
// _____________________________________________________________________________
{**
  Jvr - 06/02/2001 16:45:31.<P>
}
  TGad = class(TPersistent)
  private
    FID: Word;
    FGender: TDBIString;
    FYieldRate: Double;
    FAge: Integer;
    FValue: Double;
    FComment: WideString;
    FStatus: AnsiString;
    FDate: TDateTime;
    FTime: TDBIString;
    FCreated: TDateTime;

  protected
    procedure SetGender(const Value: TDBIString);

  public
    class procedure AssertBlank(ADataset: TDataset);
    class procedure AssertFields(ADataset: TDataset);
    class procedure ClearFields(ADataset: TDataset);
    class procedure CreateFields(ADataset: TDataset);
    class procedure CreateFieldDefs(ADataset: TDataset);
    class procedure UpdateFields(ADataset: TDataset);

  published
    property ID: Word read FID write FID;
    property Age: Integer read FAge write FAge;
    property Gender: TDBIString read FGender write SetGender;
    property YieldRate: Double read FYieldRate write FYieldRate;
    property Value: Double read FValue write FValue;
    property Comment: WideString read FComment write FComment;
    property Status: AnsiString read FStatus write FStatus;
    property Date: TDateTime read FDate write FDate;
    property Time: TDBIString read FTime write FTime;
    property Created: TDateTime read FCreated write Fcreated;

  end;  { TGad }


  TGadRecord = record
    Age: Integer;
    Gender: TDBIString;
    YieldRate: Double;
    Value: Integer;
    Status: AnsiString;
  end;  { TGadRecord }


const
  cdsOGad = 'oGad.cds';
  cdsXGad = 'xGad.cds';
  dbfOGad = 'oGad.dbf';
  dbfXGad = 'xGad.dbf';
  dbfGadRecordCount = 100; //##JVR 2842;

  // ___________________________________________________________________________
  {**
    Data for the Gad dbf Table
  }
  GadData: array[0..99] of TGadRecord = (
    ( Age: 26; Gender: 'M'; YieldRate: 3; Value: 37; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3; Value: 36; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 3.25; Value: 39; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3.25; Value: 38; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 3.5; Value: 41; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3.5; Value: 40; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 3.75; Value: 43; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 3.75; Value: 42; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4; Value: 45; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4; Value: 44; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4.25; Value: 47; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4.25; Value: 46; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4.5; Value: 49; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4.5; Value: 48; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 4.75; Value: 51; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 4.75; Value: 50; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5; Value: 53; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5; Value: 52; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5.25; Value: 55; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5.25; Value: 54; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5.5; Value: 57; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5.5; Value: 56; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 5.75; Value: 59; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 5.75; Value: 58; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6; Value: 62; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6; Value: 61; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6.25; Value: 64; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6.25; Value: 63; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6.5; Value: 66; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6.5; Value: 65; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 6.75; Value: 68; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 6.75; Value: 67; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7; Value: 70; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7; Value: 69; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7.25; Value: 72; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7.25; Value: 72; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7.5; Value: 75; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7.5; Value: 74; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 7.75; Value: 77; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 7.75; Value: 76; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8; Value: 79; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8; Value: 79; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8.25; Value: 81; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8.25; Value: 81; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8.5; Value: 84; Status: 'abcd'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8.5; Value: 83; Status: 'abcd'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 8.75; Value: 86; Status: 'abc'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 8.75; Value: 85; Status: 'abc'; ),
    ( Age: 26; Gender: 'M'; YieldRate: 9; Value: 88; Status: 'abcdef'; ),
    ( Age: 26; Gender: 'F'; YieldRate: 9; Value: 88; Status: 'abcdef'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 6.5; Value: 68; Status: 'abcd'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 6.5; Value: 67; Status: 'abcd'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 6.75; Value: 70; Status: 'abc'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 6.75; Value: 69; Status: 'abc'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 7; Value: 72; Status: 'abcdef'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 7; Value: 71; Status: 'abcdef'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 7.25; Value: 75; Status: 'abc'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 7.25; Value: 73; Status: 'abc'; ),
    ( Age: 35; Gender: 'M'; YieldRate: 7.5; Value: 77; Status: 'abcd'; ),
    ( Age: 35; Gender: 'F'; YieldRate: 7.5; Value: 75; Status: 'abcd'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 4.5; Value: 59; Status: 'abcd'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 4.5; Value: 56; Status: 'abcd'; ),

{}  ( Age: 47; Gender: 'M'; YieldRate: 4.75; Value: 61; Status: 'ABC'; ),

    ( Age: 47; Gender: 'F'; YieldRate: 4.75; Value: 57; Status: 'ABC'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 5; Value: 63; Status: 'ABCDEF'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 5; Value: 59; Status: 'ABCDEF'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 5.25; Value: 65; Status: 'ABC'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 5.25; Value: 61; Status: 'ABC'; ),
    ( Age: 47; Gender: 'M'; YieldRate: 5.5; Value: 67; Status: 'ABCD'; ),
    ( Age: 47; Gender: 'F'; YieldRate: 5.5; Value: 63; Status: 'ABCD'; ),
    ( Age: 52; Gender: 'M'; YieldRate: 9.5; Value: 104; Status: 'ABC'; ),
    ( Age: 52; Gender: 'F'; YieldRate: 9.5; Value: 100; Status: 'ABC'; ),
    ( Age: 52; Gender: 'M'; YieldRate: 9.75; Value: 106; Status: 'AB'; ),
    ( Age: 52; Gender: 'F'; YieldRate: 9.75; Value: 102; Status: 'AB'; ),
    ( Age: 52; Gender: 'M'; YieldRate: 10; Value: 108; Status: 'ABCD'; ),
    ( Age: 52; Gender: 'F'; YieldRate: 10; Value: 104; Status: 'ABCD'; ),
    ( Age: 53; Gender: 'M'; YieldRate: 3; Value: 55; Status: 'ABCDEF'; ),
    ( Age: 53; Gender: 'F'; YieldRate: 3; Value: 49; Status: 'ABCDEF'; ),
    ( Age: 53; Gender: 'M'; YieldRate: 3.25; Value: 56; Status: 'ABC'; ),
    ( Age: 53; Gender: 'F'; YieldRate: 3.25; Value: 51; Status: 'ABC'; ),
    ( Age: 63; Gender: 'M'; YieldRate: 9.5; Value: 121; Status: 'ABC'; ),
    ( Age: 63; Gender: 'F'; YieldRate: 9.5; Value: 112; Status: 'ABC'; ),
    ( Age: 63; Gender: 'M'; YieldRate: 9.75; Value: 123; Status: 'AB'; ),
    ( Age: 63; Gender: 'F'; YieldRate: 9.75; Value: 114; Status: 'AB'; ),
    ( Age: 63; Gender: 'M'; YieldRate: 10; Value: 125; Status: 'ABCD'; ),
    ( Age: 63; Gender: 'F'; YieldRate: 10; Value: 116; Status: 'ABCD'; ),
    ( Age: 64; Gender: 'M'; YieldRate: 3; Value: 74; Status: 'ABCDEF'; ),
    ( Age: 64; Gender: 'F'; YieldRate: 3; Value: 65; Status: 'ABCDEF'; ),
    ( Age: 64; Gender: 'M'; YieldRate: 3.25; Value: 76; Status: 'ABC'; ),
    ( Age: 64; Gender: 'F'; YieldRate: 3.25; Value: 66; Status: 'ABC'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 5.5; Value: 127; Status: 'ABC'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 5.5; Value: 111; Status: 'ABC'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 5.75; Value: 129; Status: 'AB'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 5.75; Value: 113; Status: 'AB'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 6; Value: 131; Status: 'ABCDE'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 6; Value: 115; Status: 'ABCDE'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 6.25; Value: 133; Status: 'AB'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 6.25; Value: 116; Status: 'AB'; ),
    ( Age: 74; Gender: 'M'; YieldRate: 6.5; Value: 134; Status: 'ABC'; ),
    ( Age: 74; Gender: 'F'; YieldRate: 6.5; Value: 118;Status: 'ABCD'; )
  );



const
  GadData_Comment = 'John_Vander_Reest';

  NameOrderIdx: array[Low(NewBooks)..High(NewBooks)] of Integer =
    (4, 3, 0, 1, 2);
  NameReverseIdx: array[Low(NewBooks)..High(NewBooks)] of Integer =
    (2, 1, 0, 3, 4);


type
  TStringData = class(TPersistent)
  private
    FID: Integer;
    FEnvironment: WideString;
    FFullName: AnsiString;
    FValue: String;

  protected
    class procedure FieldProps(ADataset: TDBIClientDataset); overload;
    class procedure FieldProps(ADataset: TDBIObjectListDataset); overload;

    function GetApplication: String;
    function GetPath: String;
    function GetUserName: WideString;

  public
    class procedure AssertFields(ADataset: TDataset);
    class procedure CreateFields(ADataset: TDataset);
    class procedure OccupyFields(ADataset: TDataset);
    class procedure VerifyFields(ADataset: TDataset);

    class procedure SetupDataset(ADataset: TDataset);

  published
    property ID: Integer read FID write FID;
    property Environment: WideString read FEnvironment write FEnvironment;
    property FullName: AnsiString read FFullName write FFullName;
    property Value: String read FValue write FValue;

    property Application: String read GetApplication;
    property Path: String read GetPath;
    property UserName: WideString read GetUserName;
  end;


const
  TStringDataFields: array[0..6] of TFieldRecord = (
    (FieldName: 'ID';          FieldType: ftInteger;       ),
    (FieldName: 'Environment'; FieldType: ftUnicodeString; FieldSize: 20;),
    (FieldName: 'FullName';    FieldType: ftUnicodeString; FieldSize: 40;),
    (FieldName: 'Value';       FieldType: ftAnsiString;    FieldSize: 60;),
    (FieldName: 'Application'; FieldType: ftDefaultString; FieldSize: 32;),
    (FieldName: 'Path';        FieldType: ftDefaultString; FieldSize: 64;),
    (FieldName: 'UserName';    FieldType: ftUnicodeString; FieldSize: 32;)
    );


type
  TStringRecord = record
    ID: Integer;
    Environment: WideString;
    Fullname: AnsiString;
    Value: String;
  end;  { TStringData }


const
  // ___________________________________________________________________________
  {**
    String Data
  }
  StringData: array[0..3] of TStringRecord = (
    ( ID: 101; Environment: 'Android'; Fullname: 'Samuel Vamder Reest'; Value: 'Star Wars Trilogy'; ),
    ( ID: 102; Environment: 'Linux'; Fullname: 'John Vander Reest'; Value: '2001, A Space Odyssey'; ),
    ( ID: 103; Environment: 'Windows'; Fullname: 'Charlotte Vander Reest'; Value: 'The Adventures of Tin Tin'; ),
    ( ID: 104; Environment: 'Firefox OS'; Fullname: 'Melinda Vander Reest'; Value: 'Lord of the Rings'; )
    );

function AddField(const FieldName: String; const Datatype: TFieldType; Adataset: TDataset): TField;

function CheckField(
  PFieldData: PFieldRecord;
  PFieldProps: DBIIntfConsts.pDSFLDDesc
  ): Boolean;

procedure CompareFieldProps(CDS: TDBIClientDataset; ODS: TDBIObjectListDataset);
procedure Equalz(const Str1: String; const Str2: AnsiString);


implementation

uses
  SysUtils, Dialogs, Forms, DSIntf, DBIConst, DBIUtils, DBIXbaseDatasets;


{ Helpers }

function AddField(const FieldName: String; const Datatype: TFieldType; Adataset: TDataset): TField;
begin
  Result := nil;
  case DataType of
    ftDate: Result := TDateField.Create(ADataset);
    ftDateTime: Result := TDateTimeField.Create(ADataset);
    ftInteger: Result := TIntegerField.Create(ADataset);
    ftFloat: Result := TFloatField.Create(ADataset);
    ftString: Result := TStringField.Create(ADataset);
    ftWideString: Result := TWideStringField.Create(ADataset);
    ftWord: Result := TWordField.Create(ADataset);
  end;
  Result.FieldName := FieldName;
  Result.Dataset := ADataset;
end;


function CheckField(
  PFieldData: PFieldRecord;
  PFieldProps: DBIIntfConsts.pDSFLDDesc
  ): Boolean;
var
  iFldType: Word;

begin
  if (PFieldProps^.iFldType = fldUniCode) then begin
    iFldType := fldWideString;
  end
  else begin
    iFldType := PFieldProps^.iFldType;
  end;

  Result := String(PFieldProps^.szName) = PFieldData^.FieldName;
  Result := Result and (DBIIntfConsts.DataTypeMap[iFldType] = PFieldData^.FieldType);

  if (PFieldData^.FieldType = ftWideString) then begin
    Result := Result and (PFieldProps^.iUnits1 = 2 * PFieldData^.FieldSize);
  end
  else if (PFieldData^.FieldType = ftString) then begin
    Result := Result and (PFieldProps^.iUnits1 = PFieldData^.FieldSize);
  end;

  Assert(Result, PFieldData^.FieldName + ' is not equal to the predefined Field');
end;


procedure CompareFieldProps(CDS: TDBIClientDataset; ODS: TDBIObjectListDataset);
var
  CDSProps: DSIntf.DSProps;
  CDSFieldProps: DBClient.TFieldDescList;

  ODSProps: DBIIntfConsts.DSProps;
  ODSFieldProps: DBIIntfConsts.TFieldDescList;

  Index: Integer;

begin
  Assert(CDS.DSBase.GetProps(CDSProps) = 0);
  Assert(CDSProps.iFields = CDS.Fields.Count);
  SetLength(CDSFieldProps, CDSProps.iFields);
  Assert(CDS.DSBase.GetFieldDescs(DSIntf.PDSFldDesc(CDSFieldProps)) = 0);

  Assert(ODS.DSBase.GetProps(ODSProps) = 0);
  Assert(ODSProps.iFields = ODS.Fields.Count);
  SetLength(ODSFieldProps, ODSProps.iFields);
  Assert(ODS.DSBase.GetFieldDescs(DBIIntfConsts.PDSFldDesc(ODSFieldProps)) = 0);


  for Index := 0 to ODS.FieldCount-1 do begin
    Assert(CDSFieldProps[Index].szName = ODSFieldProps[Index].szName, 'szName Is Wrong!');
    Assert(CDSFieldProps[Index].iFldType = ODSFieldProps[Index].iFldType, 'iFldType Is Wrong!');
    Assert(CDSFieldProps[Index].iFldSubType = ODSFieldProps[Index].iFldSubType, 'iFldSubType Is Wrong!');
    Assert(CDSFieldProps[Index].iUnits1 = ODSFieldProps[Index].iUnits1, 'iUnits1 Is Wrong!');
    Assert(CDSFieldProps[Index].iUnits2 = ODSFieldProps[Index].iUnits2, 'iUnits2 Is Wrong!');
    Assert(CDSFieldProps[Index].iFldLen = ODSFieldProps[Index].iFldLen, 'iFldLen Is Wrong!');
    Assert(CDSFieldProps[Index].iFldOffsInRec = ODSFieldProps[Index].iFldOffsInRec, 'iFldOffsInRec Is Wrong!');
    Assert(CDSFieldProps[Index].iNullOffsInRec = ODSFieldProps[Index].iNullOffsInRec, 'iNullOffsInRec Is Wrong!');
    Assert(CDSFieldProps[Index].iFieldID = ODSFieldProps[Index].iFieldID, 'iFieldID Is Wrong!');
    Assert(CDSFieldProps[Index].iFieldIDParent = ODSFieldProps[Index].iFieldIDParent, 'iFieldIDParent Is Wrong!');
    Assert(CDSFieldProps[Index].bCalculated = ODSFieldProps[Index].bCalculated, 'bCalculated Is Wrong!');
    Assert(CDSFieldProps[Index].iFldAttr = ODSFieldProps[Index].iFldAttr, 'iFldAttr Is Wrong!');
    Assert(CDSFieldProps[Index].iOptParameters = ODSFieldProps[Index].iOptParameters, 'iOptParameters Is Wrong!');
  end;
end;


procedure Equalz(const Str1: String; const Str2: AnsiString);
begin
  Assert(Str1 = String(Str2));
end;





{ TStringDataList }

type
  TStringDataList = class(TStringList)
  public
    procedure AddProps(F: DSIntf.DSFLDDesc); overload;
    procedure AddProps(F: DBIIntfConsts.DSFLDDesc); overload;
  end;

procedure TStringDataList.AddProps(F: DSIntf.DSFLDDesc);
const
  Fmt = 'Name: %s, Type: %d, SubType: %d, Units1: %d, Units2: %d, FldLen: %d';
begin
  Add(
    Format(Fmt, [F.szName, F.iFldType, F.ifldSubType, F.iUnits1, F.iUnits2, F.iFldLen])
    );
end;

procedure TStringDataList.AddProps(F: DBIIntfConsts.DSFLDDesc);
const
  Fmt = 'Name: %s, Type: %d, SubType: %d, Units1: %d, Units2: %d, FldLen: %d';
begin
  Add(
    Format(Fmt, [F.szName, F.iFldType, F.ifldSubType, F.iUnits1, F.iUnits2, F.iFldLen])
    );
end;





{ TStringData }

class procedure TStringData.AssertFields(ADataset: TDataset);
var
  Index: Integer;

begin
  Index := Low(StringData);
  ADataset.First;

  while not ADataset.Eof do begin
    Assert(ADataset.FieldByName('ID').AsInteger = StringData[Index].ID);
    Assert(ADataset.FieldByName('Environment').AsString = StringData[Index].Environment);
    Equalz(ADataset.FieldByName('Fullname').AsString , StringData[Index].Fullname);
    Assert(ADataset.FieldByName('Value').AsString = StringData[Index].Value);

    Inc(Index);
    ADataset.Next;
  end;
end;


class procedure TStringData.CreateFields(ADataset: TDataset);
var
  ODS: TObjectListDataset;
  Index: Integer;

begin
  if (ADataset is TObjectListDataset) then begin
    ODS := ADataset as TObjectListDataset;
    ODS.ClassTypeName := Self.ClassName;
    ODS.StringFieldSize := 32;
    ODS.Options := ODS.Options - [osErrorOnReadOnlyProperty];
  end;

  ADataset.FieldDefs.Clear;
  ADataset.Fields.Clear;

  for Index := Low(TStringDataFields) to High(TStringDataFields) do begin
    AddField(
      TStringDataFields[Index].FieldName,
      TStringDataFields[Index].FieldType,
      ADataset
      ).Size := TStringDataFields[Index].FieldSize;
  end;
end;


class procedure TStringData.FieldProps(ADataset: TDBIClientDataset);
var
  Data: TStringDataList;
  CursorProps: DSIntf.DSProps;
  FieldProps: DBClient.TFieldDescList;
  Index: Integer;
//##JVR  FieldID: Integer;

begin
  Assert(ADataset.DSBase.GetProps(CursorProps) = 0);
  Assert(CursorProps.iFields = ADataset.Fields.Count);

  SetLength(FieldProps, CursorProps.iFields);
  Assert(ADataset.DSBase.GetFieldDescs(DSIntf.PDSFldDesc(FieldProps)) = 0);

  for Index := Low(TStringDataFields) to High(TStringDataFields) do begin
    CheckField(@TStringDataFields[Index], @FieldProps[Index]);
  end;
//##JVR  FieldID := 1;
//##JVR  Index := 0;

  Data := TStringDataList.Create;
  try
    for Index := 0 to CursorProps.iFields-1 do begin
      Data.AddProps(FieldProps[Index]);
    end;

    ShowMessage(Data.Text);
  finally
    Data.Free;
  end;
end;


class procedure TStringData.FieldProps(ADataset: TDBIObjectListDataset);
var
  Data: TStringDataList;
  CursorProps: DBIIntfConsts.DSProps;
  FieldProps: DBIIntfConsts.TFieldDescList;
  Index: Integer;
//##JVR  FieldID: Integer;

begin
  Assert(ADataset.DSBase.GetProps(CursorProps) = 0);
  Assert(CursorProps.iFields = ADataset.Fields.Count);

  SetLength(FieldProps, CursorProps.iFields);
  Assert(ADataset.DSBase.GetFieldDescs(DBIIntfConsts.PDSFldDesc(FieldProps)) = 0);

  for Index := Low(TStringDataFields) to High(TStringDataFields) do begin
    CheckField(@TStringDataFields[Index], @FieldProps[Index]);
  end;
//##JVR  FieldID := 1;
//##JVR  Index := 0;

  Data := TStringDataList.Create;
  try
    for Index := 0 to CursorProps.iFields-1 do begin
      Data.AddProps(FieldProps[Index]);
    end;

    ShowMessage(Data.Text);
  finally
    Data.Free;
  end;
end;


function TStringData.GetApplication: String;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;


function TStringData.GetUserName: WideString;
begin
  Result := DBIUtils.DBIGetUserName;
end;


function TStringData.GetPath: String;
begin
  Result := ExtractFileDir(ParamStr(0));
end;


class procedure TStringData.OccupyFields(ADataset: TDataset);
var
  Index: Integer;

begin
  for Index := Low(StringData) to High(StringData) do begin
    ADataset.Append;
    ADataset.FieldByName('ID').AsInteger := StringData[Index].ID;
    ADataset.FieldByName('Environment').AsString := String(StringData[Index].Environment);
    ADataset.FieldByName('Fullname').AsString := String(StringData[Index].Fullname);
    ADataset.FieldByName('Value').AsString := String(StringData[Index].Value);
    ADataset.Post;
  end;
end;


class procedure TStringData.SetupDataset(ADataset: TDataset);
var
  ODS: TObjectListDataset;
  CDS: TClientDataset;

begin
  if (ADataset is TObjectListDataset) then begin
    ODS := ADataset as TObjectListDataset;
    ODS.ClassTypeName := Self.ClassName;
    ODS.StringFieldSize := 32;
    ODS.Options := ODS.Options - [osErrorOnReadOnlyProperty];
  end;

  if (ADataset is TClientDataset) then begin
    CDS := ADataset as TClientDataset;
    CDS.LogChanges := False;
  end;
end;


class procedure TStringData.VerifyFields(ADataset: TDataset);
begin
  if (ADataset is TDBIClientDataset) then begin
    FieldProps(ADataset as TDBIClientDataset);
  end
  else begin
    FieldProps(ADataset as TDBIObjectListDataset);
  end;
end;





{ TGad}

class procedure TGad.AssertBlank(ADataset: TDataset);
begin
  ADataset.First;
  while not ADataset.Eof do begin
    Assert(ADataset.FieldByName('Age').AsInteger = 0);
//##NOP    Assert(ADataset.FieldByName('Gender').IsNull);
    Assert(ADataset.FieldByName('YieldRate').AsFloat = 0);
    Assert(ADataset.FieldByName('Value').AsFloat = 0);
    Assert(ADataset.FieldByName('Comment').IsNull);
    Assert(ADataset.FieldByName('Status').IsNull);

    ADataset.Next;
  end;
end;


class procedure TGad.AssertFields(ADataset: TDataset);
var
  Index: Integer;

begin
  Index := Low(GadData);
  ADataset.First;

  while not ADataset.Eof do begin
    Assert(ADataset.FieldByName('Age').AsInteger = GadData[Index].Age);
    Equalz(ADataset.FieldByName('Gender').ASString , GadData[Index].Gender);
    Assert(ADataset.FieldByName('YieldRate').AsFloat = GadData[Index].YieldRate);
    Assert(ADataset.FieldByName('Value').AsInteger = GadData[Index].Value);
{$ifdef DELPHI6}
    Assert(ADataset.FieldByName('Comment').AsString = GadData_Comment);
{$endif}    
    Equalz(ADataset.FieldByName('Status').AsString , GadData[Index].Status);

    Inc(Index);
    ADataset.Next;
  end;
end;


class procedure TGad.ClearFields(ADataset: TDataset);
begin
  ADataset.First;
  while not ADataset.Eof do begin
    ADataset.Edit;
    ADataset.FieldByName('Age').Clear;
//##NOP    ADataset.FieldByName('Gender').Clear;
    ADataset.FieldByName('YieldRate').Clear;
    ADataset.FieldByName('Value').Clear;
    ADataset.FieldByName('Comment').Clear;
    ADataset.FieldByName('Status').Clear;

    ADataset.CheckBrowseMode;
    ADataset.Next;
  end;
end;


class procedure TGad.CreateFieldDefs(ADataset: TDataset);
begin
(*##JVR
    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'Age';
    Field.DataType := ftSmallInt;

    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'Gender';
    Field.DataType := ftString;
    Field.Size := 1;

    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'YieldRate';
    Field.DataType := ftFloat;
{##JVR
    Field.DataType := ftBCD;
    Field.Size := 14;
    Field.Precision := 4;
//}
    Field := ODS.FieldDefs.AddFieldDef;
    Field.Name := 'Value';
    Field.DataType := ftSmallInt;
{##JVR
    ODS.FieldDefs.Add('Gender', ftString, 1, False);
    ODS.FieldDefs.Add('YieldRate', ftFloat, 2, False);
    ODS.FieldDefs.Add('Value', ftSmallInt, 0, False);
//}
*)
end;

class procedure TGad.CreateFields(ADataset: TDataset);
begin
  if (ADataset is TObjectListDataset) then begin
    (ADataset as TObjectListDataset).ClassTypeName := Self.ClassName;
  end;

  ADataset.FieldDefs.Clear;
  ADataset.Fields.Clear;

  AddField('ID', ftWord, ADataset);
  AddField('Age', ftInteger, ADataset);
  AddField('Gender', ftString, ADataset).Size := 1;
  AddField('YieldRate', ftFloat, ADataset);
  AddField('Value', ftFloat, ADataset);
  AddField('Comment', ftWideString, ADataset).Size := 20;
  AddField('Status', ftString, ADataset).Size := 6;
  AddField('Date', ftDate, ADataset);
  AddField('Time', ftString, ADataset).Size := 12; //##JVR8;
  AddField('Created', ftDateTime, ADataset);
end;


// _____________________________________________________________________________
{**
  Jvr - 03/12/2002 11:22:46.<P>
}
procedure TGad.SetGender(const Value: TDBIString);
begin
  if (Value <> 'M') and (Value <> 'F') then begin
    raise Exception.Create('Illegal gender value, values [F, M] permited');
  end;

  FGender := Value;
end;  { SetGender }


class procedure TGad.UpdateFields(ADataset: TDataset);
var
  Index: Integer;

begin
  Index := Low(GadData);
  ADataset.First;

  while not ADataset.Eof do begin
    ADataset.Edit;
    ADataset.FieldByName('Age').AsInteger := GadData[Index].Age;
    ADataset.FieldByName('Gender').ASString := String(GadData[Index].Gender);
    ADataset.FieldByName('YieldRate').AsFloat := GadData[Index].YieldRate;
    ADataset.FieldByName('Value').AsInteger := GadData[Index].Value;
    ADataset.FieldByName('Comment').AsString := GadData_Comment;
    ADataset.FieldByName('Status').AsString := String(GadData[Index].Status);
    ADataset.CheckBrowseMode;

    Inc(Index);
    ADataset.Next;
  end;
end;





{ TBookData }

class procedure TBookData.AssertFields(ADataset: TDataset);
var
  Index: Integer;

begin
  Index := Low(NewBooks);

  ADataset.First;
  while not ADataset.Eof do begin
    Assert(ADataset.RecNo = (Index + 1));
    Assert(ADataset.FieldByName('Sequence').AsInteger = NewBooks[Index].Sequence);
    Equalz(ADataset.FieldByName('Name').AsString , NewBooks[Index].Name);
    Equalz(ADataset.FieldByName('Author').AsString , NewBooks[Index].Author);
    Equalz(
      FormatDateTime(DateTimeFormat, ADataset.FieldByName('Purchased').AsDateTime),
      NewBooks[Index].Purchased
      );
    Assert(ADataset.FieldByName('Price').AsFloat = NewBooks[Index].Price);
    Equalz(ADataset.FieldByName('Currency').AsString , NewBooks[Index].Currency);
    Assert(ADataset.FieldByName('Rating').AsInteger = NewBooks[Index].Rating);
    Assert(ADataset.FieldByName('Approved').AsBoolean = NewBooks[Index].Approved);
    Equalz(ADataset.FieldByName('Comments').AsString , NewBooks[Index].Comments);
    Equalz(ADataset.FieldByName('Notes').AsString , NewBooks[Index].Notes);
    Equalz(ADataset.FieldByName('Details').AsString , NewBooks[Index].Details);

    Inc(Index);
    ADataset.Next;
  end;

  Assert(ADataset.RecordCount = Length(NewBooks));
end;


class procedure TBookData.CreateFieldDefs(ADataset: TDataset);
var
  Index: Integer;
  FieldDef: TFieldDef;

begin
  ADataset.Fields.Clear;
  ADataset.FieldDefs.Clear;

  for Index := Low(TBookFields) to High(TBookFields) do begin
    FieldDef := ADataset.FieldDefs.AddFieldDef;
    FieldDef.Name := UpperCase(TBookFields[Index].FieldName);
    FieldDef.DataType := TBookFields[Index].FieldType;

    if (TBookFields[Index].FieldSize > 0) then begin
      FieldDef.Size := TBookFields[Index].FieldSize;
    end;

    if (TBookFields[Index].Precision > 0) then begin
      FieldDef.Precision := TBookFields[Index].Precision;
    end;
  end;
end;


class procedure TBookData.OccupyFields(ADataset: TDataset);
var
  Index: Integer;

begin
  for Index := Low(NewBooks) to High(NewBooks) do begin
    ADataset.AppendRecord([
      NewBooks[Index].Sequence,
      NewBooks[Index].Name,
      NewBooks[Index].Author,
      StrToDateTime(String(NewBooks[Index].Purchased)),
      NewBooks[Index].Price,
      NewBooks[Index].Currency,
      NewBooks[Index].Rating,
      NewBooks[Index].Approved,
      NewBooks[Index].Comments,
      NewBooks[Index].Notes,
      NewBooks[Index].Details
      ]);
  end;
end;


class procedure TBookData.XbaseCreateTable(const AFileName: String);
var
  XDS: TXbaseDataset;

begin
  XDS := TXbaseDataset.Create(nil);
  try
    XDS.FileName := AFileName;
    TBookData.CreateFieldDefs(XDS);
    XDS.CreateDataset;

    TBookData.OccupyFields(XDS);
    TBookData.AssertFields(XDS);

    XDS.Close;
  finally
    XDS.Free;
  end;

  Assert(SysUtils.FileExists(ChangeFileExt(AFileName, '.dbf')));
  Assert(SysUtils.FileExists(ChangeFileExt(AFileName, '.fpt')));
end;  { XbaseCreateTable }





{ TDBIUnitTests }

// _____________________________________________________________________________
{**
  Jvr - 25/01/2001 12:49:01 - Initial code.<br>
  Jvr - 13/09/2004 14:55:58 - Modified way path is determined.<p>
}
function TDBIUnitTests.DataPath(const FileName: String = ''): String;
begin
  Result := TestTempDir + FileName;
end;


function TDBIUnitTests.GetParent: TomTestSuite;
begin
  Result := Self;
end;


{$ifndef omTesting}
function TDBIUnitTests.GetTestTempDir: String;
begin
  Result := DBIUtils.DBITempFolder;
end;
{$endif}


procedure TDBIUnitTests.ZapOGadTable;
begin
  // Delete the OGad Datafile if it exists
  SysUtils.DeleteFile(DataPath(dbfOGad));
  Assert(not SysUtils.FileExists(DataPath(dbfOGad)));
end;


procedure TDBIUnitTests.ZapXGadTable;
begin
  // Delete the XGad Datafile if it exists
  SysUtils.DeleteFile(DataPath(dbfXGad));
  Assert(not SysUtils.FileExists(DataPath(dbfXGad)));
end;





initialization
  Classes.RegisterClass(TGad);
  Classes.RegisterClass(TStringData);

{$ifdef omTesting}
  TomTestMastery.RegisterTestSuite(TDBIUnitTests);
{$else}
  RegisterTest('', TDBIUnitTests.Suite);
{$endif}

end.
