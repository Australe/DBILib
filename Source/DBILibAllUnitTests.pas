unit DBILibAllUnitTests;

interface

implementation

uses
  DBIConst,
  DBICustomListDataConnections,
  DBICustomStreamDataConnections,
  DBIDataset,
  DBIFileStreams,
  DBIFilters,
  DBIIndices,
  DBIInterfaces,
  DBIIntfConsts,
  DBIObjectListDataConnections,
  DBIObjectListDatasets,
  DBIStrings,
  DBIStringsDataConnections,
  DBIUtils,
  DBIXBaseBlobConnections,
  DBIXBaseConsts,
  DBIXBaseDataConnections,
  DBIXbaseDatasets,
  DBIXmlUtils,

  // Tests
  DBIUnitTests,
{$ifdef omTesting}
  DBICDSUnitTests,
{$endif}
  DBIODSUnitTests,
  DBIXDSUnitTests;

end.
