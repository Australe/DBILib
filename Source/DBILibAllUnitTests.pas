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
  DBIStreamAdapters,
  DBIStrings,
  DBIStringsDataConnections,
  DBITokenizers,
  DBITokenizerConsts,
  DBIUtils,
  DBIXBaseBlobConnections,
  DBIXBaseConsts,
  DBIXBaseDataConnections,
  DBIXbaseDatasets,
  DBIXmlDataConnections,
  DBIXmlUtils,

  // Tests
  DBIUnitTests,
{$ifdef __omTesting}
  DBICDSUnitTests,
{$endif}
  DBIODSUnitTests,
  DBIXDSUnitTests;

end.
