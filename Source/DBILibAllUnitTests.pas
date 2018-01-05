unit DBILibAllUnitTests;

interface

implementation

uses
  DBIConst,
  DBICustomListDataConnections,
  DBICustomStreamDataConnections,
  DBIDataPacketReaders,
  DBIDataPacketWriters,
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
  DBIXmlUtils,

  // Tests
  DBIUnitTests,
{$ifndef fpc}
  DBICDSUnitTests,
{$endif}
  DBIAUXUnitTests,
  DBIODSUnitTests,
  DBIXDSUnitTests,
  DBITokenizeUnitTests;

end.
