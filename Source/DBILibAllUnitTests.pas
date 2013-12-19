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
{$ifdef __omTesting}
  DBICDSUnitTests,
{$endif}
  DBIAUXUnitTests,
  DBIODSUnitTests,
  DBIXDSUnitTests;

end.
