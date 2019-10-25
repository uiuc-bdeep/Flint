Ohio <- get_from_db_usr("SELECT ImportParcelID AS \"ImportParcelID\", TransID AS \"TransId\", PropertySequenceNumber AS \"PropertySequenceNumber\", RecordingDate AS \"RecordingDate\", DocumentDate AS \"DocumentDate\", SignatureDate AS \"SignatureDate\", EffectiveDate AS \"EffectiveDate\", SalesPriceAmount AS \"SalesPriceAmount\", LoanAmount AS \"LoanAmount\", SalesPriceAmountStndCode AS \"SalesPriceAmountStndCode\", LoanAmountStndCode AS \"LoanAmountStndCode\", DataClassStndCode AS \"DataClassStndCode\", DocumentTypeStndCode AS \"DocumentTypeStndCode\", PartialInterestTransferStndCode AS \"PartialInterestTransferStndCode\", IntraFamilyTransferFlag AS \"IntraFamilyTransferFlag\", TransferTaxExemptFlag AS \"TransferTaxExemptFlag\", PropertyUseStndCode AS \"PropertyUseStndCode\", AssessmentLandUseStndCode AS \"AssessmentLandUseStndCode\", OccupancyStatusStndCode AS \"OccupancyStatusStndCode\", RowID AS \"RowID\", BuildingOrImprovementNumber AS \"BuildingOrImprovementNumber\", LoadID AS \"LoadID\", FIPS AS \"FIPS\", State AS \"State\", County AS \"County\", PropertyFullStreetAddress AS \"PropertyFullStreetAddress\", PropertyHouseNumber AS \"PropertyHouseNumber\", PropertyHouseNumberExt AS \"PropertyHouseNumberExt\", PropertyStreetPreDirectional AS \"PropertyStreetPreDirectional\", PropertyStreetName AS \"PropertyStreetName\", PropertyStreetSuffix AS \"PropertyStreetSuffix\", PropertyStreetPostDirectional AS \"PropertyStreetPostDirectional\", PropertyCity AS \"PropertyCity\", PropertyState AS \"PropertyState\", PropertyZip AS \"PropertyZip\", PropertyBuildingNumber AS \"PropertyBuildingNumber\", PropertyAddressUnitDesignator AS \"PropertyAddressUnitDesignator\", PropertyAddressUnitNumber AS \"PropertyAddressUnitNumber\", PropertyAddressLatitude AS \"PropertyAddressLatitude\", PropertyAddressLongitude AS \"PropertyAddressLongitude\", PropertyAddressCensusTractAndBlock AS \"PropertyAddressCensusTractAndBlock\", NoOfBuildings AS \"NoOfBuildings\", LotSizeAcres AS \"LotSizeAcres\", LotSizeSquareFeet AS \"LotSizeSquareFeet\", TaxAmount AS \"TaxAmount\", TaxYear AS \"TaxYear\", NoOfUnits AS \"NoOfUnits\", YearBuilt AS \"YearBuilt\", EffectiveYearBuilt AS \"EffectiveYearBuilt\", YearRemodeled AS \"YearRemodeled\", NoOfStories AS \"NoOfStories\", StoryTypeStndCode AS \"StoryTypeStndCode\", TotalRooms AS \"TotalRooms\", TotalBedrooms AS \"TotalBedrooms\", FullBath AS \"FullBath\", ThreeQuarterBath AS \"ThreeQuarterBath\", HalfBath AS \"HalfBath\", QuarterBath AS \"QuarterBath\", HeatingTypeorSystemStndCode AS \"HeatingTypeorSystemStndCode\", PropertyLandUseStndCode AS \"PropertyLandUseStndCode\", WaterStndCode AS \"WaterStndCode\", sqfeet AS \"sqfeet\" FROM hedonics_new.hedonics_39 WHERE (County = 'LUCAS' AND PropertyZip IN ('43528', '43537', '43542', '43615', '43617')) OR (County = 'ROSS' AND PropertyZip IN ('45601')) OR PropertyCity IN ('YOUNGSTOWN', 'DAYTON', 'youngstown', 'dayton')")
saveRDS(Ohio, file = "/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/hedonics_output_2019/OHHedonics_2019.rds")


Texas <- get_from_db_usr("SELECT ImportParcelID AS \"ImportParcelID\", TransID AS \"TransId\", PropertySequenceNumber AS \"PropertySequenceNumber\", RecordingDate AS \"RecordingDate\", DocumentDate AS \"DocumentDate\", SignatureDate AS \"SignatureDate\", EffectiveDate AS \"EffectiveDate\", SalesPriceAmount AS \"SalesPriceAmount\", LoanAmount AS \"LoanAmount\", SalesPriceAmountStndCode AS \"SalesPriceAmountStndCode\", LoanAmountStndCode AS \"LoanAmountStndCode\", DataClassStndCode AS \"DataClassStndCode\", DocumentTypeStndCode AS \"DocumentTypeStndCode\", PartialInterestTransferStndCode AS \"PartialInterestTransferStndCode\", IntraFamilyTransferFlag AS \"IntraFamilyTransferFlag\", TransferTaxExemptFlag AS \"TransferTaxExemptFlag\", PropertyUseStndCode AS \"PropertyUseStndCode\", AssessmentLandUseStndCode AS \"AssessmentLandUseStndCode\", OccupancyStatusStndCode AS \"OccupancyStatusStndCode\", RowID AS \"RowID\", BuildingOrImprovementNumber AS \"BuildingOrImprovementNumber\", LoadID AS \"LoadID\", FIPS AS \"FIPS\", State AS \"State\", County AS \"County\", PropertyFullStreetAddress AS \"PropertyFullStreetAddress\", PropertyHouseNumber AS \"PropertyHouseNumber\", PropertyHouseNumberExt AS \"PropertyHouseNumberExt\", PropertyStreetPreDirectional AS \"PropertyStreetPreDirectional\", PropertyStreetName AS \"PropertyStreetName\", PropertyStreetSuffix AS \"PropertyStreetSuffix\", PropertyStreetPostDirectional AS \"PropertyStreetPostDirectional\", PropertyCity AS \"PropertyCity\", PropertyState AS \"PropertyState\", PropertyZip AS \"PropertyZip\", PropertyBuildingNumber AS \"PropertyBuildingNumber\", PropertyAddressUnitDesignator AS \"PropertyAddressUnitDesignator\", PropertyAddressUnitNumber AS \"PropertyAddressUnitNumber\", PropertyAddressLatitude AS \"PropertyAddressLatitude\", PropertyAddressLongitude AS \"PropertyAddressLongitude\", PropertyAddressCensusTractAndBlock AS \"PropertyAddressCensusTractAndBlock\", NoOfBuildings AS \"NoOfBuildings\", LotSizeAcres AS \"LotSizeAcres\", LotSizeSquareFeet AS \"LotSizeSquareFeet\", TaxAmount AS \"TaxAmount\", TaxYear AS \"TaxYear\", NoOfUnits AS \"NoOfUnits\", YearBuilt AS \"YearBuilt\", EffectiveYearBuilt AS \"EffectiveYearBuilt\", YearRemodeled AS \"YearRemodeled\", NoOfStories AS \"NoOfStories\", StoryTypeStndCode AS \"StoryTypeStndCode\", TotalRooms AS \"TotalRooms\", TotalBedrooms AS \"TotalBedrooms\", FullBath AS \"FullBath\", ThreeQuarterBath AS \"ThreeQuarterBath\", HalfBath AS \"HalfBath\", QuarterBath AS \"QuarterBath\", HeatingTypeorSystemStndCode AS \"HeatingTypeorSystemStndCode\", PropertyLandUseStndCode AS \"PropertyLandUseStndCode\", WaterStndCode AS \"WaterStndCode\", sqfeet AS \"sqfeet\" FROM hedonics_new.hedonics_48 WHERE County = 'HUNT' AND PropertyCity IN ('GREENVILLE', 'Greenville')")
saveRDS(Texas, file = "/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/hedonics_output_2019/TXHedonics_2019.rds")