# Qingyu Wang && Zixiong Feng
# transfer database hedonics to rstudio

library(BDEEPdbZillow)

# wrong <- get_from_db_usr("SELECT ImportParcelID AS \"ImportParcelID\", TransID AS \"TransID\", PropertySequenceNumber AS \"PropertySequenceNumber\", RecordingDate AS \"RecordingDate\", DocumentDate AS \"DocumentDate\", SignatureDate AS \"SignatureDate\", EffectiveDate AS \"EffectiveDate\", SalesPriceAmount AS \"SalesPriceAmount\", LoanAmount AS \"LoanAmount\", SalesPriceAmountStndCode AS \"SalesPriceAmountStndCode\", LoanAmountStndCode AS \"LoanAmountStndCode\", DataClassStndCode AS \"DataClassStndCode\", DocumentTypeStndCode AS \"DocumentTypeStndCode\", PartialInterestTransferStndCode AS \"PartialInterestTransferStndCode\", IntraFamilyTransferFlag AS \"IntraFamilyTransferFlag\", TransferTaxExemptFlag AS \"TransferTaxExemptFlag\", PropertyUseStndCode AS \"PropertyUseStndCode\", AssessmentLandUseStndCode AS \"AssessmentLandUseStndCode\", OccupancyStatusStndCode AS \"OccupancyStatusStndCode\", RowID AS \"RowID\", BuildingOrImprovementNumber AS \"BuildingOrImprovementNumber\", LoadID AS \"LoadID\", FIPS AS \"FIPS\", State AS \"State\", County AS \"County\", PropertyFullStreetAddress AS \"PropertyFullStreetAddress\", PropertyHouseNumber AS \"PropertyHouseNumber\", PropertyHouseNumberExt AS \"PropertyHouseNumberExt\", PropertyStreetPreDirectional AS \"PropertyStreetPreDirectional\", PropertyStreetName AS \"PropertyStreetName\", PropertyStreetSuffix AS \"PropertyStreetSuffix\", PropertyStreetPostDirectional AS \"PropertyStreetPostDirectional\", PropertyCity AS \"PropertyCity\", PropertyState AS \"PropertyState\", PropertyZip AS \"PropertyZip\", PropertyBuildingNumber AS \"PropertyBuildingNumber\", PropertyAddressUnitDesignator AS \"PropertyAddressUnitDesignator\", PropertyAddressUnitNumber AS \"PropertyAddressUnitNumber\", PropertyAddressLatitude AS \"PropertyAddressLatitude\", PropertyAddressLongitude AS \"PropertyAddressLongitude\", PropertyAddressCensusTractAndBlock AS \"PropertyAddressCensusTractAndBlock\", NoOfBuildings AS \"NoOfBuildings\", LotSizeAcres AS \"LotSizeAcres\", LotSizeSquareFeet AS \"LotSizeSquareFeet\", TaxAmount AS \"TaxAmount\", TaxYear AS \"TaxYear\", NoOfUnits AS \"NoOfUnits\", YearBuilt AS \"YearBuilt\", EffectiveYearBuilt AS \"EffectiveYearBuilt\", YearRemodeled AS \"YearRemodeled\", NoOfStories AS \"NoOfStories\", StoryTypeStndCode AS \"StoryTypeStndCode\", TotalRooms AS \"TotalRooms\", TotalBedrooms AS \"TotalBedrooms\", FullBath AS \"FullBath\", ThreeQuarterBath AS \"ThreeQuarterBath\", HalfBath AS \"HalfBath\", QuarterBath AS \"QuarterBath\", HeatingTypeorSystemStndCode AS \"HeatingTypeorSystemStndCode\", PropertyLandUseStndCode AS \"PropertyLandUseStndCode\", WaterStndCode AS \"WaterStndCode\", sqfeet AS \"sqfeet\" FROM hedonics_new.hedonics_bernardino06")

# wrong <- readRDS("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/Grand Blanc/springfield_OH.rds")

#right_class <- sapply(right, class)
#wrong_class <- sapply(wrong, class)

wrong <- data

wrong$ImportParcelID <- as.integer(wrong$ImportParcelID)
names(wrong)[2] <- "TransId"
wrong$TransId <- as.integer(wrong$TransId)
wrong$RecordingDate <- as.character(wrong$RecordingDate)
wrong$DocumentDate <- as.character(wrong$DocumentDate)
wrong$SignatureDate <- as.character(wrong$SignatureDate)
wrong$EffectiveDate <- as.character(wrong$EffectiveDate)
wrong$FIPS <- as.integer(wrong$FIPS)
wrong$PropertyHouseNumberExt <- as.logical(wrong$PropertyHouseNumberExt)
wrong$PropertyBuildingNumber <- as.logical(wrong$PropertyBuildingNumber)
wrong$PropertyAddressCensusTractAndBlock <- as.numeric(wrong$PropertyAddressCensusTractAndBlock)
wrong$EffectiveYearBuilt <- as.logical(wrong$EffectiveYearBuilt)
wrong$NoOfStories <- as.numeric(wrong$NoOfStories)
wrong$WaterStndCode <- as.logical(wrong$WaterStndCode)
wrong$SalesPriceAmount <- as.numeric(gsub('[$,.]', '', wrong$SalesPriceAmount)) / 100
wrong$LoanAmount <- as.numeric(gsub('[$,.]', '', wrong$LoanAmount)) / 100
wrong$TaxAmount <- as.numeric(gsub('[$,.]', '', wrong$TaxAmount)) / 100

sapply(wrong, class)
saveRDS(wrong, "/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/Texas_HUNT_2019.rds")