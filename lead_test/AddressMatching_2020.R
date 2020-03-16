## Preliminaries
rm(list=ls())

## algorithm: 1. match street number, 2. match street name,
## 3. manual filter, 4. date of trans before date of test

## read cleaned up data ##
#library(BDEEPdbZillow)
a <- read.csv("/home/bdeep/share/projects/Flint_Housing/production/GeneseeAnalysis/GeneseeAnalysis_Dec_09_2019.csv")
res_data <- read.csv("/home/bdeep/share/projects/Flint/production/LeadTestsResult/flintLeadTest_merged_til_2019_Address.csv", stringsAsFactors = FALSE)
#zillow_data <- get_from_db_usr("SELECT propertyfullstreetaddress, propertyhousenumber, propertystreetname, propertystreetsuffix, propertycity, propertystate, recordingdate FROM hedonics_new.hedonics_26")
#colnames(zillow_data)[2] <-"Street.."
#colnames(zillow_data)[3]<-"Street.Name"

#make res_data unique by earliest lead test date
library(data.table)
res_firstdate<-setDT(res_data)[,.SD[which.max(as.Date(TestDate,"%y-%m-%d"))],keyby=c("Street..","Street.Name")]
colnames(a)[which(names(a) == "PropertyHouseNumber")] <- "Street.."
#merge lead with zillow by street number
res_firstdate <- unique(res_firstdate)
merge_result <- merge(a, res_firstdate, by="Street..")

#remove street, ave from street names in lead test
library(dplyr)
merge_result$Street.Name <- gsub(" AVE| DR| ST| ST APT1| AVE 1| BLVD|W |E |S |N | PLZ", "", merge_result$Street.Name)
merge_result$PropertyStreetName <- gsub(" AVE| DR| ST| ST APT1| AVE 1| BLVD|W |E |S |N | PLZ", "", merge_result$PropertyStreetName)
merge_result$Street.Name <- gsub('\\s+', '',merge_result$Street.Name)
merge_result$PropertyStreetName <- gsub('\\s+', '',merge_result$PropertyStreetName)

#find instances where street names match
name_match<-merge_result[which(merge_result$PropertyStreetName==merge_result$Street.Name),]
#remove redundant col for easier manual check
name_match<-name_match[which(name_match$PropertyCity == "FLINT"),]
#name_match <- unique(name_match)
#choose the first post date
name_match$postBeforeRec <- (as.Date(name_match$RecordingDate,"%Y-%m-%d") > as.Date(name_match$PostDate,"%Y-%m-%d"))


#for (x in colnames(a)) { name_match[,x] <- eval( call( paste0("as.", class(a[,x])), name_match[,x]) )}
#colnames(name_match)[which(names(name_match) == "Street.Name.")] <- "PropertyStreetName"
colnames(a)[which(names(a) == "Street..")] <- "PropertyHouseNumber"
#colnames(name_match)[which(names(name_match) == "post")] <- "postBeforeRec"
cols_to_keep <- intersect(colnames(name_match),colnames(a))
cols_to_keep <- c(cols_to_keep, "postBeforeRec")
name_match <- name_match[,cols_to_keep, drop=FALSE]
name_match_uniq <- unique(name_match)#8162
#
name_match_wo <- name_match_uniq[ , !(names(name_match_uniq) %in% "postBeforeRec")]
name_match_wo <- unique(name_match_wo)#8158
remove = c("PropertyStreetName")
cols_to_keep <- intersect(colnames(name_match),colnames(a))
joined_data <- left_join(a, name_match_uniq,by=cols_to_keep[- which(cols_to_keep %in% remove)])

joined_data$PropertyStreetName.y = NULL
colnames(joined_data)[which(names(joined_data) == "PropertyStreetName.x")] <- "PropertyStreetName"
joined_data$PropertyStreetName.y = NULL
#colnames(joined)[which(names(joined) == "PropertyStreetName.x")] <- "PropertyStreetName"
#keep only zillow date later than res date
#date_testdate <- name_match[which(as.Date(name_match$recordingdate,"%Y-%m-%d") > as.Date(name_match$TestDate,"%y-%m-%d")),]
#date_postdate <-name_match[which(as.Date(name_match$recordingdate,"%Y-%m-%d") > as.Date(name_match$PostDate,"%Y-%m-%d")),]

#date_testdate <- date_testdate[c("propertyfullstreetaddress","recordingdate","Postal","TestDate","PostDate","LABID","post")]
#date_postdate <- date_postdate[c("propertyfullstreetaddress","recordingdate","Postal","TestDate","PostDate","LABID","post")]

#write.csv(name_match, "/home/bdeep/share/projects/Flint_Housing/stores/address_match.csv")
