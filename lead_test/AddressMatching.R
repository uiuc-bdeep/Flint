## Preliminaries
rm(list=ls())

## algorithm: 1. match street number, 2. match street name,
## 3. manual filter, 4. date of trans before date of test

## read cleaned up data ##
library(BDEEPdbZillow)
res_data <- read.csv("/home/bdeep/share/projects/Flint/production/LeadTestsResult/flintLeadTest_merged_til_2019_Address.csv", stringsAsFactors = FALSE)
zillow_data <- get_from_db_usr("SELECT propertyfullstreetaddress, propertyhousenumber, propertystreetname, propertystreetsuffix, propertycity, propertystate, recordingdate FROM hedonics_new.hedonics_26")
colnames(zillow_data)[2] <-"Street.."
colnames(zillow_data)[3]<-"Street.Name"

#make res_data unique by earliest lead test date
library(data.table)
res_firstdate<-setDT(res_data)[,.SD[which.max(as.Date(TestDate,"%y-%m-%d"))],keyby=c("Street..","Street.Name")]

#merge lead with zillow by street number
merge_result <- merge(zillow_data, res_firstdate, by="Street..")


#remove street, ave from street names in lead test
library(dplyr)
merge_st<-merge_result %>% mutate_all(~gsub(" AVE| DR| ST| ST APT1| AVE 1| BLVD|W |E |S |N | PLZ", "", .))

#find instances where street names match
name_match<-merge_st[which(merge_st$Street.Name.x==merge_st$Street.Name.y),]
#remove redundant col for easier manual check
name_match<-name_match[which(name_match$propertycity == "FLINT"),]
name_match$post <- as.Date(name_match$recordingdate,"%Y-%m-%d") > as.Date(name_match$PostDate,"%Y-%m-%d")
#keep only zillow date later than res date
date_testdate <- name_match[which(as.Date(name_match$recordingdate,"%Y-%m-%d") > as.Date(name_match$TestDate,"%y-%m-%d")),]
date_postdate <-name_match[which(as.Date(name_match$recordingdate,"%Y-%m-%d") > as.Date(name_match$PostDate,"%Y-%m-%d")),]

date_testdate <- date_testdate[c("propertyfullstreetaddress","recordingdate","Postal","TestDate","PostDate","LABID","post")]
date_postdate <-date_postdate[c("propertyfullstreetaddress","recordingdate","Postal","TestDate","PostDate","LABID","post")]
