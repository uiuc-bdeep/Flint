rm(list=ls())
#setwd("/Volumes/share/projects/Flint/")
#setwd("share/projects/Flint/")
#old_data <- read.csv("production/crawledWithRepeatedTagged.csv")
old_data <- read.csv("production/most_recent_flint_transactions.csv")
crawled_data <- read.csv("production/new_data_leads.csv")

for (i in 1:nrow(crawled_data))
{
  current_transaction <- crawled_data[i,]
  if (as.Date(current_transaction$RecordingDate) > as.Date("2014-04-14"))
  {
    ipd <- current_transaction$ImportParcelID
    temp_set <- old_data[old_data$ImportParcelID==ipd,]
    if (nrow(temp_set[as.Date(temp_set$RecordingDate) < as.Date("2014-04-14"),]) > 0)
    {
      old_data[old_data$ImportParcelID==ipd,]$repeated <- 1
      crawled_data[i,]$repeated <- 1
    }
  }
}
new_data <- rbind(old_data, crawled_data)
write.csv(new_data, "production/most_recent_flint_transactions.csv")


## calculating yes and no
tests <- read.csv("production/LeadTestsResult/leadData_merged.csv")
crawled_data$yes = 0
crawled_data$no = 0
for (i in 1:nrow(crawled_data))
{
  tests_temp <- tests[as.Date(tests$Results_Se, format="%m/%d/%Y") < crawled_data[i,]$RecordingDate,]
  tests_temp_y <- tests_temp[tests_temp$Lead_Viola=="yes",]
  tests_temp_n <- tests_temp[tests_temp$Lead_Viola=="no",]
  if (nrow(tests_temp_y) != 0) {
    crawled_data[i,]$Yes = nrow(tests_temp_y)
  }
  if (nrow(tests_temp_n) != 0) {
    crawled_data[i,]$No = nrow(tests_temp_n)
  }
}