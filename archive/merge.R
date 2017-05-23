rm(list=ls())
#setwd("/Volumes/share/projects/Flint/")
#setwd("share/projects/Flint/")
#old_data <- read.csv("production/crawledWithRepeatedTagged.csv")
old_data <- read.csv("production/most_recent_flint_transactions.csv")
crawled_data <- read.csv("production/new_data_leads.csv")

## update the crawled code into the data chain ##

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


