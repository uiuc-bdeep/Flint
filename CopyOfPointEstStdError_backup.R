library(ggplot2)
library(scales)
library(sp)

mergeback <- 0 # flag for merging the new data back to the original one after calculating the stats. used at the end

dist1 <- 0.25
dist2 <- 0.5
dist3 <- 1
dist4 <- 1.5
dist5 <- 2
dist6 <- 4

dists <- c(dist1, dist2, dist3, dist4, dist5, dist6) # all the distances to loop through
name1_base <- "expected_leadLevel"
name2_base <- "standard_error"



transactions <- read.csv("production/most_recent_flint_transactions.csv")
o_transactions <- transactions     # used in the end for merging new statistics back to the original dataset
transactions <- transactions[as.Date(transactions$RecordingDate) > as.Date("2015-02-25"), ]
#load post testing transactions
#loop through transactions

drop <- c("cumulative.prob","dev","positive.tests_byDay", "tests_byDay", "deviation_byDay")
transactions <- transactions[,!(names(results) %in% drop)]
for (dist in dists)
{
  dist_s <- toString(dist)
  
  expected_LeadLevel <- paste(name1_base, dist_s, sep="_")
  cumulative.prob <- paste(expected_LeadLevel, "km", sep="")
  transactions[, cumulative.prob] <- 0
  standard_error <- paste(name2_base, dist_s, sep="_")
  dev <- paste(standard_error, "km", sep="")
  transactions[, dev] <- 0
  
}
transactions$positive.tests_byDay <- 0
transactions$tests_byDay <- 0
transactions$cumulative.prob <- 0
transactions$dev <- 0
transactions$deviation_byDay <- 0

#leadData_merged <- read.csv("~/share/projects/Flint/production/LeadTestsResult/leadData_merged.csv")
leadData_merged_all <- read.csv("production/LeadTestsResult/leadData_merged.csv")
lead_tests <- leadData_merged_all



trans_addr <- transactions
tests_addr <- leadData_merged_all
trans_addr <- trans_addr[!is.na(trans_addr$PropertyAddressLatitude),]        # filter out the data that don't have valid longitude/latitude
trans_addr <- trans_addr[!is.na(trans_addr$PropertyAddressLongitude),]
tests_addr <- tests_addr[!is.na(tests_addr$X),]
tests_addr <- tests_addr[!is.na(tests_addr$Y),]
trans_coords <- trans_addr
tests_coords <- tests_addr
coordinates(trans_coords) <- c("PropertyAddressLongitude", "PropertyAddressLatitude")   # coordinates for all the housing points
coordinates(tests_coords) <- c("X", "Y")    

# add to plot -- red vertical line at date of transaction i
# output cumulative.prob, cumulative.standard.deviation for transaction i, by date i and date of last test
for (j in 1:nrow(transactions))
{
  if(j %% 1000 == 1){
    print(j)
  }
  valid_tests <- lead_tests[as.Date(lead_tests$Results_Se, format="%m/%d/%Y") < as.Date(transactions[j,]$RecordingDate),]
  temp_tests_coords <- valid_tests
  temp_tests_coords <- temp_tests_coords[!is.na(temp_tests_coords$X),]
  temp_tests_coords <- temp_tests_coords[!is.na(temp_tests_coords$Y),]
  coordinates(temp_tests_coords) <- c("X", "Y") 
  distVec <- spDistsN1(temp_tests_coords, trans_coords[j,], longlat=TRUE)
  log <- valid_tests[distVec<1, ]
  #nr <- nrow(valid)
  
  if (nrow(log) != 0) # there exists lead tests happened before the transactions
  { 
    # merge by distance 1km for transaction i
    #subset leadData_merged
    
    #log <- leadData_merged
    log$Date <- as.Date(log$Results_Se, "%m/%d/%Y")
    
    # Create test dummy indicator
    log$test <- as.integer(1)
    
    # agreggate results by day
    log$positive.tests_byDay <- ave(log$Lead_mgl, log$Date, FUN = sum)
    log$tests_byDay <- ave(log$test, log$Date, FUN = sum)
    
    # subset of results by day  
    daily <- log[!duplicated(log$Date),]
    daily <- daily[as.Date(daily$Date) < as.Date(transactions[j,]$RecordingDate), ]  
    # calculate cumulative probability
    n <- as.integer(nrow(daily))
    daily$cumulative.prob <- 0
    
    for (i in 1:n){
      my.date <- daily$Date[i]
      
      cumul_tests <- sum(daily[which(daily$Date <= my.date),"tests_byDay"])
      cumul_positive.tests <- sum(daily[which(daily$Date <= my.date),"positive.tests_byDay"])
      
      daily[i,"cumulative.prob"] <- cumul_positive.tests/cumul_tests
    }
    
    
    # collect daily mu
    daily.m <- daily[,c("Date","cumulative.prob")]
    
    # bring back mu to original dataset
    log <- merge(log, daily.m, by="Date")
    
    # calculate deviation to each obs
    log$dev <- sqrt((log$Lead_mgl - log$cumulative.prob)^2)
    
    # calculate cumulative standard deviation
    log$deviation_byDay <- ave(log$dev, log$Date, FUN=sum)
    
    # subset of results by day  
    #       daily <- log[!duplicated(log$Date),]
    
    
    for (i in 1:n)
    {
      my.date <- daily$Date[i]
      
      cumul_tests <- sum(daily[which(daily$Date <= my.date),"tests_byDay"])
      cumul_deviation <- sum(daily[which(daily$Date <= my.date),"deviation_byDay"])
      
      daily[i,"cumulative.standard.deviation"] <- (cumul_deviation/cumul_tests)/sqrt(cumul_tests)
    }
    # add the statistics back to the transaction dataset
    transactions[j,]$tests_byDay <- daily[i,]$tests_byDay
    transactions[j,]$deviation_byDay <- log[i,]$deviation_byDay
    transactions[j,]$dev <- log[i,]$dev
    transactions[j,]$positive.tests_byDay <- daily[i, ]$positive.tests_byDay
    transactions[j,]$cumulative.prob <- daily.m[i,]$cumulative.prob
  }
  else
  {
    
  }
}

if (mergeback == 1)
{
  transactions <- transactions[,-c(1,2,3)]
  o_transactions <- o_transactions[,-c(1,2)]
  o_transactions[transactions$X,] = transactions
} else {
write.csv(transactions, "production/transactions_with_stats_1004.csv")
}
#ggplot(daily, aes(y=cumulative.prob, x=Date)) + geom_point() +
#  geom_errorbar(aes(ymax = cumulative.prob + cumulative.standard.deviation,
#                    ymin = cumulative.prob - cumulative.standard.deviation))

##
# calculate the distances from lead tests to the transaction address

