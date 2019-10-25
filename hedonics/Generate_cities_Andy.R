rm(list=ls())
library(data.table)

###############################################################################################################
#                                                                                                             #
# This is a script that extracts certain cities from a given state hedonics data                              #
# Input: ~/share/projects/Zillow_Housing/production/Hedonics/states/STATEHedonics_withTract.rds               #
# Output: ~/share/projects/Zillow_Housing/production/Hedonics/cities/MATCH/CITY_STATE.rds                     #
###############################################################################################################

# Choose a certain state
state <- "PA"
temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))

# Currently there are 4 matches: Grand Blanc, Davison, Burton, Flint
matches <- "Davison"

# Filter by matches
# Cities selection are based on ~/share/projects/Zillow_Housing/stores/Genesee_Matches.xlsx (For Grand Blanc, Davison, Burton, Flit)
# and ~/share/projects/Zillow_Housing/stores/Flint-Matched-Cities_20170808.xlsx (For Flint)
if (matches != "Flint"){
  if (matches == "Grand Blanc"){
    if (state == "IL"){
      counties <- c(toupper("Kankakee"), toupper("Winnebago"))
      zips <- list(c(60901, 60914, 60915, 60950), c(61011, 61073, 61103, 61111, 61115))
      cities <- list(c("Bourbonnais", toupper("Bradley"), toupper("Kankakee")), c(toupper("Loves Park"), toupper("Machesney Park"), toupper("Roscoe")))
      save_name <- c("bourbonnais", "harlem")
    }
    if (state == "IN"){
      counties <- c(toupper("Marion"), toupper("Porter"))
      zips <- list(c(46113, 46183, 46217, 46221, 46231, 46241), c(46304, 46342, 46368, 46385))
      cities <- list(c(), c("Portage", "OGDEN DUNES", "CHESTERTON"))
      save_name <- c("decatur", "portage")
    }
    if (state == "MI"){
      #counties <- c(toupper("Ottawa"), toupper("Wayne"), toupper("Genesee"))
      #cities <- c(toupper("Holland"), toupper("Brownstown"), toupper("Grand Blanc"))
      
      counties <- c(toupper("Eaton"), toupper("Ottawa"), toupper("Kalamazoo"), toupper("Wayne"), toupper("Genesee"))
      zips <- list(c(48837, 48906, 48911, 48917), c(49423, 49424, 49464), c(), c(48134, 48164, 48173, 48174, 48183, 48193), c(48439, 48442, 48507))
      cities <- list(c(), c(toupper("Holland"), toupper("West Ottawa"), toupper("Zeeland")), c("PORTAGE"), c(), c())
      save_name <- c("delta", "holland", "portage", "brownstown", "grand_blanc")
    }
    if (state == "NC"){
      #toupper("Iredell"),toupper("Coddle Creek"),missing
      #counties <- c( toupper("Forsyth"))
      #cities <- c(toupper("Kernersville"))
      
      counties <- c(toupper("Iredell"), toupper("Forsyth"))
      zips <- list(c(28036, 28115, 28117), c(27051, 27101, 27284))
      cities <- list(c("Mooresville"), c("Kernersville", toupper("Walkertown")))
      save_name <- c("coddle_creek", "kernersville")
    }
    if (state == "NM"){
      counties <- c(toupper("Lea"))
      zips <- list(c())
      cities <- list(c("Hobbs"))
      save_name <- c("hobbs")
      
      #cities <- c(toupper("North Whitfield"))
      #temp_trans <- temp_state[County == "LEA"]
      #temp_trans <- temp_trans[TRACTCE10 =="000100" | TRACTCE10 =="000200" | TRACTCE10 =="000300" | TRACTCE10 =="000400"]
      #saveRDS(temp_trans, paste0("~/share/projects/Zillow_Housing/production/Hedonics/cities/", matches, "/whitfield", "_", state, ".rds"))
      #write.csv(temp_trans, paste0("~/share/projects/Zillow_Housing/production/Hedonics/cities/", matches, "/whitefield", "_", state, ".csv"), row.names = FALSE)
      #next
      #tmp <- t1[(as.character(BLOCKCE10) >= "1000" & as.character(BLOCKCE10) <= "1034") | 
      #          (as.character(BLOCKCE10) >= "2000" & as.character(BLOCKCE10) <= "2065") |
      #          (as.character(BLOCKCE10) >= "3000" & as.character(BLOCKCE10) <= "3061") ]
      #t2 <- temp_trans[TRACTCE10 == "000102"]
      #tmp <- t2[(as.character(BLOCKCE10) >= "1000" & as.character(BLOCKCE10) <= "1071") | 
      #          (as.character(BLOCKCE10) >= "2000" & as.character(BLOCKCE10) <= "2162") |
      #          (as.character(BLOCKCE10) >= "3000" & as.character(BLOCKCE10) <= "3048") |
      #          (as.character(BLOCKCE10) >= "4000" & as.character(BLOCKCE10) <= "4062") ]
    }
    if (state == "OH"){
      #counties <- c(toupper("Lucas"))
      #cities <- c(toupper("Springfield"))
      
      counties <- c(toupper("Lucas"))
      zips <- list(c(43528, 43537, 43542, 43615, 43617))
      cities <- list(c())
      save_name <- c("springfield")
    }
    if (state == "TX"){
      counties <- c(toupper("Hunt"))
      zips <- list(c())
      cities <- c("Greenville")
      save_name <- c("greenville")
    }
    if (state == "SC"){
      
    }
  }
  
  if (matches == "Davison"){
    if (state == "CO"){
      counties <- c(toupper("Montezuma"))
      zips <- list(c())
      cities <- list(c(toupper("Cortez")))
      save_name <- c("cortez")
    }
    if (state == "NC"){
      counties <- c(toupper("Stokes"))
      zips <- list(c(27019, 27021, 27041, 27043, 27045, 27050, 27053))
      cities <- list(c(toupper("King"), toupper("Tobaccoville"), toupper("Pinnacle")))
      save_name <- c("yadkin")
    }
    if (state == "MI"){
      #counties <- c(toupper("Monroe"), toupper("genesee"))
      #cities <- c(toupper("Monroe"), toupper("Davison"))
      
      counties <- c(toupper("Monroe"), toupper("genesee"))
      zips <- list(c(48145, 48161), c(48423))
      cities <- list(c(), c())
      save_name <- c("monroe", "davison")
    }
    if (state == "WA"){
      counties <- c(toupper("Mason"))
      zips <- list(c())
      cities <- list(c(toupper("Shelton")))
      save_name <- c("shelton")
    }
    if (state == "GA"){
      #missing
      counties <- c(toupper("Whitfield"))
      #cities <- c(toupper("North Whitfield"))
      temp_trans <- temp_state[County == "WHITFIELD"]
      temp_trans <- temp_trans[TRACTCE10 =="000101" | TRACTCE10 == "000102"]
      saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/whitfield", "_", state, ".rds"))
      write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/whitefield", "_", state, ".csv"), row.names = FALSE)
      next
      #tmp <- t1[(as.character(BLOCKCE10) >= "1000" & as.character(BLOCKCE10) <= "1034") | 
      #          (as.character(BLOCKCE10) >= "2000" & as.character(BLOCKCE10) <= "2065") |
      #          (as.character(BLOCKCE10) >= "3000" & as.character(BLOCKCE10) <= "3061") ]
      #t2 <- temp_trans[TRACTCE10 == "000102"]
      #tmp <- t2[(as.character(BLOCKCE10) >= "1000" & as.character(BLOCKCE10) <= "1071") | 
      #          (as.character(BLOCKCE10) >= "2000" & as.character(BLOCKCE10) <= "2162") |
      #          (as.character(BLOCKCE10) >= "3000" & as.character(BLOCKCE10) <= "3048") |
      #          (as.character(BLOCKCE10) >= "4000" & as.character(BLOCKCE10) <= "4062") ]
      
    }
    if (state == "KY"){
      counties <- c(toupper("Nelson"))
      zips <- list(c())
      cities <- list(c(toupper("Bardstown")))
      save_name <- c("bardstown")
    }
    if (state == "IL"){
      counties <- c(toupper("Winnebago"))
      zips <- list(c(60146, 61016, 61108, 61109, 61112))
      cities <- list(c(toupper("Rockford"), toupper("Cherry Valley")))
      save_name <- c("cherry_valley")
    }
    if (state == "IN"){
      # missing
      #counties <- c(toupper("Kosciusko"), toupper("Jackson"))
      #cities <- c(toupper("Wayne"), toupper("Jackson"))
      
      counties <- c(toupper("Kosciusko"), toupper("Jackson"))
      zips <- list(c(46562, 46580, 46582, 46590), c(47274))
      cities <- list(c("WARSAW"), c(toupper("Seymour")))
      save_name <- c("wayne", "jackson")
    }
  }
  
  if (matches == "Burton"){
    if (state == "CO"){
      counties <- c(toupper("Adams"))
      zips <- list(c())
      cities <- list(c(toupper("Commerce city")))
      save_name <- c("commerce_city")
    }
    if (state == "PA"){
      counties <- c(toupper("Lebanon"))
      cities <- c(toupper("Lebanon"))
    }
    if (state == "MI"){
      # missing toupper("Monroe"), toupper("Frenchtown"),
      #counties <- c(toupper("Oakland"), toupper("Genesee"))
      #cities <- c(toupper("Madison Heights"),  toupper("Burton"))
      
      counties <- c(toupper("Oakland"), toupper("Monroe"), toupper("Genesee"))
      zips <- list(c(), c(48117, 48161, 48162, 48166), c())
      cities <- list(c(toupper("Madison Heights")), c(), c("BURTON"))
      save_name <- c("madison_heights", "monroe", "burton")
    }
    if (state == "WI"){
      counties <- c(toupper("Rock"))
      cities <- c(toupper("Beloit"))
    }
    if (state == "OH"){
      #counties <- c(toupper("Ross"))
      #cities <- c(toupper("Scioto"))
      
      counties <- c(toupper("Ross"))
      zips <- list(c(45601))
      cities <- list(c())
      save_name <- c("scioto")
    }
    if (state == "NC"){
      #counties <- c(toupper("Rowan"), toupper("Lincoln"), toupper("Burke"))
      #cities <- c(toupper("China Grove"), toupper("Lincolnton"), toupper("Morganton"))
      
      counties <- c(toupper("Rowan"), toupper("Lincoln"), toupper("Burke"))
      zips <- list(c(28023, 28081, 28083, 28088, 28138, 28146, 28147), c(28092), c(28655, 28690))
      cities <- list(c(toupper("Kannapolis"), toupper("China Grove"), toupper("Landis")), c(toupper("Lincolnton")), c(toupper("Morganton")))
      save_name <- c("china_grove", "lincolnton", "morganton")
    }
    if (state == "IL"){
      counties <- c(toupper("Stephenson"))
      zips <- list(c(61032))
      cities <- list(c(toupper("Freeport")))
      save_name <- c("freeport")
    }
    if (state == "IN"){
      # , toupper("Henry"), toupper("Henry")
      #counties <- c(toupper("Lake"), toupper("Huntington"))
      #cities <- c(toupper("Hobart"), toupper("Huntington"))
      
      counties <- c(toupper("Lake"), toupper("Henry"), toupper("Huntington"))
      zips <- list(c(46342, 46402, 46405, 46407, 46409), c(47362), c(46702, 46750))
      cities <- list(c(toupper("Gary"), toupper("Hobart"), toupper("Lake Station")), c(toupper("New Castle")), c(toupper("Huntington")))
      save_name <- c("hobart", "henry", "huntington")
    }
    if (state == "CT"){
      counties <- c(toupper("Windham"))
      zips <- list(c())
      cities <- c(toupper("Windham"))
      save_name <- c("")
    }
    if (state == "KY"){
      counties <- c(toupper("Henderson"))
      zips <- list(c())
      cities <- list(c(toupper("Henderson")))
      save_name <- c("henderson")
    }
  }
  
  i = 1
  for (city in cities){
    county <- counties[i]
    zip <- zips[i] 
    temp_trans <- temp_state[County == county]
    if (!is.null(city[[1]])){
      temp_trans <- temp_trans[PropertyCity %in% city[[1]]]
    }
    if (!is.null(zip[[1]])){
      temp_trans <- temp_trans[PropertyZip %in% zip[[1]]]
    }
    #temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", save_name[i], "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", save_name[i], "_", state, ".csv"), row.names = FALSE)
    i <- i + 1
  }
  # for (city in cities){
  #   county <- counties[i]
  #   temp_trans <- temp_state[County == county & PropertyCity == city]
  #   #temp_trans <- temp_state[temp_state$PropertyCity == city,]
  #   saveRDS(temp_trans, paste0("~/share/projects/Zillow_Housing/production/Hedonics/cities/", matches, "/", tolower(city), "_", state, ".rds"))
  #   write.csv(temp_trans, paste0("~/share/projects/Zillow_Housing/production/Hedonics/cities/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  #   i <- i + 1
  # }
} else {
  ##############################################################
  
  state <- "MI"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))
  
  cities <- c("BATTLE CREEK", "LANSING", "KALAMAZOO", "WARREN", "PONTIAC", "SOUTHFIELD", "SAGINAW", "TAYLOR", "DEARBORN", "WESTLAND")
  cities_save <- c("battle_creek", "lansing", "kalamazoo", "warren", "pontiac", "southfield", "saginaw", "taylor", "dearborn", "westland")
  
  for (city in cities){
    temp_trans <- temp_state[temp_state$PropertyCity == city,]
    city_save <- cities_save[which(cities == city)]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  }
  
  ##############################################################
  
  state <- "CT"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))
  
  cities <- c("BRIDGEPORT", "HARTFORD")
  cities_save <- c("bridgeport", "hartford")
  
  for (city in cities){
    #county <- counties[i]
    #temp_trans <- temp_state[County == county & PropertyCity == city]
    temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  }
  
  ##############################################################
  
  state <- "DE"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))
  
  cities <- c("WILMINGTON")
  cities_save <- c("wilmington")
  
  for (city in cities){
    #county <- counties[i]
    #temp_trans <- temp_state[County == county & PropertyCity == city]
    temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  }
  
  ##############################################################
  
  state <- "IL"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))
  
  cities <- c("THORNTON", "BLOOMINGTON", "DECATUR")
  cities_save <- c("thornton", "bloomington", "decatur")
  
  for (city in cities){
    #county <- counties[i]
    #temp_trans <- temp_state[County == county & PropertyCity == city]
    temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  }
  
  ##############################################################
  
  
  state <- "IN"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))

  counties <- c("ALLEN", "DELAWARE", "LAKE", "MARION", "ST JOSEPH")
  cities <- list(c("FORT WAYNE", "NEW HAVEN", "WOODBURN"), c("MUNCIE"), c("CROWN POINT", "EAST CHICAGO", "GARY", "HAMMOND", "HOBART", "LAKE STATION", "WHITING"), c("INDIANAPOLIS", "BEECH GROVE"), c("SOUTH BEND", "MISHAWAKA"))
  save_name <- c("wayne", "center_delaware", "calumet", "center_marion", "portage_st_joseph")
  i = 1
  for (city in cities){
    county <- counties[i]
    temp_trans <- temp_state[County == county & PropertyCity %in% city[[1]]]
    #temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", save_name[i], "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", save_name[i], "_", state, ".csv"), row.names = FALSE)
    i = i + 1
  }
  
  ##############################################################
  
  state <- "NJ"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))
  
  cities <- c("CAMDEN", "EAST ORANGE", "IRVINGTON", "TRENTON")
  cities_save <- c("camdem", "est_orange", "irvington", "trenton")
  
  for (city in cities){
    #county <- counties[i]
    #temp_trans <- temp_state[County == county & PropertyCity == city]
    temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  }
  
  ##############################################################
  
  state <- "OH"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))
  
  cities <- c("YOUNGSTOWN", "DAYTON")
  cities_save <- c("youngstown", "dayton")
  
  for (city in cities){
    #county <- counties[i]
    #temp_trans <- temp_state[County == county & PropertyCity == city]
    temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  }
  
  ##############################################################
  
  state <- "PA"
  temp_state <- as.data.table(readRDS(paste0("/home/bdeep/share/projects/Zillow_Housing/stores/Hedonics/State_Hedonics_Census/", state, "Hedonics_withTract_2019.rds")))
  
  cities <- c("READING")
  cities_save <- c("reading")
  
  for (city in cities){
    #county <- counties[i]
    #temp_trans <- temp_state[County == county & PropertyCity == city]
    temp_trans <- temp_state[temp_state$PropertyCity == city,]
    saveRDS(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".rds"))
    write.csv(temp_trans, paste0("/home/bdeep/share/projects/Zillow_Housing/production/Hedonics/cities_2019/", matches, "/", tolower(city), "_", state, ".csv"), row.names = FALSE)
  }
}
