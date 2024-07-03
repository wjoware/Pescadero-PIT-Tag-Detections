# Created - 07/02/2024
# Modified
  # 07/02/2024 @ 2:40 PM - corrected code errors & added explanatory comments

# Part I. Set up
#----------------------------------------------
  
  # empty global working environment
    rm(list = ls())

install.packages("xlsx")
  
# load packages
    library(dplyr)
    library(readxl)
    library(lubridate)
    library("xlsx")

  # set reference folder
    setwd("/Users/wware/Library/CloudStorage/OneDrive-ucsc.edu/Records/Jobs/CalTrout/Data.Comp")

# Part II. Import datasets
# ------------------------------------------------
  dat <-read_excel("CalTrout queries 010324.xlsx")
    # CDFW steelhead seine data (queried on 1/03/2024)
  dat2 <-read_excel("Pesacdero_PISCES_PITs_20231017.xlsx")
    # NMFS coho & steelhead data (queried 10/17/2023)
  dat3 <-read_excel("Pescadero_RLS_Records_20240208.xlsx")
    # NMFS coho releases (queried 2/08/2024)

# Part III. Wrangle data
# ----------------------------------------
  
  # change ID to show all digits rather than scientific notation
    dat2$PITNum <- format(dat2$PITNum, scientific = F)
    dat3$PITNum <- format(dat3$PITNum, scientific = F)
    
  # remove unneeded column
    dat4 <- dat2[, -1]
    
# rename different column names for PIT ID
  dat <- dat %>% rename("PITNum" = "Fish.PITNum")  


# compile
  dat5 <- full_join(dat, dat3, dat4, by = "PITNum")
    # problem merging so please explore other methods
# Remove column "PTArrays.PITNum" because values are the same/redundant for "PITNum"
  dat5$PITArrays.PITNum <- NULL
  
# Convert 'Time' column to a datetime object (if not already)
  dat5 <- dat5 %>% mutate(Time = ymd_hms(Time))
  
  
# Remove duplicate rows by 'PITNum', keeping the earliest 'Time' for each 'PITNum'
  dat6 <- dat5 %>%
    group_by(PITNum) %>%
    arrange(Time) %>%
    slice(1) %>%
    ungroup()
  
  
# Check to make sure that number of rows in final cleaned data ('dat6') 
# matches the amount of duplicate rows in pre-cleaned data ('dat5')
  unique_pitnum_count <- dat5 %>% distinct(PITNum) %>% nrow()
  cleaned_row_Count <- nrow(dat6)
  
    # Values for both are "169" and thus all duplicates were deleted, and all 
    # necessary values were kept (nothing important was lost)
  
  
# Join "Species.x" and "Species.y", "LifeStage.x" and "LifeStage.y", "Site.x" and "Site.y"
  dat7 <- dat6 %>%
    mutate(Species = coalesce(Species.x, Species.y),
           LifeStage = coalesce(LifeStage.x, LifeStage.y),
           Site = coalesce(Site.x, Site.y))

  
# Relocate new columns "Species", "LifeStage", and "Site" from end of sheet
  dat7 <- dat7 %>% relocate(Species, LifeStage, Site, .after = PITNum)
  
  
# Delete old '.x' and '.y' columns
  dat7 <- dat7 %>% select(-Species.x,-Species.y,-LifeStage.x,-LifeStage.y,
                          -Site.x,-Site.y)

# Write cleaned "dat7" file as a .xlsx file
  write.xlsx(dat7, file = "Cleaned_PIT_ID_Data.xlsx",
             sheetName = "PIT Data", append = F)
  
  
# Part IV. Next steps (suggested)
  #----------------------------------------------------
  # 1. check if you removed only duplicate PIT ID #'s
  # 2. review available life stage data per PIT ID #
  
  
  
  
  
  
  