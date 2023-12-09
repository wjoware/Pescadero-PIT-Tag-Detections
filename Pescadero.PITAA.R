# empty global working environment
  rm(list = ls())

# load packages
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(data.table)
  library(lubridate)
  library(boot)
  library(ggplot2)
  library(ggpubr)
  library(grid)
  library(cowplot)
  library(generics)

# set working directory
  setwd("/Users/wware/Library/CloudStorage/OneDrive-ucsc.edu/Records/Jobs/CalTrout/Projects/Central Coast/Pescadero PITAA/Data")
  
# -----------------------------------------------------------------------------
# Import & compile Pescadero detections from NOAA & CDFW 
  
 # Step 1: Prepare CDFW records for Pescadero Creek detections for merging with other data
      
    # a: import data
      dat <-read_excel("Caltrout queries 050823.xlsx")
        # detections from 2021-12-17 to 2023-03-03 queried on 2023-05-08
  
    # b: review data
      head(dat) # first 6 rows
      tail(dat) # last 6 rows
      str(dat) # format of each column (not how dates are in character format)
      
    # c: change "Date" column from character to date variable
      # check format of Time variable
        str(dat$Time)
      # make variable for date from Time variable
        dat$DetDate <- as.Date(dat$Time, "%Y-%m-%d", tz = "UTC") 
      # change "Date" to date format with Universal Time Coordinated timezone
        str(dat$DetDate)
      
    # d: reformat variables
      # 1. rename columns to align with other dataframes & streamline data sorting
        dat <- dat %>% rename("ID" = "PITArrays.PITNum",  # PIT number
                              "Origin" = "Sample Method", # sampling methods
                              "FL" = "FL(mm)")            # fork length
      # 2. add column
        dat$DetMonth <- as.Date(dat$DetDate, "%m/%Y", tz = "UTC")
        dat$DetMonth <- format(dat$DetMonth, "%m/%Y")
        
      # 3. change ID to show all digits rather than scientific notation
        dat$ID <- format(dat$ID, scientific = F)
       
    # e: only select the first & last detection by PIT #, antenna, & dat
      dat2 <- dat %>% group_by(ID, Antenna, DetDate,  DetMonth, Species, SampDate, 
                               FL, Origin, Reach, Site, Marked, LifeStage) %>% 
          # group all other variables in dat by PIT #, antenna, & date
        summarise(min(Time), max(Time))
          # only select the 1st & last detections by the prior groupings (PIT #, antenna, date)
    
    # f: clean data
      # 1. remove the last column
        dat3 <- dat2[-c(14)]
          # duplicate time data is an artifact of selecting timestamps
      # 2. rename the last column
        dat3 <- dat3 %>% rename("Time" = `min(Time)`)
        # min(Time) was actually the detection time
      
    # g. remove physical test tags & those that are suspicious of
      # 1. flagged PIT tag numbers
          dat4 <- dat3 %>%
          filter(!ID %in% c(982000365411489, 982000365411491, 982000365411625, 982000365411657, 982000365411680,
                              # 4 physical test tags 
                            900226001052951,  
                              # suspect - determined by NOAA SW Fisheries Sci Center 
                            982126057446372)) 
                              # suspect - 5960 detections in "dat"
          # Result: 25 rows removed from "dat3"
        
      # 2. PIT tag numbers ending in 5 zeroes
        dat5 <- dat4 %>% filter(!str_detect(ID, "00000$"))
          # Result: nothing changed so these ghost tags were not found in the dataframe
    
  # Step 2: Prepare NOAA detections on Pescadero Creek for merging with other data
    
    # a: import data 
      dat6 <- read_excel("NOAA-Pescadero&Butano.Detections.xlsx", 3)
        # NOAA database records for Pescadero Creek detections of NOAA fish from 
          # Oct. 2nd, 2017 to Jan. 25th, 2022
    
    # b: reformat & edit variables
      # b1. remove redundant PIT number columns so that one can be manipulated
        dat7 <- dat6[,-c(1,2)]
        
      # b2. rename column headers to facilitate subsequent merging with other datasets
        dat7 <- dat7 %>% 
          rename("ID" = "PITNum", "Time" = "Timestamp", 
                 "Notes" = "NoteRecords", "DetDate" = "Date")
        
      # b3. show full numbers rather than scientific notation
        dat7$ID <- format(dat7$ID, scientific = F)
    
    
      # b4: add month to group detections by
        dat7$DetMonth <- format(dat7$DetDate, "%m/%Y")
      
      # b5: add a column to merge dat6 with other dataframes
        dat7$Antenna <- rep("Scanned", 45)
      
    # d: QA/QC observations
      # remove select PIT tag numbers
          dat8 <- dat7 %>%
            filter(!ID %in% c(982000365411489, 982000365411491, 982000365411625, 982000365411657, 982000365411680,
                                # 4 physical test tags 
                              900226001052951, 982126057446372)) 
                                # suspect tags - determined by NOAA Southwest Fisheries Sci Center
          # Result: none of these observations were in the dataframe
      
    # e. check whether observations in dat8 are for different fish
      length(unique(dat8$ID)) # 31 different fish
        
  # Step 3: Prepare more NOAA records Pescadero Creek detections for merging with other data
    
    # a: import data
      dat10 <- read.csv("NOAA-Pescadero&Butano.Detections-sheet1.csv")
        # Pescadero detections from 2021-12-07 to 2022-07-05 reviewed by NOAA
    
    # b: review data
      head(dat10) # headers & first 6 rows
      tail(dat10) # headers & last 6 rows
      str(dat10)  # format of each variable
      
    # c: remove select rows & columns
      # 1: unxeplained rows & columns
        dat11 <- dat10[-81, # no explanatory header name 
                      -4] # last row contains false headers
      
      # 2: suspected ghost tags
        dat12 <- dat11 %>% filter(!Notes == "Potentially corrupt PIT tag number",  
                                    # Result of "!Notes" argument: removes 13 suspect observations
                                  !ID %in% c(982000365411489, 982000365411491, 982000365411625, 982000365411657, 982000365411680,
                                              # above are 4 physical test tags 
                                             900226001052951, 982126057446372))
                                              # suspect tags - determined by NOAA Southwest Fisheries Sci Center
                                    # Result of "!ID" argument: removes 6 suspect observations
      
      # 3: filter detection records
        # only select the first & last detection by PIT #, antenna, & date
          dat13 <- dat12 %>% group_by(ID, Species, Site, Origin, Watershed, Notes) %>% 
            # group all other variables in dat by PIT #, antenna, & date
          summarise(min(Time), max(Time))
            # only select the 1st & last detections by the prior groupings (PIT #, antenna, date)
          # Result: 1918 observations removed
    
    # c: edit variables
        # 1. convert "Time" variable to time object in R
          # remove more unneded rows & columns
            dat14 <- dat13[-1, # empty row
                         -8] # duplicate column
          # 2. edit column header since min(Time) is actually the detection time
            dat14 <- dat14 %>% rename("Time" = `min(Time)`)
        
          # 3. format Time variable
            str(dat14$Time) # series of character strings
            dat14$Time <- ymd_hms(dat14$Time) # convert to time data
            str(dat14$Time) # worked!
            
          # 4. reformat ID
            # change to numeric variable
              dat14$ID <- as.numeric(dat14$ID)
            # show all digits
              dat14$ID <- format(dat14$ID, scientific = F)
      
    # d: make a dataframe for Pescadero detections from NOAA data
        # 1: select data
          dat15 <- dat14 %>% filter(Site %in% 
                                c("Pescadero_Upstream", "Pescadero_Downstream"))
        
        # 2: reformat variables for detection antenna in NOAA data
          # change to factor
            dat15$Site <- factor(dat15$Site) 
            str(dat15$Site) # worked
        
        # 3: rename values to align with other dataframes 
          dat15$Site <- recode_factor(dat15$Site, 
                        "Pescadero_Upstream" = "1", "Pescadero_Downstream" = "2")
        
        # 4: rename column to align with other dataframes  
          dat15 <- dat15 %>% rename("Antenna" = "Site")
          
        # 5: add columns to merge dat8 with other dataframes
          # dates to group detections by
            dat15$DetDate <-  as.Date(dat15$Time, "%Y-%m-%d", tz = "UTC")
          # months to group detections by
            dat15$DetMonth <- format(dat15$Time, "%m/%Y", tz = "UTC")
        
        # 6: check whether all 63 PIT numbers in dat11 are unique or repeats
          length(unique(dat15$ID)) # all are unique
        
  # Step 4 Join NOAA dataframes for its fish detected in Pescadero Creek
      # a: join dataframes by common columns 
        dat16 <- full_join(dat8, dat15, 
                           by = c("ID", "Time", "DetDate", "DetMonth", "Antenna", 
                                  "Species", "Watershed", "Origin", "Notes"))
        
        
      # b: select only the first 15 columns & also the 53 column
        dat17 <- select(dat16, 1:15, 52:53)
          # others have sparse data
  
  # Step 5: join CDFW dataframes with previously compiled NOAA dataframes
        dat18 <- full_join(dat5, dat17, by = c("ID", "Time", "Site", "DetDate", "DetMonth"))
        

# Import & compile Butano detections from NOAA, CDFW, & raw BioLogic --------------------------
    
      # Step 1: Prepare CalTrout detections for merging with other data - Giannini site
        # a: import:
          dat19 <-read.csv("Giannini.Butano.Detections.csv")
        # b: revas.Piew
          head(dat19) # first 6 rows
          tail(dat19) # last 6 rows
          str(dat19) # format of each column (not how dates are in character format)
        
        # c: reformat variables
            # 1. show full PIT # rather than just scientific notation
              dat19$ID <- format(dat19$ID, scientific = F)
              
            # 2. rename columns
              dat19 <- dat19 %>% rename("Time" = "detected", 
                                        "Antenna" = "antenna")
              
            # 3. make antenna a factorial variable
              dat19$Antenna <- factor(dat19$Antenna, levels = c("1","2"))
              
            # 4: change detection times from character string
              dat19$Time <- as.POSIXct(dat19$Time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
              str(dat19$Time) # worked!
          
        # d: filter detections
          # remove duplicates
            dat20 <- dat19 %>% group_by(ID, Antenna) %>% 
                    # group by PIT # & antenna
                   summarise(min(Time), max(Time))
                    # only select first & last detection by PIT # & antenna
              # Result: 262 detections removed
            
            # remove select PIT tag numbers
            dat21 <- dat20 %>%
              filter(!ID %in% 
                       c(982000365411489, 982000365411491, 982000365411625, 982000365411657, 982000365411680,
                          # 4 physical test tags 
                         900226001052951, 982126057446372)) 
                          # suspect tags - determined by NOAA Southwest Fisheries Sci Center
            # Result: 9 observations removed
        
        # e: reformat selected detections
            # 1. drop unneccesary columns
              dat22 <- dat21[-c(4)]
            # 2. rename last column
              dat22 <- dat22 %>% rename("Time" = `min(Time)`)
                # min(Time) was actually the detection time
        
        # f: create columns
          # 1: create variable for date of detection
            dat22$DetDate <- as.Date(dat22$Time, "%Y-%m-%d", tz = "UTC")
          # 2: create variable for month of detection  
            dat22$DetMonth <- format(dat22$Time, "%m/%Y") 
          # 3: create a new variable species
            # a: cross check unidentified fish in compiled Butano Creek detections (dat18) vs.
              # identified fish in compiled Pescadero Creek detections (dat15)
            dat22$ID %in% dat18$ID
              # result: 5 common PIT tag numbers: 
                # 3 of the common numbers are hysical test tags
                  # 982000365411625, 982000365411680,982000365411711
                # O. mykiss under "Species column in "dat15"
                  # 982126057446191 & 982126057448876
          
    # Step 5: prepare CDFW detections for merging with NOAA & raw BioLogic data - Giannini site
        # a: create dataframe
          dat23 <- dat5 %>% filter(DetMonth %in% c("01/2023", "02/2023", "03/2023"))
        # b: change format of variables
          # check format
            str(dat23$Antenna) # number without levels
          # reformat
            dat23$Antenna <- factor(dat23$Antenna, levels = c("1","2"))
          # check format
            str(dat23$Antenna) # factor with levels
            
    # Step 7: merge NOAA, CDFW, & raw BioLogic detections on Butano Creek - Giannini site
        # a: merge O. mykiss observations together
            dat24 <- full_join(dat22, dat23, 
                               by = c("ID", "Antenna", 
                                      "Time", "DetDate", "DetMonth"))
            # Result: all values merged, including 3 duplicates across dataframes
            
    # Step 8: import raw detections on Butano Creek from BioLogic - Reynolds site
      
      # a. import detections near Reynolds farm field
          dat25 <- read.csv("Reynolds-Butano.Detections.csv")
          
      # b. show full PIT # rather than just scientific notation
          dat25$ID <- format(dat25$ID, scientific = F)
          
      # c. edit variables
          # 1. rename columns
            dat25 <- dat25 %>% rename("Time" = "detected", 
                                      "Antenna" = "antenna")
            
          # 2. reformat variables
            # detection time
             dat25$Time <- as.POSIXct(dat25$Time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
             str(dat25$Time) # worked
            # antenna
             dat25$Antenna <- factor(dat25$Antenna, levels = c("1","2"))
             str(dat25$Antenna) # worked
             
          # 3. add columns
            # detection date
              dat25$DetDate <- as.Date(dat25$Time, "%Y-%m-%d", tz = "UTC")
            # detection month
              dat25$DetMonth <- format(dat25$Time, "%m/%Y", tz = "UTC")
              
      # d. check duplicate observations
         # view number of detections by ID
          dat26 <- dat25 %>% count(ID)
          
      # e. select unique detections
         dat27 <- dat25 %>% group_by(ID, DetDate, DetMonth, Antenna) %>%
           summarise(min(Time), max(Time))
         # Result: 2048 observations removed
      
      # f. remove select observations
         # cleave the last column
          dat28 <- dat27[-c(6)]
          
        # cleave select PIT tag numbers
          dat29 <- dat28 %>%
            filter(!ID %in% c(982000365411489, 982000365411491, 982000365411625, 982000365411657, 982000365411680,
                                # 5 physical test tags used by CalTrout & San Mateo RCD
                              900226001052951, 982126057446372)) 
                                # 2 suspect tags - determined by NOAA Southwest Fisheries Sci Center
          # Result: 15 observations removed
      
      # g. edit variables
        # rename last column
         dat29 <- dat29 %>% rename("Time" = `min(Time)`)
          # min(Time) is actually the detection time
         
      # h. merge NOAA data record for Butano Creek with detections at Reynold's site
         # h1. select data
          dat30 <- filter(dat14, Site == "Butano_Upstream" | Site == "Butano_Downstream")
         
        # h2. reformat columns
            # rename antenna variable
              dat30 <- dat30 %>% rename("Antenna" = "Site")
            
            # change antenna name to number
              dat30$Antenna <- recode_factor(dat30$Antenna, 
                               "Butano_Upstream" = "1", "Butano_Downstream" = "2")
          
            # change antenna to a factor variable to align with other dataframes 
              dat30$Antenna <- factor(dat30$Antenna, levels = c("1", "2"))
            
        # h3. add columns
            # detection date
              dat30$DetDate <- as.Date(dat30$Time, "%Y-%m-%d", tz = "UTC")
              str(dat30$DetDate) # changed to Date variable
            # detection month
              dat30$DetMonth <- format(dat30$Time, "%m/%Y")
              str(dat30$DetMonth) # changed from time variable to character variable
                
      # h4. merge dataframes for Butano Creek detections at Reynold's site
            dat31 <- full_join(dat29, dat30, by = c("ID", "Time", "DetDate", 
                                                    "DetMonth", "Antenna"))
            # Note: the two records from NOAA data (dat30) were already in dat29
                    # NOAA data provides more context on the coho record
                    # these records were not duplicated when joined
            
  # Step 9: merge Butano Creek detections from Giannini & Reynold's sites together
            dat32 <- full_join(dat24, dat31, 
                               by = c("ID", "Time", "DetDate", 
                                      "DetMonth", "Antenna"))
            # Result: all observations merged & the 4 duplicates from dat25 were not repeated
            
# Import NOAA PISCES records --------------------------------------------------------------
            
  # NOAA observations of mortalities (MOR), releases (RLS), & tagging (TAG) 
    # events in Pescadero Creek watershed
    noaa.PISCES <- read.csv("Pesacdero_PISCES_PITs_20231017.csv")
    noaa.PISCES$PITNum <- format(noaa.PISCES$PITNum, scientific = F)
    noaa.PISCES <- noaa.PISCES[, -1]
    noaa.PISCES <- noaa.PISCES %>% rename("ID" = "PITNum")
    noaa.PISCES <- noaa.PISCES %>% rename("EventDate" = "Date")
    
# Compile PIT ID for detected fish observed across projects by creek --------
  
  # I. Butano Creek
      
      # 1a. merge PIT IDs from BioLogic & NOAA PISCES records
        dat32$ID %in% noaa.PISCES$ID
          # dat32 IDs in PISCES tagging, mortality, & release records
        Bs <- dplyr::inner_join(dat32, noaa.PISCES, by = "ID")
          # select PIT IDs shared in both dataframes
        Bs2 <- Bs[-c(6:17,23:24,30)]
          # remove columns with no values
        Bu <- dplyr::anti_join(dat32, noaa.PISCES, by = "ID")
          # select PIT IDs from BioLogic & NOAA PIT detections on Butano Creek
            # that were not observed by other NOAA sampling
        
      # 1b. join Butano records together
        B.dat <- rbind(Bs2, Bu)
          # combine shared & unshared records from Butano Creek
      
      # 1c. combine duplicate B.dat columns together
        # vector for each variable that requires compiling
          col.comb <- c("Watershed", "Watershed.y")
          col.comb2 <- c("LifeStage", "LifeStage.y")
          col.comb3 <- c("Species", "Species.x", "Species.y")
          col.comb4 <- c("Site", "Site.y")
          col.comb5 <- c("Origin.x", "Origin.y")
      
      # 1d. edit variables
        # Compile duplicate columns
          # Watershed
            B.dat2 <- B.dat %>%                                          
              dplyr::mutate(Watershed = 
                              invoke(coalesce, across(all_of(col.comb)))) %>%
              dplyr::select(Watershed, 
                            colnames(B.dat)[! colnames(B.dat) %in% col.comb])                                          
          # Life stage
            B.dat3 <- B.dat2 %>%  
              dplyr::mutate(Lifestage = 
                              invoke(coalesce, across(all_of(col.comb2)))) %>%
              dplyr::select(Lifestage, 
                            colnames(B.dat2)[! colnames(B.dat2) %in% col.comb2])
      
          # Species
            B.dat4 <- B.dat3 %>%  
              dplyr::mutate(Species = 
                              invoke(coalesce, across(all_of(col.comb3)))) %>%
              dplyr::select(Species, 
                            colnames(B.dat3)[! colnames(B.dat3) %in% col.comb3])
      
          # Site
            B.dat5 <- B.dat4 %>%  
              dplyr::mutate(Site = 
                              invoke(coalesce, across(all_of(col.comb4)))) %>%
              dplyr::select(Site, colnames(B.dat4)[! colnames(B.dat4) %in% col.comb4])
      
          # Origin
            B.dat6 <- B.dat5 %>%  
              dplyr::mutate(Origin = 
                              invoke(coalesce, across(all_of(col.comb5)))) %>%
              dplyr::select(Origin, colnames(B.dat5)[! colnames(B.dat5) %in% col.comb5])
      
        # Change O. mykiss species code to align with other dataframes
          B.dat6$Species <- str_replace_all(B.dat6$Species, 'onmy', 'Onmy')
    
        # Change detection month to factorial variable
          B.dat6$DetMonth <- factor(B.dat6$DetMonth, 
                                      levels = c("12/2021", "05/2022", "08/2022", 
                                                 "10/2022", "11/2022", "12/2022", 
                                                 "01/2023", "02/2023", "03/2023",
                                                 "04/2023", "05/2023", "06/2023", 
                                                 "07/2023"))
    
 # II. Pescadero Creek
      
      # 1a.   
        dat18$ID %in% noaa.PISCES$ID
        Ps <- dplyr::inner_join(dat18, noaa.PISCES, by = "ID") 
          # 46 PIT IDs from dat18 in PISCES
        Ps2 <- Ps[ , !names(Ps) %in% c("Subsite", "TaggerID", "Mass_g", "X")]
        Pu <- dplyr::anti_join(dat18, noaa.PISCES, by = "ID")
          # 165 PIT IDs from dat18 not in PISCES
        
      # 1b. join dataframes
        P.dat <- rbind(Ps2, Pu)
        
      # 1c. combine duplicate B.dat columns together
        # vector for each variable that requires compiling
          col.comb6 <- c("Antenna.x", "Antenna.y")
          col.comb7 <- c("Species.x", "Species.y")
          col.comb8 <- c("LifeStage.x", "LifeStage.y")
          col.comb9 <- c("Site.x", "Site.y")
          col.comb10 <- c("Event.x", "Event.y")
          col.comb11 <- c("Watershed.x", "Watershed.y")
          col.comb12 <- c("ReachID.x", "ReachID.y")
          col.comb13 <- c("Latitude.x", "Latitude.y")
          col.comb14 <- c("Longitude.x", "Longitude.y")
          col.comb15 <- c("CoordinatorID.x", "CoordinatorID.y")
          col.comb16 <- c("Organization", "Organization.x", "Organization.y")
        
      # 1d. edit variables
        
        # compile duplicate columns
          
          # Antenna
            # change to character variable
              P.dat$Antenna.x <- as.character(P.dat$Antenna.x)
            # compile
              P.dat2 <- P.dat %>%  
                dplyr::mutate(Antenna = 
                          invoke(coalesce, across(all_of(col.comb6)))) %>%
              dplyr::select(Antenna, 
                        colnames(P.dat)[! colnames(P.dat) %in% col.comb6])
              
          # Species
              P.dat3 <- P.dat2 %>%  
                dplyr::mutate(Species = 
                                invoke(coalesce, across(all_of(col.comb7)))) %>%
                dplyr::select(Species, 
                              colnames(P.dat2)[! colnames(P.dat2) %in% col.comb7])
        
          # LifeStage
              P.dat4 <- P.dat3 %>%  
                dplyr::mutate(LifeStage = 
                                invoke(coalesce, across(all_of(col.comb8)))) %>%
                dplyr::select(LifeStage, 
                              colnames(P.dat3)[! colnames(P.dat3) %in% col.comb8])
              
          # Site
              P.dat5 <- P.dat4 %>%  
                dplyr::mutate(Site = 
                                invoke(coalesce, across(all_of(col.comb9)))) %>%
                dplyr::select(Site, 
                              colnames(P.dat4)[! colnames(P.dat4) %in% col.comb9])
              
          # Event
              P.dat6 <- P.dat5 %>%  
                dplyr::mutate(Event = 
                                invoke(coalesce, across(all_of(col.comb10)))) %>%
                dplyr::select(Site, 
                              colnames(P.dat5)[! colnames(P.dat5) %in% col.comb10])
              
          # Watershed
              P.dat7 <- P.dat6 %>%  
                dplyr::mutate(Watershed = 
                                invoke(coalesce, across(all_of(col.comb11)))) %>%
                dplyr::select(Watershed, 
                              colnames(P.dat6)[! colnames(P.dat6) %in% col.comb11])
              
          # ReachID
              P.dat8 <- P.dat7 %>%  
                dplyr::mutate(ReachID = 
                                invoke(coalesce, across(all_of(col.comb12)))) %>%
                dplyr::select(ReachID, 
                              colnames(P.dat7)[! colnames(P.dat7) %in% col.comb12])
              
          # change detection month to factorial variable
              P.dat8$DetMonth <- factor(P.dat8$DetMonth, 
                                        levels = c("10/2017", "11/2017", "04/2018",
                                                   "05/2018", "10/2019", "11/2019",
                                                   "12/2019", "03/2021", "04/2021", 
                                                   "05/2021", "12/2021", "01/2022", 
                                                   "03/2022", "04/2022", "05/2022", 
                                                   "06/2022", "07/2022", "08/2022", 
                                                   "09/2022", "10/2022", "11/2022", 
                                                   "12/2022", "01/2023", "02/2023", 
                                                   "03/2023"))
          
# --------------------------------------------------------------------------------------
# Visualize data 
            
  # Step 1: Pescadero detections
        
    # a: separate by field season
      # 2021 water year
        P.21 <- P.dat8 %>% filter(DetMonth %in% c("12/2021", "01/2022","02/2022", "03/2022",
                                                 "04/2022", "05/2022", "06/2022", "07/2022",
                                                 "08/2022", "09/2022"))
          # edit Species variable
            # replace "NA" values for species with "onmy"
              P.21$Species <- P.21$Species %>% replace(is.na(.), "Onmy")
                # assumes all fish other than noted coho are steelhead
            # replace "onki" with "Onki"
              P.21$Species[P.21$Species == "onki"] <- "Onki"
            
        # set "Month" as factorial variable
        P.21$DetMonth <- factor(P.21$DetMonth, levels = 
                               c("10/2021", "11/2021", "12/2021", "01/2022",
                                 "02/2022", "03/2022", "04/2022", "05/2022", 
                                 "06/2022", "07/2022", "08/2022", "09/2022"))
      # 2022 water year
        P.22 <- P.dat8 %>% filter (DetMonth %in% c("10/2022", "11/2022", "12/2022"))
        # set "Month" as factorial variable
          P.22$DetMonth <- factor(P.22$DetMonth, 
                             levels = c("10/2022", "11/2022", "12/2022", 
                                        "01/2023", "02/2023", "03/2023",
                                        "04/2023", "05/2023", "06/2023",
                                        "07/2023", "08/2023", "09/2023"))
        
        # edit Species variable
          # replace "NA" values for species with "onmy"
            P.22$Species <- P.22$Species %>% replace(is.na(.), "Onmy")
            
  # b: plot results
        
      # y-axis limit
        y1 = c(0, 40)      
    
  # Pescadero Creek
    # 2021 field season
      p1 <- ggplot(P.21, aes(x = DetMonth, fill = Species), y = ID) +
        geom_bar(stat = "count",
                 position = position_dodge(width = 0.5), width = 0.5,
                 color = "black") +
        scale_fill_manual(values = c("white", "black"),
                          name = "Species", 
                          labels = c("Coho Salmon", "O. mykiss")) +
        theme_classic() +
        labs(title = "Pescadero Creek (2021 Water Year)",
           subtitle = "157 total PIT tag detections at PC1 Site",
           x = "Month",
           y = "Detections") +
        scale_x_discrete(breaks = c("10/2021", "11/2021", "12/2021", "01/2022","02/2022", 
                                    "03/2022", "04/2022", "05/2022", "06/2022", "07/2022", 
                                    "08/2022", "09/2022", "10/2022", "11/2022"),
                         labels = c("10/21", "11/21", "12/21", "01/22","02/22", 
                                    "03/22", "04/22", "05/22", "06/22", "07/22",
                                    "08/22",  "09/22", "10/22", "11/22"),
                         drop = F) +
        theme(legend.position = c(0.95, 0.95)) +
          # Note: order of the above line matters for legend position
        scale_y_continuous(expand = expansion(mult = 0, add = 0), limits = y1)
      
    # 2022 field season
      # y-axis limit
        y2 = c(0, 25)      
      # set standardized plot limit
      
      p2 <- ggplot(P.22, aes(x = DetMonth, fill = Species), y = ID) +
        geom_bar(stat = "count", 
                 color = "black", 
                 position = position_dodge(width = 0.5), width = 0.5,
                 show.legend = F) +
        scale_fill_manual(values = c("black","white")) +
        theme_classic() +
        labs(title = "Pescadero Creek (2022 Water Year)",
             subtitle = "24 total PIT tag detections at PC1 Site",
             x = "Sampling month",
             y = "Detections") +
        scale_x_discrete(breaks = c("10/2022", "11/2022", "12/2022", "01/2023", 
                                    "02/2023", "03/2023", "04/2023", "05/2023", 
                                    "06/2023", "07/2023", "08/2023", "09/2023"), 
                         labels = c("10/22", "11/22", "12/22", "01/23", "02/23", 
                                    "03/23", "04/23", "05/23", "06/23", "07/23", 
                                    "08/23", "09/23"),
                         drop = F) +
        scale_y_continuous(expand = expansion(mult = 0, add = 0), limits = y2)
      
  # combine Pescadero Creek detection plots
    # plot in one window
      p3 <- ggarrange(p1,p2)
      # common title & axes title names
      annotate_figure(p3, top = textGrob("", gp = gpar(cex = 2)),
                      left = textGrob("", rot = 90, gp = gpar(cex = 1)),
                      bottom = textGrob("Sampling month", gp = gpar(cex = 1)))
  
  # Step 2: Plot Butano Creek detections
      
    # a: separate data by field season
    # --------------------------------
      # 2021 - 2022 field season
        B.21 <- B.dat6 %>% filter(DetMonth %in%  
                                         c("12/2021", "05/2022", "08/2022"))
        
        # replace "NA" & blank values for species with "onmy"
          # "NA" values
            B.21$Species <- B.21$Species %>% replace(is.na(.), "Onmy")
          # blank values
            B.21$Species[B.21$Species == ""] <- "Onmy"
            
          # last 2 lines assume all fish other than noted coho are steelhead
      
      # set month as a factorial variable
        B.21$DetMonth <- factor(B.21$DetMonth, 
                              levels = c("10/2021", "11/2021", "12/2021", "01/2022", 
                                         "02/2022", "03/2022", "04/2022", "05/2022", 
                                         "06/2022", "07/2022", "08/2022","09/2022"))
      # 2022 - 2023 field season
        B.22 <- B.dat6 %>% filter(DetMonth %in% 
                                          c("10/2022", "11/2022", "12/2022", 
                                            "01/2023", "02/2023", "03/2023", 
                                            "04/2023", "05/2023", "06/2023", 
                                            "07/2023"))
        # set month as a factorial variable
          B.22$DetMonth <- factor(B.22$DetMonth, 
                              levels = c("10/2022", "11/2022", "12/2022", 
                                         "01/2023", "02/2023", "03/2023", 
                                         "04/2023", "05/2023", "06/2023", 
                                         "07/2023", "08/2023", "09/2023"))
          
        # replace "NA" values for species with "onmy"
          B.22$Species <- B.22$Species %>% replace(is.na(.), "Onmy")
            # assumes all fish other than noted coho are steelhead
      
  # plot Butano Creek results by field season
    # ---------------------------------------
        
    # y-axis limit
      y4 = c(0, 10)      
        
    # 2021 field season
      p4 <- ggplot(B.21, aes(x = DetMonth, fill = Species), y = ID) +
        geom_bar(stat  = "count", 
                 position = position_dodge(width = 0.5), width = 0.5, 
                 color = "black") +
        scale_fill_manual(values = c("white", "black"),
                          name = "Species", 
                          labels = c("Coho Salmon", "O. mykiss")) +
        theme_classic() +
        theme(legend.position = c(0.95,0.95)) +
        labs(title = "Butano Creek (2021 Water Year)",
             subtitle = "9 total PIT tag detections at BC1 Site",
             x = "Month",
             y = "Detections") +
        scale_x_discrete(breaks = c("10/2021", "11/2021", "12/2021", "01/2022", "02/2022", "03/2022", 
                         "04/2022", "05/2022", "06/2022", "07/2022", 
                         "08/2022", "09/2022", "10/2022","11/2022"),
                         labels = c("10/21", "11/21", "12/21", "01/22", "02/22", "03/22", 
                                    "04/22", "05/22", "06/22", "07/22", 
                                    "08/22", "09/22", "10/22","11/22"),
                         drop = F) +
        scale_y_continuous(expand = expansion(mult = 0, add = 0), limits = y4)
      p4
      
      # y-axis limit
        y5 = c(0, 40) 
      
    # 2022 field season
      p5 <- ggplot(B.22, aes(x = DetMonth, fill = Species), y = ID) +
        geom_bar(stat = "count", position = position_dodge(width = 0.5), width = 0.5, 
                 color = "black", na.rm = F) +
        scale_fill_manual(values = c("white", "black"), 
                          name = "Species", 
                          labels = c("Coho Salmon", "O. mykiss")) +
        theme_classic() +
        theme(legend.position = c(0.95,0.95)) +
        labs(title = "Butano Creek (2022 Water Year)",
             subtitle = "172 total PIT tag detections",
             x = "Month",
             y = "Detections") +
        scale_x_discrete(breaks = c("10/2022", "11/2022", "12/2022", "01/2023", 
                                    "02/2023", "03/2023", "04/2023", "05/2023", 
                                    "06/2023", "07/2023", "08/2023", "09/2023"),
                         labels = c("10/22", "11/22", "12/22", "01/23", "02/23", 
                                    "03/23", "04/23",  "05/23", "06/23", "07/23", 
                                    "08/23", "09/23"),
                         drop = F) +
        scale_y_continuous(expand = expansion(mult = 0, add = 0), limits = y5)
      p5
      
  # plot O. mykiss detected in Butano Creek by life stage
    # ---------------------------------------------------
    
    # select O. mykiss detected in 2021 water year
      B.onmy.21 <- B.21 %>% filter(Species == "Onmy")
      
    # select O. mykiss detected in 2022 water year
      B.onmy.22 <- B.22 %>% filter(Species == "Onmy")
      
    # edit variable for life stages
      # replace "unknown" with "Parr" for life stage
        B.onmy.22$Lifestage[B.onmy.22$Lifestage == c("Juvenile")] <- "Parr"
          # fish were tagged in October 2022 & the lagoon was closed
        B.onmy.22$Lifestage <- B.onmy.22$Lifestage %>% replace(is.na(.), "Unknown")
      # rename variable
        B.onmy.22 <- B.onmy.22 %>% rename("SampLifestage" = "Lifestage")
      
    # create variable for life stage at detection
        B.onmy.22$DetLifestage <- c(rep("Parr", 108), rep("Unknown", 2), 
                                    "Smolt", rep("Parr", 4), rep("Unknown", 3))
     
    # y-axis limit
      y6 = c(0,40)
      
    # plot data
      p6 <- ggplot(B.onmy.22, aes(x = DetMonth, fill = SampLifestage), y = unique(ID)) +
        geom_bar(stat  = "count",
                 color = "black",
                 width = 0.5) +
        theme_classic() +
        theme(legend.position = c(0.95,0.95)) +
        mdthemes::md_theme_classic() +
        labs(title = "Butano Creek (2022 Water Year)",
             subtitle = "*O. mykiss* life stages when PIT-tagged & detected",
             x = "Month",
             y = "Detections") +
        scale_x_discrete(breaks = c("10/2022", "11/2022", "12/2022", "01/2023", 
                                    "02/2023", "03/2023", "04/2023", "05/2023", 
                                    "06/2023", "07/2023", "08/2023", "09/2023"),
                         labels = c("10/22", "11/22", "12/22", "01/23", "02/23", 
                                    "03/23", "04/23",  "05/23", "06/23", "07/23", 
                                    "08/23", "09/23"),
                         drop = F) +
        scale_y_continuous(expand = expansion(mult = 0, add = 0), limits = y6) +
        scale_fill_manual(values = c("#4DBBD5FF", "#3C5488FF", "white"),
                          name = "Life stages",
                          labels = c("parr", "smolt", "unknown"))
      p6
      
      
        
# plot life stages by sampling month for lagoon seines
    # select steelhead from Pescadero detections
      dat33 <- filter(dat5, Species == "onmy") 
      
    # edit variables for visualization
      # order life stages for when salmonids were sampled
        dat33$LifeStage <- as.factor(dat33$LifeStage)
        str(dat33$LifeStage) # worked
        
      # rename "LifeStage"  
        dat33 <- dat33 %>% rename("SampLifeStage" = "LifeStage")
        
      # replace NA values for "LifeStage" variable
        dat33$SampLifeStage <- dat33$SampLifeStage %>% replace_na("Parr")
        # CDFW district biologist said "Parr" is likely life stage
      
      # create new variables
        # months when fish were tagged
          dat33$SampMonth <- format(dat33$SampDate, "%m/%Y")
        # life stage when detected
          dat33$DetLifeStage <- c(rep("Adult",3), #900226000295133
                                  rep("Adult",6),
                                  rep("Adult",4),
                                  rep("Adult",6), #900226000899861
                                  rep("Adult",2), #900226000899980
                                  rep("Adult",1), #900228000479335
                                  rep("Adult",2), #982000365411212
                                  rep("Adult",2), #982000365411304
                                  rep("Adult",1), #982000365411306
                                  rep("Adult",2), #982000365411332
                                  rep("Adult",2), #982000365411333
                                  rep("Adult",1), #982000365411414
                                  rep("Adult",1), #982000365411488
                                  rep("Adult",2), #982000365411511
                                  rep("Adult",2), #982000365411671
                                  rep("Adult",3), #982126057446191
                                  rep("Smolt",4), #982126057446348
                                  rep("Smolt",12), #982126057446366
                                  rep("Unknown",2), #982126057446377
                                  rep("Adult",4), #982126057446381
                                  rep("Adult",2), #982126057446441
                                  rep("Smolt",1), #982126057448453
                                  rep("Adult",3), #982126057448453
                                  rep("Smolt",1), #982126057448453
                                  rep("Adult",2), #982126057448453
                                  rep("Adult",2), #982126057448545
                                  rep("Adult",4), #982126057448585
                                  rep("Adult",3), #982126057448604
                                  rep("Adult",1), #982126057448633
                                  rep("Smolt",2), #982126057448680
                                  rep("Adult",2), #982126057448689
                                  rep("Adult",5), #982126057448705
                                  rep("Adult",2), #982126057448736
                                  rep("Parr",2), #982126057448749
                                  rep("Smolt",6)) #982126057448786
        
      # order months when fish were tagged
        dat33$SampMonth <- factor(dat33$SampMonth, 
                                      levels = c("07/2019", "08/2019", "09/2019",
                                                 "10/2019", "10/2020", "04/2022", 
                                                 "05/2022", "06/2022", "08/2022", 
                                                 "10/2022", "11/2022", "12/2022",
                                                 "01/2023", "02/2023", "03/2023"))
        
      # order months of detections
        dat33$DetMonth <- factor(dat33$DetMonth, 
                           levels = c("12/2021", "01/2022", "03/2022", 
                                      "04/2022", "05/2022", "06/2022",
                                      "07/2022", "08/2022", "09/2022",
                                      "10/2022", "11/2022", "12/2022",
                                      "01/2023", "02/2023", "03/2023"))
    
    # show results
      # steelhead life stage by lagoon seine sampling month
        p7 <- ggplot(dat33, aes(x = SampMonth, fill = SampLifeStage), y = ID) + 
           geom_bar(stat = "count", color = "black", show.legend = F, width = 0.3) +
           scale_fill_manual(values = c("#4DBBD5FF", "#8491B4FF")) +
           mdthemes::md_theme_classic() +
           labs(title = "Lower Pescadero Creek Watershed",
             subtitle = "*O. mykiss* when tagged",
             x = "",
             y = "Seine captures") +
           ylim(0,30) +
          theme_classic()
        
      # detections for tagged steelhead
        p8 <- ggplot(dat33, aes(x = DetMonth, fill = DetLifeStage), y = ID) +
          geom_bar(stat = "count", color = "black", width = 0.5) +
          scale_fill_manual(values = c("white", "#4DBBD5FF", "#8491B4FF", "#3C5488FF"),
                            name = "Life stages",
                            labels = c("unknown", "parr", "smolt", "adult")) +
          mdthemes::md_theme_classic() +
          labs(title = "Pescadero Creek - PC1 site",
               subtitle = "*O. mykiss* when captured",
               x = "",
               y = "Detections") +
          ylim(0,30) +
          theme(legend.position = c(0.5,1.0)) +
          theme_classic()
      # combine plots
        # plot in one window
          p9 <- ggarrange(p7,p8, ncol = 1, nrow = 2)
        # common title & axes title names
          annotate_figure(p9, 
                        top = textGrob("Observations by life stage",
                                       gp = gpar(cex = 2)),
                        bottom = textGrob("Sampling month", gp = gpar(cex = 1)))

# ------------------------------------------------------------------------------  
# Old Code (used for "Sink or Source" presentation - https://docs.google.com/presentation/d/1ILeXbiuEXRkBjoFQtj0bFxohjHzt_BUB/edit?usp=sharing&ouid=106434337117436946989&rtpof=true&sd=true)
  
# create column with abbreviated dates & call it "Day"
  dat$Day <- dat$Date %>% format('%m/%d')
  
# separate detections by sampling week
  w1 <- filter(dat, Week == "1")
  w2 <- filter(dat, Week == "2")
  w3 <- filter(dat, Week == "3")
  w4 <- filter(dat, Week == "4")
  w5 <- filter(dat, Week == "5")
  w6 <- filter(dat, Week == "6")
  w7 <- filter(dat, Week == "7")
  w8 <- filter(dat, Week == "8")
  w9 <- filter(dat, Week == "9")
  w10 <- filter(dat, Week == "10")
  w11 <- filter(dat, Week == "11")
  w12 <- filter(dat, Week == "12")
  w13 <- filter(dat, Week == "13")
  w14 <- filter(dat, Week == "14")
  w15 <- filter(dat, Week == "15")
  w16 <- filter(dat, Week == "16")
  w17 <- filter(dat, Week == "17")
  w18 <- filter(dat, Week == "18")
  w19 <- filter(dat, Week == "19")
  w20 <- filter(dat, Week == "20")
  w21 <- filter(dat, Week == "21")
  w22 <- filter(dat, Week == "22")
  w23 <- filter(dat, Week == "23")
  w24 <- filter(dat, Week == "24")
  w25 <- filter(dat, Week == "25")
  w26 <- filter(dat, Week == "26")
  w27 <- filter(dat, Week == "27")
  w28 <- filter(dat, Week == "28")
  w29 <- filter(dat, Week == "29")
  w30 <- filter(dat, Week == "30")
  w31 <- filter(dat, Week == "31")
  w32 <- filter(dat, Week == "32")
  w33 <- filter(dat, Week == "33")
  w34 <- filter(dat, Week == "34")
  w35 <- filter(dat, Week == "35")
  
# convert "Lifestage" column from character to binary variable
  dat$Lifestage <- 
    as.factor(dat$Lifestage) # convert to factorial variable 1st
  dat <-
    dat %>% mutate(Lifestage = recode(Lifestage, "Adult" = "1", "Juvenile" = "0"))
  
# plot weekly detections by life stage for coho & steelhead
  # coho
    ggplot(coho, aes(x = Month, fill = Lifestage)) +
      geom_bar(color = "black") +
      theme_classic() +
      labs(title = "Coho salmon",
           subtitle = "Pescadero Creek - San Mateo County, CA (2021-2022)",
           x = "Sampling month",
           y = "PIT detections") +
      scale_fill_manual(values = c("white", "black"),
                        name = "Life stage when released", labels = c("juveniles", "adults")) +
      scale_x_discrete(limits = c("02/2021", "04/2021", "05/2021", "06/2021", 
                                  "12/2021", "01/2022", "03/2022", "04/2022", 
                                  "05/2022"))
  # steelhead
    ggplot(sthd, aes(x = Month, fill = Lifestage)) +
      geom_bar(color = "black") +
      theme_classic() +
      labs(title = "Steelhead trout",
           subtitle = "Pescadero Creek - San Mateo County, CA (2021-2022)",
           x = "Sampling month",
           y = "PIT detections") +
      scale_fill_manual(values = c("black"),
                        name = "Life stage", labels = c("adults (TL > 150 mm)")) +
      scale_x_discrete(limits = c("01/2021", "02/2021", "03/2021", "04/2021", 
                                  "06/2021", "12/2021", "01/2022", "03/2022", 
                                  "04/2022", "05/2022", "06/2022", "07/2022"))

# plot detections by sampling week
  p1 <- ggplot(w1, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = F) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic() +
    scale_fill_manual(values = c("White", "Black"))
  p2 <- ggplot(w2, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = F) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    scale_fill_manual(values = c("White", "Black"))
  p3 <- ggplot(w3, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))  +
    scale_fill_manual(values = c("White", "Black"))
  p4 <- ggplot(w4, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p5 <- ggplot(w5, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    scale_y_continuous(limits = c(0, 20))  +
    scale_fill_manual(values = c("White", "Black"))
  p6 <- ggplot(w6, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 5))  +
    scale_fill_manual(values = c("White", "Black"))
  p7 <- ggplot(w7, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))  +
    scale_fill_manual(values = c("White", "Black"))
  p8 <- ggplot(w8, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 5))  +
    scale_fill_manual(values = c("White", "Black"))
  p9 <- ggplot(w9, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 5))  +
    scale_fill_manual(values = c("White", "Black"))
  p10 <- ggplot(w10, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6))  +
    scale_fill_manual(values = c("White", "Black"))
  p11 <- ggplot(w11, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6))  +
    scale_fill_manual(values = c("White", "Black"))
  p12 <- ggplot(w12, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6))  +
    scale_fill_manual(values = c("White", "Black"))
  p13 <- ggplot(w13, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p14 <- ggplot(w14, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p15 <- ggplot(w15, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 5),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))  +
    scale_fill_manual(values = c("White", "Black"))
  p16 <- ggplot(w16, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))  +
    scale_fill_manual(values = c("White", "Black"))
  p17 <- ggplot(w17, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 5))  +
    scale_fill_manual(values = c("White", "Black"))
  p18 <- ggplot(w18, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic()  +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    scale_fill_manual(values = c("White", "Black"))
  p19 <- ggplot(w19, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6))  +
    scale_fill_manual(values = c("White", "Black"))
  p20 <- ggplot(w20, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p21 <- ggplot(w21, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 20)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))  +
    scale_fill_manual(values = c("White", "Black"))
  p22 <- ggplot(w22, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 5))  +
    scale_fill_manual(values = c("White", "Black"))
  p23 <- ggplot(w23, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = F) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 5)) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 6))  +
    scale_fill_manual(values = c("White", "Black"))
  p24 <- ggplot(w24, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p25 <- ggplot(w25, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p26 <- ggplot(w26, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p27 <- ggplot(w27, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p28 <- ggplot(w28, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p29 <- ggplot(w29, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p30 <- ggplot(w30, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p31 <- ggplot(w31, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p32 <- ggplot(w32, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p33 <- ggplot(w33, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p34 <- ggplot(w34, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p35 <- ggplot(w35, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  p36 <- ggplot(w36, aes(x = Day, fill = Species)) + 
    geom_histogram(color = "black", stat = "count", 
                   show.legend = T) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 7)) +
    theme_classic()  +
    scale_fill_manual(values = c("White", "Black"))
  
# combine weekly plots
  # 2021 PIT tag detections
    h21 <- 
      ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
                p11, p12, p13, p14, p15, p16, p17, p18, p19, 
                p20, p21, p22, p23)
    annotate_figure(h21, 
                    top = textGrob("Salmonid PIT Tag Detections in Pescadero Creek (San Mateo County, CA; 2021)", 
                                   gp = gpar(cex = 1.3)),
                    left = textGrob("Counts", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                    bottom = textGrob("Dates", gp = gpar(cex = 1)))
    
  # 2022 PIT tag detections  
    h22 <-
      ggarrange(p24, p25, p26, p27, p28, p29, p30,
                p31, p32, p33, p34, p35)
    annotate_figure(f22, 
                    top = textGrob("Salmonid PIT Tag Detections in Pescadero Creek (San Mateo County, CA; 2022)", 
                                   gp = gpar(cex = 1.3)),
                    left = textGrob("Counts", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                    bottom = textGrob("Dates", gp = gpar(cex = 1)))

# Separate detections by sampling month
  m1 <- filter(dat, Month == "01/2021")
  m2 <- filter(dat, Month == "02/2021")
  m3 <- filter(dat, Month == "03/2021")
  m4 <- filter(dat, Month == "04/2021")
  m5 <- filter(dat, Month == "05/2021")
  m6 <- filter(dat, Month == "06/2021")
  m7 <- filter(dat, Month == "12/2021")
  m8 <- filter(dat, Month == "01/2022")
  m9 <- filter(dat, Month == "03/2022")
  m10 <- filter(dat, Month == "04/2022")
  m11 <- filter(dat, Month == "05/2022")
  m12 <- filter(dat, Month == "06/2022")
  m13 <- filter(dat, Month == "07/2022")
    
# Plot detections by sampling month
  f1 <- ggplot(m1, aes(x = Month, fill = Species)) + 
      geom_bar(color = "black", 
                     stat = "count",
                     position = "dodge",
                     show.legend = F) +
      scale_y_continuous(limits = c(0, 150)) +
      labs(x = "", y = "") +
      theme_classic() +
      theme(panel.border = element_rect(colour = "black", 
                                        fill = NA, size = 1)) +
      scale_fill_manual(values = c("White", "Black"))
  f2 <- ggplot(m2, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
                   stat = "count",
                   position = "dodge", 
                   show.legend = F) +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_fill_manual(values = c("White", "Black"))
  f3 <- ggplot(m3, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black",
                   stat = "count",
                   position = "dodge", 
                   show.legend = F) +
    scale_y_continuous(limits = c(0, 150)) +
    labs(x = "", y = "") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", 
                                      fill = NA, size = 1)) +
    scale_fill_manual(values = c("White", "Black"))
  f4 <- ggplot(m4, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
                   stat = "count",
                   position = "dodge", 
                   show.legend = F) +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_fill_manual(values = c("White", "Black"))
  f5 <- ggplot(m5, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
             stat = "count",
             position = "dodge",
             show.legend = F) +
    scale_y_continuous(limits = c(0, 150)) +
    labs(x = "", y = "") +
    theme_classic() +
    theme(panel.border = element_rect(colour = "black", 
                                      fill = NA, size = 1)) +
    scale_fill_manual(values = c("White", "Black"))
  f6 <- ggplot(m6, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
                   stat = "count",
                   position = "dodge", 
                   show.legend = F) +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_fill_manual(values = c("White", "Black"))
  f7 <- ggplot(m7, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
                   stat = "count",
                   position = "dodge",  
                   show.legend = F) +
    scale_y_continuous(limits = c(0, 30)) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_fill_manual(values = c("White", "Black"))
  f8 <- ggplot(m8, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
             stat = "count",
             position = "dodge",
             show.legend = F) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 20)) +
    scale_fill_manual(values = c("White", "Black"))
  f9 <- ggplot(m9, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
             stat = "count",
             position = "dodge",
             show.legend = F) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 20)) +
    scale_fill_manual(values = c("White", "Black"))
  f10 <- ggplot(m10, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
             stat = "count", 
             position = "dodge",
             show.legend = F) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 20)) +
    scale_fill_manual(values = c("White", "Black"))
  f11 <- ggplot(m11, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
             stat = "count", 
             position = "dodge",
             show.legend = F) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 20)) +
    scale_fill_manual(values = c("White", "Black"))
  f12 <- ggplot(m12, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", stat = "count", 
                   show.legend = F) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 20)) +
    scale_fill_manual(values = c("White", "Black"))
  f13 <- ggplot(m13, aes(x = Month, fill = Species)) + 
    geom_bar(color = "black", 
             stat = "count", 
             position = "dodge",
             show.legend = F) +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_continuous(limits = c(0, 20)) +
    scale_fill_manual(values = c("White", "Black"))
  
  # combine monthly plots for 2021
    h21 <- 
      ggarrange(f1, f2, f3, f4, f5, f6, f7)
    
    annotate_figure(h21, 
                  top = textGrob("Monthly Salmonid PIT Tag Detections in Pescadero Creek (San Mateo County, CA; 2021)", 
                                 gp = gpar(cex = 1.3)),
                  left = textGrob("Counts", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                  bottom = textGrob("Month", gp = gpar(cex = 1)))
  
  # combine monthly plots for 2022 
    h22 <- 
      ggarrange(f8, f9, f10, f11, f12, f13)
    
    annotate_figure(h22, 
                    top = textGrob("Monthly Salmonid PIT Tag Detections in Pescadero Creek (San Mateo County, CA; 2022)", 
                                   gp = gpar(cex = 1.3)),
                    left = textGrob("Counts", rot = 90, vjust = 1, gp = gpar(cex = 1)),
                    bottom = textGrob("Month", gp = gpar(cex = 1)))
    
# 2021 detections 
  # filter data for 2021
    dat21 <- filter(dat, Date < "2022-01-01")
  # plot detection counts by week
    ggplot(dat21, aes(x = Week, fill = Species)) + 
      geom_bar(color = "black",
             stat = "count",
             show.legend = F) +
      geom_line(aes(Week, Flow), size = 0.5, col = "Blue") +
      ggtitle("PIT Tag Detections on Pescadero Creek",
            subtitle = "San Mateo County, CA (2021)") +
      labs(y = "Counts", x = "Date") +
      theme_classic()  +
      scale_fill_manual(values = c("White", "Black")) +
      scale_y_continuous(breaks = seq(0, 60, 10),
                         labels = seq(0, 60, 10)) +
      scale_x_continuous(breaks = seq(1, 23, 1),
                     labels = c("1/14", # week 1
                                "1/22", # week 2
                                "1/28", # week 3
                                "2/5",  # week 4
                                "2/12", # week 5
                                "2/20", # week 6
                                "3/3",  # week 7
                                "3/10", # week 8
                                "3/18", # week 9
                                "3/26", # week 10
                                "4/2",  # week 11
                                "4/9",  # week 12
                                "4/17", # week 13
                                "4/25", # week 14
                                "5/1",  # week 15
                                "5/8",  # week 16
                                "5/15", # week 17
                                "5/24", # week 18
                                "6/4",  # week 19
                                "6/13", # week 20
                                "12/7", # week 21
                                "12/17", # week 22
                                "12/25")) # week 23
    
# 2022 detections 
  # filter data for 2022
    dat22 <- filter(dat, Date > "2021-12-31")
    # plot detection counts by week
    ggplot(dat22, aes(x = Week, fill = Species)) + 
      geom_bar(color = "black",
               stat = "count",
               show.legend = F) +
      geom_line(aes(Week, Flow), size = 0.8, col = "Blue") +
      ggtitle("PIT Tag Detections on Pescadero Creek",
              subtitle = "San Mateo County, CA (2022)") +
      labs(y = "Counts", x = "Date") +
      theme_classic()  +
      scale_fill_manual(values = c("White", "Black")) +
      scale_y_continuous(breaks = seq(0, 15, 5),
                         labels = seq(0, 15, 5),
                         sec.axis = sec_axis()) +
      scale_x_discrete(breaks = seq(1, 12, 1),
                         labels = c("1/01",  # week 24
                                    "1/13",  # week 25
                                    "3/04",  # week 26
                                    "3/15",  # week 27
                                    "3/24",  # week 28
                                    "4/03",  # week 29
                                    "4/15",  # week 30
                                    "5/07",  # week 31
                                    "5/25",  # week 32
                                    "6/10",  # week 33
                                    "06/18", # week 34
                                    "7/05")) # week 35   
    
# Old code to compile NOAA PISCES & NOAA electrofishing records with other data
  # ----------------------------------------------------------------------------
    # a. compile all prior dataframes
    comp <- purrr::reduce(
      list(dat18, # Pescadero Creek records; duplicates & physical test tags filtered
           dat31, # Butano Creek records; duplicates & physical test tags filtered
           noaa.efish, 
           noaa.release, 
           noaa.PISCES),
      function(left, right) {
        dplyr::full_join(left, right, by = "ID")
      }
    )
    
    # Note: 159 common rows between "noaa.PISCES" & "noaa.release"
    # no common rows between these datafranes & "noaa.efish"
    # (determined by separate compilations)
    
    # b. compile variables with duplicate names into shared columns
    # 1. Antenna
    cv1 <-
      comp %>%
      mutate(Antenna = coalesce(Antenna.x, Antenna.y)) %>%
      select(ID, Antenna)
    
    # 2. Time
    cv2 <-
      comp %>%
      mutate(Time = coalesce(Time.x, Time.y)) %>%
      select(ID, Time)
    
    # DetDate
    cv3 <-
      comp %>%
      mutate(DetDate = coalesce(DetDate.x, DetDate.y)) %>%
      select(ID, DetDate)
    
    # DetMonth
    cv4 <-
      comp %>%
      mutate(DetMonth = coalesce(DetMonth.x, DetMonth.y)) %>%
      select(ID, DetMonth)
    
    # Species
    cv5 <-
      comp %>%
      mutate(Species = coalesce(Species.x, Species.y)) %>%
      select(ID, Species)
    
    # LifeStage
    cv6 <-
      comp %>%
      mutate(LifeStage = coalesce(LifeStage.x, LifeStage.y)) %>%
      select(ID, LifeStage)
    
    # c. remove columns with duplicate names from "comp"
    comp2 <- comp[-c(2:6,13:18)] 
    
    comp3 <- dplyr::bind_cols(comp2, cv1, cv2, cv3, cv4, cv5, cv6, by = "ID")
    