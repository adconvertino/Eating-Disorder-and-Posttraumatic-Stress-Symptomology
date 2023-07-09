# import baseline data ----

# load required packages
library(dplyr)
library(readxl)
library(haven)
library(lubridate)
library(naniar)
library(psych)

# import ID spreadsheet
IDMatch <- read_excel("~/Library/CloudStorage/GoogleDrive-lconvertino7960@sdsu.edu/Shared drives/BISH Lab - EMA Eating Pathology Study/Part 2/Participant ID and Name Tracker.xlsx",
                      col_types = c("guess", "skip", "guess", "skip", "skip", "skip", "guess", "numeric", "guess", "skip", "guess", "skip", "skip", "skip", "skip", "skip", "skip"))
head(IDMatch)

# delete participants whose Part 1 ID is n/a
IDMatch <- IDMatch[IDMatch$`Part 1 Pt ID` != "n/a", ]
IDMatch <- IDMatch[!is.na(IDMatch[, 1]), ]

# delete participants whose SEMA3 ID is missing
IDMatch <- IDMatch[!is.na(IDMatch[, 4]), ]

# delete participants whose orientation date is missing
IDMatch <- IDMatch[!is.na(IDMatch[, 5]), ]

# create new PARTICIPANT_ID variable from SEMA3 ID
IDMatch$PARTICIPANT_ID <- sprintf("%09d",IDMatch$"SEMA3 ID")

# delete helper variables
IDMatch <- IDMatch[ -c(3:4) ]

# format PARTICIPANT_ID variable with "s" in front
IDMatch$PARTICIPANT_ID <- paste0("s", IDMatch$PARTICIPANT_ID)

# rename variables
IDMatch <- IDMatch %>%
  rename(ID = `Part 1 Pt ID`) %>%
  rename(ResponseId = `Eligibility ID`) %>%
  rename(Day14 = `Day 14`)

# format and calculate dates
IDMatch$Day14 <- as.Date(IDMatch$Day14)
IDMatch$Orientation <- as.Date(IDMatch$Orientation)
IDMatch$Day1 <- ymd(IDMatch$Orientation) + days(1)

# convert n/a text in ResponseId to NAs
IDMatch <- IDMatch %>% 
  replace_with_na(replace = list(ResponseId = "n/a"))

# delete orientation date variable
IDMatch <- IDMatch[ -c(3) ]

# import demographic data
demo <- read_sav("/Users/Lexie/Library/CloudStorage/GoogleDrive-lconvertino7960@sdsu.edu/Shared drives/BISH Lab - EMA Eating Pathology Study/Part 1/Data Analyses/Part+1+Survey_April+28,+2023_10.52.sav",
                 col_select = c("ID", "SONA", "RecordedDate", "DOB", 
                                "Race_1", "Race_2", "Race_3", "Race_4", "Race_5", "Race_5_TEXT", "Ethnicity",
                                "Height_1", "Height_2", "Weight", "Sex", "Gender", "Gender_7_TEXT", "Transgender",
                                "Sexual_Orientation", "Sexual_Orientation_8_TEXT", 
                                "Grade", "CritA_1", 
                                "PCL_Total", "PCL_1", "PCL_2", "PCL_3", "PCL_4", "PCL_5", "PCL_6",
                                "PCL_7", "PCL_8", "PCL_9", "PCL_10", "PCL_11", "PCL_12",
                                "PCL_13", "PCL_14", "PCL_15", "PCL_16", "PCL_17", "PCL_18",
                                "PCL_19", "PCL_20",
                                "EDEQ_Mean", "EDEQ_1", "EDEQ_2", "EDEQ_3", "EDEQ_4", "EDEQ_5", "EDEQ_6",
                                          "EDEQ_7", "EDEQ_8", "EDEQ_9", "EDEQ_10", "EDEQ_11", "EDEQ_12", 
                                          "EDEQ_14", "EDEQ_18",
                                          "EDEQ_19", "EDEQ_20", "EDEQ_21", "EDEQ_22", "EDEQ_23", "EDEQ_24",
                                          "EDEQ_25", "EDEQ_26", "EDEQ_27", "EDEQ_28"))
head(demo)

# score EDEQ cut score
EDEQitems <- c("EDEQ_1", "EDEQ_2", "EDEQ_3", "EDEQ_4", "EDEQ_5", "EDEQ_6",
               "EDEQ_7", "EDEQ_8", "EDEQ_9", "EDEQ_10", "EDEQ_11", "EDEQ_12",
               "EDEQ_19", "EDEQ_20", "EDEQ_21", "EDEQ_22", "EDEQ_23", "EDEQ_24",
               "EDEQ_25", "EDEQ_26", "EDEQ_27", "EDEQ_28")
EDEQkey <- c(rep(1,22))
EDEQscoring <- scoreItems(keys = EDEQkey, items = demo[EDEQitems],  impute = "mean",
                         totals = TRUE, missing = TRUE, digits=0)
demo$EDEQ_Total <- EDEQscoring$scores # Extract the actual total score
demo$AnyOBEs <- ifelse(demo$EDEQ_14 >= 4, 1, 0) # Recode OBEs as 1 or 0
demo$EDEQ_Cut <- 0  # initialize with zeros
demo$EDEQ_Cut <- ifelse((demo$EDEQ_Total>=56 & (demo$AnyOBEs == 1 | demo$EDEQ_18 >= 4)), 1, 0) # score cut
demo <- demo %>% 
  select(-EDEQ_Total, -AnyOBEs, -EDEQ_1, -EDEQ_2, -EDEQ_3, -EDEQ_4, -EDEQ_5, -EDEQ_6, -EDEQ_7, -EDEQ_8, -EDEQ_9, 
         -EDEQ_10, -EDEQ_11, -EDEQ_12, -EDEQ_14, -EDEQ_18, -EDEQ_19, -EDEQ_20, -EDEQ_21, -EDEQ_22, -EDEQ_23, -EDEQ_24,
         -EDEQ_25, -EDEQ_26, -EDEQ_27, -EDEQ_28) # delete helper variables

# score PCL provisional diagnosis
demo$PCL_B <- ifelse((demo$PCL_1 >= 2 | demo$PCL_2 >= 2 | demo$PCL_3 >= 2 | demo$PCL_4 >= 2 | demo$PCL_5 >= 2), 1, 0) # Code B if at least 1 symptom is endorsed "moderately" or greater
demo$PCL_C <- ifelse((demo$PCL_6 >= 2 | demo$PCL_7 >= 2), 1, 0) # Code C if at least 1 symptom is endorsed "moderately" or greater
demo$PCL_8_bin <- ifelse(demo$PCL_8 >=2, 1, 0)
demo$PCL_9_bin <- ifelse(demo$PCL_9 >=2, 1, 0)
demo$PCL_10_bin <- ifelse(demo$PCL_10 >=2, 1, 0)
demo$PCL_11_bin <- ifelse(demo$PCL_11 >=2, 1, 0)
demo$PCL_12_bin <- ifelse(demo$PCL_12 >=2, 1, 0)
demo$PCL_13_bin <- ifelse(demo$PCL_13 >=2, 1, 0)
demo$PCL_14_bin <- ifelse(demo$PCL_14 >=2, 1, 0)
demo$PCL_15_bin <- ifelse(demo$PCL_15 >=2, 1, 0)
demo$PCL_16_bin <- ifelse(demo$PCL_16 >=2, 1, 0)
demo$PCL_17_bin <- ifelse(demo$PCL_17 >=2, 1, 0)
demo$PCL_18_bin <- ifelse(demo$PCL_18 >=2, 1, 0)
demo$PCL_19_bin <- ifelse(demo$PCL_19 >=2, 1, 0)
demo$PCL_20_bin <- ifelse(demo$PCL_20 >=2, 1, 0) # creates binary outcome for these variables if "moderately" or higher is endorsed
demo$PCL_D_sum <- demo$PCL_8_bin + demo$PCL_9_bin + demo$PCL_10_bin + demo$PCL_11_bin + demo$PCL_12_bin + demo$PCL_13_bin + demo$PCL_14_bin 
demo$PCL_D <- ifelse(demo$PCL_D_sum >= 2, 1, 0) # Code D if at least 2 symptoms are endorsed "moderately" or higher
demo$PCL_E_sum <- demo$PCL_15_bin + demo$PCL_16_bin + demo$PCL_17_bin + demo$PCL_18_bin + demo$PCL_19_bin + demo$PCL_20_bin
demo$PCL_E <- ifelse(demo$PCL_E_sum >= 2, 1, 0) # Code E if at least 2 symptoms are endorsed "moderately" or higher
demo$PCL_Prov <- ifelse((demo$PCL_B == 1 & demo$PCL_C == 1 & demo$PCL_D == 1 & demo$PCL_E == 1), 1, 0) # codes provisional PTSD based on subscales
demo <- demo %>% 
  select(-PCL_B, -PCL_C, -PCL_8_bin, -PCL_9_bin, -PCL_10_bin, -PCL_11_bin, -PCL_12_bin, -PCL_13_bin, -PCL_14_bin, 
         -PCL_15_bin, -PCL_16_bin, -PCL_17_bin, -PCL_18_bin, -PCL_19_bin, -PCL_20_bin, -PCL_D_sum, -PCL_D, -PCL_E_sum, 
         -PCL_E, -PCL_1, -PCL_2, -PCL_3, -PCL_4, -PCL_5, -PCL_6, -PCL_7, -PCL_8, -PCL_9, -PCL_10, -PCL_11, -PCL_12,
         -PCL_13, -PCL_14, -PCL_15, -PCL_16, -PCL_17, -PCL_18, -PCL_19, -PCL_20) # delete helper variables


# removes SONA ID 83519 which has the same ID number (39202) as SONA ID 82486 / s223037011
demo <- demo[demo$SONA != 83519, ]

# rename PCL and EDE-Q variables from Part 1
demo <- demo %>%
  rename(PCL_Total_P1 = PCL_Total) %>%
  rename(EDEQ_Mean_P1 = EDEQ_Mean)

head(demo)

# import eligibility survey data
eligibilitydata <- read_sav("/Users/Lexie/Library/CloudStorage/GoogleDrive-lconvertino7960@sdsu.edu/Shared drives/BISH Lab - EMA Eating Pathology Study/Part 2/Eligibility Survey/Part+2+Eligibility+Survey_April+12,+2023_09.22.sav",
                 col_select = c("ResponseId", "PCL_Total", "EDEQ_Mean"))
head(eligibilitydata)

# rename PCL and EDE-Q variables from eligibility survey
eligibilitydata <- eligibilitydata %>%
  rename(PCL_Total_e = PCL_Total) %>%
  rename(EDEQ_Mean_e = EDEQ_Mean)

# merge ID with eligibility survey data
IDMatch_EligibilityData <- merge(IDMatch, eligibilitydata, by = "ResponseId", all=TRUE)
head(IDMatch_EligibilityData)
IDMatch_EligibilityData <- IDMatch_EligibilityData[!is.na(IDMatch_EligibilityData[, 3]), ]

# merge ID with demo data
BaselineData <- merge(IDMatch_EligibilityData, demo, by = "ID", all=TRUE)
head(BaselineData)

# # use eligibility survey PCL if available; else use Part 1 PCL
# BaselineData <- BaselineData%>%
#   mutate(PCL_Total= ifelse(is.na(PCL_Total_e),PCL_Total_P1,PCL_Total_e))
# 
# # use eligibility survey EDE-Q if available; else use Part 1 EDE-Q
# BaselineData <- BaselineData%>%
#   mutate(EDEQ_Mean= ifelse(is.na(EDEQ_Mean_e),EDEQ_Mean_P1,EDEQ_Mean_e))
# 
# # delete helper variables
# BaselineData <- BaselineData %>% select(-c(PCL_Total_e, PCL_Total_P1, EDEQ_Mean_e, EDEQ_Mean_P1))



# import longitudinal data ----
Data <- read.csv("/Users/Lexie/Library/CloudStorage/GoogleDrive-lconvertino7960@sdsu.edu/Shared drives/BISH Lab - EMA Eating Pathology Study/Part 2/Data Analysis/Data Downloads/exports_xdTOopxBQ_export_rB-80Dqdj.csv")
head(Data)

# clean longitudinal data ----

# recode -999 and -888 as missing
Data[Data == -999] <- NA
Data[Data == -888] <- NA

head(Data)

# remove test surveys
Data$TestSurvey <- ifelse(Data$SURVEY_NAME == "Test Questions", 1,0)
Data <- Data[Data$TestSurvey != 1,]
Data$TestSurvey <- NULL

# remove test participants
Data <- Data[!Data$PARTICIPANT_ID %in% c("s207826021", "s088082586", "s518596013", "s834862678", "s007976004", "s614416778"), ]

# isolate day of beep
Data$CREATED_TS <- dmy_hm(Data$CREATED_TS) # assuming the date format is dd-mm-yyyy hh:mm
Data$DateOnly <- as.Date(Data$CREATED_TS)

# get survey range
Data <- merge(Data, BaselineData[ , c("PARTICIPANT_ID", "Day1", "Day14")], by = "PARTICIPANT_ID", all.x = TRUE)

# delete IDs without start dates
Data <- Data[!is.na(Data[, 337]), ]

# delete rows that are outside of the Day1 and Day14 date range
Data <- subset(Data, DateOnly >= Day1 & DateOnly <= Day14)

# remove useless variables
Data <- Data %>% 
  select(-PARTICIPANT_TZ, -STUDY_ID, -STUDY_NAME, -STUDY_VERSION, -SURVEY_ID, -SURVEY_NAME, -TRIGGER, -EXPORT_TZ, -RAND_PROB)

# recode duplicate variables into one

Data$P_DREAMS <- NA
Data$P_SLEEP <- NA
Data$P_INMEM <- NA
Data$P_UPSET <- NA
Data$P_AVMEM <- NA
Data$P_AVEX <- NA
Data$P_LOSS <- NA
Data$P_DISTANT <- NA
Data$P_TROUBPOS <- NA
Data$P_IRRIT <- NA
Data$P_RISK <- NA
Data$P_CONCEN <- NA

Data$P_DREAMS[!is.na(Data$PCL_DREAMS_M)] <- Data$PCL_DREAMS_M[!is.na(Data$PCL_DREAMS_M)]
Data$P_DREAMS[!is.na(Data$PCL_DREAMS_M_1)] <- Data$PCL_DREAMS_M_1[!is.na(Data$PCL_DREAMS_M_1)]
Data$P_SLEEP[!is.na(Data$PCL_SLEEP_M)] <- Data$PCL_SLEEP_M[!is.na(Data$PCL_SLEEP_M)]
Data$P_SLEEP[!is.na(Data$PCL_SLEEP_M_1)] <- Data$PCL_SLEEP_M_1[!is.na(Data$PCL_SLEEP_M_1)]
Data$P_INMEM[!is.na(Data$PCL_INMEM_M)] <- Data$PCL_INMEM_M[!is.na(Data$PCL_INMEM_M)]
Data$P_INMEM[!is.na(Data$PCL_INMEM_M_1)] <- Data$PCL_INMEM_M_1[!is.na(Data$PCL_INMEM_M_1)]
Data$P_INMEM[!is.na(Data$PCL_INMEM)] <- Data$PCL_INMEM[!is.na(Data$PCL_INMEM)]
Data$P_UPSET[!is.na(Data$PCL_UPSET_M)] <- Data$PCL_UPSET_M[!is.na(Data$PCL_UPSET_M)]
Data$P_UPSET[!is.na(Data$PCL_UPSET_M_1)] <- Data$PCL_UPSET_M_1[!is.na(Data$PCL_UPSET_M_1)]
Data$P_UPSET[!is.na(Data$PCL_UPSET)] <- Data$PCL_UPSET[!is.na(Data$PCL_UPSET)]
Data$P_AVMEM[!is.na(Data$PCL_AVOIDMEM_M)] <- Data$PCL_AVOIDMEM_M[!is.na(Data$PCL_AVOIDMEM_M)]
Data$P_AVMEM[!is.na(Data$PCL_AVOIDMEM_M_1)] <- Data$PCL_AVOIDMEM_M_1[!is.na(Data$PCL_AVOIDMEM_M_1)]
Data$P_AVMEM[!is.na(Data$PCL_AVOIDMEM)] <- Data$PCL_AVOIDMEM[!is.na(Data$PCL_AVOIDMEM)]
Data$P_AVEX[!is.na(Data$PCL_AVOIDPL_M)] <- Data$PCL_AVOIDPL_M[!is.na(Data$PCL_AVOIDPL_M)]
Data$P_AVEX[!is.na(Data$PCL_AVOIDPL_M_1)] <- Data$PCL_AVOIDPL_M_1[!is.na(Data$PCL_AVOIDPL_M_1)]
Data$P_AVEX[!is.na(Data$PCL_AVOIDPL)] <- Data$PCL_AVOIDPL[!is.na(Data$PCL_AVOIDPL)]
Data$P_LOSS[!is.na(Data$PCL_LOSS_M)] <- Data$PCL_LOSS_M[!is.na(Data$PCL_LOSS_M)]
Data$P_LOSS[!is.na(Data$PCL_LOSS_M_1)] <- Data$PCL_LOSS_M_1[!is.na(Data$PCL_LOSS_M_1)]
Data$P_LOSS[!is.na(Data$PCL_LOSS)] <- Data$PCL_LOSS[!is.na(Data$PCL_LOSS)]
Data$P_DISTANT[!is.na(Data$PCL_DISTANT_M)] <- Data$PCL_DISTANT_M[!is.na(Data$PCL_DISTANT_M)]
Data$P_DISTANT[!is.na(Data$PCL_DISTANT_M_1)] <- Data$PCL_DISTANT_M_1[!is.na(Data$PCL_DISTANT_M_1)]
Data$P_DISTANT[!is.na(Data$PCL_DISTANT)] <- Data$PCL_DISTANT[!is.na(Data$PCL_DISTANT)]
Data$P_TROUBPOS[!is.na(Data$PCL_TROUBPOS_M)] <- Data$PCL_TROUBPOS_M[!is.na(Data$PCL_TROUBPOS_M)]
Data$P_TROUBPOS[!is.na(Data$PCL_TROUBPOS_M_1)] <- Data$PCL_TROUBPOS_M_1[!is.na(Data$PCL_TROUBPOS_M_1)]
Data$P_TROUBPOS[!is.na(Data$PCL_TROUBPOS)] <- Data$PCL_TROUBPOS[!is.na(Data$PCL_TROUBPOS)]
Data$P_IRRIT[!is.na(Data$PCL_IRRITABLE_M)] <- Data$PCL_IRRITABLE_M[!is.na(Data$PCL_IRRITABLE_M)]
Data$P_IRRIT[!is.na(Data$PCL_IRRITABLE_M_1)] <- Data$PCL_IRRITABLE_M_1[!is.na(Data$PCL_IRRITABLE_M_1)]
Data$P_IRRIT[!is.na(Data$PCL_IRRITABLE)] <- Data$PCL_IRRITABLE[!is.na(Data$PCL_IRRITABLE)]
Data$P_RISK[!is.na(Data$PCL_RISK_M)] <- Data$PCL_RISK_M[!is.na(Data$PCL_RISK_M)]
Data$P_RISK[!is.na(Data$PCL_RISK_M_1)] <- Data$PCL_RISK_M_1[!is.na(Data$PCL_RISK_M_1)]
Data$P_RISK[!is.na(Data$PCL_RISK)] <- Data$PCL_RISK[!is.na(Data$PCL_RISK)]
Data$P_CONCEN[!is.na(Data$PCL_CONCEN_M)] <- Data$PCL_CONCEN_M[!is.na(Data$PCL_CONCEN_M)]
Data$P_CONCEN[!is.na(Data$PCL_CONCEN_M_1)] <- Data$PCL_CONCEN_M_1[!is.na(Data$PCL_CONCEN_M_1)]
Data$P_CONCEN[!is.na(Data$PCL_CONCEN)] <- Data$PCL_CONCEN[!is.na(Data$PCL_CONCEN)]

Data$E_BD <- NA
Data$E_FEARWT <- NA
Data$E_CONCENSHWT <- NA
Data$E_CONCENFOOD <- NA
Data$E_DRIVEN <- NA
Data$E_FAST <- NA

Data$E_BD[!is.na(Data$ED_BD_M)] <- Data$ED_BD_M[!is.na(Data$ED_BD_M)]
Data$E_BD[!is.na(Data$ED_BD_M_1)] <- Data$ED_BD_M_1[!is.na(Data$ED_BD_M_1)]
Data$E_BD[!is.na(Data$ED_BD)] <- Data$ED_BD[!is.na(Data$ED_BD)]
Data$E_FEARWT[!is.na(Data$ED_FEARWT_M)] <- Data$ED_FEARWT_M[!is.na(Data$ED_FEARWT_M)]
Data$E_FEARWT[!is.na(Data$ED_FEARWT_M_1)] <- Data$ED_FEARWT_M_1[!is.na(Data$ED_FEARWT_M_1)]
Data$E_FEARWT[!is.na(Data$ED_FEARWT)] <- Data$ED_FEARWT[!is.na(Data$ED_FEARWT)]
Data$E_CONCENSHWT[!is.na(Data$ED_CONCENSHWT_M)] <- Data$ED_CONCENSHWT_M[!is.na(Data$ED_CONCENSHWT_M)]
Data$E_CONCENSHWT[!is.na(Data$ED_CONCENSHWT_M_1)] <- Data$ED_CONCENSHWT_M_1[!is.na(Data$ED_CONCENSHWT_M_1)]
Data$E_CONCENSHWT[!is.na(Data$ED_CONCENSHWT)] <- Data$ED_CONCENSHWT[!is.na(Data$ED_CONCENSHWT)]
Data$E_CONCENFOOD[!is.na(Data$ED_CONCENFOOD_M)] <- Data$ED_CONCENFOOD_M[!is.na(Data$ED_CONCENFOOD_M)]
Data$E_CONCENFOOD[!is.na(Data$ED_CONCENFOOD_M_1)] <- Data$ED_CONCENFOOD_M_1[!is.na(Data$ED_CONCENFOOD_M_1)]
Data$E_CONCENFOOD[!is.na(Data$ED_CONCENFOOD)] <- Data$ED_CONCENFOOD[!is.na(Data$ED_CONCENFOOD)]
Data$E_DRIVEN[!is.na(Data$ED_DRIVEN_M)] <- Data$ED_DRIVEN_M[!is.na(Data$ED_DRIVEN_M)]
Data$E_DRIVEN[!is.na(Data$ED_DRIVEN_M_1)] <- Data$ED_DRIVEN_M_1[!is.na(Data$ED_DRIVEN_M_1)]
Data$E_DRIVEN[!is.na(Data$ED_DRIVEN)] <- Data$ED_DRIVEN[!is.na(Data$ED_DRIVEN)]
Data$E_FAST[!is.na(Data$ED_FAST_M)] <- Data$ED_FAST_M[!is.na(Data$ED_FAST_M)]
Data$E_FAST[!is.na(Data$ED_FAST_M_1)] <- Data$ED_FAST_M_1[!is.na(Data$ED_FAST_M_1)]
Data$E_FAST[!is.na(Data$ED_FAST)] <- Data$ED_FAST[!is.na(Data$ED_FAST)]

Data$E_PATTERN_B <- NA
Data$E_PATTERN_MS <- NA
Data$E_PATTERN_L <- NA
Data$E_PATTERN_AS <- NA
Data$E_PATTERN_D <- NA
Data$E_PATTERN_ES <- NA
Data$E_PATTERN_NO <- NA

Data$E_PATTERN_B[!is.na(Data$ED_PATTERN_M_1)] <- Data$ED_PATTERN_M_1[!is.na(Data$ED_PATTERN_M_1)]
Data$E_PATTERN_MS[!is.na(Data$ED_PATTERN_M_2)] <- Data$ED_PATTERN_M_2[!is.na(Data$ED_PATTERN_M_2)]
Data$E_PATTERN_L[!is.na(Data$ED_PATTERN_M_3)] <- Data$ED_PATTERN_M_3[!is.na(Data$ED_PATTERN_M_3)]
Data$E_PATTERN_AS[!is.na(Data$ED_PATTERN_M_4)] <- Data$ED_PATTERN_M_4[!is.na(Data$ED_PATTERN_M_4)]
Data$E_PATTERN_D[!is.na(Data$ED_PATTERN_M_5)] <- Data$ED_PATTERN_M_5[!is.na(Data$ED_PATTERN_M_5)]
Data$E_PATTERN_ES[!is.na(Data$ED_PATTERN_M_6)] <- Data$ED_PATTERN_M_6[!is.na(Data$ED_PATTERN_M_6)]
Data$E_PATTERN_NO[!is.na(Data$ED_PATTERN_M_7)] <- Data$ED_PATTERN_M_7[!is.na(Data$ED_PATTERN_M_7)]
Data$E_PATTERN_B[!is.na(Data$ED_PATTERN_M_1_1)] <- Data$ED_PATTERN_M_1_1[!is.na(Data$ED_PATTERN_M_1_1)]
Data$E_PATTERN_MS[!is.na(Data$ED_PATTERN_M_1_2)] <- Data$ED_PATTERN_M_1_2[!is.na(Data$ED_PATTERN_M_1_2)]
Data$E_PATTERN_L[!is.na(Data$ED_PATTERN_M_1_3)] <- Data$ED_PATTERN_M_1_3[!is.na(Data$ED_PATTERN_M_1_3)]
Data$E_PATTERN_AS[!is.na(Data$ED_PATTERN_M_1_4)] <- Data$ED_PATTERN_M_1_4[!is.na(Data$ED_PATTERN_M_1_4)]
Data$E_PATTERN_D[!is.na(Data$ED_PATTERN_M_1_5)] <- Data$ED_PATTERN_M_1_5[!is.na(Data$ED_PATTERN_M_1_5)]
Data$E_PATTERN_ES[!is.na(Data$ED_PATTERN_M_1_6)] <- Data$ED_PATTERN_M_1_6[!is.na(Data$ED_PATTERN_M_1_6)]
Data$E_PATTERN_NO[!is.na(Data$ED_PATTERN_M_1_7)] <- Data$ED_PATTERN_M_1_7[!is.na(Data$ED_PATTERN_M_1_7)]
Data$E_PATTERN_B[!is.na(Data$ED_PATTERN_1)] <- Data$ED_PATTERN_1[!is.na(Data$ED_PATTERN_1)]
Data$E_PATTERN_MS[!is.na(Data$ED_PATTERN_2)] <- Data$ED_PATTERN_2[!is.na(Data$ED_PATTERN_2)]
Data$E_PATTERN_L[!is.na(Data$ED_PATTERN_3)] <- Data$ED_PATTERN_3[!is.na(Data$ED_PATTERN_3)]
Data$E_PATTERN_AS[!is.na(Data$ED_PATTERN_4)] <- Data$ED_PATTERN_4[!is.na(Data$ED_PATTERN_4)]
Data$E_PATTERN_D[!is.na(Data$ED_PATTERN_5)] <- Data$ED_PATTERN_5[!is.na(Data$ED_PATTERN_5)]
Data$E_PATTERN_ES[!is.na(Data$ED_PATTERN_6)] <- Data$ED_PATTERN_6[!is.na(Data$ED_PATTERN_6)]
Data$E_PATTERN_NO[!is.na(Data$ED_PATTERN_7)] <- Data$ED_PATTERN_7[!is.na(Data$ED_PATTERN_7)]

Data$E_WTCON_SKIP <- NA
Data$E_WTCON_VOM <- NA
Data$E_WTCON_LAX <- NA
Data$E_WTCON_DIU <- NA
Data$E_WTCON_EXER <- NA
Data$E_WTCON_FLUID <- NA
Data$E_WTCON_NO <- NA

Data$E_WTCON_SKIP[!is.na(Data$ED_WTCON_M_1)] <- Data$ED_WTCON_M_1[!is.na(Data$ED_WTCON_M_1)]
Data$E_WTCON_VOM[!is.na(Data$ED_WTCON_M_2)] <- Data$ED_WTCON_M_2[!is.na(Data$ED_WTCON_M_2)]
Data$E_WTCON_LAX[!is.na(Data$ED_WTCON_M_3)] <- Data$ED_WTCON_M_3[!is.na(Data$ED_WTCON_M_3)]
Data$E_WTCON_DIU[!is.na(Data$ED_WTCON_M_4)] <- Data$ED_WTCON_M_4[!is.na(Data$ED_WTCON_M_4)]
Data$E_WTCON_EXER[!is.na(Data$ED_WTCON_M_5)] <- Data$ED_WTCON_M_5[!is.na(Data$ED_WTCON_M_5)]
Data$E_WTCON_FLUID[!is.na(Data$ED_WTCON_M_6)] <- Data$ED_WTCON_M_6[!is.na(Data$ED_WTCON_M_6)]
Data$E_WTCON_NO[!is.na(Data$ED_WTCON_M_7)] <- Data$ED_WTCON_M_7[!is.na(Data$ED_WTCON_M_7)]
Data$E_WTCON_SKIP[!is.na(Data$ED_WTCON_M_1_1)] <- Data$ED_WTCON_M_1_1[!is.na(Data$ED_WTCON_M_1_1)]
Data$E_WTCON_VOM[!is.na(Data$ED_WTCON_M_1_2)] <- Data$ED_WTCON_M_1_2[!is.na(Data$ED_WTCON_M_1_2)]
Data$E_WTCON_LAX[!is.na(Data$ED_WTCON_M_1_3)] <- Data$ED_WTCON_M_1_3[!is.na(Data$ED_WTCON_M_1_3)]
Data$E_WTCON_DIU[!is.na(Data$ED_WTCON_M_1_4)] <- Data$ED_WTCON_M_1_4[!is.na(Data$ED_WTCON_M_1_4)]
Data$E_WTCON_EXER[!is.na(Data$ED_WTCON_M_1_5)] <- Data$ED_WTCON_M_1_5[!is.na(Data$ED_WTCON_M_1_5)]
Data$E_WTCON_FLUID[!is.na(Data$ED_WTCON_M_1_6)] <- Data$ED_WTCON_M_1_6[!is.na(Data$ED_WTCON_M_1_6)]
Data$E_WTCON_NO[!is.na(Data$ED_WTCON_M_1_7)] <- Data$ED_WTCON_M_1_7[!is.na(Data$ED_WTCON_M_1_7)]
Data$E_WTCON_SKIP[!is.na(Data$ED_WTCON_1)] <- Data$ED_WTCON_1[!is.na(Data$ED_WTCON_1)]
Data$E_WTCON_VOM[!is.na(Data$ED_WTCON_2)] <- Data$ED_WTCON_2[!is.na(Data$ED_WTCON_2)]
Data$E_WTCON_LAX[!is.na(Data$ED_WTCON_3)] <- Data$ED_WTCON_3[!is.na(Data$ED_WTCON_3)]
Data$E_WTCON_DIU[!is.na(Data$ED_WTCON_4)] <- Data$ED_WTCON_4[!is.na(Data$ED_WTCON_4)]
Data$E_WTCON_EXER[!is.na(Data$ED_WTCON_5)] <- Data$ED_WTCON_5[!is.na(Data$ED_WTCON_5)]
Data$E_WTCON_FLUID[!is.na(Data$ED_WTCON_6)] <- Data$ED_WTCON_6[!is.na(Data$ED_WTCON_6)]
Data$E_WTCON_NO[!is.na(Data$ED_WTCON_7)] <- Data$ED_WTCON_7[!is.na(Data$ED_WTCON_7)]

Data$E_MOE_SHAKE <- NA
Data$E_MOE_CALS <- NA
Data$E_MOE_SUPP <- NA
Data$E_MOE_THAPED <- NA
Data$E_MOE_USEAPED <- NA
Data$E_MOE_NO <- NA

Data$E_MOE_SHAKE[!is.na(Data$ED_MOE_M_1)] <- Data$ED_MOE_M_1[!is.na(Data$ED_MOE_M_1)]
Data$E_MOE_CALS[!is.na(Data$ED_MOE_M_2)] <- Data$ED_MOE_M_2[!is.na(Data$ED_MOE_M_2)]
Data$E_MOE_SUPP[!is.na(Data$ED_MOE_M_3)] <- Data$ED_MOE_M_3[!is.na(Data$ED_MOE_M_3)]
Data$E_MOE_THAPED[!is.na(Data$ED_MOE_M_4)] <- Data$ED_MOE_M_4[!is.na(Data$ED_MOE_M_4)]
Data$E_MOE_USEAPED[!is.na(Data$ED_MOE_M_5)] <- Data$ED_MOE_M_5[!is.na(Data$ED_MOE_M_5)]
Data$E_MOE_NO[!is.na(Data$ED_MOE_M_6)] <- Data$ED_MOE_M_6[!is.na(Data$ED_MOE_M_6)]
Data$E_MOE_SHAKE[!is.na(Data$ED_MOE_M_1_1)] <- Data$ED_MOE_M_1_1[!is.na(Data$ED_MOE_M_1_1)]
Data$E_MOE_CALS[!is.na(Data$ED_MOE_M_1_2)] <- Data$ED_MOE_M_1_2[!is.na(Data$ED_MOE_M_1_2)]
Data$E_MOE_SUPP[!is.na(Data$ED_MOE_M_1_3)] <- Data$ED_MOE_M_1_3[!is.na(Data$ED_MOE_M_1_3)]
Data$E_MOE_THAPED[!is.na(Data$ED_MOE_M_1_4)] <- Data$ED_MOE_M_1_4[!is.na(Data$ED_MOE_M_1_4)]
Data$E_MOE_USEAPED[!is.na(Data$ED_MOE_M_1_5)] <- Data$ED_MOE_M_1_5[!is.na(Data$ED_MOE_M_1_5)]
Data$E_MOE_NO[!is.na(Data$ED_MOE_M_1_6)] <- Data$ED_MOE_M_1_6[!is.na(Data$ED_MOE_M_1_6)]
Data$E_MOE_SHAKE[!is.na(Data$ED_MOE_1)] <- Data$ED_MOE_1[!is.na(Data$ED_MOE_1)]
Data$E_MOE_CALS[!is.na(Data$ED_MOE_2)] <- Data$ED_MOE_2[!is.na(Data$ED_MOE_2)]
Data$E_MOE_SUPP[!is.na(Data$ED_MOE_3)] <- Data$ED_MOE_3[!is.na(Data$ED_MOE_3)]
Data$E_MOE_THAPED[!is.na(Data$ED_MOE_4)] <- Data$ED_MOE_4[!is.na(Data$ED_MOE_4)]
Data$E_MOE_USEAPED[!is.na(Data$ED_MOE_5)] <- Data$ED_MOE_5[!is.na(Data$ED_MOE_5)]
Data$E_MOE_NO[!is.na(Data$ED_MOE_6)] <- Data$ED_MOE_6[!is.na(Data$ED_MOE_6)]

Data$E_BODYCH_WEIGH <- NA
Data$E_BODYCH_THIGH <- NA
Data$E_BODYCH_SUCKSTOM <- NA
Data$E_BODYCH_FELTSTOM <- NA
Data$E_BODYCH_COMP <- NA
Data$E_BODYCH_MIRROR <- NA
Data$E_BODYCH_JIGGLE <- NA
Data$E_BODYCH_THIGHSIT <- NA
Data$E_BODYCH_NO <- NA

Data$E_BODYCH_WEIGH[!is.na(Data$ED_BODYCH_M_1)] <- Data$ED_BODYCH_M_1[!is.na(Data$ED_BODYCH_M_1)]
Data$E_BODYCH_THIGH[!is.na(Data$ED_BODYCH_M_2)] <- Data$ED_BODYCH_M_2[!is.na(Data$ED_BODYCH_M_2)]
Data$E_BODYCH_SUCKSTOM[!is.na(Data$ED_BODYCH_M_3)] <- Data$ED_BODYCH_M_3[!is.na(Data$ED_BODYCH_M_3)]
Data$E_BODYCH_FELTSTOM[!is.na(Data$ED_BODYCH_M_4)] <- Data$ED_BODYCH_M_4[!is.na(Data$ED_BODYCH_M_4)]
Data$E_BODYCH_COMP[!is.na(Data$ED_BODYCH_M_5)] <- Data$ED_BODYCH_M_5[!is.na(Data$ED_BODYCH_M_5)]
Data$E_BODYCH_MIRROR[!is.na(Data$ED_BODYCH_M_6)] <- Data$ED_BODYCH_M_6[!is.na(Data$ED_BODYCH_M_6)]
Data$E_BODYCH_JIGGLE[!is.na(Data$ED_BODYCH_M_7)] <- Data$ED_BODYCH_M_7[!is.na(Data$ED_BODYCH_M_7)]
Data$E_BODYCH_THIGHSIT[!is.na(Data$ED_BODYCH_M_8)] <- Data$ED_BODYCH_M_8[!is.na(Data$ED_BODYCH_M_8)]
Data$E_BODYCH_NO[!is.na(Data$ED_BODYCH_M_9)] <- Data$ED_BODYCH_M_9[!is.na(Data$ED_BODYCH_M_9)]
Data$E_BODYCH_WEIGH[!is.na(Data$ED_BODYCH_M_1_1)] <- Data$ED_BODYCH_M_1_1[!is.na(Data$ED_BODYCH_M_1_1)]
Data$E_BODYCH_THIGH[!is.na(Data$ED_BODYCH_M_1_2)] <- Data$ED_BODYCH_M_1_2[!is.na(Data$ED_BODYCH_M_1_2)]
Data$E_BODYCH_SUCKSTOM[!is.na(Data$ED_BODYCH_M_1_3)] <- Data$ED_BODYCH_M_1_3[!is.na(Data$ED_BODYCH_M_1_3)]
Data$E_BODYCH_FELTSTOM[!is.na(Data$ED_BODYCH_M_1_4)] <- Data$ED_BODYCH_M_1_4[!is.na(Data$ED_BODYCH_M_1_4)]
Data$E_BODYCH_COMP[!is.na(Data$ED_BODYCH_M_1_5)] <- Data$ED_BODYCH_M_1_5[!is.na(Data$ED_BODYCH_M_1_5)]
Data$E_BODYCH_MIRROR[!is.na(Data$ED_BODYCH_M_1_6)] <- Data$ED_BODYCH_M_1_6[!is.na(Data$ED_BODYCH_M_1_6)]
Data$E_BODYCH_JIGGLE[!is.na(Data$ED_BODYCH_M_1_7)] <- Data$ED_BODYCH_M_1_7[!is.na(Data$ED_BODYCH_M_1_7)]
Data$E_BODYCH_THIGHSIT[!is.na(Data$ED_BODYCH_M_1_8)] <- Data$ED_BODYCH_M_1_8[!is.na(Data$ED_BODYCH_M_1_8)]
Data$E_BODYCH_NO[!is.na(Data$ED_BODYCH_M_1_9)] <- Data$ED_BODYCH_M_1_9[!is.na(Data$ED_BODYCH_M_1_9)]
Data$E_BODYCH_WEIGH[!is.na(Data$ED_BODYCH_1)] <- Data$ED_BODYCH_1[!is.na(Data$ED_BODYCH_1)]
Data$E_BODYCH_THIGH[!is.na(Data$ED_BODYCH_2)] <- Data$ED_BODYCH_2[!is.na(Data$ED_BODYCH_2)]
Data$E_BODYCH_SUCKSTOM[!is.na(Data$ED_BODYCH_3)] <- Data$ED_BODYCH_3[!is.na(Data$ED_BODYCH_3)]
Data$E_BODYCH_FELTSTOM[!is.na(Data$ED_BODYCH_4)] <- Data$ED_BODYCH_4[!is.na(Data$ED_BODYCH_4)]
Data$E_BODYCH_COMP[!is.na(Data$ED_BODYCH_5)] <- Data$ED_BODYCH_5[!is.na(Data$ED_BODYCH_5)]
Data$E_BODYCH_MIRROR[!is.na(Data$ED_BODYCH_6)] <- Data$ED_BODYCH_6[!is.na(Data$ED_BODYCH_6)]
Data$E_BODYCH_JIGGLE[!is.na(Data$ED_BODYCH_7)] <- Data$ED_BODYCH_7[!is.na(Data$ED_BODYCH_7)]
Data$E_BODYCH_THIGHSIT[!is.na(Data$ED_BODYCH_8)] <- Data$ED_BODYCH_8[!is.na(Data$ED_BODYCH_8)]
Data$E_BODYCH_NO[!is.na(Data$ED_BODYCH_9)] <- Data$ED_BODYCH_9[!is.na(Data$ED_BODYCH_9)]

Data$E_B_LOC <- NA
Data$E_B_OVEREAT <- NA
Data$E_B_REST <- NA
Data$E_MS_LOC <- NA
Data$E_MS_OVEREAT <- NA
Data$E_MS_REST <- NA
Data$E_L_LOC <- NA
Data$E_L_OVEREAT <- NA
Data$E_L_REST <- NA
Data$E_AS_LOC <- NA
Data$E_AS_OVEREAT <- NA
Data$E_AS_REST <- NA
Data$E_D_LOC <- NA
Data$E_D_OVEREAT <- NA
Data$E_D_REST <- NA
Data$E_ES_LOC <- NA
Data$E_ES_OVEREAT <- NA
Data$E_ES_REST <- NA

Data$E_B_LOC[!is.na(Data$ED_B_LOC_M)] <- Data$ED_B_LOC_M[!is.na(Data$ED_B_LOC_M)]
Data$E_B_LOC[!is.na(Data$ED_B_LOC_M_1)] <- Data$ED_B_LOC_M_1[!is.na(Data$ED_B_LOC_M_1)]
Data$E_B_LOC[!is.na(Data$ED_B_LOC)] <- Data$ED_B_LOC[!is.na(Data$ED_B_LOC)]
Data$E_B_OVEREAT[!is.na(Data$ED_B_OVEREAT_M)] <- Data$ED_B_OVEREAT_M[!is.na(Data$ED_B_OVEREAT_M)]
Data$E_B_OVEREAT[!is.na(Data$ED_B_OVEREAT_M_1)] <- Data$ED_B_OVEREAT_M_1[!is.na(Data$ED_B_OVEREAT_M_1)]
Data$E_B_OVEREAT[!is.na(Data$ED_B_OVEREAT)] <- Data$ED_B_OVEREAT[!is.na(Data$ED_B_OVEREAT)]
Data$E_B_REST[!is.na(Data$ED_B_REST_M)] <- Data$ED_B_REST_M[!is.na(Data$ED_B_REST_M)]
Data$E_B_REST[!is.na(Data$ED_B_REST_M_1)] <- Data$ED_B_REST_M_1[!is.na(Data$ED_B_REST_M_1)]
Data$E_B_REST[!is.na(Data$ED_B_REST)] <- Data$ED_B_REST[!is.na(Data$ED_B_REST)]
Data$E_MS_LOC[!is.na(Data$ED_MS_LOC_M)] <- Data$ED_MS_LOC_M[!is.na(Data$ED_MS_LOC_M)]
Data$E_MS_LOC[!is.na(Data$ED_MS_LOC_M_1)] <- Data$ED_MS_LOC_M_1[!is.na(Data$ED_MS_LOC_M_1)]
Data$E_MS_LOC[!is.na(Data$ED_MS_LOC)] <- Data$ED_MS_LOC[!is.na(Data$ED_MS_LOC)]
Data$E_MS_OVEREAT[!is.na(Data$ED_MS_OVEREAT_M)] <- Data$ED_MS_OVEREAT_M[!is.na(Data$ED_MS_OVEREAT_M)]
Data$E_MS_OVEREAT[!is.na(Data$ED_MS_OVEREAT_M_1)] <- Data$ED_MS_OVEREAT_M_1[!is.na(Data$ED_MS_OVEREAT_M_1)]
Data$E_MS_OVEREAT[!is.na(Data$ED_MS_OVEREAT)] <- Data$ED_MS_OVEREAT[!is.na(Data$ED_MS_OVEREAT)]
Data$E_MS_REST[!is.na(Data$ED_MS_REST_M)] <- Data$ED_MS_REST_M[!is.na(Data$ED_MS_REST_M)]
Data$E_MS_REST[!is.na(Data$ED_MS_REST_M_1)] <- Data$ED_MS_REST_M_1[!is.na(Data$ED_MS_REST_M_1)]
Data$E_MS_REST[!is.na(Data$ED_MS_REST)] <- Data$ED_MS_REST[!is.na(Data$ED_MS_REST)]
Data$E_L_LOC[!is.na(Data$ED_L_LOC_M)] <- Data$ED_L_LOC_M[!is.na(Data$ED_L_LOC_M)]
Data$E_L_LOC[!is.na(Data$ED_L_LOC_M_1)] <- Data$ED_L_LOC_M_1[!is.na(Data$ED_L_LOC_M_1)]
Data$E_L_LOC[!is.na(Data$ED_L_LOC)] <- Data$ED_L_LOC[!is.na(Data$ED_L_LOC)]
Data$E_L_OVEREAT[!is.na(Data$ED_L_OVEREAT_M)] <- Data$ED_L_OVEREAT_M[!is.na(Data$ED_L_OVEREAT_M)]
Data$E_L_OVEREAT[!is.na(Data$ED_L_OVEREAT_M_1)] <- Data$ED_L_OVEREAT_M_1[!is.na(Data$ED_L_OVEREAT_M_1)]
Data$E_L_OVEREAT[!is.na(Data$ED_L_OVEREAT)] <- Data$ED_L_OVEREAT[!is.na(Data$ED_L_OVEREAT)]
Data$E_L_REST[!is.na(Data$ED_L_REST_M)] <- Data$ED_L_REST_M[!is.na(Data$ED_L_REST_M)]
Data$E_L_REST[!is.na(Data$ED_L_REST_M_1)] <- Data$ED_L_REST_M_1[!is.na(Data$ED_L_REST_M_1)]
Data$E_L_REST[!is.na(Data$ED_L_REST)] <- Data$ED_L_REST[!is.na(Data$ED_L_REST)]
Data$E_AS_LOC[!is.na(Data$ED_AS_LOC_M)] <- Data$ED_AS_LOC_M[!is.na(Data$ED_AS_LOC_M)]
Data$E_AS_LOC[!is.na(Data$ED_AS_LOC_M_1)] <- Data$ED_AS_LOC_M_1[!is.na(Data$ED_AS_LOC_M_1)]
Data$E_AS_LOC[!is.na(Data$ED_AS_LOC)] <- Data$ED_AS_LOC[!is.na(Data$ED_AS_LOC)]
Data$E_AS_OVEREAT[!is.na(Data$ED_AS_OVEREAT_M)] <- Data$ED_AS_OVEREAT_M[!is.na(Data$ED_AS_OVEREAT_M)]
Data$E_AS_OVEREAT[!is.na(Data$ED_AS_OVEREAT_M_1)] <- Data$ED_AS_OVEREAT_M_1[!is.na(Data$ED_AS_OVEREAT_M_1)]
Data$E_AS_OVEREAT[!is.na(Data$ED_AS_OVEREAT)] <- Data$ED_AS_OVEREAT[!is.na(Data$ED_AS_OVEREAT)]
Data$E_AS_REST[!is.na(Data$ED_AS_REST_M)] <- Data$ED_AS_REST_M[!is.na(Data$ED_AS_REST_M)]
Data$E_AS_REST[!is.na(Data$ED_AS_REST_M_1)] <- Data$ED_AS_REST_M_1[!is.na(Data$ED_AS_REST_M_1)]
Data$E_AS_REST[!is.na(Data$ED_AS_REST)] <- Data$ED_AS_REST[!is.na(Data$ED_AS_REST)]
Data$E_D_LOC[!is.na(Data$ED_D_LOC_M)] <- Data$ED_D_LOC_M[!is.na(Data$ED_D_LOC_M)]
Data$E_D_LOC[!is.na(Data$ED_D_LOC_M_1)] <- Data$ED_D_LOC_M_1[!is.na(Data$ED_D_LOC_M_1)]
Data$E_D_LOC[!is.na(Data$ED_D_LOC)] <- Data$ED_D_LOC[!is.na(Data$ED_D_LOC)]
Data$E_D_OVEREAT[!is.na(Data$ED_D_OVEREAT_M)] <- Data$ED_D_OVEREAT_M[!is.na(Data$ED_D_OVEREAT_M)]
Data$E_D_OVEREAT[!is.na(Data$ED_D_OVEREAT_M_1)] <- Data$ED_D_OVEREAT_M_1[!is.na(Data$ED_D_OVEREAT_M_1)]
Data$E_D_OVEREAT[!is.na(Data$ED_D_OVEREAT)] <- Data$ED_D_OVEREAT[!is.na(Data$ED_D_OVEREAT)]
Data$E_D_REST[!is.na(Data$ED_D_REST_M)] <- Data$ED_D_REST_M[!is.na(Data$ED_D_REST_M)]
Data$E_D_REST[!is.na(Data$ED_D_REST_M_1)] <- Data$ED_D_REST_M_1[!is.na(Data$ED_D_REST_M_1)]
Data$E_D_REST[!is.na(Data$ED_D_REST)] <- Data$ED_D_REST[!is.na(Data$ED_D_REST)]
Data$E_ES_LOC[!is.na(Data$ED_ES_LOC_M)] <- Data$ED_ES_LOC_M[!is.na(Data$ED_ES_LOC_M)]
Data$E_ES_LOC[!is.na(Data$ED_ES_LOC_M_1)] <- Data$ED_ES_LOC_M_1[!is.na(Data$ED_ES_LOC_M_1)]
Data$E_ES_LOC[!is.na(Data$ED_ES_LOC)] <- Data$ED_ES_LOC[!is.na(Data$ED_ES_LOC)]
Data$E_ES_OVEREAT[!is.na(Data$ED_ES_OVEREAT_M)] <- Data$ED_ES_OVEREAT_M[!is.na(Data$ED_ES_OVEREAT_M)]
Data$E_ES_OVEREAT[!is.na(Data$ED_ES_OVEREAT_M_1)] <- Data$ED_ES_OVEREAT_M_1[!is.na(Data$ED_ES_OVEREAT_M_1)]
Data$E_ES_OVEREAT[!is.na(Data$ED_ES_OVEREAT)] <- Data$ED_ES_OVEREAT[!is.na(Data$ED_ES_OVEREAT)]
Data$E_ES_REST[!is.na(Data$ED_ES_REST_M)] <- Data$ED_ES_REST_M[!is.na(Data$ED_ES_REST_M)]
Data$E_ES_REST[!is.na(Data$ED_ES_REST_M_1)] <- Data$ED_ES_REST_M_1[!is.na(Data$ED_ES_REST_M_1)]
Data$E_ES_REST[!is.na(Data$ED_ES_REST)] <- Data$ED_ES_REST[!is.na(Data$ED_ES_REST)]

Data$E_LOC <- NA
Data$E_OVEREAT <- NA
Data$E_REST <- NA

Data$E_REST <- ifelse(Data$E_PATTERN_B == 1, Data$E_B_REST, Data$E_REST)
Data$E_REST <- ifelse(Data$E_PATTERN_MS == 1, Data$E_MS_REST, Data$E_REST)
Data$E_REST <- ifelse(Data$E_PATTERN_L == 1, Data$E_L_REST, Data$E_REST)
Data$E_REST <- ifelse(Data$E_PATTERN_AS == 1, Data$E_AS_REST, Data$E_REST)
Data$E_REST <- ifelse(Data$E_PATTERN_D == 1, Data$E_D_REST, Data$E_REST)
Data$E_REST <- ifelse(Data$E_PATTERN_ES == 1, Data$E_ES_REST, Data$E_REST)
Data$E_LOC <- ifelse(Data$E_PATTERN_B == 1, Data$E_B_LOC, Data$E_LOC)
Data$E_LOC <- ifelse(Data$E_PATTERN_MS == 1, Data$E_MS_LOC, Data$E_LOC)
Data$E_LOC <- ifelse(Data$E_PATTERN_L == 1, Data$E_L_LOC, Data$E_LOC)
Data$E_LOC <- ifelse(Data$E_PATTERN_AS == 1, Data$E_AS_LOC, Data$E_LOC)
Data$E_LOC <- ifelse(Data$E_PATTERN_D == 1, Data$E_D_LOC, Data$E_LOC)
Data$E_LOC <- ifelse(Data$E_PATTERN_ES == 1, Data$E_ES_LOC, Data$E_LOC)
Data$E_OVEREAT <- ifelse(Data$E_PATTERN_B == 1, Data$E_B_OVEREAT, Data$E_OVEREAT)
Data$E_OVEREAT <- ifelse(Data$E_PATTERN_MS == 1, Data$E_MS_OVEREAT, Data$E_OVEREAT)
Data$E_OVEREAT <- ifelse(Data$E_PATTERN_L == 1, Data$E_L_OVEREAT, Data$E_OVEREAT)
Data$E_OVEREAT <- ifelse(Data$E_PATTERN_AS == 1, Data$E_AS_OVEREAT, Data$E_OVEREAT)
Data$E_OVEREAT <- ifelse(Data$E_PATTERN_D == 1, Data$E_D_OVEREAT, Data$E_OVEREAT)
Data$E_OVEREAT <- ifelse(Data$E_PATTERN_ES == 1, Data$E_ES_OVEREAT, Data$E_OVEREAT)



Data <- Data %>% 
  select(-PCL_DREAMS_M_1,
         -PCL_DREAMS_M_1_RT,
         -PCL_SLEEP_M_1,
         -PCL_SLEEP_M_1_RT,
         -PCL_INMEM_M_1,
         -PCL_INMEM_M_1_RT,
         -PCL_UPSET_M_1,
         -PCL_UPSET_M_1_RT,
         -PCL_AVOIDMEM_M_1,
         -PCL_AVOIDMEM_M_1_RT,
         -PCL_AVOIDPL_M_1,
         -PCL_AVOIDPL_M_1_RT,
         -PCL_LOSS_M_1,
         -PCL_LOSS_M_1_RT,
         -PCL_DISTANT_M_1,
         -PCL_DISTANT_M_1_RT,
         -PCL_TROUBPOS_M_1,
         -PCL_TROUBPOS_M_1_RT,
         -PCL_IRRITABLE_M_1,
         -PCL_IRRITABLE_M_1_RT,
         -PCL_RISK_M_1,
         -PCL_RISK_M_1_RT,
         -PCL_CONCEN_M_1,
         -PCL_CONCEN_M_1_RT,
         -ED_BD_M_1,
         -ED_BD_M_1_RT,
         -ED_FEARWT_M_1,
         -ED_FEARWT_M_1_RT,
         -ED_CONCENSHWT_M_1,
         -ED_CONCENSHWT_M_1_RT,
         -ED_CONCENFOOD_M_1,
         -ED_CONCENFOOD_M_1_RT,
         -ED_PATTERN_M_1_1,
         -ED_PATTERN_M_1_2,
         -ED_PATTERN_M_1_3,
         -ED_PATTERN_M_1_4,
         -ED_PATTERN_M_1_5,
         -ED_PATTERN_M_1_6,
         -ED_PATTERN_M_1_7,
         -ED_PATTERN_M_1_RT,
         -ED_D_LOC_M_1,
         -ED_D_LOC_M_1_RT,
         -ED_D_OVEREAT_M_1,
         -ED_D_OVEREAT_M_1_RT,
         -ED_D_REST_M_1,
         -ED_D_REST_M_1_RT,
         -ED_ES_LOC_M_1,
         -ED_ES_LOC_M_1_RT,
         -ED_ES_OVEREAT_M_1,
         -ED_ES_OVEREAT_M_1_RT,
         -ED_ES_REST_M_1,
         -ED_ES_REST_M_1_RT,
         -ED_L_LOC_M_1,
         -ED_L_LOC_M_1_RT,
         -ED_L_OVEREAT_M_1,
         -ED_L_OVEREAT_M_1_RT,
         -ED_L_REST_M_1,
         -ED_L_REST_M_1_RT,
         -ED_MS_LOC_M_1,
         -ED_MS_LOC_M_1_RT,
         -ED_MS_OVEREAT_M_1,
         -ED_MS_OVEREAT_M_1_RT,
         -ED_MS_REST_M_1,
         -ED_MS_REST_M_1_RT,
         -ED_B_LOC_M_1,
         -ED_B_LOC_M_1_RT,
         -ED_B_OVEREAT_M_1,
         -ED_B_OVEREAT_M_1_RT,
         -ED_B_REST_M_1,
         -ED_B_REST_M_1_RT,
         -ED_AS_LOC_M_1,
         -ED_AS_LOC_M_1_RT,
         -ED_AS_OVEREAT_M_1,
         -ED_AS_OVEREAT_M_1_RT,
         -ED_AS_REST_M_1,
         -ED_AS_REST_M_1_RT,
         -ED_WTCON_M_1_1,
         -ED_WTCON_M_1_2,
         -ED_WTCON_M_1_3,
         -ED_WTCON_M_1_4,
         -ED_WTCON_M_1_5,
         -ED_WTCON_M_1_6,
         -ED_WTCON_M_1_7,
         -ED_WTCON_M_1_RT,
         -ED_DRIVEN_M_1,
         -ED_DRIVEN_M_1_RT,
         -ED_FAST_M_1,
         -ED_FAST_M_1_RT,
         -ED_MOE_M_1_1,
         -ED_MOE_M_1_2,
         -ED_MOE_M_1_3,
         -ED_MOE_M_1_4,
         -ED_MOE_M_1_5,
         -ED_MOE_M_1_6,
         -ED_MOE_M_1_RT,
         -ED_BODYCH_M_1_1,
         -ED_BODYCH_M_1_2,
         -ED_BODYCH_M_1_3,
         -ED_BODYCH_M_1_4,
         -ED_BODYCH_M_1_5,
         -ED_BODYCH_M_1_6,
         -ED_BODYCH_M_1_7,
         -ED_BODYCH_M_1_8,
         -ED_BODYCH_M_1_9,
         -ED_BODYCH_M_1_RT,
         -PCL_INMEM,
         -PCL_INMEM_RT,
         -PCL_UPSET,
         -PCL_UPSET_RT,
         -PCL_AVOIDMEM,
         -PCL_AVOIDMEM_RT,
         -PCL_AVOIDPL,
         -PCL_AVOIDPL_RT,
         -PCL_LOSS,
         -PCL_LOSS_RT,
         -PCL_DISTANT,
         -PCL_DISTANT_RT,
         -PCL_TROUBPOS,
         -PCL_TROUBPOS_RT,
         -PCL_IRRITABLE,
         -PCL_IRRITABLE_RT,
         -PCL_RISK,
         -PCL_RISK_RT,
         -PCL_CONCEN,
         -PCL_CONCEN_RT,
         -ED_BD,
         -ED_BD_RT,
         -ED_FEARWT,
         -ED_FEARWT_RT,
         -ED_CONCENSHWT,
         -ED_CONCENSHWT_RT,
         -ED_CONCENFOOD,
         -ED_CONCENFOOD_RT,
         -ED_PATTERN_1,
         -ED_PATTERN_2,
         -ED_PATTERN_3,
         -ED_PATTERN_4,
         -ED_PATTERN_5,
         -ED_PATTERN_6,
         -ED_PATTERN_7,
         -ED_PATTERN_RT,
         -ED_MS_LOC,
         -ED_MS_LOC_RT,
         -ED_MS_OVEREAT,
         -ED_MS_OVEREAT_RT,
         -ED_MS_REST,
         -ED_MS_REST_RT,
         -ED_AS_LOC,
         -ED_AS_LOC_RT,
         -ED_AS_OVEREAT,
         -ED_AS_OVEREAT_RT,
         -ED_AS_REST,
         -ED_AS_REST_RT,
         -ED_ES_LOC,
         -ED_ES_LOC_RT,
         -ED_ES_OVEREAT,
         -ED_ES_OVEREAT_RT,
         -ED_ES_REST,
         -ED_ES_REST_RT,
         -ED_L_LOC,
         -ED_L_LOC_RT,
         -ED_L_OVEREAT,
         -ED_L_OVEREAT_RT,
         -ED_L_REST,
         -ED_L_REST_RT,
         -ED_D_LOC,
         -ED_D_LOC_RT,
         -ED_D_OVEREAT,
         -ED_D_OVEREAT_RT,
         -ED_D_REST,
         -ED_D_REST_RT,
         -ED_B_LOC,
         -ED_B_LOC_RT,
         -ED_B_OVEREAT,
         -ED_B_OVEREAT_RT,
         -ED_B_REST,
         -ED_B_REST_RT,
         -ED_WTCON_1,
         -ED_WTCON_2,
         -ED_WTCON_3,
         -ED_WTCON_4,
         -ED_WTCON_5,
         -ED_WTCON_6,
         -ED_WTCON_7,
         -ED_WTCON_RT,
         -ED_DRIVEN,
         -ED_DRIVEN_RT,
         -ED_FAST,
         -ED_FAST_RT,
         -ED_MOE_1,
         -ED_MOE_2,
         -ED_MOE_3,
         -ED_MOE_4,
         -ED_MOE_5,
         -ED_MOE_6,
         -ED_MOE_RT,
         -ED_BODYCH_1,
         -ED_BODYCH_2,
         -ED_BODYCH_3,
         -ED_BODYCH_4,
         -ED_BODYCH_5,
         -ED_BODYCH_6,
         -ED_BODYCH_7,
         -ED_BODYCH_8,
         -ED_BODYCH_9,
         -ED_BODYCH_RT,
         -TEST1,
         -TEST1_RT,
         -TEST2,
         -TEST2_RT,
         -TEST3,
         -TEST3_RT,
         -PCL_DREAMS_M,
         -PCL_DREAMS_M_RT,
         -PCL_SLEEP_M,
         -PCL_SLEEP_M_RT,
         -PCL_INMEM_M,
         -PCL_INMEM_M_RT,
         -PCL_UPSET_M,
         -PCL_UPSET_M_RT,
         -PCL_AVOIDMEM_M,
         -PCL_AVOIDMEM_M_RT,
         -PCL_AVOIDPL_M,
         -PCL_AVOIDPL_M_RT,
         -PCL_LOSS_M,
         -PCL_LOSS_M_RT,
         -PCL_DISTANT_M,
         -PCL_DISTANT_M_RT,
         -PCL_TROUBPOS_M,
         -PCL_TROUBPOS_M_RT,
         -PCL_IRRITABLE_M,
         -PCL_IRRITABLE_M_RT,
         -PCL_RISK_M,
         -PCL_RISK_M_RT,
         -PCL_CONCEN_M,
         -PCL_CONCEN_M_RT,
         -ED_BD_M,
         -ED_BD_M_RT,
         -ED_FEARWT_M,
         -ED_FEARWT_M_RT,
         -ED_CONCENSHWT_M,
         -ED_CONCENSHWT_M_RT,
         -ED_CONCENFOOD_M,
         -ED_CONCENFOOD_M_RT,
         -ED_PATTERN_M_1,
         -ED_PATTERN_M_2,
         -ED_PATTERN_M_3,
         -ED_PATTERN_M_4,
         -ED_PATTERN_M_5,
         -ED_PATTERN_M_6,
         -ED_PATTERN_M_7,
         -ED_PATTERN_M_RT,
         -ED_B_LOC_M,
         -ED_B_LOC_M_RT,
         -ED_B_OVEREAT_M,
         -ED_B_OVEREAT_M_RT,
         -ED_B_REST_M,
         -ED_B_REST_M_RT,
         -ED_AS_LOC_M,
         -ED_AS_LOC_M_RT,
         -ED_AS_OVEREAT_M,
         -ED_AS_OVEREAT_M_RT,
         -ED_AS_REST_M,
         -ED_AS_REST_M_RT,
         -ED_L_LOC_M,
         -ED_L_LOC_M_RT,
         -ED_L_OVEREAT_M,
         -ED_L_OVEREAT_M_RT,
         -ED_L_REST_M,
         -ED_L_REST_M_RT,
         -ED_MS_LOC_M,
         -ED_MS_LOC_M_RT,
         -ED_MS_OVEREAT_M,
         -ED_MS_OVEREAT_M_RT,
         -ED_MS_REST_M,
         -ED_MS_REST_M_RT,
         -ED_D_LOC_M,
         -ED_D_LOC_M_RT,
         -ED_D_OVEREAT_M,
         -ED_D_OVEREAT_M_RT,
         -ED_D_REST_M,
         -ED_D_REST_M_RT,
         -ED_ES_LOC_M,
         -ED_ES_LOC_M_RT,
         -ED_ES_OVEREAT_M,
         -ED_ES_OVEREAT_M_RT,
         -ED_ES_REST_M,
         -ED_ES_REST_M_RT,
         -ED_WTCON_M_1,
         -ED_WTCON_M_2,
         -ED_WTCON_M_3,
         -ED_WTCON_M_4,
         -ED_WTCON_M_5,
         -ED_WTCON_M_6,
         -ED_WTCON_M_7,
         -ED_WTCON_M_RT,
         -ED_FAST_M,
         -ED_FAST_M_RT,
         -ED_DRIVEN_M,
         -ED_DRIVEN_M_RT,
         -ED_MOE_M_1,
         -ED_MOE_M_2,
         -ED_MOE_M_3,
         -ED_MOE_M_4,
         -ED_MOE_M_5,
         -ED_MOE_M_6,
         -ED_MOE_M_RT,
         -ED_BODYCH_M_1,
         -ED_BODYCH_M_2,
         -ED_BODYCH_M_3,
         -ED_BODYCH_M_4,
         -ED_BODYCH_M_5,
         -ED_BODYCH_M_6,
         -ED_BODYCH_M_7,
         -ED_BODYCH_M_8,
         -ED_BODYCH_M_9,
         -ED_BODYCH_M_RT,
         -E_B_LOC,
         -E_B_OVEREAT,
         -E_B_REST,
         -E_MS_LOC,
         -E_MS_OVEREAT,
         -E_MS_REST,
         -E_L_LOC,
         -E_L_OVEREAT,
         -E_L_REST,
         -E_AS_LOC,
         -E_AS_OVEREAT,
         -E_AS_REST,
         -E_D_LOC,
         -E_D_OVEREAT,
         -E_D_REST,
         -E_ES_LOC,
         -E_ES_OVEREAT,
         -E_ES_REST)

Data <- Data %>% 
  mutate(TOTAL_RT_Min = TOTAL_RT/60000)

# preprocess data for longitudinal modeling ----

#load required packages
library(lubridate)
library(imputeTS)

# sort dataset by ID and time of beep
Data<-Data[order(Data$PARTICIPANT_ID, Data$CREATED_TS),]

# number days
Data <- Data %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(DayNumber = as.numeric(DateOnly - min(DateOnly)) + 1)

# number beeps
Data <- Data %>%
  group_by(PARTICIPANT_ID, DayNumber) %>%
  mutate(ResponseNumber = row_number(CREATED_TS))

# sort dataset by ID, day, and response
Data<-Data[order(Data$PARTICIPANT_ID, Data$DayNumber, Data$ResponseNumber),]

# delete extra response on day 6 for participant number s051454654
Data <- Data %>%
  filter(!(PARTICIPANT_ID == "s051454654" & DayNumber == 6 & ResponseNumber ==1))

# delete the extra response on day 7 for participant number s625494407
Data <- Data %>%
  filter(!(PARTICIPANT_ID == "s625494407" & DayNumber == 7 & ResponseNumber ==1))

# delete the extra response on day 5 for participant number s361828721
Data <- Data %>%
  filter(!(PARTICIPANT_ID == "s361828721" & DayNumber == 5 & ResponseNumber ==5))

# delete the extra response on day 7 for participant number s750684328
Data <- Data %>%
  filter(!(PARTICIPANT_ID == "s750684328" & DayNumber == 7 & ResponseNumber ==4))

# renumber beeps
Data <- Data %>%
  group_by(PARTICIPANT_ID, DayNumber) %>%
  mutate(ResponseNumber = row_number(CREATED_TS))

# sort dataset by ID, day, and response
Data<-Data[order(Data$PARTICIPANT_ID, Data$DayNumber, Data$ResponseNumber),]

ggplot_na_distribution(Data[Data$PARTICIPANT_ID == "s491208961", "P_DREAMS"])

# carry forward response from P_DREAMS and P_SLEEP for the first survey throughout the day
Data <- Data %>%
  group_by(PARTICIPANT_ID, DayNumber) %>%
  mutate(P_DREAMS = first(P_DREAMS)) %>%
  mutate(P_SLEEP = first(P_SLEEP))

ggplot_na_distribution(Data[Data$PARTICIPANT_ID == "s491208961", "P_DREAMS"])

# indicator of survey that has been completed
Data$COMPLETE <- ifelse(!is.na(Data$TOTAL_RT), 1, 0)

# calculate the number of completed surveys
Data <- Data %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(NoCompleteSurveys = sum(COMPLETE))

# calculate the percentage of completed surveys
Data <- Data %>% 
  mutate(PerComplete = NoCompleteSurveys / 84)

# exclude people with <25% of the surveys completed (21 surveys)
Data <- Data[Data$NoCompleteSurveys >= 21, ]

# exclude participants s065239759 and s762471830 who does not have a criterion A event identified
Data <- Data[!Data$PARTICIPANT_ID %in% c("s065239759"), ]
Data <- Data[!Data$PARTICIPANT_ID %in% c("s762471830"), ]

# add an extra observation between days to account for night
# Get unique values of PARTICIPANT_ID and DayNumber
unique_participants_and_days <- unique(Data[,c("PARTICIPANT_ID", "DayNumber")])

# Create a data frame of NA values for all variables except PARTICIPANT_ID and DayNumber
new_row <- data.frame(matrix(NA, nrow = nrow(unique_participants_and_days), ncol = ncol(Data) - 2))
names(new_row) <- names(Data)[-c(1,64)]

# Combine the unique values of PARTICIPANT_ID and DayNumber with the new_row data frame
new_data <- cbind(unique_participants_and_days, new_row)

# Bind the new_data to the original Data data frame
Data_with_NA_rows <- rbind(Data, new_data)

# sort data
Data_with_NA_rows<-Data_with_NA_rows[order(Data_with_NA_rows$PARTICIPANT_ID, Data_with_NA_rows$DayNumber, Data_with_NA_rows$ResponseNumber),]

# carry forward date
Data_with_NA_rows <- Data_with_NA_rows %>%
  group_by(PARTICIPANT_ID, DayNumber) %>%
  mutate(DateOnly = first(DateOnly))

# number for total responses
Data_with_NA_rows <- Data_with_NA_rows %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(TotalResponse = row_number(PARTICIPANT_ID))




# merge baseline data with longitudinal data ----

# Day1 and Day14 duplicate variable prior to merge
Data_with_NA_rows <- Data_with_NA_rows %>% 
  select(-Day1, -Day14)

Data_merge <- merge(BaselineData, Data_with_NA_rows, by = "PARTICIPANT_ID")

# sort data
Data_merge<-Data_merge[order(Data_merge$PARTICIPANT_ID, Data_merge$DayNumber, Data_merge$ResponseNumber),]

# eligibility flag based on Part 1
# Compute Part2Flag
Data_merge$Part2Flag <- NA

# Conditions
CisWomPos    <- Data_merge$PCL_Total_P1 >= 28 & Data_merge$EDEQ_Mean_P1 >= 2.86 & Data_merge$Gender == 2 & Data_merge$Sex == 2
CisWomNeg    <- Data_merge$PCL_Total_P1 < 28  | Data_merge$EDEQ_Mean_P1 < 2.86  & Data_merge$Gender == 2 & Data_merge$Sex == 2
CisManPos    <- Data_merge$PCL_Total_P1 >= 28 & Data_merge$EDEQ_Mean_P1 >= 1.38 & Data_merge$Gender == 1 & Data_merge$Sex == 1
CisManNeg    <- Data_merge$PCL_Total_P1 < 28  | Data_merge$EDEQ_Mean_P1 < 1.38  & Data_merge$Gender == 1 & Data_merge$Sex == 1
TransWomPos  <- Data_merge$PCL_Total_P1 >= 28 & Data_merge$EDEQ_Mean_P1 >= 3.11 & Data_merge$Gender == 2 & Data_merge$Sex == 1
TransWomNeg  <- Data_merge$PCL_Total_P1 < 28  | Data_merge$EDEQ_Mean_P1 < 3.11  & Data_merge$Gender == 2 & Data_merge$Sex == 1
TransManPos  <- Data_merge$PCL_Total_P1 >= 28 & Data_merge$EDEQ_Mean_P1 >= 3.12 & Data_merge$Gender == 1 & Data_merge$Sex == 2
TransManNeg  <- Data_merge$PCL_Total_P1 < 28  | Data_merge$EDEQ_Mean_P1 < 3.12  & Data_merge$Gender == 1 & Data_merge$Sex == 2
ExpanPos     <- Data_merge$PCL_Total_P1 >= 28 & Data_merge$EDEQ_Mean_P1 >= 3.09 & Data_merge$Gender > 2
ExpanNeg     <- Data_merge$PCL_Total_P1 < 28  | Data_merge$EDEQ_Mean_P1 < 3.09  & Data_merge$Gender > 2
PCLMiss      <- is.na(Data_merge$PCL_Total_P1)

# Set values based on conditions
Data_merge$Part2Flag[CisWomPos] <- 1
Data_merge$Part2Flag[CisWomNeg] <- 0
Data_merge$Part2Flag[CisManPos] <- 1
Data_merge$Part2Flag[CisManNeg] <- 0
Data_merge$Part2Flag[TransWomPos] <- 1
Data_merge$Part2Flag[TransWomNeg] <- 0
Data_merge$Part2Flag[TransManPos] <- 1
Data_merge$Part2Flag[TransManNeg] <- 0
Data_merge$Part2Flag[ExpanPos] <- 1
Data_merge$Part2Flag[ExpanNeg] <- 0
Data_merge$Part2Flag[PCLMiss] <- NA

# save complete cleaned data
write.csv(Data_merge, "long_clean_all.csv", row.names = FALSE)

# Delete rows with Part2Flag = 0
Data_merge_pos <- Data_merge[Data_merge$Part2Flag != 0, ]

# save cleaned data with people removed that no longer met criteria at orientation
write.csv(Data_merge_pos, "long_clean_pos.csv", row.names = FALSE)



# demographic information for tables----

# https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html

# load library
library(table1)

# copy baseline data for these participants to a new dataframe
Data_merge <- group_by(Data_merge, PARTICIPANT_ID)
Baseline_only <- filter(Data_merge, TotalResponse == min(TotalResponse))

# removes group for later analyses
Data_merge %>%
  ungroup() 

# write to file
write.csv(Baseline_only, "long_clean_baseline.csv", row.names = FALSE)

# import crit A categories
CritA <- read_excel("/Users/Lexie/Library/CloudStorage/GoogleDrive-lconvertino7960@sdsu.edu/Shared drives/BISH Lab - EMA Eating Pathology Study/Part 2/Data Analysis/CritACat.xlsx",
                    col_types = c("guess", "skip", "guess"))
Baseline_only <- merge(Baseline_only, CritA, by = "PARTICIPANT_ID")

# calculate age
Baseline_only$Age <- trunc((Baseline_only$DOB %--% Baseline_only$RecordedDate) / years(1))

# compliance
round(mean(Baseline_only$PerComplete),4)*100
round(sd(Baseline_only$PerComplete),4)*100
round(range(Baseline_only$PerComplete),4)*100

# Sex assigned at birth
Baseline_only$Sex[Baseline_only$Sex == 4] <- 3
Baseline_only$Sex <- factor(Baseline_only$Sex,
                            levels = c(1,2,3),
                            labels = c("Male", "Female", "Intersex"))

# Gender
Baseline_only$Gender <- factor(Baseline_only$Gender,
                               levels = c(1,2,3,4,5,6,7),
                               labels = c("Man", "Woman", "Agender", "Genderqueer", "Genderfluid", "Nonbinary", "Self-described: transmasculine"))

# Grade
Baseline_only$Grade <- factor(Baseline_only$Grade,
                              levels = c(1,2,3,4,5),
                              labels = c("Freshman", "Sophomore", "Junior", "Senior", "Senior (5+ years)"))

# Race
Baseline_only$Race_1 <- factor(Baseline_only$Race_1,
                              levels = c(1),
                              labels = c("White"))
Baseline_only$Race_2 <- factor(Baseline_only$Race_2,
                               levels = c(1),
                               labels = c("Black or African American"))
Baseline_only$Race_3 <- factor(Baseline_only$Race_3,
                               levels = c(1),
                               labels = c("Native American or American Indian"))
Baseline_only$Race_4 <- factor(Baseline_only$Race_4,
                               levels = c(1),
                               labels = c("Asian or Pacific Islander"))
Baseline_only$Race_5 <- factor(Baseline_only$Race_5,
                               levels = c(1),
                               labels = c("Different race ᶜ"))

# Ethnicity
Baseline_only$Ethnicity <- factor(Baseline_only$Ethnicity,
                                  levels = c(0,1),
                                  labels = c("Non-Hispanic/Latino", "Hispanic/Latino"))

# Sexual Orientation
Baseline_only$Sexual_Orientation <- factor(Baseline_only$Sexual_Orientation,
                                           levels = c(1,2,3,4,5,6,7,8),
                                           labels = c("Heterosexual or straight", "Gay", "Lesbian", "Bisexual", "Pansexual", "Queer", "Asexual", "Different sexual orientation ᵈ"))

# Criterion A categories
Baseline_only$CritA_cat <- factor(Baseline_only$CritA_cat,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                                  labels = c("Natural disaster", 
                                             "Fire or explosion", 
                                             "Transportation accident", 
                                             "Serious accident at work, home, or during recreational activity", 
                                             "Exposure to toxic substance", 
                                             "Physical assault", 
                                             "Assault with a weapon", 
                                             "Sexual assault",
                                             "Other unwanted or uncomfortable sexual experience",
                                             "Combat or exposure to a war-zone",
                                             "Captivity",
                                             "Life-threatening illness or injury",
                                             "Severe human suffering",
                                             "Sudden violent death (for example, homicide, suicide)",
                                             "Sudden accidental death",
                                             "Serious injury, harm, or death you caused to someone else",
                                             "Any other very stressful event or experience: stalking"))

# ED screen
Baseline_only$EDEQ_Cut <- factor(Baseline_only$EDEQ_Cut,
                                 levels = c(1,0),
                                 labels = c("Yes", "No"))

# PTSD screen
Baseline_only$PCL_Prov <- factor(Baseline_only$PCL_Prov,
                                 levels = c(1,0),
                                 labels = c("Yes", "No"))

# create demographic table
label(Baseline_only$PCL_Total_P1) <- "PCL-5"
label(Baseline_only$EDEQ_Mean_P1) <- "EDE-Q"
label(Baseline_only$PCL_Prov)  <- "Provisional PTSD Diagnosis"
label(Baseline_only$EDEQ_Cut)  <- "Positive Eating Disorder Screen"
label(Baseline_only$Sex)  <- "Sex Assigned at Birth"
label(Baseline_only$Gender) <- "Gender ᵃ"
label(Baseline_only$Race_1) <- "Race ᵇ"
label(Baseline_only$Sexual_Orientation) <- "Sexual Orientation"
label(Baseline_only$CritA_cat) <- "Type of Criterion A Event Identified"

units(Baseline_only$Age)       <- "years"
units(Baseline_only$PCL_Total_P1) <- "sum"
units(Baseline_only$EDEQ_Mean_P1) <- "mean"

my.render.cont <- function(x) {
  with(stats.default(x), c("",
                                                           "Mean (SD)"=sprintf("%.2f (%.2f)", MEAN, SD),
                                                           "Range"=sprintf("%.2f - %.2f", MIN, MAX)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.01f%%)", FREQ, PCT))))
}

footnote <- "Abbreviations: EDE-Q, eating disorder examination questionnaire; PCL-5, posttraumatic stress disorder checklist for the Diagnostic and Statistical Manual of Mental Disorders, Fifth Edition; SD, standard deviation. 
ᵃ Options also included agender, genderfluid, and genderqueer, but these options were not endorsed in the sample.
ᵇ Categories are not mutually exclusive; participants were permitted to identify more than one race.
ᶜ The two participants who self-described identified as: “ethically native but not racially” and “mixed race.”
ᵈ The two participants who self-described identified as demisexual and questioning."

Table_1 <- table1(~ Age + PCL_Total_P1 + EDEQ_Mean_P1 + PCL_Prov + EDEQ_Cut + Grade + Sex + Gender + Race_1 + Race_2 + Race_3 + Race_4 + Race_5 + 
         Ethnicity + Sexual_Orientation + CritA_cat, 
       data=Baseline_only, render.continuous=my.render.cont, 
       render.categorical=my.render.cat, overall=c(left="Analytic Sample"), footnote=footnote)

# write to Word file
library(flextable)
t1flex(Table_1) %>% 
  save_as_docx(path="Table_1.docx")


# Kalman imputation ----

# Load packages
library(imputeTS)

# sort dataset by ID, day, and response
Data_merge<-Data_merge[order(Data_merge$PARTICIPANT_ID, Data_merge$DayNumber, Data_merge$ResponseNumber),]

# plot example missing values 
ggplot_na_distribution(Data_merge[Data_merge$PARTICIPANT_ID == "s594284749", "P_TROUBPOS"])

# impute continuous PTSD data with Kalman
Data_merge_imp <- Data_merge %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(P_DREAMS_imp = na_kalman(P_DREAMS, type="level", smooth=TRUE)) %>%
  mutate(P_SLEEP_imp = na_kalman(P_SLEEP, type="level", smooth=TRUE)) %>%
  mutate(P_INMEM_imp = na_kalman(P_INMEM, type="level", smooth=TRUE)) %>%
  mutate(P_UPSET_imp = na_kalman(P_UPSET, type="level", smooth=TRUE)) %>%
  mutate(P_AVMEM_imp = na_kalman(P_AVMEM, type="level", smooth=TRUE)) %>%
  mutate(P_AVEX_imp = na_kalman(P_AVEX, type="level", smooth=TRUE)) %>%
  mutate(P_LOSS_imp = na_kalman(P_LOSS, type="level", smooth=TRUE))  %>%
  mutate(P_DISTANT_imp = na_kalman(P_DISTANT, type="level", smooth=TRUE)) %>%
  mutate(P_TROUBPOS_imp = na_kalman(P_TROUBPOS, type="level", smooth=TRUE))  %>%
  mutate(P_IRRIT_imp = na_kalman(P_IRRIT, type="level", smooth=TRUE)) %>%
  mutate(P_RISK_imp = na_kalman(P_RISK, type="level", smooth=TRUE))  %>%
  mutate(P_CONCEN_imp = na_kalman(P_CONCEN, type="level", smooth=TRUE))

# check imputation
ggplot_na_distribution(Data_merge_imp[Data_merge_imp$PARTICIPANT_ID == "s594284749", "P_TROUBPOS_imp"])

# plot example missing values
ggplot_na_distribution(Data_merge[Data_merge$PARTICIPANT_ID == "s676604469", "E_WTCON_VOM"])

# impute continuous ED data with Kalman
Data_merge_imp <- Data_merge_imp %>%
  group_by(PARTICIPANT_ID) %>%
  mutate(E_BD_imp = na_kalman(E_BD, type="level")) %>%
  mutate(E_FEARWT_imp = na_kalman(E_FEARWT, type="level")) %>%
  mutate(E_CONCENSHWT_imp = na_kalman(E_CONCENSHWT, type="level")) %>%
  mutate(E_REST_imp = na_kalman(E_REST, type="level")) %>%
  mutate(E_LOC_imp = na_kalman(E_LOC, type="level"))  %>%
  mutate(E_OVEREAT_imp = na_kalman(E_OVEREAT, type="level")) %>%
  mutate(E_WTCON_VOM_imp = na_kalman(E_WTCON_VOM, type="level") )%>%
  mutate(E_WTCON_LAX_imp = na_kalman(E_WTCON_LAX, type="level")) %>%
  mutate(E_WTCON_DIU_imp = na_kalman(E_WTCON_DIU, type="level"))

# check imputation
ggplot_na_distribution(Data_merge_imp[Data_merge_imp$PARTICIPANT_ID == "s676604469", "E_WTCON_VOM_imp"])


# recoding data ----

# coding PCL in subscales
PCL_R <- paste(c('P_DREAMS_imp', 'P_INMEM_imp', 'P_UPSET_imp'))
Data_merge_imp$P_Reexp <- rowMeans(Data_merge_imp[ , PCL_R])
PCL_Av <- paste(c('P_AVMEM_imp', 'P_AVEX_imp'))
Data_merge_imp$P_Avoid <- rowMeans(Data_merge_imp[ , PCL_Av])
PCL_NACM <- paste(c('P_LOSS_imp', 'P_DISTANT_imp', 'P_TROUBPOS_imp'))
Data_merge_imp$P_NACM <- rowMeans(Data_merge_imp[ , PCL_NACM])
PCL_Ar <- paste(c('P_SLEEP_imp', 'P_IRRIT_imp', 'P_RISK_imp', 'P_CONCEN_imp'))
Data_merge_imp$P_Arous <- rowMeans(Data_merge_imp[ , PCL_Ar])

# coding purging as one variable
Data_merge_imp$E_Purge <- as.numeric(rowSums(Data_merge_imp[, c("E_WTCON_VOM_imp", "E_WTCON_LAX_imp", "E_WTCON_DIU_imp")]) > 0)

write.csv(Data_merge_imp, "long_clean_wimp.csv", row.names = FALSE)


# test for trends - augmented Dickey-Fuller test ----

# load package
library(tseries)

# imputed variables
adf.test(Data_merge_imp$P_Reexp)
adf.test(Data_merge_imp$P_Avoid)
adf.test(Data_merge_imp$P_NACM)
adf.test(Data_merge_imp$P_Arous)
adf.test(Data_merge_imp$E_BD_imp)
adf.test(Data_merge_imp$E_CONCENSHWT_imp)
adf.test(Data_merge_imp$E_FEARWT_imp)
adf.test(Data_merge_imp$E_REST_imp)
adf.test(Data_merge_imp$E_LOC_imp)
adf.test(Data_merge_imp$E_Purge)

# non-imputed variables
adf.test(na.omit(Data_merge$P_Reexp))
adf.test(na.omit(Data_merge$P_Avoid))
adf.test(na.omit(Data_merge$P_NACM))
adf.test(na.omit(Data_merge$P_Arous))
adf.test(na.omit(Data_merge$E_BD))
adf.test(na.omit(Data_merge$E_CONCENSHWT))
adf.test(na.omit(Data_merge$E_FEARWT))
adf.test(na.omit(Data_merge$E_REST))
adf.test(na.omit(Data_merge$E_LOC))
adf.test(na.omit(Data_merge$E_Purge))

# syntax setting up GGM variables ----
Data_merge_imp <- Data_merge_imp %>% 
  rename(Reexp = P_Reexp) %>%
  rename(Avoid = P_Avoid) %>%
  rename(NACM = P_NACM) %>%
  rename(Arous = P_Arous) %>%
  rename(BD = E_BD_imp) %>%
  rename(FearWt = E_FEARWT_imp) %>%
  rename(ConShWt = E_CONCENSHWT_imp) %>%
  rename(Rest = E_REST_imp) %>%
  rename(LOC = E_LOC_imp) %>%
  rename(Purge = E_Purge) %>%
  rename(Dream = P_DREAMS_imp) %>%
  rename(Sleep = P_SLEEP_imp) %>%
  rename(Upset = P_UPSET_imp) %>%
  rename(TroPos = P_TROUBPOS_imp) %>%
  rename(Irrit = P_IRRIT_imp) %>%
  rename(DifCon = P_CONCEN_imp)
  
Data_merge <- Data_merge %>% 
  rename(Reexp = P_Reexp) %>%
  rename(Avoid = P_Avoid) %>%
  rename(NACM = P_NACM) %>%
  rename(Arous = P_Arous) %>%
  rename(BD = E_BD) %>%
  rename(FearWt = E_FEARWT) %>%
  rename(ConShWt = E_CONCENSHWT) %>%
  rename(Rest = E_REST) %>%
  rename(LOC = E_LOC) %>%
  rename(Purge = E_Purge) %>%
  rename(Dream = P_DREAMS) %>%
  rename(Sleep = P_SLEEP) %>%
  rename(Upset = P_UPSET) %>%
  rename(TroPos = P_TROUBPOS) %>%
  rename(Irrit = P_IRRIT) %>%
  rename(DifCon = P_CONCEN)

vars <- c("Reexp",	"Avoid",	"NACM",	"Arous",	"BD", "FearWt",	"ConShWt", "Rest",	"LOC", "Purge")
vars2 <- c("BD", "FearWt",	"ConShWt", "Rest",	"LOC", "Dream", "Sleep", "Upset", "TroPos", "Irrit", "DifCon")
idvar <- "PARTICIPANT_ID"
mlVARbeepvar <- "ResponseNumber"
PNbeepvar <- "TotalResponse"
dayvar <- "DayNumber"
diagnosis <- list("PTSD"=c(1:4),"ED"=c(5:10)) 
# GGM network analysis (mlvar package) with Kalman imputation, full dataset ----

# syntax pulled from https://doi.org/10.1080/00273171.2018.1454823

# load required packages
library(mlVAR)
library(qgraph)

# run model
GGM_wKalmanimp <- mlVAR(Data_merge_imp, dayvar, beepvar = mlVARbeepvar, vars=vars,
             idvar = idvar, lags = 1, estimator = "lmer",
             temporal = "correlated", contemporaneous = "correlated")

# save figure to file
pdf("GGM_wKalmanimp_temp.pdf",width=6,height=6)
GGM_wKalmanimp_t <- plot(GGM_wKalmanimp, "temporal", layout = "circle", nonsig = "hide", 
                         labels=TRUE, theme = "colorblind", vsize = 11, vTrans = 200, rule = "and", 
                         asize = 5, mar = c(5,5,5,5), label.scale.equal=TRUE, edge.labels = TRUE, 
                         edge.label.color = "black", edge.label.cex = .65, edge.label.margin = .02,
                         edge.label.position = 0.45,
                         groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, negDashed = T)
dev.off()
pdf("GGM_wKalmanimp_cont.pdf",width=6,height=6)
GGM_wKalmanimp_c <- plot(GGM_wKalmanimp, "contemporaneous", layout = "circle", nonsig = "hide", 
                         labels=TRUE, theme = "colorblind", vsize = 11, vTrans = 200, rule = "and", 
                         asize = 5, mar = c(5,5,5,5), label.scale.equal=TRUE, edge.labels = TRUE, 
                         edge.label.color = "black", edge.label.cex = .65, edge.label.margin = .02,
                         edge.label.position = 0.45,
                         groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, negDashed = T)
dev.off()
pdf("GGM_wKalmanimp_betw.pdf",width=6,height=6)
GGM_wKalmanimp_b <- plot(GGM_wKalmanimp, "between", layout = "circle", nonsig = "hide",  
                         labels=TRUE, theme = "colorblind", vsize = 11, vTrans = 200, rule = "and", 
                         asize = 5, mar = c(5,5,5,5), label.scale.equal=TRUE, edge.labels = TRUE, 
                         edge.label.color = "black", edge.label.cex = .65, edge.label.margin = .02,
                         edge.label.position = 0.5,
                         groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, negDashed = T)
dev.off()

pdf("mlVAR.pdf",width=9,height=3)
layout(t(1:3))
plot(GGM_wKalmanimp, "temporal", layout = "circle", nonsig = "hide", title = "Temporal", 
     labels=TRUE, theme = "colorblind", vsize = 15, vTrans = 200, rule = "and", 
     asize = 5, mar = c(5,5,5,5), label.scale.equal=TRUE, edge.labels = TRUE, 
     edge.label.color = "black", edge.label.cex = 1.35, edge.label.margin = .02,
     edge.label.position = 0.4,
     groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, negDashed = T)
box("figure")
plot(GGM_wKalmanimp, "contemporaneous", layout = "circle", nonsig = "hide", title = "Contemporaneous", 
     labels=TRUE, theme = "colorblind", vsize = 15, vTrans = 200, rule = "and", 
     asize = 5, mar = c(5,5,5,5), label.scale.equal=TRUE, edge.labels = TRUE, 
     edge.label.color = "black", edge.label.cex = 1.35, edge.label.margin = .02,
     edge.label.position = 0.45,
     groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, negDashed = T)
box("figure")
plot(GGM_wKalmanimp, "between", layout = "circle", nonsig = "hide",  title = "Between-Subjects", 
     labels=TRUE, theme = "colorblind", vsize = 15, vTrans = 200, rule = "and", 
     asize = 5, mar = c(5,5,5,5), label.scale.equal=TRUE, edge.labels = TRUE, 
     edge.label.color = "black", edge.label.cex = 1.35, edge.label.margin = .02,
     edge.label.position = 0.5,
     groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, negDashed = T)
box("figure")
dev.off()

qgraph(betInclude[,1:3], layout = "circle", theme = "colorblind", directed = FALSE, title = "Between-Subjects",
       edge.color = ifelse(betInclude$type=="pos","#0000D5","#BF0000"), edge.label.position = 0.2,
       vsize = 15, mar=c(3,3,3,3), label.cex= 1.25, 
       labels = vars,  maximum = 1, esize = 6,


# save output to file
save(GGM_wKalmanimp, file = "GGM_wKalmanimp.Rdata")
saveRDS(GGM_wKalmanimp, file="mlVAR_wKalmanimp_results.RDS")

GGM_wKalmanimp_summary<-summary(GGM_wKalmanimp, round=3)
write.csv(GGM_wKalmanimp_summary[["temporal"]], "GGM_wKalmanimp_summary_t.csv")
write.csv(GGM_wKalmanimp_summary[["contemporaneous"]], "GGM_wKalmanimp_summary_c.csv")
write.csv(GGM_wKalmanimp_summary[["between"]], "GGM_wKalmanimp_summary_b.csv")


# save csv files for edges
GGM_wKalmanimp_tedges <-getWmat(GGM_wKalmanimp_t)
write.csv(GGM_wKalmanimp_tedges, "GGM_wKalmanimp_tedges.csv")
GGM_wKalmanimp_cedges <-getWmat(GGM_wKalmanimp_c)
write.csv(GGM_wKalmanimp_cedges, "GGM_wKalmanimp_cedges.csv")
GGM_wKalmanimp_bedges <-getWmat(GGM_wKalmanimp_b)
write.csv(GGM_wKalmanimp_bedges, "GGM_wKalmanimp_bedges.csv")


# Bootstrapping ----
packages<-c("dplyr", "tidyr", "reshape2", "imputeTS", 
            "huge", "mlVAR", "qgraph", "parSim",
            "ggplot2", "ggpubr", "ggpmisc", "ggcorrplot", "summarytools"
)
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only=TRUE)

dir.create('./GGM_wKalmanimp-bootstraps')
users<-GGM_wKalmanimp[["IDs"]]
nBoots=20
users.boot<-vector("list", length(nBoots))
for (bootNo in seq(1,nBoots)) {
  #select random sample of 75% of users (25% subject-level case drop)
  users.boot<-sample(users, round(0.75*length(users)))
  #grab data for these users
  Data_merge_imp.boot<-Data_merge_imp %>%
    filter(PARTICIPANT_ID %in% users.boot)
  #run mlVAR models on case-dropped subsamples
  GGM_wKalmanimp.boot<-mlVAR(Data_merge_imp.boot, vars = vars,
                             idvar = idvar, lags = 1, estimator = "lmer",
                             temporal = "correlated", contemporaneous = "correlated")
  saveRDS(GGM_wKalmanimp.boot, file = paste0("./GGM_wKalmanimp-bootstraps/mlVARboot", bootNo, ".rds"))
}

#define some helper functions (adapted from those in Epskamp 2020 supplementary material, see: https://osf.io/9gq2d/):
edgeInclude <- function(x, mat, transpose = FALSE){
  n <- length(x)
  res <- 1/n * Reduce("+", lapply(x, function(x) x[[mat]]!=0))
  if (transpose){
    res <- t(res)
  }
  nBoots * res
}
signInclude <- function(x, mat, transpose = FALSE, lowertri = FALSE){
  Pos <- lapply(x, function(x) x[[mat]]>0)
  inclPos <- 1/length(x) * Reduce("+",Pos)
  dfPos <- data.frame(
    from = c(row(inclPos)),
    to = c(col(inclPos)),
    incl = c(inclPos),
    type = "pos"
  )
  Neg <- lapply(x, function(x) x[[mat]]<0)
  inclNeg <- 1/length(x) * Reduce("+",Neg)
  dfNeg <- data.frame(
    from = c(row(inclNeg)),
    to = c(col(inclNeg)),
    incl = c(inclNeg),
    type = "neg"
  )
  tab <- rbind(dfPos,dfNeg)
  if (transpose){
    tab[,1:2] <- tab[,2:1]
  }
  if (lowertri){
    tab <- tab[tab$from >= tab$to,]
  }
  tab
}

#read 75% bootstraps:
files<-list.files(path="./GGM_wKalmanimp-bootstraps", pattern="mlVARboot", full.names=TRUE)
bootstraps<-vector("list", nBoots)
for (f in seq (1, nBoots)) {
  bootstraps[[f]]$temporal<-getNet(readRDS(files[f]), "temporal")
  bootstraps[[f]]$contemporaneous<-getNet(readRDS(files[f]), "contemporaneous")
  bootstraps[[f]]$between<-getNet(readRDS(files[f]), "between")
}
saveRDS(bootstraps, "./GGM_wKalmanimp-bootstraps/all_bootstrapped_nets.rds")
#read full sample estimated network:
GGM_wKalmanimp<-readRDS("mlVAR_wKalmanimp_results.rds")
GGM_wKalmanimp_temp<-getNet(GGM_wKalmanimp, "temporal")
GGM_wKalmanimp_cont<-getNet(GGM_wKalmanimp, "contemporaneous")
GGM_wKalmanimp_betw<-getNet(GGM_wKalmanimp, "between")
#get bootstrap results (number of times edges retained across bootstraps):
#inclusion over edges (disregard sign):
#temporal:
tempInclude_noSign<-edgeInclude(bootstraps, "temporal")
write.csv(tempInclude_noSign, "bootstraps_temp.csv")
print(paste("Temporal network edge inclusion: "), quote=FALSE)
print(tempInclude_noSign)
#contemporaneous (lowertri);
contInclude_noSign<-edgeInclude(bootstraps, "contemporaneous")
write.csv(contInclude_noSign, "bootstraps_cont.csv")
print(paste("Contemporaneous network edge inclusion: "), quote=FALSE)
print(contInclude_noSign)
#between (lowertri):
betInclude_noSign<-edgeInclude(bootstraps, "between")
write.csv(betInclude_noSign, "bootstraps_betw.csv")
print(paste("Between-users network edge inclusion: "), quote=FALSE)
print(betInclude_noSign)

#visualise bootstrap inclusions probabilities
tempInclude<-signInclude(bootstraps, "temporal", transpose = FALSE)
contInclude<-signInclude(bootstraps, "contemporaneous", lowertri = TRUE)
betInclude<-signInclude(bootstraps, "between", lowertri = TRUE)

pdf("bootstrapping_results.pdf",width=9,height=3)
layout(t(1:3))
qgraph(tempInclude[,1:3], layout = "circle", theme = "colorblind", directed = TRUE, title = "Temporal", 
       edge.color = ifelse(tempInclude$type=="pos","#0000D5","#BF0000"), edge.label.position = 0.25, asize= 6, 
       vsize = 15, mar=c(3.5,3.5,3.5,3.5), label.cex= 1.25, 
       labels = vars,  maximum = 1, esize = 6,
       edge.labels=TRUE, minimum = .25,
       edge.label.cex=1.35, edge.label.margin=0.02, 
       groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, diag = FALSE, 
       vTrans = 200, label.scale.equal = TRUE)
box("figure")
qgraph(contInclude[,1:3], layout = "circle", theme = "colorblind", directed = FALSE, title = "Contemporaneous",
       edge.color = ifelse(contInclude$type=="pos","#0000D5","#BF0000"), edge.label.position = 0.5,
       vsize = 15, mar=c(3,3,3,3), label.cex= 1.25, 
       labels = vars,  maximum = 1, esize = 6,
       edge.labels=TRUE, minimum = .25,
       edge.label.cex=1.35, edge.label.margin=0.02, 
       groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, diag = FALSE, 
       vTrans = 200, label.scale.equal = TRUE)
box("figure")
qgraph(betInclude[,1:3], layout = "circle", theme = "colorblind", directed = FALSE, title = "Between-Subjects",
       edge.color = ifelse(betInclude$type=="pos","#0000D5","#BF0000"), edge.label.position = 0.2,
       vsize = 15, mar=c(3,3,3,3), label.cex= 1.25, 
       labels = vars,  maximum = 1, esize = 6,
       edge.labels=TRUE, minimum = .25,
       edge.label.cex=1.35, edge.label.margin=0.02, 
       groups=diagnosis, color=c('#f19c79', '#ffe66d'), legend=FALSE, diag = FALSE, 
       vTrans = 200, label.scale.equal = TRUE)
box("figure")
dev.off()

# CentralityPlot modified function ----
centralityPlot_LC <- function(..., labels, scale = c("raw0","raw","z-scores", "relative"), 
                              include = c("Degree","Strength","OutDegree","InDegree","OutStrength","InStrength"), 
                              theme_bw = TRUE, print = TRUE,
                              verbose = TRUE, standardized, relative, weighted = TRUE, signed = TRUE,
                              orderBy = "default", # Can also be one of the measures
                              decreasing = FALSE, varorder=vars, type
)
  
  
  
{
  if (any(include=="all") | any(include=="All")){
    include <- c("Degree","Strength","OutDegree","InDegree","OutStrength","InStrength","Closeness","Betweenness",
                 "ExpectedInfluence","OutExpectedInfluence","InExpectedInfluence")
  }
  scale <- match.arg(scale)
  if (!missing(standardized)){
    warning("'standardized' argument is deprecated and will be removed.")
  } else {
    standardized <- scale == "z-scores"
  }
  
  if (!missing(relative)){
    warning("'relative' argument is deprecated and will be removed.")    
  } else {
    relative <- scale == "relative"
  }
  
  if (scale == "z-scores"){
    if (verbose) message("Note: z-scores are shown on x-axis rather than raw centrality indices.")
  }
  if (scale == "relative"){
    if (verbose) message("Note: relative centrality indices are shown on x-axis rather than raw centrality indices.")
  }
  
  # Some dummies to get rid of NOTES:
  measure <- NULL
  value <- NULL
  node <- NULL
  type <- NULL
  
  ## I realize I should have used a more appropriate programmatic way of doing this. My
  ## programming is bad and I fo feel bad.
  
  Long <- centralityTable(..., standardized=standardized, labels=labels, relative=relative, weighted = weighted, signed = signed)
  
  # If not missing, include only include vars:
  # if (!missing(include))
  # {
  Long <- subset(Long, measure %in% include)
  # }
  
  # Re-order:
  Long$measure <- factor(Long$measure,levels = include)
  
  # Ordereing by node name to make nice paths:
  if (orderBy == "default"){
    nodeLevels <- rev(vars)
  } else {
    nodeLevels <- names(sort(tapply(Long$value[Long$measure == orderBy],Long$node[Long$measure == orderBy],mean), decreasing=decreasing))
  }
  Long$node <- factor(as.character(Long$node), levels = nodeLevels)
  Long <- Long[gtools::mixedorder(Long$node),]
  # 
  #      Long <- Long[gtools::mixedorder(Long$node),] 
  #      Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
  #    } else {
  # 
  #      Long <- Long[gtools::mixedorder(Long[[orderBy]]),] 
  #      Long$node <- factor(as.character(Long$node), levels = unique(gtools::mixedsort(as.character(Long$node))))
  #    }
  
  # PLOT:
  if (length(unique(Long$type)) > 1)
  {
    g <- ggplot(Long, aes(x = value, y = node, group = type, colour = type))
  } else {
    g <- ggplot(Long, aes(x = value, y = node, group = type)) 
  }
  
  g <- g +  geom_path() +  xlab("") + ylab("") + geom_point() 
  
  measure.labs <- c(       "Degree Centrality",               "Strength Centrality", 
                           "Outdegree Centrality",            "Indegree Centrality",
                           "Outstrength Centrality",          "Instrength Centrality", 
                           "Expected Influence Centrality",   "Out Expected Influence Centrality", 
                           "In Expected Influence Centrality")
  names(measure.labs) <- c("Degree",                          "Strength",            
                           "OutDegree",                       "InDegree",
                           "OutStrength",                     "InStrength",            
                           "ExpectedInfluence",               "OutExpectedInfluence",   
                           "InExpectedInfluence")
  
  if (length(unique(Long$graph)) > 1)
  {
    g <- g + facet_grid(graph ~ measure, scales = "free") 
  } else 
  {
    g <- g + facet_grid( ~ measure, scales = "free", labeller=labeller(measure=measure.labs)) 
  }
  
  if (theme_bw){
    g <- g + theme_bw() + theme(axis.text.y = element_text(face="bold", colour = c("#ffe66d", "#ffe66d", "#ffe66d", "#ffe66d", 
                                                                                            "#ffe66d", "#ffe66d", "#f19c79", "#f19c79", "#f19c79", "#f19c79")))
  }
  
  if (scale == "raw0"){
    g <-g + xlim(0,NA)
  }
  
  
  if (print){
    print(g)
    invisible(g)
  } else {
    return(g)
  }
}
# Centrality and bridge symptoms ----

# load required packages
library(mlVAR)
library(qgraph)
library(networktools)
library(ggplot2)



# centrality for temporal network
# create graph
pdf ("temp_cent_plot.pdf", width=4)
GGM_wKalmanimp_t_cent_plot <- centralityPlot_LC(GGM_wKalmanimp_t, scale="z-scores", labels=vars, include=c("OutExpectedInfluence"),  theme_bw=TRUE, varorder=vars, type=2)
dev.off()

# save values
GGM_wKalmanimp_t_cent <-centralityTable(GGM_wKalmanimp_t, standardized=TRUE)
write.csv(GGM_wKalmanimp_t_cent, "temp_cent.csv")

# bridge for temporal network
# temporal bridge object
GGM_wKalmanimp_t_bridge <- bridge(GGM_wKalmanimp_t, communities=diagnosis, 
                                  useCommunities = "all", directed = NULL, nodes = NULL)
# create graph
pdf("temp_bridge.pdf", width=4)
plot(GGM_wKalmanimp_t_bridge, order = "given", zscore=TRUE, include = c("Bridge Expected Influence (1-step)"), color=TRUE, colpalette="Pastel2") + 
      theme_bw() + theme(axis.text.y = element_text(face="bold", colour = c("#ffe66d", "#ffe66d", "#ffe66d", "#ffe66d", 
      "#ffe66d", "#ffe66d", "#f19c79", "#f19c79", "#f19c79", "#f19c79"
      )))
dev.off()

# calculate z-scores
GGM_wKalmanimp_t_bridge_z <- (GGM_wKalmanimp_t_bridge$`Bridge Expected Influence (1-step)`-mean(GGM_wKalmanimp_t_bridge$`Bridge Expected Influence (1-step)`))/sd(GGM_wKalmanimp_t_bridge$`Bridge Expected Influence (1-step)`)
GGM_wKalmanimp_t_bridge_z



# centrality for contemporaneous network
# create graph
pdf ("cont_cent_plot.pdf", width=4)
GGM_wKalmanimp_c_cent_plot <- centralityPlot_LC(GGM_wKalmanimp_c, include="ExpectedInfluence", scale="z-score", labels=vars, theme_bw=TRUE, varorder=vars, type=2)
dev.off()

# save values
GGM_wKalmanimp_c_cent <-centralityTable(GGM_wKalmanimp_c, standardized=TRUE)
write.csv(GGM_wKalmanimp_c_cent, "cont_cent.csv")

# bridge for contemporaneous network
# contemporaneous bridge object
GGM_wKalmanimp_c_bridge <- bridge(GGM_wKalmanimp_c, communities=diagnosis, 
                                  useCommunities = "all", directed = NULL, nodes = NULL)
# create graph
pdf("cont_bridge.pdf", width=4)
plot(GGM_wKalmanimp_c_bridge, zscore=TRUE, include = "Bridge Expected Influence (1-step)", color=TRUE, colpalette="Pastel2") + 
  theme_bw() + theme(axis.text.y = element_text(face="bold", colour = c("#ffe66d", "#ffe66d", "#ffe66d", "#ffe66d", 
                                                                                 "#ffe66d", "#ffe66d", "#f19c79", "#f19c79", "#f19c79", "#f19c79"
  )))
dev.off()

# calculate z-scores
GGM_wKalmanimp_c_bridge_z <- (GGM_wKalmanimp_c_bridge$`Bridge Expected Influence (1-step)`-mean(GGM_wKalmanimp_c_bridge$`Bridge Expected Influence (1-step)`))/sd(GGM_wKalmanimp_c_bridge$`Bridge Expected Influence (1-step)`)
GGM_wKalmanimp_c_bridge_z





# centrality for between-subjects network
# create graph
pdf ("betw_cent_plot.pdf", width=4)
GGM_wKalmanimp_b_cent_plot <- centralityPlot_LC(GGM_wKalmanimp_b, include="ExpectedInfluence", scale="z-score", labels=vars, theme_bw=TRUE, varorder=vars, type=2)
dev.off()

# save values
GGM_wKalmanimp_b_cent <-centralityTable(GGM_wKalmanimp_b, standardized=TRUE)
write.csv(GGM_wKalmanimp_b_cent, "betw_cent.csv")

# bridge for between-subjects network
# bridge object
GGM_wKalmanimp_b_bridge <- bridge(GGM_wKalmanimp_b, communities=diagnosis, 
                                  useCommunities = "all", directed = NULL, nodes = NULL)
# create graph
pdf("betw_bridge.pdf", width=4)
plot(GGM_wKalmanimp_b_bridge, zscore=TRUE, include = "Bridge Expected Influence (1-step)", color=TRUE, colpalette="Pastel2") + 
  theme_bw() + theme(axis.text.y = element_text(face="bold", colour = c("#ffe66d", "#ffe66d", "#ffe66d", "#ffe66d", 
                                                                                 "#ffe66d", "#ffe66d", "#f19c79", "#f19c79", "#f19c79", "#f19c79"
  )))
dev.off()

# calculate z-scores
GGM_wKalmanimp_b_bridge_z <- (GGM_wKalmanimp_b_bridge$`Bridge Expected Influence (1-step)`-mean(GGM_wKalmanimp_b_bridge$`Bridge Expected Influence (1-step)`))/sd(GGM_wKalmanimp_b_bridge$`Bridge Expected Influence (1-step)`)
GGM_wKalmanimp_b_bridge_z