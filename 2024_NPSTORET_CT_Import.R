library(tidyverse)
library(knitr)
library(stringi)
library(readxl)
library(openxlsx)
library(lubridate)
library(data.table)
library(readr)
library(purrr)
library(rio)

knitr::opts_knit$set(root.dir = "M:/Monitoring/Water_Quality/Large_Rivers/Data/Lab_Data/CT lab data/SACN/2024SACN")

my_files <- list.files(path = "M:/Monitoring/Water_Quality/Large_Rivers/Data/Lab_Data/CT lab data/SACN/2024SACN",
                       pattern = "EDD.xlsx$",
                       full.names = TRUE,
                       recursive = TRUE)

my_df_list <- lapply(my_files, rio::import)
my_df <- data.table::rbindlist(my_df_list, fill = TRUE)

visits_2023 <- read_xlsx("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/NPSTORET_import_2024_2025-01-31.xlsx")

qa_visits_2023 <- subset(visits_2023, QC_Site == "yes")

qa_visits_2023$`Activity End Date` <- as.POSIXct(qa_visits_2023$`Activity End Date`, format = "%m/%d/%Y")

colnames(my_df)

my_df <- my_df %>% 
  select("Sample Description", "Sampled" ,"Analyte",  "Result", "UNITS", "Reporting Limit", "LOD", "LOQ", "Dilution", "Qualifiers")  

my_df$LOQ[is.na(my_df$LOQ)] <- my_df$`Reporting Limit`[is.na(my_df$LOQ)]

my_df <- my_df %>% 
  rename("StationID" = "Sample Description") %>% 
  rename("Activity End Date" = "Sampled") %>% 
  rename("Result Text" = "Result") %>% 
  rename("Units" = "UNITS") %>% 
  rename("Detection Limit" = "LOD") %>% 
  rename("Lower Quantification Limit" = "LOQ")

#create a Numerical Results Column called "results_num" this should resolve issues below with using logical operators on a chr column (`Result Text`)
my_df <- my_df %>% 
  mutate(results_num = as.numeric(`Result Text`))

my_df <- my_df %>% 
  mutate(`Activity Start Date` = `Activity End Date`) %>% 
  mutate(QC_Sample = case_when(grepl("GLKN_00|TripQC|98|99|INDU_03!.", StationID) ~ as.character("Y"), ignore.case = TRUE ~ as.character("N"))) %>% 
  mutate(`Detection Condition` = case_when((results_num >= `Lower Quantification Limit` ~ "Detected and Quantified"), 
                                           results_num > `Detection Limit` & results_num < `Lower Quantification Limit` ~"*Present <QL", .default = "*Non-Detect")) %>% 
  mutate(StationID = case_when(grepl("GLKN|APIS00|ISRO00|INDU00|PIRO00|SLBE00|SACN00|VOYA00", StationID) ~ as.character("TripQC", StationID), TRUE ~StationID)) %>% 
  mutate(StationID = str_remove(StationID, "B10|B20|_B10|_B20"))


qa_visits_2023$`Activity End Date` <- as.POSIXct(qa_visits_2023$`Activity End Date`, format = "%m/%d/%Y")
my_df$`Activity End Date` <- as.POSIXct(my_df$`Activity End Date`, format = "%Y-%m-%d")
qa_visits_2023$`Activity End Date` <- as.POSIXct(qa_visits_2023$`Activity End Date`, format = "%Y-%m-%d")


qa_my_df <- subset(my_df, StationID == "SACN_98")

my_df <- subset(my_df, StationID != "SACN_98")

TripQC <- subset(my_df, StationID == "TripQC")

my_df <- subset(my_df, StationID != "TripQC")

qa_my_df <- as.data.frame(qa_my_df)
qa_visits_lookup_2023 <- as.data.frame(qa_visits_2023)

qa_visits_lookup_2023 <- qa_visits_lookup_2023 %>% 
  select("StationID", "Activity End Date")

qa_my_df$StationID <- qa_visits_lookup_2023[match(as.character(qa_my_df$`Activity End Date`), as.character(qa_visits_lookup_2023$`Activity End Date`)),"StationID"]


my_df$StationID <- ifelse(!grepl("^SACN_", my_df$StationID), paste0("SACN_", my_df$StationID), my_df$StationID)

df_list <- list(my_df, qa_my_df, TripQC)

ct_data_all <- df_list %>% reduce(full_join)

colnames(ct_data_all)


ct_data_all <- ct_data_all %>% 
  mutate(`Local Characteristic Name` = dplyr::recode(ct_data_all$Analyte, 
                                              "Dissolved Calcium" = "Calcium_CT",
                                              "Dissolved Organic Carbon" = "DOC_CT",
                                              "Dissolved Magnesium" = "Magnesium_CT",
                                              "Dissolved Potassium" = "Potassium_CT",
                                              "Dissolved Sodium" = "Sodium_CT",
                                              "Dissolved Silica" = "Silicate_CT",
                                              "Alkalinity Total" = "Alkalinity_CT",
                                              "Alkalinity" = "Alkalinity_CT",
                                              "Total Chloride" = "Chloride_CT",
                                              "Total Sulfate" = "Sulfate_CT",
                                              "Total Suspended Solids" = "Total_Suspended_Solids_CT"
                                              ))


ct_data_all <- ct_data_all %>%
  mutate(Relative_Depth = case_when(
    grepl("TripQC", StationID) ~ as.character(""),
    TRUE ~ "Surface"
  )) %>% 
  mutate(Activity_Type = case_when(
    grepl("Y", QC_Sample) ~ as.character("Quality Control Sample-Blind Duplicate"),
    TRUE ~ "Sample-Integrated Vertical Profile"
  ))

ct_data_all$Activity_Type <- ifelse(
  ct_data_all$StationID == "TripQC",
  "Quality Control Sample-Equipment Blank",
  ct_data_all$Activity_Type
)

ct_data_all$`Result Comment` <- paste0("Reported Value: ", as.character(ct_data_all$`Result Text`))


ct_data_all$`Result Comment` <- ifelse(
  ct_data_all$`Detection Condition` == "Detected and Quantified",
  "", ct_data_all$`Result Comment`
)

ct_data_all$`Activity Start Date` <- format(ct_data_all$`Activity Start Date`, "%m/%d/%Y")

ct_data_all$`Activity End Date` <- format(ct_data_all$`Activity End Date`, "%m/%d/%Y")

#qa_my_df$`Activity Start Date` <- format(qa_my_df$`Activity Start Date`, "%m/%d/%Y")

#qa_my_df$`Activity End Date` <- format(qa_my_df$`Activity End Date`, "%m/%d/%Y")

#qa_my_df <- subset(ct_data_all, QC_Sample == "Y")

visits_4_ct_2023 <- visits_2023 %>% 
  select(ProjectID, StationID, `Activity End Date`,`Person Name`, `Activity Start Time`, `Activity End Time`, `Activity ID`, `Start Time Zone`, `End Time Zone`, QC_Site, skip_site)

visits_4_ct_2023 <- subset(visits_4_ct_2023, skip_site == "no")

visits_4_ct_2023$QC_Site <- gsub("yes", "Y", visits_4_ct_2023$QC_Site)

visits_4_ct_2023$QC_Site <- gsub("no", "N", visits_4_ct_2023$QC_Site)

visits_4_ct_2023 <- visits_4_ct_2023 %>% rename_at('QC_Site', ~ 'QC_Sample')

#visits_4_ct_2023$QC_Site <- gsub("Y", "N", visits_4_ct_2023$QC_Site)

#unmatched_rows <- anti_join(ct_data_all, visits_4_ct_2023, c("StationID" = "StationID", "Activity End Date" = "Activity End Date", "QC_Sample" = "QC_Sample"))

#ct_data_all_2023_x <- left_join(visits_4_ct_2023, ct_data_all, c("StationID" = "StationID", "Activity End Date" = "Activity End Date", "QC_Sample" = "QC_Sample"))

#ct_data_all_2023_y <- left_join(ct_data_all, visits_4_ct_2023, c("StationID" = "StationID", "Activity End Date" = "Activity End Date", "QC_Sample" = "QC_Sample"))

ct_data_all_2023 <- left_join(ct_data_all, visits_4_ct_2023, c("StationID" = "StationID", "Activity End Date" = "Activity End Date"))

colnames(ct_data_all_2023)

ct_data_all_2023 <- ct_data_all_2023 %>% 
  select("ProjectID", "StationID", "Person Name", "Relative_Depth", "Activity Start Date", "Activity End Date",  "Activity Start Time", "Activity End Time", "Start Time Zone", "End Time Zone", 
         "QC_Sample.x", "Analyte", "Local Characteristic Name", "Result Text", "Units", "Detection Condition", "Detection Limit", "Reporting Limit", "Lower Quantification Limit", "Qualifiers", "Result Comment",  
         "Activity_Type", "Activity ID", "skip_site")

ct_data_all_2023 <- ct_data_all_2023 %>% 
  rename("QC" = "QC_Sample.x") %>% 
  rename("Activity Type" = "Activity_Type") %>% 
  rename("Relative Depth" = "Relative_Depth")

#x <- subset(ct_data_all_2023_y1, is.na(`Activity Start Time`))

#d <- c(as.character("04/11/2023", "04/14/2013"))

#y <- subset(visits_4_ct_2023, `Activity End Date` == d)

#ct_data_qc_2023 <- left_join(qa_my_df, visits_4_ct_2023, c("StationID" = "StationID", "Activity End Date" = "Activity End Date"))



#ct_data_FINAL_2023 <- rbind(ct_data_all_2023, ct_data_qc_2023)

wb <- createWorkbook()
addWorksheet(wb, "CT_2_npstoret_2024")
writeData(wb, sheet = "CT_2_npstoret_2024", ct_data_all_2023)
saveWorkbook(wb, file = paste0("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/CT_NPSTORET_import_2024_", Sys.Date(), ".xlsx"), overwrite = TRUE)


colnames(ct_data_all)



## File: GLKN_WQ_DataModifications.R
## Purpose: R code for GLKN WQ Monitoring for conversions/reformatting of lab results
##          for importing in to NPStoret.  This is the "shell" that uses function calls to
##          open additional code segments to do the reformatting depending on the lab.
##          This file is located at M:\Monitoring\Water_Quality\Data_Formatting_Macros.
##          The associated code that this calls is located at 
##          M:\Monitoring\Water_Quality\Data_Formatting_Macros\R_code_parts
## Programmer: Rebecca Key, Assistant Data Manager, NPS Great Lakes Inventory & Monitoring Network
## Date: January 7-8, 2020
## Revisions:  RLK, 3/23/2022 - added some additional notes & explanations

# Available functions:

# GLKNWQ_CT_Reformat()
#       This function will prep CT data for NPStoret import.
#       Will ask the user for the following information:
#             The WQ program (aka River or Lake)
#             The sampling year (in YYYY format)
#             The sampling park ((in 4-letter park code)
#             The Sampler's name (or initials)  [Can be left Blank if NA/Unknown]
#             The file name (e.g.: 135674EDD.xlsx))


# GLKNWQ_SCWRS_Reformat()
#       This function will prep SCWRS data for NPStoret import.
#       Will ask the user for the following information:
#             The WQ program (aka River or Lake)
#             The sampling year (in YYYY format)
#             The file name (e.g.: 2018 NPS GLKN Results.xlsx))
#             THE DL and RL for Total Nitrogen (TN) in mg/L
#             THE DL and RL for Total Phosphorus (TP) in ug/L
#             THE DL and RL for Nitrates/Nitrites (NOX) in mg/L
#             THE DL and RL for Ammonium (NH4) in mg/L
#             THE DL and RL for Chlorophyll a (Chl a) in ug/L


## NEED TO LOOK AT 2022 data for SCWRS, revise code if NOx, NH4, or TN is reported in ug/L instead of mg/L

## If running the code makes the data output look weird or breaks it (error messages, etc.) - 
## compare lab data (number of columns and column headers) to the GLKN code (number of columns 
##being pulled in and the column headers).


## How to use:
#Step 1: Run the two lines of code below, or go to CODE > RUN REGION > RUN ALL

source("M:/Monitoring/Water_Quality/Data_Formatting_Macros/R_code_parts/GLKN_WQ_CT_LabPrep_For_NPStoret.R")
source("M:/Monitoring/Water_Quality/Data_Formatting_Macros/R_code_parts/GLKN_WQ_SCWRS_LabPrep_For_NPStoret.R")

#Step 2: In the console, type GLKNWQ_CT_Reformat() or GLKNWQ_SCWRS_Reformat() depending on the dataset

#Step 3: Answer the questions asked

#Step 4: Let code run (no user interaction needed) then review the outputted excel file(s).