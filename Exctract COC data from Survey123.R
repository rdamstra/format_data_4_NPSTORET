#load packages
library(tidyverse)
library(knitr)
library(stringi)
library(readxl)
library(openxlsx)
library(openxlsx)
library(lubridate)

#Import Data from Survey123 Excel Deliverable####
#set working directory for all chunks in code
#path should typically remain the same, EXCEPT update the month/year.
knitr::opts_knit$set(root.dir = "M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2025SACN/April")

#read csv file from M-drive, as dataframe (df) "i" this is all data from Survey123
i <- read.csv("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2025SACN/April/GLKN_WaterQuality_FieldDatasheet_0.csv", header = TRUE)

i <- unite(i, col = 'sitedate', c('site_code', 'surveydate_format'), sep = ' ', remove = FALSE, na.rm = FALSE)

n <- read.csv("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2025SACN/April/field_contact_details_1.csv", header = TRUE)

n <- n %>% filter(n$contact_role == 'Primary')

colnames(n)

n <- n %>% 
  select("sitedate", "contact",  "contact_role", "contact_name", "ParentGlobalID")

i_n <- merge(i, n[, c("sitedate", "contact_name")], by = "sitedate", all.x = TRUE)

colnames(i_n)

#supply column names for construction of working dfs to cut down on only needed columns
colnames(i)
#pick needed columns to work with to construct COCs and lab sample roster and assign to df "j"
#SCWRS
j <- i_n %>%
  select("park", "site_code", "contact_name", "end_time", "surveydate_format", "sample_collected_for", "sample_collected_for_other", "sacn_sample_collected_for", "qaqc_sample_collected_for", 
         "qaqc_sample_collected_for_other","chl_a_vol", "qaqc_chl_a_vol", "tss_vol", "qaqc_tss_vol", "qaqcSite", "wq_sample_type", "wq_sample_type_other", "vandorn_depth", "skip_site")


#set up time and date as date and not char.
j$surveydate_format <- as.POSIXct(j$surveydate_format, format = "%m/%d/%Y")


#filter just to 2025
#j  <- j$end_time %>% filter(between(Date, as.Date('2023-01-01'), as.Date('2024-12-31'))) #doesn't work
#j <- j %>% 
 # filter(lubridate::year(surveydate_format) %in% c(2024, 2025))

j <- j %>% 
  filter(lubridate::year(surveydate_format) == 2025)


#pick QAQC sites from "j" and assign to new df "j_qaqc".
j_qaqc <- j %>% filter(qaqcSite == "yes")

j <- j %>% filter(qaqcSite == "no")
#create a new column to capture amount chl-a filtered will to consolidate both "parent" and qaqc samples together
j <- j %>% 
  mutate(chla_filt_vol = chl_a_vol) %>% 
  mutate(tss_filt_vol = tss_vol)


#duplicate "j_qaqc" to df "j_qaqc_1" to separate and preserve filter amounts "parent" samples and the duplicate samples  
j_qaqc_1 <- j_qaqc
#create columns indicating presence of parent sample (_p) for each parameter in the sites marked as QAQC. 
#This parses the text of "sample_collected_for", and supplies a 1 if the parameter is present, 0 if it does not.  
j_qaqc <- j_qaqc %>%
  mutate(Chl_a_p = case_when(grepl("CHLa", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TP_TN_p = case_when(grepl("TN_TP", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(NAM_p = case_when(grepl("NOx_NH4", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Bottom_TP_p = case_when(grepl("Bottom TP", sample_collected_for_other) ~ 1, ignore.case = TRUE ~ 0),) %>% 
  mutate(SO4_p = case_when(grepl("SO4", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(DOC_p = case_when(grepl("DOC", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(SiO2_p = case_when(grepl("SiO2", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TSS_p = case_when(grepl("TSS", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cations_p = case_when(grepl("Cations", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0)) %>%
  mutate(Alk_p = case_when(grepl("Alk", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cl_p = case_when(grepl("Cl", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),)

#SO4, DOC, SiO2, TSS, Cations, Alk, Cl

#create a new column with the real site code as a check, for QAQC only. Replace site code with "PARK_98" in next step.
j_qaqc_1 <- j_qaqc_1 %>% 
  mutate(actual_site_code = paste0(j_qaqc_1$site_code))
#Replace actual site id with "PARK_98", where park ID is the four letter park code. Site above preserves the actual site id in another column.   
j_qaqc_1 <- j_qaqc_1 %>%
  mutate(site_code = paste0(j_qaqc_1$park, "_98", sep = ""))
#crate columns indicating presence of QAQC samples (_q) for each parameter in the sites marked as QAQC.
#This parses the text of "qaqc_sample_collected_for", and supplies a 1 if the parameter is present, 0 if it does not.
j_qaqc_1 <- j_qaqc_1 %>%
  mutate(Chl_a_q = case_when(grepl("CHLa", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TP_TN_q = case_when(grepl("TN_TP", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(NAM_q = case_when(grepl("NOx_NH4", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Bottom_TP_q= case_when(grepl("Bottom TP", qaqc_sample_collected_for_other) ~ 1, ignore.case = TRUE ~ 0),) %>% 
  mutate(SO4_p = case_when(grepl("SO4", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(DOC_p = case_when(grepl("DOC", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(SiO2_p = case_when(grepl("SiO2", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TSS_p = case_when(grepl("TSS", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cations_p = case_when(grepl("Cations", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0)) %>%
  mutate(Alk_p = case_when(grepl("Alk", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cl_p = case_when(grepl("Cl", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),)
  
  
j_qaqc <- j_qaqc  %>% 
  mutate(chla_filt_vol = chl_a_vol) %>% 
  mutate(tss_filt_vol = tss_vol)

j_qaqc_1 <- j_qaqc_1  %>% 
  mutate(chla_filt_vol = qaqc_chl_a_vol) %>% 
  mutate(tss_filt_vol = tss_vol)



#bind j_qaqc and j_qaqc_1 together to create seperate rows for "parent" and QAQC results from the two dataframes
qaqc_f <- bind_rows(j_qaqc, j_qaqc_1)

j <- j %>%
  mutate(Chl_a_p = case_when(grepl("CHLa", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TP_TN_p = case_when(grepl("TN_TP", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(NAM_p = case_when(grepl("NOx_NH4", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Bottom_TP_p = case_when(grepl("Bottom TP", sample_collected_for_other) ~ 1, ignore.case = TRUE ~ 0),) %>% 
  mutate(SO4_p = case_when(grepl("SO4", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(DOC_p = case_when(grepl("DOC", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(SiO2_p = case_when(grepl("SiO2", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TSS_p = case_when(grepl("TSS", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cations_p = case_when(grepl("Cations", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0)) %>%
  mutate(Alk_p = case_when(grepl("Alk", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cl_p = case_when(grepl("Cl", sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),)
  

j <- j %>%
  mutate(Chl_a_q = case_when(grepl("CHLa", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TP_TN_q = case_when(grepl("TN_TP", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(NAM_q = case_when(grepl("NOx_NH4", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Bottom_TP_q= case_when(grepl("Bottom TP", qaqc_sample_collected_for_other) ~ 1, ignore.case = TRUE ~ 0),) %>% 
  mutate(SO4_q = case_when(grepl("SO4", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(DOC_q = case_when(grepl("DOC", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(SiO2_q = case_when(grepl("SiO2", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(TSS_q = case_when(grepl("TSS", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cations_q = case_when(grepl("Cations", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0)) %>%
  mutate(Alk_q = case_when(grepl("Alk", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),) %>%
  mutate(Cl_q = case_when(grepl("Cl", qaqc_sample_collected_for) ~ 1, ignore.case = TRUE ~ 0),)
  

#bind the qaqc visits back into the main data stream, "j". Asign to new df "j_1". 
j_1 <- bind_rows(j, qaqc_f)
#FUSE Parent and QA data types. Use mutate to create rows and add "_p" and "q" columns for each parameter and simply add number for each parameter together 
#j_1_1 <- unite(j_1, Chl_a, Chl_a_p, Chl_a_q, sep = "", remove = TRUE, na.rm = FALSE); SHOULD = 1 for each.
j_1 <- j_1 %>% 
  mutate(Chl_a = rowSums(across(c(Chl_a_p, Chl_a_q)), na.rm = TRUE)) %>% 
  mutate(TP_TN = rowSums(across(c(TP_TN_p, TP_TN_q)), na.rm = TRUE)) %>% 
  mutate(NAM = rowSums(across(c(NAM_p, NAM_q)), na.rm = TRUE)) %>% 
  mutate(Bottom_TP = rowSums(across(c(Bottom_TP_p, Bottom_TP_q)), na.rm = TRUE)) %>% 
  mutate(SO4 = rowSums(across(c(SO4_p, SO4_q)), na.rm = TRUE)) %>% 
  mutate(DOC = rowSums(across(c(DOC_p, DOC_q)), na.rm = TRUE)) %>% 
  mutate(SiO2 = rowSums(across(c(SiO2_p, SiO2_q)), na.rm = TRUE)) %>% 
  mutate(TSS = rowSums(across(c(TSS_p, TSS_q)), na.rm = TRUE)) %>%
  mutate(Cations = rowSums(across(c(Cations_p, Cations_q)), na.rm = TRUE)) %>% 
  mutate(Alk = rowSums(across(c(Alk_p, Alk_q)), na.rm = TRUE)) %>% 
  mutate(Cl = rowSums(across(c(Cl_p, Cl_q)), na.rm = TRUE))

#create new DF "scwrs" from "j_1" and rename column names to same as on SCWRS COC.
jj <- j_1 %>%
  rename(Park_Code = park) %>%
  rename(Sample_ID = site_code) %>%
  rename(Time = end_time)%>%
  rename(Date = surveydate_format)
#Temporally split Items with Bottom TP into df "x"
#x <- jj %>% filter(Bottom_TP == 1)
#Temporally split Items with Bottom TP into df "x"
x <- jj %>% filter(Bottom_TP %in% c(1, 0))

#create new df "x_1" to hold Bottom TP QAQC sites and delete other data types. Remerge later, will be preserved in "x" 
x_1 <- x 
#add "_B10" to site codes on "x" to denote non-bottom samples
x$Sample_ID <- paste0(x$Sample_ID, "_B10", sep = "")
#set bottom TP values from x to 0, they are preserved in x_1
x$Bottom_TP[] <- 0 
x$Bottom_TP_p[] <- 0
x$Bottom_TP_q[] <- 0
#add "_B20" to sites codes on "x_1" to denote bottom TP
x_1$Sample_ID <- paste0(x_1$Sample_ID, "_B20", sep = "")
#set non-bottom TP values from x_1 to 0, they are preserved in x
x_1$chl_a_vol[] <- 0
x_1$qaqc_chl_a_vol[] <- 0
x_1$tss_vol[] <- 0
x_1$qaqc_tss_vol[] <- 0
x_1$chla_filt_vol[] <- 0
x_1$Chl_a_p[] <- 0
x_1$TP_TN_p[] <- 0
x_1$NAM_p[] <- 0
x_1$Chl_a_q[] <- 0
x_1$TP_TN_q[] <- 0
x_1$NAM_q[] <- 0
x_1$Chl_a[] <- 0
x_1$TP_TN[] <- 0
x_1$NAM_p[] <- 0
x_1$SO4_p[] <- 0
x_1$DOC_p[] <- 0
x_1$SiO2_p[] <- 0
x_1$TSS_p[] <- 0
x_1$Cations_p[] <- 0
x_1$Alk_p[] <- 0
x_1$Cl_p[] <- 0
x_1$NAM_q[] <- 0
x_1$SO4_q[] <- 0
x_1$DOC_q[] <- 0
x_1$SiO2_q[] <- 0
x_1$TSS_q[] <- 0
x_1$Cations_q[] <- 0
x_1$Alk_q[] <- 0
x_1$Cl_q[] <- 0
x_1$SO4[] <- 0
x_1$DOC[] <- 0
x_1$SiO2[] <- 0
x_1$TSS[] <- 0
x_1$Cations[] <- 0
x_1$Alk[] <- 0
x_1$Cl[] <- 0

#bind x and x_1 back together into df "btp"
btp <- bind_rows(x, x_1)
#add "_B10" to remaining site IDs in SCWRS
jj$Sample_ID <- paste0(jj$Sample_ID, "_B10", sep = "")
#bind scwrs and btp back together to re-integrate sites with Bottom TP back into regular data stream
jj_final <- bind_rows(jj, btp)
#delete non-bottom TP results in "x_1"; they are preserved in "x", will be merged back in later.
lab_samples_final <- select(jj_final, Park_Code, Sample_ID, Time, Date, Chl_a, TP_TN, NAM, SO4, DOC, SiO2, TSS, Cations, Alk, Cl, Bottom_TP, chla_filt_vol, tss_filt_vol, qaqcSite, skip_site, wq_sample_type, vandorn_depth, actual_site_code)
#Limit to 2024
#lab_samples_final_2024 <- lab_samples_final %>% 
 # filter(lubridate::year(Date) %in% c(2025))
lab_samples_final_2024 <- lab_samples_final %>% 
  filter(lubridate::year(Date) == 2025)

#remove test sites 2024
lab_samples_final_2024 <- subset(lab_samples_final_2024, !grepl("SACN_CLAM", Sample_ID))
lab_samples_final_2024 <- subset(lab_samples_final_2024, !grepl("SACN_APLE", Sample_ID))
lab_samples_final_2024 <- subset(lab_samples_final_2024, !grepl("SACN_CLAM", actual_site_code))

ct_2024 <- select(lab_samples_final_2024, Park_Code, Sample_ID, Date, Time, SO4, DOC, SiO2, TSS, Cations, Alk, Cl,tss_filt_vol, qaqcSite, skip_site, wq_sample_type, vandorn_depth, actual_site_code)

ct_2024 <- ct_2024 %>% 
  filter(if_any(c(SO4, DOC, SiO2, TSS, Cations, Alk, Cl), ~ . !=0))

ct_2024 <- ct_2024[order(as.Date(ct_2024$Date, format = "%m/%d/%Y")),]

#ct_2024 <- ct_2024 %>% arrange(mdy(ct_2024$Date))

scwrs_2024 <- select(lab_samples_final_2024, Park_Code, Sample_ID, Time, Date, Chl_a, TP_TN, NAM, Bottom_TP, chla_filt_vol, qaqcSite, skip_site, wq_sample_type, vandorn_depth, actual_site_code)

scwrs_2024 <- scwrs_2024 %>% 
  filter(if_any(c(Chl_a, TP_TN, NAM, Bottom_TP), ~ . !=0))

scwrs_2024 <- scwrs_2024[order(as.Date(scwrs_2024$Date, format = "%m/%d/%Y")),]

#scwrs_2024 <- scwrs_2024 %>% arrange(mdy(scwrs_2024$Date))

#generate date for file name 
t <- Sys.Date()

#save auto generate coc/sample roster data for 2025, date code is run is added to the end of the file name to create new version each time code is run.
write.csv(ct_2024, paste0("M:/Monitoring/Water_Quality/Large_Rivers/Data/Lab_Data/CT lab data/SACN/2025SACN/", "ct_auto_coc_", t, ".csv"), row.names = FALSE)
write.csv(scwrs_2024, paste0("M:/Monitoring/Water_Quality/Large_Rivers/Data/Lab_Data/SCWRS lab data/2025_SCWRS/", "scwrs_auto_coc_", t, ".csv"), row.names = FALSE)




  