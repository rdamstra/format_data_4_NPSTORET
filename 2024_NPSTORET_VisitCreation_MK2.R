#load packages
install.packages("xfun", type="binary")
library(tidyverse)
library(knitr)
library(stringi)
library(readxl)
library(openxlsx)
library(lubridate)
library(data.table)
library(readr)


#Import Data from Survey123 Excel Deliverable####
#set working directory for all chunks in code
#path should typically remain the same, EXCEPT update the year when there is new EDD from lab.
knitr::opts_knit$set(root.dir = "M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/November/GLKN_Field_WQ_Datasheet_Nov2024")

#read csv file from M-drive, as dataframe (df) "i" this is all data from Survey123
i <- read.csv("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/November/GLKN_Field_WQ_Datasheet_Nov2024/GLKN_WaterQuality_FieldDatasheet_0.csv", header = TRUE)

i <- unite(i, col = 'sitedate', c('site_code', 'surveydate_format'), sep = ' ', remove = FALSE, na.rm = FALSE)

n <- read.csv("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/November/GLKN_Field_WQ_Datasheet_Nov2024/field_contact_details_1.csv", header = TRUE)

n <- n %>% filter(n$contact_role == 'Primary')
colnames(i)
colnames(n)
#add staff list from npstoret
#sl_r <- read_excel("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/Export_Staff and Roles_20240419.xlsx", sheet = "ExportStaffandRoles") 

#sl_r <- unite(sl_r, col = 'contact_name', c('FIRST_NAME', 'LAST_NAME'), sep = ' ')

#sl_r <- sl_r %>% distinct(contact_name, .keep_all = TRUE)

#sl_r <- sl_r %>% 
  #select("contact_name")

n <- n %>% 
  select("sitedate", "contact",  "contact_role", "contact_name", "ParentGlobalID")

i_n <- merge(i, n[, c("sitedate", "contact_name")], by = "sitedate", all.x = TRUE, na.rm = TRUE)

#i_n$contact_name <- gsub("^*.\\.", "", i_x$contact_name)

#i_n$contact_name <- sl_r$contact_name[match(i_x$contact_name, sl_r$contact_name)]

#i_n <- left_join(i_x, sl_r, by = c("contact_name" = "contact_name"))

colnames(i_n$general_notes)

col_head <- read_excel("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2022 SACN/2022_qrpt_GLKN_Import_4_NPStoret_Rivers.xlsx", range = "A1:AF1")
colnames(col_head)

npstoret <- i_n %>% 
  select("park", "site_code", "contact_name", "qaqcSite", "skip_site", "skip_reason", "skip_explanation", "surveydate_format", "start_time", "end_time", "start_time_zone", "end_time_zone", 
  "secchi_avg", "secchi_read_cond", "trans_tube_avg", "water_color", "vandorn_depth", "wq_sample_type", "sonde_zmax", "sounder_zmax", "other_zmax", "benchmark_avg", "nail_avg", "air_temp_c", 
  "wind_dir", "wind_condition", "air_temp_c", "wave_height_cm", "sky_cover", "weather_notes", "general_notes", "other_comments", "cont_data_download", "cont_data_comment", "algae_presence", 
  "algae_prevalence", "algae_location", "algae_color", "algae_color_other", "algae_documentation", "algae_doc_location", "algae_smell", "algae_smell_desc", "algae_smell_other", 
  "sonde_manufacturer", "sonde_model_name", "sonde_model2_name", "x", "y", "sample_collected_for", "sample_collected_for_other", "qaqc_sample_collected_for", "qaqc_sample_collected_for_other", "internal_time")


npstoret <- unite(npstoret, col = 'Activity_Comment', c('general_notes', 'cont_data_comment'), sep = ' ', na.rm = TRUE)


npstoret <- unite(npstoret, col = 'Samples_Collected', c('sample_collected_for', 'sample_collected_for_other'), sep = ' ', na.rm = TRUE)
npstoret <- unite(npstoret, col = 'QAQC_Sample_Collected', c('qaqc_sample_collected_for', 'qaqc_sample_collected_for_other'))
class(npstoret$surveydate_format)

npstoret <- npstoret %>%
  mutate(ProjectID = case_when(grepl("ISRO|PIRO|SLBE|APIS|VOYA|INDU", park) ~ as.character("GLKNLKWQ"), grepl("MISS|UM|SACN|NAKA|STCR|CLAM|KINI|SNKE|WILO|PHIP|PACQ|APLE", park, ignore.case = TRUE) ~ as.character("GLKNRVWQ")),.before = park)

npstoret$surveydate_format <- mdy(npstoret$surveydate_format)

class(npstoret$surveydate_format)

npstoret$`Activity Start Date` <- npstoret$surveydate_format


npstoret$surveydate_format <- as.character(npstoret$surveydate_format)

#delete dashes
class(npstoret$`Activity Start Date`)

class(npstoret$surveydate_format)

#npstoret$surveydate_format <- POSIXct(npstoret$surveydate_format, format = "%m/%d/%Y")

npstoret$start_time_posix <- npstoret$start_time
npstoret$start_time_posix <- gsub(":", "", npstoret$start_time_posix)
npstoret$start_time_posix <- paste0(npstoret$start_time_posix,"00")
#$npstoret$start_time_posix <- parse_time(npstoret$start_time_posix, "%H%M%S")

#npstoret$start_time_posix <- strptime(npstoret$start_time_posix, format = "%H:%M:%S")
#npstoret$start_time_posix <- format(npstoret$start_time_posix, format = "%H:%M:%S")


npstoret$end_time_posix <- npstoret$end_time
npstoret$end_time_posix <- gsub(":", "", npstoret$end_time_posix)
npstoret$end_time_posix <- paste0(npstoret$end_time_posix,"00")
#npstoret$end_time_posix <- parse_time(npstoret$end_time_posix, "%H%M%S")

npstoret$chr_date <- gsub("-", "", npstoret$surveydate_format)

npstoret$posix_start_dt <- ymd_hms(paste(npstoret$chr_date, npstoret$start_time_posix))

npstoret$posix_end_dt <- ymd_hms(paste(npstoret$chr_date, npstoret$end_time_posix))

#npstoret$end_time_posix <- strptime(npstoret$end_time_posix, format = "%H:%M:%S")
#npstoret$end_time_posix <- format(npstoret$end_time_posix, format = "%H:%M:%S")



npstoret <- npstoret %>%
  rename(StationID = site_code) %>%
  rename(QC_Site = qaqcSite) %>%
  rename(`Person Name` = contact_name) %>% 
  rename(`Depth, Secchi Disk Depth` = secchi_avg) %>%
  rename(`Secchi Reading Condition (choice list)` = secchi_read_cond) %>%
  rename(`Transparency Tube` = trans_tube_avg) %>%
  rename(`Activity Start Time` = start_time) %>% 
  rename(`Activity End Time` = end_time) %>%
  rename(`Start Time Zone` = start_time_zone) %>%
  rename(`End Time Zone` = end_time_zone) %>%
  rename(`Water appearance (text)` = water_color) %>%
  rename(`SiteDepth(Max)`= sonde_zmax) %>%
  rename(`Water Level` = benchmark_avg) %>%
  rename(Water_Level_to_Nail = nail_avg) %>%
  rename(Wind_Direction = wind_dir) %>%
  rename(`Temperature, air` = air_temp_c) %>%
  rename(`Cloud cover (choice list)` = sky_cover) %>%
  rename(Wind_Condition = wind_condition) %>%
  rename(`Activity comment` = Activity_Comment) %>%
  rename(Weather = weather_notes) %>% 
  rename(x_coord = x) %>% 
  rename(y_coord = y)

npstoret <- add_column(npstoret, "Activity Type" = "Field Msr/Obs")
npstoret <- add_column(npstoret, "Activity ID" = "")

#Cloud Cover
#CLEAR
#SCATTERED
#BROKEN
#OVERCAST
#OBSCURE
#FOG
#LIGHT MIST

 npstoret_2024 <- npstoret %>% 
  filter(lubridate::year(posix_end_dt) %in% c(2024))

npstoret_2024$`Activity Start Date` <- format(npstoret_2024$`Activity Start Date`, "%m/%d/%Y")


#Secchi Reading Choice list (values copied from NPSTORET choice list)
#Not Recorded (no condition and no secchi readings)
#Excellent
#Moderate
#Poor
#On Bottom
npstoret_2024$`Secchi Reading Condition (choice list)`[npstoret_2024$`Secchi Reading Condition (choice list)` == '0'] <- 'Not Recorded (no condition and no secchi readings)'
npstoret_2024$`Secchi Reading Condition (choice list)`[npstoret_2024$`Secchi Reading Condition (choice list)` == '1'] <- 'Excellent'
npstoret_2024$`Secchi Reading Condition (choice list)`[npstoret_2024$`Secchi Reading Condition (choice list)` == '2'] <- 'Moderate'
npstoret_2024$`Secchi Reading Condition (choice list)`[npstoret_2024$`Secchi Reading Condition (choice list)` == '3'] <- 'Poor'
npstoret_2024$`Secchi Reading Condition (choice list)`[npstoret_2024$`Secchi Reading Condition (choice list)` == '4'] <- 'On Bottom'

#Cloud Cover (values copied from NPSTORET choice list)
#CLEAR
#SCATTERED
#BROKEN
#OVERCAST
#OBSCURE
#FOG
#LIGHT MIST
npstoret_2024$`Cloud cover (choice list)`[npstoret_2024$`Cloud cover (choice list)` == '0'] <- 'CLEAR'
npstoret_2024$`Cloud cover (choice list)`[npstoret_2024$`Cloud cover (choice list)` == '1'] <- 'SCATTERED'
npstoret_2024$`Cloud cover (choice list)`[npstoret_2024$`Cloud cover (choice list)` == '2'] <- 'BROKEN'
npstoret_2024$`Cloud cover (choice list)`[npstoret_2024$`Cloud cover (choice list)` == '3'] <- 'OVERCAST'
npstoret_2024$`Cloud cover (choice list)`[npstoret_2024$`Cloud cover (choice list)` == '4'] <- 'FOG'
npstoret_2024$`Cloud cover (choice list)`[npstoret_2024$`Cloud cover (choice list)` == '5'] <- 'LIGHT MIST'

#Set T-Tube values '> 120 cm' to a dummy value above 120 cm to force NPSTORET to populate result with *Present > QL'. Why not use '666', the mark of the Beast, for it is a human number? 
#IRON MAIDEN RULES!
npstoret_2024$`Transparency Tube`[npstoret_2024$`Transparency Tube` == '> 120 cm'] <- '666'


 npstoret_2024 <- npstoret_2024 %>% 
  mutate(`Activity End Date` = `Activity Start Date`)
class(npstoret_2024$posix_end_dt) 

npstoret_2024 <- npstoret_2024 %>% 
  select("ProjectID", "StationID", "Person Name", "QC_Site", "Activity Start Date", "Activity End Date", "Activity Start Time", "Start Time Zone", "Activity End Time", "End Time Zone", "skip_site","skip_reason", "skip_explanation","Activity ID", "Activity Type", "Depth, Secchi Disk Depth", 
         "Secchi Reading Condition (choice list)", "Transparency Tube", "Water appearance (text)", "SiteDepth(Max)", "Temperature, air", "Water Level", "Cloud cover (choice list)","Water_Level_to_Nail","Wind_Direction" , "Wind_Condition", 
         "wave_height_cm", "Weather","algae_presence", "Activity comment", "sonde_manufacturer", "sonde_model_name", "sonde_model2_name", "x_coord", "y_coord", "Samples_Collected", "QAQC_Sample_Collected", "posix_start_dt","posix_end_dt") 

class(npstoret_2024$posix_end_dt) 
npstoret_2024 <- unite(npstoret_2024, col = 'samples_plus_comments', c('Activity comment', 'Samples_Collected'), sep = ' ', remove = FALSE, na.rm = FALSE)

npstoret_2024$`Person Name`[npstoret_2024$`Person Name` == "R.Damstra"] <- "Rick Damstra" 

npstoret_2024 <- npstoret_2024 %>% 
  mutate(Invert_Samples = case_when(grepl("macro|invert|Macro|Invert|macroinvertebrate|Macroinvertebrate|bugs|Bugs", samples_plus_comments) ~ as.character("yes"), ignore.case = TRUE ~ as.character("no")))


#df_TP<-df_TP %>%
 # mutate(QC_Sample = case_when(grepl("GLKN_00|TripQC|98|99|INDU_03!.", StationID) ~ as.character("Y"), ignore.case = TRUE ~ as.character("N")), .after = `Time Collected`)

#df_TP<-df_TP %>%
 # mutate(ProjectID = case_when(grepl("ISRO|PIRO|SLBE|APIS|VOYA|INDU", `Sample ID`) ~ as.character("GLKNLKWQ"), grepl("MISS|UM|SACN|NAKA|STCR|CLAM|KINI|SNKE|WILO|PHIP|PACQ|APLE", `Sample ID`, ignore.case = TRUE) 
  #                             ~ as.character("GLKNRVWQ"), grepl("GLKN", `Sample ID`, ignore.case = TRUE) ~ as.character("TripQC")), .before = `Lab ID`)

npstoret_2024 <- npstoret_2024 %>% 
  filter(npstoret_2024$ProjectID != 'GLKNLKWQ')


#delete known test sites for 2024 must be deleted in subsequent years.
npstoret_2024 <- subset(npstoret_2024, !grepl("SACN_CLAM", StationID))
npstoret_2024 <- subset(npstoret_2024, !grepl("SACN_WILO", StationID))


npstoret_2024_skipped_sites <- npstoret_2024 %>% filter(npstoret_2024$skip_site == 'yes')

npstoret_2024 <- subset(npstoret_2024, !grepl("yes", skip_site))


other_2024 <- npstoret_2024 %>% filter(npstoret_2024$sonde_manufacturer %in% c("Unknown", "unknown", "other", "Other", "Not Applicable", "not applicable"))

npstoret_2024 <- subset(npstoret_2024, !grepl("Unknown|unknown|other|Other|Not Applicable|not applicable", sonde_manufacturer))

others_not_rep <- npstoret_2024 %>% filter(npstoret_2024$`Activity End Date` == '07/17/2024')

npstoret_2024 <- subset(npstoret_2024, !grepl("07/17/2024", `Activity End Date`))

npstoret_2024 <- subset(npstoret_2024, !grepl("yes", skip_site))

qaqc_sites_2024 <- npstoret_2024 %>% filter(npstoret_2024$QC_Site == 'yes')

invert_site_activity_2024 <- npstoret_2024 %>% filter(npstoret_2024$Invert_Samples == 'yes')

other_site_activity_2024 <- rbind(other_2024, others_not_rep)


wb <- createWorkbook()
addWorksheet(wb, "NPSTORET_2024")
addWorksheet(wb, "Skipped_Sites_2024")
addWorksheet(wb, "Other_Activity_2024")
addWorksheet(wb, "QAQC_Sites_2024")
addWorksheet(wb, "Invert_Activity_2024")
writeData(wb, sheet = "NPSTORET_2024", npstoret_2024)
writeData(wb, sheet = "Skipped_Sites_2024", npstoret_2024_skipped_sites)
writeData(wb, sheet = "Other_Activity_2024", other_site_activity_2024)
writeData(wb, sheet = "QAQC_Sites_2024", qaqc_sites_2024)
writeData(wb, sheet = "Invert_Activity_2024", invert_site_activity_2024)
saveWorkbook(wb, file = paste0("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/NPSTORET_import_2024_", Sys.Date(), ".xlsx"), overwrite = TRUE)

#write.xlsx(npstoret_2024, "M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/NPSTORET_import_2024.xlsx", sheetName = "NPSTORET_2024", append = TRUE, rowNames = FALSE)
#write.xlsx(npstoret_2024_skipped_sites, "M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/NPSTORET_import_2024.xlsx", sheetName = "Skips_2024", append = TRUE, rowNames =FALSE)
write.csv(npstoret_2024, paste0("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/", "npstoret_import_2024_", Sys.Date(), ".csv"), row.names = FALSE)

#my_files <- dir("Your path/folder etc", pattern = '\\.csv', full.names = TRUE)
#result <- do.call(rbind, lapply(my_files, read.csv, header = F, skip = 1))
#names(result) <- c("Half","Play","Type","Time")


########Create Sonde Data########
#Check and re-order columns into the correct order
#Later work: make the code column column order agnostic  
april_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/April", pattern = '\\.csv', full.names = TRUE)
april_2024 <- do.call(rbind, lapply(april_24_raw, read.csv, header = F, skip = 1))
names(april_2024) <- c("file",	"timestamp",	"manta",	"latitude",	"longitude", "annotation",	"Depth_m",	"Temp_deg_C",	"pH_units",	"HDO_mg/l",	"HDO_%Sat",	"SpCond_uS/cm",	"BP_mmHg")

#april_2024 <- april_2024 %>% 
#  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "Depth_m", "Temp_deg_C", "pH_units", "HDO_mg/l", "HDO_%Sat", "SpCond_uS/cm", "BP_mmHg")

april_2024 <- april_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

may_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/May", pattern = '\\.csv', full.names = TRUE)
may_2024 <- do.call(rbind, lapply(may_24_raw, read.csv, header = F, skip = 1))
names(may_2024) <- c("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

may_2024 <- may_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")


june_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/June", pattern = '\\.csv', full.names = TRUE)
june_2024 <- do.call(rbind, lapply(june_24_raw, read.csv, header = F, skip = 1))
names(june_2024) <- c("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

june_2024 <- june_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

july_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/July", pattern = '\\.csv', full.names = TRUE)
july_2024 <- do.call(rbind, lapply(july_24_raw, read.csv, header = F, skip = 1))
names(july_2024) <- c("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

july_2024 <- july_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

aug_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/August", pattern = '\\.csv', full.names = TRUE)
aug_2024 <- do.call(rbind, lapply(aug_24_raw, read.csv, header = F, skip = 1))
names(aug_2024) <- c("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

aug_2024 <- aug_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

sept_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/September", pattern = '\\.csv', full.names = TRUE)
sept_2024 <- do.call(rbind, lapply(sept_24_raw, read.csv, header = F, skip = 1))
names(sept_2024) <- c("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

sept_2024 <- sept_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

				
oct_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/October", pattern = '\\.csv', full.names = TRUE)
oct_2024 <- do.call(rbind, lapply(oct_24_raw, read.csv, header = F, skip = 1))
names(oct_2024) <- c("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

oct_2024 <- oct_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")


nov_24_raw <- dir("M:/Monitoring/Water_Quality/Large_Rivers/Data/Field_Data/SACN/2024SACN/sonde/November", pattern = '\\.csv', full.names = TRUE)
nov_2024 <- do.call(rbind, lapply(nov_24_raw, read.csv, header = F, skip = 1))
names(nov_2024) <- c("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")

nov_2024 <- nov_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "BP_mmHg", "Depth_m", "HDO_%Sat", "HDO_mg/l", "pH_units", "SpCond_uS/cm", "Temp_deg_C")


sonde_2024 <- bind_rows(april_2024, may_2024, june_2024, july_2024, aug_2024, sept_2024, oct_2024, nov_2024)

sonde_2024 <- unique(sonde_2024)


sonde_2024 <- sonde_2024 %>% 
  select("file", "timestamp", "manta", "latitude", "longitude", "annotation", "Depth_m", "Temp_deg_C", "pH_units", "HDO_mg/l", "HDO_%Sat", "SpCond_uS/cm", "BP_mmHg")

sonde_2024$timestamp <- as.POSIXct(sonde_2024$timestamp, "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")

sonde_2024 <- sonde_2024 %>% 
  filter(lubridate::year(timestamp) %in% 2024)

#create time ranks column in both dataframes. This assumes that time is always moving forward. May need to re-integrate skips in npstoret_2024
#sonde_2024 <- sonde_2024 %>% 
#  mutate(time_rank = rank(sonde_2024$timestamp, ties.method = "first"))
sonde_2024$file <- gsub("leonards", "Leonards", sonde_2024$file)
sonde_2024$file <- gsub("Leonards_SACN_NAKA_84.6", "Leonards", sonde_2024$file)
sonde_2024$file <- gsub("earl", "Earl", sonde_2024$file)
sonde_2024$file <- gsub("Earl_SACN_NAKA_41.3", "Earl", sonde_2024$file)
sonde_2024$file <- gsub("nam trail", "Namekagon Trail", sonde_2024$file)
sonde_2024$file <- gsub("Nam Trail", "Namekagon Trail", sonde_2024$file)
sonde_2024$file <- gsub("nam tr", "Namekagon Trail", sonde_2024$file)
sonde_2024$file <- gsub("Namkagon_Tr_SACN_NAKA_4.8", "Namekagon Trail", sonde_2024$file)
sonde_2024$file <- gsub("ccc bridge", "CCC Bridge", sonde_2024$file)
sonde_2024$file <- gsub("CCC_SACN_STCR_138.9", "CCC Bridge", sonde_2024$file)
sonde_2024$file <- gsub("norway pt", "Norway Pt", sonde_2024$file)
sonde_2024$file <- gsub("Norway_Pt_SACN_STCR_104.0", "Norway Pt", sonde_2024$file)
sonde_2024$file <- gsub("hwy 70", "Hwy 70", sonde_2024$file)
sonde_2024$file <- gsub("Hwy_70_SACN_STCR_89.7", "Hwy 70", sonde_2024$file)
sonde_2024$file <- gsub("nevers dam", "Nevers Dam", sonde_2024$file)
sonde_2024$file <- gsub("never dam", "Nevers Dam", sonde_2024$file)
sonde_2024$file <- gsub("Never Dam", "Nevers Dam", sonde_2024$file)
sonde_2024$file <- gsub("Nevers_Dam_SACN_STCR_63.8", "Nevers Dam", sonde_2024$file)
sonde_2024$file <- gsub("bayport", "Bayport", sonde_2024$file)
sonde_2024$file <- gsub("Bayport_SACN_STCR_20.0", "Bayport", sonde_2024$file)
sonde_2024$file <- gsub("hudson", "Hudson", sonde_2024$file)
sonde_2024$file <- gsub("Husdon_SACN_STCR_15.8", "Hudson", sonde_2024$file)
#sonde_2024_rank <- sonde_2024 %>% 
 # group_by(sonde_2024$file, as.Date(sonde_2024$timestamp)) %>% 
#  summarise(avg_time = mean(timestamp))

#colnames(sonde_2024_rank) <- c("filename", "Day", "Avg_Time")

#sonde_2024_rank$Day <- as.POSIXct(sonde_2024_rank$Day)

#sonde_2024_rank$time_rank <- rank(sonde_2024_rank$Avg_Time)

#locations <- read.csv("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2022 SACN/SCWRS/GLKNStations.csv")



sonde_2024 <- sonde_2024 %>% 
  mutate(StationID =  case_when(grepl("Leonards", file) ~ ("SACN_NAKA_84.6"),
                                grepl("Earl", file) ~ ("SACN_NAKA_41.3"),
                                grepl("Namekagon Trail", file) ~ ("SACN_NAKA_4.8"),
                                grepl("CCC", file) ~ ("SACN_STCR_138.9"),
                                grepl("Norway Pt", file) ~ ("SACN_STCR_104.0"),
                                grepl("Hwy 70", file) ~ ("SACN_STCR_89.7"),
                                grepl("Nevers Dam", file) ~ ("SACN_STCR_63.8"),
                                grepl("Osceola", file) ~ ("SACN_STCR_43.7"),
                                grepl("Bayport", file) ~ ("SACN_STCR_20.0"),
                                grepl("Hudson", file) ~ ("SACN_STCR_15.8"),
                                grepl("Prescott", file) ~ ("SACN_STCR_2.0")))


npstoret_2024$timestamp <- npstoret_2024$posix_end_dt

#start here
#https://stackoverflow.com/questions/74128459/merge-with-nearest-dates-in-r
#I would like to merge two datasets by ID. The date in dataset1 should match only the nearest date in dataset2. I want all dates from dataset1 to be included in the merge.
#library(dplyr)
#dataset2 %>% 
#  left_join(., dataset1, join_by(ID, closest(Date >= Date))) %>%
#  left_join(., dataset1, join_by(ID, closest(Date.x <= Date))) %>%
#  mutate(Date.y = ifelse(is.na(Date.y), Date, Date.y)) %>%
#  select(-Date)
visits_2024 <- npstoret_2024 %>% 
  select("ProjectID","StationID", "Activity Start Date", "Activity End Date", "Activity Start Time", "Start Time Zone", "Activity End Time", "End Time Zone", "timestamp", "SiteDepth(Max)", "sonde_manufacturer", "sonde_model_name", "sonde_model2_name")

visits_2024$date <- as.Date(visits_2024$timestamp)

sonde_2024$date <- as.Date(sonde_2024$timestamp)


sonde_2_npstoret_2024 <- left_join(visits_2024, sonde_2024, c("StationID" = "StationID", "date" = "date"))

colnames(sonde_2_npstoret_2024)

sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  select("ProjectID", "StationID", "Activity Start Date", "Activity End Date", "Activity Start Time", "Start Time Zone", "Activity End Time", "End Time Zone","Depth_m", "Temp_deg_C", "pH_units", "HDO_mg/l", "HDO_%Sat", "SpCond_uS/cm", "annotation", "SiteDepth(Max)", "sonde_manufacturer", "sonde_model_name", "sonde_model2_name", "manta")

#indices <- which(grepl("previous | Previous" , sonde_2_npstoret_2024$annotation))

#del_sonde <- sonde_2_npstoret_2024[indices - 1, ]


#problematic
#sonde_2_npstoret_2024 <- sonde_2_npstoret_2024[-indices, ]

#sonde_2_npstoret_2024$annotation <- gsub("(?i)delete previous", "", sonde_2_npstoret_2024$annotation)


#sonde_2_npstoret_2024$annotation <- toupper(as.character(sonde_2_npstoret_2024$annotation))

p <- subset(sonde_2_npstoret_2024, grepl("DELETE", annotation, ignore.case = TRUE))

pi <- which(grepl("DELETE", sonde_2_npstoret_2024$annotation, ignore.case = TRUE))

sonde_2_npstoret_2024 <- sonde_2_npstoret_2024[-pi, ]

#df_list <- list(del_sonde, p)

#deleted_sonde_2024 <- Reduce(function(x, y) merge(x, y, all = TRUE), df_list)

sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  group_by(StationID) %>% 
  mutate(QC_Site = if_else(abs(Depth_m - lag(Depth_m)) <= 0.2 & `Activity End Date` == lag(`Activity End Date`), "Y", "N")) %>% 
  ungroup

sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  replace_na(list(QC_Site = "N"))

sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  rename(z_max = `SiteDepth(Max)`)

sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  mutate(Relative_Depth = case_when(
    (z_max <= 2) & (abs(Depth_m - z_max) <= 0.2) ~ as.character("Bottom"),
    (z_max > 2) & (abs(Depth_m - z_max) <= 0.5) ~ as.character("Bottom"),
    (z_max <= 1) & abs(Depth_m <= 0.25) ~ as.character("Surface"), 
    (z_max > 1) & abs(Depth_m <= 0.5) ~ as.character("Surface"), 
    TRUE ~ ("Midwater")
  ))

colnames(sonde_2_npstoret_2024)       
         
sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  rename(Depth = Depth_m) %>% 
  rename(`Temp_Manta+20` = Temp_deg_C) %>% 
  rename(`pH_Manta+20` = pH_units) %>% 
  rename(`DO_Manta+20` = `HDO_mg/l`) %>% 
  rename(`DO%_Manta+20` = `HDO_%Sat`) %>% 
  rename(`EC25_Manta+20` = `SpCond_uS/cm`) %>% 
  rename(`manta_field_annotation` = annotation) %>% 
  rename(`s123_sonde_sn` = sonde_model2_name) %>% 
  rename(manta_sn = manta) %>% 
  rename(`QC Sample` = QC_Site)
  
sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  mutate(`Activity Type` = case_when(
    grepl("N", `QC Sample`) ~ "Field Msr/Obs-Portable Data Logger",
    grepl("Y", `QC Sample`) ~ "Quality Control Field Replicate Portable Data Logger"))




sonde_2_npstoret_2024 <- sonde_2_npstoret_2024 %>% 
  select("ProjectID", "StationID", "Activity Start Date", "Activity End Date", "Activity Start Time", "Start Time Zone", "Activity End Time", "End Time Zone", "Activity Type",
         "Relative_Depth", "QC Sample","Depth", "Temp_Manta+20", "pH_Manta+20", "DO_Manta+20", "DO%_Manta+20", "EC25_Manta+20", "manta_field_annotation","sonde_manufacturer", "sonde_model_name",
         "s123_sonde_sn", "manta_sn") 


wb <- createWorkbook()
addWorksheet(wb, "sonde_2_npstoret_2024")
addWorksheet(wb, "Auto_Deleled_by_Code")
writeData(wb, sheet = "sonde_2_npstoret_2024", sonde_2_npstoret_2024)
writeData(wb, sheet = "Auto_Deleled_by_Code", del_sonde)
saveWorkbook(wb, file = paste0("M:/Monitoring/Water_Quality/Large_Rivers/Data/Final_Data/SACN/2024 SACN/Sonde_NPSTORET_import_2024_", Sys.Date(), ".xlsx"), overwrite = TRUE)


#x$timestamp_diff <- as.numeric(difftime(sonde_2024$timestamp, visits_2024$timestamp, units = "mins"))


#sonde_visits_2024 <- sonde_2024 %>% 
  #left_join(sonde_2024, visits_2024, join_by(StationID, closest(timestamp >= timestamp))) %>% 
  #left_join(sonde_2024, visits_2024, join_by(StationID, closest(timestamp <= timestamp)))
  


  
  #df_TP<-left_join(df_TP, ActivityLookUp_Final, by=c("StationID"="StationID", "Local Characteristic Name" = "Local.Characteristic.Name", "QC_Sample" = "QC_Sample"))





#npstoret_2024$posix_start_dt <- as.POSIXct(npstoret_2024$posix_start_dt, "%Y-%m-%d %H:%M:%S")

#npstoret_2024$posix_end_dt <- as.POSIXct(npstoret_2024$posix_end_dt, "%Y-%m-%d %H:%M:%S")


#npstoret_2024$timestamp <- as.POSIXct((as.numeric(npstoret_2024$posix_end_dt) + as.numeric(npstoret_2024$posix_start_dt)) / 2, origin = '1970-01-01', "%Y-%m-%d %H:%M:%S", tz = "America/Chicago") 


#sonde_2024$timestamp <- as.POSIXct(sonde_2024$timestamp, "%Y-%m-%d %H:%M:%S", tz = "America/Chicago")  


#sonde_2024_rank$StationID <- sonde_2024_rank %>% 
  
#sonde_2024 <- sonde_2024 %>% 
#  mutate(Hour_Group = cumsum(sonde_2024$timestamp), breaks = "1 hour", labels = FALSE) %>% 
#  mutate(time_rank = rank(Hour_Group, ties.method = "min"))
  
  
#npstoret_2024 <- npstoret_2024 %>% 
 # mutate(time_rank = rank(npstoret_2024$posix_end_dt, ties.method = "first"))

#npstoret_2024$Day <- as.POSIXct.Date(npstoret_2024$posix_end_dt, format = "%Y-m%-%d") 

#f <- left_join(sonde_2024_rank, npstoret_2024,  by = c("StationID" = "StationID", "Day" = "Day"), copy = FALSE, suffix = c(".sonde", ".npstoret"), keep = TRUE, )


#f <- f %>% 
  #select("StationID.sonde" ,"Day.sonde","filename")


#f <- left_join(sonde_2024_rank, npstoret_2024, by = c("time_rank" = "time_rank"), copy = TRUE, suffix = c(".x", ".y"))



#df_TP<-left_join(df_TP, ActivityLookUp_Final, by=c("StationID"="StationID", "Local Characteristic Name" = "Local.Characteristic.Name", "QC_Sample" = "QC_Sample"))
# Assuming your dataframes are already loaded into R as npstoret_2024 and sonde_2024

# First, we'll create a function to check if the timestamp falls within the start and end date/times
#within_range <- function(timestamp, start, end) {
 # timestamp >= start & timestamp <= end
#}



# Now, we'll use the rowwise() function to apply this to each row, and then use mutate() to add the new columns
#sonde_2024 <- sonde_2024 %>%
 # rowwise() %>%
  #mutate(
   # StationID = npstoret_2024$StationID[within_range(timestamp, npstoret_2024$posix_start_dt, npstoret_2024$posix_end_dt)],
    #ActivityStartDate = npstoret_2024$`Activity Start Date`[within_range(timestamp, npstoret_2024$posix_start_dt, npstoret_2024$posix_end_dt)],
  #  ActivityEndDate = npstoret_2024$`Activity End Date`[within_range(timestamp, npstoret_2024$posix_start_dt, npstoret_2024$posix_end_dt)],
   # ActivityStartTime = npstoret_2024$`Activity Start Time`[within_range(timestamp, npstoret_2024$posix_start_dt, npstoret_2024$posix_end_dt)],
  #  ActivityEndTime = npstoret_2024$`Activity End Time`[within_range(timestamp, npstoret_2024$posix_start_dt, npstoret_2024$posix_end_dt)]
#  ) %>%
 # ungroup() # Ungroup the data frame after rowwise operations


#npstoret_2024 <- data.table(npstoret_2024)

#sonde_2024 <- data.table(sonde_2024)

#create a day rank


#sonde_list <- list(april_2024, may_2024, june_2024, july_2024, aug_2024, sept_2024, oct_2024, nov_2024)

#sonde_list %>% reduce(full_join, by = 'timestamp')

#sacn_sonde_2024 <- merge(april_2024, may_2024, june_2024, july_2024, aug_2024, sept_2024, oct_2024, nov_2024, by = 'timestamp', all.x = TRUE)
#sacn_sonde_2024[is.na(sacn_sonde_2024)] <- 0