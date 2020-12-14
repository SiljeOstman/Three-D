# Read in packages

library("tidyverse")
library("lubridate")
library("dplyr")

###########################
### READ IN DATA ###
###########################

source("R/Load packages.R")
# only needed for soiltemp template
source("R/Rgathering/ReadInPlotLevel.R")


#### CLIMATE DATA ####

# Read in meta data
metaTomst <- read_csv2("data/INCLINE/metaData/Logger_info.csv", col_names = TRUE, na = c(""), col_types = "fcffffcccc") %>% 
  mutate(
    Date_logger_in = dmy(Date_logger_in),
    Date_logger2_in = dmy(Date_logger2_in)
  ) %>%
  filter(!is.na(Date_logger_in), !is.na(Date_logger2_in))

### Read in files
files <- dir(path = "data/INCLINE/Fall_2020", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# remove empty file
files <- files[!(files %in% c("data/INCLINE/Fall_2020/data_94194607_2.csv"))]

# Function to read in data
temp <- map_df(set_names(files), function(file) {
  file %>% 
    set_names() %>% 
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
}, .id = "File")


# These are 3 unknown loggers. Temp pattern does not fit with rest of the data. And short time period
# "94201711", "94201712", "94201713"


TomstLogger_2020 <- temp %>% 
  # rename column names
  rename("ID" = "X1", "Date_Time" = "X2", "Time_zone" = "X3", "SoilTemperature" = "X4", "GroundTemperature" = "X5", "AirTemperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>% 
  mutate(Date_Time = ymd_hm(Date_Time)) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(LoggerID = substr(File, nchar(File)-13, nchar(File)-6)) %>% 
  left_join(metaTomst, by = c("LoggerID" = "Tomst-logger")) %>% 
  
  # Data curation
  
  # Remove data before initial date time
  group_by(LoggerID) %>% 
  filter(Date_Time > InitialDate_Time) %>% 
  
  # fix wrong values
  mutate(AirTemperature = case_when(LoggerID %in% c("94195252", "94195220") & AirTemperature < -40 ~ NA_real_,
                                    LoggerID == "94195209" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                    TRUE ~ as.numeric(AirTemperature)),
         
         GroundTemperature = case_when(LoggerID %in% c("94195208", "94195252") & GroundTemperature < -40 ~ NA_real_,
                                       LoggerID == "94195209" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                       TRUE ~ as.numeric(GroundTemperature)),
         
         SoilTemperature = case_when(LoggerID %in% c("94195252", "94195236") & SoilTemperature < -40 ~ NA_real_,
                                     LoggerID %in% c("94200493", "94200499") & Date_Time < "2020-07-03 08:00:00" ~ NA_real_,
                                     LoggerID %in% c("94195208") & ErrorFlag == 1 ~ NA_real_,
                                     LoggerID %in% c("94200493") & Date_Time > "2020-07-17 01:00:00" & Date_Time < "2020-09-16 01:00:00" ~ NA_real_,
                                     LoggerID == "94195209" & Date_Time > "2020-08-12 00:00:00" & Date_Time < "2020-08-13 00:00:00" ~ NA_real_,
                                    TRUE ~ as.numeric(SoilTemperature)))


# Save clean file
write_csv(x = TomstLogger_2019_2020, path = "data_cleaned/climate/THREE-D_TomstLogger_2019_2020.csv")


# Checking data
dd <- TomstLogger_2019_2020


dd %>% 
  #filter(destSiteID == "Lia") %>% 
  filter(LoggerID %in% c("94200493", "94200499")) %>% 
  #filter(SoilTemperature < 20) %>% 
  filter(Date_Time < "2020-07-05 08:00:00") %>% 
  ggplot(aes(x = Date_Time, y = SoilTemperature, colour = as.factor(LoggerID))) +
  geom_line() +
  geom_vline(xintercept = ymd_hms("2020-06-25 12:00:00")) +
  facet_wrap(~ LoggerID) +
  theme(legend.position="none")

