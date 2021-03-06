# This script is to clean raw data from various loggers into one datafile with calculated fluxes
library(fs)

source("R/Load packages.R")
source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")


measurement <- 120 #the length of the measurement taken on the field in seconds
startcrop <- 0 #how much to crop at the beginning of the measurement in seconds
endcrop <- 0 #how much to crop at the end of the measurement in seconds

#download and unzip files from OSF
get_file(node = "pk4bg",
         file = "Three-D_cflux_2020.zip",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_field-record_2020.csv",
         path = "data/C-Flux/summer_2020",
         remote_path = "RawData/C-Flux")

# Unzip files
zipFile <- "data/C-Flux/summer_2020/Three-D_cflux_2020.zip"
if(file.exists(zipFile)){
  outDir <- "data/C-Flux/summer_2020"
  unzip(zipFile, exdir = outDir)
}

location <- "data/C-Flux/summer_2020/rawData" #location of datafiles
#import all squirrel files and select date/time and CO2_calc columns, merge them, name it fluxes
fluxes <-
  dir_ls(location, regexp = "*CO2*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% 
  rename(CO2 = "CO2 (ppm)") %>%  #rename the column to get something more practical without space
  mutate(
    Date = dmy(Date), #convert date in POSIXct
    Datetime = as_datetime(paste(Date, Time))  #paste date and time in one column
    ) %>%
  select(Datetime,CO2)

#import date/time and PAR columns from PAR file
PAR <-
  list.files(path = location, pattern = "*PAR*", full.names = T) %>% 
  map_df(~read_table2(., "", na = c("NA"), col_names = paste0("V",seq_len(12)))) %>% #need that because logger is adding columns with useless information
  rename(Date = V2, Time = V3, PAR = V4) %>% 
  mutate(
    PAR = as.numeric(as.character(.$PAR)), #removing any text from the PAR column (again, the logger...)
    Datetime = paste(Date, Time),
    Datetime = ymd_hms(Datetime)
    ) %>% 
  select(Datetime, PAR)

#import date/time and value column from iButton file

temp_air <-dir_ls(location, regexp = "*temp*") %>% 
  map_dfr(read_csv,  na = c("#N/A"), skip = 20, col_names = c("Datetime", "Unit", "Temp_value", "Temp_dec"), col_types = "ccnn") %>%
  mutate(Temp_dec = replace_na(Temp_dec,0),
    Temp_air = Temp_value + Temp_dec/1000, #because stupid iButtons use comma as delimiter AND as decimal point
    Datetime = dmy_hms(Datetime)
    ) %>% 
  select(Datetime, Temp_air)


#join the df


combined <- fluxes %>% 
  left_join(PAR, by = "Datetime") %>% 
  left_join(temp_air, by = "Datetime")

#import the record file
#next part is specific for Three-D

three_d <- read_csv("data/C-Flux/summer_2020/Three-D_field-record_2020.csv", na = c(""), col_types = "ccntDnc") %>% 
  mutate(
    Start = as_datetime(paste(Date, Starting_time)), #converting the date as posixct, pasting date and starting time together
    # Datetime = Start, #useful for left_join
    End = Start + measurement - endcrop, #creating column End and cropping the end of the measurement
    Start = Start + startcrop #cropping the start
  ) %>%  
  rename(Plot_ID = Turf_ID) %>% 
  select(Plot_ID,Type,Replicate,Starting_time,Date,Campaign,Remarks,Start,End)
  
#matching fluxes


co2_threed <- match.flux(combined,three_d)

flux_threed <- flux.calc(co2_threed) %>% 
  rename(
    Turf_ID = Plot_ID
  ) %>% 
  write_csv("data/C-Flux/summer_2020/Three-D_c-flux_2020.csv")


#graph CO2 fluxes to visually check the data

#graph for three-d

ggplot(co2_threed, aes(x=Datetime, y=CO2)) + 
  # geom_point(size=0.005) +
  geom_line(size = 0.1, aes(group = ID)) +
  # coord_fixed(ratio = 10) +
  scale_x_datetime(date_breaks = "30 min") +
  facet_wrap(vars(Date), ncol = 1, scales = "free") +
  # geom_line(size=0.05)
  ggsave("threed.png", height = 40, width = 100, units = "cm")