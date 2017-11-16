# Load Daily Toronto Temperature Data (1990-2017)

# Initialize Session ####
cat("\014")
rm(list=ls())
cat("\014")
Sys.Date()
sessionInfo()

list.of.packages <- c("readxl","readr","ggplot2","dplyr","magrittr","viridis","lubridate","grid","gridExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

dir.path <- getwd()
setwd(dir.path)

weather.files <- list.files("data/weather", pattern = "\\.csv$", full.names = T)
# NO MEAN TEMPERATURE FOR >2003 years, download more files from
# http://climate.weather.gc.ca/climate_data/daily_data_e.html?timeframe=2&hlyRange=2002-06-04%7C2017-10-24&dlyRange=2002-06-04%7C2017-10-24&mlyRange=2003-07-01%7C2006-12-01&StationID=31688&Prov=ON&urlExtension=_e.html&searchType=stnProx&optLimit=yearRange&StartYear=2004&EndYear=2017&selRowPerPage=25&Line=0&txtRadius=25&optProxType=custom&selCity=&selPark=&Day=24&Year=2004&Month=12#


df <- weather.files %>% 
   lapply(read_csv,skip = 25, 
          col_types = cols(`Date/Time` = col_character(),
                           `Year` = col_integer(),
                           `Month` = col_integer(),
                           `Day` = col_integer(),
                           `Data Quality` = col_character(),
                           `Max Temp (°C)`  = col_number(),
                           `Max Temp Flag` = col_character(),
                           `Min Temp (°C)` = col_number(),
                           `Min Temp Flag` = col_character(),
                           `Mean Temp (°C)` = col_number(),
                           `Mean Temp Flag` = col_character(),
                           `Heat Deg Days (°C)` = col_number(),
                           `Heat Deg Days Flag` = col_character(),
                           `Cool Deg Days (°C)` = col_number(),
                           `Cool Deg Days Flag` = col_character(),
                           `Total Rain (mm)` = col_number(),
                           `Total Rain Flag` = col_character(),
                           `Total Snow (cm)` = col_number(),
                           `Total Snow Flag` = col_character(),
                           `Total Precip (mm)` = col_number(),
                           `Total Precip Flag` = col_character(),
                           `Snow on Grnd (cm)` = col_number(),
                           `Snow on Grnd Flag` = col_character(),
                           `Dir of Max Gust (10s deg)` = col_number(),
                           `Dir of Max Gust Flag` = col_character(),
                           `Spd of Max Gust (km/h)` = col_number(),
                           `Spd of Max Gust Flag` = col_character())) %>%
   bind_rows()

# Rename Column Names
names(df)

df <- df %>% 
   rename(Date = `Date/Time`, Data_Quality = `Data Quality`,
          Max_TC = `Max Temp (°C)`, Min_TC = `Min Temp (°C)`,
          Mean_TC = `Mean Temp (°C)`, Heat_C = `Heat Deg Days (°C)`,
          Cool_C = `Cool Deg Days (°C)`, Tot_Rain_mm = `Total Rain (mm)`,
          Tot_Snow_cm = `Total Snow (cm)`, Tot_Precip_mm = `Total Precip (mm)`,
          Snow_G_cm = `Snow on Grnd (cm)`, D_Max_Gust_deg = `Dir of Max Gust (10s deg)`,
          Max_Gust_kmh = `Spd of Max Gust (km/h)`) %>% 
   rename(Max_T_Flag = `Max Temp Flag`,
          Min_T_Flag = `Min Temp Flag`,
          Mean_T_Flag = `Mean Temp Flag`,
          Heat_Flag = `Heat Deg Days Flag`,
          Cool_Flag = `Cool Deg Days Flag`,
          Tot_Rain_Flag = `Total Rain Flag`,
          Tot_Snow_Flag = `Total Snow Flag`,
          Tot_Precip_Flag = `Total Precip Flag`,
          Snow_G_Flag = `Snow on Grnd Flag`,
          D_Max_Gust_Flag = `Dir of Max Gust Flag`,
          Max_Gust_Flag = `Spd of Max Gust Flag`)
                               
# Change to Appropriate Column Types ####
str(df)

remover <- function(x) {gsub("\\s*", "F", x)}
# newflags <- df %>% select(ends_with("Flag")) %>% 
#    mutate_each(funs(remover)) %>% #replaces all blanks with F
#    mutate_each(funs(gsub("^FTF","T",.))) %>% #replaces all "FTF" with T
#    mutate_each(funs(as.logical)) #convert to logical

df <- df %>% select(-ends_with("Flag")) %>% 
   bind_cols(df %>% select(ends_with("Flag")) %>% 
                mutate_each(funs(remover)) %>% #replaces all blanks with F
                mutate_each(funs(gsub("^FTF","T",.))) %>% #replaces all "FTF" with T
                mutate_each(funs(as.logical)) #convert to logical
             )

# Convert to Date-time
df$Date <- ymd(df$Date) # convert 'Date' to date object
df$Year <- as.Date(paste(df$Year, 1, 1, sep = "-")) #convert 'Year' to date object
year(df$Year) #convert back to numeric value

df$Month <- month(df$Month, label = T) #
df$Day #day of month

df %>% filter(Year > "2004-01-01") %>% select(starts_with("Mean"))

# watermain data
as.POSIXct(df$Date, tz ="UTC")
str(df$Date)
wm.df$Date <- as.Date(wm.df$Date)
wm.df$wmbreak <- rep(38,length(wm.df$Date))
merge.df <- left_join(df,wm.df, by="Date")

getSeason <- function(DATES) {
   WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
   SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
   SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
   FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
   
   # Convert dates from any year to 2012 dates
   d <- as.Date(strftime(DATES, format="2012-%m-%d"))
   
   ifelse (d >= WS | d < SE, "Winter",
           ifelse (d >= SE & d < SS, "Spring",
                   ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
merge.df$Season <- getSeason(merge.df$Date)

merge.df <- merge.df %>% mutate(B = gl(4,37470/4,labels = c("Fall","Summer","Spring","Winter")))

length(merge.df$Date)


ggplot(data = merge.df, aes(x = Date)) + 
   geom_line(aes(y = Mean_TC), alpha = 0.5, size = 0.75) +
   geom_jitter(aes(y = wmbreak), colour = "#298A08", height = 20, alpha = 0.1, size = 2) +
   scale_x_date(limits = c(as.Date("1990-01-01"),as.Date("2004-01-01"))) +
   guides(colour = guide_legend(override.aes = list(alpha=1)))
