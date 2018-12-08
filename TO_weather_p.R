# Toronto Weather from Pearson International Airport
#   https://toronto.weatherstats.ca/download.html

list.of.packages <- c("readr","ggplot2","plyr","dplyr","tidyr","magrittr",
                      "viridis","lubridate","grid","gridExtra","purrr",
                      "knitr","dygraphs","xts","ochRe")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)


weather_p.path <- paste0(getwd(),"/data/weather_p/")
weather.files <- list.files(weather_p.path, pattern = "\\.csv$")

weather.files

TO.daily <- read_csv(paste0(weather_p.path,weather.files[1])) #Toronto Daily
TO.xdaily <- read_csv(paste0(weather_p.path,weather.files[2])) #Toronto Extreme Daily
TO.ndaily <- read_csv(paste0(weather_p.path,weather.files[3])) #Toronto Normal Daily

# Explore
str(TO.daily)
dim(TO.daily)

# Assess Missing Data
lapply(TO.daily,function(x) { any(is.na(x))} ) %>% unlist() %>% as.data.frame() 
# all columns have at least one missing data point

NA.TO.daily <- TO.daily %>% 
  map(~sum(is.na(.))) %>% 
  as.data.frame() %>% 
  gather(var,nMiss)

class.TO.daily <- TO.daily %>%
  map(~class(.)[1]) %>%
  as.data.frame() %>%
  gather(var,class)

NA.TO.daily <- NA.TO.daily %>% 
  mutate(exist_prop = 1-nMiss/nrow(TO.daily)) %>% 
  mutate(class = class.TO.daily$class) %>%
  arrange(desc(var)) %>%
  mutate(var = factor(var, levels = unique(sort(var, decreasing = TRUE))))

# Threshold for "good" variables to use
threshold <- 0.9

# Visualize Missing Data by class
ggplot(data=NA.TO.daily, aes(x=var, y=exist_prop, fill=class)) +
  geom_bar(stat="identity", color="grey5", width=0.75, alpha=0.95, size=0.25) + 
  scale_x_discrete(expand = c(0.01,0.01)) + 
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  coord_flip() +
  scale_fill_brewer(type="qual", palette=3, direction=-1) + theme_minimal() +
  labs(title="Missing Values in Toronto Daily Weather Data",
    x="Proportion of Missing Values (NAs)", y="Variables") +
  geom_hline(yintercept = threshold,linetype = 2, alpha = 0.85)

# CONCLUSION:
NA.TO.daily %>% #select rows that contain at least 90% non-missing values
  slice(which(exist_prop > threshold)) %>% select(var) %>% as.data.frame() %>% .[,1]

# avg_temperature, cooldegdays, date, growdegdays_10, growdegdays_5, heatdegdays, 
# max_temperature, min_temperature, precipitation, rain, snow 

TO.daily %>% group_by(date) %>% count()

# Add time month and year units
TO.daily <- TO.daily %>%
  mutate(week = floor_date(.$date, unit = "week")) %>%
  mutate(month = floor_date(.$date, unit = "month")) %>%
  mutate(year = floor_date(.$date, unit = "year"))

# merge wm.df to TO.daily
wm.df <- left_join(wm.df,TO.daily,by = "date")
str(wm.df)
# all .x datetimes are POSIXct class and all .y datetimes are Date class


