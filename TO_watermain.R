# Mapping Watermain Breaks in City of Toronto ####

# Initialize Session ####
cat("\014")
rm(list=ls())
cat("\014")
Sys.Date()
sessionInfo()

list.of.packages <- c("readr","readxl","ggplot2","dplyr","magrittr",
                      "viridis","lubridate","grid","gridExtra",
                      "maps","ggmap","cluster","knitr","dygraphs","xts")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

getwd()

watermain.files <- list.files("data", pattern = "\\.xlsx$")

# Data ####
wm.df <- read_excel(paste0("data/",watermain.files))
head(wm.df)
names(wm.df) <- c("Date","year","X_coord","Y_coord")
dim(wm.df)
str(wm.df)
wm.df$year_f <- as.factor(wm.df$year)
wm.df$year <- as.integer(wm.df$year)

# Add extra resolution with dates
wm.df$week <- floor_date(wm.df$Date, unit = "week")
wm.df$month <- floor_date(wm.df$Date, unit = "month")

# Save modified data frame ####
#write.table(wm.df,file = "clean_data/TO_watermain_breaks.csv",sep = ",", row.names = FALSE)

# Exploratory Analysis + Cleaning ####
summary(wm.df) #some values for X and Y coords are very high or low? likely errors

wm.df %>% arrange(desc(Y_coord))
wm.df %>% arrange(desc(X_coord)) #error is X_coord = 4845681.6, not a Y_coord either

summary(wm.df$X_coord)
head(sort(wm.df$X_coord,decreasing = T))
wm.df[which(wm.df$X_coord == max(wm.df$X_coord)),]
wm.df <- wm.df[-which(wm.df$X_coord == max(wm.df$X_coord)),] #remove error 2000-01-22

summary(wm.df$Y_coord)
head(sort(wm.df$Y_coord,decreasing = F)) #first three Y_coord are very low, likely errors
sort(wm.df$Y_coord,decreasing = F)[1:3] # Y_coord errors, three
wm.df[which(wm.df$Y_coord %in% sort(wm.df$Y_coord,decreasing = F)[1:3]),]
wm.df <- wm.df[-which(wm.df$Y_coord %in% sort(wm.df$Y_coord,decreasing = F)[1:3]),] #remove these errors

# Rename for left join with weather data
wm.df <- wm.df %>% 
  mutate(date = as.Date(Date))

# Visualize Time Data ####

# Number of counts in each month and week ####
month.wm <- wm.df %>% count(month)
week.wm <- wm.df %>% count(week)
year.wm <- wm.df %>% group_by(year) %>% count(year)

mthwk.wm <- wm.df %>% group_by(year,week,month) %>%
   count(week,month,year) %>% mutate(month_n = month(month, label = T)) %>% mutate(yweek = week(week))
mthwk.wm #counts by week, but corresponding month and year used


# line plot
ggplot(data = week.wm, aes(x=week,y=n)) +
   geom_line()

# line plot with smoother and mean
ggplot(data = mthwk.wm, aes(x=month,y=n)) +
   geom_boxplot(aes(group = month),alpha=0.9) +
   stat_summary(geom="line",fun.y = "mean",color = "red",size=1.5,alpha=0.8) +
   geom_smooth()

# seasonality assessment
ggplot(mthwk.wm, aes(x=month_n,y=n)) +
   geom_boxplot(aes(group=month_n))

# colour by year
ggplot(mthwk.wm, aes(x=yweek,y=n)) +
   geom_line(aes(colour = as.factor(year)), alpha = 0.6, size = 1.5)

#jitter
ggplot(data = mthwk.wm, aes(x = week, y = n)) +
   geom_jitter(width = 0.4, size = 2, alpha = 0.5)

# yearly trend line plot
ggplot(data = year.wm, aes(x = year, y = n)) +
   geom_line() + geom_smooth(method = "loess", se = TRUE) +
   geom_smooth(method = "lm", se = FALSE, colour = "red")

summary(lm(n ~ year, year.wm))
lm(n ~ year, year.wm)$coef[2]


# Interactive htmlwidgets use ####
week.wm <- as.data.frame(week.wm)
wm.ts <- xts(week.wm$n, order.by=week.wm$week, tz="UTC")
# wm.ts <- cbind(wm.ts, m.mean = rollmeanr(wm.ts,k=12))
names(wm.ts) <- c("Breaks")

month.wm <- as.data.frame(month.wm)
wm.tsm <- xts(month.wm$n, order.by=month.wm$month, tz="UTC")
names(wm.tsm) <- c("Breaks")


str(index(wm.ts))

# add shading? https://stackoverflow.com/questions/30805017/dyshading-r-dygraph
ok_periods <- list(
   list(from = "1990-01-01", to = "1991-01-01"),
   list(from = "1992-01-01", to = "1993-01-01"),
   list(from = "1994-01-01", to = "1995-01-01"),
   list(from = "1996-01-01", to = "1997-01-01"),
   list(from = "1998-01-01", to = "1999-01-01"),
   list(from = "2000-01-01", to = "2001-01-01"),
   list(from = "2002-01-01", to = "2003-01-01"),
   list(from = "2004-01-01", to = "2005-01-01"),
   list(from = "2006-01-01", to = "2007-01-01"),
   list(from = "2008-01-01", to = "2009-01-01"),
   list(from = "2010-01-01", to = "2011-01-01"),
   list(from = "2012-01-01", to = "2013-01-01"),
   list(from = "2014-01-01", to = "2015-01-01"),
   list(from = "2016-01-01", to = "2017-01-01")
)

add_shades <- function(x, periods, ...) {
  for( period in periods ) {
    x <- dyShading(x, from = period$from , to = period$to, ... )
  }
  x
}
# by week
dygraph(wm.ts, main = "City of Toronto Watermain Breaks by Week") %>% 
   dyAxis("y", label = "Watermain Breaks per Week") %>%
   dySeries("Breaks", strokeWidth = 1.75, fillGraph = TRUE, color = "#1B5EA2") %>%
   dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
   dyOptions(includeZero = TRUE, fillAlpha = 0.25) %>%
   dyRangeSelector(dateWindow = c("2012-01-01", "2017-01-01")) %>%
  add_shades(ok_periods, color = "#E3E2E2")

# by month
dygraph(wm.tsm, main = "City of Toronto Watermain Breaks by Month") %>% 
  dyAxis("y", label = "Watermain Breaks per Month") %>%
  dySeries("Breaks", strokeWidth = 1.75, fillGraph = TRUE, color = "#1B5EA2") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  dyOptions(includeZero = TRUE, fillAlpha = 0.25) %>%
  dyRangeSelector(dateWindow = c("2007-01-01", "2017-01-01")) %>%
  add_shades(ok_periods, color = "#E3E2E2")

# Visualize Spatial Data ####
# Plot the spatial data
source('~/GitHub/TO_Watermain/fnc/plot.TO.wm.R')

# plot of only 2016
plot.TO.wm(2016,band=250)

# plot of only 1991
plot.TO.wm(y1=1991,band = 200, file.out = F,
           h = 8.5, w = 11)

# cumulative plot of 2000-2005
plot.TO.wm(2000,2005, face = F, band = 400,
           h = 8.5, w = 11)

# facet plot from 1995 to 1997
plot.TO.wm(1993,1998,ncol=2,face = T, band = 200)

# plot all yaers with 4 columns, 8*15 inch output
plot.TO.wm(1990,2016,face = T,ncol = 4,band = 200,
           file.out = TRUE, h = 14, w = 9)

# Overlay on Map #
names(wm.df)
wm.coord <- wm.df %>% select(X_coord, Y_coord) %>% as.data.frame()
library(rgdal)
coordinates(wm.coord) <-  ~ X_coord + Y_coord
str(wm.coord)
#http://leware.net/geo/utmgoogleapp.htm

wm.coord <- SpatialPoints(wm.coord, proj4string=CRS("+proj=utm + +zone=17T +datum=NAD27"))
wm.latlon <- spTransform(wm.coord, CRS("+proj=longlat +datum=WGS84"))
df.latlon <- as.data.frame(wm.latlon)
sbbox <- make_bbox(lon = df.latlon[,1], lat = df.latlon[,2], f = 0.01)
my_map <- get_map(location = sbbox, maptype = "roadmap", 
                  scale = 2, color="bw", zoom = 10)
ggmap(my_map)
