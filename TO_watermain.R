# Mapping Watermain Breaks in City of Toronto ####

# Initialize Session ####
cat("\014")
rm(list=ls())
cat("\014")
Sys.Date()
sessionInfo()

list.of.packages <- c("readr","readxl","ggplot2","plyr","tidyr","dplyr","magrittr",
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

# write MTM X-,Y-coord file
# follow steps for http://webapp.geod.nrcan.gc.ca/geod/tools-outils/ntv2.php#GRIDSELECTOR
# Grid: TO27CSv1, NAD83 1997 (selection for Toronto)
# MTM, NAD27 to geographic coordinates (lat,lon)
out.wm <- as.data.frame(cbind(seq(nrow(wm.df)), #id values (row numbers)
                              wm.df$Y_coord, #northing
                              wm.df$X_coord, #easting
                              rep("ON-10",nrow(wm.df)))) #MTM Zone in Ontario #10
names(out.wm) <- c("STA","utm_n","utm_e","utm_z")
write_csv(out.wm,path = paste0(getwd(),"/result/wm_coord.csv"))

# Read-in lat, lon data
latlon.d <- list.files(paste0(getwd(),"/result/"),pattern = "35564")
latlon <- read_csv(paste0(getwd(),"/result/",latlon.d))
latlon[,c("lon","lat")]

# Bind lat/lon to wm.df
wm.df <- wm.df %>% bind_cols(latlon %>% select(lat,lon))

# Add floor dates by week and month
wm.df$week <- floor_date(wm.df$Date, unit = "week")
wm.df$month <- floor_date(wm.df$Date, unit = "month")

# Save modified data frame ####
#write.table(wm.df,file = "clean_data/TO_watermain_breaks.csv",sep = ",", row.names = FALSE)

# Exploratory Analysis + Cleaning ####
summary(wm.df) #some values for X and Y coords are very high or low? likely errors

?map
purrr::map
wm.df %>% summarise(median = median(year),
                    mean = mean(year),
                    max = max(year),
                    min = min(year),
                    sd = sd(year),
                    n = n())





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
   stat_summary(geom="line",fun.y = "mean",color = "red",size=1.5,alpha=0.7) +
   geom_smooth(method="loess")

# seasonality assessment
ggplot(mthwk.wm, aes(x=month_n,y=n)) +
   geom_boxplot(aes(group=month_n))

# colour by year
ggplot(mthwk.wm, aes(x=yweek,y=n)) +
   geom_line(aes(colour = as.factor(year)), alpha = 0.6, size = 1.5)

library(animation)
aspect.w <- 800
aspect.r <- 1.6
title.size = 20
ani.options(ani.width=aspect.w,ani.height=aspect.w/aspect.r, units="px")

saveGIF( {
  for (i in unique(mth.wm$Year)) {
    g.loop <- ggplot(data=mth.wm, aes(x=month_n,y=n)) +
      geom_boxplot(aes(group=month_n)) +
      stat_summary(data = subset(mth.wm, Year == i), fun.y=median, geom="line",
                   aes(group=1), color = "#222FC8", alpha = 0.6, size = 2) +
      labs(title = paste0("Seasonality of Watermain Breaks in Toronto (",i,")"),
           x = "Month", y = "Number of Breaks per Month") +
      theme(plot.title = element_text(size = title.size, face = "bold"))
    print(g.loop)
  }
}, movie.name = "wm_wm.gif",interval=0.9,nmax=30,2)

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

# time series decomposition
m.ts <- ts(month.wm$n,start=c(1990,1),frequency=12) #create ts object
library(forecast)
autoplot(m.ts, main = "Time Series Watermain Breaks per Month in Toronto") #plot monthly trend
ytrend_m.ts <- ma(m.ts,order = 12, centre = TRUE) #order 12 for yearly trend
autoplot(ytrend_m.ts,main = "Yearly Trend for Watermain Breaks per Month")
# detrended time series
detrend_m.ts <- m.ts - ytrend_m.ts
autoplot(detrend_m.ts, main = "Detrended Watermain Breaks per Month")

grid.arrange(
  autoplot(m.ts, main = "Time Series Watermain Breaks per Month in Toronto"), #plot monthly trend
  autoplot(detrend_m.ts, main = "Detrended Watermain Breaks per Month")
)

# using decompose
autoplot(decompose(m.ts, type = "additive"))
plot(decompose(m.ts, type = "additive"))

# using stl
autoplot(stl(m.ts, s.window = "periodic", robust = TRUE))
plot(stl(m.ts, s.window = "periodic", robust = TRUE))

# detect outliers using median not mean
# http://www.sciencedirect.com/science/article/pii/S0022103113000668




plot(decompose(ts(month.wm[,2],start=c(1990,1),frequency=12),type = "add"))
plot(stl(ts(month.wm[,2],start=c(1990,1),frequency=12),s.window = "periodic",robust = TRUE))

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

# DEPRECATED: Visualize Spatial Data

# using X_coord, Y_coord
g1 <-ggplot(data = wm.df %>% filter(year == i), aes(x = X_coord, y = Y_coord)) +
  geom_point(size = 1, alpha = 0.75, colour = "grey5") +
  labs(title = paste0("Point Density Map of Watermain Breaks"),
       subtitle = paste0("City of Toronto ","(",i,")")) +
  stat_density_2d(aes(fill = ..density.., alpha = ..density..), contour = FALSE, 
                  geom = "raster", n = 200) +
  scale_fill_viridis(guide = "none", option = "inferno") + 
  scale_alpha(breaks=c(0,1),range = c(0, 1)) 

# using lon, lat
g2 <- ggplot(data = wm.df %>% filter(year == i), aes(x = lon, y = lat)) +
  geom_point(size = 1, alpha = 0.75, colour = "grey5") +
  labs(title = paste0("Point Density Map of Watermain Breaks"),
       subtitle = paste0("City of Toronto ","(",i,")")) +
  stat_density_2d(aes(fill = ..density.., alpha = ..density..), contour = FALSE, 
                  geom = "raster", n = 200) +
  scale_fill_viridis(guide = "none", option = "inferno") + 
  scale_alpha(breaks=c(0,1),range = c(0, 1)) 

grid.arrange(g1,g2,nrow=2)





# Plot the spatial data ####
ggplot(data = wm.df, aes(x = X_coord, y = Y_coord, color = year)) +
  geom_point(size = 1.5, alpha = 0.2) +
  scale_color_viridis(option = "B") +
  labs(title = paste0("Map of Watermain Breaks"),
       subtitle = paste0("City of Toronto (1990-2016)")) +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.background = element_rect(fill = "grey95",colour = "grey5"),
        legend.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        plot.title = element_text(size = title.size, face = "bold"),
        plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
        plot.background = element_rect(fill = "grey95"))

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

# Plot cumulative effect of watermain breaks using 'gganimate' package
library(gganimate)
library(animation)
aspect.w <- 800  #mm
aspect.r <- 1.5
title.size <- 18
ani.options(interval = 0.1, ani.width=aspect.w,ani.height=aspect.w/aspect.r, units="px") #set .gif dimensions

# Overlay on Map #
TO.lims <- c(left=-79.73238,bottom=43.59641,right=-79.02154,top=43.74549)
TO.coord <- c(lon = -79.3832, lat = 43.6532)
TO_map1 <- get_map(location = TO.lims, maptype = "terrain",
                   scale = 2, color = "bw", zoom = 11)

ggmap(TO_map1) +
  geom_point(data = wm.df, aes(x = lon, y = lat), 
             size = 1, alpha = 0.25, colour = "grey5") +
  stat_density_2d(data = wm.df, 
                  aes(x = lon, y = lat, fill = ..density.., alpha = ..density..), 
                  geom = "density_2d", bins = 20) +
  scale_fill_viridis(guide = "none", option = "inferno") + 
  scale_alpha(range = c(0, 1))


sbbox <- make_bbox(lon = wm.df$lon, lat = wm.df$lat, f = 1.5)
TO_map2 <- get_map(location = sbbox, maptype = "roadmap",
                   scale = 2, color = "bw", zoom = 10)


pout1 <- ggmap(TO_map2, extent = "panel", darken = 0.8) + 
  geom_point(data = wm.df %>% filter(year %in% seq(1990,2001)), aes(x = lon, y = lat,
                               frame = month, cumulative = TRUE),
             size = 1, alpha = 0.2, color = "yellow") +
  # scale_color_viridis(option = "B") +
  labs(title = paste0("Map of Watermain Breaks: "),
       subtitle = paste0("City of Toronto (1990-2016)")) +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.background = element_rect(fill = "grey95",colour = "grey5"),
        legend.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        plot.title = element_text(size = title.size, face = "bold"),
        plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
        plot.background = element_rect(fill = "grey95")) +
  facet_wrap(~year)

gganimate(pout1, filename = "wm_month_yr_map.gif", title_frame = TRUE)

pbyyr1990 <- ggplot(data = wm.df %>% filter(year == 1990), aes(x = X_coord, y = Y_coord, color = as.factor(month), frame = week, cumulative = TRUE)) +
  geom_point(size = 1.5, alpha = 0.5) +
  scale_color_viridis(option = "B", discrete = TRUE) +
  labs(title = paste0("Map of Watermain Breaks"),
       subtitle = paste0("City of Toronto (1990-2016)")) +
  theme(axis.text = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), panel.background = element_rect(fill = "grey95",colour = "grey5"),
        legend.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        plot.title = element_text(size = title.size, face = "bold"),
        plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
        plot.background = element_rect(fill = "grey95"))

gganimate(pbyyr1, filename = "wm_week_1990.gif", title_frame = TRUE)








# Plot animation of spatial data ####
library(animation)
aspect.w <- 800
aspect.r <- 1.6
title.size <- 20
t <- 2000
ani.options(ani.width=aspect.w,ani.height=aspect.w/aspect.r, units="px")

saveGIF( {
  for (i in unique(wm.df$year)) {
    g.loop <- ggplot(data = wm.df %>% filter(year == i), aes(x = X_coord, y = Y_coord)) +
      geom_point(size = 1, alpha = 0.75, colour = "grey5") +
      labs(title = paste0("Point Density Map of Watermain Breaks"),
           subtitle = paste0("City of Toronto ","(",i,")")) +
      stat_density_2d(aes(fill = ..density.., alpha = ..density..), 
                      geom = "tile", contour = FALSE, n = 150) +
      scale_fill_viridis(guide = "none", option = "inferno") + 
      scale_alpha(breaks=c(0,1),range = c(0, 1)) +
      geom_text(data = wm.df %>% filter(year == i) %>% 
                  summarise(n = paste("n =",sum(year=n()))),
                aes(x=328000,y=4830000,label = n), size = title.size*0.4) +
      theme(axis.text = element_blank(),axis.title = element_blank(),
            axis.ticks = element_blank(), legend.position = "none",
            panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            plot.title = element_text(size = title.size, face = "bold"),
            plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
            plot.background = element_rect(fill = "grey95")) +
      coord_fixed(xlim = c(294164+t,335202-t), ylim = c(4827194+t,4855952-t))
    print(g.loop)
  }
}, movie.name = "wm_sp.gif", interval=0.9, nmax=30,2)


i <- 2016
t <- 2000
ggplot(data = wm.df %>% filter(year == i), aes(x = X_coord, y = Y_coord)) +
  geom_point(size = 1, alpha = 0.75, colour = "grey5") +
  labs(title = paste0("Point Density Map of Watermain Breaks"),
       subtitle = paste0("City of Toronto ","(",i,")")) +
  stat_density_2d(aes(fill = ..density.., alpha = ..density..), contour = FALSE, 
                  geom = "raster", n = 200) +
  scale_fill_viridis(guide = "none", option = "inferno") + 
  scale_alpha(breaks=c(0,1),range = c(0, 1)) +
  geom_text(data = wm.df %>% filter(year == i) %>% 
              summarise(n = paste("n =",sum(year=n()))),
            aes(x=328000,y=4830000,label = n), size = title.size*0.4) +
  theme(axis.text = element_blank(),axis.title = element_blank(),
        axis.ticks = element_blank(), legend.position = "none",
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        plot.title = element_text(size = title.size, face = "bold"),
        plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
        plot.background = element_rect(fill = "grey95"))
 
# Overlay on Map #
TO.lims <- c(left=-79.73238,bottom=43.59641,right=-79.02154,top=43.74549)
TO.coord <- c(lon = -79.3832, lat = 43.6532)
TO_map1 <- get_map(location = TO.lims, maptype = "terrain",
              scale = 2, color = "bw", zoom = 11)

ggmap(TO_map1) +
  geom_point(data = wm.df, aes(x = lon, y = lat), 
             size = 1, alpha = 0.25, colour = "grey5") +
  stat_density_2d(data = wm.df, 
                  aes(x = lon, y = lat, fill = ..density.., alpha = ..density..), 
                  geom = "density_2d", bins = 20) +
  scale_fill_viridis(guide = "none", option = "inferno") + 
  scale_alpha(range = c(0, 1))


sbbox <- make_bbox(lon = wm.df$lon, lat = wm.df$lat, f = 0.001)
TO_map2 <- get_map(location = sbbox, maptype = "roadmap",
        scale = 2, color = "bw", zoom = 10)

ggmap(TO_map2, extent = "panel") +
  geom_point(data = wm.df, aes(x = lon, y = lat), shape = 3,
             size = 1, alpha = 0.1, colour = "grey5") +
  stat_density_2d(data = wm.df, 
                  aes(x = lon, y = lat, alpha = ..level.., fill = ..level..), 
                  geom = "polygon", bins = 200) +
  scale_fill_viridis(guide = "none", option = "inferno", begin = 0, end = 1, direction = 1) + 
  scale_alpha(range = c(0, 0.1), guide = FALSE) + 
  scale_y_continuous(limits = c(43.57,43.815)) +
  scale_x_continuous(limits = c(-79.65,-79.1)) +
  theme(axis.text = element_blank(),axis.title = element_blank(),
        axis.ticks = element_blank(), legend.position = "none",
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        plot.title = element_text(size = title.size, face = "bold"),
        plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
        plot.background = element_rect(fill = "grey95")) +
  labs(title = paste0("Point Density Map of Watermain Breaks"),
       subtitle = paste0("City of Toronto "))
# year
saveGIF( {
  for (i in unique(wm.df$year)) {
    g.loop <- ggmap(TO_map2, extent = "panel") +
      geom_point(data = wm.df %>% filter(year == i), aes(x = lon, y = lat), shape = 3,
                 size = 1, alpha = 0.8, colour = "grey5") +
      stat_density_2d(data = wm.df %>% filter(year == i), 
                      aes(x = lon, y = lat, alpha = ..level.., fill = ..level..), 
                      geom = "polygon", bins = 100) +
      scale_fill_viridis(guide = "none", option = "inferno") + 
      scale_alpha(range = c(0, 0.05), guide = FALSE) + 
      scale_y_continuous(limits = c(43.57,43.815)) +
      scale_x_continuous(limits = c(-79.65,-79.1)) +
      theme(axis.text = element_blank(),axis.title = element_blank(),
            axis.ticks = element_blank(), legend.position = "none",
            panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            plot.title = element_text(size = title.size, face = "bold"),
            plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
            plot.background = element_rect(fill = "grey95")) +
      labs(title = paste0("Point Density Map of Watermain Breaks"),
           subtitle = paste0("City of Toronto ","(",i,")")) +
      geom_text(data = wm.df %>% filter(year == i) %>% 
                  summarise(n = paste("n =",sum(year=n()))),
                aes(x=-79.17,y=43.62,label = n), size = title.size*0.3) 
    print(g.loop)
  }
}, movie.name = "wm_map.gif", interval=0.9, nmax=30,2)


saveGIF( {
  for (i in unique(wm.df$month)) {
    g.loop <- ggmap(TO_map2, extent = "panel") +
      geom_point(data = wm.df %>% filter(month == i), aes(x = lon, y = lat), shape = 3,
                 size = 1, alpha = 0.8, colour = "grey5") +
      stat_density_2d(data = wm.df %>% filter(month == i), 
                      aes(x = lon, y = lat, alpha = ..level.., fill = ..level..), 
                      geom = "polygon", bins = 100) +
      scale_fill_viridis(guide = "none", option = "inferno") + 
      scale_alpha(range = c(0, 0.05), guide = FALSE) + 
      scale_y_continuous(limits = c(43.57,43.815)) +
      scale_x_continuous(limits = c(-79.65,-79.1)) +
      theme(axis.text = element_blank(),axis.title = element_blank(),
            axis.ticks = element_blank(), legend.position = "none",
            panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            plot.title = element_text(size = title.size, face = "bold"),
            plot.subtitle = element_text(size = title.size*0.6, face = "plain"),
            plot.background = element_rect(fill = "grey95")) +
      labs(title = paste0("Point Density Map of Watermain Breaks"),
           subtitle = paste0("City of Toronto ","(",i,")")) +
      geom_text(data = wm.df %>% filter(month == i) %>% 
                  summarise(n = paste("n =",sum(month=n()))),
                aes(x=-79.17,y=43.62,label = n), size = title.size*0.3) 
    print(g.loop)
  }
}, movie.name = "wm_map_mth.gif", interval=0.9, nmax=30,2)


