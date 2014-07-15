## load libraries and functions
library(scales)
library(ggplot2)

library(TSA)

library(DBI)
library(RPostgreSQL)

library(nnet)

source("fun\\TableOperations.R")
source("fun\\GenerateStatsDataFrame.R")
source("fun\\DBconn.R")

## data directory (external directory for input and output)
basedir = "D:\\Experiments\\R\\"


################################################################################
# load checkin data and weather data
# join weather data with checkin data
################################################################################
## load data
DF_checkin = read.csv( paste0(basedir, "data\\allcheckins.csv"), 
                       header=TRUE, sep=",", nrows=400,  
                       colClasses = c("character","numeric","character",
                                      "character", "numeric","numeric",
                                      "numeric","character","character",
                                      "character")
                       )
DF_weather = read.csv( paste0(basedir, "data\\weather.csv"), 
                       header=TRUE, sep=",", 
                       colClasses = c("numeric","numeric","numeric","character",
                                      "numeric","character","numeric","numeric",
                                      "numeric")
                       )
## deal with time 
DF_checkin$datetime = strptime( strtrim(DF_checkin$localtime,19), 
                                format="%Y-%m-%d %H:%M:%S")
DF_checkin$hour = as.numeric(format(DF_checkin$datetime,"%H"))
DF_checkin$yearday = format(DF_checkin$datetime,"%j")
DF_checkin$isweekend = ifelse(
    (format(DF_checkin$datetime,"%w")>5 | format(DF_checkin$datetime,"%w")<1),
    "Weekend", "Workday")
## the influence time of each weather record
obs_time = DF_weather$timestamps
nxt_obs_time = c(obs_time[-1],(tail(obs_time,1)+3600))
lst_obs_time = c((head(obs_time,1)-3600),obs_time[1:(length(obs_time)-1)])
DF_weather$influ_ts = (obs_time + lst_obs_time )/2
DF_weather$influ_te = (obs_time + nxt_obs_time )/2
rm(obs_time, nxt_obs_time, lst_obs_time)
## join checkin data with weather data based on timestamps
DF_checkin$weather_id=0
for(i in 1:nrow(DF_checkin)){
    weather_id = DF_weather[
        which(DF_checkin[i, "timestamps"]>=DF_weather$influ_ts &
                  DF_checkin[i, "timestamps"]<DF_weather$influ_te),
        "id"]
    DF_checkin[i,"weather_id"] = weather_id
} 
DF_checkin_weather = merge(x=DF_checkin, y=DF_weather, 
                           by.x="weather_id", by.y="id", all.X=TRUE)
# remove the unnecessary columns
DF_checkin_weather$weather_id = NULL
DF_checkin_weather$localtime.x = NULL
DF_checkin_weather$lat.y = NULL
DF_checkin_weather$lon.y = NULL
DF_checkin_weather$localtime.y = NULL
DF_checkin_weather$timestamps.y = NULL
DF_checkin_weather$influ_ts = NULL
DF_checkin_weather$influ_te = NULL


################################################################################
# analysis with DF_checkin_weather
################################################################################
test = DF_checkin_weather
