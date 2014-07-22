## load libraries and functions
library(scales)
library(ggplot2)
library(nnet)

## data directory (external directory for input and output)
basedir = "D:\\Experiments\\R\\"


################################################################################
# load checkin data and weather data
# join weather data with checkin data
################################################################################
## load data
DF_checkin = read.csv( paste0(basedir, "data\\userA.csv"), 
                       header=TRUE, sep=",", #nrows=100000,  
                       na.strings = "none",
                       colClasses = c("numeric","numeric","factor",
                                      "character", "numeric","numeric",
                                      "numeric","character","factor",
                                      "factor")
                       )
DF_weather = read.csv( paste0(basedir, "data\\weather.csv"), 
                       header=TRUE, sep=",", na.strings = c("-9999","Unknown"),
                       colClasses = c("numeric","numeric","numeric","character",
                                      "numeric","factor","numeric","numeric",
                                      "numeric","numeric","numeric","numeric",
                                      "numeric","numeric")
                       )
DF_weather$fog=as.logical(DF_weather$fog)
DF_weather$rain=as.logical(DF_weather$rain)
DF_weather$snow=as.logical(DF_weather$snow)
DF_weather$thunder=as.logical(DF_weather$thunder)
DF_weather$tornado=as.logical(DF_weather$tornado)

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
rm(i,weather_id)
DF_checkin_weather = merge(x=DF_checkin, y=DF_weather, 
                           by.x="weather_id", by.y="id", all.X=TRUE)
rm(DF_checkin)
## deal with time 
DF_checkin_weather$datetime = strptime( strtrim(DF_checkin_weather$localtime.x,19), 
                                format="%Y-%m-%d %H:%M:%S")
DF_checkin_weather$hour = as.factor(format(DF_checkin_weather$datetime,"%H"))
DF_checkin_weather$yearday = format(DF_checkin_weather$datetime,"%j")
DF_checkin_weather$weekday = as.factor(format(DF_checkin_weather$datetime,"%w"))
DF_checkin_weather$isweekend = as.factor(ifelse(
    (format(DF_checkin_weather$datetime,"%w")>5 | 
         format(DF_checkin_weather$datetime,"%w")<1),
    "Weekend", "Workday"))
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

################################################################################
# multinominal logistic regression model
tmodel<-multinom(cate_l2~hour+isweekend+fog+snow+rain,
                 data=DF_checkin_weather,maxit = 500)
tsummary = summary(tmodel)
# The multinom package does not include p-value calculation for the regression 
# coefficients, so we calculate p-values using Wald tests (here z-tests).
z = tsummary$coefficients/tsummary$standard.errors
# 2-tailed z test
p = (1 - pnorm(abs(z), 0, 1)) * 2
expcoef = exp(coef(tmodel))
pp = fitted(tmodel)

## ref: http://www.r-bloggers.com/how-to-multinomial-regression-models-in-r/
predictMNL <- function(model, newdata) {
    
    # Only works for neural network models
    if (is.element("nnet",class(model))) {
        # Calculate the individual and cumulative probabilities
        probs <- predict(model,newdata,"probs")
        cum.probs <- t(apply(probs,1,cumsum))
        
        # Draw random values
        vals <- runif(nrow(newdata))
        
        # Join cumulative probabilities and random draws
        tmp <- cbind(cum.probs,vals)
        
        # For each row, get choice index.
        k <- ncol(probs)
        ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
        
        # Return the values
        return(ids)
    }
}

##
newdata=DF_checkin_weather
newdata["cate_l1"]=NULL
y2 <- predictMNL(tmodel,newdata)
y3 <- predictMNL(tmodel,newdata)
comp = data.frame(DF_checkin_weather$cate_l1, 
                  levels(DF_checkin_weather$cate_l1)[y2],
                  levels(DF_checkin_weather$cate_l1)[y3])
comp$correct= ifelse(comp[1]==comp[2],1,0)
comp$correctu= ifelse(comp[1]==comp[2]|comp[1]==comp[3],1,0)

################################################################################
# chi-square correlation test: hour - venue_category
dfmat_hour_cate = as.data.frame.matrix(
    with(DF_checkin_weather, table(hour, cate_l1)))
mat_hour_cate = as.matrix(dfmat_hour_cate)
chi_hour_cate = chisq.test(mat_hour_cate)
# output:
# Pearson's Chi-squared test
# data:  mat_hour_cate_2
# X-squared = 5579.292, df = 207, p-value < 2.2e-16
################################################################################

################################################################################
# chi-square correlation test: hour - venue_category
dfmat_conds_cate = as.data.frame.matrix(
    with(DF_checkin_weather, table(conds, cate_l1)))
dfmat_conds_cate=dfmat_conds_cate[-3,]  # remove "Heavy Snow"
mat_conds_cate = as.matrix(dfmat_conds_cate)
chi_conds_cate = chisq.test(mat_conds_cate)
# output:
# X-squared = 1483.779, df = 90, p-value < 2.2e-16
################################################################################