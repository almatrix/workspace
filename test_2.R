## load libraries and functions
library(scales)
library(ggplot2)
library(nnet)

source("fun\\mathmatrixcal.R")
source("fun\\regression_single_user.R")
## data directory (external directory for input and output)
basedir = "D:\\Experiments\\R\\"


################################################################################
# load checkin data and weather data
# join weather data with checkin data
################################################################################
## load data
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

# DF_userset = read.csv( paste0(basedir, "data\\30users.csv"), 
#                      header=TRUE, sep=",", nrows=100000,  
#                      na.strings = "none",
#                      colClasses = c("numeric","numeric","factor",
#                                     "factor", "numeric","numeric",
#                                     "numeric","character","factor",
#                                     "factor")
# )
DF_userset = read.csv( paste0(basedir, "data\\random20user.csv"), 
                      header=TRUE, sep=",", nrows=100000,  
                      na.strings = "none",
                      colClasses = c("numeric","numeric","factor",
                                     "factor", "numeric","numeric",
                                     "numeric","character","factor",
                                     "factor")
)
# DF_checkin = read.csv( paste0(basedir, "data\\userA.csv"), 
#                        header=TRUE, sep=",", #nrows=100000,  
#                        na.strings = "none",
#                        colClasses = c("numeric","numeric","factor",
#                                       "factor", "numeric","numeric",
#                                       "numeric","character","factor",
#                                       "factor")
#                        )

list_df = split(DF_userset,DF_userset$user_id)
cate_l1_levels = levels(DF_userset$cate_l1)
i=0
result = lapply(list_df,function(DF_checkin){
    i<<- i+1
    print(paste0("*************",i,"*************"))
    testforsingleuser(DF_checkin)  
    
})


# show accuracy 
accurates=sapply(result, function(i){
    c(i$corr,i$records)
})
accurates=t(accurates)
accurates= as.data.frame(accurates[order(accurates[,6]),])  #order
#accurates$V6=NULL
accurates$user= 1:10
colnames(accurates)[1:5]=c("combind","temporal","weather","sequence","spatial")
accurates_complete=accurates[complete.cases(accurates),]
# plot
plot(accurates$user,accurates$combind,type="l", col="green")
lines(accurates$user,accurates$temporal,col="red")
lines(accurates$user,accurates$weather,col="orange")
lines(accurates$user,accurates$sequence,col="blue")
lines(accurates$user,accurates$spatial,col="yellow")

curve(x^2,-1,1, add=TRUE)
curve(x^3,-1,1, add=TRUE)

ggplot(data=accurates, aes(x=user)) + 
    #geom_point
    geom_point(aes(y=combind)) + 
    geom_point(aes(y=temporal)) + 
    geom_point(aes(y=weather)) + 
    geom_point(aes(y=sequence)) + 
    geom_point(aes(y=spatial)) 
ggplot(DF_hour, aes(x=hour,y=prop)) + 
    geom_bar(stat="identity") +
    xlab("") +
    scale_y_continuous(labels  = percent)















## join checkin data with weather data based on timestamps 
DF_checkin_weather = joindfsbytime(DF_checkin, DF_weather)
rm(DF_checkin, DF_weather)
cate_l1_levels = levels(DF_checkin_weather$cate_l1)

## deal with time 
DF_checkin_weather$hour = as.factor(format(DF_checkin_weather$datetime,"%H"))
DF_checkin_weather$yearday = format(DF_checkin_weather$datetime,"%j")
DF_checkin_weather$weekday = format(DF_checkin_weather$datetime,"%w")
DF_checkin_weather$isweekend = as.factor(ifelse(
    ( DF_checkin_weather$weekday>5 | DF_checkin_weather$weekday<1),
    "Weekend", "Workday"))

## add record for last checkin
DF_checkin_weather = copylastcheckinrec(DF_checkin_weather)

## temporally weighted sequences
list_cate = split(DF_checkin_weather,DF_checkin_weather$last_cate_l1)
temp = lapply(seq_along(list_cate), function(i){
    name = names(list_cate[i])
    df = list_cate[[i]]
    expsum = sum(exp(-2*df$time_diff/60))
    ls = split(df, df$cate_l1)
    lapply(seq_along(ls), function(j){
        innername = names(ls[j])
        innerdf = ls[[j]]
        innerexpsum = sum(exp(-2*innerdf$time_diff/60))
        DF_checkin_weather[which(DF_checkin_weather$last_cate_l1==name),
                           paste0("t.",innername)] <<- innerexpsum/expsum
    })
})

## geographically weighted
df_venue = unique(data.frame(id=DF_checkin_weather$venue_id,
                             lat=DF_checkin_weather$lat.x,
                             lon=DF_checkin_weather$lon.x,
                             cate_l1=DF_checkin_weather$cate_l1))
df_distsqr=getdistmatrix(df_venue)
dims = nrow(df_venue)
for(i in 1:dims){
    venue_id = df_venue[i,"id"]
    expsum = sum(exp(-df_distsqr[i,]))
    for(j in 2:10){ # the first level is the baseline
        venue_with_cate_j = which(df_venue$cate_l1==cate_l1_levels[j])
        subexpsum = sum(exp(-df_distsqr[i,venue_with_cate_j]))
        DF_checkin_weather[which(DF_checkin_weather$venue_id==venue_id),
                           paste0("s.",j)] = subexpsum/expsum
    } 
}
colnames(DF_checkin_weather)[26:44] <- c("t0", "t1", "t2", "t3", "t4", "t5", 
                                         "t6", "t7", "t8", "t9","s1", 
                                         "s2", "s3", "s4", "s5", "s6", "s7", 
                                         "s8", "s9")

# 
# ################################################################################
# # analysis with DF_checkin_weather
# ################################################################################
# 
# ################################################################################
# # multinominal logistic regression model
# tmodel<-multinom(cate_l1~hour+isweekend+fog+snow+rain,
#                  data=DF_checkin_weather,maxit = 500)
# tsummary = summary(tmodel)
# # The multinom package does not include p-value calculation for the regression 
# # coefficients, so we calculate p-values using Wald tests (here z-tests).
# z = tsummary$coefficients/tsummary$standard.errors
# # 2-tailed z test
# p = (1 - pnorm(abs(z), 0, 1)) * 2
# expcoef = exp(coef(tmodel))
# pp = fitted(tmodel)

################################################################################
# multinomial logistic regression model - 2
tmodel1<-multinom(cate_l1~ hour+isweekend+fog+snow+rain+t1+t2+t3+t4+t5+t6+t7+t8+t9+s1+s2+s3+s4+s5+s6+s7+s8+s9, 
                 data=DF_checkin_weather,maxit = 1000)
tsummary1 = summary(tmodel1)
z1 = tsummary1$coefficients/tsummary1$standard.errors
p1 = (1 - pnorm(abs(z1), 0, 1)) * 2
y1=predict(tmodel1)

tmodel2<-multinom(cate_l1~ hour+isweekend, 
                  data=DF_checkin_weather,maxit = 1000)
tsummary2 = summary(tmodel2)
z2 = tsummary1$coefficients/tsummary1$standard.errors
p2 = (1 - pnorm(abs(z1), 0, 1)) * 2
y2=predict(tmodel2)

tmodel3<-multinom(cate_l1~ fog+snow+rain, 
                  data=DF_checkin_weather,maxit = 1000)
tsummary3 = summary(tmodel3)
z3 = tsummary1$coefficients/tsummary1$standard.errors
p3 = (1 - pnorm(abs(z1), 0, 1)) * 2
y3=predict(tmodel3)

tmodel4<-multinom(cate_l1~ t1+t2+t3+t4+t5+t6+t7+t8+t9, 
                  data=DF_checkin_weather,maxit = 1000)
tsummary4 = summary(tmodel4)
z4 = tsummary1$coefficients/tsummary1$standard.errors
p4 = (1 - pnorm(abs(z1), 0, 1)) * 2
y4=predict(tmodel4)

tmodel5<-multinom(cate_l1~ s1+s2+s3+s4+s5+s6+s7+s8+s9, 
                  data=DF_checkin_weather,maxit = 1000)
tsummary5 = summary(tmodel5)
z5 = tsummary1$coefficients/tsummary1$standard.errors
p5 = (1 - pnorm(abs(z1), 0, 1)) * 2
y5=predict(tmodel5)

comp = data.frame(DF_checkin_weather$cate_l1, 
                  y1,y2,y3,y4,y5)
comp$corr1=ifelse(comp[1]==comp[2],1,0)
comp$corr2=ifelse(comp[1]==comp[3],1,0)
comp$corr3=ifelse(comp[1]==comp[4],1,0)
comp$corr4=ifelse(comp[1]==comp[5],1,0)
comp$corr5=ifelse(comp[1]==comp[6],1,0)

result = list(
              "p"=list(p1,p2,p3,p4,p5),
              "corr"=c(sum(comp$corr1),sum(comp$corr2),sum(comp$corr3),
                       sum(comp$corr4),sum(comp$corr5))/nrow(comp),
              "records"=nrow(comp),
              "model"=list(tmodel1,tmodel2,tmodel3,tmodel4,tmodel5),
              "summary"=list(tsummary1,tsummary2,tsummary3,tsummary4,tsummary5)
              )

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
                  levels(DF_checkin_weather$cate_l1)[y3],y4)
comp$correct= ifelse(comp[1]==comp[2],1,0)
comp$correctu= ifelse(comp[1]==comp[2]|comp[1]==comp[3],1,0)
comp$correcto= ifelse(comp[1]==comp[4],1,0)

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