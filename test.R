## load libraries and functions
library(ggplot2)
library(scales)

library(TSA)

source("fun\\GenerateStatsDataFrame.R")


## data directory (external directory for input and output)
basedir = "D:\\Experiments\\R\\"
## load data
DF1 = read.csv( paste0(basedir, "data\\allcheckins.csv"), 
                     header=TRUE, sep=",", nrows=200000,
                     colClasses = c("numeric","numeric","numeric","character",
                                    "numeric","character","character","character")
                    )
## add additional columns
DF1$datetime = strptime( strtrim(DF1$time_str,19),  
                              format="%Y-%m-%d %H:%M:%S" 
                             )
DF1$hour=format(DF1$datetime,"%H")
DF1$yearday=format(DF1$datetime,"%j")
DF1$weekday = ifelse(
    (format(DF1$datetime,"%w")>5 | format(DF1$datetime,"%w")<1),
    "Weekend", "Workday")


########### statistics for the dataset ###########

## phase 1: category and date combined, statistics by hour 
## i.e.: mean checkin counts in each hour
## prepare them in a data frame for plotting
DF_hour = gen_df_by_hour(DF1)
## plot
png(paste0(basedir,"img\\plot_mean_hour.png"),width=1000)
ggplot(DF_hour, aes(x=hour,y=prop)) + 
    geom_bar(stat="identity") +
    xlab("") +
    scale_y_continuous(labels  = percent)
dev.off()


## phase 2: category combined, statistics by hour and weekend/workday 
## i.e.: mean checkin counts in each hour for weekend and for workdays
## prepare them in a data frame for plotting
L_weekday = split(DF1, DF1$weekday)
DF_hour_weekday = data.frame()
temp = lapply(L_weekday, function(i){
    df = gen_df_by_hour(i, weekday=i[1,"weekday"])
    DF_hour_weekday <<- rbind(DF_hour_weekday, df)
})
rm(temp,L_weekday)
## plot
png(paste0(basedir,"img\\plot_mean_hour_weekday.png"),width=1000)
ggplot(DF_hour_weekday, aes(x=hour,y=prop, fill=weekday)) + 
    geom_bar(stat="identity") +
    xlab("") +
    facet_grid(~weekday) +
    # coord_cartesian(ylim = c(0,0.1)) +
    scale_y_continuous(labels  = percent)
dev.off()


## phase 3: statistics by hour and category
## i.e.: mean checkin counts in each hour for each category
## prepare them in a data frame for plotting
L_category = split(DF1, DF1$cate_l1)
DF_hour_category = data.frame()
temp = lapply(L_category, function(i){
    df = gen_df_by_hour(i, category = i[1,"cate_l1"])
    DF_hour_category <<- rbind(DF_hour_category,df)
})
rm(temp)
## plot
png(paste0(basedir,"img\\plot_mean_hour_category.png"),width = 1000)
ggplot(DF_hour_category, aes(x=hour,y=prop)) + 
    geom_bar(stat="identity") +
    xlab("") +
    facet_wrap(~cate_l1, ncol=5, nrow=2) +
    coord_cartesian(ylim = c(0,0.13)) +
    scale_y_continuous(labels  = percent)
dev.off()


## phase 4: category combined, statistics by date and hour
## i.e.: checkin counts in each hour for each day
## prepare them in a data frame for plotting
DF_date_hour = gen_df_by_date_hour(DF1)
## plot
png(paste0(basedir,"img\\plot_date_hour.png"),width=1000)
ggplot(DF_date_hour, aes(x=datetime, y=prop)) + 
    geom_point(aes(x=datetime, y=prop, color=weekday)) +
    geom_line() +
    xlab("") +
    scale_y_continuous(labels  = percent)
dev.off()


## phase 5: statistics by date and hour and by category 
## i.e.: checkin counts in each hour for each day
## prepare them in a data frame for plotting
DF_date_hour_category = data.frame()
temp = lapply(L_category, function(i){
    df = gen_df_by_date_hour(i, category = i[1,"cate_l1"])
    DF_date_hour_category <<- rbind(DF_date_hour_category,df)
})
rm(temp)
## plot
png(paste0(basedir,"img\\plot_date_hour_category.png"),width=1000)
ggplot(DF_date_hour_category, aes(x=datetime, y=prop)) + 
    geom_point(aes(x=datetime, y=prop, color=weekday)) +
    geom_line() +
    facet_wrap(~cate_l1, ncol=2, nrow=5) +
    coord_cartesian(ylim = c(0,0.2)) +
    xlab("") +
    scale_y_continuous(labels  = percent)
# ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
#     geom_histogram(binwidth=1800) +
#     facet_grid(cate_l1~.)
dev.off()


## differences (between DF_hour_category and DF_date_hour_category)
L_mean = split(DF_hour_category, DF_hour_category$cate_l1)
L_date = split(DF_date_hour_category, DF_date_hour_category$cate_l1)
DF_diff = data.frame()
temp = lapply(seq_along(L_mean), function(i){
    mean_cate_i = L_mean[[i]]
    date_cate_i = L_date[[i]]
    
    for (j in 1:nrow(date_cate_i)){
        idx= which(mean_cate_i$hour == date_cate_i[j,"hour"])
        diff = date_cate_i[j,"prop"] - mean_cate_i[idx, "prop"]
        date_cate_i[j,"difference"] = diff
    }

    DF_diff <<- rbind(DF_diff, date_cate_i)
    
})
rm(temp)
## plot
png(paste0(basedir,"img\\plot_diff.png"),width=1000)
ggplot(DF_diff, aes(x=datetime, y=difference)) + 
    geom_point(aes(x=datetime, y=difference, color=weekday)) +
    #geom_line() +
    facet_wrap(~cate_l1, ncol=2, nrow=5) +
    coord_cartesian(ylim = c(-0.1,0.3)) +
    xlab("") +
    scale_y_continuous(labels  = percent)
dev.off()


####### frequency domain
globalfre = spec.pgram(DF_date_hour$prop, plot=FALSE)
png(paste0(basedir,"img\\plot_freq.png"))
plot(globalfre[["freq"]][1:180],globalfre[["spec"]][1:180], 
     type="l", main="Global frequency-spectrum",
     xlab="Frequency", ylab="Spectrum")
dev.off()

L_date_hour_category=split(DF_date_hour_category,DF_date_hour_category$cate_l1)
png(paste0(basedir,"img\\plot_freq_category.png"),width=1000)
par(mfrow=c(2,5))
temp = lapply(seq_along(L_date_hour_category), function(i){
    fre = spec.pgram(L_date_hour_category[[i]]$prop, plot=FALSE)
    plot(fre[["freq"]][1:180], fre[["spec"]][1:180], 
         type="l", main= names(L_date_hour_category[i]),
         xlab="Frequency", ylab="Spectrum")
    })
dev.off()


L_diff=split(DF_diff,DF_diff$cate_l1)
png(paste0(basedir,"img\\plot_diff_freq_category.png"),width=1000)
par(mfrow=c(2,5))
temp = lapply(seq_along(L_diff), function(i){
    fre = spec.pgram(L_diff[[i]]$difference, plot=FALSE)
    plot(fre[["freq"]][1:180], fre[["spec"]][1:180], 
         type="l", main= names(L_diff[i]),
         xlab="Frequency", ylab="Spectrum")
})
dev.off()


################
## load weather data
DF2 = read.csv( paste0(basedir, "data\\weatherdata.csv"), 
                header=TRUE, sep=",", nrows=308,
                colClasses = c("character","numeric","character","numeric",
                               "character","numeric"),
                na.strings = "-9999"
)
DF2$datetime = strptime( DF2$LocalTime,  
                         format="%Y/%m/%d %H:%M" 
)
png(paste0(basedir,"img\\plot_windSp.png"),width=1000)
ggplot(DF2, aes(x=datetime, y=WindSpd.Te)) + 
    geom_point(aes(x=datetime, y=WindSpd.Te)) +
    geom_line() +
    xlab("")
dev.off()


# # png(paste0(basedir,"img\\plot_total.png"),width=1200)
# # ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
# #     geom_histogram(binwidth=1800) +
# #     xlab("")
# # dev.off()
# L_splited = split(DF1,DF1$cate_l1)
# 
# L_categorical=lapply( L_splited, gen_df_by_datetime)
# DF_dt_categorical=data.frame()
# temp = lapply( seq_along(L_categorical),
#         function(i){
#             df_temp = L_categorical[[i]]
#             df_temp$cate_l1=names(L_categorical[i])
#             DF_dt_categorical <<- rbind(DF_dt_categorical,df_temp)
#             })
# rm(temp)
# # DF_dt_categorical=join_all(L_categorical1)
# # DF_dt_categorical = L_categorical[[1]][0,]
# # merged = lapply(L_categorical,function(i){
# #     DF_dt_categorical <<- merge(DF_dt_categorical,i,all=TRUE)
# #     })
# 
# 
# 
# 
# ########### average by hour
# vec_hour = table(DF1$hour)
# DF_hour = data.frame(
#     count = as.vector(vec_hour),
#     hour = names(vec_hour) )
# 
# 
# ########### categorical average by hour
# DF_hour_categorical = data.frame()
# L_hour_categorical=lapply( seq_along(L_splited), 
#                       function(i){
#                           df_temp = table(L_splited[[i]]$hour)
#                           newdf = data.frame(
#                               count = as.vector(df_temp),
#                               hour = names(df_temp),
#                               cate_l1 = names(L_splited[i]))
#                           newdf$total = sum(newdf$count)
#                           DF_hour_categorical <<- rbind(DF_hour_categorical,newdf)
#                       })
# 
# # weekend_data = DF1[(DF1$weekday>5 | DF1$weekday<1), ]
# # 
# # table1=table(DF1$hour,DF1$yearday)
# # table1=table1[,c(2:(ncol(table1)-1))]
# # vec = as.vector(table1) 
# # 
# # weekendflag=rep(c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,
# #                   TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,
# #                   TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,
# #                   TRUE,TRUE,FALSE,FALSE
# #                   ),each=24)
# # indexflag=c(1:600)
# # 
# # plotdataframe=data.frame(indexflag,weekendflag,vec)
# # 
# # with(plotdataframe, plot(indexflag,vec,type="l", col="blue"))
# # with(subset(plotdataframe,weekendflag==TRUE), 
# #      points(indexflag,vec, pch=1, col="red"))
# 
# 
# ########
# ## difference
# DF_dt_categorical_diff = data.frame()
# DF_dt_categorical$hour = format(DF_dt_categorical$datetime,"%H")
# DF_dt_categorical$diff = rep(0,nrow(DF_dt_categorical))
# L_dt_categorical = split(DF_dt_categorical, DF_dt_categorical$cate_l1)
# L_hour_categorical = split(DF_hour_categorical,DF_hour_categorical$cate_l1)
# 
# temp=lapply(seq_along(L_hour_categorical), function(i){
#     df = L_dt_categorical[[i]]
#     mean_df = L_hour_categorical[[i]]
#     for (j in 1:nrow(df)){
#         idx= which(mean_df$hour %in% c(df[j,"hour"]))
#         diff = df[j,"count_hour"]/df[j,"count_daily"] - 
#             mean_df[idx,"count"]/mean_df[idx,"total"]
#         L_dt_categorical[[i]][j,"diff"]<<-diff 
#     }   
# })
# temp = lapply( L_dt_categorical,
#                function(i){
#                    DF_dt_categorical_diff <<- rbind(DF_dt_categorical_diff,i)
#                })
# png(paste0(basedir,"img\\plot_categorical_diff.png"),width=1000,height=800)
# ggplot(DF_dt_categorical_diff, aes(x=datetime, y=diff)) + 
#     #geom_point(aes(x=datetime, y=diff, color=isweekend)) +
#     geom_line() +
#     facet_grid(cate_l1~.)+
#     coord_cartesian(ylim = c(0.01,0.1)) +
#     xlab("") +
#     scale_y_continuous(labels  = percent)
# # ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
# #     geom_histogram(binwidth=1800) +
# #     facet_grid(cate_l1~.)
# dev.off()
