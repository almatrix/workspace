library(ggplot2)
library(scales)

## data directory
basedir = "D:\\Experiment\\R\\"

## load data
DF1 = read.csv( paste0(basedir, "data\\allcheckins.csv"), 
                     header=TRUE, sep=",", nrows=200000,
                     colClasses = c("numeric","numeric","numeric","character",
                                    "numeric","character","character","character")
                    )

## datetime column
DF1$datetime = strptime( strtrim(DF1$time_str,19),  
                              format="%Y-%m-%d %H:%M:%S" 
                             )

DF1$hour=format(DF1$datetime,"%H")
DF1$yearday=format(DF1$datetime,"%j")
DF1$isweekend = format(DF1$datetime,"%w")>5 | format(DF1$datetime,"%w")<1


gen_df_dt <- function(df) {
    
    tab_dt = table(df$hour,df$yearday)
    tab_dt = tab_dt[,c(2:(ncol(tab_dt)-1))]
    
    vec_date = as.vector(table(df$yearday))
    vec_date = vec_date[c(2:(length(vec_date)-1))]
    
    df_dt = data.frame(
        count_hour = as.vector(tab_dt),
        count_daily = rep(vec_date, each = nrow(tab_dt)),
        datetime = strptime(paste(rep( colnames(tab_dt), each=nrow(tab_dt) ),
                                  rep( rownames(tab_dt), ncol(tab_dt)      ) ), 
                            format = "%j %H")
        
    )
    df_dt$isweekend = format(df_dt$datetime,"%w")>5 | format(df_dt$datetime,"%w")<1
    
    df_dt
}



DF_dt = gen_df_dt(DF1)
png(paste0(basedir,"img\\plot_total.png"),width=1000)
ggplot(DF_dt, aes(x=datetime, y=count_hour/count_daily)) + 
    geom_point(aes(x=datetime, y=count_hour/count_daily, color=isweekend)) +
    geom_line() +
    xlab("") +
    scale_y_continuous(labels  = percent)
dev.off()

# png(paste0(basedir,"img\\plot_total.png"),width=1200)
# ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
#     geom_histogram(binwidth=1800) +
#     xlab("")
# dev.off()
L_splited = split(DF1,DF1$cate_l1)

L_categorical=lapply( L_splited, gen_df_dt)
DF_dt_categorical=data.frame()
temp = lapply( seq_along(L_categorical),
        function(i){
            df_temp = L_categorical[[i]]
            df_temp$cate_l1=names(L_categorical[i])
            DF_dt_categorical <<- rbind(DF_dt_categorical,df_temp)
            })
rm(temp)
# DF_dt_categorical=join_all(L_categorical1)
# DF_dt_categorical = L_categorical[[1]][0,]
# merged = lapply(L_categorical,function(i){
#     DF_dt_categorical <<- merge(DF_dt_categorical,i,all=TRUE)
#     })

png(paste0(basedir,"img\\plot_categorical.png"),width=1000,height=800)
ggplot(DF_dt_categorical, aes(x=datetime, y=count_hour/count_daily)) + 
    geom_point(aes(x=datetime, y=count_hour/count_daily, color=isweekend)) +
    geom_line() +
    facet_grid(cate_l1~.)+
    coord_cartesian(ylim = c(0,0.2)) +
    xlab("") +
    scale_y_continuous(labels  = percent)
# ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
#     geom_histogram(binwidth=1800) +
#     facet_grid(cate_l1~.)
dev.off()


########### average by hour
vec_hour = table(DF1$hour)
DF_hour = data.frame(
    count = as.vector(vec_hour),
    hour = names(vec_hour) )
png(paste0(basedir,"img\\plot_total_hour_mean.png"),width=1000)
ggplot(DF_hour, aes(x=hour,y=count/sum(count))) + 
    geom_bar(stat="identity") +
    xlab("") +
    scale_y_continuous(labels  = percent)
dev.off()

########### categorical average by hour
DF_hour_categorical = data.frame()
L_hour_categorical=lapply( seq_along(L_splited), 
                      function(i){
                          df_temp = table(L_splited[[i]]$hour)
                          newdf = data.frame(
                              count = as.vector(df_temp),
                              hour = names(df_temp),
                              cate_l1 = names(L_splited[i]))
                          newdf$total = sum(newdf$count)
                          DF_hour_categorical <<- rbind(DF_hour_categorical,newdf)
                      })
png(paste0(basedir,"img\\plot_categorical_hour_mean.png"),height=800)
ggplot(DF_hour_categorical, aes(x=hour,y=count/total)) + 
    geom_bar(stat="identity") +
    xlab("") +
    facet_grid(cate_l1~.) +
    coord_cartesian(ylim = c(0,0.1)) +
    scale_y_continuous(labels  = percent)
dev.off()
# weekend_data = DF1[(DF1$weekday>5 | DF1$weekday<1), ]
# 
# table1=table(DF1$hour,DF1$yearday)
# table1=table1[,c(2:(ncol(table1)-1))]
# vec = as.vector(table1) 
# 
# weekendflag=rep(c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,
#                   TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,
#                   TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,
#                   TRUE,TRUE,FALSE,FALSE
#                   ),each=24)
# indexflag=c(1:600)
# 
# plotdataframe=data.frame(indexflag,weekendflag,vec)
# 
# with(plotdataframe, plot(indexflag,vec,type="l", col="blue"))
# with(subset(plotdataframe,weekendflag==TRUE), 
#      points(indexflag,vec, pch=1, col="red"))


########
## difference
DF_dt_categorical_diff = data.frame()
DF_dt_categorical$hour = format(DF_dt_categorical$datetime,"%H")
DF_dt_categorical$diff = rep(0,nrow(DF_dt_categorical))
L_dt_categorical = split(DF_dt_categorical, DF_dt_categorical$cate_l1)
L_hour_categorical = split(DF_hour_categorical,DF_hour_categorical$cate_l1)

temp=lapply(seq_along(L_hour_categorical), function(i){
    df = L_dt_categorical[[i]]
    mean_df = L_hour_categorical[[i]]
    for (j in 1:nrow(df)){
        idx= which(mean_df$hour %in% c(df[j,"hour"]))
        diff = df[j,"count_hour"]/df[j,"count_daily"] - 
            mean_df[idx,"count"]/mean_df[idx,"total"]
        L_dt_categorical[[i]][j,"diff"]<<-diff 
    }   
})
temp = lapply( L_dt_categorical,
               function(i){
                   DF_dt_categorical_diff <<- rbind(DF_dt_categorical_diff,i)
               })
png(paste0(basedir,"img\\plot_categorical_diff.png"),width=1000,height=800)
ggplot(DF_dt_categorical_diff, aes(x=datetime, y=diff)) + 
    #geom_point(aes(x=datetime, y=diff, color=isweekend)) +
    geom_line() +
    facet_grid(cate_l1~.)+
    coord_cartesian(ylim = c(0.01,0.1)) +
    xlab("") +
    scale_y_continuous(labels  = percent)
# ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
#     geom_histogram(binwidth=1800) +
#     facet_grid(cate_l1~.)
dev.off()
