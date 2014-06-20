library(ggplot2)

## load data
DF1 = read.csv( "data/allcheckins.csv", 
                     header=TRUE, sep=",", nrows=50000,
                     colClasses = c("numeric","numeric","numeric","character",
                                    "numeric","character","character","character")
                    )

## datetime column
DF1$datetime = strptime( strtrim(DF1$time_str,19),  
                              format="%Y-%m-%d %H:%M:%S" 
                             )

DF1$hour=format(DF1$datetime,"%H")
DF1$weekday=format(DF1$datetime,"%w")
DF1$yearday=format(DF1$datetime,"%j")
DF1$hourandday = paste0(DF1$yearday,DF1$hour)
DF1$isweekend = (DF1$weekday>5 | DF1$weekday<1) 



png("plot_total.png",width=1200)
ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
    geom_histogram(binwidth=1800) 
dev.off()


png("plot_categorical.png",width=1000,height=800)
ggplot(DF1, aes(x=datetime, colour=isweekend, fill=isweekend)) + 
    geom_histogram(binwidth=1800) +
    facet_grid(cate_l1~.)
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


