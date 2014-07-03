## exploratory analysis
## test: how many foursquare checkins are available via twitter?
## conclusion: ~10%


basedir = "D:\\Experiments\\R\\"

DFA = read.csv( paste0(basedir, "data\\venue_stats_sample_A.csv"), 
                header=TRUE, sep=",",
                colClasses = c("numeric","numeric","numeric","numeric")
)
DFB = read.csv( paste0(basedir, "data\\venue_stats_sample_B.csv"), 
                header=TRUE, sep=",",
                colClasses = c("numeric","numeric","numeric","numeric")
)
DFC = read.csv( paste0(basedir, "data\\venue_stats_sample_C.csv"), 
                header=TRUE, sep=",",
                colClasses = c("numeric","numeric","numeric","numeric")
)

## keep only the first and the last record for each single venue
stats_groups = function(df){
    list = split(df, df$row)
    df_ht = data.frame()
    temp = lapply(list, function(i){
        head1= head(i,1)
        tail1 = tail(i,1)
        diff_total = max(i$checkin_co)-min(i$checkin_co)
        diff_time = abs(tail1$timestamps-head1$timestamps)
        prop = head1$count/diff_total*100
        
        if(diff_time > 30*86400 & prop< 100){
            row = c(head1$row, head1$count, 
                    head1$timestamps, tail1$timestamps, 
                    diff_time/86400,
                    diff_total, prop)
            df_ht <<- rbind(df_ht, row)
        }
    })
    colnames(df_ht) <- c("venue", "inc_tweet", "time1", "time2",
                         "diff_time", "inc_total", "prop")
    
    df_ht
}

stats_GA=stats_groups(DFA)
stats_GB=stats_groups(DFB)
stats_GC=stats_groups(DFC)


png(paste0(basedir,"img\\boxplot_groupA.png"))
boxplot(stats_GA$prop,
        stats_GB$prop,
        stats_GC$prop)
dev.off()

png(paste0(basedir,"img\\hist_groupA.png"), width=800, height=350)
par(mfrow=c(1,3))
hist(stats_GA$prop)
hist(stats_GB$prop)
hist(stats_GC$prop)
dev.off()



