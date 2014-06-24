## FUNCTION 
## description:     generate statistical dataframe aggregated by hour
## input:           df: a dataframe
## optional input:  weekday: character describing weekday of the dataframe (if only)
## optional input:  category: character describing category of the dataframe (if only)
## output:          a new dataframe describing the statistics aggregated by hour
gen_df_by_hour <- function(df, weekday=NA, category=NA){
    
    vec_hour = table(df$hour)
    
    df_h = data.frame(
        count = as.vector(vec_hour),
        hour = names(vec_hour)
        )
    
    df_h$prop = df_h$count/sum(df_h$count)
    
    if(!is.na(weekday)){
        df_h$weekday = weekday
    }


    if(!is.na(category)){
        df_h$cate_l1 = category
    }

    df_h
}



## FUNCTION 
## description:     generate statistical dataframe by aggregated by date and hour
## input:           df: a dataframe
## optional input:  category: character describing category of the dataframe (if only)
## output:          a new dataframe describing the statistics aggregated by hour
gen_df_by_date_hour <- function(df, category=NA) {
    
    tab_date_hour = table(df$hour,df$yearday)
    tab_date_hour = tab_date_hour[,c(2:(ncol(tab_date_hour)-1))]  # make sure each day have 24h data
    

    
    vec_date = as.vector(table(df$yearday))
    vec_date = vec_date[c(2:(length(vec_date)-1))] # the same date range
    
    df_date_hour = data.frame(
        count_hour = as.vector(tab_date_hour),   # how many checkins in this hour of this day
        count_daily = rep(vec_date, each = nrow(tab_date_hour)), # total checkins in this day
        hour = rep( rownames(tab_date_hour), ncol(tab_date_hour) ),
        #prop = count_hour/count_daily,
        datetime = strptime(paste(rep( colnames(tab_date_hour), each=nrow(tab_date_hour) ),
                                  rep( rownames(tab_date_hour), ncol(tab_date_hour)      ) ), 
                            format = "%j %H")
        
        
    )
    df_date_hour$prop = df_date_hour$count_hour/df_date_hour$count_daily
    df_date_hour$weekday = ifelse(
        (format(df_date_hour$datetime,"%w")>5 | format(df_date_hour$datetime,"%w")<1),
        "Weekend", "Workday")
    
    if(!is.na(category)){
        df_date_hour$cate_l1 = category
    }
    
    df_date_hour
}
