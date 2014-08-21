## FUNCTION 
## description:     generat dataframe describing checkin counts aggregated by hour
## input:           df: a dataframe
## optional input:  weekday: character describing weekday of the dataframe (if only)
## optional input:  category: character describing category of the dataframe (if only)
## output:          a new dataframe describing the statistics aggregated by hour
stats_checkin_by_hour <- function(df, weekday=NA, category=NA){
    
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
## description:     generate dataframe describing checkin counts by aggregated by date and hour
## input:           df: a dataframe
## optional input:  category: character describing category of the dataframe (if only)
## output:          a new dataframe describing the statistics aggregated by hour
stats_by_date_hour <- function(df, category=NA) {

    
    tab_date_hour = table(df$hour,df$yearday)
    vec_date = as.vector(table(df$yearday))
    
    ## make sure that the table is all complete
    hourlist = rownames(tab_date_hour)
    daylist = colnames(tab_date_hour)
    if(length(hourlist)<24){
        ## filling the row
        for (i in 1:24){
            hourname = ifelse( i<=10,  paste0("0", i-1),  as.character(i-1) )
            if( rownames(tab_date_hour)[i]!= hourname | is.na(rownames(tab_date_hour)[i]) ) {
                tab_date_hour = insertrow(tab_date_hour,i,name=hourname)
            }
        }
    } 
    if(length(daylist)< (as.numeric(max(daylist))-as.numeric(min(daylist))+1) ){
        ## filling the column
        for (i in 1:(as.numeric(max(daylist))-as.numeric(min(daylist))+1)){
            
            datename = i + as.numeric(min(daylist)) - 1
            if(datename<10) datename=paste0("00",datename)
            else if(datename>=10 & datename<100) datename=paste0("0",datename)
            else datename=as.character(datename)
            if( colnames(tab_date_hour)[i]!= datename ) {
                tab_date_hour = insertcol(tab_date_hour,i,name=datename)
                 vec_date = c(vec_date[1:(i-1)], 0, vec_date[i:length(vec_date)])
#                  names(vec_date)= c(names(vec_date)[1:(i-1)],datename,
#                                     names(vec_date)[i:length(vec_date)])
            }
        }
    }
#     
#     if(ncol(tab_date_hour)>=3){
#         tab_date_hour = tab_date_hour[,c(2:(ncol(tab_date_hour)-1))]  # make sure each day have 24h data
#         vec_date = vec_date[c(2:(length(vec_date)-1))] # the same date range
#     }
        
    df_date_hour = data.frame(
        count_hour = as.vector(tab_date_hour),   # how many checkins in this hour of this day
        count_daily = rep(vec_date, each = nrow(tab_date_hour)), # total checkins in this day
        hour = rep( rownames(tab_date_hour), ncol(tab_date_hour) ),
        #prop = count_hour/count_daily,
        datetime = strptime(paste(rep( colnames(tab_date_hour), each=nrow(tab_date_hour) ),
                                  rep( rownames(tab_date_hour), ncol(tab_date_hour)      ) ), 
                            format = "%j %H")
        
        
    )
    df_date_hour$prop = df_date_hour$count_hour/(df_date_hour$count_daily + 1E-6)
    df_date_hour$weekday = ifelse(
        (format(df_date_hour$datetime,"%w")>5 | format(df_date_hour$datetime,"%w")<1),
        "Weekend", "Workday")
    
    if(!is.na(category)){
        df_date_hour$cate_l1 = category
    }
    
    df_date_hour
}



