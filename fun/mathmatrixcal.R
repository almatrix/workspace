getdistmatrix =  function(df_venue,xattr="lon",yattr="lat"){
    dims = nrow(df_venue)
    df_distsqr = data.frame(matrix(0,ncol = dims, nrow = dims))
    for(i in 2:dims){
        for(j in 1:(i-1)){
            df_distsqr[i,j]= (df_venue[i,xattr]-df_venue[j,xattr])^2 + 
                (df_venue[i,yattr]-df_venue[j,yattr])^2
        }
    }
    df_distsqr+t(df_distsqr)
}

setweatherinfluence = function(df_weather){
    obs_time = df_weather$timestamps
    len = length(obs_time)
    #nxt_obs_time = c(obs_time[-1],(tail(obs_time,1)+3600))
    #lst_obs_time = c((head(obs_time,1)-3600),obs_time[1:(length(obs_time)-1)])
    nxt_obs_time = c(obs_time[2:len], (obs_time[len]+3600))
    lst_obs_time = c((obs_time[1]-3600), obs_time[1:len-1])
    df_weather$influ_ts = (obs_time + lst_obs_time )/2
    df_weather$influ_te = (obs_time + nxt_obs_time )/2
    df_weather
}

copylastcheckinrec = function(df){
    
    checkinrec = df$cate_l1
    len = length(checkinrec)    
    #df$last_cate_l1 = cate_l1_levels[c(checkinrec[1], checkinrec[1:len-1])]
    #df$last_cate_l1 = c(checkinrec[1], checkinrec[1:len-1])
    df$last_cate_l1=checkinrec
    df$last_cate_l1[2:len]=checkinrec[1:len-1]
    
    timerec = df$timestamps.x
    lst_time = c(timerec[1], timerec[1:len-1])
    df$time_diff= timerec - lst_time
    
    df
}

## join checkin data with weather data based on timestamps and weather influence
joindfsbytime =  function(df_base,df_ref){
    ## the influence time of each weather record
    df_ref = setweatherinfluence(df_ref)
    for(i in 1:nrow(df_base)){
        index = which( df_base[i, "timestamps"]>=df_ref$influ_ts &
                            df_base[i, "timestamps"]< df_ref$influ_te )
        df_base[i,"weather_id"] = df_ref[index, "id"]
    } 
    df_res = merge(x=df_base, y=df_ref, 
                               by.x="weather_id", by.y="id", all.X=TRUE)
    
    # remove the unnecessary columns
    df_res$weather_id = NULL
    df_res$id = NULL
    df_res$localtime.x = NULL
    df_res$localtime.y = NULL
    df_res$lat.y = NULL
    df_res$lon.y = NULL
    df_res$timestamps.y = NULL
    df_res$influ_ts = NULL
    df_res$influ_te = NULL
    
    df_res
}

### begin copying script here
likelihood.test = function(x) {
    nrows = dim(x)[1]                      # no. of rows in contingency table
    ncols = dim(x)[2]                      # no. of cols in contingency table
    chi.out = chisq.test(x,correct=F)      # do a Pearson chi square test
    table = chi.out[[6]]                   # get the OFs
    ratios = chi.out[[6]]/chi.out[[7]]     # calculate OF/EF ratios
    sum = 0                                # storage for the test statistic
    for (i in 1:nrows) {
        for (j in 1:ncols) {
            sum = sum + table[i,j]*log(ratios[i,j])
        }
    }
    sum = 2 * sum                          # the likelihood ratio chi square
    df = chi.out[[2]]                      # degrees of freedom
    p = 1 - pchisq(sum,df)                 # p-value
    out = c(sum, df, p, chi.out[[1]])      # the output vector
    names(out) = c("LR-chisq","df","p-value","Pears-chisq")
    round(out,4)                           # done!
}
### end copying script here