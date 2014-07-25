testforsingleuser = function(DF_checkin){
    
    
    DF_checkin$datetime = strptime( strtrim(DF_checkin$localtime,19), 
                                    format="%Y-%m-%d %H:%M:%S")
    ## join checkin data with weather data based on timestamps 
    DF_checkin_weather = joindfsbytime(DF_checkin, DF_weather)
    #rm(DF_checkin, DF_weather)
    
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
    #list_cate = split(DF_checkin_weather,DF_checkin_weather$last_cate_l1)
    list_cate = split(DF_checkin_weather,DF_checkin_weather$last_cate_l1)
    temp = lapply(seq_along(list_cate), function(i){
        name = names(list_cate[i])
        df = list_cate[[i]]
        expsum = sum(exp(-2*df$time_diff/60))
        ls = split(df, df$cate_l1)
        lapply(seq_along(ls), function(j){
            innername = names(ls[j])
            innerdf = ls[[j]]
            
            if(nrow(innerdf)>0){
                innerexpsum = sum(exp(-2*innerdf$time_diff/60))
                DF_checkin_weather[which(DF_checkin_weather$last_cate_l1==name),
                                   paste0("t.",innername)] <<- innerexpsum/expsum
            }else{
                DF_checkin_weather[which(DF_checkin_weather$last_cate_l1==name),
                                   paste0("t.",innername)] <<- 0
              
            }
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
            if(length(venue_with_cate_j)>0){
                subexpsum = sum(exp(-df_distsqr[i,venue_with_cate_j]))
                DF_checkin_weather[which(DF_checkin_weather$venue_id==venue_id),
                               paste0("s.",j)] = subexpsum/expsum
            }else {
                DF_checkin_weather[which(DF_checkin_weather$venue_id==venue_id),
                                   paste0("s.",j)] = 0
            }
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
    newdata = DF_checkin_weather
    newdata[,"cate_l1"]=NULL
    
    tmodel1<-multinom(cate_l1~ hour+isweekend+fog+snow+rain+t1+t2+t3+t4+t5+t6+t7+t8+t9+s1+s2+s3+s4+s5+s6+s7+s8+s9, 
                      data=DF_checkin_weather,maxit = 1000)
    tsummary1 = summary(tmodel1)
    z1 = tsummary1$coefficients/tsummary1$standard.errors
    p1 = (1 - pnorm(abs(z1), 0, 1)) * 2
    y1=predict(tmodel1,newdata)
    
    tmodel2<-multinom(cate_l1~ hour+isweekend, 
                      data=DF_checkin_weather,maxit = 1000)
    tsummary2 = summary(tmodel2)
    z2 = tsummary2$coefficients/tsummary2$standard.errors
    p2 = (1 - pnorm(abs(z2), 0, 1)) * 2
    y2=predict(tmodel2,newdata)
    
    tmodel3<-multinom(cate_l1~ fog+snow+rain, 
                      data=DF_checkin_weather,maxit = 1000)
    tsummary3 = summary(tmodel3)
    z3 = tsummary3$coefficients/tsummary3$standard.errors
    p3 = (1 - pnorm(abs(z3), 0, 1)) * 2
    y3=predict(tmodel3,newdata)
    
    tmodel4<-multinom(cate_l1~ t1+t2+t3+t4+t5+t6+t7+t8+t9, 
                      data=DF_checkin_weather,maxit = 1000)
    tsummary4 = summary(tmodel4)
    z4 = tsummary4$coefficients/tsummary4$standard.errors
    p4 = (1 - pnorm(abs(z4), 0, 1)) * 2
    y4=predict(tmodel4,newdata)

    
    tmodel5<-multinom(cate_l1~ s1+s2+s3+s4+s5+s6+s7+s8+s9, 
                      data=DF_checkin_weather,maxit = 1000)
    tsummary5 = summary(tmodel5)
    z5 = tsummary5$coefficients/tsummary5$standard.errors
    p5 = (1 - pnorm(abs(z5), 0, 1)) * 2
    y5=predict(tmodel5,newdata)


    
    comp = data.frame(DF_checkin_weather$cate_l1, 
                      y1,y2,y3,y4,y5)
    comp <- data.frame(lapply(comp, as.character), stringsAsFactors=FALSE)
    comp$corr1=ifelse(comp[1]==comp[2],1,0)
    comp$corr2=ifelse(comp[1]==comp[3],1,0)
    comp$corr3=ifelse(comp[1]==comp[4],1,0)
    comp$corr4=ifelse(comp[1]==comp[5],1,0)
    comp$corr5=ifelse(comp[1]==comp[6],1,0)
    
    list(
        "p"=list(p1,p2,p3,p4,p5),
        "corr"=c(sum(comp$corr1),sum(comp$corr2),sum(comp$corr3),
                 sum(comp$corr4),sum(comp$corr5))/nrow(comp),
        "records"=nrow(comp),
        "model"=list(tmodel1,tmodel2,tmodel3,tmodel4,tmodel5),
        "summary"=list(tsummary1,tsummary2,tsummary3,tsummary4,tsummary5)
    )
}