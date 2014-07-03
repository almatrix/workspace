insertrow = function(tab, position, vector=NA, name=NA){
    ## how many original rows before insertation
    n_rows = nrow(tab)
    
    ## specify the row content 
    n_cols = ncol(tab)
    if(is.na(vector)){
        vector = rep(0, n_cols)
    }
    
    ## insert the row in the specified position
    ## add row name if specified  
    if(position == 1 || position == "top"|| position == "first"|| position == "head"){
        newtab = rbind(vector, tab)
        if(!is.na(name)){
            rownames(newtab)=c(name,rownames(tab))
        }
    }
    else if(position == (n_rows+1) || position == "tail"|| position == "end"|| position == "last"){
        newtab = rbind(tab, vector)
        if(!is.na(name)){
            rownames(newtab)= c(rownames(tab),name)
        }
    }
    else{
        head1 = tab[1:(position-1),]
        tail1 = tab[position:n_rows,]
        if(n_cols>1) { # works for tables with more than one columns
            newtab = rbind(head1, vector, tail1)
        } else {
            newtab=as.table(matrix(c(head1, rep(0,1), tail1),
                                   ncol=1,byrow=TRUE))
            colnames(newtab)=colnames(tab)
        }
        if(!is.na(name)){
            rownames(newtab)= c(rownames(tab)[1:(position-1)],name,
                                rownames(tab)[position:n_rows])
        }
    }
    newtab
}

insertcol = function(tab, position, vector=NA, name=NA){
    ## how many original columns before insertation   
    n_cols = ncol(tab)
    
    
    ## specify the column content 
    n_rows = nrow(tab)
    if(is.na(vector)){
        vector = rep(0, n_rows)
    }
    
    ## insert the row in the specified position
    ## add row name if specified
    if(position == 1 || position == "first"|| position == "left"){
        newtab = cbind(vector, tab)
        if(!is.na(name)){
            colnames(newtab)=c(name,colnames(tab))
        }
    }
    else if( position == (n_cols+1) || position == "right"|| position == "last"){
        newtab = cbind(tab, vector)
        if(!is.na(name)){
            colnames(newtab)= c(colnames(tab),name)
        }
    }
    else{
        left = tab[,1:(position-1)]
        right = tab[,position:n_cols]
        newtab = cbind(left, vector, right)
        if(!is.na(name)){
            colnames(newtab)= c(colnames(tab)[1:(position-1)],name,
                                colnames(tab)[position:n_cols])
        }
    }
    
    newtab
}