dbconn = function(){
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host="localhost", port=5432, 
                     dbname="foursquare_checkins",
                     user="postgres", password="shelia0514") 
    con
}


# ## test function
# if(!exists("conn")){
#     print("create a connection.")
#     conn = dbconn()
# }