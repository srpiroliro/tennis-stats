# get all data
list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

to_num<-function(n){
    if(is.nan(n) | is.null(n) | is.na(n)) return(0.0)
    return(n)
}

age_diff<-c() # winner-looser
ht_diff<-c() # winner-looser

w_ages<-c()
w_hts<-c()

l_hts<-c()
l_ages<-c()


for(i in 1:nrow(all_data)){
    w_age<-to_num(all_data[i,]$winner_age)
    l_age<-to_num(all_data[i,]$loser_age)

    if(w_age>0 & l_age>0){
        w_ages<-c(w_ages,w_age)
        l_ages<-c(l_ages,l_age)
        age_diff<-c(age_diff, w_age-l_age)
    }

    l_ht<-to_num(all_data[i,]$loser_ht)
    w_ht<-to_num(all_data[i,]$winner_ht)

    if(w_ht>0 & w_ht>0){
        l_hts<-c(l_hts,l_ht)
        w_hts<-c(w_hts,w_ht)
        ht_diff<-c(ht_diff, w_ht-l_ht)
    }
}