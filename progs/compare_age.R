# get all data
list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

to_num<-function(n){
    if(is.nan(n) | is.null(n) | is.na(n)) return(0.0)
    return(n)
}

age_diff<-c() # winner-looser

w_ages<-c()
w_hts<-c()

l_hts<-c()
l_ages<-c()


for(i in 1:nrow(all_data)){
    w_age<-to_num(all_data[i,]$winner_age)
    w_ages<-c(w_ages,w_age)

    l_age<-to_num(all_data[i,]$loser_age)
    l_ages<-c(l_ages,l_age)

    if(l_age>0 & w_age>0){
        age_diff<-c(age_diff, w_age-l_age)
    }
}