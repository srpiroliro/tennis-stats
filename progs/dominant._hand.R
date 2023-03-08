# list_csv_files<-list.files(path="stats/", pattern = "*.csv")
# all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

# rr_cnt<-nrow(all_data[all_data&winner_hand=="R" && all_data&winner_hand==all_data$loser_hand])
# lr_cnt<-c(nrow(all_data[all_data&winner_hand!=all_data$loser_hand]))
# ll_cnt<-nrow(all_data[all_data&winner_hand=="L" && all_data&winner_hand==all_data$loser_hand])


winners_data<-data.frame(matrix(ncol=7, nrow=0))
colnames(winners_data)<-names

losers_data<-data.frame(matrix(ncol=7, nrow=0))
colnames(losers_data)<-names

####

# order data by date & match number
all_data<-all_data[order(all_data$tourney_date, all_data$match_num, decreasing=TRUE), ]



# catch data foreach line

count<-0

last<-as.numeric(Sys.time(), units = "secs")

# 198.000 * 198.000^6 = 11.930.436.453.209.472

for(i in 1:nrow(all_data)){ # nrow(all_data) = 198.000
    if(i %% 10 == 0){
        actual<-as.numeric(Sys.time(), units = "secs")
        print(paste(i,": ", actual-last))
        last<-actual
    }

    w_age<-all_data[i,]$winner_age
    l_age<-all_data[i,]$loser_age
    age_diff<-w_age-l_age

    w_ht<-all_data[i,]$winner_ht
    l_ht<-all_data[i,]$loser_ht
    ht_diff<-w_ht-l_ht

    w_rank_points<-all_data[i,]$winner_rank_points
    l_rank_points<-all_data[i,]$loser_rank_points

    rank_points_diff<-w_rank_points-l_rank_points


    # results <- get_last_result(all_data[i,]$winner_name, all_data[i,]$loser_name, i, all_data)

    # winner_results<-results$p1$a3
    # loser_results<-results$p2$a3

    # winner_5_results<-results$p1$a5
    # loser_5_results<-results$p2$a5

    surface<-all_data[i,]$surface
    if(surface=="Clay")
        surface<-1
    else if(surface=="Hard")
        surface<-2
    else if(surface=="Grass")
        surface<-3
    else
        surface<-0

    winner_exp<-get_exp(all_data[i,]$winner_name,i,all_data)
    loser_exp<-get_exp(all_data[i,]$loser_name,i,all_data)

    if(!is.na(age_diff) & !is.na(ht_diff) & !is.na(rank_points_diff)){
        winners_data[nrow(winners_data)+1,]<-list(
            winner_5_results,
            winner_results,
            surface,
            age_diff,
            ht_diff,
            rank_points_diff,
            winner_exp
        )
        losers_data[nrow(losers_data)+1,]<-list(
            loser_5_results,
            loser_results,
            surface,
            -age_diff,
            -ht_diff,
            -rank_points_diff,
            loser_exp
        )
    }
}