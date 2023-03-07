list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

rr_cnt<-nrow(all_data[all_data&winner_hand=="R" && all_data&winner_hand==all_data$loser_hand])
lr_cnt<-c(nrow(all_data[all_data&winner_hand!=all_data$loser_hand]))
ll_cnt<-nrow(all_data[all_data&winner_hand=="L" && all_data&winner_hand==all_data$loser_hand])
