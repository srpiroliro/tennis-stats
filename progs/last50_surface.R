
# data_2023<-read.csv("stats/2023.csv")
# data_2022<-read.csv("stats/2022.csv")
# data_2021<-read.csv("stats/2021.csv")
# data_2020<-read.csv("stats/2020.csv")

# all_data<-data_2020
list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))


clay<-all_data[all_data$surface=="Clay",]
hard<-all_data[all_data$surface=="Hard",]
grass<-all_data[all_data$surface=="Grass",]


# ALL PLAYER NAMES
players<-unique(c(all_data$winner_name,all_data$loser_name))


clay_results<-c()
hard_results<-c()
grass_results<-c()
played<-c()


for (player in players) {
    c_wins<-clay[clay$winner_name==player,][1:50,]
    c_loses<-clay[clay$loser_name==player,][1:50,]

    h_wins<-hard[hard$winner_name==player,][1:50,]
    h_loses<-hard[hard$loser_name==player,][1:50,]

    g_wins<-grass[grass$winner_name==player,][1:50,]
    g_loses<-grass[grass$loser_name==player,][1:50,]

    number_c_wins<-ifelse(is.null(nrow(c_wins)), 0, nrow(c_wins))
    number_c_loses<-ifelse(is.null(nrow(c_loses)), 0, nrow(c_loses))
    c_result<-number_c_wins/(number_c_loses+number_c_wins)

    number_h_wins<-ifelse(is.null(nrow(h_wins)), 0, nrow(h_wins))
    number_h_loses<-ifelse(is.null(nrow(h_loses)), 0, nrow(h_loses))
    h_result<-number_h_wins/(number_h_loses+number_h_wins)

    number_g_wins<-ifelse(is.null(nrow(g_wins)), 0, nrow(g_wins))
    number_g_loses<-ifelse(is.null(nrow(g_loses)), 0, nrow(g_loses))
    g_result<-number_g_wins/(number_g_loses+number_g_wins)

    clay_results<-c(clay_results,ifelse(is.nan(c_result), 0, c_result))
    hard_results<-c(hard_results,ifelse(is.nan(h_result), 0, h_result))
    grass_results<-c(grass_results,ifelse(is.nan(g_result), 0, g_result))

    played<-c(played,nrow(subset(all_data, winner_name==player | all_data$loser_name==player)))

}

result<-data.frame(Name=players,Clay=clay_results,Hard=hard_results,Grass=grass_results,Played=played)

# final_result<-final_result[order(final_result$Played, decreasing=FALSE), ]

write.csv(result, "progs/last50_surface.csv")