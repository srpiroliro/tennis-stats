list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))


# another dataframe for players
formatted_data<-data.frame(matrix(ncol=17,nrow=0))
colnames(formatted_data)<-c(
    "player",
    "player_rank",
    "h2h", # % wins with opponent
    "previous_surface_result",
    "surface_streak",
    "previous_result",
    "streak",

    # opponen stats
    "opponent",
    "opponent_rank",
    "o_previous_surface_result",
    "o_surface_streak",
    "o_previous_result",
    "o_streak",

    # match stats
    "t_id",
    "match_num",
    "result", # win/loss of player
    "t_date"

)

players_data<-data.frame(matrix(ncol=6,nrow=0))
colnames(players_data)<-c(
    "player",
    "player_hand",
    "player_age",
    "player_height",
    "player_nationality"
)

matches_data<-data.frame(matrix(ncol=8,nrow=0))
colnames(matches_data)<-c(
    "t_id",
    "match_num",
    "t_name",
    "t_date",
    "t_level",
    "surface",
    "best_of", # longer the match = more stamina needed (younger = better stamina)
    "round",
    # "minutes",
    # "score"
)


for(i in 1:nrow(all_data)){
    d<-all_data[i,]

    match<-list(d$tourney_id,d$match_num,d$tourney_name,d$tourney_date,d$tourney_level,d$surface,d$best_of,d$round)
    matches_data[nrow(matches_data)+1,]<-match

    #########################################################################################################################

    playerA<-list(d$winner_name,d$winner_hand,d$winner_age,d$winner_ht,d$winner_ioc)
    playerB<-list(d$loser_name,d$loser_hand,d$loser_age,d$loser_ht,d$loser_ioc)

    if(nrow(players_data[players_data$player==playerA[[1]]])==0){
        players_data[nrow(players_data)+1,]<-playerA
    }

    if(nrow(players_data[players_data$player==playerB[[1]]])==0){
        players_data[nrow(players_data)+1,]<-playerB
    }

    #########################################################################################################################

    win<-list(d$winner_name,d$winner_rank_points,NaN,NaN,NaN,NaN,NaN,d$loser_name,d$loser_rank_points,NaN,NaN,NaN,NaN,d$tourney_id,d$match_num,"win",d$tourney_date)
    loss<-list(d$loser_name,d$loser_rank_points,NaN,NaN,NaN,NaN,NaN,d$winner_name,d$winner_rank_points,NaN,NaN,NaN,NaN,d$tourney_id,d$match_num,"loss",d$tourney_date)

    formatted_data[nrow(formatted_data)+1,]<-win
    formatted_data[nrow(formatted_data)+1,]<-loss
}