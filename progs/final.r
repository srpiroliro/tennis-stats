
get_last_result<-function(name1, name2, current_index, matches){
    cnt3<-list()
    cnt5<-list()

    cnt3_b<-list()
    cnt5_b<-list()


    cnt_e1<-0
    cnt_e2<-0

    for(i in current_index:nrow(matches)){
        current_match<-matches[i,]
         
        if(length(cnt5)<5){
        
            if(current_match$winner_name==name1 | current_match$loser_name==name1){
                tmp<-ifelse(current_match$winner_name==name,1,0)
                if(length(cnt3)!=3){
                    cnt3<-list(cnt3, tmp)
                }
                cnt5<-list(cnt5,tmp)


            } else if (current_match$winner_name==name2 | current_match$loser_name==name2) {
                tmp<-ifelse(current_match$winner_name==name,1,0)
                if(length(cnt3_b)!=3){
                    cnt3_b<-list(cnt3_b, tmp)
                }
                cnt5_b<-list(cnt5_b,tmp)
            }
        }




        
    }
    r<-list(
        p1=list(a3=mean(cnt3),a5=mean(cnt5),exp=cnt_e1),
        p2=list(a3=mean(cnt3_b),a5=mean(cnt5_b),exp=cnt_e2),
        
    )

    return(r)
}

get_exp<-function(name, current_index, matches){
    shorted_m<-matches[-c(1:current_index),]

    return(nrow(shorted_m[shorted_m$winner_name==name | shorted_m$loser_name==name,]))
}

separate_data<-function(seed, margin){
    set.seed(seed)

    train_indices <- sample(nrow(all_data), size = round(margin * nrow(all_data)), replace = FALSE)

    train <- all_data[train_indices, ]
    test <- all_data[-train_indices, ]

    return(train) # adads$test
}

list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

backup<-all_data

all_data<-separate_data(as.numeric(Sys.time(), units = "secs"), 0.85)


names<-c(
    "last_5_results", # win rate last 5
    "last_3_results", # win rate last 3
    "surface",
    "age_diff",
    "ht_diff",
    "rank_diff",
    "exp" # matches
    # "last_result",    #0  1
    # "last_2_result",  #0  0
    # "last_3_result"   #1  0
)

####

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

print(nrow(all_data))
print("start")

for(i in 1:nrow(all_data)){ # nrow(all_data) = 168.300
    if(i %% 100 == 0){
        actual<-as.numeric(Sys.time(), units = "milisecs")
        print(paste(i,":",actual-last))
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

    # winner_exp<-get_exp(all_data[i,]$winner_name,i,all_data)
    # loser_exp<-get_exp(all_data[i,]$loser_name,i,all_data)

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

# density graph of the winner_exp and loser_exp
# plot(density(winners_data$exp), main="Winner Experience", xlab="Experience", ylab="Density")
# plot(density(losers_data$exp), main="Loser Experience", xlab="Experience", ylab="Density")

# glm(win_lose~surface+tourney_level+diff_seed+age_diff+ht_diff+rank_diff+experience+last_result, data = df_model)


# surface
    # X tourney_level
    # X diff_seed
# age_diff
# ht_diff
# rank_diff

# experience
# last_result