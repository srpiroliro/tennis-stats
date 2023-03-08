# get all data
list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

# rr_cnt<-nrow(all_data[all_data&winner_hand=="R" & all_data&winner_hand==all_data$loser_hand])
# lr_cnt<-c(nrow(all_data[all_data&winner_hand!=all_data$loser_hand]))
# ll_cnt<-nrow(all_data[all_data&winner_hand=="L" & all_data&winner_hand==all_data$loser_hand])

to_num<-function(n){
    if(is.nan(n) | is.null(n) | is.na(n)) return(0.0)
    return(n)
}

age_diff<-c()
ht_diff<-c() 

w_ages<-c()
w_hts<-c()

l_hts<-c()
l_ages<-c()

wnum_aces<-c()

all_aces<-c()
all_hts<-c()

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

    
    w_ace<-to_num(all_data[i,]$w_ace)
    l_ace<-to_num(all_data[i,]$l_ace)

    if(l_ht>0 & l_ace>0){
        all_aces<-c(all_aces,l_ace)
        all_hts<-c(all_hts,l_ht)
    }

    if(w_ht>0 & w_ace>0){
        all_aces<-c(all_aces,w_ace)
        all_hts<-c(all_hts,w_ht)
    }

    if(w_ht>0 & l_ht>0){
        l_hts<-c(l_hts,l_ht)

        w_hts<-c(w_hts,w_ht)
        wnum_aces<-c(wnum_aces, w_ace)
        ht_diff<-c(ht_diff, w_ht-l_ht)
    }
}


l_aces<-c()
w_aces<-c()
for(i in 1:nrow(all_data)){
    l_aces<-c(l_aces, all_data[i,]$l_ace)
    w_aces<-c(w_aces, all_data[i,]$w_ace)
}



library(ggplot2)


##############################################################################

#  HEIGHTS

# loser heihgts
filtered_l_hts<-l_hts[l_hts>0]
df<-data.frame(filtered_l_hts)
ggplot(df, aes(x=filtered_l_hts))+geom_density()
ggsave("loser_heights.png")

# winner heihgts
filtered_w_hts<-w_hts[w_hts>0]
df<-data.frame(filtered_w_hts)
ggplot(df, aes(x=filtered_w_hts))+geom_density()
ggsave("winner_heights.png")


##############################################################################

# AGES

# loser ages
filtered_l_ages<-l_ages[l_ages>0]
df<-data.frame(filtered_l_ages)
ggplot(df, aes(x=filtered_l_ages))+geom_density()
ggsave("loser_ages.png")

# winner ages
filtered_w_ages<-w_ages[w_ages>0]
df<-data.frame(filtered_w_ages)
ggplot(df, aes(x=filtered_w_ages))+geom_density()
ggsave("winner_ages.png")

##############################################################################

# AGE DIFFERENCE




# more aces = >% win? TODO
# aw_df<-data.frame()



##############################################################################

# HEIGHT VS NUM. OF ACES

ha_df<-data.frame(Height=all_hts, Aces=all_aces)
p<-ggplot(ha_df,aes(x=Height, y=Aces))
p+geom_jitter()+geom_smooth()
ggsave("heihgt_vs_aces.png")




##############################################################################

# HEIGHT DIFFERENCE - % OF WINNING

stats<-data.frame(matrix(ncol=3,nrow=0))
colnames(stats)<-c("h_diff","wins","looses")


for(i in 1:nrow(all_data)){
    l_ht<-to_num(all_data[i,]$loser_ht)
    w_ht<-to_num(all_data[i,]$winner_ht)

    h_diff<-abs(w_ht-l_ht)

    if(w_ht>0 & l_ht>0){
        if(nrow(stats[stats$h_diff==h_diff,])==1){
            r_f<-stats[stats$h_diff==h_diff,]
            
            if(w_ht>l_ht){
                stats[stats$h_diff==h_diff,]<-c(h_diff,r_f$wins+1,r_f$looses)
            } else {
                stats[stats$h_diff==h_diff,]<-c(h_diff,r_f$wins,r_f$looses+1)
            }

        } else {    
            if(w_ht>l_ht){
                stats[nrow(stats)+1,]<-c(h_diff,1,0)
            } else {
                stats[nrow(stats)+1,]<-c(h_diff,0,1)
            }
        }
    }
}

for(i in 1:nrow(stats)){
    stats[i,]$win_p<-stats[i,]$wins/(stats[i,]$wins+stats[i,]$looses)
}


##############################################################################

# WINNING % AGAINST HEIGHT

uh<-unique(c(all_data$loser_ht, all_data$winner_ht))
u<-data.frame(Height=uh)
u$wins<-0
u$loses<-0

for(i in 1:nrow(all_data)){
    l_ht<-to_num(all_data[i,]$loser_ht)
    w_ht<-to_num(all_data[i,]$winner_ht)

    if(w_ht>0 & l_ht>0){
        u[u$Height==w_ht,]$wins<-u[u$Height==w_ht,]$wins+1
        u[u$Height==l_ht,]$loses<-u[u$Height==l_ht,]$loses+1
    }
}

u$wp<-0
for(i in 1:nrow(u)){
    u[i,]$wp<-u[i,]$wins/(u[i,]$wins+u[i,]$loses)
}

ggplot(u[u$wp>0,],aes(x=Height, y=wp))+geom_point()+geom_smooth(method="lm",col="red")
ggsave("height_against_win_perc.png")


##############################################################################

# NAME

ggsave("")





rank_dif<-c()
for(i in 1:nrow(all_data)){
    w_rank<-all_data[i,]$winner_rank_points
    l_rank<-all_data[i,]$loser_rank_points

    rank_dif<-c(rank_dif,w_rank-l_rank)
}

##############################################################################

# SURFACE POPULARITY

ss<-c(nrow(all_data[all_data$surface=="Grass",]),nrow(all_data[all_data$surface=="Hard",]),nrow(all_data[all_data$surface=="Clay",]))
df<-data.frame(name=c("Grass","Hard","Clay"),value=ss)
ggplot(df, aes(x=name, y=value))+geom_bar(stat="identity")
ggsave("surface_popularity.png")


##############################################################################

# ACES PER SERVE DEPENDING ON THE SURFACE
grass<-all_data[all_data$surface=="Grass",]
grass_aces<-c(grass$w_ace,grass$l_ace)
grass_aces<-grass_aces[ !is.na(grass_aces)]

hard<-all_data[all_data$surface=="Hard",]
hard_aces<-c(hard$w_ace,hard$l_ace)
hard_aces<-hard_aces[ !is.na(hard_aces)]

clay<-all_data[all_data$surface=="Clay",]
clay_aces<-c(clay$w_ace,clay$l_ace)
clay_aces<-clay_aces[ !is.na(clay_aces)]

df<-data.frame(name=c("Grass","Hard","Clay"),value=c(mean(grass_aces),mean(hard_aces),mean(clay_aces)))
ggplot(df, aes(x=name, y=value))+geom_bar(stat="identity")
ggsave("aces_by_surface.png")


##############################################################################

# NAME


grass<-all_data[all_data$surface=="Grass",]
grass_minutes<-grass$minutes
grass_minutes<-grass_minutes[ !is.na(grass_minutes)]
grass_minutes<-grass_minutes[ !grass_minutes==0]

clay<-all_data[all_data$surface=="Clay",]
clay_minutes<-clay$minutes
clay_minutes<-clay_minutes[ !is.na(clay_minutes)]
clay_minutes<-clay_minutes[ !clay_minutes==0]



ggsave("")


##############################################################################

# NAME

ggsave("")


##############################################################################

# NAME

ggsave("")


##############################################################################

# NAME

ggsave("")
