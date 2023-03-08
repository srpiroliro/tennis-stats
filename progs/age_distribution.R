library(ggplot2)
library(dplyr)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)


list_csv_files<-list.files(path="stats/", pattern = "2015.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

to_num<-function(n){
    if(is.nan(n) | is.null(n) | is.na(n)) return(0.0)
    return(n)
}

ages<-c()

for(i in 1:nrow(all_data)){
    age<-to_num(all_data[i,]$winner_age)
    ages<-c(ages,age)

    age<-to_num(all_data[i,]$loser_age)
    ages<-c(ages,age)
}


# Make the histogram
plotgraph<-data.frame(ages) %>%  
  filter(ages>0 ) %>%
  ggplot(aes(x=ages)) +
    geom_density(fill="#69b3a2", color="#e9ecef",alpha=0.8)

# Save as a png file
ggsave(plotgraph, filename="imgs/histogram.png", width=10, height=7, units="cm")