list_csv_files<-list.files(path="stats/", pattern = "*.csv")
all_data<-do.call(rbind, lapply(list_csv_files, function(x) read.csv(paste("stats/",x, sep=""),stringsAsFactors = FALSE)))

set.seed(as.numeric(Sys.time(), units = "secs"))

training_indices <- sample(nrow(all_data), size = round(0.85 * nrow(all_data)), replace = FALSE)

training <- all_data[training_indices, ]

test <- all_data[-training_indices, ]
