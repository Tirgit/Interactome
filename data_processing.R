library(readxl)
library(dplyr)
library(reshape2)

##### FOR EXCEL SHEET DATA COLLECTION

setwd("C:/Users/vrw657/Documents/Interactome_data")

# file list
files <- list.files(pattern="*.xlsx", 
                    full.names=TRUE, recursive=FALSE)

# initialize empty collection data frame
collect_df <- data.frame()

# loop for generating edges
for (i in files) {
  # load individual files in a loop
  tempfile <- read_xlsx(i)

  # rename columns
  colnames(tempfile) <- c("Name1", "Name2",
                          "monthly", "biweekly",
                          "weekly", "daily", "mult")
  
  # find person
  tempfile$Name2 <- tempfile$Name1[!is.na(tempfile$Name2)]
  tempfile <- tempfile[!(tempfile$Name1 == tempfile$Name2[1]),]
  
  # calculate edges
  tempfile$monthly[!is.na(tempfile$monthly)] <- 0.1
  tempfile$biweekly[!is.na(tempfile$biweekly)] <- 0.2
  tempfile$weekly[!is.na(tempfile$weekly)] <- 0.4
  tempfile$daily[!is.na(tempfile$daily)] <- 2
  tempfile$mult[!is.na(tempfile$mult)] <- 3
  tempfile[,3:7] <- sapply(tempfile[,3:7],as.numeric)
  tempfile$edge <- rowMeans(tempfile[,3:7], na.rm = T)
  tempfile$edge[is.na(tempfile$edge)] <- 0
  
  # remove not needed variables
  remove_var <- c("never", "monthly", "biweekly",
                  "weekly", "daily", "mult")
  tempfile <- tempfile[ , !(names(tempfile) %in% remove_var)]

  # append collection dataframe
  collect_df <- rbind(collect_df,tempfile)
}

# sort pairs alphabetically (so that A-B is the same as B-A)
collect_df[,1:2] <- collect_df %>%
  transmute(
    new_Name1 = ifelse(substr(Name1, 1, 1) <= substr(Name2, 1, 1),  Name1, Name2),
    new_Name2 = ifelse(new_Name1 == Name1, Name2, Name1)
  )

# average of interaction pairs A-B and B-A
edges <- aggregate(collect_df$edge,by=list(Name1=collect_df$Name1,Name2=collect_df$Name2),data=collect_df,FUN=mean)
colnames(edges) <- c("from", "to", "width")
edges <- edges[edges$width > 0,]
saveRDS(edges, "C:/Users/vrw657/Documents/Interactome_data/Clean/edges.rds")


nodes <- as.data.frame(cbind(id = unique(c(edges$from,edges$to)), label= unique(c(edges$from,edges$to))))
saveRDS(nodes, "C:/Users/vrw657/Documents/Interactome_data/Clean/nodes.rds")

























##### FOR GOOGLE FORMS DATA COLLECTION

ustwo_file <- read.csv2("C:/Users/vrw657/Documents/Interactome_data/Ustwo Interactome.csv", sep = ",")
ustwo_file$Timestamp <- NULL

ustwo_file[ustwo_file == ""] <- "0"
ustwo_file[ustwo_file == "Once a month"] <- "1"
ustwo_file[ustwo_file == "Biweekly"] <- "2"
ustwo_file[ustwo_file == "Weekly"] <- "4"
ustwo_file[ustwo_file == "Daily"] <- "20"
ustwo_file[ustwo_file == "Multiple times a day"] <- "30"
ustwo_file$Name1 <- ""

for (i in 1:nrow(ustwo_file)) {
  ustwo_file$Name1[i] <- colnames(ustwo_file)[ustwo_file[i,] == "This is me"]
}

ustwo_file[ustwo_file == "This is me"] <- "0"
as.numeric(ustwo_file)

ustwo_file2 <- as.data.frame(sapply(ustwo_file[-ncol(ustwo_file)], as.numeric))
ustwo_file2 <- cbind(Name1 = ustwo_file$Name1, ustwo_file2)

collect_df <- melt(ustwo_file2)
colnames(collect_df) <- c("Name1", "Name2", "edge")
collect_df$Name2 <- as.character(collect_df$Name2)

collect_df <- collect_df[!(collect_df$Name1 == collect_df$Name2),]


# sort pairs alphabetically (so that A-B is the same as B-A)
collect_df[,1:2] <- collect_df %>%
  transmute(
    new_Name1 = ifelse(substr(Name1, 1, 1) <= substr(Name2, 1, 1),  Name1, Name2),
    new_Name2 = ifelse(new_Name1 == Name1, Name2, Name1)
  )

# average of interaction pairs A-B and B-A
edges <- aggregate(collect_df$edge,by=list(Name1=collect_df$Name1,Name2=collect_df$Name2),data=collect_df,FUN=mean)
colnames(edges) <- c("from", "to", "width")
saveRDS(edges, "C:/Users/vrw657/Documents/Interactome_data/Clean/edges.rds")


nodes <- as.data.frame(cbind(id = unique(c(edges$from,edges$to)), label= unique(c(edges$from,edges$to))))
saveRDS(nodes, "C:/Users/vrw657/Documents/Interactome_data/Clean/nodes.rds")



