library(readxl)
library(dplyr)
library(reshape2)

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
  colnames(tempfile) <- c("Name1", "Name2", "never",
                          "monthly", "biweekly",
                          "weekly", "daily", "mult")
  
  # find person
  tempfile$Name2 <- tempfile$Name1[!is.na(tempfile$Name2)]
  tempfile <- tempfile[!(tempfile$Name1 == tempfile$Name2[1]),]
  
  # calculate edges
  tempfile$never[!is.na(tempfile$never)] <- 0
  tempfile$monthly[!is.na(tempfile$monthly)] <- 1
  tempfile$biweekly[!is.na(tempfile$biweekly)] <- 2
  tempfile$weekly[!is.na(tempfile$weekly)] <- 4
  tempfile$daily[!is.na(tempfile$daily)] <- 20
  tempfile$mult[!is.na(tempfile$mult)] <- 40
  tempfile[,3:8] <- sapply(tempfile[,3:8],as.numeric)
  tempfile$edge <- rowMeans(tempfile[,3:8], na.rm = T)
  
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
saveRDS(edges, "C:/Users/vrw657/Documents/Interactome_data/Clean/edges.rds")


nodes <- as.data.frame(cbind(id = unique(edges$from), label= unique(edges$from)))
saveRDS(nodes, "C:/Users/vrw657/Documents/Interactome_data/Clean/nodes.rds")



