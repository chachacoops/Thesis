library (ggpubr)
library(dplyr)
library(readr)
require(lubridate)
require(igraph)
require(asnipe)
require(ggraph)
require(chron)
require(ggplot2)

rm(list=ls()) 


###### SEPARATE NETWORKS FEMALES MAZE
getwd()
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS")


###### FEMALES RIGHT SIDE MAZE
#combine datafiles into one
rrfidfs <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/RightRFIDFemaleS", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(rrfidfs, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/combinedRRFIDfs.csv")

unique(rrfidfs$Transpondercode)

#read in the csv
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS")
rrfidfs <- read.csv("combinedRrfidfs.csv", sep =";", header = T, quote = "")
write.csv(rrfidfs, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/combinedRrfidfs.csv")

write.csv(rrfidfs, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/combinedRrfidfs.csv")
#Here I have to round time in excel as I cant get it to work in R!

#there is also one recording of 00076C42B3 across the whole time period so that is removed

rrfidfs <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/combinedRrfidfs.csv")
names(rrfidfs) <- gsub("\\.", "", names(rrfidfs)) #remove full stops from column names
rrfidfs$Location <- "Right"

#adding in the letters for each bird
rrfidfs %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode)) %>% print(n=40)

rrfidfs <- mutate(rrfidfs, Letter = case_when(
  (Transpondercode == "0007838EE2") ~ "A",
  (Transpondercode== "00078394D9") ~ "B",
  (Transpondercode== "000783A389") ~ "C",
  (Transpondercode== "000783A448") ~ "D",
  (Transpondercode== "000783CA72") ~ "E",
  (Transpondercode== "000783E377") ~ "F",
  (Transpondercode== "0007AB9F6A") ~ "G",
  (Transpondercode== "0007ABAFBF") ~ "H",
  (Transpondercode== "0007AC2770") ~ "I",
  (Transpondercode== "0007AC388E") ~ "J",
  (Transpondercode== "0007AC5F96") ~ "K",
  (Transpondercode== "0007AC6746") ~ "L",
  (Transpondercode== "0007AC6CCE") ~ "M",
  (Transpondercode== "0007AC93ED") ~ "N",
  (Transpondercode== "0007ACA9A5") ~ "O",
  (Transpondercode== "0007ACB9B7") ~ "P",
  (Transpondercode== "0007ACDAC3") ~ "Q",
  (Transpondercode== "0007B0C904") ~ "R",
  (Transpondercode== "0007B0ED87") ~ "S",
  (Transpondercode== "0007B0F2B3") ~ "T"
))


#remove first weird column and remove any duplication, and save as a new data frame
rrfidfs <- select(rrfidfs, Date, RoundTime, Transpondercode, Location, Letter)
nrow(rrfidfs)
rrfidfs <- rrfidfs[!duplicated(rrfidfs),]

unique(rrfidfs$Date)

###### Test OxU algorithm Right ------------------------------------------------------
subset.ox.fsR <- rrfidfs %>% filter(date(Date) >= "04-06-2021" & date(Date) <= "25-06-2021")
View(subset.ox.fsR)
nodesfsR <- subset.ox.fsR %>% distinct(Letter) #list of all birds in the subset


#format data
subset.ox.fsR <- subset.ox.fsR %>% mutate(date.time = dmy_hms(paste(subset.ox.fsR$Date, subset.ox.fsR$RoundTime, sep = " ")))
subset.ox.fsR <- arrange(subset.ox.fsR, date.time)
subset.ox.fsR <-  mutate(subset.ox.fsR,formatted.time = as.numeric(subset.ox.fsR$date.time - subset.ox.fsR$date.time[1]))
subset.ox.fsR <-  select(subset.ox.fsR, -Date, -RoundTime)


#Create network
assocFemaleRightMaze <- gmmevents(time = subset.ox.fsR$formatted.time, identity = subset.ox.fsR$Letter, location = subset.ox.fsR$Location) #assign birds into groups
network.ox.fsR <- get_network(assocFemaleRightMaze[[1]]) #Get association matrix
inetwork.ox.fsR <- graph_from_adjacency_matrix(network.ox.fsR, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics.fsR <- nodesfsR %>% mutate(degree = degree(inetwork.ox.fsR, v = Letter),
                                            strength = strength(inetwork.ox.fsR, v = Letter),
                                            betweenness = betweenness(inetwork.ox.fsR, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.fsR, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/FemalesRightMaze.csv", row.names = FALSE)
getwd()
save(assocFemaleRightMaze, file = "assocFemaleRightMaze.RData")
load("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/assocFemaleRightMaze.RData")

gsize(inetwork.ox.fsR)

##### FEMALES LEFT SIDE MAZE #####

#combine datasets

lrfidfs <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/LeftRFIDFemalesS", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(lrfidfs, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/combinedLrfidfs.csv")


#Here I have to round time in excel as I cant get it to work in R
#read in the csv
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS")
lrfidfs<- read.csv("combinedLrfidfs.csv", sep = ";", header = T, quote = "")
names(lrfidfs) <- gsub("\\.", "", names(lrfidfs)) #remove full stops from column names
write.csv(lrfidfs, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/combinedLrfidfs.csv")


#remove first weird column and remove any duplication, and save as a new data frame
lrfidfs<- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/combinedLrfidfs.csv")
lrfidfs$Location <- "Left"
View(lrfidfs)

lrfidfs %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))
#assign letters to birds

lrfidfs <- mutate(lrfidfs, Letter = case_when(
  (Transpondercode == "0007838EE2") ~ "A",
  (Transpondercode== "00078394D9") ~ "B",
  (Transpondercode== "000783A389") ~ "C",
  (Transpondercode== "000783A448") ~ "D",
  (Transpondercode== "000783CA72") ~ "E",
  (Transpondercode== "000783E377") ~ "F",
  (Transpondercode== "0007AB9F6A") ~ "G",
  (Transpondercode== "0007ABAFBF") ~ "H",
  (Transpondercode== "0007AC2770") ~ "I",
  (Transpondercode== "0007AC388E") ~ "J",
  (Transpondercode== "0007AC5F96") ~ "K",
  (Transpondercode== "0007AC6746") ~ "L",
  (Transpondercode== "0007AC6CCE") ~ "M",
  (Transpondercode== "0007AC93ED") ~ "N",
  (Transpondercode== "0007ACA9A5") ~ "O",
  (Transpondercode== "0007ACB9B7") ~ "P",
  (Transpondercode== "0007ACDAC3") ~ "Q",
  (Transpondercode== "0007B0C904") ~ "R",
  (Transpondercode== "0007B0ED87") ~ "S",
  (Transpondercode== "0007B0F2B3") ~ "T"
))

View(lrfidfs)
#take subset
lrfidfs <- select(lrfidfs, Date, RoundTime, Transpondercode, Location, Letter)
lrfidfs <- lrfidfs[!duplicated(lrfidfs),]
nrow(lrfidfs)

lrfidfs$Date <- dmy(lrfidfs$Date)
unique(lrfidfs$Date)

###### Test OxU algorithm Left------------------------------------------------------
subset.ox.fsL <- lrfidfs %>% filter(date(Date) >= "2021-06-04" & date(Date) <= "2021-06-25")

nodesfsL <- subset.ox.fsL %>% distinct(Letter) #list of all birds in the subset


#format data
subset.ox.fsL <- subset.ox.fsL %>% mutate(date.time = ymd_hms(paste(subset.ox.fsL$Date, subset.ox.fsL$RoundTime, sep = " ")))
subset.ox.fsL <- arrange(subset.ox.fsL, date.time)
subset.ox.fsL <-  mutate(subset.ox.fsL,formatted.time = as.numeric(subset.ox.fsL$date.time - subset.ox.fsL$date.time[1]))
subset.ox.fsL <-  select(subset.ox.fsL, -Date, -RoundTime)



#Create network
assocFemaleLeftMaze <- gmmevents(time = subset.ox.fsL$formatted.time, identity = subset.ox.fsL$Letter, location = subset.ox.fsL$Location) #assign birds into groups
network.ox.fsL <- get_network(assocFemaleLeftMaze[[1]]) #Get association matrix
inetwork.ox.fsL <- graph_from_adjacency_matrix(network.ox.fsL, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics.fsL <- nodesfsL %>% mutate(degree = degree(inetwork.ox.fsL, v = Letter),
                                            strength = strength(inetwork.ox.fsL, v = Letter),
                                            betweenness = betweenness(inetwork.ox.fsL, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.fsL, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/FemalesLeftMaze.csv", row.names = FALSE)

save(assocFemaleLeftMaze, file = "assocFemaleLeftMaze.RData")
load("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesS/assocFemaleLeftMaze.RData")
#network size
gsize(inetwork.ox.fsL)
