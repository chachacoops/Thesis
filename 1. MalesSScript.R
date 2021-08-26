##### PACKAGES

install.packages("ggpubr")
library (ggpubr)
library(dplyr)
library(readr)
require(lubridate)
require(igraph)
require(asnipe)
require(ggraph)
require(chron)

rm(list=ls()) 

#download data
url <- "https://github.com/chachacoops/Thesis/archive/refs/heads/main.zip"
destfile <- "C:/Users/...YourWorkingDirectory"

download.file(url, destfile)

###### SEPARATE NETWORKS MALES MAZE
getwd()


###### MALES RIGHT SIDE MAZE
#combine files
rrfidms <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/RightRFIDMaless", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(rrfidms, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/combinedRRFIDms.csv")

unique(rrfidms$Transpondercode)

#read in the csv
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep")
rrfidms <- read.csv("combinedRRFIDms.csv", sep =";", header = T, quote = "")
write.csv(rrfidms, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/combinedRRFIDms.csv")

write.csv(rrfidms, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/combinedRRFIDms.csv")
#Here I have to round time in excel as I cant get it to work in R!

#there is also one recording of 00076C42B3 across the whole time period so that is removed

rrfidms <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/combinedRRFIDms.csv")
names(rrfidms) <- gsub("\\.", "", names(rrfidms)) #remove full stops from column names

View(rrfidms)
unique(rrfidms$Transpondercode)
rrfidms$Location <- "Right"

#adding in the condensed letters for each bird
rrfidms %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))

rrfidms <- mutate(rrfidms, Letter = case_when(
  (Transpondercode == "000783A8B7") ~ "A",
  (Transpondercode== "000783A3D1") ~ "B",
  (Transpondercode== "0007B117A4") ~ "C",
  (Transpondercode== "0007AC60AD") ~ "D",
  (Transpondercode== "0007B046BA") ~ "E",
  (Transpondercode== "0007ABAEC7") ~ "F",
  (Transpondercode== "0007B0F1CE") ~ "G",
  (Transpondercode== "0007ABB698") ~ "H",
  (Transpondercode== "0007AC4333") ~ "I",
  (Transpondercode== "00078397BE") ~ "J",
  (Transpondercode== "00078395C8") ~ "K",
  (Transpondercode== "00079D4DD3") ~ "L",
  (Transpondercode== "0007AC512E") ~ "M",
  (Transpondercode== "000783D5F0") ~ "N",
  (Transpondercode== "0007B015D8") ~ "O",
  (Transpondercode== "0007AFDFC5") ~ "P"
))

View(rrfidms)



#remove first weird column and remove any duplication, and save as a new data frame
rrfidms <- select(rrfidms, Date, RoundTime, Transpondercode, Location, Letter)
nrow(rrfidms)
rrfidms <- rrfidms[!duplicated(rrfidms),]

rrfidms$Date <- dmy(rrfidms$Date)

unique(rrfidms$Date)

###### Test OxU algorithm Right ------------------------------------------------------
subset.ox.msR <- rrfidms %>% filter(date(Date) >= "03-04-2021" & date(Date) <= "10-05-2021")
View(subset.ox.msR)
nodesmsR <- subset.ox.msR %>% distinct(Letter) #list of all birds in the subset


#format data
subset.ox.msR <- subset.ox.msR %>% mutate(date.time = dmy_hms(paste(subset.ox.msR$Date, subset.ox.msR$RoundTime, sep = " ")))
subset.ox.msR <- arrange(subset.ox.msR, date.time)
subset.ox.msR <-  mutate(subset.ox.msR,formatted.time = as.numeric(subset.ox.msR$date.time - subset.ox.msR$date.time[1]))
subset.ox.msR <-  select(subset.ox.msR, -Date, -RoundTime)


#Create network
assocMaleRightMaze <- gmmevents(time = subset.ox.msR$formatted.time, identity = subset.ox.msR$Letter, location = subset.ox.msR$Location) #assign birds into groups
network.ox.msR <- get_network(assocMaleRightMaze[[1]]) #Get association matrix
inetwork.ox.msR <- graph_from_adjacency_matrix(network.ox.msR, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics.msR <- nodesmsR %>% mutate(degree = degree(inetwork.ox.msR, v = Letter),
                                            strength = strength(inetwork.ox.msR, v = Letter),
                                            betweenness = betweenness(inetwork.ox.msR, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.msR, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/MalesRightMaze.csv", row.names = FALSE)

save(assocMaleRightMaze, file = "assocMaleRightMaze.RData")
load("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/assocMaleRightMaze.RData")

mean_distance(inetwork.ox.msR) # = 1 completely linked

unique(rrfidms$Date)

##### MALES LEFT SIDE MAZE #####

lrfidms <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/LeftRFIDMalesS", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(lrfidms, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/combinedLRFIDms.csv")


#Here I have to round time in excel as I cant get it to work in R
#read in the csv
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep")
lrfidms<- read.csv("combinedLRFIDms.csv", sep = ";", header = T, quote = "")
write.csv(lrfidms, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/combinedLRFIDms.csv")

lrfidms<- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep/combinedLRFIDms.csv")
names(lrfidms) <- gsub("\\.", "", names(lrfidms)) #remove full stops from column names
#remove first weird column and remove any duplication, and save as a new data frame
lrfidms$Location <- "Left"
View(lrfidms)

lrfidms %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))

unique(lrfidms$Transpondercode)

lrfidms <- mutate(lrfidms, Letter = case_when(
  (Transpondercode == "000783A8B7") ~ "A",
  (Transpondercode== "0007B117A4") ~ "C",
  (Transpondercode== "0007ABAEC7") ~ "F"
))

View(lrfidms)

lrfidms <- select(lrfidms, Date, RoundTime, Transpondercode, Location, Letter)
lrfidms <- lrfidms[!duplicated(lrfidms),]
nrow(lrfidms)

lrfidms$Date <- dmy(lrfidms$Date)

###### Test OxU algorithm Left------------------------------------------------------
subset.ox.msL <- lrfidms %>% filter(date(Date) >= "2021-04-03" & date(Date) <= "2021-05-10")

nodesmsL <- subset.ox.msL %>% distinct(Letter) #list of all birds in the subset


#format data
subset.ox.msL <- subset.ox.msL %>% mutate(date.time = ymd_hms(paste(subset.ox.msL$Date, subset.ox.msL$RoundTime, sep = " ")))
subset.ox.msL <- arrange(subset.ox.msL, date.time)
subset.ox.msL <-  mutate(subset.ox.msL,formatted.time = as.numeric(subset.ox.msL$date.time - subset.ox.msL$date.time[1]))
subset.ox.msL <-  select(subset.ox.msL, -Date, -RoundTime)


#Create network
assocMaleLeftMaze <- gmmevents(time = subset.ox.msL$formatted.time, identity = subset.ox.msL$Letter, location = subset.ox.msL$Location) #assign birds into groups
network.ox.msL <- get_network(assocMaleLeftMaze[[1]]) #Get association matrix
inetwork.ox.msL <- graph_from_adjacency_matrix(network.ox.msL, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics.msL <- nodesmsL %>% mutate(degree = degree(inetwork.ox.msL, v = Letter),
                                           strength = strength(inetwork.ox.msL, v = Letter),
                                           betweenness = betweenness(inetwork.ox.msL, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.msL, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesS/MalesLeftMaze.csv", row.names = FALSE)
save(assocMaleLeftMaze, file = "assocMaleLeftMaze.RData")
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesSSep")
load("assocMaleLeftMaze.RData")

mean_distance(inetwork.ox.msL)

