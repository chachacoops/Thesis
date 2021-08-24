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
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO")


###### FEMALES RIGHT SIDE MAZE

rrfidfo <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/RightRFIDFemalesO", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(rrfidfo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/combinedRRFIDfo.csv")

unique(rrfidfo$Transpondercode)

#read in the csv
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO")
rrfidfo <- read.csv("combinedRrfidfo.csv", sep =";", header = T, quote = "")
View(rrfidfo)
write.csv(rrfidfo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/combinedRrfidfo.csv")
#Here I have to round time in excel as I cant get it to work in R!

#amend any ghost reads

rrfidfo <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/combinedrrfidfo.csv")
names(rrfidfo) <- gsub("\\.", "", names(rrfidfo)) #remove full stops from column names

rrfidfo$Location <- "Right"

#adding in the condensed letters for each bird
rrfidfo %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode)) %>% print(n=40)



rrfidfo <- mutate(rrfidfo, Letter = case_when(
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

View(rrfidfo)

unique(rrfidfo$Date)

#remove first weird column and remove any duplication, and save as a new data frame
rrfidfo <- select(rrfidfo, Date, RoundTime, Transpondercode, Location, Letter)
nrow(rrfidfo)
rrfidfo <- rrfidfo[!duplicated(rrfidfo),]

rrfidfoO <- rrfidfo
rrfidfo$Date <- dmy(rrfidfo$Date)


###### Test OxU algorithm Right ------------------------------------------------------
subset.ox.foR <- rrfidfo %>% filter(date(Date) >= "2021-06-25" & date(Date) <= "2021-07-20")
View(subset.ox.foR)
nodesfoR <- subset.ox.foR %>% distinct(Letter) #list of all birds in the subset


#format data
subset.ox.foR <- subset.ox.foR %>% mutate(date.time = ymd_hms(paste(subset.ox.foR$Date, subset.ox.foR$RoundTime, sep = " ")))
subset.ox.foR <- arrange(subset.ox.foR, date.time)
subset.ox.foR <-  mutate(subset.ox.foR,formatted.time = as.numeric(subset.ox.foR$date.time - subset.ox.foR$date.time[1]))
subset.ox.foR <-  select(subset.ox.foR, -Date, -RoundTime)


#Create network
assocFemaleRightOpen <- gmmevents(time = subset.ox.foR$formatted.time, identity = subset.ox.foR$Letter, location = subset.ox.foR$Location) #assign birds into groups
network.ox.foR <- get_network(assocFemaleRightOpen[[1]]) #Get association matrix
inetwork.ox.foR <- graph_from_adjacency_matrix(network.ox.foR, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics.foR <- nodesfoR %>% mutate(degree = degree(inetwork.ox.foR, v = Letter),
                                            strength = strength(inetwork.ox.foR, v = Letter),
                                            betweenness = betweenness(inetwork.ox.foR, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.foR, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/FemalesRightOpen.csv", row.names = FALSE)
getwd()
save(assocFemaleRightOpen, file = "assocFemaleRightOpen.RData")
load("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/assocFemaleRightOpen.RData")


gsize(inetwork.ox.foR)
summary(subset.ox.foR)

##### Females LEFT SIDE MAZE #####

lrfidfo <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/LeftRFIDFemalesO", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(lrfidfo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/combinedLrfidfo.csv")


#Here I have to round time in excel as I cant get it to work in R
#read in the csv
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO")
lrfidfo<- read.csv("combinedLrfidfo.csv", sep = ";", header = T, quote = "")
names(lrfidfo) <- gsub("\\.", "", names(lrfidfo)) #remove full stops from column names
write.csv(lrfidfo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/combinedlrfidfo.csv")
View(lrfidfo)

#remove first weird column and remove any duplication, and save as a new data frame
lrfidfo<- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/combinedlrfidfo.csv")
lrfidfo$Location <- "Left"
View(lrfidfo)

lrfidfo %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))

unique(lrfidfo$Transpondercode)

lrfidfo <- mutate(lrfidfo, Letter = case_when(
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

View(lrfidfo)

lrfidfo <- select(lrfidfo, Date, Time, Transpondercode, Location, Letter)
lrfidfo <- lrfidfo[!duplicated(lrfidfo),]
nrow(lrfidfo)

lrfidfo$Date <- dmy(lrfidfo$Date)
unique(lrfidfo$Letter)

#There's only one bird in the left Open network, so I've made a data frame with 0s for strength, betweenness, and degree
Letter <- c("M")
degree <- c(0)
strength <- c(0)
betweenness <- c(0)
Type <- c("Open")
FemalesOL <- data.frame(Letter, degree, strength, betweenness)


###### Test OxU algorithm Left------------------------------------------------------
subset.ox.foL <- lrfidfo %>% filter(date(Date) >= "2021-06-04" & date(Date) <= "2021-06-25")

nodesfoL <- subset.ox.foL %>% distinct(Letter) #list of all birds in the subset


#format data
subset.ox.foL <- subset.ox.foL %>% mutate(date.time = ymd_hms(paste(subset.ox.foL$Date, subset.ox.foL$Time, sep = " ")))
subset.ox.foL <- arrange(subset.ox.foL, date.time)
subset.ox.foL <-  mutate(subset.ox.foL,formatted.time = as.numeric(subset.ox.foL$date.time - subset.ox.foL$date.time[1]))
subset.ox.foL <-  select(subset.ox.foL, -Date, -Time)

View(subset.ox.foL)

#Create network
assocFemalesOpenMaze <- gmmevents(time = subset.ox.foL$formatted.time, identity = subset.ox.foL$Letter, location = subset.ox.foL$Location) #assign birds into groups
network.ox.foL <- get_network(assocFemaleOpenMaze[[1]]) #Get association matrix
inetwork.ox.foL <- graph_from_adjacency_matrix(network.ox.foL, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics.foL <- nodesfoL %>% mutate(degree = degree(inetwork.ox.foL, v = Letter),
                                            strength = strength(inetwork.ox.foL, v = Letter),
                                            betweenness = betweenness(inetwork.ox.foL, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.foL, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesO/FemalesLeftMaze.csv", row.names = FALSE)


graph.ox.Females.OL <- ggraph(inetwork.ox.foL, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name, color = "white")) +
  ggtitle("Females Open Right feeder")+
  theme_graph()+
  theme(legend.position = "none")
graph.ox.Females.OL
