library(dplyr)
library(readr)
require(lubridate)
require(igraph)
require(asnipe)
require(ggraph)
require(chron)

rm(list=ls()) 


getwd()
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO")


##### MALES RIGHT SIDE OPEN
rrfidmo <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/RightRFIDMalesO", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(rrfidmo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedRRFIDmo.csv")

#read in the csv
rrfidmo <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedRRFIDmo.csv", sep = ";", header = TRUE, quote = "")
names(rrfidmo) <- gsub("\\.", "", names(rrfidmo)) #remove full stops from column names
rrfidmo$Location <- "Right" #add location


View(rrfid)
names(rOrfid)
#remove first weird column and remove any duplication, and save as a new data frame
rrfidmo <- select(rrfidmo, -XIdentifier)
rrfidmo <- rrfidmo[!duplicated(rrfidmo),]

#check how many times each bird is recorded over the time period
rrfid2 %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))

unique(rOrfid$Date)

##### MALES LEFT SIDE OPEN

lrfidmo <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/LeftRFIDMalesO", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(lrfidmo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedLRFIDmo.csv")

#read in the csv
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO")
lrfidmo<- read.csv("combinedLRFIDmo.csv", sep = ";", header = TRUE, quote = "")
names(lrfidmo) <- gsub("\\.", "", names(lrfidmo)) #remove full stops from column names
lrfidmo$Location <- "Left"

#remove first weird column and remove any duplication, and save as a new data frame
lrfidmo <- select(lrfidmo, -XIdentifier)
lrfidmo <- lrfidmo[!duplicated(lrfidmo),]


##### Combine Left and Right final CSVs

crfidmo <- rbind(rrfidmo, lrfidmo)
crfidmo <- crfidmo[!duplicated(crfidmo),]
crfidmo2 <- select(crfidmo, Date, Time, Transpondercode, Location)
nrow(cOrfid2)
unique(cOrfid$Date)

#reading as csv to round time
write.csv(crfidmo2, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedCRFIDmo2.csv")
#Here I have to round time in excel as I cant get it to work in R!
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO")
rfidmo<- read.csv("combinedCRFIDmo2.csv")
rfidmo <- select(rfidmo, -X, -Time)

nrow(rfidmo)
rfidmo <- rfidmo[!duplicated(rfidmo),]
nrow(rfidmo)


rfidmo$Date <- dmy(rfidmo$Date)
unique(rfidmo$Date)

###### Test OxU algorithm ------------------------------------------------------
subset.ox.mo <- rfidmo %>% filter(date(Date) >= "2021-05-11" & date(Date) <= "2021-06-02")

nodesmo <- subset.ox.mo %>% distinct(Transpondercode) #list of all birds in the subset


#format data
View(subset.ox.Males)
subset.ox.mo <- subset.ox.mo %>% mutate(date.time = ymd_hms(paste(subset.ox.mo$Date, subset.ox.mo$RoundTime, sep = " ")))
arrange(subset.ox.mo, date.time)
subset.ox.mo <-  mutate(subset.ox.mo,formatted.time = as.numeric(subset.ox.mo$date.time - subset.ox.mo$date.time[1]))
subset.ox.mo <-  select(subset.ox.mo, -Date, -RoundTime)


#Create network
assoc <- gmmevents(time = subset.ox.mo$formatted.time, identity = subset.ox.mo$Transpondercode, location = subset.ox.mo$Location) #assign birds into groups
network.ox.mo <- get_network(assoc[[1]]) #Get association matrix
inetwork.ox.mo <- graph_from_adjacency_matrix(network.ox.mo, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
inetwork.metrics.mo <- nodesmo %>% mutate(degree = degree(inetwork.ox.mo, v = Transpondercode),
                                     strength = strength(inetwork.ox.mo, v = Transpondercode),
                                     betweenness = betweenness(inetwork.ox.mo, v = Transpondercode, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/MalesONetworkTest.csv", row.names = FALSE)


V(inetwork.ox.mo)$ID <-1:18

inetwork.ox.mo

#SRI - % of time spent together

# Plot graphs -------------------------------------------------------------

graph.ox.Males.O <- ggraph(inetwork.ox.mo, 'stress') +
  geom_edge_link() +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = V(inetwork.ox.mo)$ID), colour = "white") +
  ggtitle("Males O")
graph.ox.Males.O

geom_node_text(aes(label = name), # add node labels
               colour = 'white')+

?LETTERS
?geom_node_point
install.packages("ggnetwork")
require(ggnetwork)
require(ggplot2)
View(inetwork.ox.mo)
yes

got_palette <- c("#FFFFFF", "#999999", "#FF9900", "#FF3300", "#99CC00", "FFFF00", 
                 "660000", ""
                 )

better.graph <- ggraph(inetwork.ox.mo,layout = "stress")+
  geom_edge_link0(aes(edge_width = weight),edge_colour = "grey66")+
  geom_node_point(aes(fill = inetwork.ox.mo$Transpondercode),shape=21)+
  geom_node_text(aes(size = 26, label = name),repel = TRUE, cex = 1.5)+
  scale_fill_manual(values = got_palette)+
  scale_edge_width(range = c(0.2,3))+
  scale_size(range = c(1,6))+
  theme_graph()+
  theme(legend.position = "none")

ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal() +
  geom_node_text(aes( label=name, filter=leaf, color=group) , angle=90 , hjust=1, nudge_y=-0.1) +
  geom_node_point(aes(filter=leaf, size=value, color=group) , alpha=0.6) +
  ylim(-.6, NA) +
  theme(legend.position="none")

better.graph

ggraph(inetwork.ox)
ggsave(graph.ox, filename = "/Users/charlottecooper/Desktop/Masters/Dissertation/Data/MalesSFinal/MalesSNetworkTest.png")



