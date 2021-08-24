#SEPARATE NETWORKS
getwd()
setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesOSep")


##### MALES RIGHT SIDE OPEN

rrfidmo <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/RightRFIDMalesO", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(rrfidmo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedRRFIDmo.csv")
rrfidmo <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedRRFIDmo.csv", sep = ";", header = TRUE, quote = "")
write.csv(rrfidmo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedRRFIDmo.csv")

#Here I have to round time in excel as I cant get it to work in R!

#read in the csv

rrfidmo <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedRRFIDmo.csv")
names(rrfidmo) <- gsub("\\.", "", names(rrfidmo)) #remove full stops from column names

View(rrfidmo)
unique(rrfidmo$Transpondercode)
rrfidmo <- rrfidmo[-c(8381), ]
rrfidmo$Location <- "Right"

rrfidmo %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))

rrfidmo <- mutate(rrfidmo, Letter = case_when(
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


#remove first weird column and remove any duplication, and save as a new data frame
rrfidmo <- select(rrfidmo, Date, RoundTime, Transpondercode, Location, Letter)
nrow(rrfidmo)
rrfidmo <- rrfidmo[!duplicated(rrfidmo),]
nrow(rrfidmo)


rrfidmo$Date <- dmy(rrfidmo$Date)

unique(rrfidmo$Date)
###### Test OxU algorithm ------------------------------------------------------
subset.ox.moR <- rrfidmo %>% filter(date(Date) >= "2021-05-10" & date(Date) <= "2021-06-02")

nodesmoR <- subset.ox.moR %>% distinct(Letter) #list of all birds in the subset

#format data
subset.ox.moR <- subset.ox.moR %>% mutate(date.time = ymd_hms(paste(subset.ox.moR$Date, subset.ox.moR$RoundTime, sep = " ")))
subset.ox.moR <- arrange(subset.ox.moR, date.time)
subset.ox.moR <-  mutate(subset.ox.moR,formatted.time = as.numeric(subset.ox.moR$date.time - subset.ox.moR$date.time[1]))
subset.ox.moR <-  select(subset.ox.moR, -Date, -RoundTime)


#Create network
assocMaleRightOpen <- gmmevents(time = subset.ox.moR$formatted.time, identity = subset.ox.moR$Letter, location = subset.ox.moR$Location) #assign birds into groups
network.ox.moR <- get_network(assocMaleRightOpen[[1]]) #Get association matrix
inetwork.ox.moR <- graph_from_adjacency_matrix(network.ox.moR, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph

inetwork.metrics.moR <- nodesmoR %>% mutate(degree = degree(inetwork.ox.moR, v = Letter),
                                          strength = strength(inetwork.ox.moR, v = Letter),
                                          betweenness = betweenness(inetwork.ox.moR, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.moR, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/MalesRightOpen.csv", row.names = FALSE)

save(assocMaleRightOpen, file = "assocMaleRightOpen.RData")
load("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesOSep/assocMaleRightOpen.RData")


gsize(inetwork.ox.moR)



##### MALES LEFT SIDE OPEN #####

lrfidmo <- list.files(path="/Users/charlottecooper/Desktop/Masters/Dissertation/Data/LeftRFIDMalesO", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
write.csv(lrfidmo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedLRFIDmo.csv")
lrfidmo <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedLRFIDmo.csv", sep = ";", header = TRUE, quote = "")

write.csv(lrfidmo, "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedLRFIDmo.csv")

#Here I have to round time in excel as I cant get it to work in R
#read in the csv
lrfidmo <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/combinedLRFIDmo.csv")
names(lrfidmo) <- gsub("\\.", "", names(lrfidmo)) #remove full stops from column names
lrfidmo$Location <- "Left"

lrfidmo %>% group_by(Transpondercode) %>%
  summarise(count=length(Transpondercode))

lrfidmo <- mutate(lrfidmo, Letter = case_when(
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

#remove first weird column and remove any duplication, and save as a new data frame
lrfidmo <- select(lrfidmo, Date, RoundTime, Transpondercode, Location, Letter)
lrfidmo <- lrfidmo[!duplicated(lrfidmo),]

nrow(lrfidmo)

lrfidmo$Date <- dmy(lrfidmo$Date)

unique(lrfidmo$Date)
###### Test OxU algorithm ------------------------------------------------------
subset.ox.moL <- lrfidmo %>% filter(date(Date) >= "2021-05-11" & date(Date) <= "2021-06-02")

nodesmoL <- subset.ox.moL %>% distinct(Letter) #list of all birds in the subset


#format data
subset.ox.moL <- subset.ox.moL %>% mutate(date.time = ymd_hms(paste(subset.ox.moL$Date, subset.ox.moL$RoundTime, sep = " ")))
subset.ox.mol <- arrange(subset.ox.moL, date.time)
subset.ox.moL <-  mutate(subset.ox.moL,formatted.time = as.numeric(subset.ox.moL$date.time - subset.ox.moL$date.time[1]))
subset.ox.moL <-  select(subset.ox.moL, -Date, -RoundTime)


#Create network
assocMaleLeftOpen <- gmmevents(time = subset.ox.moL$formatted.time, identity = subset.ox.moL$Letter, location = subset.ox.moL$Location) #assign birds into groups
network.ox.moL <- get_network(assocMaleLeftOpen[[1]]) #Get association matrix
inetwork.ox.moL <- graph_from_adjacency_matrix(network.ox.moL, weighted = TRUE, mode = "undirected") #Convert it to pass into igraph
#export the bits and bobs
inetwork.metrics.moL <- nodesmoL %>% mutate(degree = degree(inetwork.ox.moL, v = Letter),
                                          strength = strength(inetwork.ox.moL, v = Letter),
                                          betweenness = betweenness(inetwork.ox.moL, v = Letter, directed = TRUE, nobigint = FALSE))
write.csv(inetwork.metrics.moL, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesO/MalesLeftOpen.csv", row.names = FALSE)

save(assocMaleLeftOpen, file = "assocMaleLeftOpen.RData")
load("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesOSep/assocMaleLeftOpen.RData")

gsize(inetwork.ox.moL)

