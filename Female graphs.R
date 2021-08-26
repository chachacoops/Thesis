
require(lme4)
graph.ox.Females.SL  <- ggraph(inetwork.ox.fsL, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = "white") +
  ggtitle("Females Maze Left feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Females.SL

graph.ox.Females.SR <- ggraph(inetwork.ox.fsR, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Females Maze Right feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Females.SR 

graph.ox.Females.OR <- ggraph(inetwork.ox.foR, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Females Open Right feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Females.OR



FemaleGraphs <- ggarrange(graph.ox.Females.SL, graph.ox.Females.SR, graph.ox.Females.OR + rremove("x.text"),
          ncol = 2, nrow = 2)
FemaleGraphs

##### female repeatability

setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesRep")

FemalesRep <- list.files(path="/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesRep", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

FemalesRep %>% group_by(Letter) %>%
  summarise(count=length(Letter))

FemalesRep <- FemalesRep[-c(21),]

FeRep <- lmer(strength ~1+(1|Letter), data=FemalesRep)

##### Females t-test


##### Females t test

FemalesMR <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesRep/FemalesRightMaze.csv")
FemalesML <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesRep/FemalesLeftMaze.csv")

FemalesOR <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/FemalesRep/FemalesRightOpen.csv")

#There's only one bird in the left Open network, so I've made a data frame with 0s for strength, betweenness, and degree
Letter <- c("M")
degree <- c(0)
strength <- c(0)
betweenness <- c(0)
Type <- c("Open")
FemalesOL <- data.frame(Letter, degree, strength, betweenness)

View(FemalesOL)
View(FemalesML)

FemalesOpen <- rbind(FemalesOL, FemalesOR)
FemalesOpen$Type <- "Open"
FemalesMaze <- rbind(FemalesML, FemalesMR)
FemalesMaze$Type <- "Maze"

FemalesR <- rbind(FemalesOpen, FemalesMaze)
hist(FemalesR$betweenness)

wilcox.test(FemalesMaze$strength, FemalesOpen$strength)
wilcox.test(FemalesMR$strength, FemalesOR$strength)
wilcox.test(FemalesML$strength, FemalesOL$strength)

wilcox.test(FemalesMaze$degree, FemalesOpen$degree, exact = F)
wilcox.test(FemalesMR$degree, FemalesOR$degree, exact = F)
wilcox.test(FemalesML$degree, FemalesOL$degree, exact = F)

wilcox.test(FemalesMaze$betweenness, FemalesOpen$betweenness, exact=F)
wilcox.test(FemalesMR$betweenness, FemalesOR$betweenness,exact=F)
wilcox.test(FemalesML$betweenness, FemalesOL$betweenness,exact=F)


View(FemalesOR)

ggplot(FemalesR,aes(x=Letter,y=strength,col=Type)) + geom_point()
ggplot(FemalesR,aes(x=Letter,y=strength,col=Type)) +
  geom_boxplot()+
  theme(text = element_text(size=15),)+
  labs(x = "\nBird ID", y = "Interaction stength\n", colour = "Setup", title = "Strength - Female networks")+
  theme(strip.text=element_text(size=15))+
  theme(plot.margin=unit(c(0.5,0.5,0.75,0.6), "cm"))

ggplot(FemalesR,aes(x=Letter,y=degree,col=Type)) +
  geom_boxplot()+
  theme(text = element_text(size=15),)+
  labs(x = "\nBird ID", y = "Degree\n", colour = "Setup", title = "Degree - Female networks")+
  theme(strip.text=element_text(size=15))+
  theme(plot.margin=unit(c(0.5,0.5,0.75,0.6), "cm"))

#female maze descriptives
mean(FemalesMaze$strength)
range(FemalesMaze$strength)
var(FemalesMaze$strength)
std.error(FemalesMaze$strength)

mean(FemalesMaze$degree)
range(FemalesMaze$degree)
var(FemalesMaze$degree)
std.error(FemalesMaze$degree)

mean(FemalesMaze$betweenness)
range(FemalesMaze$betweenness)
var(FemalesMaze$betweenness)
std.error(FemalesMaze$betweenness)

#female open descriptives
mean(FemalesOpen$strength)
range(FemalesOpen$strength)
var(FemalesOpen$strength)
std.error(FemalesOpen$strength)

median(FemalesOpen$degree)
range(FemalesOpen$degree)
var(FemalesOpen$degree)
std.error(FemalesOpen$degree)

mean(FemalesOpen$betweenness)
range(FemalesOpen$betweenness)
var(FemalesOpen$betweenness)
std.error(FemalesOpen$betweenness)


#female ML descriptives
mean(FemalesML$strength)
range(FemalesML$strength)
var(FemalesML$strength)
std.error(FemalesML$strength)

mean(FemalesML$degree)
range(FemalesML$degree)
var(FemalesML$degree)
std.error(FemalesML$degree)

mean(FemalesML$betweenness)
range(FemalesML$betweenness)
var(FemalesML$betweenness)
std.error(FemalesML$betweenness)

#female MR descriptives
mean(FemalesMR$strength)
range(FemalesMR$strength)
var(FemalesMR$strength)
std.error(FemalesMR$strength)

mean(FemalesMR$degree)
range(FemalesMR$degree)
var(FemalesMR$degree)
std.error(FemalesMR$degree)

mean(FemalesMR$betweenness)
range(FemalesMR$betweenness)
var(FemalesMR$betweenness)
std.error(FemalesMR$betweenness)

View(FemalesMR)

#female OR descriptives
mean(FemalesOR$strength)
range(FemalesOR$strength)
var(FemalesOR$strength)
std.error(FemalesOR$strength)

mean(FemalesOR$degree)
range(FemalesOR$degree)
var(FemalesOR$degree)
std.error(FemalesOR$degree)

mean(FemalesOR$betweenness)
range(FemalesOR$betweenness)
var(FemalesOR$betweenness)
std.error(FemalesOR$betweenness)


#female OL descriptives
mean(FemalesOL$strength)
range(FemalesOL$strength)
var(FemalesOL$strength)
std.error(FemalesOL$strength)

mean(FemalesOL$degree)
range(FemalesOL$degree)
var(FemalesOL$degree)
std.error(FemalesOL$degree)

mean(FemalesOL$betweenness)
range(FemalesOL$betweenness)
var(FemalesOL$betweenness)
std.error(FemalesOL$betweenness)






