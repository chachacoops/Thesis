install.packages("plotrix")
library(plotrix)
library(MCMCglmm)
##### Male graphs

#Maze right network plot

graph.ox.Males.SR <- ggraph(inetwork.ox.msR, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Males Maze Right feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.SR


#Maze left network plot

graph.ox.Males.SL  <- ggraph(inetwork.ox.msL, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Males Maze Left feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.SL


#Open right network plot

graph.ox.Males.OR <- ggraph(inetwork.ox.moR, 'stress') +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour='white') +
  ggtitle("Males Open Right feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.OR

#Open left network plot

graph.ox.Males.OL <- ggraph(inetwork.ox.moL, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Males Open Left feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.OL

#combine into one plot
MaleGraphs <- ggarrange(graph.ox.Males.SL, graph.ox.Males.SR, graph.ox.Males.OL, graph.ox.Males.OR + rremove("x.text"),
          ncol = 2, nrow = 2)


##### male repeatability



#load in data
MalesRep <- list.files(path="/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesRep", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

View(MalesRep)
MalesRep %>% group_by(Letter) %>%
  summarise(count=length(Letter))

MalesRep <- MalesRep[-c(27),]




MalesORep <- list.files(path="/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesORep", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

##### Males t test


MalesMR <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesRep/MalesRightMaze.csv")
MalesML <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesRep/MalesLeftMaze.csv")

MalesMaze <- rbind(MalesMR, MalesML)
MalesMaze$Type <- "Maze"
View(MalesMaze)
MalesOR <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesRep/MalesRightOpen.csv")
MalesOL <- read.csv("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesRep/MalesLeftOpen.csv")

MalesOpen <- rbind(MalesOR, MalesOL)
MalesOpen$Type <- "Open"
MalesR <- rbind(MalesMaze, MalesOpen)

write.csv(MalesR, file = "/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesRep/MalesR.csv", row.names = FALSE)

#calculate repeatability of strength

mcmcMales <- MCMCglmm(Strength~1, random=~ID, data=MalesR, nitt=100000, burnin=50000)
autocorr(mcmcMales$VCV)
R1 <- mcmcMales$VCV[,"ID"]/(mcmcMales$VCV[,"ID"]+mcmcMales$VCV[,"units"])
posterior.mode(R1)
HPDinterval(R1)


#Male maze descriptives
range(MalesMaze$strength)
mean(MalesMaze$strength)
std.error(MalesMaze$strength)
var(MalesMaze$strength)

range(MalesMaze$degree)
mean(MalesMaze$degree)
std.error(MalesMaze$degree)
var(MalesMaze$degree)


range(MalesMaze$betweenness)
mean(MalesMaze$betweenness)
std.error(MalesMaze$betweenness)
var(MalesMaze$betweenness)

#Male open descriptives
range(MalesOpen$strength)
mean(MalesOpen$strength)
std.error(MalesOpen$betweenness)
var(MalesOpen$strength)

range(MalesOpen$degree)
mean(MalesOpen$degree)
std.error(MalesOpen$degree)
var(MalesOpen$degree)

range(MalesOpen$betweenness)
mean(MalesOpen$betweenness)
std.error(MalesOpen$be)
var(MalesOpen$betweenness)

#male ML descriptives
range(MalesML$strength)
mean(MalesML$strength)
std.error(MalesML$strength)
var(MalesML$strength)

range(MalesML$degree)
mean(MalesML$degree)
std.error(MalesML$degree)
var(MalesML$degree)

range(MalesML$betweenness)
median(MalesML$betweenness)
std.error(MalesML$betweenness)
var(MalesML$betweenness)


#male MR descriptives
range(MalesMR$strength)
mean(MalesMR$strength)
std.error(MalesMR$strength)
var(MalesMR$strength)

range(MalesMR$degree)
median(MalesMR$degree)
std.error(MalesMR$degree)
var(MalesMR$degree)

range(MalesMR$betweenness)
mean(MalesMR$betweenness)
std.error(MalesMR$betweenness)
var(MalesMR$betweenness)


#male OR descriptives
range(MalesOR$strength)
mean(MalesOR$strength)
std.error(MalesOR$strength)
var(MalesOR$strength)

range(MalesOR$degree)
median(MalesOR$degree)
std.error(MalesOR$degree)
var(MalesOR$degree)

range(MalesOR$betweenness)
mean(MalesOR$betweenness)
std.error(MalesOR$betweenness)
var(MalesOR$betweenness)



#male OL descriptives
range(MalesOL$strength)
mean(MalesOL$strength)
std.error(MalesOL$strength)
var(MalesOL$strength)

range(MalesOL$degree)
median(MalesOL$degree)
std.error(MalesOL$degree)
var(MalesOL$degree)

range(MalesOL$betweenness)
median(MalesOL$betweenness)
std.error(MalesOL$betweenness)
var(MalesOL$betweenness)



#t.tests
hist(MalesR$degree)

wilcox.test(MalesMaze$strength, MalesOpen$strength)
wilcox.test(MalesMR$strength, MalesOR$strength)
wilcox.test(MalesML$strength, MalesOL$strength)

wilcox.test(MalesMaze$degree, MalesOpen$degree, exact = F)
wilcox.test(MalesMR$degree, MalesOR$degree, exact = F)
wilcox.test(MalesML$degree, MalesOL$degree, exact = F)


wilcox.test(MalesMaze$betweenness, MalesOpen$betweenness, exact = F)
wilcox.test(MalesMR$betweenness, MalesOR$betweenness, exact = F)
wilcox.test(MalesML$betweenness, MalesOL$betweenness, exact = F)



