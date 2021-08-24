install.packages("plotrix")
library(plotrix)
##### Male graphs

#Maze right

graph.ox.Males.SR <- ggraph(inetwork.ox.msR, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Males Maze Right feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.SR


#Maze left

graph.ox.Males.SL  <- ggraph(inetwork.ox.msL, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Males Maze Left feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.SL


#Open right

graph.ox.Males.OR <- ggraph(inetwork.ox.moR, 'stress') +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour='white') +
  ggtitle("Males Open Right feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.OR

#Open left

graph.ox.Males.OL <- ggraph(inetwork.ox.moL, 'stress') +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(size = 7, shape=21, fill="black", colour="black") + 
  geom_node_text(aes(label = name), colour = 'white') +
  ggtitle("Males Open Left feeder")+
  theme(legend.position = "none", plot.title = element_text(size=15))
graph.ox.Males.OL

?ggtitle

?geom_node_text
#combine
MaleGraphs <- ggarrange(graph.ox.Males.SL, graph.ox.Males.SR, graph.ox.Males.OL, graph.ox.Males.OR + rremove("x.text"),
          ncol = 2, nrow = 2)


##### male repeatability
install.packages("lme4")
require(lme4)

setwd("/Users/charlottecooper/shrubs-hub/SparrowsScripts/MalesRep")
MalesSNetwork <- read.csv("MalesSNetwork.csv")

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


library(lme4)
mM <- lmer(strength ~1+(1|Letter), data=MalesR)
summary(mM)


ggplot(MalesR,aes(x=Letter,y=strength,col=Type)) +
  geom_boxplot()+
  theme(text = element_text(size=15),)+
  labs(x = "\nBird ID", y = "Interaction stength\n", colour = "Setup", title = "Strength - Male networks")+
  theme(strip.text=element_text(size=15))+
  theme(plot.margin=unit(c(0.5,0.5,0.75,0.6), "cm"))
  
ggplot(MalesR,aes(x=Letter,y=degree,col=Type)) +
  geom_boxplot()+
  theme(text = element_text(size=15),)+
  labs(x = "\nBird ID", y = "Degree\n", colour = "Setup", title = "Degree - Male networks")+
  theme(strip.text=element_text(size=15))+
  theme(plot.margin=unit(c(0.5,0.5,0.75,0.6), "cm"))


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



#SIR Males Maze


males_isv <- c(S=20, I=1, R=0)
parameters <- c(gamma=0.2, beta=0.2)
time<-seq(from=1,to=100,by=1)
sir_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+I+R
    lambda=beta*(I/N)
    dS=-lambda*S
    dI=lambda*S-gamma*I
    dR=gamma*I
    
    return(list(c(dS,dI,dR)))
  })
}

output <- as.data.frame(ode(y=males_isv, func=sir_model, parms=parameters,times=time))
out_long<- melt(output, id="time")

ggplot(data = out_long,
       aes(x = time, y = value/20, colour = variable, group = variable)) + 
  geom_line()+xlab("Time (days)")+ylab("Proportion of population")+scale_color_discrete(name="State")
