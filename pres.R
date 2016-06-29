library(ggplot2)


pres.full<-read.csv("pres_data.csv")

#Unrun code to show the various IMDB pages.
#for (j in 1:43){
#  system(paste('open http://www.imdb.com/character/',pres.full$IMDBCharNum[j],'/filmotype#main',sep=''))
#  }


#Subest out the rank data
pres.rankings <- pres.full[1:43, c(5:26)]
pres.rankings.nc <- dim(pres.rankings)[2]


#Build a matrix of distance between the rankings using scaled Kendall
dist.mat <- mat.or.vec(pres.rankings.nc, pres.rankings.nc)

for (i in 1:pres.rankings.nc){
  for (j in 1:pres.rankings.nc){
  #dist.mat[i,j] <- ConDisPairs(table(pres.rankings[,i],pres.rankings[,j]))$D
    dist.mat[i,j] <- (1-cor(pres.rankings[,i],pres.rankings[,j],use="pairwise.complete.obs", method="k"))/2
  }
}

colnames(dist.mat) <- colnames(pres.rankings)
rownames(dist.mat) <- colnames(dist.mat)


#Shamelessly use the code from the cmdscale help page
#Note the when we add in stuff like height, then Kruskal scaling collapses all the survey rankings
#Classical scaling is right, I think.
loc <- cmdscale(as.dist(dist.mat))

pres.plot <- NULL

pres.plot$x <- loc[, 1]
pres.plot$y <- -loc[, 2] #The minus sign is a copy & paste thing (although it doesn't really matter.)
pres.plot <- as.data.frame(pres.plot)

#Krukal scaling
#loc.k <- isoMDS(as.dist(dist.mat))

#Produce the graph that shows that popular presidents are in lots of movies
pres.full.s<-data.frame(pres.full$Name,pres.full$num.of.movies,pres.full$Schl..1996)
colnames(pres.full.s) <- c("Name","num.of.movies","S1996.rating")
pres.full.s <- pres.full.s[complete.cases(pres.full.s),]
pres.full.s$Name <- factor(pres.full.s$Name, levels = pres.full.s$Name[order(pres.full.s$S1996.rating)])

g.bar <- ggplot(data=pres.full.s, aes(x=Name,y=num.of.movies)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0)) + 
  xlab("Presidents ranked by Schlesinger 1996") +
  ylab("Number of movie appearances")
g.bar


#Produce point clouds that show the traditional rankings clustering together, movie rankings close, and other stuff 
# fra off

g.dots.1 <- ggplot(pres.plot[c(-1,-20,-21,-22),], aes(x,y)) + 
  geom_point() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),legend.position="none",
                          panel.grid.minor=element_blank(),plot.background=element_blank()) +
  annotate("text",x=pres.plot$x[10],y=pres.plot$y[10],label="Schlesinger 1996",hjust=0) +
  annotate("text",x=pres.plot$x[12],y=pres.plot$y[12],label="Wall Street Journal 2000",hjust=0)
g.dots.1

g2 <- ggplot(pres.plot[c(-1,-20,-21),], aes(x,y))
g.dots.2 <- ggplot(pres.plot[c(-1,-20,-21),], aes(x,y)) + 
  geom_point(alpha=.4) + 
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),legend.position="none",
                          panel.grid.minor=element_blank(),plot.background=element_blank()) +
  annotate("text",x=pres.plot$x[10],y=pres.plot$y[10],label=" Schlesinger 1996",hjust=0) +
  annotate("text",x=pres.plot$x[12],y=pres.plot$y[12],label=" Wall Street Journal 2000",hjust=0) +
  annotate("text",x=pres.plot$x[21],y=pres.plot$y[21],label="Height ",hjust=1) +
  annotate("text",x=pres.plot$x[22],y=pres.plot$y[22],label="Length of last name ",hjust=1)
g.dots.2

g.dots.3 <- ggplot(pres.plot[c(-20),], aes(x,y)) + 
                geom_point(alpha=.4) + 
                theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                        axis.text.y=element_blank(),axis.ticks=element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.title.y=element_blank(),legend.position="none",
                                        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  annotate("text",x=pres.plot$x[10],y=pres.plot$y[10],label=" Schlesinger 1996",hjust=0) +
  annotate("text",x=pres.plot$x[12],y=pres.plot$y[12],label=" Wall Street Journal 2000",hjust=0) +
  annotate("text",x=pres.plot$x[22],y=pres.plot$y[22],label="Length of last name ",hjust=1) +
  annotate("text",x=pres.plot$x[21],y=pres.plot$y[21],label="Height ",hjust=1) +
  annotate("text",x=pres.plot$x[1],y=pres.plot$y[1],label=" Movie protrayals",hjust=0)
g.dots.3


