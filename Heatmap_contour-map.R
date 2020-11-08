#install.packages('maps')
library(maps)
#install.packages('ggmap')
library(ggmap)
library(ggplot2)
library(ggrepel)
library(grid)
library(gridExtra)
library(dplyr)
library(ggExtra)
costco<-read.csv('costcos-geocoded.csv')
head(costco)
map(database = "state", col='#cccccc')
points(x = costco$Longitude, costco$Latitude, pch = 19, col = alpha('orange', 0.4), cex = 1)
title(main = "\nCOSTCO STORES IN THE CONTIGUOUS USA",font.main= 3, col.main= "dimgray", adj=0)


#contour plot
library(tidyverse)
contour<-read.csv('Data_4.csv')
names(contour)[names(contour) == "Team.Against"] <- "Team"
head(contour)
contour<-contour %>% filter(Na.1== 4)
head(contour)
contour1<-contour %>% drop_na(Team)
contour2<-contour1 %>% drop_na(ID2)
contour3<-contour2 %>% drop_na(Result.2)
head(contour3)
PltData <- subset(contour3, Result.2=="lost")
PltData
ggplot(data=contour3,aes(x = X, y = Y))+
  ylim(60,95)+
  xlim(15,22)+
  geom_point(data = contour3, aes (x=X,y=Y, alpha=0.3))+
  geom_density2d(color= 'gray')+
  geom_text_repel(data=PltData,mapping=(aes(x=X,y=Y)),label = PltData$Result.2,
                  size = 3.55, color = 'orange')+
  geom_text_repel(data=PltData,mapping=(aes(x=X,y=Y)),label = PltData$Team,
                  size = 3.55, color = 'orange')+
  ggtitle("\nPASSES AND PERFORMANCE BY TOTTENHAM HOTSPUR\n")+
  labs(x = '\npass length (m)', y = 'pass completion (%)')+
  theme_bw()+  removeGrid()+
  theme(panel.border = element_blank(), title=element_text(color = 'dimgray'), axis.ticks.y = element_blank(), axis.ticks.x = element_blank(),axis.text=element_text(size=11, color = 'dimgray'),axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),  axis.title.y=element_text(size=14, color = 'dimgray'), axis.title.x=element_text(size=14, color = 'dimgray'))+
  theme(axis.line = element_line(color = 'gray'))

#heatmap
library(RColorBrewer)
bbb<-read.csv('ppg2008.csv')
head(bbb)
bbb1<-subset(bbb,select=-c(PF))
head(bbb1)
bbb2<-bbb1[order(bbb1$PTS, decreasing=FALSE),]
row.names(bbb2)<-bbb2$Name
bbb2<-bbb2[,2:20]
head(bbb2)
names(bbb2)[names(bbb2) == "G"] <- "Games"
names(bbb2)[names(bbb2) == "MIN"] <- "Minutes"
names(bbb2)[names(bbb2) == "PTS"] <- "Points"
names(bbb2)[names(bbb2) ==  "FGM"] <- "Field goals"
names(bbb2)[names(bbb2) ==  "FGA"] <- "F.G.Attempts"
names(bbb2)[names(bbb2) == "FGP"] <- "F.G.Percentage"
names(bbb2)[names(bbb2) == "FTM"] <- "Free Throws"
names(bbb2)[names(bbb2) == "FTA"] <- "F.Th.Attempts"
names(bbb2)[names(bbb2) == "FTA"] <- "F.Th.Percentage"
names(bbb2)[names(bbb2) == "X3PM"] <- "Three Pointer"
names(bbb2)[names(bbb2) == "X3PA"] <- "Th.P.Attempts"
names(bbb2)[names(bbb2) == "X3PP"] <- "Th.P.Percentage"
names(bbb2)[names(bbb2) == "ORB"] <- "Offen. Rebounds"
names(bbb2)[names(bbb2) == "DRB"] <- "Defen. Rebounds"
names(bbb2)[names(bbb2) == "TRB"] <- "Total Rebounds"
names(bbb2)[names(bbb2) == "AST"] <- "Assists"
names(bbb2)[names(bbb2) == "STL"] <- "Steals"
names(bbb2)[names(bbb2) == "BLK"] <- "Blocks"
names(bbb2)[names(bbb2) == "TO"] <- "Turnovers"                            
head(bbb2)
matrixb<-data.matrix(bbb2)
HEATMAPB<-heatmap(matrixb, cexCol = 0.7,Rowv = NA, Colv=NA, col=brewer.pal(9,'Blues'),main = "\nNBA Performance", scale='column', margins=c(5,10))
