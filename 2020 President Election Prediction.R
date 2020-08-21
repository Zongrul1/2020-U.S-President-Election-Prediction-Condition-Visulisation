#library
library("ggplot2")
library("RColorBrewer")
library("maptools")
library("plyr")
library("rJava")
#import map data
American_map <-readShapePoly("C:\\Users\\Administrator\\GEOM90007 Assignment1 947539 Zongru Li\\map\\STATES.SHP") #need to modify
AD1 <- American_map@data
AD2 <- data.frame(id=rownames(AD1),AD1)
American_map1 <- fortify(American_map)
American_map_data <- join(American_map1,AD2, type = "full")
American_map_data<-American_map_data[,1:12]
newdata<-read.csv("C:\\Users\\Administrator\\Desktop\\GEOM90007 Assignment1 947539 Zongru Li\\2020 President Election Prediction.csv")  #need to modify
#pre-process the map(Alaska and Hawaii)
data1<-subset(American_map_data,STATE_NAME!='Alaska'& STATE_NAME!='Hawaii')
data2<-subset(American_map_data,STATE_NAME=="Hawaii")    
data3<-subset(American_map_data,STATE_NAME=="Alaska") 
data2$long<-data2$long+65
data2$lat<-data2$lat+5
data3$long<-(data3$long+40)*0.3-78
data3$lat<-(data3$lat-42)*0.3+20
data4<-rbind(data1,data2,data3)
#combine mapdata and data
American_data <- join(data4, newdata, type="full")
midpos <- function(AD1){mean(range(AD1,na.rm=TRUE))} 
centres <- ddply(American_data,.(STATE_ABBR),colwise(midpos,.(long,lat)))
mynewdata<-join(centres,newdata,type="full")

# overall forecast
p1<-ggplot(American_data,aes(x=long,y=lat,group=group,fill=Results))+
  geom_polygon(colour="white")+      scale_fill_manual(values=c("#19609F","#CB1C2A"),labels=c("Biden", "Trump"))+
  coord_map("polyconic") +
  guides(fill=guide_legend(title=NULL))+ 
  labs(title = "Overall Forecast Condition in Each State") +
  theme(         
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position ="top"
  )

#biden's odds
#classify
qa <- quantile(na.omit(American_data$Biden.Odds), c(0,0.5,0.65,0.75,1.0))
#combine
American_data$Biden_q<-cut(American_data$Biden.Odds,qa,labels = c("0-10%","10-50%","50-80%", "80-100%"),include.lowest = TRUE)
#draw the map
p2<-ggplot(American_data,aes(long,lat,group=group,fill=Biden_q))+
  geom_polygon(colour="white")+
  scale_fill_brewer(palette="Blues")+
  coord_map("polyconic") +
  labs(title = "Biden's Odds of Winning in Each State") +
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+ 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.9,0.2),
    legend.text.align=1
  ) 

#trump's odds
#classfiy
qb <- quantile(na.omit(American_data$Trump.Odds), c(0,0.2,0.4,0.5,1.0))
#combine
American_data$Trump_q<-cut(American_data$Trump.Odds,qb,labels = c("0-10%","10-50%","50-80%", "80-100%"),include.lowest = TRUE)
#draw the map
p3<-ggplot(American_data,aes(long,lat,group=group,fill=Trump_q))+
  geom_polygon(colour="white")+
  scale_fill_brewer(palette="Reds")+
  coord_map("polyconic") +
  labs(title = "Trump's Odds of Winning in Each State") +
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+ 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.9,0.2),
    legend.text.align=1
  ) 

#compare the electoral vote
#preprocess
value<-aggregate(x=newdata[c('Count')], by = list(newdata$Results), FUN=sum)
colnames(value)<-NULL
value<-t(value)
value<-value[2,]
specie <- c(rep("" , 2))
Nominee <- rep(c("Biden" , "Trump") , 1)
data <- data.frame(specie,condition,value)

#draw the bar chart
p4<-ggplot(data, aes(fill=Nominee, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#19609F","#CB1C2A")) +
  xlab("Electoral Vote") +
  ylab("Numbers") +
  labs(title = "Trump vs Biden in Electoral Vote") +
  geom_hline(yintercept = 270/188,,colour='orange',size = 1) +
  annotate("text", label = "270 to Win", x = 1, y = 298/188, size = 4, colour = "orange") +
  annotate("text", label = "188", x = 1, y = 480/188, size = 8, colour = "white") +
  annotate("text", label = "370", x = 1, y = 200/188, size = 8, colour = "white") +
  theme(plot.title = element_text(hjust = 0.5))


# compare the Popular Vote
#preprocess
value1<-apply(newdata[c('Biden.Votes')],2,sum)
colnames(value1)<-NULL
print(value1)
value2<-apply(newdata[c('Trump.Votes')],2,sum)
colnames(value2)<-NULL
print(value2)
value = rbind (value1,value2)
specie <- c(rep("" , 2))
Nominee <- rep(c("Biden" , "Trump") , 1)
data <- data.frame(specie,condition,value)
Nominee = c("Biden", "Trump")
#draw the bar chart
p5<-ggplot(data, aes(x=Nominee, y=value,fill = Nominee)) + 
  geom_bar(stat = "identity", width=0.4) +
  scale_fill_manual(values=c("#19609F","#CB1C2A")) +
  xlab("Total Vote") +
  ylab("Numbers")+
  labs(title = "Trump vs Biden in Popular Vote") +
  theme(plot.title = element_text(hjust = 0.5)) +
  options(scipen=200)


#draw the layout
#set the layout
require(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5,2)))
vplayout = function(x,y)viewport(layout.pos.row = x,layout.pos.col = y)
grid.text("GEOM90007 Assignment 1 : R graphics", vp = vplayout(1,1:2),y = 0.6)
grid.text("2020 U.S President Election Prediction Condition Visulisation", vp = vplayout(1,1:2), y = 0.5)
grid.text("947539", vp = vplayout(1,1:2), y = 0.4)
grid.text("Zongru Li", vp = vplayout(1,1:2), y = 0.3)
#place each figures on the layout
print(p1,vp = vplayout(2:3,1:2))
print(p2,vp = vplayout(4,1))
print(p3,vp = vplayout(4,2))
print(p4,vp = vplayout(5,1))
print(p5,vp = vplayout(5,2))


