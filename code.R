library(ggplot2)
library(plyr)
library(gridExtra)
library(ggpubr)
library(ggpmisc)
library(sciplot)
library(Hmisc)
library(RColorBrewer)
library(scales)

setwd("D:/")
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
agglodata<-read.csv("agglo_data_2020.csv", sep=",", header = T)
names(agglodata)[2] <-"ISO"
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Republic of the Congo"]="Congo"
cregion[cregion=="Central African Republic"]="CAF"
agglodata1 <- merge(agglodata, cregion, by="ISO")
names(agglodata1)[3] <-"Population"
names(agglodata1)[4] <-"Builtup"



agglodata4<-data.frame()
i <- "Angola"
for (i in unique(agglodata1$Country))
{
  eachCountry <- agglodata1[which(agglodata1$Country==i),]
  if(nrow(eachCountry)<10)  next()
  
  model <- lm(log10(Builtup)~log10(Population),data = eachCountry)
  slope <-round(coef(model)[2],4)
  intercept <- round(coef(model)[1],3)
  eachCountry$expection <- slope * log10(eachCountry$Population) + intercept
  eachCountry$residual <- log10(eachCountry$Builtup) - eachCountry$expection
  eachCountry$outlier <- 1
  
  #
  q25 <- quantile(eachCountry$residual, 0.25)
  q75 <- quantile(eachCountry$residual, 0.75)
  iqr <- q75 - q25
  # min <- q25 - 1.5 * iqr
  # max <- q75 + 1.5 * iqr
  max <- mean(eachCountry$residual, na.rm = T)+2*sd(eachCountry$residual, na.rm = T)
  min <- mean(eachCountry$residual, na.rm = T)-2*sd(eachCountry$residual, na.rm = T)
  eachCountry[which(eachCountry$residual >= min & eachCountry$residual <= max),]$outlier <- 0
  agglodata2 <- eachCountry[which(eachCountry$outlier==0),]
  numCity <- as.data.frame(table(unlist(agglodata2$ISO)))
  names(numCity)[1] <-"ISO"
  agglodata3 <- merge(agglodata2, numCity, by="ISO")
  agglodata3 <- agglodata3[which(agglodata3$Freq >9),]
  agglodata4 <- rbind(agglodata4, agglodata3)
  print(i)
}

# fig 1 -------------------------------------------------------------------
urbandata<-read.csv("城市人口（分区）.csv", sep=",", header = T)
totaldata<-read.csv("总人口（分区）.csv", sep=",", header = T)


urbanregion<-data.frame()
totalregion<-data.frame()
tempurban<-data.frame()
urbandata1<-data.frame()
temptotal<-data.frame()
totaldata1<-data.frame()
temprate<-data.frame()
rate<-data.frame()
for (i in unique(urbandata$AU_Regions)){
  urbanregion <- urbandata[which(urbandata$AU_Regions==i),]
  totalregion <- totaldata[which(urbandata$AU_Regions==i),]
  for (j in 4:24){
    tempurban<-data.frame(region=i,year=1950+5*(j-4),
                          urbanpop=sum(urbanregion[,j])/1000)
    urbandata1 <- rbind(urbandata1, tempurban)
    temptotal<-data.frame(region=i,year=1950+5*(j-4),
                          totalpop=sum(totalregion[,j])/1000)
    totaldata1 <- rbind(totaldata1, temptotal)
    temprate<-data.frame(region=i,year=1950+5*(j-4),
                         urbanrate=100*sum(urbanregion[,j])/sum(totalregion[,j]))
    rate<-rbind(rate, temprate)
  }
}
#
for (j in 4:24){
  tempurban<-data.frame(region="Africa",year=1950+5*(j-4),
                        urbanpop=sum(urbandata[,j])/1000)
  urbandata1 <- rbind(urbandata1, tempurban)
  temptotal<-data.frame(region="Africa",year=1950+5*(j-4),
                        totalpop=sum(totaldata[,j])/1000)
  totaldata1 <- rbind(totaldata1, temptotal)
  temprate<-data.frame(region="Africa",year=1950+5*(j-4),
                       urbanrate=sum(urbandata[,j])/sum(totaldata[,j]))
  rate<-rbind(rate, temprate)
}

#
plot1 <- ggplot(urbandata1, aes(year, urbanpop, colour=factor(region)))+
  geom_rect(
    aes(
      xmin = 2025,
      xmax = 2050,
      ymin = 0,
      ymax = 520
    ),
    fill = "#C4DBEA", # 
    color = NA,
    alpha=0.05# 
  )+
  geom_point(size=1.2, shape=19,stroke = 0.1, alpha=0.8)+
  geom_smooth(size=0.7,fill="white")+
  #scale_color_manual(values=c('#D35C7C','#527A71',  '#B8967A', '#4A5A69' ,'776081','727F65'))+
  scale_color_brewer(palette = "Set2")+
  labs (x="Year", 
        y="Urban Population (million)")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =6), 
         axis.title = element_text(size=7))+
  theme (legend.position = c (0.35,0.85),
         legend.background = element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =6),
         legend.title=element_blank())
#+scale_color_discrete(breaks=c("Southern Africa","Central Africa","Northern Africa","Eastern Africa","Western Africa"))

plot1



#add global urbanization
world<-read.csv("world urbanization.csv", sep=",", header = T)
rate<-rbind(rate, world)

# 
Dark2_5<- brewer.pal(5, "Dark2")
print(Dark2_5)
#"#1B9E77" "#D95F02" "#7570B3" "#E7298A" "#66A61E"

plot2 <- ggplot(rate, aes(year, urbanrate, colour=factor(region)))+
  geom_rect(
    aes(
      xmin = 2025,
      xmax = 2050,
      ymin = 0,
      ymax = 80
    ),
    fill = "#FFE6FFFF", 
    color = NA,
    alpha=0.05
  )+
  geom_point(size=1.2, shape=19,stroke = 0.1, alpha=0.8)+
  geom_smooth(size=0.7,fill="white")+
  scale_color_manual(values=c("#1B9E77","#D95F02", "#7570B3" ,"#E7298A", "#66A61E","black"))+
  #scale_color_brewer(palette = "Dark2")+
  labs (x="Year", 
        y="Urbanization level (%)")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =6), 
         axis.title = element_text(size=7))+
  theme (legend.position = c (0.35,0.85),
         legend.background = element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =6),
         legend.title=element_blank())
#+scale_color_discrete(breaks=c("Southern Africa","Central Africa","Northern Africa","Eastern Africa","Western Africa"))
plot2





newurban<-read.csv("新增人口占比.csv", sep=",", header = T)

newplot<- ggplot(newurban, aes(year, new_a/new_w))+
  geom_point(size=2, shape=19, stroke = 0.3,color="#F3C846")+
  geom_line(aes(year, new_a/new_w),
            colour="#4A7298",size=1)+
  scale_x_continuous(breaks = scales::breaks_width(20))+
  scale_y_continuous(labels = scales::percent)+
  labs(x="Year", 
       y="Proportion")+  #"7-day average cases \n(thousand)"
  theme (axis.text=element_text(size =6), 
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))

newplot


tiff(file="Figure1 bcd.tiff", res = 600, width = 3543, height = 1062, compression = "lzw")
ggarrange(plot1,plot2,newplot,
          ncol=3,nrow=1, 
          #labels = c("  b. Central Africa","c. Eastern Africa","d. Northern Africa","e. Southern Africa","f. Western Africa"),
          # font.label = list(size = 7, color = "black", face = "bold", family = NULL),
          # label.x = 0.28,label.y=0.97,
          align="v") 
dev.off()


# fig 2 --------------------------------------------------------------------
zdata<-read.csv("1950-2020城市人口及排名.csv", sep=",", header = T)
zdata2<-data.frame(zdata,
                   lnPop = log10(zdata$population),
                   lnRank = log10(zdata$rank-1/2))


crossexponent<-data.frame()
i <- 1950
for (i in unique(zdata2$year))
{
  eachyear <- zdata2[which(zdata2$year==i),]
  #if(i=="Equatorial Guinea"|i=="South Sudan")  next()
  
  mod <- lm(eachyear$lnRank ~ eachyear$lnPop)
  summary (mod)
  r_square<-round(summary(mod)$r.squared,3)
  slope<-round(coef(mod)[2],3)
  intercept <- round(coef(mod)[1],3)
  low <- round(confint.lm(mod)[2],4)
  up <- round(confint.lm(mod)[4],4)
  tempdata<- data.frame(exponent=slope, low2.5=low, up97.5=up, 
                        r_square=  r_square,  intercept=  intercept, Year =i)
  crossexponent <- rbind(crossexponent, tempdata)
  print(i)
}
plot<- ggplot(crossexponent, aes(Year, -exponent))+
  geom_point(size=1.5, shape=19, stroke = 0.3,color="rosybrown1")+
  geom_abline(slope=0,intercept=1,lty=2,size=0.4) + 
  geom_ribbon(aes(ymin=-low2.5, ymax = -up97.5),linetype="dashed",
              alpha = 0.2, size=0.5)+
  geom_line(aes(Year, -exponent),
            colour="steelblue1",size=0.5)+
  scale_x_continuous(breaks = scales::breaks_width(20))+
  labs(x="Year", 
       y="Pareto exponent (α)")+  #"7-day average cases \n(thousand)"
  theme (axis.text=element_text(size =6), 
         axis.title = element_text(size=7))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))

plot
plot1 <- ggplot(zdata2, aes(population, rank-1/2, colour=factor(year)))+
  geom_point(size=0.2, shape=19, stroke = 0.2)+
  geom_abline(slope=-1,intercept=8.25,lty=2,colour="blue",size=0.3) + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  labs (x=expression(paste("Population")), 
        y=expression(paste("Rank-1/2")))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(color="black",size=6,angle = 0,family = 'myFont'), 
         axis.title = element_text(size=7, family = 'myFont'))+
  theme (legend.position = c (0.15,0.3),
         legend.background = element_blank(),
         legend.key=element_blank(),
         legend.key.size=unit(0.5,'mm'),
         legend.text=element_text(size =5, family = 'myFont'),
         legend.title=element_blank())
#+scale_color_discrete(breaks=c("Southern Africa","Central Africa","Northern Africa","Eastern Africa","Western Africa"))

plot1
spdata<-zdata2[which(zdata2$Agglomeration_Name=="Al-iskandariya"),]
spdata1<-zdata2[which(zdata2$Agglomeration_Name=="Lagos"),]
plot11<-plot1+geom_point(data=spdata,size=0.3, shape=19, stroke = 0.3,color="black")+
  geom_line(data=spdata, colour="black",size=0.6,alpha=0.3)
plot12<-plot11+geom_point(data=spdata1,size=0.3, shape=19, stroke = 0.3,color="black")+
  geom_line(data=spdata1, colour="black",size=0.6,alpha=0.3)

cregion<-read.csv("country_with_region.csv", sep=",", header = T)
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Central African Republic"]="CAF"
names(zdata2)[3] <-"ISO"
zdata3 <- merge(zdata2, cregion, by="ISO")

Sdata3<-data.frame()
Edata3<-data.frame()
Ndata3<-data.frame()
Wdata3<-data.frame()
Cdata3<-data.frame()


Sdata <- zdata3[which(zdata3$AU_Regions =="Southern Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Sdata[which(Sdata$year==i),]
  Sdata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Sdata2  <- transform(Sdata2 , order =1:length(Sdata2$population))
  Sdata2<-data.frame(Sdata2[,-c(5,8)],
                     lnRank = log10(Sdata2$order-1/2))
  Sdata3 <- rbind(Sdata3, Sdata2)
}

Ndata <- zdata3[which(zdata3$AU_Regions =="Northern Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Ndata[which(Ndata$year==i),]
  Ndata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Ndata2  <- transform(Ndata2 , order =1:length(Ndata2$population))
  Ndata2<-data.frame(Ndata2[,-c(5,8)],
                     lnRank = log10(Ndata2$order-1/2))
  Ndata3 <- rbind(Ndata3, Ndata2)
}

Edata <- zdata3[which(zdata3$AU_Regions =="Eastern Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Edata[which(Edata$year==i),]
  Edata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Edata2  <- transform(Edata2 , order =1:length(Edata2$population))
  Edata2<-data.frame(Edata2[,-c(5,8)],
                     lnRank = log10(Edata2$order-1/2))
  Edata3 <- rbind(Edata3, Edata2)
}

Wdata <- zdata3[which(zdata3$AU_Regions =="Western Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Wdata[which(Wdata$year==i),]
  Wdata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Wdata2  <- transform(Wdata2 , order =1:length(Wdata2$population))
  Wdata2<-data.frame(Wdata2[,-c(5,8)],
                     lnRank = log10(Wdata2$order-1/2))
  Wdata3 <- rbind(Wdata3, Wdata2)
}

Cdata <- zdata3[which(zdata3$AU_Regions =="Central Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Cdata[which(Cdata$year==i),]
  Cdata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Cdata2  <- transform(Cdata2 , order =1:length(Cdata2$population))
  Cdata2<-data.frame(Cdata2[,-c(5,8)],
                     lnRank = log10(Cdata2$order-1/2))
  Cdata3 <- rbind(Cdata3, Cdata2)
}

plotdata<-rbind(Ndata3, Cdata3, Wdata3, Edata3, Sdata3)

plot2 <- ggplot(plotdata, aes(lnPop, lnRank, colour=factor(year)))+
  geom_point(size=0.3, shape=19, stroke = 0.3)+
  geom_abline(slope=-1,intercept=8,lty=2,colour="blue",size=0.4) + 
  facet_wrap(~AU_Regions, ncol = 3)+
  scale_x_continuous(breaks = scales::breaks_width(1))+
  scale_y_continuous(breaks = scales::breaks_width(1))+
  labs (x=expression(paste(Log[10],"[Population]")), 
        y=expression(paste(Log[10],"[Rank-1/2]")))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black")#,strip.background=element_blank()
  )+
  theme (legend.justification = c("right","top"),
         legend.background = element_blank(),
         legend.key=element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =9, family = 'myFont'),
         legend.title=element_blank())+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=10))+
  theme (strip.text.x = element_text(size = 8,margin = margin(1,0,1,0, "pt")))

plot2

crossexponent<-data.frame()
i <- 1950
j <- "Central Africa"

for (i in unique(plotdata$year))
{ 
  eachyear <- plotdata[which(plotdata$year==i),]
  for (j in unique(plotdata$AU_Regions))
  {
    eachregion<-eachyear[which(eachyear$AU_Regions==j),]
    mod <- lm(eachregion$lnRank ~ eachregion$lnPop)
    summary (mod)
    r_square<-round(summary(mod)$r.squared,3)
    slope<-round(coef(mod)[2],3)
    intercept <- round(coef(mod)[1],3)
    low <- round(confint.lm(mod)[2],4)
    up <- round(confint.lm(mod)[4],4)
    tempdata<- data.frame(exponent=slope, low2.5=low, up97.5=up, 
                          r_square=  r_square,  intercept=  intercept, Year =i, Region=j)
    crossexponent <- rbind(crossexponent, tempdata)
    print(j)
  }
  print(i)
}

plot3<- ggplot(crossexponent, aes(Year, -exponent,colour=factor(Region),fill=factor(Region)))+
  geom_point(size=0.6, shape=19, stroke = 0.2)+
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  geom_abline(slope=0,intercept=1,lty=2,size=0.4) + 
  geom_ribbon(aes(ymin=-low2.5, ymax = -up97.5,fill=factor(Region)),colour=NA,
              alpha = 0.1)+
  geom_line(aes(Year, -exponent,colour=factor(Region)),
            size=0.3)+
  scale_x_continuous(breaks = scales::breaks_width(20))+
  labs(x="Year", 
       y="Pareto exponent (α)")+  #"7-day average cases \n(thousand)"
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black",size = 0.4))+
  theme (axis.text=element_text(size =6), 
         axis.title = element_text(size=7))+
  theme (strip.text.x = element_text(size = 6,margin = margin(1,0,1,0, "pt")))+
  theme(legend.position ="none")
# theme(legend.position = c(0.8,0.85),
#   legend.title = element_blank(),
#       legend.key=element_blank(),
#       legend.key.size=unit(3,'mm'),
#       legend.text=element_text(size =6, family = 'myFont'),)


plot3

crossexponent<-data.frame()
zdata4<-data.frame()
zdata5<-data.frame()
crossexponent1<-data.frame()
i <- 1950
j <- "Angola"
for (i in unique(zdata3$year))
{ 
  eachyear <- zdata3[which(zdata3$year==i),]
  for (j in unique(zdata3$Country))
  {
    eachCountry <- eachyear[which(eachyear$Country==j),]
    if(length(unique(eachCountry$Agglomeration_Name))<10)  next()
    zdata4 <- rbind(zdata4, eachCountry)
    print(j)
  }
  print(i)
}

j <- "Angola"

for (j in unique(zdata4$Country))
{ 
  eachCountry <- zdata4[which(zdata4$Country==j),]
  if(length(unique(eachCountry$year))<9)  next()
  zdata5 <- rbind(zdata5, eachCountry)
  print(j)
}
##筛选9个年份都有10个以上居民点的国家
i <- 1950
j <- "Angola"
zdata6<-data.frame()
for (i in unique(zdata5$year))
{ 
  eachyear <- zdata5[which(zdata5$year==i),]
  for(j in unique(zdata5$Country))
  {
    eachcountry<-eachyear[which(eachyear$Country==j),]
    tempdata <-eachcountry [order(eachcountry $population,decreasing=TRUE),]
    tempdata  <- transform(tempdata , order =1:length(tempdata$population))
    tempdata<-data.frame(tempdata[,-c(5,8)],
                         lnRank = log10(tempdata$order-1/2))
    zdata6 <- rbind(zdata6, tempdata)
    
    mod <- lm(tempdata$lnRank ~ tempdata$lnPop)
    summary (mod)
    r_square<-round(summary(mod)$r.squared,3)
    slope<-round(coef(mod)[2],3)
    intercept <- round(coef(mod)[1],3)
    low <- round(confint.lm(mod)[2],4)
    up <- round(confint.lm(mod)[4],4)
    tempdata1<- data.frame(exponent=slope, low2.5=low, up97.5=up, 
                           r_square=  r_square,  intercept=  intercept, Year =i, Region=j)
    crossexponent1 <- rbind(crossexponent1, tempdata1)
  }
}

plot5<- ggplot(crossexponent1, aes(Year, -exponent,colour=factor(Region)))+
  geom_point(size=0.7, shape=19, stroke = 0.2)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_brewer(palette = "Paired")+
  geom_abline(slope=0,intercept=1,lty=2,size=0.3) + 
  geom_ribbon(aes(ymin=-low2.5, ymax = -up97.5,fill=factor(Region)),colour=NA,
              alpha = 0.1)+
  geom_line(aes(Year, -exponent,colour=factor(Region)),
            size=0.4)+
  scale_x_continuous(breaks = scales::breaks_width(20))+
  labs(x="Year", 
       y="Pareto exponent (α)")+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black",size = 0.4))+
  theme (axis.text=element_text(size =6), 
         axis.title = element_text(size=7))+
  theme (strip.text.x = element_text(size = 6,margin = margin(1,0,1,0, "pt")))+
  theme(legend.position = "none")
# scale_color_manual(values = c('orange','#72be64'))+
# scale_fill_manual(values = c('orange','#72be64'))
# theme(legend.position = c(0.85,0.9),
#   legend.title = element_blank(),
#       legend.key=element_blank(),
#       legend.key.size=unit(3,'mm'),
#       legend.text=element_text(size =6, family = 'myFont'))


plot5

cregion<-read.csv("country_with_region.csv", sep=",", header = T)
pareto<-read.csv("2020各国内部人口及排名.csv", sep=",", header = T)
pareto1 <- merge(pareto, cregion, by="Country")
urbaniz<-read.csv("2020年各国城市化率.csv", sep=",", header = T)
names(urbaniz)[2] <-"ISO"
data <- merge(pareto1, urbaniz, by="ISO")
mod<-lm(-data$exponent ~ data$X2020)
summary(mod)
data <- na.omit(data)
corre<-cor(data$exponent,data$X2020,method="pearson")
print(corre)
p1 <- ggplot(data, aes(X2020, -exponent))+#
  geom_point(size=2, shape=19, stroke = 0.3,alpha=0.5,color="steelblue1")+
  geom_smooth(method="lm",size=0.6, se=F,color="black")+
  stat_poly_eq(formula = y~x,
               aes(label =  paste(stat(eq.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2, label.x = 0.9, label.y = 0.9)+
  stat_poly_eq(formula = y~x,
               aes(label =  paste(stat(rr.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2, label.x = 0.88, label.y =0.8)+
  labs(x="Urbanization level in 2020 (%)", #轴标题设置下标
       y="Pareto exponent in 2020 (α)",
       color="Region")+ 
  scale_color_brewer(palette = "Set2")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme(axis.text=element_text(size =6), 
        axis.title = element_text(size=6.5))
p1

plot8<-ggplot()+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank())

tiff(file="Figure2 abcdf.tiff", res = 600, width = 2834, height = 1889, compression = "lzw")
ggarrange(plot12,plot,plot8,
          plot3,plot5,p1,
          ncol=3,nrow=2, 
          #labels = c("  b. Central Africa","c. Eastern Africa","d. Northern Africa","e. Southern Africa","f. Western Africa"),
          # font.label = list(size = 7, color = "black", face = "bold", family = NULL),
          # label.x = 0.28,label.y=0.97,
          align="v") 
dev.off()


# fig 3 -------------------------------------------------------------------
data2010<-read.csv("2010基期.csv", sep=",", header = T)
names(data2010)[3] <-"ISO"
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
data2010 <- merge(data2010, cregion, by="ISO")

tempdata<-data.frame()
result<-data.frame()

mod <- lm(data2010$X2020.2010 ~ data2010$log2010)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="2020-2010",
                     cofficient=round(coef(mod)[2],3),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2010,
                     n= length(data2010[,1]))
result<-rbind(result,tempdata)

plot1<- ggplot(data2010, aes(Population_2010, Population_2020/Population_2010))+#,color=AU_Regions
  geom_point(size=2, shape=21, stroke = 0.3,alpha=0.5,fill="steelblue1")+
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  geom_smooth(method="lm",size=0.6, se=F, colour="black")+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 2),labels = trans_format("log10", math_format(10^.x)))+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(eq.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=3, label.x = 0.95, label.y = 0.88)+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(rr.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=3, label.x = 0.88, label.y = 0.78)+
  scale_color_brewer(palette = "Set2")+
  labs(x=expression(paste("Urban population in 2010 (person)")), 
       y=expression(paste("Growth rate (2010-2020)")))+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme (strip.text.x = element_text(size = 9,margin = margin(1,0,1,0, "pt")))+
  theme (
    legend.position = c (0.8,0.8),
    legend.background = element_blank(),
    legend.key.size=unit(4,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =7))

plot1



plot11<- ggplot(data2010, aes(Population_2010, Population_2020/Population_2010,fill=AU_Regions))+#
  geom_point(size=1.5, shape=21, stroke = 0.2,alpha=0.3)+#,color="steelblue1"
  facet_wrap(~AU_Regions, ncol = 3)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 2),labels = trans_format("log10", math_format(10^.x)))+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  geom_smooth(method="lm",size=0.6, se=F,aes(color=AU_Regions) )+#
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(eq.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2.5, label.x = 0.95, label.y = 0.90, colour="black")+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(rr.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2.5, label.x = 0.83, label.y = 0.80, colour="black")+
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Dark2")+
  labs(x=expression(paste("Urban population in 2010 (person)")), 
       y=expression(paste("Growth rate (2010-2020)")))+ 
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme (strip.text=element_blank())+
  theme (
    legend.position = c (0.85,0.2),
    legend.background = element_blank(),
    legend.key.size=unit(4,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =8))

plot11


result<-data.frame()

data1950<-read.csv("1950基期1.csv", sep=",", header = T)
names(data1950)[3] <-"ISO"
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
data1950<- merge(data1950, cregion, by="ISO")
tempdata<-data.frame()


mod <- lm(data1950$X1960.1950 ~ data1950$log1950)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="whole",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1950,
                     n= length(data1950[,1]))
result<-rbind(result,tempdata)

Sdata <- data1950[which(data1950$AU_Regions =="Southern Africa"),]
mod <- lm(Sdata$X1960.1950 ~ Sdata$log1950)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Southern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1950,
                     n= length(Sdata[,1]))
result<-rbind(result,tempdata)

Ndata <- data1950[which(data1950$AU_Regions =="Northern Africa"),]
mod <- lm(Ndata$X1960.1950 ~ Ndata$log1950)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Northern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1950,
                     n= length(Ndata[,1]))
result<-rbind(result,tempdata)


Cdata <- data1950[which(data1950$AU_Regions =="Central Africa"),]
mod <- lm(Cdata$X1960.1950 ~ Cdata$log1950)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Central Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1950,
                     n= length(Cdata[,1]))
result<-rbind(result,tempdata)


Wdata <- data1950[which(data1950$AU_Regions =="Western Africa"),]
mod <- lm(Wdata$X1960.1950 ~ Wdata$log1950)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Western Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1950,
                     n= length(Wdata[,1]))
result<-rbind(result,tempdata)


Edata <- data1950[which(data1950$AU_Regions =="Eastern Africa"),]
mod <- lm(Edata$X1960.1950 ~ Edata$log1950)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Eastern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1950,
                     n= length(Edata[,1]))
result<-rbind(result,tempdata)


data1960<-read.csv("1960基期1.csv", sep=",", header = T)
names(data1960)[3] <-"ISO"
data1960<- merge(data1960, cregion, by="ISO")
tempdata<-data.frame()


mod <- lm(data1960$X1970.1960 ~ data1960$log1960)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="whole",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1960,
                     n= length(data1960[,1]))
result<-rbind(result,tempdata)

Sdata <- data1960[which(data1960$AU_Regions =="Southern Africa"),]
mod <- lm(Sdata$X1970.1960 ~ Sdata$log1960)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Southern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1960,
                     n= length(Sdata[,1]))
result<-rbind(result,tempdata)

Ndata <- data1960[which(data1960$AU_Regions =="Northern Africa"),]
mod <- lm(Ndata$X1970.1960 ~ Ndata$log1960)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Northern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1960,
                     n= length(Ndata[,1]))
result<-rbind(result,tempdata)


Cdata <- data1960[which(data1960$AU_Regions =="Central Africa"),]
mod <- lm(Cdata$X1970.1960 ~ Cdata$log1960)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Central Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1960,
                     n= length(Cdata[,1]))
result<-rbind(result,tempdata)


Wdata <- data1960[which(data1960$AU_Regions =="Western Africa"),]
mod <- lm(Wdata$X1970.1960 ~ Wdata$log1960)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Western Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1960,
                     n= length(Wdata[,1]))
result<-rbind(result,tempdata)


Edata <- data1960[which(data1960$AU_Regions =="Eastern Africa"),]
mod <- lm(Edata$X1970.1960 ~ Edata$log1960)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Eastern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1960,
                     n= length(Edata[,1]))
result<-rbind(result,tempdata)


data1970<-read.csv("1970基期1.csv", sep=",", header = T)
names(data1970)[3] <-"ISO"
data1970<- merge(data1970, cregion, by="ISO")
tempdata<-data.frame()


mod <- lm(data1970$X1980.1970 ~ data1970$log1970)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="whole",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1970,
                     n= length(data1970[,1]))
result<-rbind(result,tempdata)

Sdata <- data1970[which(data1970$AU_Regions =="Southern Africa"),]
mod <- lm(Sdata$X1980.1970 ~ Sdata$log1970)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Southern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1970,
                     n= length(Sdata[,1]))
result<-rbind(result,tempdata)

Ndata <- data1970[which(data1970$AU_Regions =="Northern Africa"),]
mod <- lm(Ndata$X1980.1970 ~ Ndata$log1970)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Northern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1970,
                     n= length(Ndata[,1]))
result<-rbind(result,tempdata)


Cdata <- data1970[which(data1970$AU_Regions =="Central Africa"),]
mod <- lm(Cdata$X1980.1970 ~ Cdata$log1970)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Central Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1970,
                     n= length(Cdata[,1]))
result<-rbind(result,tempdata)


Wdata <- data1970[which(data1970$AU_Regions =="Western Africa"),]
mod <- lm(Wdata$X1980.1970 ~ Wdata$log1970)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Western Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1970,
                     n= length(Wdata[,1]))
result<-rbind(result,tempdata)


Edata <- data1970[which(data1970$AU_Regions =="Eastern Africa"),]
mod <- lm(Edata$X1980.1970 ~ Edata$log1970)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Eastern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1970,
                     n= length(Edata[,1]))
result<-rbind(result,tempdata)

data1980<-read.csv("1980基期1.csv", sep=",", header = T)
names(data1980)[3] <-"ISO"
data1980<- merge(data1980, cregion, by="ISO")
tempdata<-data.frame()


mod <- lm(data1980$X1990.1980 ~ data1980$log1980)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="whole",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1980,
                     n= length(data1980[,1]))
result<-rbind(result,tempdata)

Sdata <- data1980[which(data1980$AU_Regions =="Southern Africa"),]
mod <- lm(Sdata$X1990.1980 ~ Sdata$log1980)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Southern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1980,
                     n= length(Sdata[,1]))
result<-rbind(result,tempdata)

Ndata <- data1980[which(data1980$AU_Regions =="Northern Africa"),]
mod <- lm(Ndata$X1990.1980 ~ Ndata$log1980)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Northern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1980,
                     n= length(Ndata[,1]))
result<-rbind(result,tempdata)


Cdata <- data1980[which(data1980$AU_Regions =="Central Africa"),]
mod <- lm(Cdata$X1990.1980 ~ Cdata$log1980)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Central Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1980,
                     n= length(Cdata[,1]))
result<-rbind(result,tempdata)


Wdata <- data1980[which(data1980$AU_Regions =="Western Africa"),]
mod <- lm(Wdata$X1990.1980 ~ Wdata$log1980)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Western Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1980,
                     n= length(Wdata[,1]))
result<-rbind(result,tempdata)


Edata <- data1980[which(data1980$AU_Regions =="Eastern Africa"),]
mod <- lm(Edata$X1990.1980 ~ Edata$log1980)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Eastern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1980,
                     n= length(Edata[,1]))
result<-rbind(result,tempdata)


data1990<-read.csv("1990基期1.csv", sep=",", header = T)
names(data1990)[3] <-"ISO"
data1990<- merge(data1990, cregion, by="ISO")
tempdata<-data.frame()


mod <- lm(data1990$X2000.1990 ~ data1990$log1990)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="whole",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1990,
                     n= length(data1990[,1]))
result<-rbind(result,tempdata)

Sdata <- data1990[which(data1990$AU_Regions =="Southern Africa"),]
mod <- lm(Sdata$X2000.1990 ~ Sdata$log1990)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Southern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1990,
                     n= length(Sdata[,1]))
result<-rbind(result,tempdata)

Ndata <- data1990[which(data1990$AU_Regions =="Northern Africa"),]
mod <- lm(Ndata$X2000.1990 ~ Ndata$log1990)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Northern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1990,
                     n= length(Ndata[,1]))
result<-rbind(result,tempdata)


Cdata <- data1990[which(data1990$AU_Regions =="Central Africa"),]
mod <- lm(Cdata$X2000.1990 ~ Cdata$log1990)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Central Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1990,
                     n= length(Cdata[,1]))
result<-rbind(result,tempdata)


Wdata <- data1990[which(data1990$AU_Regions =="Western Africa"),]
mod <- lm(Wdata$X2000.1990 ~ Wdata$log1990)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Western Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1990,
                     n= length(Wdata[,1]))
result<-rbind(result,tempdata)


Edata <- data1990[which(data1990$AU_Regions =="Eastern Africa"),]
mod <- lm(Edata$X2000.1990 ~ Edata$log1990)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Eastern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=1990,
                     n= length(Edata[,1]))
result<-rbind(result,tempdata)



data2000<-read.csv("2000基期1.csv", sep=",", header = T)
names(data2000)[3] <-"ISO"
data2000<- merge(data2000, cregion, by="ISO")
tempdata<-data.frame()


mod <- lm(data2000$X2010.2000 ~ data2000$log2000)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="whole",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2000,
                     n= length(data2000[,1]))
result<-rbind(result,tempdata)

Sdata <- data2000[which(data2000$AU_Regions =="Southern Africa"),]
mod <- lm(Sdata$X2010.2000 ~ Sdata$log2000)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Southern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2000,
                     n= length(Sdata[,1]))
result<-rbind(result,tempdata)

Ndata <- data2000[which(data2000$AU_Regions =="Northern Africa"),]
mod <- lm(Ndata$X2010.2000 ~ Ndata$log2000)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Northern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2000,
                     n= length(Ndata[,1]))
result<-rbind(result,tempdata)


Cdata <- data2000[which(data2000$AU_Regions =="Central Africa"),]
mod <- lm(Cdata$X2010.2000 ~ Cdata$log2000)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Central Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2000,
                     n= length(Cdata[,1]))
result<-rbind(result,tempdata)


Wdata <- data2000[which(data2000$AU_Regions =="Western Africa"),]
mod <- lm(Wdata$X2010.2000 ~ Wdata$log2000)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Western Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2000,
                     n= length(Wdata[,1]))
result<-rbind(result,tempdata)


Edata <- data2000[which(data2000$AU_Regions =="Eastern Africa"),]
mod <- lm(Edata$X2010.2000 ~ Edata$log2000)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Eastern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2000,
                     n= length(Edata[,1]))
result<-rbind(result,tempdata)


data2010<-read.csv("2010基期1.csv", sep=",", header = T)
names(data2010)[3] <-"ISO"
data2010<- merge(data2010, cregion, by="ISO")
tempdata<-data.frame()


mod <- lm(data2010$X2020.2010 ~ data2010$log2010)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="whole",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2010,
                     n= length(data2010[,1]))
result<-rbind(result,tempdata)

Sdata <- data2010[which(data2010$AU_Regions =="Southern Africa"),]
mod <- lm(Sdata$X2020.2010 ~ Sdata$log2010)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Southern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2010,
                     n= length(Sdata[,1]))
result<-rbind(result,tempdata)

Ndata <- data2010[which(data2010$AU_Regions =="Northern Africa"),]
mod <- lm(Ndata$X2020.2010 ~ Ndata$log2010)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Northern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2010,
                     n= length(Ndata[,1]))
result<-rbind(result,tempdata)


Cdata <- data2010[which(data2010$AU_Regions =="Central Africa"),]
mod <- lm(Cdata$X2020.2010 ~ Cdata$log2010)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Central Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2010,
                     n= length(Cdata[,1]))
result<-rbind(result,tempdata)


Wdata <- data2010[which(data2010$AU_Regions =="Western Africa"),]
mod <- lm(Wdata$X2020.2010 ~ Wdata$log2010)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Western Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2010,
                     n= length(Wdata[,1]))
result<-rbind(result,tempdata)


Edata <- data2010[which(data2010$AU_Regions =="Eastern Africa"),]
mod <- lm(Edata$X2020.2010 ~ Edata$log2010)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="Eastern Africa",
                     cofficient=round(coef(mod)[2],3),
                     low=round(confint.lm(mod)[2],4),
                     up=round(confint.lm(mod)[4],4),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2010,
                     n= length(Edata[,1]))
result<-rbind(result,tempdata)


wholedata<-result[which(result$name =="whole"),]
wholeplot<- ggplot(wholedata,aes( x=base,y=cofficient))+#,color=AU_Regions
  geom_point(size=3, shape=19, stroke = 0.3,color="#1F78B4")+
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  #geom_line(aes(base, cofficient),colour="#A6CEE3",size=1)+
  scale_x_continuous(breaks = scales::breaks_width(10))+
  geom_errorbar(aes(ymin = low, ymax = up),colour="#1F78B4",size=0.6,width=4)+
  labs(y="δ for Gibrat's law", 
       x="Growth period"
  )+
  scale_x_continuous(breaks = c(1960,1980,2000),labels = c("1960s","1980s" ,"2000s"))+
  theme (axis.text=element_text(size =7),
         axis.title = element_text(size=9))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))

wholeplot


regiondata<-result[which(result$name !="whole"),]
regionplot<- ggplot(regiondata,aes( x=base,y=cofficient,color=name))+#
  facet_wrap(~name, ncol = 3)+
  scale_color_brewer(palette = "Dark2")+
  geom_point(size=1.5, shape=19, stroke = 0.3)+
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  #geom_line(aes(base, cofficient),colour="grey",size=0.6)+
  scale_x_continuous(breaks = scales::breaks_width(10))+
  geom_errorbar(aes(ymin = low, ymax = up),size=0.4,width=4)+
  labs(y="δ for Gibrat's law", 
       x="Growth period"
  )+
  scale_x_continuous(breaks = c(1960,1980,2000),labels = c("1960s", "1980s","2000s"))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =7), 
         axis.title = element_text(size=9))+
  theme (strip.text=element_blank())+
  theme (
    legend.position = c (0.85,0.2),
    legend.background = element_blank(),
    legend.key.size=unit(4,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =8))
regionplot

# fig 4 -------------------------------------------------------------------

crossexponent<-data.frame()
i <- "Angola"
for (i in unique(agglodata4$Country))
{
  eachCountry <- agglodata4[which(agglodata4$Country==i),]
  #if(i=="Equatorial Guinea"|i=="South Sudan")  next()
  
  mod <- lm(log10(eachCountry$Builtup) ~ log10(eachCountry$Population))
  summary (mod)
  r_square<-round(summary(mod)$r.squared,3)
  slope<-round(coef(mod)[2],3)
  intercept <- round(coef(mod)[1],3)
  low <- round(confint.lm(mod)[2],4)#
  up <- round(confint.lm(mod)[4],4)#
  tempdata<- data.frame(exponent=slope, low2.5=low, up97.5=up, 
                        r_square=  r_square,  intercept=  intercept, Country =i)
  crossexponent <- rbind(crossexponent, tempdata)
  print(i)
}


crossexponent 
crossexponent <- merge(crossexponent, cregion, by="Country")    
crossexponent <-crossexponent[order(crossexponent$exponent),]#
crossexponent  <- transform(crossexponent , order =1:length(crossexponent$exponent))


crossexponent[crossexponent=="CAF"]="Central Africa Rep."
crossexponent[crossexponent=="Republic of the Congo"]="Congo"
write.table(crossexponent,"country_exponent_2020.csv",row.names=FALSE,col.names=TRUE,sep=",")

#
expoAll <- ggplot(crossexponent,aes( x=exponent,y=order,color=AU_Regions))+
  geom_point()+
  geom_vline(xintercept  = 1, colour="black",size=0.6,linetype="dashed")+
  #geom_vline(xintercept  = 1, colour="red",size=0.6,linetype="dashed")+
  geom_errorbarh(aes(xmin = low2.5, xmax = up97.5),  size=0.4)+
  scale_y_continuous(breaks =c(1:length(crossexponent$exponent)),
                     labels = crossexponent$Country)+
  labs(y="African countries", 
       x=expression(paste("Scaling exponent"," (",beta,")"," in 2020"))
  )+
  theme(plot.margin=unit(c(0.05,0.2,0.03,0.05),"lines"),
        axis.title.y = element_blank(),
        axis.text=element_text(size =7), 
        axis.title = element_text(size=9),
        panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  scale_color_brewer(palette = "Set2")+
  theme(
    legend.position = c(1,0),#
    legend.justification = c("right", "bottom"),#
    legend.background = element_blank(),#
    legend.key = element_blank(),#
    legend.box.background = element_rect(fill=NA,color=NA), #
    legend.text=element_text(size =7.5), 
    legend.key.size=unit(4,'mm'),
    legend.title = element_blank()
  )

expoAll


Sdata <- agglodata1[which(agglodata1$AU_Regions =="Southern Africa"),]
lm(log10(Sdata$Builtup) ~ log10(Sdata$Population))##-2.5663,0.8078 #15： -2.7900        0.8441
Sdata<-data.frame(Sdata,
                  sami=log10(Sdata$Builtup)-0.8078*log10(Sdata$Population)+2.5663)
Sdata1 <- Sdata[which(Sdata$sami < 2*sd(Sdata$sami)),]
Sdata2<-Sdata1[which(Sdata1$sami > -2*sd(Sdata$sami)),]


Edata <- agglodata1[which(agglodata1$AU_Regions =="Eastern Africa"),]
lm(log10(Edata$Builtup) ~ log10(Edata$Population))##-3.615,1.008 #15：      -4.096         1.101 
Edata<-data.frame(Edata,
                  sami=log10(Edata$Builtup)-1.008*log10(Edata$Population)+3.615 )
Edata1 <- Edata[which(Edata$sami < 2*sd(Edata$sami)),]
Edata2<-Edata1[which(Edata1$sami > -2*sd(Edata$sami)),]


Wdata <- agglodata1[which(agglodata1$AU_Regions =="Western Africa"),]
lm(log10(Wdata$Builtup) ~ log10(Wdata$Population))##-3.755,1.008 #15：      -3.881         1.029  
Wdata<-data.frame(Wdata,
                  sami=log10(Wdata$Builtup)-1.008*log10(Wdata$Population)+3.755)
Wdata1 <- Wdata[which(Wdata$sami < 2*sd(Wdata$sami)),]
Wdata2<-Wdata1[which(Wdata1$sami > -2*sd(Wdata$sami)),]


Cdata <- agglodata1[which(agglodata1$AU_Regions =="Central Africa"),]
lm(log10(Cdata$Builtup) ~ log10(Cdata$Population))##-3.0980,0.8661 #15：     -3.3868        0.9228 
Cdata<-data.frame(Cdata,
                  sami=log10(Cdata$Builtup)-0.8661*log10(Cdata$Population)+3.0980)
Cdata1 <- Cdata[which(Cdata$sami < 2*sd(Cdata$sami)),]
Cdata2<-Cdata1[which(Cdata1$sami > -2*sd(Cdata$sami)),]


Ndata <- agglodata1[which(agglodata1$AU_Regions =="Northern Africa"),]
lm(log10(Ndata$Builtup) ~ log10(Ndata$Population))##-3.4307,0.9297   #15：   -4.073         1.016 
Ndata<-data.frame(Ndata,
                  sami=log10(Ndata$Builtup)-0.9297*log10(Ndata$Population)+3.4307)
Ndata1 <- Ndata[which(Ndata$sami < 2*sd(Ndata$sami)),]
Ndata2<-Ndata1[which(Ndata1$sami > -2*sd(Ndata$sami)),]


plotdata<-rbind(Ndata2, Cdata2, Wdata2, Edata2, Sdata2)

plot1 <- ggplot(plotdata, aes(Population, Builtup, colour=factor(AU_Regions)))+
  geom_point(size=0.5, shape=19,stroke = 0.01, alpha=0.5)+
  geom_smooth(method="lm",size=0.7, se=F,)+
  #scale_color_manual(values=c('#343D97','#4AB5E2',  '#1F9752', '#A7BD3F' ))+
  scale_color_brewer(palette = "Set2")+
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., 
                                 sep = '~~~~~~~~')),
               formula = y ~ x,label.x = 0.98,label.y = c(0.25,0.19,0.13,0.07,0.01), parse = T,size=2.2,
               vstep = 0.06, family = 'myFont') + 
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  labs (x=expression(paste("Population in 2020")), 
        y=expression(paste("Built up area in 2020 (", km^2,")")))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =7), 
         axis.title = element_text(size=9))+
  theme (legend.position = c (0.25,0.85),
         legend.background = element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =7),
         legend.title=element_blank())
#+scale_color_discrete(breaks=c("Southern Africa","Central Africa","Northern Africa","Eastern Africa","Western Africa"))

plot1

print(set2_8)
#"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854"
spdata<-plotdata[which(plotdata$Agglomeration_Name=="Al-qahira"),]
spdata1<-plotdata[which(plotdata$Agglomeration_Name=="Lagos"),]
spdata2<-plotdata[which(plotdata$Agglomeration_Name=="Johannesburg"),]
spdata3<-plotdata[which(plotdata$Agglomeration_Name=="Nairobi"),]
spdata4<-plotdata[which(plotdata$Agglomeration_Name=="Al-iskandariya"),]

plot11<-plot1+geom_point(data=spdata,size=1, shape=19, stroke = 0.3,color="#8DA0CB",alpha=0.8)
plot12<-plot11+geom_point(data=spdata1,size=1, shape=19, stroke = 0.3,color="#A6D854",alpha=0.8)
plot13<-plot12+geom_point(data=spdata2,size=1, shape=19, stroke = 0.3,color="#E78AC3",alpha=0.8)
plot14<-plot13+geom_point(data=spdata3,size=1, shape=19, stroke = 0.3,color="#FC8D62",alpha=0.8)
plot15<-plot14+geom_point(data=spdata4,size=1, shape=19, stroke = 0.3,color="#8DA0CB",alpha=0.8)

mydata15<-read.csv("country_exponent_2015.csv", sep=",", header = T)
mydata20<-read.csv("country_exponent_2020.csv", sep=",", header = T)
mydata15 <- mydata15[,c(1,2,7,8)]
mydata20 <- mydata20[,c(1,2)]
names(mydata20)[1] <-"country"
names(mydata15)[1] <-"country"
contradata <- merge(mydata15, mydata20, by="country")  
names(contradata)[2] <-"beta15"
names(contradata)[5] <-"beta20"


p1 <- ggplot(contradata, aes(beta15, beta20,label=country,color=AU_Regions))+#
  geom_point(shape=19,size=1.5,alpha=0.8)+
  # geom_smooth(method="lm",size=0.6, se=F, colour="red")+
  # stat_poly_eq(formula = y~x,
  #              aes(label =  paste(stat(eq.label), sep = "~~~~")),
  #              rr.digits = 2, coef.digits = 2,parse = TRUE,
  #              size=2, label.x = 0.9, label.y = 0.2)+
  # stat_poly_eq(formula = y~x,
  #              aes(label =  paste(stat(rr.label), sep = "~~~~")),
  #              rr.digits = 2, coef.digits = 2,parse = TRUE,
  #              size=2, label.x = 0.88, label.y =0.1)+
  geom_abline(slope=1,intercept=0,lty=2,colour="red",size=0.4) + 
  geom_abline(slope=0,intercept=1,lty=2,colour="black",size=0.4) + 
  geom_vline(xintercept  = 1, colour="black",size=0.4,linetype="dashed")+
  #geom_abline(slope=0,intercept=5/6,lty=2,colour="black",alpha=0.5) + 
  #geom_vline(xintercept  = 5/6, colour="black",size=0.4,linetype="dashed",alpha=0.5)+
  labs(x="Scaling exponent (β) in 2015", 
       y="Scaling exponent (β) in 2020",
       color="Region")+ 
  scale_color_brewer(palette = "Set2")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=9))+
  theme (
    legend.position = c (0.25,0.84),
    legend.background = element_blank(),
    legend.key.size=unit(2.2,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =6))

set2_8 <- brewer.pal(5, "Set2")


print(set2_8)
#"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854"

p2<-p1+#geom_text(hjust=0.4, vjust=2,size=2,check_overlap = TRUE)
  annotate("text",
           x=contradata[which(contradata$country=='Comoros'),]$beta15-0.06,
           y=contradata[which(contradata$country=='Comoros'),]$beta20,
           label = contradata[which(contradata$country=='Comoros'),]$country,
           size=2,
           color="#FC8D62")+
  annotate("text",
           x=contradata[which(contradata$country=='Benin'),]$beta15,
           y=contradata[which(contradata$country=='Benin'),]$beta20+0.03,
           label = contradata[which(contradata$country=='Benin'),]$country,
           size=2,
           color="#A6D854")+
  annotate("text",
           x=contradata[which(contradata$country=='Rwanda'),]$beta15+0.06,
           y=contradata[which(contradata$country=='Rwanda'),]$beta20,
           label = contradata[which(contradata$country=='Rwanda'),]$country,
           size=2,
           color="#FC8D62")+
  annotate("text",
           x=contradata[which(contradata$country=='Egypt'),]$beta15+0.04,
           y=contradata[which(contradata$country=='Egypt'),]$beta20+0.03,
           label = contradata[which(contradata$country=='Egypt'),]$country,
           size=2,
           color="#8DA0CB")+
  annotate("text",
           x=contradata[which(contradata$country=='Kenya'),]$beta15-0.01,
           y=contradata[which(contradata$country=='Kenya'),]$beta20+0.02,
           label = contradata[which(contradata$country=='Kenya'),]$country,
           size=2,
           color="#FC8D62")+
  annotate("text",
           x=contradata[which(contradata$country=='Uganda'),]$beta15,
           y=contradata[which(contradata$country=='Uganda'),]$beta20+0.02,
           label = contradata[which(contradata$country=='Uganda'),]$country,
           size=2,
           color="#FC8D62")+
  annotate("text",
           x=contradata[which(contradata$country=='Central Africa Rep.'),]$beta15-0.04,
           y=contradata[which(contradata$country=='Central Africa Rep.'),]$beta20-0.02,
           label = contradata[which(contradata$country=='Central Africa Rep.'),]$country,
           size=2,
           color="#66C2A5")+
  annotate("text",
           x=contradata[which(contradata$country=='Morocco'),]$beta15,
           y=contradata[which(contradata$country=='Morocco'),]$beta20-0.02,
           label = contradata[which(contradata$country=='Morocco'),]$country,
           size=2,
           color="#8DA0CB")+
  annotate("text",
           x=contradata[which(contradata$country=='Namibia'),]$beta15+0.06,
           y=contradata[which(contradata$country=='Namibia'),]$beta20,
           label = contradata[which(contradata$country=='Namibia'),]$country,
           size=2,
           color="#E78AC3")+
  annotate("text",
           x=contradata[which(contradata$country=='Mauritania'),]$beta15+0.04,
           y=contradata[which(contradata$country=='Mauritania'),]$beta20-0.02,
           label = contradata[which(contradata$country=='Mauritania'),]$country,
           size=2,
           color="#8DA0CB")+
  annotate("text",
           x=contradata[which(contradata$country=='Somalia'),]$beta15,
           y=contradata[which(contradata$country=='Somalia'),]$beta20+0.02,
           label = contradata[which(contradata$country=='Somalia'),]$country,
           size=2,
           color="#FC8D62")
# fig S2 ------------------------------------------------------------------

zdata<-read.csv("1950-2020城市人口及排名.csv", sep=",", header = T)
zdata2<-data.frame(zdata,
                   lnPop = log10(zdata$population),
                   lnRank = log10(zdata$rank-1/2))
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Central African Republic"]="CAF"
names(zdata2)[3] <-"ISO"
zdata3 <- merge(zdata2, cregion, by="ISO")

Sdata3<-data.frame()
Edata3<-data.frame()
Ndata3<-data.frame()
Wdata3<-data.frame()
Cdata3<-data.frame()

data_1990 <- subset(zdata3, year == 1990)
data_2000 <- subset(zdata3, year == 2000)
zdata3<-rbind(data_1990,data_2000)

Sdata <- zdata3[which(zdata3$AU_Regions =="Southern Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Sdata[which(Sdata$year==i),]
  Sdata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Sdata2  <- transform(Sdata2 , order =1:length(Sdata2$population))
  Sdata2<-data.frame(Sdata2[,-c(5,8)],
                     lnRank = log10(Sdata2$order-1/2))
  Sdata3 <- rbind(Sdata3, Sdata2)
}

Ndata <- zdata3[which(zdata3$AU_Regions =="Northern Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Ndata[which(Ndata$year==i),]
  Ndata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Ndata2  <- transform(Ndata2 , order =1:length(Ndata2$population))
  Ndata2<-data.frame(Ndata2[,-c(5,8)],
                     lnRank = log10(Ndata2$order-1/2))
  Ndata3 <- rbind(Ndata3, Ndata2)
}

Edata <- zdata3[which(zdata3$AU_Regions =="Eastern Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Edata[which(Edata$year==i),]
  Edata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Edata2  <- transform(Edata2 , order =1:length(Edata2$population))
  Edata2<-data.frame(Edata2[,-c(5,8)],
                     lnRank = log10(Edata2$order-1/2))
  Edata3 <- rbind(Edata3, Edata2)
}

Wdata <- zdata3[which(zdata3$AU_Regions =="Western Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Wdata[which(Wdata$year==i),]
  Wdata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Wdata2  <- transform(Wdata2 , order =1:length(Wdata2$population))
  Wdata2<-data.frame(Wdata2[,-c(5,8)],
                     lnRank = log10(Wdata2$order-1/2))
  Wdata3 <- rbind(Wdata3, Wdata2)
}

Cdata <- zdata3[which(zdata3$AU_Regions =="Central Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Cdata[which(Cdata$year==i),]
  Cdata2  <-eachyear [order(eachyear $population,decreasing=TRUE),]
  Cdata2  <- transform(Cdata2 , order =1:length(Cdata2$population))
  Cdata2<-data.frame(Cdata2[,-c(5,8)],
                     lnRank = log10(Cdata2$order-1/2))
  Cdata3 <- rbind(Cdata3, Cdata2)
}

plotdata<-rbind(Ndata3, Cdata3, Wdata3, Edata3, Sdata3)
#zipf's law拟合
plot2 <- ggplot(plotdata, aes(population, order-1/2, colour=factor(year)))+
  geom_point(size=0.3, shape=19, stroke = 0.3)+
  geom_abline(slope=-1,intercept=7.5,lty=2,colour="blue",size=0.4) + 
  facet_wrap(~AU_Regions, ncol = 3)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  labs (x=expression(paste("Population")), 
        y=expression(paste("Rank-1/2")))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black")#,strip.background=element_blank()
  )+
  theme (legend.position = c (0.85,0.2),
         legend.background = element_blank(),
         legend.key=element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =9, family = 'myFont'),
         legend.title=element_blank())+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=10))+
  theme (strip.text.x = element_text(size = 8,margin = margin(1,0,1,0, "pt")),
         strip.background = element_blank(),
         #panel.grid.major =element_blank(),
         #panel.grid.minor = element_blank(),
         panel.border =element_rect(fill=NA),
         panel.background =element_blank(),
         axis.line = element_line(colour ="black"))

plot2


# fig S4------------------------------------------------------------------
zdata<-read.csv("1950-2020城市建成面积及排名.csv", sep=",", header = T)
zdata1 <-zdata[which(zdata$built_up>10),]
zdata2<-data.frame(zdata1,
                   lnPop = log10(zdata1$built_up),
                   lnRank = log10(zdata1$rank-1/2))
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Central African Republic"]="CAF"
names(zdata2)[3] <-"ISO"
zdata3 <- merge(zdata2, cregion, by="ISO")

Sdata3<-data.frame()
Edata3<-data.frame()
Ndata3<-data.frame()
Wdata3<-data.frame()
Cdata3<-data.frame()


Sdata <- zdata3[which(zdata3$AU_Regions =="Southern Africa"),]

for (i in unique(zdata3$year))
{ 
  eachyear <- Sdata[which(Sdata$year==i),]
  Sdata2  <-eachyear [order(eachyear $built_up,decreasing=TRUE),]
  Sdata2  <- transform(Sdata2 , order =1:length(Sdata2$built_up))
  Sdata2<-data.frame(Sdata2[,-c(5,8)],
                     lnRank = log10(Sdata2$order-1/2))
  Sdata3 <- rbind(Sdata3, Sdata2)
}

unique(Sdata3$year)

Ndata <- zdata3[which(zdata3$AU_Regions =="Northern Africa"),]
for (i in unique(zdata3$year))
{ 
  eachyear <- Ndata[which(Ndata$year==i),]
  Ndata2  <-eachyear [order(eachyear $built_up,decreasing=TRUE),]
  Ndata2  <- transform(Ndata2 , order =1:length(Ndata2$built_up))
  Ndata2<-data.frame(Ndata2[,-c(5,8)],
                     lnRank = log10(Ndata2$order-1/2))
  Ndata3 <- rbind(Ndata3, Ndata2)
}

unique(Ndata3$year)

Edata <- zdata3[which(zdata3$AU_Regions =="Eastern Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Edata[which(Edata$year==i),]
  Edata2  <-eachyear [order(eachyear $built_up,decreasing=TRUE),]
  Edata2  <- transform(Edata2 , order =1:length(Edata2$built_up))
  Edata2<-data.frame(Edata2[,-c(5,8)],
                     lnRank = log10(Edata2$order-1/2))
  Edata3 <- rbind(Edata3, Edata2)
}

Wdata <- zdata3[which(zdata3$AU_Regions =="Western Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Wdata[which(Wdata$year==i),]
  Wdata2  <-eachyear [order(eachyear $built_up,decreasing=TRUE),]
  Wdata2  <- transform(Wdata2 , order =1:length(Wdata2$built_up))
  Wdata2<-data.frame(Wdata2[,-c(5,8)],
                     lnRank = log10(Wdata2$order-1/2))
  Wdata3 <- rbind(Wdata3, Wdata2)
}

Cdata <- zdata3[which(zdata3$AU_Regions =="Central Africa"),]
i<-1950
for (i in unique(zdata3$year))
{ 
  eachyear <- Cdata[which(Cdata$year==i),]
  Cdata2  <-eachyear [order(eachyear $built_up,decreasing=TRUE),]
  Cdata2  <- transform(Cdata2 , order =1:length(Cdata2$built_up))
  Cdata2<-data.frame(Cdata2[,-c(5,8)],
                     lnRank = log10(Cdata2$order-1/2))
  Cdata3 <- rbind(Cdata3, Cdata2)
}

plotdata<-rbind(Ndata3, Cdata3, Wdata3, Edata3, Sdata3)
#zipf's law拟合
plot2 <- ggplot(plotdata, aes(built_up, 10^lnRank, colour=factor(year)))+
  geom_point(size=0.3, shape=19, stroke = 0.3)+
  geom_abline(slope=-1,intercept=4,lty=2,colour="blue",size=0.4) + 
  facet_wrap(~AU_Regions, ncol = 3)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  labs (x=expression(paste("Built up area (", km^2,")")), 
        y=expression(paste("Rank-1/2")))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black")#,strip.background=element_blank()
  )+
  theme (legend.position = c (0.85,0.2),
         legend.background = element_blank(),
         legend.key=element_blank(),
         legend.key.size=unit(2,'mm'),
         legend.text=element_text(size =7, family = 'myFont'),
         legend.title=element_blank())+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme (strip.text.x = element_text(size = 8,margin = margin(1,0,1,0, "pt")))

plot2


# fig S5 ------------------------------------------------------------------
)
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
agglodata<-read.csv("agglo_data_2020.csv", sep=",", header = T)
names(agglodata)[2] <-"ISO"
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Republic of the Congo"]="Congo"
cregion[cregion=="Central African Republic"]="CAF"
agglodata1 <- merge(agglodata, cregion, by="ISO")
names(agglodata1)[3] <-"Population"
names(agglodata1)[4] <-"Builtup"

data15<-read.csv("agglo_data_2015.csv", sep=",", header = T)
names(data15)[2] <-"ISO"

datawhole<-merge(agglodata1,data15,by="Agglomeration_Name")
names(datawhole)[3] <-"pop20"
names(datawhole)[4] <-"built20"
names(datawhole)[8] <-"pop15"
names(datawhole)[9] <-"built15"
datawhole<-data.frame(datawhole,growth=(datawhole$pop20-datawhole$pop15)/datawhole$pop15)
DRCdata<-subset(datawhole,Country=="Dem.Rep.Congo")
EGYdata<-subset(datawhole,Country=="Egypt")
NIGdata<-subset(datawhole,Country=="Nigeria")
SAdata<-subset(datawhole,Country=="South Africa")
KNYdata<-subset(datawhole,Country=="Kenya")

pDRC<-ggplot(DRCdata, aes(growth))+
  geom_histogram(aes(y=..count..),
                 binwidth=0.035,
                 fill="#66C2A5")+
  #scale_x_log10(limits = c(10^4,10^7),labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous(limits = c(-1,4))+   
  labs(x="Population growth rate",
       y="Frequency")+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(color = "black",fill="white"),
        axis.line = element_line())+
  theme(legend.position ="none")
plot(pDRC)

pEGY<-ggplot(EGYdata, aes(growth))+
  geom_histogram(aes(y=..count..),
                 binwidth=0.035,
                 fill="#8DA0CB")+
  #scale_x_log10(limits = c(10^4,10^7),labels = trans_format("log10", math_format(10^.x)))+
  #scale_x_continuous(limits = c(-1,4))+  
  labs(x="Population growth rate",
       y="Frequency")+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(color = "black",fill="white"),
        axis.line = element_line())+
  theme(legend.position ="none")
plot(pEGY)

pNIG<-ggplot(NIGdata, aes(growth))+
  geom_histogram(aes(y=..count..),
                 binwidth=0.035,
                 fill="#A6D854")+
  #scale_x_log10(limits = c(10^4,10^7),labels = trans_format("log10", math_format(10^.x)))+
  scale_x_continuous(limits = c(-1,7))+   
  labs(x="Population growth rate",
       y="Frequency")+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(color = "black",fill="white"),
        axis.line = element_line())+
  theme(legend.position ="none")
plot(pNIG)

pSA<-ggplot(SAdata, aes(growth))+
  geom_histogram(aes(y=..count..),
                 binwidth=0.035,
                 fill="#E78AC3")+
  #scale_x_log10(limits = c(10^4,10^7),labels = trans_format("log10", math_format(10^.x)))+
  #scale_x_continuous(limits = c(-1,7))+   
  labs(x="Population growth rate",
       y="Frequency")+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(color = "black",fill="white"),
        axis.line = element_line())+
  theme(legend.position ="none")
plot(pSA)

pKNY<-ggplot(KNYdata, aes(growth))+
  geom_histogram(aes(y=..count..),
                 binwidth=0.035,
                 fill="#FC8D62")+
  #scale_x_log10(limits = c(10^4,10^7),labels = trans_format("log10", math_format(10^.x)))+
  #scale_x_continuous(limits = c(-1,5))+   #设置x轴刻度范围、刻度等
  labs(x="Population growth rate ",
       y="Frequency")+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_rect(color = "black",fill="white"),
        axis.line = element_line())+
  theme(legend.position ="none")
plot(pKNY)

tiff(file="各国城市人口增长率直方图15-20.tiff", res = 600, width = 4000, height = 800, compression = "lzw")
ggarrange(pDRC, pKNY,pEGY,pSA,pNIG,
          ncol=5,nrow=1,
          # labels = c("     Dem.Rep.Congo","Kenya","Egypt","South Africa","Nigeria"),
          # font.label = list(size = 7, color = "black", face = "bold", family = NULL),
          # #label.x = 0.5,label.y=0.97,
          align="v") 
dev.off()

# fig S6 ------------------------------------------------------------------

cregion<-read.csv("country_with_region.csv", sep=",", header = T)
citydata<-read.csv("城市人口.csv", sep=",", header = T)
names(citydata)[3] <-"ISO"
citydata1 <- merge(citydata, cregion, by="ISO")

datayear <- citydata1[which(citydata1[14]!=0),]
datayear <- datayear[which(datayear[14]>=10000),]
names(datayear)[14] <-"population"

ksresult<-ks.test(log10(datayear[14]),pnorm,alternative = "greater",exact = TRUE)
print(ksresult)
ksresult[1]
ksresult[2]

plist <- list()
for (j in 1:9){
  datayear <- citydata1[which(citydata1[j+5]!=0),]
  datayear <- datayear[which(datayear[j+5]>=10000),]
  names(datayear)[j+5] <-"population"
  ksresult<-ks.test(log10(datayear[j+5]),
                    pnorm,alternative = "greater",exact = TRUE)
  print(ksresult)
  plist[[j]]<-ggplot(datayear, aes(population,fill=AU_Regions))+
    scale_fill_brewer(palette = "Set2")+
    geom_histogram(aes(y=..count..),
                   binwidth=0.035)+
    scale_x_log10(limits = c(10^4,10^7),labels = trans_format("log10", math_format(10^.x)))+
    #scale_x_continuous(limits = c(4,7))+   
    labs(x="Population (person)",
         y="Frequency")+
    theme (axis.text=element_text(size =8), 
           axis.title = element_text(size=9))+
    theme(panel.grid.major =element_blank(),
          panel.grid.minor = element_blank(),
          panel.background =element_rect(color = "black",fill="white"),
          axis.line = element_line(colour ="black"))+
    theme(legend.position ="none")
  plot(plist[[j]])
  
}


tiff(file="1950-2020人口频数分布.tiff", res = 600, width = 3600, height = 3600, compression = "lzw")
ggarrange(plist[[1]], plist[[2]],plist[[3]],plist[[4]],plist[[5]], plist[[6]],plist[[7]],plist[[9]],
          ncol=3,nrow=3, 
          labels = c("1950","1960","1970","1980","1990","2000","2010","2020"),
          font.label = list(size = 9, color = "black", face = "bold", family = NULL),
          label.x = 0.65,label.y=0.95) 
dev.off()


# fig S7 ------------------------------------------------------------------
data2015<-read.csv("2015基期建成面积.csv", sep=",", header = T)
names(data2015)[3] <-"ISO"
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
data2015 <- merge(data2015, cregion, by="ISO")

tempdata<-data.frame()
result<-data.frame()

mod <- lm(data2015$X2020.2015 ~ data2015$log2015)
summary(mod)
f <- summary(mod)$fstatistic
tempdata<-data.frame(name="2020-2015",
                     cofficient=round(coef(mod)[2],3),
                     p=pf(f[1],f[2],f[3],lower.tail=F),
                     r_square=round(summary(mod)$r.squared,3),
                     base=2015,
                     n= length(data2010[,1]))
result<-rbind(result,tempdata)
plot1<- ggplot(data2015, aes(Built.up_2015, Built.up_2020/Built.up_2015))+#,color=AU_Regions
  geom_point(size=2, shape=21, stroke = 0.3,alpha=0.5,fill="steelblue1")+#
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  geom_smooth(method="lm",size=0.6, se=F, colour="black")+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(eq.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=3, label.x = 0.95, label.y = 0.88)+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(rr.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=3, label.x = 0.88, label.y = 0.78)+
  scale_color_brewer(palette = "Set2")+
  labs(x=expression(paste("Built up area in 2015(", km^2,")")),  
       y=expression(paste("Growth rate during 2015-2020")))+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme (strip.text.x = element_text(size = 9,margin = margin(1,0,1,0, "pt")))+
  theme (
    legend.position = c (0.8,0.8),
    legend.background = element_blank(),
    legend.key.size=unit(4,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =7))

plot1
plot11<- ggplot(data2015, aes(Built.up_2015, Built.up_2020/Built.up_2015,fill=AU_Regions))+#
  geom_point(size=1.5, shape=21, stroke = 0.2,alpha=0.5)+#,color="steelblue1"
  facet_wrap(~AU_Regions, ncol = 3)+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 2),labels = trans_format("log10", math_format(10^.x)))+
  geom_smooth(method="lm",size=0.4, se=F, colour="black")+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(eq.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2.5, label.x = 0.95, label.y = 0.90, colour="black")+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(rr.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2.5, label.x = 0.83, label.y = 0.80, colour="black")+
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  scale_fill_brewer(palette = "Set2")+
  labs(x=expression(paste("Built up area in 2015 (", km^2,")")), 
       y=expression(paste("Growth rate during 2015-2020")))+ 
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
  theme (strip.text=element_blank())+
  theme (
    legend.position = c (0.85,0.2),
    legend.background = element_blank(),
    legend.key.size=unit(4,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =8))

plot11


# fig S8 ------------------------------------------------------------------

cregion<-read.csv("country_with_region.csv", sep=",", header = T)
agglodata<-read.csv("agglo_data_2020.csv", sep=",", header = T)
names(agglodata)[2] <-"ISO"
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Republic of the Congo"]="Congo"
cregion[cregion=="Central African Republic"]="CAF"
agglodata1 <- merge(agglodata, cregion, by="ISO")
names(agglodata1)[3] <-"Population"
names(agglodata1)[4] <-"Builtup"

country1<-subset(agglodata1,Country=="Egypt")
country2<-subset(agglodata1,Country=="Nigeria")
country3<-subset(agglodata1,Country=="Chad")
country4<-subset(agglodata1,Country=="Somalia")
country5<-subset(agglodata1,Country=="Comoros")
country6<-subset(agglodata1,Country=="Namibia")
agglodata1<-rbind(country1,country2,country3,country4,country5,country6)

plist <- list()
j<-1
i<-"Egypt"
for (i in unique(agglodata1$Country))
{
  handleData <- agglodata1[which(agglodata1$Country==i),]
  handleData <- na.omit(handleData)
  #title <- handleData[]
  handleData$Population <- as.numeric(handleData$Population)
  handleData$Builtup<- as.numeric(handleData$Builtup)
  
  model <- lm(log10(Builtup)~log10(Population),data = handleData)
  slope <-round(coef(model)[2],4)
  intercept <- round(coef(model)[1],3)
  handleData$expection <- slope * log10(handleData$Population) + intercept
  handleData$residual <- log10(handleData$Builtup) - handleData$expection
  handleData$outlier <- 1

  # q25 <- quantile(handleData$residual, 0.25)
  # q75 <- quantile(handleData$residual, 0.75)
  # iqr <- q75 - q25
  # min <- q25 - 1.5 * iqr
  # max <- q75 + 1.5 * iqr
  max <- mean(handleData$residual, na.rm = T)+2*sd(handleData$residual, na.rm = T)
  min <- mean(handleData$residual, na.rm = T)-2*sd(handleData$residual, na.rm = T)
  handleData[which(handleData$residual >= min & handleData$residual <= max),]$outlier <- 0
  new.handleData <- handleData[which(handleData$outlier==0),]
  model <- lm(log10(Builtup)~log10(Population),data = new.handleData[which(new.handleData$Builtup > 0),])
  new.RSquare <- round(summary(model)$r.squared,4) #RSquare
  new.slope <- round(coef(model)[2],4) #斜率
  new.interval <- round(coef(model)[1],4) #截距
  new.down <- round(confint.lm(model)[2],4)   
  new.up <- round(confint.lm(model)[4],4)     #
  plist[[j]] <- ggplot(handleData, aes(Population, Builtup, colour=factor(outlier)))+  #
    geom_point(size=0.5)+
    scale_colour_manual(values =c ("#436ab3","#F08080"),
                        labels=c("Remaining samples","Outliers"))+
    geom_abline(slope = slope, 
                intercept = intercept+2*sd(handleData$residual, na.rm = T),
                size = 0.2,
                lty=2)+
    geom_abline(slope = slope, 
                intercept = intercept-2*sd(handleData$residual, na.rm = T), 
                size = 0.2,
                lty=2)+
    geom_smooth(colour="black", method="lm", se=F, size = 0.3)+
    geom_smooth(data=new.handleData,aes(Population, Builtup),  colour="red", method="lm", se=F,size=0.4)+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3),labels = trans_format("log10", math_format(10^.x)))+
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3),labels = trans_format("log10", math_format(10^.x)))+
    labs (x="Urban population (person)", 
          y=expression(paste("Built-up area (", km^2,")")))+
    # scale_y_continuous(limits=c(min(new.handleData$Builtup),max(new.handleData$Builtup)),
    #                    breaks=seq(0, max(new.handleData$Builtup), 1))+
    # scale_y_continuous(limits=c(0,2), breaks=seq(0, 2, 1))+
    theme (axis.text=element_text(size =6),
           axis.title = element_text(size=7))+
    theme(axis.line.x.bottom =element_line(linetype=1,color="black",size=0.2),
          axis.line.x.top = element_line(linetype = 1, color = "black", size = 0.2),
          axis.line.y.left  = element_line(linetype = 1, color = "black", size = 0.2),
          axis.line.y.right  = element_line(linetype = 1, color = "black", size = 0.2))+
    theme (legend.position = c(0.41,0.85),
           legend.background = element_blank(),
           legend.title=element_blank(),
           legend.key.size=unit(3,'mm'),
           legend.text=element_text(size =5))+
    theme(panel.background = element_rect(fill = 'transparent', color='black', linetype = 1))
  # annotate ("text",x=1000,y=10, parse = TRUE, label = paste0("beta==", round(new.slope,2)), size =2)+  #这里???0.911是剔除异常点后计算的
  # annotate ("text",x=1000,y=7, parse = TRUE, label = paste0("R^2==", round(new.RSquare,2)), size =2)
  print(plist[[j]])
  j<-j+1
  
  #IQR
  handleData <- agglodata1[which(agglodata1$Country==i),]
  handleData <- na.omit(handleData)
  #title <- handleData[]
  handleData$Population <- as.numeric(handleData$Population)
  handleData$Builtup<- as.numeric(handleData$Builtup)
  
  model <- lm(log10(Builtup)~log10(Population),data = handleData)
  slope <-round(coef(model)[2],4)
  intercept <- round(coef(model)[1],3)
  handleData$expection <- slope * log10(handleData$Population) + intercept
  handleData$residual <- log10(handleData$Builtup) - handleData$expection
  handleData$outlier <- 1
  #以下三步计算标记outliner
  q25 <- quantile(handleData$residual, 0.25)
  q75 <- quantile(handleData$residual, 0.75)
  iqr <- q75 - q25
  min <- q25 - 1.5 * iqr
  max <- q75 + 1.5 * iqr
  # max <- mean(handleData$residual, na.rm = T)+2*sd(handleData$residual, na.rm = T)
  # min <- mean(handleData$residual, na.rm = T)-2*sd(handleData$residual, na.rm = T)
  handleData[which(handleData$residual >= min & handleData$residual <= max),]$outlier <- 0
  new.handleData <- handleData[which(handleData$outlier==0),]
  model <- lm(log10(Builtup)~log10(Population),data = new.handleData[which(new.handleData$Builtup > 0),])
  new.RSquare <- round(summary(model)$r.squared,4) #RSquare
  new.slope <- round(coef(model)[2],4) 
  new.interval <- round(coef(model)[1],4) 
  new.down <- round(confint.lm(model)[2],4)    
  new.up <- round(confint.lm(model)[4],4)     
  plist[[j]] <- ggplot(handleData, aes(Population, Builtup, colour=factor(outlier)))+  #
    geom_point(size=0.5)+
    scale_colour_manual(values =c ("#436ab3","#F08080"),
                        labels=c("Remaining samples","Outliers"))+
    geom_abline(slope = slope, 
                intercept = intercept+max,
                size = 0.2,
                lty=2)+
    geom_abline(slope = slope, 
                intercept = intercept+min, 
                size = 0.2,
                lty=2)+
    geom_smooth(colour="black", method="lm", se=F, size = 0.3)+
    geom_smooth(data=new.handleData,aes(Population, Builtup),  colour="red", method="lm", se=F,size=0.4)+
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3),labels = trans_format("log10", math_format(10^.x)))+
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 3),labels = trans_format("log10", math_format(10^.x)))+
    labs (x="Urban population (person)", 
          y=expression(paste("Built-up area (", km^2,")")))+
    # scale_y_continuous(limits=c(min(new.handleData$Builtup),max(new.handleData$Builtup)),
    #                    breaks=seq(0, max(new.handleData$Builtup), 1))+
    # scale_y_continuous(limits=c(0,2), breaks=seq(0, 2, 1))+
    theme (axis.text=element_text(size =6),
           axis.title = element_text(size=7))+
    theme(axis.line.x.bottom =element_line(linetype=1,color="black",size=0.2),
          axis.line.x.top = element_line(linetype = 1, color = "black", size = 0.2),
          axis.line.y.left  = element_line(linetype = 1, color = "black", size = 0.2),
          axis.line.y.right  = element_line(linetype = 1, color = "black", size = 0.2))+
    theme (legend.position = c(0.41,0.85),
           legend.background = element_blank(),
           legend.title=element_blank(),
           legend.key.size=unit(3,'mm'),
           legend.text=element_text(size =5))+
    theme(panel.background = element_rect(fill = 'transparent', color='black', linetype = 1))
  # annotate ("text",x=1000,y=10, parse = TRUE, label = paste0("beta==", round(new.slope,2)), size =2)+  #这里???0.911是剔除异常点后计算的
  # annotate ("text",x=1000,y=7, parse = TRUE, label = paste0("R^2==", round(new.RSquare,2)), size =2)
  print(plist[[j]])
  j<-j+1
}

tiff(file="拟合方法对比.tif", res = 600, width = 3800, height = 2850,  compression = "lzw")
ggarrange(plist[[1]], plist[[2]], plist[[3]],plist[[4]], 
          plist[[5]], plist[[6]], plist[[7]], plist[[8]],
          plist[[9]],plist[[10]],plist[[11]],plist[[12]],
          ncol = 4,nrow=3,
          labels = c("a","", "b","", "c","", "d","", "e","", "f",""),
          font.label = list(size=8,color="black"), align="v")
dev.off()


# fig S9 ------------------------------------------------------------------
countryAfri <- ggplot(agglodata4, aes(Population, Builtup))+
  geom_point(size=0.6,alpha=0.5,shape=19, stroke = 0.2)+
  geom_smooth(method="lm",size=0.6, se=F, colour="red")+
  facet_wrap(~Country, ncol = 6)+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(eq.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2.2, label.x = 0.95, label.y = 0.24)+
  stat_poly_eq(formula = y~x, 
               aes(label =  paste(stat(rr.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=2.2, label.x = 0.88, label.y = 0.1)+
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(labels = trans_format("log10", math_format(10^.x)))+
  labs (x="Urban population (person)", 
        y=expression(paste("Built-up area (", km^2,")")))+
  theme (axis.text=element_text(size =7), 
         axis.title = element_text(size=9))+
  theme (strip.text.x = element_text(size = 7,margin = margin(1,0,1,0, "pt")),
         strip.background = element_blank())+
  theme(#panel.grid.major =element_blank(),
    #panel.grid.minor = element_blank(),
    panel.border =element_rect(fill=NA),
    panel.background =element_blank(),
    axis.line = element_line(colour ="black"))

countryAfri

# fig S10 -----------------------------------------------------------------

cregion<-read.csv("country_with_region.csv", sep=",", header = T)
agglodata<-read.csv("agglo_data_2020.csv", sep=",", header = T)
names(agglodata)[2] <-"ISO"
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Republic of the Congo"]="Congo"
cregion[cregion=="Central African Republic"]="CAF"
agglodata1 <- merge(agglodata, cregion, by="ISO")
names(agglodata1)[3] <-"Population"
names(agglodata1)[4] <-"Builtup"

agglodata4<-data.frame()
i <- "Angola"
for (i in unique(agglodata1$Country))
{
  eachCountry <- agglodata1[which(agglodata1$Country==i),]
  if(nrow(eachCountry)<10)  next()
  
  model <- lm(log10(Builtup)~log10(Population),data = eachCountry)
  slope <-round(coef(model)[2],4)
  intercept <- round(coef(model)[1],3)
  eachCountry$expection <- slope * log10(eachCountry$Population) + intercept
  eachCountry$residual <- log10(eachCountry$Builtup) - eachCountry$expection
  eachCountry$outlier <- 1
  

  # q25 <- quantile(eachCountry$residual, 0.25)
  # q75 <- quantile(eachCountry$residual, 0.75)
  # iqr <- q75 - q25
  # min <- q25 - 1.5 * iqr
  # max <- q75 + 1.5 * iqr
  max <- mean(eachCountry$residual, na.rm = T)+2*sd(eachCountry$residual, na.rm = T)
  min <- mean(eachCountry$residual, na.rm = T)-2*sd(eachCountry$residual, na.rm = T)
  eachCountry[which(eachCountry$residual >= min & eachCountry$residual <= max),]$outlier <- 0
  agglodata2 <- eachCountry[which(eachCountry$outlier==0),]
  numCity <- as.data.frame(table(unlist(agglodata2$ISO)))
  names(numCity)[1] <-"ISO"
  agglodata3 <- merge(agglodata2, numCity, by="ISO")
  agglodata3 <- agglodata3[which(agglodata3$Freq >9),]
  agglodata4 <- rbind(agglodata4, agglodata3)
  print(i)
}

crossexponent<-data.frame()
i <- "Angola"
for (i in unique(agglodata4$Country))
{
  eachCountry <- agglodata4[which(agglodata4$Country==i),]
  #if(i=="Equatorial Guinea"|i=="South Sudan")  next()
  
  mod <- lm(log10(eachCountry$Builtup) ~ log10(eachCountry$Population))
  summary (mod)
  r_square<-round(summary(mod)$r.squared,3)
  slope<-round(coef(mod)[2],3)
  intercept <- round(coef(mod)[1],3)
  low <- round(confint.lm(mod)[2],4)
  up <- round(confint.lm(mod)[4],4)
  tempdata<- data.frame(exponent_STD=slope, Country =i)
  crossexponent <- rbind(crossexponent, tempdata)
  print(i)
}


crossexponent 
crossexponent <- merge(crossexponent, cregion, by="Country")    
crossexponent <-crossexponent[order(crossexponent$exponent),]
crossexponent  <- transform(crossexponent , order =1:length(crossexponent$exponent))


crossexponent[crossexponent=="CAF"]="Central Africa Rep."
crossexponent[crossexponent=="Republic of the Congo"]="Congo"



agglodata5<-data.frame()
i <- "Angola"
for (i in unique(agglodata1$Country))
{
  eachCountry <- agglodata1[which(agglodata1$Country==i),]
  if(nrow(eachCountry)<10)  next()
  
  model <- lm(log10(Builtup)~log10(Population),data = eachCountry)
  slope <-round(coef(model)[2],4)
  intercept <- round(coef(model)[1],3)
  eachCountry$expection <- slope * log10(eachCountry$Population) + intercept
  eachCountry$residual <- log10(eachCountry$Builtup) - eachCountry$expection
  eachCountry$outlier <- 1
  
  #以下三步计算标记outliner
  q25 <- quantile(eachCountry$residual, 0.25)
  q75 <- quantile(eachCountry$residual, 0.75)
  iqr <- q75 - q25
  min <- q25 - 1.5 * iqr
  max <- q75 + 1.5 * iqr
  # max <- mean(eachCountry$residual, na.rm = T)+2*sd(eachCountry$residual, na.rm = T)
  # min <- mean(eachCountry$residual, na.rm = T)-2*sd(eachCountry$residual, na.rm = T)
  eachCountry[which(eachCountry$residual >= min & eachCountry$residual <= max),]$outlier <- 0
  agglodata2 <- eachCountry[which(eachCountry$outlier==0),]
  numCity <- as.data.frame(table(unlist(agglodata2$ISO)))
  names(numCity)[1] <-"ISO"
  agglodata3 <- merge(agglodata2, numCity, by="ISO")
  agglodata3 <- agglodata3[which(agglodata3$Freq >9),]
  agglodata5 <- rbind(agglodata5, agglodata3)
  print(i)
}

crossexponent1<-data.frame()
i <- "Angola"
for (i in unique(agglodata5$Country))
{
  eachCountry <- agglodata5[which(agglodata5$Country==i),]
  #if(i=="Equatorial Guinea"|i=="South Sudan")  next()
  
  mod <- lm(log10(eachCountry$Builtup) ~ log10(eachCountry$Population))
  summary (mod)
  r_square<-round(summary(mod)$r.squared,3)
  slope<-round(coef(mod)[2],3)
  intercept <- round(coef(mod)[1],3)
  low <- round(confint.lm(mod)[2],4)#
  up <- round(confint.lm(mod)[4],4)
  tempdata<- data.frame(exponent_IQR=slope, Country =i)
  crossexponent1 <- rbind(crossexponent1, tempdata)
  print(i)
}


crossexponent1 

crossexponent1[crossexponent1=="CAF"]="Central Africa Rep."
crossexponent1[crossexponent1=="Republic of the Congo"]="Congo"

plotdata<-merge(crossexponent, crossexponent1, by="Country")

p1 <- ggplot(plotdata, aes(exponent_STD, exponent_IQR,color=AU_Regions))+#
  geom_point(shape=19,size=1.5,alpha=0.8)+
  # geom_smooth(method="lm",size=0.6, se=F, colour="red")+
  # stat_poly_eq(formula = y~x,
  #              aes(label =  paste(stat(eq.label), sep = "~~~~")),
  #              rr.digits = 2, coef.digits = 2,parse = TRUE,
  #              size=2, label.x = 0.9, label.y = 0.2)+
  # stat_poly_eq(formula = y~x,
  #              aes(label =  paste(stat(rr.label), sep = "~~~~")),
  #              rr.digits = 2, coef.digits = 2,parse = TRUE,
  #              size=2, label.x = 0.88, label.y =0.1)+
  geom_abline(slope=1,intercept=0,lty=2,colour="red",size=0.4) + 
  geom_abline(slope=0,intercept=1,lty=2,colour="black",size=0.4) + 
  geom_vline(xintercept  = 1, colour="black",size=0.4,linetype="dashed")+
  #geom_abline(slope=0,intercept=5/6,lty=2,colour="black",alpha=0.5) + 
  #geom_vline(xintercept  = 5/6, colour="black",size=0.4,linetype="dashed",alpha=0.5)+
  labs(x="Scaling exponent (β) by STD", 
       y="Scaling exponent (β) by IQR",
       color="Region")+ 
  scale_color_brewer(palette = "Set2")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=9))+
  theme (
    legend.position = c (0.25,0.84),
    legend.background = element_blank(),
    legend.key.size=unit(2.2,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =6))
# geom_text(aes(label = Country), vjust = -1,size=1)
p2<-p1+ annotate("text",
                 x=plotdata[which(plotdata$Country=='Mauritania'),]$exponent_STD+0.04,
                 y=plotdata[which(plotdata$Country=='Mauritania'),]$exponent_IQR-0.02,
                 label = plotdata[which(plotdata$Country=='Mauritania'),]$Country,
                 size=2,
                 color="#8DA0CB")+
  annotate("text",
           x=plotdata[which(plotdata$Country=='Congo'),]$exponent_STD,
           y=plotdata[which(plotdata$Country=='Congo'),]$exponent_IQR+0.02,
           label = plotdata[which(plotdata$Country=='Congo'),]$Country,
           size=2,
           color="#66C2A5")+
  annotate("text",
           x=plotdata[which(plotdata$Country=='Togo'),]$exponent_STD,
           y=plotdata[which(plotdata$Country=='Togo'),]$exponent_IQR+0.03,
           label = plotdata[which(plotdata$Country=='Togo'),]$Country,
           size=2,
           color="#A6D854")+
  annotate("text",
           x=plotdata[which(plotdata$Country=='Cote d`Ivoire'),]$exponent_STD+0.02,
           y=plotdata[which(plotdata$Country=='Cote d`Ivoire'),]$exponent_IQR-0.02,
           label = plotdata[which(plotdata$Country=='Cote d`Ivoire'),]$Country,
           size=2,
           color="#A6D854")+
  annotate("text",
           x=plotdata[which(plotdata$Country=='Mozambique'),]$exponent_STD-0.02,
           y=plotdata[which(plotdata$Country=='Mozambique'),]$exponent_IQR+0.02,
           label = plotdata[which(plotdata$Country=='Mozambique'),]$Country,
           size=2,
           color="#E78AC3")

p2

