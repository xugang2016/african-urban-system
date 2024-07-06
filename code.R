library(ggplot2)
library(plyr)
library(gridExtra)
library(ggpubr)
library(ggpmisc)
library(sciplot)
library(Hmisc)
library(RColorBrewer)

##Please download the data from www.africapolis.org
setwd("D:/")
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
agglodata<-read.csv("agglo_data_2020.csv", sep=",", header = T)
names(agglodata)[2] <-"ISO"
agglodata1 <- merge(agglodata, cregion, by="ISO")
names(agglodata1)[3] <-"Population"
names(agglodata1)[4] <-"Builtup"
agglodata2<-data.frame(agglodata1,
                       logPop = log10(agglodata1$Population),
                       logArea = log10(agglodata1$Builtup))
cregion[cregion=="Democratic Republic of the Congo"]="Dem.Rep.Congo"
cregion[cregion=="Central African Republic"]="CAF"

# FigS4 -------------------------------------------------------------------
countryAfri <- ggplot(agglodata3, aes(logPop, logArea))+
  geom_point(size=0.8)+
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
  labs (x=expression(paste(Log[10],"[Urban population (person)]")), 
        y=expression(paste(Log[10],"[Built-up area (", km^2,")]")))+
  theme (axis.text=element_text(size =7), 
         axis.title = element_text(size=9))+
  theme (strip.text.x = element_text(size = 7,margin = margin(1,0,1,0, "pt")))

countryAfri
tiff(file="FigureS4.tiff", res = 600, width = 3600, height = 3800, compression = "lzw")
countryAfri
dev.off()


# Fig4b -------------------------------------------------------------------
crossexponent<-data.frame()
i <- "Angola"
for (i in unique(agglodata3$Country))
{
  eachCountry <- agglodata3[which(agglodata3$Country==i),]
 
  mod <- lm(eachCountry$logArea ~ eachCountry$logPop)
  summary (mod)
  r_square<-round(summary(mod)$r.squared,3)
  slope<-round(coef(mod)[2],3)
  intercept <- round(coef(mod)[1],3)
  low <- round(confint.lm(mod)[2],4)
  up <- round(confint.lm(mod)[4],4)
  tempdata<- data.frame(exponent=slope, low2.5=low, up97.5=up, 
                        r_square=  r_square,  intercept=  intercept, Country =i)
  crossexponent <- rbind(crossexponent, tempdata)
  print(i)
}


crossexponent 
crossexponent <- merge(crossexponent, cregion, by="Country")    
crossexponent <-crossexponent[order(crossexponent$exponent),]
crossexponent  <- transform(crossexponent , order =1:length(crossexponent$exponent))


crossexponent[crossexponent=="CAF"]="Central Africa Rep."
crossexponent[crossexponent=="Republic of the Congo"]="Congo"
write.table(crossexponent,"country_exponent.csv",row.names=FALSE,col.names=TRUE,sep=",")

expoAll <- ggplot(crossexponent,aes( x=exponent,y=order,color=AU_Regions))+
  geom_point()+
  geom_vline(xintercept  = 1, colour="black",size=0.6,linetype="dashed")+
  geom_errorbarh(aes(xmin = low2.5, xmax = up97.5),  size=0.4)+
  scale_y_continuous(breaks =c(1:length(crossexponent$exponent)),
                     labels = crossexponent$Country)+
  labs(y="African countries", 
       x=expression(paste("Urban settlement scaling exponent"," (",beta,")"))
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
    legend.position = c(1,0),
    legend.justification = c("right", "bottom"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.box.background = element_rect(fill=NA,color=NA), 
    legend.text=element_text(size =7.5), 
    legend.key.size=unit(4,'mm'),
    legend.title = element_blank()
  )

expoAll
tiff(file="Figure4b.tiff", res = 600, width = 2000, height = 3200, compression = "lzw")
expoAll
dev.off() 

# Fig4a -----------------------------------------------------------------
Sdata <- agglodata2[which(agglodata1$AU_Regions =="Southern Africa"),]
lm(Sdata$logArea ~ Sdata$logPop)##-2.5663,0.8078 #15£º -2.7900        0.8441
Sdata<-data.frame(Sdata,
                  sami=Sdata$logArea-0.8441*Sdata$logPop+2.7900 )
Sdata1 <- Sdata[which(Sdata$sami < 2*sd(Sdata$sami)),]
Sdata2<-Sdata1[which(Sdata1$sami > -2*sd(Sdata$sami)),]

Edata <- agglodata2[which(agglodata1$AU_Regions =="Eastern Africa"),]
lm(Edata$logArea ~ Edata$logPop)##-3.615,1.008 #15£º      -4.096         1.101 
Edata<-data.frame(Edata,
                  sami=Edata$logArea-1.101*Edata$logPop+4.096 )
Edata1 <- Edata[which(Edata$sami < 2*sd(Edata$sami)),]
Edata2<-Edata1[which(Edata1$sami > -2*sd(Edata$sami)),]

Wdata <- agglodata2[which(agglodata1$AU_Regions =="Western Africa"),]
lm(Wdata$logArea ~ Wdata$logPop)##-3.755,1.008 #15£º      -3.881         1.029  
Wdata<-data.frame(Wdata,
                  sami=Wdata$logArea-1.029*Wdata$logPop+3.881)
Wdata1 <- Wdata[which(Wdata$sami < 2*sd(Wdata$sami)),]
Wdata2<-Wdata1[which(Wdata1$sami > -2*sd(Wdata$sami)),]

Cdata <- agglodata2[which(agglodata1$AU_Regions =="Central Africa"),]
lm(Cdata$logArea ~ Cdata$logPop)##-3.0980,0.8661 #15£º     -3.3868        0.9228 
Cdata<-data.frame(Cdata,
                  sami=Cdata$logArea-0.9228*Cdata$logPop+3.3868)
Cdata1 <- Cdata[which(Cdata$sami < 2*sd(Cdata$sami)),]
Cdata2<-Cdata1[which(Cdata1$sami > -2*sd(Cdata$sami)),]

Ndata <- agglodata2[which(agglodata1$AU_Regions =="Northern Africa"),]
lm(Ndata$logArea ~ Ndata$logPop)##-3.4307,0.9297   #15£º   -4.073         1.016 
Ndata<-data.frame(Ndata,
                  sami=Ndata$logArea-1.016*Ndata$logPop+4.073)
Ndata1 <- Ndata[which(Ndata$sami < 2*sd(Ndata$sami)),]
Ndata2<-Ndata1[which(Ndata1$sami > -2*sd(Ndata$sami)),]

plotdata<-rbind(Ndata2, Cdata2, Wdata2, Edata2, Sdata2)

plot1 <- ggplot(plotdata, aes(logPop, logArea, colour=factor(AU_Regions)))+
  geom_point(size=0.5, shape=19,stroke = 0.01, alpah=0.5)+
  scale_color_brewer(palette = "Set2")+
  stat_poly_eq(aes(label = paste(..eq.label.., ..adj.rr.label.., 
                                 sep = '~~~~~~~~')),
               formula = y ~ x,label.x = 0.98,label.y = c(0.25,0.19,0.13,0.07,0.01), parse = T,size=2.2,
               vstep = 0.06, family = 'myFont') + 
  labs (x=expression(paste(Log[10],"[Urban population (person)]")), 
        y=expression(paste(Log[10],"[Built-up area (", km^2,")]")))+
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
tiff(file="Figure4a.tiff", res = 600, width = 1500, height = 1600, compression = "lzw")
plot1
dev.off()


# Fig4c -------------------------------------------------------------------
mydata15<-read.csv("country_exponent_2015.csv", sep=",", header = T)
mydata20<-read.csv("country_exponent_2020.csv", sep=",", header = T)
mydata15 <- mydata15[,c(1,2,8)]
mydata20 <- mydata20[,c(1,2)]
names(mydata20)[1] <-"country"
contradata <- merge(mydata15, mydata20, by="country")  
names(contradata)[2] <-"beta15"
names(contradata)[4] <-"beta20"

p1 <- ggplot(contradata, aes(beta15, beta20,label=country,color=AU_Regions))+#
  geom_point(shape=19,size=1.5,alpha=0.8)+
  geom_abline(slope=1,intercept=0,lty=2,colour="red",size=0.4) + 
  geom_abline(slope=0,intercept=1,lty=2,colour="black",size=0.4) + 
  geom_vline(xintercept  = 1, colour="black",size=0.4,linetype="dashed")+
  labs(x="Scaling exponent in 2015", 
       y="Scaling exponent in 2020",
       color="Region")+ 
  scale_color_brewer(palette = "Set2")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme(axis.text=element_text(size =7), 
        axis.title = element_text(size=9))+
  theme (
    legend.position = c (0.84,0.17),
    legend.background = element_blank(),
    legend.key.size=unit(2.2,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =6))

p2<-p1+geom_text(hjust=0.4, vjust=2,size=2,check_overlap = TRUE)
tiff(file="Figure4b.tiff", res = 600, width = 1500, height = 1600, compression = "lzw")
p2
dev.off()

# Fig2b -------------------------------------------------------------------
zdata<-read.csv("1950-2020poprank.csv", sep=",", header = T)
zdata2<-data.frame(zdata,
                   lnPop = log10(zdata$population),
                   lnRank = log10(zdata$rank-1/2))
crossexponent<-data.frame()
i <- 1950
for (i in unique(zdata2$year))
{
  eachyear <- zdata2[which(zdata2$year==i),]
  
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
  geom_point(size=3, shape=19, stroke = 0.3,color="rosybrown1")+
  geom_abline(slope=0,intercept=1,lty=2,size=0.4) + 
  geom_ribbon(aes(ymin=-low2.5, ymax = -up97.5),linetype="dashed",
              alpha = 0.1, size=0.2)+
  geom_line(aes(Year, -exponent),
            colour="steelblue1",size=1)+
  scale_x_continuous(breaks = scales::breaks_width(20))+
  labs(x="Year", 
       y="Exponent")+  #"7-day average cases \n(thousand)"
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))

plot

tiff(file="Figure2b.tiff", res = 600, width = 1400, height = 1400, compression = "lzw")
plot
dev.off()


# Fig2a -------------------------------------------------------------------
plot1 <- ggplot(zdata2, aes(lnPop, lnRank, colour=factor(year)))+
  geom_point(size=0.3, shape=19, stroke = 0.3)+
  geom_abline(slope=-1,intercept=8.25,lty=2,colour="blue",size=0.4) + 
  scale_x_continuous(breaks = scales::breaks_width(1))+
  scale_y_continuous(breaks = scales::breaks_width(1))+
  labs (x=expression(paste(Log[10],"[Population]")), 
        y=expression(paste(Log[10],"[Rank-1/2]")))+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme (axis.text=element_text(color="black",size=9,angle = 0,family = 'myFont'), 
         axis.title = element_text(size=10, family = 'myFont'))+
  theme (legend.position = c (0.11,0.3),
         legend.background = element_blank(),
         legend.key=element_blank(),
         legend.key.size=unit(1,'mm'),
         legend.text=element_text(size =8, family = 'myFont'),
         legend.title=element_blank())
#+scale_color_discrete(breaks=c("Southern Africa","Central Africa","Northern Africa","Eastern Africa","Western Africa"))

plot1
tiff(file="Figure2a.tiff", res = 600, width = 1400, height = 1400, compression = "lzw")
plot1
dev.off()


# Fig2c -------------------------------------------------------------------
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

#z exponent
plot3<- ggplot(crossexponent, aes(Year, -exponent,colour=factor(Region)))+
  geom_point(size=0.8, shape=19, stroke = 0.2)+
  # scale_color_brewer(palette = "Set2")+
  geom_abline(slope=0,intercept=1,lty=2,size=0.4) + 
  geom_ribbon(aes(ymin=-low2.5, ymax = -up97.5,fill=factor(Region)),colour=NA,
              alpha = 0.1)+
  geom_line(aes(Year, -exponent,colour=factor(Region)),
            size=0.4)+
  scale_x_continuous(breaks = scales::breaks_width(20))+
  labs(x="Year", 
       y="Exponent")+  #"7-day average cases \n(thousand)"
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black",size = 0.4))+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=10))+
  theme (strip.text.x = element_text(size = 8,margin = margin(1,0,1,0, "pt")))+
  #theme(legend.position = "none")
  theme(legend.position = c(0.8,0.85),
        legend.title = element_blank(),
        legend.key=element_blank(),
        legend.key.size=unit(3,'mm'),
        legend.text=element_text(size =6, family = 'myFont'),)


plot3

tiff(file="Figure2c.tiff", res = 600, width = 1400, height = 1400, compression = "lzw")
plot3
dev.off()

# Fig2d -------------------------------------------------------------------
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
write.table(zdata4,"zdata4.csv",row.names=FALSE,col.names=TRUE,sep=",")

j <- "Angola"

for (j in unique(zdata4$Country))
{ 
  eachCountry <- zdata4[which(zdata4$Country==j),]
  if(length(unique(eachCountry$year))<9)  next()
  zdata5 <- rbind(zdata5, eachCountry)
  print(j)
}

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
  geom_point(size=0.8, shape=19, stroke = 0.2)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_brewer(palette = "Paired")+
  geom_abline(slope=0,intercept=1,lty=2,size=0.4) + 
  geom_ribbon(aes(ymin=-low2.5, ymax = -up97.5,fill=factor(Region)),colour=NA,
              alpha = 0.1)+
  geom_line(aes(Year, -exponent,colour=factor(Region)),
            size=0.4)+
  scale_x_continuous(breaks = scales::breaks_width(20))+
  labs(x="Year", 
       y="Exponent")+  
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black",size = 0.4))+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=10))+
  theme (strip.text.x = element_text(size = 8,margin = margin(1,0,1,0, "pt")))+
  theme(legend.position = "none")
# scale_color_manual(values = c('orange','#72be64'))+
# scale_fill_manual(values = c('orange','#72be64'))
# theme(legend.position = c(0.85,0.9),
#   legend.title = element_blank(),
#       legend.key=element_blank(),
#       legend.key.size=unit(3,'mm'),
#       legend.text=element_text(size =6, family = 'myFont'))


plot5

tiff(file="Figure2d.tiff", res = 600, width = 1400, height = 1400, compression = "lzw")
plot5
dev.off()


# Fig2e -------------------------------------------------------------------
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
pareto<-read.csv("2020poprank.csv", sep=",", header = T)
pareto1 <- merge(pareto, cregion, by="Country")
urbaniz<-read.csv("2020urbanization.csv", sep=",", header = T)
names(urbaniz)[2] <-"ISO"
data <- merge(pareto1, urbaniz, by="ISO")

p1 <- ggplot(data, aes(X2020, -exponent))+#
  geom_point(size=2.5, shape=19, stroke = 0.3,alpha=0.5,color="steelblue1")+
  geom_smooth(method="lm",size=0.6, se=F,color="black")+
  stat_poly_eq(formula = y~x,
               aes(label =  paste(stat(eq.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=3, label.x = 0.9, label.y = 0.9)+
  stat_poly_eq(formula = y~x,
               aes(label =  paste(stat(rr.label), sep = "~~~~")),
               rr.digits = 2, coef.digits = 2,parse = TRUE,
               size=3, label.x = 0.88, label.y =0.8)+
  labs(x="Urbanization level (%)", 
       y="Pareto exponent in 2020",
       color="Region")+ 
  scale_color_brewer(palette = "Set2")+
  theme(panel.grid.major =element_blank(),
        panel.grid.minor = element_blank(),
        panel.background =element_blank(),
        axis.line = element_line(colour ="black"))+
  theme(axis.text=element_text(size =8), 
        axis.title = element_text(size=10))+
  theme (
    legend.position = c (0.25,0.85),
    legend.background = element_blank(),
    legend.key.size=unit(2.2,'mm'),
    legend.key=element_blank(),
    legend.title = element_blank(),
    legend.text=element_text(size =7))

tiff(file="urban-pareto.tiff", res = 600, width = 1400, height = 1400, compression = "lzw")
p1
dev.off()


# Fig3a -------------------------------------------------------------------
data2010<-read.csv("2010base.csv", sep=",", header = T)
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

plot1<- ggplot(data2010, aes(log2010, X2020.2010))+
  geom_point(size=2.5, shape=19, stroke = 0.3,alpha=0.5,color="steelblue1")+
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  scale_color_brewer(palette = "Set2")+
  labs(x=expression(paste(Log[10],"[Urban population (person)]")), 
       y=expression(paste(Log[10],"[Growth rate]")))+  
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
tiff(file="Figure3a.tiff", res = 600, width = 1500, height = 1600, compression = "lzw")
plot1
dev.off()


# Fig3b -------------------------------------------------------------------
plot11<- ggplot(data2010, aes(log2010, X2020.2010,color=AU_Regions))+#
  geom_point(size=1.5, shape=19, stroke = 0.3,alpha=0.5)+#,color="steelblue1"
  facet_wrap(~AU_Regions, ncol = 3)+
  theme (axis.text=element_text(size =8), 
         axis.title = element_text(size=9))+
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
  scale_color_brewer(palette = "Set2")+
  labs(x=expression(paste(Log[10],"[Urban population (person)]")), 
       y=expression(paste(Log[10],"[Growth rate]")))+ 
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
tiff(file="Figure3b.tiff", res = 600, width = 2300, height = 1600, compression = "lzw")
plot11
dev.off()


# Fig3c,3d -------------------------------------------------------------------
brewer.pal(n = 12, name = "Paired")
result<-data.frame()
#1950
data1950<-read.csv("1950base1.csv", sep=",", header = T)
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

#1960
data1960<-read.csv("1960base1.csv", sep=",", header = T)
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

#1970
data1970<-read.csv("1970base1.csv", sep=",", header = T)
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


#1980
data1980<-read.csv("1980base1.csv", sep=",", header = T)
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


#1990
data1990<-read.csv("1990base1.csv", sep=",", header = T)
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


#2000
data2000<-read.csv("2000base1.csv", sep=",", header = T)
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

#2010
data2010<-read.csv("2010base1.csv", sep=",", header = T)
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
  labs(y="Linear regression coefficient", 
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
tiff(file="Figure3c.tiff", res = 600, width = 1500, height = 1600, compression = "lzw")
wholeplot
dev.off()

regiondata<-result[which(result$name !="whole"),]
regionplot<- ggplot(regiondata,aes( x=base,y=cofficient,color=name))+#
  facet_wrap(~name, ncol = 3)+
  scale_color_brewer(palette = "Dark2")+
  geom_point(size=1.5, shape=19, stroke = 0.3)+
  geom_abline(slope=0,intercept=0,lty=2,size=0.4) +
  #geom_line(aes(base, cofficient),colour="grey",size=0.6)+
  scale_x_continuous(breaks = scales::breaks_width(10))+
  geom_errorbar(aes(ymin = low, ymax = up),size=0.4,width=4)+
  labs(y="Linear regression coefficient", 
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
tiff(file="Figure3d
     .tiff", res = 600, width = 2300, height = 1600, compression = "lzw")
regionplot
dev.off()






# FigS3 -------------------------------------------------------------------
cregion<-read.csv("country_with_region.csv", sep=",", header = T)
citydata<-read.csv("urbanpop.csv", sep=",", header = T)
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
  plist[[j]]<-ggplot(datayear, aes(log10(population),fill=AU_Regions))+
    scale_fill_brewer(palette = "Set2")+
    geom_histogram(aes(y=..count..),
                   binwidth=0.035)+
    scale_x_continuous(limits = c(4,7))+   
    labs(x=expression(paste(Log[10],"[Population (person)]")),
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


tiff(file="FigureS3.tiff", res = 600, width = 3600, height = 3600, compression = "lzw")
ggarrange(plist[[1]], plist[[2]],plist[[3]],plist[[4]],plist[[5]], plist[[6]],plist[[7]],plist[[9]],
          ncol=3,nrow=3, 
          labels = c("1950","1960","1970","1980","1990","2000","2010","2020"),
          font.label = list(size = 9, color = "black", face = "bold", family = NULL),
          label.x = 0.65,label.y=0.95) 
dev.off()

