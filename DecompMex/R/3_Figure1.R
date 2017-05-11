  
  if (system("hostname",intern=TRUE) == "ADM-108625") {
    setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")
  } else {
    if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
      # if I'm on the laptop
      setwd("/home/tim/git/DecompMex/DecompMex")
    } else {
      # in that case I'm on Berkeley system, and other people in the dept can run this too
      setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
    }}
  
  
  
  library(latticeExtra)
  library(data.table)
  
  load("Data/Temp_e0_results.RData")
  source("R/Functions_fig.R")
  
  fig1.data <- temp.data
  fig1.data$age.g <- factor(fig1.data$age.g,levels = 1:3, labels = c("Young (0-14)","Young adults (15-49)","Older adults (50-84)"))
  fig1.data$sex<- factor(fig1.data$sex,levels = 1:2, labels = c("Males","Females"))
  mean.state <- fig1.data[,list(temp_e0=mean(temp_e0)),  by = list(year,sex,age.g)]
  mean.state$state <- 35
  fig.names <- colnames(fig1.data)
  head(mean.state)
  
  fig1.data <- rbind(fig1.data,mean.state)
  
  col1 <- makeTransparent("darkgrey",alpha=80)
  col2 <- "black"
  col3 <- brewer.pal(7,"Blues")[c(6)]
  
  # to do: y-labels horizontal, check womens downward in 2006-2010
  
  Fig1 <- useOuterStrips(xyplot(temp_e0 ~ year|age.g+sex,groups = state,data = fig1.data[state!=34],type="l",
         col= c(rep(col1,32),col3,"black"),lwd=2.5,between=list(x=1,y=1),
         #col= c(rep(col1,6),rep(col2,3),rep(col1,2),col2,rep(col1,14),col2,rep(col1,5),col3),lwd=2,between=list(x=1,y=1),
         key=list(space="bottom",background="transparent",text=list(rev(c('State',"Average",'Low benchmark')),
                                col="black"),cex=1,
                  lines=list(lty=1,lwd=2,col=rev(c("darkgrey","black",brewer.pal(7,"Blues")[c(6)])))),
         xlab="",ylab = "Years",par.settings=my.settings,par.strip.text=list(cex=1.3),
         scales = list(alternating=1,x=list(cex=.9,at=c(seq(1990,2015,5))),y = list(relation = "free",cex=.9,rot=0),tck=c(1,0)),
         ylim = list(c(14,15),
                     c(24,35),
                     c(24,35)),
         panel = function(x, y, ...){                        
           panel.abline(v=c(seq(1990,2015,5)),col='dark grey',lty=3)				
           panel.abline(h=list(c(15),c(35),c(35)),col="black",lty=1)				
           #panel.abline(h=c(25),col="black",lty=1)				
           #panel.abline(h=c(35),col="black",lty=1)				
           panel.xyplot(x, y,lty=1,...)	
         }),strip.left = T, strip=F)
  
  Fig1
    
  
  
  
  # print and save figures
  pdf("Paper Figures/Figure1.pdf",width=12,height=7)
  print(Fig1)
  dev.off()


### some querys for the paper
fig1.data$Statenom <- fig1.data$state
fig1.data$Statenom  <- factor(fig1.data$Statenom ,levels=c(1:34),
                              labels=c("Aguascalientes","Baja California","Baja California Sur",
                                       "Campeche","Coahuila","Colima","Chiapas","Chihuahua",
                                       "Mexico City","Durango","Guanajuato","Guerrero",
                                       "Hidalgo","Jalisco","Mexico State","Michoacan","Morelos",
                                       "Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
                                       "Quintana Roo","San Luis Potosi","Sinaloa","Sonora",
                                       "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatan",
                                       "Zacatecas","Benchmark","Record"))

tab.state <- function(age,yr,sx){
setorder(fig1.data[year == yr & age.g==as.character(unique(fig1.data$age.g)[age]) & sex==sx],temp_e0)[]}

  
  tab.state(1,2010,"Females")
  tab.state(1,2010,"Males")
  
  
  g <- 3
  y <- 2015
  mean(tab.state(g,y,"Males")$temp_e0[-c(34,35)])
  mean(tab.state(g,y,"Females")$temp_e0[-c(34,35)])
  
  p1  <- sd(tab.state(g,y,"Males")$temp_e0[-c(34,35)])
  p2 <- sd(tab.state(g,y,"Females")$temp_e0[-c(34,35)])
  
  
  mean(tab.state(g,y,"Females")$temp_e0[-c(34,35)]) - s.e*p2
  mean(tab.state(g,y,"Females")$temp_e0[-c(34,35)]) + s.e*p2
  
  mean(tab.state(g,y,"Males")$temp_e0[-c(34,35)]) - s.e*p1
  mean(tab.state(g,y,"Males")$temp_e0[-c(34,35)]) + s.e*p1
  
  
  
  mean(tab.state(2,2015,"Males")$temp_e0[-c(34,35)]) + s.e*p1
  mean(tab.state(2,2015,"Females")$temp_e0[-c(34,35)]) + s.e*p2
  
  
  s.e <- qnorm(p = .975,mean = 0,sd = 1)
  
  
  tab.state(1,2015,"Males")

mean(tab.state(2,2015,"Males")$temp_e0[-c(33,34)])

tab.state(2,2005,"Males")
tab.state(2,2010,"Males")

mean(tab.state(2,2010,"Males")$temp_e0[-c(33,34)])

mean(tab.state(2,2015,"Males")$temp_e0[-c(33,34)])
  
  
mean(tab.state(3,1990,"Females")$temp_e0[-c(33,34)])
mean(tab.state(3,2000,"Females")$temp_e0[-c(33,34)])
mean(tab.state(3,2010,"Females")$temp_e0[-c(33,34)])
mean(tab.state(3,2015,"Females")$temp_e0[-c(33,34)])


mean(tab.state(3,1990,"Males")$temp_e0[-c(33,34)])
mean(tab.state(3,2000,"Males")$temp_e0[-c(33,34)])
mean(tab.state(3,2010,"Males")$temp_e0[-c(33,34)])
mean(tab.state(3,2015,"Males")$temp_e0[-c(33,34)])



model.data <- fig1.data[fig1.data$state < 33 & fig1.data$sex == 'Males' & fig1.data$age.g == unique(fig1.data$age.g)[3],]
model.data2 <- fig1.data[fig1.data$state < 33 & fig1.data$sex == 'Females' & fig1.data$age.g == unique(fig1.data$age.g)[3],]
model.data3 <- fig1.data[fig1.data$state < 33 & fig1.data$age.g == unique(fig1.data$age.g)[3],]

m1 <- lm(temp_e0 ~ year , data = model.data)
m2 <- lm(temp_e0 ~ year , data = model.data2)
m3 <- lm(temp_e0 ~ year + sex, data = model.data3)
summary(m1)
summary(m2)
summary(m3)





for (j in 1:32){
  plot(setorder(fig1.data[state == j & age.g=="Young (0-14)" & sex=="Females"],year)$temp_e0, type="l")
  print(j)
Sys.sleep(2)
}

