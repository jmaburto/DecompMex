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

library(reshape2)
library(latticeExtra)
library(data.table)
library(RColorBrewer)

load("Data/Temp_e0_results_smooth.RData")
source("R/Functions_fig.R")


### standardize temporary life expectancy with the maximum survival
temp.data <- temp.data[state < 33]
temp.data$sd.e <- 0
temp.data[age.g==1]$sd.e <- temp.data[age.g==1]$temp_e0/15
temp.data[age.g==2]$sd.e <- temp.data[age.g==2]$temp_e0/35
temp.data[age.g==3]$sd.e <- temp.data[age.g==3]$temp_e0/35
range(temp.data$sd.e)

### try two way anova
mod0 <- aov(sd.e ~ factor(age.g) + factor(state) + factor(year), data=temp.data)
anova(mod0)

### with interaction (makes sense)

interaction.plot(factor(temp.data$age.g), factor(temp.data$state),temp.data$sd.e)
### lines parallels, so not very important the interaction in this case

options(contrasts = c("contr.sum", "contr.poly"))
mod2.1 <- aov(temp_e0 ~ as.factor(age.g)*as.factor(state) + year, data=temp.data)
anova(mod2.1)
summary(lm(temp_e0 ~ factor(age.g)*factor(state),  data=temp.data))
hist(mod2.1$residuals,main="Histogram of residuals",xlab="Residuals")


# analyz the post hoc test
tk.test <- TukeyHSD(mod2.1)
tk.1 <- tk.test$`as.factor(age.g):as.factor(state)`
tk.1.names <- rownames(tk.1)
tk.1 <- as.data.table(tk.1)
setnames(tk.1, "p adj", "p")
tk.1$combination <- tk.1.names
head(tk.1)
rownames(tk.1)


# percentaje of siginificant different combinations
nrow(tk.1[p < .001])/nrow(tk.1) *100





# TR: begin Rank plot here. This code snippet can move elsewhere once finished.
# single out just needed e0 year ranges
dat <- temp.data
dat <- dat[dat$year >= 2010 & dat$state <= 32, ]
# get avg for 6 years starting 2010
dat <- data.table(dat)
dat <- dat[, list(mean_temp_e0 = mean(temp_e0)), by=list(sex,state,age.g)]

# get regions
state.code.recvec <- 
		c("Aguascalientes","Baja California","Baja California Sur","Campeche",
				"Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
				"Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
				"Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
				"Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
				"Tlaxcala","Veracruz","Yucatan","Zacatecas")
names(state.code.recvec) <- 1:32


# state and region codes
region.recvec            <- c(2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
		2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3)
names(region.recvec)     <- 1:32

head(dat)
dat$region               <- region.recvec[as.character(dat$state)]

table(dat$region) / 3 / 2


# Blues for North
oranges   <- colorRampPalette(brewer.pal(9,"Blues")[-c(1:3)], space = "Lab")(11)
# Purples Central
purples <- colorRampPalette(brewer.pal(9,"Purples")[-c(1:3)], space = "Lab")(11)
# Greens South
greens  <- colorRampPalette(brewer.pal(9,"Greens")[-c(1:3)], space = "Lab")(10)

# fatter lines for light colors 
lwdoranges  <- seq(from=4,to=1.4,length=11)# N (3_
lwdpurples 	<- seq(from=4,to=1.4,length=11)# C (2)
lwdgreens  	<- seq(from=4,to=1.4,length=10)# S (1)

# combine into vectors
colors 		<- c(greens, purples, oranges)
lwdvec 		<- c(lwdgreens, lwdpurples, lwdoranges)

# want darker lines for higher ranks? palettes go from light to dark, so order
# in ascending
young 		<- dat[dat$age.g == 1, ]
young 		<- young[order(young$region, young$sex, young$mean_temp_e0), ]

# order based on left column, which will have state labels. This refers
# to young age group, ergo, we need within
males <- dat[sex==1]
females <- dat[sex==2]

youngmales   <- males[males$age.g == 1, ]
youngmales   <- youngmales[order(youngmales$region, youngmales$mean_temp_e0), ]
youngfemales <- females[females$age.g == 1, ]
youngfemales <- youngfemales[order(youngfemales$region, youngfemales$mean_temp_e0), ]

# at the moment we have independent colors for males and females.
# that makes it tough to compare sexes. So probably just include
# males in the paper and females in an Appendix. Will need to point
# that out, but shouldn't matter since lines are labelled on left side.
colfemales        	<- colmales        <- colors
names(colmales)  	<- youngmales$state
names(colfemales) 	<- youngfemales$state
lwdfemales 			<- lwdmales <- lwdvec
names(lwdmales) 	<- youngmales$state
names(lwdfemales) 	<- youngfemales$state
# this matrix is no longer ordered.
m.mat   			<- acast(males, age.g ~ state, value.var = "mean_temp_e0")
f.mat   			<- acast(females, age.g ~ state, value.var = "mean_temp_e0")

# the final color ordering?
colmales 			<- colmales[colnames(m.mat)]
lwdmales        	<- lwdmales[colnames(m.mat)]

colfemales 			<- colfemales[colnames(f.mat)]
lwdfemales        	<- lwdfemales[colnames(f.mat)]


# assign pch to regions
pchvec        <- c(15,16,17)
names(pchvec) <- c(1,2,3)
# expand to match regions
pchvec        <- pchvec[as.character(region.recvec)]
# relabel to states, same order
names(pchvec) <- names(region.recvec)

pchmales      <- pchvec[colnames(m.mat)]
pchfemales    <- pchvec[colnames(f.mat)]


# how about state labels proper?
colnames(m.mat) <- state.code.recvec[colnames(m.mat)]
colnames(f.mat) <- state.code.recvec[colnames(f.mat)]

# rankplot skeleton function (minimal)
rankplot <- function(mat, 
		col,
		lwd, 
		pch,
		...){
	
	rank <- mat * 0
	for (i in 1:nrow(rank)){
		rank[i, ] <- rank(mat[i, ])
	}
	ys                <- rank - 1
	Nrank             <- ncol(ys)
	# ys               <- abs(Nrank - rankmat)
	laby              <- ys[1,]
	#ys[mat < 1e-5] <- NA
	xs                <- as.integer(rownames(ys))
	labels            <- colnames(ys)
	plot(NULL, 
			type = "n", 
			xlim = range(xs), 
			ylim = c(0, Nrank + 1), 
			axes = FALSE,
			xlab = "",
			ylab = "",
			...)
	text(min(xs),laby,labels,pos=2,xpd=TRUE)
	for (i in 1:Nrank){
		lines(xs, ys[,i ], col = col[i], lwd = lwd[i])
	}
	
	#points(rep(xs[1],Nrank), laby, col = col, pch=16,cex=1
	for (i in 1:Nrank){
		points(xs, ys[, i], col = col[i], pch=pch[i],cex=1.2)
	}
}

# display.brewer.all()
# pdf("Paper Figures/RankMales.pdf",width=5,height=8)
# par(mai=c(.5,1.5,.5,.5))
# 
# rankplot(m.mat, colmales, lwdmales, pchmales, 
# 		panel.first=
# 				list(
# 						rect(rep(.9,16),seq(1,31,by=2)-.5,rep(3.1,16),seq(2,32,by=2)-.5,col=gray(.92),border=NA)))
# text(1:3,33,c("Young (0-14)","Middle (15-49)","Older (50-84)"),xpd=TRUE)
# legend(x=1,y=-1,
# 		pch = c(17,16,15),
# 		col=c(oranges[5],purples[5],greens[5]),
# 		legend=c("North","Central","South"),
# 		bty="n",
# 		horiz=TRUE,
# 		xpd=TRUE,
# 		border = NA)
# 
# dev.off()

pdf("Manuscript/Supplemental material/RankFemales.pdf",width=5,height=8)
par(mai=c(.5,1.5,.5,.5))
rankplot(f.mat, colfemales, lwdfemales, 
		panel.first=
				list(
						rect(rep(.9,16),seq(1,31,by=2)-.5,rep(3.1,16),seq(2,32,by=2)-.5,col=gray(.92),border=NA)))
text(1:3,33,c("Young (0-14)","Middle (15-49)","Older (50-84)"),xpd=TRUE)
legend(x=1,y=-1,
		pch = c(17,16,15),
		col=c(oranges[5],purples[5],greens[5]),
		legend=c("North","Central","South"),
		bty="n",
		horiz=TRUE,
		xpd=TRUE,
		border = NA)
dev.off()



####################################  new graph with v shapes higlighted

# codes according to v and inverted v shapes
# get regions
state.code.recvec <- 
  c("Aguascalientes","Baja California","Baja California Sur","Campeche",
    "Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
    "Guanajuato","Guerrero","Hidalgo","Jalisco","Mexico State","Michoacan",
    "Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla","Queretaro",
    "Quintana Roo","San Luis Potosi","Sinaloa","Sonora","Tabasco","Tamaulipas",
    "Tlaxcala","Veracruz","Yucatan","Zacatecas")

names(state.code.recvec) <- 1:32

region.vshape <- c(2,2,2,2,2,
                   2,2,2,3,2,
                   2,1,3,2,2,
                   1,1,1,2,2,
                   3,3,2,2,1,
                   2,2,2,2,2,
                   3,1)

length(region.vshape)
names(region.vshape)     <- 1:32

head(dat)
dat$region.v               <- region.vshape[as.character(dat$state)]

d <- table(dat$region.v) / 3 / 2


# Blues for North
oranges   <-  rep("#7566AE",d[3])
oranges   <- colorRampPalette(brewer.pal(9,"Purples")[-c(1:3)], space = "Lab")(d[3])
# Purples Central
purples <- rep("lightgrey",d[2])
# Greens South
greens  <- rep("#278F48",d[1])
greens  <-colorRampPalette(brewer.pal(9,"Greens")[-c(1:3)], space = "Lab")(d[1])

# not fatter lines for light colors 
lwdoranges  <- rep(2,d[3])
lwdoranges  <- seq(from=4,to=2,length=d[3])# N (3_
lwdpurples 	<- rep(1,d[2])
  #seq(from=4,to=1.4,length=16)# C (2)
lwdgreens  	<- rep(2,d[1])
lwdgreens  	<- seq(from=4,to=2,length=d[1])# S (1)

# combine into vectors
colors 		<- c(greens, purples, oranges)
lwdvec 		<- c(lwdgreens, lwdpurples, lwdoranges)

# want darker lines for higher ranks? palettes go from light to dark, so order
# in ascending
young 		<- dat[dat$age.g == 1, ]
young 		<- young[order(young$region.v, young$sex, young$mean_temp_e0), ]

# order based on left column, which will have state labels. This refers
# to young age group, ergo, we need within
males    <- dat[dat$sex == 1, ]
females  <- dat[dat$sex == 2, ]

youngmales   <- males[males$age.g == 1, ]
youngmales   <- youngmales[order(youngmales$region.v, youngmales$mean_temp_e0), ]
youngfemales <- females[females$age.g == 1, ]
youngfemales <- youngfemales[order(youngfemales$region.v, youngfemales$mean_temp_e0), ]

# at the moment we have independent colors for males and females.
# that makes it tough to compare sexes. So probably just include
# males in the paper and females in an Appendix. Will need to point
# that out, but shouldn't matter since lines are labelled on left side.

colfemales        	<- colmales        <- colors
names(colmales)  	<- youngmales$state
names(colfemales) 	<- youngfemales$state
lwdfemales 			<- lwdmales <- lwdvec
names(lwdmales) 	<- youngmales$state
names(lwdfemales) 	<- youngfemales$state
# this matrix is no longer ordered.
m.mat   			<- acast(males, age.g ~ state, value.var = "mean_temp_e0")
f.mat   			<- acast(females, age.g ~ state, value.var = "mean_temp_e0")

# the final color ordering?
colmales 			<- colmales[colnames(m.mat)]
lwdmales        	<- lwdmales[colnames(m.mat)]

colfemales 			<- colfemales[colnames(f.mat)]
lwdfemales        	<- lwdfemales[colnames(f.mat)]


# assign pch to regions
pchvec        <- c(15,16,17)
names(pchvec) <- c(1,2,3)
# expand to match regions
pchvec        <- pchvec[as.character(region.recvec)]
# relabel to states, same order
names(pchvec) <- names(region.recvec)

pchmales      <- pchvec[colnames(m.mat)]
pchfemales    <- pchvec[colnames(f.mat)]


# how about state labels proper?
colnames(m.mat) <- state.code.recvec[colnames(m.mat)]
colnames(f.mat) <- state.code.recvec[colnames(f.mat)]

pdf("Paper Figures/RankMales_2.pdf",width=5,height=8,useDingbats = F)
par(mai=c(.5,1.5,.5,.5))

rankplot(m.mat, colmales, lwdmales, pchmales, 
         panel.first=
           list(
             rect(rep(.9,16),seq(1,31,by=2)-.5,rep(3.1,16),seq(2,32,by=2)-.5,col=gray(.95),border=NA)))
text(1:3,33,c("Young (0-14)","Middle (15-49)","Older (50-84)"),xpd=TRUE)
legend(x=1,y=-1,
       pch = c(17,16,15),
       col=c("black","black","black"),
       legend=c("North","Central","South"),
       bty="n",
       horiz=TRUE,
       xpd=TRUE,
       border = NA)

dev.off()


pdf("Manuscript/Supplemental material/RankFemales.pdf",width=5,height=8,useDingbats = F)
par(mai=c(.5,1.5,.5,.5))

rankplot(f.mat, colmales, lwdmales, pchmales, 
         panel.first=
           list(
             rect(rep(.9,16),seq(1,31,by=2)-.5,rep(3.1,16),seq(2,32,by=2)-.5,col=gray(.95),border=NA)))
text(1:3,33,c("Young (0-14)","Middle (15-49)","Older (50-84)"),xpd=TRUE)
legend(x=1,y=-1,
       pch = c(17,16,15),
       col=c("black","black","black"),
       legend=c("North","Central","South"),
       bty="n",
       horiz=TRUE,
       xpd=TRUE,
       border = NA)

dev.off()
