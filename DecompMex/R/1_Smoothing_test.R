### Based on version 1.
#######################################
# IMPORTANT: impute zeros for missing ages, causes, states, years
# otherwise BP is too high...
rm(list=ls(all=TRUE))

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
library(data.table)
library(MortalitySmooth)
library(reshape2)
# Load data and group causes according to paper ---------------------------

#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

load('Data/Counts&Rates_1990-2015Mex.RData')

# ---------------------------------------------------------
#dim(Data_Counts)
#[1] 183040     22
#head(Data_Counts)
#year sex state age   Pop         1 2        3          4 5        6 7 8 9 10
#1: 1990   1     1   0 11130 86.025661 0 0.000000 196.340921 0 5.060333 0 0 0  0
#2: 1990   1     1   1 10830 16.193066 0 0.000000   3.036200 0 0.000000 0 0 0  0
#3: 1990   1     1   2 10601  2.024133 0 0.000000   1.012067 0 1.012067 0 0 0  0
#4: 1990   1     1   3 10413  3.036200 0 1.012067   0.000000 0 0.000000 0 0 0  0
#5: 1990   1     1   4 10220  1.012067 0 0.000000   2.024133 0 1.012067 0 0 0  0
#6: 1990   1     1   5 10095  0.000000 0 0.000000   0.000000 0 0.000000 0 0 0  0
#11 12       13 14       15         16      total
#1:  0  0 1.012067  0 4.048266 104.242860 396.730108
#2:  0  0 1.012067  0 0.000000   8.096533  28.337865
#3:  0  0 0.000000  0 0.000000   3.036200   7.084466
#4:  0  0 1.012067  0 0.000000   3.036200   8.096533
#5:  0  0 1.012067  0 0.000000   4.048266   9.108599
#6:  0  0 2.024133  0 0.000000   2.024133   4.048266

# ----------------------------------------------
#dim(Data_rates)
#[1] 183040     22
#head(Data_rates)
#year sex state age   Pop            1 2            3            4 5
#1: 1990   1     1   0 11130 7.729170e-03 0 0.000000e+00 1.764069e-02 0
#2: 1990   1     1   1 10830 1.495205e-03 0 0.000000e+00 2.803509e-04 0
#3: 1990   1     1   2 10601 1.909379e-04 0 0.000000e+00 9.546897e-05 0
#4: 1990   1     1   3 10413 2.915778e-04 0 9.719261e-05 0.000000e+00 0
#5: 1990   1     1   4 10220 9.902804e-05 0 0.000000e+00 1.980561e-04 0
#6: 1990   1     1   5 10095 0.000000e+00 0 0.000000e+00 0.000000e+00 0
#6 7 8 9 10 11 12           13 14           15           16
#1: 4.546571e-04 0 0 0  0  0  0 9.093141e-05  0 0.0003637256 0.0093659353
#2: 0.000000e+00 0 0 0  0  0  0 9.345029e-05  0 0.0000000000 0.0007476023
#3: 9.546897e-05 0 0 0  0  0  0 0.000000e+00  0 0.0000000000 0.0002864069
#4: 0.000000e+00 0 0 0  0  0  0 9.719261e-05  0 0.0000000000 0.0002915778
#5: 9.902804e-05 0 0 0  0  0  0 9.902804e-05  0 0.0000000000 0.0003961122
#6: 0.000000e+00 0 0 0  0  0  0 2.005085e-04  0 0.0000000000 0.0002005085
#total
#1: 0.0356451130
#2: 0.0026166080
#3: 0.0006682828
#4: 0.0007775408
#5: 0.0008912524
#6: 0.0004010170


#Groups used in the article:
# 1. Amenable to medical service: 1+2+3+4+6
# 2. Diabetes: 5
# 3. IHD:7
# 4. HIV:8
# 5. Lung cancer: 10
# 6. Cirrhosis: 11
# 7. Homicide: 12
# 8. Road traffic: 13 
# 9. Suicide: 9
# 10. All other causes: 14+15+16

# TR: I'd change this code chunk if there's time.

#group accordingly
Counts <- Data_Counts[,1:4,with=F]
Counts$g1 <- rowSums(Data_Counts[, c(6,7,8,9,11),with=F])
Counts$g2 <- Data_Counts[,10,with=F]
Counts$g3 <- Data_Counts[,12,with=F]
Counts$g4 <- Data_Counts[,13,with=F]
Counts$g5 <- Data_Counts[,15,with=F]
Counts$g6 <- Data_Counts[,16,with=F]
Counts$g7 <- Data_Counts[,17,with=F]
Counts$g8 <- Data_Counts[,18,with=F]
Counts$g9 <- Data_Counts[,14,with=F]
Counts$g10 <- rowSums(Data_Counts[,19:21,with=F])
Counts$g11 <- rowSums(Counts[,5:14,with=F])
Counts$Pop <- Data_Counts$Pop
# sum(colSums(Counts)[5:15])/2
Rates <- Data_rates[,c(1:4,22),with=F]
gdata::keep(Counts,Rates, sure = T)

Counts <- data.table(Counts)
Rates <- data.table(Rates)

# Cause-specific smoothing -----------------------------------------------

#First smooth with Camarda's method. We smooth cause-specific deaths and then constrain to the unsmoothed rates

# TR: this is the same, but with no smoothing over calendar time
# if this is to be preserved, it should move over to Functions.R.
# Also, in the final code cleaning, I'd rename stuff for consistency
# and clarity.
sm.mat.2   <- function(DX, EX){
	ages  		<- 0:109
	years 		<- 1990:2015
	W     		<- EX
	W[W > 0] 	<- 1
	W[DX < 1 & W == 1] 	<- .3
	#W[DX == 0] 	<- 0
	mxs         <- W * 0
	for (i in 1:ncol(mxs)){
		fiti   	<- Mort1Dsmooth(
				x = ages, 
				y = DX[,i],
				offset = log(EX[,i]), 
				W = W[,i],
				control=list(MAX.IT=300))
		
		mxsi 	<- exp(fit$logmortality)
		mxs[,i] <- mxsi
	}
	
	mxs <- melt(mxs, varnames=c("age","year"), value.name = "mxs")
	mxs
}
sm.chunk.2 <- function(.SD,i){
	DX      <- acast(.SD, age~year, value.var = colnames(Counts)[i], fill = 0)
	EX      <- acast(.SD, age~year, value.var = "Pop", fill = 0)
	
	mxs     <- sm.mat.2(DX,EX)
	
	.SD$mxs <- mxs$mxs
	
	# still need to normalize exposure?
	.SD
}

# consitency of population estimates and deaths
# just in very high ages
Counts[Pop < g11]$g1  <- Counts[Pop < g11]$g2 <- Counts[Pop < g11]$g3 <- Counts[Pop < g11]$g4 <- Counts[Pop < g11]$g5 <- Counts[Pop < g11]$g6 <- Counts[Pop < g11]$g7 <- Counts[Pop < g11]$g8 <- Counts[Pop < g11]$g9 <- Counts[Pop < g11]$g10 <- 0 
Counts[Pop < g11]$g11 <- Counts[Pop < g11]$Pop
Rates[total > 1]$total <- 1

source("R/Functions.R")

causes   	<- 5:15
Counts2  	<- data.table(cbind(Counts[,1:4,with=F],g1=Counts$g11,Counts$Pop))

sm.rates 	<- data.table(as.matrix(Counts)[,1:4])

# TR: sm.rates2 is now deprecated. The difference from sm.rates is that
# all cause mortality is also smoothed. We go ahead and calculate this,
# but do not use it. We prefer to preserve period shocks in all-cause mortality.
sm.rates2 	<- data.table(as.matrix(Counts2)[,1:4])
sm.rates2  	<- sm.rates2[with(sm.rates2,order(year,sex,state,age)),]
sm.rates  	<- sm.rates[with(sm.rates,order(year,sex,state,age)),]

# TR: 29-3-2017 make new copy to put in the 1-d smoothed rates. 
# Smooth over age, but not over period.
sm.rates.age.only <- sm.rates

# s-d smooth of cause-specific rates
for (i in causes){
	Mxs      <- Counts[,sm.chunk(.SD,i),by=list(state,sex)]
	Mxs      <- Mxs[with(Mxs,order(year,sex,state,age)),]
	sm.rates[,paste0("g",i-4)] <- Mxs$mxs
	print(i)
	# cbind(sm.rates,i= Mxs$mxs)
}

# TR: 29-3-2017
# and for period-only smoothing:
for (i in causes){
	Mxs      <- Counts[,sm.chunk.2(.SD,i),by=list(state,sex)]
	Mxs      <- Mxs[with(Mxs,order(year,sex,state,age)),]
	sm.rates.age.only[,paste0("g",i-4)] <- Mxs$mxs
	print(i)
	# cbind(sm.rates,i= Mxs$mxs)
}

# 2-d smooth of all-cause mortality. (deprecated)
Mxs2      	<- sm.rates[,1:4,with=F]
Mxs2$mxs 	<- sm.rates$g11
Mxs2 		<- Mxs2[with(Mxs2,order(year,sex,state,age)),]
sm.rates2 	<- sm.rates2[with(sm.rates2,order(year,sex,state,age)),]
sm.rates2[,"Tot"] <- Mxs2$mxs

# -------------------------------------
#Order all datasets
sm.rates 				<- sm.rates[with(sm.rates,order(year,sex,state,age)),]
sm.rates.age.only  	    <- sm.rates.age.only [with(sm.rates.age.only ,order(year,sex,state,age)),]
Rates 					<- Rates[with(Rates,order(year,sex,state,age)),]

# -------------------------------------
# constrain 2d rates to raw all-cause raw
sm.r    		<- sm.rates[,5:14,with=F]
sm.tot  		<- rowSums(sm.r)
sm.prop 		<- sm.r/sm.tot
sm.prop2 		<- sm.prop*Rates$total
smooth.rates 	<- cbind(Rates[,1:4,with=F],mx=rowSums(sm.prop2),sm.prop2)
smooth.rates 	<- as.data.table(smooth.rates)

# -------------------------------------
# constrain 1d rates to raw all-cause raw
sm.r    		<- sm.rates.age.only[,5:14,with=F]
sm.tot  		<- rowSums(sm.r)
sm.prop 		<- sm.r/sm.tot
sm.prop2 		<- sm.prop*Rates$total
smooth.rates.age.only 	<- cbind(Rates[,1:4,with=F],mx=rowSums(sm.prop2),sm.prop2)
smooth.rates.age.only 	<- as.data.table(smooth.rates)

# (deprecated)
#now constrain to smoothed ones
sm.prop3 		<- sm.prop*sm.rates2$Tot
smooth.rates2 	<- cbind(Rates[,1:4,with=F],mx=rowSums(sm.prop3),sm.prop3)
smooth.rates2   <- as.data.table(smooth.rates2)

# without constrain in smoothing by cause
sm.rates$mx <- sm.tot
sm.rates <- sm.rates[,-15, with=F]
setcolorder(sm.rates,c("year","sex","state","age","mx","g1","g2","g3","g4","g5","g6","g7","g8","g9","g10"))
head(sm.rates)

# This file contains 2d smooth rates constrained to the raw all-cause rates
save(smooth.rates,file = "Data/smoothed rates_ConstraintOrig.RData")

# This file contains 2d smooth rates constrained to the 2d smoothed all-cause rates
save(smooth.rates2,file = "Data/smoothed rates_ConstraintSmooth.RData")

# This file contains 2d smooth rates NOT constraint to the original ones
save(sm.rates,file = "Data/smoothed rates_No Constraint.RData")

# This file contains 2d smooth of all-cause mortality only
save(sm.rates2,file = "Data/Total_Smooth.RData")

# This file contains 1d smooth rates constrained to raw all-cause rates
save(smooth.rates.age.only,file = "Data/smoothed rates_ConstraintOrig.age.only.RData")

# end