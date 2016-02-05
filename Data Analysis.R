# O's Data Analysis

setwd("C:/Users/nmvenuti/Desktop/Os Data")

#Import dataset

pFX<-data.frame(read.csv("C:/Users/nmvenuti/Desktop/Os Data/Data/Pitchfx.csv"))

head(pFX)


#Get strike zone
strike_zone<-pFX[pFX$event_type=='called_strike',c('plate_x','plate_z')]


summary(strike_zone)
# plate_x            plate_z     
# Min.   :-2.56100   Min.   :0.078  
# 1st Qu.:-0.55500   1st Qu.:1.966  
# Median :-0.06600   Median :2.335  
# Mean   :-0.07045   Mean   :2.369  
# 3rd Qu.: 0.41200   3rd Qu.:2.750  
# Max.   : 2.17000   Max.   :6.481 

#Appears strike zone is -2<=x<=2, 0<=z<=4
#Break up sections as follows

#First define variables

pFX$zone<-NA

#Add in Balls

pFX$zone[-2>pFX$plate_x]<-'BL'
pFX$zone[pFX$plate_x>2]<-'BR'
pFX$zone[pFX$plate_z<0]<-'BB'
pFX$zone[pFX$plate_z>4]<-'BU'


#Add in strikes

#Upper strike zone
xub<- -0.67
xlb<- -2
zub<- 4
zlb<- 2.67
tag<- 'SUL'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag

xub<- 0.67
xlb<- -0.67
zub<- 4
zlb<- 2.67
tag<- 'SUC'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag

xub<- 2
xlb<- 0.67
zub<- 4
zlb<- 2.67
tag<- 'SUR'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag

#Middle strike zone
xub<- -0.67
xlb<- -2
zub<- 2.67
zlb<- 1.33
tag<- 'SML'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag

xub<- 0.67
xlb<- -0.67
zub<- 2.67
zlb<- 1.33
tag<- 'SMC'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag

xub<- 2
xlb<- 0.67
zub<- 2.67
zlb<- 1.33
tag<- 'SMR'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag


#Bottom strike zone
xub<- -0.67
xlb<- -2
zub<- 1.33
zlb<- 0
tag<- 'SBL'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag

xub<- 0.67
xlb<- -0.67
zub<- 1.33
zlb<- 0
tag<- 'SMC'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag

xub<- 2
xlb<- 0.67
zub<- 1.33
zlb<- 0
tag<- 'SBR'

pFX$zone[(xlb<=pFX$plate_x) & (pFX$plate_x<=xub) & (zlb<=pFX$plate_z)&(pFX$plate_z<=zub)]<-tag


#Geting pitcher freqs
library(dplyr)
library(plyr)
library(reshape2)

pFX[c("pitcher_id","event_type")]



count_df <- data.frame(count(pFX,vars=c("pitcher_id","event_type")))
colnames(count_df)<-c('pitcher_id','event_type','count')

count_df_wide<-dcast(count_df, pitcher_id~event_type)