#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
#setwd("C:/Apps/projects/Retail")
setwd("Z:/project/Retail")

#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)
library(corrplot)
library(RColorBrewer)

source("./code/tools.R")

#--------------------------------------------------------
# Paths
#-------------------------------------------------------- 
gas_path = "./data/est_gas.csv"
dis_path = "./data/est_dis.csv"

#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
gas.d <- read.csv(gas_path, header=TRUE, na.strings="NA")
dis.d <- read.csv(dis_path, header=TRUE, na.strings="NA")

# correct var type
gas.d$Observation.Date <- as.Date(gas.d$Observation.Date, "%m/%d/%Y")
dis.d$Observation.Date <- as.Date(dis.d$Observation.Date, "%m/%d/%Y")

# order col names alphabatically
gas.d <- gas.d[,order(colnames(gas.d))]
dis.d <- dis.d[,order(colnames(dis.d))]

gas.d <- gas.d %>% select(Observation.Date, everything())
dis.d <- dis.d %>% select(Observation.Date, everything())


# head(gas.d %>% select(contains("1"), contains("2")))
# head(dis.d %>% select(contains("1"), contains("2")))
# Addison.Armstrong.1 Carl.Larry.1 Phil.Flynn.1 Tom.Pawlicki.1 Tom.Pawlicki.2


# combine same analyst estimation
gas1 <- combine_analyst(gas.d, "1")
gas <- combine_analyst(gas1, "2")
dis1 <- combine_analyst(dis.d, "1")
dis <- combine_analyst(dis1, "2")


#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 
# summary stats
dim(gas)
summary(gas)
dim(dis)
summary(dis)

png(paste0("Actual.png"), width=650, height=650)
par(mfrow=c(2,1))
hist(gas$Actual, nclass=100, main="Gas", xlab="Actual")
hist(dis$Actual, nclass=100, main="Dis", xlab="Actual")
dev.off()


# missing values
# gas.miss.count <- sort( sapply(gas, function(x) sum(is.na(x))) ) # count 
# gas.miss.prop <- sort( sapply(gas, function(x) sum(is.na(x))/nrow(gas)) )  # proportion
# dis.miss.count <- sort( sapply(dis, function(x) sum(is.na(x))) )  # count
# dis.miss.prop <- sort( sapply(dis, function(x) sum(is.na(x))/nrow(dis)) )  # proportion
gas.miss.prop <- sapply(gas, function(x) sum(is.na(x))/nrow(gas)) # proportion
dis.miss.prop <- sapply(dis, function(x) sum(is.na(x))/nrow(dis)) # proportion

# png("gas_miss_prop.png", width=650, height=650)
# hist(gas.miss.prop[-c(1,2)], nclass=30, main="Histogram of missing proportion (Gas)", xlab="Missing Proportion")
# dev.off()
# png("dis_miss_prop.png", width=650, height=650)
# hist(dis.miss.prop[-c(1,2)], nclass=30, main="Histogram of missing proportion (Dis)", xlab="Missing Proportion")
# dev.off()



## 1. use summary stats of all analyst by row
########################
## gas
########################
# all
cal_corr(gas, "gas_corr_all") 

# actual >0 <0
gas.all.gt0 <- gas %>% filter(Actual > 0)
gas.all.lt0 <- gas %>% filter(Actual < 0)
cal_corr(gas.all.gt0, "gas_corr_all_act_gt0") 
cal_corr(gas.all.lt0, "gas_corr_all_act_lt0") 

# analyst miss prop less than cut
miss.cut <- 0.5 #0.1, 0.3, 0.5, 0.7, 0.9
gas.lt.miss.cut <- gas[, gas.miss.prop<miss.cut]
cal_corr(gas.lt.miss.cut, "gas_corr_miss_lt90pct") 

# select analyst by corr cut
A <- gas[,-1]
#A <- gas.lt.miss.cut[,-1] 
M <- cor(A, use="pairwise.complete.obs")
corr.cut <- 0.5  # 0.4 0.45 0.5 0.55
sel <- M[1,]>corr.cut
sel[is.na(sel)] <- FALSE
A <- A[,sel]
A <- cbind(1, A)
cal_corr(A, "gas_corr_corrgt055") 
#cal_corr(A, "gas_corr_misslt05_corrgt05") 

A.gt0 <- A %>% filter(Actual > 0)
cal_corr(A.gt0, "gas_corr_corrgt055_act_gt0") 
A.lt0 <- A %>% filter(Actual < 0)
cal_corr(A.lt0, "gas_corr_corrgt055_act_lt0") 



## analyst miss prop less than miss.cut and corr gt miss.cut
# select analyst by miss cut
miss.cut <- 0.5 #0.1, 0.3, 0.5, 0.7, 0.9
gas.lt.miss.cut <- gas[, gas.miss.prop<miss.cut]

# select analyst by corr cut
A <- gas.lt.miss.cut[,-1]
M <- cor(A, use="pairwise.complete.obs")
corr.cut <- 0.5  # 0.4 0.45 0.5 0.55
sel <- M[1,]>corr.cut
sel[is.na(sel)] <- FALSE
A <- A[,sel]
A <- cbind(1, A)
cal_corr(A, "gas_corr_misslt05_corrgt05") 

B <- A %>% filter(Actual > 0)
cal_corr(B, "gas_corr_misslt05_corrgt05_actgt0") 
C <- A %>% filter(Actual < 0)
cal_corr(C, "gas_corr_misslt05_corrgt05_actlt0") 



########################
## dis
########################
cal_corr(dis, "dis_corr_all") 

# actual >0 <0
dis.all.gt0 <- dis %>% filter(Actual > 0)
dis.all.lt0 <- dis %>% filter(Actual < 0)
cal_corr(dis.all.gt0, "dis_corr_all_act_gt0") 
cal_corr(dis.all.lt0, "dis_corr_all_act_lt0") 

# analyst miss prop less than cut
miss.cut <- 0.5  #0.1, 0.3, 0.5, 0.7, 0.9
dis.lt.miss.cut <- dis[, dis.miss.prop<miss.cut]
cal_corr(dis.lt.miss.cut, "dis_corr_miss_lt90pct") 

# select analyst by corr cut
A <- dis[,-1]
A <- dis.lt.miss.cut[,-1] 
M <- cor(A, use="pairwise.complete.obs")
corr.cut <- 0.5  # 0.4 0.45 0.5 
sel <- M[1,]>corr.cut
sel[is.na(sel)] <- FALSE
A <- A[,sel]
A <- cbind(1, A)
cal_corr(A, "dis_corr_corrgt05") 
cal_corr(A, "dis_corr_misslt05_corrgt05") 


A.gt0 <- A %>% filter(Actual > 0)
cal_corr(A.gt0, "dis_corr_corrgt05_act_gt0") 
A.lt0 <- A %>% filter(Actual < 0)
cal_corr(A.lt0, "dis_corr_corrgt05_act_lt0") 



cal_corr(gas, "gas_corr_each_act") 


## analyst miss prop less than miss.cut and corr gt miss.cut
# select analyst by miss cut
miss.cut <- 0.5 #0.1, 0.3, 0.5, 0.7, 0.9
dis.lt.miss.cut <- dis[, dis.miss.prop<miss.cut]

# select analyst by corr cut
A <- dis.lt.miss.cut[,-1]
M <- cor(A, use="pairwise.complete.obs")
corr.cut <- 0.5  # 0.4 0.45 0.5 0.55
sel <- M[1,]>corr.cut
sel[is.na(sel)] <- FALSE
A <- A[,sel]
A <- cbind(1, A)
cal_corr(A, "dis_corr_misslt05_corrgt05") 

B <- A %>% filter(Actual > 0)
cal_corr(B, "dis_corr_misslt05_corrgt05_actgt0") 
C <- A %>% filter(Actual < 0)
cal_corr(C, "dis_corr_misslt05_corrgt05_actlt0") 



## each analyst corr
out <- gas[,-1]
M <- cor(out, use="pairwise.complete.obs")
sort(abs(M[1,]))

out <- dis[,-1]
M <- cor(out, use="pairwise.complete.obs")
sort(abs(M[1,]))


#--------------------------------------------------------
# Modelling
#-------------------------------------------------------- 