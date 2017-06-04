#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:\\project\\Retail")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory

source("./code/tools.R")

# load the required packages and try to install them if they are not available
reqPackages <- c("tidyverse", "corrplot", "RColorBrewer", "lubridate", "plotly","caret", 
                 "gbm","randomForest", "doParallel", "ModelMetrics", "mosaic") # mlr has conflict with caret
load_libs(reqPackages)



#--------------------------------------------------------
# Load data
#-------------------------------------------------------- 
padd1a <- read.csv("./data/padd1a.csv", as.is = T)
padd1b <- read.csv("./data/padd1b.csv", as.is = T)
padd1c <- read.csv("./data/padd1c.csv", as.is = T)

padd2 <- read.csv("./data/padd2.csv", as.is = T)
padd3 <- read.csv("./data/padd3.csv", as.is = T)
padd4 <- read.csv("./data/padd4.csv", as.is = T)
padd5 <- read.csv("./data/padd5.csv", as.is = T)


#--------------------------------------------------------
# EDA
#-------------------------------------------------------- 
# select analyst by corr cut
A <- padd3
A <- A %>% filter(year(lastwkd)>2015)
A <- A[,-c(1,2)]
M <- cor(A, use="pairwise.complete.obs")

name <- paste0("padd5","_16-17") 
#name <- paste0("padd3") 

png(paste0(name,"_circle.png"), width=650, height=650)
corrplot(M, method="circle", type="upper", col=brewer.pal(n=11, name="RdYlBu"))
dev.off()

png(paste0(name,"_num.png"), width=650, height=650)
corrplot(M, method="number", type="upper", col=brewer.pal(n=8, name="RdYlBu"))
dev.off()



