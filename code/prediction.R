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
                 "gbm","randomForest", "party", "Cubist", "bst", "earth", "mboost", "doParallel", "pls") # mlr has conflict with caret
load_libs(reqPackages)


#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
allin1 <- readRDS("./data/allin1_retail.rds")

d <- allin1 %>% 
      mutate(retail_gas=weekly_regular + weekly_mid_grade + weekly_svpn) %>%  
      mutate(retail_gas_dif=retail_gas-lag(retail_gas)) %>% 
      rename(gas_mean=gas_Mean, dis_mean=diesel_Mean)
#saveRDS(d, "./data/all.rds")

# Define Var groups
x1 <- c("retail_gas", "retail_gas_dif",  "weekly_diesel", "weekly_diesel_dif")
x2 <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel",
        "weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif")
x3 <- c("retail_gas", "weekly_diesel") # no retail diff
x4 <- c("retail_gas_dif", "weekly_diesel_dif")
x5 <- c("weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif")
x6 <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel")

season <- c("month", "wkofyr", "wkofmo")
target <- "Gas"
#target <- "Diesel"

vars <- c(x1, season, target)

dat <- d %>% dplyr::select(match(vars, names(.)))
dat <- dat[-1,] # if use diff

#--------------------------------------------------------
# Prediction
#-------------------------------------------------------- 
method <- "rf"
#method <- "gbm"
#method <- "pls"
totn <- nrow(dat)
initialWin <- 52
#initialWin <- 12

### creating sampling seeds ###
set.seed(123) #seed 1
#set.seed(456) #seed 2
#set.seed(777) #seed 3
#set.seed(888) #seed 4
#set.seed(369) #seed 5

seeds <- vector(mode = "list", length = 217)
#seeds <- vector(mode = "list", length = 256)
for(i in 1:216) seeds[[i]] <- sample.int(1000, 100) #52wk
#for(i in 1:241) seeds[[i]] <- sample.int(1000, 5)   #26wk
#for(i in 1:255) seeds[[i]] <- sample.int(1000, 5)    #12wk

### For the last model:
seeds[[217]] <- sample.int(1000, 1) #52wk
#seeds[[242]] <- sample.int(1000, 1)  #26wk
#seeds[[256]] <- sample.int(1000, 1)  #12wk

# http://stackoverflow.com/questions/24758218/time-series-data-spliting-and-model-evaluation
registerDoParallel(cores=8)
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = initialWin,
                              horizon = 1,
                              #fixedWindow = FALSE,
                              fixedWindow = TRUE,
                              allowParallel = TRUE,
                              seeds = seeds)

tuneLength <- 5

mod <- train( Gas ~ .,
              #Diesel ~ .,
              data = dat,
              method = method,
              trControl = myTimeControl,
              tuneLength=tuneLength,
              verbose=FALSE)

a=cbind(d[-1,], pred=predict(mod, dat)) # if use diff
#a=cbind(d, pred=predict(mod, dat))

#write.table(a, "./code/res/dis_rf_x2_52w_fix.csv", row.name=F, quote=F, sep=",", col.names=T)
#sol=data.frame(method="dis_rf_x2_52w_nofix", rmse=postResample(pred = a$pred[(initialWin+1):nrow(a)], obs = a$Diesel[(initialWin+1):nrow(a)])[1])
sol=data.frame(method="dis_gbm_x6_52w_fix", rmse=postResample(pred = a$pred[(initialWin+1):nrow(a)], obs = a$Diesel[(initialWin+1):nrow(a)])[1])
#write.table(sol,"./code/res/perf.csv", row.names=F, append=T, quote=F, sep=",", col.names=F)






