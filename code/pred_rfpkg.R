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
                 "gbm","randomForest", "party", "Cubist", "bst", "earth", "mboost", "doParallel") # mlr has conflict with caret
load_libs(reqPackages)


#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
allin1 <- readRDS("./data/allin1_retail.rds")

d <- allin1 %>% 
  mutate(retail_gas=weekly_regular + weekly_mid_grade + weekly_svpn) %>%  
  mutate(retail_gas_dif=retail_gas-lag(retail_gas)) %>% 
  rename(gas_mean=gas_Mean, dis_mean=diesel_Mean)


# Define Var groups
#x1 <- c("retail_gas", "retail_gas_dif",  "weekly_diesel", "weekly_diesel_dif")
x2 <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel",
        "weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif")
#x3 <- c("retail_gas", "weekly_diesel") # no retail diff
#x4 <- c("retail_gas_dif", "weekly_diesel_dif")
#x5 <- c("weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif")
x6 <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel")

season <- c("month", "wkofyr", "wkofmo")
target <- "Gas"

vars <- c(x2, season, target)

dat <- d %>% dplyr::select(match(vars, names(.)))
dat <- dat[-1,] # if use diff

#--------------------------------------------------------
# Prediction
#-------------------------------------------------------- 
### creating sampling seeds ###
set.seed(123)
seeds <- vector(mode = "list", length = 217)
for(i in 1:217) seeds[[i]] <- sample.int(1000, 1)
#for(i in 1:216) seeds[[i]] <- sample.int(1000, 100)

### For the last model:
#seeds[[217]] <- sample.int(1000, 1)



n <- nrow(dat)  # num of data  
step <- 1       # moving step size

prediction <- d
for(w in seq(52, 52, by=4) ){
  
  win <- w       # window size in weeks
  win.start <- seq(1, n-win, by=step)  # windows start index
  pred <- data.frame(week=(win.start[1]+win):n, NA)
  names(pred)[2] <- paste0("pred_w",win)
  
  for(i in 1:length(win.start)){
    set.seed(seeds[[i]])
    rf <- randomForest(Gas ~ ., data=dat[win.start[i]:(win.start[i]+win-1), ], na.action=na.omit, ntree=500, importance=T)
    pred[i,2] <- predict(rf, dat[win.start[i]+win,])    
  }
  prediction <- left_join(prediction, pred, by="week")
}
postResample(pred = prediction$pred_w52[54:nrow(prediction)], obs = prediction$Gas[54:nrow(prediction)])[1]


