#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("Z:\\project\\Retail")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory

list.of.packages <- c("dplyr",
                      "plotly",
                      "lubridate",
                      "corrplot",
                      "caret",
                      "gbm",
                      "reshape2",
                      "zoo",
                      "randomForest",
                      "doParallel",
                      "ModelMetrics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=TRUE)
rm(new.packages, list.of.packages)



#--------------------------------------------------------
# Load data
#-------------------------------------------------------- 
d <- readRDS("./data/all.rds")


#--------------------------------------------------------
# Define Var groups
#-------------------------------------------------------- 
# Define Var groups
season <- c("month", "wkofyr", "wkofmo")

x <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel",
        "weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif")


#--------------------------------------------------------
# Prediction
#-------------------------------------------------------- 
#########################################################
target <- "Gas"
#target <- "Diesel"
method <- "rf"  # gbm
initialWin <- 52
tuneLength <- 5
fixWin <- TRUE
#########################################################

# creating sampling seeds 
set.seed(123)
seeds <- vector(mode = "list", length = 217) # 52wk
for(i in 1:216) seeds[[i]] <- sample.int(1000, 100) 
seeds[[217]] <- sample.int(1000, 1)

## data for modelling
# Vars used in the model
vars <- c(x, season, target)
# data for modelling
dat <- d %>% dplyr::select(match(vars, names(.)))
# remove first records if use diff var
if(length(grep("*dif", vars))>0) { dat <- dat[-1,] }

registerDoParallel(cores=16)
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = initialWin,
                              horizon = 1,
                              fixedWindow = fixWin,
                              allowParallel = TRUE,
                              seeds = seeds)
mod <- train( Gas ~ .,
              #Diesel ~ .,
              data = dat,
              method = method,
              trControl = myTimeControl,
              tuneLength=tuneLength,
              verbose=FALSE)


###############################################################
# the way we calculate predictive value may be wrong
rmse(predict(mod, dat), dat$Gas) # that is what being used rmse=989

# if you extract RMSE from mod
min(mod$results$RMSE)  # rmse=1850 similar to gbm results

# do not know how predict function really works in caret package





