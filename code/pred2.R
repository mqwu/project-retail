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
                 "gbm","randomForest", "doParallel", "ModelMetrics") # mlr has conflict with caret
load_libs(reqPackages)


#--------------------------------------------------------
# Load data
#-------------------------------------------------------- 
d <- readRDS("./data/all.rds")


#--------------------------------------------------------
# Define Var groups
#-------------------------------------------------------- 
# Define Var groups
season <- c("month", "wkofyr", "wkofmo")


# w/ diff
x1 <- c("retail_gas", "retail_gas_dif",  "weekly_diesel", "weekly_diesel_dif")
x2 <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel",
        "weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif")
x3 <- c("retail_gas_dif", "weekly_diesel_dif")
x4 <- c("weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif")
# no diff
x5 <- c("retail_gas", "weekly_diesel") 
x6 <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel")

x7 <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel",
        "weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "weekly_diesel_dif",
        "Gas_Oil")

#--------------------------------------------------------
# Prediction
#-------------------------------------------------------- 
#########################################################
#target <- "Gas"
target <- "Diesel"
x <- "x2"
cov <- x2
method <- "rf"  # gbm
initialWin <- 52
tuneLength <- 5
fixWin <- FALSE
#########################################################

# creating sampling seeds 
set.seed(123) # seed1
#set.seed(777) # seed2
seeds <- vector(mode = "list", length = 217) # 52wk
for(i in 1:216) seeds[[i]] <- sample.int(1000, 100) 
seeds[[217]] <- sample.int(1000, 1)


## data for modelling
# Vars used in the model
vars <- c(cov, season, target)
# data for modelling
dat <- d %>% dplyr::select(match(vars, names(.)))

#dat <- dat[131:268,]
#dat <- dat %>% mutate (chg.retail.dis.diff=weekly_diesel_dif-lag(weekly_diesel_dif))

# remove first records if use diff var
if(length(grep("*dif", vars))>0) { dat <- dat[-1,] }
#if(length(grep("*dif", vars))>0) { dat <- dat[-c(1,2),] }

timeSlices <- createTimeSlices(1:nrow(dat), initialWindow = initialWin, horizon = 1, fixedWindow = fixWin)
trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]

# registerDoParallel(cores=16)
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = initialWin,
#                               horizon = 1,
#                               fixedWindow = fixWin,
#                               allowParallel = TRUE,
#                               seeds = seeds)

# mod <- train( #Gas ~ .,
#               Diesel ~ .,
#               data = dat,
#               method = method,
#               trControl = myTimeControl,
#               tuneLength=tuneLength,
#               verbose=FALSE)
# 
# mthd = paste0(target,"_", method, "_",x,"_", initialWin,"wk_", "fixWin_", fixWin)
# sol=data.frame(method=mthd, rmse=min(mod$results$RMSE))
# write.table(sol,"./code/res2/perf.csv", row.names=F, append=T, quote=F, sep=",", col.names=F)
# 
# modname=paste0("./code/res2/", mthd, ".rds")
# saveRDS(mod, modname)



## For prediction
# myTimeControl <- trainControl(allowParallel = TRUE, seeds = seeds)
# pred <- NULL
# true <- NULL
# for(i in 1:length(trainSlices)){
#   mod <- train(Gas ~ .,
#               data = dat[trainSlices[[i]],],
#               method = method,
#               trControl = myTimeControl,
#               tuneLength=tuneLength,
#               verbose=FALSE)
# 
#   pred <- c(pred, predict(mod, dat[testSlices[[i]],]))
#   true <- c(true, dat$Gas[testSlices[[i]]])
# }
# rmse(pred, true)

#cut <- 2000000
pred <- NULL; true <- NULL;
imp1 <- NULL; imp2 <- NULL;
for(i in 1:length(trainSlices)){
  set.seed(777)
  # dd <- dat[trainSlices[[i]],] %>% filter(abs(chg.retail.dis.diff)>cut) 
  # dd <- dd[,-13]
  mod <- randomForest(Diesel ~ ., 
                      data=dat[trainSlices[[i]],], 
                      #data=dd,
                      mtry=3,
                      importance=TRUE, na.action=na.omit
                      )
  
  imp1 <- cbind(imp1, mod$importance[,1])   #mse 
  imp2 <- cbind(imp2, mod$importance[,2])   #nodepurity    

  pred <- c(pred, predict(mod, dat[testSlices[[i]],]))
  true <- c(true, dat$Diesel[testSlices[[i]]])
}

imp1avg <- sort(apply(imp1, 1, mean))
imp2avg <- sort(apply(imp2, 1, mean))

#rf
rmse(pred[1:215], true[1:215])  # 2322  2335.16 with out Gas_oil (slight improve)
#bb
rmse(d$dis_mean[54:268], true[1:215])  #2595

# out <- d
# out$pred <- NA
# 
# if(length(grep("*dif", vars))>0) {
#   #out[(initialWin+3):nrow(d), 'pred'] <- pred
#   out[(initialWin+2):nrow(d), 'pred'] <- pred
# } else{
#   out[(initialWin+2):nrow(d), 'pred'] <- pred
# }
# 
#mthd = paste0(target,"_", method, "_",x,"_", initialWin,"wk_", "fixWin_", fixWin)
# #mthd = paste0(target,"_", method, "_",x,"_", initialWin,"wk_", "fixWin_", fixWin, "_cut2m")
#file = paste0("./code/res2/", mthd, ".csv")
#write.table(out, file, row.names=F, quote=F, sep=",", col.names=T)
# file = paste0("./code/res2/","imp1_", mthd, ".csv")
# write.table(imp1avg, file, row.names=T, quote=F, sep=",", col.names=F)
# file = paste0("./code/res2/","imp2_", mthd, ".csv")
# write.table(imp2avg, file, row.names=T, quote=F, sep=",", col.names=F)




# dd <- data.frame(time=1:length(pred),pred=pred, true=true)
# 
# p  <- plot_ly(dd) %>%
#   add_trace(x=~time, y=~pred, name = 'pred' , type='scatter', mode = 'lines') %>%
#   add_trace(x=~time, y=~true, name = 'Gas' , type='scatter', mode = 'lines')
# p

