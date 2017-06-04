#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
#setwd("C:/Apps/projects/Retail")
setwd("Z:\\project\\Retail")



#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory

source("./code/tools.R")

# load the required packages and try to install them if they are not available
reqPackages <- c("tidyverse", "mlr", "corrplot", "RColorBrewer", "lubridate", "plotly","caret", 
                 "gbm", "randomForest", "party", "Cubist", "bst", "earth", "mboost", "doParallel")
load_libs(reqPackages)


#--------------------------------------------------------
# Paths
#-------------------------------------------------------- 
#d_path = "C:/Apps/projects/Retail/data/dat4pred.rds"


#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
#d <- readRDS(d_path)

## data with raw var and retail weekly change
# d2 <- d %>% 
#         mutate(year=year(lastwkd), month=month(lastwkd, label=T), 
#                wkofyr=as.factor(week(lastwkd)), wkofmo=as.factor(ceiling(day(lastwkd)/7)),  
#                #wkofyr=as.ordered(week(lastwkd)), wkofmo=as.ordered(ceiling(day(lastwkd)/7)),  
#                retail_gas=weekly_regular + weekly_mid_grade + weekly_svpn) %>%                         # aggregate retail gas 
#         mutate(retail_gas_chg=retail_gas-lag(retail_gas),                                              # calculate retail gas/dis weekly change
#                retail_dis_chg=weekly_diesel-lag(weekly_diesel)) %>% 
#         dplyr::select(week, lastwkd, year, month, wkofyr, wkofmo,                                             # time
#                       Gas, Diesel,                                                                     # target
#                       retail_gas, retail_gas_chg, weekly_regular, weekly_mid_grade, weekly_svpn,       # retail gas
#                       weekly_diesel, retail_dis_chg, weekly_other,                                     # retail
#                       gas_Mean, diesel_Mean) %>%                                                       # BB estimate (only keep mean, may add min/max later)
#         dplyr::rename(retail_dis=weekly_diesel, gas_mean=gas_Mean, dis_mean=diesel_Mean)

#write.csv(d2, "./data/dat_raw.csv", row.names = F)  

#allin1$wkofmo <- as.factor(ceiling(day(allin1$lastwkd)/7))

## lag diff and sign
# d3 <- d2 %>% mutate(diff.Gas=Gas-lag(Gas), 
#                     diff.gas_mean=gas_mean-lag(gas_mean), 
#                     diff.retail_gas=retail_gas-lag(retail_gas),
#                     diff.Dis=Diesel-lag(Diesel), 
#                     diff.dis_mean=dis_mean-lag(dis_mean), 
#                     diff.retail_dis=retail_dis-lag(retail_dis)
#                     ) %>% 
#               mutate(iGasMv=as.factor(ifelse(diff.Gas>0, 1, -1)),
#                      iGasBBmeanMv=as.factor(ifelse(diff.gas_mean>0, 1, -1)),
#                      iGasRetailMv=as.factor(ifelse(diff.retail_gas>0, 1, -1)),
#                      iDisMv=as.factor(ifelse(diff.Dis>0, 1, -1)),
#                      iDisBBmeanMv=as.factor(ifelse(diff.dis_mean>0, 1, -1)),
#                      iDisRetailMv=as.factor(ifelse(diff.retail_dis>0, 1, -1))
#                      )
#write.csv(d3, "./data/dat_lag_diff_sign.csv", row.names = F)

# a1 <- d3 %>% mutate(iGaslag1=lag(iGas)) %>% mutate(chgSign=ifelse(iGas==iGaslag1, 0, 1))
# 
# b1 <- a1 %>% 
#       select(Gas, diff.Gas, iGas, iGaslag1, chgSign, retail_gas,diff.retail_gas) %>% 
#       mutate(diff.next.Gas=lead(diff.Gas), diff.next.retail_gas=lead(diff.retail_gas)) %>% 
#       mutate(Gas.mv=ifelse(diff.next.Gas>0, 1, -1), retail_gas.mv=ifelse(diff.next.retail_gas>0, 1, -1)) %>% 
#       filter(chgSign==1) %>% 
#       mutate(sum.mv=Gas.mv+retail_gas.mv)
# 
# View(b1)
# 
# b2 <- a1 %>% 
#   select(Gas, diff.Gas, iGas, iGaslag1, chgSign, retail_gas,diff.retail_gas) %>% 
#   mutate(diff.next.Gas=lead(diff.Gas), diff.next.retail_gas=lead(diff.retail_gas)) %>% 
#   mutate(Gas.mv=ifelse(diff.next.Gas>0, 1, -1), retail_gas.mv=ifelse(diff.next.retail_gas>0, 1, -1)) %>% 
#   mutate(sum.mv=Gas.mv+retail_gas.mv)
# 
# View(b2)
# 
# a1 <- d3[,6:ncol(d3)]
# a2 <- a1[,-c(3:7)]
# 
# cor(a2$Gas[210:268], a2$retail_gas[210:268], use="pairwise.complete.obs")
# cor(a2$Gas[210:268], a2$diff.retail_gas[210:268], use="pairwise.complete.obs")
# cor(a2$diff.Gas[210:268], a2$diff.retail_gas[210:268], use="pairwise.complete.obs")


#write.csv(d5, "./data/dat_lag_diff.csv", row.names = F)
# cor(d5$diff.Gas[200:268], d5$diff.gas_mean[200:268], use="pairwise.complete.obs")
# cor(d5$diff.Gas[228:268], d5$diff.gas_mean[228:268], use="pairwise.complete.obs")
# 
# lag1.gt7k <- d5 %>% filter(abs(diff.Gas)>7000)
# wk1 <- lag1.gt7k$week
# week1 <- c(wk1, wk1+1, wk1+2, wk1+3, wk1-1, wk1-2, wk1-3)
# week1 <- unique(sort(week1))
# # 
# d6 <- d5 %>% mutate(diff.lag2.Gas=Gas-lag(Gas,2), 
#                     diff.lag2.gas_mean=gas_Mean-lag(gas_Mean,2), 
#                     diff.lag2.retail_gas=retail_gas-lag(retail_gas,2))
# lag2.gt5k <- d6 %>% filter(abs(diff.lag2.Gas)>5000)
# wk2 <- lag2.gt5k$week
# 
# wk <- sort(unique(c(wk1, wk2)))
# ind <- which(d5$week %in% wk)
# sel.wk <- d5[ind,]
# 
# 
# week2 <- c(wk2, wk2+1, wk2+2, wk2+3, wk2-1, wk2-2, wk2-3)
# week2 <- unique(sort(week2))
# 
# weeks <- unique(sort(c(week1, week2)))
# 
# d8 <- d5 %>% filter(week %in% weeks)
# write.csv(d8, "./data/dat_lag1_diff_gt7k_lag2_diff_gt5k.csv", row.names = F)

# lag diff sign
# d6 <- d5 %>% mutate(diff.Gas.sign=ifelse(diff.Gas>0, 1, -1),
#                     diff.gas_mean.sign=ifelse(diff.gas_mean>0,1, -1),
#                     diff.retail_gas.sign=ifelse(diff.retail_gas>0,1, -1)
#                     )
# write.csv(d6, "./data/dat_lag_diff_sign.csv", row.names = F)
# cor(d6$diff.Gas.sign, d6$diff.gas_mean.sign, use="pairwise.complete.obs")
# cor(d6$diff.Gas.sign, d6$diff.retail_gas.sign, use="pairwise.complete.obs")


#--------------------------------------------------------
# Correlation
#-------------------------------------------------------- 

# data set for corr study
# dat <- d2
# 
# # set year range
# yrs <- c(2012:2017)
# # select data in year range
# d.yr <- dat %>% filter(year %in% yrs) 
# 
# # gas and dis matrix
# gas.m <- d.yr %>% select(Gas, retail_gas, retail_gas_chg, gas_mean, retail_dis, retail_dis_chg, dis_mean)
# dis.m <- d.yr %>% select(Diesel, retail_dis, retail_dis_chg, dis_mean, retail_gas, retail_gas_chg, gas_mean)
# 
# # corr matrix
# M <- cor(gas.m, use="pairwise.complete.obs") # gas / dis
# corrplot(M, method="number", type="upper") # col=brewer.pal(n=8, name="RdYlBu"))
# corrplot(M, method="circle", type="upper") #col=brewer.pal(n=11, name="RdYlBu"))




# #M <- cor(d.yr.m, use="pairwise.complete.obs") # all
# M <- cor(d.yr.m[,c(7,1,2,3,8,9,10)], use="pairwise.complete.obs") # gas
# # M <- cor(d.yr.m[,c(6,7,1,2,3,4)], use="pairwise.complete.obs") # gas
# #M <- cor(allin1[,c(8,9,36,37)],use="pairwise.complete.obs")
# 
# #M <- cor(d.yr[,c(6,4,11,12,13)], use="pairwise.complete.obs") # dis
# corrplot(M, method="number", type="upper")# col=brewer.pal(n=8, name="RdYlBu"))
# corrplot(M, method="circle", type="upper")#col=brewer.pal(n=11, name="RdYlBu"))
# 
# 
# ## plot
# ## std
# d.yr.z <- d.yr %>% mutate(weekly_regular_z=as.vector(scale(weekly_regular)),
#                           weekly_mid_grade_z=as.vector(scale(weekly_mid_grade)),
#                           weekly_svpn_z=as.vector(scale(weekly_svpn)),
#                           Gas_z = as.vector(scale(Gas))
#                           )
# d.yr.z <- as.data.frame(d.yr.z)
# p <- plot_ly(d.yr.z) %>% 
#      add_trace(x=~lastwkd, y=~weekly_regular_z, name = 'Regular', type='scatter', mode = 'lines') %>% 
#      add_trace(x=~lastwkd, y=~weekly_mid_grade_z, name = 'Mid Grade'  , type='scatter', mode = 'lines') %>% 
#      add_trace(x=~lastwkd, y=~weekly_svpn_z, name = 'SVPN'  , type='scatter', mode = 'lines') %>% 
#      add_trace(x=~lastwkd, y=~Gas_z, name = 'Gas'  , type='scatter', mode = 'lines') 
#   
# 
# # original
# p <- plot_ly(d.yr.z) %>% 
#   add_trace(x=~lastwkd, y=~weekly_regular_z, name = 'Regular', type='scatter', mode = 'lines') %>% 
#   add_trace(x=~lastwkd, y=~weekly_mid_grade_z, name = 'Mid Grade'  , type='scatter', mode = 'lines') %>% 
#   add_trace(x=~lastwkd, y=~weekly_svpn_z, name = 'SVPN'  , type='scatter', mode = 'lines') %>% 
#   add_trace(x=~lastwkd, y=~Gas_z, name = 'Gas'  , type='scatter', mode = 'lines') 
# 
# p


#--------------------------------------------------------
# Predictive Modelling
#--------------------------------------------------------
# d3 <- d3[-1,]
# # add vars for seasonality
# #d3$month  <- month(d3$lastwkd, label = T)
# #d3$wkofyr <- week(ymd(d3$lastwkd))
# 
# # var groups
# #retail <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_other")
# #retail <- c("diff.retail_gas")
# #retail <- c("retail_gas", "diff.retail_gas")
# #retail <- c("weekly_diesel")
# retail <- c("diff.retail_dis", "weekly_diesel")
# 
# #bb     <- c("gas_Mean", "diesel_Mean")
# bb     <- c("diesel_Mean")
# #bb     <- c("diff.gas_mean")
# #bb <- c("gas_Mean", "diff.gas_mean")
# 
# season <- c("month", "wkofyr")
#season <- c("month")
#season <- c("wkofyr")

#############
## Gas / Dis
#############
#target <- "iGasMv" 
#target <- "iGas"
# target <- "iDisMv"
# #Vars <- c(target, bb, season)
# Vars <- c(target, retail, bb, season)
# Vars <- c(target, bb, season)
# 
# #test data
# d17 <- d3 %>% filter(year %in% c(2017)) %>% select(match(Vars, names(.))) 
# 
# #trainning data
# d12to16 <- d3 %>% filter(year %in% c(2012, 2013, 2014, 2015, 2016)) %>% select(match(Vars, names(.))) 
# d13to16 <- d3 %>% filter(year %in% c(2013, 2014, 2015, 2016)) %>% select(match(Vars, names(.))) 
# d14to16 <- d3 %>% filter(year %in% c(2014, 2015, 2016)) %>% select(match(Vars, names(.)))  
# d15to16 <- d3 %>% filter(year %in% c(2015, 2016)) %>% select(match(Vars, names(.)))  
# d16 <- d3 %>% filter(year %in% c(2016)) %>% select(match(Vars, names(.)))  
# 
# ### classification models
# C12to16 <- makeClassifTask(data=d12to16, target = target, positive = "1")
# C13to16 <- makeClassifTask(data=d13to16, target = target, positive = "1")
# C14to16 <- makeClassifTask(data=d14to16, target = target, positive = "1")
# C15to16 <- makeClassifTask(data=d15to16, target = target, positive = "1")
# C16 <- makeClassifTask(data=d16, target = target, positive = "1")
# 
# lrn = makeLearner("classif.randomForest")

# perf <- NULL
# for (i in c(1:100)){
#   modC12to16 = train(lrn, C12to16)
#   modC13to16 = train(lrn, C13to16)
#   modC14to16 = train(lrn, C14to16)
#   modC15to16 = train(lrn, C15to16)
#   modC16 = train(lrn, C16)
#   
#   pred12to16.17 = predict(modC12to16, newdata = d17)
#   pred13to16.17 = predict(modC13to16, newdata = d17)
#   pred14to16.17 = predict(modC14to16, newdata = d17)
#   pred15to16.17 = predict(modC15to16, newdata = d17)
#   pred16.17 = predict(modC16, newdata = d17)
#   
#   p12to16.17 = performance(pred12to16.17, measures=list(acc))
#   p13to16.17 = performance(pred13to16.17, measures=list(acc))
#   p14to16.17 = performance(pred14to16.17, measures=list(acc))
#   p15to16.17 = performance(pred15to16.17, measures=list(acc))
#   p16.17 = performance(pred16.17, measures=list(acc))
#   
#   perf= rbind(perf, c(p12to16.17, p13to16.17, p14to16.17, p15to16.17, p16.17))
# }

# perf.m = apply(perf, 2, mean)
# perf.s = apply(perf, 2, sd)
# rbind(perf.m, perf.s)



####################
## Regression
####################
# data set
# dat <- d2[-1,]
# 
# # var groups
# retail <- c("retail_gas", "retail_gas_chg", "retail_dis", "retail_dis_chg")
# season <- c("month", "wkofyr")
# target <- c("Gas")
# 
# vars <- c(target, retail, season)
# 
# #test data
# d17 <- dat %>% filter(year %in% c(2017)) %>%  select(match(vars, names(.)))
# 
# #trainning data
# d12to16 <- dat %>% filter(year %in% c(2012:2016)) %>% select(match(vars, names(.)))
# d13to16 <- dat %>% filter(year %in% c(2013:2016)) %>% select(match(vars, names(.)))
# d14to16 <- dat %>% filter(year %in% c(2014:2016)) %>% select(match(vars, names(.)))
# d15to16 <- dat %>% filter(year %in% c(2015:2016)) %>% select(match(vars, names(.)))
# d16 <- dat %>% filter(year %in% c(2016)) %>% select(match(vars, names(.)))

# gas12to16.reg <- makeRegrTask(data=d12to16, target = "Gas")
# gas13to16.reg <- makeRegrTask(data=d13to16, target = "Gas")
# gas14to16.reg <- makeRegrTask(data=d14to16, target = "Gas")
# gas15to16.reg <- makeRegrTask(data=d15to16, target = "Gas")
# gas16.reg     <- makeRegrTask(data=d16, target = "Gas")

# define learner
#lrn = makeLearner("regr.randomForest")
#lrn = makeLearner("regr.bartMachine")
#lrn = makeLearner("regr.cubist")
#lrn = makeLearner("regr.gbm", par.vals=list(n.trees=1000, interaction.depth=2))
# lrn = makeLearner("regr.bst")
# lrn = makeLearner("regr.earth")
# lrn = makeLearner("regr.blackboost")

# trainning
# regGas12to16 = train(lrn, gas12to16.reg)
# regGas13to16 = train(lrn, gas13to16.reg)
# regGas14to16 = train(lrn, gas14to16.reg)
# regGas15to16 = train(lrn, gas15to16.reg)
# regGas16     = train(lrn, gas16.reg)
# 
# # prediction
# pred12to16.17 = predict(regGas12to16, newdata = d17)
# pred13to16.17 = predict(regGas13to16, newdata = d17)
# pred14to16.17 = predict(regGas14to16, newdata = d17)
# pred15to16.17 = predict(regGas15to16, newdata = d17)
# pred16.17     = predict(regGas16, newdata = d17)
# 
# # evaluate performance
# performance(pred12to16.17, measures=list(mae, rmse ))
# performance(pred13to16.17, measures=list(mae, rmse ))
# performance(pred14to16.17, measures=list(mae, rmse ))
# performance(pred15to16.17, measures=list(mae, rmse ))
# performance(pred16.17, measures=list(mae, rmse ))
# 

##############################
## Regression (Rolling Window)
##############################

# var groups
#allvar    <- names(allin1)
#retail <- c("retail_gas", "retail_gas_chg", "retail_dis", "retail_dis_chg")
#retail <- allvar[grep("weekly", allvar, ignore.case = T)[1:4]]
# retail <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "retail_dis") #"weekly_diesel") #
# #retail <- c("retail_gas")
# season <- c("month", "wkofyr", "wkofmo") 
# #season <- c()
# target <- c("Gas")
# 
# vars <- c(target, retail, season)
# #vars <- c("Gas", "weekly_regular", "weekly_mid_grade", "weekly_svpn", "retail_dis","month",           
# #"wkofyr", "wkofmo")
# # data set
# # dat <- d2[-1,]
# # dat <- allin1
# dat <- d2
# dat <- dat %>% dplyr::select(match(vars, names(.)))
# 
# dat=dat[-260,]
# 
# # rolling window parameters
# n <- nrow(dat)  # num of data  
# step <- 1       # moving step size
# 
# prediction <- d2
# #prediction <- allin1
# for(w in seq(54, 54, by=4) ){
# 
#     win <- w       # window size in weeks
#     win.start <- seq(1, n-win, by=step)  # windows start index
#     pred <- data.frame(week=(win.start[1]+win):n, NA)
#     names(pred)[2] <- paste0("pred_w",win)
#     lrn = makeLearner("regr.randomForest")
#     
#     for(i in 1:length(win.start)){
#       pred[i,2] <- ml_pred(
#                          train=dat[win.start[i]:(win.start[i]+win-1), ], 
#                          train=dat[1:(win.start[i]+win-1), ], 
#                          test=dat[win.start[i]+win,], 
#                          target=target, 
#                          task="regr", 
#                          lrn=lrn
#                          )
#     }
#     prediction <- left_join(prediction, pred, by="week")
# }
# 
# #write.csv(prediction, "./data/pred_win8wk_40wk_retail_predpt_included.csv", row.names = F)  
# write.csv(prediction, "./data/pred_win8wk_retail_only2.csv", row.names = F)  
# 
# 
# 
# p13 <- plot_ly(cbind(prediction)) %>%
#   add_trace(x = ~week, y = ~Gas, name = 'EIA actual Gasoline', type='scatter', mode = 'lines') %>%
#   add_trace(x = ~week, y = ~gas_mean, name = 'Bloomberg Gasoline', type='scatter', mode = 'lines') %>%
#   add_trace(x = ~week, y = ~pred_w54, name = 'Regression fit', type='scatter', mode = 'lines') %>%
#   layout(title = "Gasoline observation versus Prediction from retail", xaxis=list(title="Week"), yaxis=list(title="Weekly change"))
# p13
# 
# 
# p14 <- readRDS("d_retail_pred_plot.rds")
# p14


###########################################
## Regression (Rolling Window using caret
###########################################
# clean data
# allin1 <- readRDS("./data/allin1_retail.rds")
# d <- allin1 %>% select(lastwkd, week, month, wkofyr, wkofmo,
#                        Gas, Diesel,
#                        weekly_regular, weekly_mid_grade, weekly_svpn, weekly_diesel, weekly_other,
#                        weekly_regular_dif, weekly_mid_grade_dif, weekly_svpn_dif, weekly_diesel_dif,
#                        gas_Mean, diesel_Mean) %>% 
#                 mutate(retail_gas = weekly_regular + weekly_mid_grade + weekly_svpn) %>%                    
#                 mutate(retail_gas_chg=retail_gas-lag(retail_gas)) %>%                                      
#                 dplyr::rename(retail_dis_chg=weekly_diesel_dif, gas_mean=gas_Mean, dis_mean=diesel_Mean)
# saveRDS(d, "dat4pred.rds")
# load data
d <- readRDS("dat4pred.rds")

# group vars
retail <- c("weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel")
retail.chg <- c("weekly_regular_dif", "weekly_mid_grade_dif", "weekly_svpn_dif", "retail_dis_chg")
retail.tot <- c("retail_gas", "weekly_diesel")
retail.tot.chg <- c("retail_gas_chg", "retail_dis_chg")

season <- c("month", "wkofyr", "wkofmo") 
targetgas <- c("Gas")
targetdis <- c("Diesel")

gas.retail <- c(targetgas, retail, season)
gas.retail.chg <- c(targetgas, retail.chg, season)
gas.retail.tot <- c(targetgas, retail.tot, season)
gas.retail.tot.chg <- c(targetgas, retail.tot.chg, season)

dis.retail <- c(targetdis, retail, season)
dis.retail.chg <- c(targetdis, retail.chg, season)
dis.retail.tot <- c(targetdis, retail.tot, season)
dis.retail.tot.chg <- c(targetdis, retail.tot.chg, season)

## gas data
d.gas.retail <- d %>% dplyr::select(match(gas.retail, names(.)))
d.gas.retail.chg <- d %>% dplyr::select(match(gas.retail.chg, names(.)))
d.gas.retail.tot <- d %>% dplyr::select(match(gas.retail.tot, names(.)))
d.gas.retail.tot.chg <- d %>% dplyr::select(match(gas.retail.tot.chg, names(.)))

## dis data
d.dis.retail <- d %>% dplyr::select(match(dis.retail, names(.)))
d.dis.retail.chg <- d %>% dplyr::select(match(dis.retail.chg, names(.)))
d.dis.retail.tot <- d %>% dplyr::select(match(dis.retail.tot, names(.)))
d.dis.retail.tot.chg <- d %>% dplyr::select(match(dis.retail.tot.chg, names(.)))             
  

totn <- nrow(d)
initialWin <- 52

### creating sampling seeds ###
set.seed(123)
seeds <- vector(mode = "list", length = 217)
for(i in 1:216) seeds[[i]] <- sample.int(1000, 100)

### For the last model:
seeds[[217]] <- sample.int(1000, 1)

registerDoParallel(cores=8)
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = initialWin,
                              horizon = 1,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              seeds = seeds)

#tuneLength.num <- 5
tuneLength <- 5

prediction <- d
## Gas regression model 
gbm.gas.retail <- train(Gas ~ .,
                        data = d.gas.retail,
                        method = "gbm",
                        trControl = myTimeControl,
                        tuneLength=tuneLength,
                        verbose=FALSE)

pred <- data.frame(week=(initialWin+1):totn, NA)
pred <- data.frame(week=1:totn, NA)
names(pred)[2] <- paste0("pred_", "gbm_", "retail2" )
pred[,2] <- predict(gbm.gas.retail, d.gas.retail)
a1=postResample(pred = pred[,2], obs = d.gas.retail$Gas)
prediction <- left_join(prediction, pred, by="week")

gbm.gas.retail.chg <- train(Gas ~ .,
                        data = d.gas.retail.chg,
                        method = "gbm",
                        trControl = myTimeControl,
                        tuneLength=tuneLength,
                        verbose=FALSE)
pred <- data.frame(week=(initialWin+1):totn, NA)
names(pred)[2] <- paste0("pred_", "gbm_", "retail_chg" )
pred[,2] <- predict(gbm.gas.retail.chg, d.gas.retail.chg[(initialWin+1):totn,])
a2=postResample(pred = pred[,2], obs = d.gas.retail.chg$Gas)
prediction <- left_join(prediction, pred, by="week")

gbm.gas.retail.tot <- train(Gas ~ .,
                            data = d.gas.retail.tot,
                            method = "gbm",
                            trControl = myTimeControl,
                            tuneLength=tuneLength,
                            verbose=FALSE)
pred <- data.frame(week=(initialWin+1):totn, NA)
names(pred)[2] <- paste0("pred_", "gbm_", "retail_tot" )
pred[,2] <- predict(gbm.gas.retail.tot, d.gas.retail.tot[(initialWin+1):totn,])
a3=postResample(pred = pred[,2], obs = d.gas.retail.tot$Gas)
prediction <- left_join(prediction, pred, by="week")

gbm.gas.retail.tot.chg <- train(Gas ~ .,
                            data = d.gas.retail.tot.chg,
                            method = "gbm",
                            trControl = myTimeControl,
                            tuneLength=tuneLength,
                            verbose=FALSE)
pred <- data.frame(week=(initialWin+1):totn, NA)
names(pred)[2] <- paste0("pred_", "gbm_", "retail_tot_chg" )
pred[,2] <- predict(gbm.gas.retail.tot.chg, d.gas.retail.tot.chg[(initialWin+1):totn,])
a4=postResample(pred = pred[,2], obs = d.gas.retail.tot.chg$Gas)
prediction <- left_join(prediction, pred, by="week")

rf.gas.retail <- train(Gas ~ .,
                        data = d.gas.retail,
                        method = "rf",
                        trControl = myTimeControl,
                        tuneLength=tuneLength,
                        verbose=FALSE)
pred <- data.frame(week=(initialWin+1):totn, NA)
names(pred)[2] <- paste0("pred_", "rf_", "retail" )
pred[,2] <- predict(rf.gas.retail, d.gas.retail[(initialWin+1):totn,])
b1=postResample(pred = pred[,2], obs = d.gas.retail$Gas)
prediction <- left_join(prediction, pred, by="week")

rf.gas.retail.chg <- train(Gas ~ .,
                            data = d.gas.retail.chg,
                            method = "rf",
                            trControl = myTimeControl,
                            tuneLength=tuneLength,
                            verbose=FALSE)
pred <- data.frame(week=(initialWin+1):totn, NA)
names(pred)[2] <- paste0("pred_", "rf_", "retail_chg" )
pred[,2] <- predict(rf.gas.retail.chg, d.gas.retail.chg[(initialWin+1):totn,])
b2=postResample(pred = pred[,2], obs = d.gas.retail.chg$Gas)
prediction <- left_join(prediction, pred, by="week")

rf.gas.retail.tot <- train(Gas ~ .,
                            data = d.gas.retail.tot,
                            method = "rf",
                            trControl = myTimeControl,
                            tuneLength=tuneLength,
                            verbose=FALSE)
pred <- data.frame(week=(initialWin+1):totn, NA)
names(pred)[2] <- paste0("pred_", "rf_", "retail_tot" )
pred[,2] <- predict(rf.gas.retail.tot, d.gas.retail.tot[(initialWin+1):totn,])
b3=postResample(pred = pred[,2], obs = d.gas.retail.tot$Gas)
prediction <- left_join(prediction, pred, by="week")

rf.gas.retail.tot.chg <- train(Gas ~ .,
                                data = d.gas.retail.tot.chg,
                                method = "rf",
                                trControl = myTimeControl,
                                tuneLength=tuneLength,
                                verbose=FALSE)
pred <- data.frame(week=(initialWin+1):totn, NA)
names(pred)[2] <- paste0("pred_", "rf_", "retail_tot_chg" )
pred[,2] <- predict(rf.gas.retail.tot.chg, d.gas.retail.tot.chg[(initialWin+1):totn,])
b4=postResample(pred = pred[,2], obs = d.gas.retail.tot.chg$Gas)
prediction <- left_join(prediction, pred, by="week")

## performance summary
method=c("gbm_retail", "gbm_retail_chg", "gbm_retail_tot", "gbm_retail_tot_chg",
         "rf_retail", "rf_retail_chg", "rf_retail_tot", "rf_retail_tot_chg")

a=as.data.frame(rbind(a1,a2,a3,a4))
b=as.data.frame(rbind(b1,b2,b3,b4))
c=as.data.frame(rbind(a,b))
sol=cbind(method, c)







#######
# dis
#######

## regression model w/ retail only
gbm.dis.retail <- train(Diesel ~ .,
                        data = d.dis.retail,
                        method = "gbm",
                        trControl = myTimeControl,
                        tuneLength=tuneLength,
                        verbose=FALSE)

gbm.dis.retail.chg <- train(Diesel ~ .,
                            data = d.dis.retail.chg,
                            method = "gbm",
                            trControl = myTimeControl,
                            tuneLength=tuneLength,
                            verbose=FALSE)

gbm.dis.retail.tot <- train(Diesel ~ .,
                            data = d.dis.retail.tot,
                            method = "gbm",
                            trControl = myTimeControl,
                            tuneLength=tuneLength,
                            verbose=FALSE)

gbm.dis.retail.tot.chg <- train(Diesel ~ .,
                                data = d.dis.retail.tot.chg,
                                method = "gbm",
                                trControl = myTimeControl,
                                tuneLength=tuneLength,
                                verbose=FALSE)

rf.dis.retail <- train(Diesel ~ .,
                       data = d.dis.retail,
                       method = "rf",
                       trControl = myTimeControl,
                       tuneLength=tuneLength,
                       verbose=FALSE)

rf.dis.retail.chg <- train(Diesel ~ .,
                           data = d.dis.retail.chg,
                           method = "rf",
                           trControl = myTimeControl,
                           tuneLength=tuneLength,
                           verbose=FALSE)

rf.dis.retail.tot <- train(Diesel ~ .,
                           data = d.dis.retail.tot,
                           method = "rf",
                           trControl = myTimeControl,
                           tuneLength=tuneLength,
                           verbose=FALSE)

rf.dis.retail.tot.chg <- train(Diesel ~ .,
                               data = d.dis.retail.tot.chg,
                               method = "rf",
                               trControl = myTimeControl,
                               tuneLength=tuneLength,
                               verbose=FALSE)









pred.gas.gbm <- cbind(d, pred=predict(gbm.gas.mod, gasd))
pred.gas.rf  <- cbind(d, pred=predict(rf.gas.mod, gasd))




saveRDS(pred.gas.gbm, "pred.gas.gbm.rds")
saveRDS(pred.gas.rf, "pred.gas.rf.rds")


pgas.gbm <- plot_ly(cbind(d, pred=predict(gbm.gas.mod, gasd))) %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA actual Gasoline', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~gas_mean, name = 'Bloomberg Gasoline', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~pred, name = 'Regression fit', type='scatter', mode = 'lines') %>%
  layout(title = "Gasoline observation versus Prediction from retail", xaxis=list(title="Week"), yaxis=list(title="Weekly change"))

pgas.gbm
