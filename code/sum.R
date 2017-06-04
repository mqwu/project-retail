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
reqPackages <- c("tidyverse", "corrplot", "RColorBrewer", "lubridate", "plotly","caret", "ModelMetrics",
                 "gbm","randomForest", "party", "Cubist", "bst", "earth", "mboost", "doParallel", "pls") # mlr has conflict with caret
load_libs(reqPackages)

#--------------------------------------------------------
# Load data for summarize
#-------------------------------------------------------- 
## Gas
# a <- read.csv("./code/res2/Gas_rf_x6_52wk_fixWin_FALSE.csv", header=T)
# 
# b <- a %>%
#         mutate(diff.Gas=Gas-lag(Gas), 
#                diff.gas_mean=gas_mean-lag(gas_mean),
#                diff.gas.pred=pred-lag(pred)) %>% 
#         mutate(iGasMv=as.factor(ifelse(diff.Gas>0, 1, -1)),
#                iGasBBMv=as.factor(ifelse(diff.gas_mean>0, 1, -1)),
#                iGasPredMv=as.factor(ifelse(diff.gas.pred>0, 1, -1))
#               )
# 
# b <- b[53:nrow(b),]
# c <- b %>% 
#         select(Gas, gas_mean, pred, iGasMv, iGasBBMv, iGasPredMv) %>% 
#         mutate(bb_rmse=rmse(Gas, gas_mean), pred_rmse=rmse(Gas, pred),
#                bb_mae=mae(Gas, gas_mean), pred_mae=mae(Gas, pred)) %>% 
#         mutate(iGasEIAvBB=ifelse(iGasMv==iGasBBMv, 1, 0),
#                iGasEIAvPred=ifelse(iGasMv==iGasPredMv, 1, 0)) 
# write.csv(c, "./code/res2/Gas_rf_x6_52wk_fixWin_FALSE_rmse.csv")
# 
# # confusion matrix
# c <- c[-1,]
# xtab <- table(as.factor(c$iGasEIAvBB), as.factor(c$iGasEIAvPred))  
# xtab/nrow(c)
# 
# 
# d <- c %>% summarise(piGasEIAvBB=mean(iGasEIAvBB),
#                      piGasEIAvPred=mean(iGasEIAvPred))
#write.csv(d, "./code/res/gas_rf_x2_52w_fix5_dir.csv")        
  

## Dis
a <- read.csv("./code/res2/Diesel_rf_x2_52wk_fixWin_FALSE.csv", header=T)
#a <- read.csv("./code/res2/Diesel_rf_x2_52wk_fixWin_FALSE_cut2m.csv", header=T)

b <- a %>%
  mutate(diff.Dis=Diesel-lag(Diesel), 
         diff.dis_mean=dis_mean-lag(dis_mean),
         diff.dis.pred=pred-lag(pred),
         chg.retail.dis.diff=weekly_diesel_dif-lag(weekly_diesel_dif),
         resid=pred-Diesel) %>% 
  mutate(iDisMv=as.factor(ifelse(diff.Dis>0, 1, -1)),
         iDisBBMv=as.factor(ifelse(diff.dis_mean>0, 1, -1)),
         iDisPredMv=as.factor(ifelse(diff.dis.pred>0, 1, -1)),
         signDis=sign(Diesel),
         signBB =sign(dis_mean),
         signPred=sign(pred)
        )

#xx=b %>% select(lastwkd, weekly_diesel_dif, chg.retail.dis.diff)
# View(xx)
#write.csv(b, "./code/res2/Diesel_rf_x2_52wk_fixWin_FALSE_chgRetailDiff.csv", row.names = F)

c <- b[54:nrow(b),]

# Residual=c$resid
# acf(Residual)

## Prediction period 2013/1/18 -
# (bb.rmse=rmse(c$Diesel, c$dis_mean))
# (pred.rmse=rmse(c$Diesel, c$pred))
# (bb.mae=mae(c$Diesel, c$dis_mean))
# (pred.mae=mae(c$Diesel, c$pred))
# (bb.mdae=median(abs(c$Diesel-c$dis_mean)))
# (pred.mdae=median(abs(c$Diesel-c$pred)))

## all prediction
# d <- c %>%
#   select(lastwkd, Diesel, dis_mean, pred,
#          chg.retail.dis.diff, weekly_diesel_dif, weekly_diesel,
#          iDisMv, iDisBBMv, iDisPredMv,
#          signDis, signBB, signPred) %>%
#   mutate(bb_rmse=rmse(Diesel, dis_mean), pred_rmse=rmse(Diesel, pred),
#          bb_mae=mae(Diesel, dis_mean), pred_mae=mae(Diesel, pred),
#          bb_mdae=median(abs(Diesel-dis_mean)), pred_mdae=median(abs(Diesel-pred))) %>%
#   mutate(iDisEIAvBB=ifelse(iDisMv==iDisBBMv, 1, 0),
#          iDisEIAvPred=ifelse(iDisMv==iDisPredMv, 1, 0),
#          iSignEIAvBB=ifelse(signDis==signBB, 1, 0),
#          iSignEIAvPred=ifelse(signDis==signPred, 1, 0),
#          sel=1)



## threshold for retail
# cut <- -800000
# d <- c %>% 
#   select(lastwkd, Diesel, dis_mean, pred, 
#          chg.retail.dis.diff, weekly_diesel_dif, weekly_diesel,
#          iDisMv, iDisBBMv, iDisPredMv,
#          signDis, signBB, signPred) %>%
#   filter(weekly_diesel_dif<cut) %>% 
#   #filter(weekly_diesel_dif>cut) %>% 
#   #filter(abs(weekly_diesel_dif)>cut) %>% 
#   mutate(bb_rmse=rmse(Diesel, dis_mean), pred_rmse=rmse(Diesel, pred),
#          bb_mae=mae(Diesel, dis_mean), pred_mae=mae(Diesel, pred),
#          bb_mdae=median(abs(Diesel-dis_mean)), pred_mdae=median(abs(Diesel-pred))) %>% 
#   mutate(iDisEIAvBB=ifelse(iDisMv==iDisBBMv, 1, 0),
#          iDisEIAvPred=ifelse(iDisMv==iDisPredMv, 1, 0),
#          iSignEIAvBB=ifelse(signDis==signBB, 1, 0),
#          iSignEIAvPred=ifelse(signDis==signPred, 1, 0),
#          sel=1) 


## threshold for retail change
cut <- -2000000
#cut <- 4000000
d <- c %>%
  select(lastwkd, Diesel, dis_mean, pred,
         chg.retail.dis.diff, weekly_diesel_dif, weekly_diesel,
         iDisMv, iDisBBMv, iDisPredMv,
         signDis, signBB, signPred) %>%
  filter(chg.retail.dis.diff<cut) %>%
  #filter(chg.retail.dis.diff>cut) %>%
  #filter(abs(chg.retail.dis.diff)>cut) %>%
  mutate(bb_rmse=rmse(Diesel, dis_mean), pred_rmse=rmse(Diesel, pred),
         bb_mae=mae(Diesel, dis_mean), pred_mae=mae(Diesel, pred),
         bb_mdae=median(abs(Diesel-dis_mean)), pred_mdae=median(abs(Diesel-pred))) %>%
  mutate(iDisEIAvBB=ifelse(iDisMv==iDisBBMv, 1, 0),
         iDisEIAvPred=ifelse(iDisMv==iDisPredMv, 1, 0),
         iSignEIAvBB=ifelse(signDis==signBB, 1, 0),
         iSignEIAvPred=ifelse(signDis==signPred, 1, 0),
         sel=1)

# remove first prediction
d <- d[-1,]
out <- left_join(b, d, by='lastwkd')
out <- out %>% select(lastwkd, Diesel=Diesel.x, weekly_diesel=weekly_diesel.x, dis_mean=dis_mean.x,
                       pred=pred.x, chg.retail.dis.diff=chg.retail.dis.diff.x, weekly_diesel_dif=weekly_diesel_dif.x, sel)
#  
# 
#write.csv(out, "./code/res2/Diesel_rf_x2_52wk_fixWin_FALSE_2M_chgRetailDif.csv")

 # c <- b %>% 
#   select(Diesel, dis_mean, pred, iDisMv, iDisBBMv, iDisPredMv) %>% 
#   mutate(bb_rmse=rmse(Diesel, dis_mean), pred_rmse=rmse(Diesel, pred),
#          bb_mae=mae(Diesel, dis_mean), pred_mae=mae(Diesel, pred)) %>% 
#   mutate(iDisEIAvBB=ifelse(iDisMv==iDisBBMv, 1, 0),.
# write.csv(c, "./code/res2/Diesel_rf_x2_52wk_fixWin_FALSE_rmse.csv")

# confusion matrix

(xtab <- table(as.factor(d$iDisEIAvBB), as.factor(d$iDisEIAvPred))  )
xtab/nrow(d)

(xtab2 <- table(as.factor(d$iSignEIAvBB), as.factor(d$iSignEIAvPred))  )
xtab2/nrow(d)

BBYPredN <- d %>% filter(iSignEIAvBB==1 & iSignEIAvPred==0)
BBNPredY <- d %>% filter(iSignEIAvBB==0 & iSignEIAvPred==1)


# d <- c %>% summarise(piDisEIAvBB=mean(iDisEIAvBB),
#                      piDisEIAvPred=mean(iDisEIAvPred))
# write.csv(d, "./code/res/dis_rf_x2_52w_fix_dir.csv")        
