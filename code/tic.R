#--------------------------------------------------------
# Working dir
#-------------------------------------------------------- 
setwd("C:\\Apps\\projects\\Retail")


#--------------------------------------------------------
# Libs and tools
#-------------------------------------------------------- 
rm(list=ls(all=TRUE))  # clean memory
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(lubridate)



#--------------------------------------------------------
# Load and processing data
#-------------------------------------------------------- 
# rb <- read.csv("./data/tic/GRB_Wed_2012_2017.csv", header=TRUE, na.strings="NA")
# ho <- read.csv("./data/tic/GHO_Wed_2012_2017.csv", header=TRUE, na.strings="NA")
# brn <- read.csv("./data/tic/crude_BRN_Wed_2012_2017.csv", header=TRUE, na.strings="NA")
# gcl <- read.csv("./data/tic/crude_GCL_Wed_2012_2017.csv", header=TRUE, na.strings="NA")
# 
# rb <- rb %>% select(date, RB=close)
# ho <- ho %>% select(date, HO=close)
# brn <- brn %>% select(date, BRN=close)
# gcl <- gcl %>% select(date, GCL=close)
# 
# 
# d <- left_join(rb, ho, by="date")
# d <- left_join(d, brn, by="date")
# d <- left_join(d, gcl, by="date")

# d <- d %>% rename(time=date)
# d <- d %>% mutate(date=as.Date(parse_date_time(time, orders="mdy HM"))) %>% 
#            select(date, time, RB, HO, BRN, GCL)
# 
# write.csv(d, file="./data/tic/crude_gas.csv", row.names = F)

# tic data
d <- read.csv("./data/tic/crude_gas.csv", header=T, na.strings = "NA")
d$date <- as.Date(d$date)

# corr
A <- d[,-c(1,2)]
M <- cor(A, use="pairwise.complete.obs")

png(paste0("crude","_circle.png"), width=650, height=650)
corrplot(M, method="circle", type="upper", col=brewer.pal(n=11, name="RdYlBu"))
dev.off()

png(paste0("crude","_num.png"), width=650, height=650)
corrplot(M, method="number", type="upper", col=brewer.pal(n=8, name="RdYlBu"))
dev.off()





# BB wrong, Pred right
pr.y <- read.csv("./data/tic/Diesel_rf_x2_52wk_fixWin_FALSE_BBNPredY.csv", header=T, na.strings = "NA")
pr.y$lastwkd <- as.Date(pr.y$lastwkd)
#View(pr.y)
pr.y <- pr.y %>% select(date=lastwkd, Diesel, BB=dis_mean, Pred=pred)
pr.y$date <- pr.y$date + 5  # next wed, i.e. EIA release date
pr.y.crude <- inner_join(d, pr.y)
#write.csv(pr.y.crude, file="./data/tic/crude_gas_pr_right.csv", row.names = F)
pr.y.crude.sum <- pr.y.crude %>% group_by(date) %>% summarise(min_RB=min(RB), max_RB=max(RB),
                                                              min_HO=min(HO), max_HO=max(HO),
                                                              min_BRN=min(BRN), max_BRN=max(BRN),
                                                              min_GCL=min(GCL), max_GCL=max(GCL),
                                                              EIA=mean(Diesel),
                                                              Pred=mean(Pred),
                                                              BB = mean(BB)
                                                              )

# corr
A <- pr.y.crude[,c(4,5,6)]
M <- cor(A, use="pairwise.complete.obs")

png(paste0("pr.y.bb.n","_circle.png"), width=650, height=650)
corrplot(M, method="circle", type="upper", col=brewer.pal(n=11, name="RdYlBu"))
dev.off()

png(paste0("pr.y.bb.n","_num.png"), width=650, height=650)
corrplot(M, method="number", type="upper", col=brewer.pal(n=8, name="RdYlBu"))
dev.off()


# BB right, Pred wrong
bb.y <- read.csv("./data/tic/Diesel_rf_x2_52wk_fixWin_FALSE_BBYPredN.csv", header=T, na.strings = "NA")
bb.y$lastwkd <- as.Date(bb.y$lastwkd)
#View(bb.y)
bb.y <- bb.y %>% select(date=lastwkd, Diesel, BB=dis_mean, Pred=pred)
bb.y$date <- bb.y$date + 5  # next wed, i.e. EIA release date
bb.y.crude <- inner_join(d, bb.y)
#write.csv(bb.y.crude, file="./data/tic/crude_gas_bb_right.csv", row.names = F)
bb.y.crude.sum <- bb.y.crude %>% group_by(date) %>% summarise(min_RB=min(RB), max_RB=max(RB),
                                                              min_HO=min(HO), max_HO=max(HO),
                                                              min_BRN=min(BRN), max_BRN=max(BRN),
                                                              min_GCL=min(GCL), max_GCL=max(GCL),
                                                              EIA=mean(Diesel),
                                                              Pred=mean(Pred),
                                                              BB = mean(BB)
                                                              )


#d %>% filter(date=="2012-01-04")



##########################################################
# study corrleation between retail and crude 
##########################################################
# load tic data
d <- read.csv("./data/tic/crude_gas.csv", header=T, na.strings = "NA")
d$date <- as.Date(d$date)

# create indicator before and after EIA release
d <- d %>% 
          mutate(hr=hour(mdy_hm(d$time)), min=minute(mdy_hm(d$time))) %>% 
          mutate(aftEIA=ifelse(hr==11 | min>30, 1, 0))

tic <- d %>% 
            group_by(date) %>% 
            summarise(RB_Mean = mean(RB),
                      HO_Mean = mean(HO),
                      BRN_Mean = mean(BRN),
                      GCL_Mean = mean(GCL)
                     )


# calculate crack value
crack <- d %>% mutate(Crack_RB_BRN=RB*42-BRN,
                      Crack_HO_BRN=HO*42-BRN,
                      Crack_RB_GCL=RB*42-GCL,
                      Crack_HO_GCL=HO*42-GCL
                      )

crack.sum <- crack %>% 
                      group_by(date, aftEIA) %>% 
                      summarise(
                        mean_crack_RB_BRN = mean(Crack_RB_BRN),
                        mean_crack_HO_BRN = mean(Crack_HO_BRN),
                        max_crack_RB_BRN = max(Crack_RB_BRN),
                        max_crack_HO_BRN = max(Crack_HO_BRN),
                        min_crack_RB_BRN = min(Crack_RB_BRN),
                        min_crack_HO_BRN = min(Crack_HO_BRN)
                      )

crack.aft <- crack.sum %>% 
                          filter(aftEIA==1) %>% 
                          rename(
                                 aft_mean_crack_RB_BRN=mean_crack_RB_BRN, 
                                 aft_mean_crack_HO_BRN=mean_crack_HO_BRN,
                                 aft_max_crack_RB_BRN=max_crack_RB_BRN,
                                 aft_max_crack_HO_BRN=max_crack_HO_BRN,
                                 aft_min_crack_RB_BRN=min_crack_RB_BRN,
                                 aft_min_crack_HO_BRN=min_crack_HO_BRN
                          )

crack.bf <- crack.sum %>% 
                         filter(aftEIA==0) %>% 
                         rename(
                                bf_mean_crack_RB_BRN=mean_crack_RB_BRN, 
                                bf_mean_crack_HO_BRN=mean_crack_HO_BRN,
                                bf_max_crack_RB_BRN=max_crack_RB_BRN,
                                bf_max_crack_HO_BRN=max_crack_HO_BRN,
                                bf_min_crack_RB_BRN=min_crack_RB_BRN,
                                bf_min_crack_HO_BRN=min_crack_HO_BRN
                         )

crack.1m.bf.eia <- crack %>% 
                            filter(hr==10, min==29) %>% 
                            select(date, time, Crack_RB_BRN, Crack_HO_BRN)

crack.sum2 <- left_join(crack.aft, crack.bf, by="date") %>% select(-contains("aftEIA"))
crack.sum3 <- left_join(crack.sum2, crack.1m.bf.eia, by="date")

crack.sum4 <- crack.sum3 %>% 
                          transmute(
                                    diff_mean_crack_gas=aft_mean_crack_RB_BRN - bf_mean_crack_RB_BRN,
                                    diff_mean_crack_dis=aft_mean_crack_HO_BRN - bf_mean_crack_HO_BRN,
                                    
                                    diff_mean_pct_crack_gas=(aft_mean_crack_RB_BRN - bf_mean_crack_RB_BRN)/bf_mean_crack_RB_BRN,
                                    diff_mean_pct_crack_dis=(aft_mean_crack_HO_BRN - bf_mean_crack_HO_BRN)/bf_mean_crack_HO_BRN,
                                    
                                    max_to_bf_EIA_crack_gas=aft_max_crack_RB_BRN/Crack_RB_BRN,
                                    max_to_bf_EIA_crack_dis=aft_max_crack_HO_BRN/Crack_HO_BRN,
                                    
                                    min_to_bf_EIA_crack_gas=aft_min_crack_RB_BRN/Crack_RB_BRN,
                                    min_to_bf_EIA_crack_dis=aft_min_crack_HO_BRN/Crack_HO_BRN,
                                    
                                    diff_max_to_bf_EIA_crack_gas=aft_max_crack_RB_BRN-Crack_RB_BRN,
                                    diff_max_to_bf_EIA_crack_dis=aft_max_crack_HO_BRN-Crack_HO_BRN,
                                    
                                    diff_min_to_bf_EIA_crack_gas=aft_min_crack_RB_BRN-Crack_RB_BRN,
                                    diff_min_to_bf_EIA_crack_dis=aft_min_crack_HO_BRN-Crack_HO_BRN,
                                    
                                    
                                    volaty_gas = aft_max_crack_RB_BRN - aft_min_crack_RB_BRN,
                                    volaty_dis = aft_max_crack_HO_BRN - aft_min_crack_HO_BRN
                                    )



# load master card data
# ma <- read.csv("./data/mas/gas_shell-mas.csv", header=T, na.strings = "NA")
# ma$lastwkd <- as.Date(ma$lastwkd)
# ma$date <- ma$lastwkd + 5  # next wed, i.e. EIA release date
# keep <- c("date", "retail_gas", "Master_gas")
# ma <- ma[, keep]



# load retail data
#setwd("C:\\Apps\\projects\\Retail\\code\\Ming")
allin1 <- readRDS("./code/Ming/allin1_retail.rds")

keep <- c("lastwkd",
          "weekly_regular", "weekly_mid_grade", "weekly_svpn", "weekly_diesel",
          "Diesel", "Gas", "gas_Mean", "diesel_Mean")
retail <- allin1[, keep]
retail <- retail %>% mutate(
                            diff_eia_to_bb_gas=Gas-gas_Mean,
                            diff_eia_to_bb_dis=Diesel-diesel_Mean,
                                
                            abs_diff_eia_to_bb_gas=abs(Gas-gas_Mean),
                            abs_diff_eia_to_bb_dis=abs(Diesel-diesel_Mean)
                            )

retail$date <- retail$lastwkd + 5  # next wed, i.e. EIA release date

# crack: join all data: retail, crack
allcrack <- left_join(retail, crack.sum4, by="date")



# join all data: retail, master, tic data
# all <- left_join(retail, ma, by="date")
# all <- left_join(all, tic, by="date")





# corr
#A <- all[,-c(1, 10)]
A <- allcrack[,-c(1, 14)]
M <- cor(A, use="pairwise.complete.obs")

#png(paste0("./doc/fig/corr/corr_retail_ma_tic","_circle.png"), width=1200, height=1200)
png(paste0("./doc/fig/corr/corr_crack_retail","_circle.png"), width=1000, height=1000)
corrplot(M, method="circle", type="upper", col=brewer.pal(n=11, name="RdYlBu"))
dev.off()

#png(paste0("./doc/fig/corr/corr_retail_ma_tic", "_num.png"), width=1200, height=1200)
png(paste0("./doc/fig/corr/corr_crack_retail", "_num.png"), width=1000, height=1000)
corrplot(M, method="number", type="upper", col=brewer.pal(n=8, name="RdYlBu"))
dev.off()

          
    
#all16to17 <- all %>% filter(year(date)>2015)
all16to17 <- allcrack %>% filter(year(date)>2015)
# corr
#A <- all16to17[,-c(1, 10)]
A <- all16to17[,-c(1, 14)]
M <- cor(A, use="pairwise.complete.obs")

png(paste0("./doc/fig/corr/corr_16to17_crack_retail","_circle.png"), width=1300, height=1300)
corrplot(M, method="circle", type="upper", col=brewer.pal(n=11, name="RdYlBu"))
dev.off()

png(paste0("./doc/fig/corr/corr_16to17_crack_retail", "_num.png"), width=1300, height=1300)
corrplot(M, method="number", type="upper", col=brewer.pal(n=8, name="RdYlBu"))
dev.off()

