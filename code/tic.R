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
