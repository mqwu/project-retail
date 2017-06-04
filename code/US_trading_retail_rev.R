setwd("E:\\Trading_US_retail\\data")

# **************************************************
# Packages
# **************************************************

list.of.packages <- c("dplyr",
                      "plotly",
                      "lubridate",
                      "corrplot",
                      "mlr",
                      "caret",
                      "reshape2",
                      "zoo",
                      "doParallel",
                      "forecast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=TRUE)

rm(new.packages, list.of.packages)

# **************************************************
# Packages
# **************************************************

# trade <- read.csv("Loyalty Data 2016 Diego.csv", sep = ";", as.is = T)
# trade$transaction_date <-as.Date(trade$transaction_date, "%Y-%m-%d")
# 
# ### trade data QC
# summary(trade) 
# ### some zip codes don't look correct with 4 digits
# range(trade$transaction_date)
# sum(trade$fuel<0)   ### no negative fuel amounts
# sum(trade$fuel==0)  ### 3973 records with 0 fuel amount
# sum(duplicated(trade)) ### no duplicated records
# 
# #plot_ly(x=~trade$fuel, type = 'histogram') 
# 
# is_out 	<- function(x) x <= quantile(x,na.rm=T)[2] - 1.5*IQR(x,na.rm=T)|
#   x >= quantile(x,na.rm=T)[4] + 1.5*IQR(x,na.rm=T)
# 
# sum(is_out(trade$fuel)) 
# ### too many outliers
# ### may be able to cross check with population data if available
# trade[order(-trade$fuel)[1:10],] ### 10 records with highest fuel
# 
# #write.csv(trade[order(-trade$fuel)[1:50],],'high50.csv',row.names=F)
# #write.csv(trade[order(trade$fuel)[1:50],],'low50.csv',row.names=F)

# df    <- trade
# 
# ####daily data
# df2   <- df %>% select(transaction_date, fuel) %>% group_by(transaction_date) %>% summarise(totfuel = sum(fuel, na.rm = T)) 
# 
# p1     <- plot_ly(df2, x = ~transaction_date, y = ~totfuel, type = "scatter", mode = "lines" )
# p1

loadyear <- function(year) {
  df <- read.csv(paste0(year,"_Data_(Diego).csv"), as.is = T)
  df$transaction_date <-as.Date(df$transaction_date, "%Y-%m-%d")
  
  ### overall_fuel == mid_grade+svpn+diesel+other
  
  df$overall_fuel <- as.numeric(df$overall_fuel)
  df$regular      <- as.numeric(df$regular)
  df$mid_grade    <- as.numeric(df$mid_grade)
  df$svpn         <- as.numeric(df$svpn)
  df$diesel       <- as.numeric(df$diesel)
  df$other        <- as.numeric(df$other)
  
  df %>% select(transaction_date, regular, mid_grade, svpn, diesel, other) %>% group_by(transaction_date) %>% summarise(
    tot_regular = sum(regular, na.rm = T),
    tot_mid_grade = sum(mid_grade, na.rm = T),
    tot_svpn = sum(svpn, na.rm = T),
    tot_diesel = sum(diesel, na.rm = T),
    tot_other = sum(other, na.rm = T))
}

# ### Not run:
# daily2012 <- loadyear(2012)
# daily2013 <- loadyear(2013)
# daily2014 <- loadyear(2014)
# daily2015 <- loadyear(2015)
# daily2016 <- loadyear(2016)
# daily2017 <- loadyear(2017)
# df2 <- do.call("rbind", mget(sprintf("daily%04d",2012:2017)))
# write.csv(df2, "daily2012-2017.csv", row.names = F)

df2 <- read.csv("daily2012-2017.csv", as.is = T)
df2$transaction_date <- as.Date(df2$transaction_date)

### find first Saturday=7
saturdays <- df2[wday(df2$transaction_date) == 7,]
startDate <- min(saturdays$transaction_date)
endDate   <- max(saturdays$transaction_date)
df2$week  <- floor(as.numeric(difftime(df2$transaction_date, startDate, units = "weeks")))

### filter the first and last incomplete week
maxwk     <- ifelse(wday(endDate) == 6, max(df2$week), max(df2$week)-1)

### weekly data
df3 <- df2 %>% filter(week >= 0 & week <= maxwk) %>% group_by(week)    %>% 
  summarise(weekly_regular = sum(tot_regular, na.rm = T),
            weekly_mid_grade = sum(tot_mid_grade, na.rm = T),
            weekly_svpn = sum(tot_svpn, na.rm = T),
            weekly_diesel = sum(tot_diesel, na.rm = T),
            weekly_other = sum(tot_other, na.rm = T),
            lastwkd = max(transaction_date))
 
#df3$weeklydif <- df3$weekly_regular - lag(df3$weekly_regular)

p2    <- plot_ly(df3) %>%
  add_trace(x = ~week, y = ~weekly_regular, name = "regular", type = "scatter", mode = "lines" ) %>%
  add_trace(x = ~week, y = ~weekly_mid_grade, name = "mid_grade", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_svpn, name = "svpn", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_diesel, name = "diesel", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_other, name = "other", type = "scatter", mode = "lines") %>% layout(title = "Shell US retail 2012-2017", xaxis=list(title="Week"), yaxis=list(title="Weekly Total"))

p2

actual  <- read.csv("actual.csv", as.is = T)
actual$Date <- as.Date(actual$Date, "%m/%d/%Y")

### merge with actual stock change
df4 <- left_join(df3, actual, by=c('lastwkd'="Date")) 
# %>% 
#   mutate(RetailDZ = (weeklydif - mean(weeklydif, na.rm = T))/sd(weeklydif, na.rm=T),
#          RetailZ = (weeklytot - mean(weeklytot))/sd(weeklytot),
#          DieselZ = (Diesel - mean(Diesel))/sd(Diesel),
#          GasZ = (Gas - mean(Gas))/sd(Gas))

### tic data

### source data
tic <- read.csv("marketview.csv", as.is = T)
tic$DTM <- format(as.POSIXct(strptime(tic$Date,format="%m/%d/%Y %I:%M %p",tz="America/New_York")), tz="America/Chicago", usetz = T)
tic$DT  <- as.Date(tic$DTM)

holidays <- read.csv("holidays.csv", as.is = T)
holidays$alt <- as.Date(holidays$Alternate.Release.Date, "%d-%b-%y")

### each lag unit is 5 mins
mktlag <- function(k=0) {

wednesdays <- #function(k)
  tic %>% filter(wday(DT)==4, 
               format(as.POSIXct(DTM)-k*5*60,"%H:%M")=="09:30")

lagk <- wednesdays

#lag1 <- wednesdays(1)
#head(lag1)

lagk$week  <- floor(as.numeric(difftime(lagk$DT, startDate, units = "weeks")))-1

### replace holiday with alternative time 
idx <- which((lagk$DT+1) %in% holidays$alt)
lagk_replace <- #function(k)
  tic %>% filter(DT %in% holidays$alt, 
                 format(as.POSIXct(DTM)-k*5*60,"%H:%M")=="10:00")

lagk_rev <- lagk
lagk_rev[idx, 1:5] <- lagk_replace

##example: 2016-01-13 release on the week from 2016-01-02 to 2016-01-08
mview <- lagk_rev %>% filter(week>=0) %>% select(week, RBOB, ULSD)

left_join(df4, mview, by = "week") 
# %>%
#   mutate(RBOBZ = (RBOB - mean(RBOB))/sd(RBOB),
#          ULSDZ = (ULSD - mean(ULSD))/sd(ULSD))

}


df5 <- mktlag()

# p3  <- plot_ly(df5) %>%
#   add_trace(x=~lastwkd, y=~RetailZ, name = 'Retail', type='scatter', mode = 'lines') %>%
#   add_trace(x=~lastwkd, y=~RetailDZ, name = 'Retail change', type='scatter', mode = 'lines') %>%
#   add_trace(x=~lastwkd, y=~DieselZ, name = 'Diesel change', type='scatter', mode = 'lines') %>%
#   add_trace(x=~lastwkd, y=~GasZ, name = 'Gasoline change', type='scatter', mode = 'lines') %>% 
#   add_trace(x=~lastwkd, y=~RBOBZ, name = 'RBOB', type='scatter', mode = 'lines') %>%
#   add_trace(x=~lastwkd, y=~ULSDZ, name = 'ULSD', type='scatter', mode = 'lines') %>% layout(title = "US retail versus gas and diesel stock change <br> all standardized", xaxis=list(title="Date"), yaxis=list(title="Z"))
# 
# p3

corrplot(cor(df5[,-c(1,7,10:11)], use = 'complete.obs'), type='lower', tl.srt = 45)

### correlation matrix
cor(df5[,-c(1,7,10:11)])
cor(df5[,-c(1,7)])

### new tic data (-5, +30) on a minute base
newtic <- read.csv("EIA Reporting Period ET Nymex RB HO 2012-2017.csv", as.is = T)

newtic$DateTime <- mdy_hm(newtic$DateTime, tz = 'EST')

newtic2 <- newtic %>% 
  melt(id=c("DateTime",
            "Product")) %>%
  dcast(DateTime ~ Product + variable, fun.aggregate = sum)

### GHO: Heating oil, GRB: RBOB gasoline

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Volume"
)

mymode <-'lines'

p3 <- plot_ly(newtic2) %>%
  add_trace(x=~DateTime, y=~GHO_Price, name = 'GHO Price', type='scatter', mode = mymode) %>%
  add_trace(x=~DateTime, y=~GRB_Price, name = 'GRB Price', type='scatter', mode = mymode) %>%
  add_trace(x=~DateTime, y=~GHO_Volume, name = 'GHO Volume', type='scatter', mode = mymode, yaxis = "y2") %>%
  add_trace(x=~DateTime, y=~GRB_Volume, name = 'GRB Volume', type='scatter', mode = mymode, yaxis = "y2") %>%
  layout(title = "Tic data from 2012-2017", xaxis=list(title="DateTime"), yaxis=list(title="Price"), yaxis2 = ay)

p3

newtic2$DateTime[newtic2$GHO_Price==0]

### correlate tic data with EIA and Bloomberg
newtic2$Date <- as.Date(newtic2$DateTime)
newtic3 <- newtic2 %>% 
  group_by(Date) %>% 
  summarise(GHO_Price_Mean = mean(GHO_Price),
            GHO_Price_Median = median(GHO_Price),
            GHO_Price_SD = sd(GHO_Price),
            GHO_Price_Range = diff(range(GHO_Price)),
            GHO_Price_IQR = IQR(GHO_Price),
            GHO_Volume_Max = max(GHO_Volume),
            GHO_Value_Max = max(GHO_Price*GHO_Volume),
            GRB_Price_Mean = mean(GRB_Price),
            GRB_Price_Median = median(GRB_Price),
            GRB_Price_SD = sd(GRB_Price),
            GRB_Price_Range = diff(range(GRB_Price)),
            GRB_Price_IQR = IQR(GRB_Price),
            GRB_Volume_Max = max(GRB_Volume),
            GRB_Value_Max = max(GRB_Price*GRB_Volume)
            )

table(weekdays(newtic3$Date))
newtic3$lastwkd <- newtic3$Date-wday(newtic3$Date+1)
table(weekdays(newtic3$lastwkd))

# ### evalute the time lag effect on the correlations
# ### correlation remains stable for a couple of weeks before getting weak
# lapply(1:5, function(k) cor(df5$RetailZ, lag(df5$GasZ,k), use='complete.obs'))
# lapply(1:5, function(k) cor(df5$RetailZ, lag(df5$DieselZ,k), use='complete.obs'))
# 
# lapply(0:1, function(k) with (mktlag(k), cor(GasZ, RBOBZ)))
# 
# saveRDS(df5, 'weeklydata.RDS')

### import bloomberg analyst data
gas_summary   <- read.csv("gas_summary.csv", as.is = T)
names(gas_summary)[-1] <- paste("gas", sub("X([0-9]+).", "P\\1",names(gas_summary)[-1]), sep = "_")
gas_summary$Observation.Date <- as.Date(gas_summary$Observation.Date)

dis_summary   <- read.csv("dis_summary.csv", as.is = T)
names(dis_summary)[-1] <- paste("diesel", sub("X([0-9]+).", "P\\1",names(dis_summary)[-1]), sep = "_")
dis_summary$Observation.Date <- as.Date(dis_summary$Observation.Date)

### merge into weekly data above

df6 <- merge(df5, 
             gas_summary, 
             by.x = 'lastwkd',
             by.y = 'Observation.Date')

df6 <- merge(df6,
             dis_summary,
             by.x = 'lastwkd',
             by.y = 'Observation.Date')

sum(df6$Diesel!=df6$diesel_Actual)
sum(df6$Gas!=df6$gas_Actual)
df6$diesel_Actual <- NULL
df6$gas_Actual  <- NULL

#View(df6)
# df6 <- rename(df6, RetailChg=RetailDZ)
# df6 <- rename(df6, Retail=RetailZ)
columns <- names(df6)[c(3:7,13,25,8:9)]
#corrplot(cor(df6[,-c(1:6,11:12)], use = 'complete.obs'), type='lower', tl.srt = 45)
corrplot(cor(df6[columns], use = 'complete.obs'), type='lower', tl.srt = 45)

### daily volume
CF20142 <- read.csv("Daily Volumes_2016_SOPUS1.0_6_2014_12_2014.csv", as.is = T)
CF20151 <- read.csv("Daily Volumes_2016_SOPUS1.0_1_2015_6_2015.csv", as.is = T)
CF20152 <- read.csv("Daily Volumes_2016_SOPUS1.0_6_2015_12_2015.csv", as.is = T)
#CF20161 <- read.csv("Daily Volumes_2016_SOPUS1.0_1_2016_6_2016.csv", as.is = T)
#CF20162 <- read.csv("Daily Volumes_2016_SOPUS1.0_6_2016_12_2016.csv", as.is = T)
CF201617 <- read.csv("Daily Volumes_2016_SOPUS1.0_2.csv", as.is = T)

dayvol  <- Reduce(function(x, y) merge(x, 
                                       y, 
                                       all = TRUE,
                                       by = c("Supply.Point",
                                              "Product.Group")),
                                       list(CF20142,
                                            CF20151,
                                            CF20152,
                                            CF201617))

#dayvol  <- read.csv("Daily Volumes_2016_SOPUS1.0_2.csv", as.is = T)
mdayvol <- melt(dayvol, id = names(dayvol)[1:2])
dayvol2 <- mdayvol %>% mutate(Date = as.Date(sub("^X","",as.character(variable)),"%d.%m.%Y"),
                              Volume = as.numeric(gsub(",|GAL","",as.character(value))),
                              variable = NULL,
                              value = NULL)
#View(dayvol2)

### data QC
#View(dayvol2[which(dayvol2$Volume<0),])
#write.csv(dayvol2[which(dayvol2$Volume<=0),], "dailyvol_check.csv", row.names=F)
#write.csv(dayvol2[order(dayvol2$Volume, decreasing = T)[1:50],], "dailyvol_high50.csv", row.names = F)

### filter out records from red/yellow/green lists

dayvol2 <- filter(dayvol2, !grepl("TESWilmingtonCA", Supply.Point))
dayvol2 <- filter(dayvol2, !(Supply.Point=="Y032 : US1008#N Seattle WA Depot" & Product.Group=="Industrial / Domestic Gas Oil"))
dayvol2 <- filter(dayvol2, !grepl("WY MAG CHEYENNE", Supply.Point))
dayvol2 <- filter(dayvol2, !(Supply.Point=="Y171 : US1164#M Rockford IL BET Depot" & Product.Group=="Automotive"))
dayvol2 <- filter(dayvol2, !grepl("SCR ComrceCtyCO|SIN Casper WY|HFR Cheyenne WY|MAG Fountain CO|SCR GrandJuncCO", Supply.Point))

### filter out all negative records
#dayvol2 <- dayvol2 %>% filter(Volume>=0)

### convert monthly corrections to daily then merge back
dayvol2p <- dayvol2 %>% filter(Volume>=0)
crrctn <- dayvol2 %>% filter(Volume<0) %>% mutate(monthstart = Date - mday(Date) + 1)

daily <- as.data.frame(seq(min(crrctn$Date), as.Date(as.yearmon(max(crrctn$Date)), frac = 1), by = 'days'))
names(daily)  <- "Date"
daily$monthstart <- daily$Date - mday(daily$Date)+1
daily$days    <- days_in_month(daily$Date)

dailycrrctn   <- merge(crrctn %>% select(-Date),
                       daily,
                       by = c("monthstart"),
                       all.x = T)

dailycrrctn2 <- dailycrrctn %>% mutate(Correction = Volume/days) %>% select(-c(monthstart,Volume, days))

dayvol2c  <- merge(dayvol2p,
                   dailycrrctn2,
                   by=c("Date","Supply.Point", "Product.Group"),
                   all.x = T,
                   all.y = T)

dayvol2c$Volume[is.na(dayvol2c$Volume)] <- 0
dayvol2c$Correction[is.na(dayvol2c$Correction)] <- 0
dayvol2c$Volumec <- dayvol2c$Volume + dayvol2c$Correction

### aggregate across US on daily data
#dayvol3   <- dayvol2 %>% group_by(Product.Group, Date) %>% summarise(TotVol = sum(Volume, na.rm = T))

dayvol3   <- dayvol2c %>% group_by(Product.Group, Date) %>% summarise(TotVol = sum(Volumec, na.rm = T)) 
dayvol4   <- dcast(dayvol3, Date~Product.Group, value.var = "TotVol")
dayvol3   <- merge(dayvol3,
                   melt(dayvol4, id="Date"),
                   by.x = c("Date", "Product.Group", "TotVol"),
                   by.y = c("Date", "variable", "value"),
                   all.y = T)

p4  <- plot_ly(dayvol4) %>% 
  add_trace(x=~Date, y=~eval(as.name(names(dayvol4[2]))), name = names(dayvol4)[2] , type='scatter', mode = 'lines') %>% 
  add_trace(x=~Date, y=~eval(as.name(names(dayvol4[3]))), name = names(dayvol4)[3] , type='scatter', mode = 'lines') %>%
  add_trace(x=~Date, y=~eval(as.name(names(dayvol4[4]))), name = names(dayvol4)[4] , type='scatter', mode = 'lines') %>%
  add_trace(x=~Date, y=~eval(as.name(names(dayvol4[5]))), name = names(dayvol4)[5] , type='scatter', mode = 'lines') %>% layout(title = "Daily US commercial Fuel", xaxis=list(title="Date"), yaxis=list(title="Total Volume (Gal)"))

p4

### aggregate daily data into weekly data
### find first Saturday=7 
# saturdays <- dayvol3[wday(dayvol3$Date) == 7,]
# startDate <- min(saturdays$Date)
# endDate   <- max(saturdays$Date)
# inherit startDate and endDate from retail data
dayvol3$week  <- floor(as.numeric(difftime(dayvol3$Date, startDate, units = "weeks")))

### filter the first and last incomplete week
maxwk     <- ifelse(wday(endDate) == 6, max(dayvol3$week), max(dayvol3$week)-1)

### weekly data
wkvol <- dayvol3 %>% 
  filter(week >= 0 & week <= maxwk) %>% 
  group_by(Product.Group, week) %>% 
  summarise(weeklytot = sum(TotVol, na.rm = T),
            lastwkd = max(Date))

wkvol2   <- dcast(wkvol, week+lastwkd ~ Product.Group, value.var = "weeklytot")
wkvol2 <- rename(wkvol2, Motor_Gasoline = `Motor Gasoline`)
### combine Automotive Gas oil with Industrial/Domestic Gas Oil
wkvol2$Gas_Oil <- rowSums(wkvol2[,grep("Gas Oil", names(wkvol2))])

wkvol2$Motor_Gasoline[1] <- NA
wkvol2$`Automotive Gas Oil`[1] <- NA
wkvol2$`Industrial / Domestic Gas Oil`[1] <- NA
wkvol2$`Bio-fuels`[1] <- NA
wkvol2$Gas_Oil[1] <- NA
wkvol2$Slops[1] <-NA

p5  <- plot_ly(wkvol2) %>% 
  add_trace(x=~lastwkd, y=~eval(as.name(names(wkvol2[3]))), name = names(wkvol2)[3] , type='scatter', mode = 'lines') %>%
  add_trace(x=~lastwkd, y=~eval(as.name(names(wkvol2[4]))), name = names(wkvol2)[4] , type='scatter', mode = 'lines') %>%
  add_trace(x=~lastwkd, y=~eval(as.name(names(wkvol2[5]))), name = names(wkvol2)[5] , type='scatter', mode = 'lines') %>% 
  add_trace(x=~lastwkd, y=~eval(as.name(names(wkvol2[6]))), name = names(wkvol2)[6] , type='scatter', mode = 'lines') %>% layout(title = "Weekly US commercial Fuel", xaxis=list(title="Week"), yaxis=list(title="Total Volume (Gal)"))

p5

### merge commercial fuel data with df6 above
wkvol3 <- wkvol2 %>% select(week, lastwkd, Motor_Gasoline, Gas_Oil)

allin1 <- merge(df6, 
                wkvol3, 
                all.x = T,
                by=c("lastwkd", "week"))

allin1 <- merge(allin1,
                newtic3,
                all.x = T,
                by=c("lastwkd"))


p6    <- plot_ly(allin1) %>%
  add_trace(x = ~week, y = ~weekly_regular, name = "regular", type = "scatter", mode = "lines" ) %>%
  add_trace(x = ~week, y = ~weekly_mid_grade, name = "mid_grade", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_svpn, name = "svpn", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_diesel, name = "diesel", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_other, name = "other", type = "scatter", mode = "lines") %>% 
  add_trace(x = ~week, y = ~Motor_Gasoline, name = "comm gas", type = "scatter", mode = "lines") %>% 
  add_trace(x = ~week, y = ~Gas_Oil, name = "comm diesel", type = "scatter", mode = "lines") %>% 
  
  layout(title = "Shell US retail 2012-2017 <br>  commercial 2014-2017", xaxis=list(title="Week"), yaxis=list(title="Weekly Total"))

p6


ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Stock Change"
)

p7    <- plot_ly(allin1) %>%
  add_trace(x = ~week, y = ~weekly_regular, name = "regular", type = "scatter", mode = "lines" ) %>%
  add_trace(x = ~week, y = ~weekly_mid_grade, name = "mid_grade", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_svpn, name = "svpn", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_diesel, name = "diesel", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~weekly_other, name = "other", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~Motor_Gasoline, name = "comm gas", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~Gas_Oil, name = "comm diesel", type = "scatter", mode = "lines") %>%
  add_trace(x = ~week, y = ~Diesel, name = 'EIA Actual Diesel', type='scatter', mode = 'lines', yaxis = "y2") %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA Actual Gas', type='scatter', mode = 'lines', yaxis = "y2") %>%
  add_trace(x = ~week, y = ~diesel_Mean, name = 'Bloomberg Diesel', type='scatter', mode = 'lines', yaxis = "y2") %>%
  add_trace(x = ~week, y = ~gas_Mean, name = 'Bloomberg Gas', type='scatter', mode = 'lines', yaxis = "y2") %>%
  layout(title = "Shell US retail 2012-2017 <br>  commercial 2014-2017 <br> EIA report <br> Bloomberg estimate", xaxis=list(title="Week"), yaxis=list(title="Weekly Total"), yaxis2 = ay)

p7

p8 <- plot_ly(allin1) %>%
  add_trace(x = ~week, y = ~weekly_diesel, name = "Shell Retail Diesel", type = "scatter", mode = "lines" ) %>%
  add_trace(x = ~week, y = ~Diesel, name = 'EIA Actual Diesel', type='scatter', mode = 'lines', yaxis = "y2") %>%
  add_trace(x = ~week, y = ~diesel_Mean, name = 'Bloomberg Diesel', type='scatter', mode = 'lines', yaxis = "y2") %>%
  layout(title = "Diesel 2016-2017", xaxis=list(title="Week", range=c(208, 268)), yaxis=list(title="Weekly Total", range = c(0,25e6)), yaxis2 = ay)

p8

p9 <- plot_ly(allin1) %>%
  add_trace(x = ~week, y = ~weekly_regular + weekly_mid_grade + weekly_svpn, name = "Shell Retail Gasoline", type = "scatter", mode = "lines" ) %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA Actual Gasoline', type='scatter', mode = 'lines', yaxis = "y2") %>%
  add_trace(x = ~week, y = ~gas_Mean, name = 'Bloomberg Gasoline', type='scatter', mode = 'lines', yaxis = "y2") %>%
  layout(title = "Gasoline 2016-2017", xaxis=list(title="Week", range=c(208, 268)), yaxis=list(title="Weekly Total", range = c(0,500e6)), yaxis2 = ay)

p9

p10 <- plot_ly(allin1) %>%
  add_trace(x = ~week, y = ~Gas_Oil, name = "Shell Commercial Diesel", type = "scatter", mode = "lines" ) %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA Actual Gasoline', type='scatter', mode = 'lines', yaxis = "y2") %>%
  add_trace(x = ~week, y = ~gas_Mean, name = 'Bloomberg Gasoline', type='scatter', mode = 'lines', yaxis = "y2") %>%
  layout(title = "Diesel 2016-2017", xaxis=list(title="Week", range=c(208, 268)), yaxis=list(title="Weekly Total", range = c(0,16e6)), yaxis2 = ay)

p10

p11 <- plot_ly(allin1) %>%
  add_trace(x = ~week, y = ~Motor_Gasoline, name = "Shell Commercial Gasoline", type = "scatter", mode = "lines" ) %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA Actual Gasoline', type='scatter', mode = 'lines', yaxis = "y2") %>%
  add_trace(x = ~week, y = ~gas_Mean, name = 'Bloomberg Gasoline', type='scatter', mode = 'lines', yaxis = "y2") %>%
  layout(title = "Gasoline 2016-2017", xaxis=list(title="Week", range=c(208, 268)), yaxis=list(title="Weekly Total", range = c(0,120e6)), yaxis2 = ay)

p11

### modeling framework: no stable due to sample size!!!

# mymodel <- function(mydata, mytarget, mytask, mylearner, mymeasure) {
#   n     <- nrow(mydata)
#   train.set <- seq(1,2/3*n,1)
#   test.set  <- setdiff(1:n, train.set)
#   if (mytask=='Regression')
#     task  <- makeRegrTask(data=mydata, target = mytarget)
#   if (mytask=='Classification')
#     task  <- makeClassifTask(data=mydata, target = mytarget, positive = "1")
#   lrn   <- makeLearner(mylearner)
#   model <- train(lrn, task, subset = train.set)
#   pred  <- predict(model, task = task, subset = test.set)
#   performance(pred, measures = mymeasure)
#   
# }
# 
# mymodel(mydata = gasdata, 
#         mytarget = "Gas", 
#         mytask = "Regression", 
#         mylearner = "regr.randomForest", 
#         mymeasure = list(mae, rmse))
# 
# mymodel(mydata = gasdata2, 
#         mytarget = "Gas", 
#         mytask = "Classification", 
#         mylearner = "classif.randomForest", 
#         mymeasure = list(mmce, acc))
# 
# mymodel(mydata = dieseldata, 
#         mytarget = "Diesel", 
#         mytask = "Regression", 
#         mylearner = "regr.randomForest", 
#         mymeasure = list(mae, rmse))
# 
# mymodel(mydata = dieseldata2, 
#         mytarget = "Diesel", 
#         mytask = "Classification", 
#         mylearner = "classif.randomForest", 
#         mymeasure = list(mmce, acc))



### add vars for seasonality
allin1$month  <- month(allin1$lastwkd, label = T)
allin1$wkofyr <- as.ordered(week(ymd(allin1$lastwkd)))
allin1$wkofmo <- as.ordered(ceiling(day(allin1$lastwkd)/7))

### add vars for weekly difference
allin1 <- allin1 %>% 
  mutate(weekly_regular_dif = weekly_regular - lag(weekly_regular),
         weekly_mid_grade_dif = weekly_mid_grade - lag(weekly_mid_grade),
         weekly_svpn_dif = weekly_svpn - lag(weekly_svpn),
         weekly_diesel_dif = weekly_diesel - lag(weekly_diesel))

### add indicator for market shock

allin1 <- allin1 %>%
  mutate(gshock = Gas - gas_Mean,
         dshock = Diesel- diesel_Mean)
t1 <- with(allin1, quantile(abs(gshock), .95))
t2 <- with(allin1, quantile(abs(dshock), .95))

allin1 <- allin1 %>% 
  mutate(shock= (abs(gshock) >t1 | abs(dshock) >t2 ))

### load data
allin1 <- readRDS("./data/allin1_retail.rds")

columns <- names(allin1)[c(3:6, 36:37, 13, 25, 8:9)]
columns2 <- names(allin1)[c(1,3:6, 36:37, 13, 25, 8:9)]


#corrplot(cor(allin1[,-c(1:4,9:12)], use = 'complete.obs'), type='lower', tl.srt = 45)



corrmatrix <- allin1[columns]
names(corrmatrix) <- c("Shell_retail_regular",
                       "Shell_retail_midgrade",
                       "Shell_retail_supervpower",
                       "Shell_retail_diesel",
                       "Shell_commercial_gasoline",
                       "Shell_commercial_diesel",
                       "Bloomberg_estimated_gasoline",
                       "Bloomberg_estimated_diesel",
                       "EIA_reported_diesel",
                       "EIA_reported_gasoline")

corrmatrix2 <- allin1[columns2]
names(corrmatrix2) <- c("lastwkd",
                       "Shell_retail_regular",
                       "Shell_retail_midgrade",
                       "Shell_retail_supervpower",
                       "Shell_retail_diesel",
                       "Shell_commercial_gasoline",
                       "Shell_commercial_diesel",
                       "Bloomberg_estimated_gasoline",
                       "Bloomberg_estimated_diesel",
                       "EIA_reported_diesel",
                       "EIA_reported_gasoline")
#corrplot(cor(corrmatrix, use = 'pairwise'), type='lower', tl.srt = 45)
#corrplot(cor(corrmatrix, use = 'pairwise'), type='full', tl.srt = 45)

corrplot(cor(corrmatrix, use = 'complete'), type='lower', tl.srt = 45)
corrplot(cor(corrmatrix, use = 'complete'), type='full', tl.srt = 45)

corrplot(cor(corrmatrix[131:264,], use = 'complete'), method="number",type='lower', tl.srt = 45)

p7shock    <- plot_ly(allin1) %>%
  add_trace(x = ~week, y = ~Diesel, name = 'EIA Actual Diesel 2', type='scatter', mode = 'markers', symbol = ~shock, symbols = c('o','x'), color = I('red')) %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA Actual Gas 2', type='scatter', mode = 'markers',  symbol = ~shock, symbols = c('o','x'), color = I('blue')) %>%
  add_trace(x = ~week, y = ~Diesel, name = 'EIA Actual Diesel', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA Actual Gas', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~diesel_Mean, name = 'Bloomberg Diesel', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~gas_Mean, name = 'Bloomberg Gas', type='scatter', mode = 'lines') %>%
  layout(title = "Shell US retail 2012-2017 <br>  commercial 2014-2017 <br> EIA report <br> Bloomberg estimate", xaxis=list(title="Week"), yaxis=list(title="Weekly Change"))

p7shock

#saveRDS(allin1, "allin1_retail.rds")

usediff   <- 0
allvar    <- names(allin1)
#bbgasvar  <- allvar[grep("gas", allvar)]
#gasvar <- c(gasvar, "RBOB")
#bbdieselvar <- allvar[grep("diesel", allvar)]
#dieselvar <- c(dieselvar, "ULSD")
bbvar     <- c("gas_Mean", "gas_Max", "diesel_Mean", "diesel_Max")
ifelse(usediff, 
       retailvar <- allvar[grep("dif", allvar, ignore.case = T)],
       retailvar <- allvar[grep("weekly", allvar, ignore.case = T)[1:4]])
cfvar     <- c("Motor_Gasoline", "Gas_Oil")
ssvar     <- c("month", "wkofyr", "wkofmo") 
ticvar    <- allvar[grep("GHO|GRB", allvar)]

### gas data preparation
gasdata <- allin1[,c("Gas", retailvar, cfvar, ssvar)]
if(usediff) gasdata <- gasdata[-1,]
gasdata2 <- gasdata %>% mutate(Gas=as.factor(ifelse(Gas>0, 1, -1)))
#gasdata3 <- gasdata %>% mutate(Move=as.factor(ifelse(Gas>lag(Gas), "Up", "Down")))

### diesel data preparation
dieseldata <- allin1[,c("Diesel", retailvar, cfvar, ssvar)]
if(usediff) dieseldata <- dieseldata[-1,]
dieseldata2 <- dieseldata %>% mutate(Diesel=as.factor(ifelse(Diesel>0, 1, -1)))
#dieseldata3 <- dieseldata %>% mutate(Move=as.factor(ifelse(Diesel>lag(Diesel), "Up", "Down")))

# 
# ### classification models
# gasCtask  <- makeClassifTask(data=gasdata2, target = "Gas", positive = "1")
# dieselCtask  <- makeClassifTask(data=dieseldata2, target = "Diesel", positive = "1")
# 
# lrns = list(makeLearner("classif.randomForest"),
#             makeLearner("classif.lda"),
#             makeLearner("classif.rpart"),
#             makeLearner("classif.nnet"),
#             makeLearner("classif.svm"))
# tasks = list(gasCtask, dieselCtask)
# rdesc = makeResampleDesc("CV", iters = 2L)
# measures = list(acc, ber)
# 
# benchmark(lrns, tasks, rdesc, measures)
# 
# ### regression models
# gasRtask  <- makeRegrTask(data=gasdata, target = "Gas")
# dieselRtask  <- makeRegrTask(data=dieseldata, target = "Diesel")
# 
# lrns = list(makeLearner("regr.randomForest"),
#             makeLearner("regr.cubist"),
#             makeLearner("regr.rpart"),
#             makeLearner("regr.nnet"),
#             makeLearner("regr.svm"))
# tasks = list(gasRtask, dieselRtask)
# rdesc = makeResampleDesc("CV", iters = 2L)
# measures = list(mae, rmse)
# 
# benchmark(lrns, tasks, rdesc, measures)

# n     <- nrow(gasdata)
# train.set <- seq(1,2/3*n,1)
# test.set  <- setdiff(1:n, train.set)
# 
# ### regression model
# task  <- makeRegrTask(data=gasdata, target = "Gas")
# lrn   <- makeLearner("regr.randomForest")
# model <- train(lrn, task, subset = train.set)
# pred  <- predict(model, task = task, subset = test.set)
# performance(pred, measures = list(mae, rmse))
# 
# ### classification model
# task  <- makeClassifTask(data=gasdata2, target = "Gas", positive = "1")
# lrn   <- makeLearner("classif.randomForest")
# model <- train(lrn, task, subset = train.set)
# pred  <- predict(model, task = task, subset = test.set)
# performance(pred, measures = list(mmce, acc))
# 
# 
# ### regression model
# task  <- makeRegrTask(data=dieseldata, target = "Diesel")
# lrn   <- makeLearner("regr.randomForest")
# model <- train(lrn, task, subset = train.set)
# pred  <- predict(model, task = task, subset = test.set)
# performance(pred, measures = list(mae, rmse))
# 
# ### classification model
# task  <- makeClassifTask(data=dieseldata2, target = "Diesel", positive = "1")
# lrn   <- makeLearner("classif.randomForest")
# model <- train(lrn, task, subset = train.set)
# pred  <- predict(model, task = task, subset = test.set)
# performance(pred, measures = list(mmce, acc))

### creating sampling seeds ###
set.seed(123)
seeds <- vector(mode = "list", length = 217)
for(i in 1:216) seeds[[i]] <- sample.int(1000, 100)

### For the last model:
seeds[[217]] <- sample.int(1000, 1)


registerDoParallel(cores=2)
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 52,
                              horizon = 1,
                              fixedWindow = FALSE,
                              allowParallel = TRUE,
                              seeds = seeds)

tuneLength.num <- 5
# 
# ### Gasoline classification model
# logit.mod <- train(Gas ~ .,
#                 data = gasdata2,
#                 method = "glm",
#                 family = binomial,
#                 trControl = myTimeControl,
#                 tuneLength=tuneLength.num)
# 
# rf.mod <- train(Gas ~ .,
#                 data = gasdata2,
#                 method = "rf",
#                 trControl = myTimeControl,
#                 tuneLength=tuneLength.num)
# 
# gbm.mod <- train(Gas ~ .,
#                 data = gasdata2,
#                 method = "gbm",
#                 trControl = myTimeControl,
#                 tuneLength=tuneLength.num,
#                 verbose=FALSE)
# 
# svm.mod <- train(Gas ~ .,
#                  data = gasdata2,
#                  method = "svmRadial",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# nnet.mod <- train(Gas ~ .,
#                  data = gasdata2,
#                  method = "nnet",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# # xgb.mod <- train(Gas ~ .,
# #                   data = gasdata2,
# #                   method = "xgbTree",
# #                   trControl = myTimeControl,
# #                   tuneLength=tuneLength.num,
# #                   verbose=FALSE)
# 
# ### model performance ###
# 
# resamps <- resamples(list(logit = logit.mod,
#                           rf = rf.mod,
#                           gbm = gbm.mod,
#                           svm = svm.mod,
#                           nnet = nnet.mod))
# 
# ss1 <- summary(resamps)
# 
# knitr::kable(ss1[[3]]$Accuracy)
# 
# ### Diesel classification model
# registerDoParallel(cores=2)
# 
# logit.mod <- train(Diesel ~ .,
#                    data = dieseldata2,
#                    method = "glm",
#                    family = binomial,
#                    trControl = myTimeControl,
#                    tuneLength=tuneLength.num)
# 
# rf.mod <- train(Diesel ~ .,
#                 data = dieseldata2,
#                 method = "rf",
#                 trControl = myTimeControl,
#                 tuneLength=tuneLength.num)
# 
# gbm.mod <- train(Diesel ~ .,
#                  data = dieseldata2,
#                  method = "gbm",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# svm.mod <- train(Diesel ~ .,
#                  data = dieseldata2,
#                  method = "svmRadial",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# nnet.mod <- train(Diesel ~ .,
#                   data = dieseldata2,
#                   method = "nnet",
#                   trControl = myTimeControl,
#                   tuneLength=tuneLength.num,
#                   verbose=FALSE)
# 
# # xgb.mod <- train(Diesel ~ .,
# #                  data = dieseldata2,
# #                  method = "xgbTree",
# #                  trControl = myTimeControl,
# #                  tuneLength=tuneLength.num,
# #                  verbose=FALSE)
# 
# ### model performance ###
# 
# resamps2 <- resamples(list(logit = logit.mod,
#                            rf = rf.mod,
#                           gbm = gbm.mod,
#                           svm = svm.mod,
#                           nnet = nnet.mod))
# 
# ss2 <- summary(resamps2)
# 
# knitr::kable(ss2[[3]]$Accuracy)
# 
# ### Gasoline classification model w/o retail
# registerDoParallel(cores=2)
# 
# gasdata3 <- gasdata2[,!names(gasdata2) %in% retailvar]
# 
# logit.mod <- train(Gas ~ .,
#                    data = gasdata3,
#                    method = "glm",
#                    family = binomial,
#                    trControl = myTimeControl,
#                    tuneLength=tuneLength.num)
# 
# rf.mod <- train(Gas ~ .,
#                 data = gasdata3,
#                 method = "rf",
#                 trControl = myTimeControl,
#                 tuneLength=tuneLength.num)
# 
# gbm.mod <- train(Gas ~ .,
#                  data = gasdata3,
#                  method = "gbm",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# svm.mod <- train(Gas ~ .,
#                  data = gasdata3,
#                  method = "svmRadial",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# nnet.mod <- train(Gas ~ .,
#                   data = gasdata3,
#                   method = "nnet",
#                   trControl = myTimeControl,
#                   tuneLength=tuneLength.num,
#                   verbose=FALSE)
# 
# ### model performance ###
# 
# resamps3 <- resamples(list(logit = logit.mod,
#                            rf = rf.mod,
#                           gbm = gbm.mod,
#                           svm = svm.mod,
#                           nnet = nnet.mod))
# 
# ss3 <- summary(resamps3)
# 
# knitr::kable(ss3[[3]]$Accuracy)
# 
# ### Diesel classification model
# registerDoParallel(cores=2)
# dieseldata3 <- dieseldata2[,!names(dieseldata2) %in% retailvar]
# 
# logit.mod <- train(Diesel ~ .,
#                    data = dieseldata3,
#                    method = "glm",
#                    family = binomial,
#                    trControl = myTimeControl,
#                    tuneLength=tuneLength.num)
# 
# rf.mod <- train(Diesel ~ .,
#                 data = dieseldata3,
#                 method = "rf",
#                 trControl = myTimeControl,
#                 tuneLength=tuneLength.num)
# 
# gbm.mod <- train(Diesel ~ .,
#                  data = dieseldata3,
#                  method = "gbm",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# svm.mod <- train(Diesel ~ .,
#                  data = dieseldata3,
#                  method = "svmRadial",
#                  trControl = myTimeControl,
#                  tuneLength=tuneLength.num,
#                  verbose=FALSE)
# 
# nnet.mod <- train(Diesel ~ .,
#                   data = dieseldata3,
#                   method = "nnet",
#                   trControl = myTimeControl,
#                   tuneLength=tuneLength.num,
#                   verbose=FALSE)
# 
# ### model performance ###
# 
# resamps4 <- resamples(list(logit = logit.mod,
#                            rf = rf.mod,
#                            gbm = gbm.mod,
#                            svm = svm.mod,
#                            nnet = nnet.mod))
# 
# ss4 <- summary(resamps4)
# 
# knitr::kable(ss4[[3]]$Accuracy)

### Gasoline regression model w/ retail + commercial
gbm.mod1 <- train(Gas ~ .,
                 data = gasdata %>% filter(!is.na(Motor_Gasoline)),
                 method = "gbm",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

min(gbm.mod1$results$RMSE)
summary(gbm.mod1$finalModel)[1:10,]

p12 <- plot_ly(cbind(allin1, pred=predict(gbm.mod1, gasdata))) %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA actual Gasoline', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~gas_Mean, name = 'Bloomberg Gasoline', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~pred, name = 'Regression fit', type='scatter', mode = 'lines') %>%
  layout(title = "Gasoline observation versus Prediction from retail+commercial", xaxis=list(title="Week"), yaxis=list(title="Weekly change"))

p12

### Gasoline regression model w/ retail only
gbm.mod2 <- train(Gas ~ .,
                 data = gasdata %>% select(-c(Motor_Gasoline, Gas_Oil)),
                 method = "gbm",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

min(gbm.mod2$results$RMSE)
summary(gbm.mod2$finalModel)[1:10,]

rf.mod2 <- train(Gas ~ .,
                  data = gasdata %>% select(-c(Motor_Gasoline, Gas_Oil)),
                  method = "rf",
                  trControl = myTimeControl,
                  tuneLength=tuneLength.num,
                  verbose=FALSE)

varImpPlot(rf.mod2$finalModel)

p13 <- plot_ly(cbind(allin1, 
                     gbm = predict(gbm.mod2, gasdata), 
                     rf = predict(rf.mod2, gasdata))) %>%
  add_trace(x = ~week, y = ~Gas, name = 'EIA actual Gasoline', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~gas_Mean, name = 'Bloomberg Gasoline', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~gbm, name = 'GBM model', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~rf, name = 'RF model', type='scatter', mode = 'lines') %>%
  layout(title = "Gasoline observation versus Prediction from retail", xaxis=list(title="Week"), yaxis=list(title="Weekly change"))

p13


### Diesel regression model w/ retail + commercial
gbm.mod3 <- train(Diesel ~ .,
                 data = dieseldata %>% filter(!is.na(Motor_Gasoline)),
                 method = "gbm",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

min(gbm.mod3$results$RMSE)
summary(gbm.mod3$finalModel)[1:10,]

p14 <- plot_ly(cbind(allin1, pred=predict(gbm.mod3, dieseldata))) %>%
  add_trace(x = ~week, y = ~Diesel, name = 'EIA actual Diesel', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~diesel_Mean, name = 'Bloomberg Diesel', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~pred, name = 'Regression fit', type='scatter', mode = 'lines') %>%
  layout(title = "Diesel observation versus Prediction from retail+commercial", xaxis=list(title="Week"), yaxis=list(title="Weekly change"))

p14

### Diesel regression model w/ retail
gbm.mod4 <- train(Diesel ~ .,
                 data = dieseldata %>% select(-c(Motor_Gasoline, Gas_Oil)),
                 method = "gbm",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)

min(gbm.mod4$results$RMSE)
summary(gbm.mod4$finalModel)[1:10,]

rf.mod4 <- train(Diesel ~ .,
                 data = dieseldata %>% select(-c(Motor_Gasoline, Gas_Oil)),
                 method = "rf",
                 trControl = myTimeControl,
                 tuneLength=tuneLength.num,
                 verbose=FALSE)


varImpPlot(rf.mod4$finalModel)

p15 <- plot_ly(cbind(allin1, 
                     gbm = predict(gbm.mod4, dieseldata),
                     rf = predict(rf.mod4, dieseldata))) %>%
  add_trace(x = ~week, y = ~Diesel, name = 'EIA actual Diesel', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~diesel_Mean, name = 'Bloomberg Diesel', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~gbm, name = 'GBM model', type='scatter', mode = 'lines') %>%
  add_trace(x = ~week, y = ~rf, name = 'RF model', type='scatter', mode = 'lines') %>%
  layout(title = "Diesel observation versus Prediction from retail", xaxis=list(title="Week"), yaxis=list(title="Weekly change"))

p15

#rmse between EIA and Bloomberg
with(allin1, sqrt(mean((Gas-gas_Mean)^2)))
with(allin1, sqrt(mean((Diesel-diesel_Mean)^2)))

#mae between EIA and Bloomberg
with(allin1, mean(abs(Gas-gas_Mean)))
with(allin1, mean(abs(Diesel-diesel_Mean)))

#mdae between EIA and Bloomberg
with(allin1, median(abs(Gas-gas_Mean)))
with(allin1, median(abs(Diesel-diesel_Mean)))

#residuals

############Gasoline residuals

p16 <- plot_ly(cbind(allin1, pred=predict(gbm.mod1, gasdata))) %>%
  add_trace(x = ~week, y = ~Gas-gas_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~week, y = ~Gas-pred, name = 'Shell retail+comm residual', type='scatter', mode = 'markers') %>%
  layout(title = "Gasoline residuals", xaxis=list(title="Week"), yaxis=list(title="Residual"))

p16

p17 <- plot_ly(cbind(allin1, pred=predict(gbm.mod1, gasdata))) %>%
  add_trace(x = ~weekly_regular, y = ~Gas-gas_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~weekly_regular, y = ~Gas-pred, name = 'Shell retail+comm residual', type='scatter', mode = 'markers') %>%
  layout(title = "Gasoline residuals", xaxis=list(title="Shell weekly regular"), yaxis=list(title="Residual"))

p17

p18 <- plot_ly(cbind(allin1, pred=predict(gbm.mod1, gasdata))) %>%
  add_trace(x = ~Gas, y = ~gas_Mean, name = 'Bloomberg estimate', type='scatter', mode = 'markers') %>%
  add_trace(x = ~Gas, y = ~pred, name = 'Shell retail+comm prediction', type='scatter', mode = 'markers') %>%
  layout(title = "Gasoline actual versus model", 
         xaxis=list(title="EIA actual change", 
                    range = c(-5500, 11000)), 
         yaxis=list(title="Model estimate", 
                    range = c(-5500, 11000)))

p18

p19 <- plot_ly(cbind(allin1, pred=predict(gbm.mod2, gasdata))) %>%
  add_trace(x = ~week, y = ~Gas-gas_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~week, y = ~Gas-pred, name = 'Shell retail residual', type='scatter', mode = 'markers') %>%
  layout(title = "Gasoline residuals", xaxis=list(title="Week"), yaxis=list(title="Residual"))

p19

p20 <- plot_ly(cbind(allin1, pred=predict(gbm.mod2, gasdata))) %>%
  add_trace(x = ~weekly_regular, y = ~Gas-gas_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~weekly_regular, y = ~Gas-pred, name = 'Shell retail residual', type='scatter', mode = 'markers') %>%
  layout(title = "Gasoline residuals", xaxis=list(title="Shell weekly regular"), yaxis=list(title="Residual"))

p20

p21 <- plot_ly(cbind(allin1, pred=predict(gbm.mod2, gasdata))) %>%
  add_trace(x = ~Gas, y = ~gas_Mean, name = 'Bloomberg estimate', type='scatter', mode = 'markers') %>%
  add_trace(x = ~Gas, y = ~pred, name = 'Shell retail prediction', type='scatter', mode = 'markers') %>%
  layout(title = "Gasoline actual versus model", 
         xaxis=list(title="EIA actual change", 
                    range = c(-5500, 11500)), 
         yaxis=list(title="Model estimate", 
                    range = c(-5500, 11500)))

p21

############Diesel residuals

p22 <- plot_ly(cbind(allin1, pred=predict(gbm.mod3, dieseldata))) %>%
  add_trace(x = ~week, y = ~Diesel-diesel_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~week, y = ~Diesel-pred, name = 'Shell retail+comm residual', type='scatter', mode = 'markers') %>%
  layout(title = "Diesel residuals", xaxis=list(title="Week"), yaxis=list(title="Residual"))

p22

p23 <- plot_ly(cbind(allin1, pred=predict(gbm.mod3, dieseldata))) %>%
  add_trace(x = ~weekly_diesel, y = ~Diesel-diesel_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~weekly_diesel, y = ~Diesel-pred, name = 'Shell retail+comm residual', type='scatter', mode = 'markers') %>%
  layout(title = "Diesel residuals", xaxis=list(title="Shell weekly diesel"), yaxis=list(title="Residual"))

p23

p24 <- plot_ly(cbind(allin1, pred=predict(gbm.mod3, dieseldata))) %>%
  add_trace(x = ~Diesel, y = ~diesel_Mean, name = 'Bloomberg estimate', type='scatter', mode = 'markers') %>%
  add_trace(x = ~Diesel, y = ~pred, name = 'Shell retail+comm prediction', type='scatter', mode = 'markers') %>%
  layout(title = "Diesel actual versus model", 
         xaxis=list(title="EIA actual change", 
                    range = c(-5500, 11500)), 
         yaxis=list(title="Model estimate", 
                    range = c(-5500, 11500)))

p24

p25 <- plot_ly(cbind(allin1, pred=predict(gbm.mod4, dieseldata))) %>%
  add_trace(x = ~week, y = ~Diesel-diesel_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~week, y = ~Diesel-pred, name = 'Shell retail residual', type='scatter', mode = 'markers') %>%
  layout(title = "Diesel residuals", xaxis=list(title="Week"), yaxis=list(title="Residual"))

p25

p26 <- plot_ly(cbind(allin1, pred=predict(gbm.mod4, dieseldata))) %>%
  add_trace(x = ~weekly_diesel, y = ~Diesel-diesel_Mean, name = 'Bloomberg residual', type='scatter', mode = 'markers') %>%
  add_trace(x = ~weekly_diesel, y = ~Diesel-pred, name = 'Shell retail residual', type='scatter', mode = 'markers') %>%
  layout(title = "Diesel residuals", xaxis=list(title="Shell weekly diesel"), yaxis=list(title="Residual"))

p26

p27 <- plot_ly(cbind(allin1, pred=predict(gbm.mod4, dieseldata))) %>%
  add_trace(x = ~Diesel, y = ~diesel_Mean, name = 'Bloomberg estimate', type='scatter', mode = 'markers') %>%
  add_trace(x = ~Diesel, y = ~pred, name = 'Shell retail prediction', type='scatter', mode = 'markers') %>%
  layout(title = "Diesel actual versus model", 
         xaxis=list(title="EIA actual change", 
                    range = c(-5500, 11500)), 
         yaxis=list(title="Model estimate", 
                    range = c(-5500, 11500)))

p27

### time series with covariates from shell retail

pct   <- 0.1
nsize <- dim(allin1)[1]
fit1  <- auto.arima(allin1$Gas[1:(nsize*(1-pct))], 
                    xreg = allin1[,3:6][1:(nsize*(1-pct)),])
plot(forecast(fit1, xreg = allin1[,3:6][(nsize*(1-pct)+1):nsize,]))
lines(allin1$week[(nsize*(1-pct)+1):nsize], allin1$Gas[(nsize*(1-pct)+1):nsize], col='red')
lines(allin1$week[(nsize*(1-pct)+1):nsize], allin1$gas_Mean[(nsize*(1-pct)+1):nsize], col='green')
legend("topleft", 
       legend = c("Gasoline actual: train",
                  "Gasoline actual: test",
                  "pred from tm model w/ covariates",
                  "Bloomberg estimate"),
       lty = rep(1,4),
       col = c("black",
               "red",
               "blue",
               "green"))


fit2 <- auto.arima(allin1$Diesel[1:(nsize*(1-pct))], 
                   xreg = allin1[,3:6][1:(nsize*(1-pct)),])
plot(forecast(fit2, xreg = allin1[,3:6][(nsize*(1-pct)+1):nsize,]))
lines(allin1$week[(nsize*(1-pct)+1):nsize], allin1$Diesel[(nsize*(1-pct)+1):nsize], col='red')
lines(allin1$week[(nsize*(1-pct)+1):nsize], allin1$diesel_Mean[(nsize*(1-pct)+1):nsize], col='green')
legend("topleft", 
       legend = c("Diesel actual: train",
                  "Diesel actual: test",
                  "pred from tm model w/ covariates",
                  "Bloomberg estimate"),
       lty = rep(1,4),
       col = c("black",
               "red",
               "blue",
               "green"))


### generate shock around wk 164
allin1$shock[165]
allin1$Diesel[165]
allin1$diesel_Mean[165]
allin1$lastwkd[165]
EIAdt <- newtic3$Date[newtic3$lastwkd == allin1$lastwkd[165]]
newtic2 %>% filter(Date==EIAdt) %>% plot_ly() %>%
  add_trace(x = ~DateTime, y= ~ GHO_Price, name="Before/After EIA report", type = 'scatter', mode = 'markers') %>%
  add_trace(x = ~DateTime[6], y = ~allin1$Diesel[165], name = 'EIA Actual Diesel', type='scatter', mode = 'markers', yaxis = "y2") %>%
  add_trace(x = ~DateTime[6], y = ~allin1$diesel_Mean[165], name = 'Bloomberg', type='scatter', mode = 'markers', yaxis = "y2") %>%
  layout(title = "Tic view data around EIA release at week 164", yaxis=list(range=c(1.7, 1.9)), yaxis2 = ay)


df9 <- read.csv("Diesel_rf_x2_52wk_fixWin_FALSE_BBNPredY.csv", as.is = T)
ticdt <- newtic3$Date[newtic3$lastwkd %in% as.Date(df9$lastwkd)]
EIAdt <- which(allin1$Date %in% ticdt)


plot_list <- list()

for (i in 1:9) {

newtic2 %>% filter(Date==ticdt[i]) %>% plot_ly() %>%
  add_trace(x = ~DateTime, y= ~ GHO_Price, name="price", type = 'scatter', mode = 'markers') %>%
  add_trace(x = ~DateTime, y= ~ GHO_Volume, name="volume", type = 'scatter', mode = 'markers', yaxis = "y2") %>%
  add_trace(x = ~DateTime[6], y = ~allin1$Diesel[EIAdt[i]], name = 'EIA Actual Diesel', type='scatter', mode = 'markers', yaxis = "y2") %>%
  add_trace(x = ~DateTime[6], y = ~allin1$diesel_Mean[EIAdt[i]], name = 'Bloomberg', type='scatter', mode = 'markers', yaxis = "y2") %>%
  layout(title = "Tic view data around EIA release", yaxis2 = ay) -> plot_list[[i]]
}


misdf4 <- read.csv("Diesel_rf_x2_52wk_fixWin_FALSE_BBYPredN.csv", as.is = T)
ticdt <- newtic3$Date[newtic3$lastwkd %in% as.Date(misdf4$lastwkd)]
EIAdt <- which(allin1$Date %in% ticdt)


plot_list <- list()

for (i in 1:4) {
  
  newtic2 %>% filter(Date==ticdt[i]) %>% plot_ly() %>%
    add_trace(x = ~DateTime, y= ~ GHO_Price, name="price", type = 'scatter', mode = 'markers') %>%
    add_trace(x = ~DateTime, y= ~ GHO_Volume, name="volume", type = 'scatter', mode = 'markers', yaxis = "y2") %>%
    add_trace(x = ~DateTime[6], y = ~allin1$Diesel[EIAdt[i]], name = 'EIA Actual Diesel', type='scatter', mode = 'markers', yaxis = "y2") %>%
    add_trace(x = ~DateTime[6], y = ~allin1$diesel_Mean[EIAdt[i]], name = 'Bloomberg', type='scatter', mode = 'markers', yaxis = "y2") %>%
    layout(title = "Tic view data around EIA release", yaxis2 = ay) -> plot_list[[i]]
}

contrasts(unique(allin1$month))
contrasts(unique(allin1$wkofyr))
contrasts(unique(allin1$wkofmo))

### manual rolling windows for gbm modeling
registerDoParallel(cores=8)

dat <- gasdata %>% select(-c(Motor_Gasoline, Gas_Oil)) 

exid <- with(dat, which((weekly_regular+weekly_mid_grade+weekly_svpn+weekly_diesel) > quantile(weekly_regular+weekly_mid_grade+weekly_svpn+weekly_diesel, .95)))

exid <- exid[exid>52]

timeSlices <- createTimeSlices(1:nrow(dat), 
                               initialWindow = 52*1, 
                               horizon = 1, 
                               fixedWindow = FALSE)

trainSlices <- timeSlices[[1]]
testSlices  <- timeSlices[[2]]


myTimeControl <- trainControl(method = "none",
                              allowParallel = TRUE, 
                              seeds = seeds)

pred <- NULL
true <- NULL

for(i in 1:length(trainSlices)){
  mod <- train(Gas ~ .,
               data = dat[trainSlices[[i]],],
               method = "gbm",
               trControl = myTimeControl,
               verbose=FALSE,
               tuneGrid = data.frame(n.trees = 50,
                                     interaction.depth = 1,
                                     shrinkage = .1,
                                     n.minobsinnode = 10))
  
  pred <- c(pred, predict(mod, dat[testSlices[[i]],]))
  true <- c(true, dat$Gas[testSlices[[i]]])
}

sqrt(mean((pred-true)^2))
mean(abs(pred-true))
median(abs(pred-true))
acf(pred-true)

sqrt(mean((pred[exid-52]-true[exid-52])^2))
mean(abs(pred[exid-52]-true[exid-52]))
median(abs(pred[exid-52]-true[exid-52]))


out <- as.data.frame(cbind(BB=allin1$gas_Mean[-c(1:52)], Model=pred, EIA=true)) %>% mutate(
  BBD=(BB>lag(BB))==(EIA>lag(EIA)),
  ModelD=(Model>lag(Model))==(EIA>lag(EIA)),
  BBS=sign(BB)==sign(EIA),
  ModelS=sign(Model)==sign(EIA)
  ) 
with(out, table(BBD, ModelD))
with(out, table(BBS, ModelS))

with(out[exid-52,], table(BBD, ModelD))
with(out[exid-52,], table(BBS, ModelS))

dat <- gasdata %>% select(-c(Motor_Gasoline, Gas_Oil)) %>%
  mutate(bbgas = allin1$gas_Mean,
         bbdis = allin1$diesel_Mean)

pred <- NULL
true <- NULL

for(i in 1:length(trainSlices)){
  mod <- train(Gas ~ .,
               data = dat[trainSlices[[i]],],
               method = "gbm",
               trControl = myTimeControl,
               verbose=FALSE,
               tuneGrid = data.frame(n.trees = 50,
                                     interaction.depth = 1,
                                     shrinkage = .1,
                                     n.minobsinnode = 10))
  
  pred <- c(pred, predict(mod, dat[testSlices[[i]],]))
  true <- c(true, dat$Gas[testSlices[[i]]])
}

sqrt(mean((pred-true)^2))
mean(abs(pred-true))
median(abs(pred-true))
acf(pred-true)

sqrt(mean((dat$bbgas[exid]-dat$Gas[exid])^2))
mean(abs(dat$bbgas[exid]-dat$Gas[exid]))
median(abs(dat$bbgas[exid]-dat$Gas[exid]))
sqrt(mean((pred[exid-52]-true[exid-52])^2))
mean(abs(pred[exid-52]-true[exid-52]))
median(abs(pred[exid-52]-true[exid-52]))


out <- as.data.frame(cbind(BB=allin1$gas_Mean[-c(1:52)], Model=pred, EIA=true)) %>% mutate(
  BBD=(BB>lag(BB))==(EIA>lag(EIA)),
  ModelD=(Model>lag(Model))==(EIA>lag(EIA)),
  BBS=sign(BB)==sign(EIA),
  ModelS=sign(Model)==sign(EIA)
) 
with(out, table(BBD, ModelD))
with(out, table(BBS, ModelS))

with(out[exid-52,], table(BBD, ModelD))
with(out[exid-52,], table(BBS, ModelS))


### diesel
dat <- dieseldata %>% select(-c(Motor_Gasoline, Gas_Oil)) 

pred <- NULL
true <- NULL

for(i in 1:length(trainSlices)){
  mod <- train(Diesel ~ .,
               data = dat[trainSlices[[i]],],
               method = "gbm",
               trControl = myTimeControl,
               verbose=FALSE,
               tuneGrid = data.frame(n.trees = 100,
                                     interaction.depth = 2,
                                     shrinkage = .1,
                                     n.minobsinnode = 10))
  
  pred <- c(pred, predict(mod, dat[testSlices[[i]],]))
  true <- c(true, dat$Diesel[testSlices[[i]]])
}

sqrt(mean((pred-true)^2))
mean(abs(pred-true))
median(abs(pred-true))
acf(pred-true)

sqrt(mean((pred[exid-52]-true[exid-52])^2))
mean(abs(pred[exid-52]-true[exid-52]))
median(abs(pred[exid-52]-true[exid-52]))


out <- as.data.frame(cbind(BB=allin1$diesel_Mean[-c(1:52)], Model=pred, EIA=true)) %>% mutate(
  BBD=(BB>lag(BB))==(EIA>lag(EIA)),
  ModelD=(Model>lag(Model))==(EIA>lag(EIA)),
  BBS=sign(BB)==sign(EIA),
  ModelS=sign(Model)==sign(EIA)
) 
with(out, table(BBD, ModelD))
with(out, table(BBS, ModelS))

with(out[exid-52,], table(BBD, ModelD))
with(out[exid-52,], table(BBS, ModelS))


dat <- dieseldata %>% select(-c(Motor_Gasoline, Gas_Oil)) %>%
  mutate(bbgas = allin1$gas_Mean,
         bbdis = allin1$diesel_Mean)

pred <- NULL
true <- NULL

for(i in 1:length(trainSlices)){
  mod <- train(Diesel ~ .,
               data = dat[trainSlices[[i]],],
               method = "gbm",
               trControl = myTimeControl,
               verbose=FALSE,
               tuneGrid = data.frame(n.trees = 100,
                                     interaction.depth = 2,
                                     shrinkage = .1,
                                     n.minobsinnode = 10))
  
  pred <- c(pred, predict(mod, dat[testSlices[[i]],]))
  true <- c(true, dat$Diesel[testSlices[[i]]])
}

sqrt(mean((pred-true)^2))
mean(abs(pred-true))
median(abs(pred-true))
acf(pred-true)

sqrt(mean((dat$bbdis[exid]-dat$Diesel[exid])^2))
mean(abs(dat$bbdis[exid]-dat$Diesel[exid]))
median(abs(dat$bbdis[exid]-dat$Diesel[exid]))
sqrt(mean((pred[exid-52]-true[exid-52])^2))
mean(abs(pred[exid-52]-true[exid-52]))
median(abs(pred[exid-52]-true[exid-52]))

out <- as.data.frame(cbind(BB=allin1$diesel_Mean[-c(1:52)], Model=pred, EIA=true)) %>% mutate(
  BBD=(BB>lag(BB))==(EIA>lag(EIA)),
  ModelD=(Model>lag(Model))==(EIA>lag(EIA)),
  BBS=sign(BB)==sign(EIA),
  ModelS=sign(Model)==sign(EIA)
) 
with(out, table(BBD, ModelD))
with(out, table(BBS, ModelS))

with(out[exid-52,], table(BBD, ModelD))
with(out[exid-52,], table(BBS, ModelS))

