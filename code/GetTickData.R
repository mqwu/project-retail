setwd("/media/sf_project/Retail")

library(timeDate)
library(httr)
library(lubridate)
set_config(
  use_proxy(url="proxy-eu.shell.com", port=8080)
)



# Wed
EIAReportDates<- seq.Date(as.Date("2012-01-04"),as.Date("2017-04-26"),by=7)  # wed

# Mon-Tue
# EIAReportDatesMon<- seq.Date(as.Date("2012-01-02"),as.Date("2017-04-26"),by=7)  # Mon
# EIAReportDatesTue<- seq.Date(as.Date("2012-01-03"),as.Date("2017-04-26"),by=7)  # Tue
#EIAReportDates <- sort(c(EIAReportDatesMon, EIAReportDatesTue))

EIAReportDates[EIAReportDates %in% as.Date( holidayNYSE(2012:2017))] <- EIAReportDates[EIAReportDates %in% as.Date(holidayNYSE(2012:2017))]+1


for(i in 1:length(EIAReportDates)){
print(i)

#tempURL<-paste0("http://webservice.gvsi.com/gvsi/xml/getintraday?symbols=/GRB,/GHO&fields=symbol,date,close,volume&startdate=",format(EIAReportDates[i],"%m/%d/%Y"),"%209:25&enddate=",format(EIAReportDates[i],"%m/%d/%Y"),"%2010:30&barinterval=1&output=csv&includeheaders=true&timezone=publisher&username=shellapi4&password=shell")
#tempURL<-paste0("http://webservice.gvsi.com/gvsi/xml/getintraday?symbols=/GRB,/GHO&fields=symbol,date,close,volume&startdate=",format(EIAReportDates[i],"%m/%d/%Y"),"%210:25&enddate=",format(EIAReportDates[i],"%m/%d/%Y"),"%2011:30&barinterval=1&output=csv&includeheaders=true&timezone=publisher&username=shellapi4&password=shell")

tempURL<-paste0("http://webservice.gvsi.com/gvsi/xml/getintraday?symbols=/GRB,/GCL&fields=symbol,date,close,volume&startdate=",format(EIAReportDates[i],"%m/%d/%Y"),"%2010:25&enddate=",format(EIAReportDates[i],"%m/%d/%Y"),"%2011:30&barinterval=1&output=csv&includeheaders=true&timezone=publisher&username=shellapi4&password=shell")

tr<-GET(tempURL, authenticate("shellapi4", "shell"))
print(content(tr))

out<-data.frame(do.call("rbind",strsplit(unlist(strsplit(content(tr,"text"),"\r\n")),",")))


out<-out[c(-1,-nrow(out)),]
names(out)<-c("symbol","date","close","volume")
out$date<-gsub(" AM","",out$date)
out$date<-strptime(as.character(out$date),"%m/%d/%Y %H:%M:%S",tz="UTC")

#write.table(out,"GCL_Mon2Tue_12to17.csv",append=T,row.names=F,sep=",",col.names=FALSE)
write.table(out,"GCL_Wed_12to17.csv",append=T,row.names=F,sep=",",col.names=FALSE)

}

stop()



tempURL<-paste0("http://webservice.gvsi.com/gvsi/xml/getintraday?symbols=/GRB,/GHO&fields=symbol,date,close,volume&startdate=",format(EIAReportDates[i],"%m/%d/%Y"),"%209:25&enddate=",format(EIAReportDates[i],"%m/%d/%Y"),"%2010:30&barinterval=1&output=csv&includeheaders=true&timezone=publisher&username=shellapi4&password=shell")
tr<-GET(tempURL, authenticate("shellapi4", "shell"))
tr<-GET("http://webservice.gvsi.com/gvsi/xml/getintraday?symbols=/GRB,/GHO&fields=symbol,date,close,volume&startdate=04/26/2017%209:25&enddate=04/26/2017%2010:30&barinterval=1&output=csv&includeheaders=true&timezone=publisher&username=shellapi4&password=shell", authenticate("shellapi4", "shell"))
out<-data.frame(do.call("rbind",strsplit(unlist(strsplit(content(tr,"text"),"\r\n")),",")))
out<-out[c(-1,-nrow(out)),]
names(out)<-c("symbol","date","close","volume")

cat(content(tr,"text"),"\r\n",file="temp.csv")


#http://webservice.gvsi.com/gvsi/xml/getintraday?symbols=/GRB,/GHO&fields=symbol,date,close,volume&startdate=04/26/2017%209:25&enddate=04/26/2017%2010:30&barinterval=1&output=csv&includeheaders=true&timezone=publisher&username=shellapi4&password=shell