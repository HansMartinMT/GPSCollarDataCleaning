
#function to install and load required packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#load or install these packages:
packages <- c("ks", "lattice", "plotrix", "adehabitatHR", "maptools", "foreign", "rgdal", "sp", "raster",
              "ggplot2","colorRamps","rgeos","leaflet","lubridate", "htmlwidgets","move", "tmap","grid", "leaflet","dplyr", "tidyr", "naniar")

#run function to install packages
ipak(packages)
##################
#GPS Collar Data#
#################
LotekData<-read.csv("C:/Users/Hans Martin/Desktop/2018LotekCollarData/LOTEKGPSDATA.csv", stringsAsFactors = F,header = T)
webserviceData<-read.csv("C:/Users/Hans Martin/Desktop/2018LotekCollarData/webserviceData/WebserviceDataGPS_102918_111436_new.csv", stringsAsFactors = F,header = T)

LotekData$webservice<-"collar"
webserviceData$webservice<-"web"
vars<-c(Device.Name="Animal_ID", Device.ID="Collar_ID",Date...Time..GMT. ="GMT.Time", Date...Time..Local.="LocalDateTime", Latitude="Latitude", Longitude="Longitude", Altitude="Altitude", Fix.Status= "Fix.Status",DOP="DOP", Temp..C.="Temperature",  Main..V.="Main..V.", Back..V.="Back..V.", webservice="webservice")
colnames(webserviceData)<-c("Animal_ID", "Collar_ID","GMT.Time", "LocalDateTime", "Latitude", "Longitude", "Altitude", "Fix.Status","DOP", "Temperature", "Main..V.", "Back..V.", "webservice")
webserviceData1<-webserviceData %>% select(-LocalDateTime,-Fix.Status,-Main..V.,-Back..V.)

LotekData1<-LotekData %>% select(-Satellites,-Duration)
LotekData1<-LotekData1 %>% select(Animal_ID,Collar_ID,GMT.Time,Latitude, Longitude ,Altitude,DOP, Temperature, webservice) 
head(webserviceData1)
# head(col100892)
# columnnames<-col100892[5,]
# col100892<-col100892[-c(1:5),]
# head(col100892)
head(LotekData)
#columnnames<-col100892[1,]
# colnames(col100892)<-c("GMT_Time", "Latitude", "Longitude", "Altitude", "Duration", "Temperature", "DOP", "Satellites")
# head(col100892)
LotekData2<-bind_rows(LotekData1,webserviceData1)
LotekData<-LotekData2
LotekData <- LotekData %>% mutate(GMT.TIME=trimws(GMT.Time, which=c("both"))) 

LotekData<-LotekData %>%  
  separate(GMT.TIME, into=c("GMT_Month","GMT_Day",  "GMT_Year","hour","Minute")) %>% 
  mutate(fix_date=paste(GMT_Month,
                        GMT_Day,
                        GMT_Year, 
                        sep="-"), 
         fix_time=paste(hour,
                        Minute,
                        "00", 
                        sep=":"), 
         fxtimestamp=paste(fix_date,fix_time,sep=" "))
unique(LotekData$GMT_Month)

LotekData$UTCTelemDate <- as.POSIXct(paste(LotekData$fix_date, LotekData$fix_time), format="%d-%m-%Y %H:%M:%S",TZ="UTC")

LotekData$GMT_Month<-as.numeric(as.character(LotekData$GMT_Month))
LotekData<-LotekData %>% 
  filter(GMT_Month %in% c("2","3","4","5","6","7","8")) %>%  
  replace_with_na(replace = list(Latitude = "0", 
                                 Longitude="0"))
unique(LotekData$GMT_Day)
unique(LotekData$GMT_Month)
LotekData %>% group_by(webservice,Animal_ID) %>% summarise(TotalFixes=sum(!is.na(Latitude)), attemptedfixes=n()) %>% gather(variable, value,-(webservice:Animal_ID)) %>% unite(temp, webservice, variable) %>% spread(temp, value) %>% mutate(collarfixrate=collar_TotalFixes/collar_attemptedfixes, webfixrate=web_TotalFixes/collar_attemptedfixes) %>% summarise(avrgcollarfixrate=mean(collarfixrate),avrgwebfixrate=mean(webfixrate))

LotekData %>% group_by(Animal_ID, webservice) %>% summarise(FixRate=sum(!is.na(Latitude))/n()) )
nrow(LotekData)
distinctRows<-LotekData %>% 
  #group_by(Animal_ID) %>% 
  distinct(Animal_ID, UTCTelemDate, webservice, .keep_all = T)

distinctRows%>% group_by(Animal_ID) %>%summarise(FixRate=sum(!is.na(Latitude))/n())

distinctRows %>% group_by(webservice,Animal_ID) %>% summarise(TotalFixes=sum(!is.na(Latitude)), attemptedfixes=n()) %>% gather(variable, value,-(webservice:Animal_ID)) %>% unite(temp, webservice, variable) %>% spread(temp, value) %>% mutate(collarfixrate=collar_TotalFixes/collar_attemptedfixes, webfixrate=web_TotalFixes/collar_attemptedfixes) %>% summarise(avrgcollarfixrate=mean(collarfixrate),avrgwebfixrate=mean(webfixrate))


 nrow(distinctRows)
 nrow(LotekData)
 DistincRowsCollarData<-distinctRows %>% filter(webservice=="collar")
 
puech<-as.ltraj(xy=DistincRowsCollarData[,c("Longitude","Latitude")], date=DistincRowsCollarData$UTCTelemDate,id=DistincRowsCollarData$Animal_ID, burst = DistincRowsCollarData$Animal_ID)

runsNAltraj(puech)
plot(puech)
