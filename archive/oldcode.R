xmlName(xmltop)
xmlName(xmltop[[2]][[2]])
xmlSize(xmltop[['trk']][['trkseg']])


for (i in 1:xmlSize(xmltop[['trk']][['trkseg']])) {
  element<-xmltop[['trk']][['trkseg']][[i]]
  print(xmlName(element))
}

xmlAttrs(xmltop[['trk']][['trkseg']][[1]])["lat"]

xmltop[['trk']][['trkseg']][[1]][["ele"]]

xmlSApply(xmltop[['trk']][['trkseg']], function(element) { 
  attr<-xmlAttrs(element) 
  print(as.numeric(attr["lat"]))
  print(as.numeric(attr["lon"]))
  print(element[["ele"]])
  print(element[["time"]])
})

tracking_03_15<-ldply(xmlToList(xmltop[['trk']][['trkseg']]), function(x) {
  data.frame(x)
})


tracking_03_15<-data.frame("ele"=tracking_03_15$ele[seq(1, nrow(tracking_03_15), 2)], "time"=tracking_03_15$time[seq(1, nrow(tracking_03_15), 2)],"lat"=tracking_03_15$.attrs[seq(1, nrow(tracking_03_15), 2)], "lon"=tracking_03_15$.attrs[seq(2, nrow(tracking_03_15), 2)])

tracking_03_15$ele<-as.numeric(levels(tracking_03_15$ele))[tracking_03_15$ele]
tracking_03_15$lat<-as.numeric(levels(tracking_03_15$lat))[tracking_03_15$lat]
tracking_03_15$lon<-as.numeric(levels(tracking_03_15$lon))[tracking_03_15$lon]

plot(tracking_03_15$lon, tracking_03_15$lat)


########


#### MORLI ####

tracking_03_09<-readTrackingFile("data/LJWSIZUT_03_09.gpx")
tracking_03_10<-readTrackingFile("data/LJWSIZUT_03_10.gpx")
tracking_03_11<-readTrackingFile("data/LJWSIZUT_03_11.gpx")
tracking_03_12<-readTrackingFile("data/LJWSIZUT_03_12.gpx")
tracking_03_13<-readTrackingFile("data/LJWSIZUT_03_13.gpx")
tracking_03_14<-readTrackingFile("data/LJWSIZUT_03_14.gpx")
tracking_03_15<-readTrackingFile("data/LJWSIZUT_03_15.gpx")

plot(tracking_03_10[-444,]$lon, tracking_03_10[-444,]$lat, col=ifelse(abs(432-tracking_03_10[-444,]$ele) < 30, rgb(0, 0, 0, 0.3), rainbow(531, alpha=1)[abs(432-tracking_03_10[-444,]$ele)]), pch=19, cex=0.5)

nrow(tracking_03_15)


house<-list()
house$ele<-432
house$lon<-14.625828
house$lat<-48.320237

#001  14.625828  48.320237 # mitte
#002	14.625723	48.320276 # oben links
#003	14.625908	48.320281 # oben rechts
#004	14.625912	48.320167 # unten rechts
#005	14.625723	48.320169 # unten links

lines(x=c(14.625723, 14.625908), y=c(48.320276, 48.320281), col="blue")
lines(x=c(14.625908, 14.625912), y=c(48.320281, 48.320167), col="blue")
lines(x=c(14.625912, 14.625723), y=c(48.320167, 48.320169), col="blue")
lines(x=c(14.625723, 14.625723), y=c(48.320169, 48.320276), col="blue")

points(tracking_03_21$lon, tracking_03_21$lat, col=ifelse(abs(432-tracking_03_21$ele)<30, rgb(0, 0, 1, 0.3), topo.colors(531)[abs(432-tracking_03_21$ele)]), pch=19, cex=0.5)

tracking_03_10[which(tracking_03_10$lon==min(tracking_03_10$lon)),]

#### TEDDY ####

tracking_03_21<-readTrackingFile("data/LJWSIZUT_03_21.gpx")
tracking_03_22<-readTrackingFile("data/LJWSIZUT_03_22.gpx")
tracking_03_23<-readTrackingFile("data/LJWSIZUT_03_23.gpx")
tracking_03_24<-readTrackingFile("data/LJWSIZUT_03_24.gpx")
tracking_03_25<-readTrackingFile("data/LJWSIZUT_03_25.gpx")
tracking_03_26<-readTrackingFile("data/LJWSIZUT_03_26.gpx")
tracking_03_27<-readTrackingFile("data/LJWSIZUT_03_27.gpx")
tracking_03_28<-readTrackingFile("data/LJWSIZUT_03_28.gpx")
tracking_03_29<-readTrackingFile("data/LJWSIZUT_03_29.gpx")

#### MINERVA ####

#tracking_04_07<-readTrackingFile("data/LJWSIZUT_04_07.gpx")
tracking_04_08<-readTrackingFile("data/LJWSIZUT_04_08.gpx")
tracking_04_09<-readTrackingFile("data/LJWSIZUT_04_09.gpx")
tracking_04_10<-readTrackingFile("data/LJWSIZUT_04_10.gpx")
tracking_04_11<-readTrackingFile("data/LJWSIZUT_04_11.gpx")

#### MAYA ####

tracking_03_31<-readTrackingFile("data/LJWSIZUT_03_31.gpx")
tracking_04_01<-readTrackingFile("data/LJWSIZUT_04_01.gpx")
tracking_04_02<-readTrackingFile("data/LJWSIZUT_04_02.gpx")

#### interpolate it to 100 data points ####

interpolate<-function(tracking) {
  N<-100
  M<-60*24
  n<-nrow(tracking)
  
  track<-data.frame(stringsAsFactors = FALSE)
  for (t in seq(3, M+2, M/N)) {
    # assume order by minute
    before<-max(which(tracking$min <= t))
    after<-min(which(tracking$min >= t))
    
    if (before==after) {
      track<-rbind(track, c(tracking[before,]$ele, tracking[before,]$lat, tracking[before,]$lon, t))
    } else {
      interval<-tracking[after,]$min-tracking[before,]$min
      #print(interval)
      bt<-(t-tracking[before,]$min)/interval
      at<-(tracking[after,]$min-t)/interval
      
      ele<-(tracking[before,]$ele*bt+tracking[after,]$ele*at)/2
      lat<-(tracking[before,]$lat*bt+tracking[after,]$lat*at)/2
      lon<-(tracking[before,]$lon*bt+tracking[after,]$lon*at)/2
      
      #print(paste(tracking$min[before], t, tracking$min[after], bt, at, ele, lat, lon, interval))
      
      track<-rbind(track, c(ele, lat, lon, t))      
    }
  }
  
  names(track)<-c("ele", "lat", "lon", "min")
  return (track)
}

