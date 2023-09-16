##The file is created to extract and analyse the peak values of 
# every time series in the dataset

library(stringr)
library(ggplot2)
library(plyr)
library(wesanderson)
library(reshape2)
library(scales)
library(lubridate)


singleDay = function(chosenBuildings, dataFile, swSize, segmentDates, single_date)
{
  
  data_segments=NULL
  segment_mean = NULL
  
  
  for (i in chosenBuildings)
  {
    
    sw=nonOverlap(as.numeric(dataFile[i,]),swSize)  
    sw_given_dates = sw[single_date,]
    
    data_segments = rbind (data_segments, cbind( "ts"=rep(i,nrow(sw_given_dates)), "segNo" = seq(1, nrow(sw_given_dates)), sw_given_dates ))
    
  }
  data_segments = data.frame(data_segments, stringsAsFactors = F, row.names = NULL)
  
  ###Split each time series
  split_on_ts = split(data_segments, data_segments$ts)
  

  peak_segments = unlist( lapply(split_on_ts, function (f1) 
  {
    f1= f1[, c(-1,-2)]
    which.max(unlist(data.frame(t(f1))))
    
  }) )
  
  hist(unlist(peak_segments))   ##analyse the hist to get the hrs
  
  peak.hrs = seq(37,45)    ##For london and ausgrid both   6pm to 10 pm is chosen
  buildng.peaks = chosenBuildings [which(unlist(peak_segments) %in% peak.hrs)]
  peakvals= NULL
  peakVals = data_segments[which(data_segments$ts %in% buildng.peaks),]
  
  peakVals= split(peakVals, peakVals$ts)
  
  
  peakVals.list = lapply(peakVals, function(ff)
  {
    ff =  ff[, -c(1,2)]
    unlist(data.frame(t(ff)))[peak.hrs]
  })

  id = as.numeric (names((peakVals.list)))
  peakVals.list = data.frame(cbind("id" = id, do.call(rbind, peakVals.list), row.names= NULL), stringsAsFactors = F)
  
  melt.data = melt(peakVals.list, c("id"))
  melt.data = melt.data[order(melt.data$id),]
  melt.data$segNo = rep(seq(1, length(peak.hrs)), nrow(peakVals.list))
  melt.data$check = rep(0, nrow(melt.data))
  
  againSplit = split(melt.data, melt.data$id)
  
  againSplit = lapply(againSplit, function(ff)
  {
    ff$check[which.max(ff$value)] = 1
    ff
  })
  melt.data = do.call(rbind, againSplit)
  
  melt.data$id = as.numeric(factor(melt.data$id))
  
  dateTime = rep(unlist(data.frame( t(segmentDates[single_date,])))[peak.hrs],  nrow(peakVals.list))
  melt.data$dateTime =  as.POSIXct(dateTime)
  
  lims <- as.POSIXct( seq(from = melt.data$dateTime[1], to=  melt.data$dateTime[nrow(melt.data)], by = "1 hours"))  ##London

  ##For London
  
  pp= ggplot(melt.data, aes(x = dateTime, y = id)) +theme_bw()+ ylab("Building") + xlab(" ")+
    geom_tile(  aes( fill = value, color= factor(check), height=1 , size= factor(check))) + scale_size_manual(guide= F, values=c(1,2))+
    scale_x_datetime( breaks = lims, labels=date_format("%d-%m \n %H:%M", tz = tz(melt.data$dateTime)) , expand = c(0,0) )+
    scale_fill_gradientn(colours =wes_palette(n=3, name="Rushmore"),  breaks = c(seq(min(melt.data$value), max(melt.data$value), 0.2)), 
                         labels = c("0.01", "0.2", "0.4","0.6","0.8", "1")) +
    scale_color_manual(values=c ("white", "red"), guide =F) +
    theme(axis.text =  element_text(color= "black", size=12), panel.grid.major = element_blank(),
          axis.title = element_text(color="black", size=15),
          panel.border = element_rect(color="black", size=0.3), plot.title=element_text(hjust=0.5, vjust=0.5, color= "black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color="black", size=12), legend.key.width = unit(2.5, "cm"))+ 
    scale_y_continuous(breaks =  unique(melt.data$id), labels= c("#28",  "#50",  "#75",  "#86", "#111", "#120", "#136", "#137", "#179", "#181"))
  
  
  ### For Ausgrid
  
 pp= ggplot(melt.data, aes(x = dateTime, y = id)) +theme_bw()+ ylab("Building") + xlab(" ")+
    geom_tile(  aes( fill = value, color= factor(check), height=1 , size= factor(check))) + scale_size_manual(guide= F, values=c(1,2))+
    scale_x_datetime( breaks = lims, labels=date_format("%d-%m \n %H:%M", tz = tz(melt.data$dateTime)) , expand = c(0,0) )+
    scale_fill_gradientn(colours =wes_palette(n=3, name="Rushmore"),  breaks = c(seq(min(melt.data$value), max(melt.data$value), 0.3)), 
                         labels = c("0.01", "0.3", "0.6","0.9","1.2", "1.5")) +
    scale_color_manual(values=c ("white", "red"), guide =F) +
    theme(axis.text =  element_text(color= "black", size=12), panel.grid.major = element_blank(),
          axis.title = element_text(color="black", size=15),
          panel.border = element_rect(color="black", size=0.3), plot.title=element_text(hjust=0.5, vjust=0.5, color= "black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color="black", size=12), legend.key.width = unit(2.5, "cm"))+ 
    scale_y_continuous(breaks =  unique(melt.data$id), labels= c("#8",  "#14",  "#49",  "#86", "#87", "#116", "#139", "#154", "#244", "#258", "#277","#282"))
   
 
 return (pp)
  
} 

peakDetection = function(dataFile,swSize, clusterLabels, sym, date_seq, chosenBuildings, multiple_dates, single_date)
{
  ### This function aims to chose some random dates, divide the TS data into given segment size and then find the peak occurrence
  ### in the chosen days. 
  
  segmentDates = nonOverlap(as.character(unlist(date_seq)),swSize )
  peakTime=NULL
  segment_mean = NULL

  pplot = singleDay(chosenBuildings, dataFile, swSize, segmentDates, single_date)
  
 
  return (pplot)
  
}


