##The file is created to extract and analyse the peak values of the chosen buildingsin the dataset. The base file is "dr_application".
### This file has been called in "dr_application."

library(stringr)
library(ggplot2)
library(plyr)
library(tidyr)
library(wesanderson)
library(reshape2)
library(scales)

library(RColorBrewer)
plot_ts =  function(dataFile, chosenBuildings, clusterLabels, sym, swSize)
{
  
  
  dateTime = unlist (data.frame(t(segmentedDates)))
  first = t(dataFile[chosenBuildings[1], 4:ncol(dataFile)])
  first = data.frame(cbind(seq(1, nrow(first)), first), row.names= NULL, stringsAsFactors = F)
  colnames(first) = c("hr", "value")
  first$dateTime = as.POSIXct(dateTime[1:nrow(first)])
  
  #first = first[which(as.numeric(format(as.POSIXct(first$dateTime), "%d")) %in% seq(1,10)),]
  first= first[1:480, ]  # first 10 days 
  
  second = t(dataFile[chosenBuildings[2], 4:ncol(dataFile)])
  second = data.frame(cbind(seq(1, nrow(second)), second), row.names= NULL, stringsAsFactors = F)
  colnames(second) = c("hr", "value")
  second$dateTime = as.POSIXct(dateTime[1:nrow(second)])
  
  second =  second[1489:1968,]  #first 10 days in August is taken  in Ausgrid
  second = second[2929:3408,]   ##first 10 days of may is taken in London dataset. 
  
  limsSecond <- as.POSIXct( seq(from = second$dateTime[1], to=  second$dateTime[nrow(second)],by ="24 hours"))  
  
  limsFirst <- as.POSIXct( seq(from = first$dateTime[1], to=  first$dateTime[nrow(first)], by ="24 hours"))  
  
  
  ## saved as width= 1800 height= 350
  
  xIntercept =  seq(1, nrow(first), swSize*(48/swSize))
  
  p1 = ggplot(data= first)+ geom_line(aes(x= dateTime, y= value),  color= "blue") + theme_bw() + xlab("Time (in days)") +  ylab("Load values") + 
    scale_x_datetime( breaks = limsFirst, labels=date_format("%d ", tz = tz(first$dateTime)),
                      expand = c(0, 0)) + 
    theme( plot.title=element_text(hjust=0.5, vjust=0.5, color= "red", size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank() ,
           panel.border = element_rect(colour = "black", size= 1),
           axis.text = element_text(color = "black", size= 10), axis.title = element_text(color= "black", size=12))+
    ggtitle(paste0("Building ","#", chosenBuildings[1], ", least stable")) + 
    geom_vline(xintercept=as.numeric(first$dateTime[xIntercept]), linetype="dotted")
  
  
  p2 = ggplot(data= second)+ geom_line(aes(x= dateTime, y= value),  color= "blue") + theme_bw() +  xlab("Time (in days)") +  ylab("Load values") + 
    scale_x_datetime( breaks = limsSecond, labels=date_format("%d", tz = tz(second$dateTime)),
                      expand = c(0, 0)) + 
    theme( plot.title=element_text(hjust=0.5, vjust=0.5, color= "red", size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , 
           panel.border = element_rect(colour = "black", size= 1),
           axis.text = element_text(color = "black", size= 10), axis.title = element_text( color="black", size= 12)) + 
    ggtitle(paste0("Building ","#", chosenBuildings[2], ", most stable"))+ 
    geom_vline(xintercept=as.numeric(second$dateTime[xIntercept]), linetype="dotted")
  
  plot_list= NULL
  plot_list[[1]]=p1
  plot_list[[2]]=p2
  
  
  ###  save as width=1000 and height = 400
  grid.arrange(grobs= plot_list, nrow=2)
  
  
  ####********************ANOTHER UNSUCCESSFUL METHOD FOR PLOT --NOT TO BE DELETED*************************************************************
  ##########################################################################################################################
  # 
  # for (j in chosenBuildings)
  # {
  #   data_segments =  NULL
  #   sw=nonOverlap(as.numeric(dataFile[j,]),swSize)   ##tHE SOURCE FILE FOR nonOverlap function is in 'ts_to_symbols_old.R'. 
  #   ##Save it as source file for the nonOverlap function to be accessed.
  #   
  #   sym.vec = data.frame(t(sym[j,]))
  #   colnames(sym.vec)="vertex"
  #   ll = join(sym.vec, clusterLabels)
  #   
  #   data_segments = rbind (data_segments, cbind("ts"=rep(j,nrow(sw)) , sw , "segNo" = seq(1:nrow(sw)), "labels" =ll$labels  ))
  #   
  #   data_segments =  data.frame(data_segments, stringsAsFactors = F)
  #   
  #   copii= data.frame(data_segments[,-1], stringsAsFactors = F)
  #   xx= melt(copii[1:10,], c("segNo", "labels"))
  #   xx = data.frame(xx[order(xx$segNo, xx$labels),], row.names= NULL)
  #   xx$id= rep(seq(1, swSize), max(xx$segNo))
  #   ggplot(data= xx) + geom_line(aes(x=id, y=value, color= factor(labels),group = factor(segNo)))
  # }
  
  
  
  
}


plotAll = function(chosenBuildings, clusterLabels, sym, segmentedDates)
{
  
  my_colors = rep(c( "deeppink", "yellow",brewer.pal(name="Paired", n=12), brewer.pal(name="Dark2", n = 8), brewer.pal(name='Accent', n=8),
                     brewer.pal(name="Set1", n = 8), brewer.pal(name="Set2", n = 5), brewer.pal(name="Set3", n = 8) ), 100) 
  
  
  plot_list =  list()
  ts_labelled = NULL
  
  for (j in chosenBuildings)
  {
    sym.vec = data.frame(t(sym[j,]))
    colnames(sym.vec)="vertex"
    ll = join(sym.vec, clusterLabels)
    
    
    ts_labelled = rbind (ts_labelled, ll$labels)
    
  }
  
  
  temp = matrix(ts_labelled, ncol = ncol(ts_labelled))
  #temp = melt(temp[,1:50])
  temp = melt(temp)
  colnames(temp) = c("ts", "seg","clust")
  dateTime =  segmentedDates[, 1]
  dateTime = rep( dateTime, each = length(chosenBuildings) )
  
  plot_data = cbind(temp, "dateTime" = dateTime)
  
  plot_data$dateTime = as.POSIXct(plot_data$dateTime)
  
  ### create a named vector to assign colors that will be fixed for the cluster labels. This is important
  color_labels = my_colors[1:  max(clusterLabels$labels)]
  names(color_labels) = seq(1, length(color_labels))
  
  if (data_nm == "London")
  {
    lims <- as.POSIXct( seq(from = plot_data$dateTime[1], to=  plot_data$dateTime[nrow(plot_data)],by ="10 day"))  ##London
    
    ### P.S plot saved as eps width= 1300, height=300 this shuld be same in ausgrid
    pp = ggplot(plot_data, aes(x = dateTime, y = ts , fill=factor(clust))) +
      geom_tile( colour = "black", size = 0.3, height=0.9)+ 
      scale_fill_manual(values = color_labels, breaks =  seq(1,15)) + theme_bw()+
      scale_x_datetime( breaks = lims, labels=date_format("%d-%m \n %H:%M", tz = tz(plot_data$dateTime)),
                        expand = c(0, 0)) +
      theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(colour = "black"), axis.text.y = element_text(size=12, color="black"), 
            axis.text.x = element_text(size=12, color="black"), axis.ticks.length=unit(.30, "cm"), axis.ticks.y = element_blank(), axis.title.y = element_blank(), 
            legend.position = "top" , legend.direction = "horizontal", legend.text = element_text(color="black", size=12),
            legend.title = element_text(color="black", size=11), 
            axis.title = element_text(color="black", size=12), strip.background = element_rect(fill = "grey70")) + 
      xlab("Date-time") + guides(fill = guide_legend(nrow = 1, title="Cluster labels")) + scale_y_continuous(breaks = c(1,2), labels= c("#2", "#76"))
    
  }
  else
  {
    
    subset_data= plot_data[which(plot_data$seg %in% seq(1,500)),]
    
    #lims <- as.POSIXct( strptime(c( plot_data$dateTime[1],  plot_data$dateTime[nrow(plot_data)]), format = "%Y-%m-%d %H:%M" )) 
    lims <- as.POSIXct( seq(from = subset_data$dateTime[1], to=  subset_data$dateTime[nrow(subset_data)] , by ="10 day"))  ##AUsgrid
    
    
    ### P.S plot saved as eps width =2100, height=1800. tis shuld be same in ausgrid
    pp = ggplot(subset_data, aes(x = dateTime, y = ts , fill=factor(clust))) +
      geom_tile( colour = "black", size = 0.3, height=0.9)+ 
      scale_fill_manual(values = color_labels, breaks =  seq(1,15)) + theme_bw()+
      scale_x_datetime( breaks = lims, labels=date_format("%d-%m \n %H:%M", tz = tz(subset_data$dateTime)),
                        expand = c(0, 0)) +
      theme(plot.title = element_text(hjust = 0.5), panel.border = element_rect(colour = "black"), axis.text.y = element_text(size=12, color="black"), 
            axis.text.x = element_text(size=12, color="black"), axis.ticks.length=unit(.30, "cm"), axis.ticks.y = element_blank(), axis.title.y = element_blank(), 
            legend.position = "top" , legend.direction = "horizontal", legend.text = element_text(color="black", size=12),
            legend.title = element_text(color="black", size=11), 
            axis.title = element_text(color="black", size=12), strip.background = element_rect(fill = "grey70")) + 
      xlab("Date-time") + guides(fill = guide_legend(nrow = 1, title="Cluster labels")) + scale_y_continuous(breaks = c(1,2), labels= c("#95", "#145"))
    
    # scale_x_datetime( limits =lims, breaks = date_breaks("3 day"), labels=date_format("%d \nH:%M", tz = tz(monthly_data$dateTime)),
    
  }
  plot_list[[i]] = pp
  
  
  
  plot_list
  
  
  
}


plotPeaks =  function(peakValues, allSegments, dates)
{

  data_for_plot <- allSegments %>%
    left_join(peakValues %>% transmute(ts, segNo, check = 1)) %>%
    replace_na(list(check = 0))
  
  data_for_plot =  data_for_plot[,-1]     ###remove the sym column. it is not required.
  data_for_plot =  data.frame(apply(data_for_plot, 2, function(f)  as.numeric(f)), stringsAsFactors = F, row.names = NULL)
  
  data_for_plot$ts = as.integer(as.factor(data_for_plot$ts))
  melt_data = melt(data_for_plot, c(1,2, ncol(data_for_plot), (ncol(data_for_plot)-1)))
  
  melt_data$time_id = as.numeric(factor(melt_data$variable))
  temp =  apply(melt_data,1, function(fun)
  {
    dates[as.numeric(fun[2]), as.numeric(fun[7])]
  })
  
  melt_data$dateTime = as.POSIXct(temp)
  #melt_data$dateTime = (temp)
  
  subset_data = melt_data[which(melt_data$ts %in% c(seq(1,10))),]
  
  borderColors = c('white', 'black')
  
  mySize = c(0.3,0.9)
  
  mySize = mySize[as.factor(subset_data$check)]  
  borderColors = borderColors[as.factor(subset_data$check)]
  
  my_colors = c("yellow", "green")
  lims <- as.POSIXct( seq(from = subset_data$dateTime[1], to=  subset_data$dateTime[nrow(subset_data)],by = "4 hours"))  ##London
  
  
  pp=  ggplot(subset_data, aes(x = dateTime, y =  reorder(factor(labels), ts), fill=factor(labels))) +theme_classic()+ xlab("Datetime") + ylab("Buildings")+
    geom_tile(  aes( color= factor(check), height=0.6 ), size=0.5)  +
    scale_x_datetime( breaks = lims, labels=date_format("%d \n %H:%M", tz = tz(subset_data$dateTime))  ) +
    theme_bw()+
    scale_fill_manual(values = my_colors) +
    scale_color_manual(values=c ("white", "red"), guide =F) +
    theme(axis.text =  element_text(color= "black", size=12), panel.grid.major = element_blank(),
          axis.title = element_text(color="black", size=15),
          panel.border = element_rect(color="black", size=0.3), plot.title=element_text(hjust=0.5, vjust=0.5, color= "black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(color="black", size=12), legend.key.width = unit(2.5, "cm"))+
    ggtitle(paste0( format(subset_data$dateTime[1], "%b %d  %H:%M"), " to ",  format(subset_data$dateTime[nrow(subset_data)], "%b %d - %H:%M")))

   pp
}






date_analysis=function(peak_segments, segmentDates)
{
  
  ###get the other details like, on which days the peak occurs and the date and time of their occurrence
  
  dates_of_peaks = cbind(peak_segments, segmentDates[as.numeric(peak_segments$segNo),1])
  dates_of_peaks$weeksdays =  weekdays(as.Date(dates_of_peaks[,3]))
  dates_of_peaks
}



demandResponse_plots = function(dataFile,swSize, clusterLabels, sym, date_seq, chosenBuildings, lowEntropy_buildings)
{
  ### This function aims to chose some andom dates, divide te TS data into given segment size and then find the peak occurrence
  ### in the chosen days. If the time and date of peak occurrence matches in majority of time series, then it can be reported in 
  #### the paper as an applivation of DSM
  
  
  segmentDates = nonOverlap(as.character(unlist(date_seq)),swSize )
  
  # sym=data.frame("vertex" = as.vector(t(sym)))
  #sym=join(sym, clusterLabels)
  data_segments=NULL
  segment_mean = NULL

  chosen_dates = seq(1,2)   ##chosen dates where the peaks will be computed. This can be changed. Infact check with different values.
  ### The condition is, for the chosen dates, find the maximum value na dcheck if majority of the values lies in same cluster label. if yes, then report in the paper.
  for (i in lowEntropy_buildings)
  {
    
    sw= nonOverlap(as.numeric(dataFile[i,]),swSize)  
    
    sw_given_dates = sw[chosen_dates,]
    
    ts_sym = as.character(unlist(sym[i,]))
    ts_sym = ts_sym[chosen_dates]
    data_segments = rbind (data_segments, cbind("sym"= ts_sym, "ts"=rep(i,nrow(sw_given_dates)), "segNo" = seq(1, nrow(sw_given_dates)), sw_given_dates ))
    
  }
  data_segments = data.frame(data_segments, stringsAsFactors = F, row.names = NULL)
  
  ### get the cluster labels of the segments
  temp = join(data.frame(vertex = (data_segments$sym)), clusterLabels)
  data_segments$labels = temp$labels
  
  
  ###Split each time series
  split_on_ts = split(data_segments, data_segments$ts)
  
  
  ##For each time series in the dataset, break it into segmnets of given size, obtain the mean of the segment and the segment 
  ## with highest mean is called as the peak.
  peak_segments = lapply(split_on_ts, function (f1) 
  {
    tt= apply(f1, 1, function(f2)
    {
      max(as.numeric(f2[-c(1,2,3,4, length(f2))])) 
      
    })
    which.max(tt)
  })
  
  peak_segments = data.frame(cbind(names(peak_segments), unlist(peak_segments)), row.names = NULL,stringsAsFactors = F)
  
  colnames(peak_segments) = c("ts","segNo")
  
  ##Obtain the details about the date and time of occurrence of the peaks
  #dates_of_peaks = date_analysis(peak_segments, segmentDates)
  
  ##Obtain the cluster labels, symbols, the values of respective segments, by merging peak_segments with data_segments. 
  ###"data_segments" contains all the details. The output "pp" is a datframe which will have the cluster labels, symbols, the real values, and other details
  ###  of the segments where the peakvalues occur for each TS.
  pp = join(peak_segments, data_segments)
  
  ##Now for each pp$label, find the pp$segments  which occurs max times.
  ##Example for a  cluster label is given below
  table(as.numeric(pp[which(pp$labels==2),]$segNo))
  
  print (table(pp$labels))
  
  # pp = plotPeaks(pp, data_segments, segmentDates[chosen_dates,])
  pp = plotAll ( chosenBuildings, clusterLabels, sym, segmentDates )
  ##plot_ts(dataFile, chosenBuildings, clusterLabels, sym, swSize)
  return (pp)
  
}


