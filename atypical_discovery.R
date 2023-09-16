library(dplyr)
library(igraph)
library(TSclust)
library(clusterSim)
library(plyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)
library(grid)
library(gridExtra)


atypical_ausgrid = function(mainGraph, dataFiles, sym,clusterLabels, swSize, segmentDates, data_nm)
{
  sym= data.frame("vertex" = as.vector(t(sym)))
  sym= join(sym, clusterLabels)
  data_segments = NULL
  for (i in 1:nrow(dataFile))
  {
    sw=nonOverlap(as.numeric(dataFile[i,]),swSize) 
    data_segments = rbind (data_segments, cbind("ts"=rep(i,nrow(sw)),"dateTime" = segmentDates[,1], sw ))
    
  }
  
  data_segments=data.frame(cbind(sym,data_segments),stringsAsFactors = F)
  sort.deg = (sort(degree(mainGraph))) ## sort vertices on degree
  
  #vd= 8
  v.degree = names(sort.deg)[which(sort.deg %in% c(6,7,8))]
  my_colors = c("blue", "black", "red", "magenta", "chocolate4")
  data_segments$ts= as.numeric(as.character(data_segments$ts))
  plot_list = list()
  
  ano.info =NULL
  
  for (i in seq(1, length(v.degree))) 
  {
    pos = which(data_segments$vertex == v.degree[i])
    for (k in 1:length(pos))
    {
      tt = pos[k]
      
      data_tt = data_segments[which(data_segments$ts==data_segments$ts[tt]),]
      data_tt$anomaly= rep(1, nrow(data_tt))
      data_tt$anomaly[which(data_tt$vertex==v.degree[i])]= 0
      
      occur.at = which(data_tt$vertex==v.degree[i])
      
      for (j in 1:length(occur.at))
      {
        
        len= occur.at[j]
        if (len >= 5)
          seg.outlier =  data_tt[(len-10): (len+10),]
        
        else
          seg.outlier =  data_tt[1: (len+10),]
        
        dates = which(segmentDates[,1] %in% seg.outlier$dateTime)
        dates = segmentDates[dates,]
        dates = as.POSIXct(as.vector(t(dates)))
        
        buildingNo = as.numeric(as.character(unique(seg.outlier$ts)))
        ano.info = rbind(ano.info, cbind(buildingNo, as.character( seg.outlier$dateTime[which(seg.outlier$anomaly==0)] )  ))
        seg.outlier = data.frame(seg.outlier[,5:ncol(seg.outlier)], row.names = NULL, stringsAsFactors = F)
        seg.outlier$id = seq(1, nrow(seg.outlier))
        
        melt.data = melt(seg.outlier, c("id", "anomaly"))
        melt.data = data.frame(melt.data[order(melt.data$id, melt.data$anomaly, melt.data$variable),-3], row.names= NULL)
        melt.data$ts= seq(1, nrow(melt.data))
        melt.data$id= rep(1, nrow(melt.data))
        melt.data$dateTime = dates[1:nrow(melt.data)]
        
        melt.data$value = as.numeric(melt.data$value)
        
        
        lims <- as.POSIXct( seq(from = dates[1], to=  dates[length(dates)],by ="16 hours"))  ##London
        
        p =   ggplot(data = melt.data, aes(x = dateTime, y = value, color = factor(anomaly))) + geom_line(data =  melt.data, aes(group = factor(id)))+  theme_bw()+
          scale_x_datetime( breaks = lims, labels=date_format("%d- %m \n %H:%M", tz = tz(melt.data$dateTime)),
                            expand = c(0, 0))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.border = element_rect(color="black", size=0.5),
                legend.position = "bottom", legend.title = element_blank() , legend.text = element_text(color="black", size=10) , 
                axis.text = element_text(color="black", size=10), axis.title = element_blank(), 
                plot.title=element_text(hjust=0.5, vjust=0.5, color= "black", size=12, face="bold")) +
          scale_color_manual(values = c("red", "black"), labels = c("Unique", "Normal")) + ggtitle(paste0("#", buildingNo, " ", "\"", as.character(v.degree[i]), "\"" )) + 
          geom_vline(xintercept=as.numeric(melt.data$dateTime[seq(1, nrow(melt.data),swSize)]), linetype="dotted")
        
        plot_list [[length(plot_list) + 1]] = p           
      }
      
    }
    
    
  }
  plot_list
  
}



atypical_london = function(mainGraph, dataFiles, sym,clusterLabels, swSize, segmentDates, data_nm)
{
  sym=data.frame("vertex" = as.vector(t(sym)))
  sym=join(sym, clusterLabels)
  data_segments = NULL
  for (i in 1:nrow(dataFile))
  {
    
    sw=nonOverlap(as.numeric(dataFile[i,]),swSize)  ##tHE SOURCE FILE FOR nonOverlap function is in 'ts_to_symbols_old.R'. 
    ##Save it as source file for the nonOverlap function to be accessed.
    
    data_segments = rbind (data_segments, cbind("ts"=rep(i,nrow(sw)),"dateTime" = segmentDates[,1], sw ))
    
  }
  
  data_segments=data.frame(cbind(sym,data_segments),stringsAsFactors = F)
  sort.deg = (sort(degree(mainGraph))) ## sort vertices on degree
  
  #vd= 4
  v.degree = names(sort.deg)[which(sort.deg %in% c(2,3,4))]
  my_colors = c("blue", "black", "red", "magenta", "chocolate4")
  data_segments$ts= as.numeric(as.character(data_segments$ts))
  plot_list = list()
  ano.info= NULL
  for (i in seq(1, length(v.degree)))  ##For the first five lowest degrees
  {
    pos = which(data_segments$vertex == v.degree[i])
    
    for (k in 1:length(pos))
    {
      tt = pos[k]
      data_tt = data_segments[which(data_segments$ts==data_segments$ts[tt]),]
      data_tt$anomaly= rep(1, nrow(data_tt))
      data_tt$anomaly[which(data_tt$vertex==v.degree[i])]= 0
      occur.at = which(data_tt$vertex==v.degree[i])
      
      for (j in 1:length(occur.at))
        
      {
        
        len= occur.at[j]
        if (len >= 5)
          seg.outlier =  data_tt[(len-5): (len+5),]
        
        else
          seg.outlier =  data_tt[1: (len+5),]
        
        dates = which(segmentDates[,1] %in% seg.outlier$dateTime)
        dates = segmentDates[dates,]
        dates = as.POSIXct(as.vector(t(dates)))
        buildingNo = as.numeric(as.character(unique(seg.outlier$ts)))
        ano.info =  rbind(ano.info, cbind(buildingNo, as.character( seg.outlier$dateTime[which(seg.outlier$anomaly==0)] )))
        
        seg.outlier = data.frame(seg.outlier[,5:ncol(seg.outlier)], row.names = NULL, stringsAsFactors = F)
        seg.outlier$id = seq(1, nrow(seg.outlier))
        
        melt.data = melt(seg.outlier, c("id", "anomaly"))
        melt.data = data.frame(melt.data[order(melt.data$id, melt.data$anomaly, melt.data$variable),-3], row.names= NULL)
        melt.data$ts= seq(1, nrow(melt.data))
        melt.data$id= rep(1, nrow(melt.data))
        melt.data$dateTime = dates[1:nrow(melt.data)]
        melt.data$value = as.numeric(melt.data$value)
        
        lims <- as.POSIXct( seq(from = dates[1], to=  dates[length(dates)],by ="36 hours"))  ##London
        
        p =   ggplot(data = melt.data, aes(x = dateTime, y = value, color = factor(anomaly))) + geom_line(data =  melt.data, aes(group = factor(id)))+  theme_bw()+
          scale_x_datetime( breaks = lims, labels=date_format("%d- %m \n %H:%M", tz = tz(melt.data$dateTime)),
                            expand = c(0, 0))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.border = element_rect(color="black", size=0.5),
                legend.position = "bottom", legend.title = element_blank() , legend.text = element_text(color="black", size=10) ,
                axis.text = element_text(color="black", size=10), axis.title = element_blank(),
                plot.title=element_text(hjust=0.5, vjust=0.5, color= "black", size=12, face="bold")) +
          scale_color_manual(values = c("red", "black"), labels = c("Outlier", "Normal")) + 
          ggtitle(paste0("#", buildingNo, " ","\"", as.character(v.degree[i]), "\"" )) +
          geom_vline(xintercept=as.numeric(melt.data$dateTime[seq(1, nrow(melt.data),swSize)]), linetype="dotted")
        plot_list [[length(plot_list) + 1]] = p           
      }
      
    }
    
    
  }
  plot_list
}



edgeColors= function(mainGraph, clusterLabels, edgeCol)
{
  elist <- data.frame(get.edgelist(mainGraph), stringsAsFactors = F)
  colnames(elist) = c("from","to")
  elist$color  <- NA
  
  eColor = NULL
  
  for (i in 1:max(clusterLabels$labels))
  {
    vnames= as.character(clusterLabels[which(clusterLabels$labels==i),]$vertex)
    
    subg = subgraph(mainGraph, vnames )
    
    elist.subg = data.frame(get.edgelist(subg), stringsAsFactors = F)
    colnames(elist.subg) = c("from","to")
    
    apply(elist.subg,1, function(f)
    {
      elist$color[match(paste(f[1], f[2]), paste(elist$from, elist$to))] <<- edgeCol[i]
    })
    
  }
  
  #elist$color[is.na(elist$color)] = "grey70"     ##give colors to intercluster edges if required.
  
  
  elist$color
}



plot_graph =  function(mainGraph, clusterLabels)
{
  
  my_colors= c("antiquewhite3", "aquamarine3", "bisque3", "blue", "blueviolet", "brown4", "burlywood4", "cadetblue4", 
               "chartreuse4", "darkgoldenrod1", "darkolivegreen1", "darkorange1", "darksalmon", "deeppink", 
               "cyan",  "darkseagreen2", "gray50", "peachpuff", "yellow", "springgreen", "thistle", "khaki", "peru", "rosybrown", "tan",
               "maroon", "olivedrab1", "lightskyblue4", "red", "turquoise4", "black", "honeydew3", "indianred", "whitesmoke", "yellow4",  "lemonchiffon2" 
  )
  
  
  E(mainGraph)$arrow.size <- 0.9
  # Amplify or decrease the width of the edges
  edgeweights <- E(mainGraph)$weight * 4

  # Change shape of graph vertices
  V(mainGraph)$shape <- "sphere"
  
  # Change colour of vertex frames
  V(mainGraph)$vertex.frame.color <- "white"

  v.groups = unlist(lapply(seq(1, vcount(mainGraph)), function(ff) clusterLabels$labels[ which(V(mainGraph)$name[ff] == as.character(clusterLabels$vertex))] ))
  vcolor <- my_colors[v.groups]

  makeGroup =  split(clusterLabels, clusterLabels$labels)
  
  makeGroup = lapply(makeGroup, function(f1)
  {
    apply(f1, 1, function(f2) which(f2[1] == V(mainGraph)$name))
    
  })

  ##   Vertex size
  vSizes=8
  
  my_edgeColor = edgeColors(mainGraph, clusterLabels, my_colors)

  plot( mainGraph,
        layout=layout.lgl,
        edge.curved=TRUE,
        vertex.size=vSizes,
        vertex.color = vcolor,
        vertex.label= NA,
        asp=FALSE,
        edge.color = my_edgeColor,
        edge.arrow.mode=1,   edge.width=1.2,
        sub="", edge.arrow.size=0.3, mark.groups = makeGroup)
  p.plot= recordPlot()
  return (p.plot)
  
}



dSAX=function (x, y, alpha, n, brkPoint) 
{
  
  x=strsplit(x,"")[[1]][1:3]
  y=strsplit(y,"")[[1]][1:3]
  
  w <- length(x)
  myLetters<-letters[1:26]
  
  SAX.breakpoints.table <- function (n) {
    qnorm(0:n/n)
  }
  
  symb <- SAX.breakpoints.table(alpha)
  d <- 0
  
  x1<- match(x, myLetters)
  y1<-match(y, myLetters)
  
  for (i in 1:w) {
    xi <- x1[i]
    yi <- y1[i]
    
    if (abs(xi - yi) > 1 ) {
      d <- d + (symb[max(xi, yi)] - symb[min(xi, yi)+1])^2
    }
    
    else if (abs(xi-yi)==1 )
    {
      brkPoint.sub = brkPoint[which(brkPoint$feature==i),]
      d <- d + (brkPoint.sub$val[brkPoint.sub$alpha==max(xi,yi)] - brkPoint.sub$val[brkPoint.sub$alpha==min(xi,yi)])^2
    }
    
  }
  sqrt((n/w) * d)
}


graphConstruct=function(edge.list)
{
  mainGraph <- graph.data.frame(edge.list[,c("from","to")], directed = T
  )
  l <- layout_with_kk(mainGraph)
  
  
  #edge.list[which(edge.list$weights==0),]$weights=0.0001
  return (mainGraph)
}



compute_edgeWeights=function(edgeList,numAlpha,window,brkPoint)
{
  edgeWeights=apply(edgeList,1,function(f)
  {
    mean_dist=dSAX(f[1],f[2], numAlpha, window, brkPoint)
    
  })
  edgeWeights
}




from_to=function(temp)
{
  
  #Create a dataframe that has "from" and "to" nodes, i.e, vertices onnected by edges.
  graphEdges=NULL
  for (j in 1:(length(temp)-1))
  {
    graphEdges=rbind(graphEdges,cbind("from"=as.character(temp[j]),"to"=as.character(temp[j+1])))
    
  }
  return (data.frame(graphEdges,stringsAsFactors = F))
}




discover_edges=function(symbolicRep)
{
  pathLength=NULL
  
  allEdge.info=NULL #store info for of all the edges in the graph.
  allVertex.info=NULL
  edge.list=NULL
  
  
  for (i in 1:nrow(symbolicRep))
  {
    
    temp=t(symbolicRep[i,])
    
    edge.list=data.frame(rbind(edge.list,x=from_to(temp)),row.names = NULL)
    
    temp=NULL
    
  }
  colnames(edge.list)=c("from","to")
  
  ##Remove duplicate edges
  edge.list=edge.list[!duplicated(edge.list),]
  
  edge.list = data.frame(edge.list[!(edge.list$from==edge.list$to),], stringsAsFactors = F, row.names = NULL)
  
  return (edge.list)
}


get_graph=function(fileToRead, symbolicRep, dataSet, window, clusterLabels)
{
  clusterQuality=NULL
  print (window)
  dta.norm=(fileToRead-mean(unlist(fileToRead)))/sd(unlist(fileToRead))
  
  name_sym = paste0(dataSet,"_sym_", "win", window, ".csv")
  numAlpha = read.csv(file.path(getwd(), "Results", "Proposed", paste0(dataSet,"_numAlpha",".csv")),header=T)  
  numAlpha = numAlpha[which (numAlpha$window == window),]$alpha
  
  name_brkPoint=paste0(dataSet,"_brkPoint", "_win",window, ".csv")
  brkPoint = read.csv( file.path(getwd(), "Results", "Proposed",name_brkPoint),header=T)
  
  edgeList = discover_edges(symbolicRep)
  
  edgeWeights = round(compute_edgeWeights(edgeList,numAlpha,window,brkPoint),3)
  
  mainGraph= graphConstruct(edgeList)
  V(mainGraph)$id = seq(1, vcount(mainGraph))
  E(mainGraph)$weight = edgeWeights
  E(mainGraph)$id= seq(1, length(E(mainGraph)))
  
  ##Min-max normalization of weights, so that they range from 0 to 1
  E(mainGraph)$weight = round ((E(mainGraph)$weight - min(E(mainGraph)$weight))/ (max(E(mainGraph)$weight)- min(E(mainGraph)$weight)),4)
  E(mainGraph)$weight = 1- E(mainGraph)$weight 
  weighted_edges=data.frame(cbind(get.edgelist(mainGraph),"weights"=E(mainGraph)$weight), row.names = NULL, stringsAsFactors = F)
  colnames(weighted_edges)=c("from","to","weights")
  
  return(mainGraph)
  
}

##A sample to show  how the atypical pattern discovery works
dataSet=c("London", "Ausgrid")

plot_list= NULL
for (p in dataSet)
{
  data_nm= p
  dataFile=read.csv(file.path(getwd(),paste0(data_nm,"Dataset.csv")),header=T)
  dataFile=dataFile[,-c(1,2)]
  
  if (data_nm=="Ausgrid" )
  {
    window = 8
    ##A sequence of dates present in the buildings, is generated. 
    ## In our study, the date sequence chosen is::
    date_seq = seq(from =  as.POSIXct("2012-07-01"), to = as.POSIXct("2013-07-01"), by =(60*30))
    
    ###This cluster label is chosen because of its best cluter quality obtained.
    clusterLabels = read.csv(file.path(getwd(),"Results", "Proposed", "Ausgrid_clusterLabels_win8_beta0.65.csv" ))
    sym = read.csv(file.path(getwd(),"Results", "Symbolic representation", "Ausgrid_sym_win8.csv" ))
  }
  else
  {
  
    window = 24
    
    ##A sequence of dates present in the buildings, is generated. 
    ## In our study, the date sequence chosen is::
    date_seq = seq(from =  as.POSIXct("2012-03-01"), to = as.POSIXct("2012-08-31"), by =(60*30))
    
    ###This cluster label is chosen because of its best cluter quality obtained.
    clusterLabels = read.csv(file.path(getwd(),"Results", "Proposed", "London_clusterLabels_win24_beta0.5.csv" ))
    sym = read.csv(file.path(getwd(),"Results", "Symbolic representation", "London_sym_win24.csv" ))
  }
  segmentDates = nonOverlap(as.character(unlist(date_seq)),window )
  
  mainGraph = get_graph(dataFile, sym, data_nm, window, clusterLabels)
  
  ### Obtain the graph . Comment out the next line when outlier detection is required.
  plot_list[[p]]  = plot_graph(mainGraph, clusterLabels) 
  
  atypicalPlots = atypical_ausgrid(mainGraph, dataFiles, sym,clusterLabels, window, segmentDates, data_nm)
  atypicalPlots = atypical_london(mainGraph, dataFiles, sym,clusterLabels, window, segmentDates, data_nm)
  
  
}
