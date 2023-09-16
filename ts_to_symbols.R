nonOverlap=function (timeseries, swSize) 
{
  ####  The function is used for obtaining the non-overlapping segments
  
  SW<-NULL
  for (x in seq(1,length(timeseries) ,swSize))
  {
    windowCol=timeseries[x:(x+swSize-1)]
    SW <- cbind(SW, windowCol, deparse.level = 0)
  }
  SW=t(SW)
  SW=na.omit(SW)
  row.names(SW)<-NULL
  return (SW)
}


ts_to_symbols= function(rawData,swSize)
{
  library(rowr)
  meanVals= NULL
  slopeVals= NULL
  maxCorr.at= NULL
  max_minus_min= NULL
  allSegData= NULL
  
  alpha= seq(3,15)
  breakpointsTable= sapply(alpha,function(f) qnorm(0:f/f))
  temp=NULL
  breakpointsTable= lapply(breakpointsTable,function(f) temp<<-cbind.fill(temp,(f),fill=NA))
  breakpointsTable= breakpointsTable[[length(breakpointsTable)]]
  breakpointsTable= data.frame(breakpointsTable[,which(unlist(lapply(breakpointsTable, function(x) !all(is.na(x)))))])
  
  colnames(breakpointsTable)= c(seq(3,(ncol(breakpointsTable)+2),1))
  
  
  for (i in 1:nrow(rawData))
  {
    
    dta= unlist(rawData[i,])
    
    segData= nonOverlap(dta,swSize)
    allSegData= rbind(allSegData,segData)
    
    meanVals= rbind(meanVals,t(apply(segData,1,function(f.m) mean(unlist(f.m)))))
    
    slopeVals=  rbind(slopeVals,t(apply(segData,1,function(f.s) 
    {
      mod<-lm(unlist(f.s)~seq(1,length(f.s)))
      coef(mod)[2]
    })))
    
    max_minus_min=  rbind(max_minus_min,t(apply(segData,1,function(f) 
    {
      max(f)-min(f)
    })))
    
  }
  
  median.meanVal = median (as.numeric(meanVals))
  dist_from_median = apply(breakpointsTable,2,function(f)
  {
    f=f[!is.infinite(f) & !is.na(f)]
    abs(f-median.meanVal)
    
  })
  dist_from_median = (lapply(dist_from_median, min))
  mean.numAlpha = as.numeric (names(breakpointsTable)[which.min(unlist(dist_from_median))])
  
  
  ##**********************************************************************************************
  median.slope=median(as.numeric(slopeVals))
  dist_from_median= apply(breakpointsTable,2,function(f)
  {
    f= f[!is.infinite(f) & !is.na(f)]
    abs(f-median.slope)
    
  })
  dist_from_median= (lapply(dist_from_median, min))
  slope.numAlpha= as.numeric (names(breakpointsTable)[which.min(unlist(dist_from_median))])
  
  ##***************************************************************************************************
  median.maxMin= median(unlist(max_minus_min))
  dist_from_median= apply(breakpointsTable,2,function(f)
  {
    f= f[!is.infinite(f) & !is.na(f)]
    abs(f-median.maxMin)
    
  })
  dist_from_median= (lapply(dist_from_median, min))
  maxMin.numAlpha= as.numeric (names(breakpointsTable)[which.min(unlist(dist_from_median))])
  # ##******************************************************************************************************
  
  maxAlpha= max(c(mean.numAlpha,slope.numAlpha,maxMin.numAlpha))
  meanSym= apply(meanVals,2,function(f)
  {
    a= convert.to.SAX.symbol(f,maxAlpha)
    #letters[a]
  })
  
  ###********************************************************************************************************
  slopeSym= apply(slopeVals,2,function(f)
  {
    a= convert.to.SAX.symbol(f,maxAlpha)
    #letters[a]
  })
  ###********************************************************************************************************
  maxMinSym= apply(max_minus_min,2,function(f)
  {
    a= convert.to.SAX.symbol(f,maxAlpha)
    #letters[a]
  })
  ###********************************************************************************************************
  
  mapMean = data.frame(cbind(as.vector(t(meanVals)), as.vector(t(meanSym))))
  mapSlope= data.frame(cbind(as.vector(t(slopeVals)), as.vector(t(slopeSym))))
  mapMaxMin = data.frame(cbind(as.vector(t(max_minus_min)), as.vector(t(maxMinSym))))
  
  colnames(mapMean)=c("mean","letters")
  colnames(mapSlope)= c("mean","letters")
  colnames(mapMaxMin)= c("mean","letters")
  
  SAX.breakpoints.table <- function (n) {
    qnorm(0:n/n)
  }
  
  symb <- SAX.breakpoints.table(maxAlpha)
  lessThanZero = which(symb <= 0)
  greaterThanZero = which(symb > 0)
  
  brkPoint=NULL
  for (i in lessThanZero)
  {
    brkPoint = rbind(brkPoint, cbind(i,max(mapMean$mean[which(mapMean$letters==i)]),1))
    brkPoint = rbind(brkPoint, cbind(i,max(mapSlope$mean[which(mapSlope$letters==i)]),2))
    brkPoint = rbind(brkPoint, cbind(i,max(mapMaxMin$mean[which(mapMaxMin$letters==i)]),3))
  }
  
  for (j in greaterThanZero)
  {
    brkPoint = rbind(brkPoint, cbind(j,min(mapMean$mean[which(mapMean$letters==j)]),1))
    brkPoint = rbind(brkPoint, cbind(j,min(mapSlope$mean[which(mapSlope$letters==j)]),2))
    brkPoint = rbind(brkPoint, cbind(j,min(mapMaxMin$mean[which(mapMaxMin$letters==j)]),3))
  }
  
  brkPoint = data.frame(brkPoint)
  brkPoint = brkPoint[!is.infinite(rowSums(brkPoint)),]
  colnames(brkPoint) = c("alpha","val","feature")
  
  ###********************************************************************************************************
  meanSym= apply(meanSym,2,function(f)
  {
    letters[f]
  })
  
  ###********************************************************************************************************
  slopeSym= apply(slopeSym,2,function(f)
  {
    letters[f]
  })
  ###********************************************************************************************************
  maxMinSym= apply(maxMinSym,2,function(f)
  {
    letters[f]
  })
  ###********************************************************************************************************

  allSym= matrix( paste(meanSym,slopeSym, maxMinSym ,sep=""),nrow=nrow(meanSym))
  return (list(as.data.frame(allSym),maxAlpha, brkPoint))
  
}


