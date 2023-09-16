### This file is to find the houses suitable for demand response.  This is based on an entropy computation to meaure the stability of the buildings. 
## To obtain the plots, another file is used named as "dr_plots". The chosen buildings obtained from the entropy measure is used in the plot

library(stringr)
library(dplyr)
library(plyr)



computeEntropy =  function(clusterLabels, sym)
{
  
  numSamples = seq(1, nrow(dataFile))
  ts_labelled = NULL
  
  for (j in numSamples)
  {
    sym.vec = data.frame(t(sym[j,]))
    colnames(sym.vec)="vertex"
    ll = join(sym.vec, clusterLabels)
    
    ts_labelled = rbind (ts_labelled, ll$labels)
    
  }


  entrpy = apply(ts_labelled,1, function(fun)
  {
    entrpy = table(fun)/length(fun)
    
    entrpy =  sum ( entrpy * log2(entrpy))
    (-entrpy)
    
  })
  
  hist(entrpy,
       main="",
       xlab="Entropy", ylab="# of buildings",
       border="blue",
       col="skyblue",
       xlim=c(0,2),
       las=1)
  
  chosenBuildings = c ( which.max(entrpy), which.min(entrpy))
  
  return (list(chosenBuildings, entrpy))
  
}



dataSet=c("London", "Ausgrid")

for (x in dataSet)
{
  data_nm= x
  dataFile=read.csv(file.path(getwd(),paste0(data_nm,"Dataset.csv")),header=T)
  
  list_clustLabels = list.files(path=file.path(getwd(), "Results", "Proposed"), pattern=paste0(data_nm,"_clusterLabels")) 
  dataFile=dataFile[,-c(1,2)]
  
  if (data_nm=="Ausgrid")
  {
    ##A sequence of dates present in the buildings, is generated. 
    ## In our study, the date sequence chosen is::
    date_seq = seq(from =  as.POSIXct("2012-07-01"), to = as.POSIXct("2013-07-01"), by =(60*30))
    
    patt= c("win8_beta0.65")     ## This cluster label is  chosen for diagram, becaus of better cluster distribution
    list_clustLabels = list_clustLabels[unlist(lapply(patt,function(f) grep(f, list_clustLabels)))]
    multiple_dates  = seq(1,48)  ##chosen dates where the peaks will be computed. This can be changed. Infact check with different values.
    single_date = seq(1, 6)
  }
  
  else if (data_nm=="London"){
    
    ##A sequence of dates present in the buildings, is generated. 
    ## In our study, the date sequence chosen is::
    date_seq = seq(from =  as.POSIXct("2012-03-01"), to = as.POSIXct("2012-08-31"), by =(60*30))
    
    patt= c("win24_beta0.5")  
    list_clustLabels = list_clustLabels[unlist(lapply(patt,function(f) grep(f, list_clustLabels)))]
    
    ###used in function"peakDetection.R"
    multiple_dates = seq(1,16)   ##chosen dates where the peaks will be computed. This can be changed. Infact check with different values.
    single_date = seq(1,2)
    
  }
  

  for (q in list_clustLabels)
  {
    print (q)
    clustLabels=read.csv( file.path(getwd(), "Results", "Proposed", q), header=T)
    swSize = str_split(q, "_", simplify=T)
    swSize = swSize[!is.na(str_match(swSize, "win"))]
    swSize = as.numeric(gsub("[^0-9.-]", "", swSize))
    
    
    name_sym = paste0(data_nm,"_sym_", "win", swSize, ".csv")
    print (name_sym)
    sym = read.csv(file.path(getwd(), "Results", "Symbolic representation",name_sym),header=T)

    out = computeEntropy(clustLabels, sym)
    entrpy =  out[[2]]
    lowEntropy_buildings= which(entrpy>=0 & entrpy<=1)
    # demandResponse_plots (dataFile,swSize, clustLabels, sym, date_seq, out[[1]], lowEntropy_buildings)
    peakDetection (dataFile,swSize, clusterLabels, sym, date_seq, lowEntropy_buildings, multiple_dates, single_date)
  }
  
}