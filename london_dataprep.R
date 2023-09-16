##This is a sample file that show how to clean the data for further processing. 
## All the other datasets have been cleaned in a similar way.

##The folders need to be created on choice, in the working directory.


library(dplyr)
library(pracma)


extractFullData= function(allFiles)
{
  ### The lines below here processes the cleaned houses and finds out the common dates in all the houses, to be used in the 
  ### experiments. 
  newDf=NULL
  for (i in 1: length(allFiles))
  {
    df= read.csv(file.path(getwd(),"Cleaned houses_halfHourly", allFiles[i]), header=T)
    
    x= max(as.Date(df$Datetime))- min(as.Date(df$Datetime))
    # if (x> 365)    ##greater than 1 year data. This can be reduced accordingly
    # {
      newDf= rbind(newDf, cbind("house" = as.character(unique(df$House)), 
            "acornGroup" = as.character(unique(df$Acorn_grouped)), 
            "min" = as.character(min(as.Date(df$Datetime))),
            "max" = as.character(max(as.Date(df$Datetime))),
            "ccount" = x))
    # }
  }
  newDf = data.frame(newDf)
  common_minDate= max(as.Date(newDf$min))
  common_maxDate= min(as.Date(newDf$max))
  
  return (newDf)
}



mergeFiles= function()
{
  
  allFiles= list.files(file.path(getwd(),"Cleaned houses_halfHourly"),recursive = F)
  
  ### Use the function below to find common dates 
  commonDateList= extractFullData(allFiles)
  commonDateList$ccount <- as.integer(as.character(commonDateList$ccount))
  commonDateList = commonDateList[which((commonDateList$ccount )> 365),]
  commonDateList = commonDateList[ which(as.character(commonDateList$acornGroup)%in%  c("Adversity","Affluent","Comfortable")),]
  
  ### Now choose the houses which lies within 'common_minDate' to common_maxDate' from the calling function.
  dateSeq = seq(to =  max(as.Date(commonDateList$min)), from =  min(as.Date(commonDateList$max)), by= 1)
  splitGroups = split(commonDateList,commonDateList$acornGroup)
  allHouseData= NULL

  allHouseData = lapply(splitGroups, function(inn)
  {
    for (z in 1:nrow(inn))
    {
      df=read.csv(file.path(getwd(),"Cleaned houses_halfHourly",paste0(inn$house[z], ".csv")),header=T,
                  stringsAsFactors = F)
      df= data.frame(df[(as.Date(df$Datetime) %in% dateSeq),])
      allHouseData=  rbind(allHouseData, cbind("House" = unique(as.character(df$House)), 
                  "dates" = as.character(df$Datetime[1]), t(df$KWH)))
    }
    allHouseData
  })
  

}

missingData=function(dta)
{
  
  old=nrow(dta)
  
  library(dplyr)
  library(zoo)
  
  ##################################################################################################### 
  #Replace the leading and trailing zeros
  if (is.na(dta$KWH[1]))
    dta$KWH[1]=dta$KWH[2]
  else if (is.na(dta$KWH[nrow(dta)]))
    dta$KWH[nrow(dta)]=dta$KWH[(nrow(dta)-1)]
  
  #####################################################################################################  
  #Interpolation of missing values in the middle of the data
  date_seq <- data.frame("DateTime"=seq(min(as.POSIXct(as.character(dta$DateTime))),
                                        max(as.POSIXct(as.character(dta$DateTime))),
                                        by=(60*30)))
  
  dta=date_seq %>% full_join(dta, by = "DateTime") %>%
    mutate(KWH = na.approx(KWH,na.rm=F))
  
  new=nrow(dta)
  
  if(old!=new)
    print ("Missing data found")
  
  return (dta)           
}


replaceDuplicates=function(dta)
{
  library(zoo)
  temp <- data.frame(read.zoo(dta, header = TRUE, 
                              aggregate = mean))
  
  temp= data.frame(cbind(unique(dta$DateTime),temp))
  
  #There are certain rows where the time is not in half hours viz. in buildings[4], buildings[26], in the "buildings" list
  #where the time is "18:20:32" and the power value is NA. For further details, find for "NA" in the "dta" named dataframe
  #In the line below, such NA rows are removed. 
  
  temp=na.omit(temp)
  
  colnames(temp)=c("DateTime","KWH")
  return (temp)
}




cleanFiles= function()
{
  
  hourlyData=function(dta)
  {
    dta=as.data.frame(dta)
    dta=aggregate(KWH~Hour,data=dta,function(f) mean(as.numeric(as.character(f))))
    
    return(data.frame((dta[,2])))
  }
  
  allMerge=NULL
  allFiles= list.files(path=file.path(getwd(),"All houses"),recursive = F)
  storeDates=NULL
  
  for (files in 1:length(allFiles))
  {
    print (files)
    dfCleaned<-NULL
    df=(read.csv(file.path(getwd(),"All houses",allFiles[files]),header=TRUE,sep=",",stringsAsFactors = F))

    df$DateTime=as.POSIXct(df$DateTime)
    df$KWH.hh..per.half.hour.=as.numeric(df$KWH.hh..per.half.hour.)
    
    fileName=unique(as.character(df$LCLid))
    acornGroup=unique(as.character(df$Acorn_grouped))

    df=replaceDuplicates(df[,c("DateTime","KWH.hh..per.half.hour.")]) #duplicates replaced by mean values
    df= missingData(df)        #missing values replaced by linear interpolation
    
    library(xts)
    
    # dfAvg <- aggregate(df$KWH, list(hour=cut(as.POSIXct(df$DateTime), "hour")),mean)
    # dfCleaned=data.frame(cbind(fileName,dfAvg,acornGroup))
    # colnames(dfCleaned)=c("House","Datetime","KWH","Acorn_grouped")
    
    dfCleaned=data.frame(cbind(fileName,df,acornGroup))
    colnames(dfCleaned)=c("House","Datetime","KWH","Acorn_grouped")
    
    storeDates=rbind(storeDates,cbind.data.frame(min(as.POSIXct(dfCleaned$Datetime)),max(as.POSIXct(dfCleaned$Datetime)),acornGroup,fileName))
    write.csv(dfCleaned,file.path(getwd(),"London Data","Cleaned houses_halfHourly",paste0(fileName,".csv")), row.names = F)
  }
}

# cleanFiles()
finalData = mergeFiles()

write.csv(data.frame(finalData),file.path(getwd(),"LondonData_forGraph.csv"),row.names = F)
