edgeWeights=function(clusters,graph)
{
  
  nc <- max(clusters$labels)
  
  intraClust <- rep(NA, nc)
  
  for (i in 1:nc) {
    
    c1 <- as.character(clusters[which(clusters$labels == i),]$vertex)
    
    subg = subgraph(graph, c1)
    if (length(E(subg)))
    {
      intraClust[i] <- mean(E(subg)$weight)
      
    }
    else
      intraClust[i]<-NA
    
  }
  
  intraClust = max(intraClust,na.rm=T)
  return (round(intraClust,3))
  
}

edgeDensity =  function(clusters, graph)
{
  
  nc <- max(clusters$labels)
  
  intraClust <- rep(NA, nc)
  
  for (i in 1:nc) {
    
    c1 <- as.character(clusters[which(clusters$labels == i),]$vertex)
    
    subg = subgraph(graph, c1)
    if (length(E(subg)))
    {
      
      nv=vcount(subg)
      intraClust[i] <- length(E(subg))/ (nv*(nv-1))
    }
    else
      intraClust[i]<-NA
    
  }
  intraClust.min = min(intraClust,na.rm=T)
  intraClust.mean = mean(intraClust, na.rm=T)
  
  return ( list( round(intraClust.min,3), round(intraClust.mean,3) ))
}


clusterQuality = function(clusters, graph)
{
  ew = edgeWeights(clusters, graph)
  out= edgeDensity(clusters, graph)
  min.ed = out[[1]]
  mean.ed = out[[2]]
  return (list(ew,min.ed, mean.ed))

}


