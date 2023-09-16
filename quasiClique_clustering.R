library(RevEcoR)


##This part has 2 functions- initialize_dist, merging_part. 
##the initialize_dist is called only once. 

initialize_dist = function(graph,initial_clusterSet)
{
  ##Initialize the distance matrix to store the edge weights
  d = matrix(nrow=length(initial_clusterSet),ncol=length(initial_clusterSet), data= NA)
  for (j in 1:length(initial_clusterSet))
  {
    d[j,j] = -1
    k = j+1
    while(k <= length(initial_clusterSet))
    {
      subset.nodes = unique(append(initial_clusterSet[[j]], initial_clusterSet[[k]]))
      subG=subgraph(graph, subset.nodes)
      
      if (length(E(subG))==2)
      {
        betaCriteria =  round( unique(E(subG)$weight)/ (vcount (subG)-1),3)
        d[j,k] = d[k,j] = min(betaCriteria)
        
      }
      else if (degree(subG, initial_clusterSet[[j]], mode="out") )
        d[j,k] = E(subG)$weight ## when unidirectional edge exists from 'j' to 'k' 
      else if ( degree(subG, initial_clusterSet[[k]], mode="out") )
        d[k,j] =  E(subG)$weight ## when unidirectional edge exists from 'k' to 'j' 
      
      else
        d[j,k] = d[k,j] = -1 ##when no edge exists between the two vertices.
      
      k=k+1
      
    }
  }
  return (d)
  
}


merging_part = function(graph, betaVals, initial_clusterSet, betaMax)
{
  new_clusterSet = initial_clusterSet
  N = length(initial_clusterSet)
  update_clusterSet = initial_clusterSet
  lenn= length(which(unlist(lapply(update_clusterSet,function(f) length(f)))>0))

  k=1
  for  (k in 1: length(update_clusterSet))
  {
    if (length(update_clusterSet[[k]]))
    { 
      node = update_clusterSet[[k]]
      
      #find the neighbors
      node.neibr =  V(graph) [neighbors(graph,node, mode = "out")]$name
      betaVal = unlist(lapply(update_clusterSet, function(inn)
      {
        print (inn)
        subG=subgraph(graph, c(node, inn))
        if (is.connected(subG))
        {
          nodeWeight = round(unlist(lapply(V(subG)$name, function(fun)
          {
            sum(E(subG)[from(fun)]$weight)
          })),4)
          betaVal =  min (round( nodeWeight/ (vcount (subG)-1),4))
          betaVal
        }
        
      }))
      if (any(betaVal<= betaMax))
      {
        p = which(betaVal<=betaMax)[1]
      }
      update_clusterSet[[min(k,p)]] <- unique(append(update_clusterSet[[min(k,p)]], update_clusterSet[[max(k,p)]]))
      update_clusterSet[[max(k,p)]] <- list()
      
    }
  }

  update_clusterSet = update_clusterSet[unlist(lapply(update_clusterSet,function(f) length(f)>0))]
  z<-0
  clustLabels = lapply(update_clusterSet,function(f) {
    z<<-z+1 
    cbind(f,z)})
  
  clustLabels = data.frame(do.call(rbind,clustLabels),stringsAsFactors = F)
  
  return (clustLabels)
  
}

initialization = function(graph, edgeList)
{
  
  ##Compute the vertex weights.
  
  ver.weight = round(unlist(lapply(V(graph)$name, function(fun)
  {
    sum(E(graph)[from(fun)]$weight)
  })),4)
  
  ver.weight = data.frame(cbind("vertex"= as.character(V(graph)$name), "weight"= ver.weight))
  
  ordered_beta = data.frame(ver.weight[order(as.numeric(ver.weight$weight), decreasing=T),],row.names = NULL,stringsAsFactors = F)
  initial_clusterSet = as.list(as.character(ordered_beta$vertex))
  
  return (initial_clusterSet)
  
}


quasiClique_clustering= function(graph, edgeList, betaMax)
{
  
  ##Reverse the weight vals so that the most similar nodes gets highest edge weight. This helps in beta computation.
  E(graph)$weight = 1- E(graph)$weight
  
  initial_clusterSet = initialization(graph, edgeList)
  betaVals = initialize_dist(graph, initial_clusterSet)
  
  clustLabels = merging_part(graph, betaVals, initial_clusterSet, betaMax)
  colnames(clustLabels) = c("vertex","labels")
  clustLabels$labels = as.numeric(clustLabels$labels)
  clustLabels

}
