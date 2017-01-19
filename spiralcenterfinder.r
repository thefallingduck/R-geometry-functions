spiralcenter <- function( data, step = 2){
  # Determines the center of a spiral made of limited points
  
  # Arguments:
  #  data:            dataframe or matrix of (x, y) points
  #  step:            step for search smaller runs longer
  
  ##### Internal 
  #  CentroidX:       centroid X for center of data cloud
  #  CentroidY:       centroid Y for center of data cloud
  
  #  ClosestX:        closest X point to centroid from data
  #  ClosestY:        closest Y point to centroid from data
  
  # Returns:
  #  CenterPoint:    pair of points that identify the center
  #source(angle.r)
  
  CentroidX <- mean(data$X)
  CentroidY <- mean(data$Y)
  
  CentralX <- data$X[1]
  CentralY <- data$Y[1]
  
  DistX <- abs(CentroidX - CentralX)
  DistY <- abs(CentroidY - CentralY)
  
  SearchX <- seq(from = (CentralX - DistX), to = (CentralX + DistX), by = step)
  SearchY <- seq(from = (CentralY - DistY), to = (CentralY + DistY), by = step)
  
  SearchGrid <- expand.grid(SearchX, SearchY)
  
  SpearmanDistance <- vector(length = nrow(SearchGrid))
  
  totalDistance <- matrix(nrow = length(data$X), ncol = length(SearchGrid))
  
  for(k in 1:length(SpearmanDistance)){
    
    allDistance <- rep(0, length = nrow(data))
    
    for(j in 1:(length(data$X)-1)){
      
      allDistance[j+1] <- distance(pointOne = SearchGrid[k, ], pointTwo = data[j, ])
      
    }
    
    SpearmanDistance[k] <- cor(x = 1:length(allDistance), y = allDistance, method = 'spearman')
    #totalDistance[,k] <- allDistance
  
  }
  
  CenterPoint <- SearchGrid[which.max(SpearmanDistance),]
   
  return(CenterPoint)
  
}
