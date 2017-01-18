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
source(angle.r)

CentroidX <- mean(data$X)
CentroidY <- mean(data$Y)
  
ClosestX <- which.min(abs(data$X - CentroidX))
ClosestY <- which.min(abs(data$Y - CentroidY))

DistX <- abs(CentroidX - ClosestX)
DistY <- abs(CentroidY - ClosestY)

SearchX <- seq(from = (CentroidX - DistX), to = (CentroidX - DistX), seq = step)
SearchY <- seq(from = (CentroidY - DistY), to = (CentroidY - DistY), seq = step)

SearchGrid <- expand.grid(SearchX, SearchY)

sumAngles <- vector(length = nrow(SearchGrid))

for(k in 1:length(totalRotation)){
  
  for(j in 1:nrow(data)){
    
    allangles <- rep(0, length = nrow(data))
    allangles[j+1] <- angle(center = SearchGrid[k], pointOne = data[j, ], pointTwo = data[j+1, ])
        
      }
  
  sumAngles[k] <- sum(allangles)

  }
  
CenterPoint <- SearchGrid[which.min(sumAngles),]

return(CenterPoint)

}
