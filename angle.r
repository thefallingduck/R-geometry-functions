angle<-function(center = c(0, 0), pointOne = c(2, 2), pointTwo = c(2, -2)){

# Computes the angle between two points with one center point. Used to calculate distance 
# Arguments:
#  center:    Center point of a spiral or the intersection of the two lines.
#  pointOne:  First outer point (x,y)
#  pointTwo:  Second outer point (x,y)
  
# Returns:
#  angle: angle between two lines - first to second

library(aspace)

lineOne <- matrix(c(center, pointOne), ncol = 2, byrow = TRUE)
lineTwo <- matrix(c(center, pointTwo), ncol = 2, byrow = TRUE)
 
slopeOne <- (lineOne[1, 2] - lineOne[2, 2]) / (lineOne[1, 1] - lineOne[2, 1])
slopeTwo <- (lineTwo[1, 2] - lineTwo[2, 2]) / (lineTwo[1,1] - lineTwo[2, 1])

angle<-atan_d(abs((slopeOne - slopeTwo) / (1 + slopeOne * slopeTwo)))

return(angle)
  
}
