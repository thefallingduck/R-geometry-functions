angle<-function(center = c(0, 0), pointOne = c(2, 2), pointTwo = c(2, -2)){

# Computes the angle between two points with one center point. Used to calculate distance 
# Arguments:
#  center:    Center point of a spiral or the intersection of the two lines.
#  pointOne:  First outer point (x,y)
#  pointTwo:  Second outer point (x,y)
  
# Returns:
#  angle between two lines

library(aspace)

lineOne <- matrix(c(0, 0, 2, 2), ncol = 2)
lineTwo <- matrix(c(0, 0, 2, -2), ncol = 2)
 
slopeOne <- (dummy.one[1,2]-dummy.one[2,2])/(dummy.one[1,1]-dummy.one[2,1])
slopeTwo <- (dummy.two[1,2]-dummy.two[2,2])/(dummy.two[1,1]-dummy.two[2,1])

angle<-atan_d(abs((slopeOne-slopeTwo)/(1+slopeOne*slopeTwo)))

return(angle)
  
}
