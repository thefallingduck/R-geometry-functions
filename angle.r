#Angle between two pairs of points. To use with a spiral, include one point in each.
#Inputs are to matricies, output is a single value of the angle in degrees.
#Depends on aspace package.

angle<-function(dummy.one=matrix(c(0,0,2,2),ncol=2),dummy.two=matrix(c(0,0,2,-2),ncol=2))
{
library(aspace)

#x1,y1,x2,y2

m1<-(dummy.one[1,2]-dummy.one[2,2])/(dummy.one[1,1]-dummy.one[2,1])
m2<-(dummy.two[1,2]-dummy.two[2,2])/(dummy.two[1,1]-dummy.two[2,1])

angle<-atan_d(abs((m1-m2)/(1+m1*m2)))

return(angle)
}
