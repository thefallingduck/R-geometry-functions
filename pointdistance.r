distance<-function(pointOne=c(0,0),pointTwo=c(1,1)){

#
# Arguments:
#   pointOne:      First point in x, y form.
#   pointTwo:      Second point in x, y form.

#Returns:
#   distance:      Distance between two points.

distance <- sqrt( abs( (pointOne[1] - pointTwo[1])^2 + (pointOne[2] - pointTwo[2])^2) )

return(distance)

}
