polartocartesian <- function(r, angle, as.degree = TRUE){

#Function for converting between polar coordinate system to cartesian coordinants. This works to speed the creation of a spiral for testing center finder function.

# Arguments:
#		r:				Vector of radius values. 1 unit in this space is equal to one unit in cartesian space.
#		angle:			Vector of angle values from the polar coordinants. 0 is up in cartesian space.
#		as.degree:		Logical argument for input angle as degree or radians. If radians, convert to degrees.

# Returns:
#		coordinants:	Dataframe with x and y columns for plotting polar points in cartesian space.

if(length(r)!=length(angle)){

stop("Input vectors are of unequal length.")}

if(as.degree == TRUE){

	angleRad <- (angle*pi)/(180)

} else {

	angleRad <- angle
	
}


x <- cos(angleRad)*r
y <- sin(angleRad)*r

coordinants <- data.frame(cbind(x, y))

return(coordinants)}
