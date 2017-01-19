logspiral <- function( whorls = 3, deg.space = 1, b = 0.1759, a = 1, polar.out = TRUE){

#Function to create a dataframe representing a log spiral with specific number of whorls.

#Arguments:
#	whorls:			Number of whorls from origin
#	deg.space:		Spacing between points in degrees.
#	b:				Coeffecient in the exponent
#	a:				Coeffecient in front of the exponent.
#	polar.out:		Logical argument for polar or cartesian coordinant output.

#Returns:
#	spiral:			Dataframe of either cartesian coordinants or polar coordinants (radius, degrees).

degrees <- seq(from = 0, to = (whorls*360), by = deg.space)

radians <- (degrees*pi)/(180)

r <- a*exp(b*radians)

spiral <- data.frame(cbind(r,degrees))

if(polar.out == FALSE){

spiral <- polartocartesian(r = r, angle = degrees, as.degree = TRUE)

}

return(spiral)}
