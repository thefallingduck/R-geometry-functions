errorbar <- function(x, y, upper, lower = upper, length = 0.1, orientation = 90...){

#Plots error bars as arrows on a plot.
#Arguments:
# x:              x coordinant of a point. Can be a vector paired with other points.
# y:              y coordinant of a point. Can be a vector paired with other points.
# upper:          Upper length of error bar.
# lower:          Lower length of error bar.
# length:         Length of the end of the error bars.
# orientation:    Orientation of the error bars relative to the points.

if(length(x) != length(y) | length(y) != length(lower) | length(lower) != length(upper))

stop("vectors must be same length")

arrows(x, y + upper, x, y - lower, angle = orientation, code = 3, length = length, ...)

}
