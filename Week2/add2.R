add2 <- function(x, y){
	x+y
}

# this function get a vector x
above10 <- function(x) {
	use <- x > 10
	x[use]
}

above <- function(x, n) {
	use <- x > n
	x[use]
}