## Put comments here that give an overall description of what your
## functions do

## This function cretes an empty matrix

makeCacheMatrix <- function(x = matrix()) {
		s <- NULL
		set <- function(y){
			x <<- y
			s <<- NULL
		}
		get <- function() x
		SetInverse <- function(solve) s <<- solve
		GetInverse <- function() s
		list(set = set, get = get,
			SetInverse = SetInverse,
			GetInverse = GetInverse)
}


## This function sets and gets the value of the inverted matrix 

cacheSolve <- function(x, ...) {
        s <- x[[GetInverse()]]
		if(!is.null(s)){
			message("getting cached data")
			return(s)
		}
		data <- x[[get()]]
		s <- solve(data, ...)
		x[[SetInverse(s)]]
		s
}
