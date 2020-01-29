## The following functions, makeCacheMatrix and cacheSolve, are to cache the inverse of a matrix.

## This function creates a special "matrix", which sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse, and gets the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## The following function calculates the inverse of the special "matrix" object from above.
## It first checks if the inverse has already been calculated and gets the inverse from the cache
## without computation. If not, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse              
}
