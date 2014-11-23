## Matrix inversion is usually a costly computation. To save cumputation time the following 
## functions will cache the inverse of a matrix to allow for later use without recalculating.
## 
## The function makeCacheMatrix will create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        special_matrix <- NULL
        set <- function(y) {
                x <<- y
                special_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) special_matrix <<- inverse
        getinverse <- function() special_matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## The function cacheSolve will compute the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## will retrieve the the inverse from the cache.

cacheSolve <- function(x, ...) {
        special_matrix <- x$getinverse()
        if(!is.null(special_matrix)) {
                message("getting cached data.")
                return(special_matrix)
        }
        data <- x$get()
        special_matrix <- solve(data)
        x$setinverse(special_matrix)
        special_matrix
}
