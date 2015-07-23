## makeCacheMatrix creates a special matrix.
## cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will not calculate it again.
## Find it in the cache and return it.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix.
## If the cached inverse is available, cacheSolve retrieves it.
## Otherwise cacheSolve computes&return caches.

cacheSolve <- function(x, ...) {
        inv_x <- x$getinverse()
        if (!is.null(inv_x)) 
                {
                message("getting cached inverse matrix")
                return(inv_x)
                } 
        else 
                {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
                }
}
